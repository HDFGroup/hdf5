/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have     *
 * access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu. *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * NOTE! This is NOT thread safe!
 *
 * NOTE: There will be caveats on call-back functions.
 */

/*
 * Purpose:
 *
 *      This file has all of the code that a server (SAP) would run to
 *      handle requests from clients.
 */

#include "H5private.h"          /* Generic Functions                    */
#include "H5Eprivate.h"         /* Error Handling                       */
#include "H5Oprivate.h"         /* Object Headers                       */
#include "H5TBprivate.h"        /* Threaded, Balanced, Binary Trees     */

#ifdef H5_HAVE_FPHDF5

#include "H5FPprivate.h"        /* Flexible Parallel Functions          */

/* Pablo mask */
#define PABLO_MASK      H5FPserver_mask

/* Is the interface initialized? */
static int interface_initialize_g = 0;
#define INTERFACE_INIT  NULL

/* Internal SAP structures */

struct sap_obj_lock {
    uint8_t oid[H5R_OBJ_REF_BUF_SIZE]; /* Buffer to store OID of object referenced */
    unsigned int owned_rank;    /* rank which has the lock                  */
    enum sap_obj_type obj_type; /* type of object being locked              */
    unsigned int ref_count;     /* number of times lock was aquired by proc */
    enum sap_lock_type rw_lock; /* indicates if it's a read or write lock   */
};

struct sap_file_mod {
    unsigned long key;          /* key field for insertion into the TBBT    */
    unsigned char *procs_notified; /* bitfield of size MPI_Comm_size()      */
                                /* indicates process was notified of this
                                 * modification - 0 means no, 1 means yes   */
    unsigned num_notified;      /* counts number of processes notified      */
    enum sap_obj_type obj_type; /* type of object                           */
    enum sap_action action;     /* action done to object (H5FP_REQ_CHANGE only) */
    int md_len;                 /* the length of the metadata array         */
    char *metadata;             /* encoded metadata about the object        */
};

struct sap_file_struct {
    unsigned int sap_file_id;   /* the file id the SAP keeps per file       */
    char *filename;             /* the filename - of dubious use            */
    int closing;                /* we're closing the file - no more changes */
    H5TB_TREE *mod_tree;        /* a tree of the modifications done         */
    H5TB_TREE *locks;           /* a tree of locks on objects in the file   */
};

static H5TB_TREE *fs_tree;

/*
 *  Note on procs_notified:
 *
 *  We needed a way of indicating that a process has been notified of a
 *  change. However, we didn't want to waste space, so I implemented an array
 *  of unsigned characters which would emulate such a thing. The code:
 *
 *          rank >> 3
 *
 *  is an integer divide by 8. While the code:
 *
 *          rank & 7
 *
 *  is a mod by 8. (Note that rank should be unsigned at this point). So, the
 *  code
 *
 *          fm->procs_notified[rank >> 3] |= 1 << (rank & 7);
 *
 *  is equivalent to
 *
 *          fm->procs_notified[rank / 8] |= 1 << (rank % 8);
 *
 *  Testing whether a given bit is set or not involves a test like this:
 *
 *          if (mod->procs_notified[rank >> 3] & (1 << (rank & 7)))
 *
 *  Of course, all bytes are 8 bits wide, right? :-)
 *
 *  The following two macros help this be more readable.
 */

#define SET_PROC_NOTIFIED(arr, rank)    \
    (arr->procs_notified[(rank) >> 3] |= (1 << ((rank) & 7)), ++arr->num_notified)
#define IS_PROC_NOTIFIED(arr, rank)     \
    (arr->procs_notified[(rank) >> 3] & (1 << ((rank) & 7)))

/* local functions */
static herr_t H5FP_sap_receive(struct SAP_request *req, int source,
                               int tag, char **buf);

    /* local functions to generate unique ids for messages */
static unsigned int H5FP_gen_sap_file_id(void);

    /* local functions for handling object locks */
static int H5FP_object_lock_cmp(struct sap_obj_lock *o1,
                                struct sap_obj_lock *o2,
                                int cmparg);
static struct sap_obj_lock *H5FP_new_object_lock(const unsigned char *oid,
                                                 unsigned int rank,
                                                 enum sap_obj_type obj_type,
                                                 enum sap_lock_type rw_lock);
static herr_t H5FP_free_object_lock(struct sap_obj_lock *ol);
static struct sap_obj_lock *H5FP_find_object_lock(struct sap_file_struct *fs,
                                                  unsigned char *oid);
static herr_t H5FP_remove_object_lock_from_list(struct sap_file_struct *fs,
                                                struct sap_obj_lock *ol);

    /* local file structure handling functions */
static herr_t H5FP_add_new_file_struct_to_list(unsigned int sap_file_id, char *filename);
static int H5FP_file_struct_cmp(struct sap_file_struct *k1,
                                struct sap_file_struct *k2, int cmparg);
static struct sap_file_struct *H5FP_new_file_struct_node(unsigned int sap_file_id,
                                                         char *filename);
static struct sap_file_struct *H5FP_find_file_struct(unsigned int sap_file_id);
static herr_t H5FP_remove_file_id_from_list(unsigned int sap_file_id);
static herr_t H5FP_free_file_struct_node(struct sap_file_struct *fs);

    /* local file modification structure handling functions */
static struct sap_file_mod *H5FP_new_file_mod_node(unsigned rank,
                                                   enum sap_obj_type obj_type,
                                                   enum sap_action action,
                                                   int md_len,
                                                   char *metadata);
static herr_t H5FP_free_mod_node(struct sap_file_mod *fs);

    /* local request handling functions */
static herr_t H5FP_sap_handle_open_request(struct SAP_request req, char *mdata, int md_len);
static herr_t H5FP_sap_handle_lock_request(struct SAP_request req);
static herr_t H5FP_sap_handle_release_lock_request(struct SAP_request req);
static herr_t H5FP_sap_handle_change_request(struct SAP_request req, char *mdata, int md_len);
static herr_t H5FP_sap_handle_sync_request(struct SAP_request req);
static herr_t H5FP_sap_handle_close_request(struct SAP_request req);

/** Public Library (non-API) Functions **/

/*
 * Function:    H5FP_sap_receive_loop
 * Purpose:     Just receive message after message from the other
 *              processes and process that message. Return when we
 *              receive an "H5FP_REQ_STOP" message from all processes in
 *              H5FP_SAP_COMM.
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 * Programmer:  Bill Wendling, 05. August, 2002
 * Modifications:
 */
herr_t
H5FP_sap_receive_loop(void)
{
    herr_t ret_value = SUCCEED;
    int stop = 0;

    FUNC_ENTER_NOAPI(H5FP_sap_receive_loop, FAIL);

    if ((fs_tree = H5TB_dmake((H5TB_cmp_t)H5FP_file_struct_cmp,
                              sizeof(struct sap_file_struct), FALSE)) == NULL)
        HGOTO_ERROR(H5E_FPHDF5, H5E_CANTMAKETREE, FAIL, "cannot make TBBT tree");

    for (;;) {
        struct SAP_request req; 
        char *buf = NULL;
        herr_t hrc;

        if (H5FP_sap_receive(&req, MPI_ANY_SOURCE, H5FP_TAG_REQUEST, &buf) != SUCCEED)
            HGOTO_ERROR(H5E_FPHDF5, H5E_CANTRECV, FAIL, "cannot receive messages");

        switch (req.req_type) {
        case H5FP_REQ_OPEN:
            if ((hrc = H5FP_sap_handle_open_request(req, buf, req.md_len)) != SUCCEED)
                HGOTO_ERROR(H5E_FPHDF5, H5E_CANTOPENOBJ, FAIL, "cannot open file");

            break;
        case H5FP_REQ_LOCK:
        case H5FP_REQ_LOCK_END:
            hrc = H5FP_sap_handle_lock_request(req);
            break;
        case H5FP_REQ_RELEASE:
        case H5FP_REQ_RELEASE_END:
            hrc = H5FP_sap_handle_release_lock_request(req);
            break;
        case H5FP_REQ_CHANGE:
            hrc = H5FP_sap_handle_change_request(req, buf, req.md_len);
            break;
        case H5FP_REQ_SYNC:
            hrc = H5FP_sap_handle_sync_request(req);
            break;
        case H5FP_REQ_CLOSE:
            hrc = H5FP_sap_handle_close_request(req);
            break;
        case H5FP_REQ_STOP:
            if (++stop == H5FP_comm_size - 1)
                goto done;

            break;
        default:
            HGOTO_ERROR(H5E_FPHDF5, H5E_ARGS, FAIL, "invalid request type");
        }

        if (hrc != SUCCEED)
            HDfree(buf);
    }

done:
    FUNC_LEAVE_NOAPI(ret_value);
}

/*
 * Function:    H5FP_sap_receive
 * Purpose:     Receive a message from SOURCE with the given TAG. The REQ
 *              object passed in as a pointer is filled by the MPI_Recv
 *              function.
 * Return:      Pointer to string passed in, if one was sent. As well as
 *              the SAP_request object.
 * Programmer:  Bill Wendling, 17. September, 2002
 * Modifications:
 */
static herr_t
H5FP_sap_receive(struct SAP_request *req, int source, int tag, char **buf)
{
    MPI_Status status;
    herr_t ret_value = SUCCEED;
    int mrc;

    FUNC_ENTER_NOINIT(H5FP_sap_receive);

    HDmemset(&status, 0, sizeof(status));

    if ((mrc = MPI_Recv(req, 1, SAP_request_t, source, tag,
                        H5FP_SAP_COMM, &status)) != MPI_SUCCESS)
        HMPI_GOTO_ERROR(FAIL, "MPI_Recv failed", mrc);

    if (buf && req->md_len) {
        if ((*buf = (char *)HDmalloc((size_t)req->md_len + 1)) == NULL)
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "out of memory");

        HDmemset(*buf, 0, (size_t)req->md_len + 1);

        if ((mrc = MPI_Recv(*buf, req->md_len, MPI_BYTE, (int)req->proc_rank,
                            H5FP_TAG_METADATA, H5FP_SAP_COMM, &status)) != MPI_SUCCESS) {
            HDfree(*buf);
            *buf = NULL;
            HMPI_GOTO_ERROR(FAIL, "MPI_Recv failed", mrc);
        }
    }

done:
    FUNC_LEAVE_NOAPI(ret_value);
}

/*
 * Function:    H5FP_object_lock_cmp
 * Purpose:     Comparison function for the TBBT.
 * Return:      <0, 0, or >0
 * Programmer:  Bill Wendling, 09. September 2002
 * Modifications:
 */
static int H5FP_object_lock_cmp(struct sap_obj_lock *o1,
                                struct sap_obj_lock *o2,
                                int UNUSED cmparg)
{
    FUNC_ENTER_NOINIT(H5FP_object_lock_cmp);
    assert(o1);
    assert(o2);
    FUNC_LEAVE_NOAPI(HDmemcmp(o1->oid, o2->oid, sizeof(o1->oid)));
}

/*
 * Function:    H5FP_new_object_lock
 * Purpose:     Create a new object lock. The locks are keyed off of the
 *              OID when they're inserted into the TBBT. There's a
 *              reference count so the same process can request the lock
 *              multiple times, if need be. The rank of the requesting
 *              process is kept around so that we can determine who
 *              wanted it in the first place. RW_LOCK tells us what kind
 *              of lock it is -- READ or WRITE.
 * Return:      Success:    Pointer to SAP_OBJ_LOCK structure.
 *              Failure:    NULL
 * Programmer:  Bill Wendling, 09. September 2002
 * Modifications:
 */
static struct sap_obj_lock *
H5FP_new_object_lock(const unsigned char *oid, unsigned int rank,
                     enum sap_obj_type obj_type, enum sap_lock_type rw_lock)
{
    struct sap_obj_lock *ret_value = NULL;
    
    FUNC_ENTER_NOINIT(H5FP_new_object_lock);
    assert(oid);

    if ((ret_value = (struct sap_obj_lock *)HDmalloc(sizeof(struct sap_obj_lock))) == NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "out of memory");

    H5FP_COPY_OID(ret_value->oid, oid);
    ret_value->owned_rank = rank;
    ret_value->obj_type = obj_type;
    ret_value->ref_count = 1;
    ret_value->rw_lock = rw_lock;

done:
    FUNC_LEAVE_NOAPI(ret_value);
}

/*
 * Function:    H5FP_free_object_lock
 * Purpose:     Free up the space allocated for the object lock.
 * Return:      SUCCEED (never fails)
 * Programmer:  Bill Wendling, 09. September 2002
 * Modifications:
 */
static herr_t
H5FP_free_object_lock(struct sap_obj_lock *ol)
{
    FUNC_ENTER_NOINIT(H5FP_free_object_lock);
    HDfree(ol);
    FUNC_LEAVE_NOAPI(SUCCEED);
}

/*
 * Function:    H5FP_find_object_lock
 * Purpose:     Find the object lock for the given OID if there is one.
 * Return:      Pointer to the object or NULL.
 * Programmer:  Bill Wendling, 09. September 2002
 * Modifications:
 */
static struct sap_obj_lock *
H5FP_find_object_lock(struct sap_file_struct *fs, unsigned char *oid)
{
    struct sap_obj_lock *ret_value = NULL;

    FUNC_ENTER_NOINIT(H5FP_find_object_lock);

    assert(fs);
    assert(oid);

    if (fs->locks && fs->locks->root) {
        H5TB_NODE *node;
        struct sap_obj_lock ol;

        H5FP_COPY_OID(ol.oid, oid);

        if ((node = H5TB_dfind(fs->locks, (void *)&ol, NULL)) == NULL)
            HGOTO_ERROR(H5E_FPHDF5, H5E_NOTFOUND, NULL, "lock not found");

        ret_value = (struct sap_obj_lock *)node->data;
    }

done:
    FUNC_LEAVE_NOAPI(ret_value);
}

/*
 * Function:    H5FP_remove_object_lock_from_list
 * Purpose:     Remove the object lock from the file structure's lock
 *              list.
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 * Programmer:  Bill Wendling, 09. September 2002
 * Modifications:
 */
static herr_t
H5FP_remove_object_lock_from_list(struct sap_file_struct *fs,
                                  struct sap_obj_lock *ol)
{
    H5TB_NODE *node;
    herr_t ret_value = FAIL;

    FUNC_ENTER_NOINIT(H5FP_remove_object_lock_from_list);

    if ((node = H5TB_dfind(fs->locks, (void *)ol, NULL)) != NULL) {
        H5FP_free_object_lock(H5TB_rem(&fs->locks->root, node, NULL));
        ret_value = SUCCEED;
    }

    FUNC_LEAVE_NOAPI(ret_value);
}

/*
 * Function:    H5FP_file_mod_cmp
 * Purpose:     Comparison function for the TBBT.
 * Return:      <0, 0, or >0
 * Programmer:  Bill Wendling, 27. August, 2002
 * Modifications:
 */
static int
H5FP_file_mod_cmp(struct sap_file_mod *k1, struct sap_file_mod *k2, int UNUSED cmparg)
{
    FUNC_ENTER_NOINIT(H5FP_file_mod_cmp);
    assert(k1);
    assert(k2);
    FUNC_LEAVE_NOAPI(k1->key - k2->key);
}

/*
 * Function:    H5FP_new_file_mod_key
 * Purpose:     Generate a new key for the sap_file_mode structure.
 * Return:      Success:    New key value.
 *              Failure:    Doesn't.
 * Programmer:  Bill Wendling, 27. August, 2002
 * Modifications:
 */
static unsigned long
H5FP_new_file_mod_key(void)
{
    static unsigned long key = 0;

    FUNC_ENTER_NOINIT(H5FP_new_file_mod_key);
    FUNC_LEAVE_NOAPI(key++);
}

/*
 * Function:    H5FP_free_mod_node
 * Purpose:     Helper function to free up an SAP_FILE_MOD node and all
 *              of the malloced space it has.
 * Return:      Success:    SUCCEED
 *              Failure:    Doesn't fail
 * Programmer:  Bill Wendling, 31. July, 2002
 * Modifications:
 */
static herr_t
H5FP_free_mod_node(struct sap_file_mod *fs)
{
    FUNC_ENTER_NOINIT(H5FP_free_mod_node);

    if (fs) {
        HDfree(fs->procs_notified);
        HDfree(fs->metadata);
        HDfree(fs);
    }

    FUNC_LEAVE_NOAPI(SUCCEED);
}

/*
 * Function:    H5FP_new_file_mod_node
 * Purpose:     Create a new sap_file_mod node and initialize it.
 * Return:      Success: Pointer to new sap_file_mod structure.
 *              Failure: NULL
 * Programmer:  Bill Wendling, 02. August, 2002
 * Modifications:
 */
static struct sap_file_mod *
H5FP_new_file_mod_node(unsigned rank, enum sap_obj_type obj_type,
                       enum sap_action action, int md_len, char *metadata)
{
    struct sap_file_mod *ret_value = NULL;

    FUNC_ENTER_NOINIT(H5FP_new_file_mod_node);

    ret_value = (struct sap_file_mod *)HDmalloc(sizeof(struct sap_file_mod));

    if (!ret_value)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "out of memory");

    ret_value->procs_notified = (unsigned char *)HDcalloc((size_t)1 +
                                                   (H5FP_comm_size - 1) / H5FP_BYTE_BITS, 1);

    if (!ret_value->procs_notified) {
        HDfree(ret_value);
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "out of memory");
    }

    /* we'll just assume the SAP has been notified of this change :) */
    SET_PROC_NOTIFIED(ret_value, H5FP_sap_rank);
    SET_PROC_NOTIFIED(ret_value, rank);
    ret_value->obj_type = obj_type;
    ret_value->action = action;
    ret_value->md_len = md_len;
    ret_value->metadata = metadata;
    ret_value->key = H5FP_new_file_mod_key();

done:
    FUNC_LEAVE_NOAPI(ret_value);
}

/*
 * Function:    H5FP_add_file_mod_to_list
 * Purpose:     Add a modification to a file ID.
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 * Programmer:  Bill Wendling, 02. August, 2002
 * Modifications:
 */
static herr_t
H5FP_add_file_mod_to_list(struct sap_file_struct *fs, enum sap_obj_type obj_type,
                          enum sap_action action, unsigned rank, int md_len,
                          char *metadata)
{
    struct sap_file_mod *fm;
    herr_t ret_value = FAIL;

    FUNC_ENTER_NOINIT(H5FP_add_file_mod_to_list);

    assert(fs);

    if ((fm = H5FP_new_file_mod_node(rank, obj_type, action, md_len, metadata)) != NULL) {
        if (!H5TB_dins(fs->mod_tree, (void *)fm, NULL))
            HGOTO_ERROR(H5E_FPHDF5, H5E_CANTINSERT, FAIL, "can't insert modification into tree");

        ret_value = SUCCEED;
    }

done:
    FUNC_LEAVE_NOAPI(ret_value);
}

/*
 * Function:    H5FP_remove_mod_from_list
 * Purpose:     Remove a modification from the list of modifications on
 *              this file.
 *
 *              NOTE: Don't call this function directly. This should only be
 *              called after all processes for a particular modification have
 *              been synced.
 *
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 * Programmer:  Bill Wendling, 31. July, 2002
 * Modifications:
 */
static herr_t
H5FP_remove_mod_from_list(struct sap_file_struct *fs, struct sap_file_mod *mod)
{
    H5TB_NODE *node;
    herr_t ret_value = FAIL;

    FUNC_ENTER_NOINIT(H5FP_remove_mod_from_list);

    if ((node = H5TB_dfind(fs->mod_tree, (void *)mod, NULL)) != NULL) {
        H5FP_free_mod_node(H5TB_rem(&fs->mod_tree->root, node, NULL));
        ret_value = SUCCEED;
    }

    FUNC_LEAVE_NOAPI(ret_value);
}

/*
 * Function:    H5FP_free_file_struct_node
 * Purpose:     Helper function to free up an SAP_FILE_STRUCT node and all
 *              of the malloced space it has.
 *
 *              Note: It is an error to call this function before all
 *              modifications have been synced with all processes in the
 *              H5FP_SAP_COMM.
 * Return:      SUCCEED (never fails)
 * Programmer:  Bill Wendling, 31. July, 2002
 * Modifications:
 */
static herr_t
H5FP_free_file_struct_node(struct sap_file_struct *fs)
{
    FUNC_ENTER_NOINIT(H5FP_free_file_struct_node);

    if (fs) {
        H5TB_dfree(fs->mod_tree, (void (*)(void*))H5FP_free_mod_node, NULL);
        H5TB_dfree(fs->locks, (void (*)(void*))H5FP_free_object_lock, NULL);
        HDfree(fs->filename);
        HDfree(fs);
    }

    FUNC_LEAVE_NOAPI(SUCCEED);
}

/*
 * Function:    H5FP_file_struct_cmp
 * Purpose:     Compare two sap_file_struct structs for the TBBT.
 * Return:      <0, 0, >0
 * Programmer:  Bill Wendling, 27. August, 2002
 * Modifications:
 */
static int
H5FP_file_struct_cmp(struct sap_file_struct *k1, struct sap_file_struct *k2, int UNUSED cmparg)
{
    FUNC_ENTER_NOINIT(H5FP_file_struct_cmp);
    assert(k1);
    assert(k2);
    FUNC_LEAVE_NOAPI(k1->sap_file_id - k2->sap_file_id);
}

/*
 * Function:    H5FP_new_file_struct_node
 * Purpose:     Create and initialize an sap_file_struct node.
 * Return:      Success: Pointer to new node
 *              Failure: NULL
 * Programmer:  Bill Wendling, 02. August, 2002
 * Modifications:
 */
static struct sap_file_struct *
H5FP_new_file_struct_node(unsigned int sap_file_id, char *filename)
{
    struct sap_file_struct *ret_value;

    FUNC_ENTER_NOINIT(H5FP_new_file_struct_node);

    if ((ret_value = (struct sap_file_struct *)HDmalloc(sizeof(struct sap_file_struct))) == NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "out of memory");

    ret_value->sap_file_id = sap_file_id;
    ret_value->filename = filename;
    ret_value->closing = FALSE;
    ret_value->mod_tree = NULL;
    ret_value->locks = NULL;

    if ((ret_value->mod_tree = H5TB_dmake((H5TB_cmp_t)H5FP_file_mod_cmp,
                                          sizeof(struct sap_file_mod), FALSE)) == NULL) {
        H5FP_free_file_struct_node(ret_value);
        HGOTO_ERROR(H5E_FPHDF5, H5E_CANTMAKETREE, NULL, "cannot make TBBT tree");
    }

    if ((ret_value->locks = H5TB_dmake((H5TB_cmp_t)H5FP_object_lock_cmp,
                                       sizeof(struct sap_obj_lock), FALSE)) == NULL) {
        H5FP_free_file_struct_node(ret_value);
        HGOTO_ERROR(H5E_FPHDF5, H5E_CANTMAKETREE, NULL, "cannot make TBBT tree");
    }

done:
    FUNC_LEAVE_NOAPI(ret_value);
}

/*
 * Function:    H5FP_find_file_struct
 * Purpose:     Find the file structure for the requested sap_file_id.
 * Return:      Success:    Pointer to the file structure
 *              Failure:    NULL
 * Programmer:  Bill Wendling, 31. July, 2002
 * Modifications:
 */
static struct sap_file_struct *
H5FP_find_file_struct(unsigned int sap_file_id)
{
    H5TB_NODE *node = NULL;
    struct sap_file_struct *ret_value = NULL;

    FUNC_ENTER_NOINIT(H5FP_find_file_struct);

    if (fs_tree && fs_tree->root) {
        struct sap_file_struct s;

        s.sap_file_id = sap_file_id;

        if ((node = H5TB_dfind(fs_tree, (void *)&s, NULL)) != NULL)
            ret_value = (struct sap_file_struct *)node->data;
    }

    FUNC_LEAVE_NOAPI(ret_value);
}

/*
 * Function:    H5FP_add_new_file_struct_to_list
 * Purpose:     Add an SAP_FILE_ID to the list of file IDS.
 * Return:      SUCCEED if the node was added
 *              FAIL otherwise
 * Programmer:  Bill Wendling, 02. August, 2002
 * Modifications:
 */
static herr_t
H5FP_add_new_file_struct_to_list(unsigned int sap_file_id, char *filename)
{
    struct sap_file_struct *fs;
    herr_t ret_value = FAIL;

    FUNC_ENTER_NOINIT(H5FP_add_new_file_struct_to_list);

    if ((fs = H5FP_new_file_struct_node(sap_file_id, filename)) != NULL) {
        if (!H5TB_dins(fs_tree, (void *)fs, NULL))
            HGOTO_ERROR(H5E_FPHDF5, H5E_CANTINSERT, FAIL,
                        "can't insert file structure into tree");

        ret_value = SUCCEED;
    }

done:
    FUNC_LEAVE_NOAPI(ret_value);
}

/*
 * Function:    H5FP_remove_file_id_from_list
 * Purpose:     Remove an SAP_FILE_ID from the list of file IDS.
 *
 *              NOTE: This should only be called after all modifications to
 *              the file descriptor have been synced to all processes and the
 *              file has been closed by all processes.
 *
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 * Programmer:  Bill Wendling, 31. July, 2002
 * Modifications:
 */
static herr_t
H5FP_remove_file_id_from_list(unsigned int sap_file_id)
{
    struct sap_file_struct s;
    H5TB_NODE *node;
    herr_t ret_value = FAIL;

    FUNC_ENTER_NOINIT(H5FP_remove_file_id_from_list);

    s.sap_file_id = sap_file_id;

    if ((node = H5TB_dfind(fs_tree, (void *)&s, NULL)) != NULL) {
        H5FP_free_file_struct_node(H5TB_rem(&fs_tree->root, node, NULL));
        ret_value = SUCCEED;
    }

    FUNC_LEAVE_NOAPI(ret_value);
}

/** Functions to reply to requests **/

static herr_t
H5FP_send_reply(unsigned int to, unsigned int req_id, unsigned int file_id,
                enum sap_status status)
{
    struct SAP_reply reply;
    herr_t ret_value = SUCCEED;
    int mrc;

    FUNC_ENTER_NOINIT(H5FP_send_reply);

    reply.req_id = req_id;
    reply.sap_file_id = file_id;
    reply.status = status;

    if ((mrc = MPI_Send(&reply, 1, SAP_reply_t, (int)to, H5FP_TAG_REPLY,
                        H5FP_SAP_COMM)) != MPI_SUCCESS)
        HMPI_GOTO_ERROR(FAIL, "MPI_Send failed", mrc);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}

/** Functions to handle SAP requests **/

/*
 * Function:    H5FP_gen_request_id
 * Purpose:     Generate a unique request ID to send along with a
 *              message.
 * Return:      Integer >= 0 - Doesn't fail.
 * Programmer:  Bill Wendling, 30. July, 2002
 * Modifications:
 */
static unsigned int
H5FP_gen_sap_file_id()
{
    static unsigned int i = 0;
    FUNC_ENTER_NOINIT(H5FP_gen_sap_file_id);
    FUNC_LEAVE_NOAPI(i++);
}

/*
 * Function:    H5FP_sap_handle_open_request
 * Purpose:     Handle a request to open a file.
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 * Programmer:  Bill Wendling, 06. August, 2002
 * Modifications:
 */
static herr_t
H5FP_sap_handle_open_request(struct SAP_request req, char *mdata, int UNUSED md_len)
{
    herr_t ret_value = SUCCEED;
    int mrc;

    FUNC_ENTER_NOINIT(H5FP_sap_handle_open_request);

    if (req.obj_type == H5FP_OBJ_FILE) {
        unsigned new_file_id = H5FP_gen_sap_file_id();
        int i;

        if (H5FP_add_new_file_struct_to_list(new_file_id, mdata) != SUCCEED)
            HGOTO_ERROR(H5E_FPHDF5, H5E_CANTINSERT, FAIL,
                        "can't insert new file structure to list");

        /* broadcast the file id to all processes */
        /* FIXME: Isn't there some way to broadcast this result to the barrier group? -QAK */
        /*
         * XXX:
         *      MPI_Bcast doesn't work in this way and I don't know how
         *      to get it to work for us. From what I gather, all of the
         *      processes need to execute the same bit of code (the
         *      MPI_Bcast function) to get the value to be passed to
         *      everyone. -BW
         */
        for (i = 0; i < H5FP_comm_size; ++i)
            if ((unsigned)i != H5FP_sap_rank)
                if ((mrc = MPI_Send(&new_file_id, 1, MPI_UNSIGNED, i,
                                    H5FP_TAG_FILE_ID, H5FP_SAP_COMM)) != MPI_SUCCESS)
                    /*
                     * FIXME: This is terrible...if we can't send to all
                     * processes, we should clean the file structure from
                     * the list and tell all of the other processes that
                     * we couldn't continue...but how to do that?!?
                     */
                    HMPI_GOTO_ERROR(FAIL, "MPI_Send failed", mrc);
    }

done:
    FUNC_LEAVE_NOAPI(ret_value);
}

/*
 * Function:    H5FP_sap_handle_lock_request
 * Purpose:     Handle a request to lock an object. There are two
 *              different kinds of locks. There are READ and WRITE locks.
 *              The READ locks are sharable, that is if all processes want
 *              to READ the object, they can. They just tell the SAP that
 *              they're doing so and the SAP gives them a "token" to do
 *              that. WRITE locks, on the other hand, are exclusive. You
 *              can't have any outstanding READ or WRITE locks on an
 *              object before you get a WRITE lock on it.
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 * Programmer:  Bill Wendling, 06. August, 2002
 * Modifications:
 */
static herr_t
H5FP_sap_handle_lock_request(struct SAP_request req)
{
    struct lock_group {
        unsigned char oid[sizeof(req.oid)];
        unsigned int file_id;
        unsigned int locked;
        enum sap_lock_type rw_lock;
        struct sap_file_struct *fs;
        struct sap_obj_lock *lock;
    } *oids;
    int list_size = 2;  /* the size of the "oids" list */
    enum sap_status exit_state = H5FP_STATUS_LOCK_ACQUIRED;
    herr_t ret_value = SUCCEED;
    int i, j;

    FUNC_ENTER_NOINIT(H5FP_sap_handle_lock_request);

    if ((oids = (struct lock_group *)HDmalloc(list_size * sizeof(struct lock_group))) == NULL) {
        exit_state = H5FP_STATUS_OOM;
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "out of memory");
    }

    /*
     * Gather together all of the OIDs the process is requesting to lock
     * at one time.
     */
    for (i = 0;; ++i) {
        if (req.oid[0]) {
            if (i == list_size) {
                list_size *= 2;
                oids = HDrealloc(oids, list_size * sizeof(struct lock_group));

                if (!oids) {
                    exit_state = H5FP_STATUS_OOM;
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "out of memory");
                }
            }

            H5FP_COPY_OID(oids[i].oid, req.oid);
            oids[i].file_id = req.sap_file_id;
            oids[i].rw_lock = req.rw_lock;
            oids[i].locked = FALSE;
        }

        if (req.req_type == H5FP_REQ_LOCK_END)
            /* this was the last lock request */
            break;

        if (H5FP_sap_receive(&req, (int)req.proc_rank,
                             H5FP_TAG_REQUEST, NULL) != SUCCEED) {
            exit_state = H5FP_STATUS_LOCK_FAILED;
            HGOTO_ERROR(H5E_FPHDF5, H5E_CANTRECV, FAIL, "cannot receive messages");
        }
    }

    /*
     * Check to see if we can acquire all of the locks requested. If
     * not, the it's an error and we need to return.
     */
    for (j = 0; j <= i; ++j) {
        if ((oids[j].fs = H5FP_find_file_struct(oids[j].file_id)) == NULL) {
            /* FIXME: the file ID is invalid */
            exit_state = H5FP_STATUS_BAD_FILE_ID;
            HGOTO_DONE(FAIL);
        }
        
        if (oids[j].fs->closing) {
            /* we're closing the file - don't accept anymore locks */
            exit_state = H5FP_STATUS_FILE_CLOSING;
            HGOTO_DONE(FAIL);
        }

        oids[j].lock = H5FP_find_object_lock(oids[j].fs, oids[j].oid);

        /*
         * Don't panic!
         *
         * This horrid little if-then statement is just the logical
         * inverse of the if-then statement in the next for-loop.
         */
        if (oids[j].lock &&
               !((oids[j].rw_lock == H5FP_LOCK_READ &&
                        oids[j].lock->rw_lock == H5FP_LOCK_READ) ||
                 (oids[j].rw_lock == H5FP_LOCK_WRITE &&
                        oids[j].lock->rw_lock == H5FP_LOCK_WRITE &&
                        oids[j].lock->owned_rank == req.proc_rank))) {
            /* FAILURE */
            exit_state = H5FP_STATUS_LOCK_FAILED;
            HGOTO_DONE(FAIL);
        }
    }

    /*
     * Actually acquire the locks. This shouldn't fail because of the
     * previous check. The only thing which can likely occur is an
     * out-of-memory error.
     */
    for (j = 0; j <= i; ++j) {
        if (oids[j].lock) {
            /*
             * Don't panic!
             *
             * This horrid little if-then statement is just checking so
             * that if you want a READ lock and the current lock is a
             * READ lock, then we bump up the reference count on it. If
             * you want a WRITE lock and the current lock is a WRITE lock
             * and furthermore the current process has that lock, we will
             * also bump up the reference count.
             *
             * Otherwise, it's a failure.
             */
            if ((oids[j].rw_lock == H5FP_LOCK_READ &&
                    oids[j].lock->rw_lock == H5FP_LOCK_READ) ||
                (oids[j].rw_lock == H5FP_LOCK_WRITE &&
                    oids[j].lock->rw_lock == H5FP_LOCK_WRITE &&
                    oids[j].lock->owned_rank == req.proc_rank)) {
                /*
                 * The requesting process may already have this lock. Might
                 * be a request from some call-back function of some sort.
                 * Increase the reference count (which is decreased when
                 * released) to help to prevent deadlocks.
                 */
                ++oids[j].lock->ref_count;
                oids[j].locked = TRUE;
            } else {
                /* FIXME: reply saying object locked? */
                exit_state = H5FP_STATUS_LOCK_FAILED;
                ret_value = FAIL;
                goto rollback;
            }
        } else {
            struct sap_obj_lock *lock = H5FP_new_object_lock(oids[j].oid, req.proc_rank,
                                                             req.obj_type, req.rw_lock);

            if (lock) {
                if (!H5TB_dins(oids[j].fs->locks, (void *)lock, NULL)) {
                    H5FP_free_object_lock(lock);
                    exit_state = H5FP_STATUS_LOCK_FAILED;
                    ret_value = FAIL;
                    HCOMMON_ERROR(H5E_FPHDF5, H5E_CANTINSERT, "can't insert lock into tree");
                    goto rollback;
                }

                oids[j].locked = TRUE;
            } else {
                /* out of memory...ulp! */
                exit_state = H5FP_STATUS_OOM;
                ret_value = FAIL;
                HCOMMON_ERROR(H5E_RESOURCE, H5E_NOSPACE, "out of memory");
                goto rollback;
            }
        }
    }

    goto done;

    /* Error handling code */
rollback:
    /*
     * More than likely, out of memory during the actual locking phase.
     * Try to release any locks which may have been obtained. If it's not
     * possible to release those locks, we're in big trouble. The file is
     * now in an inconsistent state, as far as the SAP is concerned. The
     * only options left to the program are either to abort or completely
     * close the file and reopen which could cause corruption.
     */
    for (j = 0; j <= i; ++j) {
        if (oids[j].locked) {
            if (oids[j].lock) {
                if (oids[j].lock->owned_rank == req.proc_rank) {
                    if (--oids[j].lock->ref_count == 0) {
                        H5FP_remove_object_lock_from_list(oids[j].fs, oids[j].lock);
                    }
                } else {
                    /* CATASTROPHIC FAILURE!!! */
                    /* LOCK WAS NOT CLEARED */
                    exit_state = H5FP_STATUS_CATASTROPHIC;
                }
            } else {
                /* CATASTROPHIC FAILURE!!! */
                /* LOCK WAS NOT CLEARED */
                exit_state = H5FP_STATUS_CATASTROPHIC;
            }
        }
    }

done:
    if (ret_value != SUCCEED) {
        /* Can't lock the whole group at one time for some reason */
HDfprintf(stderr, "%s: locking failure (%d)!!\n", FUNC, ret_value);
    }

    HDfree(oids);
    H5FP_send_reply(req.proc_rank, req.req_id, req.sap_file_id, exit_state);
    FUNC_LEAVE_NOAPI(ret_value);
} 

/*
 * Function:    H5FP_sap_handle_release_lock_request
 * Purpose:     Handle a request to release the lock on an object.
 * Return:      Nothing
 * Programmer:  Bill Wendling, 06. August, 2002
 * Modifications:
 */
static herr_t
H5FP_sap_handle_release_lock_request(struct SAP_request req)
{
    struct release_group {
        unsigned char oid[sizeof(req.oid)];
        unsigned int file_id;
        struct sap_file_struct *fs;
        struct sap_obj_lock *lock;
    } *oids;
    int list_size = 2;  /* the size of the "oids" list */
    enum sap_status exit_state = H5FP_STATUS_LOCK_RELEASED;
    herr_t ret_value;
    int i, j;

    FUNC_ENTER_NOINIT(H5FP_sap_handle_release_lock_request);

    if ((oids = (struct release_group *)HDmalloc(list_size *
                                                 sizeof(struct release_group))) == NULL) {
        exit_state = H5FP_STATUS_OOM;
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "out of memory");
    }

    /*
     * Gather together all of the OIDs the process is requesting to
     * release locks at one time.
     */
    for (i = 0;; ++i) {
        if (req.oid[0]) {
            if (i == list_size) {
                list_size *= 2;
                oids = HDrealloc(oids, list_size * sizeof(struct release_group));

                if (!oids) {
                    exit_state = H5FP_STATUS_OOM;
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "out of memory");
                }
            }

            H5FP_COPY_OID(oids[i].oid, req.oid);
            oids[i].file_id = req.sap_file_id;
        }

        if (req.req_type == H5FP_REQ_RELEASE_END)
            /* this was the last lock request */
            break;

        if (H5FP_sap_receive(&req, (int)req.proc_rank, H5FP_TAG_REQUEST, NULL) != SUCCEED) {
            exit_state = H5FP_STATUS_LOCK_RELEASE_FAILED;
            HGOTO_DONE(FAIL);
        }
    }

    /*
     * Check here to see if the locks exist and we have the locks. This
     * will help keep us from being in a catastrophic state.
     */
    for (j = 0; j <= i; ++j) {
        if ((oids[j].fs = H5FP_find_file_struct(oids[j].file_id)) == NULL) {
            /* FIXME: the file ID is invalid */
            exit_state = H5FP_STATUS_BAD_FILE_ID;
            HGOTO_DONE(FAIL);
        }

        oids[j].lock = H5FP_find_object_lock(oids[j].fs, oids[j].oid);

        if (!oids[j].lock || oids[j].lock->owned_rank != req.proc_rank) {
            exit_state = H5FP_STATUS_BAD_LOCK;
            HGOTO_DONE(FAIL);
        }
    }

    /*
     * Release a lock. There may be multiple locks to release if they
     * were locked in a group, so loop finding all of the locks and
     * release them.
     */
    for (j = 0; j <= i; ++j) {
        if (oids[j].lock) {
            if (oids[j].lock->owned_rank == req.proc_rank) {
                if (--oids[j].lock->ref_count == 0)
                    H5FP_remove_object_lock_from_list(oids[j].fs, oids[j].lock);
            } else {
                /* AAAIIIIEEE!!! */
                exit_state = H5FP_STATUS_CATASTROPHIC;
                HGOTO_DONE(FAIL);
            }
        } else {
            /* AAAIIIIEEE!!! */
            exit_state = H5FP_STATUS_CATASTROPHIC;
            HGOTO_DONE(FAIL);
        }
    }

done:
HDfprintf(stderr, "exit_state == %d\n", exit_state);
    HDfree(oids);
    H5FP_send_reply(req.proc_rank, req.req_id, req.sap_file_id, exit_state);
    FUNC_LEAVE_NOAPI(ret_value);
}

/*
 * Function:    H5FP_sap_handle_change_request
 * Purpose:     Handle a request to change the structure of an object.
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 * Programmer:  Bill Wendling, 06. August, 2002
 * Modifications:
 */
static herr_t
H5FP_sap_handle_change_request(struct SAP_request req, char *mdata, int md_len)
{
    struct sap_file_struct *fs;
    enum sap_status exit_state = H5FP_STATUS_OK;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOINIT(H5FP_sap_handle_change_request);

    if ((fs = H5FP_find_file_struct(req.sap_file_id)) != NULL) {
        if (fs->closing) {
            /* we're closing the file - don't accept anymore changes */
            exit_state = H5FP_STATUS_FILE_CLOSING;
            ret_value = FAIL;
        } else {
            /* handle the change request */
            struct sap_obj_lock *lock = H5FP_find_object_lock(fs, req.oid);

            if (!lock || lock->owned_rank != req.proc_rank
                    || lock->rw_lock != H5FP_LOCK_WRITE) {
                /*
                 * There isn't a write lock or we don't own the write
                 * lock on this OID
                 */
                exit_state = H5FP_STATUS_NO_LOCK;
                ret_value = FAIL;
            } else if (H5FP_add_file_mod_to_list(fs, req.obj_type, req.action,
                                                 req.proc_rank, md_len, mdata) != SUCCEED) {
                exit_state = H5FP_STATUS_OOM;
                ret_value = FAIL;
            }
        }
    } else {
        /* error: there isn't a file opened to change */
        exit_state = H5FP_STATUS_BAD_FILE_ID;
        ret_value = FAIL;
    }

    H5FP_send_reply(req.proc_rank, req.req_id, req.sap_file_id, exit_state);
    FUNC_LEAVE_NOAPI(ret_value);
}

/*
 * Function:    H5FP_sap_handle_sync_request
 * Purpose:     Handle a request to sync changes to a file with the requesting
 *              process.
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 * Programmer:  Bill Wendling, 06. August, 2002
 * Modifications:
 */
static herr_t
H5FP_sap_handle_sync_request(struct SAP_request req)
{
    struct sap_file_struct *fs;
    struct SAP_sync s;
    herr_t ret_value = SUCCEED;
    int mrc, sync_id = 0;

    FUNC_ENTER_NOINIT(H5FP_sap_handle_sync_request);

HDfprintf(stderr, "%s: Trying to Synchronize!\n", FUNC);

    s.req_id = req.req_id;
    s.sap_file_id = req.sap_file_id;
    s.last_msg = FALSE;
    s.md_len = 0;
    s.obj_type = 0;
    s.action = 0;
    s.status = H5FP_STATUS_OK;

    if ((fs = H5FP_find_file_struct(req.sap_file_id)) != NULL) {
        if (fs->mod_tree->root) {
            H5TB_NODE *mod_node = H5TB_first(fs->mod_tree->root);

            while (mod_node) {
                struct sap_file_mod *mod = (struct sap_file_mod *)mod_node->data;

                if (mod && !IS_PROC_NOTIFIED(mod, req.proc_rank)) {
                    s.sync_id = sync_id++;
                    s.md_len = mod->md_len;
                    s.obj_type = mod->obj_type;
                    s.action = mod->action;

                    if ((mrc = MPI_Send(&s, 1, SAP_sync_t, (int)req.proc_rank,
                                        H5FP_TAG_SYNC, H5FP_SAP_COMM)) != MPI_SUCCESS)
                        HMPI_GOTO_ERROR(FAIL, "MPI_Send failed", mrc);


                    if (H5FP_send_metadata(mod->metadata, mod->md_len, (int)req.proc_rank)
                            != SUCCEED)
                        HGOTO_ERROR(H5E_FPHDF5, H5E_CANTSENDMDATA, FAIL,
                                    "can't send metadata to client");

                    SET_PROC_NOTIFIED(mod, req.proc_rank);
                }

                /*
                 * check to see if this modification has been sent to all
                 * clients
                 */
                if (mod->num_notified == (unsigned)H5FP_comm_size)
                    if (H5FP_remove_mod_from_list(fs, mod) == FAIL)
                        HGOTO_ERROR(H5E_FPHDF5, H5E_NOTFOUND, FAIL, "cannot remove modification");

                mod_node = H5TB_next(mod_node);
            }
        }
    } else {
        s.status = H5FP_STATUS_BAD_FILE_ID;
    }

    s.sync_id = sync_id;
    s.last_msg = TRUE;

    if ((mrc = MPI_Send(&s, 1, SAP_sync_t, (int)req.proc_rank,
                        H5FP_TAG_SYNC, H5FP_SAP_COMM)) != MPI_SUCCESS)
        HMPI_GOTO_ERROR(FAIL, "MPI_Send failed", mrc);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}

/*
 * Function:    H5FP_sap_handle_close_request
 * Purpose:     Handle a request to close a file.
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 * Programmer:  Bill Wendling, 06. August, 2002
 * Modifications:
 */
static herr_t
H5FP_sap_handle_close_request(struct SAP_request req)
{
    struct sap_file_struct *fs;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOINIT(H5FP_sap_handle_close_request);

    if ((fs = H5FP_find_file_struct(req.sap_file_id)) != NULL) {
        /* do a sync with the closing process */
        if (H5FP_sap_handle_sync_request(req) != SUCCEED)
            HGOTO_ERROR(H5E_FPHDF5, H5E_NOTFOUND, FAIL, "couldn't sync with process");

        if (++fs->closing == H5FP_comm_size - 1)
            /* all processes have closed the file - remove it from list */
            if (H5FP_remove_file_id_from_list(req.sap_file_id) != SUCCEED)
                HGOTO_ERROR(H5E_FPHDF5, H5E_NOTFOUND, FAIL, "cannot remove file ID from list");
    }

done:
    FUNC_LEAVE_NOAPI(ret_value);
}

#endif  /* H5_HAVE_FPHDF5 */
