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


/* Private header files */
#include "H5private.h"          /* Generic Functions                    */
#include "H5ACprivate.h"        /* Metadata Cache                       */
#include "H5Eprivate.h"         /* Error Handling                       */
#include "H5FDprivate.h"        /* File Driver                          */
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Oprivate.h"         /* Object Headers                       */
#include "H5Pprivate.h"         /* Property List                        */
#include "H5Rprivate.h"         /* References                           */
#include "H5SLprivate.h"	/* Skip lists				*/

#ifdef H5_HAVE_FPHDF5

#include "H5FDfphdf5.h"         /* File Driver for FPHDF5               */
#include "H5FPprivate.h"        /* Flexible Parallel Functions          */

/* Local macros */
#define H5FP_DEFAULT_SKIPLIST_HEIGHT     8

/* Internal SAP structures */

/*===----------------------------------------------------------------------===
 *                            H5FP_object_lock
 *===----------------------------------------------------------------------===
 *
 * A lock on a given object. A list of current locks is kept in the
 * appropriate H5FP_file_info structure.
 */
typedef struct {
    hobj_ref_t      oid;        /* buffer to store OID of object            */
    unsigned char  *num_locks;  /* number of times a rank has a lock        */
    unsigned        num_procs;  /* number of processes that have the lock   */
    H5FP_obj_t      obj_type;   /* type of object being locked              */
    H5FP_lock_t     rw_lock;    /* indicates if it's a read or write lock   */
} H5FP_object_lock;

/*===----------------------------------------------------------------------===
 *                             H5FP_mdata_mod
 *===----------------------------------------------------------------------===
 *
 * A given modification (write) of metadata in the file. A list of
 * current modifications is kept in the appropriate H5FP_file_info
 * structure.
 */
typedef struct {
    H5FD_mem_t      mem_type;   /* type of memory updated                   */
    H5FP_obj_t      obj_type;   /* type of object modified                  */
    haddr_t         addr;       /* address of the metadata                  */
    unsigned        md_size;    /* size of the metadata                     */
    char           *metadata;   /* encoded metadata about the object        */
} H5FP_mdata_mod;

/*===----------------------------------------------------------------------===
 *                             H5FP_file_info
 *===----------------------------------------------------------------------===
 *
 * This has all the information the SAP cares about for a given file: a
 * copy of the H5FD_fphdf5_t structure for keeping track of metadata
 * allocations in the file, the file ID assigned by the SAP, whether the
 * file is in the process of being closed (and, therefore, can't accept
 * anymore modifications), a count of the number of modifications not
 * written to the file, a list of modifications (writes) made by clients
 * to the metadata, and a list of current locks on objects in the file.
 */
typedef struct {
    H5FD_fphdf5_t   file;       /* file driver structure                    */
    unsigned        file_id;    /* the file id the SAP keeps per file       */
    int             closing;    /* we're closing the file - no more changes */
    unsigned        num_mods;   /* number of mdata writes outstanding       */
    H5SL_t         *mod_list;   /* a list of metadata updates done          */
    H5SL_t         *locks;      /* a list of locks on objects in the file   */
} H5FP_file_info;

/*
 * This marks the point at which we want to dump all of the metadata
 * to a process so that that process can write them to the file.
 */
#define H5FP_MDATA_CACHE_HIGHWATER_MARK     1024

static H5SL_t *file_info_list;

/* local functions */
static herr_t H5FP_sap_receive(H5FP_request_t *req, int source, int tag, char **buf);

    /* local functions to generate unique ids for messages */
static unsigned H5FP_gen_sap_file_id(void);

    /* local functions for handling object locks */
static H5FP_object_lock *H5FP_new_object_lock(hobj_ref_t oid,
                                              unsigned rank,
                                              H5FP_obj_t obj_type,
                                              H5FP_lock_t rw_lock);
static herr_t H5FP_free_object_lock(H5FP_object_lock *ol);
static herr_t H5FP_free_object_lock_cb(void *item, void *key, void *op_data);
static H5FP_object_lock *H5FP_find_object_lock(H5FP_file_info *info,
                                               hobj_ref_t oid);
static herr_t H5FP_remove_object_lock_from_list(H5FP_file_info *info,
                                                H5FP_object_lock *ol);

    /* local file information handling functions */
static herr_t   H5FP_add_new_file_info_to_list(unsigned file_id,
                                               haddr_t maxaddr,
                                               unsigned long feature_flags,
                                               hsize_t meta_block_size,
                                               hsize_t sdata_block_size,
                                               hsize_t threshold,
                                               hsize_t alignment);
static herr_t   H5FP_remove_file_id_from_list(unsigned file_id);
static herr_t   H5FP_free_file_info_node(H5FP_file_info *info);
static H5FP_file_info  *H5FP_new_file_info_node(unsigned file_id);

    /* local file modification structure handling functions */
static H5FP_mdata_mod  *H5FP_new_file_mod_node(H5FD_mem_t mem_type,
                                               haddr_t addr,
                                               unsigned md_size,
                                               char *metadata);
static herr_t   H5FP_free_mod_node(H5FP_mdata_mod *info);
static herr_t   H5FP_free_mod_node_cb(void *item, void *key, void *op_data);

    /* local request handling functions */
static herr_t   H5FP_sap_handle_open_request(H5FP_request_t *req, unsigned md_size);
static herr_t   H5FP_sap_handle_lock_request(H5FP_request_t *req);
static herr_t   H5FP_sap_handle_release_lock_request(H5FP_request_t *req);
static herr_t   H5FP_sap_handle_read_request(H5FP_request_t *req);
static herr_t   H5FP_sap_handle_write_request(H5FP_request_t *req,
                                              char *mdata,
                                              unsigned md_size);
static herr_t   H5FP_sap_handle_flush_request(H5FP_request_t *req);
static herr_t   H5FP_sap_handle_close_request(H5FP_request_t *req);
static herr_t   H5FP_sap_handle_alloc_request(H5FP_request_t *req);
static herr_t   H5FP_sap_handle_free_request(H5FP_request_t *req);
static herr_t   H5FP_sap_handle_get_eoa_request(H5FP_request_t *req);
static herr_t   H5FP_sap_handle_set_eoa_request(H5FP_request_t *req);
static herr_t   H5FP_sap_handle_update_eoma_eosda_request(H5FP_request_t *req);

/*
 *===----------------------------------------------------------------------===
 *                    Public Library (non-API) Functions
 *===----------------------------------------------------------------------===
 */

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
    int comm_size;
    int stop = 0;
    H5FP_request_t req;

    FUNC_ENTER_NOAPI(H5FP_sap_receive_loop, FAIL)

    /* Get the size of the SAP communicator */
    if (MPI_Comm_size(H5FP_SAP_COMM, &comm_size) != MPI_SUCCESS)
        HGOTO_ERROR(H5E_INTERNAL, H5E_MPI, FAIL, "MPI_Comm_size failed")

    /* Create the file structure tree. */
    if ((file_info_list = H5SL_create(H5SL_TYPE_UNSIGNED,0.5,H5FP_DEFAULT_SKIPLIST_HEIGHT)) == NULL)
        HGOTO_ERROR(H5E_FPHDF5, H5E_CANTCREATE, FAIL, "cannot make skip list")

    for (;;) {
        char *buf = NULL;
        herr_t hrc;

        if (H5FP_sap_receive(&req, MPI_ANY_SOURCE, H5FP_TAG_REQUEST, &buf) != SUCCEED)
            HGOTO_ERROR(H5E_FPHDF5, H5E_CANTRECV, FAIL, "cannot receive messages")

        switch (req.req_type) {
        case H5FP_REQ_OPEN:
            if ((hrc = H5FP_sap_handle_open_request(&req, req.md_size)) != SUCCEED)
                HGOTO_ERROR(H5E_FPHDF5, H5E_CANTOPENOBJ, FAIL, "cannot open file")
            break;
        case H5FP_REQ_LOCK:
        case H5FP_REQ_LOCK_END:
            hrc = H5FP_sap_handle_lock_request(&req);
            break;
        case H5FP_REQ_RELEASE:
        case H5FP_REQ_RELEASE_END:
            hrc = H5FP_sap_handle_release_lock_request(&req);
            break;
        case H5FP_REQ_READ:
            hrc = H5FP_sap_handle_read_request(&req);
            break;
        case H5FP_REQ_WRITE:
            hrc = H5FP_sap_handle_write_request(&req, buf, req.md_size);
            break;
        case H5FP_REQ_FLUSH:
            hrc = H5FP_sap_handle_flush_request(&req);
            break;
        case H5FP_REQ_CLOSE:
            hrc = H5FP_sap_handle_close_request(&req);
            break;
        case H5FP_REQ_ALLOC:
            hrc = H5FP_sap_handle_alloc_request(&req);
            break;
        case H5FP_REQ_FREE:
            hrc = H5FP_sap_handle_free_request(&req);
            break;
        case H5FP_REQ_GET_EOA:
            hrc = H5FP_sap_handle_get_eoa_request(&req);
            break;
        case H5FP_REQ_SET_EOA:
            hrc = H5FP_sap_handle_set_eoa_request(&req);
            break;
        case H5FP_REQ_UPDATE_EOMA_EOSDA:
            hrc = H5FP_sap_handle_update_eoma_eosda_request(&req);
            break;
        case H5FP_REQ_STOP:
            hrc = SUCCEED;
            if (++stop == comm_size - 1)
                goto done;
            break;
        default:
            HGOTO_ERROR(H5E_FPHDF5, H5E_ARGS, FAIL, "invalid request type")
        }

        /*
         * If the above calls didn't succeed, free the buffer
         */
        if (hrc != SUCCEED) {
            HDfree(buf);
        }
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
}

/*
 * Function:    H5FP_sap_receive
 * Purpose:     Receive a message from SOURCE with the given TAG. The REQ
 *              object passed in as a pointer is filled by the MPI_Recv
 *              function.
 * Return:      Success:    Pointer to string passed in, if one was sent.
 *                          As well as the SAP_request object.
 *              Failure:    FAIL
 * Programmer:  Bill Wendling, 17. September, 2002
 * Modifications:
 */
static herr_t
H5FP_sap_receive(H5FP_request_t *req, int source, int tag, char **buf)
{
    MPI_Status status;
    herr_t ret_value = SUCCEED;
    int mrc;

    FUNC_ENTER_NOAPI_NOINIT(H5FP_sap_receive)

    HDmemset(&status, 0, sizeof(status));

    if ((mrc = MPI_Recv(req, 1, H5FP_request, source, tag,
                        H5FP_SAP_COMM, &status)) != MPI_SUCCESS)
        HMPI_GOTO_ERROR(FAIL, "MPI_Recv failed", mrc);

    if (buf && req->md_size)
        if (H5FP_read_metadata(buf, (int)req->md_size, (int)req->proc_rank) == FAIL)
            HGOTO_ERROR(H5E_FPHDF5, H5E_CANTRECV, FAIL, "can't read metadata from process")

done:
    FUNC_LEAVE_NOAPI(ret_value)
}

/*
 * Function:    H5FP_new_object_lock
 * Purpose:     Create a new object lock. The locks are keyed off of the
 *              OID when they're inserted into the skip list. There's a
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
static H5FP_object_lock *
H5FP_new_object_lock(hobj_ref_t oid, unsigned rank, H5FP_obj_t obj_type,
                     H5FP_lock_t rw_lock)
{
    int                 comm_size;
    H5FP_object_lock   *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT(H5FP_new_object_lock)

    if (MPI_Comm_size(H5FP_SAP_COMM, &comm_size) != MPI_SUCCESS)
        HGOTO_ERROR(H5E_INTERNAL, H5E_MPI, NULL, "MPI_Comm_size failed")

    if ((ret_value = (H5FP_object_lock *)H5MM_malloc(sizeof(H5FP_object_lock))) == NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "out of memory")

    if ((ret_value->num_locks = (unsigned char *)HDcalloc((size_t)comm_size, 1)) == NULL) {
        HDfree(ret_value);
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "out of memory")
    }

    ret_value->oid = oid;
    ret_value->num_locks[rank] = TRUE;
    ret_value->obj_type = obj_type;
    ret_value->num_procs = 1;
    ret_value->rw_lock = rw_lock;

done:
    FUNC_LEAVE_NOAPI(ret_value)
}

/*
 * Function:    H5FP_free_object_lock
 * Purpose:     Free up the space allocated for the object lock.
 * Return:      SUCCEED (never fails)
 * Programmer:  Bill Wendling, 09. September 2002
 * Modifications:
 */
static herr_t
H5FP_free_object_lock(H5FP_object_lock *ol)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5FP_free_object_lock)

    if (ol) {
        HDfree(ol->num_locks);
        HDfree(ol);
    }

    FUNC_LEAVE_NOAPI(SUCCEED)
}

/*
 * Function:    H5FP_free_object_lock_cb
 * Purpose:     Free up the space allocated for the object lock.
 * Purpose:     Helper function to call H5FP_free_object_lock when closing
 *              skip lists
 * Return:      SUCCEED (never fails)
 * Programmer:  Quincey Koziol, 30. December, 2004
 * Modifications:
 */
static herr_t
H5FP_free_object_lock_cb(void *item, void UNUSED *key, void UNUSED *op_data)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5FP_free_object_lock_cb)

    H5FP_free_object_lock(item);

    FUNC_LEAVE_NOAPI(SUCCEED)
}

/*
 * Function:    H5FP_find_object_lock
 * Purpose:     Find the object lock for the given OID if there is one.
 * Return:      Success:    Pointer to the object
 *              Failure:    NULL
 * Programmer:  Bill Wendling, 09. September 2002
 * Modifications:
 */
static H5FP_object_lock *
H5FP_find_object_lock(H5FP_file_info *info, hobj_ref_t oid)
{
    H5FP_object_lock *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT(H5FP_find_object_lock)

    assert(info);
    assert(oid);

    if (info->locks)
        if ((ret_value = H5SL_search(info->locks, &oid)) == NULL)
            HGOTO_ERROR(H5E_FPHDF5, H5E_NOTFOUND, NULL, "lock not found")

done:
    FUNC_LEAVE_NOAPI(ret_value)
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
H5FP_remove_object_lock_from_list(H5FP_file_info *info,
                                  H5FP_object_lock *ol)
{
    H5FP_object_lock *lock;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT(H5FP_remove_object_lock_from_list)

    if ((lock = H5SL_remove(info->locks, &ol->oid)) == NULL)
        HGOTO_ERROR(H5E_FPHDF5, H5E_NOTFOUND, FAIL, "lock not found")

    if(H5FP_free_object_lock(lock)<0)
        HGOTO_ERROR(H5E_FPHDF5, H5E_CANTFREE, FAIL, "can't release lock")

done:
    FUNC_LEAVE_NOAPI(ret_value)
}

/*
 * Function:    H5FP_free_mod_node
 * Purpose:     Helper function to free up an SAP_FILE_MOD node and all
 *              of the malloced space it has.
 * Return:      SUCCEED (doesn't fail)
 * Programmer:  Bill Wendling, 31. July, 2002
 * Modifications:
 */
static herr_t
H5FP_free_mod_node(H5FP_mdata_mod *info)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5FP_free_mod_node)

    if (info) {
        HDfree(info->metadata);
        HDfree(info);
    }

    FUNC_LEAVE_NOAPI(SUCCEED)
}

/*
 * Function:    H5FP_free_mod_node_cb
 * Purpose:     Helper function to call H5FP_free_mod_node when closing
 *              skip lists
 * Return:      SUCCEED (doesn't fail)
 * Programmer:  Quincey Koziol, 30. December, 2004
 * Modifications:
 */
static herr_t
H5FP_free_mod_node_cb(void *item, void UNUSED *key, void UNUSED *op_data)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5FP_free_mod_node_cb)

    H5FP_free_mod_node(item);

    FUNC_LEAVE_NOAPI(SUCCEED)
}

/*
 * Function:    H5FP_new_file_mod_node
 * Purpose:     Create a new sap_file_mod node and initialize it. This
 *              object now has responsibility for freeing the metadata
 *              information.
 * Return:      Success:    Pointer to new sap_file_mod structure.
 *              Failure:    NULL
 * Programmer:  Bill Wendling, 02. August, 2002
 * Modifications:
 */
static H5FP_mdata_mod *
H5FP_new_file_mod_node(H5FD_mem_t mem_type, haddr_t addr, unsigned md_size, char *metadata)
{
    H5FP_mdata_mod *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT(H5FP_new_file_mod_node)

    if ((ret_value = (H5FP_mdata_mod *)H5MM_malloc(sizeof(H5FP_mdata_mod))) == NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "out of memory")

    ret_value->mem_type = mem_type;
    ret_value->addr = addr;
    ret_value->md_size = md_size;
    ret_value->metadata = metadata;

done:
    FUNC_LEAVE_NOAPI(ret_value)
}

/*
 * Function:	H5FP_merge_mod_node_with_next
 *
 * Purpose:	Given a node in a mod tree which overlaps with the next
 *		node in the tree, merge the two.  Where the two nodes
 *		overlap, use the data from the supplied node.
 *
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 *
 * Programmer:	JRM - 3/18/04
 *
 * Modifications:
 *
 *		None.
 */
static herr_t
H5FP_merge_mod_node_with_next(H5SL_t *slist, H5SL_node_t *node_ptr)
{
    H5SL_node_t *next_node_ptr;
    H5FP_mdata_mod *mod_ptr;
    H5FP_mdata_mod *next_mod_ptr;
    herr_t ret_value=SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT(H5FP_merge_mod_node_with_next)

    /* check parameters & do some initializations in passing */
    HDassert(slist);
    HDassert(node_ptr);
    if ( ( (mod_ptr = H5SL_item(node_ptr)) == NULL ) ||
            ( (next_node_ptr = H5SL_next(node_ptr)) == NULL ) ||
            ( (next_mod_ptr = H5SL_item(next_node_ptr)) == NULL ) ||
            ( mod_ptr->addr >= next_mod_ptr->addr ) ||
            ( (mod_ptr->addr + mod_ptr->md_size) <= next_mod_ptr->addr ) )
        HGOTO_ERROR(H5E_FPHDF5, H5E_BADVALUE, FAIL, "One or more bad params detected on entry.")

    /* Check for partial overlap */
    if ( (mod_ptr->addr + mod_ptr->md_size) <
            (next_mod_ptr->addr + next_mod_ptr->md_size) ) {
        unsigned combined_md_size;
        char *combined_metadata_ptr;
        unsigned offset;
        unsigned i,j;

        /* The next node address range is not completely subsumed in
         * that of the current node.  Must allocate a new buffer, and
         * copy over the contents of the two buffers.  Where the buffers
         * overlap, give precidence to the data from *node_ptr
         */
        combined_md_size = (next_mod_ptr->addr + next_mod_ptr->md_size) -
                           mod_ptr->addr;

        combined_metadata_ptr = (char *)H5MM_malloc((size_t)(combined_md_size));
        if ( combined_metadata_ptr == NULL )
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate buffer for combined node.")

        i = 0; /* this is the index into the combined buffer */

        for ( j = 0; j < mod_ptr->md_size; j++ )
            combined_metadata_ptr[i++] = (mod_ptr->metadata)[j];

        offset = (mod_ptr->addr + mod_ptr->md_size) - next_mod_ptr->addr;

        for ( j = offset; j < next_mod_ptr->md_size; j++ )
            combined_metadata_ptr[i++] = (next_mod_ptr->metadata)[j];

        HDassert(i == combined_md_size);

        HDfree(mod_ptr->metadata);
        mod_ptr->metadata = combined_metadata_ptr;
        mod_ptr->md_size = combined_md_size;
    } /* end if */

    /* We have copied metadata from the next node into the current node
     * if this was necessary.  All that remains is to delete the next
     * node from the tree and free it.
     */
    H5SL_remove(slist, &next_mod_ptr->addr);

    H5FP_free_mod_node(next_mod_ptr);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FP_merge_mod_node_with_next() */

/*
 * Function:	H5FP_merge_mod_node_with_prev
 *
 * Purpose:	Given a node in a mod tree which overlaps with the previous
 *		node in the tree, merge the two.  Where the two nodes
 *		overlap, use the data from the supplied node.
 *
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 *
 * Programmer:	JRM - 3/19/04
 *
 * Modifications:
 *
 *		None.
 */
static herr_t
H5FP_merge_mod_node_with_prev(H5SL_t *slist, H5SL_node_t *node_ptr)
{
    H5SL_node_t *prev_node_ptr;
    H5FP_mdata_mod *mod_ptr;
    H5FP_mdata_mod *prev_mod_ptr;
    unsigned i,j;
    herr_t ret_value=SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT(H5FP_merge_mod_node_with_prev)

    /* check parameters & do some initializations in passing */
    HDassert(slist);
    HDassert(node_ptr);
    if ( ( (mod_ptr = H5SL_item(node_ptr)) == NULL ) ||
         ( (prev_node_ptr = H5SL_prev(node_ptr)) == NULL ) ||
         ( (prev_mod_ptr = H5SL_item(prev_node_ptr)) == NULL ) ||
         ( mod_ptr->addr <= prev_mod_ptr->addr ) ||
         ( (prev_mod_ptr->addr + prev_mod_ptr->md_size) <= mod_ptr->addr ) )
        HGOTO_ERROR(H5E_FPHDF5, H5E_BADVALUE, FAIL, "One or more bad params detected on entry.")

    if ( (prev_mod_ptr->addr + prev_mod_ptr->md_size) <
         (mod_ptr->addr + mod_ptr->md_size) ) {
        unsigned combined_md_size;
        char *combined_metadata_ptr;
        unsigned limit;

        /* The node address range is not completely subsumed in
         * that of the previous node.  Must allocate a new buffer, and
         * copy over the contents of the two buffers.  Where the buffers
         * overlap, give precidence to the data from *node_ptr
         */
        combined_md_size = (mod_ptr->addr + mod_ptr->md_size) -
                           (prev_mod_ptr->addr);

        combined_metadata_ptr = (char *)H5MM_malloc((size_t)(combined_md_size));
        if ( combined_metadata_ptr == NULL )
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate buffer for combined node.")

        i = 0; /* this is the index into the combined buffer */

        limit = mod_ptr->addr - prev_mod_ptr->addr;
        HDassert(limit > 0 );

        for ( j = 0; j < limit; j++ )
            combined_metadata_ptr[i++] = (prev_mod_ptr->metadata)[j];

        for ( j = 0; j < mod_ptr->md_size; j++ )
            combined_metadata_ptr[i++] = (mod_ptr->metadata)[j];

        HDassert(i == combined_md_size);

        HDfree(prev_mod_ptr->metadata);
        prev_mod_ptr->metadata = combined_metadata_ptr;
        prev_mod_ptr->md_size = combined_md_size;
    } else { /* supplied node is completely inside previous node */
        /* no need to allocate a new buffer.  Just copy data from
         * mod_ptr->metadata to the appropriate locations in
         * prev_mod_ptr->metadata.
         */

        i = mod_ptr->addr - prev_mod_ptr->addr;

        for ( j = 0; j < mod_ptr->md_size; j++ )
            (prev_mod_ptr->metadata)[i++] = (mod_ptr->metadata)[j];

        HDassert(i <= prev_mod_ptr->md_size);
    } /* end else */

    /* We have copied metadata from the current node into the previous
     * node.  All that remains is to delete the current node from the
     * tree and free it.
     */

    H5SL_remove(slist, &mod_ptr->addr);

    H5FP_free_mod_node(mod_ptr);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FP_merge_mod_node_with_prev() */

/*
 * Function:	H5FP_mod_node_overlaps_with_next
 *
 * Purpose:	Given a node in a mod tree, see if there is an overlap
 *		between the address range of the supplied node, and that
 *		of the next node in the tree (if any).
 *
 * Return:	TRUE if there is an overlap, and FALSE if there
 *		isn't.
 *
 * Programmer:	JRM - 3/18/04
 *
 * Modifications:
 *
 *		None.
 */
static hbool_t
H5FP_mod_node_overlaps_with_next(H5SL_node_t *node_ptr)
{
    hbool_t ret_value=FALSE;
    H5SL_node_t *next_node_ptr;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5FP_mod_node_overlaps_with_next)

    HDassert(node_ptr != NULL);

    next_node_ptr = H5SL_next(node_ptr);

    if ( next_node_ptr != NULL ) {
#if 0
        if ( ( ((H5FP_mdata_mod *)H5SL_item(node_ptr))->addr > 100000 ) ||
             ( (((H5FP_mdata_mod *)H5SL_item(node_ptr))->md_size) > 1024 ) ) {
            HDfprintf(stdout, "%s: addr = %a, size = %u, mem_type = %d.\n",
                      "H5FP_mod_node_overlaps_with_next(2)",
                      ((H5FP_mdata_mod *)H5SL_item(node_ptr))->addr,
                      ((H5FP_mdata_mod *)H5SL_item(node_ptr))->md_size,
                      (int)(((H5FP_mdata_mod *)H5SL_item(node_ptr))->mem_type));
        }

        if ( (((H5FP_mdata_mod *)H5SL_item(node_ptr))->addr)
             >=
             (((H5FP_mdata_mod *)H5SL_item(next_node_ptr))->addr)
           ) {
            HDfprintf(stdout, "%s: addr,len = %a,%u, next_addr,len =  %a,%u.\n",
                     "H5FP_mod_node_overlaps_with_next",
                     (((H5FP_mdata_mod *)H5SL_item(node_ptr))->addr),
                     ((H5FP_mdata_mod *)H5SL_item(node_ptr))->md_size,
                     (((H5FP_mdata_mod *)H5SL_item(next_node_ptr))->addr),
                     ((H5FP_mdata_mod *)H5SL_item(next_node_ptr))->md_size);

            HDassert((((H5FP_mdata_mod *)H5SL_item(node_ptr))->addr)
                     <
                     (((H5FP_mdata_mod *)H5SL_item(next_node_ptr))->addr)
                    );
        }
#endif
        if ( ( (((H5FP_mdata_mod *)H5SL_item(node_ptr))->addr)
               +
               (((H5FP_mdata_mod *)H5SL_item(node_ptr))->md_size)
             )
             >
             (((H5FP_mdata_mod *)H5SL_item(next_node_ptr))->addr)
           ) {
#if 0
            /* This is useful debugging code -- keep it around for
             * a while.                     JRM -- 4/13/03
             */
            HDfprintf(stdout,
               "H5FP_mod_node_overlaps_with_next: addr = %a, next_addr = %a.\n",
               (((H5FP_mdata_mod *)(node_ptr->data))->addr),
               (((H5FP_mdata_mod *)(next_node_ptr->data))->addr));
#endif
            ret_value = TRUE;
        }
    }

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FP_mod_node_overlaps_with_next() */

/*
 * Function:	H5FP_mod_node_overlaps_with_prev
 *
 * Purpose:	Givena node in a mod tree, see if there is an overlap
 *		between the address range of the supplied node, and that
 *		of the previous node in the tree (if any).
 *
 * Return:	TRUE if there is an overlap, and FALSE if there
 *		isn't.
 *
 * Programmer:	JRM - 3/18/04
 *
 * Modifications:
 *
 *		None.
 */
static hbool_t
H5FP_mod_node_overlaps_with_prev(H5SL_node_t *node_ptr)
{
    hbool_t ret_value=FALSE;
    H5SL_node_t *prev_node_ptr;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5FP_mod_node_overlaps_with_prev)

    HDassert(node_ptr != NULL);

    prev_node_ptr = H5SL_prev(node_ptr);

    if ( prev_node_ptr != NULL )
    {
        HDassert((((H5FP_mdata_mod *)H5SL_item(node_ptr))->addr)
                 >
                 (((H5FP_mdata_mod *)H5SL_item(prev_node_ptr))->addr)
                );

        if ( ( (((H5FP_mdata_mod *)H5SL_item(prev_node_ptr))->addr)
               +
               (((H5FP_mdata_mod *)H5SL_item(prev_node_ptr))->md_size)
             )
             >
             (((H5FP_mdata_mod *)H5SL_item(node_ptr))->addr)
           ) {
#if 0
            /* This is useful debugging code -- keep it around for
             * a while.                        JRM - 4/13/04
             */
            HDfprintf(stdout,
               "H5FP_mod_node_overlaps_with_prev: addr = %a, prev_addr = %a.\n",
               (((H5FP_mdata_mod *)(node_ptr->data))->addr),
               (((H5FP_mdata_mod *)(prev_node_ptr->data))->addr));
#endif
            ret_value = TRUE;
        }
    }

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FP_mod_node_overlaps_with_prev() */

/*
 * Function:    H5FP_add_file_mod_to_list
 * Purpose:     Add a metadata write to a file ID. If the metadata is
 *              already in the cache, then we just replace it with the
 *              updated bits. (Only the metadata info and size should
 *              change in this case.)
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 * Programmer:  Bill Wendling, 02. August, 2002
 * Modifications:
 *		Re-worked code to merge overlapping metadata changes,
 *		and to avoid discarding metadata if the supplied metadata
 *		is smaller than that already in the mod list.
 *						JRM -- 3/29/04
 */
static herr_t
H5FP_add_file_mod_to_list(H5FP_file_info *info, H5FD_mem_t mem_type,
                          haddr_t addr, unsigned md_size,
                          char *metadata)
{
    H5FP_mdata_mod *fm;
    H5SL_node_t *node;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT(H5FP_add_file_mod_to_list)

    /* check args */
    assert(info);

#if 0
    /* This is useful debugging code -- keep it around for a
     * while.                         JRM -- 4/13/04
     */
    HDfprintf(stdout,
              "H5FP_add_file_mod_to_list: Adding chunk at %a of length %u.\n",
              addr, md_size);
#endif
    if ((node = H5SL_find(info->mod_list, &addr)) != NULL) {
        /*
         * The metadata is in the cache already. All we have to do is
         * replace what's there. The addr and type should be the same.
         * The only things to change is the metadata and its size.
         */
        fm = H5SL_item(node);

        if ( fm->md_size > md_size ) {
            HDmemcpy(fm->metadata,metadata,md_size);
            HDfree(metadata);
        } else if ( fm->md_size < md_size ) {
            HDfree(fm->metadata);
            fm->metadata = metadata;
            fm->md_size = md_size;

            while ( H5FP_mod_node_overlaps_with_next(node) ) {
                if ( H5FP_merge_mod_node_with_next(info->mod_list, node)<0)
                    /* Need to define better errors here. -- JRM */
                    HGOTO_ERROR(H5E_FPHDF5, H5E_CANTCHANGE, FAIL, "Can't merge with next.")

                (info->num_mods)--; /* since we just merged */
            }
        } else { /* fm->md_size == md_size */
            HDfree(fm->metadata);
            fm->metadata = metadata;
        }
    } /* end if */
    else {
        if ( (fm = H5FP_new_file_mod_node(mem_type, addr, md_size, metadata)) == NULL)
            HGOTO_ERROR(H5E_FPHDF5, H5E_CANTALLOC, FAIL, "can't create modification node")

        if ( (node = H5SL_add(info->mod_list, fm, &fm->addr)) == NULL )
            HGOTO_ERROR(H5E_FPHDF5, H5E_CANTINSERT, FAIL, "can't insert modification into list")

        (info->num_mods)++;

        /* merge with next as required */
        while ( H5FP_mod_node_overlaps_with_next(node) ) {
            if ( H5FP_merge_mod_node_with_next(info->mod_list, node) < 0)
                /* Need to define better errors here. -- JRM */
                HGOTO_ERROR(H5E_FPHDF5, H5E_CANTCHANGE, FAIL, "Can't merge new node with next.")

            (info->num_mods)--; /* since we just merged */
        }

        /* if the tree was valid to begin with, we must merge with at
         * most one previous node.
         */
        if ( H5FP_mod_node_overlaps_with_prev(node) ) {
            if ( H5FP_merge_mod_node_with_prev(info->mod_list, node) < 0 )
                /* Need to define better errors here. -- JRM */
                HGOTO_ERROR(H5E_FPHDF5, H5E_CANTCHANGE, FAIL, "Can't merge new node with prev.")

            (info->num_mods)--; /* since we just merged */
        }
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
}

/*
 * Function:    H5FP_free_file_info_node
 * Purpose:     Helper function to free up an SAP_FILE_STRUCT node and all
 *              of the malloced space it has.
 * Return:      SUCCEED (never fails)
 * Programmer:  Bill Wendling, 31. July, 2002
 * Modifications:
 */
static herr_t
H5FP_free_file_info_node(H5FP_file_info *info)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5FP_free_file_info_node)

    if (info) {
        H5SL_destroy(info->mod_list,H5FP_free_mod_node_cb,NULL);
        H5SL_destroy(info->locks,H5FP_free_object_lock_cb,NULL);
        H5FD_free_freelist(&info->file.pub);
        H5MM_xfree(info);
    }

    FUNC_LEAVE_NOAPI(SUCCEED)
}

/*
 * Function:    H5FP_new_file_info_node
 * Purpose:     Create and initialize an sap_file_info node.
 * Return:      Success: Pointer to new node
 *              Failure: NULL
 * Programmer:  Bill Wendling, 02. August, 2002
 * Modifications:
 */
static H5FP_file_info *
H5FP_new_file_info_node(unsigned file_id)
{
    H5FP_file_info *ret_value;

    FUNC_ENTER_NOAPI_NOINIT(H5FP_new_file_info_node)

    if ((ret_value = (H5FP_file_info *)H5MM_malloc(sizeof(H5FP_file_info))) == NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "out of memory")

    ret_value->file_id = file_id;
    ret_value->closing = FALSE;
    ret_value->num_mods = 0;

    if ((ret_value->mod_list = H5SL_create(H5SL_TYPE_HADDR,0.5,H5FP_DEFAULT_SKIPLIST_HEIGHT)) == NULL) {
        HDfree(ret_value);
        HGOTO_ERROR(H5E_FPHDF5, H5E_CANTCREATE, NULL, "cannot make skip list")
    }

    if ((ret_value->locks = H5SL_create(H5SL_TYPE_HADDR,0.5,H5FP_DEFAULT_SKIPLIST_HEIGHT)) == NULL) {
        H5SL_close(ret_value->mod_list);
        HDfree(ret_value);
        HGOTO_ERROR(H5E_FPHDF5, H5E_CANTCREATE, NULL, "cannot make skip list")
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
}

/*
 * Function:    H5FP_add_new_file_info_to_list
 * Purpose:     Add a FILE_ID to the list of file IDS.
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 * Programmer:  Bill Wendling, 02. August, 2002
 * Modifications:
 */
static herr_t
H5FP_add_new_file_info_to_list(unsigned file_id, haddr_t maxaddr,
                               unsigned long feature_flags,
                               hsize_t meta_block_size,
                               hsize_t sdata_block_size,
                               hsize_t threshold,
                               hsize_t alignment)
{
    H5FP_file_info *info;
    herr_t ret_value = FAIL;

    FUNC_ENTER_NOAPI_NOINIT(H5FP_add_new_file_info_to_list)

    if ((info = H5FP_new_file_info_node(file_id)) != NULL) {
        int mrc;

        if (H5SL_insert(file_info_list, info, &info->file_id)<0) {
            H5FP_free_file_info_node(info);
            HGOTO_ERROR(H5E_FPHDF5, H5E_CANTINSERT, FAIL, "can't insert file structure into tree")
        }

        /*
         * Initialize some of the information needed for metadata
         * allocation requests
         */
        HDmemset(&info->file, 0, sizeof(info->file));
        info->file.pub.driver_id = H5FD_FPHDF5;
        info->file.pub.cls = (const H5FD_class_t *)&H5FD_fphdf5_g;
        info->file.pub.maxaddr = maxaddr;
        info->file.pub.accum_loc = HADDR_UNDEF;
        info->file.pub.feature_flags = feature_flags;
        info->file.pub.def_meta_block_size = meta_block_size;
        info->file.pub.def_sdata_block_size = sdata_block_size;
        info->file.pub.threshold = threshold;
        info->file.pub.alignment = alignment;

        if ((mrc = MPI_Comm_rank(H5FP_SAP_COMM, &info->file.mpi_rank)) != MPI_SUCCESS)
            HMPI_GOTO_ERROR(FAIL, "MPI_Comm_rank failed", mrc);

        if ((mrc = MPI_Comm_size(H5FP_SAP_COMM, &info->file.mpi_size)) != MPI_SUCCESS)
            HMPI_GOTO_ERROR(FAIL, "MPI_Comm_rank failed", mrc);

        ret_value = SUCCEED;
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
}

/*
 * Function:    H5FP_remove_file_id_from_list
 * Purpose:     Remove an FILE_ID from the list of file IDS.
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
H5FP_remove_file_id_from_list(unsigned file_id)
{
    H5FP_file_info *info;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT(H5FP_remove_file_id_from_list)

    /* Remove the file info from the skip list */
    if ((info = H5SL_remove(file_info_list, &file_id)) == NULL)
        HGOTO_ERROR(H5E_FPHDF5, H5E_CANTFREE, FAIL, "can't release file info")

    /* Free file info */
    H5FP_free_file_info_node(info);

done:
    FUNC_LEAVE_NOAPI(ret_value)
}

/*
 *===----------------------------------------------------------------------===
 *                     Functions to reply to requests
 *===----------------------------------------------------------------------===
 */

/*
 * Function:    H5FP_send_reply
 * Purpose:     Send an H5FP_reply message to process TO.
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 * Programmer:  Bill Wendling, 31. July, 2002
 * Modifications:
 */
static herr_t
H5FP_send_reply(unsigned to, unsigned req_id, unsigned file_id, H5FP_status_t status)
{
    H5FP_reply_t reply;
    herr_t ret_value = SUCCEED;
    int mrc;

    FUNC_ENTER_NOAPI_NOINIT(H5FP_send_reply)

    reply.req_id = req_id;
    reply.file_id = file_id;
    reply.status = status;

    if ((mrc = MPI_Send(&reply, 1, H5FP_reply, (int)to, H5FP_TAG_REPLY,
                        H5FP_SAP_COMM)) != MPI_SUCCESS)
        HMPI_GOTO_ERROR(FAIL, "MPI_Send failed", mrc);

done:
    FUNC_LEAVE_NOAPI(ret_value)
}

/*
 * Function:    H5FP_dump
 * Purpose:     Dump all metadata writes to a process so that that
 *              process will write them to the file.
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 * Programmer:  Bill Wendling, 31. July, 2002
 * Modifications:
 */
static herr_t
H5FP_dump(H5FP_file_info *info, unsigned to, unsigned req_id, unsigned file_id)
{
    H5FP_read_t r;
    H5SL_node_t *node;
    int mrc;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT(H5FP_dump)

    /* check args */
    assert(info);

    if (info->mod_list==NULL || H5SL_count(info->mod_list)==0)
        /* Nothing to write to the file */
        HGOTO_DONE(SUCCEED)

    r.req_id = req_id;
    r.file_id = file_id;
    r.status = H5FP_STATUS_DUMPING;

    node = H5SL_first(info->mod_list);
    while (node) {
        H5FP_mdata_mod *m = H5SL_item(node);

        r.mem_type = m->mem_type;
        r.addr = m->addr;
        r.md_size = m->md_size;

        if ((mrc = MPI_Send(&r, 1, H5FP_read, (int)to, H5FP_TAG_DUMP,
                            H5FP_SAP_COMM)) != MPI_SUCCESS)
            HMPI_GOTO_ERROR(FAIL, "MPI_Send failed", mrc);

        if (H5FP_send_metadata(m->metadata, (int)m->md_size, (int)to) != SUCCEED)
            HGOTO_ERROR(H5E_FPHDF5, H5E_CANTSENDMDATA, FAIL, "can't dump metadata to client")

        node = H5SL_next(node);
    }

    /* Tell the receiving process that we're finished... */
    r.mem_type = 0;
    r.addr = 0;
    r.md_size = 0;
    r.status = H5FP_STATUS_DUMPING_FINISHED;

    if ((mrc = MPI_Send(&r, 1, H5FP_read, (int)to, H5FP_TAG_DUMP,
                        H5FP_SAP_COMM)) != MPI_SUCCESS)
        HMPI_GOTO_ERROR(FAIL, "MPI_Send failed", mrc);

    /* Free up the nodes in the modification list */
    if(H5SL_free(info->mod_list, H5FP_free_mod_node_cb, NULL)<0)
        HGOTO_ERROR(H5E_FPHDF5, H5E_CANTFREE, FAIL, "cannot free skip list")

    info->num_mods = 0;

done:
    FUNC_LEAVE_NOAPI(ret_value)
}

/*
 *===----------------------------------------------------------------------===
 *                    Functions to handle SAP requests
 *===----------------------------------------------------------------------===
 */

/*
 * Function:    H5FP_gen_request_id
 * Purpose:     Generate a unique request ID to send along with a
 *              message.
 * Return:      Integer >= 0 - Doesn't fail.
 * Programmer:  Bill Wendling, 30. July, 2002
 * Modifications:
 */
static unsigned
H5FP_gen_sap_file_id()
{
    static unsigned i = 0;
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5FP_gen_sap_file_id)
    FUNC_LEAVE_NOAPI(i++)
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
H5FP_sap_handle_open_request(H5FP_request_t *req, unsigned UNUSED md_size)
{
    herr_t ret_value = SUCCEED;
    int mrc;

    FUNC_ENTER_NOAPI_NOINIT(H5FP_sap_handle_open_request)

    if (req->obj_type == H5FP_OBJ_FILE) {
        unsigned new_file_id = H5FP_gen_sap_file_id();

        /* N.B. At this point, req->addr is equiv. to maxaddr in H5FD_open() */
        if (H5FP_add_new_file_info_to_list(new_file_id, req->addr, req->feature_flags,
                                           req->meta_block_size, req->sdata_block_size,
                                           req->threshold, req->alignment) != SUCCEED)
            HGOTO_ERROR(H5E_FPHDF5, H5E_CANTINSERT, FAIL,
                        "can't insert new file structure to list")

        /* file ID gets broadcast via the captain process */
        if ((mrc = MPI_Send(&new_file_id, 1, MPI_UNSIGNED, (int)req->proc_rank,
                            H5FP_TAG_FILE_ID, H5FP_SAP_COMM)) != MPI_SUCCESS)
            HMPI_GOTO_ERROR(FAIL, "MPI_Send failed", mrc);
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
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
H5FP_sap_handle_lock_request(H5FP_request_t *req)
{
    struct lock_group {
        hobj_ref_t          oid;
        unsigned            file_id;
        unsigned            locked;
        H5FP_lock_t         rw_lock;
        H5FP_file_info     *info;
        H5FP_object_lock   *lock;
    } *oids;
    unsigned        list_size = 2; /* the size of the "oids" list */
    H5FP_status_t   exit_state = H5FP_STATUS_LOCK_ACQUIRED;
    unsigned        i, j;
    herr_t          ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT(H5FP_sap_handle_lock_request)

    if ((oids = (struct lock_group *)H5MM_malloc(list_size *
                                                 sizeof(struct lock_group))) == NULL) {
        exit_state = H5FP_STATUS_OOM;
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "out of memory")
    }

    /*
     * Gather together all of the OIDs the process is requesting to lock
     * at one time.
     */
    for (i = 0;; ++i) {
        if (req->oid) {
            if (i == list_size) {
                list_size <<= 1;    /* equiv to list_size *= 2; */
                oids = HDrealloc(oids, list_size * sizeof(struct lock_group));

                if (!oids) {
                    exit_state = H5FP_STATUS_OOM;
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "out of memory")
                }
            }

            oids[i].oid = req->oid;
            oids[i].file_id = req->file_id;
            oids[i].rw_lock = req->rw_lock;
            oids[i].locked = FALSE;
        }

        if (req->req_type == H5FP_REQ_LOCK_END)
            /* this was the last lock request */
            break;

        if (H5FP_sap_receive(req, (int)req->proc_rank,
                             H5FP_TAG_REQUEST, NULL) != SUCCEED) {
            exit_state = H5FP_STATUS_LOCK_FAILED;
            HGOTO_ERROR(H5E_FPHDF5, H5E_CANTRECV, FAIL, "cannot receive messages")
        }
    }

    /*
     * Check to see if we can acquire all of the locks requested. If
     * not, the it's an error and we need to return.
     */
    for (j = 0; j <= i; ++j) {
        if ((oids[j].info = H5SL_search(file_info_list,&oids[j].file_id)) == NULL) {
            exit_state = H5FP_STATUS_BAD_FILE_ID;
            HGOTO_DONE(FAIL)
        }

        if (oids[j].info->closing) {
            /* we're closing the file - don't accept anymore locks */
            exit_state = H5FP_STATUS_FILE_CLOSING;
            HGOTO_DONE(FAIL)
        }

        oids[j].lock = H5FP_find_object_lock(oids[j].info, oids[j].oid);

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
                        oids[j].lock->num_locks[req->proc_rank]))) {
            /* FAILURE */
            exit_state = H5FP_STATUS_LOCK_FAILED;
            HGOTO_DONE(FAIL)
        }
    }

    /*
     * Actually acquire the locks. This shouldn't fail because of the
     * previous checks. The only thing which can likely occur is an
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
                    oids[j].lock->num_locks[req->proc_rank])) {
                /*
                 * The requesting process may already have this lock. Might
                 * be a request from some call-back function of some sort.
                 * Increase the reference count if this process hasn't
                 * gotten a lock on this before and then increment that
                 * process's num_locks reference count.
                 */
                if (!oids[j].lock->num_locks[req->proc_rank])
                    ++oids[j].lock->num_procs;

                ++oids[j].lock->num_locks[req->proc_rank];
                oids[j].locked = TRUE;
            } else {
                /* FIXME: reply saying object locked? */
                exit_state = H5FP_STATUS_LOCK_FAILED;
                ret_value = FAIL;
                goto rollback;
            }
        } else {
            H5FP_object_lock *lock = H5FP_new_object_lock(oids[j].oid, req->proc_rank,
                                                          req->obj_type, req->rw_lock);

            if (lock) {
                if (H5SL_insert(oids[j].info->locks, lock, &lock->oid)<0) {
                    H5FP_free_object_lock(lock);
                    exit_state = H5FP_STATUS_LOCK_FAILED;
                    HDONE_ERROR(H5E_FPHDF5, H5E_CANTINSERT, FAIL, "can't insert lock into tree");
                    goto rollback;
                }

                oids[j].locked = TRUE;
            } else {
                /* out of memory...ulp! */
                exit_state = H5FP_STATUS_OOM;
                HDONE_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "out of memory");
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
                if (oids[j].lock->num_locks[req->proc_rank]) {
                    if (--oids[j].lock->num_locks[req->proc_rank] == 0)
                        if (--oids[j].lock->num_procs == 0)
                            H5FP_remove_object_lock_from_list(oids[j].info, oids[j].lock);
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
assert(0);
    }

    HDfree(oids);
    H5FP_send_reply(req->proc_rank, req->req_id, req->file_id, exit_state);
    FUNC_LEAVE_NOAPI(ret_value)
}

/*
 * Function:    H5FP_sap_handle_release_lock_request
 * Purpose:     Handle a request to release the lock on an object.
 * Return:      Nothing
 * Programmer:  Bill Wendling, 06. August, 2002
 * Modifications:
 */
static herr_t
H5FP_sap_handle_release_lock_request(H5FP_request_t *req)
{
    struct release_group {
        hobj_ref_t          oid;
        unsigned            file_id;
        H5FP_file_info     *info;
        H5FP_object_lock   *lock;
    } *oids;
    unsigned list_size = 2; /* the size of the "oids" list */
    H5FP_status_t exit_state = H5FP_STATUS_LOCK_RELEASED;
    herr_t ret_value = SUCCEED;
    unsigned i, j;

    FUNC_ENTER_NOAPI_NOINIT(H5FP_sap_handle_release_lock_request)

    if ((oids = (struct release_group *)H5MM_malloc(list_size *
                                                 sizeof(struct release_group))) == NULL) {
        exit_state = H5FP_STATUS_OOM;
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "out of memory")
    }

    /*
     * Gather together all of the OIDs the process is requesting to
     * release locks at one time.
     */
    for (i = 0;; ++i) {
        if (req->oid) {
            if (i == list_size) {
                list_size <<= 1;    /* equiv to list_size *= 2; */
                oids = HDrealloc(oids, list_size * sizeof(struct release_group));

                if (!oids) {
                    exit_state = H5FP_STATUS_OOM;
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "out of memory")
                }
            }

            oids[i].oid = req->oid;
            oids[i].file_id = req->file_id;
        }

        if (req->req_type == H5FP_REQ_RELEASE_END)
            /* this was the last lock request */
            break;

        if (H5FP_sap_receive(req, (int)req->proc_rank, H5FP_TAG_REQUEST, NULL) != SUCCEED) {
            exit_state = H5FP_STATUS_LOCK_RELEASE_FAILED;
            HGOTO_DONE(FAIL)
        }
    }

    /*
     * Check here to see if the locks exist and we have the locks. This
     * will help keep us from being in a catastrophic state.
     */
    for (j = 0; j <= i; ++j) {
        if ((oids[j].info = H5SL_search(file_info_list,&oids[j].file_id)) == NULL) {
            exit_state = H5FP_STATUS_BAD_FILE_ID;
            HGOTO_DONE(FAIL)
        }

        oids[j].lock = H5FP_find_object_lock(oids[j].info, oids[j].oid);

        if (!oids[j].lock || !oids[j].lock->num_locks[req->proc_rank]) {
            exit_state = H5FP_STATUS_BAD_LOCK;
            HGOTO_DONE(FAIL)
        }
    }

    /*
     * Release a lock. There may be multiple locks to release if they
     * were locked in a group, so loop finding all of the locks and
     * release them.
     */
    for (j = 0; j <= i; ++j) {
        if (oids[j].lock) {
            if (oids[j].lock->num_locks[req->proc_rank]) {
                if (--oids[j].lock->num_locks[req->proc_rank] == 0)
                    if (--oids[j].lock->num_procs == 0)
                        H5FP_remove_object_lock_from_list(oids[j].info, oids[j].lock);
            } else {
                /* AAAIIIIEEE!!! */
                exit_state = H5FP_STATUS_CATASTROPHIC;
                HGOTO_DONE(FAIL)
            }
        } else {
            /* AAAIIIIEEE!!! */
            exit_state = H5FP_STATUS_CATASTROPHIC;
            HGOTO_DONE(FAIL)
        }
    }

done:
    HDfree(oids);
    H5FP_send_reply(req->proc_rank, req->req_id, req->file_id, exit_state);
    FUNC_LEAVE_NOAPI(ret_value)
}

/*
 * Function:    H5FP_sap_handle_read_request
 * Purpose:     Handle a read request from a client. The bit of metadata
 *              that the client wants may or may not be in here. It's not
 *              an error if it isn't here. When that's the case, the
 *              client just goes and reads it from the file.
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 * Programmer:  Bill Wendling, 06. August, 2002
 * Modifications:
 */
static herr_t
H5FP_sap_handle_read_request(H5FP_request_t *req)
{
    H5FP_file_info *info;
    H5FP_read_t r;
    herr_t ret_value = SUCCEED;
    char *metadata = NULL;
    int mrc;

    FUNC_ENTER_NOAPI_NOINIT(H5FP_sap_handle_read_request)
#if 0
    /* More useful debugging code to keep for a time.  JRM - 4/13/04 */
    HDfprintf(stdout,
              "H5FP_sap_handle_read_request: req->addr = %a.\n",
              req->addr);
#endif

    r.req_id = req->req_id;
    r.file_id = req->file_id;
    r.md_size = 0;
    r.addr = 0;
    r.status = H5FP_STATUS_MDATA_NOT_CACHED;

    if ((info = H5SL_search(file_info_list,&req->file_id)) != NULL && info->num_mods) {
        H5FP_mdata_mod *fm;

        if (info->num_mods >= H5FP_MDATA_CACHE_HIGHWATER_MARK) {
            if (H5FP_dump(info, req->proc_rank, req->req_id, req->file_id) == FAIL)
                HGOTO_ERROR(H5E_FPHDF5, H5E_CANTSENDMDATA, FAIL, "can't dump metadata to client")

            /*
             * We aren't going to find the information we need since it
             * was just dumped.
             */
            HGOTO_DONE(SUCCEED)
        }

        if ((fm = H5SL_less(info->mod_list, &req->addr)) != NULL) {
            if (H5F_addr_eq(req->addr, fm->addr)) {
                r.md_size = fm->md_size;
                r.addr = fm->addr;
                r.status = H5FP_STATUS_OK;
                metadata = fm->metadata;    /* Sent out in a separate message */
            } else if (H5F_addr_gt(req->addr, fm->addr)
                        && H5F_addr_lt(req->addr, fm->addr + fm->md_size)) {
                r.md_size = fm->md_size - (req->addr - fm->addr);
                r.addr = req->addr;
                r.status = H5FP_STATUS_OK;
                metadata = fm->metadata + (req->addr - fm->addr);   /* Sent out separately */
            } else {
HDfprintf(stderr, "Panic!!!!\n");
assert(0);
            }
        } else {
HDfprintf(stderr, "%s: Couldn't find metadata at req->addr == %a\n", FUNC, req->addr);
assert(0);
        }
    }

    if ((mrc = MPI_Send(&r, 1, H5FP_read, (int)req->proc_rank,
                        H5FP_TAG_READ, H5FP_SAP_COMM)) != MPI_SUCCESS)
        HMPI_GOTO_ERROR(FAIL, "MPI_Send failed", mrc);

    if (r.md_size)
        if (H5FP_send_metadata(metadata, (int)r.md_size, (int)req->proc_rank) != SUCCEED)
            HGOTO_ERROR(H5E_FPHDF5, H5E_CANTSENDMDATA, FAIL,
                        "can't send metadata to client")

done:
    FUNC_LEAVE_NOAPI(ret_value)
}

/*
 * Function:    H5FP_sap_handle_write_request
 * Purpose:     Handle a request to write a piece of metadata in the
 *              file.
 *
 *              N.B: We assume that the client has the lock on the
 *              requesting object before getting here.
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 * Programmer:  Bill Wendling, 06. August, 2002
 * Modifications:
 */
static herr_t
H5FP_sap_handle_write_request(H5FP_request_t *req, char *mdata, unsigned md_size)
{
    H5FP_file_info *info;
    H5FP_status_t exit_state = H5FP_STATUS_OK;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT(H5FP_sap_handle_write_request)
#if 0
    /* Debugging code -- lets keep it for a time.  JRM -- 4/13/04 */
    HDfprintf(stdout,
              "H5FP_sap_handle_write_request: addr = %a, md_size = %d.\n",
              (haddr_t)(req->addr), (int)md_size);
#endif
    if ((info = H5SL_search(file_info_list,&req->file_id)) != NULL) {
        if (info->num_mods >= H5FP_MDATA_CACHE_HIGHWATER_MARK) {
            /*
             * If there are any modifications not written out yet, dump
             * them to this process
             */
            if (H5FP_send_reply(req->proc_rank, req->req_id, req->file_id,
                                H5FP_STATUS_DUMPING) == FAIL) {
                exit_state = H5FP_STATUS_DUMPING_FAILED;
                HGOTO_ERROR(H5E_FPHDF5, H5E_CANTSENDMDATA, FAIL,
                            "can't send message to client")
            }

            if (H5FP_dump(info, req->proc_rank, req->req_id, req->file_id) == FAIL) {
                exit_state = H5FP_STATUS_DUMPING_FAILED;
                HGOTO_ERROR(H5E_FPHDF5, H5E_CANTSENDMDATA, FAIL,
                            "metadata dump failed")
            }
        }

        if (info->closing) {
            /* we're closing the file - don't accept anymore changes */
            exit_state = H5FP_STATUS_FILE_CLOSING;
            HGOTO_DONE(FAIL)
        }

        if (H5FP_add_file_mod_to_list(info, req->mem_type, (haddr_t)req->addr,
                                      md_size, mdata) != SUCCEED) {
            exit_state = H5FP_STATUS_OOM;
            HGOTO_DONE(FAIL)
        }
    } else {
        /* error: there isn't a file opened to change */
        exit_state = H5FP_STATUS_BAD_FILE_ID;
        HGOTO_DONE(FAIL)
    }

done:
    H5FP_send_reply(req->proc_rank, req->req_id, req->file_id, exit_state);
    FUNC_LEAVE_NOAPI(ret_value)
}

/*
 * Function:    H5FP_sap_handle_flush_request
 * Purpose:     Handle a request to flush the metadata to a file.
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 * Programmer:  Bill Wendling
 *              12. February 2003
 * Modifications:
 */
static herr_t
H5FP_sap_handle_flush_request(H5FP_request_t *req)
{
    H5FP_file_info *info;
    H5FP_status_t exit_state = H5FP_STATUS_OK;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT(H5FP_sap_handle_flush_request)

    if ((info = H5SL_search(file_info_list,&req->file_id)) != NULL) {
        if (info->num_mods) {
            /*
             * If there are any modifications not written out yet, dump
             * them to this process
             */
            if (H5FP_send_reply(req->proc_rank, req->req_id, req->file_id,
                                H5FP_STATUS_DUMPING) == FAIL) {
                exit_state = H5FP_STATUS_DUMPING_FAILED;
                HGOTO_ERROR(H5E_FPHDF5, H5E_CANTSENDMDATA, FAIL,
                            "can't send message to client")
            }

            if (H5FP_dump(info, req->proc_rank, req->req_id, req->file_id) == FAIL) {
                exit_state = H5FP_STATUS_DUMPING_FAILED;
                HGOTO_ERROR(H5E_FPHDF5, H5E_CANTSENDMDATA, FAIL,
                            "can't dump metadata to client")
            }
        }
    } else {
        HGOTO_ERROR(H5E_FPHDF5, H5E_NOTFOUND, FAIL,
                    "can't find information for file")
    }

done:
    H5FP_send_reply(req->proc_rank, req->req_id, req->file_id, exit_state);
    FUNC_LEAVE_NOAPI(ret_value)
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
H5FP_sap_handle_close_request(H5FP_request_t *req)
{
    H5FP_file_info *info;
    H5FP_status_t exit_state = H5FP_STATUS_OK;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT(H5FP_sap_handle_close_request)

    if ((info = H5SL_search(file_info_list,&req->file_id)) != NULL) {
        int comm_size;

        /* Get the size of the SAP communicator */
        if (MPI_Comm_size(H5FP_SAP_COMM, &comm_size) != MPI_SUCCESS)
            HGOTO_ERROR(H5E_INTERNAL, H5E_MPI, FAIL, "MPI_Comm_size failed")

        if (++info->closing == comm_size - 1) {
            /* Free the freelist -- this call never fails */
            H5FD_free_freelist((H5FD_t*)&info->file);

            /* All processes have closed the file - remove it from list */
            if (H5FP_remove_file_id_from_list(req->file_id) != SUCCEED) {
                exit_state = H5FP_STATUS_BAD_FILE_ID;
                HGOTO_ERROR(H5E_FPHDF5, H5E_NOTFOUND, FAIL,
                            "cannot remove file ID from list")
            }
        }
    } else {
        HGOTO_ERROR(H5E_FPHDF5, H5E_NOTFOUND, FAIL,
                    "can't find information for file")
    }

done:
    H5FP_send_reply(req->proc_rank, req->req_id, req->file_id, exit_state);
    FUNC_LEAVE_NOAPI(ret_value)
}

/*
 * Function:    H5FP_sap_handle_alloc_request
 * Purpose:     Handle a request to allocate data from the file.
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 * Programmer:  Bill Wendling, 19. February 2003
 * Modifications:
 */
static herr_t
H5FP_sap_handle_alloc_request(H5FP_request_t *req)
{
    H5FP_alloc_t    sap_alloc;
    H5FP_file_info *info;
    int             mrc;
    herr_t          ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT(H5FP_sap_handle_alloc_request)

    sap_alloc.req_id = req->req_id;
    sap_alloc.file_id = req->file_id;
    sap_alloc.status = H5FP_STATUS_OK;
    sap_alloc.mem_type = req->mem_type;

    if ((info = H5SL_search(file_info_list,&req->file_id)) != NULL) {
        /*
         * Try allocating from the free-list that is kept on the server
         * first. If that fails, then call the specified allocation
         * functions depending upon what type of data is being allocated.
         *
         * Note: H5P_DEFAULT is passed in as the data xfer property list.
         * This is okay since the only situation where that will be used
         * is if you have a "hole" in the middle of your metadata (in
         * aggregate mode) that needs to be freed. We've turned off
         * metadata aggregation for FPHDF5 because we don't have the
         * proper information.
         *
         * Whatta pain.
         */
        if ((sap_alloc.addr = H5FD_alloc((H5FD_t*)&info->file, req->mem_type, H5P_DEFAULT,
                                         req->meta_block_size)) == HADDR_UNDEF) {
            sap_alloc.status = H5FP_STATUS_CANT_ALLOC;
            HGOTO_ERROR(H5E_VFL, H5E_CANTINIT, FAIL,
                        "SAP unable to allocate file memory")
        }

        /* Get the EOA. */
        sap_alloc.eoa = ((H5FD_fphdf5_t*)&info->file)->eoa;
        sap_alloc.status = H5FP_STATUS_OK;
    } else {
        sap_alloc.addr = HADDR_UNDEF;
        sap_alloc.eoa = HADDR_UNDEF;
        sap_alloc.status = H5FP_STATUS_CANT_ALLOC;
    }
#if 0
    /* Debugging code -- lets keep it for a time.  JRM -- 4/13/04 */
    HDfprintf(stdout,
              "%s: req_size = %d, req_type = %d, addr = %a, eoa = %a, status = %d, rp_rank = %d.\n",
              "H5FP_sap_handle_alloc_request",
              (int)(req->meta_block_size),
              (int)(req->mem_type),
              sap_alloc.addr,
              sap_alloc.eoa,
              (int)(sap_alloc.status),
              (int)(req->proc_rank));
#endif
done:
    if ((mrc = MPI_Send(&sap_alloc, 1, H5FP_alloc, (int)req->proc_rank,
                        H5FP_TAG_ALLOC, H5FP_SAP_COMM)) != MPI_SUCCESS)
        HMPI_DONE_ERROR(FAIL, "MPI_Send failed", mrc);

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5FP_sap_handle_alloc_request() */

/*
 * Function:    H5FP_sap_handle_free_request
 * Purpose:     Handle a request to free data from the file.
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 * Programmer:  Bill Wendling, 31. March 2003
 * Modifications:
 */
static herr_t
H5FP_sap_handle_free_request(H5FP_request_t *req)
{
    H5FP_alloc_t    sap_alloc;
    H5FP_file_info *info;
    H5FP_status_t   exit_state = H5FP_STATUS_OK;
    herr_t          ret_value = SUCCEED;
    int             mrc;

    FUNC_ENTER_NOAPI_NOINIT(H5FP_sap_handle_free_request)

    sap_alloc.req_id = req->req_id;
    sap_alloc.file_id = req->file_id;
    sap_alloc.status = H5FP_STATUS_OK;
    sap_alloc.mem_type = req->mem_type;

#if 0
    /* Debugging code -- lets keep it for a time.  JRM -- 4/13/04 */
    HDfprintf(stdout,
        "%s: addr = %a, block_size = %a, mem_type = %d, rp_rank = %d.\n",
              "H5FP_sap_handle_free_request",
              req->addr,
              req->meta_block_size,
              (int)(req->mem_type),
              (int)(req->proc_rank));
#endif

    if ((info = H5SL_search(file_info_list,&req->file_id)) != NULL) {
        if (H5FD_free((H5FD_t*)&info->file, req->mem_type, H5P_DEFAULT,
                      req->addr, req->meta_block_size) != SUCCEED) {
            exit_state = H5FP_STATUS_CANT_FREE;
            sap_alloc.status = H5FP_STATUS_CANT_FREE;
            HGOTO_ERROR(H5E_VFL, H5E_CANTFREE, FAIL,
                        "SAP unable to free metadata block")
        }

        /* Get the EOA. */
        sap_alloc.eoa = ((H5FD_fphdf5_t*)&info->file)->eoa;
        sap_alloc.status = H5FP_STATUS_OK;
    } else {
        sap_alloc.status = H5FP_STATUS_CANT_FREE;
    }

done:
    if ((mrc = MPI_Send(&sap_alloc, 1, H5FP_alloc, (int)req->proc_rank,
                        H5FP_TAG_ALLOC, H5FP_SAP_COMM)) != MPI_SUCCESS)
        HMPI_DONE_ERROR(FAIL, "MPI_Send failed", mrc);

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5FP_sap_handle_free_request() */

/*
 * Function:    H5FP_sap_handle_get_eoa_request
 * Purpose:     Handle a request for the EOA of the file.
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 * Programmer:  Bill Wendling, 31. October 2003
 * Modifications:
 */
static herr_t
H5FP_sap_handle_get_eoa_request(H5FP_request_t *req)
{
    H5FP_eoa_t      sap_eoa;
    H5FP_file_info *info;
    int             mrc;
    herr_t          ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT(H5FP_sap_handle_get_eoa_request)

    sap_eoa.req_id = req->req_id;
    sap_eoa.file_id = req->file_id;

    if ((info = H5SL_search(file_info_list,&req->file_id)) != NULL) {

#if 0
        /* Debugging code -- lets keep it for a time.  JRM -- 4/13/04 */
        HDfprintf(stdout, "%s: eoa = %a, rp_rank = %d.\n",
                  "H5FP_sap_handle_get_eoa_request",
                  ((H5FD_fphdf5_t*)&info->file)->eoa,
                  (int)(req->proc_rank));
#endif

        /* Get the EOA. */
        sap_eoa.eoa = ((H5FD_fphdf5_t*)&info->file)->eoa;
        sap_eoa.status = H5FP_STATUS_OK;
    } else {
#if 1
        /* Debugging code -- lets keep it for a time.  JRM -- 4/13/04 */
        HDfprintf(stdout, "%s: function failed. rp_rank = %d.\n",
                  "H5FP_sap_handle_get_eoa_request",
                  (int)(req->proc_rank));
#endif
        sap_eoa.eoa = HADDR_UNDEF;
        sap_eoa.status = H5FP_STATUS_CANT_ALLOC;
        ret_value = FAIL;
    }

#ifdef LATER
done:
#endif /* LATER */
    if ((mrc = MPI_Send(&sap_eoa, 1, H5FP_eoa, (int)req->proc_rank,
                        H5FP_TAG_EOA, H5FP_SAP_COMM)) != MPI_SUCCESS)
        HMPI_DONE_ERROR(FAIL, "MPI_Send failed", mrc);

    FUNC_LEAVE_NOAPI(ret_value)
}

/*
 * Function:    H5FP_sap_handle_set_eoa_request
 * Purpose:     Handle a request setting the EOA of the file.
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 * Programmer:  Bill Wendling, 31. October 2003
 * Modifications:
 */
static herr_t
H5FP_sap_handle_set_eoa_request(H5FP_request_t *req)
{
    H5FP_status_t   exit_state = H5FP_STATUS_OK;
    H5FP_file_info *info;
    herr_t          ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5FP_sap_handle_set_eoa_request)

    if ((info = H5SL_search(file_info_list,&req->file_id)) != NULL) {

#if 0
    /* Debugging code -- lets keep it for a time.  JRM -- 4/13/04 */
    if ( req->addr < ((H5FD_fphdf5_t*)&info->file)->eoa ) {
        HDfprintf(stdout,
                  "%s: old eoa = %a, new eoa = %a, rp_rank = %d. %s\n",
                  "H5FP_sap_handle_set_eoa_request",
                  ((H5FD_fphdf5_t*)&info->file)->eoa,
                  req->addr,
                  (int)(req->proc_rank),
                  "<---- eoa reduced!!! -------");
    }
#if 0
    else {
        HDfprintf(stdout,
                  "%s: old eoa = %a, new eoa = %a, rp_rank = %d.\n",
                  "H5FP_sap_handle_set_eoa_request",
                  ((H5FD_fphdf5_t*)&info->file)->eoa,
                  req->addr,
                  (int)(req->proc_rank));
    }
#endif
#endif

        /* Get the EOA. */
        ((H5FD_fphdf5_t*)&info->file)->eoa = req->addr;
        exit_state = H5FP_STATUS_OK;
    } else {
#if 1
    /* Debugging code -- lets keep it for a time.  JRM -- 4/13/04 */
    HDfprintf(stdout,
              "%s: Function failed -- Couldn't get info.  new eoa = %a.\n",
              "H5FP_sap_handle_set_eoa_request",
              req->addr);
#endif
        exit_state = H5FP_STATUS_CANT_ALLOC;
        ret_value = FAIL;
    }

#ifdef LATER
done:
#endif /* LATER */
    H5FP_send_reply(req->proc_rank, req->req_id, req->file_id, exit_state);
    FUNC_LEAVE_NOAPI(ret_value)
}

/*
 * Function:    H5FP_sap_handle_update_eoma_eosda_request
 * Purpose:     Handle a request to update the EOMA and EOSDA for a file.
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 * Programmer:  Bill Wendling, 02. April 2003
 * Modifications:
 */
static herr_t
H5FP_sap_handle_update_eoma_eosda_request(H5FP_request_t *req)
{
    H5FP_eoa_t      sap_eoa;
    H5FP_file_info *info;
    H5FP_status_t   exit_state = H5FP_STATUS_OK;
    herr_t          ret_value = SUCCEED;
    int             mrc;

    FUNC_ENTER_NOAPI_NOINIT(H5FP_sap_handle_update_eoma_eosda_request)

    sap_eoa.req_id = req->req_id;
    sap_eoa.file_id = req->file_id;
    sap_eoa.status = H5FP_STATUS_OK;

    if ((info = H5SL_search(file_info_list,&req->file_id)) != NULL) {
        H5FD_t *f = (H5FD_t*)&info->file;

        if (f->eoma)
            if (H5FD_free(f, H5FD_MEM_DEFAULT, H5P_DEFAULT,
                          f->eoma, f->cur_meta_block_size) != SUCCEED) {
                exit_state = H5FP_STATUS_CANT_FREE;
                sap_eoa.status = H5FP_STATUS_CANT_FREE;
                HGOTO_ERROR(H5E_VFL, H5E_CANTFREE, FAIL,
                            "SAP unable to free metadata block")
            }

        /* Reset metadata block information, just in case */
        f->eoma = 0;
        f->cur_meta_block_size = 0;

        if (f->eosda)
            if (H5FD_free(f, H5FD_MEM_DRAW, H5P_DEFAULT,
                          f->eosda, f->cur_sdata_block_size) != SUCCEED) {
                exit_state = H5FP_STATUS_CANT_FREE;
                sap_eoa.status = H5FP_STATUS_CANT_FREE;
                HGOTO_ERROR(H5E_VFL, H5E_CANTFREE, FAIL,
                            "SAP unable to free 'small data' block")
            }

        /* Reset "small data" block information, just in case */
        f->eosda = 0;
        f->cur_sdata_block_size = 0;

        /* Get the EOA. */
        sap_eoa.eoa = ((H5FD_fphdf5_t*)&info->file)->eoa;
    } else {
        sap_eoa.eoa = HADDR_UNDEF;
        sap_eoa.status = H5FP_STATUS_CANT_FREE;
    }

done:
    if ((mrc = MPI_Send(&sap_eoa, 1, H5FP_eoa, (int)req->proc_rank,
                        H5FP_TAG_EOA, H5FP_SAP_COMM)) != MPI_SUCCESS)
        HMPI_DONE_ERROR(FAIL, "MPI_Send failed", mrc);

    FUNC_LEAVE_NOAPI(ret_value)
}

#endif  /* H5_HAVE_FPHDF5 */
