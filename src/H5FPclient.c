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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define H5S_PACKAGE             /*suppress error about including H5Spkg */

#include "H5Spkg.h"

#include "H5private.h"          /* Generic Functions                    */
#include "H5Eprivate.h"         /* Error Handling                       */
#include "H5Oprivate.h"         /* Object Headers                       */
#include "H5TBprivate.h"        /* Threaded, Balanced, Binary Trees     */

#ifdef H5_HAVE_FPHDF5

#include "H5FPprivate.h"        /* Flexible Parallel Functions          */

#include "mpi.h"

/* Pablo mask */
#define PABLO_MASK      H5FPclient_mask

/* Is the interface initialized? */
static int interface_initialize_g = 0;

#define INTERFACE_INIT  NULL

MPI_Datatype SAP_request_t;     /* MPI datatype for the SAP_request obj */
MPI_Datatype SAP_reply_t;       /* MPI datatype for the SAP_reply obj   */
MPI_Datatype SAP_sync_t;        /* MPI datatype for the SAP_sync obj    */

/* SAP specific variables */
MPI_Comm H5FP_SAP_COMM;         /* Comm we use: Supplied by user        */
MPI_Comm H5FP_SAP_BARRIER_COMM; /* Comm if you want to do a barrier     */

unsigned H5FP_sap_rank;         /* The rank of the SAP: Supplied by user*/
unsigned H5FP_my_rank;          /* Rank of this process in the COMM     */
int H5FP_comm_size;             /* Size of the COMM                     */

/* local functions */
static unsigned int H5FP_gen_request_id(void);
static herr_t H5FP_update_metadata_cache(hid_t file_id, struct SAP_sync *sap_sync,
                                         H5O_fphdf5_t *fmeta);

/** Public Library (non-API) Functions **/

/*
 * Function:    H5FP_request_open
 * Purpose:     Request an open of an object from the SAP. You pass in
 *              the metadata string (MDATA) (this is normally the
 *              filename or pathname to the object), it's length in
 *              (MD_LEN), and the type of the object you're trying to
 *              open (OBJ_TYPE). The request ID is returned in a pointer
 *              supplied by the user.
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 * Programmer:  Bill Wendling, 28. August, 2002
 * Modifications:
 */
herr_t
H5FP_request_open(const char *mdata, int md_len, enum sap_obj_type obj_type,
                  unsigned *file_id, unsigned *req_id)
{
    struct SAP_request req;
    MPI_Status mpi_status;
    int ret_value = SUCCEED, mrc;

    FUNC_ENTER_NOAPI(H5FP_request_open, FAIL);

    assert(mdata);
    assert(file_id);
    assert(req_id);
    HDmemset(&mpi_status, 0, sizeof(MPI_Status));

    if (H5FP_my_rank == H5FP_capt_rank) {
        HDmemset(&req, 0, sizeof(req));
        req.req_type = H5FP_REQ_OPEN;
        req.req_id = H5FP_gen_request_id();
        req.proc_rank = H5FP_my_rank;
        req.md_len = md_len;
        req.obj_type = obj_type;

        if ((mrc = MPI_Send(&req, 1, SAP_request_t, (int)H5FP_sap_rank,
                            H5FP_TAG_REQUEST, H5FP_SAP_COMM)) != MPI_SUCCESS)
            HMPI_GOTO_ERROR(FAIL, "MPI_Send failed", mrc);

        /* The first MPI_Send will have been sent before this one will be read. */

        if (H5FP_send_metadata(mdata, md_len, (int)H5FP_sap_rank))
            HGOTO_ERROR(H5E_FPHDF5, H5E_CANTSENDMDATA, FAIL, "can't send metadata to server");
    }

    if ((mrc = MPI_Recv(file_id, 1, MPI_UNSIGNED, (int)H5FP_sap_rank,
                        H5FP_TAG_FILE_ID, H5FP_SAP_COMM, &mpi_status)) != MPI_SUCCESS)
        HMPI_GOTO_ERROR(FAIL, "MPI_Send failed", mrc);

    printf("%u: New File ID == %u\n", H5FP_my_rank, *file_id);
    fflush(stdout);

done:
    *req_id = req.req_id;
    FUNC_LEAVE(ret_value);
}

/*
 * Function:    H5FP_request_lock
 * Purpose:     Request a lock on an object in a file from the SAP. The
 *              request ID is returned in a pointer supplied by the user.
 *              The status of the SAP is returned to the user in the
 *              supplied STATUS pointer.
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 * Programmer:  Bill Wendling, 30. July, 2002
 * Modifications:
 */
herr_t
H5FP_request_lock(unsigned int sap_file_id, unsigned char *obj_oid,
                  enum sap_lock_type rw_lock, int last, unsigned *req_id,
                  enum sap_status *status)
{
    struct SAP_request req;
    int ret_value = SUCCEED, mrc;

    FUNC_ENTER_NOAPI(H5FP_request_lock, FAIL);

    assert(obj_oid);
    assert(req_id);
    assert(status);
    HDmemset(&req, 0, sizeof(req));

    *status = H5FP_STATUS_OK;
    req.req_type = last ? H5FP_REQ_LOCK_END : H5FP_REQ_LOCK;
    req.req_id = H5FP_gen_request_id();
    req.sap_file_id = sap_file_id;
    req.rw_lock = rw_lock;
    req.md_len = 0;
    req.proc_rank = H5FP_my_rank;
    H5FP_COPY_OID(req.oid, obj_oid);

    if ((mrc = MPI_Send(&req, 1, SAP_request_t, (int)H5FP_sap_rank,
                        H5FP_TAG_REQUEST, H5FP_SAP_COMM)) != MPI_SUCCESS)
        HMPI_GOTO_ERROR(FAIL, "MPI_Send failed", mrc);

    if (last) {
        /*
         * On the last lock in the lock-group to be acquired, we expect a
         * reply from the SAP
         */
        struct SAP_reply sap_reply;
        MPI_Status mpi_status;

        HDmemset(&mpi_status, 0, sizeof(mpi_status));

        if ((mrc = MPI_Recv(&sap_reply, 1, SAP_reply_t, (int)H5FP_sap_rank,
                            H5FP_TAG_REPLY, H5FP_SAP_COMM, &mpi_status)) != MPI_SUCCESS)
            HMPI_GOTO_ERROR(FAIL, "MPI_Recv failed", mrc);

        *status = sap_reply.status;

        if (sap_reply.status != H5FP_STATUS_LOCK_ACQUIRED)
            HGOTO_DONE(FAIL);
    }

done:
    *req_id = req.req_id;
    FUNC_LEAVE(ret_value);
}

/*
 * Function:    H5FP_request_release_lock
 * Purpose:     Release a lock on the file from the SAP. Request a lock
 *              on an object in a file from the SAP. The request ID is
 *              returned in a pointer supplied by the user. The status
 *              of the SAP is returned to the user in the supplied STATUS
 *              pointer.
 * Return:      Success: The request ID.
 *              Failure: -1
 * Programmer:  Bill Wendling, 30. July, 2002
 * Modifications:
 */
herr_t
H5FP_request_release_lock(unsigned int sap_file_id, unsigned char *obj_oid,
                          int last, unsigned *req_id, enum sap_status *status)
{
    struct SAP_request req;
    herr_t ret_value = SUCCEED;
    int mrc;

    FUNC_ENTER_NOAPI(H5FP_request_release_lock, FAIL);

    assert(req_id);
    assert(status);

    HDmemset(&req, 0, sizeof(req));

    *status = H5FP_STATUS_OK;
    req.req_type = last ? H5FP_REQ_RELEASE_END : H5FP_REQ_RELEASE;
    req.req_id = H5FP_gen_request_id();
    req.sap_file_id = sap_file_id;
    req.md_len = 0;
    req.proc_rank = H5FP_my_rank;

    if (obj_oid)
        H5FP_COPY_OID(req.oid, obj_oid);
    else
        HDmemset(req.oid, '\0', sizeof(req.oid));

    if ((mrc = MPI_Send(&req, 1, SAP_request_t, (int)H5FP_sap_rank,
                        H5FP_TAG_REQUEST, H5FP_SAP_COMM)) != MPI_SUCCESS)
        HMPI_GOTO_ERROR(FAIL, "MPI_Send failed", mrc);

    if (last) {
        /*
         * On the last lock released in this lock-group, we expect a
         * reply from the SAP
         */
        struct SAP_reply sap_reply;
        MPI_Status mpi_status;

        HDmemset(&mpi_status, 0, sizeof(mpi_status));

        if ((mrc = MPI_Recv(&sap_reply, 1, SAP_reply_t, (int)H5FP_sap_rank,
                            H5FP_TAG_REPLY, H5FP_SAP_COMM, &mpi_status)) != MPI_SUCCESS)
            HMPI_GOTO_ERROR(FAIL, "MPI_Recv failed", mrc);

        *status = sap_reply.status;

        if (sap_reply.status != H5FP_STATUS_LOCK_RELEASED) {
            HDfprintf(stderr, "Release: For some reason, we couldn't release the lock\n");
            HDfprintf(stderr, "Release: reply status == %d\n", sap_reply.status);
            HGOTO_DONE(FAIL);
        }
    }

done:
    *req_id = req.req_id;
    FUNC_LEAVE(ret_value);
}

/*
 * Function:    H5FP_request_change
 * Purpose:     Tell the SAP that we want to change the structure of the file.
 *              Include the information the SAP will need to send to the
 *              other processes so that they can be synced with what you
 *              are doing. The request ID is returned in a pointer
 *              supplied by the user.
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 * Programmer:  Bill Wendling, 02. August, 2002
 * Modifications:
 */
herr_t
H5FP_request_change(unsigned int sap_file_id, enum sap_obj_type obj_type,
                    enum sap_action action, int mdata_len, const char *mdata,
                    unsigned *req_id)
{
    struct SAP_request req;
    herr_t ret_value = SUCCEED;
    int mrc;

    FUNC_ENTER_NOAPI(H5FP_request_change, FAIL);

    assert(mdata);
    HDmemset(&req, 0, sizeof(req));

    req.req_type = H5FP_REQ_CHANGE;
    req.req_id = H5FP_gen_request_id();
    req.sap_file_id = sap_file_id;
    req.obj_type = obj_type;
    req.action = action;
    req.md_len = mdata_len;
    req.proc_rank = H5FP_my_rank;

    if ((mrc = MPI_Send(&req, 1, SAP_request_t, (int)H5FP_sap_rank,
                        H5FP_TAG_REQUEST, H5FP_SAP_COMM)) != MPI_SUCCESS)
        HMPI_GOTO_ERROR(FAIL, "MPI_Send failed", mrc);

    /* The first MPI_Send will have been sent before this one will be read. */
    if (H5FP_send_metadata(mdata, mdata_len, (int)H5FP_sap_rank) != SUCCEED)
        HGOTO_ERROR(H5E_FPHDF5, H5E_CANTSENDMDATA, FAIL, "can't send metadata to server");

done:
    *req_id = req.req_id;
    FUNC_LEAVE(ret_value);
}

/*
 * Function:    H5FP_request_sync
 * Purpose:     Tell the SAP that we want all of the structural changes
 *              made on the file that we don't know about. The request ID
 *              is returned in a pointer supplied by the user. The status
 *              of the SAP is returned to the user in the supplied STATUS
 *              pointer.
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 * Programmer:  Bill Wendling, 02. August, 2002
 * Modifications:
 */
herr_t
H5FP_request_sync(unsigned int sap_file_id, hid_t hdf_file_id,
                  unsigned *req_id, enum sap_status *status)
{
    struct SAP_request req;
    herr_t ret_value = SUCCEED;
    int mrc;

    FUNC_ENTER_NOAPI(H5FP_request_sync, FAIL);

    assert(req_id);
    assert(status);

    HDmemset(&req, 0, sizeof(req));

    *status = H5FP_STATUS_OK;
    req.req_type = H5FP_REQ_SYNC;
    req.req_id = H5FP_gen_request_id();
    req.sap_file_id = sap_file_id;
    req.proc_rank = H5FP_my_rank;

    if ((mrc = MPI_Send(&req, 1, SAP_request_t, (int)H5FP_sap_rank,
                        H5FP_TAG_REQUEST, H5FP_SAP_COMM)) != MPI_SUCCESS)
        HMPI_GOTO_ERROR(FAIL, "MPI_Send failed", mrc);

    for (;;) {
        MPI_Status mpi_status;
        struct SAP_sync sap_sync;

        HDmemset(&mpi_status, 0, sizeof(mpi_status));

        if ((mrc = MPI_Recv(&sap_sync, 1, SAP_sync_t, (int)H5FP_sap_rank, H5FP_TAG_SYNC,
                            H5FP_SAP_COMM, &mpi_status)) != MPI_SUCCESS)
            HMPI_GOTO_ERROR(FAIL, "MPI_Recv failed", mrc);

        if (sap_sync.status != H5FP_STATUS_OK) {
            *status = sap_sync.status;
            HGOTO_DONE(FAIL);
        }

        if (sap_sync.last_msg)
            break;

        /* use the info in the SAP_sync_t structure to update the
         * metadata */
        if (sap_sync.md_len) {
            H5O_fphdf5_t *fmeta;
            char *buf;

            if ((buf = (char *)HDcalloc((size_t)sap_sync.md_len + 1, 1)) == NULL)
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "out of memory");

            HDmemset(&mpi_status, 0, sizeof(mpi_status));

            if ((mrc = MPI_Recv(buf, sap_sync.md_len, MPI_BYTE, (int)H5FP_sap_rank,
                                H5FP_TAG_METADATA, H5FP_SAP_COMM, &mpi_status)) != MPI_SUCCESS) {
                HDfree(buf);
                HMPI_GOTO_ERROR(FAIL, "MPI_Recv failed", mrc);
            }

            /*
             * FIXME: perform whatever metadata updates we can with the
             * metadata returned from the SAP
             */
            fmeta = H5O_FPHDF5[0].decode(NULL, buf, NULL);
            HDfree(buf);

            if (H5FP_update_metadata_cache(hdf_file_id, &sap_sync, fmeta) != SUCCEED) {
                H5O_FPHDF5[0].free(fmeta);
                HGOTO_DONE(FAIL);
            }

            H5O_FPHDF5[0].free(fmeta);
        }
    }

done:
    *req_id = req.req_id;
    FUNC_LEAVE(ret_value);
}

#if 0
struct SAP_sync {
    unsigned int req_id;        /* Request ID copied from the SAP_request   */
    unsigned int sync_id;       /* Sync ID to order the sync messages       */
    unsigned int sap_file_id;   /* SAP's file ID for the specific file      */
    unsigned int last_msg;      /* Indicates this is the last sync msg sent */
    int md_len;                 /* Length of the metadata sent in next msg  */
    enum sap_obj_type obj_type; /* Type of object                           */
    enum sap_action action;     /* Action done on the object                */
    enum sap_status status;     /* Status of the request                    */
};

typedef struct H5O_fphdf5_t {
    uint8_t oid[H5R_OBJ_REF_BUF_SIZE];  /* OID of object                    */
    struct H5S_simple_t *sdim;          /* Simple dimensionality structure  */
    H5T_t *dtype;                       /* Datatype structure               */
    time_t *mtime;                      /* Modification time                */
    H5O_name_t *group;                  /* Group name                       */
    H5O_name_t *dset;                   /* Dataset name                     */
    struct H5P_genplist_t *plist;       /* Pathname of the object           */
} H5O_fphdf5_t;
#endif

static herr_t
H5FP_update_metadata_cache(hid_t file_id, struct SAP_sync *sap_sync, H5O_fphdf5_t *fmeta)
{
    herr_t ret_value = SUCCEED;
    hid_t gid, dset_id;

    FUNC_ENTER_NOINIT(H5FP_update_metadata_cache);

    /* check args */
    assert(sap_sync);
    assert(fmeta);

    switch (sap_sync->action) {
    case H5FP_ACT_CREATE:
    case H5FP_ACT_EXTEND:
        if (sap_sync->obj_type == H5FP_OBJ_DATASET) {
            gid = H5Gopen(file_id, fmeta->group->s);
            dset_id = H5Dopen(gid, fmeta->dset->s);

            if (H5Dextend(dset_id, fmeta->sdim->size) != SUCCEED)
                HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "metadata update failed");
        }

        break;
    case H5FP_ACT_DELETE:
    default:
        break;
    }

done:
    FUNC_LEAVE(ret_value);
}

/*
 * Function:    H5FP_request_close
 * Purpose:     Tell the SAP that we want all of the structural changes
 *              made on the file and then close the file. The request ID
 *              is returned in a pointer passed to the function by the
 *              user.
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 * Programmer:  Bill Wendling, 02. August, 2002
 * Modifications:
 */
herr_t
H5FP_request_close(unsigned sap_file_id, unsigned *req_id)
{
    struct SAP_request req;
    int ret_value = SUCCEED, mrc;

    FUNC_ENTER_NOAPI(H5FP_request_close, FAIL);

    HDmemset(&req, 0, sizeof(req));
    req.req_type = H5FP_REQ_CLOSE;
    req.req_id = H5FP_gen_request_id();
    req.sap_file_id = sap_file_id;
    req.proc_rank = H5FP_my_rank;

    if ((mrc = MPI_Send(&req, 1, SAP_request_t, (int)H5FP_sap_rank,
                        H5FP_TAG_REQUEST, H5FP_SAP_COMM)) != MPI_SUCCESS)
        HMPI_GOTO_ERROR(FAIL, "MPI_Send failed", mrc);

    *req_id = req.req_id;

done:
    FUNC_LEAVE(ret_value);
}

/** Private Functions **/

/*
 * Function:    H5FP_gen_request_id
 * Purpose:     Generate a unique request ID to send along with a
 *              message.
 * Return:      Integer >= 0 - Doesn't fail.
 * Programmer:  Bill Wendling, 30. July, 2002
 * Modifications:
 */
static unsigned int H5FP_gen_request_id()
{
    static unsigned int i = 0;

    FUNC_ENTER_NOINIT(H5FP_gen_request_id);
    FUNC_LEAVE(i++);
}

#endif  /* H5_HAVE_FPHDF5 */
