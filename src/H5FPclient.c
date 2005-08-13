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

#define H5S_PACKAGE             /*suppress error about including H5Spkg */
#define H5G_PACKAGE             /*suppress error about including H5Gpkg */


#include "H5private.h"          /* Generic Functions                    */
#include "H5ACprivate.h"        /* Metadata Cache                       */
#include "H5Dprivate.h"         /* Dataset Functions                    */
#include "H5Eprivate.h"         /* Error Handling                       */
#include "H5Fprivate.h"         /* Files                                */
#include "H5FDprivate.h"        /* File Drivers                         */
#include "H5Gpkg.h"             /* Group Functions                      */
#include "H5Iprivate.h"         /* ID Functions                         */
#include "H5MMprivate.h"        /* Memory Allocation                    */
#include "H5Oprivate.h"         /* Object Headers                       */
#include "H5Rprivate.h"         /* References                           */
#include "H5Spkg.h"             /* Dataspace Functions                  */

#ifdef H5_HAVE_FPHDF5

#include "H5FDfphdf5.h"         /* File Driver for FPHDF5               */
#include "H5FPprivate.h"        /* Flexible Parallel Functions          */

/* local functions */
static unsigned H5FP_gen_request_id(void);
static herr_t H5FP_dump_to_file(H5FD_t *file, hid_t dxpl_id);


/*
 *===----------------------------------------------------------------------===
 *                    Public Library (non-API) Functions
 *===----------------------------------------------------------------------===
 */

/*
 * Function:    H5FP_request_open
 * Purpose:     Request an open of a file from the SAP. You pass in the
 *              metadata string (MDATA) (the filename), it's length in
 *              (MD_LEN), and the type of the object you're trying to
 *              open (OBJ_TYPE). The request ID is returned in a pointer
 *              supplied by the user.
 *
 *              The so-called "captain" process is in charge of telling
 *              the SAP that the processes opened a file. All processes
 *              opening the file, though, should call this function so
 *              that they can get the file ID that the SAP assigns to it.
 *
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 * Programmer:  Bill Wendling, 28. August, 2002
 * Modifications:
 */
herr_t
H5FP_request_open(H5FP_obj_t obj_type, haddr_t maxaddr,
                  unsigned long feature_flags, hsize_t meta_block_size,
                  hsize_t sdata_block_size, hsize_t threshold,
                  hsize_t alignment, unsigned *file_id, unsigned *req_id)
{
    H5FP_request_t req;
    MPI_Status mpi_status;
    int mrc, my_rank;
    int ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5FP_request_open, FAIL);

    /* check args */
    assert(file_id);
    assert(req_id);

    HDmemset(&mpi_status, 0, sizeof(MPI_Status));

    if ((mrc = MPI_Comm_rank(H5FP_SAP_COMM, &my_rank)) != MPI_SUCCESS)
        HMPI_GOTO_ERROR(FAIL, "MPI_Comm_rank failed", mrc);

    if ((unsigned)my_rank == H5FP_capt_rank) {
        /*
         * The captain process sends the information about the file to
         * the SAP.
         */
        HDmemset(&req, 0, sizeof(req));
        req.req_type = H5FP_REQ_OPEN;
        req.req_id = H5FP_gen_request_id();
        req.proc_rank = my_rank;
        req.md_size = 0;
        req.obj_type = obj_type;
        req.addr = maxaddr;
        req.feature_flags = feature_flags;
        req.meta_block_size = meta_block_size;
        req.sdata_block_size = sdata_block_size;
        req.threshold = threshold;
        req.alignment = alignment;

        if ((mrc = MPI_Send(&req, 1, H5FP_request, (int)H5FP_sap_rank,
                            H5FP_TAG_REQUEST, H5FP_SAP_COMM)) != MPI_SUCCESS)
            HMPI_GOTO_ERROR(FAIL, "MPI_Send failed", mrc);

        if ((mrc = MPI_Recv(file_id, 1, MPI_UNSIGNED, (int)H5FP_sap_rank,
                            H5FP_TAG_FILE_ID, H5FP_SAP_COMM,
                            &mpi_status)) != MPI_SUCCESS)
            HMPI_GOTO_ERROR(FAIL, "MPI_Send failed", mrc);
    }

done:
    *req_id = req.req_id;
    FUNC_LEAVE_NOAPI(ret_value);
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
H5FP_request_lock(unsigned file_id, hobj_ref_t obj_oid,
                  H5FP_lock_t rw_lock, int last, unsigned *req_id,
                  H5FP_status_t *status)
{
    H5FP_request_t req;
    int mrc, my_rank;
    int ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5FP_request_lock, FAIL);

    /* check args */
    assert(obj_oid);
    assert(req_id);
    assert(status);

    HDmemset(&req, 0, sizeof(req));

    if ((mrc = MPI_Comm_rank(H5FP_SAP_COMM, &my_rank)) != MPI_SUCCESS)
        HMPI_GOTO_ERROR(FAIL, "MPI_Comm_rank failed", mrc);

    *status = H5FP_STATUS_OK;
    req.req_type = last ? H5FP_REQ_LOCK_END : H5FP_REQ_LOCK;
    req.req_id = H5FP_gen_request_id();
    req.file_id = file_id;
    req.rw_lock = rw_lock;
    req.md_size = 0;
    req.proc_rank = my_rank;
    req.oid = obj_oid;

    if ((mrc = MPI_Send(&req, 1, H5FP_request, (int)H5FP_sap_rank,
                        H5FP_TAG_REQUEST, H5FP_SAP_COMM)) != MPI_SUCCESS) {
        *status = H5FP_STATUS_LOCK_FAILED;
        HMPI_GOTO_ERROR(FAIL, "MPI_Send failed", mrc);
    }

    if (last) {
        /*
         * On the last lock in the lock-group to be acquired, we expect a
         * reply from the SAP
         */
        H5FP_reply_t    sap_reply;
        MPI_Status      mpi_status;

        HDmemset(&mpi_status, 0, sizeof(mpi_status));

        if ((mrc = MPI_Recv(&sap_reply, 1, H5FP_reply, (int)H5FP_sap_rank,
                            H5FP_TAG_REPLY, H5FP_SAP_COMM, &mpi_status)) != MPI_SUCCESS)
            HMPI_GOTO_ERROR(FAIL, "MPI_Recv failed", mrc);

        *status = sap_reply.status;

        if (sap_reply.status != H5FP_STATUS_LOCK_ACQUIRED)
            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTLOCK, FAIL, "can't lock object on server");
    }

done:
    *req_id = req.req_id;
    FUNC_LEAVE_NOAPI(ret_value);
}

/*
 * Function:    H5FP_request_release_lock
 * Purpose:     Release a lock on the file from the SAP. Request a lock
 *              on an object in a file from the SAP. The request ID is
 *              returned in a pointer supplied by the user. The status
 *              of the SAP is returned to the user in the supplied STATUS
 *              pointer.
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 * Programmer:  Bill Wendling, 30. July, 2002
 * Modifications:
 */
herr_t
H5FP_request_release_lock(unsigned file_id, hobj_ref_t obj_oid,
                          int last, unsigned *req_id, H5FP_status_t *status)
{
    H5FP_request_t req;
    int mrc, my_rank;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5FP_request_release_lock, FAIL);

    /* check args */
    assert(obj_oid);
    assert(req_id);
    assert(status);

    HDmemset(&req, 0, sizeof(req));

    if ((mrc = MPI_Comm_rank(H5FP_SAP_COMM, &my_rank)) != MPI_SUCCESS)
        HMPI_GOTO_ERROR(FAIL, "MPI_Comm_rank failed", mrc);

    *status = H5FP_STATUS_OK;
    req.oid = obj_oid;
    req.req_type = last ? H5FP_REQ_RELEASE_END : H5FP_REQ_RELEASE;
    req.req_id = H5FP_gen_request_id();
    req.file_id = file_id;
    req.md_size = 0;
    req.proc_rank = my_rank;

    if ((mrc = MPI_Send(&req, 1, H5FP_request, (int)H5FP_sap_rank,
                        H5FP_TAG_REQUEST, H5FP_SAP_COMM)) != MPI_SUCCESS) {
        *status = H5FP_STATUS_LOCK_RELEASE_FAILED;
        HMPI_GOTO_ERROR(FAIL, "MPI_Send failed", mrc);
    }

    if (last) {
        /*
         * On the last lock released in this lock-group, we expect a
         * reply from the SAP
         */
        H5FP_reply_t    sap_reply;
        MPI_Status      mpi_status;

        HDmemset(&mpi_status, 0, sizeof(mpi_status));

        if ((mrc = MPI_Recv(&sap_reply, 1, H5FP_reply, (int)H5FP_sap_rank,
                            H5FP_TAG_REPLY, H5FP_SAP_COMM, &mpi_status)) != MPI_SUCCESS)
            HMPI_GOTO_ERROR(FAIL, "MPI_Recv failed", mrc);

        *status = sap_reply.status;

        if (sap_reply.status != H5FP_STATUS_LOCK_RELEASED)
            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTUNLOCK, FAIL,
                        "can't unlock object on server");
    }

done:
    *req_id = req.req_id;
    FUNC_LEAVE_NOAPI(ret_value);
}

/*
 * Function:    H5FP_request_read_metadata
 * Purpose:     Read a piece of metadata from the SAP. That is, if the
 *              SAP has access to that metadata. If not, then we'll need
 *              to read it from disk.
 *
 *              This function has the potential of causing the process to
 *              act as a dumper for the SAP's metadata. Places which call
 *              this function and check the STATUS variable should take
 *              this into account.
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 * Programmer:  Bill Wendling, 02. August, 2002
 * Modifications:
 */
herr_t
H5FP_request_read_metadata(H5FD_t *file, unsigned file_id, hid_t dxpl_id,
                           H5FD_mem_t UNUSED mem_type, haddr_t addr,
                           size_t size, uint8_t **buf,
                           unsigned *req_id, H5FP_status_t *status)
{
    H5FP_request_t req;
    H5FP_read_t sap_read;
    MPI_Status mpi_status;
    int mrc, my_rank;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5FP_request_read_metadata, FAIL);

    /* check args */
    assert(file);
    assert(buf);
    assert(req_id);
    assert(status);

    HDmemset(&req, 0, sizeof(req));

    if ((mrc = MPI_Comm_rank(H5FP_SAP_COMM, &my_rank)) != MPI_SUCCESS)
        HMPI_GOTO_ERROR(FAIL, "MPI_Comm_rank failed", mrc);

    *status = H5FP_STATUS_OK;
    req.req_type = H5FP_REQ_READ;
    req.req_id = H5FP_gen_request_id();
    req.file_id = file_id;
    req.proc_rank = my_rank;
    req.addr = addr;

    if ((mrc = MPI_Send(&req, 1, H5FP_request, (int)H5FP_sap_rank,
                        H5FP_TAG_REQUEST, H5FP_SAP_COMM)) != MPI_SUCCESS)
        HMPI_GOTO_ERROR(FAIL, "MPI_Send failed", mrc);

    HDmemset(&mpi_status, 0, sizeof(mpi_status));

    if ((mrc = MPI_Recv(&sap_read, 1, H5FP_read, (int)H5FP_sap_rank, H5FP_TAG_READ,
                        H5FP_SAP_COMM, &mpi_status)) != MPI_SUCCESS)
        HMPI_GOTO_ERROR(FAIL, "MPI_Recv failed", mrc);

    switch (sap_read.status) {
    case H5FP_STATUS_OK:
        /* use the info in the H5FP_read_t structure to update the metadata */
        *status = H5FP_STATUS_OK;
        HDmemset(*buf, '\0', size);
        HDmemset(&mpi_status, 0, sizeof(mpi_status));

        /* the following code is a bit odd and doubtless needs a bit
         * of explanation.  I certainly stumbled over it the first
         * time I read it.
         *
         * For reasons unknown, read requests sent to the SAP only
         * include a base address, not a length.  Thus the SAP sends
         * along the largest contiguous chunk it has starting at the
         * specified address.
         *
         * If the chunk is bigger than we want, we just copy over what
         * we want, and discard the rest.
         *
         * If it is just the right size, we receive it in the provided
         * buffer.
         *
         * if it is too small to fulfil our request, we scream and die.
         *
         *					JRM - 4/13/04
         */
        if (size < sap_read.md_size)
        {
            char *mdata;

            if (H5FP_read_metadata(&mdata, (int)sap_read.md_size, (int)H5FP_sap_rank) == FAIL) {
HDfprintf(stderr, "Metadata Read Failed!!!!\n");
            }

            HDmemcpy(*buf, mdata, size);
            HDfree(mdata);
        } else if (size == sap_read.md_size) {
            if ((mrc = MPI_Recv(*buf, (int)sap_read.md_size, MPI_BYTE,
                                (int)H5FP_sap_rank, H5FP_TAG_METADATA,
                                H5FP_SAP_COMM, &mpi_status)) != MPI_SUCCESS)
                HMPI_GOTO_ERROR(FAIL, "MPI_Recv failed", mrc);
        } else {
	    HDfprintf(stdout,
                    "H5FP_request_read_metadata: size = %d > md_size = %d.\n",
                    (int)size, (int)(sap_read.md_size));
            HDfprintf(stdout, "Mssg received from SAP is too small!!!!\n");
            assert(0);
        }

        break;
    case H5FP_STATUS_DUMPING:
        /*
         * Collect the metadata updates from the SAP and write them to
         * the file. We fall through because at this point the metadata
         * won't be cached on the server anymore.
         */
        if (H5FP_dump_to_file(file, dxpl_id) == FAIL)
            HGOTO_ERROR(H5E_FPHDF5, H5E_WRITEERROR, FAIL,
                        "can't write metadata update to file");
        /* FALLTHROUGH */
    case H5FP_STATUS_MDATA_NOT_CACHED:
        /*
         * The metadata wasn't in the SAP's cache. Should read from disk
         * now.
         */
        *status = H5FP_STATUS_MDATA_NOT_CACHED;
        break;
    default:
        *status = sap_read.status;
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTCHANGE, FAIL, "can't write metadata to server");
    }

done:
    *req_id = req.req_id;
    FUNC_LEAVE_NOAPI(ret_value);
}

/*
 * Function:    H5FP_request_write_metadata
 * Purpose:     Tell the SAP that we want to change a piece of metadata
 *              associated with the file. The request ID is returned in a
 *              pointer supplied by the user.
 *
 *              This function has the potential of causing the process to
 *              act as a dumper for the SAP's metadata. Places which call
 *              this function and check the STATUS variable should take
 *              this into account.
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 * Programmer:  Bill Wendling, 02. August, 2002
 * Modifications:
 */
herr_t
H5FP_request_write_metadata(H5FD_t *file, unsigned file_id, hid_t dxpl_id,
                            H5FD_mem_t mem_type, haddr_t addr,
                            int mdata_size, const char *mdata,
                            unsigned *req_id, H5FP_status_t *status)
{
    H5FP_reply_t sap_reply;
    MPI_Status mpi_status;
    H5FP_request_t req;
    int mrc, my_rank;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5FP_request_write_metadata, FAIL);

    /* check args */
    assert(file);
    assert(mdata);
    assert(req_id);
    assert(status);

    HDmemset(&req, 0, sizeof(req));

    if ((mrc = MPI_Comm_rank(H5FP_SAP_COMM, &my_rank)) != MPI_SUCCESS)
        HMPI_GOTO_ERROR(FAIL, "MPI_Comm_rank failed", mrc);

    req.req_type = H5FP_REQ_WRITE;
    req.req_id = H5FP_gen_request_id();
    req.proc_rank = my_rank;
    req.file_id = file_id;
    req.mem_type = mem_type;
    req.addr = addr;
    req.md_size = mdata_size;

    if ((mrc = MPI_Send(&req, 1, H5FP_request, (int)H5FP_sap_rank,
                        H5FP_TAG_REQUEST, H5FP_SAP_COMM)) != MPI_SUCCESS)
        HMPI_GOTO_ERROR(FAIL, "MPI_Send failed", mrc);

    /* The first MPI_Send will have been sent before this one will be read. */
    if (H5FP_send_metadata(mdata, mdata_size, (int)H5FP_sap_rank) != SUCCEED)
        HGOTO_ERROR(H5E_FPHDF5, H5E_CANTSENDMDATA, FAIL, "can't send metadata to server");

    HDmemset(&mpi_status, 0, sizeof(mpi_status));
    HDmemset(&sap_reply, 0, sizeof(sap_reply));

    if ((mrc = MPI_Recv(&sap_reply, 1, H5FP_reply, (int)H5FP_sap_rank,
                        H5FP_TAG_REPLY, H5FP_SAP_COMM, &mpi_status)) != MPI_SUCCESS)
        HMPI_GOTO_ERROR(FAIL, "MPI_Recv failed", mrc);

    switch (sap_reply.status) {
    case H5FP_STATUS_OK:
        /* Nothing to do... */
        break;
    case H5FP_STATUS_DUMPING:
        /*
         * Collect the metadata updates from the SAP and write them to
         * the file. The function which sends us the dumping data sends
         * it to us as an H5FP_read object instead of the H5FP_reply
         * object we got above. So we need this "extra" read.
         *
         * FIXME: This is probably too much of a hack and could be fixed
         * for read/write/closing instances...
         */
        if (H5FP_dump_to_file(file, dxpl_id) == FAIL) {
            *status = H5FP_STATUS_DUMPING_FAILED;
            HGOTO_ERROR(H5E_FPHDF5, H5E_WRITEERROR, FAIL,
                        "can't write metadata update to file");
        }

        if ((mrc = MPI_Recv(&sap_reply, 1, H5FP_reply, (int)H5FP_sap_rank,
                            H5FP_TAG_REPLY, H5FP_SAP_COMM, &mpi_status)) != MPI_SUCCESS)
            HMPI_GOTO_ERROR(FAIL, "MPI_Recv failed", mrc);

        if (sap_reply.status != H5FP_STATUS_OK)
            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTCHANGE, FAIL, "can't write metadata to server");

        break;
    default:
        *status = sap_reply.status;
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTCHANGE, FAIL, "can't write metadata to server");
    }

    *status = H5FP_STATUS_OK;

done:
    *req_id = req.req_id;
    FUNC_LEAVE_NOAPI(ret_value);
}

/*
 * Function:    H5FP_request_flush_metadata
 * Purpose:     Tell the SAP that we want to change a piece of metadata
 *              associated with the file. The request ID is returned in a
 *              pointer supplied by the user.
 *
 *              This function has the potential of causing the process to
 *              act as a dumper for the SAP's metadata. Places which call
 *              this function and check the STATUS variable should take
 *              this into account.
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 * Programmer:  Bill Wendling, 02. August, 2002
 * Modifications:
 */
herr_t
H5FP_request_flush_metadata(H5FD_t *file, unsigned file_id, hid_t dxpl_id,
                            unsigned *req_id, H5FP_status_t *status)
{
    H5FP_reply_t sap_reply;
    H5FP_request_t req;
    MPI_Status mpi_status;
    int mrc, ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5FP_request_flush_metadata, FAIL);

    /* check args */
    assert(file);
    assert(req_id);
    assert(status);

    HDmemset(&req, 0, sizeof(req));

    if ((mrc = MPI_Comm_rank(H5FP_SAP_COMM, (int *)&req.proc_rank)) != MPI_SUCCESS)
        HMPI_GOTO_ERROR(FAIL, "MPI_Comm_rank failed", mrc);

    req.req_type = H5FP_REQ_FLUSH;
    req.req_id = H5FP_gen_request_id();
    req.file_id = file_id;

    if ((mrc = MPI_Send(&req, 1, H5FP_request, (int)H5FP_sap_rank,
                        H5FP_TAG_REQUEST, H5FP_SAP_COMM)) != MPI_SUCCESS)
        HMPI_GOTO_ERROR(FAIL, "MPI_Send failed", mrc);

    HDmemset(&mpi_status, 0, sizeof(mpi_status));

    if ((mrc = MPI_Recv(&sap_reply, 1, H5FP_reply, (int)H5FP_sap_rank,
                        H5FP_TAG_REPLY, H5FP_SAP_COMM, &mpi_status)) != MPI_SUCCESS)
        HMPI_GOTO_ERROR(FAIL, "MPI_Recv failed", mrc);

    switch (sap_reply.status) {
    case H5FP_STATUS_OK:
        /* Nothing to do... */
        break;
    case H5FP_STATUS_DUMPING:
        /*
         * Collect the metadata updates from the SAP and write them to
         * the file. The function which sends us the dumping data sends
         * it to us as an H5FP_read object instead of the H5FP_reply
         * object we got above.
         */
        if (H5FP_dump_to_file(file, dxpl_id) == FAIL) {
            *status = H5FP_STATUS_DUMPING_FAILED;
            HGOTO_ERROR(H5E_FPHDF5, H5E_WRITEERROR, FAIL,
                        "can't write metadata update to file");
        }

        if ((mrc = MPI_Recv(&sap_reply, 1, H5FP_reply, (int)H5FP_sap_rank,
                            H5FP_TAG_REPLY, H5FP_SAP_COMM, &mpi_status)) != MPI_SUCCESS)
            HMPI_GOTO_ERROR(FAIL, "MPI_Recv failed", mrc);

        if (sap_reply.status != H5FP_STATUS_OK)
            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTCHANGE, FAIL, "can't write metadata to server");

        break;
    default:
        *status = sap_reply.status;
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTCHANGE, FAIL, "can't write metadata to server");
    }

    *status = H5FP_STATUS_OK;

done:
    *req_id = req.req_id;
    FUNC_LEAVE_NOAPI(ret_value);
}

/*
 * Function:    H5FP_request_close
 * Purpose:     Tell the SAP that we want all of the structural changes
 *              made on the file and then close the file. The request ID
 *              is returned in a pointer passed to the function by the
 *              user.
 *
 *              This function has the potential of causing the process to
 *              act as a dumper for the SAP's metadata. Places which call
 *              this function and check the STATUS variable should take
 *              this into account.
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 * Programmer:  Bill Wendling, 02. August, 2002
 * Modifications:
 */
herr_t
H5FP_request_close(H5FD_t *file, unsigned file_id, unsigned *req_id,
                   H5FP_status_t *status)
{
    H5FP_reply_t    sap_reply;
    H5FP_request_t  req;
    MPI_Status      mpi_status;
    int             mrc;
    herr_t          ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5FP_request_close, FAIL);

    /* check args */
    assert(file);
    assert(req_id);
    assert(status);

    HDmemset(&req, 0, sizeof(req));
    req.req_type = H5FP_REQ_CLOSE;
    req.req_id = H5FP_gen_request_id();
    req.file_id = file_id;
    req.proc_rank = H5FD_mpi_get_rank(file);

    if ((mrc = MPI_Send(&req, 1, H5FP_request, (int)H5FP_sap_rank,
                        H5FP_TAG_REQUEST, H5FP_SAP_COMM)) != MPI_SUCCESS)
        HMPI_GOTO_ERROR(FAIL, "MPI_Send failed", mrc);

    HDmemset(&mpi_status, 0, sizeof(mpi_status));

    if ((mrc = MPI_Recv(&sap_reply, 1, H5FP_reply, (int)H5FP_sap_rank,
                        H5FP_TAG_REPLY, H5FP_SAP_COMM, &mpi_status)) != MPI_SUCCESS)
        HMPI_GOTO_ERROR(FAIL, "MPI_Recv failed", mrc);

    if (sap_reply.status != H5FP_STATUS_OK)
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTCHANGE, FAIL, "can't close file on server");

    *status = H5FP_STATUS_OK;

done:
    *req_id = req.req_id;
    FUNC_LEAVE_NOAPI(ret_value);
}


/*
 * Function:    H5FP_client_alloc
 * Purpose:     Handle the client side of an allocation in the FP case.
 *		In essence, this is simply a matter of referring the
 *		request to the SAP, and then returning the reply.
 *
 *		A modified version of this code used to live in H5FD_alloc(),
 *		but I move it here to encapsulate it and generally tidy up.
 *
 *		One can argue that we should all be done in an alloc
 *		routine in H5FDfdhdf5.c, but this invlves a smaller
 *		change to the code, and thus a smaller loss if I missed
 *		a major gotcha.  If things go well, and we don't heave
 *		the current implementation of FP, I'll probably go that
 *		route eventually.
 * Return:      Success:    The format address of the new file memory.
 *              Failure:    The undefined address HADDR_UNDEF
 * Programmer:  JRM - 4/7/04
 * Modifications:
 */
haddr_t
H5FP_client_alloc(H5FD_t *file, H5FD_mem_t type, hid_t dxpl_id, hsize_t size)
{
    haddr_t         ret_value = HADDR_UNDEF;
    unsigned        req_id = 0;
    unsigned        capt_only = 0;
    H5FP_status_t   status = H5FP_STATUS_OK;
    H5P_genplist_t *plist;
    H5FP_alloc_t    fp_alloc;

    FUNC_ENTER_NOAPI(H5FP_client_alloc, HADDR_UNDEF)

    /* check args */
    HDassert(file);
    HDassert(file->cls);
    HDassert(type >= 0 && type < H5FD_MEM_NTYPES);
    HDassert(size > 0);

    /* verify that we are running FP and we are not the SAP.  */
    HDassert(H5FD_is_fphdf5_driver(file) && !H5FD_fphdf5_is_sap(file));

    /* Get the data xfer property list */
    if ( (plist = H5I_object(dxpl_id)) == NULL ) {
        HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, HADDR_UNDEF, "not a dataset transfer list")
    }

    if ( H5P_exist_plist(plist, H5FD_FPHDF5_CAPTN_ALLOC_ONLY) > 0 ) {
        if ( H5P_get(plist, H5FD_FPHDF5_CAPTN_ALLOC_ONLY, &capt_only) < 0 ) {
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, HADDR_UNDEF, "can't retrieve FPHDF5 property")
        }
    }

    HDmemset(&fp_alloc, 0, sizeof(fp_alloc));

    /*
     * If the captain is the only one who should allocate resources,
     * then do just that...
     */
    if ( !capt_only || H5FD_fphdf5_is_captain(file) ) {
        /* Send the request to the SAP */
        if ( H5FP_request_allocate(file, type, size, &fp_alloc.addr,
                                   &fp_alloc.eoa, &req_id, &status)
             != SUCCEED ) {
                HGOTO_ERROR(H5E_FPHDF5, H5E_CANTALLOC, HADDR_UNDEF,
                            "server couldn't allocate from file")
        }
    }

    /* It should be impossible for this assertion to fail, but then
     * that is what assertions are for.
     */
    HDassert(status == H5FP_STATUS_OK);

    if ( capt_only ) {
        int mrc;

        if ( (mrc = MPI_Bcast(&fp_alloc, 1, H5FP_alloc,
                              (int)H5FP_capt_barrier_rank,
                              H5FP_SAP_BARRIER_COMM)) != MPI_SUCCESS ) {
            HMPI_GOTO_ERROR(HADDR_UNDEF, "MPI_Bcast failed", mrc);
        }
    }

    /* we used to send the eoa to the sap here, but that is silly,
     * as the sap already knows, and it is possible that another
     * interleaving allocation will result in a corrupted eoa.
     *
     *                                     JRM - 4/7/04
     */

    /* We've succeeded -- return the value */
    HGOTO_DONE(fp_alloc.addr)

done:
    FUNC_LEAVE_NOAPI(ret_value)

} /* H5FP_client_alloc() */


/* This function is now called only by H5FP_client_alloc() above.
 * Should we make it a private function only accessible from this
 * file?                                 JRM - 4/8/04
 */
/*
 * Function:    H5FP_request_allocate
 * Purpose:     Request an allocation of space from the SAP.
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 * Programmer:  Bill Wendling, 19. February 2003
 * Modifications:
 */
herr_t
H5FP_request_allocate(H5FD_t *file, H5FD_mem_t mem_type, hsize_t size,
                      haddr_t *addr, haddr_t *eoa, unsigned *req_id,
                      H5FP_status_t *status)
{
    H5FP_alloc_t    sap_alloc;
    H5FP_request_t  req;
    MPI_Status      mpi_status;
    int             mrc;
    herr_t          ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5FP_request_allocate, FAIL);

    /* check args */
    assert(file);
    assert(addr);
    assert(eoa);
    assert(req_id);
    assert(status);

    HDmemset(&req, 0, sizeof(req));

    req.req_type = H5FP_REQ_ALLOC;
    req.req_id = H5FP_gen_request_id();
    req.file_id = H5FD_fphdf5_file_id(file);
    req.proc_rank = H5FD_mpi_get_rank(file);
    req.mem_type = mem_type;
    req.meta_block_size = size; /* use this field as the size to allocate */

    if ((mrc = MPI_Send(&req, 1, H5FP_request, (int)H5FP_sap_rank,
                        H5FP_TAG_REQUEST, H5FP_SAP_COMM)) != MPI_SUCCESS)
        HMPI_GOTO_ERROR(FAIL, "MPI_Send failed", mrc);

    HDmemset(&mpi_status, 0, sizeof(mpi_status));

    if ((mrc = MPI_Recv(&sap_alloc, 1, H5FP_alloc, (int)H5FP_sap_rank,
                        H5FP_TAG_ALLOC, H5FP_SAP_COMM, &mpi_status)) != MPI_SUCCESS)
        HMPI_GOTO_ERROR(FAIL, "MPI_Recv failed", mrc);

    if (sap_alloc.status != H5FP_STATUS_OK)
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTCHANGE, FAIL, "can't allocate space on server");

    *status = H5FP_STATUS_OK;
    *addr = sap_alloc.addr;
    *eoa = sap_alloc.eoa;

done:
    *req_id = req.req_id;
    FUNC_LEAVE_NOAPI(ret_value);
}

/*
 * Function:    H5FP_request_free
 * Purpose:     Request freeing of space from the SAP.
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 * Programmer:  Bill Wendling, 20. February 2003
 * Modifications:
 */
herr_t
H5FP_request_free(H5FD_t *file, H5FD_mem_t mem_type, haddr_t addr, hsize_t size,
                  unsigned *req_id, H5FP_status_t *status)
{
    H5FP_alloc_t    sap_alloc;
    H5FP_request_t  req;
    MPI_Status      mpi_status;
    int             mrc;
    herr_t          ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5FP_request_free, FAIL);

    /* check args */
    assert(file);
    assert(req_id);
    assert(status);

    /* Allow zero-sized frees to occur without penalty */
    if (size == 0)
        HGOTO_DONE(SUCCEED);

    HDmemset(&req, 0, sizeof(req));

    req.req_type = H5FP_REQ_FREE;
    req.req_id = H5FP_gen_request_id();
    req.file_id = H5FD_fphdf5_file_id(file);
    req.proc_rank = H5FD_mpi_get_rank(file);
    req.mem_type = mem_type;
    req.addr = addr;
    req.meta_block_size = size;

    if ((mrc = MPI_Send(&req, 1, H5FP_request, (int)H5FP_sap_rank,
                        H5FP_TAG_REQUEST, H5FP_SAP_COMM)) != MPI_SUCCESS)
        HMPI_GOTO_ERROR(FAIL, "MPI_Send failed", mrc);

    HDmemset(&mpi_status, 0, sizeof(mpi_status));

    if ((mrc = MPI_Recv(&sap_alloc, 1, H5FP_alloc, (int)H5FP_sap_rank,
                        H5FP_TAG_ALLOC, H5FP_SAP_COMM, &mpi_status)) != MPI_SUCCESS)
        HMPI_GOTO_ERROR(FAIL, "MPI_Recv failed", mrc);

    if (sap_alloc.status != H5FP_STATUS_OK)
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTCHANGE, FAIL, "can't free space on server");

#if 0 /* JRM */
    /* the set_eoa call just sends the eoa we received from the SAP back
     * -- with obvious race condition problems if there are interleaving
     * calls.  Thus I am commenting this call out for now, and will delete
     * it in time if I can't find a reason for it.
     *
     *                                        JRM -- 4/7/04
     */
    /* Set the EOA for all processes. This call doesn't fail. */
    file->cls->set_eoa(file, sap_alloc.eoa);
#endif /* JRM */
    *status = H5FP_STATUS_OK;

done:
    *req_id = req.req_id;
    FUNC_LEAVE_NOAPI(ret_value);
}

/*
 * Function:    H5FP_request_get_eoa
 * Purpose:     Request the SAP send the EOA of the file.
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 * Programmer:  Bill Wendling, 30. October 2003
 * Modifications:
 */
herr_t
H5FP_request_get_eoa(const H5FD_t *file, haddr_t *eoa, unsigned *req_id, H5FP_status_t *status)
{
    H5FP_eoa_t      sap_eoa;
    H5FP_request_t  req;
    MPI_Status      mpi_status;
    int             mrc;
    herr_t          ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5FP_request_get_eoa, FAIL);

    /* check args */
    assert(file);
    assert(eoa);
    assert(req_id);
    assert(status);

    HDmemset(&req, 0, sizeof(req));

    req.req_type = H5FP_REQ_GET_EOA;
    req.req_id = H5FP_gen_request_id();
    req.file_id = H5FD_fphdf5_file_id(file);
    req.proc_rank = H5FD_mpi_get_rank(file);

    if ((mrc = MPI_Send(&req, 1, H5FP_request, (int)H5FP_sap_rank,
                        H5FP_TAG_REQUEST, H5FP_SAP_COMM)) != MPI_SUCCESS)
        HMPI_GOTO_ERROR(FAIL, "MPI_Send failed", mrc);

    HDmemset(&mpi_status, 0, sizeof(mpi_status));

    if ((mrc = MPI_Recv(&sap_eoa, 1, H5FP_eoa, (int)H5FP_sap_rank,
                        H5FP_TAG_EOA, H5FP_SAP_COMM, &mpi_status)) != MPI_SUCCESS)
        HMPI_GOTO_ERROR(FAIL, "MPI_Recv failed", mrc);

    if (sap_eoa.status != H5FP_STATUS_OK)
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTCHANGE, FAIL, "can't free space on server");

    /* Set the EOA for all processes. This doesn't fail. */
    *eoa = sap_eoa.eoa;
    *status = H5FP_STATUS_OK;

done:
    *req_id = req.req_id;
    FUNC_LEAVE_NOAPI(ret_value);
}

/*
 * Function:    H5FP_request_set_eoa
 * Purpose:     Request the SAP set the EOA of the file.
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 * Programmer:  Bill Wendling, 31. October 2003
 * Modifications:
 */
herr_t
H5FP_request_set_eoa(H5FD_t *file, haddr_t eoa, unsigned *req_id, H5FP_status_t *status)
{
    H5FP_reply_t    sap_reply;
    H5FP_request_t  req;
    MPI_Status      mpi_status;
    int             mrc;
    herr_t          ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5FP_request_set_eoa, FAIL);

    /* check args */
    assert(file);
    assert(req_id);
    assert(status);

    HDmemset(&req, 0, sizeof(req));

    req.req_type = H5FP_REQ_SET_EOA;
    req.req_id = H5FP_gen_request_id();
    req.file_id = H5FD_fphdf5_file_id(file);
    req.proc_rank = H5FD_mpi_get_rank(file);
    req.addr = eoa;

#if 0
    /* This is useful debugging code -- lets keep for a while.
     *                                     JRM -- 4/13/04
     */
    /* dump stack each time we set the eoa */
    {
        int mpi_rank;

        MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);

        HDfprintf(stdout,
                  "%d: %s: setting eoa: last eoa = %a, new eoa = %a.\n",
                  mpi_rank, "H5FP_request_set_eoa", last_eoa_received, eoa);
        H5FS_print(stdout);

    }
#endif

    if ((mrc = MPI_Send(&req, 1, H5FP_request, (int)H5FP_sap_rank,
                        H5FP_TAG_REQUEST, H5FP_SAP_COMM)) != MPI_SUCCESS)
        HMPI_GOTO_ERROR(FAIL, "MPI_Send failed", mrc);

    HDmemset(&mpi_status, 0, sizeof(mpi_status));

    if ((mrc = MPI_Recv(&sap_reply, 1, H5FP_reply, (int)H5FP_sap_rank,
                        H5FP_TAG_REPLY, H5FP_SAP_COMM, &mpi_status)) != MPI_SUCCESS)
        HMPI_GOTO_ERROR(FAIL, "MPI_Recv failed", mrc);

    if (sap_reply.status != H5FP_STATUS_OK)
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTCHANGE, FAIL, "can't close file on server");

    *status = H5FP_STATUS_OK;

done:
    *req_id = req.req_id;
    FUNC_LEAVE_NOAPI(ret_value);
}

/*
 * Function:    H5FP_request_update_eoma_eosda
 * Purpose:     Request the SAP updates the EOMA and EOSDA information
 *              for the file.
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 * Programmer:  Bill Wendling, 02. April 2003
 * Modifications:
 */
herr_t
H5FP_request_update_eoma_eosda(H5FD_t *file, unsigned *req_id, H5FP_status_t *status)
{
    H5FP_eoa_t      sap_eoa;
    H5FP_request_t  req;
    MPI_Status      mpi_status;
    int             mrc;
    herr_t          ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5FP_request_update_eoma_eosda, FAIL);

    /* check args */
    assert(file);
    assert(req_id);
    assert(status);

    HDmemset(&req, 0, sizeof(req));

    req.req_type = H5FP_REQ_UPDATE_EOMA_EOSDA;
    req.req_id = H5FP_gen_request_id();
    req.file_id = H5FD_fphdf5_file_id(file);
    req.proc_rank = H5FD_mpi_get_rank(file);

    if ((mrc = MPI_Send(&req, 1, H5FP_request, (int)H5FP_sap_rank,
                        H5FP_TAG_REQUEST, H5FP_SAP_COMM)) != MPI_SUCCESS)
        HMPI_GOTO_ERROR(FAIL, "MPI_Send failed", mrc);

    HDmemset(&mpi_status, 0, sizeof(mpi_status));

    if ((mrc = MPI_Recv(&sap_eoa, 1, H5FP_eoa, (int)H5FP_sap_rank,
                        H5FP_TAG_EOA, H5FP_SAP_COMM, &mpi_status)) != MPI_SUCCESS)
        HMPI_GOTO_ERROR(FAIL, "MPI_Recv failed", mrc);

    if (sap_eoa.status != H5FP_STATUS_OK)
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTCHANGE, FAIL, "can't free space on server");

    if ((mrc = MPI_Bcast(&sap_eoa, 1, H5FP_eoa, (int)H5FP_capt_barrier_rank,
                         H5FP_SAP_BARRIER_COMM)) != MPI_SUCCESS)
        HMPI_GOTO_ERROR(FAIL, "MPI_Bcast failed!", mrc);

#if 0
    /* The following set_eoa just parrots back to the SAP the eoa
     * we just received from it.  While I don't think it is a problem
     * in this case, there are obvious potentials for race conditions,
     * and I don't see that it does anything useful.
     *
     * Thus I am commenting it out for now.  I'll delete it completely
     * as soon as I am sure that it serves no purpose whatsoever.
     *
     *                                         JRM - 4/8/04
     */
    /* Set the EOA for all processes. This doesn't fail. */
    file->cls->set_eoa(file, sap_eoa.eoa);
#endif
    *status = H5FP_STATUS_OK;

done:
    *req_id = req.req_id;
    FUNC_LEAVE_NOAPI(ret_value);
}

/*
 *===----------------------------------------------------------------------===
 *                    Functions Private to Client Module
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
H5FP_gen_request_id()
{
    static unsigned int i = 0;
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5FP_gen_request_id);
    FUNC_LEAVE_NOAPI(i++);
}

/*
 * Function:    H5FP_dump_to_file
 * Purpose:     Dump the updated metadata to the file.
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 * Programmer:  Bill Wendling, 03. February 2003
 * Modifications:
 */
static herr_t
H5FP_dump_to_file(H5FD_t *file, hid_t dxpl_id)
{
    H5FP_read_t     sap_read;
    hid_t           new_dxpl_id = FAIL;
    H5P_genplist_t *plist = NULL, *old_plist;
    unsigned        dumping = 1;
    herr_t          ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT(H5FP_dump_to_file);

    /* check args */
    assert(file);

    if ((old_plist = H5I_object(dxpl_id)) == NULL)
        HGOTO_ERROR(H5E_PLIST, H5E_NOTFOUND, FAIL, "property object doesn't exist");

    /* Compare property lists */
    if ((new_dxpl_id = H5P_copy_plist(old_plist)) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTCOPY, FAIL, "can't copy property list");

    if ((plist = H5P_object_verify(new_dxpl_id, H5P_DATASET_XFER)) == NULL)
        HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a dataset transfer list");

    /* Set the fact that we're dumping metadata to the file */
    if (H5P_insert(plist, H5FD_FPHDF5_XFER_DUMPING_METADATA,
                   H5FD_FPHDF5_XFER_DUMPING_SIZE, &dumping,
                   NULL, NULL, NULL, NULL, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't insert MPI-I/O property");

    /*
     * At this point, we've received a message saying that the SAP is
     * dumping info to us. There's a metadata read waiting for us right
     * now...
     */
    for (;;) {
        MPI_Status mpi_status;
        int mrc;
        char *mdata;

        HDmemset(&mpi_status, 0, sizeof(mpi_status));
        HDmemset(&sap_read, 0, sizeof(sap_read));

        if ((mrc = MPI_Recv(&sap_read, 1, H5FP_read, (int)H5FP_sap_rank,
                            H5FP_TAG_DUMP, H5FP_SAP_COMM, &mpi_status)) != MPI_SUCCESS)
            HMPI_GOTO_ERROR(FAIL, "MPI_Recv failed", mrc);

        if (sap_read.status != H5FP_STATUS_DUMPING) {
            if (sap_read.status == H5FP_STATUS_DUMPING_FINISHED)
                break;

            /* FIXME: ERROR */
        }

        if (H5FP_read_metadata(&mdata, (int)sap_read.md_size,
                               (int)H5FP_sap_rank) != FAIL) {
            if (H5FD_fphdf5_write_real(file, H5FD_MEM_DEFAULT, plist,
                        sap_read.addr, (int)sap_read.md_size, mdata) == FAIL) {
                HDfree(mdata);
                HGOTO_ERROR(H5E_FPHDF5, H5E_WRITEERROR, FAIL, "can't write metadata to file");
            }

            HDfree(mdata);
        } else {
            /* FIXME: Error */
        }
    }

done:
    if (new_dxpl_id > 0)
        H5I_dec_ref(new_dxpl_id);

    FUNC_LEAVE_NOAPI(ret_value);
}

#endif  /* H5_HAVE_FPHDF5 */
