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

/* Private header files */
#include "H5private.h"          /* Generic Functions                    */
#include "H5Eprivate.h"         /* Error Handling                       */
#include "H5Oprivate.h"         /* Object Headers                       */
#include "H5TBprivate.h"        /* Threaded, Balanced, Binary Trees     */

#ifdef H5_HAVE_FPHDF5

#include "H5FPprivate.h"        /* Flexible Parallel Functions          */

/* Pablo mask */
#define PABLO_MASK          H5FP_mask

/* Interface initialization */
#define INTERFACE_INIT      NULL
static int interface_initialize_g = 0;

MPI_Datatype SAP_request_t;     /* MPI datatype for the SAP_request obj */
MPI_Datatype SAP_reply_t;       /* MPI datatype for the SAP_reply obj   */
MPI_Datatype SAP_sync_t;        /* MPI datatype for the SAP_sync obj    */

/* SAP specific variables */
MPI_Comm H5FP_SAP_COMM;         /* Comm we use: Supplied by user        */
MPI_Comm H5FP_SAP_BARRIER_COMM; /* Comm if you want to do a barrier     */

unsigned H5FP_sap_rank;         /* The rank of the SAP: Supplied by user*/
unsigned H5FP_capt_rank;        /* The rank which tells SAP of opens    */
unsigned H5FP_my_rank;          /* Rank of this process in the COMM     */
int H5FP_comm_size;             /* Size of the COMM                     */

/* local functions */
static herr_t H5FP_commit_sap_datatypes(void);
static herr_t H5FP_request_sap_stop(void);

/** API Functions **/

/*
 * Function:    H5FPinit
 * Purpose:     Initialize the SAP environment: duplicate the COMM the user
 *              supplies to us, set aside the SAP_RANK as the SAP.
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 * Programmer:  Bill Wendling, 26. July, 2002
 * Modifications:
 */
herr_t
H5FPinit(MPI_Comm comm, int sap_rank)
{
    MPI_Group sap_group = MPI_GROUP_NULL, sap_barrier_group = MPI_GROUP_NULL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API(H5FPinit, FAIL);
    H5TRACE2("e","McIs",comm,sap_rank);

    /* Set the global variable to track the SAP's rank */
    H5FP_sap_rank = sap_rank;

    /* Make a private copy of the communicator we were passed */
    if (MPI_Comm_dup(comm, &H5FP_SAP_COMM) != MPI_SUCCESS)
        HGOTO_ERROR(H5E_INTERNAL, H5E_MPI, FAIL, "MPI_Comm_dup failed");

    if (MPI_Comm_group(H5FP_SAP_COMM, &sap_group) != MPI_SUCCESS)
        HGOTO_ERROR(H5E_INTERNAL, H5E_MPI, FAIL, "MPI_Comm_group failed");

    /* Exclude the SAP from the barrier group group */
    if (MPI_Group_excl(sap_group, 1, (int *)&H5FP_sap_rank, &sap_barrier_group)
            != MPI_SUCCESS)
        HGOTO_ERROR(H5E_INTERNAL, H5E_MPI, FAIL, "MPI_Group_excl failed");

    /* Create communicator for barrier group (all processes except the SAP) */
    if (MPI_Comm_create(H5FP_SAP_COMM, sap_barrier_group, &H5FP_SAP_BARRIER_COMM)
            != MPI_SUCCESS)
        HGOTO_ERROR(H5E_INTERNAL, H5E_MPI, FAIL, "MPI_Comm_create failed");

    /* Get the size of all the processes (including the SAP) */
    if (MPI_Comm_size(H5FP_SAP_COMM, &H5FP_comm_size) != MPI_SUCCESS)
        HGOTO_ERROR(H5E_INTERNAL, H5E_MPI, FAIL, "MPI_Comm_size failed");

    /* we assign the process right after the sap_rank as the one which
     * will tell the SAP that files have been opened or closed.
     * we mod it so that we don't go over the size of the communicator. */
    H5FP_capt_rank = (H5FP_sap_rank + 1) % H5FP_comm_size;

    /* Get this processes rank */
    if (MPI_Comm_rank(H5FP_SAP_COMM, (int *)&H5FP_my_rank) != MPI_SUCCESS)
        HGOTO_ERROR(H5E_INTERNAL, H5E_MPI, FAIL, "MPI_Comm_rank failed");

    /* Create the MPI types used for communicating with the SAP */
    if (H5FP_commit_sap_datatypes() != MPI_SUCCESS)
        HGOTO_ERROR(H5E_INTERNAL, H5E_MPI, FAIL, "H5FP_commit_sap_datatypes failed");

    /* Go loop, if we are the SAP */
    if (H5FP_my_rank == H5FP_sap_rank)
        H5FP_sap_receive_loop();

    /* Fall through and return to user, if not SAP */

done:
    if (sap_group != MPI_GROUP_NULL)
        MPI_Group_free(&sap_group);

    if (sap_barrier_group != MPI_GROUP_NULL)
        MPI_Group_free(&sap_barrier_group);

    FUNC_LEAVE(ret_value);
}

/*
 * Function:    H5FPfinalize
 * Purpose:     Get rid of the initilized environment we setup with H5FPinit.
 *              Mostly just freeing the duplicated COMM object and committed
 *              datatypes.
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 * Programmer:  Bill Wendling, 26. July, 2002
 * Modifications:
 */
herr_t
H5FPfinalize(void)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API(H5FPfinalize, FAIL);
    H5TRACE0("e","");

    /* Stop the SAP */
    if (H5FP_my_rank != H5FP_sap_rank)
        if (H5FP_request_sap_stop() < 0)
            HGOTO_ERROR(H5E_INTERNAL, H5E_MPI, FAIL, "Error stopping the SAP");

    /* Release the MPI types we created */
    if (MPI_Type_free(&SAP_request_t) != MPI_SUCCESS)
        HGOTO_ERROR(H5E_INTERNAL, H5E_MPI, FAIL, "MPI_Type_free failed");

    if (MPI_Type_free(&SAP_reply_t) != MPI_SUCCESS)
        HGOTO_ERROR(H5E_INTERNAL, H5E_MPI, FAIL, "MPI_Type_free failed");

    if (MPI_Type_free(&SAP_sync_t) != MPI_SUCCESS)
        HGOTO_ERROR(H5E_INTERNAL, H5E_MPI, FAIL, "MPI_Type_free failed");

    /* Release the barrier communicator */
    if (H5FP_SAP_BARRIER_COMM != MPI_COMM_NULL)
        /* this comm will be NULL for the SAP */
        if (MPI_Comm_free(&H5FP_SAP_BARRIER_COMM) != MPI_SUCCESS)
            HGOTO_ERROR(H5E_INTERNAL, H5E_MPI, FAIL, "MPI_Comm_free failed");

    /* Release the FPH5 communicator */
    if (MPI_Comm_free(&H5FP_SAP_COMM) != MPI_SUCCESS)
        HGOTO_ERROR(H5E_INTERNAL, H5E_MPI, FAIL, "MPI_Comm_free failed");

done:
    FUNC_LEAVE(ret_value);
}

/** Public Libarary (non-API) Functions **/

/*
 * Function:    H5FP_send_metadata
 * Purpose:     Send a string of metadata to a process.
 *
 *              NOTE: You should never call this function directly!!
 *              There's special setup for sending a string to a processor
 *              which needs to occur first. The H5FP_request_* and
 *              H5FP_reply_* functions take care of this for you.
 *
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 * Programmer:  Bill Wendling, 30. July, 2002
 * Modifications:
 */
herr_t
H5FP_send_metadata(const char *mdata, int len, int rank)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5FP_send_metadata, FAIL);
    assert(mdata);
    assert(len);

    /* casts the CONST away: Okay */
    if (MPI_Send((void *)mdata, len, MPI_BYTE, rank, H5FP_TAG_METADATA, H5FP_SAP_COMM)
            != MPI_SUCCESS)
        HGOTO_ERROR(H5E_INTERNAL, H5E_MPI, FAIL, "MPI_Send failed");

done:
    FUNC_LEAVE(ret_value);
}

/** Private Functions **/

/*
 * Function:    H5FP_commit_sap_datatypes
 * Purpose:     Commit the SAP_request_t, SAP_reply_t, and SAP_sync_t
 *              structure datatypes to MPI.
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 * Programmer:  Bill Wendling, 26. July, 2002
 * Modifications:
 */
static herr_t
H5FP_commit_sap_datatypes(void)
{
    int block_length[2];
    MPI_Aint displs[2];
    MPI_Datatype old_types[2];
    struct SAP_request req;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5FP_commit_sap_datatypes, FAIL);

    /* Commit the SAP_request_t datatype */
    block_length[0] = 8;
    block_length[1] = sizeof(req.oid);
    MPI_Address(&req.req_type, &displs[0]);
    MPI_Address(&req.oid, &displs[1]);
    displs[1] -= displs[0];
    displs[0] -= displs[0];
    old_types[0] = MPI_INT;
    old_types[1] = MPI_UNSIGNED_CHAR;

    if (MPI_Type_struct(2, block_length, displs, old_types, &SAP_request_t) != MPI_SUCCESS)
        HGOTO_ERROR(H5E_INTERNAL, H5E_MPI, FAIL, "MPI_Type_struct failed");

    if (MPI_Type_commit(&SAP_request_t) != MPI_SUCCESS)
        HGOTO_ERROR(H5E_INTERNAL, H5E_MPI, FAIL, "MPI_Type_commit failed");

    /* Commit the SAP_reply_t datatype */
    block_length[0] = 3;
    displs[0] = 0;
    old_types[0] = MPI_INT;

    if (MPI_Type_struct(1, block_length, displs, old_types, &SAP_reply_t) != MPI_SUCCESS)
        HGOTO_ERROR(H5E_INTERNAL, H5E_MPI, FAIL, "MPI_Type_struct failed");

    if (MPI_Type_commit(&SAP_reply_t) != MPI_SUCCESS)
        HGOTO_ERROR(H5E_INTERNAL, H5E_MPI, FAIL, "MPI_Type_commit failed");

    /* Commit the SAP_sync_t datatype */
    block_length[0] = 8;
    displs[0] = 0;
    old_types[0] = MPI_INT;

    if (MPI_Type_struct(1, block_length, displs, old_types, &SAP_sync_t) != MPI_SUCCESS)
        HGOTO_ERROR(H5E_INTERNAL, H5E_MPI, FAIL, "MPI_Type_struct failed");

    if (MPI_Type_commit(&SAP_sync_t) != MPI_SUCCESS)
        HGOTO_ERROR(H5E_INTERNAL, H5E_MPI, FAIL, "MPI_Type_commit failed");

done:
    FUNC_LEAVE(ret_value);
}

/*
 * Function:    H5FP_request_sap_stop
 * Purpose:     Request that the SAP stop it's loop processing. Each
 *              process should send this to the SAP.
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 * Programmer:  Bill Wendling, 02. August, 2002
 * Modifications:
 */
static herr_t
H5FP_request_sap_stop(void)
{
    struct SAP_request req;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5FP_request_sap_stop, FAIL);

    HDmemset(&req, 0, sizeof(req));
    req.req_type = H5FP_REQ_STOP;
    req.req_id = 0;
    req.proc_rank = H5FP_my_rank;

    if (MPI_Send(&req, 1, SAP_request_t, (int)H5FP_sap_rank,
                 H5FP_TAG_REQUEST, H5FP_SAP_COMM) != MPI_SUCCESS)
        HGOTO_ERROR(H5E_INTERNAL, H5E_MPI, FAIL, "MPI_Send failed");

done:
    FUNC_LEAVE(ret_value);
}

#endif  /* H5_HAVE_FPHDF5 */
