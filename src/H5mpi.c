/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Purpose:     Common MPI routines
 *
 */


#include "H5private.h"          /* Generic Functions                        */
#include "H5CXprivate.h"        /* API Contexts                             */
#include "H5Eprivate.h"         /* Error handling                           */
#include "H5Fprivate.h"         /* File access                              */
#include "H5FDprivate.h"        /* File drivers                             */
#include "H5FDmpi.h"            /* Common MPI file driver                   */
#include "H5Pprivate.h"         /* Property lists                           */

#ifdef H5_HAVE_PARALLEL

/*-------------------------------------------------------------------------
 * Function:    H5_mpi_comm_dup
 *
 * Purpose:     Duplicate an MPI communicator.
 *
 *              The new communicator is returned via the comm_new pointer.
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5_mpi_comm_dup(MPI_Comm comm, MPI_Comm *comm_new)
{
    herr_t      ret_value = SUCCEED;
    MPI_Comm    comm_dup = MPI_COMM_NULL;
    int         mpi_code;

    FUNC_ENTER_NOAPI(FAIL)

    /* Check arguments */
    if (!comm_new)
        HGOTO_ERROR(H5E_INTERNAL, H5E_BADVALUE, FAIL, "comm_new cannot be NULL")
    if (MPI_COMM_NULL == comm)
        HGOTO_ERROR(H5E_INTERNAL, H5E_BADVALUE, FAIL, "can't duplicate MPI_COMM_NULL")

    /* Duplicate the MPI communicator */
    if (MPI_SUCCESS != (mpi_code = MPI_Comm_dup(comm, &comm_dup)))
        HMPI_GOTO_ERROR(FAIL, "MPI_Comm_dup failed", mpi_code)

    /* Set MPI_ERRORS_RETURN on comm_dup so that MPI failures are not fatal, 
     * and return codes can be checked and handled.
     */
    if (MPI_SUCCESS != (mpi_code = MPI_Comm_set_errhandler(comm_dup, MPI_ERRORS_RETURN)))
        HMPI_GOTO_ERROR(FAIL, "MPI_Errhandler_set failed", mpi_code)
 
    /* Copy the new communicator to the return argument */
    *comm_new = comm_dup;

done:
    if (FAIL == ret_value) {
        /* need to free anything created here */
        if (MPI_COMM_NULL != comm_dup)
            MPI_Comm_free(&comm_dup);
    }

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5_mpi_comm_dup() */ 


/*-------------------------------------------------------------------------
 * Function:    H5_mpi_info_dup
 *
 * Purpose:     Duplicate an MPI info.
 *
 *              If the info object is MPI_INFO_NULL, no duplicate
 *              is made but the same value assigned to the new info object
 *              handle.
 *
 *              The new info is returned via the info_new pointer.
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5_mpi_info_dup(MPI_Info info, MPI_Info *info_new)
{
    herr_t      ret_value = SUCCEED;
    MPI_Info    info_dup = MPI_INFO_NULL;
    int         mpi_code;

    FUNC_ENTER_NOAPI(FAIL)

    /* Check arguments */
    if (!info_new)
        HGOTO_ERROR(H5E_INTERNAL, H5E_BADVALUE, FAIL, "info_new cannot be NULL")

    /* Duplicate the MPI info */
    if (info == MPI_INFO_NULL) {
        /* Don't duplicate MPI_INFO_NULL. Just copy it. */
        info_dup = info;
    }
    else {
        /* Duplicate the info */
        if (MPI_SUCCESS != (mpi_code = MPI_Info_dup(info, &info_dup)))
            HMPI_GOTO_ERROR(FAIL, "MPI_Info_dup failed", mpi_code)
    }

    /* Copy the new info to the return argument */
    *info_new = info_dup;

done:
    if (FAIL == ret_value) {
        /* need to free anything created here */
        if (MPI_INFO_NULL != info_dup)
            MPI_Info_free(&info_dup);
    }

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5_mpi_info_dup() */ 


/*-------------------------------------------------------------------------
 * Function:    H5_mpi_comm_free
 *
 * Purpose:     Free an MPI communicator.
 *
 *              If comm is MPI_COMM_NULL this call does nothing.
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5_mpi_comm_free(MPI_Comm *comm)
{
    herr_t      ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    /* Check arguments */
    if (!comm)
        HGOTO_ERROR(H5E_INTERNAL, H5E_BADVALUE, FAIL, "comm pointer cannot be NULL")

    /* Free the communicator */
    if (MPI_COMM_NULL != *comm)
        MPI_Comm_free(comm);

    *comm = MPI_COMM_NULL;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* End H5_mpi_comm_free() */

/*-------------------------------------------------------------------------
 * Function:    H5_mpi_info_free
 *
 * Purpose:     Free the MPI info.
 *
 *              If info is MPI_INFO_NULL this call does nothing.
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5_mpi_info_free(MPI_Info *info)
{
    herr_t      ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    /* Check arguments */
    if (!info)
        HGOTO_ERROR(H5E_INTERNAL, H5E_BADVALUE, FAIL, "info pointer cannot be NULL")

    /* Free the info */
    if (MPI_INFO_NULL != *info)
        MPI_Info_free(info);

    *info = MPI_INFO_NULL;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* End H5_mpi_info_free() */

#endif /* H5_HAVE_PARALLEL */

