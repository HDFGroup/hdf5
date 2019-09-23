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
#include "H5Eprivate.h"         /* Error handling                           */
#include "H5MMprivate.h"        /* Memory Management                        */


#ifdef H5_HAVE_PARALLEL

/*-------------------------------------------------------------------------
 * Function:    H5_mpi_comm_dup
 *
 * Purpose:     Duplicate an MPI communicator.
 *
 *              Does not duplicate MPI_COMM_NULL. Instead, comm_new will
 *              be set to MPI_COMM_NULL directly.
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

    /* Handle MPI_COMM_NULL separately */
    if (MPI_COMM_NULL == comm) {
        /* Don't duplicate MPI_COMM_NULL since that's an error in MPI */
        comm_dup = MPI_COMM_NULL;
    }
    else {

        /* Duplicate the MPI communicator */
        if (MPI_SUCCESS != (mpi_code = MPI_Comm_dup(comm, &comm_dup)))
            HMPI_GOTO_ERROR(FAIL, "MPI_Comm_dup failed", mpi_code)

        /* Set MPI_ERRORS_RETURN on comm_dup so that MPI failures are not fatal,
         * and return codes can be checked and handled.
         */
        if (MPI_SUCCESS != (mpi_code = MPI_Comm_set_errhandler(comm_dup, MPI_ERRORS_RETURN)))
            HMPI_GOTO_ERROR(FAIL, "MPI_Errhandler_set failed", mpi_code)
 
    }

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
        info_dup = MPI_INFO_NULL;
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


/*-------------------------------------------------------------------------
 * Function:    H5_mpi_comm_cmp
 *
 * Purpose:     Compares two MPI communicators.
 *
 *              Note that passing MPI_COMM_NULL to this function will not
 *              throw errors, unlike MPI_Comm_compare().
 *
 *              We consider MPI communicators to be the "same" when the
 *              groups are identical. We don't care about the context
 *              since that will always be different as we call MPI_Comm_dup
 *              when we store the communicator in the fapl.
 *
 *              The out parameter is a value like strcmp. The value is
 *              undefined when the return value is FAIL.
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5_mpi_comm_cmp(MPI_Comm comm1, MPI_Comm comm2, int *result)
{
    int         mpi_code;
    int         mpi_result = MPI_IDENT;
    herr_t      ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    /* Check arguments */
    if (!result)
        HGOTO_ERROR(H5E_INTERNAL, H5E_BADVALUE, FAIL, "result cannot be NULL")

    /* Set out parameter to something reasonable in case something goes wrong */
    *result = 0;

    /* Can't pass MPI_COMM_NULL to MPI_Comm_compare() so we have to handle
     * it in special cases.
     *
     * MPI_Comm can either be an integer type or a pointer. We cast them
     * to intptr_t so we can compare them with < and > when needed.
     */
    if (MPI_COMM_NULL == comm1 && MPI_COMM_NULL == comm2) {
        /* Special case of both communicators being MPI_COMM_NULL */
        *result = 0;
    }
    else if (MPI_COMM_NULL == comm1 || MPI_COMM_NULL == comm2) {

        /* Special case of one communicator being MPI_COMM_NULL */
        *result = (intptr_t)comm1 < (intptr_t)comm2 ? -1 : 1;
    }
    else {

        /* Normal communicator compare */

        /* Compare the MPI communicators */
        if (MPI_SUCCESS != (mpi_code = MPI_Comm_compare(comm1, comm2, &mpi_result)))
            HMPI_GOTO_ERROR(FAIL, "MPI_Comm_compare failed", mpi_code)

        /* Set the result */
        if (MPI_IDENT == mpi_result || MPI_CONGRUENT == mpi_result)
            *result = 0;
        else
            *result = (intptr_t)comm1 < (intptr_t)comm2 ? -1 : 1;
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5_mpi_comm_cmp() */


/*-------------------------------------------------------------------------
 * Function:    H5_mpi_info_cmp
 *
 * Purpose:     Compares two MPI info objects.
 *
 *              For our purposes, two mpi info objects are the "same" if
 *              they contain the same key-value pairs or are both
 *              MPI_INFO_NULL.
 *
 *              The out parameter is a value like strcmp. The value is
 *              undefined when the return value is FAIL.
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5_mpi_info_cmp(MPI_Info info1, MPI_Info info2, int *result)
{
    hbool_t     same = FALSE;
    char        *key = NULL;
    char        *value1 = NULL;
    char        *value2 = NULL;
    herr_t      ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    /* Check arguments */
    if (!result)
        HGOTO_ERROR(H5E_INTERNAL, H5E_BADVALUE, FAIL, "result cannot be NULL")

    /* Check for MPI_INFO_NULL */
    if (MPI_INFO_NULL == info1 && MPI_INFO_NULL == info2) {
        /* Special case of both info objects being MPI_INFO_NULL */
        same = TRUE;
    }
    else if (MPI_INFO_NULL == info1 || MPI_INFO_NULL == info2) {

        /* Special case of one info object being MPI_INFO_NULL */
        same = FALSE;
    }
    else {
        int         mpi_code;
        int         nkeys_1;
        int         nkeys_2;

        /* Check if the number of keys is the same */
        if (MPI_SUCCESS != (mpi_code = MPI_Info_get_nkeys(info1, &nkeys_1)))
            HMPI_GOTO_ERROR(FAIL, "MPI_Info_get_nkeys failed", mpi_code)
        if (MPI_SUCCESS != (mpi_code = MPI_Info_get_nkeys(info2, &nkeys_2)))
            HMPI_GOTO_ERROR(FAIL, "MPI_Info_get_nkeys failed", mpi_code)

        if (nkeys_1 != nkeys_2)
            same = FALSE;
        else if (0 == nkeys_1 && 0 == nkeys_2)
            same = TRUE;
        else {
            int i;
            int flag1 = -1;
            int flag2 = -1;

            /* Allocate buffers for iteration */
            if (NULL == (key = (char *)H5MM_malloc(MPI_MAX_INFO_KEY * sizeof(char))))
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")
            if (NULL == (value1 = (char *)H5MM_malloc(MPI_MAX_INFO_VAL * sizeof(char))))
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")
            if (NULL == (value2 = (char *)H5MM_malloc(MPI_MAX_INFO_VAL * sizeof(char))))
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")

            /* Iterate over the keys, comparing them */
            for (i = 0; i < nkeys_1; i++) {

                same = TRUE;

                /* Memset the buffers to zero */
                HDmemset(key, 0, MPI_MAX_INFO_KEY);
                HDmemset(value1, 0, MPI_MAX_INFO_VAL);
                HDmemset(value2, 0, MPI_MAX_INFO_VAL);

                /* Get the nth key */
                if (MPI_SUCCESS != (mpi_code = MPI_Info_get_nthkey(info1, i, key)))
                    HMPI_GOTO_ERROR(FAIL, "MPI_Info_get_nthkey failed", mpi_code)

                /* Get the values */
                if (MPI_SUCCESS != (mpi_code = MPI_Info_get(info1, key, MPI_MAX_INFO_VAL, value1, &flag1)))
                    HMPI_GOTO_ERROR(FAIL, "MPI_Info_get failed", mpi_code)
                if (MPI_SUCCESS != (mpi_code = MPI_Info_get(info2, key, MPI_MAX_INFO_VAL, value2, &flag2)))
                    HMPI_GOTO_ERROR(FAIL, "MPI_Info_get failed", mpi_code)

                /* Compare values and flags */
                if (!flag1 || !flag2 || HDmemcmp(value1, value2, MPI_MAX_INFO_VAL)) {
                    same = FALSE;
                    break;
                }

            } /* end for */
        } /* end else */
    } /* end else */

    /* Set the output value
     *
     * MPI_Info can either be an integer type or a pointer. We cast them
     * to intptr_t so we can compare them with < and > when needed.
     */
    if (same)
        *result = 0;
    else
        *result = (intptr_t)info1 < (intptr_t)info2 ? -1 : 1;

done:
    if (key)
        H5MM_xfree(key);
    if (value1)
        H5MM_xfree(value1);
    if (value2)
        H5MM_xfree(value2);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5_mpi_info_cmp() */

#endif /* H5_HAVE_PARALLEL */

