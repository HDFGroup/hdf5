/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Purpose:     Common MPI routines
 *
 */

#include "H5private.h"   /* Generic Functions                        */
#include "H5Eprivate.h"  /* Error handling                           */
#include "H5MMprivate.h" /* Memory Management                        */

#ifdef H5_HAVE_PARALLEL

/****************/
/* Local Macros */
/****************/
#define TWO_GIG_LIMIT INT32_MAX
#ifndef H5_MAX_MPI_COUNT
#define H5_MAX_MPI_COUNT (1 << 30)
#endif

/*******************/
/* Local Variables */
/*******************/
static hsize_t bigio_count_g = H5_MAX_MPI_COUNT;

/*-------------------------------------------------------------------------
 * Function:  H5_mpi_set_bigio_count
 *
 * Purpose:   Allow us to programmatically change the switch point
 *            when we utilize derived datatypes.  This is of
 *            particular interest for allowing nightly testing
 *
 * Return:    The current/previous value of bigio_count_g.
 *
 * Programmer: Richard Warren,  March 10, 2017
 *
 *-------------------------------------------------------------------------
 */
hsize_t
H5_mpi_set_bigio_count(hsize_t new_count)
{
    hsize_t orig_count = bigio_count_g;

    if ((new_count > 0) && (new_count < (hsize_t)TWO_GIG_LIMIT)) {
        bigio_count_g = new_count;
    }
    return orig_count;
} /* end H5_mpi_set_bigio_count() */

/*-------------------------------------------------------------------------
 * Function:  H5_mpi_get_bigio_count
 *
 * Purpose:   Allow other HDF5 library functions to access
 *            the current value for bigio_count_g.
 *
 * Return:    The current/previous value of bigio_count_g.
 *
 * Programmer: Richard Warren,  October 7, 2019
 *
 *-------------------------------------------------------------------------
 */
hsize_t
H5_mpi_get_bigio_count(void)
{
    return bigio_count_g;
}

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
    herr_t   ret_value = SUCCEED;
    MPI_Comm comm_dup  = MPI_COMM_NULL;
    int      mpi_code;

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
    herr_t   ret_value = SUCCEED;
    MPI_Info info_dup  = MPI_INFO_NULL;
    int      mpi_code;

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
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    /* Check arguments */
    if (!comm)
        HGOTO_ERROR(H5E_INTERNAL, H5E_BADVALUE, FAIL, "comm pointer cannot be NULL")

    /* Free the communicator */
    if (MPI_COMM_WORLD != *comm && MPI_COMM_NULL != *comm)
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
    herr_t ret_value = SUCCEED;

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
    int    mpi_code;
    int    mpi_result = MPI_IDENT;
    herr_t ret_value  = SUCCEED;

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
    hbool_t same      = FALSE;
    char *  key       = NULL;
    char *  value1    = NULL;
    char *  value2    = NULL;
    herr_t  ret_value = SUCCEED;

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
        int mpi_code;
        int nkeys_1;
        int nkeys_2;

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
        }     /* end else */
    }         /* end else */

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

/*-------------------------------------------------------------------------
 * Function:    H5_mpio_create_large_type
 *
 * Purpose:     Create a large datatype of size larger than what a 32 bit integer
 *              can hold.
 *
 * Return:      Non-negative on success, negative on failure.
 *
 *              *new_type    the new datatype created
 *
 * Programmer:  Mohamad Chaarawi
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5_mpio_create_large_type(hsize_t num_elements, MPI_Aint stride_bytes, MPI_Datatype old_type,
                          MPI_Datatype *new_type)
{
    int          num_big_types;   /* num times the 2G datatype will be repeated */
    int          remaining_bytes; /* the number of bytes left that can be held in an int value */
    hsize_t      leftover;
    int          block_len[2];
    int          mpi_code; /* MPI return code */
    MPI_Datatype inner_type, outer_type, leftover_type, type[2];
    MPI_Aint     disp[2], old_extent;
    herr_t       ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Calculate how many Big MPI datatypes are needed to represent the buffer */
    num_big_types = (int)(num_elements / bigio_count_g);
    leftover      = (hsize_t)num_elements - (hsize_t)num_big_types * bigio_count_g;
    H5_CHECKED_ASSIGN(remaining_bytes, int, leftover, hsize_t);

    /* Create a contiguous datatype of size equal to the largest
     * number that a 32 bit integer can hold x size of old type.
     * If the displacement is 0, then the type is contiguous, otherwise
     * use type_hvector to create the type with the displacement provided
     */
    if (0 == stride_bytes) {
        if (MPI_SUCCESS != (mpi_code = MPI_Type_contiguous((int)bigio_count_g, old_type, &inner_type)))
            HMPI_GOTO_ERROR(FAIL, "MPI_Type_contiguous failed", mpi_code)
    } /* end if */
    else if (MPI_SUCCESS !=
             (mpi_code = MPI_Type_create_hvector((int)bigio_count_g, 1, stride_bytes, old_type, &inner_type)))
        HMPI_GOTO_ERROR(FAIL, "MPI_Type_create_hvector failed", mpi_code)

    /* Create a contiguous datatype of the buffer (minus the remaining < 2GB part)
     * If a stride is present, use hvector type
     */
    if (0 == stride_bytes) {
        if (MPI_SUCCESS != (mpi_code = MPI_Type_contiguous(num_big_types, inner_type, &outer_type)))
            HMPI_GOTO_ERROR(FAIL, "MPI_Type_contiguous failed", mpi_code)
    } /* end if */
    else if (MPI_SUCCESS !=
             (mpi_code = MPI_Type_create_hvector(num_big_types, 1, stride_bytes, inner_type, &outer_type)))
        HMPI_GOTO_ERROR(FAIL, "MPI_Type_create_hvector failed", mpi_code)

    MPI_Type_free(&inner_type);

    /* If there is a remaining part create a contiguous/vector datatype and then
     * use a struct datatype to encapsulate everything.
     */
    if (remaining_bytes) {
        if (stride_bytes == 0) {
            if (MPI_SUCCESS != (mpi_code = MPI_Type_contiguous(remaining_bytes, old_type, &leftover_type)))
                HMPI_GOTO_ERROR(FAIL, "MPI_Type_contiguous failed", mpi_code)
        } /* end if */
        else if (MPI_SUCCESS != (mpi_code = MPI_Type_create_hvector(
                                     (int)(num_elements - (hsize_t)num_big_types * bigio_count_g), 1,
                                     stride_bytes, old_type, &leftover_type)))
            HMPI_GOTO_ERROR(FAIL, "MPI_Type_create_hvector failed", mpi_code)

        /* As of version 4.0, OpenMPI now turns off MPI-1 API calls by default,
         * so we're using the MPI-2 version even though we don't need the lb
         * value.
         */
        {
            MPI_Aint unused_lb_arg;
            MPI_Type_get_extent(old_type, &unused_lb_arg, &old_extent);
        }

        /* Set up the arguments for MPI_Type_create_struct() */
        type[0]      = outer_type;
        type[1]      = leftover_type;
        block_len[0] = 1;
        block_len[1] = 1;
        disp[0]      = 0;
        disp[1]      = (old_extent + stride_bytes) * num_big_types * (MPI_Aint)bigio_count_g;

        if (MPI_SUCCESS != (mpi_code = MPI_Type_create_struct(2, block_len, disp, type, new_type)))
            HMPI_GOTO_ERROR(FAIL, "MPI_Type_create_struct failed", mpi_code)

        MPI_Type_free(&outer_type);
        MPI_Type_free(&leftover_type);
    } /* end if */
    else
        /* There are no remaining bytes so just set the new type to
         * the outer type created */
        *new_type = outer_type;

    if (MPI_SUCCESS != (mpi_code = MPI_Type_commit(new_type)))
        HMPI_GOTO_ERROR(FAIL, "MPI_Type_commit failed", mpi_code)

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5_mpio_create_large_type() */

/*-------------------------------------------------------------------------
 * Function:    H5_mpio_gatherv_to_contig_array
 *
 * Purpose:     A wrapper around MPI_(All)gatherv that performs allocation
 *              of the receive buffer on the caller's behalf and gathers
 *              data to a contiguous array. This routine's parameters are
 *              as follows:
 *
 *              `send_buf` - The buffer that data will be sent from for
 *                           the calling MPI rank. Analogous to
 *                           MPI_(All)gatherv's `sendbuf` parameter.
 *
 *              `send_count` - The number of `send_type` elements in the
 *                             send buffer. Analogous to MPI_(All)gatherv's
 *                             `sendcount` parameter.
 *
 *              `send_type` - The MPI Datatype of the elements in the send
 *                            buffer. Analogous to MPI_(All)gatherv's
 *                            `sendtype` parameter.
 *
 *              `recv_type` - The MPI Datatype of the elements in the
 *                            receive buffer. Analogous to
 *                            MPI_(All)gatherv's `recvtype` parameter.
 *
 *              `allgather` - Specifies whether the gather operation to be
 *                            performed should be MPI_Allgatherv (TRUE) or
 *                            MPI_Gatherv (FALSE).
 *
 *              `sort_func` - An optional qsort callback. If non-NULL, the
 *                            resulting array will be sorted according to
 *                            the callback function before being returned.
 *
 *              `root` - For MPI_Gatherv operations, specifies the rank
 *                       that will receive the data sent by other ranks.
 *                       Analogous to MPI_Gatherv's `root` parameter. For
 *                       MPI_Allgatherv operations, this parameter is
 *                       ignored.
 *
 *              `comm` - Specifies the MPI Communicator for the operation.
 *                       Analogous to MPI_(All)gatherv's `comm` parameter.
 *
 *              `mpi_rank` - Specifies the calling rank's rank value, as
 *                           obtained by calling MPI_Comm_rank on the
 *                           MPI Communicator `comm`.
 *
 *              `mpi_size` - Specifies the MPI Communicator size, as
 *                           obtained by calling MPI_Comm_size on the
 *                           MPI Communicator `comm`.
 *
 *              `out_array` - Resulting array that is allocated and
 *                            returned to the caller after data has been
 *                            gathered into it. Returned only to the rank
 *                            specified by `root` for MPI_Gatherv
 *                            operations, or to all ranks for
 *                            MPI_Allgatherv operations.
 *
 *              `out_array_num_entries` - The number of elements in the
 *                                        resulting array, in terms of
 *                                        the MPI Datatype provided for
 *                                        `recv_type`.
 *
 * Notes:       This routine is collective across `comm`.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5_mpio_gatherv_to_contig_array(void *send_buf, int send_count, MPI_Datatype send_type,
                                MPI_Datatype recv_type, hbool_t allgather, H5_sort_func_cb_t sort_func,
                                int root, MPI_Comm comm, int mpi_rank, int mpi_size, void **out_array,
                                size_t *out_array_num_entries)
{
    size_t recv_buf_num_entries = 0;
    void * recv_buf             = NULL;
#if MPI_VERSION >= 3
    MPI_Count type_lb;
    MPI_Count type_extent;
#else
    int type_lb;
    int type_extent;
#endif
    int    mpi_code;
    int *  recv_counts_disps_array = NULL;
    herr_t ret_value               = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    HDassert(send_buf || send_count == 0);
    if (allgather || (mpi_rank == root))
        HDassert(out_array && out_array_num_entries);

        /* Retrieve the extent of the MPI Datatype being used */
#if MPI_VERSION >= 3
    if (MPI_SUCCESS != (mpi_code = MPI_Type_get_extent_x(recv_type, &type_lb, &type_extent)))
#else
    if (MPI_SUCCESS != (mpi_code = MPI_Type_get_extent(recv_type, &type_lb, &type_extent)))
#endif
        HMPI_GOTO_ERROR(FAIL, "MPI_Type_get_extent(_x) failed", mpi_code)

    HDassert(type_lb == 0);

    /* Allocate array to store the send counts of each rank, as well as
     * the displacements into the final array where each rank will place
     * their data. The first half of the array contains the send counts
     * (in rank order), while the latter half contains the displacements
     * (also in rank order).
     */
    if (allgather || (mpi_rank == root)) {
        if (NULL == (recv_counts_disps_array =
                         H5MM_malloc(2 * (size_t)mpi_size * sizeof(*recv_counts_disps_array)))) {
            if (!allgather) {
                /* Push an error, but still participate in collective gather operation */
                HDONE_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL,
                            "couldn't allocate receive counts and displacements array")
            }
            else
                HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL,
                            "couldn't allocate receive counts and displacements array")
        }
    }

    /* If gathering to all ranks, inform each rank of how many copies of
     * the `send_type` datatype each other rank is contributing to the
     * resulting array. Otherwise, inform only the root rank of how many
     * copies of `send_type` each rank is contributing.
     */
    if (allgather) {
        if (MPI_SUCCESS !=
            (mpi_code = MPI_Allgather(&send_count, 1, MPI_INT, recv_counts_disps_array, 1, MPI_INT, comm)))
            HMPI_GOTO_ERROR(FAIL, "MPI_Allgather failed", mpi_code)
    }
    else {
        if (MPI_SUCCESS !=
            (mpi_code = MPI_Gather(&send_count, 1, MPI_INT, recv_counts_disps_array, 1, MPI_INT, root, comm)))
            HMPI_GOTO_ERROR(FAIL, "MPI_Gather failed", mpi_code)
    }

    /* Calculate the total size of and allocate the final array, then
     * set the displacements into the array for the final gather operation.
     */
    if (allgather || (mpi_rank == root)) {
        size_t i;
        size_t total_data_size;
        int *  displacements_ptr;

#if MPI_VERSION >= 3
        H5_CHECK_OVERFLOW(type_extent, MPI_Count, size_t);
#endif

        for (i = 0, recv_buf_num_entries = 0; i < (size_t)mpi_size; i++)
            recv_buf_num_entries += (size_t)recv_counts_disps_array[i];
        total_data_size = recv_buf_num_entries * (size_t)type_extent;

        /* Check if there is no work to do */
        if (total_data_size != 0) {
            if (NULL == (recv_buf = H5MM_malloc(total_data_size))) {
                if (!allgather) {
                    /* Push an error, but still participate in collective gather operation */
                    HDONE_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL,
                                "couldn't allocate buffer for gather operation")
                }
                else
                    HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL,
                                "couldn't allocate buffer for gather operation")
            }
        }

        displacements_ptr = &recv_counts_disps_array[mpi_size];

        *displacements_ptr = 0;
        for (i = 1; i < (size_t)mpi_size; i++)
            displacements_ptr[i] = displacements_ptr[i - 1] + recv_counts_disps_array[i - 1];
    }

    /* Perform the actual gather operation */
    if (allgather) {
        if (MPI_SUCCESS !=
            (mpi_code = MPI_Allgatherv(send_buf, send_count, send_type, recv_buf, recv_counts_disps_array,
                                       &recv_counts_disps_array[mpi_size], recv_type, comm)))
            HMPI_GOTO_ERROR(FAIL, "MPI_Allgatherv failed", mpi_code)
    }
    else {
        if (MPI_SUCCESS !=
            (mpi_code = MPI_Gatherv(send_buf, send_count, send_type, recv_buf, recv_counts_disps_array,
                                    &recv_counts_disps_array[mpi_size], recv_type, root, comm)))
            HMPI_GOTO_ERROR(FAIL, "MPI_Gatherv failed", mpi_code)
    }

    if (allgather || (mpi_rank == root)) {
#if MPI_VERSION >= 3
        H5_CHECK_OVERFLOW(type_extent, MPI_Count, size_t);
#endif

        /* Sort the gathered array, if requested */
        if (sort_func)
            HDqsort(recv_buf, recv_buf_num_entries, (size_t)type_extent, sort_func);

        *out_array             = recv_buf;
        *out_array_num_entries = recv_buf_num_entries;
    }

done:
    if (recv_counts_disps_array)
        H5MM_free(recv_counts_disps_array);

    if (ret_value < 0) {
        if (recv_buf)
            H5MM_free(recv_buf);
    }

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5_mpio_gatherv_to_contig_array() */

/*-------------------------------------------------------------------------
 * Function:    H5_mpio_array_gatherv
 *
 * Purpose:     Given an array, specified in `local_array`, by each
 *              processor calling this function, collects each array into a
 *              single array. This new array is then either gathered to the
 *              processor specified by `root` when `allgather` is false, or
 *              is distributed back to all processors when `allgather` is
 *              true.
 *
 *              The number of entries in the array contributed by an
 *              individual processor and the size of each entry should be
 *              specified in `local_array_num_entries` and
 *              `array_entry_size`, respectively.
 *
 *              The MPI communicator to use should be specified for `comm`.
 *
 *              If the `sort_func` argument is supplied, the array is
 *              sorted before the function returns.
 *
 *              Upon successful completion, the new array is returned
 *              through `gathered_array` and the number of entries in this
 *              array is returned through `gathered_array_num_entries`.
 *
 * Notes:       This routine is collective across `comm`.
 *
 *              If `allgather` is specified as true, `root` is ignored.
 *
 *              `array_entry_size` is expected to be the same for all
 *              processors across `comm`, even if a processor has nothing
 *              to contribute to the gather operation.
 *
 *              Since data is sent as bytes and MPI has limits due to the
 *              use of int parameters, no more than INT_MAX bytes may be
 *              sent by any individual processor and no more than
 *              approximately 2 * INT_MAX bytes may be gathered in total.
 *              This restriction may need to be worked around in the
 *              future, but hopefully is sufficient for now.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5_mpio_array_gatherv(void *local_array, size_t local_array_num_entries, size_t array_entry_size,
                      void **gathered_array, size_t *gathered_array_num_entries, hbool_t allgather, int root,
                      MPI_Comm comm, H5_sort_func_cb_t sort_func)
{
    size_t _gathered_array_num_entries = 0;    /* The size of the newly-constructed array */
    void * _gathered_array             = NULL; /* The newly-constructed array returned to the caller */
    int *  recv_counts_disps_array =
        NULL; /* Array containing number of entries each processor is contributing
                 and displacements where each processor places its data in the final array */
    int    send_count = 0;
    int    mpi_code   = MPI_SUCCESS;
    int    mpi_rank   = 0;
    int    mpi_size   = 0;
    herr_t ret_value  = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    HDassert(gathered_array);
    HDassert(gathered_array_num_entries);
    HDassert(array_entry_size > 0);

    if (MPI_SUCCESS != (mpi_code = MPI_Comm_size(comm, &mpi_size)))
        HMPI_GOTO_ERROR(FAIL, "MPI_Comm_size failed", mpi_code)
    if (MPI_SUCCESS != (mpi_code = MPI_Comm_rank(comm, &mpi_rank)))
        HMPI_GOTO_ERROR(FAIL, "MPI_Comm_rank failed", mpi_code)

    /* Since the data is sent as bytes, calculate the true send_count for the processor-local data. */
    H5_CHECKED_ASSIGN(send_count, int, local_array_num_entries *array_entry_size, size_t);

    /* Allocate array to store the amounts of data being sent by each processor, as
     * well as the displacements into the final array where each processor will place
     * their data. The first half of the array contains the sent data amounts (in rank
     * order), while the latter half contains the displacements (also in rank order).
     */
    if (allgather || (mpi_rank == root)) {
        if (NULL ==
            (recv_counts_disps_array = H5MM_malloc(2 * (size_t)mpi_size * sizeof(recv_counts_disps_array)))) {
            if (!allgather) {
                /* Push an error, but still participate in collective gather operation */
                HDONE_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL,
                            "couldn't allocate receive counts and displacements array")
            }
            else
                HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL,
                            "couldn't allocate receive counts and displacements array")
        }
    }

    /* If gathering to all processors, inform each processor of how many bytes
     * each other processor is contributing to the resulting array by collecting
     * the counts into each processor's "receive counts" array. Otherwise, inform
     * only the root processor of how many bytes each other processor is
     * contributing.
     */
    if (allgather) {
        if (MPI_SUCCESS !=
            (mpi_code = MPI_Allgather(&send_count, 1, MPI_INT, recv_counts_disps_array, 1, MPI_INT, comm)))
            HMPI_GOTO_ERROR(FAIL, "MPI_Allgather failed", mpi_code)
    }
    else {
        if (MPI_SUCCESS !=
            (mpi_code = MPI_Gather(&send_count, 1, MPI_INT, recv_counts_disps_array, 1, MPI_INT, root, comm)))
            HMPI_GOTO_ERROR(FAIL, "MPI_Gather failed", mpi_code)
    }

    /* Calculate the total size of and allocate the final array, then
     * set the displacements into the array for the final gather operation.
     */
    if (allgather || (mpi_rank == root)) {
        size_t i, total_data_size;
        int *  displacements_ptr;

        for (i = 0, total_data_size = 0; i < (size_t)mpi_size; i++)
            total_data_size += (size_t)recv_counts_disps_array[i];

        /* Check if there is no work to do */
        if (total_data_size != 0) {
            if (NULL == (_gathered_array = H5MM_malloc(total_data_size))) {
                if (!allgather) {
                    /* Push an error, but still participate in collective gather operation */
                    HDONE_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL,
                                "couldn't allocate array for gather operation")
                }
                else
                    HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL,
                                "couldn't allocate array for gather operation")
            }
        }

        displacements_ptr = &recv_counts_disps_array[mpi_size];

        *displacements_ptr = 0;
        for (i = 1; i < (size_t)mpi_size; i++)
            displacements_ptr[i] = displacements_ptr[i - 1] + recv_counts_disps_array[i - 1];

        /* Set the "number of entries in array" value that is returned to the caller */
        _gathered_array_num_entries = total_data_size / array_entry_size;
    }

    /* Perform the actual gather operation */
    if (allgather) {
        if (MPI_SUCCESS != (mpi_code = MPI_Allgatherv(local_array, send_count, MPI_BYTE, _gathered_array,
                                                      recv_counts_disps_array,
                                                      &recv_counts_disps_array[mpi_size], MPI_BYTE, comm)))
            HMPI_GOTO_ERROR(FAIL, "MPI_Allgatherv failed", mpi_code)
    }
    else {
        if (MPI_SUCCESS != (mpi_code = MPI_Gatherv(local_array, send_count, MPI_BYTE, _gathered_array,
                                                   recv_counts_disps_array,
                                                   &recv_counts_disps_array[mpi_size], MPI_BYTE, root, comm)))
            HMPI_GOTO_ERROR(FAIL, "MPI_Gatherv failed", mpi_code)
    }

    /* Sort the gathered array, if requested */
    if (sort_func && (allgather || (mpi_rank == root)))
        HDqsort(_gathered_array, _gathered_array_num_entries, array_entry_size, sort_func);

    *gathered_array             = _gathered_array;
    *gathered_array_num_entries = _gathered_array_num_entries;

done:
    if (recv_counts_disps_array)
        H5MM_free(recv_counts_disps_array);

    if (ret_value < 0) {
        if (_gathered_array)
            H5MM_free(_gathered_array);
    }

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5_mpio_array_gatherv() */

#endif /* H5_HAVE_PARALLEL */
