/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
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
 * Programmer:  Richard Warren
 *              Wednesday, July 1, 2020
 *
 * Purpose:     This is part of a parallel subfiling I/O driver.
 *
 */

/***********/
/* Headers */
/***********/

#include "H5FDsubfiling_priv.h"

/*-------------------------------------------------------------------------
 * Function:    H5FD__subfiling__truncate_sub_files
 *
 *              Note: This code should be moved -- most likely to the IOC
 *                    code files.
 *
 * Purpose:     Apply a truncate operation to the sub-files.
 *
 *              In the context of the I/O concentrators, the eof must be
 *              translated into the appropriate value for each of the
 *              sub-files, and then applied to same.
 *
 *              Further, we must ensure that all prior I/O requests complete
 *              before the truncate is applied.
 *
 *              We do this as follows:
 *
 *              1) Run a barrier on entry.
 *
 *              2) Determine if this rank is a IOC.  If it is, compute
 *                 the correct EOF for this sub-file, and send a truncate
 *                 request to the IOC.
 *
 *              3) On the IOC thread, allow all pending I/O requests
 *                 received prior to the truncate request to complete
 *                 before performing the truncate.
 *
 *              4) Run a barrier on exit.
 *
 *              Observe that the barrier on entry ensures that any prior
 *              I/O requests will have been queue before the truncate
 *              request is sent to the IOC.
 *
 *              Similarly, the barrier on exit ensures that no subsequent
 *              I/O request will reach the IOC before the truncate request
 *              has been queued.
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  JRM -- 12/13/21
 *
 * Changes:     None.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5FD__subfiling__truncate_sub_files(int64_t logical_file_eof, hid_t context_id)
{
    int                  mpi_code;                   /* MPI return code */
    MPI_Comm             comm       = MPI_COMM_NULL; /* MPI Communicator, from plist */
    subfiling_context_t *sf_context = NULL;
    int64_t              msg[3]     = {
        0,
    };
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* for now, set comm to MPI_COMM_WORLD.  This is incorrect -- should use
     * the communicator supplied with the file open, or a copy thereof.
     */
    comm = MPI_COMM_WORLD;

    /* Barrier on entry */
    if (MPI_SUCCESS != (mpi_code = MPI_Barrier(comm)))
        HMPI_GOTO_ERROR(FAIL, "MPI_Barrier failed", mpi_code)

    if (NULL == (sf_context = (subfiling_context_t *)H5_get_subfiling_object(context_id)))
        HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, FAIL, "can't get subfile context")

    /* Test to see if this rank is running an I/O concentrator. */

    if (sf_context->topology->rank_is_ioc) {

        int     i;
        int64_t subfile_eof;
        int64_t num_full_stripes;
        int64_t partial_stripe_len;
#ifndef NDEBUG
        int64_t test_file_eof;
#endif /* NDEBUG */

        /* if it is, first compute the sub-file EOF */

        num_full_stripes   = logical_file_eof / sf_context->sf_blocksize_per_stripe;
        partial_stripe_len = logical_file_eof % sf_context->sf_blocksize_per_stripe;

        subfile_eof = num_full_stripes * sf_context->sf_stripe_size;

        if (sf_context->topology->subfile_rank < (partial_stripe_len / sf_context->sf_stripe_size)) {

            subfile_eof += sf_context->sf_stripe_size;
        }
        else if (sf_context->topology->subfile_rank == (partial_stripe_len / sf_context->sf_stripe_size)) {

            subfile_eof += partial_stripe_len % sf_context->sf_stripe_size;
        }

        /* sanity check -- compute the file eof using the same mechanism used to
         * compute the sub-file eof.  Assert that the computed value and the
         * actual value match.
         *
         * Do this only for debug builds -- probably delete this before release.
         *
         *                                           JRM -- 12/15/21
         */

#ifndef NDEBUG
        test_file_eof = 0;

        for (i = 0; i < sf_context->topology->n_io_concentrators; i++) {

            test_file_eof += num_full_stripes * sf_context->sf_stripe_size;

            if (i < (partial_stripe_len / sf_context->sf_stripe_size)) {

                test_file_eof += sf_context->sf_stripe_size;
            }
            else if (i == (partial_stripe_len / sf_context->sf_stripe_size)) {

                test_file_eof += partial_stripe_len % sf_context->sf_stripe_size;
            }
        }
        HDassert(test_file_eof == logical_file_eof);
#endif /* NDEBUG */

        /* then direct the IOC to truncate the sub-file to the correct EOF */

        msg[0] = subfile_eof;
        msg[1] = 0; /* padding -- not used in this message */
        msg[2] = context_id;

        if (MPI_SUCCESS != (mpi_code = MPI_Send(msg, 3, MPI_INT64_T, sf_context->topology->subfile_rank,
                                                TRUNC_OP, sf_context->sf_msg_comm)))
            HMPI_GOTO_ERROR(FAIL, "MPI_Send failed", mpi_code)
    }

    /* Barrier on exit */
    if (MPI_SUCCESS != (mpi_code = MPI_Barrier(comm)))
        HMPI_GOTO_ERROR(FAIL, "MPI_Barrier failed", mpi_code)

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5FD__subfiling__truncate_sub_files() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__subfiling__get_real_eof
 *
 *              Note: This code should be moved -- most likely to the IOC
 *                    code files.
 *
 * Purpose:     Query each subfile to get its local EOF, and then use this
 *              data to calculate the actual EOF.
 *
 *              Do this as follows:
 *
 *              1) allocate an array of int64_t of length equal to the
 *                 the number of IOCs, and initialize all fields to -1.
 *
 *              2) Send each IOC a message requesting that sub-file's EOF.
 *
 *              3) Await reply from each IOC, storing the reply in
 *                 the appropriate entry in the array allocated in 1.
 *
 *              4) After all IOCs have replied, compute the offset of
 *                 each subfile in the logical file.  Take the maximum
 *                 of these values, and report this value as the overall
 *                 EOF.
 *
 *              Note that this operation is not collective, and can return
 *              invalid data if other ranks perform writes while this
 *              operation is in progress.
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  JRM -- 1/18/22
 *
 * Changes:     None.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5FD__subfiling__get_real_eof(hid_t context_id, int64_t *logical_eof_ptr)
{
    subfiling_context_t *sf_context = NULL;
    MPI_Status           status;
    int64_t *            sf_eofs     = NULL; /* dynamically allocated array for subfile EOFs */
    int64_t              msg[3]      = {0, 0, 0};
    int64_t              logical_eof = 0;
    int64_t              sf_logical_eof;
    int                  i;
    int                  reply_count;
    int                  ioc_rank;
    int                  mpi_code;            /* MPI return code */
    int                  n_io_concentrators;  /* copy of value in topology */
    herr_t               ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(logical_eof_ptr);

    if (NULL == (sf_context = (subfiling_context_t *)H5_get_subfiling_object(context_id)))
        HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, FAIL, "can't get subfile context")

    HDassert(sf_context->topology);

    n_io_concentrators = sf_context->topology->n_io_concentrators;

    HDassert(n_io_concentrators > 0);

    /* 1) allocate an array of int64_t of length equal to the
     *    the number of IOCs, and initialize all fields to -1.
     */
    sf_eofs = (int64_t *)HDmalloc((size_t)n_io_concentrators * sizeof(int64_t));

    if (sf_eofs == NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate sub-file EOFs array.");

    for (i = 0; i < n_io_concentrators; i++) {

        sf_eofs[i] = -1;
    }

    /* 2) Send each IOC an asynchronous message requesting that
     *    sub-file's EOF.
     */
    msg[0] = 0; /* padding -- not used in this message */
    msg[1] = 0; /* padding -- not used in this message */
    msg[2] = context_id;

    for (i = 0; i < n_io_concentrators; i++) {

        ioc_rank = sf_context->topology->io_concentrators[i];

        if (MPI_SUCCESS !=
            (mpi_code = MPI_Send(msg, 3, MPI_INT64_T, ioc_rank, GET_EOF_OP, sf_context->sf_msg_comm)))
            HMPI_GOTO_ERROR(FAIL, "MPI_Send", mpi_code)
    }

    /* 3) Await reply from each IOC, storing the reply in
     *    the appropriate entry in sf_eofs.
     */
    reply_count = 0;
    while (reply_count < n_io_concentrators) {

        if (MPI_SUCCESS != (mpi_code = MPI_Recv(msg, 3, MPI_INT64_T, MPI_ANY_SOURCE, GET_EOF_COMPLETED,
                                                sf_context->sf_data_comm, &status))) {

            HMPI_GOTO_ERROR(FAIL, "MPI_Recv", mpi_code)
        }

        ioc_rank = (int)msg[0];

        HDassert(ioc_rank >= 0);
        HDassert(ioc_rank < n_io_concentrators);
        HDassert(sf_eofs[ioc_rank] == -1);

        sf_eofs[ioc_rank] = msg[1];

        reply_count++;
    }

    /* 4) After all IOCs have replied, compute the offset of
     *    each subfile in the logical file.  Take the maximum
     *    of these values, and report this value as the overall
     *    EOF.
     */

    for (i = 0; i < n_io_concentrators; i++) {

        /* compute number of complete stripes */
        sf_logical_eof = sf_eofs[i] / sf_context->sf_stripe_size;

        /* multiply by stripe size */
        sf_logical_eof *= sf_context->sf_stripe_size * n_io_concentrators;

        /* if the sub-file doesn't end on a stripe size boundary, must add in a partial stripe */
        if (sf_eofs[i] % sf_context->sf_stripe_size > 0) {

            /* add in the size of the partial stripe up to but not including this subfile */
            sf_logical_eof += i * sf_context->sf_stripe_size;

            /* finally, add in the number of bytes in the last partial stripe depth in the sub-file */
            sf_logical_eof += sf_eofs[i] % sf_context->sf_stripe_size;
        }

        if (sf_logical_eof > logical_eof) {

            logical_eof = sf_logical_eof;
        }
    }

    *logical_eof_ptr = logical_eof;

done:
    HDfree(sf_eofs);

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FD__subfiling__get_real_eof() */
