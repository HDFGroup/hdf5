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
H5FD__subfiling__truncate_sub_files(hid_t context_id, int64_t logical_file_eof, MPI_Comm comm)
{
    int                  mpi_size;
    int                  mpi_code; /* MPI return code */
    subfiling_context_t *sf_context = NULL;
    int64_t              msg[3]     = {
        0,
    };
    herr_t ret_value = SUCCEED; /* Return value */

    if (MPI_SUCCESS != (mpi_code = MPI_Comm_size(comm, &mpi_size)))
        H5_SUBFILING_MPI_GOTO_ERROR(FAIL, "MPI_Comm_size failed", mpi_code);

    /* Barrier on entry */
    if (mpi_size > 1)
        if (MPI_SUCCESS != (mpi_code = MPI_Barrier(comm)))
            H5_SUBFILING_MPI_GOTO_ERROR(FAIL, "MPI_Barrier failed", mpi_code);

    if (NULL == (sf_context = (subfiling_context_t *)H5_get_subfiling_object(context_id)))
        H5_SUBFILING_GOTO_ERROR(H5E_FILE, H5E_BADVALUE, FAIL, "can't get subfile context");

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

        if (MPI_SUCCESS !=
            (mpi_code = MPI_Send(msg, 3, MPI_INT64_T,
                                 sf_context->topology->io_concentrators[sf_context->topology->subfile_rank],
                                 TRUNC_OP, sf_context->sf_msg_comm)))
            H5_SUBFILING_MPI_GOTO_ERROR(FAIL, "MPI_Send failed", mpi_code);
    }

    /* Barrier on exit */
    if (mpi_size > 1)
        if (MPI_SUCCESS != (mpi_code = MPI_Barrier(comm)))
            H5_SUBFILING_MPI_GOTO_ERROR(FAIL, "MPI_Barrier failed", mpi_code);

done:

    H5_SUBFILING_FUNC_LEAVE;
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
 *              SUBFILING NOTE:
 *              The EOF calculation for subfiling is somewhat different
 *              than for the more traditional HDF5 file implementations.
 *              This statement derives from the fact that unlike "normal"
 *              HDF5 files, subfiling introduces a multi-file representation
 *              of a single HDF5 file.  The plurality of sub-files represents
 *              a software RAID-0 based HDF5 file.  As such, each sub-file
 *              contains a designated portion of the address space of the
 *              virtual HDF5 storage.  We have no notion of HDF5 datatypes,
 *              datasets, metadata, or other HDF5 structures; only BYTES.
 *
 *              The organization of the bytes within sub-files is consistent
 *              with the RAID-0 striping, i.e. there are IO Concentrators
 *              (IOCs) which correspond to a stripe-count (in Lustre) as
 *              well as a stripe_size.  The combination of these two
 *              variables determines the "address" (a combination of IOC
 *              and a file offset) of any storage operation.
 *
 *              Having a defined storage layout, the virtual file EOF
 *              calculation should be the MAXIMUM value returned by the
 *              collection of IOCs.  Every MPI rank which hosts an IOC
 *              maintains its own EOF by updating that value for each
 *              WRITE operation that completes, i.e. if a new local EOF
 *              is greater than the existing local EOF, the new EOF
 *              will replace the old.  The local EOF calculation is as
 *              follows.
 *              1. At file creation, each IOC is assigned a rank value
 *                 (0 to N-1, where N is the total number of IOCs) and
 *                 a 'sf_base_addr' = 'subfile_rank' * 'sf_stripe_size')
 *                 we also determine the 'sf_blocksize_per_stripe' which
 *                 is simply the 'sf_stripe_size' * 'n_ioc_concentrators'
 *
 *              2. For every write operation, the IOC receives a message
 *                 containing a file_offset and the data_size.
 *
 *              3. The file_offset + data_size are in turn used to
 *                 create a stripe_id:
 *                   IOC-(ioc_rank)       IOC-(ioc_rank+1)
 *                   |<- sf_base_address  |<- sf_base_address  |
 *                ID +--------------------+--------------------+
 *                 0:|<- sf_stripe_size ->|<- sf_stripe_size ->|
 *                 1:|<- sf_stripe_size ->|<- sf_stripe_size ->|
 *                   ~                    ~                    ~
 *                 N:|<- sf_stripe_size ->|<- sf_stripe_size ->|
 *                   +--------------------+--------------------+
 *
 *                The new 'stripe_id' is then used to calculate a
 *                potential new EOF:
 *                sf_eof = (stripe_id * sf_blocksize_per_stripe) + sf_base_addr
 *                         + ((file_offset + data_size) % sf_stripe_size)
 *
 *              4. If (sf_eof > current_sf_eof), then current_sf_eof = sf_eof.
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
    subfiling_context_t *sf_context  = NULL;
    MPI_Request         *recv_reqs   = NULL;
    int64_t             *recv_msg    = NULL;
    int64_t             *sf_eofs     = NULL; /* dynamically allocated array for subfile EOFs */
    int64_t              msg[3]      = {0, 0, 0};
    int64_t              logical_eof = 0;
    int64_t              sf_logical_eof;
    int                  n_io_concentrators = 0; /* copy of value in topology */
    int                  mpi_code;               /* MPI return code */
    herr_t               ret_value = SUCCEED;    /* Return value */

    HDassert(logical_eof_ptr);

    if (NULL == (sf_context = (subfiling_context_t *)H5_get_subfiling_object(context_id)))
        H5_SUBFILING_GOTO_ERROR(H5E_FILE, H5E_BADVALUE, FAIL, "can't get subfile context");

    HDassert(sf_context->topology);

    n_io_concentrators = sf_context->topology->n_io_concentrators;

    HDassert(n_io_concentrators > 0);

    if (NULL == (sf_eofs = HDmalloc((size_t)n_io_concentrators * sizeof(int64_t))))
        H5_SUBFILING_GOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "can't allocate sub-file EOFs array");
    if (NULL == (recv_reqs = HDmalloc((size_t)n_io_concentrators * sizeof(*recv_reqs))))
        H5_SUBFILING_GOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "can't allocate receive requests array");
    if (NULL == (recv_msg = HDmalloc((size_t)n_io_concentrators * 3 * sizeof(*recv_msg))))
        H5_SUBFILING_GOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "can't allocate message array");

    for (int i = 0; i < n_io_concentrators; i++) {
        sf_eofs[i]   = -1;
        recv_reqs[i] = MPI_REQUEST_NULL;
    }

    /* Post early non-blocking receives for replies from each IOC */
    for (int i = 0; i < n_io_concentrators; i++) {
        int ioc_rank = sf_context->topology->io_concentrators[i];

        if (MPI_SUCCESS != (mpi_code = MPI_Irecv(&recv_msg[3 * i], 3, MPI_INT64_T, ioc_rank,
                                                 GET_EOF_COMPLETED, sf_context->sf_eof_comm, &recv_reqs[i])))
            H5_SUBFILING_MPI_GOTO_ERROR(FAIL, "MPI_Irecv", mpi_code);
    }

    /* Send each IOC a message requesting that subfile's EOF */

    msg[0] = 0; /* padding -- not used in this message */
    msg[1] = 0; /* padding -- not used in this message */
    msg[2] = context_id;

    for (int i = 0; i < n_io_concentrators; i++) {
        int ioc_rank = sf_context->topology->io_concentrators[i];

        if (MPI_SUCCESS !=
            (mpi_code = MPI_Send(msg, 3, MPI_INT64_T, ioc_rank, GET_EOF_OP, sf_context->sf_msg_comm)))
            H5_SUBFILING_MPI_GOTO_ERROR(FAIL, "MPI_Send", mpi_code);
    }

    /* Wait for EOF communication to complete */
    if (MPI_SUCCESS != (mpi_code = MPI_Waitall(n_io_concentrators, recv_reqs, MPI_STATUSES_IGNORE)))
        H5_SUBFILING_MPI_GOTO_ERROR(FAIL, "MPI_Waitall", mpi_code);

    for (int i = 0; i < n_io_concentrators; i++) {
        int ioc_rank = (int)recv_msg[3 * i];

        HDassert(ioc_rank >= 0);
        HDassert(ioc_rank < n_io_concentrators);
        HDassert(sf_eofs[ioc_rank] == -1);

        sf_eofs[ioc_rank] = recv_msg[(3 * i) + 1];
    }

    /* 4) After all IOCs have replied, compute the offset of
     *    each subfile in the logical file.  Take the maximum
     *    of these values, and report this value as the overall
     *    EOF.
     */

    for (int i = 0; i < n_io_concentrators; i++) {

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

#ifdef H5_SUBFILING_DEBUG
    H5_subfiling_log(context_id, "%s: calculated logical EOF = %" PRId64 ".", __func__, logical_eof);
#endif

    *logical_eof_ptr = logical_eof;

done:
    if (ret_value < 0) {
        for (int i = 0; i < n_io_concentrators; i++) {
            if (recv_reqs && (recv_reqs[i] != MPI_REQUEST_NULL)) {
                if (MPI_SUCCESS != (mpi_code = MPI_Cancel(&recv_reqs[i])))
                    H5_SUBFILING_MPI_DONE_ERROR(FAIL, "MPI_Cancel", mpi_code);
            }
        }
    }

    HDfree(recv_msg);
    HDfree(recv_reqs);
    HDfree(sf_eofs);

    H5_SUBFILING_FUNC_LEAVE;
} /* H5FD__subfiling__get_real_eof() */
