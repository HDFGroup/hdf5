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
 * Purpose: This is part of an I/O concentrator driver.
 */

#include "H5FDioc_priv.h"

static int async_completion(void *arg);

/*
 * Given a file offset, the stripe size and
 * the number of IOCs, calculate the target
 * IOC for I/O and the file offset for the
 * subfile that IOC controls
 */
static inline void
calculate_target_ioc(int64_t file_offset, int64_t stripe_size, int n_io_concentrators, int64_t *target_ioc,
                     int64_t *ioc_file_offset)
{
    int64_t stripe_idx;
    int64_t subfile_row;

    HDassert(target_ioc);
    HDassert(ioc_file_offset);
    HDassert(stripe_size > 0);
    HDassert(n_io_concentrators > 0);

    stripe_idx  = file_offset / stripe_size;
    subfile_row = stripe_idx / n_io_concentrators;

    *target_ioc      = stripe_idx % n_io_concentrators;
    *ioc_file_offset = (subfile_row * stripe_size) + (file_offset % stripe_size);
}

/*
 * Utility routine to hack around casting away const
 */
static inline void *
cast_to_void(const void *data)
{
    union {
        const void *const_ptr_to_data;
        void *      ptr_to_data;
    } eliminate_const_warning;
    eliminate_const_warning.const_ptr_to_data = data;
    return eliminate_const_warning.ptr_to_data;
}

/*-------------------------------------------------------------------------
 * Function:    ioc__write_independent_async
 *
 * Purpose:     TODO: revise. The IO operations can be striped across a selection of
 *              IO concentrators.  The read and write independent calls
 *              compute the group of 1 or more IOCs and further create
 *              derived MPI datatypes when required by the size of the
 *              contiguous read or write requests.
 *
 *              IOC(0) contains the logical data storage for file offset
 *              zero and all offsets that reside within modulo range of
 *              the subfiling stripe_size.
 *
 *              We cycle through all 'n_io_conentrators' and send a
 *              descriptor to each IOC that has a non-zero sized IO
 *              request to fulfill.
 *
 *              Sending descriptors to an IOC usually gets an ACK or
 *              NACK in response.  For the write operations, we post
 *              asynch READs to receive ACKs from IOC ranks that have
 *              allocated memory receive the data to write to the
 *              subfile.  Upon receiving an ACK, we send the actual
 *              user data to the IOC.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Richard Warren
 *              7/17/2020
 *
 * Changes:     Initial Version/None.
 *-------------------------------------------------------------------------
 */
herr_t
ioc__write_independent_async(int64_t context_id, int n_io_concentrators, int64_t offset, int64_t elements,
                             const void *data, io_req_t **io_req)
{
    subfiling_context_t *sf_context    = NULL;
    MPI_Request          ack_request   = MPI_REQUEST_NULL;
    io_req_t *           sf_io_request = NULL;
    int64_t              ioc_start;
    int64_t              ioc_offset;
    int64_t              msg[3]           = {0};
    int *                io_concentrators = NULL;
    int                  data_tag         = 0;
    int                  mpi_code;
    herr_t               ret_value = SUCCEED;

    HDassert(io_req);

    if (NULL == (sf_context = H5_get_subfiling_object(context_id)))
        H5FD_IOC_GOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "can't get subfiling context from ID");
    HDassert(sf_context->topology);
    HDassert(sf_context->topology->io_concentrators);

    io_concentrators = sf_context->topology->io_concentrators;

    /*
     * Calculate the IOC that we'll send the I/O request to
     * and the offset within that IOC's subfile
     */
    calculate_target_ioc(offset, sf_context->sf_stripe_size, n_io_concentrators, &ioc_start, &ioc_offset);

    /*
     * Wait for memory to be allocated on the target IOC before
     * beginning send of user data. Once that memory has been
     * allocated, we will receive an ACK (or NACK) message from
     * the IOC to allow us to proceed.
     *
     * On ACK, the IOC will send the tag to be used for sending
     * data. This allows us to distinguish between multiple
     * concurrent writes from a single rank.
     *
     * Post an early non-blocking receive for the MPI tag here.
     */
    if (MPI_SUCCESS != (mpi_code = MPI_Irecv(&data_tag, 1, MPI_INT, io_concentrators[ioc_start],
                                             WRITE_INDEP_ACK, sf_context->sf_data_comm, &ack_request)))
        H5FD_IOC_MPI_GOTO_ERROR(FAIL, "MPI_Irecv failed", mpi_code);

    /*
     * Prepare and send an I/O request to the IOC identified
     * by the file offset
     */
    msg[0] = elements;
    msg[1] = ioc_offset;
    msg[2] = context_id;
    if (MPI_SUCCESS != (mpi_code = MPI_Send(msg, 3, MPI_INT64_T, io_concentrators[ioc_start], WRITE_INDEP,
                                            sf_context->sf_msg_comm)))
        H5FD_IOC_MPI_GOTO_ERROR(FAIL, "MPI_Send failed", mpi_code);

    /* Wait to receive data tag */
    if (MPI_SUCCESS != (mpi_code = MPI_Wait(&ack_request, MPI_STATUS_IGNORE)))
        H5FD_IOC_MPI_GOTO_ERROR(FAIL, "MPI_Wait failed", mpi_code);

    if (data_tag == 0)
        H5FD_IOC_GOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "received NACK from IOC");

    /* At this point in the new implementation, we should queue
     * the async write so that when the top level VFD tells us
     * to complete all pending IO requests, we have all the info
     * we need to accomplish that.
     */
    if (NULL == (sf_io_request = HDmalloc(sizeof(io_req_t))))
        H5FD_IOC_GOTO_ERROR(H5E_RESOURCE, H5E_WRITEERROR, FAIL, "couldn't allocate I/O request");

    H5_CHECK_OVERFLOW(ioc_start, int64_t, int);
    sf_io_request->completion_func.io_args.ioc        = (int)ioc_start;
    sf_io_request->completion_func.io_args.context_id = context_id;
    sf_io_request->completion_func.io_args.offset     = offset;
    sf_io_request->completion_func.io_args.elements   = elements;
    sf_io_request->completion_func.io_args.data       = cast_to_void(data);
    sf_io_request->completion_func.io_args.io_req     = MPI_REQUEST_NULL;
    sf_io_request->completion_func.io_function        = async_completion;
    sf_io_request->completion_func.pending            = 0;

    sf_io_request->prev = sf_io_request->next = NULL;

    /*
     * Start the actual data transfer using the ack received
     * from the IOC as the tag for the send
     */
    H5_CHECK_OVERFLOW(elements, int64_t, int);
    if (MPI_SUCCESS !=
        (mpi_code = MPI_Isend(data, (int)elements, MPI_BYTE, io_concentrators[ioc_start], data_tag,
                              sf_context->sf_data_comm, &sf_io_request->completion_func.io_args.io_req)))
        H5FD_IOC_MPI_GOTO_ERROR(FAIL, "MPI_Isend failed", mpi_code);

    /*
     * NOTE: When we actually have the async I/O support,
     * the request should be queued before we return to
     * the caller. Having queued the I/O operation, we
     * might want to get additional work started before
     * allowing the queued I/O requests to make further
     * progress and/or to complete, so we just return
     * to the caller.
     */

    sf_io_request->completion_func.pending = 1;
    *io_req                                = sf_io_request;

done:
    if (ret_value < 0) {
        if (ack_request != MPI_REQUEST_NULL) {
            if (MPI_SUCCESS != (mpi_code = MPI_Cancel(&ack_request)))
                H5FD_IOC_MPI_DONE_ERROR(FAIL, "MPI_Cancel failed", mpi_code);
        }

        HDfree(sf_io_request);
        *io_req = NULL;
    }

    H5FD_IOC_FUNC_LEAVE;
} /* end ioc__write_independent_async() */

/*-------------------------------------------------------------------------
 * Function:    Internal ioc__read_independent_async
 *
 * Purpose:     TODO: revise. The IO operations can be striped across a selection of
 *              IO concentrators.  The read and write independent calls
 *              compute the group of 1 or more IOCs and further create
 *              derived MPI datatypes when required by the size of the
 *              contiguous read or write requests.
 *
 *              IOC(0) contains the logical data storage for file offset
 *              zero and all offsets that reside within modulo range of
 *              the subfiling stripe_size.
 *
 *              We cycle through all 'n_io_conentrators' and send a
 *              descriptor to each IOC that has a non-zero sized IO
 *              request to fulfill.
 *
 *              Sending descriptors to an IOC usually gets an ACK or
 *              NACK in response.  For the read operations, we post
 *              asynch READs to receive the file data and wait until
 *              all pending operations have completed.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Richard Warren
 *              7/17/2020
 *
 * Changes:     Initial Version/None.
 *-------------------------------------------------------------------------
 */
herr_t
ioc__read_independent_async(int64_t context_id, int n_io_concentrators, int64_t offset, int64_t elements,
                            void *data, io_req_t **io_req)
{
    subfiling_context_t *sf_context    = NULL;
    io_req_t *           sf_io_request = NULL;
    int64_t              ioc_start;
    int64_t              ioc_offset;
    int64_t              msg[3]           = {0};
    int *                io_concentrators = NULL;
    int                  mpi_code;
    herr_t               ret_value = SUCCEED;

    HDassert(io_req);

    if (NULL == (sf_context = H5_get_subfiling_object(context_id)))
        H5FD_IOC_GOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, "can't get subfiling context from ID");
    HDassert(sf_context->topology);
    HDassert(sf_context->topology->io_concentrators);

    io_concentrators = sf_context->topology->io_concentrators;

    /*
     * Calculate the IOC that we'll send the I/O request to
     * and the offset within that IOC's subfile
     */
    calculate_target_ioc(offset, sf_context->sf_stripe_size, n_io_concentrators, &ioc_start, &ioc_offset);

    /*
     * At this point in the new implementation, we should queue
     * the non-blocking recv so that when the top level VFD tells
     * us to complete all pending IO requests, we have all the info
     * we need to accomplish that.
     *
     * Post the early non-blocking receive here.
     */
    if (NULL == (sf_io_request = HDmalloc(sizeof(io_req_t))))
        H5FD_IOC_GOTO_ERROR(H5E_RESOURCE, H5E_READERROR, FAIL, "couldn't allocate I/O request");

    H5_CHECK_OVERFLOW(ioc_start, int64_t, int);
    sf_io_request->completion_func.io_args.ioc        = (int)ioc_start;
    sf_io_request->completion_func.io_args.context_id = context_id;
    sf_io_request->completion_func.io_args.offset     = offset;
    sf_io_request->completion_func.io_args.elements   = elements;
    sf_io_request->completion_func.io_args.data       = data;
    sf_io_request->completion_func.io_args.io_req     = MPI_REQUEST_NULL;
    sf_io_request->completion_func.io_function        = async_completion;
    sf_io_request->completion_func.pending            = 0;

    sf_io_request->prev = sf_io_request->next = NULL;

    H5_CHECK_OVERFLOW(elements, int64_t, int);
    if (MPI_SUCCESS !=
        (mpi_code = MPI_Irecv(data, (int)elements, MPI_BYTE, io_concentrators[ioc_start], READ_INDEP_DATA,
                              sf_context->sf_data_comm, &sf_io_request->completion_func.io_args.io_req)))
        H5FD_IOC_MPI_GOTO_ERROR(FAIL, "MPI_Irecv failed", mpi_code);

    sf_io_request->completion_func.pending = 1;
    *io_req                                = sf_io_request;

    /*
     * Prepare and send an I/O request to the IOC identified
     * by the file offset
     */
    msg[0] = elements;
    msg[1] = ioc_offset;
    msg[2] = context_id;
    if (MPI_SUCCESS != (mpi_code = MPI_Send(msg, 3, MPI_INT64_T, io_concentrators[ioc_start], READ_INDEP,
                                            sf_context->sf_msg_comm)))
        H5FD_IOC_MPI_GOTO_ERROR(FAIL, "MPI_Send failed", mpi_code);

done:
    if (ret_value < 0) {
        if (sf_io_request && sf_io_request->completion_func.io_args.io_req != MPI_REQUEST_NULL) {
            if (MPI_SUCCESS != (mpi_code = MPI_Cancel(&sf_io_request->completion_func.io_args.io_req)))
                H5FD_IOC_MPI_DONE_ERROR(FAIL, "MPI_Cancel failed", mpi_code);
        }

        HDfree(sf_io_request);
        *io_req = NULL;
    }

    H5FD_IOC_FUNC_LEAVE;
} /* end ioc__read_independent_async() */

/*-------------------------------------------------------------------------
 * Function:    async_completion
 *
 * Purpose:     Given a single io_func_t structure containing the function
 *              pointer and it's input arguments and a single MPI_Request
 *              argument which needs to be completed, we make progress
 *              by calling MPI_Test.  In this initial example, we loop
 *              until the request is completed as indicated by a non-zero
 *              flag variable.
 *
 *              As we go further with the implementation, we anticipate that
 *              rather than testing a single request variable, we will
 *              deal with a collection of all pending IO requests (on
 *              this rank).
 *
 * Return:      an integer status.  Zero(0) indicates success. Negative
 *              values (-1) indicates an error.
 *-------------------------------------------------------------------------
 */
static int
async_completion(void *arg)
{
    useconds_t delay = 5;
    int        n_reqs;
    int        n_waiting;
    int *      indices   = NULL;
    int        ret_value = 0;
    struct async_arg {
        int          n_reqs;
        MPI_Request *sf_reqs;
    } *in_progress = (struct async_arg *)arg;

    HDassert(arg);

    n_reqs    = in_progress->n_reqs;
    n_waiting = n_reqs;

    if (n_reqs < 0) {
#ifdef H5FD_IOC_DEBUG
        HDprintf("%s: invalid number of in progress I/O requests\n", __func__);
#endif

        ret_value = -1;
        goto done;
    }

    if (NULL == (indices = HDmalloc((size_t)n_reqs * sizeof(*indices)))) {
#ifdef H5FD_IOC_DEBUG
        HDprintf("%s: couldn't allocate MPI request array\n", __func__);
#endif

        ret_value = -1;
        goto done;
    }

    while (n_waiting) {
        int ready = 0;
        int mpi_code;

        if (MPI_SUCCESS !=
            (mpi_code = MPI_Testsome(n_reqs, in_progress->sf_reqs, &ready, indices, MPI_STATUSES_IGNORE))) {
#ifdef H5FD_IOC_DEBUG
            HDprintf("%s: MPI_Testsome failed with rc %d\n", __func__, mpi_code);
#endif

            ret_value = -1;
            goto done;
        }

        if (ready == 0) {
            usleep(delay);
        }

        for (int i = 0; i < ready; i++) {
            n_waiting--;
        }
    }

done:
    HDfree(indices);

    H5FD_IOC_FUNC_LEAVE;
}
