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

#define MIN_RETRIES 10

#define WORLD_SIZE(ctx) ((ctx)->topology->app_layout->world_size)
#define WORLD_RANK(ctx) ((ctx)->topology->app_layout->world_size)

io_req_t pending_io_requests;

static int sf_write_ops = 0;
static double sf_pwrite_time     = 0.0;
static double sf_write_wait_time = 0.0;
static int    sf_read_ops         = 0;
static double sf_pread_time       = 0.0;
static double sf_read_wait_time   = 0.0;
static double sf_queue_delay_time = 0.0;

atomic_int sf_file_close_count  = 0;
atomic_int sf_file_refcount     = 0;
atomic_int sf_ioc_fini_refcount = 0;
atomic_int sf_workinprogress    = 0;
atomic_int sf_work_pending      = 0;
#if 1 /* JRM */
/* sf_io_ops_pending is use to track the number of I/O operations pending so that we can wait
 * until all I/O operations have been serviced before shutting down the worker thread pool.
 * The value of this variable must always be non-negative.
 */
atomic_int sf_io_ops_pending = 0;
#endif /* JRM */

static int async_completion(void *arg);

#if 0 /* JRM */ /* original version of ioc_main() */
/*-------------------------------------------------------------------------
 * Function:    Public/IOC ioc_main
 *
 * Purpose:     This is the principal function run by the IO Concentrator
 *              main thread.  It remains within a loop until allowed to
 *              exit by means of setting the 'sf_shutdown_flag'.   This
 *              usually accomplished as part of the file close operation.
 *
 *              The function implements an asynchronous polling approach
 *              for incoming messages. These messages can be thought of
 *              as a primitive RPC which utilizes MPI TAGs to code and
 *              implement the desired subfiling functionality.
 *
 *              As each incoming message is received, it get added to
 *              a queue for processing by a thread_pool thread.
 *              The message handlers are dispatched via the
 *              "handle_work_request" ftn (see H5FDsubfile_thread.c)

 *              Subfiling is effectively a software RAID-0 implementation
 *              where having multiple IO Concentrators and independent
 *              subfiles is equated to the multiple disks and a true
 *              hardware base RAID implementation.
 *
 *              IO Concentrators are ordered according to their MPI rank.
 *              In the simplest interpretation, IOC(0) will always contain
 *              the initial bytes of the logical disk image.  Byte 0 of
 *              IOC(1) will contain the byte written to the logical disk
 *              offset "stripe_size" X IOC(number).
 *
 *              Example: If the stripe size is defined to be 256K, then
 *              byte 0 of subfile(1) is at logical offset 262144 of the
 *              file.   Similarly, byte 0 of subfile(2) represents the
 *              logical file offset = 524288.   For logical files larger
 *              than 'N' X stripe_size, we simply "wrap around" back to
 *              subfile(0).  The following shows the mapping of 30
 *              logical blocks of data over 3 subfiles:
 *              +--------+--------+--------+--------+--------+--------+
 *              | blk(0 )| blk(1) | blk(2 )| blk(3 )| blk(4 )| blk(5 )|
 *              | IOC(0) | IOC(1) | IOC(2) | IOC(0) | IOC(1) | IOC(2) |
 *              +--------+--------+--------+--------+--------+--------+
 *              | blk(6 )| blk(7) | blk(8 )| blk(9 )| blk(10)| blk(11)|
 *              | IOC(0) | IOC(1) | IOC(2) | IOC(0) | IOC(1) | IOC(2) |
 *              +--------+--------+--------+--------+--------+--------+
 *              | blk(12)| blk(13)| blk(14)| blk(15)| blk(16)| blk(17)|
 *              | IOC(0) | IOC(1) | IOC(2) | IOC(0) | IOC(1) | IOC(2) |
 *              +--------+--------+--------+--------+--------+--------+
 *              | blk(18)| blk(19)| blk(20)| blk(21)| blk(22)| blk(23)|
 *              | IOC(0) | IOC(1) | IOC(2) | IOC(0) | IOC(1) | IOC(2) |
 *              +--------+--------+--------+--------+--------+--------+
 *              | blk(24)| blk(25)| blk(26)| blk(27)| blk(28)| blk(29)|
 *              | IOC(0) | IOC(1) | IOC(2) | IOC(0) | IOC(1) | IOC(2) |
 *              +--------+--------+--------+--------+--------+--------+
 *
 * Return:      None
 * Errors:      None
 *
 * Programmer:  Richard Warren
 *              7/17/2020
 *
 * Changes:     Initial Version/None.
 *-------------------------------------------------------------------------
 */
int
ioc_main(int64_t context_id)
{
    int                  subfile_rank;
    int                  flag, ret;
    int                  max_work_depth;
    int                  shutdown_requested;
    MPI_Status           status, msg_status;
    sf_work_request_t *  incoming_requests = NULL;
    useconds_t           delay             = 20;
    subfiling_context_t *context           = get__subfiling_object(context_id);
    double               queue_start_time;

    assert(context != NULL);
    /* We can't have opened any files at this point..
     * The file open approach has changed so that the normal
     * application rank (hosting this thread) does the file open.
     * We can simply utilize the file descriptor (which should now
     * represent an open file).
     */

    subfile_rank = context->sf_group_rank;

    if (request_count_per_rank == NULL) {
        request_count_per_rank = (int *)calloc((size_t)WORLD_SIZE(context), sizeof(int));
        assert(request_count_per_rank != NULL);
    }

    max_work_depth    = MAX(8, WORLD_SIZE(context) * MAX_WORK_PER_RANK);
    incoming_requests = (sf_work_request_t *)calloc((size_t)(max_work_depth + 1), sizeof(sf_work_request_t));

    /* Validate that the allocation succeeded */
    assert(incoming_requests != NULL);

    /* Initialize atomic vars */
    atomic_init(&sf_workinprogress, 0);
    atomic_init(&sf_work_pending, 0);
    atomic_init(&sf_file_close_count, 0);
    atomic_init(&sf_file_refcount, 0);
    atomic_init(&sf_ioc_fini_refcount, 0);
    atomic_init(&sf_shutdown_flag, 0);
    atomic_init(&sf_ioc_ready, 1);
#if 1           /* JRM */
    /* this variable is incremented by tpool_add_work(), and decremented when the
     * received I/O request is completed.
     *
     * On shutdown, we must wait until this field is decremented to zero before
     * taking down the thread pool.
     */
    atomic_init(&sf_io_ops_pending, 0);
#endif          /* JRM */
    shutdown_requested = 0;

#if 0  /* JRM */
    while (!shutdown_requested || sf_work_pending) {
#else  /* JRM */
    while ( ( ! shutdown_requested ) || ( 0 < atomic_load(&sf_io_ops_pending) ) || sf_work_pending) {
#endif /* JRM */
        flag = 0;
        ret  = MPI_Iprobe(MPI_ANY_SOURCE, MPI_ANY_TAG, context->sf_msg_comm, &flag, &status);
        if ((ret == MPI_SUCCESS) && (flag != 0)) {
            sf_work_request_t *msg = NULL;
            int                count;
            int                index        = 0;
            int                request_size = (int)sizeof(sf_work_request_t);
            int                source       = status.MPI_SOURCE;
            int                tag          = status.MPI_TAG;

#if 1  /* JRM */
            if ( ( tag != READ_INDEP ) && ( tag != WRITE_INDEP ) ) {

                HDprintf("\n\nioc_main: received non READ_INDEP / WRITE_INDEP mssg. tag = %d.\n\n", tag);
                HDfflush(stdout);
            }
#endif /* JRM */

            MPI_Get_count(&status, MPI_BYTE, &count);
            if (count > request_size) {
                msg = (sf_work_request_t *)malloc((size_t)count);
                ret = MPI_Recv(msg, count, MPI_BYTE, source, tag, context->sf_msg_comm, &msg_status);
            }
            else {
                index = atomic_load(&sf_workinprogress);
                ret = MPI_Recv(&incoming_requests[index], count, MPI_BYTE, source, tag, context->sf_msg_comm,
                               &msg_status);
                if (MPI_SUCCESS == ret) {
                    int howmany = 0;
                    MPI_Get_count(&msg_status, MPI_BYTE, &howmany);
                    if (howmany != count) {
                        printf("%s: MPI_Recv completed %d bytes of %d\n", __func__, howmany, count);
                        fflush(stdout);
                    }
                }
            }
            queue_start_time = MPI_Wtime();
            if (ret == MPI_SUCCESS) {
                if (msg) {
                    printf("%s: non-std msg=(%p) from %d\n", __func__, (void *)msg, source);
                    fflush(stdout);

                    msg->source       = source;
                    msg->subfile_rank = subfile_rank;
                    msg->context_id   = context->sf_context_id;
                    msg->start_time   = queue_start_time;
                    tpool_add_work(msg);
                }
                else {
                    incoming_requests[index].tag          = tag;
                    incoming_requests[index].source       = source;
                    incoming_requests[index].subfile_rank = subfile_rank;
                    incoming_requests[index].start_time   = queue_start_time;
                    incoming_requests[index].buffer       = NULL;
                    tpool_add_work(&incoming_requests[index]);
                    if (index == max_work_depth - 1) {
                        atomic_init(&sf_workinprogress, 0);
                    }
                    else {
                        atomic_fetch_add(&sf_workinprogress, 1); // atomic
                    }
                }
            }
        }
        else {
            usleep(delay);
        }
        shutdown_requested = atomic_load(&sf_shutdown_flag);
    }

    if (incoming_requests) {
        free(incoming_requests);
    }

    /* Reset the shutdown flag */
    atomic_init(&sf_shutdown_flag, 0);

    return 0;
}

#else /* JRM */ /* re-written version of ioc_main() */

int
ioc_main(int64_t context_id)
{
    int                  subfile_rank;
    int                  flag, ret;
    int                  max_work_depth;
    int                  shutdown_requested;
    MPI_Status           status, msg_status;
    sf_work_request_t    wk_req;
    useconds_t           delay   = 20;
    subfiling_context_t *context = get__subfiling_object(context_id);
    double               queue_start_time;

#if 0  /* JRM */
    HDfprintf(stdout, "\n\nioc_main: entering.\n\n");
    HDfflush(stdout);
#endif /* JRM */

    assert(context != NULL);
    /* We can't have opened any files at this point..
     * The file open approach has changed so that the normal
     * application rank (hosting this thread) does the file open.
     * We can simply utilize the file descriptor (which should now
     * represent an open file).
     */

    subfile_rank = context->sf_group_rank;

    /* zero out the wk_req, since the received message will typically be smaller
     * than sizeof(sf_work_request_t).
     */
    HDmemset(&wk_req, 0, sizeof(sf_work_request_t));

    /* Initialize atomic vars */
    /* JRM */ /* delete most of these? */
    atomic_init(&sf_workinprogress, 0);
#if 1  /* JRM */
    atomic_init(&sf_work_pending, 0);
#endif /* JRM */
    atomic_init(&sf_file_close_count, 0);
    atomic_init(&sf_file_refcount, 0);
    atomic_init(&sf_ioc_fini_refcount, 0);
    atomic_init(&sf_shutdown_flag, 0);
#if 1  /* JRM */
    /* this variable is incremented by H5FD_ioc__queue_io_q_entry() when work
     * is added to the I/O request queue, and decremented by H5FD_ioc__complete_io_q_entry()
     * when an I/O request is completed and removed from the queue..
     *
     * On shutdown, we must wait until this field is decremented to zero before
     * taking down the thread pool.
     *
     * Note that this is a convenience variable -- we could use io_queue_g.q_len instead.
     * However, accessing this field requires locking io_queue_g.q_mutex.
     */
#if 0  /* JRM */
    HDfprintf(stdout, "\n\nioc_main: setting sf_io_ops_pending to zero.  sf_io_ops_pending = %d.\n\n",
             atomic_load(&sf_io_ops_pending));
    HDfflush(stdout);
#endif /* JRM */
    atomic_init(&sf_io_ops_pending, 0);
#endif /* JRM */
    /* tell initialize_ioc_threads() that ioc_main() is ready to enter its main loop */
    atomic_init(&sf_ioc_ready, 1);
    shutdown_requested = 0;

    while ((!shutdown_requested) || (0 < atomic_load(&sf_io_ops_pending))
#if 1  /* JRM */
           || (0 < atomic_load(&sf_work_pending))
#endif /* JRM */
    ) {
        flag = 0;
        ret  = MPI_Iprobe(MPI_ANY_SOURCE, MPI_ANY_TAG, context->sf_msg_comm, &flag, &status);
        if ((ret == MPI_SUCCESS) && (flag != 0)) {
            sf_work_request_t *msg = NULL;
            int                count;
            int                index        = 0;
            int                request_size = (int)sizeof(sf_work_request_t);
            int                source       = status.MPI_SOURCE;
            int                tag          = status.MPI_TAG;

#if 1  /* JRM */
            if ((tag != READ_INDEP) && (tag != WRITE_INDEP) && (tag != TRUNC_OP) && (tag != GET_EOF_OP)) {

                HDprintf("\n\nioc_main: received non READ_INDEP / WRITE_INDEP / TRUNC_OP / GET_EOF_OP mssg. "
                         "tag = %d.\n\n",
                         tag);
                HDfflush(stdout);
            }
#endif /* JRM */

            MPI_Get_count(&status, MPI_BYTE, &count);

            /* convert this assert to a proper error message once we decide how to handle error
             * reporting from the I/O concentrator.
             */
            HDassert(count <= sizeof(sf_work_request_t));

            /* zero out the wk_req, since the received message will typically be smaller
             * than sizeof(sf_work_request_t).
             */
            HDmemset(&wk_req, 0, sizeof(sf_work_request_t));

            ret = MPI_Recv(&wk_req, count, MPI_BYTE, source, tag, context->sf_msg_comm, &msg_status);

            if (MPI_SUCCESS == ret) {

                int howmany = 0;

                MPI_Get_count(&msg_status, MPI_BYTE, &howmany);

                if (howmany != count) {
                    printf("%s: MPI_Recv completed %d bytes of %d\n", __func__, howmany, count);
                    fflush(stdout);
                }
            }

            queue_start_time = MPI_Wtime();

            if (ret == MPI_SUCCESS) {

                int curr_io_ops_pending;

                wk_req.tag          = tag;
                wk_req.source       = source;
                wk_req.subfile_rank = subfile_rank;
                wk_req.start_time   = queue_start_time;
                wk_req.buffer       = NULL;

                H5FD_ioc__queue_io_q_entry(&wk_req);

                HDassert(atomic_load(&sf_io_ops_pending) >= 0);

                H5FD_ioc__dispatch_elegible_io_q_entries();
            }
        }
        else {
            usleep(delay);
        }
        shutdown_requested = atomic_load(&sf_shutdown_flag);
    }

    /* Reset the shutdown flag */
    atomic_init(&sf_shutdown_flag, 0);

#if 0  /* JRM */
    HDfprintf(stdout, "\n\nioc_main: exiting.\n\n");
    HDfflush(stdout);
#endif /* JRM */

    return 0;

} /* ioc_main() */

#endif /* JRM */ /* re-written version of ioc_main() */

/*
=========================================
Private helper functions
=========================================
*/

#if 0 /* JRM */ /* original version */
static int
send_ack__(int target, int subfile_rank, int tag, MPI_Comm comm)
{
    int ack = 1;
    int ret = MPI_Send(&ack, 1, MPI_INT, target, tag, comm);
#ifndef NDEBUG
    if (sf_verbose_flag) {
        if (sf_logfile) {
            fprintf(sf_logfile, "[ioc(%d): Sending ACK to MPI_rank(%d)\n", subfile_rank, target);
        }
    }
#endif
    return ret;
}
#else /* JRM */ /* version modified to send expected data send tag */

static int
send_ack__(int target, int subfile_rank, int tag, MPI_Comm comm, int ack)
{

    HDassert(ack > 0);

    int ret = MPI_Send(&ack, 1, MPI_INT, target, tag, comm);
#ifndef NDEBUG
    if (sf_verbose_flag) {
        if (sf_logfile) {
            fprintf(sf_logfile, "[ioc(%d): Sending ACK to MPI_rank(%d)\n", subfile_rank, target);
        }
    }
#endif
    return ret;

} /* send_ack__() */

#endif /* JRM */ /* version modified to send expected data send tag */

static int
send_nack__(int target, int subfile_rank, int tag, MPI_Comm comm)
{
    int nack = 0;
    int ret  = MPI_Send(&nack, 1, MPI_INT, target, tag, comm);

#ifndef NDEBUG
    if (sf_verbose_flag) {
        if (sf_logfile) {
            fprintf(sf_logfile, "[ioc(%d): Sending NACK to MPI_rank(%d)\n", subfile_rank, target);
        }
    }
#endif
    return ret;
}

/*
=========================================
queue_xxx functions that should be run
from the thread pool threads...
=========================================
*/

/*-------------------------------------------------------------------------
 * Function:    Public/IOC queue_write_indep
 *
 * Purpose:     Implement the IOC independent write function.  The
 *              function is invoked as a result of the IOC receiving the
 *              "header"/RPC.  What remains is to allocate memory for the
 *              data sent by the client and then write the data to our
 *              subfile.  We utilize pwrite for the actual file writing.
 *              File flushing is done at file close.
 *
 * Return:      The integer status returned by the Internal read_independent
 *              function.  Successful operations will return 0.
 * Errors:      An MPI related error value.
 *
 * Programmer:  Richard Warren
 *              7/17/2020
 *
 * Changes:     Initial Version/None.
 *
 *-------------------------------------------------------------------------
 */
#if 0 /* JRM */ /* original version */
int
queue_write_indep(sf_work_request_t *msg, int subfile_rank, int source, MPI_Comm comm)
{
    int                  fd;
#if 1           /* JRM */
    int                  actual_bytes_received;
#endif          /* JRM */
    char *               recv_buffer = NULL;
    int                  ret         = MPI_SUCCESS;
    MPI_Status           msg_status;
    int64_t              data_size       = msg->header[0];
    int64_t              file_offset     = msg->header[1];
    int64_t              file_context_id = msg->header[2];
    double               t_start, t_end;
    double               t_write, t_wait, t_queue_delay;
    subfiling_context_t *sf_context = get__subfiling_object(file_context_id);
    int64_t              stripe_id  = file_offset + data_size;
    haddr_t              sf_eof;
    assert(sf_context != NULL);

    sf_eof = (haddr_t)(stripe_id % sf_context->sf_stripe_size);
    stripe_id /= sf_context->sf_stripe_size;
    sf_eof += (haddr_t)((stripe_id * sf_context->sf_blocksize_per_stripe) + sf_context->sf_base_addr);

    /* flag that we've attempted to write data to the file */
    sf_context->sf_write_count++;
    /* For debugging performance */
    sf_write_ops++;

    t_start       = MPI_Wtime();
    t_queue_delay = t_start - msg->start_time;

#ifndef NDEBUG
    if (sf_verbose_flag) {
        if (sf_logfile) {
            fprintf(sf_logfile,
                    "[ioc(%d) %s]: msg from %d: datasize=%ld\toffset=%ld, "
                    "queue_delay = %lf seconds\n",
                    subfile_rank, __func__, source, data_size, file_offset, t_queue_delay);
        }
    }
#endif

    if (recv_buffer == NULL) {
        if ((recv_buffer = (char *)malloc((size_t)data_size)) == NULL) {
            perror("malloc");
            send_nack__(source, subfile_rank, WRITE_INDEP_ACK, comm);
            return -1;
        }
    }

    send_ack__(source, subfile_rank, WRITE_INDEP_ACK, comm);
    ret = MPI_Recv(recv_buffer, (int)data_size, MPI_BYTE, source, WRITE_INDEP_DATA, comm, &msg_status);

#if 1  /* JRM */
    if ( MPI_SUCCESS != MPI_Get_count(&msg_status, MPI_BYTE, &actual_bytes_received) ) {

        HDprintf("\n\nqueue_write_indep(): can't get actual bytes receive.\n\n");
        HDfflush(stdout);

    } else if ( actual_bytes_received != data_size ) {

        HDprintf("\n\nqueue_write_indep(): message size mismatch -- expected = %ld, actual = %d.\n\n",
                 data_size, actual_bytes_received);
        HDfflush(stdout);

    }
#endif /* JRM */

    t_end  = MPI_Wtime();
    t_wait = t_end - t_start;
    sf_write_wait_time += t_wait;
    t_start = t_end;
#ifndef NDEBUG
    if (sf_verbose_flag) {
        if (sf_logfile) {
            fprintf(sf_logfile, "[ioc(%d) %s] MPI_Recv(%ld bytes, from = %d) status = %d\n", subfile_rank,
                    __func__, data_size, source, ret);
        }
    }
#endif

    if (ret != MPI_SUCCESS) {
        int  len;
        char estring[MPI_MAX_ERROR_STRING];
        MPI_Error_string(ret, estring, &len);
        printf("[ioc(%d) %s] MPI_ERROR(%d)! MPI_Recv of %ld bytes from %d "
               "returned an error(%s)\n",
               subfile_rank, __func__, msg_status.MPI_ERROR, data_size, source, estring);
        fflush(stdout);
        return ret;
    }

    if (msg->serialize)
        ioc__wait_for_serialize(msg);

    fd = sf_context->sf_fid;

    if (fd < 0) {
        printf("[ioc(%d)] WARNING: %s called while subfile_fid = %d (closed)\n", subfile_rank, __func__, fd);
        fflush(stdout);
    }
    else {
        if (sf_write_data(fd, file_offset, recv_buffer, data_size, subfile_rank) < 0) {
            free(recv_buffer);
            recv_buffer = NULL;
            printf("[ioc(%d) %s] sf_write_data returned an error!\n", subfile_rank, __func__);
            fflush(stdout);
            return -1;
        }
        t_end   = MPI_Wtime();
        t_write = t_end - t_start;
        sf_pwrite_time += t_write;
    }

    sf_queue_delay_time += t_queue_delay;

    /* Done... */
    if (sf_eof > sf_context->sf_eof)
        sf_context->sf_eof = sf_eof;

#ifdef VERBOSE
    printf("[ioc(%d)] %s local sf_eof = %ld sf_context=%p\n", subfile_rank, __func__, sf_context->sf_eof,
           (void *)sf_context);
    fflush(stdout);
#endif
    if (recv_buffer) {
        free(recv_buffer);
    }
    return 0;
}

#else /* JRM */ /* version modified for new dispatch code */

int
queue_write_indep(sf_work_request_t *msg, int subfile_rank, int source, MPI_Comm comm, uint32_t counter)
{
    int                  fd;
#if 1           /* JRM */
    int                  actual_bytes_received;
#endif          /* JRM */
    int                  rcv_tag     = ((counter & 0xFFFF) << 12) | WRITE_INDEP_DATA;
    char *               recv_buffer = NULL;
    int                  ret         = MPI_SUCCESS;
    MPI_Status           msg_status;
    int64_t              data_size       = msg->header[0];
    int64_t              file_offset     = msg->header[1];
    int64_t              file_context_id = msg->header[2];
    double               t_start, t_end;
    double               t_write, t_wait, t_queue_delay;
    subfiling_context_t *sf_context = get__subfiling_object(file_context_id);
    int64_t              stripe_id  = file_offset + data_size;
    haddr_t              sf_eof;
    assert(sf_context != NULL);

    sf_eof = (haddr_t)(stripe_id % sf_context->sf_stripe_size);
    stripe_id /= sf_context->sf_stripe_size;
    sf_eof += (haddr_t)((stripe_id * sf_context->sf_blocksize_per_stripe) + sf_context->sf_base_addr);

    /* flag that we've attempted to write data to the file */
    sf_context->sf_write_count++;
    /* For debugging performance */
    sf_write_ops++;

    t_start       = MPI_Wtime();
    t_queue_delay = t_start - msg->start_time;

#ifndef NDEBUG
    if (sf_verbose_flag) {
        if (sf_logfile) {
            fprintf(sf_logfile,
                    "[ioc(%d) %s]: msg from %d: datasize=%ld\toffset=%ld, "
                    "queue_delay = %lf seconds\n",
                    subfile_rank, __func__, source, data_size, file_offset, t_queue_delay);
        }
    }
#endif

    if (recv_buffer == NULL) {
        if ((recv_buffer = (char *)malloc((size_t)data_size)) == NULL) {
            perror("malloc");
            send_nack__(source, subfile_rank, WRITE_INDEP_ACK, comm);
            return -1;
        }
    }

    send_ack__(source, subfile_rank, WRITE_INDEP_ACK, comm, rcv_tag);
    ret = MPI_Recv(recv_buffer, (int)data_size, MPI_BYTE, source, rcv_tag, comm, &msg_status);

#if 1  /* JRM */
    if (MPI_SUCCESS != MPI_Get_count(&msg_status, MPI_BYTE, &actual_bytes_received)) {

        HDprintf("\n\nqueue_write_indep(): can't get actual bytes receive.\n\n");
        HDfflush(stdout);
    }
    else if (actual_bytes_received != data_size) {

        HDprintf("\n\nqueue_write_indep(): message size mismatch -- expected = %ld, actual = %d.\n\n",
                 data_size, actual_bytes_received);
        HDfflush(stdout);
    }
#endif /* JRM */

    t_end  = MPI_Wtime();
    t_wait = t_end - t_start;
    sf_write_wait_time += t_wait;
    t_start = t_end;
#ifndef NDEBUG
    if (sf_verbose_flag) {
        if (sf_logfile) {
            fprintf(sf_logfile, "[ioc(%d) %s] MPI_Recv(%ld bytes, from = %d) status = %d\n", subfile_rank,
                    __func__, data_size, source, ret);
        }
    }
#endif

    if (ret != MPI_SUCCESS) {
        int  len;
        char estring[MPI_MAX_ERROR_STRING];
        MPI_Error_string(ret, estring, &len);
        printf("[ioc(%d) %s] MPI_ERROR(%d)! MPI_Recv of %ld bytes from %d "
               "returned an error(%s)\n",
               subfile_rank, __func__, msg_status.MPI_ERROR, data_size, source, estring);
        fflush(stdout);
        return ret;
    }

    fd = sf_context->sf_fid;

    if (fd < 0) {
        printf("[ioc(%d)] WARNING: %s called while subfile_fid = %d (closed)\n", subfile_rank, __func__, fd);
        fflush(stdout);
    }
    else {
        if (sf_write_data(fd, file_offset, recv_buffer, data_size, subfile_rank) < 0) {
            free(recv_buffer);
            recv_buffer = NULL;
            printf("[ioc(%d) %s] sf_write_data returned an error!\n", subfile_rank, __func__);
            fflush(stdout);
            return -1;
        }
        t_end   = MPI_Wtime();
        t_write = t_end - t_start;
        sf_pwrite_time += t_write;
    }

    sf_queue_delay_time += t_queue_delay;

    /* Done... */
    if (sf_eof > sf_context->sf_eof)
        sf_context->sf_eof = sf_eof;

#ifdef VERBOSE
    printf("[ioc(%d)] %s local sf_eof = %ld sf_context=%p\n", subfile_rank, __func__, sf_context->sf_eof,
           (void *)sf_context);
    fflush(stdout);
#endif
    if (recv_buffer) {
        free(recv_buffer);
    }
    return 0;

} /* queue_write_indep() */

#endif /* JRM */ /* version modified for new dispatch code */

/*-------------------------------------------------------------------------
 * Function:    Public/IOC queue_read_indep
 *
 * Purpose:     Implement the IOC independent read function.  The
 *              function is invoked as a result of the IOC receiving the
 *              "header"/RPC.  What remains is to allocate memory for
 *              reading the data and then to send this to the client.
 *              We utilize pread for the actual file reading.
 *
 * Return:      The integer status returned by the Internal read_independent
 *              function.  Successful operations will return 0.
 * Errors:      An MPI related error value.
 *
 * Programmer:  Richard Warren
 *              7/17/2020
 *
 * Changes:     Initial Version/None.
 *
 *-------------------------------------------------------------------------
 */
#if 0 /* JRM */ /* original version */
int
queue_read_indep(sf_work_request_t *msg, int subfile_rank, int source, MPI_Comm comm)
{
    int     fd;
    char *  send_buffer     = NULL;
    int     ret             = MPI_SUCCESS;
    int64_t data_size       = msg->header[0];
    int64_t file_offset     = msg->header[1];
    int64_t file_context_id = msg->header[2];
    double  t_start, t_end;
    double  t_read, t_queue_delay;

    subfiling_context_t *sf_context = get__subfiling_object(file_context_id);
    assert(sf_context != NULL);

    sf_context->sf_read_count++;
    /* For debugging performance */
    sf_read_ops++;

    t_start       = MPI_Wtime();
    t_queue_delay = t_start - msg->start_time;

    fd = sf_context->sf_fid;
    if (fd < 0) {
        printf("[ioc(%d) %s] subfile(%d) file descriptor not valid\n", subfile_rank, __func__, fd);
        return -1;
    }

#ifndef NDEBUG
    if (sf_verbose_flag && (sf_logfile != NULL)) {
        fprintf(sf_logfile,
                "[ioc(%d) %s] msg from %d: datasize=%ld\toffset=%ld "
                "queue_delay=%lf seconds\n",
                subfile_rank, __func__, source, data_size, file_offset, t_queue_delay);
    }
#endif
    if ((send_buffer = (char *)malloc((size_t)data_size)) == NULL) {
        perror("malloc");
        return -1;
    }

    if (sf_read_data(fd, file_offset, send_buffer, data_size, subfile_rank) < 0) {
        printf("[%d] %s - sf_read_data fd=%d for source(%d) returned an error!\n", subfile_rank, __func__, fd,
               source);
        fflush(stdout);
        /*
         * Should send a zero(0) byte message to the client to prevent
         * it from hanging...
         */
        MPI_Send(send_buffer, 0, MPI_BYTE, source, READ_INDEP_DATA, comm);
        free(send_buffer);
        return -1;
    }

    ret = MPI_Send(send_buffer, (int)data_size, MPI_BYTE, source, READ_INDEP_DATA, comm);
    if (ret != MPI_SUCCESS) {
        int  len;
        char estring[MPI_MAX_ERROR_STRING];
        MPI_Error_string(ret, estring, &len);
        printf("[ioc(%d)] ERROR! MPI_Send of %ld bytes to %d returned an "
               "error(%s)\n",
               subfile_rank, data_size, source, estring);
        fflush(stdout);
        return ret;
    }
    t_end  = MPI_Wtime();
    t_read = t_end - t_start;
    sf_pread_time += t_read;
    sf_queue_delay_time += t_queue_delay;

#ifndef NDEBUG
    if (sf_verbose_flag && (sf_logfile != NULL)) {
        fprintf(sf_logfile, "[ioc(%d)] MPI_Send to source(%d) completed\n", subfile_rank, source);
    }
#endif

    if (send_buffer) {
        free(send_buffer);
        send_buffer = NULL;
    }

    return 0;
} /* end queue_read_indep() */

#else /* JRM */ /* version modified for new dispatch code */

int
queue_read_indep(sf_work_request_t *msg, int subfile_rank, int source, MPI_Comm comm)
{
    int     fd;
    char *  send_buffer     = NULL;
    int     ret             = MPI_SUCCESS;
    int64_t data_size       = msg->header[0];
    int64_t file_offset     = msg->header[1];
    int64_t file_context_id = msg->header[2];
    double  t_start, t_end;
    double  t_read, t_queue_delay;

    subfiling_context_t *sf_context = get__subfiling_object(file_context_id);
    assert(sf_context != NULL);

    sf_context->sf_read_count++;
    /* For debugging performance */
    sf_read_ops++;

    t_start       = MPI_Wtime();
    t_queue_delay = t_start - msg->start_time;

    fd = sf_context->sf_fid;
    if (fd < 0) {
        printf("[ioc(%d) %s] subfile(%d) file descriptor not valid\n", subfile_rank, __func__, fd);
        return -1;
    }

#ifndef NDEBUG
    if (sf_verbose_flag && (sf_logfile != NULL)) {
        fprintf(sf_logfile,
                "[ioc(%d) %s] msg from %d: datasize=%ld\toffset=%ld "
                "queue_delay=%lf seconds\n",
                subfile_rank, __func__, source, data_size, file_offset, t_queue_delay);
    }
#endif
    if ((send_buffer = (char *)malloc((size_t)data_size)) == NULL) {
        perror("malloc");
        return -1;
    }

    if (sf_read_data(fd, file_offset, send_buffer, data_size, subfile_rank) < 0) {
        printf("[%d] %s - sf_read_data fd=%d for source(%d) returned an error!\n", subfile_rank, __func__, fd,
               source);
        fflush(stdout);
        /*
         * Should send a zero(0) byte message to the client to prevent
         * it from hanging...
         */
        MPI_Send(send_buffer, 0, MPI_BYTE, source, READ_INDEP_DATA, comm);
        free(send_buffer);
        return -1;
    }

    ret = MPI_Send(send_buffer, (int)data_size, MPI_BYTE, source, READ_INDEP_DATA, comm);
    if (ret != MPI_SUCCESS) {
        int  len;
        char estring[MPI_MAX_ERROR_STRING];
        MPI_Error_string(ret, estring, &len);
        printf("[ioc(%d)] ERROR! MPI_Send of %ld bytes to %d returned an "
               "error(%s)\n",
               subfile_rank, data_size, source, estring);
        fflush(stdout);
        return ret;
    }
    t_end  = MPI_Wtime();
    t_read = t_end - t_start;
    sf_pread_time += t_read;
    sf_queue_delay_time += t_queue_delay;

#ifndef NDEBUG
    if (sf_verbose_flag && (sf_logfile != NULL)) {
        fprintf(sf_logfile, "[ioc(%d)] MPI_Send to source(%d) completed\n", subfile_rank, source);
    }
#endif

    if (send_buffer) {
        free(send_buffer);
        send_buffer = NULL;
    }

    return 0;
} /* end queue_read_indep() */

#endif /* JRM */ /* version modified for new dispatch code */

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
 * Function:    Internal write__independent_async.
 *
 * Purpose:     The IO operations can be striped across a selection of
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
 * Return:      Success (0) or Faiure (non-zero)
 * Errors:      If MPI operations fail for some reason.
 *
 * Programmer:  Richard Warren
 *              7/17/2020
 *
 * Changes:     Initial Version/None.
 *-------------------------------------------------------------------------
 */
#if 0 /* JRM */ /* original version */
int
write__independent_async(int n_io_concentrators, hid_t context_id, int64_t offset, int64_t elements,
                         int H5_ATTR_PARALLEL_UNUSED dtype_extent, const void *data, io_req_t **io_req)
{

    int         ack = 0, active_sends = 0, n_waiting = 0, status = 0;
    int64_t     stripe_size, ioc_row, start_id, ioc_start, ioc_offset;
    int *       io_concentrator = NULL;
    io_req_t *  sf_io_request   = NULL;
    MPI_Request ackrequest;
    int64_t     msg[3] = {
        0,
    };

    subfiling_context_t *sf_context = get__subfiling_object(context_id);
    assert(sf_context != NULL);

    /* Calculate the IOC that we'll send the IO request to */
    stripe_size = sf_context->sf_stripe_size;

    start_id   = offset / stripe_size;
    ioc_row    = start_id / n_io_concentrators;
    ioc_offset = (offset % stripe_size) + (ioc_row * stripe_size);
    ioc_start  = start_id % n_io_concentrators;

    io_concentrator = sf_context->topology->io_concentrator;
    assert(io_concentrator != NULL);

    /* Make sure that we can return a request structure
     * if everything is working correctly
     */
    assert(io_req);

    /* Prepare an IO request.
     * This gets sent to the ioc identified by the file offset.
     * (see above: Calculate the IOC))
     */
    msg[0] = elements;
    msg[1] = ioc_offset;
    msg[2] = context_id;
#ifdef VERBOSE
    printf("[%s ioc(%ld)] elements=%ld, offset=%ld, file_offset=%ld\n", __func__, ioc_start, elements, offset,
           ioc_offset);
    fflush(stdout);
#endif
    status = MPI_Send(msg, 3, MPI_INT64_T, io_concentrator[ioc_start], WRITE_INDEP, sf_context->sf_msg_comm);
    if (status != MPI_SUCCESS) {
        int  len;
        char estring[MPI_MAX_ERROR_STRING];
        MPI_Error_string(status, estring, &len);
        printf("[%d] ERROR! MPI_Send of %ld bytes to %d returned an "
               "error(%s)\n",
               WORLD_RANK(sf_context), sizeof(msg), io_concentrator[ioc_start], estring);
        fflush(stdout);
        return -1;
    }
    else
        active_sends++;
    /*
     * We wait for memory to be allocated on the target IOC so that we can
     * start sending user data. Once memory is allocated, we will receive
     * an ACK (or NACK) message from the IOC to allow us to proceed.
     */
    status = MPI_Irecv(&ack, 1, MPI_INT, io_concentrator[ioc_start], WRITE_INDEP_ACK,
                       sf_context->sf_data_comm, &ackrequest);

    if (status != MPI_SUCCESS) {
        printf("[%d %s] MPI_Irecv failed\n", WORLD_RANK(sf_context), __func__);
        fflush(stdout);
        return -1;
    }

    n_waiting = active_sends;

    while (n_waiting) {
        int flag = 0;
        status   = MPI_Test(&ackrequest, &flag, MPI_STATUS_IGNORE);
        if (status == MPI_SUCCESS) {
            if (flag == 0)
                usleep(0);
            else {
                n_waiting--;
                if (ack == 0) { /* NACK */
                    printf("%s - Received NACK!\n", __func__);
                }
            }
        }
    }

    /* At this point in the new implementation, we should queue
     * the async write so that when the top level VFD tells us
     * to complete all pending IO requests, we have all the info
     * we need to accomplish that.
     */
    sf_io_request = (io_req_t *)malloc(sizeof(io_req_t));
    assert(sf_io_request);

    sf_io_request->completion_func.io_args.ioc        = (int)ioc_start;
    sf_io_request->completion_func.io_args.context_id = context_id;
    sf_io_request->completion_func.io_args.offset     = offset;
    sf_io_request->completion_func.io_args.elements   = elements;
    sf_io_request->completion_func.io_args.data       = cast_to_void(data);
    sf_io_request->completion_func.io_args.io_req     = MPI_REQUEST_NULL;
    sf_io_request->completion_func.io_function        = async_completion;
    sf_io_request->completion_func.pending            = 0;

    sf_io_request->prev = sf_io_request->next = NULL;
    /* Start the actual data transfer */

#if 1 /* JRM */ /* experiment with MPI_Issend() */
    status = MPI_Isend(data, (int)elements, MPI_BYTE, io_concentrator[ioc_start], WRITE_INDEP_DATA,
                       sf_context->sf_data_comm, &sf_io_request->completion_func.io_args.io_req);
#else           /* JRM */
#if 1 /* JRM */ /* experiment with MPI_Send */
    status = MPI_Issend(data, (int)elements, MPI_BYTE, io_concentrator[ioc_start], WRITE_INDEP_DATA,
                        sf_context->sf_data_comm, &sf_io_request->completion_func.io_args.io_req);
#else           /* JRM */
    status = MPI_Send(data, (int)elements, MPI_BYTE, io_concentrator[ioc_start], WRITE_INDEP_DATA,
                        sf_context->sf_data_comm);
#endif          /* JRM */
#endif          /* JRM */

    /* When we actually have the async IO support,
     * the request should be queued before we
     * return to the caller.
     * Having queued the IO operation, we might want to
     * get additional work started before allowing the
     * queued IO requests to make further progress and/or
     * to complete, so we just return to the caller.
     */

    if (status == MPI_SUCCESS) {
        sf_io_request->completion_func.pending = 1;
        *io_req                                = sf_io_request;
    }
    else {
        puts("MPI_Isend must have failed!");
        free(sf_io_request);
        *io_req = NULL;
    }
    return status;
} /* end write__independent_async() */

#else /* JRM */ /* modified to use IOC supplied tag for data send */

int
write__independent_async(int n_io_concentrators, hid_t context_id, int64_t offset, int64_t elements,
                         int H5_ATTR_PARALLEL_UNUSED dtype_extent, const void *data, io_req_t **io_req)
{

    int         ack = 0, active_sends = 0, n_waiting = 0, status = 0;
    int64_t     stripe_size, ioc_row, start_id, ioc_start, ioc_offset;
    int *       io_concentrator = NULL;
    io_req_t *  sf_io_request   = NULL;
    MPI_Request ackrequest;
    int64_t     msg[3] = {
        0,
    };

    subfiling_context_t *sf_context = get__subfiling_object(context_id);
    assert(sf_context != NULL);

    /* Calculate the IOC that we'll send the IO request to */
    stripe_size = sf_context->sf_stripe_size;

    start_id   = offset / stripe_size;
    ioc_row    = start_id / n_io_concentrators;
    ioc_offset = (offset % stripe_size) + (ioc_row * stripe_size);
    ioc_start  = start_id % n_io_concentrators;

    io_concentrator = sf_context->topology->io_concentrator;
    assert(io_concentrator != NULL);

    /* Make sure that we can return a request structure
     * if everything is working correctly
     */
    assert(io_req);

    /* Prepare an IO request.
     * This gets sent to the ioc identified by the file offset.
     * (see above: Calculate the IOC))
     */
    msg[0] = elements;
    msg[1] = ioc_offset;
    msg[2] = context_id;
#ifdef VERBOSE
    printf("[%s ioc(%ld)] elements=%ld, offset=%ld, file_offset=%ld\n", __func__, ioc_start, elements, offset,
           ioc_offset);
    fflush(stdout);
#endif
    status = MPI_Send(msg, 3, MPI_INT64_T, io_concentrator[ioc_start], WRITE_INDEP, sf_context->sf_msg_comm);
    if (status != MPI_SUCCESS) {
        int  len;
        char estring[MPI_MAX_ERROR_STRING];
        MPI_Error_string(status, estring, &len);
        printf("[%d] ERROR! MPI_Send of %ld bytes to %d returned an "
               "error(%s)\n",
               WORLD_RANK(sf_context), sizeof(msg), io_concentrator[ioc_start], estring);
        fflush(stdout);
        return -1;
    }
    else
        active_sends++;
    /*
     * We wait for memory to be allocated on the target IOC so that we can
     * start sending user data. Once memory is allocated, we will receive
     * an ACK (or NACK) message from the IOC to allow us to proceed.
     */
    /* On ACK, IOC will send tag to be used for data send -- need this to
     * distinguish between multiple concurrent writes from a single rank.
     */
    status = MPI_Irecv(&ack, 1, MPI_INT, io_concentrator[ioc_start], WRITE_INDEP_ACK,
                       sf_context->sf_data_comm, &ackrequest);

    if (status != MPI_SUCCESS) {
        printf("[%d %s] MPI_Irecv failed\n", WORLD_RANK(sf_context), __func__);
        fflush(stdout);
        return -1;
    }

    n_waiting = active_sends;

    while (n_waiting) {
        int flag = 0;
        status   = MPI_Test(&ackrequest, &flag, MPI_STATUS_IGNORE);
        if (status == MPI_SUCCESS) {
            if (flag == 0)
                usleep(0);
            else {
                n_waiting--;
                if (ack == 0) { /* NACK */
                    printf("%s - Received NACK!\n", __func__);
                }
            }
        }
    }

    /* At this point in the new implementation, we should queue
     * the async write so that when the top level VFD tells us
     * to complete all pending IO requests, we have all the info
     * we need to accomplish that.
     */
    sf_io_request = (io_req_t *)malloc(sizeof(io_req_t));
    assert(sf_io_request);

    sf_io_request->completion_func.io_args.ioc        = (int)ioc_start;
    sf_io_request->completion_func.io_args.context_id = context_id;
    sf_io_request->completion_func.io_args.offset     = offset;
    sf_io_request->completion_func.io_args.elements   = elements;
    sf_io_request->completion_func.io_args.data       = cast_to_void(data);
    sf_io_request->completion_func.io_args.io_req     = MPI_REQUEST_NULL;
    sf_io_request->completion_func.io_function        = async_completion;
    sf_io_request->completion_func.pending            = 0;

    sf_io_request->prev = sf_io_request->next = NULL;
    /* Start the actual data transfer */

#if 1 /* JRM */ /* experiment with MPI_Issend() */
    /* use ack from IOC as the tag for the send */
    status = MPI_Isend(data, (int)elements, MPI_BYTE, io_concentrator[ioc_start], ack,
                       sf_context->sf_data_comm, &sf_io_request->completion_func.io_args.io_req);
#else           /* JRM */
#if 1 /* JRM */ /* experiment with MPI_Send */
    status = MPI_Issend(data, (int)elements, MPI_BYTE, io_concentrator[ioc_start], WRITE_INDEP_DATA,
                        sf_context->sf_data_comm, &sf_io_request->completion_func.io_args.io_req);
#else           /* JRM */
    status = MPI_Send(data, (int)elements, MPI_BYTE, io_concentrator[ioc_start], WRITE_INDEP_DATA,
                      sf_context->sf_data_comm);
#endif          /* JRM */
#endif          /* JRM */

    /* When we actually have the async IO support,
     * the request should be queued before we
     * return to the caller.
     * Having queued the IO operation, we might want to
     * get additional work started before allowing the
     * queued IO requests to make further progress and/or
     * to complete, so we just return to the caller.
     */

    if (status == MPI_SUCCESS) {
        sf_io_request->completion_func.pending = 1;
        *io_req                                = sf_io_request;
    }
    else {
        puts("MPI_Isend must have failed!");
        free(sf_io_request);
        *io_req = NULL;
    }
    return status;
} /* end write__independent_async() */

#endif /* JRM */ /* modified to use IOC supplied tag for data send */

/*-------------------------------------------------------------------------
 * Function:    Internal read__independent_async
 *
 * Purpose:     The IO operations can be striped across a selection of
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
 * Return:      Success (0) or Faiure (non-zero)
 * Errors:      If MPI operations fail for some reason.
 *
 * Programmer:  Richard Warren
 *              7/17/2020
 *
 * Changes:     Initial Version/None.
 *-------------------------------------------------------------------------
 */
int
read__independent_async(int n_io_concentrators, hid_t context_id, int64_t offset, int64_t elements,
                        int H5_ATTR_PARALLEL_UNUSED dtype_extent, void *data, io_req_t **io_req)
{
    int       status = 0;
    int64_t   stripe_size, ioc_row, start_id, ioc_start, ioc_offset;
    int *     io_concentrator = NULL;
    io_req_t *sf_io_request   = NULL;
    int64_t   msg[3]          = {
        0,
    };

    subfiling_context_t *sf_context = get__subfiling_object(context_id);
    assert(sf_context != NULL);

    /* Calculate the IOC that we'll send the IO request to */
    stripe_size = sf_context->sf_stripe_size;

    start_id   = offset / stripe_size;
    ioc_row    = start_id / n_io_concentrators;
    ioc_offset = (offset % stripe_size) + (ioc_row * stripe_size);

    ioc_start = start_id % n_io_concentrators;

    io_concentrator = sf_context->topology->io_concentrator;
    assert(io_concentrator != NULL);

    /* Make sure that we can return a request structure
     * if everything is working correctly
     */
    assert(io_req);

    /* Prepare an IO request.
     * This gets sent to the ioc identified by the file offset
     */
    msg[0] = elements;
    msg[1] = ioc_offset;
    msg[2] = context_id;
#ifdef VERBOSE
    printf("[%s ioc(%ld)] elements=%ld, offset=%ld, file_offset=%ld\n", __func__, ioc_start, elements, offset,
           ioc_offset);
    fflush(stdout);
#endif
    status = MPI_Send(msg, 3, MPI_INT64_T, io_concentrator[ioc_start], READ_INDEP, sf_context->sf_msg_comm);

    if (status != MPI_SUCCESS) {
        int  len;
        char estring[MPI_MAX_ERROR_STRING];
        MPI_Error_string(status, estring, &len);
        printf("[%d] ERROR! MPI_Send request header (%ld) "
               "bytes to %d returned an error(%s)\n",
               WORLD_RANK(sf_context), sizeof(msg), io_concentrator[ioc_start], estring);
        fflush(stdout);
        return -1;
    }

    /* At this point in the new implementation, we should queue
     * the async recv so that when the top level VFD tells us
     * to complete all pending IO requests, we have all the info
     * we need to accomplish that.
     */
    sf_io_request = (io_req_t *)malloc(sizeof(io_req_t));
    assert(sf_io_request);

    sf_io_request->completion_func.io_args.ioc        = (int)ioc_start;
    sf_io_request->completion_func.io_args.context_id = context_id;
    sf_io_request->completion_func.io_args.offset     = offset;
    sf_io_request->completion_func.io_args.elements   = elements;
    sf_io_request->completion_func.io_args.data       = data;
    sf_io_request->completion_func.io_args.io_req     = MPI_REQUEST_NULL;
    sf_io_request->completion_func.io_function        = async_completion;
    sf_io_request->completion_func.pending            = 0;

    sf_io_request->prev = sf_io_request->next = NULL;
    /* Start the actual data transfer */

    status = MPI_Irecv(data, (int)elements, MPI_BYTE, io_concentrator[ioc_start], READ_INDEP_DATA,
                       sf_context->sf_data_comm, &sf_io_request->completion_func.io_args.io_req);

    if (status == MPI_SUCCESS) {
        sf_io_request->completion_func.pending = 1;
        *io_req                                = sf_io_request;
    }
    else {
        puts("MPI_Irecv must have failed!");
        free(sf_io_request);
        *io_req = NULL;
    }

    return status;
} /* end read__independent_async() */

/*-------------------------------------------------------------------------
 * Function:    write_data
 *
 * Purpose:     Given a io_func_t structure containing the function pointer
 *              and it's input arguments, we write the supplied data out
 *              asynchronous using MPI_Isend, to the appropriate IOC.
 *
 * Return:      an integer status.  Zero(0) indicates success. Negative
 *              values (-1) indicates an error.
 *-------------------------------------------------------------------------
 */
static int
write_data(io_func_t *this_func)
{
    int                  ioc, status;
    int64_t              elements;
    void *               data;
    int *                io_concentrator = NULL;
    subfiling_context_t *sf_context      = NULL;
    assert(this_func);

    sf_context = get__subfiling_object(this_func->io_args.context_id);

    assert(sf_context);

    io_concentrator = sf_context->topology->io_concentrator;
    ioc             = this_func->io_args.ioc;

    status = MPI_Isend(data, (int)elements, MPI_BYTE, io_concentrator[ioc], WRITE_INDEP_DATA,
                       sf_context->sf_data_comm, &this_func->io_args.io_req);
    return status;
}

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
    struct async_arg {
        int          n_reqs;
        MPI_Request *sf_reqs;
    } *in_progress = (struct async_arg *)arg;

    assert(arg);
    int        status, errors = 0;
    int        count     = in_progress->n_reqs;
    int        n_waiting = count;
    int        indices[count];
    MPI_Status stats[count];
    useconds_t delay = 5;

    while (n_waiting) {
        int i, ready = 0;
        status = MPI_Testsome(count, in_progress->sf_reqs, &ready, indices, stats);
        if (status != MPI_SUCCESS) {
            int  len;
            char estring[MPI_MAX_ERROR_STRING];
            MPI_Error_string(status, estring, &len);
            printf("[%s] MPI_ERROR! MPI_Testsome returned an error(%s)\n", __func__, estring);
            fflush(stdout);
            errors++;
            return -1;
        }

        if (ready == 0) {
            usleep(delay);
        }

        for (i = 0; i < ready; i++) {
            n_waiting--;
        }
    }
    return errors;
}

/*-------------------------------------------------------------------------
 * Utility functions in support of a first pass attempt at handling async
 * IO.  The working assumption is that reads and writes to a collection
 * of IO Concentrators (IOCs) will proceed by stages.  In the first stage,
 * each MPI rank will get their individual IOs started by preping the IOC
 * with a message which indicates (via the MPI tag) what operation is
 * starting, along with the file offset, data size, and a context_id.
 * The latter will be used to access the actual open file descriptor.
 *
 *-------------------------------------------------------------------------
 * Function:    progress_this_pending_io
 *
 * Purpose:     In this initial example, we can progress an individual
 *              IO request which is described by the io_req_t input arg.
 *
 * Return:      an integer status.  Zero(0) indicates success. Negative
 *              values (-1) indicates an error.
 *-------------------------------------------------------------------------
 */
static int
progress_this_pending_io(io_req_t *this_req)
{
    assert(this_req);
    assert(this_req->completion_func.io_function);
    return (*this_req->completion_func.io_function)(&this_req->completion_func);
}

/*
======================================================
File functions

The pread and pwrite posix functions are described as
being thread safe.
======================================================
*/

int
sf_write_data(int fd, int64_t file_offset, void *data_buffer, int64_t data_size, int subfile_rank)
{
    int     ret             = 0;
    char *  this_data       = (char *)data_buffer;
    ssize_t bytes_remaining = (ssize_t)data_size;
    ssize_t written         = 0;
    while (bytes_remaining) {
        if ((written = pwrite(fd, this_data, (size_t)bytes_remaining, file_offset)) < 0) {
            int         saved_errno = errno;
            struct stat statbuf;
            perror("pwrite failed!");
            HDprintf("\nerrno = %d (%s)\n\n", saved_errno, strerror(saved_errno));
            fstat(fd, &statbuf);
            HDprintf("[ioc(%d) %s] pwrite(fd, data, bytes_remaining=%ld, "
                     "file_offset=%ld), fd=%d, st_size=%ld\n",
                     subfile_rank, __func__, bytes_remaining, file_offset, fd, statbuf.st_size);
            HDfflush(stdout);
            return -1;
        }
        else {
            bytes_remaining -= written;
#ifdef VERBOSE
            printf("[ioc(%d) %s]: wrote %ld bytes, remaining=%ld, file_offset=%ld\n", subfile_rank, __func__,
                   written, bytes_remaining, file_offset);
            fflush(stdout);
#endif
            this_data += written;
            file_offset += written;
        }
    }
    /* We don't usually use this for each file write.  We usually do the file
     * flush as part of file close operation.
     */
#ifdef SUBFILE_REQUIRE_FLUSH
    fdatasync(fd);
#endif
    return ret;
} /* end sf_write_data() */

int
sf_read_data(int fd, int64_t file_offset, void *data_buffer, int64_t data_size, int subfile_rank)
{
    int        ret     = 0;
    int        retries = MIN_RETRIES;
    useconds_t delay   = 100;
    ssize_t    bytes_read;
    ssize_t    bytes_remaining = (ssize_t)data_size;
    char *     this_buffer     = data_buffer;

    while (bytes_remaining) {
        if ((bytes_read = (ssize_t)pread(fd, this_buffer, (size_t)bytes_remaining, file_offset)) < 0) {

            perror("pread failed!");
            HDprintf("[ioc(%d) %s] pread(fd, buf, bytes_remaining=%ld, "
                     "file_offset =%ld)\n",
                     subfile_rank, __func__, bytes_remaining, file_offset);
            HDfflush(stdout);
            return -1;
        }
        else if (bytes_read > 0) {
            /* reset retry params */
            retries = MIN_RETRIES;
            delay   = 100;
            bytes_remaining -= bytes_read;
#ifdef VERBOSE
            printf("[ioc(%d) %s]: read %ld bytes, remaining=%ld, file_offset=%ld\n", subfile_rank, __func__,
                   bytes_read, bytes_remaining, file_offset);
            fflush(stdout);
#endif
            this_buffer += bytes_read;
            file_offset += bytes_read;
        }
        else {
            if (retries == 0) {
#ifdef VERBOSE
                printf("[ioc(%d) %s] TIMEOUT: file_offset=%ld, data_size=%ld\n", subfile_rank, __func__,
                       file_offset, data_size);
                printf("[ioc(%d) %s] ERROR! read of 0 bytes == eof!\n", subfile_rank, __func__);

                fflush(stdout);
#endif
                return -2;
            }
            retries--;
            usleep(delay);
            delay *= 2;
        }
    }
    return ret;
} /* end sf_read_data() */

int
sf_truncate(int fd, int64_t length, int subfile_rank)
{
    int ret = 0;

    if (HDftruncate(fd, (off_t)length) != 0) {

        HDfprintf(stdout, "ftruncate failed on subfile rank %d.  errno = %d (%s)\n", subfile_rank, errno,
                  strerror(errno));
        fflush(stdout);
        ret = -1;
    }

#ifdef VERBOSE
    HDprintf("[ioc(%d) %s]: truncated subfile to %lld bytes. ret = %d\n", subfile_rank, __func__,
             (long long)length, ret);
    HDfflush(stdout);
#endif

    return ret;
} /* end sf_truncate() */

/*-------------------------------------------------------------------------
 * Function:    report_sf_eof
 *
 * Purpose:     Determine the target sub-file's eof and report this value
 *              to the requesting rank.
 *
 *              Notes: This function will have to be reworked once we solve
 *                     the IOC error reporting problem.
 *
 *                     This function mixes functionality that should be
 *                     in two different VFDs.
 *
 * Return:      0 if successful, 1 or an MPI error code on failure.
 *
 * Programmer:  John Mainzer
 *              7/17/2020
 *
 * Changes:     Initial Version/None.
 *
 *-------------------------------------------------------------------------
 */

int
report_sf_eof(sf_work_request_t *msg, int subfile_rank, int source, MPI_Comm comm)
{
    int                  fd;
    int                  mpi_ret;
    int64_t              eof_req_reply[3];
    int64_t              file_context_id;
    subfiling_context_t *sf_context = NULL;
    h5_stat_t            sb;

    HDassert(msg);

    /* first get the EOF of the target file. */

    file_context_id = msg->header[2];

    if (NULL == (sf_context = get__subfiling_object(file_context_id))) {

        HDfprintf(stdout, "report_sf_eof: get__subfiling_object() failed.\n");
        HDfflush(stdout);
        return (1);
    }

    fd = sf_context->sf_fid;

    if (HDfstat(fd, &sb) < 0) {

        HDfprintf(stdout, "report_sf_eof: get__subfiling_object() failed.\n");
        HDfflush(stdout);
        return (1);
    }

    eof_req_reply[0] = (int64_t)subfile_rank;
    eof_req_reply[1] = (int64_t)(sb.st_size);
    eof_req_reply[2] = 0; /* not used */

    /* return the subfile EOF to the querying rank */
    if (MPI_SUCCESS != (mpi_ret = MPI_Send(eof_req_reply, 3, MPI_INT64_T, source, GET_EOF_COMPLETED, comm))) {

        HDfprintf(stdout, "report_sf_eof: MPI_Send failed -- return code = %d.\n", mpi_ret);
        HDfflush(stdout);
        return (mpi_ret);
    }

    return 0;
} /* report_sf_eof() */
