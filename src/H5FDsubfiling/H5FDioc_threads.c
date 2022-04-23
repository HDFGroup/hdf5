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

#include "H5FDioc_priv.h"

#include "H5FDsubfiling.h"

#include "mercury_thread.h"
#include "mercury_thread_mutex.h"
#include "mercury_thread_pool.h"

#ifndef HG_TEST_NUM_THREADS_DEFAULT
#define HG_TEST_NUM_THREADS_DEFAULT 4
#endif

#define MIN_READ_RETRIES 10

/*
 * NOTES:
 * Rather than re-create the code for creating and managing a thread pool,
 * I'm utilizing a reasonably well tested implementation from the mercury
 * project.  At some point, we should revisit this decision or possibly
 * directly link against the mercury library.  This would make sense if
 * we move away from using MPI as the messaging infrastructure and instead
 * use mercury for that purpose...
 */

static hg_thread_mutex_t ioc_mutex           = PTHREAD_MUTEX_INITIALIZER;
static hg_thread_mutex_t ioc_thread_mutex    = PTHREAD_MUTEX_INITIALIZER;
static hg_thread_mutex_t ioc_serialize_mutex = PTHREAD_MUTEX_INITIALIZER;
static hg_thread_pool_t *ioc_thread_pool     = NULL;
static hg_thread_t       ioc_thread;

#ifdef H5FD_IOC_DEBUG
static int    sf_write_ops        = 0;
static int    sf_read_ops         = 0;
static double sf_pwrite_time      = 0.0;
static double sf_pread_time       = 0.0;
static double sf_write_wait_time  = 0.0;
static double sf_read_wait_time   = 0.0;
static double sf_queue_delay_time = 0.0;
#endif

static int                    pool_concurrent_max = 0;
static struct hg_thread_work *pool_request        = NULL;

atomic_int sf_ioc_ready    = 0;
atomic_int sf_work_pending = 0;

/* sf_io_ops_pending is use to track the number of I/O operations pending so that we can wait
 * until all I/O operations have been serviced before shutting down the worker thread pool.
 * The value of this variable must always be non-negative.
 */
atomic_int sf_io_ops_pending = 0;

static ioc_io_queue_t io_queue_g = {
    /* magic               = */ H5FD_IOC__IO_Q_MAGIC,
    /* q_head              = */ NULL,
    /* q_tail              = */ NULL,
    /* num_pending         = */ 0,
    /* num_in_progress     = */ 0,
    /* q_len               = */ 0,
    /* req_counter         = */ 0,
    /* q_mutex             = */
    PTHREAD_MUTEX_INITIALIZER,
#if H5FD_IOC__COLLECT_STATS
    /* max_q_len           = */ 0,
    /* max_num_pending     = */ 0,
    /* max_num_in_progress = */ 0,
    /* ind_read_requests   = */ 0,
    /* ind_write_requests  = */ 0,
    /* truncate_requests   = */ 0,
    /* get_eof_requests    = */ 0,
    /* requests_queued     = */ 0,
    /* requests_dispatched = */ 0,
    /* requests_completed  = */ 0
#endif /* H5FD_IOC__COLLECT_STATS */
};

/* Prototypes */
static HG_THREAD_RETURN_TYPE ioc_thread_main(void *arg);
static int                   ioc_main(int64_t context_id);

static int ioc_file_queue_write_indep(sf_work_request_t *msg, int subfile_rank, int source, MPI_Comm comm,
                                      uint32_t counter);
static int ioc_file_queue_read_indep(sf_work_request_t *msg, int subfile_rank, int source, MPI_Comm comm);

static int ioc_file_write_data(int fd, int64_t file_offset, void *data_buffer, int64_t data_size,
                               int subfile_rank);
static int ioc_file_read_data(int fd, int64_t file_offset, void *data_buffer, int64_t data_size,
                              int subfile_rank);
static int ioc_file_truncate(int fd, int64_t length, int subfile_rank);
static int ioc_file_report_eof(sf_work_request_t *msg, int subfile_rank, int source, MPI_Comm comm);

static ioc_io_queue_entry_t *ioc_io_queue_alloc_entry(void);
static void                  ioc_io_queue_complete_entry(ioc_io_queue_entry_t *entry_ptr);
static void                  ioc_io_queue_dispatch_eligible_entries(void);
static void                  ioc_io_queue_free_entry(ioc_io_queue_entry_t *q_entry_ptr);
static void                  ioc_io_queue_add_entry(sf_work_request_t *wk_req_ptr);

void __attribute__((destructor)) finalize_ioc_threads(void);
bool tpool_is_empty(void);

/*-------------------------------------------------------------------------
 * Function:    initialize_ioc_threads
 *
 * Purpose:     The principal entry point to initialize the execution
 *              context for an I/O Concentrator (IOC). The main thread
 *              is responsible for receiving I/O requests from each
 *              HDF5 "client" and distributing those to helper threads
 *              for actual processing. We initialize a fixed number
 *              of helper threads by creating a thread pool.
 *
 * Return:      SUCCESS (0) or FAIL (-1) if any errors are detected
 *              for the multi-threaded initialization.
 *
 * Programmer:  Richard Warren
 *              7/17/2020
 *
 * Changes:     Initial Version/None.
 *
 *-------------------------------------------------------------------------
 */
int
initialize_ioc_threads(void *_sf_context)
{
    subfiling_context_t *sf_context        = _sf_context;
    unsigned             thread_pool_count = HG_TEST_NUM_THREADS_DEFAULT;
    int64_t *            context_id        = NULL;
    size_t               pool_req_alloc_size;
    char *               env_value;
    int                  world_size;
    int                  file_open_count;
    int                  status;
    int                  ret_value = 0;
#ifdef H5FD_IOC_DEBUG
    double t_start = 0.0, t_end = 0.0;
#endif

    HDassert(sf_context);

    if (NULL == (context_id = HDmalloc(sizeof(*context_id))))
        H5FD_IOC_GOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL,
                            "couldn't allocate subfiling context ID for IOC main thread");

    file_open_count = atomic_load(&sf_file_open_count);
    atomic_fetch_add(&sf_file_open_count, 1);

    /*
     * TODO: since IOC main thread is only initialized for first
     * open file, hangs are caused with multiple files open. The
     * main thread will only send out MPI communications on the
     * MPI communicator for the first opened file's context ID,
     * whereas the IOC VFD rank will communicate across different
     * file's communicators.
     */
    if (file_open_count > 0)
        H5FD_IOC_GOTO_DONE(0);

#ifdef H5FD_IOC_DEBUG
    t_start = MPI_Wtime();
#endif

    /* Initialize the main IOC thread input argument.
     * Each IOC request will utilize this context_id which is
     * consistent across all MPI ranks, to ensure that requests
     * involving reference counting are correctly using the
     * correct file contexts.
     */
    *context_id = sf_context->sf_context_id;

    /* Allocate and initialize space for worker threads in thread pool */
    world_size          = sf_context->topology->app_layout->world_size;
    pool_req_alloc_size = ((size_t)world_size * sizeof(struct hg_thread_work));

    if (!pool_request) {
        if (NULL == (pool_request = HDmalloc(pool_req_alloc_size)))
            H5FD_IOC_GOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL,
                                "couldn't allocate space for worker threads");

        pool_concurrent_max = world_size;
    }

    HDmemset(pool_request, 0, pool_req_alloc_size);

    /* Initialize a couple of mutex variables that are used
     * during IO concentrator operations to serialize
     * access to key objects, e.g. reference counting.
     */
    status = hg_thread_mutex_init(&ioc_mutex);
    if (status)
        H5FD_IOC_GOTO_ERROR(H5E_RESOURCE, H5E_CANTINIT, FAIL, "couldn't initialize IOC mutex");
    status = hg_thread_mutex_init(&ioc_thread_mutex);
    if (status)
        H5FD_IOC_GOTO_ERROR(H5E_RESOURCE, H5E_CANTINIT, FAIL, "couldn't initialize IOC thread mutex");

    status = hg_thread_mutex_init(&(io_queue_g.q_mutex));
    if (status)
        H5FD_IOC_GOTO_ERROR(H5E_RESOURCE, H5E_CANTINIT, FAIL, "couldn't initialize IOC thread queue mutex");

    /* Allow experimentation with the number of helper threads */
    if ((env_value = HDgetenv(H5_IOC_THREAD_POOL_COUNT)) != NULL) {
        int value_check = HDatoi(env_value);
        if (value_check > 0) {
            thread_pool_count = (unsigned int)value_check;
        }
    }

    /* Initialize a thread pool for the I/O Concentrator to use */
    status = hg_thread_pool_init(thread_pool_count, &ioc_thread_pool);
    if (status)
        H5FD_IOC_GOTO_ERROR(H5E_RESOURCE, H5E_CANTINIT, FAIL, "couldn't initialize IOC thread pool");

    /* Arguments to hg_thread_create are:
     * 1. A pointer to reference the created thread.
     * 2. User function pointer for the new thread to execute.
     * 3. Pointer to the input argument that gets passed along to the user
     * function.
     */
    atomic_init(&sf_ioc_ready, 0);
    status = hg_thread_create(&ioc_thread, ioc_thread_main, (void *)context_id);
    if (status)
        H5FD_IOC_GOTO_ERROR(H5E_RESOURCE, H5E_CANTINIT, FAIL, "couldn't create IOC main thread");

    /* Wait until ioc_main() reports that it is ready */
    while (atomic_load(&sf_ioc_ready) != 1) {
        usleep(20);
    }

#ifdef H5FD_IOC_DEBUG
    t_end = MPI_Wtime();
    if (sf_verbose_flag) {
        if (sf_context->topology->subfile_rank == 0) {
            HDprintf("%s: time = %lf seconds\n", __func__, (t_end - t_start));
            HDfflush(stdout);
        }
    }
#endif

done:
    H5FD_IOC_FUNC_LEAVE;
}

/*-------------------------------------------------------------------------
 * Function:    finalize_ioc_threads
 *
 * Purpose:     Normally we shouldn't have any IOC threads running by the
 *              program exits. If we do, this destructor function gets
 *              called to cleanup
 *
 * Return:      None
 *
 * Programmer:  Richard Warren
 *              7/17/2020
 *
 * Changes:     Initial Version/None.
 *
 *-------------------------------------------------------------------------
 */
void __attribute__((destructor)) finalize_ioc_threads(void)
{
    if (ioc_thread_pool != NULL) {
        hg_thread_pool_destroy(ioc_thread_pool);
        ioc_thread_pool = NULL;
    }
}

/*-------------------------------------------------------------------------
 * Function:    ioc_thread_main
 *
 * Purpose:     An IO Concentrator instance is initialized with the
 *              specified subfiling context.
 *
 * Return:      The IO concentrator thread executes as long as the HDF5
 *              file associated with this context is open.  At file close,
 *              the thread will return from 'ioc_main' and the thread
 *              exit status will be checked by the main program.
 *
 * Programmer:  Richard Warren
 *              7/17/2020
 *
 * Changes:     Initial Version/None.
 *
 *-------------------------------------------------------------------------
 */
static HG_THREAD_RETURN_TYPE
ioc_thread_main(void *arg)
{
    int64_t         context_id = *(int64_t *)arg;
    hg_thread_ret_t thread_ret = (hg_thread_ret_t)0;

    /* Pass along the subfiling_context_t */
    ioc_main(context_id);

    HDfree(arg);
    return thread_ret;
}

/*-------------------------------------------------------------------------
 * Function:    ioc_main
 *
 * Purpose:     This is the principal function run by the I/O Concentrator
 *              main thread.  It remains within a loop until allowed to
 *              exit by means of setting the 'sf_shutdown_flag'. This is
 *              usually accomplished as part of the file close operation.
 *
 *              The function implements an asynchronous polling approach
 *              for incoming messages. These messages can be thought of
 *              as a primitive RPC which utilizes MPI tags to code and
 *              implement the desired subfiling functionality.
 *
 *              As each incoming message is received, it gets added to
 *              a queue for processing by a thread_pool thread. The
 *              message handlers are dispatched via the
 *              "handle_work_request" routine.

 *              Subfiling is effectively a software RAID-0 implementation
 *              where having multiple I/O Concentrators and independent
 *              subfiles is equated to the multiple disks and a true
 *              hardware base RAID implementation.
 *
 *              I/O Concentrators are ordered according to their MPI rank.
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
static int
ioc_main(int64_t context_id)
{
    subfiling_context_t *context = NULL;
    sf_work_request_t    wk_req;
    MPI_Status           msg_status;
    int                  subfile_rank;
    int                  shutdown_requested;
    int                  ret_value = 0;

    context = H5_get_subfiling_object(context_id);
    HDassert(context);

    /* We can't have opened any files at this point..
     * The file open approach has changed so that the normal
     * application rank (hosting this thread) does the file open.
     * We can simply utilize the file descriptor (which should now
     * represent an open file).
     */

    subfile_rank = context->sf_group_rank;

    /* Initialize atomic vars */
    atomic_init(&sf_work_pending, 0);
    atomic_init(&sf_shutdown_flag, 0);

    /* this variable is incremented by ioc_io_queue_add_entry() when work
     * is added to the I/O request queue, and decremented by ioc_io_queue_complete_entry()
     * when an I/O request is completed and removed from the queue..
     *
     * On shutdown, we must wait until this field is decremented to zero before
     * taking down the thread pool.
     *
     * Note that this is a convenience variable -- we could use io_queue_g.q_len instead.
     * However, accessing this field requires locking io_queue_g.q_mutex.
     */
    atomic_init(&sf_io_ops_pending, 0);

    /* tell initialize_ioc_threads() that ioc_main() is ready to enter its main loop */
    atomic_init(&sf_ioc_ready, 1);

    shutdown_requested = 0;

    while ((!shutdown_requested) || (0 < atomic_load(&sf_io_ops_pending)) || (0 < atomic_load(&sf_work_pending))) {
        MPI_Message mpi_msg;
        MPI_Status  status;
        useconds_t  delay = 20;
        int         flag  = 0;
        int         mpi_code;

        /* Probe for incoming work requests */
        if (MPI_SUCCESS != (mpi_code = (MPI_Improbe(MPI_ANY_SOURCE, MPI_ANY_TAG, context->sf_msg_comm, &flag,
                                                    &mpi_msg, &status))))
            H5FD_IOC_MPI_GOTO_ERROR(-1, "MPI_Improbe failed", mpi_code);

        if (flag) {
            double queue_start_time;
            int    count;
            int    source = status.MPI_SOURCE;
            int    tag    = status.MPI_TAG;

            if ((tag != READ_INDEP) && (tag != WRITE_INDEP) && (tag != TRUNC_OP) && (tag != GET_EOF_OP))
                H5FD_IOC_GOTO_ERROR(H5E_ARGS, H5E_BADVALUE, -1, "invalid work request operation (%d)", tag);

            if (MPI_SUCCESS != (mpi_code = MPI_Get_count(&status, MPI_BYTE, &count)))
                H5FD_IOC_MPI_GOTO_ERROR(-1, "MPI_Get_count failed", mpi_code);

            if (count < 0)
                H5FD_IOC_GOTO_ERROR(H5E_ARGS, H5E_BADVALUE, -1, "invalid work request message size (%d)",
                                    count);

            if ((size_t)count > sizeof(sf_work_request_t))
                H5FD_IOC_GOTO_ERROR(H5E_ARGS, H5E_BADVALUE, -1, "work request message is too large (%d)",
                                    count);

            /*
             * Zero out work request, since the received message should
             * be smaller than sizeof(sf_work_request_t)
             */
            HDmemset(&wk_req, 0, sizeof(sf_work_request_t));

            if (MPI_SUCCESS != (mpi_code = MPI_Mrecv(&wk_req, count, MPI_BYTE, &mpi_msg, &msg_status)))
                H5FD_IOC_MPI_GOTO_ERROR(-1, "MPI_Mrecv failed", mpi_code);

#ifdef H5FD_IOC_DEBUG
            {
                int received_count;

                if (MPI_SUCCESS != (mpi_code = MPI_Get_count(&msg_status, MPI_BYTE, &received_count)))
                    H5FD_IOC_MPI_GOTO_ERROR(-1, "MPI_Get_count failed", mpi_code);

                if (received_count != count)
                    H5FD_IOC_GOTO_ERROR(H5E_ARGS, H5E_BADVALUE, -1, "MPI_Mrecv only received %d bytes of %d",
                                        received_count, count);
            }
#endif

            /* Dispatch work request to worker threads in thread pool */

            queue_start_time = MPI_Wtime();

            wk_req.tag          = tag;
            wk_req.source       = source;
            wk_req.subfile_rank = subfile_rank;
            wk_req.start_time   = queue_start_time;
            wk_req.buffer       = NULL;

            ioc_io_queue_add_entry(&wk_req);

            HDassert(atomic_load(&sf_io_ops_pending) >= 0);

            ioc_io_queue_dispatch_eligible_entries();
        }
        else {
            usleep(delay);
        }

        shutdown_requested = atomic_load(&sf_shutdown_flag);
    }

    /* Reset the shutdown flag */
    atomic_init(&sf_shutdown_flag, 0);

done:
    H5FD_IOC_FUNC_LEAVE;
} /* ioc_main() */

static const char *
translate_opcode(io_op_t op)
{
    switch (op) {
        case READ_OP:
            return "READ_OP";
            break;
        case WRITE_OP:
            return "WRITE_OP";
            break;
        case OPEN_OP:
            return "OPEN_OP";
            break;
        case CLOSE_OP:
            return "CLOSE_OP";
            break;
        case TRUNC_OP:
            return "TRUNC_OP";
            break;
        case GET_EOF_OP:
            return "GET_EOF_OP";
            break;
        case FINI_OP:
            return "FINI_OP";
            break;
        case LOGGING_OP:
            return "LOGGING_OP";
            break;
    }
    return "unknown";
}

/*-------------------------------------------------------------------------
 * Function:    handle_work_request
 *
 * Purpose:     Handle a work request from the thread pool work queue.
 *              We dispatch the specific function as indicated by the
 *              TAG that has been added to the work request by the
 *              IOC main thread (which is just a copy of the MPI tag
 *              associated with the RPC message) and provide the subfiling
 *              context associated with the HDF5 file.
 *
 *              Any status associated with the function processing is
 *              returned directly to the client via ACK or NACK messages.
 *
 * Return:      (none) Doesn't fail.
 *
 * Programmer:  Richard Warren
 *              7/17/2020
 *
 * Changes:     Initial Version/None.
 *
 *-------------------------------------------------------------------------
 */
static HG_THREAD_RETURN_TYPE
handle_work_request(void *arg)
{
    ioc_io_queue_entry_t *q_entry_ptr     = (ioc_io_queue_entry_t *)arg;
    subfiling_context_t * sf_context      = NULL;
    sf_work_request_t *   msg             = &(q_entry_ptr->wk_req);
    int64_t               file_context_id = msg->header[2];
    int                   op_ret;
#ifdef H5FD_IOC_DEBUG
    int curr_io_ops_pending;
#endif
    hg_thread_ret_t ret_value = 0;

    HDassert(q_entry_ptr);
    HDassert(q_entry_ptr->magic == H5FD_IOC__IO_Q_ENTRY_MAGIC);
    HDassert(q_entry_ptr->in_progress);

    sf_context = H5_get_subfiling_object(file_context_id);
    assert(sf_context != NULL);

    atomic_fetch_add(&sf_work_pending, 1);

    msg->in_progress = 1;

    switch (msg->tag) {
        case WRITE_INDEP:
            op_ret = ioc_file_queue_write_indep(msg, msg->subfile_rank, msg->source, sf_context->sf_data_comm,
                                                q_entry_ptr->counter);
            break;

        case READ_INDEP:
            op_ret = ioc_file_queue_read_indep(msg, msg->subfile_rank, msg->source, sf_context->sf_data_comm);
            break;

        case TRUNC_OP:
            op_ret = ioc_file_truncate(sf_context->sf_fid, q_entry_ptr->wk_req.header[0],
                                       sf_context->topology->subfile_rank);
            break;

        case GET_EOF_OP:
            /* Use of data comm to return EOF to the requesting rank seems a bit odd, but follow existing
             * convention for now.
             */
            op_ret = ioc_file_report_eof(msg, msg->subfile_rank, msg->source, sf_context->sf_data_comm);
            break;

        default:
#ifdef H5FD_IOC_DEBUG
            HDprintf("[ioc(%d)] received message tag(%x)from rank %d\n", msg->subfile_rank, msg->tag,
                     msg->source);
#endif
            op_ret = -1;
            break;
    }

    atomic_fetch_sub(&sf_work_pending, 1);

    if (op_ret < 0) {
#ifdef H5FD_IOC_DEBUG
        HDprintf("[ioc(%d) %s]: request(%s) filename=%s from "
                 "rank(%d), size=%ld, offset=%ld FAILED\n",
                 msg->subfile_rank, __func__, translate_opcode((io_op_t)msg->tag), sf_context->sf_filename,
                 msg->source, msg->header[0], msg->header[1]);
        HDfflush(stdout);
#endif

        /* TODO: set error value for work request queue entry */
        /* H5FD_IOC_GOTO_ERROR(H5E_IO, H5E_BADVALUE, 0, "work request (%s) operation from rank %d failed",
                               translate_opcode((io_op_t)msg->tag), msg->subfile_rank); */
    }

#ifdef H5FD_IOC_DEBUG
    curr_io_ops_pending = atomic_load(&sf_io_ops_pending);
    HDassert(curr_io_ops_pending > 0);
#endif

    /* complete the I/O request */
    ioc_io_queue_complete_entry(q_entry_ptr);

    HDassert(atomic_load(&sf_io_ops_pending) >= 0);

    /* Check the I/O Queue to see if there are any dispatchable entries */
    ioc_io_queue_dispatch_eligible_entries();

done:
    H5FD_IOC_FUNC_LEAVE;
}

void
ioc__wait_for_serialize(void *_work)
{
    sf_work_request_t *work    = (sf_work_request_t *)_work;
    volatile int       waiting = 1;
    while (waiting) {
        usleep(5);
        hg_thread_mutex_lock(&ioc_serialize_mutex);
        waiting = work->serialize;
        hg_thread_mutex_unlock(&ioc_serialize_mutex);
    }
}

void
ioc__release_dependency(int qid)
{
    sf_work_request_t *work = (sf_work_request_t *)pool_request[qid].args;
    hg_thread_mutex_lock(&ioc_serialize_mutex);
    work->serialize = 0;
    hg_thread_mutex_unlock(&ioc_serialize_mutex);
}

/*-------------------------------------------------------------------------
 * Function:    tpool_is_empty
 *
 * Purpose:     Utility function to indicate to the caller whether there
 *              is any remaining work in the thread pool queue.
 *
 * Return:      TRUE or FALSE to indicate whether the work queue is empty.
 *
 * Programmer:  Richard Warren
 *              7/17/2020
 *
 * Changes:     Initial Version/None.
 *
 *-------------------------------------------------------------------------
 */
bool
tpool_is_empty(void)
{
    return HG_QUEUE_IS_EMPTY(&ioc_thread_pool->queue);
}

/*-------------------------------------------------------------------------
 * Function:    begin_thread_exclusive
 *
 * Purpose:     Mutex lock to restrict access to code or variables.
 *
 * Return:      integer result of mutex_lock request.
 *
 * Programmer:  Richard Warren
 *              7/17/2020
 *
 * Changes:     Initial Version/None.
 *
 *-------------------------------------------------------------------------
 */
void
begin_thread_exclusive(void)
{
    hg_thread_mutex_lock(&ioc_thread_mutex);
}

/*-------------------------------------------------------------------------
 * Function:    end_thread_exclusive
 *
 * Purpose:     Mutex unlock.  Should only be called by the current holder
 *              of the locked mutex.
 *
 * Return:      result of mutex_unlock operation.
 *
 * Programmer:  Richard Warren
 *              7/17/2020
 *
 * Changes:     Initial Version/None.
 *
 *-------------------------------------------------------------------------
 */
void
end_thread_exclusive(void)
{
    hg_thread_mutex_unlock(&ioc_thread_mutex);
}

/*-------------------------------------------------------------------------
 * Function:    wait_for_thread_main
 *
 * Purpose:     Perform a thread_join on the IOC main thread.
 *
 * Return:      SUCCESS (0) or FAIL (-1) if the thread_join
 *              does not succeed.
 *
 * Programmer:  Richard Warren
 *              7/17/2020
 *
 * Changes:     Initial Version/None.
 *
 *-------------------------------------------------------------------------
 */
int
wait_for_thread_main(void)
{
    if (hg_thread_join(ioc_thread) != 0) {
        return -1;
    }
    return 0;
}

static herr_t
send_ack_to_client(int ack_val, int dest_rank, int source_rank, int msg_tag, MPI_Comm comm)
{
    int    mpi_code;
    herr_t ret_value = SUCCEED;

    HDassert(ack_val > 0);

    if (MPI_SUCCESS != (mpi_code = MPI_Send(&ack_val, 1, MPI_INT, dest_rank, msg_tag, comm)))
        H5FD_IOC_MPI_GOTO_ERROR(FAIL, "MPI_Send", mpi_code);

#ifdef H5FD_IOC_DEBUG
    if (sf_verbose_flag) {
        if (sf_logfile) {
            HDfprintf(sf_logfile, "[ioc(%d) %s]: Sent ACK(%d) to MPI rank %d\n", source_rank, __func__,
                      ack_val, dest_rank);
        }
    }
#else
    (void)source_rank;
#endif

done:
    H5FD_IOC_FUNC_LEAVE;
}

static herr_t
send_nack_to_client(int dest_rank, int source_rank, int msg_tag, MPI_Comm comm)
{
    int    nack = 0;
    int    mpi_code;
    herr_t ret_value = SUCCEED;

    if (MPI_SUCCESS != (mpi_code = MPI_Send(&nack, 1, MPI_INT, dest_rank, msg_tag, comm)))
        H5FD_IOC_MPI_GOTO_ERROR(FAIL, "MPI_Send", mpi_code);

#ifdef H5FD_IOC_DEBUG
    if (sf_verbose_flag) {
        if (sf_logfile) {
            HDfprintf(sf_logfile, "[ioc(%d) %s]: Sent NACK(%d) to MPI rank %d\n", source_rank, __func__, nack,
                      dest_rank);
        }
    }
#else
    (void)source_rank;
#endif

done:
    H5FD_IOC_FUNC_LEAVE;
}

/*
=========================================
queue_xxx functions that should be run
from the thread pool threads...
=========================================
*/

/*-------------------------------------------------------------------------
 * Function:    ioc_file_queue_write_indep
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
static int
ioc_file_queue_write_indep(sf_work_request_t *msg, int subfile_rank, int source, MPI_Comm comm,
                           uint32_t counter)
{
    subfiling_context_t *sf_context = NULL;
    MPI_Status           msg_status;
    uint32_t             rcv_tag;
    hbool_t              send_nack = FALSE;
    int64_t              data_size;
    int64_t              file_offset;
    int64_t              file_context_id;
    int64_t              stripe_id;
    haddr_t              sf_eof;
#ifdef H5FD_IOC_DEBUG
    double t_start;
    double t_end;
    double t_write;
    double t_wait;
    double t_queue_delay;
#endif
    char *recv_buf = NULL;
    int   sf_fid;
    int   data_bytes_received;
    int   write_ret;
    int   mpi_code;
    int   ret_value = 0;

    HDassert(msg);

    /* Retrieve the fields of the RPC message for the write operation */
    data_size       = msg->header[0];
    file_offset     = msg->header[1];
    file_context_id = msg->header[2];

    if (data_size < 0) {
        send_nack = TRUE;
        H5FD_IOC_GOTO_ERROR(H5E_IO, H5E_BADVALUE, -1, "invalid data size for write");
    }

    sf_context = H5_get_subfiling_object(file_context_id);
    HDassert(sf_context);

    stripe_id = file_offset + data_size;
    sf_eof    = (haddr_t)(stripe_id % sf_context->sf_stripe_size);

    stripe_id /= sf_context->sf_stripe_size;
    sf_eof += (haddr_t)((stripe_id * sf_context->sf_blocksize_per_stripe) + sf_context->sf_base_addr);

    /* Flag that we've attempted to write data to the file */
    sf_context->sf_write_count++;

#ifdef H5FD_IOC_DEBUG
    /* For debugging performance */
    sf_write_ops++;

    t_start       = MPI_Wtime();
    t_queue_delay = t_start - msg->start_time;
    if (sf_verbose_flag) {
        if (sf_logfile) {
            HDfprintf(sf_logfile,
                      "[ioc(%d) %s]: msg from %d: datasize=%ld\toffset=%ld, "
                      "queue_delay = %lf seconds\n",
                      subfile_rank, __func__, source, data_size, file_offset, t_queue_delay);
        }
    }
#endif

    /* Allocate space to receive data sent from the client */
    if (NULL == (recv_buf = HDmalloc((size_t)data_size))) {
        send_nack = TRUE;
        H5FD_IOC_GOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, -1, "couldn't allocate receive buffer for data");
    }

    /*
     * Calculate message tag for the client to use for sending
     * data, then send an ACK message to the client with the
     * calculated message tag. This calculated message tag
     * allows us to distinguish between multiple concurrent
     * writes from a single rank.
     */
    rcv_tag = ((counter & 0xFFFF) << 12) | WRITE_INDEP_DATA;

    H5_CHECK_OVERFLOW(rcv_tag, uint32_t, int);
    if (send_ack_to_client((int)rcv_tag, source, subfile_rank, WRITE_INDEP_ACK, comm) < 0)
        H5FD_IOC_GOTO_ERROR(H5E_IO, H5E_WRITEERROR, -1, "couldn't send ACK to client");

    /* Receive data from client */
    H5_CHECK_OVERFLOW(data_size, int64_t, int);
    if (MPI_SUCCESS !=
        (mpi_code = MPI_Recv(recv_buf, (int)data_size, MPI_BYTE, source, (int)rcv_tag, comm, &msg_status)))
        H5FD_IOC_MPI_GOTO_ERROR(-1, "MPI_Recv failed", mpi_code);

    if (MPI_SUCCESS != (mpi_code = MPI_Get_count(&msg_status, MPI_BYTE, &data_bytes_received)))
        H5FD_IOC_MPI_GOTO_ERROR(-1, "MPI_Get_count failed", mpi_code);

    if (data_bytes_received != data_size)
        H5FD_IOC_GOTO_ERROR(H5E_IO, H5E_WRITEERROR, -1,
                            "message size mismatch -- expected = %ld, actual = %d", data_size,
                            data_bytes_received);

#ifdef H5FD_IOC_DEBUG
    t_end  = MPI_Wtime();
    t_wait = t_end - t_start;
    sf_write_wait_time += t_wait;

    t_start = t_end;

    if (sf_verbose_flag) {
        if (sf_logfile) {
            HDfprintf(sf_logfile, "[ioc(%d) %s] MPI_Recv(%ld bytes, from = %d) status = %d\n", subfile_rank,
                      __func__, data_size, source, mpi_code);
        }
    }
#endif

    sf_fid = sf_context->sf_fid;

    if (sf_fid >= 0) {
        /* Actually write data received from client into subfile */
        if ((write_ret = ioc_file_write_data(sf_fid, file_offset, recv_buf, data_size, subfile_rank)) < 0)
            H5FD_IOC_GOTO_ERROR(H5E_IO, H5E_WRITEERROR, -1,
                                "write function(FID=%d, Source=%d) returned an error (%d)", sf_fid, source,
                                write_ret);

#ifdef H5FD_IOC_DEBUG
        t_end   = MPI_Wtime();
        t_write = t_end - t_start;
        sf_pwrite_time += t_write;
#endif
    }
    else {
        HDprintf("[ioc(%d) %s]: WARNING: attempt to write data to closed subfile FID %d\n", subfile_rank,
                 __func__, sf_fid);
    }

#ifdef H5FD_IOC_DEBUG
    sf_queue_delay_time += t_queue_delay;
#endif

    /* Adjust EOF if necessary */
    if (sf_eof > sf_context->sf_eof)
        sf_context->sf_eof = sf_eof;

done:
    if (send_nack) {
        /* Send NACK back to client so client can handle failure gracefully */
        if (send_nack_to_client(source, subfile_rank, WRITE_INDEP_ACK, comm) < 0)
            H5FD_IOC_DONE_ERROR(H5E_IO, H5E_WRITEERROR, -1, "couldn't send NACK to client");
    }

    HDfree(recv_buf);

    H5FD_IOC_FUNC_LEAVE;
} /* ioc_file_queue_write_indep() */

/*-------------------------------------------------------------------------
 * Function:    ioc_file_queue_read_indep
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
static int
ioc_file_queue_read_indep(sf_work_request_t *msg, int subfile_rank, int source, MPI_Comm comm)
{
    subfiling_context_t *sf_context     = NULL;
    hbool_t              send_empty_buf = FALSE;
    int64_t              data_size;
    int64_t              file_offset;
    int64_t              file_context_id;
#ifdef H5FD_IOC_DEBUG
    double t_start;
    double t_end;
    double t_read;
    double t_queue_delay;
#endif
    char *send_buf = NULL;
    int   sf_fid;
    int   read_ret;
    int   mpi_code;
    int   ret_value = 0;

    HDassert(msg);

    /* Retrieve the fields of the RPC message for the read operation */
    data_size       = msg->header[0];
    file_offset     = msg->header[1];
    file_context_id = msg->header[2];

    if (data_size < 0) {
        send_empty_buf = TRUE;
        H5FD_IOC_GOTO_ERROR(H5E_IO, H5E_BADVALUE, -1, "invalid data size for read");
    }

    sf_context = H5_get_subfiling_object(file_context_id);
    HDassert(sf_context);

    /* Flag that we've attempted to read data from the file */
    sf_context->sf_read_count++;

#ifdef H5FD_IOC_DEBUG
    /* For debugging performance */
    sf_read_ops++;

    t_start       = MPI_Wtime();
    t_queue_delay = t_start - msg->start_time;
    if (sf_verbose_flag && (sf_logfile != NULL)) {
        HDfprintf(sf_logfile,
                  "[ioc(%d) %s] msg from %d: datasize=%ld\toffset=%ld "
                  "queue_delay=%lf seconds\n",
                  subfile_rank, __func__, source, data_size, file_offset, t_queue_delay);
    }
#endif

    /* Allocate space to send data read from file to client */
    if (NULL == (send_buf = HDmalloc((size_t)data_size))) {
        send_empty_buf = TRUE;
        H5FD_IOC_GOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, -1, "couldn't allocate send buffer for data");
    }

    sf_fid = sf_context->sf_fid;
    if (sf_fid < 0) {
        send_empty_buf = TRUE;
        H5FD_IOC_GOTO_ERROR(H5E_IO, H5E_BADVALUE, -1, "subfile file descriptor %d is invalid", sf_fid);
    }

    /* Read data from the subfile */
    if ((read_ret = ioc_file_read_data(sf_fid, file_offset, send_buf, data_size, subfile_rank)) < 0) {
        send_empty_buf = TRUE;
        H5FD_IOC_GOTO_ERROR(H5E_IO, H5E_READERROR, -1,
                            "read function(FID=%d, Source=%d) returned an error (%d)", sf_fid, source,
                            read_ret);
    }

    /* Send read data to the client */
    H5_CHECK_OVERFLOW(data_size, int64_t, int);
    if (MPI_SUCCESS !=
        (mpi_code = MPI_Send(send_buf, (int)data_size, MPI_BYTE, source, READ_INDEP_DATA, comm)))
        H5FD_IOC_MPI_GOTO_ERROR(-1, "MPI_Send failed", mpi_code);

#ifdef H5FD_IOC_DEBUG
    t_end  = MPI_Wtime();
    t_read = t_end - t_start;
    sf_pread_time += t_read;
    sf_queue_delay_time += t_queue_delay;

    if (sf_verbose_flag && (sf_logfile != NULL)) {
        HDfprintf(sf_logfile, "[ioc(%d)] MPI_Send to source(%d) completed\n", subfile_rank, source);
    }
#endif

done:
    if (send_empty_buf) {
        /*
         * Send an empty message back to client on failure. The client will
         * likely get a message truncation error, but at least shouldn't hang.
         */
        if (MPI_SUCCESS != (mpi_code = MPI_Send(NULL, 0, MPI_BYTE, source, READ_INDEP_DATA, comm)))
            H5FD_IOC_MPI_DONE_ERROR(-1, "MPI_Send failed", mpi_code);
    }

    HDfree(send_buf);

    return ret_value;
} /* end ioc_file_queue_read_indep() */

/*
======================================================
File functions

The pread and pwrite posix functions are described as
being thread safe.
======================================================
*/

static int
ioc_file_write_data(int fd, int64_t file_offset, void *data_buffer, int64_t data_size, int subfile_rank)
{
    ssize_t bytes_remaining = (ssize_t)data_size;
    ssize_t bytes_written   = 0;
    char *  this_data       = (char *)data_buffer;
    int     ret_value       = 0;

#ifndef H5FD_IOC_DEBUG
    (void)subfile_rank;
#endif

    HDcompile_assert(H5_SIZEOF_OFF_T == sizeof(file_offset));

    while (bytes_remaining) {
        errno = 0;

        bytes_written = HDpwrite(fd, this_data, (size_t)bytes_remaining, file_offset);

        if (bytes_written >= 0) {
            bytes_remaining -= bytes_written;

#ifdef H5FD_IOC_DEBUG
            HDprintf("[ioc(%d) %s]: wrote %ld bytes, remaining=%ld, file_offset=%" PRId64 "\n", subfile_rank,
                     __func__, bytes_written, bytes_remaining, file_offset);
#endif

            this_data += bytes_written;
            file_offset += bytes_written;
        }
        else {
            H5FD_IOC_SYS_GOTO_ERROR(H5E_IO, H5E_WRITEERROR, -1, "HDpwrite failed");
        }
    }

    /* We don't usually use this for each file write.  We usually do the file
     * flush as part of file close operation.
     */
#ifdef H5FD_IOC_REQUIRE_FLUSH
    fdatasync(fd);
#endif

done:
    H5FD_IOC_FUNC_LEAVE;
} /* end ioc_file_write_data() */

static int
ioc_file_read_data(int fd, int64_t file_offset, void *data_buffer, int64_t data_size, int subfile_rank)
{
    useconds_t delay           = 100;
    ssize_t    bytes_remaining = (ssize_t)data_size;
    ssize_t    bytes_read      = 0;
    char *     this_buffer     = (char *)data_buffer;
    int        retries         = MIN_READ_RETRIES;
    int        ret_value       = 0;

#ifndef H5FD_IOC_DEBUG
    (void)subfile_rank;
#endif

    HDcompile_assert(H5_SIZEOF_OFF_T == sizeof(file_offset));

    while (bytes_remaining) {
        errno = 0;

        bytes_read = HDpread(fd, this_buffer, (size_t)bytes_remaining, file_offset);

        if (bytes_read > 0) {
            /* Reset retry params */
            retries = MIN_READ_RETRIES;
            delay   = 100;

            bytes_remaining -= bytes_read;

#ifdef H5FD_IOC_DEBUG
            HDprintf("[ioc(%d) %s]: read %ld bytes, remaining=%ld, file_offset=%" PRId64 "\n", subfile_rank,
                     __func__, bytes_read, bytes_remaining, file_offset);
#endif

            this_buffer += bytes_read;
            file_offset += bytes_read;
        }
        else if (bytes_read == 0) {
            if (retries == 0) {
#ifdef H5FD_IOC_DEBUG
                HDprintf("[ioc(%d) %s]: TIMEOUT: file_offset=%" PRId64 ", data_size=%ld\n", subfile_rank,
                         __func__, file_offset, data_size);
                HDprintf("[ioc(%d) %s]: ERROR! read of 0 bytes == eof!\n", subfile_rank, __func__);
#endif

                ret_value = -2;
                goto done;
            }

            retries--;
            usleep(delay);
            delay *= 2;
        }
        else {
            H5FD_IOC_SYS_GOTO_ERROR(H5E_IO, H5E_WRITEERROR, -1, "HDpread failed");
        }
    }

done:
    H5FD_IOC_FUNC_LEAVE;
} /* end ioc_file_read_data() */

static int
ioc_file_truncate(int fd, int64_t length, int subfile_rank)
{
    int ret_value = 0;

#ifndef H5FD_IOC_DEBUG
    (void)subfile_rank;
#endif

    if (HDftruncate(fd, (off_t)length) != 0)
        H5FD_IOC_SYS_GOTO_ERROR(H5E_FILE, H5E_SEEKERROR, -1, "HDftruncate failed");

#ifdef H5FD_IOC_DEBUG
    HDprintf("[ioc(%d) %s]: truncated subfile to %lld bytes. ret = %d\n", subfile_rank, __func__,
             (long long)length, errno);
    HDfflush(stdout);
#endif

done:
    H5FD_IOC_FUNC_LEAVE;
} /* end ioc_file_truncate() */

/*-------------------------------------------------------------------------
 * Function:    ioc_file_report_eof
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

static int
ioc_file_report_eof(sf_work_request_t *msg, int subfile_rank, int source, MPI_Comm comm)
{
    subfiling_context_t *sf_context = NULL;
    h5_stat_t            sb;
    int64_t              eof_req_reply[3];
    int64_t              file_context_id;
    int                  fd;
    int                  mpi_code;
    int                  ret_value = 0;

    HDassert(msg);

    /* first get the EOF of the target file. */

    file_context_id = msg->header[2];

    if (NULL == (sf_context = H5_get_subfiling_object(file_context_id)))
        H5FD_IOC_GOTO_ERROR(H5E_FILE, H5E_CANTGET, -1, "couldn't retrieve subfiling context");

    fd = sf_context->sf_fid;

    if (HDfstat(fd, &sb) < 0)
        H5FD_IOC_SYS_GOTO_ERROR(H5E_FILE, H5E_SYSERRSTR, -1, "HDfstat failed");

    eof_req_reply[0] = (int64_t)subfile_rank;
    eof_req_reply[1] = (int64_t)(sb.st_size);
    eof_req_reply[2] = 0; /* not used */

    /* return the subfile EOF to the querying rank */
    if (MPI_SUCCESS != (mpi_code = MPI_Send(eof_req_reply, 3, MPI_INT64_T, source, GET_EOF_COMPLETED, comm)))
        H5FD_IOC_MPI_GOTO_ERROR(-1, "MPI_Send", mpi_code);

done:
    H5FD_IOC_FUNC_LEAVE;
} /* ioc_file_report_eof() */

/*-------------------------------------------------------------------------
 * Function:    H5FD_ioc_take_down_thread_pool
 *
 * Purpose:     Destroy the thread pool if it exists.
 *
 *              This function should only be called on shutdown after all
 *              pending I/O operations have completed.
 *
 * Return:      void
 *
 * Programmer:  JRM -- 10/27/21
 *
 * Changes:     None.
 *
 *-------------------------------------------------------------------------
 */
void
H5FD_ioc_take_down_thread_pool(void)
{
    HDassert(0 == atomic_load(&sf_io_ops_pending));

    if (ioc_thread_pool != NULL) {
        hg_thread_pool_destroy(ioc_thread_pool);
        ioc_thread_pool = NULL;
    }

    return;
} /* H5FD_ioc_take_down_thread_pool() */

/*-------------------------------------------------------------------------
 * Function:    ioc_io_queue_alloc_entry
 *
 * Purpose:     Allocate and initialize an instance of
 *              ioc_io_queue_entry_t.  Return pointer to the new
 *              instance on success, and NULL on failure.
 *
 * Return:      Pointer to new instance of ioc_io_queue_entry_t
 *              on success, and NULL on failure.
 *
 * Programmer:  JRM -- 11/6/21
 *
 * Changes:     None.
 *
 *-------------------------------------------------------------------------
 */
/* TODO: update function when we decide how to handle error reporting in the IOCs */
static ioc_io_queue_entry_t *
ioc_io_queue_alloc_entry(void)
{
    ioc_io_queue_entry_t *q_entry_ptr = NULL;

    q_entry_ptr = (ioc_io_queue_entry_t *)HDmalloc(sizeof(ioc_io_queue_entry_t));

    if (q_entry_ptr) {

        q_entry_ptr->magic       = H5FD_IOC__IO_Q_ENTRY_MAGIC;
        q_entry_ptr->next        = NULL;
        q_entry_ptr->prev        = NULL;
        q_entry_ptr->in_progress = FALSE;
        q_entry_ptr->counter     = 0;

        /* will memcpy the wk_req field, so don't bother to initialize */
        /* will initialize thread_wk field before use */

#if H5FD_IOC__COLLECT_STATS
        q_entry_ptr->q_time        = 0;
        q_entry_ptr->dispatch_time = 0;
#endif /* H5FD_IOC__COLLECT_STATS */
    }

    return q_entry_ptr;
} /* ioc_io_queue_alloc_entry() */

/*-------------------------------------------------------------------------
 * Function:    ioc_io_queue_add_entry
 *
 * Purpose:     Add an I/O request to the tail of the IOC I/O Queue.
 *
 *              To do this, we must:
 *
 *              1) allocate a new instance of ioc_io_queue_entry_t
 *
 *              2) Initialize the new instance and copy the supplied
 *                 instance of sf_work_request_t into it.
 *
 *              3) Append it to the IOC I/O queue.
 *
 *              Note that this does not dispatch the request even if it
 *              is eligible for immediate dispatch.  This is done with
 *              a call to ioc_io_queue_dispatch_eligible_entries().
 *
 * Return:      void.
 *
 * Programmer:  JRM -- 11/7/21
 *
 * Changes:     None.
 *
 *-------------------------------------------------------------------------
 */
/* TODO: update function when we decide how to handle error reporting in the IOCs */
/* TODO: Update for per file I/O Queue */
static void
ioc_io_queue_add_entry(sf_work_request_t *wk_req_ptr)
{
    ioc_io_queue_entry_t *entry_ptr = NULL;

    HDassert(wk_req_ptr);
    HDassert(io_queue_g.magic == H5FD_IOC__IO_Q_MAGIC);

    entry_ptr = ioc_io_queue_alloc_entry();

    HDassert(entry_ptr);
    HDassert(entry_ptr->magic == H5FD_IOC__IO_Q_ENTRY_MAGIC);

    HDmemcpy((void *)(&(entry_ptr->wk_req)), (const void *)wk_req_ptr, sizeof(sf_work_request_t));

    /* must obtain io_queue_g mutex before appending */
    hg_thread_mutex_lock(&(io_queue_g.q_mutex));

    HDassert(io_queue_g.q_len == atomic_load(&sf_io_ops_pending));

    entry_ptr->counter = io_queue_g.req_counter++;

    io_queue_g.num_pending++;

    H5FD_IOC__Q_APPEND(&io_queue_g, entry_ptr);

    atomic_fetch_add(&sf_io_ops_pending, 1);

#ifdef H5FD_IOC_DEBUG
    HDfprintf(stdout,
              "\n\nioc_io_queue_add_entry: request %d queued. op = %d, offset/len = %lld/%lld, q-ed/disp/ops_pend = %d/%d/%d.\n",
              entry_ptr->counter, (entry_ptr->wk_req.tag), (long long)(entry_ptr->wk_req.header[1]),
              (long long)(entry_ptr->wk_req.header[0]), io_queue_g.num_pending, io_queue_g.num_in_progress,
              atomic_load(&sf_io_ops_pending));
    HDfflush(stdout);
#endif

    HDassert(io_queue_g.num_pending + io_queue_g.num_in_progress == io_queue_g.q_len);

#if H5FD_IOC__COLLECT_STATS

    entry_ptr->q_time = H5_now_usec();

    if (io_queue_g.q_len > io_queue_g.max_q_len) {

        io_queue_g.max_q_len = io_queue_g.q_len;
    }

    if (io_queue_g.num_pending > io_queue_g.max_num_pending) {

        io_queue_g.max_num_pending = io_queue_g.num_pending;
    }

    if (entry_ptr->wk_req.tag == READ_INDEP) {

        io_queue_g.ind_read_requests++;
    }
    else if (entry_ptr->wk_req.tag == WRITE_INDEP) {

        io_queue_g.ind_write_requests++;
    }
    else if (entry_ptr->wk_req.tag == TRUNC_OP) {

        io_queue_g.truncate_requests++;
    }
    else if (entry_ptr->wk_req.tag == GET_EOF_OP) {

        io_queue_g.get_eof_requests++;
    }

    io_queue_g.requests_queued++;

#endif /* H5FD_IOC__COLLECT_STATS */

#ifdef H5FD_IOC_DEBUG
    if ( io_queue_g.q_len != atomic_load(&sf_io_ops_pending) ) {

        HDfprintf(stdout, "\n\nioc_io_queue_add_entry: io_queue_g.q_len = %d != %d = atomic_load(&sf_io_ops_pending).\n\n",
                  io_queue_g.q_len, atomic_load(&sf_io_ops_pending));
        HDfflush(stdout);
    }
#endif

    HDassert(io_queue_g.q_len == atomic_load(&sf_io_ops_pending));

    hg_thread_mutex_unlock(&(io_queue_g.q_mutex));

    return;
} /* ioc_io_queue_add_entry() */

/*-------------------------------------------------------------------------
 * Function:    ioc_io_queue_dispatch_eligible_entries
 *
 * Purpose:     Scan the IOC I/O Queue for dispatchable entries, and
 *              dispatch any such entries found.
 *
 *              Do this by scanning the I/O queue from head to tail for
 *              entries that:
 *
 *              1) Have not already been dispatched
 *
 *              2) Either:
 *
 *                 a) do not intersect with any prior entries on the
 *                    I/O queue, or
 *
 *                 b) Are read requests, and all intersections are with
 *                    prior read requests.
 *
 *              Dispatch any such entries found.
 *
 *              Do this to maintain the POSIX semantics required by
 *              HDF5.
 *
 *              Note that TRUNC_OPs and GET_EOF_OPs are a special case.
 *              Specifically, no I/O queue entry can be dispatched if
 *              there is a truncate or get EOF operation between it and
 *              the head of the queue.  Further, a truncate or get EOF
 *              request cannot be executed unless it is at the head of
 *              the queue.
 *
 * Return:      void.
 *
 * Programmer:  JRM -- 11/7/21
 *
 * Changes:     None.
 *
 *-------------------------------------------------------------------------
 */
/* TODO: update function when we decide how to handle error reporting in the IOCs */
/* TODO: Update for per file I/O Queue */
/* TODO: Keep an eye on statistics and optimize this algorithm if necessary.  While it is O(N)
 *       where N is the number of elements in the I/O Queue if there are are no-overlaps, it
 *       can become O(N**2) in the worst case.
 */
static void
ioc_io_queue_dispatch_eligible_entries(void)
{
    hbool_t               conflict_detected;
    int64_t               entry_offset;
    int64_t               entry_len;
    int64_t               scan_offset;
    int64_t               scan_len;
    ioc_io_queue_entry_t *entry_ptr = NULL;
    ioc_io_queue_entry_t *scan_ptr  = NULL;

    hg_thread_mutex_lock(&(io_queue_g.q_mutex));

    HDassert(io_queue_g.magic == H5FD_IOC__IO_Q_MAGIC);

    entry_ptr = io_queue_g.q_head;

    /* sanity check on first element in the I/O queue */
    HDassert((entry_ptr == NULL) || (entry_ptr->prev == NULL));

    while ((entry_ptr) && (io_queue_g.num_pending > 0)) {

        HDassert(entry_ptr->magic == H5FD_IOC__IO_Q_ENTRY_MAGIC);

        if (!entry_ptr->in_progress) {

            entry_offset = entry_ptr->wk_req.header[1];
            entry_len    = entry_ptr->wk_req.header[0];

            conflict_detected = FALSE;

            scan_ptr = entry_ptr->prev;

            HDassert((scan_ptr == NULL) || (scan_ptr->magic == H5FD_IOC__IO_Q_ENTRY_MAGIC));

            if ((entry_ptr->wk_req.tag == TRUNC_OP) || (entry_ptr->wk_req.tag == GET_EOF_OP)) {

                if (scan_ptr != NULL) {

                    /* the TRUNC_OP or GET_EOF_OP is not at the head of the queue, and thus cannot
                     * be dispatched.  Further, no operation can be dispatched if a truncate request
                     * appears before it in the queue.  Thus we have done all we can and will break
                     * out of the loop.
                     */
                    break;
                }
            }

            while ((scan_ptr) && (!conflict_detected)) {

                /* check for overlaps */
                scan_offset = scan_ptr->wk_req.header[1];
                scan_len    = scan_ptr->wk_req.header[0];

                /* at present, I/O requests are scalar -- i.e. single blocks specified by offset and length.
                 * when this changes, this if statement will have to be updated accordingly.
                 */
                if (!(((scan_offset + scan_len) < entry_offset) ||
                      ((entry_offset + entry_len) < scan_offset))) {

                    /* the two request overlap -- unless they are both reads, we have detected a conflict */

                    /* TODO: update this if statement when we add collective I/O */
                    if ((entry_ptr->wk_req.tag != READ_INDEP) || (scan_ptr->wk_req.tag != READ_INDEP)) {

                        conflict_detected = TRUE;
                    }
                }

                scan_ptr = scan_ptr->prev;
            }

            if (!conflict_detected) { /* dispatch I/O request */

                HDassert(scan_ptr == NULL);
                HDassert(!entry_ptr->in_progress);

                entry_ptr->in_progress = TRUE;

                HDassert(io_queue_g.num_pending > 0);

                io_queue_g.num_pending--;
                io_queue_g.num_in_progress++;

                HDassert(io_queue_g.num_pending + io_queue_g.num_in_progress == io_queue_g.q_len);

                entry_ptr->thread_wk.func = handle_work_request;
                entry_ptr->thread_wk.args = entry_ptr;

#if H5FD_IOC__COLLECT_STATS
                if (io_queue_g.num_in_progress > io_queue_g.max_num_in_progress) {

                    io_queue_g.max_num_in_progress = io_queue_g.num_in_progress;
                }

                io_queue_g.requests_dispatched++;

#ifdef H5FD_IOC_DEBUG
                HDfprintf(stdout, 
"\n\nioc_io_queue_dispatch_eligible_entries: request %d dispatched. op = %d, offset/len = %lld/%lld, q-ed/disp/ops_pend = %d/%d/%d.\n",
                    entry_ptr->counter, (entry_ptr->wk_req.tag), (long long)(entry_ptr->wk_req.header[1]), 
                    (long long)(entry_ptr->wk_req.header[0]), io_queue_g.num_pending, io_queue_g.num_in_progress,
                    atomic_load(&sf_io_ops_pending));
                HDfflush(stdout);
#endif

                entry_ptr->dispatch_time = H5_now_usec();

#endif /* H5FD_IOC__COLLECT_STATS */

                hg_thread_pool_post(ioc_thread_pool, &(entry_ptr->thread_wk));
            }
        }
        else if ((entry_ptr->wk_req.tag == TRUNC_OP) || (entry_ptr->wk_req.tag == GET_EOF_OP)) {

            /* we have a truncate or get eof operation in progress -- thus no other operations
             * can be dispatched until the truncate or get eof operation completes.  Just break
             * out of the loop.
             */
            /* the truncate or get eof operation in progress must be at the head of the queue -- verify this
             */
            HDassert(entry_ptr->prev == NULL);

            break;
        }

        entry_ptr = entry_ptr->next;
    }

    HDassert(io_queue_g.q_len == atomic_load(&sf_io_ops_pending));

    hg_thread_mutex_unlock(&(io_queue_g.q_mutex));
} /* ioc_io_queue_dispatch_eligible_entries() */

/*-------------------------------------------------------------------------
 * Function:    ioc_io_queue_complete_entry
 *
 * Purpose:     Update the IOC I/O Queue for the completion of an I/O
 *              request.
 *
 *              To do this:
 *
 *              1) Remove the entry from the I/O Queue
 *
 *              2) If so configured, update statistics
 *
 *              3) Discard the instance of ioc_io_queue_entry_t.
 *
 * Return:      void.
 *
 * Programmer:  JRM -- 11/7/21
 *
 * Changes:     None.
 *
 *-------------------------------------------------------------------------
 */
/* TODO: update function when we decide how to handle error reporting in the IOCs */
/* TODO: Update for per file I/O Queue */
static void
ioc_io_queue_complete_entry(ioc_io_queue_entry_t *entry_ptr)
{
#if 0  /* H5FD_IOC__COLLECT_STATS */
    uint64_t queued_time;
    uint64_t execution_time;
#endif /* H5FD_IOC__COLLECT_STATS */

    HDassert(entry_ptr);
    HDassert(entry_ptr->magic == H5FD_IOC__IO_Q_ENTRY_MAGIC);

    /* must obtain io_queue_g mutex before deleting and updating stats */
    hg_thread_mutex_lock(&(io_queue_g.q_mutex));

    HDassert(io_queue_g.magic == H5FD_IOC__IO_Q_MAGIC);
    HDassert(io_queue_g.num_pending + io_queue_g.num_in_progress == io_queue_g.q_len);
    HDassert(io_queue_g.num_in_progress > 0);

    H5FD_IOC__Q_REMOVE(&io_queue_g, entry_ptr);

    io_queue_g.num_in_progress--;

    HDassert(io_queue_g.num_pending + io_queue_g.num_in_progress == io_queue_g.q_len);

    atomic_fetch_sub(&sf_io_ops_pending, 1);

#ifdef H5FD_IOC_DEBUG
    /*
     * If this I/O request is a truncate or "get eof" op, make sure
     * there aren't other operations in progress
     */
    if ((entry_ptr->wk_req.tag == GET_EOF_OP) || (entry_ptr->wk_req.tag == TRUNC_OP))
        HDassert(io_queue_g.num_in_progress == 0);

    HDfprintf(stdout,
           "ioc_io_queue_complete_entry: request %d completed. op = %d, offset/len = %lld/%lld, q-ed/disp/ops_pend = %d/%d/%d.\n",
              entry_ptr->counter, (entry_ptr->wk_req.tag), (long long)(entry_ptr->wk_req.header[1]),
              (long long)(entry_ptr->wk_req.header[0]), io_queue_g.num_pending, io_queue_g.num_in_progress,
              atomic_load(&sf_io_ops_pending));
    HDfflush(stdout);
#endif

    HDassert(io_queue_g.q_len == atomic_load(&sf_io_ops_pending));

#if H5FD_IOC__COLLECT_STATS
#if 0 /* no place to collect this yet */
    /* Compute the queued and execution time */
    queued_time = entry_ptr->dispatch_time - entry_ptr->q_time;
    execution_time = H5_now_usec() = entry_ptr->dispatch_time;
#endif

    io_queue_g.requests_completed++;

    entry_ptr->q_time = H5_now_usec();

#endif /* H5FD_IOC__COLLECT_STATS */

    hg_thread_mutex_unlock(&(io_queue_g.q_mutex));

    HDassert(entry_ptr->wk_req.buffer == NULL);

    ioc_io_queue_free_entry(entry_ptr);

    entry_ptr = NULL;

    return;
} /* ioc_io_queue_complete_entry() */

/*-------------------------------------------------------------------------
 * Function:    ioc_io_queue_free_entry
 *
 * Purpose:     Free the supplied instance of ioc_io_queue_entry_t.
 *
 *              Verify that magic field is set to
 *              H5FD_IOC__IO_Q_ENTRY_MAGIC, and that the next and prev
 *              fields are NULL.
 *
 * Return:      void.
 *
 * Programmer:  JRM -- 11/6/21
 *
 * Changes:     None.
 *
 *-------------------------------------------------------------------------
 */
/* TODO: update function when we decide how to handle error reporting in the IOCs */
static void
ioc_io_queue_free_entry(ioc_io_queue_entry_t *q_entry_ptr)
{
    /* use assertions for error checking, since the following should never fail. */
    HDassert(q_entry_ptr);
    HDassert(q_entry_ptr->magic == H5FD_IOC__IO_Q_ENTRY_MAGIC);
    HDassert(q_entry_ptr->next == NULL);
    HDassert(q_entry_ptr->prev == NULL);
    HDassert(q_entry_ptr->wk_req.buffer == NULL);

    q_entry_ptr->magic = 0;

    HDfree(q_entry_ptr);

    q_entry_ptr = NULL;

    return;
} /* H5FD_ioc__free_c_io_q_entry() */
