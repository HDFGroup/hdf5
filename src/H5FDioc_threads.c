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

#include "H5FDsubfiling.h"
#include "mercury_thread.h"
#include "mercury_thread_mutex.h"
#include "mercury_thread_pool.h"

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

#ifndef HG_TEST_NUM_THREADS_DEFAULT
#define HG_TEST_NUM_THREADS_DEFAULT 4
#endif

extern int ioc_main(int64_t context_id);

static int                    pool_concurrent_max = 0;
static struct hg_thread_work *pool_request        = NULL;

/* Prototypes */
void __attribute__((destructor)) finalize_ioc_threads(void);
int  wait_for_thread_main(void);
bool tpool_is_empty(void);

/*-------------------------------------------------------------------------
 * Function:    local ioc_thread_main
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
 * Function:    initialize_ioc_threads
 *
 * Purpose:     The principal entry point to initialize the execution
 *              context for an IO Concentrator (IOC). The main thread
 *              is responsible for receiving IO requests from each
 *              HDF5 "client" and distibuting those to helper threads
 *              for actual processing.  We initialize a fixed number
 *              of helper threads by creating a thread_pool.
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
    int                  status;
    int                  file_open_count;
    subfiling_context_t *sf_context        = _sf_context;
    unsigned int         thread_pool_count = HG_TEST_NUM_THREADS_DEFAULT;
    int64_t *            context_id        = (int64_t *)HDmalloc(sizeof(int64_t));
    int                  world_size        = sf_context->topology->app_layout->world_size;
    size_t               alloc_size        = ((size_t)world_size * sizeof(struct hg_thread_work));
    char *               envValue;
    double               t_start = 0.0, t_end = 0.0;

    assert(context_id != NULL);

    file_open_count = atomic_load(&sf_file_open_count);
    atomic_fetch_add(&sf_file_open_count, 1);

    if (file_open_count > 0)
        return 0;

    t_start = MPI_Wtime();

    /* Initialize the main IOC thread input argument.
     * Each IOC request will utilize this context_id which is
     * consistent across all MPI ranks, to ensure that requests
     * involving reference counting are correctly using the
     * correct file contexts.
     */
    context_id[0] = sf_context->sf_context_id;

    if (pool_request == NULL) {
        if ((pool_request = (struct hg_thread_work *)malloc(alloc_size)) == NULL) {
            perror("malloc error");
            return -1;
        }
        else
            pool_concurrent_max = world_size;
    }

    memset(pool_request, 0, alloc_size);

    /* Initialize a couple of mutex variables that are used
     * during IO concentrator operations to serialize
     * access to key objects, e.g. reference counting.
     */
    status = hg_thread_mutex_init(&ioc_mutex);
    if (status) {
        puts("hg_thread_mutex_init failed");
        goto err_exit;
    }
    status = hg_thread_mutex_init(&ioc_thread_mutex);
    if (status) {
        puts("hg_thread_mutex_init failed");
        goto err_exit;
    }

    /* Allow experimentation with the number of helper threads */
    if ((envValue = getenv("IOC_THREAD_POOL_COUNT")) != NULL) {
        int value_check = atoi(envValue);
        if (value_check > 0) {
            thread_pool_count = (unsigned int)value_check;
        }
    }

    /* Initialize a thread pool for the IO Concentrator to use */
    status = hg_thread_pool_init(thread_pool_count, &ioc_thread_pool);
    if (status) {
        puts("hg_thread_pool_init failed");
        goto err_exit;
    }

    /* Arguments to hg_thread_create are:
     * 1. A pointer to reference the created thread.
     * 2. User function pointer for the new thread to execute.
     * 3. Pointer to the input argument that gets passed along to the user
     * function.
     */
    status = hg_thread_create(&ioc_thread, ioc_thread_main, (void *)context_id);
    if (status) {
        puts("hg_thread_create failed");
        goto err_exit;
    }

#ifndef NDEBUG
    t_end = MPI_Wtime();
    if (sf_verbose_flag) {
        if (sf_context->topology->subfile_rank == 0) {
            HDprintf("%s: time = %lf seconds\n", __func__, (t_end - t_start));
            HDfflush(stdout);
        }
    }
#endif
    return 0;

err_exit:
    return -1;
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
 * Function:    local: handle_work_request
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
    int                  status          = 0;
    hg_thread_ret_t      ret             = 0;
    sf_work_request_t *  msg             = (sf_work_request_t *)arg;
    int64_t              file_context_id = msg->header[2];
    subfiling_context_t *sf_context      = NULL;

    sf_context = get__subfiling_object(file_context_id);
    assert(sf_context != NULL);

    atomic_fetch_add(&sf_work_pending, 1); // atomic
    msg->in_progress = 1;
    switch (msg->tag) {
        case WRITE_INDEP:
            status = queue_write_indep(msg, msg->subfile_rank, msg->source, sf_context->sf_data_comm);
            break;
        case READ_INDEP:
            if (msg->serialize)
                ioc__wait_for_serialize(arg); // wait for dependency
            status = queue_read_indep(msg, msg->subfile_rank, msg->source, sf_context->sf_data_comm);
            break;
        default:
            HDprintf("[ioc(%d)] received message tag(%x)from rank %d\n", msg->subfile_rank, msg->tag,
                     msg->source);
            status = -1;
            break;
    }
    fflush(stdout);

    atomic_fetch_sub(&sf_work_pending, 1); // atomic
    msg->in_progress = 0;
    if (msg->dependents) {
        ioc__release_dependency(msg->depend_id);
        msg->dependents = 0;
    }
    if (status < 0) {
        HDprintf("[ioc(%d) %s]: request(%s) filename=%s from "
                 "rank(%d), size=%ld, offset=%ld FAILED\n",
                 msg->subfile_rank, __func__, translate_opcode((io_op_t)msg->tag), sf_context->sf_filename,
                 msg->source, msg->header[0], msg->header[1]);

        fflush(stdout);
    }
    return ret;
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

static int
check__overlap(void *_work, int current_index, int *conflict_id)
{
    sf_work_request_t *work = (sf_work_request_t *)_work;
    sf_work_request_t *next = NULL;
    int                index, count = 0;
    /* Search backward thru the queue of work requests */

    for (index = current_index; count < pool_concurrent_max; count++, index--) {
        if (index == 0) {
            index = pool_concurrent_max - 1;
        }
        if (index == current_index)
            return 0;
        if ((next = (sf_work_request_t *)(pool_request[index].args)) == NULL)
            continue;
        /* The queued operation need NOT be running at present... */
        else /* if (next->in_progress) */ {
            if (work->tag == WRITE_INDEP) {
                /* a WRITE should not overlap with anything else */
                int64_t n_data_size  = next->header[0];
                int64_t n_offset     = next->header[1];
                int64_t n_max_offset = (n_offset + n_data_size) - 1;
                int64_t w_data_size  = work->header[0];
                int64_t w_offset     = work->header[1];
                int64_t w_max_offset = (w_offset + w_data_size) - 1;
                if ((w_max_offset >= n_offset) && (w_max_offset < n_max_offset)) {
                    next->dependents = 1;
                    next->depend_id  = current_index;
                    work->serialize  = true;
                    *conflict_id     = index;
                    return 1;
                }
                else if ((w_offset <= n_max_offset) && (w_offset > n_offset)) {
                    next->dependents = 1;
                    next->depend_id  = current_index;
                    work->serialize  = true;
                    *conflict_id     = index;
                    return 1;
                }
            }
            /* The work->tag indicates READ, so only check for a conflicting WRITE */
            else if (next->tag == WRITE_INDEP) {
                int64_t n_data_size  = next->header[0];
                int64_t n_offset     = next->header[1];
                int64_t n_max_offset = (n_offset + n_data_size) - 1;
                int64_t w_data_size  = work->header[0];
                int64_t w_offset     = work->header[1];
                int64_t w_max_offset = (w_offset + w_data_size) - 1;
                if ((w_max_offset >= n_offset) && (w_max_offset < n_max_offset)) {
                    next->dependents = 1;
                    next->depend_id  = current_index;
                    work->serialize  = true;
                    *conflict_id     = index;
                    return 1;
                }
                else if ((w_offset <= n_max_offset) && (w_offset > n_offset)) {
                    next->dependents = 1;
                    next->depend_id  = current_index;
                    work->serialize  = true;
                    *conflict_id     = index;
                    return 1;
                }
            }
        }
    }
    return 0;
}

/*-------------------------------------------------------------------------
 * Function:    tpool_add_work
 *
 * Purpose:     Initiate the handoff of client request processing to a
 *              thread in the thread pool.  A work request is created and
 *              added to the thread pool work queue.  Once
 *
 * Return:      result of: (hostid1 > hostid2)
 *
 * Programmer:  Richard Warren
 *              7/17/2020
 *
 * Changes:     Initial Version/None.
 *
 *-------------------------------------------------------------------------
 */
int
tpool_add_work(void *_work)
{
    static int         work_index  = 0;
    int                conflict_id = -1;
    sf_work_request_t *work        = (sf_work_request_t *)_work;
    /* We have yet to start processing this new request... */
    work->in_progress = 0;
    hg_thread_mutex_lock(&ioc_mutex);
    if (check__overlap(_work, work_index, &conflict_id) > 0) {
        const char *       type = (work->tag == WRITE_INDEP ? "WRITE" : "READ");
        sf_work_request_t *next = (sf_work_request_t *)(pool_request[conflict_id].args);
        printf("%s - (%d) Found conflict: index=%d: work(offset=%ld,length=%ld) conflict(offset=%ld, "
               "length=%ld)\n",
               type, work_index, conflict_id, work->header[1], work->header[0], next->header[1],
               next->header[0]);
        fflush(stdout);
#if 0
      add_delayed(work, conflict_id);
      hg_thread_mutex_unlock(&ioc_mutex);
      return 0;
#endif
    }

    if (work_index == pool_concurrent_max)
        work_index = 0;

    pool_request[work_index].func = handle_work_request;
    pool_request[work_index].args = work;
    hg_thread_pool_post(ioc_thread_pool, &pool_request[work_index++]);
    hg_thread_mutex_unlock(&ioc_mutex);
    return 0;
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
