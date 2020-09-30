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

#include "H5FDsubfile_private.h"

/*
 * NOTES:
 * Rather than re-create the code for creating and managing a thread pool,
 * I'm utilizing a reasonably well tested implementation from the mercury
 * project.  At some point, we should revisit this decision or possibly
 * directly link against the mercury library.  This would make sense if
 * we move away from using MPI as the messaging infrastructure and instead
 * use mercury for that purpose...
 */

#include "mercury/mercury_log.c"
#include "mercury/mercury_log.h"
#include "mercury/mercury_thread.c"
#include "mercury/mercury_thread_condition.c"
#include "mercury/mercury_thread_condition.h"
#include "mercury/mercury_thread_mutex.c"
#include "mercury/mercury_thread_pool.c"
#include "mercury/mercury_thread_spin.c"
#include "mercury/mercury_util_config.h"
#include "mercury/mercury_util_error.c"

static hg_thread_mutex_t ioc_mutex = PTHREAD_MUTEX_INITIALIZER;
static hg_thread_mutex_t ioc_thread_mutex = PTHREAD_MUTEX_INITIALIZER;
static hg_thread_pool_t *ioc_thread_pool = NULL;
static hg_thread_t       ioc_thread;

#ifndef HG_TEST_NUM_THREADS_DEFAULT
#    define HG_TEST_NUM_THREADS_DEFAULT 4
#endif
#define POOL_CONCURRENT_MAX 256

static struct hg_thread_work pool_request[POOL_CONCURRENT_MAX];

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
    int64_t *       context_id = (int64_t *) arg;
    hg_thread_ret_t thread_ret = (hg_thread_ret_t) 0;

    /* Pass along the subfiling_context_t */
    ioc_main(context_id[0]);

    /* Upon exit, we can free the input arg */
    free(arg);
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
initialize_ioc_threads(subfiling_context_t *sf_context)
{
    int      status;
	unsigned int thread_pool_count = HG_TEST_NUM_THREADS_DEFAULT;
    int64_t *context_id = (int64_t *) malloc(sizeof(int64_t));
	char    *envValue;
    assert(context_id != NULL);
    /* Initialize the main IOC thread input argument.
     * Each IOC request will utilize this context_id which is
     * consistent across all MPI ranks, to ensure that requests
     * involving reference counting are correctly using the
     * correct file contexts.
     */
    context_id[0] = sf_context->sf_context_id;

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
    status = hg_thread_create(&ioc_thread, ioc_thread_main, context_id);
    if (status) {
        puts("hg_thread_create failed");
        goto err_exit;
    }
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
    int                  status = 0;
    hg_thread_ret_t      ret = 0;
    sf_work_request_t *  msg = (sf_work_request_t *) arg;
    int64_t              file_context_id = msg->header[2];
    subfiling_context_t *sf_context = get_subfiling_object(file_context_id);
    assert(sf_context != NULL);

    atomic_fetch_add(&sf_work_pending, 1); // atomic
    switch (msg->tag) {
        case WRITE_COLL:
            status = queue_write_coll(
                msg, msg->subfile_rank, msg->source, sf_context->sf_data_comm);
            break;
        case READ_COLL:
            status = queue_read_coll(
                msg, msg->subfile_rank, msg->source, sf_context->sf_data_comm);
            break;
        case WRITE_INDEP:
            status = queue_write_indep(
                msg, msg->subfile_rank, msg->source, sf_context->sf_data_comm);
            break;
        case READ_INDEP:
            status = queue_read_indep(
                msg, msg->subfile_rank, msg->source, sf_context->sf_data_comm);
            break;
        case CLOSE_OP:
            status = decrement_file_ref_counts(msg, msg->subfile_rank,
                msg->source, sf_context->sf_data_comm, subfiling_close_file);
            break;
        case OPEN_OP:
            status = queue_file_open(
                msg, msg->subfile_rank, msg->source, sf_context->sf_data_comm);
            break;
        case FINI_OP:
            status = increment_ioc_fini_counts(msg, msg->subfile_rank,
                msg->source, sf_context->sf_data_comm, subfiling_shutdown);
            break;
        default:
            printf("[ioc(%d)] received message tag(%x)from rank %d\n",
                msg->subfile_rank, msg->tag, msg->source);
            status = -1;
            break;
    }

    atomic_fetch_sub(&sf_work_pending, 1); // atomic
    if (status < 0) {
        printf("[ioc(%d) %s]: Error encounted processing request(%x) from "
               "rank(%d)\n",
            msg->subfile_rank, __func__, msg->tag, msg->source);
        fflush(stdout);
    }
    return ret;
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
tpool_add_work(sf_work_request_t *work)
{
    static int work_index = 0;
    hg_thread_mutex_lock(&ioc_mutex);
    if (work_index == POOL_CONCURRENT_MAX)
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
int
begin_thread_exclusive(void)
{
    return hg_thread_mutex_lock(&ioc_thread_mutex);
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
int
end_thread_exclusive(void)
{
    return hg_thread_mutex_unlock(&ioc_thread_mutex);
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
