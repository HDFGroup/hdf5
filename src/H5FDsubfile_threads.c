
#include "H5FDsubfile_private.h"

#include "mercury/mercury_util_config.h"
#include "mercury/mercury_log.h"
#include "mercury/mercury_log.c"
#include "mercury/mercury_util_error.c"
#include "mercury/mercury_thread.c"
#include "mercury/mercury_thread_mutex.c"
#include "mercury/mercury_thread_condition.h"
#include "mercury/mercury_thread_condition.c"
#include "mercury/mercury_thread_pool.c"
#include "mercury/mercury_thread_spin.c"

static hg_thread_mutex_t ioc_mutex = PTHREAD_MUTEX_INITIALIZER;
static hg_thread_pool_t *ioc_thread_pool = NULL;
static hg_thread_t ioc_thread;

#define HG_TEST_NUM_THREADS_DEFAULT 4
#define POOL_CONCURRENT_MAX 64

static struct hg_thread_work pool_request[POOL_CONCURRENT_MAX];

static HG_THREAD_RETURN_TYPE
ioc_thread_main(void *arg)
{
    hg_thread_ret_t thread_ret = (hg_thread_ret_t) 0;

	/* Pass along the subfiling_context_t */
	ioc_main(arg);

    // hg_thread_exit(thread_ret);
    return thread_ret;
}

int
initialize_ioc_threads(subfiling_context_t *sf_context)
{
	int status;
	status = hg_thread_mutex_init(&ioc_mutex);
	if (status) {
		puts("hg_thread_mutex_init failed");
		goto err_exit;
	}
	status = hg_thread_pool_init(HG_TEST_NUM_THREADS_DEFAULT, &ioc_thread_pool);
	if (status) {
		puts("hg_thread_pool_init failed");
		goto err_exit;
	}
	status = hg_thread_create(&ioc_thread, ioc_thread_main, sf_context);
	if (status) {
		puts("hg_thread_create failed");
		goto err_exit;
	}
	return 0;

err_exit:
	return -1;
}


void __attribute__((destructor)) finalize_ioc_threads()
{
	if (ioc_thread_pool != NULL) {
		hg_thread_pool_destroy(ioc_thread_pool);
		ioc_thread_pool = NULL;

		if (hg_thread_join(ioc_thread) == 0)
			puts("thread_join succeeded");
		else puts("thread_join failed");
	}
}


static HG_THREAD_RETURN_TYPE
handle_work_request(void *arg)
{
    hg_thread_ret_t ret = 0;
    sf_work_request_t *msg = (sf_work_request_t *)arg;
    int status = 0;

	atomic_fetch_add(&sf_work_pending, 1); // atomic
	switch(msg->tag) {
	case WRITE_COLL:
            status = queue_write_coll( msg, msg->subfile_rank, msg->source, sf_data_comm);
            break;
	case READ_COLL:
            status = queue_read_coll( msg, msg->subfile_rank, msg->source, sf_data_comm);
            break;
	case WRITE_INDEP:
            status = queue_write_indep( msg, msg->subfile_rank, msg->source, sf_data_comm);
            break;
	case READ_INDEP:
            status = queue_read_indep( msg, msg->subfile_rank, msg->source, sf_data_comm);
            break;
	case CLOSE_OP:
            hg_thread_mutex_lock(&ioc_mutex);
            status = decrement_file_ref_counts( msg->subfile_rank, msg->source, sf_data_comm,
												subfiling_close_file);
            hg_thread_mutex_unlock(&ioc_mutex);
            break;
	case OPEN_OP:
            status = queue_file_open( msg, msg->subfile_rank, msg->source, sf_data_comm);
            break;

	default:
            printf("[ioc(%d)] received message tag(%x)from rank %d\n", msg->subfile_rank, msg->tag, msg->source);
            status = -1;
            break;
    }
	
	atomic_fetch_sub(&sf_work_pending, 1); // atomic
    if (status < 0) {
        printf("[ioc(%d) %s]: Error encounted processing request(%x) from rank(%d\n",
               msg->subfile_rank, __func__, msg->tag, msg->source);
		fflush(stdout);
    }
	return ret;
}

int tpool_add_work(sf_work_request_t *work)
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
