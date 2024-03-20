/**
 * Copyright (c) 2013-2022 UChicago Argonne, LLC and The HDF Group.
 * Copyright (c) 2022-2023 Intel Corporation.
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#ifndef MERCURY_THREAD_POOL_H
#define MERCURY_THREAD_POOL_H

#include "mercury_queue.h"
#include "mercury_util_config.h"

/*************************************/
/* Public Type and Struct Definition */
/*************************************/

typedef struct hg_thread_pool hg_thread_pool_t;

struct hg_thread_pool {
    unsigned int sleeping_worker_count;
    HG_QUEUE_HEAD(hg_thread_work) queue;
    int               shutdown;
    H5TS_mutex_t mutex;
    H5TS_cond_t  cond;
};

struct hg_thread_work {
    H5TS_thread_start_func_t func;
    void            *args;
    HG_QUEUE_ENTRY(hg_thread_work) entry; /* Internal */
};

/*****************/
/* Public Macros */
/*****************/

/*********************/
/* Public Prototypes */
/*********************/

#ifdef __cplusplus
extern "C" {
#endif

/**
 * Initialize the thread pool.
 *
 * \param thread_count [IN]     number of threads that will be created at
 *                              initialization
 * \param pool [OUT]            pointer to pool object
 *
 * \return Non-negative on success or negative on failure
 */
int hg_thread_pool_init(unsigned int thread_count, hg_thread_pool_t **pool);

/**
 * Destroy the thread pool.
 *
 * \param pool [IN/OUT]         pointer to pool object
 *
 * \return Non-negative on success or negative on failure
 */
int hg_thread_pool_destroy(hg_thread_pool_t *pool);

/**
 * Post work to the pool. Note that the operation may be queued depending on
 * the number of threads and number of tasks already running.
 *
 * \param pool [IN/OUT]         pointer to pool object
 * \param work [IN]             pointer to work struct
 *
 * \return Non-negative on success or negative on failure
 */
static inline int hg_thread_pool_post(hg_thread_pool_t *pool, struct hg_thread_work *work);

/*---------------------------------------------------------------------------*/
static inline int
hg_thread_pool_post(hg_thread_pool_t *pool, struct hg_thread_work *work)
{
    int ret = HG_UTIL_SUCCESS;

    if (!pool || !work)
        return HG_UTIL_FAIL;

    if (!work->func)
        return HG_UTIL_FAIL;

    H5TS_mutex_lock(&pool->mutex);

    /* Are we shutting down ? */
    if (pool->shutdown) {
        ret = HG_UTIL_FAIL;
        goto unlock;
    }

    /* Add task to task queue */
    HG_QUEUE_PUSH_TAIL(&pool->queue, work, entry);

    /* Wake up sleeping worker */
    if (pool->sleeping_worker_count && (H5TS_cond_signal(&pool->cond) != HG_UTIL_SUCCESS))
        ret = HG_UTIL_FAIL;

unlock:
    H5TS_mutex_unlock(&pool->mutex);

    return ret;
}

#ifdef __cplusplus
}
#endif

#endif /* MERCURY_THREAD_POOL_H */
