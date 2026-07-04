/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the LICENSE file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/********************************************************************
 *
 * Test the correctness of the thread pool routines
 *
 ********************************************************************/

#include "ttsafe.h"

#ifdef H5_HAVE_THREADS

#define NUM_THREADS 16

typedef struct {
    H5TS_mutex_t mutex;
    int          val;
} atomic_counter_t;

static atomic_counter_t counter_g;

static H5TS_THREAD_RETURN_TYPE
noop_task(void *_counter)
{
    atomic_counter_t *counter = (atomic_counter_t *)_counter;
    herr_t            result;
    H5TS_thread_ret_t ret_value = 0;

    result = H5TS_mutex_lock(&counter->mutex);
    CHECK_I(result, "H5TS_mutex_lock");

    VERIFY(counter->val, 0, "noop_task");

    result = H5TS_mutex_unlock(&counter->mutex);
    CHECK_I(result, "H5TS_mutex_unlock");

    return ret_value;
}

static H5TS_THREAD_RETURN_TYPE
incr_task(void *_counter)
{
    atomic_counter_t *counter = (atomic_counter_t *)_counter;
    herr_t            result;
    H5TS_thread_ret_t ret_value = 0;

    result = H5TS_mutex_lock(&counter->mutex);
    CHECK_I(result, "H5TS_mutex_lock");

    /* Increment value */
    counter->val++;

    result = H5TS_mutex_unlock(&counter->mutex);
    CHECK_I(result, "H5TS_mutex_unlock");

    return ret_value;
}

static H5TS_THREAD_RETURN_TYPE
decr_task(void *_counter)
{
    atomic_counter_t *counter = (atomic_counter_t *)_counter;
    herr_t            result;
    H5TS_thread_ret_t ret_value = 0;

    result = H5TS_mutex_lock(&counter->mutex);
    CHECK_I(result, "H5TS_mutex_lock");

    /* Decrement value */
    counter->val--;

    result = H5TS_mutex_unlock(&counter->mutex);
    CHECK_I(result, "H5TS_mutex_unlock");

    return ret_value;
}

/*
 **********************************************************************
 * tts_thread_pool
 *
 **********************************************************************
 */
void
tts_thread_pool(void H5_ATTR_UNUSED *params)
{
    H5TS_pool_t *pool = NULL;
    herr_t       result;

    /* Initialize the counter */
    result = H5TS_mutex_init(&counter_g.mutex, H5TS_MUTEX_TYPE_PLAIN);
    CHECK_I(result, "H5TS_mutex_init");
    counter_g.val = 0;

    /* Sanity checks on bad input */
    result = H5TS_pool_create(NULL, NUM_THREADS);
    VERIFY(result, FAIL, "H5TS_pool_create");
    result = H5TS_pool_create(&pool, 0);
    VERIFY(result, FAIL, "H5TS_pool_create");
    result = H5TS_pool_add_task(NULL, noop_task, NULL);
    VERIFY(result, FAIL, "H5TS_pool_add_task");
    result = H5TS_pool_add_task(pool, NULL, NULL);
    VERIFY(result, FAIL, "H5TS_pool_add_task");
    result = H5TS_pool_destroy(NULL);
    VERIFY(result, FAIL, "H5TS_pool_destroy");

    /* Create & destroy empty pool */
    result = H5TS_pool_create(&pool, NUM_THREADS);
    CHECK_I(result, "H5TS_pool_create");

    result = H5TS_pool_destroy(pool);
    CHECK_I(result, "H5TS_pool_destroy");

    /* Create pool, add single 'noop' task, destroy pool */
    result = H5TS_pool_create(&pool, NUM_THREADS);
    CHECK_I(result, "H5TS_pool_create");

    result = H5TS_pool_add_task(pool, noop_task, &counter_g);
    CHECK_I(result, "H5TS_pool_add_task");

    result = H5TS_pool_destroy(pool);
    CHECK_I(result, "H5TS_pool_destroy");

    VERIFY(counter_g.val, 0, "noop");

    /* Create pool, add single 'incr' task, destroy pool */
    result = H5TS_pool_create(&pool, NUM_THREADS);
    CHECK_I(result, "H5TS_pool_create");

    result = H5TS_pool_add_task(pool, incr_task, &counter_g);
    CHECK_I(result, "H5TS_pool_add_task");

    result = H5TS_pool_destroy(pool);
    CHECK_I(result, "H5TS_pool_destroy");

    VERIFY(counter_g.val, 1, "single incr");

    /* Create pool, add pair of 'incr' & 'decr' tasks, destroy pool */
    counter_g.val = 10;
    result        = H5TS_pool_create(&pool, NUM_THREADS);
    CHECK_I(result, "H5TS_pool_create");

    result = H5TS_pool_add_task(pool, incr_task, &counter_g);
    CHECK_I(result, "H5TS_pool_add_task");

    result = H5TS_pool_add_task(pool, decr_task, &counter_g);
    CHECK_I(result, "H5TS_pool_add_task");

    result = H5TS_pool_destroy(pool);
    CHECK_I(result, "H5TS_pool_destroy");

    VERIFY(counter_g.val, 10, "incr + decr");

    /* Create pool, add many 'incr' & 'decr' tasks, destroy pool */
    counter_g.val = 3;
    result        = H5TS_pool_create(&pool, NUM_THREADS);
    CHECK_I(result, "H5TS_pool_create");

    for (unsigned u = 0; u < 200; u++) {
        result = H5TS_pool_add_task(pool, incr_task, &counter_g);
        CHECK_I(result, "H5TS_pool_add_task");
    }

    for (unsigned u = 0; u < 100; u++) {
        result = H5TS_pool_add_task(pool, decr_task, &counter_g);
        CHECK_I(result, "H5TS_pool_add_task");
    }

    result = H5TS_pool_destroy(pool);
    CHECK_I(result, "H5TS_pool_destroy");

    VERIFY(counter_g.val, 103, "200 incr + 100 decr");

    /* Create pool, add *lots* of 'incr' & 'decr' tasks, destroy pool */
    counter_g.val = 5;
    result        = H5TS_pool_create(&pool, NUM_THREADS);
    CHECK_I(result, "H5TS_pool_create");

    for (unsigned u = 0; u < 2 * 1000 * 1000; u++) {
        result = H5TS_pool_add_task(pool, incr_task, &counter_g);
        CHECK_I(result, "H5TS_pool_add_task");
    }

    for (unsigned u = 0; u < 1 * 1000 * 1000; u++) {
        result = H5TS_pool_add_task(pool, decr_task, &counter_g);
        CHECK_I(result, "H5TS_pool_add_task");
    }

    result = H5TS_pool_destroy(pool);
    CHECK_I(result, "H5TS_pool_destroy");

    VERIFY(counter_g.val, 5 + (1000 * 1000), "2,000,000 incr + 1,000,000 decr");

    /* Destroy the counter's mutex */
    result = H5TS_mutex_destroy(&counter_g.mutex);
    CHECK_I(result, "H5TS_mutex_destroy");

} /* end tts_thread_pool() */

#endif /*H5_HAVE_THREADS*/
