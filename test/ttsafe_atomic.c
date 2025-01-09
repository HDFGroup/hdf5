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

#if defined(H5_HAVE_THREADS) && !defined(H5_HAVE_STDATOMIC_H)

#define NUM_THREADS 16

static H5TS_atomic_int_t counter_g;

static H5TS_THREAD_RETURN_TYPE
noop_task(void *_counter)
{
    H5TS_atomic_int_t *counter   = (H5TS_atomic_int_t *)_counter;
    H5TS_thread_ret_t  ret_value = 0;

    VERIFY(H5TS_atomic_load_int(counter), 0, "noop_task");

    return ret_value;
}

static H5TS_THREAD_RETURN_TYPE
incr_task(void *_counter)
{
    H5TS_atomic_int_t *counter   = (H5TS_atomic_int_t *)_counter;
    H5TS_thread_ret_t  ret_value = 0;

    H5TS_atomic_fetch_add_int(counter, 1);

    return ret_value;
}

static H5TS_THREAD_RETURN_TYPE
decr_task(void *_counter)
{
    H5TS_atomic_int_t *counter   = (H5TS_atomic_int_t *)_counter;
    H5TS_thread_ret_t  ret_value = 0;

    H5TS_atomic_fetch_sub_int(counter, 1);

    return ret_value;
}

/*
 **********************************************************************
 * tts_atomics
 *
 **********************************************************************
 */
void
tts_atomics(void H5_ATTR_UNUSED *params)
{
    H5TS_pool_t *pool = NULL;
    herr_t       result;

    /* Initialize the counter */
    H5TS_atomic_init_int(&counter_g, 0);

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

    VERIFY(H5TS_atomic_load_int(&counter_g), 0, "noop");

    /* Create pool, add single 'incr' task, destroy pool */
    result = H5TS_pool_create(&pool, NUM_THREADS);
    CHECK_I(result, "H5TS_pool_create");

    result = H5TS_pool_add_task(pool, incr_task, &counter_g);
    CHECK_I(result, "H5TS_pool_add_task");

    result = H5TS_pool_destroy(pool);
    CHECK_I(result, "H5TS_pool_destroy");

    VERIFY(H5TS_atomic_load_int(&counter_g), 1, "single incr");

    /* Create pool, add pair of 'incr' & 'decr' tasks, destroy pool */
    H5TS_atomic_store_int(&counter_g, 10);
    result = H5TS_pool_create(&pool, NUM_THREADS);
    CHECK_I(result, "H5TS_pool_create");

    result = H5TS_pool_add_task(pool, incr_task, &counter_g);
    CHECK_I(result, "H5TS_pool_add_task");

    result = H5TS_pool_add_task(pool, decr_task, &counter_g);
    CHECK_I(result, "H5TS_pool_add_task");

    result = H5TS_pool_destroy(pool);
    CHECK_I(result, "H5TS_pool_destroy");

    VERIFY(H5TS_atomic_load_int(&counter_g), 10, "incr + decr");

    /* Create pool, add many 'incr' & 'decr' tasks, destroy pool */
    H5TS_atomic_store_int(&counter_g, 3);
    result = H5TS_pool_create(&pool, NUM_THREADS);
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

    VERIFY(H5TS_atomic_load_int(&counter_g), 103, "200 incr + 100 decr");

    /* Create pool, add *lots* of 'incr' & 'decr' tasks, destroy pool */
    H5TS_atomic_store_int(&counter_g, 5);
    result = H5TS_pool_create(&pool, NUM_THREADS);
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

    VERIFY(H5TS_atomic_load_int(&counter_g), 5 + (1000 * 1000), "2,000,000 incr + 1,000,000 decr");

    /* Destroy the atomic counter */
    H5TS_atomic_destroy_int(&counter_g);

} /* end tts_atomics() */

#endif /* defined(H5_HAVE_THREADS) && !defined(H5_HAVE_STDATOMIC_H) */
