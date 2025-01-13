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
 * Test the correctness of the non-recursive R/W lock routines
 *
 ********************************************************************/

#include "ttsafe.h"

#ifdef H5_HAVE_THREADS

#define NUM_THREADS 16
#define NUM_WRITERS 4

#define NUM_ITERS 12
#define COUNT_MAX 512

typedef struct {
    H5TS_rwlock_t  lock;
    int            val;
    H5TS_barrier_t barrier;
} atomic_counter_t;

static H5TS_THREAD_RETURN_TYPE
incr_task(void *_counter)
{
    atomic_counter_t *counter = (atomic_counter_t *)_counter;
    herr_t            result;
    H5TS_thread_ret_t ret_value = 0;

    result = H5TS_rwlock_wrlock(&counter->lock);
    CHECK_I(result, "H5TS_rwlock_wrlock");

    /* Increment value */
    counter->val++;

    result = H5TS_rwlock_wrunlock(&counter->lock);
    CHECK_I(result, "H5TS_rwlock_wrunlock");

    return ret_value;
}

static H5TS_THREAD_RETURN_TYPE
many_read(void *_counter)
{
    atomic_counter_t *counter = (atomic_counter_t *)_counter;
    herr_t            result;
    H5TS_thread_ret_t ret_value = 0;

    result = H5TS_rwlock_rdlock(&counter->lock);
    CHECK_I(result, "H5TS_rwlock_rdlock");

    /* Wait at barrier, to confirm that many readers can hold lock */
    result = H5TS_barrier_wait(&counter->barrier);
    CHECK_I(result, "H5TS_barrier_wait");

    result = H5TS_rwlock_rdunlock(&counter->lock);
    CHECK_I(result, "H5TS_rdlock_rdunlock");

    return ret_value;
}

static H5TS_THREAD_RETURN_TYPE
count_up_and_down(void *_counter)
{
    atomic_counter_t *counter = (atomic_counter_t *)_counter;
    herr_t            result;
    H5TS_thread_ret_t ret_value = 0;

    /* Count up & down a number of times */
    for (unsigned u = 0; u < NUM_ITERS; u++) {
        /* Wait at barrier, to ensure all threads are ready to count */
        result = H5TS_barrier_wait(&counter->barrier);
        CHECK_I(result, "H5TS_barrier_wait");

        /* Count up */
        for (unsigned v = 0; v < COUNT_MAX; v++) {
            result = H5TS_rwlock_wrlock(&counter->lock);
            CHECK_I(result, "H5TS_rwlock_wrlock");

            /* Increment value */
            counter->val++;

            result = H5TS_rwlock_wrunlock(&counter->lock);
            CHECK_I(result, "H5TS_rwlock_wrunlock");
        }

        /* Wait at barrier, to ensure all threads have finishend counting up */
        result = H5TS_barrier_wait(&counter->barrier);
        CHECK_I(result, "H5TS_barrier_wait");

        /* Count down */
        for (unsigned v = 0; v < COUNT_MAX; v++) {
            result = H5TS_rwlock_wrlock(&counter->lock);
            CHECK_I(result, "H5TS_rwlock_wrlock");

            /* Decrement value */
            counter->val--;

            result = H5TS_rwlock_wrunlock(&counter->lock);
            CHECK_I(result, "H5TS_rwlock_wrunlock");
        }
    }

    return ret_value;
}

static H5TS_THREAD_RETURN_TYPE
verify_counting(void *_counter)
{
    atomic_counter_t *counter = (atomic_counter_t *)_counter;
    herr_t            result;
    int               last_val  = 0;
    H5TS_thread_ret_t ret_value = 0;

    /* Count up & down a number of times */
    for (unsigned u = 0; u < NUM_ITERS; u++) {
        /* Wait at barrier, to ensure all threads are ready to count */
        result = H5TS_barrier_wait(&counter->barrier);
        CHECK_I(result, "H5TS_barrier_wait");

        /* Verify that counter goes only up */
        do {
            result = H5TS_rwlock_rdlock(&counter->lock);
            CHECK_I(result, "H5TS_rwlock_rdlock");

            /* Check counter value */
            if (counter->val < last_val)
                ERROR("incorrect counter value");

            /* Save value */
            last_val = counter->val;

            result = H5TS_rwlock_rdunlock(&counter->lock);
            CHECK_I(result, "H5TS_rdlock_wrunlock");

            /* Give the writers a chance to make progress */
            H5TS_thread_yield();
        } while (last_val < (NUM_WRITERS * COUNT_MAX));

        /* Wait at barrier, to ensure all threads have finishend counting up */
        result = H5TS_barrier_wait(&counter->barrier);
        CHECK_I(result, "H5TS_barrier_wait");

        /* Verify that counter goes only down */
        do {
            result = H5TS_rwlock_rdlock(&counter->lock);
            CHECK_I(result, "H5TS_rwlock_rdlock");

            /* Check counter value */
            if (counter->val > last_val)
                ERROR("incorrect counter value");

            /* Save value */
            last_val = counter->val;

            result = H5TS_rwlock_rdunlock(&counter->lock);
            CHECK_I(result, "H5TS_rdlock_wrunlock");

            /* Give the writers a chance to make progress */
            H5TS_thread_yield();
        } while (last_val > 0);
    }

    return ret_value;
}

/*
 **********************************************************************
 * tts_rwlock
 **********************************************************************
 */
void
tts_rwlock(void H5_ATTR_UNUSED *params)
{
    H5TS_thread_t    threads[NUM_THREADS];
    H5TS_pool_t     *pool = NULL;
    H5TS_rwlock_t    lock;
    atomic_counter_t counter;
    herr_t           result;

    /* Sanity checks on bad input */
    result = H5TS_rwlock_init(NULL);
    VERIFY(result, FAIL, "H5TS_rwlock_init");
    result = H5TS_rwlock_rdlock(NULL);
    VERIFY(result, FAIL, "H5TS_rwlock_rdlock");
    result = H5TS_rwlock_rdunlock(NULL);
    VERIFY(result, FAIL, "H5TS_rwlock_rdunlock");
    result = H5TS_rwlock_wrlock(NULL);
    VERIFY(result, FAIL, "H5TS_rwlock_wrlock");
    result = H5TS_rwlock_wrunlock(NULL);
    VERIFY(result, FAIL, "H5TS_rwlock_wrunlock");
    result = H5TS_rwlock_destroy(NULL);
    VERIFY(result, FAIL, "H5TS_rwlock_destroy");

    /* Create & destroy lock */
    result = H5TS_rwlock_init(&lock);
    CHECK_I(result, "H5TS_rwlock_init");

    result = H5TS_rwlock_destroy(&lock);
    CHECK_I(result, "H5TS_rwlock_destroy");

    /* Read lock & unlock */
    result = H5TS_rwlock_init(&lock);
    CHECK_I(result, "H5TS_rwlock_init");

    result = H5TS_rwlock_rdlock(&lock);
    CHECK_I(result, "H5TS_rwlock_rdlock");

    result = H5TS_rwlock_rdunlock(&lock);
    CHECK_I(result, "H5TS_rwlock_rdunlock");

    result = H5TS_rwlock_destroy(&lock);
    CHECK_I(result, "H5TS_rwlock_destroy");

    /* Write lock & unlock */
    result = H5TS_rwlock_init(&lock);
    CHECK_I(result, "H5TS_rwlock_init");

    result = H5TS_rwlock_wrlock(&lock);
    CHECK_I(result, "H5TS_rwlock_wrlock");

    result = H5TS_rwlock_wrunlock(&lock);
    CHECK_I(result, "H5TS_rwlock_wrunlock");

    result = H5TS_rwlock_destroy(&lock);
    CHECK_I(result, "H5TS_rwlock_destroy");

    /* Hold read lock w/many threads */
    result = H5TS_rwlock_init(&counter.lock);
    CHECK_I(result, "H5TS_rwlock_init");

    result = H5TS_barrier_init(&counter.barrier, NUM_THREADS);
    CHECK_I(result, "H5TS_barrier_init");

    for (unsigned u = 0; u < NUM_THREADS; u++) {
        result = H5TS_thread_create(&threads[u], many_read, &counter);
        CHECK_I(result, "H5TS_thread_create");
    }

    for (unsigned u = 0; u < NUM_THREADS; u++) {
        result = H5TS_thread_join(threads[u], NULL);
        CHECK_I(result, "H5TS_thread_join");
    }

    result = H5TS_barrier_destroy(&counter.barrier);
    CHECK_I(result, "H5TS_barrier_destroy");

    result = H5TS_rwlock_destroy(&counter.lock);
    CHECK_I(result, "H5TS_rwlock_destroy");

    /* Increment counter w/many threads */
    result = H5TS_rwlock_init(&counter.lock);
    CHECK_I(result, "H5TS_rwlock_init");

    result = H5TS_pool_create(&pool, NUM_THREADS);
    CHECK_I(result, "H5TS_pool_create");

    counter.val = 0;
    for (unsigned u = 0; u < NUM_THREADS; u++) {
        result = H5TS_pool_add_task(pool, incr_task, &counter);
        CHECK_I(result, "H5TS_pool_add_task");
    }

    result = H5TS_pool_destroy(pool);
    CHECK_I(result, "H5TS_pool_destroy");

    VERIFY(counter.val, NUM_THREADS, "many incr");

    result = H5TS_rwlock_destroy(&counter.lock);
    CHECK_I(result, "H5TS_rwlock_destroy");

    /* Increment & decrement counter w/many threads while reading */
    result = H5TS_rwlock_init(&counter.lock);
    CHECK_I(result, "H5TS_rwlock_init");

    result = H5TS_barrier_init(&counter.barrier, NUM_THREADS);
    CHECK_I(result, "H5TS_barrier_init");

    result = H5TS_pool_create(&pool, NUM_THREADS);
    CHECK_I(result, "H5TS_pool_create");

    counter.val = 0;
    for (unsigned u = 0; u < NUM_WRITERS; u++) {
        result = H5TS_pool_add_task(pool, count_up_and_down, &counter);
        CHECK_I(result, "H5TS_pool_add_task");
    }
    for (unsigned u = 0; u < (NUM_THREADS - NUM_WRITERS); u++) {
        result = H5TS_pool_add_task(pool, verify_counting, &counter);
        CHECK_I(result, "H5TS_pool_add_task");
    }

    result = H5TS_pool_destroy(pool);
    CHECK_I(result, "H5TS_pool_destroy");

    VERIFY(counter.val, 0, "count up & down");

    result = H5TS_barrier_destroy(&counter.barrier);
    CHECK_I(result, "H5TS_barrier_destroy");

    result = H5TS_rwlock_destroy(&counter.lock);
    CHECK_I(result, "H5TS_rwlock_destroy");

} /* end tts_rwlock() */

#endif /*H5_HAVE_THREADS*/
