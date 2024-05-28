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

/********************************************************************
 *
 * Test the correctness of the FFS R/W lock routines
 *
 ********************************************************************/

#include "ttsafe.h"

#if defined(H5_HAVE_THREADS) && defined(H5_HAVE_STDATOMIC_H)

#define NUM_THREADS 16

typedef struct {
    H5TS_ffs_rwlock_t lock;
    int            val;
    H5TS_barrier_t barrier;
} atomic_counter_t;

static H5TS_THREAD_RETURN_TYPE
incr_task(void *_counter)
{
    H5TS_ffs_rwlock_local_t local;
    atomic_counter_t *counter = (atomic_counter_t *)_counter;
    herr_t            result;
    H5TS_thread_ret_t ret_value = 0;

    result = H5TS_ffs_rwlock_wrlock(&counter->lock, &local);
    CHECK_I(result, "H5TS_ffs_rwlock_wrlock");

    /* Increment value */
    counter->val++;

    result = H5TS_ffs_rwlock_wrunlock(&counter->lock, &local);
    CHECK_I(result, "H5TS_ffs_rwlock_wrunlock");

    return ret_value;
}

static H5TS_THREAD_RETURN_TYPE
many_read(void *_counter)
{
    H5TS_ffs_rwlock_local_t local;
    atomic_counter_t *counter = (atomic_counter_t *)_counter;
    herr_t            result;
    H5TS_thread_ret_t ret_value = 0;

    result = H5TS_ffs_rwlock_rdlock(&counter->lock, &local);
    CHECK_I(result, "H5TS_ffs_rwlock_rdlock");

    /* Wait at barrier, to confirm that many readers can hold lock */
    result = H5TS_barrier_wait(&counter->barrier);
    CHECK_I(result, "H5TS_barrier_wait");

    result = H5TS_ffs_rwlock_rdunlock(&counter->lock, &local);
    CHECK_I(result, "H5TS_ffs_rdlock_rdunlock");

    return ret_value;
}

/*
 **********************************************************************
 * tts_ffs_rwlock
 **********************************************************************
 */
void
tts_ffs_rwlock(void)
{
    H5TS_ffs_rwlock_t lock;
    H5TS_ffs_rwlock_local_t local;
    H5TS_pool_t     *pool = NULL;
    H5TS_thread_t threads[NUM_THREADS];
    atomic_counter_t counter;
    herr_t           result;

    /* Sanity checks on bad input */
    result = H5TS_ffs_rwlock_init(NULL);
    VERIFY(result, FAIL, "H5TS_ffs_rwlock_init");
    result = H5TS_ffs_rwlock_rdlock(NULL, NULL);
    VERIFY(result, FAIL, "H5TS_ffs_rwlock_rdlock");
    result = H5TS_ffs_rwlock_rdlock(&lock, NULL);
    VERIFY(result, FAIL, "H5TS_ffs_rwlock_rdlock");
    result = H5TS_ffs_rwlock_rdlock(NULL, &local);
    VERIFY(result, FAIL, "H5TS_ffs_rwlock_rdlock");
    result = H5TS_ffs_rwlock_rdunlock(NULL, NULL);
    VERIFY(result, FAIL, "H5TS_ffs_rwlock_rdunlock");
    result = H5TS_ffs_rwlock_rdunlock(&lock, NULL);
    VERIFY(result, FAIL, "H5TS_ffs_rwlock_rdunlock");
    result = H5TS_ffs_rwlock_rdunlock(NULL, &local);
    VERIFY(result, FAIL, "H5TS_ffs_rwlock_rdunlock");
    result = H5TS_ffs_rwlock_wrlock(NULL, NULL);
    VERIFY(result, FAIL, "H5TS_ffs_rwlock_wrlock");
    result = H5TS_ffs_rwlock_wrlock(&lock, NULL);
    VERIFY(result, FAIL, "H5TS_ffs_rwlock_wrlock");
    result = H5TS_ffs_rwlock_wrlock(NULL, &local);
    VERIFY(result, FAIL, "H5TS_ffs_rwlock_wrlock");
    result = H5TS_ffs_rwlock_wrunlock(NULL, NULL);
    VERIFY(result, FAIL, "H5TS_ffs_rwlock_wrunlock");
    result = H5TS_ffs_rwlock_wrunlock(&lock, NULL);
    VERIFY(result, FAIL, "H5TS_ffs_rwlock_wrunlock");
    result = H5TS_ffs_rwlock_wrunlock(NULL, &local);
    VERIFY(result, FAIL, "H5TS_ffs_rwlock_wrunlock");


    /* Create lock */
    result = H5TS_ffs_rwlock_init(&lock);
    CHECK_I(result, "H5TS_ffs_rwlock_init");


    /* Read lock & unlock */
    result = H5TS_ffs_rwlock_rdlock(&lock, &local);
    CHECK_I(result, "H5TS_ffs_rwlock_rdlock");

    result = H5TS_ffs_rwlock_rdunlock(&lock, &local);
    CHECK_I(result, "H5TS_ffs_rwlock_rdunlock");


    /* Write lock & unlock */
    result = H5TS_ffs_rwlock_wrlock(&lock, &local);
    CHECK_I(result, "H5TS_ffs_rwlock_wrlock");

    result = H5TS_ffs_rwlock_wrunlock(&lock, &local);
    CHECK_I(result, "H5TS_ffs_rwlock_wrunlock");


    /* Hold read lock w/many threads */
    result = H5TS_ffs_rwlock_init(&counter.lock);
    CHECK_I(result, "H5TS_ffs_rwlock_init");

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


    /* Increment counter w/many threads */
    result = H5TS_ffs_rwlock_init(&counter.lock);
    CHECK_I(result, "H5TS_ffs_rwlock_init");

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


} /* end tts_ffs_rwlock() */

#endif /* defined(H5_HAVE_THREADS) && defined(H5_HAVE_STDATOMIC_H) */

