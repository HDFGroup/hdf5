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
 * Test the correctness of the threadsafety developer API routines
 *
 ********************************************************************/

#include "ttsafe.h"

#ifdef H5_HAVE_THREADSAFE

typedef struct {
    H5TS_barrier_t *barrier;
} tts_develop_api_udata_t;

/*
 **********************************************************************
 * tts_develop_api_thr_1
 *
 **********************************************************************
 */
static void *
tts_develop_api_thr_1(void *_udata)
{
    tts_develop_api_udata_t *udata      = (tts_develop_api_udata_t *)_udata;
    unsigned                 lock_count = UINT_MAX;
    bool                     acquired   = false;
    herr_t                   result;

    /* Acquire the API lock - should acquire it */
    result = H5TSmutex_acquire(1, &acquired);
    CHECK_I(result, "H5TSmutex_acquire");
    VERIFY(acquired, true, "H5TSmutex_acquire");

    H5TS__barrier_wait(udata->barrier);

    /* Thread #2 will attempt (unsuccessfully) to acquire the API lock */

    H5TS__barrier_wait(udata->barrier);

    /* Release the API lock */
    result = H5TSmutex_release(&lock_count);
    CHECK_I(result, "H5TSmutex_release");
    VERIFY(lock_count, 1, "H5TSmutex_release");

    return NULL;
} /* end tts_develop_api_thr_1() */

/*
 **********************************************************************
 * tts_develop_api_thr_2
 *
 **********************************************************************
 */
static void *
tts_develop_api_thr_2(void *_udata)
{
    tts_develop_api_udata_t *udata    = (tts_develop_api_udata_t *)_udata;
    bool                     acquired = false;
    herr_t                   result;

    /* Thread #1 will acquire the API lock */

    H5TS__barrier_wait(udata->barrier);

    /* Attempt to acquire the API lock - should not acquire it */
    result = H5TSmutex_acquire(1, &acquired);
    CHECK_I(result, "H5TSmutex_acquire");
    VERIFY(acquired, false, "H5TSmutex_acquire");

    H5TS__barrier_wait(udata->barrier);

    /* Thread #1 will release the API lock */

    return NULL;
} /* end tts_develop_api_thr_2() */

/*
 **********************************************************************
 * tts_develop_api
 *
 **********************************************************************
 */
void
tts_develop_api(void)
{
    H5TS_thread_t           thread_1, thread_2;
    H5TS_barrier_t          barrier;
    unsigned                lock_count = UINT_MAX;
    bool                    acquired   = false;
    tts_develop_api_udata_t udata;
    unsigned                api_count_1 = 0, api_count_2 = 0;
    herr_t                  result;

    /* Check that API count increases with each API call */
    result = H5TSmutex_get_attempt_count(&api_count_1);
    CHECK_I(result, "H5TSmutex_get_attempt_count");

    /* No-op API call, to increment the API counter */
    result = H5garbage_collect();
    CHECK_I(result, "H5garbage_collect");

    result = H5TSmutex_get_attempt_count(&api_count_2);
    CHECK_I(result, "H5TSmutex_get_attempt_count");

    VERIFY(api_count_2, (api_count_1 + 1), "H5TSmutex_get_attempt_count");

    /* Check H5TSmutex_acquire & H5TSmutex_release in thread callbacks */

    /* Create the thread barrier for the two threads */
    result = H5TS__barrier_init(&barrier, 2);
    CHECK_I(result, "H5TS__barrier_init");

    /* Create the threads */
    udata.barrier = &barrier;
    result        = H5TS_thread_create(&thread_1, tts_develop_api_thr_1, &udata);
    CHECK_I(result, "H5TS_thread_create");
    result = H5TS_thread_create(&thread_2, tts_develop_api_thr_2, &udata);
    CHECK_I(result, "H5TS_thread_create");

    /* Wait for threads to complete. */
    result = H5TS_thread_join(thread_1, NULL);
    CHECK_I(result, "H5TS_thread_join");
    result = H5TS_thread_join(thread_2, NULL);
    CHECK_I(result, "H5TS_thread_join");

    result = H5TS__barrier_destroy(&barrier);
    CHECK_I(result, "H5TS__barrier_destroy");

    /* Test multiple / recursive acquisition of the API lock  */

    /* Acquire the API lock - should acquire it */
    result = H5TSmutex_acquire(1, &acquired);
    CHECK_I(result, "H5TSmutex_acquire");
    VERIFY(acquired, true, "H5TSmutex_acquire");

    /* Acquire the API lock again - should acquire it, since it's the same thread  */
    acquired = false;
    result   = H5TSmutex_acquire(1, &acquired);
    CHECK_I(result, "H5TSmutex_acquire");
    VERIFY(acquired, true, "H5TSmutex_acquire");

    /* Release the API lock */
    result = H5TSmutex_release(&lock_count);
    CHECK_I(result, "H5TSmutex_release");
    VERIFY(lock_count, 2, "H5TSmutex_release");

} /* end tts_develop_api() */

#endif /*H5_HAVE_THREADSAFE*/
