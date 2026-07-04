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
 * Test the correctness of the threadsafety developer API routines
 *
 ********************************************************************/

#define H5VL_FRIEND /* Suppress error about including H5VLpkg    */
#define H5VL_TESTING

#include "ttsafe.h"
#include "H5VLpkg.h" /* Virtual Object Layer                 */

#ifdef H5_HAVE_THREADSAFE_API

typedef struct {
    H5TS_barrier_t *barrier;
} tts_develop_api_udata_t;

/*
 **********************************************************************
 * tts_develop_api_thr_1
 *
 **********************************************************************
 */
static H5TS_THREAD_RETURN_TYPE
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

    result = H5TS_barrier_wait(udata->barrier);
    CHECK_I(result, "H5TS_barrier_wait");

    /* Thread #2 will attempt (unsuccessfully) to acquire the API lock */

    result = H5TS_barrier_wait(udata->barrier);
    CHECK_I(result, "H5TS_barrier_wait");

    /* Release the API lock */
    result = H5TSmutex_release(&lock_count);
    CHECK_I(result, "H5TSmutex_release");
    VERIFY(lock_count, 1, "H5TSmutex_release");

    return (H5TS_thread_ret_t)0;
} /* end tts_develop_api_thr_1() */

/*
 **********************************************************************
 * tts_develop_api_thr_2
 *
 **********************************************************************
 */
static H5TS_THREAD_RETURN_TYPE
tts_develop_api_thr_2(void *_udata)
{
    tts_develop_api_udata_t *udata    = (tts_develop_api_udata_t *)_udata;
    bool                     acquired = false;
    herr_t                   result;

    /* Thread #1 will acquire the API lock */

    result = H5TS_barrier_wait(udata->barrier);
    CHECK_I(result, "H5TS_barrier_wait");

    /* Attempt to acquire the API lock - should not acquire it */
    result = H5TSmutex_acquire(1, &acquired);
    CHECK_I(result, "H5TSmutex_acquire");
    VERIFY(acquired, false, "H5TSmutex_acquire");

    result = H5TS_barrier_wait(udata->barrier);
    CHECK_I(result, "H5TS_barrier_wait");

    /* Thread #1 will release the API lock */

    return (H5TS_thread_ret_t)0;
} /* end tts_develop_api_thr_2() */

/*
 **********************************************************************
 * tts_develop_api
 *
 **********************************************************************
 */
void
tts_develop_api(void H5_ATTR_UNUSED *params)
{
    hid_t                   def_fapl = H5I_INVALID_HID;
    hid_t                   vol_id   = H5I_INVALID_HID;
    H5TS_thread_t           thread_1, thread_2;
    H5TS_barrier_t          barrier;
    unsigned                lock_count = UINT_MAX;
    bool                    acquired   = false;
    tts_develop_api_udata_t udata;
    unsigned                api_count_1 = 0, api_count_2 = 0;
    int                     is_native;
    herr_t                  result;

    def_fapl = H5Pcreate(H5P_FILE_ACCESS);
    CHECK(def_fapl, H5I_INVALID_HID, "H5Pcreate");

    result = H5Pget_vol_id(def_fapl, &vol_id);
    CHECK(result, FAIL, "H5Pget_vol_id");

    is_native = H5VL__is_native_connector_test(vol_id);
    CHECK(is_native, FAIL, "H5VL__is_native_connector_test");

    if (is_native) {
        /* Check that API count increases with each API call */
        result = H5TSmutex_get_attempt_count(&api_count_1);
        CHECK_I(result, "H5TSmutex_get_attempt_count");

        /* No-op API call, to increment the API counter */
        result = H5garbage_collect();
        CHECK_I(result, "H5garbage_collect");

        result = H5TSmutex_get_attempt_count(&api_count_2);
        CHECK_I(result, "H5TSmutex_get_attempt_count");

        VERIFY(api_count_2, (api_count_1 + 1), "H5TSmutex_get_attempt_count");
    } /* end if */
    else
        printf("Non-native VOL connector used, skipping mutex attempt count test\n");

    /* Check H5TSmutex_acquire & H5TSmutex_release in thread callbacks */

    /* Create the thread barrier for the two threads */
    result = H5TS_barrier_init(&barrier, 2);
    CHECK_I(result, "H5TS_barrier_init");

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

    result = H5TS_barrier_destroy(&barrier);
    CHECK_I(result, "H5TS_barrier_destroy");

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

#endif /* H5_HAVE_THREADSAFE_API */
