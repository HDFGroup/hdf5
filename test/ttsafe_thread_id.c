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

#define CYCLE_COUNT 2
#define NTHREADS    5

static volatile bool  failed = false;
static H5TS_barrier_t barrier;
static int            times;
static bool           used[NTHREADS * CYCLE_COUNT];
static H5TS_mutex_t   used_lock = PTHREAD_MUTEX_INITIALIZER;

/* Each thread runs this routine.  The routine fetches the current
 * thread's ID, makes sure that it is in the expected range, makes
 * sure that in this round of testing, no two threads shared the
 * same ID, and checks that each thread's ID is constant over its lifetime.
 *
 * main() checks that every ID in the range
 * [(times * NTHREADS) + 2, (times * NTHREADS) + NTHREADS + 1] is used in each
 * round of testing.  All NTHREADS threads synchronize on a barrier after each
 * has fetched its ID.  The barrier guarantees that all threads' lifetimes
 * overlap at least momentarily, so the IDs will be unique, and there
 * will be NTHREADS of them.  Further, since thread IDs are assigned
 * starting with 1 (which the main thread gets), the number of threads with
 * IDs alive never exceeds NTHREADS, and thread IDs are never recycled, the
 * least ID has to be (times * NTHREADS) + 2 and the greatest,
 * (times * NTHREADS) + NTHREADS + 1.
 */
static void *
thread_main(void H5_ATTR_UNUSED *arg)
{
    int      min_id, max_id;
    uint64_t ntid, tid;

    tid = H5TS_thread_id();

    H5TS_mutex_lock(&used_lock);
    min_id = (times * NTHREADS) + 2;
    max_id = (times * NTHREADS) + NTHREADS + 1;

    /* Verify that thread ID is in correct range */
    if (tid < (uint64_t)min_id || (uint64_t)max_id < tid) {
        TestErrPrintf("unexpected tid %" PRIu64 " FAIL\n", tid);
        goto pre_barrier_error;
    }

    /* Verify that the thread ID hasn't been re-used */
    if (used[tid - 2]) {
        TestErrPrintf("reused tid %" PRIu64 " FAIL\n", tid);
        H5TS_mutex_unlock(&used_lock);
        goto pre_barrier_error;
    }
    used[tid - 2] = true;
    H5TS_mutex_unlock(&used_lock);

    H5TS__barrier_wait(&barrier);

    /* Verify that the thread ID hasn't changed */
    ntid = H5TS_thread_id();
    if (ntid != tid)
        TestErrPrintf("tid changed from %" PRIu64 " to %" PRIu64 " FAIL\n", tid, ntid);

    return NULL;

pre_barrier_error:
    H5TS__barrier_wait(&barrier);

    return NULL;
}

/*
 **********************************************************************
 * tts_thread_id
 *
 **********************************************************************
 */
void
tts_thread_id(void)
{
    H5TS_thread_t threads[NTHREADS];
    uint64_t      tid;
    int           i;
    herr_t        result;

    result = H5TS__barrier_init(&barrier, NTHREADS);
    CHECK_I(result, "H5TS__barrier_init");

    /* Get the thread ID for the main thread, so that the child threads
     * always start from a thread ID of 2.
     */
    tid = H5TS_thread_id();
    VERIFY(tid, 1, "H5TS_thread_id");

    /* Start the test threads and join them twice to make sure that
     * the thread IDs are recycled in the second round.
     */
    memset(used, 0, sizeof(used));
    for (times = 0; times < CYCLE_COUNT; times++) {
        for (i = 0; i < NTHREADS; i++)
            threads[i] = H5TS__create_thread(thread_main, NULL, NULL);

        for (i = 0; i < NTHREADS; i++)
            H5TS__wait_for_thread(threads[i]);

        /* Access synchronized by thread create/join */
        for (i = 0; i < NTHREADS; i++) {
            if (!used[(times * NTHREADS) + i])
                TestErrPrintf("thread ID %d did not run.", i + 1);
        }
    }
    result = H5TS__barrier_destroy(&barrier);
    CHECK_I(result, "H5TS__barrier_init");

} /* end tts_thread_id() */

#endif /*H5_HAVE_THREADSAFE*/
