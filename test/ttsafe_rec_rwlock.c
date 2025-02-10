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
 * Test the correctness of the recursive R/W lock in the HDF5 library
 * -------------------------------------------------------------
 *
 * Test the recursive R/W lock in isolation, using a combination of
 * error return values and statistics collected by the recursive
 * R/W lock to detect any failures.
 *
 * No file is created.
 *
 * Multiple threads are created, and allowed to compete for the lock.
 * The number of threads, and the number of times they obtain the
 * lock depends on the express test level.
 *
 ********************************************************************/

#include "ttsafe.h"

#ifdef H5_HAVE_THREADS
#ifndef H5_HAVE_WIN_THREADS

#define MAX_NUM_THREADS 32
#define MAX_LOCK_CYCLES (1000 * 1000)

/* structure used to configure test threads in the recursive
 * R/W/ lock tests.
 */
/***********************************************************************
 *
 * Structure rec_rwlock_test_udata_t
 *
 * Arrays of instances of rec_rwlock_test_udata_t are used to configure
 * the threads used to test the recursive R/W lock, and to collect
 * statistics on their behaviour.  These statistics are aggregated and
 * used to cross-check the statistics collected by the recursive R/W
 * lock proper.
 *
 * The fields of the structure are discussed below:
 *
 * lock: Pointer to the recursive R/W lock under test.
 *
 * target_rd_lock_cycles: The number of times the test thread is
 *              required to obtain and drop the read lock.  Note
 *              that this value restricts the number of initial
 *              read locks only.  Additional recursive locks are
 *              possible -- see max_recursive_lock_depth below.
 *
 * target_wr_lock_cycles: The number of times the test thread is
 *              required to obtain and drop the write lock.  Note
 *              that this value restricts the number of initial
 *              write locks only.  Additional recursive locks are
 *              possible -- see max_recursive_lock_depth below.
 *
 * max_recursive_lock_depth: Once a test thread gains a lock, it does
 *              random recursive leocks and unlocks until it happens
 *              to drop the lock.  The max_recursive_lock_depth
 *              places an upper bound on the net number of locks.
 *              Any attempt exceed this limit is converted into
 *              an unlock.
 *
 * The remaining fields are used for statistics collection.  They are
 * thread specific versions of the fields of the same name in
 * H5TS_rec_rwlock_stats_t.  See the header comment for that
 * structure (in H5TSprivate.h) for further details.
 *
 ***********************************************************************/
typedef struct rec_rwlock_test_udata_t {

    /* thread control fields */
    H5TS_rec_rwlock_t *lock;
    int32_t            target_rd_lock_cycles;
    int32_t            target_wr_lock_cycles;
    int32_t            max_recursive_lock_depth;

    /* thread stats fields */
    int64_t read_locks_granted;
    int64_t read_locks_released;
    int64_t real_read_locks_granted;
    int64_t real_read_locks_released;
    int64_t write_locks_granted;
    int64_t write_locks_released;
    int64_t real_write_locks_granted;
    int64_t real_write_locks_released;

} rec_rwlock_test_udata_t;

/*
 **********************************************************************
 * tts_rec_rwlock_smoke_check_test_thread
 *
 * Perform a sequence of recursive read and/or write locks on the
 * target recursive R/W lock as directed by the supplied user data.
 * Record all operations in the user data for later cross-checking
 * with the statistics maintained by the recursive R/W lock.
 *
 * Note: while the number of read and/or write locks is fixed, the
 * number of _recursive_ lock and unlock calls is random, as is the
 * order of the read and write locks, if both are enabled.
 *
 **********************************************************************
 */
static H5TS_THREAD_RETURN_TYPE
tts_rec_rwlock_smoke_check_test_thread(void *_udata)
{
    bool                     read;
    int32_t                  rec_lock_depth = 0;
    int32_t                  max_rec_lock_depth;
    int32_t                  rd_locks_remaining;
    int32_t                  wr_locks_remaining;
    herr_t                   result;
    H5TS_rec_rwlock_t       *lock;
    rec_rwlock_test_udata_t *udata = (rec_rwlock_test_udata_t *)_udata;

    assert(_udata);
    rd_locks_remaining = udata->target_rd_lock_cycles;
    wr_locks_remaining = udata->target_wr_lock_cycles;
    max_rec_lock_depth = udata->max_recursive_lock_depth;
    lock               = udata->lock;

    while (rd_locks_remaining > 0 || wr_locks_remaining > 0) {
        if (wr_locks_remaining == 0)
            read = true;
        else if (rd_locks_remaining == 0)
            read = false;
        else {
            if ((rand() % 2) == 0)
                read = true;
            else
                read = false;
        }

        if (read) {
            result = H5TS__rec_rwlock_rdlock(lock);
            CHECK_I(result, "H5TS__rec_rwlock_rdlock");

            udata->read_locks_granted++;
            udata->real_read_locks_granted++;
            rd_locks_remaining--;
            rec_lock_depth = 1;

            while (rec_lock_depth > 0) {
                if (rec_lock_depth >= max_rec_lock_depth || (rand() % 2) == 0) {
                    result = H5TS__rec_rwlock_rdunlock(lock);
                    CHECK_I(result, "H5TS__rec_rwlock_rdunlock");

                    rec_lock_depth--;
                    udata->read_locks_released++;
                }
                else {
                    result = H5TS__rec_rwlock_rdlock(lock);
                    CHECK_I(result, "H5TS__rec_rwlock_rdlock");

                    rec_lock_depth++;
                    udata->read_locks_granted++;
                }
            }

            udata->real_read_locks_released++;
        }
        else {
            result = H5TS__rec_rwlock_wrlock(lock);
            CHECK_I(result, "H5TS__rec_rwlock_wrlock");

            udata->write_locks_granted++;
            udata->real_write_locks_granted++;
            wr_locks_remaining--;
            rec_lock_depth = 1;

            while (rec_lock_depth > 0) {
                if (rec_lock_depth >= max_rec_lock_depth || (rand() % 2) == 0) {
                    result = H5TS__rec_rwlock_wrunlock(lock);
                    CHECK_I(result, "H5TS__rec_rwlock_wrunlock");

                    rec_lock_depth--;
                    udata->write_locks_released++;
                }
                else {
                    result = H5TS__rec_rwlock_wrlock(lock);
                    CHECK_I(result, "H5TS__rec_rwlock_wrlock");

                    rec_lock_depth++;
                    udata->write_locks_granted++;
                }
            }

            udata->real_write_locks_released++;
        }
    }

    return (H5TS_thread_ret_t)0;
} /* end tts_rec_rwlock_smoke_check_test_thread() */

/*
 **********************************************************************
 * tts_rec_rwlock_smoke_check_1
 *
 * Single thread test to verify basic functionality and error
 * rejection of the recursive R/W lock.
 *
 *  1) Initialize an instance of the recursive R/W lock.
 *
 *  2) Obtain a read lock.
 *
 *  3) Drop the read lock.
 *
 *  4) Verify the expected stats, and then reset them.
 *
 *  5) Obtain a read lock.
 *
 *  6) Obtain the read lock a second time.
 *
 *  7) Drop the read lock.
 *
 *  8) Drop the read lock a second time.
 *
 *  9) Verify the expected stats, and then reset them.
 *
 * 10) Obtain a write lock.
 *
 * 11) Drop the write lock.
 *
 * 12) Verify the expected stats, and then reset them.
 *
 * 13) Obtain a write lock.
 *
 * 14) Obtain the write lock a second time.
 *
 * 15) Drop the write lock.
 *
 * 16) Drop the write lock a second time.
 *
 * 17) Verify the expected stats, and then reset them.
 *
 * 18) Obtain a write lock.
 *
 * 19) Attempt to obtain a read lock -- should fail.
 *
 * 20) Drop the write lock.
 *
 * 21) Obtain a read lock.
 *
 * 22) Attempt to obtain a write lock -- should fail.
 *
 * 23) Drop the read lock.
 *
 * 24) Verify the expected stats, and then reset them.
 *
 * 25) Shut down the recursive R/W lock.
 *
 **********************************************************************
 */
void
tts_rec_rwlock_smoke_check_1(void H5_ATTR_UNUSED *params)
{
    herr_t result;
#if H5TS_ENABLE_REC_RWLOCK_STATS
    H5TS_rec_rwlock_stats_t stats;
#endif
    H5TS_rec_rwlock_t lock;

    /* 1) Initialize an instance of the recursive R/W lock. */
    result = H5TS__rec_rwlock_init(&lock);
    CHECK_I(result, "H5TS__rec_rwlock_init");

    /* 2) Obtain a read lock. */
    result = H5TS__rec_rwlock_rdlock(&lock);
    CHECK_I(result, "H5TS__rec_rwlock_rdlock");

    /* 3) Drop the read lock. */
    result = H5TS__rec_rwlock_rdunlock(&lock);
    CHECK_I(result, "H5TS__rec_rwlock_rdunlock");

#if H5TS_ENABLE_REC_RWLOCK_STATS
    /* 4) Verify the expected stats, and then reset them. */
    result = H5TS__rec_rwlock_get_stats(&lock, &stats);
    CHECK_I(result, "H5TS__rec_rwlock_get_stats");

    result = H5TS__rec_rwlock_reset_stats(&lock);
    CHECK_I(result, "H5TS__rec_rwlock_reset_stats");

    /* clang-format makes this conditional unreadable, so turn it off. */
    /* clang-format off */
    if (stats.read_locks_granted             != 1 ||
        stats.read_locks_released            != 1 ||
        stats.real_read_locks_granted        != 1 ||
        stats.real_read_locks_released       != 1 ||
        stats.max_read_locks                 != 1 ||
        stats.max_read_lock_recursion_depth  != 1 ||
        stats.read_locks_delayed             != 0 ||
        stats.write_locks_granted            != 0 ||
        stats.write_locks_released           != 0 ||
        stats.real_write_locks_granted       != 0 ||
        stats.real_write_locks_released      != 0 ||
        stats.max_write_locks                != 0 ||
        stats.max_write_lock_recursion_depth != 0 ||
        stats.write_locks_delayed            != 0 ||
        stats.max_write_locks_pending        != 0 ) {

        TestErrPrintf("Unexpected recursive R/W lock stats -- 1");
        H5TS__rec_rwlock_print_stats("Actual stats", &stats);
    }
    /* clang-format on */
#endif

    /* 5) Obtain a read lock. */
    result = H5TS__rec_rwlock_rdlock(&lock);
    CHECK_I(result, "H5TS__rec_rwlock_rdlock");

    /* 6) Obtain the read lock a second time. */
    result = H5TS__rec_rwlock_rdlock(&lock);
    CHECK_I(result, "H5TS__rec_rwlock_rdlock");

    /* 7) Drop the read lock. */
    result = H5TS__rec_rwlock_rdunlock(&lock);
    CHECK_I(result, "H5TS__rec_rwlock_rdunlock");

    /* 8) Drop the read lock a second time. */
    result = H5TS__rec_rwlock_rdunlock(&lock);
    CHECK_I(result, "H5TS__rec_rwlock_rdunlock");

#if H5TS_ENABLE_REC_RWLOCK_STATS
    /* 9) Verify the expected stats, and then reset them. */
    result = H5TS__rec_rwlock_get_stats(&lock, &stats);
    CHECK_I(result, "H5TS__rec_rwlock_get_stats");

    result = H5TS__rec_rwlock_reset_stats(&lock);
    CHECK_I(result, "H5TS__rec_rwlock_reset_stats");

    /* clang-format makes this conditional unreadable, so turn it off. */
    /* clang-format off */
    if (stats.read_locks_granted             != 2 ||
        stats.read_locks_released            != 2 ||
        stats.real_read_locks_granted        != 1 ||
        stats.real_read_locks_released       != 1 ||
        stats.max_read_locks                 != 1 ||
        stats.max_read_lock_recursion_depth  != 2 ||
        stats.read_locks_delayed             != 0 ||
        stats.write_locks_granted            != 0 ||
        stats.write_locks_released           != 0 ||
        stats.real_write_locks_granted       != 0 ||
        stats.real_write_locks_released      != 0 ||
        stats.max_write_locks                != 0 ||
        stats.max_write_lock_recursion_depth != 0 ||
        stats.write_locks_delayed            != 0 ||
        stats.max_write_locks_pending        != 0 ) {

        TestErrPrintf("Unexpected recursive R/W lock stats -- 2");
        H5TS__rec_rwlock_print_stats("Actual stats", &stats);
    }
    /* clang-format on */
#endif

    /* 10) Obtain a write lock. */
    result = H5TS__rec_rwlock_wrlock(&lock);
    CHECK_I(result, "H5TS__rec_rwlock_wrlock");

    /* 11) Drop the write lock. */
    result = H5TS__rec_rwlock_wrunlock(&lock);
    CHECK_I(result, "H5TS__rec_rwlock_wrunlock");

#if H5TS_ENABLE_REC_RWLOCK_STATS
    /* 12) Verify the expected stats, and then reset them. */
    result = H5TS__rec_rwlock_get_stats(&lock, &stats);
    CHECK_I(result, "H5TS__rec_rwlock_get_stats");

    result = H5TS__rec_rwlock_reset_stats(&lock);
    CHECK_I(result, "H5TS__rec_rwlock_reset_stats");

    /* clang-format makes this conditional unreadable, so turn it off. */
    /* clang-format off */
    if (stats.read_locks_granted             != 0 ||
        stats.read_locks_released            != 0 ||
        stats.real_read_locks_granted        != 0 ||
        stats.real_read_locks_released       != 0 ||
        stats.max_read_locks                 != 0 ||
        stats.max_read_lock_recursion_depth  != 0 ||
        stats.read_locks_delayed             != 0 ||
        stats.write_locks_granted            != 1 ||
        stats.write_locks_released           != 1 ||
        stats.real_write_locks_granted       != 1 ||
        stats.real_write_locks_released      != 1 ||
        stats.max_write_locks                != 1 ||
        stats.max_write_lock_recursion_depth != 1 ||
        stats.write_locks_delayed            != 0 ||
        stats.max_write_locks_pending        != 0 ) {

        TestErrPrintf("Unexpected recursive R/W lock stats -- 3");
        H5TS__rec_rwlock_print_stats("Actual stats", &stats);
    }
    /* clang-format on */
#endif

    /* 13) Obtain a write lock. */
    result = H5TS__rec_rwlock_wrlock(&lock);
    CHECK_I(result, "H5TS__rec_rwlock_wrlock");

    /* 14) Obtain the write lock a second time. */
    result = H5TS__rec_rwlock_wrlock(&lock);
    CHECK_I(result, "H5TS__rec_rwlock_wrlock");

    /* 15) Drop the write lock. */
    result = H5TS__rec_rwlock_wrunlock(&lock);
    CHECK_I(result, "H5TS__rec_rwlock_wrunlock");

    /* 16) Drop the write lock a second time. */
    result = H5TS__rec_rwlock_wrunlock(&lock);
    CHECK_I(result, "H5TS__rec_rwlock_wrunlock");

#if H5TS_ENABLE_REC_RWLOCK_STATS
    /* 17) Verify the expected stats, and then reset them. */
    result = H5TS__rec_rwlock_get_stats(&lock, &stats);
    CHECK_I(result, "H5TS__rec_rwlock_get_stats");

    result = H5TS__rec_rwlock_reset_stats(&lock);
    CHECK_I(result, "H5TS__rec_rwlock_reset_stats");

    /* clang-format makes this conditional unreadable, so turn it off. */
    /* clang-format off */
    if (stats.read_locks_granted             != 0 ||
        stats.read_locks_released            != 0 ||
        stats.real_read_locks_granted        != 0 ||
        stats.real_read_locks_released       != 0 ||
        stats.max_read_locks                 != 0 ||
        stats.max_read_lock_recursion_depth  != 0 ||
        stats.read_locks_delayed             != 0 ||
        stats.write_locks_granted            != 2 ||
        stats.write_locks_released           != 2 ||
        stats.real_write_locks_granted       != 1 ||
        stats.real_write_locks_released      != 1 ||
        stats.max_write_locks                != 1 ||
        stats.max_write_lock_recursion_depth != 2 ||
        stats.write_locks_delayed            != 0 ||
        stats.max_write_locks_pending        != 0 ) {

        TestErrPrintf("Unexpected recursive R/W lock stats -- 4");
        H5TS__rec_rwlock_print_stats("Actual stats", &stats);
    }
    /* clang-format on */
#endif

    /* 18) Obtain a write lock. */
    result = H5TS__rec_rwlock_wrlock(&lock);
    CHECK_I(result, "H5TS__rec_rwlock_wrlock");

    /* 19) Attempt to obtain a read lock -- should fail. */
    result = H5TS__rec_rwlock_rdlock(&lock);
    VERIFY(result, FAIL, "H5TS__rec_rwlock_rdlock");

    /* 20) Drop the write lock. */
    result = H5TS__rec_rwlock_wrunlock(&lock);
    CHECK_I(result, "H5TS__rec_rwlock_wrunlock");

    /* 21) Obtain a read lock. */
    result = H5TS__rec_rwlock_rdlock(&lock);
    CHECK_I(result, "H5TS__rec_rwlock_rdlock");

    /* 22) Attempt to obtain a write lock -- should fail. */
    result = H5TS__rec_rwlock_wrlock(&lock);
    VERIFY(result, FAIL, "H5TS__rec_rwlock_wrlock");

    /* 23) Drop the read lock. */
    result = H5TS__rec_rwlock_rdunlock(&lock);
    CHECK_I(result, "H5TS__rec_rwlock_rdunlock");

#if H5TS_ENABLE_REC_RWLOCK_STATS
    /* 24) Verify the expected stats, and then reset them. */
    result = H5TS__rec_rwlock_get_stats(&lock, &stats);
    CHECK_I(result, "H5TS__rec_rwlock_get_stats");

    result = H5TS__rec_rwlock_reset_stats(&lock);
    CHECK_I(result, "H5TS__rec_rwlock_reset_stats");

    /* clang-format makes this conditional unreadable, so turn it off. */
    /* clang-format off */
    if (stats.read_locks_granted             != 1 ||
        stats.read_locks_released            != 1 ||
        stats.real_read_locks_granted        != 1 ||
        stats.real_read_locks_released       != 1 ||
        stats.max_read_locks                 != 1 ||
        stats.max_read_lock_recursion_depth  != 1 ||
        stats.read_locks_delayed             != 0 ||
        stats.write_locks_granted            != 1 ||
        stats.write_locks_released           != 1 ||
        stats.real_write_locks_granted       != 1 ||
        stats.real_write_locks_released      != 1 ||
        stats.max_write_locks                != 1 ||
        stats.max_write_lock_recursion_depth != 1 ||
        stats.write_locks_delayed            != 0 ||
        stats.max_write_locks_pending        != 0 ) {

        TestErrPrintf("Unexpected recursive R/W lock stats");
        H5TS__rec_rwlock_print_stats("Actual stats", &stats);
    }
    /* clang-format on */
#endif

    /* 25) Shut down the recursive R/W lock. */
    result = H5TS__rec_rwlock_destroy(&lock);
    CHECK_I(result, "H5TS__rec_rwlock_destroy");
} /* end tts_rec_rwlock_smoke_check_1() */

/*
 **********************************************************************
 * tts_rec_rwlock_smoke_check_2 -- mob of readers
 *
 * Multi-threaded test to check management of multiple readers ONLY by
 * the recursive R/W lock.  Test proceeds as follows:
 *
 *  1) Initialize an instance of the recursive R/W lock.
 *
 *  2) Setup the user data to be passed to each reader test thread.
 *
 *  3) Create the reader threads, each with its own user data.
 *     Activities of the reader threads is discussed in the header
 *     comment to tts_rec_rwlock_smoke_check_test_thread().
 *
 *  4) Wait for all threads to complete.
 *
 *  5) Examine the user data from the threads, to determine the
 *     total number of real and recursive read locks and unlocks.
 *
 *  6) Obtain the stats from the recursive R/W lock, and compare
 *     with the data gathered above.
 *
 *  7) Shut down the recursive R/W lock.
 *
 * The reader threads obtain and drop the read lock a specified
 * number of times.  Once a reader has a read lock, it does random
 * recursive read locks / unlocks until drops the read lock, and then
 * repeats the process until the specified number of read locks have
 * been acquired and dropped.
 *
 **********************************************************************
 */
void
tts_rec_rwlock_smoke_check_2(void H5_ATTR_UNUSED *params)
{
    herr_t                   result;
    int                      express_test;
    int                      i;
    int                      num_threads = MAX_NUM_THREADS;
    int                      lock_cycles = MAX_LOCK_CYCLES;
    H5TS_thread_t            threads[MAX_NUM_THREADS];
    rec_rwlock_test_udata_t *udata = NULL;
#if H5TS_ENABLE_REC_RWLOCK_STATS
    bool                    verbose                     = false;
    int32_t                 total_target_rd_lock_cycles = 0;
    int32_t                 total_target_wr_lock_cycles = 0;
    H5TS_rec_rwlock_stats_t stats;
    H5TS_rec_rwlock_stats_t expected;
#endif
    H5TS_rec_rwlock_t lock;

#if H5TS_ENABLE_REC_RWLOCK_STATS
    /* Reset expected stats fields to zero -- we will construct the expected
     * stats from the thread udata after completion.
     */
    memset(&expected, 0, sizeof(expected));
#endif

    /* Allocate the udata */
    udata = malloc(sizeof(*udata) * MAX_NUM_THREADS);
    if (NULL == udata) {
        TestErrPrintf("thread udata allocation failed.\n");

        /* We can't do anything without the udata, so just return */
        return;
    }

    /* Reduce # of threads and test cycles for higher levels of express testing */
    express_test = GetTestExpress();
    if (express_test >= H5_TEST_EXPRESS_FULL) {
        num_threads /= 2;
        lock_cycles /= 10;
    }
    if (express_test >= H5_TEST_EXPRESS_QUICK) {
        num_threads /= 2;
        lock_cycles /= 10;
    }
    if (express_test >= H5_TEST_EXPRESS_SMOKE_TEST) {
        num_threads /= 2;
        lock_cycles /= 10;
    }

    /* 1) Initialize an instance of the recursive R/W lock. */
    result = H5TS__rec_rwlock_init(&lock);
    CHECK_I(result, "H5TS__rec_rwlock_init");

    /* 2) Setup the user data to be passed to each reader test thread. */
    for (i = 0; i < MAX_NUM_THREADS; i++) {
        memset(&udata[i], 0, sizeof(udata[i]));
        udata[i].lock                     = &lock;
        udata[i].target_rd_lock_cycles    = lock_cycles;
        udata[i].max_recursive_lock_depth = 10;
    }

#if H5TS_ENABLE_REC_RWLOCK_STATS
    uint64_t start_time = H5_now_usec();
#endif
    /* 3) Create the reader threads, each with its own user data. */
    for (i = 0; i < num_threads; i++)
        if (H5TS_thread_create(&threads[i], tts_rec_rwlock_smoke_check_test_thread, &udata[i]) < 0)
            TestErrPrintf("thread # %d did not start", i);

    /* 4) Wait for all threads to complete. */
    for (i = 0; i < num_threads; i++)
        if (H5TS_thread_join(threads[i], NULL) < 0)
            TestErrPrintf("thread %d failed to join", i);
#if H5TS_ENABLE_REC_RWLOCK_STATS
    uint64_t end_time  = H5_now_usec();
    uint64_t elap_time = (unsigned long long)(end_time - start_time);
    if (verbose)
        fprintf(stdout, "elapsed usec: %" PRIu64 ", usec per lock_cycle = %" PRIu64 "\n", elap_time,
                (elap_time / (uint64_t)lock_cycles));
#endif

    /* 5) Examine the user data from the threads, to determine the
     *    total number of real and recursive read locks and unlocks.
     *
     *    First, tally up the lock entries and exits from the test threads,
     *    and store this data in the expected recursive R/W/ lock stats..
     *    In passing, verify that each thread has done the expected number
     *    of locks and unlocks.  Do these as asserts -- will run checks on
     *    aggregate data shortly.
     */

    for (i = 0; i < num_threads; i++) {
        assert(udata[i].target_rd_lock_cycles == udata[i].real_read_locks_granted);
        assert(udata[i].target_rd_lock_cycles == udata[i].real_read_locks_released);
        assert(udata[i].target_wr_lock_cycles == udata[i].real_write_locks_granted);
        assert(udata[i].target_wr_lock_cycles == udata[i].real_write_locks_released);

#if H5TS_ENABLE_REC_RWLOCK_STATS
        total_target_rd_lock_cycles += udata[i].target_rd_lock_cycles;
        total_target_wr_lock_cycles += udata[i].target_wr_lock_cycles;

        expected.read_locks_granted += udata[i].read_locks_granted;
        expected.read_locks_released += udata[i].read_locks_released;
        expected.real_read_locks_granted += udata[i].real_read_locks_granted;
        expected.real_read_locks_released += udata[i].real_read_locks_released;
        expected.write_locks_granted += udata[i].write_locks_granted;
        expected.write_locks_released += udata[i].write_locks_released;
        expected.real_write_locks_granted += udata[i].real_write_locks_granted;
        expected.real_write_locks_released += udata[i].real_write_locks_released;
#endif
    }

#if H5TS_ENABLE_REC_RWLOCK_STATS
    /* Verify that the threads executed the expected number of read and write
     * lock cycles.  If they didn't, some thread probably encountered an error
     * and exited early.
     */
    if (total_target_rd_lock_cycles != expected.real_read_locks_granted ||
        total_target_rd_lock_cycles != expected.real_read_locks_released ||
        total_target_wr_lock_cycles != expected.real_write_locks_granted ||
        total_target_wr_lock_cycles != expected.real_write_locks_released)
        TestErrPrintf("Threads reported unexpected number of locks/unlocks.\n");

    /* initialize remaining non-zero fields in the expected stats */
    expected.max_read_locks                = num_threads;
    expected.max_read_lock_recursion_depth = 10;

    /* 6) Obtain the stats from the recursive R/W lock, and compare
     *     with the data gathered above.
     */

    result = H5TS__rec_rwlock_get_stats(&lock, &stats);
    CHECK_I(result, "H5TS__rec_rwlock_get_stats");

    /* turn off clang-format for readability */
    /* clang-format off */
    if (stats.read_locks_granted                 != expected.read_locks_granted ||
            stats.read_locks_released            != expected.read_locks_released ||
            stats.real_read_locks_granted        != expected.real_read_locks_granted ||
            stats.real_read_locks_released       != expected.real_read_locks_released ||
            stats.max_read_locks                  > expected.max_read_locks ||
            stats.max_read_locks                  < 1 ||
            stats.max_read_lock_recursion_depth   > expected.max_read_lock_recursion_depth ||
            stats.max_read_lock_recursion_depth   < 1 ||
            stats.read_locks_delayed             != expected.read_locks_delayed ||
            stats.write_locks_granted            != expected.write_locks_granted ||
            stats.write_locks_released           != expected.write_locks_released ||
            stats.real_write_locks_granted       != expected.real_write_locks_granted ||
            stats.real_write_locks_released      != expected.real_write_locks_released ||
            stats.max_write_locks                != expected.max_write_locks ||
            stats.max_write_lock_recursion_depth != expected.max_write_lock_recursion_depth ||
            stats.write_locks_delayed            != expected.write_locks_delayed ||
            stats.max_write_locks_pending        != expected.max_write_locks_pending) {
        TestErrPrintf("Unexpected recursive R/W lock stats");
        H5TS__rec_rwlock_print_stats("Actual stats", &stats);
        H5TS__rec_rwlock_print_stats("Expected stats", &expected);
    }
    /* clang-format on */

    if (verbose)
        H5TS__rec_rwlock_print_stats("mob of readers stats", &stats);
#endif

    /* 7) Shut down the recursive R/W lock. */
    result = H5TS__rec_rwlock_destroy(&lock);
    CHECK_I(result, "H5TS__rec_rwlock_destroy");

    /* discard the udata if it exists */
    if (udata)
        free(udata);
} /* end tts_rec_rwlock_smoke_check_2() */

/*
 **********************************************************************
 * tts_rec_rwlock_smoke_check_3 -- mob of writers
 *
 * Multi-thread test to check management of multiple writers ONLY by
 * the recursive R/W lock.  Test proceeds as follows:
 *
 *  1) Initialize an instance of the recursive R/W lock.
 *
 *  2) Setup the user data to be passed to each writer test thread.
 *
 *  3) Create the writer threads, each with its own user data.
 *     Activities of the writer threads is discussed in the header
 *     comment to tts_rec_rwlock_smoke_check_test_thread().
 *
 *  4) Wait for all threads to complete.
 *
 *  5) Examine the user data from the threads, to determine the
 *     total number of real and recursive read locks and unlock.
 *
 *  6) Obtain the stats from the recursive R/W lock, and compare
 *     with the data gathered above.
 *
 *  7) Shut down the recursive R/W lock.
 *
 * The writer threads obtain and drop the read lock a specified
 * number of times.  Once a writeer has a write lock, it does random
 * recursive write locks / unlocks until drops the write lock, and then
 * repeats the process until the specified number of write locks have
 * been acquired and dropped.
 *
 **********************************************************************
 */
void
tts_rec_rwlock_smoke_check_3(void H5_ATTR_UNUSED *params)
{
    herr_t                   result;
    int                      i;
    int                      express_test;
    int                      num_threads = MAX_NUM_THREADS;
    int                      lock_cycles = MAX_LOCK_CYCLES;
    H5TS_thread_t            threads[MAX_NUM_THREADS];
    rec_rwlock_test_udata_t *udata = NULL;
#if H5TS_ENABLE_REC_RWLOCK_STATS
    bool                    verbose                     = false;
    int32_t                 total_target_rd_lock_cycles = 0;
    int32_t                 total_target_wr_lock_cycles = 0;
    H5TS_rec_rwlock_stats_t stats;
    H5TS_rec_rwlock_stats_t expected;
#endif
    H5TS_rec_rwlock_t lock;

#if H5TS_ENABLE_REC_RWLOCK_STATS
    /* Reset expected stats fields to zero -- we will construct the expected
     * stats from the thread udata after completion.
     */
    memset(&expected, 0, sizeof(expected));
#endif

    /* Allocate the udata */
    udata = malloc(sizeof(*udata) * MAX_NUM_THREADS);
    if (udata == NULL) {
        TestErrPrintf("thread udata allocation failed.\n");

        /* We can't do anything without the udata, so just return */
        return;
    }

    /* Reduce # of threads and test cycles for higher levels of express testing */
    express_test = GetTestExpress();
    if (express_test >= H5_TEST_EXPRESS_FULL) {
        num_threads /= 2;
        lock_cycles /= 10;
    }
    if (express_test >= H5_TEST_EXPRESS_QUICK) {
        num_threads /= 2;
        lock_cycles /= 10;
    }
    if (express_test >= H5_TEST_EXPRESS_SMOKE_TEST) {
        num_threads /= 2;
        lock_cycles /= 10;
    }

    /* 1) Initialize an instance of the recursive R/W lock. */
    result = H5TS__rec_rwlock_init(&lock);
    CHECK_I(result, "H5TS__rec_rwlock_init");

    /* 2) Setup the user data to be passed to each writer test thread. */
    for (i = 0; i < MAX_NUM_THREADS; i++) {
        memset(&udata[i], 0, sizeof(udata[i]));
        udata[i].lock                     = &lock;
        udata[i].target_wr_lock_cycles    = lock_cycles;
        udata[i].max_recursive_lock_depth = 10;
    }

#if H5TS_ENABLE_REC_RWLOCK_STATS
    uint64_t start_time = H5_now_usec();
#endif
    /* 3) Create the writer threads, each with its own user data. */
    for (i = 0; i < num_threads; i++)
        if (H5TS_thread_create(&threads[i], tts_rec_rwlock_smoke_check_test_thread, &udata[i]) < 0)
            TestErrPrintf("thread # %d did not start", i);

    /* 4) Wait for all threads to complete. */
    for (i = 0; i < num_threads; i++)
        if (H5TS_thread_join(threads[i], NULL) < 0)
            TestErrPrintf("thread %d failed to join", i);
#if H5TS_ENABLE_REC_RWLOCK_STATS
    uint64_t end_time  = H5_now_usec();
    uint64_t elap_time = (unsigned long long)(end_time - start_time);
    if (verbose)
        fprintf(stdout, "elapsed usec: %" PRIu64 ", usec per lock_cycle = %" PRIu64 "\n", elap_time,
                (elap_time / (uint64_t)lock_cycles));
#endif

    /* 5) Examine the user data from the threads, to determine the
     *    total number of real and recursive read locks and unlock.
     *
     *    First, tally up the lock entries and exits from the test threads,
     *    and store this data in the expected recursive R/W/ lock stats..
     *    In passing, verify that each thread has done the expected number
     *    of locks and unlocks.  Do these as asserts -- will run checks on
     *    aggregate data shortly.
     */

    for (i = 0; i < num_threads; i++) {
        assert(udata[i].target_rd_lock_cycles == udata[i].real_read_locks_granted);
        assert(udata[i].target_rd_lock_cycles == udata[i].real_read_locks_released);
        assert(udata[i].target_wr_lock_cycles == udata[i].real_write_locks_granted);
        assert(udata[i].target_wr_lock_cycles == udata[i].real_write_locks_released);

#if H5TS_ENABLE_REC_RWLOCK_STATS
        total_target_rd_lock_cycles += udata[i].target_rd_lock_cycles;
        total_target_wr_lock_cycles += udata[i].target_wr_lock_cycles;

        expected.read_locks_granted += udata[i].read_locks_granted;
        expected.read_locks_released += udata[i].read_locks_released;
        expected.real_read_locks_granted += udata[i].real_read_locks_granted;
        expected.real_read_locks_released += udata[i].real_read_locks_released;
        expected.write_locks_granted += udata[i].write_locks_granted;
        expected.write_locks_released += udata[i].write_locks_released;
        expected.real_write_locks_granted += udata[i].real_write_locks_granted;
        expected.real_write_locks_released += udata[i].real_write_locks_released;
#endif
    }

#if H5TS_ENABLE_REC_RWLOCK_STATS
    /* Verify that the threads executed the expected number of read and write
     * lock cycles.  If they didn't, some thread probably encountered an error
     * and exited early.
     */
    if (total_target_rd_lock_cycles != expected.real_read_locks_granted ||
        total_target_rd_lock_cycles != expected.real_read_locks_released ||
        total_target_wr_lock_cycles != expected.real_write_locks_granted ||
        total_target_wr_lock_cycles != expected.real_write_locks_released)
        TestErrPrintf("Threads reported unexpected number of locks/unlocks.\n");

    /* initialize remaining non-zero fields in the expected stats */
    expected.max_write_locks                = 1;
    expected.max_write_lock_recursion_depth = 10;
    expected.max_write_locks_pending        = num_threads - 1;

    /* 6) Obtain the stats from the recursive R/W lock, and compare
     *     with the data gathered above.
     */
    result = H5TS__rec_rwlock_get_stats(&lock, &stats);
    CHECK_I(result, "H5TS__rec_rwlock_get_stats");

    /* turn off clang-format for readability */
    /* clang-format off */
    if (stats.read_locks_granted                 != expected.read_locks_granted ||
            stats.read_locks_released            != expected.read_locks_released ||
            stats.real_read_locks_granted        != expected.real_read_locks_granted ||
            stats.real_read_locks_released       != expected.real_read_locks_released ||
            stats.max_read_locks                 != expected.max_read_locks ||
            stats.max_read_lock_recursion_depth  != expected.max_read_lock_recursion_depth ||
            stats.read_locks_delayed             != expected.read_locks_delayed ||
            stats.write_locks_granted            != expected.write_locks_granted ||
            stats.write_locks_released           != expected.write_locks_released ||
            stats.real_write_locks_granted       != expected.real_write_locks_granted ||
            stats.real_write_locks_released      != expected.real_write_locks_released ||
            stats.max_write_locks                != expected.max_write_locks ||
            stats.max_write_lock_recursion_depth  > expected.max_write_lock_recursion_depth ||
            stats.max_write_lock_recursion_depth  < 1 ||
            stats.write_locks_delayed             < expected.write_locks_delayed ||
            stats.max_write_locks_pending         > expected.max_write_locks_pending) {
        TestErrPrintf("Unexpected recursive R/W lock stats");
        H5TS__rec_rwlock_print_stats("Actual stats", &stats);
        H5TS__rec_rwlock_print_stats("Expected stats", &expected);
    }
    /* clang-format on */

    if (verbose)
        H5TS__rec_rwlock_print_stats("Actual stats", &stats);
#endif

    /* 7) Shut down the recursive R/W lock. */
    result = H5TS__rec_rwlock_destroy(&lock);
    CHECK_I(result, "H5TS__rec_rwlock_destroy");

    /* discard the udata if it exists */
    if (udata)
        free(udata);
} /* end tts_rec_rwlock_smoke_check_3() */

/*
 **********************************************************************
 * tts_rec_rwlock_smoke_check_4 -- mixed mob
 *
 * Multi-thread test to check management of multiple readers and
 * writers by the recursive R/W lock.  Test proceeds as follows:
 *
 *  1) Initialize an instance of the recursive R/W lock.
 *
 *  2) Setup the user data to be passed to each writer test thread.
 *
 *  3) Create the reader / writer threads, each with its own user data.
 *     Activities of the reader / writer threads is discussed in the
 *     header comment to tts_rec_rwlock_smoke_check_test_thread().
 *
 *  4) Wait for all threads to complete.
 *
 *  5) Examine the user data from the threads, to determine the
 *     total number of real and recursive read & write locks and
 *     unlock.
 *
 *  6) Obtain the stats from the recursive R/W lock, and compare
 *     with the data gathered above.
 *
 *  7) Shut down the recursive R/W lock.
 *
 * The reader / writer threads obtain and drop the read or write
 * locks a specified number of times.  Once a thread has a lock, it
 * does random recursive locks / unlocks until drops the lock, and then
 * repeats the process until the specified number of locks have
 * been acquired and dropped.
 *
 **********************************************************************
 */
void
tts_rec_rwlock_smoke_check_4(void H5_ATTR_UNUSED *params)
{
    herr_t                   result;
    int                      i;
    int                      express_test;
    int                      num_threads = MAX_NUM_THREADS;
    int                      lock_cycles = MAX_LOCK_CYCLES;
    H5TS_thread_t            threads[MAX_NUM_THREADS];
    rec_rwlock_test_udata_t *udata = NULL;
#if H5TS_ENABLE_REC_RWLOCK_STATS
    bool                    verbose                     = false;
    int32_t                 total_target_rd_lock_cycles = 0;
    int32_t                 total_target_wr_lock_cycles = 0;
    H5TS_rec_rwlock_stats_t stats;
    H5TS_rec_rwlock_stats_t expected;
#endif
    H5TS_rec_rwlock_t lock;

#if H5TS_ENABLE_REC_RWLOCK_STATS
    /* Reset expected stats fields to zero -- we will construct the expected
     * stats from the thread udata after completion.
     */
    memset(&expected, 0, sizeof(expected));
#endif

    /* Allocate the udata */
    udata = malloc(sizeof(*udata) * MAX_NUM_THREADS);
    if (udata == NULL) {
        TestErrPrintf("thread udata allocation failed.\n");

        /* We can't do anything without the udata, so just return */
        return;
    }

    /* Reduce # of threads and test cycles for higher levels of express testing */
    express_test = GetTestExpress();
    if (express_test >= H5_TEST_EXPRESS_FULL) {
        num_threads /= 2;
        lock_cycles /= 10;
    }
    if (express_test >= H5_TEST_EXPRESS_QUICK) {
        num_threads /= 2;
        lock_cycles /= 10;
    }
    if (express_test >= H5_TEST_EXPRESS_SMOKE_TEST) {
        num_threads /= 2;
        lock_cycles /= 10;
    }

    /* 1) Initialize an instance of the recursive R/W lock. */
    result = H5TS__rec_rwlock_init(&lock);
    CHECK_I(result, "H5TS__rec_rwlock_init");

    /* 2) Setup the user data to be passed to each writer test thread. */
    for (i = 0; i < MAX_NUM_THREADS; i++) {
        memset(&udata[i], 0, sizeof(udata[i]));
        udata[i].lock                     = &lock;
        udata[i].target_rd_lock_cycles    = lock_cycles;
        udata[i].target_wr_lock_cycles    = lock_cycles;
        udata[i].max_recursive_lock_depth = 10;
    }

#if H5TS_ENABLE_REC_RWLOCK_STATS
    uint64_t start_time = H5_now_usec();
#endif
    /* 3) Create the reader threads, each with its own user data. */
    for (i = 0; i < num_threads; i++)
        if (H5TS_thread_create(&threads[i], tts_rec_rwlock_smoke_check_test_thread, &udata[i]) < 0)
            TestErrPrintf("thread # %d did not start", i);

    /* 4) Wait for all threads to complete. */
    for (i = 0; i < num_threads; i++)
        if (H5TS_thread_join(threads[i], NULL) < 0)
            TestErrPrintf("thread %d failed to join", i);
#if H5TS_ENABLE_REC_RWLOCK_STATS
    uint64_t end_time  = H5_now_usec();
    uint64_t elap_time = (unsigned long long)(end_time - start_time);
    if (verbose)
        fprintf(stdout, "elapsed usec: %" PRIu64 ", usec per lock_cycle = %" PRIu64 "\n", elap_time,
                (elap_time / (uint64_t)lock_cycles));
#endif

    /* 5) Examine the user data from the threads, to determine the
     *    total number of real and recursive read locks and unlock.
     *
     *    First, tally up the lock entries and exits from the test threads,
     *    and store this data in the expected recursive R/W/ lock stats..
     *    In passing, verify that each thread has done the expected number
     *    of locks and unlocks.  Do these as asserts -- will run checks on
     *    aggregate data shortly.
     */

    for (i = 0; i < num_threads; i++) {
        assert(udata[i].target_rd_lock_cycles == udata[i].real_read_locks_granted);
        assert(udata[i].target_rd_lock_cycles == udata[i].real_read_locks_released);
        assert(udata[i].target_wr_lock_cycles == udata[i].real_write_locks_granted);
        assert(udata[i].target_wr_lock_cycles == udata[i].real_write_locks_released);

#if H5TS_ENABLE_REC_RWLOCK_STATS
        total_target_rd_lock_cycles += udata[i].target_rd_lock_cycles;
        total_target_wr_lock_cycles += udata[i].target_wr_lock_cycles;

        expected.read_locks_granted += udata[i].read_locks_granted;
        expected.read_locks_released += udata[i].read_locks_released;
        expected.real_read_locks_granted += udata[i].real_read_locks_granted;
        expected.real_read_locks_released += udata[i].real_read_locks_released;
        expected.write_locks_granted += udata[i].write_locks_granted;
        expected.write_locks_released += udata[i].write_locks_released;
        expected.real_write_locks_granted += udata[i].real_write_locks_granted;
        expected.real_write_locks_released += udata[i].real_write_locks_released;
#endif
    }

#if H5TS_ENABLE_REC_RWLOCK_STATS
    /* Verify that the threads executed the expected number of read and write
     * lock cycles.  If they didn't, some thread probably encountered an error
     * and exited early.
     */
    if (total_target_rd_lock_cycles != expected.real_read_locks_granted ||
        total_target_rd_lock_cycles != expected.real_read_locks_released ||
        total_target_wr_lock_cycles != expected.real_write_locks_granted ||
        total_target_wr_lock_cycles != expected.real_write_locks_released)
        TestErrPrintf("Threads reported unexpected number of locks/unlocks.\n");

    /* initialize remaining non-zero fields in the expected stats */
    expected.max_read_locks                 = num_threads;
    expected.max_read_lock_recursion_depth  = 10;
    expected.max_write_locks                = 1;
    expected.max_write_lock_recursion_depth = 10;
    expected.max_write_locks_pending        = num_threads - 1;

    /* 6) Obtain the stats from the recursive R/W lock, and compare
     *     with the data gathered above.
     */
    result = H5TS__rec_rwlock_get_stats(&lock, &stats);
    CHECK_I(result, "H5TS__rec_rwlock_get_stats");

    /* turn off clang-format for readability */
    /* clang-format off */
    if (stats.read_locks_granted                 != expected.read_locks_granted ||
            stats.read_locks_released            != expected.read_locks_released ||
            stats.real_read_locks_granted        != expected.real_read_locks_granted ||
            stats.real_read_locks_released       != expected.real_read_locks_released ||
            stats.max_read_locks                  > expected.max_read_locks ||
            stats.max_read_locks                  < 1 ||
            stats.max_read_lock_recursion_depth   > expected.max_read_lock_recursion_depth ||
            stats.read_locks_delayed              < expected.read_locks_delayed ||
            stats.write_locks_granted            != expected.write_locks_granted ||
            stats.write_locks_released           != expected.write_locks_released ||
            stats.real_write_locks_granted       != expected.real_write_locks_granted ||
            stats.real_write_locks_released      != expected.real_write_locks_released ||
            stats.max_write_locks                != expected.max_write_locks ||
            stats.max_write_lock_recursion_depth  > expected.max_write_lock_recursion_depth ||
            stats.max_write_lock_recursion_depth  < 1 ||
            stats.write_locks_delayed             < expected.write_locks_delayed ||
            stats.max_write_locks_pending         > expected.max_write_locks_pending) {
        TestErrPrintf("Unexpected recursive R/W lock stats");
        H5TS__rec_rwlock_print_stats("Actual stats", &stats);
        H5TS__rec_rwlock_print_stats("Expected stats", &expected);
    }
    /* clang-format on */

    if (verbose)
        H5TS__rec_rwlock_print_stats("Actual stats", &stats);
#endif

    /* 7) Shut down the recursive R/W lock. */
    result = H5TS__rec_rwlock_destroy(&lock);
    CHECK_I(result, "H5TS__rec_rwlock_destroy");

    /* discard the udata if it exists */
    if (udata)
        free(udata);
} /* end tts_rec_rwlock_smoke_check_4() */

#endif /* H5_HAVE_WIN_THREADS */
#endif /* H5_HAVE_THREADS */
