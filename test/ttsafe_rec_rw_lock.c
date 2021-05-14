/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
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
 * Created: Sept. 3, 2020
 * Programmer: John Mainzer
 *
 ********************************************************************/

#include "ttsafe.h"

/* Include library header files */
#include "H5ACprivate.h"

#ifdef H5_USE_RECURSIVE_WRITER_LOCKS

#ifdef H5_HAVE_THREADSAFE

#define MAX_NUM_THREADS 32
#define MAX_LOCK_CYCLES 1000000

/* structure used to configure test threads in the recurive
 * R/W/ lock tests.
 */
/***********************************************************************
 *
 * Structure rec_rw_lock_test_udata_t
 *
 * Arrays of instances of rec_rw_lock_test_udata_t are used to configure
 * the threads used to test the recursive R/W lock, and to collect
 * statistics on their behaviour.  These statistics are aggregated and
 * used to cross check the statistics collected by the recursive R/W
 * lock proper.
 *
 * The fields of the structure are discussed below:
 *
 * rw_lock: Pointer to the recursive R/W under test.
 *
 * id:          ID assigned to the target thread.  Used primarily for
 *              sanity checking.
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
 * H5TS_rw_lock_stats_t.  See the header comment for that
 * structure (in H5TSprivate.h) for further details.
 *
 *                                         JRM -- 9/3/20
 *
 ***********************************************************************/
typedef struct rec_rw_lock_test_udata_t {

    /* thread control fields */
    H5TS_rw_lock_t *rw_lock;
    int32_t         id;
    int32_t         target_rd_lock_cycles;
    int32_t         target_wr_lock_cycles;
    int32_t         max_recursive_lock_depth;

    /* thread stats fields */
    int64_t read_locks_granted;
    int64_t read_locks_released;
    int64_t real_read_locks_granted;
    int64_t real_read_locks_released;
    int64_t write_locks_granted;
    int64_t write_locks_released;
    int64_t real_write_locks_granted;
    int64_t real_write_locks_released;

} rec_rw_lock_test_udata_t;

void *tts_rw_lock_smoke_check_test_thread(void *_udata);

/*
 **********************************************************************
 * tts_rec_rw_lock_smoke_check_1
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
 *  9) Verify the exptected stats, and then reset them.
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
 * Creted Sept. 3. 2020.
 *
 * Programmer: John Mainzer
 *
 **********************************************************************
 */
void
tts_rec_rw_lock_smoke_check_1(void)
{
    herr_t                      result;
    struct H5TS_rw_lock_stats_t stats;
    struct H5TS_rw_lock_t       rec_rw_lock;

    /* 1) Initialize an instance of the recursive R/W lock. */
    result = H5TS_rw_lock_init(&rec_rw_lock, H5TS_RW_LOCK_POLICY_FAVOR_WRITERS);
    CHECK_I(result, "H5TS_rw_lock_init");

    /* 2) Obtain a read lock. */
    result = H5TS_rw_rdlock(&rec_rw_lock);
    CHECK_I(result, "H5TS_rw_rdlock -- 1");

    /* 3) Drop the read lock. */
    result = H5TS_rw_unlock(&rec_rw_lock);
    CHECK_I(result, "H5TS_rw_unlock -- 1");

    /* 4) Verify the expected stats, and then reset them. */
    result = H5TS_rw_lock_get_stats(&rec_rw_lock, &stats);
    CHECK_I(result, "H5TS_rw_lock_get_stats -- 1");

    result = H5TS_rw_lock_reset_stats(&rec_rw_lock);
    CHECK_I(result, "H5TS_rw_lock_reset_stats -- 1");

    /* clang-format makes this conditional unreadable, so turn it off. */
    /* clang-format off */
    if ( ( stats.read_locks_granted             != 1 ) ||
         ( stats.read_locks_released            != 1 ) ||
         ( stats.real_read_locks_granted        != 1 ) ||
         ( stats.real_read_locks_released       != 1 ) ||
         ( stats.max_read_locks                 != 1 ) ||
         ( stats.max_read_lock_recursion_depth  != 1 ) ||
         ( stats.read_locks_delayed             != 0 ) ||
         ( stats.max_read_locks_pending         != 0 ) ||
         ( stats.write_locks_granted            != 0 ) ||
         ( stats.write_locks_released           != 0 ) ||
         ( stats.real_write_locks_granted       != 0 ) ||
         ( stats.real_write_locks_released      != 0 ) ||
         ( stats.max_write_locks                != 0 ) ||
         ( stats.max_write_lock_recursion_depth != 0 ) ||
         ( stats.write_locks_delayed            != 0 ) ||
         ( stats.max_write_locks_pending        != 0 ) ) {

        TestErrPrintf("Unexpected recursive R/W lock stats -- 1");
        H5TS_rw_lock_print_stats("Actual stats", &stats);
    }
    /* clang-format on */

    /* 5) Obtain a read lock. */
    result = H5TS_rw_rdlock(&rec_rw_lock);
    CHECK_I(result, "H5TS_rw_rdlock -- 2");

    /* 6) Obtain the read lock a second time. */
    result = H5TS_rw_rdlock(&rec_rw_lock);
    CHECK_I(result, "H5TS_rw_rdlock -- 3");

    /* 7) Drop the read lock. */
    result = H5TS_rw_unlock(&rec_rw_lock);
    CHECK_I(result, "H5TS_rw_unlock -- 2");

    /* 8) Drop the read lock a second time. */
    result = H5TS_rw_unlock(&rec_rw_lock);
    CHECK_I(result, "H5TS_rw_unlock -- 3");

    /* 9) Verify the exptected stats, and then reset them. */
    result = H5TS_rw_lock_get_stats(&rec_rw_lock, &stats);
    CHECK_I(result, "H5TS_rw_lock_get_stats -- 2");

    result = H5TS_rw_lock_reset_stats(&rec_rw_lock);
    CHECK_I(result, "H5TS_rw_lock_reset_stats -- 2");

    /* clang-format makes this conditional unreadable, so turn it off. */
    /* clang-format off */
    if ( ( stats.read_locks_granted             != 2 ) ||
         ( stats.read_locks_released            != 2 ) ||
         ( stats.real_read_locks_granted        != 1 ) ||
         ( stats.real_read_locks_released       != 1 ) ||
         ( stats.max_read_locks                 != 1 ) ||
         ( stats.max_read_lock_recursion_depth  != 2 ) ||
         ( stats.read_locks_delayed             != 0 ) ||
         ( stats.max_read_locks_pending         != 0 ) ||
         ( stats.write_locks_granted            != 0 ) ||
         ( stats.write_locks_released           != 0 ) ||
         ( stats.real_write_locks_granted       != 0 ) ||
         ( stats.real_write_locks_released      != 0 ) ||
         ( stats.max_write_locks                != 0 ) ||
         ( stats.max_write_lock_recursion_depth != 0 ) ||
         ( stats.write_locks_delayed            != 0 ) ||
         ( stats.max_write_locks_pending        != 0 ) ) {

        TestErrPrintf("Unexpected recursive R/W lock stats -- 2");
        H5TS_rw_lock_print_stats("Actual stats", &stats);
    }
    /* clang-format on */

    /* 10) Obtain a write lock. */
    result = H5TS_rw_wrlock(&rec_rw_lock);
    CHECK_I(result, "H5TS_rw_wrlock -- 1");

    /* 11) Drop the write lock. */
    result = H5TS_rw_unlock(&rec_rw_lock);
    CHECK_I(result, "H5TS_rw_unlock -- 4");

    /* 12) Verify the expected stats, and then reset them. */
    result = H5TS_rw_lock_get_stats(&rec_rw_lock, &stats);
    CHECK_I(result, "H5TS_rw_lock_get_stats -- 3");

    result = H5TS_rw_lock_reset_stats(&rec_rw_lock);
    CHECK_I(result, "H5TS_rw_lock_reset_stats -- 3");

    /* clang-format makes this conditional unreadable, so turn it off. */
    /* clang-format off */
    if ( ( stats.read_locks_granted             != 0 ) ||
         ( stats.read_locks_released            != 0 ) ||
         ( stats.real_read_locks_granted        != 0 ) ||
         ( stats.real_read_locks_released       != 0 ) ||
         ( stats.max_read_locks                 != 0 ) ||
         ( stats.max_read_lock_recursion_depth  != 0 ) ||
         ( stats.read_locks_delayed             != 0 ) ||
         ( stats.max_read_locks_pending         != 0 ) ||
         ( stats.write_locks_granted            != 1 ) ||
         ( stats.write_locks_released           != 1 ) ||
         ( stats.real_write_locks_granted       != 1 ) ||
         ( stats.real_write_locks_released      != 1 ) ||
         ( stats.max_write_locks                != 1 ) ||
         ( stats.max_write_lock_recursion_depth != 1 ) ||
         ( stats.write_locks_delayed            != 0 ) ||
         ( stats.max_write_locks_pending        != 0 ) ) {

        TestErrPrintf("Unexpected recursive R/W lock stats -- 3");
        H5TS_rw_lock_print_stats("Actual stats", &stats);
    }
    /* clang-format on */

    /* 13) Obtain a write lock. */
    result = H5TS_rw_wrlock(&rec_rw_lock);
    CHECK_I(result, "H5TS_rw_wrlock -- 2");

    /* 14) Obtain the write lock a second time. */
    result = H5TS_rw_wrlock(&rec_rw_lock);
    CHECK_I(result, "H5TS_rw_wrlock -- 3");

    /* 15) Drop the write lock. */
    result = H5TS_rw_unlock(&rec_rw_lock);
    CHECK_I(result, "H5TS_rw_unlock -- 5");

    /* 16) Drop the write lock a second time. */
    result = H5TS_rw_unlock(&rec_rw_lock);
    CHECK_I(result, "H5TS_rw_unlock -- 6");

    /* 17) Verify the expected stats, and then reset them. */
    result = H5TS_rw_lock_get_stats(&rec_rw_lock, &stats);
    CHECK_I(result, "H5TS_rw_lock_get_stats -- 4");

    result = H5TS_rw_lock_reset_stats(&rec_rw_lock);
    CHECK_I(result, "H5TS_rw_lock_reset_stats -- 4");

    /* clang-format makes this conditional unreadable, so turn it off. */
    /* clang-format off */
    if ( ( stats.read_locks_granted             != 0 ) ||
         ( stats.read_locks_released            != 0 ) ||
         ( stats.real_read_locks_granted        != 0 ) ||
         ( stats.real_read_locks_released       != 0 ) ||
         ( stats.max_read_locks                 != 0 ) ||
         ( stats.max_read_lock_recursion_depth  != 0 ) ||
         ( stats.read_locks_delayed             != 0 ) ||
         ( stats.max_read_locks_pending         != 0 ) ||
         ( stats.write_locks_granted            != 2 ) ||
         ( stats.write_locks_released           != 2 ) ||
         ( stats.real_write_locks_granted       != 1 ) ||
         ( stats.real_write_locks_released      != 1 ) ||
         ( stats.max_write_locks                != 1 ) ||
         ( stats.max_write_lock_recursion_depth != 2 ) ||
         ( stats.write_locks_delayed            != 0 ) ||
         ( stats.max_write_locks_pending        != 0 ) ) {

        TestErrPrintf("Unexpected recursive R/W lock stats -- 4");
        H5TS_rw_lock_print_stats("Actual stats", &stats);
    }
    /* clang-format on */

    /* 18) Obtain a write lock. */
    result = H5TS_rw_wrlock(&rec_rw_lock);
    CHECK_I(result, "H5TS_rw_wrlock -- 4");

    /* 19) Attempt to obtain a read lock -- should fail. */
    result = H5TS_rw_rdlock(&rec_rw_lock);
    VERIFY(result, FAIL, "H5TS_rw_rdlock -- 4");

    /* 20) Drop the write lock. */
    result = H5TS_rw_unlock(&rec_rw_lock);
    CHECK_I(result, "H5TS_rw_unlock -- 6");

    /* 21) Obtain a read lock. */
    result = H5TS_rw_rdlock(&rec_rw_lock);
    CHECK_I(result, "H5TS_rw_rdlock -- 5");

    /* 22) Attempt to obtain a write lock -- should fail. */
    result = H5TS_rw_wrlock(&rec_rw_lock);
    VERIFY(result, FAIL, "H5TS_rw_wrlock -- 5");

    /* 23) Drop the read lock. */
    result = H5TS_rw_unlock(&rec_rw_lock);
    CHECK_I(result, "H5TS_rw_unlock -- 6");

    /* 24) Verify the expected stats, and then reset them. */
    result = H5TS_rw_lock_get_stats(&rec_rw_lock, &stats);
    CHECK_I(result, "H5TS_rw_lock_get_stats -- 5");

    result = H5TS_rw_lock_reset_stats(&rec_rw_lock);
    CHECK_I(result, "H5TS_rw_lock_reset_stats -- 5");

    /* clang-format makes this conditional unreadable, so turn it off. */
    /* clang-format off */
    if ( ( stats.read_locks_granted             != 1 ) ||
         ( stats.read_locks_released            != 1 ) ||
         ( stats.real_read_locks_granted        != 1 ) ||
         ( stats.real_read_locks_released       != 1 ) ||
         ( stats.max_read_locks                 != 1 ) ||
         ( stats.max_read_lock_recursion_depth  != 1 ) ||
         ( stats.read_locks_delayed             != 0 ) ||
         ( stats.max_read_locks_pending         != 0 ) ||
         ( stats.write_locks_granted            != 1 ) ||
         ( stats.write_locks_released           != 1 ) ||
         ( stats.real_write_locks_granted       != 1 ) ||
         ( stats.real_write_locks_released      != 1 ) ||
         ( stats.max_write_locks                != 1 ) ||
         ( stats.max_write_lock_recursion_depth != 1 ) ||
         ( stats.write_locks_delayed            != 0 ) ||
         ( stats.max_write_locks_pending        != 0 ) ) {

        TestErrPrintf("Unexpected recursive R/W lock stats -- 5");
        H5TS_rw_lock_print_stats("Actual stats", &stats);
    }
    /* clang-format on */

    /* 25) Shut down the recursive R/W lock. */
    result = H5TS_rw_lock_destroy(&rec_rw_lock);
    CHECK_I(result, "H5TS_rw_lock_destroy");

    return;

} /* end tts_rec_rw_lock_smoke_check_1() */

void
cleanup_rec_rw_lock_smoke_check_1(void)
{
    /* nothing to do */
    return;
}

/*
 **********************************************************************
 * tts_rw_lock_smoke_check_test_thread
 *
 * Perform a sequence of recursive read and/or write locks on the
 * target recursive R/W lock as directed by the supplied user data.
 * Record all operations in the user data for later cross checking
 * with the statistics maintained by the recursive R/W lock.
 *
 * Note that while number of read and/or write locks is fixed, the
 * number of recursive lock and unlock calls is random, as is the
 * order of read and write locks if both are enabled.
 *
 * Creted Sept. 3. 2020.
 *
 * Programmer: John Mainzer
 *
 **********************************************************************
 */

void *
tts_rw_lock_smoke_check_test_thread(void *_udata)
{
    hbool_t                          read;
    int32_t                          rec_lock_depth = 0;
    int32_t                          max_rec_lock_depth;
    int32_t                          rd_locks_remaining;
    int32_t                          wr_locks_remaining;
    herr_t                           result;
    H5TS_rw_lock_t *                 rw_lock;
    struct rec_rw_lock_test_udata_t *udata;

    HDassert(_udata);

    udata = (struct rec_rw_lock_test_udata_t *)_udata;

    rd_locks_remaining = udata->target_rd_lock_cycles;
    wr_locks_remaining = udata->target_wr_lock_cycles;
    max_rec_lock_depth = udata->max_recursive_lock_depth;
    rw_lock            = udata->rw_lock;

    while ((rd_locks_remaining > 0) || (wr_locks_remaining > 0)) {

        if (wr_locks_remaining == 0) {

            read = TRUE;
        }
        else if (rd_locks_remaining == 0) {

            read = FALSE;
        }
        else {

            if ((HDrand() % 2) == 0) {

                read = TRUE;
            }
            else {

                read = FALSE;
            }
        }

        if (read) {

            result = H5TS_rw_rdlock(rw_lock);
            CHECK_I(result, "H5TS_rw_rdlock -- 1");

            udata->read_locks_granted++;
            udata->real_read_locks_granted++;
            rd_locks_remaining--;
            rec_lock_depth = 1;

            while (rec_lock_depth > 0) {

                if ((rec_lock_depth >= max_rec_lock_depth) || ((HDrand() % 2) == 0)) {

                    result = H5TS_rw_unlock(rw_lock);
                    CHECK_I(result, "H5TS_rw_unlock -- 1");

                    rec_lock_depth--;
                    udata->read_locks_released++;
                }
                else {

                    result = H5TS_rw_rdlock(rw_lock);
                    CHECK_I(result, "H5TS_rw_rdlock -- 2");

                    rec_lock_depth++;
                    udata->read_locks_granted++;
                }
            }

            udata->real_read_locks_released++;
        }
        else {

            result = H5TS_rw_wrlock(rw_lock);
            CHECK_I(result, "H5TS_rw_wrlock -- 1");

            udata->write_locks_granted++;
            udata->real_write_locks_granted++;
            wr_locks_remaining--;
            rec_lock_depth = 1;

            while (rec_lock_depth > 0) {

                if ((rec_lock_depth >= max_rec_lock_depth) || ((HDrand() % 2) == 0)) {

                    result = H5TS_rw_unlock(rw_lock);
                    CHECK_I(result, "H5TS_rw_unlock -- 2");

                    rec_lock_depth--;
                    udata->write_locks_released++;
                }
                else {

                    result = H5TS_rw_wrlock(rw_lock);
                    CHECK_I(result, "H5TS_rw_wrlock -- 2");

                    rec_lock_depth++;
                    udata->write_locks_granted++;
                }
            }

            udata->real_write_locks_released++;
        }
    }

    return NULL;

} /* end tts_rw_lock_smoke_check_test_thread() */

/*
 **********************************************************************
 * tts_rec_rw_lock_smoke_check_2 -- mob of readers
 *
 * Multi-thread test to check management of multiple readers ONLY by
 * the recursive R/W lock.  Test proceeds as follows:
 *
 *  1) Initialize an instance of the recursive R/W lock.
 *
 *  2) Setup the user data to be passed to each reader test thread.
 *
 *  3) Create the reader threads, each with its own user data.
 *     Activities of the reader threads is discussed in the header
 *     comment to tts_rw_lock_smoke_check_test_thread().
 *
 *  4) Wait for all threads to complete.
 *
 *  5) Examine the user data from the threads, to determine the
 *     total number of real and recursive read locks and un-lock.
 *
 *  6) Obtain the stats from the recursive R/W lock, and compare
 *     with the data gathered above.
 *
 *  7) Shut down the recursive R/W lock.
 *
 * The reader threads obtain and drop the read lock a specified
 * number of times.  Once a reader has a read lock, it does random
 * recursive read locks / unlocks until drops the read lock, and then
 * repeats the process until the spcified number of read locks have
 * been acquired and dropped.
 *
 * Creted Sept. 3. 2020.
 *
 * Programmer: John Mainzer
 *
 **********************************************************************
 */

void
tts_rec_rw_lock_smoke_check_2(void)
{
    hbool_t                          verbose = FALSE;
    herr_t                           result;
    int                              express_test;
    int                              i;
    int                              num_threads                 = MAX_NUM_THREADS;
    int                              lock_cycles                 = MAX_LOCK_CYCLES;
    int32_t                          total_target_rd_lock_cycles = 0;
    int32_t                          total_target_wr_lock_cycles = 0;
    H5TS_thread_t                    threads[MAX_NUM_THREADS];
    struct rec_rw_lock_test_udata_t *udata = NULL;
    struct H5TS_rw_lock_stats_t      stats;
    struct H5TS_rw_lock_stats_t      expected = {/* Initialize all fields to zero -- we will construct
                                             * the expected stats from the thread udata after
                                             * completion.
                                             */
                                            /* read_locks_granted             = */ 0,
                                            /* read_locks_released            = */ 0,
                                            /* real_read_locks_granted        = */ 0,
                                            /* real_read_locks_released       = */ 0,
                                            /* max_read_locks                 = */ 0,
                                            /* max_read_lock_recursion_depth  = */ 0,
                                            /* read_locks_delayed             = */ 0,
                                            /* max_read_locks_pending         = */ 0,
                                            /* write_locks_granted            = */ 0,
                                            /* write_locks_released           = */ 0,
                                            /* real_write_locks_granted       = */ 0,
                                            /* real_write_locks_released      = */ 0,
                                            /* max_write_locks                = */ 0,
                                            /* max_write_lock_recursion_depth = */ 0,
                                            /* write_locks_delayed            = */ 0,
                                            /* max_write_locks_pending        = */ 0};
    struct H5TS_rw_lock_t rec_rw_lock;

    /* Allocate the udata */
    udata = HDmalloc(sizeof(*udata) * MAX_NUM_THREADS);

    if (udata == NULL) {

        TestErrPrintf("thread udata allocation failed.\n");

        /* We can't do anything without the udata, so just return */
        return;
    }

    express_test = GetTestExpress();

    if (express_test >= 1) {

        num_threads /= 2;
        lock_cycles /= 10;
    }

    if (express_test >= 2) {

        num_threads /= 2;
        lock_cycles /= 10;
    }

    if (express_test >= 3) {

        num_threads /= 2;
        lock_cycles /= 10;
    }

    /* 1) Initialize an instance of the recursive R/W lock. */
    result = H5TS_rw_lock_init(&rec_rw_lock, H5TS_RW_LOCK_POLICY_FAVOR_WRITERS);
    CHECK_I(result, "H5TS_rw_lock_init");

    /* 2) Setup the user data to be passed to each reader test thread. */
    for (i = 0; i < MAX_NUM_THREADS; i++) {

        udata[i].rw_lock                   = &rec_rw_lock;
        udata[i].id                        = i;
        udata[i].target_rd_lock_cycles     = lock_cycles;
        udata[i].target_wr_lock_cycles     = 0;
        udata[i].max_recursive_lock_depth  = 10;
        udata[i].read_locks_granted        = 0;
        udata[i].read_locks_released       = 0;
        udata[i].real_read_locks_granted   = 0;
        udata[i].real_read_locks_released  = 0;
        udata[i].write_locks_granted       = 0;
        udata[i].write_locks_released      = 0;
        udata[i].real_write_locks_granted  = 0;
        udata[i].real_write_locks_released = 0;
    }

    /* 3) Create the reader threads, each with its own user data. */
    for (i = 0; i < num_threads; i++) {

        threads[i] = H5TS_create_thread(tts_rw_lock_smoke_check_test_thread, NULL, &(udata[i]));
    }

    /* 4) Wait for all threads to complete. */
    for (i = 0; i < num_threads; i++) {

        H5TS_wait_for_thread(threads[i]);
    }

    /* 5) Examine the user data from the threads, to determine the
     *    total number of real and recursive read locks and un-lock.
     *
     *    First, tally up the lock entries and exits from the test threads,
     *    and store this data in the expected recursive R/W/ lock stats..
     *    In passing, verify that each thread has done the expected number
     *    of locks and unlocks.  Do these as asserts -- will run checks on
     *    aggregate data shortly.
     */

    for (i = 0; i < num_threads; i++) {

        HDassert(udata[i].id == i);
        HDassert(udata[i].target_rd_lock_cycles == udata[i].real_read_locks_granted);
        HDassert(udata[i].target_rd_lock_cycles == udata[i].real_read_locks_released);
        HDassert(udata[i].target_wr_lock_cycles == udata[i].real_write_locks_granted);
        HDassert(udata[i].target_wr_lock_cycles == udata[i].real_write_locks_released);

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
    }

    /* Verify that the threads executed the expected number of read and write
     * lock cycles.  If they didn't, some thread probably encountered an error
     * and exited early.
     */
    if ((total_target_rd_lock_cycles != expected.real_read_locks_granted) ||
        (total_target_rd_lock_cycles != expected.real_read_locks_released) ||
        (total_target_wr_lock_cycles != expected.real_write_locks_granted) ||
        (total_target_wr_lock_cycles != expected.real_write_locks_released)) {

        TestErrPrintf("Threads reported unexpected number of locks/unlocks.\n");
    }

    /* initialize remaining non-zero fields in the expected stats */
    expected.max_read_locks                = num_threads;
    expected.max_read_lock_recursion_depth = 10;

    /* 6) Obtain the stats from the recursive R/W lock, and compare
     *     with the data gathered above.
     */

    result = H5TS_rw_lock_get_stats(&rec_rw_lock, &stats);
    CHECK_I(result, "H5TS_rw_lock_get_stats -- 1");

    /* turn off clang-format for readability */
    /* clang-format off */
    if ((stats.read_locks_granted              != expected.read_locks_granted) ||
        (stats.read_locks_released             != expected.read_locks_released) ||
        (stats.real_read_locks_granted         != expected.real_read_locks_granted) ||
        (stats.real_read_locks_released        != expected.real_read_locks_released) ||
        (stats.max_read_locks                   > expected.max_read_locks) || 
        (stats.max_read_locks                   < 1) ||
        (stats.max_read_lock_recursion_depth    > expected.max_read_lock_recursion_depth) ||
        (stats.max_read_lock_recursion_depth    < 1) ||
        (stats.read_locks_delayed              != expected.read_locks_delayed) ||
        (stats.max_read_locks_pending          != expected.max_read_locks_pending) ||
        (stats.write_locks_granted             != expected.write_locks_granted) ||
        (stats.write_locks_released            != expected.write_locks_released) ||
        (stats.real_write_locks_granted        != expected.real_write_locks_granted) ||
        (stats.real_write_locks_released       != expected.real_write_locks_released) ||
        (stats.max_write_locks                 != expected.max_write_locks) ||
        (stats.max_write_lock_recursion_depth  != expected.max_write_lock_recursion_depth) ||
        (stats.write_locks_delayed             != expected.write_locks_delayed) ||
        (stats.max_write_locks_pending         != expected.max_write_locks_pending)) {

        TestErrPrintf("Unexpected recursive R/W lock stats -- 1");
        H5TS_rw_lock_print_stats("Actual stats", &stats);
        H5TS_rw_lock_print_stats("Expected stats", &expected);
    }
    /* clang-format on */

    if (verbose) {

        H5TS_rw_lock_print_stats("mob of readers stats", &stats);
    }

    /* 7) Shut down the recursive R/W lock. */
    result = H5TS_rw_lock_destroy(&rec_rw_lock);
    CHECK_I(result, "H5TS_rw_lock_destroy");

    /* discard the udata if it exists */
    if (udata) {

        HDfree(udata);
    }

    return;

} /* end tts_rec_rw_lock_smoke_check_2() */

void
cleanup_rec_rw_lock_smoke_check_2(void)
{
    /* nothing to do */
    return;
}

/*
 **********************************************************************
 * tts_rec_rw_lock_smoke_check_3 -- mob of writers
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
 *     comment to tts_rw_lock_smoke_check_test_thread().
 *
 *  4) Wait for all threads to complete.
 *
 *  5) Examine the user data from the threads, to determine the
 *     total number of real and recursive read locks and un-lock.
 *
 *  6) Obtain the stats from the recursive R/W lock, and compare
 *     with the data gathered above.
 *
 *  7) Shut down the recursive R/W lock.
 *
 * The writer threads obtain and drop the read lock a specified
 * number of times.  Once a writeer has a write lock, it does random
 * recursive write locks / unlocks until drops the write lock, and then
 * repeats the process until the spcified number of write locks have
 * been acquired and dropped.
 *
 * Creted Sept. 3. 2020.
 *
 * Programmer: John Mainzer
 *
 **********************************************************************
 */

void
tts_rec_rw_lock_smoke_check_3(void)
{
    hbool_t                          verbose = FALSE;
    herr_t                           result;
    int                              i;
    int                              express_test;
    int                              num_threads                 = MAX_NUM_THREADS;
    int                              lock_cycles                 = MAX_LOCK_CYCLES;
    int32_t                          total_target_rd_lock_cycles = 0;
    int32_t                          total_target_wr_lock_cycles = 0;
    H5TS_thread_t                    threads[MAX_NUM_THREADS];
    struct rec_rw_lock_test_udata_t *udata = NULL;
    struct H5TS_rw_lock_stats_t      stats;
    struct H5TS_rw_lock_stats_t      expected = {/* Initialize all fields to zero -- we will construct
                                             * the expected stats from the thread udata after
                                             * completion.
                                             */
                                            /* read_locks_granted             = */ 0,
                                            /* read_locks_released            = */ 0,
                                            /* real_read_locks_granted        = */ 0,
                                            /* real_read_locks_released       = */ 0,
                                            /* max_read_locks                 = */ 0,
                                            /* max_read_lock_recursion_depth  = */ 0,
                                            /* read_locks_delayed             = */ 0,
                                            /* max_read_locks_pending         = */ 0,
                                            /* write_locks_granted            = */ 0,
                                            /* write_locks_released           = */ 0,
                                            /* real_write_locks_granted       = */ 0,
                                            /* real_write_locks_released      = */ 0,
                                            /* max_write_locks                = */ 0,
                                            /* max_write_lock_recursion_depth = */ 0,
                                            /* write_locks_delayed            = */ 0,
                                            /* max_write_locks_pending        = */ 0};
    struct H5TS_rw_lock_t rec_rw_lock;

    /* Allocate the udata */
    udata = HDmalloc(sizeof(*udata) * MAX_NUM_THREADS);

    if (udata == NULL) {

        TestErrPrintf("thread udata allocation failed.\n");

        /* We can't do anything without the udata, so just return */
        return;
    }

    express_test = GetTestExpress();

    if (express_test >= 1) {

        num_threads /= 2;
        lock_cycles /= 10;
    }

    if (express_test >= 2) {

        num_threads /= 2;
        lock_cycles /= 10;
    }

    if (express_test >= 3) {

        num_threads /= 2;
        lock_cycles /= 10;
    }

    /* 1) Initialize an instance of the recursive R/W lock. */
    result = H5TS_rw_lock_init(&rec_rw_lock, H5TS_RW_LOCK_POLICY_FAVOR_WRITERS);
    CHECK_I(result, "H5TS_rw_lock_init");

    /* 2) Setup the user data to be passed to each writer test thread. */
    for (i = 0; i < MAX_NUM_THREADS; i++) {

        udata[i].rw_lock                   = &rec_rw_lock;
        udata[i].id                        = i;
        udata[i].target_rd_lock_cycles     = 0;
        udata[i].target_wr_lock_cycles     = lock_cycles;
        udata[i].max_recursive_lock_depth  = 10;
        udata[i].read_locks_granted        = 0;
        udata[i].read_locks_released       = 0;
        udata[i].real_read_locks_granted   = 0;
        udata[i].real_read_locks_released  = 0;
        udata[i].write_locks_granted       = 0;
        udata[i].write_locks_released      = 0;
        udata[i].real_write_locks_granted  = 0;
        udata[i].real_write_locks_released = 0;
    }

    /* 3) Create the writer threads, each with its own user data. */
    for (i = 0; i < num_threads; i++) {

        threads[i] = H5TS_create_thread(tts_rw_lock_smoke_check_test_thread, NULL, &(udata[i]));
    }

    /* 4) Wait for all threads to complete. */
    for (i = 0; i < num_threads; i++) {

        H5TS_wait_for_thread(threads[i]);
    }

    /* 5) Examine the user data from the threads, to determine the
     *    total number of real and recursive read locks and un-lock.
     *
     *    First, tally up the lock entries and exits from the test threads,
     *    and store this data in the expected recursive R/W/ lock stats..
     *    In passing, verify that each thread has done the expected number
     *    of locks and unlocks.  Do these as asserts -- will run checks on
     *    aggregate data shortly.
     */

    for (i = 0; i < num_threads; i++) {

        HDassert(udata[i].id == i);
        HDassert(udata[i].target_rd_lock_cycles == udata[i].real_read_locks_granted);
        HDassert(udata[i].target_rd_lock_cycles == udata[i].real_read_locks_released);
        HDassert(udata[i].target_wr_lock_cycles == udata[i].real_write_locks_granted);
        HDassert(udata[i].target_wr_lock_cycles == udata[i].real_write_locks_released);

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
    }

    /* Verify that the threads executed the expected number of read and write
     * lock cycles.  If they didn't, some thread probably encountered an error
     * and exited early.
     */
    if ((total_target_rd_lock_cycles != expected.real_read_locks_granted) ||
        (total_target_rd_lock_cycles != expected.real_read_locks_released) ||
        (total_target_wr_lock_cycles != expected.real_write_locks_granted) ||
        (total_target_wr_lock_cycles != expected.real_write_locks_released)) {

        TestErrPrintf("Threads reported unexpected number of locks/unlocks.\n");
    }

    /* initialize remaining non-zero fields in the expected stats */
    expected.max_write_locks                = 1;
    expected.max_write_lock_recursion_depth = 10;
    expected.max_write_locks_pending        = num_threads - 1;

    /* 6) Obtain the stats from the recursive R/W lock, and compare
     *     with the data gathered above.
     */
    result = H5TS_rw_lock_get_stats(&rec_rw_lock, &stats);
    CHECK_I(result, "H5TS_rw_lock_get_stats -- 1");

    /* turn off clang-format for readability */
    /* clang-format off */
    if ((stats.read_locks_granted             != expected.read_locks_granted) ||
        (stats.read_locks_released            != expected.read_locks_released) ||
        (stats.real_read_locks_granted        != expected.real_read_locks_granted) ||
        (stats.real_read_locks_released       != expected.real_read_locks_released) ||
        (stats.max_read_locks                 != expected.max_read_locks) ||
        (stats.max_read_lock_recursion_depth  != expected.max_read_lock_recursion_depth) ||
        (stats.read_locks_delayed             != expected.read_locks_delayed) ||
        (stats.max_read_locks_pending         != expected.max_read_locks_pending) ||
        (stats.write_locks_granted            != expected.write_locks_granted) ||
        (stats.write_locks_released           != expected.write_locks_released) ||
        (stats.real_write_locks_granted       != expected.real_write_locks_granted) ||
        (stats.real_write_locks_released      != expected.real_write_locks_released) ||
        (stats.max_write_locks                != expected.max_write_locks) ||
        (stats.max_write_lock_recursion_depth  > expected.max_write_lock_recursion_depth) ||
        (stats.max_write_lock_recursion_depth  < 1) ||
        (stats.write_locks_delayed             < expected.write_locks_delayed) ||
        (stats.max_write_locks_pending         > expected.max_write_locks_pending)) {

        TestErrPrintf("Unexpected recursive R/W lock stats -- 1");
        H5TS_rw_lock_print_stats("Actual stats", &stats);
        H5TS_rw_lock_print_stats("Expected stats", &expected);
    }
    /* clang-format on */

    if (verbose) {

        H5TS_rw_lock_print_stats("Actual stats", &stats);
    }

    /* 7) Shut down the recursive R/W lock. */
    result = H5TS_rw_lock_destroy(&rec_rw_lock);
    CHECK_I(result, "H5TS_rw_lock_destroy");

    /* discard the udata if it exists */
    if (udata) {

        HDfree(udata);
    }

    return;

} /* end tts_rec_rw_lock_smoke_check_3() */

void
cleanup_rec_rw_lock_smoke_check_3(void)
{
    /* nothing to do */
    return;
}

/*
 **********************************************************************
 * tts_rec_rw_lock_smoke_check_4 -- mixed mob
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
 *     header comment to tts_rw_lock_smoke_check_test_thread().
 *
 *  4) Wait for all threads to complete.
 *
 *  5) Examine the user data from the threads, to determine the
 *     total number of real and recursive read & write locks and
 *     un-lock.
 *
 *  6) Obtain the stats from the recursive R/W lock, and compare
 *     with the data gathered above.
 *
 *  7) Shut down the recursive R/W lock.
 *
 * The reader / writer threads obtain and drop the read or write
 * locks a specified number of times.  Once a thread has a lock, it
 * does random recursive locks / unlocks until drops the lock, and then
 * repeats the process until the spcified number of locks have
 * been acquired and dropped.
 *
 * Creted Sept. 3. 2020.
 *
 * Programmer: John Mainzer
 *
 **********************************************************************
 */

void
tts_rec_rw_lock_smoke_check_4(void)
{
    hbool_t                          verbose = FALSE;
    herr_t                           result;
    int                              i;
    int                              express_test;
    int                              num_threads                 = MAX_NUM_THREADS;
    int                              lock_cycles                 = MAX_LOCK_CYCLES;
    int32_t                          total_target_rd_lock_cycles = 0;
    int32_t                          total_target_wr_lock_cycles = 0;
    H5TS_thread_t                    threads[MAX_NUM_THREADS];
    struct rec_rw_lock_test_udata_t *udata = NULL;
    struct H5TS_rw_lock_stats_t      stats;
    struct H5TS_rw_lock_stats_t      expected = {/* Initialize all fields to zero -- we will construct
                                             * the expected stats from the thread udata after
                                             * completion.
                                             */
                                            /* read_locks_granted             = */ 0,
                                            /* read_locks_released            = */ 0,
                                            /* real_read_locks_granted        = */ 0,
                                            /* real_read_locks_released       = */ 0,
                                            /* max_read_locks                 = */ 0,
                                            /* max_read_lock_recursion_depth  = */ 0,
                                            /* read_locks_delayed             = */ 0,
                                            /* max_read_locks_pending         = */ 0,
                                            /* write_locks_granted            = */ 0,
                                            /* write_locks_released           = */ 0,
                                            /* real_write_locks_granted       = */ 0,
                                            /* real_write_locks_released      = */ 0,
                                            /* max_write_locks                = */ 0,
                                            /* max_write_lock_recursion_depth = */ 0,
                                            /* write_locks_delayed            = */ 0,
                                            /* max_write_locks_pending        = */ 0};
    struct H5TS_rw_lock_t rec_rw_lock;

    /* Allocate the udata */
    udata = HDmalloc(sizeof(*udata) * MAX_NUM_THREADS);

    if (udata == NULL) {

        TestErrPrintf("thread udata allocation failed.\n");

        /* We can't do anything without the udata, so just return */
        return;
    }

    express_test = GetTestExpress();

    if (express_test >= 1) {

        lock_cycles /= 10;
    }

    if (express_test >= 2) {

        num_threads /= 2;
        lock_cycles /= 10;
    }

    if (express_test >= 3) {

        num_threads /= 2;
        lock_cycles /= 10;
    }

    /* 1) Initialize an instance of the recursive R/W lock. */
    result = H5TS_rw_lock_init(&rec_rw_lock, H5TS_RW_LOCK_POLICY_FAVOR_WRITERS);
    CHECK_I(result, "H5TS_rw_lock_init");

    /* 2) Setup the user data to be passed to each writer test thread. */
    for (i = 0; i < MAX_NUM_THREADS; i++) {

        udata[i].rw_lock                   = &rec_rw_lock;
        udata[i].id                        = i;
        udata[i].target_rd_lock_cycles     = lock_cycles;
        udata[i].target_wr_lock_cycles     = lock_cycles;
        udata[i].max_recursive_lock_depth  = 10;
        udata[i].read_locks_granted        = 0;
        udata[i].read_locks_released       = 0;
        udata[i].real_read_locks_granted   = 0;
        udata[i].real_read_locks_released  = 0;
        udata[i].write_locks_granted       = 0;
        udata[i].write_locks_released      = 0;
        udata[i].real_write_locks_granted  = 0;
        udata[i].real_write_locks_released = 0;
    }

    /* 3) Create the reader threads, each with its own user data. */
    for (i = 0; i < num_threads; i++) {

        threads[i] = H5TS_create_thread(tts_rw_lock_smoke_check_test_thread, NULL, &(udata[i]));
    }

    /* 4) Wait for all threads to complete. */
    for (i = 0; i < num_threads; i++) {

        H5TS_wait_for_thread(threads[i]);
    }

    /* 5) Examine the user data from the threads, to determine the
     *    total number of real and recursive read locks and un-lock.
     *
     *    First, tally up the lock entries and exits from the test threads,
     *    and store this data in the expected recursive R/W/ lock stats..
     *    In passing, verify that each thread has done the expected number
     *    of locks and unlocks.  Do these as asserts -- will run checks on
     *    aggregate data shortly.
     */

    for (i = 0; i < num_threads; i++) {

        HDassert(udata[i].id == i);
        HDassert(udata[i].target_rd_lock_cycles == udata[i].real_read_locks_granted);
        HDassert(udata[i].target_rd_lock_cycles == udata[i].real_read_locks_released);
        HDassert(udata[i].target_wr_lock_cycles == udata[i].real_write_locks_granted);
        HDassert(udata[i].target_wr_lock_cycles == udata[i].real_write_locks_released);

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
    }

    /* Verify that the threads executed the expected number of read and write
     * lock cycles.  If they didn't, some thread probably encountered an error
     * and exited early.
     */
    if ((total_target_rd_lock_cycles != expected.real_read_locks_granted) ||
        (total_target_rd_lock_cycles != expected.real_read_locks_released) ||
        (total_target_wr_lock_cycles != expected.real_write_locks_granted) ||
        (total_target_wr_lock_cycles != expected.real_write_locks_released)) {

        TestErrPrintf("Threads reported unexpected number of locks/unlocks.\n");
    }

    /* initialize remaining non-zero fields in the expected stats */
    expected.max_read_locks                 = num_threads;
    expected.max_read_lock_recursion_depth  = 10;
    expected.max_read_locks_pending         = num_threads - 1;
    expected.max_write_locks                = 1;
    expected.max_write_lock_recursion_depth = 10;
    expected.max_write_locks_pending        = num_threads - 1;

    /* 6) Obtain the stats from the recursive R/W lock, and compare
     *     with the data gathered above.
     */
    result = H5TS_rw_lock_get_stats(&rec_rw_lock, &stats);
    CHECK_I(result, "H5TS_rw_lock_get_stats -- 1");

    /* turn off clang-format for readability */
    /* clang-format off */
    if ((stats.read_locks_granted             != expected.read_locks_granted) ||
        (stats.read_locks_released            != expected.read_locks_released) ||
        (stats.real_read_locks_granted        != expected.real_read_locks_granted) ||
        (stats.real_read_locks_released       != expected.real_read_locks_released) ||
        (stats.max_read_locks                  > expected.max_read_locks) || 
        (stats.max_read_locks                  < 1) ||
        (stats.max_read_lock_recursion_depth   > expected.max_read_lock_recursion_depth) ||
        (stats.read_locks_delayed              < expected.read_locks_delayed) ||
        (stats.max_read_locks_pending          > expected.max_read_locks_pending) ||
        (stats.write_locks_granted            != expected.write_locks_granted) ||
        (stats.write_locks_released           != expected.write_locks_released) ||
        (stats.real_write_locks_granted       != expected.real_write_locks_granted) ||
        (stats.real_write_locks_released      != expected.real_write_locks_released) ||
        (stats.max_write_locks                != expected.max_write_locks) ||
        (stats.max_write_lock_recursion_depth  > expected.max_write_lock_recursion_depth) ||
        (stats.max_write_lock_recursion_depth  < 1) ||
        (stats.write_locks_delayed             < expected.write_locks_delayed) ||
        (stats.max_write_locks_pending         > expected.max_write_locks_pending)) {

        TestErrPrintf("Unexpected recursive R/W lock stats -- 1");
        H5TS_rw_lock_print_stats("Actual stats", &stats);
        H5TS_rw_lock_print_stats("Expected stats", &expected);
    }
    /* clang-format on */

    if (verbose) {

        H5TS_rw_lock_print_stats("Actual stats", &stats);
    }

    /* 7) Shut down the recursive R/W lock. */
    result = H5TS_rw_lock_destroy(&rec_rw_lock);
    CHECK_I(result, "H5TS_rw_lock_destroy");

    /* discard the udata if it exists */
    if (udata) {

        HDfree(udata);
    }

    return;

} /* end tts_rec_rw_lock_smoke_check_4() */

void
cleanup_rec_rw_lock_smoke_check_4(void)
{
    /* nothing to do */
    return;
}

#endif /* H5_USE_RECURSIVE_WRITER_LOCKS */

#endif /*H5_HAVE_THREADSAFE*/
