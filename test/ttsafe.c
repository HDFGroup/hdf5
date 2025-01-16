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

/*
 * FILE
 * ttsafe.c - HDF5 threadsafe testing framework main file.
 *
 * REMARKS
 * General test wrapper for HDF5 library thread safety test programs
 *
 * DESIGN
 * Each test function should be implemented as function having no
 * parameters and returning void (i.e. no return value).  They should be put
 * into the list of InitTest() calls in main() below.  Functions which depend
 * on other functionality should be placed below the InitTest() call for the
 * base functionality testing.
 * Each test module should include ttsafe.h and define a unique set of
 * names for test files they create.
 *
 */

/* ANY new test needs to have a prototype in ttsafe.h */
#include "ttsafe.h"

#define MAX_NUM_NAME 1000
#define NAME_OFFSET  6 /* offset for "name<num>" */

/* pre-condition: num must be a non-negative number */
H5_ATTR_PURE static unsigned
num_digits(int num)
{
    unsigned u;

    if (num == 0)
        return 1;

    for (u = 0; num > 0; u++)
        num = num / 10;

    return u;
}

/* Test the H5is_library_threadsafe() function */
void
tts_is_threadsafe(void H5_ATTR_UNUSED *params)
{
    bool is_ts;
    bool should_be;

#ifdef H5_HAVE_THREADSAFE_API
    is_ts     = false;
    should_be = true;
#else  /* H5_HAVE_THREADSAFE_API */
    is_ts     = true;
    should_be = false;
#endif /* H5_HAVE_THREADSAFE_API */

    if (H5is_library_threadsafe(&is_ts) != SUCCEED)
        TestErrPrintf("H5_is_library_threadsafe() call failed - test failed\n");

    if (is_ts != should_be)
        TestErrPrintf("Thread-safety value incorrect - test failed\n");
}

/* Routine to generate attribute names for numeric values */
char *
gen_name(int value)
{
    char    *temp;
    unsigned length;
    int      i;

    length                     = num_digits(MAX_NUM_NAME - 1);
    temp                       = (char *)malloc(NAME_OFFSET + length + 1);
    temp                       = strcpy(temp, "attrib");
    temp[NAME_OFFSET + length] = '\0';

    for (i = (int)(length - 1); i >= 0; i--) {
        temp[NAME_OFFSET + i] = (char)((int)'0' + value % 10);
        value                 = value / 10;
    }

    return temp;
}

int
main(int argc, char *argv[])
{

    /* Initialize testing framework */
    if (TestInit(argv[0], NULL, NULL, NULL, NULL, 0) < 0) {
        fprintf(stderr, "couldn't initialize testing framework\n");
        return -1;
    }

#ifdef H5_HAVE_THREADS
    MESSAGE(2, ("\nConcurrency Configuration:\n"));
    MESSAGE(2, ("\tThreading enabled, using "));
#ifdef H5_HAVE_C11_THREADS
    MESSAGE(2, ("C11 threads\n"));
#else
#ifdef H5_HAVE_WIN_THREADS
    MESSAGE(2, ("Windows threads\n"));
#else
    MESSAGE(2, ("pthreads\n"));
#endif
#endif
#ifdef H5_HAVE_STDATOMIC_H
    MESSAGE(2, ("\tC11 atomics enabled\n"));
#endif
#ifdef H5_HAVE_THREADSAFE_API
    MESSAGE(2, ("\tThreadsafe API enabled\n"));
#endif
#endif

    /* Tests are generally arranged from least to most complexity... */
    AddTest("is_threadsafe", tts_is_threadsafe, NULL, NULL, NULL, 0, "library threadsafe status");
#ifdef H5_HAVE_THREADS
    AddTest("thread_pool", tts_thread_pool, NULL, NULL, NULL, 0, "thread pools");
#ifndef H5_HAVE_STDATOMIC_H
    /* C11 atomics only tested when emulated */
    AddTest("atomics", tts_atomics, NULL, NULL, NULL, 0, "emulation of C11 atomics");
#endif /* H5_HAVE_STDATOMIC_H */
    AddTest("rwlock", tts_rwlock, NULL, NULL, NULL, 0, "simple R/W locks");
#ifndef H5_HAVE_WIN_THREADS
    /* Recursive R/W locks */
    AddTest("rec_rwlock_1", tts_rec_rwlock_smoke_check_1, NULL, NULL, NULL, 0,
            "recursive R/W lock smoke check 1 -- basic");
    AddTest("rec_rwlock_2", tts_rec_rwlock_smoke_check_2, NULL, NULL, NULL, 0,
            "recursive R/W lock smoke check 2 -- mob of readers");
    AddTest("rec_rwlock_3", tts_rec_rwlock_smoke_check_3, NULL, NULL, NULL, 0,
            "recursive R/W lock smoke check 3 -- mob of writers");
    AddTest("rec_rwlock_4", tts_rec_rwlock_smoke_check_4, NULL, NULL, NULL, 0,
            "recursive R/W lock smoke check 4 -- mixed mob");
#endif /* !H5_HAVE_WIN_THREADS */
    AddTest("semaphore", tts_semaphore, NULL, NULL, NULL, 0, "lightweight system semaphores");

#ifdef H5_HAVE_THREADSAFE_API
    AddTest("thread_id", tts_thread_id, NULL, NULL, NULL, 0, "thread IDs");

    AddTest("dcreate", tts_dcreate, NULL, cleanup_dcreate, NULL, 0, "multi-dataset creation");
    AddTest("error", tts_error, NULL, cleanup_error, NULL, 0, "per-thread error stacks");
#ifdef H5_HAVE_PTHREAD_H
    /* Thread cancellability only supported with pthreads ... */
    AddTest("cancel", tts_cancel, NULL, cleanup_cancel, NULL, 0, "thread cancellation safety test");
#endif /* H5_HAVE_PTHREAD_H */
    AddTest("acreate", tts_acreate, NULL, cleanup_acreate, NULL, 0, "multi-attribute creation");
    AddTest("attr_vlen", tts_attr_vlen, NULL, cleanup_attr_vlen, NULL, 0, "multi-file-attribute-vlen read");

    /* Error stack test must be done after thread_id test to not mess up expected IDs */
    AddTest("error_stacks", tts_error_stacks, NULL, NULL, NULL, 0, "error stack tests");

    /* Developer API routine tests */
    AddTest("developer", tts_develop_api, NULL, NULL, NULL, 0, "developer API routines");

#else /* H5_HAVE_THREADSAFE_API */

    printf("Most thread-safety tests skipped because THREADSAFE not enabled\n");

#endif /* H5_HAVE_THREADSAFE_API */

#else /* H5_HAVE_THREADS */

    printf("Most threading tests skipped because THREADS not enabled\n");

#endif /* H5_HAVE_THREADS */

    /* Display testing information */
    TestInfo(stdout);

    /* Parse command line arguments */
    if (TestParseCmdLine(argc, argv) < 0) {
        fprintf(stderr, "couldn't parse command-line arguments\n");
        TestShutdown();
        return -1;
    }

    /* Perform requested testing */
    if (PerformTests() < 0) {
        fprintf(stderr, "couldn't run tests\n");
        TestShutdown();
        return -1;
    }

    /* Display test summary, if requested */
    if (GetTestSummary())
        TestSummary(stdout);

    /* Release test infrastructure */
    if (TestShutdown() < 0) {
        fprintf(stderr, "couldn't shut down testing framework\n");
        return -1;
    }

    return GetTestNumErrs();

} /* end main() */
