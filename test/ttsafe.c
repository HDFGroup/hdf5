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
#define NAME_OFFSET 6         /* offset for "name<num>" */

/* pre-condition: num must be a non-negative number */
H5_ATTR_PURE static unsigned
num_digits(int num)
{
    unsigned u;

    if(num == 0)
        return 1;

    for(u = 0; num > 0; u++)
        num = num / 10;

    return u;
}

/* Test the H5is_library_threadsafe() function */
void
tts_is_threadsafe(void)
{
    hbool_t is_ts;
    hbool_t should_be;

#ifdef H5_HAVE_THREADSAFE
    is_ts = FALSE;
    should_be = TRUE;
#else /* H5_HAVE_THREADSAFE */
    is_ts = TRUE;
    should_be = FALSE;
#endif /* H5_HAVE_THREADSAFE */

    if(H5is_library_threadsafe(&is_ts) != SUCCEED)
            TestErrPrintf("H5_is_library_threadsafe() call failed - test failed\n");

    if(is_ts != should_be)
            TestErrPrintf("Thread-safety value incorrect - test failed\n");

    return;
}

/* Routine to generate attribute names for numeric values */
char *gen_name(int value)
{
    char *temp;
    unsigned length;
    int i;

    length = num_digits(MAX_NUM_NAME - 1);
    temp = (char *)HDmalloc(NAME_OFFSET + length + 1);
    temp = HDstrcpy(temp, "attrib");
    temp[NAME_OFFSET + length] = '\0';

    for (i = (int)(length - 1); i >= 0; i--) {
        temp[NAME_OFFSET + i] = (char)((int)'0' + value % 10);
        value = value / 10;
    }

    return temp;
}

int main(int argc, char *argv[])
{

    /* Initialize testing framework */
    TestInit(argv[0], NULL, NULL);

    /* Tests are generally arranged from least to most complexity... */
    AddTest("is_threadsafe", tts_is_threadsafe, NULL, "library threadsafe status", NULL);
#ifdef H5_HAVE_THREADSAFE
    AddTest("dcreate", tts_dcreate, cleanup_dcreate, "multi-dataset creation", NULL);
    AddTest("error", tts_error, cleanup_error, "per-thread error stacks", NULL);
#ifdef H5_HAVE_PTHREAD_H    
    /* Thread cancellability only supported with pthreads ... */
    AddTest("cancel", tts_cancel, cleanup_cancel, "thread cancellation safety test", NULL);
#endif /* H5_HAVE_PTHREAD_H */
    AddTest("acreate", tts_acreate, cleanup_acreate, "multi-attribute creation", NULL);

#else /* H5_HAVE_THREADSAFE */

    HDprintf("Most thread-safety tests skipped because THREADSAFE not enabled\n");

#endif /* H5_HAVE_THREADSAFE */

    /* Display testing information */
    TestInfo(argv[0]);

    /* Parse command line arguments */
    TestParseCmdLine(argc,argv);

    /* Perform requested testing */
    PerformTests();

    /* Display test summary, if requested */
    if (GetTestSummary())
        TestSummary();

    /* Clean up test files, if allowed */
    if (GetTestCleanup() && !getenv("HDF5_NOCLEANUP"))
        TestCleanup();

    /* Release test infrastructure */
    TestShutdown();

    return GetTestNumErrs();

} /* end main() */

