/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Programmer:    Raymond Lu
 *              October 14, 2001
 *
 * Purpose:    Tests Error API
 */
#include "h5test.h"

#ifdef H5_NO_DEPRECATED_SYMBOLS
int
main(void)
{
    HDprintf("Test skipped because backward compatibility with v1.6 is NOT configured in\n");
    return 0;
}
#else /* H5_NO_DEPRECATED_SYMBOLS */

const char *FILENAME[] = {"errors_compat", NULL};

#define DIM0 100
#define DIM1 200

int **ipoints2      = NULL;
int **icheck2       = NULL;
int * ipoints2_data = NULL;
int * icheck2_data  = NULL;

#define DSET_NAME "a_dataset"
#define FAKE_ID   (hid_t) - 1

herr_t custom_print_cb1(int n, H5E_error1_t *err_desc, void *client_data);
herr_t custom_print_cb2(int n, H5E_error2_t *err_desc, void *client_data);

/*-------------------------------------------------------------------------
 * Function:    user_print1
 *
 * Purpose:    This function is a user-defined old-style printing function.
 *              This is just a convenience function for H5Ewalk1() with a
 *              function that prints error messages.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 * Programmer:    Raymond Lu
 *              4 October 2010
 *
 *-------------------------------------------------------------------------
 */
static herr_t
user_print1(FILE *stream)
{
    /* Customized way to print errors */
    HDfprintf(stderr, "\n********* Print error stack in customized way *********\n");
    if (H5Ewalk1(H5E_WALK_UPWARD, (H5E_walk1_t)custom_print_cb1, stream) < 0)
        TEST_ERROR;

    return 0;

error:
    return -1;
}

/*-------------------------------------------------------------------------
 * Function:    user_print2
 *
 * Purpose:    This function is a user-defined new-style printing function.
 *              This is just a convenience function for H5Ewalk2() with a
 *              function that prints error messages.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 * Programmer:    Raymond Lu
 *              4 October 2010
 *
 *-------------------------------------------------------------------------
 */
static herr_t
user_print2(hid_t err_stack, FILE *stream)
{
    /* Customized way to print errors */
    HDfprintf(stderr, "\n********* Print error stack in customized way *********\n");
    if (H5Ewalk2(err_stack, H5E_WALK_UPWARD, (H5E_walk2_t)custom_print_cb2, stream) < 0)
        TEST_ERROR;

    return 0;

error:
    return -1;
}

/*-------------------------------------------------------------------------
 * Function:    custom_print_cb1
 *
 * Purpose:    Callback function to print error stack in customized way
 *              for H5Ewalk1.
 *
 * Return:    Success:    0
 *
 *        Failure:    -1
 *
 * Programmer:    Raymond Lu
 *              4 October 2010
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
custom_print_cb1(int n, H5E_error1_t *err_desc, void *client_data)
{
    FILE *    stream = (FILE *)client_data;
    char *    maj    = NULL;
    char *    min    = NULL;
    const int indent = 4;

    if (NULL == (min = H5Eget_minor(err_desc->min_num)))
        TEST_ERROR;

    if (NULL == (maj = H5Eget_major(err_desc->maj_num)))
        TEST_ERROR;

    HDfprintf(stream, "%*serror #%03d: %s in %s(): line %u\n", indent, "", n, err_desc->file_name,
              err_desc->func_name, err_desc->line);

    HDfprintf(stream, "%*smajor: %s\n", indent * 2, "", maj);
    HDfprintf(stream, "%*sminor: %s\n", indent * 2, "", min);

    H5free_memory(maj);
    H5free_memory(min);

    return 0;

error:
    if (maj)
        H5free_memory(maj);
    if (min)
        H5free_memory(min);

    return -1;
}

/*-------------------------------------------------------------------------
 * Function:    custom_print_cb2
 *
 * Purpose:    Callback function to print error stack in customized way
 *              for H5Ewalk1.
 *
 * Return:    Success:    0
 *
 *        Failure:    -1
 *
 * Programmer:    Raymond Lu
 *              4 October 2010
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
custom_print_cb2(int n, H5E_error2_t *err_desc, void *client_data)
{
    FILE *    stream = (FILE *)client_data;
    char *    maj    = NULL;
    char *    min    = NULL;
    const int indent = 4;

    if (NULL == (min = H5Eget_minor(err_desc->min_num)))
        TEST_ERROR;

    if (NULL == (maj = H5Eget_major(err_desc->maj_num)))
        TEST_ERROR;

    HDfprintf(stream, "%*serror #%03d: %s in %s(): line %u\n", indent, "", n, err_desc->file_name,
              err_desc->func_name, err_desc->line);

    HDfprintf(stream, "%*smajor: %s\n", indent * 2, "", maj);
    HDfprintf(stream, "%*sminor: %s\n", indent * 2, "", min);

    H5free_memory(maj);
    H5free_memory(min);

    return 0;

error:
    if (maj)
        H5free_memory(maj);
    if (min)
        H5free_memory(min);

    return -1;
}

/*-------------------------------------------------------------------------
 * Function:    test_error1
 *
 * Purpose:    Test the backward compatibility of H5Eset/get_auto.
 *
 * Return:    Success:    0
 *
 *        Failure:    -1
 *
 * Programmer:    Raymond Lu
 *        17 September 2010
 *
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_error1(void)
{
    hid_t       dataset, space;
    hsize_t     dims[2];
    H5E_auto1_t old_func1;
    H5E_auto2_t old_func2;
    void *      old_data;
    herr_t      ret;

    TESTING("error API H5Eset/get_auto");
    HDfprintf(stderr, "\n");

    /* Create the data space */
    dims[0] = DIM0;
    dims[1] = DIM1;
    if ((space = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR;

    /* Use H5Eget_auto2 to query the default printing function.  The library
     *should indicate H5Eprint2 as the default. */
    if (H5Eget_auto2(H5E_DEFAULT, &old_func2, &old_data) < 0)
        TEST_ERROR;
    if (old_data != NULL)
        TEST_ERROR;
    if (!old_func2 || (H5E_auto2_t)H5Eprint2 != old_func2)
        TEST_ERROR;

    /* This function sets the default printing function to be H5Eprint2. */
    if (H5Eset_auto2(H5E_DEFAULT, old_func2, old_data) < 0)
        TEST_ERROR;

    /* Try the printing function. Dataset creation should fail because the file
     * doesn't exist. */
    dataset = H5Dcreate2(FAKE_ID, DSET_NAME, H5T_STD_I32BE, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if (dataset >= 0)
        TEST_ERROR;

    /* This call should work.  It simply returns H5Eprint1. */
    if ((ret = H5Eget_auto1(&old_func1, &old_data)) < 0)
        TEST_ERROR;
    if (old_data != NULL)
        TEST_ERROR;
    if (!old_func1 || (H5E_auto1_t)H5Eprint1 != old_func1)
        TEST_ERROR;

    /* This function changes the old-style printing function to be user_print1. */
    if (H5Eset_auto1((H5E_auto1_t)user_print1, stderr) < 0)
        TEST_ERROR;

    /* Try the printing function. Dataset creation should fail because the file
     * doesn't exist. */
    dataset = H5Dcreate2(FAKE_ID, DSET_NAME, H5T_STD_I32BE, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if (dataset >= 0)
        TEST_ERROR;

    /* This call should fail because the test mixes H5Eget_auto2 with H5Eset_auto1.
     * Once the H5Eset_auto1 is called with a user-defined printing function,
     * a call to H5Eget_auto2 will fail. But keep in mind the printing function is
     * user_print1. */
    if ((ret = H5Eget_auto2(H5E_DEFAULT, &old_func2, &old_data)) >= 0)
        TEST_ERROR;

    /* This function changes the new-style printing function to be user_print2. */
    if (H5Eset_auto2(H5E_DEFAULT, (H5E_auto2_t)user_print2, stderr) < 0)
        TEST_ERROR;

    /* Try the printing function. Dataset creation should fail because the file
     * doesn't exist. */
    dataset = H5Dcreate2(FAKE_ID, DSET_NAME, H5T_STD_I32BE, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if (dataset >= 0)
        TEST_ERROR;

    /* This function changes the new-style printing function back to the default H5Eprint2. */
    if (H5Eset_auto2(H5E_DEFAULT, (H5E_auto2_t)H5Eprint2, NULL) < 0)
        TEST_ERROR;

    /* This call should work because the H5Eset_auto2 above restored the default printing
     * function H5Eprint2.  It simply returns user_print1. */
    if ((ret = H5Eget_auto1(&old_func1, &old_data)) < 0)
        TEST_ERROR;
    if (old_data != NULL)
        TEST_ERROR;
    if (!old_func1 || (H5E_auto1_t)user_print1 != old_func1)
        TEST_ERROR;

    /* This function changes the new-style printing function back to the default H5Eprint1. */
    if (H5Eset_auto1((H5E_auto1_t)H5Eprint1, NULL) < 0)
        TEST_ERROR;

    /* This call should work because the H5Eset_auto1 above restored the default printing
     * function H5Eprint1.  It simply returns H5Eprint2. */
    if ((ret = H5Eget_auto2(H5E_DEFAULT, &old_func2, &old_data)) < 0)
        TEST_ERROR;
    if (old_data != NULL)
        TEST_ERROR;
    if (!old_func2 || (H5E_auto2_t)H5Eprint2 != old_func2)
        TEST_ERROR;

    /* Try the printing function. Dataset creation should fail because the file
     * doesn't exist. */
    dataset = H5Dcreate2(FAKE_ID, DSET_NAME, H5T_STD_I32BE, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if (dataset >= 0)
        TEST_ERROR;

    return 0;

error:
    return -1;
}

/*-------------------------------------------------------------------------
 * Function:    test_error2
 *
 * Purpose:    Test error API functions, mainly on H5Epush1.
 *
 * Return:    Success:    0
 *
 *        Failure:    -1
 *
 * Programmer:    Raymond Lu
 *        July 10, 2003
 *
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_error2(hid_t file)
{
    hid_t       dataset, space;
    hsize_t     dims[2];
    const char *FUNC_test_error = "test_error2";

    TESTING("error API based on data I/O");
    HDfprintf(stderr, "\n");

    /* Create the data space */
    dims[0] = DIM0;
    dims[1] = DIM1;
    if ((space = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR;

    /* Test H5E_BEGIN_TRY */
    H5E_BEGIN_TRY
    {
        dataset = H5Dcreate2(FAKE_ID, DSET_NAME, H5T_STD_I32BE, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    }
    H5E_END_TRY;

    /* Create the dataset */
    if ((dataset = H5Dcreate2(file, DSET_NAME, H5T_STD_I32BE, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) <
        0) {
        H5Epush1(__FILE__, FUNC_test_error, __LINE__, H5E_ERROR, H5E_CANTCREATE, "H5Dcreate2 failed");
        goto error;
    }

    /* Disable the library's default printing function */
#ifdef H5_USE_16_API_DEFAULT
    if (H5Eset_auto(NULL, NULL) < 0)
#else
    if (H5Eset_auto(H5E_DEFAULT, NULL, NULL) < 0)
#endif
        TEST_ERROR;

    /* Make H5Dwrite fail, verify default print is disabled */
    if (H5Dwrite(FAKE_ID, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, ipoints2) < 0) {
        H5Epush1(__FILE__, FUNC_test_error, __LINE__, H5E_ERROR, H5E_WRITEERROR,
                 "H5Dwrite shouldn't succeed");
        goto error;
    }

    /* In case program comes to this point, close dataset */
    if (H5Dclose(dataset) < 0)
        TEST_ERROR;

    TEST_ERROR;

error:
    return -1;
}

/*-------------------------------------------------------------------------
 * Function:    dump_error
 *
 * Purpose:    Prints error stack in default and customized ways.
 *
 * Return:    Success:    0
 *
 *        Failure:    -1
 *
 * Programmer:    Raymond Lu
 *        July 17, 2003
 *
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
dump_error(void)
{
    /* Print errors in library default way */
    HDfprintf(stderr, "********* Print error stack in HDF5 default way *********\n");
    if (H5Eprint1(stderr) < 0)
        TEST_ERROR;

    /* Customized way to print errors */
    HDfprintf(stderr, "\n********* Print error stack in customized way *********\n");
    if (H5Ewalk1(H5E_WALK_UPWARD, custom_print_cb1, stderr) < 0)
        TEST_ERROR;

    return 0;

error:
    return -1;
}

/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:    Test error API.
 *
 * Programmer:    Raymond Lu
 *        July 10, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    hid_t       file, fapl;
    char        filename[1024];
    const char *FUNC_main = "main";
    int         i;

    HDfprintf(stderr, "   This program tests the Error API compatible with HDF5 v1.6.  There are supposed to "
                      "be some error messages\n");
    fapl = h5_fileaccess();

    /* Set up data arrays */
    if (NULL == (ipoints2_data = (int *)HDcalloc(DIM0 * DIM1, sizeof(int))))
        TEST_ERROR;
    if (NULL == (ipoints2 = (int **)HDcalloc(DIM0, sizeof(ipoints2_data))))
        TEST_ERROR;
    for (i = 0; i < DIM0; i++)
        ipoints2[i] = ipoints2_data + (i * DIM1);

    if (NULL == (icheck2_data = (int *)HDcalloc(DIM0 * DIM1, sizeof(int))))
        TEST_ERROR;
    if (NULL == (icheck2 = (int **)HDcalloc(DIM0, sizeof(icheck2_data))))
        TEST_ERROR;
    for (i = 0; i < DIM0; i++)
        icheck2[i] = icheck2_data + (i * DIM1);

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
    if ((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR;

    /* Test error stack */

    /* Push an error onto error stack */
    H5Epush1(__FILE__, FUNC_main, __LINE__, H5E_ERROR, H5E_BADVALUE, "Error test failed");

    /* Print out the errors on stack */
    dump_error();

    /* Empty error stack */
    H5Eclear1();

    /* Test error API */
    if (test_error1() < 0)
        TEST_ERROR;

    if (test_error2(file) < 0) {
        H5Epush1(__FILE__, FUNC_main, __LINE__, H5E_ERROR, H5E_BADMESG, "Error test failed");
        H5Eprint1(stderr);
    }

    if (H5Fclose(file) < 0)
        TEST_ERROR;
    h5_clean_files(FILENAME, fapl);

    HDfree(ipoints2);
    HDfree(ipoints2_data);
    HDfree(icheck2);
    HDfree(icheck2_data);

    HDprintf("All error API tests passed.\n");
    return 0;

error:
    HDfree(ipoints2);
    HDfree(ipoints2_data);
    HDfree(icheck2);
    HDfree(icheck2_data);

    HDprintf("***** ERROR TEST FAILED! *****\n");
    return 1;
}
#endif /* H5_NO_DEPRECATED_SYMBOLS */
