/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
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
 * Purpose:    Tests Error API
 */
#include "h5test.h"

#ifdef H5_NO_DEPRECATED_SYMBOLS
int
main(void)
{
    printf("Test skipped because backward compatibility with v1.6 is NOT configured in\n");
    return 0;
}
#else /* H5_NO_DEPRECATED_SYMBOLS */

static const char *FILENAME[] = {"errors_compat", NULL};

#define DIM0 100
#define DIM1 200

static int **ipoints2      = NULL;
static int **icheck2       = NULL;
static int  *ipoints2_data = NULL;
static int  *icheck2_data  = NULL;

#define DSET_NAME "a_dataset"

herr_t custom_print_cb1(int n, H5E_error1_t *err_desc, void *client_data);
herr_t custom_print_cb2(int n, H5E_error2_t *err_desc, void *client_data);

/*-------------------------------------------------------------------------
 * Function:    user_print1
 *
 * Purpose:     This function is a user-defined old-style printing function.
 *              This is just a convenience function for H5Ewalk1() with a
 *              function that prints error messages.
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
user_print1(FILE *stream)
{
    /* Customized way to print errors */
    fprintf(stderr, "\n********* Print error stack in customized way *********\n");
    if (H5Ewalk1(H5E_WALK_UPWARD, (H5E_walk1_t)custom_print_cb1, stream) < 0)
        TEST_ERROR;

    return SUCCEED;

error:
    return FAIL;
}

/*-------------------------------------------------------------------------
 * Function:    user_print2
 *
 * Purpose:     This function is a user-defined new-style printing function.
 *              This is just a convenience function for H5Ewalk2() with a
 *              function that prints error messages.
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
user_print2(hid_t err_stack, FILE *stream)
{
    /* Customized way to print errors */
    fprintf(stderr, "\n********* Print error stack in customized way *********\n");
    if (H5Ewalk2(err_stack, H5E_WALK_UPWARD, (H5E_walk2_t)custom_print_cb2, stream) < 0)
        TEST_ERROR;

    return SUCCEED;

error:
    return FAIL;
}

/*-------------------------------------------------------------------------
 * Function:    custom_print_cb1
 *
 * Purpose:     Callback function to print error stack in customized way
 *              for H5Ewalk1
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
custom_print_cb1(int n, H5E_error1_t *err_desc, void *client_data)
{
    FILE     *stream = (FILE *)client_data;
    char     *maj    = NULL;
    char     *min    = NULL;
    const int indent = 4;

    if (NULL == (min = H5Eget_minor(err_desc->min_num)))
        TEST_ERROR;

    if (NULL == (maj = H5Eget_major(err_desc->maj_num)))
        TEST_ERROR;

    fprintf(stream, "%*serror #%03d: %s in %s(): line %u\n", indent, "", n, err_desc->file_name,
            err_desc->func_name, err_desc->line);

    fprintf(stream, "%*smajor: %s\n", indent * 2, "", maj);
    fprintf(stream, "%*sminor: %s\n", indent * 2, "", min);

    H5free_memory(maj);
    H5free_memory(min);

    return SUCCEED;

error:
    if (maj)
        H5free_memory(maj);
    if (min)
        H5free_memory(min);

    return FAIL;
}

/*-------------------------------------------------------------------------
 * Function:    custom_print_cb2
 *
 * Purpose:     Callback function to print error stack in customized way
 *              for H5Ewalk1
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
custom_print_cb2(int n, H5E_error2_t *err_desc, void *client_data)
{
    FILE     *stream = (FILE *)client_data;
    char     *maj    = NULL;
    char     *min    = NULL;
    const int indent = 4;

    if (NULL == (min = H5Eget_minor(err_desc->min_num)))
        TEST_ERROR;

    if (NULL == (maj = H5Eget_major(err_desc->maj_num)))
        TEST_ERROR;

    fprintf(stream, "%*serror #%03d: %s in %s(): line %u\n", indent, "", n, err_desc->file_name,
            err_desc->func_name, err_desc->line);

    fprintf(stream, "%*smajor: %s\n", indent * 2, "", maj);
    fprintf(stream, "%*sminor: %s\n", indent * 2, "", min);

    H5free_memory(maj);
    H5free_memory(min);

    return SUCCEED;

error:
    if (maj)
        H5free_memory(maj);
    if (min)
        H5free_memory(min);

    return FAIL;
}

/*-------------------------------------------------------------------------
 * Function:    test_error_compat
 *
 * Purpose:     Test the backward compatibility of H5Eset/get_auto
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_error_compat(void)
{
    hid_t       did = H5I_INVALID_HID;
    hid_t       sid = H5I_INVALID_HID;
    hsize_t     dims[2];
    H5E_auto1_t old_func1;
    H5E_auto2_t old_func2;
    void       *old_data = NULL;
    herr_t      ret;

    TESTING("error API H5Eset/get_auto");

    /* Add a newline and flush so the output file looks nicer */
    printf("\n");
    fflush(stdout);

    /* Create the dataspace */
    dims[0] = DIM0;
    dims[1] = DIM1;
    if ((sid = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR;

    /* Use H5Eget_auto2 to query the default printing function. */
    if (H5Eget_auto2(H5E_DEFAULT, &old_func2, &old_data) < 0)
        TEST_ERROR;
    if (old_data != NULL)
        TEST_ERROR;
    if (old_func2 == NULL)
        TEST_ERROR;

    if (H5Eset_auto2(H5E_DEFAULT, old_func2, old_data) < 0)
        TEST_ERROR;

    /* Try the printing function. Dataset creation should fail because the file
     * doesn't exist. */
    did = H5Dcreate2(H5I_INVALID_HID, DSET_NAME, H5T_STD_I32BE, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if (did >= 0)
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
    did = H5Dcreate2(H5I_INVALID_HID, DSET_NAME, H5T_STD_I32BE, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if (did >= 0)
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
    did = H5Dcreate2(H5I_INVALID_HID, DSET_NAME, H5T_STD_I32BE, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if (did >= 0)
        TEST_ERROR;

    /* This function changes the new-style printing function to the original. */
    if (H5Eset_auto2(H5E_DEFAULT, old_func2, NULL) < 0)
        TEST_ERROR;

    /* This call should work because the H5Eset_auto2 above set the default printing
     * function to H5Eprint2.  It simply returns user_print1. */
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
    if (old_func2 == NULL)
        TEST_ERROR;

    /* Try the printing function. Dataset creation should fail because the file
     * doesn't exist. */
    did = H5Dcreate2(H5I_INVALID_HID, DSET_NAME, H5T_STD_I32BE, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if (did >= 0)
        TEST_ERROR;

    if (H5Sclose(sid) < 0)
        TEST_ERROR;

    return SUCCEED;

error:
    H5E_BEGIN_TRY
    {
        H5Dclose(did);
        H5Sclose(sid);
    }
    H5E_END_TRY

    return FAIL;
}

/*-------------------------------------------------------------------------
 * Function:    test_h5epush1
 *
 * Purpose:     Test error API functions, mainly H5Epush1
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_h5epush1(hid_t file)
{
    hid_t       did       = H5I_INVALID_HID;
    hid_t       sid       = H5I_INVALID_HID;
    hid_t       estack_id = H5I_INVALID_HID;
    hsize_t     dims[2];
    const char *FUNC_test_error = "test_h5epush1";

    TESTING("error API based on data I/O");

    /* Add a newline and flush so the output file looks nicer */
    printf("\n");
    fflush(stdout);

    /* Create the dataspace */
    dims[0] = DIM0;
    dims[1] = DIM1;
    if ((sid = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR;

    /* Test H5E_BEGIN_TRY */
    H5E_BEGIN_TRY
    {
        did =
            H5Dcreate2(H5I_INVALID_HID, DSET_NAME, H5T_STD_I32BE, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    }
    H5E_END_TRY

    /* Create the dataset */
    if ((did = H5Dcreate2(file, DSET_NAME, H5T_STD_I32BE, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

        /* Disable the library's default printing function */
#ifdef H5_USE_16_API_DEFAULT
    if (H5Eset_auto(NULL, NULL) < 0)
#else
    if (H5Eset_auto(H5E_DEFAULT, NULL, NULL) < 0)
#endif
        TEST_ERROR;

    /* Make H5Dwrite fail, verify default print is disabled */
    if (H5Dwrite(H5I_INVALID_HID, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, ipoints2) < 0)
        H5Epush1(__FILE__, FUNC_test_error, __LINE__, H5E_ERROR, H5E_WRITEERROR, "expected H5Dwrite error");
    else
        TEST_ERROR;

    /* Save the error stack so the close calls don't interfere with it */
    if ((estack_id = H5Eget_current_stack()) < 0)
        TEST_ERROR;

    /* Close open identifiers */
    if (H5Dclose(did) < 0)
        TEST_ERROR;
    if (H5Sclose(sid) < 0)
        TEST_ERROR;

    /* Restore the stack containing errors */
    if (H5Eset_current_stack(estack_id) < 0)
        TEST_ERROR;

    return SUCCEED;

error:
    H5E_BEGIN_TRY
    {
        H5Dclose(did);
        H5Sclose(sid);
        H5Eclose_stack(estack_id);
    }
    H5E_END_TRY

    return FAIL;
}

/*-------------------------------------------------------------------------
 * Function:    dump_error
 *
 * Purpose:     Prints error stack in default and customized ways
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
dump_error(void)
{
    /* Print errors in library default way */
    fprintf(stderr, "********* Print error stack in HDF5 default way *********\n");
    if (H5Eprint1(stderr) < 0)
        TEST_ERROR;

    /* Customized way to print errors */
    fprintf(stderr, "\n********* Print error stack in customized way *********\n");
    if (H5Ewalk1(H5E_WALK_UPWARD, custom_print_cb1, stderr) < 0)
        TEST_ERROR;

    return SUCCEED;

error:
    return FAIL;
}

/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     Test error API
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    hid_t       fid     = H5I_INVALID_HID;
    hid_t       fapl_id = H5I_INVALID_HID;
    char        filename[1024];
    const char *FUNC_main = "main";
    int         i;

    fprintf(stderr, "   This program tests the Error API compatible with HDF5 v1.6.  There are supposed to "
                    "be some error messages\n");
    fapl_id = h5_fileaccess();

    /* Set up data arrays */
    if (NULL == (ipoints2_data = (int *)calloc(DIM0 * DIM1, sizeof(int))))
        TEST_ERROR;
    if (NULL == (ipoints2 = (int **)calloc(DIM0, sizeof(ipoints2_data))))
        TEST_ERROR;
    for (i = 0; i < DIM0; i++)
        ipoints2[i] = ipoints2_data + (i * DIM1);

    if (NULL == (icheck2_data = (int *)calloc(DIM0 * DIM1, sizeof(int))))
        TEST_ERROR;
    if (NULL == (icheck2 = (int **)calloc(DIM0, sizeof(icheck2_data))))
        TEST_ERROR;
    for (i = 0; i < DIM0; i++)
        icheck2[i] = icheck2_data + (i * DIM1);

    h5_fixname(FILENAME[0], fapl_id, filename, sizeof(filename));
    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id)) < 0)
        TEST_ERROR;

    /* Test error stack */

    /* Push an error onto error stack */
    H5Epush1(__FILE__, FUNC_main, __LINE__, H5E_ERROR, H5E_BADVALUE, "fake error message 1");

    /* Print out the errors on stack */
    dump_error();

    /* Empty error stack */
    H5Eclear1();

    /* Test error API */
    if (test_error_compat() < 0)
        TEST_ERROR;

    /* Test H5Epush1
     *
     * On success, there will be errors on the stack to print.
     */
    if (test_h5epush1(fid) < 0) {
        TEST_ERROR;
    }
    else {
        H5Epush1(__FILE__, FUNC_main, __LINE__, H5E_ERROR, H5E_BADMESG, "fake error message 2");
        H5Eprint1(stderr);
    }

    if (H5Fclose(fid) < 0)
        TEST_ERROR;
    h5_clean_files(FILENAME, fapl_id);

    free(ipoints2);
    free(ipoints2_data);
    free(icheck2);
    free(icheck2_data);

    printf("All error API tests passed.\n");
    return EXIT_SUCCESS;

error:
    free(ipoints2);
    free(ipoints2_data);
    free(icheck2);
    free(icheck2_data);

    H5E_BEGIN_TRY
    {
        H5Fclose(fid);
        H5Pclose(fapl_id);
    }
    H5E_END_TRY

    printf("***** ERROR TEST FAILED! *****\n");
    return EXIT_FAILURE;
}
#endif /* H5_NO_DEPRECATED_SYMBOLS */
