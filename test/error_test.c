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
 * Purpose:    Tests the error API routines.
 */
#include "h5test.h"
#include "H5srcdir.h"

#ifdef H5_USE_16_API
int
main(void)
{
    fprintf(stderr, "Test skipped because backward compatibility with v1.6 is configured in\n");
    return 0;
}
#else /* H5_USE_16_API */

static const char *FILENAME[] = {"errors", NULL};

#define DATAFILE "filter_error"

#define DIM0 100
#define DIM1 200

static int **ipoints2      = NULL;
static int **icheck2       = NULL;
static int  *ipoints2_data = NULL;
static int  *icheck2_data  = NULL;

static hid_t ERR_CLS;
static hid_t ERR_CLS2;
static hid_t ERR_STACK;

static hid_t ERR_MAJ_TEST;
static hid_t ERR_MAJ_IO;
static hid_t ERR_MAJ_API;

static hid_t ERR_MIN_SUBROUTINE;
static hid_t ERR_MIN_ERRSTACK;
static hid_t ERR_MIN_CREATE;
static hid_t ERR_MIN_WRITE;
static hid_t ERR_MIN_GETNUM;

#define DSET_NAME "a_dataset"
#define FAKE_ID   (hid_t)0

#define ERR_CLS_NAME  "Error Test"
#define ERR_CLS2_NAME "Second Test"
#define PROG_NAME     "Error Program"
#define PROG2_NAME    "Second Program"
#define PROG_VERS     "1.0"

#define ERR_MAJ_TEST_MSG       "Error in test"
#define ERR_MAJ_IO_MSG         "Error in IO"
#define ERR_MAJ_API_MSG        "Error in API"
#define ERR_MIN_SUBROUTINE_MSG "Error in subroutine"
#define ERR_MIN_ERRSTACK_MSG   "Error in error stack"
#define ERR_MIN_CREATE_MSG     "Error in H5Dcreate2"
#define ERR_MIN_WRITE_MSG      "Error in H5Dwrite"
#define ERR_MIN_GETNUM_MSG     "Error in H5Eget_num"

#define MSG_SIZE       64
#define LONG_DESC_SIZE 8192

#define DSET_FILTER_NAME "dataset_with_filter"

static herr_t custom_print_cb(unsigned n, const H5E_error2_t *err_desc, void *client_data);

/*-------------------------------------------------------------------------
 * Function:    test_error
 *
 * Purpose:     Test error API functions
 *
 * Return:      Success:    0
 *              Failure:    -1
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_error(hid_t file)
{
    hid_t       dataset   = H5I_INVALID_HID;
    hid_t       space     = H5I_INVALID_HID;
    hid_t       estack_id = H5I_INVALID_HID;
    hsize_t     dims[2];
    const char *FUNC_test_error = "test_error";
    H5E_auto2_t old_func;
    void       *old_data = NULL;

    fprintf(stderr, "\nTesting error API based on data I/O\n");

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
    H5E_END_TRY

    /* Create the dataset */
    if ((dataset = H5Dcreate2(file, DSET_NAME, H5T_STD_I32BE, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) <
        0) {
        H5Epush(H5E_DEFAULT, __FILE__, FUNC_test_error, __LINE__, ERR_CLS, ERR_MAJ_IO, ERR_MIN_CREATE,
                "H5Dcreate2 failed");
        goto error;
    }

    /* Test enabling and disabling default printing */
    if (H5Eget_auto2(H5E_DEFAULT, &old_func, &old_data) < 0)
        TEST_ERROR;
    if (old_data != NULL)
        TEST_ERROR;
    if (old_func == NULL)
        TEST_ERROR;

    if (H5Eset_auto2(H5E_DEFAULT, NULL, NULL) < 0)
        TEST_ERROR;

    /* Make H5Dwrite fail, verify default print is disabled */
    if (H5Dwrite(FAKE_ID, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, ipoints2) >= 0) {
        H5Epush(H5E_DEFAULT, __FILE__, FUNC_test_error, __LINE__, ERR_CLS, ERR_MAJ_IO, ERR_MIN_WRITE,
                "H5Dwrite shouldn't succeed");
        goto error;
    }

    if (H5Eset_auto2(H5E_DEFAULT, old_func, old_data) < 0)
        TEST_ERROR;

    /* Test saving and restoring the current error stack */
    if (H5Dwrite(FAKE_ID, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, ipoints2) < 0) {
        H5Epush(H5E_DEFAULT, __FILE__, FUNC_test_error, __LINE__, ERR_CLS, ERR_MAJ_IO, ERR_MIN_WRITE,
                "H5Dwrite failed as supposed to");
        estack_id = H5Eget_current_stack();
        H5Dclose(dataset);
        H5Sclose(space);
        H5Eset_current_stack(estack_id);
        goto error;
    }

    /* In case program comes to this point, close dataset */
    if (H5Dclose(dataset) < 0)
        TEST_ERROR;

    TEST_ERROR;

error:
    return -1;
} /* end test_error() */

/*-------------------------------------------------------------------------
 * Function:    init_error
 *
 * Purpose:     Initialize error information.
 *
 * Return:      Success:    0
 *              Failure:    -1
 *
 *-------------------------------------------------------------------------
 */
static herr_t
init_error(void)
{
    ssize_t    cls_size = (ssize_t)strlen(ERR_CLS_NAME) + 1;
    ssize_t    msg_size = (ssize_t)strlen(ERR_MIN_SUBROUTINE_MSG) + 1;
    char      *cls_name = NULL;
    char      *msg      = NULL;
    H5E_type_t msg_type;

    if (NULL == (cls_name = (char *)malloc(strlen(ERR_CLS_NAME) + 1)))
        TEST_ERROR;
    if (NULL == (msg = (char *)malloc(strlen(ERR_MIN_SUBROUTINE_MSG) + 1)))
        TEST_ERROR;

    if ((ERR_CLS = H5Eregister_class(ERR_CLS_NAME, PROG_NAME, PROG_VERS)) < 0)
        TEST_ERROR;

    if (cls_size != H5Eget_class_name(ERR_CLS, cls_name, (size_t)cls_size) + 1)
        TEST_ERROR;
    if (strcmp(ERR_CLS_NAME, cls_name) != 0)
        TEST_ERROR;

    if ((ERR_MAJ_TEST = H5Ecreate_msg(ERR_CLS, H5E_MAJOR, ERR_MAJ_TEST_MSG)) < 0)
        TEST_ERROR;
    if ((ERR_MAJ_IO = H5Ecreate_msg(ERR_CLS, H5E_MAJOR, ERR_MAJ_IO_MSG)) < 0)
        TEST_ERROR;
    if ((ERR_MAJ_API = H5Ecreate_msg(ERR_CLS, H5E_MAJOR, ERR_MAJ_API_MSG)) < 0)
        TEST_ERROR;

    if ((ERR_MIN_SUBROUTINE = H5Ecreate_msg(ERR_CLS, H5E_MINOR, ERR_MIN_SUBROUTINE_MSG)) < 0)
        TEST_ERROR;
    if ((ERR_MIN_ERRSTACK = H5Ecreate_msg(ERR_CLS, H5E_MINOR, ERR_MIN_ERRSTACK_MSG)) < 0)
        TEST_ERROR;
    if ((ERR_MIN_CREATE = H5Ecreate_msg(ERR_CLS, H5E_MINOR, ERR_MIN_CREATE_MSG)) < 0)
        TEST_ERROR;
    if ((ERR_MIN_WRITE = H5Ecreate_msg(ERR_CLS, H5E_MINOR, ERR_MIN_WRITE_MSG)) < 0)
        TEST_ERROR;
    if ((ERR_MIN_GETNUM = H5Ecreate_msg(ERR_CLS, H5E_MINOR, ERR_MIN_GETNUM_MSG)) < 0)
        TEST_ERROR;

    if (msg_size != H5Eget_msg(ERR_MIN_SUBROUTINE, &msg_type, msg, (size_t)msg_size) + 1)
        TEST_ERROR;
    if (msg_type != H5E_MINOR)
        TEST_ERROR;
    if (strcmp(msg, ERR_MIN_SUBROUTINE_MSG) != 0)
        TEST_ERROR;

    /* Register another class for later testing. */
    if ((ERR_CLS2 = H5Eregister_class(ERR_CLS2_NAME, PROG2_NAME, PROG_VERS)) < 0)
        TEST_ERROR;

    free(cls_name);
    free(msg);

    return 0;

error:
    if (cls_name)
        free(cls_name);
    if (msg)
        free(msg);

    return -1;
} /* end init_error() */

/*-------------------------------------------------------------------------
 * Function:    error_stack
 *
 * Purpose:     Manipulates current error stack.
 *
 * Return:      Success:    0
 *              Failure:    -1
 *
 *-------------------------------------------------------------------------
 */
static herr_t
error_stack(void)
{
    ssize_t     err_num;
    const char *FUNC_error_stack = "error_stack";

    if ((err_num = H5Eget_num(H5E_DEFAULT)) < 0)
        TEST_ERROR;
    if (err_num)
        TEST_ERROR;

    if ((ERR_STACK = H5Eget_current_stack()) < 0)
        TEST_ERROR;

    /* Make it push error, force this function to fail */
    if ((err_num = H5Eget_num(ERR_STACK)) == 0) {
        H5Epush(ERR_STACK, __FILE__, FUNC_error_stack, __LINE__, ERR_CLS, ERR_MAJ_API, ERR_MIN_GETNUM,
                "Get number test failed, returned %d", (int)err_num);
        goto error;
    }

    /* In case program falls through here, close the stack and let it fail. */
    if (H5Eclose_stack(ERR_STACK) < 0)
        TEST_ERROR;

    return -1;

error:
    return -1;
} /* end error_stack() */

/*-------------------------------------------------------------------------
 * Function:    long_desc_cb
 *
 * Purpose:     Callback function to help test long description handling
 *
 * Return:      Success:    0
 *              Failure:    -1
 *
 *-------------------------------------------------------------------------
 */
static herr_t
long_desc_cb(unsigned H5_ATTR_UNUSED n, const H5E_error2_t *err_desc, void *client_data)
{
    char *real_desc = (char *)client_data;

    if (err_desc->desc != NULL && strcmp(err_desc->desc, real_desc) == 0)
        return 0;
    else
        return -1;
} /* end long_desc_cb() */

/*-------------------------------------------------------------------------
 * Function:    test_long_desc
 *
 * Purpose:     Test long error description handling
 *
 * Return:      Success:    0
 *              Failure:    -1
 *
 *-------------------------------------------------------------------------
 */
/* Disable warning for "format not a string literal" here -QAK */
/*
 *      This pragma only needs to surround the snprintf() calls with
 *      'full_desc' in the code below, but early (4.4.7, at least) gcc only
 *      allows diagnostic pragmas to be toggled outside of functions.
 */
H5_GCC_CLANG_DIAG_OFF("format-nonliteral")
static herr_t
test_long_desc(void)
{
    const char *format    = "Testing very long description string, %s";
    char       *long_desc = NULL;
    char       *full_desc = NULL;
    size_t      u;
    const char *test_FUNC = "test_long_desc";

    /* Allocate space for the error description info */
    if (NULL == (long_desc = (char *)malloc(LONG_DESC_SIZE)))
        TEST_ERROR;
    if (NULL == (full_desc = (char *)malloc(LONG_DESC_SIZE + 128)))
        TEST_ERROR;

    /* Create the long part of the error description */
    for (u = 0; u < LONG_DESC_SIZE; u++)
        long_desc[u] = (char)('A' + (u % 26));
    long_desc[LONG_DESC_SIZE - 1] = '\0';

    /* Clear the default error stack */
    if (H5Eclear2(H5E_DEFAULT) < 0)
        TEST_ERROR;

    /* Push an error with a long description */
    if (H5Epush(H5E_DEFAULT, __FILE__, test_FUNC, __LINE__, ERR_CLS, ERR_MAJ_TEST, ERR_MIN_SUBROUTINE, format,
                long_desc) < 0)
        TEST_ERROR;

    /* Create the string that should be in the description. Must use snprintf here
     * because snprintf is _snprintf on Windows
     */
    snprintf(full_desc, (size_t)(LONG_DESC_SIZE + 128), format, long_desc);

    /* Make certain that the description is correct */
    if (H5Ewalk2(H5E_DEFAULT, H5E_WALK_UPWARD, long_desc_cb, full_desc) < 0)
        TEST_ERROR;

    /* Clear the default error stack again */
    if (H5Eclear2(H5E_DEFAULT) < 0)
        TEST_ERROR;

    free(long_desc);
    free(full_desc);

    return 0;

error:
    if (long_desc)
        free(long_desc);
    if (full_desc)
        free(full_desc);

    return -1;
} /* end test_long_desc() */
H5_GCC_CLANG_DIAG_ON("format-nonliteral")

/*-------------------------------------------------------------------------
 * Function:    dump_error
 *
 * Purpose:     Prints error stack in default and customized ways.
 *
 * Return:      Success:    0
 *              Failure:    -1
 *
 *-------------------------------------------------------------------------
 */
static herr_t
dump_error(hid_t estack)
{
    /* Print errors in library default way */
    fprintf(stderr, "********* Print error stack in HDF5 default way *********\n");
    if (H5Eprint2(estack, stderr) < 0)
        TEST_ERROR;

    /* Customized way to print errors */
    fprintf(stderr, "\n********* Print error stack in customized way *********\n");
    if (H5Ewalk2(estack, H5E_WALK_UPWARD, custom_print_cb, stderr) < 0)
        TEST_ERROR;

    return 0;

error:
    return -1;
} /* end dump_error() */

/*-------------------------------------------------------------------------
 * Function:    custom_print_cb
 *
 * Purpose:     Callback function to print error stack in customized way.
 *
 * Return:      Success:    0
 *              Failure:    -1
 *
 *-------------------------------------------------------------------------
 */
static herr_t
custom_print_cb(unsigned n, const H5E_error2_t *err_desc, void *client_data)
{
    FILE     *stream = (FILE *)client_data;
    char      maj[MSG_SIZE];
    char      min[MSG_SIZE];
    char      cls[MSG_SIZE];
    const int indent = 4;

    /* Get descriptions for the major and minor error numbers */
    if (H5Eget_class_name(err_desc->cls_id, cls, MSG_SIZE) < 0)
        TEST_ERROR;

    if (H5Eget_msg(err_desc->maj_num, NULL, maj, MSG_SIZE) < 0)
        TEST_ERROR;

    if (H5Eget_msg(err_desc->min_num, NULL, min, MSG_SIZE) < 0)
        TEST_ERROR;

    fprintf(stream, "%*serror #%03d: %s in %s(): line %u\n", indent, "", n, err_desc->file_name,
            err_desc->func_name, err_desc->line);
    fprintf(stream, "%*sclass: %s\n", indent * 2, "", cls);
    fprintf(stream, "%*smajor: %s\n", indent * 2, "", maj);
    fprintf(stream, "%*sminor: %s\n", indent * 2, "", min);

    return 0;

error:
    return -1;
} /* end custom_print_cb() */

/*-------------------------------------------------------------------------
 * Function:    test_create
 *
 * Purpose:     Test creating an empty error stack
 *
 * Return:      Success:    0
 *              Failure:    -1
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_create(void)
{
    const char *err_func = "test_create";    /* Function name for pushing error */
    const char *err_msg  = "Error message";  /* Error message for pushing error */
    ssize_t     err_num;                     /* Number of errors on stack */
    hid_t       estack_id = H5I_INVALID_HID; /* Error stack ID */

    /* Create an empty error stack */
    if ((estack_id = H5Ecreate_stack()) < 0)
        TEST_ERROR;

    /* Check the number of errors on stack */
    err_num = H5Eget_num(estack_id);
    if (err_num != 0)
        TEST_ERROR;

    /* Push an error with a long description */
    if (H5Epush(estack_id, __FILE__, err_func, __LINE__, ERR_CLS, ERR_MAJ_TEST, ERR_MIN_SUBROUTINE, "%s",
                err_msg) < 0)
        TEST_ERROR;

    /* Check the number of errors on stack */
    err_num = H5Eget_num(estack_id);
    if (err_num != 1)
        TEST_ERROR;

    /* Clear the error stack */
    if (H5Eclear2(estack_id) < 0)
        TEST_ERROR;

    /* Check the number of errors on stack */
    err_num = H5Eget_num(estack_id);
    if (err_num != 0)
        TEST_ERROR;

    /* Close error stack */
    if (H5Eclose_stack(estack_id) < 0)
        TEST_ERROR;

    return 0;

error:
    return -1;
} /* end test_create() */

/*-------------------------------------------------------------------------
 * Function:    test_copy
 *
 * Purpose:     Test copying an error stack
 *
 * Return:      Success:    0
 *              Failure:    -1
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_copy(void)
{
    const char *err_func = "test_copy";      /* Function name for pushing error */
    const char *err_msg  = "Error message";  /* Error message for pushing error */
    ssize_t     err_num;                     /* Number of errors on stack */
    hid_t       estack_id = H5I_INVALID_HID; /* Error stack ID */
    herr_t      ret;                         /* Generic return value */

    /* Push an error with a long description */
    if (H5Epush(H5E_DEFAULT, __FILE__, err_func, __LINE__, ERR_CLS, ERR_MAJ_TEST, ERR_MIN_SUBROUTINE, "%s",
                err_msg) < 0)
        TEST_ERROR;

    /* Check the number of errors on stack */
    err_num = H5Eget_num(H5E_DEFAULT);
    if (err_num != 1)
        TEST_ERROR;

    /* Copy error stack, which clears the original */
    if ((estack_id = H5Eget_current_stack()) < 0)
        TEST_ERROR;

    /* Check the number of errors on stack copy */
    err_num = H5Eget_num(estack_id);
    if (err_num != 1)
        TEST_ERROR;

    /* Check the number of errors on original stack */
    err_num = H5Eget_num(H5E_DEFAULT);
    if (err_num != 0)
        TEST_ERROR;

    /* Put the stack copy as the default.  It closes the stack copy, too. */
    if (H5Eset_current_stack(estack_id) < 0)
        TEST_ERROR;

    /* Check the number of errors on default stack */
    err_num = H5Eget_num(H5E_DEFAULT);
    if (err_num != 1)
        TEST_ERROR;

    /* Try to close error stack copy.  Should fail because
     * the current H5Eset_current_stack closes the stack to be set.
     */
    H5E_BEGIN_TRY
    {
        ret = H5Eclose_stack(estack_id);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;

    return 0;

error:
    return -1;
} /* end test_copy() */

/*-------------------------------------------------------------------------
 * Function:    test_append
 *
 * Purpose:     Test appending one error stack to another
 *
 * Return:      Success:    0
 *              Failure:    -1
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_append(void)
{
    const char *err_func = "test_append";      /* Function name for pushing error */
    const char *err_msg1 = "Error message #1"; /* Error message #1 for pushing error */
    const char *err_msg2 = "Error message #2"; /* Error message #2 for pushing error */
    ssize_t     err_num;                       /* Number of errors on stack */
    hid_t       estack_id1 = H5I_INVALID_HID;  /* Error stack ID */
    hid_t       estack_id2 = H5I_INVALID_HID;  /* Error stack ID */
    herr_t      ret;                           /* Generic return value */

    /* Push an error */
    if (H5Epush(H5E_DEFAULT, __FILE__, err_func, __LINE__, ERR_CLS, ERR_MAJ_TEST, ERR_MIN_SUBROUTINE, "%s",
                err_msg1) < 0)
        TEST_ERROR;

    /* Copy error stack, which clears the original */
    if ((estack_id1 = H5Eget_current_stack()) < 0)
        TEST_ERROR;

    /* Check the number of errors on stack #1 */
    err_num = H5Eget_num(estack_id1);
    if (err_num != 1)
        TEST_ERROR;

    /* Create another stack, from scratch */
    if ((estack_id2 = H5Ecreate_stack()) < 0)
        TEST_ERROR;

    /* Check the number of errors on stack #2 */
    err_num = H5Eget_num(estack_id2);
    if (err_num != 0)
        TEST_ERROR;

    /* Push an error on stack #2 */
    if (H5Epush(estack_id2, __FILE__, err_func, __LINE__, ERR_CLS, ERR_MAJ_IO, ERR_MIN_CREATE, "%s",
                err_msg2) < 0)
        TEST_ERROR;

    /* Check the number of errors on stack #2 */
    err_num = H5Eget_num(estack_id2);
    if (err_num != 1)
        TEST_ERROR;

    /* Try to append bad error stack IDs */
    H5E_BEGIN_TRY
    {
        ret = H5Eappend_stack(H5E_DEFAULT, H5E_DEFAULT, false);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;
    H5E_BEGIN_TRY
    {
        ret = H5Eappend_stack(estack_id1, H5E_DEFAULT, false);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;
    H5E_BEGIN_TRY
    {
        ret = H5Eappend_stack(H5E_DEFAULT, estack_id2, false);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;

    /* Append error stack #2 to error stack #1, without closing stack #2 */
    if (H5Eappend_stack(estack_id1, estack_id2, false) < 0)
        TEST_ERROR;

    /* Check the number of errors on stack #1 */
    err_num = H5Eget_num(estack_id1);
    if (err_num != 2)
        TEST_ERROR;

    /* Check the number of errors on stack #2 */
    err_num = H5Eget_num(estack_id2);
    if (err_num != 1)
        TEST_ERROR;

    /* Append error stack #2 to error stack #1, and close stack #2 */
    if (H5Eappend_stack(estack_id1, estack_id2, true) < 0)
        TEST_ERROR;

    /* Try to close error stack #2.  Should fail because H5Eappend_stack
     * should have already closed it.
     */
    H5E_BEGIN_TRY
    {
        ret = H5Eclose_stack(estack_id2);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;

    /* Check the number of errors on stack #1 */
    err_num = H5Eget_num(estack_id1);
    if (err_num != 3)
        TEST_ERROR;

    return 0;

error:
    return -1;
} /* end test_append() */

/*-------------------------------------------------------------------------
 * Function:    close_error
 *
 * Purpose:     Closes error information.
 *
 * Return:      Success:    0
 *              Failure:    -1
 *
 *-------------------------------------------------------------------------
 */
static herr_t
close_error(void)
{
    /* Close major errors, let H5Eunregister_class close minor errors */
    if (H5Eclose_msg(ERR_MAJ_TEST) < 0)
        TEST_ERROR;

    if (H5Eclose_msg(ERR_MAJ_IO) < 0)
        TEST_ERROR;

    if (H5Eclose_msg(ERR_MAJ_API) < 0)
        TEST_ERROR;

    if (H5Eunregister_class(ERR_CLS) < 0)
        TEST_ERROR;

    if (H5Eunregister_class(ERR_CLS2) < 0)
        TEST_ERROR;

    return 0;

error:
    return -1;
} /* end close_error() */

/*-------------------------------------------------------------------------
 * Function:    test_filter_error
 *
 * Purpose:     Make sure the error message prints out the filter name
 *              when the existent file is opened but the filter isn't
 *              registered. The existent file was created with
 *              gen_filters.c.
 *
 * Return:      Success:    0
 *                Failure:    -1
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_filter_error(const char *fname, hid_t fapl)
{
    const char *pathname = H5_get_srcdir_filename(fname); /* Corrected test file name */
    hid_t       file     = H5I_INVALID_HID;
    hid_t       dataset  = H5I_INVALID_HID;
    int         buf[20];

    fprintf(stderr, "\nTesting error message during data reading when filter isn't registered\n");

    /* Open the file */
    if ((file = H5Fopen(pathname, H5F_ACC_RDONLY, fapl)) < 0)
        TEST_ERROR;

    /* Open the regular dataset */
    if ((dataset = H5Dopen2(file, DSET_FILTER_NAME, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    if (H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) >= 0)
        TEST_ERROR;

    /* Close/release resources */
    if (H5Dclose(dataset) < 0)
        TEST_ERROR;

    if (H5Fclose(file) < 0)
        TEST_ERROR;

    return 0;

error:
    return -1;
} /* end test_filter_error() */

/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     Test error API.
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    hid_t       file      = H5I_INVALID_HID;
    hid_t       fapl      = H5I_INVALID_HID;
    hid_t       estack_id = H5I_INVALID_HID;
    char        filename[1024];
    const char *driver_name; /* File driver value from environment */
    const char *FUNC_main = "main";
    int         i;

    /* Get the VFD to use */
    driver_name = h5_get_test_driver_name();

    fprintf(stderr, "   This program tests the Error API.  There're supposed to be some error messages\n");

    /* Initialize errors */
    if (init_error() < 0)
        TEST_ERROR;

    if ((fapl = h5_fileaccess()) < 0)
        TEST_ERROR;

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

    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));
    if ((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR;

    /* Test error stack */
    if (error_stack() < 0) {
        /* Push an error onto error stack */
        if (H5Epush(ERR_STACK, __FILE__, FUNC_main, __LINE__, ERR_CLS, ERR_MAJ_TEST, ERR_MIN_ERRSTACK,
                    "Error stack test failed") < 0)
            TEST_ERROR;

        /* Delete an error from the top of error stack */
        H5Epop(ERR_STACK, 1);

        /* Make sure we can use other class's major or minor errors. */
        H5Epush(ERR_STACK, __FILE__, FUNC_main, __LINE__, ERR_CLS2, ERR_MAJ_TEST, ERR_MIN_ERRSTACK,
                "Error stack test failed");

        /* Print out the errors on stack */
        dump_error(ERR_STACK);

        /* Empty error stack */
        H5Eclear2(ERR_STACK);

        /* Close error stack */
        H5Eclose_stack(ERR_STACK);
    } /* end if */

    /* Test error API */
    if (test_error(file) < 0) {
        H5Epush(H5E_DEFAULT, __FILE__, FUNC_main, __LINE__, ERR_CLS, ERR_MAJ_TEST, ERR_MIN_SUBROUTINE,
                "Error test failed, %s", "it's wrong");
        estack_id = H5Eget_current_stack();
        H5Eprint2(estack_id, stderr);
        H5Eclose_stack(estack_id);
    }

    /* Test pushing a very long error description */
    if (test_long_desc() < 0)
        TEST_ERROR;

    /* Test creating a new error stack */
    if (test_create() < 0)
        TEST_ERROR;

    /* Test copying a new error stack */
    if (test_copy() < 0)
        TEST_ERROR;

    if (H5Fclose(file) < 0)
        TEST_ERROR;

    /* Test appending error stacks */
    if (test_append() < 0)
        TEST_ERROR;

    /* Close error information */
    if (close_error() < 0)
        TEST_ERROR;

    /* Test error message during data reading when filter isn't registered
     * Use default FAPL to avoid some VFD drivers by the check-vfd test because
     * the test file was pre-generated.
     */
    h5_fixname(DATAFILE, H5P_DEFAULT, filename, sizeof filename);
    if (!h5_using_default_driver(driver_name) && strcmp(driver_name, "stdio")) {
        /* If not using the library's default VFD or the stdio VFD, force
         * the library's default VFD here. The test file was pre-generated
         * and can cause issues with many VFDs.
         */
        if (H5Pset_driver(fapl, H5_DEFAULT_VFD, NULL) < 0)
            TEST_ERROR;
    }
    if (test_filter_error(filename, fapl) < 0)
        TEST_ERROR;

    h5_clean_files(FILENAME, fapl);

    free(ipoints2);
    free(ipoints2_data);
    free(icheck2);
    free(icheck2_data);

    fprintf(stderr, "\nAll error API tests passed.\n");
    return 0;

error:
    free(ipoints2);
    free(ipoints2_data);
    free(icheck2);
    free(icheck2_data);

    fprintf(stderr, "\n***** ERROR TEST FAILED (real problem)! *****\n");
    return 1;
}
#endif /* H5_USE_16_API */
