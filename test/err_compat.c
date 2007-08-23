/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Programmer:	Raymond Lu
 *              October 14, 2001
 *
 * Purpose:	Tests Error API
 */
#include "h5test.h"

#ifdef H5_NO_DEPRECATED_SYMBOLS
int main(void)
{
    printf("Test skipped because backward compatability with v1.6 is NOT configured in\n");
    return 0;
}
#else /* H5_NO_DEPRECATED_SYMBOLS */

const char *FILENAME[] = {
    "errors_compat",
    NULL
};

#define DIM0    100
#define DIM1    200

int	ipoints2[DIM0][DIM1], icheck2[DIM0][DIM1];

#define DSET_NAME               "a_dataset"
#define FAKE_ID                 -1

herr_t custom_print_cb(int n, H5E_error1_t *err_desc, void* client_data);


/*-------------------------------------------------------------------------
 * Function:	test_error
 *
 * Purpose:	Test error API functions
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Raymond Lu
 *		July 10, 2003
 *
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_error(hid_t file)
{
    hid_t		dataset, space;
    hsize_t		dims[2];
    const char          *FUNC_test_error="test_error";
    H5E_auto1_t         old_func;
    void                *old_data;

    TESTING("error API based on data I/O");
    fprintf(stderr, "\n");

    /* Create the data space */
    dims[0] = DIM0;
    dims[1] = DIM1;
    if ((space = H5Screate_simple(2, dims, NULL))<0) TEST_ERROR;

    /* Test H5E_BEGIN_TRY */
    H5E_BEGIN_TRY {
        dataset = H5Dcreate(FAKE_ID, DSET_NAME, H5T_STD_I32BE, space, H5P_DEFAULT);
    } H5E_END_TRY;

    /* Create the dataset */
    if ((dataset = H5Dcreate(file, DSET_NAME, H5T_STD_I32BE, space,
			     H5P_DEFAULT))<0) {
        H5Epush1(__FILE__, FUNC_test_error, __LINE__, H5E_ERROR, H5E_CANTCREATE,
                "H5Dcreate failed");
        goto error;
    }

    /* Test enabling and disabling default printing */
    if (H5Eget_auto1(&old_func, &old_data)<0)
	TEST_ERROR;
    if (old_data != NULL)
	TEST_ERROR;
    if (!old_func)
	TEST_ERROR;
#ifdef H5_USE_16_API
    if (old_func != (H5E_auto1_t)H5Eprint1)
	TEST_ERROR;
#else /* H5_USE_16_API */
    if (old_func != (H5E_auto1_t)H5Eprint2)
	TEST_ERROR;
#endif /* H5_USE_16_API */

    if(H5Eset_auto1(NULL, NULL)<0)
        TEST_ERROR;

    /* Make H5Dwrite fail, verify default print is disabled */
    if (H5Dwrite(FAKE_ID, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, ipoints2)<0) {
        H5Epush1(__FILE__, FUNC_test_error, __LINE__, H5E_ERROR, H5E_WRITEERROR,
                "H5Dwrite shouldn't succeed");
        goto error;
    }

    if(H5Eset_auto1(old_func, old_data)<0)
        TEST_ERROR;

    /* In case program comes to this point, close dataset */
    if(H5Dclose(dataset)<0) TEST_ERROR;

    TEST_ERROR;

  error:
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:    dump_error
 *
 * Purpose:	Prints error stack in default and customized ways.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Raymond Lu
 *		July 17, 2003
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
    fprintf(stderr, "********* Print error stack in HDF5 default way *********\n");
    if(H5Eprint1(stderr) < 0)
        TEST_ERROR;

    /* Customized way to print errors */
    fprintf(stderr, "\n********* Print error stack in customized way *********\n");
    if(H5Ewalk1(H5E_WALK_UPWARD, custom_print_cb, stderr) < 0)
        TEST_ERROR;

    return 0;

  error:
    return -1;
}

/*-------------------------------------------------------------------------
 * Function:    custom_print_cb
 *
 * Purpose:	Callback function to print error stack in customized way.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Raymond Lu
 *		July 17, 2003
 *
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
custom_print_cb(int n, H5E_error1_t *err_desc, void* client_data)
{
    FILE		*stream  = (FILE *)client_data;
    char                *maj;
    char                *min;
    const int		indent = 4;

    if(NULL == (min = H5Eget_minor(err_desc->min_num)))
        TEST_ERROR;

    if(NULL == (maj = H5Eget_major(err_desc->maj_num)))
        TEST_ERROR;

    fprintf(stream, "%*serror #%03d: %s in %s(): line %u\n",
	     indent, "", n, err_desc->file_name,
	     err_desc->func_name, err_desc->line);
    fprintf(stream, "%*smajor: %s\n", indent*2, "", maj);
    HDfree(maj);
    fprintf (stream, "%*sminor: %s\n", indent*2, "", min);
    HDfree(min);

    return 0;

error:
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	Test error API.
 *
 * Programmer:	Raymond Lu
 *		July 10, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    hid_t		file, fapl;
    char		filename[1024];
    const char          *FUNC_main="main";

    fprintf(stderr, "   This program tests the Error API compatible with HDF5 v1.6.  There're supposed to be some error messages\n");
    fapl = h5_fileaccess();

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
	TEST_ERROR ;

    /* Test error stack */

    /* Push an error onto error stack */
    H5Epush1(__FILE__, FUNC_main, __LINE__, H5E_ERROR, H5E_BADVALUE,
            "Error test failed");

    /* Print out the errors on stack */
    dump_error();

    /* Empty error stack */
    H5Eclear1();

    /* Test error API */
    if(test_error(file) < 0) {
        H5Epush1(__FILE__, FUNC_main, __LINE__, H5E_ERROR, H5E_BADMESG,
                "Error test failed");
        H5Eprint1(stderr);
    }

    if(H5Fclose(file) < 0) TEST_ERROR ;
    h5_cleanup(FILENAME, fapl);

    printf("All error API tests passed.\n");
    return 0;

 error:
    printf("***** ERROR TEST FAILED! *****\n");
    return 1;
}
#endif /* H5_NO_DEPRECATED_SYMBOLS */

