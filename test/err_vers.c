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
 * Programmer:  Neil Fortner <nfortne2@hdfgroup.org>
 *              Saturday, October 11, 2008
 *
 * Purpose:	Tests H5E version compatibility macros.
 */
#include "h5test.h"
#include "H5Gprivate.h"		/* Groups				*/

#define ERR_VERS_NAME   "err_vers"
#define LINK_BUF_SIZE   1024
#define NAME_BUF_SIZE   1024
#define MAX_NAME_LEN    ((64*1024)+1024)


/*-------------------------------------------------------------------------
 *
 * Function:    error_version_macros_cb
 *
 * Purpose:     Callback function for H5Ewalk in error_version_macros.
 *
 * Return:      0
 *
 * Programmer:  Neil Fortner
 *              Saturday, October 11, 2008
 *
 *-------------------------------------------------------------------------
 */
herr_t
error_version_macros_cb(int n, H5E_error1_t *err_desc, void *client_data)
{
    return 0;
}


/*-------------------------------------------------------------------------
 *
 * Function:    error_version_macros
 *
 * Purpose:     Test H5E version compatibility macros.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Neil Fortner
 *              Saturday, October 11, 2008
 *
 *-------------------------------------------------------------------------
 */
static int
error_version_macros(void)
{
    H5E_walk1_t     walk_cb = &error_version_macros_cb; /* H5Ewalk callback function (test H5E_walk1_t) */
    H5E_auto1_t     auto_func;                      /* Automatic error handler (test H5E_auto1_t) */
    void            *client_data;                   /* Automatic error handler data */
    FILE            *file;                          /* File to redirect output to */
    char            filename[NAME_BUF_SIZE]; 

    /* Output message about test being performed */
    TESTING("H5E version compatibility macros");

    /* Create the test file (used to prevent output of H5Eprint to the terminal) */
    h5_fixname(ERR_VERS_NAME, H5P_DEFAULT, filename, sizeof filename);
    if (NULL == (file = HDfopen(filename, "w"))) TEST_ERROR;

    /* Print error stack (check H5Eprint1) */
    if (H5Eprint1(file) < 0) TEST_ERROR;

    /* Get automatic error traversal function (check H5Eget_auto1) */
    if (H5Eget_auto1(&auto_func, &client_data) < 0) TEST_ERROR;

    /* Set automatic error traversal function (check H5Eset_auto1) */
    if (H5Eset_auto1(auto_func, client_data) < 0) TEST_ERROR;

    /* Push an error message (check H5Epush1) */
    if (H5Epush1("somefile", "somefunc", 13241, H5E_STORAGE, H5E_BADATOM,
        "Hi!  I'm an error message!") < 0) TEST_ERROR;

    /* Clear error stack (check H5Eclear1) */
    if (H5Eclear1() < 0) TEST_ERROR;

    /* Walk the error stack (check H5Ewalk1) */
    if (H5Ewalk1(H5E_WALK_DOWNWARD, walk_cb, NULL) < 0) TEST_ERROR;

    /* Close the file */
    HDfclose(file);
    HDremove(filename);

    PASSED();
    return 0;

error:
    HDfclose(file);
    HDremove(filename);
    return -1;
} /* end error_version_macros() */


/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	Test H5E version compatibility macros.
 *
 * Return:	Success:	exit(0)
 *
 *		Failure:	exit(non-zero)
 *
 * Programmer:	Neil Fortner
 *              Saturday, October 11, 2008
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    int		nerrors = 0;

    h5_reset();

    /* The test... */
    nerrors += error_version_macros() < 0 ? 1 : 0;

    /* Results */
    if (nerrors) {
	printf("***** %d ERROR COMPATIBILITY TEST%s FAILED! *****\n",
	       nerrors, 1 == nerrors ? "" : "S");
	exit(1);
    }
    printf("All error compatibility tests passed.\n");
    return 0;
}
