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
 * Programmer:  Dana Robinson
 *              Spring 2016
 *
 * Purpose:     Tests the basic operation of the evict-on-close cache
 *              behavior. Tests that ensure the tagging is handled correctly
 *              are located in cache.c.
 */

#include "h5test.h"

static herr_t check_evict_on_close_api(void);


/*-------------------------------------------------------------------------
 * Function:    check_evict_on_close_api()
 *
 * Purpose:     Verify that the H5Pset/get_evict_on_close() calls behave
 *              correctly.
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Dana Robinson
 *              Spring 2016
 *
 *-------------------------------------------------------------------------
 */
static herr_t
check_evict_on_close_api(void)
{
    hid_t       fapl_id = -1;
    hid_t       dapl_id = -1;
    hbool_t     evict_on_close;
    herr_t      status;

    TESTING("evict on close API");

    /* Create a fapl */
    if((fapl_id = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        FAIL_STACK_ERROR;

    /* Check the default */
    evict_on_close = TRUE;
    if(H5Pget_evict_on_close(fapl_id, &evict_on_close) < 0)
        FAIL_STACK_ERROR;
    if(evict_on_close != FALSE)
        FAIL_PUTS_ERROR("Incorrect default evict on close value.");

    /* Set the evict on close property */
    evict_on_close = TRUE;
    if(H5Pset_evict_on_close(fapl_id, evict_on_close) < 0)
        FAIL_STACK_ERROR;

    /* Make sure we can get it back out */
    evict_on_close = FALSE;
    if(H5Pget_evict_on_close(fapl_id, &evict_on_close) < 0)
        FAIL_STACK_ERROR;
    if(evict_on_close != TRUE)
        FAIL_PUTS_ERROR("Incorrect evict on close value.");

    /* close fapl */
    if(H5Pclose(fapl_id) < 0)
        FAIL_STACK_ERROR;

    /**********************************************/
    /* Trying passing in a non-fapl property list */
    /**********************************************/

    if((dapl_id = H5Pcreate(H5P_DATASET_ACCESS)) < 0)
        FAIL_STACK_ERROR;

    /* ensure using an incorrect access plist fails */
    H5E_BEGIN_TRY {
        status = H5Pset_evict_on_close(dapl_id, evict_on_close);
    } H5E_END_TRY;
    if(status >= 0)
        FAIL_PUTS_ERROR("H5Pset_evict_on_close() accepted invalid access plist.");

    /* ensure an invalid plist fails */
    H5E_BEGIN_TRY {
        status = H5Pget_evict_on_close((hid_t)-1, &evict_on_close);
    } H5E_END_TRY;
    if(status >= 0)
        FAIL_PUTS_ERROR("H5Pget_evict_on_close() accepted invalid hid_t.");

    /* close dapl */
    if(H5Pclose(dapl_id) < 0)
        FAIL_STACK_ERROR;

    PASSED();
    return SUCCEED;

error:
    H5_FAILED();
    return FAIL;

} /* check_evict_on_close_api() */


/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Return:      EXIT_FAILURE/EXIT_SUCCESS
 *
 * Programmer:  Dana Robinson
 *              Spring 2016
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    unsigned nerrors = 0;

    printf("Testing evict-on-close cache behavior.\n");

    nerrors += check_evict_on_close_api() < 0 ? 1 : 0;

    if(nerrors) {
        printf("***** %u evict-on-close test%s FAILED! *****\n",
            nerrors, nerrors > 1 ? "S" : "");
        return EXIT_FAILURE;
    }

    printf("All evict-on-close tests passed.\n");

    return EXIT_SUCCESS;
}

