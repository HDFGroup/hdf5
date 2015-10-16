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

/* Purpose:     This is a small program that checks if the HDF5_DRIVER
 *              environment variable is set to a value that supports SWMR.
 *              
 *              It is intended for use in shell scripts.
 */

#include <stdlib.h>

#include "H5private.h"

/* This file needs to access the file driver testing code */
#define H5FD_FRIEND		/*suppress error about including H5FDpkg	  */
#define H5FD_TESTING
#include "H5FDpkg.h"	/* File drivers	 			*/


/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     Inspects the HDF5_DRIVER environment variable, which
 *              determines the VFD that the test harness will use with
 *              the majority of the tests.
 *
 * Return:      VFD supports SWMR:          EXIT_SUCCESS
 *
 *              VFD does not support SWMR
 *              or failure:                 EXIT_FAILURE
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    char *driver = NULL;

    driver = HDgetenv("HDF5_DRIVER");

    if(H5FD_supports_swmr_test(driver))
        return EXIT_SUCCESS;
    else
        return EXIT_FAILURE;

} /* end main() */

