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

/* Purpose:     This is a small program that checks if the HDF5_VOL_CONNECTOR
 *              environment variable is set to a value that supports virtual
 *              datasets & external links.  Currently that is limited to a
 *              VOL connector stack that is composed of _only_ the trivial
 *              single connector stack of the native VOL connector.
 *
 *              It is intended for use in shell scripts.
 */

#include "h5test.h"
#include "H5VLprivate.h" /* Virtual Object Layer                     */

/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     Uses the default file access property lists, which is
 *              initialized with the VOL connector from the HDF5_VOL_CONNECTOR
 *              environment variable to determine if virtual datasets & external
 *              links are supported.
 *
 * Return:      VOL connector supports virtual datasets & external links: EXIT_SUCCESS
 *
 *              VOL connector does not support virtual datasets & external links,
 *              or failure:                 EXIT_FAILURE
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    hid_t   fapl_id;
    hbool_t is_native; /* Whether native VOL connector is being used */

    /* Open the VDS file and dataset */
    if ((fapl_id = h5_fileaccess()) < 0)
        return EXIT_FAILURE;

    /* Check for operating with native (only) VOL connector */
    is_native = FALSE;
    if (H5VL_fapl_is_native(fapl_id, &is_native) < 0)
        return EXIT_FAILURE;

    /* Currently, only the native VOL connector supports virtual datasets */
    if (is_native)
        return EXIT_SUCCESS;
    else
        return EXIT_FAILURE;
} /* end main() */
