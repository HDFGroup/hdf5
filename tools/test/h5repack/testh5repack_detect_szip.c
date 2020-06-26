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

#include "h5repack.h"
#include "h5tools.h"
#include "h5tools_utils.h"
#include "h5test.h"


/* Name of tool */
#define PROGRAMNAME "h5repack_detect_szip"

/*-------------------------------------------------------------------------
 * Function: main
 *
 * Purpose: detects szip encoder, prints "yes" or "no" to stdout.
 *          Intended to be used in test scripts.
 *
 * Return:
 *
 * Programmer:
 *
 * Date:
 *
 * Comments:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */


int main(void)
{
    h5tools_setprogname(PROGRAMNAME);
    h5tools_setstatus(EXIT_SUCCESS);

    /* Initialize h5tools lib */
    h5tools_init();

#ifdef H5_HAVE_FILTER_SZIP
    if (h5tools_can_encode(H5Z_FILTER_SZIP) == 1) {
        HDprintf("yes\n");
        return(1);
    }
#endif /* H5_HAVE_FILTER_SZIP */
    HDprintf("no\n");
    return(0);
}
