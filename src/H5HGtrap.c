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

/****************/
/* Module Setup */
/****************/

#include "H5HGmodule.h" /* This source code file is part of the H5HG module */

/*
 * Headers
 */
#include "H5private.h"  /* Generic Functions			*/
#include "H5Eprivate.h" /* Error handling		  	*/
#include "H5HGpkg.h"    /* Global heaps				*/

/* H5HG_trap() is an instrumentation point for the global heap.
 * The H5HG_trap() result modifies the global heap's treatment of
 * an unexpected condition that ordinarily would cause an
 * HDassert() statement to abort the program.
 *
 * Currently, just one function, H5HG_read(), calls H5HG_trap(), using
 * the `reason` string "out of bounds".
 *
 * Test programs such as test/vfd_swmr_vlstr_{reader,writer}.c provide
 * their own H5HG_trap() implementation that overrides the one in the library.
 *
 * H5HG_trap() returns `true` if the caller should generate an error-stack
 * entry and return an error code to the caller's caller.
 *
 * H5HG_trap() returns `false` if the caller should blithely carry on;
 * if NDEBUG is not #defined, then the caller will ordinarily abort the
 * program in a subsequent HDassert() statement.
 */
bool
H5HG_trap(const char *reason)
{
    return false;
}
