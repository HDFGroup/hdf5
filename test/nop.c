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

#include "testhdf5.h"
#include "H5srcdir.h"
#include "H5Iprivate.h"     /* For checking that datatype id's don't leak */

#define H5T_FRIEND        /*suppress error about including H5Tpkg      */
#include "H5Tpkg.h"

herr_t no_operation(H5T_t *t, int mode);

herr_t
no_operation(H5T_t *t, int mode)
{
        static int ncalls = 0;
        ncalls++;
        return SUCCEED;
}
