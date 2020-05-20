/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#include "H5private.h"  /* H5_ATTR_UNUSED */
#include "H5Fpublic.h"
#include "H5FDvfd_swmr.h"

bool
vfd_swmr_writer_may_increase_tick_to(uint64_t H5_ATTR_UNUSED tick_num,
    bool H5_ATTR_UNUSED wait_for_reader)
{
    return true;
}

void
vfd_swmr_reader_did_increase_tick_to(uint64_t H5_ATTR_UNUSED tick_num)
{
    return;
}
