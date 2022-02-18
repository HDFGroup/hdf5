/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by Akadio, Inc.                                                 *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#include "H5private.h" /* H5_ATTR_UNUSED */
#include "H5Fpublic.h"
#include "H5FDvfd_swmr.h"

/* vfd_swmr_writer_may_increase_tick_to() and
 * vfd_swmr_reader_did_increase_tick_to() are instrumentation points for
 * VFD SWMR tests to use to coordinate the tick-number increases
 * on a single writer with the progress of a single reader.
 *
 * This file provides the default, do-nothing implementations for both
 * instrumentation routines.
 *
 * A VFD SWMR writer calls vfd_swmr_writer_may_increase_tick_to() with the
 * increased tick number that it proposes, `tick_num`.  The argument
 * `wait_for_reader` tells whether or not the writer can wait for the reader
 * before increasing its tick number.  If `true`, then
 * vfd_swmr_writer_may_increase_tick_to() should
 * block until the reader is finished using the shadow-file content
 * from ticks `tick_num - max_lag` and before, returning `true`.
 * If `false`, then
 * vfd_swmr_writer_may_increase_tick_to() immediately return `true` if
 * the new tick number does permissible, otherwise `false`.
 *
 * After a VFD SWMR reader increases its tick number, it calls
 * vfd_swmr_reader_did_increase_tick_to() with the new tick number.
 *
 * The test programs test/vfd_swmr_zoo_{reader,writer} provide
 * their own vfd_swmr_writer_may_increase_tick_to() and
 * vfd_swmr_reader_did_increase_tick_to() implementations that override the
 * ones in the library.  In the "zoo"
 * test (test/vfd_swmr_zoo_{reader,writer}), the reader and the writer
 * use a shared file to coordinate tick-number increases so that the writer
 * can call H5Fvfd_swmr_end_tick() to increase its tick number at an arbitrary
 * rate without outrunning the reader.
 */
bool
vfd_swmr_writer_may_increase_tick_to(uint64_t H5_ATTR_UNUSED tick_num, bool H5_ATTR_UNUSED wait_for_reader)
{
    return true;
}

void
vfd_swmr_reader_did_increase_tick_to(uint64_t H5_ATTR_UNUSED tick_num)
{
    return;
}
