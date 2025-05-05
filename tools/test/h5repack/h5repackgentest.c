/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the LICENSE file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/* For generating files for verifying h5repack with external storage. . .
 *
 * Each case file should follow the format of:
 * + h5repack_<NAME>.h5
 * + h5repack_<NAME>_ex.h5
 * + h5repack_<NAME>_ex-<N>.dat
 * ...where NAME identifies the type, and N is a positive decimal number;
 * multiple external files (*.dat) are allowed per file, but they must
 * follow the pattern and be in contiguous numerical sequence starting at 0.
 *
 * Each file typename must be added to the listing for
 * `VERIFY_EXTERNAL_CONSOLIDATION` in h5repack.sh
 *
 * There is no restriction on the name, number, or structure of datasets and
 * groups in HDF5 file.
 *
 * The included datatypes should be more than adequate to verify the correctness
 * of the behavior -- if one type can be consolidated from external storage,
 * then they all can.
 */

#include "h5repackgentest.h"

/* ----------------------------------------------------------------------------
 * Create files.
 * Return 0 on success, nonzero on failure.
 */
int
main(void)
{
    int i = 0;

    for (i = 0; i < 2; i++) {
        bool external = (i & 1) ? true : false;
        if (generate_int32le_1d(external) < 0)
            printf("A generate_int32le_1d failed!\n");

        if (generate_int32le_2d(external) < 0)
            printf("A generate_int32le_2d failed!\n");

        if (generate_int32le_3d(external) < 0)
            printf("A generate_int32le_3d failed!\n");

        if (generate_uint8be(external) < 0)
            printf("A generate_uint8be failed!\n");

        if (generate_f32le(external) < 0)
            printf("A generate_f32le failed!\n");

    } /* end for external data storage or not */

    return 0;
} /* end main() */
