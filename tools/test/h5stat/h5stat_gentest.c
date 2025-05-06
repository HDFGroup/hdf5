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

/*
 * Generate the binary hdf5 files for the h5stat tests.
 * Usage: just execute the program without any arguments will
 * generate all the binary hdf5 files
 *
 * If you regenerate the test files (e.g., changing some code,
 * trying it on a new platform, ...), you need to verify the correctness
 * of the expected output and update the corresponding *.ddl files.
 */
#include "hdf5.h"
#include "H5private.h"
#include "h5stat_gentest.h"

#define NEWGRAT_FILE      "h5stat_newgrat.h5"
#define IDX_FILE          "h5stat_idx.h5"
#define THRESHOLD_FILE    "h5stat_threshold.h5"
#define ERR_REFCOUNT_FILE "h5stat_err_refcount.h5"

/*
 * The following two test files are generated with older versions
 * of the library for HDFFV-10333.  They are used for testing in
 * testh5stat.sh.in.
 *
 * (1) h5stat_err_old_layout.h5
 *     This file is generated with the 1.6 library so that a file
 *     with a version 2 layout message is created.
 *     Then a "0" is written to the "dimension" field in the layout
 *     message to trigger the error.
 *     This is to verify HDFFV-10333 that h5stat will exit gracefully
 *     when encountered error similar to H5O__layout_decode in the
 *     jira issue.
 *
 * (2) h5stat_err_old_fill.h5
 *     This file is generated with the 1.4 library so that a file
 *     with an old fill value message is created.
 *     Then an illegal size is written to the "size" fild in the
 *     fill value message to trigger the error.
 *     This is to verify HDFFV-10333 that h5stat will exit gracefully
 *     when encountered error similar to H5O_fill_old_decode in the
 *     jira issue.
 */
int
main(void)
{
    if (gen_newgrat_file(NEWGRAT_FILE) < 0)
        goto error;
    if (gen_threshold_file(THRESHOLD_FILE) < 0)
        goto error;

    /* Generate an HDF file to test for datasets with Fixed Array indexing */
    if (gen_idx_file(IDX_FILE) < 0)
        goto error;

    /* Generate a file with a refcount message ID */
    if (gen_err_refcount(ERR_REFCOUNT_FILE) < 0)
        goto error;

    return EXIT_SUCCESS;

error:
    fprintf(stderr, "h5stat test generator FAILED\n");
    return EXIT_FAILURE;
}
