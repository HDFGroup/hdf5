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
 * Generate the binary hdf5 files for the h5format_convert tests.
 * Usage: just execute the program without any arguments will
 * generate all the binary hdf5 files
 *
 * If you regenerate the test files (e.g., changing some code,
 * trying it on a new platform, ...), you need to verify the correctness
 * of the expected output and update the corresponding *.ddl files.
 */

#include <string.h>
#include "h5fc_gentest.h"

#define NON_V3_FILE    "h5fc_non_v3.h5"
#define EDGE_V3_FILE   "h5fc_edge_v3.h5"
#define ERR_LEVEL_FILE "h5fc_err_level.h5"

static const char *FILENAME[] = {"h5fc_ext1_i.h5",   /* 0 */
                                 "h5fc_ext1_s.h5",   /* 1 */
                                 "h5fc_ext1_f.h5",   /* 2 */
                                 "h5fc_ext2_is.h5",  /* 3 */
                                 "h5fc_ext2_if.h5",  /* 4 */
                                 "h5fc_ext2_sf.h5",  /* 5 */
                                 "h5fc_ext3_isf.h5", /* 6 */
                                 "h5fc_ext_none.h5", /* 7 */
                                 NULL};

int
main(void)
{
    unsigned i, new_format;

    /* Generate a non-latest-format file with v3 superblock */
    gen_non(NON_V3_FILE);

    /* Generate a new format file with a no-filter-edge-chunk dataset */
    gen_edge(EDGE_V3_FILE);

    /* Generate a new format file with 'K' value of 1 in H5Pset_istore_k() */
    gen_err_level(ERR_LEVEL_FILE);

    /* Generate old/new format file with/without messages in the superblock extension */
    for (new_format = false; new_format <= true; new_format++) {
        for (i = 0; i < 8; i++) {
            char filename[50];

            memset(filename, 0, sizeof(filename));
            if (!new_format)
                strcat(filename, "old_");
            strcat(filename, FILENAME[i]);

            gen_ext(filename, new_format, i);
        } /* end for */
    }     /* end for */

    return 0;
} /* end main */
