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
 * Generate the binary hdf5 files and user block data for the jam/unjam tests.
 * Usage: just execute the program without any arguments will
 * generate all the files in the local directory.
 *
 * If you regenerate the test files (e.g., changing some code,
 * trying it on a new platform, ...), you need to verify the correctness
 * of the expected output and update the corresponding *.ddl files.
 */

#include "h5jamgentest.h"

/* not used yet
#define UBTXT1 "u0.txt"
*/
#define UBTXT2 "u10.txt"
#define UBTXT3 "u511.txt"
#define UBTXT4 "u512.txt"
#define UBTXT5 "u513.txt"

/* tall is same as dumper test */
#define FILE7 "tall.h5"
#define FILE8 "twithub.h5"
#define FILE9 "twithub513.h5"

/*-------------------------------------------------------------------------
 * Function: main
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    if (create_textfile(UBTXT2, 10) < 0)
        goto error;
    if (create_textfile(UBTXT3, 511) < 0)
        goto error;
    if (create_textfile(UBTXT4, 512) < 0)
        goto error;
    if (create_textfile(UBTXT5, 513) < 0)
        goto error;

    if (gent_ub(FILE7, 0, 0) < 0)
        goto error;
    if (gent_ub(FILE8, 512, PATTERN_LEN) < 0)
        goto error;
    if (gent_ub(FILE9, 1024, 513) < 0)
        goto error;

    return 0;

error:
    fprintf(stderr, "h5jam test generator FAILED\n");
    return 1;
}
