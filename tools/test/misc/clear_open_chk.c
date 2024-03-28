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
#include "hdf5.h"
#include "H5private.h"
#include "h5tools.h"

static void usage(void);

static void
usage(void)
{
    fprintf(stdout, "\n");
    fprintf(stdout, "Usage error!\n");
    fprintf(stdout, "Usage: clear_open_chk filename\n");
} /* usage() */

/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:    To open the file which has zero or nonzero status_flags in
 *        the superblock.
 *
 * Return:    0 on success
 *        1 on failure
 *
 *-------------------------------------------------------------------------
 */
int
main(int argc, char *argv[])
{
    char *fname; /* The HDF5 file name */
    hid_t fid;   /* File ID */

    /* Check the # of arguments */
    if (argc != 2) {
        usage();
        exit(EXIT_FAILURE);
    }

    /* Get the file name */
    fname = strdup(argv[1]);

    /* Try opening the file */
    if ((fid = h5tools_fopen(fname, H5F_ACC_RDONLY, H5P_DEFAULT, false, NULL, (size_t)0)) < 0) {
        fprintf(stderr, "clear_open_chk: unable to open the file\n");
        free(fname);
        exit(EXIT_FAILURE);
    }
    free(fname);

    /* Close the file */
    if (H5Fclose(fid) < 0) {
        fprintf(stderr, "clear_open_chk: cannot close the file\n");
        exit(EXIT_FAILURE);
    }

    /* Return success */
    exit(EXIT_SUCCESS);
} /* main() */
