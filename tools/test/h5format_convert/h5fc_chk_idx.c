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

/*
 * A program to verify that the chunk indexing type of a dataset in a file
 * is version 1 B-tree.
 * This is to support the testing of the tool "h5format_convert".
 */

#include "hdf5.h"
#include "H5private.h"
#include "h5tools.h"

static void usage(void);

static void
usage(void)
{
    fprintf(stdout, "Usage: h5fc_chk_idx file_name dataset_pathname\n");
} /* usage() */

/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	To check that the chunk indexing type for the dataset in
 *		the file is version 1 B-tree.
 *
 * Return:	0 -- the indexing type is version 1 B-tree
 *		1 -- otherwise
 *
 *-------------------------------------------------------------------------
 */
int
main(int argc, char *argv[])
{
    char             *fname = NULL;
    char             *dname = NULL;
    hid_t             fid   = H5I_INVALID_HID;
    hid_t             did   = H5I_INVALID_HID;
    H5D_chunk_index_t idx_type;

    /* h5fc_chk_idx fname dname */
    if (argc != 3) {
        usage();
        exit(EXIT_FAILURE);
    } /* end if */

    /* Duplicate the file name  & dataset name */
    fname = strdup(argv[1]);
    dname = strdup(argv[2]);

    /* Try opening the file */
    if ((fid = h5tools_fopen(fname, H5F_ACC_RDONLY, H5P_DEFAULT, false, NULL, (size_t)0)) < 0) {
        fprintf(stderr, "h5fc_chk_idx: unable to open the file\n");
        goto error;
    } /* end if */

    /* Open the dataset */
    if ((did = H5Dopen2(fid, dname, H5P_DEFAULT)) < 0) {
        fprintf(stderr, "h5fc_chk_idx: unable to open the dataset\n");
        goto error;
    } /* end if */

    /* Get the dataset's chunk indexing type */
    if (H5Dget_chunk_index_type(did, &idx_type) < 0) {
        fprintf(stderr, "h5fc_chk_idx: unable to get chunk index type for the dataset\n");
        goto error;
    } /* end if */

    /* Close the dataset */
    if (H5Dclose(did) < 0) {
        fprintf(stderr, "h5fc_chk_idx: unable to close the dataset\n");
        goto error;
    } /* end if */

    /* Close the file */
    if (H5Fclose(fid) < 0) {
        fprintf(stderr, "h5fc_chk_idx_type: cannot close the file\n");
        goto error;
    } /* end if */

    /* Return success when the chunk indexing type is version 1 B-tree */
    if (idx_type != H5D_CHUNK_IDX_BTREE) {
        fprintf(stderr, "Error: chunk indexing type is %d\n", idx_type);
        goto error;
    } /* end if */

    free(fname);
    free(dname);

    exit(EXIT_SUCCESS);

error:
    free(fname);
    free(dname);

    exit(EXIT_FAILURE);
} /* main() */
