/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#include "h5test.h"
#include "vds_swmr.h"

int
main(void)
{
    hid_t fid           = -1;   /* HDF5 file ID                     */
    hid_t did           = -1;   /* dataset ID                       */
    hid_t msid          = -1;   /* memory dataspace ID              */
    hid_t fsid          = -1;   /* file dataspace ID                */

    hsize_t start[RANK];        /* hyperslab start point            */

    int n_elements      = 0;    /* size of buffer (elements)        */
    size_t size         = 0;    /* size of buffer (bytes)           */
    int *buffer         = NULL; /* data buffer                      */

    int n_dims          = -1;   /* # dimensions in dataset          */
    hsize_t dims[RANK];         /* current size of dataset          */
    hsize_t max_dims[RANK];     /* max size of dataset              */

    hbool_t has_errors  = FALSE;/* if the read data contains errors */


    /* Open the VDS file and dataset */
    if((fid = H5Fopen(VDS_FILE_NAME, H5F_ACC_RDONLY | H5F_ACC_SWMR_READ, H5P_DEFAULT)) < 0)
        TEST_ERROR
    if((did = H5Dopen2(fid, VDS_DSET_NAME, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Create the read buffer */
    n_elements = VDS_PLANE[1] * VDS_PLANE[2];
    size = n_elements * sizeof(int);
    if(NULL == (buffer = (int *)HDmalloc(size)))
        TEST_ERROR

    /* Create memory dataspace */
    if((msid = H5Screate_simple(RANK, VDS_PLANE, NULL)) < 0)
        TEST_ERROR

    /* Read data until the dataset is full (via the writer) */
    do {

        /* Refresh metadata */
        if(H5Drefresh(did) < 0)
            TEST_ERROR

        /* Get the dataset dimensions */
        if((fsid = H5Dget_space(did)) < 0)
            TEST_ERROR
        if(H5Sget_simple_extent_dims(fsid, dims, max_dims) < 0)
            TEST_ERROR

        /* Check the reported size of the VDS */
        if((n_dims = H5Sget_simple_extent_ndims(fsid)) < 0)
            TEST_ERROR
        if(n_dims != RANK)
            TEST_ERROR
        if(H5Sget_simple_extent_dims(fsid, dims, max_dims) < 0)
            TEST_ERROR
        /* NOTE: Don't care what dims[0] is. */
        if(dims[1] != FULL_HEIGHT)
            TEST_ERROR
        if(dims[2] != WIDTH)
            TEST_ERROR
        if(max_dims[0] != H5S_UNLIMITED)
            TEST_ERROR
        if(max_dims[1] != FULL_HEIGHT)
            TEST_ERROR
        if(max_dims[2] != WIDTH)
            TEST_ERROR

        /* Continue if there's nothing to read */
        if(0 == dims[0]) {
            if(H5Sclose(fsid) < 0)
                TEST_ERROR
            continue;
        }

        /* Read a plane from the VDS */
        /* At this time, we just make sure we can read planes without errors. */
        start[0] = dims[0] - 1;
        start[1] = 0;
        start[2] = 0;
        if(H5Sselect_hyperslab(fsid, H5S_SELECT_SET, start, NULL, VDS_PLANE, NULL) < 0)
            TEST_ERROR
        if(H5Dread(did, H5T_NATIVE_INT, msid, fsid, H5P_DEFAULT, buffer) < 0)
            TEST_ERROR

        if(H5Sclose(fsid) < 0)
            TEST_ERROR

    } while (dims[0] < N_PLANES_TO_WRITE);

    /* Close file and dataset */
    if(H5Sclose(msid) < 0)
        TEST_ERROR
    if(H5Dclose(did) < 0)
        TEST_ERROR
    if(H5Fclose(fid) < 0)
        TEST_ERROR

    HDfree(buffer);

    HDfprintf(stderr, "SWMR reader exited successfully\n");
    return EXIT_SUCCESS;

error:

    H5E_BEGIN_TRY {
        if(fid >= 0)
            (void)H5Fclose(fid);
        if(did >= 0)
            (void)H5Dclose(did);
        if(msid >= 0)
            (void)H5Sclose(msid);
        if(fsid >= 0)
            (void)H5Sclose(fsid);
        if(buffer != NULL)
            HDfree(buffer);
    } H5E_END_TRY

    HDfprintf(stderr, "ERROR: SWMR reader exited with errors\n");
    return EXIT_FAILURE;

} /* end main */

