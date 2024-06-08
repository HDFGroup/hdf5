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
 *  This example illustrates how to read/write a subset of data (a slab)
 *  from/to a dataset in an HDF5 file.  It is used in the HDF5 Tutorial.
 */

#include "hdf5.h"

#define FILE        "subset.h5"
#define DATASETNAME "IntArray"
#define RANK        2

#define DIM0_SUB 3 /* subset dimensions */
#define DIM1_SUB 4

#define DIM0 8 /* size of dataset */
#define DIM1 10

int
main(void)
{
    hsize_t dims[2], dimsm[2];
    int     data[DIM0][DIM1];          /* data to write */
    int     sdata[DIM0_SUB][DIM1_SUB]; /* subset to write */
    int     rdata[DIM0][DIM1];         /* buffer for read */

    hid_t file_id, dataset_id; /* handles */
    hid_t dataspace_id, memspace_id;

    herr_t status;

    hsize_t count[2];  /* size of subset in the file */
    hsize_t offset[2]; /* subset offset in the file */
    hsize_t stride[2];
    hsize_t block[2];
    int     i, j;

    /*****************************************************************
     * Create a new file with default creation and access properties.*
     * Then create a dataset and write data to it and close the file *
     * and dataset.                                                  *
     *****************************************************************/

    file_id = H5Fcreate(FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    dims[0]      = DIM0;
    dims[1]      = DIM1;
    dataspace_id = H5Screate_simple(RANK, dims, NULL);

    dataset_id =
        H5Dcreate2(file_id, DATASETNAME, H5T_STD_I32BE, dataspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    for (j = 0; j < DIM0; j++) {
        for (i = 0; i < DIM1; i++)
            if (i < (DIM1 / 2))
                data[j][i] = 1;
            else
                data[j][i] = 2;
    }

    status = H5Dwrite(dataset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data);

    printf("\nData Written to File:\n");
    for (i = 0; i < DIM0; i++) {
        for (j = 0; j < DIM1; j++)
            printf(" %i", data[i][j]);
        printf("\n");
    }
    status = H5Sclose(dataspace_id);
    status = H5Dclose(dataset_id);
    status = H5Fclose(file_id);

    /*****************************************************
     * Reopen the file and dataset and write a subset of *
     * values to the dataset.
     *****************************************************/

    file_id    = H5Fopen(FILE, H5F_ACC_RDWR, H5P_DEFAULT);
    dataset_id = H5Dopen2(file_id, DATASETNAME, H5P_DEFAULT);

    /* Specify size and shape of subset to write. */

    offset[0] = 1;
    offset[1] = 2;

    count[0] = DIM0_SUB;
    count[1] = DIM1_SUB;

    stride[0] = 1;
    stride[1] = 1;

    block[0] = 1;
    block[1] = 1;

    /* Create memory space with size of subset. Get file dataspace
       and select subset from file dataspace. */

    dimsm[0]    = DIM0_SUB;
    dimsm[1]    = DIM1_SUB;
    memspace_id = H5Screate_simple(RANK, dimsm, NULL);

    dataspace_id = H5Dget_space(dataset_id);
    status       = H5Sselect_hyperslab(dataspace_id, H5S_SELECT_SET, offset, stride, count, block);

    /* Write a subset of data to the dataset, then read the
       entire dataset back from the file.  */

    printf("\nWrite subset to file specifying:\n");
    printf("    offset=1x2 stride=1x1 count=3x4 block=1x1\n");
    for (j = 0; j < DIM0_SUB; j++) {
        for (i = 0; i < DIM1_SUB; i++)
            sdata[j][i] = 5;
    }

    status = H5Dwrite(dataset_id, H5T_NATIVE_INT, memspace_id, dataspace_id, H5P_DEFAULT, sdata);

    status = H5Dread(dataset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata);

    printf("\nData in File after Subset is Written:\n");
    for (i = 0; i < DIM0; i++) {
        for (j = 0; j < DIM1; j++)
            printf(" %i", rdata[i][j]);
        printf("\n");
    }

    status = H5Sclose(memspace_id);
    status = H5Sclose(dataspace_id);
    status = H5Dclose(dataset_id);
    status = H5Fclose(file_id);
}
