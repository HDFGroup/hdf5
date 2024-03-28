/************************************************************

  This example shows how to read and write integer datatypes
  to a dataset.  The program first writes integers to a
  dataset with a dataspace of DIM0xDIM1, then closes the
  file.  Next, it reopens the file, reads back the data, and
  outputs it to the screen.

 ************************************************************/

#include "hdf5.h"
#include <stdio.h>
#include <stdlib.h>

#define FILE    "h5ex_t_int.h5"
#define DATASET "DS1"
#define DIM0    4
#define DIM1    7

int
main(void)
{
    hid_t   file, space, dset; /* Handles */
    herr_t  status;
    hsize_t dims[2] = {DIM0, DIM1};
    int     wdata[DIM0][DIM1], /* Write buffer */
        **rdata,               /* Read buffer */
        ndims;
    hsize_t i, j;

    /*
     * Initialize data.
     */
    for (i = 0; i < DIM0; i++)
        for (j = 0; j < DIM1; j++)
            wdata[i][j] = i * j - j;

    /*
     * Create a new file using the default properties.
     */
    file = H5Fcreate(FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Create dataspace.  Setting maximum size to NULL sets the maximum
     * size to be the current size.
     */
    space = H5Screate_simple(2, dims, NULL);

    /*
     * Create the dataset and write the integer data to it.  In this
     * example we will save the data as 64 bit big endian integers,
     * regardless of the native integer type.  The HDF5 library
     * automatically converts between different integer types.
     */
    dset   = H5Dcreate(file, DATASET, H5T_STD_I64BE, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    status = H5Dwrite(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata[0]);

    /*
     * Close and release resources.
     */
    status = H5Dclose(dset);
    status = H5Sclose(space);
    status = H5Fclose(file);

    /*
     * Now we begin the read section of this example.  Here we assume
     * the dataset has the same name and rank, but can have any size.
     * Therefore we must allocate a new array to read in data using
     * malloc().
     */

    /*
     * Open file and dataset.
     */
    file = H5Fopen(FILE, H5F_ACC_RDONLY, H5P_DEFAULT);
    dset = H5Dopen(file, DATASET, H5P_DEFAULT);

    /*
     * Get dataspace and allocate memory for read buffer.  This is a
     * two dimensional dataset so the dynamic allocation must be done
     * in steps.
     */
    space = H5Dget_space(dset);
    ndims = H5Sget_simple_extent_dims(space, dims, NULL);

    /*
     * Allocate array of pointers to rows.
     */
    rdata = (int **)malloc(dims[0] * sizeof(int *));

    /*
     * Allocate space for integer data.
     */
    rdata[0] = (int *)malloc(dims[0] * dims[1] * sizeof(int));

    /*
     * Set the rest of the pointers to rows to the correct addresses.
     */
    for (i = 1; i < dims[0]; i++)
        rdata[i] = rdata[0] + i * dims[1];

    /*
     * Read the data.
     */
    status = H5Dread(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata[0]);

    /*
     * Output the data to the screen.
     */
    printf("%s:\n", DATASET);
    for (i = 0; i < dims[0]; i++) {
        printf(" [");
        for (j = 0; j < dims[1]; j++)
            printf(" %3d", rdata[i][j]);
        printf("]\n");
    }

    /*
     * Close and release resources.
     */
    free(rdata[0]);
    free(rdata);
    status = H5Dclose(dset);
    status = H5Sclose(space);
    status = H5Fclose(file);

    return 0;
}
