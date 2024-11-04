/************************************************************

  This example shows how to read and write complex number
  datatypes to a dataset.  The program first writes float
  complex values to a dataset with a dataspace of DIM0xDIM1,
  then closes the file.  Next, it reopens the file, reads
  back the data, and outputs it to the screen. This example
  assumes MSVC's complex number types are supported rather
  than the C99 complex number types. For an example that uses
  the C99 complex number types, see the h5ex_t_complex.c
  example file.

 ************************************************************/

#include "hdf5.h"
#include <stdio.h>
#include <stdlib.h>
#include <complex.h>

#define FILENAME "h5ex_t_complex_msvc.h5"
#define DATASET  "DS1"
#define DIM0     4
#define DIM1     7

int
main(void)
{
    _Fcomplex   wdata[DIM0][DIM1]; /* Write buffer */
    _Fcomplex **rdata;             /* Read buffer */
    hid_t       file, space, dset; /* Handles */
    herr_t      status;
    hsize_t     dims[2] = {DIM0, DIM1};
    int         ndims;
    hsize_t     i, j;

    /*
     * Initialize data.
     */
    for (i = 0; i < DIM0; i++)
        for (j = 0; j < DIM1; j++) {
            float real      = (float)i / (j + 0.5) + j;
            float imaginary = (float)i / (j + 0.5) + j + 1;
            wdata[i][j]     = _FCbuild(real, imaginary);
        }

    /*
     * Create a new file using the default properties.
     */
    file = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Create dataspace.  Setting maximum size to NULL sets the maximum
     * size to be the current size.
     */
    space = H5Screate_simple(2, dims, NULL);

    /*
     * Create the dataset and write the complex number data to it.  In
     * this example we will save the data as complex numbers of 2 64-bit
     * little endian IEEE floating point numbers, regardless of the native
     * type.  The HDF5 library automatically converts between different
     * complex number types.
     */
    dset   = H5Dcreate(file, DATASET, H5T_COMPLEX_IEEE_F64LE, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    status = H5Dwrite(dset, H5T_NATIVE_FLOAT_COMPLEX, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata[0]);

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
    file = H5Fopen(FILENAME, H5F_ACC_RDONLY, H5P_DEFAULT);
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
    rdata = malloc(dims[0] * sizeof(_Fcomplex *));

    /*
     * Allocate space for complex number data.
     */
    rdata[0] = malloc(dims[0] * dims[1] * sizeof(_Fcomplex));

    /*
     * Set the rest of the pointers to rows to the correct addresses.
     */
    for (i = 1; i < dims[0]; i++)
        rdata[i] = rdata[0] + i * dims[1];

    /*
     * Read the data.
     */
    status = H5Dread(dset, H5T_NATIVE_FLOAT_COMPLEX, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata[0]);

    /*
     * Output the data to the screen.
     */
    printf("%s:\n", DATASET);
    for (i = 0; i < dims[0]; i++) {
        printf(" [");
        for (j = 0; j < dims[1]; j++) {
            printf(" %6.4f%+6.4fi", crealf(rdata[i][j]), cimagf(rdata[i][j]));
        }
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
