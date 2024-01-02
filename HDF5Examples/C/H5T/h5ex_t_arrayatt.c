/************************************************************

  This example shows how to read and write array datatypes
  to an attribute.  The program first writes integers arrays
  of dimension ADIM0xADIM1 to an attribute with a dataspace
  of DIM0, then closes the  file.  Next, it reopens the
  file, reads back the data, and outputs it to the screen.

 ************************************************************/

#include "hdf5.h"
#include <stdio.h>
#include <stdlib.h>

#define FILE      "h5ex_t_arrayatt.h5"
#define DATASET   "DS1"
#define ATTRIBUTE "A1"
#define DIM0      4
#define ADIM0     3
#define ADIM1     5

int
main(void)
{
    hid_t file     = H5I_INVALID_HID; /* File Handle */
    hid_t space    = H5I_INVALID_HID; /* Dataspace Handle */
    hid_t dset     = H5I_INVALID_HID; /* Dataset Handle */
    hid_t filetype = H5I_INVALID_HID;
    hid_t memtype  = H5I_INVALID_HID;
    hid_t attr     = H5I_INVALID_HID; /* Attribute Handle */
    /* Handles */
    herr_t  status;
    hsize_t dims[1]  = {DIM0};
    hsize_t adims[2] = {ADIM0, ADIM1};
    int     wdata[DIM0][ADIM0][ADIM1]; /* Write buffer */
    int  ***rdata = NULL;              /* Read buffer */
    int     ndims;
    hsize_t i, j, k;

    /*
     * Initialize data.  i is the element in the dataspace, j and k the
     * elements within the array datatype.
     */
    for (i = 0; i < DIM0; i++)
        for (j = 0; j < ADIM0; j++)
            for (k = 0; k < ADIM1; k++)
                wdata[i][j][k] = i * j - j * k + i * k;

    /*
     * Create a new file using the default properties.
     */
    file = H5Fcreate(FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Create array datatypes for file and memory.
     */
    filetype = H5Tarray_create(H5T_STD_I64LE, 2, adims);
    memtype  = H5Tarray_create(H5T_NATIVE_INT, 2, adims);

    /*
     * Create dataset with a null dataspace.
     */
    space  = H5Screate(H5S_NULL);
    dset   = H5Dcreate(file, DATASET, H5T_STD_I32LE, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    status = H5Sclose(space);

    /*
     * Create dataspace.  Setting maximum size to NULL sets the maximum
     * size to be the current size.
     */
    space = H5Screate_simple(1, dims, NULL);

    /*
     * Create the attribute and write the array data to it.
     */
    attr   = H5Acreate(dset, ATTRIBUTE, filetype, space, H5P_DEFAULT, H5P_DEFAULT);
    status = H5Awrite(attr, memtype, wdata[0][0]);

    /*
     * Close and release resources.
     */
    status = H5Aclose(attr);
    status = H5Dclose(dset);
    status = H5Sclose(space);
    status = H5Tclose(filetype);
    status = H5Tclose(memtype);
    status = H5Fclose(file);

    /*
     * Now we begin the read section of this example.  Here we assume
     * the attribute and array have the same name and rank, but can
     * have any size.  Therefore we must allocate a new array to read
     * in data using malloc().
     */

    /*
     * Open file, dataset, and attribute.
     */
    file = H5Fopen(FILE, H5F_ACC_RDONLY, H5P_DEFAULT);
    dset = H5Dopen(file, DATASET, H5P_DEFAULT);
    attr = H5Aopen(dset, ATTRIBUTE, H5P_DEFAULT);

    /*
     * Get the datatype and its dimensions.
     */
    filetype = H5Aget_type(attr);
    ndims    = H5Tget_array_dims(filetype, adims);

    /*
     * Get dataspace and allocate memory for read buffer.  This is a
     * three dimensional attribute when the array datatype is included
     * so the dynamic allocation must be done in steps.
     */
    space = H5Aget_space(attr);
    ndims = H5Sget_simple_extent_dims(space, dims, NULL);

    /*
     * Allocate array of pointers to two-dimensional arrays (the
     * elements of the attribute.
     */
    rdata = (int ***)malloc(dims[0] * sizeof(int **));

    /*
     * Allocate two dimensional array of pointers to rows in the data
     * elements.
     */
    rdata[0] = (int **)malloc(dims[0] * adims[0] * sizeof(int *));

    /*
     * Allocate space for integer data.
     */
    rdata[0][0] = (int *)malloc(dims[0] * adims[0] * adims[1] * sizeof(int));

    /*
     * Set the members of the pointer arrays allocated above to point
     * to the correct locations in their respective arrays.
     */
    for (i = 0; i < dims[0]; i++) {
        rdata[i] = rdata[0] + i * adims[0];
        for (j = 0; j < adims[0]; j++)
            rdata[i][j] = rdata[0][0] + (adims[0] * adims[1] * i) + (adims[1] * j);
    }

    /*
     * Create the memory datatype.
     */
    memtype = H5Tarray_create(H5T_NATIVE_INT, 2, adims);

    /*
     * Read the data.
     */
    status = H5Aread(attr, memtype, rdata[0][0]);

    /*
     * Output the data to the screen.
     */
    for (i = 0; i < dims[0]; i++) {
        printf("%s[%llu]:\n", ATTRIBUTE, i);
        for (j = 0; j < adims[0]; j++) {
            printf(" [");
            for (k = 0; k < adims[1]; k++)
                printf(" %3d", rdata[i][j][k]);
            printf("]\n");
        }
        printf("\n");
    }

    /*
     * Close and release resources.
     */
    free(rdata[0][0]);
    free(rdata[0]);
    free(rdata);
    status = H5Aclose(attr);
    status = H5Dclose(dset);
    status = H5Sclose(space);
    status = H5Tclose(filetype);
    status = H5Tclose(memtype);
    status = H5Fclose(file);

    return 0;
}
