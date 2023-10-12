/************************************************************

  This example shows how to read and write bitfield
  datatypes to an attribute.  The program first writes bit
  fields to an attribute with a dataspace of DIM0xDIM1, then
  closes the file.  Next, it reopens the file, reads back
  the data, and outputs it to the screen.

 ************************************************************/

#include "hdf5.h"
#include <stdio.h>
#include <stdlib.h>

#define FILE      "h5ex_t_bitatt.h5"
#define DATASET   "DS1"
#define ATTRIBUTE "A1"
#define DIM0      4
#define DIM1      7

int
main(void)
{
    hid_t         file, space, dset, attr; /* Handles */
    herr_t        status;
    hsize_t       dims[2] = {DIM0, DIM1};
    unsigned char wdata[DIM0][DIM1], /* Write buffer */
        **rdata;                     /* Read buffer */
    int     ndims, A, B, C, D;
    hsize_t i, j;

    /*
     * Initialize data.  We will manually pack 4 2-bit integers into
     * each unsigned char data element.
     */
    for (i = 0; i < DIM0; i++)
        for (j = 0; j < DIM1; j++) {
            wdata[i][j] = 0;
            wdata[i][j] |= (i * j - j) & 0x03;    /* Field "A" */
            wdata[i][j] |= (i & 0x03) << 2;       /* Field "B" */
            wdata[i][j] |= (j & 0x03) << 4;       /* Field "C" */
            wdata[i][j] |= ((i + j) & 0x03) << 6; /* Field "D" */
        }

    /*
     * Create a new file using the default properties.
     */
    file = H5Fcreate(FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

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
    space = H5Screate_simple(2, dims, NULL);

    /*
     * Create the attribute and write the bitfield data to it.
     */
    attr   = H5Acreate(dset, ATTRIBUTE, H5T_STD_B8BE, space, H5P_DEFAULT, H5P_DEFAULT);
    status = H5Awrite(attr, H5T_NATIVE_B8, wdata[0]);

    /*
     * Close and release resources.
     */
    status = H5Aclose(attr);
    status = H5Dclose(dset);
    status = H5Sclose(space);
    status = H5Fclose(file);

    /*
     * Now we begin the read section of this example.  Here we assume
     * the attribute has the same name and rank, but can have any size.
     * Therefore we must allocate a new array to read in data using
     * malloc().
     */

    /*
     * Open file, dataset, and attribute.
     */
    file = H5Fopen(FILE, H5F_ACC_RDONLY, H5P_DEFAULT);
    dset = H5Dopen(file, DATASET, H5P_DEFAULT);
    attr = H5Aopen(dset, ATTRIBUTE, H5P_DEFAULT);

    /*
     * Get dataspace and allocate memory for read buffer.  This is a
     * two dimensional attribute so the dynamic allocation must be done
     * in steps.
     */
    space = H5Aget_space(attr);
    ndims = H5Sget_simple_extent_dims(space, dims, NULL);

    /*
     * Allocate array of pointers to rows.
     */
    rdata = (unsigned char **)malloc(dims[0] * sizeof(unsigned char *));

    /*
     * Allocate space for bitfield data.
     */
    rdata[0] = (unsigned char *)malloc(dims[0] * dims[1] * sizeof(unsigned char));

    /*
     * Set the rest of the pointers to rows to the correct addresses.
     */
    for (i = 1; i < dims[0]; i++)
        rdata[i] = rdata[0] + i * dims[1];

    /*
     * Read the data.
     */
    status = H5Aread(attr, H5T_NATIVE_B8, rdata[0]);

    /*
     * Output the data to the screen.
     */
    printf("%s:\n", ATTRIBUTE);
    for (i = 0; i < dims[0]; i++) {
        printf(" [");
        for (j = 0; j < dims[1]; j++) {
            A = rdata[i][j] & 0x03;        /* Retrieve field "A" */
            B = (rdata[i][j] >> 2) & 0x03; /* Retrieve field "B" */
            C = (rdata[i][j] >> 4) & 0x03; /* Retrieve field "C" */
            D = (rdata[i][j] >> 6) & 0x03; /* Retrieve field "D" */
            printf(" {%d, %d, %d, %d}", A, B, C, D);
        }
        printf(" ]\n");
    }

    /*
     * Close and release resources.
     */
    free(rdata[0]);
    free(rdata);
    status = H5Aclose(attr);
    status = H5Dclose(dset);
    status = H5Sclose(space);
    status = H5Fclose(file);

    return 0;
}
