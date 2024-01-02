/************************************************************

  This example shows how to read and write variable-length
  string datatypes to an attribute.  The program first
  writes variable-length strings to an attribute with a
  dataspace of DIM0, then closes the file.  Next, it reopens
  the file, reads back the data, and outputs it to the
  screen.

  This file is intended for use with HDF5 Library version 1.8

 ************************************************************/

#include "hdf5.h"
#include <stdio.h>
#include <stdlib.h>

#define FILE      "h5ex_t_vlstringatt.h5"
#define DATASET   "DS1"
#define ATTRIBUTE "A1"
#define DIM0      4

int
main(void)
{
    hid_t file, filetype, memtype, space, dset, attr;
    /* Handles */
    herr_t  status;
    hsize_t dims[1]     = {DIM0};
    char   *wdata[DIM0] = {"Parting", "is such", "sweet", "sorrow."},
         /* Write buffer */
        **rdata; /* Read buffer */
    int ndims, i;

    /*
     * Create a new file using the default properties.
     */
    file = H5Fcreate(FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Create file and memory datatypes.  For this example we will save
     * the strings as FORTRAN strings.
     */
    filetype = H5Tcopy(H5T_FORTRAN_S1);
    status   = H5Tset_size(filetype, H5T_VARIABLE);
    memtype  = H5Tcopy(H5T_C_S1);
    status   = H5Tset_size(memtype, H5T_VARIABLE);

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
     * Create the attribute and write the variable-length string data
     * to it.
     */
    attr   = H5Acreate(dset, ATTRIBUTE, filetype, space, H5P_DEFAULT, H5P_DEFAULT);
    status = H5Awrite(attr, memtype, wdata);

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
     * Get the datatype.
     */
    filetype = H5Aget_type(attr);

    /*
     * Get dataspace and allocate memory for read buffer.
     */
    space = H5Aget_space(attr);
    ndims = H5Sget_simple_extent_dims(space, dims, NULL);
    rdata = (char **)malloc(dims[0] * sizeof(char *));

    /*
     * Create the memory datatype.
     */
    memtype = H5Tcopy(H5T_C_S1);
    status  = H5Tset_size(memtype, H5T_VARIABLE);

    /*
     * Read the data.
     */
    status = H5Aread(attr, memtype, rdata);

    /*
     * Output the data to the screen.
     */
    for (i = 0; i < dims[0]; i++)
        printf("%s[%d]: %s\n", ATTRIBUTE, i, rdata[i]);

    /*
     * Close and release resources.  Note that H5Dvlen_reclaim works
     * for variable-length strings as well as variable-length arrays.
     * Also note that we must still free the array of pointers stored
     * in rdata, as H5Tvlen_reclaim only frees the data these point to.
     */
    status = H5Dvlen_reclaim(memtype, space, H5P_DEFAULT, rdata);
    free(rdata);
    status = H5Aclose(attr);
    status = H5Dclose(dset);
    status = H5Sclose(space);
    status = H5Tclose(filetype);
    status = H5Tclose(memtype);
    status = H5Fclose(file);

    return 0;
}
