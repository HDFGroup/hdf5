/************************************************************

  This example shows how to read and write region references
  to a dataset.  The program first creates a dataset
  containing characters and writes references to region of
  the dataset to a new dataset with a dataspace of DIM0,
  then closes the file.  Next, it reopens the file,
  dereferences the references, and outputs the referenced
  regions to the screen.

 ************************************************************/

#include "hdf5.h"
#include <stdio.h>
#include <stdlib.h>

#define FILE     "h5ex_t_regref.h5"
#define DATASET  "DS1"
#define DATASET2 "DS2"
#define DIM0     2
#define DS2DIM0  3
#define DS2DIM1  16

int
main(void)
{
    hid_t    file     = H5I_INVALID_HID; /* File Handle */
    hid_t    space    = H5I_INVALID_HID; /* Dataspace Handle */
    hid_t    dset     = H5I_INVALID_HID; /* Dataset Handle */
    hid_t    dset2    = H5I_INVALID_HID; /* Dataset Handle */
    hid_t    memspace = H5I_INVALID_HID; /* Mem dataspace */
    herr_t   status;
    hsize_t  dims[1]      = {DIM0};
    hsize_t  dims2[2]     = {DS2DIM0, DS2DIM1};
    hsize_t  coords[4][2] = {{0, 1}, {2, 11}, {1, 0}, {2, 4}};
    hsize_t  start[2]     = {0, 0};
    hsize_t  stride[2]    = {2, 11};
    hsize_t  count[2]     = {2, 2};
    hsize_t  block[2]     = {1, 3};
    hssize_t npoints;
    ssize_t  size;
    char    *name = NULL;
    int      ndims;
    hsize_t  i;
    char     wdata2[DS2DIM0][DS2DIM1] = {"The quick brown", "fox jumps over ", "the 5 lazy dogs"};
    char    *rdata2                   = NULL;

#if H5_VERSION_GE(1, 12, 0) && !defined(H5_USE_110_API) && !defined(H5_USE_18_API) && !defined(H5_USE_16_API)
    hid_t      ref_type = H5T_STD_REF; /* Reference datatype */
    H5R_ref_t  wdata[DIM0];            /* buffer to write to disk */
    H5R_ref_t *rdata = NULL;           /* buffer to read into*/
    H5R_type_t objtype;                /* Reference type */
#else
    hid_t            ref_type = H5T_STD_REF_DSETREG; /* Reference datatype */
    hdset_reg_ref_t  wdata[DIM0];                    /* Write buffer */
    hdset_reg_ref_t *rdata = NULL;                   /* Read buffer */
    H5O_type_t       objtype;
#endif

    /*
     * Create a new file using the default properties.
     */
    file = H5Fcreate(FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    if (file < 0)
        goto done;

    /*
     * Create a dataset with character data.
     */
    space = H5Screate_simple(2, dims2, NULL);
    dset2 = H5Dcreate(file, DATASET2, H5T_STD_I8LE, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if (dset2 < 0)
        goto done;
    status = H5Dwrite(dset2, H5T_NATIVE_CHAR, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata2);

    /*
     * Create reference to a list of elements in dset2.
     */
    status = H5Sselect_elements(space, H5S_SELECT_SET, 4, coords[0]);

#if H5_VERSION_GE(1, 12, 0) && !defined(H5_USE_110_API) && !defined(H5_USE_18_API) && !defined(H5_USE_16_API)
    status = H5Rcreate_region(file, DATASET2, space, H5P_DEFAULT, &wdata[0]);
#else
    status = H5Rcreate(&wdata[0], file, DATASET2, H5R_DATASET_REGION, space);
#endif
    if (status < 0)
        goto done;

    /*
     * Create reference to a hyperslab in dset2, close dataspace.
     */
    status = H5Sselect_hyperslab(space, H5S_SELECT_SET, start, stride, count, block);

#if H5_VERSION_GE(1, 12, 0) && !defined(H5_USE_110_API) && !defined(H5_USE_18_API) && !defined(H5_USE_16_API)
    status = H5Rcreate_region(file, DATASET2, space, H5P_DEFAULT, &wdata[1]);
#else
    status = H5Rcreate(&wdata[1], file, DATASET2, H5R_DATASET_REGION, space);
#endif
    if (status < 0)
        goto done;

    status = H5Sclose(space);

    /*
     * Create dataspace.  Setting maximum size to NULL sets the maximum
     * size to be the current size.
     */
    space = H5Screate_simple(1, dims, NULL);

    /*
     * Create the dataset and write the region references to it.
     */
    dset = H5Dcreate(file, DATASET, ref_type, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if (dset < 0)
        goto done;
    status = H5Dwrite(dset, ref_type, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata);

    /*
     * Close and release resources.
     */
#if H5_VERSION_GE(1, 12, 0) && !defined(H5_USE_110_API) && !defined(H5_USE_18_API) && !defined(H5_USE_16_API)
    status = H5Rdestroy(&wdata[0]);
    status = H5Rdestroy(&wdata[1]);
#endif
    status = H5Dclose(dset);
    status = H5Dclose(dset2);
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
    if (file < 0)
        goto done;

    dset = H5Dopen(file, DATASET, H5P_DEFAULT);
    if (dset < 0)
        goto done;

    /*
     * Get dataspace and allocate memory for read buffer.
     */
    space = H5Dget_space(dset);
    ndims = H5Sget_simple_extent_dims(space, dims, NULL);

#if H5_VERSION_GE(1, 12, 0) && !defined(H5_USE_110_API) && !defined(H5_USE_18_API) && !defined(H5_USE_16_API)
    rdata = (H5R_ref_t *)malloc(dims[0] * sizeof(H5R_ref_t));
#else
    rdata  = (hdset_reg_ref_t *)malloc(dims[0] * sizeof(hdset_reg_ref_t));
#endif

    status = H5Sclose(space);

    /*
     * Read the data.
     */
    status = H5Dread(dset, ref_type, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata);

    /*
     * Output the data to the screen.
     */
    for (i = 0; i < dims[0]; i++) {
        printf("%s[%" PRIuHSIZE "]:\n  ->", DATASET, i);

        /*
         * Open the referenced object, retrieve its region as a
         * dataspace selection.
         */
#if H5_VERSION_GE(1, 10, 0) && !defined(H5_USE_18_API) && !defined(H5_USE_16_API)
#if H5_VERSION_GE(1, 12, 0) && !defined(H5_USE_110_API) && !defined(H5_USE_18_API) && !defined(H5_USE_16_API)
        dset2 = H5Ropen_object(&rdata[i], H5P_DEFAULT, H5P_DEFAULT);
        space = H5Ropen_region(&rdata[i], H5P_DEFAULT, H5P_DEFAULT);
#else
        dset2 = H5Rdereference(dset, H5P_DEFAULT, H5R_DATASET_REGION, &rdata[i]);
        space = H5Rget_region(dset, H5R_DATASET_REGION, &rdata[i]);
#endif
#else
        dset2 = H5Rdereference(dset, H5R_DATASET_REGION, &rdata[i]);
        space = H5Rget_region(dset, H5R_DATASET_REGION, &rdata[i]);
#endif
        if (dset2 < 0)
            goto done;

        /*
         * Get the length of the object's name, allocate space, then
         * retrieve the name.
         */
        size = 1 + H5Iget_name(dset2, NULL, 0);
        name = (char *)malloc(size);
        size = H5Iget_name(dset2, name, size);

        /*
         * Allocate space for the read buffer.  We will only allocate
         * enough space for the selection, plus a null terminator.  The
         * read buffer will be 1-dimensional.
         */
        npoints = H5Sget_select_npoints(space);
        rdata2  = (char *)malloc(npoints + 1);

        /*
         * Read the dataset region, and add a null terminator so we can
         * print it as a string.
         */
        memspace        = H5Screate_simple(1, (hsize_t *)&npoints, NULL);
        status          = H5Dread(dset2, H5T_NATIVE_CHAR, memspace, space, H5P_DEFAULT, rdata2);
        rdata2[npoints] = '\0';

        /*
         * Print the name and region data, close and release resources.
         */
        printf(" %s: %s\n", name, rdata2);
        free(rdata2);
        free(name);

#if H5_VERSION_GE(1, 12, 0) && !defined(H5_USE_110_API) && !defined(H5_USE_18_API) && !defined(H5_USE_16_API)
        status = H5Rdestroy(&rdata[i]);
#endif

        status = H5Sclose(space);
        status = H5Sclose(memspace);
        status = H5Dclose(dset2);
    }

    /*
     * Close and release resources.
     */
    free(rdata);
    status = H5Dclose(dset);
    status = H5Fclose(file);

    return 0;

done:
    return 1;
}
