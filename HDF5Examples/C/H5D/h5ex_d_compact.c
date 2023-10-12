/************************************************************

  This example shows how to read and write data to a compact
  dataset.  The program first writes integers to a compact
  dataset with dataspace dimensions of DIM0xDIM1, then
  closes the file.  Next, it reopens the file, reads back
  the data, and outputs it to the screen.

 ************************************************************/

#include "hdf5.h"
#include <stdio.h>
#include <stdlib.h>

#define FILE    "h5ex_d_compact.h5"
#define DATASET "DS1"
#define DIM0    4
#define DIM1    7

int
main(void)
{
    hid_t        file  = H5I_INVALID_HID;
    hid_t        space = H5I_INVALID_HID;
    hid_t        dset  = H5I_INVALID_HID;
    hid_t        dcpl  = H5I_INVALID_HID;
    herr_t       status;
    H5D_layout_t layout;
    hsize_t      dims[2] = {DIM0, DIM1};
    int          wdata[DIM0][DIM1]; /* Write buffer */
    int          rdata[DIM0][DIM1]; /* Read buffer */
    hsize_t      i, j;

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
     * Create the dataset creation property list, set the layout to
     * compact.
     */
    dcpl   = H5Pcreate(H5P_DATASET_CREATE);
    status = H5Pset_layout(dcpl, H5D_COMPACT);

    /*
     * Create the dataset.  We will use all default properties for this
     * example.
     */
    dset = H5Dcreate(file, DATASET, H5T_STD_I32LE, space, H5P_DEFAULT, dcpl, H5P_DEFAULT);

    /*
     * Write the data to the dataset.
     */
    status = H5Dwrite(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata[0]);

    /*
     * Close and release resources.
     */
    status = H5Pclose(dcpl);
    status = H5Dclose(dset);
    status = H5Sclose(space);
    status = H5Fclose(file);

    /*
     * Now we begin the read section of this example.
     */

    /*
     * Open file and dataset using the default properties.
     */
    file = H5Fopen(FILE, H5F_ACC_RDONLY, H5P_DEFAULT);
    dset = H5Dopen(file, DATASET, H5P_DEFAULT);

    /*
     * Retrieve the dataset creation property list, and print the
     * storage layout.
     */
    dcpl   = H5Dget_create_plist(dset);
    layout = H5Pget_layout(dcpl);
    printf("Storage layout for %s is: ", DATASET);
    switch (layout) {
        case H5D_COMPACT:
            printf("H5D_COMPACT\n");
            break;
        case H5D_CONTIGUOUS:
            printf("H5D_CONTIGUOUS\n");
            break;
        case H5D_CHUNKED:
            printf("H5D_CHUNKED\n");
            break;
#if H5_VERSION_GE(1, 10, 0) && !defined(H5_USE_18_API) && !defined(H5_USE_16_API)
        case H5D_VIRTUAL:
            printf("H5D_VIRTUAL\n");
            break;
#endif
        case H5D_LAYOUT_ERROR:
        case H5D_NLAYOUTS:
            printf("H5D_LAYOUT_ERROR\n");
    }

    /*
     * Read the data using the default properties.
     */
    status = H5Dread(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata[0]);

    /*
     * Output the data to the screen.
     */
    printf("%s:\n", DATASET);
    for (i = 0; i < DIM0; i++) {
        printf(" [");
        for (j = 0; j < DIM1; j++)
            printf(" %3d", rdata[i][j]);
        printf("]\n");
    }

    /*
     * Close and release resources.
     */
    status = H5Pclose(dcpl);
    status = H5Dclose(dset);
    status = H5Fclose(file);

    return 0;
}
