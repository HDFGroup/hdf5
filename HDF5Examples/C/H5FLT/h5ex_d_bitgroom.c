/************************************************************

  This example shows how to write data and read it from a dataset
  using BitGroom quantization.
  The BitGroom filter is not available in HDF5.
  The example uses a new feature available in HDF5 version 1.8.11
  to discover, load and register filters at run time.

 ************************************************************/

#include "hdf5.h"
#include <stdio.h>
#include <stdlib.h>

#define FILENAME            "h5ex_d_bitgroom.h5"
#define DATASET             "DS1"
#define DIM0                32
#define DIM1                64
#define CHUNK0              4
#define CHUNK1              8
#define H5Z_FILTER_BITGROOM 32022

int
main(void)
{
    hid_t              file_id  = H5I_INVALID_HID;
    hid_t              space_id = H5I_INVALID_HID;
    hid_t              dset_id  = H5I_INVALID_HID;
    hid_t              dcpl_id  = H5I_INVALID_HID;
    herr_t             status;
    htri_t             avail;
    H5Z_filter_t       filter_id = 0;
    char               filter_name[80];
    hsize_t            dims[2] = {DIM0, DIM1}, chunk[2] = {CHUNK0, CHUNK1};
    size_t             nelmts = 5; /* number of elements in cd_values */
    unsigned int       flags;
    unsigned           filter_config;
    const unsigned int cd_values[5] = {
        3, 4, 0, 0, 0}; /* BitGroom argument ordering is
                           NSD,sizeof(data),has_mss_val,mss_val_byt_1to4[,mss_val_byt_5to8] */
    unsigned int values_out[5] = {99, 99, 99, 99, 99};
    float        wdata[DIM0][DIM1]; /* Write buffer */
    float        rdata[DIM0][DIM1]; /* Read buffer */
    float        max;
    hsize_t      i, j;
    int          ret_value = 1;

    /*
     * Initialize data.
     */
    for (i = 0; i < DIM0; i++)
        for (j = 0; j < DIM1; j++)
            wdata[i][j] = (float)(i * j) - (float)(j);

    /*
     * Create a new file using the default properties.
     */
    file_id = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    if (file_id < 0)
        goto done;

    /*
     * Create dataspace.  Setting maximum size to NULL sets the maximum
     * size to be the current size.
     */
    space_id = H5Screate_simple(2, dims, NULL);
    if (space_id < 0)
        goto done;

    /*
     * Create the dataset creation property list, add the BitGroom
     * quantization filter and set the chunk size.
     */
    dcpl_id = H5Pcreate(H5P_DATASET_CREATE);
    if (dcpl_id < 0)
        goto done;

    /* 20200929: csz change this to H5Z_FLAG_OPTIONAL, so that can_apply() can reject filter for
       integers without causing program to exit()? */
    status = H5Pset_filter(dcpl_id, H5Z_FILTER_BITGROOM, H5Z_FLAG_MANDATORY, nelmts, cd_values);
    if (status < 0)
        goto done;

    /*
     * Check that filter is registered with the library now.
     * If it is registered, retrieve filter's configuration.
     */
    avail = H5Zfilter_avail(H5Z_FILTER_BITGROOM);
    if (avail) {
        status = H5Zget_filter_info(H5Z_FILTER_BITGROOM, &filter_config);
        if ((filter_config & H5Z_FILTER_CONFIG_ENCODE_ENABLED) &&
            (filter_config & H5Z_FILTER_CONFIG_DECODE_ENABLED))
            printf("BitGroom filter is available for quantization and decoding.\n");
    }
    else {
        printf("H5Zfilter_avail - not found.\n");
        goto done;
    }
    status = H5Pset_chunk(dcpl_id, 2, chunk);
    if (status < 0)
        printf("failed to set chunk.\n");

    /*
     * Create the dataset.
     */
    printf("....Create dataset ................\n");
    dset_id = H5Dcreate(file_id, DATASET, H5T_IEEE_F32LE, space_id, H5P_DEFAULT, dcpl_id, H5P_DEFAULT);
    if (dset_id < 0) {
        printf("failed to create dataset.\n");
        goto done;
    }

    /*
     * Write the data to the dataset.
     */
    printf("....Writing BitGroom-quantized data ................\n");
    status = H5Dwrite(dset_id, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, (void *)wdata);
    if (status < 0)
        printf("failed to write data.\n");

    /*
     * Close and release resources.
     */
    H5Dclose(dset_id);
    dset_id = -1;
    H5Pclose(dcpl_id);
    dcpl_id = -1;
    H5Sclose(space_id);
    space_id = -1;
    H5Fclose(file_id);
    file_id = -1;
    status  = H5close();
    if (status < 0) {
        printf("\nFAILED to close library\n");
        goto done;
    }

    printf("....Close the file and reopen for reading ........\n");
    /*
     * Now we begin the read section of this example.
     */

    /*
     * Open file and dataset using the default properties.
     */
    file_id = H5Fopen(FILENAME, H5F_ACC_RDONLY, H5P_DEFAULT);
    if (file_id < 0)
        goto done;

    dset_id = H5Dopen(file_id, DATASET, H5P_DEFAULT);
    if (dset_id < 0)
        goto done;

    /*
     * Retrieve dataset creation property list.
     */
    dcpl_id = H5Dget_create_plist(dset_id);
    if (dcpl_id < 0)
        goto done;

    /*
     * Retrieve and print the filter id, quantization level and filter's name for BitGroom.
     */
    filter_id = H5Pget_filter2(dcpl_id, (unsigned)0, &flags, &nelmts, values_out, sizeof(filter_name),
                               filter_name, NULL);
    printf("Filter info is available from the dataset creation property\n");
    printf("   Filter identifier is ");
    switch (filter_id) {
        case H5Z_FILTER_BITGROOM:
            printf("%d\n", filter_id);
            printf("   Number of parameters is %lu with the values %u, %u, %u, %u, %u\n", nelmts,
                   values_out[0], values_out[1], values_out[2], values_out[3], values_out[4]);
            printf("   To find more about the filter check %s\n", filter_name);
            break;
        default:
            printf("Not expected filter\n");
            break;
    }

    /*
     * Read the data using the default properties.
     */
    printf("....Reading BitGroom-quantized data ................\n");
    status = H5Dread(dset_id, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata[0]);
    if (status < 0)
        printf("failed to read data.\n");

    /*
     * Find the maximum value in the dataset, to verify that it was
     * read correctly.
     */
    max = rdata[0][0];
    for (i = 0; i < DIM0; i++)
        for (j = 0; j < DIM1; j++) {
            /*printf("%f \n", rdata[i][j]); */
            if (max < rdata[i][j])
                max = rdata[i][j];
        }
    /*
     * Print the maximum value.
     */
    printf("Maximum value in %s is %g\n", DATASET, max);
    /*
     * Check that filter is registered with the library now.
     */
    avail = H5Zfilter_avail(H5Z_FILTER_BITGROOM);
    if (avail)
        printf("BitGroom filter is available now since H5Dread triggered loading of the filter.\n");

    ret_value = 0;

done:
    /*
     * Close and release resources.
     */
    if (dcpl_id >= 0)
        H5Pclose(dcpl_id);
    if (dset_id >= 0)
        H5Dclose(dset_id);
    if (space_id >= 0)
        H5Sclose(space_id);
    if (file_id >= 0)
        H5Fclose(file_id);

    return ret_value;
}
