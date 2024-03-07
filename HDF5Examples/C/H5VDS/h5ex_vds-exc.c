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
/************************************************************

  This example illustrates the concept of the virtual dataset.
  Excalibur use case with k=2 and m=3.
  This file is intended for use with HDF5 Library version 1.10

 ************************************************************/

#include "hdf5.h"
#include <stdio.h>
#include <stdlib.h>

#define FILE    "h5ex_vds-exc.h5"
#define DATASET "VDS-Excalibur"
#define VDSDIM0 0
#define VDSDIM1 15
#define VDSDIM2 6
#define LDIM0   0
#define LDIM1   2
#define LDIM2   6
#define NDIM0   0
#define NDIM1   3
#define NDIM2   6
#define RANK    3

const char *SRC_FILE[] = {"ae.h5", "be.h5", "ce.h5", "de.h5", "ee.h5", "fe.h5"};

const char *SRC_DATASET[] = {"A", "B", "C", "D", "E", "F"};

int
main(void)
{
    hid_t        file       = H5I_INVALID_HID;
    hid_t        space      = H5I_INVALID_HID;
    hid_t        dset       = H5I_INVALID_HID;
    hid_t        src_space  = H5I_INVALID_HID;
    hid_t        lsrc_space = H5I_INVALID_HID;
    hid_t        nsrc_space = H5I_INVALID_HID;
    hid_t        vspace     = H5I_INVALID_HID;
    hid_t        dcpl       = H5I_INVALID_HID;
    herr_t       status;
    hsize_t      vdsdims[3]     = {VDSDIM0, VDSDIM1, VDSDIM2};
    hsize_t      vdsdims_max[3] = {H5S_UNLIMITED, VDSDIM1, VDSDIM2};
    hsize_t      ldims[3]       = {LDIM0, LDIM1, LDIM2};
    hsize_t      ldims_max[3]   = {H5S_UNLIMITED, LDIM1, LDIM2};
    hsize_t      ndims[3]       = {NDIM0, NDIM1, NDIM2};
    hsize_t      ndims_max[3]   = {H5S_UNLIMITED, NDIM1, NDIM2};
    hsize_t      start[3], count[3], block[3]; /* Hyperslab parameters */
    hsize_t      start_out[3], stride_out[3], count_out[3], block_out[3];
    int          rdata[40][VDSDIM1][VDSDIM2]; /* Read buffer for virtual dataset */
    int          l = 2;
    int          n = 3;
    int          i, j, k;
    H5D_layout_t layout;  /* Storage layout */
    size_t       num_map; /* Number of mappings */
    ssize_t      len;     /* Length of the string; also a return value */
    char        *filename = NULL;
    char        *dsetname = NULL;

    file = H5Fcreate(FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /* Create VDS dataspace.  */
    space = H5Screate_simple(RANK, vdsdims, vdsdims_max);
    /* Create dataspaces for A, C, and E datasets. */
    lsrc_space = H5Screate_simple(RANK, ldims, ldims_max);
    /* Create dataspaces for B, D, and F datasets. */
    nsrc_space = H5Screate_simple(RANK, ndims, ndims_max);

    /* Create VDS creation property */
    dcpl = H5Pcreate(H5P_DATASET_CREATE);

    /* Initialize hyperslab values */

    start[0] = 0;
    start[1] = 0;
    start[2] = 0;
    count[0] = H5S_UNLIMITED;
    count[1] = 1;
    count[2] = 1;
    block[0] = 1;
    block[1] = l;
    block[2] = VDSDIM2;

    /*
     * Build the mappings for A, C and E source datasets.
     * Unlimited hyperslab selection is the same in the source datasets.
     * Unlimited hyperslab selections in the virtual dataset have different offsets.
     */
    status = H5Sselect_hyperslab(lsrc_space, H5S_SELECT_SET, start, NULL, count, block);
    for (i = 0; i < 3; i++) {
        start[1] = (hsize_t)((l + n) * i);
        status   = H5Sselect_hyperslab(space, H5S_SELECT_SET, start, NULL, count, block);
        status   = H5Pset_virtual(dcpl, space, SRC_FILE[2 * i], SRC_DATASET[2 * i], lsrc_space);
    }

    /* Reinitialize start[1] and block[1] to build the second set of mappings. */
    start[1] = 0;
    block[1] = n;
    /*
     * Build the mappings for B, D and F source datasets.
     * Unlimited hyperslab selection is the same in the source datasets.
     * Unlimited hyperslab selections in the virtual dataset have different offsets.
     */
    status = H5Sselect_hyperslab(nsrc_space, H5S_SELECT_SET, start, NULL, count, block);
    for (i = 0; i < 3; i++) {
        start[1] = (hsize_t)(l + (l + n) * i);
        status   = H5Sselect_hyperslab(space, H5S_SELECT_SET, start, NULL, count, block);
        status   = H5Pset_virtual(dcpl, space, SRC_FILE[2 * i + 1], SRC_DATASET[2 * i + 1], nsrc_space);
    }

    /* Create a virtual dataset */
    dset   = H5Dcreate2(file, DATASET, H5T_STD_I32LE, space, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    status = H5Sclose(space);
    status = H5Sclose(nsrc_space);
    status = H5Sclose(lsrc_space);
    status = H5Dclose(dset);
    status = H5Fclose(file);

    /*
     * Now we begin the read section of this example.
     */

    /*
     * Open file and dataset using the default properties.
     */
    file = H5Fopen(FILE, H5F_ACC_RDONLY, H5P_DEFAULT);
    dset = H5Dopen2(file, DATASET, H5P_DEFAULT);

    /*
     * Get creation property list and mapping properties.
     */
    dcpl = H5Dget_create_plist(dset);

    /*
     * Get storage layout.
     */
    layout = H5Pget_layout(dcpl);
    if (H5D_VIRTUAL == layout)
        printf(" Dataset has a virtual layout \n");
    else
        printf("Wrong layout found \n");

    /*
     * Find the number of mappings.
     */
    status = H5Pget_virtual_count(dcpl, &num_map);
    printf(" Number of mappings is %lu\n", (unsigned long)num_map);

    /*
     * Get mapping parameters for each mapping.
     */
    for (i = 0; i < (int)num_map; i++) {
        printf(" Mapping %d \n", i);
        printf("         Selection in the virtual dataset \n");
        /* Get selection in the virtual dataset */
        vspace = H5Pget_virtual_vspace(dcpl, (size_t)i);

        /* Make sure that this is a hyperslab selection and then print information. */
        if (H5Sget_select_type(vspace) == H5S_SEL_HYPERSLABS) {
            if (H5Sis_regular_hyperslab(vspace)) {
                status = H5Sget_regular_hyperslab(vspace, start_out, stride_out, count_out, block_out);
                printf("         start  = [%llu, %llu, %llu] \n", (unsigned long long)start_out[0],
                       (unsigned long long)start_out[1], (unsigned long long)start_out[2]);
                printf("         stride = [%llu, %llu, %llu] \n", (unsigned long long)stride_out[0],
                       (unsigned long long)stride_out[1], (unsigned long long)stride_out[2]);
                printf("         count  = [%llu, %llu, %llu] \n", (unsigned long long)count_out[0],
                       (unsigned long long)count_out[1], (unsigned long long)count_out[2]);
                printf("         block  = [%llu, %llu, %llu] \n", (unsigned long long)block_out[0],
                       (unsigned long long)block_out[1], (unsigned long long)block_out[2]);
            }
        }
        /* Get source file name */
        len      = H5Pget_virtual_filename(dcpl, (size_t)i, NULL, 0);
        filename = (char *)malloc((size_t)len * sizeof(char) + 1);
        H5Pget_virtual_filename(dcpl, (size_t)i, filename, len + 1);
        printf("         Source filename %s\n", filename);

        /* Get source dataset name */
        len      = H5Pget_virtual_dsetname(dcpl, (size_t)i, NULL, 0);
        dsetname = (char *)malloc((size_t)len * sizeof(char) + 1);
        H5Pget_virtual_dsetname(dcpl, (size_t)i, dsetname, len + 1);
        printf("         Source dataset name %s\n", dsetname);

        /* Get selection in the source dataset */
        printf("         Selection in the source dataset \n");
        src_space = H5Pget_virtual_srcspace(dcpl, (size_t)i);
        if (H5Sget_select_type(src_space) == H5S_SEL_HYPERSLABS) {
            if (H5Sis_regular_hyperslab(vspace)) {
                status = H5Sget_regular_hyperslab(src_space, start_out, stride_out, count_out, block_out);
                printf("         start  = [%llu, %llu, %llu] \n", (unsigned long long)start_out[0],
                       (unsigned long long)start_out[1], (unsigned long long)start_out[2]);
                printf("         stride = [%llu, %llu, %llu] \n", (unsigned long long)stride_out[0],
                       (unsigned long long)stride_out[1], (unsigned long long)stride_out[2]);
                printf("         count  = [%llu, %llu, %llu] \n", (unsigned long long)count_out[0],
                       (unsigned long long)count_out[1], (unsigned long long)count_out[2]);
                printf("         block  = [%llu, %llu, %llu] \n", (unsigned long long)block_out[0],
                       (unsigned long long)block_out[1], (unsigned long long)block_out[2]);
            }
        }
        H5Sclose(vspace);
        H5Sclose(src_space);
        free(filename);
        free(dsetname);
    }

    /*
     * Read the data using the default properties.
     */
    status = H5Dread(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata[0]);

    /*
     * Output the data to the screen.
     */
    printf(" VDS Data:\n");
    for (i = 0; i < VDSDIM0; i++) {
        printf(" [");
        for (j = 0; j < VDSDIM1; j++) {
            printf(" [");
            for (k = 0; k < VDSDIM1; k++)
                printf(" %3d", rdata[i][j][k]);
            printf("]");
        }
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
