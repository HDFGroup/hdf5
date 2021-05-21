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
  Percival use case. Every fifth 10x10 plane in VDS is stored in
  the corresponding 3D unlimited dataset.
  There are 4 source datasets total.
  This file is intended for use with HDF5 Library version 1.10

 ************************************************************/
/* EIP Add link to the picture */

#include "hdf5.h"
#include <stdio.h>
#include <stdlib.h>

#define VFILE   "vds-percival-unlim.h5"
#define DATASET "VDS-Percival-unlim"
#define VDSDIM0 H5S_UNLIMITED
#define VDSDIM1 10
#define VDSDIM2 10

#define DIM0         H5S_UNLIMITED
#define DIM0_1       10 /* Initial size of the datasets */
#define DIM1         10
#define DIM2         10
#define RANK         3
#define PLANE_STRIDE 4

const char *SRC_FILE[] = {"a.h5", "b.h5", "c.h5", "d.h5"};

const char *SRC_DATASET[] = {"A", "B", "C", "D"};

int
main(void)
{
    hid_t   vfile, file, src_space, mem_space, vspace, vdset, dset; /* Handles */
    hid_t   dcpl;
    herr_t  status;
    hsize_t vdsdims[3] = {4 * DIM0_1, VDSDIM1, VDSDIM2}, vdsdims_max[3] = {VDSDIM0, VDSDIM1, VDSDIM2},
            dims[3] = {DIM0_1, DIM1, DIM2}, extdims[3] = {2 * DIM0_1, DIM1, DIM2},
            chunk_dims[3] = {DIM0_1, DIM1, DIM2}, dims_max[3] = {DIM0, DIM1, DIM2}, vdsdims_out[3],
            vdsdims_max_out[3], start[3], /* Hyperslab parameters */
        stride[3], count[3], src_count[3], block[3];
    hsize_t start_out[3], /* Hyperslab parameter out */
        stride_out[3], count_out[3], block_out[3];
    int          i, j, k;
    H5D_layout_t layout;  /* Storage layout */
    size_t       num_map; /* Number of mappings */
    ssize_t      len;     /* Length of the string; also a return value */
    char *       filename;
    char *       dsetname;
    int          wdata[DIM0_1 * DIM1 * DIM2];
    int          rdata[80][10][10];
    int          a_rdata[20][10][10];

    /*
     * Create source files and datasets. This step is optional.
     */
    for (i = 0; i < PLANE_STRIDE; i++) {
        /*
         * Initialize data for i-th source dataset.
         */
        for (j = 0; j < DIM0_1 * DIM1 * DIM2; j++)
            wdata[j] = i + 1;

        /*
         * Create the source files and  datasets. Write data to each dataset and
         * close all resources.
         */

        file      = H5Fcreate(SRC_FILE[i], H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
        src_space = H5Screate_simple(RANK, dims, dims_max);
        dcpl      = H5Pcreate(H5P_DATASET_CREATE);
        status    = H5Pset_chunk(dcpl, RANK, chunk_dims);
        dset   = H5Dcreate2(file, SRC_DATASET[i], H5T_NATIVE_INT, src_space, H5P_DEFAULT, dcpl, H5P_DEFAULT);
        status = H5Dwrite(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata);
        status = H5Sclose(src_space);
        status = H5Pclose(dcpl);
        status = H5Dclose(dset);
        status = H5Fclose(file);
    }

    vfile = H5Fcreate(VFILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /* Create VDS dataspace.  */
    vspace = H5Screate_simple(RANK, vdsdims, vdsdims_max);

    /* Create dataspaces for the source dataset. */
    src_space = H5Screate_simple(RANK, dims, dims_max);

    /* Create VDS creation property */
    dcpl = H5Pcreate(H5P_DATASET_CREATE);

    /* Initialize hyperslab values */

    start[0]     = 0;
    start[1]     = 0;
    start[2]     = 0;
    stride[0]    = PLANE_STRIDE; /* we will select every fifth plane in VDS */
    stride[1]    = 1;
    stride[2]    = 1;
    count[0]     = H5S_UNLIMITED;
    count[1]     = 1;
    count[2]     = 1;
    src_count[0] = H5S_UNLIMITED;
    src_count[1] = 1;
    src_count[2] = 1;
    block[0]     = 1;
    block[1]     = DIM1;
    block[2]     = DIM2;

    /*
     * Build the mappings
     *
     */
    status = H5Sselect_hyperslab(src_space, H5S_SELECT_SET, start, NULL, src_count, block);
    for (i = 0; i < PLANE_STRIDE; i++) {
        status = H5Sselect_hyperslab(vspace, H5S_SELECT_SET, start, stride, count, block);
        status = H5Pset_virtual(dcpl, vspace, SRC_FILE[i], SRC_DATASET[i], src_space);
        start[0]++;
    }

    H5Sselect_none(vspace);

    /* Create a virtual dataset */
    vdset  = H5Dcreate2(vfile, DATASET, H5T_NATIVE_INT, vspace, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    status = H5Sclose(vspace);
    status = H5Sclose(src_space);
    status = H5Pclose(dcpl);
    /* Let's get space of the VDS and its dimension; we should get 40x10x10 */
    vspace = H5Dget_space(vdset);
    H5Sget_simple_extent_dims(vspace, vdsdims_out, vdsdims_max_out);
    printf("VDS dimensions first time \n");
    printf(" Current: ");
    for (i = 0; i < RANK; i++)
        printf(" %d ", (int)vdsdims_out[i]);
    printf("\n");

    /* Let's add data to the source datasets and check new dimensions for VDS */

    for (i = 0; i < PLANE_STRIDE; i++) {
        /*
         * Initialize data for i-th source dataset.
         */
        for (j = 0; j < DIM0_1 * DIM1 * DIM2; j++)
            wdata[j] = 10 * (i + 1);

        /*
         * Create the source files and  datasets. Write data to each dataset and
         * close all resources.
         */

        file      = H5Fopen(SRC_FILE[i], H5F_ACC_RDWR, H5P_DEFAULT);
        dset      = H5Dopen2(file, SRC_DATASET[i], H5P_DEFAULT);
        status    = H5Dset_extent(dset, extdims);
        src_space = H5Dget_space(dset);
        start[0]  = DIM0_1;
        start[1]  = 0;
        start[2]  = 0;
        count[0]  = 1;
        count[1]  = 1;
        count[2]  = 1;
        block[0]  = DIM0_1;
        block[1]  = DIM1;
        block[2]  = DIM2;

        mem_space = H5Screate_simple(RANK, dims, NULL);
        status    = H5Sselect_hyperslab(src_space, H5S_SELECT_SET, start, NULL, count, block);
        status    = H5Dwrite(dset, H5T_NATIVE_INT, mem_space, src_space, H5P_DEFAULT, wdata);
        status    = H5Sclose(src_space);
        status    = H5Dclose(dset);
        status    = H5Fclose(file);
    }

    status = H5Dclose(vdset);
    status = H5Fclose(vfile);

    /*
     * Now we begin the read section of this example.
     */

    /*
     * Open file and dataset using the default properties.
     */
    vfile = H5Fopen(VFILE, H5F_ACC_RDONLY, H5P_DEFAULT);
    vdset = H5Dopen2(vfile, DATASET, H5P_DEFAULT);

    /*
     * Get creation property list and mapping properties.
     */
    dcpl = H5Dget_create_plist(vdset);

    /*
     * Get storage layout.
     */
    layout = H5Pget_layout(dcpl);

    if (H5D_VIRTUAL == layout)
        printf(" Dataset has a virtual layout \n");
    else
        printf(" Wrong layout found \n");

    /*
     * Find number of mappings.
     */
    status = H5Pget_virtual_count(dcpl, &num_map);
    printf(" Number of mappings is %lu\n", (unsigned long)num_map);

    /*
     * Get mapping parameters for each mapping.
     */
    for (i = 0; i < (int)num_map; i++) {
        printf(" Mapping %d \n", i);
        printf("         Selection in the virtual dataset \n");
        /* Get selection in the virttual  dataset */
        vspace = H5Pget_virtual_vspace(dcpl, (size_t)i);
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
            if (H5Sis_regular_hyperslab(src_space)) {
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
     * Read data from VDS.
     */
    vspace = H5Dget_space(vdset);
    H5Sget_simple_extent_dims(vspace, vdsdims_out, vdsdims_max_out);
    printf("VDS dimensions second time \n");
    printf(" Current: ");
    for (i = 0; i < RANK; i++)
        printf(" %d ", (int)vdsdims_out[i]);
    printf("\n");

    /* Read all VDS data */

    /* EIP We should be able to do it by using H5S_ALL instead of making selection
     * or using H5Sselect_all from vspace.
     */
    start[0] = 0;
    start[1] = 0;
    start[2] = 0;
    count[0] = 1;
    count[1] = 1;
    count[2] = 1;
    block[0] = vdsdims_out[0];
    block[1] = vdsdims_out[1];
    block[2] = vdsdims_out[2];

    status    = H5Sselect_hyperslab(vspace, H5S_SELECT_SET, start, NULL, count, block);
    mem_space = H5Screate_simple(RANK, vdsdims_out, NULL);
    status    = H5Dread(vdset, H5T_NATIVE_INT, mem_space, vspace, H5P_DEFAULT, rdata);
    printf(" All data: \n");
    for (i = 0; i < (int)vdsdims_out[0]; i++) {
        for (j = 0; j < (int)vdsdims_out[1]; j++) {
            printf("(%d, %d, 0)", i, j);
            for (k = 0; k < (int)vdsdims_out[2]; k++)
                printf(" %d ", rdata[i][j][k]);
            printf("\n");
        }
    }
    /* Read VDS, but only data mapeed to dataset a.h5 */
    start[0]  = 0;
    start[1]  = 0;
    start[2]  = 0;
    stride[0] = PLANE_STRIDE;
    stride[1] = 1;
    stride[2] = 1;
    count[0]  = 2 * DIM0_1;
    count[1]  = 1;
    count[2]  = 1;
    block[0]  = 1;
    block[1]  = vdsdims_out[1];
    block[2]  = vdsdims_out[2];
    dims[0]   = 2 * DIM0_1;
    status    = H5Sselect_hyperslab(vspace, H5S_SELECT_SET, start, stride, count, block);
    mem_space = H5Screate_simple(RANK, dims, NULL);
    status    = H5Dread(vdset, H5T_NATIVE_INT, mem_space, vspace, H5P_DEFAULT, a_rdata);
    printf(" All data: \n");
    for (i = 0; i < 2 * DIM0_1; i++) {
        for (j = 0; j < (int)vdsdims_out[1]; j++) {
            printf("(%d, %d, 0)", i, j);
            for (k = 0; k < (int)vdsdims_out[2]; k++)
                printf(" %d ", a_rdata[i][j][k]);
            printf("\n");
        }
    }
    /*
     * Close and release resources.
     */
    status = H5Sclose(mem_space);
    status = H5Pclose(dcpl);
    status = H5Dclose(vdset);
    status = H5Fclose(vfile);
    return 0;
}
