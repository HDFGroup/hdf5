/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/* This source code was developed under the Mochi project
 * (https://www.mcs.anl.gov/research/projects/mochi), supported by the U.S.
 * Department of Energy, Office of Science, under contract DE-AC02-06CH11357.
 */

#include "h5rados_example.h"
#include <time.h>

int main(int argc, char *argv[]) {
    rados_t cluster;
    char *pool = "mypool";
    hid_t file = -1, dset = -1, file_space = -1, mem_space = -1, fapl = -1;
    hsize_t dims[2] = {4, 6};
    hsize_t mstart[2], count[2], fstart[2];
    int buf[4][6];
    int i, j;

    (void)MPI_Init(&argc, &argv);

    /* Seed random number generator */
    srand(time(NULL));

    if(argc != 9)
        PRINTF_ERROR("argc != 9\n");

    mstart[0] = (hsize_t)atoi(argv[3]);
    mstart[1] = (hsize_t)atoi(argv[4]);
    count[0] = (hsize_t)atoi(argv[5]);
    count[1] = (hsize_t)atoi(argv[6]);
    fstart[0] = (hsize_t)atoi(argv[7]);
    fstart[1] = (hsize_t)atoi(argv[8]);

    if(rados_create(&cluster, NULL) < 0)
        ERROR;
    if(rados_conf_read_file(cluster, "ceph.conf") < 0)
        ERROR;

    /* Initialize VOL */
    if(H5VLrados_init(cluster, pool) < 0)
        ERROR;

    /* Set up FAPL */
    if((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        ERROR;
    if(H5Pset_fapl_rados(fapl, MPI_COMM_WORLD, MPI_INFO_NULL) < 0)
        ERROR;
    if(H5Pset_all_coll_metadata_ops(fapl, true) < 0)
        ERROR;

    /* Open file */
    if((file = H5Fopen(argv[1], H5F_ACC_RDWR, fapl)) < 0)
        ERROR;

    /* Open dataset */
    if((dset = H5Dopen2(file, argv[2], H5P_DEFAULT)) < 0)
        ERROR;

    printf("Selecting elements denoted with X\n");
    printf("Memory:\n");
    for(i = 0; i < 4; i++) {
        if((i >= mstart[0]) && (i < mstart[0] + count[0])) {
            for(j = 0; j < 6; j++)
                if((j >= mstart[1]) && (j < mstart[1] + count[1]))
                    printf("X");
                else
                    printf(".");
            printf("\n");
        }
        else
            printf("......\n");
    }
    printf("\nFile:\n");
    for(i = 0; i < 4; i++) {
        if((i >= fstart[0]) && (i < fstart[0] + count[0])) {
            for(j = 0; j < 6; j++)
                if((j >= fstart[1]) && (j < fstart[1] + count[1]))
                    printf("X");
                else
                    printf(".");
            printf("\n");
        }
        else
            printf("......\n");
    }

    /* Fill and print buffer */
    printf("Writing data. Buffer is:\n");
    for(i = 0; i < 4; i++) {
        for(j = 0; j < 6; j++) {
            buf[i][j] = rand() % 10;
            printf("%d ", buf[i][j]);
        }
        printf("\n");
    }


    /* Set up dataspaces */
    if((file_space = H5Screate_simple(2, dims, NULL)) < 0)
        ERROR;
    if((mem_space = H5Screate_simple(2, dims, NULL)) < 0)
        ERROR;
    if(H5Sselect_hyperslab(file_space, H5S_SELECT_SET, fstart, NULL, count, NULL) < 0)
        ERROR;
    if(H5Sselect_hyperslab(mem_space, H5S_SELECT_SET, mstart, NULL, count, NULL) < 0)
        ERROR;

    /* Write data */
    if(H5Dwrite(dset, H5T_NATIVE_INT, mem_space, file_space, H5P_DEFAULT, buf) < 0)
        ERROR;

    /* Close */
    if(H5Dclose(dset) < 0)
        ERROR;
    if(H5Fclose(file) < 0)
        ERROR;
    if(H5Sclose(file_space) < 0)
        ERROR;
    if(H5Sclose(mem_space) < 0)
        ERROR;
    if(H5Pclose(fapl) < 0)
        ERROR;

    printf("Success\n");

    (void)MPI_Finalize();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Dclose(dset);
        H5Fclose(file);
        H5Sclose(file_space);
        H5Sclose(mem_space);
        H5Pclose(fapl);
    } H5E_END_TRY;

    (void)MPI_Finalize();
    return 1;
}

