/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have     *
 * access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu. *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Flexible Parallel HDF5 test.
 *
 * Author:
 *      Bill Wendling (wendling@ncsa.uiuc.edu)
 *      20. February 2003
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "testphdf5.h"

#ifdef H5_HAVE_FPHDF5

MPI_Comm SAP_Comm = MPI_COMM_NULL;
MPI_Comm SAP_Barrier_Comm = MPI_COMM_NULL;

static void create_file(int);

/* FILENAME and filenames must have the same number of names */
const char *FILENAME[2] = {
    "FPHDF5Test",
    NULL
};

char    filenames[2][PATH_MAX];
int     nerrors = 0;
int     verbose = 0;
hid_t   fapl = -1;

static void create_file(int sap_rank)
{
    const char dset_3x5[] = "/dataset-3x5";
    const char dset_3x5x7[] = "/dataset-3x5x7";
    const char dset_2x4x8[] = "dataset-2x4x8";
    const char dset_5x7x11[] = "dataset-5x7x11";
    const char group_1[] = "/group-1";
    const char group_2[] = "/group-2";

    int mpi_rank, mrc;
    hid_t fid = -1, sid = -1, dataset = -1;

    mrc = MPI_Comm_rank(SAP_Comm, &mpi_rank);
    VRFY((mrc == MPI_SUCCESS), "MPI_Comm_rank");

    /*===-------------------------------------------------------------------===
     * Create the file
     *===-------------------------------------------------------------------===
     */
    printf("%d: Creating file %s\n", mpi_rank, filenames[0]);

    fid = H5Fcreate(filenames[0], H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
    VRFY((fid >= 0), "H5Fcreate");
    printf("%d: Created file %s\n", mpi_rank, filenames[0]);

    /*===-------------------------------------------------------------------===
     * Create datasets
     *===-------------------------------------------------------------------===
     */
    if (mpi_rank == 0) {
        /* Create a dataset in the file */
        hsize_t dims[3];

        /*===---------------------------------------------------------------===
         * Create 3x5 dataset
         *===---------------------------------------------------------------===
         */
        dims[0] = 3;
        dims[1] = 5;

        sid = H5Screate_simple(2, dims, NULL);
        VRFY((sid >= 0), "H5Screate_simple");
        printf("%d: Created simple 3x5\n", mpi_rank);

        dataset = H5Dcreate(fid, dset_3x5, H5T_NATIVE_INT, sid, H5P_DEFAULT);
        VRFY((dataset >= 0), "H5Dcreate");
        printf("%d: Created dataset 3x5 \"%s\"\n", mpi_rank, dset_3x5);

        VRFY((H5Sclose(sid) >= 0), "H5Sclose");
        VRFY((H5Dclose(dataset) >= 0), "H5Dclose");

        /*===---------------------------------------------------------------===
         * Create 3x5x7 dataset
         *===---------------------------------------------------------------===
         */
        dims[0] = 3;
        dims[1] = 5;
        dims[2] = 7;

        sid = H5Screate_simple(3, dims, NULL);
        VRFY((sid >= 0), "H5Screate_simple");
        printf("%d: Created 3x5x7 simple\n", mpi_rank);

        dataset = H5Dcreate(fid, dset_3x5x7, H5T_NATIVE_INT, sid, H5P_DEFAULT);
        VRFY((dataset >= 0), "H5Dcreate");
        printf("%d: Created 3x5x7 dataset \"%s\"\n", mpi_rank, dset_3x5x7);

        VRFY((H5Sclose(sid) >= 0), "H5Sclose");
        VRFY((H5Dclose(dataset) >= 0), "H5Dclose");
    } 

    printf("%d: MPI barrier 1.0\n", mpi_rank);
    MPI_Barrier(SAP_Barrier_Comm);

    /*===-------------------------------------------------------------------===
     * Access datasets
     *===-------------------------------------------------------------------===
     */
    if (mpi_rank == 2) {
        hid_t xfer;
        int i, j;
        int rdata[3][5];

        /*===---------------------------------------------------------------===
         * Open 3x5 dataset and write values to it.
         *===---------------------------------------------------------------===
         */
        for (i = 0; i < 3; ++i)
            for (j = 0; j < 5; ++j)
                rdata[i][j] = i * j + 37;

        /* See if dataset is there */
        dataset = H5Dopen(fid, dset_3x5);
        VRFY((dataset >= 0), "H5Dopen");
        printf("%d: Opened dataset \"%s\"\n", mpi_rank, dset_3x5);

        xfer = H5Pcreate(H5P_DATASET_XFER);
        VRFY((xfer >= 0), "H5Pcreate");

        mrc = H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, xfer, rdata);
        VRFY((mrc >= 0), "H5Dwrite");
        printf("%d: Wrote to dataset \"%s\"\n", mpi_rank, dset_3x5);

        VRFY((H5Pclose(xfer) >= 0), "H5Pclose");
        VRFY((H5Dclose(dataset) >= 0), "H5Dclose");

        /*===---------------------------------------------------------------===
         * Open 3x5x7 dataset
         *===---------------------------------------------------------------===
         */
        dataset = H5Dopen(fid, dset_3x5x7);
        VRFY((dataset >= 0), "H5Dopen");
        printf("%d: Opened dataset \"%s\"\n", mpi_rank, dset_3x5x7);

        VRFY((H5Dclose(dataset) >= 0), "H5Dclose");
    }

    printf("%d: MPI barrier 2.0\n", mpi_rank);
    MPI_Barrier(SAP_Barrier_Comm);

    /*===-------------------------------------------------------------------===
     * Create a group
     *===-------------------------------------------------------------------===
     */
    if (mpi_rank == 2) {
        hid_t group;

        /*===---------------------------------------------------------------===
         * Create group "/group_1"
         *===---------------------------------------------------------------===
         */
        printf("%d: Creating group \"%s\"\n", mpi_rank, group_1);

        group = H5Gcreate(fid, group_1, 37);
        VRFY((group >= 0), "H5Gcreate");
        printf("%d: Created group \"%s\"\n", mpi_rank, group_1);

        VRFY((H5Gclose(group) >= 0), "H5Gclose");
    }

    printf("%d: MPI barrier 3.0\n", mpi_rank);
    MPI_Barrier(SAP_Barrier_Comm);

    /*===-------------------------------------------------------------------===
     * Create another group
     *===-------------------------------------------------------------------===
     */
    if (mpi_rank == 0) {
        hid_t group;

        /*===---------------------------------------------------------------===
         * Create group "/group_2"
         *===---------------------------------------------------------------===
         */
        printf("%d: Creating group \"%s\"\n", mpi_rank, group_2);

        group = H5Gcreate(fid, group_2, 37);
        VRFY((group >= 0), "H5Gcreate");
        printf("%d: Created group \"%s\"\n", mpi_rank, group_2);

        VRFY((H5Gclose(group) >= 0), "H5Gclose");
    }

    printf("%d: MPI barrier 4.0\n", mpi_rank);
    MPI_Barrier(SAP_Barrier_Comm);

    /*===-------------------------------------------------------------------===
     * Access groups and co-create datasets in them
     *===-------------------------------------------------------------------===
     */
    if (mpi_rank == 0) {
        hid_t group;
        hsize_t dims[3];

        /*===---------------------------------------------------------------===
         * Access group "/group_1"
         *===---------------------------------------------------------------===
         */
        printf("%d: Opening group \"%s\"\n", mpi_rank, group_1);

        group = H5Gopen(fid, group_1);
        VRFY((group >= 0), "H5Gopen");
        printf("%d: Opened group \"%s\"\n", mpi_rank, group_1);

        /*===---------------------------------------------------------------===
         * Create dataset "/group_1/dset-2x4x8"
         *===---------------------------------------------------------------===
         */
        dims[0] = 2;
        dims[1] = 4;
        dims[2] = 8;

        sid = H5Screate_simple(3, dims, NULL);
        VRFY((sid >= 0), "H5Screate_simple");
        printf("%d: Created simple 2x4x8\n", mpi_rank);

        dataset = H5Dcreate(group, dset_2x4x8, H5T_NATIVE_INT, sid, H5P_DEFAULT);
        VRFY((dataset >= 0), "H5Dcreate");
        printf("%d: Created dataset 2x4x8 \"%s\"\n", mpi_rank, dset_2x4x8);

        VRFY((H5Sclose(sid) >= 0), "H5Sclose");
        VRFY((H5Dclose(dataset) >= 0), "H5Dclose");
        VRFY((H5Gclose(group) >= 0), "H5Gclose");
    } else {
        hid_t group;
        hsize_t dims[3];

        /*===---------------------------------------------------------------===
         * Access group "/group_2"
         *===---------------------------------------------------------------===
         */
        printf("%d: Opening group \"%s\"\n", mpi_rank, group_2);

        group = H5Gopen(fid, group_2);
        VRFY((group >= 0), "H5Gopen");
        printf("%d: Opened group \"%s\"\n", mpi_rank, group_2);

        /*===---------------------------------------------------------------===
         * Create dataset "/group_2/dset-5x7x11"
         *===---------------------------------------------------------------===
         */
        dims[0] = 5;
        dims[1] = 7;
        dims[2] = 11;

        sid = H5Screate_simple(3, dims, NULL);
        VRFY((sid >= 0), "H5Screate_simple");
        printf("%d: Created simple 5x7x11\n", mpi_rank);

        dataset = H5Dcreate(group, dset_5x7x11, H5T_NATIVE_INT, sid, H5P_DEFAULT);
        VRFY((dataset >= 0), "H5Dcreate");
        printf("%d: Created dataset 5x7x11 \"%s\"\n", mpi_rank, dset_5x7x11);

        VRFY((H5Sclose(sid) >= 0), "H5Sclose");
        VRFY((H5Dclose(dataset) >= 0), "H5Dclose");
        VRFY((H5Gclose(group) >= 0), "H5Gclose");
    }

    printf("%d: MPI barrier 5.0\n", mpi_rank);
    MPI_Barrier(SAP_Barrier_Comm);

done:
    if (fid > -1) {
        VRFY((H5Fclose(fid) >= 0), "H5Fclose");
        printf("%d: Closed file\n", mpi_rank);
    }
}

int main(int argc, char *argv[])
{
    int     mrc;
    int     mpi_rank;
    int     sap_rank = 1;
    int     ret = EXIT_SUCCESS;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);

    H5open();
    h5_show_hostname();

    if (MAINPROCESS) {
        printf("===================================\n");
        printf("FPHDF5 functionality tests\n");
        printf("===================================\n");
    }

    mrc = H5FPinit(MPI_COMM_WORLD, sap_rank, &SAP_Comm, &SAP_Barrier_Comm);
    VRFY((mrc == MPI_SUCCESS), "H5FP_init");
    printf("%d: Initialized FPHDF5\n", mpi_rank);

    if (mpi_rank != sap_rank) {
        /*
         * Setup the file access property list that's used to create the
         * file.
         */
        int i;

        fapl = H5Pcreate(H5P_FILE_ACCESS);
        VRFY((fapl >= 0), "H5Pcreate");
        fprintf(stderr, "%d: Created access property list\n", mpi_rank);

        mrc = H5Pset_fapl_fphdf5(fapl, SAP_Comm, SAP_Barrier_Comm,
                                 MPI_INFO_NULL, (unsigned)sap_rank);
        VRFY((fapl >= 0), "H5Pset_fapl_fphdf5");
        printf("%d: Set access property list\n", mpi_rank);

        for (i = 0; i < sizeof(FILENAME) / sizeof(FILENAME[0]) - 1; ++i)
            if (h5_fixname(FILENAME[i], fapl, filenames[i], sizeof(filenames[i])) == NULL) {
                printf("h5_fixname failed\n");
                ++nerrors;
                goto fail;
            }

        create_file(sap_rank);

        if (fapl > -1) {
            if (!h5_cleanup(FILENAME, fapl)) {
                printf("%d: h5_cleanup failed\n", mpi_rank);
                ++nerrors;
                goto fail;
            }

            printf("%d: Closed property list\n", mpi_rank);
        }
    }

fail:
    VRFY((H5FPfinalize() >= 0), "H5FPfinalize");
    printf("%d: H5FP finalized\n", mpi_rank);

    if (MAINPROCESS) {  /* only process 0 reports */
        printf("===================================\n");

        if (nerrors)
            printf("***FPHDF5 test detected %d errors***\n", nerrors);
        else
            printf("FPHDF5 test finished with no errors\n");

        printf("===================================\n");
    }

    H5close();
    MPI_Finalize();
    return 0;
}

#else

/* dummy program since FPHDF5 is not configured in */
int
main(int argc, char *argv[])
{
    int mpi_rank;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);

    if (mpi_rank == 0)
        printf("No t_fphdf5 Test because FPHDF5 is not configured in\n");

    MPI_Finalize();
    return 0;
}
#endif
