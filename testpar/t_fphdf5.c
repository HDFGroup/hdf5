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
 * Flexible Parallel HDF5 test: Create a file using the FPHDF5 file
 * driver. Populate it with groups and datasets to make sure that FPHDF5
 * can handle it.
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

/*===-----------------------------------------------------------------------===
 *                              Local Functions
 *===-----------------------------------------------------------------------===
 */
static hid_t create_group(hid_t loc, const char *name, size_t size_hint);
static hid_t create_dset(hid_t loc, const char *name, hsize_t *dims, int ndims);
static void write_data(hid_t loc, const char *name, const void *buf);
static void create_file(const char *filename, int sap_rank);

/*===-----------------------------------------------------------------------===
 *                                Filenames
 *===-----------------------------------------------------------------------===
 * The names of the test files for 
 */
static const char *FILENAME[2] = {  /* List of files we want to create       */
    "FPHDF5Test",
    NULL
};
static char filenames[2][PATH_MAX]; /* "Fixed" filenames                     */

/*===-----------------------------------------------------------------------===
 *                             Global Variables
 *===-----------------------------------------------------------------------===
 */
static MPI_Comm SAP_Comm = MPI_COMM_NULL;           /* COMM for FPHDF5       */
static MPI_Comm SAP_Barrier_Comm = MPI_COMM_NULL;   /* COMM used in barriers */

static int      nerrors = 0;        /* Number of errors encountered          */
static hid_t    fapl = -1;          /* FPHDF5 file access property list      */
static int      verbose = 0;        /* Verbose output?                       */

/*-------------------------------------------------------------------------
 * Function:	create_group
 * Purpose:	Helper function that creates a group at the given
 *              location with the given name and size_hint.
 * Return:	Success:    The handle to the new group
 *              Failure:    <0
 * Programmer:	Bill Wendling
 *              29. October 2003
 * Modifications:
 *-------------------------------------------------------------------------
 */
static hid_t create_group(hid_t loc, const char *name, size_t size_hint)
{
    int     mpi_rank, mrc;
    hid_t   group;

    mrc = MPI_Comm_rank(SAP_Comm, &mpi_rank);
    VRFY((mrc == MPI_SUCCESS), "MPI_Comm_rank");
    printf("%d: Creating group \"%s\"\n", mpi_rank, name);

    group = H5Gcreate(loc, name, size_hint);
    VRFY((group >= 0), "H5Gcreate");
    printf("%d: Created group \"%s\"\n", mpi_rank, name);
    return group;
}

/*-------------------------------------------------------------------------
 * Function:	create_dset
 * Purpose:	Helper function that creates a dataset at the given
 *              location with the given name and dimensions
 * Return:	Success:    The handle to the new dataset
 *              Failure:    <0
 * Programmer:	Bill Wendling
 *              29. October 2003
 * Modifications:
 *-------------------------------------------------------------------------
 */
static hid_t create_dset(hid_t loc, const char *name, hsize_t *dims, int ndims)
{
    int     mpi_rank, mrc;
    hid_t   dset, sid;

    mrc = MPI_Comm_rank(SAP_Comm, &mpi_rank);
    VRFY((mrc == MPI_SUCCESS), "MPI_Comm_rank");

    sid = H5Screate_simple(ndims, dims, NULL);
    VRFY((sid >= 0), "H5Screate_simple");
    printf("%d: Created simple dataspace\n", mpi_rank);

    dset = H5Dcreate(loc, name, H5T_NATIVE_INT, sid, H5P_DEFAULT);
    VRFY((dset >= 0), "H5Dcreate");
    printf("%d: Created dataset \"%s\"\n", mpi_rank, name);

    VRFY((H5Sclose(sid) >= 0), "H5Sclose");
    return dset;
}

/*-------------------------------------------------------------------------
 * Function:	write_data
 * Purpose:	Helper function that writes data to a dataset.
 * Return:	Nothing
 * Programmer:	Bill Wendling
 *              29. October 2003
 * Modifications:
 *-------------------------------------------------------------------------
 */
static void write_data(hid_t loc, const char *name, const void *buf)
{
    int     mpi_rank, mrc;
    hid_t   dataset, xfer;

    mrc = MPI_Comm_rank(SAP_Comm, &mpi_rank);
    VRFY((mrc == MPI_SUCCESS), "MPI_Comm_rank");

    /* See if dataset is there */
    dataset = H5Dopen(loc, name);
    VRFY((dataset >= 0), "H5Dopen");
    printf("%d: Opened dataset \"%s\"\n", mpi_rank, name);

    xfer = H5Pcreate(H5P_DATASET_XFER);
    VRFY((xfer >= 0), "H5Pcreate");

    mrc = H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, xfer, buf);
    VRFY((mrc >= 0), "H5Dwrite");
    printf("%d: Wrote to dataset \"%s\"\n", mpi_rank, name);

    VRFY((H5Pclose(xfer) >= 0), "H5Pclose");
    VRFY((H5Dclose(dataset) >= 0), "H5Dclose");
}

/*-------------------------------------------------------------------------
 * Function:	create_file
 * Purpose:	The main function. Creates a file and populates it with
 *              groups and datasets so that we can make sure that FPHDF5
 *              works.
 * Return:	Nothing
 * Programmer:	Bill Wendling
 *              29. October 2003
 * Modifications:
 *-------------------------------------------------------------------------
 */
static void create_file(const char *filename, int sap_rank)
{
    int     mpi_rank, mrc;
    hid_t   fid = -1, dataset = -1;

    /* Datasets to create */
    const char dset_3x5[]       = "Dataset 3x5";
    const char dset_3x5x7[]     = "Dataset 3x5x7";
    const char dset_2x4x8[]     = "Dataset 2x4x8";
    const char dset_5x7x11[]    = "Dataset 5x7x11";

    /* Groups to create */
    const char group_dsets[]    = "Group Datasets";
    const char group_1[]        = "Group 1";
    const char group_2[]        = "Group 2";

    mrc = MPI_Comm_rank(SAP_Comm, &mpi_rank);
    VRFY((mrc == MPI_SUCCESS), "MPI_Comm_rank");

    /*===-------------------------------------------------------------------===
     * Create the file
     *===-------------------------------------------------------------------===
     */
    printf("%d: Creating file %s\n", mpi_rank, filenames[0]);

    fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
    VRFY((fid >= 0), "H5Fcreate");
    printf("%d: Created file %s\n", mpi_rank, filename);

    /*===-------------------------------------------------------------------===
     * Create datasets
     *===-------------------------------------------------------------------===
     */
    if (mpi_rank == 0) {
        /* Create a dataset in the file */
        hid_t   group;
        hsize_t dims[3];

        group = create_group(fid, group_dsets, 9);

        /*===---------------------------------------------------------------===
         * Create 3x5 dataset
         *===---------------------------------------------------------------===
         */
        dims[0] = 3;
        dims[1] = 5;

        dataset = create_dset(group, dset_3x5, dims, 2);
        VRFY((H5Dclose(dataset) >= 0), "H5Dclose");

        /*===---------------------------------------------------------------===
         * Create 3x5x7 dataset
         *===---------------------------------------------------------------===
         */
        dims[0] = 3;
        dims[1] = 5;
        dims[2] = 7;

        dataset = create_dset(group, dset_3x5x7, dims, 3);
        VRFY((H5Dclose(dataset) >= 0), "H5Dclose");
        VRFY((H5Gclose(group) >= 0), "H5Gclose");
    } 

    printf("%d: MPI barrier 1.0\n", mpi_rank);
    MPI_Barrier(SAP_Barrier_Comm);

    /*===-------------------------------------------------------------------===
     * Access datasets
     *===-------------------------------------------------------------------===
     */
    if (mpi_rank == 2) {
        hid_t   xfer, group;
        int     i, j, k;
        int     rdata[3][5];
        int     sdata[3][5][7];

        group = H5Gopen(fid, group_dsets);

        /*===---------------------------------------------------------------===
         * Open 3x5 dataset and write values to it.
         *===---------------------------------------------------------------===
         */
        for (i = 0; i < 3; ++i)
            for (j = 0; j < 5; ++j)
                rdata[i][j] = i * j + 37;

        write_data(group, dset_3x5, rdata);

        /*===---------------------------------------------------------------===
         * Open 3x5x7 dataset
         *===---------------------------------------------------------------===
         */
        for (i = 0; i < 3; ++i)
            for (j = 0; j < 5; ++j)
                for (k = 0; k < 7; ++k)
                    sdata[i][j][k] = i * j * k + 927;

        write_data(group, dset_3x5x7, sdata);
        VRFY((H5Gclose(group) >= 0), "H5Gclose");
    }

    printf("%d: MPI barrier 2.0\n", mpi_rank);
    MPI_Barrier(SAP_Barrier_Comm);

    /*===-------------------------------------------------------------------===
     * Create a group
     *===-------------------------------------------------------------------===
     */
    if (mpi_rank == 2) {
        /*===---------------------------------------------------------------===
         * Create group "/group_1"
         *===---------------------------------------------------------------===
         */
        hid_t group = create_group(fid, group_1, 37);
        VRFY((H5Gclose(group) >= 0), "H5Gclose");
    }

    printf("%d: MPI barrier 3.0\n", mpi_rank);
    MPI_Barrier(SAP_Barrier_Comm);

    /*===-------------------------------------------------------------------===
     * Create another group
     *===-------------------------------------------------------------------===
     */
    if (mpi_rank == 0) {
        /*===---------------------------------------------------------------===
         * Create group "/group_2"
         *===---------------------------------------------------------------===
         */
        hid_t group = create_group(fid, group_2, 37);
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

        dataset = create_dset(group, dset_2x4x8, dims, 3);
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

        dataset = create_dset(group, dset_5x7x11, dims, 3);
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

        for (i = 0; i < sizeof(FILENAME) / sizeof(FILENAME[0]) - 1; ++i) {
            if (h5_fixname(FILENAME[i], fapl, filenames[i], sizeof(filenames[i])) == NULL) {
                printf("h5_fixname failed\n");
                ++nerrors;
                goto fail;
            }

            create_file(filenames[i], sap_rank);
        }

        if (fapl > -1)
            h5_cleanup(FILENAME, fapl);
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

/* Dummy program since FPHDF5 is not configured in */
int
main(int argc, char *argv[])
{
    int mpi_rank;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);

    if (mpi_rank == 0)
        printf("No t_fphdf5 test because FPHDF5 is not configured in\n");

    MPI_Finalize();
    return 0;
}
#endif  /* H5_HAVE_FPHDF5 */
