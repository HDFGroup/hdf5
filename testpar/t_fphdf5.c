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
#include <stdarg.h>
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
static hid_t create_dset(hid_t loc, const char *name);
static void access_dset(hid_t loc, const char *dset_name, ...);
static void slab_set(hssize_t start[], hsize_t count[],
                     hsize_t stride[], hsize_t block[]);
static void write_data(hid_t loc, const char *name, int *buf);
static void create_file(const char *filename);

/*===-----------------------------------------------------------------------===
 *                                Filenames
 *===-----------------------------------------------------------------------===
 * The names of the test files for 
 */
static const char *FILENAME[2] = {  /* List of files we want to create      */
    "FPHDF5Test",
    NULL
};
static char filenames[2][PATH_MAX]; /* "Fixed" filenames                    */

/*===-----------------------------------------------------------------------===
 *                             Global Variables
 *===-----------------------------------------------------------------------===
 */
#ifdef RANK
#undef RANK
#endif  /* RANK */
#ifdef DIM0
#undef DIM0
#endif  /* !DIM0 */
#ifdef DIM1
#undef DIM1
#endif  /* !DIM1 */

enum {
    SAP_RANK = 1,                /* The rank acting as the SAP           */
    RANK = 2,
    DIM0 = 6,
    DIM1 = 12
};

static MPI_Comm SAP_Comm = MPI_COMM_NULL;           /* COMM for FPHDF5      */
static MPI_Comm SAP_Barrier_Comm = MPI_COMM_NULL;   /* COMM used in barriers*/

static hid_t    fapl = -1;          /* FPHDF5 file access property list     */
static int      mpi_rank;           /* Rank of this process                 */
static int      mpi_size;           /* Size of the COMM passed to FPHDF5    */

int             nerrors;            /* Errors count                         */
int             verbose;            /* Verbose, default is no               */

/*-------------------------------------------------------------------------
 * Function:	create_group
 * Purpose:	Helper function that creates a group at the given
 *              location with the given name and size_hint.
 * Return:	Success:    The handle to the new group
 *              Failure:    Aborts
 * Programmer:	Bill Wendling
 *              29. October 2003
 * Modifications:
 *-------------------------------------------------------------------------
 */
static hid_t create_group(hid_t loc, const char *name, size_t size_hint)
{
    hid_t   group;

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
 *              Failure:    Aborts
 * Programmer:	Bill Wendling
 *              29. October 2003
 * Modifications:
 *-------------------------------------------------------------------------
 */
static hid_t create_dset(hid_t loc, const char *name)
{
    hsize_t dims[RANK] = { DIM0, DIM1 };
    hid_t   dset, sid;

    sid = H5Screate_simple(RANK, dims, NULL);
    VRFY((sid >= 0), "H5Screate_simple");
    printf("%d: Created simple dataspace\n", mpi_rank);

    dset = H5Dcreate(loc, name, H5T_NATIVE_INT, sid, H5P_DEFAULT);
    VRFY((dset >= 0), "H5Dcreate");
    printf("%d: Created dataset \"%s\"\n", mpi_rank, name);

    VRFY((H5Sclose(sid) >= 0), "H5Sclose");
    return dset;
}

/*-------------------------------------------------------------------------
 * Function:	access_dset
 * Purpose:	Quickly check that we can access this dataset.
 * Return:	Nothing, but aborts if an error occurs.
 * Programmer:	Bill Wendling
 *              03. November 2003
 * Modifications:
 *-------------------------------------------------------------------------
 */
static void access_dset(hid_t loc, const char *dset_name, ...)
{
    va_list ap;
    hid_t   dataset;

    /* Open the dataset */
    dataset = H5Dopen(loc, dset_name);
    VRFY((dataset >= 0), "H5Dopen");
    printf("%d: Opened dataset \"%s\"\n", mpi_rank, dset_name);

    /* Close the dataset */
    VRFY((H5Dclose(dataset) >= 0), "H5Dclose");

    /* Print out a nice message */
    printf("%d: Accessed dataset \"/", mpi_rank);
    va_start(ap, dset_name);

    for (;;) {
        const char *str = va_arg(ap, char *);

        if (!str)
            break;

        printf("%s/", str);
    }

    va_end(ap);
    printf("%s\"\n", dset_name);
}

/*-------------------------------------------------------------------------
 * Function:	slab_set
 * Purpose:	Setup the dimensions of the hyperslab.
 * Return:	Nothing
 * Programmer:	Bill Wendling
 *              05. November 2003
 * Modifications:
 *-------------------------------------------------------------------------
 */
static void slab_set(hssize_t start[], hsize_t count[],
                     hsize_t stride[], hsize_t block[])
{
    /* Each process takes a slab of rows. */
    block[0] = DIM0 / mpi_size;
    block[1] = DIM1;

    stride[0] = block[0];
    stride[1] = block[1];

    count[0] = 1;
    count[1] = 1;

    start[0] = mpi_rank * block[0];
    start[1] = 0;
}

/*-------------------------------------------------------------------------
 * Function:	write_data
 * Purpose:	Helper function that writes data to a dataset.
 * Return:	Nothing, but aborts if an error occurs.
 * Programmer:	Bill Wendling
 *              29. October 2003
 * Modifications:
 *-------------------------------------------------------------------------
 */
static void write_data(hid_t loc, const char *name, int *buf)
{
    herr_t      hrc;
    hid_t       file_dataspace, mem_dataspace;
    hid_t       dataset, xfer;
    hsize_t     j, k;
    int        *dataptr = buf;
    hssize_t    start[RANK];    /* for hyperslab setting    */
    hsize_t     count[RANK];    /* for hyperslab setting    */
    hsize_t     stride[RANK];   /* for hyperslab setting    */
    hsize_t     block[RANK];    /* for hyperslab setting    */

    /* See if dataset is there */
    VRFY(((dataset = H5Dopen(loc, name)) >= 0), "H5Dopen");
    printf("%d: Opened dataset \"%s\"\n", mpi_rank, name);

    xfer = H5Pcreate(H5P_DATASET_XFER);
    VRFY((xfer >= 0), "H5Pcreate");

    file_dataspace = H5Dget_space(dataset);
    VRFY((file_dataspace >= 0), "H5Dget_space");

    slab_set(start, count, stride, block);

    /* put some trivial data in the buffer */
    for (j = 0; j < block[0]; ++j)
        for (k = 0; k < block[1]; ++k)
            *dataptr++ = (k + start[0]) * 100 + k + start[1] + 1;

    hrc = H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET,
                              start, stride, count, block);
    VRFY((hrc >= 0), "H5Sselect_hyperslab");

    /* create a memory dataspace independently */
    mem_dataspace = H5Screate_simple(RANK, block, NULL);
    VRFY((mem_dataspace >= 0), "");

    hrc = H5Dwrite(dataset, H5T_NATIVE_INT, mem_dataspace,
                   file_dataspace, H5P_DEFAULT, buf);
    VRFY((hrc >= 0), "H5Dwrite");

    VRFY((H5Sclose(mem_dataspace) >= 0), "H5Sclose");
    VRFY((H5Sclose(file_dataspace) >= 0), "H5Sclose");
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
static void create_file(const char *filename)
{
    hid_t       fid = -1, dataset = -1, group = -1;
    int         i;
    int        *data_array = NULL;

    /* Dataset Name Template */
    const char *dset_tmpl       = "Dataset %d";
    char        dset_name[128];

    /* Dataset Group Name Template */
    const char *grp_tmpl        = "Process %d's Datasets";
    char        grp_name[128];

    /*=========================================================================
     * Create the file
     *=========================================================================
     */
    printf("%d: Creating file %s\n", mpi_rank, filenames[0]);
    fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
    VRFY((fid >= 0), "H5Fcreate");
    printf("%d: Created file %s\n", mpi_rank, filename);

    /*=========================================================================
     *  For each non-SAP process:
     *      Create datasets in non-root group
     *      Access all datasets from all processes
     *      Write to all datasets from all processes
     *  End
     *=========================================================================
     */
    if (mpi_rank == 0)
        for (i = 0; i < mpi_size; ++i)
            if (i != SAP_RANK) {
                sprintf(grp_name, grp_tmpl, i);
                group = create_group(fid, grp_name, 4);
                VRFY((H5Gclose(group) >= 0), "H5Gclose");
            }

    SYNC(SAP_Barrier_Comm);

    /*===-------------------------------------------------------------------===
     * Each process creates datasets in individual, non-root groups.
     *===-------------------------------------------------------------------===
     */
    sprintf(grp_name, grp_tmpl, mpi_rank);
    group = H5Gopen(fid, grp_name);
    VRFY((group >= 0), "H5Gopen");

    sprintf(dset_name, dset_tmpl, 0);
    dataset = create_dset(group, dset_name);
    VRFY((H5Dclose(dataset) >= 0), "H5Dclose");

    /* Create 3x5x(mpi_size) dataset */
    sprintf(dset_name, dset_tmpl, 1);
    dataset = create_dset(group, dset_name);
    VRFY((H5Dclose(dataset) >= 0), "H5Dclose");

    /* Create 2x4x(mpi_size) dataset */
    sprintf(dset_name, dset_tmpl, 2);
    dataset = create_dset(group, dset_name);
    VRFY((H5Dclose(dataset) >= 0), "H5Dclose");

    /* Create 5x7x(mpi_size) dataset */
    sprintf(dset_name, dset_tmpl, 3);
    dataset = create_dset(group, dset_name);
    VRFY((H5Dclose(dataset) >= 0), "H5Dclose");
    VRFY((H5Gclose(group) >= 0), "H5Gclose");

    SYNC(SAP_Barrier_Comm);

    /*===-------------------------------------------------------------------===
     * Check that we can access all of the datasets created above
     *===-------------------------------------------------------------------===
     */
    for (i = 0; i < mpi_size; ++i)
        if (i != SAP_RANK) {
            sprintf(grp_name, grp_tmpl, i);
            VRFY(((group = H5Gopen(fid, grp_name)) >= 0), "H5Gopen");
            sprintf(dset_name, dset_tmpl, 0);
            access_dset(group, dset_name, grp_name, NULL);
            sprintf(dset_name, dset_tmpl, 1);
            access_dset(group, dset_name, grp_name, NULL);
            sprintf(dset_name, dset_tmpl, 2);
            access_dset(group, dset_name, grp_name, NULL);
            sprintf(dset_name, dset_tmpl, 3);
            access_dset(group, dset_name, grp_name, NULL);
            VRFY((H5Gclose(group) >= 0), "H5Gclose");
        }

    SYNC(SAP_Barrier_Comm);

    data_array = (int *)malloc(DIM0 * DIM1 * sizeof(int));
    VRFY((data_array != NULL), "data_array malloc succeeded");

    /*===-------------------------------------------------------------------===
     * All processes write to each dataset.
     *===-------------------------------------------------------------------===
     */
    for (i = 0; i < mpi_size; ++i)
        if (i != SAP_RANK) {
            sprintf(grp_name, grp_tmpl, i);
            VRFY(((group = H5Gopen(fid, grp_name)) >= 0), "H5Gopen");

            /* Write to this dataset */
            sprintf(dset_name, dset_tmpl, 0);
            printf("%d: Writing to /%s/%s\n", mpi_rank, grp_name, dset_name);
            write_data(group, dset_name, data_array);

            sprintf(dset_name, dset_tmpl, 1);
            printf("%d: Writing to /%s/%s\n", mpi_rank, grp_name, dset_name);
            write_data(group, dset_name, data_array);

            sprintf(dset_name, dset_tmpl, 2);
            printf("%d: Writing to /%s/%s\n", mpi_rank, grp_name, dset_name);
            write_data(group, dset_name, data_array);

            sprintf(dset_name, dset_tmpl, 3);
            printf("%d: Writing to /%s/%s\n", mpi_rank, grp_name, dset_name);
            write_data(group, dset_name, data_array);
            
            /* Close the group */
            VRFY((H5Gclose(group) >= 0), "H5Gclose");
        }

    free(data_array);

    if (fid > -1) {
        VRFY((H5Fclose(fid) >= 0), "H5Fclose");
        printf("%d: Closed file\n", mpi_rank);
    }
}

int main(int argc, char *argv[])
{
    herr_t  hrc;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);
    MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);

    H5open();
    h5_show_hostname();

    if (MAINPROCESS) {
        printf("===================================\n");
        printf("FPHDF5 functionality tests\n");
        printf("===================================\n");
    }

    hrc = H5FPinit(MPI_COMM_WORLD, SAP_RANK, &SAP_Comm, &SAP_Barrier_Comm);
    VRFY((hrc == MPI_SUCCESS), "H5FP_init");
    printf("%d: Initialized FPHDF5\n", mpi_rank);

    if (mpi_rank != SAP_RANK) {
        /*
         * Setup the file access property list that's used to create the
         * file.
         */
        unsigned i;

        fapl = H5Pcreate(H5P_FILE_ACCESS);
        VRFY((fapl >= 0), "H5Pcreate");
        fprintf(stderr, "%d: Created access property list\n", mpi_rank);

        hrc = H5Pset_fapl_fphdf5(fapl, SAP_Comm, SAP_Barrier_Comm,
                                 MPI_INFO_NULL, (unsigned)SAP_RANK);
        VRFY((fapl >= 0), "H5Pset_fapl_fphdf5");
        printf("%d: Set access property list\n", mpi_rank);

        for (i = 0; i < sizeof(FILENAME) / sizeof(FILENAME[0]) - 1; ++i) {
            if (h5_fixname(FILENAME[i], fapl, filenames[i], sizeof(filenames[i])) == NULL) {
                printf("h5_fixname failed\n");
                ++nerrors;
                goto fail;
            }

            create_file(filenames[i]);
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
