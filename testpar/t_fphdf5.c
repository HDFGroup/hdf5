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

/*===----------------------------------------------------------------------===
 *                              Local Functions
 *===----------------------------------------------------------------------===
 */
static hid_t create_group(hid_t loc, const char *grp_name, size_t size_hint);
static hid_t create_dset(hid_t loc, const char *dset_name);
static void access_dset(hid_t loc, const char *dset_name);
static void slab_set(hssize_t start[], hsize_t count[],
                     hsize_t stride[], hsize_t block[]);
static void write_data(hid_t loc, const char *dset_name, 
                       hssize_t start[], hsize_t count[],
                       hsize_t stride[], hsize_t block[],
                       int *buf);
static void verify_dataset(hid_t loc, const char *dset_name, hssize_t start[],
                           hsize_t count[], hsize_t stride[], hsize_t block[],
                           int *original);
static hid_t create_file(const char *filename);
static void test_group_creation(hid_t loc);
static void test_dataset_creation(hid_t loc);
static void test_dataset_access(hid_t loc);
static void test_dataset_write(hid_t loc);
static void usage(const char *prog);

/*===----------------------------------------------------------------------===
 *                                Filenames
 *===----------------------------------------------------------------------===
 * The names of the test files for 
 */
static const char *FILENAME[2] = {  /* List of files we want to create      */
    "FPHDF5Test",
    NULL
};
static char filenames[2][PATH_MAX]; /* "Fixed" filenames                    */

/*===----------------------------------------------------------------------===
 *                             Global Variables
 *===----------------------------------------------------------------------===
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

int             nerrors = 0;        /* Errors count                         */
int             verbose = 0;        /* Verbose, default is no               */

static const char  *progname = "t_fphdf5";

/* Dataset Name Template */
static const char *dset_tmpl       = "Dataset %d";
static char        dset_name[128];

/* Group Name Template */
static const char *grp_tmpl        = "Process %d's Datasets";
static char        grp_name[128];

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
static hid_t
create_group(hid_t loc, const char *grp_name, size_t size_hint)
{
    hid_t   group;

    fprintf(stderr, "%d: Creating group \"%s\"\n", mpi_rank, grp_name);

    VRFY(((group = H5Gcreate(loc, grp_name, size_hint)) >= 0), "H5Gcreate");
    fprintf(stderr, "%d: Created group \"%s\"\n", mpi_rank, grp_name);
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
static hid_t
create_dset(hid_t loc, const char *dset_name)
{
    hsize_t dims[RANK] = { DIM0, DIM1 };
    hid_t   dset, sid;

    VRFY(((sid = H5Screate_simple(RANK, dims, NULL)) >= 0), "H5Screate_simple");
    fprintf(stderr, "%d: Created simple dataspace\n", mpi_rank);

    dset = H5Dcreate(loc, dset_name, H5T_NATIVE_INT, sid, H5P_DEFAULT);
    VRFY((dset >= 0), "H5Dcreate");
    fprintf(stderr, "%d: Created dataset \"%s\"\n", mpi_rank, dset_name);

    VRFY((H5Sclose(sid) >= 0), "H5Sclose");
    return dset;
}

/*-------------------------------------------------------------------------
 * Function:	access_dset
 * Purpose:	Quickly check to see if we can access this dataset.
 * Return:	Nothing, but aborts if an error occurs.
 * Programmer:	Bill Wendling
 *              03. November 2003
 * Modifications:
 *-------------------------------------------------------------------------
 */
static void
access_dset(hid_t loc, const char *dset_name)
{
    hid_t   dataset;

    VRFY(((dataset = H5Dopen(loc, dset_name)) >= 0), "H5Dopen");
    VRFY((H5Dclose(dataset) >= 0), "H5Dclose");
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
static void
slab_set(hssize_t start[], hsize_t count[], hsize_t stride[], hsize_t block[])
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
static void
write_data(hid_t loc, const char *dset_name, hssize_t start[], hsize_t count[],
           hsize_t stride[], hsize_t block[], int *buf)
{
    herr_t      hrc;
    hid_t       file_dataspace, mem_dataspace;
    hid_t       dataset, xfer;
    hsize_t     j, k;
    int        *dataptr = buf;

    /* See if dataset is there */
    VRFY(((dataset = H5Dopen(loc, dset_name)) >= 0), "H5Dopen");

    xfer = H5Pcreate(H5P_DATASET_XFER);
    VRFY((xfer >= 0), "H5Pcreate");

    file_dataspace = H5Dget_space(dataset);
    VRFY((file_dataspace >= 0), "H5Dget_space");

    /* put some trivial data in the buffer */
    for (j = 0; j < block[0]; ++j)
        for (k = 0; k < block[1]; ++k)
            *dataptr++ = (k + start[0]) * 100 + k + start[1] + 1;

    hrc = H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET,
                              start, stride, count, block);
    VRFY((hrc >= 0), "H5Sselect_hyperslab");

    /* create a memory dataspace independently */
    mem_dataspace = H5Screate_simple(RANK, block, NULL);
    VRFY((mem_dataspace >= 0), "H5Screate_simple");

    hrc = H5Dwrite(dataset, H5T_NATIVE_INT, mem_dataspace,
                   file_dataspace, H5P_DEFAULT, buf);
    VRFY((hrc >= 0), "H5Dwrite");

    VRFY((H5Sclose(mem_dataspace) >= 0), "H5Sclose");
    VRFY((H5Sclose(file_dataspace) >= 0), "H5Sclose");
    VRFY((H5Pclose(xfer) >= 0), "H5Pclose");
    VRFY((H5Dclose(dataset) >= 0), "H5Dclose");
}

/*-------------------------------------------------------------------------
 * Function:	verify_dataset
 * Purpose:	Verify the data in the dataset is correct.
 * Return:	Nothing
 * Programmer:	Bill Wendling
 *              10. November 2003
 * Modifications:
 *-------------------------------------------------------------------------
 */
static void
verify_dataset(hid_t loc, const char *dset_name, hssize_t start[], hsize_t count[],
               hsize_t stride[], hsize_t block[], int *original)
{
    hid_t       dataset, file_dataspace, mem_dataspace;
    hsize_t     i, j;
    int        *data_array;
    int         vrfyerrs = 0;

    /* Open the dataset */
    VRFY(((dataset = H5Dopen(loc, dset_name)) >= 0), "H5Dopen");

    /* Create a file dataspace */
    file_dataspace = H5Dget_space(dataset);
    VRFY((file_dataspace >= 0), "H5Dget_space");
    VRFY((H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET,
                              start, stride, count, block) >= 0), "H5Sselect_hyperslab");

    /* Create a memory dataspace */
    mem_dataspace = H5Screate_simple(RANK, block, NULL);
    VRFY((mem_dataspace >= 0), "H5Screate_simple");

    /* Read the dataset */
    data_array = (int *)malloc(DIM0 * DIM1 * sizeof(int));
    VRFY((H5Dread(dataset, H5T_NATIVE_INT, mem_dataspace, file_dataspace,
                  H5P_DEFAULT, data_array) >= 0), "H5Dread");

    /* Verify the contents of the dataset */
    for (i = 0; i < block[0]; ++i)
	for (j = 0; j < block[1]; ++j)
	    if (*data_array != *original) {
		if (vrfyerrs++ < MAX_ERR_REPORT)
		    fprintf(stderr,
                            "Dataset Verify failed at "
                            "[%ld][%ld](row %ld, col %ld): expect %d, got %d\n",
                            (long)i, (long)j, (long)(i + start[0]),
                            (long)(j + start[1]), *original, *data_array);

		++data_array;
		++original;
	    }

    if (vrfyerrs)
	fprintf(stderr, "%d errors found in verify_dataset\n", vrfyerrs);

    VRFY((H5Sclose(mem_dataspace) >= 0), "H5Sclose");
    VRFY((H5Sclose(file_dataspace) >= 0), "H5Sclose");
    VRFY((H5Dclose(dataset) >= 0), "H5Dclose");
    free(data_array);
}

/*-------------------------------------------------------------------------
 * Function:	create_file
 * Purpose:	Create a new file with the given filename.
 * Return:	Success:    Valid file ID
 *              Failure:    -1
 * Programmer:	Bill Wendling
 *              29. October 2003
 * Modifications:
 *-------------------------------------------------------------------------
 */
static hid_t
create_file(const char *filename)
{
    hid_t       fid = -1;

    fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
    VRFY((fid >= 0), "H5Fcreate");
    fprintf(stderr, "%d: Created file %s\n", mpi_rank, filename);
    return fid;
}

/*-------------------------------------------------------------------------
 * Function:	test_group_creation
 * Purpose:	Test creation of multiple groups in the file.
 * Return:	Nothing
 * Programmer:	Bill Wendling
 *              11. November 2003
 * Modifications:
 *-------------------------------------------------------------------------
 */
static void
test_group_creation(hid_t loc)
{
    if (mpi_rank == 0) {
        hid_t   group;
        int     i;

        for (i = 0; i < mpi_size; ++i)
            if (i != SAP_RANK) {
                sprintf(grp_name, grp_tmpl, i);
                group = create_group(loc, grp_name, 4);
                VRFY((H5Gclose(group) >= 0), "H5Gclose");
            }
    }

    SYNC(SAP_Barrier_Comm);
}

/*-------------------------------------------------------------------------
 * Function:	test_dataset_creation
 * Purpose:	Test simultaneous creation of multiple datasets in a
 *              non-root group.
 * Return:	Nothing
 * Programmer:	Bill Wendling
 *              11. November 2003
 * Modifications:
 *-------------------------------------------------------------------------
 */
static void
test_dataset_creation(hid_t loc)
{
    hid_t   dataset, group;

    sprintf(grp_name, grp_tmpl, mpi_rank);
    group = H5Gopen(loc, grp_name);
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
}

/*-------------------------------------------------------------------------
 * Function:	test_dataset_access
 * Purpose:	Test that we can access the datasets in the file from all
 *              processes.
 * Return:	Nothing
 * Programmer:	Bill Wendling
 *              11. November 2003
 * Modifications:
 *-------------------------------------------------------------------------
 */
static void
test_dataset_access(hid_t loc)
{
    hid_t   group;
    int     i;

    for (i = 0; i < mpi_size; ++i)
        if (i != SAP_RANK) {
            sprintf(grp_name, grp_tmpl, i);
            VRFY(((group = H5Gopen(loc, grp_name)) >= 0), "H5Gopen");

            sprintf(dset_name, dset_tmpl, 0);
            fprintf(stderr, "%d: Accessing dataset \"%s/%s\"\n", mpi_rank, grp_name, dset_name);
            access_dset(group, dset_name);

            sprintf(dset_name, dset_tmpl, 1);
            fprintf(stderr, "%d: Accessing dataset \"%s/%s\"\n", mpi_rank, grp_name, dset_name);
            access_dset(group, dset_name);

            sprintf(dset_name, dset_tmpl, 2);
            fprintf(stderr, "%d: Accessing dataset \"%s/%s\"\n", mpi_rank, grp_name, dset_name);
            access_dset(group, dset_name);

            sprintf(dset_name, dset_tmpl, 3);
            fprintf(stderr, "%d: Accessing dataset \"%s/%s\"\n", mpi_rank, grp_name, dset_name);
            access_dset(group, dset_name);

            VRFY((H5Gclose(group) >= 0), "H5Gclose");
        }

    SYNC(SAP_Barrier_Comm);
}

/*-------------------------------------------------------------------------
 * Function:	test_dataset_write
 * Purpose:	Test that we can write to the datasets in the file from
 *              all processes simultaneously.
 * Return:	Nothing
 * Programmer:	Bill Wendling
 *              11. November 2003
 * Modifications:
 *-------------------------------------------------------------------------
 */
static void
test_dataset_write(hid_t loc)
{
    hid_t       group;
    int         i, *data_array = NULL;

    /* Hyperslab settings */
    hssize_t    start[RANK];
    hsize_t     count[RANK];
    hsize_t     stride[RANK];
    hsize_t     block[RANK];

    data_array = (int *)malloc(DIM0 * DIM1 * sizeof(int));
    VRFY((data_array != NULL), "data_array malloc succeeded");

    slab_set(start, count, stride, block);

    /*===-------------------------------------------------------------------===
     * All processes write to each dataset.
     *===-------------------------------------------------------------------===
     */
    for (i = 0; i < mpi_size; ++i)
        if (i != SAP_RANK) {
            sprintf(grp_name, grp_tmpl, i);
            VRFY(((group = H5Gopen(loc, grp_name)) >= 0), "H5Gopen");

            /* Write to this dataset */
            sprintf(dset_name, dset_tmpl, 0);
            fprintf(stderr, "%d: Writing to \"/%s/%s\"\n", mpi_rank, grp_name, dset_name);
            write_data(group, dset_name, start, count, stride, block, data_array);
            fprintf(stderr, "%d: Verifying dataset \"/%s/%s\"\n", mpi_rank, grp_name, dset_name);
            verify_dataset(group, dset_name, start, count, stride, block, data_array);

            sprintf(dset_name, dset_tmpl, 1);
            fprintf(stderr, "%d: Writing to \"/%s/%s\"\n", mpi_rank, grp_name, dset_name);
            write_data(group, dset_name, start, count, stride, block, data_array);
            fprintf(stderr, "%d: Verifying dataset \"/%s/%s\"\n", mpi_rank, grp_name, dset_name);
            verify_dataset(group, dset_name, start, count, stride, block, data_array);

            sprintf(dset_name, dset_tmpl, 2);
            fprintf(stderr, "%d: Writing to \"/%s/%s\"\n", mpi_rank, grp_name, dset_name);
            write_data(group, dset_name, start, count, stride, block, data_array);
            fprintf(stderr, "%d: Verifying dataset \"/%s/%s\"\n", mpi_rank, grp_name, dset_name);
            verify_dataset(group, dset_name, start, count, stride, block, data_array);

            sprintf(dset_name, dset_tmpl, 3);
            fprintf(stderr, "%d: Writing to \"/%s/%s\"\n", mpi_rank, grp_name, dset_name);
            write_data(group, dset_name, start, count, stride, block, data_array);
            fprintf(stderr, "%d: Verifying dataset \"/%s/%s\"\n", mpi_rank, grp_name, dset_name);
            verify_dataset(group, dset_name, start, count, stride, block, data_array);
            
            /* Close the group */
            VRFY((H5Gclose(group) >= 0), "H5Gclose");
        }

    free(data_array);
}

/*-------------------------------------------------------------------------
 * Function:	usage
 * Purpose:	Print a usage message.
 * Return:	Nothing
 * Programmer:	Bill Wendling
 *              11. November 2003
 * Modifications:
 *-------------------------------------------------------------------------
 */
static void
usage(const char *prog)
{
    if (mpi_rank == 0) {
        fprintf(stderr, "usage: %s [OPTIONS]\n", prog);
        fprintf(stderr, "  OPTIONS\n");
        fprintf(stderr, "     -h, --help         Print a usage message and exit\n");
        fprintf(stderr, "     -v, --verbose      Verbose output [default: no]\n");
        fprintf(stderr, "\n");
        fflush(stdout);
    }
}

/*-------------------------------------------------------------------------
 * Function:	main
 * Purpose:	Parse the command line variables and run the test
 *              program.
 * Return:	Success:        0
 *              Failure:        >0
 * Programmer:	Bill Wendling
 *              11. November 2003
 * Modifications:
 *-------------------------------------------------------------------------
 */
int
main(int argc, char *argv[])
{
    hid_t   fid, fapl2;
    herr_t  hrc;
    int     nargs;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);
    MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);

    H5open();

    for (nargs = argc; nargs > 1; --nargs)
        if (strcmp(argv[nargs - 1], "-v") == 0 || 
                strcmp(argv[nargs - 1], "--verbose") == 0 ||
                strcmp(argv[nargs - 1], "--verbos") == 0 ||
                strcmp(argv[nargs - 1], "--verbo") == 0 ||
                strcmp(argv[nargs - 1], "--verb") == 0 ||
                strcmp(argv[nargs - 1], "--ver") == 0 ||
                strcmp(argv[nargs - 1], "--ve") == 0) {
            verbose = 1;
        } else if (strcmp(argv[nargs - 1], "-h") == 0 ||
                strcmp(argv[nargs - 1], "--help") == 0 ||
                strcmp(argv[nargs - 1], "--hel") == 0 ||
                strcmp(argv[nargs - 1], "--he") == 0) {
            usage(progname);
            return 0;
        } else {
            fprintf(stderr, "Unknown option: %s\n", argv[nargs - 1]);
            usage(progname);
            return 1;
        }

    h5_show_hostname();

    if (MAINPROCESS) {
        fprintf(stderr, "===================================\n");
        fprintf(stderr, "FPHDF5 functionality tests\n");
        fprintf(stderr, "===================================\n");
    }

    hrc = H5FPinit(MPI_COMM_WORLD, SAP_RANK, &SAP_Comm, &SAP_Barrier_Comm);
    VRFY((hrc == MPI_SUCCESS), "H5FP_init");
    fprintf(stderr, "%d: Initialized FPHDF5\n", mpi_rank);

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
        fprintf(stderr, "%d: Set access property list\n", mpi_rank);

        for (i = 0; i < sizeof(FILENAME) / sizeof(FILENAME[0]) - 1; ++i) {
            if (h5_fixname(FILENAME[i], fapl, filenames[i], sizeof(filenames[i])) == NULL) {
                fprintf(stderr, "h5_fixname failed\n");
                ++nerrors;
                break;
            }

            fid = create_file(filenames[i]);

            if (fid < 0)
                break;

            test_group_creation(fid);
            test_dataset_creation(fid);
            test_dataset_access(fid);
            test_dataset_write(fid);

            VRFY((H5Fclose(fid) >= 0), "H5Fclose");
            SYNC(SAP_Barrier_Comm);
            fprintf(stderr, "%d: Closed file\n", mpi_rank);

            fid = H5Fopen(filenames[i], H5F_ACC_RDONLY, fapl);
            VRFY((fid >= 0), "H5Fopen");
            SYNC(SAP_Barrier_Comm);
            VRFY((H5Fclose(fid) >= 0), "H5Fclose");
        }

        if (fapl > -1)
            h5_cleanup(FILENAME, fapl);
    }

    VRFY((H5FPfinalize() >= 0), "H5FPfinalize");
    fprintf(stderr, "%d: H5FP finalized\n", mpi_rank);

    if (MAINPROCESS) {  /* only process 0 reports */
        fprintf(stderr, "===================================\n");

        if (nerrors)
            fprintf(stderr, "***FPHDF5 test detected %d errors***\n", nerrors);
        else
            fprintf(stderr, "FPHDF5 test finished with no errors\n");

        fprintf(stderr, "===================================\n");
    }

done:
    H5close();
    MPI_Finalize();
    return nerrors;
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
        fprintf(stderr, "No t_fphdf5 test because FPHDF5 is not configured in\n");

    MPI_Finalize();
    return 0;
}
#endif  /* H5_HAVE_FPHDF5 */
