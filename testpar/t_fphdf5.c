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
static hid_t create_file(const char *filename);
static hid_t create_group(hid_t loc, const char *grp_name, size_t size_hint);
static hid_t create_dset(hid_t loc, const char *dset_name);
static void access_dset(hid_t loc, const char *dset_name);
static void slab_set(hsize_t start[], hsize_t count[],
                     hsize_t stride[], hsize_t block[]);
static void fill_data(void);
static void write_data(hid_t loc, const char *dset_name,
                       hsize_t start[], hsize_t count[],
                       hsize_t stride[], hsize_t block[]);
static void verify_complete_dataset(hid_t loc, const char *dset_name);
static void verify_partial_dataset(hid_t loc, const char *dset_name,
                                   hsize_t start[], hsize_t count[],
                                   hsize_t stride[], hsize_t block[],
                                   int * buf, hsize_t buf_len);
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

int             nerrors = 0;        /* Errors count                         */
int             verbose = 0;        /* Verbose, default is no               */

static MPI_Comm SAP_Comm = MPI_COMM_NULL;           /* COMM for FPHDF5      */
static MPI_Comm SAP_Barrier_Comm = MPI_COMM_NULL;   /* COMM used in barriers*/

static hid_t    fapl = -1;          /* FPHDF5 file access property list     */
static int      mpi_rank;           /* Rank of this process                 */
static int      mpi_size;           /* Size of the COMM passed to FPHDF5    */
static int     *orig_data = NULL;   /* Data that's written to datasets      */
static int     *local_orig_data = NULL;  /* data that's written to datasets */
				   /* by this process.			    */

/* dimensions of file data space -- initialized in slab_set() */
static hsize_t dims[RANK] = {0, 0};

/* Hyperslab settings */
static hsize_t  start[RANK];
static hsize_t  count[RANK];
static hsize_t  stride[RANK];
static hsize_t  block[RANK];

static const char  *progname = "t_fphdf5";

/* Dataset Name Template */
static const char *dset_tmpl       = "Dataset %d";
static char        dset_name[128];

/* Group Name Template */
static const char *grp_tmpl        = "Process %d's Datasets";
static char        grp_name[128];

#if 0
/* A useful debugging function, but no need to compile it unless
 * we are going to use it.              JRM - 4/13/4
 */
/*-------------------------------------------------------------------------
 * Function:	check_globals
 * Purpose:	Debugging Function.  Check the current values of some
 *		globals, and generate a message when they change.
 * Return:	void
 * Programmer:	John Mainzer - 3/3/04
 * Modifications:
 *-------------------------------------------------------------------------
 */
static void
check_globals(char * location_name)
{
    static hsize_t local_dims[RANK] = {0,0};
    static hsize_t local_start[RANK] = {0,0};
    static hsize_t  local_count[RANK] = {0,0};
    static hsize_t  local_stride[RANK] = {0,0};
    static hsize_t  local_block[RANK] = {0,0};

    if ( ( dims[0] != local_dims[0] ) ||
         ( dims[1] != local_dims[1] ) ||
         ( start[0] != local_start[0] ) ||
         ( start[1] != local_start[1] ) ||
         ( count[0] != local_count[0] ) ||
         ( count[1] != local_count[1] ) ||
         ( stride[0] != local_stride[0] ) ||
         ( stride[1] != local_stride[1] ) ||
         ( block[0] != local_block[0] ) ||
         ( block[1] != local_block[1] ) ) {
       printf("%d: globals have changed at %s.\n old: dims=[%d,%d], start=[%d,%d], count=[%d, %d], stride=[%d,%d], block=[%d,%d]\n new: dims=[%d,%d], start=[%d,%d], count=[%d, %d], stride=[%d,%d], block=[%d,%d]\n",
               mpi_rank,
               location_name,
               (int)(local_dims[0]), (int)(local_dims[1]),
               (int)(local_start[0]), (int)(local_start[1]),
               (int)(local_count[0]), (int)(local_count[1]),
               (int)(local_stride[0]), (int)(local_stride[1]),
               (int)(local_block[0]), (int)(local_block[1]),
               (int)(dims[0]), (int)(dims[1]),
               (int)(start[0]), (int)(start[1]),
               (int)(count[0]), (int)(count[1]),
               (int)(stride[0]), (int)(stride[1]),
               (int)(block[0]), (int)(block[1]));
        local_dims[0] = dims[0];
        local_dims[1] = dims[1];
        local_start[0] = start[0];
        local_start[1] = start[1];
        local_count[0] = count[0];
        local_count[1] = count[1];
        local_stride[0] = stride[0];
        local_stride[1] = stride[1];
        local_block[0] = block[0];
        local_block[1] = block[1];
    }
    return;
} /* check_globals() */
#endif


#if 0
/* Another useful debugging function, again no need to compile it unless
 * we are going to use it.              JRM - 4/13/04
 */
/*-------------------------------------------------------------------------
 * Function:	print_globals
 * Purpose:	Debugging Function.  Display the current values of some
 *		globals.
 * Return:	void
 * Programmer:	John Mainzer - 3/9/04
 * Modifications:
 *-------------------------------------------------------------------------
 */
static void
print_globals(void)
{
    printf("%d: dims=[%d,%d], start=[%d,%d], count=[%d, %d], stride=[%d,%d], block=[%d,%d]\n",
               (int)mpi_rank,
               (int)(dims[0]), (int)(dims[1]),
               (int)(start[0]), (int)(start[1]),
               (int)(count[0]), (int)(count[1]),
               (int)(stride[0]), (int)(stride[1]),
               (int)(block[0]), (int)(block[1]));
    return;
} /* print_globals() */
#endif

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
    hid_t   fid = -1;

    fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
    VRFY((fid >= 0), "H5Fcreate");
    printf("%d: Created file %s\n", mpi_rank, filename);
    return fid;
}

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
create_group(hid_t loc, const char *group_name, size_t size_hint)
{
    hid_t   group;

    VRFY(((group = H5Gcreate(loc, group_name, size_hint)) >= 0), "H5Gcreate");
    printf("%d: Created group \"%s\"\n", mpi_rank, group_name);
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
 *		Altered function to use the global dims array, instead
 *		of a locally declared and initialized version.
 *						JRM - 3/3/04
 *-------------------------------------------------------------------------
 */
static hid_t
create_dset(hid_t loc, const char *dataset_name)
{
    hid_t   dset, sid;

    VRFY((dims[0] != 0), "dims array initialized.");

    VRFY(((sid = H5Screate_simple(RANK, dims, NULL)) >= 0), "H5Screate_simple");
    printf("%d: Created simple dataspace\n", mpi_rank);

    dset = H5Dcreate(loc, dataset_name, H5T_NATIVE_INT, sid, H5P_DEFAULT);
    VRFY((dset >= 0), "H5Dcreate");
    printf("%d: Created dataset \"%s\"\n", mpi_rank, dataset_name);

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
access_dset(hid_t loc, const char *dataset_name)
{
    hid_t   dataset;

    VRFY(((dataset = H5Dopen(loc, dataset_name)) >= 0), "H5Dopen");
    VRFY((H5Dclose(dataset) >= 0), "H5Dclose");
}

/*-------------------------------------------------------------------------
 * Function:	slab_set
 * Purpose:	Setup the dimensions of the hyperslab.
 * Return:	Nothing
 * Programmer:	Bill Wendling
 *              05. November 2003
 * Modifications:
 *		Modified function to initialize the dims array.  Also
 *		altered the initialization of block[0] so that the
 *		the test program will run with mpi_size > 6.
 *						JRM - 3/3/04
 *-------------------------------------------------------------------------
 */
static void
slab_set(hsize_t my_start[], hsize_t my_count[], hsize_t my_stride[], hsize_t my_block[])
{
    /* initialize dims according to the number of processes: */
    dims[0] = DIM0 * mpi_size;
    dims[1] = DIM1;

    /* Each process takes a slab of rows. */
    my_block[0] = DIM0;
    my_block[1] = DIM1;

    my_stride[0] = my_block[0];
    my_stride[1] = my_block[1];

    my_count[0] = 1;
    my_count[1] = 1;

    my_start[0] = mpi_rank * my_block[0];
    my_start[1] = 0;
}

/*-------------------------------------------------------------------------
 * Function:	fill_data
 * Purpose:	Fill data buffer with some data.
 * Return:	Nothing
 * Programmer:	Bill Wendling
 *              13. November 2003
 * Modifications:
 *		Complete re-write of function.  The orig_data array is
 *		now allocated (in main) with size equal the size of the
 *		array on file, and is loaded with the data we expect to
 *		find there.
 *
 *		The new local_orig_data array is allocated to match the
 *		size of this processes contribution to the on file data
 *		set, and is loaded with this processes data.
 *
 *		Note how mpi_rank, row, and column are encoded in each
 *		cell of the arrays.
 *							JRM - 3/8/04
 *-------------------------------------------------------------------------
 */
static void
fill_data(void)
{
    int col;
    int row;
    int offset = 0;
    int local_offset = 0;
    int proc_num = -1;

    for ( proc_num = 0; proc_num < mpi_size; proc_num++ ) {
        for ( row = 0 ; row < DIM0; row++ ) {
            if ( proc_num == SAP_RANK ) {
                for ( col = 0; col < DIM1; col++ ) {
                    /* The SAP doesn't write to file, so its section */
                    /* of the matrix will be filled with zeros.      */
                    orig_data[offset++] = 0;
                }
            } else if ( proc_num == mpi_rank ) {
                for ( col = 0; col < DIM1; col++ ) {
                    local_orig_data[local_offset++] =
                        orig_data[offset++] =
                            (proc_num * 1000) + (row * 100) + col;
                }
            } else {
                for ( col = 0; col < DIM1; col++ ) {
                    orig_data[offset++] = (proc_num * 1000) + (row * 100) + col;
                }
            }
        }
    }

    VRFY((offset == (mpi_size * DIM0 * DIM1)), "offset OK");
    VRFY((local_offset == (DIM0 * DIM1)), "local_offset OK");

    return;

} /* fill_data() */

/*-------------------------------------------------------------------------
 * Function:	write_data
 * Purpose:	Writes data to a dataset.
 * Return:	Nothing, but aborts if an error occurs.
 * Programmer:	Bill Wendling
 *              29. October 2003
 * Modifications:
 *		Modified to use local_orig_data, instead of initializing
 *		a portion of orig_data.  Also removed some orphan code.
 *							JRM - 3/8/04
 *-------------------------------------------------------------------------
 */
static void
write_data(hid_t loc, const char *dataset_name, hsize_t my_start[], hsize_t my_count[],
           hsize_t my_stride[], hsize_t my_block[])
{
    herr_t      hrc;
    hid_t       file_dataspace, mem_dataspace;
    hid_t       dataset;

    /* See if dataset is there */
    VRFY(((dataset = H5Dopen(loc, dataset_name)) >= 0), "H5Dopen");

    file_dataspace = H5Dget_space(dataset);
    VRFY((file_dataspace >= 0), "H5Dget_space");

    hrc = H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET,
                              my_start, my_stride, my_count, my_block);
#if 0
    /* some debugging code we may want to keep for a time.  JRM - 4/13/04 */
    if ( hrc < 0 ) { /* dump the parameters */
        printf("%d: start=[%d,%d], count=[%d, %d], stride=[%d,%d], block=[%d,%d]\n",
               mpi_rank,
               (int)(my_start[0]), (int)(my_start[1]),
               (int)(my_count[0]), (int)(my_count[1]),
               (int)(my_stride[0]), (int)(my_stride[1]),
               (int)(my_block[0]), (int)(my_block[1]));
    }
#endif
    VRFY((hrc >= 0), "H5Sselect_hyperslab in write_data");

    /* create a memory dataspace independently */
    mem_dataspace = H5Screate_simple(RANK, my_block, NULL);
    VRFY((mem_dataspace >= 0), "H5Screate_simple");

    hrc = H5Dwrite(dataset, H5T_NATIVE_INT, mem_dataspace,
                   file_dataspace, H5P_DEFAULT, local_orig_data);
    VRFY((hrc >= 0), "H5Dwrite");

    VRFY((H5Sclose(mem_dataspace) >= 0), "H5Sclose");
    VRFY((H5Sclose(file_dataspace) >= 0), "H5Sclose");
    VRFY((H5Dclose(dataset) >= 0), "H5Dclose");

} /* write_data() */

/*-------------------------------------------------------------------------
 * Function:	verify_complete_dataset
 * Purpose:	Verify that all the data in the dataset is correct --
 *		including that written by other processes.
 * Return:	Nothing
 * Programmer:	John Mainzer
 *              3/8/04
 * Modifications:
 *		None.
 *-------------------------------------------------------------------------
 */
static void
verify_complete_dataset(hid_t loc, const char *dataset_name)
{
    hid_t       dataset;
    int        *data_array = NULL;
    size_t      data_array_len = 0;
    unsigned	col;
    unsigned	row;
    int		offset = 0;
    int         vrfyerrs = 0;

    /* Open the dataset */
    VRFY(((dataset = H5Dopen(loc, dataset_name)) >= 0), "H5Dopen");

    /* allocate a buffer to receive the contents of the file dataset */
    VRFY((dims[0] != 0), "dims array initialized.");
    data_array_len = ((size_t)dims[0]) * ((size_t)dims[1]) * sizeof(int);
    data_array = (int *)malloc(data_array_len);
    VRFY((data_array != NULL), "data_array allocated.");

    /* Read the dataset */
    VRFY((H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL,
                  H5P_DEFAULT, data_array) >= 0), "H5Dread");


    /* Verify the contents of the dataset */
    for ( row = 0; row < dims[0]; row++ ) {
        for ( col = 0; col < DIM1; col++ ) {
            if ( data_array[offset] != orig_data[offset] ) {
                if ( vrfyerrs++ < MAX_ERR_REPORT ) {
		    fprintf(stdout, "%d: Dataset Verify failed at "
                            "row %u, col %u: expect %d, got %d\n", mpi_rank,
                            row, col, orig_data[offset], data_array[offset]);
                }
            }
            offset++;
        }
    }
    VRFY((offset == (mpi_size * DIM0 * DIM1)), "offset OK");

    if (vrfyerrs) {
        fprintf(stdout, "%d: %d errors found in verify_complete_dataset\n",
                mpi_rank, vrfyerrs);
        ++nerrors;
    }

    VRFY((H5Dclose(dataset) >= 0), "H5Dclose");

    if ( data_array != NULL ) {
        free(data_array);
    }

    return;

} /* verify_complete_dataset() */

/*-------------------------------------------------------------------------
 * Function:	verify_partial_dataset
 * Purpose:	Verify that the data in the specified section of the
 *		dataset matches the contents of the provided buffer.
 * Return:	Nothing
 * Programmer:	John Mainzer
 *              3/8/04
 * Modifications:
 *		None.
 *-------------------------------------------------------------------------
 */
static void
verify_partial_dataset(hid_t loc, const char *dataset_name,
                       hsize_t my_start[], hsize_t my_count[],
                       hsize_t my_stride[], hsize_t my_block[],
                       int * buf, hsize_t buf_len)
{
    hid_t       dataset, file_dataspace, mem_dataspace;
    int        *data_array;
    unsigned	col;
    unsigned	row;
    hsize_t	offset = 0;
    int         vrfyerrs = 0;

    /* Open the dataset */
    VRFY(((dataset = H5Dopen(loc, dataset_name)) >= 0), "H5Dopen");

    /* Create a file dataspace */
    file_dataspace = H5Dget_space(dataset);
    VRFY((file_dataspace >= 0), "H5Dget_space");
    VRFY((H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET,
                              my_start, my_stride, my_count, my_block) >= 0),
         "H5Sselect_hyperslab in verify_partial_dataset");

    /* Create a memory dataspace */
    mem_dataspace = H5Screate_simple(RANK, my_block, NULL);
    VRFY((mem_dataspace >= 0), "H5Screate_simple");
    VRFY(((block[0] * block[1]) == buf_len), "buf_len matches.");

    /* Read the dataset */
    VRFY((dims[0] != 0), "dims array initialized.");
    data_array = (int *)malloc(((size_t)block[0]) *
                               ((size_t)block[1]) *
                               sizeof(int));
    VRFY((H5Dread(dataset, H5T_NATIVE_INT, mem_dataspace, file_dataspace,
                  H5P_DEFAULT, data_array) >= 0), "H5Dread");

    /* Verify the contents of the dataset */
    for ( row = 0; row < block[0]; row++ ) {
        for ( col = 0; col < block[1]; col++ ) {
            if ( data_array[offset] != buf[offset] ) {
                if ( vrfyerrs++ < MAX_ERR_REPORT ) {
		    fprintf(stdout, "%d: Dataset Verify failed at "
                            "row %u, col %u: expected %d, got %d\n", mpi_rank,
                            row, col, buf[offset], data_array[offset]);
                }
            }
            offset++;
        }
    }
    VRFY((offset == buf_len), "offset OK");

    if (vrfyerrs) {
	fprintf(stdout, "%d: %d errors found in verify_partial_dataset\n",
                mpi_rank, vrfyerrs);
        ++nerrors;
    }

    VRFY((H5Sclose(mem_dataspace) >= 0), "H5Sclose");
    VRFY((H5Sclose(file_dataspace) >= 0), "H5Sclose");
    VRFY((H5Dclose(dataset) >= 0), "H5Dclose");

    if ( data_array != NULL ) {
        free(data_array);
    }

    return;

} /* verify_partial_dataset() */

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
        int     i;

        for (i = 0; i < mpi_size; ++i)
            if (i != SAP_RANK) {
                hid_t   group;

                sprintf(grp_name, grp_tmpl, i);
                group = create_group(loc, grp_name, 4);
                VRFY((H5Gclose(group) >= 0), "H5Gclose");
            }
    }

    SYNC(SAP_Barrier_Comm);

} /* test_group_creation() */

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
} /* test_dataset_creation() */

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
    int     i;

    for (i = 0; i < mpi_size; ++i)
        if (i != SAP_RANK) {
            hid_t   group;

            sprintf(grp_name, grp_tmpl, i);
            VRFY(((group = H5Gopen(loc, grp_name)) >= 0), "H5Gopen");

            sprintf(dset_name, dset_tmpl, 0);
            printf("%d: Accessing dataset \"%s/%s\"\n", mpi_rank, grp_name, dset_name);
            access_dset(group, dset_name);

            sprintf(dset_name, dset_tmpl, 1);
            printf("%d: Accessing dataset \"%s/%s\"\n", mpi_rank, grp_name, dset_name);
            access_dset(group, dset_name);

            sprintf(dset_name, dset_tmpl, 2);
            printf("%d: Accessing dataset \"%s/%s\"\n", mpi_rank, grp_name, dset_name);
            access_dset(group, dset_name);

            sprintf(dset_name, dset_tmpl, 3);
            printf("%d: Accessing dataset \"%s/%s\"\n", mpi_rank, grp_name, dset_name);
            access_dset(group, dset_name);

            VRFY((H5Gclose(group) >= 0), "H5Gclose");
        }

    SYNC(SAP_Barrier_Comm);
} /* test_dataset_access() */

/*-------------------------------------------------------------------------
 * Function:	test_dataset_write
 * Purpose:	Test that we can write to the datasets in the file from
 *              all processes simultaneously.
 * Return:	Nothing
 * Programmer:	Bill Wendling
 *              11. November 2003
 * Modifications:
 *		Replaced calls to verify_dataset() with calls to
 *		verify_partial_dataset().  In the absence of a barrier,
 *		we don't know that the other processes have done their
 *		writes to the datasets as well.  Thus we only check the
 *		data we have written for now.
 *
 *		Also re-formatted code in passing.	JRM - 3/8/04
 *-------------------------------------------------------------------------
 */
static void
test_dataset_write(hid_t loc)
{
    int     i;

    /*===-------------------------------------------------------------------===
     * All processes write to each dataset.
     *===-------------------------------------------------------------------===
     */
    for (i = 0; i < mpi_size; ++i) {
        if (i != SAP_RANK) {
            hid_t   group;

            sprintf(grp_name, grp_tmpl, i);
            VRFY(((group = H5Gopen(loc, grp_name)) >= 0), "H5Gopen");

            /* Write to this dataset */
            sprintf(dset_name, dset_tmpl, 0);
            printf("%d: Writing to \"/%s/%s\"\n",
                   mpi_rank, grp_name, dset_name);
            write_data(group, dset_name, start, count, stride, block);
            printf("%d: Verifying dataset \"/%s/%s\"\n",
                   mpi_rank, grp_name, dset_name);
            verify_partial_dataset(group, dset_name,
                                   start, count, stride, block,
                                   local_orig_data, (block[0] * block[1]));

            sprintf(dset_name, dset_tmpl, 1);
            printf("%d: Writing to \"/%s/%s\"\n",
                   mpi_rank, grp_name, dset_name);
            write_data(group, dset_name, start, count, stride, block);
            printf("%d: Verifying dataset \"/%s/%s\"\n",
                   mpi_rank, grp_name, dset_name);
            verify_partial_dataset(group, dset_name,
                                   start, count, stride, block,
                                   local_orig_data, (block[0] * block[1]));

            sprintf(dset_name, dset_tmpl, 2);
            printf("%d: Writing to \"/%s/%s\"\n",
                   mpi_rank, grp_name, dset_name);
            write_data(group, dset_name, start, count, stride, block);
            printf("%d: Verifying dataset \"/%s/%s\"\n",
                   mpi_rank, grp_name, dset_name);
            verify_partial_dataset(group, dset_name,
                                   start, count, stride, block,
                                   local_orig_data, (block[0] * block[1]));

            sprintf(dset_name, dset_tmpl, 3);
            printf("%d: Writing to \"/%s/%s\"\n",
                   mpi_rank, grp_name, dset_name);
            write_data(group, dset_name, start, count, stride, block);
            printf("%d: Verifying dataset \"/%s/%s\"\n",
                   mpi_rank, grp_name, dset_name);
            verify_partial_dataset(group, dset_name,
                                   start, count, stride, block,
                                   local_orig_data, (block[0] * block[1]));

            /* Close the group */
            VRFY((H5Gclose(group) >= 0), "H5Gclose");
        }
    }
} /* test_dataset_write() */

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
 *		Moved the malloc of orig_data to just after the call to
 *		slab_set(), and modified the call to use the global dims
 *		array instead of the DIM0 and DIM1 constants.
 *						JRM - 3/3/04
 *
 *		Added the allocation and deletion of local_orig_data.
 *		This array is used to store the data written by this
 *		process.			JRM - 3/5/04
 *
 *		Replaced calls to verify_dataset() with calls to
 *		verify_complete_dataset().	JRM - 3/8/04
 *-------------------------------------------------------------------------
 */
int
main(int argc, char *argv[])
{
    hid_t   fid;
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
        printf("%d: Created access property list\n", mpi_rank);

        hrc = H5Pset_fapl_fphdf5(fapl, SAP_Comm, SAP_Barrier_Comm,
                                 MPI_INFO_NULL, (unsigned)SAP_RANK);
        VRFY((fapl >= 0), "H5Pset_fapl_fphdf5");
        printf("%d: Set access property list\n", mpi_rank);

        slab_set(start, count, stride, block);

        VRFY((dims[0] != 0), "dims array initialized.");
        orig_data = (int *)malloc(((size_t)dims[0]) *
                                  ((size_t)dims[1]) *
                                  sizeof(int));
        VRFY((orig_data != NULL), "orig_data malloc succeeded");

        VRFY((block[0] != 0), "block array initialized.");
        local_orig_data = (int *)malloc(((size_t)block[0]) *
                                        ((size_t)block[1]) *
                                        sizeof(int));
        VRFY((orig_data != NULL), "local_orig_data malloc succeeded");

        fill_data();

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
            printf("%d: Closed file\n", mpi_rank);

            fid = H5Fopen(filenames[i], H5F_ACC_RDONLY, fapl);
            VRFY((fid >= 0), "H5Fopen");
            SYNC(SAP_Barrier_Comm);

            /*===------------------------------------------------------------===
             * Reverify that the data is still "correct"
             *===------------------------------------------------------------===
             */
            for (i = 0; i < (unsigned)mpi_size; ++i)
                if (i != SAP_RANK) {
                    hid_t   group;

                    sprintf(grp_name, grp_tmpl, i);
                    VRFY(((group = H5Gopen(fid, grp_name)) >= 0), "H5Gopen");


                    /* Write to this dataset */
                    sprintf(dset_name, dset_tmpl, 0);
                    printf("%d: Reverifying dataset \"/%s/%s\"\n", mpi_rank,
                           grp_name, dset_name);
                    verify_complete_dataset(group, dset_name);

                    sprintf(dset_name, dset_tmpl, 1);
                    printf("%d: Reverifying dataset \"/%s/%s\"\n", mpi_rank,
                           grp_name, dset_name);
                    verify_complete_dataset(group, dset_name);

                    sprintf(dset_name, dset_tmpl, 2);
                    printf("%d: Reverifying dataset \"/%s/%s\"\n", mpi_rank,
                           grp_name, dset_name);
                    verify_complete_dataset(group, dset_name);

                    sprintf(dset_name, dset_tmpl, 3);
                    printf("%d: Reverifying dataset \"/%s/%s\"\n", mpi_rank,
                           grp_name, dset_name);
                    verify_complete_dataset(group, dset_name);

                    /* Close the group */
		    printf("%d: Closing group.", mpi_rank);
                    VRFY((H5Gclose(group) >= 0), "H5Gclose");
                }

            SYNC(SAP_Barrier_Comm);
            VRFY((H5Fclose(fid) >= 0), "H5Fclose");
        }

        if ( orig_data != NULL ) {
       	    free(orig_data);
	}
	if ( local_orig_data != NULL ) {
	    free(local_orig_data);
	}
#if 1
        /* It is useful to keep the hdf file created by this test for
         * debugging purposes.  However, this code should always be
         * turned on for checkin.                 JRM - 4/13/04
         */
        if (fapl > -1)
            h5_cleanup(FILENAME, fapl);
#endif
    }

    VRFY((H5FPfinalize() >= 0), "H5FPfinalize");
    printf("%d: H5FP finalized\n", mpi_rank);

    if (MAINPROCESS) {  /* only process 0 reports */
        fprintf(stderr, "===================================\n");

        if (nerrors)
            fprintf(stderr, "***FPHDF5 test detected %d errors***\n", nerrors);
        else
            fprintf(stderr, "FPHDF5 test finished with no errors\n");

        fprintf(stderr, "===================================\n");
    }

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
        printf("No t_fphdf5 test because FPHDF5 is not configured in\n");

    MPI_Finalize();
    return 0;
}
#endif  /* H5_HAVE_FPHDF5 */
