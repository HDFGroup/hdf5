/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Programmer: Jordan Henderson
 *             01/31/2017
 *
 * This file contains tests for writing to and reading from
 * datasets in parallel with filters applied to the data.
 */

#include "tfilters_parallel.h"

#define ARRAY_SIZE(a) sizeof(a) / sizeof(a[0])

static int test_one_chunk_filtered_dataset(void);
static int test_filtered_dataset_no_overlap(void);
static int test_filtered_dataset_overlap(void);
static int test_filtered_dataset_single_no_selection(void);
static int test_filtered_dataset_all_no_selection(void);
static int test_filtered_dataset_point_selection(void);
static int test_filtered_dataset_interleaved_write(void);
static int test_3d_filtered_dataset_no_overlap_separate_pages(void);
static int test_3d_filtered_dataset_no_overlap_same_pages(void);
static int test_3d_filtered_dataset_overlap(void);
static int test_32d_filtered_dataset_no_overlap_separate_pages(void);
static int test_32d_filtered_dataset_no_overlap_same_pages(void);
static int test_32d_filtered_dataset_overlap(void);
static int test_cmpd_filtered_dataset_no_conversion_unshared(void);
static int test_cmpd_filtered_dataset_no_conversion_shared(void);
static int test_cmpd_filtered_dataset_type_conversion_unshared(void);
static int test_cmpd_filtered_dataset_type_conversion_shared(void);
static int test_write_serial_read_parallel(void);
static int test_write_parallel_read_serial(void);

static int read_outfile(char* filename, char* datasetname);

static MPI_Comm comm = MPI_COMM_WORLD;
static MPI_Info info = MPI_INFO_NULL;
static int      mpi_rank;
static int      mpi_size;

static int (*tests[])(void) = {
        test_one_chunk_filtered_dataset,
        test_filtered_dataset_no_overlap,
        test_filtered_dataset_overlap,
        test_filtered_dataset_single_no_selection,
        test_filtered_dataset_all_no_selection,
        test_filtered_dataset_point_selection,
        test_filtered_dataset_interleaved_write,
        test_3d_filtered_dataset_no_overlap_separate_pages,
        test_3d_filtered_dataset_no_overlap_same_pages,
        test_3d_filtered_dataset_overlap,
        test_32d_filtered_dataset_no_overlap_separate_pages,
        test_32d_filtered_dataset_no_overlap_same_pages,
        test_32d_filtered_dataset_overlap,
        test_cmpd_filtered_dataset_no_conversion_unshared,
        test_cmpd_filtered_dataset_no_conversion_shared,
        test_cmpd_filtered_dataset_type_conversion_unshared,
        test_cmpd_filtered_dataset_type_conversion_shared,
        test_write_serial_read_parallel,
        test_write_parallel_read_serial,
};

/*
 * Tests parallel write of filtered data in the special
 * case where a dataset is composed of a single chunk.
 *
 * Programmer: Jordan Henderson
 *             02/01/2017
 */
static int
test_one_chunk_filtered_dataset()
{
    C_DATATYPE *data = NULL;
    hsize_t     dataset_dims[ONE_CHUNK_FILTERED_DATASET_DIMS];
    hsize_t     chunk_dims[ONE_CHUNK_FILTERED_DATASET_DIMS];
    hsize_t     sel_dims[ONE_CHUNK_FILTERED_DATASET_DIMS];
    hsize_t     count[ONE_CHUNK_FILTERED_DATASET_DIMS];
    hsize_t     stride[ONE_CHUNK_FILTERED_DATASET_DIMS];
    hsize_t     block[ONE_CHUNK_FILTERED_DATASET_DIMS];
    hsize_t     offset[ONE_CHUNK_FILTERED_DATASET_DIMS];
    size_t      i, data_size;
    hid_t       file_id, dset_id, plist_id;
    hid_t       filespace, memspace;
    int         ret_value = 0;

    if (mpi_rank == 0) puts("Testing one-chunk filtered dataset");

    if ((plist_id = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        goto error;
    if (H5Pset_fapl_mpio(plist_id, comm, info) < 0)
        goto error;
    if (H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
        goto exit;

    if ((file_id = H5Fopen(FILENAME, H5F_ACC_RDWR, plist_id)) < 0)
        goto error;
    if (H5Pclose(plist_id) < 0)
        goto error;

    dataset_dims[0] = ONE_CHUNK_FILTERED_DATASET_NROWS;
    dataset_dims[1] = ONE_CHUNK_FILTERED_DATASET_NCOLS;
    chunk_dims[0] = ONE_CHUNK_FILTERED_DATASET_CH_NROWS;
    chunk_dims[1] = ONE_CHUNK_FILTERED_DATASET_CH_NCOLS;
    sel_dims[0] = ONE_CHUNK_FILTERED_DATASET_NROWS / NUM_MPI_RANKS;
    sel_dims[1] = ONE_CHUNK_FILTERED_DATASET_NCOLS;

    if ((filespace = H5Screate_simple(ONE_CHUNK_FILTERED_DATASET_DIMS, dataset_dims, NULL)) < 0)
        goto error;
    if ((memspace = H5Screate_simple(ONE_CHUNK_FILTERED_DATASET_DIMS, sel_dims, NULL)) < 0)
        goto error;

    if ((plist_id = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        goto error;

    if (H5Pset_chunk(plist_id, ONE_CHUNK_FILTERED_DATASET_DIMS, chunk_dims) < 0)
        goto error;

    if (SET_FILTER(plist_id) < 0)
        goto error;

    if ((dset_id = H5Dcreate(file_id, ONE_CHUNK_FILTERED_DATASET_NAME, HDF5_DATATYPE_NAME, filespace,
            H5P_DEFAULT, plist_id, H5P_DEFAULT)) < 0)
        goto error;
    if (H5Pclose(plist_id) < 0)
        goto error;
    if (H5Sclose(filespace) < 0)
        goto error;

    count[0] = 1;
    count[1] = 1;
    stride[0] = ONE_CHUNK_FILTERED_DATASET_CH_NROWS;
    stride[1] = ONE_CHUNK_FILTERED_DATASET_CH_NCOLS;
    block[0] = sel_dims[0];
    block[1] = sel_dims[1];
    offset[0] = (mpi_rank * sel_dims[0]);
    offset[1] = 0;

    printf("Process %d is writing with count[ %llu, %llu ], stride[ %llu, %llu ], offset[ %llu, %llu ], block size[ %llu, %llu ]\n",
            mpi_rank, count[0], count[1], stride[0], stride[1], offset[0], offset[1], block[0], block[1]);

    if ((filespace = H5Dget_space(dset_id)) < 0)
        goto error;
    if (H5Sselect_hyperslab(filespace, H5S_SELECT_SET, offset, stride, count, block) < 0)
        goto error;

    data_size = ONE_CHUNK_FILTERED_DATASET_CH_NROWS * ONE_CHUNK_FILTERED_DATASET_NCOLS * sizeof(*data);

    if (NULL == (data = malloc(data_size))) {
        fprintf(stderr, "Couldn't allocate memory.\n");
        goto error;
    }

    for (i = 0; i < data_size / sizeof(*data); i++)
        data[i] = GEN_DATA(i);

    if ((plist_id = H5Pcreate(H5P_DATASET_XFER)) < 0)
        goto error;
    if (H5Pset_dxpl_mpio(plist_id, H5FD_MPIO_COLLECTIVE) < 0)
        goto error;

    if (H5Dwrite(dset_id, HDF5_DATATYPE_NAME, memspace, filespace, plist_id, data) < 0)
        goto error;

    goto exit;

error:
    if (mpi_rank == 0) puts("*** ONE-CHUNK FILTERED DATASET TEST FAILED ***");

    ret_value = 1;

exit:
    if (data)
        free(data);
    if (H5Dclose(dset_id) < 0)
        fprintf(stderr, "Unable to close dataset\n");
    if (H5Sclose(filespace) < 0)
        fprintf(stderr, "Unable to close filespace\n");
    if (H5Sclose(memspace) < 0)
        fprintf(stderr, "Unable to close memspace\n");
    if (H5Pclose(plist_id) < 0)
        fprintf(stderr, "Unable to close plist\n");
    if (H5Fclose(file_id) < 0)
        fprintf(stderr, "Unable to close file\n");

    return ret_value;
}

/*
 * Tests parallel write of filtered data in the case where only
 * one process is writing to a particular chunk in the operation.
 * In this case, the write operation can be optimized because
 * chunks do not have to be redistributed to new owners.
 *
 * Programmer: Jordan Henderson
 *             02/01/2017
 */
static int
test_filtered_dataset_no_overlap()
{
    C_DATATYPE *data = NULL;
    hsize_t     dataset_dims[UNSHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     chunk_dims[UNSHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     sel_dims[UNSHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     count[UNSHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     stride[UNSHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     block[UNSHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     offset[UNSHARED_FILTERED_CHUNKS_DATASET_DIMS];
    size_t      i, data_size;
    hid_t       file_id, dset_id, plist_id;
    hid_t       filespace, memspace;
    int         ret_value = 0;

    if (mpi_rank == 0) puts("Testing write to unshared filtered chunks");

    /* Set up file access property list with parallel I/O access */
    if ((plist_id = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        goto error;
    if (H5Pset_fapl_mpio(plist_id, comm, info) < 0)
        goto error;
    if (H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
        goto exit;

    if ((file_id = H5Fopen(FILENAME, H5F_ACC_RDWR, plist_id)) < 0)
        goto error;
    if (H5Pclose(plist_id) < 0)
        goto error;

    /* Create the dataspace for the dataset */
    dataset_dims[0] = UNSHARED_FILTERED_CHUNKS_NROWS;
    dataset_dims[1] = UNSHARED_FILTERED_CHUNKS_NCOLS;
    chunk_dims[0] = UNSHARED_FILTERED_CHUNKS_CH_NROWS;
    chunk_dims[1] = UNSHARED_FILTERED_CHUNKS_CH_NCOLS;
    sel_dims[0] = UNSHARED_FILTERED_CHUNKS_CH_NROWS;
    sel_dims[1] = UNSHARED_FILTERED_CHUNKS_NCOLS;

    if ((filespace = H5Screate_simple(UNSHARED_FILTERED_CHUNKS_DATASET_DIMS, dataset_dims, NULL)) < 0)
        goto error;
    if ((memspace = H5Screate_simple(UNSHARED_FILTERED_CHUNKS_DATASET_DIMS, sel_dims, NULL)) < 0)
        goto error;

    /* Create chunked dataset */
    if ((plist_id = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        goto error;
    if (H5Pset_chunk(plist_id, UNSHARED_FILTERED_CHUNKS_DATASET_DIMS, chunk_dims) < 0)
        goto error;

    /* Add test filter to the pipeline */
    if (SET_FILTER(plist_id) < 0)
        goto error;

    if ((dset_id = H5Dcreate(file_id, UNSHARED_FILTERED_CHUNKS_DATASET_NAME, HDF5_DATATYPE_NAME, filespace,
            H5P_DEFAULT, plist_id, H5P_DEFAULT)) < 0)
        goto error;
    if (H5Pclose(plist_id) < 0)
        goto error;
    if (H5Sclose(filespace) < 0)
        goto error;

    /* Each process defines the dataset selection in memory and writes
     * it to the hyperslab in the file
     */
    count[0] = 1;
    count[1] = UNSHARED_FILTERED_CHUNKS_NCOLS / UNSHARED_FILTERED_CHUNKS_CH_NCOLS;
    stride[0] = UNSHARED_FILTERED_CHUNKS_CH_NROWS;
    stride[1] = UNSHARED_FILTERED_CHUNKS_CH_NCOLS;
    block[0] = UNSHARED_FILTERED_CHUNKS_CH_NROWS;
    block[1] = UNSHARED_FILTERED_CHUNKS_CH_NCOLS;
    offset[0] = (mpi_rank * UNSHARED_FILTERED_CHUNKS_CH_NROWS * count[0]);
    offset[1] = 0;

    printf("Process %d is writing with count[ %llu, %llu ], stride[ %llu, %llu ], offset[ %llu, %llu ], block size[ %llu, %llu ]\n",
            mpi_rank, count[0], count[1], stride[0], stride[1], offset[0], offset[1], block[0], block[1]);

    /* Select hyperslab in the file */
    if ((filespace = H5Dget_space(dset_id)) < 0)
        goto error;
    if (H5Sselect_hyperslab(filespace, H5S_SELECT_SET, offset, stride, count, block) < 0)
        goto error;

    /* Fill data buffer */
    data_size = sel_dims[0] * sel_dims[1] * sizeof(*data);

    if (NULL == (data = malloc(data_size))) {
        fprintf(stderr, "Couldn't allocate memory.\n");
        goto error;
    }

    for (i = 0; i < data_size / sizeof(*data); i++)
        data[i] = GEN_DATA(i);

    /* Create property list for collective dataset write */
    if ((plist_id = H5Pcreate(H5P_DATASET_XFER)) < 0)
        goto error;
    if (H5Pset_dxpl_mpio(plist_id, H5FD_MPIO_COLLECTIVE) < 0)
        goto error;

    if (H5Dwrite(dset_id, HDF5_DATATYPE_NAME, memspace, filespace, plist_id, data) < 0)
        goto error;

    goto exit;

error:
    if (mpi_rank == 0) puts("*** UNSHARED FILTERED CHUNKS WRITE TEST FAILED ***");

    ret_value = 1;

exit:
    if (data)
        free(data);
    if (H5Dclose(dset_id) < 0)
        fprintf(stderr, "Unable to close dataset\n");
    if (H5Sclose(filespace) < 0)
        fprintf(stderr, "Unable to close filespace\n");
    if (H5Sclose(memspace) < 0)
        fprintf(stderr, "Unable to close memspace\n");
    if (H5Pclose(plist_id) < 0)
        fprintf(stderr, "Unable to close plist\n");
    if (H5Fclose(file_id) < 0)
        fprintf(stderr, "Unable to close file\n");

    return ret_value;
}

/*
 * Tests parallel write of filtered data in the case where
 * more than one process is writing to a particular chunk
 * in the operation. In this case, the chunks have to be
 * redistributed before the operation so that only one process
 * writes to a particular chunk.
 *
 * Programmer: Jordan Henderson
 *             02/01/2017
 */
static int
test_filtered_dataset_overlap()
{
    C_DATATYPE *data = NULL;
    hsize_t     dataset_dims[SHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     chunk_dims[SHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     sel_dims[SHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     count[SHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     stride[SHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     block[SHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     offset[SHARED_FILTERED_CHUNKS_DATASET_DIMS];
    size_t      i, data_size;
    hid_t       file_id, dset_id, plist_id;
    hid_t       filespace, memspace;
    int         ret_value = 0;

    if (mpi_rank == 0) puts("Testing write to shared filtered chunks");

    /* Set up file access property list with parallel I/O access */
    if ((plist_id = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        goto error;
    if (H5Pset_fapl_mpio(plist_id, comm, info) < 0)
        goto error;
    if (H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
        goto exit;

    if ((file_id = H5Fopen(FILENAME, H5F_ACC_RDWR, plist_id)) < 0)
        goto error;
    if (H5Pclose(plist_id) < 0)
        goto error;

    /* Create the dataspace for the dataset */
    dataset_dims[0] = SHARED_FILTERED_CHUNKS_NROWS;
    dataset_dims[1] = SHARED_FILTERED_CHUNKS_NCOLS;
    chunk_dims[0] = SHARED_FILTERED_CHUNKS_CH_NROWS;
    chunk_dims[1] = SHARED_FILTERED_CHUNKS_CH_NCOLS;
    sel_dims[0] = SHARED_FILTERED_CHUNKS_CH_NROWS;
    sel_dims[1] = SHARED_FILTERED_CHUNKS_NCOLS;

    if ((filespace = H5Screate_simple(SHARED_FILTERED_CHUNKS_DATASET_DIMS, dataset_dims, NULL)) < 0)
        goto error;
    if ((memspace = H5Screate_simple(SHARED_FILTERED_CHUNKS_DATASET_DIMS, sel_dims, NULL)) < 0)
        goto error;

    /* Create chunked dataset */
    if ((plist_id = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        goto error;
    if (H5Pset_chunk(plist_id, SHARED_FILTERED_CHUNKS_DATASET_DIMS, chunk_dims) < 0)
        goto error;

    /* Add test filter to the pipeline */
    if (SET_FILTER(plist_id) < 0)
        goto error;

    if ((dset_id = H5Dcreate(file_id, SHARED_FILTERED_CHUNKS_DATASET_NAME, HDF5_DATATYPE_NAME, filespace,
            H5P_DEFAULT, plist_id, H5P_DEFAULT)) < 0)
        goto error;
    if (H5Pclose(plist_id) < 0)
        goto error;
    if (H5Sclose(filespace) < 0)
        goto error;

    /* Each process defines the dataset selection in memory and writes
     * it to the hyperslab in the file
     */
    count[0] = SHARED_FILTERED_CHUNKS_NROWS / SHARED_FILTERED_CHUNKS_CH_NROWS;
    count[1] = SHARED_FILTERED_CHUNKS_NCOLS / SHARED_FILTERED_CHUNKS_CH_NCOLS;
    stride[0] = SHARED_FILTERED_CHUNKS_CH_NROWS;
    stride[1] = SHARED_FILTERED_CHUNKS_CH_NCOLS;
    block[0] = 1;
    block[1] = SHARED_FILTERED_CHUNKS_CH_NCOLS;
    offset[0] = mpi_rank;
    offset[1] = 0;

    printf("Process %d is writing with count[ %llu, %llu ], stride[ %llu, %llu ], offset[ %llu, %llu ], block size[ %llu, %llu ]\n",
            mpi_rank, count[0], count[1], stride[0], stride[1], offset[0], offset[1], block[0], block[1]);

    /* Select hyperslab in the file */
    if ((filespace = H5Dget_space(dset_id)) < 0)
        goto error;
    if (H5Sselect_hyperslab(filespace, H5S_SELECT_SET, offset, stride, count, block) < 0)
        goto error;

    /* Fill data buffer */
    data_size = sel_dims[0] * sel_dims[1] * sizeof(*data);

    if (NULL == (data = malloc(data_size))) {
        fprintf(stderr, "Couldn't allocate memory.\n");
        goto error;
    }

    for (i = 0; i < data_size / sizeof(*data); i++)
        data[i] = GEN_DATA(i);

    /* Create property list for collective dataset write */
    if ((plist_id = H5Pcreate(H5P_DATASET_XFER)) < 0)
        goto error;
    if (H5Pset_dxpl_mpio(plist_id, H5FD_MPIO_COLLECTIVE) < 0)
        goto error;

    if (H5Dwrite(dset_id, HDF5_DATATYPE_NAME, memspace, filespace, plist_id, data) < 0)
        goto error;

    goto exit;

error:
    if (mpi_rank == 0) puts("*** SHARED FILTERED CHUNKS WRITE TEST FAILED ***");

    ret_value = 1;

exit:
    if (data)
        free(data);
    if (H5Dclose(dset_id) < 0)
        fprintf(stderr, "Unable to close dataset\n");
    if (H5Sclose(filespace) < 0)
        fprintf(stderr, "Unable to close filespace\n");
    if (H5Sclose(memspace) < 0)
        fprintf(stderr, "Unable to close memspace\n");
    if (H5Pclose(plist_id) < 0)
        fprintf(stderr, "Unable to close plist\n");
    if (H5Fclose(file_id) < 0)
        fprintf(stderr, "Unable to close file\n");

    return ret_value;
}

/*
 * Tests parallel write of filtered data in the case where
 * a single process in the write operation has no selection
 * in the datasets dataspace. In this case, the process with
 * no selection still has to participate in the collective
 * space re-allocation for the filtered chunks and also must
 * participate in the re-insertion of the filtered chunks
 * into the chunk index.
 *
 * Programmer: Jordan Henderson
 *             02/01/2017
 */
static int
test_filtered_dataset_single_no_selection()
{
    C_DATATYPE *data = NULL;
    hsize_t     dataset_dims[SINGLE_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     chunk_dims[SINGLE_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     sel_dims[SINGLE_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     count[SINGLE_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     stride[SINGLE_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     block[SINGLE_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     offset[SINGLE_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    size_t      i, data_size;
    hid_t       file_id, dset_id, plist_id;
    hid_t       filespace, memspace;
    int         ret_value = 0;

    if (mpi_rank == 0) puts("Testing write to filtered chunks with a single process having no selection");

    /* Set up file access property list with parallel I/O access */
    if ((plist_id = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        goto error;
    if (H5Pset_fapl_mpio(plist_id, comm, info) < 0)
        goto error;
    if (H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
        goto exit;

    if ((file_id = H5Fopen(FILENAME, H5F_ACC_RDWR, plist_id)) < 0)
        goto error;
    if (H5Pclose(plist_id) < 0)
        goto error;

    /* Create the dataspace for the dataset */
    dataset_dims[0] = SINGLE_NO_SELECTION_FILTERED_CHUNKS_NROWS;
    dataset_dims[1] = SINGLE_NO_SELECTION_FILTERED_CHUNKS_NCOLS;
    chunk_dims[0] = SINGLE_NO_SELECTION_FILTERED_CHUNKS_CH_NROWS;
    chunk_dims[1] = SINGLE_NO_SELECTION_FILTERED_CHUNKS_CH_NCOLS;
    sel_dims[0] = SINGLE_NO_SELECTION_FILTERED_CHUNKS_CH_NROWS;
    sel_dims[1] = SINGLE_NO_SELECTION_FILTERED_CHUNKS_NCOLS;

    if (mpi_rank == SINGLE_NO_SELECTION_FILTERED_CHUNKS_NO_SELECT_PROC)
        sel_dims[0] = sel_dims[1] = 0;

    if ((filespace = H5Screate_simple(SINGLE_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS, dataset_dims, NULL)) < 0)
        goto error;
    if ((memspace = H5Screate_simple(SINGLE_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS, sel_dims, NULL)) < 0)
        goto error;

    /* Create chunked dataset */
    if ((plist_id = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        goto error;
    if (H5Pset_chunk(plist_id, SINGLE_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS, chunk_dims) < 0)
        goto error;

    /* Add test filter to the pipeline */
    if (SET_FILTER(plist_id) < 0)
        goto error;

    if ((dset_id = H5Dcreate(file_id, SINGLE_NO_SELECTION_FILTERED_CHUNKS_DATASET_NAME, HDF5_DATATYPE_NAME, filespace,
            H5P_DEFAULT, plist_id, H5P_DEFAULT)) < 0)
        goto error;
    if (H5Pclose(plist_id) < 0)
        goto error;
    if (H5Sclose(filespace) < 0)
        goto error;

    /* Each process defines the dataset selection in memory and writes
     * it to the hyperslab in the file
     */
    count[0] = 1;
    count[1] = SINGLE_NO_SELECTION_FILTERED_CHUNKS_NCOLS / SINGLE_NO_SELECTION_FILTERED_CHUNKS_CH_NCOLS;
    stride[0] = SINGLE_NO_SELECTION_FILTERED_CHUNKS_CH_NROWS;
    stride[1] = SINGLE_NO_SELECTION_FILTERED_CHUNKS_CH_NCOLS;
    block[0] = SINGLE_NO_SELECTION_FILTERED_CHUNKS_CH_NROWS;
    block[1] = SINGLE_NO_SELECTION_FILTERED_CHUNKS_CH_NCOLS;
    offset[0] = mpi_rank * SINGLE_NO_SELECTION_FILTERED_CHUNKS_CH_NROWS * count[0];
    offset[1] = 0;

    printf("Process %d is writing with count[ %llu, %llu ], stride[ %llu, %llu ], offset[ %llu, %llu ], block size[ %llu, %llu ]\n",
            mpi_rank, count[0], count[1], stride[0], stride[1], offset[0], offset[1], block[0], block[1]);

    /* Select hyperslab in the file */
    if ((filespace = H5Dget_space(dset_id)) < 0)
        goto error;

    if (mpi_rank == SINGLE_NO_SELECTION_FILTERED_CHUNKS_NO_SELECT_PROC) {
        if (H5Sselect_none(filespace) < 0)
            goto error;
    }
    else {
        if (H5Sselect_hyperslab(filespace, H5S_SELECT_SET, offset, stride, count, block) < 0)
            goto error;
    }

    /* Fill data buffer */
    data_size = sel_dims[0] * sel_dims[1] * sizeof(*data);

    if (NULL == (data = malloc(data_size))) {
        fprintf(stderr, "Couldn't allocate memory.\n");
        goto error;
    }

    for (i = 0; i < data_size / sizeof(*data); i++)
        data[i] = GEN_DATA(i);

    /* Create property list for collective dataset write */
    if ((plist_id = H5Pcreate(H5P_DATASET_XFER)) < 0)
        goto error;
    if (H5Pset_dxpl_mpio(plist_id, H5FD_MPIO_COLLECTIVE) < 0)
        goto error;

    if (H5Dwrite(dset_id, HDF5_DATATYPE_NAME, memspace, filespace, plist_id, data) < 0)
        goto error;

    goto exit;

error:
    if (mpi_rank == 0) puts("*** SINGLE NO SELECTION FILTERED CHUNKS WRITE TEST FAILED ***");

    ret_value = 1;

exit:
    if (data)
        free(data);
    if (H5Dclose(dset_id) < 0)
        fprintf(stderr, "Unable to close dataset\n");
    if (H5Sclose(filespace) < 0)
        fprintf(stderr, "Unable to close filespace\n");
    if (H5Sclose(memspace) < 0)
        fprintf(stderr, "Unable to close memspace\n");
    if (H5Pclose(plist_id) < 0)
        fprintf(stderr, "Unable to close plist\n");
    if (H5Fclose(file_id) < 0)
        fprintf(stderr, "Unable to close file\n");

    return ret_value;
}

/*
 * Tests parallel write of filtered data in the case
 * where no process in the write operation has a
 * selection in the datasets dataspace. This test is
 * to ensure that there are no assertion failures or
 * similar issues due to size 0 allocations and the
 * like. In this case, the file and dataset are created
 * but the dataset is populated with the default fill
 * value.
 *
 * Programmer: Jordan Henderson
 *             02/02/2017
 */
static int
test_filtered_dataset_all_no_selection()
{
    C_DATATYPE *data = NULL;
    hsize_t     dataset_dims[ALL_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     chunk_dims[ALL_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     sel_dims[ALL_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    size_t      i, data_size;
    hid_t       file_id, dset_id, plist_id;
    hid_t       filespace, memspace;
    int         ret_value = 0;

    if (mpi_rank == 0) puts("Testing write to filtered chunks with all processes having no selection");

    /* Set up file access property list with parallel I/O access */
    if ((plist_id = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        goto error;
    if (H5Pset_fapl_mpio(plist_id, comm, info) < 0)
        goto error;
    if (H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
        goto exit;

    if ((file_id = H5Fopen(FILENAME, H5F_ACC_RDWR, plist_id)) < 0)
        goto error;
    if (H5Pclose(plist_id) < 0)
        goto error;

    /* Create the dataspace for the dataset */
    dataset_dims[0] = ALL_NO_SELECTION_FILTERED_CHUNKS_NROWS;
    dataset_dims[1] = ALL_NO_SELECTION_FILTERED_CHUNKS_NCOLS;
    chunk_dims[0] = ALL_NO_SELECTION_FILTERED_CHUNKS_CH_NROWS;
    chunk_dims[1] = ALL_NO_SELECTION_FILTERED_CHUNKS_CH_NCOLS;
    sel_dims[0] = sel_dims[1] = 0;

    if ((filespace = H5Screate_simple(ALL_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS, dataset_dims, NULL)) < 0)
        goto error;
    if ((memspace = H5Screate_simple(ALL_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS, sel_dims, NULL)) < 0)
        goto error;

    /* Create chunked dataset */
    if ((plist_id = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        goto error;
    if (H5Pset_chunk(plist_id, ALL_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS, chunk_dims) < 0)
        goto error;

    /* Add test filter to the pipeline */
    if (SET_FILTER(plist_id) < 0)
        goto error;

    if ((dset_id = H5Dcreate(file_id, ALL_NO_SELECTION_FILTERED_CHUNKS_DATASET_NAME, HDF5_DATATYPE_NAME, filespace,
            H5P_DEFAULT, plist_id, H5P_DEFAULT)) < 0)
        goto error;
    if (H5Pclose(plist_id) < 0)
        goto error;
    if (H5Sclose(filespace) < 0)
        goto error;

    if ((filespace = H5Dget_space(dset_id)) < 0)
        goto error;
    if (H5Sselect_none(filespace) < 0)
        goto error;

    /* Fill data buffer */
    data_size = sel_dims[0] * sel_dims[1] * sizeof(*data);

    if (NULL == (data = malloc(data_size))) {
        fprintf(stderr, "Couldn't allocate memory.\n");
        goto error;
    }

    for (i = 0; i < data_size / sizeof(*data); i++)
        data[i] = GEN_DATA(i);

    /* Create property list for collective dataset write */
    if ((plist_id = H5Pcreate(H5P_DATASET_XFER)) < 0)
        goto error;
    if (H5Pset_dxpl_mpio(plist_id, H5FD_MPIO_COLLECTIVE) < 0)
        goto error;

    if (H5Dwrite(dset_id, HDF5_DATATYPE_NAME, memspace, filespace, plist_id, data) < 0)
        goto error;

    goto exit;

error:
    if (mpi_rank == 0) puts("*** ALL NO SELECTION FILTERED CHUNKS WRITE TEST FAILED ***");

    ret_value = 1;

exit:
    if (data)
        free(data);
    if (H5Dclose(dset_id) < 0)
        fprintf(stderr, "Unable to close dataset\n");
    if (H5Sclose(filespace) < 0)
        fprintf(stderr, "Unable to close filespace\n");
    if (H5Sclose(memspace) < 0)
        fprintf(stderr, "Unable to close memspace\n");
    if (H5Pclose(plist_id) < 0)
        fprintf(stderr, "Unable to close plist\n");
    if (H5Fclose(file_id) < 0)
        fprintf(stderr, "Unable to close file\n");

    return ret_value;
}

/*
 * Tests parallel write of filtered data by using
 * point selections instead of hyperslab selections.
 *
 * Programmer: Jordan Henderson
 *             02/02/2017
 */
static int
test_filtered_dataset_point_selection()
{
    C_DATATYPE *data = NULL;
    hsize_t     coords[2 * POINT_SELECTION_FILTERED_CHUNKS_NUM_CHUNKS][POINT_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     dataset_dims[POINT_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     chunk_dims[POINT_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     sel_dims[POINT_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    size_t      i, j, data_size;
    hid_t       file_id, dset_id, plist_id;
    hid_t       filespace, memspace;
    int         ret_value = 0;

    if (mpi_rank == 0) puts("Testing write to filtered chunks with point selection");

    /* Set up file access property list with parallel I/O access */
    if ((plist_id = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        goto error;
    if (H5Pset_fapl_mpio(plist_id, comm, info) < 0)
        goto error;
    if (H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
        goto exit;

    if ((file_id = H5Fopen(FILENAME, H5F_ACC_RDWR, plist_id)) < 0)
        goto error;
    if (H5Pclose(plist_id) < 0)
        goto error;

    /* Create the dataspace for the dataset */
    dataset_dims[0] = POINT_SELECTION_FILTERED_CHUNKS_NROWS;
    dataset_dims[1] = POINT_SELECTION_FILTERED_CHUNKS_NCOLS;
    chunk_dims[0] = POINT_SELECTION_FILTERED_CHUNKS_CH_NROWS;
    chunk_dims[1] = POINT_SELECTION_FILTERED_CHUNKS_CH_NCOLS;
    sel_dims[0] = POINT_SELECTION_FILTERED_CHUNKS_NROWS / NUM_MPI_RANKS;
    sel_dims[1] = POINT_SELECTION_FILTERED_CHUNKS_NCOLS;

    if ((filespace = H5Screate_simple(POINT_SELECTION_FILTERED_CHUNKS_DATASET_DIMS, dataset_dims, NULL)) < 0)
        goto error;
    if ((memspace = H5Screate_simple(POINT_SELECTION_FILTERED_CHUNKS_DATASET_DIMS, sel_dims, NULL)) < 0)
        goto error;

    /* Create chunked dataset */
    if ((plist_id = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        goto error;
    if (H5Pset_chunk(plist_id, POINT_SELECTION_FILTERED_CHUNKS_DATASET_DIMS, chunk_dims) < 0)
        goto error;

    /* Add test filter to the pipeline */
    if (SET_FILTER(plist_id) < 0)
        goto error;

    if ((dset_id = H5Dcreate(file_id, POINT_SELECTION_FILTERED_CHUNKS_DATASET_NAME, HDF5_DATATYPE_NAME, filespace,
            H5P_DEFAULT, plist_id, H5P_DEFAULT)) < 0)
        goto error;
    if (H5Pclose(plist_id) < 0)
        goto error;
    if (H5Sclose(filespace) < 0)
        goto error;

    /* Set up point selection */
    if ((filespace = H5Dget_space(dset_id)) < 0)
        goto error;

    for (i = 0; i < 2 * POINT_SELECTION_FILTERED_CHUNKS_NUM_CHUNKS; i++)
        for (j = 0; j < POINT_SELECTION_FILTERED_CHUNKS_DATASET_DIMS; j++) {
            if (j > 0)
                coords[i][j] = i % POINT_SELECTION_FILTERED_CHUNKS_NCOLS;
            else
                coords[i][j] = mpi_rank + ((i / POINT_SELECTION_FILTERED_CHUNKS_NCOLS) * POINT_SELECTION_FILTERED_CHUNKS_CH_NROWS);
        }

    if (H5Sselect_elements(filespace, H5S_SELECT_SET, 2 * POINT_SELECTION_FILTERED_CHUNKS_NUM_CHUNKS, (const hsize_t *) coords) < 0)
        goto error;

    /* Fill data buffer */
    data_size = sel_dims[0] * sel_dims[1] * sizeof(*data);

    if (NULL == (data = malloc(data_size))) {
        fprintf(stderr, "Couldn't allocate memory.\n");
        goto error;
    }

    for (i = 0; i < data_size / sizeof(*data); i++)
        data[i] = GEN_DATA(i);


    /* Create property list for collective dataset write */
    if ((plist_id = H5Pcreate(H5P_DATASET_XFER)) < 0)
        goto error;
    if (H5Pset_dxpl_mpio(plist_id, H5FD_MPIO_COLLECTIVE) < 0)
        goto error;

    if (H5Dwrite(dset_id, HDF5_DATATYPE_NAME, memspace, filespace, plist_id, data) < 0)
        goto error;

    goto exit;

error:
    if (mpi_rank == 0) puts("*** POINT SELECTION FILTERED CHUNKS WRITE TEST FAILED ***");

    ret_value = 1;

exit:
    if (data)
        free(data);
    if (H5Dclose(dset_id) < 0)
        fprintf(stderr, "Unable to close dataset\n");
    if (H5Sclose(filespace) < 0)
        fprintf(stderr, "Unable to close filespace\n");
    if (H5Sclose(memspace) < 0)
        fprintf(stderr, "Unable to close memspace\n");
    if (H5Pclose(plist_id) < 0)
        fprintf(stderr, "Unable to close plist\n");
    if (H5Fclose(file_id) < 0)
        fprintf(stderr, "Unable to close file\n");

    return ret_value;
}

/*
 * Tests parallel write of filtered data in the case where
 * each process writes an equal amount of data to each chunk
 * in the dataset. Each chunk is distributed among the
 * processes in round-robin fashion by blocks of size 1 until
 * the whole chunk is selected, leading to an interleaved
 * write pattern.
 *
 * Programmer: Jordan Henderson
 *             02/02/2017
 */
static int
test_filtered_dataset_interleaved_write()
{
    C_DATATYPE *data = NULL;
    hsize_t     dataset_dims[INTERLEAVED_WRITE_FILTERED_DATASET_DIMS];
    hsize_t     chunk_dims[INTERLEAVED_WRITE_FILTERED_DATASET_DIMS];
    hsize_t     sel_dims[INTERLEAVED_WRITE_FILTERED_DATASET_DIMS];
    hsize_t     count[INTERLEAVED_WRITE_FILTERED_DATASET_DIMS];
    hsize_t     stride[INTERLEAVED_WRITE_FILTERED_DATASET_DIMS];
    hsize_t     block[INTERLEAVED_WRITE_FILTERED_DATASET_DIMS];
    hsize_t     offset[INTERLEAVED_WRITE_FILTERED_DATASET_DIMS];
    size_t      i, data_size;
    hid_t       file_id, dset_id, plist_id;
    hid_t       filespace, memspace;
    int         ret_value = 0;

    if (mpi_rank == 0) puts("Testing interleaved write to filtered chunks");

    /* Set up file access property list with parallel I/O access */
    if ((plist_id = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        goto error;
    if (H5Pset_fapl_mpio(plist_id, comm, info) < 0)
        goto error;
    if (H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
        goto exit;

    if ((file_id = H5Fopen(FILENAME, H5F_ACC_RDWR, plist_id)) < 0)
        goto error;
    if (H5Pclose(plist_id) < 0)
        goto error;

    /* Create the dataspace for the dataset */
    dataset_dims[0] = INTERLEAVED_WRITE_FILTERED_DATASET_NROWS;
    dataset_dims[1] = INTERLEAVED_WRITE_FILTERED_DATASET_NCOLS;
    chunk_dims[0] = INTERLEAVED_WRITE_FILTERED_DATASET_CH_NROWS;
    chunk_dims[1] = INTERLEAVED_WRITE_FILTERED_DATASET_CH_NCOLS;
    sel_dims[0] = INTERLEAVED_WRITE_FILTERED_DATASET_NROWS / 2;
    sel_dims[1] = INTERLEAVED_WRITE_FILTERED_DATASET_NCOLS / 2;

    if ((filespace = H5Screate_simple(INTERLEAVED_WRITE_FILTERED_DATASET_DIMS, dataset_dims, NULL)) < 0)
        goto error;
    if ((memspace = H5Screate_simple(INTERLEAVED_WRITE_FILTERED_DATASET_DIMS, sel_dims, NULL)) < 0)
        goto error;

    /* Create chunked dataset */
    if ((plist_id = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        goto error;
    if (H5Pset_chunk(plist_id, INTERLEAVED_WRITE_FILTERED_DATASET_DIMS, chunk_dims) < 0)
        goto error;

    /* Add test filter to the pipeline */
    if (SET_FILTER(plist_id) < 0)
        goto error;

    if ((dset_id = H5Dcreate(file_id, INTERLEAVED_WRITE_FILTERED_DATASET_NAME, HDF5_DATATYPE_NAME, filespace,
            H5P_DEFAULT, plist_id, H5P_DEFAULT)) < 0)
        goto error;
    if (H5Pclose(plist_id) < 0)
        goto error;
    if (H5Sclose(filespace) < 0)
        goto error;

    /* Each process defines the dataset selection in memory and writes
     * it to the hyperslab in the file
     */
    count[0] = SHARED_FILTERED_CHUNKS_NROWS / 2;
    count[1] = SHARED_FILTERED_CHUNKS_NCOLS / 2;
    stride[0] = 2;
    stride[1] = SHARED_FILTERED_CHUNKS_CH_NCOLS;
    block[0] = 1;
    block[1] = 1;
    offset[0] = mpi_rank / SHARED_FILTERED_CHUNKS_CH_NCOLS;
    offset[1] = mpi_rank % SHARED_FILTERED_CHUNKS_CH_NCOLS;

    printf("Process %d is writing with count[ %llu, %llu ], stride[ %llu, %llu ], offset[ %llu, %llu ], block size[ %llu, %llu ]\n",
            mpi_rank, count[0], count[1], stride[0], stride[1], offset[0], offset[1], block[0], block[1]);

    /* Select hyperslab in the file */
    if ((filespace = H5Dget_space(dset_id)) < 0)
        goto error;
    if (H5Sselect_hyperslab(filespace, H5S_SELECT_SET, offset, stride, count, block) < 0)
        goto error;

    /* Fill data buffer */
    data_size = sel_dims[0] * sel_dims[1] * sizeof(*data);

    if (NULL == (data = malloc(data_size))) {
        fprintf(stderr, "Couldn't allocate memory.\n");
        goto error;
    }

    for (i = 0; i < data_size / sizeof(*data); i++)
        data[i] = GEN_DATA(i);

    /* Create property list for collective dataset write */
    if ((plist_id = H5Pcreate(H5P_DATASET_XFER)) < 0)
        goto error;
    if (H5Pset_dxpl_mpio(plist_id, H5FD_MPIO_COLLECTIVE) < 0)
        goto error;

    if (H5Dwrite(dset_id, HDF5_DATATYPE_NAME, memspace, filespace, plist_id, data) < 0)
        goto error;

    goto exit;

error:
    if (mpi_rank == 0) puts("*** INTERLEAVED FILTERED CHUNK WRITE TEST FAILED ***");

    ret_value = 1;

exit:
    if (data)
        free(data);
    if (H5Dclose(dset_id) < 0)
        fprintf(stderr, "Unable to close dataset\n");
    if (H5Sclose(filespace) < 0)
        fprintf(stderr, "Unable to close filespace\n");
    if (H5Sclose(memspace) < 0)
        fprintf(stderr, "Unable to close memspace\n");
    if (H5Pclose(plist_id) < 0)
        fprintf(stderr, "Unable to close plist\n");
    if (H5Fclose(file_id) < 0)
        fprintf(stderr, "Unable to close file\n");

    return ret_value;
}

/*
 * Tests parallel write of filtered data in the case where
 * the dataset has 3 dimensions and each process writes
 * to its own "page" in the 3rd dimension.
 *
 * Programmer: Jordan Henderson
 *             02/06/2017
 */
static int
test_3d_filtered_dataset_no_overlap_separate_pages()
{
    C_DATATYPE *data = NULL;
    hsize_t     dataset_dims[UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_DIMS];
    hsize_t     chunk_dims[UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_DIMS];
    hsize_t     sel_dims[UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_DIMS];
    hsize_t     count[UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_DIMS];
    hsize_t     stride[UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_DIMS];
    hsize_t     block[UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_DIMS];
    hsize_t     offset[UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_DIMS];
    size_t      i, data_size;
    hid_t       file_id, dset_id, plist_id;
    hid_t       filespace, memspace;
    int         ret_value = 0;

    if (mpi_rank == 0) puts("Testing write to unshared filtered chunks on separate pages in 3D dataset");

    /* Set up file access property list with parallel I/O access */
    if ((plist_id = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        goto error;
    if (H5Pset_fapl_mpio(plist_id, comm, info) < 0)
        goto error;
    if (H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
        goto exit;

    if ((file_id = H5Fopen(FILENAME, H5F_ACC_RDWR, plist_id)) < 0)
        goto error;
    if (H5Pclose(plist_id) < 0)
        goto error;

    /* Create the dataspace for the dataset */
    dataset_dims[0] = UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_NROWS;
    dataset_dims[1] = UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_NCOLS;
    dataset_dims[2] = UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DEPTH;
    chunk_dims[0] = UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_CH_NROWS;
    chunk_dims[1] = UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_CH_NCOLS;
    chunk_dims[2] = 1;
    sel_dims[0] = UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_NROWS;
    sel_dims[1] = UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_NCOLS;
    sel_dims[2] = 1;

    if ((filespace = H5Screate_simple(UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_DIMS, dataset_dims, NULL)) < 0)
        goto error;
    if ((memspace = H5Screate_simple(UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_DIMS, sel_dims, NULL)) < 0)
        goto error;

    /* Create chunked dataset */
    if ((plist_id = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        goto error;
    if (H5Pset_chunk(plist_id, UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_DIMS, chunk_dims) < 0)
        goto error;

    /* Add test filter to the pipeline */
    if (SET_FILTER(plist_id) < 0)
        goto error;

    if ((dset_id = H5Dcreate(file_id, UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_NAME, HDF5_DATATYPE_NAME, filespace,
            H5P_DEFAULT, plist_id, H5P_DEFAULT)) < 0)
        goto error;
    if (H5Pclose(plist_id) < 0)
        goto error;
    if (H5Sclose(filespace) < 0)
        goto error;

    /* Each process defines the dataset selection in memory and writes
     * it to the hyperslab in the file
     */
    count[0] = UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_NROWS / UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_CH_NROWS;
    count[1] = UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_NCOLS / UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_CH_NCOLS;
    count[2] = 1;
    stride[0] = UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_CH_NROWS;
    stride[1] = UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_CH_NCOLS;
    stride[2] = 1;
    block[0] = UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_CH_NROWS;
    block[1] = UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_CH_NCOLS;
    block[2] = 1;
    offset[0] = 0;
    offset[1] = 0;
    offset[2] = mpi_rank;

    printf("Process %d is writing with count[ %llu, %llu, %llu ], stride[ %llu, %llu, %llu ], offset[ %llu, %llu, %llu ], block size[ %llu, %llu, %llu ]\n",
            mpi_rank, count[0], count[1], count[2], stride[0], stride[1], stride[2], offset[0], offset[1], offset[2], block[0], block[1], block[2]);

    /* Select hyperslab in the file */
    if ((filespace = H5Dget_space(dset_id)) < 0)
        goto error;
    if (H5Sselect_hyperslab(filespace, H5S_SELECT_SET, offset, stride, count, block) < 0)
        goto error;

    /* Fill data buffer */
    data_size = sel_dims[0] * sel_dims[1] * sel_dims[2] * sizeof(*data);

    if (NULL == (data = malloc(data_size))) {
        fprintf(stderr, "Couldn't allocate memory.\n");
        goto error;
    }

    for (i = 0; i < data_size / sizeof(*data); i++)
        data[i] = GEN_DATA(i);

    /* Create property list for collective dataset write */
    if ((plist_id = H5Pcreate(H5P_DATASET_XFER)) < 0)
        goto error;
    if (H5Pset_dxpl_mpio(plist_id, H5FD_MPIO_COLLECTIVE) < 0)
        goto error;

    if (H5Dwrite(dset_id, HDF5_DATATYPE_NAME, memspace, filespace, plist_id, data) < 0)
        goto error;

    goto exit;

error:
    if (mpi_rank == 0) puts("*** 3D UNSHARED FILTERED CHUNKS SEPARATE PAGE TEST FAILED ***");

    ret_value = 1;

exit:
    if (data)
        free(data);
    if (H5Dclose(dset_id) < 0)
        fprintf(stderr, "Unable to close dataset\n");
    if (H5Sclose(filespace) < 0)
        fprintf(stderr, "Unable to close filespace\n");
    if (H5Sclose(memspace) < 0)
        fprintf(stderr, "Unable to close memspace\n");
    if (H5Pclose(plist_id) < 0)
        fprintf(stderr, "Unable to close plist\n");
    if (H5Fclose(file_id) < 0)
        fprintf(stderr, "Unable to close file\n");

    return ret_value;
}

/*
 * Tests parallel write of filtered data in the case where
 * the dataset has 3 dimensions and each process writes
 * to each "page" in the 3rd dimension. However, no chunk
 * on a given "page" is written to by more than one process.
 *
 * Programmer: Jordan Henderson
 *             02/06/2017
 */
static int
test_3d_filtered_dataset_no_overlap_same_pages()
{
    C_DATATYPE *data = NULL;
    hsize_t     dataset_dims[UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_DIMS];
    hsize_t     chunk_dims[UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_DIMS];
    hsize_t     sel_dims[UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_DIMS];
    hsize_t     count[UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_DIMS];
    hsize_t     stride[UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_DIMS];
    hsize_t     block[UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_DIMS];
    hsize_t     offset[UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_DIMS];
    size_t      i, data_size;
    hid_t       file_id, dset_id, plist_id;
    hid_t       filespace, memspace;
    int         ret_value = 0;

    if (mpi_rank == 0) puts("Testing write to unshared filtered chunks on the same pages in 3D dataset");

    /* Set up file access property list with parallel I/O access */
    if ((plist_id = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        goto error;
    if (H5Pset_fapl_mpio(plist_id, comm, info) < 0)
        goto error;
    if (H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
        goto exit;

    if ((file_id = H5Fopen(FILENAME, H5F_ACC_RDWR, plist_id)) < 0)
        goto error;
    if (H5Pclose(plist_id) < 0)
        goto error;

    /* Create the dataspace for the dataset */
    dataset_dims[0] = UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_NROWS;
    dataset_dims[1] = UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_NCOLS;
    dataset_dims[2] = UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DEPTH;
    chunk_dims[0] = UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_CH_NROWS;
    chunk_dims[1] = UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_CH_NCOLS;
    chunk_dims[2] = 1;
    sel_dims[0] = UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_CH_NROWS;
    sel_dims[1] = UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_NCOLS;
    sel_dims[2] = UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DEPTH;

    if ((filespace = H5Screate_simple(UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_DIMS, dataset_dims, NULL)) < 0)
        goto error;
    if ((memspace = H5Screate_simple(UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_DIMS, sel_dims, NULL)) < 0)
        goto error;

    /* Create chunked dataset */
    if ((plist_id = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        goto error;
    if (H5Pset_chunk(plist_id, UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_DIMS, chunk_dims) < 0)
        goto error;

    /* Add test filter to the pipeline */
    if (SET_FILTER(plist_id) < 0)
        goto error;

    if ((dset_id = H5Dcreate(file_id, UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_NAME, HDF5_DATATYPE_NAME, filespace,
            H5P_DEFAULT, plist_id, H5P_DEFAULT)) < 0)
        goto error;
    if (H5Pclose(plist_id) < 0)
        goto error;
    if (H5Sclose(filespace) < 0)
        goto error;

    /* Each process defines the dataset selection in memory and writes
     * it to the hyperslab in the file
     */
    count[0] = 1;
    count[1] = UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_NCOLS / UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_CH_NCOLS;
    count[2] = NUM_MPI_RANKS;
    stride[0] = UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_CH_NROWS;
    stride[1] = UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_CH_NCOLS;
    stride[2] = 1;
    block[0] = UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_CH_NROWS;
    block[1] = UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_CH_NCOLS;
    block[2] = 1;
    offset[0] = (mpi_rank * UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_CH_NROWS * count[0]);
    offset[1] = 0;
    offset[2] = 0;

    printf("Process %d is writing with count[ %llu, %llu, %llu ], stride[ %llu, %llu, %llu ], offset[ %llu, %llu, %llu ], block size[ %llu, %llu, %llu ]\n",
            mpi_rank, count[0], count[1], count[2], stride[0], stride[1], stride[2], offset[0], offset[1], offset[2], block[0], block[1], block[2]);

    /* Select hyperslab in the file */
    if ((filespace = H5Dget_space(dset_id)) < 0)
        goto error;
    if (H5Sselect_hyperslab(filespace, H5S_SELECT_SET, offset, stride, count, block) < 0)
        goto error;

    /* Fill data buffer */
    data_size = sel_dims[0] * sel_dims[1] * sel_dims[2] * sizeof(*data);

    if (NULL == (data = malloc(data_size))) {
        fprintf(stderr, "Couldn't allocate memory.\n");
        goto error;
    }

    for (i = 0; i < data_size / sizeof(*data); i++)
        data[i] = GEN_DATA(i);

    /* Create property list for collective dataset write */
    if ((plist_id = H5Pcreate(H5P_DATASET_XFER)) < 0)
        goto error;
    if (H5Pset_dxpl_mpio(plist_id, H5FD_MPIO_COLLECTIVE) < 0)
        goto error;

    if (H5Dwrite(dset_id, HDF5_DATATYPE_NAME, memspace, filespace, plist_id, data) < 0)
        goto error;

    goto exit;

error:
    if (mpi_rank == 0) puts("*** 3D UNSHARED FILTERED CHUNKS SAME PAGE TEST FAILED ***");

    ret_value = 1;

exit:
    if (data)
        free(data);
    if (H5Dclose(dset_id) < 0)
        fprintf(stderr, "Unable to close dataset\n");
    if (H5Sclose(filespace) < 0)
        fprintf(stderr, "Unable to close filespace\n");
    if (H5Sclose(memspace) < 0)
        fprintf(stderr, "Unable to close memspace\n");
    if (H5Pclose(plist_id) < 0)
        fprintf(stderr, "Unable to close plist\n");
    if (H5Fclose(file_id) < 0)
        fprintf(stderr, "Unable to close file\n");

    return ret_value;
}

/*
 * Tests parallel write of filtered data in the case where
 * the dataset has 3 dimensions and each process writes
 * to each "page" in the 3rd dimension. Further, each chunk
 * in each "page" is written to equally by all processes.
 *
 * Programmer: Jordan Henderson
 *             02/06/2017
 */
static int
test_3d_filtered_dataset_overlap()
{
    C_DATATYPE *data = NULL;
    hsize_t     dataset_dims[SHARED_FILTERED_CHUNKS_3D_DATASET_DIMS];
    hsize_t     chunk_dims[SHARED_FILTERED_CHUNKS_3D_DATASET_DIMS];
    hsize_t     sel_dims[SHARED_FILTERED_CHUNKS_3D_DATASET_DIMS];
    hsize_t     count[SHARED_FILTERED_CHUNKS_3D_DATASET_DIMS];
    hsize_t     stride[SHARED_FILTERED_CHUNKS_3D_DATASET_DIMS];
    hsize_t     block[SHARED_FILTERED_CHUNKS_3D_DATASET_DIMS];
    hsize_t     offset[SHARED_FILTERED_CHUNKS_3D_DATASET_DIMS];
    size_t      i, data_size;
    hid_t       file_id, dset_id, plist_id;
    hid_t       filespace, memspace;
    int         ret_value = 0;

    if (mpi_rank == 0) puts("Testing write to shared filtered chunks in 3D dataset");

    /* Set up file access property list with parallel I/O access */
    if ((plist_id = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        goto error;
    if (H5Pset_fapl_mpio(plist_id, comm, info) < 0)
        goto error;
    if (H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
        goto exit;

    if ((file_id = H5Fopen(FILENAME, H5F_ACC_RDWR, plist_id)) < 0)
        goto error;
    if (H5Pclose(plist_id) < 0)
        goto error;

    /* Create the dataspace for the dataset */
    dataset_dims[0] = SHARED_FILTERED_CHUNKS_3D_NROWS;
    dataset_dims[1] = SHARED_FILTERED_CHUNKS_3D_NCOLS;
    dataset_dims[2] = SHARED_FILTERED_CHUNKS_3D_DEPTH;
    chunk_dims[0] = SHARED_FILTERED_CHUNKS_3D_CH_NROWS;
    chunk_dims[1] = SHARED_FILTERED_CHUNKS_3D_CH_NCOLS;
    chunk_dims[2] = 1;
    sel_dims[0] = SHARED_FILTERED_CHUNKS_3D_NROWS / 2;
    sel_dims[1] = SHARED_FILTERED_CHUNKS_3D_NCOLS / 2;
    sel_dims[2] = SHARED_FILTERED_CHUNKS_3D_DEPTH;

    if ((filespace = H5Screate_simple(SHARED_FILTERED_CHUNKS_3D_DATASET_DIMS, dataset_dims, NULL)) < 0)
        goto error;
    if ((memspace = H5Screate_simple(SHARED_FILTERED_CHUNKS_3D_DATASET_DIMS, sel_dims, NULL)) < 0)
        goto error;

    /* Create chunked dataset */
    if ((plist_id = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        goto error;
    if (H5Pset_chunk(plist_id, SHARED_FILTERED_CHUNKS_3D_DATASET_DIMS, chunk_dims) < 0)
        goto error;

    /* Add test filter to the pipeline */
    if (SET_FILTER(plist_id) < 0)
        goto error;

    if ((dset_id = H5Dcreate(file_id, SHARED_FILTERED_CHUNKS_3D_DATASET_NAME, HDF5_DATATYPE_NAME, filespace,
            H5P_DEFAULT, plist_id, H5P_DEFAULT)) < 0)
        goto error;
    if (H5Pclose(plist_id) < 0)
        goto error;
    if (H5Sclose(filespace) < 0)
        goto error;

    /* Each process defines the dataset selection in memory and writes
     * it to the hyperslab in the file
     */
    count[0] = SHARED_FILTERED_CHUNKS_3D_NROWS / 2;
    count[1] = SHARED_FILTERED_CHUNKS_3D_NCOLS / 2;
    count[2] = SHARED_FILTERED_CHUNKS_3D_DEPTH;
    stride[0] = 2;
    stride[1] = SHARED_FILTERED_CHUNKS_3D_CH_NCOLS;
    stride[2] = 1;
    block[0] = 1;
    block[1] = 1;
    block[2] = 1;
    offset[0] = mpi_rank / SHARED_FILTERED_CHUNKS_3D_CH_NCOLS;
    offset[1] = mpi_rank % SHARED_FILTERED_CHUNKS_3D_CH_NCOLS;
    offset[2] = 0;

    printf("Process %d is writing with count[ %llu, %llu, %llu ], stride[ %llu, %llu, %llu ], offset[ %llu, %llu, %llu ], block size[ %llu, %llu, %llu ]\n",
            mpi_rank, count[0], count[1], count[2], stride[0], stride[1], stride[2], offset[0], offset[1], offset[2], block[0], block[1], block[2]);

    /* Select hyperslab in the file */
    if ((filespace = H5Dget_space(dset_id)) < 0)
        goto error;
    if (H5Sselect_hyperslab(filespace, H5S_SELECT_SET, offset, stride, count, block) < 0)
        goto error;

    /* Fill data buffer */
    data_size = sel_dims[0] * sel_dims[1] * sel_dims[2] * sizeof(*data);

    if (NULL == (data = malloc(data_size))) {
        fprintf(stderr, "Couldn't allocate memory.\n");
        goto error;
    }

    for (i = 0; i < data_size / sizeof(*data); i++)
        data[i] = GEN_DATA(i);

    /* Create property list for collective dataset write */
    if ((plist_id = H5Pcreate(H5P_DATASET_XFER)) < 0)
        goto error;
    if (H5Pset_dxpl_mpio(plist_id, H5FD_MPIO_COLLECTIVE) < 0)
        goto error;

    if (H5Dwrite(dset_id, HDF5_DATATYPE_NAME, memspace, filespace, plist_id, data) < 0)
        goto error;

    goto exit;

error:
    if (mpi_rank == 0) puts("*** 3D SHARED FILTERED CHUNKS TEST FAILED ***");

    ret_value = 1;

exit:
    if (data)
        free(data);
    if (H5Dclose(dset_id) < 0)
        fprintf(stderr, "Unable to close dataset\n");
    if (H5Sclose(filespace) < 0)
        fprintf(stderr, "Unable to close filespace\n");
    if (H5Sclose(memspace) < 0)
        fprintf(stderr, "Unable to close memspace\n");
    if (H5Pclose(plist_id) < 0)
        fprintf(stderr, "Unable to close plist\n");
    if (H5Fclose(file_id) < 0)
        fprintf(stderr, "Unable to close file\n");

    return ret_value;
}

static int
test_32d_filtered_dataset_no_overlap_separate_pages(void)
{
    return 1;
}

static int
test_32d_filtered_dataset_no_overlap_same_pages(void)
{
    return 1;
}

static int
test_32d_filtered_dataset_overlap(void)
{
    return 1;
}

/*
 * Tests parallel write of filtered data to unshared
 * chunks using a compound datatype which doesn't
 * require a datatype conversion.
 *
 * Programmer: Jordan Henderson
 *             02/10/2017
 */
static int
test_cmpd_filtered_dataset_no_conversion_unshared()
{
    cmpd_filtered_t data[COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_ENTRIES_PER_PROC];
    hsize_t         dataset_dims[COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t         chunk_dims[COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t         sel_dims[COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t         count[COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t         stride[COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t         block[COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t         offset[COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_DIMS];
    size_t          i;
    hid_t           file_id, dset_id, plist_id, memtype;
    hid_t           filespace, memspace;
    int             ret_value = 0;

    if (mpi_rank == 0) puts("Testing write to unshared filtered chunks in Compound Datatype dataset without Datatype conversion");

    /* Set up file access property list with parallel I/O access */
    if ((plist_id = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        goto error;
    if (H5Pset_fapl_mpio(plist_id, comm, info) < 0)
        goto error;
    if (H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
        goto exit;

    if ((file_id = H5Fopen(FILENAME, H5F_ACC_RDWR, plist_id)) < 0)
        goto error;
    if (H5Pclose(plist_id) < 0)
        goto error;

    /* Create the dataspace for the dataset */
    dataset_dims[0] = COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_NROWS;
    dataset_dims[1] = COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_NCOLS;
    chunk_dims[0] = COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_CH_NROWS;
    chunk_dims[1] = COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_CH_NCOLS;
    sel_dims[0] = COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_CH_NROWS;
    sel_dims[1] = COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_ENTRIES_PER_PROC;

    if ((filespace = H5Screate_simple(COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_DIMS, dataset_dims, NULL)) < 0)
        goto error;
    if ((memspace = H5Screate_simple(COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_DIMS, sel_dims, NULL)) < 0)
        goto error;

    /* Create chunked dataset */
    if ((plist_id = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        goto error;
    if (H5Pset_chunk(plist_id, COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_DIMS, chunk_dims) < 0)
        goto error;

    /* Add test filter to the pipeline */
    if (SET_FILTER(plist_id) < 0)
        goto error;

    /* Create the compound type for memory. */
    if ((memtype = H5Tcreate(H5T_COMPOUND, sizeof(cmpd_filtered_t))) < 0)
        goto error;
    if (H5Tinsert(memtype, "ShortData", HOFFSET(cmpd_filtered_t, field1), H5T_NATIVE_SHORT) < 0)
        goto error;
    if (H5Tinsert(memtype, "IntData", HOFFSET(cmpd_filtered_t, field2), H5T_NATIVE_INT) < 0)
        goto error;
    if (H5Tinsert(memtype, "LongData", HOFFSET(cmpd_filtered_t, field3), H5T_NATIVE_LONG) < 0)
        goto error;
    if (H5Tinsert(memtype, "DoubleData", HOFFSET(cmpd_filtered_t, field4), H5T_NATIVE_DOUBLE) < 0)
        goto error;

    if ((dset_id = H5Dcreate(file_id, COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_NAME, memtype, filespace,
            H5P_DEFAULT, plist_id, H5P_DEFAULT)) < 0)
        goto error;
    if (H5Pclose(plist_id) < 0)
        goto error;
    if (H5Sclose(filespace) < 0)
        goto error;

    /* Each process defines the dataset selection in memory and writes
     * it to the hyperslab in the file
     */
    count[0] = 1;
    count[1] = COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_ENTRIES_PER_PROC;
    stride[0] = COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_CH_NROWS;
    stride[1] = COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_CH_NCOLS;
    block[0] = COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_CH_NROWS;
    block[1] = COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_CH_NCOLS;
    offset[0] = 0;
    offset[1] = (mpi_rank * COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_CH_NCOLS);

    printf("Process %d is writing with count[ %llu, %llu ], stride[ %llu, %llu ], offset[ %llu, %llu ], block size[ %llu, %llu ]\n",
                mpi_rank, count[0], count[1], stride[0], stride[1], offset[0], offset[1], block[0], block[1]);

    /* Select hyperslab in the file */
    if ((filespace = H5Dget_space(dset_id)) < 0)
        goto error;
    if (H5Sselect_hyperslab(filespace, H5S_SELECT_SET, offset, stride, count, block) < 0)
        goto error;

    /* Fill data buffer */
    memset(data, 0, sizeof(cmpd_filtered_t) * COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_ENTRIES_PER_PROC);
    for (i = 0; i < COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_ENTRIES_PER_PROC; i++)
        data[i].field1 = data[i].field2 = data[i].field3 = data[i].field4 = GEN_DATA(i);

    /* Create property list for collective dataset write */
    if ((plist_id = H5Pcreate(H5P_DATASET_XFER)) < 0)
        goto error;
    if (H5Pset_dxpl_mpio(plist_id, H5FD_MPIO_COLLECTIVE) < 0)
        goto error;

    if (H5Dwrite(dset_id, memtype, memspace, filespace, plist_id, data) < 0)
        goto error;

    goto exit;

error:
    if (mpi_rank == 0) puts("*** COMPOUND DATATYPE FILTERED CHUNKS NO CONVERSION UNSHARED TEST FAILED ***");

    ret_value = 1;

exit:
    if (H5Dclose(dset_id) < 0)
        fprintf(stderr, "Unable to close dataset\n");
    if (H5Sclose(filespace) < 0)
        fprintf(stderr, "Unable to close filespace\n");
    if (H5Sclose(memspace) < 0)
        fprintf(stderr, "Unable to close memspace\n");
    if (H5Tclose(memtype) < 0)
        fprintf(stderr, "Unable to close mem type\n");
    if (H5Pclose(plist_id) < 0)
        fprintf(stderr, "Unable to close plist\n");
    if (H5Fclose(file_id) < 0)
        fprintf(stderr, "Unable to close file\n");

    return ret_value;
}

/*
 * Tests parallel write of filtered data to shared
 * chunks using a compound datatype which doesn't
 * require a datatype conversion.
 *
 * Programmer: Jordan Henderson
 *             02/10/2017
 */
static int
test_cmpd_filtered_dataset_no_conversion_shared()
{
    cmpd_filtered_t data[COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_ENTRIES_PER_PROC];
    hsize_t         dataset_dims[COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t         chunk_dims[COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t         sel_dims[COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t         count[COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t         stride[COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t         block[COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t         offset[COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_DIMS];
    size_t          i;
    hid_t           file_id, dset_id, plist_id, memtype;
    hid_t           filespace, memspace;
    int             ret_value = 0;

    if (mpi_rank == 0) puts("Testing write to shared filtered chunks in Compound Datatype dataset without Datatype conversion");

    /* Set up file access property list with parallel I/O access */
    if ((plist_id = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        goto error;
    if (H5Pset_fapl_mpio(plist_id, comm, info) < 0)
        goto error;
    if (H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
        goto exit;

    if ((file_id = H5Fopen(FILENAME, H5F_ACC_RDWR, plist_id)) < 0)
        goto error;
    if (H5Pclose(plist_id) < 0)
        goto error;

    /* Create the dataspace for the dataset */
    dataset_dims[0] = COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_NROWS;
    dataset_dims[1] = COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_NCOLS;
    chunk_dims[0] = COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_CH_NROWS;
    chunk_dims[1] = COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_CH_NCOLS;
    sel_dims[0] = COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_CH_NROWS / NUM_MPI_RANKS;
    sel_dims[1] = COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_ENTRIES_PER_PROC;

    if ((filespace = H5Screate_simple(COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_DIMS, dataset_dims, NULL)) < 0)
        goto error;
    if ((memspace = H5Screate_simple(COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_DIMS, sel_dims, NULL)) < 0)
        goto error;

    /* Create chunked dataset */
    if ((plist_id = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        goto error;
    if (H5Pset_chunk(plist_id, COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_DIMS, chunk_dims) < 0)
        goto error;

    /* Add test filter to the pipeline */
    if (SET_FILTER(plist_id) < 0)
        goto error;

    /* Create the compound type for memory. */
    if ((memtype = H5Tcreate(H5T_COMPOUND, sizeof(cmpd_filtered_t))) < 0)
        goto error;
    if (H5Tinsert(memtype, "ShortData", HOFFSET(cmpd_filtered_t, field1), H5T_NATIVE_SHORT) < 0)
        goto error;
    if (H5Tinsert(memtype, "IntData", HOFFSET(cmpd_filtered_t, field2), H5T_NATIVE_INT) < 0)
        goto error;
    if (H5Tinsert(memtype, "LongData", HOFFSET(cmpd_filtered_t, field3), H5T_NATIVE_LONG) < 0)
        goto error;
    if (H5Tinsert(memtype, "DoubleData", HOFFSET(cmpd_filtered_t, field4), H5T_NATIVE_DOUBLE) < 0)
        goto error;

    if ((dset_id = H5Dcreate(file_id, COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_NAME, memtype, filespace,
            H5P_DEFAULT, plist_id, H5P_DEFAULT)) < 0)
        goto error;
    if (H5Pclose(plist_id) < 0)
        goto error;
    if (H5Sclose(filespace) < 0)
        goto error;

    /* Each process defines the dataset selection in memory and writes
     * it to the hyperslab in the file
     */
    count[0] = 1;
    count[1] = COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_ENTRIES_PER_PROC;
    stride[0] = COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_CH_NROWS;
    stride[1] = COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_CH_NCOLS;
    block[0] = COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_CH_NROWS / NUM_MPI_RANKS;
    block[1] = COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_CH_NCOLS;
    offset[0] = mpi_rank;
    offset[1] = 0;

    printf("Process %d is writing with count[ %llu, %llu ], stride[ %llu, %llu ], offset[ %llu, %llu ], block size[ %llu, %llu ]\n",
                mpi_rank, count[0], count[1], stride[0], stride[1], offset[0], offset[1], block[0], block[1]);

    /* Select hyperslab in the file */
    if ((filespace = H5Dget_space(dset_id)) < 0)
        goto error;
    if (H5Sselect_hyperslab(filespace, H5S_SELECT_SET, offset, stride, count, block) < 0)
        goto error;

    /* Fill data buffer */
    memset(data, 0, sizeof(cmpd_filtered_t) * COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_ENTRIES_PER_PROC);
    for (i = 0; i < COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_ENTRIES_PER_PROC; i++)
        data[i].field1 = data[i].field2 = data[i].field3 = data[i].field4 = GEN_DATA(i);

    /* Create property list for collective dataset write */
    if ((plist_id = H5Pcreate(H5P_DATASET_XFER)) < 0)
        goto error;
    if (H5Pset_dxpl_mpio(plist_id, H5FD_MPIO_COLLECTIVE) < 0)
        goto error;

    if (H5Dwrite(dset_id, memtype, memspace, filespace, plist_id, data) < 0)
        goto error;

    goto exit;

error:
    if (mpi_rank == 0) puts("*** COMPOUND DATATYPE FILTERED CHUNKS NO CONVERSION SHARED TEST FAILED ***");

    ret_value = 1;

exit:
    if (H5Dclose(dset_id) < 0)
        fprintf(stderr, "Unable to close dataset\n");
    if (H5Sclose(filespace) < 0)
        fprintf(stderr, "Unable to close filespace\n");
    if (H5Sclose(memspace) < 0)
        fprintf(stderr, "Unable to close memspace\n");
    if (H5Tclose(memtype) < 0)
        fprintf(stderr, "Unable to close mem type\n");
    if (H5Pclose(plist_id) < 0)
        fprintf(stderr, "Unable to close plist\n");
    if (H5Fclose(file_id) < 0)
        fprintf(stderr, "Unable to close file\n");

    return ret_value;
}

/*
 * Tests parallel write of filtered data to unshared
 * chunks using a compound datatype which requires a
 * datatype conversion.
 *
 * This test currently fails because the datatype
 * conversion causes the parallel library to break
 * to independent I/O and this isn't allowed when
 * there are filters in the pipeline.
 *
 * Programmer: Jordan Henderson
 *             02/07/2017
 */
static int
test_cmpd_filtered_dataset_type_conversion_unshared()
{
    cmpd_filtered_t data[COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_ENTRIES_PER_PROC];
    hsize_t         dataset_dims[COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t         chunk_dims[COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t         sel_dims[COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t         count[COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t         stride[COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t         block[COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t         offset[COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_DIMS];
    size_t          i;
    hid_t           file_id, dset_id, plist_id, filetype, memtype;
    hid_t           filespace, memspace;
    int             ret_value = 0;

    if (mpi_rank == 0) puts("Testing write to unshared filtered chunks in Compound Datatype dataset with Datatype conversion");

    /* Set up file access property list with parallel I/O access */
    if ((plist_id = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        goto error;
    if (H5Pset_fapl_mpio(plist_id, comm, info) < 0)
        goto error;
    if (H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
        goto exit;

    if ((file_id = H5Fopen(FILENAME, H5F_ACC_RDWR, plist_id)) < 0)
        goto error;
    if (H5Pclose(plist_id) < 0)
        goto error;

    /* Create the dataspace for the dataset */
    dataset_dims[0] = COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_NROWS;
    dataset_dims[1] = COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_NCOLS;
    chunk_dims[0] = COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_CH_NROWS;
    chunk_dims[1] = COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_CH_NCOLS;
    sel_dims[0] = COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_CH_NROWS;
    sel_dims[1] = COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_ENTRIES_PER_PROC;

    if ((filespace = H5Screate_simple(COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_DIMS, dataset_dims, NULL)) < 0)
        goto error;
    if ((memspace = H5Screate_simple(COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_DIMS, sel_dims, NULL)) < 0)
        goto error;

    /* Create chunked dataset */
    if ((plist_id = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        goto error;
    if (H5Pset_chunk(plist_id, COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_DIMS, chunk_dims) < 0)
        goto error;

    /* Add test filter to the pipeline */
    if (SET_FILTER(plist_id) < 0)
        goto error;

    /* Create the compound type for memory. */
    if ((memtype = H5Tcreate(H5T_COMPOUND, sizeof(cmpd_filtered_t))) < 0)
        goto error;
    if (H5Tinsert(memtype, "ShortData", HOFFSET(cmpd_filtered_t, field1), H5T_NATIVE_SHORT) < 0)
        goto error;
    if (H5Tinsert(memtype, "IntData", HOFFSET(cmpd_filtered_t, field2), H5T_NATIVE_INT) < 0)
        goto error;
    if (H5Tinsert(memtype, "LongData", HOFFSET(cmpd_filtered_t, field3), H5T_NATIVE_LONG) < 0)
        goto error;
    if (H5Tinsert(memtype, "DoubleData", HOFFSET(cmpd_filtered_t, field4), H5T_NATIVE_DOUBLE) < 0)
        goto error;

    /* Create the compound type for file. */
    if ((filetype = H5Tcreate(H5T_COMPOUND, 32)) < 0)
        goto error;
    if (H5Tinsert(filetype, "ShortData", 0, H5T_STD_I64BE) < 0)
        goto error;
    if (H5Tinsert(filetype, "IntData", 8, H5T_STD_I64BE) < 0)
        goto error;
    if (H5Tinsert(filetype, "LongData", 16, H5T_STD_I64BE) < 0)
        goto error;
    if (H5Tinsert(filetype, "DoubleData", 24, H5T_IEEE_F64BE) < 0)
        goto error;

    if ((dset_id = H5Dcreate(file_id, COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_NAME, filetype, filespace,
            H5P_DEFAULT, plist_id, H5P_DEFAULT)) < 0)
        goto error;
    if (H5Pclose(plist_id) < 0)
        goto error;
    if (H5Sclose(filespace) < 0)
        goto error;

    /* Each process defines the dataset selection in memory and writes
     * it to the hyperslab in the file
     */
    count[0] = 1;
    count[1] = COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_ENTRIES_PER_PROC;
    stride[0] = COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_CH_NROWS;
    stride[1] = COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_CH_NCOLS;
    block[0] = COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_CH_NROWS;
    block[1] = COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_CH_NCOLS;
    offset[0] = 0;
    offset[1] = (mpi_rank * COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_CH_NCOLS);

    printf("Process %d is writing with count[ %llu, %llu ], stride[ %llu, %llu ], offset[ %llu, %llu ], block size[ %llu, %llu ]\n",
                mpi_rank, count[0], count[1], stride[0], stride[1], offset[0], offset[1], block[0], block[1]);

    /* Select hyperslab in the file */
    if ((filespace = H5Dget_space(dset_id)) < 0)
        goto error;
    if (H5Sselect_hyperslab(filespace, H5S_SELECT_SET, offset, stride, count, block) < 0)
        goto error;

    /* Fill data buffer */
    memset(data, 0, sizeof(cmpd_filtered_t) * COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_ENTRIES_PER_PROC);
    for (i = 0; i < COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_ENTRIES_PER_PROC; i++)
        data[i].field1 = data[i].field2 = data[i].field3 = data[i].field4 = GEN_DATA(i);

    /* Create property list for collective dataset write */
    if ((plist_id = H5Pcreate(H5P_DATASET_XFER)) < 0)
        goto error;
    if (H5Pset_dxpl_mpio(plist_id, H5FD_MPIO_COLLECTIVE) < 0)
        goto error;

    if (H5Dwrite(dset_id, memtype, memspace, filespace, plist_id, data) < 0)
        goto error;

    goto exit;

error:
    if (mpi_rank == 0) puts("*** COMPOUND DATATYPE FILTERED CHUNKS TYPE CONVERSION UNSHARED TEST FAILED ***");

    ret_value = 1;

exit:
    if (H5Dclose(dset_id) < 0)
        fprintf(stderr, "Unable to close dataset\n");
    if (H5Sclose(filespace) < 0)
        fprintf(stderr, "Unable to close filespace\n");
    if (H5Sclose(memspace) < 0)
        fprintf(stderr, "Unable to close memspace\n");
    if (H5Tclose(filetype) < 0)
        fprintf(stderr, "Unable to close file type\n");
    if (H5Tclose(memtype) < 0)
        fprintf(stderr, "Unable to close mem type\n");
    if (H5Pclose(plist_id) < 0)
        fprintf(stderr, "Unable to close plist\n");
    if (H5Fclose(file_id) < 0)
        fprintf(stderr, "Unable to close file\n");

    return ret_value;
}

/*
 * Tests parallel write of filtered data to shared
 * chunks using a compound datatype which requires
 * a datatype conversion.
 *
 * This test currently fails because the datatype
 * conversion causes the parallel library to break
 * to independent I/O and this isn't allowed when
 * there are filters in the pipeline.
 *
 * Programmer: Jordan Henderson
 *             02/10/2017
 */
static int
test_cmpd_filtered_dataset_type_conversion_shared()
{
    cmpd_filtered_t data[COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_ENTRIES_PER_PROC];
    hsize_t         dataset_dims[COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t         chunk_dims[COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t         sel_dims[COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t         count[COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t         stride[COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t         block[COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t         offset[COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_DIMS];
    size_t          i;
    hid_t           file_id, dset_id, plist_id, filetype, memtype;
    hid_t           filespace, memspace;
    int             ret_value = 0;

    if (mpi_rank == 0) puts("Testing write to shared filtered chunks in Compound Datatype dataset with Datatype conversion");

    /* Set up file access property list with parallel I/O access */
    if ((plist_id = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        goto error;
    if (H5Pset_fapl_mpio(plist_id, comm, info) < 0)
        goto error;
    if (H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
        goto exit;

    if ((file_id = H5Fopen(FILENAME, H5F_ACC_RDWR, plist_id)) < 0)
        goto error;
    if (H5Pclose(plist_id) < 0)
        goto error;

    /* Create the dataspace for the dataset */
    dataset_dims[0] = COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_NROWS;
    dataset_dims[1] = COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_NCOLS;
    chunk_dims[0] = COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_CH_NROWS;
    chunk_dims[1] = COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_CH_NCOLS;
    sel_dims[0] = COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_CH_NROWS / NUM_MPI_RANKS;
    sel_dims[1] = COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_ENTRIES_PER_PROC;

    if ((filespace = H5Screate_simple(COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_DIMS, dataset_dims, NULL)) < 0)
        goto error;
    if ((memspace = H5Screate_simple(COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_DIMS, sel_dims, NULL)) < 0)
        goto error;

    /* Create chunked dataset */
    if ((plist_id = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        goto error;
    if (H5Pset_chunk(plist_id, COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_DIMS, chunk_dims) < 0)
        goto error;

    /* Add test filter to the pipeline */
    if (SET_FILTER(plist_id) < 0)
        goto error;

    /* Create the compound type for memory. */
    if ((memtype = H5Tcreate(H5T_COMPOUND, sizeof(cmpd_filtered_t))) < 0)
        goto error;
    if (H5Tinsert(memtype, "ShortData", HOFFSET(cmpd_filtered_t, field1), H5T_NATIVE_SHORT) < 0)
        goto error;
    if (H5Tinsert(memtype, "IntData", HOFFSET(cmpd_filtered_t, field2), H5T_NATIVE_INT) < 0)
        goto error;
    if (H5Tinsert(memtype, "LongData", HOFFSET(cmpd_filtered_t, field3), H5T_NATIVE_LONG) < 0)
        goto error;
    if (H5Tinsert(memtype, "DoubleData", HOFFSET(cmpd_filtered_t, field4), H5T_NATIVE_DOUBLE) < 0)
        goto error;

    /* Create the compound type for file. */
    if ((filetype = H5Tcreate(H5T_COMPOUND, 32)) < 0)
        goto error;
    if (H5Tinsert(filetype, "ShortData", 0, H5T_STD_I64BE) < 0)
        goto error;
    if (H5Tinsert(filetype, "IntData", 8, H5T_STD_I64BE) < 0)
        goto error;
    if (H5Tinsert(filetype, "LongData", 16, H5T_STD_I64BE) < 0)
        goto error;
    if (H5Tinsert(filetype, "DoubleData", 24, H5T_IEEE_F64BE) < 0)
        goto error;

    if ((dset_id = H5Dcreate(file_id, COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_NAME, filetype, filespace,
            H5P_DEFAULT, plist_id, H5P_DEFAULT)) < 0)
        goto error;
    if (H5Pclose(plist_id) < 0)
        goto error;
    if (H5Sclose(filespace) < 0)
        goto error;

    /* Each process defines the dataset selection in memory and writes
     * it to the hyperslab in the file
     */
    count[0] = 1;
    count[1] = COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_ENTRIES_PER_PROC;
    stride[0] = COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_CH_NROWS;
    stride[1] = COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_CH_NCOLS;
    block[0] = COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_CH_NROWS / NUM_MPI_RANKS;
    block[1] = COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_CH_NCOLS;
    offset[0] = mpi_rank;
    offset[1] = 0;

    printf("Process %d is writing with count[ %llu, %llu ], stride[ %llu, %llu ], offset[ %llu, %llu ], block size[ %llu, %llu ]\n",
                mpi_rank, count[0], count[1], stride[0], stride[1], offset[0], offset[1], block[0], block[1]);

    /* Select hyperslab in the file */
    if ((filespace = H5Dget_space(dset_id)) < 0)
        goto error;
    if (H5Sselect_hyperslab(filespace, H5S_SELECT_SET, offset, stride, count, block) < 0)
        goto error;

    /* Fill data buffer */
    memset(data, 0, sizeof(cmpd_filtered_t) * COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_ENTRIES_PER_PROC);
    for (i = 0; i < COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_ENTRIES_PER_PROC; i++)
        data[i].field1 = data[i].field2 = data[i].field3 = data[i].field4 = GEN_DATA(i);

    /* Create property list for collective dataset write */
    if ((plist_id = H5Pcreate(H5P_DATASET_XFER)) < 0)
        goto error;
    if (H5Pset_dxpl_mpio(plist_id, H5FD_MPIO_COLLECTIVE) < 0)
        goto error;

    if (H5Dwrite(dset_id, memtype, memspace, filespace, plist_id, data) < 0)
        goto error;

    goto exit;

error:
    if (mpi_rank == 0) puts("*** COMPOUND DATATYPE FILTERED CHUNKS TYPE CONVERSION SHARED TEST FAILED ***");

    ret_value = 1;

exit:
    if (H5Dclose(dset_id) < 0)
        fprintf(stderr, "Unable to close dataset\n");
    if (H5Sclose(filespace) < 0)
        fprintf(stderr, "Unable to close filespace\n");
    if (H5Sclose(memspace) < 0)
        fprintf(stderr, "Unable to close memspace\n");
    if (H5Tclose(filetype) < 0)
        fprintf(stderr, "Unable to close file type\n");
    if (H5Tclose(memtype) < 0)
        fprintf(stderr, "Unable to close mem type\n");
    if (H5Pclose(plist_id) < 0)
        fprintf(stderr, "Unable to close plist\n");
    if (H5Fclose(file_id) < 0)
        fprintf(stderr, "Unable to close file\n");

    return ret_value;
}

static int
read_outfile(char* filename, char* datasetname)
{
    H5Z_filter_t filter_type;
    C_DATATYPE  *rbuf = NULL;
    unsigned     flags, filter_info;
    hsize_t     *dims, *maxdims;
    size_t       i, j, k, nelmts, buf_size;
    hid_t        file_id, dset_id, plist_id, creation_plist, dspace_id;
    int          num_filters, ndims;

    if ((plist_id = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        goto error;
    if (H5Pset_fapl_mpio(plist_id, comm, info) < 0)
        goto error;

    if ((file_id = H5Fopen(filename, H5F_ACC_RDONLY, plist_id)) < 0)
        goto error;
    if (H5Pclose(plist_id) < 0)
        goto error;

    if ((dset_id = H5Dopen(file_id, datasetname, H5P_DEFAULT)) < 0)
        goto error;

    if ((dspace_id = H5Dget_space(dset_id)) < 0)
        goto error;

    if ((creation_plist = H5Dget_create_plist(dset_id)) < 0)
        goto error;

    if ((num_filters = H5Pget_nfilters(creation_plist)) < 0)
        goto error;

    if (mpi_rank == 0) {
        printf("Number of filters: %i\n", num_filters);

        for (i = 0; i < num_filters; i++) {
            nelmts = 0;
            if ((filter_type = H5Pget_filter2(creation_plist, 0, &flags, &nelmts, NULL, 0,
                    NULL, &filter_info)) < 0)
                goto error;

            printf("Filter Type: ");
            switch (filter_type) {
                case H5Z_FILTER_DEFLATE:
                    printf("H5Z_FILTER_DEFLATE\n");
                    break;
                case H5Z_FILTER_SZIP:
                    printf("H5Z_FILTER_SZIP\n");
                    break;
                default:
                    printf("Other filter type included.\n");
            }
        }
    }

    if ((plist_id = H5Pcreate(H5P_DATASET_XFER)) < 0)
        goto error;
    if (H5Pset_dxpl_mpio(plist_id, H5FD_MPIO_COLLECTIVE) < 0)
        goto error;

    if ((ndims = H5Sget_simple_extent_ndims(dspace_id)) < 0)
        goto error;
    if (NULL == (dims = malloc(ndims * sizeof(*dims))))
        goto error;
    if (NULL == (maxdims = malloc(ndims * sizeof(*dims))))
        goto error;

    if (H5Sget_simple_extent_dims(dspace_id, dims, maxdims) < 0)
        goto error;

    buf_size = 0;
    for (i = 0; i < ndims; i++)
        buf_size += dims[i] * sizeof(*rbuf);

    if (NULL == (rbuf = malloc(buf_size))) {
        fprintf(stderr, "Couldn't allocate memory.\n");
        goto error;
    }

    if (H5Dread(dset_id, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, plist_id, rbuf) < 0)
        goto error;

    if (mpi_rank == 0) {
        /* Only set up for use in 2D and 3D cases */
        if (ndims > 2) {
            for (i = 0; i < dims[0]; i++)
                for (j = 0; j < dims[1]; j++)
                    for (k = 0; k < dims[2]; k++)
                        printf("(%zu, %zu): %ld\n", i, j, rbuf[(i * dims[1]) + j]);
        }
        else {
            for (i = 0; i < dims[0]; i++)
                for (j = 0; j < dims[1]; j++)
                    printf("(%zu, %zu): %ld\n", i, j, rbuf[(i * dims[1]) + j]);
        }
    }

    free(rbuf);
    if (H5Dclose(dset_id) < 0)
        goto error;
    if (H5Pclose(plist_id) < 0)
        goto error;
    if (H5Pclose(creation_plist) < 0)
        goto error;
    if (H5Fclose(file_id) < 0)
        goto error;

    return 0;

error:
    if (rbuf)
        free(rbuf);
    if (dims)
        free(dims);
    if (maxdims)
        free(maxdims);

    return 1;
}

static int
test_write_serial_read_parallel(void)
{
    return 1;
}

static int
test_write_parallel_read_serial(void)
{
    return 1;
}

int
main(int argc, char** argv)
{
    size_t i;
    hid_t  file_id, fapl;
    int    mpi_code;
    int    nerrors = 0;

    /* Initialize MPI */
    MPI_Init(&argc, &argv);
    MPI_Comm_size(comm, &mpi_size);
    MPI_Comm_rank(comm, &mpi_rank);

    if (mpi_size != NUM_MPI_RANKS) {
        printf("These tests are set up to use %d ranks.\n", NUM_MPI_RANKS);
        printf("Quitting...\n");
        return 0;
    }

    /* Create test file */
    if ((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        goto exit;
    if (H5Pset_fapl_mpio(fapl, comm, info) < 0)
        goto exit;
    if (H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
        goto exit;

    if ((file_id = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        goto exit;
    if (H5Fclose(file_id) < 0)
        goto exit;

    for (i = 0; i < ARRAY_SIZE(tests); i++) {
        if (MPI_SUCCESS == (mpi_code = MPI_Barrier(comm))) {
            nerrors += (*tests[i])();
        } else {
            if (mpi_rank == 0) fprintf(stderr, "MPI_Barrier failed");
            nerrors++;
        }
    }

    if (nerrors) goto exit;

    puts("All Parallel Filters tests passed\n");

exit:
    if (H5Pclose(fapl) < 0)
        fprintf(stderr, "Couldn't close fapl.\n");

    if (nerrors)
        if (mpi_rank == 0) printf("*** %d TEST%s FAILED ***\n", nerrors, nerrors > 1 ? "S" : "");

    MPI_Finalize();

    exit(EXIT_SUCCESS);
}
