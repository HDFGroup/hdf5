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

#include "t_filters_parallel.h"

const char *FILENAME[] = {
        "t_filters_parallel",
        NULL
};
char filenames[1][256];

int nerrors = 0;

#define ARRAY_SIZE(a) sizeof(a) / sizeof(a[0])

static void test_one_chunk_filtered_dataset(void);
static void test_filtered_dataset_no_overlap(void);
static void test_filtered_dataset_overlap(void);
static void test_filtered_dataset_single_no_selection(void);
static void test_filtered_dataset_all_no_selection(void);
static void test_filtered_dataset_point_selection(void);
static void test_filtered_dataset_interleaved_write(void);
static void test_3d_filtered_dataset_no_overlap_separate_pages(void);
static void test_3d_filtered_dataset_no_overlap_same_pages(void);
static void test_3d_filtered_dataset_overlap(void);
static void test_cmpd_filtered_dataset_no_conversion_unshared(void);
static void test_cmpd_filtered_dataset_no_conversion_shared(void);
static void test_cmpd_filtered_dataset_type_conversion_unshared(void);
static void test_cmpd_filtered_dataset_type_conversion_shared(void);
static void test_write_serial_read_parallel(void);
static void test_write_parallel_read_serial(void);

static MPI_Comm comm = MPI_COMM_WORLD;
static MPI_Info info = MPI_INFO_NULL;
static int      mpi_rank;
static int      mpi_size;

static void (*tests[])(void) = {
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
static void
test_one_chunk_filtered_dataset(void)
{
    C_DATATYPE *data = NULL;
    C_DATATYPE *read_buf = NULL;
    C_DATATYPE *correct_buf = NULL;
    hsize_t     dataset_dims[ONE_CHUNK_FILTERED_DATASET_DIMS];
    hsize_t     chunk_dims[ONE_CHUNK_FILTERED_DATASET_DIMS];
    hsize_t     sel_dims[ONE_CHUNK_FILTERED_DATASET_DIMS];
    hsize_t     count[ONE_CHUNK_FILTERED_DATASET_DIMS];
    hsize_t     stride[ONE_CHUNK_FILTERED_DATASET_DIMS];
    hsize_t     block[ONE_CHUNK_FILTERED_DATASET_DIMS];
    hsize_t     offset[ONE_CHUNK_FILTERED_DATASET_DIMS];
    size_t      i, data_size, correct_buf_size;
    hid_t       file_id = -1, dset_id = -1, plist_id = -1;
    hid_t       filespace = -1, memspace = -1;

    if (MAINPROCESS) puts("Testing one-chunk filtered dataset");

    /* Set up file access property list with parallel I/O access */
    plist_id = H5Pcreate(H5P_FILE_ACCESS);
    VRFY((plist_id >= 0), "FAPL creation succeeded");

    VRFY((H5Pset_fapl_mpio(plist_id, comm, info) >= 0), "Set FAPL MPIO succeeded");

    VRFY((H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0), "Set libver bounds succeeded");

    file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, plist_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    VRFY((H5Pclose(plist_id) >= 0), "FAPL close succeeded");

    /* Create the dataspace for the dataset */
    dataset_dims[0] = (hsize_t) ONE_CHUNK_FILTERED_DATASET_NROWS;
    dataset_dims[1] = (hsize_t) ONE_CHUNK_FILTERED_DATASET_NCOLS;
    chunk_dims[0] = (hsize_t) ONE_CHUNK_FILTERED_DATASET_CH_NROWS;
    chunk_dims[1] = (hsize_t) ONE_CHUNK_FILTERED_DATASET_CH_NCOLS;
    sel_dims[0] = (hsize_t) ONE_CHUNK_FILTERED_DATASET_NROWS / (hsize_t) mpi_size;
    sel_dims[1] = (hsize_t) ONE_CHUNK_FILTERED_DATASET_NCOLS;

    filespace = H5Screate_simple(ONE_CHUNK_FILTERED_DATASET_DIMS, dataset_dims, NULL);
    VRFY((filespace >= 0), "File dataspace creation succeeded");

    memspace = H5Screate_simple(ONE_CHUNK_FILTERED_DATASET_DIMS, sel_dims, NULL);
    VRFY((memspace >= 0), "Memory dataspace creation succeeded");

    /* Create chunked dataset */
    plist_id = H5Pcreate(H5P_DATASET_CREATE);
    VRFY((plist_id >= 0), "DCPL creation succeeded");

    VRFY((H5Pset_chunk(plist_id, ONE_CHUNK_FILTERED_DATASET_DIMS, chunk_dims) >= 0), "Chunk size set");

    /* Add test filter to the pipeline */
    VRFY((SET_FILTER(plist_id) >= 0), "Filter set");

    dset_id = H5Dcreate2(file_id, ONE_CHUNK_FILTERED_DATASET_NAME, HDF5_DATATYPE_NAME, filespace,
            H5P_DEFAULT, plist_id, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset creation succeeded");

    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

    /* Each process defines the dataset selection in memory and writes
     * it to the hyperslab in the file
     */
    count[0] = 1;
    count[1] = 1;
    stride[0] = (hsize_t) ONE_CHUNK_FILTERED_DATASET_CH_NROWS;
    stride[1] = (hsize_t) ONE_CHUNK_FILTERED_DATASET_CH_NCOLS;
    block[0] = sel_dims[0];
    block[1] = sel_dims[1];
    offset[0] = ((hsize_t) mpi_rank * sel_dims[0]);
    offset[1] = 0;

    if (VERBOSE_MED)
        printf("Process %d: count[ %llu, %llu ], stride[ %llu, %llu ], offset[ %llu, %llu ], block size[ %llu, %llu ]\n",
            mpi_rank, count[0], count[1], stride[0], stride[1], offset[0], offset[1], block[0], block[1]);

    /* Select hyperslab in the file */
    filespace = H5Dget_space(dset_id);
    VRFY((filespace >= 0), "File dataspace retrieval succeeded");

    VRFY((H5Sselect_hyperslab(filespace, H5S_SELECT_SET, offset, stride, count, block) >= 0),
            "Hyperslab selection succeeded");

    /* Fill data buffer */
    data_size = (hsize_t) ONE_CHUNK_FILTERED_DATASET_CH_NROWS * (hsize_t) ONE_CHUNK_FILTERED_DATASET_NCOLS * sizeof(*data);
    correct_buf_size = dataset_dims[0] * dataset_dims[1] * sizeof(*correct_buf);

    data = (C_DATATYPE *) calloc(1, data_size);
    VRFY((NULL != data), "calloc succeeded");

    correct_buf = (C_DATATYPE *) calloc(1, correct_buf_size);
    VRFY((NULL != correct_buf), "calloc succeeded");

    for (i = 0; i < data_size / sizeof(*data); i++)
        data[i] = (C_DATATYPE) GEN_DATA(i);

    for (i = 0; i < correct_buf_size / sizeof(*correct_buf); i++)
        correct_buf[i] = ((C_DATATYPE) i % (ONE_CHUNK_FILTERED_DATASET_CH_NROWS / mpi_size * ONE_CHUNK_FILTERED_DATASET_CH_NCOLS))
                       + ((C_DATATYPE) i / (ONE_CHUNK_FILTERED_DATASET_CH_NROWS / mpi_size * ONE_CHUNK_FILTERED_DATASET_CH_NCOLS));

    /* Create property list for collective dataset write */
    plist_id = H5Pcreate(H5P_DATASET_XFER);
    VRFY((plist_id >= 0), "DXPL creation succeeded");

    VRFY((H5Pset_dxpl_mpio(plist_id, H5FD_MPIO_COLLECTIVE) >= 0), "Set DXPL MPIO succeeded");

    VRFY((H5Dwrite(dset_id, HDF5_DATATYPE_NAME, memspace, filespace, plist_id, data) >= 0), "Dataset write succeeded");

    if (data) free(data);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");

    /* Verify the correct data was written */
    read_buf = (C_DATATYPE *) calloc(1, correct_buf_size);
    VRFY((NULL != read_buf), "calloc succeeded");

    dset_id = H5Dopen2(file_id, "/" ONE_CHUNK_FILTERED_DATASET_NAME, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    VRFY((H5Dread(dset_id, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, plist_id, read_buf) >= 0), "Dataset read succeeded");

    VRFY((0 == memcmp(read_buf, correct_buf, correct_buf_size)), "Data verification succeeded");

    if (correct_buf) free(correct_buf);
    if (read_buf) free(read_buf);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Sclose(memspace) >= 0), "Memory dataspace close succeeded");
    VRFY((H5Pclose(plist_id) >= 0), "DXPL close succeeded");
    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    return;
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
static void
test_filtered_dataset_no_overlap(void)
{
    C_DATATYPE *data = NULL;
    C_DATATYPE *read_buf = NULL;
    C_DATATYPE *correct_buf = NULL;
    hsize_t     dataset_dims[UNSHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     chunk_dims[UNSHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     sel_dims[UNSHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     count[UNSHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     stride[UNSHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     block[UNSHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     offset[UNSHARED_FILTERED_CHUNKS_DATASET_DIMS];
    size_t      i, data_size, correct_buf_size;
    hid_t       file_id = -1, dset_id = -1, plist_id = -1;
    hid_t       filespace = -1, memspace = -1;

    if (MAINPROCESS) puts("Testing write to unshared filtered chunks");

    /* Set up file access property list with parallel I/O access */
    plist_id = H5Pcreate(H5P_FILE_ACCESS);
    VRFY((plist_id >= 0), "FAPL creation succeeded");

    VRFY((H5Pset_fapl_mpio(plist_id, comm, info) >= 0), "Set FAPL MPIO succeeded");

    VRFY((H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0), "Set libver bounds succeeded");

    file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, plist_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    VRFY((H5Pclose(plist_id) >= 0), "FAPL close succeeded");

    /* Create the dataspace for the dataset */
    dataset_dims[0] = (hsize_t) UNSHARED_FILTERED_CHUNKS_NROWS;
    dataset_dims[1] = (hsize_t) UNSHARED_FILTERED_CHUNKS_NCOLS;
    chunk_dims[0] = (hsize_t) UNSHARED_FILTERED_CHUNKS_CH_NROWS;
    chunk_dims[1] = (hsize_t) UNSHARED_FILTERED_CHUNKS_CH_NCOLS;
    sel_dims[0] = (hsize_t) UNSHARED_FILTERED_CHUNKS_CH_NROWS;
    sel_dims[1] = (hsize_t) UNSHARED_FILTERED_CHUNKS_NCOLS;

    filespace = H5Screate_simple(UNSHARED_FILTERED_CHUNKS_DATASET_DIMS, dataset_dims, NULL);
    VRFY((filespace >= 0), "File dataspace creation succeeded");

    memspace = H5Screate_simple(UNSHARED_FILTERED_CHUNKS_DATASET_DIMS, sel_dims, NULL);
    VRFY((memspace >= 0), "Memory dataspace creation succeeded");

    /* Create chunked dataset */
    plist_id = H5Pcreate(H5P_DATASET_CREATE);
    VRFY((plist_id >= 0), "DCPL creation succeeded");

    VRFY((H5Pset_chunk(plist_id, UNSHARED_FILTERED_CHUNKS_DATASET_DIMS, chunk_dims) >= 0), "Chunk size set");

    /* Add test filter to the pipeline */
    VRFY((SET_FILTER(plist_id) >= 0), "Filter set");

    dset_id = H5Dcreate2(file_id, UNSHARED_FILTERED_CHUNKS_DATASET_NAME, HDF5_DATATYPE_NAME, filespace,
            H5P_DEFAULT, plist_id, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset creation succeeded");

    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

    /* Each process defines the dataset selection in memory and writes
     * it to the hyperslab in the file
     */
    count[0] = 1;
    count[1] = (hsize_t) UNSHARED_FILTERED_CHUNKS_NCOLS / (hsize_t) UNSHARED_FILTERED_CHUNKS_CH_NCOLS;
    stride[0] = (hsize_t) UNSHARED_FILTERED_CHUNKS_CH_NROWS;
    stride[1] = (hsize_t) UNSHARED_FILTERED_CHUNKS_CH_NCOLS;
    block[0] = (hsize_t) UNSHARED_FILTERED_CHUNKS_CH_NROWS;
    block[1] = (hsize_t) UNSHARED_FILTERED_CHUNKS_CH_NCOLS;
    offset[0] = ((hsize_t) mpi_rank * (hsize_t) UNSHARED_FILTERED_CHUNKS_CH_NROWS * count[0]);
    offset[1] = 0;

    if (VERBOSE_MED)
        printf("Process %d: count[ %llu, %llu ], stride[ %llu, %llu ], offset[ %llu, %llu ], block size[ %llu, %llu ]\n",
            mpi_rank, count[0], count[1], stride[0], stride[1], offset[0], offset[1], block[0], block[1]);

    /* Select hyperslab in the file */
    filespace = H5Dget_space(dset_id);
    VRFY((dset_id >= 0), "File dataspace retrieval succeeded");

    VRFY((H5Sselect_hyperslab(filespace, H5S_SELECT_SET, offset, stride, count, block) >= 0), "Hyperslab selection succeeded");

    /* Fill data buffer */
    data_size = sel_dims[0] * sel_dims[1] * sizeof(*data);
    correct_buf_size = dataset_dims[0] * dataset_dims[1] * sizeof(*correct_buf);

    data = (C_DATATYPE *) calloc(1, data_size);
    VRFY((NULL != data), "calloc succeeded");

    correct_buf = (C_DATATYPE *) calloc(1, correct_buf_size);
    VRFY((NULL != correct_buf), "calloc succeeded");

    for (i = 0; i < data_size / sizeof(*data); i++)
        data[i] = (C_DATATYPE) GEN_DATA(i);

    for (i = 0; i < correct_buf_size / sizeof(*correct_buf); i++)
        correct_buf[i] = (C_DATATYPE) ( (i % (dataset_dims[0] / (hsize_t) mpi_size * dataset_dims[1]))
                                      + (i / (dataset_dims[0] / (hsize_t) mpi_size * dataset_dims[1])));

    /* Create property list for collective dataset write */
    plist_id = H5Pcreate(H5P_DATASET_XFER);
    VRFY((plist_id >= 0), "DXPL creation succeeded");

    VRFY((H5Pset_dxpl_mpio(plist_id, H5FD_MPIO_COLLECTIVE) >= 0), "Set DXPL MPIO succeeded");

    VRFY((H5Dwrite(dset_id, HDF5_DATATYPE_NAME, memspace, filespace, plist_id, data) >= 0), "Dataset write succeeded");

    if (data) free(data);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");

    /* Verify the correct data was written */
    read_buf = (C_DATATYPE *) calloc(1, correct_buf_size);
    VRFY((NULL != read_buf), "calloc succeeded");

    dset_id = H5Dopen2(file_id, "/" UNSHARED_FILTERED_CHUNKS_DATASET_NAME, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    VRFY((H5Dread(dset_id, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, plist_id, read_buf) >= 0), "Dataset read succeeded");

    VRFY((0 == memcmp(read_buf, correct_buf, correct_buf_size)), "Data verification succeeded");

    if (correct_buf) free(correct_buf);
    if (read_buf) free(read_buf);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Sclose(memspace) >= 0), "Memory dataspace close succeeded");
    VRFY((H5Pclose(plist_id) >= 0), "DXPL close succeeded");
    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    return;
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
static void
test_filtered_dataset_overlap(void)
{
    C_DATATYPE *data = NULL;
    C_DATATYPE *read_buf = NULL;
    C_DATATYPE *correct_buf = NULL;
    hsize_t     dataset_dims[SHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     chunk_dims[SHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     sel_dims[SHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     count[SHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     stride[SHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     block[SHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     offset[SHARED_FILTERED_CHUNKS_DATASET_DIMS];
    size_t      i, data_size, correct_buf_size;
    hid_t       file_id = -1, dset_id = -1, plist_id = -1;
    hid_t       filespace = -1, memspace = -1;

    if (MAINPROCESS) puts("Testing write to shared filtered chunks");

    /* Set up file access property list with parallel I/O access */
    plist_id = H5Pcreate(H5P_FILE_ACCESS);
    VRFY((plist_id >= 0), "FAPL creation succeeded");

    VRFY((H5Pset_fapl_mpio(plist_id, comm, info) >= 0), "Set FAPL MPIO succeeded");

    VRFY((H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0), "Set libver bounds succeeded");

    file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, plist_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    VRFY((H5Pclose(plist_id) >= 0), "FAPL close succeeded");

    /* Create the dataspace for the dataset */
    dataset_dims[0] = (hsize_t) SHARED_FILTERED_CHUNKS_NROWS;
    dataset_dims[1] = (hsize_t) SHARED_FILTERED_CHUNKS_NCOLS;
    chunk_dims[0] = (hsize_t) SHARED_FILTERED_CHUNKS_CH_NROWS;
    chunk_dims[1] = (hsize_t) SHARED_FILTERED_CHUNKS_CH_NCOLS;
    sel_dims[0] = (hsize_t) DIM0_SCALE_FACTOR;
    sel_dims[1] = (hsize_t) SHARED_FILTERED_CHUNKS_CH_NCOLS * (hsize_t) DIM1_SCALE_FACTOR;

    filespace = H5Screate_simple(SHARED_FILTERED_CHUNKS_DATASET_DIMS, dataset_dims, NULL);
    VRFY((filespace >= 0), "File dataspace creation succeeded");

    memspace = H5Screate_simple(SHARED_FILTERED_CHUNKS_DATASET_DIMS, sel_dims, NULL);
    VRFY((memspace >= 0), "Memory dataspace creation succeeded");

    /* Create chunked dataset */
    plist_id = H5Pcreate(H5P_DATASET_CREATE);
    VRFY((plist_id >= 0), "DCPL creation succeeded");

    VRFY((H5Pset_chunk(plist_id, SHARED_FILTERED_CHUNKS_DATASET_DIMS, chunk_dims) >= 0), "Chunk size set");

    /* Add test filter to the pipeline */
    VRFY((SET_FILTER(plist_id) >= 0), "Filter set");

    dset_id = H5Dcreate2(file_id, SHARED_FILTERED_CHUNKS_DATASET_NAME, HDF5_DATATYPE_NAME, filespace,
            H5P_DEFAULT, plist_id, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset creation succeeded");

    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

    /* Each process defines the dataset selection in memory and writes
     * it to the hyperslab in the file
     */
    count[0] = (hsize_t) SHARED_FILTERED_CHUNKS_NROWS / (hsize_t) SHARED_FILTERED_CHUNKS_CH_NROWS;
    count[1] = (hsize_t) SHARED_FILTERED_CHUNKS_NCOLS / (hsize_t) SHARED_FILTERED_CHUNKS_CH_NCOLS;
    stride[0] = (hsize_t) SHARED_FILTERED_CHUNKS_CH_NROWS;
    stride[1] = (hsize_t) SHARED_FILTERED_CHUNKS_CH_NCOLS;
    block[0] = (hsize_t) SHARED_FILTERED_CHUNKS_CH_NROWS / (hsize_t) mpi_size;
    block[1] = (hsize_t) SHARED_FILTERED_CHUNKS_CH_NCOLS;
    offset[0] = (hsize_t) mpi_rank * block[0];
    offset[1] = 0;

    if (VERBOSE_MED)
        printf("Process %d is writing with count[ %llu, %llu ], stride[ %llu, %llu ], offset[ %llu, %llu ], block size[ %llu, %llu ]\n",
            mpi_rank, count[0], count[1], stride[0], stride[1], offset[0], offset[1], block[0], block[1]);

    /* Select hyperslab in the file */
    filespace = H5Dget_space(dset_id);
    VRFY((filespace >= 0), "File dataspace retrieval succeeded");

    VRFY((H5Sselect_hyperslab(filespace, H5S_SELECT_SET, offset, stride, count, block) >= 0), "Hyperslab selection succeeded");

    /* Fill data buffer */
    data_size = sel_dims[0] * sel_dims[1] * sizeof(*data);
    correct_buf_size = dataset_dims[0] * dataset_dims[1] * sizeof(*correct_buf);

    data = (C_DATATYPE *) calloc(1, data_size);
    VRFY((NULL != data), "calloc succeeded");

    correct_buf = (C_DATATYPE *) calloc(1, correct_buf_size);
    VRFY((NULL != correct_buf), "calloc succeeded");

    for (i = 0; i < data_size / sizeof(*data); i++)
        data[i] = (C_DATATYPE) GEN_DATA(i);

    for (i = 0; i < correct_buf_size / sizeof(*correct_buf); i++)
        correct_buf[i] = (C_DATATYPE)  ((dataset_dims[1] * (i / ((hsize_t) mpi_size * dataset_dims[1])))
                                     + (i % dataset_dims[1])
                                     + (((i % ((hsize_t) mpi_size * dataset_dims[1])) / dataset_dims[1]) % dataset_dims[1]));

    /* Create property list for collective dataset write */
    plist_id = H5Pcreate(H5P_DATASET_XFER);
    VRFY((plist_id >= 0), "DXPL creation succeeded");

    VRFY((H5Pset_dxpl_mpio(plist_id, H5FD_MPIO_COLLECTIVE) >= 0), "Set DXPL MPIO succeeded");

    VRFY((H5Dwrite(dset_id, HDF5_DATATYPE_NAME, memspace, filespace, plist_id, data) >= 0), "Dataset write succeeded");

    if (data) free(data);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");

    /* Verify correct data was written */
    read_buf = (C_DATATYPE *) calloc(1, correct_buf_size);
    VRFY((NULL != read_buf), "calloc succeeded");

    dset_id = H5Dopen2(file_id, "/" SHARED_FILTERED_CHUNKS_DATASET_NAME, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    VRFY((H5Dread(dset_id, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, plist_id, read_buf) >= 0), "Dataset read succeeded");

    VRFY((0 == memcmp(read_buf, correct_buf, correct_buf_size)), "Data verification succeeded");

    if (correct_buf) free(correct_buf);
    if (read_buf) free(read_buf);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Sclose(memspace) >= 0), "Memory dataspace close succeeded");
    VRFY((H5Pclose(plist_id) >= 0), "DXPL close succeeded");
    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    return;
}

/*
 * Tests parallel write of filtered data in the case where
 * a single process in the write operation has no selection
 * in the dataset's dataspace. In this case, the process with
 * no selection still has to participate in the collective
 * space re-allocation for the filtered chunks and also must
 * participate in the re-insertion of the filtered chunks
 * into the chunk index.
 *
 * Programmer: Jordan Henderson
 *             02/01/2017
 */
static void
test_filtered_dataset_single_no_selection(void)
{
    C_DATATYPE *data = NULL;
    C_DATATYPE *read_buf = NULL;
    C_DATATYPE *correct_buf = NULL;
    hsize_t     dataset_dims[SINGLE_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     chunk_dims[SINGLE_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     sel_dims[SINGLE_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     count[SINGLE_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     stride[SINGLE_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     block[SINGLE_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     offset[SINGLE_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    size_t      i, data_size, correct_buf_size;
    size_t      segment_length;
    hid_t       file_id = -1, dset_id = -1, plist_id = -1;
    hid_t       filespace = -1, memspace = -1;

    if (MAINPROCESS) puts("Testing write to filtered chunks with a single process having no selection");

    /* Set up file access property list with parallel I/O access */
    plist_id = H5Pcreate(H5P_FILE_ACCESS);
    VRFY((plist_id >= 0), "FAPL creation succeeded");

    VRFY((H5Pset_fapl_mpio(plist_id, comm, info) >= 0), "Set FAPL MPIO succeeded");

    VRFY((H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0), "Set libver bounds succeeded");

    file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, plist_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    VRFY((H5Pclose(plist_id) >= 0), "FAPL close succeeded");

    /* Create the dataspace for the dataset */
    dataset_dims[0] = (hsize_t) SINGLE_NO_SELECTION_FILTERED_CHUNKS_NROWS;
    dataset_dims[1] = (hsize_t) SINGLE_NO_SELECTION_FILTERED_CHUNKS_NCOLS;
    chunk_dims[0] = (hsize_t) SINGLE_NO_SELECTION_FILTERED_CHUNKS_CH_NROWS;
    chunk_dims[1] = (hsize_t) SINGLE_NO_SELECTION_FILTERED_CHUNKS_CH_NCOLS;
    sel_dims[0] = (hsize_t) SINGLE_NO_SELECTION_FILTERED_CHUNKS_CH_NROWS;
    sel_dims[1] = (hsize_t) SINGLE_NO_SELECTION_FILTERED_CHUNKS_NCOLS;

    if (mpi_rank == SINGLE_NO_SELECTION_FILTERED_CHUNKS_NO_SELECT_PROC)
        sel_dims[0] = sel_dims[1] = 0;

    filespace = H5Screate_simple(SINGLE_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS, dataset_dims, NULL);
    VRFY((filespace >= 0), "File dataspace creation succeeded");

    memspace = H5Screate_simple(SINGLE_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS, sel_dims, NULL);
    VRFY((memspace >= 0), "Memory dataspace creation succeeded");

    /* Create chunked dataset */
    plist_id = H5Pcreate(H5P_DATASET_CREATE);
    VRFY((plist_id >= 0), "DCPL creation succeeded");

    VRFY((H5Pset_chunk(plist_id, SINGLE_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS, chunk_dims) >= 0), "Chunk size set");

    /* Add test filter to the pipeline */
    VRFY((SET_FILTER(plist_id) >= 0), "Filter set");

    dset_id = H5Dcreate2(file_id, SINGLE_NO_SELECTION_FILTERED_CHUNKS_DATASET_NAME, HDF5_DATATYPE_NAME, filespace,
            H5P_DEFAULT, plist_id, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset creation succeeded");

    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

    /* Each process defines the dataset selection in memory and writes
     * it to the hyperslab in the file
     */
    count[0] = 1;
    count[1] = (hsize_t) SINGLE_NO_SELECTION_FILTERED_CHUNKS_NCOLS / (hsize_t) SINGLE_NO_SELECTION_FILTERED_CHUNKS_CH_NCOLS;
    stride[0] = (hsize_t) SINGLE_NO_SELECTION_FILTERED_CHUNKS_CH_NROWS;
    stride[1] = (hsize_t) SINGLE_NO_SELECTION_FILTERED_CHUNKS_CH_NCOLS;
    block[0] = (hsize_t) SINGLE_NO_SELECTION_FILTERED_CHUNKS_CH_NROWS;
    block[1] = (hsize_t) SINGLE_NO_SELECTION_FILTERED_CHUNKS_CH_NCOLS;
    offset[0] = (hsize_t) mpi_rank * (hsize_t) SINGLE_NO_SELECTION_FILTERED_CHUNKS_CH_NROWS * count[0];
    offset[1] = 0;

    if (VERBOSE_MED)
        printf("Process %d is writing with count[ %llu, %llu ], stride[ %llu, %llu ], offset[ %llu, %llu ], block size[ %llu, %llu ]\n",
            mpi_rank, count[0], count[1], stride[0], stride[1], offset[0], offset[1], block[0], block[1]);

    /* Select hyperslab in the file */
    filespace = H5Dget_space(dset_id);
    VRFY((filespace >= 0), "File dataspace retrieval succeeded");

    if (mpi_rank == SINGLE_NO_SELECTION_FILTERED_CHUNKS_NO_SELECT_PROC)
        VRFY((H5Sselect_none(filespace) >= 0), "Select none succeeded");
    else
        VRFY((H5Sselect_hyperslab(filespace, H5S_SELECT_SET, offset, stride, count, block) >= 0), "Hyperslab selection succeeded");

    /* Fill data buffer */
    data_size = sel_dims[0] * sel_dims[1] * sizeof(*data);
    correct_buf_size = dataset_dims[0] * dataset_dims[1] * sizeof(*correct_buf);

    data = (C_DATATYPE *) calloc(1, data_size);
    VRFY((NULL != data), "calloc succeeded");

    correct_buf = (C_DATATYPE *) calloc(1, correct_buf_size);
    VRFY((NULL != correct_buf), "calloc succeeded");

    for (i = 0; i < data_size / sizeof(*data); i++)
        data[i] = (C_DATATYPE) GEN_DATA(i);

    for (i = 0; i < correct_buf_size / sizeof(*correct_buf); i++)
        correct_buf[i] = (C_DATATYPE) ( (i % (dataset_dims[0] / (hsize_t) mpi_size * dataset_dims[1]))
                                      + (i / (dataset_dims[0] / (hsize_t) mpi_size * dataset_dims[1])));

    /* Compute the correct offset into the buffer for the process having no selection and clear it */
    segment_length = dataset_dims[0] * dataset_dims[1] / (hsize_t) mpi_size;
    HDmemset(correct_buf + ((size_t) SINGLE_NO_SELECTION_FILTERED_CHUNKS_NO_SELECT_PROC * segment_length), 0, segment_length * sizeof(*data));

    /* Create property list for collective dataset write */
    plist_id = H5Pcreate(H5P_DATASET_XFER);
    VRFY((plist_id >= 0), "DXPL creation succeeded");

    VRFY((H5Pset_dxpl_mpio(plist_id, H5FD_MPIO_COLLECTIVE) >= 0), "Set DXPL MPIO succeeded");

    VRFY((H5Dwrite(dset_id, HDF5_DATATYPE_NAME, memspace, filespace, plist_id, data) >= 0), "Dataset write succeeded");

    if (data) free(data);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");

    /* Verify the correct data was written */
    read_buf = (C_DATATYPE *) calloc(1, correct_buf_size);
    VRFY((NULL != read_buf), "calloc succeeded");

    dset_id = H5Dopen2(file_id, "/" SINGLE_NO_SELECTION_FILTERED_CHUNKS_DATASET_NAME, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    VRFY((H5Dread(dset_id, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, plist_id, read_buf) >= 0), "Dataset read succeeded");

    VRFY((0 == memcmp(read_buf, correct_buf, correct_buf_size)), "Data verification succeeded");

    if (correct_buf) free(correct_buf);
    if (read_buf) free(read_buf);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Sclose(memspace) >= 0), "Memory dataspace close succeeded");
    VRFY((H5Pclose(plist_id) >= 0), "DXPL close succeeded");
    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    return;
}

/*
 * Tests parallel write of filtered data in the case
 * where no process in the write operation has a
 * selection in the dataset's dataspace. This test is
 * to ensure that there are no assertion failures or
 * similar issues due to size 0 allocations and the
 * like. In this case, the file and dataset are created
 * but the dataset is populated with the default fill
 * value.
 *
 * Programmer: Jordan Henderson
 *             02/02/2017
 */
static void
test_filtered_dataset_all_no_selection(void)
{
    C_DATATYPE *data = NULL;
    C_DATATYPE *read_buf = NULL;
    C_DATATYPE *correct_buf = NULL;
    hsize_t     dataset_dims[ALL_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     chunk_dims[ALL_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     sel_dims[ALL_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    size_t      i, data_size, correct_buf_size;
    hid_t       file_id = -1, dset_id = -1, plist_id = -1;
    hid_t       filespace = -1, memspace = -1;

    if (MAINPROCESS) puts("Testing write to filtered chunks with all processes having no selection");

    /* Set up file access property list with parallel I/O access */
    plist_id = H5Pcreate(H5P_FILE_ACCESS);
    VRFY((plist_id >= 0), "FAPL creation succeeded");

    VRFY((H5Pset_fapl_mpio(plist_id, comm, info) >= 0), "Set FAPL MPIO succeeded");

    VRFY((H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0), "Set libver bounds succeeded");

    file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, plist_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    VRFY((H5Pclose(plist_id) >= 0), "FAPL close succeeded");

    /* Create the dataspace for the dataset */
    dataset_dims[0] = (hsize_t) ALL_NO_SELECTION_FILTERED_CHUNKS_NROWS;
    dataset_dims[1] = (hsize_t) ALL_NO_SELECTION_FILTERED_CHUNKS_NCOLS;
    chunk_dims[0] = (hsize_t) ALL_NO_SELECTION_FILTERED_CHUNKS_CH_NROWS;
    chunk_dims[1] = (hsize_t) ALL_NO_SELECTION_FILTERED_CHUNKS_CH_NCOLS;
    sel_dims[0] = sel_dims[1] = 0;

    filespace = H5Screate_simple(ALL_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS, dataset_dims, NULL);
    VRFY((filespace >= 0), "File dataspace creation succeeded");

    memspace = H5Screate_simple(ALL_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS, sel_dims, NULL);
    VRFY((memspace >= 0), "Memory dataspace creation succeeded");

    /* Create chunked dataset */
    plist_id = H5Pcreate(H5P_DATASET_CREATE);
    VRFY((plist_id >= 0), "DCPL creation succeeded");

    VRFY((H5Pset_chunk(plist_id, ALL_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS, chunk_dims) >= 0), "Chunk size set");

    /* Add test filter to the pipeline */
    VRFY((SET_FILTER(plist_id) >= 0), "Filter set");

    dset_id = H5Dcreate2(file_id, ALL_NO_SELECTION_FILTERED_CHUNKS_DATASET_NAME, HDF5_DATATYPE_NAME, filespace,
            H5P_DEFAULT, plist_id, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset creation succeeded");

    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

    filespace = H5Dget_space(dset_id);
    VRFY((filespace >= 0), "File dataspace retrieval succeeded");

    VRFY((H5Sselect_none(filespace) >= 0), "Select none succeeded");

    /* Fill data buffer */
    data_size = sel_dims[0] * sel_dims[1] * sizeof(*data);
    correct_buf_size = dataset_dims[0] * dataset_dims[1] * sizeof(*correct_buf);

    data = (C_DATATYPE *) calloc(1, data_size);
    VRFY((NULL != data), "calloc succeeded");

    correct_buf = (C_DATATYPE *) calloc(1, correct_buf_size);
    VRFY((NULL != correct_buf), "calloc succeeded");

    for (i = 0; i < data_size / sizeof(*data); i++)
        data[i] = (C_DATATYPE) GEN_DATA(i);

    /* Create property list for collective dataset write */
    plist_id = H5Pcreate(H5P_DATASET_XFER);
    VRFY((plist_id >= 0), "DXPL creation succeeded");

    VRFY((H5Pset_dxpl_mpio(plist_id, H5FD_MPIO_COLLECTIVE) >= 0), "Set DXPL MPIO succeeded");

    VRFY((H5Dwrite(dset_id, HDF5_DATATYPE_NAME, memspace, filespace, plist_id, data) >= 0), "Dataset write succeeded");

    if (data) free(data);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");

    /* Verify the correct data was written */
    read_buf = (C_DATATYPE *) calloc(1, correct_buf_size);
    VRFY((NULL != read_buf), "calloc succeeded");

    dset_id = H5Dopen2(file_id, "/" ALL_NO_SELECTION_FILTERED_CHUNKS_DATASET_NAME, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    VRFY((H5Dread(dset_id, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, plist_id, read_buf) >= 0), "Dataset read succeeded");

    VRFY((0 == memcmp(read_buf, correct_buf, correct_buf_size)), "Data verification succeeded");

    if (correct_buf) free(correct_buf);
    if (read_buf) free(read_buf);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Sclose(memspace) >= 0), "Memory dataspace close succeeded");
    VRFY((H5Pclose(plist_id) >= 0), "DXPL close succeeded");
    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    return;
}

/*
 * Tests parallel write of filtered data by using
 * point selections instead of hyperslab selections.
 *
 * Programmer: Jordan Henderson
 *             02/02/2017
 */
static void
test_filtered_dataset_point_selection(void)
{
    C_DATATYPE *data = NULL;
    C_DATATYPE *correct_buf = NULL;
    C_DATATYPE *read_buf = NULL;
    hsize_t    *coords = NULL;
    hsize_t     dataset_dims[POINT_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     chunk_dims[POINT_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     sel_dims[POINT_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    size_t      i, j, data_size, correct_buf_size;
    size_t      num_points;
    hid_t       file_id = -1, dset_id = -1, plist_id = -1;
    hid_t       filespace = -1, memspace = -1;

    if (MAINPROCESS) puts("Testing write to filtered chunks with point selection");

    /* Set up file access property list with parallel I/O access */
    plist_id = H5Pcreate(H5P_FILE_ACCESS);
    VRFY((plist_id >= 0), "FAPL creation succeeded");

    VRFY((H5Pset_fapl_mpio(plist_id, comm, info) >= 0), "Set FAPL MPIO succeeded");

    VRFY((H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0), "Set libver bounds succeeded");

    file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, plist_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    VRFY((H5Pclose(plist_id) >= 0), "FAPL close succeeded");

    /* Create the dataspace for the dataset */
    dataset_dims[0] = (hsize_t) POINT_SELECTION_FILTERED_CHUNKS_NROWS;
    dataset_dims[1] = (hsize_t) POINT_SELECTION_FILTERED_CHUNKS_NCOLS;
    chunk_dims[0] = (hsize_t) POINT_SELECTION_FILTERED_CHUNKS_CH_NROWS;
    chunk_dims[1] = (hsize_t) POINT_SELECTION_FILTERED_CHUNKS_CH_NCOLS;
    sel_dims[0] = (hsize_t) POINT_SELECTION_FILTERED_CHUNKS_NROWS / (hsize_t) mpi_size;
    sel_dims[1] = (hsize_t) POINT_SELECTION_FILTERED_CHUNKS_NCOLS;

    filespace = H5Screate_simple(POINT_SELECTION_FILTERED_CHUNKS_DATASET_DIMS, dataset_dims, NULL);
    VRFY((filespace >= 0), "File dataspace creation succeeded");

    memspace = H5Screate_simple(POINT_SELECTION_FILTERED_CHUNKS_DATASET_DIMS, sel_dims, NULL);
    VRFY((memspace >= 0), "Memory dataspace creation succeeded");

    /* Create chunked dataset */
    plist_id = H5Pcreate(H5P_DATASET_CREATE);
    VRFY((plist_id >= 0), "DCPL creation succeeded");

    VRFY((H5Pset_chunk(plist_id, POINT_SELECTION_FILTERED_CHUNKS_DATASET_DIMS, chunk_dims) >= 0), "Chunk size set");

    /* Add test filter to the pipeline */
    VRFY((SET_FILTER(plist_id) >= 0), "Filter set");

    dset_id = H5Dcreate2(file_id, POINT_SELECTION_FILTERED_CHUNKS_DATASET_NAME, HDF5_DATATYPE_NAME, filespace,
            H5P_DEFAULT, plist_id, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset creation succeeded");

    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

    /* Set up point selection */
    filespace = H5Dget_space(dset_id);
    VRFY((filespace >= 0), "File dataspace retrieval succeeded");

    num_points = (hsize_t) POINT_SELECTION_FILTERED_CHUNKS_NROWS * (hsize_t) POINT_SELECTION_FILTERED_CHUNKS_NCOLS / (hsize_t) mpi_size;
    coords = (hsize_t *) calloc(1, 2 * num_points * sizeof(*coords));
    VRFY((NULL != coords), "Coords calloc succeeded");

    for (i = 0; i < num_points; i++)
        for (j = 0; j < POINT_SELECTION_FILTERED_CHUNKS_DATASET_DIMS; j++)
            coords[(i * POINT_SELECTION_FILTERED_CHUNKS_DATASET_DIMS) + j] = (j > 0) ? (i % (hsize_t) POINT_SELECTION_FILTERED_CHUNKS_NCOLS)
                                                                                     : ((hsize_t) mpi_rank + ((hsize_t) mpi_size * (i / (hsize_t) POINT_SELECTION_FILTERED_CHUNKS_NCOLS)));

    VRFY((H5Sselect_elements(filespace, H5S_SELECT_SET, (hsize_t) num_points, (const hsize_t *) coords) >= 0),
            "Point selection succeeded");

    /* Fill data buffer */
    data_size = sel_dims[0] * sel_dims[1] * sizeof(*data);
    correct_buf_size = dataset_dims[0] * dataset_dims[1] * sizeof(*correct_buf);

    data = (C_DATATYPE *) calloc(1, data_size);
    VRFY((NULL != data), "calloc succeeded");

    correct_buf = (C_DATATYPE *) calloc(1, correct_buf_size);
    VRFY((NULL != correct_buf), "calloc succeeded");

    for (i = 0; i < data_size / sizeof(*data); i++)
        data[i] = (C_DATATYPE) GEN_DATA(i);

    for (i = 0; i < correct_buf_size / sizeof(*correct_buf); i++)
        correct_buf[i] = (C_DATATYPE)  ((dataset_dims[1] * (i / ((hsize_t) mpi_size * dataset_dims[1])))
                                     + (i % dataset_dims[1])
                                     + (((i % ((hsize_t) mpi_size * dataset_dims[1])) / dataset_dims[1]) % dataset_dims[1]));

    /* Create property list for collective dataset write */
    plist_id = H5Pcreate(H5P_DATASET_XFER);
    VRFY((plist_id >= 0), "DXPL creation succeeded");

    VRFY((H5Pset_dxpl_mpio(plist_id, H5FD_MPIO_COLLECTIVE) >= 0), "Set DXPL MPIO succeeded");

    VRFY((H5Dwrite(dset_id, HDF5_DATATYPE_NAME, memspace, filespace, plist_id, data) >= 0), "Dataset write succeeded");

    if (data) free(data);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");

    /* Verify the correct data was written */
    read_buf = (C_DATATYPE *) calloc(1, correct_buf_size);
    VRFY((NULL != read_buf), "calloc succeeded");

    dset_id = H5Dopen2(file_id, "/" POINT_SELECTION_FILTERED_CHUNKS_DATASET_NAME, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    VRFY((H5Dread(dset_id, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, plist_id, read_buf) >= 0), "Dataset read succeeded");

    VRFY((0 == memcmp(read_buf, correct_buf, correct_buf_size)), "Data verification succeeded");

    if (coords) free(coords);
    if (correct_buf) free(correct_buf);
    if (read_buf) free(read_buf);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Sclose(memspace) >= 0), "Memory dataspace close succeeded");
    VRFY((H5Pclose(plist_id) >= 0), "DXPL close succeeded");
    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    return;
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
static void
test_filtered_dataset_interleaved_write(void)
{
    C_DATATYPE *data = NULL;
    C_DATATYPE *read_buf = NULL;
    C_DATATYPE *correct_buf = NULL;
    hsize_t     dataset_dims[INTERLEAVED_WRITE_FILTERED_DATASET_DIMS];
    hsize_t     chunk_dims[INTERLEAVED_WRITE_FILTERED_DATASET_DIMS];
    hsize_t     sel_dims[INTERLEAVED_WRITE_FILTERED_DATASET_DIMS];
    hsize_t     count[INTERLEAVED_WRITE_FILTERED_DATASET_DIMS];
    hsize_t     stride[INTERLEAVED_WRITE_FILTERED_DATASET_DIMS];
    hsize_t     block[INTERLEAVED_WRITE_FILTERED_DATASET_DIMS];
    hsize_t     offset[INTERLEAVED_WRITE_FILTERED_DATASET_DIMS];
    size_t      i, data_size, correct_buf_size;
    hid_t       file_id = -1, dset_id = -1, plist_id = -1;
    hid_t       filespace = -1, memspace = -1;

    if (MAINPROCESS) puts("Testing interleaved write to filtered chunks");

    /* Set up file access property list with parallel I/O access */
    plist_id = H5Pcreate(H5P_FILE_ACCESS);
    VRFY((plist_id >= 0), "FAPL creation succeeded");

    VRFY((H5Pset_fapl_mpio(plist_id, comm, info) >= 0), "Set FAPL MPIO succeeded");

    VRFY((H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0), "Set libver bounds succeeded");

    file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, plist_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    VRFY((H5Pclose(plist_id) >= 0), "FAPL close succeeded");

    /* Create the dataspace for the dataset */
    dataset_dims[0] = (hsize_t) INTERLEAVED_WRITE_FILTERED_DATASET_NROWS;
    dataset_dims[1] = (hsize_t) INTERLEAVED_WRITE_FILTERED_DATASET_NCOLS;
    chunk_dims[0] = (hsize_t) INTERLEAVED_WRITE_FILTERED_DATASET_CH_NROWS;
    chunk_dims[1] = (hsize_t) INTERLEAVED_WRITE_FILTERED_DATASET_CH_NCOLS;
    sel_dims[0] = (hsize_t) (INTERLEAVED_WRITE_FILTERED_DATASET_NROWS / mpi_size);
    sel_dims[1] = (hsize_t) INTERLEAVED_WRITE_FILTERED_DATASET_NCOLS;

    filespace = H5Screate_simple(INTERLEAVED_WRITE_FILTERED_DATASET_DIMS, dataset_dims, NULL);
    VRFY((filespace >= 0), "File dataspace creation succeeded");

    memspace = H5Screate_simple(INTERLEAVED_WRITE_FILTERED_DATASET_DIMS, sel_dims, NULL);
    VRFY((memspace >= 0), "Memory dataspace creation succeeded");

    /* Create chunked dataset */
    plist_id = H5Pcreate(H5P_DATASET_CREATE);
    VRFY((plist_id >= 0), "DCPL creation succeeded");

    VRFY((H5Pset_chunk(plist_id, INTERLEAVED_WRITE_FILTERED_DATASET_DIMS, chunk_dims) >= 0), "Chunk size set");

    /* Add test filter to the pipeline */
    VRFY((SET_FILTER(plist_id) >= 0), "Filter set");

    dset_id = H5Dcreate2(file_id, INTERLEAVED_WRITE_FILTERED_DATASET_NAME, HDF5_DATATYPE_NAME, filespace,
            H5P_DEFAULT, plist_id, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset creation succeeded");

    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

    /* Each process defines the dataset selection in memory and writes
     * it to the hyperslab in the file
     */
    count[0] = (hsize_t) (INTERLEAVED_WRITE_FILTERED_DATASET_NROWS / INTERLEAVED_WRITE_FILTERED_DATASET_CH_NROWS);
    count[1] = (hsize_t) (INTERLEAVED_WRITE_FILTERED_DATASET_NCOLS / INTERLEAVED_WRITE_FILTERED_DATASET_CH_NCOLS);
    stride[0] = (hsize_t) INTERLEAVED_WRITE_FILTERED_DATASET_CH_NROWS;
    stride[1] = (hsize_t) INTERLEAVED_WRITE_FILTERED_DATASET_CH_NCOLS;
    block[0] = 1;
    block[1] = (hsize_t) INTERLEAVED_WRITE_FILTERED_DATASET_CH_NCOLS;
    offset[0] = (hsize_t) mpi_rank;
    offset[1] = 0;

    if (VERBOSE_MED)
        printf("Process %d is writing with count[ %llu, %llu ], stride[ %llu, %llu ], offset[ %llu, %llu ], block size[ %llu, %llu ]\n",
            mpi_rank, count[0], count[1], stride[0], stride[1], offset[0], offset[1], block[0], block[1]);

    /* Select hyperslab in the file */
    filespace = H5Dget_space(dset_id);
    VRFY((filespace >= 0), "File dataspace retrieval succeeded");

    VRFY((H5Sselect_hyperslab(filespace, H5S_SELECT_SET, offset, stride, count, block) >= 0), "Hyperslab selection succeeded");

    /* Fill data buffer */
    data_size = sel_dims[0] * sel_dims[1] * sizeof(*data);
    correct_buf_size = dataset_dims[0] * dataset_dims[1] * sizeof(*correct_buf);

    data = (C_DATATYPE *) calloc(1, data_size);
    VRFY((NULL != data), "calloc succeeded");

    correct_buf = (C_DATATYPE *) calloc(1, correct_buf_size);
    VRFY((NULL != correct_buf), "calloc succeeded");

    for (i = 0; i < data_size / sizeof(*data); i++)
        data[i] = (C_DATATYPE) GEN_DATA(i);

    for (i = 0; i < correct_buf_size / sizeof(*correct_buf); i++)
                                        /* Add Column Index */
        correct_buf[i] = (C_DATATYPE) ( (i % (hsize_t) INTERLEAVED_WRITE_FILTERED_DATASET_NCOLS)

                                        /* Add the Row Index */
                                      + ((i % (hsize_t) (mpi_size * INTERLEAVED_WRITE_FILTERED_DATASET_NCOLS)) / (hsize_t) INTERLEAVED_WRITE_FILTERED_DATASET_NCOLS)

                                        /* Add the amount that gets added when a rank moves down to its next section vertically in the dataset */
                                      + ((hsize_t) INTERLEAVED_WRITE_FILTERED_DATASET_NCOLS * (i / (hsize_t) (mpi_size * INTERLEAVED_WRITE_FILTERED_DATASET_NCOLS))));

    /* Create property list for collective dataset write */
    plist_id = H5Pcreate(H5P_DATASET_XFER);
    VRFY((plist_id >= 0), "DXPL creation succeeded");

    VRFY((H5Pset_dxpl_mpio(plist_id, H5FD_MPIO_COLLECTIVE) >= 0), "Set DXPL MPIO succeeded");

    VRFY((H5Dwrite(dset_id, HDF5_DATATYPE_NAME, memspace, filespace, plist_id, data) >= 0), "Dataset write succeeded");

    if (data) free(data);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");

    /* Verify the correct data was written */
    read_buf = (C_DATATYPE *) calloc(1, correct_buf_size);
    VRFY((NULL != read_buf), "calloc succeeded");

    dset_id = H5Dopen2(file_id, "/" INTERLEAVED_WRITE_FILTERED_DATASET_NAME, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    VRFY((H5Dread(dset_id, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, plist_id, read_buf) >= 0), "Dataset read succeeded");

    VRFY((0 == memcmp(read_buf, correct_buf, correct_buf_size)), "Data verification succeeded");

    if (correct_buf) free(correct_buf);
    if (read_buf) free(read_buf);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Sclose(memspace) >= 0), "Memory dataspace close succeeded");
    VRFY((H5Pclose(plist_id) >= 0), "DXPL close succeeded");
    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    return;
}

/*
 * Tests parallel write of filtered data in the case where
 * the dataset has 3 dimensions and each process writes
 * to its own "page" in the 3rd dimension.
 *
 * Programmer: Jordan Henderson
 *             02/06/2017
 */
static void
test_3d_filtered_dataset_no_overlap_separate_pages(void)
{
    C_DATATYPE *data = NULL;
    C_DATATYPE *read_buf = NULL;
    C_DATATYPE *correct_buf = NULL;
    hsize_t     dataset_dims[UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_DIMS];
    hsize_t     chunk_dims[UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_DIMS];
    hsize_t     sel_dims[UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_DIMS];
    hsize_t     count[UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_DIMS];
    hsize_t     stride[UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_DIMS];
    hsize_t     block[UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_DIMS];
    hsize_t     offset[UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_DIMS];
    size_t      i, data_size, correct_buf_size;
    hid_t       file_id = -1, dset_id = -1, plist_id = -1;
    hid_t       filespace = -1, memspace = -1;

    if (MAINPROCESS) puts("Testing write to unshared filtered chunks on separate pages in 3D dataset");

    /* Set up file access property list with parallel I/O access */
    plist_id = H5Pcreate(H5P_FILE_ACCESS);
    VRFY((plist_id >= 0), "FAPL creation succeeded");

    VRFY((H5Pset_fapl_mpio(plist_id, comm, info) >= 0), "Set FAPL MPIO succeeded");

    VRFY((H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0), "Set libver bounds succeeded");

    file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, plist_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    VRFY((H5Pclose(plist_id) >= 0), "FAPL close succeeded");

    /* Create the dataspace for the dataset */
    dataset_dims[0] = (hsize_t) UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_NROWS;
    dataset_dims[1] = (hsize_t) UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_NCOLS;
    dataset_dims[2] = (hsize_t) UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DEPTH;
    chunk_dims[0] = (hsize_t) UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_CH_NROWS;
    chunk_dims[1] = (hsize_t) UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_CH_NCOLS;
    chunk_dims[2] = 1;
    sel_dims[0] = (hsize_t) UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_NROWS;
    sel_dims[1] = (hsize_t) UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_NCOLS;
    sel_dims[2] = 1;

    filespace = H5Screate_simple(UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_DIMS, dataset_dims, NULL);
    VRFY((filespace >= 0), "File dataspace creation succeeded");

    memspace = H5Screate_simple(UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_DIMS, sel_dims, NULL);
    VRFY((memspace >= 0), "Memory dataspace creation succeeded");

    /* Create chunked dataset */
    plist_id = H5Pcreate(H5P_DATASET_CREATE);
    VRFY((plist_id >= 0), "DCPL creation succeeded");

    VRFY((H5Pset_chunk(plist_id, UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_DIMS, chunk_dims) >= 0), "Chunk size set");

    /* Add test filter to the pipeline */
    VRFY((SET_FILTER(plist_id) >= 0), "Filter set");

    dset_id = H5Dcreate2(file_id, UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_NAME, HDF5_DATATYPE_NAME, filespace,
            H5P_DEFAULT, plist_id, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset creation succeeded");

    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

    /* Each process defines the dataset selection in memory and writes
     * it to the hyperslab in the file
     */
    count[0] = (hsize_t) UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_NROWS / (hsize_t) UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_CH_NROWS;
    count[1] = (hsize_t) UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_NCOLS / (hsize_t) UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_CH_NCOLS;
    count[2] = 1;
    stride[0] = (hsize_t) UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_CH_NROWS;
    stride[1] = (hsize_t) UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_CH_NCOLS;
    stride[2] = 1;
    block[0] = (hsize_t) UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_CH_NROWS;
    block[1] = (hsize_t) UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_CH_NCOLS;
    block[2] = 1;
    offset[0] = 0;
    offset[1] = 0;
    offset[2] = (hsize_t) mpi_rank;

    if (VERBOSE_MED)
        printf("Process %d is writing with count[ %llu, %llu, %llu ], stride[ %llu, %llu, %llu ], offset[ %llu, %llu, %llu ], block size[ %llu, %llu, %llu ]\n",
            mpi_rank, count[0], count[1], count[2], stride[0], stride[1], stride[2], offset[0], offset[1], offset[2], block[0], block[1], block[2]);

    /* Select hyperslab in the file */
    filespace = H5Dget_space(dset_id);
    VRFY((filespace >= 0), "File dataspace retrieval succeeded");

    VRFY((H5Sselect_hyperslab(filespace, H5S_SELECT_SET, offset, stride, count, block) >= 0), "Hyperslab selection succeeded");

    /* Fill data buffer */
    data_size = sel_dims[0] * sel_dims[1] * sel_dims[2] * sizeof(*data);
    correct_buf_size = dataset_dims[0] * dataset_dims[1] * dataset_dims[2] * sizeof(*correct_buf);

    data = (C_DATATYPE *) calloc(1, data_size);
    VRFY((NULL != data), "calloc succeeded");

    correct_buf = (C_DATATYPE *) calloc(1, correct_buf_size);
    VRFY((NULL != correct_buf), "calloc succeeded");

    for (i = 0; i < data_size / sizeof(*data); i++)
        data[i] = (C_DATATYPE) GEN_DATA(i);

    for (i = 0; i < correct_buf_size / sizeof(*correct_buf); i++)
        correct_buf[i] = (C_DATATYPE) ((i % (hsize_t) mpi_size) + (i / (hsize_t) mpi_size));

    /* Create property list for collective dataset write */
    plist_id = H5Pcreate(H5P_DATASET_XFER);
    VRFY((plist_id >= 0), "DXPL creation succeeded");

    VRFY((H5Pset_dxpl_mpio(plist_id, H5FD_MPIO_COLLECTIVE) >= 0), "Set DXPL MPIO succeeded");

    VRFY((H5Dwrite(dset_id, HDF5_DATATYPE_NAME, memspace, filespace, plist_id, data) >= 0), "Dataset write succeeded");

    if (data) free(data);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");

    /* Verify the correct data was written */
    read_buf = (C_DATATYPE *) calloc(1, correct_buf_size);
    VRFY((NULL != read_buf), "calloc succeeded");

    dset_id = H5Dopen2(file_id, "/" UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_NAME, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    VRFY((H5Dread(dset_id, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, plist_id, read_buf) >= 0), "Dataset read succeeded");

    VRFY((0 == memcmp(read_buf, correct_buf, correct_buf_size)), "Data verification succeeded");

    if (correct_buf) free(correct_buf);
    if (read_buf) free(read_buf);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Sclose(memspace) >= 0), "Memory dataspace close succeeded");
    VRFY((H5Pclose(plist_id) >= 0), "DXPL close succeeded");
    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    return;
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
static void
test_3d_filtered_dataset_no_overlap_same_pages(void)
{
    C_DATATYPE *data = NULL;
    C_DATATYPE *read_buf = NULL;
    C_DATATYPE *correct_buf = NULL;
    hsize_t     dataset_dims[UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_DIMS];
    hsize_t     chunk_dims[UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_DIMS];
    hsize_t     sel_dims[UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_DIMS];
    hsize_t     count[UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_DIMS];
    hsize_t     stride[UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_DIMS];
    hsize_t     block[UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_DIMS];
    hsize_t     offset[UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_DIMS];
    size_t      i, data_size, correct_buf_size;
    hid_t       file_id, dset_id, plist_id;
    hid_t       filespace, memspace;

    if (MAINPROCESS) puts("Testing write to unshared filtered chunks on the same pages in 3D dataset");

    /* Set up file access property list with parallel I/O access */
    plist_id = H5Pcreate(H5P_FILE_ACCESS);
    VRFY((plist_id >= 0), "FAPL creation succeeded");

    VRFY((H5Pset_fapl_mpio(plist_id, comm, info) >= 0), "Set FAPL MPIO succeeded");

    VRFY((H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0), "Set libver bounds succeeded");

    file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, plist_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    VRFY((H5Pclose(plist_id) >= 0), "FAPL close succeeded");

    /* Create the dataspace for the dataset */
    dataset_dims[0] = (hsize_t) UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_NROWS;
    dataset_dims[1] = (hsize_t) UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_NCOLS;
    dataset_dims[2] = (hsize_t) UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DEPTH;
    chunk_dims[0] = (hsize_t) UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_CH_NROWS;
    chunk_dims[1] = (hsize_t) UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_CH_NCOLS;
    chunk_dims[2] = 1;
    sel_dims[0] = (hsize_t) UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_CH_NROWS;
    sel_dims[1] = (hsize_t) UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_NCOLS;
    sel_dims[2] = (hsize_t) UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DEPTH;

    filespace = H5Screate_simple(UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_DIMS, dataset_dims, NULL);
    VRFY((filespace >= 0), "File dataspace creation succeeded");

    memspace = H5Screate_simple(UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_DIMS, sel_dims, NULL);
    VRFY((memspace >= 0), "Memory dataspace creation succeeded");

    /* Create chunked dataset */
    plist_id = H5Pcreate(H5P_DATASET_CREATE);
    VRFY((plist_id >= 0), "DCPL creation succeeded");

    VRFY((H5Pset_chunk(plist_id, UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_DIMS, chunk_dims) >= 0), "Chunk size set");

    /* Add test filter to the pipeline */
    VRFY((SET_FILTER(plist_id) >= 0), "Filter set");

    dset_id = H5Dcreate2(file_id, UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_NAME, HDF5_DATATYPE_NAME, filespace,
            H5P_DEFAULT, plist_id, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset creation succeeded");

    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

    /* Each process defines the dataset selection in memory and writes
     * it to the hyperslab in the file
     */
    count[0] = 1;
    count[1] = (hsize_t) UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_NCOLS / (hsize_t) UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_CH_NCOLS;
    count[2] = (hsize_t) mpi_size;
    stride[0] = (hsize_t) UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_CH_NROWS;
    stride[1] = (hsize_t) UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_CH_NCOLS;
    stride[2] = 1;
    block[0] = (hsize_t) UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_CH_NROWS;
    block[1] = (hsize_t) UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_CH_NCOLS;
    block[2] = 1;
    offset[0] = ((hsize_t) mpi_rank * (hsize_t) UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_CH_NROWS * count[0]);
    offset[1] = 0;
    offset[2] = 0;

    if (VERBOSE_MED)
        printf("Process %d is writing with count[ %llu, %llu, %llu ], stride[ %llu, %llu, %llu ], offset[ %llu, %llu, %llu ], block size[ %llu, %llu, %llu ]\n",
            mpi_rank, count[0], count[1], count[2], stride[0], stride[1], stride[2], offset[0], offset[1], offset[2], block[0], block[1], block[2]);

    /* Select hyperslab in the file */
    filespace = H5Dget_space(dset_id);
    VRFY((filespace >= 0), "File dataspace retrieval succeeded");

    VRFY((H5Sselect_hyperslab(filespace, H5S_SELECT_SET, offset, stride, count, block) >= 0), "Hyperslab selection succeeded");

    /* Fill data buffer */
    data_size = sel_dims[0] * sel_dims[1] * sel_dims[2] * sizeof(*data);
    correct_buf_size = dataset_dims[0] * dataset_dims[1] * dataset_dims[2] * sizeof(*correct_buf);

    data = (C_DATATYPE *) calloc(1, data_size);
    VRFY((NULL != data), "calloc succeeded");

    correct_buf = (C_DATATYPE *) calloc(1, correct_buf_size);
    VRFY((NULL != correct_buf), "calloc succeeded");

    for (i = 0; i < data_size / sizeof(*data); i++)
        data[i] = (C_DATATYPE) GEN_DATA(i);

    for (i = 0; i < correct_buf_size / sizeof(*correct_buf); i++)
        correct_buf[i] = (C_DATATYPE) ((i % (dataset_dims[0] * dataset_dims[1])) + (i / (dataset_dims[0] * dataset_dims[1])));

    /* Create property list for collective dataset write */
    plist_id = H5Pcreate(H5P_DATASET_XFER);
    VRFY((plist_id >= 0), "DXPL creation succeeded");

    VRFY((H5Pset_dxpl_mpio(plist_id, H5FD_MPIO_COLLECTIVE) >= 0), "Set DXPL MPIO succeeded");

    VRFY((H5Dwrite(dset_id, HDF5_DATATYPE_NAME, memspace, filespace, plist_id, data) >= 0), "Dataset write succeeded");

    if (data) free(data);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");

    /* Verify the correct data was written */
    read_buf = (C_DATATYPE *) calloc(1, correct_buf_size);
    VRFY((NULL != read_buf), "calloc succeeded");

    dset_id = H5Dopen2(file_id, "/" UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_NAME, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    VRFY((H5Dread(dset_id, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, plist_id, read_buf) >= 0), "Dataset read succeeded");

    VRFY((0 == memcmp(read_buf, correct_buf, correct_buf_size)), "Data verification succeeded");

    if (correct_buf) free(correct_buf);
    if (read_buf) free(read_buf);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Sclose(memspace) >= 0), "Memory dataspace close succeeded");
    VRFY((H5Pclose(plist_id) >= 0), "DXPL close succeeded");
    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    return;
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
static void
test_3d_filtered_dataset_overlap(void)
{
    C_DATATYPE *data = NULL;
    C_DATATYPE *read_buf = NULL;
    C_DATATYPE *correct_buf = NULL;
    hsize_t     dataset_dims[SHARED_FILTERED_CHUNKS_3D_DATASET_DIMS];
    hsize_t     chunk_dims[SHARED_FILTERED_CHUNKS_3D_DATASET_DIMS];
    hsize_t     sel_dims[SHARED_FILTERED_CHUNKS_3D_DATASET_DIMS];
    hsize_t     count[SHARED_FILTERED_CHUNKS_3D_DATASET_DIMS];
    hsize_t     stride[SHARED_FILTERED_CHUNKS_3D_DATASET_DIMS];
    hsize_t     block[SHARED_FILTERED_CHUNKS_3D_DATASET_DIMS];
    hsize_t     offset[SHARED_FILTERED_CHUNKS_3D_DATASET_DIMS];
    size_t      i, data_size, correct_buf_size;
    hid_t       file_id = -1, dset_id = -1, plist_id = -1;
    hid_t       filespace = -1, memspace = -1;

    if (MAINPROCESS) puts("Testing write to shared filtered chunks in 3D dataset");

    /* Set up file access property list with parallel I/O access */
    plist_id = H5Pcreate(H5P_FILE_ACCESS);
    VRFY((plist_id >= 0), "FAPL creation succeeded");

    VRFY((H5Pset_fapl_mpio(plist_id, comm, info) >= 0), "Set FAPL MPIO succeeded");

    VRFY((H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0), "Set libver bounds succeeded");

    file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, plist_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    VRFY((H5Pclose(plist_id) >= 0), "FAPL close succeeded");

    /* Create the dataspace for the dataset */
    dataset_dims[0] = (hsize_t) SHARED_FILTERED_CHUNKS_3D_NROWS;
    dataset_dims[1] = (hsize_t) SHARED_FILTERED_CHUNKS_3D_NCOLS;
    dataset_dims[2] = (hsize_t) SHARED_FILTERED_CHUNKS_3D_DEPTH;
    chunk_dims[0] = (hsize_t) SHARED_FILTERED_CHUNKS_3D_CH_NROWS;
    chunk_dims[1] = (hsize_t) SHARED_FILTERED_CHUNKS_3D_CH_NCOLS;
    chunk_dims[2] = 1;
    sel_dims[0] = (hsize_t) (SHARED_FILTERED_CHUNKS_3D_NROWS / mpi_size);
    sel_dims[1] = (hsize_t) SHARED_FILTERED_CHUNKS_3D_NCOLS;
    sel_dims[2] = (hsize_t) SHARED_FILTERED_CHUNKS_3D_DEPTH;

    filespace = H5Screate_simple(SHARED_FILTERED_CHUNKS_3D_DATASET_DIMS, dataset_dims, NULL);
    VRFY((filespace >= 0), "File dataspace creation succeeded");

    memspace = H5Screate_simple(SHARED_FILTERED_CHUNKS_3D_DATASET_DIMS, sel_dims, NULL);
    VRFY((memspace >= 0), "Memory dataspace creation succeeded");

    /* Create chunked dataset */
    plist_id = H5Pcreate(H5P_DATASET_CREATE);
    VRFY((plist_id >= 0), "DCPL creation succeeded");

    VRFY((H5Pset_chunk(plist_id, SHARED_FILTERED_CHUNKS_3D_DATASET_DIMS, chunk_dims) >= 0), "Chunk size set");

    /* Add test filter to the pipeline */
    VRFY((SET_FILTER(plist_id) >= 0), "Filter set");

    dset_id = H5Dcreate2(file_id, SHARED_FILTERED_CHUNKS_3D_DATASET_NAME, HDF5_DATATYPE_NAME, filespace,
            H5P_DEFAULT, plist_id, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset creation succeeded");

    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

    /* Each process defines the dataset selection in memory and writes
     * it to the hyperslab in the file
     */
    count[0] = (hsize_t) (SHARED_FILTERED_CHUNKS_3D_NROWS / SHARED_FILTERED_CHUNKS_3D_CH_NROWS);
    count[1] = (hsize_t) (SHARED_FILTERED_CHUNKS_3D_NCOLS / SHARED_FILTERED_CHUNKS_3D_CH_NCOLS);
    count[2] = (hsize_t) SHARED_FILTERED_CHUNKS_3D_DEPTH;
    stride[0] = (hsize_t) SHARED_FILTERED_CHUNKS_3D_CH_NROWS;
    stride[1] = (hsize_t) SHARED_FILTERED_CHUNKS_3D_CH_NCOLS;
    stride[2] = 1;
    block[0] = 1;
    block[1] = (hsize_t) SHARED_FILTERED_CHUNKS_3D_CH_NCOLS;
    block[2] = 1;
    offset[0] = (hsize_t) mpi_rank;
    offset[1] = 0;
    offset[2] = 0;

    if (VERBOSE_MED)
        printf("Process %d is writing with count[ %llu, %llu, %llu ], stride[ %llu, %llu, %llu ], offset[ %llu, %llu, %llu ], block size[ %llu, %llu, %llu ]\n",
            mpi_rank, count[0], count[1], count[2], stride[0], stride[1], stride[2], offset[0], offset[1], offset[2], block[0], block[1], block[2]);

    /* Select hyperslab in the file */
    filespace = H5Dget_space(dset_id);
    VRFY((filespace >= 0), "File dataspace retrieval succeeded");

    VRFY((H5Sselect_hyperslab(filespace, H5S_SELECT_SET, offset, stride, count, block) >= 0), "Hyperslab selection succeeded");

    /* Fill data buffer */
    data_size = sel_dims[0] * sel_dims[1] * sel_dims[2] * sizeof(*data);
    correct_buf_size = dataset_dims[0] * dataset_dims[1] * dataset_dims[2] * sizeof(*correct_buf);

    data = (C_DATATYPE *) calloc(1, data_size);
    VRFY((NULL != data), "calloc succeeded");

    correct_buf = (C_DATATYPE *) calloc(1, correct_buf_size);
    VRFY((NULL != correct_buf), "calloc succeeded");

    for (i = 0; i < data_size / sizeof(*data); i++)
        data[i] = (C_DATATYPE) GEN_DATA(i);

    for (i = 0; i < correct_buf_size / sizeof(*correct_buf); i++)
                                       /* Add the Column Index */
        correct_buf[i] = (C_DATATYPE) ( (i % (hsize_t) (SHARED_FILTERED_CHUNKS_3D_DEPTH * SHARED_FILTERED_CHUNKS_3D_NCOLS))

                                       /* Add the Row Index */
                                      + ((i % (hsize_t) (mpi_size * SHARED_FILTERED_CHUNKS_3D_DEPTH * SHARED_FILTERED_CHUNKS_3D_NCOLS)) / (hsize_t) (SHARED_FILTERED_CHUNKS_3D_DEPTH * SHARED_FILTERED_CHUNKS_3D_NCOLS))

                                       /* Add the amount that gets added when a rank moves down to its next section vertically in the dataset */
                                      + ((hsize_t) (SHARED_FILTERED_CHUNKS_3D_DEPTH * SHARED_FILTERED_CHUNKS_3D_NCOLS) * (i / (hsize_t) (mpi_size * SHARED_FILTERED_CHUNKS_3D_DEPTH * SHARED_FILTERED_CHUNKS_3D_NCOLS))));

    /* Create property list for collective dataset write */
    plist_id = H5Pcreate(H5P_DATASET_XFER);
    VRFY((plist_id >= 0), "DXPL creation succeeded");

    VRFY((H5Pset_dxpl_mpio(plist_id, H5FD_MPIO_COLLECTIVE) >= 0), "Set DXPL MPIO succeeded");

    VRFY((H5Dwrite(dset_id, HDF5_DATATYPE_NAME, memspace, filespace, plist_id, data) >= 0), "Dataset write succeeded");

    if (data) free(data);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");

    /* Verify the correct data was written */
    read_buf = (C_DATATYPE *) calloc(1, correct_buf_size);
    VRFY((NULL != read_buf), "calloc succeeded");

    dset_id = H5Dopen2(file_id, "/" SHARED_FILTERED_CHUNKS_3D_DATASET_NAME, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    VRFY((H5Dread(dset_id, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, plist_id, read_buf) >= 0), "Dataset read succeeded");

    VRFY((0 == memcmp(read_buf, correct_buf, correct_buf_size)), "Data verification succeeded");

    if (correct_buf) free(correct_buf);
    if (read_buf) free(read_buf);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Sclose(memspace) >= 0), "Memory dataspace close succeeded");
    VRFY((H5Pclose(plist_id) >= 0), "DXPL close succeeded");
    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    return;
}

/*
 * Tests parallel write of filtered data to unshared
 * chunks using a compound datatype which doesn't
 * require a datatype conversion.
 *
 * Programmer: Jordan Henderson
 *             02/10/2017
 */
/* JTH: This test currently cannot be data-verified due to the floating-point data involved */
static void
test_cmpd_filtered_dataset_no_conversion_unshared(void)
{
    cmpd_filtered_t *data = NULL;
    hsize_t          dataset_dims[COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t          chunk_dims[COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t          sel_dims[COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t          count[COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t          stride[COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t          block[COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t          offset[COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_DIMS];
    size_t           i;
    hid_t            file_id = -1, dset_id = -1, plist_id = -1, memtype = -1;
    hid_t            filespace = -1, memspace = -1;

    if (MAINPROCESS) puts("Testing write to unshared filtered chunks in Compound Datatype dataset without Datatype conversion");

    /* Set up file access property list with parallel I/O access */
    plist_id = H5Pcreate(H5P_FILE_ACCESS);
    VRFY((plist_id >= 0), "FAPL creation succeeded");

    VRFY((H5Pset_fapl_mpio(plist_id, comm, info) >= 0), "Set FAPL MPIO succeeded");

    VRFY((H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0), "Set libver bounds succeeded");

    file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, plist_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    VRFY((H5Pclose(plist_id) >= 0), "FAPL close succeeded");

    /* Create the dataspace for the dataset */
    dataset_dims[0] = COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_NROWS;
    dataset_dims[1] = (hsize_t) COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_NCOLS;
    chunk_dims[0] = COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_CH_NROWS;
    chunk_dims[1] = COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_CH_NCOLS;
    sel_dims[0] = COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_CH_NROWS;
    sel_dims[1] = (hsize_t) COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_ENTRIES_PER_PROC;

    filespace = H5Screate_simple(COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_DIMS, dataset_dims, NULL);
    VRFY((filespace >= 0), "File dataspace creation succeeded");

    memspace = H5Screate_simple(COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_DIMS, sel_dims, NULL);
    VRFY((memspace >= 0), "Memory dataspace creation succeeded");

    /* Create chunked dataset */
    plist_id = H5Pcreate(H5P_DATASET_CREATE);
    VRFY((plist_id >= 0), "DCPL creation succeeded");

    VRFY((H5Pset_chunk(plist_id, COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_DIMS, chunk_dims) >= 0), "Chunk size set");

    /* Add test filter to the pipeline */
    VRFY((SET_FILTER(plist_id) >= 0), "Filter set");

    /* Create the compound type for memory. */
    memtype = H5Tcreate(H5T_COMPOUND, sizeof(cmpd_filtered_t));
    VRFY((memtype >= 0), "Datatype creation succeeded");

    VRFY((H5Tinsert(memtype, "ShortData", HOFFSET(cmpd_filtered_t, field1), H5T_NATIVE_SHORT) >= 0), "Datatype insertion succeeded");
    VRFY((H5Tinsert(memtype, "IntData", HOFFSET(cmpd_filtered_t, field2), H5T_NATIVE_INT) >= 0), "Datatype insertion succeeded");
    VRFY((H5Tinsert(memtype, "LongData", HOFFSET(cmpd_filtered_t, field3), H5T_NATIVE_LONG) >= 0), "Datatype insertion succeeded");
    VRFY((H5Tinsert(memtype, "DoubleData", HOFFSET(cmpd_filtered_t, field4), H5T_NATIVE_DOUBLE) >= 0), "Datatype insertion succeeded");

    dset_id = H5Dcreate2(file_id, COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_NAME, memtype, filespace,
            H5P_DEFAULT, plist_id, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset creation succeeded");

    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

    /* Each process defines the dataset selection in memory and writes
     * it to the hyperslab in the file
     */
    count[0] = 1;
    count[1] = (hsize_t) COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_ENTRIES_PER_PROC;
    stride[0] = COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_CH_NROWS;
    stride[1] = COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_CH_NCOLS;
    block[0] = COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_CH_NROWS;
    block[1] = COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_CH_NCOLS;
    offset[0] = 0;
    offset[1] = ((hsize_t) mpi_rank * COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_CH_NCOLS);

    if (VERBOSE_MED)
        printf("Process %d is writing with count[ %llu, %llu ], stride[ %llu, %llu ], offset[ %llu, %llu ], block size[ %llu, %llu ]\n",
                mpi_rank, count[0], count[1], stride[0], stride[1], offset[0], offset[1], block[0], block[1]);

    /* Select hyperslab in the file */
    filespace = H5Dget_space(dset_id);
    VRFY((filespace >= 0), "File dataspace retrieval succeeded");

    VRFY((H5Sselect_hyperslab(filespace, H5S_SELECT_SET, offset, stride, count, block) >= 0), "Hyperslab selection succeeded");

    data = (COMPOUND_C_DATATYPE *) calloc(1, (hsize_t) COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_ENTRIES_PER_PROC * sizeof(*data));
    VRFY((NULL != data), "calloc succeeded");

    /* Fill data buffer */
    memset(data, 0, sizeof(cmpd_filtered_t) * (size_t) COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_ENTRIES_PER_PROC);
    for (i = 0; i < (hsize_t) COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_ENTRIES_PER_PROC; i++) {
        data[i].field1 = (short) GEN_DATA(i);
        data[i].field2 = (int) GEN_DATA(i);
        data[i].field3 = (long) GEN_DATA(i);
        data[i].field4 = (double) GEN_DATA(i);
    }

    /* Create property list for collective dataset write */
    plist_id = H5Pcreate(H5P_DATASET_XFER);
    VRFY((plist_id >= 0), "DXPL creation succeeded");

    VRFY((H5Pset_dxpl_mpio(plist_id, H5FD_MPIO_COLLECTIVE) >= 0), "Set DXPL MPIO succeeded");

    VRFY((H5Dwrite(dset_id, memtype, memspace, filespace, plist_id, data) >= 0), "Dataset write succeeded");

    if (data) free(data);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Sclose(memspace) >= 0), "Memory dataspace close succeeded");
    VRFY((H5Tclose(memtype) >= 0), "Datatype close succeeded");
    VRFY((H5Pclose(plist_id) >= 0), "DXPL close succeeded");
    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    return;
}

/*
 * Tests parallel write of filtered data to shared
 * chunks using a compound datatype which doesn't
 * require a datatype conversion.
 *
 * Programmer: Jordan Henderson
 *             02/10/2017
 */
/* JTH: This test currently cannot be data-verified due to the floating-point data involved */
static void
test_cmpd_filtered_dataset_no_conversion_shared(void)
{
    cmpd_filtered_t *data = NULL;
    hsize_t          dataset_dims[COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t          chunk_dims[COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t          sel_dims[COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t          count[COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t          stride[COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t          block[COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t          offset[COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_DIMS];
    size_t           i;
    hid_t            file_id, dset_id, plist_id, memtype;
    hid_t            filespace, memspace;

    if (MAINPROCESS) puts("Testing write to shared filtered chunks in Compound Datatype dataset without Datatype conversion");

    /* Set up file access property list with parallel I/O access */
    plist_id = H5Pcreate(H5P_FILE_ACCESS);
    VRFY((plist_id>= 0), "FAPL creation succeeded");

    VRFY((H5Pset_fapl_mpio(plist_id, comm, info) >= 0), "Set FAPL MPIO succeeded");

    VRFY((H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0), "Set libver bounds succeeded");

    file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, plist_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    VRFY((H5Pclose(plist_id) >= 0), "FAPL close succeeded");

    /* Create the dataspace for the dataset */
    dataset_dims[0] = (hsize_t) COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_NROWS;
    dataset_dims[1] = (hsize_t) COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_NCOLS;
    chunk_dims[0] = (hsize_t) COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_CH_NROWS;
    chunk_dims[1] = COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_CH_NCOLS;
    sel_dims[0] = (hsize_t) COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_CH_NROWS / (hsize_t) mpi_size;
    sel_dims[1] = (hsize_t) COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_ENTRIES_PER_PROC;

    filespace = H5Screate_simple(COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_DIMS, dataset_dims, NULL);
    VRFY((filespace >= 0), "File dataspace creation succeeded");

    memspace = H5Screate_simple(COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_DIMS, sel_dims, NULL);
    VRFY((memspace >= 0), "Memory dataspace creation succeeded");

    /* Create chunked dataset */
    plist_id = H5Pcreate(H5P_DATASET_CREATE);
    VRFY((plist_id >= 0), "DCPL creation succeeded");

    VRFY((H5Pset_chunk(plist_id, COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_DIMS, chunk_dims) >= 0), "Chunk size set");

    /* Add test filter to the pipeline */
    VRFY((SET_FILTER(plist_id) >= 0), "Filter set");

    /* Create the compound type for memory. */
    memtype = H5Tcreate(H5T_COMPOUND, sizeof(cmpd_filtered_t));
    VRFY((memtype >= 0), "Datatype creation succeeded");

    VRFY((H5Tinsert(memtype, "ShortData", HOFFSET(cmpd_filtered_t, field1), H5T_NATIVE_SHORT) >= 0), "Datatype insertion succeeded");
    VRFY((H5Tinsert(memtype, "IntData", HOFFSET(cmpd_filtered_t, field2), H5T_NATIVE_INT) >= 0), "Datatype insertion succeeded");
    VRFY((H5Tinsert(memtype, "LongData", HOFFSET(cmpd_filtered_t, field3), H5T_NATIVE_LONG) >= 0), "Datatype insertion succeeded");
    VRFY((H5Tinsert(memtype, "DoubleData", HOFFSET(cmpd_filtered_t, field4), H5T_NATIVE_DOUBLE) >= 0), "Datatype insertion succeeded");

    dset_id = H5Dcreate2(file_id, COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_NAME, memtype, filespace,
            H5P_DEFAULT, plist_id, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset creation succeeded");

    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

    /* Each process defines the dataset selection in memory and writes
     * it to the hyperslab in the file
     */
    count[0] = 1;
    count[1] = (hsize_t) COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_ENTRIES_PER_PROC;
    stride[0] = (hsize_t) COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_CH_NROWS;
    stride[1] = COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_CH_NCOLS;
    block[0] = (hsize_t) COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_CH_NROWS / (hsize_t) mpi_size;
    block[1] = COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_CH_NCOLS;
    offset[0] = (hsize_t) mpi_rank;
    offset[1] = 0;

    if (VERBOSE_MED)
        printf("Process %d is writing with count[ %llu, %llu ], stride[ %llu, %llu ], offset[ %llu, %llu ], block size[ %llu, %llu ]\n",
            mpi_rank, count[0], count[1], stride[0], stride[1], offset[0], offset[1], block[0], block[1]);

    /* Select hyperslab in the file */
    filespace = H5Dget_space(dset_id);
    VRFY((filespace >= 0), "File dataspace retrieval succeeded");

    VRFY((H5Sselect_hyperslab(filespace, H5S_SELECT_SET, offset, stride, count, block) >= 0), "Hyperslab selection succeeded");

    data = (COMPOUND_C_DATATYPE *) calloc(1, (hsize_t) COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_ENTRIES_PER_PROC * sizeof(*data));
    VRFY((NULL != data), "calloc succeeded");

    /* Fill data buffer */
    memset(data, 0, sizeof(cmpd_filtered_t) * (size_t) COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_ENTRIES_PER_PROC);
    for (i = 0; i < (hsize_t) COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_ENTRIES_PER_PROC; i++) {
        data[i].field1 = (short) GEN_DATA(i);
        data[i].field2 = (int) GEN_DATA(i);
        data[i].field3 = (long) GEN_DATA(i);
        data[i].field4 = (double) GEN_DATA(i);
    }

    /* Create property list for collective dataset write */
    plist_id = H5Pcreate(H5P_DATASET_XFER);
    VRFY((plist_id >= 0), "DXPL creation succeeded");

    VRFY((H5Pset_dxpl_mpio(plist_id, H5FD_MPIO_COLLECTIVE) >= 0), "Set DXPL MPIO succeeded");

    VRFY((H5Dwrite(dset_id, memtype, memspace, filespace, plist_id, data) >= 0), "Dataset write succeeded");

    if (data) free(data);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Sclose(memspace) >= 0), "Memory dataspace close succeeded");
    VRFY((H5Tclose(memtype) >= 0), "Datatype close succeeded");
    VRFY((H5Pclose(plist_id) >= 0), "DXPL close succeeded");
    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    return;
}

/*
 * Tests parallel write of filtered data to unshared
 * chunks using a compound datatype which requires a
 * datatype conversion.
 *
 * This test currently should fail because the datatype
 * conversion causes the parallel library to break
 * to independent I/O and this isn't allowed when
 * there are filters in the pipeline.
 *
 * Programmer: Jordan Henderson
 *             02/07/2017
 */
/* JTH: This test currently cannot be data-verified due to the floating-point data involved */
static void
test_cmpd_filtered_dataset_type_conversion_unshared(void)
{
    cmpd_filtered_t *data = NULL;
    hsize_t          dataset_dims[COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t          chunk_dims[COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t          sel_dims[COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t          count[COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t          stride[COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t          block[COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t          offset[COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_DIMS];
    size_t           i;
    hid_t            file_id = -1, dset_id = -1, plist_id = -1, filetype = -1, memtype = -1;
    hid_t            filespace = -1, memspace = -1;

    if (MAINPROCESS) puts("Testing write to unshared filtered chunks in Compound Datatype dataset with Datatype conversion");

    /* Set up file access property list with parallel I/O access */
    plist_id = H5Pcreate(H5P_FILE_ACCESS);
    VRFY((plist_id >= 0), "FAPL creation succeeded");

    VRFY((H5Pset_fapl_mpio(plist_id, comm, info) >= 0), "Set FAPL MPIO succeeded");

    VRFY((H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0), "Set libver bounds succeeded");

    file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, plist_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    VRFY((H5Pclose(plist_id) >= 0), "FAPL close succeeded");

    /* Create the dataspace for the dataset */
    dataset_dims[0] = COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_NROWS;
    dataset_dims[1] = (hsize_t) COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_NCOLS;
    chunk_dims[0] = COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_CH_NROWS;
    chunk_dims[1] = COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_CH_NCOLS;
    sel_dims[0] = COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_CH_NROWS;
    sel_dims[1] = (hsize_t) COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_ENTRIES_PER_PROC;

    filespace = H5Screate_simple(COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_DIMS, dataset_dims, NULL);
    VRFY((filespace >= 0), "File dataspace creation succeeded");

    memspace = H5Screate_simple(COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_DIMS, sel_dims, NULL);
    VRFY((memspace >= 0), "Memory dataspace creation succeeded");

    /* Create chunked dataset */
    plist_id = H5Pcreate(H5P_DATASET_CREATE);
    VRFY((plist_id >= 0), "DCPL creation succeeded");

    VRFY((H5Pset_chunk(plist_id, COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_DIMS, chunk_dims) >= 0), "Chunk size set");

    /* Add test filter to the pipeline */
    VRFY((SET_FILTER(plist_id) >= 0), "Filter set");

    /* Create the compound type for memory. */
    memtype = H5Tcreate(H5T_COMPOUND, sizeof(cmpd_filtered_t));
    VRFY((memtype >= 0), "Datatype creation succeeded");

    VRFY((H5Tinsert(memtype, "ShortData", HOFFSET(cmpd_filtered_t, field1), H5T_NATIVE_SHORT) >= 0), "Datatype insertion succeeded");
    VRFY((H5Tinsert(memtype, "IntData", HOFFSET(cmpd_filtered_t, field2), H5T_NATIVE_INT) >= 0), "Datatype insertion succeeded");
    VRFY((H5Tinsert(memtype, "LongData", HOFFSET(cmpd_filtered_t, field3), H5T_NATIVE_LONG) >= 0), "Datatype insertion succeeded");
    VRFY((H5Tinsert(memtype, "DoubleData", HOFFSET(cmpd_filtered_t, field4), H5T_NATIVE_DOUBLE) >= 0), "Datatype insertion succeeded");

    /* Create the compound type for file. */
    filetype = H5Tcreate(H5T_COMPOUND, 32);
    VRFY((filetype >= 0), "Datatype creation succeeded");

    VRFY((H5Tinsert(filetype, "ShortData", 0, H5T_STD_I64BE) >= 0), "Datatype insertion succeeded");
    VRFY((H5Tinsert(filetype, "IntData", 8, H5T_STD_I64BE) >= 0), "Datatype insertion succeeded");
    VRFY((H5Tinsert(filetype, "LongData", 16, H5T_STD_I64BE) >= 0), "Datatype insertion succeeded");
    VRFY((H5Tinsert(filetype, "DoubleData", 24, H5T_IEEE_F64BE) >= 0), "Datatype insertion succeeded");

    dset_id = H5Dcreate2(file_id, COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_NAME, filetype, filespace,
            H5P_DEFAULT, plist_id, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset creation succeeded");

    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

    /* Each process defines the dataset selection in memory and writes
     * it to the hyperslab in the file
     */
    count[0] = 1;
    count[1] = (hsize_t) COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_ENTRIES_PER_PROC;
    stride[0] = COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_CH_NROWS;
    stride[1] = COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_CH_NCOLS;
    block[0] = COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_CH_NROWS;
    block[1] = COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_CH_NCOLS;
    offset[0] = 0;
    offset[1] = ((hsize_t) mpi_rank * COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_CH_NCOLS);

    if (VERBOSE_MED)
        printf("Process %d is writing with count[ %llu, %llu ], stride[ %llu, %llu ], offset[ %llu, %llu ], block size[ %llu, %llu ]\n",
            mpi_rank, count[0], count[1], stride[0], stride[1], offset[0], offset[1], block[0], block[1]);

    /* Select hyperslab in the file */
    filespace = H5Dget_space(dset_id);
    VRFY((filespace >= 0), "File dataspace retrieval succeeded");

    VRFY((H5Sselect_hyperslab(filespace, H5S_SELECT_SET, offset, stride, count, block) >= 0), "Hyperslab selection succeeded");

    data = (COMPOUND_C_DATATYPE *) calloc(1, (hsize_t) COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_ENTRIES_PER_PROC * sizeof(*data));
    VRFY((NULL != data), "calloc succeeded");

    /* Fill data buffer */
    memset(data, 0, sizeof(cmpd_filtered_t) * (hsize_t) COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_ENTRIES_PER_PROC);
    for (i = 0; i < (hsize_t) COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_ENTRIES_PER_PROC; i++) {
        data[i].field1 = (short) GEN_DATA(i);
        data[i].field2 = (int) GEN_DATA(i);
        data[i].field3 = (long) GEN_DATA(i);
        data[i].field4 = (double) GEN_DATA(i);
    }

    /* Create property list for collective dataset write */
    plist_id = H5Pcreate(H5P_DATASET_XFER);
    VRFY((plist_id >= 0), "DXPL creation succeeded");

    VRFY((H5Pset_dxpl_mpio(plist_id, H5FD_MPIO_COLLECTIVE) >= 0), "Set DXPL MPIO succeeded");

    /* Ensure that this test currently fails since type conversions break collective mode */
    H5E_BEGIN_TRY {
        VRFY((H5Dwrite(dset_id, memtype, memspace, filespace, plist_id, data) < 0), "Dataset write succeeded");
    } H5E_END_TRY;

    if (data) free(data);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Sclose(memspace) >= 0), "Memory dataspace close succeeded");
    VRFY((H5Tclose(filetype) >= 0), "File datatype close succeeded");
    VRFY((H5Tclose(memtype) >= 0), "Memory datatype close succeeded");
    VRFY((H5Pclose(plist_id) >= 0), "DXPL close succeeded");
    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    return;
}

/*
 * Tests parallel write of filtered data to shared
 * chunks using a compound datatype which requires
 * a datatype conversion.
 *
 * This test currently should fail because the datatype
 * conversion causes the parallel library to break
 * to independent I/O and this isn't allowed when
 * there are filters in the pipeline.
 *
 * Programmer: Jordan Henderson
 *             02/10/2017
 */
/* JTH: This test currently cannot be data-verified due to the floating-point data involved */
static void
test_cmpd_filtered_dataset_type_conversion_shared(void)
{
    cmpd_filtered_t *data = NULL;
    hsize_t          dataset_dims[COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t          chunk_dims[COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t          sel_dims[COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t          count[COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t          stride[COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t          block[COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t          offset[COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_DIMS];
    size_t           i;
    hid_t            file_id, dset_id, plist_id, filetype, memtype;
    hid_t            filespace, memspace;

    if (MAINPROCESS) puts("Testing write to shared filtered chunks in Compound Datatype dataset with Datatype conversion");

    /* Set up file access property list with parallel I/O access */
    plist_id = H5Pcreate(H5P_FILE_ACCESS);
    VRFY((plist_id >= 0), "FAPL creation succeeded");

    VRFY((H5Pset_fapl_mpio(plist_id, comm, info) >= 0), "Set FAPL MPIO succeeded");

    VRFY((H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0), "Set libver bounds succeeded");

    file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, plist_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    VRFY((H5Pclose(plist_id) >= 0), "FAPL close succeeded");

    /* Create the dataspace for the dataset */
    dataset_dims[0] = (hsize_t) COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_NROWS;
    dataset_dims[1] = (hsize_t) COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_NCOLS;
    chunk_dims[0] = (hsize_t) COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_CH_NROWS;
    chunk_dims[1] = COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_CH_NCOLS;
    sel_dims[0] = (hsize_t) COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_CH_NROWS / (hsize_t) mpi_size;
    sel_dims[1] = (hsize_t) COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_ENTRIES_PER_PROC;

    filespace = H5Screate_simple(COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_DIMS, dataset_dims, NULL);
    VRFY((filespace >= 0), "File dataspace creation succeeded");

    memspace = H5Screate_simple(COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_DIMS, sel_dims, NULL);
    VRFY((memspace >= 0), "Memory dataspace creation succeeded");

    /* Create chunked dataset */
    plist_id = H5Pcreate(H5P_DATASET_CREATE);
    VRFY((plist_id >= 0), "DCPL creation succeeded");

    VRFY((H5Pset_chunk(plist_id, COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_DIMS, chunk_dims) >= 0), "Chunk size set");

    /* Add test filter to the pipeline */
    VRFY((SET_FILTER(plist_id) >= 0), "Filter set");

    /* Create the compound type for memory. */
    memtype = H5Tcreate(H5T_COMPOUND, sizeof(cmpd_filtered_t));
    VRFY((memtype >= 0), "Datatype creation succeeded");

    VRFY((H5Tinsert(memtype, "ShortData", HOFFSET(cmpd_filtered_t, field1), H5T_NATIVE_SHORT) >= 0), "Datatype insertion succeeded");
    VRFY((H5Tinsert(memtype, "IntData", HOFFSET(cmpd_filtered_t, field2), H5T_NATIVE_INT) >= 0), "Datatype insertion succeeded");
    VRFY((H5Tinsert(memtype, "LongData", HOFFSET(cmpd_filtered_t, field3), H5T_NATIVE_LONG) >= 0), "Datatype insertion succeeded");
    VRFY((H5Tinsert(memtype, "DoubleData", HOFFSET(cmpd_filtered_t, field4), H5T_NATIVE_DOUBLE) >= 0), "Datatype insertion succeeded");

    /* Create the compound type for file. */
    filetype = H5Tcreate(H5T_COMPOUND, 32);
    VRFY((filetype >= 0), "Datatype creation succeeded");

    VRFY((H5Tinsert(filetype, "ShortData", 0, H5T_STD_I64BE) >= 0), "Datatype insertion succeeded");
    VRFY((H5Tinsert(filetype, "IntData", 8, H5T_STD_I64BE) >= 0), "Datatype insertion succeeded");
    VRFY((H5Tinsert(filetype, "LongData", 16, H5T_STD_I64BE) >= 0), "Datatype insertion succeeded");
    VRFY((H5Tinsert(filetype, "DoubleData", 24, H5T_IEEE_F64BE) >= 0), "Datatype insertion succeeded");

    dset_id = H5Dcreate2(file_id, COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_NAME, filetype, filespace,
            H5P_DEFAULT, plist_id, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset creation succeeded");

    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

    /* Each process defines the dataset selection in memory and writes
     * it to the hyperslab in the file
     */
    count[0] = 1;
    count[1] = (hsize_t) COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_ENTRIES_PER_PROC;
    stride[0] = (hsize_t) COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_CH_NROWS;
    stride[1] = COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_CH_NCOLS;
    block[0] = (hsize_t) COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_CH_NROWS / (hsize_t) mpi_size;
    block[1] = COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_CH_NCOLS;
    offset[0] = (hsize_t) mpi_rank;
    offset[1] = 0;

    if (VERBOSE_MED)
        printf("Process %d is writing with count[ %llu, %llu ], stride[ %llu, %llu ], offset[ %llu, %llu ], block size[ %llu, %llu ]\n",
            mpi_rank, count[0], count[1], stride[0], stride[1], offset[0], offset[1], block[0], block[1]);

    /* Select hyperslab in the file */
    filespace = H5Dget_space(dset_id);
    VRFY((filespace >= 0), "File dataspace retrieval succeeded");

    VRFY((H5Sselect_hyperslab(filespace, H5S_SELECT_SET, offset, stride, count, block) >= 0), "Hyperslab selection succeeded");

    data = (COMPOUND_C_DATATYPE *) calloc(1, (hsize_t) COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_ENTRIES_PER_PROC * sizeof(*data));
    VRFY((NULL != data), "calloc succeeded");

    /* Fill data buffer */
    memset(data, 0, sizeof(cmpd_filtered_t) * (size_t) COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_ENTRIES_PER_PROC);
    for (i = 0; i < (hsize_t) COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_ENTRIES_PER_PROC; i++) {
        data[i].field1 = (short) GEN_DATA(i);
        data[i].field2 = (int) GEN_DATA(i);
        data[i].field3 = (long) GEN_DATA(i);
        data[i].field4 = (double) GEN_DATA(i);
    }

    /* Create property list for collective dataset write */
    plist_id = H5Pcreate(H5P_DATASET_XFER);
    VRFY((plist_id >= 0), "DXPL creation succeeded");

    VRFY((H5Pset_dxpl_mpio(plist_id, H5FD_MPIO_COLLECTIVE) >= 0), "Set DXPL MPIO succeeded");

    /* Ensure that this test currently fails since type conversions break collective mode */
    H5E_BEGIN_TRY {
        VRFY((H5Dwrite(dset_id, memtype, memspace, filespace, plist_id, data) < 0), "Dataset write succeeded");
    } H5E_END_TRY;

    if (data) free(data);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Sclose(memspace) >= 0), "Memory dataspace close succeeded");
    VRFY((H5Tclose(filetype) >= 0), "File datatype close succeeded");
    VRFY((H5Tclose(memtype) >= 0), "Memory datatype close succeeded");
    VRFY((H5Pclose(plist_id) >= 0), "DXPL close succeeded");
    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    return;
}

/*
 * Tests write of filtered data to a dataset
 * by a single process. After the write has
 * succeeded, the dataset is closed and then
 * re-opened in parallel and read by all
 * processes to ensure data correctness.
 *
 * Programmer: Jordan Henderson
 *             08/03/2017
 */
static void
test_write_serial_read_parallel(void)
{
    C_DATATYPE *data = NULL;
    C_DATATYPE *read_buf = NULL;
    C_DATATYPE *correct_buf = NULL;
    hsize_t     dataset_dims[WRITE_SERIAL_READ_PARALLEL_DATASET_DIMS];
    hsize_t     chunk_dims[WRITE_SERIAL_READ_PARALLEL_DATASET_DIMS];
    size_t      i, data_size, correct_buf_size;
    hid_t       file_id = -1, dset_id = -1, plist_id = -1;
    hid_t       filespace = -1;

    if (MAINPROCESS) puts("Testing write file serially; read file in parallel");

    dataset_dims[0] = (hsize_t) WRITE_SERIAL_READ_PARALLEL_NROWS;
    dataset_dims[1] = (hsize_t) WRITE_SERIAL_READ_PARALLEL_NCOLS;
    dataset_dims[2] = (hsize_t) WRITE_SERIAL_READ_PARALLEL_DEPTH;

    /* Write the file on the MAINPROCESS rank */
    if (MAINPROCESS) {
        /* Set up file access property list */
        plist_id = H5Pcreate(H5P_FILE_ACCESS);
        VRFY((plist_id >= 0), "FAPL creation succeeded");

        VRFY((H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0), "Set libver bounds succeeded");

        file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, plist_id);
        VRFY((file_id >= 0), "Test file open succeeded");

        VRFY((H5Pclose(plist_id) >= 0), "FAPL close succeeded");

        /* Create the dataspace for the dataset */
        chunk_dims[0] = (hsize_t) WRITE_SERIAL_READ_PARALLEL_CH_NROWS;
        chunk_dims[1] = (hsize_t) WRITE_SERIAL_READ_PARALLEL_CH_NCOLS;
        chunk_dims[2] = 1;

        filespace = H5Screate_simple(WRITE_SERIAL_READ_PARALLEL_DATASET_DIMS, dataset_dims, NULL);
        VRFY((filespace >= 0), "File dataspace creation succeeded");

        /* Create chunked dataset */
        plist_id = H5Pcreate(H5P_DATASET_CREATE);
        VRFY((plist_id >= 0), "DCPL creation succeeded");

        VRFY((H5Pset_chunk(plist_id, WRITE_SERIAL_READ_PARALLEL_DATASET_DIMS, chunk_dims) >= 0), "Chunk size set");

        /* Add test filter to the pipeline */
        VRFY((SET_FILTER(plist_id) >= 0), "Filter set");

        dset_id = H5Dcreate2(file_id, WRITE_SERIAL_READ_PARALLEL_DATASET_NAME, HDF5_DATATYPE_NAME, filespace,
                H5P_DEFAULT, plist_id, H5P_DEFAULT);
        VRFY((dset_id >= 0), "Dataset creation succeeded");

        VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
        VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

        data_size = dataset_dims[0] * dataset_dims[1] * dataset_dims[2] * sizeof(*data);

        data = (C_DATATYPE *) calloc(1, data_size);
        VRFY((NULL != data), "calloc succeeded");

        for (i = 0; i < data_size / sizeof(*data); i++)
            data[i] = (C_DATATYPE) GEN_DATA(i);

        VRFY((H5Dwrite(dset_id, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, H5P_DEFAULT, data) >= 0), "Dataset write succeeded");

        if (data) free(data);

        VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
        VRFY((H5Fclose(file_id) >= 0), "File close succeeded");
    }

    correct_buf_size = dataset_dims[0] * dataset_dims[1] * dataset_dims[2] * sizeof(*correct_buf);

    correct_buf = (C_DATATYPE *) calloc(1, correct_buf_size);
    VRFY((NULL != correct_buf), "calloc succeeded");

    read_buf = (C_DATATYPE *) calloc(1, correct_buf_size);
    VRFY((NULL != read_buf), "calloc succeeded");

    for (i = 0; i < correct_buf_size / sizeof(*correct_buf); i++)
        correct_buf[i] = (long) i;

    /* All ranks open the file and verify their "portion" of the dataset is correct */
    plist_id = H5Pcreate(H5P_FILE_ACCESS);
    VRFY((plist_id >= 0), "FAPL creation succeeded");

    VRFY((H5Pset_fapl_mpio(plist_id, comm, info) >= 0), "Set FAPL MPIO succeeded");

    VRFY((H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0), "Set libver bounds succeeded");

    file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, plist_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    VRFY((H5Pclose(plist_id) >= 0), "FAPL close succeeded");

    dset_id = H5Dopen2(file_id, "/" WRITE_SERIAL_READ_PARALLEL_DATASET_NAME, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    plist_id = H5Pcreate(H5P_DATASET_XFER);
    VRFY((plist_id >= 0), "DXPL creation succeeded");

    VRFY((H5Pset_dxpl_mpio(plist_id, H5FD_MPIO_COLLECTIVE) >= 0), "Set DXPL MPIO succeeded");

    VRFY((H5Dread(dset_id, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, plist_id, read_buf) >= 0), "Dataset read succeeded");

    VRFY((0 == memcmp(read_buf, correct_buf, correct_buf_size)), "Data verification succeeded");

    if (correct_buf) free(correct_buf);
    if (read_buf) free(read_buf);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
    VRFY((H5Pclose(plist_id) >= 0), "DXPL close succeeded");
    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    return;
}

/*
 * Tests parallel write of filtered data
 * to a dataset. After the write has
 * succeeded, the dataset is closed and
 * then re-opened and read by a single
 * process to ensure data correctness.
 *
 * Programmer: Jordan Henderson
 *             08/03/2017
 */
static void
test_write_parallel_read_serial(void)
{
    C_DATATYPE *data = NULL;
    C_DATATYPE *read_buf = NULL;
    C_DATATYPE *correct_buf = NULL;
    hsize_t     dataset_dims[WRITE_PARALLEL_READ_SERIAL_DATASET_DIMS];
    hsize_t     chunk_dims[WRITE_PARALLEL_READ_SERIAL_DATASET_DIMS];
    hsize_t     sel_dims[WRITE_PARALLEL_READ_SERIAL_DATASET_DIMS];
    hsize_t     count[WRITE_PARALLEL_READ_SERIAL_DATASET_DIMS];
    hsize_t     stride[WRITE_PARALLEL_READ_SERIAL_DATASET_DIMS];
    hsize_t     block[WRITE_PARALLEL_READ_SERIAL_DATASET_DIMS];
    hsize_t     offset[WRITE_PARALLEL_READ_SERIAL_DATASET_DIMS];
    size_t      i, data_size, correct_buf_size;
    hid_t       file_id = -1, dset_id = -1, plist_id = -1;
    hid_t       filespace = -1, memspace = -1;

    if (MAINPROCESS) puts("Testing write file in parallel; read serially");

    /* Set up file access property list with parallel I/O access */
    plist_id = H5Pcreate(H5P_FILE_ACCESS);
    VRFY((plist_id >= 0), "FAPL creation succeeded");

    VRFY((H5Pset_fapl_mpio(plist_id, comm, info) >= 0), "Set FAPL MPIO succeeded");

    VRFY((H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0), "Set libver bounds succeeded");

    file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, plist_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    VRFY((H5Pclose(plist_id) >= 0), "FAPL close succeeded");

    /* Create the dataspace for the dataset */
    dataset_dims[0] = (hsize_t) WRITE_PARALLEL_READ_SERIAL_NROWS;
    dataset_dims[1] = (hsize_t) WRITE_PARALLEL_READ_SERIAL_NCOLS;
    dataset_dims[2] = (hsize_t) WRITE_PARALLEL_READ_SERIAL_DEPTH;
    chunk_dims[0]   = (hsize_t) WRITE_PARALLEL_READ_SERIAL_CH_NROWS;
    chunk_dims[1]   = (hsize_t) WRITE_PARALLEL_READ_SERIAL_CH_NCOLS;
    chunk_dims[2]   = 1;
    sel_dims[0]     = (hsize_t) WRITE_PARALLEL_READ_SERIAL_CH_NROWS;
    sel_dims[1]     = (hsize_t) WRITE_PARALLEL_READ_SERIAL_NCOLS;
    sel_dims[2]     = (hsize_t) WRITE_PARALLEL_READ_SERIAL_DEPTH;

    filespace = H5Screate_simple(WRITE_PARALLEL_READ_SERIAL_DATASET_DIMS, dataset_dims, NULL);
    VRFY((filespace >= 0), "File dataspace creation succeeded");

    memspace = H5Screate_simple(WRITE_PARALLEL_READ_SERIAL_DATASET_DIMS, sel_dims, NULL);
    VRFY((memspace >= 0), "Memory dataspace creation succeeded");

    /* Create chunked dataset */
    plist_id = H5Pcreate(H5P_DATASET_CREATE);
    VRFY((plist_id >= 0), "DCPL creation succeeded");

    VRFY((H5Pset_chunk(plist_id, WRITE_PARALLEL_READ_SERIAL_DATASET_DIMS, chunk_dims) >= 0), "Chunk size set");

    /* Add test filter to the pipeline */
    VRFY((SET_FILTER(plist_id) >= 0), "Filter set");

    dset_id = H5Dcreate2(file_id, WRITE_PARALLEL_READ_SERIAL_DATASET_NAME, HDF5_DATATYPE_NAME, filespace,
            H5P_DEFAULT, plist_id, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset creation succeeded");

    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

    /* Each process defines the dataset selection in memory and writes
     * it to the hyperslab in the file
     */
    count[0]  = 1;
    count[1]  = (hsize_t) WRITE_PARALLEL_READ_SERIAL_NCOLS / (hsize_t) WRITE_PARALLEL_READ_SERIAL_CH_NCOLS;
    count[2]  = (hsize_t) mpi_size;
    stride[0] = (hsize_t) WRITE_PARALLEL_READ_SERIAL_CH_NROWS;
    stride[1] = (hsize_t) WRITE_PARALLEL_READ_SERIAL_CH_NCOLS;
    stride[2] = 1;
    block[0]  = (hsize_t) WRITE_PARALLEL_READ_SERIAL_CH_NROWS;
    block[1]  = (hsize_t) WRITE_PARALLEL_READ_SERIAL_CH_NCOLS;
    block[2]  = 1;
    offset[0] = ((hsize_t) mpi_rank * (hsize_t) WRITE_PARALLEL_READ_SERIAL_CH_NROWS * count[0]);
    offset[1] = 0;
    offset[2] = 0;

    if (VERBOSE_MED)
        printf("Process %d is writing with count[ %llu, %llu, %llu ], stride[ %llu, %llu, %llu ], offset[ %llu, %llu, %llu ], block size[ %llu, %llu, %llu ]\n",
            mpi_rank, count[0], count[1], count[2], stride[0], stride[1], stride[2], offset[0], offset[1], offset[2], block[0], block[1], block[2]);

    /* Select hyperslab in the file */
    filespace = H5Dget_space(dset_id);
    VRFY((filespace >= 0), "File dataspace retrieval succeeded");

    VRFY((H5Sselect_hyperslab(filespace, H5S_SELECT_SET, offset, stride, count, block) >= 0), "Hyperslab selection succeeded");

    /* Fill data buffer */
    data_size = sel_dims[0] * sel_dims[1] * sel_dims[2] * sizeof(*data);

    data = (C_DATATYPE *) calloc(1, data_size);
    VRFY((NULL != data), "calloc succeeded");

    for (i = 0; i < data_size / sizeof(*data); i++)
        data[i] = (C_DATATYPE) GEN_DATA(i);

    /* Create property list for collective dataset write */
    plist_id = H5Pcreate(H5P_DATASET_XFER);
    VRFY((plist_id >= 0), "DXPL creation succeeded");

    VRFY((H5Pset_dxpl_mpio(plist_id, H5FD_MPIO_COLLECTIVE) >= 0), "Set DXPL MPIO succeeded");

    VRFY((H5Dwrite(dset_id, HDF5_DATATYPE_NAME, memspace, filespace, plist_id, data) >= 0), "Dataset write succeeded");

    if (data) free(data);

    VRFY((H5Pclose(plist_id) >= 0), "DXPL close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Sclose(memspace) >= 0), "Memory dataspace close succeeded");
    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    if (MAINPROCESS) {
        plist_id = H5Pcreate(H5P_FILE_ACCESS);
        VRFY((plist_id >= 0), "FAPL creation succeeded");

        VRFY((H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0), "Set libver bounds succeeded");

        file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, plist_id);
        VRFY((file_id >= 0), "Test file open succeeded");

        VRFY((H5Pclose(plist_id) >= 0), "FAPL close succeeded");

        dset_id = H5Dopen2(file_id, "/" WRITE_PARALLEL_READ_SERIAL_DATASET_NAME, H5P_DEFAULT);
        VRFY((dset_id >= 0), "Dataset open succeeded");

        correct_buf_size = dataset_dims[0] * dataset_dims[1] * dataset_dims[2] * sizeof(*correct_buf);

        correct_buf = (C_DATATYPE *) calloc(1, correct_buf_size);
        VRFY((NULL != correct_buf), "calloc succeeded");

        read_buf = (C_DATATYPE *) calloc(1, correct_buf_size);
        VRFY((NULL != read_buf), "calloc succeeded");

        for (i = 0; i < correct_buf_size / sizeof(*correct_buf); i++)
            correct_buf[i] = (C_DATATYPE) ((i % (dataset_dims[0] * dataset_dims[1])) + (i / (dataset_dims[0] * dataset_dims[1])));;

        VRFY((H5Dread(dset_id, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, H5P_DEFAULT, read_buf) >= 0), "Dataset read succeeded");

        VRFY((0 == memcmp(read_buf, correct_buf, correct_buf_size)), "Data verification succeeded");

        VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
        VRFY((H5Fclose(file_id) >= 0), "File close succeeded");
    }

    return;
}

int
main(int argc, char** argv)
{
    size_t i;
    hid_t  file_id = -1, fapl = -1;
    int    mpi_code;

    /* Initialize MPI */
    MPI_Init(&argc, &argv);
    MPI_Comm_size(comm, &mpi_size);
    MPI_Comm_rank(comm, &mpi_rank);

    if (mpi_size <= 0) {
        if (MAINPROCESS) {
            printf("The Parallel Filters tests require at least 1 rank.\n");
            printf("Quitting...\n");
        }

        MPI_Abort(MPI_COMM_WORLD, 1);
    }

    if (H5dont_atexit() < 0) {
        printf("Failed to turn off atexit processing. Continue.\n");
    }

    H5open();

    if (MAINPROCESS) {
        printf("==========================\n");
        printf("Parallel Filters tests\n");
        printf("==========================\n\n");
    }

    if (VERBOSE_MED) h5_show_hostname();

    ALARM_ON;

    /* Create test file */
    fapl = H5Pcreate(H5P_FILE_ACCESS);
    VRFY((fapl >= 0), "FAPL creation succeeded");

    VRFY((H5Pset_fapl_mpio(fapl, comm, info) >= 0), "Set FAPL MPIO succeeded");

    VRFY((H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0), "Set libver bounds succeeded");

    VRFY((h5_fixname(FILENAME[0], fapl, filenames[0], sizeof(filenames[0])) != NULL), "Test file name created");

    file_id = H5Fcreate(filenames[0], H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
    VRFY((file_id >= 0), "Test file creation succeeded");

    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    for (i = 0; i < ARRAY_SIZE(tests); i++) {
        if (MPI_SUCCESS == (mpi_code = MPI_Barrier(comm))) {
            (*tests[i])();
        } else {
            if (MAINPROCESS) MESG("MPI_Barrier failed");
            nerrors++;
        }
    }

    if (nerrors) goto exit;

    if (MAINPROCESS) puts("All Parallel Filters tests passed\n");

exit:
    if (nerrors)
        if (MAINPROCESS) printf("*** %d TEST ERROR%s OCCURRED ***\n", nerrors, nerrors > 1 ? "S" : "");

    ALARM_OFF;

    h5_clean_files(FILENAME, fapl);

    H5close();

    MPI_Finalize();

    exit((nerrors ? EXIT_FAILURE : EXIT_SUCCESS));
}
