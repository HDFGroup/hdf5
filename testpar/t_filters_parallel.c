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

size_t cur_filter_idx = 0;
#define GZIP_INDEX 0
#define FLETCHER32_INDEX 1

#define ARRAY_SIZE(a) sizeof(a) / sizeof(a[0])

/*
 * Used to check if a filter is available before running a test.
 */
#define CHECK_CUR_FILTER_AVAIL()                                               \
{                                                                              \
    htri_t filter_is_avail;                                                    \
                                                                               \
    if (cur_filter_idx == GZIP_INDEX) {                                        \
        if ((filter_is_avail = H5Zfilter_avail(H5Z_FILTER_DEFLATE)) != TRUE) { \
            if (MAINPROCESS) {                                                 \
                HDputs("    - SKIPPED - Deflate filter not available");        \
            }                                                                  \
            return;                                                            \
        }                                                                      \
    }                                                                          \
}

static herr_t set_dcpl_filter(hid_t dcpl);

#if MPI_VERSION >= 3
/* Tests for writing data in parallel */
static void test_write_one_chunk_filtered_dataset(void);
static void test_write_filtered_dataset_no_overlap(void);
static void test_write_filtered_dataset_overlap(void);
static void test_write_filtered_dataset_single_no_selection(void);
static void test_write_filtered_dataset_all_no_selection(void);
static void test_write_filtered_dataset_point_selection(void);
static void test_write_filtered_dataset_interleaved_write(void);
static void test_write_3d_filtered_dataset_no_overlap_separate_pages(void);
static void test_write_3d_filtered_dataset_no_overlap_same_pages(void);
static void test_write_3d_filtered_dataset_overlap(void);
static void test_write_cmpd_filtered_dataset_no_conversion_unshared(void);
static void test_write_cmpd_filtered_dataset_no_conversion_shared(void);
static void test_write_cmpd_filtered_dataset_type_conversion_unshared(void);
static void test_write_cmpd_filtered_dataset_type_conversion_shared(void);
#endif

/* Tests for reading data in parallel */
static void test_read_one_chunk_filtered_dataset(void);
static void test_read_filtered_dataset_no_overlap(void);
static void test_read_filtered_dataset_overlap(void);
static void test_read_filtered_dataset_single_no_selection(void);
static void test_read_filtered_dataset_all_no_selection(void);
static void test_read_filtered_dataset_point_selection(void);
static void test_read_filtered_dataset_interleaved_read(void);
static void test_read_3d_filtered_dataset_no_overlap_separate_pages(void);
static void test_read_3d_filtered_dataset_no_overlap_same_pages(void);
static void test_read_3d_filtered_dataset_overlap(void);
static void test_read_cmpd_filtered_dataset_no_conversion_unshared(void);
static void test_read_cmpd_filtered_dataset_no_conversion_shared(void);
static void test_read_cmpd_filtered_dataset_type_conversion_unshared(void);
static void test_read_cmpd_filtered_dataset_type_conversion_shared(void);

#if MPI_VERSION >= 3
/* Other miscellaneous tests */
static void test_shrinking_growing_chunks(void);
#endif

/*
 * Tests for attempting to round-trip the data going from
 *
 * written serially -> read in parallel
 *
 * and
 *
 * written in parallel -> read serially
 */
static void test_write_serial_read_parallel(void);
#if MPI_VERSION >= 3
static void test_write_parallel_read_serial(void);
#endif

static MPI_Comm comm = MPI_COMM_WORLD;
static MPI_Info info = MPI_INFO_NULL;
static int      mpi_rank;
static int      mpi_size;

static void (*tests[])(void) = {
#if MPI_VERSION >= 3
    test_write_one_chunk_filtered_dataset,
    test_write_filtered_dataset_no_overlap,
    test_write_filtered_dataset_overlap,
    test_write_filtered_dataset_single_no_selection,
    test_write_filtered_dataset_all_no_selection,
    test_write_filtered_dataset_point_selection,
    test_write_filtered_dataset_interleaved_write,
    test_write_3d_filtered_dataset_no_overlap_separate_pages,
    test_write_3d_filtered_dataset_no_overlap_same_pages,
    test_write_3d_filtered_dataset_overlap,
    test_write_cmpd_filtered_dataset_no_conversion_unshared,
    test_write_cmpd_filtered_dataset_no_conversion_shared,
    test_write_cmpd_filtered_dataset_type_conversion_unshared,
    test_write_cmpd_filtered_dataset_type_conversion_shared,
#endif
    test_read_one_chunk_filtered_dataset,
    test_read_filtered_dataset_no_overlap,
    test_read_filtered_dataset_overlap,
    test_read_filtered_dataset_single_no_selection,
    test_read_filtered_dataset_all_no_selection,
    test_read_filtered_dataset_point_selection,
    test_read_filtered_dataset_interleaved_read,
    test_read_3d_filtered_dataset_no_overlap_separate_pages,
    test_read_3d_filtered_dataset_no_overlap_same_pages,
    test_read_3d_filtered_dataset_overlap,
    test_read_cmpd_filtered_dataset_no_conversion_unshared,
    test_read_cmpd_filtered_dataset_no_conversion_shared,
    test_read_cmpd_filtered_dataset_type_conversion_unshared,
    test_read_cmpd_filtered_dataset_type_conversion_shared,
    test_write_serial_read_parallel,
#if MPI_VERSION >= 3
    test_write_parallel_read_serial,
    test_shrinking_growing_chunks,
#endif
};

/*
 * Function to call the appropriate HDF5 filter-setting function
 * depending on the currently set index. Used to re-run the tests
 * with different filters to check that the data still comes back
 * correctly under a variety of circumstances, such as the
 * Fletcher32 checksum filter increasing the size of the chunk.
 */
static herr_t
set_dcpl_filter(hid_t dcpl)
{
    switch (cur_filter_idx) {
        case GZIP_INDEX:
            return H5Pset_deflate(dcpl, DEFAULT_DEFLATE_LEVEL);
        case FLETCHER32_INDEX:
            return H5Pset_fletcher32(dcpl);
        default:
            return H5Pset_deflate(dcpl, DEFAULT_DEFLATE_LEVEL);
    }
}

#if MPI_VERSION >= 3
/*
 * Tests parallel write of filtered data in the special
 * case where a dataset is composed of a single chunk.
 *
 * Programmer: Jordan Henderson
 *             02/01/2017
 */
static void
test_write_one_chunk_filtered_dataset(void)
{
    C_DATATYPE *data = NULL;
    C_DATATYPE *read_buf = NULL;
    C_DATATYPE *correct_buf = NULL;
    hsize_t     dataset_dims[WRITE_ONE_CHUNK_FILTERED_DATASET_DIMS];
    hsize_t     chunk_dims[WRITE_ONE_CHUNK_FILTERED_DATASET_DIMS];
    hsize_t     sel_dims[WRITE_ONE_CHUNK_FILTERED_DATASET_DIMS];
    hsize_t     start[WRITE_ONE_CHUNK_FILTERED_DATASET_DIMS];
    hsize_t     stride[WRITE_ONE_CHUNK_FILTERED_DATASET_DIMS];
    hsize_t     count[WRITE_ONE_CHUNK_FILTERED_DATASET_DIMS];
    hsize_t     block[WRITE_ONE_CHUNK_FILTERED_DATASET_DIMS];
    size_t      i, data_size, correct_buf_size;
    hid_t       file_id = -1, dset_id = -1, plist_id = -1;
    hid_t       filespace = -1, memspace = -1;

    if (MAINPROCESS) HDputs("Testing write to one-chunk filtered dataset");

    CHECK_CUR_FILTER_AVAIL();

    /* Set up file access property list with parallel I/O access */
    plist_id = H5Pcreate(H5P_FILE_ACCESS);
    VRFY((plist_id >= 0), "FAPL creation succeeded");

    VRFY((H5Pset_fapl_mpio(plist_id, comm, info) >= 0),
            "Set FAPL MPIO succeeded");
    VRFY((H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0),
            "Set libver bounds succeeded");

    file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, plist_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    VRFY((H5Pclose(plist_id) >= 0), "FAPL close succeeded");

    /* Create the dataspace for the dataset */
    dataset_dims[0] = (hsize_t) WRITE_ONE_CHUNK_FILTERED_DATASET_NROWS;
    dataset_dims[1] = (hsize_t) WRITE_ONE_CHUNK_FILTERED_DATASET_NCOLS;
    chunk_dims[0] = (hsize_t) WRITE_ONE_CHUNK_FILTERED_DATASET_CH_NROWS;
    chunk_dims[1] = (hsize_t) WRITE_ONE_CHUNK_FILTERED_DATASET_CH_NCOLS;
    sel_dims[0] = (hsize_t) WRITE_ONE_CHUNK_FILTERED_DATASET_NROWS / (hsize_t) mpi_size;
    sel_dims[1] = (hsize_t) WRITE_ONE_CHUNK_FILTERED_DATASET_NCOLS;

    filespace = H5Screate_simple(WRITE_ONE_CHUNK_FILTERED_DATASET_DIMS, dataset_dims, NULL);
    VRFY((filespace >= 0), "File dataspace creation succeeded");

    memspace = H5Screate_simple(WRITE_ONE_CHUNK_FILTERED_DATASET_DIMS, sel_dims, NULL);
    VRFY((memspace >= 0), "Memory dataspace creation succeeded");

    /* Create chunked dataset */
    plist_id = H5Pcreate(H5P_DATASET_CREATE);
    VRFY((plist_id >= 0), "DCPL creation succeeded");

    VRFY((H5Pset_chunk(plist_id, WRITE_ONE_CHUNK_FILTERED_DATASET_DIMS, chunk_dims) >= 0),
            "Chunk size set");

    /* Add test filter to the pipeline */
    VRFY((set_dcpl_filter(plist_id) >= 0), "Filter set");

    dset_id = H5Dcreate2(file_id, WRITE_ONE_CHUNK_FILTERED_DATASET_NAME, HDF5_DATATYPE_NAME, filespace,
            H5P_DEFAULT, plist_id, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset creation succeeded");

    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

    /* Each process defines the dataset selection in memory and writes
     * it to the hyperslab in the file
     */
    count[0] = 1;
    count[1] = 1;
    stride[0] = (hsize_t) WRITE_ONE_CHUNK_FILTERED_DATASET_CH_NROWS;
    stride[1] = (hsize_t) WRITE_ONE_CHUNK_FILTERED_DATASET_CH_NCOLS;
    block[0] = sel_dims[0];
    block[1] = sel_dims[1];
    start[0] = ((hsize_t) mpi_rank * sel_dims[0]);
    start[1] = 0;

    if (VERBOSE_MED) {
        HDprintf("Process %d is writing with count[ %llu, %llu ], stride[ %llu, %llu ], start[ %llu, %llu ], block size[ %llu, %llu ]\n",
                mpi_rank, count[0], count[1], stride[0], stride[1], start[0], start[1], block[0], block[1]);
        HDfflush(stdout);
    }

    /* Select hyperslab in the file */
    filespace = H5Dget_space(dset_id);
    VRFY((filespace >= 0), "File dataspace retrieval succeeded");

    VRFY((H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, stride, count, block) >= 0),
            "Hyperslab selection succeeded");

    /* Fill data buffer */
    data_size = (hsize_t) WRITE_ONE_CHUNK_FILTERED_DATASET_CH_NROWS
              * (hsize_t) WRITE_ONE_CHUNK_FILTERED_DATASET_NCOLS * sizeof(*data);
    correct_buf_size = dataset_dims[0] * dataset_dims[1] * sizeof(*correct_buf);

    data = (C_DATATYPE *) HDcalloc(1, data_size);
    VRFY((NULL != data), "HDcalloc succeeded");

    correct_buf = (C_DATATYPE *) HDcalloc(1, correct_buf_size);
    VRFY((NULL != correct_buf), "HDcalloc succeeded");

    for (i = 0; i < data_size / sizeof(*data); i++)
        data[i] = (C_DATATYPE) GEN_DATA(i);

    for (i = 0; i < correct_buf_size / sizeof(*correct_buf); i++)
        correct_buf[i] = ((C_DATATYPE) i % (WRITE_ONE_CHUNK_FILTERED_DATASET_CH_NROWS / mpi_size * WRITE_ONE_CHUNK_FILTERED_DATASET_CH_NCOLS))
                       + ((C_DATATYPE) i / (WRITE_ONE_CHUNK_FILTERED_DATASET_CH_NROWS / mpi_size * WRITE_ONE_CHUNK_FILTERED_DATASET_CH_NCOLS));

    /* Create property list for collective dataset write */
    plist_id = H5Pcreate(H5P_DATASET_XFER);
    VRFY((plist_id >= 0), "DXPL creation succeeded");

    VRFY((H5Pset_dxpl_mpio(plist_id, H5FD_MPIO_COLLECTIVE) >= 0),
            "Set DXPL MPIO succeeded");

    VRFY((H5Dwrite(dset_id, HDF5_DATATYPE_NAME, memspace, filespace, plist_id, data) >= 0),
            "Dataset write succeeded");

    if (data) HDfree(data);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");

    /* Verify the correct data was written */
    read_buf = (C_DATATYPE *) HDcalloc(1, correct_buf_size);
    VRFY((NULL != read_buf), "HDcalloc succeeded");

    dset_id = H5Dopen2(file_id, "/" WRITE_ONE_CHUNK_FILTERED_DATASET_NAME, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    VRFY((H5Dread(dset_id, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, plist_id, read_buf) >= 0),
            "Dataset read succeeded");

    VRFY((0 == HDmemcmp(read_buf, correct_buf, correct_buf_size)),
            "Data verification succeeded");

    if (correct_buf) HDfree(correct_buf);
    if (read_buf) HDfree(read_buf);

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
test_write_filtered_dataset_no_overlap(void)
{
    C_DATATYPE *data = NULL;
    C_DATATYPE *read_buf = NULL;
    C_DATATYPE *correct_buf = NULL;
    hsize_t     dataset_dims[WRITE_UNSHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     chunk_dims[WRITE_UNSHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     sel_dims[WRITE_UNSHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     start[WRITE_UNSHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     stride[WRITE_UNSHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     count[WRITE_UNSHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     block[WRITE_UNSHARED_FILTERED_CHUNKS_DATASET_DIMS];
    size_t      i, data_size, correct_buf_size;
    hid_t       file_id = -1, dset_id = -1, plist_id = -1;
    hid_t       filespace = -1, memspace = -1;

    if (MAINPROCESS) HDputs("Testing write to unshared filtered chunks");

    CHECK_CUR_FILTER_AVAIL();

    /* Set up file access property list with parallel I/O access */
    plist_id = H5Pcreate(H5P_FILE_ACCESS);
    VRFY((plist_id >= 0), "FAPL creation succeeded");

    VRFY((H5Pset_fapl_mpio(plist_id, comm, info) >= 0),
            "Set FAPL MPIO succeeded");

    VRFY((H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0),
            "Set libver bounds succeeded");

    file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, plist_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    VRFY((H5Pclose(plist_id) >= 0), "FAPL close succeeded");

    /* Create the dataspace for the dataset */
    dataset_dims[0] = (hsize_t) WRITE_UNSHARED_FILTERED_CHUNKS_NROWS;
    dataset_dims[1] = (hsize_t) WRITE_UNSHARED_FILTERED_CHUNKS_NCOLS;
    chunk_dims[0] = (hsize_t) WRITE_UNSHARED_FILTERED_CHUNKS_CH_NROWS;
    chunk_dims[1] = (hsize_t) WRITE_UNSHARED_FILTERED_CHUNKS_CH_NCOLS;
    sel_dims[0] = (hsize_t) WRITE_UNSHARED_FILTERED_CHUNKS_CH_NROWS;
    sel_dims[1] = (hsize_t) WRITE_UNSHARED_FILTERED_CHUNKS_NCOLS;

    filespace = H5Screate_simple(WRITE_UNSHARED_FILTERED_CHUNKS_DATASET_DIMS, dataset_dims, NULL);
    VRFY((filespace >= 0), "File dataspace creation succeeded");

    memspace = H5Screate_simple(WRITE_UNSHARED_FILTERED_CHUNKS_DATASET_DIMS, sel_dims, NULL);
    VRFY((memspace >= 0), "Memory dataspace creation succeeded");

    /* Create chunked dataset */
    plist_id = H5Pcreate(H5P_DATASET_CREATE);
    VRFY((plist_id >= 0), "DCPL creation succeeded");

    VRFY((H5Pset_chunk(plist_id, WRITE_UNSHARED_FILTERED_CHUNKS_DATASET_DIMS, chunk_dims) >= 0),
            "Chunk size set");

    /* Add test filter to the pipeline */
    VRFY((set_dcpl_filter(plist_id) >= 0), "Filter set");

    dset_id = H5Dcreate2(file_id, WRITE_UNSHARED_FILTERED_CHUNKS_DATASET_NAME, HDF5_DATATYPE_NAME, filespace,
            H5P_DEFAULT, plist_id, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset creation succeeded");

    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

    /* Each process defines the dataset selection in memory and writes
     * it to the hyperslab in the file
     */
    count[0] = 1;
    count[1] = (hsize_t) WRITE_UNSHARED_FILTERED_CHUNKS_NCOLS / (hsize_t) WRITE_UNSHARED_FILTERED_CHUNKS_CH_NCOLS;
    stride[0] = (hsize_t) WRITE_UNSHARED_FILTERED_CHUNKS_CH_NROWS;
    stride[1] = (hsize_t) WRITE_UNSHARED_FILTERED_CHUNKS_CH_NCOLS;
    block[0] = (hsize_t) WRITE_UNSHARED_FILTERED_CHUNKS_CH_NROWS;
    block[1] = (hsize_t) WRITE_UNSHARED_FILTERED_CHUNKS_CH_NCOLS;
    start[0] = ((hsize_t) mpi_rank * (hsize_t) WRITE_UNSHARED_FILTERED_CHUNKS_CH_NROWS * count[0]);
    start[1] = 0;

    if (VERBOSE_MED) {
        HDprintf("Process %d is writing with count[ %llu, %llu ], stride[ %llu, %llu ], start[ %llu, %llu ], block size[ %llu, %llu ]\n",
                mpi_rank, count[0], count[1], stride[0], stride[1], start[0], start[1], block[0], block[1]);
        HDfflush(stdout);
    }

    /* Select hyperslab in the file */
    filespace = H5Dget_space(dset_id);
    VRFY((dset_id >= 0), "File dataspace retrieval succeeded");

    VRFY((H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, stride, count, block) >= 0),
            "Hyperslab selection succeeded");

    /* Fill data buffer */
    data_size = sel_dims[0] * sel_dims[1] * sizeof(*data);
    correct_buf_size = dataset_dims[0] * dataset_dims[1] * sizeof(*correct_buf);

    data = (C_DATATYPE *) HDcalloc(1, data_size);
    VRFY((NULL != data), "HDcalloc succeeded");

    correct_buf = (C_DATATYPE *) HDcalloc(1, correct_buf_size);
    VRFY((NULL != correct_buf), "HDcalloc succeeded");

    for (i = 0; i < data_size / sizeof(*data); i++)
        data[i] = (C_DATATYPE) GEN_DATA(i);

    for (i = 0; i < correct_buf_size / sizeof(*correct_buf); i++)
        correct_buf[i] =
                (C_DATATYPE) (
                                 (i % (dataset_dims[0] / (hsize_t) mpi_size * dataset_dims[1]))
                               + (i / (dataset_dims[0] / (hsize_t) mpi_size * dataset_dims[1]))
                             );

    /* Create property list for collective dataset write */
    plist_id = H5Pcreate(H5P_DATASET_XFER);
    VRFY((plist_id >= 0), "DXPL creation succeeded");

    VRFY((H5Pset_dxpl_mpio(plist_id, H5FD_MPIO_COLLECTIVE) >= 0),
            "Set DXPL MPIO succeeded");

    VRFY((H5Dwrite(dset_id, HDF5_DATATYPE_NAME, memspace, filespace, plist_id, data) >= 0),
            "Dataset write succeeded");

    if (data) HDfree(data);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");

    /* Verify the correct data was written */
    read_buf = (C_DATATYPE *) HDcalloc(1, correct_buf_size);
    VRFY((NULL != read_buf), "HDcalloc succeeded");

    dset_id = H5Dopen2(file_id, "/" WRITE_UNSHARED_FILTERED_CHUNKS_DATASET_NAME, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    VRFY((H5Dread(dset_id, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, plist_id, read_buf) >= 0),
            "Dataset read succeeded");

    VRFY((0 == HDmemcmp(read_buf, correct_buf, correct_buf_size)),
            "Data verification succeeded");

    if (correct_buf) HDfree(correct_buf);
    if (read_buf) HDfree(read_buf);

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
test_write_filtered_dataset_overlap(void)
{
    C_DATATYPE *data = NULL;
    C_DATATYPE *read_buf = NULL;
    C_DATATYPE *correct_buf = NULL;
    hsize_t     dataset_dims[WRITE_SHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     chunk_dims[WRITE_SHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     sel_dims[WRITE_SHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     start[WRITE_SHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     stride[WRITE_SHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     count[WRITE_SHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     block[WRITE_SHARED_FILTERED_CHUNKS_DATASET_DIMS];
    size_t      i, data_size, correct_buf_size;
    hid_t       file_id = -1, dset_id = -1, plist_id = -1;
    hid_t       filespace = -1, memspace = -1;

    if (MAINPROCESS) HDputs("Testing write to shared filtered chunks");

    CHECK_CUR_FILTER_AVAIL();

    /* Set up file access property list with parallel I/O access */
    plist_id = H5Pcreate(H5P_FILE_ACCESS);
    VRFY((plist_id >= 0), "FAPL creation succeeded");

    VRFY((H5Pset_fapl_mpio(plist_id, comm, info) >= 0),
            "Set FAPL MPIO succeeded");

    VRFY((H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0),
            "Set libver bounds succeeded");

    file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, plist_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    VRFY((H5Pclose(plist_id) >= 0), "FAPL close succeeded");

    /* Create the dataspace for the dataset */
    dataset_dims[0] = (hsize_t) WRITE_SHARED_FILTERED_CHUNKS_NROWS;
    dataset_dims[1] = (hsize_t) WRITE_SHARED_FILTERED_CHUNKS_NCOLS;
    chunk_dims[0] = (hsize_t) WRITE_SHARED_FILTERED_CHUNKS_CH_NROWS;
    chunk_dims[1] = (hsize_t) WRITE_SHARED_FILTERED_CHUNKS_CH_NCOLS;
    sel_dims[0] = (hsize_t) DIM0_SCALE_FACTOR;
    sel_dims[1] = (hsize_t) WRITE_SHARED_FILTERED_CHUNKS_CH_NCOLS * (hsize_t) DIM1_SCALE_FACTOR;

    filespace = H5Screate_simple(WRITE_SHARED_FILTERED_CHUNKS_DATASET_DIMS, dataset_dims, NULL);
    VRFY((filespace >= 0), "File dataspace creation succeeded");

    memspace = H5Screate_simple(WRITE_SHARED_FILTERED_CHUNKS_DATASET_DIMS, sel_dims, NULL);
    VRFY((memspace >= 0), "Memory dataspace creation succeeded");

    /* Create chunked dataset */
    plist_id = H5Pcreate(H5P_DATASET_CREATE);
    VRFY((plist_id >= 0), "DCPL creation succeeded");

    VRFY((H5Pset_chunk(plist_id, WRITE_SHARED_FILTERED_CHUNKS_DATASET_DIMS, chunk_dims) >= 0),
            "Chunk size set");

    /* Add test filter to the pipeline */
    VRFY((set_dcpl_filter(plist_id) >= 0), "Filter set");

    dset_id = H5Dcreate2(file_id, WRITE_SHARED_FILTERED_CHUNKS_DATASET_NAME, HDF5_DATATYPE_NAME, filespace,
            H5P_DEFAULT, plist_id, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset creation succeeded");

    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

    /* Each process defines the dataset selection in memory and writes
     * it to the hyperslab in the file
     */
    count[0] = (hsize_t) WRITE_SHARED_FILTERED_CHUNKS_NROWS / (hsize_t) WRITE_SHARED_FILTERED_CHUNKS_CH_NROWS;
    count[1] = (hsize_t) WRITE_SHARED_FILTERED_CHUNKS_NCOLS / (hsize_t) WRITE_SHARED_FILTERED_CHUNKS_CH_NCOLS;
    stride[0] = (hsize_t) WRITE_SHARED_FILTERED_CHUNKS_CH_NROWS;
    stride[1] = (hsize_t) WRITE_SHARED_FILTERED_CHUNKS_CH_NCOLS;
    block[0] = (hsize_t) WRITE_SHARED_FILTERED_CHUNKS_CH_NROWS / (hsize_t) mpi_size;
    block[1] = (hsize_t) WRITE_SHARED_FILTERED_CHUNKS_CH_NCOLS;
    start[0] = (hsize_t) mpi_rank * block[0];
    start[1] = 0;

    if (VERBOSE_MED) {
        HDprintf("Process %d is writing with count[ %llu, %llu ], stride[ %llu, %llu ], start[ %llu, %llu ], block size[ %llu, %llu ]\n",
                mpi_rank, count[0], count[1], stride[0], stride[1], start[0], start[1], block[0], block[1]);
        HDfflush(stdout);
    }

    /* Select hyperslab in the file */
    filespace = H5Dget_space(dset_id);
    VRFY((filespace >= 0), "File dataspace retrieval succeeded");

    VRFY((H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, stride, count, block) >= 0),
            "Hyperslab selection succeeded");

    /* Fill data buffer */
    data_size = sel_dims[0] * sel_dims[1] * sizeof(*data);
    correct_buf_size = dataset_dims[0] * dataset_dims[1] * sizeof(*correct_buf);

    data = (C_DATATYPE *) HDcalloc(1, data_size);
    VRFY((NULL != data), "HDcalloc succeeded");

    correct_buf = (C_DATATYPE *) HDcalloc(1, correct_buf_size);
    VRFY((NULL != correct_buf), "HDcalloc succeeded");

    for (i = 0; i < data_size / sizeof(*data); i++)
        data[i] = (C_DATATYPE) GEN_DATA(i);

    for (i = 0; i < correct_buf_size / sizeof(*correct_buf); i++)
        correct_buf[i] = (C_DATATYPE) (
                                          (dataset_dims[1] * (i / ((hsize_t) mpi_size * dataset_dims[1])))
                                        + (i % dataset_dims[1])
                                        + (((i % ((hsize_t) mpi_size * dataset_dims[1])) / dataset_dims[1]) % dataset_dims[1])
                                      );

    /* Create property list for collective dataset write */
    plist_id = H5Pcreate(H5P_DATASET_XFER);
    VRFY((plist_id >= 0), "DXPL creation succeeded");

    VRFY((H5Pset_dxpl_mpio(plist_id, H5FD_MPIO_COLLECTIVE) >= 0),
            "Set DXPL MPIO succeeded");

    VRFY((H5Dwrite(dset_id, HDF5_DATATYPE_NAME, memspace, filespace, plist_id, data) >= 0),
            "Dataset write succeeded");

    if (data) HDfree(data);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");

    /* Verify correct data was written */
    read_buf = (C_DATATYPE *) HDcalloc(1, correct_buf_size);
    VRFY((NULL != read_buf), "HDcalloc succeeded");

    dset_id = H5Dopen2(file_id, "/" WRITE_SHARED_FILTERED_CHUNKS_DATASET_NAME, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    VRFY((H5Dread(dset_id, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, plist_id, read_buf) >= 0),
            "Dataset read succeeded");

    VRFY((0 == HDmemcmp(read_buf, correct_buf, correct_buf_size)),
            "Data verification succeeded");

    if (correct_buf) HDfree(correct_buf);
    if (read_buf) HDfree(read_buf);

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
test_write_filtered_dataset_single_no_selection(void)
{
    C_DATATYPE *data = NULL;
    C_DATATYPE *read_buf = NULL;
    C_DATATYPE *correct_buf = NULL;
    hsize_t     dataset_dims[WRITE_SINGLE_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     chunk_dims[WRITE_SINGLE_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     sel_dims[WRITE_SINGLE_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     start[WRITE_SINGLE_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     stride[WRITE_SINGLE_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     count[WRITE_SINGLE_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     block[WRITE_SINGLE_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    size_t      i, data_size, correct_buf_size;
    size_t      segment_length;
    hid_t       file_id = -1, dset_id = -1, plist_id = -1;
    hid_t       filespace = -1, memspace = -1;

    if (MAINPROCESS) HDputs("Testing write to filtered chunks with a single process having no selection");

    CHECK_CUR_FILTER_AVAIL();

    /* Set up file access property list with parallel I/O access */
    plist_id = H5Pcreate(H5P_FILE_ACCESS);
    VRFY((plist_id >= 0), "FAPL creation succeeded");

    VRFY((H5Pset_fapl_mpio(plist_id, comm, info) >= 0),
            "Set FAPL MPIO succeeded");

    VRFY((H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0),
            "Set libver bounds succeeded");

    file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, plist_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    VRFY((H5Pclose(plist_id) >= 0), "FAPL close succeeded");

    /* Create the dataspace for the dataset */
    dataset_dims[0] = (hsize_t) WRITE_SINGLE_NO_SELECTION_FILTERED_CHUNKS_NROWS;
    dataset_dims[1] = (hsize_t) WRITE_SINGLE_NO_SELECTION_FILTERED_CHUNKS_NCOLS;
    chunk_dims[0] = (hsize_t) WRITE_SINGLE_NO_SELECTION_FILTERED_CHUNKS_CH_NROWS;
    chunk_dims[1] = (hsize_t) WRITE_SINGLE_NO_SELECTION_FILTERED_CHUNKS_CH_NCOLS;
    sel_dims[0] = (hsize_t) WRITE_SINGLE_NO_SELECTION_FILTERED_CHUNKS_CH_NROWS;
    sel_dims[1] = (hsize_t) WRITE_SINGLE_NO_SELECTION_FILTERED_CHUNKS_NCOLS;

    if (mpi_rank == WRITE_SINGLE_NO_SELECTION_FILTERED_CHUNKS_NO_SELECT_PROC)
        sel_dims[0] = sel_dims[1] = 0;

    filespace = H5Screate_simple(WRITE_SINGLE_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS, dataset_dims, NULL);
    VRFY((filespace >= 0), "File dataspace creation succeeded");

    memspace = H5Screate_simple(WRITE_SINGLE_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS, sel_dims, NULL);
    VRFY((memspace >= 0), "Memory dataspace creation succeeded");

    /* Create chunked dataset */
    plist_id = H5Pcreate(H5P_DATASET_CREATE);
    VRFY((plist_id >= 0), "DCPL creation succeeded");

    VRFY((H5Pset_chunk(plist_id, WRITE_SINGLE_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS, chunk_dims) >= 0),
            "Chunk size set");

    /* Add test filter to the pipeline */
    VRFY((set_dcpl_filter(plist_id) >= 0), "Filter set");

    dset_id = H5Dcreate2(file_id, WRITE_SINGLE_NO_SELECTION_FILTERED_CHUNKS_DATASET_NAME, HDF5_DATATYPE_NAME, filespace,
            H5P_DEFAULT, plist_id, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset creation succeeded");

    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

    /* Each process defines the dataset selection in memory and writes
     * it to the hyperslab in the file
     */
    count[0] = 1;
    count[1] = (hsize_t) WRITE_SINGLE_NO_SELECTION_FILTERED_CHUNKS_NCOLS / (hsize_t) WRITE_SINGLE_NO_SELECTION_FILTERED_CHUNKS_CH_NCOLS;
    stride[0] = (hsize_t) WRITE_SINGLE_NO_SELECTION_FILTERED_CHUNKS_CH_NROWS;
    stride[1] = (hsize_t) WRITE_SINGLE_NO_SELECTION_FILTERED_CHUNKS_CH_NCOLS;
    block[0] = (hsize_t) WRITE_SINGLE_NO_SELECTION_FILTERED_CHUNKS_CH_NROWS;
    block[1] = (hsize_t) WRITE_SINGLE_NO_SELECTION_FILTERED_CHUNKS_CH_NCOLS;
    start[0] = (hsize_t) mpi_rank * (hsize_t) WRITE_SINGLE_NO_SELECTION_FILTERED_CHUNKS_CH_NROWS * count[0];
    start[1] = 0;

    if (VERBOSE_MED) {
        HDprintf("Process %d is writing with count[ %llu, %llu ], stride[ %llu, %llu ], start[ %llu, %llu ], block size[ %llu, %llu ]\n",
                mpi_rank, count[0], count[1], stride[0], stride[1], start[0], start[1], block[0], block[1]);
        HDfflush(stdout);
    }

    /* Select hyperslab in the file */
    filespace = H5Dget_space(dset_id);
    VRFY((filespace >= 0), "File dataspace retrieval succeeded");

    if (mpi_rank == WRITE_SINGLE_NO_SELECTION_FILTERED_CHUNKS_NO_SELECT_PROC)
        VRFY((H5Sselect_none(filespace) >= 0), "Select none succeeded");
    else
        VRFY((H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, stride, count, block) >= 0),
                "Hyperslab selection succeeded");

    /* Fill data buffer */
    data_size = sel_dims[0] * sel_dims[1] * sizeof(*data);
    correct_buf_size = dataset_dims[0] * dataset_dims[1] * sizeof(*correct_buf);

    data = (C_DATATYPE *) HDcalloc(1, data_size);
    VRFY((NULL != data), "HDcalloc succeeded");

    correct_buf = (C_DATATYPE *) HDcalloc(1, correct_buf_size);
    VRFY((NULL != correct_buf), "HDcalloc succeeded");

    for (i = 0; i < data_size / sizeof(*data); i++)
        data[i] = (C_DATATYPE) GEN_DATA(i);

    for (i = 0; i < correct_buf_size / sizeof(*correct_buf); i++)
        correct_buf[i] =
                (C_DATATYPE) (
                                (i % (dataset_dims[0] / (hsize_t) mpi_size * dataset_dims[1]))
                              + (i / (dataset_dims[0] / (hsize_t) mpi_size * dataset_dims[1]))
                             );

    /* Compute the correct offset into the buffer for the process having no selection and clear it */
    segment_length = dataset_dims[0] * dataset_dims[1] / (hsize_t) mpi_size;
    HDmemset(correct_buf + ((size_t) WRITE_SINGLE_NO_SELECTION_FILTERED_CHUNKS_NO_SELECT_PROC * segment_length),
            0, segment_length * sizeof(*data));

    /* Create property list for collective dataset write */
    plist_id = H5Pcreate(H5P_DATASET_XFER);
    VRFY((plist_id >= 0), "DXPL creation succeeded");

    VRFY((H5Pset_dxpl_mpio(plist_id, H5FD_MPIO_COLLECTIVE) >= 0),
            "Set DXPL MPIO succeeded");

    VRFY((H5Dwrite(dset_id, HDF5_DATATYPE_NAME, memspace, filespace, plist_id, data) >= 0),
            "Dataset write succeeded");

    if (data) HDfree(data);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");

    /* Verify the correct data was written */
    read_buf = (C_DATATYPE *) HDcalloc(1, correct_buf_size);
    VRFY((NULL != read_buf), "HDcalloc succeeded");

    dset_id = H5Dopen2(file_id, "/" WRITE_SINGLE_NO_SELECTION_FILTERED_CHUNKS_DATASET_NAME, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    VRFY((H5Dread(dset_id, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, plist_id, read_buf) >= 0),
            "Dataset read succeeded");

    VRFY((0 == HDmemcmp(read_buf, correct_buf, correct_buf_size)),
            "Data verification succeeded");

    if (correct_buf) HDfree(correct_buf);
    if (read_buf) HDfree(read_buf);

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
test_write_filtered_dataset_all_no_selection(void)
{
    C_DATATYPE *data = NULL;
    C_DATATYPE *read_buf = NULL;
    C_DATATYPE *correct_buf = NULL;
    hsize_t     dataset_dims[WRITE_ALL_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     chunk_dims[WRITE_ALL_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     sel_dims[WRITE_ALL_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    size_t      i, data_size, correct_buf_size;
    hid_t       file_id = -1, dset_id = -1, plist_id = -1;
    hid_t       filespace = -1, memspace = -1;

    if (MAINPROCESS) HDputs("Testing write to filtered chunks with all processes having no selection");

    CHECK_CUR_FILTER_AVAIL();

    /* Set up file access property list with parallel I/O access */
    plist_id = H5Pcreate(H5P_FILE_ACCESS);
    VRFY((plist_id >= 0), "FAPL creation succeeded");

    VRFY((H5Pset_fapl_mpio(plist_id, comm, info) >= 0),
            "Set FAPL MPIO succeeded");

    VRFY((H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0),
            "Set libver bounds succeeded");

    file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, plist_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    VRFY((H5Pclose(plist_id) >= 0), "FAPL close succeeded");

    /* Create the dataspace for the dataset */
    dataset_dims[0] = (hsize_t) WRITE_ALL_NO_SELECTION_FILTERED_CHUNKS_NROWS;
    dataset_dims[1] = (hsize_t) WRITE_ALL_NO_SELECTION_FILTERED_CHUNKS_NCOLS;
    chunk_dims[0] = (hsize_t) WRITE_ALL_NO_SELECTION_FILTERED_CHUNKS_CH_NROWS;
    chunk_dims[1] = (hsize_t) WRITE_ALL_NO_SELECTION_FILTERED_CHUNKS_CH_NCOLS;
    sel_dims[0] = sel_dims[1] = 0;

    filespace = H5Screate_simple(WRITE_ALL_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS, dataset_dims, NULL);
    VRFY((filespace >= 0), "File dataspace creation succeeded");

    memspace = H5Screate_simple(WRITE_ALL_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS, sel_dims, NULL);
    VRFY((memspace >= 0), "Memory dataspace creation succeeded");

    /* Create chunked dataset */
    plist_id = H5Pcreate(H5P_DATASET_CREATE);
    VRFY((plist_id >= 0), "DCPL creation succeeded");

    VRFY((H5Pset_chunk(plist_id, WRITE_ALL_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS, chunk_dims) >= 0),
            "Chunk size set");

    /* Add test filter to the pipeline */
    VRFY((set_dcpl_filter(plist_id) >= 0), "Filter set");

    dset_id = H5Dcreate2(file_id, WRITE_ALL_NO_SELECTION_FILTERED_CHUNKS_DATASET_NAME, HDF5_DATATYPE_NAME, filespace,
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

    data = (C_DATATYPE *) HDcalloc(1, data_size);
    VRFY((NULL != data), "HDcalloc succeeded");

    correct_buf = (C_DATATYPE *) HDcalloc(1, correct_buf_size);
    VRFY((NULL != correct_buf), "HDcalloc succeeded");

    for (i = 0; i < data_size / sizeof(*data); i++)
        data[i] = (C_DATATYPE) GEN_DATA(i);

    /* Create property list for collective dataset write */
    plist_id = H5Pcreate(H5P_DATASET_XFER);
    VRFY((plist_id >= 0), "DXPL creation succeeded");

    VRFY((H5Pset_dxpl_mpio(plist_id, H5FD_MPIO_COLLECTIVE) >= 0),
            "Set DXPL MPIO succeeded");

    VRFY((H5Dwrite(dset_id, HDF5_DATATYPE_NAME, memspace, filespace, plist_id, data) >= 0),
            "Dataset write succeeded");

    if (data) HDfree(data);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");

    /* Verify the correct data was written */
    read_buf = (C_DATATYPE *) HDcalloc(1, correct_buf_size);
    VRFY((NULL != read_buf), "HDcalloc succeeded");

    dset_id = H5Dopen2(file_id, "/" WRITE_ALL_NO_SELECTION_FILTERED_CHUNKS_DATASET_NAME, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    VRFY((H5Dread(dset_id, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, plist_id, read_buf) >= 0),
            "Dataset read succeeded");

    VRFY((0 == HDmemcmp(read_buf, correct_buf, correct_buf_size)),
            "Data verification succeeded");

    if (correct_buf) HDfree(correct_buf);
    if (read_buf) HDfree(read_buf);

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
test_write_filtered_dataset_point_selection(void)
{
    C_DATATYPE *data = NULL;
    C_DATATYPE *correct_buf = NULL;
    C_DATATYPE *read_buf = NULL;
    hsize_t    *coords = NULL;
    hsize_t     dataset_dims[WRITE_POINT_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     chunk_dims[WRITE_POINT_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     sel_dims[WRITE_POINT_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    size_t      i, j, data_size, correct_buf_size;
    size_t      num_points;
    hid_t       file_id = -1, dset_id = -1, plist_id = -1;
    hid_t       filespace = -1, memspace = -1;

    if (MAINPROCESS) HDputs("Testing write to filtered chunks with point selection");

    CHECK_CUR_FILTER_AVAIL();

    /* Set up file access property list with parallel I/O access */
    plist_id = H5Pcreate(H5P_FILE_ACCESS);
    VRFY((plist_id >= 0), "FAPL creation succeeded");

    VRFY((H5Pset_fapl_mpio(plist_id, comm, info) >= 0),
            "Set FAPL MPIO succeeded");

    VRFY((H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0),
            "Set libver bounds succeeded");

    file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, plist_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    VRFY((H5Pclose(plist_id) >= 0), "FAPL close succeeded");

    /* Create the dataspace for the dataset */
    dataset_dims[0] = (hsize_t) WRITE_POINT_SELECTION_FILTERED_CHUNKS_NROWS;
    dataset_dims[1] = (hsize_t) WRITE_POINT_SELECTION_FILTERED_CHUNKS_NCOLS;
    chunk_dims[0] = (hsize_t) WRITE_POINT_SELECTION_FILTERED_CHUNKS_CH_NROWS;
    chunk_dims[1] = (hsize_t) WRITE_POINT_SELECTION_FILTERED_CHUNKS_CH_NCOLS;
    sel_dims[0] = (hsize_t) WRITE_POINT_SELECTION_FILTERED_CHUNKS_NROWS / (hsize_t) mpi_size;
    sel_dims[1] = (hsize_t) WRITE_POINT_SELECTION_FILTERED_CHUNKS_NCOLS;

    filespace = H5Screate_simple(WRITE_POINT_SELECTION_FILTERED_CHUNKS_DATASET_DIMS, dataset_dims,NULL);
    VRFY((filespace >= 0), "File dataspace creation succeeded");

    memspace = H5Screate_simple(WRITE_POINT_SELECTION_FILTERED_CHUNKS_DATASET_DIMS, sel_dims, NULL);
    VRFY((memspace >= 0), "Memory dataspace creation succeeded");

    /* Create chunked dataset */
    plist_id = H5Pcreate(H5P_DATASET_CREATE);
    VRFY((plist_id >= 0), "DCPL creation succeeded");

    VRFY((H5Pset_chunk(plist_id, WRITE_POINT_SELECTION_FILTERED_CHUNKS_DATASET_DIMS, chunk_dims) >= 0),
            "Chunk size set");

    /* Add test filter to the pipeline */
    VRFY((set_dcpl_filter(plist_id) >= 0), "Filter set");

    dset_id = H5Dcreate2(file_id, WRITE_POINT_SELECTION_FILTERED_CHUNKS_DATASET_NAME, HDF5_DATATYPE_NAME, filespace,
            H5P_DEFAULT, plist_id, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset creation succeeded");

    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

    /* Set up point selection */
    filespace = H5Dget_space(dset_id);
    VRFY((filespace >= 0), "File dataspace retrieval succeeded");

    num_points = (hsize_t) WRITE_POINT_SELECTION_FILTERED_CHUNKS_NROWS * (hsize_t) WRITE_POINT_SELECTION_FILTERED_CHUNKS_NCOLS / (hsize_t) mpi_size;
    coords = (hsize_t *) HDcalloc(1, 2 * num_points * sizeof(*coords));
    VRFY((NULL != coords), "Coords HDcalloc succeeded");

    for (i = 0; i < num_points; i++)
        for (j = 0; j < WRITE_POINT_SELECTION_FILTERED_CHUNKS_DATASET_DIMS; j++)
            coords[(i * WRITE_POINT_SELECTION_FILTERED_CHUNKS_DATASET_DIMS) + j] =
                    (j > 0) ? (i % (hsize_t) WRITE_POINT_SELECTION_FILTERED_CHUNKS_NCOLS)
                            : ((hsize_t) mpi_rank + ((hsize_t) mpi_size * (i / (hsize_t) WRITE_POINT_SELECTION_FILTERED_CHUNKS_NCOLS)));

    VRFY((H5Sselect_elements(filespace, H5S_SELECT_SET, (hsize_t ) num_points, (const hsize_t * ) coords) >= 0),
            "Point selection succeeded");

    /* Fill data buffer */
    data_size = sel_dims[0] * sel_dims[1] * sizeof(*data);
    correct_buf_size = dataset_dims[0] * dataset_dims[1] * sizeof(*correct_buf);

    data = (C_DATATYPE *) HDcalloc(1, data_size);
    VRFY((NULL != data), "HDcalloc succeeded");

    correct_buf = (C_DATATYPE *) HDcalloc(1, correct_buf_size);
    VRFY((NULL != correct_buf), "HDcalloc succeeded");

    for (i = 0; i < data_size / sizeof(*data); i++)
        data[i] = (C_DATATYPE) GEN_DATA(i);

    for (i = 0; i < correct_buf_size / sizeof(*correct_buf); i++)
        correct_buf[i] = (C_DATATYPE) (
                                         (dataset_dims[1] * (i / ((hsize_t) mpi_size * dataset_dims[1])))
                                       + (i % dataset_dims[1])
                                       + (((i % ((hsize_t) mpi_size * dataset_dims[1])) / dataset_dims[1]) % dataset_dims[1])
                                      );

    /* Create property list for collective dataset write */
    plist_id = H5Pcreate(H5P_DATASET_XFER);
    VRFY((plist_id >= 0), "DXPL creation succeeded");

    VRFY((H5Pset_dxpl_mpio(plist_id, H5FD_MPIO_COLLECTIVE) >= 0),
            "Set DXPL MPIO succeeded");

    VRFY((H5Dwrite(dset_id, HDF5_DATATYPE_NAME, memspace, filespace, plist_id, data) >= 0),
            "Dataset write succeeded");

    if (data) HDfree(data);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");

    /* Verify the correct data was written */
    read_buf = (C_DATATYPE *) HDcalloc(1, correct_buf_size);
    VRFY((NULL != read_buf), "HDcalloc succeeded");

    dset_id = H5Dopen2(file_id, "/" WRITE_POINT_SELECTION_FILTERED_CHUNKS_DATASET_NAME, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    VRFY((H5Dread(dset_id, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, plist_id, read_buf) >= 0),
            "Dataset read succeeded");

    VRFY((0 == HDmemcmp(read_buf, correct_buf, correct_buf_size)),
            "Data verification succeeded");

    if (coords) HDfree(coords);
    if (correct_buf) HDfree(correct_buf);
    if (read_buf) HDfree(read_buf);

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
test_write_filtered_dataset_interleaved_write(void)
{
    C_DATATYPE *data = NULL;
    C_DATATYPE *read_buf = NULL;
    C_DATATYPE *correct_buf = NULL;
    hsize_t     dataset_dims[INTERLEAVED_WRITE_FILTERED_DATASET_DIMS];
    hsize_t     chunk_dims[INTERLEAVED_WRITE_FILTERED_DATASET_DIMS];
    hsize_t     sel_dims[INTERLEAVED_WRITE_FILTERED_DATASET_DIMS];
    hsize_t     start[INTERLEAVED_WRITE_FILTERED_DATASET_DIMS];
    hsize_t     stride[INTERLEAVED_WRITE_FILTERED_DATASET_DIMS];
    hsize_t     count[INTERLEAVED_WRITE_FILTERED_DATASET_DIMS];
    hsize_t     block[INTERLEAVED_WRITE_FILTERED_DATASET_DIMS];
    size_t      i, data_size, correct_buf_size;
    hid_t       file_id = -1, dset_id = -1, plist_id = -1;
    hid_t       filespace = -1, memspace = -1;

    if (MAINPROCESS) HDputs("Testing interleaved write to filtered chunks");

    CHECK_CUR_FILTER_AVAIL();

    /* Set up file access property list with parallel I/O access */
    plist_id = H5Pcreate(H5P_FILE_ACCESS);
    VRFY((plist_id >= 0), "FAPL creation succeeded");

    VRFY((H5Pset_fapl_mpio(plist_id, comm, info) >= 0),
            "Set FAPL MPIO succeeded");

    VRFY((H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0),
            "Set libver bounds succeeded");

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

    VRFY((H5Pset_chunk(plist_id, INTERLEAVED_WRITE_FILTERED_DATASET_DIMS, chunk_dims) >= 0),
            "Chunk size set");

    /* Add test filter to the pipeline */
    VRFY((set_dcpl_filter(plist_id) >= 0), "Filter set");

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
    start[0] = (hsize_t) mpi_rank;
    start[1] = 0;

    if (VERBOSE_MED) {
        HDprintf("Process %d is writing with count[ %llu, %llu ], stride[ %llu, %llu ], start[ %llu, %llu ], block size[ %llu, %llu ]\n",
                mpi_rank, count[0], count[1], stride[0], stride[1], start[0], start[1], block[0], block[1]);
        HDfflush(stdout);
    }

    /* Select hyperslab in the file */
    filespace = H5Dget_space(dset_id);
    VRFY((filespace >= 0), "File dataspace retrieval succeeded");

    VRFY((H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, stride, count, block) >= 0),
            "Hyperslab selection succeeded");

    /* Fill data buffer */
    data_size = sel_dims[0] * sel_dims[1] * sizeof(*data);
    correct_buf_size = dataset_dims[0] * dataset_dims[1] * sizeof(*correct_buf);

    data = (C_DATATYPE *) HDcalloc(1, data_size);
    VRFY((NULL != data), "HDcalloc succeeded");

    correct_buf = (C_DATATYPE *) HDcalloc(1, correct_buf_size);
    VRFY((NULL != correct_buf), "HDcalloc succeeded");

    for (i = 0; i < data_size / sizeof(*data); i++)
        data[i] = (C_DATATYPE) GEN_DATA(i);

    for (i = 0; i < correct_buf_size / sizeof(*correct_buf); i++)
        /* Add Column Index */
        correct_buf[i] =
                (C_DATATYPE) (
                                 (i % (hsize_t) INTERLEAVED_WRITE_FILTERED_DATASET_NCOLS)

                                 /* Add the Row Index */
                               + ((i % (hsize_t) (mpi_size * INTERLEAVED_WRITE_FILTERED_DATASET_NCOLS)) / (hsize_t) INTERLEAVED_WRITE_FILTERED_DATASET_NCOLS)

                                 /* Add the amount that gets added when a rank moves down to its next section vertically in the dataset */
                               + ((hsize_t) INTERLEAVED_WRITE_FILTERED_DATASET_NCOLS * (i / (hsize_t) (mpi_size * INTERLEAVED_WRITE_FILTERED_DATASET_NCOLS)))
                             );

    /* Create property list for collective dataset write */
    plist_id = H5Pcreate(H5P_DATASET_XFER);
    VRFY((plist_id >= 0), "DXPL creation succeeded");

    VRFY((H5Pset_dxpl_mpio(plist_id, H5FD_MPIO_COLLECTIVE) >= 0),
            "Set DXPL MPIO succeeded");

    VRFY((H5Dwrite(dset_id, HDF5_DATATYPE_NAME, memspace, filespace, plist_id, data) >= 0),
            "Dataset write succeeded");

    if (data) HDfree(data);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");

    /* Verify the correct data was written */
    read_buf = (C_DATATYPE *) HDcalloc(1, correct_buf_size);
    VRFY((NULL != read_buf), "HDcalloc succeeded");

    dset_id = H5Dopen2(file_id, "/" INTERLEAVED_WRITE_FILTERED_DATASET_NAME, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    VRFY((H5Dread(dset_id, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, plist_id, read_buf) >= 0),
            "Dataset read succeeded");

    VRFY((0 == HDmemcmp(read_buf, correct_buf, correct_buf_size)),
            "Data verification succeeded");

    if (correct_buf) HDfree(correct_buf);
    if (read_buf) HDfree(read_buf);

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
test_write_3d_filtered_dataset_no_overlap_separate_pages(void)
{
    C_DATATYPE *data = NULL;
    C_DATATYPE *read_buf = NULL;
    C_DATATYPE *correct_buf = NULL;
    hsize_t     dataset_dims[WRITE_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_DIMS];
    hsize_t     chunk_dims[WRITE_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_DIMS];
    hsize_t     sel_dims[WRITE_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_DIMS];
    hsize_t     start[WRITE_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_DIMS];
    hsize_t     stride[WRITE_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_DIMS];
    hsize_t     count[WRITE_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_DIMS];
    hsize_t     block[WRITE_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_DIMS];
    size_t      i, data_size, correct_buf_size;
    hid_t       file_id = -1, dset_id = -1, plist_id = -1;
    hid_t       filespace = -1, memspace = -1;

    if (MAINPROCESS) HDputs("Testing write to unshared filtered chunks on separate pages in 3D dataset");

    CHECK_CUR_FILTER_AVAIL();

    /* Set up file access property list with parallel I/O access */
    plist_id = H5Pcreate(H5P_FILE_ACCESS);
    VRFY((plist_id >= 0), "FAPL creation succeeded");

    VRFY((H5Pset_fapl_mpio(plist_id, comm, info) >= 0),
            "Set FAPL MPIO succeeded");

    VRFY((H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0),
            "Set libver bounds succeeded");

    file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, plist_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    VRFY((H5Pclose(plist_id) >= 0), "FAPL close succeeded");

    /* Create the dataspace for the dataset */
    dataset_dims[0] = (hsize_t) WRITE_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_NROWS;
    dataset_dims[1] = (hsize_t) WRITE_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_NCOLS;
    dataset_dims[2] = (hsize_t) WRITE_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DEPTH;
    chunk_dims[0] = (hsize_t) WRITE_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_CH_NROWS;
    chunk_dims[1] = (hsize_t) WRITE_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_CH_NCOLS;
    chunk_dims[2] = 1;
    sel_dims[0] = (hsize_t) WRITE_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_NROWS;
    sel_dims[1] = (hsize_t) WRITE_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_NCOLS;
    sel_dims[2] = 1;

    filespace = H5Screate_simple( WRITE_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_DIMS, dataset_dims, NULL);
    VRFY((filespace >= 0), "File dataspace creation succeeded");

    memspace = H5Screate_simple( WRITE_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_DIMS, sel_dims, NULL);
    VRFY((memspace >= 0), "Memory dataspace creation succeeded");

    /* Create chunked dataset */
    plist_id = H5Pcreate(H5P_DATASET_CREATE);
    VRFY((plist_id >= 0), "DCPL creation succeeded");

    VRFY((H5Pset_chunk(plist_id, WRITE_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_DIMS, chunk_dims) >= 0),
            "Chunk size set");

    /* Add test filter to the pipeline */
    VRFY((set_dcpl_filter(plist_id) >= 0), "Filter set");

    dset_id = H5Dcreate2(file_id, WRITE_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_NAME, HDF5_DATATYPE_NAME, filespace,
            H5P_DEFAULT, plist_id, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset creation succeeded");

    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

    /* Each process defines the dataset selection in memory and writes
     * it to the hyperslab in the file
     */
    count[0] = (hsize_t) WRITE_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_NROWS / (hsize_t) WRITE_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_CH_NROWS;
    count[1] = (hsize_t) WRITE_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_NCOLS / (hsize_t) WRITE_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_CH_NCOLS;
    count[2] = 1;
    stride[0] = (hsize_t) WRITE_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_CH_NROWS;
    stride[1] = (hsize_t) WRITE_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_CH_NCOLS;
    stride[2] = 1;
    block[0] = (hsize_t) WRITE_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_CH_NROWS;
    block[1] = (hsize_t) WRITE_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_CH_NCOLS;
    block[2] = 1;
    start[0] = 0;
    start[1] = 0;
    start[2] = (hsize_t) mpi_rank;

    if (VERBOSE_MED) {
        HDprintf("Process %d is writing with count[ %llu, %llu, %llu ], stride[ %llu, %llu, %llu ], start[ %llu, %llu, %llu ], block size[ %llu, %llu, %llu ]\n",
                mpi_rank, count[0], count[1], count[2], stride[0], stride[1], stride[2], start[0], start[1], start[2], block[0], block[1], block[2]);
        HDfflush(stdout);
    }

    /* Select hyperslab in the file */
    filespace = H5Dget_space(dset_id);
    VRFY((filespace >= 0), "File dataspace retrieval succeeded");

    VRFY((H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, stride, count, block) >= 0),
            "Hyperslab selection succeeded");

    /* Fill data buffer */
    data_size = sel_dims[0] * sel_dims[1] * sel_dims[2] * sizeof(*data);
    correct_buf_size = dataset_dims[0] * dataset_dims[1] * dataset_dims[2] * sizeof(*correct_buf);

    data = (C_DATATYPE *) HDcalloc(1, data_size);
    VRFY((NULL != data), "HDcalloc succeeded");

    correct_buf = (C_DATATYPE *) HDcalloc(1, correct_buf_size);
    VRFY((NULL != correct_buf), "HDcalloc succeeded");

    for (i = 0; i < data_size / sizeof(*data); i++)
        data[i] = (C_DATATYPE) GEN_DATA(i);

    for (i = 0; i < correct_buf_size / sizeof(*correct_buf); i++)
        correct_buf[i] = (C_DATATYPE) ((i % (hsize_t) mpi_size) + (i / (hsize_t) mpi_size));

    /* Create property list for collective dataset write */
    plist_id = H5Pcreate(H5P_DATASET_XFER);
    VRFY((plist_id >= 0), "DXPL creation succeeded");

    VRFY((H5Pset_dxpl_mpio(plist_id, H5FD_MPIO_COLLECTIVE) >= 0),
            "Set DXPL MPIO succeeded");

    VRFY((H5Dwrite(dset_id, HDF5_DATATYPE_NAME, memspace, filespace, plist_id, data) >= 0),
            "Dataset write succeeded");

    if (data) HDfree(data);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");

    /* Verify the correct data was written */
    read_buf = (C_DATATYPE *) HDcalloc(1, correct_buf_size);
    VRFY((NULL != read_buf), "HDcalloc succeeded");

    dset_id = H5Dopen2(file_id, "/" WRITE_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_NAME, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    VRFY((H5Dread(dset_id, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, plist_id, read_buf) >= 0),
            "Dataset read succeeded");

    VRFY((0 == HDmemcmp(read_buf, correct_buf, correct_buf_size)),
            "Data verification succeeded");

    if (correct_buf) HDfree(correct_buf);
    if (read_buf) HDfree(read_buf);

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
test_write_3d_filtered_dataset_no_overlap_same_pages(void)
{
    C_DATATYPE *data = NULL;
    C_DATATYPE *read_buf = NULL;
    C_DATATYPE *correct_buf = NULL;
    hsize_t     dataset_dims[WRITE_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_DIMS];
    hsize_t     chunk_dims[WRITE_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_DIMS];
    hsize_t     sel_dims[WRITE_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_DIMS];
    hsize_t     start[WRITE_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_DIMS];
    hsize_t     stride[WRITE_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_DIMS];
    hsize_t     count[WRITE_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_DIMS];
    hsize_t     block[WRITE_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_DIMS];
    size_t      i, data_size, correct_buf_size;
    hid_t       file_id, dset_id, plist_id;
    hid_t       filespace, memspace;

    if (MAINPROCESS) HDputs("Testing write to unshared filtered chunks on the same pages in 3D dataset");

    CHECK_CUR_FILTER_AVAIL();

    /* Set up file access property list with parallel I/O access */
    plist_id = H5Pcreate(H5P_FILE_ACCESS);
    VRFY((plist_id >= 0), "FAPL creation succeeded");

    VRFY((H5Pset_fapl_mpio(plist_id, comm, info) >= 0),
            "Set FAPL MPIO succeeded");

    VRFY((H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0),
            "Set libver bounds succeeded");

    file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, plist_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    VRFY((H5Pclose(plist_id) >= 0), "FAPL close succeeded");

    /* Create the dataspace for the dataset */
    dataset_dims[0] = (hsize_t) WRITE_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_NROWS;
    dataset_dims[1] = (hsize_t) WRITE_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_NCOLS;
    dataset_dims[2] = (hsize_t) WRITE_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DEPTH;
    chunk_dims[0] = (hsize_t) WRITE_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_CH_NROWS;
    chunk_dims[1] = (hsize_t) WRITE_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_CH_NCOLS;
    chunk_dims[2] = 1;
    sel_dims[0] = (hsize_t) WRITE_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_CH_NROWS;
    sel_dims[1] = (hsize_t) WRITE_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_NCOLS;
    sel_dims[2] = (hsize_t) WRITE_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DEPTH;

    filespace = H5Screate_simple(WRITE_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_DIMS, dataset_dims, NULL);
    VRFY((filespace >= 0), "File dataspace creation succeeded");

    memspace = H5Screate_simple(WRITE_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_DIMS, sel_dims, NULL);
    VRFY((memspace >= 0), "Memory dataspace creation succeeded");

    /* Create chunked dataset */
    plist_id = H5Pcreate(H5P_DATASET_CREATE);
    VRFY((plist_id >= 0), "DCPL creation succeeded");

    VRFY((H5Pset_chunk(plist_id, WRITE_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_DIMS, chunk_dims) >= 0),
            "Chunk size set");

    /* Add test filter to the pipeline */
    VRFY((set_dcpl_filter(plist_id) >= 0), "Filter set");

    dset_id = H5Dcreate2(file_id, WRITE_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_NAME, HDF5_DATATYPE_NAME, filespace,
            H5P_DEFAULT, plist_id, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset creation succeeded");

    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

    /* Each process defines the dataset selection in memory and writes
     * it to the hyperslab in the file
     */
    count[0] = 1;
    count[1] = (hsize_t) WRITE_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_NCOLS / (hsize_t) WRITE_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_CH_NCOLS;
    count[2] = (hsize_t) mpi_size;
    stride[0] = (hsize_t) WRITE_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_CH_NROWS;
    stride[1] = (hsize_t) WRITE_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_CH_NCOLS;
    stride[2] = 1;
    block[0] = (hsize_t) WRITE_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_CH_NROWS;
    block[1] = (hsize_t) WRITE_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_CH_NCOLS;
    block[2] = 1;
    start[0] = ((hsize_t) mpi_rank * (hsize_t) WRITE_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_CH_NROWS * count[0]);
    start[1] = 0;
    start[2] = 0;

    if (VERBOSE_MED) {
        HDprintf("Process %d is writing with count[ %llu, %llu, %llu ], stride[ %llu, %llu, %llu ], start[ %llu, %llu, %llu ], block size[ %llu, %llu, %llu ]\n",
                mpi_rank, count[0], count[1], count[2], stride[0], stride[1], stride[2], start[0], start[1], start[2], block[0], block[1], block[2]);
        HDfflush(stdout);
    }

    /* Select hyperslab in the file */
    filespace = H5Dget_space(dset_id);
    VRFY((filespace >= 0), "File dataspace retrieval succeeded");

    VRFY((H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, stride, count, block) >= 0),
            "Hyperslab selection succeeded");

    /* Fill data buffer */
    data_size = sel_dims[0] * sel_dims[1] * sel_dims[2] * sizeof(*data);
    correct_buf_size = dataset_dims[0] * dataset_dims[1] * dataset_dims[2] * sizeof(*correct_buf);

    data = (C_DATATYPE *) HDcalloc(1, data_size);
    VRFY((NULL != data), "HDcalloc succeeded");

    correct_buf = (C_DATATYPE *) HDcalloc(1, correct_buf_size);
    VRFY((NULL != correct_buf), "HDcalloc succeeded");

    for (i = 0; i < data_size / sizeof(*data); i++)
        data[i] = (C_DATATYPE) GEN_DATA(i);

    for (i = 0; i < correct_buf_size / sizeof(*correct_buf); i++)
        correct_buf[i] = (C_DATATYPE) (
                                         (i % (dataset_dims[0] * dataset_dims[1]))
                                       + (i / (dataset_dims[0] * dataset_dims[1]))
                                      );

    /* Create property list for collective dataset write */
    plist_id = H5Pcreate(H5P_DATASET_XFER);
    VRFY((plist_id >= 0), "DXPL creation succeeded");

    VRFY((H5Pset_dxpl_mpio(plist_id, H5FD_MPIO_COLLECTIVE) >= 0),
            "Set DXPL MPIO succeeded");

    VRFY((H5Dwrite(dset_id, HDF5_DATATYPE_NAME, memspace, filespace, plist_id, data) >= 0),
            "Dataset write succeeded");

    if (data) HDfree(data);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");

    /* Verify the correct data was written */
    read_buf = (C_DATATYPE *) HDcalloc(1, correct_buf_size);
    VRFY((NULL != read_buf), "HDcalloc succeeded");

    dset_id = H5Dopen2(file_id, "/" WRITE_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_NAME, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    VRFY((H5Dread(dset_id, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, plist_id, read_buf) >= 0),
            "Dataset read succeeded");

    VRFY((0 == HDmemcmp(read_buf, correct_buf, correct_buf_size)),
            "Data verification succeeded");

    if (correct_buf) HDfree(correct_buf);
    if (read_buf) HDfree(read_buf);

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
test_write_3d_filtered_dataset_overlap(void)
{
    C_DATATYPE *data = NULL;
    C_DATATYPE *read_buf = NULL;
    C_DATATYPE *correct_buf = NULL;
    hsize_t     dataset_dims[WRITE_SHARED_FILTERED_CHUNKS_3D_DATASET_DIMS];
    hsize_t     chunk_dims[WRITE_SHARED_FILTERED_CHUNKS_3D_DATASET_DIMS];
    hsize_t     sel_dims[WRITE_SHARED_FILTERED_CHUNKS_3D_DATASET_DIMS];
    hsize_t     start[WRITE_SHARED_FILTERED_CHUNKS_3D_DATASET_DIMS];
    hsize_t     stride[WRITE_SHARED_FILTERED_CHUNKS_3D_DATASET_DIMS];
    hsize_t     count[WRITE_SHARED_FILTERED_CHUNKS_3D_DATASET_DIMS];
    hsize_t     block[WRITE_SHARED_FILTERED_CHUNKS_3D_DATASET_DIMS];
    size_t      i, data_size, correct_buf_size;
    hid_t       file_id = -1, dset_id = -1, plist_id = -1;
    hid_t       filespace = -1, memspace = -1;

    if (MAINPROCESS) HDputs("Testing write to shared filtered chunks in 3D dataset");

    CHECK_CUR_FILTER_AVAIL();

    /* Set up file access property list with parallel I/O access */
    plist_id = H5Pcreate(H5P_FILE_ACCESS);
    VRFY((plist_id >= 0), "FAPL creation succeeded");

    VRFY((H5Pset_fapl_mpio(plist_id, comm, info) >= 0),
            "Set FAPL MPIO succeeded");

    VRFY((H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0),
            "Set libver bounds succeeded");

    file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, plist_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    VRFY((H5Pclose(plist_id) >= 0), "FAPL close succeeded");

    /* Create the dataspace for the dataset */
    dataset_dims[0] = (hsize_t) WRITE_SHARED_FILTERED_CHUNKS_3D_NROWS;
    dataset_dims[1] = (hsize_t) WRITE_SHARED_FILTERED_CHUNKS_3D_NCOLS;
    dataset_dims[2] = (hsize_t) WRITE_SHARED_FILTERED_CHUNKS_3D_DEPTH;
    chunk_dims[0] = (hsize_t) WRITE_SHARED_FILTERED_CHUNKS_3D_CH_NROWS;
    chunk_dims[1] = (hsize_t) WRITE_SHARED_FILTERED_CHUNKS_3D_CH_NCOLS;
    chunk_dims[2] = 1;
    sel_dims[0] = (hsize_t) (WRITE_SHARED_FILTERED_CHUNKS_3D_NROWS / mpi_size);
    sel_dims[1] = (hsize_t) WRITE_SHARED_FILTERED_CHUNKS_3D_NCOLS;
    sel_dims[2] = (hsize_t) WRITE_SHARED_FILTERED_CHUNKS_3D_DEPTH;

    filespace = H5Screate_simple(WRITE_SHARED_FILTERED_CHUNKS_3D_DATASET_DIMS, dataset_dims, NULL);
    VRFY((filespace >= 0), "File dataspace creation succeeded");

    memspace = H5Screate_simple(WRITE_SHARED_FILTERED_CHUNKS_3D_DATASET_DIMS, sel_dims, NULL);
    VRFY((memspace >= 0), "Memory dataspace creation succeeded");

    /* Create chunked dataset */
    plist_id = H5Pcreate(H5P_DATASET_CREATE);
    VRFY((plist_id >= 0), "DCPL creation succeeded");

    VRFY((H5Pset_chunk(plist_id, WRITE_SHARED_FILTERED_CHUNKS_3D_DATASET_DIMS, chunk_dims) >= 0),
            "Chunk size set");

    /* Add test filter to the pipeline */
    VRFY((set_dcpl_filter(plist_id) >= 0), "Filter set");

    dset_id = H5Dcreate2(file_id, WRITE_SHARED_FILTERED_CHUNKS_3D_DATASET_NAME, HDF5_DATATYPE_NAME, filespace,
            H5P_DEFAULT, plist_id, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset creation succeeded");

    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

    /* Each process defines the dataset selection in memory and writes
     * it to the hyperslab in the file
     */
    count[0] = (hsize_t) (WRITE_SHARED_FILTERED_CHUNKS_3D_NROWS / WRITE_SHARED_FILTERED_CHUNKS_3D_CH_NROWS);
    count[1] = (hsize_t) (WRITE_SHARED_FILTERED_CHUNKS_3D_NCOLS / WRITE_SHARED_FILTERED_CHUNKS_3D_CH_NCOLS);
    count[2] = (hsize_t) WRITE_SHARED_FILTERED_CHUNKS_3D_DEPTH;
    stride[0] = (hsize_t) WRITE_SHARED_FILTERED_CHUNKS_3D_CH_NROWS;
    stride[1] = (hsize_t) WRITE_SHARED_FILTERED_CHUNKS_3D_CH_NCOLS;
    stride[2] = 1;
    block[0] = 1;
    block[1] = (hsize_t) WRITE_SHARED_FILTERED_CHUNKS_3D_CH_NCOLS;
    block[2] = 1;
    start[0] = (hsize_t) mpi_rank;
    start[1] = 0;
    start[2] = 0;

    if (VERBOSE_MED) {
        HDprintf("Process %d is writing with count[ %llu, %llu, %llu ], stride[ %llu, %llu, %llu ], start[ %llu, %llu, %llu ], block size[ %llu, %llu, %llu ]\n",
                mpi_rank, count[0], count[1], count[2], stride[0], stride[1], stride[2], start[0], start[1], start[2], block[0], block[1], block[2]);
        HDfflush(stdout);
    }

    /* Select hyperslab in the file */
    filespace = H5Dget_space(dset_id);
    VRFY((filespace >= 0), "File dataspace retrieval succeeded");

    VRFY((H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, stride,
                    count, block) >= 0), "Hyperslab selection succeeded");

    /* Fill data buffer */
    data_size = sel_dims[0] * sel_dims[1] * sel_dims[2] * sizeof(*data);
    correct_buf_size = dataset_dims[0] * dataset_dims[1] * dataset_dims[2] * sizeof(*correct_buf);

    data = (C_DATATYPE *) HDcalloc(1, data_size);
    VRFY((NULL != data), "HDcalloc succeeded");

    correct_buf = (C_DATATYPE *) HDcalloc(1, correct_buf_size);
    VRFY((NULL != correct_buf), "HDcalloc succeeded");

    for (i = 0; i < data_size / sizeof(*data); i++)
        data[i] = (C_DATATYPE) GEN_DATA(i);

    for (i = 0; i < correct_buf_size / sizeof(*correct_buf); i++)
        /* Add the Column Index */
        correct_buf[i] =
                (C_DATATYPE) (
                                 (i % (hsize_t) (WRITE_SHARED_FILTERED_CHUNKS_3D_DEPTH * WRITE_SHARED_FILTERED_CHUNKS_3D_NCOLS))

                                 /* Add the Row Index */
                               + ((i % (hsize_t) (mpi_size * WRITE_SHARED_FILTERED_CHUNKS_3D_DEPTH * WRITE_SHARED_FILTERED_CHUNKS_3D_NCOLS))
                                 / (hsize_t) (WRITE_SHARED_FILTERED_CHUNKS_3D_DEPTH * WRITE_SHARED_FILTERED_CHUNKS_3D_NCOLS))

                                 /* Add the amount that gets added when a rank moves down to its next section vertically in the dataset */
                               + ((hsize_t) (WRITE_SHARED_FILTERED_CHUNKS_3D_DEPTH * WRITE_SHARED_FILTERED_CHUNKS_3D_NCOLS)
                                 * (i / (hsize_t) (mpi_size * WRITE_SHARED_FILTERED_CHUNKS_3D_DEPTH * WRITE_SHARED_FILTERED_CHUNKS_3D_NCOLS)))
                             );

    /* Create property list for collective dataset write */
    plist_id = H5Pcreate(H5P_DATASET_XFER);
    VRFY((plist_id >= 0), "DXPL creation succeeded");

    VRFY((H5Pset_dxpl_mpio(plist_id, H5FD_MPIO_COLLECTIVE) >= 0),
            "Set DXPL MPIO succeeded");

    VRFY((H5Dwrite(dset_id, HDF5_DATATYPE_NAME, memspace, filespace, plist_id, data) >= 0),
            "Dataset write succeeded");

    if (data) HDfree(data);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");

    /* Verify the correct data was written */
    read_buf = (C_DATATYPE *) HDcalloc(1, correct_buf_size);
    VRFY((NULL != read_buf), "HDcalloc succeeded");

    dset_id = H5Dopen2(file_id, "/" WRITE_SHARED_FILTERED_CHUNKS_3D_DATASET_NAME, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    VRFY((H5Dread(dset_id, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, plist_id, read_buf) >= 0),
            "Dataset read succeeded");

    VRFY((0 == HDmemcmp(read_buf, correct_buf, correct_buf_size)),
            "Data verification succeeded");

    if (correct_buf) HDfree(correct_buf);
    if (read_buf) HDfree(read_buf);

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
static void
test_write_cmpd_filtered_dataset_no_conversion_unshared(void)
{
    COMPOUND_C_DATATYPE *data = NULL;
    COMPOUND_C_DATATYPE *read_buf = NULL;
    COMPOUND_C_DATATYPE *correct_buf = NULL;
    hsize_t              dataset_dims[WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t              chunk_dims[WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t              sel_dims[WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t              start[WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t              stride[WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t              count[WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t              block[WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_DIMS];
    size_t               i, correct_buf_size;
    hid_t                file_id = -1, dset_id = -1, plist_id = -1, memtype = -1;
    hid_t                filespace = -1, memspace = -1;

    if (MAINPROCESS) HDputs("Testing write to unshared filtered chunks in Compound Datatype dataset without Datatype conversion");

    CHECK_CUR_FILTER_AVAIL();

    /* Set up file access property list with parallel I/O access */
    plist_id = H5Pcreate(H5P_FILE_ACCESS);
    VRFY((plist_id >= 0), "FAPL creation succeeded");

    VRFY((H5Pset_fapl_mpio(plist_id, comm, info) >= 0),
            "Set FAPL MPIO succeeded");

    VRFY((H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0),
            "Set libver bounds succeeded");

    file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, plist_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    VRFY((H5Pclose(plist_id) >= 0), "FAPL close succeeded");

    /* Create the dataspace for the dataset */
    dataset_dims[0] = WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_NROWS;
    dataset_dims[1] = (hsize_t) WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_NCOLS;
    chunk_dims[0] = WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_CH_NROWS;
    chunk_dims[1] = WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_CH_NCOLS;
    sel_dims[0] = WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_CH_NROWS;
    sel_dims[1] = (hsize_t) WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_ENTRIES_PER_PROC;

    filespace = H5Screate_simple(WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_DIMS, dataset_dims, NULL);
    VRFY((filespace >= 0), "File dataspace creation succeeded");

    memspace = H5Screate_simple(WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_DIMS, sel_dims, NULL);
    VRFY((memspace >= 0), "Memory dataspace creation succeeded");

    /* Create chunked dataset */
    plist_id = H5Pcreate(H5P_DATASET_CREATE);
    VRFY((plist_id >= 0), "DCPL creation succeeded");

    VRFY((H5Pset_chunk(plist_id, WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_DIMS, chunk_dims) >= 0),
            "Chunk size set");

    /* Add test filter to the pipeline */
    VRFY((set_dcpl_filter(plist_id) >= 0), "Filter set");

    /* Create the compound type for memory. */
    memtype = H5Tcreate(H5T_COMPOUND, sizeof(COMPOUND_C_DATATYPE));
    VRFY((memtype >= 0), "Datatype creation succeeded");

    VRFY((H5Tinsert(memtype, "ShortData", HOFFSET(COMPOUND_C_DATATYPE, field1), H5T_NATIVE_SHORT) >= 0),
            "Datatype insertion succeeded");
    VRFY((H5Tinsert(memtype, "IntData", HOFFSET(COMPOUND_C_DATATYPE, field2), H5T_NATIVE_INT) >= 0),
            "Datatype insertion succeeded");
    VRFY((H5Tinsert(memtype, "LongData", HOFFSET(COMPOUND_C_DATATYPE, field3), H5T_NATIVE_LONG) >= 0),
            "Datatype insertion succeeded");

    dset_id = H5Dcreate2(file_id, WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_NAME, memtype, filespace,
            H5P_DEFAULT, plist_id, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset creation succeeded");

    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

    /* Each process defines the dataset selection in memory and writes
     * it to the hyperslab in the file
     */
    count[0] = 1;
    count[1] = (hsize_t) WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_ENTRIES_PER_PROC;
    stride[0] = WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_CH_NROWS;
    stride[1] = WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_CH_NCOLS;
    block[0] = WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_CH_NROWS;
    block[1] = WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_CH_NCOLS;
    start[0] = 0;
    start[1] = ((hsize_t) mpi_rank * WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_CH_NCOLS);

    if (VERBOSE_MED) {
        HDprintf("Process %d is writing with count[ %llu, %llu ], stride[ %llu, %llu ], start[ %llu, %llu ], block size[ %llu, %llu ]\n",
                mpi_rank, count[0], count[1], stride[0], stride[1], start[0], start[1], block[0], block[1]);
        HDfflush(stdout);
    }

    /* Select hyperslab in the file */
    filespace = H5Dget_space(dset_id);
    VRFY((filespace >= 0), "File dataspace retrieval succeeded");

    VRFY((H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, stride, count, block) >= 0),
            "Hyperslab selection succeeded");

    data = (COMPOUND_C_DATATYPE *) HDcalloc(1, (hsize_t) WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_ENTRIES_PER_PROC * sizeof(*data));
    VRFY((NULL != data), "HDcalloc succeeded");

    correct_buf_size = dataset_dims[0] * dataset_dims[1] * sizeof(COMPOUND_C_DATATYPE);

    correct_buf = (COMPOUND_C_DATATYPE *) HDcalloc(1, correct_buf_size);
    VRFY((NULL != correct_buf), "HDcalloc succeeded");

    /* Fill data buffer */
    for (i = 0; i < (hsize_t) WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_ENTRIES_PER_PROC; i++) {
        data[i].field1 = (short) GEN_DATA(i);
        data[i].field2 = (int) GEN_DATA(i);
        data[i].field3 = (long) GEN_DATA(i);
    }

    for (i = 0; i < correct_buf_size / sizeof(*correct_buf); i++) {
        correct_buf[i].field1 = (short) (
                                            (i % dataset_dims[1])
                                          + (i / dataset_dims[1])
                                        );

        correct_buf[i].field2 = (int) (
                                          (i % dataset_dims[1])
                                        + (i / dataset_dims[1])
                                      );

        correct_buf[i].field3 = (long) (
                                           (i % dataset_dims[1])
                                         + (i / dataset_dims[1])
                                       );
    }

    /* Create property list for collective dataset write */
    plist_id = H5Pcreate(H5P_DATASET_XFER);
    VRFY((plist_id >= 0), "DXPL creation succeeded");

    VRFY((H5Pset_dxpl_mpio(plist_id, H5FD_MPIO_COLLECTIVE) >= 0),
            "Set DXPL MPIO succeeded");

    VRFY((H5Dwrite(dset_id, memtype, memspace, filespace, plist_id, data) >= 0),
            "Dataset write succeeded");

    if (data) HDfree(data);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");

    /* Verify the correct data was written */
    read_buf = (COMPOUND_C_DATATYPE *) HDcalloc(1, correct_buf_size);
    VRFY((NULL != read_buf), "HDcalloc succeeded");

    dset_id = H5Dopen2(file_id, "/" WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_NAME, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    VRFY((H5Dread(dset_id, memtype, H5S_ALL, H5S_ALL, plist_id, read_buf) >= 0),
            "Dataset read succeeded");

    VRFY((0 == HDmemcmp(read_buf, correct_buf, correct_buf_size)),
            "Data verification succeeded");

    if (correct_buf) HDfree(correct_buf);
    if (read_buf) HDfree(read_buf);

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
static void
test_write_cmpd_filtered_dataset_no_conversion_shared(void)
{
    COMPOUND_C_DATATYPE *data = NULL;
    COMPOUND_C_DATATYPE *read_buf = NULL;
    COMPOUND_C_DATATYPE *correct_buf = NULL;
    hsize_t              dataset_dims[WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t              chunk_dims[WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t              sel_dims[WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t              start[WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t              stride[WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t              count[WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t              block[WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_DIMS];
    size_t               i, correct_buf_size;
    hid_t                file_id, dset_id, plist_id, memtype;
    hid_t                filespace, memspace;

    if (MAINPROCESS) HDputs("Testing write to shared filtered chunks in Compound Datatype dataset without Datatype conversion");

    CHECK_CUR_FILTER_AVAIL();

    /* Set up file access property list with parallel I/O access */
    plist_id = H5Pcreate(H5P_FILE_ACCESS);
    VRFY((plist_id >= 0), "FAPL creation succeeded");

    VRFY((H5Pset_fapl_mpio(plist_id, comm, info) >= 0),
            "Set FAPL MPIO succeeded");

    VRFY((H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0),
            "Set libver bounds succeeded");

    file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, plist_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    VRFY((H5Pclose(plist_id) >= 0), "FAPL close succeeded");

    /* Create the dataspace for the dataset */
    dataset_dims[0] = (hsize_t) WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_NROWS;
    dataset_dims[1] = (hsize_t) WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_NCOLS;
    chunk_dims[0] = (hsize_t) WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_CH_NROWS;
    chunk_dims[1] = WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_CH_NCOLS;
    sel_dims[0] = (hsize_t) WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_CH_NROWS / (hsize_t) mpi_size;
    sel_dims[1] = (hsize_t) WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_ENTRIES_PER_PROC;

    filespace = H5Screate_simple(WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_DIMS, dataset_dims, NULL);
    VRFY((filespace >= 0), "File dataspace creation succeeded");

    memspace = H5Screate_simple(WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_DIMS, sel_dims, NULL);
    VRFY((memspace >= 0), "Memory dataspace creation succeeded");

    /* Create chunked dataset */
    plist_id = H5Pcreate(H5P_DATASET_CREATE);
    VRFY((plist_id >= 0), "DCPL creation succeeded");

    VRFY((H5Pset_chunk(plist_id, WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_DIMS, chunk_dims) >= 0),
            "Chunk size set");

    /* Add test filter to the pipeline */
    VRFY((set_dcpl_filter(plist_id) >= 0), "Filter set");

    /* Create the compound type for memory. */
    memtype = H5Tcreate(H5T_COMPOUND, sizeof(COMPOUND_C_DATATYPE));
    VRFY((memtype >= 0), "Datatype creation succeeded");

    VRFY((H5Tinsert(memtype, "ShortData", HOFFSET(COMPOUND_C_DATATYPE, field1), H5T_NATIVE_SHORT) >= 0),
            "Datatype insertion succeeded");
    VRFY((H5Tinsert(memtype, "IntData", HOFFSET(COMPOUND_C_DATATYPE, field2), H5T_NATIVE_INT) >= 0),
            "Datatype insertion succeeded");
    VRFY((H5Tinsert(memtype, "LongData", HOFFSET(COMPOUND_C_DATATYPE, field3), H5T_NATIVE_LONG) >= 0),
            "Datatype insertion succeeded");

    dset_id = H5Dcreate2(file_id, WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_NAME, memtype, filespace,
            H5P_DEFAULT, plist_id, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset creation succeeded");

    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

    /* Each process defines the dataset selection in memory and writes
     * it to the hyperslab in the file
     */
    count[0] = 1;
    count[1] = (hsize_t) WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_ENTRIES_PER_PROC;
    stride[0] = (hsize_t) WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_CH_NROWS;
    stride[1] = WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_CH_NCOLS;
    block[0] = (hsize_t) WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_CH_NROWS / (hsize_t) mpi_size;
    block[1] = WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_CH_NCOLS;
    start[0] = (hsize_t) mpi_rank;
    start[1] = 0;

    if (VERBOSE_MED) {
        HDprintf("Process %d is writing with count[ %llu, %llu ], stride[ %llu, %llu ], start[ %llu, %llu ], block size[ %llu, %llu ]\n",
                mpi_rank, count[0], count[1], stride[0], stride[1], start[0], start[1], block[0], block[1]);
        HDfflush(stdout);
    }

    /* Select hyperslab in the file */
    filespace = H5Dget_space(dset_id);
    VRFY((filespace >= 0), "File dataspace retrieval succeeded");

    VRFY((H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, stride, count, block) >= 0),
            "Hyperslab selection succeeded");

    data = (COMPOUND_C_DATATYPE *) HDcalloc(1, (hsize_t) WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_ENTRIES_PER_PROC * sizeof(*data));
    VRFY((NULL != data), "HDcalloc succeeded");

    correct_buf_size = dataset_dims[0] * dataset_dims[1] * sizeof(COMPOUND_C_DATATYPE);

    correct_buf = (COMPOUND_C_DATATYPE *) HDcalloc(1, correct_buf_size);
    VRFY((NULL != correct_buf), "HDcalloc succeeded");

    /* Fill data buffer */
    for (i = 0; i < (hsize_t) WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_ENTRIES_PER_PROC; i++) {
        data[i].field1 = (short) GEN_DATA(i);
        data[i].field2 = (int) GEN_DATA(i);
        data[i].field3 = (long) GEN_DATA(i);
    }

    for (i = 0; i < correct_buf_size / sizeof(*correct_buf); i++) {
        correct_buf[i].field1 = (short) (
                                            (dataset_dims[1] * (i / ((hsize_t) mpi_size * dataset_dims[1])))
                                          + (i % dataset_dims[1])
                                          + (((i % ((hsize_t) mpi_size * dataset_dims[1])) / dataset_dims[1]) % dataset_dims[1])
                                        );

        correct_buf[i].field2 = (int) (
                                          (dataset_dims[1] * (i / ((hsize_t) mpi_size * dataset_dims[1])))
                                        + (i % dataset_dims[1])
                                        + (((i % ((hsize_t) mpi_size * dataset_dims[1])) / dataset_dims[1]) % dataset_dims[1])
                                      );

        correct_buf[i].field3 = (long) (
                                           (dataset_dims[1] * (i / ((hsize_t) mpi_size * dataset_dims[1])))
                                         + (i % dataset_dims[1])
                                         + (((i % ((hsize_t) mpi_size * dataset_dims[1])) / dataset_dims[1]) % dataset_dims[1])
                                       );
    }

    /* Create property list for collective dataset write */
    plist_id = H5Pcreate(H5P_DATASET_XFER);
    VRFY((plist_id >= 0), "DXPL creation succeeded");

    VRFY((H5Pset_dxpl_mpio(plist_id, H5FD_MPIO_COLLECTIVE) >= 0),
            "Set DXPL MPIO succeeded");

    VRFY((H5Dwrite(dset_id, memtype, memspace, filespace, plist_id, data) >= 0),
            "Dataset write succeeded");

    if (data) HDfree(data);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");

    /* Verify the correct data was written */
    read_buf = (COMPOUND_C_DATATYPE *) HDcalloc(1, correct_buf_size);
    VRFY((NULL != read_buf), "HDcalloc succeeded");

    dset_id = H5Dopen2(file_id, "/" WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_NAME, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    VRFY((H5Dread(dset_id, memtype, H5S_ALL, H5S_ALL, plist_id, read_buf) >= 0),
            "Dataset read succeeded");

    VRFY((0 == HDmemcmp(read_buf, correct_buf, correct_buf_size)),
            "Data verification succeeded");

    if (correct_buf) HDfree(correct_buf);
    if (read_buf) HDfree(read_buf);

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
 * NOTE: This test currently should fail because the
 * datatype conversion causes the parallel library to
 * break to independent I/O and this isn't allowed when
 * there are filters in the pipeline.
 *
 * Programmer: Jordan Henderson
 *             02/07/2017
 */
static void
test_write_cmpd_filtered_dataset_type_conversion_unshared(void)
{
    COMPOUND_C_DATATYPE *data = NULL;
    COMPOUND_C_DATATYPE *read_buf = NULL;
    COMPOUND_C_DATATYPE *correct_buf = NULL;
    hsize_t              dataset_dims[WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t              chunk_dims[WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t              sel_dims[WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t              start[WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t              stride[WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t              count[WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t              block[WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_DIMS];
    size_t               i, correct_buf_size;
    hid_t                file_id = -1, dset_id = -1, plist_id = -1, filetype = -1, memtype = -1;
    hid_t                filespace = -1, memspace = -1;

    if (MAINPROCESS) HDputs("Testing write to unshared filtered chunks in Compound Datatype dataset with Datatype conversion");

    CHECK_CUR_FILTER_AVAIL();

    /* Set up file access property list with parallel I/O access */
    plist_id = H5Pcreate(H5P_FILE_ACCESS);
    VRFY((plist_id >= 0), "FAPL creation succeeded");

    VRFY((H5Pset_fapl_mpio(plist_id, comm, info) >= 0),
            "Set FAPL MPIO succeeded");

    VRFY((H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0),
            "Set libver bounds succeeded");

    file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, plist_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    VRFY((H5Pclose(plist_id) >= 0), "FAPL close succeeded");

    /* Create the dataspace for the dataset */
    dataset_dims[0] = WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_NROWS;
    dataset_dims[1] = (hsize_t) WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_NCOLS;
    chunk_dims[0] = WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_CH_NROWS;
    chunk_dims[1] = WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_CH_NCOLS;
    sel_dims[0] = WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_CH_NROWS;
    sel_dims[1] = (hsize_t) WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_ENTRIES_PER_PROC;

    filespace = H5Screate_simple(WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_DIMS, dataset_dims, NULL);
    VRFY((filespace >= 0), "File dataspace creation succeeded");

    memspace = H5Screate_simple(WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_DIMS, sel_dims, NULL);
    VRFY((memspace >= 0), "Memory dataspace creation succeeded");

    /* Create chunked dataset */
    plist_id = H5Pcreate(H5P_DATASET_CREATE);
    VRFY((plist_id >= 0), "DCPL creation succeeded");

    VRFY((H5Pset_chunk(plist_id, WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_DIMS, chunk_dims) >= 0),
            "Chunk size set");

    /* Add test filter to the pipeline */
    VRFY((set_dcpl_filter(plist_id) >= 0), "Filter set");

    /* Create the compound type for memory. */
    memtype = H5Tcreate(H5T_COMPOUND, sizeof(COMPOUND_C_DATATYPE));
    VRFY((memtype >= 0), "Datatype creation succeeded");

    VRFY((H5Tinsert(memtype, "ShortData", HOFFSET(COMPOUND_C_DATATYPE, field1), H5T_NATIVE_SHORT) >= 0),
            "Datatype insertion succeeded");
    VRFY((H5Tinsert(memtype, "IntData", HOFFSET(COMPOUND_C_DATATYPE, field2), H5T_NATIVE_INT) >= 0),
            "Datatype insertion succeeded");
    VRFY((H5Tinsert(memtype, "LongData", HOFFSET(COMPOUND_C_DATATYPE, field3), H5T_NATIVE_LONG) >= 0),
            "Datatype insertion succeeded");

    /* Create the compound type for file. */
    filetype = H5Tcreate(H5T_COMPOUND, 32);
    VRFY((filetype >= 0), "Datatype creation succeeded");

    VRFY((H5Tinsert(filetype, "ShortData", 0, H5T_STD_I64BE) >= 0),
            "Datatype insertion succeeded");
    VRFY((H5Tinsert(filetype, "IntData", 8, H5T_STD_I64BE) >= 0),
            "Datatype insertion succeeded");
    VRFY((H5Tinsert(filetype, "LongData", 16, H5T_STD_I64BE) >= 0),
            "Datatype insertion succeeded");

    dset_id = H5Dcreate2(file_id, WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_NAME, filetype, filespace,
                    H5P_DEFAULT, plist_id, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset creation succeeded");

    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

    /* Each process defines the dataset selection in memory and writes
     * it to the hyperslab in the file
     */
    count[0] = 1;
    count[1] = (hsize_t) WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_ENTRIES_PER_PROC;
    stride[0] = WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_CH_NROWS;
    stride[1] = WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_CH_NCOLS;
    block[0] = WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_CH_NROWS;
    block[1] = WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_CH_NCOLS;
    start[0] = 0;
    start[1] = ((hsize_t) mpi_rank * WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_CH_NCOLS);

    if (VERBOSE_MED) {
        HDprintf("Process %d is writing with count[ %llu, %llu ], stride[ %llu, %llu ], start[ %llu, %llu ], block size[ %llu, %llu ]\n",
                mpi_rank, count[0], count[1], stride[0], stride[1], start[0], start[1], block[0], block[1]);
        HDfflush(stdout);
    }

    /* Select hyperslab in the file */
    filespace = H5Dget_space(dset_id);
    VRFY((filespace >= 0), "File dataspace retrieval succeeded");

    VRFY((H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, stride, count, block) >= 0),
            "Hyperslab selection succeeded");

    data = (COMPOUND_C_DATATYPE *) HDcalloc(1, (hsize_t) WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_ENTRIES_PER_PROC * sizeof(*data));
    VRFY((NULL != data), "HDcalloc succeeded");

    correct_buf_size = dataset_dims[0] * dataset_dims[1] * sizeof(COMPOUND_C_DATATYPE);

    correct_buf = (COMPOUND_C_DATATYPE *) HDcalloc(1, correct_buf_size);
    VRFY((NULL != correct_buf), "HDcalloc succeeded");

    /* Fill data buffer */
    for (i = 0; i < (hsize_t) WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_ENTRIES_PER_PROC; i++) {
        data[i].field1 = (short) GEN_DATA(i);
        data[i].field2 = (int) GEN_DATA(i);
        data[i].field3 = (long) GEN_DATA(i);
    }

    /* Create property list for collective dataset write */
    plist_id = H5Pcreate(H5P_DATASET_XFER);
    VRFY((plist_id >= 0), "DXPL creation succeeded");

    VRFY((H5Pset_dxpl_mpio(plist_id, H5FD_MPIO_COLLECTIVE) >= 0),
            "Set DXPL MPIO succeeded");

    /* Ensure that this test currently fails since type conversions break collective mode */
    H5E_BEGIN_TRY {
        VRFY((H5Dwrite(dset_id, memtype, memspace, filespace, plist_id, data) < 0),
                "Dataset write succeeded");
    } H5E_END_TRY;

    if (data) HDfree(data);

    /* Verify that no data was written */
    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");

    read_buf = (COMPOUND_C_DATATYPE *) HDcalloc(1, correct_buf_size);
    VRFY((NULL != read_buf), "HDcalloc succeeded");

    dset_id = H5Dopen2(file_id, "/" WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_NAME, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    VRFY((H5Dread(dset_id, memtype, H5S_ALL, H5S_ALL, plist_id, read_buf) >= 0),
            "Dataset read succeeded");

    VRFY((0 == HDmemcmp(read_buf, correct_buf, correct_buf_size)),
            "Data verification succeeded");

    if (correct_buf) HDfree(correct_buf);
    if (read_buf) HDfree(read_buf);

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
 * NOTE: This test currently should fail because the
 * datatype conversion causes the parallel library to
 * break to independent I/O and this isn't allowed when
 * there are filters in the pipeline.
 *
 * Programmer: Jordan Henderson
 *             02/10/2017
 */
static void
test_write_cmpd_filtered_dataset_type_conversion_shared(void)
{
    COMPOUND_C_DATATYPE *data = NULL;
    COMPOUND_C_DATATYPE *read_buf = NULL;
    COMPOUND_C_DATATYPE *correct_buf = NULL;
    hsize_t              dataset_dims[WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t              chunk_dims[WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t              sel_dims[WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t              start[WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t              stride[WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t              count[WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t              block[WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_DIMS];
    size_t               i, correct_buf_size;
    hid_t                file_id, dset_id, plist_id, filetype, memtype;
    hid_t                filespace, memspace;

    if (MAINPROCESS) HDputs("Testing write to shared filtered chunks in Compound Datatype dataset with Datatype conversion");

    CHECK_CUR_FILTER_AVAIL();

    /* Set up file access property list with parallel I/O access */
    plist_id = H5Pcreate(H5P_FILE_ACCESS);
    VRFY((plist_id >= 0), "FAPL creation succeeded");

    VRFY((H5Pset_fapl_mpio(plist_id, comm, info) >= 0),
            "Set FAPL MPIO succeeded");

    VRFY((H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0),
            "Set libver bounds succeeded");

    file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, plist_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    VRFY((H5Pclose(plist_id) >= 0), "FAPL close succeeded");

    /* Create the dataspace for the dataset */
    dataset_dims[0] = (hsize_t) WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_NROWS;
    dataset_dims[1] = (hsize_t) WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_NCOLS;
    chunk_dims[0] = (hsize_t) WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_CH_NROWS;
    chunk_dims[1] = WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_CH_NCOLS;
    sel_dims[0] = (hsize_t) WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_CH_NROWS / (hsize_t) mpi_size;
    sel_dims[1] = (hsize_t) WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_ENTRIES_PER_PROC;

    filespace = H5Screate_simple(WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_DIMS, dataset_dims, NULL);
    VRFY((filespace >= 0), "File dataspace creation succeeded");

    memspace = H5Screate_simple(WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_DIMS, sel_dims, NULL);
    VRFY((memspace >= 0), "Memory dataspace creation succeeded");

    /* Create chunked dataset */
    plist_id = H5Pcreate(H5P_DATASET_CREATE);
    VRFY((plist_id >= 0), "DCPL creation succeeded");

    VRFY((H5Pset_chunk(plist_id, WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_DIMS, chunk_dims) >= 0),
            "Chunk size set");

    /* Add test filter to the pipeline */
    VRFY((set_dcpl_filter(plist_id) >= 0), "Filter set");

    /* Create the compound type for memory. */
    memtype = H5Tcreate(H5T_COMPOUND, sizeof(COMPOUND_C_DATATYPE));
    VRFY((memtype >= 0), "Datatype creation succeeded");

    VRFY((H5Tinsert(memtype, "ShortData", HOFFSET(COMPOUND_C_DATATYPE, field1), H5T_NATIVE_SHORT) >= 0),
            "Datatype insertion succeeded");
    VRFY((H5Tinsert(memtype, "IntData", HOFFSET(COMPOUND_C_DATATYPE, field2), H5T_NATIVE_INT) >= 0),
            "Datatype insertion succeeded");
    VRFY((H5Tinsert(memtype, "LongData", HOFFSET(COMPOUND_C_DATATYPE, field3), H5T_NATIVE_LONG) >= 0),
            "Datatype insertion succeeded");

    /* Create the compound type for file. */
    filetype = H5Tcreate(H5T_COMPOUND, 32);
    VRFY((filetype >= 0), "Datatype creation succeeded");

    VRFY((H5Tinsert(filetype, "ShortData", 0, H5T_STD_I64BE) >= 0),
            "Datatype insertion succeeded");
    VRFY((H5Tinsert(filetype, "IntData", 8, H5T_STD_I64BE) >= 0),
            "Datatype insertion succeeded");
    VRFY((H5Tinsert(filetype, "LongData", 16, H5T_STD_I64BE) >= 0),
            "Datatype insertion succeeded");

    dset_id = H5Dcreate2(file_id, WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_NAME, filetype, filespace,
            H5P_DEFAULT, plist_id, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset creation succeeded");

    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

    /* Each process defines the dataset selection in memory and writes
     * it to the hyperslab in the file
     */
    count[0] = 1;
    count[1] = (hsize_t) WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_ENTRIES_PER_PROC;
    stride[0] = (hsize_t) WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_CH_NROWS;
    stride[1] = WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_CH_NCOLS;
    block[0] = (hsize_t) WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_CH_NROWS / (hsize_t) mpi_size;
    block[1] = WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_CH_NCOLS;
    start[0] = (hsize_t) mpi_rank;
    start[1] = 0;

    if (VERBOSE_MED) {
        HDprintf("Process %d is writing with count[ %llu, %llu ], stride[ %llu, %llu ], start[ %llu, %llu ], block size[ %llu, %llu ]\n",
                mpi_rank, count[0], count[1], stride[0], stride[1], start[0], start[1], block[0], block[1]);
        HDfflush(stdout);
    }

    /* Select hyperslab in the file */
    filespace = H5Dget_space(dset_id);
    VRFY((filespace >= 0), "File dataspace retrieval succeeded");

    VRFY((H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, stride, count, block) >= 0),
            "Hyperslab selection succeeded");

    data = (COMPOUND_C_DATATYPE *) HDcalloc(1, (hsize_t) WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_ENTRIES_PER_PROC * sizeof(*data));
    VRFY((NULL != data), "HDcalloc succeeded");

    correct_buf_size = dataset_dims[0] * dataset_dims[1] * sizeof(COMPOUND_C_DATATYPE);

    correct_buf = (COMPOUND_C_DATATYPE *) HDcalloc(1, correct_buf_size);
    VRFY((NULL != correct_buf), "HDcalloc succeeded");

    /* Fill data buffer */
    for (i = 0; i < (hsize_t) WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_ENTRIES_PER_PROC; i++) {
        data[i].field1 = (short) GEN_DATA(i);
        data[i].field2 = (int) GEN_DATA(i);
        data[i].field3 = (long) GEN_DATA(i);
    }

    /* Create property list for collective dataset write */
    plist_id = H5Pcreate(H5P_DATASET_XFER);
    VRFY((plist_id >= 0), "DXPL creation succeeded");

    VRFY((H5Pset_dxpl_mpio(plist_id, H5FD_MPIO_COLLECTIVE) >= 0),
            "Set DXPL MPIO succeeded");

    /* Ensure that this test currently fails since type conversions break collective mode */
    H5E_BEGIN_TRY {
        VRFY((H5Dwrite(dset_id, memtype, memspace, filespace, plist_id, data) < 0),
                "Dataset write succeeded");
    } H5E_END_TRY;

    if (data) HDfree(data);

    /* Verify that no data was written */
    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");

    read_buf = (COMPOUND_C_DATATYPE *) HDcalloc(1, correct_buf_size);
    VRFY((NULL != read_buf), "HDcalloc succeeded");

    dset_id = H5Dopen2(file_id, "/" WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_NAME, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    VRFY((H5Dread(dset_id, memtype, H5S_ALL, H5S_ALL, plist_id, read_buf) >= 0),
            "Dataset read succeeded");

    VRFY((0 == HDmemcmp(read_buf, correct_buf, correct_buf_size)),
            "Data verification succeeded");

    if (correct_buf) HDfree(correct_buf);
    if (read_buf) HDfree(read_buf);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Sclose(memspace) >= 0), "Memory dataspace close succeeded");
    VRFY((H5Tclose(filetype) >= 0), "File datatype close succeeded");
    VRFY((H5Tclose(memtype) >= 0), "Memory datatype close succeeded");
    VRFY((H5Pclose(plist_id) >= 0), "DXPL close succeeded");
    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    return;
}
#endif

/*
 * Tests parallel read of filtered data in the special
 * case where a dataset is composed of a single chunk.
 *
 * The MAINPROCESS rank will first write out all of the
 * data to the dataset. Then, each rank reads a part of
 * the singular chunk and contributes its piece to a
 * global buffer that is checked for consistency.
 *
 * Programmer: Jordan Henderson
 *             05/14/2018
 */
static void
test_read_one_chunk_filtered_dataset(void)
{
    C_DATATYPE *read_buf = NULL;
    C_DATATYPE *correct_buf = NULL;
    C_DATATYPE *global_buf = NULL;
    hsize_t     dataset_dims[READ_ONE_CHUNK_FILTERED_DATASET_DIMS];
    hsize_t     chunk_dims[READ_ONE_CHUNK_FILTERED_DATASET_DIMS];
    hsize_t     sel_dims[READ_ONE_CHUNK_FILTERED_DATASET_DIMS];
    hsize_t     start[READ_ONE_CHUNK_FILTERED_DATASET_DIMS];
    hsize_t     stride[READ_ONE_CHUNK_FILTERED_DATASET_DIMS];
    hsize_t     count[READ_ONE_CHUNK_FILTERED_DATASET_DIMS];
    hsize_t     block[READ_ONE_CHUNK_FILTERED_DATASET_DIMS];
    hsize_t     flat_dims[1];
    size_t      i, read_buf_size, correct_buf_size;
    hid_t       file_id = -1, dset_id = -1, plist_id = -1;
    hid_t       filespace = -1, memspace = -1;
    int        *recvcounts = NULL;
    int        *displs = NULL;

    if (MAINPROCESS) HDputs("Testing read from one-chunk filtered dataset");

    CHECK_CUR_FILTER_AVAIL();

    dataset_dims[0] = (hsize_t) READ_ONE_CHUNK_FILTERED_DATASET_NROWS;
    dataset_dims[1] = (hsize_t) READ_ONE_CHUNK_FILTERED_DATASET_NCOLS;

    /* Setup the buffer for writing and for comparison */
    correct_buf_size = dataset_dims[0] * dataset_dims[1] * sizeof(*correct_buf);

    correct_buf = (C_DATATYPE *) HDcalloc(1, correct_buf_size);
    VRFY((NULL != correct_buf), "HDcalloc succeeded");

    for (i = 0; i < correct_buf_size / sizeof(*correct_buf); i++)
            correct_buf[i] = ((C_DATATYPE) i % (READ_ONE_CHUNK_FILTERED_DATASET_CH_NROWS / mpi_size * READ_ONE_CHUNK_FILTERED_DATASET_CH_NCOLS))
                           + ((C_DATATYPE) i / (READ_ONE_CHUNK_FILTERED_DATASET_CH_NROWS / mpi_size * READ_ONE_CHUNK_FILTERED_DATASET_CH_NCOLS));

    if (MAINPROCESS) {
        plist_id = H5Pcreate(H5P_FILE_ACCESS);
        VRFY((plist_id >= 0), "FAPL creation succeeded");

        VRFY((H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0),
                "Set libver bounds succeeded");

        file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, plist_id);
        VRFY((file_id >= 0), "Test file open succeeded");

        VRFY((H5Pclose(plist_id) >= 0), "FAPL close succeeded");

        /* Create the dataspace for the dataset */
        filespace = H5Screate_simple(READ_ONE_CHUNK_FILTERED_DATASET_DIMS, dataset_dims, NULL);
        VRFY((filespace >= 0), "File dataspace creation succeeded");

        /* Create chunked dataset */
        chunk_dims[0] = (hsize_t) READ_ONE_CHUNK_FILTERED_DATASET_CH_NROWS;
        chunk_dims[1] = (hsize_t) READ_ONE_CHUNK_FILTERED_DATASET_CH_NCOLS;

        plist_id = H5Pcreate(H5P_DATASET_CREATE);
        VRFY((plist_id >= 0), "DCPL creation succeeded");

        VRFY((H5Pset_chunk(plist_id, READ_ONE_CHUNK_FILTERED_DATASET_DIMS, chunk_dims) >= 0),
                "Chunk size set");

        /* Add test filter to the pipeline */
        VRFY((set_dcpl_filter(plist_id) >= 0), "Filter set");

        dset_id = H5Dcreate2(file_id, READ_ONE_CHUNK_FILTERED_DATASET_NAME, HDF5_DATATYPE_NAME, filespace,
                H5P_DEFAULT, plist_id, H5P_DEFAULT);
        VRFY((dset_id >= 0), "Dataset creation succeeded");

        VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
        VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

        VRFY((H5Dwrite(dset_id, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, H5P_DEFAULT, correct_buf) >= 0),
                "Dataset write succeeded");

        VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
        VRFY((H5Fclose(file_id) >= 0), "File close succeeded");
    }

    /* Set up file access property list with parallel I/O access */
    plist_id = H5Pcreate(H5P_FILE_ACCESS);
    VRFY((plist_id >= 0), "FAPL creation succeeded");

    VRFY((H5Pset_fapl_mpio(plist_id, comm, info) >= 0),
            "Set FAPL MPIO succeeded");

    VRFY((H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0),
            "Set libver bounds succeeded");

    file_id = H5Fopen(filenames[0], H5F_ACC_RDONLY, plist_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    VRFY((H5Pclose(plist_id) >= 0), "FAPL close succeeded");

    dset_id = H5Dopen2(file_id, "/" READ_ONE_CHUNK_FILTERED_DATASET_NAME, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    sel_dims[0] = (hsize_t) READ_ONE_CHUNK_FILTERED_DATASET_NROWS / (hsize_t) mpi_size;
    sel_dims[1] = (hsize_t) READ_ONE_CHUNK_FILTERED_DATASET_NCOLS;

    /* Setup one-dimensional memory dataspace for reading the dataset data into a contiguous buffer */
    flat_dims[0] = sel_dims[0] * sel_dims[1];

    memspace = H5Screate_simple(1, flat_dims, NULL);
    VRFY((memspace >= 0), "Memory dataspace creation succeeded");

    /* Select hyperslab in the file */
    filespace = H5Dget_space(dset_id);
    VRFY((filespace >= 0), "File dataspace retrieval succeeded");

    /*
     * Each process defines the dataset selection in the file and
     * reads it to the selection in memory
     */
    count[0] = 1;
    count[1] = 1;
    stride[0] = (hsize_t) READ_ONE_CHUNK_FILTERED_DATASET_CH_NROWS;
    stride[1] = (hsize_t) READ_ONE_CHUNK_FILTERED_DATASET_CH_NCOLS;
    block[0] = sel_dims[0];
    block[1] = sel_dims[1];
    start[0] = ((hsize_t) mpi_rank * sel_dims[0]);
    start[1] = 0;

    if (VERBOSE_MED) {
        HDprintf("Process %d is reading with count[ %llu, %llu ], stride[ %llu, %llu ], start[ %llu, %llu ], block size[ %llu, %llu ]\n",
                mpi_rank, count[0], count[1], stride[0], stride[1], start[0], start[1], block[0], block[1]);
        HDfflush(stdout);
    }

    VRFY((H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, stride, count, block) >= 0),
            "Hyperslab selection succeeded");

    /* Create property list for collective dataset read */
    plist_id = H5Pcreate(H5P_DATASET_XFER);
    VRFY((plist_id >= 0), "DXPL creation succeeded");

    VRFY((H5Pset_dxpl_mpio(plist_id, H5FD_MPIO_COLLECTIVE) >= 0),
            "Set DXPL MPIO succeeded");

    read_buf_size = flat_dims[0] * sizeof(*read_buf);

    read_buf = (C_DATATYPE *) HDcalloc(1, read_buf_size);
    VRFY((NULL != read_buf), "HDcalloc succeeded");

    VRFY((H5Dread(dset_id, HDF5_DATATYPE_NAME, memspace, filespace, plist_id, read_buf) >= 0),
            "Dataset read succeeded");

    global_buf = (C_DATATYPE *) HDcalloc(1, correct_buf_size);
    VRFY((NULL != global_buf), "HDcalloc succeeded");

    /* Collect each piece of data from all ranks into a global buffer on all ranks */
    recvcounts = (int *) HDcalloc(1, (size_t) mpi_size * sizeof(*recvcounts));
    VRFY((NULL != recvcounts), "HDcalloc succeeded");

    for (i = 0; i < (size_t) mpi_size; i++)
        recvcounts[i] = (int) flat_dims[0];

    displs = (int *) HDcalloc(1, (size_t) mpi_size * sizeof(*displs));
    VRFY((NULL != displs), "HDcalloc succeeded");

    for (i = 0; i < (size_t) mpi_size; i++)
        displs[i] = (int) (i * flat_dims[0]);

    VRFY((MPI_SUCCESS == MPI_Allgatherv(read_buf, (int) flat_dims[0], C_DATATYPE_MPI, global_buf, recvcounts, displs, C_DATATYPE_MPI, comm)),
            "MPI_Allgatherv succeeded");

    VRFY((0 == HDmemcmp(global_buf, correct_buf, correct_buf_size)),
            "Data verification succeeded");

    if (displs) HDfree(displs);
    if (recvcounts) HDfree(recvcounts);
    if (global_buf) HDfree(global_buf);
    if (read_buf) HDfree(read_buf);
    if (correct_buf) HDfree(correct_buf);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Sclose(memspace) >= 0), "Memory dataspace close succeeded");
    VRFY((H5Pclose(plist_id) >= 0), "DXPL close succeeded");
    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    return;
}

/*
 * Tests parallel read of filtered data in the case where only
 * one process is reading from a particular chunk in the operation.
 *
 * The MAINPROCESS rank will first write out all of the
 * data to the dataset. Then, each rank reads a part of
 * the dataset and contributes its piece to a global buffer
 * that is checked for consistency.
 *
 * Programmer: Jordan Henderson
 *             05/15/2018
 */
static void
test_read_filtered_dataset_no_overlap(void)
{
    C_DATATYPE *read_buf = NULL;
    C_DATATYPE *correct_buf = NULL;
    C_DATATYPE *global_buf = NULL;
    hsize_t     dataset_dims[READ_UNSHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     chunk_dims[READ_UNSHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     sel_dims[READ_UNSHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     start[READ_UNSHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     stride[READ_UNSHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     count[READ_UNSHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     block[READ_UNSHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     flat_dims[1];
    size_t      i, read_buf_size, correct_buf_size;
    hid_t       file_id = -1, dset_id = -1, plist_id = -1;
    hid_t       filespace = -1, memspace = -1;
    int        *recvcounts = NULL;
    int        *displs = NULL;

    if (MAINPROCESS) HDputs("Testing read from unshared filtered chunks");

    CHECK_CUR_FILTER_AVAIL();

    dataset_dims[0] = (hsize_t) READ_UNSHARED_FILTERED_CHUNKS_NROWS;
    dataset_dims[1] = (hsize_t) READ_UNSHARED_FILTERED_CHUNKS_NCOLS;

    /* Setup the buffer for writing and for comparison */
    correct_buf_size = (hsize_t) READ_UNSHARED_FILTERED_CHUNKS_NROWS * (hsize_t) READ_UNSHARED_FILTERED_CHUNKS_NCOLS * sizeof(*correct_buf);

    correct_buf = (C_DATATYPE *) HDcalloc(1, correct_buf_size);
    VRFY((NULL != correct_buf), "HDcalloc succeeded");

    for (i = 0; i < correct_buf_size / sizeof(*correct_buf); i++)
        correct_buf[i] = (C_DATATYPE) (
                                         (i % (dataset_dims[0] / (hsize_t) mpi_size * dataset_dims[1]))
                                       + (i / (dataset_dims[0] / (hsize_t) mpi_size * dataset_dims[1]))
                                      );

    if (MAINPROCESS) {
        plist_id = H5Pcreate(H5P_FILE_ACCESS);
        VRFY((plist_id >= 0), "FAPL creation succeeded");

        VRFY((H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0),
                "Set libver bounds succeeded");

        file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, plist_id);
        VRFY((file_id >= 0), "Test file open succeeded");

        VRFY((H5Pclose(plist_id) >= 0), "FAPL close succeeded");

        /* Create the dataspace for the dataset */
        filespace = H5Screate_simple(READ_UNSHARED_FILTERED_CHUNKS_DATASET_DIMS, dataset_dims, NULL);
        VRFY((filespace >= 0), "File dataspace creation succeeded");

        /* Create chunked dataset */
        chunk_dims[0] = (hsize_t) READ_UNSHARED_FILTERED_CHUNKS_CH_NROWS;
        chunk_dims[1] = (hsize_t) READ_UNSHARED_FILTERED_CHUNKS_CH_NCOLS;

        plist_id = H5Pcreate(H5P_DATASET_CREATE);
        VRFY((plist_id >= 0), "DCPL creation succeeded");

        VRFY((H5Pset_chunk(plist_id, READ_UNSHARED_FILTERED_CHUNKS_DATASET_DIMS, chunk_dims) >= 0),
                "Chunk size set");

        /* Add test filter to the pipeline */
        VRFY((set_dcpl_filter(plist_id) >= 0), "Filter set");

        dset_id = H5Dcreate2(file_id, READ_UNSHARED_FILTERED_CHUNKS_DATASET_NAME, HDF5_DATATYPE_NAME, filespace,
                H5P_DEFAULT, plist_id, H5P_DEFAULT);
        VRFY((dset_id >= 0), "Dataset creation succeeded");

        VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
        VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

        VRFY((H5Dwrite(dset_id, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, H5P_DEFAULT, correct_buf) >= 0),
                "Dataset write succeeded");

        VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
        VRFY((H5Fclose(file_id) >= 0), "File close succeeded");
    }

    /* Set up file access property list with parallel I/O access */
    plist_id = H5Pcreate(H5P_FILE_ACCESS);
    VRFY((plist_id >= 0), "FAPL creation succeeded");

    VRFY((H5Pset_fapl_mpio(plist_id, comm, info) >= 0),
            "Set FAPL MPIO succeeded");

    VRFY((H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0),
                "Set libver bounds succeeded");

    file_id = H5Fopen(filenames[0], H5F_ACC_RDONLY, plist_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    VRFY((H5Pclose(plist_id) >= 0), "FAPL close succeeded");

    dset_id = H5Dopen2(file_id, "/" READ_UNSHARED_FILTERED_CHUNKS_DATASET_NAME, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    sel_dims[0] = (hsize_t) READ_UNSHARED_FILTERED_CHUNKS_CH_NROWS;
    sel_dims[1] = (hsize_t) READ_UNSHARED_FILTERED_CHUNKS_NCOLS;

    /* Setup one-dimensional memory dataspace for reading the dataset data into a contiguous buffer */
    flat_dims[0] = sel_dims[0] * sel_dims[1];

    memspace = H5Screate_simple(1, flat_dims, NULL);
    VRFY((memspace >= 0), "Memory dataspace creation succeeded");

    /* Select hyperslab in the file */
    filespace = H5Dget_space(dset_id);
    VRFY((filespace >= 0), "File dataspace retrieval succeeded");

    /*
     * Each process defines the dataset selection in the file and reads
     * it to the selection in memory
     */
    count[0] = 1;
    count[1] = (hsize_t) READ_UNSHARED_FILTERED_CHUNKS_NCOLS / (hsize_t) READ_UNSHARED_FILTERED_CHUNKS_CH_NCOLS;
    stride[0] = (hsize_t) READ_UNSHARED_FILTERED_CHUNKS_CH_NROWS;
    stride[1] = (hsize_t) READ_UNSHARED_FILTERED_CHUNKS_CH_NCOLS;
    block[0] = (hsize_t) READ_UNSHARED_FILTERED_CHUNKS_CH_NROWS;
    block[1] = (hsize_t) READ_UNSHARED_FILTERED_CHUNKS_CH_NCOLS;
    start[0] = ((hsize_t) mpi_rank * (hsize_t) READ_UNSHARED_FILTERED_CHUNKS_CH_NROWS * count[0]);
    start[1] = 0;

    if (VERBOSE_MED) {
        HDprintf("Process %d is reading with count[ %llu, %llu ], stride[ %llu, %llu ], start[ %llu, %llu ], block size[ %llu, %llu ]\n",
                mpi_rank, count[0], count[1], stride[0], stride[1], start[0], start[1], block[0], block[1]);
        HDfflush(stdout);
    }

    VRFY((H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, stride, count, block) >= 0),
            "Hyperslab selection succeeded");

    /* Create property list for collective dataset read */
    plist_id = H5Pcreate(H5P_DATASET_XFER);
    VRFY((plist_id >= 0), "DXPL creation succeeded");

    VRFY((H5Pset_dxpl_mpio(plist_id, H5FD_MPIO_COLLECTIVE) >= 0),
            "Set DXPL MPIO succeeded");

    read_buf_size = flat_dims[0] * sizeof(*read_buf);

    read_buf = (C_DATATYPE *) HDcalloc(1, read_buf_size);
    VRFY((NULL != read_buf), "HDcalloc succeeded");

    VRFY((H5Dread(dset_id, HDF5_DATATYPE_NAME, memspace, filespace, plist_id, read_buf) >= 0),
            "Dataset read succeeded");

    global_buf = (C_DATATYPE *) HDcalloc(1, correct_buf_size);
    VRFY((NULL != global_buf), "HDcalloc succeeded");

    /* Collect each piece of data from all ranks into a global buffer on all ranks */
    recvcounts = (int *) HDcalloc(1, (size_t) mpi_size * sizeof(*recvcounts));
    VRFY((NULL != recvcounts), "HDcalloc succeeded");

    for (i = 0; i < (size_t) mpi_size; i++)
        recvcounts[i] = (int) flat_dims[0];

    displs = (int *) HDcalloc(1, (size_t) mpi_size * sizeof(*displs));
    VRFY((NULL != displs), "HDcalloc succeeded");

    for (i = 0; i < (size_t) mpi_size; i++)
        displs[i] = (int) (i * flat_dims[0]);

    VRFY((MPI_SUCCESS == MPI_Allgatherv(read_buf, (int) flat_dims[0], C_DATATYPE_MPI, global_buf, recvcounts, displs, C_DATATYPE_MPI, comm)),
            "MPI_Allgatherv succeeded");

    VRFY((0 == HDmemcmp(global_buf, correct_buf, correct_buf_size)),
            "Data verification succeeded");

    if (displs) HDfree(displs);
    if (recvcounts) HDfree(recvcounts);
    if (global_buf) HDfree(global_buf);
    if (read_buf) HDfree(read_buf);
    if (correct_buf) HDfree(correct_buf);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Sclose(memspace) >= 0), "Memory dataspace close succeeded");
    VRFY((H5Pclose(plist_id) >= 0), "DXPL close succeeded");
    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    return;
}

/*
 * Tests parallel read of filtered data in the case where
 * more than one process is reading from a particular chunk
 * in the operation.
 *
 * The MAINPROCESS rank will first write out all of the
 * data to the dataset. Then, each rank reads a part of
 * each chunk of the dataset and contributes its pieces
 * to a global buffer that is checked for consistency.
 *
 * Programmer: Jordan Henderson
 *             05/15/2018
 */
static void
test_read_filtered_dataset_overlap(void)
{
    C_DATATYPE *read_buf = NULL;
    C_DATATYPE *correct_buf = NULL;
    C_DATATYPE *global_buf = NULL;
    hsize_t     dataset_dims[READ_SHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     chunk_dims[READ_SHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     sel_dims[READ_SHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     start[READ_SHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     stride[READ_SHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     count[READ_SHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     block[READ_SHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     flat_dims[1];
    size_t      i, read_buf_size, correct_buf_size;
    hid_t       file_id = -1, dset_id = -1, plist_id = -1;
    hid_t       filespace = -1, memspace = -1;
    int        *recvcounts = NULL;
    int        *displs = NULL;

    if (MAINPROCESS) HDputs("Testing read from shared filtered chunks");

    CHECK_CUR_FILTER_AVAIL();

    dataset_dims[0] = (hsize_t) READ_SHARED_FILTERED_CHUNKS_NROWS;
    dataset_dims[1] = (hsize_t) READ_SHARED_FILTERED_CHUNKS_NCOLS;

    /* Setup the buffer for writing and for comparison */
    correct_buf_size = dataset_dims[0] * dataset_dims[1] * sizeof(*correct_buf);

    correct_buf = (C_DATATYPE *) HDcalloc(1, correct_buf_size);
    VRFY((NULL != correct_buf), "HDcalloc succeeded");

    for (i = 0; i < correct_buf_size / sizeof(*correct_buf); i++)
        correct_buf[i] = (C_DATATYPE) (
                                          (dataset_dims[1] * (i / ((hsize_t) mpi_size * dataset_dims[1])))
                                        + (i % dataset_dims[1])
                                        + (((i % ((hsize_t) mpi_size * dataset_dims[1])) / dataset_dims[1]) % dataset_dims[1])
                                      );

    if (MAINPROCESS) {
        plist_id = H5Pcreate(H5P_FILE_ACCESS);
        VRFY((plist_id >= 0), "FAPL creation succeeded");

        VRFY((H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0),
                "Set libver bounds succeeded");

        file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, plist_id);
        VRFY((file_id >= 0), "Test file open succeeded");

        VRFY((H5Pclose(plist_id) >= 0), "FAPL close succeeded");

        /* Create the dataspace for the dataset */
        filespace = H5Screate_simple(READ_SHARED_FILTERED_CHUNKS_DATASET_DIMS, dataset_dims, NULL);
        VRFY((filespace >= 0), "File dataspace creation succeeded");

        /* Create chunked dataset */
        chunk_dims[0] = (hsize_t) READ_SHARED_FILTERED_CHUNKS_CH_NROWS;
        chunk_dims[1] = (hsize_t) READ_SHARED_FILTERED_CHUNKS_CH_NCOLS;

        plist_id = H5Pcreate(H5P_DATASET_CREATE);
        VRFY((plist_id >= 0), "DCPL creation succeeded");

        VRFY((H5Pset_chunk(plist_id, READ_SHARED_FILTERED_CHUNKS_DATASET_DIMS, chunk_dims) >= 0),
                "Chunk size set");

        /* Add test filter to the pipeline */
        VRFY((set_dcpl_filter(plist_id) >= 0), "Filter set");

        dset_id = H5Dcreate2(file_id, READ_SHARED_FILTERED_CHUNKS_DATASET_NAME, HDF5_DATATYPE_NAME, filespace,
                H5P_DEFAULT, plist_id, H5P_DEFAULT);
        VRFY((dset_id >= 0), "Dataset creation succeeded");

        VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
        VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

        VRFY((H5Dwrite(dset_id, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, H5P_DEFAULT, correct_buf) >= 0),
                "Dataset write succeeded");

        VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
        VRFY((H5Fclose(file_id) >= 0), "File close succeeded");
    }

    /* Set up file access property list with parallel I/O access */
    plist_id = H5Pcreate(H5P_FILE_ACCESS);
    VRFY((plist_id >= 0), "FAPL creation succeeded");

    VRFY((H5Pset_fapl_mpio(plist_id, comm, info) >= 0),
            "Set FAPL MPIO succeeded");

    VRFY((H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0),
            "Set libver bounds succeeded");

    file_id = H5Fopen(filenames[0], H5F_ACC_RDONLY, plist_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    VRFY((H5Pclose(plist_id) >= 0), "FAPL close succeeded");

    dset_id = H5Dopen2(file_id, "/" READ_SHARED_FILTERED_CHUNKS_DATASET_NAME, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    sel_dims[0] = (hsize_t) DIM0_SCALE_FACTOR;
    sel_dims[1] = (hsize_t) READ_SHARED_FILTERED_CHUNKS_CH_NCOLS * (hsize_t) DIM1_SCALE_FACTOR;

    /* Setup one-dimensional memory dataspace for reading the dataset data into a contiguous buffer */
    flat_dims[0] = sel_dims[0] * sel_dims[1];

    memspace = H5Screate_simple(1, flat_dims, NULL);
    VRFY((memspace >= 0), "Memory dataspace creation succeeded");

    /* Select hyperslab in the file */
    filespace = H5Dget_space(dset_id);
    VRFY((filespace >= 0), "File dataspace retrieval succeeded");

    /*
     * Each process defines the dataset selection in the file and
     * reads it to the selection in memory
     */
    count[0] = (hsize_t) READ_SHARED_FILTERED_CHUNKS_NROWS / (hsize_t) READ_SHARED_FILTERED_CHUNKS_CH_NROWS;
    count[1] = (hsize_t) READ_SHARED_FILTERED_CHUNKS_NCOLS / (hsize_t) READ_SHARED_FILTERED_CHUNKS_CH_NCOLS;
    stride[0] = (hsize_t) READ_SHARED_FILTERED_CHUNKS_CH_NROWS;
    stride[1] = (hsize_t) READ_SHARED_FILTERED_CHUNKS_CH_NCOLS;
    block[0] = (hsize_t) READ_SHARED_FILTERED_CHUNKS_CH_NROWS / (hsize_t) mpi_size;
    block[1] = (hsize_t) READ_SHARED_FILTERED_CHUNKS_CH_NCOLS;
    start[0] = (hsize_t) mpi_rank * block[0];
    start[1] = 0;

    if (VERBOSE_MED) {
        HDprintf("Process %d is reading with count[ %llu, %llu ], stride[ %llu, %llu ], start[ %llu, %llu ], block size[ %llu, %llu ]\n",
                mpi_rank, count[0], count[1], stride[0], stride[1], start[0], start[1], block[0], block[1]);
        HDfflush(stdout);
    }

    VRFY((H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, stride, count, block) >= 0),
            "Hyperslab selection succeeded");

    /* Create property list for collective dataset read */
    plist_id = H5Pcreate(H5P_DATASET_XFER);
    VRFY((plist_id >= 0), "DXPL creation succeeded");

    VRFY((H5Pset_dxpl_mpio(plist_id, H5FD_MPIO_COLLECTIVE) >= 0),
            "Set DXPL MPIO succeeded");

    read_buf_size = flat_dims[0] * sizeof(*read_buf);

    read_buf = (C_DATATYPE *) HDcalloc(1, read_buf_size);
    VRFY((NULL != read_buf), "HDcalloc succeeded");

    VRFY((H5Dread(dset_id, HDF5_DATATYPE_NAME, memspace, filespace, plist_id, read_buf) >= 0),
            "Dataset read succeeded");

    global_buf = (C_DATATYPE *) HDcalloc(1, correct_buf_size);
    VRFY((NULL != global_buf), "HDcalloc succeeded");

    /*
     * Since these chunks are shared, run multiple rounds of MPI_Allgatherv
     * to collect all of the pieces into their appropriate locations. The
     * number of times MPI_Allgatherv is run should be equal to the number
     * of chunks in the first dimension of the dataset.
     */
    {
        size_t loop_count = count[0];
        size_t total_recvcounts = 0;

        recvcounts = (int *) HDcalloc(1, (size_t) mpi_size * sizeof(*recvcounts));
        VRFY((NULL != recvcounts), "HDcalloc succeeded");

        displs = (int *) HDcalloc(1, (size_t) mpi_size * sizeof(*displs));
        VRFY((NULL != displs), "HDcalloc succeeded");

        for (i = 0; i < (size_t) mpi_size; i++) {
            recvcounts[i] = (int) dataset_dims[1];
            total_recvcounts += (size_t) recvcounts[i];
        }

        for (i = 0; i < (size_t) mpi_size; i++)
            displs[i] = (int) (i * dataset_dims[1]);

        for (; loop_count; loop_count--) {
            VRFY((MPI_SUCCESS == MPI_Allgatherv(&read_buf[(count[0] - loop_count) * dataset_dims[1]], recvcounts[mpi_rank], C_DATATYPE_MPI,
                    &global_buf[(count[0] - loop_count) * total_recvcounts], recvcounts, displs, C_DATATYPE_MPI, comm)),
                    "MPI_Allgatherv succeeded");
        }
    }

    VRFY((0 == HDmemcmp(global_buf, correct_buf, correct_buf_size)),
            "Data verification succeeded");

    if (displs) HDfree(displs);
    if (recvcounts) HDfree(recvcounts);
    if (global_buf) HDfree(global_buf);
    if (read_buf) HDfree(read_buf);
    if (correct_buf) HDfree(correct_buf);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Sclose(memspace) >= 0), "Memory dataspace close succeeded");
    VRFY((H5Pclose(plist_id) >= 0), "DXPL close succeeded");
    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    return;
}

/*
 * Tests parallel read of filtered data in the case where
 * a single process in the read operation has no selection
 * in the dataset's dataspace.
 *
 * The MAINPROCESS rank will first write out all of the
 * data to the dataset. Then, each rank (except for one)
 * reads a part of the dataset and contributes its piece
 * to a global buffer that is checked for consistency.
 *
 * Programmer: Jordan Henderson
 *             05/15/2018
 */
static void
test_read_filtered_dataset_single_no_selection(void)
{
    C_DATATYPE *read_buf = NULL;
    C_DATATYPE *correct_buf = NULL;
    C_DATATYPE *global_buf = NULL;
    hsize_t     dataset_dims[READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     chunk_dims[READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     sel_dims[READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     start[READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     stride[READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     count[READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     block[READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     flat_dims[1];
    size_t      i, read_buf_size, correct_buf_size;
    size_t      segment_length;
    hid_t       file_id = -1, dset_id = -1, plist_id = -1;
    hid_t       filespace = -1, memspace = -1;
    int        *recvcounts = NULL;
    int        *displs = NULL;

    if (MAINPROCESS) HDputs("Testing read from filtered chunks with a single process having no selection");

    CHECK_CUR_FILTER_AVAIL();

    dataset_dims[0] = (hsize_t) READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_NROWS;
    dataset_dims[1] = (hsize_t) READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_NCOLS;

    /* Setup the buffer for writing and for comparison */
    correct_buf_size = dataset_dims[0] * dataset_dims[1] * sizeof(*correct_buf);

    correct_buf = (C_DATATYPE *) HDcalloc(1, correct_buf_size);
    VRFY((NULL != correct_buf), "HDcalloc succeeded");

    for (i = 0; i < correct_buf_size / sizeof(*correct_buf); i++)
            correct_buf[i] =
                    (C_DATATYPE) (
                                    (i % (dataset_dims[0] / (hsize_t) mpi_size * dataset_dims[1]))
                                  + (i / (dataset_dims[0] / (hsize_t) mpi_size * dataset_dims[1]))
                                 );

    /* Compute the correct offset into the buffer for the process having no selection and clear it */
    segment_length = dataset_dims[0] * dataset_dims[1] / (hsize_t) mpi_size;
    HDmemset(correct_buf + ((size_t) READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_NO_SELECT_PROC * segment_length),
            0, segment_length * sizeof(*correct_buf));

    if (MAINPROCESS) {
        plist_id = H5Pcreate(H5P_FILE_ACCESS);
        VRFY((plist_id >= 0), "FAPL creation succeeded");

        VRFY((H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0),
                "Set libver bounds succeeded");

        file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, plist_id);
        VRFY((file_id >= 0), "Test file open succeeded");

        VRFY((H5Pclose(plist_id) >= 0), "FAPL close succeeded");

        /* Create the dataspace for the dataset */
        filespace = H5Screate_simple(READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS, dataset_dims, NULL);
        VRFY((filespace >= 0), "File dataspace creation succeeded");

        /* Create chunked dataset */
        chunk_dims[0] = (hsize_t) READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_CH_NROWS;
        chunk_dims[1] = (hsize_t) READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_CH_NCOLS;

        plist_id = H5Pcreate(H5P_DATASET_CREATE);
        VRFY((plist_id >= 0), "DCPL creation succeeded");

        VRFY((H5Pset_chunk(plist_id, READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS, chunk_dims) >= 0),
                "Chunk size set");

        /* Add test filter to the pipeline */
        VRFY((set_dcpl_filter(plist_id) >= 0), "Filter set");

        dset_id = H5Dcreate2(file_id, READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_DATASET_NAME, HDF5_DATATYPE_NAME, filespace,
                H5P_DEFAULT, plist_id, H5P_DEFAULT);
        VRFY((dset_id >= 0), "Dataset creation succeeded");

        VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
        VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

        VRFY((H5Dwrite(dset_id, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, H5P_DEFAULT, correct_buf) >= 0),
                "Dataset write succeeded");

        VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
        VRFY((H5Fclose(file_id) >= 0), "File close succeeded");
    }

    /* Set up file access property list with parallel I/O access */
    plist_id = H5Pcreate(H5P_FILE_ACCESS);
    VRFY((plist_id >= 0), "FAPL creation succeeded");

    VRFY((H5Pset_fapl_mpio(plist_id, comm, info) >= 0),
            "Set FAPL MPIO succeeded");

    VRFY((H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0),
            "Set libver bounds succeeded");

    file_id = H5Fopen(filenames[0], H5F_ACC_RDONLY, plist_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    VRFY((H5Pclose(plist_id) >= 0), "FAPL close succeeded");

    dset_id = H5Dopen2(file_id, "/" READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_DATASET_NAME, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    sel_dims[0] = (hsize_t) READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_CH_NROWS;
    sel_dims[1] = (hsize_t) READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_NCOLS;

    if (mpi_rank == READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_NO_SELECT_PROC)
        sel_dims[0] = sel_dims[1] = 0;

    /* Setup one-dimensional memory dataspace for reading the dataset data into a contiguous buffer */
    flat_dims[0] = sel_dims[0] * sel_dims[1];

    memspace = H5Screate_simple(1, flat_dims, NULL);
    VRFY((memspace >= 0), "Memory dataspace creation succeeded");

    /* Select hyperslab in the file */
    filespace = H5Dget_space(dset_id);
    VRFY((filespace >= 0), "File dataspace retrieval succeeded");

    /*
     * Each process defines the dataset selection in the file and
     * reads it to the selection in memory
     */
    count[0] = 1;
    count[1] = (hsize_t) READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_NCOLS / (hsize_t) READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_CH_NCOLS;
    stride[0] = (hsize_t) READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_CH_NROWS;
    stride[1] = (hsize_t) READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_CH_NCOLS;
    block[0] = (hsize_t) READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_CH_NROWS;
    block[1] = (hsize_t) READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_CH_NCOLS;
    start[0] = (hsize_t) mpi_rank * (hsize_t) READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_CH_NROWS * count[0];
    start[1] = 0;

    if (VERBOSE_MED) {
        HDprintf("Process %d is reading with count[ %llu, %llu ], stride[ %llu, %llu ], start[ %llu, %llu ], block size[ %llu, %llu ]\n",
                mpi_rank, count[0], count[1], stride[0], stride[1], start[0], start[1], block[0], block[1]);
        HDfflush(stdout);
    }

    if (mpi_rank == READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_NO_SELECT_PROC)
        VRFY((H5Sselect_none(filespace) >= 0), "Select none succeeded");
    else
        VRFY((H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, stride, count, block) >= 0),
                "Hyperslab selection succeeded");

    /* Create property list for collective dataset read */
    plist_id = H5Pcreate(H5P_DATASET_XFER);
    VRFY((plist_id >= 0), "DXPL creation succeeded");

    VRFY((H5Pset_dxpl_mpio(plist_id, H5FD_MPIO_COLLECTIVE) >= 0),
            "Set DXPL MPIO succeeded");

    read_buf_size = flat_dims[0] * sizeof(*read_buf);

    read_buf = (C_DATATYPE *) HDcalloc(1, read_buf_size);
    VRFY((NULL != read_buf), "HDcalloc succeeded");

    VRFY((H5Dread(dset_id, HDF5_DATATYPE_NAME, memspace, filespace, plist_id, read_buf) >= 0),
            "Dataset read succeeded");

    global_buf = (C_DATATYPE *) HDcalloc(1, correct_buf_size);
    VRFY((NULL != global_buf), "HDcalloc succeeded");

    /* Collect each piece of data from all ranks into a global buffer on all ranks */
    recvcounts = (int *) HDcalloc(1, (size_t) mpi_size * sizeof(*recvcounts));
    VRFY((NULL != recvcounts), "HDcalloc succeeded");

    for (i = 0; i < (size_t) mpi_size; i++)
        recvcounts[i] = (int) (READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_CH_NROWS * READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_NCOLS);
    recvcounts[READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_NO_SELECT_PROC] = 0;

    displs = (int *) HDcalloc(1, (size_t) mpi_size * sizeof(*displs));
    VRFY((NULL != displs), "HDcalloc succeeded");

    for (i = 0; i < (size_t) mpi_size; i++)
        displs[i] = (int) (i * (size_t) (READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_CH_NROWS * READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_NCOLS));

    if (mpi_rank == READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_NO_SELECT_PROC)
        VRFY((MPI_SUCCESS == MPI_Allgatherv(read_buf, 0, C_DATATYPE_MPI, global_buf, recvcounts, displs, C_DATATYPE_MPI, comm)),
                "MPI_Allgatherv succeeded");
    else
        VRFY((MPI_SUCCESS == MPI_Allgatherv(read_buf, (int) flat_dims[0], C_DATATYPE_MPI, global_buf, recvcounts, displs, C_DATATYPE_MPI, comm)),
                "MPI_Allgatherv succeeded");

    VRFY((0 == HDmemcmp(global_buf, correct_buf, correct_buf_size)),
            "Data verification succeeded");

    if (displs) HDfree(displs);
    if (recvcounts) HDfree(recvcounts);
    if (global_buf) HDfree(global_buf);
    if (read_buf) HDfree(read_buf);
    if (correct_buf) HDfree(correct_buf);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Sclose(memspace) >= 0), "Memory dataspace close succeeded");
    VRFY((H5Pclose(plist_id) >= 0), "DXPL close succeeded");
    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    return;
}

/*
 * Tests parallel read of filtered data in the case where
 * no process in the read operation has a selection in the
 * dataset's dataspace. This test is to ensure that there
 * are no assertion failures or similar issues due to size
 * 0 allocations and the like.
 *
 * The MAINPROCESS rank will first write out all of the
 * data to the dataset. Then, each rank will simply issue
 * a no-op read.
 *
 * Programmer: Jordan Henderson
 *             05/15/2018
 */
static void
test_read_filtered_dataset_all_no_selection(void)
{
    C_DATATYPE *read_buf = NULL;
    C_DATATYPE *correct_buf = NULL;
    hsize_t     dataset_dims[READ_ALL_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     chunk_dims[READ_ALL_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     sel_dims[READ_ALL_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    size_t      read_buf_size, correct_buf_size;
    hid_t       file_id = -1, dset_id = -1, plist_id = -1;
    hid_t       filespace = -1, memspace = -1;

    if (MAINPROCESS) HDputs("Testing read from filtered chunks with all processes having no selection");

    CHECK_CUR_FILTER_AVAIL();

    dataset_dims[0] = (hsize_t) READ_ALL_NO_SELECTION_FILTERED_CHUNKS_NROWS;
    dataset_dims[1] = (hsize_t) READ_ALL_NO_SELECTION_FILTERED_CHUNKS_NCOLS;

    /* Setup the buffer for writing and for comparison */
    correct_buf_size = dataset_dims[0] * dataset_dims[1] * sizeof(*correct_buf);

    correct_buf = (C_DATATYPE *) HDcalloc(1, correct_buf_size);
    VRFY((NULL != correct_buf), "HDcalloc succeeded");

    if (MAINPROCESS) {
        plist_id = H5Pcreate(H5P_FILE_ACCESS);
        VRFY((plist_id >= 0), "FAPL creation succeeded");

        VRFY((H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0),
                "Set libver bounds succeeded");

        file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, plist_id);
        VRFY((file_id >= 0), "Test file open succeeded");

        VRFY((H5Pclose(plist_id) >= 0), "FAPL close succeeded");

        /* Create the dataspace for the dataset */
        filespace = H5Screate_simple(READ_ALL_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS, dataset_dims, NULL);
        VRFY((filespace >= 0), "File dataspace creation succeeded");

        /* Create chunked dataset */
        chunk_dims[0] = (hsize_t) READ_ALL_NO_SELECTION_FILTERED_CHUNKS_CH_NROWS;
        chunk_dims[1] = (hsize_t) READ_ALL_NO_SELECTION_FILTERED_CHUNKS_CH_NCOLS;

        plist_id = H5Pcreate(H5P_DATASET_CREATE);
        VRFY((plist_id >= 0), "DCPL creation succeeded");

        VRFY((H5Pset_chunk(plist_id, READ_ALL_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS, chunk_dims) >= 0),
                "Chunk size set");

        /* Add test filter to the pipeline */
        VRFY((set_dcpl_filter(plist_id) >= 0), "Filter set");

        dset_id = H5Dcreate2(file_id, READ_ALL_NO_SELECTION_FILTERED_CHUNKS_DATASET_NAME, HDF5_DATATYPE_NAME, filespace,
                H5P_DEFAULT, plist_id, H5P_DEFAULT);
        VRFY((dset_id >= 0), "Dataset creation succeeded");

        VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
        VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

        VRFY((H5Dwrite(dset_id, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, H5P_DEFAULT, correct_buf) >= 0),
                "Dataset write succeeded");

        VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
        VRFY((H5Fclose(file_id) >= 0), "File close succeeded");
    }

    /* Set up file access property list with parallel I/O access */
    plist_id = H5Pcreate(H5P_FILE_ACCESS);
    VRFY((plist_id >= 0), "FAPL creation succeeded");

    VRFY((H5Pset_fapl_mpio(plist_id, comm, info) >= 0),
            "Set FAPL MPIO succeeded");

    VRFY((H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0),
            "Set libver bounds succeeded");

    file_id = H5Fopen(filenames[0], H5F_ACC_RDONLY, plist_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    VRFY((H5Pclose(plist_id) >= 0), "FAPL close succeeded");

    dset_id = H5Dopen2(file_id, "/" READ_ALL_NO_SELECTION_FILTERED_CHUNKS_DATASET_NAME, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    sel_dims[0] = sel_dims[1] = 0;

    memspace = H5Screate_simple(READ_ALL_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS, sel_dims, NULL);
    VRFY((memspace >= 0), "Memory dataspace creation succeeded");

    /* Select hyperslab in the file */
    filespace = H5Dget_space(dset_id);
    VRFY((filespace >= 0), "File dataspace retrieval succeeded");

    VRFY((H5Sselect_none(filespace) >= 0), "Select none succeeded");

    /* Create property list for collective dataset read */
    plist_id = H5Pcreate(H5P_DATASET_XFER);
    VRFY((plist_id >= 0), "DXPL creation succeeded");

    VRFY((H5Pset_dxpl_mpio(plist_id, H5FD_MPIO_COLLECTIVE) >= 0),
            "Set DXPL MPIO succeeded");

    read_buf_size = dataset_dims[0] * dataset_dims[1] * sizeof(*read_buf);

    read_buf = (C_DATATYPE *) HDcalloc(1, read_buf_size);
    VRFY((NULL != read_buf), "HDcalloc succeeded");

    VRFY((H5Dread(dset_id, HDF5_DATATYPE_NAME, memspace, filespace, plist_id, read_buf) >= 0),
            "Dataset read succeeded");

    if (read_buf) HDfree(read_buf);
    if (correct_buf) HDfree(correct_buf);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Sclose(memspace) >= 0), "Memory dataspace close succeeded");
    VRFY((H5Pclose(plist_id) >= 0), "DXPL close succeeded");
    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    return;
}

/*
 * Tests parallel read of filtered data by using point
 * selections instead of hyperslab selections.
 *
 * The MAINPROCESS rank will first write out all of the
 * data to the dataset. Then, each rank will read part
 * of the dataset using a point selection and will
 * contribute its piece to a global buffer that is
 * checked for consistency.
 *
 * Programmer: Jordan Henderson
 *             05/15/2018
 */
static void
test_read_filtered_dataset_point_selection(void)
{
    C_DATATYPE *correct_buf = NULL;
    C_DATATYPE *read_buf = NULL;
    C_DATATYPE *global_buf = NULL;
    hsize_t    *coords = NULL;
    hsize_t     dataset_dims[READ_POINT_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     chunk_dims[READ_POINT_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     sel_dims[READ_POINT_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     flat_dims[1];
    size_t      i, j, read_buf_size, correct_buf_size;
    size_t      num_points;
    hid_t       file_id = -1, dset_id = -1, plist_id = -1;
    hid_t       filespace = -1, memspace = -1;
    int        *recvcounts = NULL;
    int        *displs = NULL;

    if (MAINPROCESS) HDputs("Testing read from filtered chunks with point selection");

    CHECK_CUR_FILTER_AVAIL();

    dataset_dims[0] = (hsize_t) READ_POINT_SELECTION_FILTERED_CHUNKS_NROWS;
    dataset_dims[1] = (hsize_t) READ_POINT_SELECTION_FILTERED_CHUNKS_NCOLS;

    /* Setup the buffer for writing and for comparison */
    correct_buf_size = dataset_dims[0] * dataset_dims[1] * sizeof(*correct_buf);

    correct_buf = (C_DATATYPE *) HDcalloc(1, correct_buf_size);
    VRFY((NULL != correct_buf), "HDcalloc succeeded");

    for (i = 0; i < correct_buf_size / sizeof(*correct_buf); i++)
        correct_buf[i] = (C_DATATYPE) (
                                         (dataset_dims[1] * (i / ((hsize_t) mpi_size * dataset_dims[1])))
                                       + (i % dataset_dims[1])
                                       + (((i % ((hsize_t) mpi_size * dataset_dims[1])) / dataset_dims[1]) % dataset_dims[1])
                                      );

    if (MAINPROCESS) {
        plist_id = H5Pcreate(H5P_FILE_ACCESS);
        VRFY((plist_id >= 0), "FAPL creation succeeded");

        VRFY((H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0),
                "Set libver bounds succeeded");

        file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, plist_id);
        VRFY((file_id >= 0), "Test file open succeeded");

        VRFY((H5Pclose(plist_id) >= 0), "FAPL close succeeded");

        /* Create the dataspace for the dataset */
        filespace = H5Screate_simple(READ_POINT_SELECTION_FILTERED_CHUNKS_DATASET_DIMS, dataset_dims, NULL);
        VRFY((filespace >= 0), "File dataspace creation succeeded");

        /* Create chunked dataset */
        chunk_dims[0] = (hsize_t) READ_POINT_SELECTION_FILTERED_CHUNKS_CH_NROWS;
        chunk_dims[1] = (hsize_t) READ_POINT_SELECTION_FILTERED_CHUNKS_CH_NCOLS;

        plist_id = H5Pcreate(H5P_DATASET_CREATE);
        VRFY((plist_id >= 0), "DCPL creation succeeded");

        VRFY((H5Pset_chunk(plist_id, READ_POINT_SELECTION_FILTERED_CHUNKS_DATASET_DIMS, chunk_dims) >= 0),
                "Chunk size set");

        /* Add test filter to the pipeline */
        VRFY((set_dcpl_filter(plist_id) >= 0), "Filter set");

        dset_id = H5Dcreate2(file_id, READ_POINT_SELECTION_FILTERED_CHUNKS_DATASET_NAME, HDF5_DATATYPE_NAME, filespace,
                H5P_DEFAULT, plist_id, H5P_DEFAULT);
        VRFY((dset_id >= 0), "Dataset creation succeeded");

        VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
        VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

        VRFY((H5Dwrite(dset_id, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, H5P_DEFAULT, correct_buf) >= 0),
                "Dataset write succeeded");

        VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
        VRFY((H5Fclose(file_id) >= 0), "File close succeeded");
    }

    /* Set up file access property list with parallel I/O access */
    plist_id = H5Pcreate(H5P_FILE_ACCESS);
    VRFY((plist_id >= 0), "FAPL creation succeeded");

    VRFY((H5Pset_fapl_mpio(plist_id, comm, info) >= 0),
            "Set FAPL MPIO succeeded");

    VRFY((H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0),
            "Set libver bounds succeeded");

    file_id = H5Fopen(filenames[0], H5F_ACC_RDONLY, plist_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    VRFY((H5Pclose(plist_id) >= 0), "FAPL close succeeded");

    dset_id = H5Dopen2(file_id, "/" READ_POINT_SELECTION_FILTERED_CHUNKS_DATASET_NAME, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    sel_dims[0] = (hsize_t) READ_POINT_SELECTION_FILTERED_CHUNKS_NROWS / (hsize_t) mpi_size;
    sel_dims[1] = (hsize_t) READ_POINT_SELECTION_FILTERED_CHUNKS_NCOLS;

    /* Setup one-dimensional memory dataspace for reading the dataset data into a contiguous buffer */
    flat_dims[0] = sel_dims[0] * sel_dims[1];

    memspace = H5Screate_simple(1, flat_dims, NULL);
    VRFY((memspace >= 0), "Memory dataspace creation succeeded");

    /* Set up point selection */
    filespace = H5Dget_space(dset_id);
    VRFY((filespace >= 0), "File dataspace retrieval succeeded");

    num_points = (hsize_t) READ_POINT_SELECTION_FILTERED_CHUNKS_NROWS * (hsize_t) READ_POINT_SELECTION_FILTERED_CHUNKS_NCOLS / (hsize_t) mpi_size;
    coords = (hsize_t *) HDcalloc(1, 2 * num_points * sizeof(*coords));
    VRFY((NULL != coords), "Coords HDcalloc succeeded");

    for (i = 0; i < num_points; i++)
        for (j = 0; j < READ_POINT_SELECTION_FILTERED_CHUNKS_DATASET_DIMS; j++)
            coords[(i * READ_POINT_SELECTION_FILTERED_CHUNKS_DATASET_DIMS) + j] =
                    (j > 0) ? (i % (hsize_t) READ_POINT_SELECTION_FILTERED_CHUNKS_NCOLS)
                            : ((hsize_t) mpi_rank + ((hsize_t) mpi_size * (i / (hsize_t) READ_POINT_SELECTION_FILTERED_CHUNKS_NCOLS)));

    VRFY((H5Sselect_elements(filespace, H5S_SELECT_SET, (hsize_t ) num_points, (const hsize_t * ) coords) >= 0),
            "Point selection succeeded");

    /* Create property list for collective dataset read */
    plist_id = H5Pcreate(H5P_DATASET_XFER);
    VRFY((plist_id >= 0), "DXPL creation succeeded");

    VRFY((H5Pset_dxpl_mpio(plist_id, H5FD_MPIO_COLLECTIVE) >= 0),
            "Set DXPL MPIO succeeded");

    read_buf_size = flat_dims[0] * sizeof(*read_buf);

    read_buf = (C_DATATYPE *) HDcalloc(1, read_buf_size);
    VRFY((NULL != read_buf), "HDcalloc succeeded");

    VRFY((H5Dread(dset_id, HDF5_DATATYPE_NAME, memspace, filespace, plist_id, read_buf) >= 0),
            "Dataset read succeeded");

    global_buf = (C_DATATYPE *) HDcalloc(1, correct_buf_size);
    VRFY((NULL != global_buf), "HDcalloc succeeded");

    /*
     * Since these chunks are shared, run multiple rounds of MPI_Allgatherv
     * to collect all of the pieces into their appropriate locations. The
     * number of times MPI_Allgatherv is run should be equal to the number
     * of chunks in the first dimension of the dataset.
     */
    {
        size_t original_loop_count = dataset_dims[0] / (hsize_t) mpi_size;
        size_t cur_loop_count = original_loop_count;
        size_t total_recvcounts = 0;

        recvcounts = (int *) HDcalloc(1, (size_t) mpi_size * sizeof(*recvcounts));
        VRFY((NULL != recvcounts), "HDcalloc succeeded");

        displs = (int *) HDcalloc(1, (size_t) mpi_size * sizeof(*displs));
        VRFY((NULL != displs), "HDcalloc succeeded");

        for (i = 0; i < (size_t) mpi_size; i++) {
            recvcounts[i] = (int) dataset_dims[1];
            total_recvcounts += (size_t) recvcounts[i];
        }

        for (i = 0; i < (size_t) mpi_size; i++)
            displs[i] = (int) (i * dataset_dims[1]);

        for (; cur_loop_count; cur_loop_count--) {
            VRFY((MPI_SUCCESS == MPI_Allgatherv(&read_buf[(original_loop_count - cur_loop_count) * dataset_dims[1]], recvcounts[mpi_rank], C_DATATYPE_MPI,
                    &global_buf[(original_loop_count - cur_loop_count) * total_recvcounts], recvcounts, displs, C_DATATYPE_MPI, comm)),
                    "MPI_Allgatherv succeeded");
        }
    }

    VRFY((0 == HDmemcmp(global_buf, correct_buf, correct_buf_size)),
            "Data verification succeeded");

    if (displs) HDfree(displs);
    if (recvcounts) HDfree(recvcounts);
    if (global_buf) HDfree(global_buf);
    if (read_buf) HDfree(read_buf);
    if (correct_buf) HDfree(correct_buf);

    HDfree(coords);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Sclose(memspace) >= 0), "Memory dataspace close succeeded");
    VRFY((H5Pclose(plist_id) >= 0), "DXPL close succeeded");
    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    return;
}

/*
 * Tests parallel read of filtered data in the case where
 * each process reads an equal amount of data from each
 * chunk in the dataset. Each chunk is distributed among the
 * processes in round-robin fashion by blocks of size 1 until
 * the whole chunk is selected, leading to an interleaved
 * read pattern.
 *
 * The MAINPROCESS rank will first write out all of the
 * data to the dataset. Then, each rank will read part
 * of each chunk of the dataset and will contribute its
 * pieces to a global buffer that is checked for consistency.
 *
 * Programmer: Jordan Henderson
 *             05/15/2018
 */
static void
test_read_filtered_dataset_interleaved_read(void)
{
    C_DATATYPE *read_buf = NULL;
    C_DATATYPE *correct_buf = NULL;
    C_DATATYPE *global_buf = NULL;
    hsize_t     dataset_dims[INTERLEAVED_READ_FILTERED_DATASET_DIMS];
    hsize_t     chunk_dims[INTERLEAVED_READ_FILTERED_DATASET_DIMS];
    hsize_t     sel_dims[INTERLEAVED_READ_FILTERED_DATASET_DIMS];
    hsize_t     start[INTERLEAVED_READ_FILTERED_DATASET_DIMS];
    hsize_t     stride[INTERLEAVED_READ_FILTERED_DATASET_DIMS];
    hsize_t     count[INTERLEAVED_READ_FILTERED_DATASET_DIMS];
    hsize_t     block[INTERLEAVED_READ_FILTERED_DATASET_DIMS];
    hsize_t     flat_dims[1];
    size_t      i, read_buf_size, correct_buf_size;
    hid_t       file_id = -1, dset_id = -1, plist_id = -1;
    hid_t       filespace = -1, memspace = -1;
    int        *recvcounts = NULL;
    int        *displs = NULL;

    if (MAINPROCESS) HDputs("Testing interleaved read from filtered chunks");

    CHECK_CUR_FILTER_AVAIL();

    dataset_dims[0] = (hsize_t) INTERLEAVED_READ_FILTERED_DATASET_NROWS;
    dataset_dims[1] = (hsize_t) INTERLEAVED_READ_FILTERED_DATASET_NCOLS;

    /* Setup the buffer for writing and for comparison */
    correct_buf_size = dataset_dims[0] * dataset_dims[1] * sizeof(*correct_buf);

    correct_buf = (C_DATATYPE *) HDcalloc(1, correct_buf_size);
    VRFY((NULL != correct_buf), "HDcalloc succeeded");

    for (i = 0; i < correct_buf_size / sizeof(*correct_buf); i++)
        /* Add Column Index */
        correct_buf[i] =
                (C_DATATYPE) (
                                 (i % (hsize_t) INTERLEAVED_READ_FILTERED_DATASET_NCOLS)

                                 /* Add the Row Index */
                               + ((i % (hsize_t) (mpi_size * INTERLEAVED_READ_FILTERED_DATASET_NCOLS)) / (hsize_t) INTERLEAVED_READ_FILTERED_DATASET_NCOLS)

                                 /* Add the amount that gets added when a rank moves down to its next section vertically in the dataset */
                               + ((hsize_t) INTERLEAVED_READ_FILTERED_DATASET_NCOLS * (i / (hsize_t) (mpi_size * INTERLEAVED_READ_FILTERED_DATASET_NCOLS)))
                             );

    if (MAINPROCESS) {
        plist_id = H5Pcreate(H5P_FILE_ACCESS);
        VRFY((plist_id >= 0), "FAPL creation succeeded");

        VRFY((H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0),
                "Set libver bounds succeeded");

        file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, plist_id);
        VRFY((file_id >= 0), "Test file open succeeded");

        VRFY((H5Pclose(plist_id) >= 0), "FAPL close succeeded");

        /* Create the dataspace for the dataset */
        filespace = H5Screate_simple(INTERLEAVED_READ_FILTERED_DATASET_DIMS, dataset_dims, NULL);
        VRFY((filespace >= 0), "File dataspace creation succeeded");

        /* Create chunked dataset */
        chunk_dims[0] = (hsize_t) INTERLEAVED_READ_FILTERED_DATASET_CH_NROWS;
        chunk_dims[1] = (hsize_t) INTERLEAVED_READ_FILTERED_DATASET_CH_NCOLS;

        plist_id = H5Pcreate(H5P_DATASET_CREATE);
        VRFY((plist_id >= 0), "DCPL creation succeeded");

        VRFY((H5Pset_chunk(plist_id, INTERLEAVED_READ_FILTERED_DATASET_DIMS, chunk_dims) >= 0),
                "Chunk size set");

        /* Add test filter to the pipeline */
        VRFY((set_dcpl_filter(plist_id) >= 0), "Filter set");

        dset_id = H5Dcreate2(file_id, INTERLEAVED_READ_FILTERED_DATASET_NAME, HDF5_DATATYPE_NAME, filespace,
                H5P_DEFAULT, plist_id, H5P_DEFAULT);
        VRFY((dset_id >= 0), "Dataset creation succeeded");

        VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
        VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

        VRFY((H5Dwrite(dset_id, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, H5P_DEFAULT, correct_buf) >= 0),
                "Dataset write succeeded");

        VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
        VRFY((H5Fclose(file_id) >= 0), "File close succeeded");
    }

    /* Set up file access property list with parallel I/O access */
    plist_id = H5Pcreate(H5P_FILE_ACCESS);
    VRFY((plist_id >= 0), "FAPL creation succeeded");

    VRFY((H5Pset_fapl_mpio(plist_id, comm, info) >= 0),
            "Set FAPL MPIO succeeded");

    VRFY((H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0),
            "Set libver bounds succeeded");

    file_id = H5Fopen(filenames[0], H5F_ACC_RDONLY, plist_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    VRFY((H5Pclose(plist_id) >= 0), "FAPL close succeeded");

    dset_id = H5Dopen2(file_id, "/" INTERLEAVED_READ_FILTERED_DATASET_NAME, H5P_DEFAULT);
        VRFY((dset_id >= 0), "Dataset open succeeded");

    sel_dims[0] = (hsize_t) (INTERLEAVED_READ_FILTERED_DATASET_NROWS / mpi_size);
    sel_dims[1] = (hsize_t) INTERLEAVED_READ_FILTERED_DATASET_NCOLS;

    /* Setup one-dimensional memory dataspace for reading the dataset data into a contiguous buffer */
    flat_dims[0] = sel_dims[0] * sel_dims[1];

    memspace = H5Screate_simple(1, flat_dims, NULL);
    VRFY((memspace >= 0), "Memory dataspace creation succeeded");

    /* Select hyperslab in the file */
    filespace = H5Dget_space(dset_id);
    VRFY((filespace >= 0), "File dataspace retrieval succeeded");

    /*
     * Each process defines the dataset selection in the file and
     * reads it to the selection in memory
     */
    count[0] = (hsize_t) (INTERLEAVED_READ_FILTERED_DATASET_NROWS / INTERLEAVED_READ_FILTERED_DATASET_CH_NROWS);
    count[1] = (hsize_t) (INTERLEAVED_READ_FILTERED_DATASET_NCOLS / INTERLEAVED_READ_FILTERED_DATASET_CH_NCOLS);
    stride[0] = (hsize_t) INTERLEAVED_READ_FILTERED_DATASET_CH_NROWS;
    stride[1] = (hsize_t) INTERLEAVED_READ_FILTERED_DATASET_CH_NCOLS;
    block[0] = 1;
    block[1] = (hsize_t) INTERLEAVED_READ_FILTERED_DATASET_CH_NCOLS;
    start[0] = (hsize_t) mpi_rank;
    start[1] = 0;

    if (VERBOSE_MED) {
        HDprintf("Process %d is reading with count[ %llu, %llu ], stride[ %llu, %llu ], start[ %llu, %llu ], block size[ %llu, %llu ]\n",
                mpi_rank, count[0], count[1], stride[0], stride[1], start[0], start[1], block[0], block[1]);
        HDfflush(stdout);
    }

    VRFY((H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, stride, count, block) >= 0),
            "Hyperslab selection succeeded");

    /* Create property list for collective dataset read */
    plist_id = H5Pcreate(H5P_DATASET_XFER);
    VRFY((plist_id >= 0), "DXPL creation succeeded");

    VRFY((H5Pset_dxpl_mpio(plist_id, H5FD_MPIO_COLLECTIVE) >= 0),
            "Set DXPL MPIO succeeded");

    read_buf_size = flat_dims[0] * sizeof(*read_buf);

    read_buf = (C_DATATYPE *) HDcalloc(1, read_buf_size);
    VRFY((NULL != read_buf), "HDcalloc succeeded");

    VRFY((H5Dread(dset_id, HDF5_DATATYPE_NAME, memspace, filespace, plist_id, read_buf) >= 0),
            "Dataset read succeeded");

    global_buf = (C_DATATYPE *) HDcalloc(1, correct_buf_size);
    VRFY((NULL != global_buf), "HDcalloc succeeded");

    /*
     * Since these chunks are shared, run multiple rounds of MPI_Allgatherv
     * to collect all of the pieces into their appropriate locations. The
     * number of times MPI_Allgatherv is run should be equal to the number
     * of chunks in the first dimension of the dataset.
     */
    {
        size_t loop_count = count[0];
        size_t total_recvcounts = 0;

        recvcounts = (int *) HDcalloc(1, (size_t) mpi_size * sizeof(*recvcounts));
        VRFY((NULL != recvcounts), "HDcalloc succeeded");

        displs = (int *) HDcalloc(1, (size_t) mpi_size * sizeof(*displs));
        VRFY((NULL != displs), "HDcalloc succeeded");

        for (i = 0; i < (size_t) mpi_size; i++) {
            recvcounts[i] = (int) dataset_dims[1];
            total_recvcounts += (size_t) recvcounts[i];
        }

        for (i = 0; i < (size_t) mpi_size; i++)
            displs[i] = (int) (i * dataset_dims[1]);

        for (; loop_count; loop_count--) {
            VRFY((MPI_SUCCESS == MPI_Allgatherv(&read_buf[(count[0] - loop_count) * dataset_dims[1]], recvcounts[mpi_rank], C_DATATYPE_MPI,
                    &global_buf[(count[0] - loop_count) * total_recvcounts], recvcounts, displs, C_DATATYPE_MPI, comm)),
                    "MPI_Allgatherv succeeded");
        }
    }

    VRFY((0 == HDmemcmp(global_buf, correct_buf, correct_buf_size)),
            "Data verification succeeded");

    if (displs) HDfree(displs);
    if (recvcounts) HDfree(recvcounts);
    if (global_buf) HDfree(global_buf);
    if (read_buf) HDfree(read_buf);
    if (correct_buf) HDfree(correct_buf);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Sclose(memspace) >= 0), "Memory dataspace close succeeded");
    VRFY((H5Pclose(plist_id) >= 0), "DXPL close succeeded");
    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    return;
}

/*
 * Tests parallel read of filtered data in the case where
 * the dataset has 3 dimensions and each process reads from
 * its own "page" in the 3rd dimension.
 *
 * The MAINPROCESS rank will first write out all of the
 * data to the dataset. Then, each rank reads its own "page"
 * of the dataset and contributes its piece to a global buffer
 * that is checked for consistency.
 *
 * Programmer: Jordan Henderson
 *             05/16/2018
 */
static void
test_read_3d_filtered_dataset_no_overlap_separate_pages(void)
{
    MPI_Datatype  vector_type;
    MPI_Datatype  resized_vector_type;
    C_DATATYPE   *read_buf = NULL;
    C_DATATYPE   *correct_buf = NULL;
    C_DATATYPE   *global_buf = NULL;
    hsize_t       dataset_dims[READ_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_DIMS];
    hsize_t       chunk_dims[READ_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_DIMS];
    hsize_t       sel_dims[READ_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_DIMS];
    hsize_t       start[READ_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_DIMS];
    hsize_t       stride[READ_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_DIMS];
    hsize_t       count[READ_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_DIMS];
    hsize_t       block[READ_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_DIMS];
    hsize_t       flat_dims[1];
    size_t        i, read_buf_size, correct_buf_size;
    hid_t         file_id = -1, dset_id = -1, plist_id = -1;
    hid_t         filespace = -1, memspace = -1;

    if (MAINPROCESS) HDputs("Testing read from unshared filtered chunks on separate pages in 3D dataset");

    CHECK_CUR_FILTER_AVAIL();

    dataset_dims[0] = (hsize_t) READ_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_NROWS;
    dataset_dims[1] = (hsize_t) READ_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_NCOLS;
    dataset_dims[2] = (hsize_t) READ_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DEPTH;

    /* Setup the buffer for writing and for comparison */
    correct_buf_size = dataset_dims[0] * dataset_dims[1] * dataset_dims[2] * sizeof(*correct_buf);

    correct_buf = (C_DATATYPE *) HDcalloc(1, correct_buf_size);
    VRFY((NULL != correct_buf), "HDcalloc succeeded");

    for (i = 0; i < correct_buf_size / sizeof(*correct_buf); i++)
        correct_buf[i] = (C_DATATYPE) ((i % (hsize_t) mpi_size) + (i / (hsize_t) mpi_size));

    if (MAINPROCESS) {
        plist_id = H5Pcreate(H5P_FILE_ACCESS);
        VRFY((plist_id >= 0), "FAPL creation succeeded");

        VRFY((H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0),
                "Set libver bounds succeeded");

        file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, plist_id);
        VRFY((file_id >= 0), "Test file open succeeded");

        VRFY((H5Pclose(plist_id) >= 0), "FAPL close succeeded");

        /* Create the dataspace for the dataset */
        filespace = H5Screate_simple(READ_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_DIMS, dataset_dims, NULL);
        VRFY((filespace >= 0), "File dataspace creation succeeded");

        /* Create chunked dataset */
        chunk_dims[0] = (hsize_t) READ_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_CH_NROWS;
        chunk_dims[1] = (hsize_t) READ_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_CH_NCOLS;
        chunk_dims[2] = 1;

        plist_id = H5Pcreate(H5P_DATASET_CREATE);
        VRFY((plist_id >= 0), "DCPL creation succeeded");

        VRFY((H5Pset_chunk(plist_id, READ_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_DIMS, chunk_dims) >= 0),
                "Chunk size set");

        /* Add test filter to the pipeline */
        VRFY((set_dcpl_filter(plist_id) >= 0), "Filter set");

        dset_id = H5Dcreate2(file_id, READ_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_NAME, HDF5_DATATYPE_NAME, filespace,
                H5P_DEFAULT, plist_id, H5P_DEFAULT);
        VRFY((dset_id >= 0), "Dataset creation succeeded");

        VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
        VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

        VRFY((H5Dwrite(dset_id, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, H5P_DEFAULT, correct_buf) >= 0),
                "Dataset write succeeded");

        VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
        VRFY((H5Fclose(file_id) >= 0), "File close succeeded");
    }

    /* Set up file access property list with parallel I/O access */
    plist_id = H5Pcreate(H5P_FILE_ACCESS);
    VRFY((plist_id >= 0), "FAPL creation succeeded");

    VRFY((H5Pset_fapl_mpio(plist_id, comm, info) >= 0),
            "Set FAPL MPIO succeeded");

    VRFY((H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0),
            "Set libver bounds succeeded");

    file_id = H5Fopen(filenames[0], H5F_ACC_RDONLY, plist_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    VRFY((H5Pclose(plist_id) >= 0), "FAPL close succeeded");

    dset_id = H5Dopen2(file_id, "/" READ_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_NAME, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    sel_dims[0] = (hsize_t) READ_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_NROWS;
    sel_dims[1] = (hsize_t) READ_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_NCOLS;
    sel_dims[2] = 1;

    /* Setup one-dimensional memory dataspace for reading the dataset data into a contiguous buffer */
    flat_dims[0] = sel_dims[0] * sel_dims[1] * sel_dims[2];

    memspace = H5Screate_simple(1, flat_dims, NULL);
    VRFY((memspace >= 0), "Memory dataspace creation succeeded");

    /* Select hyperslab in the file */
    filespace = H5Dget_space(dset_id);
    VRFY((filespace >= 0), "File dataspace retrieval succeeded");

    /*
     * Each process defines the dataset selection in the file and
     * reads it to the selection in memory
     */
    count[0] = (hsize_t) READ_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_NROWS / (hsize_t) READ_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_CH_NROWS;
    count[1] = (hsize_t) READ_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_NCOLS / (hsize_t) READ_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_CH_NCOLS;
    count[2] = 1;
    stride[0] = (hsize_t) READ_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_CH_NROWS;
    stride[1] = (hsize_t) READ_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_CH_NCOLS;
    stride[2] = 1;
    block[0] = (hsize_t) READ_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_CH_NROWS;
    block[1] = (hsize_t) READ_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_CH_NCOLS;
    block[2] = 1;
    start[0] = 0;
    start[1] = 0;
    start[2] = (hsize_t) mpi_rank;

    if (VERBOSE_MED) {
        HDprintf("Process %d is reading with count[ %llu, %llu ], stride[ %llu, %llu ], start[ %llu, %llu ], block size[ %llu, %llu ]\n",
                mpi_rank, count[0], count[1], stride[0], stride[1], start[0], start[1], block[0], block[1]);
        HDfflush(stdout);
    }

    VRFY((H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, stride, count, block) >= 0),
            "Hyperslab selection succeeded");

    /* Create property list for collective dataset read */
    plist_id = H5Pcreate(H5P_DATASET_XFER);
    VRFY((plist_id >= 0), "DXPL creation succeeded");

    VRFY((H5Pset_dxpl_mpio(plist_id, H5FD_MPIO_COLLECTIVE) >= 0),
            "Set DXPL MPIO succeeded");

    read_buf_size = flat_dims[0] * sizeof(*read_buf);

    read_buf = (C_DATATYPE *) HDcalloc(1, read_buf_size);
    VRFY((NULL != read_buf), "HDcalloc succeeded");

    VRFY((H5Dread(dset_id, HDF5_DATATYPE_NAME, memspace, filespace, plist_id, read_buf) >= 0),
            "Dataset read succeeded");

    global_buf = (C_DATATYPE *) HDcalloc(1, correct_buf_size);
    VRFY((NULL != global_buf), "HDcalloc succeeded");

    /*
     * Due to the nature of 3-dimensional reading, create an MPI vector type that allows each
     * rank to write to the nth position of the global data buffer, where n is the rank number.
     */
    VRFY((MPI_SUCCESS == MPI_Type_vector((int) flat_dims[0], 1, mpi_size, C_DATATYPE_MPI, &vector_type)),
            "MPI_Type_vector succeeded");
    VRFY((MPI_SUCCESS == MPI_Type_commit(&vector_type)), "MPI_Type_commit succeeded");

    /*
     * Resize the type to allow interleaving,
     * so make it only one MPI_LONG wide
     */
    VRFY((MPI_SUCCESS == MPI_Type_create_resized(vector_type, 0, sizeof(long), &resized_vector_type)),
            "MPI_Type_create_resized");
    VRFY((MPI_SUCCESS == MPI_Type_commit(&resized_vector_type)), "MPI_Type_commit succeeded");

    VRFY((MPI_SUCCESS == MPI_Allgather(read_buf, (int) flat_dims[0], C_DATATYPE_MPI, global_buf, 1, resized_vector_type, comm)),
            "MPI_Allgather succeeded");

    VRFY((0 == HDmemcmp(global_buf, correct_buf, correct_buf_size)),
            "Data verification succeeded");

    VRFY((MPI_SUCCESS == MPI_Type_free(&vector_type)), "MPI_Type_free succeeded");
    VRFY((MPI_SUCCESS == MPI_Type_free(&resized_vector_type)), "MPI_Type_free succeeded");

    if (global_buf) HDfree(global_buf);
    if (read_buf) HDfree(read_buf);
    if (correct_buf) HDfree(correct_buf);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Sclose(memspace) >= 0), "Memory dataspace close succeeded");
    VRFY((H5Pclose(plist_id) >= 0), "DXPL close succeeded");
    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    return;
}

/*
 * Tests parallel read of filtered data in the case where
 * the dataset has 3 dimensions and each process reads from
 * each "page" in the 3rd dimension. However, no chunk on a
 * given "page" is read from by more than one process.
 *
 * The MAINPROCESS rank will first write out all of the
 * data to the dataset. Then, each rank reads a part of
 * each "page" of the dataset and contributes its piece to a
 * global buffer that is checked for consistency.
 *
 * Programmer: Jordan Henderson
 *             05/16/2018
 */
static void
test_read_3d_filtered_dataset_no_overlap_same_pages(void)
{
    C_DATATYPE *read_buf = NULL;
    C_DATATYPE *correct_buf = NULL;
    C_DATATYPE *global_buf = NULL;
    hsize_t     dataset_dims[READ_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_DIMS];
    hsize_t     chunk_dims[READ_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_DIMS];
    hsize_t     sel_dims[READ_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_DIMS];
    hsize_t     start[READ_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_DIMS];
    hsize_t     stride[READ_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_DIMS];
    hsize_t     count[READ_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_DIMS];
    hsize_t     block[READ_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_DIMS];
    hsize_t     flat_dims[1];
    size_t      i, read_buf_size, correct_buf_size;
    hid_t       file_id, dset_id, plist_id;
    hid_t       filespace, memspace;
    int        *recvcounts = NULL;
    int        *displs = NULL;

    if (MAINPROCESS) HDputs("Testing read from unshared filtered chunks on the same pages in 3D dataset");

    CHECK_CUR_FILTER_AVAIL();

    dataset_dims[0] = (hsize_t) READ_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_NROWS;
    dataset_dims[1] = (hsize_t) READ_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_NCOLS;
    dataset_dims[2] = (hsize_t) READ_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DEPTH;

    /* Setup the buffer for writing and for comparison */
    correct_buf_size = dataset_dims[0] * dataset_dims[1] * dataset_dims[2] * sizeof(*correct_buf);

    correct_buf = (C_DATATYPE *) HDcalloc(1, correct_buf_size);
    VRFY((NULL != correct_buf), "HDcalloc succeeded");

    for (i = 0; i < correct_buf_size / sizeof(*correct_buf); i++)
        correct_buf[i] = (C_DATATYPE) (
                                         (i % (dataset_dims[0] * dataset_dims[1]))
                                       + (i / (dataset_dims[0] * dataset_dims[1]))
                                      );

    if (MAINPROCESS) {
        plist_id = H5Pcreate(H5P_FILE_ACCESS);
        VRFY((plist_id >= 0), "FAPL creation succeeded");

        VRFY((H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0),
                "Set libver bounds succeeded");

        file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, plist_id);
        VRFY((file_id >= 0), "Test file open succeeded");

        VRFY((H5Pclose(plist_id) >= 0), "FAPL close succeeded");

        /* Create the dataspace for the dataset */
        filespace = H5Screate_simple(READ_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_DIMS, dataset_dims, NULL);
        VRFY((filespace >= 0), "File dataspace creation succeeded");

        /* Create chunked dataset */
        chunk_dims[0] = (hsize_t) READ_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_CH_NROWS;
        chunk_dims[1] = (hsize_t) READ_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_CH_NCOLS;
        chunk_dims[2] = 1;

        plist_id = H5Pcreate(H5P_DATASET_CREATE);
        VRFY((plist_id >= 0), "DCPL creation succeeded");

        VRFY((H5Pset_chunk(plist_id, READ_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_DIMS, chunk_dims) >= 0),
                "Chunk size set");

        /* Add test filter to the pipeline */
        VRFY((set_dcpl_filter(plist_id) >= 0), "Filter set");

        dset_id = H5Dcreate2(file_id, READ_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_NAME, HDF5_DATATYPE_NAME, filespace,
                H5P_DEFAULT, plist_id, H5P_DEFAULT);
        VRFY((dset_id >= 0), "Dataset creation succeeded");

        VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
        VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

        VRFY((H5Dwrite(dset_id, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, H5P_DEFAULT, correct_buf) >= 0),
                "Dataset write succeeded");

        VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
        VRFY((H5Fclose(file_id) >= 0), "File close succeeded");
    }

    /* Set up file access property list with parallel I/O access */
    plist_id = H5Pcreate(H5P_FILE_ACCESS);
    VRFY((plist_id >= 0), "FAPL creation succeeded");

    VRFY((H5Pset_fapl_mpio(plist_id, comm, info) >= 0),
            "Set FAPL MPIO succeeded");

    VRFY((H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0),
            "Set libver bounds succeeded");

    file_id = H5Fopen(filenames[0], H5F_ACC_RDONLY, plist_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    VRFY((H5Pclose(plist_id) >= 0), "FAPL close succeeded");

    dset_id = H5Dopen2(file_id, "/" READ_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_NAME, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    sel_dims[0] = (hsize_t) READ_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_CH_NROWS;
    sel_dims[1] = (hsize_t) READ_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_NCOLS;
    sel_dims[2] = (hsize_t) READ_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DEPTH;

    /* Setup one-dimensional memory dataspace for reading the dataset data into a contiguous buffer */
    flat_dims[0] = sel_dims[0] * sel_dims[1] * sel_dims[2];

    memspace = H5Screate_simple(1, flat_dims, NULL);
    VRFY((memspace >= 0), "Memory dataspace creation succeeded");

    /* Select hyperslab in the file */
    filespace = H5Dget_space(dset_id);
    VRFY((filespace >= 0), "File dataspace retrieval succeeded");

    /*
     * Each process defines the dataset selection in the file and
     * reads it to the selection in memory
     */
    count[0] = 1;
    count[1] = (hsize_t) READ_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_NCOLS / (hsize_t) READ_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_CH_NCOLS;
    count[2] = (hsize_t) mpi_size;
    stride[0] = (hsize_t) READ_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_CH_NROWS;
    stride[1] = (hsize_t) READ_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_CH_NCOLS;
    stride[2] = 1;
    block[0] = (hsize_t) READ_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_CH_NROWS;
    block[1] = (hsize_t) READ_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_CH_NCOLS;
    block[2] = 1;
    start[0] = ((hsize_t) mpi_rank * (hsize_t) READ_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_CH_NROWS * count[0]);
    start[1] = 0;
    start[2] = 0;

    if (VERBOSE_MED) {
        HDprintf("Process %d is reading with count[ %llu, %llu ], stride[ %llu, %llu ], start[ %llu, %llu ], block size[ %llu, %llu ]\n",
                mpi_rank, count[0], count[1], stride[0], stride[1], start[0], start[1], block[0], block[1]);
        HDfflush(stdout);
    }

    VRFY((H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, stride, count, block) >= 0),
            "Hyperslab selection succeeded");

    /* Create property list for collective dataset read */
    plist_id = H5Pcreate(H5P_DATASET_XFER);
    VRFY((plist_id >= 0), "DXPL creation succeeded");

    VRFY((H5Pset_dxpl_mpio(plist_id, H5FD_MPIO_COLLECTIVE) >= 0),
            "Set DXPL MPIO succeeded");

    read_buf_size = flat_dims[0] * sizeof(*read_buf);

    read_buf = (C_DATATYPE *) HDcalloc(1, read_buf_size);
    VRFY((NULL != read_buf), "HDcalloc succeeded");

    VRFY((H5Dread(dset_id, HDF5_DATATYPE_NAME, memspace, filespace, plist_id, read_buf) >= 0),
            "Dataset read succeeded");

    global_buf = (C_DATATYPE *) HDcalloc(1, correct_buf_size);
    VRFY((NULL != global_buf), "HDcalloc succeeded");

    /* Collect each piece of data from all ranks into a global buffer on all ranks */
    recvcounts = (int *) HDcalloc(1, (size_t) mpi_size * sizeof(*recvcounts));
    VRFY((NULL != recvcounts), "HDcalloc succeeded");

    for (i = 0; i < (size_t) mpi_size; i++)
        recvcounts[i] = (int) flat_dims[0];

    displs = (int *) HDcalloc(1, (size_t) mpi_size * sizeof(*displs));
    VRFY((NULL != displs), "HDcalloc succeeded");

    for (i = 0; i < (size_t) mpi_size; i++)
        displs[i] = (int) (i * flat_dims[0]);

    VRFY((MPI_SUCCESS == MPI_Allgatherv(read_buf, (int) flat_dims[0], C_DATATYPE_MPI, global_buf, recvcounts, displs, C_DATATYPE_MPI, comm)),
            "MPI_Allgatherv succeeded");

    VRFY((0 == HDmemcmp(global_buf, correct_buf, correct_buf_size)),
            "Data verification succeeded");

    if (displs) HDfree(displs);
    if (recvcounts) HDfree(recvcounts);
    if (global_buf) HDfree(global_buf);
    if (read_buf) HDfree(read_buf);
    if (correct_buf) HDfree(correct_buf);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Sclose(memspace) >= 0), "Memory dataspace close succeeded");
    VRFY((H5Pclose(plist_id) >= 0), "DXPL close succeeded");
    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    return;
}

/*
 * Tests parallel read of filtered data in the case where
 * the dataset has 3 dimensions and each process reads from
 * each "page" in the 3rd dimension. Further, each chunk in
 * each "page" is read from equally by all processes.
 *
 * The MAINPROCESS rank will first write out all of the
 * data to the dataset. Then, each rank reads part of each
 * chunk of each "page" and contributes its pieces to a
 * global buffer that is checked for consistency.
 *
 * Programmer: Jordan Henderson
 *             05/16/2018
 */
static void
test_read_3d_filtered_dataset_overlap(void)
{
    MPI_Datatype  vector_type;
    MPI_Datatype  resized_vector_type;
    C_DATATYPE   *read_buf = NULL;
    C_DATATYPE   *correct_buf = NULL;
    C_DATATYPE   *global_buf = NULL;
    hsize_t       dataset_dims[READ_SHARED_FILTERED_CHUNKS_3D_DATASET_DIMS];
    hsize_t       chunk_dims[READ_SHARED_FILTERED_CHUNKS_3D_DATASET_DIMS];
    hsize_t       sel_dims[READ_SHARED_FILTERED_CHUNKS_3D_DATASET_DIMS];
    hsize_t       start[READ_SHARED_FILTERED_CHUNKS_3D_DATASET_DIMS];
    hsize_t       stride[READ_SHARED_FILTERED_CHUNKS_3D_DATASET_DIMS];
    hsize_t       count[READ_SHARED_FILTERED_CHUNKS_3D_DATASET_DIMS];
    hsize_t       block[READ_SHARED_FILTERED_CHUNKS_3D_DATASET_DIMS];
    hsize_t       flat_dims[1];
    size_t        i, read_buf_size, correct_buf_size;
    hid_t         file_id = -1, dset_id = -1, plist_id = -1;
    hid_t         filespace = -1, memspace = -1;

    if (MAINPROCESS) HDputs("Testing read from shared filtered chunks in 3D dataset");

    CHECK_CUR_FILTER_AVAIL();

    dataset_dims[0] = (hsize_t) READ_SHARED_FILTERED_CHUNKS_3D_NROWS;
    dataset_dims[1] = (hsize_t) READ_SHARED_FILTERED_CHUNKS_3D_NCOLS;
    dataset_dims[2] = (hsize_t) READ_SHARED_FILTERED_CHUNKS_3D_DEPTH;

    /* Setup the buffer for writing and for comparison */
    correct_buf_size = dataset_dims[0] * dataset_dims[1] * dataset_dims[2] * sizeof(*correct_buf);

    correct_buf = (C_DATATYPE *) HDcalloc(1, correct_buf_size);
    VRFY((NULL != correct_buf), "HDcalloc succeeded");

    for (i = 0; i < correct_buf_size / sizeof(*correct_buf); i++)
        /* Add the Column Index */
        correct_buf[i] =
                (C_DATATYPE) (
                                 (i % (hsize_t) (READ_SHARED_FILTERED_CHUNKS_3D_DEPTH * READ_SHARED_FILTERED_CHUNKS_3D_NCOLS))

                                 /* Add the Row Index */
                               + ((i % (hsize_t) (mpi_size * READ_SHARED_FILTERED_CHUNKS_3D_DEPTH * READ_SHARED_FILTERED_CHUNKS_3D_NCOLS))
                                 / (hsize_t) (READ_SHARED_FILTERED_CHUNKS_3D_DEPTH * READ_SHARED_FILTERED_CHUNKS_3D_NCOLS))

                                 /* Add the amount that gets added when a rank moves down to its next section vertically in the dataset */
                               + ((hsize_t) (READ_SHARED_FILTERED_CHUNKS_3D_DEPTH * READ_SHARED_FILTERED_CHUNKS_3D_NCOLS)
                                 * (i / (hsize_t) (mpi_size * READ_SHARED_FILTERED_CHUNKS_3D_DEPTH * READ_SHARED_FILTERED_CHUNKS_3D_NCOLS)))
                             );

    if (MAINPROCESS) {
        plist_id = H5Pcreate(H5P_FILE_ACCESS);
        VRFY((plist_id >= 0), "FAPL creation succeeded");

        VRFY((H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0),
                "Set libver bounds succeeded");

        file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, plist_id);
        VRFY((file_id >= 0), "Test file open succeeded");

        VRFY((H5Pclose(plist_id) >= 0), "FAPL close succeeded");

        /* Create the dataspace for the dataset */
        filespace = H5Screate_simple(READ_SHARED_FILTERED_CHUNKS_3D_DATASET_DIMS, dataset_dims, NULL);
        VRFY((filespace >= 0), "File dataspace creation succeeded");

        /* Create chunked dataset */
        chunk_dims[0] = (hsize_t) READ_SHARED_FILTERED_CHUNKS_3D_CH_NROWS;
        chunk_dims[1] = (hsize_t) READ_SHARED_FILTERED_CHUNKS_3D_CH_NCOLS;
        chunk_dims[2] = 1;

        plist_id = H5Pcreate(H5P_DATASET_CREATE);
        VRFY((plist_id >= 0), "DCPL creation succeeded");

        VRFY((H5Pset_chunk(plist_id, READ_SHARED_FILTERED_CHUNKS_3D_DATASET_DIMS, chunk_dims) >= 0),
                "Chunk size set");

        /* Add test filter to the pipeline */
        VRFY((set_dcpl_filter(plist_id) >= 0), "Filter set");

        dset_id = H5Dcreate2(file_id, READ_SHARED_FILTERED_CHUNKS_3D_DATASET_NAME, HDF5_DATATYPE_NAME, filespace,
                H5P_DEFAULT, plist_id, H5P_DEFAULT);
        VRFY((dset_id >= 0), "Dataset creation succeeded");

        VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
        VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

        VRFY((H5Dwrite(dset_id, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, H5P_DEFAULT, correct_buf) >= 0),
                "Dataset write succeeded");

        VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
        VRFY((H5Fclose(file_id) >= 0), "File close succeeded");
    }

    /* Set up file access property list with parallel I/O access */
    plist_id = H5Pcreate(H5P_FILE_ACCESS);
    VRFY((plist_id >= 0), "FAPL creation succeeded");

    VRFY((H5Pset_fapl_mpio(plist_id, comm, info) >= 0),
            "Set FAPL MPIO succeeded");

    VRFY((H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0),
            "Set libver bounds succeeded");

    file_id = H5Fopen(filenames[0], H5F_ACC_RDONLY, plist_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    VRFY((H5Pclose(plist_id) >= 0), "FAPL close succeeded");

    dset_id = H5Dopen2(file_id, "/" READ_SHARED_FILTERED_CHUNKS_3D_DATASET_NAME, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    sel_dims[0] = (hsize_t) (READ_SHARED_FILTERED_CHUNKS_3D_NROWS / mpi_size);
    sel_dims[1] = (hsize_t) READ_SHARED_FILTERED_CHUNKS_3D_NCOLS;
    sel_dims[2] = (hsize_t) READ_SHARED_FILTERED_CHUNKS_3D_DEPTH;

    /* Setup one-dimensional memory dataspace for reading the dataset data into a contiguous buffer */
    flat_dims[0] = sel_dims[0] * sel_dims[1] * sel_dims[2];

    memspace = H5Screate_simple(1, flat_dims, NULL);
    VRFY((memspace >= 0), "Memory dataspace creation succeeded");

    /* Select hyperslab in the file */
    filespace = H5Dget_space(dset_id);
    VRFY((filespace >= 0), "File dataspace retrieval succeeded");

    /*
     * Each process defines the dataset selection in the file and
     * reads it to the selection in memory
     */
    count[0] = (hsize_t) (READ_SHARED_FILTERED_CHUNKS_3D_NROWS / READ_SHARED_FILTERED_CHUNKS_3D_CH_NROWS);
    count[1] = (hsize_t) (READ_SHARED_FILTERED_CHUNKS_3D_NCOLS / READ_SHARED_FILTERED_CHUNKS_3D_CH_NCOLS);
    count[2] = (hsize_t) READ_SHARED_FILTERED_CHUNKS_3D_DEPTH;
    stride[0] = (hsize_t) READ_SHARED_FILTERED_CHUNKS_3D_CH_NROWS;
    stride[1] = (hsize_t) READ_SHARED_FILTERED_CHUNKS_3D_CH_NCOLS;
    stride[2] = 1;
    block[0] = 1;
    block[1] = (hsize_t) READ_SHARED_FILTERED_CHUNKS_3D_CH_NCOLS;
    block[2] = 1;
    start[0] = (hsize_t) mpi_rank;
    start[1] = 0;
    start[2] = 0;

    if (VERBOSE_MED) {
        HDprintf("Process %d is reading with count[ %llu, %llu ], stride[ %llu, %llu ], start[ %llu, %llu ], block size[ %llu, %llu ]\n",
                mpi_rank, count[0], count[1], stride[0], stride[1], start[0], start[1], block[0], block[1]);
        HDfflush(stdout);
    }

    VRFY((H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, stride, count, block) >= 0),
            "Hyperslab selection succeeded");

    /* Create property list for collective dataset read */
    plist_id = H5Pcreate(H5P_DATASET_XFER);
    VRFY((plist_id >= 0), "DXPL creation succeeded");

    VRFY((H5Pset_dxpl_mpio(plist_id, H5FD_MPIO_COLLECTIVE) >= 0),
            "Set DXPL MPIO succeeded");

    read_buf_size = flat_dims[0] * sizeof(*read_buf);

    read_buf = (C_DATATYPE *) HDcalloc(1, read_buf_size);
    VRFY((NULL != read_buf), "HDcalloc succeeded");

    VRFY((H5Dread(dset_id, HDF5_DATATYPE_NAME, memspace, filespace, plist_id, read_buf) >= 0),
            "Dataset read succeeded");

    global_buf = (C_DATATYPE *) HDcalloc(1, correct_buf_size);
    VRFY((NULL != global_buf), "HDcalloc succeeded");

    {
        size_t run_length = (size_t) (READ_SHARED_FILTERED_CHUNKS_3D_NCOLS * READ_SHARED_FILTERED_CHUNKS_3D_DEPTH);
        size_t num_blocks = (size_t) (READ_SHARED_FILTERED_CHUNKS_3D_NROWS / mpi_size);

        /*
         * Due to the nature of 3-dimensional reading, create an MPI vector type that allows each
         * rank to write to the nth position of the global data buffer, where n is the rank number.
         */
        VRFY((MPI_SUCCESS == MPI_Type_vector((int) num_blocks, (int) run_length, (int) (mpi_size * (int) run_length), C_DATATYPE_MPI, &vector_type)),
                "MPI_Type_vector succeeded");
        VRFY((MPI_SUCCESS == MPI_Type_commit(&vector_type)), "MPI_Type_commit succeeded");

        /*
         * Resize the type to allow interleaving,
         * so make it "run_length" MPI_LONGs wide
         */
        VRFY((MPI_SUCCESS == MPI_Type_create_resized(vector_type, 0, (MPI_Aint) (run_length * sizeof(long)), &resized_vector_type)),
                "MPI_Type_create_resized");
        VRFY((MPI_SUCCESS == MPI_Type_commit(&resized_vector_type)), "MPI_Type_commit succeeded");
    }

    VRFY((MPI_SUCCESS == MPI_Allgather(read_buf, (int) flat_dims[0], C_DATATYPE_MPI, global_buf, 1, resized_vector_type, comm)),
            "MPI_Allgatherv succeeded");

    VRFY((0 == HDmemcmp(global_buf, correct_buf, correct_buf_size)),
            "Data verification succeeded");

    VRFY((MPI_SUCCESS == MPI_Type_free(&vector_type)), "MPI_Type_free succeeded");
    VRFY((MPI_SUCCESS == MPI_Type_free(&resized_vector_type)), "MPI_Type_free succeeded");

    if (global_buf) HDfree(global_buf);
    if (read_buf) HDfree(read_buf);
    if (correct_buf) HDfree(correct_buf);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Sclose(memspace) >= 0), "Memory dataspace close succeeded");
    VRFY((H5Pclose(plist_id) >= 0), "DXPL close succeeded");
    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    return;
}

/*
 * Tests parallel read of filtered data to unshared
 * chunks using a compound datatype which doesn't
 * require a datatype conversion.
 *
 * The MAINPROCESS rank will first write out all of the
 * data to the dataset. Then, each rank reads a part of
 * the dataset and contributes its piece to a global
 * buffer that is checked for consistency.
 *
 * Programmer: Jordan Henderson
 *             05/17/2018
 */
static void
test_read_cmpd_filtered_dataset_no_conversion_unshared(void)
{
    COMPOUND_C_DATATYPE *read_buf = NULL;
    COMPOUND_C_DATATYPE *correct_buf = NULL;
    COMPOUND_C_DATATYPE *global_buf = NULL;
    hsize_t              dataset_dims[READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t              chunk_dims[READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t              sel_dims[READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t              start[READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t              stride[READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t              count[READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t              block[READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t              flat_dims[1];
    size_t               i, read_buf_size, correct_buf_size;
    hid_t                file_id = -1, dset_id = -1, plist_id = -1, memtype = -1;
    hid_t                filespace = -1, memspace = -1;
    int                 *recvcounts = NULL;
    int                 *displs = NULL;

    if (MAINPROCESS) HDputs("Testing read from unshared filtered chunks in Compound Datatype dataset without Datatype conversion");

    CHECK_CUR_FILTER_AVAIL();

    dataset_dims[0] = (hsize_t) READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_NROWS;
    dataset_dims[1] = (hsize_t) READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_NCOLS;

    /* Setup the buffer for writing and for comparison */
    correct_buf_size = dataset_dims[0] * dataset_dims[1] * sizeof(*correct_buf);

    correct_buf = (COMPOUND_C_DATATYPE *) HDcalloc(1, correct_buf_size);
    VRFY((NULL != correct_buf), "HDcalloc succeeded");

    for (i = 0; i < correct_buf_size / sizeof(*correct_buf); i++) {
        correct_buf[i].field1 = (short) (
                                            (i % dataset_dims[1])
                                          + (i / dataset_dims[1])
                                        );

        correct_buf[i].field2 = (int) (
                                          (i % dataset_dims[1])
                                        + (i / dataset_dims[1])
                                      );

        correct_buf[i].field3 = (long) (
                                           (i % dataset_dims[1])
                                         + (i / dataset_dims[1])
                                       );
    }

    /* Create the compound type for memory. */
    memtype = H5Tcreate(H5T_COMPOUND, sizeof(COMPOUND_C_DATATYPE));
    VRFY((memtype >= 0), "Datatype creation succeeded");

    VRFY((H5Tinsert(memtype, "ShortData", HOFFSET(COMPOUND_C_DATATYPE, field1), H5T_NATIVE_SHORT) >= 0),
            "Datatype insertion succeeded");
    VRFY((H5Tinsert(memtype, "IntData", HOFFSET(COMPOUND_C_DATATYPE, field2), H5T_NATIVE_INT) >= 0),
            "Datatype insertion succeeded");
    VRFY((H5Tinsert(memtype, "LongData", HOFFSET(COMPOUND_C_DATATYPE, field3), H5T_NATIVE_LONG) >= 0),
            "Datatype insertion succeeded");

    if (MAINPROCESS) {
        plist_id = H5Pcreate(H5P_FILE_ACCESS);
        VRFY((plist_id >= 0), "FAPL creation succeeded");

        VRFY((H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0),
                "Set libver bounds succeeded");

        file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, plist_id);
        VRFY((file_id >= 0), "Test file open succeeded");

        VRFY((H5Pclose(plist_id) >= 0), "FAPL close succeeded");

        /* Create the dataspace for the dataset */
        filespace = H5Screate_simple(READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_DIMS, dataset_dims, NULL);
        VRFY((filespace >= 0), "File dataspace creation succeeded");

        /* Create chunked dataset */
        chunk_dims[0] = READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_CH_NROWS;
        chunk_dims[1] = READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_CH_NCOLS;

        plist_id = H5Pcreate(H5P_DATASET_CREATE);
        VRFY((plist_id >= 0), "DCPL creation succeeded");

        VRFY((H5Pset_chunk(plist_id, READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_DIMS, chunk_dims) >= 0),
                "Chunk size set");

        /* Add test filter to the pipeline */
        VRFY((set_dcpl_filter(plist_id) >= 0), "Filter set");

        dset_id = H5Dcreate2(file_id, READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_NAME, memtype, filespace,
                H5P_DEFAULT, plist_id, H5P_DEFAULT);
        VRFY((dset_id >= 0), "Dataset creation succeeded");

        VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
        VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

        VRFY((H5Dwrite(dset_id, memtype, H5S_ALL, H5S_ALL, H5P_DEFAULT, correct_buf) >= 0),
                "Dataset write succeeded");

        VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
        VRFY((H5Fclose(file_id) >= 0), "File close succeeded");
    }

    /* Set up file access property list with parallel I/O access */
    plist_id = H5Pcreate(H5P_FILE_ACCESS);
    VRFY((plist_id >= 0), "FAPL creation succeeded");

    VRFY((H5Pset_fapl_mpio(plist_id, comm, info) >= 0),
            "Set FAPL MPIO succeeded");

    VRFY((H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0),
            "Set libver bounds succeeded");

    file_id = H5Fopen(filenames[0], H5F_ACC_RDONLY, plist_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    VRFY((H5Pclose(plist_id) >= 0), "FAPL close succeeded");

    dset_id = H5Dopen2(file_id, "/" READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_NAME, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    sel_dims[0] = (hsize_t) READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_CH_NROWS;
    sel_dims[1] = (hsize_t) READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_ENTRIES_PER_PROC;

    /* Setup one-dimensional memory dataspace for reading the dataset data into a contiguous buffer */
    flat_dims[0] = sel_dims[0] * sel_dims[1];

    memspace = H5Screate_simple(1, flat_dims, NULL);
    VRFY((memspace >= 0), "Memory dataspace creation succeeded");

    /* Select hyperslab in the file */
    filespace = H5Dget_space(dset_id);
    VRFY((filespace >= 0), "File dataspace retrieval succeeded");

    /*
     * Each process defines the dataset selection in the file and
     * reads it to the selection in memory
     */
    count[0] = 1;
    count[1] = (hsize_t) READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_ENTRIES_PER_PROC;
    stride[0] = READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_CH_NROWS;
    stride[1] = READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_CH_NCOLS;
    block[0] = READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_CH_NROWS;
    block[1] = READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_CH_NCOLS;
    start[0] = 0;
    start[1] = ((hsize_t) mpi_rank * READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_CH_NCOLS);

    if (VERBOSE_MED) {
        HDprintf("Process %d is reading with count[ %llu, %llu ], stride[ %llu, %llu ], start[ %llu, %llu ], block size[ %llu, %llu ]\n",
                mpi_rank, count[0], count[1], stride[0], stride[1], start[0], start[1], block[0], block[1]);
        HDfflush(stdout);
    }

    VRFY((H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, stride, count, block) >= 0),
            "Hyperslab selection succeeded");

    /* Create property list for collective dataset read */
    plist_id = H5Pcreate(H5P_DATASET_XFER);
    VRFY((plist_id >= 0), "DXPL creation succeeded");

    VRFY((H5Pset_dxpl_mpio(plist_id, H5FD_MPIO_COLLECTIVE) >= 0),
            "Set DXPL MPIO succeeded");

    read_buf_size = flat_dims[0] * sizeof(*read_buf);

    read_buf = (COMPOUND_C_DATATYPE *) HDcalloc(1, read_buf_size);
    VRFY((NULL != read_buf), "HDcalloc succeeded");

    VRFY((H5Dread(dset_id, memtype, memspace, filespace, plist_id, read_buf) >= 0),
            "Dataset read succeeded");

    global_buf = (COMPOUND_C_DATATYPE *) HDcalloc(1, correct_buf_size);
    VRFY((NULL != global_buf), "HDcalloc succeeded");

    /* Collect each piece of data from all ranks into a global buffer on all ranks */
    recvcounts = (int *) HDcalloc(1, (size_t) mpi_size * sizeof(*recvcounts));
    VRFY((NULL != recvcounts), "HDcalloc succeeded");

    for (i = 0; i < (size_t) mpi_size; i++)
        recvcounts[i] = (int) (flat_dims[0] * sizeof(*read_buf));

    displs = (int *) HDcalloc(1, (size_t) mpi_size * sizeof(*displs));
    VRFY((NULL != displs), "HDcalloc succeeded");

    for (i = 0; i < (size_t) mpi_size; i++)
        displs[i] = (int) (i * flat_dims[0] * sizeof(*read_buf));

    VRFY((MPI_SUCCESS == MPI_Allgatherv(read_buf, (int) (flat_dims[0] * sizeof(COMPOUND_C_DATATYPE)), MPI_BYTE, global_buf, recvcounts, displs, MPI_BYTE, comm)),
            "MPI_Allgatherv succeeded");

    VRFY((0 == HDmemcmp(global_buf, correct_buf, correct_buf_size)),
            "Data verification succeeded");

    if (displs) HDfree(displs);
    if (recvcounts) HDfree(recvcounts);
    if (global_buf) HDfree(global_buf);
    if (read_buf) HDfree(read_buf);
    if (correct_buf) HDfree(correct_buf);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Sclose(memspace) >= 0), "Memory dataspace close succeeded");
    VRFY((H5Tclose(memtype) >= 0), "Memory datatype close succeeded");
    VRFY((H5Pclose(plist_id) >= 0), "DXPL close succeeded");
    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    return;
}

/*
 * Tests parallel read of filtered data from shared
 * chunks using a compound datatype which doesn't
 * require a datatype conversion.
 *
 * The MAINPROCESS rank will first write out all of the
 * data to the dataset. Then, each rank reads a part of
 * each chunk of the dataset and contributes its piece
 * to a global buffer that is checked for consistency.
 *
 * Programmer: Jordan Henderson
 *             05/17/2018
 */
static void
test_read_cmpd_filtered_dataset_no_conversion_shared(void)
{
    COMPOUND_C_DATATYPE *read_buf = NULL;
    COMPOUND_C_DATATYPE *correct_buf = NULL;
    COMPOUND_C_DATATYPE *global_buf = NULL;
    hsize_t              dataset_dims[READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t              chunk_dims[READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t              sel_dims[READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t              start[READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t              stride[READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t              count[READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t              block[READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t              flat_dims[1];
    size_t               i, read_buf_size, correct_buf_size;
    hid_t                file_id, dset_id, plist_id, memtype;
    hid_t                filespace, memspace;
    int                 *recvcounts = NULL;
    int                 *displs = NULL;

    if (MAINPROCESS) HDputs("Testing read from shared filtered chunks in Compound Datatype dataset without Datatype conversion");

    CHECK_CUR_FILTER_AVAIL();

    dataset_dims[0] = (hsize_t) READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_NROWS;
    dataset_dims[1] = (hsize_t) READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_NCOLS;

    /* Setup the buffer for writing and for comparison */
    correct_buf_size = dataset_dims[0] * dataset_dims[1] * sizeof(*correct_buf);

    correct_buf = (COMPOUND_C_DATATYPE *) HDcalloc(1, correct_buf_size);
    VRFY((NULL != correct_buf), "HDcalloc succeeded");

    for (i = 0; i < correct_buf_size / sizeof(*correct_buf); i++) {
        correct_buf[i].field1 = (short) (
                                            (dataset_dims[1] * (i / ((hsize_t) mpi_size * dataset_dims[1])))
                                          + (i % dataset_dims[1])
                                          + (((i % ((hsize_t) mpi_size * dataset_dims[1])) / dataset_dims[1]) % dataset_dims[1])
                                        );

        correct_buf[i].field2 = (int) (
                                          (dataset_dims[1] * (i / ((hsize_t) mpi_size * dataset_dims[1])))
                                        + (i % dataset_dims[1])
                                        + (((i % ((hsize_t) mpi_size * dataset_dims[1])) / dataset_dims[1]) % dataset_dims[1])
                                      );

        correct_buf[i].field3 = (long) (
                                           (dataset_dims[1] * (i / ((hsize_t) mpi_size * dataset_dims[1])))
                                         + (i % dataset_dims[1])
                                         + (((i % ((hsize_t) mpi_size * dataset_dims[1])) / dataset_dims[1]) % dataset_dims[1])
                                       );
    }

    /* Create the compound type for memory. */
    memtype = H5Tcreate(H5T_COMPOUND, sizeof(COMPOUND_C_DATATYPE));
    VRFY((memtype >= 0), "Datatype creation succeeded");

    VRFY((H5Tinsert(memtype, "ShortData", HOFFSET(COMPOUND_C_DATATYPE, field1), H5T_NATIVE_SHORT) >= 0),
            "Datatype insertion succeeded");
    VRFY((H5Tinsert(memtype, "IntData", HOFFSET(COMPOUND_C_DATATYPE, field2), H5T_NATIVE_INT) >= 0),
            "Datatype insertion succeeded");
    VRFY((H5Tinsert(memtype, "LongData", HOFFSET(COMPOUND_C_DATATYPE, field3), H5T_NATIVE_LONG) >= 0),
            "Datatype insertion succeeded");

    if (MAINPROCESS) {
        plist_id = H5Pcreate(H5P_FILE_ACCESS);
        VRFY((plist_id >= 0), "FAPL creation succeeded");

        VRFY((H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0),
                "Set libver bounds succeeded");

        file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, plist_id);
        VRFY((file_id >= 0), "Test file open succeeded");

        VRFY((H5Pclose(plist_id) >= 0), "FAPL close succeeded");

        /* Create the dataspace for the dataset */
        filespace = H5Screate_simple(READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_DIMS, dataset_dims, NULL);
        VRFY((filespace >= 0), "File dataspace creation succeeded");

        /* Create chunked dataset */
        chunk_dims[0] = (hsize_t) READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_CH_NROWS;
        chunk_dims[1] = (hsize_t) READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_CH_NCOLS;

        plist_id = H5Pcreate(H5P_DATASET_CREATE);
        VRFY((plist_id >= 0), "DCPL creation succeeded");

        VRFY((H5Pset_chunk(plist_id, READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_DIMS, chunk_dims) >= 0),
                "Chunk size set");

        /* Add test filter to the pipeline */
        VRFY((set_dcpl_filter(plist_id) >= 0), "Filter set");

        dset_id = H5Dcreate2(file_id, READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_NAME, memtype, filespace,
                H5P_DEFAULT, plist_id, H5P_DEFAULT);
        VRFY((dset_id >= 0), "Dataset creation succeeded");

        VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
        VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

        VRFY((H5Dwrite(dset_id, memtype, H5S_ALL, H5S_ALL, H5P_DEFAULT, correct_buf) >= 0),
                "Dataset write succeeded");

        VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
        VRFY((H5Fclose(file_id) >= 0), "File close succeeded");
    }

    /* Set up file access property list with parallel I/O access */
    plist_id = H5Pcreate(H5P_FILE_ACCESS);
    VRFY((plist_id >= 0), "FAPL creation succeeded");

    VRFY((H5Pset_fapl_mpio(plist_id, comm, info) >= 0),
            "Set FAPL MPIO succeeded");

    VRFY((H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0),
            "Set libver bounds succeeded");

    file_id = H5Fopen(filenames[0], H5F_ACC_RDONLY, plist_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    VRFY((H5Pclose(plist_id) >= 0), "FAPL close succeeded");

    dset_id = H5Dopen2(file_id, "/" READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_NAME, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    sel_dims[0] = (hsize_t) READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_CH_NROWS / (hsize_t) mpi_size;
    sel_dims[1] = (hsize_t) READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_ENTRIES_PER_PROC;

    /* Setup one-dimensional memory dataspace for reading the dataset data into a contiguous buffer */
    flat_dims[0] = sel_dims[0] * sel_dims[1];

    memspace = H5Screate_simple(1, flat_dims, NULL);
    VRFY((memspace >= 0), "Memory dataspace creation succeeded");

    /* Select hyperslab in the file */
    filespace = H5Dget_space(dset_id);
    VRFY((filespace >= 0), "File dataspace retrieval succeeded");

    /*
     * Each process defines the dataset selection in the file and
     * reads it to the selection in memory
     */
    count[0] = 1;
    count[1] = (hsize_t) READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_ENTRIES_PER_PROC;
    stride[0] = (hsize_t) READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_CH_NROWS;
    stride[1] = READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_CH_NCOLS;
    block[0] = (hsize_t) READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_CH_NROWS / (hsize_t) mpi_size;
    block[1] = READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_CH_NCOLS;
    start[0] = (hsize_t) mpi_rank;
    start[1] = 0;

    if (VERBOSE_MED) {
        HDprintf("Process %d is reading with count[ %llu, %llu ], stride[ %llu, %llu ], start[ %llu, %llu ], block size[ %llu, %llu ]\n",
                mpi_rank, count[0], count[1], stride[0], stride[1], start[0], start[1], block[0], block[1]);
        HDfflush(stdout);
    }

    VRFY((H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, stride, count, block) >= 0),
            "Hyperslab selection succeeded");

    /* Create property list for collective dataset read */
    plist_id = H5Pcreate(H5P_DATASET_XFER);
    VRFY((plist_id >= 0), "DXPL creation succeeded");

    VRFY((H5Pset_dxpl_mpio(plist_id, H5FD_MPIO_COLLECTIVE) >= 0),
            "Set DXPL MPIO succeeded");

    read_buf_size = flat_dims[0] * sizeof(*read_buf);

    read_buf = (COMPOUND_C_DATATYPE *) HDcalloc(1, read_buf_size);
    VRFY((NULL != read_buf), "HDcalloc succeeded");

    VRFY((H5Dread(dset_id, memtype, memspace, filespace, plist_id, read_buf) >= 0),
            "Dataset read succeeded");

    global_buf = (COMPOUND_C_DATATYPE *) HDcalloc(1, correct_buf_size);
    VRFY((NULL != global_buf), "HDcalloc succeeded");

    /* Collect each piece of data from all ranks into a global buffer on all ranks */
    recvcounts = (int *) HDcalloc(1, (size_t) mpi_size * sizeof(*recvcounts));
    VRFY((NULL != recvcounts), "HDcalloc succeeded");

    for (i = 0; i < (size_t) mpi_size; i++)
        recvcounts[i] = (int) (flat_dims[0] * sizeof(*read_buf));

    displs = (int *) HDcalloc(1, (size_t) mpi_size * sizeof(*displs));
    VRFY((NULL != displs), "HDcalloc succeeded");

    for (i = 0; i < (size_t) mpi_size; i++)
        displs[i] = (int) (i * flat_dims[0] * sizeof(*read_buf));

    VRFY((MPI_SUCCESS == MPI_Allgatherv(read_buf, (int) (flat_dims[0] * sizeof(COMPOUND_C_DATATYPE)), MPI_BYTE, global_buf, recvcounts, displs, MPI_BYTE, comm)),
            "MPI_Allgatherv succeeded");

    VRFY((0 == HDmemcmp(global_buf, correct_buf, correct_buf_size)),
            "Data verification succeeded");

    if (displs) HDfree(displs);
    if (recvcounts) HDfree(recvcounts);
    if (global_buf) HDfree(global_buf);
    if (read_buf) HDfree(read_buf);
    if (correct_buf) HDfree(correct_buf);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Sclose(memspace) >= 0), "Memory dataspace close succeeded");
    VRFY((H5Tclose(memtype) >= 0), "Memory datatype close succeeded");
    VRFY((H5Pclose(plist_id) >= 0), "DXPL close succeeded");
    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    return;
}

/*
 * Tests parallel read of filtered data from unshared
 * chunks using a compound datatype which requires a
 * datatype conversion.
 *
 * The MAINPROCESS rank will first write out all of the
 * data to the dataset. Then, each rank reads a part of
 * the dataset and contributes its piece to a global
 * buffer that is checked for consistency.
 *
 * Programmer: Jordan Henderson
 *             05/17/2018
 */
static void
test_read_cmpd_filtered_dataset_type_conversion_unshared(void)
{
    COMPOUND_C_DATATYPE *read_buf = NULL;
    COMPOUND_C_DATATYPE *correct_buf = NULL;
    COMPOUND_C_DATATYPE *global_buf = NULL;
    hsize_t              dataset_dims[READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t              chunk_dims[READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t              sel_dims[READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t              start[READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t              stride[READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t              count[READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t              block[READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t              flat_dims[1];
    size_t               i, read_buf_size, correct_buf_size;
    hid_t                file_id = -1, dset_id = -1, plist_id = -1, filetype = -1, memtype = -1;
    hid_t                filespace = -1, memspace = -1;
    int                 *recvcounts = NULL;
    int                 *displs = NULL;

    if (MAINPROCESS) HDputs("Testing read from unshared filtered chunks in Compound Datatype dataset with Datatype conversion");

    CHECK_CUR_FILTER_AVAIL();

    dataset_dims[0] = (hsize_t) READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_NROWS;
    dataset_dims[1] = (hsize_t) READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_NCOLS;

    /* Setup the buffer for writing and for comparison */
    correct_buf_size = dataset_dims[0] * dataset_dims[1] * sizeof(*correct_buf);

    correct_buf = (COMPOUND_C_DATATYPE *) HDcalloc(1, correct_buf_size);
    VRFY((NULL != correct_buf), "HDcalloc succeeded");

    for (i = 0; i < correct_buf_size / sizeof(*correct_buf); i++) {
        correct_buf[i].field1 = (short) (
                                            (i % dataset_dims[1])
                                          + (i / dataset_dims[1])
                                        );

        correct_buf[i].field2 = (int) (
                                          (i % dataset_dims[1])
                                        + (i / dataset_dims[1])
                                      );

        correct_buf[i].field3 = (long) (
                                           (i % dataset_dims[1])
                                         + (i / dataset_dims[1])
                                       );
    }

    /* Create the compound type for memory. */
    memtype = H5Tcreate(H5T_COMPOUND, sizeof(COMPOUND_C_DATATYPE));
    VRFY((memtype >= 0), "Datatype creation succeeded");

    VRFY((H5Tinsert(memtype, "ShortData", HOFFSET(COMPOUND_C_DATATYPE, field1), H5T_NATIVE_SHORT) >= 0),
            "Datatype insertion succeeded");
    VRFY((H5Tinsert(memtype, "IntData", HOFFSET(COMPOUND_C_DATATYPE, field2), H5T_NATIVE_INT) >= 0),
            "Datatype insertion succeeded");
    VRFY((H5Tinsert(memtype, "LongData", HOFFSET(COMPOUND_C_DATATYPE, field3), H5T_NATIVE_LONG) >= 0),
            "Datatype insertion succeeded");

    /* Create the compound type for file. */
    filetype = H5Tcreate(H5T_COMPOUND, 32);
    VRFY((filetype >= 0), "Datatype creation succeeded");

    VRFY((H5Tinsert(filetype, "ShortData", 0, H5T_STD_I64BE) >= 0),
            "Datatype insertion succeeded");
    VRFY((H5Tinsert(filetype, "IntData", 8, H5T_STD_I64BE) >= 0),
            "Datatype insertion succeeded");
    VRFY((H5Tinsert(filetype, "LongData", 16, H5T_STD_I64BE) >= 0),
            "Datatype insertion succeeded");

    if (MAINPROCESS) {
        plist_id = H5Pcreate(H5P_FILE_ACCESS);
        VRFY((plist_id >= 0), "FAPL creation succeeded");

        VRFY((H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0),
                "Set libver bounds succeeded");

        file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, plist_id);
        VRFY((file_id >= 0), "Test file open succeeded");

        VRFY((H5Pclose(plist_id) >= 0), "FAPL close succeeded");

        /* Create the dataspace for the dataset */
        filespace = H5Screate_simple(READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_DIMS, dataset_dims, NULL);
        VRFY((filespace >= 0), "File dataspace creation succeeded");

        /* Create chunked dataset */
        chunk_dims[0] = READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_CH_NROWS;
        chunk_dims[1] = READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_CH_NCOLS;

        plist_id = H5Pcreate(H5P_DATASET_CREATE);
        VRFY((plist_id >= 0), "DCPL creation succeeded");

        VRFY((H5Pset_chunk(plist_id, READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_DIMS, chunk_dims) >= 0),
                "Chunk size set");

        /* Add test filter to the pipeline */
        VRFY((set_dcpl_filter(plist_id) >= 0), "Filter set");

        dset_id = H5Dcreate2(file_id, READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_NAME, filetype, filespace,
                H5P_DEFAULT, plist_id, H5P_DEFAULT);
        VRFY((dset_id >= 0), "Dataset creation succeeded");

        VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
        VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

        VRFY((H5Dwrite(dset_id, memtype, H5S_ALL, H5S_ALL, H5P_DEFAULT, correct_buf) >= 0),
                "Dataset write succeeded");

        VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
        VRFY((H5Fclose(file_id) >= 0), "File close succeeded");
    }

    /* Set up file access property list with parallel I/O access */
    plist_id = H5Pcreate(H5P_FILE_ACCESS);
    VRFY((plist_id >= 0), "FAPL creation succeeded");

    VRFY((H5Pset_fapl_mpio(plist_id, comm, info) >= 0),
            "Set FAPL MPIO succeeded");

    VRFY((H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0),
            "Set libver bounds succeeded");

    file_id = H5Fopen(filenames[0], H5F_ACC_RDONLY, plist_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    VRFY((H5Pclose(plist_id) >= 0), "FAPL close succeeded");

    dset_id = H5Dopen2(file_id, "/" READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_NAME, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    sel_dims[0] = (hsize_t) READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_CH_NROWS;
    sel_dims[1] = (hsize_t) READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_ENTRIES_PER_PROC;

    /* Setup one-dimensional memory dataspace for reading the dataset data into a contiguous buffer */
    flat_dims[0] = sel_dims[0] * sel_dims[1];

    memspace = H5Screate_simple(1, flat_dims, NULL);
    VRFY((memspace >= 0), "Memory dataspace creation succeeded");

    /* Select hyperslab in the file */
    filespace = H5Dget_space(dset_id);
    VRFY((filespace >= 0), "File dataspace retrieval succeeded");

    /*
     * Each process defines the dataset selection in the file and
     * reads it to the selection in memory
     */
    count[0] = 1;
    count[1] = (hsize_t) READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_ENTRIES_PER_PROC;
    stride[0] = READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_CH_NROWS;
    stride[1] = READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_CH_NCOLS;
    block[0] = READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_CH_NROWS;
    block[1] = READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_CH_NCOLS;
    start[0] = 0;
    start[1] = ((hsize_t) mpi_rank * READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_CH_NCOLS);

    if (VERBOSE_MED) {
        HDprintf("Process %d is reading with count[ %llu, %llu ], stride[ %llu, %llu ], start[ %llu, %llu ], block size[ %llu, %llu ]\n",
                mpi_rank, count[0], count[1], stride[0], stride[1], start[0], start[1], block[0], block[1]);
        HDfflush(stdout);
    }

    VRFY((H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, stride, count, block) >= 0),
            "Hyperslab selection succeeded");

    /* Create property list for collective dataset read */
    plist_id = H5Pcreate(H5P_DATASET_XFER);
    VRFY((plist_id >= 0), "DXPL creation succeeded");

    VRFY((H5Pset_dxpl_mpio(plist_id, H5FD_MPIO_COLLECTIVE) >= 0),
            "Set DXPL MPIO succeeded");

    read_buf_size = flat_dims[0] * sizeof(*read_buf);

    read_buf = (COMPOUND_C_DATATYPE *) HDcalloc(1, read_buf_size);
    VRFY((NULL != read_buf), "HDcalloc succeeded");

    VRFY((H5Dread(dset_id, memtype, memspace, filespace, plist_id, read_buf) >= 0),
            "Dataset read succeeded");

    global_buf = (COMPOUND_C_DATATYPE *) HDcalloc(1, correct_buf_size);
    VRFY((NULL != global_buf), "HDcalloc succeeded");

    /* Collect each piece of data from all ranks into a global buffer on all ranks */
    recvcounts = (int *) HDcalloc(1, (size_t) mpi_size * sizeof(*recvcounts));
    VRFY((NULL != recvcounts), "HDcalloc succeeded");

    for (i = 0; i < (size_t) mpi_size; i++)
        recvcounts[i] = (int) (flat_dims[0] * sizeof(*read_buf));

    displs = (int *) HDcalloc(1, (size_t) mpi_size * sizeof(*displs));
    VRFY((NULL != displs), "HDcalloc succeeded");

    for (i = 0; i < (size_t) mpi_size; i++)
        displs[i] = (int) (i * flat_dims[0] * sizeof(*read_buf));

    VRFY((MPI_SUCCESS == MPI_Allgatherv(read_buf, (int) (flat_dims[0] * sizeof(COMPOUND_C_DATATYPE)), MPI_BYTE, global_buf, recvcounts, displs, MPI_BYTE, comm)),
            "MPI_Allgatherv succeeded");

    VRFY((0 == HDmemcmp(global_buf, correct_buf, correct_buf_size)),
            "Data verification succeeded");

    if (displs) HDfree(displs);
    if (recvcounts) HDfree(recvcounts);
    if (global_buf) HDfree(global_buf);
    if (read_buf) HDfree(read_buf);
    if (correct_buf) HDfree(correct_buf);

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
 * Tests parallel read of filtered data from shared
 * chunks using a compound datatype which requires
 * a datatype conversion.
 *
 * The MAINPROCESS rank will first write out all of the
 * data to the dataset. Then, each rank reads a part of
 * each chunk of the dataset and contributes its pieces
 * to a global buffer that is checked for consistency.
 *
 * Programmer: Jordan Henderson
 *             05/17/2018
 */
static void
test_read_cmpd_filtered_dataset_type_conversion_shared(void)
{
    COMPOUND_C_DATATYPE *read_buf = NULL;
    COMPOUND_C_DATATYPE *correct_buf = NULL;
    COMPOUND_C_DATATYPE *global_buf = NULL;
    hsize_t              dataset_dims[READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t              chunk_dims[READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t              sel_dims[READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t              start[READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t              stride[READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t              count[READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t              block[READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t              flat_dims[1];
    size_t               i, read_buf_size, correct_buf_size;
    hid_t                file_id, dset_id, plist_id, filetype, memtype;
    hid_t                filespace, memspace;
    int                 *recvcounts = NULL;
    int                 *displs = NULL;

    if (MAINPROCESS) HDputs("Testing read from shared filtered chunks in Compound Datatype dataset with Datatype conversion");

    CHECK_CUR_FILTER_AVAIL();

    dataset_dims[0] = (hsize_t) READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_NROWS;
    dataset_dims[1] = (hsize_t) READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_NCOLS;

    /* Setup the buffer for writing and for comparison */
    correct_buf_size = dataset_dims[0] * dataset_dims[1] * sizeof(*correct_buf);

    correct_buf = (COMPOUND_C_DATATYPE *) HDcalloc(1, correct_buf_size);
    VRFY((NULL != correct_buf), "HDcalloc succeeded");

    for (i = 0; i < correct_buf_size / sizeof(*correct_buf); i++) {
        correct_buf[i].field1 = (short) (
                                            (dataset_dims[1] * (i / ((hsize_t) mpi_size * dataset_dims[1])))
                                          + (i % dataset_dims[1])
                                          + (((i % ((hsize_t) mpi_size * dataset_dims[1])) / dataset_dims[1]) % dataset_dims[1])
                                        );

        correct_buf[i].field2 = (int) (
                                          (dataset_dims[1] * (i / ((hsize_t) mpi_size * dataset_dims[1])))
                                        + (i % dataset_dims[1])
                                        + (((i % ((hsize_t) mpi_size * dataset_dims[1])) / dataset_dims[1]) % dataset_dims[1])
                                      );

        correct_buf[i].field3 = (long) (
                                           (dataset_dims[1] * (i / ((hsize_t) mpi_size * dataset_dims[1])))
                                         + (i % dataset_dims[1])
                                         + (((i % ((hsize_t) mpi_size * dataset_dims[1])) / dataset_dims[1]) % dataset_dims[1])
                                       );
    }

    /* Create the compound type for memory. */
    memtype = H5Tcreate(H5T_COMPOUND, sizeof(COMPOUND_C_DATATYPE));
    VRFY((memtype >= 0), "Datatype creation succeeded");

    VRFY((H5Tinsert(memtype, "ShortData", HOFFSET(COMPOUND_C_DATATYPE, field1), H5T_NATIVE_SHORT) >= 0),
            "Datatype insertion succeeded");
    VRFY((H5Tinsert(memtype, "IntData", HOFFSET(COMPOUND_C_DATATYPE, field2), H5T_NATIVE_INT) >= 0),
            "Datatype insertion succeeded");
    VRFY((H5Tinsert(memtype, "LongData", HOFFSET(COMPOUND_C_DATATYPE, field3), H5T_NATIVE_LONG) >= 0),
            "Datatype insertion succeeded");

    /* Create the compound type for file. */
    filetype = H5Tcreate(H5T_COMPOUND, 32);
    VRFY((filetype >= 0), "Datatype creation succeeded");

    VRFY((H5Tinsert(filetype, "ShortData", 0, H5T_STD_I64BE) >= 0),
            "Datatype insertion succeeded");
    VRFY((H5Tinsert(filetype, "IntData", 8, H5T_STD_I64BE) >= 0),
            "Datatype insertion succeeded");
    VRFY((H5Tinsert(filetype, "LongData", 16, H5T_STD_I64BE) >= 0),
            "Datatype insertion succeeded");

    if (MAINPROCESS) {
        plist_id = H5Pcreate(H5P_FILE_ACCESS);
        VRFY((plist_id >= 0), "FAPL creation succeeded");

        VRFY((H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0),
                "Set libver bounds succeeded");

        file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, plist_id);
        VRFY((file_id >= 0), "Test file open succeeded");

        VRFY((H5Pclose(plist_id) >= 0), "FAPL close succeeded");

        /* Create the dataspace for the dataset */
        filespace = H5Screate_simple(READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_DIMS, dataset_dims, NULL);
        VRFY((filespace >= 0), "File dataspace creation succeeded");

        /* Create chunked dataset */
        chunk_dims[0] = (hsize_t) READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_CH_NROWS;
        chunk_dims[1] = (hsize_t) READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_CH_NCOLS;

        plist_id = H5Pcreate(H5P_DATASET_CREATE);
        VRFY((plist_id >= 0), "DCPL creation succeeded");

        VRFY((H5Pset_chunk(plist_id, READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_DIMS, chunk_dims) >= 0),
                "Chunk size set");

        /* Add test filter to the pipeline */
        VRFY((set_dcpl_filter(plist_id) >= 0), "Filter set");

        dset_id = H5Dcreate2(file_id, READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_NAME, filetype, filespace,
                H5P_DEFAULT, plist_id, H5P_DEFAULT);
        VRFY((dset_id >= 0), "Dataset creation succeeded");

        VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
        VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

        VRFY((H5Dwrite(dset_id, memtype, H5S_ALL, H5S_ALL, H5P_DEFAULT, correct_buf) >= 0),
                "Dataset write succeeded");

        VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
        VRFY((H5Fclose(file_id) >= 0), "File close succeeded");
    }

    /* Set up file access property list with parallel I/O access */
    plist_id = H5Pcreate(H5P_FILE_ACCESS);
    VRFY((plist_id >= 0), "FAPL creation succeeded");

    VRFY((H5Pset_fapl_mpio(plist_id, comm, info) >= 0),
            "Set FAPL MPIO succeeded");

    VRFY((H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0),
            "Set libver bounds succeeded");

    file_id = H5Fopen(filenames[0], H5F_ACC_RDONLY, plist_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    VRFY((H5Pclose(plist_id) >= 0), "FAPL close succeeded");

    dset_id = H5Dopen2(file_id, "/" READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_NAME, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    sel_dims[0] = (hsize_t) READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_CH_NROWS / (hsize_t) mpi_size;
    sel_dims[1] = (hsize_t) READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_ENTRIES_PER_PROC;

    /* Setup one-dimensional memory dataspace for reading the dataset data into a contiguous buffer */
    flat_dims[0] = sel_dims[0] * sel_dims[1];

    memspace = H5Screate_simple(1, flat_dims, NULL);
    VRFY((memspace >= 0), "Memory dataspace creation succeeded");

    /* Select hyperslab in the file */
    filespace = H5Dget_space(dset_id);
    VRFY((filespace >= 0), "File dataspace retrieval succeeded");

    /*
     * Each process defines the dataset selection in the file and
     * reads it to the selection in memory
     */
    count[0] = 1;
    count[1] = (hsize_t) READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_ENTRIES_PER_PROC;
    stride[0] = (hsize_t) READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_CH_NROWS;
    stride[1] = READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_CH_NCOLS;
    block[0] = (hsize_t) READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_CH_NROWS / (hsize_t) mpi_size;
    block[1] = READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_CH_NCOLS;
    start[0] = (hsize_t) mpi_rank;
    start[1] = 0;

    if (VERBOSE_MED) {
        HDprintf("Process %d is reading with count[ %llu, %llu ], stride[ %llu, %llu ], start[ %llu, %llu ], block size[ %llu, %llu ]\n",
                mpi_rank, count[0], count[1], stride[0], stride[1], start[0], start[1], block[0], block[1]);
        HDfflush(stdout);
    }

    VRFY((H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, stride, count, block) >= 0),
            "Hyperslab selection succeeded");

    /* Create property list for collective dataset read */
    plist_id = H5Pcreate(H5P_DATASET_XFER);
    VRFY((plist_id >= 0), "DXPL creation succeeded");

    VRFY((H5Pset_dxpl_mpio(plist_id, H5FD_MPIO_COLLECTIVE) >= 0),
            "Set DXPL MPIO succeeded");

    read_buf_size = flat_dims[0] * sizeof(*read_buf);

    read_buf = (COMPOUND_C_DATATYPE *) HDcalloc(1, read_buf_size);
    VRFY((NULL != read_buf), "HDcalloc succeeded");

    VRFY((H5Dread(dset_id, memtype, memspace, filespace, plist_id, read_buf) >= 0),
            "Dataset read succeeded");

    global_buf = (COMPOUND_C_DATATYPE *) HDcalloc(1, correct_buf_size);
    VRFY((NULL != global_buf), "HDcalloc succeeded");

    /* Collect each piece of data from all ranks into a global buffer on all ranks */
    recvcounts = (int *) HDcalloc(1, (size_t) mpi_size * sizeof(*recvcounts));
    VRFY((NULL != recvcounts), "HDcalloc succeeded");

    for (i = 0; i < (size_t) mpi_size; i++)
        recvcounts[i] = (int) (flat_dims[0] * sizeof(*read_buf));

    displs = (int *) HDcalloc(1, (size_t) mpi_size * sizeof(*displs));
    VRFY((NULL != displs), "HDcalloc succeeded");

    for (i = 0; i < (size_t) mpi_size; i++)
        displs[i] = (int) (i * flat_dims[0] * sizeof(*read_buf));

    VRFY((MPI_SUCCESS == MPI_Allgatherv(read_buf, (int) (flat_dims[0] * sizeof(COMPOUND_C_DATATYPE)), MPI_BYTE, global_buf, recvcounts, displs, MPI_BYTE, comm)),
            "MPI_Allgatherv succeeded");

    VRFY((0 == HDmemcmp(global_buf, correct_buf, correct_buf_size)),
            "Data verification succeeded");

    if (displs) HDfree(displs);
    if (recvcounts) HDfree(recvcounts);
    if (global_buf) HDfree(global_buf);
    if (read_buf) HDfree(read_buf);
    if (correct_buf) HDfree(correct_buf);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Sclose(memspace) >= 0), "Memory dataspace close succeeded");
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

    if (MAINPROCESS) HDputs("Testing write file serially; read file in parallel");

    CHECK_CUR_FILTER_AVAIL();

    dataset_dims[0] = (hsize_t) WRITE_SERIAL_READ_PARALLEL_NROWS;
    dataset_dims[1] = (hsize_t) WRITE_SERIAL_READ_PARALLEL_NCOLS;
    dataset_dims[2] = (hsize_t) WRITE_SERIAL_READ_PARALLEL_DEPTH;

    /* Write the file on the MAINPROCESS rank */
    if (MAINPROCESS) {
        /* Set up file access property list */
        plist_id = H5Pcreate(H5P_FILE_ACCESS);
        VRFY((plist_id >= 0), "FAPL creation succeeded");

        VRFY((H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0),
                "Set libver bounds succeeded");

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

        VRFY((H5Pset_chunk(plist_id, WRITE_SERIAL_READ_PARALLEL_DATASET_DIMS, chunk_dims) >= 0),
                "Chunk size set");

        /* Add test filter to the pipeline */
        VRFY((set_dcpl_filter(plist_id) >= 0), "Filter set");

        dset_id = H5Dcreate2(file_id, WRITE_SERIAL_READ_PARALLEL_DATASET_NAME, HDF5_DATATYPE_NAME, filespace,
                H5P_DEFAULT, plist_id, H5P_DEFAULT);
        VRFY((dset_id >= 0), "Dataset creation succeeded");

        VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
        VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

        data_size = dataset_dims[0] * dataset_dims[1] * dataset_dims[2] * sizeof(*data);

        data = (C_DATATYPE *) HDcalloc(1, data_size);
        VRFY((NULL != data), "HDcalloc succeeded");

        for (i = 0; i < data_size / sizeof(*data); i++)
            data[i] = (C_DATATYPE) GEN_DATA(i);

        VRFY((H5Dwrite(dset_id, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, H5P_DEFAULT, data) >= 0),
                "Dataset write succeeded");

        if (data) HDfree(data);

        VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
        VRFY((H5Fclose(file_id) >= 0), "File close succeeded");
    }

    correct_buf_size = dataset_dims[0] * dataset_dims[1] * dataset_dims[2] * sizeof(*correct_buf);

    correct_buf = (C_DATATYPE *) HDcalloc(1, correct_buf_size);
    VRFY((NULL != correct_buf), "HDcalloc succeeded");

    read_buf = (C_DATATYPE *) HDcalloc(1, correct_buf_size);
    VRFY((NULL != read_buf), "HDcalloc succeeded");

    for (i = 0; i < correct_buf_size / sizeof(*correct_buf); i++)
        correct_buf[i] = (long) i;

    /* All ranks open the file and verify their "portion" of the dataset is correct */
    plist_id = H5Pcreate(H5P_FILE_ACCESS);
    VRFY((plist_id >= 0), "FAPL creation succeeded");

    VRFY((H5Pset_fapl_mpio(plist_id, comm, info) >= 0),
            "Set FAPL MPIO succeeded");

    VRFY((H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0),
            "Set libver bounds succeeded");

    file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, plist_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    VRFY((H5Pclose(plist_id) >= 0), "FAPL close succeeded");

    dset_id = H5Dopen2(file_id, "/" WRITE_SERIAL_READ_PARALLEL_DATASET_NAME, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    plist_id = H5Pcreate(H5P_DATASET_XFER);
    VRFY((plist_id >= 0), "DXPL creation succeeded");

    VRFY((H5Pset_dxpl_mpio(plist_id, H5FD_MPIO_COLLECTIVE) >= 0),
            "Set DXPL MPIO succeeded");

    VRFY((H5Dread(dset_id, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, plist_id, read_buf) >= 0),
            "Dataset read succeeded");

    VRFY((0 == HDmemcmp(read_buf, correct_buf, correct_buf_size)),
            "Data verification succeeded");

    if (correct_buf) HDfree(correct_buf);
    if (read_buf) HDfree(read_buf);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
    VRFY((H5Pclose(plist_id) >= 0), "DXPL close succeeded");
    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    return;
}

#if MPI_VERSION >= 3
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

    if (MAINPROCESS) HDputs("Testing write file in parallel; read serially");

    CHECK_CUR_FILTER_AVAIL();

    /* Set up file access property list with parallel I/O access */
    plist_id = H5Pcreate(H5P_FILE_ACCESS);
    VRFY((plist_id >= 0), "FAPL creation succeeded");

    VRFY((H5Pset_fapl_mpio(plist_id, comm, info) >= 0),
            "Set FAPL MPIO succeeded");

    VRFY((H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0),
            "Set libver bounds succeeded");

    file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, plist_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    VRFY((H5Pclose(plist_id) >= 0), "FAPL close succeeded");

    /* Create the dataspace for the dataset */
    dataset_dims[0] = (hsize_t) WRITE_PARALLEL_READ_SERIAL_NROWS;
    dataset_dims[1] = (hsize_t) WRITE_PARALLEL_READ_SERIAL_NCOLS;
    dataset_dims[2] = (hsize_t) WRITE_PARALLEL_READ_SERIAL_DEPTH;
    chunk_dims[0] = (hsize_t) WRITE_PARALLEL_READ_SERIAL_CH_NROWS;
    chunk_dims[1] = (hsize_t) WRITE_PARALLEL_READ_SERIAL_CH_NCOLS;
    chunk_dims[2] = 1;
    sel_dims[0] = (hsize_t) WRITE_PARALLEL_READ_SERIAL_CH_NROWS;
    sel_dims[1] = (hsize_t) WRITE_PARALLEL_READ_SERIAL_NCOLS;
    sel_dims[2] = (hsize_t) WRITE_PARALLEL_READ_SERIAL_DEPTH;

    filespace = H5Screate_simple(WRITE_PARALLEL_READ_SERIAL_DATASET_DIMS, dataset_dims, NULL);
    VRFY((filespace >= 0), "File dataspace creation succeeded");

    memspace = H5Screate_simple(WRITE_PARALLEL_READ_SERIAL_DATASET_DIMS, sel_dims, NULL);
    VRFY((memspace >= 0), "Memory dataspace creation succeeded");

    /* Create chunked dataset */
    plist_id = H5Pcreate(H5P_DATASET_CREATE);
    VRFY((plist_id >= 0), "DCPL creation succeeded");

    VRFY((H5Pset_chunk(plist_id, WRITE_PARALLEL_READ_SERIAL_DATASET_DIMS, chunk_dims) >= 0),
            "Chunk size set");

    /* Add test filter to the pipeline */
    VRFY((set_dcpl_filter(plist_id) >= 0), "Filter set");

    dset_id = H5Dcreate2(file_id, WRITE_PARALLEL_READ_SERIAL_DATASET_NAME, HDF5_DATATYPE_NAME, filespace,
            H5P_DEFAULT, plist_id, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset creation succeeded");

    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

    /* Each process defines the dataset selection in memory and writes
     * it to the hyperslab in the file
     */
    count[0] = 1;
    count[1] = (hsize_t) WRITE_PARALLEL_READ_SERIAL_NCOLS / (hsize_t) WRITE_PARALLEL_READ_SERIAL_CH_NCOLS;
    count[2] = (hsize_t) mpi_size;
    stride[0] = (hsize_t) WRITE_PARALLEL_READ_SERIAL_CH_NROWS;
    stride[1] = (hsize_t) WRITE_PARALLEL_READ_SERIAL_CH_NCOLS;
    stride[2] = 1;
    block[0] = (hsize_t) WRITE_PARALLEL_READ_SERIAL_CH_NROWS;
    block[1] = (hsize_t) WRITE_PARALLEL_READ_SERIAL_CH_NCOLS;
    block[2] = 1;
    offset[0] = ((hsize_t) mpi_rank * (hsize_t) WRITE_PARALLEL_READ_SERIAL_CH_NROWS * count[0]);
    offset[1] = 0;
    offset[2] = 0;

    if (VERBOSE_MED) {
        HDprintf("Process %d is writing with count[ %llu, %llu, %llu ], stride[ %llu, %llu, %llu ], offset[ %llu, %llu, %llu ], block size[ %llu, %llu, %llu ]\n",
                mpi_rank, count[0], count[1], count[2], stride[0], stride[1], stride[2], offset[0], offset[1], offset[2], block[0], block[1], block[2]);
        HDfflush(stdout);
    }

    /* Select hyperslab in the file */
    filespace = H5Dget_space(dset_id);
    VRFY((filespace >= 0), "File dataspace retrieval succeeded");

    VRFY((H5Sselect_hyperslab(filespace, H5S_SELECT_SET, offset, stride,
                    count, block) >= 0), "Hyperslab selection succeeded");

    /* Fill data buffer */
    data_size = sel_dims[0] * sel_dims[1] * sel_dims[2] * sizeof(*data);

    data = (C_DATATYPE *) HDcalloc(1, data_size);
    VRFY((NULL != data), "HDcalloc succeeded");

    for (i = 0; i < data_size / sizeof(*data); i++)
        data[i] = (C_DATATYPE) GEN_DATA(i);

    /* Create property list for collective dataset write */
    plist_id = H5Pcreate(H5P_DATASET_XFER);
    VRFY((plist_id >= 0), "DXPL creation succeeded");

    VRFY((H5Pset_dxpl_mpio(plist_id, H5FD_MPIO_COLLECTIVE) >= 0),
            "Set DXPL MPIO succeeded");

    VRFY((H5Dwrite(dset_id, HDF5_DATATYPE_NAME, memspace, filespace, plist_id, data) >= 0),
            "Dataset write succeeded");

    if (data) HDfree(data);

    VRFY((H5Pclose(plist_id) >= 0), "DXPL close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Sclose(memspace) >= 0), "Memory dataspace close succeeded");
    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    if (MAINPROCESS) {
        plist_id = H5Pcreate(H5P_FILE_ACCESS);
        VRFY((plist_id >= 0), "FAPL creation succeeded");

        VRFY((H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0),
                "Set libver bounds succeeded");

        file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, plist_id);
        VRFY((file_id >= 0), "Test file open succeeded");

        VRFY((H5Pclose(plist_id) >= 0), "FAPL close succeeded");

        dset_id = H5Dopen2(file_id, "/" WRITE_PARALLEL_READ_SERIAL_DATASET_NAME, H5P_DEFAULT);
        VRFY((dset_id >= 0), "Dataset open succeeded");

        correct_buf_size = dataset_dims[0] * dataset_dims[1] * dataset_dims[2] * sizeof(*correct_buf);

        correct_buf = (C_DATATYPE *) HDcalloc(1, correct_buf_size);
        VRFY((NULL != correct_buf), "HDcalloc succeeded");

        read_buf = (C_DATATYPE *) HDcalloc(1, correct_buf_size);
        VRFY((NULL != read_buf), "HDcalloc succeeded");

        for (i = 0; i < correct_buf_size / sizeof(*correct_buf); i++)
            correct_buf[i] = (C_DATATYPE) (
                                             (i % (dataset_dims[0] * dataset_dims[1]))
                                           + (i / (dataset_dims[0] * dataset_dims[1]))
                                          );

        VRFY((H5Dread(dset_id, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, H5P_DEFAULT, read_buf) >= 0),
                "Dataset read succeeded");

        VRFY((0 == HDmemcmp(read_buf, correct_buf, correct_buf_size)),
                "Data verification succeeded");

        VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
        VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

        HDfree(correct_buf);
        HDfree(read_buf);
    }

    return;
}

/*
 * Tests that causing chunks to continually grow and shrink
 * by writing random data followed by zeroed-out data (and
 * thus controlling the compression ratio) does not cause
 * problems.
 *
 * Programmer: Jordan Henderson
 *             06/04/2018
 */
static void
test_shrinking_growing_chunks(void)
{
    double  *data = NULL;
    hsize_t  dataset_dims[SHRINKING_GROWING_CHUNKS_DATASET_DIMS];
    hsize_t  chunk_dims[SHRINKING_GROWING_CHUNKS_DATASET_DIMS];
    hsize_t  sel_dims[SHRINKING_GROWING_CHUNKS_DATASET_DIMS];
    hsize_t  start[SHRINKING_GROWING_CHUNKS_DATASET_DIMS];
    hsize_t  stride[SHRINKING_GROWING_CHUNKS_DATASET_DIMS];
    hsize_t  count[SHRINKING_GROWING_CHUNKS_DATASET_DIMS];
    hsize_t  block[SHRINKING_GROWING_CHUNKS_DATASET_DIMS];
    size_t   i, data_size;
    hid_t    file_id = -1, dset_id = -1, plist_id = -1;
    hid_t    filespace = -1, memspace = -1;

    if (MAINPROCESS) HDputs("Testing continually shrinking/growing chunks");

    CHECK_CUR_FILTER_AVAIL();

    /* Set up file access property list with parallel I/O access */
    plist_id = H5Pcreate(H5P_FILE_ACCESS);
    VRFY((plist_id >= 0), "FAPL creation succeeded");

    VRFY((H5Pset_fapl_mpio(plist_id, comm, info) >= 0),
            "Set FAPL MPIO succeeded");

    VRFY((H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0),
            "Set libver bounds succeeded");

    file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, plist_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    VRFY((H5Pclose(plist_id) >= 0), "FAPL close succeeded");

    /* Create the dataspace for the dataset */
    dataset_dims[0] = (hsize_t) SHRINKING_GROWING_CHUNKS_NROWS;
    dataset_dims[1] = (hsize_t) SHRINKING_GROWING_CHUNKS_NCOLS;
    chunk_dims[0] = (hsize_t) SHRINKING_GROWING_CHUNKS_CH_NROWS;
    chunk_dims[1] = (hsize_t) SHRINKING_GROWING_CHUNKS_CH_NCOLS;
    sel_dims[0] = (hsize_t) SHRINKING_GROWING_CHUNKS_CH_NROWS;
    sel_dims[1] = (hsize_t) SHRINKING_GROWING_CHUNKS_NCOLS;

    filespace = H5Screate_simple(SHRINKING_GROWING_CHUNKS_DATASET_DIMS, dataset_dims, NULL);
    VRFY((filespace >= 0), "File dataspace creation succeeded");

    memspace = H5Screate_simple(SHRINKING_GROWING_CHUNKS_DATASET_DIMS, sel_dims, NULL);
    VRFY((memspace >= 0), "Memory dataspace creation succeeded");

    /* Create chunked dataset */
    plist_id = H5Pcreate(H5P_DATASET_CREATE);
    VRFY((plist_id >= 0), "DCPL creation succeeded");

    VRFY((H5Pset_chunk(plist_id, SHRINKING_GROWING_CHUNKS_DATASET_DIMS, chunk_dims) >= 0),
            "Chunk size set");

    /* Add test filter to the pipeline */
    VRFY((set_dcpl_filter(plist_id) >= 0), "Filter set");

    dset_id = H5Dcreate2(file_id, SHRINKING_GROWING_CHUNKS_DATASET_NAME, H5T_NATIVE_DOUBLE, filespace,
            H5P_DEFAULT, plist_id, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset creation succeeded");

    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

    /*
     * Each process defines the dataset selection in memory and writes
     * it to the hyperslab in the file
     */
    count[0] = 1;
    count[1] = (hsize_t) SHRINKING_GROWING_CHUNKS_NCOLS / (hsize_t) SHRINKING_GROWING_CHUNKS_CH_NCOLS;
    stride[0] = (hsize_t) SHRINKING_GROWING_CHUNKS_CH_NROWS;
    stride[1] = (hsize_t) SHRINKING_GROWING_CHUNKS_CH_NCOLS;
    block[0] = (hsize_t) SHRINKING_GROWING_CHUNKS_CH_NROWS;
    block[1] = (hsize_t) SHRINKING_GROWING_CHUNKS_CH_NCOLS;
    start[0] = ((hsize_t) mpi_rank * (hsize_t) SHRINKING_GROWING_CHUNKS_CH_NROWS * count[0]);
    start[1] = 0;

    if (VERBOSE_MED) {
        HDprintf("Process %d is writing with count[ %llu, %llu ], stride[ %llu, %llu ], start[ %llu, %llu ], block size[ %llu, %llu ]\n",
                mpi_rank, count[0], count[1], stride[0], stride[1], start[0], start[1], block[0], block[1]);
        HDfflush(stdout);
    }

    /* Select hyperslab in the file */
    filespace = H5Dget_space(dset_id);
    VRFY((dset_id >= 0), "File dataspace retrieval succeeded");

    VRFY((H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, stride, count, block) >= 0),
            "Hyperslab selection succeeded");

    /* Create property list for collective dataset write */
    plist_id = H5Pcreate(H5P_DATASET_XFER);
    VRFY((plist_id >= 0), "DXPL creation succeeded");

    VRFY((H5Pset_dxpl_mpio(plist_id, H5FD_MPIO_COLLECTIVE) >= 0),
            "Set DXPL MPIO succeeded");

    data_size = sel_dims[0] * sel_dims[1] * sizeof(double);

    data = (double *) HDcalloc(1, data_size);
    VRFY((NULL != data), "HDcalloc succeeded");

    for (i = 0; i < SHRINKING_GROWING_CHUNKS_NLOOPS; i++) {
        /* Continually write random float data, followed by zeroed-out data */
        if ((i % 2))
            HDmemset(data, 0, data_size);
        else {
            size_t j;
            for (j = 0; j < data_size / sizeof(*data); j++) {
                data[j] = (float) ( rand() / (double) (RAND_MAX / (double) 1.0L) );
            }
        }

        VRFY((H5Dwrite(dset_id, H5T_NATIVE_DOUBLE, memspace, filespace, plist_id, data) >= 0),
                "Dataset write succeeded");
    }

    if (data) HDfree(data);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Sclose(memspace) >= 0), "Memory dataspace close succeeded");
    VRFY((H5Pclose(plist_id) >= 0), "DXPL close succeeded");
    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    return;
}
#endif

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
            HDprintf("The Parallel Filters tests require at least 1 rank.\n");
            HDprintf("Quitting...\n");
        }

        MPI_Abort(MPI_COMM_WORLD, 1);
    }

    if (H5dont_atexit() < 0) {
        if (MAINPROCESS) {
            HDprintf("Failed to turn off atexit processing. Continue.\n");
        }
    }

    H5open();

    if (MAINPROCESS) {
        HDprintf("==========================\n");
        HDprintf("Parallel Filters tests\n");
        HDprintf("==========================\n\n");
    }

    if (VERBOSE_MED) h5_show_hostname();

    ALARM_ON;

    /* Create test file */
    fapl = H5Pcreate(H5P_FILE_ACCESS);
    VRFY((fapl >= 0), "FAPL creation succeeded");

    VRFY((H5Pset_fapl_mpio(fapl, comm, info) >= 0), "Set FAPL MPIO succeeded");

    VRFY((H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0),
            "Set libver bounds succeeded");

    VRFY((h5_fixname(FILENAME[0], fapl, filenames[0], sizeof(filenames[0])) != NULL),
            "Test file name created");

    file_id = H5Fcreate(filenames[0], H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
    VRFY((file_id >= 0), "Test file creation succeeded");

    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    for (i = 0; i < ARRAY_SIZE(tests); i++) {
        if (MPI_SUCCESS == (mpi_code = MPI_Barrier(comm))) {
            (*tests[i])();
        }
        else {
            if (MAINPROCESS) MESG("MPI_Barrier failed");
            nerrors++;
        }
    }

    /*
     * Increment the filter index to switch to the checksum filter
     * and re-run the tests.
     */
    cur_filter_idx++;

    h5_clean_files(FILENAME, fapl);

    fapl = H5Pcreate(H5P_FILE_ACCESS);
    VRFY((fapl >= 0), "FAPL creation succeeded");

    VRFY((H5Pset_fapl_mpio(fapl, comm, info) >= 0), "Set FAPL MPIO succeeded");

    VRFY((H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0),
            "Set libver bounds succeeded");

    file_id = H5Fcreate(filenames[0], H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
    VRFY((file_id >= 0), "Test file creation succeeded");

    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    if (MAINPROCESS) {
        HDprintf("\n=================================================================\n");
        HDprintf("Re-running Parallel Filters tests with Fletcher32 checksum filter\n");
        HDprintf("=================================================================\n\n");
    }

    for (i = 0; i < ARRAY_SIZE(tests); i++) {
        if (MPI_SUCCESS == (mpi_code = MPI_Barrier(comm))) {
            (*tests[i])();
        }
        else {
            if (MAINPROCESS) MESG("MPI_Barrier failed");
            nerrors++;
        }
    }

    if (nerrors) goto exit;

    if (MAINPROCESS) HDputs("All Parallel Filters tests passed\n");

exit:
    if (nerrors)
        if (MAINPROCESS)
            HDprintf("*** %d TEST ERROR%s OCCURRED ***\n", nerrors,
                    nerrors > 1 ? "S" : "");

    ALARM_OFF;

    h5_clean_files(FILENAME, fapl);

    H5close();

    MPI_Finalize();

    exit((nerrors ? EXIT_FAILURE : EXIT_SUCCESS));
}
