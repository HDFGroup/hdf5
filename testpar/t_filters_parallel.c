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

const char *FILENAME[] = {"t_filters_parallel", NULL};
char        filenames[1][256];

static MPI_Comm comm = MPI_COMM_WORLD;
static MPI_Info info = MPI_INFO_NULL;
static int      mpi_rank;
static int      mpi_size;

int nerrors = 0;

/* Arrays of filter ID values and filter names (should match each other) */
H5Z_filter_t filterIDs[] = {
    H5Z_FILTER_DEFLATE, H5Z_FILTER_SHUFFLE, H5Z_FILTER_FLETCHER32,
    H5Z_FILTER_SZIP,    H5Z_FILTER_NBIT,    H5Z_FILTER_SCALEOFFSET,
};

const char *filterNames[] = {"Deflate", "Shuffle", "Fletcher32", "SZIP", "Nbit", "ScaleOffset"};

/* Function pointer typedef for test functions */
typedef void (*test_func)(const char *parent_group, H5Z_filter_t filter_id, hid_t fapl_id, hid_t dcpl_id,
                          hid_t dxpl_id);

/* Typedef for filter arguments for user-defined filters */
typedef struct filter_options_t {
    unsigned int       flags;
    size_t             cd_nelmts;
    const unsigned int cd_values[];
} filter_options_t;

/*
 * Enum for verify_space_alloc_status which specifies
 * how many chunks have been written to in a dataset
 */
typedef enum num_chunks_written_t {
    DATASET_JUST_CREATED,
    NO_CHUNKS_WRITTEN,
    SOME_CHUNKS_WRITTEN,
    ALL_CHUNKS_WRITTEN
} num_chunks_written_t;

static herr_t set_dcpl_filter(hid_t dcpl_id, H5Z_filter_t filter_id, filter_options_t *filter_options);
static herr_t verify_space_alloc_status(hid_t dset_id, hid_t dcpl_id, num_chunks_written_t chunks_written);

#ifdef H5_HAVE_PARALLEL_FILTERED_WRITES
/* Tests for writing data in parallel */
static void test_write_one_chunk_filtered_dataset(const char *parent_group, H5Z_filter_t filter_id,
                                                  hid_t fapl_id, hid_t dcpl_id, hid_t dxpl_id);
static void test_write_filtered_dataset_no_overlap(const char *parent_group, H5Z_filter_t filter_id,
                                                   hid_t fapl_id, hid_t dcpl_id, hid_t dxpl_id);
static void test_write_filtered_dataset_no_overlap_partial(const char *parent_group, H5Z_filter_t filter_id,
                                                           hid_t fapl_id, hid_t dcpl_id, hid_t dxpl_id);
static void test_write_filtered_dataset_overlap(const char *parent_group, H5Z_filter_t filter_id,
                                                hid_t fapl_id, hid_t dcpl_id, hid_t dxpl_id);
static void test_write_filtered_dataset_single_unlim_dim_no_overlap(const char  *parent_group,
                                                                    H5Z_filter_t filter_id, hid_t fapl_id,
                                                                    hid_t dcpl_id, hid_t dxpl_id);
static void test_write_filtered_dataset_single_unlim_dim_overlap(const char  *parent_group,
                                                                 H5Z_filter_t filter_id, hid_t fapl_id,
                                                                 hid_t dcpl_id, hid_t dxpl_id);
static void test_write_filtered_dataset_multi_unlim_dim_no_overlap(const char  *parent_group,
                                                                   H5Z_filter_t filter_id, hid_t fapl_id,
                                                                   hid_t dcpl_id, hid_t dxpl_id);
static void test_write_filtered_dataset_multi_unlim_dim_overlap(const char  *parent_group,
                                                                H5Z_filter_t filter_id, hid_t fapl_id,
                                                                hid_t dcpl_id, hid_t dxpl_id);
static void test_write_filtered_dataset_single_no_selection(const char *parent_group, H5Z_filter_t filter_id,
                                                            hid_t fapl_id, hid_t dcpl_id, hid_t dxpl_id);
static void test_write_filtered_dataset_all_no_selection(const char *parent_group, H5Z_filter_t filter_id,
                                                         hid_t fapl_id, hid_t dcpl_id, hid_t dxpl_id);
static void test_write_filtered_dataset_point_selection(const char *parent_group, H5Z_filter_t filter_id,
                                                        hid_t fapl_id, hid_t dcpl_id, hid_t dxpl_id);
static void test_write_filtered_dataset_interleaved_write(const char *parent_group, H5Z_filter_t filter_id,
                                                          hid_t fapl_id, hid_t dcpl_id, hid_t dxpl_id);
static void test_write_transformed_filtered_dataset_no_overlap(const char  *parent_group,
                                                               H5Z_filter_t filter_id, hid_t fapl_id,
                                                               hid_t dcpl_id, hid_t dxpl_id);
static void test_write_3d_filtered_dataset_no_overlap_separate_pages(const char  *parent_group,
                                                                     H5Z_filter_t filter_id, hid_t fapl_id,
                                                                     hid_t dcpl_id, hid_t dxpl_id);
static void test_write_3d_filtered_dataset_no_overlap_same_pages(const char  *parent_group,
                                                                 H5Z_filter_t filter_id, hid_t fapl_id,
                                                                 hid_t dcpl_id, hid_t dxpl_id);
static void test_write_3d_filtered_dataset_overlap(const char *parent_group, H5Z_filter_t filter_id,
                                                   hid_t fapl_id, hid_t dcpl_id, hid_t dxpl_id);
static void test_write_cmpd_filtered_dataset_no_conversion_unshared(const char  *parent_group,
                                                                    H5Z_filter_t filter_id, hid_t fapl_id,
                                                                    hid_t dcpl_id, hid_t dxpl_id);
static void test_write_cmpd_filtered_dataset_no_conversion_shared(const char  *parent_group,
                                                                  H5Z_filter_t filter_id, hid_t fapl_id,
                                                                  hid_t dcpl_id, hid_t dxpl_id);
static void test_write_cmpd_filtered_dataset_type_conversion_unshared(const char  *parent_group,
                                                                      H5Z_filter_t filter_id, hid_t fapl_id,
                                                                      hid_t dcpl_id, hid_t dxpl_id);
static void test_write_cmpd_filtered_dataset_type_conversion_shared(const char  *parent_group,
                                                                    H5Z_filter_t filter_id, hid_t fapl_id,
                                                                    hid_t dcpl_id, hid_t dxpl_id);
#endif

/* Tests for reading data in parallel */
static void test_read_one_chunk_filtered_dataset(const char *parent_group, H5Z_filter_t filter_id,
                                                 hid_t fapl_id, hid_t dcpl_id, hid_t dxpl_id);
static void test_read_filtered_dataset_no_overlap(const char *parent_group, H5Z_filter_t filter_id,
                                                  hid_t fapl_id, hid_t dcpl_id, hid_t dxpl_id);
static void test_read_filtered_dataset_overlap(const char *parent_group, H5Z_filter_t filter_id,
                                               hid_t fapl_id, hid_t dcpl_id, hid_t dxpl_id);
static void test_read_filtered_dataset_single_no_selection(const char *parent_group, H5Z_filter_t filter_id,
                                                           hid_t fapl_id, hid_t dcpl_id, hid_t dxpl_id);
static void test_read_filtered_dataset_all_no_selection(const char *parent_group, H5Z_filter_t filter_id,
                                                        hid_t fapl_id, hid_t dcpl_id, hid_t dxpl_id);
static void test_read_filtered_dataset_point_selection(const char *parent_group, H5Z_filter_t filter_id,
                                                       hid_t fapl_id, hid_t dcpl_id, hid_t dxpl_id);
static void test_read_filtered_dataset_interleaved_read(const char *parent_group, H5Z_filter_t filter_id,
                                                        hid_t fapl_id, hid_t dcpl_id, hid_t dxpl_id);
static void test_read_transformed_filtered_dataset_no_overlap(const char  *parent_group,
                                                              H5Z_filter_t filter_id, hid_t fapl_id,
                                                              hid_t dcpl_id, hid_t dxpl_id);
static void test_read_3d_filtered_dataset_no_overlap_separate_pages(const char  *parent_group,
                                                                    H5Z_filter_t filter_id, hid_t fapl_id,
                                                                    hid_t dcpl_id, hid_t dxpl_id);
static void test_read_3d_filtered_dataset_no_overlap_same_pages(const char  *parent_group,
                                                                H5Z_filter_t filter_id, hid_t fapl_id,
                                                                hid_t dcpl_id, hid_t dxpl_id);
static void test_read_3d_filtered_dataset_overlap(const char *parent_group, H5Z_filter_t filter_id,
                                                  hid_t fapl_id, hid_t dcpl_id, hid_t dxpl_id);
static void test_read_cmpd_filtered_dataset_no_conversion_unshared(const char  *parent_group,
                                                                   H5Z_filter_t filter_id, hid_t fapl_id,
                                                                   hid_t dcpl_id, hid_t dxpl_id);
static void test_read_cmpd_filtered_dataset_no_conversion_shared(const char  *parent_group,
                                                                 H5Z_filter_t filter_id, hid_t fapl_id,
                                                                 hid_t dcpl_id, hid_t dxpl_id);
static void test_read_cmpd_filtered_dataset_type_conversion_unshared(const char  *parent_group,
                                                                     H5Z_filter_t filter_id, hid_t fapl_id,
                                                                     hid_t dcpl_id, hid_t dxpl_id);
static void test_read_cmpd_filtered_dataset_type_conversion_shared(const char  *parent_group,
                                                                   H5Z_filter_t filter_id, hid_t fapl_id,
                                                                   hid_t dcpl_id, hid_t dxpl_id);

/*
 * Tests for attempting to round-trip the data going from
 *
 * written serially -> read in parallel
 *
 * and
 *
 * written in parallel -> read serially
 */
static void test_write_serial_read_parallel(const char *parent_group, H5Z_filter_t filter_id, hid_t fapl_id,
                                            hid_t dcpl_id, hid_t dxpl_id);

#ifdef H5_HAVE_PARALLEL_FILTERED_WRITES
static void test_write_parallel_read_serial(const char *parent_group, H5Z_filter_t filter_id, hid_t fapl_id,
                                            hid_t dcpl_id, hid_t dxpl_id);

/* Other miscellaneous tests */
static void test_shrinking_growing_chunks(const char *parent_group, H5Z_filter_t filter_id, hid_t fapl_id,
                                          hid_t dcpl_id, hid_t dxpl_id);
static void test_edge_chunks_no_overlap(const char *parent_group, H5Z_filter_t filter_id, hid_t fapl_id,
                                        hid_t dcpl_id, hid_t dxpl_id);
static void test_edge_chunks_overlap(const char *parent_group, H5Z_filter_t filter_id, hid_t fapl_id,
                                     hid_t dcpl_id, hid_t dxpl_id);
static void test_edge_chunks_partial_write(const char *parent_group, H5Z_filter_t filter_id, hid_t fapl_id,
                                           hid_t dcpl_id, hid_t dxpl_id);
static void test_fill_values(const char *parent_group, H5Z_filter_t filter_id, hid_t fapl_id, hid_t dcpl_id,
                             hid_t dxpl_id);
static void test_fill_value_undefined(const char *parent_group, H5Z_filter_t filter_id, hid_t fapl_id,
                                      hid_t dcpl_id, hid_t dxpl_id);
static void test_fill_time_never(const char *parent_group, H5Z_filter_t filter_id, hid_t fapl_id,
                                 hid_t dcpl_id, hid_t dxpl_id);
#endif

static test_func tests[] = {
#ifdef H5_HAVE_PARALLEL_FILTERED_WRITES
    test_write_one_chunk_filtered_dataset,
    test_write_filtered_dataset_no_overlap,
    test_write_filtered_dataset_no_overlap_partial,
    test_write_filtered_dataset_overlap,
    test_write_filtered_dataset_single_unlim_dim_no_overlap,
    test_write_filtered_dataset_single_unlim_dim_overlap,
    test_write_filtered_dataset_multi_unlim_dim_no_overlap,
    test_write_filtered_dataset_multi_unlim_dim_overlap,
    test_write_filtered_dataset_single_no_selection,
    test_write_filtered_dataset_all_no_selection,
    test_write_filtered_dataset_point_selection,
    test_write_filtered_dataset_interleaved_write,
    test_write_transformed_filtered_dataset_no_overlap,
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
    test_read_transformed_filtered_dataset_no_overlap,
    test_read_3d_filtered_dataset_no_overlap_separate_pages,
    test_read_3d_filtered_dataset_no_overlap_same_pages,
    test_read_3d_filtered_dataset_overlap,
    test_read_cmpd_filtered_dataset_no_conversion_unshared,
    test_read_cmpd_filtered_dataset_no_conversion_shared,
    test_read_cmpd_filtered_dataset_type_conversion_unshared,
    test_read_cmpd_filtered_dataset_type_conversion_shared,
    test_write_serial_read_parallel,
#ifdef H5_HAVE_PARALLEL_FILTERED_WRITES
    test_write_parallel_read_serial,
    test_shrinking_growing_chunks,
    test_edge_chunks_no_overlap,
    test_edge_chunks_overlap,
    test_edge_chunks_partial_write,
    test_fill_values,
    test_fill_value_undefined,
    test_fill_time_never,
#endif
};

/*
 * Function to call the appropriate HDF5 filter-setting function
 * depending on the given filter ID. Used to re-run the tests
 * with different filters to check that the data still comes back
 * correctly under a variety of circumstances, such as the
 * Fletcher32 checksum filter increasing the size of the chunk.
 */
static herr_t
set_dcpl_filter(hid_t dcpl_id, H5Z_filter_t filter_id, filter_options_t *filter_options)
{
    switch (filter_id) {
        case H5Z_FILTER_DEFLATE:
            return H5Pset_deflate(dcpl_id, DEFAULT_DEFLATE_LEVEL);
        case H5Z_FILTER_SHUFFLE:
            return H5Pset_shuffle(dcpl_id);
        case H5Z_FILTER_FLETCHER32:
            return H5Pset_fletcher32(dcpl_id);
        case H5Z_FILTER_SZIP: {
            unsigned pixels_per_block         = H5_SZIP_MAX_PIXELS_PER_BLOCK;
            hsize_t  chunk_dims[H5S_MAX_RANK] = {0};
            size_t   i, chunk_nelemts;

            VRFY(H5Pget_chunk(dcpl_id, H5S_MAX_RANK, chunk_dims) >= 0, "H5Pget_chunk succeeded");

            for (i = 0, chunk_nelemts = 1; i < H5S_MAX_RANK; i++)
                if (chunk_dims[i] > 0)
                    chunk_nelemts *= chunk_dims[i];

            if (chunk_nelemts < H5_SZIP_MAX_PIXELS_PER_BLOCK) {
                /*
                 * Can't set SZIP for chunk of 1 data element.
                 * Pixels-per-block value must be both even
                 * and non-zero.
                 */
                if (chunk_nelemts == 1)
                    return SUCCEED;

                if ((chunk_nelemts % 2) == 0)
                    pixels_per_block = (unsigned)chunk_nelemts;
                else
                    pixels_per_block = (unsigned)(chunk_nelemts - 1);
            }
            else
                pixels_per_block = H5_SZIP_MAX_PIXELS_PER_BLOCK;

            return H5Pset_szip(dcpl_id, 0, pixels_per_block);
        }
        case H5Z_FILTER_NBIT:
            return H5Pset_nbit(dcpl_id);
        case H5Z_FILTER_SCALEOFFSET:
            return H5Pset_scaleoffset(dcpl_id, H5Z_SO_INT, 0);
        default: {
            if (!filter_options)
                return FAIL;

            return H5Pset_filter(dcpl_id, filter_id, filter_options->flags, filter_options->cd_nelmts,
                                 filter_options->cd_values);
        }
    }
}

/*
 * Function to verify the status of dataset storage space allocation
 * based on the dataset's allocation time setting and how many chunks
 * in the dataset have been written to.
 */
static herr_t
verify_space_alloc_status(hid_t dset_id, hid_t dcpl_id, num_chunks_written_t chunks_written)
{
    int    nfilters;
    herr_t ret_value = SUCCEED;

    VRFY(((nfilters = H5Pget_nfilters(dcpl_id)) >= 0), "H5Pget_nfilters succeeded");

    /*
     * Only verify space allocation status when there are filters
     * in the dataset's filter pipeline. When filters aren't in the
     * pipeline, the space allocation time and status can vary based
     * on whether the file was created in parallel or serial mode.
     */
    if (nfilters > 0) {
        H5D_space_status_t space_status;
        H5D_alloc_time_t   alloc_time;

        VRFY((H5Pget_alloc_time(dcpl_id, &alloc_time) >= 0), "H5Pget_alloc_time succeeded");
        VRFY((H5Dget_space_status(dset_id, &space_status) >= 0), "H5Dget_space_status succeeded");

        switch (alloc_time) {
            case H5D_ALLOC_TIME_EARLY:
                /*
                 * Early space allocation should always result in the
                 * full dataset storage space being allocated.
                 */
                VRFY(space_status == H5D_SPACE_STATUS_ALLOCATED, "verified space allocation status");
                break;
            case H5D_ALLOC_TIME_LATE:
                /*
                 * Late space allocation should always result in the
                 * full dataset storage space being allocated when
                 * the dataset gets written to. However, if the dataset
                 * is extended the dataset's space allocation status
                 * can become partly allocated until the dataset is
                 * written to again.
                 */
                if (chunks_written == SOME_CHUNKS_WRITTEN || chunks_written == ALL_CHUNKS_WRITTEN)
                    VRFY((space_status == H5D_SPACE_STATUS_ALLOCATED) ||
                             (space_status == H5D_SPACE_STATUS_PART_ALLOCATED),
                         "verified space allocation status");
                else if (chunks_written == NO_CHUNKS_WRITTEN)
                    /*
                     * A special case where we wrote to a dataset that
                     * uses late space allocation, but the write was
                     * either a no-op (no selection in the dataset
                     * from any rank) or something caused the write to
                     * fail late in the process of performing the actual
                     * write. In either case, space should still have
                     * been allocated.
                     */
                    VRFY(space_status == H5D_SPACE_STATUS_ALLOCATED, "verified space allocation status");
                else
                    VRFY(space_status == H5D_SPACE_STATUS_NOT_ALLOCATED, "verified space allocation status");
                break;
            case H5D_ALLOC_TIME_DEFAULT:
            case H5D_ALLOC_TIME_INCR:
                /*
                 * Incremental space allocation should result in
                 * the dataset's storage space being incrementally
                 * allocated as chunks are written to. Once all chunks
                 * have been written to, the space allocation should be
                 * seen as fully allocated.
                 */
                if (chunks_written == SOME_CHUNKS_WRITTEN)
                    VRFY((space_status == H5D_SPACE_STATUS_PART_ALLOCATED),
                         "verified space allocation status");
                else if (chunks_written == ALL_CHUNKS_WRITTEN)
                    VRFY((space_status == H5D_SPACE_STATUS_ALLOCATED), "verified space allocation status");
                else
                    VRFY(space_status == H5D_SPACE_STATUS_NOT_ALLOCATED, "verified space allocation status");
                break;
            default:
                if (MAINPROCESS)
                    MESG("unknown space allocation time");
                MPI_Abort(MPI_COMM_WORLD, 1);
        }
    }

    return ret_value;
}

#ifdef H5_HAVE_PARALLEL_FILTERED_WRITES
/*
 * Tests parallel write of filtered data in the special
 * case where a dataset is composed of a single chunk.
 *
 * Programmer: Jordan Henderson
 *             02/01/2017
 */
static void
test_write_one_chunk_filtered_dataset(const char *parent_group, H5Z_filter_t filter_id, hid_t fapl_id,
                                      hid_t dcpl_id, hid_t dxpl_id)
{
    C_DATATYPE *data        = NULL;
    C_DATATYPE *read_buf    = NULL;
    C_DATATYPE *correct_buf = NULL;
    hsize_t     dataset_dims[WRITE_ONE_CHUNK_FILTERED_DATASET_DIMS];
    hsize_t     chunk_dims[WRITE_ONE_CHUNK_FILTERED_DATASET_DIMS];
    hsize_t     sel_dims[WRITE_ONE_CHUNK_FILTERED_DATASET_DIMS];
    hsize_t     start[WRITE_ONE_CHUNK_FILTERED_DATASET_DIMS];
    hsize_t     stride[WRITE_ONE_CHUNK_FILTERED_DATASET_DIMS];
    hsize_t     count[WRITE_ONE_CHUNK_FILTERED_DATASET_DIMS];
    hsize_t     block[WRITE_ONE_CHUNK_FILTERED_DATASET_DIMS];
    size_t      i, data_size, correct_buf_size;
    hid_t       file_id = H5I_INVALID_HID, dset_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID;
    hid_t       group_id  = H5I_INVALID_HID;
    hid_t       filespace = H5I_INVALID_HID, memspace = H5I_INVALID_HID;

    if (MAINPROCESS)
        HDputs("Testing write to one-chunk filtered dataset");

    file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, fapl_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
    VRFY((group_id >= 0), "H5Gopen2 succeeded");

    /* Create the dataspace for the dataset */
    dataset_dims[0] = (hsize_t)WRITE_ONE_CHUNK_FILTERED_DATASET_NROWS;
    dataset_dims[1] = (hsize_t)WRITE_ONE_CHUNK_FILTERED_DATASET_NCOLS;
    chunk_dims[0]   = (hsize_t)WRITE_ONE_CHUNK_FILTERED_DATASET_CH_NROWS;
    chunk_dims[1]   = (hsize_t)WRITE_ONE_CHUNK_FILTERED_DATASET_CH_NCOLS;
    sel_dims[0]     = (hsize_t)WRITE_ONE_CHUNK_FILTERED_DATASET_NROWS / (hsize_t)mpi_size;
    sel_dims[1]     = (hsize_t)WRITE_ONE_CHUNK_FILTERED_DATASET_NCOLS;

    filespace = H5Screate_simple(WRITE_ONE_CHUNK_FILTERED_DATASET_DIMS, dataset_dims, NULL);
    VRFY((filespace >= 0), "File dataspace creation succeeded");

    memspace = H5Screate_simple(WRITE_ONE_CHUNK_FILTERED_DATASET_DIMS, sel_dims, NULL);
    VRFY((memspace >= 0), "Memory dataspace creation succeeded");

    /* Create chunked dataset */
    plist_id = H5Pcopy(dcpl_id);
    VRFY((plist_id >= 0), "DCPL copy succeeded");

    VRFY((H5Pset_chunk(plist_id, WRITE_ONE_CHUNK_FILTERED_DATASET_DIMS, chunk_dims) >= 0), "Chunk size set");

    /* Add test filter to the pipeline */
    VRFY((set_dcpl_filter(plist_id, filter_id, NULL) >= 0), "Filter set");

    dset_id = H5Dcreate2(group_id, WRITE_ONE_CHUNK_FILTERED_DATASET_NAME, HDF5_DATATYPE_NAME, filespace,
                         H5P_DEFAULT, plist_id, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset creation succeeded");

    /* Verify space allocation status */
    verify_space_alloc_status(dset_id, plist_id, DATASET_JUST_CREATED);

    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

    /* Each process defines the dataset selection in memory and writes
     * it to the hyperslab in the file
     */
    count[0]  = 1;
    count[1]  = 1;
    stride[0] = (hsize_t)WRITE_ONE_CHUNK_FILTERED_DATASET_CH_NROWS;
    stride[1] = (hsize_t)WRITE_ONE_CHUNK_FILTERED_DATASET_CH_NCOLS;
    block[0]  = sel_dims[0];
    block[1]  = sel_dims[1];
    start[0]  = ((hsize_t)mpi_rank * sel_dims[0]);
    start[1]  = 0;

    if (VERBOSE_MED) {
        HDprintf("Process %d is writing with count[ %" PRIuHSIZE ", %" PRIuHSIZE " ], stride[ %" PRIuHSIZE
                 ", %" PRIuHSIZE " ], start[ %" PRIuHSIZE ", %" PRIuHSIZE " ], block size[ %" PRIuHSIZE
                 ", %" PRIuHSIZE " ]\n",
                 mpi_rank, count[0], count[1], stride[0], stride[1], start[0], start[1], block[0], block[1]);
        HDfflush(stdout);
    }

    /* Select hyperslab in the file */
    filespace = H5Dget_space(dset_id);
    VRFY((filespace >= 0), "File dataspace retrieval succeeded");

    VRFY((H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, stride, count, block) >= 0),
         "Hyperslab selection succeeded");

    /* Fill data buffer */
    data_size = (hsize_t)WRITE_ONE_CHUNK_FILTERED_DATASET_CH_NROWS *
                (hsize_t)WRITE_ONE_CHUNK_FILTERED_DATASET_NCOLS * sizeof(*data);
    correct_buf_size = dataset_dims[0] * dataset_dims[1] * sizeof(*correct_buf);

    data = (C_DATATYPE *)HDcalloc(1, data_size);
    VRFY((NULL != data), "HDcalloc succeeded");

    correct_buf = (C_DATATYPE *)HDcalloc(1, correct_buf_size);
    VRFY((NULL != correct_buf), "HDcalloc succeeded");

    for (i = 0; i < data_size / sizeof(*data); i++)
        data[i] = (C_DATATYPE)GEN_DATA(i);

    for (i = 0; i < correct_buf_size / sizeof(*correct_buf); i++)
        correct_buf[i] = ((C_DATATYPE)i % (WRITE_ONE_CHUNK_FILTERED_DATASET_CH_NROWS / mpi_size *
                                           WRITE_ONE_CHUNK_FILTERED_DATASET_CH_NCOLS)) +
                         ((C_DATATYPE)i / (WRITE_ONE_CHUNK_FILTERED_DATASET_CH_NROWS / mpi_size *
                                           WRITE_ONE_CHUNK_FILTERED_DATASET_CH_NCOLS));

    VRFY((H5Dwrite(dset_id, HDF5_DATATYPE_NAME, memspace, filespace, dxpl_id, data) >= 0),
         "Dataset write succeeded");

    /* Verify space allocation status */
    verify_space_alloc_status(dset_id, plist_id, ALL_CHUNKS_WRITTEN);

    if (data)
        HDfree(data);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");

    /* Verify the correct data was written */
    read_buf = (C_DATATYPE *)HDcalloc(1, correct_buf_size);
    VRFY((NULL != read_buf), "HDcalloc succeeded");

    dset_id = H5Dopen2(group_id, WRITE_ONE_CHUNK_FILTERED_DATASET_NAME, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    VRFY((H5Dread(dset_id, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, dxpl_id, read_buf) >= 0),
         "Dataset read succeeded");

    VRFY((0 == HDmemcmp(read_buf, correct_buf, correct_buf_size)), "Data verification succeeded");

    if (correct_buf)
        HDfree(correct_buf);
    if (read_buf)
        HDfree(read_buf);

    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Sclose(memspace) >= 0), "Memory dataspace close succeeded");
    VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
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
test_write_filtered_dataset_no_overlap(const char *parent_group, H5Z_filter_t filter_id, hid_t fapl_id,
                                       hid_t dcpl_id, hid_t dxpl_id)
{
    C_DATATYPE *data        = NULL;
    C_DATATYPE *read_buf    = NULL;
    C_DATATYPE *correct_buf = NULL;
    hsize_t     dataset_dims[WRITE_UNSHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     chunk_dims[WRITE_UNSHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     sel_dims[WRITE_UNSHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     start[WRITE_UNSHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     stride[WRITE_UNSHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     count[WRITE_UNSHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     block[WRITE_UNSHARED_FILTERED_CHUNKS_DATASET_DIMS];
    size_t      i, data_size, correct_buf_size;
    hid_t       file_id = H5I_INVALID_HID, dset_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID;
    hid_t       group_id  = H5I_INVALID_HID;
    hid_t       filespace = H5I_INVALID_HID, memspace = H5I_INVALID_HID;

    if (MAINPROCESS)
        HDputs("Testing write to unshared filtered chunks");

    file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, fapl_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
    VRFY((group_id >= 0), "H5Gopen2 succeeded");

    /* Create the dataspace for the dataset */
    dataset_dims[0] = (hsize_t)WRITE_UNSHARED_FILTERED_CHUNKS_NROWS;
    dataset_dims[1] = (hsize_t)WRITE_UNSHARED_FILTERED_CHUNKS_NCOLS;
    chunk_dims[0]   = (hsize_t)WRITE_UNSHARED_FILTERED_CHUNKS_CH_NROWS;
    chunk_dims[1]   = (hsize_t)WRITE_UNSHARED_FILTERED_CHUNKS_CH_NCOLS;
    sel_dims[0]     = (hsize_t)WRITE_UNSHARED_FILTERED_CHUNKS_CH_NROWS;
    sel_dims[1]     = (hsize_t)WRITE_UNSHARED_FILTERED_CHUNKS_NCOLS;

    filespace = H5Screate_simple(WRITE_UNSHARED_FILTERED_CHUNKS_DATASET_DIMS, dataset_dims, NULL);
    VRFY((filespace >= 0), "File dataspace creation succeeded");

    memspace = H5Screate_simple(WRITE_UNSHARED_FILTERED_CHUNKS_DATASET_DIMS, sel_dims, NULL);
    VRFY((memspace >= 0), "Memory dataspace creation succeeded");

    /* Create chunked dataset */
    plist_id = H5Pcopy(dcpl_id);
    VRFY((plist_id >= 0), "DCPL copy succeeded");

    VRFY((H5Pset_chunk(plist_id, WRITE_UNSHARED_FILTERED_CHUNKS_DATASET_DIMS, chunk_dims) >= 0),
         "Chunk size set");

    /* Add test filter to the pipeline */
    VRFY((set_dcpl_filter(plist_id, filter_id, NULL) >= 0), "Filter set");

    dset_id = H5Dcreate2(group_id, WRITE_UNSHARED_FILTERED_CHUNKS_DATASET_NAME, HDF5_DATATYPE_NAME, filespace,
                         H5P_DEFAULT, plist_id, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset creation succeeded");

    /* Verify space allocation status */
    verify_space_alloc_status(dset_id, plist_id, DATASET_JUST_CREATED);

    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

    /* Each process defines the dataset selection in memory and writes
     * it to the hyperslab in the file
     */
    count[0] = 1;
    count[1] =
        (hsize_t)WRITE_UNSHARED_FILTERED_CHUNKS_NCOLS / (hsize_t)WRITE_UNSHARED_FILTERED_CHUNKS_CH_NCOLS;
    stride[0] = (hsize_t)WRITE_UNSHARED_FILTERED_CHUNKS_CH_NROWS;
    stride[1] = (hsize_t)WRITE_UNSHARED_FILTERED_CHUNKS_CH_NCOLS;
    block[0]  = (hsize_t)WRITE_UNSHARED_FILTERED_CHUNKS_CH_NROWS;
    block[1]  = (hsize_t)WRITE_UNSHARED_FILTERED_CHUNKS_CH_NCOLS;
    start[0]  = ((hsize_t)mpi_rank * (hsize_t)WRITE_UNSHARED_FILTERED_CHUNKS_CH_NROWS * count[0]);
    start[1]  = 0;

    if (VERBOSE_MED) {
        HDprintf("Process %d is writing with count[ %" PRIuHSIZE ", %" PRIuHSIZE " ], stride[ %" PRIuHSIZE
                 ", %" PRIuHSIZE " ], start[ %" PRIuHSIZE ", %" PRIuHSIZE " ], block size[ %" PRIuHSIZE
                 ", %" PRIuHSIZE " ]\n",
                 mpi_rank, count[0], count[1], stride[0], stride[1], start[0], start[1], block[0], block[1]);
        HDfflush(stdout);
    }

    /* Select hyperslab in the file */
    filespace = H5Dget_space(dset_id);
    VRFY((filespace >= 0), "File dataspace retrieval succeeded");

    VRFY((H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, stride, count, block) >= 0),
         "Hyperslab selection succeeded");

    /* Fill data buffer */
    data_size        = sel_dims[0] * sel_dims[1] * sizeof(*data);
    correct_buf_size = dataset_dims[0] * dataset_dims[1] * sizeof(*correct_buf);

    data = (C_DATATYPE *)HDcalloc(1, data_size);
    VRFY((NULL != data), "HDcalloc succeeded");

    correct_buf = (C_DATATYPE *)HDcalloc(1, correct_buf_size);
    VRFY((NULL != correct_buf), "HDcalloc succeeded");

    for (i = 0; i < data_size / sizeof(*data); i++)
        data[i] = (C_DATATYPE)GEN_DATA(i);

    for (i = 0; i < correct_buf_size / sizeof(*correct_buf); i++)
        correct_buf[i] = (C_DATATYPE)((i % (dataset_dims[0] / (hsize_t)mpi_size * dataset_dims[1])) +
                                      (i / (dataset_dims[0] / (hsize_t)mpi_size * dataset_dims[1])));

    VRFY((H5Dwrite(dset_id, HDF5_DATATYPE_NAME, memspace, filespace, dxpl_id, data) >= 0),
         "Dataset write succeeded");

    /* Verify space allocation status */
    verify_space_alloc_status(dset_id, plist_id, ALL_CHUNKS_WRITTEN);

    if (data)
        HDfree(data);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");

    /* Verify the correct data was written */
    read_buf = (C_DATATYPE *)HDcalloc(1, correct_buf_size);
    VRFY((NULL != read_buf), "HDcalloc succeeded");

    dset_id = H5Dopen2(group_id, WRITE_UNSHARED_FILTERED_CHUNKS_DATASET_NAME, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    VRFY((H5Dread(dset_id, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, dxpl_id, read_buf) >= 0),
         "Dataset read succeeded");

    VRFY((0 == HDmemcmp(read_buf, correct_buf, correct_buf_size)), "Data verification succeeded");

    if (correct_buf)
        HDfree(correct_buf);
    if (read_buf)
        HDfree(read_buf);

    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Sclose(memspace) >= 0), "Memory dataspace close succeeded");
    VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    return;
}

/*
 * Tests parallel write of filtered data in the case where only
 * one process is writing to a particular chunk in the operation
 * and that process only writes to part of a chunk.
 */
static void
test_write_filtered_dataset_no_overlap_partial(const char *parent_group, H5Z_filter_t filter_id,
                                               hid_t fapl_id, hid_t dcpl_id, hid_t dxpl_id)
{
    C_DATATYPE *data        = NULL;
    C_DATATYPE *read_buf    = NULL;
    C_DATATYPE *correct_buf = NULL;
    hsize_t     dataset_dims[WRITE_UNSHARED_FILTERED_CHUNKS_PARTIAL_DATASET_DIMS];
    hsize_t     chunk_dims[WRITE_UNSHARED_FILTERED_CHUNKS_PARTIAL_DATASET_DIMS];
    hsize_t     sel_dims[WRITE_UNSHARED_FILTERED_CHUNKS_PARTIAL_DATASET_DIMS];
    hsize_t     start[WRITE_UNSHARED_FILTERED_CHUNKS_PARTIAL_DATASET_DIMS];
    hsize_t     stride[WRITE_UNSHARED_FILTERED_CHUNKS_PARTIAL_DATASET_DIMS];
    hsize_t     count[WRITE_UNSHARED_FILTERED_CHUNKS_PARTIAL_DATASET_DIMS];
    hsize_t     block[WRITE_UNSHARED_FILTERED_CHUNKS_PARTIAL_DATASET_DIMS];
    size_t      i, data_size, correct_buf_size;
    hid_t       file_id = H5I_INVALID_HID, dset_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID;
    hid_t       group_id  = H5I_INVALID_HID;
    hid_t       filespace = H5I_INVALID_HID;

    if (MAINPROCESS)
        HDputs("Testing partial write to unshared filtered chunks");

    file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, fapl_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
    VRFY((group_id >= 0), "H5Gopen2 succeeded");

    /* Create the dataspace for the dataset */
    dataset_dims[0] = (hsize_t)WRITE_UNSHARED_FILTERED_CHUNKS_PARTIAL_NROWS;
    dataset_dims[1] = (hsize_t)WRITE_UNSHARED_FILTERED_CHUNKS_PARTIAL_NCOLS;
    chunk_dims[0]   = (hsize_t)WRITE_UNSHARED_FILTERED_CHUNKS_PARTIAL_CH_NROWS;
    chunk_dims[1]   = (hsize_t)WRITE_UNSHARED_FILTERED_CHUNKS_PARTIAL_CH_NCOLS;
    sel_dims[0]     = (hsize_t)WRITE_UNSHARED_FILTERED_CHUNKS_PARTIAL_CH_NROWS;
    sel_dims[1]     = (hsize_t)(WRITE_UNSHARED_FILTERED_CHUNKS_PARTIAL_NCOLS /
                            WRITE_UNSHARED_FILTERED_CHUNKS_PARTIAL_CH_NCOLS);

    filespace = H5Screate_simple(WRITE_UNSHARED_FILTERED_CHUNKS_PARTIAL_DATASET_DIMS, dataset_dims, NULL);
    VRFY((filespace >= 0), "File dataspace creation succeeded");

    /* Create chunked dataset */
    plist_id = H5Pcopy(dcpl_id);
    VRFY((plist_id >= 0), "DCPL copy succeeded");

    VRFY((H5Pset_chunk(plist_id, WRITE_UNSHARED_FILTERED_CHUNKS_PARTIAL_DATASET_DIMS, chunk_dims) >= 0),
         "Chunk size set");

    /* Add test filter to the pipeline */
    VRFY((set_dcpl_filter(plist_id, filter_id, NULL) >= 0), "Filter set");

    dset_id = H5Dcreate2(group_id, WRITE_UNSHARED_FILTERED_CHUNKS_PARTIAL_DATASET_NAME, HDF5_DATATYPE_NAME,
                         filespace, H5P_DEFAULT, plist_id, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset creation succeeded");

    /* Verify space allocation status */
    verify_space_alloc_status(dset_id, plist_id, DATASET_JUST_CREATED);

    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

    /* Each process defines the dataset selection in memory and writes
     * it to the hyperslab in the file
     */
    count[0]  = 1;
    count[1]  = (hsize_t)(WRITE_UNSHARED_FILTERED_CHUNKS_PARTIAL_NCOLS /
                         WRITE_UNSHARED_FILTERED_CHUNKS_PARTIAL_CH_NCOLS);
    stride[0] = (hsize_t)WRITE_UNSHARED_FILTERED_CHUNKS_PARTIAL_CH_NROWS;
    stride[1] = (hsize_t)WRITE_UNSHARED_FILTERED_CHUNKS_PARTIAL_CH_NCOLS;
    block[0]  = (hsize_t)WRITE_UNSHARED_FILTERED_CHUNKS_PARTIAL_CH_NROWS;
    block[1]  = (hsize_t)1;
    start[0]  = ((hsize_t)mpi_rank * (hsize_t)WRITE_UNSHARED_FILTERED_CHUNKS_PARTIAL_CH_NROWS * count[0]);
    start[1]  = 0;

    if (VERBOSE_MED) {
        HDprintf("Process %d is writing with count[ %" PRIuHSIZE ", %" PRIuHSIZE " ], stride[ %" PRIuHSIZE
                 ", %" PRIuHSIZE " ], start[ %" PRIuHSIZE ", %" PRIuHSIZE " ], block size[ %" PRIuHSIZE
                 ", %" PRIuHSIZE " ]\n",
                 mpi_rank, count[0], count[1], stride[0], stride[1], start[0], start[1], block[0], block[1]);
        HDfflush(stdout);
    }

    /* Select hyperslab in the file */
    filespace = H5Dget_space(dset_id);
    VRFY((filespace >= 0), "File dataspace retrieval succeeded");

    VRFY((H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, stride, count, block) >= 0),
         "Hyperslab selection succeeded");

    /* Fill data buffer */
    data_size        = sel_dims[0] * sel_dims[1] * sizeof(*data);
    correct_buf_size = dataset_dims[0] * dataset_dims[1] * sizeof(*correct_buf);

    data = (C_DATATYPE *)HDcalloc(1, data_size);
    VRFY((NULL != data), "HDcalloc succeeded");

    correct_buf = (C_DATATYPE *)HDcalloc(1, correct_buf_size);
    VRFY((NULL != correct_buf), "HDcalloc succeeded");

    for (i = 0; i < data_size / sizeof(*data); i++)
        data[i] = (C_DATATYPE)GEN_DATA(i);

    for (i = 0; i < (size_t)mpi_size; i++) {
        size_t rank_n_elems = (size_t)(mpi_size * (WRITE_UNSHARED_FILTERED_CHUNKS_PARTIAL_CH_NROWS *
                                                   WRITE_UNSHARED_FILTERED_CHUNKS_PARTIAL_CH_NCOLS));
        size_t data_idx     = i;

        for (size_t j = 0; j < rank_n_elems; j++) {
            if ((j % WRITE_UNSHARED_FILTERED_CHUNKS_PARTIAL_CH_NCOLS) == 0) {
                correct_buf[(i * rank_n_elems) + j] = (C_DATATYPE)data_idx;
                data_idx++;
            }
        }
    }

    VRFY((H5Dwrite(dset_id, HDF5_DATATYPE_NAME, H5S_BLOCK, filespace, dxpl_id, data) >= 0),
         "Dataset write succeeded");

    /* Verify space allocation status */
    verify_space_alloc_status(dset_id, plist_id, ALL_CHUNKS_WRITTEN);

    if (data)
        HDfree(data);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");

    /* Verify the correct data was written */
    read_buf = (C_DATATYPE *)HDcalloc(1, correct_buf_size);
    VRFY((NULL != read_buf), "HDcalloc succeeded");

    dset_id = H5Dopen2(group_id, WRITE_UNSHARED_FILTERED_CHUNKS_PARTIAL_DATASET_NAME, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    VRFY((H5Dread(dset_id, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, dxpl_id, read_buf) >= 0),
         "Dataset read succeeded");

    VRFY((0 == HDmemcmp(read_buf, correct_buf, correct_buf_size)), "Data verification succeeded");

    if (correct_buf)
        HDfree(correct_buf);
    if (read_buf)
        HDfree(read_buf);

    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
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
test_write_filtered_dataset_overlap(const char *parent_group, H5Z_filter_t filter_id, hid_t fapl_id,
                                    hid_t dcpl_id, hid_t dxpl_id)
{
    C_DATATYPE *data        = NULL;
    C_DATATYPE *read_buf    = NULL;
    C_DATATYPE *correct_buf = NULL;
    hsize_t     dataset_dims[WRITE_SHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     chunk_dims[WRITE_SHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     sel_dims[WRITE_SHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     start[WRITE_SHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     stride[WRITE_SHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     count[WRITE_SHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     block[WRITE_SHARED_FILTERED_CHUNKS_DATASET_DIMS];
    size_t      i, data_size, correct_buf_size;
    hid_t       file_id = H5I_INVALID_HID, dset_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID;
    hid_t       group_id  = H5I_INVALID_HID;
    hid_t       filespace = H5I_INVALID_HID, memspace = H5I_INVALID_HID;

    if (MAINPROCESS)
        HDputs("Testing write to shared filtered chunks");

    file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, fapl_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
    VRFY((group_id >= 0), "H5Gopen2 succeeded");

    /* Create the dataspace for the dataset */
    dataset_dims[0] = (hsize_t)WRITE_SHARED_FILTERED_CHUNKS_NROWS;
    dataset_dims[1] = (hsize_t)WRITE_SHARED_FILTERED_CHUNKS_NCOLS;
    chunk_dims[0]   = (hsize_t)WRITE_SHARED_FILTERED_CHUNKS_CH_NROWS;
    chunk_dims[1]   = (hsize_t)WRITE_SHARED_FILTERED_CHUNKS_CH_NCOLS;
    sel_dims[0]     = (hsize_t)DIM0_SCALE_FACTOR;
    sel_dims[1]     = (hsize_t)WRITE_SHARED_FILTERED_CHUNKS_CH_NCOLS * (hsize_t)DIM1_SCALE_FACTOR;

    filespace = H5Screate_simple(WRITE_SHARED_FILTERED_CHUNKS_DATASET_DIMS, dataset_dims, NULL);
    VRFY((filespace >= 0), "File dataspace creation succeeded");

    memspace = H5Screate_simple(WRITE_SHARED_FILTERED_CHUNKS_DATASET_DIMS, sel_dims, NULL);
    VRFY((memspace >= 0), "Memory dataspace creation succeeded");

    /* Create chunked dataset */
    plist_id = H5Pcopy(dcpl_id);
    VRFY((plist_id >= 0), "DCPL copy succeeded");

    VRFY((H5Pset_chunk(plist_id, WRITE_SHARED_FILTERED_CHUNKS_DATASET_DIMS, chunk_dims) >= 0),
         "Chunk size set");

    /* Add test filter to the pipeline */
    VRFY((set_dcpl_filter(plist_id, filter_id, NULL) >= 0), "Filter set");

    dset_id = H5Dcreate2(group_id, WRITE_SHARED_FILTERED_CHUNKS_DATASET_NAME, HDF5_DATATYPE_NAME, filespace,
                         H5P_DEFAULT, plist_id, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset creation succeeded");

    /* Verify space allocation status */
    verify_space_alloc_status(dset_id, plist_id, DATASET_JUST_CREATED);

    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

    /* Each process defines the dataset selection in memory and writes
     * it to the hyperslab in the file
     */
    count[0]  = (hsize_t)WRITE_SHARED_FILTERED_CHUNKS_NROWS / (hsize_t)WRITE_SHARED_FILTERED_CHUNKS_CH_NROWS;
    count[1]  = (hsize_t)WRITE_SHARED_FILTERED_CHUNKS_NCOLS / (hsize_t)WRITE_SHARED_FILTERED_CHUNKS_CH_NCOLS;
    stride[0] = (hsize_t)WRITE_SHARED_FILTERED_CHUNKS_CH_NROWS;
    stride[1] = (hsize_t)WRITE_SHARED_FILTERED_CHUNKS_CH_NCOLS;
    block[0]  = (hsize_t)WRITE_SHARED_FILTERED_CHUNKS_CH_NROWS / (hsize_t)mpi_size;
    block[1]  = (hsize_t)WRITE_SHARED_FILTERED_CHUNKS_CH_NCOLS;
    start[0]  = (hsize_t)mpi_rank * block[0];
    start[1]  = 0;

    if (VERBOSE_MED) {
        HDprintf("Process %d is writing with count[ %" PRIuHSIZE ", %" PRIuHSIZE " ], stride[ %" PRIuHSIZE
                 ", %" PRIuHSIZE " ], start[ %" PRIuHSIZE ", %" PRIuHSIZE " ], block size[ %" PRIuHSIZE
                 ", %" PRIuHSIZE " ]\n",
                 mpi_rank, count[0], count[1], stride[0], stride[1], start[0], start[1], block[0], block[1]);
        HDfflush(stdout);
    }

    /* Select hyperslab in the file */
    filespace = H5Dget_space(dset_id);
    VRFY((filespace >= 0), "File dataspace retrieval succeeded");

    VRFY((H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, stride, count, block) >= 0),
         "Hyperslab selection succeeded");

    /* Fill data buffer */
    data_size        = sel_dims[0] * sel_dims[1] * sizeof(*data);
    correct_buf_size = dataset_dims[0] * dataset_dims[1] * sizeof(*correct_buf);

    data = (C_DATATYPE *)HDcalloc(1, data_size);
    VRFY((NULL != data), "HDcalloc succeeded");

    correct_buf = (C_DATATYPE *)HDcalloc(1, correct_buf_size);
    VRFY((NULL != correct_buf), "HDcalloc succeeded");

    for (i = 0; i < data_size / sizeof(*data); i++)
        data[i] = (C_DATATYPE)GEN_DATA(i);

    for (i = 0; i < correct_buf_size / sizeof(*correct_buf); i++)
        correct_buf[i] =
            (C_DATATYPE)((dataset_dims[1] * (i / ((hsize_t)mpi_size * dataset_dims[1]))) +
                         (i % dataset_dims[1]) +
                         (((i % ((hsize_t)mpi_size * dataset_dims[1])) / dataset_dims[1]) % dataset_dims[1]));

    VRFY((H5Dwrite(dset_id, HDF5_DATATYPE_NAME, memspace, filespace, dxpl_id, data) >= 0),
         "Dataset write succeeded");

    /* Verify space allocation status */
    verify_space_alloc_status(dset_id, plist_id, ALL_CHUNKS_WRITTEN);

    if (data)
        HDfree(data);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");

    /* Verify correct data was written */
    read_buf = (C_DATATYPE *)HDcalloc(1, correct_buf_size);
    VRFY((NULL != read_buf), "HDcalloc succeeded");

    dset_id = H5Dopen2(group_id, WRITE_SHARED_FILTERED_CHUNKS_DATASET_NAME, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    VRFY((H5Dread(dset_id, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, dxpl_id, read_buf) >= 0),
         "Dataset read succeeded");

    VRFY((0 == HDmemcmp(read_buf, correct_buf, correct_buf_size)), "Data verification succeeded");

    if (correct_buf)
        HDfree(correct_buf);
    if (read_buf)
        HDfree(read_buf);

    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Sclose(memspace) >= 0), "Memory dataspace close succeeded");
    VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    return;
}

/*
 * Tests parallel write of filtered data in the case where
 * a dataset has a single unlimited dimension and each
 * MPI rank writes to its own separate chunk. On each
 * iteration, the dataset is extended in its extensible
 * dimension by "MPI size" chunks per rank and the new
 * chunks are written to, read back and verified.
 */
static void
test_write_filtered_dataset_single_unlim_dim_no_overlap(const char *parent_group, H5Z_filter_t filter_id,
                                                        hid_t fapl_id, hid_t dcpl_id, hid_t dxpl_id)
{
    C_DATATYPE *data     = NULL;
    C_DATATYPE *read_buf = NULL;
    hsize_t     dataset_dims[WRITE_UNSHARED_ONE_UNLIM_DIM_DATASET_DIMS];
    hsize_t     max_dims[WRITE_UNSHARED_ONE_UNLIM_DIM_DATASET_DIMS];
    hsize_t     chunk_dims[WRITE_UNSHARED_ONE_UNLIM_DIM_DATASET_DIMS];
    hsize_t     sel_dims[WRITE_UNSHARED_ONE_UNLIM_DIM_DATASET_DIMS];
    hsize_t     start[WRITE_UNSHARED_ONE_UNLIM_DIM_DATASET_DIMS];
    hsize_t     stride[WRITE_UNSHARED_ONE_UNLIM_DIM_DATASET_DIMS];
    hsize_t     count[WRITE_UNSHARED_ONE_UNLIM_DIM_DATASET_DIMS];
    hsize_t     block[WRITE_UNSHARED_ONE_UNLIM_DIM_DATASET_DIMS];
    size_t      i, data_size;
    hid_t       file_id = H5I_INVALID_HID, dset_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID;
    hid_t       group_id  = H5I_INVALID_HID;
    hid_t       filespace = H5I_INVALID_HID;

    if (MAINPROCESS)
        HDputs("Testing write to unshared filtered chunks w/ single unlimited dimension");

    file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, fapl_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
    VRFY((group_id >= 0), "H5Gopen2 succeeded");

    /* Create the dataspace for the dataset */
    dataset_dims[0] = (hsize_t)WRITE_UNSHARED_ONE_UNLIM_DIM_NROWS;
    dataset_dims[1] = (hsize_t)WRITE_UNSHARED_ONE_UNLIM_DIM_NCOLS;
    max_dims[0]     = dataset_dims[0];
    max_dims[1]     = H5S_UNLIMITED;
    chunk_dims[0]   = (hsize_t)WRITE_UNSHARED_ONE_UNLIM_DIM_CH_NROWS;
    chunk_dims[1]   = (hsize_t)WRITE_UNSHARED_ONE_UNLIM_DIM_CH_NCOLS;
    sel_dims[0]     = (hsize_t)WRITE_UNSHARED_ONE_UNLIM_DIM_CH_NROWS;
    sel_dims[1]     = (hsize_t)WRITE_UNSHARED_ONE_UNLIM_DIM_NCOLS;

    filespace = H5Screate_simple(WRITE_UNSHARED_ONE_UNLIM_DIM_DATASET_DIMS, dataset_dims, max_dims);
    VRFY((filespace >= 0), "File dataspace creation succeeded");

    /* Create chunked dataset */
    plist_id = H5Pcopy(dcpl_id);
    VRFY((plist_id >= 0), "DCPL copy succeeded");

    VRFY((H5Pset_chunk(plist_id, WRITE_UNSHARED_ONE_UNLIM_DIM_DATASET_DIMS, chunk_dims) >= 0),
         "Chunk size set");

    /* Add test filter to the pipeline */
    VRFY((set_dcpl_filter(plist_id, filter_id, NULL) >= 0), "Filter set");

    dset_id = H5Dcreate2(group_id, WRITE_UNSHARED_ONE_UNLIM_DIM_DATASET_NAME, HDF5_DATATYPE_NAME, filespace,
                         H5P_DEFAULT, plist_id, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset creation succeeded");

    /* Verify space allocation status */
    verify_space_alloc_status(dset_id, plist_id, DATASET_JUST_CREATED);

    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

    /* Fill data buffer */
    data_size = sel_dims[0] * sel_dims[1] * sizeof(*data);

    data = (C_DATATYPE *)HDcalloc(1, data_size);
    VRFY((NULL != data), "HDcalloc succeeded");

    read_buf = (C_DATATYPE *)HDcalloc(1, data_size);
    VRFY((NULL != read_buf), "HDcalloc succeeded");

    for (i = 0; i < data_size / sizeof(*data); i++)
        data[i] = (C_DATATYPE)GEN_DATA(i);

    for (i = 0; i < (size_t)WRITE_UNSHARED_ONE_UNLIM_DIM_NLOOPS; i++) {
        /* Select hyperslab in the file */
        filespace = H5Dget_space(dset_id);
        VRFY((filespace >= 0), "File dataspace retrieval succeeded");

        /* Each process defines the dataset selection in memory and writes
         * it to the hyperslab in the file
         */
        count[0] = 1;
        count[1] =
            (hsize_t)WRITE_UNSHARED_ONE_UNLIM_DIM_NCOLS / (hsize_t)WRITE_UNSHARED_ONE_UNLIM_DIM_CH_NCOLS;
        stride[0] = (hsize_t)WRITE_UNSHARED_ONE_UNLIM_DIM_CH_NROWS;
        stride[1] = (hsize_t)WRITE_UNSHARED_ONE_UNLIM_DIM_CH_NCOLS;
        block[0]  = (hsize_t)WRITE_UNSHARED_ONE_UNLIM_DIM_CH_NROWS;
        block[1]  = (hsize_t)WRITE_UNSHARED_ONE_UNLIM_DIM_CH_NCOLS;
        start[0]  = ((hsize_t)mpi_rank * block[0] * count[0]);
        start[1]  = i * count[1] * block[1];

        if (VERBOSE_MED) {
            HDprintf("Process %d is writing with count[ %" PRIuHSIZE ", %" PRIuHSIZE " ], stride[ %" PRIuHSIZE
                     ", %" PRIuHSIZE " ], start[ %" PRIuHSIZE ", %" PRIuHSIZE " ], block size[ %" PRIuHSIZE
                     ", %" PRIuHSIZE " ]\n",
                     mpi_rank, count[0], count[1], stride[0], stride[1], start[0], start[1], block[0],
                     block[1]);
            HDfflush(stdout);
        }

        VRFY((H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, stride, count, block) >= 0),
             "Hyperslab selection succeeded");

        VRFY((H5Dwrite(dset_id, HDF5_DATATYPE_NAME, H5S_BLOCK, filespace, dxpl_id, data) >= 0),
             "Dataset write succeeded");

        /* Verify space allocation status */
        verify_space_alloc_status(dset_id, plist_id, ALL_CHUNKS_WRITTEN);

        VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");

        dset_id = H5Dopen2(group_id, WRITE_UNSHARED_ONE_UNLIM_DIM_DATASET_NAME, H5P_DEFAULT);
        VRFY((dset_id >= 0), "Dataset open succeeded");

        HDmemset(read_buf, 255, data_size);

        VRFY((H5Dread(dset_id, HDF5_DATATYPE_NAME, H5S_BLOCK, filespace, dxpl_id, read_buf) >= 0),
             "Dataset read succeeded");

        /* Verify the correct data was written */
        VRFY((0 == HDmemcmp(read_buf, data, data_size)), "Data verification succeeded");

        if (i < (size_t)WRITE_UNSHARED_ONE_UNLIM_DIM_NLOOPS - 1) {
            /* Extend the dataset by count[1] chunks in the extensible dimension */
            dataset_dims[1] += count[1] * block[1];
            VRFY(H5Dset_extent(dset_id, dataset_dims) >= 0, "H5Dset_extent succeeded");

            /* Verify space allocation status */
            verify_space_alloc_status(dset_id, plist_id, SOME_CHUNKS_WRITTEN);
        }

        VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    }

    if (data)
        HDfree(data);
    if (read_buf)
        HDfree(read_buf);

    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
    VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    return;
}

/*
 * Tests parallel write of filtered data in the case where
 * a dataset has a single unlimited dimension and each
 * MPI rank writes to a portion of each chunk in the dataset.
 * On each iteration, the dataset is extended in its extensible
 * dimension by two chunks and the new chunks are written to
 * by all ranks, then read back and verified.
 */
static void
test_write_filtered_dataset_single_unlim_dim_overlap(const char *parent_group, H5Z_filter_t filter_id,
                                                     hid_t fapl_id, hid_t dcpl_id, hid_t dxpl_id)
{
    C_DATATYPE *data     = NULL;
    C_DATATYPE *read_buf = NULL;
    hsize_t     dataset_dims[WRITE_SHARED_ONE_UNLIM_DIM_DATASET_DIMS];
    hsize_t     max_dims[WRITE_SHARED_ONE_UNLIM_DIM_DATASET_DIMS];
    hsize_t     chunk_dims[WRITE_SHARED_ONE_UNLIM_DIM_DATASET_DIMS];
    hsize_t     sel_dims[WRITE_SHARED_ONE_UNLIM_DIM_DATASET_DIMS];
    hsize_t     start[WRITE_SHARED_ONE_UNLIM_DIM_DATASET_DIMS];
    hsize_t     stride[WRITE_SHARED_ONE_UNLIM_DIM_DATASET_DIMS];
    hsize_t     count[WRITE_SHARED_ONE_UNLIM_DIM_DATASET_DIMS];
    hsize_t     block[WRITE_SHARED_ONE_UNLIM_DIM_DATASET_DIMS];
    size_t      i, data_size;
    hid_t       file_id = H5I_INVALID_HID, dset_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID;
    hid_t       group_id  = H5I_INVALID_HID;
    hid_t       filespace = H5I_INVALID_HID;

    if (MAINPROCESS)
        HDputs("Testing write to shared filtered chunks w/ single unlimited dimension");

    file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, fapl_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
    VRFY((group_id >= 0), "H5Gopen2 succeeded");

    /* Create the dataspace for the dataset */
    dataset_dims[0] = (hsize_t)WRITE_SHARED_ONE_UNLIM_DIM_NROWS;
    dataset_dims[1] = (hsize_t)WRITE_SHARED_ONE_UNLIM_DIM_NCOLS;
    max_dims[0]     = dataset_dims[0];
    max_dims[1]     = H5S_UNLIMITED;
    chunk_dims[0]   = (hsize_t)WRITE_SHARED_ONE_UNLIM_DIM_CH_NROWS;
    chunk_dims[1]   = (hsize_t)WRITE_SHARED_ONE_UNLIM_DIM_CH_NCOLS;
    sel_dims[0]     = (hsize_t)DIM0_SCALE_FACTOR;
    sel_dims[1]     = (hsize_t)WRITE_SHARED_ONE_UNLIM_DIM_CH_NCOLS * (hsize_t)DIM1_SCALE_FACTOR;

    filespace = H5Screate_simple(WRITE_SHARED_ONE_UNLIM_DIM_DATASET_DIMS, dataset_dims, max_dims);
    VRFY((filespace >= 0), "File dataspace creation succeeded");

    /* Create chunked dataset */
    plist_id = H5Pcopy(dcpl_id);
    VRFY((plist_id >= 0), "DCPL copy succeeded");

    VRFY((H5Pset_chunk(plist_id, WRITE_SHARED_ONE_UNLIM_DIM_DATASET_DIMS, chunk_dims) >= 0),
         "Chunk size set");

    /* Add test filter to the pipeline */
    VRFY((set_dcpl_filter(plist_id, filter_id, NULL) >= 0), "Filter set");

    dset_id = H5Dcreate2(group_id, WRITE_SHARED_ONE_UNLIM_DIM_DATASET_NAME, HDF5_DATATYPE_NAME, filespace,
                         H5P_DEFAULT, plist_id, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset creation succeeded");

    /* Verify space allocation status */
    verify_space_alloc_status(dset_id, plist_id, DATASET_JUST_CREATED);

    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

    /* Fill data buffer */
    data_size = sel_dims[0] * sel_dims[1] * sizeof(*data);

    data = (C_DATATYPE *)HDcalloc(1, data_size);
    VRFY((NULL != data), "HDcalloc succeeded");

    read_buf = (C_DATATYPE *)HDcalloc(1, data_size);
    VRFY((NULL != read_buf), "HDcalloc succeeded");

    for (i = 0; i < data_size / sizeof(*data); i++)
        data[i] = (C_DATATYPE)GEN_DATA(i);

    for (i = 0; i < (size_t)WRITE_SHARED_ONE_UNLIM_DIM_NLOOPS; i++) {
        /* Select hyperslab in the file */
        filespace = H5Dget_space(dset_id);
        VRFY((filespace >= 0), "File dataspace retrieval succeeded");

        /* Each process defines the dataset selection in memory and writes
         * it to the hyperslab in the file
         */
        count[0]  = (hsize_t)WRITE_SHARED_ONE_UNLIM_DIM_NROWS / (hsize_t)WRITE_SHARED_ONE_UNLIM_DIM_CH_NROWS;
        count[1]  = (hsize_t)WRITE_SHARED_ONE_UNLIM_DIM_NCOLS / (hsize_t)WRITE_SHARED_ONE_UNLIM_DIM_CH_NCOLS;
        stride[0] = (hsize_t)WRITE_SHARED_ONE_UNLIM_DIM_CH_NROWS;
        stride[1] = (hsize_t)WRITE_SHARED_ONE_UNLIM_DIM_CH_NCOLS;
        block[0]  = (hsize_t)WRITE_SHARED_ONE_UNLIM_DIM_CH_NROWS / (hsize_t)mpi_size;
        block[1]  = (hsize_t)WRITE_SHARED_ONE_UNLIM_DIM_CH_NCOLS;
        start[0]  = (hsize_t)mpi_rank * block[0];
        start[1]  = i * count[1] * block[1];

        if (VERBOSE_MED) {
            HDprintf("Process %d is writing with count[ %" PRIuHSIZE ", %" PRIuHSIZE " ], stride[ %" PRIuHSIZE
                     ", %" PRIuHSIZE " ], start[ %" PRIuHSIZE ", %" PRIuHSIZE " ], block size[ %" PRIuHSIZE
                     ", %" PRIuHSIZE " ]\n",
                     mpi_rank, count[0], count[1], stride[0], stride[1], start[0], start[1], block[0],
                     block[1]);
            HDfflush(stdout);
        }

        VRFY((H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, stride, count, block) >= 0),
             "Hyperslab selection succeeded");

        VRFY((H5Dwrite(dset_id, HDF5_DATATYPE_NAME, H5S_BLOCK, filespace, dxpl_id, data) >= 0),
             "Dataset write succeeded");

        /* Verify space allocation status */
        verify_space_alloc_status(dset_id, plist_id, ALL_CHUNKS_WRITTEN);

        VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");

        dset_id = H5Dopen2(group_id, WRITE_SHARED_ONE_UNLIM_DIM_DATASET_NAME, H5P_DEFAULT);
        VRFY((dset_id >= 0), "Dataset open succeeded");

        HDmemset(read_buf, 255, data_size);

        VRFY((H5Dread(dset_id, HDF5_DATATYPE_NAME, H5S_BLOCK, filespace, dxpl_id, read_buf) >= 0),
             "Dataset read succeeded");

        /* Verify correct data was written */
        VRFY((0 == HDmemcmp(read_buf, data, data_size)), "Data verification succeeded");

        if (i < (size_t)WRITE_SHARED_ONE_UNLIM_DIM_NLOOPS - 1) {
            /* Extend the dataset by count[1] chunks in the extensible dimension */
            dataset_dims[1] += count[1] * block[1];
            VRFY(H5Dset_extent(dset_id, dataset_dims) >= 0, "H5Dset_extent succeeded");

            /* Verify space allocation status */
            verify_space_alloc_status(dset_id, plist_id, SOME_CHUNKS_WRITTEN);
        }

        VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    }

    if (data)
        HDfree(data);
    if (read_buf)
        HDfree(read_buf);

    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
    VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    return;
}

/*
 * Tests parallel write of filtered data in the case where
 * a dataset has two unlimited dimensions and each
 * MPI rank writes to its own separate chunks. On each
 * iteration, the dataset is extended in its first
 * extensible dimension by the size of one chunk per rank
 * and in its second extensible dimension by the size of
 * one chunk. Then, all chunks are written to, read back
 * and verified.
 */
static void
test_write_filtered_dataset_multi_unlim_dim_no_overlap(const char *parent_group, H5Z_filter_t filter_id,
                                                       hid_t fapl_id, hid_t dcpl_id, hid_t dxpl_id)
{
    C_DATATYPE *data     = NULL;
    C_DATATYPE *read_buf = NULL;
    hsize_t     dataset_dims[WRITE_UNSHARED_TWO_UNLIM_DIM_DATASET_DIMS];
    hsize_t     max_dims[WRITE_UNSHARED_TWO_UNLIM_DIM_DATASET_DIMS];
    hsize_t     chunk_dims[WRITE_UNSHARED_TWO_UNLIM_DIM_DATASET_DIMS];
    hsize_t     sel_dims[WRITE_UNSHARED_TWO_UNLIM_DIM_DATASET_DIMS];
    hsize_t     start[WRITE_UNSHARED_TWO_UNLIM_DIM_DATASET_DIMS];
    hsize_t     stride[WRITE_UNSHARED_TWO_UNLIM_DIM_DATASET_DIMS];
    hsize_t     count[WRITE_UNSHARED_TWO_UNLIM_DIM_DATASET_DIMS];
    hsize_t     block[WRITE_UNSHARED_TWO_UNLIM_DIM_DATASET_DIMS];
    size_t      i, data_size;
    hid_t       file_id = H5I_INVALID_HID, dset_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID;
    hid_t       group_id  = H5I_INVALID_HID;
    hid_t       filespace = H5I_INVALID_HID;

    if (MAINPROCESS)
        HDputs("Testing write to unshared filtered chunks w/ two unlimited dimensions");

    file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, fapl_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
    VRFY((group_id >= 0), "H5Gopen2 succeeded");

    /* Create the dataspace for the dataset */
    dataset_dims[0] = (hsize_t)WRITE_UNSHARED_TWO_UNLIM_DIM_NROWS;
    dataset_dims[1] = (hsize_t)WRITE_UNSHARED_TWO_UNLIM_DIM_NCOLS;
    max_dims[0]     = H5S_UNLIMITED;
    max_dims[1]     = H5S_UNLIMITED;
    chunk_dims[0]   = (hsize_t)WRITE_UNSHARED_TWO_UNLIM_DIM_CH_NROWS;
    chunk_dims[1]   = (hsize_t)WRITE_UNSHARED_TWO_UNLIM_DIM_CH_NCOLS;
    sel_dims[0]     = (hsize_t)WRITE_UNSHARED_TWO_UNLIM_DIM_CH_NROWS;
    sel_dims[1]     = (hsize_t)WRITE_UNSHARED_TWO_UNLIM_DIM_NCOLS;

    filespace = H5Screate_simple(WRITE_UNSHARED_TWO_UNLIM_DIM_DATASET_DIMS, dataset_dims, max_dims);
    VRFY((filespace >= 0), "File dataspace creation succeeded");

    /* Create chunked dataset */
    plist_id = H5Pcopy(dcpl_id);
    VRFY((plist_id >= 0), "DCPL copy succeeded");

    VRFY((H5Pset_chunk(plist_id, WRITE_UNSHARED_TWO_UNLIM_DIM_DATASET_DIMS, chunk_dims) >= 0),
         "Chunk size set");

    /* Add test filter to the pipeline */
    VRFY((set_dcpl_filter(plist_id, filter_id, NULL) >= 0), "Filter set");

    dset_id = H5Dcreate2(group_id, WRITE_UNSHARED_TWO_UNLIM_DIM_DATASET_NAME, HDF5_DATATYPE_NAME, filespace,
                         H5P_DEFAULT, plist_id, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset creation succeeded");

    /* Verify space allocation status */
    verify_space_alloc_status(dset_id, plist_id, DATASET_JUST_CREATED);

    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

    for (i = 0; i < (size_t)WRITE_UNSHARED_TWO_UNLIM_DIM_NLOOPS; i++) {
        C_DATATYPE *tmp_realloc = NULL;
        size_t      j;

        /* Set selected dimensions */
        sel_dims[0] = (i + 1) * WRITE_UNSHARED_TWO_UNLIM_DIM_CH_NROWS;
        sel_dims[1] = (i + 1) * WRITE_UNSHARED_TWO_UNLIM_DIM_CH_NCOLS;

        /* Fill data buffer */
        data_size = sel_dims[0] * sel_dims[1] * sizeof(*data);

        tmp_realloc = (C_DATATYPE *)HDrealloc(data, data_size);
        VRFY((NULL != tmp_realloc), "HDrealloc succeeded");
        data = tmp_realloc;

        tmp_realloc = (C_DATATYPE *)HDrealloc(read_buf, data_size);
        VRFY((NULL != tmp_realloc), "HDrealloc succeeded");
        read_buf = tmp_realloc;

        for (j = 0; j < data_size / sizeof(*data); j++)
            data[j] = (C_DATATYPE)GEN_DATA(j);

        /* Select hyperslab in the file */
        filespace = H5Dget_space(dset_id);
        VRFY((filespace >= 0), "File dataspace retrieval succeeded");

        /* Each process defines the dataset selection in memory and writes
         * it to the hyperslab in the file
         */
        count[0]  = (i + 1);
        count[1]  = (i + 1);
        stride[0] = (hsize_t)WRITE_UNSHARED_TWO_UNLIM_DIM_CH_NROWS;
        stride[1] = (hsize_t)WRITE_UNSHARED_TWO_UNLIM_DIM_CH_NCOLS;
        block[0]  = (hsize_t)WRITE_UNSHARED_TWO_UNLIM_DIM_CH_NROWS;
        block[1]  = (hsize_t)WRITE_UNSHARED_TWO_UNLIM_DIM_CH_NCOLS;
        start[0]  = ((hsize_t)mpi_rank * block[0] * count[0]);
        start[1]  = 0;

        if (VERBOSE_MED) {
            HDprintf("Process %d is writing with count[ %" PRIuHSIZE ", %" PRIuHSIZE " ], stride[ %" PRIuHSIZE
                     ", %" PRIuHSIZE " ], start[ %" PRIuHSIZE ", %" PRIuHSIZE " ], block size[ %" PRIuHSIZE
                     ", %" PRIuHSIZE " ]\n",
                     mpi_rank, count[0], count[1], stride[0], stride[1], start[0], start[1], block[0],
                     block[1]);
            HDfflush(stdout);
        }

        VRFY((H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, stride, count, block) >= 0),
             "Hyperslab selection succeeded");

        VRFY((H5Dwrite(dset_id, HDF5_DATATYPE_NAME, H5S_BLOCK, filespace, dxpl_id, data) >= 0),
             "Dataset write succeeded");

        /* Verify space allocation status */
        verify_space_alloc_status(dset_id, plist_id, ALL_CHUNKS_WRITTEN);

        VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");

        dset_id = H5Dopen2(group_id, WRITE_UNSHARED_TWO_UNLIM_DIM_DATASET_NAME, H5P_DEFAULT);
        VRFY((dset_id >= 0), "Dataset open succeeded");

        HDmemset(read_buf, 255, data_size);

        VRFY((H5Dread(dset_id, HDF5_DATATYPE_NAME, H5S_BLOCK, filespace, dxpl_id, read_buf) >= 0),
             "Dataset read succeeded");

        /* Verify the correct data was written */
        VRFY((0 == HDmemcmp(read_buf, data, data_size)), "Data verification succeeded");

        if (i < (size_t)WRITE_UNSHARED_TWO_UNLIM_DIM_NLOOPS - 1) {
            /*
             * Extend the dataset by the size of one chunk per rank
             * in the first extensible dimension. Extend the dataset
             * by the size of chunk in the second extensible dimension.
             */
            dataset_dims[0] += (hsize_t)mpi_size * block[0];
            dataset_dims[1] += block[1];
            VRFY(H5Dset_extent(dset_id, dataset_dims) >= 0, "H5Dset_extent succeeded");

            /* Verify space allocation status */
            verify_space_alloc_status(dset_id, plist_id, SOME_CHUNKS_WRITTEN);
        }

        VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    }

    if (data)
        HDfree(data);
    if (read_buf)
        HDfree(read_buf);

    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
    VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    return;
}

/*
 * Tests parallel write of filtered data in the case where
 * a dataset has two unlimited dimensions and each MPI
 * rank writes to a portion of each chunk in the dataset.
 * On each iteration, the dataset is extended in its extensible
 * dimensions by the size of a chunk and then all chunks are
 * written to by all ranks, then read back and verified.
 */
static void
test_write_filtered_dataset_multi_unlim_dim_overlap(const char *parent_group, H5Z_filter_t filter_id,
                                                    hid_t fapl_id, hid_t dcpl_id, hid_t dxpl_id)
{
    C_DATATYPE *data     = NULL;
    C_DATATYPE *read_buf = NULL;
    hsize_t     dataset_dims[WRITE_SHARED_TWO_UNLIM_DIM_DATASET_DIMS];
    hsize_t     max_dims[WRITE_SHARED_TWO_UNLIM_DIM_DATASET_DIMS];
    hsize_t     chunk_dims[WRITE_SHARED_TWO_UNLIM_DIM_DATASET_DIMS];
    hsize_t     sel_dims[WRITE_SHARED_TWO_UNLIM_DIM_DATASET_DIMS];
    hsize_t     start[WRITE_SHARED_TWO_UNLIM_DIM_DATASET_DIMS];
    hsize_t     stride[WRITE_SHARED_TWO_UNLIM_DIM_DATASET_DIMS];
    hsize_t     count[WRITE_SHARED_TWO_UNLIM_DIM_DATASET_DIMS];
    hsize_t     block[WRITE_SHARED_TWO_UNLIM_DIM_DATASET_DIMS];
    size_t      i, data_size;
    hid_t       file_id = H5I_INVALID_HID, dset_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID;
    hid_t       group_id  = H5I_INVALID_HID;
    hid_t       filespace = H5I_INVALID_HID;

    if (MAINPROCESS)
        HDputs("Testing write to shared filtered chunks w/ two unlimited dimensions");

    file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, fapl_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
    VRFY((group_id >= 0), "H5Gopen2 succeeded");

    /* Create the dataspace for the dataset */
    dataset_dims[0] = (hsize_t)WRITE_SHARED_TWO_UNLIM_DIM_NROWS;
    dataset_dims[1] = (hsize_t)WRITE_SHARED_TWO_UNLIM_DIM_NCOLS;
    max_dims[0]     = H5S_UNLIMITED;
    max_dims[1]     = H5S_UNLIMITED;
    chunk_dims[0]   = (hsize_t)WRITE_SHARED_TWO_UNLIM_DIM_CH_NROWS;
    chunk_dims[1]   = (hsize_t)WRITE_SHARED_TWO_UNLIM_DIM_CH_NCOLS;
    sel_dims[0]     = (hsize_t)DIM0_SCALE_FACTOR;
    sel_dims[1]     = (hsize_t)WRITE_SHARED_TWO_UNLIM_DIM_CH_NCOLS * (hsize_t)DIM1_SCALE_FACTOR;

    filespace = H5Screate_simple(WRITE_SHARED_TWO_UNLIM_DIM_DATASET_DIMS, dataset_dims, max_dims);
    VRFY((filespace >= 0), "File dataspace creation succeeded");

    /* Create chunked dataset */
    plist_id = H5Pcopy(dcpl_id);
    VRFY((plist_id >= 0), "DCPL copy succeeded");

    VRFY((H5Pset_chunk(plist_id, WRITE_SHARED_TWO_UNLIM_DIM_DATASET_DIMS, chunk_dims) >= 0),
         "Chunk size set");

    /* Add test filter to the pipeline */
    VRFY((set_dcpl_filter(plist_id, filter_id, NULL) >= 0), "Filter set");

    dset_id = H5Dcreate2(group_id, WRITE_SHARED_TWO_UNLIM_DIM_DATASET_NAME, HDF5_DATATYPE_NAME, filespace,
                         H5P_DEFAULT, plist_id, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset creation succeeded");

    /* Verify space allocation status */
    verify_space_alloc_status(dset_id, plist_id, DATASET_JUST_CREATED);

    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

    for (i = 0; i < (size_t)WRITE_SHARED_TWO_UNLIM_DIM_NLOOPS; i++) {
        C_DATATYPE *tmp_realloc = NULL;
        size_t      j;

        /* Set selected dimensions */
        sel_dims[0] = (i + 1);
        sel_dims[1] = (i + 1) * (size_t)WRITE_SHARED_TWO_UNLIM_DIM_CH_NCOLS;

        /* Fill data buffer */
        data_size = sel_dims[0] * sel_dims[1] * sizeof(*data);

        tmp_realloc = (C_DATATYPE *)HDrealloc(data, data_size);
        VRFY((NULL != tmp_realloc), "HDrealloc succeeded");
        data = tmp_realloc;

        tmp_realloc = (C_DATATYPE *)HDrealloc(read_buf, data_size);
        VRFY((NULL != tmp_realloc), "HDrealloc succeeded");
        read_buf = tmp_realloc;

        for (j = 0; j < data_size / sizeof(*data); j++)
            data[j] = (C_DATATYPE)GEN_DATA(j);

        /* Select hyperslab in the file */
        filespace = H5Dget_space(dset_id);
        VRFY((filespace >= 0), "File dataspace retrieval succeeded");

        /* Each process defines the dataset selection in memory and writes
         * it to the hyperslab in the file
         */
        count[0]  = (i + 1);
        count[1]  = (i + 1);
        stride[0] = (hsize_t)WRITE_SHARED_TWO_UNLIM_DIM_CH_NROWS;
        stride[1] = (hsize_t)WRITE_SHARED_TWO_UNLIM_DIM_CH_NCOLS;
        block[0]  = 1;
        block[1]  = (hsize_t)WRITE_SHARED_TWO_UNLIM_DIM_CH_NROWS;
        start[0]  = (hsize_t)mpi_rank;
        start[1]  = 0;

        if (VERBOSE_MED) {
            HDprintf("Process %d is writing with count[ %" PRIuHSIZE ", %" PRIuHSIZE " ], stride[ %" PRIuHSIZE
                     ", %" PRIuHSIZE " ], start[ %" PRIuHSIZE ", %" PRIuHSIZE " ], block size[ %" PRIuHSIZE
                     ", %" PRIuHSIZE " ]\n",
                     mpi_rank, count[0], count[1], stride[0], stride[1], start[0], start[1], block[0],
                     block[1]);
            HDfflush(stdout);
        }

        VRFY((H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, stride, count, block) >= 0),
             "Hyperslab selection succeeded");

        VRFY((H5Dwrite(dset_id, HDF5_DATATYPE_NAME, H5S_BLOCK, filespace, dxpl_id, data) >= 0),
             "Dataset write succeeded");

        /* Verify space allocation status */
        verify_space_alloc_status(dset_id, plist_id, ALL_CHUNKS_WRITTEN);

        VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");

        dset_id = H5Dopen2(group_id, WRITE_SHARED_TWO_UNLIM_DIM_DATASET_NAME, H5P_DEFAULT);
        VRFY((dset_id >= 0), "Dataset open succeeded");

        HDmemset(read_buf, 255, data_size);

        VRFY((H5Dread(dset_id, HDF5_DATATYPE_NAME, H5S_BLOCK, filespace, dxpl_id, read_buf) >= 0),
             "Dataset read succeeded");

        /* Verify correct data was written */
        VRFY((0 == HDmemcmp(read_buf, data, data_size)), "Data verification succeeded");

        if (i < (size_t)WRITE_SHARED_TWO_UNLIM_DIM_NLOOPS - 1) {
            /* Extend the dataset by the size of a chunk in each extensible dimension */
            dataset_dims[0] += (hsize_t)WRITE_SHARED_TWO_UNLIM_DIM_CH_NROWS;
            dataset_dims[1] += (hsize_t)WRITE_SHARED_TWO_UNLIM_DIM_CH_NCOLS;
            VRFY(H5Dset_extent(dset_id, dataset_dims) >= 0, "H5Dset_extent succeeded");

            /* Verify space allocation status */
            verify_space_alloc_status(dset_id, plist_id, SOME_CHUNKS_WRITTEN);
        }

        VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    }

    if (data)
        HDfree(data);
    if (read_buf)
        HDfree(read_buf);

    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
    VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
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
test_write_filtered_dataset_single_no_selection(const char *parent_group, H5Z_filter_t filter_id,
                                                hid_t fapl_id, hid_t dcpl_id, hid_t dxpl_id)
{
    C_DATATYPE *data        = NULL;
    C_DATATYPE *read_buf    = NULL;
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
    hid_t       file_id = H5I_INVALID_HID, dset_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID;
    hid_t       group_id  = H5I_INVALID_HID;
    hid_t       filespace = H5I_INVALID_HID, memspace = H5I_INVALID_HID;

    if (MAINPROCESS)
        HDputs("Testing write to filtered chunks with a single process having no selection");

    file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, fapl_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
    VRFY((group_id >= 0), "H5Gopen2 succeeded");

    /* Create the dataspace for the dataset */
    dataset_dims[0] = (hsize_t)WRITE_SINGLE_NO_SELECTION_FILTERED_CHUNKS_NROWS;
    dataset_dims[1] = (hsize_t)WRITE_SINGLE_NO_SELECTION_FILTERED_CHUNKS_NCOLS;
    chunk_dims[0]   = (hsize_t)WRITE_SINGLE_NO_SELECTION_FILTERED_CHUNKS_CH_NROWS;
    chunk_dims[1]   = (hsize_t)WRITE_SINGLE_NO_SELECTION_FILTERED_CHUNKS_CH_NCOLS;
    sel_dims[0]     = (hsize_t)WRITE_SINGLE_NO_SELECTION_FILTERED_CHUNKS_CH_NROWS;
    sel_dims[1]     = (hsize_t)WRITE_SINGLE_NO_SELECTION_FILTERED_CHUNKS_NCOLS;

    if (mpi_rank == WRITE_SINGLE_NO_SELECTION_FILTERED_CHUNKS_NO_SELECT_PROC)
        sel_dims[0] = sel_dims[1] = 0;

    filespace = H5Screate_simple(WRITE_SINGLE_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS, dataset_dims, NULL);
    VRFY((filespace >= 0), "File dataspace creation succeeded");

    memspace = H5Screate_simple(WRITE_SINGLE_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS, sel_dims, NULL);
    VRFY((memspace >= 0), "Memory dataspace creation succeeded");

    /* Create chunked dataset */
    plist_id = H5Pcopy(dcpl_id);
    VRFY((plist_id >= 0), "DCPL copy succeeded");

    VRFY((H5Pset_chunk(plist_id, WRITE_SINGLE_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS, chunk_dims) >= 0),
         "Chunk size set");

    /* Add test filter to the pipeline */
    VRFY((set_dcpl_filter(plist_id, filter_id, NULL) >= 0), "Filter set");

    dset_id = H5Dcreate2(group_id, WRITE_SINGLE_NO_SELECTION_FILTERED_CHUNKS_DATASET_NAME, HDF5_DATATYPE_NAME,
                         filespace, H5P_DEFAULT, plist_id, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset creation succeeded");

    /* Verify space allocation status */
    verify_space_alloc_status(dset_id, plist_id, DATASET_JUST_CREATED);

    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

    /* Each process defines the dataset selection in memory and writes
     * it to the hyperslab in the file
     */
    count[0] = 1;
    count[1] = (hsize_t)WRITE_SINGLE_NO_SELECTION_FILTERED_CHUNKS_NCOLS /
               (hsize_t)WRITE_SINGLE_NO_SELECTION_FILTERED_CHUNKS_CH_NCOLS;
    stride[0] = (hsize_t)WRITE_SINGLE_NO_SELECTION_FILTERED_CHUNKS_CH_NROWS;
    stride[1] = (hsize_t)WRITE_SINGLE_NO_SELECTION_FILTERED_CHUNKS_CH_NCOLS;
    block[0]  = (hsize_t)WRITE_SINGLE_NO_SELECTION_FILTERED_CHUNKS_CH_NROWS;
    block[1]  = (hsize_t)WRITE_SINGLE_NO_SELECTION_FILTERED_CHUNKS_CH_NCOLS;
    start[0]  = (hsize_t)mpi_rank * (hsize_t)WRITE_SINGLE_NO_SELECTION_FILTERED_CHUNKS_CH_NROWS * count[0];
    start[1]  = 0;

    if (VERBOSE_MED) {
        HDprintf("Process %d is writing with count[ %" PRIuHSIZE ", %" PRIuHSIZE " ], stride[ %" PRIuHSIZE
                 ", %" PRIuHSIZE " ], start[ %" PRIuHSIZE ", %" PRIuHSIZE " ], block size[ %" PRIuHSIZE
                 ", %" PRIuHSIZE " ]\n",
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
    data_size        = sel_dims[0] * sel_dims[1] * sizeof(*data);
    correct_buf_size = dataset_dims[0] * dataset_dims[1] * sizeof(*correct_buf);

    if (mpi_rank != WRITE_SINGLE_NO_SELECTION_FILTERED_CHUNKS_NO_SELECT_PROC) {
        data = (C_DATATYPE *)HDcalloc(1, data_size);
        VRFY((NULL != data), "HDcalloc succeeded");

        for (i = 0; i < data_size / sizeof(*data); i++)
            data[i] = (C_DATATYPE)GEN_DATA(i);
    }

    correct_buf = (C_DATATYPE *)HDcalloc(1, correct_buf_size);
    VRFY((NULL != correct_buf), "HDcalloc succeeded");

    for (i = 0; i < correct_buf_size / sizeof(*correct_buf); i++)
        correct_buf[i] = (C_DATATYPE)((i % (dataset_dims[0] / (hsize_t)mpi_size * dataset_dims[1])) +
                                      (i / (dataset_dims[0] / (hsize_t)mpi_size * dataset_dims[1])));

    /* Compute the correct offset into the buffer for the process having no selection and clear it */
    segment_length = dataset_dims[0] * dataset_dims[1] / (hsize_t)mpi_size;
    HDmemset(correct_buf +
                 ((size_t)WRITE_SINGLE_NO_SELECTION_FILTERED_CHUNKS_NO_SELECT_PROC * segment_length),
             0, segment_length * sizeof(*data));

    VRFY((H5Dwrite(dset_id, HDF5_DATATYPE_NAME, memspace, filespace, dxpl_id, data) >= 0),
         "Dataset write succeeded");

    /* Verify space allocation status - data should only have been written if MPI size > 1 */
    verify_space_alloc_status(dset_id, plist_id, (mpi_size > 1 ? SOME_CHUNKS_WRITTEN : NO_CHUNKS_WRITTEN));

    if (data)
        HDfree(data);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");

    /* Verify the correct data was written */
    read_buf = (C_DATATYPE *)HDcalloc(1, correct_buf_size);
    VRFY((NULL != read_buf), "HDcalloc succeeded");

    dset_id = H5Dopen2(group_id, WRITE_SINGLE_NO_SELECTION_FILTERED_CHUNKS_DATASET_NAME, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    VRFY((H5Dread(dset_id, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, dxpl_id, read_buf) >= 0),
         "Dataset read succeeded");

    VRFY((0 == HDmemcmp(read_buf, correct_buf, correct_buf_size)), "Data verification succeeded");

    if (correct_buf)
        HDfree(correct_buf);
    if (read_buf)
        HDfree(read_buf);

    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Sclose(memspace) >= 0), "Memory dataspace close succeeded");
    VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
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
test_write_filtered_dataset_all_no_selection(const char *parent_group, H5Z_filter_t filter_id, hid_t fapl_id,
                                             hid_t dcpl_id, hid_t dxpl_id)
{
    C_DATATYPE *data        = NULL;
    C_DATATYPE *read_buf    = NULL;
    C_DATATYPE *correct_buf = NULL;
    hsize_t     dataset_dims[WRITE_ALL_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     chunk_dims[WRITE_ALL_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     sel_dims[WRITE_ALL_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    size_t      i, data_size, correct_buf_size;
    hid_t       file_id = H5I_INVALID_HID, dset_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID;
    hid_t       group_id  = H5I_INVALID_HID;
    hid_t       filespace = H5I_INVALID_HID, memspace = H5I_INVALID_HID;

    if (MAINPROCESS)
        HDputs("Testing write to filtered chunks with all processes having no selection");

    file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, fapl_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
    VRFY((group_id >= 0), "H5Gopen2 succeeded");

    /* Create the dataspace for the dataset */
    dataset_dims[0] = (hsize_t)WRITE_ALL_NO_SELECTION_FILTERED_CHUNKS_NROWS;
    dataset_dims[1] = (hsize_t)WRITE_ALL_NO_SELECTION_FILTERED_CHUNKS_NCOLS;
    chunk_dims[0]   = (hsize_t)WRITE_ALL_NO_SELECTION_FILTERED_CHUNKS_CH_NROWS;
    chunk_dims[1]   = (hsize_t)WRITE_ALL_NO_SELECTION_FILTERED_CHUNKS_CH_NCOLS;
    sel_dims[0] = sel_dims[1] = 0;

    filespace = H5Screate_simple(WRITE_ALL_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS, dataset_dims, NULL);
    VRFY((filespace >= 0), "File dataspace creation succeeded");

    memspace = H5Screate_simple(WRITE_ALL_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS, sel_dims, NULL);
    VRFY((memspace >= 0), "Memory dataspace creation succeeded");

    /* Create chunked dataset */
    plist_id = H5Pcopy(dcpl_id);
    VRFY((plist_id >= 0), "DCPL copy succeeded");

    VRFY((H5Pset_chunk(plist_id, WRITE_ALL_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS, chunk_dims) >= 0),
         "Chunk size set");

    /* Add test filter to the pipeline */
    VRFY((set_dcpl_filter(plist_id, filter_id, NULL) >= 0), "Filter set");

    dset_id = H5Dcreate2(group_id, WRITE_ALL_NO_SELECTION_FILTERED_CHUNKS_DATASET_NAME, HDF5_DATATYPE_NAME,
                         filespace, H5P_DEFAULT, plist_id, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset creation succeeded");

    /* Verify space allocation status */
    verify_space_alloc_status(dset_id, plist_id, DATASET_JUST_CREATED);

    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

    filespace = H5Dget_space(dset_id);
    VRFY((filespace >= 0), "File dataspace retrieval succeeded");

    VRFY((H5Sselect_none(filespace) >= 0), "Select none succeeded");

    /* Fill data buffer */
    data_size        = sel_dims[0] * sel_dims[1] * sizeof(*data);
    correct_buf_size = dataset_dims[0] * dataset_dims[1] * sizeof(*correct_buf);

    data = (C_DATATYPE *)HDcalloc(1, data_size);
    VRFY((NULL != data), "HDcalloc succeeded");

    correct_buf = (C_DATATYPE *)HDcalloc(1, correct_buf_size);
    VRFY((NULL != correct_buf), "HDcalloc succeeded");

    for (i = 0; i < data_size / sizeof(*data); i++)
        data[i] = (C_DATATYPE)GEN_DATA(i);

    VRFY((H5Dwrite(dset_id, HDF5_DATATYPE_NAME, memspace, filespace, dxpl_id, data) >= 0),
         "Dataset write succeeded");

    /* Verify space allocation status - no ranks should have written any data */
    verify_space_alloc_status(dset_id, plist_id, NO_CHUNKS_WRITTEN);

    if (data)
        HDfree(data);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");

    /* Verify the correct data was written */
    read_buf = (C_DATATYPE *)HDcalloc(1, correct_buf_size);
    VRFY((NULL != read_buf), "HDcalloc succeeded");

    dset_id = H5Dopen2(group_id, WRITE_ALL_NO_SELECTION_FILTERED_CHUNKS_DATASET_NAME, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    VRFY((H5Dread(dset_id, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, dxpl_id, read_buf) >= 0),
         "Dataset read succeeded");

    VRFY((0 == HDmemcmp(read_buf, correct_buf, correct_buf_size)), "Data verification succeeded");

    if (correct_buf)
        HDfree(correct_buf);
    if (read_buf)
        HDfree(read_buf);

    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Sclose(memspace) >= 0), "Memory dataspace close succeeded");
    VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
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
test_write_filtered_dataset_point_selection(const char *parent_group, H5Z_filter_t filter_id, hid_t fapl_id,
                                            hid_t dcpl_id, hid_t dxpl_id)
{
    C_DATATYPE *data        = NULL;
    C_DATATYPE *correct_buf = NULL;
    C_DATATYPE *read_buf    = NULL;
    hsize_t    *coords      = NULL;
    hsize_t     dataset_dims[WRITE_POINT_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     chunk_dims[WRITE_POINT_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     sel_dims[WRITE_POINT_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    size_t      i, j, data_size, correct_buf_size;
    size_t      num_points;
    hid_t       file_id = H5I_INVALID_HID, dset_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID;
    hid_t       group_id  = H5I_INVALID_HID;
    hid_t       filespace = H5I_INVALID_HID, memspace = H5I_INVALID_HID;

    if (MAINPROCESS)
        HDputs("Testing write to filtered chunks with point selection");

    file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, fapl_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
    VRFY((group_id >= 0), "H5Gopen2 succeeded");

    /* Create the dataspace for the dataset */
    dataset_dims[0] = (hsize_t)WRITE_POINT_SELECTION_FILTERED_CHUNKS_NROWS;
    dataset_dims[1] = (hsize_t)WRITE_POINT_SELECTION_FILTERED_CHUNKS_NCOLS;
    chunk_dims[0]   = (hsize_t)WRITE_POINT_SELECTION_FILTERED_CHUNKS_CH_NROWS;
    chunk_dims[1]   = (hsize_t)WRITE_POINT_SELECTION_FILTERED_CHUNKS_CH_NCOLS;
    sel_dims[0]     = (hsize_t)WRITE_POINT_SELECTION_FILTERED_CHUNKS_NROWS / (hsize_t)mpi_size;
    sel_dims[1]     = (hsize_t)WRITE_POINT_SELECTION_FILTERED_CHUNKS_NCOLS;

    filespace = H5Screate_simple(WRITE_POINT_SELECTION_FILTERED_CHUNKS_DATASET_DIMS, dataset_dims, NULL);
    VRFY((filespace >= 0), "File dataspace creation succeeded");

    memspace = H5Screate_simple(WRITE_POINT_SELECTION_FILTERED_CHUNKS_DATASET_DIMS, sel_dims, NULL);
    VRFY((memspace >= 0), "Memory dataspace creation succeeded");

    /* Create chunked dataset */
    plist_id = H5Pcopy(dcpl_id);
    VRFY((plist_id >= 0), "DCPL copy succeeded");

    VRFY((H5Pset_chunk(plist_id, WRITE_POINT_SELECTION_FILTERED_CHUNKS_DATASET_DIMS, chunk_dims) >= 0),
         "Chunk size set");

    /* Add test filter to the pipeline */
    VRFY((set_dcpl_filter(plist_id, filter_id, NULL) >= 0), "Filter set");

    dset_id = H5Dcreate2(group_id, WRITE_POINT_SELECTION_FILTERED_CHUNKS_DATASET_NAME, HDF5_DATATYPE_NAME,
                         filespace, H5P_DEFAULT, plist_id, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset creation succeeded");

    /* Verify space allocation status */
    verify_space_alloc_status(dset_id, plist_id, DATASET_JUST_CREATED);

    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

    /* Set up point selection */
    filespace = H5Dget_space(dset_id);
    VRFY((filespace >= 0), "File dataspace retrieval succeeded");

    num_points = (hsize_t)WRITE_POINT_SELECTION_FILTERED_CHUNKS_NROWS *
                 (hsize_t)WRITE_POINT_SELECTION_FILTERED_CHUNKS_NCOLS / (hsize_t)mpi_size;
    coords = (hsize_t *)HDcalloc(1, 2 * num_points * sizeof(*coords));
    VRFY((NULL != coords), "Coords HDcalloc succeeded");

    for (i = 0; i < num_points; i++)
        for (j = 0; j < WRITE_POINT_SELECTION_FILTERED_CHUNKS_DATASET_DIMS; j++)
            coords[(i * WRITE_POINT_SELECTION_FILTERED_CHUNKS_DATASET_DIMS) + j] =
                (j > 0) ? (i % (hsize_t)WRITE_POINT_SELECTION_FILTERED_CHUNKS_NCOLS)
                        : ((hsize_t)mpi_rank +
                           ((hsize_t)mpi_size * (i / (hsize_t)WRITE_POINT_SELECTION_FILTERED_CHUNKS_NCOLS)));

    VRFY((H5Sselect_elements(filespace, H5S_SELECT_SET, (hsize_t)num_points, (const hsize_t *)coords) >= 0),
         "Point selection succeeded");

    /* Fill data buffer */
    data_size        = sel_dims[0] * sel_dims[1] * sizeof(*data);
    correct_buf_size = dataset_dims[0] * dataset_dims[1] * sizeof(*correct_buf);

    data = (C_DATATYPE *)HDcalloc(1, data_size);
    VRFY((NULL != data), "HDcalloc succeeded");

    correct_buf = (C_DATATYPE *)HDcalloc(1, correct_buf_size);
    VRFY((NULL != correct_buf), "HDcalloc succeeded");

    for (i = 0; i < data_size / sizeof(*data); i++)
        data[i] = (C_DATATYPE)GEN_DATA(i);

    for (i = 0; i < correct_buf_size / sizeof(*correct_buf); i++)
        correct_buf[i] =
            (C_DATATYPE)((dataset_dims[1] * (i / ((hsize_t)mpi_size * dataset_dims[1]))) +
                         (i % dataset_dims[1]) +
                         (((i % ((hsize_t)mpi_size * dataset_dims[1])) / dataset_dims[1]) % dataset_dims[1]));

    VRFY((H5Dwrite(dset_id, HDF5_DATATYPE_NAME, memspace, filespace, dxpl_id, data) >= 0),
         "Dataset write succeeded");

    /* Verify space allocation status */
    verify_space_alloc_status(dset_id, plist_id, ALL_CHUNKS_WRITTEN);

    if (data)
        HDfree(data);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");

    /* Verify the correct data was written */
    read_buf = (C_DATATYPE *)HDcalloc(1, correct_buf_size);
    VRFY((NULL != read_buf), "HDcalloc succeeded");

    dset_id = H5Dopen2(group_id, WRITE_POINT_SELECTION_FILTERED_CHUNKS_DATASET_NAME, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    VRFY((H5Dread(dset_id, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, dxpl_id, read_buf) >= 0),
         "Dataset read succeeded");

    VRFY((0 == HDmemcmp(read_buf, correct_buf, correct_buf_size)), "Data verification succeeded");

    if (coords)
        HDfree(coords);
    if (correct_buf)
        HDfree(correct_buf);
    if (read_buf)
        HDfree(read_buf);

    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Sclose(memspace) >= 0), "Memory dataspace close succeeded");
    VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
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
test_write_filtered_dataset_interleaved_write(const char *parent_group, H5Z_filter_t filter_id, hid_t fapl_id,
                                              hid_t dcpl_id, hid_t dxpl_id)
{
    C_DATATYPE *data        = NULL;
    C_DATATYPE *read_buf    = NULL;
    C_DATATYPE *correct_buf = NULL;
    hsize_t     dataset_dims[INTERLEAVED_WRITE_FILTERED_DATASET_DIMS];
    hsize_t     chunk_dims[INTERLEAVED_WRITE_FILTERED_DATASET_DIMS];
    hsize_t     sel_dims[INTERLEAVED_WRITE_FILTERED_DATASET_DIMS];
    hsize_t     start[INTERLEAVED_WRITE_FILTERED_DATASET_DIMS];
    hsize_t     stride[INTERLEAVED_WRITE_FILTERED_DATASET_DIMS];
    hsize_t     count[INTERLEAVED_WRITE_FILTERED_DATASET_DIMS];
    hsize_t     block[INTERLEAVED_WRITE_FILTERED_DATASET_DIMS];
    size_t      i, data_size, correct_buf_size;
    hid_t       file_id = H5I_INVALID_HID, dset_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID;
    hid_t       group_id  = H5I_INVALID_HID;
    hid_t       filespace = H5I_INVALID_HID, memspace = H5I_INVALID_HID;

    if (MAINPROCESS)
        HDputs("Testing interleaved write to filtered chunks");

    file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, fapl_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
    VRFY((group_id >= 0), "H5Gopen2 succeeded");

    /* Create the dataspace for the dataset */
    dataset_dims[0] = (hsize_t)INTERLEAVED_WRITE_FILTERED_DATASET_NROWS;
    dataset_dims[1] = (hsize_t)INTERLEAVED_WRITE_FILTERED_DATASET_NCOLS;
    chunk_dims[0]   = (hsize_t)INTERLEAVED_WRITE_FILTERED_DATASET_CH_NROWS;
    chunk_dims[1]   = (hsize_t)INTERLEAVED_WRITE_FILTERED_DATASET_CH_NCOLS;
    sel_dims[0]     = (hsize_t)(INTERLEAVED_WRITE_FILTERED_DATASET_NROWS / mpi_size);
    sel_dims[1]     = (hsize_t)INTERLEAVED_WRITE_FILTERED_DATASET_NCOLS;

    filespace = H5Screate_simple(INTERLEAVED_WRITE_FILTERED_DATASET_DIMS, dataset_dims, NULL);
    VRFY((filespace >= 0), "File dataspace creation succeeded");

    memspace = H5Screate_simple(INTERLEAVED_WRITE_FILTERED_DATASET_DIMS, sel_dims, NULL);
    VRFY((memspace >= 0), "Memory dataspace creation succeeded");

    /* Create chunked dataset */
    plist_id = H5Pcopy(dcpl_id);
    VRFY((plist_id >= 0), "DCPL copy succeeded");

    VRFY((H5Pset_chunk(plist_id, INTERLEAVED_WRITE_FILTERED_DATASET_DIMS, chunk_dims) >= 0),
         "Chunk size set");

    /* Add test filter to the pipeline */
    VRFY((set_dcpl_filter(plist_id, filter_id, NULL) >= 0), "Filter set");

    dset_id = H5Dcreate2(group_id, INTERLEAVED_WRITE_FILTERED_DATASET_NAME, HDF5_DATATYPE_NAME, filespace,
                         H5P_DEFAULT, plist_id, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset creation succeeded");

    /* Verify space allocation status */
    verify_space_alloc_status(dset_id, plist_id, DATASET_JUST_CREATED);

    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

    /* Each process defines the dataset selection in memory and writes
     * it to the hyperslab in the file
     */
    count[0] =
        (hsize_t)(INTERLEAVED_WRITE_FILTERED_DATASET_NROWS / INTERLEAVED_WRITE_FILTERED_DATASET_CH_NROWS);
    count[1] =
        (hsize_t)(INTERLEAVED_WRITE_FILTERED_DATASET_NCOLS / INTERLEAVED_WRITE_FILTERED_DATASET_CH_NCOLS);
    stride[0] = (hsize_t)INTERLEAVED_WRITE_FILTERED_DATASET_CH_NROWS;
    stride[1] = (hsize_t)INTERLEAVED_WRITE_FILTERED_DATASET_CH_NCOLS;
    block[0]  = 1;
    block[1]  = (hsize_t)INTERLEAVED_WRITE_FILTERED_DATASET_CH_NCOLS;
    start[0]  = (hsize_t)mpi_rank;
    start[1]  = 0;

    if (VERBOSE_MED) {
        HDprintf("Process %d is writing with count[ %" PRIuHSIZE ", %" PRIuHSIZE " ], stride[ %" PRIuHSIZE
                 ", %" PRIuHSIZE " ], start[ %" PRIuHSIZE ", %" PRIuHSIZE " ], block size[ %" PRIuHSIZE
                 ", %" PRIuHSIZE " ]\n",
                 mpi_rank, count[0], count[1], stride[0], stride[1], start[0], start[1], block[0], block[1]);
        HDfflush(stdout);
    }

    /* Select hyperslab in the file */
    filespace = H5Dget_space(dset_id);
    VRFY((filespace >= 0), "File dataspace retrieval succeeded");

    VRFY((H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, stride, count, block) >= 0),
         "Hyperslab selection succeeded");

    /* Fill data buffer */
    data_size        = sel_dims[0] * sel_dims[1] * sizeof(*data);
    correct_buf_size = dataset_dims[0] * dataset_dims[1] * sizeof(*correct_buf);

    data = (C_DATATYPE *)HDcalloc(1, data_size);
    VRFY((NULL != data), "HDcalloc succeeded");

    correct_buf = (C_DATATYPE *)HDcalloc(1, correct_buf_size);
    VRFY((NULL != correct_buf), "HDcalloc succeeded");

    for (i = 0; i < data_size / sizeof(*data); i++)
        data[i] = (C_DATATYPE)GEN_DATA(i);

    for (i = 0; i < correct_buf_size / sizeof(*correct_buf); i++)
        /* Add Column Index */
        correct_buf[i] =
            (C_DATATYPE)((i % (hsize_t)INTERLEAVED_WRITE_FILTERED_DATASET_NCOLS)

                         /* Add the Row Index */
                         + ((i % (hsize_t)(mpi_size * INTERLEAVED_WRITE_FILTERED_DATASET_NCOLS)) /
                            (hsize_t)INTERLEAVED_WRITE_FILTERED_DATASET_NCOLS)

                         /* Add the amount that gets added when a rank moves down to its next section
                            vertically in the dataset */
                         + ((hsize_t)INTERLEAVED_WRITE_FILTERED_DATASET_NCOLS *
                            (i / (hsize_t)(mpi_size * INTERLEAVED_WRITE_FILTERED_DATASET_NCOLS))));

    VRFY((H5Dwrite(dset_id, HDF5_DATATYPE_NAME, memspace, filespace, dxpl_id, data) >= 0),
         "Dataset write succeeded");

    /* Verify space allocation status */
    verify_space_alloc_status(dset_id, plist_id, ALL_CHUNKS_WRITTEN);

    if (data)
        HDfree(data);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");

    /* Verify the correct data was written */
    read_buf = (C_DATATYPE *)HDcalloc(1, correct_buf_size);
    VRFY((NULL != read_buf), "HDcalloc succeeded");

    dset_id = H5Dopen2(group_id, INTERLEAVED_WRITE_FILTERED_DATASET_NAME, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    VRFY((H5Dread(dset_id, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, dxpl_id, read_buf) >= 0),
         "Dataset read succeeded");

    VRFY((0 == HDmemcmp(read_buf, correct_buf, correct_buf_size)), "Data verification succeeded");

    if (correct_buf)
        HDfree(correct_buf);
    if (read_buf)
        HDfree(read_buf);

    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Sclose(memspace) >= 0), "Memory dataspace close succeeded");
    VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    return;
}

/*
 * Tests parallel write of transformed and filtered data
 * in the case where only one process is writing to a
 * particular chunk in the operation. Normally, a data
 * transform function will cause the parallel library to
 * break to independent I/O and this isn't allowed when
 * there are filters in the pipeline. However, in this
 * case the parallel library recognizes that the used
 * data transform function "x" is the same as not applying
 * the transform function. Therefore it does not apply
 * the transform function resulting in not breaking to
 * independent I/O.
 *
 * Programmer: Jan-Willem Blokland
 *             08/20/2021
 */
static void
test_write_transformed_filtered_dataset_no_overlap(const char *parent_group, H5Z_filter_t filter_id,
                                                   hid_t fapl_id, hid_t dcpl_id, hid_t dxpl_id)
{
    C_DATATYPE *data        = NULL;
    C_DATATYPE *read_buf    = NULL;
    C_DATATYPE *correct_buf = NULL;
    hsize_t     dataset_dims[WRITE_UNSHARED_TRANSFORMED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     chunk_dims[WRITE_UNSHARED_TRANSFORMED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     sel_dims[WRITE_UNSHARED_TRANSFORMED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     start[WRITE_UNSHARED_TRANSFORMED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     stride[WRITE_UNSHARED_TRANSFORMED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     count[WRITE_UNSHARED_TRANSFORMED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     block[WRITE_UNSHARED_TRANSFORMED_FILTERED_CHUNKS_DATASET_DIMS];
    size_t      i, data_size, correct_buf_size;
    hid_t       file_id = H5I_INVALID_HID, dset_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID;
    hid_t       group_id  = H5I_INVALID_HID;
    hid_t       filespace = H5I_INVALID_HID, memspace = H5I_INVALID_HID;

    if (MAINPROCESS)
        HDputs("Testing write to unshared transformed and filtered chunks");

    file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, fapl_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
    VRFY((group_id >= 0), "H5Gopen2 succeeded");

    /* Create the dataspace for the dataset */
    dataset_dims[0] = (hsize_t)WRITE_UNSHARED_TRANSFORMED_FILTERED_CHUNKS_NROWS;
    dataset_dims[1] = (hsize_t)WRITE_UNSHARED_TRANSFORMED_FILTERED_CHUNKS_NCOLS;
    chunk_dims[0]   = (hsize_t)WRITE_UNSHARED_TRANSFORMED_FILTERED_CHUNKS_CH_NROWS;
    chunk_dims[1]   = (hsize_t)WRITE_UNSHARED_TRANSFORMED_FILTERED_CHUNKS_CH_NCOLS;
    sel_dims[0]     = (hsize_t)WRITE_UNSHARED_TRANSFORMED_FILTERED_CHUNKS_CH_NROWS;
    sel_dims[1]     = (hsize_t)WRITE_UNSHARED_TRANSFORMED_FILTERED_CHUNKS_NCOLS;

    filespace = H5Screate_simple(WRITE_UNSHARED_TRANSFORMED_FILTERED_CHUNKS_DATASET_DIMS, dataset_dims, NULL);
    VRFY((filespace >= 0), "File dataspace creation succeeded");

    memspace = H5Screate_simple(WRITE_UNSHARED_TRANSFORMED_FILTERED_CHUNKS_DATASET_DIMS, sel_dims, NULL);
    VRFY((memspace >= 0), "Memory dataspace creation succeeded");

    /* Create chunked dataset */
    plist_id = H5Pcopy(dcpl_id);
    VRFY((plist_id >= 0), "DCPL copy succeeded");

    VRFY((H5Pset_chunk(plist_id, WRITE_UNSHARED_TRANSFORMED_FILTERED_CHUNKS_DATASET_DIMS, chunk_dims) >= 0),
         "Chunk size set");

    /* Add test filter to the pipeline */
    VRFY((set_dcpl_filter(plist_id, filter_id, NULL) >= 0), "Filter set");

    dset_id = H5Dcreate2(group_id, WRITE_UNSHARED_TRANSFORMED_FILTERED_CHUNKS_DATASET_NAME,
                         HDF5_DATATYPE_NAME, filespace, H5P_DEFAULT, plist_id, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset creation succeeded");

    /* Verify space allocation status */
    verify_space_alloc_status(dset_id, plist_id, DATASET_JUST_CREATED);

    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

    /* Each process defines the dataset selection in memory and writes
     * it to the hyperslab in the file
     */
    count[0] = 1;
    count[1] = (hsize_t)WRITE_UNSHARED_TRANSFORMED_FILTERED_CHUNKS_NCOLS /
               (hsize_t)WRITE_UNSHARED_TRANSFORMED_FILTERED_CHUNKS_CH_NCOLS;
    stride[0] = (hsize_t)WRITE_UNSHARED_TRANSFORMED_FILTERED_CHUNKS_CH_NROWS;
    stride[1] = (hsize_t)WRITE_UNSHARED_TRANSFORMED_FILTERED_CHUNKS_CH_NCOLS;
    block[0]  = (hsize_t)WRITE_UNSHARED_TRANSFORMED_FILTERED_CHUNKS_CH_NROWS;
    block[1]  = (hsize_t)WRITE_UNSHARED_TRANSFORMED_FILTERED_CHUNKS_CH_NCOLS;
    start[0]  = ((hsize_t)mpi_rank * (hsize_t)WRITE_UNSHARED_TRANSFORMED_FILTERED_CHUNKS_CH_NROWS * count[0]);
    start[1]  = 0;

    if (VERBOSE_MED) {
        HDprintf("Process %d is writing with count[ %" PRIuHSIZE ", %" PRIuHSIZE " ], stride[ %" PRIuHSIZE
                 ", %" PRIuHSIZE " ], start[ %" PRIuHSIZE ", %" PRIuHSIZE " ], block size[ %" PRIuHSIZE
                 ", %" PRIuHSIZE " ]\n",
                 mpi_rank, count[0], count[1], stride[0], stride[1], start[0], start[1], block[0], block[1]);
        HDfflush(stdout);
    }

    /* Select hyperslab in the file */
    filespace = H5Dget_space(dset_id);
    VRFY((filespace >= 0), "File dataspace retrieval succeeded");

    VRFY((H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, stride, count, block) >= 0),
         "Hyperslab selection succeeded");

    /* Fill data buffer */
    data_size        = sel_dims[0] * sel_dims[1] * sizeof(*data);
    correct_buf_size = dataset_dims[0] * dataset_dims[1] * sizeof(*correct_buf);

    data = (C_DATATYPE *)HDcalloc(1, data_size);
    VRFY((NULL != data), "HDcalloc succeeded");

    correct_buf = (C_DATATYPE *)HDcalloc(1, correct_buf_size);
    VRFY((NULL != correct_buf), "HDcalloc succeeded");

    for (i = 0; i < data_size / sizeof(*data); i++)
        data[i] = (C_DATATYPE)GEN_DATA(i);

    for (i = 0; i < correct_buf_size / sizeof(*correct_buf); i++)
        correct_buf[i] = (C_DATATYPE)((i % (dataset_dims[0] / (hsize_t)mpi_size * dataset_dims[1])) +
                                      (i / (dataset_dims[0] / (hsize_t)mpi_size * dataset_dims[1])));

    /* Create property list for data transform */
    plist_id = H5Pcopy(dxpl_id);
    VRFY((plist_id >= 0), "DXPL copy succeeded");

    /* Set data transform expression */
    VRFY((H5Pset_data_transform(plist_id, "x") >= 0), "Set data transform expression succeeded");

    VRFY((H5Dwrite(dset_id, HDF5_DATATYPE_NAME, memspace, filespace, plist_id, data) >= 0),
         "Dataset write succeeded");

    if (data)
        HDfree(data);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");

    /* Verify the correct data was written */
    read_buf = (C_DATATYPE *)HDcalloc(1, correct_buf_size);
    VRFY((NULL != read_buf), "HDcalloc succeeded");

    dset_id = H5Dopen2(group_id, WRITE_UNSHARED_TRANSFORMED_FILTERED_CHUNKS_DATASET_NAME, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    VRFY((H5Dread(dset_id, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, plist_id, read_buf) >= 0),
         "Dataset read succeeded");

    VRFY((0 == HDmemcmp(read_buf, correct_buf, correct_buf_size)), "Data verification succeeded");

    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");

    /* Verify space allocation status */
    plist_id = H5Dget_create_plist(dset_id);
    VRFY((plist_id >= 0), "H5Dget_create_plist succeeded");
    verify_space_alloc_status(dset_id, plist_id, ALL_CHUNKS_WRITTEN);

    if (correct_buf)
        HDfree(correct_buf);
    if (read_buf)
        HDfree(read_buf);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Sclose(memspace) >= 0), "Memory dataspace close succeeded");
    VRFY((H5Pclose(plist_id) >= 0), "DXPL close succeeded");
    VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
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
test_write_3d_filtered_dataset_no_overlap_separate_pages(const char *parent_group, H5Z_filter_t filter_id,
                                                         hid_t fapl_id, hid_t dcpl_id, hid_t dxpl_id)
{
    C_DATATYPE *data        = NULL;
    C_DATATYPE *read_buf    = NULL;
    C_DATATYPE *correct_buf = NULL;
    hsize_t     dataset_dims[WRITE_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_DIMS];
    hsize_t     chunk_dims[WRITE_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_DIMS];
    hsize_t     sel_dims[WRITE_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_DIMS];
    hsize_t     start[WRITE_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_DIMS];
    hsize_t     stride[WRITE_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_DIMS];
    hsize_t     count[WRITE_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_DIMS];
    hsize_t     block[WRITE_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_DIMS];
    size_t      i, data_size, correct_buf_size;
    hid_t       file_id = H5I_INVALID_HID, dset_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID;
    hid_t       group_id  = H5I_INVALID_HID;
    hid_t       filespace = H5I_INVALID_HID, memspace = H5I_INVALID_HID;

    if (MAINPROCESS)
        HDputs("Testing write to unshared filtered chunks on separate pages in 3D dataset");

    file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, fapl_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
    VRFY((group_id >= 0), "H5Gopen2 succeeded");

    /* Create the dataspace for the dataset */
    dataset_dims[0] = (hsize_t)WRITE_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_NROWS;
    dataset_dims[1] = (hsize_t)WRITE_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_NCOLS;
    dataset_dims[2] = (hsize_t)WRITE_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DEPTH;
    chunk_dims[0]   = (hsize_t)WRITE_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_CH_NROWS;
    chunk_dims[1]   = (hsize_t)WRITE_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_CH_NCOLS;
    chunk_dims[2]   = 1;
    sel_dims[0]     = (hsize_t)WRITE_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_NROWS;
    sel_dims[1]     = (hsize_t)WRITE_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_NCOLS;
    sel_dims[2]     = 1;

    filespace = H5Screate_simple(WRITE_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_DIMS, dataset_dims, NULL);
    VRFY((filespace >= 0), "File dataspace creation succeeded");

    memspace = H5Screate_simple(WRITE_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_DIMS, sel_dims, NULL);
    VRFY((memspace >= 0), "Memory dataspace creation succeeded");

    /* Create chunked dataset */
    plist_id = H5Pcopy(dcpl_id);
    VRFY((plist_id >= 0), "DCPL copy succeeded");

    VRFY((H5Pset_chunk(plist_id, WRITE_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_DIMS, chunk_dims) >= 0),
         "Chunk size set");

    /* Add test filter to the pipeline */
    VRFY((set_dcpl_filter(plist_id, filter_id, NULL) >= 0), "Filter set");

    dset_id = H5Dcreate2(group_id, WRITE_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_NAME,
                         HDF5_DATATYPE_NAME, filespace, H5P_DEFAULT, plist_id, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset creation succeeded");

    /* Verify space allocation status */
    verify_space_alloc_status(dset_id, plist_id, DATASET_JUST_CREATED);

    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

    /* Each process defines the dataset selection in memory and writes
     * it to the hyperslab in the file
     */
    count[0] = (hsize_t)WRITE_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_NROWS /
               (hsize_t)WRITE_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_CH_NROWS;
    count[1] = (hsize_t)WRITE_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_NCOLS /
               (hsize_t)WRITE_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_CH_NCOLS;
    count[2]  = 1;
    stride[0] = (hsize_t)WRITE_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_CH_NROWS;
    stride[1] = (hsize_t)WRITE_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_CH_NCOLS;
    stride[2] = 1;
    block[0]  = (hsize_t)WRITE_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_CH_NROWS;
    block[1]  = (hsize_t)WRITE_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_CH_NCOLS;
    block[2]  = 1;
    start[0]  = 0;
    start[1]  = 0;
    start[2]  = (hsize_t)mpi_rank;

    if (VERBOSE_MED) {
        HDprintf("Process %d is writing with count[ %" PRIuHSIZE ", %" PRIuHSIZE ", %" PRIuHSIZE
                 " ], stride[ %" PRIuHSIZE ", %" PRIuHSIZE ", %" PRIuHSIZE " ], start[ %" PRIuHSIZE
                 ", %" PRIuHSIZE ", %" PRIuHSIZE " ], block size[ %" PRIuHSIZE ", %" PRIuHSIZE ", %" PRIuHSIZE
                 " ]\n",
                 mpi_rank, count[0], count[1], count[2], stride[0], stride[1], stride[2], start[0], start[1],
                 start[2], block[0], block[1], block[2]);
        HDfflush(stdout);
    }

    /* Select hyperslab in the file */
    filespace = H5Dget_space(dset_id);
    VRFY((filespace >= 0), "File dataspace retrieval succeeded");

    VRFY((H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, stride, count, block) >= 0),
         "Hyperslab selection succeeded");

    /* Fill data buffer */
    data_size        = sel_dims[0] * sel_dims[1] * sel_dims[2] * sizeof(*data);
    correct_buf_size = dataset_dims[0] * dataset_dims[1] * dataset_dims[2] * sizeof(*correct_buf);

    data = (C_DATATYPE *)HDcalloc(1, data_size);
    VRFY((NULL != data), "HDcalloc succeeded");

    correct_buf = (C_DATATYPE *)HDcalloc(1, correct_buf_size);
    VRFY((NULL != correct_buf), "HDcalloc succeeded");

    for (i = 0; i < data_size / sizeof(*data); i++)
        data[i] = (C_DATATYPE)GEN_DATA(i);

    for (i = 0; i < correct_buf_size / sizeof(*correct_buf); i++)
        correct_buf[i] = (C_DATATYPE)((i % (hsize_t)mpi_size) + (i / (hsize_t)mpi_size));

    VRFY((H5Dwrite(dset_id, HDF5_DATATYPE_NAME, memspace, filespace, dxpl_id, data) >= 0),
         "Dataset write succeeded");

    /* Verify space allocation status */
    verify_space_alloc_status(dset_id, plist_id, ALL_CHUNKS_WRITTEN);

    if (data)
        HDfree(data);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");

    /* Verify the correct data was written */
    read_buf = (C_DATATYPE *)HDcalloc(1, correct_buf_size);
    VRFY((NULL != read_buf), "HDcalloc succeeded");

    dset_id = H5Dopen2(group_id, WRITE_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_NAME, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    VRFY((H5Dread(dset_id, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, dxpl_id, read_buf) >= 0),
         "Dataset read succeeded");

    VRFY((0 == HDmemcmp(read_buf, correct_buf, correct_buf_size)), "Data verification succeeded");

    if (correct_buf)
        HDfree(correct_buf);
    if (read_buf)
        HDfree(read_buf);

    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Sclose(memspace) >= 0), "Memory dataspace close succeeded");
    VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
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
test_write_3d_filtered_dataset_no_overlap_same_pages(const char *parent_group, H5Z_filter_t filter_id,
                                                     hid_t fapl_id, hid_t dcpl_id, hid_t dxpl_id)
{
    C_DATATYPE *data        = NULL;
    C_DATATYPE *read_buf    = NULL;
    C_DATATYPE *correct_buf = NULL;
    hsize_t     dataset_dims[WRITE_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_DIMS];
    hsize_t     chunk_dims[WRITE_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_DIMS];
    hsize_t     sel_dims[WRITE_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_DIMS];
    hsize_t     start[WRITE_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_DIMS];
    hsize_t     stride[WRITE_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_DIMS];
    hsize_t     count[WRITE_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_DIMS];
    hsize_t     block[WRITE_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_DIMS];
    size_t      i, data_size, correct_buf_size;
    hid_t       file_id = H5I_INVALID_HID, dset_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID;
    hid_t       group_id  = H5I_INVALID_HID;
    hid_t       filespace = H5I_INVALID_HID, memspace = H5I_INVALID_HID;

    if (MAINPROCESS)
        HDputs("Testing write to unshared filtered chunks on the same pages in 3D dataset");

    file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, fapl_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
    VRFY((group_id >= 0), "H5Gopen2 succeeded");

    /* Create the dataspace for the dataset */
    dataset_dims[0] = (hsize_t)WRITE_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_NROWS;
    dataset_dims[1] = (hsize_t)WRITE_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_NCOLS;
    dataset_dims[2] = (hsize_t)WRITE_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DEPTH;
    chunk_dims[0]   = (hsize_t)WRITE_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_CH_NROWS;
    chunk_dims[1]   = (hsize_t)WRITE_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_CH_NCOLS;
    chunk_dims[2]   = 1;
    sel_dims[0]     = (hsize_t)WRITE_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_CH_NROWS;
    sel_dims[1]     = (hsize_t)WRITE_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_NCOLS;
    sel_dims[2]     = (hsize_t)WRITE_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DEPTH;

    filespace =
        H5Screate_simple(WRITE_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_DIMS, dataset_dims, NULL);
    VRFY((filespace >= 0), "File dataspace creation succeeded");

    memspace = H5Screate_simple(WRITE_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_DIMS, sel_dims, NULL);
    VRFY((memspace >= 0), "Memory dataspace creation succeeded");

    /* Create chunked dataset */
    plist_id = H5Pcopy(dcpl_id);
    VRFY((plist_id >= 0), "DCPL copy succeeded");

    VRFY((H5Pset_chunk(plist_id, WRITE_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_DIMS, chunk_dims) >= 0),
         "Chunk size set");

    /* Add test filter to the pipeline */
    VRFY((set_dcpl_filter(plist_id, filter_id, NULL) >= 0), "Filter set");

    dset_id = H5Dcreate2(group_id, WRITE_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_NAME,
                         HDF5_DATATYPE_NAME, filespace, H5P_DEFAULT, plist_id, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset creation succeeded");

    /* Verify space allocation status */
    verify_space_alloc_status(dset_id, plist_id, DATASET_JUST_CREATED);

    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

    /* Each process defines the dataset selection in memory and writes
     * it to the hyperslab in the file
     */
    count[0] = 1;
    count[1] = (hsize_t)WRITE_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_NCOLS /
               (hsize_t)WRITE_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_CH_NCOLS;
    count[2]  = (hsize_t)mpi_size;
    stride[0] = (hsize_t)WRITE_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_CH_NROWS;
    stride[1] = (hsize_t)WRITE_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_CH_NCOLS;
    stride[2] = 1;
    block[0]  = (hsize_t)WRITE_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_CH_NROWS;
    block[1]  = (hsize_t)WRITE_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_CH_NCOLS;
    block[2]  = 1;
    start[0] = ((hsize_t)mpi_rank * (hsize_t)WRITE_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_CH_NROWS * count[0]);
    start[1] = 0;
    start[2] = 0;

    if (VERBOSE_MED) {
        HDprintf("Process %d is writing with count[ %" PRIuHSIZE ", %" PRIuHSIZE ", %" PRIuHSIZE
                 " ], stride[ %" PRIuHSIZE ", %" PRIuHSIZE ", %" PRIuHSIZE " ], start[ %" PRIuHSIZE
                 ", %" PRIuHSIZE ", %" PRIuHSIZE " ], block size[ %" PRIuHSIZE ", %" PRIuHSIZE ", %" PRIuHSIZE
                 " ]\n",
                 mpi_rank, count[0], count[1], count[2], stride[0], stride[1], stride[2], start[0], start[1],
                 start[2], block[0], block[1], block[2]);
        HDfflush(stdout);
    }

    /* Select hyperslab in the file */
    filespace = H5Dget_space(dset_id);
    VRFY((filespace >= 0), "File dataspace retrieval succeeded");

    VRFY((H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, stride, count, block) >= 0),
         "Hyperslab selection succeeded");

    /* Fill data buffer */
    data_size        = sel_dims[0] * sel_dims[1] * sel_dims[2] * sizeof(*data);
    correct_buf_size = dataset_dims[0] * dataset_dims[1] * dataset_dims[2] * sizeof(*correct_buf);

    data = (C_DATATYPE *)HDcalloc(1, data_size);
    VRFY((NULL != data), "HDcalloc succeeded");

    correct_buf = (C_DATATYPE *)HDcalloc(1, correct_buf_size);
    VRFY((NULL != correct_buf), "HDcalloc succeeded");

    for (i = 0; i < data_size / sizeof(*data); i++)
        data[i] = (C_DATATYPE)GEN_DATA(i);

    for (i = 0; i < correct_buf_size / sizeof(*correct_buf); i++)
        correct_buf[i] = (C_DATATYPE)((i % (dataset_dims[0] * dataset_dims[1])) +
                                      (i / (dataset_dims[0] * dataset_dims[1])));

    VRFY((H5Dwrite(dset_id, HDF5_DATATYPE_NAME, memspace, filespace, dxpl_id, data) >= 0),
         "Dataset write succeeded");

    /* Verify space allocation status */
    verify_space_alloc_status(dset_id, plist_id, ALL_CHUNKS_WRITTEN);

    if (data)
        HDfree(data);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");

    /* Verify the correct data was written */
    read_buf = (C_DATATYPE *)HDcalloc(1, correct_buf_size);
    VRFY((NULL != read_buf), "HDcalloc succeeded");

    dset_id = H5Dopen2(group_id, WRITE_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_NAME, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    VRFY((H5Dread(dset_id, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, dxpl_id, read_buf) >= 0),
         "Dataset read succeeded");

    VRFY((0 == HDmemcmp(read_buf, correct_buf, correct_buf_size)), "Data verification succeeded");

    if (correct_buf)
        HDfree(correct_buf);
    if (read_buf)
        HDfree(read_buf);

    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Sclose(memspace) >= 0), "Memory dataspace close succeeded");
    VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
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
test_write_3d_filtered_dataset_overlap(const char *parent_group, H5Z_filter_t filter_id, hid_t fapl_id,
                                       hid_t dcpl_id, hid_t dxpl_id)
{
    C_DATATYPE *data        = NULL;
    C_DATATYPE *read_buf    = NULL;
    C_DATATYPE *correct_buf = NULL;
    hsize_t     dataset_dims[WRITE_SHARED_FILTERED_CHUNKS_3D_DATASET_DIMS];
    hsize_t     chunk_dims[WRITE_SHARED_FILTERED_CHUNKS_3D_DATASET_DIMS];
    hsize_t     sel_dims[WRITE_SHARED_FILTERED_CHUNKS_3D_DATASET_DIMS];
    hsize_t     start[WRITE_SHARED_FILTERED_CHUNKS_3D_DATASET_DIMS];
    hsize_t     stride[WRITE_SHARED_FILTERED_CHUNKS_3D_DATASET_DIMS];
    hsize_t     count[WRITE_SHARED_FILTERED_CHUNKS_3D_DATASET_DIMS];
    hsize_t     block[WRITE_SHARED_FILTERED_CHUNKS_3D_DATASET_DIMS];
    size_t      i, data_size, correct_buf_size;
    hid_t       file_id = H5I_INVALID_HID, dset_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID;
    hid_t       group_id  = H5I_INVALID_HID;
    hid_t       filespace = H5I_INVALID_HID, memspace = H5I_INVALID_HID;

    if (MAINPROCESS)
        HDputs("Testing write to shared filtered chunks in 3D dataset");

    file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, fapl_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
    VRFY((group_id >= 0), "H5Gopen2 succeeded");

    /* Create the dataspace for the dataset */
    dataset_dims[0] = (hsize_t)WRITE_SHARED_FILTERED_CHUNKS_3D_NROWS;
    dataset_dims[1] = (hsize_t)WRITE_SHARED_FILTERED_CHUNKS_3D_NCOLS;
    dataset_dims[2] = (hsize_t)WRITE_SHARED_FILTERED_CHUNKS_3D_DEPTH;
    chunk_dims[0]   = (hsize_t)WRITE_SHARED_FILTERED_CHUNKS_3D_CH_NROWS;
    chunk_dims[1]   = (hsize_t)WRITE_SHARED_FILTERED_CHUNKS_3D_CH_NCOLS;
    chunk_dims[2]   = 1;
    sel_dims[0]     = (hsize_t)(WRITE_SHARED_FILTERED_CHUNKS_3D_NROWS / mpi_size);
    sel_dims[1]     = (hsize_t)WRITE_SHARED_FILTERED_CHUNKS_3D_NCOLS;
    sel_dims[2]     = (hsize_t)WRITE_SHARED_FILTERED_CHUNKS_3D_DEPTH;

    filespace = H5Screate_simple(WRITE_SHARED_FILTERED_CHUNKS_3D_DATASET_DIMS, dataset_dims, NULL);
    VRFY((filespace >= 0), "File dataspace creation succeeded");

    memspace = H5Screate_simple(WRITE_SHARED_FILTERED_CHUNKS_3D_DATASET_DIMS, sel_dims, NULL);
    VRFY((memspace >= 0), "Memory dataspace creation succeeded");

    /* Create chunked dataset */
    plist_id = H5Pcopy(dcpl_id);
    VRFY((plist_id >= 0), "DCPL copy succeeded");

    VRFY((H5Pset_chunk(plist_id, WRITE_SHARED_FILTERED_CHUNKS_3D_DATASET_DIMS, chunk_dims) >= 0),
         "Chunk size set");

    /* Add test filter to the pipeline */
    VRFY((set_dcpl_filter(plist_id, filter_id, NULL) >= 0), "Filter set");

    dset_id = H5Dcreate2(group_id, WRITE_SHARED_FILTERED_CHUNKS_3D_DATASET_NAME, HDF5_DATATYPE_NAME,
                         filespace, H5P_DEFAULT, plist_id, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset creation succeeded");

    /* Verify space allocation status */
    verify_space_alloc_status(dset_id, plist_id, DATASET_JUST_CREATED);

    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

    /* Each process defines the dataset selection in memory and writes
     * it to the hyperslab in the file
     */
    count[0]  = (hsize_t)(WRITE_SHARED_FILTERED_CHUNKS_3D_NROWS / WRITE_SHARED_FILTERED_CHUNKS_3D_CH_NROWS);
    count[1]  = (hsize_t)(WRITE_SHARED_FILTERED_CHUNKS_3D_NCOLS / WRITE_SHARED_FILTERED_CHUNKS_3D_CH_NCOLS);
    count[2]  = (hsize_t)WRITE_SHARED_FILTERED_CHUNKS_3D_DEPTH;
    stride[0] = (hsize_t)WRITE_SHARED_FILTERED_CHUNKS_3D_CH_NROWS;
    stride[1] = (hsize_t)WRITE_SHARED_FILTERED_CHUNKS_3D_CH_NCOLS;
    stride[2] = 1;
    block[0]  = 1;
    block[1]  = (hsize_t)WRITE_SHARED_FILTERED_CHUNKS_3D_CH_NCOLS;
    block[2]  = 1;
    start[0]  = (hsize_t)mpi_rank;
    start[1]  = 0;
    start[2]  = 0;

    if (VERBOSE_MED) {
        HDprintf("Process %d is writing with count[ %" PRIuHSIZE ", %" PRIuHSIZE ", %" PRIuHSIZE
                 " ], stride[ %" PRIuHSIZE ", %" PRIuHSIZE ", %" PRIuHSIZE " ], start[ %" PRIuHSIZE
                 ", %" PRIuHSIZE ", %" PRIuHSIZE " ], block size[ %" PRIuHSIZE ", %" PRIuHSIZE ", %" PRIuHSIZE
                 " ]\n",
                 mpi_rank, count[0], count[1], count[2], stride[0], stride[1], stride[2], start[0], start[1],
                 start[2], block[0], block[1], block[2]);
        HDfflush(stdout);
    }

    /* Select hyperslab in the file */
    filespace = H5Dget_space(dset_id);
    VRFY((filespace >= 0), "File dataspace retrieval succeeded");

    VRFY((H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, stride, count, block) >= 0),
         "Hyperslab selection succeeded");

    /* Fill data buffer */
    data_size        = sel_dims[0] * sel_dims[1] * sel_dims[2] * sizeof(*data);
    correct_buf_size = dataset_dims[0] * dataset_dims[1] * dataset_dims[2] * sizeof(*correct_buf);

    data = (C_DATATYPE *)HDcalloc(1, data_size);
    VRFY((NULL != data), "HDcalloc succeeded");

    correct_buf = (C_DATATYPE *)HDcalloc(1, correct_buf_size);
    VRFY((NULL != correct_buf), "HDcalloc succeeded");

    for (i = 0; i < data_size / sizeof(*data); i++)
        data[i] = (C_DATATYPE)GEN_DATA(i);

    for (i = 0; i < correct_buf_size / sizeof(*correct_buf); i++)
        /* Add the Column Index */
        correct_buf[i] = (C_DATATYPE)((i % (hsize_t)(WRITE_SHARED_FILTERED_CHUNKS_3D_DEPTH *
                                                     WRITE_SHARED_FILTERED_CHUNKS_3D_NCOLS))

                                      /* Add the Row Index */
                                      + ((i % (hsize_t)(mpi_size * WRITE_SHARED_FILTERED_CHUNKS_3D_DEPTH *
                                                        WRITE_SHARED_FILTERED_CHUNKS_3D_NCOLS)) /
                                         (hsize_t)(WRITE_SHARED_FILTERED_CHUNKS_3D_DEPTH *
                                                   WRITE_SHARED_FILTERED_CHUNKS_3D_NCOLS))

                                      /* Add the amount that gets added when a rank moves down to its next
                                         section vertically in the dataset */
                                      + ((hsize_t)(WRITE_SHARED_FILTERED_CHUNKS_3D_DEPTH *
                                                   WRITE_SHARED_FILTERED_CHUNKS_3D_NCOLS) *
                                         (i / (hsize_t)(mpi_size * WRITE_SHARED_FILTERED_CHUNKS_3D_DEPTH *
                                                        WRITE_SHARED_FILTERED_CHUNKS_3D_NCOLS))));

    VRFY((H5Dwrite(dset_id, HDF5_DATATYPE_NAME, memspace, filespace, dxpl_id, data) >= 0),
         "Dataset write succeeded");

    /* Verify space allocation status */
    verify_space_alloc_status(dset_id, plist_id, ALL_CHUNKS_WRITTEN);

    if (data)
        HDfree(data);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");

    /* Verify the correct data was written */
    read_buf = (C_DATATYPE *)HDcalloc(1, correct_buf_size);
    VRFY((NULL != read_buf), "HDcalloc succeeded");

    dset_id = H5Dopen2(group_id, WRITE_SHARED_FILTERED_CHUNKS_3D_DATASET_NAME, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    VRFY((H5Dread(dset_id, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, dxpl_id, read_buf) >= 0),
         "Dataset read succeeded");

    VRFY((0 == HDmemcmp(read_buf, correct_buf, correct_buf_size)), "Data verification succeeded");

    if (correct_buf)
        HDfree(correct_buf);
    if (read_buf)
        HDfree(read_buf);

    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Sclose(memspace) >= 0), "Memory dataspace close succeeded");
    VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
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
test_write_cmpd_filtered_dataset_no_conversion_unshared(const char *parent_group, H5Z_filter_t filter_id,
                                                        hid_t fapl_id, hid_t dcpl_id, hid_t dxpl_id)
{
    COMPOUND_C_DATATYPE *data        = NULL;
    COMPOUND_C_DATATYPE *read_buf    = NULL;
    COMPOUND_C_DATATYPE *correct_buf = NULL;
    hsize_t              dataset_dims[WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t              chunk_dims[WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t              sel_dims[WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t              start[WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t              stride[WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t              count[WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t              block[WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_DIMS];
    size_t               i, correct_buf_size;
    hid_t                file_id = H5I_INVALID_HID, dset_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID,
          memtype   = H5I_INVALID_HID;
    hid_t group_id  = H5I_INVALID_HID;
    hid_t filespace = H5I_INVALID_HID, memspace = H5I_INVALID_HID;

    if (MAINPROCESS)
        HDputs("Testing write to unshared filtered chunks in Compound Datatype dataset without Datatype "
               "conversion");

    /* SZIP and ScaleOffset filters don't support compound types */
    if (filter_id == H5Z_FILTER_SZIP || filter_id == H5Z_FILTER_SCALEOFFSET) {
        if (MAINPROCESS)
            SKIPPED();
        return;
    }

    file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, fapl_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
    VRFY((group_id >= 0), "H5Gopen2 succeeded");

    /* Create the dataspace for the dataset */
    dataset_dims[0] = WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_NROWS;
    dataset_dims[1] = (hsize_t)WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_NCOLS;
    chunk_dims[0]   = WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_CH_NROWS;
    chunk_dims[1]   = WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_CH_NCOLS;
    sel_dims[0]     = WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_CH_NROWS;
    sel_dims[1]     = (hsize_t)WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_ENTRIES_PER_PROC;

    filespace = H5Screate_simple(WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_DIMS,
                                 dataset_dims, NULL);
    VRFY((filespace >= 0), "File dataspace creation succeeded");

    memspace =
        H5Screate_simple(WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_DIMS, sel_dims, NULL);
    VRFY((memspace >= 0), "Memory dataspace creation succeeded");

    /* Create chunked dataset */
    plist_id = H5Pcopy(dcpl_id);
    VRFY((plist_id >= 0), "DCPL copy succeeded");

    VRFY((H5Pset_chunk(plist_id, WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_DIMS,
                       chunk_dims) >= 0),
         "Chunk size set");

    /* Add test filter to the pipeline */
    VRFY((set_dcpl_filter(plist_id, filter_id, NULL) >= 0), "Filter set");

    /* Create the compound type for memory. */
    memtype = H5Tcreate(H5T_COMPOUND, sizeof(COMPOUND_C_DATATYPE));
    VRFY((memtype >= 0), "Datatype creation succeeded");

    VRFY((H5Tinsert(memtype, "ShortData", HOFFSET(COMPOUND_C_DATATYPE, field1), H5T_NATIVE_SHORT) >= 0),
         "Datatype insertion succeeded");
    VRFY((H5Tinsert(memtype, "IntData", HOFFSET(COMPOUND_C_DATATYPE, field2), H5T_NATIVE_INT) >= 0),
         "Datatype insertion succeeded");
    VRFY((H5Tinsert(memtype, "LongData", HOFFSET(COMPOUND_C_DATATYPE, field3), H5T_NATIVE_LONG) >= 0),
         "Datatype insertion succeeded");

    dset_id = H5Dcreate2(group_id, WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_NAME,
                         memtype, filespace, H5P_DEFAULT, plist_id, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset creation succeeded");

    /* Verify space allocation status */
    verify_space_alloc_status(dset_id, plist_id, DATASET_JUST_CREATED);

    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

    /* Each process defines the dataset selection in memory and writes
     * it to the hyperslab in the file
     */
    count[0]  = 1;
    count[1]  = (hsize_t)WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_ENTRIES_PER_PROC;
    stride[0] = WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_CH_NROWS;
    stride[1] = WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_CH_NCOLS;
    block[0]  = WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_CH_NROWS;
    block[1]  = WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_CH_NCOLS;
    start[0]  = 0;
    start[1]  = ((hsize_t)mpi_rank * WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_CH_NCOLS);

    if (VERBOSE_MED) {
        HDprintf("Process %d is writing with count[ %" PRIuHSIZE ", %" PRIuHSIZE " ], stride[ %" PRIuHSIZE
                 ", %" PRIuHSIZE " ], start[ %" PRIuHSIZE ", %" PRIuHSIZE " ], block size[ %" PRIuHSIZE
                 ", %" PRIuHSIZE " ]\n",
                 mpi_rank, count[0], count[1], stride[0], stride[1], start[0], start[1], block[0], block[1]);
        HDfflush(stdout);
    }

    /* Select hyperslab in the file */
    filespace = H5Dget_space(dset_id);
    VRFY((filespace >= 0), "File dataspace retrieval succeeded");

    VRFY((H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, stride, count, block) >= 0),
         "Hyperslab selection succeeded");

    data = (COMPOUND_C_DATATYPE *)HDcalloc(
        1, (hsize_t)WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_ENTRIES_PER_PROC * sizeof(*data));
    VRFY((NULL != data), "HDcalloc succeeded");

    correct_buf_size = dataset_dims[0] * dataset_dims[1] * sizeof(COMPOUND_C_DATATYPE);

    correct_buf = (COMPOUND_C_DATATYPE *)HDcalloc(1, correct_buf_size);
    VRFY((NULL != correct_buf), "HDcalloc succeeded");

    /* Fill data buffer */
    for (i = 0; i < (hsize_t)WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_ENTRIES_PER_PROC; i++) {
        data[i].field1 = (short)GEN_DATA(i);
        data[i].field2 = (int)GEN_DATA(i);
        data[i].field3 = (long)GEN_DATA(i);
    }

    for (i = 0; i < correct_buf_size / sizeof(*correct_buf); i++) {
        correct_buf[i].field1 = (short)((i % dataset_dims[1]) + (i / dataset_dims[1]));

        correct_buf[i].field2 = (int)((i % dataset_dims[1]) + (i / dataset_dims[1]));

        correct_buf[i].field3 = (long)((i % dataset_dims[1]) + (i / dataset_dims[1]));
    }

    VRFY((H5Dwrite(dset_id, memtype, memspace, filespace, dxpl_id, data) >= 0), "Dataset write succeeded");

    /* Verify space allocation status */
    verify_space_alloc_status(dset_id, plist_id, ALL_CHUNKS_WRITTEN);

    if (data)
        HDfree(data);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");

    /* Verify the correct data was written */
    read_buf = (COMPOUND_C_DATATYPE *)HDcalloc(1, correct_buf_size);
    VRFY((NULL != read_buf), "HDcalloc succeeded");

    dset_id =
        H5Dopen2(group_id, WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_NAME, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    VRFY((H5Dread(dset_id, memtype, H5S_ALL, H5S_ALL, dxpl_id, read_buf) >= 0), "Dataset read succeeded");

    VRFY((0 == HDmemcmp(read_buf, correct_buf, correct_buf_size)), "Data verification succeeded");

    if (correct_buf)
        HDfree(correct_buf);
    if (read_buf)
        HDfree(read_buf);

    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Sclose(memspace) >= 0), "Memory dataspace close succeeded");
    VRFY((H5Tclose(memtype) >= 0), "Datatype close succeeded");
    VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
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
test_write_cmpd_filtered_dataset_no_conversion_shared(const char *parent_group, H5Z_filter_t filter_id,
                                                      hid_t fapl_id, hid_t dcpl_id, hid_t dxpl_id)
{
    COMPOUND_C_DATATYPE *data        = NULL;
    COMPOUND_C_DATATYPE *read_buf    = NULL;
    COMPOUND_C_DATATYPE *correct_buf = NULL;
    hsize_t              dataset_dims[WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t              chunk_dims[WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t              sel_dims[WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t              start[WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t              stride[WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t              count[WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t              block[WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_DIMS];
    size_t               i, correct_buf_size;
    hid_t                file_id = H5I_INVALID_HID, dset_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID,
          memtype   = H5I_INVALID_HID;
    hid_t group_id  = H5I_INVALID_HID;
    hid_t filespace = H5I_INVALID_HID, memspace = H5I_INVALID_HID;

    if (MAINPROCESS)
        HDputs("Testing write to shared filtered chunks in Compound Datatype dataset without Datatype "
               "conversion");

    /* SZIP and ScaleOffset filters don't support compound types */
    if (filter_id == H5Z_FILTER_SZIP || filter_id == H5Z_FILTER_SCALEOFFSET) {
        if (MAINPROCESS)
            SKIPPED();
        return;
    }

    file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, fapl_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
    VRFY((group_id >= 0), "H5Gopen2 succeeded");

    /* Create the dataspace for the dataset */
    dataset_dims[0] = (hsize_t)WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_NROWS;
    dataset_dims[1] = (hsize_t)WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_NCOLS;
    chunk_dims[0]   = (hsize_t)WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_CH_NROWS;
    chunk_dims[1]   = WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_CH_NCOLS;
    sel_dims[0] = (hsize_t)WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_CH_NROWS / (hsize_t)mpi_size;
    sel_dims[1] = (hsize_t)WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_ENTRIES_PER_PROC;

    filespace = H5Screate_simple(WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_DIMS,
                                 dataset_dims, NULL);
    VRFY((filespace >= 0), "File dataspace creation succeeded");

    memspace =
        H5Screate_simple(WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_DIMS, sel_dims, NULL);
    VRFY((memspace >= 0), "Memory dataspace creation succeeded");

    /* Create chunked dataset */
    plist_id = H5Pcopy(dcpl_id);
    VRFY((plist_id >= 0), "DCPL copy succeeded");

    VRFY((H5Pset_chunk(plist_id, WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_DIMS,
                       chunk_dims) >= 0),
         "Chunk size set");

    /* Add test filter to the pipeline */
    VRFY((set_dcpl_filter(plist_id, filter_id, NULL) >= 0), "Filter set");

    /* Create the compound type for memory. */
    memtype = H5Tcreate(H5T_COMPOUND, sizeof(COMPOUND_C_DATATYPE));
    VRFY((memtype >= 0), "Datatype creation succeeded");

    VRFY((H5Tinsert(memtype, "ShortData", HOFFSET(COMPOUND_C_DATATYPE, field1), H5T_NATIVE_SHORT) >= 0),
         "Datatype insertion succeeded");
    VRFY((H5Tinsert(memtype, "IntData", HOFFSET(COMPOUND_C_DATATYPE, field2), H5T_NATIVE_INT) >= 0),
         "Datatype insertion succeeded");
    VRFY((H5Tinsert(memtype, "LongData", HOFFSET(COMPOUND_C_DATATYPE, field3), H5T_NATIVE_LONG) >= 0),
         "Datatype insertion succeeded");

    dset_id = H5Dcreate2(group_id, WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_NAME, memtype,
                         filespace, H5P_DEFAULT, plist_id, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset creation succeeded");

    /* Verify space allocation status */
    verify_space_alloc_status(dset_id, plist_id, DATASET_JUST_CREATED);

    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

    /* Each process defines the dataset selection in memory and writes
     * it to the hyperslab in the file
     */
    count[0]  = 1;
    count[1]  = (hsize_t)WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_ENTRIES_PER_PROC;
    stride[0] = (hsize_t)WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_CH_NROWS;
    stride[1] = WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_CH_NCOLS;
    block[0]  = (hsize_t)WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_CH_NROWS / (hsize_t)mpi_size;
    block[1]  = WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_CH_NCOLS;
    start[0]  = (hsize_t)mpi_rank;
    start[1]  = 0;

    if (VERBOSE_MED) {
        HDprintf("Process %d is writing with count[ %" PRIuHSIZE ", %" PRIuHSIZE " ], stride[ %" PRIuHSIZE
                 ", %" PRIuHSIZE " ], start[ %" PRIuHSIZE ", %" PRIuHSIZE " ], block size[ %" PRIuHSIZE
                 ", %" PRIuHSIZE " ]\n",
                 mpi_rank, count[0], count[1], stride[0], stride[1], start[0], start[1], block[0], block[1]);
        HDfflush(stdout);
    }

    /* Select hyperslab in the file */
    filespace = H5Dget_space(dset_id);
    VRFY((filespace >= 0), "File dataspace retrieval succeeded");

    VRFY((H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, stride, count, block) >= 0),
         "Hyperslab selection succeeded");

    data = (COMPOUND_C_DATATYPE *)HDcalloc(
        1, (hsize_t)WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_ENTRIES_PER_PROC * sizeof(*data));
    VRFY((NULL != data), "HDcalloc succeeded");

    correct_buf_size = dataset_dims[0] * dataset_dims[1] * sizeof(COMPOUND_C_DATATYPE);

    correct_buf = (COMPOUND_C_DATATYPE *)HDcalloc(1, correct_buf_size);
    VRFY((NULL != correct_buf), "HDcalloc succeeded");

    /* Fill data buffer */
    for (i = 0; i < (hsize_t)WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_ENTRIES_PER_PROC; i++) {
        data[i].field1 = (short)GEN_DATA(i);
        data[i].field2 = (int)GEN_DATA(i);
        data[i].field3 = (long)GEN_DATA(i);
    }

    for (i = 0; i < correct_buf_size / sizeof(*correct_buf); i++) {
        correct_buf[i].field1 =
            (short)((dataset_dims[1] * (i / ((hsize_t)mpi_size * dataset_dims[1]))) + (i % dataset_dims[1]) +
                    (((i % ((hsize_t)mpi_size * dataset_dims[1])) / dataset_dims[1]) % dataset_dims[1]));

        correct_buf[i].field2 =
            (int)((dataset_dims[1] * (i / ((hsize_t)mpi_size * dataset_dims[1]))) + (i % dataset_dims[1]) +
                  (((i % ((hsize_t)mpi_size * dataset_dims[1])) / dataset_dims[1]) % dataset_dims[1]));

        correct_buf[i].field3 =
            (long)((dataset_dims[1] * (i / ((hsize_t)mpi_size * dataset_dims[1]))) + (i % dataset_dims[1]) +
                   (((i % ((hsize_t)mpi_size * dataset_dims[1])) / dataset_dims[1]) % dataset_dims[1]));
    }

    VRFY((H5Dwrite(dset_id, memtype, memspace, filespace, dxpl_id, data) >= 0), "Dataset write succeeded");

    /* Verify space allocation status */
    verify_space_alloc_status(dset_id, plist_id, ALL_CHUNKS_WRITTEN);

    if (data)
        HDfree(data);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");

    /* Verify the correct data was written */
    read_buf = (COMPOUND_C_DATATYPE *)HDcalloc(1, correct_buf_size);
    VRFY((NULL != read_buf), "HDcalloc succeeded");

    dset_id =
        H5Dopen2(group_id, WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_NAME, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    VRFY((H5Dread(dset_id, memtype, H5S_ALL, H5S_ALL, dxpl_id, read_buf) >= 0), "Dataset read succeeded");

    VRFY((0 == HDmemcmp(read_buf, correct_buf, correct_buf_size)), "Data verification succeeded");

    if (correct_buf)
        HDfree(correct_buf);
    if (read_buf)
        HDfree(read_buf);

    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Sclose(memspace) >= 0), "Memory dataspace close succeeded");
    VRFY((H5Tclose(memtype) >= 0), "Datatype close succeeded");
    VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    return;
}

/*
 * Tests parallel write of filtered data to unshared
 * chunks using a compound datatype which requires a
 * datatype conversion.
 *
 * NOTE: This test currently should fail for mpi_size > 1
 * because the datatype conversion causes the parallel
 * library to break to independent I/O and this isn't
 * allowed when there are filters in the pipeline,
 * unless there is only one MPI rank.
 *
 * Programmer: Jordan Henderson
 *             02/07/2017
 */
static void
test_write_cmpd_filtered_dataset_type_conversion_unshared(const char *parent_group, H5Z_filter_t filter_id,
                                                          hid_t fapl_id, hid_t dcpl_id, hid_t dxpl_id)
{
    COMPOUND_C_DATATYPE *data        = NULL;
    COMPOUND_C_DATATYPE *read_buf    = NULL;
    COMPOUND_C_DATATYPE *correct_buf = NULL;
    hsize_t              dataset_dims[WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t              chunk_dims[WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t              sel_dims[WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t              start[WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t              stride[WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t              count[WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t              block[WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_DIMS];
    size_t               i, correct_buf_size;
    hid_t                file_id = H5I_INVALID_HID, dset_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID,
          filetype = H5I_INVALID_HID, memtype = H5I_INVALID_HID;
    hid_t group_id  = H5I_INVALID_HID;
    hid_t filespace = H5I_INVALID_HID, memspace = H5I_INVALID_HID;

    if (MAINPROCESS)
        HDputs("Testing write to unshared filtered chunks in Compound Datatype dataset with Datatype "
               "conversion");

    /* Skip for MPI communicator size of 1 */
    if (mpi_size == 1) {
        SKIPPED();
        return;
    }

    /* SZIP and ScaleOffset filters don't support compound types */
    if (filter_id == H5Z_FILTER_SZIP || filter_id == H5Z_FILTER_SCALEOFFSET) {
        if (MAINPROCESS)
            SKIPPED();
        return;
    }

    file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, fapl_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
    VRFY((group_id >= 0), "H5Gopen2 succeeded");

    /* Create the dataspace for the dataset */
    dataset_dims[0] = WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_NROWS;
    dataset_dims[1] = (hsize_t)WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_NCOLS;
    chunk_dims[0]   = WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_CH_NROWS;
    chunk_dims[1]   = WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_CH_NCOLS;
    sel_dims[0]     = WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_CH_NROWS;
    sel_dims[1]     = (hsize_t)WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_ENTRIES_PER_PROC;

    filespace = H5Screate_simple(WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_DIMS,
                                 dataset_dims, NULL);
    VRFY((filespace >= 0), "File dataspace creation succeeded");

    memspace = H5Screate_simple(WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_DIMS,
                                sel_dims, NULL);
    VRFY((memspace >= 0), "Memory dataspace creation succeeded");

    /* Create chunked dataset */
    plist_id = H5Pcopy(dcpl_id);
    VRFY((plist_id >= 0), "DCPL copy succeeded");

    VRFY((H5Pset_chunk(plist_id, WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_DIMS,
                       chunk_dims) >= 0),
         "Chunk size set");

    /* Add test filter to the pipeline */
    VRFY((set_dcpl_filter(plist_id, filter_id, NULL) >= 0), "Filter set");

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

    VRFY((H5Tinsert(filetype, "ShortData", 0, H5T_STD_I64BE) >= 0), "Datatype insertion succeeded");
    VRFY((H5Tinsert(filetype, "IntData", 8, H5T_STD_I64BE) >= 0), "Datatype insertion succeeded");
    VRFY((H5Tinsert(filetype, "LongData", 16, H5T_STD_I64BE) >= 0), "Datatype insertion succeeded");

    dset_id = H5Dcreate2(group_id, WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_NAME,
                         filetype, filespace, H5P_DEFAULT, plist_id, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset creation succeeded");

    /* Verify space allocation status */
    verify_space_alloc_status(dset_id, plist_id, DATASET_JUST_CREATED);

    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

    /* Each process defines the dataset selection in memory and writes
     * it to the hyperslab in the file
     */
    count[0]  = 1;
    count[1]  = (hsize_t)WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_ENTRIES_PER_PROC;
    stride[0] = WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_CH_NROWS;
    stride[1] = WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_CH_NCOLS;
    block[0]  = WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_CH_NROWS;
    block[1]  = WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_CH_NCOLS;
    start[0]  = 0;
    start[1]  = ((hsize_t)mpi_rank * WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_CH_NCOLS);

    if (VERBOSE_MED) {
        HDprintf("Process %d is writing with count[ %" PRIuHSIZE ", %" PRIuHSIZE " ], stride[ %" PRIuHSIZE
                 ", %" PRIuHSIZE " ], start[ %" PRIuHSIZE ", %" PRIuHSIZE " ], block size[ %" PRIuHSIZE
                 ", %" PRIuHSIZE " ]\n",
                 mpi_rank, count[0], count[1], stride[0], stride[1], start[0], start[1], block[0], block[1]);
        HDfflush(stdout);
    }

    /* Select hyperslab in the file */
    filespace = H5Dget_space(dset_id);
    VRFY((filespace >= 0), "File dataspace retrieval succeeded");

    VRFY((H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, stride, count, block) >= 0),
         "Hyperslab selection succeeded");

    data = (COMPOUND_C_DATATYPE *)HDcalloc(
        1, (hsize_t)WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_ENTRIES_PER_PROC * sizeof(*data));
    VRFY((NULL != data), "HDcalloc succeeded");

    correct_buf_size = dataset_dims[0] * dataset_dims[1] * sizeof(COMPOUND_C_DATATYPE);

    correct_buf = (COMPOUND_C_DATATYPE *)HDcalloc(1, correct_buf_size);
    VRFY((NULL != correct_buf), "HDcalloc succeeded");

    /* Fill data buffer */
    for (i = 0; i < (hsize_t)WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_ENTRIES_PER_PROC; i++) {
        data[i].field1 = (short)GEN_DATA(i);
        data[i].field2 = (int)GEN_DATA(i);
        data[i].field3 = (long)GEN_DATA(i);
    }

    /* Ensure that this test currently fails since type conversions break collective mode */
    H5E_BEGIN_TRY
    {
        VRFY((H5Dwrite(dset_id, memtype, memspace, filespace, dxpl_id, data) < 0), "Dataset write succeeded");
    }
    H5E_END_TRY;

    /* Verify space allocation status */
    verify_space_alloc_status(dset_id, plist_id, NO_CHUNKS_WRITTEN);

    if (data)
        HDfree(data);

    /* Verify that no data was written */
    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");

    read_buf = (COMPOUND_C_DATATYPE *)HDcalloc(1, correct_buf_size);
    VRFY((NULL != read_buf), "HDcalloc succeeded");

    dset_id =
        H5Dopen2(group_id, WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_NAME, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    VRFY((H5Dread(dset_id, memtype, H5S_ALL, H5S_ALL, dxpl_id, read_buf) >= 0), "Dataset read succeeded");

    VRFY((0 == HDmemcmp(read_buf, correct_buf, correct_buf_size)), "Data verification succeeded");

    if (correct_buf)
        HDfree(correct_buf);
    if (read_buf)
        HDfree(read_buf);

    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Sclose(memspace) >= 0), "Memory dataspace close succeeded");
    VRFY((H5Tclose(filetype) >= 0), "File datatype close succeeded");
    VRFY((H5Tclose(memtype) >= 0), "Memory datatype close succeeded");
    VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    return;
}

/*
 * Tests parallel write of filtered data to shared
 * chunks using a compound datatype which requires
 * a datatype conversion.
 *
 * NOTE: This test currently should fail for mpi_size > 1
 * because the datatype conversion causes the parallel
 * library to break to independent I/O and this isn't
 * allowed when there are filters in the pipeline,
 * unless there is only one MPI rank.
 *
 * Programmer: Jordan Henderson
 *             02/10/2017
 */
static void
test_write_cmpd_filtered_dataset_type_conversion_shared(const char *parent_group, H5Z_filter_t filter_id,
                                                        hid_t fapl_id, hid_t dcpl_id, hid_t dxpl_id)
{
    COMPOUND_C_DATATYPE *data        = NULL;
    COMPOUND_C_DATATYPE *read_buf    = NULL;
    COMPOUND_C_DATATYPE *correct_buf = NULL;
    hsize_t              dataset_dims[WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t              chunk_dims[WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t              sel_dims[WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t              start[WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t              stride[WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t              count[WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t              block[WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_DIMS];
    size_t               i, correct_buf_size;
    hid_t                file_id = H5I_INVALID_HID, dset_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID;
    hid_t                filetype = H5I_INVALID_HID, memtype = H5I_INVALID_HID;
    hid_t                group_id  = H5I_INVALID_HID;
    hid_t                filespace = H5I_INVALID_HID, memspace = H5I_INVALID_HID;

    if (MAINPROCESS)
        HDputs(
            "Testing write to shared filtered chunks in Compound Datatype dataset with Datatype conversion");

    /* Skip for MPI communicator size of 1 */
    if (mpi_size == 1) {
        SKIPPED();
        return;
    }

    /* SZIP and ScaleOffset filters don't support compound types */
    if (filter_id == H5Z_FILTER_SZIP || filter_id == H5Z_FILTER_SCALEOFFSET) {
        if (MAINPROCESS)
            SKIPPED();
        return;
    }

    file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, fapl_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
    VRFY((group_id >= 0), "H5Gopen2 succeeded");

    /* Create the dataspace for the dataset */
    dataset_dims[0] = (hsize_t)WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_NROWS;
    dataset_dims[1] = (hsize_t)WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_NCOLS;
    chunk_dims[0]   = (hsize_t)WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_CH_NROWS;
    chunk_dims[1]   = WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_CH_NCOLS;
    sel_dims[0] = (hsize_t)WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_CH_NROWS / (hsize_t)mpi_size;
    sel_dims[1] = (hsize_t)WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_ENTRIES_PER_PROC;

    filespace = H5Screate_simple(WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_DIMS,
                                 dataset_dims, NULL);
    VRFY((filespace >= 0), "File dataspace creation succeeded");

    memspace =
        H5Screate_simple(WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_DIMS, sel_dims, NULL);
    VRFY((memspace >= 0), "Memory dataspace creation succeeded");

    /* Create chunked dataset */
    plist_id = H5Pcopy(dcpl_id);
    VRFY((plist_id >= 0), "DCPL copy succeeded");

    VRFY((H5Pset_chunk(plist_id, WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_DIMS,
                       chunk_dims) >= 0),
         "Chunk size set");

    /* Add test filter to the pipeline */
    VRFY((set_dcpl_filter(plist_id, filter_id, NULL) >= 0), "Filter set");

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

    VRFY((H5Tinsert(filetype, "ShortData", 0, H5T_STD_I64BE) >= 0), "Datatype insertion succeeded");
    VRFY((H5Tinsert(filetype, "IntData", 8, H5T_STD_I64BE) >= 0), "Datatype insertion succeeded");
    VRFY((H5Tinsert(filetype, "LongData", 16, H5T_STD_I64BE) >= 0), "Datatype insertion succeeded");

    dset_id = H5Dcreate2(group_id, WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_NAME,
                         filetype, filespace, H5P_DEFAULT, plist_id, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset creation succeeded");

    /* Verify space allocation status */
    verify_space_alloc_status(dset_id, plist_id, DATASET_JUST_CREATED);

    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

    /* Each process defines the dataset selection in memory and writes
     * it to the hyperslab in the file
     */
    count[0]  = 1;
    count[1]  = (hsize_t)WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_ENTRIES_PER_PROC;
    stride[0] = (hsize_t)WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_CH_NROWS;
    stride[1] = WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_CH_NCOLS;
    block[0]  = (hsize_t)WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_CH_NROWS / (hsize_t)mpi_size;
    block[1]  = WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_CH_NCOLS;
    start[0]  = (hsize_t)mpi_rank;
    start[1]  = 0;

    if (VERBOSE_MED) {
        HDprintf("Process %d is writing with count[ %" PRIuHSIZE ", %" PRIuHSIZE " ], stride[ %" PRIuHSIZE
                 ", %" PRIuHSIZE " ], start[ %" PRIuHSIZE ", %" PRIuHSIZE " ], block size[ %" PRIuHSIZE
                 ", %" PRIuHSIZE " ]\n",
                 mpi_rank, count[0], count[1], stride[0], stride[1], start[0], start[1], block[0], block[1]);
        HDfflush(stdout);
    }

    /* Select hyperslab in the file */
    filespace = H5Dget_space(dset_id);
    VRFY((filespace >= 0), "File dataspace retrieval succeeded");

    VRFY((H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, stride, count, block) >= 0),
         "Hyperslab selection succeeded");

    data = (COMPOUND_C_DATATYPE *)HDcalloc(
        1, (hsize_t)WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_ENTRIES_PER_PROC * sizeof(*data));
    VRFY((NULL != data), "HDcalloc succeeded");

    correct_buf_size = dataset_dims[0] * dataset_dims[1] * sizeof(COMPOUND_C_DATATYPE);

    correct_buf = (COMPOUND_C_DATATYPE *)HDcalloc(1, correct_buf_size);
    VRFY((NULL != correct_buf), "HDcalloc succeeded");

    /* Fill data buffer */
    for (i = 0; i < (hsize_t)WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_ENTRIES_PER_PROC; i++) {
        data[i].field1 = (short)GEN_DATA(i);
        data[i].field2 = (int)GEN_DATA(i);
        data[i].field3 = (long)GEN_DATA(i);
    }

    /* Ensure that this test currently fails since type conversions break collective mode */
    H5E_BEGIN_TRY
    {
        VRFY((H5Dwrite(dset_id, memtype, memspace, filespace, dxpl_id, data) < 0), "Dataset write succeeded");
    }
    H5E_END_TRY;

    /* Verify space allocation status */
    verify_space_alloc_status(dset_id, plist_id, NO_CHUNKS_WRITTEN);

    if (data)
        HDfree(data);

    /* Verify that no data was written */
    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");

    read_buf = (COMPOUND_C_DATATYPE *)HDcalloc(1, correct_buf_size);
    VRFY((NULL != read_buf), "HDcalloc succeeded");

    dset_id =
        H5Dopen2(group_id, WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_NAME, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    VRFY((H5Dread(dset_id, memtype, H5S_ALL, H5S_ALL, dxpl_id, read_buf) >= 0), "Dataset read succeeded");

    VRFY((0 == HDmemcmp(read_buf, correct_buf, correct_buf_size)), "Data verification succeeded");

    if (correct_buf)
        HDfree(correct_buf);
    if (read_buf)
        HDfree(read_buf);

    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Sclose(memspace) >= 0), "Memory dataspace close succeeded");
    VRFY((H5Tclose(filetype) >= 0), "File datatype close succeeded");
    VRFY((H5Tclose(memtype) >= 0), "Memory datatype close succeeded");
    VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
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
test_read_one_chunk_filtered_dataset(const char *parent_group, H5Z_filter_t filter_id, hid_t fapl_id,
                                     hid_t dcpl_id, hid_t dxpl_id)
{
    C_DATATYPE *read_buf    = NULL;
    C_DATATYPE *correct_buf = NULL;
    C_DATATYPE *global_buf  = NULL;
    hsize_t     dataset_dims[READ_ONE_CHUNK_FILTERED_DATASET_DIMS];
    hsize_t     chunk_dims[READ_ONE_CHUNK_FILTERED_DATASET_DIMS];
    hsize_t     sel_dims[READ_ONE_CHUNK_FILTERED_DATASET_DIMS];
    hsize_t     start[READ_ONE_CHUNK_FILTERED_DATASET_DIMS];
    hsize_t     stride[READ_ONE_CHUNK_FILTERED_DATASET_DIMS];
    hsize_t     count[READ_ONE_CHUNK_FILTERED_DATASET_DIMS];
    hsize_t     block[READ_ONE_CHUNK_FILTERED_DATASET_DIMS];
    hsize_t     flat_dims[1];
    size_t      i, read_buf_size, correct_buf_size;
    hid_t       file_id = H5I_INVALID_HID, dset_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID;
    hid_t       group_id  = H5I_INVALID_HID;
    hid_t       filespace = H5I_INVALID_HID, memspace = H5I_INVALID_HID;
    int        *recvcounts = NULL;
    int        *displs     = NULL;

    if (MAINPROCESS)
        HDputs("Testing read from one-chunk filtered dataset");

    dataset_dims[0] = (hsize_t)READ_ONE_CHUNK_FILTERED_DATASET_NROWS;
    dataset_dims[1] = (hsize_t)READ_ONE_CHUNK_FILTERED_DATASET_NCOLS;

    /* Setup the buffer for writing and for comparison */
    correct_buf_size = dataset_dims[0] * dataset_dims[1] * sizeof(*correct_buf);

    correct_buf = (C_DATATYPE *)HDcalloc(1, correct_buf_size);
    VRFY((NULL != correct_buf), "HDcalloc succeeded");

    for (i = 0; i < correct_buf_size / sizeof(*correct_buf); i++)
        correct_buf[i] = ((C_DATATYPE)i % (READ_ONE_CHUNK_FILTERED_DATASET_CH_NROWS / mpi_size *
                                           READ_ONE_CHUNK_FILTERED_DATASET_CH_NCOLS)) +
                         ((C_DATATYPE)i / (READ_ONE_CHUNK_FILTERED_DATASET_CH_NROWS / mpi_size *
                                           READ_ONE_CHUNK_FILTERED_DATASET_CH_NCOLS));

    if (MAINPROCESS) {
        plist_id = H5Pcreate(H5P_FILE_ACCESS);
        VRFY((plist_id >= 0), "FAPL creation succeeded");

        VRFY((H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0),
             "Set libver bounds succeeded");

        file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, plist_id);
        VRFY((file_id >= 0), "Test file open succeeded");

        VRFY((H5Pclose(plist_id) >= 0), "FAPL close succeeded");

        group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
        VRFY((group_id >= 0), "H5Gopen2 succeeded");

        /* Create the dataspace for the dataset */
        filespace = H5Screate_simple(READ_ONE_CHUNK_FILTERED_DATASET_DIMS, dataset_dims, NULL);
        VRFY((filespace >= 0), "File dataspace creation succeeded");

        /* Create chunked dataset */
        chunk_dims[0] = (hsize_t)READ_ONE_CHUNK_FILTERED_DATASET_CH_NROWS;
        chunk_dims[1] = (hsize_t)READ_ONE_CHUNK_FILTERED_DATASET_CH_NCOLS;

        plist_id = H5Pcopy(dcpl_id);
        VRFY((plist_id >= 0), "DCPL copy succeeded");

        VRFY((H5Pset_chunk(plist_id, READ_ONE_CHUNK_FILTERED_DATASET_DIMS, chunk_dims) >= 0),
             "Chunk size set");

        /* Add test filter to the pipeline */
        VRFY((set_dcpl_filter(plist_id, filter_id, NULL) >= 0), "Filter set");

        dset_id = H5Dcreate2(group_id, READ_ONE_CHUNK_FILTERED_DATASET_NAME, HDF5_DATATYPE_NAME, filespace,
                             H5P_DEFAULT, plist_id, H5P_DEFAULT);
        VRFY((dset_id >= 0), "Dataset creation succeeded");

        /* Verify space allocation status */
        verify_space_alloc_status(dset_id, plist_id, DATASET_JUST_CREATED);

        VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

        VRFY((H5Dwrite(dset_id, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, H5P_DEFAULT, correct_buf) >= 0),
             "Dataset write succeeded");

        /* Verify space allocation status */
        verify_space_alloc_status(dset_id, plist_id, ALL_CHUNKS_WRITTEN);

        VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
        VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
        VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
        VRFY((H5Fclose(file_id) >= 0), "File close succeeded");
    }

    file_id = H5Fopen(filenames[0], H5F_ACC_RDONLY, fapl_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
    VRFY((group_id >= 0), "H5Gopen2 succeeded");

    dset_id = H5Dopen2(group_id, READ_ONE_CHUNK_FILTERED_DATASET_NAME, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    sel_dims[0] = (hsize_t)READ_ONE_CHUNK_FILTERED_DATASET_NROWS / (hsize_t)mpi_size;
    sel_dims[1] = (hsize_t)READ_ONE_CHUNK_FILTERED_DATASET_NCOLS;

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
    count[0]  = 1;
    count[1]  = 1;
    stride[0] = (hsize_t)READ_ONE_CHUNK_FILTERED_DATASET_CH_NROWS;
    stride[1] = (hsize_t)READ_ONE_CHUNK_FILTERED_DATASET_CH_NCOLS;
    block[0]  = sel_dims[0];
    block[1]  = sel_dims[1];
    start[0]  = ((hsize_t)mpi_rank * sel_dims[0]);
    start[1]  = 0;

    if (VERBOSE_MED) {
        HDprintf("Process %d is reading with count[ %" PRIuHSIZE ", %" PRIuHSIZE " ], stride[ %" PRIuHSIZE
                 ", %" PRIuHSIZE " ], start[ %" PRIuHSIZE ", %" PRIuHSIZE " ], block size[ %" PRIuHSIZE
                 ", %" PRIuHSIZE " ]\n",
                 mpi_rank, count[0], count[1], stride[0], stride[1], start[0], start[1], block[0], block[1]);
        HDfflush(stdout);
    }

    VRFY((H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, stride, count, block) >= 0),
         "Hyperslab selection succeeded");

    read_buf_size = flat_dims[0] * sizeof(*read_buf);

    read_buf = (C_DATATYPE *)HDcalloc(1, read_buf_size);
    VRFY((NULL != read_buf), "HDcalloc succeeded");

    VRFY((H5Dread(dset_id, HDF5_DATATYPE_NAME, memspace, filespace, dxpl_id, read_buf) >= 0),
         "Dataset read succeeded");

    global_buf = (C_DATATYPE *)HDcalloc(1, correct_buf_size);
    VRFY((NULL != global_buf), "HDcalloc succeeded");

    /* Collect each piece of data from all ranks into a global buffer on all ranks */
    recvcounts = (int *)HDcalloc(1, (size_t)mpi_size * sizeof(*recvcounts));
    VRFY((NULL != recvcounts), "HDcalloc succeeded");

    for (i = 0; i < (size_t)mpi_size; i++)
        recvcounts[i] = (int)flat_dims[0];

    displs = (int *)HDcalloc(1, (size_t)mpi_size * sizeof(*displs));
    VRFY((NULL != displs), "HDcalloc succeeded");

    for (i = 0; i < (size_t)mpi_size; i++)
        displs[i] = (int)(i * flat_dims[0]);

    VRFY((MPI_SUCCESS == MPI_Allgatherv(read_buf, (int)flat_dims[0], C_DATATYPE_MPI, global_buf, recvcounts,
                                        displs, C_DATATYPE_MPI, comm)),
         "MPI_Allgatherv succeeded");

    VRFY((0 == HDmemcmp(global_buf, correct_buf, correct_buf_size)), "Data verification succeeded");

    if (displs)
        HDfree(displs);
    if (recvcounts)
        HDfree(recvcounts);
    if (global_buf)
        HDfree(global_buf);
    if (read_buf)
        HDfree(read_buf);
    if (correct_buf)
        HDfree(correct_buf);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Sclose(memspace) >= 0), "Memory dataspace close succeeded");
    VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
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
test_read_filtered_dataset_no_overlap(const char *parent_group, H5Z_filter_t filter_id, hid_t fapl_id,
                                      hid_t dcpl_id, hid_t dxpl_id)
{
    C_DATATYPE *read_buf    = NULL;
    C_DATATYPE *correct_buf = NULL;
    C_DATATYPE *global_buf  = NULL;
    hsize_t     dataset_dims[READ_UNSHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     chunk_dims[READ_UNSHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     sel_dims[READ_UNSHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     start[READ_UNSHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     stride[READ_UNSHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     count[READ_UNSHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     block[READ_UNSHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     flat_dims[1];
    size_t      i, read_buf_size, correct_buf_size;
    hid_t       file_id = H5I_INVALID_HID, dset_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID;
    hid_t       group_id  = H5I_INVALID_HID;
    hid_t       filespace = H5I_INVALID_HID, memspace = H5I_INVALID_HID;
    int        *recvcounts = NULL;
    int        *displs     = NULL;

    if (MAINPROCESS)
        HDputs("Testing read from unshared filtered chunks");

    dataset_dims[0] = (hsize_t)READ_UNSHARED_FILTERED_CHUNKS_NROWS;
    dataset_dims[1] = (hsize_t)READ_UNSHARED_FILTERED_CHUNKS_NCOLS;

    /* Setup the buffer for writing and for comparison */
    correct_buf_size = (hsize_t)READ_UNSHARED_FILTERED_CHUNKS_NROWS *
                       (hsize_t)READ_UNSHARED_FILTERED_CHUNKS_NCOLS * sizeof(*correct_buf);

    correct_buf = (C_DATATYPE *)HDcalloc(1, correct_buf_size);
    VRFY((NULL != correct_buf), "HDcalloc succeeded");

    for (i = 0; i < correct_buf_size / sizeof(*correct_buf); i++)
        correct_buf[i] = (C_DATATYPE)((i % (dataset_dims[0] / (hsize_t)mpi_size * dataset_dims[1])) +
                                      (i / (dataset_dims[0] / (hsize_t)mpi_size * dataset_dims[1])));

    if (MAINPROCESS) {
        plist_id = H5Pcreate(H5P_FILE_ACCESS);
        VRFY((plist_id >= 0), "FAPL creation succeeded");

        VRFY((H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0),
             "Set libver bounds succeeded");

        file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, plist_id);
        VRFY((file_id >= 0), "Test file open succeeded");

        VRFY((H5Pclose(plist_id) >= 0), "FAPL close succeeded");

        group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
        VRFY((group_id >= 0), "H5Gopen2 succeeded");

        /* Create the dataspace for the dataset */
        filespace = H5Screate_simple(READ_UNSHARED_FILTERED_CHUNKS_DATASET_DIMS, dataset_dims, NULL);
        VRFY((filespace >= 0), "File dataspace creation succeeded");

        /* Create chunked dataset */
        chunk_dims[0] = (hsize_t)READ_UNSHARED_FILTERED_CHUNKS_CH_NROWS;
        chunk_dims[1] = (hsize_t)READ_UNSHARED_FILTERED_CHUNKS_CH_NCOLS;

        plist_id = H5Pcopy(dcpl_id);
        VRFY((plist_id >= 0), "DCPL copy succeeded");

        VRFY((H5Pset_chunk(plist_id, READ_UNSHARED_FILTERED_CHUNKS_DATASET_DIMS, chunk_dims) >= 0),
             "Chunk size set");

        /* Add test filter to the pipeline */
        VRFY((set_dcpl_filter(plist_id, filter_id, NULL) >= 0), "Filter set");

        dset_id = H5Dcreate2(group_id, READ_UNSHARED_FILTERED_CHUNKS_DATASET_NAME, HDF5_DATATYPE_NAME,
                             filespace, H5P_DEFAULT, plist_id, H5P_DEFAULT);
        VRFY((dset_id >= 0), "Dataset creation succeeded");

        /* Verify space allocation status */
        verify_space_alloc_status(dset_id, plist_id, DATASET_JUST_CREATED);

        VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

        VRFY((H5Dwrite(dset_id, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, H5P_DEFAULT, correct_buf) >= 0),
             "Dataset write succeeded");

        /* Verify space allocation status */
        verify_space_alloc_status(dset_id, plist_id, ALL_CHUNKS_WRITTEN);

        VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
        VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
        VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
        VRFY((H5Fclose(file_id) >= 0), "File close succeeded");
    }

    file_id = H5Fopen(filenames[0], H5F_ACC_RDONLY, fapl_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
    VRFY((group_id >= 0), "H5Gopen2 succeeded");

    dset_id = H5Dopen2(group_id, READ_UNSHARED_FILTERED_CHUNKS_DATASET_NAME, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    sel_dims[0] = (hsize_t)READ_UNSHARED_FILTERED_CHUNKS_CH_NROWS;
    sel_dims[1] = (hsize_t)READ_UNSHARED_FILTERED_CHUNKS_NCOLS;

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
    count[1] = (hsize_t)READ_UNSHARED_FILTERED_CHUNKS_NCOLS / (hsize_t)READ_UNSHARED_FILTERED_CHUNKS_CH_NCOLS;
    stride[0] = (hsize_t)READ_UNSHARED_FILTERED_CHUNKS_CH_NROWS;
    stride[1] = (hsize_t)READ_UNSHARED_FILTERED_CHUNKS_CH_NCOLS;
    block[0]  = (hsize_t)READ_UNSHARED_FILTERED_CHUNKS_CH_NROWS;
    block[1]  = (hsize_t)READ_UNSHARED_FILTERED_CHUNKS_CH_NCOLS;
    start[0]  = ((hsize_t)mpi_rank * (hsize_t)READ_UNSHARED_FILTERED_CHUNKS_CH_NROWS * count[0]);
    start[1]  = 0;

    if (VERBOSE_MED) {
        HDprintf("Process %d is reading with count[ %" PRIuHSIZE ", %" PRIuHSIZE " ], stride[ %" PRIuHSIZE
                 ", %" PRIuHSIZE " ], start[ %" PRIuHSIZE ", %" PRIuHSIZE " ], block size[ %" PRIuHSIZE
                 ", %" PRIuHSIZE " ]\n",
                 mpi_rank, count[0], count[1], stride[0], stride[1], start[0], start[1], block[0], block[1]);
        HDfflush(stdout);
    }

    VRFY((H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, stride, count, block) >= 0),
         "Hyperslab selection succeeded");

    read_buf_size = flat_dims[0] * sizeof(*read_buf);

    read_buf = (C_DATATYPE *)HDcalloc(1, read_buf_size);
    VRFY((NULL != read_buf), "HDcalloc succeeded");

    VRFY((H5Dread(dset_id, HDF5_DATATYPE_NAME, memspace, filespace, dxpl_id, read_buf) >= 0),
         "Dataset read succeeded");

    global_buf = (C_DATATYPE *)HDcalloc(1, correct_buf_size);
    VRFY((NULL != global_buf), "HDcalloc succeeded");

    /* Collect each piece of data from all ranks into a global buffer on all ranks */
    recvcounts = (int *)HDcalloc(1, (size_t)mpi_size * sizeof(*recvcounts));
    VRFY((NULL != recvcounts), "HDcalloc succeeded");

    for (i = 0; i < (size_t)mpi_size; i++)
        recvcounts[i] = (int)flat_dims[0];

    displs = (int *)HDcalloc(1, (size_t)mpi_size * sizeof(*displs));
    VRFY((NULL != displs), "HDcalloc succeeded");

    for (i = 0; i < (size_t)mpi_size; i++)
        displs[i] = (int)(i * flat_dims[0]);

    VRFY((MPI_SUCCESS == MPI_Allgatherv(read_buf, (int)flat_dims[0], C_DATATYPE_MPI, global_buf, recvcounts,
                                        displs, C_DATATYPE_MPI, comm)),
         "MPI_Allgatherv succeeded");

    VRFY((0 == HDmemcmp(global_buf, correct_buf, correct_buf_size)), "Data verification succeeded");

    if (displs)
        HDfree(displs);
    if (recvcounts)
        HDfree(recvcounts);
    if (global_buf)
        HDfree(global_buf);
    if (read_buf)
        HDfree(read_buf);
    if (correct_buf)
        HDfree(correct_buf);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Sclose(memspace) >= 0), "Memory dataspace close succeeded");
    VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
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
test_read_filtered_dataset_overlap(const char *parent_group, H5Z_filter_t filter_id, hid_t fapl_id,
                                   hid_t dcpl_id, hid_t dxpl_id)
{
    C_DATATYPE *read_buf    = NULL;
    C_DATATYPE *correct_buf = NULL;
    C_DATATYPE *global_buf  = NULL;
    hsize_t     dataset_dims[READ_SHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     chunk_dims[READ_SHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     sel_dims[READ_SHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     start[READ_SHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     stride[READ_SHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     count[READ_SHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     block[READ_SHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     flat_dims[1];
    size_t      i, read_buf_size, correct_buf_size;
    hid_t       file_id = H5I_INVALID_HID, dset_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID;
    hid_t       group_id  = H5I_INVALID_HID;
    hid_t       filespace = H5I_INVALID_HID, memspace = H5I_INVALID_HID;
    int        *recvcounts = NULL;
    int        *displs     = NULL;

    if (MAINPROCESS)
        HDputs("Testing read from shared filtered chunks");

    dataset_dims[0] = (hsize_t)READ_SHARED_FILTERED_CHUNKS_NROWS;
    dataset_dims[1] = (hsize_t)READ_SHARED_FILTERED_CHUNKS_NCOLS;

    /* Setup the buffer for writing and for comparison */
    correct_buf_size = dataset_dims[0] * dataset_dims[1] * sizeof(*correct_buf);

    correct_buf = (C_DATATYPE *)HDcalloc(1, correct_buf_size);
    VRFY((NULL != correct_buf), "HDcalloc succeeded");

    for (i = 0; i < correct_buf_size / sizeof(*correct_buf); i++)
        correct_buf[i] =
            (C_DATATYPE)((dataset_dims[1] * (i / ((hsize_t)mpi_size * dataset_dims[1]))) +
                         (i % dataset_dims[1]) +
                         (((i % ((hsize_t)mpi_size * dataset_dims[1])) / dataset_dims[1]) % dataset_dims[1]));

    if (MAINPROCESS) {
        plist_id = H5Pcreate(H5P_FILE_ACCESS);
        VRFY((plist_id >= 0), "FAPL creation succeeded");

        VRFY((H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0),
             "Set libver bounds succeeded");

        file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, plist_id);
        VRFY((file_id >= 0), "Test file open succeeded");

        VRFY((H5Pclose(plist_id) >= 0), "FAPL close succeeded");

        group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
        VRFY((group_id >= 0), "H5Gopen2 succeeded");

        /* Create the dataspace for the dataset */
        filespace = H5Screate_simple(READ_SHARED_FILTERED_CHUNKS_DATASET_DIMS, dataset_dims, NULL);
        VRFY((filespace >= 0), "File dataspace creation succeeded");

        /* Create chunked dataset */
        chunk_dims[0] = (hsize_t)READ_SHARED_FILTERED_CHUNKS_CH_NROWS;
        chunk_dims[1] = (hsize_t)READ_SHARED_FILTERED_CHUNKS_CH_NCOLS;

        plist_id = H5Pcopy(dcpl_id);
        VRFY((plist_id >= 0), "DCPL copy succeeded");

        VRFY((H5Pset_chunk(plist_id, READ_SHARED_FILTERED_CHUNKS_DATASET_DIMS, chunk_dims) >= 0),
             "Chunk size set");

        /* Add test filter to the pipeline */
        VRFY((set_dcpl_filter(plist_id, filter_id, NULL) >= 0), "Filter set");

        dset_id = H5Dcreate2(group_id, READ_SHARED_FILTERED_CHUNKS_DATASET_NAME, HDF5_DATATYPE_NAME,
                             filespace, H5P_DEFAULT, plist_id, H5P_DEFAULT);
        VRFY((dset_id >= 0), "Dataset creation succeeded");

        /* Verify space allocation status */
        verify_space_alloc_status(dset_id, plist_id, DATASET_JUST_CREATED);

        VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

        VRFY((H5Dwrite(dset_id, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, H5P_DEFAULT, correct_buf) >= 0),
             "Dataset write succeeded");

        /* Verify space allocation status */
        verify_space_alloc_status(dset_id, plist_id, ALL_CHUNKS_WRITTEN);

        VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
        VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
        VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
        VRFY((H5Fclose(file_id) >= 0), "File close succeeded");
    }

    file_id = H5Fopen(filenames[0], H5F_ACC_RDONLY, fapl_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
    VRFY((group_id >= 0), "H5Gopen2 succeeded");

    dset_id = H5Dopen2(group_id, READ_SHARED_FILTERED_CHUNKS_DATASET_NAME, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    sel_dims[0] = (hsize_t)DIM0_SCALE_FACTOR;
    sel_dims[1] = (hsize_t)READ_SHARED_FILTERED_CHUNKS_CH_NCOLS * (hsize_t)DIM1_SCALE_FACTOR;

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
    count[0]  = (hsize_t)READ_SHARED_FILTERED_CHUNKS_NROWS / (hsize_t)READ_SHARED_FILTERED_CHUNKS_CH_NROWS;
    count[1]  = (hsize_t)READ_SHARED_FILTERED_CHUNKS_NCOLS / (hsize_t)READ_SHARED_FILTERED_CHUNKS_CH_NCOLS;
    stride[0] = (hsize_t)READ_SHARED_FILTERED_CHUNKS_CH_NROWS;
    stride[1] = (hsize_t)READ_SHARED_FILTERED_CHUNKS_CH_NCOLS;
    block[0]  = (hsize_t)READ_SHARED_FILTERED_CHUNKS_CH_NROWS / (hsize_t)mpi_size;
    block[1]  = (hsize_t)READ_SHARED_FILTERED_CHUNKS_CH_NCOLS;
    start[0]  = (hsize_t)mpi_rank * block[0];
    start[1]  = 0;

    if (VERBOSE_MED) {
        HDprintf("Process %d is reading with count[ %" PRIuHSIZE ", %" PRIuHSIZE " ], stride[ %" PRIuHSIZE
                 ", %" PRIuHSIZE " ], start[ %" PRIuHSIZE ", %" PRIuHSIZE " ], block size[ %" PRIuHSIZE
                 ", %" PRIuHSIZE " ]\n",
                 mpi_rank, count[0], count[1], stride[0], stride[1], start[0], start[1], block[0], block[1]);
        HDfflush(stdout);
    }

    VRFY((H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, stride, count, block) >= 0),
         "Hyperslab selection succeeded");

    read_buf_size = flat_dims[0] * sizeof(*read_buf);

    read_buf = (C_DATATYPE *)HDcalloc(1, read_buf_size);
    VRFY((NULL != read_buf), "HDcalloc succeeded");

    VRFY((H5Dread(dset_id, HDF5_DATATYPE_NAME, memspace, filespace, dxpl_id, read_buf) >= 0),
         "Dataset read succeeded");

    global_buf = (C_DATATYPE *)HDcalloc(1, correct_buf_size);
    VRFY((NULL != global_buf), "HDcalloc succeeded");

    /*
     * Since these chunks are shared, run multiple rounds of MPI_Allgatherv
     * to collect all of the pieces into their appropriate locations. The
     * number of times MPI_Allgatherv is run should be equal to the number
     * of chunks in the first dimension of the dataset.
     */
    {
        size_t loop_count       = count[0];
        size_t total_recvcounts = 0;

        recvcounts = (int *)HDcalloc(1, (size_t)mpi_size * sizeof(*recvcounts));
        VRFY((NULL != recvcounts), "HDcalloc succeeded");

        displs = (int *)HDcalloc(1, (size_t)mpi_size * sizeof(*displs));
        VRFY((NULL != displs), "HDcalloc succeeded");

        for (i = 0; i < (size_t)mpi_size; i++) {
            recvcounts[i] = (int)dataset_dims[1];
            total_recvcounts += (size_t)recvcounts[i];
        }

        for (i = 0; i < (size_t)mpi_size; i++)
            displs[i] = (int)(i * dataset_dims[1]);

        for (; loop_count; loop_count--) {
            VRFY((MPI_SUCCESS == MPI_Allgatherv(&read_buf[(count[0] - loop_count) * dataset_dims[1]],
                                                recvcounts[mpi_rank], C_DATATYPE_MPI,
                                                &global_buf[(count[0] - loop_count) * total_recvcounts],
                                                recvcounts, displs, C_DATATYPE_MPI, comm)),
                 "MPI_Allgatherv succeeded");
        }
    }

    VRFY((0 == HDmemcmp(global_buf, correct_buf, correct_buf_size)), "Data verification succeeded");

    if (displs)
        HDfree(displs);
    if (recvcounts)
        HDfree(recvcounts);
    if (global_buf)
        HDfree(global_buf);
    if (read_buf)
        HDfree(read_buf);
    if (correct_buf)
        HDfree(correct_buf);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Sclose(memspace) >= 0), "Memory dataspace close succeeded");
    VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
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
test_read_filtered_dataset_single_no_selection(const char *parent_group, H5Z_filter_t filter_id,
                                               hid_t fapl_id, hid_t dcpl_id, hid_t dxpl_id)
{
    C_DATATYPE *read_buf    = NULL;
    C_DATATYPE *correct_buf = NULL;
    C_DATATYPE *global_buf  = NULL;
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
    hid_t       file_id = H5I_INVALID_HID, dset_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID;
    hid_t       group_id  = H5I_INVALID_HID;
    hid_t       filespace = H5I_INVALID_HID, memspace = H5I_INVALID_HID;
    int        *recvcounts = NULL;
    int        *displs     = NULL;

    if (MAINPROCESS)
        HDputs("Testing read from filtered chunks with a single process having no selection");

    dataset_dims[0] = (hsize_t)READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_NROWS;
    dataset_dims[1] = (hsize_t)READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_NCOLS;

    /* Setup the buffer for writing and for comparison */
    correct_buf_size = dataset_dims[0] * dataset_dims[1] * sizeof(*correct_buf);

    correct_buf = (C_DATATYPE *)HDcalloc(1, correct_buf_size);
    VRFY((NULL != correct_buf), "HDcalloc succeeded");

    for (i = 0; i < correct_buf_size / sizeof(*correct_buf); i++)
        correct_buf[i] = (C_DATATYPE)((i % (dataset_dims[0] / (hsize_t)mpi_size * dataset_dims[1])) +
                                      (i / (dataset_dims[0] / (hsize_t)mpi_size * dataset_dims[1])));

    /* Compute the correct offset into the buffer for the process having no selection and clear it */
    segment_length = dataset_dims[0] * dataset_dims[1] / (hsize_t)mpi_size;
    HDmemset(correct_buf + ((size_t)READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_NO_SELECT_PROC * segment_length),
             0, segment_length * sizeof(*correct_buf));

    if (MAINPROCESS) {
        plist_id = H5Pcreate(H5P_FILE_ACCESS);
        VRFY((plist_id >= 0), "FAPL creation succeeded");

        VRFY((H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0),
             "Set libver bounds succeeded");

        file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, plist_id);
        VRFY((file_id >= 0), "Test file open succeeded");

        VRFY((H5Pclose(plist_id) >= 0), "FAPL close succeeded");

        group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
        VRFY((group_id >= 0), "H5Gopen2 succeeded");

        /* Create the dataspace for the dataset */
        filespace =
            H5Screate_simple(READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS, dataset_dims, NULL);
        VRFY((filespace >= 0), "File dataspace creation succeeded");

        /* Create chunked dataset */
        chunk_dims[0] = (hsize_t)READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_CH_NROWS;
        chunk_dims[1] = (hsize_t)READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_CH_NCOLS;

        plist_id = H5Pcopy(dcpl_id);
        VRFY((plist_id >= 0), "DCPL copy succeeded");

        VRFY((H5Pset_chunk(plist_id, READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS, chunk_dims) >= 0),
             "Chunk size set");

        /* Add test filter to the pipeline */
        VRFY((set_dcpl_filter(plist_id, filter_id, NULL) >= 0), "Filter set");

        dset_id = H5Dcreate2(group_id, READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_DATASET_NAME,
                             HDF5_DATATYPE_NAME, filespace, H5P_DEFAULT, plist_id, H5P_DEFAULT);
        VRFY((dset_id >= 0), "Dataset creation succeeded");

        /* Verify space allocation status */
        verify_space_alloc_status(dset_id, plist_id, DATASET_JUST_CREATED);

        VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

        VRFY((H5Dwrite(dset_id, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, H5P_DEFAULT, correct_buf) >= 0),
             "Dataset write succeeded");

        /* Verify space allocation status */
        verify_space_alloc_status(dset_id, plist_id, ALL_CHUNKS_WRITTEN);

        VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
        VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
        VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
        VRFY((H5Fclose(file_id) >= 0), "File close succeeded");
    }

    file_id = H5Fopen(filenames[0], H5F_ACC_RDONLY, fapl_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
    VRFY((group_id >= 0), "H5Gopen2 succeeded");

    dset_id = H5Dopen2(group_id, READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_DATASET_NAME, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    sel_dims[0] = (hsize_t)READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_CH_NROWS;
    sel_dims[1] = (hsize_t)READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_NCOLS;

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
    count[1] = (hsize_t)READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_NCOLS /
               (hsize_t)READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_CH_NCOLS;
    stride[0] = (hsize_t)READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_CH_NROWS;
    stride[1] = (hsize_t)READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_CH_NCOLS;
    block[0]  = (hsize_t)READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_CH_NROWS;
    block[1]  = (hsize_t)READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_CH_NCOLS;
    start[0]  = (hsize_t)mpi_rank * (hsize_t)READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_CH_NROWS * count[0];
    start[1]  = 0;

    if (VERBOSE_MED) {
        HDprintf("Process %d is reading with count[ %" PRIuHSIZE ", %" PRIuHSIZE " ], stride[ %" PRIuHSIZE
                 ", %" PRIuHSIZE " ], start[ %" PRIuHSIZE ", %" PRIuHSIZE " ], block size[ %" PRIuHSIZE
                 ", %" PRIuHSIZE " ]\n",
                 mpi_rank, count[0], count[1], stride[0], stride[1], start[0], start[1], block[0], block[1]);
        HDfflush(stdout);
    }

    if (mpi_rank == READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_NO_SELECT_PROC)
        VRFY((H5Sselect_none(filespace) >= 0), "Select none succeeded");
    else
        VRFY((H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, stride, count, block) >= 0),
             "Hyperslab selection succeeded");

    read_buf_size = flat_dims[0] * sizeof(*read_buf);

    read_buf = (C_DATATYPE *)HDcalloc(1, read_buf_size);
    VRFY((NULL != read_buf), "HDcalloc succeeded");

    if (mpi_rank == READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_NO_SELECT_PROC) {
        VRFY((H5Dread(dset_id, HDF5_DATATYPE_NAME, memspace, filespace, dxpl_id, NULL) >= 0),
             "Dataset read succeeded");
    }
    else {
        VRFY((H5Dread(dset_id, HDF5_DATATYPE_NAME, memspace, filespace, dxpl_id, read_buf) >= 0),
             "Dataset read succeeded");
    }

    global_buf = (C_DATATYPE *)HDcalloc(1, correct_buf_size);
    VRFY((NULL != global_buf), "HDcalloc succeeded");

    /* Collect each piece of data from all ranks into a global buffer on all ranks */
    recvcounts = (int *)HDcalloc(1, (size_t)mpi_size * sizeof(*recvcounts));
    VRFY((NULL != recvcounts), "HDcalloc succeeded");

    for (i = 0; i < (size_t)mpi_size; i++)
        recvcounts[i] = (int)(READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_CH_NROWS *
                              READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_NCOLS);
    recvcounts[READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_NO_SELECT_PROC] = 0;

    displs = (int *)HDcalloc(1, (size_t)mpi_size * sizeof(*displs));
    VRFY((NULL != displs), "HDcalloc succeeded");

    for (i = 0; i < (size_t)mpi_size; i++)
        displs[i] = (int)(i * (size_t)(READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_CH_NROWS *
                                       READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_NCOLS));

    if (mpi_rank == READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_NO_SELECT_PROC)
        VRFY((MPI_SUCCESS == MPI_Allgatherv(read_buf, 0, C_DATATYPE_MPI, global_buf, recvcounts, displs,
                                            C_DATATYPE_MPI, comm)),
             "MPI_Allgatherv succeeded");
    else
        VRFY((MPI_SUCCESS == MPI_Allgatherv(read_buf, (int)flat_dims[0], C_DATATYPE_MPI, global_buf,
                                            recvcounts, displs, C_DATATYPE_MPI, comm)),
             "MPI_Allgatherv succeeded");

    VRFY((0 == HDmemcmp(global_buf, correct_buf, correct_buf_size)), "Data verification succeeded");

    if (displs)
        HDfree(displs);
    if (recvcounts)
        HDfree(recvcounts);
    if (global_buf)
        HDfree(global_buf);
    if (read_buf)
        HDfree(read_buf);
    if (correct_buf)
        HDfree(correct_buf);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Sclose(memspace) >= 0), "Memory dataspace close succeeded");
    VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
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
test_read_filtered_dataset_all_no_selection(const char *parent_group, H5Z_filter_t filter_id, hid_t fapl_id,
                                            hid_t dcpl_id, hid_t dxpl_id)
{
    C_DATATYPE *read_buf    = NULL;
    C_DATATYPE *correct_buf = NULL;
    hsize_t     dataset_dims[READ_ALL_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     chunk_dims[READ_ALL_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     sel_dims[READ_ALL_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    size_t      read_buf_size, correct_buf_size;
    hid_t       file_id = H5I_INVALID_HID, dset_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID;
    hid_t       group_id  = H5I_INVALID_HID;
    hid_t       filespace = H5I_INVALID_HID, memspace = H5I_INVALID_HID;

    if (MAINPROCESS)
        HDputs("Testing read from filtered chunks with all processes having no selection");

    dataset_dims[0] = (hsize_t)READ_ALL_NO_SELECTION_FILTERED_CHUNKS_NROWS;
    dataset_dims[1] = (hsize_t)READ_ALL_NO_SELECTION_FILTERED_CHUNKS_NCOLS;

    /* Setup the buffer for writing and for comparison */
    correct_buf_size = dataset_dims[0] * dataset_dims[1] * sizeof(*correct_buf);

    correct_buf = (C_DATATYPE *)HDcalloc(1, correct_buf_size);
    VRFY((NULL != correct_buf), "HDcalloc succeeded");

    if (MAINPROCESS) {
        plist_id = H5Pcreate(H5P_FILE_ACCESS);
        VRFY((plist_id >= 0), "FAPL creation succeeded");

        VRFY((H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0),
             "Set libver bounds succeeded");

        file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, plist_id);
        VRFY((file_id >= 0), "Test file open succeeded");

        VRFY((H5Pclose(plist_id) >= 0), "FAPL close succeeded");

        group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
        VRFY((group_id >= 0), "H5Gopen2 succeeded");

        /* Create the dataspace for the dataset */
        filespace = H5Screate_simple(READ_ALL_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS, dataset_dims, NULL);
        VRFY((filespace >= 0), "File dataspace creation succeeded");

        /* Create chunked dataset */
        chunk_dims[0] = (hsize_t)READ_ALL_NO_SELECTION_FILTERED_CHUNKS_CH_NROWS;
        chunk_dims[1] = (hsize_t)READ_ALL_NO_SELECTION_FILTERED_CHUNKS_CH_NCOLS;

        plist_id = H5Pcopy(dcpl_id);
        VRFY((plist_id >= 0), "DCPL copy succeeded");

        VRFY((H5Pset_chunk(plist_id, READ_ALL_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS, chunk_dims) >= 0),
             "Chunk size set");

        /* Add test filter to the pipeline */
        VRFY((set_dcpl_filter(plist_id, filter_id, NULL) >= 0), "Filter set");

        dset_id = H5Dcreate2(group_id, READ_ALL_NO_SELECTION_FILTERED_CHUNKS_DATASET_NAME, HDF5_DATATYPE_NAME,
                             filespace, H5P_DEFAULT, plist_id, H5P_DEFAULT);
        VRFY((dset_id >= 0), "Dataset creation succeeded");

        /* Verify space allocation status */
        verify_space_alloc_status(dset_id, plist_id, DATASET_JUST_CREATED);

        VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

        VRFY((H5Dwrite(dset_id, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, H5P_DEFAULT, correct_buf) >= 0),
             "Dataset write succeeded");

        /* Verify space allocation status */
        verify_space_alloc_status(dset_id, plist_id, ALL_CHUNKS_WRITTEN);

        VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
        VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
        VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
        VRFY((H5Fclose(file_id) >= 0), "File close succeeded");
    }

    file_id = H5Fopen(filenames[0], H5F_ACC_RDONLY, fapl_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
    VRFY((group_id >= 0), "H5Gopen2 succeeded");

    dset_id = H5Dopen2(group_id, READ_ALL_NO_SELECTION_FILTERED_CHUNKS_DATASET_NAME, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    sel_dims[0] = sel_dims[1] = 0;

    memspace = H5Screate_simple(READ_ALL_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS, sel_dims, NULL);
    VRFY((memspace >= 0), "Memory dataspace creation succeeded");

    /* Select hyperslab in the file */
    filespace = H5Dget_space(dset_id);
    VRFY((filespace >= 0), "File dataspace retrieval succeeded");

    VRFY((H5Sselect_none(filespace) >= 0), "Select none succeeded");

    read_buf_size = dataset_dims[0] * dataset_dims[1] * sizeof(*read_buf);

    read_buf = (C_DATATYPE *)HDcalloc(1, read_buf_size);
    VRFY((NULL != read_buf), "HDcalloc succeeded");

    VRFY((H5Dread(dset_id, HDF5_DATATYPE_NAME, memspace, filespace, dxpl_id, read_buf) >= 0),
         "Dataset read succeeded");

    VRFY((0 == HDmemcmp(read_buf, correct_buf, correct_buf_size)), "Data verification succeeded");

    if (read_buf)
        HDfree(read_buf);
    if (correct_buf)
        HDfree(correct_buf);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Sclose(memspace) >= 0), "Memory dataspace close succeeded");
    VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
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
test_read_filtered_dataset_point_selection(const char *parent_group, H5Z_filter_t filter_id, hid_t fapl_id,
                                           hid_t dcpl_id, hid_t dxpl_id)
{
    C_DATATYPE *correct_buf = NULL;
    C_DATATYPE *read_buf    = NULL;
    C_DATATYPE *global_buf  = NULL;
    hsize_t    *coords      = NULL;
    hsize_t     dataset_dims[READ_POINT_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     chunk_dims[READ_POINT_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     sel_dims[READ_POINT_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     flat_dims[1];
    size_t      i, j, read_buf_size, correct_buf_size;
    size_t      num_points;
    hid_t       file_id = H5I_INVALID_HID, dset_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID;
    hid_t       group_id  = H5I_INVALID_HID;
    hid_t       filespace = H5I_INVALID_HID, memspace = H5I_INVALID_HID;
    int        *recvcounts = NULL;
    int        *displs     = NULL;

    if (MAINPROCESS)
        HDputs("Testing read from filtered chunks with point selection");

    dataset_dims[0] = (hsize_t)READ_POINT_SELECTION_FILTERED_CHUNKS_NROWS;
    dataset_dims[1] = (hsize_t)READ_POINT_SELECTION_FILTERED_CHUNKS_NCOLS;

    /* Setup the buffer for writing and for comparison */
    correct_buf_size = dataset_dims[0] * dataset_dims[1] * sizeof(*correct_buf);

    correct_buf = (C_DATATYPE *)HDcalloc(1, correct_buf_size);
    VRFY((NULL != correct_buf), "HDcalloc succeeded");

    for (i = 0; i < correct_buf_size / sizeof(*correct_buf); i++)
        correct_buf[i] =
            (C_DATATYPE)((dataset_dims[1] * (i / ((hsize_t)mpi_size * dataset_dims[1]))) +
                         (i % dataset_dims[1]) +
                         (((i % ((hsize_t)mpi_size * dataset_dims[1])) / dataset_dims[1]) % dataset_dims[1]));

    if (MAINPROCESS) {
        plist_id = H5Pcreate(H5P_FILE_ACCESS);
        VRFY((plist_id >= 0), "FAPL creation succeeded");

        VRFY((H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0),
             "Set libver bounds succeeded");

        file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, plist_id);
        VRFY((file_id >= 0), "Test file open succeeded");

        VRFY((H5Pclose(plist_id) >= 0), "FAPL close succeeded");

        group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
        VRFY((group_id >= 0), "H5Gopen2 succeeded");

        /* Create the dataspace for the dataset */
        filespace = H5Screate_simple(READ_POINT_SELECTION_FILTERED_CHUNKS_DATASET_DIMS, dataset_dims, NULL);
        VRFY((filespace >= 0), "File dataspace creation succeeded");

        /* Create chunked dataset */
        chunk_dims[0] = (hsize_t)READ_POINT_SELECTION_FILTERED_CHUNKS_CH_NROWS;
        chunk_dims[1] = (hsize_t)READ_POINT_SELECTION_FILTERED_CHUNKS_CH_NCOLS;

        plist_id = H5Pcopy(dcpl_id);
        VRFY((plist_id >= 0), "DCPL copy succeeded");

        VRFY((H5Pset_chunk(plist_id, READ_POINT_SELECTION_FILTERED_CHUNKS_DATASET_DIMS, chunk_dims) >= 0),
             "Chunk size set");

        /* Add test filter to the pipeline */
        VRFY((set_dcpl_filter(plist_id, filter_id, NULL) >= 0), "Filter set");

        dset_id = H5Dcreate2(group_id, READ_POINT_SELECTION_FILTERED_CHUNKS_DATASET_NAME, HDF5_DATATYPE_NAME,
                             filespace, H5P_DEFAULT, plist_id, H5P_DEFAULT);
        VRFY((dset_id >= 0), "Dataset creation succeeded");

        /* Verify space allocation status */
        verify_space_alloc_status(dset_id, plist_id, DATASET_JUST_CREATED);

        VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

        VRFY((H5Dwrite(dset_id, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, H5P_DEFAULT, correct_buf) >= 0),
             "Dataset write succeeded");

        /* Verify space allocation status */
        verify_space_alloc_status(dset_id, plist_id, ALL_CHUNKS_WRITTEN);

        VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
        VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
        VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
        VRFY((H5Fclose(file_id) >= 0), "File close succeeded");
    }

    file_id = H5Fopen(filenames[0], H5F_ACC_RDONLY, fapl_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
    VRFY((group_id >= 0), "H5Gopen2 succeeded");

    dset_id = H5Dopen2(group_id, READ_POINT_SELECTION_FILTERED_CHUNKS_DATASET_NAME, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    sel_dims[0] = (hsize_t)READ_POINT_SELECTION_FILTERED_CHUNKS_NROWS / (hsize_t)mpi_size;
    sel_dims[1] = (hsize_t)READ_POINT_SELECTION_FILTERED_CHUNKS_NCOLS;

    /* Setup one-dimensional memory dataspace for reading the dataset data into a contiguous buffer */
    flat_dims[0] = sel_dims[0] * sel_dims[1];

    memspace = H5Screate_simple(1, flat_dims, NULL);
    VRFY((memspace >= 0), "Memory dataspace creation succeeded");

    /* Set up point selection */
    filespace = H5Dget_space(dset_id);
    VRFY((filespace >= 0), "File dataspace retrieval succeeded");

    num_points = (hsize_t)READ_POINT_SELECTION_FILTERED_CHUNKS_NROWS *
                 (hsize_t)READ_POINT_SELECTION_FILTERED_CHUNKS_NCOLS / (hsize_t)mpi_size;
    coords = (hsize_t *)HDcalloc(1, 2 * num_points * sizeof(*coords));
    VRFY((NULL != coords), "Coords HDcalloc succeeded");

    for (i = 0; i < num_points; i++)
        for (j = 0; j < READ_POINT_SELECTION_FILTERED_CHUNKS_DATASET_DIMS; j++)
            coords[(i * READ_POINT_SELECTION_FILTERED_CHUNKS_DATASET_DIMS) + j] =
                (j > 0) ? (i % (hsize_t)READ_POINT_SELECTION_FILTERED_CHUNKS_NCOLS)
                        : ((hsize_t)mpi_rank +
                           ((hsize_t)mpi_size * (i / (hsize_t)READ_POINT_SELECTION_FILTERED_CHUNKS_NCOLS)));

    VRFY((H5Sselect_elements(filespace, H5S_SELECT_SET, (hsize_t)num_points, (const hsize_t *)coords) >= 0),
         "Point selection succeeded");

    read_buf_size = flat_dims[0] * sizeof(*read_buf);

    read_buf = (C_DATATYPE *)HDcalloc(1, read_buf_size);
    VRFY((NULL != read_buf), "HDcalloc succeeded");

    VRFY((H5Dread(dset_id, HDF5_DATATYPE_NAME, memspace, filespace, dxpl_id, read_buf) >= 0),
         "Dataset read succeeded");

    global_buf = (C_DATATYPE *)HDcalloc(1, correct_buf_size);
    VRFY((NULL != global_buf), "HDcalloc succeeded");

    /*
     * Since these chunks are shared, run multiple rounds of MPI_Allgatherv
     * to collect all of the pieces into their appropriate locations. The
     * number of times MPI_Allgatherv is run should be equal to the number
     * of chunks in the first dimension of the dataset.
     */
    {
        size_t original_loop_count = dataset_dims[0] / (hsize_t)mpi_size;
        size_t cur_loop_count      = original_loop_count;
        size_t total_recvcounts    = 0;

        recvcounts = (int *)HDcalloc(1, (size_t)mpi_size * sizeof(*recvcounts));
        VRFY((NULL != recvcounts), "HDcalloc succeeded");

        displs = (int *)HDcalloc(1, (size_t)mpi_size * sizeof(*displs));
        VRFY((NULL != displs), "HDcalloc succeeded");

        for (i = 0; i < (size_t)mpi_size; i++) {
            recvcounts[i] = (int)dataset_dims[1];
            total_recvcounts += (size_t)recvcounts[i];
        }

        for (i = 0; i < (size_t)mpi_size; i++)
            displs[i] = (int)(i * dataset_dims[1]);

        for (; cur_loop_count; cur_loop_count--) {
            VRFY((MPI_SUCCESS ==
                  MPI_Allgatherv(&read_buf[(original_loop_count - cur_loop_count) * dataset_dims[1]],
                                 recvcounts[mpi_rank], C_DATATYPE_MPI,
                                 &global_buf[(original_loop_count - cur_loop_count) * total_recvcounts],
                                 recvcounts, displs, C_DATATYPE_MPI, comm)),
                 "MPI_Allgatherv succeeded");
        }
    }

    VRFY((0 == HDmemcmp(global_buf, correct_buf, correct_buf_size)), "Data verification succeeded");

    if (displs)
        HDfree(displs);
    if (recvcounts)
        HDfree(recvcounts);
    if (global_buf)
        HDfree(global_buf);
    if (read_buf)
        HDfree(read_buf);
    if (correct_buf)
        HDfree(correct_buf);

    HDfree(coords);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Sclose(memspace) >= 0), "Memory dataspace close succeeded");
    VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
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
test_read_filtered_dataset_interleaved_read(const char *parent_group, H5Z_filter_t filter_id, hid_t fapl_id,
                                            hid_t dcpl_id, hid_t dxpl_id)
{
    C_DATATYPE *read_buf    = NULL;
    C_DATATYPE *correct_buf = NULL;
    C_DATATYPE *global_buf  = NULL;
    hsize_t     dataset_dims[INTERLEAVED_READ_FILTERED_DATASET_DIMS];
    hsize_t     chunk_dims[INTERLEAVED_READ_FILTERED_DATASET_DIMS];
    hsize_t     sel_dims[INTERLEAVED_READ_FILTERED_DATASET_DIMS];
    hsize_t     start[INTERLEAVED_READ_FILTERED_DATASET_DIMS];
    hsize_t     stride[INTERLEAVED_READ_FILTERED_DATASET_DIMS];
    hsize_t     count[INTERLEAVED_READ_FILTERED_DATASET_DIMS];
    hsize_t     block[INTERLEAVED_READ_FILTERED_DATASET_DIMS];
    hsize_t     flat_dims[1];
    size_t      i, read_buf_size, correct_buf_size;
    hid_t       file_id = H5I_INVALID_HID, dset_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID;
    hid_t       group_id  = H5I_INVALID_HID;
    hid_t       filespace = H5I_INVALID_HID, memspace = H5I_INVALID_HID;
    int        *recvcounts = NULL;
    int        *displs     = NULL;

    if (MAINPROCESS)
        HDputs("Testing interleaved read from filtered chunks");

    dataset_dims[0] = (hsize_t)INTERLEAVED_READ_FILTERED_DATASET_NROWS;
    dataset_dims[1] = (hsize_t)INTERLEAVED_READ_FILTERED_DATASET_NCOLS;

    /* Setup the buffer for writing and for comparison */
    correct_buf_size = dataset_dims[0] * dataset_dims[1] * sizeof(*correct_buf);

    correct_buf = (C_DATATYPE *)HDcalloc(1, correct_buf_size);
    VRFY((NULL != correct_buf), "HDcalloc succeeded");

    for (i = 0; i < correct_buf_size / sizeof(*correct_buf); i++)
        /* Add Column Index */
        correct_buf[i] =
            (C_DATATYPE)((i % (hsize_t)INTERLEAVED_READ_FILTERED_DATASET_NCOLS)

                         /* Add the Row Index */
                         + ((i % (hsize_t)(mpi_size * INTERLEAVED_READ_FILTERED_DATASET_NCOLS)) /
                            (hsize_t)INTERLEAVED_READ_FILTERED_DATASET_NCOLS)

                         /* Add the amount that gets added when a rank moves down to its next section
                            vertically in the dataset */
                         + ((hsize_t)INTERLEAVED_READ_FILTERED_DATASET_NCOLS *
                            (i / (hsize_t)(mpi_size * INTERLEAVED_READ_FILTERED_DATASET_NCOLS))));

    if (MAINPROCESS) {
        plist_id = H5Pcreate(H5P_FILE_ACCESS);
        VRFY((plist_id >= 0), "FAPL creation succeeded");

        VRFY((H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0),
             "Set libver bounds succeeded");

        file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, plist_id);
        VRFY((file_id >= 0), "Test file open succeeded");

        VRFY((H5Pclose(plist_id) >= 0), "FAPL close succeeded");

        group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
        VRFY((group_id >= 0), "H5Gopen2 succeeded");

        /* Create the dataspace for the dataset */
        filespace = H5Screate_simple(INTERLEAVED_READ_FILTERED_DATASET_DIMS, dataset_dims, NULL);
        VRFY((filespace >= 0), "File dataspace creation succeeded");

        /* Create chunked dataset */
        chunk_dims[0] = (hsize_t)INTERLEAVED_READ_FILTERED_DATASET_CH_NROWS;
        chunk_dims[1] = (hsize_t)INTERLEAVED_READ_FILTERED_DATASET_CH_NCOLS;

        plist_id = H5Pcopy(dcpl_id);
        VRFY((plist_id >= 0), "DCPL copy succeeded");

        VRFY((H5Pset_chunk(plist_id, INTERLEAVED_READ_FILTERED_DATASET_DIMS, chunk_dims) >= 0),
             "Chunk size set");

        /* Add test filter to the pipeline */
        VRFY((set_dcpl_filter(plist_id, filter_id, NULL) >= 0), "Filter set");

        dset_id = H5Dcreate2(group_id, INTERLEAVED_READ_FILTERED_DATASET_NAME, HDF5_DATATYPE_NAME, filespace,
                             H5P_DEFAULT, plist_id, H5P_DEFAULT);
        VRFY((dset_id >= 0), "Dataset creation succeeded");

        /* Verify space allocation status */
        verify_space_alloc_status(dset_id, plist_id, DATASET_JUST_CREATED);

        VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

        VRFY((H5Dwrite(dset_id, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, H5P_DEFAULT, correct_buf) >= 0),
             "Dataset write succeeded");

        /* Verify space allocation status */
        verify_space_alloc_status(dset_id, plist_id, ALL_CHUNKS_WRITTEN);

        VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
        VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
        VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
        VRFY((H5Fclose(file_id) >= 0), "File close succeeded");
    }

    file_id = H5Fopen(filenames[0], H5F_ACC_RDONLY, fapl_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
    VRFY((group_id >= 0), "H5Gopen2 succeeded");

    dset_id = H5Dopen2(group_id, INTERLEAVED_READ_FILTERED_DATASET_NAME, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    sel_dims[0] = (hsize_t)(INTERLEAVED_READ_FILTERED_DATASET_NROWS / mpi_size);
    sel_dims[1] = (hsize_t)INTERLEAVED_READ_FILTERED_DATASET_NCOLS;

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
    count[0] =
        (hsize_t)(INTERLEAVED_READ_FILTERED_DATASET_NROWS / INTERLEAVED_READ_FILTERED_DATASET_CH_NROWS);
    count[1] =
        (hsize_t)(INTERLEAVED_READ_FILTERED_DATASET_NCOLS / INTERLEAVED_READ_FILTERED_DATASET_CH_NCOLS);
    stride[0] = (hsize_t)INTERLEAVED_READ_FILTERED_DATASET_CH_NROWS;
    stride[1] = (hsize_t)INTERLEAVED_READ_FILTERED_DATASET_CH_NCOLS;
    block[0]  = 1;
    block[1]  = (hsize_t)INTERLEAVED_READ_FILTERED_DATASET_CH_NCOLS;
    start[0]  = (hsize_t)mpi_rank;
    start[1]  = 0;

    if (VERBOSE_MED) {
        HDprintf("Process %d is reading with count[ %" PRIuHSIZE ", %" PRIuHSIZE " ], stride[ %" PRIuHSIZE
                 ", %" PRIuHSIZE " ], start[ %" PRIuHSIZE ", %" PRIuHSIZE " ], block size[ %" PRIuHSIZE
                 ", %" PRIuHSIZE " ]\n",
                 mpi_rank, count[0], count[1], stride[0], stride[1], start[0], start[1], block[0], block[1]);
        HDfflush(stdout);
    }

    VRFY((H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, stride, count, block) >= 0),
         "Hyperslab selection succeeded");

    read_buf_size = flat_dims[0] * sizeof(*read_buf);

    read_buf = (C_DATATYPE *)HDcalloc(1, read_buf_size);
    VRFY((NULL != read_buf), "HDcalloc succeeded");

    VRFY((H5Dread(dset_id, HDF5_DATATYPE_NAME, memspace, filespace, dxpl_id, read_buf) >= 0),
         "Dataset read succeeded");

    global_buf = (C_DATATYPE *)HDcalloc(1, correct_buf_size);
    VRFY((NULL != global_buf), "HDcalloc succeeded");

    /*
     * Since these chunks are shared, run multiple rounds of MPI_Allgatherv
     * to collect all of the pieces into their appropriate locations. The
     * number of times MPI_Allgatherv is run should be equal to the number
     * of chunks in the first dimension of the dataset.
     */
    {
        size_t loop_count       = count[0];
        size_t total_recvcounts = 0;

        recvcounts = (int *)HDcalloc(1, (size_t)mpi_size * sizeof(*recvcounts));
        VRFY((NULL != recvcounts), "HDcalloc succeeded");

        displs = (int *)HDcalloc(1, (size_t)mpi_size * sizeof(*displs));
        VRFY((NULL != displs), "HDcalloc succeeded");

        for (i = 0; i < (size_t)mpi_size; i++) {
            recvcounts[i] = (int)dataset_dims[1];
            total_recvcounts += (size_t)recvcounts[i];
        }

        for (i = 0; i < (size_t)mpi_size; i++)
            displs[i] = (int)(i * dataset_dims[1]);

        for (; loop_count; loop_count--) {
            VRFY((MPI_SUCCESS == MPI_Allgatherv(&read_buf[(count[0] - loop_count) * dataset_dims[1]],
                                                recvcounts[mpi_rank], C_DATATYPE_MPI,
                                                &global_buf[(count[0] - loop_count) * total_recvcounts],
                                                recvcounts, displs, C_DATATYPE_MPI, comm)),
                 "MPI_Allgatherv succeeded");
        }
    }

    VRFY((0 == HDmemcmp(global_buf, correct_buf, correct_buf_size)), "Data verification succeeded");

    if (displs)
        HDfree(displs);
    if (recvcounts)
        HDfree(recvcounts);
    if (global_buf)
        HDfree(global_buf);
    if (read_buf)
        HDfree(read_buf);
    if (correct_buf)
        HDfree(correct_buf);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Sclose(memspace) >= 0), "Memory dataspace close succeeded");
    VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
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
test_read_3d_filtered_dataset_no_overlap_separate_pages(const char *parent_group, H5Z_filter_t filter_id,
                                                        hid_t fapl_id, hid_t dcpl_id, hid_t dxpl_id)
{
    MPI_Datatype vector_type;
    MPI_Datatype resized_vector_type;
    C_DATATYPE  *read_buf    = NULL;
    C_DATATYPE  *correct_buf = NULL;
    C_DATATYPE  *global_buf  = NULL;
    hsize_t      dataset_dims[READ_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_DIMS];
    hsize_t      chunk_dims[READ_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_DIMS];
    hsize_t      sel_dims[READ_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_DIMS];
    hsize_t      start[READ_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_DIMS];
    hsize_t      stride[READ_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_DIMS];
    hsize_t      count[READ_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_DIMS];
    hsize_t      block[READ_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_DIMS];
    hsize_t      flat_dims[1];
    size_t       i, read_buf_size, correct_buf_size;
    hid_t        file_id = H5I_INVALID_HID, dset_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID;
    hid_t        group_id  = H5I_INVALID_HID;
    hid_t        filespace = H5I_INVALID_HID, memspace = H5I_INVALID_HID;

    if (MAINPROCESS)
        HDputs("Testing read from unshared filtered chunks on separate pages in 3D dataset");

    dataset_dims[0] = (hsize_t)READ_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_NROWS;
    dataset_dims[1] = (hsize_t)READ_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_NCOLS;
    dataset_dims[2] = (hsize_t)READ_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DEPTH;

    /* Setup the buffer for writing and for comparison */
    correct_buf_size = dataset_dims[0] * dataset_dims[1] * dataset_dims[2] * sizeof(*correct_buf);

    correct_buf = (C_DATATYPE *)HDcalloc(1, correct_buf_size);
    VRFY((NULL != correct_buf), "HDcalloc succeeded");

    for (i = 0; i < correct_buf_size / sizeof(*correct_buf); i++)
        correct_buf[i] = (C_DATATYPE)((i % (hsize_t)mpi_size) + (i / (hsize_t)mpi_size));

    if (MAINPROCESS) {
        plist_id = H5Pcreate(H5P_FILE_ACCESS);
        VRFY((plist_id >= 0), "FAPL creation succeeded");

        VRFY((H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0),
             "Set libver bounds succeeded");

        file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, plist_id);
        VRFY((file_id >= 0), "Test file open succeeded");

        VRFY((H5Pclose(plist_id) >= 0), "FAPL close succeeded");

        group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
        VRFY((group_id >= 0), "H5Gopen2 succeeded");

        /* Create the dataspace for the dataset */
        filespace =
            H5Screate_simple(READ_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_DIMS, dataset_dims, NULL);
        VRFY((filespace >= 0), "File dataspace creation succeeded");

        /* Create chunked dataset */
        chunk_dims[0] = (hsize_t)READ_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_CH_NROWS;
        chunk_dims[1] = (hsize_t)READ_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_CH_NCOLS;
        chunk_dims[2] = 1;

        plist_id = H5Pcopy(dcpl_id);
        VRFY((plist_id >= 0), "DCPL copy succeeded");

        VRFY(
            (H5Pset_chunk(plist_id, READ_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_DIMS, chunk_dims) >= 0),
            "Chunk size set");

        /* Add test filter to the pipeline */
        VRFY((set_dcpl_filter(plist_id, filter_id, NULL) >= 0), "Filter set");

        dset_id = H5Dcreate2(group_id, READ_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_NAME,
                             HDF5_DATATYPE_NAME, filespace, H5P_DEFAULT, plist_id, H5P_DEFAULT);
        VRFY((dset_id >= 0), "Dataset creation succeeded");

        /* Verify space allocation status */
        verify_space_alloc_status(dset_id, plist_id, DATASET_JUST_CREATED);

        VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

        VRFY((H5Dwrite(dset_id, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, H5P_DEFAULT, correct_buf) >= 0),
             "Dataset write succeeded");

        /* Verify space allocation status */
        verify_space_alloc_status(dset_id, plist_id, ALL_CHUNKS_WRITTEN);

        VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
        VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
        VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
        VRFY((H5Fclose(file_id) >= 0), "File close succeeded");
    }

    file_id = H5Fopen(filenames[0], H5F_ACC_RDONLY, fapl_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
    VRFY((group_id >= 0), "H5Gopen2 succeeded");

    dset_id = H5Dopen2(group_id, READ_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_NAME, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    sel_dims[0] = (hsize_t)READ_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_NROWS;
    sel_dims[1] = (hsize_t)READ_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_NCOLS;
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
    count[0] = (hsize_t)READ_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_NROWS /
               (hsize_t)READ_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_CH_NROWS;
    count[1] = (hsize_t)READ_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_NCOLS /
               (hsize_t)READ_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_CH_NCOLS;
    count[2]  = 1;
    stride[0] = (hsize_t)READ_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_CH_NROWS;
    stride[1] = (hsize_t)READ_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_CH_NCOLS;
    stride[2] = 1;
    block[0]  = (hsize_t)READ_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_CH_NROWS;
    block[1]  = (hsize_t)READ_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_CH_NCOLS;
    block[2]  = 1;
    start[0]  = 0;
    start[1]  = 0;
    start[2]  = (hsize_t)mpi_rank;

    if (VERBOSE_MED) {
        HDprintf("Process %d is reading with count[ %" PRIuHSIZE ", %" PRIuHSIZE " ], stride[ %" PRIuHSIZE
                 ", %" PRIuHSIZE " ], start[ %" PRIuHSIZE ", %" PRIuHSIZE " ], block size[ %" PRIuHSIZE
                 ", %" PRIuHSIZE " ]\n",
                 mpi_rank, count[0], count[1], stride[0], stride[1], start[0], start[1], block[0], block[1]);
        HDfflush(stdout);
    }

    VRFY((H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, stride, count, block) >= 0),
         "Hyperslab selection succeeded");

    read_buf_size = flat_dims[0] * sizeof(*read_buf);

    read_buf = (C_DATATYPE *)HDcalloc(1, read_buf_size);
    VRFY((NULL != read_buf), "HDcalloc succeeded");

    VRFY((H5Dread(dset_id, HDF5_DATATYPE_NAME, memspace, filespace, dxpl_id, read_buf) >= 0),
         "Dataset read succeeded");

    global_buf = (C_DATATYPE *)HDcalloc(1, correct_buf_size);
    VRFY((NULL != global_buf), "HDcalloc succeeded");

    /*
     * Due to the nature of 3-dimensional reading, create an MPI vector type that allows each
     * rank to write to the nth position of the global data buffer, where n is the rank number.
     */
    VRFY((MPI_SUCCESS == MPI_Type_vector((int)flat_dims[0], 1, mpi_size, C_DATATYPE_MPI, &vector_type)),
         "MPI_Type_vector succeeded");
    VRFY((MPI_SUCCESS == MPI_Type_commit(&vector_type)), "MPI_Type_commit succeeded");

    /*
     * Resize the type to allow interleaving,
     * so make it only one MPI_LONG wide
     */
    VRFY((MPI_SUCCESS == MPI_Type_create_resized(vector_type, 0, sizeof(long), &resized_vector_type)),
         "MPI_Type_create_resized");
    VRFY((MPI_SUCCESS == MPI_Type_commit(&resized_vector_type)), "MPI_Type_commit succeeded");

    VRFY((MPI_SUCCESS == MPI_Allgather(read_buf, (int)flat_dims[0], C_DATATYPE_MPI, global_buf, 1,
                                       resized_vector_type, comm)),
         "MPI_Allgather succeeded");

    VRFY((0 == HDmemcmp(global_buf, correct_buf, correct_buf_size)), "Data verification succeeded");

    VRFY((MPI_SUCCESS == MPI_Type_free(&vector_type)), "MPI_Type_free succeeded");
    VRFY((MPI_SUCCESS == MPI_Type_free(&resized_vector_type)), "MPI_Type_free succeeded");

    if (global_buf)
        HDfree(global_buf);
    if (read_buf)
        HDfree(read_buf);
    if (correct_buf)
        HDfree(correct_buf);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Sclose(memspace) >= 0), "Memory dataspace close succeeded");
    VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    return;
}

/*
 * Tests parallel read of transformed and filtered data in the
 * case where only one process is reading from a particular
 * chunk in the operation. Normally, a data transform function
 * will cause the parallel library to break to independent I/O
 * and this isn't allowed when there are filters in the pipeline.
 * However, in this case the parallel library recognizes that
 * the used data transform function "x" is the same as not
 * applying the transform function. Therefore it does not apply
 * the transform function resulting in not breaking to
 * independent I/O.
 *
 * The MAINPROCESS rank will first write out all of the
 * data to the dataset. Then, each rank reads a part of
 * the dataset and contributes its piece to a global buffer
 * that is checked for consistency.
 *
 * Programmer: Jan-Willem Blokland
 *             08/20/2021
 */
static void
test_read_transformed_filtered_dataset_no_overlap(const char *parent_group, H5Z_filter_t filter_id,
                                                  hid_t fapl_id, hid_t dcpl_id, hid_t dxpl_id)
{
    C_DATATYPE *read_buf    = NULL;
    C_DATATYPE *correct_buf = NULL;
    C_DATATYPE *global_buf  = NULL;
    hsize_t     dataset_dims[READ_UNSHARED_TRANSFORMED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     chunk_dims[READ_UNSHARED_TRANSFORMED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     sel_dims[READ_UNSHARED_TRANSFORMED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     start[READ_UNSHARED_TRANSFORMED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     stride[READ_UNSHARED_TRANSFORMED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     count[READ_UNSHARED_TRANSFORMED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     block[READ_UNSHARED_TRANSFORMED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     flat_dims[1];
    size_t      i, read_buf_size, correct_buf_size;
    hid_t       file_id = H5I_INVALID_HID, dset_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID;
    hid_t       group_id  = H5I_INVALID_HID;
    hid_t       filespace = H5I_INVALID_HID, memspace = H5I_INVALID_HID;
    int        *recvcounts = NULL;
    int        *displs     = NULL;

    if (MAINPROCESS)
        HDputs("Testing read from unshared transformed and filtered chunks");

    dataset_dims[0] = (hsize_t)READ_UNSHARED_TRANSFORMED_FILTERED_CHUNKS_NROWS;
    dataset_dims[1] = (hsize_t)READ_UNSHARED_TRANSFORMED_FILTERED_CHUNKS_NCOLS;

    /* Setup the buffer for writing and for comparison */
    correct_buf_size = (hsize_t)READ_UNSHARED_TRANSFORMED_FILTERED_CHUNKS_NROWS *
                       (hsize_t)READ_UNSHARED_TRANSFORMED_FILTERED_CHUNKS_NCOLS * sizeof(*correct_buf);

    correct_buf = (C_DATATYPE *)HDcalloc(1, correct_buf_size);
    VRFY((NULL != correct_buf), "HDcalloc succeeded");

    for (i = 0; i < correct_buf_size / sizeof(*correct_buf); i++)
        correct_buf[i] = (C_DATATYPE)((i % (dataset_dims[0] / (hsize_t)mpi_size * dataset_dims[1])) +
                                      (i / (dataset_dims[0] / (hsize_t)mpi_size * dataset_dims[1])));

    if (MAINPROCESS) {
        plist_id = H5Pcreate(H5P_FILE_ACCESS);
        VRFY((plist_id >= 0), "FAPL creation succeeded");

        VRFY((H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0),
             "Set libver bounds succeeded");

        file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, plist_id);
        VRFY((file_id >= 0), "Test file open succeeded");

        VRFY((H5Pclose(plist_id) >= 0), "FAPL close succeeded");

        group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
        VRFY((group_id >= 0), "H5Gopen2 succeeded");

        /* Create the dataspace for the dataset */
        filespace =
            H5Screate_simple(READ_UNSHARED_TRANSFORMED_FILTERED_CHUNKS_DATASET_DIMS, dataset_dims, NULL);
        VRFY((filespace >= 0), "File dataspace creation succeeded");

        /* Create chunked dataset */
        chunk_dims[0] = (hsize_t)READ_UNSHARED_TRANSFORMED_FILTERED_CHUNKS_CH_NROWS;
        chunk_dims[1] = (hsize_t)READ_UNSHARED_TRANSFORMED_FILTERED_CHUNKS_CH_NCOLS;

        plist_id = H5Pcopy(dcpl_id);
        VRFY((plist_id >= 0), "DCPL copy succeeded");

        VRFY(
            (H5Pset_chunk(plist_id, READ_UNSHARED_TRANSFORMED_FILTERED_CHUNKS_DATASET_DIMS, chunk_dims) >= 0),
            "Chunk size set");

        /* Add test filter to the pipeline */
        VRFY((set_dcpl_filter(plist_id, filter_id, NULL) >= 0), "Filter set");

        dset_id = H5Dcreate2(group_id, READ_UNSHARED_TRANSFORMED_FILTERED_CHUNKS_DATASET_NAME,
                             HDF5_DATATYPE_NAME, filespace, H5P_DEFAULT, plist_id, H5P_DEFAULT);
        VRFY((dset_id >= 0), "Dataset creation succeeded");

        /* Verify space allocation status */
        verify_space_alloc_status(dset_id, plist_id, DATASET_JUST_CREATED);

        VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
        VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

        /* Create property list for collective dataset read */
        plist_id = H5Pcreate(H5P_DATASET_XFER);
        VRFY((plist_id >= 0), "DXPL creation succeeded");

        /* Set data transform expression */
        VRFY((H5Pset_data_transform(plist_id, "x") >= 0), "Set data transform expression succeeded");

        VRFY((H5Dwrite(dset_id, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, plist_id, correct_buf) >= 0),
             "Dataset write succeeded");

        VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");

        /* Verify space allocation status */
        plist_id = H5Dget_create_plist(dset_id);
        VRFY((plist_id >= 0), "H5Dget_create_plist succeeded");
        verify_space_alloc_status(dset_id, plist_id, ALL_CHUNKS_WRITTEN);

        VRFY((H5Pclose(plist_id) >= 0), "DXPL close succeeded");
        VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
        VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
        VRFY((H5Fclose(file_id) >= 0), "File close succeeded");
    }

    file_id = H5Fopen(filenames[0], H5F_ACC_RDONLY, fapl_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
    VRFY((group_id >= 0), "H5Gopen2 succeeded");

    dset_id = H5Dopen2(group_id, READ_UNSHARED_TRANSFORMED_FILTERED_CHUNKS_DATASET_NAME, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    sel_dims[0] = (hsize_t)READ_UNSHARED_TRANSFORMED_FILTERED_CHUNKS_CH_NROWS;
    sel_dims[1] = (hsize_t)READ_UNSHARED_TRANSFORMED_FILTERED_CHUNKS_NCOLS;

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
    count[1] = (hsize_t)READ_UNSHARED_TRANSFORMED_FILTERED_CHUNKS_NCOLS /
               (hsize_t)READ_UNSHARED_TRANSFORMED_FILTERED_CHUNKS_CH_NCOLS;
    stride[0] = (hsize_t)READ_UNSHARED_TRANSFORMED_FILTERED_CHUNKS_CH_NROWS;
    stride[1] = (hsize_t)READ_UNSHARED_TRANSFORMED_FILTERED_CHUNKS_CH_NCOLS;
    block[0]  = (hsize_t)READ_UNSHARED_TRANSFORMED_FILTERED_CHUNKS_CH_NROWS;
    block[1]  = (hsize_t)READ_UNSHARED_TRANSFORMED_FILTERED_CHUNKS_CH_NCOLS;
    start[0]  = ((hsize_t)mpi_rank * (hsize_t)READ_UNSHARED_TRANSFORMED_FILTERED_CHUNKS_CH_NROWS * count[0]);
    start[1]  = 0;

    if (VERBOSE_MED) {
        HDprintf("Process %d is reading with count[ %" PRIuHSIZE ", %" PRIuHSIZE " ], stride[ %" PRIuHSIZE
                 ", %" PRIuHSIZE " ], start[ %" PRIuHSIZE ", %" PRIuHSIZE " ], block size[ %" PRIuHSIZE
                 ", %" PRIuHSIZE " ]\n",
                 mpi_rank, count[0], count[1], stride[0], stride[1], start[0], start[1], block[0], block[1]);
        HDfflush(stdout);
    }

    VRFY((H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, stride, count, block) >= 0),
         "Hyperslab selection succeeded");

    /* Create property list for data transform */
    plist_id = H5Pcopy(dxpl_id);
    VRFY((plist_id >= 0), "DXPL copy succeeded");

    /* Set data transform expression */
    VRFY((H5Pset_data_transform(plist_id, "x") >= 0), "Set data transform expression succeeded");

    read_buf_size = flat_dims[0] * sizeof(*read_buf);

    read_buf = (C_DATATYPE *)HDcalloc(1, read_buf_size);
    VRFY((NULL != read_buf), "HDcalloc succeeded");

    VRFY((H5Dread(dset_id, HDF5_DATATYPE_NAME, memspace, filespace, plist_id, read_buf) >= 0),
         "Dataset read succeeded");

    global_buf = (C_DATATYPE *)HDcalloc(1, correct_buf_size);
    VRFY((NULL != global_buf), "HDcalloc succeeded");

    /* Collect each piece of data from all ranks into a global buffer on all ranks */
    recvcounts = (int *)HDcalloc(1, (size_t)mpi_size * sizeof(*recvcounts));
    VRFY((NULL != recvcounts), "HDcalloc succeeded");

    for (i = 0; i < (size_t)mpi_size; i++)
        recvcounts[i] = (int)flat_dims[0];

    displs = (int *)HDcalloc(1, (size_t)mpi_size * sizeof(*displs));
    VRFY((NULL != displs), "HDcalloc succeeded");

    for (i = 0; i < (size_t)mpi_size; i++)
        displs[i] = (int)(i * flat_dims[0]);

    VRFY((MPI_SUCCESS == MPI_Allgatherv(read_buf, (int)flat_dims[0], C_DATATYPE_MPI, global_buf, recvcounts,
                                        displs, C_DATATYPE_MPI, comm)),
         "MPI_Allgatherv succeeded");

    VRFY((0 == HDmemcmp(global_buf, correct_buf, correct_buf_size)), "Data verification succeeded");

    if (displs)
        HDfree(displs);
    if (recvcounts)
        HDfree(recvcounts);
    if (global_buf)
        HDfree(global_buf);
    if (read_buf)
        HDfree(read_buf);
    if (correct_buf)
        HDfree(correct_buf);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Sclose(memspace) >= 0), "Memory dataspace close succeeded");
    VRFY((H5Pclose(plist_id) >= 0), "DXPL close succeeded");
    VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
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
test_read_3d_filtered_dataset_no_overlap_same_pages(const char *parent_group, H5Z_filter_t filter_id,
                                                    hid_t fapl_id, hid_t dcpl_id, hid_t dxpl_id)
{
    C_DATATYPE *read_buf    = NULL;
    C_DATATYPE *correct_buf = NULL;
    C_DATATYPE *global_buf  = NULL;
    hsize_t     dataset_dims[READ_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_DIMS];
    hsize_t     chunk_dims[READ_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_DIMS];
    hsize_t     sel_dims[READ_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_DIMS];
    hsize_t     start[READ_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_DIMS];
    hsize_t     stride[READ_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_DIMS];
    hsize_t     count[READ_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_DIMS];
    hsize_t     block[READ_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_DIMS];
    hsize_t     flat_dims[1];
    size_t      i, read_buf_size, correct_buf_size;
    hid_t       file_id = H5I_INVALID_HID, dset_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID;
    hid_t       group_id  = H5I_INVALID_HID;
    hid_t       filespace = H5I_INVALID_HID, memspace = H5I_INVALID_HID;
    int        *recvcounts = NULL;
    int        *displs     = NULL;

    if (MAINPROCESS)
        HDputs("Testing read from unshared filtered chunks on the same pages in 3D dataset");

    dataset_dims[0] = (hsize_t)READ_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_NROWS;
    dataset_dims[1] = (hsize_t)READ_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_NCOLS;
    dataset_dims[2] = (hsize_t)READ_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DEPTH;

    /* Setup the buffer for writing and for comparison */
    correct_buf_size = dataset_dims[0] * dataset_dims[1] * dataset_dims[2] * sizeof(*correct_buf);

    correct_buf = (C_DATATYPE *)HDcalloc(1, correct_buf_size);
    VRFY((NULL != correct_buf), "HDcalloc succeeded");

    for (i = 0; i < correct_buf_size / sizeof(*correct_buf); i++)
        correct_buf[i] = (C_DATATYPE)((i % (dataset_dims[0] * dataset_dims[1])) +
                                      (i / (dataset_dims[0] * dataset_dims[1])));

    if (MAINPROCESS) {
        plist_id = H5Pcreate(H5P_FILE_ACCESS);
        VRFY((plist_id >= 0), "FAPL creation succeeded");

        VRFY((H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0),
             "Set libver bounds succeeded");

        file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, plist_id);
        VRFY((file_id >= 0), "Test file open succeeded");

        VRFY((H5Pclose(plist_id) >= 0), "FAPL close succeeded");

        group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
        VRFY((group_id >= 0), "H5Gopen2 succeeded");

        /* Create the dataspace for the dataset */
        filespace =
            H5Screate_simple(READ_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_DIMS, dataset_dims, NULL);
        VRFY((filespace >= 0), "File dataspace creation succeeded");

        /* Create chunked dataset */
        chunk_dims[0] = (hsize_t)READ_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_CH_NROWS;
        chunk_dims[1] = (hsize_t)READ_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_CH_NCOLS;
        chunk_dims[2] = 1;

        plist_id = H5Pcopy(dcpl_id);
        VRFY((plist_id >= 0), "DCPL copy succeeded");

        VRFY((H5Pset_chunk(plist_id, READ_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_DIMS, chunk_dims) >=
              0),
             "Chunk size set");

        /* Add test filter to the pipeline */
        VRFY((set_dcpl_filter(plist_id, filter_id, NULL) >= 0), "Filter set");

        dset_id = H5Dcreate2(group_id, READ_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_NAME,
                             HDF5_DATATYPE_NAME, filespace, H5P_DEFAULT, plist_id, H5P_DEFAULT);
        VRFY((dset_id >= 0), "Dataset creation succeeded");

        /* Verify space allocation status */
        verify_space_alloc_status(dset_id, plist_id, DATASET_JUST_CREATED);

        VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

        VRFY((H5Dwrite(dset_id, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, H5P_DEFAULT, correct_buf) >= 0),
             "Dataset write succeeded");

        /* Verify space allocation status */
        verify_space_alloc_status(dset_id, plist_id, ALL_CHUNKS_WRITTEN);

        VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
        VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
        VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
        VRFY((H5Fclose(file_id) >= 0), "File close succeeded");
    }

    file_id = H5Fopen(filenames[0], H5F_ACC_RDONLY, fapl_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
    VRFY((group_id >= 0), "H5Gopen2 succeeded");

    dset_id = H5Dopen2(group_id, READ_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_NAME, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    sel_dims[0] = (hsize_t)READ_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_CH_NROWS;
    sel_dims[1] = (hsize_t)READ_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_NCOLS;
    sel_dims[2] = (hsize_t)READ_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DEPTH;

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
    count[1] = (hsize_t)READ_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_NCOLS /
               (hsize_t)READ_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_CH_NCOLS;
    count[2]  = (hsize_t)mpi_size;
    stride[0] = (hsize_t)READ_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_CH_NROWS;
    stride[1] = (hsize_t)READ_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_CH_NCOLS;
    stride[2] = 1;
    block[0]  = (hsize_t)READ_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_CH_NROWS;
    block[1]  = (hsize_t)READ_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_CH_NCOLS;
    block[2]  = 1;
    start[0]  = ((hsize_t)mpi_rank * (hsize_t)READ_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_CH_NROWS * count[0]);
    start[1]  = 0;
    start[2]  = 0;

    if (VERBOSE_MED) {
        HDprintf("Process %d is reading with count[ %" PRIuHSIZE ", %" PRIuHSIZE " ], stride[ %" PRIuHSIZE
                 ", %" PRIuHSIZE " ], start[ %" PRIuHSIZE ", %" PRIuHSIZE " ], block size[ %" PRIuHSIZE
                 ", %" PRIuHSIZE " ]\n",
                 mpi_rank, count[0], count[1], stride[0], stride[1], start[0], start[1], block[0], block[1]);
        HDfflush(stdout);
    }

    VRFY((H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, stride, count, block) >= 0),
         "Hyperslab selection succeeded");

    read_buf_size = flat_dims[0] * sizeof(*read_buf);

    read_buf = (C_DATATYPE *)HDcalloc(1, read_buf_size);
    VRFY((NULL != read_buf), "HDcalloc succeeded");

    VRFY((H5Dread(dset_id, HDF5_DATATYPE_NAME, memspace, filespace, dxpl_id, read_buf) >= 0),
         "Dataset read succeeded");

    global_buf = (C_DATATYPE *)HDcalloc(1, correct_buf_size);
    VRFY((NULL != global_buf), "HDcalloc succeeded");

    /* Collect each piece of data from all ranks into a global buffer on all ranks */
    recvcounts = (int *)HDcalloc(1, (size_t)mpi_size * sizeof(*recvcounts));
    VRFY((NULL != recvcounts), "HDcalloc succeeded");

    for (i = 0; i < (size_t)mpi_size; i++)
        recvcounts[i] = (int)flat_dims[0];

    displs = (int *)HDcalloc(1, (size_t)mpi_size * sizeof(*displs));
    VRFY((NULL != displs), "HDcalloc succeeded");

    for (i = 0; i < (size_t)mpi_size; i++)
        displs[i] = (int)(i * flat_dims[0]);

    VRFY((MPI_SUCCESS == MPI_Allgatherv(read_buf, (int)flat_dims[0], C_DATATYPE_MPI, global_buf, recvcounts,
                                        displs, C_DATATYPE_MPI, comm)),
         "MPI_Allgatherv succeeded");

    VRFY((0 == HDmemcmp(global_buf, correct_buf, correct_buf_size)), "Data verification succeeded");

    if (displs)
        HDfree(displs);
    if (recvcounts)
        HDfree(recvcounts);
    if (global_buf)
        HDfree(global_buf);
    if (read_buf)
        HDfree(read_buf);
    if (correct_buf)
        HDfree(correct_buf);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Sclose(memspace) >= 0), "Memory dataspace close succeeded");
    VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
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
test_read_3d_filtered_dataset_overlap(const char *parent_group, H5Z_filter_t filter_id, hid_t fapl_id,
                                      hid_t dcpl_id, hid_t dxpl_id)
{
    MPI_Datatype vector_type;
    MPI_Datatype resized_vector_type;
    C_DATATYPE  *read_buf    = NULL;
    C_DATATYPE  *correct_buf = NULL;
    C_DATATYPE  *global_buf  = NULL;
    hsize_t      dataset_dims[READ_SHARED_FILTERED_CHUNKS_3D_DATASET_DIMS];
    hsize_t      chunk_dims[READ_SHARED_FILTERED_CHUNKS_3D_DATASET_DIMS];
    hsize_t      sel_dims[READ_SHARED_FILTERED_CHUNKS_3D_DATASET_DIMS];
    hsize_t      start[READ_SHARED_FILTERED_CHUNKS_3D_DATASET_DIMS];
    hsize_t      stride[READ_SHARED_FILTERED_CHUNKS_3D_DATASET_DIMS];
    hsize_t      count[READ_SHARED_FILTERED_CHUNKS_3D_DATASET_DIMS];
    hsize_t      block[READ_SHARED_FILTERED_CHUNKS_3D_DATASET_DIMS];
    hsize_t      flat_dims[1];
    size_t       i, read_buf_size, correct_buf_size;
    hid_t        file_id = H5I_INVALID_HID, dset_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID;
    hid_t        group_id  = H5I_INVALID_HID;
    hid_t        filespace = H5I_INVALID_HID, memspace = H5I_INVALID_HID;

    if (MAINPROCESS)
        HDputs("Testing read from shared filtered chunks in 3D dataset");

    dataset_dims[0] = (hsize_t)READ_SHARED_FILTERED_CHUNKS_3D_NROWS;
    dataset_dims[1] = (hsize_t)READ_SHARED_FILTERED_CHUNKS_3D_NCOLS;
    dataset_dims[2] = (hsize_t)READ_SHARED_FILTERED_CHUNKS_3D_DEPTH;

    /* Setup the buffer for writing and for comparison */
    correct_buf_size = dataset_dims[0] * dataset_dims[1] * dataset_dims[2] * sizeof(*correct_buf);

    correct_buf = (C_DATATYPE *)HDcalloc(1, correct_buf_size);
    VRFY((NULL != correct_buf), "HDcalloc succeeded");

    for (i = 0; i < correct_buf_size / sizeof(*correct_buf); i++)
        /* Add the Column Index */
        correct_buf[i] = (C_DATATYPE)((i % (hsize_t)(READ_SHARED_FILTERED_CHUNKS_3D_DEPTH *
                                                     READ_SHARED_FILTERED_CHUNKS_3D_NCOLS))

                                      /* Add the Row Index */
                                      + ((i % (hsize_t)(mpi_size * READ_SHARED_FILTERED_CHUNKS_3D_DEPTH *
                                                        READ_SHARED_FILTERED_CHUNKS_3D_NCOLS)) /
                                         (hsize_t)(READ_SHARED_FILTERED_CHUNKS_3D_DEPTH *
                                                   READ_SHARED_FILTERED_CHUNKS_3D_NCOLS))

                                      /* Add the amount that gets added when a rank moves down to its next
                                         section vertically in the dataset */
                                      + ((hsize_t)(READ_SHARED_FILTERED_CHUNKS_3D_DEPTH *
                                                   READ_SHARED_FILTERED_CHUNKS_3D_NCOLS) *
                                         (i / (hsize_t)(mpi_size * READ_SHARED_FILTERED_CHUNKS_3D_DEPTH *
                                                        READ_SHARED_FILTERED_CHUNKS_3D_NCOLS))));

    if (MAINPROCESS) {
        plist_id = H5Pcreate(H5P_FILE_ACCESS);
        VRFY((plist_id >= 0), "FAPL creation succeeded");

        VRFY((H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0),
             "Set libver bounds succeeded");

        file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, plist_id);
        VRFY((file_id >= 0), "Test file open succeeded");

        VRFY((H5Pclose(plist_id) >= 0), "FAPL close succeeded");

        group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
        VRFY((group_id >= 0), "H5Gopen2 succeeded");

        /* Create the dataspace for the dataset */
        filespace = H5Screate_simple(READ_SHARED_FILTERED_CHUNKS_3D_DATASET_DIMS, dataset_dims, NULL);
        VRFY((filespace >= 0), "File dataspace creation succeeded");

        /* Create chunked dataset */
        chunk_dims[0] = (hsize_t)READ_SHARED_FILTERED_CHUNKS_3D_CH_NROWS;
        chunk_dims[1] = (hsize_t)READ_SHARED_FILTERED_CHUNKS_3D_CH_NCOLS;
        chunk_dims[2] = 1;

        plist_id = H5Pcopy(dcpl_id);
        VRFY((plist_id >= 0), "DCPL copy succeeded");

        VRFY((H5Pset_chunk(plist_id, READ_SHARED_FILTERED_CHUNKS_3D_DATASET_DIMS, chunk_dims) >= 0),
             "Chunk size set");

        /* Add test filter to the pipeline */
        VRFY((set_dcpl_filter(plist_id, filter_id, NULL) >= 0), "Filter set");

        dset_id = H5Dcreate2(group_id, READ_SHARED_FILTERED_CHUNKS_3D_DATASET_NAME, HDF5_DATATYPE_NAME,
                             filespace, H5P_DEFAULT, plist_id, H5P_DEFAULT);
        VRFY((dset_id >= 0), "Dataset creation succeeded");

        /* Verify space allocation status */
        verify_space_alloc_status(dset_id, plist_id, DATASET_JUST_CREATED);

        VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

        VRFY((H5Dwrite(dset_id, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, H5P_DEFAULT, correct_buf) >= 0),
             "Dataset write succeeded");

        /* Verify space allocation status */
        verify_space_alloc_status(dset_id, plist_id, ALL_CHUNKS_WRITTEN);

        VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
        VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
        VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
        VRFY((H5Fclose(file_id) >= 0), "File close succeeded");
    }

    file_id = H5Fopen(filenames[0], H5F_ACC_RDONLY, fapl_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
    VRFY((group_id >= 0), "H5Gopen2 succeeded");

    dset_id = H5Dopen2(group_id, READ_SHARED_FILTERED_CHUNKS_3D_DATASET_NAME, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    sel_dims[0] = (hsize_t)(READ_SHARED_FILTERED_CHUNKS_3D_NROWS / mpi_size);
    sel_dims[1] = (hsize_t)READ_SHARED_FILTERED_CHUNKS_3D_NCOLS;
    sel_dims[2] = (hsize_t)READ_SHARED_FILTERED_CHUNKS_3D_DEPTH;

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
    count[0]  = (hsize_t)(READ_SHARED_FILTERED_CHUNKS_3D_NROWS / READ_SHARED_FILTERED_CHUNKS_3D_CH_NROWS);
    count[1]  = (hsize_t)(READ_SHARED_FILTERED_CHUNKS_3D_NCOLS / READ_SHARED_FILTERED_CHUNKS_3D_CH_NCOLS);
    count[2]  = (hsize_t)READ_SHARED_FILTERED_CHUNKS_3D_DEPTH;
    stride[0] = (hsize_t)READ_SHARED_FILTERED_CHUNKS_3D_CH_NROWS;
    stride[1] = (hsize_t)READ_SHARED_FILTERED_CHUNKS_3D_CH_NCOLS;
    stride[2] = 1;
    block[0]  = 1;
    block[1]  = (hsize_t)READ_SHARED_FILTERED_CHUNKS_3D_CH_NCOLS;
    block[2]  = 1;
    start[0]  = (hsize_t)mpi_rank;
    start[1]  = 0;
    start[2]  = 0;

    if (VERBOSE_MED) {
        HDprintf("Process %d is reading with count[ %" PRIuHSIZE ", %" PRIuHSIZE " ], stride[ %" PRIuHSIZE
                 ", %" PRIuHSIZE " ], start[ %" PRIuHSIZE ", %" PRIuHSIZE " ], block size[ %" PRIuHSIZE
                 ", %" PRIuHSIZE " ]\n",
                 mpi_rank, count[0], count[1], stride[0], stride[1], start[0], start[1], block[0], block[1]);
        HDfflush(stdout);
    }

    VRFY((H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, stride, count, block) >= 0),
         "Hyperslab selection succeeded");

    read_buf_size = flat_dims[0] * sizeof(*read_buf);

    read_buf = (C_DATATYPE *)HDcalloc(1, read_buf_size);
    VRFY((NULL != read_buf), "HDcalloc succeeded");

    VRFY((H5Dread(dset_id, HDF5_DATATYPE_NAME, memspace, filespace, dxpl_id, read_buf) >= 0),
         "Dataset read succeeded");

    global_buf = (C_DATATYPE *)HDcalloc(1, correct_buf_size);
    VRFY((NULL != global_buf), "HDcalloc succeeded");

    {
        size_t run_length =
            (size_t)(READ_SHARED_FILTERED_CHUNKS_3D_NCOLS * READ_SHARED_FILTERED_CHUNKS_3D_DEPTH);
        size_t num_blocks = (size_t)(READ_SHARED_FILTERED_CHUNKS_3D_NROWS / mpi_size);

        /*
         * Due to the nature of 3-dimensional reading, create an MPI vector type that allows each
         * rank to write to the nth position of the global data buffer, where n is the rank number.
         */
        VRFY(
            (MPI_SUCCESS == MPI_Type_vector((int)num_blocks, (int)run_length,
                                            (int)(mpi_size * (int)run_length), C_DATATYPE_MPI, &vector_type)),
            "MPI_Type_vector succeeded");
        VRFY((MPI_SUCCESS == MPI_Type_commit(&vector_type)), "MPI_Type_commit succeeded");

        /*
         * Resize the type to allow interleaving,
         * so make it "run_length" MPI_LONGs wide
         */
        VRFY((MPI_SUCCESS == MPI_Type_create_resized(vector_type, 0, (MPI_Aint)(run_length * sizeof(long)),
                                                     &resized_vector_type)),
             "MPI_Type_create_resized");
        VRFY((MPI_SUCCESS == MPI_Type_commit(&resized_vector_type)), "MPI_Type_commit succeeded");
    }

    VRFY((MPI_SUCCESS == MPI_Allgather(read_buf, (int)flat_dims[0], C_DATATYPE_MPI, global_buf, 1,
                                       resized_vector_type, comm)),
         "MPI_Allgatherv succeeded");

    VRFY((0 == HDmemcmp(global_buf, correct_buf, correct_buf_size)), "Data verification succeeded");

    VRFY((MPI_SUCCESS == MPI_Type_free(&vector_type)), "MPI_Type_free succeeded");
    VRFY((MPI_SUCCESS == MPI_Type_free(&resized_vector_type)), "MPI_Type_free succeeded");

    if (global_buf)
        HDfree(global_buf);
    if (read_buf)
        HDfree(read_buf);
    if (correct_buf)
        HDfree(correct_buf);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Sclose(memspace) >= 0), "Memory dataspace close succeeded");
    VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
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
test_read_cmpd_filtered_dataset_no_conversion_unshared(const char *parent_group, H5Z_filter_t filter_id,
                                                       hid_t fapl_id, hid_t dcpl_id, hid_t dxpl_id)
{
    COMPOUND_C_DATATYPE *read_buf    = NULL;
    COMPOUND_C_DATATYPE *correct_buf = NULL;
    COMPOUND_C_DATATYPE *global_buf  = NULL;
    hsize_t              dataset_dims[READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t              chunk_dims[READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t              sel_dims[READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t              start[READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t              stride[READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t              count[READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t              block[READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t              flat_dims[1];
    size_t               i, read_buf_size, correct_buf_size;
    hid_t                file_id = H5I_INVALID_HID, dset_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID,
          memtype   = H5I_INVALID_HID;
    hid_t group_id  = H5I_INVALID_HID;
    hid_t filespace = H5I_INVALID_HID, memspace = H5I_INVALID_HID;
    int  *recvcounts = NULL;
    int  *displs     = NULL;

    if (MAINPROCESS)
        HDputs("Testing read from unshared filtered chunks in Compound Datatype dataset without Datatype "
               "conversion");

    /* SZIP and ScaleOffset filters don't support compound types */
    if (filter_id == H5Z_FILTER_SZIP || filter_id == H5Z_FILTER_SCALEOFFSET) {
        if (MAINPROCESS)
            SKIPPED();
        return;
    }

    dataset_dims[0] = (hsize_t)READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_NROWS;
    dataset_dims[1] = (hsize_t)READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_NCOLS;

    /* Setup the buffer for writing and for comparison */
    correct_buf_size = dataset_dims[0] * dataset_dims[1] * sizeof(*correct_buf);

    correct_buf = (COMPOUND_C_DATATYPE *)HDcalloc(1, correct_buf_size);
    VRFY((NULL != correct_buf), "HDcalloc succeeded");

    for (i = 0; i < correct_buf_size / sizeof(*correct_buf); i++) {
        correct_buf[i].field1 = (short)((i % dataset_dims[1]) + (i / dataset_dims[1]));

        correct_buf[i].field2 = (int)((i % dataset_dims[1]) + (i / dataset_dims[1]));

        correct_buf[i].field3 = (long)((i % dataset_dims[1]) + (i / dataset_dims[1]));
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

        group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
        VRFY((group_id >= 0), "H5Gopen2 succeeded");

        /* Create the dataspace for the dataset */
        filespace = H5Screate_simple(READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_DIMS,
                                     dataset_dims, NULL);
        VRFY((filespace >= 0), "File dataspace creation succeeded");

        /* Create chunked dataset */
        chunk_dims[0] = READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_CH_NROWS;
        chunk_dims[1] = READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_CH_NCOLS;

        plist_id = H5Pcopy(dcpl_id);
        VRFY((plist_id >= 0), "DCPL copy succeeded");

        VRFY((H5Pset_chunk(plist_id, READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_DIMS,
                           chunk_dims) >= 0),
             "Chunk size set");

        /* Add test filter to the pipeline */
        VRFY((set_dcpl_filter(plist_id, filter_id, NULL) >= 0), "Filter set");

        dset_id = H5Dcreate2(group_id, READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_NAME,
                             memtype, filespace, H5P_DEFAULT, plist_id, H5P_DEFAULT);
        VRFY((dset_id >= 0), "Dataset creation succeeded");

        /* Verify space allocation status */
        verify_space_alloc_status(dset_id, plist_id, DATASET_JUST_CREATED);

        VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

        VRFY((H5Dwrite(dset_id, memtype, H5S_ALL, H5S_ALL, H5P_DEFAULT, correct_buf) >= 0),
             "Dataset write succeeded");

        /* Verify space allocation status */
        verify_space_alloc_status(dset_id, plist_id, ALL_CHUNKS_WRITTEN);

        VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
        VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
        VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
        VRFY((H5Fclose(file_id) >= 0), "File close succeeded");
    }

    file_id = H5Fopen(filenames[0], H5F_ACC_RDONLY, fapl_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
    VRFY((group_id >= 0), "H5Gopen2 succeeded");

    dset_id =
        H5Dopen2(group_id, READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_NAME, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    sel_dims[0] = (hsize_t)READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_CH_NROWS;
    sel_dims[1] = (hsize_t)READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_ENTRIES_PER_PROC;

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
    count[0]  = 1;
    count[1]  = (hsize_t)READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_ENTRIES_PER_PROC;
    stride[0] = READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_CH_NROWS;
    stride[1] = READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_CH_NCOLS;
    block[0]  = READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_CH_NROWS;
    block[1]  = READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_CH_NCOLS;
    start[0]  = 0;
    start[1]  = ((hsize_t)mpi_rank * READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_CH_NCOLS);

    if (VERBOSE_MED) {
        HDprintf("Process %d is reading with count[ %" PRIuHSIZE ", %" PRIuHSIZE " ], stride[ %" PRIuHSIZE
                 ", %" PRIuHSIZE " ], start[ %" PRIuHSIZE ", %" PRIuHSIZE " ], block size[ %" PRIuHSIZE
                 ", %" PRIuHSIZE " ]\n",
                 mpi_rank, count[0], count[1], stride[0], stride[1], start[0], start[1], block[0], block[1]);
        HDfflush(stdout);
    }

    VRFY((H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, stride, count, block) >= 0),
         "Hyperslab selection succeeded");

    read_buf_size = flat_dims[0] * sizeof(*read_buf);

    read_buf = (COMPOUND_C_DATATYPE *)HDcalloc(1, read_buf_size);
    VRFY((NULL != read_buf), "HDcalloc succeeded");

    VRFY((H5Dread(dset_id, memtype, memspace, filespace, dxpl_id, read_buf) >= 0), "Dataset read succeeded");

    global_buf = (COMPOUND_C_DATATYPE *)HDcalloc(1, correct_buf_size);
    VRFY((NULL != global_buf), "HDcalloc succeeded");

    /* Collect each piece of data from all ranks into a global buffer on all ranks */
    recvcounts = (int *)HDcalloc(1, (size_t)mpi_size * sizeof(*recvcounts));
    VRFY((NULL != recvcounts), "HDcalloc succeeded");

    for (i = 0; i < (size_t)mpi_size; i++)
        recvcounts[i] = (int)(flat_dims[0] * sizeof(*read_buf));

    displs = (int *)HDcalloc(1, (size_t)mpi_size * sizeof(*displs));
    VRFY((NULL != displs), "HDcalloc succeeded");

    for (i = 0; i < (size_t)mpi_size; i++)
        displs[i] = (int)(i * flat_dims[0] * sizeof(*read_buf));

    VRFY((MPI_SUCCESS == MPI_Allgatherv(read_buf, (int)(flat_dims[0] * sizeof(COMPOUND_C_DATATYPE)), MPI_BYTE,
                                        global_buf, recvcounts, displs, MPI_BYTE, comm)),
         "MPI_Allgatherv succeeded");

    VRFY((0 == HDmemcmp(global_buf, correct_buf, correct_buf_size)), "Data verification succeeded");

    if (displs)
        HDfree(displs);
    if (recvcounts)
        HDfree(recvcounts);
    if (global_buf)
        HDfree(global_buf);
    if (read_buf)
        HDfree(read_buf);
    if (correct_buf)
        HDfree(correct_buf);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Sclose(memspace) >= 0), "Memory dataspace close succeeded");
    VRFY((H5Tclose(memtype) >= 0), "Memory datatype close succeeded");
    VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
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
test_read_cmpd_filtered_dataset_no_conversion_shared(const char *parent_group, H5Z_filter_t filter_id,
                                                     hid_t fapl_id, hid_t dcpl_id, hid_t dxpl_id)
{
    COMPOUND_C_DATATYPE *read_buf    = NULL;
    COMPOUND_C_DATATYPE *correct_buf = NULL;
    COMPOUND_C_DATATYPE *global_buf  = NULL;
    hsize_t              dataset_dims[READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t              chunk_dims[READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t              sel_dims[READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t              start[READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t              stride[READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t              count[READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t              block[READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t              flat_dims[1];
    size_t               i, read_buf_size, correct_buf_size;
    hid_t                file_id = H5I_INVALID_HID, dset_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID,
          memtype   = H5I_INVALID_HID;
    hid_t group_id  = H5I_INVALID_HID;
    hid_t filespace = H5I_INVALID_HID, memspace = H5I_INVALID_HID;
    int  *recvcounts = NULL;
    int  *displs     = NULL;

    if (MAINPROCESS)
        HDputs("Testing read from shared filtered chunks in Compound Datatype dataset without Datatype "
               "conversion");

    /* SZIP and ScaleOffset filters don't support compound types */
    if (filter_id == H5Z_FILTER_SZIP || filter_id == H5Z_FILTER_SCALEOFFSET) {
        if (MAINPROCESS)
            SKIPPED();
        return;
    }

    dataset_dims[0] = (hsize_t)READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_NROWS;
    dataset_dims[1] = (hsize_t)READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_NCOLS;

    /* Setup the buffer for writing and for comparison */
    correct_buf_size = dataset_dims[0] * dataset_dims[1] * sizeof(*correct_buf);

    correct_buf = (COMPOUND_C_DATATYPE *)HDcalloc(1, correct_buf_size);
    VRFY((NULL != correct_buf), "HDcalloc succeeded");

    for (i = 0; i < correct_buf_size / sizeof(*correct_buf); i++) {
        correct_buf[i].field1 =
            (short)((dataset_dims[1] * (i / ((hsize_t)mpi_size * dataset_dims[1]))) + (i % dataset_dims[1]) +
                    (((i % ((hsize_t)mpi_size * dataset_dims[1])) / dataset_dims[1]) % dataset_dims[1]));

        correct_buf[i].field2 =
            (int)((dataset_dims[1] * (i / ((hsize_t)mpi_size * dataset_dims[1]))) + (i % dataset_dims[1]) +
                  (((i % ((hsize_t)mpi_size * dataset_dims[1])) / dataset_dims[1]) % dataset_dims[1]));

        correct_buf[i].field3 =
            (long)((dataset_dims[1] * (i / ((hsize_t)mpi_size * dataset_dims[1]))) + (i % dataset_dims[1]) +
                   (((i % ((hsize_t)mpi_size * dataset_dims[1])) / dataset_dims[1]) % dataset_dims[1]));
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

        group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
        VRFY((group_id >= 0), "H5Gopen2 succeeded");

        /* Create the dataspace for the dataset */
        filespace = H5Screate_simple(READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_DIMS,
                                     dataset_dims, NULL);
        VRFY((filespace >= 0), "File dataspace creation succeeded");

        /* Create chunked dataset */
        chunk_dims[0] = (hsize_t)READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_CH_NROWS;
        chunk_dims[1] = (hsize_t)READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_CH_NCOLS;

        plist_id = H5Pcopy(dcpl_id);
        VRFY((plist_id >= 0), "DCPL copy succeeded");

        VRFY((H5Pset_chunk(plist_id, READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_DIMS,
                           chunk_dims) >= 0),
             "Chunk size set");

        /* Add test filter to the pipeline */
        VRFY((set_dcpl_filter(plist_id, filter_id, NULL) >= 0), "Filter set");

        dset_id = H5Dcreate2(group_id, READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_NAME,
                             memtype, filespace, H5P_DEFAULT, plist_id, H5P_DEFAULT);
        VRFY((dset_id >= 0), "Dataset creation succeeded");

        /* Verify space allocation status */
        verify_space_alloc_status(dset_id, plist_id, DATASET_JUST_CREATED);

        VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

        VRFY((H5Dwrite(dset_id, memtype, H5S_ALL, H5S_ALL, H5P_DEFAULT, correct_buf) >= 0),
             "Dataset write succeeded");

        /* Verify space allocation status */
        verify_space_alloc_status(dset_id, plist_id, ALL_CHUNKS_WRITTEN);

        VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
        VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
        VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
        VRFY((H5Fclose(file_id) >= 0), "File close succeeded");
    }

    file_id = H5Fopen(filenames[0], H5F_ACC_RDONLY, fapl_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
    VRFY((group_id >= 0), "H5Gopen2 succeeded");

    dset_id =
        H5Dopen2(group_id, READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_NAME, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    sel_dims[0] = (hsize_t)READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_CH_NROWS / (hsize_t)mpi_size;
    sel_dims[1] = (hsize_t)READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_ENTRIES_PER_PROC;

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
    count[0]  = 1;
    count[1]  = (hsize_t)READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_ENTRIES_PER_PROC;
    stride[0] = (hsize_t)READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_CH_NROWS;
    stride[1] = READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_CH_NCOLS;
    block[0]  = (hsize_t)READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_CH_NROWS / (hsize_t)mpi_size;
    block[1]  = READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_CH_NCOLS;
    start[0]  = (hsize_t)mpi_rank;
    start[1]  = 0;

    if (VERBOSE_MED) {
        HDprintf("Process %d is reading with count[ %" PRIuHSIZE ", %" PRIuHSIZE " ], stride[ %" PRIuHSIZE
                 ", %" PRIuHSIZE " ], start[ %" PRIuHSIZE ", %" PRIuHSIZE " ], block size[ %" PRIuHSIZE
                 ", %" PRIuHSIZE " ]\n",
                 mpi_rank, count[0], count[1], stride[0], stride[1], start[0], start[1], block[0], block[1]);
        HDfflush(stdout);
    }

    VRFY((H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, stride, count, block) >= 0),
         "Hyperslab selection succeeded");

    read_buf_size = flat_dims[0] * sizeof(*read_buf);

    read_buf = (COMPOUND_C_DATATYPE *)HDcalloc(1, read_buf_size);
    VRFY((NULL != read_buf), "HDcalloc succeeded");

    VRFY((H5Dread(dset_id, memtype, memspace, filespace, dxpl_id, read_buf) >= 0), "Dataset read succeeded");

    global_buf = (COMPOUND_C_DATATYPE *)HDcalloc(1, correct_buf_size);
    VRFY((NULL != global_buf), "HDcalloc succeeded");

    /* Collect each piece of data from all ranks into a global buffer on all ranks */
    recvcounts = (int *)HDcalloc(1, (size_t)mpi_size * sizeof(*recvcounts));
    VRFY((NULL != recvcounts), "HDcalloc succeeded");

    for (i = 0; i < (size_t)mpi_size; i++)
        recvcounts[i] = (int)(flat_dims[0] * sizeof(*read_buf));

    displs = (int *)HDcalloc(1, (size_t)mpi_size * sizeof(*displs));
    VRFY((NULL != displs), "HDcalloc succeeded");

    for (i = 0; i < (size_t)mpi_size; i++)
        displs[i] = (int)(i * flat_dims[0] * sizeof(*read_buf));

    VRFY((MPI_SUCCESS == MPI_Allgatherv(read_buf, (int)(flat_dims[0] * sizeof(COMPOUND_C_DATATYPE)), MPI_BYTE,
                                        global_buf, recvcounts, displs, MPI_BYTE, comm)),
         "MPI_Allgatherv succeeded");

    VRFY((0 == HDmemcmp(global_buf, correct_buf, correct_buf_size)), "Data verification succeeded");

    if (displs)
        HDfree(displs);
    if (recvcounts)
        HDfree(recvcounts);
    if (global_buf)
        HDfree(global_buf);
    if (read_buf)
        HDfree(read_buf);
    if (correct_buf)
        HDfree(correct_buf);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Sclose(memspace) >= 0), "Memory dataspace close succeeded");
    VRFY((H5Tclose(memtype) >= 0), "Memory datatype close succeeded");
    VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
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
test_read_cmpd_filtered_dataset_type_conversion_unshared(const char *parent_group, H5Z_filter_t filter_id,
                                                         hid_t fapl_id, hid_t dcpl_id, hid_t dxpl_id)
{
    COMPOUND_C_DATATYPE *read_buf    = NULL;
    COMPOUND_C_DATATYPE *correct_buf = NULL;
    COMPOUND_C_DATATYPE *global_buf  = NULL;
    hsize_t              dataset_dims[READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t              chunk_dims[READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t              sel_dims[READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t              start[READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t              stride[READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t              count[READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t              block[READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t              flat_dims[1];
    size_t               i, read_buf_size, correct_buf_size;
    hid_t                file_id = H5I_INVALID_HID, dset_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID;
    hid_t                filetype = H5I_INVALID_HID, memtype = H5I_INVALID_HID;
    hid_t                group_id  = H5I_INVALID_HID;
    hid_t                filespace = H5I_INVALID_HID, memspace = H5I_INVALID_HID;
    int                 *recvcounts = NULL;
    int                 *displs     = NULL;

    if (MAINPROCESS)
        HDputs("Testing read from unshared filtered chunks in Compound Datatype dataset with Datatype "
               "conversion");

    /* SZIP and ScaleOffset filters don't support compound types */
    if (filter_id == H5Z_FILTER_SZIP || filter_id == H5Z_FILTER_SCALEOFFSET) {
        if (MAINPROCESS)
            SKIPPED();
        return;
    }

    dataset_dims[0] = (hsize_t)READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_NROWS;
    dataset_dims[1] = (hsize_t)READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_NCOLS;

    /* Setup the buffer for writing and for comparison */
    correct_buf_size = dataset_dims[0] * dataset_dims[1] * sizeof(*correct_buf);

    correct_buf = (COMPOUND_C_DATATYPE *)HDcalloc(1, correct_buf_size);
    VRFY((NULL != correct_buf), "HDcalloc succeeded");

    for (i = 0; i < correct_buf_size / sizeof(*correct_buf); i++) {
        correct_buf[i].field1 = (short)((i % dataset_dims[1]) + (i / dataset_dims[1]));

        correct_buf[i].field2 = (int)((i % dataset_dims[1]) + (i / dataset_dims[1]));

        correct_buf[i].field3 = (long)((i % dataset_dims[1]) + (i / dataset_dims[1]));
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

    VRFY((H5Tinsert(filetype, "ShortData", 0, H5T_STD_I64BE) >= 0), "Datatype insertion succeeded");
    VRFY((H5Tinsert(filetype, "IntData", 8, H5T_STD_I64BE) >= 0), "Datatype insertion succeeded");
    VRFY((H5Tinsert(filetype, "LongData", 16, H5T_STD_I64BE) >= 0), "Datatype insertion succeeded");

    if (MAINPROCESS) {
        plist_id = H5Pcreate(H5P_FILE_ACCESS);
        VRFY((plist_id >= 0), "FAPL creation succeeded");

        VRFY((H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0),
             "Set libver bounds succeeded");

        file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, plist_id);
        VRFY((file_id >= 0), "Test file open succeeded");

        VRFY((H5Pclose(plist_id) >= 0), "FAPL close succeeded");

        group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
        VRFY((group_id >= 0), "H5Gopen2 succeeded");

        /* Create the dataspace for the dataset */
        filespace = H5Screate_simple(READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_DIMS,
                                     dataset_dims, NULL);
        VRFY((filespace >= 0), "File dataspace creation succeeded");

        /* Create chunked dataset */
        chunk_dims[0] = READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_CH_NROWS;
        chunk_dims[1] = READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_CH_NCOLS;

        plist_id = H5Pcopy(dcpl_id);
        VRFY((plist_id >= 0), "DCPL copy succeeded");

        VRFY((H5Pset_chunk(plist_id, READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_DIMS,
                           chunk_dims) >= 0),
             "Chunk size set");

        /* Add test filter to the pipeline */
        VRFY((set_dcpl_filter(plist_id, filter_id, NULL) >= 0), "Filter set");

        dset_id = H5Dcreate2(group_id, READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_NAME,
                             filetype, filespace, H5P_DEFAULT, plist_id, H5P_DEFAULT);
        VRFY((dset_id >= 0), "Dataset creation succeeded");

        /* Verify space allocation status */
        verify_space_alloc_status(dset_id, plist_id, DATASET_JUST_CREATED);

        VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

        VRFY((H5Dwrite(dset_id, memtype, H5S_ALL, H5S_ALL, H5P_DEFAULT, correct_buf) >= 0),
             "Dataset write succeeded");

        /* Verify space allocation status */
        verify_space_alloc_status(dset_id, plist_id, ALL_CHUNKS_WRITTEN);

        VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
        VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
        VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
        VRFY((H5Fclose(file_id) >= 0), "File close succeeded");
    }

    file_id = H5Fopen(filenames[0], H5F_ACC_RDONLY, fapl_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
    VRFY((group_id >= 0), "H5Gopen2 succeeded");

    dset_id =
        H5Dopen2(group_id, READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_NAME, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    sel_dims[0] = (hsize_t)READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_CH_NROWS;
    sel_dims[1] = (hsize_t)READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_ENTRIES_PER_PROC;

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
    count[0]  = 1;
    count[1]  = (hsize_t)READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_ENTRIES_PER_PROC;
    stride[0] = READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_CH_NROWS;
    stride[1] = READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_CH_NCOLS;
    block[0]  = READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_CH_NROWS;
    block[1]  = READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_CH_NCOLS;
    start[0]  = 0;
    start[1]  = ((hsize_t)mpi_rank * READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_CH_NCOLS);

    if (VERBOSE_MED) {
        HDprintf("Process %d is reading with count[ %" PRIuHSIZE ", %" PRIuHSIZE " ], stride[ %" PRIuHSIZE
                 ", %" PRIuHSIZE " ], start[ %" PRIuHSIZE ", %" PRIuHSIZE " ], block size[ %" PRIuHSIZE
                 ", %" PRIuHSIZE " ]\n",
                 mpi_rank, count[0], count[1], stride[0], stride[1], start[0], start[1], block[0], block[1]);
        HDfflush(stdout);
    }

    VRFY((H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, stride, count, block) >= 0),
         "Hyperslab selection succeeded");

    read_buf_size = flat_dims[0] * sizeof(*read_buf);

    read_buf = (COMPOUND_C_DATATYPE *)HDcalloc(1, read_buf_size);
    VRFY((NULL != read_buf), "HDcalloc succeeded");

    VRFY((H5Dread(dset_id, memtype, memspace, filespace, dxpl_id, read_buf) >= 0), "Dataset read succeeded");

    global_buf = (COMPOUND_C_DATATYPE *)HDcalloc(1, correct_buf_size);
    VRFY((NULL != global_buf), "HDcalloc succeeded");

    /* Collect each piece of data from all ranks into a global buffer on all ranks */
    recvcounts = (int *)HDcalloc(1, (size_t)mpi_size * sizeof(*recvcounts));
    VRFY((NULL != recvcounts), "HDcalloc succeeded");

    for (i = 0; i < (size_t)mpi_size; i++)
        recvcounts[i] = (int)(flat_dims[0] * sizeof(*read_buf));

    displs = (int *)HDcalloc(1, (size_t)mpi_size * sizeof(*displs));
    VRFY((NULL != displs), "HDcalloc succeeded");

    for (i = 0; i < (size_t)mpi_size; i++)
        displs[i] = (int)(i * flat_dims[0] * sizeof(*read_buf));

    VRFY((MPI_SUCCESS == MPI_Allgatherv(read_buf, (int)(flat_dims[0] * sizeof(COMPOUND_C_DATATYPE)), MPI_BYTE,
                                        global_buf, recvcounts, displs, MPI_BYTE, comm)),
         "MPI_Allgatherv succeeded");

    VRFY((0 == HDmemcmp(global_buf, correct_buf, correct_buf_size)), "Data verification succeeded");

    if (displs)
        HDfree(displs);
    if (recvcounts)
        HDfree(recvcounts);
    if (global_buf)
        HDfree(global_buf);
    if (read_buf)
        HDfree(read_buf);
    if (correct_buf)
        HDfree(correct_buf);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Sclose(memspace) >= 0), "Memory dataspace close succeeded");
    VRFY((H5Tclose(filetype) >= 0), "File datatype close succeeded");
    VRFY((H5Tclose(memtype) >= 0), "Memory datatype close succeeded");
    VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
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
test_read_cmpd_filtered_dataset_type_conversion_shared(const char *parent_group, H5Z_filter_t filter_id,
                                                       hid_t fapl_id, hid_t dcpl_id, hid_t dxpl_id)
{
    COMPOUND_C_DATATYPE *read_buf    = NULL;
    COMPOUND_C_DATATYPE *correct_buf = NULL;
    COMPOUND_C_DATATYPE *global_buf  = NULL;
    hsize_t              dataset_dims[READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t              chunk_dims[READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t              sel_dims[READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t              start[READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t              stride[READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t              count[READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t              block[READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t              flat_dims[1];
    size_t               i, read_buf_size, correct_buf_size;
    hid_t                file_id = H5I_INVALID_HID, dset_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID;
    hid_t                filetype = H5I_INVALID_HID, memtype = H5I_INVALID_HID;
    hid_t                group_id  = H5I_INVALID_HID;
    hid_t                filespace = H5I_INVALID_HID, memspace = H5I_INVALID_HID;
    int                 *recvcounts = NULL;
    int                 *displs     = NULL;

    if (MAINPROCESS)
        HDputs(
            "Testing read from shared filtered chunks in Compound Datatype dataset with Datatype conversion");

    /* SZIP and ScaleOffset filters don't support compound types */
    if (filter_id == H5Z_FILTER_SZIP || filter_id == H5Z_FILTER_SCALEOFFSET) {
        if (MAINPROCESS)
            SKIPPED();
        return;
    }

    dataset_dims[0] = (hsize_t)READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_NROWS;
    dataset_dims[1] = (hsize_t)READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_NCOLS;

    /* Setup the buffer for writing and for comparison */
    correct_buf_size = dataset_dims[0] * dataset_dims[1] * sizeof(*correct_buf);

    correct_buf = (COMPOUND_C_DATATYPE *)HDcalloc(1, correct_buf_size);
    VRFY((NULL != correct_buf), "HDcalloc succeeded");

    for (i = 0; i < correct_buf_size / sizeof(*correct_buf); i++) {
        correct_buf[i].field1 =
            (short)((dataset_dims[1] * (i / ((hsize_t)mpi_size * dataset_dims[1]))) + (i % dataset_dims[1]) +
                    (((i % ((hsize_t)mpi_size * dataset_dims[1])) / dataset_dims[1]) % dataset_dims[1]));

        correct_buf[i].field2 =
            (int)((dataset_dims[1] * (i / ((hsize_t)mpi_size * dataset_dims[1]))) + (i % dataset_dims[1]) +
                  (((i % ((hsize_t)mpi_size * dataset_dims[1])) / dataset_dims[1]) % dataset_dims[1]));

        correct_buf[i].field3 =
            (long)((dataset_dims[1] * (i / ((hsize_t)mpi_size * dataset_dims[1]))) + (i % dataset_dims[1]) +
                   (((i % ((hsize_t)mpi_size * dataset_dims[1])) / dataset_dims[1]) % dataset_dims[1]));
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

    VRFY((H5Tinsert(filetype, "ShortData", 0, H5T_STD_I64BE) >= 0), "Datatype insertion succeeded");
    VRFY((H5Tinsert(filetype, "IntData", 8, H5T_STD_I64BE) >= 0), "Datatype insertion succeeded");
    VRFY((H5Tinsert(filetype, "LongData", 16, H5T_STD_I64BE) >= 0), "Datatype insertion succeeded");

    if (MAINPROCESS) {
        plist_id = H5Pcreate(H5P_FILE_ACCESS);
        VRFY((plist_id >= 0), "FAPL creation succeeded");

        VRFY((H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0),
             "Set libver bounds succeeded");

        file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, plist_id);
        VRFY((file_id >= 0), "Test file open succeeded");

        VRFY((H5Pclose(plist_id) >= 0), "FAPL close succeeded");

        group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
        VRFY((group_id >= 0), "H5Gopen2 succeeded");

        /* Create the dataspace for the dataset */
        filespace = H5Screate_simple(READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_DIMS,
                                     dataset_dims, NULL);
        VRFY((filespace >= 0), "File dataspace creation succeeded");

        /* Create chunked dataset */
        chunk_dims[0] = (hsize_t)READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_CH_NROWS;
        chunk_dims[1] = (hsize_t)READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_CH_NCOLS;

        plist_id = H5Pcopy(dcpl_id);
        VRFY((plist_id >= 0), "DCPL copy succeeded");

        VRFY((H5Pset_chunk(plist_id, READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_DIMS,
                           chunk_dims) >= 0),
             "Chunk size set");

        /* Add test filter to the pipeline */
        VRFY((set_dcpl_filter(plist_id, filter_id, NULL) >= 0), "Filter set");

        dset_id = H5Dcreate2(group_id, READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_NAME,
                             filetype, filespace, H5P_DEFAULT, plist_id, H5P_DEFAULT);
        VRFY((dset_id >= 0), "Dataset creation succeeded");

        /* Verify space allocation status */
        verify_space_alloc_status(dset_id, plist_id, DATASET_JUST_CREATED);

        VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

        VRFY((H5Dwrite(dset_id, memtype, H5S_ALL, H5S_ALL, H5P_DEFAULT, correct_buf) >= 0),
             "Dataset write succeeded");

        /* Verify space allocation status */
        verify_space_alloc_status(dset_id, plist_id, ALL_CHUNKS_WRITTEN);

        VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
        VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
        VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
        VRFY((H5Fclose(file_id) >= 0), "File close succeeded");
    }

    file_id = H5Fopen(filenames[0], H5F_ACC_RDONLY, fapl_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
    VRFY((group_id >= 0), "H5Gopen2 succeeded");

    dset_id =
        H5Dopen2(group_id, READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_NAME, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    sel_dims[0] = (hsize_t)READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_CH_NROWS / (hsize_t)mpi_size;
    sel_dims[1] = (hsize_t)READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_ENTRIES_PER_PROC;

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
    count[0]  = 1;
    count[1]  = (hsize_t)READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_ENTRIES_PER_PROC;
    stride[0] = (hsize_t)READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_CH_NROWS;
    stride[1] = READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_CH_NCOLS;
    block[0]  = (hsize_t)READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_CH_NROWS / (hsize_t)mpi_size;
    block[1]  = READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_CH_NCOLS;
    start[0]  = (hsize_t)mpi_rank;
    start[1]  = 0;

    if (VERBOSE_MED) {
        HDprintf("Process %d is reading with count[ %" PRIuHSIZE ", %" PRIuHSIZE " ], stride[ %" PRIuHSIZE
                 ", %" PRIuHSIZE " ], start[ %" PRIuHSIZE ", %" PRIuHSIZE " ], block size[ %" PRIuHSIZE
                 ", %" PRIuHSIZE " ]\n",
                 mpi_rank, count[0], count[1], stride[0], stride[1], start[0], start[1], block[0], block[1]);
        HDfflush(stdout);
    }

    VRFY((H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, stride, count, block) >= 0),
         "Hyperslab selection succeeded");

    read_buf_size = flat_dims[0] * sizeof(*read_buf);

    read_buf = (COMPOUND_C_DATATYPE *)HDcalloc(1, read_buf_size);
    VRFY((NULL != read_buf), "HDcalloc succeeded");

    VRFY((H5Dread(dset_id, memtype, memspace, filespace, dxpl_id, read_buf) >= 0), "Dataset read succeeded");

    global_buf = (COMPOUND_C_DATATYPE *)HDcalloc(1, correct_buf_size);
    VRFY((NULL != global_buf), "HDcalloc succeeded");

    /* Collect each piece of data from all ranks into a global buffer on all ranks */
    recvcounts = (int *)HDcalloc(1, (size_t)mpi_size * sizeof(*recvcounts));
    VRFY((NULL != recvcounts), "HDcalloc succeeded");

    for (i = 0; i < (size_t)mpi_size; i++)
        recvcounts[i] = (int)(flat_dims[0] * sizeof(*read_buf));

    displs = (int *)HDcalloc(1, (size_t)mpi_size * sizeof(*displs));
    VRFY((NULL != displs), "HDcalloc succeeded");

    for (i = 0; i < (size_t)mpi_size; i++)
        displs[i] = (int)(i * flat_dims[0] * sizeof(*read_buf));

    VRFY((MPI_SUCCESS == MPI_Allgatherv(read_buf, (int)(flat_dims[0] * sizeof(COMPOUND_C_DATATYPE)), MPI_BYTE,
                                        global_buf, recvcounts, displs, MPI_BYTE, comm)),
         "MPI_Allgatherv succeeded");

    VRFY((0 == HDmemcmp(global_buf, correct_buf, correct_buf_size)), "Data verification succeeded");

    if (displs)
        HDfree(displs);
    if (recvcounts)
        HDfree(recvcounts);
    if (global_buf)
        HDfree(global_buf);
    if (read_buf)
        HDfree(read_buf);
    if (correct_buf)
        HDfree(correct_buf);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Sclose(memspace) >= 0), "Memory dataspace close succeeded");
    VRFY((H5Tclose(memtype) >= 0), "Memory datatype close succeeded");
    VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
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
test_write_serial_read_parallel(const char *parent_group, H5Z_filter_t filter_id, hid_t fapl_id,
                                hid_t dcpl_id, hid_t dxpl_id)
{
    C_DATATYPE *data        = NULL;
    C_DATATYPE *read_buf    = NULL;
    C_DATATYPE *correct_buf = NULL;
    hsize_t     dataset_dims[WRITE_SERIAL_READ_PARALLEL_DATASET_DIMS];
    hsize_t     chunk_dims[WRITE_SERIAL_READ_PARALLEL_DATASET_DIMS];
    size_t      i, data_size, correct_buf_size;
    hid_t       file_id = H5I_INVALID_HID, dset_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID;
    hid_t       group_id  = H5I_INVALID_HID;
    hid_t       filespace = H5I_INVALID_HID;

    if (MAINPROCESS)
        HDputs("Testing write file serially; read file in parallel");

    dataset_dims[0] = (hsize_t)WRITE_SERIAL_READ_PARALLEL_NROWS;
    dataset_dims[1] = (hsize_t)WRITE_SERIAL_READ_PARALLEL_NCOLS;
    dataset_dims[2] = (hsize_t)WRITE_SERIAL_READ_PARALLEL_DEPTH;

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

        group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
        VRFY((group_id >= 0), "H5Gopen2 succeeded");

        /* Create the dataspace for the dataset */
        chunk_dims[0] = (hsize_t)WRITE_SERIAL_READ_PARALLEL_CH_NROWS;
        chunk_dims[1] = (hsize_t)WRITE_SERIAL_READ_PARALLEL_CH_NCOLS;
        chunk_dims[2] = 1;

        filespace = H5Screate_simple(WRITE_SERIAL_READ_PARALLEL_DATASET_DIMS, dataset_dims, NULL);
        VRFY((filespace >= 0), "File dataspace creation succeeded");

        /* Create chunked dataset */
        plist_id = H5Pcopy(dcpl_id);
        VRFY((plist_id >= 0), "DCPL copy succeeded");

        VRFY((H5Pset_chunk(plist_id, WRITE_SERIAL_READ_PARALLEL_DATASET_DIMS, chunk_dims) >= 0),
             "Chunk size set");

        /* Add test filter to the pipeline */
        VRFY((set_dcpl_filter(plist_id, filter_id, NULL) >= 0), "Filter set");

        dset_id = H5Dcreate2(group_id, WRITE_SERIAL_READ_PARALLEL_DATASET_NAME, HDF5_DATATYPE_NAME, filespace,
                             H5P_DEFAULT, plist_id, H5P_DEFAULT);
        VRFY((dset_id >= 0), "Dataset creation succeeded");

        /* Verify space allocation status */
        verify_space_alloc_status(dset_id, plist_id, DATASET_JUST_CREATED);

        VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

        data_size = dataset_dims[0] * dataset_dims[1] * dataset_dims[2] * sizeof(*data);

        data = (C_DATATYPE *)HDcalloc(1, data_size);
        VRFY((NULL != data), "HDcalloc succeeded");

        for (i = 0; i < data_size / sizeof(*data); i++)
            data[i] = (C_DATATYPE)GEN_DATA(i);

        VRFY((H5Dwrite(dset_id, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, H5P_DEFAULT, data) >= 0),
             "Dataset write succeeded");

        /* Verify space allocation status */
        verify_space_alloc_status(dset_id, plist_id, ALL_CHUNKS_WRITTEN);

        if (data)
            HDfree(data);

        VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
        VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
        VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
        VRFY((H5Fclose(file_id) >= 0), "File close succeeded");
    }

    correct_buf_size = dataset_dims[0] * dataset_dims[1] * dataset_dims[2] * sizeof(*correct_buf);

    correct_buf = (C_DATATYPE *)HDcalloc(1, correct_buf_size);
    VRFY((NULL != correct_buf), "HDcalloc succeeded");

    read_buf = (C_DATATYPE *)HDcalloc(1, correct_buf_size);
    VRFY((NULL != read_buf), "HDcalloc succeeded");

    for (i = 0; i < correct_buf_size / sizeof(*correct_buf); i++)
        correct_buf[i] = (long)i;

    /* All ranks open the file and verify their "portion" of the dataset is correct */
    file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, fapl_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
    VRFY((group_id >= 0), "H5Gopen2 succeeded");

    dset_id = H5Dopen2(group_id, WRITE_SERIAL_READ_PARALLEL_DATASET_NAME, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    VRFY((H5Dread(dset_id, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, dxpl_id, read_buf) >= 0),
         "Dataset read succeeded");

    VRFY((0 == HDmemcmp(read_buf, correct_buf, correct_buf_size)), "Data verification succeeded");

    if (correct_buf)
        HDfree(correct_buf);
    if (read_buf)
        HDfree(read_buf);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
    VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    return;
}

#ifdef H5_HAVE_PARALLEL_FILTERED_WRITES
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
test_write_parallel_read_serial(const char *parent_group, H5Z_filter_t filter_id, hid_t fapl_id,
                                hid_t dcpl_id, hid_t dxpl_id)
{
    C_DATATYPE *data        = NULL;
    C_DATATYPE *read_buf    = NULL;
    C_DATATYPE *correct_buf = NULL;
    hsize_t     dataset_dims[WRITE_PARALLEL_READ_SERIAL_DATASET_DIMS];
    hsize_t     chunk_dims[WRITE_PARALLEL_READ_SERIAL_DATASET_DIMS];
    hsize_t     sel_dims[WRITE_PARALLEL_READ_SERIAL_DATASET_DIMS];
    hsize_t     count[WRITE_PARALLEL_READ_SERIAL_DATASET_DIMS];
    hsize_t     stride[WRITE_PARALLEL_READ_SERIAL_DATASET_DIMS];
    hsize_t     block[WRITE_PARALLEL_READ_SERIAL_DATASET_DIMS];
    hsize_t     offset[WRITE_PARALLEL_READ_SERIAL_DATASET_DIMS];
    size_t      i, data_size, correct_buf_size;
    hid_t       file_id = H5I_INVALID_HID, dset_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID;
    hid_t       group_id  = H5I_INVALID_HID;
    hid_t       filespace = H5I_INVALID_HID, memspace = H5I_INVALID_HID;

    if (MAINPROCESS)
        HDputs("Testing write file in parallel; read serially");

    file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, fapl_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
    VRFY((group_id >= 0), "H5Gopen2 succeeded");

    /* Create the dataspace for the dataset */
    dataset_dims[0] = (hsize_t)WRITE_PARALLEL_READ_SERIAL_NROWS;
    dataset_dims[1] = (hsize_t)WRITE_PARALLEL_READ_SERIAL_NCOLS;
    dataset_dims[2] = (hsize_t)WRITE_PARALLEL_READ_SERIAL_DEPTH;
    chunk_dims[0]   = (hsize_t)WRITE_PARALLEL_READ_SERIAL_CH_NROWS;
    chunk_dims[1]   = (hsize_t)WRITE_PARALLEL_READ_SERIAL_CH_NCOLS;
    chunk_dims[2]   = 1;
    sel_dims[0]     = (hsize_t)WRITE_PARALLEL_READ_SERIAL_CH_NROWS;
    sel_dims[1]     = (hsize_t)WRITE_PARALLEL_READ_SERIAL_NCOLS;
    sel_dims[2]     = (hsize_t)WRITE_PARALLEL_READ_SERIAL_DEPTH;

    filespace = H5Screate_simple(WRITE_PARALLEL_READ_SERIAL_DATASET_DIMS, dataset_dims, NULL);
    VRFY((filespace >= 0), "File dataspace creation succeeded");

    memspace = H5Screate_simple(WRITE_PARALLEL_READ_SERIAL_DATASET_DIMS, sel_dims, NULL);
    VRFY((memspace >= 0), "Memory dataspace creation succeeded");

    /* Create chunked dataset */
    plist_id = H5Pcopy(dcpl_id);
    VRFY((plist_id >= 0), "DCPL copy succeeded");

    VRFY((H5Pset_chunk(plist_id, WRITE_PARALLEL_READ_SERIAL_DATASET_DIMS, chunk_dims) >= 0),
         "Chunk size set");

    /* Add test filter to the pipeline */
    VRFY((set_dcpl_filter(plist_id, filter_id, NULL) >= 0), "Filter set");

    dset_id = H5Dcreate2(group_id, WRITE_PARALLEL_READ_SERIAL_DATASET_NAME, HDF5_DATATYPE_NAME, filespace,
                         H5P_DEFAULT, plist_id, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset creation succeeded");

    /* Verify space allocation status */
    verify_space_alloc_status(dset_id, plist_id, DATASET_JUST_CREATED);

    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

    /* Each process defines the dataset selection in memory and writes
     * it to the hyperslab in the file
     */
    count[0]  = 1;
    count[1]  = (hsize_t)WRITE_PARALLEL_READ_SERIAL_NCOLS / (hsize_t)WRITE_PARALLEL_READ_SERIAL_CH_NCOLS;
    count[2]  = (hsize_t)mpi_size;
    stride[0] = (hsize_t)WRITE_PARALLEL_READ_SERIAL_CH_NROWS;
    stride[1] = (hsize_t)WRITE_PARALLEL_READ_SERIAL_CH_NCOLS;
    stride[2] = 1;
    block[0]  = (hsize_t)WRITE_PARALLEL_READ_SERIAL_CH_NROWS;
    block[1]  = (hsize_t)WRITE_PARALLEL_READ_SERIAL_CH_NCOLS;
    block[2]  = 1;
    offset[0] = ((hsize_t)mpi_rank * (hsize_t)WRITE_PARALLEL_READ_SERIAL_CH_NROWS * count[0]);
    offset[1] = 0;
    offset[2] = 0;

    if (VERBOSE_MED) {
        HDprintf("Process %d is writing with count[ %" PRIuHSIZE ", %" PRIuHSIZE ", %" PRIuHSIZE
                 " ], stride[ %" PRIuHSIZE ", %" PRIuHSIZE ", %" PRIuHSIZE " ], offset[ %" PRIuHSIZE
                 ", %" PRIuHSIZE ", %" PRIuHSIZE " ], block size[ %" PRIuHSIZE ", %" PRIuHSIZE ", %" PRIuHSIZE
                 " ]\n",
                 mpi_rank, count[0], count[1], count[2], stride[0], stride[1], stride[2], offset[0],
                 offset[1], offset[2], block[0], block[1], block[2]);
        HDfflush(stdout);
    }

    /* Select hyperslab in the file */
    filespace = H5Dget_space(dset_id);
    VRFY((filespace >= 0), "File dataspace retrieval succeeded");

    VRFY((H5Sselect_hyperslab(filespace, H5S_SELECT_SET, offset, stride, count, block) >= 0),
         "Hyperslab selection succeeded");

    /* Fill data buffer */
    data_size = sel_dims[0] * sel_dims[1] * sel_dims[2] * sizeof(*data);

    data = (C_DATATYPE *)HDcalloc(1, data_size);
    VRFY((NULL != data), "HDcalloc succeeded");

    for (i = 0; i < data_size / sizeof(*data); i++)
        data[i] = (C_DATATYPE)GEN_DATA(i);

    VRFY((H5Dwrite(dset_id, HDF5_DATATYPE_NAME, memspace, filespace, dxpl_id, data) >= 0),
         "Dataset write succeeded");

    /* Verify space allocation status */
    verify_space_alloc_status(dset_id, plist_id, ALL_CHUNKS_WRITTEN);

    if (data)
        HDfree(data);

    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Sclose(memspace) >= 0), "Memory dataspace close succeeded");
    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
    VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    if (MAINPROCESS) {
        plist_id = H5Pcreate(H5P_FILE_ACCESS);
        VRFY((plist_id >= 0), "FAPL creation succeeded");

        VRFY((H5Pset_libver_bounds(plist_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0),
             "Set libver bounds succeeded");

        file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, plist_id);
        VRFY((file_id >= 0), "Test file open succeeded");

        VRFY((H5Pclose(plist_id) >= 0), "FAPL close succeeded");

        group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
        VRFY((group_id >= 0), "H5Gopen2 succeeded");

        dset_id = H5Dopen2(group_id, WRITE_PARALLEL_READ_SERIAL_DATASET_NAME, H5P_DEFAULT);
        VRFY((dset_id >= 0), "Dataset open succeeded");

        correct_buf_size = dataset_dims[0] * dataset_dims[1] * dataset_dims[2] * sizeof(*correct_buf);

        correct_buf = (C_DATATYPE *)HDcalloc(1, correct_buf_size);
        VRFY((NULL != correct_buf), "HDcalloc succeeded");

        read_buf = (C_DATATYPE *)HDcalloc(1, correct_buf_size);
        VRFY((NULL != read_buf), "HDcalloc succeeded");

        for (i = 0; i < correct_buf_size / sizeof(*correct_buf); i++)
            correct_buf[i] = (C_DATATYPE)((i % (dataset_dims[0] * dataset_dims[1])) +
                                          (i / (dataset_dims[0] * dataset_dims[1])));

        VRFY((H5Dread(dset_id, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, H5P_DEFAULT, read_buf) >= 0),
             "Dataset read succeeded");

        VRFY((0 == HDmemcmp(read_buf, correct_buf, correct_buf_size)), "Data verification succeeded");

        VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
        VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
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
test_shrinking_growing_chunks(const char *parent_group, H5Z_filter_t filter_id, hid_t fapl_id, hid_t dcpl_id,
                              hid_t dxpl_id)
{
    double *data     = NULL;
    double *read_buf = NULL;
    hsize_t dataset_dims[SHRINKING_GROWING_CHUNKS_DATASET_DIMS];
    hsize_t chunk_dims[SHRINKING_GROWING_CHUNKS_DATASET_DIMS];
    hsize_t sel_dims[SHRINKING_GROWING_CHUNKS_DATASET_DIMS];
    hsize_t start[SHRINKING_GROWING_CHUNKS_DATASET_DIMS];
    hsize_t stride[SHRINKING_GROWING_CHUNKS_DATASET_DIMS];
    hsize_t count[SHRINKING_GROWING_CHUNKS_DATASET_DIMS];
    hsize_t block[SHRINKING_GROWING_CHUNKS_DATASET_DIMS];
    size_t  i, data_size;
    hid_t   file_id = H5I_INVALID_HID, dset_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID;
    hid_t   group_id  = H5I_INVALID_HID;
    hid_t   filespace = H5I_INVALID_HID, memspace = H5I_INVALID_HID;

    if (MAINPROCESS)
        HDputs("Testing continually shrinking/growing chunks");

    file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, fapl_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
    VRFY((group_id >= 0), "H5Gopen2 succeeded");

    /* Create the dataspace for the dataset */
    dataset_dims[0] = (hsize_t)SHRINKING_GROWING_CHUNKS_NROWS;
    dataset_dims[1] = (hsize_t)SHRINKING_GROWING_CHUNKS_NCOLS;
    chunk_dims[0]   = (hsize_t)SHRINKING_GROWING_CHUNKS_CH_NROWS;
    chunk_dims[1]   = (hsize_t)SHRINKING_GROWING_CHUNKS_CH_NCOLS;
    sel_dims[0]     = (hsize_t)SHRINKING_GROWING_CHUNKS_CH_NROWS;
    sel_dims[1]     = (hsize_t)SHRINKING_GROWING_CHUNKS_NCOLS;

    filespace = H5Screate_simple(SHRINKING_GROWING_CHUNKS_DATASET_DIMS, dataset_dims, NULL);
    VRFY((filespace >= 0), "File dataspace creation succeeded");

    memspace = H5Screate_simple(SHRINKING_GROWING_CHUNKS_DATASET_DIMS, sel_dims, NULL);
    VRFY((memspace >= 0), "Memory dataspace creation succeeded");

    /* Create chunked dataset */
    plist_id = H5Pcopy(dcpl_id);
    VRFY((plist_id >= 0), "DCPL copy succeeded");

    VRFY((H5Pset_chunk(plist_id, SHRINKING_GROWING_CHUNKS_DATASET_DIMS, chunk_dims) >= 0), "Chunk size set");

    /* Add test filter to the pipeline */
    VRFY((set_dcpl_filter(plist_id, filter_id, NULL) >= 0), "Filter set");

    dset_id = H5Dcreate2(group_id, SHRINKING_GROWING_CHUNKS_DATASET_NAME, H5T_NATIVE_DOUBLE, filespace,
                         H5P_DEFAULT, plist_id, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset creation succeeded");

    /* Verify space allocation status */
    verify_space_alloc_status(dset_id, plist_id, DATASET_JUST_CREATED);

    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

    /*
     * Each process defines the dataset selection in memory and writes
     * it to the hyperslab in the file
     */
    count[0]  = 1;
    count[1]  = (hsize_t)SHRINKING_GROWING_CHUNKS_NCOLS / (hsize_t)SHRINKING_GROWING_CHUNKS_CH_NCOLS;
    stride[0] = (hsize_t)SHRINKING_GROWING_CHUNKS_CH_NROWS;
    stride[1] = (hsize_t)SHRINKING_GROWING_CHUNKS_CH_NCOLS;
    block[0]  = (hsize_t)SHRINKING_GROWING_CHUNKS_CH_NROWS;
    block[1]  = (hsize_t)SHRINKING_GROWING_CHUNKS_CH_NCOLS;
    start[0]  = ((hsize_t)mpi_rank * (hsize_t)SHRINKING_GROWING_CHUNKS_CH_NROWS * count[0]);
    start[1]  = 0;

    if (VERBOSE_MED) {
        HDprintf("Process %d is writing with count[ %" PRIuHSIZE ", %" PRIuHSIZE " ], stride[ %" PRIuHSIZE
                 ", %" PRIuHSIZE " ], start[ %" PRIuHSIZE ", %" PRIuHSIZE " ], block size[ %" PRIuHSIZE
                 ", %" PRIuHSIZE " ]\n",
                 mpi_rank, count[0], count[1], stride[0], stride[1], start[0], start[1], block[0], block[1]);
        HDfflush(stdout);
    }

    /* Select hyperslab in the file */
    filespace = H5Dget_space(dset_id);
    VRFY((dset_id >= 0), "File dataspace retrieval succeeded");

    VRFY((H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, stride, count, block) >= 0),
         "Hyperslab selection succeeded");

    data_size = sel_dims[0] * sel_dims[1] * sizeof(double);

    data = (double *)HDcalloc(1, data_size);
    VRFY((NULL != data), "HDcalloc succeeded");

    read_buf = (double *)HDcalloc(1, data_size);
    VRFY((NULL != read_buf), "HDcalloc succeeded");

    for (i = 0; i < SHRINKING_GROWING_CHUNKS_NLOOPS; i++) {
        /* Continually write random float data, followed by zeroed-out data */
        if (i % 2)
            HDmemset(data, 0, data_size);
        else {
            size_t j;
            for (j = 0; j < data_size / sizeof(*data); j++) {
                data[j] = (rand() / (double)(RAND_MAX / (double)1.0L));
            }
        }

        VRFY((H5Dwrite(dset_id, H5T_NATIVE_DOUBLE, memspace, filespace, dxpl_id, data) >= 0),
             "Dataset write succeeded");

        /* Verify space allocation status */
        verify_space_alloc_status(dset_id, plist_id, ALL_CHUNKS_WRITTEN);

        if (i % 2) {
            HDmemset(read_buf, 255, data_size);
        }
        else {
            HDmemset(read_buf, 0, data_size);
        }

        VRFY((H5Dread(dset_id, H5T_NATIVE_DOUBLE, memspace, filespace, dxpl_id, read_buf) >= 0),
             "Dataset read succeeded");

        VRFY((0 == HDmemcmp(read_buf, data, data_size)), "data verification succeeded");
    }

    if (read_buf)
        HDfree(read_buf);
    if (data)
        HDfree(data);

    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Sclose(memspace) >= 0), "Memory dataspace close succeeded");
    VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    return;
}

/*
 * Tests that filtered and unfiltered partial edge chunks can be
 * written to and read from correctly in parallel when only one MPI
 * rank writes to a particular partial edge chunk in the dataset.
 *
 * The dataset contains partial edge chunks in the second dimension.
 * Each MPI rank selects a hyperslab in the shape of a single chunk
 * that is offset to cover the whole edge chunk and part of the
 * full chunk next to the edge chunk.
 */
static void
test_edge_chunks_no_overlap(const char *parent_group, H5Z_filter_t filter_id, hid_t fapl_id, hid_t dcpl_id,
                            hid_t dxpl_id)
{
    C_DATATYPE *data     = NULL;
    C_DATATYPE *read_buf = NULL;
    hsize_t     dataset_dims[WRITE_UNSHARED_FILTERED_EDGE_CHUNKS_DATASET_DIMS];
    hsize_t     chunk_dims[WRITE_UNSHARED_FILTERED_EDGE_CHUNKS_DATASET_DIMS];
    hsize_t     sel_dims[WRITE_UNSHARED_FILTERED_EDGE_CHUNKS_DATASET_DIMS];
    hsize_t     start[WRITE_UNSHARED_FILTERED_EDGE_CHUNKS_DATASET_DIMS];
    hsize_t     stride[WRITE_UNSHARED_FILTERED_EDGE_CHUNKS_DATASET_DIMS];
    hsize_t     count[WRITE_UNSHARED_FILTERED_EDGE_CHUNKS_DATASET_DIMS];
    hsize_t     block[WRITE_UNSHARED_FILTERED_EDGE_CHUNKS_DATASET_DIMS];
    size_t      i, data_size;
    hid_t       file_id = H5I_INVALID_HID, dset_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID;
    hid_t       group_id  = H5I_INVALID_HID;
    hid_t       filespace = H5I_INVALID_HID;

    if (MAINPROCESS)
        HDputs("Testing write to unshared filtered edge chunks");

    file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, fapl_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
    VRFY((group_id >= 0), "H5Gopen2 succeeded");

    /* Create the dataspace for the dataset */
    dataset_dims[0] = (hsize_t)WRITE_UNSHARED_FILTERED_EDGE_CHUNKS_NROWS;
    dataset_dims[1] = (hsize_t)WRITE_UNSHARED_FILTERED_EDGE_CHUNKS_NCOLS;
    chunk_dims[0]   = (hsize_t)WRITE_UNSHARED_FILTERED_EDGE_CHUNKS_CH_NROWS;
    chunk_dims[1]   = (hsize_t)WRITE_UNSHARED_FILTERED_EDGE_CHUNKS_CH_NCOLS;
    sel_dims[0]     = (hsize_t)WRITE_UNSHARED_FILTERED_EDGE_CHUNKS_CH_NROWS;
    sel_dims[1]     = (hsize_t)WRITE_UNSHARED_FILTERED_EDGE_CHUNKS_CH_NCOLS;

    filespace = H5Screate_simple(WRITE_UNSHARED_FILTERED_EDGE_CHUNKS_DATASET_DIMS, dataset_dims, NULL);
    VRFY((filespace >= 0), "File dataspace creation succeeded");

    /* Create chunked dataset */
    plist_id = H5Pcopy(dcpl_id);
    VRFY((plist_id >= 0), "DCPL copy succeeded");

    VRFY((H5Pset_chunk(plist_id, WRITE_UNSHARED_FILTERED_EDGE_CHUNKS_DATASET_DIMS, chunk_dims) >= 0),
         "Chunk size set");

    /* Add test filter to the pipeline */
    VRFY((set_dcpl_filter(plist_id, filter_id, NULL) >= 0), "Filter set");

    dset_id = H5Dcreate2(group_id, WRITE_UNSHARED_FILTERED_EDGE_CHUNKS_DATASET_NAME, HDF5_DATATYPE_NAME,
                         filespace, H5P_DEFAULT, plist_id, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset creation succeeded");

    /* Verify space allocation status */
    verify_space_alloc_status(dset_id, plist_id, DATASET_JUST_CREATED);

    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

    /* Each process defines the dataset selection in memory and writes
     * it to the hyperslab in the file
     */
    count[0]  = 1;
    count[1]  = 1;
    stride[0] = (hsize_t)WRITE_UNSHARED_FILTERED_EDGE_CHUNKS_CH_NROWS;
    stride[1] = (hsize_t)WRITE_UNSHARED_FILTERED_EDGE_CHUNKS_CH_NCOLS;
    block[0]  = (hsize_t)WRITE_UNSHARED_FILTERED_EDGE_CHUNKS_CH_NROWS;
    block[1]  = (hsize_t)WRITE_UNSHARED_FILTERED_EDGE_CHUNKS_CH_NCOLS;
    start[0]  = ((hsize_t)mpi_rank * (hsize_t)WRITE_UNSHARED_FILTERED_EDGE_CHUNKS_CH_NROWS);
    start[1] =
        (hsize_t)(WRITE_UNSHARED_FILTERED_EDGE_CHUNKS_NCOLS - WRITE_UNSHARED_FILTERED_EDGE_CHUNKS_CH_NCOLS);

    if (VERBOSE_MED) {
        HDprintf("Process %d is writing with count[ %" PRIuHSIZE ", %" PRIuHSIZE " ], stride[ %" PRIuHSIZE
                 ", %" PRIuHSIZE " ], start[ %" PRIuHSIZE ", %" PRIuHSIZE " ], block size[ %" PRIuHSIZE
                 ", %" PRIuHSIZE " ]\n",
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

    data = (C_DATATYPE *)HDcalloc(1, data_size);
    VRFY((NULL != data), "HDcalloc succeeded");

    read_buf = (C_DATATYPE *)HDcalloc(1, data_size);
    VRFY((NULL != read_buf), "HDcalloc succeeded");

    for (i = 0; i < data_size / sizeof(*data); i++)
        data[i] = (C_DATATYPE)GEN_DATA(i);

    VRFY((H5Dwrite(dset_id, HDF5_DATATYPE_NAME, H5S_BLOCK, filespace, dxpl_id, data) >= 0),
         "Dataset write succeeded");

    /* Verify space allocation status */
    verify_space_alloc_status(dset_id, plist_id, (mpi_size > 1) ? SOME_CHUNKS_WRITTEN : ALL_CHUNKS_WRITTEN);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");

    /* Verify the correct data was written */
    dset_id = H5Dopen2(group_id, WRITE_UNSHARED_FILTERED_EDGE_CHUNKS_DATASET_NAME, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    VRFY((H5Dread(dset_id, HDF5_DATATYPE_NAME, H5S_BLOCK, filespace, dxpl_id, read_buf) >= 0),
         "Dataset read succeeded");

    VRFY((0 == HDmemcmp(read_buf, data, data_size)), "Data verification succeeded");

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");

    /* Repeat the previous, but set option to not filter partial edge chunks */
    if (MAINPROCESS)
        HDputs("Testing write to unshared unfiltered edge chunks");

    H5Pset_chunk_opts(plist_id, H5D_CHUNK_DONT_FILTER_PARTIAL_CHUNKS);

    dset_id = H5Dcreate2(group_id, WRITE_UNSHARED_FILTERED_EDGE_CHUNKS_DATASET_NAME2, HDF5_DATATYPE_NAME,
                         filespace, H5P_DEFAULT, plist_id, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset creation succeeded");

    /* Verify space allocation status */
    verify_space_alloc_status(dset_id, plist_id, DATASET_JUST_CREATED);

    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

    /* Each process defines the dataset selection in memory and writes
     * it to the hyperslab in the file
     */
    count[0]  = 1;
    count[1]  = 1;
    stride[0] = (hsize_t)WRITE_UNSHARED_FILTERED_EDGE_CHUNKS_CH_NROWS;
    stride[1] = (hsize_t)WRITE_UNSHARED_FILTERED_EDGE_CHUNKS_CH_NCOLS;
    block[0]  = (hsize_t)WRITE_UNSHARED_FILTERED_EDGE_CHUNKS_CH_NROWS;
    block[1]  = (hsize_t)WRITE_UNSHARED_FILTERED_EDGE_CHUNKS_CH_NCOLS;
    start[0]  = ((hsize_t)mpi_rank * (hsize_t)WRITE_UNSHARED_FILTERED_EDGE_CHUNKS_CH_NROWS);
    start[1] =
        (hsize_t)(WRITE_UNSHARED_FILTERED_EDGE_CHUNKS_NCOLS - WRITE_UNSHARED_FILTERED_EDGE_CHUNKS_CH_NCOLS);

    if (VERBOSE_MED) {
        HDprintf("Process %d is writing with count[ %" PRIuHSIZE ", %" PRIuHSIZE " ], stride[ %" PRIuHSIZE
                 ", %" PRIuHSIZE " ], start[ %" PRIuHSIZE ", %" PRIuHSIZE " ], block size[ %" PRIuHSIZE
                 ", %" PRIuHSIZE " ]\n",
                 mpi_rank, count[0], count[1], stride[0], stride[1], start[0], start[1], block[0], block[1]);
        HDfflush(stdout);
    }

    /* Select hyperslab in the file */
    filespace = H5Dget_space(dset_id);
    VRFY((filespace >= 0), "File dataspace retrieval succeeded");

    VRFY((H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, stride, count, block) >= 0),
         "Hyperslab selection succeeded");

    VRFY((H5Dwrite(dset_id, HDF5_DATATYPE_NAME, H5S_BLOCK, filespace, dxpl_id, data) >= 0),
         "Dataset write succeeded");

    /* Verify space allocation status */
    verify_space_alloc_status(dset_id, plist_id, (mpi_size > 1) ? SOME_CHUNKS_WRITTEN : ALL_CHUNKS_WRITTEN);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");

    /* Verify the correct data was written */
    dset_id = H5Dopen2(group_id, WRITE_UNSHARED_FILTERED_EDGE_CHUNKS_DATASET_NAME2, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    HDmemset(read_buf, 255, data_size);

    VRFY((H5Dread(dset_id, HDF5_DATATYPE_NAME, H5S_BLOCK, filespace, dxpl_id, read_buf) >= 0),
         "Dataset read succeeded");

    VRFY((0 == HDmemcmp(read_buf, data, data_size)), "Data verification succeeded");

    if (data)
        HDfree(data);
    if (read_buf)
        HDfree(read_buf);

    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
    VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    return;
}

/*
 * Tests that filtered and unfiltered partial edge chunks can be
 * written to and read from correctly in parallel when every MPI
 * rank writes to every partial edge chunk in the dataset.
 *
 * The dataset contains partial edge chunks in the second dimension.
 * Each MPI rank selects a hyperslab in the shape of one row of each
 * chunk that is offset in the second dimension to cover the whole
 * edge chunk and part of the full chunk next to the edge chunk.
 */
static void
test_edge_chunks_overlap(const char *parent_group, H5Z_filter_t filter_id, hid_t fapl_id, hid_t dcpl_id,
                         hid_t dxpl_id)
{
    C_DATATYPE *data     = NULL;
    C_DATATYPE *read_buf = NULL;
    hsize_t     dataset_dims[WRITE_SHARED_FILTERED_EDGE_CHUNKS_DATASET_DIMS];
    hsize_t     chunk_dims[WRITE_SHARED_FILTERED_EDGE_CHUNKS_DATASET_DIMS];
    hsize_t     sel_dims[WRITE_SHARED_FILTERED_EDGE_CHUNKS_DATASET_DIMS];
    hsize_t     start[WRITE_SHARED_FILTERED_EDGE_CHUNKS_DATASET_DIMS];
    hsize_t     stride[WRITE_SHARED_FILTERED_EDGE_CHUNKS_DATASET_DIMS];
    hsize_t     count[WRITE_SHARED_FILTERED_EDGE_CHUNKS_DATASET_DIMS];
    hsize_t     block[WRITE_SHARED_FILTERED_EDGE_CHUNKS_DATASET_DIMS];
    size_t      i, data_size;
    hid_t       file_id = H5I_INVALID_HID, dset_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID;
    hid_t       group_id  = H5I_INVALID_HID;
    hid_t       filespace = H5I_INVALID_HID;

    if (MAINPROCESS)
        HDputs("Testing write to shared filtered edge chunks");

    file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, fapl_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
    VRFY((group_id >= 0), "H5Gopen2 succeeded");

    /* Create the dataspace for the dataset */
    dataset_dims[0] = (hsize_t)WRITE_SHARED_FILTERED_EDGE_CHUNKS_NROWS;
    dataset_dims[1] = (hsize_t)WRITE_SHARED_FILTERED_EDGE_CHUNKS_NCOLS;
    chunk_dims[0]   = (hsize_t)WRITE_SHARED_FILTERED_EDGE_CHUNKS_CH_NROWS;
    chunk_dims[1]   = (hsize_t)WRITE_SHARED_FILTERED_EDGE_CHUNKS_CH_NCOLS;
    sel_dims[0]     = (hsize_t)DIM0_SCALE_FACTOR;
    sel_dims[1]     = (hsize_t)WRITE_SHARED_FILTERED_EDGE_CHUNKS_CH_NCOLS;

    filespace = H5Screate_simple(WRITE_SHARED_FILTERED_EDGE_CHUNKS_DATASET_DIMS, dataset_dims, NULL);
    VRFY((filespace >= 0), "File dataspace creation succeeded");

    /* Create chunked dataset */
    plist_id = H5Pcopy(dcpl_id);
    VRFY((plist_id >= 0), "DCPL copy succeeded");

    VRFY((H5Pset_chunk(plist_id, WRITE_SHARED_FILTERED_EDGE_CHUNKS_DATASET_DIMS, chunk_dims) >= 0),
         "Chunk size set");

    /* Add test filter to the pipeline */
    VRFY((set_dcpl_filter(plist_id, filter_id, NULL) >= 0), "Filter set");

    dset_id = H5Dcreate2(group_id, WRITE_SHARED_FILTERED_EDGE_CHUNKS_DATASET_NAME, HDF5_DATATYPE_NAME,
                         filespace, H5P_DEFAULT, plist_id, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset creation succeeded");

    /* Verify space allocation status */
    verify_space_alloc_status(dset_id, plist_id, DATASET_JUST_CREATED);

    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

    /* Each process defines the dataset selection in memory and writes
     * it to the hyperslab in the file
     */
    count[0] =
        (hsize_t)(WRITE_SHARED_FILTERED_EDGE_CHUNKS_NROWS / WRITE_SHARED_FILTERED_EDGE_CHUNKS_CH_NROWS);
    count[1]  = 1;
    stride[0] = (hsize_t)WRITE_SHARED_FILTERED_EDGE_CHUNKS_CH_NROWS;
    stride[1] = (hsize_t)WRITE_SHARED_FILTERED_EDGE_CHUNKS_CH_NCOLS;
    block[0]  = (hsize_t)1;
    block[1]  = (hsize_t)WRITE_SHARED_FILTERED_EDGE_CHUNKS_CH_NCOLS;
    start[0]  = (hsize_t)mpi_rank;
    start[1] =
        (hsize_t)(WRITE_SHARED_FILTERED_EDGE_CHUNKS_NCOLS - WRITE_SHARED_FILTERED_EDGE_CHUNKS_CH_NCOLS);

    if (VERBOSE_MED) {
        HDprintf("Process %d is writing with count[ %" PRIuHSIZE ", %" PRIuHSIZE " ], stride[ %" PRIuHSIZE
                 ", %" PRIuHSIZE " ], start[ %" PRIuHSIZE ", %" PRIuHSIZE " ], block size[ %" PRIuHSIZE
                 ", %" PRIuHSIZE " ]\n",
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

    data = (C_DATATYPE *)HDcalloc(1, data_size);
    VRFY((NULL != data), "HDcalloc succeeded");

    read_buf = (C_DATATYPE *)HDcalloc(1, data_size);
    VRFY((NULL != read_buf), "HDcalloc succeeded");

    for (i = 0; i < data_size / sizeof(*data); i++)
        data[i] = (C_DATATYPE)GEN_DATA(i);

    VRFY((H5Dwrite(dset_id, HDF5_DATATYPE_NAME, H5S_BLOCK, filespace, dxpl_id, data) >= 0),
         "Dataset write succeeded");

    /* Verify space allocation status */
    verify_space_alloc_status(dset_id, plist_id, SOME_CHUNKS_WRITTEN);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");

    /* Verify the correct data was written */
    dset_id = H5Dopen2(group_id, WRITE_SHARED_FILTERED_EDGE_CHUNKS_DATASET_NAME, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    VRFY((H5Dread(dset_id, HDF5_DATATYPE_NAME, H5S_BLOCK, filespace, dxpl_id, read_buf) >= 0),
         "Dataset read succeeded");

    VRFY((0 == HDmemcmp(read_buf, data, data_size)), "Data verification succeeded");

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");

    /* Repeat the previous, but set option to not filter partial edge chunks */
    if (MAINPROCESS)
        HDputs("Testing write to shared unfiltered edge chunks");

    H5Pset_chunk_opts(plist_id, H5D_CHUNK_DONT_FILTER_PARTIAL_CHUNKS);

    dset_id = H5Dcreate2(group_id, WRITE_SHARED_FILTERED_EDGE_CHUNKS_DATASET_NAME2, HDF5_DATATYPE_NAME,
                         filespace, H5P_DEFAULT, plist_id, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset creation succeeded");

    /* Verify space allocation status */
    verify_space_alloc_status(dset_id, plist_id, DATASET_JUST_CREATED);

    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

    /* Each process defines the dataset selection in memory and writes
     * it to the hyperslab in the file
     */
    count[0] =
        (hsize_t)(WRITE_SHARED_FILTERED_EDGE_CHUNKS_NROWS / WRITE_SHARED_FILTERED_EDGE_CHUNKS_CH_NROWS);
    count[1]  = 1;
    stride[0] = (hsize_t)WRITE_SHARED_FILTERED_EDGE_CHUNKS_CH_NROWS;
    stride[1] = (hsize_t)WRITE_SHARED_FILTERED_EDGE_CHUNKS_CH_NCOLS;
    block[0]  = (hsize_t)1;
    block[1]  = (hsize_t)WRITE_SHARED_FILTERED_EDGE_CHUNKS_CH_NCOLS;
    start[0]  = (hsize_t)mpi_rank;
    start[1] =
        (hsize_t)(WRITE_SHARED_FILTERED_EDGE_CHUNKS_NCOLS - WRITE_SHARED_FILTERED_EDGE_CHUNKS_CH_NCOLS);

    if (VERBOSE_MED) {
        HDprintf("Process %d is writing with count[ %" PRIuHSIZE ", %" PRIuHSIZE " ], stride[ %" PRIuHSIZE
                 ", %" PRIuHSIZE " ], start[ %" PRIuHSIZE ", %" PRIuHSIZE " ], block size[ %" PRIuHSIZE
                 ", %" PRIuHSIZE " ]\n",
                 mpi_rank, count[0], count[1], stride[0], stride[1], start[0], start[1], block[0], block[1]);
        HDfflush(stdout);
    }

    /* Select hyperslab in the file */
    filespace = H5Dget_space(dset_id);
    VRFY((filespace >= 0), "File dataspace retrieval succeeded");

    VRFY((H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, stride, count, block) >= 0),
         "Hyperslab selection succeeded");

    VRFY((H5Dwrite(dset_id, HDF5_DATATYPE_NAME, H5S_BLOCK, filespace, dxpl_id, data) >= 0),
         "Dataset write succeeded");

    /* Verify space allocation status */
    verify_space_alloc_status(dset_id, plist_id, SOME_CHUNKS_WRITTEN);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");

    /* Verify the correct data was written */
    dset_id = H5Dopen2(group_id, WRITE_SHARED_FILTERED_EDGE_CHUNKS_DATASET_NAME2, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    HDmemset(read_buf, 255, data_size);

    VRFY((H5Dread(dset_id, HDF5_DATATYPE_NAME, H5S_BLOCK, filespace, dxpl_id, read_buf) >= 0),
         "Dataset read succeeded");

    VRFY((0 == HDmemcmp(read_buf, data, data_size)), "Data verification succeeded");

    if (data)
        HDfree(data);
    if (read_buf)
        HDfree(read_buf);

    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
    VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    return;
}

/*
 * Tests that filtered and unfiltered partial edge chunks can be
 * written to and read from correctly in parallel when only one
 * MPI rank writes to a particular edge chunk in the dataset and
 * only performs a partial write to the edge chunk.
 *
 * The dataset contains partial edge chunks in the second dimension.
 * Each MPI rank selects a hyperslab in the shape of part of a single
 * edge chunk and writes to just a portion of the edge chunk.
 */
static void
test_edge_chunks_partial_write(const char H5_ATTR_PARALLEL_UNUSED  *parent_group,
                               H5Z_filter_t H5_ATTR_PARALLEL_UNUSED filter_id,
                               hid_t H5_ATTR_PARALLEL_UNUSED fapl_id, hid_t H5_ATTR_PARALLEL_UNUSED dcpl_id,
                               hid_t H5_ATTR_PARALLEL_UNUSED dxpl_id)
{
    /* TODO */
}

/*
 * Tests that the parallel compression feature correctly handles
 * writing fill values to a dataset and reading fill values from
 * unallocated parts of a dataset.
 */
static void
test_fill_values(const char *parent_group, H5Z_filter_t filter_id, hid_t fapl_id, hid_t dcpl_id,
                 hid_t dxpl_id)
{
    C_DATATYPE *data        = NULL;
    C_DATATYPE *read_buf    = NULL;
    C_DATATYPE *correct_buf = NULL;
    C_DATATYPE  fill_value;
    hsize_t     dataset_dims[FILL_VALUES_TEST_DATASET_DIMS];
    hsize_t     chunk_dims[FILL_VALUES_TEST_DATASET_DIMS];
    hsize_t     sel_dims[FILL_VALUES_TEST_DATASET_DIMS];
    hsize_t     start[FILL_VALUES_TEST_DATASET_DIMS];
    hsize_t     stride[FILL_VALUES_TEST_DATASET_DIMS];
    hsize_t     count[FILL_VALUES_TEST_DATASET_DIMS];
    hsize_t     block[FILL_VALUES_TEST_DATASET_DIMS];
    size_t      i, data_size, read_buf_size;
    hid_t       file_id = H5I_INVALID_HID, dset_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID;
    hid_t       group_id   = H5I_INVALID_HID;
    hid_t       filespace  = H5I_INVALID_HID;
    int        *recvcounts = NULL;
    int        *displs     = NULL;

    if (MAINPROCESS)
        HDputs("Testing fill values");

    file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, fapl_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
    VRFY((group_id >= 0), "H5Gopen2 succeeded");

    /* Create the dataspace for the dataset */
    dataset_dims[0] = (hsize_t)FILL_VALUES_TEST_NROWS;
    dataset_dims[1] = (hsize_t)FILL_VALUES_TEST_NCOLS;
    chunk_dims[0]   = (hsize_t)FILL_VALUES_TEST_CH_NROWS;
    chunk_dims[1]   = (hsize_t)FILL_VALUES_TEST_CH_NCOLS;
    sel_dims[0]     = (hsize_t)DIM0_SCALE_FACTOR;
    sel_dims[1]     = (hsize_t)FILL_VALUES_TEST_CH_NCOLS * (hsize_t)DIM1_SCALE_FACTOR;

    filespace = H5Screate_simple(FILL_VALUES_TEST_DATASET_DIMS, dataset_dims, NULL);
    VRFY((filespace >= 0), "File dataspace creation succeeded");

    /* Create chunked dataset */
    plist_id = H5Pcopy(dcpl_id);
    VRFY((plist_id >= 0), "DCPL copy succeeded");

    VRFY((H5Pset_chunk(plist_id, FILL_VALUES_TEST_DATASET_DIMS, chunk_dims) >= 0), "Chunk size set");

    /* Add test filter to the pipeline */
    VRFY((set_dcpl_filter(plist_id, filter_id, NULL) >= 0), "Filter set");

    /* Set a fill value */
    fill_value = FILL_VALUES_TEST_FILL_VAL;
    VRFY((H5Pset_fill_value(plist_id, HDF5_DATATYPE_NAME, &fill_value) >= 0), "Fill Value set");

    dset_id = H5Dcreate2(group_id, FILL_VALUES_TEST_DATASET_NAME, HDF5_DATATYPE_NAME, filespace, H5P_DEFAULT,
                         plist_id, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset creation succeeded");

    /* Verify space allocation status */
    verify_space_alloc_status(dset_id, plist_id, DATASET_JUST_CREATED);

    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

    /* Allocate buffer for reading entire dataset */
    read_buf_size = dataset_dims[0] * dataset_dims[1] * sizeof(*read_buf);

    read_buf = HDcalloc(1, read_buf_size);
    VRFY((NULL != read_buf), "HDcalloc succeeded");

    correct_buf = HDcalloc(1, read_buf_size);
    VRFY((NULL != correct_buf), "HDcalloc succeeded");

    /* Read entire dataset and verify that the fill value is returned */
    VRFY((H5Dread(dset_id, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, dxpl_id, read_buf) >= 0),
         "Dataset read succeeded");

    for (i = 0; i < read_buf_size / sizeof(*read_buf); i++)
        correct_buf[i] = FILL_VALUES_TEST_FILL_VAL;

    VRFY((0 == HDmemcmp(read_buf, correct_buf, read_buf_size)), "Data verification succeeded");

    /*
     * Write to part of the first chunk in the dataset with
     * all ranks, then read the whole dataset and ensure that
     * the fill value is returned for the unwritten part of
     * the chunk, as well as for the rest of the dataset that
     * hasn't been written to yet.
     */
    count[0]  = 1;
    count[1]  = 1;
    stride[0] = (hsize_t)FILL_VALUES_TEST_CH_NROWS;
    stride[1] = (hsize_t)FILL_VALUES_TEST_CH_NCOLS;
    block[0]  = 1;
    block[1]  = (hsize_t)(FILL_VALUES_TEST_CH_NCOLS - 1);
    start[0]  = (hsize_t)mpi_rank;
    start[1]  = 0;

    if (VERBOSE_MED) {
        HDprintf("Process %d is writing with count[ %" PRIuHSIZE ", %" PRIuHSIZE " ], stride[ %" PRIuHSIZE
                 ", %" PRIuHSIZE " ], start[ %" PRIuHSIZE ", %" PRIuHSIZE " ], block size[ %" PRIuHSIZE
                 ", %" PRIuHSIZE " ]\n",
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

    data = (C_DATATYPE *)HDcalloc(1, data_size);
    VRFY((NULL != data), "HDcalloc succeeded");

    for (i = 0; i < data_size / sizeof(*data); i++)
        data[i] = (C_DATATYPE)GEN_DATA(i);

    VRFY((H5Dwrite(dset_id, HDF5_DATATYPE_NAME, H5S_BLOCK, filespace, dxpl_id, data) >= 0),
         "Dataset write succeeded");

    /* Verify space allocation status */
    verify_space_alloc_status(dset_id, plist_id, SOME_CHUNKS_WRITTEN);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");

    /* Verify correct data was written */
    dset_id = H5Dopen2(group_id, FILL_VALUES_TEST_DATASET_NAME, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    VRFY((H5Dread(dset_id, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, dxpl_id, read_buf) >= 0),
         "Dataset read succeeded");

    /*
     * Each MPI rank communicates their written piece of data
     * into each other rank's correctness-checking buffer
     */
    recvcounts = HDcalloc(1, (size_t)mpi_size * sizeof(*recvcounts));
    VRFY((NULL != recvcounts), "HDcalloc succeeded");

    displs = HDcalloc(1, (size_t)mpi_size * sizeof(*displs));
    VRFY((NULL != displs), "HDcalloc succeeded");

    for (i = 0; i < (size_t)mpi_size; i++) {
        recvcounts[i] = (int)(count[1] * block[1]);
        displs[i]     = (int)(i * dataset_dims[1]);
    }

    VRFY((MPI_SUCCESS == MPI_Allgatherv(data, recvcounts[mpi_rank], C_DATATYPE_MPI, correct_buf, recvcounts,
                                        displs, C_DATATYPE_MPI, comm)),
         "MPI_Allgatherv succeeded");

    VRFY((0 == HDmemcmp(read_buf, correct_buf, read_buf_size)), "Data verification succeeded");

    /*
     * Write to whole dataset and ensure fill value isn't returned
     * after reading whole dataset back
     */

    /* Each process defines the dataset selection in memory and writes
     * it to the hyperslab in the file
     */
    count[0]  = (hsize_t)FILL_VALUES_TEST_NROWS / (hsize_t)FILL_VALUES_TEST_CH_NROWS;
    count[1]  = (hsize_t)FILL_VALUES_TEST_NCOLS / (hsize_t)FILL_VALUES_TEST_CH_NCOLS;
    stride[0] = (hsize_t)FILL_VALUES_TEST_CH_NROWS;
    stride[1] = (hsize_t)FILL_VALUES_TEST_CH_NCOLS;
    block[0]  = (hsize_t)FILL_VALUES_TEST_CH_NROWS / (hsize_t)mpi_size;
    block[1]  = (hsize_t)FILL_VALUES_TEST_CH_NCOLS;
    start[0]  = (hsize_t)mpi_rank * block[0];
    start[1]  = 0;

    if (VERBOSE_MED) {
        HDprintf("Process %d is writing with count[ %" PRIuHSIZE ", %" PRIuHSIZE " ], stride[ %" PRIuHSIZE
                 ", %" PRIuHSIZE " ], start[ %" PRIuHSIZE ", %" PRIuHSIZE " ], block size[ %" PRIuHSIZE
                 ", %" PRIuHSIZE " ]\n",
                 mpi_rank, count[0], count[1], stride[0], stride[1], start[0], start[1], block[0], block[1]);
        HDfflush(stdout);
    }

    VRFY((H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, stride, count, block) >= 0),
         "Hyperslab selection succeeded");

    VRFY((H5Dwrite(dset_id, HDF5_DATATYPE_NAME, H5S_BLOCK, filespace, dxpl_id, data) >= 0),
         "Dataset write succeeded");

    /* Verify space allocation status */
    verify_space_alloc_status(dset_id, plist_id, ALL_CHUNKS_WRITTEN);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");

    /* Verify correct data was written */
    dset_id = H5Dopen2(group_id, FILL_VALUES_TEST_DATASET_NAME, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    VRFY((H5Dread(dset_id, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, dxpl_id, read_buf) >= 0),
         "Dataset read succeeded");

    for (i = 0; i < read_buf_size / sizeof(*read_buf); i++)
        VRFY((read_buf[i] != FILL_VALUES_TEST_FILL_VAL), "Data verification succeeded");

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");

    /********************************************************************
     * Set the fill time to H5D_FILL_TIME_ALLOC and repeat the previous *
     ********************************************************************/

    VRFY((H5Pset_fill_time(plist_id, H5D_FILL_TIME_ALLOC) >= 0), "H5Pset_fill_time succeeded");

    dset_id = H5Dcreate2(group_id, FILL_VALUES_TEST_DATASET_NAME2, HDF5_DATATYPE_NAME, filespace, H5P_DEFAULT,
                         plist_id, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset creation succeeded");

    /* Verify space allocation status */
    verify_space_alloc_status(dset_id, plist_id, DATASET_JUST_CREATED);

    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

    /* Read entire dataset and verify that the fill value is returned */
    VRFY((H5Dread(dset_id, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, dxpl_id, read_buf) >= 0),
         "Dataset read succeeded");

    for (i = 0; i < read_buf_size / sizeof(*read_buf); i++)
        correct_buf[i] = FILL_VALUES_TEST_FILL_VAL;

    VRFY((0 == HDmemcmp(read_buf, correct_buf, read_buf_size)), "Data verification succeeded");

    /*
     * Write to part of the first chunk in the dataset with
     * all ranks, then read the whole dataset and ensure that
     * the fill value is returned for the unwritten part of
     * the chunk, as well as for the rest of the dataset that
     * hasn't been written to yet.
     */
    count[0]  = 1;
    count[1]  = 1;
    stride[0] = (hsize_t)FILL_VALUES_TEST_CH_NROWS;
    stride[1] = (hsize_t)FILL_VALUES_TEST_CH_NCOLS;
    block[0]  = 1;
    block[1]  = (hsize_t)(FILL_VALUES_TEST_CH_NCOLS - 1);
    start[0]  = (hsize_t)mpi_rank;
    start[1]  = 0;

    if (VERBOSE_MED) {
        HDprintf("Process %d is writing with count[ %" PRIuHSIZE ", %" PRIuHSIZE " ], stride[ %" PRIuHSIZE
                 ", %" PRIuHSIZE " ], start[ %" PRIuHSIZE ", %" PRIuHSIZE " ], block size[ %" PRIuHSIZE
                 ", %" PRIuHSIZE " ]\n",
                 mpi_rank, count[0], count[1], stride[0], stride[1], start[0], start[1], block[0], block[1]);
        HDfflush(stdout);
    }

    /* Select hyperslab in the file */
    filespace = H5Dget_space(dset_id);
    VRFY((filespace >= 0), "File dataspace retrieval succeeded");

    VRFY((H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, stride, count, block) >= 0),
         "Hyperslab selection succeeded");

    for (i = 0; i < data_size / sizeof(*data); i++)
        data[i] = (C_DATATYPE)GEN_DATA(i);

    VRFY((H5Dwrite(dset_id, HDF5_DATATYPE_NAME, H5S_BLOCK, filespace, dxpl_id, data) >= 0),
         "Dataset write succeeded");

    /* Verify space allocation status */
    verify_space_alloc_status(dset_id, plist_id, SOME_CHUNKS_WRITTEN);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");

    /* Verify correct data was written */
    dset_id = H5Dopen2(group_id, FILL_VALUES_TEST_DATASET_NAME2, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    VRFY((H5Dread(dset_id, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, dxpl_id, read_buf) >= 0),
         "Dataset read succeeded");

    for (i = 0; i < (size_t)mpi_size; i++) {
        recvcounts[i] = (int)(count[1] * block[1]);
        displs[i]     = (int)(i * dataset_dims[1]);
    }

    /*
     * Each MPI rank communicates their written piece of data
     * into each other rank's correctness-checking buffer
     */
    VRFY((MPI_SUCCESS == MPI_Allgatherv(data, recvcounts[mpi_rank], C_DATATYPE_MPI, correct_buf, recvcounts,
                                        displs, C_DATATYPE_MPI, comm)),
         "MPI_Allgatherv succeeded");

    VRFY((0 == HDmemcmp(read_buf, correct_buf, read_buf_size)), "Data verification succeeded");

    /*
     * Write to whole dataset and ensure fill value isn't returned
     * after reading whole dataset back
     */

    /* Each process defines the dataset selection in memory and writes
     * it to the hyperslab in the file
     */
    count[0]  = (hsize_t)FILL_VALUES_TEST_NROWS / (hsize_t)FILL_VALUES_TEST_CH_NROWS;
    count[1]  = (hsize_t)FILL_VALUES_TEST_NCOLS / (hsize_t)FILL_VALUES_TEST_CH_NCOLS;
    stride[0] = (hsize_t)FILL_VALUES_TEST_CH_NROWS;
    stride[1] = (hsize_t)FILL_VALUES_TEST_CH_NCOLS;
    block[0]  = (hsize_t)FILL_VALUES_TEST_CH_NROWS / (hsize_t)mpi_size;
    block[1]  = (hsize_t)FILL_VALUES_TEST_CH_NCOLS;
    start[0]  = (hsize_t)mpi_rank * block[0];
    start[1]  = 0;

    if (VERBOSE_MED) {
        HDprintf("Process %d is writing with count[ %" PRIuHSIZE ", %" PRIuHSIZE " ], stride[ %" PRIuHSIZE
                 ", %" PRIuHSIZE " ], start[ %" PRIuHSIZE ", %" PRIuHSIZE " ], block size[ %" PRIuHSIZE
                 ", %" PRIuHSIZE " ]\n",
                 mpi_rank, count[0], count[1], stride[0], stride[1], start[0], start[1], block[0], block[1]);
        HDfflush(stdout);
    }

    VRFY((H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, stride, count, block) >= 0),
         "Hyperslab selection succeeded");

    VRFY((H5Dwrite(dset_id, HDF5_DATATYPE_NAME, H5S_BLOCK, filespace, dxpl_id, data) >= 0),
         "Dataset write succeeded");

    /* Verify space allocation status */
    verify_space_alloc_status(dset_id, plist_id, ALL_CHUNKS_WRITTEN);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");

    /* Verify correct data was written */
    dset_id = H5Dopen2(group_id, FILL_VALUES_TEST_DATASET_NAME2, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    VRFY((H5Dread(dset_id, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, dxpl_id, read_buf) >= 0),
         "Dataset read succeeded");

    for (i = 0; i < read_buf_size / sizeof(*read_buf); i++)
        VRFY((read_buf[i] != FILL_VALUES_TEST_FILL_VAL), "Data verification succeeded");

    if (displs)
        HDfree(displs);
    if (recvcounts)
        HDfree(recvcounts);
    if (data)
        HDfree(data);
    if (read_buf)
        HDfree(read_buf);
    if (correct_buf)
        HDfree(correct_buf);

    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    return;
}

/*
 * Tests that the parallel compression feature can handle
 * an undefined fill value. Nothing is verified in this
 * test since the fill value isn't defined.
 */
static void
test_fill_value_undefined(const char *parent_group, H5Z_filter_t filter_id, hid_t fapl_id, hid_t dcpl_id,
                          hid_t dxpl_id)
{
    H5D_alloc_time_t alloc_time;
    C_DATATYPE      *data     = NULL;
    C_DATATYPE      *read_buf = NULL;
    hsize_t          dataset_dims[FILL_VALUE_UNDEFINED_TEST_DATASET_DIMS];
    hsize_t          chunk_dims[FILL_VALUE_UNDEFINED_TEST_DATASET_DIMS];
    hsize_t          sel_dims[FILL_VALUE_UNDEFINED_TEST_DATASET_DIMS];
    hsize_t          start[FILL_VALUE_UNDEFINED_TEST_DATASET_DIMS];
    hsize_t          stride[FILL_VALUE_UNDEFINED_TEST_DATASET_DIMS];
    hsize_t          count[FILL_VALUE_UNDEFINED_TEST_DATASET_DIMS];
    hsize_t          block[FILL_VALUE_UNDEFINED_TEST_DATASET_DIMS];
    size_t           i, data_size, read_buf_size;
    hid_t            file_id = H5I_INVALID_HID, dset_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID;
    hid_t            group_id  = H5I_INVALID_HID;
    hid_t            filespace = H5I_INVALID_HID;

    if (MAINPROCESS)
        HDputs("Testing undefined fill value");

    VRFY((H5Pget_alloc_time(dcpl_id, &alloc_time) >= 0), "H5Pget_alloc_time succeeded");

    file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, fapl_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
    VRFY((group_id >= 0), "H5Gopen2 succeeded");

    /* Create the dataspace for the dataset */
    dataset_dims[0] = (hsize_t)FILL_VALUE_UNDEFINED_TEST_NROWS;
    dataset_dims[1] = (hsize_t)FILL_VALUE_UNDEFINED_TEST_NCOLS;
    chunk_dims[0]   = (hsize_t)FILL_VALUE_UNDEFINED_TEST_CH_NROWS;
    chunk_dims[1]   = (hsize_t)FILL_VALUE_UNDEFINED_TEST_CH_NCOLS;
    sel_dims[0]     = (hsize_t)DIM0_SCALE_FACTOR;
    sel_dims[1]     = (hsize_t)FILL_VALUE_UNDEFINED_TEST_CH_NCOLS * (hsize_t)DIM1_SCALE_FACTOR;

    filespace = H5Screate_simple(FILL_VALUE_UNDEFINED_TEST_DATASET_DIMS, dataset_dims, NULL);
    VRFY((filespace >= 0), "File dataspace creation succeeded");

    /* Create chunked dataset */
    plist_id = H5Pcopy(dcpl_id);
    VRFY((plist_id >= 0), "DCPL copy succeeded");

    VRFY((H5Pset_chunk(plist_id, FILL_VALUE_UNDEFINED_TEST_DATASET_DIMS, chunk_dims) >= 0), "Chunk size set");

    /* Add test filter to the pipeline */
    VRFY((set_dcpl_filter(plist_id, filter_id, NULL) >= 0), "Filter set");

    /* Set an undefined fill value */
    VRFY((H5Pset_fill_value(plist_id, HDF5_DATATYPE_NAME, NULL) >= 0), "Fill Value set");

    dset_id = H5Dcreate2(group_id, FILL_VALUE_UNDEFINED_TEST_DATASET_NAME, HDF5_DATATYPE_NAME, filespace,
                         H5P_DEFAULT, plist_id, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset creation succeeded");

    /* Verify space allocation status */
    verify_space_alloc_status(dset_id, plist_id, DATASET_JUST_CREATED);

    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

    /* Allocate buffer for reading entire dataset */
    read_buf_size = dataset_dims[0] * dataset_dims[1] * sizeof(*read_buf);

    read_buf = HDcalloc(1, read_buf_size);
    VRFY((NULL != read_buf), "HDcalloc succeeded");

    /*
     * Read entire dataset - nothing to verify since there's no fill value.
     * If not using early space allocation, the read should fail since storage
     * isn't allocated yet and no fill value is defined.
     */
    if (alloc_time == H5D_ALLOC_TIME_EARLY) {
        VRFY((H5Dread(dset_id, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, dxpl_id, read_buf) >= 0),
             "Dataset read succeeded");
    }
    else {
        H5E_BEGIN_TRY
        {
            VRFY((H5Dread(dset_id, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, dxpl_id, read_buf) < 0),
                 "Dataset read succeeded");
        }
        H5E_END_TRY;
    }

    /*
     * Write to part of the first chunk in the dataset with
     * all ranks, then read the whole dataset. Don't verify
     * anything since there's no fill value defined.
     */
    count[0]  = 1;
    count[1]  = 1;
    stride[0] = (hsize_t)FILL_VALUE_UNDEFINED_TEST_CH_NROWS;
    stride[1] = (hsize_t)FILL_VALUE_UNDEFINED_TEST_CH_NCOLS;
    block[0]  = 1;
    block[1]  = (hsize_t)(FILL_VALUE_UNDEFINED_TEST_CH_NCOLS - 1);
    start[0]  = (hsize_t)mpi_rank;
    start[1]  = 0;

    if (VERBOSE_MED) {
        HDprintf("Process %d is writing with count[ %" PRIuHSIZE ", %" PRIuHSIZE " ], stride[ %" PRIuHSIZE
                 ", %" PRIuHSIZE " ], start[ %" PRIuHSIZE ", %" PRIuHSIZE " ], block size[ %" PRIuHSIZE
                 ", %" PRIuHSIZE " ]\n",
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

    data = (C_DATATYPE *)HDcalloc(1, data_size);
    VRFY((NULL != data), "HDcalloc succeeded");

    for (i = 0; i < data_size / sizeof(*data); i++)
        data[i] = (C_DATATYPE)GEN_DATA(i);

    VRFY((H5Dwrite(dset_id, HDF5_DATATYPE_NAME, H5S_BLOCK, filespace, dxpl_id, data) >= 0),
         "Dataset write succeeded");

    /* Verify space allocation status */
    verify_space_alloc_status(dset_id, plist_id, SOME_CHUNKS_WRITTEN);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");

    dset_id = H5Dopen2(group_id, FILL_VALUE_UNDEFINED_TEST_DATASET_NAME, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    VRFY((H5Dread(dset_id, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, dxpl_id, read_buf) >= 0),
         "Dataset read succeeded");

    /*
     * Write to whole dataset and ensure data is correct
     * after reading whole dataset back
     */

    /* Each process defines the dataset selection in memory and writes
     * it to the hyperslab in the file
     */
    count[0]  = (hsize_t)FILL_VALUE_UNDEFINED_TEST_NROWS / (hsize_t)FILL_VALUE_UNDEFINED_TEST_CH_NROWS;
    count[1]  = (hsize_t)FILL_VALUE_UNDEFINED_TEST_NCOLS / (hsize_t)FILL_VALUE_UNDEFINED_TEST_CH_NCOLS;
    stride[0] = (hsize_t)FILL_VALUE_UNDEFINED_TEST_CH_NROWS;
    stride[1] = (hsize_t)FILL_VALUE_UNDEFINED_TEST_CH_NCOLS;
    block[0]  = (hsize_t)FILL_VALUE_UNDEFINED_TEST_CH_NROWS / (hsize_t)mpi_size;
    block[1]  = (hsize_t)FILL_VALUE_UNDEFINED_TEST_CH_NCOLS;
    start[0]  = (hsize_t)mpi_rank * block[0];
    start[1]  = 0;

    if (VERBOSE_MED) {
        HDprintf("Process %d is writing with count[ %" PRIuHSIZE ", %" PRIuHSIZE " ], stride[ %" PRIuHSIZE
                 ", %" PRIuHSIZE " ], start[ %" PRIuHSIZE ", %" PRIuHSIZE " ], block size[ %" PRIuHSIZE
                 ", %" PRIuHSIZE " ]\n",
                 mpi_rank, count[0], count[1], stride[0], stride[1], start[0], start[1], block[0], block[1]);
        HDfflush(stdout);
    }

    VRFY((H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, stride, count, block) >= 0),
         "Hyperslab selection succeeded");

    VRFY((H5Dwrite(dset_id, HDF5_DATATYPE_NAME, H5S_BLOCK, filespace, dxpl_id, data) >= 0),
         "Dataset write succeeded");

    /* Verify space allocation status */
    verify_space_alloc_status(dset_id, plist_id, ALL_CHUNKS_WRITTEN);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");

    /* Verify correct data was written */
    dset_id = H5Dopen2(group_id, FILL_VALUE_UNDEFINED_TEST_DATASET_NAME, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    VRFY((H5Dread(dset_id, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, dxpl_id, read_buf) >= 0),
         "Dataset read succeeded");

    if (data)
        HDfree(data);
    if (read_buf)
        HDfree(read_buf);

    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    return;
}

/*
 * Tests that the parallel compression feature correctly handles
 * avoiding writing fill values to a dataset when the fill time
 * is set as H5D_FILL_TIME_NEVER.
 */
static void
test_fill_time_never(const char *parent_group, H5Z_filter_t filter_id, hid_t fapl_id, hid_t dcpl_id,
                     hid_t dxpl_id)
{
    C_DATATYPE *data     = NULL;
    C_DATATYPE *read_buf = NULL;
    C_DATATYPE *fill_buf = NULL;
    C_DATATYPE  fill_value;
    hsize_t     dataset_dims[FILL_TIME_NEVER_TEST_DATASET_DIMS];
    hsize_t     chunk_dims[FILL_TIME_NEVER_TEST_DATASET_DIMS];
    hsize_t     sel_dims[FILL_TIME_NEVER_TEST_DATASET_DIMS];
    hsize_t     start[FILL_TIME_NEVER_TEST_DATASET_DIMS];
    hsize_t     stride[FILL_TIME_NEVER_TEST_DATASET_DIMS];
    hsize_t     count[FILL_TIME_NEVER_TEST_DATASET_DIMS];
    hsize_t     block[FILL_TIME_NEVER_TEST_DATASET_DIMS];
    size_t      i, data_size, read_buf_size;
    hid_t       file_id = H5I_INVALID_HID, dset_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID;
    hid_t       group_id   = H5I_INVALID_HID;
    hid_t       filespace  = H5I_INVALID_HID;
    int        *recvcounts = NULL;
    int        *displs     = NULL;

    if (MAINPROCESS)
        HDputs("Testing fill time H5D_FILL_TIME_NEVER");

    /*
     * Only run this test when incremental file space allocation is
     * used, as HDF5's chunk allocation code always writes fill values
     * when filters are in the pipeline, but parallel compression does
     * incremental file space allocation differently.
     */
    {
        H5D_alloc_time_t alloc_time;

        VRFY((H5Pget_alloc_time(dcpl_id, &alloc_time) >= 0), "H5Pget_alloc_time succeeded");

        if (alloc_time != H5D_ALLOC_TIME_INCR) {
            if (MAINPROCESS)
                SKIPPED();
            return;
        }
    }

    file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, fapl_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
    VRFY((group_id >= 0), "H5Gopen2 succeeded");

    /* Create the dataspace for the dataset */
    dataset_dims[0] = (hsize_t)FILL_TIME_NEVER_TEST_NROWS;
    dataset_dims[1] = (hsize_t)FILL_TIME_NEVER_TEST_NCOLS;
    chunk_dims[0]   = (hsize_t)FILL_TIME_NEVER_TEST_CH_NROWS;
    chunk_dims[1]   = (hsize_t)FILL_TIME_NEVER_TEST_CH_NCOLS;
    sel_dims[0]     = (hsize_t)DIM0_SCALE_FACTOR;
    sel_dims[1]     = (hsize_t)FILL_TIME_NEVER_TEST_CH_NCOLS * (hsize_t)DIM1_SCALE_FACTOR;

    filespace = H5Screate_simple(FILL_TIME_NEVER_TEST_DATASET_DIMS, dataset_dims, NULL);
    VRFY((filespace >= 0), "File dataspace creation succeeded");

    /* Create chunked dataset */
    plist_id = H5Pcopy(dcpl_id);
    VRFY((plist_id >= 0), "DCPL copy succeeded");

    VRFY((H5Pset_chunk(plist_id, FILL_TIME_NEVER_TEST_DATASET_DIMS, chunk_dims) >= 0), "Chunk size set");

    /* Add test filter to the pipeline */
    VRFY((set_dcpl_filter(plist_id, filter_id, NULL) >= 0), "Filter set");

    /* Set a fill value */
    fill_value = FILL_VALUES_TEST_FILL_VAL;
    VRFY((H5Pset_fill_value(plist_id, HDF5_DATATYPE_NAME, &fill_value) >= 0), "Fill Value set");

    /* Set fill time of 'never' */
    VRFY((H5Pset_fill_time(plist_id, H5D_FILL_TIME_NEVER) >= 0), "H5Pset_fill_time succeeded");

    dset_id = H5Dcreate2(group_id, FILL_TIME_NEVER_TEST_DATASET_NAME, HDF5_DATATYPE_NAME, filespace,
                         H5P_DEFAULT, plist_id, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset creation succeeded");

    /* Verify space allocation status */
    verify_space_alloc_status(dset_id, plist_id, DATASET_JUST_CREATED);

    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

    /* Allocate buffer for reading entire dataset */
    read_buf_size = dataset_dims[0] * dataset_dims[1] * sizeof(*read_buf);

    read_buf = HDcalloc(1, read_buf_size);
    VRFY((NULL != read_buf), "HDcalloc succeeded");

    fill_buf = HDcalloc(1, read_buf_size);
    VRFY((NULL != fill_buf), "HDcalloc succeeded");

    /* Read entire dataset and verify that the fill value isn't returned */
    VRFY((H5Dread(dset_id, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, dxpl_id, read_buf) >= 0),
         "Dataset read succeeded");

    for (i = 0; i < read_buf_size / sizeof(*read_buf); i++)
        fill_buf[i] = FILL_TIME_NEVER_TEST_FILL_VAL;

    /*
     * It should be very unlikely for the dataset's random
     * values to all be the fill value, so this should be
     * a safe comparison in theory.
     */
    VRFY((0 != HDmemcmp(read_buf, fill_buf, read_buf_size)), "Data verification succeeded");

    /*
     * Write to part of the first chunk in the dataset with
     * all ranks, then read the whole dataset and ensure that
     * the fill value isn't returned for the unwritten part of
     * the chunk, as well as for the rest of the dataset that
     * hasn't been written to yet.
     */
    count[0]  = 1;
    count[1]  = 1;
    stride[0] = (hsize_t)FILL_TIME_NEVER_TEST_CH_NROWS;
    stride[1] = (hsize_t)FILL_TIME_NEVER_TEST_CH_NCOLS;
    block[0]  = 1;
    block[1]  = (hsize_t)(FILL_TIME_NEVER_TEST_CH_NCOLS - 1);
    start[0]  = (hsize_t)mpi_rank;
    start[1]  = 0;

    if (VERBOSE_MED) {
        HDprintf("Process %d is writing with count[ %" PRIuHSIZE ", %" PRIuHSIZE " ], stride[ %" PRIuHSIZE
                 ", %" PRIuHSIZE " ], start[ %" PRIuHSIZE ", %" PRIuHSIZE " ], block size[ %" PRIuHSIZE
                 ", %" PRIuHSIZE " ]\n",
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

    data = (C_DATATYPE *)HDcalloc(1, data_size);
    VRFY((NULL != data), "HDcalloc succeeded");

    for (i = 0; i < data_size / sizeof(*data); i++)
        data[i] = (C_DATATYPE)GEN_DATA(i);

    VRFY((H5Dwrite(dset_id, HDF5_DATATYPE_NAME, H5S_BLOCK, filespace, dxpl_id, data) >= 0),
         "Dataset write succeeded");

    /* Verify space allocation status */
    verify_space_alloc_status(dset_id, plist_id, SOME_CHUNKS_WRITTEN);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");

    /* Verify correct data was written */
    dset_id = H5Dopen2(group_id, FILL_TIME_NEVER_TEST_DATASET_NAME, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    VRFY((H5Dread(dset_id, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, dxpl_id, read_buf) >= 0),
         "Dataset read succeeded");

    /*
     * Each MPI rank communicates their written piece of data
     * into each other rank's correctness-checking buffer
     */
    recvcounts = HDcalloc(1, (size_t)mpi_size * sizeof(*recvcounts));
    VRFY((NULL != recvcounts), "HDcalloc succeeded");

    displs = HDcalloc(1, (size_t)mpi_size * sizeof(*displs));
    VRFY((NULL != displs), "HDcalloc succeeded");

    for (i = 0; i < (size_t)mpi_size; i++) {
        recvcounts[i] = (int)(count[1] * block[1]);
        displs[i]     = (int)(i * dataset_dims[1]);
    }

    VRFY((MPI_SUCCESS == MPI_Allgatherv(data, recvcounts[mpi_rank], C_DATATYPE_MPI, fill_buf, recvcounts,
                                        displs, C_DATATYPE_MPI, comm)),
         "MPI_Allgatherv succeeded");

    /*
     * It should be very unlikely for the dataset's random
     * values to all be the fill value, so this should be
     * a safe comparison in theory.
     */
    VRFY((0 != HDmemcmp(read_buf, fill_buf, read_buf_size)), "Data verification succeeded");

    /*
     * Write to whole dataset and ensure fill value isn't returned
     * after reading whole dataset back
     */

    /* Each process defines the dataset selection in memory and writes
     * it to the hyperslab in the file
     */
    count[0]  = (hsize_t)FILL_TIME_NEVER_TEST_NROWS / (hsize_t)FILL_TIME_NEVER_TEST_CH_NROWS;
    count[1]  = (hsize_t)FILL_TIME_NEVER_TEST_NCOLS / (hsize_t)FILL_TIME_NEVER_TEST_CH_NCOLS;
    stride[0] = (hsize_t)FILL_TIME_NEVER_TEST_CH_NROWS;
    stride[1] = (hsize_t)FILL_TIME_NEVER_TEST_CH_NCOLS;
    block[0]  = (hsize_t)FILL_TIME_NEVER_TEST_CH_NROWS / (hsize_t)mpi_size;
    block[1]  = (hsize_t)FILL_TIME_NEVER_TEST_CH_NCOLS;
    start[0]  = (hsize_t)mpi_rank * block[0];
    start[1]  = 0;

    if (VERBOSE_MED) {
        HDprintf("Process %d is writing with count[ %" PRIuHSIZE ", %" PRIuHSIZE " ], stride[ %" PRIuHSIZE
                 ", %" PRIuHSIZE " ], start[ %" PRIuHSIZE ", %" PRIuHSIZE " ], block size[ %" PRIuHSIZE
                 ", %" PRIuHSIZE " ]\n",
                 mpi_rank, count[0], count[1], stride[0], stride[1], start[0], start[1], block[0], block[1]);
        HDfflush(stdout);
    }

    VRFY((H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, stride, count, block) >= 0),
         "Hyperslab selection succeeded");

    VRFY((H5Dwrite(dset_id, HDF5_DATATYPE_NAME, H5S_BLOCK, filespace, dxpl_id, data) >= 0),
         "Dataset write succeeded");

    /* Verify space allocation status */
    verify_space_alloc_status(dset_id, plist_id, ALL_CHUNKS_WRITTEN);

    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");

    /* Verify correct data was written */
    dset_id = H5Dopen2(group_id, FILL_TIME_NEVER_TEST_DATASET_NAME, H5P_DEFAULT);
    VRFY((dset_id >= 0), "Dataset open succeeded");

    VRFY((H5Dread(dset_id, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, dxpl_id, read_buf) >= 0),
         "Dataset read succeeded");

    for (i = 0; i < read_buf_size / sizeof(*read_buf); i++)
        VRFY((read_buf[i] != FILL_TIME_NEVER_TEST_FILL_VAL), "Data verification succeeded");

    if (displs)
        HDfree(displs);
    if (recvcounts)
        HDfree(recvcounts);
    if (data)
        HDfree(data);
    if (read_buf)
        HDfree(read_buf);
    if (fill_buf)
        HDfree(fill_buf);

    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
    VRFY((H5Dclose(dset_id) >= 0), "Dataset close succeeded");
    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    return;
}
#endif

int
main(int argc, char **argv)
{
    size_t cur_filter_idx = 0;
    size_t num_filters    = 0;
    hid_t  file_id        = H5I_INVALID_HID;
    hid_t  fcpl_id        = H5I_INVALID_HID;
    hid_t  group_id       = H5I_INVALID_HID;
    hid_t  fapl_id        = H5I_INVALID_HID;
    hid_t  dxpl_id        = H5I_INVALID_HID;
    hid_t  dcpl_id        = H5I_INVALID_HID;
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
        HDprintf("  Parallel Filters tests\n");
        HDprintf("==========================\n\n");
    }

    if (VERBOSE_MED)
        h5_show_hostname();

    TestAlarmOn();

    num_filters = ARRAY_SIZE(filterIDs);

    /* Set up file access property list with parallel I/O access,
     * collective metadata reads/writes and the latest library
     * version bounds */
    fapl_id = H5Pcreate(H5P_FILE_ACCESS);
    VRFY((fapl_id >= 0), "FAPL creation succeeded");

    VRFY((H5Pset_fapl_mpio(fapl_id, comm, info) >= 0), "Set FAPL MPIO succeeded");
    VRFY((H5Pset_all_coll_metadata_ops(fapl_id, TRUE) >= 0), "H5Pset_all_coll_metadata_ops succeeded");
    VRFY((H5Pset_coll_metadata_write(fapl_id, TRUE) >= 0), "H5Pset_coll_metadata_write succeeded");

    VRFY((H5Pset_libver_bounds(fapl_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0),
         "Set libver bounds succeeded");

    /*
     * Set up Paged and Persistent Free Space Management
     */
    fcpl_id = H5Pcreate(H5P_FILE_CREATE);
    VRFY((fcpl_id >= 0), "FCPL creation succeeded");

    VRFY((H5Pset_file_space_strategy(fcpl_id, H5F_FSPACE_STRATEGY_PAGE, TRUE, 1) >= 0),
         "H5Pset_file_space_strategy succeeded");

    VRFY((h5_fixname(FILENAME[0], fapl_id, filenames[0], sizeof(filenames[0])) != NULL),
         "Test file name created");

    file_id = H5Fcreate(filenames[0], H5F_ACC_TRUNC, fcpl_id, fapl_id);
    VRFY((file_id >= 0), "Test file creation succeeded");

    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");
    file_id = H5I_INVALID_HID;

    /* Create property list for collective dataset write */
    dxpl_id = H5Pcreate(H5P_DATASET_XFER);
    VRFY((dxpl_id >= 0), "DXPL creation succeeded");

    VRFY((H5Pset_dxpl_mpio(dxpl_id, H5FD_MPIO_COLLECTIVE) >= 0), "H5Pset_dxpl_mpio succeeded");

    /* Create DCPL for dataset creation */
    dcpl_id = H5Pcreate(H5P_DATASET_CREATE);
    VRFY((dcpl_id >= 0), "DCPL creation succeeded");

    /* Run tests with all available filters */
    for (cur_filter_idx = 0; cur_filter_idx < num_filters; cur_filter_idx++) {
        H5FD_mpio_chunk_opt_t chunk_opt;
        H5Z_filter_t          cur_filter = filterIDs[cur_filter_idx];

        /* Run tests with both linked-chunk and multi-chunk I/O */
        for (chunk_opt = H5FD_MPIO_CHUNK_ONE_IO; chunk_opt <= H5FD_MPIO_CHUNK_MULTI_IO; chunk_opt++) {
            H5D_alloc_time_t space_alloc_time;

            /* Run tests with all available space allocation times */
            for (space_alloc_time = H5D_ALLOC_TIME_EARLY; space_alloc_time <= H5D_ALLOC_TIME_INCR;
                 space_alloc_time++) {
                const char *alloc_time;
                unsigned    filter_config;
                htri_t      filter_avail;
                size_t      i;
                char        group_name[512];

                switch (space_alloc_time) {
                    case H5D_ALLOC_TIME_EARLY:
                        alloc_time = "Early";
                        break;
                    case H5D_ALLOC_TIME_LATE:
                        alloc_time = "Late";
                        break;
                    case H5D_ALLOC_TIME_INCR:
                        alloc_time = "Incremental";
                        break;
                    default:
                        alloc_time = "Unknown";
                }

                if (MAINPROCESS)
                    HDprintf("== Running tests with filter '%s' using '%s' and '%s' allocation time ==\n\n",
                             filterNames[cur_filter_idx],
                             H5FD_MPIO_CHUNK_ONE_IO == chunk_opt ? "Linked-Chunk I/O" : "Multi-Chunk I/O",
                             alloc_time);

                /* Make sure current filter is available before testing with it */
                filter_avail = H5Zfilter_avail(cur_filter);
                VRFY((filter_avail >= 0), "H5Zfilter_avail succeeded");

                if (!filter_avail) {
                    if (MAINPROCESS)
                        HDprintf(" ** SKIPPED tests with filter '%s' - filter unavailable **\n\n",
                                 filterNames[cur_filter_idx]);
                    continue;
                }

                /* Get the current filter's info */
                VRFY((H5Zget_filter_info(cur_filter, &filter_config) >= 0), "H5Zget_filter_info succeeded");

                /* Determine if filter is encode-enabled */
                if (0 == (filter_config & H5Z_FILTER_CONFIG_ENCODE_ENABLED)) {
                    if (MAINPROCESS)
                        HDprintf(" ** SKIPPED tests with filter '%s' - filter not encode-enabled **\n\n",
                                 filterNames[cur_filter_idx]);
                    continue;
                }

                /* Set space allocation time */
                VRFY((H5Pset_alloc_time(dcpl_id, space_alloc_time) >= 0), "H5Pset_alloc_time succeeded");

                /* Set chunk I/O optimization method */
                VRFY((H5Pset_dxpl_mpio_chunk_opt(dxpl_id, chunk_opt) >= 0),
                     "H5Pset_dxpl_mpio_chunk_opt succeeded");

                /* Create a group to hold all the datasets for this combination
                 * of filter and chunk optimization mode. Then, close the file
                 * again since some tests may need to open the file in a special
                 * way, like on rank 0 only */
                file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, fapl_id);
                VRFY((file_id >= 0), "H5Fopen succeeded");

                HDsnprintf(group_name, sizeof(group_name), "%s_%s_%s", filterNames[cur_filter_idx],
                           H5FD_MPIO_CHUNK_ONE_IO == chunk_opt ? "linked-chunk-io" : "multi-chunk-io",
                           alloc_time);

                group_id = H5Gcreate2(file_id, group_name, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
                VRFY((group_id >= 0), "H5Gcreate2 succeeded");

                VRFY((H5Gclose(group_id) >= 0), "H5Gclose failed");
                group_id = H5I_INVALID_HID;

                VRFY((H5Fclose(file_id) >= 0), "H5Fclose succeeded");
                file_id = H5I_INVALID_HID;

                for (i = 0; i < ARRAY_SIZE(tests); i++) {
                    test_func func = tests[i];

                    if (MPI_SUCCESS == (mpi_code = MPI_Barrier(comm))) {
                        func(group_name, cur_filter, fapl_id, dcpl_id, dxpl_id);
                    }
                    else {
                        if (MAINPROCESS)
                            MESG("MPI_Barrier failed");
                        nerrors++;
                    }
                }

                if (MAINPROCESS)
                    HDputs("");
            }
        }
    }

    VRFY((H5Pclose(dcpl_id) >= 0), "DCPL close succeeded");
    dcpl_id = H5I_INVALID_HID;

    VRFY((H5Pclose(dxpl_id) >= 0), "DXPL close succeeded");
    dxpl_id = H5I_INVALID_HID;

    if (nerrors)
        goto exit;

    if (MAINPROCESS)
        HDputs("All Parallel Filters tests passed\n");

exit:
    if (nerrors)
        if (MAINPROCESS)
            HDprintf("*** %d TEST ERROR%s OCCURRED ***\n", nerrors, nerrors > 1 ? "S" : "");

    TestAlarmOff();

    h5_clean_files(FILENAME, fapl_id);
    fapl_id = H5I_INVALID_HID;

    if (dcpl_id >= 0)
        VRFY((H5Pclose(dcpl_id) >= 0), "H5Pclose succeeded");
    if (dxpl_id >= 0)
        VRFY((H5Pclose(dxpl_id) >= 0), "H5Pclose succeeded");
    if (fapl_id >= 0)
        VRFY((H5Pclose(fapl_id) >= 0), "H5Pclose succeeded");
    if (fcpl_id >= 0)
        VRFY((H5Pclose(fcpl_id) >= 0), "H5Pclose succeeded");
    if (group_id >= 0)
        VRFY((H5Gclose(group_id) >= 0), "H5Gclose succeeded");
    if (file_id >= 0)
        VRFY((H5Fclose(file_id) >= 0), "H5Fclose succeeded");

    H5close();

    MPI_Finalize();

    exit((nerrors ? EXIT_FAILURE : EXIT_SUCCESS));
}
