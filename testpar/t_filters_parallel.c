/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 *
 * This file contains tests for writing to and reading from
 * datasets in parallel with filters applied to the data.
 */

#include "t_filters_parallel.h"

static const char *FILENAME[] = {"t_filters_parallel", NULL};
static char        filenames[1][256];

static MPI_Comm comm     = MPI_COMM_WORLD;
static MPI_Info info     = MPI_INFO_NULL;
static int      mpi_rank = 0;
static int      mpi_size = 0;

static int test_express_level_g;

int nerrors = 0;

/* Arrays of filter ID values and filter names (should match each other) */
static H5Z_filter_t filterIDs[] = {
    H5Z_FILTER_DEFLATE, H5Z_FILTER_SHUFFLE, H5Z_FILTER_FLETCHER32,
    H5Z_FILTER_SZIP,    H5Z_FILTER_NBIT,    H5Z_FILTER_SCALEOFFSET,
};

static const char *filterNames[] = {"Deflate", "Shuffle", "Fletcher32", "SZIP", "Nbit", "ScaleOffset"};

/* Typedef for filter arguments for user-defined filters */
typedef struct filter_options_t {
    unsigned int       flags;
    size_t             cd_nelmts;
    const unsigned int cd_values[];
} filter_options_t;

/* Enum for running these tests in different modes */
typedef enum test_mode_t {
    USE_SINGLE_DATASET,                   /* Operate on a single dataset with H5Dwrite/read */
    USE_MULTIPLE_DATASETS,                /* Operate on multiple datasets with H5Dwrite_multi/read_multi */
    USE_MULTIPLE_DATASETS_MIXED_FILTERED, /* Operate on multiple datasets with H5Dwrite_multi/read_multi
                                             and with some of the datasets being unfiltered */
    TEST_MODE_SENTINEL
} test_mode_t;

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

/* Function pointer typedef for test functions */
typedef void (*test_func)(const char *parent_group, H5Z_filter_t filter_id, hid_t fapl_id, hid_t dcpl_id,
                          hid_t dxpl_id, test_mode_t test_mode);

static herr_t set_dcpl_filter(hid_t dcpl_id, H5Z_filter_t filter_id, filter_options_t *filter_options);
static void   verify_space_alloc_status(size_t num_dsets, hid_t *dset_ids, hid_t dcpl_id,
                                        num_chunks_written_t chunks_written);
static void   verify_chunk_opt_status(size_t num_dsets, test_mode_t test_mode, bool any_io, bool any_filters,
                                      bool collective, bool unalloc_read, bool did_alloc, hid_t dxpl_id);
static const char *test_mode_to_string(test_mode_t test_mode);

static void create_datasets(hid_t parent_obj_id, const char *dset_name, hid_t type_id, hid_t filespace_id,
                            hid_t dcpl_id, test_mode_t test_mode, size_t *num_dsets, hid_t *dset_ids);
static void open_datasets(hid_t parent_obj_id, const char *dset_name, size_t num_dsets, test_mode_t test_mode,
                          hid_t *dset_ids);
static void write_datasets(size_t num_dsets, hid_t *dset_ids, hid_t type_id, hid_t mspace_id,
                           hid_t *fspace_ids, hid_t dcpl_id, hid_t dxpl_id, const void **bufs,
                           test_mode_t test_mode, bool any_io, bool collective, bool overwrite);
static void read_datasets(size_t num_dsets, hid_t *dset_ids, hid_t type_id, hid_t mspace_id, hid_t fspace_id,
                          hid_t dcpl_id, hid_t dxpl_id, void **bufs, test_mode_t test_mode, bool any_io,
                          bool collective, bool all_uninit_read);

static void select_hyperslab(size_t num_dsets, hid_t *dset_ids, hsize_t *start, hsize_t *stride,
                             hsize_t *count, hsize_t *block, hid_t *fspace_ids);
static void select_all(size_t num_dsets, hid_t *dset_ids, hid_t *fspace_ids);
static void select_none(size_t num_dsets, hid_t *dset_ids, hid_t *fspace_ids);
static void select_elements(size_t num_dsets, hid_t *dset_ids, size_t num_points, hsize_t *coords,
                            hid_t *fspace_ids);

#ifdef H5_HAVE_PARALLEL_FILTERED_WRITES
/* Tests for writing data in parallel */
static void test_write_one_chunk_filtered_dataset(const char *parent_group, H5Z_filter_t filter_id,
                                                  hid_t fapl_id, hid_t dcpl_id, hid_t dxpl_id,
                                                  test_mode_t test_mode);
static void test_write_filtered_dataset_no_overlap(const char *parent_group, H5Z_filter_t filter_id,
                                                   hid_t fapl_id, hid_t dcpl_id, hid_t dxpl_id,
                                                   test_mode_t test_mode);
static void test_write_filtered_dataset_no_overlap_partial(const char *parent_group, H5Z_filter_t filter_id,
                                                           hid_t fapl_id, hid_t dcpl_id, hid_t dxpl_id,
                                                           test_mode_t test_mode);
static void test_write_filtered_dataset_overlap(const char *parent_group, H5Z_filter_t filter_id,
                                                hid_t fapl_id, hid_t dcpl_id, hid_t dxpl_id,
                                                test_mode_t test_mode);
static void test_write_filtered_dataset_single_unlim_dim_no_overlap(const char  *parent_group,
                                                                    H5Z_filter_t filter_id, hid_t fapl_id,
                                                                    hid_t dcpl_id, hid_t dxpl_id,
                                                                    test_mode_t test_mode);
static void test_write_filtered_dataset_single_unlim_dim_overlap(const char  *parent_group,
                                                                 H5Z_filter_t filter_id, hid_t fapl_id,
                                                                 hid_t dcpl_id, hid_t dxpl_id,
                                                                 test_mode_t test_mode);
static void test_write_filtered_dataset_multi_unlim_dim_no_overlap(const char  *parent_group,
                                                                   H5Z_filter_t filter_id, hid_t fapl_id,
                                                                   hid_t dcpl_id, hid_t dxpl_id,
                                                                   test_mode_t test_mode);
static void test_write_filtered_dataset_multi_unlim_dim_overlap(const char  *parent_group,
                                                                H5Z_filter_t filter_id, hid_t fapl_id,
                                                                hid_t dcpl_id, hid_t dxpl_id,
                                                                test_mode_t test_mode);
static void test_write_filtered_dataset_single_no_selection(const char *parent_group, H5Z_filter_t filter_id,
                                                            hid_t fapl_id, hid_t dcpl_id, hid_t dxpl_id,
                                                            test_mode_t test_mode);
static void test_write_filtered_dataset_all_no_selection(const char *parent_group, H5Z_filter_t filter_id,
                                                         hid_t fapl_id, hid_t dcpl_id, hid_t dxpl_id,
                                                         test_mode_t test_mode);
static void test_write_filtered_dataset_point_selection(const char *parent_group, H5Z_filter_t filter_id,
                                                        hid_t fapl_id, hid_t dcpl_id, hid_t dxpl_id,
                                                        test_mode_t test_mode);
static void test_write_filtered_dataset_interleaved_write(const char *parent_group, H5Z_filter_t filter_id,
                                                          hid_t fapl_id, hid_t dcpl_id, hid_t dxpl_id,
                                                          test_mode_t test_mode);
static void test_write_transformed_filtered_dataset_no_overlap(const char  *parent_group,
                                                               H5Z_filter_t filter_id, hid_t fapl_id,
                                                               hid_t dcpl_id, hid_t dxpl_id,
                                                               test_mode_t test_mode);
static void test_write_3d_filtered_dataset_no_overlap_separate_pages(const char  *parent_group,
                                                                     H5Z_filter_t filter_id, hid_t fapl_id,
                                                                     hid_t dcpl_id, hid_t dxpl_id,
                                                                     test_mode_t test_mode);
static void test_write_3d_filtered_dataset_no_overlap_same_pages(const char  *parent_group,
                                                                 H5Z_filter_t filter_id, hid_t fapl_id,
                                                                 hid_t dcpl_id, hid_t dxpl_id,
                                                                 test_mode_t test_mode);
static void test_write_3d_filtered_dataset_overlap(const char *parent_group, H5Z_filter_t filter_id,
                                                   hid_t fapl_id, hid_t dcpl_id, hid_t dxpl_id,
                                                   test_mode_t test_mode);
static void test_write_cmpd_filtered_dataset_no_conversion_unshared(const char  *parent_group,
                                                                    H5Z_filter_t filter_id, hid_t fapl_id,
                                                                    hid_t dcpl_id, hid_t dxpl_id,
                                                                    test_mode_t test_mode);
static void test_write_cmpd_filtered_dataset_no_conversion_shared(const char  *parent_group,
                                                                  H5Z_filter_t filter_id, hid_t fapl_id,
                                                                  hid_t dcpl_id, hid_t dxpl_id,
                                                                  test_mode_t test_mode);
static void test_write_cmpd_filtered_dataset_type_conversion_unshared(const char  *parent_group,
                                                                      H5Z_filter_t filter_id, hid_t fapl_id,
                                                                      hid_t dcpl_id, hid_t dxpl_id,
                                                                      test_mode_t test_mode);
static void test_write_cmpd_filtered_dataset_type_conversion_shared(const char  *parent_group,
                                                                    H5Z_filter_t filter_id, hid_t fapl_id,
                                                                    hid_t dcpl_id, hid_t dxpl_id,
                                                                    test_mode_t test_mode);
#endif

/* Tests for reading data in parallel */
static void test_read_one_chunk_filtered_dataset(const char *parent_group, H5Z_filter_t filter_id,
                                                 hid_t fapl_id, hid_t dcpl_id, hid_t dxpl_id,
                                                 test_mode_t test_mode);
static void test_read_filtered_dataset_no_overlap(const char *parent_group, H5Z_filter_t filter_id,
                                                  hid_t fapl_id, hid_t dcpl_id, hid_t dxpl_id,
                                                  test_mode_t test_mode);
static void test_read_filtered_dataset_overlap(const char *parent_group, H5Z_filter_t filter_id,
                                               hid_t fapl_id, hid_t dcpl_id, hid_t dxpl_id,
                                               test_mode_t test_mode);
static void test_read_filtered_dataset_single_no_selection(const char *parent_group, H5Z_filter_t filter_id,
                                                           hid_t fapl_id, hid_t dcpl_id, hid_t dxpl_id,
                                                           test_mode_t test_mode);
static void test_read_filtered_dataset_all_no_selection(const char *parent_group, H5Z_filter_t filter_id,
                                                        hid_t fapl_id, hid_t dcpl_id, hid_t dxpl_id,
                                                        test_mode_t test_mode);
static void test_read_filtered_dataset_point_selection(const char *parent_group, H5Z_filter_t filter_id,
                                                       hid_t fapl_id, hid_t dcpl_id, hid_t dxpl_id,
                                                       test_mode_t test_mode);
static void test_read_filtered_dataset_interleaved_read(const char *parent_group, H5Z_filter_t filter_id,
                                                        hid_t fapl_id, hid_t dcpl_id, hid_t dxpl_id,
                                                        test_mode_t test_mode);
static void test_read_transformed_filtered_dataset_no_overlap(const char  *parent_group,
                                                              H5Z_filter_t filter_id, hid_t fapl_id,
                                                              hid_t dcpl_id, hid_t dxpl_id,
                                                              test_mode_t test_mode);
static void test_read_3d_filtered_dataset_no_overlap_separate_pages(const char  *parent_group,
                                                                    H5Z_filter_t filter_id, hid_t fapl_id,
                                                                    hid_t dcpl_id, hid_t dxpl_id,
                                                                    test_mode_t test_mode);
static void test_read_3d_filtered_dataset_no_overlap_same_pages(const char  *parent_group,
                                                                H5Z_filter_t filter_id, hid_t fapl_id,
                                                                hid_t dcpl_id, hid_t dxpl_id,
                                                                test_mode_t test_mode);
static void test_read_3d_filtered_dataset_overlap(const char *parent_group, H5Z_filter_t filter_id,
                                                  hid_t fapl_id, hid_t dcpl_id, hid_t dxpl_id,
                                                  test_mode_t test_mode);
static void test_read_cmpd_filtered_dataset_no_conversion_unshared(const char  *parent_group,
                                                                   H5Z_filter_t filter_id, hid_t fapl_id,
                                                                   hid_t dcpl_id, hid_t dxpl_id,
                                                                   test_mode_t test_mode);
static void test_read_cmpd_filtered_dataset_no_conversion_shared(const char  *parent_group,
                                                                 H5Z_filter_t filter_id, hid_t fapl_id,
                                                                 hid_t dcpl_id, hid_t dxpl_id,
                                                                 test_mode_t test_mode);
static void test_read_cmpd_filtered_dataset_type_conversion_unshared(const char  *parent_group,
                                                                     H5Z_filter_t filter_id, hid_t fapl_id,
                                                                     hid_t dcpl_id, hid_t dxpl_id,
                                                                     test_mode_t test_mode);
static void test_read_cmpd_filtered_dataset_type_conversion_shared(const char  *parent_group,
                                                                   H5Z_filter_t filter_id, hid_t fapl_id,
                                                                   hid_t dcpl_id, hid_t dxpl_id,
                                                                   test_mode_t test_mode);

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
                                            hid_t dcpl_id, hid_t dxpl_id, test_mode_t test_mode);

#ifdef H5_HAVE_PARALLEL_FILTERED_WRITES
static void test_write_parallel_read_serial(const char *parent_group, H5Z_filter_t filter_id, hid_t fapl_id,
                                            hid_t dcpl_id, hid_t dxpl_id, test_mode_t test_mode);

/* Other miscellaneous tests */
static void test_shrinking_growing_chunks(const char *parent_group, H5Z_filter_t filter_id, hid_t fapl_id,
                                          hid_t dcpl_id, hid_t dxpl_id, test_mode_t test_mode);
static void test_edge_chunks_no_overlap(const char *parent_group, H5Z_filter_t filter_id, hid_t fapl_id,
                                        hid_t dcpl_id, hid_t dxpl_id, test_mode_t test_mode);
static void test_edge_chunks_overlap(const char *parent_group, H5Z_filter_t filter_id, hid_t fapl_id,
                                     hid_t dcpl_id, hid_t dxpl_id, test_mode_t test_mode);
static void test_fill_values(const char *parent_group, H5Z_filter_t filter_id, hid_t fapl_id, hid_t dcpl_id,
                             hid_t dxpl_id, test_mode_t test_mode);
static void test_fill_value_undefined(const char *parent_group, H5Z_filter_t filter_id, hid_t fapl_id,
                                      hid_t dcpl_id, hid_t dxpl_id, test_mode_t test_mode);
static void test_fill_time_never(const char *parent_group, H5Z_filter_t filter_id, hid_t fapl_id,
                                 hid_t dcpl_id, hid_t dxpl_id, test_mode_t test_mode);
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
            size_t   chunk_nelemts;

            VRFY(H5Pget_chunk(dcpl_id, H5S_MAX_RANK, chunk_dims) >= 0, "H5Pget_chunk succeeded");

            chunk_nelemts = 1;
            for (size_t i = 0; i < H5S_MAX_RANK; i++)
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
static void
verify_space_alloc_status(size_t num_dsets, hid_t *dset_ids, hid_t dcpl_id,
                          num_chunks_written_t chunks_written)
{
    H5D_space_status_t space_status;
    H5D_alloc_time_t   alloc_time;

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        hid_t dset_dcpl;
        int   nfilters;

        /* Check if this particular dataset has any filters applied */
        dset_dcpl = H5Dget_create_plist(dset_ids[dset_idx]);
        VRFY((dset_dcpl >= 0), "H5Dget_create_plist");

        nfilters = H5Pget_nfilters(dset_dcpl);
        VRFY((nfilters >= 0), "H5Pget_nfilters");

        VRFY((H5Pclose(dset_dcpl) >= 0), "H5Pclose");

        /*
         * Only verify space allocation status when there are filters
         * in the dataset's filter pipeline. When filters aren't in the
         * pipeline, the space allocation time and status can vary based
         * on whether the file was created in parallel or serial mode.
         */
        if (nfilters == 0)
            return;

        VRFY((H5Pget_alloc_time(dcpl_id, &alloc_time) >= 0), "H5Pget_alloc_time succeeded");
        VRFY((H5Dget_space_status(dset_ids[dset_idx], &space_status) >= 0), "H5Dget_space_status succeeded");

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
                else if (chunks_written == NO_CHUNKS_WRITTEN) {
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
                }
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
            case H5D_ALLOC_TIME_ERROR:
            default:
                if (MAINPROCESS)
                    MESG("unknown space allocation time");
                MPI_Abort(MPI_COMM_WORLD, 1);
        }
    }
}

/*
 * Function to verify the status of the chunk I/O optimization method
 * used when the multi-dataset I/O API routines were used. As long as
 * multi-dataset I/O was actually performed, the library should return
 * that linked-chunk I/O was performed. Otherwise, if datasets were
 * processed one at a time, the library should return that multi-chunk
 * I/O was performed.
 */
static void
verify_chunk_opt_status(size_t num_dsets, test_mode_t test_mode, bool any_io, bool any_filters,
                        bool collective, bool unalloc_read, bool did_alloc, hid_t dxpl_id)
{
    H5D_mpio_actual_chunk_opt_mode_t chunk_opt_mode;
    H5D_selection_io_mode_t          sel_io_mode;
    uint32_t                         actual_sel_io_mode;
    uint32_t                         actual_sel_io_mode_reduced;
    uint32_t                         no_sel_io_cause = 0;
    int                              mpi_code;
    herr_t                           ret;

    if (H5P_DEFAULT != dxpl_id) {
        ret = H5Pget_mpio_actual_chunk_opt_mode(dxpl_id, &chunk_opt_mode);
        VRFY((ret >= 0), "H5Pget_mpio_actual_chunk_opt_mode succeeded");

        ret = H5Pget_selection_io(dxpl_id, &sel_io_mode);
        VRFY((ret >= 0), "H5Pget_selection_io succeeded");

        if (sel_io_mode == H5D_SELECTION_IO_MODE_DEFAULT || sel_io_mode == H5D_SELECTION_IO_MODE_ON) {
            ret = H5Pget_no_selection_io_cause(dxpl_id, &no_sel_io_cause);
            VRFY((ret >= 0), "H5Pget_no_selection_io_cause succeeded");
        }

        if (num_dsets == 0) {
            /*
             * num_dsets == 0 implies that the write call was expected to
             * failed and did so. Verify that the library returns
             * H5D_MPIO_NO_CHUNK_OPTIMIZATION as the chunk I/O optimization
             * method
             */
            VRFY((H5D_MPIO_NO_CHUNK_OPTIMIZATION == chunk_opt_mode),
                 "verified I/O optimization was H5D_MPIO_NO_CHUNK_OPTIMIZATION");
        }
        else if (num_dsets == 1) {
            /*
             * If selection I/O is set to ON and was actually performed, just
             * verify that the library returns that either linked-chunk or
             * multi-chunk I/O was performed. Otherwise, any of the optimization
             * methods could potentially be returned by the library.
             */
            if ((sel_io_mode == H5D_SELECTION_IO_MODE_DEFAULT || sel_io_mode == H5D_SELECTION_IO_MODE_ON) &&
                !no_sel_io_cause) {
                VRFY((H5D_MPIO_NO_CHUNK_OPTIMIZATION != chunk_opt_mode),
                     "verified I/O optimization wasn't H5D_MPIO_NO_CHUNK_OPTIMIZATION");
                VRFY((H5D_MPIO_LINK_CHUNK == chunk_opt_mode || H5D_MPIO_MULTI_CHUNK == chunk_opt_mode),
                     "verified I/O optimization was linked-chunk I/O or multi-chunk I/O");
            }
        }
        else {
            /*
             * If selection I/O is set to ON and was actually performed, verify
             * that the library returns that linked-chunk I/O was performed.
             * Otherwise, any of the optimization methods could potentially be
             * returned by the library.
             */
            if ((sel_io_mode == H5D_SELECTION_IO_MODE_DEFAULT || sel_io_mode == H5D_SELECTION_IO_MODE_ON) &&
                !no_sel_io_cause) {
                VRFY((H5D_MPIO_LINK_CHUNK == chunk_opt_mode),
                     "verified I/O optimization was linked-chunk I/O");
            }
        }

        /* Verify actual selection I/O mode */
        ret = H5Pget_actual_selection_io_mode(dxpl_id, &actual_sel_io_mode);
        VRFY((ret >= 0), "H5Pget_actual_selection_io_mode succeeded");

        /* Reduce results to process 0 (bitwise OR so we get all I/O types) */
        mpi_code =
            MPI_Reduce(&actual_sel_io_mode, &actual_sel_io_mode_reduced, 1, MPI_UINT32_T, MPI_BOR, 0, comm);
        VRFY((MPI_SUCCESS == mpi_code), "MPI_Reduce succeeded");

        /* Verify selection I/O mode on rank 0 */
        if (mpi_rank == 0) {
            /* No actual I/O performed, the only reported I/O will be from allocation which is vector I/O,
             * even if "no" datasets were involved (num_dsets == 0 implies the call was expected to fail,
             * but it fails after allocation).
             * Also if the test mode is mixed filtered and unfiltered and the call did not fail, then there
             * will always be an I/O callback made with raw data. This is because unfiltered datasets fall
             * back to scalar I/O when mixed with filtered, and scalar I/O reports an I/O call was made even
             * with a size of 0 bytes, while vector I/O does not report I/O was made if passed 0 vector
             * elements (because no elements were raw data), which is what happens when performing I/O on a
             * filtered dataset with no selection. Vector I/O does report an I/O call was made if passed a raw
             * data element of size 0, so this is consistent. */
            if (!any_io) {
                if (did_alloc && (num_dsets > 0 && test_mode == USE_MULTIPLE_DATASETS_MIXED_FILTERED)) {
                    VRFY((H5D_VECTOR_IO | H5D_SCALAR_IO) == actual_sel_io_mode_reduced,
                         "verified actual selection I/O mode was vector and scalar I/O");
                }
                else if (did_alloc) {
                    VRFY(H5D_VECTOR_IO == actual_sel_io_mode_reduced,
                         "verified actual selection I/O mode was vector I/O");
                }
                else if (num_dsets > 0 && test_mode == USE_MULTIPLE_DATASETS_MIXED_FILTERED) {
                    VRFY(H5D_SCALAR_IO == actual_sel_io_mode_reduced,
                         "verified actual selection I/O mode was scalar I/O");
                }
                else
                    VRFY(0 == actual_sel_io_mode_reduced,
                         "verified actual selection I/O mode was 0 (no I/O)");
            }
            /* No filters, library should have used selection I/O if enabled, scalar I/O otherwise */
            else if (!any_filters) {
                assert(!unalloc_read && !did_alloc);
                if (sel_io_mode == H5D_SELECTION_IO_MODE_DEFAULT || sel_io_mode == H5D_SELECTION_IO_MODE_ON)
                    VRFY(H5D_SELECTION_IO == actual_sel_io_mode_reduced,
                         "verified actual selection I/O mode was selection I/O");
                else
                    VRFY(H5D_SCALAR_IO == actual_sel_io_mode_reduced,
                         "verified actual selection I/O mode was scalar I/O");
            }
            /* Independent I/O, library should have done no I/O if reading from unallocated datasets, scalar
             * I/O otherwise, since filtered I/O is only supported with scalar I/O in independent/serial */
            else if (!collective) {
                if (unalloc_read)
                    VRFY(0 == actual_sel_io_mode_reduced,
                         "verified actual selection I/O mode was 0 (no I/O)");
                else
                    VRFY(H5D_SCALAR_IO == actual_sel_io_mode_reduced,
                         "verified actual selection I/O mode was scalar I/O");
            }
            else
                switch (test_mode) {
                    case USE_SINGLE_DATASET:
                    case USE_MULTIPLE_DATASETS:
                        /* Collective case with only filtered datasets.
                         * If we're reading from an unallocated dataset then there
                         * should be no actual I/O.
                         * Otherwise, only vector I/O is reported whether or not
                         * allocation happened. */
                        if (unalloc_read)
                            VRFY(0 == actual_sel_io_mode_reduced,
                                 "verified actual selection I/O mode was 0 (no I/O)");
                        else { /* did_alloc || !unalloc_read */
                            VRFY(H5D_VECTOR_IO == actual_sel_io_mode_reduced,
                                 "verified actual selection I/O mode was vector I/O");
                        }
                        break;

                    case USE_MULTIPLE_DATASETS_MIXED_FILTERED:
                        /* Collective case with mixed filtered and unfiltered datasets. If we're reading from
                         * a unallocated datasets then there should be scalar I/O from reading the unfilitered
                         * datasets, since they are always allocated in parallel. Otherwise there should be
                         * vector I/O from the filtered datasets and scalar I/O from the unfiltered datasets.
                         */
                        if (unalloc_read)
                            VRFY(H5D_SCALAR_IO == actual_sel_io_mode_reduced,
                                 "verified actual selection I/O mode was scalar I/O");
                        else
                            VRFY((H5D_SCALAR_IO | H5D_VECTOR_IO) == actual_sel_io_mode_reduced,
                                 "verified actual selection I/O mode was scalar and vector I/O");
                        break;

                    case TEST_MODE_SENTINEL:
                    default:
                        printf("Invalid test mode\n");
                        fflush(stdout);
                        MPI_Abort(MPI_COMM_WORLD, -1);
                }
        }
    }
}

static const char *
test_mode_to_string(test_mode_t test_mode)
{
    switch (test_mode) {
        case USE_SINGLE_DATASET:
            return "USE_SINGLE_DATASET";
        case USE_MULTIPLE_DATASETS:
            return "USE_MULTIPLE_DATASETS";
        case USE_MULTIPLE_DATASETS_MIXED_FILTERED:
            return "USE_MULTIPLE_DATASETS_MIXED_FILTERED";
        case TEST_MODE_SENTINEL:
        default:
            return "INVALID";
    }
}

/*
 * Utility routine to create the datasets for each test,
 * after adjusting for the current test mode
 */
static void
create_datasets(hid_t parent_obj_id, const char *dset_name, hid_t type_id, hid_t filespace_id, hid_t dcpl_id,
                test_mode_t test_mode, size_t *num_dsets, hid_t *dset_ids)
{
    const char *dset_name_ptr   = NULL;
    hid_t       unfiltered_dcpl = H5I_INVALID_HID;
    char        dset_name_multi_buf[512];
    int         n_dsets      = 0;
    int         n_unfiltered = 0;

    VRFY((num_dsets != NULL), "verify num_dsets");
    VRFY((dset_ids != NULL), "verify dset_ids");

    for (size_t dset_idx = 0; dset_idx < MAX_NUM_DSETS_MULTI; dset_idx++)
        dset_ids[dset_idx] = H5I_INVALID_HID;

    switch (test_mode) {
        case USE_SINGLE_DATASET:
            dset_name_ptr = dset_name;
            n_dsets       = 1;
            break;

        case USE_MULTIPLE_DATASETS:
        case USE_MULTIPLE_DATASETS_MIXED_FILTERED:
            dset_name_ptr = dset_name_multi_buf;

            if (MAINPROCESS)
                n_dsets = (rand() % (MAX_NUM_DSETS_MULTI - 1)) + 2;

            if (mpi_size > 1)
                VRFY((MPI_SUCCESS == MPI_Bcast(&n_dsets, 1, MPI_INT, 0, comm)), "MPI_Bcast succeeded");

            /* Select between 1 and (n_dsets - 1) datasets to be unfiltered */
            if (test_mode == USE_MULTIPLE_DATASETS_MIXED_FILTERED) {
                if (MAINPROCESS)
                    n_unfiltered = (rand() % (n_dsets - 1)) + 1;

                if (mpi_size > 1)
                    VRFY((MPI_SUCCESS == MPI_Bcast(&n_unfiltered, 1, MPI_INT, 0, comm)),
                         "MPI_Bcast succeeded");

                unfiltered_dcpl = H5Pcopy(dcpl_id);
                VRFY((unfiltered_dcpl >= 0), "H5Pcopy succeeded");

                VRFY((H5Premove_filter(unfiltered_dcpl, H5Z_FILTER_ALL) >= 0), "H5Premove_filter succeeded");
            }
            break;

        case TEST_MODE_SENTINEL:
        default:
            if (MAINPROCESS)
                printf("Invalid test mode\n");
            fflush(stdout);
            MPI_Abort(MPI_COMM_WORLD, -1);
    }

    for (size_t dset_idx = 0; dset_idx < (size_t)n_dsets; dset_idx++) {
        hid_t curr_dcpl = dcpl_id;

        /* Add suffix to dataset name for multi-dataset tests */
        if (test_mode == USE_MULTIPLE_DATASETS || test_mode == USE_MULTIPLE_DATASETS_MIXED_FILTERED)
            snprintf(dset_name_multi_buf, 512, "%s_%d", dset_name, (int)dset_idx);

        /* Determine if this should be an unfiltered dataset */
        if ((test_mode == USE_MULTIPLE_DATASETS_MIXED_FILTERED) && (n_unfiltered > 0)) {
            size_t dsets_left = (size_t)n_dsets - dset_idx;
            bool   unfiltered;

            /*
             * The number of unfiltered datasets should never be
             * greater than the number of datasets left to create
             */
            VRFY(((size_t)n_unfiltered <= dsets_left), "number of unfiltered datasets sanity check");

            /*
             * If the number of unfiltered datasets left is the
             * same as the number of datasets left, create the
             * remaining datasets as unfiltered datasets. Otherwise,
             * randomly determine if a dataset will be unfiltered.
             */
            if (MAINPROCESS)
                unfiltered = ((size_t)n_unfiltered == dsets_left) || ((rand() % 2) == 0);

            if (mpi_size > 1)
                VRFY((MPI_SUCCESS == MPI_Bcast(&unfiltered, 1, MPI_C_BOOL, 0, comm)), "MPI_Bcast succeeded");

            if (unfiltered) {
                curr_dcpl = unfiltered_dcpl;
                n_unfiltered--;
            }
        }

        dset_ids[dset_idx] = H5Dcreate2(parent_obj_id, dset_name_ptr, type_id, filespace_id, H5P_DEFAULT,
                                        curr_dcpl, H5P_DEFAULT);

        VRFY((dset_ids[dset_idx] >= 0), "Dataset creation succeeded");
    }

    if (unfiltered_dcpl >= 0)
        VRFY((H5Pclose(unfiltered_dcpl) >= 0), "H5Pclose succeeded");

    *num_dsets = (size_t)n_dsets;
}

/*
 * Utility routine to open the datasets that were created
 * for each test, after adjusting for the current test mode
 */
static void
open_datasets(hid_t parent_obj_id, const char *dset_name, size_t num_dsets, test_mode_t test_mode,
              hid_t *dset_ids)
{
    const char *dset_name_ptr = NULL;
    char        dset_name_multi_buf[512];

    VRFY((dset_ids != NULL), "verify dset_ids");
    VRFY((num_dsets <= INT_MAX), "verify num_dsets value");

    for (size_t dset_idx = 0; dset_idx < MAX_NUM_DSETS_MULTI; dset_idx++)
        dset_ids[dset_idx] = H5I_INVALID_HID;

    switch (test_mode) {
        case USE_SINGLE_DATASET:
            dset_name_ptr = dset_name;
            break;

        case USE_MULTIPLE_DATASETS:
        case USE_MULTIPLE_DATASETS_MIXED_FILTERED:
            dset_name_ptr = dset_name_multi_buf;
            break;

        case TEST_MODE_SENTINEL:
        default:
            if (MAINPROCESS)
                printf("Invalid test mode\n");
            fflush(stdout);
            MPI_Abort(MPI_COMM_WORLD, -1);
    }

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        /* Add suffix to dataset name for multi-dataset tests */
        if (test_mode == USE_MULTIPLE_DATASETS || test_mode == USE_MULTIPLE_DATASETS_MIXED_FILTERED)
            snprintf(dset_name_multi_buf, 512, "%s_%d", dset_name, (int)dset_idx);

        dset_ids[dset_idx] = H5Dopen2(parent_obj_id, dset_name_ptr, H5P_DEFAULT);

        VRFY((dset_ids[dset_idx] >= 0), "Dataset open succeeded");
    }
}

/*
 * Utility routine to write to the datasets that were created
 * for each test, after adjusting for the current test mode
 */
static void
write_datasets(size_t num_dsets, hid_t *dset_ids, hid_t type_id, hid_t mspace_id, hid_t *fspace_ids,
               hid_t dcpl_id, hid_t dxpl_id, const void **bufs, test_mode_t test_mode, bool any_io,
               bool collective, bool overwrite)
{
    hid_t            mem_type_ids[MAX_NUM_DSETS_MULTI];
    hid_t            mem_space_ids[MAX_NUM_DSETS_MULTI];
    H5D_alloc_time_t alloc_time = H5D_ALLOC_TIME_DEFAULT;

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        mem_type_ids[dset_idx]  = type_id;
        mem_space_ids[dset_idx] = mspace_id;
    }

    switch (test_mode) {
        case USE_SINGLE_DATASET:
            VRFY((H5Dwrite(dset_ids[0], type_id, mspace_id, fspace_ids[0], dxpl_id, bufs[0]) >= 0),
                 "Dataset write succeeded");
            break;

        case USE_MULTIPLE_DATASETS:
        case USE_MULTIPLE_DATASETS_MIXED_FILTERED:
            VRFY((H5Dwrite_multi(num_dsets, dset_ids, mem_type_ids, mem_space_ids, fspace_ids, dxpl_id,
                                 bufs) >= 0),
                 "Dataset write succeeded");
            break;

        case TEST_MODE_SENTINEL:
        default:
            if (MAINPROCESS)
                printf("Invalid test mode\n");
            fflush(stdout);
            MPI_Abort(MPI_COMM_WORLD, -1);
    }

    if (!overwrite)
        VRFY(H5Pget_alloc_time(dcpl_id, &alloc_time) >= 0, "H5Pget_alloc_time succeeded");

    verify_chunk_opt_status(num_dsets, test_mode, any_io, true, collective, false,
                            !overwrite && (alloc_time == H5D_ALLOC_TIME_LATE), dxpl_id);
}

/*
 * Utility routine to read from the datasets that were created
 * for each test, after adjusting for the current test mode
 */
static void
read_datasets(size_t num_dsets, hid_t *dset_ids, hid_t type_id, hid_t mspace_id, hid_t fspace_id,
              hid_t dcpl_id, hid_t dxpl_id, void **bufs, test_mode_t test_mode, bool any_io, bool collective,
              bool all_uninit_read)
{
    hid_t            mem_type_ids[MAX_NUM_DSETS_MULTI];
    hid_t            mem_space_ids[MAX_NUM_DSETS_MULTI];
    hid_t            file_space_ids[MAX_NUM_DSETS_MULTI];
    H5D_alloc_time_t alloc_time = H5D_ALLOC_TIME_DEFAULT;

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        mem_type_ids[dset_idx]   = type_id;
        mem_space_ids[dset_idx]  = mspace_id;
        file_space_ids[dset_idx] = fspace_id;
    }

    switch (test_mode) {
        case USE_SINGLE_DATASET:
            VRFY((H5Dread(dset_ids[0], type_id, mspace_id, fspace_id, dxpl_id, bufs[0]) >= 0),
                 "Dataset read succeeded");
            break;

        case USE_MULTIPLE_DATASETS:
        case USE_MULTIPLE_DATASETS_MIXED_FILTERED:
            VRFY((H5Dread_multi(num_dsets, dset_ids, mem_type_ids, mem_space_ids, file_space_ids, dxpl_id,
                                bufs) >= 0),
                 "Dataset read succeeded");
            break;

        case TEST_MODE_SENTINEL:
        default:
            if (MAINPROCESS)
                printf("Invalid test mode\n");
            fflush(stdout);
            MPI_Abort(MPI_COMM_WORLD, -1);
    }

    if (all_uninit_read)
        VRFY(H5Pget_alloc_time(dcpl_id, &alloc_time) >= 0, "H5Pget_alloc_time succeeded");

    verify_chunk_opt_status(num_dsets, test_mode, any_io, true, collective,
                            all_uninit_read &&
                                (alloc_time == H5D_ALLOC_TIME_INCR || alloc_time == H5D_ALLOC_TIME_LATE),
                            false, dxpl_id);
}

static void
select_hyperslab(size_t num_dsets, hid_t *dset_ids, hsize_t *start, hsize_t *stride, hsize_t *count,
                 hsize_t *block, hid_t *fspace_ids)
{
    VRFY((fspace_ids != NULL), "verify fspace_ids");

    for (size_t dset_idx = 0; dset_idx < MAX_NUM_DSETS_MULTI; dset_idx++)
        fspace_ids[dset_idx] = H5I_INVALID_HID;

    if (VERBOSE_MED) {
        printf("Process %d is writing with count[ %" PRIuHSIZE ", %" PRIuHSIZE " ], stride[ %" PRIuHSIZE
               ", %" PRIuHSIZE " ], start[ %" PRIuHSIZE ", %" PRIuHSIZE " ], block size[ %" PRIuHSIZE
               ", %" PRIuHSIZE " ]\n",
               mpi_rank, count[0], count[1], stride[0], stride[1], start[0], start[1], block[0], block[1]);
        fflush(stdout);
    }

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        fspace_ids[dset_idx] = H5Dget_space(dset_ids[dset_idx]);
        VRFY((fspace_ids[dset_idx] >= 0), "File dataspace retrieval succeeded");

        VRFY((H5Sselect_hyperslab(fspace_ids[dset_idx], H5S_SELECT_SET, start, stride, count, block) >= 0),
             "Hyperslab selection succeeded");
    }
}

static void
select_all(size_t num_dsets, hid_t *dset_ids, hid_t *fspace_ids)
{
    VRFY((fspace_ids != NULL), "verify fspace_ids");

    for (size_t dset_idx = 0; dset_idx < MAX_NUM_DSETS_MULTI; dset_idx++)
        fspace_ids[dset_idx] = H5I_INVALID_HID;

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        fspace_ids[dset_idx] = H5Dget_space(dset_ids[dset_idx]);
        VRFY((fspace_ids[dset_idx] >= 0), "File dataspace retrieval succeeded");

        VRFY((H5Sselect_all(fspace_ids[dset_idx]) >= 0), "H5Sselect_all succeeded");
    }
}

static void
select_none(size_t num_dsets, hid_t *dset_ids, hid_t *fspace_ids)
{
    VRFY((fspace_ids != NULL), "verify fspace_ids");

    for (size_t dset_idx = 0; dset_idx < MAX_NUM_DSETS_MULTI; dset_idx++)
        fspace_ids[dset_idx] = H5I_INVALID_HID;

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        fspace_ids[dset_idx] = H5Dget_space(dset_ids[dset_idx]);
        VRFY((fspace_ids[dset_idx] >= 0), "File dataspace retrieval succeeded");

        VRFY((H5Sselect_none(fspace_ids[dset_idx]) >= 0), "H5Sselect_none succeeded");
    }
}

static void
select_elements(size_t num_dsets, hid_t *dset_ids, size_t num_points, hsize_t *coords, hid_t *fspace_ids)
{
    VRFY((fspace_ids != NULL), "verify fspace_ids");

    for (size_t dset_idx = 0; dset_idx < MAX_NUM_DSETS_MULTI; dset_idx++)
        fspace_ids[dset_idx] = H5I_INVALID_HID;

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        fspace_ids[dset_idx] = H5Dget_space(dset_ids[dset_idx]);
        VRFY((fspace_ids[dset_idx] >= 0), "File dataspace retrieval succeeded");

        VRFY((H5Sselect_elements(fspace_ids[dset_idx], H5S_SELECT_SET, num_points, coords) >= 0),
             "Point selection succeeded");
    }
}

#ifdef H5_HAVE_PARALLEL_FILTERED_WRITES
/*
 * Tests parallel write of filtered data in the special
 * case where a dataset is composed of a single chunk.
 *
 */
static void
test_write_one_chunk_filtered_dataset(const char *parent_group, H5Z_filter_t filter_id, hid_t fapl_id,
                                      hid_t dcpl_id, hid_t dxpl_id, test_mode_t test_mode)
{
    C_DATATYPE *correct_bufs[MAX_NUM_DSETS_MULTI] = {0};
    const void *data_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    void       *data_bufs_nc[MAX_NUM_DSETS_MULTI] = {0}; /* non-const buffer pointers for freeing */
    void       *read_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    hsize_t     dataset_dims[WRITE_ONE_CHUNK_FILTERED_DATASET_DIMS];
    hsize_t     chunk_dims[WRITE_ONE_CHUNK_FILTERED_DATASET_DIMS];
    hsize_t     sel_dims[WRITE_ONE_CHUNK_FILTERED_DATASET_DIMS];
    hsize_t     start[WRITE_ONE_CHUNK_FILTERED_DATASET_DIMS];
    hsize_t     stride[WRITE_ONE_CHUNK_FILTERED_DATASET_DIMS];
    hsize_t     count[WRITE_ONE_CHUNK_FILTERED_DATASET_DIMS];
    hsize_t     block[WRITE_ONE_CHUNK_FILTERED_DATASET_DIMS];
    size_t      data_size, correct_buf_size;
    size_t      num_dsets;
    hid_t       dset_ids[MAX_NUM_DSETS_MULTI];
    hid_t       fspace_ids[MAX_NUM_DSETS_MULTI];
    hid_t       file_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID;
    hid_t       group_id  = H5I_INVALID_HID;
    hid_t       filespace = H5I_INVALID_HID;

    if (MAINPROCESS)
        puts("Testing write to one-chunk filtered dataset");

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

    /* Create chunked dataset */
    plist_id = H5Pcopy(dcpl_id);
    VRFY((plist_id >= 0), "DCPL copy succeeded");

    VRFY((H5Pset_chunk(plist_id, WRITE_ONE_CHUNK_FILTERED_DATASET_DIMS, chunk_dims) >= 0), "Chunk size set");

    /* Add test filter to the pipeline */
    VRFY((set_dcpl_filter(plist_id, filter_id, NULL) >= 0), "Filter set");

    /* Create datasets depending on the current test mode */
    create_datasets(group_id, WRITE_ONE_CHUNK_FILTERED_DATASET_NAME, HDF5_DATATYPE_NAME, filespace, plist_id,
                    test_mode, &num_dsets, dset_ids);

    /* Verify space allocation status */
    verify_space_alloc_status(num_dsets, dset_ids, plist_id, DATASET_JUST_CREATED);

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

    select_hyperslab(num_dsets, dset_ids, start, stride, count, block, fspace_ids);

    /* Fill data buffer */
    data_size = (hsize_t)WRITE_ONE_CHUNK_FILTERED_DATASET_CH_NROWS *
                (hsize_t)WRITE_ONE_CHUNK_FILTERED_DATASET_NCOLS * sizeof(C_DATATYPE);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        C_DATATYPE *tmp_buf = (C_DATATYPE *)calloc(1, data_size);
        VRFY((NULL != tmp_buf), "calloc succeeded");

        for (size_t j = 0; j < data_size / sizeof(C_DATATYPE); j++)
            tmp_buf[j] = (C_DATATYPE)(GEN_DATA(j) + dset_idx);

        data_bufs[dset_idx]    = tmp_buf;
        data_bufs_nc[dset_idx] = tmp_buf;
    }

    write_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_BLOCK, fspace_ids, dcpl_id, dxpl_id,
                   data_bufs, test_mode, true, true, false);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        free(data_bufs_nc[dset_idx]);

    /* Verify space allocation status */
    verify_space_alloc_status(num_dsets, dset_ids, plist_id, ALL_CHUNKS_WRITTEN);

    /* Close and re-open datasets */
    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");

    open_datasets(group_id, WRITE_ONE_CHUNK_FILTERED_DATASET_NAME, num_dsets, test_mode, dset_ids);

    /* Verify the correct data was written */
    correct_buf_size = dataset_dims[0] * dataset_dims[1] * sizeof(C_DATATYPE);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        correct_bufs[dset_idx] = (C_DATATYPE *)calloc(1, correct_buf_size);
        VRFY((NULL != correct_bufs[dset_idx]), "calloc succeeded");
        read_bufs[dset_idx] = calloc(1, correct_buf_size);
        VRFY((NULL != read_bufs[dset_idx]), "calloc succeeded");

        for (size_t j = 0; j < correct_buf_size / sizeof(C_DATATYPE); j++)
            correct_bufs[dset_idx][j] =
                ((C_DATATYPE)j % (WRITE_ONE_CHUNK_FILTERED_DATASET_CH_NROWS / mpi_size *
                                  WRITE_ONE_CHUNK_FILTERED_DATASET_CH_NCOLS)) +
                ((C_DATATYPE)j / (WRITE_ONE_CHUNK_FILTERED_DATASET_CH_NROWS / mpi_size *
                                  WRITE_ONE_CHUNK_FILTERED_DATASET_CH_NCOLS)) +
                (C_DATATYPE)dset_idx;
    }

    read_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, dcpl_id, dxpl_id, read_bufs,
                  test_mode, true, true, false);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        VRFY((0 == memcmp(read_bufs[dset_idx], correct_bufs[dset_idx], correct_buf_size)),
             "Data verification succeeded");

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        free(read_bufs[dset_idx]);
        free(correct_bufs[dset_idx]);
    }

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        VRFY((H5Sclose(fspace_ids[dset_idx]) >= 0), "File dataspace close succeeded");
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");
    }

    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
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
 */
static void
test_write_filtered_dataset_no_overlap(const char *parent_group, H5Z_filter_t filter_id, hid_t fapl_id,
                                       hid_t dcpl_id, hid_t dxpl_id, test_mode_t test_mode)
{
    C_DATATYPE *correct_bufs[MAX_NUM_DSETS_MULTI] = {0};
    const void *data_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    void       *data_bufs_nc[MAX_NUM_DSETS_MULTI] = {0}; /* non-const buffer pointers for freeing */
    void       *read_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    hsize_t     dataset_dims[WRITE_UNSHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     chunk_dims[WRITE_UNSHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     sel_dims[WRITE_UNSHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     start[WRITE_UNSHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     stride[WRITE_UNSHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     count[WRITE_UNSHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     block[WRITE_UNSHARED_FILTERED_CHUNKS_DATASET_DIMS];
    size_t      data_size, correct_buf_size;
    size_t      num_dsets;
    hid_t       dset_ids[MAX_NUM_DSETS_MULTI];
    hid_t       fspace_ids[MAX_NUM_DSETS_MULTI];
    hid_t       file_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID;
    hid_t       group_id  = H5I_INVALID_HID;
    hid_t       filespace = H5I_INVALID_HID;

    if (MAINPROCESS)
        puts("Testing write to unshared filtered chunks");

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

    /* Create chunked dataset */
    plist_id = H5Pcopy(dcpl_id);
    VRFY((plist_id >= 0), "DCPL copy succeeded");

    VRFY((H5Pset_chunk(plist_id, WRITE_UNSHARED_FILTERED_CHUNKS_DATASET_DIMS, chunk_dims) >= 0),
         "Chunk size set");

    /* Add test filter to the pipeline */
    VRFY((set_dcpl_filter(plist_id, filter_id, NULL) >= 0), "Filter set");

    /* Create datasets depending on the current test mode */
    create_datasets(group_id, WRITE_UNSHARED_FILTERED_CHUNKS_DATASET_NAME, HDF5_DATATYPE_NAME, filespace,
                    plist_id, test_mode, &num_dsets, dset_ids);

    /* Verify space allocation status */
    verify_space_alloc_status(num_dsets, dset_ids, plist_id, DATASET_JUST_CREATED);

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

    select_hyperslab(num_dsets, dset_ids, start, stride, count, block, fspace_ids);

    /* Fill data buffer */
    data_size = sel_dims[0] * sel_dims[1] * sizeof(C_DATATYPE);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        C_DATATYPE *tmp_buf = (C_DATATYPE *)calloc(1, data_size);
        VRFY((NULL != tmp_buf), "calloc succeeded");

        for (size_t j = 0; j < data_size / sizeof(C_DATATYPE); j++)
            tmp_buf[j] = (C_DATATYPE)(GEN_DATA(j) + dset_idx);

        data_bufs[dset_idx]    = tmp_buf;
        data_bufs_nc[dset_idx] = tmp_buf;
    }

    write_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_BLOCK, fspace_ids, dcpl_id, dxpl_id,
                   data_bufs, test_mode, true, true, false);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        free(data_bufs_nc[dset_idx]);

    /* Verify space allocation status */
    verify_space_alloc_status(num_dsets, dset_ids, plist_id, ALL_CHUNKS_WRITTEN);

    /* Close and re-open datasets */
    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");

    open_datasets(group_id, WRITE_UNSHARED_FILTERED_CHUNKS_DATASET_NAME, num_dsets, test_mode, dset_ids);

    /* Verify the correct data was written */
    correct_buf_size = dataset_dims[0] * dataset_dims[1] * sizeof(C_DATATYPE);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        correct_bufs[dset_idx] = (C_DATATYPE *)calloc(1, correct_buf_size);
        VRFY((NULL != correct_bufs[dset_idx]), "calloc succeeded");
        read_bufs[dset_idx] = calloc(1, correct_buf_size);
        VRFY((NULL != read_bufs[dset_idx]), "calloc succeeded");

        for (size_t j = 0; j < correct_buf_size / sizeof(C_DATATYPE); j++)
            correct_bufs[dset_idx][j] =
                (C_DATATYPE)((j % (dataset_dims[0] / (hsize_t)mpi_size * dataset_dims[1])) +
                             (j / (dataset_dims[0] / (hsize_t)mpi_size * dataset_dims[1])) + dset_idx);
    }

    read_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, dcpl_id, dxpl_id, read_bufs,
                  test_mode, true, true, false);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        VRFY((0 == memcmp(read_bufs[dset_idx], correct_bufs[dset_idx], correct_buf_size)),
             "Data verification succeeded");

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        free(read_bufs[dset_idx]);
        free(correct_bufs[dset_idx]);
    }

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        VRFY((H5Sclose(fspace_ids[dset_idx]) >= 0), "File dataspace close succeeded");
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");
    }

    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
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
                                               hid_t fapl_id, hid_t dcpl_id, hid_t dxpl_id,
                                               test_mode_t test_mode)
{
    C_DATATYPE *correct_bufs[MAX_NUM_DSETS_MULTI] = {0};
    const void *data_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    void       *data_bufs_nc[MAX_NUM_DSETS_MULTI] = {0}; /* non-const buffer pointers for freeing */
    void       *read_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    hsize_t     dataset_dims[WRITE_UNSHARED_FILTERED_CHUNKS_PARTIAL_DATASET_DIMS];
    hsize_t     chunk_dims[WRITE_UNSHARED_FILTERED_CHUNKS_PARTIAL_DATASET_DIMS];
    hsize_t     sel_dims[WRITE_UNSHARED_FILTERED_CHUNKS_PARTIAL_DATASET_DIMS];
    hsize_t     start[WRITE_UNSHARED_FILTERED_CHUNKS_PARTIAL_DATASET_DIMS];
    hsize_t     stride[WRITE_UNSHARED_FILTERED_CHUNKS_PARTIAL_DATASET_DIMS];
    hsize_t     count[WRITE_UNSHARED_FILTERED_CHUNKS_PARTIAL_DATASET_DIMS];
    hsize_t     block[WRITE_UNSHARED_FILTERED_CHUNKS_PARTIAL_DATASET_DIMS];
    size_t      data_size, correct_buf_size;
    size_t      num_dsets;
    hid_t       dset_ids[MAX_NUM_DSETS_MULTI];
    hid_t       fspace_ids[MAX_NUM_DSETS_MULTI];
    hid_t       file_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID;
    hid_t       group_id  = H5I_INVALID_HID;
    hid_t       filespace = H5I_INVALID_HID;

    if (MAINPROCESS)
        puts("Testing partial write to unshared filtered chunks");

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

    /*
     * Since we're only doing a partial write to the dataset, make
     * sure the fill time is set appropriately
     */
    VRFY((H5Pset_fill_time(plist_id, H5D_FILL_TIME_IFSET) >= 0), "H5Pset_fill_time succeeded");

    VRFY((H5Pset_chunk(plist_id, WRITE_UNSHARED_FILTERED_CHUNKS_PARTIAL_DATASET_DIMS, chunk_dims) >= 0),
         "Chunk size set");

    /* Add test filter to the pipeline */
    VRFY((set_dcpl_filter(plist_id, filter_id, NULL) >= 0), "Filter set");

    /* Create datasets depending on the current test mode */
    create_datasets(group_id, WRITE_UNSHARED_FILTERED_CHUNKS_PARTIAL_DATASET_NAME, HDF5_DATATYPE_NAME,
                    filespace, plist_id, test_mode, &num_dsets, dset_ids);

    /* Verify space allocation status */
    verify_space_alloc_status(num_dsets, dset_ids, plist_id, DATASET_JUST_CREATED);

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

    select_hyperslab(num_dsets, dset_ids, start, stride, count, block, fspace_ids);

    /* Fill data buffer */
    data_size = sel_dims[0] * sel_dims[1] * sizeof(C_DATATYPE);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        C_DATATYPE *tmp_buf = (C_DATATYPE *)calloc(1, data_size);
        VRFY((NULL != tmp_buf), "calloc succeeded");

        for (size_t j = 0; j < data_size / sizeof(C_DATATYPE); j++)
            tmp_buf[j] = (C_DATATYPE)(GEN_DATA(j) + dset_idx);

        data_bufs[dset_idx]    = tmp_buf;
        data_bufs_nc[dset_idx] = tmp_buf;
    }

    write_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_BLOCK, fspace_ids, dcpl_id, dxpl_id,
                   data_bufs, test_mode, true, true, false);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        free(data_bufs_nc[dset_idx]);

    verify_space_alloc_status(num_dsets, dset_ids, plist_id, ALL_CHUNKS_WRITTEN);

    /* Close and re-open datasets */
    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");

    open_datasets(group_id, WRITE_UNSHARED_FILTERED_CHUNKS_PARTIAL_DATASET_NAME, num_dsets, test_mode,
                  dset_ids);

    /* Verify the correct data was written */
    correct_buf_size = dataset_dims[0] * dataset_dims[1] * sizeof(C_DATATYPE);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        correct_bufs[dset_idx] = (C_DATATYPE *)calloc(1, correct_buf_size);
        VRFY((NULL != correct_bufs[dset_idx]), "calloc succeeded");
        read_bufs[dset_idx] = calloc(1, correct_buf_size);
        VRFY((NULL != read_bufs[dset_idx]), "calloc succeeded");

        for (size_t j = 0; j < (size_t)mpi_size; j++) {
            size_t data_idx     = j;
            size_t rank_n_elems = (size_t)(mpi_size * (WRITE_UNSHARED_FILTERED_CHUNKS_PARTIAL_CH_NROWS *
                                                       WRITE_UNSHARED_FILTERED_CHUNKS_PARTIAL_CH_NCOLS));

            for (size_t k = 0; k < rank_n_elems; k++) {
                if ((k % WRITE_UNSHARED_FILTERED_CHUNKS_PARTIAL_CH_NCOLS) == 0) {
                    correct_bufs[dset_idx][(j * rank_n_elems) + k] = (C_DATATYPE)(data_idx + dset_idx);
                    data_idx++;
                }
            }
        }
    }

    read_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, dcpl_id, dxpl_id, read_bufs,
                  test_mode, true, true, false);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        VRFY((0 == memcmp(read_bufs[dset_idx], correct_bufs[dset_idx], correct_buf_size)),
             "Data verification succeeded");

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        free(read_bufs[dset_idx]);
        free(correct_bufs[dset_idx]);
    }

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        VRFY((H5Sclose(fspace_ids[dset_idx]) >= 0), "File dataspace close succeeded");
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");
    }

    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
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
 */
static void
test_write_filtered_dataset_overlap(const char *parent_group, H5Z_filter_t filter_id, hid_t fapl_id,
                                    hid_t dcpl_id, hid_t dxpl_id, test_mode_t test_mode)
{
    C_DATATYPE *correct_bufs[MAX_NUM_DSETS_MULTI] = {0};
    const void *data_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    void       *data_bufs_nc[MAX_NUM_DSETS_MULTI] = {0}; /* non-const buffer pointers for freeing */
    void       *read_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    hsize_t     dataset_dims[WRITE_SHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     chunk_dims[WRITE_SHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     sel_dims[WRITE_SHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     start[WRITE_SHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     stride[WRITE_SHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     count[WRITE_SHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     block[WRITE_SHARED_FILTERED_CHUNKS_DATASET_DIMS];
    size_t      data_size, correct_buf_size;
    size_t      num_dsets;
    hid_t       dset_ids[MAX_NUM_DSETS_MULTI];
    hid_t       fspace_ids[MAX_NUM_DSETS_MULTI];
    hid_t       file_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID;
    hid_t       group_id  = H5I_INVALID_HID;
    hid_t       filespace = H5I_INVALID_HID;

    if (MAINPROCESS)
        puts("Testing write to shared filtered chunks");

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

    /* Create chunked dataset */
    plist_id = H5Pcopy(dcpl_id);
    VRFY((plist_id >= 0), "DCPL copy succeeded");

    VRFY((H5Pset_chunk(plist_id, WRITE_SHARED_FILTERED_CHUNKS_DATASET_DIMS, chunk_dims) >= 0),
         "Chunk size set");

    /* Add test filter to the pipeline */
    VRFY((set_dcpl_filter(plist_id, filter_id, NULL) >= 0), "Filter set");

    /* Create datasets depending on the current test mode */
    create_datasets(group_id, WRITE_SHARED_FILTERED_CHUNKS_DATASET_NAME, HDF5_DATATYPE_NAME, filespace,
                    plist_id, test_mode, &num_dsets, dset_ids);

    /* Verify space allocation status */
    verify_space_alloc_status(num_dsets, dset_ids, plist_id, DATASET_JUST_CREATED);

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

    select_hyperslab(num_dsets, dset_ids, start, stride, count, block, fspace_ids);

    /* Fill data buffer */
    data_size = sel_dims[0] * sel_dims[1] * sizeof(C_DATATYPE);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        C_DATATYPE *tmp_buf = (C_DATATYPE *)calloc(1, data_size);
        VRFY((NULL != tmp_buf), "calloc succeeded");

        for (size_t j = 0; j < data_size / sizeof(C_DATATYPE); j++)
            tmp_buf[j] = (C_DATATYPE)(GEN_DATA(j) + dset_idx);

        data_bufs[dset_idx]    = tmp_buf;
        data_bufs_nc[dset_idx] = tmp_buf;
    }

    write_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_BLOCK, fspace_ids, dcpl_id, dxpl_id,
                   data_bufs, test_mode, true, true, false);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        free(data_bufs_nc[dset_idx]);

    verify_space_alloc_status(num_dsets, dset_ids, plist_id, ALL_CHUNKS_WRITTEN);

    /* Close and re-open datasets */
    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");

    open_datasets(group_id, WRITE_SHARED_FILTERED_CHUNKS_DATASET_NAME, num_dsets, test_mode, dset_ids);

    /* Verify the correct data was written */
    correct_buf_size = dataset_dims[0] * dataset_dims[1] * sizeof(C_DATATYPE);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        correct_bufs[dset_idx] = (C_DATATYPE *)calloc(1, correct_buf_size);
        VRFY((NULL != correct_bufs[dset_idx]), "calloc succeeded");
        read_bufs[dset_idx] = calloc(1, correct_buf_size);
        VRFY((NULL != read_bufs[dset_idx]), "calloc succeeded");

        for (size_t j = 0; j < correct_buf_size / sizeof(C_DATATYPE); j++)
            correct_bufs[dset_idx][j] =
                (C_DATATYPE)((dataset_dims[1] * (j / ((hsize_t)mpi_size * dataset_dims[1]))) +
                             (j % dataset_dims[1]) +
                             (((j % ((hsize_t)mpi_size * dataset_dims[1])) / dataset_dims[1]) %
                              dataset_dims[1]) +
                             dset_idx);
    }

    read_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, dcpl_id, dxpl_id, read_bufs,
                  test_mode, true, true, false);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        VRFY((0 == memcmp(read_bufs[dset_idx], correct_bufs[dset_idx], correct_buf_size)),
             "Data verification succeeded");

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        free(read_bufs[dset_idx]);
        free(correct_bufs[dset_idx]);
    }

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        VRFY((H5Sclose(fspace_ids[dset_idx]) >= 0), "File dataspace close succeeded");
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");
    }

    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
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
                                                        hid_t fapl_id, hid_t dcpl_id, hid_t dxpl_id,
                                                        test_mode_t test_mode)
{
    const void *data_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    void       *data_bufs_nc[MAX_NUM_DSETS_MULTI] = {0}; /* non-const buffer pointers for freeing */
    void       *read_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    hsize_t     dataset_dims[WRITE_UNSHARED_ONE_UNLIM_DIM_DATASET_DIMS];
    hsize_t     max_dims[WRITE_UNSHARED_ONE_UNLIM_DIM_DATASET_DIMS];
    hsize_t     chunk_dims[WRITE_UNSHARED_ONE_UNLIM_DIM_DATASET_DIMS];
    hsize_t     sel_dims[WRITE_UNSHARED_ONE_UNLIM_DIM_DATASET_DIMS];
    hsize_t     start[WRITE_UNSHARED_ONE_UNLIM_DIM_DATASET_DIMS];
    hsize_t     stride[WRITE_UNSHARED_ONE_UNLIM_DIM_DATASET_DIMS];
    hsize_t     count[WRITE_UNSHARED_ONE_UNLIM_DIM_DATASET_DIMS];
    hsize_t     block[WRITE_UNSHARED_ONE_UNLIM_DIM_DATASET_DIMS];
    size_t      data_size;
    size_t      num_dsets;
    size_t      num_loops;
    hid_t       dset_ids[MAX_NUM_DSETS_MULTI];
    hid_t       fspace_ids[MAX_NUM_DSETS_MULTI];
    hid_t       file_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID;
    hid_t       group_id  = H5I_INVALID_HID;
    hid_t       filespace = H5I_INVALID_HID;

    if (MAINPROCESS)
        puts("Testing write to unshared filtered chunks w/ single unlimited dimension");

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

    /* Create datasets depending on the current test mode */
    create_datasets(group_id, WRITE_UNSHARED_ONE_UNLIM_DIM_DATASET_NAME, HDF5_DATATYPE_NAME, filespace,
                    plist_id, test_mode, &num_dsets, dset_ids);

    /* Verify space allocation status */
    verify_space_alloc_status(num_dsets, dset_ids, plist_id, DATASET_JUST_CREATED);

    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

    /* Fill data buffer */
    data_size = sel_dims[0] * sel_dims[1] * sizeof(C_DATATYPE);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        C_DATATYPE *tmp_buf = (C_DATATYPE *)calloc(1, data_size);
        VRFY((NULL != tmp_buf), "calloc succeeded");

        for (size_t j = 0; j < data_size / sizeof(C_DATATYPE); j++)
            tmp_buf[j] = (C_DATATYPE)(GEN_DATA(j) + dset_idx);

        data_bufs[dset_idx]    = tmp_buf;
        data_bufs_nc[dset_idx] = tmp_buf;

        tmp_buf = (C_DATATYPE *)calloc(1, data_size);
        VRFY((NULL != tmp_buf), "calloc succeeded");

        read_bufs[dset_idx] = tmp_buf;
    }

    /* Determine number of loops to run through */
    num_loops = WRITE_UNSHARED_ONE_UNLIM_DIM_NLOOPS;
    if ((test_mode == USE_MULTIPLE_DATASETS) || (test_mode == USE_MULTIPLE_DATASETS_MIXED_FILTERED))
        num_loops /= 2;

    for (size_t i = 0; i < num_loops; i++) {
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

        select_hyperslab(num_dsets, dset_ids, start, stride, count, block, fspace_ids);

        write_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_BLOCK, fspace_ids, dcpl_id, dxpl_id,
                       data_bufs, test_mode, true, true, i > 0);

        /* Verify space allocation status */
        verify_space_alloc_status(num_dsets, dset_ids, plist_id, ALL_CHUNKS_WRITTEN);

        /* Close and re-open datasets */
        for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
            VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");

        open_datasets(group_id, WRITE_UNSHARED_ONE_UNLIM_DIM_DATASET_NAME, num_dsets, test_mode, dset_ids);

        for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
            memset(read_bufs[dset_idx], 255, data_size);

        read_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_BLOCK, fspace_ids[0], dcpl_id, dxpl_id,
                      read_bufs, test_mode, true, true, false);

        /* Verify the correct data was written */
        for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
            VRFY((0 == memcmp(read_bufs[dset_idx], data_bufs[dset_idx], data_size)),
                 "Data verification succeeded");

        if (i < num_loops - 1) {
            /* Extend the dataset(s) by count[1] chunks in the extensible dimension */
            dataset_dims[1] += count[1] * block[1];

            for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
                VRFY(H5Dset_extent(dset_ids[dset_idx], dataset_dims) >= 0, "H5Dset_extent succeeded");

            /* Verify space allocation status */
            verify_space_alloc_status(num_dsets, dset_ids, plist_id, SOME_CHUNKS_WRITTEN);
        }

        for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
            VRFY((H5Sclose(fspace_ids[dset_idx]) >= 0), "File dataspace close succeeded");
    }

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        free(read_bufs[dset_idx]);
        free(data_bufs_nc[dset_idx]);
    }

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");

    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
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
                                                     hid_t fapl_id, hid_t dcpl_id, hid_t dxpl_id,
                                                     test_mode_t test_mode)
{
    const void *data_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    void       *data_bufs_nc[MAX_NUM_DSETS_MULTI] = {0}; /* non-const buffer pointers for freeing */
    void       *read_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    hsize_t     dataset_dims[WRITE_SHARED_ONE_UNLIM_DIM_DATASET_DIMS];
    hsize_t     max_dims[WRITE_SHARED_ONE_UNLIM_DIM_DATASET_DIMS];
    hsize_t     chunk_dims[WRITE_SHARED_ONE_UNLIM_DIM_DATASET_DIMS];
    hsize_t     sel_dims[WRITE_SHARED_ONE_UNLIM_DIM_DATASET_DIMS];
    hsize_t     start[WRITE_SHARED_ONE_UNLIM_DIM_DATASET_DIMS];
    hsize_t     stride[WRITE_SHARED_ONE_UNLIM_DIM_DATASET_DIMS];
    hsize_t     count[WRITE_SHARED_ONE_UNLIM_DIM_DATASET_DIMS];
    hsize_t     block[WRITE_SHARED_ONE_UNLIM_DIM_DATASET_DIMS];
    size_t      data_size;
    size_t      num_dsets;
    size_t      num_loops;
    hid_t       dset_ids[MAX_NUM_DSETS_MULTI];
    hid_t       fspace_ids[MAX_NUM_DSETS_MULTI];
    hid_t       file_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID;
    hid_t       group_id  = H5I_INVALID_HID;
    hid_t       filespace = H5I_INVALID_HID;

    if (MAINPROCESS)
        puts("Testing write to shared filtered chunks w/ single unlimited dimension");

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

    /* Create datasets depending on the current test mode */
    create_datasets(group_id, WRITE_SHARED_ONE_UNLIM_DIM_DATASET_NAME, HDF5_DATATYPE_NAME, filespace,
                    plist_id, test_mode, &num_dsets, dset_ids);

    /* Verify space allocation status */
    verify_space_alloc_status(num_dsets, dset_ids, plist_id, DATASET_JUST_CREATED);

    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

    /* Fill data buffer */
    data_size = sel_dims[0] * sel_dims[1] * sizeof(C_DATATYPE);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        C_DATATYPE *tmp_buf = (C_DATATYPE *)calloc(1, data_size);
        VRFY((NULL != tmp_buf), "calloc succeeded");

        for (size_t j = 0; j < data_size / sizeof(C_DATATYPE); j++)
            tmp_buf[j] = (C_DATATYPE)(GEN_DATA(j) + dset_idx);

        data_bufs[dset_idx]    = tmp_buf;
        data_bufs_nc[dset_idx] = tmp_buf;

        tmp_buf = (C_DATATYPE *)calloc(1, data_size);
        VRFY((NULL != tmp_buf), "calloc succeeded");

        read_bufs[dset_idx] = tmp_buf;
    }

    /* Determine number of loops to run through */
    num_loops = WRITE_SHARED_ONE_UNLIM_DIM_NLOOPS;
    if ((test_mode == USE_MULTIPLE_DATASETS) || (test_mode == USE_MULTIPLE_DATASETS_MIXED_FILTERED))
        num_loops /= 2;

    for (size_t i = 0; i < num_loops; i++) {
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

        select_hyperslab(num_dsets, dset_ids, start, stride, count, block, fspace_ids);

        write_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_BLOCK, fspace_ids, dcpl_id, dxpl_id,
                       data_bufs, test_mode, true, true, i > 0);

        /* Verify space allocation status */
        verify_space_alloc_status(num_dsets, dset_ids, plist_id, ALL_CHUNKS_WRITTEN);

        /* Close and re-open datasets */
        for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
            VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");

        open_datasets(group_id, WRITE_SHARED_ONE_UNLIM_DIM_DATASET_NAME, num_dsets, test_mode, dset_ids);

        for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
            memset(read_bufs[dset_idx], 255, data_size);

        read_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_BLOCK, fspace_ids[0], dcpl_id, dxpl_id,
                      read_bufs, test_mode, true, true, false);

        /* Verify the correct data was written */
        for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
            VRFY((0 == memcmp(read_bufs[dset_idx], data_bufs[dset_idx], data_size)),
                 "Data verification succeeded");

        if (i < num_loops - 1) {
            /* Extend the dataset(s) by count[1] chunks in the extensible dimension */
            dataset_dims[1] += count[1] * block[1];

            for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
                VRFY(H5Dset_extent(dset_ids[dset_idx], dataset_dims) >= 0, "H5Dset_extent succeeded");

            /* Verify space allocation status */
            verify_space_alloc_status(num_dsets, dset_ids, plist_id, SOME_CHUNKS_WRITTEN);
        }

        for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
            VRFY((H5Sclose(fspace_ids[dset_idx]) >= 0), "File dataspace close succeeded");
    }

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        free(read_bufs[dset_idx]);
        free(data_bufs_nc[dset_idx]);
    }

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");

    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
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
                                                       hid_t fapl_id, hid_t dcpl_id, hid_t dxpl_id,
                                                       test_mode_t test_mode)
{
    const void *data_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    void       *data_bufs_nc[MAX_NUM_DSETS_MULTI] = {0}; /* non-const buffer pointers for freeing */
    void       *read_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    hsize_t     dataset_dims[WRITE_UNSHARED_TWO_UNLIM_DIM_DATASET_DIMS];
    hsize_t     max_dims[WRITE_UNSHARED_TWO_UNLIM_DIM_DATASET_DIMS];
    hsize_t     chunk_dims[WRITE_UNSHARED_TWO_UNLIM_DIM_DATASET_DIMS];
    hsize_t     sel_dims[WRITE_UNSHARED_TWO_UNLIM_DIM_DATASET_DIMS];
    hsize_t     start[WRITE_UNSHARED_TWO_UNLIM_DIM_DATASET_DIMS];
    hsize_t     stride[WRITE_UNSHARED_TWO_UNLIM_DIM_DATASET_DIMS];
    hsize_t     count[WRITE_UNSHARED_TWO_UNLIM_DIM_DATASET_DIMS];
    hsize_t     block[WRITE_UNSHARED_TWO_UNLIM_DIM_DATASET_DIMS];
    size_t      data_size;
    size_t      num_dsets;
    size_t      num_loops;
    hid_t       dset_ids[MAX_NUM_DSETS_MULTI];
    hid_t       fspace_ids[MAX_NUM_DSETS_MULTI];
    hid_t       file_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID;
    hid_t       group_id  = H5I_INVALID_HID;
    hid_t       filespace = H5I_INVALID_HID;

    if (MAINPROCESS)
        puts("Testing write to unshared filtered chunks w/ two unlimited dimensions");

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

    /* Create datasets depending on the current test mode */
    create_datasets(group_id, WRITE_UNSHARED_TWO_UNLIM_DIM_DATASET_NAME, HDF5_DATATYPE_NAME, filespace,
                    plist_id, test_mode, &num_dsets, dset_ids);

    /* Verify space allocation status */
    verify_space_alloc_status(num_dsets, dset_ids, plist_id, DATASET_JUST_CREATED);

    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

    /* Determine number of loops to run through */
    num_loops = WRITE_UNSHARED_TWO_UNLIM_DIM_NLOOPS;
    if ((test_mode == USE_MULTIPLE_DATASETS) || (test_mode == USE_MULTIPLE_DATASETS_MIXED_FILTERED))
        num_loops /= 2;

    for (size_t i = 0; i < num_loops; i++) {
        /* Set selected dimensions */
        sel_dims[0] = (i + 1) * WRITE_UNSHARED_TWO_UNLIM_DIM_CH_NROWS;
        sel_dims[1] = (i + 1) * WRITE_UNSHARED_TWO_UNLIM_DIM_CH_NCOLS;

        /* Fill data buffer */
        data_size = sel_dims[0] * sel_dims[1] * sizeof(C_DATATYPE);

        for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
            C_DATATYPE *tmp_buf = realloc(data_bufs_nc[dset_idx], data_size);
            VRFY((NULL != tmp_buf), "realloc succeeded");

            for (size_t k = 0; k < data_size / sizeof(C_DATATYPE); k++)
                tmp_buf[k] = (C_DATATYPE)(GEN_DATA(k) + dset_idx);

            data_bufs[dset_idx]    = tmp_buf;
            data_bufs_nc[dset_idx] = tmp_buf;

            tmp_buf = realloc(read_bufs[dset_idx], data_size);
            VRFY((NULL != tmp_buf), "realloc succeeded");

            read_bufs[dset_idx] = tmp_buf;
        }

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

        select_hyperslab(num_dsets, dset_ids, start, stride, count, block, fspace_ids);

        write_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_BLOCK, fspace_ids, dcpl_id, dxpl_id,
                       data_bufs, test_mode, true, true, i > 0);

        /* Verify space allocation status */
        verify_space_alloc_status(num_dsets, dset_ids, plist_id, ALL_CHUNKS_WRITTEN);

        /* Close and re-open datasets */
        for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
            VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");

        open_datasets(group_id, WRITE_UNSHARED_TWO_UNLIM_DIM_DATASET_NAME, num_dsets, test_mode, dset_ids);

        for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
            memset(read_bufs[dset_idx], 255, data_size);

        read_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_BLOCK, fspace_ids[0], dcpl_id, dxpl_id,
                      read_bufs, test_mode, true, true, false);

        /* Verify the correct data was written */
        for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
            VRFY((0 == memcmp(read_bufs[dset_idx], data_bufs[dset_idx], data_size)),
                 "Data verification succeeded");

        if (i < num_loops - 1) {
            /*
             * Extend the dataset(s) by the size of one chunk per rank
             * in the first extensible dimension. Extend the dataset(s)
             * by the size of chunk in the second extensible dimension.
             */
            dataset_dims[0] += (hsize_t)mpi_size * block[0];
            dataset_dims[1] += block[1];

            for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
                VRFY(H5Dset_extent(dset_ids[dset_idx], dataset_dims) >= 0, "H5Dset_extent succeeded");

            /* Verify space allocation status */
            verify_space_alloc_status(num_dsets, dset_ids, plist_id, SOME_CHUNKS_WRITTEN);
        }

        for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
            VRFY((H5Sclose(fspace_ids[dset_idx]) >= 0), "File dataspace close succeeded");
    }

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        free(read_bufs[dset_idx]);
        free(data_bufs_nc[dset_idx]);
    }

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");

    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
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
                                                    hid_t fapl_id, hid_t dcpl_id, hid_t dxpl_id,
                                                    test_mode_t test_mode)
{
    const void *data_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    void       *data_bufs_nc[MAX_NUM_DSETS_MULTI] = {0}; /* non-const buffer pointers for freeing */
    void       *read_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    hsize_t     dataset_dims[WRITE_SHARED_TWO_UNLIM_DIM_DATASET_DIMS];
    hsize_t     max_dims[WRITE_SHARED_TWO_UNLIM_DIM_DATASET_DIMS];
    hsize_t     chunk_dims[WRITE_SHARED_TWO_UNLIM_DIM_DATASET_DIMS];
    hsize_t     sel_dims[WRITE_SHARED_TWO_UNLIM_DIM_DATASET_DIMS];
    hsize_t     start[WRITE_SHARED_TWO_UNLIM_DIM_DATASET_DIMS];
    hsize_t     stride[WRITE_SHARED_TWO_UNLIM_DIM_DATASET_DIMS];
    hsize_t     count[WRITE_SHARED_TWO_UNLIM_DIM_DATASET_DIMS];
    hsize_t     block[WRITE_SHARED_TWO_UNLIM_DIM_DATASET_DIMS];
    size_t      data_size;
    size_t      num_dsets;
    size_t      num_loops;
    hid_t       dset_ids[MAX_NUM_DSETS_MULTI];
    hid_t       fspace_ids[MAX_NUM_DSETS_MULTI];
    hid_t       file_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID;
    hid_t       group_id  = H5I_INVALID_HID;
    hid_t       filespace = H5I_INVALID_HID;

    if (MAINPROCESS)
        puts("Testing write to shared filtered chunks w/ two unlimited dimensions");

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

    /* Create datasets depending on the current test mode */
    create_datasets(group_id, WRITE_SHARED_TWO_UNLIM_DIM_DATASET_NAME, HDF5_DATATYPE_NAME, filespace,
                    plist_id, test_mode, &num_dsets, dset_ids);

    /* Verify space allocation status */
    verify_space_alloc_status(num_dsets, dset_ids, plist_id, DATASET_JUST_CREATED);

    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

    /* Determine number of loops to run through */
    num_loops = WRITE_SHARED_TWO_UNLIM_DIM_NLOOPS;
    if ((test_mode == USE_MULTIPLE_DATASETS) || (test_mode == USE_MULTIPLE_DATASETS_MIXED_FILTERED))
        num_loops /= 2;

    for (size_t i = 0; i < num_loops; i++) {
        /* Set selected dimensions */
        sel_dims[0] = (i + 1);
        sel_dims[1] = (i + 1) * (size_t)WRITE_SHARED_TWO_UNLIM_DIM_CH_NCOLS;

        /* Fill data buffer */
        data_size = sel_dims[0] * sel_dims[1] * sizeof(C_DATATYPE);

        for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
            C_DATATYPE *tmp_buf = realloc(data_bufs_nc[dset_idx], data_size);
            VRFY((NULL != tmp_buf), "realloc succeeded");

            for (size_t k = 0; k < data_size / sizeof(C_DATATYPE); k++)
                tmp_buf[k] = (C_DATATYPE)(GEN_DATA(k) + dset_idx);

            data_bufs[dset_idx]    = tmp_buf;
            data_bufs_nc[dset_idx] = tmp_buf;

            tmp_buf = realloc(read_bufs[dset_idx], data_size);
            VRFY((NULL != tmp_buf), "realloc succeeded");

            read_bufs[dset_idx] = tmp_buf;
        }

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

        select_hyperslab(num_dsets, dset_ids, start, stride, count, block, fspace_ids);

        write_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_BLOCK, fspace_ids, dcpl_id, dxpl_id,
                       data_bufs, test_mode, true, true, i > 0);

        /* Verify space allocation status */
        verify_space_alloc_status(num_dsets, dset_ids, plist_id, ALL_CHUNKS_WRITTEN);

        /* Close and re-open datasets */
        for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
            VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");

        open_datasets(group_id, WRITE_SHARED_TWO_UNLIM_DIM_DATASET_NAME, num_dsets, test_mode, dset_ids);

        for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
            memset(read_bufs[dset_idx], 255, data_size);

        read_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_BLOCK, fspace_ids[0], dcpl_id, dxpl_id,
                      read_bufs, test_mode, true, true, false);

        /* Verify the correct data was written */
        for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
            VRFY((0 == memcmp(read_bufs[dset_idx], data_bufs[dset_idx], data_size)),
                 "Data verification succeeded");

        if (i < num_loops - 1) {
            /* Extend the dataset(s) by the size of a chunk in each extensible dimension */
            dataset_dims[0] += (hsize_t)WRITE_SHARED_TWO_UNLIM_DIM_CH_NROWS;
            dataset_dims[1] += (hsize_t)WRITE_SHARED_TWO_UNLIM_DIM_CH_NCOLS;

            for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
                VRFY(H5Dset_extent(dset_ids[dset_idx], dataset_dims) >= 0, "H5Dset_extent succeeded");

            /* Verify space allocation status */
            verify_space_alloc_status(num_dsets, dset_ids, plist_id, SOME_CHUNKS_WRITTEN);
        }

        for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
            VRFY((H5Sclose(fspace_ids[dset_idx]) >= 0), "File dataspace close succeeded");
    }

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        free(read_bufs[dset_idx]);
        free(data_bufs_nc[dset_idx]);
    }

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");

    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
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
 */
static void
test_write_filtered_dataset_single_no_selection(const char *parent_group, H5Z_filter_t filter_id,
                                                hid_t fapl_id, hid_t dcpl_id, hid_t dxpl_id,
                                                test_mode_t test_mode)
{
    H5D_alloc_time_t alloc_time;
    C_DATATYPE      *correct_bufs[MAX_NUM_DSETS_MULTI] = {0};
    const void      *data_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    void            *data_bufs_nc[MAX_NUM_DSETS_MULTI] = {0}; /* non-const buffer pointers for freeing */
    void            *read_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    hsize_t          dataset_dims[WRITE_SINGLE_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t          chunk_dims[WRITE_SINGLE_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t          sel_dims[WRITE_SINGLE_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t          start[WRITE_SINGLE_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t          stride[WRITE_SINGLE_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t          count[WRITE_SINGLE_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t          block[WRITE_SINGLE_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    size_t           data_size, correct_buf_size;
    size_t           num_dsets;
    hid_t            dset_ids[MAX_NUM_DSETS_MULTI];
    hid_t            fspace_ids[MAX_NUM_DSETS_MULTI];
    hid_t            file_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID;
    hid_t            group_id  = H5I_INVALID_HID;
    hid_t            filespace = H5I_INVALID_HID;

    if (MAINPROCESS)
        puts("Testing write to filtered chunks with a single process having no selection");

    VRFY((H5Pget_alloc_time(dcpl_id, &alloc_time) >= 0), "H5Pget_alloc_time succeeded");

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

    /* Create chunked dataset */
    plist_id = H5Pcopy(dcpl_id);
    VRFY((plist_id >= 0), "DCPL copy succeeded");

    /*
     * Since we're only doing a partial write to the dataset, make
     * sure the fill time is set appropriately
     */
    VRFY((H5Pset_fill_time(plist_id, H5D_FILL_TIME_IFSET) >= 0), "H5Pset_fill_time succeeded");

    VRFY((H5Pset_chunk(plist_id, WRITE_SINGLE_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS, chunk_dims) >= 0),
         "Chunk size set");

    /* Add test filter to the pipeline */
    VRFY((set_dcpl_filter(plist_id, filter_id, NULL) >= 0), "Filter set");

    /* Create datasets depending on the current test mode */
    create_datasets(group_id, WRITE_SINGLE_NO_SELECTION_FILTERED_CHUNKS_DATASET_NAME, HDF5_DATATYPE_NAME,
                    filespace, plist_id, test_mode, &num_dsets, dset_ids);

    /* Verify space allocation status */
    verify_space_alloc_status(num_dsets, dset_ids, plist_id, DATASET_JUST_CREATED);

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

    if (mpi_rank == WRITE_SINGLE_NO_SELECTION_FILTERED_CHUNKS_NO_SELECT_PROC)
        select_none(num_dsets, dset_ids, fspace_ids);
    else
        select_hyperslab(num_dsets, dset_ids, start, stride, count, block, fspace_ids);

    /* Fill data buffer */
    data_size = sel_dims[0] * sel_dims[1] * sizeof(C_DATATYPE);

    if (mpi_rank != WRITE_SINGLE_NO_SELECTION_FILTERED_CHUNKS_NO_SELECT_PROC) {
        for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
            C_DATATYPE *tmp_buf = (C_DATATYPE *)calloc(1, data_size);
            VRFY((NULL != tmp_buf), "calloc succeeded");

            for (size_t j = 0; j < data_size / sizeof(C_DATATYPE); j++)
                tmp_buf[j] = (C_DATATYPE)(GEN_DATA(j) + dset_idx);

            data_bufs[dset_idx]    = tmp_buf;
            data_bufs_nc[dset_idx] = tmp_buf;
        }
    }

    write_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_BLOCK, fspace_ids, dcpl_id, dxpl_id,
                   data_bufs, test_mode, mpi_size > 1, true, false);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        free(data_bufs_nc[dset_idx]);

    /* Verify space allocation status - data should only have been written if MPI size > 1 */
    verify_space_alloc_status(num_dsets, dset_ids, plist_id,
                              (mpi_size > 1 ? SOME_CHUNKS_WRITTEN : NO_CHUNKS_WRITTEN));

    /* Close and re-open datasets */
    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");

    open_datasets(group_id, WRITE_SINGLE_NO_SELECTION_FILTERED_CHUNKS_DATASET_NAME, num_dsets, test_mode,
                  dset_ids);

    /* Verify the correct data was written */
    correct_buf_size = dataset_dims[0] * dataset_dims[1] * sizeof(C_DATATYPE);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        correct_bufs[dset_idx] = (C_DATATYPE *)calloc(1, correct_buf_size);
        VRFY((NULL != correct_bufs[dset_idx]), "calloc succeeded");
        read_bufs[dset_idx] = calloc(1, correct_buf_size);
        VRFY((NULL != read_bufs[dset_idx]), "calloc succeeded");

        for (size_t j = 0; j < correct_buf_size / sizeof(C_DATATYPE); j++) {
            size_t segment_length;

            correct_bufs[dset_idx][j] =
                (C_DATATYPE)((j % (dataset_dims[0] / (hsize_t)mpi_size * dataset_dims[1])) +
                             (j / (dataset_dims[0] / (hsize_t)mpi_size * dataset_dims[1])) + dset_idx);

            /* Compute the correct offset into the buffer for the process having no selection and clear it */
            segment_length = dataset_dims[0] * dataset_dims[1] / (hsize_t)mpi_size;
            memset(correct_bufs[dset_idx] +
                       ((size_t)WRITE_SINGLE_NO_SELECTION_FILTERED_CHUNKS_NO_SELECT_PROC * segment_length),
                   0, segment_length * sizeof(C_DATATYPE));
        }
    }

    read_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, dcpl_id, dxpl_id, read_bufs,
                  test_mode, true, true, mpi_size == 1 && alloc_time == H5D_ALLOC_TIME_INCR);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        VRFY((0 == memcmp(read_bufs[dset_idx], correct_bufs[dset_idx], correct_buf_size)),
             "Data verification succeeded");

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        free(read_bufs[dset_idx]);
        free(correct_bufs[dset_idx]);
    }

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        VRFY((H5Sclose(fspace_ids[dset_idx]) >= 0), "File dataspace close succeeded");
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");
    }

    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
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
 */
static void
test_write_filtered_dataset_all_no_selection(const char *parent_group, H5Z_filter_t filter_id, hid_t fapl_id,
                                             hid_t dcpl_id, hid_t dxpl_id, test_mode_t test_mode)
{
    H5D_alloc_time_t alloc_time;
    C_DATATYPE      *correct_bufs[MAX_NUM_DSETS_MULTI] = {0};
    const void      *data_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    void            *data_bufs_nc[MAX_NUM_DSETS_MULTI] = {0}; /* non-const buffer pointers for freeing */
    void            *read_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    hsize_t          dataset_dims[WRITE_ALL_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t          chunk_dims[WRITE_ALL_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    size_t           data_size, correct_buf_size;
    size_t           num_dsets;
    hid_t            dset_ids[MAX_NUM_DSETS_MULTI];
    hid_t            fspace_ids[MAX_NUM_DSETS_MULTI];
    hid_t            file_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID;
    hid_t            group_id  = H5I_INVALID_HID;
    hid_t            filespace = H5I_INVALID_HID;

    if (MAINPROCESS)
        puts("Testing write to filtered chunks with all processes having no selection");

    VRFY((H5Pget_alloc_time(dcpl_id, &alloc_time) >= 0), "H5Pget_alloc_time succeeded");

    file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, fapl_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
    VRFY((group_id >= 0), "H5Gopen2 succeeded");

    /* Create the dataspace for the dataset */
    dataset_dims[0] = (hsize_t)WRITE_ALL_NO_SELECTION_FILTERED_CHUNKS_NROWS;
    dataset_dims[1] = (hsize_t)WRITE_ALL_NO_SELECTION_FILTERED_CHUNKS_NCOLS;
    chunk_dims[0]   = (hsize_t)WRITE_ALL_NO_SELECTION_FILTERED_CHUNKS_CH_NROWS;
    chunk_dims[1]   = (hsize_t)WRITE_ALL_NO_SELECTION_FILTERED_CHUNKS_CH_NCOLS;

    filespace = H5Screate_simple(WRITE_ALL_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS, dataset_dims, NULL);
    VRFY((filespace >= 0), "File dataspace creation succeeded");

    /* Create chunked dataset */
    plist_id = H5Pcopy(dcpl_id);
    VRFY((plist_id >= 0), "DCPL copy succeeded");

    /*
     * Since we're doing a no-op write to the dataset,
     * make sure the fill time is set appropriately
     */
    VRFY((H5Pset_fill_time(plist_id, H5D_FILL_TIME_IFSET) >= 0), "H5Pset_fill_time succeeded");

    VRFY((H5Pset_chunk(plist_id, WRITE_ALL_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS, chunk_dims) >= 0),
         "Chunk size set");

    /* Add test filter to the pipeline */
    VRFY((set_dcpl_filter(plist_id, filter_id, NULL) >= 0), "Filter set");

    /* Create datasets depending on the current test mode */
    create_datasets(group_id, WRITE_ALL_NO_SELECTION_FILTERED_CHUNKS_DATASET_NAME, HDF5_DATATYPE_NAME,
                    filespace, plist_id, test_mode, &num_dsets, dset_ids);

    /* Verify space allocation status */
    verify_space_alloc_status(num_dsets, dset_ids, plist_id, DATASET_JUST_CREATED);

    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

    select_none(num_dsets, dset_ids, fspace_ids);

    /* Fill data buffer */
    data_size = sizeof(C_DATATYPE);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        C_DATATYPE *tmp_buf = (C_DATATYPE *)calloc(1, data_size);
        VRFY((NULL != tmp_buf), "calloc succeeded");

        for (size_t j = 0; j < data_size / sizeof(C_DATATYPE); j++)
            tmp_buf[j] = (C_DATATYPE)(GEN_DATA(j) + dset_idx);

        data_bufs[dset_idx]    = tmp_buf;
        data_bufs_nc[dset_idx] = tmp_buf;
    }

    write_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_BLOCK, fspace_ids, dcpl_id, dxpl_id,
                   data_bufs, test_mode, false, true, false);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        free(data_bufs_nc[dset_idx]);

    verify_space_alloc_status(num_dsets, dset_ids, plist_id, NO_CHUNKS_WRITTEN);

    /* Close and re-open datasets */
    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");

    open_datasets(group_id, WRITE_ALL_NO_SELECTION_FILTERED_CHUNKS_DATASET_NAME, num_dsets, test_mode,
                  dset_ids);

    /* Verify the correct data was written */
    correct_buf_size = dataset_dims[0] * dataset_dims[1] * sizeof(C_DATATYPE);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        correct_bufs[dset_idx] = (C_DATATYPE *)calloc(1, correct_buf_size);
        VRFY((NULL != correct_bufs[dset_idx]), "calloc succeeded");
        read_bufs[dset_idx] = calloc(1, correct_buf_size);
        VRFY((NULL != read_bufs[dset_idx]), "calloc succeeded");
    }

    read_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, dcpl_id, dxpl_id, read_bufs,
                  test_mode, true, true, alloc_time == H5D_ALLOC_TIME_INCR);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        VRFY((0 == memcmp(read_bufs[dset_idx], correct_bufs[dset_idx], correct_buf_size)),
             "Data verification succeeded");

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        free(read_bufs[dset_idx]);
        free(correct_bufs[dset_idx]);
    }

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        VRFY((H5Sclose(fspace_ids[dset_idx]) >= 0), "File dataspace close succeeded");
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");
    }

    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
    VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    return;
}

/*
 * Tests parallel write of filtered data by using
 * point selections instead of hyperslab selections.
 *
 */
static void
test_write_filtered_dataset_point_selection(const char *parent_group, H5Z_filter_t filter_id, hid_t fapl_id,
                                            hid_t dcpl_id, hid_t dxpl_id, test_mode_t test_mode)
{
    C_DATATYPE *correct_bufs[MAX_NUM_DSETS_MULTI] = {0};
    const void *data_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    void       *data_bufs_nc[MAX_NUM_DSETS_MULTI] = {0}; /* non-const buffer pointers for freeing */
    void       *read_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    hsize_t    *coords                            = NULL;
    hsize_t     dataset_dims[WRITE_POINT_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     chunk_dims[WRITE_POINT_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     sel_dims[WRITE_POINT_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    size_t      data_size, correct_buf_size;
    size_t      num_points;
    size_t      num_dsets;
    hid_t       dset_ids[MAX_NUM_DSETS_MULTI];
    hid_t       fspace_ids[MAX_NUM_DSETS_MULTI];
    hid_t       file_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID;
    hid_t       group_id  = H5I_INVALID_HID;
    hid_t       filespace = H5I_INVALID_HID;

    if (MAINPROCESS)
        puts("Testing write to filtered chunks with point selection");

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

    /* Create chunked dataset */
    plist_id = H5Pcopy(dcpl_id);
    VRFY((plist_id >= 0), "DCPL copy succeeded");

    VRFY((H5Pset_chunk(plist_id, WRITE_POINT_SELECTION_FILTERED_CHUNKS_DATASET_DIMS, chunk_dims) >= 0),
         "Chunk size set");

    /* Add test filter to the pipeline */
    VRFY((set_dcpl_filter(plist_id, filter_id, NULL) >= 0), "Filter set");

    /* Create datasets depending on the current test mode */
    create_datasets(group_id, WRITE_POINT_SELECTION_FILTERED_CHUNKS_DATASET_NAME, HDF5_DATATYPE_NAME,
                    filespace, plist_id, test_mode, &num_dsets, dset_ids);

    /* Verify space allocation status */
    verify_space_alloc_status(num_dsets, dset_ids, plist_id, DATASET_JUST_CREATED);

    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

    /* Set up point selection */
    num_points = (hsize_t)WRITE_POINT_SELECTION_FILTERED_CHUNKS_NROWS *
                 (hsize_t)WRITE_POINT_SELECTION_FILTERED_CHUNKS_NCOLS / (hsize_t)mpi_size;
    coords = (hsize_t *)calloc(1, 2 * num_points * sizeof(*coords));
    VRFY((NULL != coords), "Coords calloc succeeded");

    for (size_t i = 0; i < num_points; i++)
        for (size_t j = 0; j < WRITE_POINT_SELECTION_FILTERED_CHUNKS_DATASET_DIMS; j++)
            coords[(i * WRITE_POINT_SELECTION_FILTERED_CHUNKS_DATASET_DIMS) + j] =
                (j > 0) ? (i % (hsize_t)WRITE_POINT_SELECTION_FILTERED_CHUNKS_NCOLS)
                        : ((hsize_t)mpi_rank +
                           ((hsize_t)mpi_size * (i / (hsize_t)WRITE_POINT_SELECTION_FILTERED_CHUNKS_NCOLS)));

    select_elements(num_dsets, dset_ids, num_points, coords, fspace_ids);

    /* Fill data buffer */
    data_size = sel_dims[0] * sel_dims[1] * sizeof(C_DATATYPE);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        C_DATATYPE *tmp_buf = (C_DATATYPE *)calloc(1, data_size);
        VRFY((NULL != tmp_buf), "calloc succeeded");

        for (size_t j = 0; j < data_size / sizeof(C_DATATYPE); j++)
            tmp_buf[j] = (C_DATATYPE)(GEN_DATA(j) + dset_idx);

        data_bufs[dset_idx]    = tmp_buf;
        data_bufs_nc[dset_idx] = tmp_buf;
    }

    write_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_BLOCK, fspace_ids, dcpl_id, dxpl_id,
                   data_bufs, test_mode, true, true, false);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        free(data_bufs_nc[dset_idx]);

    verify_space_alloc_status(num_dsets, dset_ids, plist_id, ALL_CHUNKS_WRITTEN);

    /* Close and re-open datasets */
    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");

    open_datasets(group_id, WRITE_POINT_SELECTION_FILTERED_CHUNKS_DATASET_NAME, num_dsets, test_mode,
                  dset_ids);

    /* Verify the correct data was written */
    correct_buf_size = dataset_dims[0] * dataset_dims[1] * sizeof(C_DATATYPE);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        correct_bufs[dset_idx] = (C_DATATYPE *)calloc(1, correct_buf_size);
        VRFY((NULL != correct_bufs[dset_idx]), "calloc succeeded");
        read_bufs[dset_idx] = calloc(1, correct_buf_size);
        VRFY((NULL != read_bufs[dset_idx]), "calloc succeeded");

        for (size_t j = 0; j < correct_buf_size / sizeof(C_DATATYPE); j++)
            correct_bufs[dset_idx][j] =
                (C_DATATYPE)((dataset_dims[1] * (j / ((hsize_t)mpi_size * dataset_dims[1]))) +
                             (j % dataset_dims[1]) +
                             (((j % ((hsize_t)mpi_size * dataset_dims[1])) / dataset_dims[1]) %
                              dataset_dims[1]) +
                             dset_idx);
    }

    read_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, dcpl_id, dxpl_id, read_bufs,
                  test_mode, true, true, false);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        VRFY((0 == memcmp(read_bufs[dset_idx], correct_bufs[dset_idx], correct_buf_size)),
             "Data verification succeeded");

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        free(read_bufs[dset_idx]);
        free(correct_bufs[dset_idx]);
    }

    free(coords);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        VRFY((H5Sclose(fspace_ids[dset_idx]) >= 0), "File dataspace close succeeded");
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");
    }

    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
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
 */
static void
test_write_filtered_dataset_interleaved_write(const char *parent_group, H5Z_filter_t filter_id, hid_t fapl_id,
                                              hid_t dcpl_id, hid_t dxpl_id, test_mode_t test_mode)
{
    C_DATATYPE *correct_bufs[MAX_NUM_DSETS_MULTI] = {0};
    const void *data_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    void       *data_bufs_nc[MAX_NUM_DSETS_MULTI] = {0}; /* non-const buffer pointers for freeing */
    void       *read_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    hsize_t     dataset_dims[INTERLEAVED_WRITE_FILTERED_DATASET_DIMS];
    hsize_t     chunk_dims[INTERLEAVED_WRITE_FILTERED_DATASET_DIMS];
    hsize_t     sel_dims[INTERLEAVED_WRITE_FILTERED_DATASET_DIMS];
    hsize_t     start[INTERLEAVED_WRITE_FILTERED_DATASET_DIMS];
    hsize_t     stride[INTERLEAVED_WRITE_FILTERED_DATASET_DIMS];
    hsize_t     count[INTERLEAVED_WRITE_FILTERED_DATASET_DIMS];
    hsize_t     block[INTERLEAVED_WRITE_FILTERED_DATASET_DIMS];
    size_t      data_size, correct_buf_size;
    size_t      num_dsets;
    hid_t       dset_ids[MAX_NUM_DSETS_MULTI];
    hid_t       fspace_ids[MAX_NUM_DSETS_MULTI];
    hid_t       file_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID;
    hid_t       group_id  = H5I_INVALID_HID;
    hid_t       filespace = H5I_INVALID_HID;

    if (MAINPROCESS)
        puts("Testing interleaved write to filtered chunks");

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

    /* Create chunked dataset */
    plist_id = H5Pcopy(dcpl_id);
    VRFY((plist_id >= 0), "DCPL copy succeeded");

    VRFY((H5Pset_chunk(plist_id, INTERLEAVED_WRITE_FILTERED_DATASET_DIMS, chunk_dims) >= 0),
         "Chunk size set");

    /* Add test filter to the pipeline */
    VRFY((set_dcpl_filter(plist_id, filter_id, NULL) >= 0), "Filter set");

    /* Create datasets depending on the current test mode */
    create_datasets(group_id, INTERLEAVED_WRITE_FILTERED_DATASET_NAME, HDF5_DATATYPE_NAME, filespace,
                    plist_id, test_mode, &num_dsets, dset_ids);

    /* Verify space allocation status */
    verify_space_alloc_status(num_dsets, dset_ids, plist_id, DATASET_JUST_CREATED);

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

    select_hyperslab(num_dsets, dset_ids, start, stride, count, block, fspace_ids);

    /* Fill data buffer */
    data_size = sel_dims[0] * sel_dims[1] * sizeof(C_DATATYPE);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        C_DATATYPE *tmp_buf = (C_DATATYPE *)calloc(1, data_size);
        VRFY((NULL != tmp_buf), "calloc succeeded");

        for (size_t j = 0; j < data_size / sizeof(C_DATATYPE); j++)
            tmp_buf[j] = (C_DATATYPE)(GEN_DATA(j) + dset_idx);

        data_bufs[dset_idx]    = tmp_buf;
        data_bufs_nc[dset_idx] = tmp_buf;
    }

    write_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_BLOCK, fspace_ids, dcpl_id, dxpl_id,
                   data_bufs, test_mode, true, true, false);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        free(data_bufs_nc[dset_idx]);

    /* Verify space allocation status */
    verify_space_alloc_status(num_dsets, dset_ids, plist_id, ALL_CHUNKS_WRITTEN);

    /* Close and re-open datasets */
    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");

    open_datasets(group_id, INTERLEAVED_WRITE_FILTERED_DATASET_NAME, num_dsets, test_mode, dset_ids);

    /* Verify the correct data was written */
    correct_buf_size = dataset_dims[0] * dataset_dims[1] * sizeof(C_DATATYPE);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        correct_bufs[dset_idx] = (C_DATATYPE *)calloc(1, correct_buf_size);
        VRFY((NULL != correct_bufs[dset_idx]), "calloc succeeded");
        read_bufs[dset_idx] = calloc(1, correct_buf_size);
        VRFY((NULL != read_bufs[dset_idx]), "calloc succeeded");

        for (size_t j = 0; j < correct_buf_size / sizeof(C_DATATYPE); j++)
            correct_bufs[dset_idx][j] =
                /* Add the Column Index */
                (C_DATATYPE)((j % (hsize_t)INTERLEAVED_WRITE_FILTERED_DATASET_NCOLS)

                             /* Add the Row Index */
                             + ((j % (hsize_t)(mpi_size * INTERLEAVED_WRITE_FILTERED_DATASET_NCOLS)) /
                                (hsize_t)INTERLEAVED_WRITE_FILTERED_DATASET_NCOLS)

                             /* Add the amount that gets added when a rank moves down to its next section
                                vertically in the dataset */
                             + ((hsize_t)INTERLEAVED_WRITE_FILTERED_DATASET_NCOLS *
                                (j / (hsize_t)(mpi_size * INTERLEAVED_WRITE_FILTERED_DATASET_NCOLS)))

                             /* Add an increment factor for the multi-dataset case */
                             + dset_idx);
    }

    read_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, dcpl_id, dxpl_id, read_bufs,
                  test_mode, true, true, false);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        VRFY((0 == memcmp(read_bufs[dset_idx], correct_bufs[dset_idx], correct_buf_size)),
             "Data verification succeeded");

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        free(read_bufs[dset_idx]);
        free(correct_bufs[dset_idx]);
    }

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        VRFY((H5Sclose(fspace_ids[dset_idx]) >= 0), "File dataspace close succeeded");
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");
    }

    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
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
 */
static void
test_write_transformed_filtered_dataset_no_overlap(const char *parent_group, H5Z_filter_t filter_id,
                                                   hid_t fapl_id, hid_t dcpl_id, hid_t dxpl_id,
                                                   test_mode_t test_mode)
{
    C_DATATYPE *correct_bufs[MAX_NUM_DSETS_MULTI] = {0};
    const void *data_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    void       *data_bufs_nc[MAX_NUM_DSETS_MULTI] = {0}; /* non-const buffer pointers for freeing */
    void       *read_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    hsize_t     dataset_dims[WRITE_UNSHARED_TRANSFORMED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     chunk_dims[WRITE_UNSHARED_TRANSFORMED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     sel_dims[WRITE_UNSHARED_TRANSFORMED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     start[WRITE_UNSHARED_TRANSFORMED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     stride[WRITE_UNSHARED_TRANSFORMED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     count[WRITE_UNSHARED_TRANSFORMED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     block[WRITE_UNSHARED_TRANSFORMED_FILTERED_CHUNKS_DATASET_DIMS];
    size_t      data_size, correct_buf_size;
    size_t      num_dsets;
    hid_t       dset_ids[MAX_NUM_DSETS_MULTI];
    hid_t       fspace_ids[MAX_NUM_DSETS_MULTI];
    hid_t       file_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID;
    hid_t       group_id  = H5I_INVALID_HID;
    hid_t       filespace = H5I_INVALID_HID;

    if (MAINPROCESS)
        puts("Testing write to unshared transformed and filtered chunks");

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

    /* Create chunked dataset */
    plist_id = H5Pcopy(dcpl_id);
    VRFY((plist_id >= 0), "DCPL copy succeeded");

    VRFY((H5Pset_chunk(plist_id, WRITE_UNSHARED_TRANSFORMED_FILTERED_CHUNKS_DATASET_DIMS, chunk_dims) >= 0),
         "Chunk size set");

    /* Add test filter to the pipeline */
    VRFY((set_dcpl_filter(plist_id, filter_id, NULL) >= 0), "Filter set");

    /* Create datasets depending on the current test mode */
    create_datasets(group_id, WRITE_UNSHARED_TRANSFORMED_FILTERED_CHUNKS_DATASET_NAME, HDF5_DATATYPE_NAME,
                    filespace, plist_id, test_mode, &num_dsets, dset_ids);

    /* Verify space allocation status */
    verify_space_alloc_status(num_dsets, dset_ids, plist_id, DATASET_JUST_CREATED);

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

    select_hyperslab(num_dsets, dset_ids, start, stride, count, block, fspace_ids);

    /* Fill data buffer */
    data_size = sel_dims[0] * sel_dims[1] * sizeof(C_DATATYPE);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        C_DATATYPE *tmp_buf = (C_DATATYPE *)calloc(1, data_size);
        VRFY((NULL != tmp_buf), "calloc succeeded");

        for (size_t j = 0; j < data_size / sizeof(C_DATATYPE); j++)
            tmp_buf[j] = (C_DATATYPE)(GEN_DATA(j) + dset_idx);

        data_bufs[dset_idx]    = tmp_buf;
        data_bufs_nc[dset_idx] = tmp_buf;
    }

    /* Create property list for data transform */
    plist_id = H5Pcopy(dxpl_id);
    VRFY((plist_id >= 0), "DXPL copy succeeded");

    /* Set data transform expression */
    VRFY((H5Pset_data_transform(plist_id, "x") >= 0), "Set data transform expression succeeded");

    write_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_BLOCK, fspace_ids, dcpl_id, plist_id,
                   data_bufs, test_mode, true, true, false);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        free(data_bufs_nc[dset_idx]);

    VRFY((H5Pclose(plist_id) >= 0), "DXPL close succeeded");

    /* Verify space allocation status */
    plist_id = H5Dget_create_plist(dset_ids[0]);
    VRFY((plist_id >= 0), "H5Dget_create_plist succeeded");
    verify_space_alloc_status(num_dsets, dset_ids, plist_id, ALL_CHUNKS_WRITTEN);

    /* Close and re-open datasets */
    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");

    open_datasets(group_id, WRITE_UNSHARED_TRANSFORMED_FILTERED_CHUNKS_DATASET_NAME, num_dsets, test_mode,
                  dset_ids);

    /* Verify the correct data was written */
    correct_buf_size = dataset_dims[0] * dataset_dims[1] * sizeof(C_DATATYPE);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        correct_bufs[dset_idx] = (C_DATATYPE *)calloc(1, correct_buf_size);
        VRFY((NULL != correct_bufs[dset_idx]), "calloc succeeded");
        read_bufs[dset_idx] = calloc(1, correct_buf_size);
        VRFY((NULL != read_bufs[dset_idx]), "calloc succeeded");

        for (size_t j = 0; j < correct_buf_size / sizeof(C_DATATYPE); j++)
            correct_bufs[dset_idx][j] =
                (C_DATATYPE)((j % (dataset_dims[0] / (hsize_t)mpi_size * dataset_dims[1])) +
                             (j / (dataset_dims[0] / (hsize_t)mpi_size * dataset_dims[1])) + dset_idx);
    }

    read_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, dcpl_id, dxpl_id, read_bufs,
                  test_mode, true, true, false);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        VRFY((0 == memcmp(read_bufs[dset_idx], correct_bufs[dset_idx], correct_buf_size)),
             "Data verification succeeded");

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        free(read_bufs[dset_idx]);
        free(correct_bufs[dset_idx]);
    }

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        VRFY((H5Sclose(fspace_ids[dset_idx]) >= 0), "File dataspace close succeeded");
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");
    }

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
 */
static void
test_write_3d_filtered_dataset_no_overlap_separate_pages(const char *parent_group, H5Z_filter_t filter_id,
                                                         hid_t fapl_id, hid_t dcpl_id, hid_t dxpl_id,
                                                         test_mode_t test_mode)
{
    C_DATATYPE *correct_bufs[MAX_NUM_DSETS_MULTI] = {0};
    const void *data_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    void       *data_bufs_nc[MAX_NUM_DSETS_MULTI] = {0}; /* non-const buffer pointers for freeing */
    void       *read_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    hsize_t     dataset_dims[WRITE_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_DIMS];
    hsize_t     chunk_dims[WRITE_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_DIMS];
    hsize_t     sel_dims[WRITE_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_DIMS];
    hsize_t     start[WRITE_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_DIMS];
    hsize_t     stride[WRITE_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_DIMS];
    hsize_t     count[WRITE_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_DIMS];
    hsize_t     block[WRITE_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_DIMS];
    size_t      data_size, correct_buf_size;
    size_t      num_dsets;
    hid_t       dset_ids[MAX_NUM_DSETS_MULTI];
    hid_t       fspace_ids[MAX_NUM_DSETS_MULTI];
    hid_t       file_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID;
    hid_t       group_id  = H5I_INVALID_HID;
    hid_t       filespace = H5I_INVALID_HID;

    if (MAINPROCESS)
        puts("Testing write to unshared filtered chunks on separate pages in 3D dataset");

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

    /* Create chunked dataset */
    plist_id = H5Pcopy(dcpl_id);
    VRFY((plist_id >= 0), "DCPL copy succeeded");

    VRFY((H5Pset_chunk(plist_id, WRITE_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_DIMS, chunk_dims) >= 0),
         "Chunk size set");

    /* Add test filter to the pipeline */
    VRFY((set_dcpl_filter(plist_id, filter_id, NULL) >= 0), "Filter set");

    /* Create datasets depending on the current test mode */
    create_datasets(group_id, WRITE_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_NAME, HDF5_DATATYPE_NAME,
                    filespace, plist_id, test_mode, &num_dsets, dset_ids);

    /* Verify space allocation status */
    verify_space_alloc_status(num_dsets, dset_ids, plist_id, DATASET_JUST_CREATED);

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

    select_hyperslab(num_dsets, dset_ids, start, stride, count, block, fspace_ids);

    /* Fill data buffer */
    data_size = sel_dims[0] * sel_dims[1] * sel_dims[2] * sizeof(C_DATATYPE);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        C_DATATYPE *tmp_buf = (C_DATATYPE *)calloc(1, data_size);
        VRFY((NULL != tmp_buf), "calloc succeeded");

        for (size_t j = 0; j < data_size / sizeof(C_DATATYPE); j++)
            tmp_buf[j] = (C_DATATYPE)(GEN_DATA(j) + dset_idx);

        data_bufs[dset_idx]    = tmp_buf;
        data_bufs_nc[dset_idx] = tmp_buf;
    }

    write_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_BLOCK, fspace_ids, dcpl_id, dxpl_id,
                   data_bufs, test_mode, true, true, false);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        free(data_bufs_nc[dset_idx]);

    /* Verify space allocation status */
    verify_space_alloc_status(num_dsets, dset_ids, plist_id, ALL_CHUNKS_WRITTEN);

    /* Close and re-open datasets */
    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");

    open_datasets(group_id, WRITE_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_NAME, num_dsets, test_mode,
                  dset_ids);

    /* Verify the correct data was written */
    correct_buf_size = dataset_dims[0] * dataset_dims[1] * dataset_dims[2] * sizeof(C_DATATYPE);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        correct_bufs[dset_idx] = (C_DATATYPE *)calloc(1, correct_buf_size);
        VRFY((NULL != correct_bufs[dset_idx]), "calloc succeeded");
        read_bufs[dset_idx] = calloc(1, correct_buf_size);
        VRFY((NULL != read_bufs[dset_idx]), "calloc succeeded");

        for (size_t j = 0; j < correct_buf_size / sizeof(C_DATATYPE); j++)
            correct_bufs[dset_idx][j] =
                (C_DATATYPE)((j % (hsize_t)mpi_size) + (j / (hsize_t)mpi_size) + dset_idx);
    }

    read_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, dcpl_id, dxpl_id, read_bufs,
                  test_mode, true, true, false);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        VRFY((0 == memcmp(read_bufs[dset_idx], correct_bufs[dset_idx], correct_buf_size)),
             "Data verification succeeded");

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        free(read_bufs[dset_idx]);
        free(correct_bufs[dset_idx]);
    }

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        VRFY((H5Sclose(fspace_ids[dset_idx]) >= 0), "File dataspace close succeeded");
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");
    }

    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
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
 */
static void
test_write_3d_filtered_dataset_no_overlap_same_pages(const char *parent_group, H5Z_filter_t filter_id,
                                                     hid_t fapl_id, hid_t dcpl_id, hid_t dxpl_id,
                                                     test_mode_t test_mode)
{
    C_DATATYPE *correct_bufs[MAX_NUM_DSETS_MULTI] = {0};
    const void *data_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    void       *data_bufs_nc[MAX_NUM_DSETS_MULTI] = {0}; /* non-const buffer pointers for freeing */
    void       *read_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    hsize_t     dataset_dims[WRITE_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_DIMS];
    hsize_t     chunk_dims[WRITE_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_DIMS];
    hsize_t     sel_dims[WRITE_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_DIMS];
    hsize_t     start[WRITE_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_DIMS];
    hsize_t     stride[WRITE_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_DIMS];
    hsize_t     count[WRITE_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_DIMS];
    hsize_t     block[WRITE_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_DIMS];
    size_t      data_size, correct_buf_size;
    size_t      num_dsets;
    hid_t       dset_ids[MAX_NUM_DSETS_MULTI];
    hid_t       fspace_ids[MAX_NUM_DSETS_MULTI];
    hid_t       file_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID;
    hid_t       group_id  = H5I_INVALID_HID;
    hid_t       filespace = H5I_INVALID_HID;

    if (MAINPROCESS)
        puts("Testing write to unshared filtered chunks on the same pages in 3D dataset");

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

    /* Create chunked dataset */
    plist_id = H5Pcopy(dcpl_id);
    VRFY((plist_id >= 0), "DCPL copy succeeded");

    VRFY((H5Pset_chunk(plist_id, WRITE_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_DIMS, chunk_dims) >= 0),
         "Chunk size set");

    /* Add test filter to the pipeline */
    VRFY((set_dcpl_filter(plist_id, filter_id, NULL) >= 0), "Filter set");

    /* Create datasets depending on the current test mode */
    create_datasets(group_id, WRITE_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_NAME, HDF5_DATATYPE_NAME,
                    filespace, plist_id, test_mode, &num_dsets, dset_ids);

    /* Verify space allocation status */
    verify_space_alloc_status(num_dsets, dset_ids, plist_id, DATASET_JUST_CREATED);

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

    select_hyperslab(num_dsets, dset_ids, start, stride, count, block, fspace_ids);

    /* Fill data buffer */
    data_size = sel_dims[0] * sel_dims[1] * sel_dims[2] * sizeof(C_DATATYPE);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        C_DATATYPE *tmp_buf = (C_DATATYPE *)calloc(1, data_size);
        VRFY((NULL != tmp_buf), "calloc succeeded");

        for (size_t j = 0; j < data_size / sizeof(C_DATATYPE); j++)
            tmp_buf[j] = (C_DATATYPE)(GEN_DATA(j) + dset_idx);

        data_bufs[dset_idx]    = tmp_buf;
        data_bufs_nc[dset_idx] = tmp_buf;
    }

    write_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_BLOCK, fspace_ids, dcpl_id, dxpl_id,
                   data_bufs, test_mode, true, true, false);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        free(data_bufs_nc[dset_idx]);

    /* Verify space allocation status */
    verify_space_alloc_status(num_dsets, dset_ids, plist_id, ALL_CHUNKS_WRITTEN);

    /* Close and re-open datasets */
    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");

    open_datasets(group_id, WRITE_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_NAME, num_dsets, test_mode,
                  dset_ids);

    /* Verify the correct data was written */
    correct_buf_size = dataset_dims[0] * dataset_dims[1] * dataset_dims[2] * sizeof(C_DATATYPE);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        correct_bufs[dset_idx] = (C_DATATYPE *)calloc(1, correct_buf_size);
        VRFY((NULL != correct_bufs[dset_idx]), "calloc succeeded");
        read_bufs[dset_idx] = calloc(1, correct_buf_size);
        VRFY((NULL != read_bufs[dset_idx]), "calloc succeeded");

        for (size_t j = 0; j < correct_buf_size / sizeof(C_DATATYPE); j++)
            correct_bufs[dset_idx][j] = (C_DATATYPE)((j % (dataset_dims[0] * dataset_dims[1])) +
                                                     (j / (dataset_dims[0] * dataset_dims[1])) + dset_idx);
    }

    read_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, dcpl_id, dxpl_id, read_bufs,
                  test_mode, true, true, false);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        VRFY((0 == memcmp(read_bufs[dset_idx], correct_bufs[dset_idx], correct_buf_size)),
             "Data verification succeeded");

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        free(read_bufs[dset_idx]);
        free(correct_bufs[dset_idx]);
    }

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        VRFY((H5Sclose(fspace_ids[dset_idx]) >= 0), "File dataspace close succeeded");
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");
    }

    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
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
 */
static void
test_write_3d_filtered_dataset_overlap(const char *parent_group, H5Z_filter_t filter_id, hid_t fapl_id,
                                       hid_t dcpl_id, hid_t dxpl_id, test_mode_t test_mode)
{
    C_DATATYPE *correct_bufs[MAX_NUM_DSETS_MULTI] = {0};
    const void *data_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    void       *data_bufs_nc[MAX_NUM_DSETS_MULTI] = {0}; /* non-const buffer pointers for freeing */
    void       *read_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    hsize_t     dataset_dims[WRITE_SHARED_FILTERED_CHUNKS_3D_DATASET_DIMS];
    hsize_t     chunk_dims[WRITE_SHARED_FILTERED_CHUNKS_3D_DATASET_DIMS];
    hsize_t     sel_dims[WRITE_SHARED_FILTERED_CHUNKS_3D_DATASET_DIMS];
    hsize_t     start[WRITE_SHARED_FILTERED_CHUNKS_3D_DATASET_DIMS];
    hsize_t     stride[WRITE_SHARED_FILTERED_CHUNKS_3D_DATASET_DIMS];
    hsize_t     count[WRITE_SHARED_FILTERED_CHUNKS_3D_DATASET_DIMS];
    hsize_t     block[WRITE_SHARED_FILTERED_CHUNKS_3D_DATASET_DIMS];
    size_t      data_size, correct_buf_size;
    size_t      num_dsets;
    hid_t       dset_ids[MAX_NUM_DSETS_MULTI];
    hid_t       fspace_ids[MAX_NUM_DSETS_MULTI];
    hid_t       file_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID;
    hid_t       group_id  = H5I_INVALID_HID;
    hid_t       filespace = H5I_INVALID_HID;

    if (MAINPROCESS)
        puts("Testing write to shared filtered chunks in 3D dataset");

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

    /* Create chunked dataset */
    plist_id = H5Pcopy(dcpl_id);
    VRFY((plist_id >= 0), "DCPL copy succeeded");

    VRFY((H5Pset_chunk(plist_id, WRITE_SHARED_FILTERED_CHUNKS_3D_DATASET_DIMS, chunk_dims) >= 0),
         "Chunk size set");

    /* Add test filter to the pipeline */
    VRFY((set_dcpl_filter(plist_id, filter_id, NULL) >= 0), "Filter set");

    /* Create datasets depending on the current test mode */
    create_datasets(group_id, WRITE_SHARED_FILTERED_CHUNKS_3D_DATASET_NAME, HDF5_DATATYPE_NAME, filespace,
                    plist_id, test_mode, &num_dsets, dset_ids);

    /* Verify space allocation status */
    verify_space_alloc_status(num_dsets, dset_ids, plist_id, DATASET_JUST_CREATED);

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

    select_hyperslab(num_dsets, dset_ids, start, stride, count, block, fspace_ids);

    /* Fill data buffer */
    data_size = sel_dims[0] * sel_dims[1] * sel_dims[2] * sizeof(C_DATATYPE);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        C_DATATYPE *tmp_buf = (C_DATATYPE *)calloc(1, data_size);
        VRFY((NULL != tmp_buf), "calloc succeeded");

        for (size_t j = 0; j < data_size / sizeof(C_DATATYPE); j++)
            tmp_buf[j] = (C_DATATYPE)(GEN_DATA(j) + dset_idx);

        data_bufs[dset_idx]    = tmp_buf;
        data_bufs_nc[dset_idx] = tmp_buf;
    }

    write_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_BLOCK, fspace_ids, dcpl_id, dxpl_id,
                   data_bufs, test_mode, true, true, false);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        free(data_bufs_nc[dset_idx]);

    /* Verify space allocation status */
    verify_space_alloc_status(num_dsets, dset_ids, plist_id, ALL_CHUNKS_WRITTEN);

    /* Close and re-open datasets */
    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");

    open_datasets(group_id, WRITE_SHARED_FILTERED_CHUNKS_3D_DATASET_NAME, num_dsets, test_mode, dset_ids);

    /* Verify the correct data was written */
    correct_buf_size = dataset_dims[0] * dataset_dims[1] * dataset_dims[2] * sizeof(C_DATATYPE);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        correct_bufs[dset_idx] = (C_DATATYPE *)calloc(1, correct_buf_size);
        VRFY((NULL != correct_bufs[dset_idx]), "calloc succeeded");
        read_bufs[dset_idx] = calloc(1, correct_buf_size);
        VRFY((NULL != read_bufs[dset_idx]), "calloc succeeded");

        for (size_t j = 0; j < correct_buf_size / sizeof(C_DATATYPE); j++)
            correct_bufs[dset_idx][j] =
                /* Add the Column Index */
                (C_DATATYPE)((j % (hsize_t)(WRITE_SHARED_FILTERED_CHUNKS_3D_DEPTH *
                                            WRITE_SHARED_FILTERED_CHUNKS_3D_NCOLS))

                             /* Add the Row Index */
                             + ((j % (hsize_t)(mpi_size * WRITE_SHARED_FILTERED_CHUNKS_3D_DEPTH *
                                               WRITE_SHARED_FILTERED_CHUNKS_3D_NCOLS)) /
                                (hsize_t)(WRITE_SHARED_FILTERED_CHUNKS_3D_DEPTH *
                                          WRITE_SHARED_FILTERED_CHUNKS_3D_NCOLS))

                             /* Add the amount that gets added when a rank moves down to its next
                                section vertically in the dataset */
                             + ((hsize_t)(WRITE_SHARED_FILTERED_CHUNKS_3D_DEPTH *
                                          WRITE_SHARED_FILTERED_CHUNKS_3D_NCOLS) *
                                (j / (hsize_t)(mpi_size * WRITE_SHARED_FILTERED_CHUNKS_3D_DEPTH *
                                               WRITE_SHARED_FILTERED_CHUNKS_3D_NCOLS)))

                             /* Add an increment factor for the multi-dataset case */
                             + dset_idx);
    }

    read_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, dcpl_id, dxpl_id, read_bufs,
                  test_mode, true, true, false);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        VRFY((0 == memcmp(read_bufs[dset_idx], correct_bufs[dset_idx], correct_buf_size)),
             "Data verification succeeded");

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        free(read_bufs[dset_idx]);
        free(correct_bufs[dset_idx]);
    }

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        VRFY((H5Sclose(fspace_ids[dset_idx]) >= 0), "File dataspace close succeeded");
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");
    }

    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
    VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    return;
}

/*
 * Tests parallel write of filtered data to unshared
 * chunks using a compound datatype which doesn't
 * require a datatype conversion.
 *
 */
static void
test_write_cmpd_filtered_dataset_no_conversion_unshared(const char *parent_group, H5Z_filter_t filter_id,
                                                        hid_t fapl_id, hid_t dcpl_id, hid_t dxpl_id,
                                                        test_mode_t test_mode)
{
    COMPOUND_C_DATATYPE *correct_bufs[MAX_NUM_DSETS_MULTI] = {0};
    const void          *data_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    void                *data_bufs_nc[MAX_NUM_DSETS_MULTI] = {0}; /* non-const buffer pointers for freeing */
    void                *read_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    hsize_t              dataset_dims[WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t              chunk_dims[WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t              start[WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t              stride[WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t              count[WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t              block[WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_DIMS];
    size_t               data_size, correct_buf_size;
    size_t               num_dsets;
    hid_t                dset_ids[MAX_NUM_DSETS_MULTI];
    hid_t                fspace_ids[MAX_NUM_DSETS_MULTI];
    hid_t                file_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID;
    hid_t                group_id  = H5I_INVALID_HID;
    hid_t                memtype   = H5I_INVALID_HID;
    hid_t                filespace = H5I_INVALID_HID;

    if (MAINPROCESS)
        puts("Testing write to unshared filtered chunks in Compound Datatype dataset without Datatype "
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

    filespace = H5Screate_simple(WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_DIMS,
                                 dataset_dims, NULL);
    VRFY((filespace >= 0), "File dataspace creation succeeded");

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

    /* Create datasets depending on the current test mode */
    create_datasets(group_id, WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_NAME, memtype,
                    filespace, plist_id, test_mode, &num_dsets, dset_ids);

    /* Verify space allocation status */
    verify_space_alloc_status(num_dsets, dset_ids, plist_id, DATASET_JUST_CREATED);

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

    select_hyperslab(num_dsets, dset_ids, start, stride, count, block, fspace_ids);

    /* Fill data buffer */
    data_size = (hsize_t)WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_ENTRIES_PER_PROC *
                sizeof(COMPOUND_C_DATATYPE);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        COMPOUND_C_DATATYPE *tmp_buf = (COMPOUND_C_DATATYPE *)calloc(1, data_size);
        VRFY((NULL != tmp_buf), "calloc succeeded");

        for (size_t j = 0;
             j < (hsize_t)WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_ENTRIES_PER_PROC; j++) {
            tmp_buf[j].field1 = (short)(GEN_DATA(j) + dset_idx);
            tmp_buf[j].field2 = (int)(GEN_DATA(j) + dset_idx);
            tmp_buf[j].field3 = (long)(GEN_DATA(j) + dset_idx);
        }

        data_bufs[dset_idx]    = tmp_buf;
        data_bufs_nc[dset_idx] = tmp_buf;
    }

    write_datasets(num_dsets, dset_ids, memtype, H5S_BLOCK, fspace_ids, dcpl_id, dxpl_id, data_bufs,
                   test_mode, true, true, false);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        free(data_bufs_nc[dset_idx]);

    /* Verify space allocation status */
    verify_space_alloc_status(num_dsets, dset_ids, plist_id, ALL_CHUNKS_WRITTEN);

    /* Close and re-open datasets */
    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");

    open_datasets(group_id, WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_NAME, num_dsets,
                  test_mode, dset_ids);

    /* Verify the correct data was written */
    correct_buf_size = dataset_dims[0] * dataset_dims[1] * sizeof(COMPOUND_C_DATATYPE);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        correct_bufs[dset_idx] = (COMPOUND_C_DATATYPE *)calloc(1, correct_buf_size);
        VRFY((NULL != correct_bufs[dset_idx]), "calloc succeeded");
        read_bufs[dset_idx] = calloc(1, correct_buf_size);
        VRFY((NULL != read_bufs[dset_idx]), "calloc succeeded");

        for (size_t j = 0; j < correct_buf_size / sizeof(COMPOUND_C_DATATYPE); j++) {
            size_t val = (j % dataset_dims[1]) + (j / dataset_dims[1]) + dset_idx;

            correct_bufs[dset_idx][j].field1 = (short)val;
            correct_bufs[dset_idx][j].field2 = (int)val;
            correct_bufs[dset_idx][j].field3 = (long)val;
        }
    }

    read_datasets(num_dsets, dset_ids, memtype, H5S_ALL, H5S_ALL, dcpl_id, dxpl_id, read_bufs, test_mode,
                  true, true, false);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        VRFY((0 == memcmp(read_bufs[dset_idx], correct_bufs[dset_idx], correct_buf_size)),
             "Data verification succeeded");

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        free(read_bufs[dset_idx]);
        free(correct_bufs[dset_idx]);
    }

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        VRFY((H5Sclose(fspace_ids[dset_idx]) >= 0), "File dataspace close succeeded");
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");
    }

    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
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
 */
static void
test_write_cmpd_filtered_dataset_no_conversion_shared(const char *parent_group, H5Z_filter_t filter_id,
                                                      hid_t fapl_id, hid_t dcpl_id, hid_t dxpl_id,
                                                      test_mode_t test_mode)
{
    COMPOUND_C_DATATYPE *correct_bufs[MAX_NUM_DSETS_MULTI] = {0};
    const void          *data_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    void                *data_bufs_nc[MAX_NUM_DSETS_MULTI] = {0}; /* non-const buffer pointers for freeing */
    void                *read_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    hsize_t              dataset_dims[WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t              chunk_dims[WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t              start[WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t              stride[WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t              count[WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t              block[WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_DIMS];
    size_t               data_size, correct_buf_size;
    size_t               num_dsets;
    hid_t                dset_ids[MAX_NUM_DSETS_MULTI];
    hid_t                fspace_ids[MAX_NUM_DSETS_MULTI];
    hid_t                file_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID;
    hid_t                group_id  = H5I_INVALID_HID;
    hid_t                memtype   = H5I_INVALID_HID;
    hid_t                filespace = H5I_INVALID_HID;

    if (MAINPROCESS)
        puts("Testing write to shared filtered chunks in Compound Datatype dataset without Datatype "
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

    filespace = H5Screate_simple(WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_DIMS,
                                 dataset_dims, NULL);
    VRFY((filespace >= 0), "File dataspace creation succeeded");

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

    /* Create datasets depending on the current test mode */
    create_datasets(group_id, WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_NAME, memtype,
                    filespace, plist_id, test_mode, &num_dsets, dset_ids);

    /* Verify space allocation status */
    verify_space_alloc_status(num_dsets, dset_ids, plist_id, DATASET_JUST_CREATED);

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

    select_hyperslab(num_dsets, dset_ids, start, stride, count, block, fspace_ids);

    /* Fill data buffer */
    data_size = (hsize_t)WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_ENTRIES_PER_PROC *
                sizeof(COMPOUND_C_DATATYPE);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        COMPOUND_C_DATATYPE *tmp_buf = (COMPOUND_C_DATATYPE *)calloc(1, data_size);
        VRFY((NULL != tmp_buf), "calloc succeeded");

        for (size_t j = 0; j < (hsize_t)WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_ENTRIES_PER_PROC;
             j++) {
            tmp_buf[j].field1 = (short)(GEN_DATA(j) + dset_idx);
            tmp_buf[j].field2 = (int)(GEN_DATA(j) + dset_idx);
            tmp_buf[j].field3 = (long)(GEN_DATA(j) + dset_idx);
        }

        data_bufs[dset_idx]    = tmp_buf;
        data_bufs_nc[dset_idx] = tmp_buf;
    }

    write_datasets(num_dsets, dset_ids, memtype, H5S_BLOCK, fspace_ids, dcpl_id, dxpl_id, data_bufs,
                   test_mode, true, true, false);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        free(data_bufs_nc[dset_idx]);

    /* Verify space allocation status */
    verify_space_alloc_status(num_dsets, dset_ids, plist_id, ALL_CHUNKS_WRITTEN);

    /* Close and re-open datasets */
    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");

    open_datasets(group_id, WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_NAME, num_dsets,
                  test_mode, dset_ids);

    /* Verify the correct data was written */
    correct_buf_size = dataset_dims[0] * dataset_dims[1] * sizeof(COMPOUND_C_DATATYPE);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        correct_bufs[dset_idx] = (COMPOUND_C_DATATYPE *)calloc(1, correct_buf_size);
        VRFY((NULL != correct_bufs[dset_idx]), "calloc succeeded");
        read_bufs[dset_idx] = calloc(1, correct_buf_size);
        VRFY((NULL != read_bufs[dset_idx]), "calloc succeeded");

        for (size_t j = 0; j < correct_buf_size / sizeof(COMPOUND_C_DATATYPE); j++) {
            size_t val1 = (dataset_dims[1] * (j / ((hsize_t)mpi_size * dataset_dims[1])));
            size_t val2 = (j % dataset_dims[1]);
            size_t val3 = (((j % ((hsize_t)mpi_size * dataset_dims[1])) / dataset_dims[1]) % dataset_dims[1]);
            size_t val  = val1 + val2 + val3 + dset_idx;

            correct_bufs[dset_idx][j].field1 = (short)val;
            correct_bufs[dset_idx][j].field2 = (int)val;
            correct_bufs[dset_idx][j].field3 = (long)val;
        }
    }

    read_datasets(num_dsets, dset_ids, memtype, H5S_ALL, H5S_ALL, dcpl_id, dxpl_id, read_bufs, test_mode,
                  true, true, false);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        VRFY((0 == memcmp(read_bufs[dset_idx], correct_bufs[dset_idx], correct_buf_size)),
             "Data verification succeeded");

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        free(read_bufs[dset_idx]);
        free(correct_bufs[dset_idx]);
    }

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        VRFY((H5Sclose(fspace_ids[dset_idx]) >= 0), "File dataspace close succeeded");
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");
    }

    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
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
 */
static void
test_write_cmpd_filtered_dataset_type_conversion_unshared(const char *parent_group, H5Z_filter_t filter_id,
                                                          hid_t fapl_id, hid_t dcpl_id, hid_t dxpl_id,
                                                          test_mode_t test_mode)
{
    COMPOUND_C_DATATYPE *correct_bufs[MAX_NUM_DSETS_MULTI] = {0};
    const void          *data_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    void                *data_bufs_nc[MAX_NUM_DSETS_MULTI] = {0}; /* non-const buffer pointers for freeing */
    void                *read_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    hsize_t              dataset_dims[WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t              chunk_dims[WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t              start[WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t              stride[WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t              count[WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t              block[WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_DIMS];
    size_t               data_size, correct_buf_size;
    size_t               num_dsets;
    hid_t                dset_ids[MAX_NUM_DSETS_MULTI];
    hid_t                fspace_ids[MAX_NUM_DSETS_MULTI];
    hid_t                file_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID;
    hid_t                group_id = H5I_INVALID_HID;
    hid_t                filetype = H5I_INVALID_HID, memtype = H5I_INVALID_HID;
    hid_t                filespace = H5I_INVALID_HID;
    H5D_alloc_time_t     alloc_time;

    if (MAINPROCESS)
        puts("Testing write to unshared filtered chunks in Compound Datatype dataset with Datatype "
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

    filespace = H5Screate_simple(WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_DIMS,
                                 dataset_dims, NULL);
    VRFY((filespace >= 0), "File dataspace creation succeeded");

    /* Retrieve allocation time */
    VRFY((H5Pget_alloc_time(dcpl_id, &alloc_time) >= 0), "H5Pget_alloc_time succeeded");

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

    /* Create datasets depending on the current test mode */
    create_datasets(group_id, WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_NAME, filetype,
                    filespace, plist_id, test_mode, &num_dsets, dset_ids);

    /* Verify space allocation status */
    verify_space_alloc_status(num_dsets, dset_ids, plist_id, DATASET_JUST_CREATED);

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

    select_hyperslab(num_dsets, dset_ids, start, stride, count, block, fspace_ids);

    /* Fill data buffer */
    data_size = (hsize_t)WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_ENTRIES_PER_PROC *
                sizeof(COMPOUND_C_DATATYPE);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        COMPOUND_C_DATATYPE *tmp_buf = (COMPOUND_C_DATATYPE *)calloc(1, data_size);
        VRFY((NULL != tmp_buf), "calloc succeeded");

        for (size_t j = 0;
             j < (hsize_t)WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_ENTRIES_PER_PROC; j++) {
            tmp_buf[j].field1 = (short)(GEN_DATA(j) + dset_idx);
            tmp_buf[j].field2 = (int)(GEN_DATA(j) + dset_idx);
            tmp_buf[j].field3 = (long)(GEN_DATA(j) + dset_idx);
        }

        data_bufs[dset_idx]    = tmp_buf;
        data_bufs_nc[dset_idx] = tmp_buf;
    }

    /*
     * Ensure that this test currently fails in most cases since type
     * conversions break collective mode when selection I/O is disabled
     * and the library will currently disable selection I/O when filters
     * are applied to a dataset.
     */

    /* NOTE: Once type conversions no longer break collective mode, remove
     * the H5E_BEGIN/END_TRY block and switch to the following code instead
     * of the H5Dwrite loop:
     */
    /* write_datasets(num_dsets, dset_ids, memtype, H5S_BLOCK, fspace_ids,
                      dcpl_id, dxpl_id, data_bufs, test_mode, true, true, false); */
    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        herr_t expected = FAIL;
        herr_t ret;

        /*
         * Since this currently writes datasets one by one regardless of
         * test mode, the write call could succeed if the dataset doesn't
         * have any filters applied to it (can currently only happen when
         * testing a mix of filtered and unfiltered datasets with the
         * multi-dataset APIs).
         */
        if (test_mode == USE_MULTIPLE_DATASETS_MIXED_FILTERED) {
            hid_t dset_dcpl;
            int   nfilters;

            dset_dcpl = H5Dget_create_plist(dset_ids[dset_idx]);
            VRFY((dset_dcpl >= 0), "H5Dget_create_plist");

            nfilters = H5Pget_nfilters(dset_dcpl);
            VRFY((nfilters >= 0), "H5Pget_nfilters");

            if (nfilters == 0)
                expected = SUCCEED;

            VRFY((H5Pclose(dset_dcpl) >= 0), "H5Pclose");
        }

        if (expected == SUCCEED)
            ret = H5Dwrite(dset_ids[dset_idx], memtype, H5S_BLOCK, fspace_ids[dset_idx], dxpl_id,
                           data_bufs[dset_idx]);
        else {
            H5E_BEGIN_TRY
            {
                ret = H5Dwrite(dset_ids[dset_idx], memtype, H5S_BLOCK, fspace_ids[dset_idx], dxpl_id,
                               data_bufs[dset_idx]);
            }
            H5E_END_TRY
        }

        VRFY((ret == expected), "Dataset write");

        if (expected == SUCCEED)
            verify_chunk_opt_status(1, test_mode, true, false, true, false, false, dxpl_id);
        else
            verify_chunk_opt_status(0, test_mode, false, true, true, false, alloc_time == H5D_ALLOC_TIME_LATE,
                                    dxpl_id);
    }

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        free(data_bufs_nc[dset_idx]);

    /* Verify space allocation status */
    verify_space_alloc_status(num_dsets, dset_ids, plist_id, NO_CHUNKS_WRITTEN);

    /* Close and re-open datasets */
    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");

    open_datasets(group_id, WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_NAME, num_dsets,
                  test_mode, dset_ids);

    /* Verify that no data was written */
    correct_buf_size = dataset_dims[0] * dataset_dims[1] * sizeof(COMPOUND_C_DATATYPE);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        correct_bufs[dset_idx] = (COMPOUND_C_DATATYPE *)calloc(1, correct_buf_size);
        VRFY((NULL != correct_bufs[dset_idx]), "calloc succeeded");
        read_bufs[dset_idx] = calloc(1, correct_buf_size);
        VRFY((NULL != read_bufs[dset_idx]), "calloc succeeded");
    }

    /* If some writes succeeded (due to mixed filtered mode) or if allocation time is late, then there is data
     * on disk to be read */
    read_datasets(num_dsets, dset_ids, memtype, H5S_ALL, H5S_ALL, dcpl_id, dxpl_id, read_bufs, test_mode,
                  true, false,
                  !(test_mode == USE_MULTIPLE_DATASETS_MIXED_FILTERED || alloc_time == H5D_ALLOC_TIME_LATE));

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        hid_t dset_dcpl;
        int   nfilters;

        dset_dcpl = H5Dget_create_plist(dset_ids[dset_idx]);
        VRFY((dset_dcpl >= 0), "H5Dget_create_plist");

        nfilters = H5Pget_nfilters(dset_dcpl);
        VRFY((nfilters >= 0), "H5Pget_nfilters");

        VRFY((H5Pclose(dset_dcpl) >= 0), "H5Pclose");

        /*
         * TODO: For now, skip data verification for the datasets where
         *       writes with type conversion succeeded due to selection
         *       I/O being enabled.
         */
        if (nfilters == 0)
            continue;

        VRFY((0 == memcmp(read_bufs[dset_idx], correct_bufs[dset_idx], correct_buf_size)),
             "Data verification succeeded");
    }

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        free(read_bufs[dset_idx]);
        free(correct_bufs[dset_idx]);
    }

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        VRFY((H5Sclose(fspace_ids[dset_idx]) >= 0), "File dataspace close succeeded");
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");
    }

    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
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
 */
static void
test_write_cmpd_filtered_dataset_type_conversion_shared(const char *parent_group, H5Z_filter_t filter_id,
                                                        hid_t fapl_id, hid_t dcpl_id, hid_t dxpl_id,
                                                        test_mode_t test_mode)
{
    COMPOUND_C_DATATYPE *correct_bufs[MAX_NUM_DSETS_MULTI] = {0};
    const void          *data_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    void                *data_bufs_nc[MAX_NUM_DSETS_MULTI] = {0}; /* non-const buffer pointers for freeing */
    void                *read_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    hsize_t              dataset_dims[WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t              chunk_dims[WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t              start[WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t              stride[WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t              count[WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t              block[WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_DIMS];
    size_t               data_size, correct_buf_size;
    size_t               num_dsets;
    hid_t                dset_ids[MAX_NUM_DSETS_MULTI];
    hid_t                fspace_ids[MAX_NUM_DSETS_MULTI];
    hid_t                file_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID;
    hid_t                group_id = H5I_INVALID_HID;
    hid_t                filetype = H5I_INVALID_HID, memtype = H5I_INVALID_HID;
    hid_t                filespace = H5I_INVALID_HID;
    H5D_alloc_time_t     alloc_time;

    if (MAINPROCESS)
        puts("Testing write to shared filtered chunks in Compound Datatype dataset with Datatype conversion");

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

    filespace = H5Screate_simple(WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_DIMS,
                                 dataset_dims, NULL);
    VRFY((filespace >= 0), "File dataspace creation succeeded");

    /* Retrieve allocation time */
    VRFY((H5Pget_alloc_time(dcpl_id, &alloc_time) >= 0), "H5Pget_alloc_time succeeded");

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

    /* Create datasets depending on the current test mode */
    create_datasets(group_id, WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_NAME, filetype,
                    filespace, plist_id, test_mode, &num_dsets, dset_ids);

    /* Verify space allocation status */
    verify_space_alloc_status(num_dsets, dset_ids, plist_id, DATASET_JUST_CREATED);

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

    select_hyperslab(num_dsets, dset_ids, start, stride, count, block, fspace_ids);

    /* Fill data buffer */
    data_size = (hsize_t)WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_ENTRIES_PER_PROC *
                sizeof(COMPOUND_C_DATATYPE);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        COMPOUND_C_DATATYPE *tmp_buf = (COMPOUND_C_DATATYPE *)calloc(1, data_size);
        VRFY((NULL != tmp_buf), "calloc succeeded");

        for (size_t j = 0;
             j < (hsize_t)WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_ENTRIES_PER_PROC; j++) {
            tmp_buf[j].field1 = (short)(GEN_DATA(j) + dset_idx);
            tmp_buf[j].field2 = (int)(GEN_DATA(j) + dset_idx);
            tmp_buf[j].field3 = (long)(GEN_DATA(j) + dset_idx);
        }

        data_bufs[dset_idx]    = tmp_buf;
        data_bufs_nc[dset_idx] = tmp_buf;
    }

    /*
     * Ensure that this test currently fails in most cases since type
     * conversions break collective mode when selection I/O is disabled
     * and the library will currently disable selection I/O when filters
     * are applied to a dataset.
     */

    /* NOTE: Once type conversions no longer break collective mode, remove
     * the H5E_BEGIN/END_TRY block and switch to the following code instead
     * of the H5Dwrite loop:
     */
    /* write_datasets(num_dsets, dset_ids, memtype, H5S_BLOCK, fspace_ids,
                      dcpl_id, dxpl_id, data_bufs, test_mode, true, true, false); */
    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        herr_t expected = FAIL;
        herr_t ret;

        /*
         * Since this currently writes datasets one by one regardless of
         * test mode, the write call could succeed if the dataset doesn't
         * have any filters applied to it (can currently only happen when
         * testing a mix of filtered and unfiltered datasets with the
         * multi-dataset APIs).
         */
        if (test_mode == USE_MULTIPLE_DATASETS_MIXED_FILTERED) {
            hid_t dset_dcpl;
            int   nfilters;

            dset_dcpl = H5Dget_create_plist(dset_ids[dset_idx]);
            VRFY((dset_dcpl >= 0), "H5Dget_create_plist");

            nfilters = H5Pget_nfilters(dset_dcpl);
            VRFY((nfilters >= 0), "H5Pget_nfilters");

            if (nfilters == 0)
                expected = SUCCEED;

            VRFY((H5Pclose(dset_dcpl) >= 0), "H5Pclose");
        }

        if (expected == SUCCEED)
            ret = H5Dwrite(dset_ids[dset_idx], memtype, H5S_BLOCK, fspace_ids[dset_idx], dxpl_id,
                           data_bufs[dset_idx]);
        else {
            H5E_BEGIN_TRY
            {
                ret = H5Dwrite(dset_ids[dset_idx], memtype, H5S_BLOCK, fspace_ids[dset_idx], dxpl_id,
                               data_bufs[dset_idx]);
            }
            H5E_END_TRY
        }

        VRFY((ret == expected), "Dataset write");

        if (expected == SUCCEED)
            verify_chunk_opt_status(1, test_mode, true, false, true, false, false, dxpl_id);
        else
            verify_chunk_opt_status(0, test_mode, false, true, true, false, alloc_time == H5D_ALLOC_TIME_LATE,
                                    dxpl_id);
    }

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        free(data_bufs_nc[dset_idx]);

    /* Verify space allocation status */
    verify_space_alloc_status(num_dsets, dset_ids, plist_id, NO_CHUNKS_WRITTEN);

    /* Close and re-open datasets */
    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");

    open_datasets(group_id, WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_NAME, num_dsets,
                  test_mode, dset_ids);

    /* Verify that no data was written */
    correct_buf_size = dataset_dims[0] * dataset_dims[1] * sizeof(COMPOUND_C_DATATYPE);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        correct_bufs[dset_idx] = (COMPOUND_C_DATATYPE *)calloc(1, correct_buf_size);
        VRFY((NULL != correct_bufs[dset_idx]), "calloc succeeded");
        read_bufs[dset_idx] = calloc(1, correct_buf_size);
        VRFY((NULL != read_bufs[dset_idx]), "calloc succeeded");
    }

    /* If some writes succeeded (due to mixed filtered mode) or if allocation time is late, then there is data
     * on disk to be read */
    read_datasets(num_dsets, dset_ids, memtype, H5S_ALL, H5S_ALL, dcpl_id, dxpl_id, read_bufs, test_mode,
                  true, false,
                  !(test_mode == USE_MULTIPLE_DATASETS_MIXED_FILTERED || alloc_time == H5D_ALLOC_TIME_LATE));

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        hid_t dset_dcpl;
        int   nfilters;

        dset_dcpl = H5Dget_create_plist(dset_ids[dset_idx]);
        VRFY((dset_dcpl >= 0), "H5Dget_create_plist");

        nfilters = H5Pget_nfilters(dset_dcpl);
        VRFY((nfilters >= 0), "H5Pget_nfilters");

        VRFY((H5Pclose(dset_dcpl) >= 0), "H5Pclose");

        /*
         * TODO: For now, skip data verification for the datasets where
         *       writes with type conversion succeeded due to selection
         *       I/O being enabled.
         */
        if (nfilters == 0)
            continue;

        VRFY((0 == memcmp(read_bufs[dset_idx], correct_bufs[dset_idx], correct_buf_size)),
             "Data verification succeeded");
    }

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        free(read_bufs[dset_idx]);
        free(correct_bufs[dset_idx]);
    }

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        VRFY((H5Sclose(fspace_ids[dset_idx]) >= 0), "File dataspace close succeeded");
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");
    }

    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
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
 */
static void
test_read_one_chunk_filtered_dataset(const char *parent_group, H5Z_filter_t filter_id, hid_t fapl_id,
                                     hid_t dcpl_id, hid_t dxpl_id, test_mode_t test_mode)
{
    const void *data_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    void       *data_bufs_nc[MAX_NUM_DSETS_MULTI] = {0}; /* non-const buffer pointers for freeing */
    void       *read_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    void       *global_buf                        = NULL;
    hsize_t     dataset_dims[READ_ONE_CHUNK_FILTERED_DATASET_DIMS];
    hsize_t     chunk_dims[READ_ONE_CHUNK_FILTERED_DATASET_DIMS];
    hsize_t     sel_dims[READ_ONE_CHUNK_FILTERED_DATASET_DIMS];
    hsize_t     start[READ_ONE_CHUNK_FILTERED_DATASET_DIMS];
    hsize_t     stride[READ_ONE_CHUNK_FILTERED_DATASET_DIMS];
    hsize_t     count[READ_ONE_CHUNK_FILTERED_DATASET_DIMS];
    hsize_t     block[READ_ONE_CHUNK_FILTERED_DATASET_DIMS];
    hsize_t     flat_dims[1];
    size_t      data_size, read_buf_size;
    size_t      num_dsets;
    hid_t       dset_ids[MAX_NUM_DSETS_MULTI];
    hid_t       fspace_ids[MAX_NUM_DSETS_MULTI];
    hid_t       file_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID;
    hid_t       group_id   = H5I_INVALID_HID;
    hid_t       filespace  = H5I_INVALID_HID;
    int        *recvcounts = NULL;
    int        *displs     = NULL;
    int         mpi_code;

    if (MAINPROCESS)
        puts("Testing read from one-chunk filtered dataset");

    dataset_dims[0] = (hsize_t)READ_ONE_CHUNK_FILTERED_DATASET_NROWS;
    dataset_dims[1] = (hsize_t)READ_ONE_CHUNK_FILTERED_DATASET_NCOLS;

    /* Setup the buffer for writing and for comparison */
    data_size = dataset_dims[0] * dataset_dims[1] * sizeof(C_DATATYPE);

    for (size_t dset_idx = 0; dset_idx < MAX_NUM_DSETS_MULTI; dset_idx++) {
        C_DATATYPE *tmp_buf = calloc(1, data_size);
        VRFY((NULL != tmp_buf), "calloc succeeded");

        for (size_t j = 0; j < data_size / sizeof(C_DATATYPE); j++)
            tmp_buf[j] = ((C_DATATYPE)j % (READ_ONE_CHUNK_FILTERED_DATASET_CH_NROWS / mpi_size *
                                           READ_ONE_CHUNK_FILTERED_DATASET_CH_NCOLS)) +
                         ((C_DATATYPE)j / (READ_ONE_CHUNK_FILTERED_DATASET_CH_NROWS / mpi_size *
                                           READ_ONE_CHUNK_FILTERED_DATASET_CH_NCOLS)) +
                         (C_DATATYPE)dset_idx;

        data_bufs[dset_idx]    = tmp_buf;
        data_bufs_nc[dset_idx] = tmp_buf;
    }

    file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, fapl_id);
    VRFY((file_id >= 0), "Test file open succeeded");

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

    VRFY((H5Pset_chunk(plist_id, READ_ONE_CHUNK_FILTERED_DATASET_DIMS, chunk_dims) >= 0), "Chunk size set");

    /* Add test filter to the pipeline */
    VRFY((set_dcpl_filter(plist_id, filter_id, NULL) >= 0), "Filter set");

    /* Create datasets depending on the current test mode */
    create_datasets(group_id, READ_ONE_CHUNK_FILTERED_DATASET_NAME, HDF5_DATATYPE_NAME, filespace, plist_id,
                    test_mode, &num_dsets, dset_ids);

    /* Verify space allocation status */
    verify_space_alloc_status(num_dsets, dset_ids, plist_id, DATASET_JUST_CREATED);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");

    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
    VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    MPI_Barrier(comm);

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

        open_datasets(group_id, READ_ONE_CHUNK_FILTERED_DATASET_NAME, num_dsets, test_mode, dset_ids);

        select_all(num_dsets, dset_ids, fspace_ids);

        write_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_ALL, fspace_ids, dcpl_id, H5P_DEFAULT,
                       data_bufs, test_mode, true, false, false);

        /* Verify space allocation status */
        plist_id = H5Dget_create_plist(dset_ids[0]);
        VRFY((plist_id >= 0), "H5Dget_create_plist succeeded");
        verify_space_alloc_status(num_dsets, dset_ids, plist_id, ALL_CHUNKS_WRITTEN);

        for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
            VRFY((H5Sclose(fspace_ids[dset_idx]) >= 0), "File dataspace close succeeded");
            VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");
        }

        VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
        VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
        VRFY((H5Fclose(file_id) >= 0), "File close succeeded");
    }

    MPI_Barrier(comm);

    file_id = H5Fopen(filenames[0], H5F_ACC_RDONLY, fapl_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
    VRFY((group_id >= 0), "H5Gopen2 succeeded");

    open_datasets(group_id, READ_ONE_CHUNK_FILTERED_DATASET_NAME, num_dsets, test_mode, dset_ids);

    sel_dims[0] = (hsize_t)READ_ONE_CHUNK_FILTERED_DATASET_NROWS / (hsize_t)mpi_size;
    sel_dims[1] = (hsize_t)READ_ONE_CHUNK_FILTERED_DATASET_NCOLS;

    /* Setup one-dimensional memory dataspace for reading the dataset data into a contiguous buffer */
    flat_dims[0] = sel_dims[0] * sel_dims[1];

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

    select_hyperslab(num_dsets, dset_ids, start, stride, count, block, fspace_ids);

    read_buf_size = flat_dims[0] * sizeof(C_DATATYPE);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        read_bufs[dset_idx] = calloc(1, read_buf_size);
        VRFY((NULL != read_bufs[dset_idx]), "calloc succeeded");
    }

    read_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_BLOCK, fspace_ids[0], dcpl_id, dxpl_id,
                  read_bufs, test_mode, true, true, false);

    /* Collect each piece of data from all ranks into a global buffer on all ranks */
    global_buf = calloc(1, data_size);
    VRFY((NULL != global_buf), "calloc succeeded");
    recvcounts = calloc(1, (size_t)mpi_size * sizeof(*recvcounts));
    VRFY((NULL != recvcounts), "calloc succeeded");
    displs = calloc(1, (size_t)mpi_size * sizeof(*displs));
    VRFY((NULL != displs), "calloc succeeded");

    for (size_t i = 0; i < (size_t)mpi_size; i++) {
        recvcounts[i] = (int)flat_dims[0];
        displs[i]     = (int)(i * flat_dims[0]);
    }

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        mpi_code = MPI_Allgatherv(read_bufs[dset_idx], (int)flat_dims[0], C_DATATYPE_MPI, global_buf,
                                  recvcounts, displs, C_DATATYPE_MPI, comm);
        VRFY((MPI_SUCCESS == mpi_code), "MPI_Allgatherv succeeded");

        VRFY((0 == memcmp(global_buf, data_bufs[dset_idx], data_size)), "Data verification succeeded");
    }

    free(displs);
    free(recvcounts);
    free(global_buf);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        free(read_bufs[dset_idx]);

    for (size_t dset_idx = 0; dset_idx < MAX_NUM_DSETS_MULTI; dset_idx++)
        free(data_bufs_nc[dset_idx]);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        VRFY((H5Sclose(fspace_ids[dset_idx]) >= 0), "File dataspace close succeeded");
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");
    }

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
 */
static void
test_read_filtered_dataset_no_overlap(const char *parent_group, H5Z_filter_t filter_id, hid_t fapl_id,
                                      hid_t dcpl_id, hid_t dxpl_id, test_mode_t test_mode)
{
    const void *data_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    void       *data_bufs_nc[MAX_NUM_DSETS_MULTI] = {0}; /* non-const buffer pointers for freeing */
    void       *read_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    void       *global_buf                        = NULL;
    hsize_t     dataset_dims[READ_UNSHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     chunk_dims[READ_UNSHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     sel_dims[READ_UNSHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     start[READ_UNSHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     stride[READ_UNSHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     count[READ_UNSHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     block[READ_UNSHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     flat_dims[1];
    size_t      data_size, read_buf_size;
    size_t      num_dsets;
    hid_t       dset_ids[MAX_NUM_DSETS_MULTI];
    hid_t       fspace_ids[MAX_NUM_DSETS_MULTI];
    hid_t       file_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID;
    hid_t       group_id   = H5I_INVALID_HID;
    hid_t       filespace  = H5I_INVALID_HID;
    int        *recvcounts = NULL;
    int        *displs     = NULL;
    int         mpi_code;

    if (MAINPROCESS)
        puts("Testing read from unshared filtered chunks");

    dataset_dims[0] = (hsize_t)READ_UNSHARED_FILTERED_CHUNKS_NROWS;
    dataset_dims[1] = (hsize_t)READ_UNSHARED_FILTERED_CHUNKS_NCOLS;

    /* Setup the buffer for writing and for comparison */
    data_size = (hsize_t)READ_UNSHARED_FILTERED_CHUNKS_NROWS * (hsize_t)READ_UNSHARED_FILTERED_CHUNKS_NCOLS *
                sizeof(C_DATATYPE);

    for (size_t dset_idx = 0; dset_idx < MAX_NUM_DSETS_MULTI; dset_idx++) {
        C_DATATYPE *tmp_buf = calloc(1, data_size);
        VRFY((NULL != tmp_buf), "calloc succeeded");

        for (size_t j = 0; j < data_size / sizeof(C_DATATYPE); j++)
            tmp_buf[j] =
                (C_DATATYPE)((j % (dataset_dims[0] / (hsize_t)mpi_size * dataset_dims[1])) +
                             (j / (dataset_dims[0] / (hsize_t)mpi_size * dataset_dims[1])) + dset_idx);

        data_bufs[dset_idx]    = tmp_buf;
        data_bufs_nc[dset_idx] = tmp_buf;
    }

    file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, fapl_id);
    VRFY((file_id >= 0), "Test file open succeeded");

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

    /* Create datasets depending on the current test mode */
    create_datasets(group_id, READ_UNSHARED_FILTERED_CHUNKS_DATASET_NAME, HDF5_DATATYPE_NAME, filespace,
                    plist_id, test_mode, &num_dsets, dset_ids);

    /* Verify space allocation status */
    verify_space_alloc_status(num_dsets, dset_ids, plist_id, DATASET_JUST_CREATED);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");

    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
    VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    MPI_Barrier(comm);

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

        open_datasets(group_id, READ_UNSHARED_FILTERED_CHUNKS_DATASET_NAME, num_dsets, test_mode, dset_ids);

        select_all(num_dsets, dset_ids, fspace_ids);

        write_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_ALL, fspace_ids, dcpl_id, H5P_DEFAULT,
                       data_bufs, test_mode, true, false, false);

        /* Verify space allocation status */
        plist_id = H5Dget_create_plist(dset_ids[0]);
        VRFY((plist_id >= 0), "H5Dget_create_plist succeeded");
        verify_space_alloc_status(num_dsets, dset_ids, plist_id, ALL_CHUNKS_WRITTEN);

        for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
            VRFY((H5Sclose(fspace_ids[dset_idx]) >= 0), "File dataspace close succeeded");
            VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");
        }

        VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
        VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
        VRFY((H5Fclose(file_id) >= 0), "File close succeeded");
    }

    MPI_Barrier(comm);

    file_id = H5Fopen(filenames[0], H5F_ACC_RDONLY, fapl_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
    VRFY((group_id >= 0), "H5Gopen2 succeeded");

    open_datasets(group_id, READ_UNSHARED_FILTERED_CHUNKS_DATASET_NAME, num_dsets, test_mode, dset_ids);

    sel_dims[0] = (hsize_t)READ_UNSHARED_FILTERED_CHUNKS_CH_NROWS;
    sel_dims[1] = (hsize_t)READ_UNSHARED_FILTERED_CHUNKS_NCOLS;

    /* Setup one-dimensional memory dataspace for reading the dataset data into a contiguous buffer */
    flat_dims[0] = sel_dims[0] * sel_dims[1];

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

    select_hyperslab(num_dsets, dset_ids, start, stride, count, block, fspace_ids);

    read_buf_size = flat_dims[0] * sizeof(C_DATATYPE);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        read_bufs[dset_idx] = calloc(1, read_buf_size);
        VRFY((NULL != read_bufs[dset_idx]), "calloc succeeded");
    }

    read_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_BLOCK, fspace_ids[0], dcpl_id, dxpl_id,
                  read_bufs, test_mode, true, true, false);

    /* Collect each piece of data from all ranks into a global buffer on all ranks */
    global_buf = calloc(1, data_size);
    VRFY((NULL != global_buf), "calloc succeeded");
    recvcounts = calloc(1, (size_t)mpi_size * sizeof(*recvcounts));
    VRFY((NULL != recvcounts), "calloc succeeded");
    displs = calloc(1, (size_t)mpi_size * sizeof(*displs));
    VRFY((NULL != displs), "calloc succeeded");

    for (size_t i = 0; i < (size_t)mpi_size; i++) {
        recvcounts[i] = (int)flat_dims[0];
        displs[i]     = (int)(i * flat_dims[0]);
    }

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        mpi_code = MPI_Allgatherv(read_bufs[dset_idx], (int)flat_dims[0], C_DATATYPE_MPI, global_buf,
                                  recvcounts, displs, C_DATATYPE_MPI, comm);
        VRFY((MPI_SUCCESS == mpi_code), "MPI_Allgatherv succeeded");

        VRFY((0 == memcmp(global_buf, data_bufs[dset_idx], data_size)), "Data verification succeeded");
    }

    free(displs);
    free(recvcounts);
    free(global_buf);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        free(read_bufs[dset_idx]);

    for (size_t dset_idx = 0; dset_idx < MAX_NUM_DSETS_MULTI; dset_idx++)
        free(data_bufs_nc[dset_idx]);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        VRFY((H5Sclose(fspace_ids[dset_idx]) >= 0), "File dataspace close succeeded");
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");
    }

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
 */
static void
test_read_filtered_dataset_overlap(const char *parent_group, H5Z_filter_t filter_id, hid_t fapl_id,
                                   hid_t dcpl_id, hid_t dxpl_id, test_mode_t test_mode)
{
    const void *data_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    void       *data_bufs_nc[MAX_NUM_DSETS_MULTI] = {0}; /* non-const buffer pointers for freeing */
    void       *read_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    void       *global_buf                        = NULL;
    hsize_t     dataset_dims[READ_SHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     chunk_dims[READ_SHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     sel_dims[READ_SHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     start[READ_SHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     stride[READ_SHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     count[READ_SHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     block[READ_SHARED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     flat_dims[1];
    size_t      data_size, read_buf_size;
    size_t      num_dsets;
    hid_t       dset_ids[MAX_NUM_DSETS_MULTI];
    hid_t       fspace_ids[MAX_NUM_DSETS_MULTI];
    hid_t       file_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID;
    hid_t       group_id   = H5I_INVALID_HID;
    hid_t       filespace  = H5I_INVALID_HID;
    int        *recvcounts = NULL;
    int        *displs     = NULL;
    int         mpi_code;

    if (MAINPROCESS)
        puts("Testing read from shared filtered chunks");

    dataset_dims[0] = (hsize_t)READ_SHARED_FILTERED_CHUNKS_NROWS;
    dataset_dims[1] = (hsize_t)READ_SHARED_FILTERED_CHUNKS_NCOLS;

    /* Setup the buffer for writing and for comparison */
    data_size = dataset_dims[0] * dataset_dims[1] * sizeof(C_DATATYPE);

    for (size_t dset_idx = 0; dset_idx < MAX_NUM_DSETS_MULTI; dset_idx++) {
        C_DATATYPE *tmp_buf = calloc(1, data_size);
        VRFY((NULL != tmp_buf), "calloc succeeded");

        for (size_t j = 0; j < data_size / sizeof(C_DATATYPE); j++)
            tmp_buf[j] = (C_DATATYPE)((dataset_dims[1] * (j / ((hsize_t)mpi_size * dataset_dims[1]))) +
                                      (j % dataset_dims[1]) +
                                      (((j % ((hsize_t)mpi_size * dataset_dims[1])) / dataset_dims[1]) %
                                       dataset_dims[1]) +
                                      dset_idx);

        data_bufs[dset_idx]    = tmp_buf;
        data_bufs_nc[dset_idx] = tmp_buf;
    }

    file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, fapl_id);
    VRFY((file_id >= 0), "Test file open succeeded");

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

    /* Create datasets depending on the current test mode */
    create_datasets(group_id, READ_SHARED_FILTERED_CHUNKS_DATASET_NAME, HDF5_DATATYPE_NAME, filespace,
                    plist_id, test_mode, &num_dsets, dset_ids);

    /* Verify space allocation status */
    verify_space_alloc_status(num_dsets, dset_ids, plist_id, DATASET_JUST_CREATED);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");

    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
    VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    MPI_Barrier(comm);

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

        open_datasets(group_id, READ_SHARED_FILTERED_CHUNKS_DATASET_NAME, num_dsets, test_mode, dset_ids);

        select_all(num_dsets, dset_ids, fspace_ids);

        write_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_ALL, fspace_ids, dcpl_id, H5P_DEFAULT,
                       data_bufs, test_mode, true, false, false);

        /* Verify space allocation status */
        plist_id = H5Dget_create_plist(dset_ids[0]);
        VRFY((plist_id >= 0), "H5Dget_create_plist succeeded");
        verify_space_alloc_status(num_dsets, dset_ids, plist_id, ALL_CHUNKS_WRITTEN);

        for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
            VRFY((H5Sclose(fspace_ids[dset_idx]) >= 0), "File dataspace close succeeded");
            VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");
        }

        VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
        VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
        VRFY((H5Fclose(file_id) >= 0), "File close succeeded");
    }

    MPI_Barrier(comm);

    file_id = H5Fopen(filenames[0], H5F_ACC_RDONLY, fapl_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
    VRFY((group_id >= 0), "H5Gopen2 succeeded");

    open_datasets(group_id, READ_SHARED_FILTERED_CHUNKS_DATASET_NAME, num_dsets, test_mode, dset_ids);

    sel_dims[0] = (hsize_t)DIM0_SCALE_FACTOR;
    sel_dims[1] = (hsize_t)READ_SHARED_FILTERED_CHUNKS_CH_NCOLS * (hsize_t)DIM1_SCALE_FACTOR;

    /* Setup one-dimensional memory dataspace for reading the dataset data into a contiguous buffer */
    flat_dims[0] = sel_dims[0] * sel_dims[1];

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

    select_hyperslab(num_dsets, dset_ids, start, stride, count, block, fspace_ids);

    read_buf_size = flat_dims[0] * sizeof(C_DATATYPE);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        read_bufs[dset_idx] = calloc(1, read_buf_size);
        VRFY((NULL != read_bufs[dset_idx]), "calloc succeeded");
    }

    read_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_BLOCK, fspace_ids[0], dcpl_id, dxpl_id,
                  read_bufs, test_mode, true, true, false);

    /* Collect each piece of data from all ranks into a global buffer on all ranks */
    global_buf = calloc(1, data_size);
    VRFY((NULL != global_buf), "calloc succeeded");
    recvcounts = calloc(1, (size_t)mpi_size * sizeof(*recvcounts));
    VRFY((NULL != recvcounts), "calloc succeeded");
    displs = calloc(1, (size_t)mpi_size * sizeof(*displs));
    VRFY((NULL != displs), "calloc succeeded");

    /*
     * Since these chunks are shared, run multiple rounds of MPI_Allgatherv
     * to collect all of the pieces into their appropriate locations. The
     * number of times MPI_Allgatherv is run for each dataset should be equal
     * to the number of chunks in the first dimension of the dataset.
     */
    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        size_t total_recvcounts = 0;

        for (size_t j = 0; j < (size_t)mpi_size; j++) {
            recvcounts[j] = (int)dataset_dims[1];
            total_recvcounts += (size_t)recvcounts[j];

            displs[j] = (int)(j * dataset_dims[1]);
        }

        for (size_t loop_count = count[0]; loop_count; loop_count--) {
            C_DATATYPE *tmp_buf      = (C_DATATYPE *)read_bufs[dset_idx];
            C_DATATYPE *tmp_glob_buf = (C_DATATYPE *)global_buf;

            mpi_code =
                MPI_Allgatherv(&tmp_buf[(count[0] - loop_count) * dataset_dims[1]], recvcounts[mpi_rank],
                               C_DATATYPE_MPI, &tmp_glob_buf[(count[0] - loop_count) * total_recvcounts],
                               recvcounts, displs, C_DATATYPE_MPI, comm);
            VRFY((MPI_SUCCESS == mpi_code), "MPI_Allgatherv succeeded");
        }

        VRFY((0 == memcmp(global_buf, data_bufs[dset_idx], data_size)), "Data verification succeeded");
    }

    free(displs);
    free(recvcounts);
    free(global_buf);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        free(read_bufs[dset_idx]);

    for (size_t dset_idx = 0; dset_idx < MAX_NUM_DSETS_MULTI; dset_idx++)
        free(data_bufs_nc[dset_idx]);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        VRFY((H5Sclose(fspace_ids[dset_idx]) >= 0), "File dataspace close succeeded");
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");
    }

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
 */
static void
test_read_filtered_dataset_single_no_selection(const char *parent_group, H5Z_filter_t filter_id,
                                               hid_t fapl_id, hid_t dcpl_id, hid_t dxpl_id,
                                               test_mode_t test_mode)
{
    const void *data_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    void       *data_bufs_nc[MAX_NUM_DSETS_MULTI] = {0}; /* non-const buffer pointers for freeing */
    void       *read_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    void       *global_buf                        = NULL;
    hsize_t     dataset_dims[READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     chunk_dims[READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     sel_dims[READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     start[READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     stride[READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     count[READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     block[READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     flat_dims[1];
    size_t      data_size, read_buf_size;
    size_t      num_dsets;
    hid_t       dset_ids[MAX_NUM_DSETS_MULTI];
    hid_t       fspace_ids[MAX_NUM_DSETS_MULTI];
    hid_t       file_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID;
    hid_t       group_id   = H5I_INVALID_HID;
    hid_t       filespace  = H5I_INVALID_HID;
    int        *recvcounts = NULL;
    int        *displs     = NULL;
    int         mpi_code;

    if (MAINPROCESS)
        puts("Testing read from filtered chunks with a single process having no selection");

    dataset_dims[0] = (hsize_t)READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_NROWS;
    dataset_dims[1] = (hsize_t)READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_NCOLS;

    /* Setup the buffer for writing and for comparison */
    data_size = dataset_dims[0] * dataset_dims[1] * sizeof(C_DATATYPE);

    for (size_t dset_idx = 0; dset_idx < MAX_NUM_DSETS_MULTI; dset_idx++) {
        size_t segment_length;

        C_DATATYPE *tmp_buf = calloc(1, data_size);
        VRFY((NULL != tmp_buf), "calloc succeeded");

        for (size_t j = 0; j < data_size / sizeof(C_DATATYPE); j++)
            tmp_buf[j] =
                (C_DATATYPE)((j % (dataset_dims[0] / (hsize_t)mpi_size * dataset_dims[1])) +
                             (j / (dataset_dims[0] / (hsize_t)mpi_size * dataset_dims[1])) + dset_idx);

        /* Compute the correct offset into the buffer for the process having no selection and clear it */
        segment_length = dataset_dims[0] * dataset_dims[1] / (hsize_t)mpi_size;
        memset(tmp_buf + ((size_t)READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_NO_SELECT_PROC * segment_length),
               0, segment_length * sizeof(C_DATATYPE));

        data_bufs[dset_idx]    = tmp_buf;
        data_bufs_nc[dset_idx] = tmp_buf;
    }

    file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, fapl_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
    VRFY((group_id >= 0), "H5Gopen2 succeeded");

    /* Create the dataspace for the dataset */
    filespace = H5Screate_simple(READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS, dataset_dims, NULL);
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

    /* Create datasets depending on the current test mode */
    create_datasets(group_id, READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_DATASET_NAME, HDF5_DATATYPE_NAME,
                    filespace, plist_id, test_mode, &num_dsets, dset_ids);

    /* Verify space allocation status */
    verify_space_alloc_status(num_dsets, dset_ids, plist_id, DATASET_JUST_CREATED);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");

    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
    VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    MPI_Barrier(comm);

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

        open_datasets(group_id, READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_DATASET_NAME, num_dsets, test_mode,
                      dset_ids);

        select_all(num_dsets, dset_ids, fspace_ids);

        write_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_ALL, fspace_ids, dcpl_id, H5P_DEFAULT,
                       data_bufs, test_mode, true, false, false);

        /* Verify space allocation status */
        plist_id = H5Dget_create_plist(dset_ids[0]);
        VRFY((plist_id >= 0), "H5Dget_create_plist succeeded");
        verify_space_alloc_status(num_dsets, dset_ids, plist_id, ALL_CHUNKS_WRITTEN);

        for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
            VRFY((H5Sclose(fspace_ids[dset_idx]) >= 0), "File dataspace close succeeded");
            VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");
        }

        VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
        VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
        VRFY((H5Fclose(file_id) >= 0), "File close succeeded");
    }

    MPI_Barrier(comm);

    file_id = H5Fopen(filenames[0], H5F_ACC_RDONLY, fapl_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
    VRFY((group_id >= 0), "H5Gopen2 succeeded");

    open_datasets(group_id, READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_DATASET_NAME, num_dsets, test_mode,
                  dset_ids);

    sel_dims[0] = (hsize_t)READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_CH_NROWS;
    sel_dims[1] = (hsize_t)READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_NCOLS;

    if (mpi_rank == READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_NO_SELECT_PROC)
        sel_dims[0] = sel_dims[1] = 0;

    /* Setup one-dimensional memory dataspace for reading the dataset data into a contiguous buffer */
    flat_dims[0] = sel_dims[0] * sel_dims[1];

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

    if (mpi_rank == READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_NO_SELECT_PROC)
        select_none(num_dsets, dset_ids, fspace_ids);
    else
        select_hyperslab(num_dsets, dset_ids, start, stride, count, block, fspace_ids);

    read_buf_size = flat_dims[0] * sizeof(C_DATATYPE);

    if (mpi_rank != READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_NO_SELECT_PROC) {
        for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
            read_bufs[dset_idx] = calloc(1, read_buf_size);
            VRFY((NULL != read_bufs[dset_idx]), "calloc succeeded");
        }
    }

    read_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_BLOCK, fspace_ids[0], dcpl_id, dxpl_id,
                  read_bufs, test_mode, mpi_size > 1 ? true : false, true, false);

    /* Collect each piece of data from all ranks into a global buffer on all ranks */
    global_buf = calloc(1, data_size);
    VRFY((NULL != global_buf), "calloc succeeded");
    recvcounts = calloc(1, (size_t)mpi_size * sizeof(*recvcounts));
    VRFY((NULL != recvcounts), "calloc succeeded");
    displs = calloc(1, (size_t)mpi_size * sizeof(*displs));
    VRFY((NULL != displs), "calloc succeeded");

    for (size_t i = 0; i < (size_t)mpi_size; i++) {
        recvcounts[i] = (int)(READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_CH_NROWS *
                              READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_NCOLS);
        displs[i]     = (int)(i * (size_t)(READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_CH_NROWS *
                                       READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_NCOLS));
    }

    recvcounts[READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_NO_SELECT_PROC] = 0;

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        if (mpi_rank == READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_NO_SELECT_PROC) {
            mpi_code = MPI_Allgatherv(read_bufs[dset_idx], 0, C_DATATYPE_MPI, global_buf, recvcounts, displs,
                                      C_DATATYPE_MPI, comm);
            VRFY((MPI_SUCCESS == mpi_code), "MPI_Allgatherv succeeded");
        }
        else {
            mpi_code = MPI_Allgatherv(read_bufs[dset_idx], (int)flat_dims[0], C_DATATYPE_MPI, global_buf,
                                      recvcounts, displs, C_DATATYPE_MPI, comm);
            VRFY((MPI_SUCCESS == mpi_code), "MPI_Allgatherv succeeded");
        }

        VRFY((0 == memcmp(global_buf, data_bufs[dset_idx], data_size)), "Data verification succeeded");
    }

    free(displs);
    free(recvcounts);
    free(global_buf);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        free(read_bufs[dset_idx]);

    for (size_t dset_idx = 0; dset_idx < MAX_NUM_DSETS_MULTI; dset_idx++)
        free(data_bufs_nc[dset_idx]);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        VRFY((H5Sclose(fspace_ids[dset_idx]) >= 0), "File dataspace close succeeded");
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");
    }

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
 */
static void
test_read_filtered_dataset_all_no_selection(const char *parent_group, H5Z_filter_t filter_id, hid_t fapl_id,
                                            hid_t dcpl_id, hid_t dxpl_id, test_mode_t test_mode)
{
    const void *data_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    void       *data_bufs_nc[MAX_NUM_DSETS_MULTI] = {0}; /* non-const buffer pointers for freeing */
    void       *read_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    hsize_t     dataset_dims[READ_ALL_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     chunk_dims[READ_ALL_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    size_t      data_size, read_buf_size;
    size_t      num_dsets;
    hid_t       dset_ids[MAX_NUM_DSETS_MULTI];
    hid_t       fspace_ids[MAX_NUM_DSETS_MULTI];
    hid_t       file_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID;
    hid_t       group_id  = H5I_INVALID_HID;
    hid_t       filespace = H5I_INVALID_HID;

    if (MAINPROCESS)
        puts("Testing read from filtered chunks with all processes having no selection");

    dataset_dims[0] = (hsize_t)READ_ALL_NO_SELECTION_FILTERED_CHUNKS_NROWS;
    dataset_dims[1] = (hsize_t)READ_ALL_NO_SELECTION_FILTERED_CHUNKS_NCOLS;

    /* Setup the buffer for writing and for comparison */
    data_size = dataset_dims[0] * dataset_dims[1] * sizeof(C_DATATYPE);

    for (size_t dset_idx = 0; dset_idx < MAX_NUM_DSETS_MULTI; dset_idx++) {
        C_DATATYPE *tmp_buf = calloc(1, data_size);
        VRFY((NULL != tmp_buf), "calloc succeeded");

        /* Fill buffer with garbage data before write call */
        memset(tmp_buf, 255, data_size);

        data_bufs[dset_idx]    = tmp_buf;
        data_bufs_nc[dset_idx] = tmp_buf;
    }

    file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, fapl_id);
    VRFY((file_id >= 0), "Test file open succeeded");

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

    /* Create datasets depending on the current test mode */
    create_datasets(group_id, READ_ALL_NO_SELECTION_FILTERED_CHUNKS_DATASET_NAME, HDF5_DATATYPE_NAME,
                    filespace, plist_id, test_mode, &num_dsets, dset_ids);

    /* Verify space allocation status */
    verify_space_alloc_status(num_dsets, dset_ids, plist_id, DATASET_JUST_CREATED);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");

    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
    VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    MPI_Barrier(comm);

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

        open_datasets(group_id, READ_ALL_NO_SELECTION_FILTERED_CHUNKS_DATASET_NAME, num_dsets, test_mode,
                      dset_ids);

        select_all(num_dsets, dset_ids, fspace_ids);

        write_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_ALL, fspace_ids, dcpl_id, H5P_DEFAULT,
                       data_bufs, test_mode, true, false, false);

        /* Verify space allocation status */
        plist_id = H5Dget_create_plist(dset_ids[0]);
        VRFY((plist_id >= 0), "H5Dget_create_plist succeeded");
        verify_space_alloc_status(num_dsets, dset_ids, plist_id, ALL_CHUNKS_WRITTEN);

        for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
            VRFY((H5Sclose(fspace_ids[dset_idx]) >= 0), "File dataspace close succeeded");
            VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");
        }

        VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
        VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
        VRFY((H5Fclose(file_id) >= 0), "File close succeeded");
    }

    MPI_Barrier(comm);

    file_id = H5Fopen(filenames[0], H5F_ACC_RDONLY, fapl_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
    VRFY((group_id >= 0), "H5Gopen2 succeeded");

    open_datasets(group_id, READ_ALL_NO_SELECTION_FILTERED_CHUNKS_DATASET_NAME, num_dsets, test_mode,
                  dset_ids);

    select_none(num_dsets, dset_ids, fspace_ids);

    read_buf_size = dataset_dims[0] * dataset_dims[1] * sizeof(C_DATATYPE);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        read_bufs[dset_idx] = calloc(1, read_buf_size);
        VRFY((NULL != read_bufs[dset_idx]), "calloc succeeded");
    }

    /* Clear data buffer that will be used for comparison since
     * no data should end up being read
     */
    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        memset(data_bufs_nc[dset_idx], 0, data_size);

    read_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_BLOCK, fspace_ids[0], dcpl_id, dxpl_id,
                  read_bufs, test_mode, false, true, false);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        VRFY((0 == memcmp(read_bufs[dset_idx], data_bufs[dset_idx], data_size)),
             "Data verification succeeded");

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        free(read_bufs[dset_idx]);

    for (size_t dset_idx = 0; dset_idx < MAX_NUM_DSETS_MULTI; dset_idx++)
        free(data_bufs_nc[dset_idx]);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        VRFY((H5Sclose(fspace_ids[dset_idx]) >= 0), "File dataspace close succeeded");
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");
    }

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
 */
static void
test_read_filtered_dataset_point_selection(const char *parent_group, H5Z_filter_t filter_id, hid_t fapl_id,
                                           hid_t dcpl_id, hid_t dxpl_id, test_mode_t test_mode)
{
    const void *data_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    void       *data_bufs_nc[MAX_NUM_DSETS_MULTI] = {0}; /* non-const buffer pointers for freeing */
    void       *read_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    void       *global_buf                        = NULL;
    hsize_t    *coords                            = NULL;
    hsize_t     dataset_dims[READ_POINT_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     chunk_dims[READ_POINT_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     sel_dims[READ_POINT_SELECTION_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     flat_dims[1];
    size_t      data_size, read_buf_size;
    size_t      num_points;
    size_t      num_dsets;
    hid_t       dset_ids[MAX_NUM_DSETS_MULTI];
    hid_t       fspace_ids[MAX_NUM_DSETS_MULTI];
    hid_t       file_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID;
    hid_t       group_id   = H5I_INVALID_HID;
    hid_t       filespace  = H5I_INVALID_HID;
    int        *recvcounts = NULL;
    int        *displs     = NULL;
    int         mpi_code;

    if (MAINPROCESS)
        puts("Testing read from filtered chunks with point selection");

    dataset_dims[0] = (hsize_t)READ_POINT_SELECTION_FILTERED_CHUNKS_NROWS;
    dataset_dims[1] = (hsize_t)READ_POINT_SELECTION_FILTERED_CHUNKS_NCOLS;

    /* Setup the buffer for writing and for comparison */
    data_size = dataset_dims[0] * dataset_dims[1] * sizeof(C_DATATYPE);

    for (size_t dset_idx = 0; dset_idx < MAX_NUM_DSETS_MULTI; dset_idx++) {
        C_DATATYPE *tmp_buf = calloc(1, data_size);
        VRFY((NULL != tmp_buf), "calloc succeeded");

        for (size_t j = 0; j < data_size / sizeof(C_DATATYPE); j++)
            tmp_buf[j] = (C_DATATYPE)((dataset_dims[1] * (j / ((hsize_t)mpi_size * dataset_dims[1]))) +
                                      (j % dataset_dims[1]) +
                                      (((j % ((hsize_t)mpi_size * dataset_dims[1])) / dataset_dims[1]) %
                                       dataset_dims[1]) +
                                      dset_idx);

        data_bufs[dset_idx]    = tmp_buf;
        data_bufs_nc[dset_idx] = tmp_buf;
    }

    file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, fapl_id);
    VRFY((file_id >= 0), "Test file open succeeded");

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

    /* Create datasets depending on the current test mode */
    create_datasets(group_id, READ_POINT_SELECTION_FILTERED_CHUNKS_DATASET_NAME, HDF5_DATATYPE_NAME,
                    filespace, plist_id, test_mode, &num_dsets, dset_ids);

    /* Verify space allocation status */
    verify_space_alloc_status(num_dsets, dset_ids, plist_id, DATASET_JUST_CREATED);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");

    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
    VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    MPI_Barrier(comm);

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

        open_datasets(group_id, READ_POINT_SELECTION_FILTERED_CHUNKS_DATASET_NAME, num_dsets, test_mode,
                      dset_ids);

        select_all(num_dsets, dset_ids, fspace_ids);

        write_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_ALL, fspace_ids, dcpl_id, H5P_DEFAULT,
                       data_bufs, test_mode, true, false, false);

        /* Verify space allocation status */
        plist_id = H5Dget_create_plist(dset_ids[0]);
        VRFY((plist_id >= 0), "H5Dget_create_plist succeeded");
        verify_space_alloc_status(num_dsets, dset_ids, plist_id, ALL_CHUNKS_WRITTEN);

        for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
            VRFY((H5Sclose(fspace_ids[dset_idx]) >= 0), "File dataspace close succeeded");
            VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");
        }

        VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
        VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
        VRFY((H5Fclose(file_id) >= 0), "File close succeeded");
    }

    MPI_Barrier(comm);

    file_id = H5Fopen(filenames[0], H5F_ACC_RDONLY, fapl_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
    VRFY((group_id >= 0), "H5Gopen2 succeeded");

    open_datasets(group_id, READ_POINT_SELECTION_FILTERED_CHUNKS_DATASET_NAME, num_dsets, test_mode,
                  dset_ids);

    sel_dims[0] = (hsize_t)READ_POINT_SELECTION_FILTERED_CHUNKS_NROWS / (hsize_t)mpi_size;
    sel_dims[1] = (hsize_t)READ_POINT_SELECTION_FILTERED_CHUNKS_NCOLS;

    /* Setup one-dimensional memory dataspace for reading the dataset data into a contiguous buffer */
    flat_dims[0] = sel_dims[0] * sel_dims[1];

    /* Set up point selection */
    num_points = (hsize_t)READ_POINT_SELECTION_FILTERED_CHUNKS_NROWS *
                 (hsize_t)READ_POINT_SELECTION_FILTERED_CHUNKS_NCOLS / (hsize_t)mpi_size;
    coords = (hsize_t *)calloc(1, 2 * num_points * sizeof(*coords));
    VRFY((NULL != coords), "Coords calloc succeeded");

    for (size_t i = 0; i < num_points; i++)
        for (size_t j = 0; j < READ_POINT_SELECTION_FILTERED_CHUNKS_DATASET_DIMS; j++)
            coords[(i * READ_POINT_SELECTION_FILTERED_CHUNKS_DATASET_DIMS) + j] =
                (j > 0) ? (i % (hsize_t)READ_POINT_SELECTION_FILTERED_CHUNKS_NCOLS)
                        : ((hsize_t)mpi_rank +
                           ((hsize_t)mpi_size * (i / (hsize_t)READ_POINT_SELECTION_FILTERED_CHUNKS_NCOLS)));

    select_elements(num_dsets, dset_ids, num_points, coords, fspace_ids);

    read_buf_size = flat_dims[0] * sizeof(C_DATATYPE);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        read_bufs[dset_idx] = calloc(1, read_buf_size);
        VRFY((NULL != read_bufs[dset_idx]), "calloc succeeded");
    }

    read_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_BLOCK, fspace_ids[0], dcpl_id, dxpl_id,
                  read_bufs, test_mode, true, true, false);

    /* Collect each piece of data from all ranks into a global buffer on all ranks */
    global_buf = calloc(1, data_size);
    VRFY((NULL != global_buf), "calloc succeeded");
    recvcounts = calloc(1, (size_t)mpi_size * sizeof(*recvcounts));
    VRFY((NULL != recvcounts), "calloc succeeded");
    displs = calloc(1, (size_t)mpi_size * sizeof(*displs));
    VRFY((NULL != displs), "calloc succeeded");

    /*
     * Since these chunks are shared, run multiple rounds of MPI_Allgatherv
     * to collect all of the pieces into their appropriate locations. The
     * number of times MPI_Allgatherv is run for each dataset should be equal
     * to the number of chunks in the first dimension of the dataset.
     */
    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        size_t original_loop_count = dataset_dims[0] / (hsize_t)mpi_size;
        size_t total_recvcounts    = 0;

        for (size_t j = 0; j < (size_t)mpi_size; j++) {
            recvcounts[j] = (int)dataset_dims[1];
            total_recvcounts += (size_t)recvcounts[j];

            displs[j] = (int)(j * dataset_dims[1]);
        }

        for (size_t cur_loop_count = original_loop_count; cur_loop_count; cur_loop_count--) {
            C_DATATYPE *tmp_buf      = read_bufs[dset_idx];
            C_DATATYPE *tmp_glob_buf = (C_DATATYPE *)global_buf;

            mpi_code = MPI_Allgatherv(
                &tmp_buf[(original_loop_count - cur_loop_count) * dataset_dims[1]], recvcounts[mpi_rank],
                C_DATATYPE_MPI, &tmp_glob_buf[(original_loop_count - cur_loop_count) * total_recvcounts],
                recvcounts, displs, C_DATATYPE_MPI, comm);
            VRFY((MPI_SUCCESS == mpi_code), "MPI_Allgatherv succeeded");
        }

        VRFY((0 == memcmp(global_buf, data_bufs[dset_idx], data_size)), "Data verification succeeded");
    }

    free(displs);
    free(recvcounts);
    free(global_buf);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        free(read_bufs[dset_idx]);

    for (size_t dset_idx = 0; dset_idx < MAX_NUM_DSETS_MULTI; dset_idx++)
        free(data_bufs_nc[dset_idx]);

    free(coords);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        VRFY((H5Sclose(fspace_ids[dset_idx]) >= 0), "File dataspace close succeeded");
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");
    }

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
 */
static void
test_read_filtered_dataset_interleaved_read(const char *parent_group, H5Z_filter_t filter_id, hid_t fapl_id,
                                            hid_t dcpl_id, hid_t dxpl_id, test_mode_t test_mode)
{
    const void *data_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    void       *data_bufs_nc[MAX_NUM_DSETS_MULTI] = {0}; /* non-const buffer pointers for freeing */
    void       *read_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    void       *global_buf                        = NULL;
    hsize_t     dataset_dims[INTERLEAVED_READ_FILTERED_DATASET_DIMS];
    hsize_t     chunk_dims[INTERLEAVED_READ_FILTERED_DATASET_DIMS];
    hsize_t     sel_dims[INTERLEAVED_READ_FILTERED_DATASET_DIMS];
    hsize_t     start[INTERLEAVED_READ_FILTERED_DATASET_DIMS];
    hsize_t     stride[INTERLEAVED_READ_FILTERED_DATASET_DIMS];
    hsize_t     count[INTERLEAVED_READ_FILTERED_DATASET_DIMS];
    hsize_t     block[INTERLEAVED_READ_FILTERED_DATASET_DIMS];
    hsize_t     flat_dims[1];
    size_t      data_size, read_buf_size;
    size_t      num_dsets;
    hid_t       dset_ids[MAX_NUM_DSETS_MULTI];
    hid_t       fspace_ids[MAX_NUM_DSETS_MULTI];
    hid_t       file_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID;
    hid_t       group_id   = H5I_INVALID_HID;
    hid_t       filespace  = H5I_INVALID_HID;
    int        *recvcounts = NULL;
    int        *displs     = NULL;
    int         mpi_code;

    if (MAINPROCESS)
        puts("Testing interleaved read from filtered chunks");

    dataset_dims[0] = (hsize_t)INTERLEAVED_READ_FILTERED_DATASET_NROWS;
    dataset_dims[1] = (hsize_t)INTERLEAVED_READ_FILTERED_DATASET_NCOLS;

    /* Setup the buffer for writing and for comparison */
    data_size = dataset_dims[0] * dataset_dims[1] * sizeof(C_DATATYPE);

    for (size_t dset_idx = 0; dset_idx < MAX_NUM_DSETS_MULTI; dset_idx++) {
        C_DATATYPE *tmp_buf = calloc(1, data_size);
        VRFY((NULL != tmp_buf), "calloc succeeded");

        for (size_t j = 0; j < data_size / sizeof(C_DATATYPE); j++)
            tmp_buf[j] =
                /* Add the Column Index */
                (C_DATATYPE)((j % (hsize_t)INTERLEAVED_READ_FILTERED_DATASET_NCOLS)

                             /* Add the Row Index */
                             + ((j % (hsize_t)(mpi_size * INTERLEAVED_READ_FILTERED_DATASET_NCOLS)) /
                                (hsize_t)INTERLEAVED_READ_FILTERED_DATASET_NCOLS)

                             /* Add the amount that gets added when a rank moves down to its next section
                                vertically in the dataset */
                             + ((hsize_t)INTERLEAVED_READ_FILTERED_DATASET_NCOLS *
                                (j / (hsize_t)(mpi_size * INTERLEAVED_READ_FILTERED_DATASET_NCOLS)))

                             /* Add an increment factor for the multi-dataset case */
                             + dset_idx);

        data_bufs[dset_idx]    = tmp_buf;
        data_bufs_nc[dset_idx] = tmp_buf;
    }

    file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, fapl_id);
    VRFY((file_id >= 0), "Test file open succeeded");

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

    VRFY((H5Pset_chunk(plist_id, INTERLEAVED_READ_FILTERED_DATASET_DIMS, chunk_dims) >= 0), "Chunk size set");

    /* Add test filter to the pipeline */
    VRFY((set_dcpl_filter(plist_id, filter_id, NULL) >= 0), "Filter set");

    /* Create datasets depending on the current test mode */
    create_datasets(group_id, INTERLEAVED_READ_FILTERED_DATASET_NAME, HDF5_DATATYPE_NAME, filespace, plist_id,
                    test_mode, &num_dsets, dset_ids);

    /* Verify space allocation status */
    verify_space_alloc_status(num_dsets, dset_ids, plist_id, DATASET_JUST_CREATED);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");

    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
    VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    MPI_Barrier(comm);

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

        open_datasets(group_id, INTERLEAVED_READ_FILTERED_DATASET_NAME, num_dsets, test_mode, dset_ids);

        select_all(num_dsets, dset_ids, fspace_ids);

        write_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_ALL, fspace_ids, dcpl_id, H5P_DEFAULT,
                       data_bufs, test_mode, true, false, false);

        /* Verify space allocation status */
        plist_id = H5Dget_create_plist(dset_ids[0]);
        VRFY((plist_id >= 0), "H5Dget_create_plist succeeded");
        verify_space_alloc_status(num_dsets, dset_ids, plist_id, ALL_CHUNKS_WRITTEN);

        for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
            VRFY((H5Sclose(fspace_ids[dset_idx]) >= 0), "File dataspace close succeeded");
            VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");
        }

        VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
        VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
        VRFY((H5Fclose(file_id) >= 0), "File close succeeded");
    }

    MPI_Barrier(comm);

    file_id = H5Fopen(filenames[0], H5F_ACC_RDONLY, fapl_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
    VRFY((group_id >= 0), "H5Gopen2 succeeded");

    open_datasets(group_id, INTERLEAVED_READ_FILTERED_DATASET_NAME, num_dsets, test_mode, dset_ids);

    sel_dims[0] = (hsize_t)(INTERLEAVED_READ_FILTERED_DATASET_NROWS / mpi_size);
    sel_dims[1] = (hsize_t)INTERLEAVED_READ_FILTERED_DATASET_NCOLS;

    /* Setup one-dimensional memory dataspace for reading the dataset data into a contiguous buffer */
    flat_dims[0] = sel_dims[0] * sel_dims[1];

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

    select_hyperslab(num_dsets, dset_ids, start, stride, count, block, fspace_ids);

    read_buf_size = flat_dims[0] * sizeof(C_DATATYPE);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        read_bufs[dset_idx] = calloc(1, read_buf_size);
        VRFY((NULL != read_bufs[dset_idx]), "calloc succeeded");
    }

    read_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_BLOCK, fspace_ids[0], dcpl_id, dxpl_id,
                  read_bufs, test_mode, true, true, false);

    /* Collect each piece of data from all ranks into a global buffer on all ranks */
    global_buf = calloc(1, data_size);
    VRFY((NULL != global_buf), "calloc succeeded");
    recvcounts = calloc(1, (size_t)mpi_size * sizeof(*recvcounts));
    VRFY((NULL != recvcounts), "calloc succeeded");
    displs = calloc(1, (size_t)mpi_size * sizeof(*displs));
    VRFY((NULL != displs), "calloc succeeded");

    /*
     * Since these chunks are shared, run multiple rounds of MPI_Allgatherv
     * to collect all of the pieces into their appropriate locations. The
     * number of times MPI_Allgatherv is run should be equal to the number
     * of chunks in the first dimension of the dataset.
     */
    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        size_t total_recvcounts = 0;

        for (size_t j = 0; j < (size_t)mpi_size; j++) {
            recvcounts[j] = (int)dataset_dims[1];
            total_recvcounts += (size_t)recvcounts[j];

            displs[j] = (int)(j * dataset_dims[1]);
        }

        for (size_t loop_count = count[0]; loop_count; loop_count--) {
            C_DATATYPE *tmp_buf      = read_bufs[dset_idx];
            C_DATATYPE *tmp_glob_buf = (C_DATATYPE *)global_buf;

            mpi_code =
                MPI_Allgatherv(&tmp_buf[(count[0] - loop_count) * dataset_dims[1]], recvcounts[mpi_rank],
                               C_DATATYPE_MPI, &tmp_glob_buf[(count[0] - loop_count) * total_recvcounts],
                               recvcounts, displs, C_DATATYPE_MPI, comm);
            VRFY((MPI_SUCCESS == mpi_code), "MPI_Allgatherv succeeded");
        }

        VRFY((0 == memcmp(global_buf, data_bufs[dset_idx], data_size)), "Data verification succeeded");
    }

    free(displs);
    free(recvcounts);
    free(global_buf);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        free(read_bufs[dset_idx]);

    for (size_t dset_idx = 0; dset_idx < MAX_NUM_DSETS_MULTI; dset_idx++)
        free(data_bufs_nc[dset_idx]);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        VRFY((H5Sclose(fspace_ids[dset_idx]) >= 0), "File dataspace close succeeded");
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");
    }

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
 */
static void
test_read_3d_filtered_dataset_no_overlap_separate_pages(const char *parent_group, H5Z_filter_t filter_id,
                                                        hid_t fapl_id, hid_t dcpl_id, hid_t dxpl_id,
                                                        test_mode_t test_mode)
{
    MPI_Datatype vector_type;
    MPI_Datatype resized_vector_type;
    const void  *data_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    void        *data_bufs_nc[MAX_NUM_DSETS_MULTI] = {0}; /* non-const buffer pointers for freeing */
    void        *read_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    void        *global_buf                        = NULL;
    hsize_t      dataset_dims[READ_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_DIMS];
    hsize_t      chunk_dims[READ_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_DIMS];
    hsize_t      sel_dims[READ_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_DIMS];
    hsize_t      start[READ_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_DIMS];
    hsize_t      stride[READ_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_DIMS];
    hsize_t      count[READ_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_DIMS];
    hsize_t      block[READ_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_DIMS];
    hsize_t      flat_dims[1];
    size_t       data_size, read_buf_size;
    size_t       num_dsets;
    hid_t        dset_ids[MAX_NUM_DSETS_MULTI];
    hid_t        fspace_ids[MAX_NUM_DSETS_MULTI];
    hid_t        file_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID;
    hid_t        group_id  = H5I_INVALID_HID;
    hid_t        filespace = H5I_INVALID_HID;
    int          mpi_code;

    if (MAINPROCESS)
        puts("Testing read from unshared filtered chunks on separate pages in 3D dataset");

    dataset_dims[0] = (hsize_t)READ_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_NROWS;
    dataset_dims[1] = (hsize_t)READ_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_NCOLS;
    dataset_dims[2] = (hsize_t)READ_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DEPTH;

    /* Setup the buffer for writing and for comparison */
    data_size = dataset_dims[0] * dataset_dims[1] * dataset_dims[2] * sizeof(C_DATATYPE);

    for (size_t dset_idx = 0; dset_idx < MAX_NUM_DSETS_MULTI; dset_idx++) {
        C_DATATYPE *tmp_buf = calloc(1, data_size);
        VRFY((NULL != tmp_buf), "calloc succeeded");

        for (size_t j = 0; j < data_size / sizeof(C_DATATYPE); j++)
            tmp_buf[j] = (C_DATATYPE)((j % (hsize_t)mpi_size) + (j / (hsize_t)mpi_size) + dset_idx);

        data_bufs[dset_idx]    = tmp_buf;
        data_bufs_nc[dset_idx] = tmp_buf;
    }

    file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, fapl_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
    VRFY((group_id >= 0), "H5Gopen2 succeeded");

    /* Create the dataspace for the dataset */
    filespace = H5Screate_simple(READ_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_DIMS, dataset_dims, NULL);
    VRFY((filespace >= 0), "File dataspace creation succeeded");

    /* Create chunked dataset */
    chunk_dims[0] = (hsize_t)READ_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_CH_NROWS;
    chunk_dims[1] = (hsize_t)READ_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_CH_NCOLS;
    chunk_dims[2] = 1;

    plist_id = H5Pcopy(dcpl_id);
    VRFY((plist_id >= 0), "DCPL copy succeeded");

    VRFY((H5Pset_chunk(plist_id, READ_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_DIMS, chunk_dims) >= 0),
         "Chunk size set");

    /* Add test filter to the pipeline */
    VRFY((set_dcpl_filter(plist_id, filter_id, NULL) >= 0), "Filter set");

    /* Create datasets depending on the current test mode */
    create_datasets(group_id, READ_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_NAME, HDF5_DATATYPE_NAME,
                    filespace, plist_id, test_mode, &num_dsets, dset_ids);

    /* Verify space allocation status */
    verify_space_alloc_status(num_dsets, dset_ids, plist_id, DATASET_JUST_CREATED);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");

    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
    VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    MPI_Barrier(comm);

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

        open_datasets(group_id, READ_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_NAME, num_dsets, test_mode,
                      dset_ids);

        select_all(num_dsets, dset_ids, fspace_ids);

        write_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_ALL, fspace_ids, dcpl_id, H5P_DEFAULT,
                       data_bufs, test_mode, true, false, false);

        /* Verify space allocation status */
        plist_id = H5Dget_create_plist(dset_ids[0]);
        VRFY((plist_id >= 0), "H5Dget_create_plist succeeded");
        verify_space_alloc_status(num_dsets, dset_ids, plist_id, ALL_CHUNKS_WRITTEN);

        for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
            VRFY((H5Sclose(fspace_ids[dset_idx]) >= 0), "File dataspace close succeeded");
            VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");
        }

        VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
        VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
        VRFY((H5Fclose(file_id) >= 0), "File close succeeded");
    }

    MPI_Barrier(comm);

    file_id = H5Fopen(filenames[0], H5F_ACC_RDONLY, fapl_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
    VRFY((group_id >= 0), "H5Gopen2 succeeded");

    open_datasets(group_id, READ_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_NAME, num_dsets, test_mode,
                  dset_ids);

    sel_dims[0] = (hsize_t)READ_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_NROWS;
    sel_dims[1] = (hsize_t)READ_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_NCOLS;
    sel_dims[2] = 1;

    /* Setup one-dimensional memory dataspace for reading the dataset data into a contiguous buffer */
    flat_dims[0] = sel_dims[0] * sel_dims[1] * sel_dims[2];

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

    select_hyperslab(num_dsets, dset_ids, start, stride, count, block, fspace_ids);

    read_buf_size = flat_dims[0] * sizeof(C_DATATYPE);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        read_bufs[dset_idx] = calloc(1, read_buf_size);
        VRFY((NULL != read_bufs[dset_idx]), "calloc succeeded");
    }

    read_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_BLOCK, fspace_ids[0], dcpl_id, dxpl_id,
                  read_bufs, test_mode, true, true, false);

    /* Collect each piece of data from all ranks into a global buffer on all ranks */
    global_buf = calloc(1, data_size);
    VRFY((NULL != global_buf), "calloc succeeded");

    /*
     * Due to the nature of 3-dimensional reading, create an MPI vector type that allows each
     * rank to write to the nth position of the global data buffer, where n is the rank number.
     */
    mpi_code = MPI_Type_vector((int)flat_dims[0], 1, mpi_size, C_DATATYPE_MPI, &vector_type);
    VRFY((MPI_SUCCESS == mpi_code), "MPI_Type_vector succeeded");
    mpi_code = MPI_Type_commit(&vector_type);
    VRFY((MPI_SUCCESS == mpi_code), "MPI_Type_commit succeeded");

    /*
     * Resize the type to allow interleaving,
     * so make it only one MPI_LONG wide
     */
    mpi_code = MPI_Type_create_resized(vector_type, 0, sizeof(long), &resized_vector_type);
    VRFY((MPI_SUCCESS == mpi_code), "MPI_Type_create_resized");
    mpi_code = MPI_Type_commit(&resized_vector_type);
    VRFY((MPI_SUCCESS == mpi_code), "MPI_Type_commit succeeded");

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        mpi_code = MPI_Allgather(read_bufs[dset_idx], (int)flat_dims[0], C_DATATYPE_MPI, global_buf, 1,
                                 resized_vector_type, comm);
        VRFY((MPI_SUCCESS == mpi_code), "MPI_Allgather succeeded");

        VRFY((0 == memcmp(global_buf, data_bufs[dset_idx], data_size)), "Data verification succeeded");
    }

    mpi_code = MPI_Type_free(&vector_type);
    VRFY((MPI_SUCCESS == mpi_code), "MPI_Type_free succeeded");
    mpi_code = MPI_Type_free(&resized_vector_type);
    VRFY((MPI_SUCCESS == mpi_code), "MPI_Type_free succeeded");

    free(global_buf);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        free(read_bufs[dset_idx]);

    for (size_t dset_idx = 0; dset_idx < MAX_NUM_DSETS_MULTI; dset_idx++)
        free(data_bufs_nc[dset_idx]);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        VRFY((H5Sclose(fspace_ids[dset_idx]) >= 0), "File dataspace close succeeded");
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");
    }

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
 */
static void
test_read_transformed_filtered_dataset_no_overlap(const char *parent_group, H5Z_filter_t filter_id,
                                                  hid_t fapl_id, hid_t dcpl_id, hid_t dxpl_id,
                                                  test_mode_t test_mode)
{
    const void *data_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    void       *data_bufs_nc[MAX_NUM_DSETS_MULTI] = {0}; /* non-const buffer pointers for freeing */
    void       *read_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    void       *global_buf                        = NULL;
    hsize_t     dataset_dims[READ_UNSHARED_TRANSFORMED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     chunk_dims[READ_UNSHARED_TRANSFORMED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     sel_dims[READ_UNSHARED_TRANSFORMED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     start[READ_UNSHARED_TRANSFORMED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     stride[READ_UNSHARED_TRANSFORMED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     count[READ_UNSHARED_TRANSFORMED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     block[READ_UNSHARED_TRANSFORMED_FILTERED_CHUNKS_DATASET_DIMS];
    hsize_t     flat_dims[1];
    size_t      data_size, read_buf_size;
    size_t      num_dsets;
    hid_t       dset_ids[MAX_NUM_DSETS_MULTI];
    hid_t       fspace_ids[MAX_NUM_DSETS_MULTI];
    hid_t       file_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID;
    hid_t       group_id   = H5I_INVALID_HID;
    hid_t       filespace  = H5I_INVALID_HID;
    int        *recvcounts = NULL;
    int        *displs     = NULL;
    int         mpi_code;

    if (MAINPROCESS)
        puts("Testing read from unshared transformed and filtered chunks");

    dataset_dims[0] = (hsize_t)READ_UNSHARED_TRANSFORMED_FILTERED_CHUNKS_NROWS;
    dataset_dims[1] = (hsize_t)READ_UNSHARED_TRANSFORMED_FILTERED_CHUNKS_NCOLS;

    /* Setup the buffer for writing and for comparison */
    data_size = (hsize_t)READ_UNSHARED_TRANSFORMED_FILTERED_CHUNKS_NROWS *
                (hsize_t)READ_UNSHARED_TRANSFORMED_FILTERED_CHUNKS_NCOLS * sizeof(C_DATATYPE);

    for (size_t dset_idx = 0; dset_idx < MAX_NUM_DSETS_MULTI; dset_idx++) {
        C_DATATYPE *tmp_buf = calloc(1, data_size);
        VRFY((NULL != tmp_buf), "calloc succeeded");

        for (size_t j = 0; j < data_size / sizeof(C_DATATYPE); j++)
            tmp_buf[j] =
                (C_DATATYPE)((j % (dataset_dims[0] / (hsize_t)mpi_size * dataset_dims[1])) +
                             (j / (dataset_dims[0] / (hsize_t)mpi_size * dataset_dims[1])) + dset_idx);

        data_bufs[dset_idx]    = tmp_buf;
        data_bufs_nc[dset_idx] = tmp_buf;
    }

    file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, fapl_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
    VRFY((group_id >= 0), "H5Gopen2 succeeded");

    /* Create the dataspace for the dataset */
    filespace = H5Screate_simple(READ_UNSHARED_TRANSFORMED_FILTERED_CHUNKS_DATASET_DIMS, dataset_dims, NULL);
    VRFY((filespace >= 0), "File dataspace creation succeeded");

    /* Create chunked dataset */
    chunk_dims[0] = (hsize_t)READ_UNSHARED_TRANSFORMED_FILTERED_CHUNKS_CH_NROWS;
    chunk_dims[1] = (hsize_t)READ_UNSHARED_TRANSFORMED_FILTERED_CHUNKS_CH_NCOLS;

    plist_id = H5Pcopy(dcpl_id);
    VRFY((plist_id >= 0), "DCPL copy succeeded");

    VRFY((H5Pset_chunk(plist_id, READ_UNSHARED_TRANSFORMED_FILTERED_CHUNKS_DATASET_DIMS, chunk_dims) >= 0),
         "Chunk size set");

    /* Add test filter to the pipeline */
    VRFY((set_dcpl_filter(plist_id, filter_id, NULL) >= 0), "Filter set");

    /* Create datasets depending on the current test mode */
    create_datasets(group_id, READ_UNSHARED_TRANSFORMED_FILTERED_CHUNKS_DATASET_NAME, HDF5_DATATYPE_NAME,
                    filespace, plist_id, test_mode, &num_dsets, dset_ids);

    /* Verify space allocation status */
    verify_space_alloc_status(num_dsets, dset_ids, plist_id, DATASET_JUST_CREATED);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");

    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
    VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    MPI_Barrier(comm);

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

        open_datasets(group_id, READ_UNSHARED_TRANSFORMED_FILTERED_CHUNKS_DATASET_NAME, num_dsets, test_mode,
                      dset_ids);

        /* Create property list for collective dataset read */
        plist_id = H5Pcreate(H5P_DATASET_XFER);
        VRFY((plist_id >= 0), "DXPL creation succeeded");

        /* Set data transform expression */
        VRFY((H5Pset_data_transform(plist_id, "x") >= 0), "Set data transform expression succeeded");

        select_all(num_dsets, dset_ids, fspace_ids);

        write_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_ALL, fspace_ids, dcpl_id, H5P_DEFAULT,
                       data_bufs, test_mode, true, false, false);

        VRFY((H5Pclose(plist_id) >= 0), "DXPL close succeeded");

        /* Verify space allocation status */
        plist_id = H5Dget_create_plist(dset_ids[0]);
        VRFY((plist_id >= 0), "H5Dget_create_plist succeeded");
        verify_space_alloc_status(num_dsets, dset_ids, plist_id, ALL_CHUNKS_WRITTEN);

        for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
            VRFY((H5Sclose(fspace_ids[dset_idx]) >= 0), "File dataspace close succeeded");
            VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");
        }

        VRFY((H5Pclose(plist_id) >= 0), "DXPL close succeeded");
        VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
        VRFY((H5Fclose(file_id) >= 0), "File close succeeded");
    }

    MPI_Barrier(comm);

    file_id = H5Fopen(filenames[0], H5F_ACC_RDONLY, fapl_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
    VRFY((group_id >= 0), "H5Gopen2 succeeded");

    open_datasets(group_id, READ_UNSHARED_TRANSFORMED_FILTERED_CHUNKS_DATASET_NAME, num_dsets, test_mode,
                  dset_ids);

    sel_dims[0] = (hsize_t)READ_UNSHARED_TRANSFORMED_FILTERED_CHUNKS_CH_NROWS;
    sel_dims[1] = (hsize_t)READ_UNSHARED_TRANSFORMED_FILTERED_CHUNKS_NCOLS;

    /* Setup one-dimensional memory dataspace for reading the dataset data into a contiguous buffer */
    flat_dims[0] = sel_dims[0] * sel_dims[1];

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

    select_hyperslab(num_dsets, dset_ids, start, stride, count, block, fspace_ids);

    /* Create property list for data transform */
    plist_id = H5Pcopy(dxpl_id);
    VRFY((plist_id >= 0), "DXPL copy succeeded");

    /* Set data transform expression */
    VRFY((H5Pset_data_transform(plist_id, "x") >= 0), "Set data transform expression succeeded");

    read_buf_size = flat_dims[0] * sizeof(C_DATATYPE);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        read_bufs[dset_idx] = calloc(1, read_buf_size);
        VRFY((NULL != read_bufs[dset_idx]), "calloc succeeded");
    }

    read_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_BLOCK, fspace_ids[0], dcpl_id, dxpl_id,
                  read_bufs, test_mode, true, true, false);

    /* Collect each piece of data from all ranks into a global buffer on all ranks */
    global_buf = calloc(1, data_size);
    VRFY((NULL != global_buf), "calloc succeeded");
    recvcounts = calloc(1, (size_t)mpi_size * sizeof(*recvcounts));
    VRFY((NULL != recvcounts), "calloc succeeded");
    displs = calloc(1, (size_t)mpi_size * sizeof(*displs));
    VRFY((NULL != displs), "calloc succeeded");

    for (size_t i = 0; i < (size_t)mpi_size; i++) {
        recvcounts[i] = (int)flat_dims[0];
        displs[i]     = (int)(i * flat_dims[0]);
    }

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        mpi_code = MPI_Allgatherv(read_bufs[dset_idx], (int)flat_dims[0], C_DATATYPE_MPI, global_buf,
                                  recvcounts, displs, C_DATATYPE_MPI, comm);
        VRFY((MPI_SUCCESS == mpi_code), "MPI_Allgatherv succeeded");

        VRFY((0 == memcmp(global_buf, data_bufs[dset_idx], data_size)), "Data verification succeeded");
    }

    free(displs);
    free(recvcounts);
    free(global_buf);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        free(read_bufs[dset_idx]);

    for (size_t dset_idx = 0; dset_idx < MAX_NUM_DSETS_MULTI; dset_idx++)
        free(data_bufs_nc[dset_idx]);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        VRFY((H5Sclose(fspace_ids[dset_idx]) >= 0), "File dataspace close succeeded");
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");
    }

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
 */
static void
test_read_3d_filtered_dataset_no_overlap_same_pages(const char *parent_group, H5Z_filter_t filter_id,
                                                    hid_t fapl_id, hid_t dcpl_id, hid_t dxpl_id,
                                                    test_mode_t test_mode)
{
    const void *data_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    void       *data_bufs_nc[MAX_NUM_DSETS_MULTI] = {0}; /* non-const buffer pointers for freeing */
    void       *read_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    void       *global_buf                        = NULL;
    hsize_t     dataset_dims[READ_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_DIMS];
    hsize_t     chunk_dims[READ_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_DIMS];
    hsize_t     sel_dims[READ_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_DIMS];
    hsize_t     start[READ_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_DIMS];
    hsize_t     stride[READ_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_DIMS];
    hsize_t     count[READ_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_DIMS];
    hsize_t     block[READ_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_DIMS];
    hsize_t     flat_dims[1];
    size_t      data_size, read_buf_size;
    size_t      num_dsets;
    hid_t       dset_ids[MAX_NUM_DSETS_MULTI];
    hid_t       fspace_ids[MAX_NUM_DSETS_MULTI];
    hid_t       file_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID;
    hid_t       group_id   = H5I_INVALID_HID;
    hid_t       filespace  = H5I_INVALID_HID;
    int        *recvcounts = NULL;
    int        *displs     = NULL;
    int         mpi_code;

    if (MAINPROCESS)
        puts("Testing read from unshared filtered chunks on the same pages in 3D dataset");

    dataset_dims[0] = (hsize_t)READ_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_NROWS;
    dataset_dims[1] = (hsize_t)READ_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_NCOLS;
    dataset_dims[2] = (hsize_t)READ_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DEPTH;

    /* Setup the buffer for writing and for comparison */
    data_size = dataset_dims[0] * dataset_dims[1] * dataset_dims[2] * sizeof(C_DATATYPE);

    for (size_t dset_idx = 0; dset_idx < MAX_NUM_DSETS_MULTI; dset_idx++) {
        C_DATATYPE *tmp_buf = calloc(1, data_size);
        VRFY((NULL != tmp_buf), "calloc succeeded");

        for (size_t j = 0; j < data_size / sizeof(C_DATATYPE); j++)
            tmp_buf[j] = (C_DATATYPE)((j % (dataset_dims[0] * dataset_dims[1])) +
                                      (j / (dataset_dims[0] * dataset_dims[1])) + dset_idx);

        data_bufs[dset_idx]    = tmp_buf;
        data_bufs_nc[dset_idx] = tmp_buf;
    }

    file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, fapl_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
    VRFY((group_id >= 0), "H5Gopen2 succeeded");

    /* Create the dataspace for the dataset */
    filespace = H5Screate_simple(READ_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_DIMS, dataset_dims, NULL);
    VRFY((filespace >= 0), "File dataspace creation succeeded");

    /* Create chunked dataset */
    chunk_dims[0] = (hsize_t)READ_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_CH_NROWS;
    chunk_dims[1] = (hsize_t)READ_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_CH_NCOLS;
    chunk_dims[2] = 1;

    plist_id = H5Pcopy(dcpl_id);
    VRFY((plist_id >= 0), "DCPL copy succeeded");

    VRFY((H5Pset_chunk(plist_id, READ_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_DIMS, chunk_dims) >= 0),
         "Chunk size set");

    /* Add test filter to the pipeline */
    VRFY((set_dcpl_filter(plist_id, filter_id, NULL) >= 0), "Filter set");

    /* Create datasets depending on the current test mode */
    create_datasets(group_id, READ_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_NAME, HDF5_DATATYPE_NAME,
                    filespace, plist_id, test_mode, &num_dsets, dset_ids);

    /* Verify space allocation status */
    verify_space_alloc_status(num_dsets, dset_ids, plist_id, DATASET_JUST_CREATED);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");

    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
    VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    MPI_Barrier(comm);

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

        open_datasets(group_id, READ_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_NAME, num_dsets, test_mode,
                      dset_ids);

        select_all(num_dsets, dset_ids, fspace_ids);

        write_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_ALL, fspace_ids, dcpl_id, H5P_DEFAULT,
                       data_bufs, test_mode, true, false, false);

        /* Verify space allocation status */
        plist_id = H5Dget_create_plist(dset_ids[0]);
        VRFY((plist_id >= 0), "H5Dget_create_plist succeeded");
        verify_space_alloc_status(num_dsets, dset_ids, plist_id, ALL_CHUNKS_WRITTEN);

        for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
            VRFY((H5Sclose(fspace_ids[dset_idx]) >= 0), "File dataspace close succeeded");
            VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");
        }

        VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
        VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
        VRFY((H5Fclose(file_id) >= 0), "File close succeeded");
    }

    MPI_Barrier(comm);

    file_id = H5Fopen(filenames[0], H5F_ACC_RDONLY, fapl_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
    VRFY((group_id >= 0), "H5Gopen2 succeeded");

    open_datasets(group_id, READ_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_NAME, num_dsets, test_mode,
                  dset_ids);

    sel_dims[0] = (hsize_t)READ_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_CH_NROWS;
    sel_dims[1] = (hsize_t)READ_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_NCOLS;
    sel_dims[2] = (hsize_t)READ_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DEPTH;

    /* Setup one-dimensional memory dataspace for reading the dataset data into a contiguous buffer */
    flat_dims[0] = sel_dims[0] * sel_dims[1] * sel_dims[2];

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

    select_hyperslab(num_dsets, dset_ids, start, stride, count, block, fspace_ids);

    read_buf_size = flat_dims[0] * sizeof(C_DATATYPE);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        read_bufs[dset_idx] = calloc(1, read_buf_size);
        VRFY((NULL != read_bufs[dset_idx]), "calloc succeeded");
    }

    read_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_BLOCK, fspace_ids[0], dcpl_id, dxpl_id,
                  read_bufs, test_mode, true, true, false);

    /* Collect each piece of data from all ranks into a global buffer on all ranks */
    global_buf = calloc(1, data_size);
    VRFY((NULL != global_buf), "calloc succeeded");
    recvcounts = calloc(1, (size_t)mpi_size * sizeof(*recvcounts));
    VRFY((NULL != recvcounts), "calloc succeeded");
    displs = calloc(1, (size_t)mpi_size * sizeof(*displs));
    VRFY((NULL != displs), "calloc succeeded");

    for (size_t i = 0; i < (size_t)mpi_size; i++) {
        recvcounts[i] = (int)flat_dims[0];
        displs[i]     = (int)(i * flat_dims[0]);
    }

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        mpi_code = MPI_Allgatherv(read_bufs[dset_idx], (int)flat_dims[0], C_DATATYPE_MPI, global_buf,
                                  recvcounts, displs, C_DATATYPE_MPI, comm);
        VRFY((MPI_SUCCESS == mpi_code), "MPI_Allgatherv succeeded");

        VRFY((0 == memcmp(global_buf, data_bufs[dset_idx], data_size)), "Data verification succeeded");
    }

    free(displs);
    free(recvcounts);
    free(global_buf);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        free(read_bufs[dset_idx]);

    for (size_t dset_idx = 0; dset_idx < MAX_NUM_DSETS_MULTI; dset_idx++)
        free(data_bufs_nc[dset_idx]);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        VRFY((H5Sclose(fspace_ids[dset_idx]) >= 0), "File dataspace close succeeded");
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");
    }

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
 */
static void
test_read_3d_filtered_dataset_overlap(const char *parent_group, H5Z_filter_t filter_id, hid_t fapl_id,
                                      hid_t dcpl_id, hid_t dxpl_id, test_mode_t test_mode)
{
    MPI_Datatype vector_type;
    MPI_Datatype resized_vector_type;
    const void  *data_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    void        *data_bufs_nc[MAX_NUM_DSETS_MULTI] = {0}; /* non-const buffer pointers for freeing */
    void        *read_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    void        *global_buf                        = NULL;
    hsize_t      dataset_dims[READ_SHARED_FILTERED_CHUNKS_3D_DATASET_DIMS];
    hsize_t      chunk_dims[READ_SHARED_FILTERED_CHUNKS_3D_DATASET_DIMS];
    hsize_t      sel_dims[READ_SHARED_FILTERED_CHUNKS_3D_DATASET_DIMS];
    hsize_t      start[READ_SHARED_FILTERED_CHUNKS_3D_DATASET_DIMS];
    hsize_t      stride[READ_SHARED_FILTERED_CHUNKS_3D_DATASET_DIMS];
    hsize_t      count[READ_SHARED_FILTERED_CHUNKS_3D_DATASET_DIMS];
    hsize_t      block[READ_SHARED_FILTERED_CHUNKS_3D_DATASET_DIMS];
    hsize_t      flat_dims[1];
    size_t       data_size, read_buf_size;
    size_t       num_dsets;
    hid_t        dset_ids[MAX_NUM_DSETS_MULTI];
    hid_t        fspace_ids[MAX_NUM_DSETS_MULTI];
    hid_t        file_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID;
    hid_t        group_id  = H5I_INVALID_HID;
    hid_t        filespace = H5I_INVALID_HID;
    int          mpi_code;

    if (MAINPROCESS)
        puts("Testing read from shared filtered chunks in 3D dataset");

    dataset_dims[0] = (hsize_t)READ_SHARED_FILTERED_CHUNKS_3D_NROWS;
    dataset_dims[1] = (hsize_t)READ_SHARED_FILTERED_CHUNKS_3D_NCOLS;
    dataset_dims[2] = (hsize_t)READ_SHARED_FILTERED_CHUNKS_3D_DEPTH;

    /* Setup the buffer for writing and for comparison */
    data_size = dataset_dims[0] * dataset_dims[1] * dataset_dims[2] * sizeof(C_DATATYPE);

    for (size_t dset_idx = 0; dset_idx < MAX_NUM_DSETS_MULTI; dset_idx++) {
        C_DATATYPE *tmp_buf = calloc(1, data_size);
        VRFY((NULL != tmp_buf), "calloc succeeded");

        for (size_t j = 0; j < data_size / sizeof(C_DATATYPE); j++)
            tmp_buf[j] =
                /* Add the Column Index */
                (C_DATATYPE)((j % (hsize_t)(READ_SHARED_FILTERED_CHUNKS_3D_DEPTH *
                                            READ_SHARED_FILTERED_CHUNKS_3D_NCOLS))

                             /* Add the Row Index */
                             + ((j % (hsize_t)(mpi_size * READ_SHARED_FILTERED_CHUNKS_3D_DEPTH *
                                               READ_SHARED_FILTERED_CHUNKS_3D_NCOLS)) /
                                (hsize_t)(READ_SHARED_FILTERED_CHUNKS_3D_DEPTH *
                                          READ_SHARED_FILTERED_CHUNKS_3D_NCOLS))

                             /* Add the amount that gets added when a rank moves down to its next
                                section vertically in the dataset */
                             + ((hsize_t)(READ_SHARED_FILTERED_CHUNKS_3D_DEPTH *
                                          READ_SHARED_FILTERED_CHUNKS_3D_NCOLS) *
                                (j / (hsize_t)(mpi_size * READ_SHARED_FILTERED_CHUNKS_3D_DEPTH *
                                               READ_SHARED_FILTERED_CHUNKS_3D_NCOLS)))

                             /* Add an increment factor for the multi-dataset case */
                             + dset_idx);

        data_bufs[dset_idx]    = tmp_buf;
        data_bufs_nc[dset_idx] = tmp_buf;
    }

    file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, fapl_id);
    VRFY((file_id >= 0), "Test file open succeeded");

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

    /* Create datasets depending on the current test mode */
    create_datasets(group_id, READ_SHARED_FILTERED_CHUNKS_3D_DATASET_NAME, HDF5_DATATYPE_NAME, filespace,
                    plist_id, test_mode, &num_dsets, dset_ids);

    /* Verify space allocation status */
    verify_space_alloc_status(num_dsets, dset_ids, plist_id, DATASET_JUST_CREATED);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");

    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
    VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    MPI_Barrier(comm);

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

        open_datasets(group_id, READ_SHARED_FILTERED_CHUNKS_3D_DATASET_NAME, num_dsets, test_mode, dset_ids);

        select_all(num_dsets, dset_ids, fspace_ids);

        write_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_ALL, fspace_ids, dcpl_id, H5P_DEFAULT,
                       data_bufs, test_mode, true, false, false);

        /* Verify space allocation status */
        plist_id = H5Dget_create_plist(dset_ids[0]);
        VRFY((plist_id >= 0), "H5Dget_create_plist succeeded");
        verify_space_alloc_status(num_dsets, dset_ids, plist_id, ALL_CHUNKS_WRITTEN);

        for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
            VRFY((H5Sclose(fspace_ids[dset_idx]) >= 0), "File dataspace close succeeded");
            VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");
        }

        VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
        VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
        VRFY((H5Fclose(file_id) >= 0), "File close succeeded");
    }

    MPI_Barrier(comm);

    file_id = H5Fopen(filenames[0], H5F_ACC_RDONLY, fapl_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
    VRFY((group_id >= 0), "H5Gopen2 succeeded");

    open_datasets(group_id, READ_SHARED_FILTERED_CHUNKS_3D_DATASET_NAME, num_dsets, test_mode, dset_ids);

    sel_dims[0] = (hsize_t)(READ_SHARED_FILTERED_CHUNKS_3D_NROWS / mpi_size);
    sel_dims[1] = (hsize_t)READ_SHARED_FILTERED_CHUNKS_3D_NCOLS;
    sel_dims[2] = (hsize_t)READ_SHARED_FILTERED_CHUNKS_3D_DEPTH;

    /* Setup one-dimensional memory dataspace for reading the dataset data into a contiguous buffer */
    flat_dims[0] = sel_dims[0] * sel_dims[1] * sel_dims[2];

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

    select_hyperslab(num_dsets, dset_ids, start, stride, count, block, fspace_ids);

    read_buf_size = flat_dims[0] * sizeof(C_DATATYPE);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        read_bufs[dset_idx] = calloc(1, read_buf_size);
        VRFY((NULL != read_bufs[dset_idx]), "calloc succeeded");
    }

    read_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_BLOCK, fspace_ids[0], dcpl_id, dxpl_id,
                  read_bufs, test_mode, true, true, false);

    /* Collect each piece of data from all ranks into a global buffer on all ranks */
    global_buf = calloc(1, data_size);
    VRFY((NULL != global_buf), "calloc succeeded");

    {
        size_t run_length =
            (size_t)(READ_SHARED_FILTERED_CHUNKS_3D_NCOLS * READ_SHARED_FILTERED_CHUNKS_3D_DEPTH);
        size_t num_blocks = (size_t)(READ_SHARED_FILTERED_CHUNKS_3D_NROWS / mpi_size);

        /*
         * Due to the nature of 3-dimensional reading, create an MPI vector type that allows each
         * rank to write to the nth position of the global data buffer, where n is the rank number.
         */
        mpi_code = MPI_Type_vector((int)num_blocks, (int)run_length, (int)(mpi_size * (int)run_length),
                                   C_DATATYPE_MPI, &vector_type);
        VRFY((MPI_SUCCESS == mpi_code), "MPI_Type_vector succeeded");
        mpi_code = MPI_Type_commit(&vector_type);
        VRFY((MPI_SUCCESS == mpi_code), "MPI_Type_commit succeeded");

        /*
         * Resize the type to allow interleaving,
         * so make it "run_length" MPI_LONGs wide
         */
        mpi_code = MPI_Type_create_resized(vector_type, 0, (MPI_Aint)(run_length * sizeof(long)),
                                           &resized_vector_type);
        VRFY((MPI_SUCCESS == mpi_code), "MPI_Type_create_resized");
        mpi_code = MPI_Type_commit(&resized_vector_type);
        VRFY((MPI_SUCCESS == mpi_code), "MPI_Type_commit succeeded");
    }

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        mpi_code = MPI_Allgather(read_bufs[dset_idx], (int)flat_dims[0], C_DATATYPE_MPI, global_buf, 1,
                                 resized_vector_type, comm);
        VRFY((MPI_SUCCESS == mpi_code), "MPI_Allgatherv succeeded");

        VRFY((0 == memcmp(global_buf, data_bufs[dset_idx], data_size)), "Data verification succeeded");
    }

    mpi_code = MPI_Type_free(&vector_type);
    VRFY((MPI_SUCCESS == mpi_code), "MPI_Type_free succeeded");
    mpi_code = MPI_Type_free(&resized_vector_type);
    VRFY((MPI_SUCCESS == mpi_code), "MPI_Type_free succeeded");

    free(global_buf);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        free(read_bufs[dset_idx]);

    for (size_t dset_idx = 0; dset_idx < MAX_NUM_DSETS_MULTI; dset_idx++)
        free(data_bufs_nc[dset_idx]);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        VRFY((H5Sclose(fspace_ids[dset_idx]) >= 0), "File dataspace close succeeded");
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");
    }

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
 */
static void
test_read_cmpd_filtered_dataset_no_conversion_unshared(const char *parent_group, H5Z_filter_t filter_id,
                                                       hid_t fapl_id, hid_t dcpl_id, hid_t dxpl_id,
                                                       test_mode_t test_mode)
{
    const void *data_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    void       *data_bufs_nc[MAX_NUM_DSETS_MULTI] = {0}; /* non-const buffer pointers for freeing */
    void       *read_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    void       *global_buf                        = NULL;
    hsize_t     dataset_dims[READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t     chunk_dims[READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t     sel_dims[READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t     start[READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t     stride[READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t     count[READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t     block[READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t     flat_dims[1];
    size_t      data_size, read_buf_size;
    size_t      num_dsets;
    hid_t       dset_ids[MAX_NUM_DSETS_MULTI];
    hid_t       fspace_ids[MAX_NUM_DSETS_MULTI];
    hid_t       file_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID;
    hid_t       group_id   = H5I_INVALID_HID;
    hid_t       memtype    = H5I_INVALID_HID;
    hid_t       filespace  = H5I_INVALID_HID;
    int        *recvcounts = NULL;
    int        *displs     = NULL;
    int         mpi_code;

    if (MAINPROCESS)
        puts("Testing read from unshared filtered chunks in Compound Datatype dataset without Datatype "
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
    data_size = dataset_dims[0] * dataset_dims[1] * sizeof(COMPOUND_C_DATATYPE);

    for (size_t dset_idx = 0; dset_idx < MAX_NUM_DSETS_MULTI; dset_idx++) {
        COMPOUND_C_DATATYPE *tmp_buf = calloc(1, data_size);
        VRFY((NULL != tmp_buf), "calloc succeeded");

        for (size_t j = 0; j < data_size / sizeof(COMPOUND_C_DATATYPE); j++) {
            tmp_buf[j].field1 = (short)((j % dataset_dims[1]) + (j / dataset_dims[1]) + dset_idx);
            tmp_buf[j].field2 = (int)((j % dataset_dims[1]) + (j / dataset_dims[1]) + dset_idx);
            tmp_buf[j].field3 = (long)((j % dataset_dims[1]) + (j / dataset_dims[1]) + dset_idx);
        }

        data_bufs[dset_idx]    = tmp_buf;
        data_bufs_nc[dset_idx] = tmp_buf;
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

    file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, fapl_id);
    VRFY((file_id >= 0), "Test file open succeeded");

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

    /* Create datasets depending on the current test mode */
    create_datasets(group_id, READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_NAME, memtype,
                    filespace, plist_id, test_mode, &num_dsets, dset_ids);

    /* Verify space allocation status */
    verify_space_alloc_status(num_dsets, dset_ids, plist_id, DATASET_JUST_CREATED);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");

    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
    VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    MPI_Barrier(comm);

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

        open_datasets(group_id, READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_NAME, num_dsets,
                      test_mode, dset_ids);

        select_all(num_dsets, dset_ids, fspace_ids);

        write_datasets(num_dsets, dset_ids, memtype, H5S_ALL, fspace_ids, dcpl_id, H5P_DEFAULT, data_bufs,
                       test_mode, true, false, false);

        /* Verify space allocation status */
        plist_id = H5Dget_create_plist(dset_ids[0]);
        VRFY((plist_id >= 0), "H5Dget_create_plist succeeded");
        verify_space_alloc_status(num_dsets, dset_ids, plist_id, ALL_CHUNKS_WRITTEN);

        for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
            VRFY((H5Sclose(fspace_ids[dset_idx]) >= 0), "File dataspace close succeeded");
            VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");
        }

        VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
        VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
        VRFY((H5Fclose(file_id) >= 0), "File close succeeded");
    }

    MPI_Barrier(comm);

    file_id = H5Fopen(filenames[0], H5F_ACC_RDONLY, fapl_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
    VRFY((group_id >= 0), "H5Gopen2 succeeded");

    open_datasets(group_id, READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_NAME, num_dsets,
                  test_mode, dset_ids);

    sel_dims[0] = (hsize_t)READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_CH_NROWS;
    sel_dims[1] = (hsize_t)READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_ENTRIES_PER_PROC;

    /* Setup one-dimensional memory dataspace for reading the dataset data into a contiguous buffer */
    flat_dims[0] = sel_dims[0] * sel_dims[1];

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

    select_hyperslab(num_dsets, dset_ids, start, stride, count, block, fspace_ids);

    read_buf_size = flat_dims[0] * sizeof(COMPOUND_C_DATATYPE);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        read_bufs[dset_idx] = calloc(1, read_buf_size);
        VRFY((NULL != read_bufs[dset_idx]), "calloc succeeded");
    }

    read_datasets(num_dsets, dset_ids, memtype, H5S_BLOCK, fspace_ids[0], dcpl_id, dxpl_id, read_bufs,
                  test_mode, true, true, false);

    /* Collect each piece of data from all ranks into a global buffer on all ranks */
    global_buf = calloc(1, data_size);
    VRFY((NULL != global_buf), "calloc succeeded");
    recvcounts = calloc(1, (size_t)mpi_size * sizeof(*recvcounts));
    VRFY((NULL != recvcounts), "calloc succeeded");
    displs = calloc(1, (size_t)mpi_size * sizeof(*displs));
    VRFY((NULL != displs), "calloc succeeded");

    for (size_t i = 0; i < (size_t)mpi_size; i++) {
        recvcounts[i] = (int)(flat_dims[0] * sizeof(COMPOUND_C_DATATYPE));
        displs[i]     = (int)(i * flat_dims[0] * sizeof(COMPOUND_C_DATATYPE));
    }

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        mpi_code = MPI_Allgatherv(read_bufs[dset_idx], (int)(flat_dims[0] * sizeof(COMPOUND_C_DATATYPE)),
                                  MPI_BYTE, global_buf, recvcounts, displs, MPI_BYTE, comm);
        VRFY((MPI_SUCCESS == mpi_code), "MPI_Allgatherv succeeded");

        VRFY((0 == memcmp(global_buf, data_bufs[dset_idx], data_size)), "Data verification succeeded");
    }

    free(displs);
    free(recvcounts);
    free(global_buf);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        free(read_bufs[dset_idx]);

    for (size_t dset_idx = 0; dset_idx < MAX_NUM_DSETS_MULTI; dset_idx++)
        free(data_bufs_nc[dset_idx]);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        VRFY((H5Sclose(fspace_ids[dset_idx]) >= 0), "File dataspace close succeeded");
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");
    }

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
 */
static void
test_read_cmpd_filtered_dataset_no_conversion_shared(const char *parent_group, H5Z_filter_t filter_id,
                                                     hid_t fapl_id, hid_t dcpl_id, hid_t dxpl_id,
                                                     test_mode_t test_mode)
{
    const void *data_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    void       *data_bufs_nc[MAX_NUM_DSETS_MULTI] = {0}; /* non-const buffer pointers for freeing */
    void       *read_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    void       *global_buf                        = NULL;
    hsize_t     dataset_dims[READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t     chunk_dims[READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t     sel_dims[READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t     start[READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t     stride[READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t     count[READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t     block[READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t     flat_dims[1];
    size_t      data_size, read_buf_size;
    size_t      num_dsets;
    hid_t       dset_ids[MAX_NUM_DSETS_MULTI];
    hid_t       fspace_ids[MAX_NUM_DSETS_MULTI];
    hid_t       file_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID;
    hid_t       group_id   = H5I_INVALID_HID;
    hid_t       memtype    = H5I_INVALID_HID;
    hid_t       filespace  = H5I_INVALID_HID;
    int        *recvcounts = NULL;
    int        *displs     = NULL;
    int         mpi_code;

    if (MAINPROCESS)
        puts("Testing read from shared filtered chunks in Compound Datatype dataset without Datatype "
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
    data_size = dataset_dims[0] * dataset_dims[1] * sizeof(COMPOUND_C_DATATYPE);

    for (size_t dset_idx = 0; dset_idx < MAX_NUM_DSETS_MULTI; dset_idx++) {
        COMPOUND_C_DATATYPE *tmp_buf = calloc(1, data_size);
        VRFY((NULL != tmp_buf), "calloc succeeded");

        for (size_t j = 0; j < data_size / sizeof(COMPOUND_C_DATATYPE); j++) {
            size_t val1 = (dataset_dims[1] * (j / ((hsize_t)mpi_size * dataset_dims[1])));
            size_t val2 = (j % dataset_dims[1]);
            size_t val3 = (((j % ((hsize_t)mpi_size * dataset_dims[1])) / dataset_dims[1]) % dataset_dims[1]);
            size_t val  = val1 + val2 + val3 + dset_idx;

            tmp_buf[j].field1 = (short)val;
            tmp_buf[j].field2 = (int)val;
            tmp_buf[j].field3 = (long)val;
        }

        data_bufs[dset_idx]    = tmp_buf;
        data_bufs_nc[dset_idx] = tmp_buf;
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

    file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, fapl_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
    VRFY((group_id >= 0), "H5Gopen2 succeeded");

    /* Create the dataspace for the dataset */
    filespace =
        H5Screate_simple(READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_DIMS, dataset_dims, NULL);
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

    /* Create datasets depending on the current test mode */
    create_datasets(group_id, READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_NAME, memtype,
                    filespace, plist_id, test_mode, &num_dsets, dset_ids);

    /* Verify space allocation status */
    verify_space_alloc_status(num_dsets, dset_ids, plist_id, DATASET_JUST_CREATED);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");

    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
    VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    MPI_Barrier(comm);

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

        open_datasets(group_id, READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_NAME, num_dsets,
                      test_mode, dset_ids);

        select_all(num_dsets, dset_ids, fspace_ids);

        write_datasets(num_dsets, dset_ids, memtype, H5S_ALL, fspace_ids, dcpl_id, H5P_DEFAULT, data_bufs,
                       test_mode, true, false, false);

        /* Verify space allocation status */
        plist_id = H5Dget_create_plist(dset_ids[0]);
        VRFY((plist_id >= 0), "H5Dget_create_plist succeeded");
        verify_space_alloc_status(num_dsets, dset_ids, plist_id, ALL_CHUNKS_WRITTEN);

        for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
            VRFY((H5Sclose(fspace_ids[dset_idx]) >= 0), "File dataspace close succeeded");
            VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");
        }

        VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
        VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
        VRFY((H5Fclose(file_id) >= 0), "File close succeeded");
    }

    MPI_Barrier(comm);

    file_id = H5Fopen(filenames[0], H5F_ACC_RDONLY, fapl_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
    VRFY((group_id >= 0), "H5Gopen2 succeeded");

    open_datasets(group_id, READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_NAME, num_dsets,
                  test_mode, dset_ids);

    sel_dims[0] = (hsize_t)READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_CH_NROWS / (hsize_t)mpi_size;
    sel_dims[1] = (hsize_t)READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_ENTRIES_PER_PROC;

    /* Setup one-dimensional memory dataspace for reading the dataset data into a contiguous buffer */
    flat_dims[0] = sel_dims[0] * sel_dims[1];

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

    select_hyperslab(num_dsets, dset_ids, start, stride, count, block, fspace_ids);

    read_buf_size = flat_dims[0] * sizeof(COMPOUND_C_DATATYPE);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        read_bufs[dset_idx] = calloc(1, read_buf_size);
        VRFY((NULL != read_bufs[dset_idx]), "calloc succeeded");
    }

    read_datasets(num_dsets, dset_ids, memtype, H5S_BLOCK, fspace_ids[0], dcpl_id, dxpl_id, read_bufs,
                  test_mode, true, true, false);

    /* Collect each piece of data from all ranks into a global buffer on all ranks */
    global_buf = calloc(1, data_size);
    VRFY((NULL != global_buf), "calloc succeeded");
    recvcounts = calloc(1, (size_t)mpi_size * sizeof(*recvcounts));
    VRFY((NULL != recvcounts), "calloc succeeded");
    displs = calloc(1, (size_t)mpi_size * sizeof(*displs));
    VRFY((NULL != displs), "calloc succeeded");

    for (size_t i = 0; i < (size_t)mpi_size; i++) {
        recvcounts[i] = (int)(flat_dims[0] * sizeof(COMPOUND_C_DATATYPE));
        displs[i]     = (int)(i * flat_dims[0] * sizeof(COMPOUND_C_DATATYPE));
    }

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        mpi_code = MPI_Allgatherv(read_bufs[dset_idx], (int)(flat_dims[0] * sizeof(COMPOUND_C_DATATYPE)),
                                  MPI_BYTE, global_buf, recvcounts, displs, MPI_BYTE, comm);
        VRFY((MPI_SUCCESS == mpi_code), "MPI_Allgatherv succeeded");

        VRFY((0 == memcmp(global_buf, data_bufs[dset_idx], data_size)), "Data verification succeeded");
    }

    free(displs);
    free(recvcounts);
    free(global_buf);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        free(read_bufs[dset_idx]);

    for (size_t dset_idx = 0; dset_idx < MAX_NUM_DSETS_MULTI; dset_idx++)
        free(data_bufs_nc[dset_idx]);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        VRFY((H5Sclose(fspace_ids[dset_idx]) >= 0), "File dataspace close succeeded");
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");
    }

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
 */
static void
test_read_cmpd_filtered_dataset_type_conversion_unshared(const char *parent_group, H5Z_filter_t filter_id,
                                                         hid_t fapl_id, hid_t dcpl_id, hid_t dxpl_id,
                                                         test_mode_t test_mode)
{
    const void *data_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    void       *data_bufs_nc[MAX_NUM_DSETS_MULTI] = {0}; /* non-const buffer pointers for freeing */
    void       *read_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    void       *global_buf                        = NULL;
    hsize_t     dataset_dims[READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t     chunk_dims[READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t     sel_dims[READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t     start[READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t     stride[READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t     count[READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t     block[READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_DIMS];
    hsize_t     flat_dims[1];
    size_t      data_size, read_buf_size;
    size_t      num_dsets;
    hid_t       dset_ids[MAX_NUM_DSETS_MULTI];
    hid_t       fspace_ids[MAX_NUM_DSETS_MULTI];
    hid_t       file_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID;
    hid_t       group_id = H5I_INVALID_HID;
    hid_t       filetype = H5I_INVALID_HID, memtype = H5I_INVALID_HID;
    hid_t       filespace  = H5I_INVALID_HID;
    int        *recvcounts = NULL;
    int        *displs     = NULL;
    int         mpi_code;

    if (MAINPROCESS)
        puts("Testing read from unshared filtered chunks in Compound Datatype dataset with Datatype "
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
    data_size = dataset_dims[0] * dataset_dims[1] * sizeof(COMPOUND_C_DATATYPE);

    for (size_t dset_idx = 0; dset_idx < MAX_NUM_DSETS_MULTI; dset_idx++) {
        COMPOUND_C_DATATYPE *tmp_buf = calloc(1, data_size);
        VRFY((NULL != tmp_buf), "calloc succeeded");

        for (size_t j = 0; j < data_size / sizeof(COMPOUND_C_DATATYPE); j++) {
            tmp_buf[j].field1 = (short)((j % dataset_dims[1]) + (j / dataset_dims[1]) + dset_idx);
            tmp_buf[j].field2 = (int)((j % dataset_dims[1]) + (j / dataset_dims[1]) + dset_idx);
            tmp_buf[j].field3 = (long)((j % dataset_dims[1]) + (j / dataset_dims[1]) + dset_idx);
        }

        data_bufs[dset_idx]    = tmp_buf;
        data_bufs_nc[dset_idx] = tmp_buf;
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

    file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, fapl_id);
    VRFY((file_id >= 0), "Test file open succeeded");

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

    /* Create datasets depending on the current test mode */
    create_datasets(group_id, READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_NAME, filetype,
                    filespace, plist_id, test_mode, &num_dsets, dset_ids);

    /* Verify space allocation status */
    verify_space_alloc_status(num_dsets, dset_ids, plist_id, DATASET_JUST_CREATED);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");

    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
    VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    MPI_Barrier(comm);

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

        open_datasets(group_id, READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_NAME,
                      num_dsets, test_mode, dset_ids);

        select_all(num_dsets, dset_ids, fspace_ids);

        write_datasets(num_dsets, dset_ids, memtype, H5S_ALL, fspace_ids, dcpl_id, H5P_DEFAULT, data_bufs,
                       test_mode, true, false, false);

        /* Verify space allocation status */
        plist_id = H5Dget_create_plist(dset_ids[0]);
        VRFY((plist_id >= 0), "H5Dget_create_plist succeeded");
        verify_space_alloc_status(num_dsets, dset_ids, plist_id, ALL_CHUNKS_WRITTEN);

        for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
            VRFY((H5Sclose(fspace_ids[dset_idx]) >= 0), "File dataspace close succeeded");
            VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");
        }

        VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
        VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
        VRFY((H5Fclose(file_id) >= 0), "File close succeeded");
    }

    MPI_Barrier(comm);

    file_id = H5Fopen(filenames[0], H5F_ACC_RDONLY, fapl_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
    VRFY((group_id >= 0), "H5Gopen2 succeeded");

    open_datasets(group_id, READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_NAME, num_dsets,
                  test_mode, dset_ids);

    sel_dims[0] = (hsize_t)READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_CH_NROWS;
    sel_dims[1] = (hsize_t)READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_ENTRIES_PER_PROC;

    /* Setup one-dimensional memory dataspace for reading the dataset data into a contiguous buffer */
    flat_dims[0] = sel_dims[0] * sel_dims[1];

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

    select_hyperslab(num_dsets, dset_ids, start, stride, count, block, fspace_ids);

    read_buf_size = flat_dims[0] * sizeof(COMPOUND_C_DATATYPE);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        read_bufs[dset_idx] = calloc(1, read_buf_size);
        VRFY((NULL != read_bufs[dset_idx]), "calloc succeeded");
    }

    read_datasets(num_dsets, dset_ids, memtype, H5S_BLOCK, fspace_ids[0], dcpl_id, dxpl_id, read_bufs,
                  test_mode, true, false, false);

    /* Collect each piece of data from all ranks into a global buffer on all ranks */
    global_buf = calloc(1, data_size);
    VRFY((NULL != global_buf), "calloc succeeded");
    recvcounts = calloc(1, (size_t)mpi_size * sizeof(*recvcounts));
    VRFY((NULL != recvcounts), "calloc succeeded");
    displs = calloc(1, (size_t)mpi_size * sizeof(*displs));
    VRFY((NULL != displs), "calloc succeeded");

    for (size_t i = 0; i < (size_t)mpi_size; i++) {
        recvcounts[i] = (int)(flat_dims[0] * sizeof(COMPOUND_C_DATATYPE));
        displs[i]     = (int)(i * flat_dims[0] * sizeof(COMPOUND_C_DATATYPE));
    }

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        mpi_code = MPI_Allgatherv(read_bufs[dset_idx], (int)(flat_dims[0] * sizeof(COMPOUND_C_DATATYPE)),
                                  MPI_BYTE, global_buf, recvcounts, displs, MPI_BYTE, comm);
        VRFY((MPI_SUCCESS == mpi_code), "MPI_Allgatherv succeeded");

        VRFY((0 == memcmp(global_buf, data_bufs[dset_idx], data_size)), "Data verification succeeded");
    }

    free(displs);
    free(recvcounts);
    free(global_buf);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        free(read_bufs[dset_idx]);

    for (size_t dset_idx = 0; dset_idx < MAX_NUM_DSETS_MULTI; dset_idx++)
        free(data_bufs_nc[dset_idx]);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        VRFY((H5Sclose(fspace_ids[dset_idx]) >= 0), "File dataspace close succeeded");
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");
    }

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
 */
static void
test_read_cmpd_filtered_dataset_type_conversion_shared(const char *parent_group, H5Z_filter_t filter_id,
                                                       hid_t fapl_id, hid_t dcpl_id, hid_t dxpl_id,
                                                       test_mode_t test_mode)
{
    const void *data_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    void       *data_bufs_nc[MAX_NUM_DSETS_MULTI] = {0}; /* non-const buffer pointers for freeing */
    void       *read_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    void       *global_buf                        = NULL;
    hsize_t     dataset_dims[READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t     chunk_dims[READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t     sel_dims[READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t     start[READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t     stride[READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t     count[READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t     block[READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_DIMS];
    hsize_t     flat_dims[1];
    size_t      data_size, read_buf_size;
    size_t      num_dsets;
    hid_t       dset_ids[MAX_NUM_DSETS_MULTI];
    hid_t       fspace_ids[MAX_NUM_DSETS_MULTI];
    hid_t       file_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID;
    hid_t       group_id = H5I_INVALID_HID;
    hid_t       filetype = H5I_INVALID_HID, memtype = H5I_INVALID_HID;
    hid_t       filespace  = H5I_INVALID_HID;
    int        *recvcounts = NULL;
    int        *displs     = NULL;
    int         mpi_code;

    if (MAINPROCESS)
        puts(
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
    data_size = dataset_dims[0] * dataset_dims[1] * sizeof(COMPOUND_C_DATATYPE);

    for (size_t dset_idx = 0; dset_idx < MAX_NUM_DSETS_MULTI; dset_idx++) {
        COMPOUND_C_DATATYPE *tmp_buf = calloc(1, data_size);
        VRFY((NULL != tmp_buf), "calloc succeeded");

        for (size_t j = 0; j < data_size / sizeof(COMPOUND_C_DATATYPE); j++) {
            size_t val1 = (dataset_dims[1] * (j / ((hsize_t)mpi_size * dataset_dims[1])));
            size_t val2 = (j % dataset_dims[1]);
            size_t val3 = (((j % ((hsize_t)mpi_size * dataset_dims[1])) / dataset_dims[1]) % dataset_dims[1]);
            size_t val  = val1 + val2 + val3 + dset_idx;

            tmp_buf[j].field1 = (short)val;
            tmp_buf[j].field2 = (int)val;
            tmp_buf[j].field3 = (long)val;
        }

        data_bufs[dset_idx]    = tmp_buf;
        data_bufs_nc[dset_idx] = tmp_buf;
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

    file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, fapl_id);
    VRFY((file_id >= 0), "Test file open succeeded");

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

    /* Create datasets depending on the current test mode */
    create_datasets(group_id, READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_NAME, filetype,
                    filespace, plist_id, test_mode, &num_dsets, dset_ids);

    /* Verify space allocation status */
    verify_space_alloc_status(num_dsets, dset_ids, plist_id, DATASET_JUST_CREATED);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");

    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
    VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    MPI_Barrier(comm);

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

        open_datasets(group_id, READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_NAME, num_dsets,
                      test_mode, dset_ids);

        select_all(num_dsets, dset_ids, fspace_ids);

        write_datasets(num_dsets, dset_ids, memtype, H5S_ALL, fspace_ids, dcpl_id, H5P_DEFAULT, data_bufs,
                       test_mode, true, false, false);

        /* Verify space allocation status */
        plist_id = H5Dget_create_plist(dset_ids[0]);
        VRFY((plist_id >= 0), "H5Dget_create_plist succeeded");
        verify_space_alloc_status(num_dsets, dset_ids, plist_id, ALL_CHUNKS_WRITTEN);

        for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
            VRFY((H5Sclose(fspace_ids[dset_idx]) >= 0), "File dataspace close succeeded");
            VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");
        }

        VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
        VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
        VRFY((H5Fclose(file_id) >= 0), "File close succeeded");
    }

    MPI_Barrier(comm);

    file_id = H5Fopen(filenames[0], H5F_ACC_RDONLY, fapl_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
    VRFY((group_id >= 0), "H5Gopen2 succeeded");

    open_datasets(group_id, READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_NAME, num_dsets,
                  test_mode, dset_ids);

    sel_dims[0] = (hsize_t)READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_CH_NROWS / (hsize_t)mpi_size;
    sel_dims[1] = (hsize_t)READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_ENTRIES_PER_PROC;

    /* Setup one-dimensional memory dataspace for reading the dataset data into a contiguous buffer */
    flat_dims[0] = sel_dims[0] * sel_dims[1];

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

    select_hyperslab(num_dsets, dset_ids, start, stride, count, block, fspace_ids);

    read_buf_size = flat_dims[0] * sizeof(COMPOUND_C_DATATYPE);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        read_bufs[dset_idx] = calloc(1, read_buf_size);
        VRFY((NULL != read_bufs[dset_idx]), "calloc succeeded");
    }

    read_datasets(num_dsets, dset_ids, memtype, H5S_BLOCK, fspace_ids[0], dcpl_id, dxpl_id, read_bufs,
                  test_mode, true, false, false);

    /* Collect each piece of data from all ranks into a global buffer on all ranks */
    global_buf = calloc(1, data_size);
    VRFY((NULL != global_buf), "calloc succeeded");
    recvcounts = calloc(1, (size_t)mpi_size * sizeof(*recvcounts));
    VRFY((NULL != recvcounts), "calloc succeeded");
    displs = calloc(1, (size_t)mpi_size * sizeof(*displs));
    VRFY((NULL != displs), "calloc succeeded");

    for (size_t i = 0; i < (size_t)mpi_size; i++) {
        recvcounts[i] = (int)(flat_dims[0] * sizeof(COMPOUND_C_DATATYPE));
        displs[i]     = (int)(i * flat_dims[0] * sizeof(COMPOUND_C_DATATYPE));
    }

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        mpi_code = MPI_Allgatherv(read_bufs[dset_idx], (int)(flat_dims[0] * sizeof(COMPOUND_C_DATATYPE)),
                                  MPI_BYTE, global_buf, recvcounts, displs, MPI_BYTE, comm);
        VRFY((MPI_SUCCESS == mpi_code), "MPI_Allgatherv succeeded");

        VRFY((0 == memcmp(global_buf, data_bufs[dset_idx], data_size)), "Data verification succeeded");
    }

    free(displs);
    free(recvcounts);
    free(global_buf);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        free(read_bufs[dset_idx]);

    for (size_t dset_idx = 0; dset_idx < MAX_NUM_DSETS_MULTI; dset_idx++)
        free(data_bufs_nc[dset_idx]);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        VRFY((H5Sclose(fspace_ids[dset_idx]) >= 0), "File dataspace close succeeded");
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");
    }

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
 */
static void
test_write_serial_read_parallel(const char *parent_group, H5Z_filter_t filter_id, hid_t fapl_id,
                                hid_t dcpl_id, hid_t dxpl_id, test_mode_t test_mode)
{
    C_DATATYPE *correct_bufs[MAX_NUM_DSETS_MULTI] = {0};
    const void *data_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    void       *data_bufs_nc[MAX_NUM_DSETS_MULTI] = {0}; /* non-const buffer pointers for freeing */
    void       *read_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    hsize_t     dataset_dims[WRITE_SERIAL_READ_PARALLEL_DATASET_DIMS];
    hsize_t     chunk_dims[WRITE_SERIAL_READ_PARALLEL_DATASET_DIMS];
    size_t      data_size, correct_buf_size;
    size_t      num_dsets;
    hid_t       dset_ids[MAX_NUM_DSETS_MULTI];
    hid_t       fspace_ids[MAX_NUM_DSETS_MULTI];
    hid_t       file_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID;
    hid_t       group_id  = H5I_INVALID_HID;
    hid_t       filespace = H5I_INVALID_HID;

    if (MAINPROCESS)
        puts("Testing write file serially; read file in parallel");

    dataset_dims[0] = (hsize_t)WRITE_SERIAL_READ_PARALLEL_NROWS;
    dataset_dims[1] = (hsize_t)WRITE_SERIAL_READ_PARALLEL_NCOLS;
    dataset_dims[2] = (hsize_t)WRITE_SERIAL_READ_PARALLEL_DEPTH;

    file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, fapl_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
    VRFY((group_id >= 0), "H5Gopen2 succeeded");

    /* Create the dataspace for the dataset */
    filespace = H5Screate_simple(WRITE_SERIAL_READ_PARALLEL_DATASET_DIMS, dataset_dims, NULL);
    VRFY((filespace >= 0), "File dataspace creation succeeded");

    /* Create chunked dataset */
    chunk_dims[0] = (hsize_t)WRITE_SERIAL_READ_PARALLEL_CH_NROWS;
    chunk_dims[1] = (hsize_t)WRITE_SERIAL_READ_PARALLEL_CH_NCOLS;
    chunk_dims[2] = 1;

    plist_id = H5Pcopy(dcpl_id);
    VRFY((plist_id >= 0), "DCPL copy succeeded");

    VRFY((H5Pset_chunk(plist_id, WRITE_SERIAL_READ_PARALLEL_DATASET_DIMS, chunk_dims) >= 0),
         "Chunk size set");

    /* Add test filter to the pipeline */
    VRFY((set_dcpl_filter(plist_id, filter_id, NULL) >= 0), "Filter set");

    /* Create datasets depending on the current test mode */
    create_datasets(group_id, WRITE_SERIAL_READ_PARALLEL_DATASET_NAME, HDF5_DATATYPE_NAME, filespace,
                    plist_id, test_mode, &num_dsets, dset_ids);

    /* Verify space allocation status */
    verify_space_alloc_status(num_dsets, dset_ids, plist_id, DATASET_JUST_CREATED);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");

    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");
    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
    VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    MPI_Barrier(comm);

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

        open_datasets(group_id, WRITE_SERIAL_READ_PARALLEL_DATASET_NAME, num_dsets, test_mode, dset_ids);

        data_size = dataset_dims[0] * dataset_dims[1] * dataset_dims[2] * sizeof(C_DATATYPE);

        for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
            C_DATATYPE *tmp_buf = (C_DATATYPE *)calloc(1, data_size);
            VRFY((NULL != tmp_buf), "calloc succeeded");

            for (size_t j = 0; j < data_size / sizeof(C_DATATYPE); j++)
                tmp_buf[j] = (C_DATATYPE)(GEN_DATA(j) + dset_idx);

            data_bufs[dset_idx]    = tmp_buf;
            data_bufs_nc[dset_idx] = tmp_buf;
        }

        select_all(num_dsets, dset_ids, fspace_ids);

        write_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_ALL, fspace_ids, dcpl_id, H5P_DEFAULT,
                       data_bufs, test_mode, true, false, false);

        for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
            free(data_bufs_nc[dset_idx]);

        /* Verify space allocation status */
        plist_id = H5Dget_create_plist(dset_ids[0]);
        VRFY((plist_id >= 0), "H5Dget_create_plist succeeded");
        verify_space_alloc_status(num_dsets, dset_ids, plist_id, ALL_CHUNKS_WRITTEN);

        /* Close and re-open datasets */
        for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
            VRFY((H5Sclose(fspace_ids[dset_idx]) >= 0), "File dataspace close succeeded");
            VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");
        }

        VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
        VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
        VRFY((H5Fclose(file_id) >= 0), "File close succeeded");
    }

    MPI_Barrier(comm);

    correct_buf_size = dataset_dims[0] * dataset_dims[1] * dataset_dims[2] * sizeof(C_DATATYPE);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        correct_bufs[dset_idx] = calloc(1, correct_buf_size);
        VRFY((NULL != correct_bufs[dset_idx]), "calloc succeeded");
        read_bufs[dset_idx] = calloc(1, correct_buf_size);
        VRFY((NULL != read_bufs[dset_idx]), "calloc succeeded");

        for (size_t j = 0; j < correct_buf_size / sizeof(C_DATATYPE); j++)
            correct_bufs[dset_idx][j] = (long)(j + dset_idx);
    }

    /* All ranks open the file and verify their "portion" of the dataset is correct */
    file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, fapl_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
    VRFY((group_id >= 0), "H5Gopen2 succeeded");

    open_datasets(group_id, WRITE_SERIAL_READ_PARALLEL_DATASET_NAME, num_dsets, test_mode, dset_ids);

    read_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, dcpl_id, dxpl_id, read_bufs,
                  test_mode, true, true, false);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        VRFY((0 == memcmp(read_bufs[dset_idx], correct_bufs[dset_idx], correct_buf_size)),
             "Data verification succeeded");

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        free(read_bufs[dset_idx]);
        free(correct_bufs[dset_idx]);
    }

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");

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
 */
static void
test_write_parallel_read_serial(const char *parent_group, H5Z_filter_t filter_id, hid_t fapl_id,
                                hid_t dcpl_id, hid_t dxpl_id, test_mode_t test_mode)
{
    C_DATATYPE *correct_bufs[MAX_NUM_DSETS_MULTI] = {0};
    const void *data_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    void       *data_bufs_nc[MAX_NUM_DSETS_MULTI] = {0}; /* non-const buffer pointers for freeing */
    void       *read_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    hsize_t     dataset_dims[WRITE_PARALLEL_READ_SERIAL_DATASET_DIMS];
    hsize_t     chunk_dims[WRITE_PARALLEL_READ_SERIAL_DATASET_DIMS];
    hsize_t     sel_dims[WRITE_PARALLEL_READ_SERIAL_DATASET_DIMS];
    hsize_t     count[WRITE_PARALLEL_READ_SERIAL_DATASET_DIMS];
    hsize_t     stride[WRITE_PARALLEL_READ_SERIAL_DATASET_DIMS];
    hsize_t     block[WRITE_PARALLEL_READ_SERIAL_DATASET_DIMS];
    hsize_t     offset[WRITE_PARALLEL_READ_SERIAL_DATASET_DIMS];
    size_t      data_size, correct_buf_size;
    size_t      num_dsets;
    hid_t       dset_ids[MAX_NUM_DSETS_MULTI];
    hid_t       fspace_ids[MAX_NUM_DSETS_MULTI];
    hid_t       file_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID;
    hid_t       group_id  = H5I_INVALID_HID;
    hid_t       filespace = H5I_INVALID_HID;

    if (MAINPROCESS)
        puts("Testing write file in parallel; read serially");

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

    /* Create chunked dataset */
    plist_id = H5Pcopy(dcpl_id);
    VRFY((plist_id >= 0), "DCPL copy succeeded");

    VRFY((H5Pset_chunk(plist_id, WRITE_PARALLEL_READ_SERIAL_DATASET_DIMS, chunk_dims) >= 0),
         "Chunk size set");

    /* Add test filter to the pipeline */
    VRFY((set_dcpl_filter(plist_id, filter_id, NULL) >= 0), "Filter set");

    /* Create datasets depending on the current test mode */
    create_datasets(group_id, WRITE_PARALLEL_READ_SERIAL_DATASET_NAME, HDF5_DATATYPE_NAME, filespace,
                    plist_id, test_mode, &num_dsets, dset_ids);

    /* Verify space allocation status */
    verify_space_alloc_status(num_dsets, dset_ids, plist_id, DATASET_JUST_CREATED);

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

    select_hyperslab(num_dsets, dset_ids, offset, stride, count, block, fspace_ids);

    /* Fill data buffer */
    data_size = sel_dims[0] * sel_dims[1] * sel_dims[2] * sizeof(C_DATATYPE);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        C_DATATYPE *tmp_buf = calloc(1, data_size);
        VRFY((NULL != tmp_buf), "calloc succeeded");

        for (size_t j = 0; j < data_size / sizeof(C_DATATYPE); j++)
            tmp_buf[j] = (C_DATATYPE)(GEN_DATA(j) + dset_idx);

        data_bufs[dset_idx]    = tmp_buf;
        data_bufs_nc[dset_idx] = tmp_buf;
    }

    write_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_BLOCK, fspace_ids, dcpl_id, dxpl_id,
                   data_bufs, test_mode, true, true, false);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        free(data_bufs_nc[dset_idx]);

    /* Verify space allocation status */
    verify_space_alloc_status(num_dsets, dset_ids, plist_id, ALL_CHUNKS_WRITTEN);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        VRFY((H5Sclose(fspace_ids[dset_idx]) >= 0), "File dataspace close succeeded");
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");
    }

    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
    VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    MPI_Barrier(comm);

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

        open_datasets(group_id, WRITE_PARALLEL_READ_SERIAL_DATASET_NAME, num_dsets, test_mode, dset_ids);

        correct_buf_size = dataset_dims[0] * dataset_dims[1] * dataset_dims[2] * sizeof(C_DATATYPE);

        for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
            correct_bufs[dset_idx] = calloc(1, correct_buf_size);
            VRFY((NULL != correct_bufs[dset_idx]), "calloc succeeded");
            read_bufs[dset_idx] = calloc(1, correct_buf_size);
            VRFY((NULL != read_bufs[dset_idx]), "calloc succeeded");

            for (size_t j = 0; j < correct_buf_size / sizeof(C_DATATYPE); j++)
                correct_bufs[dset_idx][j] =
                    (C_DATATYPE)((j % (dataset_dims[0] * dataset_dims[1])) +
                                 (j / (dataset_dims[0] * dataset_dims[1])) + dset_idx);
        }

        read_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, dcpl_id, H5P_DEFAULT,
                      read_bufs, test_mode, true, true, false);

        for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
            VRFY((0 == memcmp(read_bufs[dset_idx], correct_bufs[dset_idx], correct_buf_size)),
                 "Data verification succeeded");

        for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
            free(read_bufs[dset_idx]);
            free(correct_bufs[dset_idx]);
        }

        for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
            VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");

        VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
        VRFY((H5Fclose(file_id) >= 0), "File close succeeded");
    }

    MPI_Barrier(comm);

    return;
}

/*
 * Tests that causing chunks to continually grow and shrink
 * by writing random data followed by zeroed-out data (and
 * thus controlling the compression ratio) does not cause
 * problems.
 *
 */
static void
test_shrinking_growing_chunks(const char *parent_group, H5Z_filter_t filter_id, hid_t fapl_id, hid_t dcpl_id,
                              hid_t dxpl_id, test_mode_t test_mode)
{
    const void *data_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    void       *data_bufs_nc[MAX_NUM_DSETS_MULTI] = {0}; /* non-const buffer pointers for freeing */
    void       *read_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    hsize_t     dataset_dims[SHRINKING_GROWING_CHUNKS_DATASET_DIMS];
    hsize_t     chunk_dims[SHRINKING_GROWING_CHUNKS_DATASET_DIMS];
    hsize_t     sel_dims[SHRINKING_GROWING_CHUNKS_DATASET_DIMS];
    hsize_t     start[SHRINKING_GROWING_CHUNKS_DATASET_DIMS];
    hsize_t     stride[SHRINKING_GROWING_CHUNKS_DATASET_DIMS];
    hsize_t     count[SHRINKING_GROWING_CHUNKS_DATASET_DIMS];
    hsize_t     block[SHRINKING_GROWING_CHUNKS_DATASET_DIMS];
    size_t      data_size;
    size_t      num_dsets;
    size_t      num_loops;
    hid_t       dset_ids[MAX_NUM_DSETS_MULTI];
    hid_t       fspace_ids[MAX_NUM_DSETS_MULTI];
    hid_t       file_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID;
    hid_t       group_id  = H5I_INVALID_HID;
    hid_t       filespace = H5I_INVALID_HID;

    if (MAINPROCESS)
        puts("Testing continually shrinking/growing chunks");

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

    /* Create chunked dataset */
    plist_id = H5Pcopy(dcpl_id);
    VRFY((plist_id >= 0), "DCPL copy succeeded");

    VRFY((H5Pset_chunk(plist_id, SHRINKING_GROWING_CHUNKS_DATASET_DIMS, chunk_dims) >= 0), "Chunk size set");

    /* Add test filter to the pipeline */
    VRFY((set_dcpl_filter(plist_id, filter_id, NULL) >= 0), "Filter set");

    /* Create datasets depending on the current test mode */
    create_datasets(group_id, SHRINKING_GROWING_CHUNKS_DATASET_NAME, H5T_NATIVE_DOUBLE, filespace, plist_id,
                    test_mode, &num_dsets, dset_ids);

    /* Verify space allocation status */
    verify_space_alloc_status(num_dsets, dset_ids, plist_id, DATASET_JUST_CREATED);

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

    select_hyperslab(num_dsets, dset_ids, start, stride, count, block, fspace_ids);

    data_size = sel_dims[0] * sel_dims[1] * sizeof(double);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        double *tmp_buf = calloc(1, data_size);
        VRFY((NULL != tmp_buf), "calloc succeeded");

        for (size_t j = 0; j < data_size / sizeof(C_DATATYPE); j++)
            tmp_buf[j] = (double)(GEN_DATA(j) + dset_idx);

        data_bufs[dset_idx]    = tmp_buf;
        data_bufs_nc[dset_idx] = tmp_buf;

        tmp_buf = calloc(1, data_size);
        VRFY((NULL != tmp_buf), "calloc succeeded");

        read_bufs[dset_idx] = tmp_buf;
    }

    /* Determine number of loops to run through */
    num_loops = SHRINKING_GROWING_CHUNKS_NLOOPS;
    if ((test_mode == USE_MULTIPLE_DATASETS) || (test_mode == USE_MULTIPLE_DATASETS_MIXED_FILTERED))
        num_loops /= 2;

    for (size_t i = 0; i < num_loops; i++) {
        for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
            /* Continually write random float data, followed by zeroed-out data */
            if (i % 2)
                memset(data_bufs_nc[dset_idx], 0, data_size);
            else {
                double *tmp_buf = data_bufs_nc[dset_idx];

                for (size_t k = 0; k < data_size / sizeof(double); k++) {
                    tmp_buf[k] = (rand() / (double)(RAND_MAX / (double)1.0L));
                }
            }
        }

        write_datasets(num_dsets, dset_ids, H5T_NATIVE_DOUBLE, H5S_BLOCK, fspace_ids, dcpl_id, dxpl_id,
                       data_bufs, test_mode, true, true, i > 0);

        /* Verify space allocation status */
        verify_space_alloc_status(num_dsets, dset_ids, plist_id, ALL_CHUNKS_WRITTEN);

        for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
            if (i % 2) {
                memset(read_bufs[dset_idx], 255, data_size);
            }
            else {
                memset(read_bufs[dset_idx], 0, data_size);
            }
        }

        read_datasets(num_dsets, dset_ids, H5T_NATIVE_DOUBLE, H5S_BLOCK, fspace_ids[0], dcpl_id, dxpl_id,
                      read_bufs, test_mode, true, true, false);

        for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
            VRFY((0 == memcmp(read_bufs[dset_idx], data_bufs[dset_idx], data_size)),
                 "data verification succeeded");
    }

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        free(read_bufs[dset_idx]);
        free(data_bufs_nc[dset_idx]);
    }

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        VRFY((H5Sclose(fspace_ids[dset_idx]) >= 0), "File dataspace close succeeded");
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");
    }

    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
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
                            hid_t dxpl_id, test_mode_t test_mode)
{
    const void *data_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    void       *data_bufs_nc[MAX_NUM_DSETS_MULTI] = {0}; /* non-const buffer pointers for freeing */
    void       *read_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    hsize_t     dataset_dims[WRITE_UNSHARED_FILTERED_EDGE_CHUNKS_DATASET_DIMS];
    hsize_t     chunk_dims[WRITE_UNSHARED_FILTERED_EDGE_CHUNKS_DATASET_DIMS];
    hsize_t     sel_dims[WRITE_UNSHARED_FILTERED_EDGE_CHUNKS_DATASET_DIMS];
    hsize_t     start[WRITE_UNSHARED_FILTERED_EDGE_CHUNKS_DATASET_DIMS];
    hsize_t     stride[WRITE_UNSHARED_FILTERED_EDGE_CHUNKS_DATASET_DIMS];
    hsize_t     count[WRITE_UNSHARED_FILTERED_EDGE_CHUNKS_DATASET_DIMS];
    hsize_t     block[WRITE_UNSHARED_FILTERED_EDGE_CHUNKS_DATASET_DIMS];
    size_t      data_size;
    size_t      num_dsets;
    hid_t       dset_ids[MAX_NUM_DSETS_MULTI];
    hid_t       fspace_ids[MAX_NUM_DSETS_MULTI];
    hid_t       file_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID;
    hid_t       group_id  = H5I_INVALID_HID;
    hid_t       filespace = H5I_INVALID_HID;

    if (MAINPROCESS)
        puts("Testing write to unshared filtered edge chunks");

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

    /* Create datasets depending on the current test mode */
    create_datasets(group_id, WRITE_UNSHARED_FILTERED_EDGE_CHUNKS_DATASET_NAME, HDF5_DATATYPE_NAME, filespace,
                    plist_id, test_mode, &num_dsets, dset_ids);

    /* Verify space allocation status */
    verify_space_alloc_status(num_dsets, dset_ids, plist_id, DATASET_JUST_CREATED);

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

    select_hyperslab(num_dsets, dset_ids, start, stride, count, block, fspace_ids);

    /* Fill data buffer */
    data_size = sel_dims[0] * sel_dims[1] * sizeof(C_DATATYPE);

    for (size_t dset_idx = 0; dset_idx < MAX_NUM_DSETS_MULTI; dset_idx++) {
        C_DATATYPE *tmp_buf = calloc(1, data_size);
        VRFY((NULL != tmp_buf), "calloc succeeded");

        for (size_t j = 0; j < data_size / sizeof(C_DATATYPE); j++)
            tmp_buf[j] = (C_DATATYPE)(GEN_DATA(j) + dset_idx);

        data_bufs[dset_idx]    = tmp_buf;
        data_bufs_nc[dset_idx] = tmp_buf;

        tmp_buf = calloc(1, data_size);
        VRFY((NULL != tmp_buf), "calloc succeeded");

        read_bufs[dset_idx] = tmp_buf;
    }

    write_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_BLOCK, fspace_ids, dcpl_id, dxpl_id,
                   data_bufs, test_mode, true, true, false);

    /* Verify space allocation status */
    verify_space_alloc_status(num_dsets, dset_ids, plist_id,
                              (mpi_size > 1) ? SOME_CHUNKS_WRITTEN : ALL_CHUNKS_WRITTEN);

    /* Close and re-open datasets */
    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");

    /* Verify the correct data was written */
    open_datasets(group_id, WRITE_UNSHARED_FILTERED_EDGE_CHUNKS_DATASET_NAME, num_dsets, test_mode, dset_ids);

    read_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_BLOCK, fspace_ids[0], dcpl_id, dxpl_id,
                  read_bufs, test_mode, true, true, false);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        VRFY((0 == memcmp(read_bufs[dset_idx], data_bufs[dset_idx], data_size)),
             "Data verification succeeded");

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        VRFY((H5Sclose(fspace_ids[dset_idx]) >= 0), "File dataspace close succeeded");
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");
    }

    /* Repeat the previous, but set option to not filter partial edge chunks */
    if (MAINPROCESS)
        puts("Testing write to unshared unfiltered edge chunks");

    filespace = H5Screate_simple(WRITE_UNSHARED_FILTERED_EDGE_CHUNKS_DATASET_DIMS, dataset_dims, NULL);
    VRFY((filespace >= 0), "File dataspace creation succeeded");

    /*
     * Since we're only doing a partial write to the dataset, make
     * sure the fill time is set appropriately
     */
    VRFY((H5Pset_fill_time(plist_id, H5D_FILL_TIME_IFSET) >= 0), "H5Pset_fill_time succeeded");

    H5Pset_chunk_opts(plist_id, H5D_CHUNK_DONT_FILTER_PARTIAL_CHUNKS);

    /* Create datasets depending on the current test mode */
    create_datasets(group_id, WRITE_UNSHARED_FILTERED_EDGE_CHUNKS_DATASET_NAME2, HDF5_DATATYPE_NAME,
                    filespace, plist_id, test_mode, &num_dsets, dset_ids);

    /* Verify space allocation status */
    verify_space_alloc_status(num_dsets, dset_ids, plist_id, DATASET_JUST_CREATED);

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

    select_hyperslab(num_dsets, dset_ids, start, stride, count, block, fspace_ids);

    write_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_BLOCK, fspace_ids, dcpl_id, dxpl_id,
                   data_bufs, test_mode, true, true, false);

    /* Verify space allocation status */
    verify_space_alloc_status(num_dsets, dset_ids, plist_id,
                              (mpi_size > 1) ? SOME_CHUNKS_WRITTEN : ALL_CHUNKS_WRITTEN);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");

    /* Verify the correct data was written */
    open_datasets(group_id, WRITE_UNSHARED_FILTERED_EDGE_CHUNKS_DATASET_NAME2, num_dsets, test_mode,
                  dset_ids);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        memset(read_bufs[dset_idx], 255, data_size);

    read_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_BLOCK, fspace_ids[0], dcpl_id, dxpl_id,
                  read_bufs, test_mode, true, true, false);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        VRFY((0 == memcmp(read_bufs[dset_idx], data_bufs[dset_idx], data_size)),
             "Data verification succeeded");

    for (size_t dset_idx = 0; dset_idx < MAX_NUM_DSETS_MULTI; dset_idx++) {
        free(read_bufs[dset_idx]);
        free(data_bufs_nc[dset_idx]);
    }

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        VRFY((H5Sclose(fspace_ids[dset_idx]) >= 0), "File dataspace close succeeded");
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");
    }

    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
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
                         hid_t dxpl_id, test_mode_t test_mode)
{
    const void *data_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    void       *data_bufs_nc[MAX_NUM_DSETS_MULTI] = {0}; /* non-const buffer pointers for freeing */
    void       *read_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    hsize_t     dataset_dims[WRITE_SHARED_FILTERED_EDGE_CHUNKS_DATASET_DIMS];
    hsize_t     chunk_dims[WRITE_SHARED_FILTERED_EDGE_CHUNKS_DATASET_DIMS];
    hsize_t     sel_dims[WRITE_SHARED_FILTERED_EDGE_CHUNKS_DATASET_DIMS];
    hsize_t     start[WRITE_SHARED_FILTERED_EDGE_CHUNKS_DATASET_DIMS];
    hsize_t     stride[WRITE_SHARED_FILTERED_EDGE_CHUNKS_DATASET_DIMS];
    hsize_t     count[WRITE_SHARED_FILTERED_EDGE_CHUNKS_DATASET_DIMS];
    hsize_t     block[WRITE_SHARED_FILTERED_EDGE_CHUNKS_DATASET_DIMS];
    size_t      data_size;
    size_t      num_dsets;
    hid_t       dset_ids[MAX_NUM_DSETS_MULTI];
    hid_t       fspace_ids[MAX_NUM_DSETS_MULTI];
    hid_t       file_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID;
    hid_t       group_id  = H5I_INVALID_HID;
    hid_t       filespace = H5I_INVALID_HID;

    if (MAINPROCESS)
        puts("Testing write to shared filtered edge chunks");

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

    /* Create datasets depending on the current test mode */
    create_datasets(group_id, WRITE_SHARED_FILTERED_EDGE_CHUNKS_DATASET_NAME, HDF5_DATATYPE_NAME, filespace,
                    plist_id, test_mode, &num_dsets, dset_ids);

    /* Verify space allocation status */
    verify_space_alloc_status(num_dsets, dset_ids, plist_id, DATASET_JUST_CREATED);

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

    select_hyperslab(num_dsets, dset_ids, start, stride, count, block, fspace_ids);

    /* Fill data buffer */
    data_size = sel_dims[0] * sel_dims[1] * sizeof(C_DATATYPE);

    for (size_t dset_idx = 0; dset_idx < MAX_NUM_DSETS_MULTI; dset_idx++) {
        C_DATATYPE *tmp_buf = calloc(1, data_size);
        VRFY((NULL != tmp_buf), "calloc succeeded");

        for (size_t j = 0; j < data_size / sizeof(C_DATATYPE); j++)
            tmp_buf[j] = (C_DATATYPE)(GEN_DATA(j) + dset_idx);

        data_bufs[dset_idx]    = tmp_buf;
        data_bufs_nc[dset_idx] = tmp_buf;

        tmp_buf = calloc(1, data_size);
        VRFY((NULL != tmp_buf), "calloc succeeded");

        read_bufs[dset_idx] = tmp_buf;
    }

    write_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_BLOCK, fspace_ids, dcpl_id, dxpl_id,
                   data_bufs, test_mode, true, true, false);

    /* Verify space allocation status */
    verify_space_alloc_status(num_dsets, dset_ids, plist_id, SOME_CHUNKS_WRITTEN);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");

    /* Verify the correct data was written */
    open_datasets(group_id, WRITE_SHARED_FILTERED_EDGE_CHUNKS_DATASET_NAME, num_dsets, test_mode, dset_ids);

    read_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_BLOCK, fspace_ids[0], dcpl_id, dxpl_id,
                  read_bufs, test_mode, true, true, false);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        VRFY((0 == memcmp(read_bufs[dset_idx], data_bufs[dset_idx], data_size)),
             "Data verification succeeded");

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        VRFY((H5Sclose(fspace_ids[dset_idx]) >= 0), "File dataspace close succeeded");
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");
    }

    /* Repeat the previous, but set option to not filter partial edge chunks */
    if (MAINPROCESS)
        puts("Testing write to shared unfiltered edge chunks");

    filespace = H5Screate_simple(WRITE_SHARED_FILTERED_EDGE_CHUNKS_DATASET_DIMS, dataset_dims, NULL);
    VRFY((filespace >= 0), "File dataspace creation succeeded");

    /*
     * Since we're only doing a partial write to the dataset, make
     * sure the fill time is set appropriately
     */
    VRFY((H5Pset_fill_time(plist_id, H5D_FILL_TIME_IFSET) >= 0), "H5Pset_fill_time succeeded");

    H5Pset_chunk_opts(plist_id, H5D_CHUNK_DONT_FILTER_PARTIAL_CHUNKS);

    /* Create datasets depending on the current test mode */
    create_datasets(group_id, WRITE_SHARED_FILTERED_EDGE_CHUNKS_DATASET_NAME2, HDF5_DATATYPE_NAME, filespace,
                    plist_id, test_mode, &num_dsets, dset_ids);

    /* Verify space allocation status */
    verify_space_alloc_status(num_dsets, dset_ids, plist_id, DATASET_JUST_CREATED);

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

    select_hyperslab(num_dsets, dset_ids, start, stride, count, block, fspace_ids);

    write_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_BLOCK, fspace_ids, dcpl_id, dxpl_id,
                   data_bufs, test_mode, true, true, false);

    /* Verify space allocation status */
    verify_space_alloc_status(num_dsets, dset_ids, plist_id, SOME_CHUNKS_WRITTEN);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");

    /* Verify the correct data was written */
    open_datasets(group_id, WRITE_SHARED_FILTERED_EDGE_CHUNKS_DATASET_NAME2, num_dsets, test_mode, dset_ids);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        memset(read_bufs[dset_idx], 255, data_size);

    read_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_BLOCK, fspace_ids[0], dcpl_id, dxpl_id,
                  read_bufs, test_mode, true, true, false);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        VRFY((0 == memcmp(read_bufs[dset_idx], data_bufs[dset_idx], data_size)),
             "Data verification succeeded");

    for (size_t dset_idx = 0; dset_idx < MAX_NUM_DSETS_MULTI; dset_idx++) {
        free(read_bufs[dset_idx]);
        free(data_bufs_nc[dset_idx]);
    }

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        VRFY((H5Sclose(fspace_ids[dset_idx]) >= 0), "File dataspace close succeeded");
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");
    }

    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
    VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    return;
}

/*
 * Tests that the parallel compression feature correctly handles
 * writing fill values to a dataset and reading fill values from
 * unallocated parts of a dataset.
 */
static void
test_fill_values(const char *parent_group, H5Z_filter_t filter_id, hid_t fapl_id, hid_t dcpl_id,
                 hid_t dxpl_id, test_mode_t test_mode)
{
    C_DATATYPE *correct_bufs[MAX_NUM_DSETS_MULTI] = {0};
    const void *data_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    void       *data_bufs_nc[MAX_NUM_DSETS_MULTI] = {0}; /* non-const buffer pointers for freeing */
    void       *read_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    C_DATATYPE  fill_value;
    hsize_t     dataset_dims[FILL_VALUES_TEST_DATASET_DIMS];
    hsize_t     chunk_dims[FILL_VALUES_TEST_DATASET_DIMS];
    hsize_t     sel_dims[FILL_VALUES_TEST_DATASET_DIMS];
    hsize_t     start[FILL_VALUES_TEST_DATASET_DIMS];
    hsize_t     stride[FILL_VALUES_TEST_DATASET_DIMS];
    hsize_t     count[FILL_VALUES_TEST_DATASET_DIMS];
    hsize_t     block[FILL_VALUES_TEST_DATASET_DIMS];
    size_t      data_size, read_buf_size;
    size_t      num_dsets;
    hid_t       dset_ids[MAX_NUM_DSETS_MULTI];
    hid_t       fspace_ids[MAX_NUM_DSETS_MULTI];
    hid_t       file_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID;
    hid_t       group_id   = H5I_INVALID_HID;
    hid_t       filespace  = H5I_INVALID_HID;
    int        *recvcounts = NULL;
    int        *displs     = NULL;
    int         mpi_code;

    if (MAINPROCESS)
        puts("Testing fill values");

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

    /* Make sure the fill time is set appropriately */
    VRFY((H5Pset_fill_time(plist_id, H5D_FILL_TIME_IFSET) >= 0), "H5Pset_fill_time succeeded");

    VRFY((H5Pset_chunk(plist_id, FILL_VALUES_TEST_DATASET_DIMS, chunk_dims) >= 0), "Chunk size set");

    /* Add test filter to the pipeline */
    VRFY((set_dcpl_filter(plist_id, filter_id, NULL) >= 0), "Filter set");

    /* Set a fill value */
    fill_value = FILL_VALUES_TEST_FILL_VAL;
    VRFY((H5Pset_fill_value(plist_id, HDF5_DATATYPE_NAME, &fill_value) >= 0), "Fill Value set");

    /* Create datasets depending on the current test mode */
    create_datasets(group_id, FILL_VALUES_TEST_DATASET_NAME, HDF5_DATATYPE_NAME, filespace, plist_id,
                    test_mode, &num_dsets, dset_ids);

    /* Verify space allocation status */
    verify_space_alloc_status(num_dsets, dset_ids, plist_id, DATASET_JUST_CREATED);

    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

    /* Allocate buffer for reading entire dataset */
    read_buf_size = dataset_dims[0] * dataset_dims[1] * sizeof(C_DATATYPE);

    for (size_t dset_idx = 0; dset_idx < MAX_NUM_DSETS_MULTI; dset_idx++) {
        correct_bufs[dset_idx] = calloc(1, read_buf_size);
        VRFY((NULL != correct_bufs[dset_idx]), "calloc succeeded");
        read_bufs[dset_idx] = calloc(1, read_buf_size);
        VRFY((NULL != read_bufs[dset_idx]), "calloc succeeded");
    }

    /* Read entire dataset and verify that the fill value is returned */
    read_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, dcpl_id, dxpl_id, read_bufs,
                  test_mode, true, true, true);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        for (size_t j = 0; j < read_buf_size / sizeof(C_DATATYPE); j++)
            correct_bufs[dset_idx][j] = FILL_VALUES_TEST_FILL_VAL;

        VRFY((0 == memcmp(read_bufs[dset_idx], correct_bufs[dset_idx], read_buf_size)),
             "Data verification succeeded");
    }

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

    select_hyperslab(num_dsets, dset_ids, start, stride, count, block, fspace_ids);

    /* Fill data buffer */
    data_size = sel_dims[0] * sel_dims[1] * sizeof(C_DATATYPE);

    for (size_t dset_idx = 0; dset_idx < MAX_NUM_DSETS_MULTI; dset_idx++) {
        C_DATATYPE *tmp_buf = calloc(1, data_size);
        VRFY((NULL != tmp_buf), "calloc succeeded");

        for (size_t j = 0; j < data_size / sizeof(C_DATATYPE); j++)
            tmp_buf[j] = (C_DATATYPE)(GEN_DATA(j) + dset_idx);

        data_bufs[dset_idx]    = tmp_buf;
        data_bufs_nc[dset_idx] = tmp_buf;
    }

    write_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_BLOCK, fspace_ids, dcpl_id, dxpl_id,
                   data_bufs, test_mode, true, true, false);

    /* Verify space allocation status */
    verify_space_alloc_status(num_dsets, dset_ids, plist_id, SOME_CHUNKS_WRITTEN);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");

    /* Verify correct data was written */
    open_datasets(group_id, FILL_VALUES_TEST_DATASET_NAME, num_dsets, test_mode, dset_ids);

    read_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, dcpl_id, dxpl_id, read_bufs,
                  test_mode, true, true, false);

    /*
     * Each MPI rank communicates their written piece of data
     * into each other rank's correctness-checking buffer
     */
    recvcounts = calloc(1, (size_t)mpi_size * sizeof(*recvcounts));
    VRFY((NULL != recvcounts), "calloc succeeded");

    displs = calloc(1, (size_t)mpi_size * sizeof(*displs));
    VRFY((NULL != displs), "calloc succeeded");

    for (size_t i = 0; i < (size_t)mpi_size; i++) {
        recvcounts[i] = (int)(count[1] * block[1]);
        displs[i]     = (int)(i * dataset_dims[1]);
    }

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        mpi_code = MPI_Allgatherv(data_bufs[dset_idx], recvcounts[mpi_rank], C_DATATYPE_MPI,
                                  correct_bufs[dset_idx], recvcounts, displs, C_DATATYPE_MPI, comm);
        VRFY((MPI_SUCCESS == mpi_code), "MPI_Allgatherv succeeded");

        VRFY((0 == memcmp(read_bufs[dset_idx], correct_bufs[dset_idx], read_buf_size)),
             "Data verification succeeded");
    }

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        VRFY((H5Sclose(fspace_ids[dset_idx]) >= 0), "File dataspace close succeeded");

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

    select_hyperslab(num_dsets, dset_ids, start, stride, count, block, fspace_ids);

    write_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_BLOCK, fspace_ids, dcpl_id, dxpl_id,
                   data_bufs, test_mode, true, true, true);

    /* Verify space allocation status */
    verify_space_alloc_status(num_dsets, dset_ids, plist_id, ALL_CHUNKS_WRITTEN);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");

    /* Verify correct data was written */
    open_datasets(group_id, FILL_VALUES_TEST_DATASET_NAME, num_dsets, test_mode, dset_ids);

    read_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, dcpl_id, dxpl_id, read_bufs,
                  test_mode, true, true, false);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        C_DATATYPE *tmp_buf = read_bufs[dset_idx];

        for (size_t j = 0; j < read_buf_size / sizeof(C_DATATYPE); j++)
            VRFY((tmp_buf[j] != FILL_VALUES_TEST_FILL_VAL), "Data verification succeeded");
    }

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        VRFY((H5Sclose(fspace_ids[dset_idx]) >= 0), "File dataspace close succeeded");
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");
    }

    /********************************************************************
     * Set the fill time to H5D_FILL_TIME_ALLOC and repeat the previous *
     ********************************************************************/

    filespace = H5Screate_simple(FILL_VALUES_TEST_DATASET_DIMS, dataset_dims, NULL);
    VRFY((filespace >= 0), "File dataspace creation succeeded");

    VRFY((H5Pset_fill_time(plist_id, H5D_FILL_TIME_ALLOC) >= 0), "H5Pset_fill_time succeeded");

    /* Create datasets depending on the current test mode */
    create_datasets(group_id, FILL_VALUES_TEST_DATASET_NAME2, HDF5_DATATYPE_NAME, filespace, plist_id,
                    test_mode, &num_dsets, dset_ids);

    /* Verify space allocation status */
    verify_space_alloc_status(num_dsets, dset_ids, plist_id, DATASET_JUST_CREATED);

    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

    /* Read entire dataset and verify that the fill value is returned */
    read_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, dcpl_id, dxpl_id, read_bufs,
                  test_mode, true, true, true);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        for (size_t j = 0; j < read_buf_size / sizeof(C_DATATYPE); j++)
            correct_bufs[dset_idx][j] = FILL_VALUES_TEST_FILL_VAL;

        VRFY((0 == memcmp(read_bufs[dset_idx], correct_bufs[dset_idx], read_buf_size)),
             "Data verification succeeded");
    }

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

    select_hyperslab(num_dsets, dset_ids, start, stride, count, block, fspace_ids);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        C_DATATYPE *tmp_buf = data_bufs_nc[dset_idx];

        for (size_t j = 0; j < data_size / sizeof(C_DATATYPE); j++)
            tmp_buf[j] = (C_DATATYPE)(GEN_DATA(j) + dset_idx);
    }

    write_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_BLOCK, fspace_ids, dcpl_id, dxpl_id,
                   data_bufs, test_mode, true, true, false);

    /* Verify space allocation status */
    verify_space_alloc_status(num_dsets, dset_ids, plist_id, SOME_CHUNKS_WRITTEN);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");

    /* Verify correct data was written */
    open_datasets(group_id, FILL_VALUES_TEST_DATASET_NAME2, num_dsets, test_mode, dset_ids);

    read_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, dcpl_id, dxpl_id, read_bufs,
                  test_mode, true, true, false);

    for (size_t i = 0; i < (size_t)mpi_size; i++) {
        recvcounts[i] = (int)(count[1] * block[1]);
        displs[i]     = (int)(i * dataset_dims[1]);
    }

    /*
     * Each MPI rank communicates their written piece of data
     * into each other rank's correctness-checking buffer
     */
    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        mpi_code = MPI_Allgatherv(data_bufs[dset_idx], recvcounts[mpi_rank], C_DATATYPE_MPI,
                                  correct_bufs[dset_idx], recvcounts, displs, C_DATATYPE_MPI, comm);
        VRFY((MPI_SUCCESS == mpi_code), "MPI_Allgatherv succeeded");

        VRFY((0 == memcmp(read_bufs[dset_idx], correct_bufs[dset_idx], read_buf_size)),
             "Data verification succeeded");
    }

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        VRFY((H5Sclose(fspace_ids[dset_idx]) >= 0), "File dataspace close succeeded");

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

    select_hyperslab(num_dsets, dset_ids, start, stride, count, block, fspace_ids);

    write_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_BLOCK, fspace_ids, dcpl_id, dxpl_id,
                   data_bufs, test_mode, true, true, true);

    /* Verify space allocation status */
    verify_space_alloc_status(num_dsets, dset_ids, plist_id, ALL_CHUNKS_WRITTEN);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");

    /* Verify correct data was written */
    open_datasets(group_id, FILL_VALUES_TEST_DATASET_NAME2, num_dsets, test_mode, dset_ids);

    read_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, dcpl_id, dxpl_id, read_bufs,
                  test_mode, true, true, false);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        C_DATATYPE *tmp_buf = read_bufs[dset_idx];

        for (size_t j = 0; j < read_buf_size / sizeof(C_DATATYPE); j++)
            VRFY((tmp_buf[j] != FILL_VALUES_TEST_FILL_VAL), "Data verification succeeded");
    }

    free(displs);
    free(recvcounts);

    for (size_t dset_idx = 0; dset_idx < MAX_NUM_DSETS_MULTI; dset_idx++) {
        free(data_bufs_nc[dset_idx]);
        free(read_bufs[dset_idx]);
        free(correct_bufs[dset_idx]);
    }

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        VRFY((H5Sclose(fspace_ids[dset_idx]) >= 0), "File dataspace close succeeded");
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");
    }

    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
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
                          hid_t dxpl_id, test_mode_t test_mode)
{
    H5D_alloc_time_t alloc_time;
    const void      *data_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    void            *data_bufs_nc[MAX_NUM_DSETS_MULTI] = {0}; /* non-const buffer pointers for freeing */
    void            *read_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    hsize_t          dataset_dims[FILL_VALUE_UNDEFINED_TEST_DATASET_DIMS];
    hsize_t          chunk_dims[FILL_VALUE_UNDEFINED_TEST_DATASET_DIMS];
    hsize_t          sel_dims[FILL_VALUE_UNDEFINED_TEST_DATASET_DIMS];
    hsize_t          start[FILL_VALUE_UNDEFINED_TEST_DATASET_DIMS];
    hsize_t          stride[FILL_VALUE_UNDEFINED_TEST_DATASET_DIMS];
    hsize_t          count[FILL_VALUE_UNDEFINED_TEST_DATASET_DIMS];
    hsize_t          block[FILL_VALUE_UNDEFINED_TEST_DATASET_DIMS];
    size_t           data_size, read_buf_size;
    size_t           num_dsets;
    hid_t            dset_ids[MAX_NUM_DSETS_MULTI];
    hid_t            fspace_ids[MAX_NUM_DSETS_MULTI];
    hid_t            file_id = H5I_INVALID_HID, plist_id = H5I_INVALID_HID;
    hid_t            group_id  = H5I_INVALID_HID;
    hid_t            filespace = H5I_INVALID_HID;

    if (MAINPROCESS)
        puts("Testing undefined fill value");

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

    /* Make sure the fill time is set appropriately */
    VRFY((H5Pset_fill_time(plist_id, H5D_FILL_TIME_IFSET) >= 0), "H5Pset_fill_time succeeded");

    VRFY((H5Pset_chunk(plist_id, FILL_VALUE_UNDEFINED_TEST_DATASET_DIMS, chunk_dims) >= 0), "Chunk size set");

    /* Add test filter to the pipeline */
    VRFY((set_dcpl_filter(plist_id, filter_id, NULL) >= 0), "Filter set");

    /* Set an undefined fill value */
    VRFY((H5Pset_fill_value(plist_id, HDF5_DATATYPE_NAME, NULL) >= 0), "Fill Value set");

    /* Create datasets depending on the current test mode */
    create_datasets(group_id, FILL_VALUE_UNDEFINED_TEST_DATASET_NAME, HDF5_DATATYPE_NAME, filespace, plist_id,
                    test_mode, &num_dsets, dset_ids);

    /* Verify space allocation status */
    verify_space_alloc_status(num_dsets, dset_ids, plist_id, DATASET_JUST_CREATED);

    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

    /*
     * Since we aren't writing fill values to the chunks of the
     * datasets we just created, close and re-open file to ensure
     * that file size is updated so we don't read past the end of
     * the file later if doing multi-dataset I/O.
     */
    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");
    VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, fapl_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
    VRFY((group_id >= 0), "H5Gopen2 succeeded");

    open_datasets(group_id, FILL_VALUE_UNDEFINED_TEST_DATASET_NAME, num_dsets, test_mode, dset_ids);

    /* Allocate buffer for reading entire dataset */
    read_buf_size = dataset_dims[0] * dataset_dims[1] * sizeof(C_DATATYPE);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        read_bufs[dset_idx] = calloc(1, read_buf_size);
        VRFY((NULL != read_bufs[dset_idx]), "calloc succeeded");
    }

    /*
     * Read entire dataset - nothing to verify since there's no fill value.
     * If not using early space allocation, the read should fail for filtered
     * datasets since storage isn't allocated yet and no fill value is defined.
     * For unfiltered datasets, the library will still be forcing early space
     * allocation in parallel, so the read should succeed in that case.
     */
    if (alloc_time == H5D_ALLOC_TIME_EARLY) {
        read_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, dcpl_id, dxpl_id, read_bufs,
                      test_mode, true, true, true);
    }
    else {
        for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
            herr_t expected = FAIL;
            herr_t ret;

            if (test_mode == USE_MULTIPLE_DATASETS_MIXED_FILTERED) {
                hid_t dset_dcpl;
                int   nfilters;

                dset_dcpl = H5Dget_create_plist(dset_ids[dset_idx]);
                VRFY((dset_dcpl >= 0), "H5Dget_create_plist");

                nfilters = H5Pget_nfilters(dset_dcpl);
                VRFY((nfilters >= 0), "H5Pget_nfilters");

                if (nfilters == 0)
                    expected = SUCCEED;

                VRFY((H5Pclose(dset_dcpl) >= 0), "H5Pclose");
            }

            if (expected == SUCCEED)
                ret = H5Dread(dset_ids[dset_idx], HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, dxpl_id,
                              read_bufs[dset_idx]);
            else {
                H5E_BEGIN_TRY
                {
                    ret = H5Dread(dset_ids[dset_idx], HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, dxpl_id,
                                  read_bufs[dset_idx]);
                }
                H5E_END_TRY
            }

            VRFY((ret == expected), "Dataset write");

            if (expected == SUCCEED)
                verify_chunk_opt_status(1, test_mode, true, false, true, false, false, dxpl_id);
            else
                verify_chunk_opt_status(
                    0, test_mode, false, true, true,
                    alloc_time == H5D_ALLOC_TIME_INCR || alloc_time == H5D_ALLOC_TIME_LATE, false, dxpl_id);
        }
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

    select_hyperslab(num_dsets, dset_ids, start, stride, count, block, fspace_ids);

    /* Fill data buffer */
    data_size = sel_dims[0] * sel_dims[1] * sizeof(C_DATATYPE);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        C_DATATYPE *tmp_buf = (C_DATATYPE *)calloc(1, data_size);
        VRFY((NULL != tmp_buf), "calloc succeeded");

        for (size_t j = 0; j < data_size / sizeof(C_DATATYPE); j++)
            tmp_buf[j] = (C_DATATYPE)(GEN_DATA(j) + dset_idx);

        data_bufs[dset_idx]    = tmp_buf;
        data_bufs_nc[dset_idx] = tmp_buf;
    }

    write_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_BLOCK, fspace_ids, dcpl_id, dxpl_id,
                   data_bufs, test_mode, true, true, false);

    /* Verify space allocation status */
    verify_space_alloc_status(num_dsets, dset_ids, plist_id, SOME_CHUNKS_WRITTEN);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");

    open_datasets(group_id, FILL_VALUE_UNDEFINED_TEST_DATASET_NAME, num_dsets, test_mode, dset_ids);

    read_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, dcpl_id, dxpl_id, read_bufs,
                  test_mode, true, true, false);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        VRFY((H5Sclose(fspace_ids[dset_idx]) >= 0), "File dataspace close succeeded");

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

    select_hyperslab(num_dsets, dset_ids, start, stride, count, block, fspace_ids);

    write_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_BLOCK, fspace_ids, dcpl_id, dxpl_id,
                   data_bufs, test_mode, true, true, true);

    /* Verify space allocation status */
    verify_space_alloc_status(num_dsets, dset_ids, plist_id, ALL_CHUNKS_WRITTEN);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");

    /* Verify correct data was written */
    open_datasets(group_id, FILL_VALUE_UNDEFINED_TEST_DATASET_NAME, num_dsets, test_mode, dset_ids);

    read_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, dcpl_id, dxpl_id, read_bufs,
                  test_mode, true, true, false);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        free(data_bufs_nc[dset_idx]);
        free(read_bufs[dset_idx]);
    }

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        VRFY((H5Sclose(fspace_ids[dset_idx]) >= 0), "File dataspace close succeeded");
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");
    }

    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
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
                     hid_t dxpl_id, test_mode_t test_mode)
{
    const void *data_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    void       *data_bufs_nc[MAX_NUM_DSETS_MULTI] = {0}; /* non-const buffer pointers for freeing */
    void       *read_bufs[MAX_NUM_DSETS_MULTI]    = {0};
    C_DATATYPE *fill_buf                          = NULL;
    C_DATATYPE  fill_value;
    hsize_t     dataset_dims[FILL_TIME_NEVER_TEST_DATASET_DIMS];
    hsize_t     chunk_dims[FILL_TIME_NEVER_TEST_DATASET_DIMS];
    hsize_t     sel_dims[FILL_TIME_NEVER_TEST_DATASET_DIMS];
    hsize_t     start[FILL_TIME_NEVER_TEST_DATASET_DIMS];
    hsize_t     stride[FILL_TIME_NEVER_TEST_DATASET_DIMS];
    hsize_t     count[FILL_TIME_NEVER_TEST_DATASET_DIMS];
    hsize_t     block[FILL_TIME_NEVER_TEST_DATASET_DIMS];
    size_t      data_size, read_buf_size;
    size_t      num_dsets;
    hid_t       dset_ids[MAX_NUM_DSETS_MULTI];
    hid_t       fspace_ids[MAX_NUM_DSETS_MULTI];
    hid_t       file_id   = H5I_INVALID_HID;
    hid_t       plist_id  = H5I_INVALID_HID;
    hid_t       group_id  = H5I_INVALID_HID;
    hid_t       filespace = H5I_INVALID_HID;

    if (MAINPROCESS)
        puts("Testing fill time H5D_FILL_TIME_NEVER");

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
    fill_value = FILL_TIME_NEVER_TEST_FILL_VAL;
    VRFY((H5Pset_fill_value(plist_id, HDF5_DATATYPE_NAME, &fill_value) >= 0), "Fill Value set");

    /* Set fill time of 'never' */
    VRFY((H5Pset_fill_time(plist_id, H5D_FILL_TIME_NEVER) >= 0), "H5Pset_fill_time succeeded");

    /* Create datasets depending on the current test mode */
    create_datasets(group_id, FILL_TIME_NEVER_TEST_DATASET_NAME, HDF5_DATATYPE_NAME, filespace, plist_id,
                    test_mode, &num_dsets, dset_ids);

    /* Verify space allocation status */
    verify_space_alloc_status(num_dsets, dset_ids, plist_id, DATASET_JUST_CREATED);

    VRFY((H5Sclose(filespace) >= 0), "File dataspace close succeeded");

    /* Allocate buffer for reading entire dataset */
    read_buf_size = dataset_dims[0] * dataset_dims[1] * sizeof(C_DATATYPE);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        read_bufs[dset_idx] = calloc(1, read_buf_size);
        VRFY((NULL != read_bufs[dset_idx]), "calloc succeeded");
    }

    /* Allocate buffer of fill values */
    fill_buf = calloc(1, read_buf_size);
    VRFY((NULL != fill_buf), "calloc succeeded");

    for (size_t i = 0; i < read_buf_size / sizeof(C_DATATYPE); i++)
        fill_buf[i] = FILL_TIME_NEVER_TEST_FILL_VAL;

    /*
     * Since we aren't writing fill values to the chunks of the
     * datasets we just created, close and re-open file to ensure
     * that file size is updated so we don't read past the end of
     * the file later if doing multi-dataset I/O.
     */
    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");
    VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, fapl_id);
    VRFY((file_id >= 0), "Test file open succeeded");

    group_id = H5Gopen2(file_id, parent_group, H5P_DEFAULT);
    VRFY((group_id >= 0), "H5Gopen2 succeeded");

    open_datasets(group_id, FILL_TIME_NEVER_TEST_DATASET_NAME, num_dsets, test_mode, dset_ids);

    /*
     * Read entire dataset just to try to verify bad behavior doesn't
     * occur. Don't attempt to verify the contents of the read buffer(s)
     * yet, because there's no guarantee as to what may have been
     * read from the dataset.
     */
    read_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, dcpl_id, dxpl_id, read_bufs,
                  test_mode, true, true, true);

    /*
     * Write to part of the first chunk in the dataset with
     * all ranks, then read the whole dataset just to try to
     * verify bad behavior doesn't occur. Don't attempt to
     * verify the contents of the read buffer(s) yet, because
     * there's no guarantee as to what may have been read from
     * the dataset.
     */
    count[0]  = 1;
    count[1]  = 1;
    stride[0] = (hsize_t)FILL_TIME_NEVER_TEST_CH_NROWS;
    stride[1] = (hsize_t)FILL_TIME_NEVER_TEST_CH_NCOLS;
    block[0]  = 1;
    block[1]  = (hsize_t)(FILL_TIME_NEVER_TEST_CH_NCOLS - 1);
    start[0]  = (hsize_t)mpi_rank;
    start[1]  = 0;

    select_hyperslab(num_dsets, dset_ids, start, stride, count, block, fspace_ids);

    /* Fill data buffer */
    data_size = sel_dims[0] * sel_dims[1] * sizeof(C_DATATYPE);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        C_DATATYPE *tmp_buf = calloc(1, data_size);
        VRFY((NULL != tmp_buf), "calloc succeeded");

        for (size_t j = 0; j < data_size / sizeof(C_DATATYPE); j++)
            tmp_buf[j] = (C_DATATYPE)(GEN_DATA(j) + dset_idx);

        data_bufs[dset_idx]    = tmp_buf;
        data_bufs_nc[dset_idx] = tmp_buf;
    }

    write_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_BLOCK, fspace_ids, dcpl_id, dxpl_id,
                   data_bufs, test_mode, true, true, false);

    /* Verify space allocation status */
    verify_space_alloc_status(num_dsets, dset_ids, plist_id, SOME_CHUNKS_WRITTEN);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");

    /* Verify correct data was written */
    open_datasets(group_id, FILL_TIME_NEVER_TEST_DATASET_NAME, num_dsets, test_mode, dset_ids);

    read_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, dcpl_id, dxpl_id, read_bufs,
                  test_mode, true, true, false);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        VRFY((H5Sclose(fspace_ids[dset_idx]) >= 0), "File dataspace close succeeded");

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

    select_hyperslab(num_dsets, dset_ids, start, stride, count, block, fspace_ids);

    write_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_BLOCK, fspace_ids, dcpl_id, dxpl_id,
                   data_bufs, test_mode, true, true, false);

    /* Verify space allocation status */
    verify_space_alloc_status(num_dsets, dset_ids, plist_id, ALL_CHUNKS_WRITTEN);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++)
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");

    /* Verify correct data was written */
    open_datasets(group_id, FILL_TIME_NEVER_TEST_DATASET_NAME, num_dsets, test_mode, dset_ids);

    read_datasets(num_dsets, dset_ids, HDF5_DATATYPE_NAME, H5S_ALL, H5S_ALL, dcpl_id, dxpl_id, read_bufs,
                  test_mode, true, true, false);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        C_DATATYPE *tmp_buf = read_bufs[dset_idx];

        for (size_t j = 0; j < read_buf_size / sizeof(C_DATATYPE); j++)
            VRFY((tmp_buf[j] != FILL_TIME_NEVER_TEST_FILL_VAL), "Data verification succeeded");
    }

    free(fill_buf);

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        free(data_bufs_nc[dset_idx]);
        free(read_bufs[dset_idx]);
    }

    for (size_t dset_idx = 0; dset_idx < num_dsets; dset_idx++) {
        VRFY((H5Sclose(fspace_ids[dset_idx]) >= 0), "File dataspace close succeeded");
        VRFY((H5Dclose(dset_ids[dset_idx]) >= 0), "Dataset close succeeded");
    }

    VRFY((H5Pclose(plist_id) >= 0), "DCPL close succeeded");
    VRFY((H5Gclose(group_id) >= 0), "Group close succeeded");
    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    return;
}
#endif

int
main(int argc, char **argv)
{
    unsigned seed;
    double   total_test_time  = 0.0;
    size_t   cur_filter_idx   = 0;
    size_t   num_filters      = 0;
    hid_t    file_id          = H5I_INVALID_HID;
    hid_t    fcpl_id          = H5I_INVALID_HID;
    hid_t    group_id         = H5I_INVALID_HID;
    hid_t    fapl_id          = H5I_INVALID_HID;
    hid_t    dxpl_id          = H5I_INVALID_HID;
    hid_t    dcpl_id          = H5I_INVALID_HID;
    bool     expedite_testing = false;
    int      mpi_code;

    /* Initialize MPI */
    if (MPI_SUCCESS != (mpi_code = MPI_Init(&argc, &argv))) {
        printf("Failed to initialize MPI: MPI error code %d\n", mpi_code);
        fflush(stdout);
        MPI_Abort(MPI_COMM_WORLD, -1);
    }

    if (MPI_SUCCESS != (mpi_code = MPI_Comm_size(comm, &mpi_size))) {
        printf("Failed to retrieve MPI communicator size: MPI error code %d\n", mpi_code);
        fflush(stdout);
        MPI_Abort(MPI_COMM_WORLD, -1);
    }

    if (MPI_SUCCESS != (mpi_code = MPI_Comm_rank(comm, &mpi_rank))) {
        printf("Failed to retrieve MPI communicator rank: MPI error code %d\n", mpi_code);
        fflush(stdout);
        MPI_Abort(MPI_COMM_WORLD, -1);
    }

    if (mpi_size <= 0) {
        if (MAINPROCESS) {
            printf("The Parallel Filters tests require at least 1 rank.\n");
            printf("Quitting...\n");
            fflush(stdout);
        }

        MPI_Abort(MPI_COMM_WORLD, -1);
    }

    if (H5dont_atexit() < 0) {
        if (MAINPROCESS) {
            printf("Failed to turn off atexit processing. Continue.\n");
        }
    }

    H5open();

    if (MAINPROCESS) {
        printf("==========================\n");
        printf("  Parallel Filters tests\n");
        printf("==========================\n\n");
    }

    if (VERBOSE_MED)
        h5_show_hostname();

    TestAlarmOn();

    /*
     * Get the TestExpress level setting
     */
    test_express_level_g = GetTestExpress();
    if ((test_express_level_g >= 1) && MAINPROCESS) {
        printf("** Some tests will be skipped due to TestExpress setting.\n");
        printf("** Exhaustive tests will only be performed for the first available filter.\n");
        printf("** Set the HDF5TestExpress environment variable to 0 to perform exhaustive testing for all "
               "available filters.\n\n");
    }

    /*
     * Obtain and broadcast seed value since ranks
     * aren't guaranteed to arrive here at exactly
     * the same time and could end up out of sync
     * with each other in regards to random number
     * generation
     */
    if (MAINPROCESS)
        seed = (unsigned)time(NULL);

    if (mpi_size > 1) {
        if (MPI_SUCCESS != (mpi_code = MPI_Bcast(&seed, 1, MPI_UNSIGNED, 0, comm))) {
            if (MAINPROCESS)
                printf("MPI_Bcast failed with error code %d\n", mpi_code);
            fflush(stdout);
            MPI_Abort(MPI_COMM_WORLD, -1);
        }
    }

    srand(seed);

    /* Print test settings */
    if (MAINPROCESS) {
        printf("Test Info:\n");
        printf("  MPI size: %d\n", mpi_size);
        printf("  Test express level: %d\n", test_express_level_g);
        printf("  Using seed: %u\n\n", seed);
    }

    num_filters = ARRAY_SIZE(filterIDs);

    /* Set up file access property list with parallel I/O access,
     * collective metadata reads/writes and the latest library
     * version bounds */
    fapl_id = H5Pcreate(H5P_FILE_ACCESS);
    VRFY((fapl_id >= 0), "FAPL creation succeeded");

    VRFY((H5Pset_fapl_mpio(fapl_id, comm, info) >= 0), "Set FAPL MPIO succeeded");
    VRFY((H5Pset_all_coll_metadata_ops(fapl_id, true) >= 0), "H5Pset_all_coll_metadata_ops succeeded");
    VRFY((H5Pset_coll_metadata_write(fapl_id, true) >= 0), "H5Pset_coll_metadata_write succeeded");
    VRFY((H5Pset_libver_bounds(fapl_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) >= 0),
         "Set libver bounds succeeded");

    /*
     * Set up Paged and Persistent Free Space Management
     */
    fcpl_id = H5Pcreate(H5P_FILE_CREATE);
    VRFY((fcpl_id >= 0), "FCPL creation succeeded");

    /*
     * TODO: Ideally, use persistent free space management. However,
     * this occasionally runs into an infinite loop in the library's
     * free space management code, so don't persist free space for now
     * until that is fixed.
     */
    VRFY((H5Pset_file_space_strategy(fcpl_id, H5F_FSPACE_STRATEGY_PAGE, false, 1) >= 0),
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

    /* Add a space after the HDF5_PARAPREFIX notice from h5_fixname */
    if (MAINPROCESS)
        puts("");

    /* Run tests with all available filters */
    for (cur_filter_idx = 0; cur_filter_idx < num_filters; cur_filter_idx++) {
        H5D_selection_io_mode_t sel_io_mode;
        H5Z_filter_t            cur_filter = filterIDs[cur_filter_idx];
        htri_t                  filter_avail;

        /* Make sure current filter is available before testing with it */
        filter_avail = H5Zfilter_avail(cur_filter);
        VRFY((filter_avail >= 0), "H5Zfilter_avail succeeded");

        if (!filter_avail) {
            if (MAINPROCESS)
                printf("== SKIPPED tests with filter '%s' - filter unavailable ==\n\n",
                       filterNames[cur_filter_idx]);
            continue;
        }

        /* Run tests with different selection I/O modes */
        for (sel_io_mode = H5D_SELECTION_IO_MODE_DEFAULT; sel_io_mode <= H5D_SELECTION_IO_MODE_ON;
             sel_io_mode++) {
            H5FD_mpio_chunk_opt_t chunk_opt;

            /* Run tests with both linked-chunk and multi-chunk I/O */
            for (chunk_opt = H5FD_MPIO_CHUNK_ONE_IO; chunk_opt <= H5FD_MPIO_CHUNK_MULTI_IO; chunk_opt++) {
                H5D_alloc_time_t space_alloc_time;

                /* Run tests with all available space allocation times */
                for (space_alloc_time = H5D_ALLOC_TIME_EARLY; space_alloc_time <= H5D_ALLOC_TIME_INCR;
                     space_alloc_time++) {
                    test_mode_t test_mode;

                    /* Run with each of the test modes (single dataset, multiple datasets, etc.) */
                    for (test_mode = USE_SINGLE_DATASET; test_mode < TEST_MODE_SENTINEL; test_mode++) {
                        const char *sel_io_str;
                        const char *alloc_time;
                        const char *mode;
                        unsigned    filter_config;
                        double      start_time = 0.0;
                        double      end_time   = 0.0;
                        char        group_name[512];

                        switch (sel_io_mode) {
                            case H5D_SELECTION_IO_MODE_DEFAULT:
                                sel_io_str = "default";
                                break;
                            case H5D_SELECTION_IO_MODE_OFF:
                                sel_io_str = "off";
                                break;
                            case H5D_SELECTION_IO_MODE_ON:
                                sel_io_str = "on";
                                break;
                            default:
                                sel_io_str = "unknown";
                        }

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
                            case H5D_ALLOC_TIME_DEFAULT:
                            case H5D_ALLOC_TIME_ERROR:
                            default:
                                alloc_time = "Unknown";
                        }

                        switch (test_mode) {
                            case USE_SINGLE_DATASET:
                                mode = "single";
                                break;
                            case USE_MULTIPLE_DATASETS:
                                mode = "multi";
                                break;
                            case USE_MULTIPLE_DATASETS_MIXED_FILTERED:
                                mode = "multi-mixed-filtered";
                                break;
                            case TEST_MODE_SENTINEL:
                            default:
                                mode = "unknown";
                        }

                        /*
                         * If expediting the remaining tests, just run with a single
                         * configuration that is interesting enough. In this case,
                         * run with:
                         *
                         *   - A single dataset
                         *   - Incremental file space allocation timing
                         *   - Linked-chunk (single) I/O
                         *   - The default setting for selection I/O
                         */
                        if (expedite_testing) {
                            if (test_mode != USE_SINGLE_DATASET || space_alloc_time != H5D_ALLOC_TIME_INCR ||
                                chunk_opt != H5FD_MPIO_CHUNK_ONE_IO ||
                                sel_io_mode != H5D_SELECTION_IO_MODE_DEFAULT)
                                continue;
                        }

                        /*
                         * If TestExpress is > 1, only run the multi-chunk I/O
                         * configuration tests for the 'USE_SINGLE_DATASET' case,
                         * as the 'USE_MULTIPLE_DATASETS' and 'USE_MULTIPLE_DATASETS_MIXED_FILTERED'
                         * cases are more stressful on the file system.
                         */
                        if (test_express_level_g > 1) {
                            if (((test_mode == USE_MULTIPLE_DATASETS) ||
                                 (test_mode == USE_MULTIPLE_DATASETS_MIXED_FILTERED)) &&
                                (chunk_opt != H5FD_MPIO_CHUNK_ONE_IO))
                                continue;
                        }

                        if (MAINPROCESS) {
                            printf("== Running tests in mode '%s' with filter '%s' using selection I/O mode "
                                   "'%s', '%s' and '%s' allocation time ==\n\n",
                                   test_mode_to_string(test_mode), filterNames[cur_filter_idx], sel_io_str,
                                   H5FD_MPIO_CHUNK_ONE_IO == chunk_opt ? "Linked-Chunk I/O"
                                                                       : "Multi-Chunk I/O",
                                   alloc_time);

                            start_time = MPI_Wtime();
                        }

                        /* Get the current filter's info */
                        VRFY((H5Zget_filter_info(cur_filter, &filter_config) >= 0),
                             "H5Zget_filter_info succeeded");

                        /* Determine if filter is encode-enabled */
                        if (0 == (filter_config & H5Z_FILTER_CONFIG_ENCODE_ENABLED)) {
                            if (MAINPROCESS)
                                printf(
                                    " ** SKIPPED tests with filter '%s' - filter not encode-enabled **\n\n",
                                    filterNames[cur_filter_idx]);
                            continue;
                        }

                        /* Set space allocation time */
                        VRFY((H5Pset_alloc_time(dcpl_id, space_alloc_time) >= 0),
                             "H5Pset_alloc_time succeeded");

                        /* Set selection I/O mode */
                        VRFY((H5Pset_selection_io(dxpl_id, sel_io_mode) >= 0),
                             "H5Pset_selection_io succeeded");

                        /* Set chunk I/O optimization method */
                        VRFY((H5Pset_dxpl_mpio_chunk_opt(dxpl_id, chunk_opt) >= 0),
                             "H5Pset_dxpl_mpio_chunk_opt succeeded");

                        /*
                         * Disable writing of fill values by default. Otherwise, a
                         * lot of time may be spent writing fill values to chunks
                         * when they're going to be fully overwritten anyway.
                         * Individual tests will alter this behavior as necessary.
                         */
                        VRFY((H5Pset_fill_time(dcpl_id, H5D_FILL_TIME_NEVER) >= 0),
                             "H5Pset_fill_time succeeded");

                        /* Create a group to hold all the datasets for this combination
                         * of filter and chunk optimization mode. Then, close the file
                         * again since some tests may need to open the file in a special
                         * way, like on rank 0 only */
                        file_id = H5Fopen(filenames[0], H5F_ACC_RDWR, fapl_id);
                        VRFY((file_id >= 0), "H5Fopen succeeded");

                        snprintf(group_name, sizeof(group_name), "%s_sel-io-%s_%s_%s_%s",
                                 filterNames[cur_filter_idx], sel_io_str,
                                 H5FD_MPIO_CHUNK_ONE_IO == chunk_opt ? "linked-chunk-io" : "multi-chunk-io",
                                 alloc_time, mode);

                        group_id = H5Gcreate2(file_id, group_name, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
                        VRFY((group_id >= 0), "H5Gcreate2 succeeded");

                        VRFY((H5Gclose(group_id) >= 0), "H5Gclose failed");
                        group_id = H5I_INVALID_HID;

                        VRFY((H5Fclose(file_id) >= 0), "H5Fclose succeeded");
                        file_id = H5I_INVALID_HID;

                        /* Run all tests */
                        for (size_t i = 0; i < ARRAY_SIZE(tests); i++) {
                            test_func func = tests[i];

                            if (MPI_SUCCESS == (mpi_code = MPI_Barrier(comm))) {
                                func(group_name, cur_filter, fapl_id, dcpl_id, dxpl_id, test_mode);
                            }
                            else {
                                if (MAINPROCESS)
                                    MESG("MPI_Barrier failed");
                                nerrors++;
                            }
                        }

                        if (MAINPROCESS)
                            puts("");

                        if (MAINPROCESS) {
                            end_time = MPI_Wtime();
                            total_test_time += end_time - start_time;
                            printf("Tests took %f seconds\n\n", end_time - start_time);
                        }
                    }
                }
            }
        }

        /*
         * If the TestExpress level setting isn't set for exhaustive
         * testing, run smoke checks for the other filters
         */
        if (!expedite_testing && (test_express_level_g >= 1))
            expedite_testing = true;
    }

    VRFY((H5Pclose(dcpl_id) >= 0), "DCPL close succeeded");
    dcpl_id = H5I_INVALID_HID;

    VRFY((H5Pclose(dxpl_id) >= 0), "DXPL close succeeded");
    dxpl_id = H5I_INVALID_HID;

    if (nerrors)
        goto exit;

    if (MAINPROCESS)
        printf("All Parallel Filters tests passed - total test time was %f seconds\n", total_test_time);

exit:
    if (nerrors)
        if (MAINPROCESS)
            printf("*** %d TEST ERROR%s OCCURRED ***\n", nerrors, nerrors > 1 ? "S" : "");

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
