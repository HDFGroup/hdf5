/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#include "vol_test_parallel.h"
#include "vol_test_util.h"

#include "vol_file_test_parallel.h"
#include "vol_group_test_parallel.h"
#include "vol_dataset_test_parallel.h"
#include "vol_datatype_test_parallel.h"
#include "vol_attribute_test_parallel.h"
#include "vol_link_test_parallel.h"
#include "vol_object_test_parallel.h"
#include "vol_misc_test_parallel.h"
#ifdef H5VL_TEST_HAS_ASYNC
#include "vol_async_test_parallel.h"
#endif

char vol_test_parallel_filename[VOL_TEST_FILENAME_MAX_LENGTH];

size_t n_tests_run_g;
size_t n_tests_passed_g;
size_t n_tests_failed_g;
size_t n_tests_skipped_g;

int      mpi_size, mpi_rank;
uint64_t vol_cap_flags;

/* X-macro to define the following for each test:
 * - enum type
 * - name
 * - test function
 * - enabled by default
 */
#ifdef H5VL_TEST_HAS_ASYNC
#define VOL_PARALLEL_TESTS                                                                                   \
    X(VOL_TEST_NULL, "", NULL, 0)                                                                            \
    X(VOL_TEST_FILE, "file", vol_file_test_parallel, 1)                                                      \
    X(VOL_TEST_GROUP, "group", vol_group_test_parallel, 1)                                                   \
    X(VOL_TEST_DATASET, "dataset", vol_dataset_test_parallel, 1)                                             \
    X(VOL_TEST_DATATYPE, "datatype", vol_datatype_test_parallel, 1)                                          \
    X(VOL_TEST_ATTRIBUTE, "attribute", vol_attribute_test_parallel, 1)                                       \
    X(VOL_TEST_LINK, "link", vol_link_test_parallel, 1)                                                      \
    X(VOL_TEST_OBJECT, "object", vol_object_test_parallel, 1)                                                \
    X(VOL_TEST_MISC, "misc", vol_misc_test_parallel, 1)                                                      \
    X(VOL_TEST_ASYNC, "async", vol_async_test_parallel, 1)                                                   \
    X(VOL_TEST_MAX, "", NULL, 0)
#else
#define VOL_PARALLEL_TESTS                                                                                   \
    X(VOL_TEST_NULL, "", NULL, 0)                                                                            \
    X(VOL_TEST_FILE, "file", vol_file_test_parallel, 1)                                                      \
    X(VOL_TEST_GROUP, "group", vol_group_test_parallel, 1)                                                   \
    X(VOL_TEST_DATASET, "dataset", vol_dataset_test_parallel, 1)                                             \
    X(VOL_TEST_DATATYPE, "datatype", vol_datatype_test_parallel, 1)                                          \
    X(VOL_TEST_ATTRIBUTE, "attribute", vol_attribute_test_parallel, 1)                                       \
    X(VOL_TEST_LINK, "link", vol_link_test_parallel, 1)                                                      \
    X(VOL_TEST_OBJECT, "object", vol_object_test_parallel, 1)                                                \
    X(VOL_TEST_MISC, "misc", vol_misc_test_parallel, 1)                                                      \
    X(VOL_TEST_MAX, "", NULL, 0)
#endif

#define X(a, b, c, d) a,
enum vol_test_type { VOL_PARALLEL_TESTS };
#undef X
#define X(a, b, c, d) b,
static char *const vol_test_name[] = {VOL_PARALLEL_TESTS};
#undef X
#define X(a, b, c, d) c,
static int (*vol_test_func[])(void) = {VOL_PARALLEL_TESTS};
#undef X
#define X(a, b, c, d) d,
static int vol_test_enabled[] = {VOL_PARALLEL_TESTS};
#undef X

static enum vol_test_type
vol_test_name_to_type(const char *test_name)
{
    enum vol_test_type i = 0;

    while (strcmp(vol_test_name[i], test_name) && i != VOL_TEST_MAX)
        i++;

    return ((i == VOL_TEST_MAX) ? VOL_TEST_NULL : i);
}

static void
vol_test_run(void)
{
    enum vol_test_type i;

    for (i = VOL_TEST_FILE; i < VOL_TEST_MAX; i++)
        if (vol_test_enabled[i])
            (void)vol_test_func[i]();
}

hid_t
create_mpi_fapl(MPI_Comm comm, MPI_Info info, hbool_t coll_md_read)
{
    hid_t ret_pl = H5I_INVALID_HID;

    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);

    if ((ret_pl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        goto error;

    if (H5Pset_fapl_mpio(ret_pl, comm, info) < 0)
        goto error;
    if (H5Pset_all_coll_metadata_ops(ret_pl, coll_md_read) < 0)
        goto error;
    if (H5Pset_coll_metadata_write(ret_pl, TRUE) < 0)
        goto error;

    return ret_pl;

error:
    return H5I_INVALID_HID;
} /* end create_mpi_fapl() */

/*
 * Generates random dimensions for a dataspace. The first dimension
 * is always `mpi_size` to allow for convenient subsetting; the rest
 * of the dimensions are randomized.
 */
int
generate_random_parallel_dimensions(int space_rank, hsize_t **dims_out)
{
    hsize_t *dims = NULL;
    size_t   i;

    if (space_rank <= 0)
        goto error;

    if (NULL == (dims = HDmalloc((size_t)space_rank * sizeof(hsize_t))))
        goto error;
    if (MAINPROCESS) {
        for (i = 0; i < (size_t)space_rank; i++) {
            if (i == 0)
                dims[i] = (hsize_t)mpi_size;
            else
                dims[i] = (hsize_t)((rand() % MAX_DIM_SIZE) + 1);
        }
    }

    /*
     * Ensure that the dataset dimensions are uniform across ranks.
     */
    if (MPI_SUCCESS != MPI_Bcast(dims, space_rank, MPI_UNSIGNED_LONG_LONG, 0, MPI_COMM_WORLD))
        goto error;

    *dims_out = dims;

    return 0;

error:
    if (dims)
        HDfree(dims);

    return -1;
}

static int
get_vol_cap_flags(const char *connector_name)
{
    hid_t connector_id, fapl_id;

    if ((fapl_id = H5Pcreate(H5P_FILE_ACCESS)) < 0) {
        H5_FAILED();
        HDprintf("    couldn't get the connector ID with name\n");
        goto error;
    }

    if ((connector_id = H5VLregister_connector_by_name(connector_name, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        HDprintf("    couldn't get the connector ID with name\n");
        goto error;
    }

    if (H5Pset_vol(fapl_id, connector_id, NULL) < 0) {
        H5_FAILED();
        HDprintf("    couldn't get the connector ID with name\n");
        goto error;
    }

    vol_cap_flags = 0L;

    if (H5Pget_vol_cap_flags(fapl_id, &vol_cap_flags) < 0) {
        H5_FAILED();
        HDprintf("    couldn't H5VLget_cap_flags\n");
        goto error;
    }

    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;

    if (H5VLclose(connector_id) < 0)
        TEST_ERROR;

    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(fapl_id);
        H5VLclose(connector_id);
    }
    H5E_END_TRY;

    return -1;
}

int
main(int argc, char **argv)
{
    char *vol_connector_name;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);
    MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);

    /* Simple argument checking, TODO can improve that later */
    if (argc > 1) {
        enum vol_test_type i = vol_test_name_to_type(argv[1]);
        if (i != VOL_TEST_NULL) {
            /* Run only specific VOL test */
            memset(vol_test_enabled, 0, sizeof(vol_test_enabled));
            vol_test_enabled[i] = 1;
        }
    }

    /*
     * Make sure that HDF5 is initialized on all MPI ranks before proceeding.
     * This is important for certain VOL connectors which may require a
     * collective initialization.
     */
    H5open();

    n_tests_run_g     = 0;
    n_tests_passed_g  = 0;
    n_tests_failed_g  = 0;
    n_tests_skipped_g = 0;

    srand((unsigned)HDtime(NULL));

    HDsnprintf(vol_test_parallel_filename, VOL_TEST_FILENAME_MAX_LENGTH, "%s", PARALLEL_TEST_FILE_NAME);

    if (NULL == (vol_connector_name = HDgetenv("HDF5_VOL_CONNECTOR"))) {
        if (MAINPROCESS)
            HDprintf("No VOL connector selected; using native VOL connector\n");
        vol_connector_name = "native";
    }

    if (MAINPROCESS) {
        HDprintf("Running parallel VOL tests with VOL connector '%s'\n\n", vol_connector_name);
        HDprintf("Test parameters:\n");
        HDprintf("  - Test file name: '%s'\n", vol_test_parallel_filename);
        HDprintf("  - Number of MPI ranks: %d\n", mpi_size);
        HDprintf("\n\n");
    }

    /*
     * Create the file that will be used for all of the tests,
     * except for those which test file creation.
     */
    BEGIN_INDEPENDENT_OP(create_test_container)
    {
        if (MAINPROCESS) {
            if (create_test_container(vol_test_parallel_filename) < 0) {
                HDprintf("    failed to create testing container file '%s'\n", vol_test_parallel_filename);
                INDEPENDENT_OP_ERROR(create_test_container);
            }
        }
    }
    END_INDEPENDENT_OP(create_test_container);

    /* Retrieve the VOL cap flags */
    BEGIN_INDEPENDENT_OP(get_capacity_flags)
    {
        if (get_vol_cap_flags(vol_connector_name) < 0) {
            if (MAINPROCESS)
                HDfprintf(stderr, "Unable to get VOL capacity flags\n");
            INDEPENDENT_OP_ERROR(get_capacity_flags);
        }
    }
    END_INDEPENDENT_OP(get_capacity_flags);

    /* Run all the tests that are enabled */
    vol_test_run();

    if (MAINPROCESS)
        HDprintf("The below statistics are minimum values due to the possibility of some ranks failing a "
                 "test while others pass:\n");

    if (MPI_SUCCESS !=
        MPI_Allreduce(MPI_IN_PLACE, &n_tests_passed_g, 1, MPI_UNSIGNED_LONG_LONG, MPI_MIN, MPI_COMM_WORLD)) {
        if (MAINPROCESS)
            HDprintf("    failed to collect consensus about the minimum number of tests that passed -- "
                     "reporting rank 0's (possibly inaccurate) value\n");
    }

    if (MAINPROCESS)
        HDprintf("%s%ld/%ld (%.2f%%) VOL tests passed across all ranks with VOL connector '%s'\n",
                 n_tests_passed_g > 0 ? "At least " : "", (long)n_tests_passed_g, (long)n_tests_run_g,
                 ((float)n_tests_passed_g / (float)n_tests_run_g * 100.0), vol_connector_name);

    if (MPI_SUCCESS !=
        MPI_Allreduce(MPI_IN_PLACE, &n_tests_failed_g, 1, MPI_UNSIGNED_LONG_LONG, MPI_MIN, MPI_COMM_WORLD)) {
        if (MAINPROCESS)
            HDprintf("    failed to collect consensus about the minimum number of tests that failed -- "
                     "reporting rank 0's (possibly inaccurate) value\n");
    }

    if (MAINPROCESS) {
        HDprintf("%s%ld/%ld (%.2f%%) VOL tests did not pass across all ranks with VOL connector '%s'\n",
                 n_tests_failed_g > 0 ? "At least " : "", (long)n_tests_failed_g, (long)n_tests_run_g,
                 ((float)n_tests_failed_g / (float)n_tests_run_g * 100.0), vol_connector_name);

        HDprintf("%ld/%ld (%.2f%%) VOL tests were skipped with VOL connector '%s'\n", (long)n_tests_skipped_g,
                 (long)n_tests_run_g, ((float)n_tests_skipped_g / (float)n_tests_run_g * 100.0),
                 vol_connector_name);
    }

    H5close();

    MPI_Finalize();

    HDexit(EXIT_SUCCESS);

error:
    MPI_Finalize();

    HDexit(EXIT_FAILURE);
}
