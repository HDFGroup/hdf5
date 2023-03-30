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

#include "H5_api_test_util.h"
#include "H5_api_test_parallel.h"

#include "H5_api_attribute_test_parallel.h"
#include "H5_api_dataset_test_parallel.h"
#include "H5_api_datatype_test_parallel.h"
#include "H5_api_file_test_parallel.h"
#include "H5_api_group_test_parallel.h"
#include "H5_api_link_test_parallel.h"
#include "H5_api_misc_test_parallel.h"
#include "H5_api_object_test_parallel.h"
#ifdef H5_API_TEST_HAS_ASYNC
#include "H5_api_async_test_parallel.h"
#endif

char H5_api_test_parallel_filename[H5_API_TEST_FILENAME_MAX_LENGTH];

size_t n_tests_run_g;
size_t n_tests_passed_g;
size_t n_tests_failed_g;
size_t n_tests_skipped_g;

int mpi_size;
int mpi_rank;

/* X-macro to define the following for each test:
 * - enum type
 * - name
 * - test function
 * - enabled by default
 */
#ifdef H5_API_TEST_HAS_ASYNC
#define H5_API_PARALLEL_TESTS                                                                                \
    X(H5_API_TEST_NULL, "", NULL, 0)                                                                         \
    X(H5_API_TEST_FILE, "file", H5_api_file_test_parallel, 1)                                                \
    X(H5_API_TEST_GROUP, "group", H5_api_group_test_parallel, 1)                                             \
    X(H5_API_TEST_DATASET, "dataset", H5_api_dataset_test_parallel, 1)                                       \
    X(H5_API_TEST_DATATYPE, "datatype", H5_api_datatype_test_parallel, 1)                                    \
    X(H5_API_TEST_ATTRIBUTE, "attribute", H5_api_attribute_test_parallel, 1)                                 \
    X(H5_API_TEST_LINK, "link", H5_api_link_test_parallel, 1)                                                \
    X(H5_API_TEST_OBJECT, "object", H5_api_object_test_parallel, 1)                                          \
    X(H5_API_TEST_MISC, "misc", H5_api_misc_test_parallel, 1)                                                \
    X(H5_API_TEST_ASYNC, "async", H5_api_async_test_parallel, 1)                                             \
    X(H5_API_TEST_MAX, "", NULL, 0)
#else
#define H5_API_PARALLEL_TESTS                                                                                \
    X(H5_API_TEST_NULL, "", NULL, 0)                                                                         \
    X(H5_API_TEST_FILE, "file", H5_api_file_test_parallel, 1)                                                \
    X(H5_API_TEST_GROUP, "group", H5_api_group_test_parallel, 1)                                             \
    X(H5_API_TEST_DATASET, "dataset", H5_api_dataset_test_parallel, 1)                                       \
    X(H5_API_TEST_DATATYPE, "datatype", H5_api_datatype_test_parallel, 1)                                    \
    X(H5_API_TEST_ATTRIBUTE, "attribute", H5_api_attribute_test_parallel, 1)                                 \
    X(H5_API_TEST_LINK, "link", H5_api_link_test_parallel, 1)                                                \
    X(H5_API_TEST_OBJECT, "object", H5_api_object_test_parallel, 1)                                          \
    X(H5_API_TEST_MISC, "misc", H5_api_misc_test_parallel, 1)                                                \
    X(H5_API_TEST_MAX, "", NULL, 0)
#endif

#define X(a, b, c, d) a,
enum H5_api_test_type { H5_API_PARALLEL_TESTS };
#undef X
#define X(a, b, c, d) b,
static const char *const H5_api_test_name[] = {H5_API_PARALLEL_TESTS};
#undef X
#define X(a, b, c, d) c,
static int (*H5_api_test_func[])(void) = {H5_API_PARALLEL_TESTS};
#undef X
#define X(a, b, c, d) d,
static int H5_api_test_enabled[] = {H5_API_PARALLEL_TESTS};
#undef X

static enum H5_api_test_type
H5_api_test_name_to_type(const char *test_name)
{
    enum H5_api_test_type i = 0;

    while (strcmp(H5_api_test_name[i], test_name) && i != H5_API_TEST_MAX)
        i++;

    return ((i == H5_API_TEST_MAX) ? H5_API_TEST_NULL : i);
}

static void
H5_api_test_run(void)
{
    enum H5_api_test_type i;

    for (i = H5_API_TEST_FILE; i < H5_API_TEST_MAX; i++)
        if (H5_api_test_enabled[i])
            (void)H5_api_test_func[i]();
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
    hid_t connector_id = H5I_INVALID_HID;
    hid_t fapl_id      = H5I_INVALID_HID;

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

    vol_cap_flags_g = H5VL_CAP_FLAG_NONE;

    if (H5Pget_vol_cap_flags(fapl_id, &vol_cap_flags_g) < 0) {
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
    const char *vol_connector_name;
    unsigned    seed;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);
    MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);

    /* Simple argument checking, TODO can improve that later */
    if (argc > 1) {
        enum H5_api_test_type i = H5_api_test_name_to_type(argv[1]);
        if (i != H5_API_TEST_NULL) {
            /* Run only specific API test */
            memset(H5_api_test_enabled, 0, sizeof(H5_api_test_enabled));
            H5_api_test_enabled[i] = 1;
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

    if (MAINPROCESS) {
        seed = (unsigned)HDtime(NULL);
    }

    if (mpi_size > 1) {
        if (MPI_SUCCESS != MPI_Bcast(&seed, 1, MPI_UNSIGNED, 0, MPI_COMM_WORLD)) {
            if (MAINPROCESS)
                HDprintf("Couldn't broadcast test seed\n");
            goto error;
        }
    }

    srand(seed);

    HDsnprintf(H5_api_test_parallel_filename, H5_API_TEST_FILENAME_MAX_LENGTH, "%s", PARALLEL_TEST_FILE_NAME);

    if (NULL == (vol_connector_name = HDgetenv(HDF5_VOL_CONNECTOR))) {
        if (MAINPROCESS)
            HDprintf("No VOL connector selected; using native VOL connector\n");
        vol_connector_name = "native";
    }

    if (MAINPROCESS) {
        HDprintf("Running parallel API tests with VOL connector '%s'\n\n", vol_connector_name);
        HDprintf("Test parameters:\n");
        HDprintf("  - Test file name: '%s'\n", H5_api_test_parallel_filename);
        HDprintf("  - Number of MPI ranks: %d\n", mpi_size);
        HDprintf("  - Test seed: %u\n", seed);
        HDprintf("\n\n");
    }

    /*
     * Create the file that will be used for all of the tests,
     * except for those which test file creation.
     */
    BEGIN_INDEPENDENT_OP(create_test_container)
    {
        if (MAINPROCESS) {
            if (create_test_container(H5_api_test_parallel_filename) < 0) {
                HDprintf("    failed to create testing container file '%s'\n", H5_api_test_parallel_filename);
                INDEPENDENT_OP_ERROR(create_test_container);
            }
        }
    }
    END_INDEPENDENT_OP(create_test_container);

    /* Retrieve the VOL cap flags */
    BEGIN_INDEPENDENT_OP(get_capability_flags)
    {
        if (get_vol_cap_flags(vol_connector_name) < 0) {
            if (MAINPROCESS)
                HDfprintf(stderr, "Unable to get VOL capability flags\n");
            INDEPENDENT_OP_ERROR(get_capability_flags);
        }
    }
    END_INDEPENDENT_OP(get_capability_flags);

    /* Run all the tests that are enabled */
    H5_api_test_run();

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
        HDprintf("%s%zu/%zu (%.2f%%) API tests passed across all ranks with VOL connector '%s'\n",
                 n_tests_passed_g > 0 ? "At least " : "", n_tests_passed_g, n_tests_run_g,
                 ((double)n_tests_passed_g / (double)n_tests_run_g * 100.0), vol_connector_name);

    if (MPI_SUCCESS !=
        MPI_Allreduce(MPI_IN_PLACE, &n_tests_failed_g, 1, MPI_UNSIGNED_LONG_LONG, MPI_MIN, MPI_COMM_WORLD)) {
        if (MAINPROCESS)
            HDprintf("    failed to collect consensus about the minimum number of tests that failed -- "
                     "reporting rank 0's (possibly inaccurate) value\n");
    }

    if (MAINPROCESS) {
        HDprintf("%s%zu/%zu (%.2f%%) API tests did not pass across all ranks with VOL connector '%s'\n",
                 n_tests_failed_g > 0 ? "At least " : "", n_tests_failed_g, n_tests_run_g,
                 ((double)n_tests_failed_g / (double)n_tests_run_g * 100.0), vol_connector_name);

        HDprintf("%zu/%zu (%.2f%%) API tests were skipped with VOL connector '%s'\n", n_tests_skipped_g,
                 n_tests_run_g, ((double)n_tests_skipped_g / (double)n_tests_run_g * 100.0),
                 vol_connector_name);
    }

    H5close();

    MPI_Finalize();

    HDexit(EXIT_SUCCESS);

error:
    MPI_Finalize();

    HDexit(EXIT_FAILURE);
}
