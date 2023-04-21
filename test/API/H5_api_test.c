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

/*
 * A test suite which only makes public HDF5 API calls and which is meant
 * to test the native VOL connector or a specified HDF5 VOL connector (or
 * set of connectors stacked with each other). This test suite must assume
 * that a VOL connector could only implement the File interface. Therefore,
 * the suite should check that a particular piece of functionality is supported
 * by the VOL connector before actually testing it. If the functionality is
 * not supported, the test should simply be skipped, perhaps with a note as
 * to why the test was skipped, if possible.
 *
 * If the VOL connector being used supports the creation of groups, this
 * test suite will attempt to organize the output of these various tests
 * into groups based on their respective HDF5 interface.
 */

#include "H5_api_test.h"

#include "H5_api_attribute_test.h"
#include "H5_api_dataset_test.h"
#include "H5_api_datatype_test.h"
#include "H5_api_file_test.h"
#include "H5_api_group_test.h"
#include "H5_api_link_test.h"
#include "H5_api_misc_test.h"
#include "H5_api_object_test.h"
#include "H5_api_test_util.h"
#ifdef H5_API_TEST_HAVE_ASYNC
#include "H5_api_async_test.h"
#endif

char H5_api_test_filename[H5_API_TEST_FILENAME_MAX_LENGTH];

const char *test_path_prefix;

/* X-macro to define the following for each test:
 * - enum type
 * - name
 * - test function
 * - enabled by default
 */
#ifdef H5_API_TEST_HAVE_ASYNC
#define H5_API_TESTS                                                                                         \
    X(H5_API_TEST_NULL, "", NULL, 0)                                                                         \
    X(H5_API_TEST_FILE, "file", H5_api_file_test, 1)                                                         \
    X(H5_API_TEST_GROUP, "group", H5_api_group_test, 1)                                                      \
    X(H5_API_TEST_DATASET, "dataset", H5_api_dataset_test, 1)                                                \
    X(H5_API_TEST_DATATYPE, "datatype", H5_api_datatype_test, 1)                                             \
    X(H5_API_TEST_ATTRIBUTE, "attribute", H5_api_attribute_test, 1)                                          \
    X(H5_API_TEST_LINK, "link", H5_api_link_test, 1)                                                         \
    X(H5_API_TEST_OBJECT, "object", H5_api_object_test, 1)                                                   \
    X(H5_API_TEST_MISC, "misc", H5_api_misc_test, 1)                                                         \
    X(H5_API_TEST_ASYNC, "async", H5_api_async_test, 1)                                                      \
    X(H5_API_TEST_MAX, "", NULL, 0)
#else
#define H5_API_TESTS                                                                                         \
    X(H5_API_TEST_NULL, "", NULL, 0)                                                                         \
    X(H5_API_TEST_FILE, "file", H5_api_file_test, 1)                                                         \
    X(H5_API_TEST_GROUP, "group", H5_api_group_test, 1)                                                      \
    X(H5_API_TEST_DATASET, "dataset", H5_api_dataset_test, 1)                                                \
    X(H5_API_TEST_DATATYPE, "datatype", H5_api_datatype_test, 1)                                             \
    X(H5_API_TEST_ATTRIBUTE, "attribute", H5_api_attribute_test, 1)                                          \
    X(H5_API_TEST_LINK, "link", H5_api_link_test, 1)                                                         \
    X(H5_API_TEST_OBJECT, "object", H5_api_object_test, 1)                                                   \
    X(H5_API_TEST_MISC, "misc", H5_api_misc_test, 1)                                                         \
    X(H5_API_TEST_MAX, "", NULL, 0)
#endif

#define X(a, b, c, d) a,
enum H5_api_test_type { H5_API_TESTS };
#undef X
#define X(a, b, c, d) b,
static const char *const H5_api_test_name[] = {H5_API_TESTS};
#undef X
#define X(a, b, c, d) c,
static int (*H5_api_test_func[])(void) = {H5_API_TESTS};
#undef X
#define X(a, b, c, d) d,
static int H5_api_test_enabled[] = {H5_API_TESTS};
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

/******************************************************************************/

int
main(int argc, char **argv)
{
    const char *vol_connector_name;
    unsigned    seed;
    hbool_t     err_occurred = FALSE;

    /* Simple argument checking, TODO can improve that later */
    if (argc > 1) {
        enum H5_api_test_type i = H5_api_test_name_to_type(argv[1]);
        if (i != H5_API_TEST_NULL) {
            /* Run only specific API test */
            memset(H5_api_test_enabled, 0, sizeof(H5_api_test_enabled));
            H5_api_test_enabled[i] = 1;
        }
    }

#ifdef H5_HAVE_PARALLEL
    /* If HDF5 was built with parallel enabled, go ahead and call MPI_Init before
     * running these tests. Even though these are meant to be serial tests, they will
     * likely be run using mpirun (or similar) and we cannot necessarily expect HDF5 or
     * an HDF5 VOL connector to call MPI_Init.
     */
    MPI_Init(&argc, &argv);
#endif

    /* h5_reset(); */

    n_tests_run_g     = 0;
    n_tests_passed_g  = 0;
    n_tests_failed_g  = 0;
    n_tests_skipped_g = 0;

    seed = (unsigned)HDtime(NULL);
    srand(seed);

    if (NULL == (test_path_prefix = HDgetenv(HDF5_API_TEST_PATH_PREFIX)))
        test_path_prefix = "";

    HDsnprintf(H5_api_test_filename, H5_API_TEST_FILENAME_MAX_LENGTH, "%s%s", test_path_prefix,
               TEST_FILE_NAME);

    if (NULL == (vol_connector_name = HDgetenv(HDF5_VOL_CONNECTOR))) {
        HDprintf("No VOL connector selected; using native VOL connector\n");
        vol_connector_name = "native";
    }

    HDprintf("Running API tests with VOL connector '%s'\n\n", vol_connector_name);
    HDprintf("Test parameters:\n");
    HDprintf("  - Test file name: '%s'\n", H5_api_test_filename);
    HDprintf("  - Test seed: %u\n", seed);
    HDprintf("\n\n");

    /* Retrieve the VOL cap flags */
    if (get_vol_cap_flags(vol_connector_name) < 0) {
        HDfprintf(stderr, "Unable to get VOL capability flags\n");
        err_occurred = TRUE;
        goto done;
    }

    /*
     * Create the file that will be used for all of the tests,
     * except for those which test file creation.
     */
    if (create_test_container(H5_api_test_filename, vol_cap_flags_g) < 0) {
        HDfprintf(stderr, "Unable to create testing container file '%s'\n", H5_api_test_filename);
        err_occurred = TRUE;
        goto done;
    }

    /* Run all the tests that are enabled */
    H5_api_test_run();

    HDprintf("Cleaning up testing files\n");
    H5Fdelete(H5_api_test_filename, H5P_DEFAULT);

    if (n_tests_run_g > 0) {
        HDprintf("%zu/%zu (%.2f%%) API tests passed with VOL connector '%s'\n", n_tests_passed_g,
                 n_tests_run_g, ((double)n_tests_passed_g / (double)n_tests_run_g * 100.0),
                 vol_connector_name);
        HDprintf("%zu/%zu (%.2f%%) API tests did not pass with VOL connector '%s'\n", n_tests_failed_g,
                 n_tests_run_g, ((double)n_tests_failed_g / (double)n_tests_run_g * 100.0),
                 vol_connector_name);
        HDprintf("%zu/%zu (%.2f%%) API tests were skipped with VOL connector '%s'\n", n_tests_skipped_g,
                 n_tests_run_g, ((double)n_tests_skipped_g / (double)n_tests_run_g * 100.0),
                 vol_connector_name);
    }

done:
    H5close();

#ifdef H5_HAVE_PARALLEL
    MPI_Finalize();
#endif

    HDexit(((err_occurred || n_tests_failed_g > 0) ? EXIT_FAILURE : EXIT_SUCCESS));
}
