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
 * to test a specified HDF5 VOL connector or set of VOL connectors. This
 * test suite must assume that a VOL connector could only implement the File
 * interface. Therefore, the suite should check that a particular piece of
 * functionality is supported by the VOL connector before actually testing
 * it. If the functionality is not supported, the test should simply be
 * skipped, perhaps with a note as to why the test was skipped, if possible.
 *
 * If the VOL connector being used supports the creation of groups, this
 * test suite will attempt to organize the output of these various tests
 * into groups based on their respective interface.
 */

#include "vol_test.h"
#include "vol_test_util.h"

#include "vol_file_test.h"
#include "vol_group_test.h"
#include "vol_dataset_test.h"
#include "vol_datatype_test.h"
#include "vol_attribute_test.h"
#include "vol_link_test.h"
#include "vol_object_test.h"
#include "vol_misc_test.h"
#ifdef H5VL_TEST_HAS_ASYNC
#include "vol_async_test.h"
#endif

char vol_test_filename[VOL_TEST_FILENAME_MAX_LENGTH];

size_t n_tests_run_g;
size_t n_tests_passed_g;
size_t n_tests_failed_g;
size_t n_tests_skipped_g;

uint64_t vol_cap_flags;

/* X-macro to define the following for each test:
 * - enum type
 * - name
 * - test function
 * - enabled by default
 */
#ifdef H5VL_TEST_HAS_ASYNC
#define VOL_TESTS                                                                                            \
    X(VOL_TEST_NULL, "", NULL, 0)                                                                            \
    X(VOL_TEST_FILE, "file", vol_file_test, 1)                                                               \
    X(VOL_TEST_GROUP, "group", vol_group_test, 1)                                                            \
    X(VOL_TEST_DATASET, "dataset", vol_dataset_test, 1)                                                      \
    X(VOL_TEST_DATATYPE, "datatype", vol_datatype_test, 1)                                                   \
    X(VOL_TEST_ATTRIBUTE, "attribute", vol_attribute_test, 1)                                                \
    X(VOL_TEST_LINK, "link", vol_link_test, 1)                                                               \
    X(VOL_TEST_OBJECT, "object", vol_object_test, 1)                                                         \
    X(VOL_TEST_MISC, "misc", vol_misc_test, 1)                                                               \
    X(VOL_TEST_ASYNC, "async", vol_async_test, 1)                                                            \
    X(VOL_TEST_MAX, "", NULL, 0)
#else
#define VOL_TESTS                                                                                            \
    X(VOL_TEST_NULL, "", NULL, 0)                                                                            \
    X(VOL_TEST_FILE, "file", vol_file_test, 1)                                                               \
    X(VOL_TEST_GROUP, "group", vol_group_test, 1)                                                            \
    X(VOL_TEST_DATASET, "dataset", vol_dataset_test, 1)                                                      \
    X(VOL_TEST_DATATYPE, "datatype", vol_datatype_test, 1)                                                   \
    X(VOL_TEST_ATTRIBUTE, "attribute", vol_attribute_test, 1)                                                \
    X(VOL_TEST_LINK, "link", vol_link_test, 1)                                                               \
    X(VOL_TEST_OBJECT, "object", vol_object_test, 1)                                                         \
    X(VOL_TEST_MISC, "misc", vol_misc_test, 1)                                                               \
    X(VOL_TEST_MAX, "", NULL, 0)
#endif

#define X(a, b, c, d) a,
enum vol_test_type { VOL_TESTS };
#undef X
#define X(a, b, c, d) b,
static char *const vol_test_name[] = {VOL_TESTS};
#undef X
#define X(a, b, c, d) c,
static int (*vol_test_func[])(void) = {VOL_TESTS};
#undef X
#define X(a, b, c, d) d,
static int vol_test_enabled[] = {VOL_TESTS};
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
        TEST_ERROR

    if (H5VLclose(connector_id) < 0)
        TEST_ERROR

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
    char   *vol_connector_name;
    hbool_t err_occurred = FALSE;

    /* Simple argument checking, TODO can improve that later */
    if (argc > 1) {
        enum vol_test_type i = vol_test_name_to_type(argv[1]);
        if (i != VOL_TEST_NULL) {
            /* Run only specific VOL test */
            memset(vol_test_enabled, 0, sizeof(vol_test_enabled));
            vol_test_enabled[i] = 1;
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

    srand((unsigned)HDtime(NULL));

    HDsnprintf(vol_test_filename, VOL_TEST_FILENAME_MAX_LENGTH, "%s", TEST_FILE_NAME);

    if (NULL == (vol_connector_name = HDgetenv("HDF5_VOL_CONNECTOR"))) {
        HDprintf("No VOL connector selected; using native VOL connector\n");
        vol_connector_name = "native";
    }

    HDprintf("Running VOL tests with VOL connector '%s'\n\n", vol_connector_name);
    HDprintf("Test parameters:\n");
    HDprintf("  - Test file name: '%s'\n", vol_test_filename);
    HDprintf("\n\n");

    /*
     * Create the file that will be used for all of the tests,
     * except for those which test file creation.
     */
    if (create_test_container(vol_test_filename) < 0) {
        HDfprintf(stderr, "Unable to create testing container file '%s'\n", vol_test_filename);
        err_occurred = TRUE;
        goto done;
    }

    /* Retrieve the VOL cap flags */
    if (get_vol_cap_flags(vol_connector_name) < 0) {
        HDfprintf(stderr, "Unable to get VOL capacity flags\n");
        err_occurred = TRUE;
        goto done;
    }

    /* Run all the tests that are enabled */
    vol_test_run();

    HDprintf("Cleaning up testing files\n");
    H5Fdelete(vol_test_filename, H5P_DEFAULT);

    HDprintf("%ld/%ld (%.2f%%) VOL tests passed with VOL connector '%s'\n", (long)n_tests_passed_g,
             (long)n_tests_run_g, ((float)n_tests_passed_g / (float)n_tests_run_g * 100.0),
             vol_connector_name);
    HDprintf("%ld/%ld (%.2f%%) VOL tests did not pass with VOL connector '%s'\n", (long)n_tests_failed_g,
             (long)n_tests_run_g, ((float)n_tests_failed_g / (float)n_tests_run_g * 100.0),
             vol_connector_name);
    HDprintf("%ld/%ld (%.2f%%) VOL tests were skipped with VOL connector '%s'\n", (long)n_tests_skipped_g,
             (long)n_tests_run_g, ((float)n_tests_skipped_g / (float)n_tests_run_g * 100.0),
             vol_connector_name);

done:
    H5close();

#ifdef H5_HAVE_PARALLEL
    MPI_Finalize();
#endif

    HDexit(((err_occurred || n_tests_failed_g > 0) ? EXIT_FAILURE : EXIT_SUCCESS));
}
