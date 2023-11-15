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

/******************************************************************************/

int
main(int argc, char **argv)
{
    const char *vol_connector_string;
    const char *vol_connector_name;
    unsigned    seed;
    hid_t       fapl_id                   = H5I_INVALID_HID;
    hid_t       default_con_id            = H5I_INVALID_HID;
    hid_t       registered_con_id         = H5I_INVALID_HID;
    char       *vol_connector_string_copy = NULL;
    char       *vol_connector_info        = NULL;
    bool        err_occurred              = false;

    /* Simple argument checking, TODO can improve that later */
    if (argc > 1) {
        enum H5_api_test_type i = H5_api_test_name_to_type(argv[1]);
        if (i != H5_API_TEST_NULL) {
            /* Run only specific API test */
            memset(H5_api_test_enabled, 0, sizeof(H5_api_test_enabled));
            H5_api_test_enabled[i] = 1;
        }
    }

    H5open();

    n_tests_run_g     = 0;
    n_tests_passed_g  = 0;
    n_tests_failed_g  = 0;
    n_tests_skipped_g = 0;

    seed = (unsigned)HDtime(NULL);
    srand(seed);

    if (NULL == (test_path_prefix = getenv(HDF5_API_TEST_PATH_PREFIX)))
        test_path_prefix = "";

    snprintf(H5_api_test_filename, H5_API_TEST_FILENAME_MAX_LENGTH, "%s%s", test_path_prefix, TEST_FILE_NAME);

    if (NULL == (vol_connector_string = getenv(HDF5_VOL_CONNECTOR))) {
        printf("No VOL connector selected; using native VOL connector\n");
        vol_connector_name = "native";
        vol_connector_info = NULL;
    }
    else {
        char *token;

        if (NULL == (vol_connector_string_copy = strdup(vol_connector_string))) {
            fprintf(stderr, "Unable to copy VOL connector string\n");
            err_occurred = true;
            goto done;
        }

        if (NULL == (token = strtok(vol_connector_string_copy, " "))) {
            fprintf(stderr, "Error while parsing VOL connector string\n");
            err_occurred = true;
            goto done;
        }

        vol_connector_name = token;

        if (NULL != (token = strtok(NULL, " "))) {
            vol_connector_info = token;
        }
    }

    printf("Running API tests with VOL connector '%s' and info string '%s'\n\n", vol_connector_name,
           vol_connector_info ? vol_connector_info : "");
    printf("Test parameters:\n");
    printf("  - Test file name: '%s'\n", H5_api_test_filename);
    printf("  - Test seed: %u\n", seed);
    printf("\n\n");

    if ((fapl_id = H5Pcreate(H5P_FILE_ACCESS)) < 0) {
        fprintf(stderr, "Unable to create FAPL\n");
        err_occurred = true;
        goto done;
    }

    /*
     * If using a VOL connector other than the native
     * connector, check whether the VOL connector was
     * successfully registered before running the tests.
     * Otherwise, HDF5 will default to running the tests
     * with the native connector, which could be misleading.
     */
    if (0 != strcmp(vol_connector_name, "native")) {
        htri_t is_registered;

        if ((is_registered = H5VLis_connector_registered_by_name(vol_connector_name)) < 0) {
            fprintf(stderr, "Unable to determine if VOL connector is registered\n");
            err_occurred = true;
            goto done;
        }

        if (!is_registered) {
            fprintf(stderr, "Specified VOL connector '%s' wasn't correctly registered!\n",
                    vol_connector_name);
            err_occurred = true;
            goto done;
        }
        else {
            /*
             * If the connector was successfully registered, check that
             * the connector ID set on the default FAPL matches the ID
             * for the registered connector before running the tests.
             */
            if (H5Pget_vol_id(fapl_id, &default_con_id) < 0) {
                fprintf(stderr, "Couldn't retrieve ID of VOL connector set on default FAPL\n");
                err_occurred = true;
                goto done;
            }

            if ((registered_con_id = H5VLget_connector_id_by_name(vol_connector_name)) < 0) {
                fprintf(stderr, "Couldn't retrieve ID of registered VOL connector\n");
                err_occurred = true;
                goto done;
            }

            if (default_con_id != registered_con_id) {
                fprintf(stderr, "VOL connector set on default FAPL didn't match specified VOL connector\n");
                err_occurred = true;
                goto done;
            }
        }
    }

    /* Retrieve the VOL cap flags - work around an HDF5
     * library issue by creating a FAPL
     */
    vol_cap_flags_g = H5VL_CAP_FLAG_NONE;
    if (H5Pget_vol_cap_flags(fapl_id, &vol_cap_flags_g) < 0) {
        fprintf(stderr, "Unable to retrieve VOL connector capability flags\n");
        err_occurred = true;
        goto done;
    }

    /*
     * Create the file that will be used for all of the tests,
     * except for those which test file creation.
     */
    if (create_test_container(H5_api_test_filename, vol_cap_flags_g) < 0) {
        fprintf(stderr, "Unable to create testing container file '%s'\n", H5_api_test_filename);
        err_occurred = true;
        goto done;
    }

    /* Run all the tests that are enabled */
    H5_api_test_run();

    printf("Cleaning up testing files\n");
    H5Fdelete(H5_api_test_filename, fapl_id);

    if (n_tests_run_g > 0) {
        printf("%zu/%zu (%.2f%%) API tests passed with VOL connector '%s'\n", n_tests_passed_g, n_tests_run_g,
               ((double)n_tests_passed_g / (double)n_tests_run_g * 100.0), vol_connector_name);
        printf("%zu/%zu (%.2f%%) API tests did not pass with VOL connector '%s'\n", n_tests_failed_g,
               n_tests_run_g, ((double)n_tests_failed_g / (double)n_tests_run_g * 100.0), vol_connector_name);
        printf("%zu/%zu (%.2f%%) API tests were skipped with VOL connector '%s'\n", n_tests_skipped_g,
               n_tests_run_g, ((double)n_tests_skipped_g / (double)n_tests_run_g * 100.0),
               vol_connector_name);
    }

done:
    free(vol_connector_string_copy);

    if (default_con_id >= 0 && H5VLclose(default_con_id) < 0) {
        fprintf(stderr, "Unable to close VOL connector ID\n");
        err_occurred = true;
    }

    if (registered_con_id >= 0 && H5VLclose(registered_con_id) < 0) {
        fprintf(stderr, "Unable to close VOL connector ID\n");
        err_occurred = true;
    }

    if (fapl_id >= 0 && H5Pclose(fapl_id) < 0) {
        fprintf(stderr, "Unable to close FAPL\n");
        err_occurred = true;
    }

    H5close();

    exit(((err_occurred || n_tests_failed_g > 0) ? EXIT_FAILURE : EXIT_SUCCESS));
}
