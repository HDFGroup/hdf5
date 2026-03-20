/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the LICENSE file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Purpose:  Test signature verification and caching
 *
 *           Tests the H5PL__verify_signature_appended() function.
 */

#include "hdf5.h"
#include "H5private.h"

/* Declare as friend of H5PL package to access package-private functions */
#define H5PL_FRIEND
#include "H5PLpkg.h" /* For H5PL__verify_signature_appended() */
#include "H5PLsig.h" /* For signature structures */
#include "H5MMprivate.h"

#ifdef H5_REQUIRE_DIGITAL_SIGNATURE

#include <sys/stat.h>
#include <time.h>

/* Test file names */
#define TEST_PLUGIN_SIGNED   "plugin_signed.so"
#define TEST_PLUGIN_UNSIGNED "plugin_unsigned.so"
#define TEST_PLUGIN_TAMPERED "plugin_tampered.so"
#define TEST_PUBLIC_KEY      "test_public.pem"
#define TEST_KEYSTORE_DIR    "test_keystore"

/* Test counters */
static int tests_passed = 0;
static int tests_failed = 0;

/*-------------------------------------------------------------------------
 * Function:    test_verify_signed_plugin
 *
 * Purpose:     Test that a properly signed plugin verifies successfully
 *
 * Return:      0 on success, 1 on failure
 *-------------------------------------------------------------------------
 */
static int
test_verify_signed_plugin(void)
{
    herr_t ret;

    printf("TEST: Verify signed plugin... ");

    /* Verify the signed plugin */
    ret = H5PL__verify_signature_appended(TEST_PLUGIN_SIGNED);

    if (ret == SUCCEED) {
        printf("PASSED\n");
        tests_passed++;
        return 0;
    }
    else {
        printf("FAILED\n");
        printf("  Expected: SUCCEED, Got: FAIL\n");
        tests_failed++;
        return 1;
    }
}

/*-------------------------------------------------------------------------
 * Function:    test_verify_unsigned_plugin
 *
 * Purpose:     Test that an unsigned plugin fails verification
 *
 * Return:      0 on success, 1 on failure
 *-------------------------------------------------------------------------
 */
static int
test_verify_unsigned_plugin(void)
{
    herr_t ret;

    printf("TEST: Verify unsigned plugin (should fail)... ");

    /* Try to verify unsigned plugin - should fail */
    ret = H5PL__verify_signature_appended(TEST_PLUGIN_UNSIGNED);

    if (ret == FAIL) {
        printf("PASSED\n");
        tests_passed++;
        return 0;
    }
    else {
        printf("FAILED\n");
        printf("  Expected: FAIL, Got: SUCCEED (unsigned plugin should not verify!)\n");
        tests_failed++;
        return 1;
    }
}

/*-------------------------------------------------------------------------
 * Function:    test_verify_tampered_plugin
 *
 * Purpose:     Test that a tampered plugin fails verification
 *
 * Return:      0 on success, 1 on failure
 *-------------------------------------------------------------------------
 */
static int
test_verify_tampered_plugin(void)
{
    herr_t ret;

    printf("TEST: Verify tampered plugin (should fail)... ");

    /* Try to verify tampered plugin - should fail */
    ret = H5PL__verify_signature_appended(TEST_PLUGIN_TAMPERED);

    if (ret == FAIL) {
        printf("PASSED\n");
        tests_passed++;
        return 0;
    }
    else {
        printf("FAILED\n");
        printf("  Expected: FAIL, Got: SUCCEED (tampered plugin should not verify!)\n");
        tests_failed++;
        return 1;
    }
}

/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     Run all signature verification tests
 *
 * Return:      EXIT_SUCCESS or EXIT_FAILURE
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    printf("\n");
    printf("========================================\n");
    printf("HDF5 Signature Verification Test Suite\n");
    printf("========================================\n");
    printf("\n");

    /* Initialize HDF5 library before using any HDF5 functions */
    if (H5open() < 0) {
        fprintf(stderr, "ERROR: Cannot initialize HDF5 library\n");
        return EXIT_FAILURE;
    }

    /* Set up environment for keystore */
    if (HDsetenv("HDF5_PLUGIN_KEYSTORE", TEST_KEYSTORE_DIR, 1) != 0) {
        fprintf(stderr, "ERROR: Cannot set HDF5_PLUGIN_KEYSTORE environment variable\n");
        H5close();
        return EXIT_FAILURE;
    }

    /* Run all tests */
    test_verify_signed_plugin();
    test_verify_unsigned_plugin();
    test_verify_tampered_plugin();

    /* Print summary */
    printf("\n");
    printf("========================================\n");
    printf("Test Summary\n");
    printf("========================================\n");
    printf("Tests Passed: %d\n", tests_passed);
    printf("Tests Failed: %d\n", tests_failed);
    printf("Total Tests:  %d\n", tests_passed + tests_failed);
    printf("\n");

    /* Clean up HDF5 library resources */
    H5close();

    if (tests_failed == 0) {
        printf("ALL TESTS PASSED!\n");
        return EXIT_SUCCESS;
    }
    else {
        printf("SOME TESTS FAILED!\n");
        return EXIT_FAILURE;
    }
}

#else /* H5_REQUIRE_DIGITAL_SIGNATURE */

int
main(void)
{
    printf("Digital signature support not enabled (H5_REQUIRE_DIGITAL_SIGNATURE not defined)\n");
    printf("Skipping signature verification tests\n");
    return EXIT_SUCCESS; /* Not a failure - just not compiled with signature support */
}

#endif /* H5_REQUIRE_DIGITAL_SIGNATURE */
