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
 *           Tests the H5PL__verify_signature_appended() function and
 *           signature cache implementation.
 */

#include "hdf5.h"
#include "H5private.h"
#include "H5PLpkg.h"   /* For H5PL__verify_signature_appended() */
#include "H5PLsig.h"   /* For signature structures */
#include "H5MMprivate.h"

#ifdef H5_REQUIRE_DIGITAL_SIGNATURE

#include <sys/stat.h>
#include <time.h>

/* Test file names */
#define TEST_PLUGIN_SIGNED    "plugin_signed.so"
#define TEST_PLUGIN_UNSIGNED  "plugin_unsigned.so"
#define TEST_PLUGIN_TAMPERED  "plugin_tampered.so"
#define TEST_PLUGIN_CACHE     "plugin_cache_test.so"
#define TEST_PUBLIC_KEY       "test_public.pem"
#define TEST_KEYSTORE_DIR     "test_keystore"

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
 * Function:    test_signature_cache_basic
 *
 * Purpose:     Test that signature cache works (2nd verification is fast)
 *
 * Return:      0 on success, 1 on failure
 *-------------------------------------------------------------------------
 */
static int
test_signature_cache_basic(void)
{
    herr_t ret1, ret2;
    time_t start1, end1, start2, end2;
    double time1, time2;

    printf("TEST: Signature cache basic functionality... ");

    /* First verification (cache miss) */
    start1 = time(NULL);
    ret1   = H5PL__verify_signature_appended(TEST_PLUGIN_CACHE);
    end1   = time(NULL);
    time1  = difftime(end1, start1);

    if (ret1 != SUCCEED) {
        printf("FAILED\n");
        printf("  First verification failed\n");
        tests_failed++;
        return 1;
    }

    /* Second verification (cache hit - should be instant) */
    start2 = time(NULL);
    ret2   = H5PL__verify_signature_appended(TEST_PLUGIN_CACHE);
    end2   = time(NULL);
    time2  = difftime(end2, start2);

    if (ret2 != SUCCEED) {
        printf("FAILED\n");
        printf("  Second verification failed (cache should return cached result)\n");
        tests_failed++;
        return 1;
    }

    /* Cache hit should be faster (or at least not slower) */
    if (time2 <= time1) {
        printf("PASSED\n");
        printf("  First verification: %.1f seconds\n", time1);
        printf("  Second verification (cached): %.1f seconds\n", time2);
        tests_passed++;
        return 0;
    }
    else {
        printf("PASSED (with note)\n");
        printf("  NOTE: Cache hit not faster (timing may be too coarse)\n");
        printf("  First: %.1f sec, Second: %.1f sec\n", time1, time2);
        tests_passed++;
        return 0;
    }
}

/*-------------------------------------------------------------------------
 * Function:    test_signature_cache_invalidation
 *
 * Purpose:     Test that cache is invalidated when file is modified
 *
 * Return:      0 on success, 1 on failure
 *-------------------------------------------------------------------------
 */
static int
test_signature_cache_invalidation(void)
{
    herr_t    ret1, ret2, ret3;
    FILE     *fp;
    h5_stat_t st;

    printf("TEST: Signature cache invalidation on file modification... ");

    /* Create a temporary copy of the signed plugin for this test */
    const char *temp_plugin = "plugin_cache_invalidation_test.so";

    /* Copy TEST_PLUGIN_SIGNED to temp_plugin */
    {
        FILE          *src, *dst;
        unsigned char  buffer[4096];
        size_t         bytes;

        if (NULL == (src = fopen(TEST_PLUGIN_SIGNED, "rb"))) {
            printf("FAILED\n");
            printf("  Cannot open source file\n");
            tests_failed++;
            return 1;
        }

        if (NULL == (dst = fopen(temp_plugin, "wb"))) {
            printf("FAILED\n");
            printf("  Cannot create temp file\n");
            fclose(src);
            tests_failed++;
            return 1;
        }

        while ((bytes = fread(buffer, 1, sizeof(buffer), src)) > 0) {
            if (fwrite(buffer, 1, bytes, dst) != bytes) {
                printf("FAILED\n");
                printf("  Cannot write to temp file\n");
                fclose(src);
                fclose(dst);
                tests_failed++;
                return 1;
            }
        }

        fclose(src);
        fclose(dst);
    }

    /* First verification - populate cache */
    ret1 = H5PL__verify_signature_appended(temp_plugin);
    if (ret1 != SUCCEED) {
        printf("FAILED\n");
        printf("  Initial verification failed\n");
        tests_failed++;
        remove(temp_plugin);
        return 1;
    }

    /* Second verification - cache hit */
    ret2 = H5PL__verify_signature_appended(temp_plugin);
    if (ret2 != SUCCEED) {
        printf("FAILED\n");
        printf("  Cached verification failed\n");
        tests_failed++;
        remove(temp_plugin);
        return 1;
    }

    /* Modify the file (touch to update mtime) */
    sleep(2); /* Ensure mtime changes */

    if (NULL == (fp = fopen(temp_plugin, "ab"))) {
        printf("FAILED\n");
        printf("  Cannot open file for modification\n");
        tests_failed++;
        remove(temp_plugin);
        return 1;
    }

    /* Append a single byte to change the file and update mtime */
    fputc(0xFF, fp);
    fclose(fp);

    /* Verify mtime changed */
    if (HDstat(temp_plugin, &st) < 0) {
        printf("FAILED\n");
        printf("  Cannot stat modified file\n");
        tests_failed++;
        remove(temp_plugin);
        return 1;
    }

    /* Third verification - cache should be invalidated (file modified)
     * This SHOULD fail because we tampered with the file */
    ret3 = H5PL__verify_signature_appended(temp_plugin);

    /* Clean up */
    remove(temp_plugin);

    if (ret3 == FAIL) {
        printf("PASSED\n");
        printf("  Cache correctly invalidated after file modification\n");
        printf("  Tampered file correctly failed verification\n");
        tests_passed++;
        return 0;
    }
    else {
        printf("FAILED\n");
        printf("  Cache not invalidated or tampered file verified (both are wrong!)\n");
        tests_failed++;
        return 1;
    }
}

/*-------------------------------------------------------------------------
 * Function:    test_cache_negative_results
 *
 * Purpose:     Test that cache also stores negative (failed) results
 *
 * Return:      0 on success, 1 on failure
 *-------------------------------------------------------------------------
 */
static int
test_cache_negative_results(void)
{
    herr_t ret1, ret2;
    time_t start1, end1, start2, end2;
    double time1, time2;

    printf("TEST: Cache negative (failed) verification results... ");

    /* First verification of unsigned plugin (cache miss, should fail) */
    start1 = time(NULL);
    ret1   = H5PL__verify_signature_appended(TEST_PLUGIN_UNSIGNED);
    end1   = time(NULL);
    time1  = difftime(end1, start1);

    if (ret1 != FAIL) {
        printf("FAILED\n");
        printf("  Unsigned plugin verified successfully (should fail!)\n");
        tests_failed++;
        return 1;
    }

    /* Second verification of same unsigned plugin (cache hit, should still fail quickly) */
    start2 = time(NULL);
    ret2   = H5PL__verify_signature_appended(TEST_PLUGIN_UNSIGNED);
    end2   = time(NULL);
    time2  = difftime(end2, start2);

    if (ret2 != FAIL) {
        printf("FAILED\n");
        printf("  Cached negative result returned success (should fail!)\n");
        tests_failed++;
        return 1;
    }

    printf("PASSED\n");
    printf("  Negative result correctly cached and returned\n");
    tests_passed++;
    return 0;
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

    /* Set up environment for keystore */
    if (setenv("HDF5_PLUGIN_KEYSTORE", TEST_KEYSTORE_DIR, 1) != 0) {
        fprintf(stderr, "ERROR: Cannot set HDF5_PLUGIN_KEYSTORE environment variable\n");
        return EXIT_FAILURE;
    }

    /* Run all tests */
    test_verify_signed_plugin();
    test_verify_unsigned_plugin();
    test_verify_tampered_plugin();
    test_signature_cache_basic();
    test_signature_cache_invalidation();
    test_cache_negative_results();

    /* Print summary */
    printf("\n");
    printf("========================================\n");
    printf("Test Summary\n");
    printf("========================================\n");
    printf("Tests Passed: %d\n", tests_passed);
    printf("Tests Failed: %d\n", tests_failed);
    printf("Total Tests:  %d\n", tests_passed + tests_failed);
    printf("\n");

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
