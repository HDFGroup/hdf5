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

#include <openssl/evp.h> /* For SHA-256 hash in revocation test */
#include "H5encode.h"    /* For UINT32DECODE in footer decode */

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
 * Function:    create_revocation_file
 *
 * Purpose:     Read a signed plugin, compute the SHA-256 hash of its
 *              signature, and write the hex hash to
 *              <keystore_dir>/revoked_signatures.txt.
 *
 * Return:      0 on success, 1 on failure
 *-------------------------------------------------------------------------
 */
static int
create_revocation_file(const char *signed_plugin, const char *keystore_dir)
{
    int               fd = -1;
    h5_stat_t         st;
    uint8_t           footer_buf[H5PL_SIG_FOOTER_SIZE];
    H5PL_sig_footer_t footer;
    unsigned char    *signature = NULL;
    size_t            binary_size;
    unsigned char     hash[EVP_MAX_MD_SIZE];
    unsigned int      hash_len = 0;
    EVP_MD_CTX       *mdctx    = NULL;
    FILE             *fp       = NULL;
    char              filepath[512];
    unsigned int      i;
    int               ret = 1; /* assume failure */

    fd = HDopen(signed_plugin, O_RDONLY, 0);
    if (fd < 0)
        goto cleanup;
    if (HDfstat(fd, &st) < 0)
        goto cleanup;

    /* Read footer from end of file */
    if (HDlseek(fd, (HDoff_t)(st.st_size - H5PL_SIG_FOOTER_SIZE), SEEK_SET) < 0)
        goto cleanup;
    if (HDread(fd, footer_buf, H5PL_SIG_FOOTER_SIZE) != (h5_posix_io_ret_t)H5PL_SIG_FOOTER_SIZE)
        goto cleanup;
    if (!H5PL_sig_decode_footer(footer_buf, sizeof(footer_buf), &footer))
        goto cleanup;

    /* Read signature bytes */
    signature = (unsigned char *)malloc(footer.signature_length);
    if (!signature)
        goto cleanup;

    binary_size = (size_t)st.st_size - footer.signature_length - H5PL_SIG_FOOTER_SIZE;
    if (HDlseek(fd, (HDoff_t)binary_size, SEEK_SET) < 0)
        goto cleanup;
    if (HDread(fd, signature, footer.signature_length) != (h5_posix_io_ret_t)footer.signature_length)
        goto cleanup;

    /* Compute SHA-256 hash of signature */
    mdctx = EVP_MD_CTX_new();
    if (!mdctx)
        goto cleanup;
    if (1 != EVP_DigestInit_ex(mdctx, EVP_sha256(), NULL))
        goto cleanup;
    if (1 != EVP_DigestUpdate(mdctx, signature, footer.signature_length))
        goto cleanup;
    if (1 != EVP_DigestFinal_ex(mdctx, hash, &hash_len))
        goto cleanup;

    /* Write hex hash to revoked_signatures.txt */
    snprintf(filepath, sizeof(filepath), "%s/revoked_signatures.txt", keystore_dir);
    fp = fopen(filepath, "w");
    if (!fp)
        goto cleanup;

    fprintf(fp, "# Revoked signature hash for testing\n");
    for (i = 0; i < hash_len; i++)
        fprintf(fp, "%02x", hash[i]);
    fprintf(fp, "\n");

    ret = 0; /* success */

cleanup:
    if (fp)
        fclose(fp);
    if (mdctx)
        EVP_MD_CTX_free(mdctx);
    free(signature);
    if (fd >= 0)
        HDclose(fd);
    return ret;
}

/*-------------------------------------------------------------------------
 * Function:    remove_revocation_file
 *
 * Purpose:     Remove revoked_signatures.txt from the keystore directory
 *
 * Return:      void
 *-------------------------------------------------------------------------
 */
static void
remove_revocation_file(const char *keystore_dir)
{
    char filepath[512];
    snprintf(filepath, sizeof(filepath), "%s/revoked_signatures.txt", keystore_dir);
    HDremove(filepath);
}

/*-------------------------------------------------------------------------
 * Function:    test_verify_revoked_plugin
 *
 * Purpose:     Test that a signed plugin with a revoked signature
 *              fails verification.
 *
 *              This test must be run after H5close() / H5open() so that
 *              the keystore (including the revocation list) is reloaded.
 *
 * Return:      0 on success, 1 on failure
 *-------------------------------------------------------------------------
 */
static int
test_verify_revoked_plugin(void)
{
    herr_t ret;

    printf("TEST: Verify revoked plugin (should fail)... ");

    ret = H5PL__verify_signature_appended(TEST_PLUGIN_SIGNED);

    if (ret == FAIL) {
        printf("PASSED\n");
        tests_passed++;
        return 0;
    }
    else {
        printf("FAILED\n");
        printf("  Expected: FAIL, Got: SUCCEED (revoked plugin should not verify!)\n");
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

    /* Initialize the H5PL package through a public API call so that
     * H5PL_term_package() runs cleanup during H5close().  Without this,
     * the package-private test functions alone would not set H5PL_init_g,
     * so H5close() would skip keystore cleanup and the revocation test
     * would fail (stale keystore survives the H5close/H5open cycle). */
    {
        unsigned mask;
        if (H5PLget_loading_state(&mask) < 0) {
            fprintf(stderr, "ERROR: Cannot initialize H5PL package\n");
            H5close();
            return EXIT_FAILURE;
        }
    }

    /* Run basic verification tests */
    test_verify_signed_plugin();
    test_verify_unsigned_plugin();
    test_verify_tampered_plugin();

    /* --- Revocation test ---
     * Close and re-open HDF5 so the keystore is re-initialized with the
     * revocation list.  The signed plugin's signature hash is written to
     * revoked_signatures.txt before re-opening. */
    H5close();

    if (create_revocation_file(TEST_PLUGIN_SIGNED, TEST_KEYSTORE_DIR) != 0) {
        fprintf(stderr, "ERROR: Cannot create revocation file for testing\n");
        return EXIT_FAILURE;
    }

    if (H5open() < 0) {
        fprintf(stderr, "ERROR: Cannot re-initialize HDF5 library for revocation test\n");
        remove_revocation_file(TEST_KEYSTORE_DIR);
        return EXIT_FAILURE;
    }

    /* Re-set keystore env var (still valid, but ensures it's set after re-init) */
    HDsetenv("HDF5_PLUGIN_KEYSTORE", TEST_KEYSTORE_DIR, 1);

    test_verify_revoked_plugin();

    /* Clean up revocation file so it doesn't affect other tests */
    H5close();
    remove_revocation_file(TEST_KEYSTORE_DIR);

    /* Re-open for final cleanup */
    H5open();

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
