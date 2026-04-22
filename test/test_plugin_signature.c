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
 * Purpose:    Comprehensive tests for HDF5 plugin signature verification
 *
 *             This test suite verifies that the plugin signature verification
 *             system correctly handles:
 *             1. Valid signed plugins (should load successfully)
 *             2. Unsigned plugins (should be rejected)
 *             3. Tampered plugins (should be rejected)
 *             4. Plugins with invalid signatures (should be rejected)
 */

#include "h5test.h"
#include "H5srcdir.h"

/*
 * This file needs to access private datatypes from the H5PL package.
 */
#define H5PL_FRIEND
#include "H5PLpkg.h"
#include "H5PLsig.h"

#ifdef H5_REQUIRE_DIGITAL_SIGNATURE

#include <sys/stat.h>
#include <fcntl.h>

/* Test filter ID */
#define TEST_SIGNATURE_FILTER_ID 260

/* Test files */
static const char *PLUGIN_DIR           = "test_plugin_signature_dir";
static const char *SIGNED_PLUGIN        = "libh5test_sig_filter.so";
static const char *UNSIGNED_PLUGIN      = "libh5test_sig_filter_unsigned.so";
static const char *TAMPERED_PLUGIN      = "libh5test_sig_filter_tampered.so";
static const char *BAD_SIG_PLUGIN       = "libh5test_sig_filter_badsig.so";
static const char *NO_FOOTER_PLUGIN     = "libh5test_sig_filter_nofooter.so";
static const char *CORRUPT_MAGIC_PLUGIN = "libh5test_sig_filter_badmagic.so";

/* Test key paths (set via environment or compile-time) */
static char test_private_key[1024] = "";
static char test_public_key[1024]  = "";

/*-------------------------------------------------------------------------
 * Function:    create_dummy_plugin
 *
 * Purpose:     Create a minimal valid plugin binary for testing
 *              This creates a simple binary file that can be used as
 *              a base for signature testing.
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
create_dummy_plugin(const char *path)
{
    int    fd;
    herr_t ret_value = SUCCEED;

    /* Create minimal plugin file - just some dummy binary data */
    const unsigned char dummy_data[] = {/* ELF header magic for shared library (simplified) */
                                        0x7f, 'E', 'L', 'F',    /* Magic number */
                                        0x02, 0x01, 0x01, 0x00, /* 64-bit, little-endian, current version */
                                        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* Padding */
                                        /* Some dummy content to make it a reasonable size */
                                        'T', 'E', 'S', 'T', ' ', 'P', 'L', 'U', 'G', 'I', 'N', '\0'};

    if ((fd = HDopen(path, O_WRONLY | O_CREAT | O_TRUNC, 0644)) < 0) {
        fprintf(stderr, "Failed to create plugin file: %s\n", path);
        return FAIL;
    }

    if (HDwrite(fd, dummy_data, sizeof(dummy_data)) < 0) {
        fprintf(stderr, "Failed to write plugin data: %s\n", path);
        HDclose(fd);
        return FAIL;
    }

    HDclose(fd);
    return SUCCEED;
}

/*-------------------------------------------------------------------------
 * Function:    sign_plugin_file
 *
 * Purpose:     Sign a plugin file using the h5sign tool
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
sign_plugin_file(const char *plugin_path, const char *private_key_path)
{
    char   cmd[2048];
    int    result;
    herr_t ret_value = SUCCEED;

    /* Build command to sign the plugin using h5sign tool (quote paths for safety) */
    snprintf(cmd, sizeof(cmd), "h5sign -p \"%s\" -k \"%s\" 2>&1", plugin_path, private_key_path);

    result = system(cmd);
    if (result != 0) {
        fprintf(stderr, "Failed to sign plugin: %s\n", plugin_path);
        return FAIL;
    }

    return SUCCEED;
}

/*-------------------------------------------------------------------------
 * Function:    append_bad_signature
 *
 * Purpose:     Append an invalid signature to a plugin file
 *              This creates a plugin that has a signature footer but
 *              with an incorrect signature value.
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
append_bad_signature(const char *plugin_path)
{
    int               fd;
    H5PL_sig_footer_t footer;
    unsigned char     bad_signature[256];
    size_t            i;
    herr_t            ret_value = SUCCEED;

    /* Create a dummy bad signature (just random bytes) */
    for (i = 0; i < sizeof(bad_signature); i++)
        bad_signature[i] = (unsigned char)(i * 7 + 13); /* Arbitrary pattern */

    /* Open plugin file in append mode */
    if ((fd = HDopen(plugin_path, O_WRONLY | O_APPEND, 0)) < 0) {
        fprintf(stderr, "Failed to open plugin for bad signature: %s\n", plugin_path);
        return FAIL;
    }

    /* Write bad signature */
    if (HDwrite(fd, bad_signature, sizeof(bad_signature)) < 0) {
        fprintf(stderr, "Failed to write bad signature\n");
        HDclose(fd);
        return FAIL;
    }

    /* Write footer with correct format but pointing to bad signature */
    footer.signature_length = sizeof(bad_signature);
    footer.algorithm_id     = H5PL_SIG_ALGO_SHA256;
    footer.format_version   = 1;

    /* Encode footer in little-endian (as expected by verification code) */
    {
        unsigned char footer_bytes[H5PL_SIG_FOOTER_SIZE];

        H5PL_sig_encode_footer(footer_bytes, sizeof(footer_bytes), &footer);

        if (HDwrite(fd, footer_bytes, sizeof(footer_bytes)) < 0) {
            fprintf(stderr, "Failed to write footer\n");
            HDclose(fd);
            return FAIL;
        }
    }

    HDclose(fd);
    return SUCCEED;
}

/*-------------------------------------------------------------------------
 * Function:    append_corrupt_footer
 *
 * Purpose:     Append a footer with corrupted magic number
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
append_corrupt_footer(const char *plugin_path)
{
    int           fd;
    unsigned char footer_bytes[H5PL_SIG_FOOTER_SIZE];
    herr_t        ret_value = SUCCEED;

    if ((fd = HDopen(plugin_path, O_WRONLY | O_APPEND, 0)) < 0) {
        fprintf(stderr, "Failed to open plugin for corrupt footer: %s\n", plugin_path);
        return FAIL;
    }

    /* Write footer with wrong magic number */
    {
        H5PL_sig_footer_t footer;

        footer.signature_length = 256;
        footer.algorithm_id     = H5PL_SIG_ALGO_SHA256;
        footer.format_version   = 1;

        H5PL_sig_encode_footer(footer_bytes, sizeof(footer_bytes), &footer);

        /* Corrupt the magic bytes (at offset 5) */
        footer_bytes[5] = 0xDE;
        footer_bytes[6] = 0xAD;
        footer_bytes[7] = 0xBE;
        footer_bytes[8] = 0xEF;
    }

    if (HDwrite(fd, footer_bytes, sizeof(footer_bytes)) < 0) {
        fprintf(stderr, "Failed to write corrupt footer\n");
        HDclose(fd);
        return FAIL;
    }

    HDclose(fd);
    return SUCCEED;
}

/*-------------------------------------------------------------------------
 * Function:    tamper_with_plugin
 *
 * Purpose:     Modify a signed plugin to invalidate its signature
 *              This simulates an attacker tampering with a signed plugin.
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
tamper_with_plugin(const char *plugin_path)
{
    int    fd;
    char   byte;
    herr_t ret_value = SUCCEED;

    /* Open plugin and modify the first byte of content */
    if ((fd = HDopen(plugin_path, O_RDWR, 0)) < 0) {
        fprintf(stderr, "Failed to open plugin for tampering: %s\n", plugin_path);
        return FAIL;
    }

    /* Read first byte */
    if (HDread(fd, &byte, 1) < 0) {
        fprintf(stderr, "Failed to read plugin byte\n");
        HDclose(fd);
        return FAIL;
    }

    /* Modify it */
    byte ^= 0xFF;

    /* Seek back and write modified byte */
    if (HDlseek(fd, 0, SEEK_SET) < 0) {
        fprintf(stderr, "Failed to seek plugin\n");
        HDclose(fd);
        return FAIL;
    }

    if (HDwrite(fd, &byte, 1) < 0) {
        fprintf(stderr, "Failed to write modified byte\n");
        HDclose(fd);
        return FAIL;
    }

    HDclose(fd);
    return SUCCEED;
}

/*-------------------------------------------------------------------------
 * Function:    generate_rsa_keypair
 *
 * Purpose:     Generate RSA key pair using OpenSSL command line
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
generate_rsa_keypair(int bits, const char *private_path, const char *public_path)
{
    char cmd[2048];
    int  result;

    /* Generate private key */
#ifdef H5_HAVE_WIN32_API
    snprintf(cmd, sizeof(cmd), "openssl genrsa -out %s %d >NUL 2>&1", private_path, bits);
#else
    snprintf(cmd, sizeof(cmd), "openssl genrsa -out %s %d 2>&1 >/dev/null", private_path, bits);
#endif
    result = system(cmd);
    if (result != 0) {
        fprintf(stderr, "Failed to generate RSA-%d private key: %s\n", bits, private_path);
        return FAIL;
    }

    /* Extract public key */
#ifdef H5_HAVE_WIN32_API
    snprintf(cmd, sizeof(cmd), "openssl rsa -in %s -pubout -out %s >NUL 2>&1", private_path, public_path);
#else
    snprintf(cmd, sizeof(cmd), "openssl rsa -in %s -pubout -out %s 2>&1 >/dev/null", private_path,
             public_path);
#endif
    result = system(cmd);
    if (result != 0) {
        fprintf(stderr, "Failed to extract RSA-%d public key: %s\n", bits, public_path);
        return FAIL;
    }

    return SUCCEED;
}

/*-------------------------------------------------------------------------
 * Function:    create_keystore_directory
 *
 * Purpose:     Create a KeyStore directory with specific permissions
 *
 * Return:      Allocated path string (caller must free), NULL on failure
 *-------------------------------------------------------------------------
 */
static char *
create_keystore_directory(const char *base_dir, const char *dir_name, unsigned permissions)
{
    char  full_path[1024];
    char *ret_value = NULL;

    snprintf(full_path, sizeof(full_path), "%s/%s", base_dir, dir_name);

    if (HDmkdir(full_path, permissions) < 0) {
        fprintf(stderr, "Failed to create keystore directory: %s\n", full_path);
        return NULL;
    }

    ret_value = strdup(full_path);
    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function:    add_key_to_keystore
 *
 * Purpose:     Copy a PEM key file into a KeyStore directory
 *
 * Return:      Allocated destination path (caller must free), NULL on failure
 *-------------------------------------------------------------------------
 */
static char *
add_key_to_keystore(const char *keystore_dir, const char *key_name, const char *key_source)
{
    char  dest_path[1024];
    char  cmd[2048];
    char *ret_value = NULL;

    snprintf(dest_path, sizeof(dest_path), "%s/%s", keystore_dir, key_name);

    /* Copy file using C standard I/O (portable across all platforms) */
    {
        FILE         *src, *dst;
        unsigned char buf[4096];
        size_t        n;
        int           copy_ok = 1;

        if (NULL == (src = fopen(key_source, "rb"))) {
            fprintf(stderr, "Failed to open source key: %s\n", key_source);
            return NULL;
        }
        if (NULL == (dst = fopen(dest_path, "wb"))) {
            fprintf(stderr, "Failed to open dest key: %s\n", dest_path);
            fclose(src);
            return NULL;
        }
        while ((n = fread(buf, 1, sizeof(buf), src)) > 0) {
            if (fwrite(buf, 1, n, dst) != n) {
                copy_ok = 0;
                break;
            }
        }
        fclose(src);
        fclose(dst);
        if (!copy_ok) {
            fprintf(stderr, "Failed to copy key to keystore: %s -> %s\n", key_source, dest_path);
            return NULL;
        }
    }

    ret_value = strdup(dest_path);
    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function:    create_corrupted_pem
 *
 * Purpose:     Create corrupted/invalid PEM files for testing
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
typedef enum {
    PEM_CORRUPT_TRUNCATED,     /* Cut off mid-key */
    PEM_CORRUPT_GARBAGE,       /* Random binary data */
    PEM_CORRUPT_WRONG_FORMAT,  /* Missing BEGIN/END markers */
    PEM_CORRUPT_WRONG_KEY_TYPE /* ECDSA instead of RSA */
} corruption_type_t;

static herr_t
create_corrupted_pem(const char *path, corruption_type_t type)
{
    int    fd;
    herr_t ret_value = SUCCEED;

    switch (type) {
        case PEM_CORRUPT_TRUNCATED: {
            /* Create truncated PEM - write partial header */
            const char truncated[] = "-----BEGIN PUBLIC KEY-----\nMIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIB";
            if ((fd = HDopen(path, O_WRONLY | O_CREAT | O_TRUNC, 0644)) < 0)
                return FAIL;
            HDwrite(fd, truncated, strlen(truncated));
            HDclose(fd);
            break;
        }

        case PEM_CORRUPT_GARBAGE: {
            /* Write random binary garbage */
            unsigned char garbage[256];
            size_t        i;
            for (i = 0; i < sizeof(garbage); i++)
                garbage[i] = (unsigned char)(i * 13 + 7);
            if ((fd = HDopen(path, O_WRONLY | O_CREAT | O_TRUNC, 0644)) < 0)
                return FAIL;
            HDwrite(fd, garbage, sizeof(garbage));
            HDclose(fd);
            break;
        }

        case PEM_CORRUPT_WRONG_FORMAT: {
            /* Write text without PEM markers */
            const char wrong[] = "This is not a PEM file\nJust some random text\nNo markers here\n";
            if ((fd = HDopen(path, O_WRONLY | O_CREAT | O_TRUNC, 0644)) < 0)
                return FAIL;
            HDwrite(fd, wrong, strlen(wrong));
            HDclose(fd);
            break;
        }

        case PEM_CORRUPT_WRONG_KEY_TYPE: {
            /* Generate ECDSA key instead of RSA */
            char cmd[2048];
#ifdef H5_HAVE_WIN32_API
            snprintf(cmd, sizeof(cmd),
                     "openssl ecparam -genkey -name prime256v1 -noout | openssl ec -pubout -out %s >NUL 2>&1",
                     path);
#else
            snprintf(cmd, sizeof(cmd),
                     "openssl ecparam -genkey -name prime256v1 -noout | openssl ec -pubout -out %s 2>&1 "
                     ">/dev/null",
                     path);
#endif
            if (system(cmd) != 0) {
                /* If ECDSA generation fails, just write invalid RSA-like content */
                const char fake_ecdsa[] =
                    "-----BEGIN PUBLIC KEY-----\nMFkwEwYHKoZIzj0CAQYIKoZIzj0DAQcDQgAE\n-----END PUBLIC "
                    "KEY-----\n";
                if ((fd = HDopen(path, O_WRONLY | O_CREAT | O_TRUNC, 0644)) < 0)
                    return FAIL;
                HDwrite(fd, fake_ecdsa, strlen(fake_ecdsa));
                HDclose(fd);
            }
            break;
        }

        default:
            return FAIL;
    }

    return SUCCEED;
}

/*-------------------------------------------------------------------------
 * Function:    reset_keystore_state
 *
 * Purpose:     Reset KeyStore global state between tests
 *              This ensures test isolation
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
reset_keystore_state(void)
{
    /* Cleanup keystore to force reinitialization.
     * This allows tests to use different KeyStore directories.
     */
    if (H5PL__cleanup_signature_resources() < 0) {
        fprintf(stderr, "Failed to cleanup signature resources\n");
        return FAIL;
    }
    return SUCCEED;
}

/*-------------------------------------------------------------------------
 * Function:    setup_test_environment
 *
 * Purpose:     Set up the test environment with various test plugins
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
setup_test_environment(void)
{
    char plugin_path[1024];
    char temp_path[1024];

    /* Create plugin directory */
    HDmkdir(PLUGIN_DIR, 0755);

    /* Get test keys from environment or use defaults */
    if (getenv("HDF5_TEST_PRIVATE_KEY")) {
        snprintf(test_private_key, sizeof(test_private_key), "%s", getenv("HDF5_TEST_PRIVATE_KEY"));
    }
    else {
        /* Try to find test keys in common locations */
        snprintf(test_private_key, sizeof(test_private_key), "%s/ci-test-private.pem", H5_get_srcdir());
    }

    if (getenv("HDF5_TEST_PUBLIC_KEY")) {
        snprintf(test_public_key, sizeof(test_public_key), "%s", getenv("HDF5_TEST_PUBLIC_KEY"));
    }
    else {
        snprintf(test_public_key, sizeof(test_public_key), "%s/ci-test-public.pem", H5_get_srcdir());
    }

    /* Verify keys exist */
    if (access(test_private_key, R_OK) != 0) {
        fprintf(stderr, "Test private key not found: %s\n", test_private_key);
        fprintf(stderr, "Set HDF5_TEST_PRIVATE_KEY environment variable or generate keys\n");
        return FAIL;
    }

    /* 1. Create and sign a valid plugin */
    snprintf(plugin_path, sizeof(plugin_path), "%s/%s", PLUGIN_DIR, SIGNED_PLUGIN);
    if (create_dummy_plugin(plugin_path) < 0)
        return FAIL;
    if (sign_plugin_file(plugin_path, test_private_key) < 0)
        return FAIL;

    /* 2. Create an unsigned plugin */
    snprintf(plugin_path, sizeof(plugin_path), "%s/%s", PLUGIN_DIR, UNSIGNED_PLUGIN);
    if (create_dummy_plugin(plugin_path) < 0)
        return FAIL;

    /* 3. Create a signed plugin then tamper with it */
    snprintf(plugin_path, sizeof(plugin_path), "%s/%s", PLUGIN_DIR, TAMPERED_PLUGIN);
    if (create_dummy_plugin(plugin_path) < 0)
        return FAIL;
    if (sign_plugin_file(plugin_path, test_private_key) < 0)
        return FAIL;
    if (tamper_with_plugin(plugin_path) < 0)
        return FAIL;

    /* 4. Create plugin with bad signature */
    snprintf(plugin_path, sizeof(plugin_path), "%s/%s", PLUGIN_DIR, BAD_SIG_PLUGIN);
    if (create_dummy_plugin(plugin_path) < 0)
        return FAIL;
    if (append_bad_signature(plugin_path) < 0)
        return FAIL;

    /* 5. Create plugin with no footer */
    snprintf(plugin_path, sizeof(plugin_path), "%s/%s", PLUGIN_DIR, NO_FOOTER_PLUGIN);
    if (create_dummy_plugin(plugin_path) < 0)
        return FAIL;

    /* 6. Create plugin with corrupted magic number */
    snprintf(plugin_path, sizeof(plugin_path), "%s/%s", PLUGIN_DIR, CORRUPT_MAGIC_PLUGIN);
    if (create_dummy_plugin(plugin_path) < 0)
        return FAIL;
    if (append_corrupt_footer(plugin_path) < 0)
        return FAIL;

    return SUCCEED;
}

/*-------------------------------------------------------------------------
 * Function:    cleanup_test_environment
 *
 * Purpose:     Clean up test files
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
cleanup_test_environment(void)
{
    char cmd[1024];

    /* Remove plugin directory (includes all KeyStore subdirectories) */
#ifdef H5_HAVE_WIN32_API
    snprintf(cmd, sizeof(cmd), "rmdir /s /q %s >NUL 2>&1", PLUGIN_DIR);
    system(cmd);
    system("del /q org*_*.pem test_*_4096.pem *_private.pem *_public.pem >NUL 2>&1");
#else
    snprintf(cmd, sizeof(cmd), "rm -rf %s", PLUGIN_DIR);
    system(cmd);
    system("rm -f org*_*.pem test_*_4096.pem *_private.pem *_public.pem 2>/dev/null");
#endif

    return SUCCEED;
}

/*-------------------------------------------------------------------------
 * Function:    test_valid_signed_plugin
 *
 * Purpose:     Test that a properly signed plugin is accepted
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
test_valid_signed_plugin(void)
{
    char   plugin_path[1024];
    herr_t ret_value = SUCCEED;

    TESTING("valid signed plugin verification");

    snprintf(plugin_path, sizeof(plugin_path), "%s/%s", PLUGIN_DIR, SIGNED_PLUGIN);

    /* Verify the signed plugin */
    if (H5PL__verify_signature_appended(plugin_path) < 0) {
        H5_FAILED();
        fprintf(stderr, "    Valid signed plugin was rejected\n");
        return FAIL;
    }

    PASSED();
    return SUCCEED;
}

/*-------------------------------------------------------------------------
 * Function:    test_unsigned_plugin_rejected
 *
 * Purpose:     Test that an unsigned plugin is rejected
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
test_unsigned_plugin_rejected(void)
{
    char   plugin_path[1024];
    herr_t status;

    TESTING("unsigned plugin rejection");

    snprintf(plugin_path, sizeof(plugin_path), "%s/%s", PLUGIN_DIR, UNSIGNED_PLUGIN);

    /* Verification should fail for unsigned plugin */
    H5E_BEGIN_TRY
    {
        status = H5PL__verify_signature_appended(plugin_path);
    }
    H5E_END_TRY;

    if (status >= 0) {
        H5_FAILED();
        fprintf(stderr, "    Unsigned plugin was incorrectly accepted\n");
        return FAIL;
    }

    PASSED();
    return SUCCEED;
}

/*-------------------------------------------------------------------------
 * Function:    test_tampered_plugin_rejected
 *
 * Purpose:     Test that a tampered plugin is rejected
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
test_tampered_plugin_rejected(void)
{
    char   plugin_path[1024];
    herr_t status;

    TESTING("tampered plugin rejection");

    snprintf(plugin_path, sizeof(plugin_path), "%s/%s", PLUGIN_DIR, TAMPERED_PLUGIN);

    /* Verification should fail for tampered plugin */
    H5E_BEGIN_TRY
    {
        status = H5PL__verify_signature_appended(plugin_path);
    }
    H5E_END_TRY;

    if (status >= 0) {
        H5_FAILED();
        fprintf(stderr, "    Tampered plugin was incorrectly accepted\n");
        return FAIL;
    }

    PASSED();
    return SUCCEED;
}

/*-------------------------------------------------------------------------
 * Function:    test_bad_signature_rejected
 *
 * Purpose:     Test that a plugin with wrong signature is rejected
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
test_bad_signature_rejected(void)
{
    char   plugin_path[1024];
    herr_t status;

    TESTING("plugin with invalid signature rejection");

    snprintf(plugin_path, sizeof(plugin_path), "%s/%s", PLUGIN_DIR, BAD_SIG_PLUGIN);

    /* Verification should fail for plugin with bad signature */
    H5E_BEGIN_TRY
    {
        status = H5PL__verify_signature_appended(plugin_path);
    }
    H5E_END_TRY;

    if (status >= 0) {
        H5_FAILED();
        fprintf(stderr, "    Plugin with bad signature was incorrectly accepted\n");
        return FAIL;
    }

    PASSED();
    return SUCCEED;
}

/*-------------------------------------------------------------------------
 * Function:    test_no_footer_rejected
 *
 * Purpose:     Test that a plugin without signature footer is rejected
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
test_no_footer_rejected(void)
{
    char   plugin_path[1024];
    herr_t status;

    TESTING("plugin without signature footer rejection");

    snprintf(plugin_path, sizeof(plugin_path), "%s/%s", PLUGIN_DIR, NO_FOOTER_PLUGIN);

    /* Verification should fail for plugin without footer */
    H5E_BEGIN_TRY
    {
        status = H5PL__verify_signature_appended(plugin_path);
    }
    H5E_END_TRY;

    if (status >= 0) {
        H5_FAILED();
        fprintf(stderr, "    Plugin without footer was incorrectly accepted\n");
        return FAIL;
    }

    PASSED();
    return SUCCEED;
}

/*-------------------------------------------------------------------------
 * Function:    test_corrupt_magic_rejected
 *
 * Purpose:     Test that a plugin with corrupted magic number is rejected
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
test_corrupt_magic_rejected(void)
{
    char   plugin_path[1024];
    herr_t status;

    TESTING("plugin with corrupt magic number rejection");

    snprintf(plugin_path, sizeof(plugin_path), "%s/%s", PLUGIN_DIR, CORRUPT_MAGIC_PLUGIN);

    /* Verification should fail for plugin with corrupt magic */
    H5E_BEGIN_TRY
    {
        status = H5PL__verify_signature_appended(plugin_path);
    }
    H5E_END_TRY;

    if (status >= 0) {
        H5_FAILED();
        fprintf(stderr, "    Plugin with corrupt magic was incorrectly accepted\n");
        return FAIL;
    }

    PASSED();
    return SUCCEED;
}

/*-------------------------------------------------------------------------
 * Function:    test_keystore_multiple_keys
 *
 * Purpose:     Test that KeyStore can load and use multiple trusted keys
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
test_keystore_multiple_keys(void)
{
    char  *keystore_dir = NULL;
    char   keystore_path[1024];
    char   plugin1_path[1024], plugin2_path[1024], plugin3_path[1024];
    char   priv1[1024], pub1[1024];
    char   priv2[1024], pub2[1024];
    char   priv3[1024], pub3[1024];
    char  *key_path  = NULL;
    herr_t ret_value = SUCCEED;

    TESTING("multiple keys in keystore verification");

    /* Create KeyStore directory */
    keystore_dir = create_keystore_directory(PLUGIN_DIR, "test_keystore_multiple", 0755);
    if (!keystore_dir) {
        H5_FAILED();
        fprintf(stderr, "Failed to create keystore directory\n");
        return FAIL;
    }

    /* Generate 3 separate key pairs */
    snprintf(priv1, sizeof(priv1), "%s/org1_private.pem", PLUGIN_DIR);
    snprintf(pub1, sizeof(pub1), "%s/org1_public.pem", PLUGIN_DIR);
    snprintf(priv2, sizeof(priv2), "%s/org2_private.pem", PLUGIN_DIR);
    snprintf(pub2, sizeof(pub2), "%s/org2_public.pem", PLUGIN_DIR);
    snprintf(priv3, sizeof(priv3), "%s/org3_private.pem", PLUGIN_DIR);
    snprintf(pub3, sizeof(pub3), "%s/org3_public.pem", PLUGIN_DIR);

    if (generate_rsa_keypair(2048, priv1, pub1) < 0 || generate_rsa_keypair(2048, priv2, pub2) < 0 ||
        generate_rsa_keypair(2048, priv3, pub3) < 0) {
        H5_FAILED();
        fprintf(stderr, "Failed to generate key pairs\n");
        goto error;
    }

    /* Add all 3 public keys to KeyStore */
    key_path = add_key_to_keystore(keystore_dir, "org1.pem", pub1);
    if (key_path)
        free(key_path);
    key_path = add_key_to_keystore(keystore_dir, "org2.pem", pub2);
    if (key_path)
        free(key_path);
    key_path = add_key_to_keystore(keystore_dir, "org3.pem", pub3);
    if (key_path)
        free(key_path);

    /* Create and sign 3 plugins with different keys */
    snprintf(plugin1_path, sizeof(plugin1_path), "%s/plugin_org1.so", PLUGIN_DIR);
    snprintf(plugin2_path, sizeof(plugin2_path), "%s/plugin_org2.so", PLUGIN_DIR);
    snprintf(plugin3_path, sizeof(plugin3_path), "%s/plugin_org3.so", PLUGIN_DIR);

    if (create_dummy_plugin(plugin1_path) < 0 || sign_plugin_file(plugin1_path, priv1) < 0) {
        H5_FAILED();
        fprintf(stderr, "Failed to create/sign plugin1\n");
        goto error;
    }

    if (create_dummy_plugin(plugin2_path) < 0 || sign_plugin_file(plugin2_path, priv2) < 0) {
        H5_FAILED();
        fprintf(stderr, "Failed to create/sign plugin2\n");
        goto error;
    }

    if (create_dummy_plugin(plugin3_path) < 0 || sign_plugin_file(plugin3_path, priv3) < 0) {
        H5_FAILED();
        fprintf(stderr, "Failed to create/sign plugin3\n");
        goto error;
    }

    /* Set environment variable to use this KeyStore */
    snprintf(keystore_path, sizeof(keystore_path), "%s", keystore_dir);
    HDsetenv("HDF5_PLUGIN_KEYSTORE", keystore_path, 1);
    reset_keystore_state();

    /* Verify all 3 plugins (each should match a different key) */
    if (H5PL__verify_signature_appended(plugin1_path) < 0) {
        H5_FAILED();
        fprintf(stderr, "Plugin1 (org1 key) was rejected\n");
        HDunsetenv("HDF5_PLUGIN_KEYSTORE");
        goto error;
    }

    if (H5PL__verify_signature_appended(plugin2_path) < 0) {
        H5_FAILED();
        fprintf(stderr, "Plugin2 (org2 key) was rejected\n");
        HDunsetenv("HDF5_PLUGIN_KEYSTORE");
        goto error;
    }

    if (H5PL__verify_signature_appended(plugin3_path) < 0) {
        H5_FAILED();
        fprintf(stderr, "Plugin3 (org3 key) was rejected\n");
        HDunsetenv("HDF5_PLUGIN_KEYSTORE");
        goto error;
    }

    /* Cleanup */
    HDunsetenv("HDF5_PLUGIN_KEYSTORE");
    free(keystore_dir);

    PASSED();
    return SUCCEED;

error:
    if (keystore_dir)
        free(keystore_dir);
    return FAIL;
}

/*-------------------------------------------------------------------------
 * Function:    test_invalid_pem_file_handling
 *
 * Purpose:     Test that corrupted PEM files are silently skipped
 *              and valid keys still work
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
test_invalid_pem_file_handling(void)
{
    char  *keystore_dir = NULL;
    char   keystore_path[1024];
    char   valid_priv[1024], valid_pub[1024];
    char   plugin_path[1024];
    char   corrupt_path[1024];
    char  *key_path  = NULL;
    herr_t ret_value = SUCCEED;

    TESTING("invalid PEM file handling");

    /* Create KeyStore directory */
    keystore_dir = create_keystore_directory(PLUGIN_DIR, "test_keystore_corrupted", 0755);
    if (!keystore_dir) {
        H5_FAILED();
        fprintf(stderr, "Failed to create keystore directory\n");
        return FAIL;
    }

    /* Generate 1 valid key pair */
    snprintf(valid_priv, sizeof(valid_priv), "%s/valid_private.pem", PLUGIN_DIR);
    snprintf(valid_pub, sizeof(valid_pub), "%s/valid_public.pem", PLUGIN_DIR);
    if (generate_rsa_keypair(2048, valid_priv, valid_pub) < 0) {
        H5_FAILED();
        fprintf(stderr, "Failed to generate valid key pair\n");
        goto error;
    }

    /* Add valid key to KeyStore */
    key_path = add_key_to_keystore(keystore_dir, "validkey.pem", valid_pub);
    if (key_path)
        free(key_path);

    /* Create 4 corrupted PEM files */
    snprintf(corrupt_path, sizeof(corrupt_path), "%s/truncated.pem", keystore_dir);
    create_corrupted_pem(corrupt_path, PEM_CORRUPT_TRUNCATED);

    snprintf(corrupt_path, sizeof(corrupt_path), "%s/garbage.pem", keystore_dir);
    create_corrupted_pem(corrupt_path, PEM_CORRUPT_GARBAGE);

    snprintf(corrupt_path, sizeof(corrupt_path), "%s/nomarkers.pem", keystore_dir);
    create_corrupted_pem(corrupt_path, PEM_CORRUPT_WRONG_FORMAT);

    snprintf(corrupt_path, sizeof(corrupt_path), "%s/ecdsa.pem", keystore_dir);
    create_corrupted_pem(corrupt_path, PEM_CORRUPT_WRONG_KEY_TYPE);

    /* Create plugin signed with valid key */
    snprintf(plugin_path, sizeof(plugin_path), "%s/plugin_valid.so", PLUGIN_DIR);
    if (create_dummy_plugin(plugin_path) < 0 || sign_plugin_file(plugin_path, valid_priv) < 0) {
        H5_FAILED();
        fprintf(stderr, "Failed to create/sign plugin\n");
        goto error;
    }

    /* Set environment variable to use this KeyStore */
    snprintf(keystore_path, sizeof(keystore_path), "%s", keystore_dir);
    HDsetenv("HDF5_PLUGIN_KEYSTORE", keystore_path, 1);
    reset_keystore_state();

    /* Verify plugin (should succeed - corrupted files silently skipped) */
    if (H5PL__verify_signature_appended(plugin_path) < 0) {
        H5_FAILED();
        fprintf(stderr, "Valid plugin was rejected despite corrupted PEM files in KeyStore\n");
        HDunsetenv("HDF5_PLUGIN_KEYSTORE");
        goto error;
    }

    /* Cleanup */
    HDunsetenv("HDF5_PLUGIN_KEYSTORE");
    free(keystore_dir);

    PASSED();
    return SUCCEED;

error:
    if (keystore_dir)
        free(keystore_dir);
    return FAIL;
}

/*-------------------------------------------------------------------------
 * Function:    test_rsa4096_signature
 *
 * Purpose:     Test support for RSA-4096 keys (512-byte signatures)
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
test_rsa4096_signature(void)
{
    char   priv4096[1024], pub4096[1024];
    char   plugin_path[1024];
    char  *keystore_dir = NULL;
    char   keystore_path[1024];
    char  *key_path  = NULL;
    herr_t ret_value = SUCCEED;

    TESTING("RSA-4096 signature verification");

    /* Create KeyStore directory */
    keystore_dir = create_keystore_directory(PLUGIN_DIR, "test_keystore_rsa4096", 0755);
    if (!keystore_dir) {
        H5_FAILED();
        fprintf(stderr, "Failed to create keystore directory\n");
        return FAIL;
    }

    /* Generate RSA-4096 key pair */
    snprintf(priv4096, sizeof(priv4096), "%s/test_private_4096.pem", PLUGIN_DIR);
    snprintf(pub4096, sizeof(pub4096), "%s/test_public_4096.pem", PLUGIN_DIR);
    if (generate_rsa_keypair(4096, priv4096, pub4096) < 0) {
        H5_FAILED();
        fprintf(stderr, "Failed to generate RSA-4096 key pair\n");
        goto error;
    }

    /* Add public key to KeyStore */
    key_path = add_key_to_keystore(keystore_dir, "rsa4096.pem", pub4096);
    if (key_path)
        free(key_path);

    /* Create and sign plugin with RSA-4096 key */
    snprintf(plugin_path, sizeof(plugin_path), "%s/plugin_rsa4096.so", PLUGIN_DIR);
    if (create_dummy_plugin(plugin_path) < 0 || sign_plugin_file(plugin_path, priv4096) < 0) {
        H5_FAILED();
        fprintf(stderr, "Failed to create/sign plugin with RSA-4096\n");
        goto error;
    }

    /* Set environment variable to use this KeyStore */
    snprintf(keystore_path, sizeof(keystore_path), "%s", keystore_dir);
    HDsetenv("HDF5_PLUGIN_KEYSTORE", keystore_path, 1);
    reset_keystore_state();

    /* Verify plugin (RSA-4096 signature is 512 bytes, within 1024-byte limit) */
    if (H5PL__verify_signature_appended(plugin_path) < 0) {
        H5_FAILED();
        fprintf(stderr, "RSA-4096 signed plugin was rejected\n");
        HDunsetenv("HDF5_PLUGIN_KEYSTORE");
        goto error;
    }

    /* Cleanup */
    HDunsetenv("HDF5_PLUGIN_KEYSTORE");
    free(keystore_dir);

    PASSED();
    return SUCCEED;

error:
    if (keystore_dir)
        free(keystore_dir);
    return FAIL;
}

/*-------------------------------------------------------------------------
 * Function:    test_keystore_symlink_rejection
 *
 * Purpose:     Test that symbolic links in KeyStore are rejected
 *              (Unix/Linux only - prevents symlink attacks)
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
test_keystore_symlink_rejection(void)
{
#ifndef H5_HAVE_WIN32_API
    char  *keystore_dir = NULL;
    char   keystore_path[1024];
    char   trusted_priv[1024], trusted_pub[1024];
    char   attacker_priv[1024], attacker_pub[1024];
    char   symlink_path[1024];
    char   plugin_trusted[1024], plugin_attacker[1024];
    char  *key_path = NULL;
    herr_t status;
    herr_t ret_value = SUCCEED;

    TESTING("symlink rejection in keystore");

    /* Create KeyStore directory */
    keystore_dir = create_keystore_directory(PLUGIN_DIR, "test_keystore_symlinks", 0755);
    if (!keystore_dir) {
        H5_FAILED();
        fprintf(stderr, "Failed to create keystore directory\n");
        return FAIL;
    }

    /* Generate trusted key pair */
    snprintf(trusted_priv, sizeof(trusted_priv), "%s/trusted_private.pem", PLUGIN_DIR);
    snprintf(trusted_pub, sizeof(trusted_pub), "%s/trusted_public.pem", PLUGIN_DIR);
    if (generate_rsa_keypair(2048, trusted_priv, trusted_pub) < 0) {
        H5_FAILED();
        fprintf(stderr, "Failed to generate trusted key pair\n");
        goto error;
    }

    /* Generate attacker key pair (outside KeyStore) */
    snprintf(attacker_priv, sizeof(attacker_priv), "%s/attacker_private.pem", PLUGIN_DIR);
    snprintf(attacker_pub, sizeof(attacker_pub), "%s/attacker_public.pem", PLUGIN_DIR);
    if (generate_rsa_keypair(2048, attacker_priv, attacker_pub) < 0) {
        H5_FAILED();
        fprintf(stderr, "Failed to generate attacker key pair\n");
        goto error;
    }

    /* Add legitimate key to KeyStore */
    key_path = add_key_to_keystore(keystore_dir, "trusted.pem", trusted_pub);
    if (key_path)
        free(key_path);

    /* Create symlink in KeyStore pointing to attacker key */
    snprintf(symlink_path, sizeof(symlink_path), "%s/bad.pem", keystore_dir);
    if (symlink(attacker_pub, symlink_path) < 0) {
        H5_FAILED();
        fprintf(stderr, "Failed to create symlink\n");
        goto error;
    }

    /* Create 2 plugins */
    snprintf(plugin_trusted, sizeof(plugin_trusted), "%s/plugin_trusted.so", PLUGIN_DIR);
    snprintf(plugin_attacker, sizeof(plugin_attacker), "%s/plugin_attacker.so", PLUGIN_DIR);

    if (create_dummy_plugin(plugin_trusted) < 0 || sign_plugin_file(plugin_trusted, trusted_priv) < 0) {
        H5_FAILED();
        fprintf(stderr, "Failed to create/sign trusted plugin\n");
        goto error;
    }

    if (create_dummy_plugin(plugin_attacker) < 0 || sign_plugin_file(plugin_attacker, attacker_priv) < 0) {
        H5_FAILED();
        fprintf(stderr, "Failed to create/sign attacker plugin\n");
        goto error;
    }

    /* Set environment variable to use this KeyStore */
    snprintf(keystore_path, sizeof(keystore_path), "%s", keystore_dir);
    HDsetenv("HDF5_PLUGIN_KEYSTORE", keystore_path, 1);
    reset_keystore_state();

    /* Verify trusted plugin should succeed */
    if (H5PL__verify_signature_appended(plugin_trusted) < 0) {
        H5_FAILED();
        fprintf(stderr, "Trusted plugin was rejected\n");
        HDunsetenv("HDF5_PLUGIN_KEYSTORE");
        goto error;
    }

    /* Verify attacker plugin should fail (symlink was skipped) */
    H5E_BEGIN_TRY
    {
        status = H5PL__verify_signature_appended(plugin_attacker);
    }
    H5E_END_TRY;

    if (status >= 0) {
        H5_FAILED();
        fprintf(stderr, "Attacker plugin was incorrectly accepted (symlink not skipped)\n");
        HDunsetenv("HDF5_PLUGIN_KEYSTORE");
        goto error;
    }

    /* Cleanup */
    HDunsetenv("HDF5_PLUGIN_KEYSTORE");
    free(keystore_dir);

    PASSED();
    return SUCCEED;

error:
    if (keystore_dir)
        free(keystore_dir);
    return FAIL;

#else
    /* Windows - skip test */
    TESTING("symlink rejection in keystore (Unix only)");
    SKIPPED();
    return SUCCEED;
#endif
}

/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     Run plugin signature verification tests
 *
 * Return:      EXIT_SUCCESS/EXIT_FAILURE
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    int nerrors = 0;

    printf("Testing HDF5 Plugin Signature Verification\n");
    printf("==========================================\n\n");

    /* Open the HDF5 library explicitly */
    H5open();

    /* Set up test environment */
    if (setup_test_environment() < 0) {
        fprintf(stderr, "Failed to set up test environment\n");
        return EXIT_FAILURE;
    }

    /* Run tests */
    nerrors += test_valid_signed_plugin() < 0 ? 1 : 0;
    nerrors += test_unsigned_plugin_rejected() < 0 ? 1 : 0;
    nerrors += test_tampered_plugin_rejected() < 0 ? 1 : 0;
    nerrors += test_bad_signature_rejected() < 0 ? 1 : 0;
    nerrors += test_no_footer_rejected() < 0 ? 1 : 0;
    nerrors += test_corrupt_magic_rejected() < 0 ? 1 : 0;

    /* Run KeyStore tests */
    nerrors += test_keystore_multiple_keys() < 0 ? 1 : 0;
    nerrors += test_invalid_pem_file_handling() < 0 ? 1 : 0;
    nerrors += test_rsa4096_signature() < 0 ? 1 : 0;
    nerrors += test_keystore_symlink_rejection() < 0 ? 1 : 0;

    /* Clean up */
    cleanup_test_environment();

    /* Report results */
    if (nerrors) {
        printf("\n***** %d PLUGIN SIGNATURE VERIFICATION TEST%s FAILED *****\n", nerrors,
               nerrors > 1 ? "S" : "");
        return EXIT_FAILURE;
    }

    printf("\nAll plugin signature verification tests passed.\n");
    return EXIT_SUCCESS;
}

#else /* H5_REQUIRE_DIGITAL_SIGNATURE */

int
main(void)
{
    printf("Plugin signature verification is not enabled.\n");
    printf("Reconfigure with -DHDF5_REQUIRE_SIGNED_PLUGINS=ON to enable these tests.\n");
    return EXIT_SUCCESS; /* Not a failure - feature not enabled */
}

#endif /* H5_REQUIRE_DIGITAL_SIGNATURE */
