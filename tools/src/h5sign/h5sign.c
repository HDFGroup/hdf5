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
 * Purpose:  Sign HDF5 plugin files with RSA digital signatures
 *
 *           This tool appends an RSA signature to a plugin binary file using
 *           the format expected by HDF5's plugin signature verification system.
 *
 *           File format after signing:
 *             [ Plugin Binary ] [ RSA Signature ] [ Footer ]
 *
 *           Footer contains:
 *             - Signature length (4 bytes, little-endian)
 *             - Magic number 0x48444635 "HDF5" (4 bytes, little-endian)
 *
 *           The plugin binary loader ignores trailing data, so signed plugins
 *           load normally on all platforms.
 */

#include "hdf5.h"
#include "H5private.h"
#include "h5tools.h"
#include "h5tools_utils.h"

/* Include signature header for footer format and magic number */
#include "H5PLsig.h"
#include "H5encode.h"

/* OpenSSL headers for RSA signing */
#include <openssl/evp.h>
#include <openssl/pem.h>
#include <openssl/bio.h>
#include <openssl/err.h>

/* On Windows, OpenSSL requires applink to bridge different CRT versions */
#ifdef _MSC_VER
#include <openssl/applink.c>
#endif

/* Name of tool */
#define PROGRAMNAME "h5sign"

/* Hash algorithm for signing (SHA-256) */
#define HASH_ALGORITHM    EVP_sha256()
#define HASH_ALGORITHM_ID H5PL_SIG_ALGO_SHA256

/* Maximum plugin file size (1GB - prevents unreasonable allocations) */
#define MAX_PLUGIN_SIZE ((hsize_t)(1024ULL * 1024ULL * 1024ULL))

/* I/O chunk size for hashing (64KB) */
#define HASH_CHUNK_SIZE ((size_t)(64 * 1024))

/* Global options */
static char *plugin_file  = NULL;
static char *privkey_file = NULL;
static int   opt_verbose  = 0;

/*
 * Command-line options: The user can specify short or long-named
 * parameters.
 */
static const char            *s_opts   = "hp:k:vV";
static struct h5_long_options l_opts[] = {{"help", no_arg, 'h'},
                                          {"plugin", require_arg, 'p'},
                                          {"key", require_arg, 'k'},
                                          {"verbose", no_arg, 'v'},
                                          {NULL, 0, '\0'}};

/*-------------------------------------------------------------------------
 * Function:    usage
 *
 * Purpose:     Print usage message
 *
 * Return:      void
 *-------------------------------------------------------------------------
 */
static void
usage(const char *prog)
{
    fflush(rawoutstream);
    fprintf(rawoutstream, "usage: %s -p <plugin> -k <private-key.pem> [OPTIONS]\n", prog);
    fprintf(rawoutstream, "\n");
    fprintf(rawoutstream, "Sign an HDF5 plugin with RSA digital signature.\n");
    fprintf(rawoutstream, "\n");
    fprintf(rawoutstream, "REQUIRED OPTIONS\n");
    fprintf(rawoutstream, "  -p, --plugin <file>     Plugin binary to sign (.so, .dll, .dylib)\n");
    fprintf(rawoutstream, "  -k, --key <file>        RSA private key in PEM format\n");
    fprintf(rawoutstream, "\n");
    fprintf(rawoutstream, "OTHER OPTIONS\n");
    fprintf(rawoutstream, "  -v, --verbose           Verbose output (show signature details)\n");
    fprintf(rawoutstream, "  -h, --help              Print this help message\n");
    fprintf(rawoutstream, "  -V                      Print HDF5 library version\n");
    fprintf(rawoutstream, "\n");
    fprintf(rawoutstream, "DESCRIPTION\n");
    fprintf(rawoutstream, "  This tool appends an RSA signature to a plugin file. The signature\n");
    fprintf(rawoutstream, "  allows HDF5 to verify the plugin's authenticity when loading.\n");
    fprintf(rawoutstream, "\n");
    fprintf(rawoutstream, "  The plugin file is modified in-place by appending:\n");
    fprintf(rawoutstream, "    1. RSA signature of the plugin binary (SHA-256 hash)\n");
    fprintf(rawoutstream, "    2. Footer with signature metadata and magic number\n");
    fprintf(rawoutstream, "\n");
    fprintf(rawoutstream, "  The binary loader ignores trailing data, so the signed plugin\n");
    fprintf(rawoutstream, "  loads normally on all platforms.\n");
    fprintf(rawoutstream, "\n");
    fprintf(rawoutstream, "EXAMPLES\n");
    fprintf(rawoutstream, "  # Sign a plugin with a private key\n");
    fprintf(rawoutstream, "  %s -p libmyplugin.so -k private.pem\n", prog);
    fprintf(rawoutstream, "\n");
    fprintf(rawoutstream, "  # Sign with verbose output\n");
    fprintf(rawoutstream, "  %s -p libmyplugin.so -k private.pem -v\n", prog);
    fprintf(rawoutstream, "\n");
    fprintf(rawoutstream, "KEY GENERATION\n");
    fprintf(rawoutstream, "  To generate an RSA key pair:\n");
    fprintf(rawoutstream, "    openssl genrsa -out private.pem 2048\n");
    fprintf(rawoutstream, "    openssl rsa -in private.pem -pubout -out public.pem\n");
    fprintf(rawoutstream, "\n");
    fprintf(rawoutstream, "  Keep the private key secure! Use the public key when building HDF5\n");
    fprintf(rawoutstream, "  with signature verification enabled.\n");
    fprintf(rawoutstream, "\n");
    fprintf(rawoutstream, "SECURITY NOTES\n");
    fprintf(rawoutstream, "  - Keep your private key secure (chmod 600 private.pem)\n");
    fprintf(rawoutstream, "  - Never share or commit your private key to version control\n");
    fprintf(rawoutstream, "  - Verify plugin code before signing\n");
    fprintf(rawoutstream, "  - Use strong keys (2048-bit minimum, 4096-bit recommended)\n");
    fprintf(rawoutstream, "\n");
    fprintf(rawoutstream, "Exit Status:\n");
    fprintf(rawoutstream, "   0   Successfully signed the plugin\n");
    fprintf(rawoutstream, "   1   An error occurred\n");
}

/*-------------------------------------------------------------------------
 * Function:    leave
 *
 * Purpose:     Shutdown and call exit()
 *
 * Return:      Does not return
 *-------------------------------------------------------------------------
 */
static void
leave(int ret)
{
    if (plugin_file)
        free(plugin_file);
    if (privkey_file)
        free(privkey_file);

    h5tools_close();
    exit(ret);
}

/*-------------------------------------------------------------------------
 * Function:    parse_command_line
 *
 * Purpose:     Parse command line arguments
 *
 * Return:      Success: SUCCEED
 *              Failure: FAIL (exits program)
 *-------------------------------------------------------------------------
 */
static herr_t
parse_command_line(int argc, const char *const *argv)
{
    int opt;

    /* Parse command line options */
    while ((opt = H5_get_option(argc, argv, s_opts, l_opts)) != EOF) {
        switch ((char)opt) {
            case 'p':
                if (plugin_file)
                    free(plugin_file);
                plugin_file = strdup(H5_optarg);
                break;
            case 'k':
                if (privkey_file)
                    free(privkey_file);
                privkey_file = strdup(H5_optarg);
                break;
            case 'v':
                opt_verbose = 1;
                break;
            case 'h':
                usage(h5tools_getprogname());
                leave(EXIT_SUCCESS);
                break;
            case 'V':
                print_version(h5tools_getprogname());
                leave(EXIT_SUCCESS);
                break;
            case '?':
            default:
                usage(h5tools_getprogname());
                leave(EXIT_FAILURE);
        }
    }

    /* Check required arguments */
    if (!plugin_file) {
        fprintf(rawerrorstream, "Error: Plugin file (-p) is required\n\n");
        usage(h5tools_getprogname());
        leave(EXIT_FAILURE);
    }

    if (!privkey_file) {
        fprintf(rawerrorstream, "Error: Private key file (-k) is required\n\n");
        usage(h5tools_getprogname());
        leave(EXIT_FAILURE);
    }

    return SUCCEED;
}

/*-------------------------------------------------------------------------
 * Function:    read_private_key
 *
 * Purpose:     Read RSA private key from PEM file
 *
 * Return:      Success: EVP_PKEY pointer
 *              Failure: NULL
 *-------------------------------------------------------------------------
 */
static EVP_PKEY *
read_private_key(const char *keyfile)
{
    FILE     *fp       = NULL;
    EVP_PKEY *pkey     = NULL;
    EVP_PKEY *ret_pkey = NULL;

    /* Open key file */
    if (NULL == (fp = fopen(keyfile, "r"))) {
        fprintf(rawerrorstream, "Error: Cannot open private key file '%s': %s\n", keyfile, strerror(errno));
        goto done;
    }

    /* Read private key using OpenSSL's PEM reader */
    if (NULL == (pkey = PEM_read_PrivateKey(fp, NULL, NULL, NULL))) {
        unsigned long ssl_err = ERR_get_error();
        char          err_buf[256];
        ERR_error_string_n(ssl_err, err_buf, sizeof(err_buf));
        fprintf(rawerrorstream, "Error: Cannot read private key from '%s': %s\n", keyfile, err_buf);
        fprintf(rawerrorstream, "       Make sure the file is in PEM format.\n");
        goto done;
    }

    /* Verify it's an RSA key */
    if (EVP_PKEY_base_id(pkey) != EVP_PKEY_RSA && EVP_PKEY_base_id(pkey) != EVP_PKEY_RSA_PSS) {
        fprintf(rawerrorstream, "Error: Key file '%s' is not an RSA key\n", keyfile);
        fprintf(rawerrorstream, "       Only RSA keys are supported for plugin signing.\n");
        goto done;
    }

    ret_pkey = pkey;
    pkey     = NULL; /* Prevent cleanup */

done:
    if (fp)
        fclose(fp);
    if (pkey)
        EVP_PKEY_free(pkey);

    /* Clear OpenSSL error queue */
    ERR_clear_error();

    return ret_pkey;
}

/*-------------------------------------------------------------------------
 * Function:    sign_plugin_file
 *
 * Purpose:     Sign a plugin file by computing SHA-256 hash and creating
 *              RSA signature, then appending signature and footer to file
 *
 * Return:      Success: SUCCEED
 *              Failure: FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
sign_plugin_file(const char *plugin_path, EVP_PKEY *private_key)
{
    int               fd = -1;
    h5_stat_t         st;
    hsize_t           file_size   = 0;
    unsigned char    *hash_buffer = NULL;
    unsigned char    *signature   = NULL;
    size_t            sig_len     = 0;
    EVP_MD_CTX       *mdctx       = NULL;
    EVP_PKEY_CTX     *pkey_ctx    = NULL;
    H5PL_sig_footer_t footer;
    herr_t            ret_value     = SUCCEED;
    hsize_t           bytes_read    = 0;
    hsize_t           total_to_read = 0;
    int               append_fd     = -1;

    /* Open plugin file for reading */
    if ((fd = HDopen(plugin_path, O_RDONLY, 0)) < 0) {
        fprintf(rawerrorstream, "Error: Cannot open plugin file '%s': %s\n", plugin_path, strerror(errno));
        ret_value = FAIL;
        goto done;
    }

    /* Get file size */
    if (HDfstat(fd, &st) < 0) {
        fprintf(rawerrorstream, "Error: Cannot get file size for '%s': %s\n", plugin_path, strerror(errno));
        ret_value = FAIL;
        goto done;
    }

    file_size = (hsize_t)st.st_size;

    /* Sanity check file size */
    if (file_size == 0) {
        fprintf(rawerrorstream, "Error: Plugin file '%s' is empty\n", plugin_path);
        ret_value = FAIL;
        goto done;
    }

    if (file_size > MAX_PLUGIN_SIZE) {
        fprintf(rawerrorstream, "Error: Plugin file '%s' is too large (%llu bytes)\n", plugin_path,
                (unsigned long long)file_size);
        fprintf(rawerrorstream, "       Maximum size is %llu bytes (1GB)\n",
                (unsigned long long)MAX_PLUGIN_SIZE);
        ret_value = FAIL;
        goto done;
    }

    if (opt_verbose) {
        fprintf(rawoutstream, "Plugin file: %s\n", plugin_path);
        fprintf(rawoutstream, "File size:   %llu bytes\n", (unsigned long long)file_size);
    }

    /* Create message digest context */
    if (NULL == (mdctx = EVP_MD_CTX_new())) {
        unsigned long ssl_err = ERR_get_error();
        char          err_buf[256];
        ERR_error_string_n(ssl_err, err_buf, sizeof(err_buf));
        fprintf(rawerrorstream, "Error: Cannot create message digest context: %s\n", err_buf);
        ret_value = FAIL;
        goto done;
    }

    /* Initialize signing context with SHA-256 */
    if (1 != EVP_DigestSignInit(mdctx, &pkey_ctx, HASH_ALGORITHM, NULL, private_key)) {
        unsigned long ssl_err = ERR_get_error();
        char          err_buf[256];
        ERR_error_string_n(ssl_err, err_buf, sizeof(err_buf));
        fprintf(rawerrorstream, "Error: Cannot initialize signing context: %s\n", err_buf);
        ret_value = FAIL;
        goto done;
    }

    /* Allocate buffer for reading file in chunks */
    if (NULL == (hash_buffer = (unsigned char *)malloc(HASH_CHUNK_SIZE))) {
        fprintf(rawerrorstream, "Error: Cannot allocate hash buffer\n");
        ret_value = FAIL;
        goto done;
    }

    /* Read file in chunks and update hash */
    total_to_read = file_size;
    bytes_read    = 0;

    if (opt_verbose)
        fprintf(rawoutstream, "Computing SHA-256 hash...\n");

    while (bytes_read < file_size) {
        size_t chunk_size =
            (size_t)((file_size - bytes_read) > HASH_CHUNK_SIZE ? HASH_CHUNK_SIZE : (file_size - bytes_read));
        h5_posix_io_ret_t read_result = 0;

        /* Read chunk with EINTR retry */
        do {
            read_result = HDread(fd, hash_buffer, chunk_size);
        } while (-1 == read_result && EINTR == errno);

        if (read_result < 0) {
            fprintf(rawerrorstream, "Error: Cannot read from plugin file '%s': %s\n", plugin_path,
                    strerror(errno));
            ret_value = FAIL;
            goto done;
        }

        if (read_result == 0) {
            fprintf(rawerrorstream, "Error: Unexpected end of file in '%s'\n", plugin_path);
            ret_value = FAIL;
            goto done;
        }

        /* Update hash with chunk */
        if (1 != EVP_DigestSignUpdate(mdctx, hash_buffer, (size_t)read_result)) {
            unsigned long ssl_err = ERR_get_error();
            char          err_buf[256];
            ERR_error_string_n(ssl_err, err_buf, sizeof(err_buf));
            fprintf(rawerrorstream, "Error: Cannot update hash: %s\n", err_buf);
            ret_value = FAIL;
            goto done;
        }

        bytes_read += (hsize_t)read_result;
    }

    /* Close read file descriptor */
    HDclose(fd);
    fd = -1;

    if (opt_verbose)
        fprintf(rawoutstream, "Hash computed successfully\n");

    /* Get signature length */
    if (1 != EVP_DigestSignFinal(mdctx, NULL, &sig_len)) {
        unsigned long ssl_err = ERR_get_error();
        char          err_buf[256];
        ERR_error_string_n(ssl_err, err_buf, sizeof(err_buf));
        fprintf(rawerrorstream, "Error: Cannot get signature length: %s\n", err_buf);
        ret_value = FAIL;
        goto done;
    }

    if (sig_len == 0 || sig_len > 8192) {
        fprintf(rawerrorstream, "Error: Invalid signature length: %zu bytes\n", sig_len);
        ret_value = FAIL;
        goto done;
    }

    /* Allocate signature buffer */
    if (NULL == (signature = (unsigned char *)malloc(sig_len))) {
        fprintf(rawerrorstream, "Error: Cannot allocate signature buffer\n");
        ret_value = FAIL;
        goto done;
    }

    /* Compute signature */
    if (1 != EVP_DigestSignFinal(mdctx, signature, &sig_len)) {
        unsigned long ssl_err = ERR_get_error();
        char          err_buf[256];
        ERR_error_string_n(ssl_err, err_buf, sizeof(err_buf));
        fprintf(rawerrorstream, "Error: Cannot compute signature: %s\n", err_buf);
        ret_value = FAIL;
        goto done;
    }

    if (opt_verbose) {
        fprintf(rawoutstream, "Signature created successfully\n");
        fprintf(rawoutstream, "Signature length: %zu bytes\n", sig_len);
    }

    /* Open file for appending */
    if ((append_fd = HDopen(plugin_path, O_WRONLY | O_APPEND, 0)) < 0) {
        fprintf(rawerrorstream, "Error: Cannot open plugin file for appending '%s': %s\n", plugin_path,
                strerror(errno));
        ret_value = FAIL;
        goto done;
    }

    /* Append signature to file */
    {
        size_t            written      = 0;
        size_t            to_write     = sig_len;
        unsigned char    *write_ptr    = signature;
        h5_posix_io_ret_t write_result = 0;

        while (written < sig_len) {
            do {
                write_result = HDwrite(append_fd, write_ptr, to_write);
            } while (-1 == write_result && EINTR == errno);

            if (write_result < 0) {
                fprintf(rawerrorstream, "Error: Cannot write signature to '%s': %s\n", plugin_path,
                        strerror(errno));
                ret_value = FAIL;
                goto done;
            }

            written += (size_t)write_result;
            write_ptr += write_result;
            to_write -= (size_t)write_result;
        }
    }

    if (opt_verbose)
        fprintf(rawoutstream, "Signature appended to plugin\n");

    /* Prepare footer in little-endian format */
    {
        uint8_t  footer_buf[12];
        uint8_t *p = footer_buf;

        /* Encode signature length as little-endian uint32 */
        UINT32ENCODE(p, (uint32_t)sig_len);
        /* Encode algorithm ID (1 byte) */
        *p++ = HASH_ALGORITHM_ID;
        /* Encode format version (1 byte, v1.0 = 1) */
        *p++ = 1;
        /* Encode reserved bytes (2 bytes, must be 0) */
        UINT16ENCODE(p, (uint16_t)0);
        /* Encode magic number as little-endian uint32 */
        UINT32ENCODE(p, H5PL_SIG_MAGIC);

        /* Write footer to file */
        h5_posix_io_ret_t write_result = 0;
        do {
            write_result = HDwrite(append_fd, footer_buf, sizeof(footer_buf));
        } while (-1 == write_result && EINTR == errno);

        if (write_result != sizeof(footer_buf)) {
            fprintf(rawerrorstream, "Error: Cannot write footer to '%s': %s\n", plugin_path, strerror(errno));
            ret_value = FAIL;
            goto done;
        }
    }

    /* Close append file descriptor */
    HDclose(append_fd);
    append_fd = -1;

    if (opt_verbose)
        fprintf(rawoutstream, "Footer written successfully\n");

    /* Success! */
    fprintf(rawoutstream, "\nPlugin signed successfully!\n");
    fprintf(rawoutstream, "  File:           %s\n", plugin_path);
    fprintf(rawoutstream, "  Original size:  %llu bytes\n", (unsigned long long)file_size);
    fprintf(rawoutstream, "  Hash algorithm: ");
    switch (HASH_ALGORITHM_ID) {
        case H5PL_SIG_ALGO_SHA256:
            fprintf(rawoutstream, "SHA-256 (0x%02X)\n", HASH_ALGORITHM_ID);
            break;
        case H5PL_SIG_ALGO_SHA384:
            fprintf(rawoutstream, "SHA-384 (0x%02X)\n", HASH_ALGORITHM_ID);
            break;
        case H5PL_SIG_ALGO_SHA512:
            fprintf(rawoutstream, "SHA-512 (0x%02X)\n", HASH_ALGORITHM_ID);
            break;
        default:
            fprintf(rawoutstream, "0x%02X\n", HASH_ALGORITHM_ID);
            break;
    }
    fprintf(rawoutstream, "  Signature size: %zu bytes\n", sig_len);
    fprintf(rawoutstream, "  Footer size:    8 bytes\n");
    fprintf(rawoutstream, "  Final size:     %llu bytes\n",
            (unsigned long long)(file_size + sig_len + H5PL_SIG_FOOTER_SIZE));
    fprintf(rawoutstream, "\n");

done:
    if (fd >= 0)
        HDclose(fd);
    if (append_fd >= 0)
        HDclose(append_fd);
    if (hash_buffer)
        free(hash_buffer);
    if (signature)
        free(signature);
    if (mdctx)
        EVP_MD_CTX_free(mdctx);

    /* Clear OpenSSL error queue */
    ERR_clear_error();

    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     HDF5 plugin signing tool
 *
 * Return:      Success: EXIT_SUCCESS
 *              Failure: EXIT_FAILURE
 *-------------------------------------------------------------------------
 */
int
main(int argc, char *argv[])
{
    EVP_PKEY *private_key = NULL;
    int       ret_value   = EXIT_SUCCESS;

    /* Initialize HDF5 tools infrastructure */
    h5tools_setprogname(PROGRAMNAME);
    h5tools_setstatus(EXIT_SUCCESS);

    /* Initialize h5tools lib */
    h5tools_init();

    /* Parse command line */
    if (parse_command_line(argc, (const char *const *)argv) < 0) {
        ret_value = EXIT_FAILURE;
        goto done;
    }

    fprintf(rawoutstream, "HDF5 Plugin Signature Tool\n");
    fprintf(rawoutstream, "===========================\n\n");

    /* Read private key */
    fprintf(rawoutstream, "Reading private key from '%s'...\n", privkey_file);
    if (NULL == (private_key = read_private_key(privkey_file))) {
        ret_value = EXIT_FAILURE;
        goto done;
    }
    fprintf(rawoutstream, "Private key loaded successfully\n\n");

    /* Sign the plugin */
    fprintf(rawoutstream, "Signing plugin '%s'...\n\n", plugin_file);
    if (sign_plugin_file(plugin_file, private_key) < 0) {
        ret_value = EXIT_FAILURE;
        goto done;
    }

    fprintf(rawoutstream, "SECURITY REMINDERS:\n");
    fprintf(rawoutstream, "  - Keep your private key secure (chmod 600 %s)\n", privkey_file);
    fprintf(rawoutstream, "  - Never share or commit your private key\n");
    fprintf(rawoutstream, "  - Test the signed plugin before deployment\n");
    fprintf(rawoutstream, "\n");

done:
    if (private_key)
        EVP_PKEY_free(private_key);

    leave(ret_value);

    return ret_value;
}
