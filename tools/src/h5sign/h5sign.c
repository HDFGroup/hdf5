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
 *           Footer (14 bytes, little-endian where applicable):
 *             - Algorithm ID      (1 byte)
 *             - Signature length  (4 bytes)
 *             - Magic number      (8 bytes, non-ASCII)
 *             - Format version    (1 byte)
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

/* Use the shared maximum plugin size from H5PLsig.h */

/* I/O chunk size for hashing (64KB) */
#define HASH_CHUNK_SIZE ((size_t)(64 * 1024))

/* Global options */
static char *plugin_file   = NULL;
static char *privkey_file  = NULL;
static char *opt_algorithm = NULL;
static int   opt_verbose   = 0;
static int   opt_force     = 0;

/*
 * Command-line options: The user can specify short or long-named
 * parameters.
 */
static const char            *s_opts   = "hp:k:a:fvV";
static struct h5_long_options l_opts[] = {{"help", no_arg, 'h'},     {"plugin", require_arg, 'p'},
                                          {"key", require_arg, 'k'}, {"algorithm", require_arg, 'a'},
                                          {"force", no_arg, 'f'},    {"verbose", no_arg, 'v'},
                                          {"version", no_arg, 'V'},  {NULL, 0, '\0'}};

/*-------------------------------------------------------------------------
 * Function:    write_with_retry
 *
 * Purpose:     Write data to a file descriptor, handling partial writes
 *              and EINTR interrupts. On failure, truncate the file to
 *              rollback_size to restore it to its pre-signing state.
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
write_with_retry(int fd, const unsigned char *data, size_t total, const char *what, const char *plugin_path,
                 hsize_t rollback_size)
{
    size_t written = 0;

    while (written < total) {
        h5_posix_io_ret_t wr;
        do {
            wr = HDwrite(fd, data + written, total - written);
        } while (-1 == wr && EINTR == errno);

        if (wr <= 0) {
            fprintf(rawerrorstream, "Error: Cannot write %s to '%s': %s\n", what, plugin_path,
                    strerror(errno));
            /* Attempt rollback: restore file to its pre-signing state */
            (void)HDftruncate(fd, (HDoff_t)rollback_size);
            return FAIL;
        }

        written += (size_t)wr;
    }

    return SUCCEED;
}

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
    fprintf(rawoutstream, "  -a, --algorithm <alg>   Hash algorithm: sha256, sha384, sha512,\n");
    fprintf(rawoutstream, "                          sha256-pss, sha384-pss, sha512-pss\n");
    fprintf(rawoutstream, "                          (default: sha512)\n");
    fprintf(rawoutstream, "  -f, --force             Re-sign an already-signed plugin by stripping\n");
    fprintf(rawoutstream, "                          the existing signature before signing\n");
    fprintf(rawoutstream, "  -v, --verbose           Verbose output (show signature details)\n");
    fprintf(rawoutstream, "  -h, --help              Print this help message\n");
    fprintf(rawoutstream, "  -V, --version           Print version number and exit\n");
    fprintf(rawoutstream, "\n");
    fprintf(rawoutstream, "DESCRIPTION\n");
    fprintf(rawoutstream, "  This tool appends an RSA signature to a plugin file. The signature\n");
    fprintf(rawoutstream, "  allows HDF5 to verify the plugin's authenticity when loading.\n");
    fprintf(rawoutstream, "\n");
    fprintf(rawoutstream, "  The plugin file is modified in-place by appending:\n");
    fprintf(rawoutstream, "    1. RSA signature of the plugin binary (configurable hash algorithm)\n");
    fprintf(rawoutstream, "    2. Footer with signature metadata and magic number\n");
    fprintf(rawoutstream, "\n");
    fprintf(rawoutstream, "  The binary loader ignores trailing data, so the signed plugin\n");
    fprintf(rawoutstream, "  loads normally on all platforms.\n");
    fprintf(rawoutstream, "\n");
    fprintf(rawoutstream, "EXAMPLES\n");
    fprintf(rawoutstream, "  # Sign a plugin with a private key (default SHA-512)\n");
    fprintf(rawoutstream, "  %s -p libmyplugin.so -k private.pem\n", prog);
    fprintf(rawoutstream, "\n");
    fprintf(rawoutstream, "  # Sign with SHA-256 hash algorithm\n");
    fprintf(rawoutstream, "  %s -p libmyplugin.so -k private.pem -a sha256\n", prog);
    fprintf(rawoutstream, "\n");
    fprintf(rawoutstream, "  # Sign with verbose output\n");
    fprintf(rawoutstream, "  %s -p libmyplugin.so -k private.pem -v\n", prog);
    fprintf(rawoutstream, "\n");
    fprintf(rawoutstream, "KEY GENERATION\n");
    fprintf(rawoutstream, "  To generate an RSA key pair:\n");
    fprintf(rawoutstream, "    openssl genrsa -out private.pem 4096\n");
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
    if (opt_algorithm)
        free(opt_algorithm);

    h5tools_close();
    exit(ret);
}

/*-------------------------------------------------------------------------
 * Function:    report_openssl_error
 *
 * Purpose:     Print an OpenSSL error message with context
 *
 * Return:      void
 *-------------------------------------------------------------------------
 */
static void
report_openssl_error(const char *context)
{
    unsigned long ssl_err = ERR_get_error();
    char          err_buf[256];
    ERR_error_string_n(ssl_err, err_buf, sizeof(err_buf));
    fprintf(rawerrorstream, "Error: %s: %s\n", context, err_buf);
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
                if (!plugin_file) {
                    fprintf(rawerrorstream, "Error: Out of memory\n");
                    leave(EXIT_FAILURE);
                }
                break;
            case 'k':
                if (privkey_file)
                    free(privkey_file);
                privkey_file = strdup(H5_optarg);
                if (!privkey_file) {
                    fprintf(rawerrorstream, "Error: Out of memory\n");
                    leave(EXIT_FAILURE);
                }
                break;
            case 'a':
                if (opt_algorithm)
                    free(opt_algorithm);
                opt_algorithm = strdup(H5_optarg);
                if (!opt_algorithm) {
                    fprintf(rawerrorstream, "Error: Out of memory\n");
                    leave(EXIT_FAILURE);
                }
                break;
            case 'f':
                opt_force = 1;
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
    BIO      *bio      = NULL;
    EVP_PKEY *pkey     = NULL;
    EVP_PKEY *ret_pkey = NULL;

    /* Open key file using BIO (avoids OPENSSL_Applink issue on Windows) */
    if (NULL == (bio = BIO_new_file(keyfile, "r"))) {
        fprintf(rawerrorstream, "Error: Cannot open private key file '%s'\n", keyfile);
        goto done;
    }

    /* Read private key using OpenSSL's PEM reader */
    if (NULL == (pkey = PEM_read_bio_PrivateKey(bio, NULL, NULL, NULL))) {
        report_openssl_error("Cannot read private key");
        fprintf(rawerrorstream, "       Key file: '%s'\n", keyfile);
        fprintf(rawerrorstream, "       Make sure the file is in PEM format.\n");
        fprintf(rawerrorstream,
                "       If the key is passphrase-protected, re-run interactively so OpenSSL\n");
        fprintf(rawerrorstream, "       can prompt for the passphrase (non-interactive use will fail).\n");
        goto done;
    }

    /* Verify it's an RSA key.
     * EVP_PKEY_is_a() is the preferred API on OpenSSL 3.0+; fall back to
     * EVP_PKEY_base_id() for older versions to avoid deprecation warnings. */
#if OPENSSL_VERSION_NUMBER >= 0x30000000L
    if (!EVP_PKEY_is_a(pkey, "RSA") && !EVP_PKEY_is_a(pkey, "RSA-PSS")) {
#else
    if (EVP_PKEY_base_id(pkey) != EVP_PKEY_RSA && EVP_PKEY_base_id(pkey) != EVP_PKEY_RSA_PSS) {
#endif
        fprintf(rawerrorstream, "Error: Key file '%s' is not an RSA key\n", keyfile);
        fprintf(rawerrorstream, "       Only RSA keys are supported for plugin signing.\n");
        goto done;
    }

    /* Enforce minimum RSA key size (2048-bit minimum; 4096-bit recommended)
     * Use EVP_PKEY_bits() for OpenSSL 1.1.x compatibility; EVP_PKEY_get_bits()
     * was introduced in OpenSSL 3.0 and EVP_PKEY_bits() remains available as
     * a backward-compatible alias. */
    {
        int key_bits = EVP_PKEY_bits(pkey);
        if (key_bits < 2048) {
            fprintf(rawerrorstream, "Error: RSA key in '%s' is too small (%d bits)\n", keyfile, key_bits);
            fprintf(rawerrorstream, "       Minimum required: 2048 bits (4096-bit recommended)\n");
            goto done;
        }
    }

    ret_pkey = pkey;
    pkey     = NULL; /* Prevent cleanup */

done:
    if (bio)
        BIO_free(bio);
    if (pkey)
        EVP_PKEY_free(pkey);

    /* Clear OpenSSL error queue */
    ERR_clear_error();

    return ret_pkey;
}

/*
 * Algorithm lookup table: maps CLI name / algorithm ID to display name
 * and OpenSSL EVP_MD getter.  Shared by parse_algorithm_name() and the
 * success-message printer in sign_plugin_file().
 */
typedef const EVP_MD *(*evp_md_getter_t)(void);

typedef struct {
    const char     *cli_name;     /* Name accepted on the command line */
    const char     *display_name; /* Human-readable name for messages */
    H5PL_sig_algo_t algo_id;      /* H5PL_SIG_ALGO_* constant */
    evp_md_getter_t md_getter;    /* OpenSSL EVP_MD factory function */
} h5sign_algo_entry_t;

/* clang-format off */
static const h5sign_algo_entry_t algo_table[] = {
    {"sha256",     "SHA-256",         H5PL_SIG_ALGO_SHA256,     EVP_sha256},
    {"sha384",     "SHA-384",         H5PL_SIG_ALGO_SHA384,     EVP_sha384},
    {"sha512",     "SHA-512",         H5PL_SIG_ALGO_SHA512,     EVP_sha512},
    {"sha256-pss", "SHA-256/RSA-PSS", H5PL_SIG_ALGO_SHA256_PSS, EVP_sha256},
    {"sha384-pss", "SHA-384/RSA-PSS", H5PL_SIG_ALGO_SHA384_PSS, EVP_sha384},
    {"sha512-pss", "SHA-512/RSA-PSS", H5PL_SIG_ALGO_SHA512_PSS, EVP_sha512},
};
/* clang-format on */

static const size_t algo_table_size = sizeof(algo_table) / sizeof(algo_table[0]);

/*-------------------------------------------------------------------------
 * Function:    algo_display_name
 *
 * Purpose:     Return the display name for a given algorithm ID
 *
 * Return:      Display name string, or NULL if unknown
 *-------------------------------------------------------------------------
 */
static const char *
algo_display_name(H5PL_sig_algo_t algo_id)
{
    for (size_t i = 0; i < algo_table_size; i++) {
        if (algo_table[i].algo_id == algo_id)
            return algo_table[i].display_name;
    }
    return NULL;
}

/*-------------------------------------------------------------------------
 * Function:    parse_algorithm_name
 *
 * Purpose:     Parse algorithm name string and return corresponding
 *              EVP_MD and algorithm ID
 *
 * Return:      Success: SUCCEED
 *              Failure: FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
parse_algorithm_name(const char *name, const EVP_MD **md_out, H5PL_sig_algo_t *algo_id_out)
{
    for (size_t i = 0; i < algo_table_size; i++) {
        if (HDstrcasecmp(name, algo_table[i].cli_name) == 0) {
            *md_out      = algo_table[i].md_getter();
            *algo_id_out = algo_table[i].algo_id;
            return SUCCEED;
        }
    }

    fprintf(rawerrorstream, "Error: Unknown algorithm '%s'\n", name);
    fprintf(rawerrorstream, "Supported: sha256, sha384, sha512, sha256-pss, sha384-pss, sha512-pss\n");
    return FAIL;
}

/*-------------------------------------------------------------------------
 * Function:    sign_plugin_file
 *
 * Purpose:     Sign a plugin file by computing hash and creating
 *              RSA signature, then appending signature and footer to file
 *
 * Return:      Success: SUCCEED
 *              Failure: FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
sign_plugin_file(const char *plugin_path, EVP_PKEY *private_key, const EVP_MD *hash_algorithm,
                 H5PL_sig_algo_t algorithm_id)
{
    int            fd = -1;
    h5_stat_t      st;
    hsize_t        file_size   = 0;
    unsigned char *hash_buffer = NULL;
    unsigned char *signature   = NULL;
    size_t         sig_len     = 0;
    EVP_MD_CTX    *mdctx       = NULL;
    EVP_PKEY_CTX  *pkey_ctx    = NULL;
    herr_t         ret_value   = SUCCEED;
    hsize_t        bytes_read  = 0;

    /* Open plugin file for reading and writing.
     * Keeping a single fd open throughout hashing and appending eliminates
     * the TOCTOU window that would exist if we closed and reopened the file. */
    if ((fd = HDopen(plugin_path, O_RDWR, 0)) < 0) {
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

    /* Detect already-signed files: check for HDF5 magic number in the footer */
    if (file_size >= (hsize_t)H5PL_SIG_FOOTER_SIZE) {
        uint8_t           check_buf[H5PL_SIG_FOOTER_SIZE];
        H5PL_sig_footer_t check_footer;

        if (HDlseek(fd, (HDoff_t)(file_size - (hsize_t)H5PL_SIG_FOOTER_SIZE), SEEK_SET) >= 0) {
            h5_posix_io_ret_t nr = HDread(fd, check_buf, H5PL_SIG_FOOTER_SIZE);
            if (nr == (h5_posix_io_ret_t)H5PL_SIG_FOOTER_SIZE) {
                if (H5PL_sig_decode_footer(check_buf, sizeof(check_buf), &check_footer)) {
                    if (!opt_force) {
                        fprintf(rawerrorstream, "Error: Plugin file '%s' is already signed\n", plugin_path);
                        fprintf(rawerrorstream,
                                "       Use -f/--force to strip the existing signature and re-sign,\n"
                                "       or start from the original unsigned binary\n");
                        ret_value = FAIL;
                        goto done;
                    }

                    /* --force: strip the existing signature and footer so we can re-sign */
                    {
                        uint32_t existing_sig_len = check_footer.signature_length;
                        hsize_t  binary_size;

                        if (existing_sig_len == 0 ||
                            (hsize_t)existing_sig_len + (hsize_t)H5PL_SIG_FOOTER_SIZE > file_size) {
                            fprintf(rawerrorstream, "Error: Corrupt signature footer in '%s' (sig_len=%u)\n",
                                    plugin_path, existing_sig_len);
                            ret_value = FAIL;
                            goto done;
                        }

                        binary_size = file_size - (hsize_t)existing_sig_len - (hsize_t)H5PL_SIG_FOOTER_SIZE;

                        if (HDftruncate(fd, (HDoff_t)binary_size) < 0) {
                            fprintf(rawerrorstream, "Error: Cannot strip existing signature from '%s': %s\n",
                                    plugin_path, strerror(errno));
                            ret_value = FAIL;
                            goto done;
                        }

                        if (opt_verbose)
                            fprintf(rawoutstream, "Existing signature stripped (%u bytes + %d byte footer)\n",
                                    existing_sig_len, H5PL_SIG_FOOTER_SIZE);

                        /* Update file_size to reflect the stripped binary */
                        file_size = binary_size;
                    }
                }
            }
        }

        /* Seek back to the beginning for hashing */
        if (HDlseek(fd, 0, SEEK_SET) < 0) {
            fprintf(rawerrorstream, "Error: Cannot seek in plugin file '%s': %s\n", plugin_path,
                    strerror(errno));
            ret_value = FAIL;
            goto done;
        }
    }

    /* Check binary size after any existing signature has been stripped.
     * The verifier enforces this same limit against the binary portion of the
     * file, so a signed file whose total on-disk size exceeds the limit but
     * whose binary is within range must still be signable. */
    if (file_size > (hsize_t)H5PL_MAX_PLUGIN_SIZE) {
        fprintf(rawerrorstream, "Error: Plugin binary '%s' is too large (%llu bytes)\n", plugin_path,
                (unsigned long long)file_size);
        fprintf(rawerrorstream, "       Maximum binary size is %llu bytes (1GB)\n",
                (unsigned long long)H5PL_MAX_PLUGIN_SIZE);
        ret_value = FAIL;
        goto done;
    }

    if (opt_verbose) {
        fprintf(rawoutstream, "Plugin file: %s\n", plugin_path);
        fprintf(rawoutstream, "File size:   %llu bytes\n", (unsigned long long)file_size);
    }

    /* Create message digest context */
    if (NULL == (mdctx = EVP_MD_CTX_new())) {
        report_openssl_error("Cannot create message digest context");
        ret_value = FAIL;
        goto done;
    }

    /* Initialize signing context with selected hash algorithm */
    if (1 != EVP_DigestSignInit(mdctx, &pkey_ctx, hash_algorithm, NULL, private_key)) {
        report_openssl_error("Cannot initialize signing context");
        ret_value = FAIL;
        goto done;
    }

    /* Configure PSS padding if needed */
    if (H5PL_SIG_ALGO_IS_PSS(algorithm_id)) {
        if (1 != EVP_PKEY_CTX_set_rsa_padding(pkey_ctx, RSA_PKCS1_PSS_PADDING)) {
            report_openssl_error("Cannot set PSS padding");
            ret_value = FAIL;
            goto done;
        }

        if (1 != EVP_PKEY_CTX_set_rsa_pss_saltlen(pkey_ctx, RSA_PSS_SALTLEN_DIGEST)) {
            report_openssl_error("Cannot set PSS salt length");
            ret_value = FAIL;
            goto done;
        }
    }

    /* Allocate buffer for reading file in chunks */
    if (NULL == (hash_buffer = (unsigned char *)malloc(HASH_CHUNK_SIZE))) {
        fprintf(rawerrorstream, "Error: Cannot allocate hash buffer\n");
        ret_value = FAIL;
        goto done;
    }

    /* Read file in chunks and update hash */
    bytes_read = 0;

    if (opt_verbose)
    /* EVP_MD_get0_name() requires OpenSSL 3.0+; fall back to OBJ_nid2sn()
     * for OpenSSL 1.1.x compatibility. */
#if OPENSSL_VERSION_NUMBER >= 0x30000000L
        fprintf(rawoutstream, "Computing %s hash...\n", EVP_MD_get0_name(hash_algorithm));
#else
        fprintf(rawoutstream, "Computing %s hash...\n", OBJ_nid2sn(EVP_MD_type(hash_algorithm)));
#endif

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
            report_openssl_error("Cannot update hash");
            ret_value = FAIL;
            goto done;
        }

        bytes_read += (hsize_t)read_result;
    }

    if (opt_verbose)
        fprintf(rawoutstream, "Hash computed successfully\n");

    /* Get signature length */
    if (1 != EVP_DigestSignFinal(mdctx, NULL, &sig_len)) {
        report_openssl_error("Cannot get signature length");
        ret_value = FAIL;
        goto done;
    }

    if (sig_len == 0 || sig_len > H5PL_MAX_SIGNATURE_SIZE) {
        fprintf(rawerrorstream, "Error: Invalid signature length: %zu bytes (max %d)\n", sig_len,
                H5PL_MAX_SIGNATURE_SIZE);
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
        report_openssl_error("Cannot compute signature");
        ret_value = FAIL;
        goto done;
    }

    if (opt_verbose) {
        fprintf(rawoutstream, "Signature created successfully\n");
        fprintf(rawoutstream, "Signature length: %zu bytes\n", sig_len);
    }

    /* Verify the signed file will not exceed the verifier's size limit */
    if ((hsize_t)(file_size + sig_len + H5PL_SIG_FOOTER_SIZE) > (hsize_t)H5PL_MAX_PLUGIN_SIZE) {
        fprintf(rawerrorstream, "Error: Signed plugin would exceed maximum size (%llu bytes)\n",
                (unsigned long long)H5PL_MAX_PLUGIN_SIZE);
        ret_value = FAIL;
        goto done;
    }

    if (HDlseek(fd, (HDoff_t)file_size, SEEK_SET) < 0) {
        fprintf(rawerrorstream, "Error: Cannot seek in plugin file '%s': %s\n", plugin_path, strerror(errno));
        ret_value = FAIL;
        goto done;
    }

    /* Append signature to file */
    if (write_with_retry(fd, signature, sig_len, "signature", plugin_path, file_size) < 0) {
        ret_value = FAIL;
        goto done;
    }

    if (opt_verbose)
        fprintf(rawoutstream, "Signature appended to plugin\n");

    /* Prepare and write footer in little-endian format */
    {
        uint8_t           footer_buf[H5PL_SIG_FOOTER_SIZE];
        H5PL_sig_footer_t footer;

        footer.signature_length = (uint32_t)sig_len;
        footer.algorithm_id     = algorithm_id;
        footer.format_version   = H5PL_SIG_FORMAT_VERSION_CURRENT;
        H5PL_sig_encode_footer(footer_buf, sizeof(footer_buf), &footer);

        /* Write footer to file */
        if (write_with_retry(fd, footer_buf, sizeof(footer_buf), "footer", plugin_path, file_size) < 0) {
            ret_value = FAIL;
            goto done;
        }
    }

    /* Close file descriptor */
    HDclose(fd);
    fd = -1;

    if (opt_verbose)
        fprintf(rawoutstream, "Footer written successfully\n");

    /* Success! */
    fprintf(rawoutstream, "\nPlugin signed successfully!\n");
    fprintf(rawoutstream, "  File:           %s\n", plugin_path);
    fprintf(rawoutstream, "  Original size:  %llu bytes\n", (unsigned long long)file_size);
    {
        const char *algo_name = algo_display_name(algorithm_id);
        if (algo_name)
            fprintf(rawoutstream, "  Hash algorithm: %s (0x%02X)\n", algo_name, algorithm_id);
        else
            fprintf(rawoutstream, "  Hash algorithm: 0x%02X\n", algorithm_id);
    }
    fprintf(rawoutstream, "  Signature size: %zu bytes\n", sig_len);
    fprintf(rawoutstream, "  Footer size:    %d bytes\n", H5PL_SIG_FOOTER_SIZE);
    fprintf(rawoutstream, "  Final size:     %llu bytes\n",
            (unsigned long long)(file_size + sig_len + H5PL_SIG_FOOTER_SIZE));
    fprintf(rawoutstream, "\n");

done:
    if (fd >= 0)
        HDclose(fd);
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
    EVP_PKEY       *private_key    = NULL;
    const EVP_MD   *hash_algorithm = NULL;
    H5PL_sig_algo_t algorithm_id   = (H5PL_sig_algo_t)0;
    int             ret_value      = EXIT_SUCCESS;

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

    /* Parse algorithm option or use default */
    if (opt_algorithm) {
        if (parse_algorithm_name(opt_algorithm, &hash_algorithm, &algorithm_id) < 0) {
            ret_value = EXIT_FAILURE;
            goto done;
        }
        fprintf(rawoutstream, "Using hash algorithm: %s\n", opt_algorithm);
    }
    else {
        /* Default: SHA-512 with PKCS1 */
        hash_algorithm = EVP_sha512();
        algorithm_id   = H5PL_SIG_ALGO_SHA512;
        fprintf(rawoutstream, "Using default hash algorithm: sha512\n");
    }

    /* Read private key */
    fprintf(rawoutstream, "Reading private key from '%s'...\n", privkey_file);
    if (NULL == (private_key = read_private_key(privkey_file))) {
        ret_value = EXIT_FAILURE;
        goto done;
    }
    fprintf(rawoutstream, "Private key loaded successfully\n\n");

    /* Sign the plugin */
    fprintf(rawoutstream, "Signing plugin '%s'...\n\n", plugin_file);
    if (sign_plugin_file(plugin_file, private_key, hash_algorithm, algorithm_id) < 0) {
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
