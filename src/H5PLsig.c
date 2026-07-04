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
 * Purpose: Digital signature verification for HDF5 plugins
 */

/****************/
/* Module Setup */
/****************/

#include "H5PLmodule.h" /* This source code file is part of the H5PL module */

/***********/
/* Headers */
/***********/
#include "H5private.h"   /* Generic Functions            */
#include "H5Eprivate.h"  /* Error handling               */
#include "H5PLpkg.h"     /* Plugin                       */
#include "H5PLsig.h"     /* Signature format             */
#include "H5MMprivate.h" /* Memory management            */
#include "H5encode.h"    /* Endianness conversion        */

#ifdef H5_REQUIRE_DIGITAL_SIGNATURE

#include <openssl/evp.h>
#include <openssl/pem.h>
#include <openssl/bio.h>
#include <openssl/err.h>

/* For directory operations */
#ifndef H5_HAVE_WIN32_API
#include <dirent.h>
#else
/* S_ISDIR may not be defined on Windows */
#ifndef S_ISDIR
#define S_ISDIR(m) (((m) & _S_IFMT) == _S_IFDIR)
#endif
#endif

/*******************/
/* Local Variables */
/*******************/

/*
 * Thread Safety Note:
 * All file-scope static variables below (keystore and revocation list)
 * are accessed without explicit synchronization. When HDF5_ENABLE_THREADSAFE is enabled,
 * these variables are protected by the HDF5 library-wide global lock that guards plugin
 * operations. Concurrent plugin loads are serialized at the H5PL__load level, ensuring
 * that keystore initialization and revocation list checks cannot race.
 */

/* KeyStore entry for storing multiple trusted public keys */
typedef struct H5PL_keystore_entry_t {
    EVP_PKEY *key;    /* OpenSSL public key object */
    char     *source; /* Key source (filename or "embedded") for debugging */
} H5PL_keystore_entry_t;

/* KeyStore for signature verification
 * TODO (Thread Safety): Requires mutex protection if global lock is removed
 */
static H5PL_keystore_entry_t *H5PL_keystore_g             = NULL;
static size_t                 H5PL_keystore_count_g       = 0;
static size_t                 H5PL_keystore_capacity_g    = 0;
static bool                   H5PL_keystore_initialized_g = false;

/* Revocation list for blocking specific signatures.
 * The file <keystore_dir>/H5PL_REVOKED_SIGS_FILENAME is read at keystore
 * init time.  Each line is a 64-hex-char SHA-256 hash of a signature blob.
 * Lines starting with '#' are comments; empty lines are ignored.
 * The file is optional — if absent, no signatures are revoked. */
#define H5PL_REVOKED_SIGS_FILENAME "revoked_signatures.txt"

/* Size of the SHA-256 hash used to identify revoked signatures.
 * This is the hash of the raw signature bytes, independent of the
 * plugin's signing algorithm (SHA-256/384/512). */
#define H5PL_SIGNATURE_HASH_SIZE    32                             /* SHA-256 = 32 bytes */
#define H5PL_SIGNATURE_HASH_HEX_LEN (H5PL_SIGNATURE_HASH_SIZE * 2) /* 64 hex chars in text file */
typedef struct H5PL_revoked_signature_t {
    unsigned char hash[H5PL_SIGNATURE_HASH_SIZE]; /* SHA-256 hash of signature */
} H5PL_revoked_signature_t;

/* TODO (Thread Safety): Requires mutex protection if global lock is removed */
static H5PL_revoked_signature_t *H5PL_revoked_sigs_g             = NULL;
static size_t                    H5PL_revoked_sigs_count_g       = 0;
static size_t                    H5PL_revoked_sigs_capacity_g    = 0;
static bool                      H5PL_revoked_sigs_initialized_g = false;

/* Initial capacity for keystore array */
#define H5PL_KEYSTORE_INITIAL_CAPACITY 4

/* I/O chunk size for verification (1MB - optimized for modern I/O subsystems) */
#define H5PL_VERIFY_CHUNK_SIZE ((size_t)(1024 * 1024))

/*********************/
/* Local Prototypes  */
/*********************/
static int    H5PL__compare_signature_hashes(const void *a, const void *b);
static herr_t H5PL__load_revoked_signatures(const char *keystore_dir);
static bool   H5PL__is_signature_revoked(const unsigned char *signature, size_t signature_len);
static void   H5PL__free_keystore(void);
static herr_t H5PL__process_key_file(const char *file_path);

/*-------------------------------------------------------------------------
 * Function:    H5PL__compare_signature_hashes
 *
 * Purpose:     Comparison function for sorting and binary searching
 *              revoked signature hashes
 *
 * Return:      <0 if a < b, 0 if a == b, >0 if a > b
 *-------------------------------------------------------------------------
 */
static int
H5PL__compare_signature_hashes(const void *a, const void *b)
{
    const H5PL_revoked_signature_t *hash_a = (const H5PL_revoked_signature_t *)a;
    const H5PL_revoked_signature_t *hash_b = (const H5PL_revoked_signature_t *)b;

    return memcmp(hash_a->hash, hash_b->hash, H5PL_SIGNATURE_HASH_SIZE);
} /* end H5PL__compare_signature_hashes() */

/*-------------------------------------------------------------------------
 * Function:    H5PL__read_file_data
 *
 * Purpose:     Portable file read with EINTR retry
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
H5PL__read_file_data(int fd, HDoff_t offset, void *buf, size_t size, const char *filename)
{
    size_t         left_to_read = size;
    unsigned char *read_ptr     = (unsigned char *)buf;
    herr_t         ret_value    = SUCCEED;

    FUNC_ENTER_PACKAGE

    assert(buf);
    assert(filename);

#ifndef H5_HAVE_PREADWRITE
    /* Seek to the correct location (if we don't have pread) */
    if (HDlseek(fd, offset, SEEK_SET) < 0)
        HGOTO_ERROR(H5E_PLUGIN, H5E_SEEKERROR, FAIL, "unable to seek to offset %llu in plugin file '%s'",
                    (unsigned long long)offset, filename);
#endif /* H5_HAVE_PREADWRITE */

    /* Read data in chunks, following HDF5's established I/O pattern from H5FDsec2.c */
    while (left_to_read > 0) {
        h5_posix_io_t     bytes_in   = 0;
        h5_posix_io_ret_t bytes_read = -1;

        /* Respect platform I/O size limits to avoid undefined behavior */
        if (left_to_read > H5_POSIX_MAX_IO_BYTES)
            bytes_in = H5_POSIX_MAX_IO_BYTES;
        else
            bytes_in = (h5_posix_io_t)left_to_read;

        /* Retry on EINTR (interrupted system call), use pread if available */
        do {
#ifdef H5_HAVE_PREADWRITE
            bytes_read = HDpread(fd, read_ptr, bytes_in, offset);
            if (bytes_read > 0)
                offset += bytes_read;
#else
            bytes_read = HDread(fd, read_ptr, bytes_in);
            if (bytes_read > 0)
                offset += bytes_read; /* track offset for error reporting */
#endif /* H5_HAVE_PREADWRITE */
        } while (-1 == bytes_read && EINTR == errno);

        if (bytes_read < 0) {
            int myerrno = errno;

            HGOTO_ERROR(H5E_PLUGIN, H5E_READERROR, FAIL,
                        "plugin file read failed: filename='%s', errno=%d (%s), offset=%llu, size=%llu",
                        filename, myerrno, strerror(myerrno), (unsigned long long)offset,
                        (unsigned long long)bytes_in);
        }

        if (0 == bytes_read)
            HGOTO_ERROR(H5E_PLUGIN, H5E_READERROR, FAIL,
                        "unexpected end of file while reading plugin '%s' at offset %llu", filename,
                        (unsigned long long)offset);

        assert(bytes_read >= 0);
        assert((size_t)bytes_read <= left_to_read);

        left_to_read -= (size_t)bytes_read;
        read_ptr += bytes_read;
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5PL__read_file_data() */

/*-------------------------------------------------------------------------
 * Function:    H5PL__get_hash_algorithm
 *
 * Purpose:     Convert algorithm ID to OpenSSL EVP_MD
 *
 * Return:      Success: Pointer to EVP_MD
 *              Failure: NULL
 *-------------------------------------------------------------------------
 */
static const EVP_MD *
H5PL__get_hash_algorithm(H5PL_sig_algo_t algorithm_id)
{
    const EVP_MD *ret_value = NULL;

    FUNC_ENTER_PACKAGE_NOERR

    switch (algorithm_id) {
        case H5PL_SIG_ALGO_SHA256:
        case H5PL_SIG_ALGO_SHA256_PSS:
            ret_value = EVP_sha256();
            break;

        case H5PL_SIG_ALGO_SHA384:
        case H5PL_SIG_ALGO_SHA384_PSS:
            ret_value = EVP_sha384();
            break;

        case H5PL_SIG_ALGO_SHA512:
        case H5PL_SIG_ALGO_SHA512_PSS:
            ret_value = EVP_sha512();
            break;

        case H5PL_SIG_ALGO_SHA3_256:
            /* SHA3-256 is reserved for a future HDF5 release */
            H5PL_SIG_DEBUG_PRINT("Algorithm SHA3-256 (0x%02X) is reserved for future use\n", algorithm_id);
            ret_value = NULL;
            break;

        case H5PL_SIG_ALGO_BLAKE3:
            /* BLAKE3 is reserved for a future HDF5 release */
            H5PL_SIG_DEBUG_PRINT("Algorithm BLAKE3 (0x%02X) is reserved for future use\n", algorithm_id);
            ret_value = NULL;
            break;

        default:
            /* Completely unknown algorithm - return NULL */
            ret_value = NULL;
            break;
    }

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5PL__get_hash_algorithm() */

/*-------------------------------------------------------------------------
 * Function:    H5PL__add_key_to_keystore
 *
 * Purpose:     Add a public key to the keystore with source tracking
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5PL__add_key_to_keystore(EVP_PKEY *key, const char *source)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE

    assert(key);
    assert(source);

    /* Expand keystore if needed */
    if (H5PL_keystore_count_g >= H5PL_keystore_capacity_g) {
        size_t new_capacity =
            H5PL_keystore_capacity_g == 0 ? H5PL_KEYSTORE_INITIAL_CAPACITY : H5PL_keystore_capacity_g * 2;
        H5PL_keystore_entry_t *new_keystore = (H5PL_keystore_entry_t *)H5MM_realloc(
            H5PL_keystore_g, new_capacity * sizeof(H5PL_keystore_entry_t));

        if (NULL == new_keystore)
            HGOTO_ERROR(H5E_PLUGIN, H5E_CANTALLOC, FAIL, "cannot expand keystore array");

        H5PL_keystore_g          = new_keystore;
        H5PL_keystore_capacity_g = new_capacity;
    }

    /* Duplicate source string before committing the entry, so a strdup
     * failure doesn't leave an orphaned key pointer in the array. */
    {
        char *dup_source = H5MM_strdup(source);
        if (NULL == dup_source)
            HGOTO_ERROR(H5E_PLUGIN, H5E_CANTALLOC, FAIL, "cannot duplicate key source string");

        H5PL_keystore_g[H5PL_keystore_count_g].key    = key;
        H5PL_keystore_g[H5PL_keystore_count_g].source = dup_source;
        H5PL_keystore_count_g++;
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5PL__add_key_to_keystore() */

/*-------------------------------------------------------------------------
 * Function:    H5PL__create_public_RSA_from_file
 *
 * Purpose:     Create EVP public key from PEM file
 *
 * Return:      Success: Pointer to EVP_PKEY
 *              Failure: NULL
 *
 *-------------------------------------------------------------------------
 */
static EVP_PKEY *
H5PL__create_public_RSA_from_file(const char *file_path)
{
    BIO      *bio       = NULL;
    EVP_PKEY *pkey      = NULL;
    EVP_PKEY *ret_value = NULL;

    FUNC_ENTER_PACKAGE_NOERR

    assert(file_path);

    /* Open key file using BIO (avoids OPENSSL_Applink issue on Windows) */
    if (NULL == (bio = BIO_new_file(file_path, "r"))) {
        /* Don't error - just skip invalid files */
        goto done;
    }

    /* Read public key using modern EVP API */
    if (NULL == (pkey = PEM_read_bio_PUBKEY(bio, NULL, NULL, NULL))) {
        /* Don't error - just skip invalid PEM files */
        goto done;
    }

    /* Validate key type - only RSA keys are supported */
    {
        int key_type = EVP_PKEY_base_id(pkey);
        if (key_type != EVP_PKEY_RSA && key_type != EVP_PKEY_RSA_PSS) {
            /* Don't error - just skip unsupported key types */
            goto done;
        }
    }

    ret_value = pkey;
    pkey      = NULL; /* Prevent cleanup */

done:
    if (bio)
        BIO_free(bio);
    if (pkey)
        EVP_PKEY_free(pkey);

    /* Clear any remaining OpenSSL errors from the error queue */
    ERR_clear_error();

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5PL__create_public_RSA_from_file() */

/*-------------------------------------------------------------------------
 * Function:    H5PL__process_key_file
 *
 * Purpose:     Load a PEM key file and add it to the keystore
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
H5PL__process_key_file(const char *file_path)
{
    EVP_PKEY *key       = NULL;
    herr_t    ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE

    assert(file_path);

    /* Try to load key; skip files that fail to load (invalid PEM, etc.) */
    if (NULL != (key = H5PL__create_public_RSA_from_file(file_path))) {
        /* Add to keystore (transfers ownership of key on success) */
        if (H5PL__add_key_to_keystore(key, file_path) < 0)
            HGOTO_ERROR(H5E_PLUGIN, H5E_CANTALLOC, FAIL, "cannot add key to keystore");
        key = NULL; /* Ownership transferred to keystore */
    }

done:
    if (key)
        EVP_PKEY_free(key);
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5PL__process_key_file() */

/*-------------------------------------------------------------------------
 * Function:    H5PL__load_keys_from_directory
 *
 * Purpose:     Load all .pem files from a directory into the keystore
 *
 * Return:      SUCCEED/FAIL (fails if directory invalid, but skips bad files)
 *
 *-------------------------------------------------------------------------
 */
#ifndef H5_HAVE_WIN32_API
static herr_t
H5PL__load_keys_from_directory(const char *dir_path)
{
    DIR           *dir       = NULL;
    struct dirent *entry     = NULL;
    size_t         dirlen    = 0;
    herr_t         ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE

    assert(dir_path);

    /* Open directory */
    if (NULL == (dir = opendir(dir_path))) {
        /* Non-existent directory is an error */
        HGOTO_ERROR(H5E_PLUGIN, H5E_CANTOPENFILE, FAIL, "cannot open keystore directory: %s", dir_path);
    }

    dirlen = strlen(dir_path);

    /* Iterate through directory entries */
    while (NULL != (entry = readdir(dir))) {
        char  *file_path = NULL;
        size_t namelen   = strlen(entry->d_name);
        size_t path_len;

        /* Skip . and .. */
        if (strcmp(entry->d_name, ".") == 0 || strcmp(entry->d_name, "..") == 0)
            continue;

        /* Only process .pem files */
        if (namelen < 5 || strcmp(entry->d_name + namelen - 4, ".pem") != 0)
            continue;

        /* Validate filename doesn't contain path separators (defense in depth) */
        if (strchr(entry->d_name, '/') != NULL) {
            H5PL_SIG_DEBUG_PRINT("WARNING: Skipping file with path separator in name: %s\n", entry->d_name);
            continue;
        }

        /* Build full path */
        path_len = dirlen + namelen + 2;
        if (NULL == (file_path = (char *)H5MM_malloc(path_len))) {
            H5PL_SIG_DEBUG_PRINT("WARNING: Cannot allocate path buffer for %s\n", entry->d_name);
            continue;
        }

        snprintf(file_path, path_len, "%s/%s", dir_path, entry->d_name);

        /* Canonicalize and verify path stays within keystore directory (path traversal protection) */
        {
            char *canonical_dir  = NULL;
            char *canonical_file = NULL;

            canonical_dir = HDrealpath(dir_path, NULL);
            if (NULL == canonical_dir) {
                H5PL_SIG_DEBUG_PRINT("WARNING: Cannot resolve keystore directory path: %s\n",
                                     strerror(errno));
                H5MM_xfree(file_path);
                continue;
            }

            canonical_file = HDrealpath(file_path, NULL);
            if (NULL == canonical_file) {
                /* File might not exist yet in some cases, but for key files it must exist */
                H5PL_SIG_DEBUG_PRINT("WARNING: Cannot resolve key file path %s: %s\n", file_path,
                                     strerror(errno));
                free(canonical_dir);
                H5MM_xfree(file_path);
                continue;
            }

            /* Verify canonical file path starts with canonical directory path */
            {
                size_t dir_len = strlen(canonical_dir);
                if (strncmp(canonical_file, canonical_dir, dir_len) != 0 ||
                    (canonical_file[dir_len] != '/' && canonical_file[dir_len] != '\0')) {
                    H5PL_SIG_DEBUG_PRINT(
                        "WARNING: Path traversal detected - %s resolves outside keystore directory\n",
                        entry->d_name);
                    free(canonical_dir);
                    free(canonical_file);
                    H5MM_xfree(file_path);
                    continue;
                }
            }

            free(canonical_dir);
            free(canonical_file);
        }

        /* Skip symlinks */
        {
            h5_stat_t file_stat;
            if (HDlstat(file_path, &file_stat) < 0) {
                H5PL_SIG_DEBUG_PRINT("WARNING: Cannot stat key file %s: %s\n", file_path, strerror(errno));
                H5MM_xfree(file_path);
                continue;
            }

            if (S_ISLNK(file_stat.st_mode)) {
                H5PL_SIG_DEBUG_PRINT("WARNING: Skipping symlink %s (security policy)\n", file_path);
                H5MM_xfree(file_path);
                continue;
            }
        }

        /* Load key and add to keystore */
        if (H5PL__process_key_file(file_path) < 0) {
            H5MM_xfree(file_path);
            HGOTO_ERROR(H5E_PLUGIN, H5E_CANTALLOC, FAIL, "cannot process key file");
        }

        /* Clean up file path */
        H5MM_xfree(file_path);
    }

done:
    if (dir)
        closedir(dir);
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5PL__load_keys_from_directory() */
#else  /* H5_HAVE_WIN32_API */
static herr_t
H5PL__load_keys_from_directory(const char *dir_path)
{
    HANDLE dir_handle = INVALID_HANDLE_VALUE;
    herr_t ret_value  = SUCCEED;

    FUNC_ENTER_PACKAGE

    assert(dir_path);

    {
        WIN32_FIND_DATAA find_data;
        char             search_pattern[MAX_PATH];

        /* Build search pattern: dir\*.pem */
        snprintf(search_pattern, sizeof(search_pattern), "%s\\*.pem", dir_path);

        dir_handle = FindFirstFileA(search_pattern, &find_data);
        if (INVALID_HANDLE_VALUE == dir_handle) {
            /* Empty directory is OK */
            goto done;
        }

        do {
            char file_path[MAX_PATH];
            char canonical_dir[MAX_PATH];
            char canonical_file[MAX_PATH];

            /* Skip directories */
            if (find_data.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)
                continue;

            /* Skip symlinks and reparse points (NTFS junctions, symlinks) */
            if (find_data.dwFileAttributes & FILE_ATTRIBUTE_REPARSE_POINT)
                continue;

            /* Build full path */
            snprintf(file_path, sizeof(file_path), "%s\\%s", dir_path, find_data.cFileName);

            /* Path traversal protection: verify file resolves within the keystore directory */
            if (GetFullPathNameA(dir_path, MAX_PATH, canonical_dir, NULL) == 0 ||
                GetFullPathNameA(file_path, MAX_PATH, canonical_file, NULL) == 0) {
                H5PL_SIG_DEBUG_PRINT("WARNING: Cannot resolve path for %s\n", find_data.cFileName);
                continue;
            }
            {
                size_t dir_len = strlen(canonical_dir);
                if (_strnicmp(canonical_file, canonical_dir, dir_len) != 0 ||
                    (canonical_file[dir_len] != '\\' && canonical_file[dir_len] != '\0')) {
                    H5PL_SIG_DEBUG_PRINT(
                        "WARNING: Path traversal detected - %s resolves outside keystore directory\n",
                        find_data.cFileName);
                    continue;
                }
            }

            /* Load key and add to keystore */
            if (H5PL__process_key_file(file_path) < 0)
                HGOTO_ERROR(H5E_PLUGIN, H5E_CANTALLOC, FAIL, "cannot process key file");

        } while (FindNextFileA(dir_handle, &find_data) != 0);
    }

done:
    if (dir_handle != INVALID_HANDLE_VALUE)
        FindClose(dir_handle);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5PL__load_keys_from_directory() */
#endif /* H5_HAVE_WIN32_API */

/*-------------------------------------------------------------------------
 * Function:    H5PL__free_keystore
 *
 * Purpose:     Free all keys and revocation entries in the keystore
 *
 * Return:      void
 *-------------------------------------------------------------------------
 */
static void
H5PL__free_keystore(void)
{
    if (H5PL_keystore_g) {
        size_t i;
        for (i = 0; i < H5PL_keystore_count_g; i++) {
            if (H5PL_keystore_g[i].key)
                EVP_PKEY_free(H5PL_keystore_g[i].key);
            if (H5PL_keystore_g[i].source)
                H5MM_xfree(H5PL_keystore_g[i].source);
        }
        H5MM_xfree(H5PL_keystore_g);
        H5PL_keystore_g = NULL;
    }
    H5PL_keystore_count_g    = 0;
    H5PL_keystore_capacity_g = 0;

    if (H5PL_revoked_sigs_g) {
        H5MM_xfree(H5PL_revoked_sigs_g);
        H5PL_revoked_sigs_g = NULL;
    }
    H5PL_revoked_sigs_count_g    = 0;
    H5PL_revoked_sigs_capacity_g = 0;

    H5PL_keystore_initialized_g     = false;
    H5PL_revoked_sigs_initialized_g = false;
} /* end H5PL__free_keystore() */

/*-------------------------------------------------------------------------
 * Function:    H5PL__init_keystore
 *
 * Purpose:     Initialize keystore
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
H5PL__init_keystore(void)
{
    const char *env_keystore = NULL;
    bool        keys_loaded  = false;
    herr_t      ret_value    = SUCCEED;

    FUNC_ENTER_PACKAGE

    /* Already initialized? */
    if (H5PL_keystore_initialized_g)
        HGOTO_DONE(SUCCEED);

    /* Initialize keystore */
    H5PL_keystore_g             = NULL;
    H5PL_keystore_count_g       = 0;
    H5PL_keystore_capacity_g    = 0;
    H5PL_keystore_initialized_g = true;

    /* Initialize revocation list */
    H5PL_revoked_sigs_g             = NULL;
    H5PL_revoked_sigs_count_g       = 0;
    H5PL_revoked_sigs_capacity_g    = 0;
    H5PL_revoked_sigs_initialized_g = true;

    /* 1. Check environment variable (highest priority) */
#ifndef H5PL_DISABLE_ENV_KEYSTORE
    if (NULL != (env_keystore = getenv("HDF5_PLUGIN_KEYSTORE"))) {
        if (H5PL__load_keys_from_directory(env_keystore) < 0)
            HGOTO_ERROR(H5E_PLUGIN, H5E_CANTLOAD, FAIL, "failed to load keys from HDF5_PLUGIN_KEYSTORE: %s",
                        env_keystore);
        keys_loaded = true;

        /* Load revoked signatures from same directory */
        if (H5PL__load_revoked_signatures(env_keystore) < 0) {
            /* Non-fatal - continue even if revoked signatures fail to load */
        }
    }
#else
    /* Environment variable override disabled at compile time (security hardening) */
    env_keystore = NULL; /* Suppress unused variable warning */
#endif

/* 2. Check CMake-configured directory */
#ifdef H5PL_KEYSTORE_DIR
    if (!keys_loaded) {
        /* Only try if directory was configured */
        h5_stat_t st;
        if (HDstat(H5PL_KEYSTORE_DIR, &st) == 0) {
            /* Directory exists, try to load.  Pause the error stack so that
             * a load failure here does not pollute the stack — the generic
             * "no valid public keys" error below is more informative. */
            H5E_pause_stack();
            if (H5PL__load_keys_from_directory(H5PL_KEYSTORE_DIR) < 0) {
                H5PL_SIG_DEBUG_PRINT("WARNING: Failed to load keys from configured keystore: %s\n",
                                     H5PL_KEYSTORE_DIR);
            }
            else {
                keys_loaded = true;

                /* Load revoked signatures from same directory */
                if (H5PL__load_revoked_signatures(H5PL_KEYSTORE_DIR) < 0) {
                    /* Non-fatal - continue even if revoked signatures fail to load */
                }
            }
            H5E_resume_stack();
        }
    }
#endif

    if (H5PL_keystore_count_g > 0) {
        H5PL_SIG_DEBUG_PRINT("HDF5 Plugin KeyStore initialized:\n");
        H5PL_SIG_DEBUG_PRINT("  Keys loaded: %zu\n", H5PL_keystore_count_g);
        for (size_t i = 0; i < H5PL_keystore_count_g; i++) {
            H5PL_SIG_DEBUG_PRINT("  [%zu] %s\n", i + 1, H5PL_keystore_g[i].source);
        }
    }
    if (H5PL_revoked_sigs_count_g > 0) {
        H5PL_SIG_DEBUG_PRINT("  Revoked signatures loaded: %zu\n", H5PL_revoked_sigs_count_g);
    }

done:
    /* Cleanup on initialization failure */
    if (ret_value < 0)
        H5PL__free_keystore();

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5PL__init_keystore() */

/*-------------------------------------------------------------------------
 * Function:    H5PL__parse_hex_hash
 *
 * Purpose:     Parse a hexadecimal string into a byte array
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
H5PL__parse_hex_hash(const char *hex_string, unsigned char *hash)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE

    assert(hex_string);
    assert(hash);

    /* Convert hex string to bytes */
    for (size_t i = 0; i < H5PL_SIGNATURE_HASH_SIZE; i++) {
        unsigned int byte;
        if (sscanf(hex_string + (i * 2), "%2x", &byte) != 1)
            HGOTO_ERROR(H5E_PLUGIN, H5E_BADVALUE, FAIL, "invalid hex character in hash string");
        hash[i] = (unsigned char)byte;
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5PL__parse_hex_hash() */

/*-------------------------------------------------------------------------
 * Function:    H5PL__load_revoked_signatures
 *
 * Purpose:     Load revoked signature hashes from blocklist file
 *
 *              File format: One SHA-256 hash per line (64 hex chars)
 *              Comments start with '#', empty lines ignored
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
H5PL__load_revoked_signatures(const char *keystore_dir)
{
    char  *filepath = NULL;
    FILE  *fp       = NULL;
    char   line[256];
    size_t path_len;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE

    assert(keystore_dir);

    /* Build path to revoked signatures file */
    path_len = strlen(keystore_dir) + 1 + strlen(H5PL_REVOKED_SIGS_FILENAME) + 1;
    if (NULL == (filepath = (char *)H5MM_malloc(path_len)))
        HGOTO_ERROR(H5E_PLUGIN, H5E_CANTALLOC, FAIL, "cannot allocate filepath buffer");

    if (snprintf(filepath, path_len, "%s/%s", keystore_dir, H5PL_REVOKED_SIGS_FILENAME) >= (int)path_len)
        HGOTO_ERROR(H5E_PLUGIN, H5E_NOSPACE, FAIL, "revoked signatures file path too long");

    /* Try to open revoked signatures file (optional - not an error if missing) */
    if (NULL == (fp = fopen(filepath, "r"))) {
        /* File doesn't exist - not an error, just means no revoked signatures */
        HGOTO_DONE(SUCCEED);
    }

    /* Read file line by line */
    while (fgets(line, sizeof(line), fp) != NULL) {
        unsigned char hash[H5PL_SIGNATURE_HASH_SIZE];
        size_t        line_len;
        char         *trimmed;
        bool          line_truncated = false;

        /* Detect truncated reads: fgets fills the buffer without finding a
         * newline, meaning the physical line exceeds sizeof(line)-1 chars.
         * Drain the remainder so the next fgets starts on a fresh line, then
         * skip this chunk — a trailing fragment could otherwise be mistaken
         * for a valid 64-hex-char hash. */
        if (strchr(line, '\n') == NULL && !feof(fp)) {
            int ch;
            line_truncated = true;
            while ((ch = fgetc(fp)) != EOF && ch != '\n')
                ;
        }
        if (line_truncated) {
            H5PL_SIG_DEBUG_PRINT("WARNING: Skipping oversized line in revoked signatures file\n");
            continue;
        }

        /* Trim whitespace */
        trimmed = line;
        while (*trimmed == ' ' || *trimmed == '\t')
            trimmed++;

        line_len = strlen(trimmed);
        while (line_len > 0 && (trimmed[line_len - 1] == '\n' || trimmed[line_len - 1] == '\r' ||
                                trimmed[line_len - 1] == ' ' || trimmed[line_len - 1] == '\t')) {
            trimmed[line_len - 1] = '\0';
            line_len--;
        }

        /* Skip empty lines and comments */
        if (line_len == 0 || trimmed[0] == '#')
            continue;

        /* Each SHA-256 byte is two hex characters → 64 chars per hash */
        if (line_len != H5PL_SIGNATURE_HASH_HEX_LEN) {
            H5PL_SIG_DEBUG_PRINT(
                "WARNING: Ignoring invalid revoked signature hash (expected %d hex chars): %s\n",
                H5PL_SIGNATURE_HASH_HEX_LEN, trimmed);
            continue;
        }

        /* Convert hex string to bytes */
        if (H5PL__parse_hex_hash(trimmed, hash) < 0) {
            H5PL_SIG_DEBUG_PRINT("WARNING: Invalid hex in revoked signature hash: %s\n", trimmed);
            continue;
        }

        /* Expand revoked signatures array if needed */
        if (H5PL_revoked_sigs_count_g >= H5PL_revoked_sigs_capacity_g) {
            size_t new_capacity = H5PL_revoked_sigs_capacity_g == 0 ? 8 : H5PL_revoked_sigs_capacity_g * 2;
            H5PL_revoked_signature_t *new_array = (H5PL_revoked_signature_t *)H5MM_realloc(
                H5PL_revoked_sigs_g, new_capacity * sizeof(H5PL_revoked_signature_t));

            if (NULL == new_array)
                HGOTO_ERROR(H5E_PLUGIN, H5E_CANTALLOC, FAIL, "cannot expand revoked signatures array");

            H5PL_revoked_sigs_g          = new_array;
            H5PL_revoked_sigs_capacity_g = new_capacity;
        }

        /* Add hash to revoked list */
        memcpy(H5PL_revoked_sigs_g[H5PL_revoked_sigs_count_g].hash, hash, H5PL_SIGNATURE_HASH_SIZE);
        H5PL_revoked_sigs_count_g++;
    }

    /* Sort the revocation list for binary search (improves O(n) to O(log n) lookup) */
    if (H5PL_revoked_sigs_count_g > 1) {
        qsort(H5PL_revoked_sigs_g, H5PL_revoked_sigs_count_g, sizeof(H5PL_revoked_signature_t),
              H5PL__compare_signature_hashes);
    }

done:
    if (fp)
        fclose(fp);
    if (filepath)
        H5MM_xfree(filepath);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5PL__load_revoked_signatures() */

/*-------------------------------------------------------------------------
 * Function:    H5PL__is_signature_revoked
 *
 * Purpose:     Check if a signature hash is in the revocation list
 *
 * Return:      true if revoked, false otherwise
 *-------------------------------------------------------------------------
 */
static bool
H5PL__is_signature_revoked(const unsigned char *signature, size_t signature_len)
{
    unsigned char hash[H5PL_SIGNATURE_HASH_SIZE];
    EVP_MD_CTX   *mdctx     = NULL;
    bool          ret_value = false;

    FUNC_ENTER_PACKAGE_NOERR

    assert(signature);

    /* Compute SHA-256 hash of signature */
    if (NULL == (mdctx = EVP_MD_CTX_new()))
        HGOTO_DONE(false);

    if (1 != EVP_DigestInit_ex(mdctx, EVP_sha256(), NULL))
        HGOTO_DONE(false);

    if (1 != EVP_DigestUpdate(mdctx, signature, signature_len))
        HGOTO_DONE(false);

    if (1 != EVP_DigestFinal_ex(mdctx, hash, NULL))
        HGOTO_DONE(false);

    /* Check if hash is in revoked list using binary search
     * (array is sorted in H5PL__load_revoked_signatures).
     * hash[] can be passed directly as the bsearch key because
     * H5PL_revoked_signature_t contains only a hash array at offset 0.
     */
    if (H5PL_revoked_sigs_count_g > 0) {
        if (NULL != bsearch(hash, H5PL_revoked_sigs_g, H5PL_revoked_sigs_count_g,
                            sizeof(H5PL_revoked_signature_t), H5PL__compare_signature_hashes))
            HGOTO_DONE(true);
    }

done:
    if (mdctx)
        EVP_MD_CTX_free(mdctx);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5PL__is_signature_revoked() */

/*-------------------------------------------------------------------------
 * Function:    H5PL__hash_file_binary
 *
 * Purpose:     Compute the message digest of the plugin binary data.
 *              Reads the first binary_size bytes of fd in 1MB chunks
 *              and feeds them into hash_algorithm.  The raw digest is
 *              written to digest_out (caller must supply EVP_MAX_MD_SIZE
 *              bytes) and its byte length to digest_len_out.
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
H5PL__hash_file_binary(int fd, HDoff_t binary_size, const EVP_MD *hash_algorithm, unsigned char *digest_out,
                       unsigned int *digest_len_out, const char *plugin_path)
{
    EVP_MD_CTX    *mdctx      = NULL;
    unsigned char *chunk_buf  = NULL;
    HDoff_t        bytes_read = 0;
    herr_t         ret_value  = SUCCEED;

    FUNC_ENTER_PACKAGE

    assert(fd >= 0);
    assert(hash_algorithm);
    assert(digest_out);
    assert(digest_len_out);
    assert(plugin_path);

    /* Allocate chunk buffer */
    if (NULL == (chunk_buf = (unsigned char *)H5MM_malloc(H5PL_VERIFY_CHUNK_SIZE)))
        HGOTO_ERROR(H5E_PLUGIN, H5E_CANTALLOC, FAIL, "cannot allocate chunk buffer for hashing");

    /* Create and initialize digest context */
    if (NULL == (mdctx = EVP_MD_CTX_new()))
        HGOTO_ERROR(H5E_PLUGIN, H5E_CANTCREATE, FAIL, "cannot create digest context");

    if (1 != EVP_DigestInit_ex(mdctx, hash_algorithm, NULL))
        HGOTO_ERROR(H5E_PLUGIN, H5E_CANTINIT, FAIL, "cannot initialize digest context");

    /* Read and hash file in chunks */
    while (bytes_read < binary_size) {
        size_t chunk_size = (size_t)((binary_size - bytes_read) > (HDoff_t)H5PL_VERIFY_CHUNK_SIZE
                                         ? H5PL_VERIFY_CHUNK_SIZE
                                         : (size_t)(binary_size - bytes_read));

        if (H5PL__read_file_data(fd, bytes_read, chunk_buf, chunk_size, plugin_path) < 0)
            HGOTO_ERROR(H5E_PLUGIN, H5E_READERROR, FAIL, "cannot read plugin data for hashing");

        if (1 != EVP_DigestUpdate(mdctx, chunk_buf, chunk_size))
            HGOTO_ERROR(H5E_PLUGIN, H5E_CANTGET, FAIL, "cannot update digest");

        bytes_read += (HDoff_t)chunk_size;
    }

    /* Finalize digest */
    if (1 != EVP_DigestFinal_ex(mdctx, digest_out, digest_len_out))
        HGOTO_ERROR(H5E_PLUGIN, H5E_CANTGET, FAIL, "cannot finalize digest");

done:
    if (chunk_buf)
        H5MM_xfree(chunk_buf);
    if (mdctx)
        EVP_MD_CTX_free(mdctx);
    ERR_clear_error();

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5PL__hash_file_binary() */

/*-------------------------------------------------------------------------
 * Function:    H5PL__read_and_validate_footer
 *
 * Purpose:     Read and validate the signature footer from a plugin file
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
H5PL__read_and_validate_footer(int fd, HDoff_t file_size, const char *plugin_path,
                               H5PL_sig_footer_t *footer_out, size_t *binary_size_out)
{
    uint8_t footer_buf[H5PL_SIG_FOOTER_SIZE];
    herr_t  ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE

    assert(fd >= 0);
    assert(plugin_path);
    assert(footer_out);
    assert(binary_size_out);

    /* File must be large enough for footer */
    if (file_size < (HDoff_t)H5PL_SIG_FOOTER_SIZE)
        HGOTO_ERROR(H5E_PLUGIN, H5E_BADVALUE, FAIL, "file too small to contain signature footer");

    /* Read footer from end of file */
    if (H5PL__read_file_data(fd, file_size - (HDoff_t)H5PL_SIG_FOOTER_SIZE, footer_buf, H5PL_SIG_FOOTER_SIZE,
                             plugin_path) < 0)
        HGOTO_ERROR(H5E_PLUGIN, H5E_READERROR, FAIL, "cannot read signature footer");

    /* Decode and validate footer (magic and format version checked inside) */
    if (!H5PL_sig_decode_footer(footer_buf, sizeof(footer_buf), footer_out))
        HGOTO_ERROR(H5E_PLUGIN, H5E_BADVALUE, FAIL,
                    "not a signed HDF5 plugin (bad magic or unsupported format version)");

    /* Validate algorithm ID */
    if (NULL == H5PL__get_hash_algorithm(footer_out->algorithm_id))
        HGOTO_ERROR(H5E_PLUGIN, H5E_BADVALUE, FAIL,
                    "unsupported or unknown hash algorithm ID 0x%02X in plugin signature",
                    (unsigned)footer_out->algorithm_id);

    /* Validate signature length */
    if (footer_out->signature_length == 0 || footer_out->signature_length > H5PL_MAX_SIGNATURE_SIZE)
        HGOTO_ERROR(H5E_PLUGIN, H5E_BADVALUE, FAIL,
                    "invalid signature length %u bytes (valid range: 1-%u bytes)",
                    footer_out->signature_length, H5PL_MAX_SIGNATURE_SIZE);

    /* Calculate binary data size with overflow protection */
    {
        /* Use uint64_t to prevent any theoretical overflow in addition */
        uint64_t sig_and_footer_size =
            (uint64_t)footer_out->signature_length + (uint64_t)H5PL_SIG_FOOTER_SIZE;

        /* Validate file size can contain signature and footer */
        if (file_size < (HDoff_t)sig_and_footer_size)
            HGOTO_ERROR(H5E_PLUGIN, H5E_BADVALUE, FAIL,
                        "file too small to contain claimed signature and footer");

        /* Calculate binary size - mathematically guaranteed non-negative after above check */
        HDoff_t binary_size_off = file_size - (HDoff_t)sig_and_footer_size;

        /* Practical size limit: 1GB for plugin files */
        if (binary_size_off > H5PL_MAX_PLUGIN_SIZE)
            HGOTO_ERROR(H5E_PLUGIN, H5E_BADVALUE, FAIL,
                        "plugin binary size %llu exceeds maximum allowed size (%llu bytes) - "
                        "file too large to verify",
                        (unsigned long long)binary_size_off, (unsigned long long)H5PL_MAX_PLUGIN_SIZE);

        *binary_size_out = (size_t)binary_size_off;
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5PL__read_and_validate_footer() */

/*-------------------------------------------------------------------------
 * Function:    H5PL__verify_with_all_keys
 *
 * Purpose:     Try verifying the plugin signature with each key in the
 *              keystore.  The binary is hashed exactly once; the digest
 *              is then checked against the stored signature for every
 *              key using EVP_PKEY_verify (no per-key file re-read).
 *
 * Return:      SUCCEED if signature verified with at least one key
 *              FAIL otherwise
 *-------------------------------------------------------------------------
 */
static herr_t
H5PL__verify_with_all_keys(int fd, size_t binary_size, const unsigned char *signature,
                           const H5PL_sig_footer_t *footer, const char *plugin_path)
{
    const EVP_MD *hash_algorithm = NULL;
    unsigned char digest[EVP_MAX_MD_SIZE];
    unsigned int  digest_len = 0;
    bool          verified   = false;
    herr_t        ret_value  = SUCCEED;

    FUNC_ENTER_PACKAGE

    assert(fd >= 0);
    assert(signature);
    assert(footer);
    assert(plugin_path);

    /* Get hash algorithm from footer (crypto-agile verification) */
    hash_algorithm = H5PL__get_hash_algorithm(footer->algorithm_id);
    if (NULL == hash_algorithm)
        HGOTO_ERROR(H5E_PLUGIN, H5E_BADVALUE, FAIL, "cannot get hash algorithm for ID 0x%02X",
                    (unsigned)footer->algorithm_id);

    /* Hash the binary exactly once - shared across all key verification attempts */
    if (H5PL__hash_file_binary(fd, (HDoff_t)binary_size, hash_algorithm, digest, &digest_len, plugin_path) <
        0)
        HGOTO_ERROR(H5E_PLUGIN, H5E_CANTGET, FAIL, "cannot compute hash of plugin binary");

    /* Try each key in keystore (OR logic - first match wins) */
    for (size_t key_idx = 0; key_idx < H5PL_keystore_count_g; key_idx++) {
        EVP_PKEY     *public_key    = H5PL_keystore_g[key_idx].key;
        EVP_PKEY_CTX *pkey_ctx      = NULL;
        int           verify_result = -1;

        /* Create per-key verification context */
        if (NULL == (pkey_ctx = EVP_PKEY_CTX_new(public_key, NULL))) {
            ERR_clear_error();
            continue;
        }

        if (1 != EVP_PKEY_verify_init(pkey_ctx)) {
            EVP_PKEY_CTX_free(pkey_ctx);
            ERR_clear_error();
            continue;
        }

        /* Bind hash algorithm to the context */
        if (1 != EVP_PKEY_CTX_set_signature_md(pkey_ctx, hash_algorithm)) {
            EVP_PKEY_CTX_free(pkey_ctx);
            ERR_clear_error();
            continue;
        }

        /* Configure PSS padding if needed */
        if (H5PL_SIG_ALGO_IS_PSS(footer->algorithm_id)) {
            if (1 != EVP_PKEY_CTX_set_rsa_padding(pkey_ctx, RSA_PKCS1_PSS_PADDING) ||
                1 != EVP_PKEY_CTX_set_rsa_pss_saltlen(pkey_ctx, RSA_PSS_SALTLEN_DIGEST)) {
                EVP_PKEY_CTX_free(pkey_ctx);
                ERR_clear_error();
                continue;
            }
        }

        /* Verify pre-computed digest against the stored signature */
        verify_result =
            EVP_PKEY_verify(pkey_ctx, signature, footer->signature_length, digest, (size_t)digest_len);
        EVP_PKEY_CTX_free(pkey_ctx);
        ERR_clear_error();

        if (verify_result == 1) {
            /* SUCCESS - signature matches this key */
            verified = true;
            H5PL_SIG_DEBUG_PRINT("Plugin '%s' verified with key from: %s\n", plugin_path,
                                 H5PL_keystore_g[key_idx].source);
            break;
        }
    }

    if (!verified)
        HGOTO_ERROR(H5E_PLUGIN, H5E_BADVALUE, FAIL,
                    "plugin signature verification failed: no key in keystore matched");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5PL__verify_with_all_keys() */

/*-------------------------------------------------------------------------
 * Function:    H5PL__verify_signature_appended
 *
 * Purpose:     Verify plugin digital signature
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
herr_t
H5PL__verify_signature_appended(const char *plugin_path)
{
    int               fd = -1;
    h5_stat_t         st;
    HDoff_t           file_size = 0;
    H5PL_sig_footer_t footer;
    unsigned char    *signature   = NULL;
    size_t            binary_size = 0;
    herr_t            ret_value   = SUCCEED;

    FUNC_ENTER_PACKAGE

    assert(plugin_path);

    /* Open plugin file */
    {
        int open_flags = O_RDONLY;
#ifdef O_CLOEXEC
        open_flags |= O_CLOEXEC;
#endif
        fd = HDopen(plugin_path, open_flags, 0);
    }
    if (fd < 0)
        HGOTO_ERROR(H5E_PLUGIN, H5E_CANTOPENFILE, FAIL, "cannot open plugin file");

    /* Get file size */
    if (HDfstat(fd, &st) < 0)
        HGOTO_ERROR(H5E_PLUGIN, H5E_CANTGET, FAIL, "cannot get file size");
    file_size = (HDoff_t)st.st_size;

    /* Read and validate footer */
    if (H5PL__read_and_validate_footer(fd, file_size, plugin_path, &footer, &binary_size) < 0)
        HGOTO_ERROR(H5E_PLUGIN, H5E_READERROR, FAIL, "cannot read or validate signature footer");

    /* Read signature data */
    if (NULL == (signature = (unsigned char *)H5MM_malloc(footer.signature_length)))
        HGOTO_ERROR(H5E_PLUGIN, H5E_CANTALLOC, FAIL, "cannot allocate signature buffer");

    if (H5PL__read_file_data(fd, (HDoff_t)binary_size, signature, footer.signature_length, plugin_path) < 0)
        HGOTO_ERROR(H5E_PLUGIN, H5E_READERROR, FAIL, "cannot read signature data");

    /* Initialize keystore on first use */
    if (!H5PL_keystore_initialized_g) {
        if (H5PL__init_keystore() < 0)
            HGOTO_ERROR(H5E_PLUGIN, H5E_CANTINIT, FAIL, "cannot initialize keystore");
    }

    /* Check if signature is revoked */
    if (H5PL__is_signature_revoked(signature, footer.signature_length))
        HGOTO_ERROR(H5E_PLUGIN, H5E_BADVALUE, FAIL, "plugin signature has been revoked: %s", plugin_path);

    /* Must have at least one key */
    if (H5PL_keystore_count_g == 0)
        HGOTO_ERROR(H5E_PLUGIN, H5E_BADVALUE, FAIL,
                    "no valid public keys found for plugin signature verification\n"
                    "\n"
                    "Configure keys via:\n"
                    "  - Environment: export HDF5_PLUGIN_KEYSTORE=/path/to/keys\n"
                    "  - CMake: -DHDF5_PLUGIN_KEYSTORE_DIR=/path/to/keys\n"
                    "\n"
                    "Verify:\n"
                    "  - Directory exists and is readable\n"
                    "  - Directory contains .pem files\n"
                    "  - .pem files are valid RSA public keys");

    /* Verify signature with all keys in keystore */
    if (H5PL__verify_with_all_keys(fd, binary_size, signature, &footer, plugin_path) < 0)
        HGOTO_ERROR(H5E_PLUGIN, H5E_BADVALUE, FAIL, "signature verification failed");

    /* Close file after verification */
    HDclose(fd);
    fd = -1;

done:
    if (fd >= 0)
        HDclose(fd);
    if (signature)
        H5MM_xfree(signature);

    ERR_clear_error();

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5PL__verify_signature_appended() */

/*-------------------------------------------------------------------------
 * Function:    H5PL__cleanup_signature_resources
 *
 * Purpose:     Clean up keystore and revocation list
 *
 * Return:      SUCCEED
 *-------------------------------------------------------------------------
 */
herr_t
H5PL__cleanup_signature_resources(void)
{
    FUNC_ENTER_PACKAGE_NOERR

    /* Free all keys in the keystore and revocation list */
    H5PL__free_keystore();

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5PL__cleanup_signature_resources() */

#endif /* H5_REQUIRE_DIGITAL_SIGNATURE */
