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
/* Windows security APIs for ACL checking */
#include <sddl.h>
#include <aclapi.h>
#include <shlobj.h>
#endif

/*******************/
/* Local Variables */
/*******************/

/*
 * Thread Safety Note:
 * All file-scope static variables below (keystore, revocation list, and signature cache)
 * are accessed without explicit synchronization. When HDF5_ENABLE_THREADSAFE is enabled,
 * these variables are protected by the HDF5 library-wide global lock that guards plugin
 * operations. Concurrent plugin loads are serialized at the H5PL__load level, ensuring
 * that keystore initialization, revocation list checks, and cache updates cannot race.
 *
 * If plugin loading is ever made to bypass the global lock, these data structures will
 * require explicit mutex protection or atomic operations.
 *
 * TODO: If H5PL__load is ever refactored to support fine-grained locking or lock-free
 *       concurrent plugin loading, wrap these static globals in a struct protected by
 *       a dedicated mutex (e.g., H5PL_sig_lock_g). This affects:
 *       - H5PL_keystore_g and related counters (lines 73-76)
 *       - H5PL_revoked_sigs_g and related counters (lines 84-87)
 *       - H5PL_sig_cache_g and related counters (lines 97-99)
 *       All read/write operations on these variables must be synchronized if the
 *       global library lock is removed or bypassed for plugin operations.
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

/* Revocation list for blocking specific signatures */
#define H5PL_SIGNATURE_HASH_SIZE 32 /* SHA-256 = 32 bytes */
typedef struct H5PL_revoked_signature_t {
    unsigned char hash[H5PL_SIGNATURE_HASH_SIZE]; /* SHA-256 hash of signature */
} H5PL_revoked_signature_t;

/* TODO (Thread Safety): Requires mutex protection if global lock is removed */
static H5PL_revoked_signature_t *H5PL_revoked_sigs_g             = NULL;
static size_t                    H5PL_revoked_sigs_count_g       = 0;
static size_t                    H5PL_revoked_sigs_capacity_g    = 0;
static bool                      H5PL_revoked_sigs_initialized_g = false;

/* Signature verification cache entry */
typedef struct H5PL_signature_cache_entry_t {
    char   *path;      /* Plugin file path */
    time_t  mtime;     /* File modification time */
    HDoff_t file_size; /* File size (guards against mtime-preserving replacement) */
    bool    verified;  /* Verification status (true=success, false=failure) */
} H5PL_signature_cache_entry_t;

/* Signature verification cache
 * TODO (Thread Safety): Requires mutex protection if global lock is removed
 */
static H5PL_signature_cache_entry_t *H5PL_sig_cache_g          = NULL;
static size_t                        H5PL_sig_cache_count_g    = 0;
static size_t                        H5PL_sig_cache_capacity_g = 0;

/* Initial capacity for keystore array */
#define H5PL_KEYSTORE_INITIAL_CAPACITY 4

/* Initial capacity for signature cache */
#define H5PL_SIG_CACHE_INITIAL_CAPACITY 8

/* Maximum signature size (1024 bytes) */
#define H5PL_MAX_SIGNATURE_SIZE 1024

/* Maximum plugin file size (1GB - prevents unreasonable allocations) */
#define H5PL_MAX_PLUGIN_SIZE ((HDoff_t)(1024 * 1024 * 1024))

/* I/O chunk size for verification (1MB - optimized for modern I/O subsystems) */
#define H5PL_VERIFY_CHUNK_SIZE ((size_t)(1024 * 1024))

/* Signature verification failure reasons for detailed diagnostics */
typedef enum {
    H5PL_VERIFY_REASON_UNKNOWN,       /* Unknown/uninitialized */
    H5PL_VERIFY_REASON_INIT_FAILED,   /* EVP_DigestVerifyInit failed (key incompatible) */
    H5PL_VERIFY_REASON_UPDATE_FAILED, /* EVP_DigestVerifyUpdate failed (I/O error) */
    H5PL_VERIFY_REASON_INVALID_SIG,   /* EVP_DigestVerifyFinal = 0 (signature mismatch) */
    H5PL_VERIFY_REASON_CRYPTO_ERROR   /* EVP_DigestVerifyFinal = -1 (OpenSSL error) */
} H5PL_verify_failure_reason_t;

/*********************/
/* Local Prototypes  */
/*********************/
static int    H5PL__compare_signature_hashes(const void *a, const void *b);
static herr_t H5PL__load_revoked_signatures(const char *keystore_dir);
static bool   H5PL__is_signature_revoked(const unsigned char *signature, size_t signature_len);

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
                offset += bytes_read;
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
H5PL__get_hash_algorithm(uint8_t algorithm_id)
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

            /* Future algorithms can be added here:
            case H5PL_SIG_ALGO_SHA3_256:
                ret_value = EVP_sha3_256();
                break;
            */

        default:
            /* Unknown algorithm - return NULL */
            ret_value = NULL;
            break;
    }

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5PL__get_hash_algorithm() */

/*-------------------------------------------------------------------------
 * Function:    H5PL__create_public_RSA_from_string
 *
 * Purpose:     Create EVP public key from PEM string
 *
 * Return:      Success: Pointer to EVP_PKEY
 *              Failure: NULL
 *-------------------------------------------------------------------------
 */
static EVP_PKEY *
H5PL__create_public_RSA_from_string(const char *key_string)
{
    BIO      *key_bio   = NULL;
    EVP_PKEY *pkey      = NULL;
    EVP_PKEY *ret_value = NULL;

    FUNC_ENTER_PACKAGE

    assert(key_string);

    /* Create BIO from string */
    if (NULL == (key_bio = BIO_new_mem_buf(key_string, -1))) {
        unsigned long ssl_err = ERR_get_error();
        char          err_buf[256];
        ERR_error_string_n(ssl_err, err_buf, sizeof(err_buf));
        HGOTO_ERROR(H5E_PLUGIN, H5E_CANTCREATE, NULL, "cannot create BIO from key string: %s", err_buf);
    }

    /* Read public key using modern EVP API */
    if (NULL == (pkey = PEM_read_bio_PUBKEY(key_bio, NULL, NULL, NULL))) {
        unsigned long ssl_err = ERR_get_error();
        char          err_buf[256];
        ERR_error_string_n(ssl_err, err_buf, sizeof(err_buf));
        HGOTO_ERROR(H5E_PLUGIN, H5E_CANTGET, NULL, "cannot read public key from BIO: %s", err_buf);
    }

    /* Validate key type - only RSA keys are supported */
    {
        int key_type = EVP_PKEY_base_id(pkey);
        if (key_type != EVP_PKEY_RSA && key_type != EVP_PKEY_RSA_PSS) {
            HGOTO_ERROR(H5E_PLUGIN, H5E_BADVALUE, NULL, "unsupported key type (expected RSA, got type %d)",
                        key_type);
        }
    }

    ret_value = pkey;
    pkey      = NULL; /* Prevent cleanup */

done:
    if (key_bio)
        BIO_free(key_bio);
    if (pkey)
        EVP_PKEY_free(pkey);

    /* Clear any remaining OpenSSL errors from the error queue */
    ERR_clear_error();

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5PL__create_public_RSA_from_string() */

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

    /* Add key to keystore */
    H5PL_keystore_g[H5PL_keystore_count_g].key = key;

    if (NULL == (H5PL_keystore_g[H5PL_keystore_count_g].source = H5MM_strdup(source)))
        HGOTO_ERROR(H5E_PLUGIN, H5E_CANTALLOC, FAIL, "cannot duplicate key source string");

    H5PL_keystore_count_g++;

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
    FILE     *key_file  = NULL;
    EVP_PKEY *pkey      = NULL;
    EVP_PKEY *ret_value = NULL;

    FUNC_ENTER_PACKAGE

    assert(file_path);

    /* Open key file */
    if (NULL == (key_file = fopen(file_path, "r"))) {
        /* Don't error - just skip invalid files */
        goto done;
    }

    /* Read public key using modern EVP API */
    if (NULL == (pkey = PEM_read_PUBKEY(key_file, NULL, NULL, NULL))) {
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
    if (key_file)
        fclose(key_file);
    if (pkey)
        EVP_PKEY_free(pkey);

    /* Clear any remaining OpenSSL errors from the error queue */
    ERR_clear_error();

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5PL__create_public_RSA_from_file() */

/*-------------------------------------------------------------------------
 * Function:    H5PL__validate_directory_permissions
 *
 * Purpose:     Validate directory permissions
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
#ifndef H5_HAVE_WIN32_API
herr_t
H5PL__validate_directory_permissions(const char *dir_path)
{
    h5_stat_t st;
    herr_t    ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE

    assert(dir_path);

    /* Check if directory exists and get permissions */
    if (HDstat(dir_path, &st) < 0)
        HGOTO_ERROR(H5E_PLUGIN, H5E_CANTGET, FAIL, "cannot stat keystore directory: %s", dir_path);

    /* Verify it's a directory */
    if (!S_ISDIR(st.st_mode))
        HGOTO_ERROR(H5E_PLUGIN, H5E_BADVALUE, FAIL, "keystore path is not a directory: %s", dir_path);

    /* Reject world-writable directories */
    if (st.st_mode & S_IWOTH)
        HGOTO_ERROR(H5E_PLUGIN, H5E_BADVALUE, FAIL,
                    "SECURITY ERROR: keystore directory is world-writable (mode %o): %s\n"
                    "This allows unprivileged users to add malicious keys.\n"
                    "Fix with: chmod o-w %s",
                    (unsigned)(st.st_mode & 0777), dir_path, dir_path);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5PL__validate_directory_permissions() */
#else  /* H5_HAVE_WIN32_API */
herr_t
H5PL__validate_directory_permissions(const char *dir_path)
{
    h5_stat_t                st;
    PSECURITY_DESCRIPTOR     pSD           = NULL;
    PACL                     pDACL         = NULL;
    PSID                     pSidEveryone  = NULL;
    PSID                     pSidUsers     = NULL;
    PSID                     pSidAuthUsers = NULL;
    SID_IDENTIFIER_AUTHORITY SIDAuthWorld  = SECURITY_WORLD_SID_AUTHORITY;
    SID_IDENTIFIER_AUTHORITY SIDAuthNT     = SECURITY_NT_AUTHORITY;
    DWORD                    dwRes         = 0;
    TRUSTEE                  trusteeEveryone;
    TRUSTEE                  trusteeUsers;
    TRUSTEE                  trusteeAuthUsers;
    ACCESS_MASK              everyoneAccess       = 0;
    ACCESS_MASK              usersAccess          = 0;
    ACCESS_MASK              authUsersAccess      = 0;
    BOOL                     hasUnsafePermissions = FALSE;
    herr_t                   ret_value            = SUCCEED;

    FUNC_ENTER_PACKAGE

    assert(dir_path);

    /* Check if directory exists and get permissions */
    if (HDstat(dir_path, &st) < 0)
        HGOTO_ERROR(H5E_PLUGIN, H5E_CANTGET, FAIL, "cannot stat keystore directory: %s", dir_path);

    /* Verify it's a directory */
    if (!S_ISDIR(st.st_mode))
        HGOTO_ERROR(H5E_PLUGIN, H5E_BADVALUE, FAIL, "keystore path is not a directory: %s", dir_path);

    /* Windows ACL-based permission checking */

    /* Get the security descriptor for the directory */
    dwRes = GetNamedSecurityInfoA(dir_path, SE_FILE_OBJECT, DACL_SECURITY_INFORMATION, NULL, NULL, &pDACL,
                                  NULL, &pSD);

    if (dwRes != ERROR_SUCCESS) {
        HGOTO_ERROR(H5E_PLUGIN, H5E_CANTGET, FAIL,
                    "SECURITY ERROR: Cannot retrieve ACL information for KeyStore directory: %s\n"
                    "  Error code: %lu",
                    dir_path, (unsigned long)dwRes);
    }

    /* Create SIDs for "Everyone", "Users", and "Authenticated Users" groups */
    if (!AllocateAndInitializeSid(&SIDAuthWorld, 1, SECURITY_WORLD_RID, 0, 0, 0, 0, 0, 0, 0, &pSidEveryone)) {
        LocalFree(pSD);
        HGOTO_ERROR(H5E_PLUGIN, H5E_CANTCREATE, FAIL, "SECURITY ERROR: Cannot create Everyone SID");
    }

    if (!AllocateAndInitializeSid(&SIDAuthNT, 2, SECURITY_BUILTIN_DOMAIN_RID, DOMAIN_ALIAS_RID_USERS, 0, 0, 0,
                                  0, 0, 0, &pSidUsers)) {
        FreeSid(pSidEveryone);
        LocalFree(pSD);
        HGOTO_ERROR(H5E_PLUGIN, H5E_CANTCREATE, FAIL, "SECURITY ERROR: Cannot create Users SID");
    }

    if (!AllocateAndInitializeSid(&SIDAuthNT, 1, SECURITY_AUTHENTICATED_USER_RID, 0, 0, 0, 0, 0, 0, 0,
                                  &pSidAuthUsers)) {
        FreeSid(pSidEveryone);
        FreeSid(pSidUsers);
        LocalFree(pSD);
        HGOTO_ERROR(H5E_PLUGIN, H5E_CANTCREATE, FAIL,
                    "SECURITY ERROR: Cannot create Authenticated Users SID");
    }

    /* Check effective permissions for "Everyone", "Users", and "Authenticated Users" groups */
    BuildTrusteeWithSidA(&trusteeEveryone, pSidEveryone);
    BuildTrusteeWithSidA(&trusteeUsers, pSidUsers);
    BuildTrusteeWithSidA(&trusteeAuthUsers, pSidAuthUsers);

    dwRes = GetEffectiveRightsFromAclA(pDACL, &trusteeEveryone, &everyoneAccess);
    if (dwRes == ERROR_SUCCESS) {
        /* Check if Everyone has write access (FILE_WRITE_DATA, FILE_ADD_FILE, etc.) */
        if (everyoneAccess &
            (FILE_WRITE_DATA | FILE_ADD_FILE | FILE_APPEND_DATA | DELETE | WRITE_DAC | WRITE_OWNER)) {
            hasUnsafePermissions = TRUE;
        }
    }

    dwRes = GetEffectiveRightsFromAclA(pDACL, &trusteeUsers, &usersAccess);
    if (dwRes == ERROR_SUCCESS) {
        /* Check if Users group has write access */
        if (usersAccess &
            (FILE_WRITE_DATA | FILE_ADD_FILE | FILE_APPEND_DATA | DELETE | WRITE_DAC | WRITE_OWNER)) {
            hasUnsafePermissions = TRUE;
        }
    }

    dwRes = GetEffectiveRightsFromAclA(pDACL, &trusteeAuthUsers, &authUsersAccess);
    if (dwRes == ERROR_SUCCESS) {
        /* Check if Authenticated Users group has write access */
        if (authUsersAccess &
            (FILE_WRITE_DATA | FILE_ADD_FILE | FILE_APPEND_DATA | DELETE | WRITE_DAC | WRITE_OWNER)) {
            hasUnsafePermissions = TRUE;
        }
    }

    /* Clean up Windows security resources */
    FreeSid(pSidEveryone);
    FreeSid(pSidUsers);
    FreeSid(pSidAuthUsers);
    LocalFree(pSD);

    /* SECURITY: Fail if directory has unsafe permissions */
    if (hasUnsafePermissions) {
        HGOTO_ERROR(H5E_PLUGIN, H5E_BADVALUE, FAIL,
                    "SECURITY ERROR: KeyStore directory has insecure ACL permissions: %s\n"
                    "  The directory is writable by non-administrators.\n"
                    "  Everyone access: 0x%lx\n"
                    "  Users access: 0x%lx\n"
                    "  Authenticated Users access: 0x%lx\n"
                    "  This allows unprivileged users to inject malicious keys.\n"
                    "  Fix: Use system-protected paths like:\n"
                    "    C:\\Program Files\\HDF_Group\\HDF5\\trusted_keys\n"
                    "  Or configure directory ACLs to allow write access only for Administrators:\n"
                    "    icacls \"%s\" /inheritance:r /grant Administrators:F",
                    dir_path, (unsigned long)everyoneAccess, (unsigned long)usersAccess,
                    (unsigned long)authUsersAccess, dir_path);
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5PL__validate_directory_permissions() */
#endif /* H5_HAVE_WIN32_API */

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

    /* Validate directory permissions */
    if (H5PL__validate_directory_permissions(dir_path) < 0)
        HGOTO_ERROR(H5E_PLUGIN, H5E_BADVALUE, FAIL, "keystore directory validation failed");

    /* Open directory */
    if (NULL == (dir = opendir(dir_path))) {
        /* Non-existent directory is an error */
        HGOTO_ERROR(H5E_PLUGIN, H5E_CANTOPENFILE, FAIL, "cannot open keystore directory: %s", dir_path);
    }

    dirlen = strlen(dir_path);

    /* Iterate through directory entries */
    while (NULL != (entry = readdir(dir))) {
        char     *file_path = NULL;
        EVP_PKEY *key       = NULL;
        size_t    namelen   = strlen(entry->d_name);
        size_t    path_len;

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

        /* Try to load key */
        if (NULL != (key = H5PL__create_public_RSA_from_file(file_path))) {
            /* Add to keystore */
            if (H5PL__add_key_to_keystore(key, file_path) < 0) {
                EVP_PKEY_free(key);
                H5MM_xfree(file_path);
                closedir(dir);
                HGOTO_ERROR(H5E_PLUGIN, H5E_CANTALLOC, FAIL, "cannot add key to keystore");
            }
            /* Key ownership transferred to keystore */
        }
        /* Skip files that fail to load (invalid PEM, etc.) */

        /* Clean up file path */
        H5MM_xfree(file_path);
    }

    closedir(dir);

done:
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

    /* Validate directory permissions */
    if (H5PL__validate_directory_permissions(dir_path) < 0)
        HGOTO_ERROR(H5E_PLUGIN, H5E_BADVALUE, FAIL, "keystore directory validation failed");

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
            char      file_path[MAX_PATH];
            EVP_PKEY *key = NULL;

            /* Skip directories */
            if (find_data.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)
                continue;

            /* Build full path */
            snprintf(file_path, sizeof(file_path), "%s\\%s", dir_path, find_data.cFileName);

            /* Skip symlinks and reparse points */
            if (find_data.dwFileAttributes & FILE_ATTRIBUTE_REPARSE_POINT)
                continue;

            /* Try to load key */
            if (NULL != (key = H5PL__create_public_RSA_from_file(file_path))) {
                /* Add to keystore */
                if (H5PL__add_key_to_keystore(key, file_path) < 0) {
                    EVP_PKEY_free(key);
                    HGOTO_ERROR(H5E_PLUGIN, H5E_CANTALLOC, FAIL, "cannot add key to keystore");
                }
                /* Key ownership transferred to keystore */
            }
            /* Skip files that fail to load (invalid PEM, etc.) */

        } while (FindNextFileA(dir_handle, &find_data) != 0);
    }

done:
    if (dir_handle != INVALID_HANDLE_VALUE)
        FindClose(dir_handle);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5PL__load_keys_from_directory() */
#endif /* H5_HAVE_WIN32_API */


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
            HGOTO_ERROR(H5E_PLUGIN, H5E_CANTLOAD, FAIL,
                        "failed to load keys from HDF5_PLUGIN_KEYSTORE: %s", env_keystore);
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
            /* Directory exists, try to load */
            if (H5PL__load_keys_from_directory(H5PL_KEYSTORE_DIR) < 0) {
                /* Not a fatal error - continue and report error below */
            }
            else {
                keys_loaded = true;

                /* Load revoked signatures from same directory */
                if (H5PL__load_revoked_signatures(H5PL_KEYSTORE_DIR) < 0) {
                    /* Non-fatal - continue even if revoked signatures fail to load */
                }
            }
        }
    }
#endif

    /* Must have at least one key */
    if (!keys_loaded || H5PL_keystore_count_g == 0) {
        const char *attempted_source = env_keystore ? env_keystore : H5PL_SIG_KEYSTORE_DIR_STR;

        HGOTO_ERROR(H5E_PLUGIN, H5E_BADVALUE, FAIL,
                    "no valid public keys found for plugin signature verification\n"
                    "  Attempted to load from: %s\n"
                    "  Keys found: 0\n"
                    "\n"
                    "Configure keys via:\n"
                    "  - Environment: export HDF5_PLUGIN_KEYSTORE=/path/to/keys\n"
                    "  - CMake: -DHDF5_PLUGIN_KEYSTORE_DIR=/path/to/keys\n"
                    "\n"
                    "Verify:\n"
                    "  - Directory exists and is readable\n"
                    "  - Directory contains .pem files\n"
                    "  - .pem files are valid RSA public keys",
                    attempted_source);
    }

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
    if (ret_value < 0 && H5PL_keystore_g) {
        size_t i;
        /* Free all keys that were added before failure */
        for (i = 0; i < H5PL_keystore_count_g; i++) {
            if (H5PL_keystore_g[i].key)
                EVP_PKEY_free(H5PL_keystore_g[i].key);
            if (H5PL_keystore_g[i].source)
                H5MM_xfree(H5PL_keystore_g[i].source);
        }
        H5MM_xfree(H5PL_keystore_g);
        H5PL_keystore_g          = NULL;
        H5PL_keystore_count_g    = 0;
        H5PL_keystore_capacity_g = 0;
    }

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
    path_len = strlen(keystore_dir) + strlen("/revoked_signatures.txt") + 1;
    if (NULL == (filepath = (char *)H5MM_malloc(path_len)))
        HGOTO_ERROR(H5E_PLUGIN, H5E_CANTALLOC, FAIL, "cannot allocate filepath buffer");

    if (snprintf(filepath, path_len, "%s/revoked_signatures.txt", keystore_dir) >= (int)path_len)
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

        /* Parse hex string (must be exactly 64 hex characters for SHA-256) */
        if (line_len != H5PL_SIGNATURE_HASH_SIZE * 2) {
            H5PL_SIG_DEBUG_PRINT(
                "WARNING: Ignoring invalid revoked signature hash (expected 64 hex chars): %s\n", trimmed);
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
     * (array is sorted in H5PL__load_revoked_signatures)
     */
    if (H5PL_revoked_sigs_count_g > 0) {
        H5PL_revoked_signature_t key;
        memcpy(key.hash, hash, H5PL_SIGNATURE_HASH_SIZE);

        if (NULL != bsearch(&key, H5PL_revoked_sigs_g, H5PL_revoked_sigs_count_g,
                            sizeof(H5PL_revoked_signature_t), H5PL__compare_signature_hashes)) {
            ret_value = true;
            HGOTO_DONE(true);
        }
    }

done:
    if (mdctx)
        EVP_MD_CTX_free(mdctx);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5PL__is_signature_revoked() */

/*-------------------------------------------------------------------------
 * Function:    H5PL__check_signature_cache
 *
 * Purpose:     Check signature verification cache
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
H5PL__check_signature_cache(const char *plugin_path, bool *cached_result)
{
    h5_stat_t st;
    herr_t    ret_value = FAIL; /* Default: cache miss */

    FUNC_ENTER_PACKAGE_NOERR

    assert(plugin_path);
    assert(cached_result);

    /* Get current file modification time */
    if (HDstat(plugin_path, &st) < 0)
        goto done; /* File stat failed - cache miss */

    /* Search cache for matching entry */
    for (size_t i = 0; i < H5PL_sig_cache_count_g; i++) {
        if (strcmp(H5PL_sig_cache_g[i].path, plugin_path) == 0) {
            /* Found cache entry - check if file has been modified */
            if (H5PL_sig_cache_g[i].mtime == st.st_mtime &&
                H5PL_sig_cache_g[i].file_size == (HDoff_t)st.st_size) {
                /* Cache hit! File unchanged, return cached result */
                *cached_result = H5PL_sig_cache_g[i].verified;
                ret_value      = SUCCEED;
                goto done;
            }
            else {
                /* File modified - cache entry is stale, fall through to cache miss */
                goto done;
            }
        }
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5PL__check_signature_cache() */

/*-------------------------------------------------------------------------
 * Function:    H5PL__update_signature_cache
 *
 * Purpose:     Update signature verification cache
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
H5PL__update_signature_cache(const char *plugin_path, bool verified)
{
    h5_stat_t st;
    size_t    entry_idx = H5PL_sig_cache_count_g; /* Default: add new entry */
    bool      found     = false;
    herr_t    ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE

    assert(plugin_path);

    /* Get current file modification time */
    if (HDstat(plugin_path, &st) < 0)
        HGOTO_ERROR(H5E_PLUGIN, H5E_CANTGET, FAIL, "cannot stat plugin file for cache update: %s",
                    plugin_path);

    /* Check if entry already exists (update instead of add) */
    for (size_t i = 0; i < H5PL_sig_cache_count_g; i++) {
        if (strcmp(H5PL_sig_cache_g[i].path, plugin_path) == 0) {
            entry_idx = i;
            found     = true;
            break;
        }
    }

    if (found) {
        /* Update existing entry */
        H5PL_sig_cache_g[entry_idx].mtime     = st.st_mtime;
        H5PL_sig_cache_g[entry_idx].file_size = (HDoff_t)st.st_size;
        H5PL_sig_cache_g[entry_idx].verified  = verified;
    }
    else {
        /* Add new entry - expand cache if needed */
        if (H5PL_sig_cache_count_g >= H5PL_sig_cache_capacity_g) {
            size_t new_capacity = H5PL_sig_cache_capacity_g == 0 ? H5PL_SIG_CACHE_INITIAL_CAPACITY
                                                                 : H5PL_sig_cache_capacity_g * 2;
            H5PL_signature_cache_entry_t *new_cache = (H5PL_signature_cache_entry_t *)H5MM_realloc(
                H5PL_sig_cache_g, new_capacity * sizeof(H5PL_signature_cache_entry_t));

            if (NULL == new_cache)
                HGOTO_ERROR(H5E_PLUGIN, H5E_CANTALLOC, FAIL, "cannot expand signature cache array");

            H5PL_sig_cache_g          = new_cache;
            H5PL_sig_cache_capacity_g = new_capacity;
        }

        if (NULL == (H5PL_sig_cache_g[entry_idx].path = H5MM_strdup(plugin_path)))
            HGOTO_ERROR(H5E_PLUGIN, H5E_CANTALLOC, FAIL, "cannot duplicate path for signature cache");

        H5PL_sig_cache_g[entry_idx].mtime     = st.st_mtime;
        H5PL_sig_cache_g[entry_idx].file_size = (HDoff_t)st.st_size;
        H5PL_sig_cache_g[entry_idx].verified  = verified;
        H5PL_sig_cache_count_g++;
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5PL__update_signature_cache() */

/*-------------------------------------------------------------------------
 * Function:    H5PL__verify_with_chunked_io
 *
 * Purpose:     Verify signature using chunked I/O to minimize memory usage
 *
 * Return:      1 = signature valid
 *              0 = signature invalid
 *             -1 = error occurred
 *-------------------------------------------------------------------------
 */
static int
H5PL__verify_with_chunked_io(int fd, HDoff_t binary_size, const unsigned char *signature, size_t sig_len,
                             const EVP_MD *hash_algorithm, EVP_PKEY *public_key, uint8_t algorithm_id,
                             const char *plugin_path)
{
    EVP_MD_CTX    *mdctx      = NULL;
    EVP_PKEY_CTX  *pkey_ctx   = NULL;
    unsigned char *chunk_buf  = NULL;
    HDoff_t        bytes_read = 0;
    int            ret_value  = -1;

    FUNC_ENTER_PACKAGE

    /* Allocate chunk buffer */
    if (NULL == (chunk_buf = (unsigned char *)H5MM_malloc(H5PL_VERIFY_CHUNK_SIZE)))
        HGOTO_ERROR(H5E_PLUGIN, H5E_CANTALLOC, -1, "cannot allocate chunk buffer");

    /* Create digest context */
    if (NULL == (mdctx = EVP_MD_CTX_new()))
        HGOTO_ERROR(H5E_PLUGIN, H5E_CANTCREATE, -1, "cannot create digest context");

    /* Initialize verification */
    if (1 != EVP_DigestVerifyInit(mdctx, &pkey_ctx, hash_algorithm, NULL, public_key)) {
        ret_value = -1;
        goto done;
    }

    /* Configure PSS padding if needed */
    if (algorithm_id == H5PL_SIG_ALGO_SHA256_PSS || algorithm_id == H5PL_SIG_ALGO_SHA384_PSS ||
        algorithm_id == H5PL_SIG_ALGO_SHA512_PSS) {

        if (1 != EVP_PKEY_CTX_set_rsa_padding(pkey_ctx, RSA_PKCS1_PSS_PADDING))
            HGOTO_ERROR(H5E_PLUGIN, H5E_CANTSET, -1, "cannot set PSS padding");

        if (1 != EVP_PKEY_CTX_set_rsa_pss_saltlen(pkey_ctx, RSA_PSS_SALTLEN_DIGEST))
            HGOTO_ERROR(H5E_PLUGIN, H5E_CANTSET, -1, "cannot set PSS salt length");
    }

    /* Read and hash file in chunks */
    while (bytes_read < binary_size) {
        size_t chunk_size = (size_t)((binary_size - bytes_read) > (HDoff_t)H5PL_VERIFY_CHUNK_SIZE
                                         ? H5PL_VERIFY_CHUNK_SIZE
                                         : (size_t)(binary_size - bytes_read));

        if (H5PL__read_file_data(fd, bytes_read, chunk_buf, chunk_size, plugin_path) < 0) {
            ret_value = -1;
            goto done;
        }

        if (1 != EVP_DigestVerifyUpdate(mdctx, chunk_buf, chunk_size)) {
            ret_value = -1;
            goto done;
        }

        bytes_read += (HDoff_t)chunk_size;
    }

    /* Finalize verification */
    ret_value = EVP_DigestVerifyFinal(mdctx, signature, sig_len);

done:
    if (chunk_buf)
        H5MM_xfree(chunk_buf);
    if (mdctx)
        EVP_MD_CTX_free(mdctx);
    ERR_clear_error();

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5PL__verify_with_chunked_io() */

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
    uint8_t  footer_buf[H5PL_SIG_FOOTER_SIZE];
    uint8_t *p         = footer_buf;
    herr_t   ret_value = SUCCEED;

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

    /* Decode footer (little-endian to native byte order) */
    UINT32DECODE(p, footer_out->signature_length);
    footer_out->algorithm_id   = *p++;
    footer_out->format_version = *p++;
    UINT16DECODE(p, footer_out->reserved);
    UINT32DECODE(p, footer_out->magic);

    /* Validate magic number */
    if (footer_out->magic != H5PL_SIG_MAGIC)
        HGOTO_ERROR(H5E_PLUGIN, H5E_BADVALUE, FAIL,
                    "invalid signature magic number (expected 0x%08X, got 0x%08X) - "
                    "not a signed HDF5 plugin or corrupted",
                    (unsigned)H5PL_SIG_MAGIC, (unsigned)footer_out->magic);

    /* Validate format version */
    if (footer_out->format_version != H5PL_SIG_FORMAT_VERSION_CURRENT)
        HGOTO_ERROR(H5E_PLUGIN, H5E_BADVALUE, FAIL, "unsupported signature format version %u (expected %u)",
                    (unsigned)footer_out->format_version, (unsigned)H5PL_SIG_FORMAT_VERSION_CURRENT);

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

        /* Check for overflow when casting to size_t */
        if (binary_size_off < 0 || (uint64_t)binary_size_off > (uint64_t)SIZE_MAX)
            HGOTO_ERROR(
                H5E_PLUGIN, H5E_BADVALUE, FAIL,
                "plugin binary size %llu exceeds SIZE_MAX - file too large to verify on this platform",
                (unsigned long long)binary_size_off);

        *binary_size_out = (size_t)binary_size_off;
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5PL__read_and_validate_footer() */

/*-------------------------------------------------------------------------
 * Function:    H5PL__verify_with_all_keys
 *
 * Purpose:     Try verifying signature with each key in the keystore
 *
 * Return:      SUCCEED if signature verified with at least one key
 *              FAIL otherwise
 *-------------------------------------------------------------------------
 */
static herr_t
H5PL__verify_with_all_keys(int fd, size_t binary_size, const unsigned char *signature,
                           const H5PL_sig_footer_t *footer, const char *plugin_path)
{
    const EVP_MD                *hash_algorithm       = NULL;
    H5PL_verify_failure_reason_t first_failure_reason = H5PL_VERIFY_REASON_UNKNOWN;
    size_t                       keys_init_failed     = 0;
    size_t                       keys_update_failed   = 0;
    size_t                       keys_crypto_invalid  = 0;
    size_t                       keys_crypto_error    = 0;
    bool                         verified             = false;
    herr_t                       ret_value            = SUCCEED;

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

    /* Try each key in keystore (OR logic - first match wins) */
    for (size_t key_idx = 0; key_idx < H5PL_keystore_count_g; key_idx++) {
        EVP_PKEY *public_key = H5PL_keystore_g[key_idx].key;
        int       verify_result =
            H5PL__verify_with_chunked_io(fd, (HDoff_t)binary_size, signature, footer->signature_length,
                                         hash_algorithm, public_key, footer->algorithm_id, plugin_path);

        if (verify_result == 1) {
            /* SUCCESS! Signature verified with this key */
            verified = true;
            H5PL_SIG_DEBUG_PRINT("Plugin '%s' verified with key from: %s\n", plugin_path,
                                 H5PL_keystore_g[key_idx].source);
            break;
        }
        else if (verify_result == 0) {
            /* Signature is cryptographically invalid (hash mismatch) */
            keys_crypto_invalid++;
            if (first_failure_reason == H5PL_VERIFY_REASON_UNKNOWN)
                first_failure_reason = H5PL_VERIFY_REASON_INVALID_SIG;
        }
        else {
            /* Error occurred - could be init, update, or crypto error */
            keys_crypto_error++;
            if (first_failure_reason == H5PL_VERIFY_REASON_UNKNOWN)
                first_failure_reason = H5PL_VERIFY_REASON_CRYPTO_ERROR;
        }

        /* Clear OpenSSL errors before trying next key */
        ERR_clear_error();
    }

    if (!verified) {
        /* Build informative error message with key sources for debugging */
        char        key_sources[1024] = "";
        size_t      remaining         = sizeof(key_sources);
        char       *ptr               = key_sources;
        const char *diagnostic        = NULL;
        const char *keystore_path     = NULL;

        for (size_t i = 0; i < H5PL_keystore_count_g; i++) {
            const char *source  = H5PL_keystore_g[i].source ? H5PL_keystore_g[i].source : "unknown";
            int         written = snprintf(ptr, remaining, "%s%s", (i > 0 ? ", " : ""), source);

            if (written < 0 || (size_t)written >= remaining) {
                if (remaining > 4)
                    memcpy(ptr, "...", 4); /* 4 = strlen("...") + 1 (NUL) */
                break;
            }
            ptr += written;
            remaining -= (size_t)written;
        }

        /* Build detailed diagnostic message based on failure pattern */
        if (keys_init_failed == H5PL_keystore_count_g) {
            diagnostic = "\n"
                         "  DIAGNOSIS: All keys failed initialization (key type mismatch)\n"
                         "  - Plugin signature algorithm may be incompatible with KeyStore keys\n"
                         "  - Verify that KeyStore contains RSA keys matching the signature algorithm\n"
                         "  - Check signature algorithm ID in plugin footer\n";
        }
        else if (keys_crypto_invalid == H5PL_keystore_count_g) {
            diagnostic = "\n"
                         "  DIAGNOSIS: Signature cryptographically invalid with ALL keys\n"
                         "  - Plugin is either:\n"
                         "    * Signed with a different key (not in KeyStore)\n"
                         "    * Tampered after signing (binary modified)\n"
                         "    * Corrupted during download/transfer (I/O error)\n"
                         "  - Try:\n"
                         "    * Obtain the correct public key from plugin developer\n"
                         "    * Re-download the plugin file\n"
                         "    * Verify file integrity (checksums)\n";
        }
        else if (keys_crypto_invalid > 0 && keys_crypto_invalid < H5PL_keystore_count_g) {
            diagnostic = "\n"
                         "  DIAGNOSIS: Signature failed with some keys (not all)\n"
                         "  - Plugin may be signed with a key not in your KeyStore\n"
                         "  - Add the correct public key to KeyStore directory\n";
        }
        else if (keys_update_failed > 0) {
            diagnostic = "\n"
                         "  DIAGNOSIS: Hash computation failed (I/O error)\n"
                         "  - File may be corrupted or inaccessible\n"
                         "  - Check file permissions and disk errors\n";
        }
        else if (keys_crypto_error > 0) {
            diagnostic = "\n"
                         "  DIAGNOSIS: OpenSSL internal error\n"
                         "  - Check OpenSSL installation and configuration\n"
                         "  - Review system logs for OpenSSL errors\n";
        }
        else {
            diagnostic = "\n"
                         "  DIAGNOSIS: Unknown verification failure\n"
                         "  - Enable debug output with: export HDF5_DEBUG=PL\n";
        }

        keystore_path = getenv("HDF5_PLUGIN_KEYSTORE");
        if (keystore_path == NULL)
            keystore_path = H5PL_SIG_KEYSTORE_DIR_STR;

        HGOTO_ERROR(H5E_PLUGIN, H5E_BADVALUE, FAIL,
                    "plugin signature verification failed\n"
                    "  Plugin: %s\n"
                    "  Keys tried: %zu [%s]\n"
                    "  - Init failed: %zu\n"
                    "  - Update failed: %zu\n"
                    "  - Crypto invalid: %zu\n"
                    "  - Crypto error: %zu\n"
                    "%s"
                    "\n"
                    "  KeyStore: %s\n"
                    "\n"
                    "  Next steps:\n"
                    "    1. Verify plugin was signed correctly (check signature algorithm compatibility)\n"
                    "    2. Check KeyStore directory contains correct public keys\n"
                    "    3. Contact plugin developer for correct public key\n"
                    "    4. Verify file integrity (checksums, re-download if needed)\n",
                    plugin_path, H5PL_keystore_count_g, key_sources, keys_init_failed, keys_update_failed,
                    keys_crypto_invalid, keys_crypto_error, diagnostic ? diagnostic : "", keystore_path);
    }

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
    bool              cached_result;

    FUNC_ENTER_PACKAGE

    assert(plugin_path);

    /* Check signature cache first */
    if (H5PL__check_signature_cache(plugin_path, &cached_result) == SUCCEED) {
        if (cached_result)
            HGOTO_DONE(SUCCEED); /* Previously verified successfully */
        else
            HGOTO_ERROR(H5E_PLUGIN, H5E_BADVALUE, FAIL,
                        "plugin signature verification failed (cached result): %s", plugin_path);
    }

    /* Cache miss or file modified - perform full verification */

    /* Open plugin file */
    if ((fd = HDopen(plugin_path, O_RDONLY, 0)) < 0)
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
        HGOTO_ERROR(H5E_PLUGIN, H5E_BADVALUE, FAIL,
                    "plugin signature is revoked (blocklisted)\n"
                    "  Plugin: %s\n"
                    "  This specific plugin version has been revoked and will not be loaded\n"
                    "  Reason: Signature hash found in revocation list\n"
                    "\n"
                    "Action required:\n"
                    "  - Remove this plugin from your system\n"
                    "  - Contact plugin developer for updated version\n"
                    "  - Check HDF5_PLUGIN_KEYSTORE/revoked_signatures.txt for details",
                    plugin_path);

    /* Must have at least one key */
    if (H5PL_keystore_count_g == 0)
        HGOTO_ERROR(H5E_PLUGIN, H5E_BADVALUE, FAIL, "keystore is empty - no keys available for verification");

    /* Verify signature with all keys in keystore */
    if (H5PL__verify_with_all_keys(fd, binary_size, signature, &footer, plugin_path) < 0) {
        /* Cache the failed verification result */
        H5PL__update_signature_cache(plugin_path, false);
        HGOTO_ERROR(H5E_PLUGIN, H5E_BADVALUE, FAIL, "signature verification failed");
    }

    /* Cache the successful verification result */
    H5PL__update_signature_cache(plugin_path, true);

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
 * Function:    H5PL__cleanup_signature_cache
 *
 * Purpose:     Clean up keystore and signature cache
 *
 * Return:      SUCCEED
 *-------------------------------------------------------------------------
 */
herr_t
H5PL__cleanup_signature_cache(void)
{
    FUNC_ENTER_PACKAGE_NOERR

    /* Free all keys in the keystore */
    if (H5PL_keystore_initialized_g) {
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
        H5PL_keystore_count_g       = 0;
        H5PL_keystore_capacity_g    = 0;
        H5PL_keystore_initialized_g = false;
    }

    /* Free all entries in the signature verification cache */
    if (H5PL_sig_cache_g) {
        size_t i;
        for (i = 0; i < H5PL_sig_cache_count_g; i++) {
            if (H5PL_sig_cache_g[i].path)
                H5MM_xfree(H5PL_sig_cache_g[i].path);
        }
        H5MM_xfree(H5PL_sig_cache_g);
        H5PL_sig_cache_g = NULL;
    }
    H5PL_sig_cache_count_g    = 0;
    H5PL_sig_cache_capacity_g = 0;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5PL__cleanup_signature_cache() */

#endif /* H5_REQUIRE_DIGITAL_SIGNATURE */
