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

/* KeyStore entry for storing multiple trusted public keys */
typedef struct H5PL_keystore_entry_t {
    EVP_PKEY *key;    /* OpenSSL public key object */
    char     *source; /* Key source (filename or "embedded") for debugging */
} H5PL_keystore_entry_t;

/* KeyStore for signature verification */
static H5PL_keystore_entry_t *H5PL_keystore_g             = NULL;
static size_t                 H5PL_keystore_count_g       = 0;
static size_t                 H5PL_keystore_capacity_g    = 0;
static bool                   H5PL_keystore_initialized_g = false;

/* Signature verification cache entry */
typedef struct H5PL_signature_cache_entry_t {
    char  *path;     /* Plugin file path */
    time_t mtime;    /* File modification time */
    bool   verified; /* Verification status (true=success, false=failure) */
} H5PL_signature_cache_entry_t;

/* Signature verification cache */
static H5PL_signature_cache_entry_t *H5PL_sig_cache_g          = NULL;
static size_t                        H5PL_sig_cache_count_g    = 0;
static size_t                        H5PL_sig_cache_capacity_g = 0;

/* Initial capacity for keystore array */
#define H5PL_KEYSTORE_INITIAL_CAPACITY 4

/* Initial capacity for signature cache */
#define H5PL_SIG_CACHE_INITIAL_CAPACITY 8

/* Maximum signature size (1024 bytes) */
#define H5PL_MAX_SIGNATURE_SIZE 1024

/* Signature verification failure reasons for detailed diagnostics */
typedef enum {
    H5PL_VERIFY_REASON_UNKNOWN,       /* Unknown/uninitialized */
    H5PL_VERIFY_REASON_INIT_FAILED,   /* EVP_DigestVerifyInit failed (key incompatible) */
    H5PL_VERIFY_REASON_UPDATE_FAILED, /* EVP_DigestVerifyUpdate failed (I/O error) */
    H5PL_VERIFY_REASON_INVALID_SIG,   /* EVP_DigestVerifyFinal = 0 (signature mismatch) */
    H5PL_VERIFY_REASON_CRYPTO_ERROR   /* EVP_DigestVerifyFinal = -1 (OpenSSL error) */
} H5PL_verify_failure_reason_t;

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
static herr_t
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

#ifndef H5_HAVE_WIN32_API
    /* Reject world-writable directories */
    if (st.st_mode & S_IWOTH)
        HGOTO_ERROR(H5E_PLUGIN, H5E_BADVALUE, FAIL,
                    "SECURITY ERROR: keystore directory is world-writable (mode %o): %s\n"
                    "This allows unprivileged users to add malicious keys.\n"
                    "Fix with: chmod o-w %s",
                    (unsigned)(st.st_mode & 0777), dir_path, dir_path);
#else
    /* Windows ACL-based permission checking */
    {
        PSECURITY_DESCRIPTOR     pSD          = NULL;
        PACL                     pDACL        = NULL;
        PSID                     pSidEveryone = NULL;
        PSID                     pSidUsers    = NULL;
        SID_IDENTIFIER_AUTHORITY SIDAuthWorld = SECURITY_WORLD_SID_AUTHORITY;
        SID_IDENTIFIER_AUTHORITY SIDAuthNT    = SECURITY_NT_AUTHORITY;
        DWORD                    dwRes        = 0;
        TRUSTEE                  trusteeEveryone;
        TRUSTEE                  trusteeUsers;
        ACCESS_MASK              everyoneAccess       = 0;
        ACCESS_MASK              usersAccess          = 0;
        BOOL                     hasUnsafePermissions = FALSE;

        /* Get the security descriptor for the directory */
        dwRes = GetNamedSecurityInfoA(dir_path, SE_FILE_OBJECT, DACL_SECURITY_INFORMATION, NULL, NULL, &pDACL,
                                      NULL, &pSD);

        if (dwRes != ERROR_SUCCESS) {
            HGOTO_ERROR(H5E_PLUGIN, H5E_CANTGET, FAIL,
                        "SECURITY ERROR: Cannot retrieve ACL information for KeyStore directory: %s\n"
                        "  Error code: %lu",
                        dir_path, (unsigned long)dwRes);
        }

        /* Create SIDs for "Everyone" and "Users" groups */
        if (!AllocateAndInitializeSid(&SIDAuthWorld, 1, SECURITY_WORLD_RID, 0, 0, 0, 0, 0, 0, 0,
                                      &pSidEveryone)) {
            LocalFree(pSD);
            HGOTO_ERROR(H5E_PLUGIN, H5E_CANTCREATE, FAIL, "SECURITY ERROR: Cannot create Everyone SID");
        }

        if (!AllocateAndInitializeSid(&SIDAuthNT, 2, SECURITY_BUILTIN_DOMAIN_RID, DOMAIN_ALIAS_RID_USERS, 0,
                                      0, 0, 0, 0, 0, &pSidUsers)) {
            FreeSid(pSidEveryone);
            LocalFree(pSD);
            HGOTO_ERROR(H5E_PLUGIN, H5E_CANTCREATE, FAIL, "SECURITY ERROR: Cannot create Users SID");
        }

        /* Check effective permissions for "Everyone" and "Users" groups */
        BuildTrusteeWithSidA(&trusteeEveryone, pSidEveryone);
        BuildTrusteeWithSidA(&trusteeUsers, pSidUsers);

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

        /* Clean up Windows security resources */
        FreeSid(pSidEveryone);
        FreeSid(pSidUsers);
        LocalFree(pSD);

        /* SECURITY: Fail if directory has unsafe permissions */
        if (hasUnsafePermissions) {
            HGOTO_ERROR(H5E_PLUGIN, H5E_BADVALUE, FAIL,
                        "SECURITY ERROR: KeyStore directory has insecure ACL permissions: %s\n"
                        "  The directory is writable by non-administrators.\n"
                        "  Everyone access: 0x%lx\n"
                        "  Users access: 0x%lx\n"
                        "  This allows unprivileged users to inject malicious keys.\n"
                        "  Fix: Use system-protected paths like:\n"
                        "    C:\\Program Files\\HDF_Group\\HDF5\\trusted_keys\n"
                        "  Or configure directory ACLs to allow write access only for Administrators:\n"
                        "    icacls \"%s\" /inheritance:r /grant Administrators:F",
                        dir_path, (unsigned long)everyoneAccess, (unsigned long)usersAccess, dir_path);
        }
    }
#endif

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5PL__validate_directory_permissions() */

/*-------------------------------------------------------------------------
 * Function:    H5PL__load_keys_from_directory
 *
 * Purpose:     Load all .pem files from a directory into the keystore
 *
 * Return:      SUCCEED/FAIL (fails if directory invalid, but skips bad files)
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5PL__load_keys_from_directory(const char *dir_path)
{
#ifdef H5_HAVE_WIN32_API
    H5PL_HANDLE dir_handle = INVALID_HANDLE_VALUE;
#else
    H5PL_HANDLE dir_handle = NULL;
#endif
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE

    assert(dir_path);

    /* Validate directory permissions */
    if (H5PL__validate_directory_permissions(dir_path) < 0)
        HGOTO_ERROR(H5E_PLUGIN, H5E_BADVALUE, FAIL, "keystore directory validation failed");

#ifdef H5_HAVE_WIN32_API
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
#else
    {
        DIR           *dir    = NULL;
        struct dirent *entry  = NULL;
        size_t         dirlen = 0;

        /* Open directory */
        if (NULL == (dir = opendir(dir_path))) {
            /* Non-existent directory is an error */
            HGOTO_ERROR(H5E_PLUGIN, H5E_CANTOPENFILE, FAIL, "cannot open keystore directory: %s", dir_path);
        }

        dirlen = strlen(dir_path);

        /* Iterate through directory entries */
        while (NULL != (entry = readdir(dir))) {
            char      file_path[4096];
            EVP_PKEY *key     = NULL;
            size_t    namelen = strlen(entry->d_name);

            /* Skip . and .. */
            if (strcmp(entry->d_name, ".") == 0 || strcmp(entry->d_name, "..") == 0)
                continue;

            /* Only process .pem files */
            if (namelen < 5 || strcmp(entry->d_name + namelen - 4, ".pem") != 0)
                continue;

            /* Build full path */
            if (dirlen + namelen + 2 > sizeof(file_path))
                continue; /* Path too long, skip */

            snprintf(file_path, sizeof(file_path), "%s/%s", dir_path, entry->d_name);

            /* Skip symlinks */
            {
                h5_stat_t file_stat;
                if (HDlstat(file_path, &file_stat) < 0) {
                    fprintf(stderr, "WARNING: Cannot stat key file %s: %s\n", file_path, strerror(errno));
                    continue;
                }

                if (S_ISLNK(file_stat.st_mode)) {
                    fprintf(stderr, "WARNING: Skipping symlink %s (security policy)\n", file_path);
                    continue;
                }
            }

            /* Try to load key */
            if (NULL != (key = H5PL__create_public_RSA_from_file(file_path))) {
                /* Add to keystore */
                if (H5PL__add_key_to_keystore(key, file_path) < 0) {
                    EVP_PKEY_free(key);
                    closedir(dir);
                    HGOTO_ERROR(H5E_PLUGIN, H5E_CANTALLOC, FAIL, "cannot add key to keystore");
                }
                /* Key ownership transferred to keystore */
            }
            /* Skip files that fail to load (invalid PEM, etc.) */
        }

        closedir(dir);
    }
#endif

done:
#ifdef H5_HAVE_WIN32_API
    if (dir_handle != INVALID_HANDLE_VALUE)
        FindClose(dir_handle);
#endif

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5PL__load_keys_from_directory() */

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

    /* 1. Check environment variable (highest priority) */
    if (NULL != (env_keystore = getenv("HDF5_PLUGIN_KEYSTORE"))) {
        if (H5PL__load_keys_from_directory(env_keystore) < 0)
            HGOTO_ERROR(H5E_PLUGIN, H5E_CANTLOAD, FAIL, "failed to load keys from HDF5_PLUGIN_KEYSTORE: %s",
                        env_keystore);
        keys_loaded = true;
    }

/* 2. Check CMake-configured directory */
#ifdef H5PL_KEYSTORE_DIR
    if (!keys_loaded) {
        /* Only try if directory was configured */
        h5_stat_t st;
        if (HDstat(H5PL_KEYSTORE_DIR, &st) == 0) {
            /* Directory exists, try to load */
            if (H5PL__load_keys_from_directory(H5PL_KEYSTORE_DIR) < 0) {
                /* Not a fatal error - fall through to embedded key */
            }
            else {
                keys_loaded = true;
            }
        }
    }
#endif

/* 3. Fallback to compile-time embedded key (backward compatibility) */
#ifdef H5PL_PUBLIC_KEY_PEM
    if (!keys_loaded) {
        EVP_PKEY *embedded_key = H5PL__create_public_RSA_from_string(H5PL_PUBLIC_KEY_PEM);
        if (NULL != embedded_key) {
            if (H5PL__add_key_to_keystore(embedded_key, "embedded") < 0) {
                EVP_PKEY_free(embedded_key);
                HGOTO_ERROR(H5E_PLUGIN, H5E_CANTALLOC, FAIL, "cannot add embedded key to keystore");
            }
            keys_loaded = true;
        }
    }
#endif

    /* Must have at least one key */
    if (!keys_loaded || H5PL_keystore_count_g == 0) {
        const char *attempted_source = env_keystore ? env_keystore :
#ifdef H5PL_KEYSTORE_DIR
                                                    H5PL_KEYSTORE_DIR
#else
                                                    "(none configured)"
#endif
            ;

        HGOTO_ERROR(H5E_PLUGIN, H5E_BADVALUE, FAIL,
                    "no valid public keys found for plugin signature verification\n"
                    "  Attempted to load from: %s\n"
                    "  Keys found: 0\n"
                    "\n"
                    "Configure keys via:\n"
                    "  - Environment: export HDF5_PLUGIN_KEYSTORE=/path/to/keys\n"
                    "  - CMake: -DHDF5_PLUGIN_KEYSTORE_DIR=/path/to/keys\n"
                    "  - Compile-time: -DHDF5_PLUGIN_PUBLIC_KEY_FILE=key.pem\n"
                    "\n"
                    "Verify:\n"
                    "  - Directory exists and is readable\n"
                    "  - Directory contains .pem files\n"
                    "  - .pem files are valid RSA public keys",
                    attempted_source);
    }

#ifdef H5PL_DEBUG_KEYSTORE
    /* Optional debug output (enable via compile-time flag) */
    if (H5PL_keystore_count_g > 0) {
        fprintf(stderr, "HDF5 Plugin KeyStore initialized:\n");
        fprintf(stderr, "  Keys loaded: %zu\n", H5PL_keystore_count_g);
        for (size_t i = 0; i < H5PL_keystore_count_g; i++) {
            fprintf(stderr, "  [%zu] %s\n", i + 1, H5PL_keystore_g[i].source);
        }
    }
#endif

done:
    /* Cleanup on initialization failure */
    if (ret_value < 0 && H5PL_keystore_g) {
        size_t i;
        /* Free all keys that were added before failure */
        for (i = 0; i < H5PL_keystore_count_g; i++) {
            if (H5PL_keystore_g[i].key)
                EVP_PKEY_free(H5PL_keystore_g[i].key);
            if (H5PL_keystore_g[i].source)
                free(H5PL_keystore_g[i].source);
        }
        H5MM_xfree(H5PL_keystore_g);
        H5PL_keystore_g          = NULL;
        H5PL_keystore_count_g    = 0;
        H5PL_keystore_capacity_g = 0;
    }

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5PL__init_keystore() */

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
            if (H5PL_sig_cache_g[i].mtime == st.st_mtime) {
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
        H5PL_sig_cache_g[entry_idx].mtime    = st.st_mtime;
        H5PL_sig_cache_g[entry_idx].verified = verified;
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

        H5PL_sig_cache_g[entry_idx].mtime    = st.st_mtime;
        H5PL_sig_cache_g[entry_idx].verified = verified;
        H5PL_sig_cache_count_g++;
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5PL__update_signature_cache() */

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
    unsigned char    *signature     = NULL;
    unsigned char    *binary_data   = NULL;
    size_t            binary_size   = 0;
    EVP_PKEY         *public_key    = NULL;
    EVP_MD_CTX       *mdctx         = NULL;
    EVP_PKEY_CTX     *pkey_ctx      = NULL;
    int               verify_result = 0;
    herr_t            ret_value     = SUCCEED;
    bool              cached_result = false;

    FUNC_ENTER_PACKAGE

    assert(plugin_path);

    /* Check signature cache first */
    if (H5PL__check_signature_cache(plugin_path, &cached_result) == SUCCEED) {
        /* Cache hit! Return cached verification result */
        if (cached_result) {
            /* Previously verified successfully */
            HGOTO_DONE(SUCCEED);
        }
        else {
            /* Previously failed verification */
            HGOTO_ERROR(H5E_PLUGIN, H5E_BADVALUE, FAIL,
                        "plugin signature verification failed (cached result): %s", plugin_path);
        }
    }

    /* Cache miss or file modified - perform full verification */

    /* Open plugin file for reading (HDopen handles O_BINARY automatically on Windows) */
    if ((fd = HDopen(plugin_path, O_RDONLY, 0)) < 0)
        HGOTO_ERROR(H5E_PLUGIN, H5E_CANTOPENFILE, FAIL, "cannot open plugin file");

    /* Get file size using portable stat */
    if (HDfstat(fd, &st) < 0)
        HGOTO_ERROR(H5E_PLUGIN, H5E_CANTGET, FAIL, "cannot get file size");

    file_size = (HDoff_t)st.st_size;

    /* File must be large enough for footer */
    if (file_size < (HDoff_t)H5PL_SIG_FOOTER_SIZE)
        HGOTO_ERROR(H5E_PLUGIN, H5E_BADVALUE, FAIL, "file too small to contain signature footer");

    /* Read footer from end of file */
    {
        uint8_t  footer_buf[H5PL_SIG_FOOTER_SIZE];
        uint8_t *p = footer_buf;

        if (H5PL__read_file_data(fd, file_size - (HDoff_t)H5PL_SIG_FOOTER_SIZE, footer_buf,
                                 H5PL_SIG_FOOTER_SIZE, plugin_path) < 0)
            HGOTO_ERROR(H5E_PLUGIN, H5E_READERROR, FAIL, "cannot read signature footer");

        /* Decode footer (little-endian to native byte order) */
        UINT32DECODE(p, footer.signature_length);
        footer.algorithm_id   = *p++;
        footer.format_version = *p++;
        UINT16DECODE(p, footer.reserved);
        UINT32DECODE(p, footer.magic);
    }

    /* Validate magic number */
    if (footer.magic != H5PL_SIG_MAGIC)
        HGOTO_ERROR(H5E_PLUGIN, H5E_BADVALUE, FAIL,
                    "invalid signature magic number (expected 0x%08X, got 0x%08X) - "
                    "not a signed HDF5 plugin or corrupted",
                    (unsigned)H5PL_SIG_MAGIC, (unsigned)footer.magic);

    /* Validate algorithm ID */
    if (NULL == H5PL__get_hash_algorithm(footer.algorithm_id))
        HGOTO_ERROR(H5E_PLUGIN, H5E_BADVALUE, FAIL,
                    "unsupported or unknown hash algorithm ID 0x%02X in plugin signature",
                    (unsigned)footer.algorithm_id);

    /* Validate signature length */
    if (footer.signature_length == 0 || footer.signature_length > H5PL_MAX_SIGNATURE_SIZE)
        HGOTO_ERROR(H5E_PLUGIN, H5E_BADVALUE, FAIL,
                    "invalid signature length %u bytes (valid range: 1-%u bytes)", footer.signature_length,
                    H5PL_MAX_SIGNATURE_SIZE);

    /* Validate file size */
    if (file_size < (HDoff_t)(footer.signature_length + H5PL_SIG_FOOTER_SIZE))
        HGOTO_ERROR(H5E_PLUGIN, H5E_BADVALUE, FAIL, "file too small to contain claimed signature and footer");

    /* Calculate binary data size */
    {
        HDoff_t binary_size_off =
            file_size - (HDoff_t)footer.signature_length - (HDoff_t)H5PL_SIG_FOOTER_SIZE;

        /* Practical size limit: 1GB for plugin files (prevents unreasonable allocations) */
#define H5PL_MAX_PLUGIN_SIZE ((HDoff_t)(1024 * 1024 * 1024))
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

        binary_size = (size_t)binary_size_off;
#undef H5PL_MAX_PLUGIN_SIZE
    }

    /* Allocate signature buffer */
    if (NULL == (signature = (unsigned char *)H5MM_malloc(footer.signature_length)))
        HGOTO_ERROR(H5E_PLUGIN, H5E_CANTALLOC, FAIL, "cannot allocate signature buffer");

    /* Read signature using safe I/O with EINTR retry and chunking */
    if (H5PL__read_file_data(fd, (HDoff_t)binary_size, signature, footer.signature_length, plugin_path) < 0)
        HGOTO_ERROR(H5E_PLUGIN, H5E_READERROR, FAIL, "cannot read signature data");

    /* Initialize keystore on first use */
    if (!H5PL_keystore_initialized_g) {
        if (H5PL__init_keystore() < 0)
            HGOTO_ERROR(H5E_PLUGIN, H5E_CANTINIT, FAIL, "cannot initialize keystore");
    }

    /* Must have at least one key */
    if (H5PL_keystore_count_g == 0)
        HGOTO_ERROR(H5E_PLUGIN, H5E_BADVALUE, FAIL, "keystore is empty - no keys available for verification");

    /* Try verifying with each key in keystore (OR logic - first match wins) */
    {
        size_t                       key_idx;
        bool                         verified             = false;
        const EVP_MD                *hash_algorithm       = NULL;
        H5PL_verify_failure_reason_t first_failure_reason = H5PL_VERIFY_REASON_UNKNOWN;
        size_t                       keys_init_failed     = 0;
        size_t                       keys_update_failed   = 0;
        size_t                       keys_crypto_invalid  = 0;
        size_t                       keys_crypto_error    = 0;

        /* Get hash algorithm from footer (crypto-agile verification) */
        hash_algorithm = H5PL__get_hash_algorithm(footer.algorithm_id);
        if (NULL == hash_algorithm)
            HGOTO_ERROR(H5E_PLUGIN, H5E_BADVALUE, FAIL, "cannot get hash algorithm for ID 0x%02X",
                        (unsigned)footer.algorithm_id);

        for (key_idx = 0; key_idx < H5PL_keystore_count_g; key_idx++) {
            public_key = H5PL_keystore_g[key_idx].key;

            /* Create fresh message digest context for this key */
            if (NULL == (mdctx = EVP_MD_CTX_new())) {
                unsigned long ssl_err = ERR_get_error();
                char          err_buf[256];
                ERR_error_string_n(ssl_err, err_buf, sizeof(err_buf));
                HGOTO_ERROR(H5E_PLUGIN, H5E_CANTCREATE, FAIL, "cannot create message digest context: %s",
                            err_buf);
            }

            /* Initialize verification with algorithm from footer (crypto-agile) */
            if (1 != EVP_DigestVerifyInit(mdctx, &pkey_ctx, hash_algorithm, NULL, public_key)) {
                unsigned long ssl_err = ERR_get_error();
                char          err_buf[256];
                ERR_error_string_n(ssl_err, err_buf, sizeof(err_buf));

                /* Track failure for diagnostics */
                keys_init_failed++;
                if (first_failure_reason == H5PL_VERIFY_REASON_UNKNOWN)
                    first_failure_reason = H5PL_VERIFY_REASON_INIT_FAILED;

                /* Clean up and try next key */
                EVP_MD_CTX_free(mdctx);
                mdctx = NULL;
                ERR_clear_error();
                continue;
            }

            /* Hash binary data in chunks */
#define H5PL_HASH_CHUNK_SIZE ((size_t)(64 * 1024))

            /* Allocate chunk buffer */
            if (binary_data == NULL) {
                if (NULL == (binary_data = (unsigned char *)H5MM_malloc(H5PL_HASH_CHUNK_SIZE)))
                    HGOTO_ERROR(H5E_PLUGIN, H5E_CANTALLOC, FAIL, "cannot allocate hash chunk buffer");
            }

            /* Process binary data in chunks */
            {
                size_t  remaining      = binary_size;
                HDoff_t current_offset = 0;
                bool    hash_ok        = true;

                while (remaining > 0) {
                    size_t chunk_size = (remaining > H5PL_HASH_CHUNK_SIZE) ? H5PL_HASH_CHUNK_SIZE : remaining;

                    /* Read chunk from file */
                    if (H5PL__read_file_data(fd, current_offset, binary_data, chunk_size, plugin_path) < 0)
                        HGOTO_ERROR(H5E_PLUGIN, H5E_READERROR, FAIL,
                                    "cannot read binary chunk at offset %llu",
                                    (unsigned long long)current_offset);

                    /* Update hash with chunk data */
                    if (1 != EVP_DigestVerifyUpdate(mdctx, binary_data, chunk_size)) {
                        hash_ok = false;
                        break;
                    }

                    remaining -= chunk_size;
                    current_offset += (HDoff_t)chunk_size;
                }

                if (!hash_ok) {
                    /* Track failure for diagnostics */
                    keys_update_failed++;
                    if (first_failure_reason == H5PL_VERIFY_REASON_UNKNOWN)
                        first_failure_reason = H5PL_VERIFY_REASON_UPDATE_FAILED;

                    /* Clean up and try next key */
                    EVP_MD_CTX_free(mdctx);
                    mdctx = NULL;
                    ERR_clear_error();
                    continue;
                }
            }

            /* Finalize verification */
            verify_result = EVP_DigestVerifyFinal(mdctx, signature, (size_t)footer.signature_length);

            /* Clean up context for this iteration */
            EVP_MD_CTX_free(mdctx);
            mdctx = NULL;

            if (verify_result == 1) {
                /* SUCCESS! Signature verified with this key */
                verified = true;
                break;
            }
            else if (verify_result == 0) {
                /* Signature is cryptographically invalid (hash mismatch) */
                keys_crypto_invalid++;
                if (first_failure_reason == H5PL_VERIFY_REASON_UNKNOWN)
                    first_failure_reason = H5PL_VERIFY_REASON_INVALID_SIG;
            }
            else {
                /* Internal OpenSSL error (verify_result == -1) */
                keys_crypto_error++;
                if (first_failure_reason == H5PL_VERIFY_REASON_UNKNOWN)
                    first_failure_reason = H5PL_VERIFY_REASON_CRYPTO_ERROR;
            }

            /* Clear OpenSSL errors before trying next key */
            ERR_clear_error();
        }

#undef H5PL_HASH_CHUNK_SIZE

        /* Close file now that we're done reading */
        HDclose(fd);
        fd = -1;

        /* Check if any key verified successfully */
        if (!verified) {
            /* Cache the failed verification result to avoid re-verification */
            if (H5PL__update_signature_cache(plugin_path, false) < 0) {
                /* Non-fatal: cache update failure shouldn't block error reporting */
            }

            /* Build informative error message with key sources for debugging */
            char   key_sources[1024] = "";
            size_t remaining         = sizeof(key_sources);
            char  *ptr               = key_sources;

            for (size_t i = 0; i < H5PL_keystore_count_g; i++) {
                const char *source  = H5PL_keystore_g[i].source ? H5PL_keystore_g[i].source : "unknown";
                int         written = snprintf(ptr, remaining, "%s%s", (i > 0 ? ", " : ""), source);

                if (written < 0 || (size_t)written >= remaining) {
                    /* Truncate with ellipsis if buffer is full */
                    if (remaining > 4)
                        strcpy(ptr, "...");
                    break;
                }

                ptr += written;
                remaining -= (size_t)written;
            }

            /* Build detailed diagnostic message based on failure pattern */
            const char *diagnostic = NULL;
            if (keys_init_failed == H5PL_keystore_count_g) {
                /* ALL keys failed to initialize - key type mismatch */
                diagnostic = "\n"
                             "  DIAGNOSIS: All keys failed initialization (key type mismatch)\n"
                             "  - Plugin signature algorithm may be incompatible with KeyStore keys\n"
                             "  - Verify that KeyStore contains RSA keys matching the signature algorithm\n"
                             "  - Check signature algorithm ID in plugin footer\n";
            }
            else if (keys_crypto_invalid == H5PL_keystore_count_g) {
                /* ALL keys showed cryptographic signature mismatch */
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
                /* SOME keys showed signature mismatch (not all) */
                diagnostic = "\n"
                             "  DIAGNOSIS: Signature failed with some keys (not all)\n"
                             "  - Plugin may be signed with a key not in your KeyStore\n"
                             "  - Add the correct public key to KeyStore directory\n";
            }
            else if (keys_update_failed > 0) {
                /* Hash update failed - possible I/O error */
                diagnostic = "\n"
                             "  DIAGNOSIS: Hash computation failed (I/O error)\n"
                             "  - File may be corrupted or inaccessible\n"
                             "  - Check file permissions and disk errors\n";
            }
            else if (keys_crypto_error > 0) {
                /* OpenSSL internal error */
                diagnostic = "\n"
                             "  DIAGNOSIS: OpenSSL internal error\n"
                             "  - Check OpenSSL installation and configuration\n"
                             "  - Review system logs for OpenSSL errors\n";
            }
            else {
                /* Unknown failure pattern */
                diagnostic = "\n"
                             "  DIAGNOSIS: Unknown verification failure\n"
                             "  - Enable HDF5_PLUGIN_KEYSTORE_DEBUG for detailed logging\n";
            }

            /* Get KeyStore path for error message */
            const char *keystore_path = getenv("HDF5_PLUGIN_KEYSTORE");
            if (keystore_path == NULL) {
#ifdef H5PL_KEYSTORE_DIR
                keystore_path = H5PL_KEYSTORE_DIR;
#else
                keystore_path = "(not configured - using embedded key)";
#endif
            }

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
                        "    1. Verify plugin signature: h5sign --verify %s (if h5sign tool available)\n"
                        "    2. Check KeyStore directory contains correct public keys\n"
                        "    3. Contact plugin developer for correct public key\n"
                        "    4. Verify file integrity (checksums, re-download if needed)\n",
                        plugin_path, H5PL_keystore_count_g, key_sources, keys_init_failed, keys_update_failed,
                        keys_crypto_invalid, keys_crypto_error, diagnostic ? diagnostic : "", keystore_path,
                        plugin_path);
        }
        else {
            /* Cache the successful verification result for future lookups */
            if (H5PL__update_signature_cache(plugin_path, true) < 0) {
                /* Non-fatal: cache update failure shouldn't block successful verification */
            }
        }
    }

done:
    if (fd >= 0)
        HDclose(fd);
    if (signature)
        H5MM_xfree(signature);
    if (binary_data)
        H5MM_xfree(binary_data);
    if (mdctx)
        EVP_MD_CTX_free(mdctx);
    /* Note: public_key points to a key in the keystore, so we don't free it here.
     * All keystore keys will be freed in H5PL__cleanup_signature_cache() during package termination.
     */

    /* Clear any remaining OpenSSL errors from the error queue */
    ERR_clear_error();

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5PL__verify_signature_appended() */

/*-------------------------------------------------------------------------
 * Function:    H5PL__cleanup_signature_cache
 *
 * Purpose:     Clean up keystore and signature cache
 *
 * Return:      SUCCEED/FAIL
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
                    free(H5PL_keystore_g[i].source);
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
                free(H5PL_sig_cache_g[i].path);
        }
        H5MM_xfree(H5PL_sig_cache_g);
        H5PL_sig_cache_g = NULL;
    }
    H5PL_sig_cache_count_g    = 0;
    H5PL_sig_cache_capacity_g = 0;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5PL__cleanup_signature_cache() */

#endif /* H5_REQUIRE_DIGITAL_SIGNATURE */
