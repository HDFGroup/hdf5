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
 * Purpose: Internal routines for managing plugins.
 *
 */

/****************/
/* Module Setup */
/****************/

#include "H5PLmodule.h" /* This source code file is part of the H5PL module */

/***********/
/* Headers */
/***********/
#include "H5private.h"  /* Generic Functions            */
#include "H5Eprivate.h" /* Error handling               */
#include "H5PLpkg.h"    /* Plugin                       */
#include "H5Zprivate.h" /* Filter pipeline              */

#ifdef H5_REQUIRE_DIGITAL_SIGNATURE

char *H5PL__get_sig_name_from_path(const char *path, const char *extension);
#if 0
#include <gpgme.h>  /* gpgme             */
#include <stdio.h>  /* printf            */
#include <unistd.h> /* write             */
#include <errno.h>  /* errno             */
#include <locale.h> /* locale support    */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>


herr_t H5PL__gpg_verify_signature(const char *plugin_name, const char *plugin_sig, const char *public_key);
#endif

#include <openssl/aes.h>
#include <openssl/evp.h>
#include <openssl/rsa.h>
#include <openssl/pem.h>
#include <openssl/ssl.h>
#include <openssl/bio.h>
#include <openssl/err.h>

int    RSAVerifySignature(RSA *rsa, unsigned char *MsgHash, size_t MsgHashLen, const char *Msg, size_t MsgLen,
                          int *Authentic);
RSA   *createPublicRSA(const char *key);
char  *openSSLReadFile(const char *filePath, int *fileLength);
herr_t H5PL__openssl_verify_signature(const char *plugin_name, const char *plugin_sig,
                                      const char *public_key);
int    checkIfBadFilename(char *filename);
int    RSACheckKey(RSA *key);

#endif // H5_REQUIRE_DIGITAL_SIGNATURE

/****************/
/* Local Macros */
/****************/

/******************/
/* Local Typedefs */
/******************/

/********************/
/* Local Prototypes */
/********************/

/*********************/
/* Package Variables */
/*********************/

/* Package initialization variable */
bool H5_PKG_INIT_VAR = false;

/*****************************/
/* Library Private Variables */
/*****************************/

/*******************/
/* Local Variables */
/*******************/

/* Bitmask that controls whether classes of plugins
 * (e.g.: filters, VOL drivers) can be loaded.
 */
static unsigned int H5PL_plugin_control_mask_g = H5PL_ALL_PLUGIN;

/* This flag will be set to false if the HDF5_PLUGIN_PRELOAD
 * environment variable was set to H5PL_NO_PLUGIN at
 * package initialization.
 */
static bool H5PL_allow_plugins_g = true;

/*-------------------------------------------------------------------------
 * Function:    H5PL__get_plugin_control_mask
 *
 * Purpose:     Gets the internal plugin control mask value.
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5PL__get_plugin_control_mask(unsigned int *mask /*out*/)
{
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_PACKAGE_NOERR

    /* Check args - Just assert on package functions */
    assert(mask);

    /* Return the mask */
    *mask = H5PL_plugin_control_mask_g;

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5PL__get_plugin_control_mask() */

/*-------------------------------------------------------------------------
 * Function:    H5PL__set_plugin_control_mask
 *
 * Purpose:     Sets the internal plugin control mask value.
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5PL__set_plugin_control_mask(unsigned int mask)
{
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_PACKAGE_NOERR

    /* Only allow setting this if plugins have not been disabled.
     * XXX: Note that we don't consider this an error, but instead
     *      silently ignore it. We may want to consider this behavior
     *      more carefully.
     */
    if (H5PL_allow_plugins_g)
        H5PL_plugin_control_mask_g = mask;

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5PL__set_plugin_control_mask() */

/*-------------------------------------------------------------------------
 * Function:    H5PL__init_package
 *
 * Purpose:     Initialize any package-specific data and call any init
 *              routines for the package.
 *
 * Return:      Success:        non-negative
 *              Failure:        negative
 *-------------------------------------------------------------------------
 */
herr_t
H5PL__init_package(void)
{
    char  *env_var   = NULL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE

    /* Check the environment variable to determine if the user wants
     * to ignore plugins. The special symbol H5PL_NO_PLUGIN (defined in
     * H5PLpublic.h) means we don't want to load plugins.
     */
    if (NULL != (env_var = getenv(HDF5_PLUGIN_PRELOAD)))
        if (!strcmp(env_var, H5PL_NO_PLUGIN)) {
            H5PL_plugin_control_mask_g = 0;
            H5PL_allow_plugins_g       = false;
        }

    /* Create the table of previously-loaded plugins */
    if (H5PL__create_plugin_cache() < 0)
        HGOTO_ERROR(H5E_PLUGIN, H5E_CANTINIT, FAIL, "can't create plugin cache");

    /* Create the table of search paths for dynamic libraries */
    if (H5PL__create_path_table() < 0)
        HGOTO_ERROR(H5E_PLUGIN, H5E_CANTINIT, FAIL, "can't create plugin search path table");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5PL__init_package() */

/*-------------------------------------------------------------------------
 * Function:    H5PL_term_package
 *
 * Purpose:     Terminate the H5PL interface: release all memory, reset all
 *              global variables to initial values. This only happens if all
 *              types have been destroyed from other interfaces.
 *
 * Return:      Success:    Positive if any action was taken that might
 *                          affect some other interface; zero otherwise
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
int
H5PL_term_package(void)
{
    bool already_closed = false;
    int  ret_value      = 0;

    FUNC_ENTER_NOAPI_NOINIT

    if (H5_PKG_INIT_VAR) {
        /* Close the plugin cache.
         * We need to bump the return value if we did any real work here.
         */
        if (H5PL__close_plugin_cache(&already_closed) < 0)
            HGOTO_ERROR(H5E_PLUGIN, H5E_CANTFREE, (-1), "problem closing plugin cache");
        if (!already_closed)
            ret_value++;

        /* Close the search path table and free the paths */
        if (H5PL__close_path_table() < 0)
            HGOTO_ERROR(H5E_PLUGIN, H5E_CANTFREE, (-1), "problem closing search path table");

        /* Mark the interface as uninitialized */
        if (0 == ret_value)
            H5_PKG_INIT_VAR = false;
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5PL_term_package() */

/*-------------------------------------------------------------------------
 * Function:    H5PL_load
 *
 * Purpose:     Given the plugin type and identifier, this function searches
 *              for and, if found, loads a dynamic plugin library.
 *
 *              The function searches first in the cached plugins and then
 *              in the paths listed in the path table.
 *
 * Return:      Success:    A pointer to the plugin info
 *              Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
const void *
H5PL_load(H5PL_type_t type, const H5PL_key_t *key)
{
    H5PL_search_params_t search_params;       /* Plugin search parameters     */
    bool                 found       = false; /* Whether the plugin was found */
    const void          *plugin_info = NULL;  /* Information from the plugin  */
    const void          *ret_value   = NULL;

    FUNC_ENTER_NOAPI(NULL)

    /* Check if plugins can be loaded for this plugin type */
    switch (type) {
        case H5PL_TYPE_FILTER:
            if ((H5PL_plugin_control_mask_g & H5PL_FILTER_PLUGIN) == 0)
                HGOTO_ERROR(H5E_PLUGIN, H5E_CANTLOAD, NULL, "filter plugins disabled");
            break;

        case H5PL_TYPE_VOL:
            if ((H5PL_plugin_control_mask_g & H5PL_VOL_PLUGIN) == 0)
                HGOTO_ERROR(H5E_PLUGIN, H5E_CANTLOAD, NULL,
                            "Virtual Object Layer (VOL) driver plugins disabled");
            break;

        case H5PL_TYPE_VFD:
            if ((H5PL_plugin_control_mask_g & H5PL_VFD_PLUGIN) == 0)
                HGOTO_ERROR(H5E_PLUGIN, H5E_CANTLOAD, NULL, "Virtual File Driver (VFD) plugins disabled");
            break;

        case H5PL_TYPE_ERROR:
        case H5PL_TYPE_NONE:
        default:
            HGOTO_ERROR(H5E_PLUGIN, H5E_CANTLOAD, NULL, "Invalid plugin type specified");
    }

    /* Set up the search parameters */
    search_params.type = type;
    search_params.key  = key;

    /* Search in the table of already loaded plugin libraries */
    if (H5PL__find_plugin_in_cache(&search_params, &found, &plugin_info) < 0)
        HGOTO_ERROR(H5E_PLUGIN, H5E_CANTGET, NULL, "search in plugin cache failed");

    /* If not found, try iterating through the path table to find an appropriate plugin */
    if (!found)
        if (H5PL__find_plugin_in_path_table(&search_params, &found, &plugin_info) < 0)
            HGOTO_ERROR(H5E_PLUGIN, H5E_CANTGET, NULL,
                        "can't find plugin in the paths either set by HDF5_PLUGIN_PATH, or default location, "
                        "or set by H5PLxxx functions");

    /* Set the return value we found the plugin */
    if (found)
        ret_value = plugin_info;
    else
        HGOTO_ERROR(H5E_PLUGIN, H5E_NOTFOUND, NULL,
                    "can't find plugin. Check either HDF5_VOL_CONNECTOR, HDF5_PLUGIN_PATH, default location, "
                    "or path set by H5PLxxx functions");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5PL_load() */

/*-------------------------------------------------------------------------
 * Function:    H5PL__open
 *
 * Purpose:     Opens a plugin.
 *
 *              `path` specifies the path to the plugin library file.
 *
 *              `type` specifies the type of plugin being searched for and
 *              will be used to verify that a loaded plugin matches the
 *              type requested. H5PL_TYPE_NONE may be passed, in which case
 *              no plugin type verification is performed. This is most
 *              useful when iterating over available plugins without regard
 *              to their types.
 *
 *              `key` specifies the information that will be used to find a
 *              specific plugin. For filter plugins, this is typically an
 *              integer identifier. For VOL connector and VFD plugins, this
 *              is typically either an integer identifier or a name string.
 *              After a plugin has been opened, this information will be
 *              compared against the relevant information provided by the
 *              plugin to ensure that the plugin is a match. If
 *              H5PL_TYPE_NONE is provided for `type`, then `key` should be
 *              NULL.
 *
 *              On successful open of a plugin, the `success` parameter
 *              will be set to true and the `plugin_type` and `plugin_info`
 *              parameters will be filled appropriately. On failure, the
 *              `success` parameter will be set to false, the `plugin_type`
 *              parameter will be set to H5PL_TYPE_ERROR and the
 *              `plugin_info` parameter will be set to NULL.
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
/* NOTE: We turn off -Wpedantic in gcc to quiet a warning about converting
 *       object pointers to function pointers, which is undefined in ANSI C.
 *       This is basically unavoidable due to the nature of dlsym() and *is*
 *       defined in POSIX, so it's fine.
 *
 *       This pragma only needs to surround the assignment of the
 *       get_plugin_info function pointer, but early (4.4.7, at least) gcc
 *       only allows diagnostic pragmas to be toggled outside of functions.
 */
H5_GCC_CLANG_DIAG_OFF("pedantic")
herr_t
H5PL__open(const char *path, H5PL_type_t type, const H5PL_key_t *key, bool *success, H5PL_type_t *plugin_type,
           const void **plugin_info)
{
    H5PL_HANDLE            handle          = NULL;
    H5PL_get_plugin_type_t get_plugin_type = NULL;
    H5PL_get_plugin_info_t get_plugin_info = NULL;
    H5PL_type_t            loaded_plugin_type;
    H5PL_key_t             tmp_key;
    herr_t                 ret_value = SUCCEED;

#ifdef H5_REQUIRE_DIGITAL_SIGNATURE
    char     *signature;
    char     *publickey;
    int       rank;
    herr_t    verify_result;
    const int root = 0;

    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
#endif // H5_REQUIRE_DIGITAL_SIGNATURE

    FUNC_ENTER_PACKAGE

    /* Check args - Just assert on package functions */
    assert(path);
    if (type == H5PL_TYPE_NONE)
        assert(!key);
    assert(success);
    assert(plugin_info);

    /* Initialize out parameters */
    *success     = false;
    *plugin_info = NULL;
    if (plugin_type)
        *plugin_type = H5PL_TYPE_ERROR;

#ifdef H5_REQUIRE_DIGITAL_SIGNATURE
    // printf("path: %s\n", path);
    if (rank == root) {
        signature = H5PL__get_sig_name_from_path(path, "sig");
        publickey = H5PL__get_sig_name_from_path(path, "key");
        // printf("sig: %s\n", signature);
        verify_result = H5PL__openssl_verify_signature(path, signature, publickey);
        free(signature);
        free(publickey);
    }
    MPI_Bcast(&verify_result, 1, MPI_INT, root, MPI_COMM_WORLD);
    // printf("[%d]: After Bcast, verify_result is %d\n", rank, verify_result);
    if (verify_result < 0) {
        HGOTO_ERROR(H5E_PLUGIN, H5E_CANTGET, FAIL, "verification check failed");
    }
    // MPI_Finalize();
#endif // H5_REQUIRE_DIGITAL_SIGNATURE

    /* There are different reasons why a library can't be open, e.g. wrong architecture.
     * If we can't open the library, just return.
     */
    if (NULL == (handle = H5PL_OPEN_DLIB(path))) {
        H5PL_CLR_ERROR; /* clear error */
        HGOTO_DONE(SUCCEED);
    }

    /* Return a handle for the function H5PLget_plugin_type in the dynamic library.
     * The plugin library is supposed to define this function.
     */
    if (NULL == (get_plugin_type = (H5PL_get_plugin_type_t)H5PL_GET_LIB_FUNC(handle, "H5PLget_plugin_type")))
        HGOTO_DONE(SUCCEED);

    /* Return a handle for the function H5PLget_plugin_info in the dynamic library.
     * The plugin library is supposed to define this function.
     */
    if (NULL == (get_plugin_info = (H5PL_get_plugin_info_t)H5PL_GET_LIB_FUNC(handle, "H5PLget_plugin_info")))
        HGOTO_DONE(SUCCEED);

    /* Check the plugin type and return if it doesn't match the one passed in */
    loaded_plugin_type = (H5PL_type_t)(*get_plugin_type)();
    if ((type != H5PL_TYPE_NONE) && (type != loaded_plugin_type))
        HGOTO_DONE(SUCCEED);

    /* Get the plugin information */
    switch (loaded_plugin_type) {
        case H5PL_TYPE_FILTER: {
            const H5Z_class2_t *filter_info;

            /* Get the plugin info */
            if (NULL == (filter_info = (const H5Z_class2_t *)(*get_plugin_info)()))
                HGOTO_ERROR(H5E_PLUGIN, H5E_CANTGET, FAIL, "can't get filter info from plugin");

            /* Setup temporary plugin key if one wasn't supplied */
            if (!key) {
                tmp_key.id = filter_info->id;
                key        = &tmp_key;
            }

            /* If the filter IDs match, we're done. Set the output parameters. */
            if (filter_info->id == key->id) {
                if (plugin_type)
                    *plugin_type = H5PL_TYPE_FILTER;
                *plugin_info = (const void *)filter_info;
                *success     = true;
            }

            break;
        }

        case H5PL_TYPE_VOL: {
            const void *cls;

            /* Get the plugin info */
            if (NULL == (cls = (const void *)(*get_plugin_info)()))
                HGOTO_ERROR(H5E_PLUGIN, H5E_CANTGET, FAIL, "can't get VOL connector info from plugin");

            /* Setup temporary plugin key if one wasn't supplied */
            if (!key) {
                tmp_key.vol.kind   = H5VL_GET_CONNECTOR_BY_NAME;
                tmp_key.vol.u.name = ((const H5VL_class_t *)cls)->name;
                key                = &tmp_key;
            }

            /* Ask VOL interface if this class is the one we are looking for and is compatible, etc */
            if (H5VL_check_plugin_load(cls, key, success) < 0)
                HGOTO_ERROR(H5E_PLUGIN, H5E_CANTLOAD, FAIL, "VOL connector compatibility check failed");

            /* Check for finding the correct plugin */
            if (*success) {
                if (plugin_type)
                    *plugin_type = H5PL_TYPE_VOL;
                *plugin_info = cls;
            }

            break;
        }

        case H5PL_TYPE_VFD: {
            const void *cls;

            /* Get the plugin info */
            if (NULL == (cls = (const void *)(*get_plugin_info)()))
                HGOTO_ERROR(H5E_PLUGIN, H5E_CANTGET, FAIL, "can't get VFD info from plugin");

            /* Setup temporary plugin key if one wasn't supplied */
            if (!key) {
                tmp_key.vfd.kind   = H5FD_GET_DRIVER_BY_NAME;
                tmp_key.vfd.u.name = ((const H5FD_class_t *)cls)->name;
                key                = &tmp_key;
            }

            /* Ask VFD interface if this class is the one we are looking for and is compatible, etc */
            if (H5FD_check_plugin_load(cls, key, success) < 0)
                HGOTO_ERROR(H5E_PLUGIN, H5E_CANTLOAD, FAIL, "VFD compatibility check failed");

            /* Check for finding the correct plugin */
            if (*success) {
                if (plugin_type)
                    *plugin_type = H5PL_TYPE_VFD;
                *plugin_info = cls;
            }
            break;
        }

        case H5PL_TYPE_ERROR:
        case H5PL_TYPE_NONE:
        default:
            HGOTO_ERROR(H5E_PLUGIN, H5E_CANTGET, FAIL, "Invalid plugin type specified");
    } /* end switch */

    /* If we found the correct plugin, store it in the cache */
    if (*success)
        if (H5PL__add_plugin(loaded_plugin_type, key, handle))
            HGOTO_ERROR(H5E_PLUGIN, H5E_CANTINSERT, FAIL, "unable to add new plugin to plugin cache");

done:
    if (!(*success) && handle)
        if (H5PL__close(handle) < 0)
            HDONE_ERROR(H5E_PLUGIN, H5E_CLOSEERROR, FAIL, "can't close dynamic library");

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5PL__open() */
H5_GCC_CLANG_DIAG_ON("pedantic")

/*-------------------------------------------------------------------------
 * Function:    H5PL__close
 *
 * Purpose:     Closes the handle for dynamic library
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5PL__close(H5PL_HANDLE handle)
{
    FUNC_ENTER_PACKAGE_NOERR

    H5PL_CLOSE_LIB(handle);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5PL__close() */

/*-------------------------------------------------------------------------
 * Function:    H5PL_iterate
 *
 * Purpose:     Iterates over all the available plugins and calls the
 *              specified callback function on each plugin.
 *
 * Return:      H5_ITER_CONT if all plugins are processed successfully
 *              H5_ITER_STOP if short-circuit success occurs while
 *                  processing plugins
 *              H5_ITER_ERROR if an error occurs while processing plugins
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5PL_iterate(H5PL_iterate_type_t iter_type, H5PL_iterate_t iter_op, void *op_data)
{
    herr_t ret_value = H5_ITER_CONT;

    FUNC_ENTER_NOAPI(H5_ITER_ERROR)

    ret_value = H5PL__path_table_iterate(iter_type, iter_op, op_data);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5PL_iterate() */

#ifdef H5_REQUIRE_DIGITAL_SIGNATURE

/*-------------------------------------------------------------------------
 * Function:    H5PL__get_sig_name_from_path
 *
 * Purpose:  Find signature file using path
 *
 * Return:      Success:    Signature file with path
 *              Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */

char *
H5PL__get_sig_name_from_path(const char *path, const char *extension)
{
    char *sig_name;
    char *tmp;
    int   len;

    len      = strlen(path) + strlen(extension);
    sig_name = calloc(len + 1, sizeof(char));
    strcpy(sig_name, path);

    tmp = strrchr(sig_name, '.');
    strcpy(tmp + sizeof(char), extension);
    return sig_name;
} /* end H5PL__get_sig_name_from_path */

/* Is there any additional processing necessary? */
int
RSACheckKey(RSA *key)
{
    if (key == NULL) {
        return 0;
    }
    return 1;
}

RSA *
createPublicRSA(const char *key)
{
    RSA *rsa = NULL;
    BIO *keybio;
    keybio = BIO_new_mem_buf((void *)key, -1);
    if (keybio == NULL) {
        return 0;
    }
    rsa = PEM_read_bio_RSA_PUBKEY(keybio, &rsa, NULL, NULL);
    if (RSACheckKey(rsa) != 1) {
        return 0;
    }
    return rsa;
}

int
RSAVerifySignature(RSA *rsa, unsigned char *MsgHash, size_t MsgHashLen, const char *Msg, size_t MsgLen,
                   int *Authentic)
{
    *Authentic       = 0;
    EVP_PKEY *pubKey = EVP_PKEY_new();
    EVP_PKEY_assign_RSA(pubKey, rsa);
    EVP_MD_CTX *m_RSAVerifyCtx = EVP_MD_CTX_create();

    if (EVP_DigestVerifyInit(m_RSAVerifyCtx, NULL, EVP_sha256(), NULL, pubKey) <= 0) {
        return 0;
    }
    if (EVP_DigestVerifyUpdate(m_RSAVerifyCtx, Msg, MsgLen) <= 0) {
        return 0;
    }
    int AuthStatus = EVP_DigestVerifyFinal(m_RSAVerifyCtx, MsgHash, MsgHashLen);

    if (AuthStatus == 1) {
        *Authentic = 1;
        EVP_MD_CTX_free(m_RSAVerifyCtx);
        return 1;
    }
    else if (AuthStatus == 0) {
        *Authentic = 0;
        EVP_MD_CTX_free(m_RSAVerifyCtx);
        return 1;
    }
    else {
        *Authentic = 0;
        EVP_MD_CTX_free(m_RSAVerifyCtx);
        return 0;
    }
}

char *
openSSLReadFile(const char *filePath, int *fileLength)
{
    char *buffer;
    FILE *fd = fopen(filePath, "rb");
    if (fd == NULL) {
        return NULL;
    }
    fseek(fd, 0, SEEK_END);  // seek to end of file
    *fileLength = ftell(fd); // get current file pointer
    fseek(fd, 0, SEEK_SET);  // seek back to beginning of file
    buffer = malloc(*fileLength * sizeof(char));
    fread(buffer, sizeof(char), *fileLength, fd);
    fclose(fd);
    return buffer;
}

int
checkIfBadFilename(char *filename)
{
    size_t len = strlen(filename);

    if (filename == NULL || filename[0] == '\0') {
        return 1;
    }
    if ((len == 1 && filename[0] == '.') || (len == 2 && filename[0] == '.' && filename[1] == '.')) {
        return 1;
    }
    if (len > 255) {
        return 1;
    }
    for (size_t i = 0; i < len; ++i) {
        unsigned char c = filename[i];
        if (c == '/' || c == '\0') {
            return 1; // Slash or null byte are not allowed
        }
        if (iscntrl(c)) {
            return 1; // Control characters
        }
    }
    return 0;
}

/*-------------------------------------------------------------------------
 * Function:    H5PL__openssl_verify_signature
 *
 * Purpose:
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */

herr_t
H5PL__openssl_verify_signature(const char *plugin_name, const char *plugin_sig, const char *public_key)
{

    char  *publicKey;
    int    keyLen;
    char  *sig;
    int    sigLen;
    char  *data;
    int    dataLen;
    int    authentic;
    size_t maxPathLen;
    char  *copied_file_name;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE

    maxPathLen = 4095;
    publicKey  = openSSLReadFile(public_key, &keyLen);
    if (publicKey == NULL) {
        HGOTO_ERROR(H5E_PLUGIN, H5E_CANTINIT, FAIL, "bad public key\n");
    }

    copied_file_name = H5PL__get_sig_name_from_path(plugin_sig, "copy");
    char sig_file_name[4095];
    snprintf(sig_file_name, maxPathLen, "%s.sig", copied_file_name);

    char copy_elf_file[4095];
    char dump_sig[4095];
    char remove_sig[4095];

    snprintf(copy_elf_file, maxPathLen, "cp %s %s", plugin_name, copied_file_name);

    if (checkIfBadFilename(copy_elf_file)) {
        HGOTO_ERROR(H5E_PLUGIN, H5E_CANTINIT, FAIL, "bad filename\n");
    }

    snprintf(dump_sig, maxPathLen, "objcopy %s --dump-section sig=%s", copied_file_name, sig_file_name);
    snprintf(remove_sig, maxPathLen, "objcopy %s --remove-section=sig", copied_file_name);
    system(copy_elf_file);
    system(dump_sig);
    system(remove_sig);

    sig = openSSLReadFile(sig_file_name, &sigLen);
    if (sig == NULL) {
        HGOTO_ERROR(H5E_PLUGIN, H5E_CANTINIT, FAIL, "bad signature\n");
    }
    data = openSSLReadFile(copied_file_name, &dataLen);
    if (data == NULL) {
        HGOTO_ERROR(H5E_PLUGIN, H5E_CANTINIT, FAIL, "bad data\n");
    }

    char delete_so[4095];
    char delete_sig[4095];
    snprintf(delete_so, maxPathLen, "rm %s", copied_file_name);
    snprintf(delete_sig, maxPathLen, "rm %s", sig_file_name);
    system(delete_so);
    system(delete_sig);

    RSA *publicRSA = createPublicRSA(publicKey);
    int  result    = RSAVerifySignature(publicRSA, sig, sigLen, data, dataLen, &authentic);

    free(copied_file_name);
    free(publicKey);
    free(sig);
    free(data);

    if (authentic != 1) {
        HGOTO_ERROR(H5E_PLUGIN, H5E_CANTINIT, FAIL, "plugin not verified. It does not match\n");
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5PL__openssl_verify_signature */

#endif // H5_REQUIRE_DIGITAL_SIGNATURE
