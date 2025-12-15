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
    herr_t    verify_result;

#ifdef H5_HAVE_PARALLEL
    int       rank;
    const int root = 0;

    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
#endif // H5_HAVE_PARALLEL
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
#ifdef H5_HAVE_PARALLEL    
    if (rank == root) {
#endif // H5_HAVE_PARALLEL  
    signature = H5PL__get_sig_name_from_path(path, "sig");
    publickey = H5PL__get_sig_name_from_path(path, "key");
    verify_result = H5PL__openssl_verify_signature(path, signature, publickey);
    free(signature);
    free(publickey);
#ifdef H5_HAVE_PARALLEL
    }
    MPI_Bcast(&verify_result, 1, MPI_INT, root, MPI_COMM_WORLD);
#endif // H5_HAVE_PARALLEL  
    // printf("[%d]: After Bcast, verify_result is %d\n", rank, verify_result);
    if (verify_result < 0) {
        HGOTO_ERROR(H5E_PLUGIN, H5E_CANTGET, FAIL, "verification check failed");
    }
#ifdef H5_HAVE_PARALLEL
    MPI_Finalize();
#endif // H5_HAVE_PARALLEL
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
 * Purpose:     Find signature file using path
 *
 * Return:      Success:    Signature file with path
 *              Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */

char *
H5PL__get_sig_name_from_path(const char *path, const char *extension)
{
    char  *sig_name   = NULL; /* Signature filename with new extension */
    char  *temp        = NULL; /* Pointer to last '.' in path */
    size_t len;               /* Length of new filename */
    char  *ret_value  = NULL; /* Return value */

    FUNC_ENTER_PACKAGE

    /* Check args */
    assert(path);
    assert(extension);

    /* Calculate length needed: path + extension (without original extension) */
    len = strlen(path) + strlen(extension);

    /* Allocate memory for new filename */
    if (NULL == (sig_name = (char *)H5MM_calloc(len + 1)))
        HGOTO_ERROR(H5E_PLUGIN, H5E_CANTALLOC, NULL, "can't allocate space for signature filename");

    /* Copy path to new string */
    strcpy(sig_name, path);

    /* Find last occurrence of '.' to replace extension */
    if (NULL == (temp = strrchr(sig_name, '.')))
        HGOTO_ERROR(H5E_PLUGIN, H5E_BADVALUE, NULL, "no extension found in path");

    /* Replace extension (skip the '.') */
    strcpy(temp + 1, extension);

    /* Set return value */
    ret_value = sig_name;

done:
    if (NULL == ret_value)
        sig_name = (char *)H5MM_xfree(sig_name);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5PL__get_sig_name_from_path */

/*-------------------------------------------------------------------------
 * Function:    H5PL__RSA_check_key
 *
 * Purpose:     Validate that an RSA key is not NULL
 *
 * Return:      1 if key is valid
 *              0 if key is NULL
 *
 *-------------------------------------------------------------------------
 */
int
H5PL__RSA_check_key(RSA *key)
{
    int ret_value = 1; /* Return value */

    FUNC_ENTER_PACKAGE_NOERR

    /* Check if key is NULL */
    if (NULL == key)
        ret_value = 0;

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5PL__RSA_check_key() */

/*-------------------------------------------------------------------------
 * Function:    H5PL__create_public_RSA
 *
 * Purpose:     Create an RSA public key structure from a PEM-formatted key string
 *
 * Return:      Success:    Pointer to RSA public key structure
 *              Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
RSA *
H5PL__create_public_RSA(const char *key)
{
    RSA *rsa       = NULL; /* RSA public key structure */
    BIO *key_bio    = NULL; /* BIO memory buffer for key */
    RSA *ret_value = NULL; /* Return value */

    FUNC_ENTER_PACKAGE

    /* Check args */
    assert(key);

    /* Create a BIO memory buffer from the key string */
    if (NULL == (key_bio = BIO_new_mem_buf((void *)key, -1)))
        HGOTO_ERROR(H5E_PLUGIN, H5E_CANTCREATE, NULL, "can't create BIO memory buffer");

    /* Read the public key from the BIO buffer */
    if (NULL == (rsa = PEM_read_bio_RSA_PUBKEY(key_bio, &rsa, NULL, NULL)))
        HGOTO_ERROR(H5E_PLUGIN, H5E_CANTGET, NULL, "can't read RSA public key from buffer");

    /* Validate the RSA key */
    if (1 != H5PL__RSA_check_key(rsa))
        HGOTO_ERROR(H5E_PLUGIN, H5E_BADVALUE, NULL, "RSA key validation failed");

    /* Set return value */
    ret_value = rsa;

done:
    /* Clean up on error */
    if (NULL == ret_value && NULL != rsa) {
        RSA_free(rsa);
        rsa = NULL;
    }
    if (NULL != key_bio)
        BIO_free(key_bio);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5PL__create_public_RSA() */

/*-------------------------------------------------------------------------
 * Function:    H5PL__RSA_verify_signature
 *
 * Purpose:     Verify an RSA signature using SHA-256 digest
 *
 * Return:      1 if verification completed (check authentic for result)
 *              0 if verification process failed
 *
 *-------------------------------------------------------------------------
 */
int
H5PL__RSA_verify_signature(RSA *rsa, unsigned char *msg_hash, size_t msg_hash_len, const char *msg, size_t msg_len,
                           int *authentic)
{
    EVP_PKEY   *pub_key      = NULL; /* EVP public key structure */
    EVP_MD_CTX *verify_ctx   = NULL; /* Message digest context for verification */
    int         auth_status;         /* Authentication status from OpenSSL */
    int         ret_value = 1;       /* Return value */

    FUNC_ENTER_PACKAGE

    /* Check args */
    assert(rsa);
    assert(msg_hash);
    assert(msg);
    assert(authentic);

    /* Initialize output parameter */
    *authentic = 0;

    /* Create EVP_PKEY structure and assign RSA key to it */
    if (NULL == (pub_key = EVP_PKEY_new()))
        HGOTO_ERROR(H5E_PLUGIN, H5E_CANTCREATE, 0, "can't create EVP_PKEY structure");
    EVP_PKEY_assign_RSA(pub_key, rsa);

    /* Create message digest context */
    if (NULL == (verify_ctx = EVP_MD_CTX_create()))
        HGOTO_ERROR(H5E_PLUGIN, H5E_CANTCREATE, 0, "can't create message digest context");

    /* Initialize digest verification with SHA-256 */
    if (EVP_DigestVerifyInit(verify_ctx, NULL, EVP_sha256(), NULL, pub_key) <= 0)
        HGOTO_ERROR(H5E_PLUGIN, H5E_CANTINIT, 0, "can't initialize digest verification");

    /* Update digest with message data */
    if (EVP_DigestVerifyUpdate(verify_ctx, msg, msg_len) <= 0)
        HGOTO_ERROR(H5E_PLUGIN, H5E_CANTGET, 0, "can't update digest with message data");

    /* Finalize verification and check signature */
    auth_status = EVP_DigestVerifyFinal(verify_ctx, msg_hash, msg_hash_len);

    if (1 == auth_status)
        *authentic = 1;
    else if (0 == auth_status)
        *authentic = 0;
    else
        HGOTO_ERROR(H5E_PLUGIN, H5E_CANTGET, 0, "digest verification final failed");

done:
    /* Clean up OpenSSL resources */
    if (NULL != verify_ctx)
        EVP_MD_CTX_free(verify_ctx);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5PL__RSA_verify_signature() */

/*-------------------------------------------------------------------------
 * Function:    H5PL__openSSL_read_file
 *
 * Purpose:     Read entire file into memory buffer
 *
 * Return:      Success:    Pointer to buffer containing file contents
 *              Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
char *
H5PL__openSSL_read_file(const char *file_path, int *file_length)
{
    char  *buffer    = NULL; /* Buffer to hold file contents */
    FILE  *fd        = NULL; /* File descriptor */
    long   file_size;        /* Size of file in bytes */
    size_t bytes_read;       /* Number of bytes read */
    char  *ret_value = NULL; /* Return value */

    FUNC_ENTER_PACKAGE

    /* Check args */
    assert(file_path);
    assert(file_length);

    /* Initialize output parameter */
    *file_length = 0;

    /* Open file for reading in binary mode */
    if (NULL == (fd = fopen(file_path, "rb")))
        HGOTO_ERROR(H5E_PLUGIN, H5E_CANTOPENFILE, NULL, "can't open file for reading");

    /* Seek to end of file to determine size */
    if (fseek(fd, 0, SEEK_END) < 0)
        HGOTO_ERROR(H5E_PLUGIN, H5E_CANTGET, NULL, "can't seek to end of file");

    /* Get current file position (file size) */
    if ((file_size = ftell(fd)) < 0)
        HGOTO_ERROR(H5E_PLUGIN, H5E_CANTGET, NULL, "can't get file size");

    /* Seek back to beginning of file */
    if (fseek(fd, 0, SEEK_SET) < 0)
        HGOTO_ERROR(H5E_PLUGIN, H5E_CANTGET, NULL, "can't seek to beginning of file");

    /* Allocate buffer for file contents */
    if (NULL == (buffer = (char *)H5MM_malloc((size_t)file_size)))
        HGOTO_ERROR(H5E_PLUGIN, H5E_CANTALLOC, NULL, "can't allocate buffer for file contents");

    /* Read file contents into buffer */
    if ((bytes_read = fread(buffer, 1, (size_t)file_size, fd)) != (size_t)file_size)
        HGOTO_ERROR(H5E_PLUGIN, H5E_READERROR, NULL, "can't read file contents");

    /* Set output parameters */
    *file_length = (int)file_size;
    ret_value   = buffer;

done:
    /* Clean up on error */
    if (NULL == ret_value && NULL != buffer)
        buffer = (char *)H5MM_xfree(buffer);

    /* Close file if open */
    if (NULL != fd)
        fclose(fd);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5PL__openSSL_read_file() */

/*-------------------------------------------------------------------------
 * Function:    H5PL__check_filename
 *
 * Purpose:     Validate filename for security issues
 *
 *              Checks for:
 *              - NULL or empty filenames
 *              - Directory traversal attempts (. and ..)
 *              - Filenames exceeding 255 characters
 *              - Path separators (/) or null bytes
 *              - Control characters
 *
 * Return:      1 if filename is invalid
 *              0 if filename is valid
 *
 *-------------------------------------------------------------------------
 */
int
H5PL__check_filename(char *filename)
{
    size_t len;              /* Length of filename */
    size_t i;                /* Loop counter */
    int    ret_value = 0;    /* Return value */

    FUNC_ENTER_PACKAGE_NOERR

    /* Check for NULL or empty filename */
    if (NULL == filename || '\0' == filename[0]) {
        ret_value = 1;
        goto done;
    }

    /* Get length of filename */
    len = strlen(filename);

    /* Check for directory traversal attempts (. or ..) */
    if ((1 == len && '.' == filename[0]) ||
        (2 == len && '.' == filename[0] && '.' == filename[1])) {
        ret_value = 1;
        goto done;
    }

    /* Check for filename length exceeding maximum (255 characters) */
    if (len > 255) {
        ret_value = 1;
        goto done;
    }

    /* Check each character for invalid content */
    for (i = 0; i < len; i++) {
        unsigned char c = (unsigned char)filename[i];

        /* Check for path separator or null byte */
        if ('/' == c || '\0' == c) {
            ret_value = 1;
            goto done;
        }

        /* Check for control characters */
        if (iscntrl((int)c)) {
            ret_value = 1;
            goto done;
        }
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5PL__check_filename() */

/*-------------------------------------------------------------------------
 * Function:    H5PL__openssl_verify_signature
 *
 * Purpose:     Verify digital signature of a plugin using OpenSSL
 *
 *              Extracts the signature from the plugin binary, verifies it
 *              against the plugin contents using the provided public key.
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5PL__openssl_verify_signature(const char *plugin_name, const char *plugin_sig, const char *public_key)
{
    char   *publicKey        = NULL; /* Public key data */
    int     keyLen;                  /* Length of public key */
    char   *sig              = NULL; /* Signature data */
    int     sigLen;                  /* Length of signature */
    char   *data             = NULL; /* Plugin binary data */
    int     dataLen;                 /* Length of plugin data */
    int     authentic;               /* Authentication result */
    size_t  maxPathLen;              /* Maximum path length */
    char   *copied_file_name = NULL; /* Temporary copy filename */
    char    sig_file_name[4096];     /* Signature file path */
    char    copy_elf_file[4096];     /* Command to copy plugin */
    char    dump_sig[4096];          /* Command to dump signature */
    char    remove_sig[4096];        /* Command to remove signature */
    char    delete_so[4096];         /* Command to delete temporary plugin copy */
    char    delete_sig[4096];        /* Command to delete temporary signature file */
    RSA    *publicRSA        = NULL; /* RSA public key structure */
    int     result;                  /* Result from signature verification */
    herr_t  ret_value        = SUCCEED; /* Return value */

    FUNC_ENTER_PACKAGE

    /* Check args */
    assert(plugin_name);
    assert(plugin_sig);
    assert(public_key);

    /* Set maximum path length */
    maxPathLen = 4095;

    /* Read public key from file */
    if (NULL == (publicKey = H5PL__openSSL_read_file(public_key, &keyLen)))
        HGOTO_ERROR(H5E_PLUGIN, H5E_CANTOPENFILE, FAIL, "can't read public key file");

    /* Generate temporary filename for plugin copy */
    if (NULL == (copied_file_name = H5PL__get_sig_name_from_path(plugin_sig, "copy")))
        HGOTO_ERROR(H5E_PLUGIN, H5E_CANTGET, FAIL, "can't generate temporary filename");

    /* Construct path for extracted signature file */
    snprintf(sig_file_name, maxPathLen, "%s.sig", copied_file_name);

    /* Construct shell commands for signature extraction */
    snprintf(copy_elf_file, maxPathLen, "cp %s %s", plugin_name, copied_file_name);

    /* Validate constructed command for security */
    if (H5PL__check_filename(copy_elf_file))
        HGOTO_ERROR(H5E_PLUGIN, H5E_BADVALUE, FAIL, "invalid filename in command");

    /* Build commands to extract and remove signature section */
    snprintf(dump_sig, maxPathLen, "objcopy %s --dump-section sig=%s", copied_file_name, sig_file_name);
    snprintf(remove_sig, maxPathLen, "objcopy %s --remove-section=sig", copied_file_name);

    /* Execute commands to extract signature from plugin binary */
    system(copy_elf_file);
    system(dump_sig);
    system(remove_sig);

    /* Read extracted signature */
    if (NULL == (sig = H5PL__openSSL_read_file(sig_file_name, &sigLen)))
        HGOTO_ERROR(H5E_PLUGIN, H5E_CANTOPENFILE, FAIL, "can't read signature file");

    /* Read plugin binary data (with signature removed) */
    if (NULL == (data = H5PL__openSSL_read_file(copied_file_name, &dataLen)))
        HGOTO_ERROR(H5E_PLUGIN, H5E_CANTOPENFILE, FAIL, "can't read plugin data file");

    /* Clean up temporary files */
    snprintf(delete_so, maxPathLen, "rm %s", copied_file_name);
    snprintf(delete_sig, maxPathLen, "rm %s", sig_file_name);
    system(delete_so);
    system(delete_sig);

    /* Create RSA public key structure from key data */
    if (NULL == (publicRSA = H5PL__create_public_RSA(publicKey)))
        HGOTO_ERROR(H5E_PLUGIN, H5E_CANTCREATE, FAIL, "can't create RSA public key structure");

    /* Verify signature */
    result = H5PL__RSA_verify_signature(publicRSA, (unsigned char *)sig, (size_t)sigLen, data, (size_t)dataLen,
                                &authentic);

    /* Check verification result */
    if (1 != authentic)
        HGOTO_ERROR(H5E_PLUGIN, H5E_BADVALUE, FAIL, "plugin signature verification failed");

done:
    /* Clean up allocated resources */
    if (NULL != copied_file_name)
        copied_file_name = (char *)H5MM_xfree(copied_file_name);
    if (NULL != publicKey)
        publicKey = (char *)H5MM_xfree(publicKey);
    if (NULL != sig)
        sig = (char *)H5MM_xfree(sig);
    if (NULL != data)
        data = (char *)H5MM_xfree(data);
    if (NULL != publicRSA)
        RSA_free(publicRSA);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5PL__openssl_verify_signature */

#endif // H5_REQUIRE_DIGITAL_SIGNATURE
