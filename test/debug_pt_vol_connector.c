/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Purpose:     This is a "pass through" VOL connector, which forwards each
 *              VOL callback to an underlying connector.
 *
 *              It is designed as a test for passthrough behavior. It
 *              passes control to the Native connector after appending
 *              a suffix to any filename, for the purpose of verifying
 *              that a given application uses the VOL layer correctly.
 *
 */

/* Header files needed */
/* Do NOT include private HDF5 files here! */
#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Public HDF5 file */
#include "hdf5.h"

/* This connector's headers */
#include "debug_pt_vol_connector.h"

/**********/
/* Macros */
/**********/

/* Whether to display log message when callback is invoked */
/* (Uncomment to enable) */
/* #define ENABLE_PASSTHRU_LOGGING */

/* Hack for missing va_copy() in old Visual Studio editions
 * (from H5win2_defs.h - used on VS2012 and earlier)
 */
#if defined(_WIN32) && defined(_MSC_VER) && (_MSC_VER < 1800)
#define va_copy(D, S) ((D) = (S))
#endif

#define PT_SUFFIX "_DEBUG_PT"
#define PT_SUFFIX_LEN (strlen(PT_SUFFIX))

/************/
/* Typedefs */
/************/

/* The pass through VOL info object */
typedef struct debug_pt_object_t {
    hid_t under_vol_id; /* ID for underlying VOL connector */
    void *under_object; /* Info object for underlying VOL connector */
} debug_pt_object_t;

/* The MT pass through VOL wrapping context */
typedef struct debug_pt_wrap_ctx_t {
    hid_t under_vol_id;   /* VOL ID for under VOL */
    void *under_wrap_ctx; /* Object wrapping context for under VOL */
} debug_pt_wrap_ctx_t;

/********************* */
/* Function prototypes */
/********************* */

/* Dynamic plugin routines */
H5PL_type_t H5PLget_plugin_type(void);
const void *H5PLget_plugin_info(void);

/* Helper routines */
static debug_pt_object_t *debug__pt_new_obj(void *under_obj, hid_t under_vol_id);
static herr_t debug__pt_free_obj(debug_pt_object_t *obj);

/* VOL info callbacks */
static void  *debug_pt_info_copy(const void *info);
static herr_t debug_pt_info_cmp(int *cmp_value, const void *info1, const void *info2);
static herr_t debug_pt_info_free(void *info);
static herr_t debug_pt_info_to_str(const void *info, char **str);
static herr_t debug_pt_str_to_info(const char *str, void **info);

/* VOL object wrap / retrieval callbacks */
static void  *debug_pt_get_object(const void *obj);
static herr_t debug_pt_get_wrap_ctx(const void *obj, void **wrap_ctx);
static void  *debug_pt_wrap_object(void *obj, H5I_type_t obj_type, void *wrap_ctx);
static void  *debug_pt_unwrap_object(void *obj);
static herr_t debug_pt_free_wrap_ctx(void *obj);

/* Attribute callbacks */
static void  *debug_pt_attr_create(void *obj, const H5VL_loc_params_t *loc_params, const char *name,
                                            hid_t type_id, hid_t space_id, hid_t acpl_id, hid_t aapl_id,
                                            hid_t dxpl_id, void **req);
static void  *debug_pt_attr_open(void *obj, const H5VL_loc_params_t *loc_params, const char *name,
                                          hid_t aapl_id, hid_t dxpl_id, void **req);
static herr_t debug_pt_attr_read(void *attr, hid_t mem_type_id, void *buf, hid_t dxpl_id,
                                          void **req);
static herr_t debug_pt_attr_write(void *attr, hid_t mem_type_id, const void *buf, hid_t dxpl_id,
                                           void **req);
static herr_t debug_pt_attr_get(void *obj, H5VL_attr_get_args_t *args, hid_t dxpl_id, void **req);
static herr_t debug_pt_attr_specific(void *obj, const H5VL_loc_params_t *loc_params,
                                              H5VL_attr_specific_args_t *args, hid_t dxpl_id, void **req);
static herr_t debug_pt_attr_optional(void *obj, H5VL_optional_args_t *args, hid_t dxpl_id,
                                              void **req);
static herr_t debug_pt_attr_close(void *attr, hid_t dxpl_id, void **req);

/* Dataset callbacks */
static void  *debug_pt_dataset_create(void *obj, const H5VL_loc_params_t *loc_params,
                                               const char *name, hid_t lcpl_id, hid_t type_id, hid_t space_id,
                                               hid_t dcpl_id, hid_t dapl_id, hid_t dxpl_id, void **req);
static void  *debug_pt_dataset_open(void *obj, const H5VL_loc_params_t *loc_params, const char *name,
                                             hid_t dapl_id, hid_t dxpl_id, void **req);
static herr_t debug_pt_dataset_read(size_t count, void *dset[], hid_t mem_type_id[],
                                             hid_t mem_space_id[], hid_t file_space_id[], hid_t plist_id,
                                             void *buf[], void **req);
static herr_t debug_pt_dataset_write(size_t count, void *dset[], hid_t mem_type_id[],
                                              hid_t mem_space_id[], hid_t file_space_id[], hid_t plist_id,
                                              const void *buf[], void **req);
static herr_t debug_pt_dataset_get(void *dset, H5VL_dataset_get_args_t *args, hid_t dxpl_id,
                                            void **req);
static herr_t debug_pt_dataset_specific(void *obj, H5VL_dataset_specific_args_t *args, hid_t dxpl_id,
                                                 void **req);
static herr_t debug_pt_dataset_optional(void *obj, H5VL_optional_args_t *args, hid_t dxpl_id,
                                                 void **req);
static herr_t debug_pt_dataset_close(void *dset, hid_t dxpl_id, void **req);

/* Datatype callbacks */
static void *debug_pt_datatype_commit(void *obj, const H5VL_loc_params_t *loc_params,
                                               const char *name, hid_t type_id, hid_t lcpl_id, hid_t tcpl_id,
                                               hid_t tapl_id, hid_t dxpl_id, void **req);
static void *debug_pt_datatype_open(void *obj, const H5VL_loc_params_t *loc_params, const char *name,
                                             hid_t tapl_id, hid_t dxpl_id, void **req);
static herr_t debug_pt_datatype_get(void *dt, H5VL_datatype_get_args_t *args, hid_t dxpl_id,
                                             void **req);
static herr_t debug_pt_datatype_specific(void *obj, H5VL_datatype_specific_args_t *args,
                                                  hid_t dxpl_id, void **req);
static herr_t debug_pt_datatype_optional(void *obj, H5VL_optional_args_t *args, hid_t dxpl_id,
                                                  void **req);
static herr_t debug_pt_datatype_close(void *dt, hid_t dxpl_id, void **req);

/* File callbacks */
static void  *debug_pt_file_create(const char *name, unsigned flags, hid_t fcpl_id, hid_t fapl_id,
                                            hid_t dxpl_id, void **req);
static void  *debug_pt_file_open(const char *name, unsigned flags, hid_t fapl_id, hid_t dxpl_id,
                                          void **req);
static herr_t debug_pt_file_get(void *file, H5VL_file_get_args_t *args, hid_t dxpl_id, void **req);
static herr_t debug_pt_file_specific(void *file, H5VL_file_specific_args_t *args, hid_t dxpl_id,
                                              void **req);
static herr_t debug_pt_file_optional(void *file, H5VL_optional_args_t *args, hid_t dxpl_id,
                                              void **req);
static herr_t debug_pt_file_close(void *file, hid_t dxpl_id, void **req);

/* Group callbacks */
static void  *debug_pt_group_create(void *obj, const H5VL_loc_params_t *loc_params, const char *name,
                                             hid_t lcpl_id, hid_t gcpl_id, hid_t gapl_id, hid_t dxpl_id,
                                             void **req);
static void  *debug_pt_group_open(void *obj, const H5VL_loc_params_t *loc_params, const char *name,
                                           hid_t gapl_id, hid_t dxpl_id, void **req);
static herr_t debug_pt_group_get(void *obj, H5VL_group_get_args_t *args, hid_t dxpl_id, void **req);
static herr_t debug_pt_group_specific(void *obj, H5VL_group_specific_args_t *args, hid_t dxpl_id,
                                               void **req);
static herr_t debug_pt_group_optional(void *obj, H5VL_optional_args_t *args, hid_t dxpl_id,
                                               void **req);
static herr_t debug_pt_group_close(void *grp, hid_t dxpl_id, void **req);

/* Link callbacks */
static herr_t debug_pt_link_create(H5VL_link_create_args_t *args, void *obj,
                                            const H5VL_loc_params_t *loc_params, hid_t lcpl_id, hid_t lapl_id,
                                            hid_t dxpl_id, void **req);
static herr_t debug_pt_link_copy(void *src_obj, const H5VL_loc_params_t *loc_params1, void *dst_obj,
                                          const H5VL_loc_params_t *loc_params2, hid_t lcpl_id, hid_t lapl_id,
                                          hid_t dxpl_id, void **req);
static herr_t debug_pt_link_move(void *src_obj, const H5VL_loc_params_t *loc_params1, void *dst_obj,
                                          const H5VL_loc_params_t *loc_params2, hid_t lcpl_id, hid_t lapl_id,
                                          hid_t dxpl_id, void **req);
static herr_t debug_pt_link_get(void *obj, const H5VL_loc_params_t *loc_params,
                                         H5VL_link_get_args_t *args, hid_t dxpl_id, void **req);
static herr_t debug_pt_link_specific(void *obj, const H5VL_loc_params_t *loc_params,
                                              H5VL_link_specific_args_t *args, hid_t dxpl_id, void **req);
static herr_t debug_pt_link_optional(void *obj, const H5VL_loc_params_t *loc_params,
                                              H5VL_optional_args_t *args, hid_t dxpl_id, void **req);

/* Object callbacks */
static void  *debug_pt_object_open(void *obj, const H5VL_loc_params_t *loc_params,
                                            H5I_type_t *opened_type, hid_t dxpl_id, void **req);
static herr_t debug_pt_object_copy(void *src_obj, const H5VL_loc_params_t *src_loc_params,
                                            const char *src_name, void *dst_obj,
                                            const H5VL_loc_params_t *dst_loc_params, const char *dst_name,
                                            hid_t ocpypl_id, hid_t lcpl_id, hid_t dxpl_id, void **req);
static herr_t debug_pt_object_get(void *obj, const H5VL_loc_params_t *loc_params,
                                           H5VL_object_get_args_t *args, hid_t dxpl_id, void **req);
static herr_t debug_pt_object_specific(void *obj, const H5VL_loc_params_t *loc_params,
                                                H5VL_object_specific_args_t *args, hid_t dxpl_id, void **req);
static herr_t debug_pt_object_optional(void *obj, const H5VL_loc_params_t *loc_params,
                                                H5VL_optional_args_t *args, hid_t dxpl_id, void **req);

/* Container/connector introspection callbacks */
static herr_t debug_pt_introspect_get_conn_cls(void *obj, H5VL_get_conn_lvl_t lvl,
                                                        const H5VL_class_t **conn_cls);
static herr_t debug_pt_introspect_get_cap_flags(const void *info, uint64_t *cap_flags);
static herr_t debug_pt_introspect_opt_query(void *obj, H5VL_subclass_t cls, int opt_type,
                                                     uint64_t *flags);

/* Async request callbacks */
static herr_t debug_pt_request_wait(void *req, uint64_t timeout, H5VL_request_status_t *status);
static herr_t debug_pt_request_notify(void *obj, H5VL_request_notify_t cb, void *ctx);
static herr_t debug_pt_request_cancel(void *req, H5VL_request_status_t *status);
static herr_t debug_pt_request_specific(void *req, H5VL_request_specific_args_t *args);
static herr_t debug_pt_request_optional(void *req, H5VL_optional_args_t *args);
static herr_t debug_pt_request_free(void *req);

/* Blob callbacks */
static herr_t debug_pt_blob_put(void *obj, const void *buf, size_t size, void *blob_id, void *ctx);
static herr_t debug_pt_blob_get(void *obj, const void *blob_id, void *buf, size_t size, void *ctx);
static herr_t debug_pt_blob_specific(void *obj, void *blob_id, H5VL_blob_specific_args_t *args);
static herr_t debug_pt_blob_optional(void *obj, void *blob_id, H5VL_optional_args_t *args);

/* Token callbacks */
static herr_t debug_pt_token_cmp(void *obj, const H5O_token_t *token1, const H5O_token_t *token2,
                                          int *cmp_value);
static herr_t debug_pt_token_to_str(void *obj, H5I_type_t obj_type, const H5O_token_t *token,
                                             char **token_str);
static herr_t debug_pt_token_from_str(void *obj, H5I_type_t obj_type, const char *token_str,
                                               H5O_token_t *token);

/* Generic optional callback */
static herr_t debug_pt_optional(void *obj, H5VL_optional_args_t *args, hid_t dxpl_id, void **req);

static herr_t get_link_type(debug_pt_object_t *o, const H5VL_loc_params_t *loc_params, hid_t dxpl_id, void **req, H5L_type_t *type);

/*******************/
/* Local variables */
/*******************/

/* Pass through VOL connector class struct */
static const H5VL_class_t debug_pt_g = {
    H5VL_VERSION,                            /* VOL class struct version */
    DEBUG_PT_VALUE, /* value        */
    DEBUG_PT_NAME,                      /* name         */
    0,                   /* connector version */
    H5VL_CAP_FLAG_THREADSAFE,  /* capability flags */
    NULL,                  /* initialize   */
    NULL,                  /* terminate    */
    {
        /* info_cls */
        sizeof(debug_pt_info_t), /* size    */
        debug_pt_info_copy,      /* copy    */
        debug_pt_info_cmp,       /* compare */
        debug_pt_info_free,      /* free    */
        debug_pt_info_to_str,    /* to_str  */
        debug_pt_str_to_info     /* from_str */
    },
    {
        /* wrap_cls */
        debug_pt_get_object,    /* get_object   */
        debug_pt_get_wrap_ctx,  /* get_wrap_ctx */
        debug_pt_wrap_object,   /* wrap_object  */
        debug_pt_unwrap_object, /* unwrap_object */
        debug_pt_free_wrap_ctx  /* free_wrap_ctx */
    },
    {
        /* attribute_cls */
        debug_pt_attr_create,   /* create */
        debug_pt_attr_open,     /* open */
        debug_pt_attr_read,     /* read */
        debug_pt_attr_write,    /* write */
        debug_pt_attr_get,      /* get */
        debug_pt_attr_specific, /* specific */
        debug_pt_attr_optional, /* optional */
        debug_pt_attr_close     /* close */
    },
    {
        /* dataset_cls */
        debug_pt_dataset_create,   /* create */
        debug_pt_dataset_open,     /* open */
        debug_pt_dataset_read,     /* read */
        debug_pt_dataset_write,    /* write */
        debug_pt_dataset_get,      /* get */
        debug_pt_dataset_specific, /* specific */
        debug_pt_dataset_optional, /* optional */
        debug_pt_dataset_close     /* close */
    },
    {
        /* datatype_cls */
        debug_pt_datatype_commit,   /* commit */
        debug_pt_datatype_open,     /* open */
        debug_pt_datatype_get,      /* get_size */
        debug_pt_datatype_specific, /* specific */
        debug_pt_datatype_optional, /* optional */
        debug_pt_datatype_close     /* close */
    },
    {
        /* file_cls */
        debug_pt_file_create,   /* create */
        debug_pt_file_open,     /* open */
        debug_pt_file_get,      /* get */
        debug_pt_file_specific, /* specific */
        debug_pt_file_optional, /* optional */
        debug_pt_file_close     /* close */
    },
    {
        /* group_cls */
        debug_pt_group_create,   /* create */
        debug_pt_group_open,     /* open */
        debug_pt_group_get,      /* get */
        debug_pt_group_specific, /* specific */
        debug_pt_group_optional, /* optional */
        debug_pt_group_close     /* close */
    },
    {
        /* link_cls */
        debug_pt_link_create,   /* create */
        debug_pt_link_copy,     /* copy */
        debug_pt_link_move,     /* move */
        debug_pt_link_get,      /* get */
        debug_pt_link_specific, /* specific */
        debug_pt_link_optional  /* optional */
    },
    {
        /* object_cls */
        debug_pt_object_open,     /* open */
        debug_pt_object_copy,     /* copy */
        debug_pt_object_get,      /* get */
        debug_pt_object_specific, /* specific */
        debug_pt_object_optional  /* optional */
    },
    {
        /* introspect_cls */
        debug_pt_introspect_get_conn_cls,  /* get_conn_cls */
        debug_pt_introspect_get_cap_flags, /* get_cap_flags */
        debug_pt_introspect_opt_query,     /* opt_query */
    },
    {
        /* request_cls */
        debug_pt_request_wait,     /* wait */
        debug_pt_request_notify,   /* notify */
        debug_pt_request_cancel,   /* cancel */
        debug_pt_request_specific, /* specific */
        debug_pt_request_optional, /* optional */
        debug_pt_request_free      /* free */
    },
    {
        /* blob_cls */
        debug_pt_blob_put,      /* put */
        debug_pt_blob_get,      /* get */
        debug_pt_blob_specific, /* specific */
        debug_pt_blob_optional  /* optional */
    },
    {
        /* token_cls */
        debug_pt_token_cmp,     /* cmp */
        debug_pt_token_to_str,  /* to_str */
        debug_pt_token_from_str /* from_str */
    },
    debug_pt_optional /* optional */
};

/*-------------------------------------------------------------------------
 * Function:    debug__pt_new_obj
 *
 * Purpose:     Create a new pass through object for an underlying object
 *
 * Return:      Success:    Pointer to the new pass through object
 *              Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
static debug_pt_object_t *
debug__pt_new_obj(void *under_obj, hid_t under_vol_id)
{
    debug_pt_object_t *new_obj;

    new_obj               = (debug_pt_object_t *)calloc(1, sizeof(debug_pt_object_t));
    new_obj->under_object = under_obj;
    new_obj->under_vol_id = under_vol_id;

    H5Iinc_ref(new_obj->under_vol_id);

    return new_obj;
} /* end debug__pt_new_obj() */

/*-------------------------------------------------------------------------
 * Function:    debug__pt_free_obj
 *
 * Purpose:     Release a pass through object
 *
 * Note:	Take care to preserve the current HDF5 error stack
 *		when calling HDF5 API calls.
 *
 * Return:      Success:    0
 *              Failure:    -1
 *
 *-------------------------------------------------------------------------
 */
static herr_t
debug__pt_free_obj(debug_pt_object_t *obj)
{
    hid_t err_id;

    err_id = H5Eget_current_stack();

    H5Idec_ref(obj->under_vol_id);

    H5Eset_current_stack(err_id);

    free(obj);

    return 0;
} /* end debug__pt_free_obj() */

/*---------------------------------------------------------------------------
 * Function:    debug_pt_info_copy
 *
 * Purpose:     Duplicate the connector's info object.
 *
 * Returns:     Success:    New connector info object
 *              Failure:    NULL
 *
 *---------------------------------------------------------------------------
 */
static void *
debug_pt_info_copy(const void *_info)
{
    const debug_pt_info_t *info = (const debug_pt_info_t *)_info;
    debug_pt_info_t       *new_info;

    /* Make sure the underneath VOL of this pass-through VOL is specified */
    if (!info) {
        printf("\n%s, line %d in %s: info for pass-through VOL can't be null\n",
                __FILE__, __LINE__, __func__);
        return NULL;
    }

    if (H5Iis_valid(info->under_vol_id) <= 0) {
        printf("\n%s line %d in %s: not a valid underneath VOL ID for pass-through VOL\n",
               __FILE__, __LINE__, __func__);
        return NULL;
    }

    /* Allocate new VOL info struct for the pass through connector */
    new_info = (debug_pt_info_t *)calloc(1, sizeof(debug_pt_info_t));

    /* Increment reference count on underlying VOL ID, and copy the VOL info */
    new_info->under_vol_id = info->under_vol_id;

    H5Iinc_ref(new_info->under_vol_id);

    if (info->under_vol_info)
        H5VLcopy_connector_info(new_info->under_vol_id, &(new_info->under_vol_info), info->under_vol_info);

    return new_info;
} /* end debug_pt_info_copy() */

/*---------------------------------------------------------------------------
 * Function:    debug_pt_info_cmp
 *
 * Purpose:     Compare two of the connector's info objects, setting *cmp_value,
 *              following the same rules as strcmp().
 *
 * Return:      Success:    0
 *              Failure:    -1
 *
 *---------------------------------------------------------------------------
 */
static herr_t
debug_pt_info_cmp(int *cmp_value, const void *_info1, const void *_info2)
{
    const debug_pt_info_t *info1 = (const debug_pt_info_t *)_info1;
    const debug_pt_info_t *info2 = (const debug_pt_info_t *)_info2;

    /* Sanity checks */
    assert(info1);
    assert(info2);

    /* Initialize comparison value */
    *cmp_value = 0;

    /* Compare under VOL connector classes */
    H5VLcmp_connector_cls(cmp_value, info1->under_vol_id, info2->under_vol_id);
    if (*cmp_value != 0)
        return 0;

    /* Compare under VOL connector info objects */
    H5VLcmp_connector_info(cmp_value, info1->under_vol_id, info1->under_vol_info, info2->under_vol_info);
    if (*cmp_value != 0)
        return 0;

    return 0;
}

/*---------------------------------------------------------------------------
 * Function:    debug_pt_info_free
 *
 * Purpose:     Release an info object for the connector.
 *
 * Note:	Take care to preserve the current HDF5 error stack
 *		when calling HDF5 API calls.
 *
 * Return:      Success:    0
 *              Failure:    -1
 *
 *---------------------------------------------------------------------------
 */
static herr_t
debug_pt_info_free(void *_info)
{
    debug_pt_info_t *info = (debug_pt_info_t *)_info;
    hid_t                     err_id;

    err_id = H5Eget_current_stack();

    /* Release underlying VOL ID and info */
    if (info->under_vol_info)
        H5VLfree_connector_info(info->under_vol_id, info->under_vol_info);
    H5Idec_ref(info->under_vol_id);

    H5Eset_current_stack(err_id);

    /* Free pass through info object itself */
    free(info);

    return 0;
} /* end debug_pt_info_free() */

/*---------------------------------------------------------------------------
 * Function:    debug_pt_info_to_str
 *
 * Purpose:     Serialize an info object for this connector into a string
 *
 * Return:      Success:    0
 *              Failure:    -1
 *
 *---------------------------------------------------------------------------
 */
static herr_t
debug_pt_info_to_str(const void *_info, char **str)
{
    const debug_pt_info_t *info              = (const debug_pt_info_t *)_info;
    H5VL_class_value_t              under_value       = (H5VL_class_value_t)-1;
    char                           *under_vol_string  = NULL;
    size_t                          under_vol_str_len = 0;

    /* Get value and string for underlying VOL connector */
    H5VLget_value(info->under_vol_id, &under_value);
    H5VLconnector_info_to_str(info->under_vol_info, info->under_vol_id, &under_vol_string);

    /* Determine length of underlying VOL info string */
    if (under_vol_string)
        under_vol_str_len = strlen(under_vol_string);

    /* Allocate space for our info */
    size_t strSize = 32 + under_vol_str_len;
    *str           = (char *)H5allocate_memory(strSize, (hbool_t)0);
    assert(*str);

    /* Encode our info */
    snprintf(*str, strSize, "under_vol=%u;under_info={%s}", (unsigned)under_value,
             (under_vol_string ? under_vol_string : ""));

    return 0;
} /* end debug_pt_info_to_str() */

/*---------------------------------------------------------------------------
 * Function:    debug_pt_str_to_info
 *
 * Purpose:     Deserialize a string into an info object for this connector.
 *
 * Return:      Success:    0
 *              Failure:    -1
 *
 *---------------------------------------------------------------------------
 */
static herr_t
debug_pt_str_to_info(const char *str, void **_info)
{
    debug_pt_info_t *info;
    unsigned                  under_vol_value;
    const char               *under_vol_info_start, *under_vol_info_end;
    hid_t                     under_vol_id;
    void                     *under_vol_info = NULL;

    /* Retrieve the underlying VOL connector value and info */
    sscanf(str, "under_vol=%u;", &under_vol_value);
    under_vol_id         = H5VLregister_connector_by_value((H5VL_class_value_t)under_vol_value, H5P_DEFAULT);
    under_vol_info_start = strchr(str, '{');
    under_vol_info_end   = strrchr(str, '}');
    assert(under_vol_info_end > under_vol_info_start);
    if (under_vol_info_end != (under_vol_info_start + 1)) {
        char *under_vol_info_str;

        under_vol_info_str = (char *)malloc((size_t)(under_vol_info_end - under_vol_info_start));
        memcpy(under_vol_info_str, under_vol_info_start + 1,
               (size_t)((under_vol_info_end - under_vol_info_start) - 1));
        *(under_vol_info_str + (under_vol_info_end - under_vol_info_start)) = '\0';

        H5VLconnector_str_to_info(under_vol_info_str, under_vol_id, &under_vol_info);

        free(under_vol_info_str);
    } /* end else */

    /* Allocate new pass-through VOL connector info and set its fields */
    info                 = (debug_pt_info_t *)calloc(1, sizeof(debug_pt_info_t));
    info->under_vol_id   = under_vol_id;
    info->under_vol_info = under_vol_info;

    /* Set return value */
    *_info = info;

    return 0;
} /* end debug_pt_str_to_info() */

/*---------------------------------------------------------------------------
 * Function:    debug_pt_get_object
 *
 * Purpose:     Retrieve the 'data' for a VOL object.
 *
 * Return:      Success:    0
 *              Failure:    -1
 *
 *---------------------------------------------------------------------------
 */
void *
debug_pt_get_object(const void *obj)
{
    const debug_pt_object_t *o = (const debug_pt_object_t *)obj;

    return H5VLget_object(o->under_object, o->under_vol_id);
} /* end debug_pt_get_object() */

/*---------------------------------------------------------------------------
 * Function:    debug_pt_get_wrap_ctx
 *
 * Purpose:     Retrieve a "wrapper context" for an object
 *
 * Return:      Success:    0
 *              Failure:    -1
 *
 *---------------------------------------------------------------------------
 */
static herr_t
debug_pt_get_wrap_ctx(const void *obj, void **wrap_ctx)
{
    const debug_pt_object_t    *o = (const debug_pt_object_t *)obj;
    debug_pt_wrap_ctx_t *new_wrap_ctx;

    /* Allocate new VOL object wrapping context for the pass through connector */
    new_wrap_ctx = (debug_pt_wrap_ctx_t *)calloc(1, sizeof(debug_pt_wrap_ctx_t));

    /* Increment reference count on underlying VOL ID, and copy the VOL info */
    new_wrap_ctx->under_vol_id = o->under_vol_id;

    if (new_wrap_ctx->under_vol_id != H5I_INVALID_HID)
        if (H5Iinc_ref(new_wrap_ctx->under_vol_id) < 0)
            return -1;

    if (H5VLget_wrap_ctx(o->under_object, o->under_vol_id, &new_wrap_ctx->under_wrap_ctx) < 0)
        return -1;

    /* Set wrap context to return */
    *wrap_ctx = new_wrap_ctx;

    return 0;
} /* end debug_pt_get_wrap_ctx() */

/*---------------------------------------------------------------------------
 * Function:    debug_pt_wrap_object
 *
 * Purpose:     Use a "wrapper context" to wrap a data object
 *
 * Return:      Success:    Pointer to wrapped object
 *              Failure:    NULL
 *
 *---------------------------------------------------------------------------
 */
void *
debug_pt_wrap_object(void *obj, H5I_type_t obj_type, void *_wrap_ctx)
{
    debug_pt_wrap_ctx_t *wrap_ctx = (debug_pt_wrap_ctx_t *)_wrap_ctx;
    debug_pt_object_t          *new_obj;
    void                         *under;

    /* Wrap the object with the underlying VOL */
    under = H5VLwrap_object(obj, obj_type, wrap_ctx->under_vol_id, wrap_ctx->under_wrap_ctx);
    if (under)
        new_obj = debug__pt_new_obj(under, wrap_ctx->under_vol_id);
    else
        new_obj = NULL;

    return new_obj;
} /* end debug_pt_wrap_object() */

/*---------------------------------------------------------------------------
 * Function:    debug_pt_unwrap_object
 *
 * Purpose:     Unwrap a wrapped object, discarding the wrapper, but returning
 *		underlying object.
 *
 * Return:      Success:    Pointer to unwrapped object
 *              Failure:    NULL
 *
 *---------------------------------------------------------------------------
 */
void *
debug_pt_unwrap_object(void *obj)
{
    debug_pt_object_t *o = (debug_pt_object_t *)obj;
    void                *under;

    /* Unrap the object with the underlying VOL */
    under = H5VLunwrap_object(o->under_object, o->under_vol_id);

    if (under)
        debug__pt_free_obj(o);

    return under;
} /* end debug_pt_unwrap_object() */

/*---------------------------------------------------------------------------
 * Function:    debug_pt_free_wrap_ctx
 *
 * Purpose:     Release a "wrapper context" for an object
 *
 * Note:	Take care to preserve the current HDF5 error stack
 *		when calling HDF5 API calls.
 *
 * Return:      Success:    0
 *              Failure:    -1
 *
 *---------------------------------------------------------------------------
 */
static herr_t
debug_pt_free_wrap_ctx(void *_wrap_ctx)
{
    debug_pt_wrap_ctx_t *wrap_ctx = (debug_pt_wrap_ctx_t *)_wrap_ctx;
    hid_t                         err_id;

    err_id = H5Eget_current_stack();

    /* Release underlying VOL ID and wrap context */
    if (wrap_ctx->under_wrap_ctx)
        H5VLfree_wrap_ctx(wrap_ctx->under_wrap_ctx, wrap_ctx->under_vol_id);
    H5Idec_ref(wrap_ctx->under_vol_id);

    H5Eset_current_stack(err_id);

    /* Free pass through wrap context object itself */
    free(wrap_ctx);

    return 0;
} /* end debug_pt_free_wrap_ctx() */

/*-------------------------------------------------------------------------
 * Function:    debug_pt_attr_create
 *
 * Purpose:     Creates an attribute on an object.
 *
 * Return:      Success:    Pointer to attribute object
 *              Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
void *
debug_pt_attr_create(void *obj, const H5VL_loc_params_t *loc_params, const char *name, hid_t type_id,
                              hid_t space_id, hid_t acpl_id, hid_t aapl_id, hid_t dxpl_id, void **req)
{
    debug_pt_object_t *attr;
    debug_pt_object_t *o = (debug_pt_object_t *)obj;
    void                *under;

    under = H5VLattr_create(o->under_object, loc_params, o->under_vol_id, name, type_id, space_id, acpl_id,
                            aapl_id, dxpl_id, req);
    if (under) {
        attr = debug__pt_new_obj(under, o->under_vol_id);

        /* Check for async request */
        if (req && *req)
            *req = debug__pt_new_obj(*req, o->under_vol_id);
    } /* end if */
    else
        attr = NULL;

    return (void *)attr;
} /* end debug_pt_attr_create() */

/*-------------------------------------------------------------------------
 * Function:    debug_pt_attr_open
 *
 * Purpose:     Opens an attribute on an object.
 *
 * Return:      Success:    Pointer to attribute object
 *              Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
void *
debug_pt_attr_open(void *obj, const H5VL_loc_params_t *loc_params, const char *name, hid_t aapl_id,
                            hid_t dxpl_id, void **req)
{
    debug_pt_object_t *attr;
    debug_pt_object_t *o = (debug_pt_object_t *)obj;
    void                *under;

    under = H5VLattr_open(o->under_object, loc_params, o->under_vol_id, name, aapl_id, dxpl_id, req);
    if (under) {
        attr = debug__pt_new_obj(under, o->under_vol_id);

        /* Check for async request */
        if (req && *req)
            *req = debug__pt_new_obj(*req, o->under_vol_id);
    } /* end if */
    else
        attr = NULL;

    return (void *)attr;
} /* end debug_pt_attr_open() */

/*-------------------------------------------------------------------------
 * Function:    debug_pt_attr_read
 *
 * Purpose:     Reads data from attribute.
 *
 * Return:      Success:    0
 *              Failure:    -1
 *
 *-------------------------------------------------------------------------
 */
static herr_t
debug_pt_attr_read(void *attr, hid_t mem_type_id, void *buf, hid_t dxpl_id, void **req)
{
    debug_pt_object_t *o = (debug_pt_object_t *)attr;
    herr_t               ret_value;

    ret_value = H5VLattr_read(o->under_object, o->under_vol_id, mem_type_id, buf, dxpl_id, req);

    /* Check for async request */
    if (req && *req)
        *req = debug__pt_new_obj(*req, o->under_vol_id);

    return ret_value;
} /* end debug_pt_attr_read() */

/*-------------------------------------------------------------------------
 * Function:    debug_pt_attr_write
 *
 * Purpose:     Writes data to attribute.
 *
 * Return:      Success:    0
 *              Failure:    -1
 *
 *-------------------------------------------------------------------------
 */
static herr_t
debug_pt_attr_write(void *attr, hid_t mem_type_id, const void *buf, hid_t dxpl_id, void **req)
{
    debug_pt_object_t *o = (debug_pt_object_t *)attr;
    herr_t               ret_value;

    ret_value = H5VLattr_write(o->under_object, o->under_vol_id, mem_type_id, buf, dxpl_id, req);

    /* Check for async request */
    if (req && *req)
        *req = debug__pt_new_obj(*req, o->under_vol_id);

    return ret_value;
} /* end debug_pt_attr_write() */

/*-------------------------------------------------------------------------
 * Function:    debug_pt_attr_get
 *
 * Purpose:     Gets information about an attribute
 *
 * Return:      Success:    0
 *              Failure:    -1
 *
 *-------------------------------------------------------------------------
 */
static herr_t
debug_pt_attr_get(void *obj, H5VL_attr_get_args_t *args, hid_t dxpl_id, void **req)
{
    debug_pt_object_t *o = (debug_pt_object_t *)obj;
    herr_t               ret_value;

    ret_value = H5VLattr_get(o->under_object, o->under_vol_id, args, dxpl_id, req);

    /* Check for async request */
    if (req && *req)
        *req = debug__pt_new_obj(*req, o->under_vol_id);

    return ret_value;
} /* end debug_pt_attr_get() */

/*-------------------------------------------------------------------------
 * Function:    debug_pt_attr_specific
 *
 * Purpose:     Specific operation on attribute
 *
 * Return:      Success:    0
 *              Failure:    -1
 *
 *-------------------------------------------------------------------------
 */
static herr_t
debug_pt_attr_specific(void *obj, const H5VL_loc_params_t *loc_params,
                                H5VL_attr_specific_args_t *args, hid_t dxpl_id, void **req)
{
    debug_pt_object_t *o = (debug_pt_object_t *)obj;
    herr_t               ret_value;

    ret_value = H5VLattr_specific(o->under_object, loc_params, o->under_vol_id, args, dxpl_id, req);

    /* Check for async request */
    if (req && *req)
        *req = debug__pt_new_obj(*req, o->under_vol_id);

    return ret_value;
} /* end debug_pt_attr_specific() */

/*-------------------------------------------------------------------------
 * Function:    debug_pt_attr_optional
 *
 * Purpose:     Perform a connector-specific operation on an attribute
 *
 * Return:      Success:    0
 *              Failure:    -1
 *
 *-------------------------------------------------------------------------
 */
static herr_t
debug_pt_attr_optional(void *obj, H5VL_optional_args_t *args, hid_t dxpl_id, void **req)
{
    debug_pt_object_t *o = (debug_pt_object_t *)obj;
    herr_t               ret_value;

    ret_value = H5VLattr_optional(o->under_object, o->under_vol_id, args, dxpl_id, req);

    /* Check for async request */
    if (req && *req)
        *req = debug__pt_new_obj(*req, o->under_vol_id);

    return ret_value;
} /* end debug_pt_attr_optional() */

/*-------------------------------------------------------------------------
 * Function:    debug_pt_attr_close
 *
 * Purpose:     Closes an attribute.
 *
 * Return:      Success:    0
 *              Failure:    -1, attr not closed.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
debug_pt_attr_close(void *attr, hid_t dxpl_id, void **req)
{
    debug_pt_object_t *o = (debug_pt_object_t *)attr;
    herr_t               ret_value;

    ret_value = H5VLattr_close(o->under_object, o->under_vol_id, dxpl_id, req);

    /* Check for async request */
    if (req && *req)
        *req = debug__pt_new_obj(*req, o->under_vol_id);

    /* Release our wrapper, if underlying attribute was closed */
    if (ret_value >= 0)
        debug__pt_free_obj(o);

    return ret_value;
} /* end debug_pt_attr_close() */

/*-------------------------------------------------------------------------
 * Function:    debug_pt_dataset_create
 *
 * Purpose:     Creates a dataset in a container
 *
 * Return:      Success:    Pointer to a dataset object
 *              Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
void *
debug_pt_dataset_create(void *obj, const H5VL_loc_params_t *loc_params, const char *name,
                                 hid_t lcpl_id, hid_t type_id, hid_t space_id, hid_t dcpl_id, hid_t dapl_id,
                                 hid_t dxpl_id, void **req)
{
    debug_pt_object_t *dset;
    debug_pt_object_t *o = (debug_pt_object_t *)obj;
    void                *under;

    under = H5VLdataset_create(o->under_object, loc_params, o->under_vol_id, name, lcpl_id, type_id, space_id,
                               dcpl_id, dapl_id, dxpl_id, req);
    if (under) {
        dset = debug__pt_new_obj(under, o->under_vol_id);

        /* Check for async request */
        if (req && *req)
            *req = debug__pt_new_obj(*req, o->under_vol_id);
    } /* end if */
    else
        dset = NULL;

    return (void *)dset;
} /* end debug_pt_dataset_create() */

/*-------------------------------------------------------------------------
 * Function:    debug_pt_dataset_open
 *
 * Purpose:     Opens a dataset in a container
 *
 * Return:      Success:    Pointer to a dataset object
 *              Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
void *
debug_pt_dataset_open(void *obj, const H5VL_loc_params_t *loc_params, const char *name,
                               hid_t dapl_id, hid_t dxpl_id, void **req)
{
    debug_pt_object_t *dset;
    debug_pt_object_t *o = (debug_pt_object_t *)obj;
    void                *under;

    under = H5VLdataset_open(o->under_object, loc_params, o->under_vol_id, name, dapl_id, dxpl_id, req);
    if (under) {
        dset = debug__pt_new_obj(under, o->under_vol_id);

        /* Check for async request */
        if (req && *req)
            *req = debug__pt_new_obj(*req, o->under_vol_id);
    } /* end if */
    else
        dset = NULL;

    return (void *)dset;
} /* end debug_pt_dataset_open() */

/*-------------------------------------------------------------------------
 * Function:    debug_pt_dataset_read
 *
 * Purpose:     Reads data elements from a dataset into a buffer.
 *
 * Return:      Success:    0
 *              Failure:    -1
 *
 *-------------------------------------------------------------------------
 */
static herr_t
debug_pt_dataset_read(size_t count, void *dset[], hid_t mem_type_id[], hid_t mem_space_id[],
                               hid_t file_space_id[], hid_t plist_id, void *buf[], void **req)
{
    void  *obj_local;        /* Local buffer for obj */
    void **obj = &obj_local; /* Array of object pointers */
    size_t i;                /* Local index variable */
    herr_t ret_value;

    /* Allocate obj array if necessary */
    if (count > 1)
        if (NULL == (obj = (void **)malloc(count * sizeof(void *))))
            return -1;

    /* Build obj array */
    for (i = 0; i < count; i++) {
        /* Get the object */
        obj[i] = ((debug_pt_object_t *)dset[i])->under_object;

        /* Make sure the class matches */
        if (((debug_pt_object_t *)dset[i])->under_vol_id != ((debug_pt_object_t *)dset[0])->under_vol_id)
            return -1;
    }

    ret_value = H5VLdataset_read(count, obj, ((debug_pt_object_t *)dset[0])->under_vol_id, mem_type_id,
                                 mem_space_id, file_space_id, plist_id, buf, req);

    /* Check for async request */
    if (req && *req)
        *req = debug__pt_new_obj(*req, ((debug_pt_object_t *)dset[0])->under_vol_id);

    /* Free memory */
    if (obj != &obj_local)
        free(obj);

    return ret_value;
} /* end debug_pt_dataset_read() */

/*-------------------------------------------------------------------------
 * Function:    debug_pt_dataset_write
 *
 * Purpose:     Writes data elements from a buffer into a dataset.
 *
 * Return:      Success:    0
 *              Failure:    -1
 *
 *-------------------------------------------------------------------------
 */
static herr_t
debug_pt_dataset_write(size_t count, void *dset[], hid_t mem_type_id[], hid_t mem_space_id[],
                                hid_t file_space_id[], hid_t plist_id, const void *buf[], void **req)
{
    void  *obj_local;        /* Local buffer for obj */
    void **obj = &obj_local; /* Array of object pointers */
    size_t i;                /* Local index variable */
    herr_t ret_value;

    /* Allocate obj array if necessary */
    if (count > 1)
        if (NULL == (obj = (void **)malloc(count * sizeof(void *))))
            return -1;

    /* Build obj array */
    for (i = 0; i < count; i++) {
        /* Get the object */
        obj[i] = ((debug_pt_object_t *)dset[i])->under_object;

        /* Make sure the class matches */
        if (((debug_pt_object_t *)dset[i])->under_vol_id != ((debug_pt_object_t *)dset[0])->under_vol_id)
            return -1;
    }

    ret_value = H5VLdataset_write(count, obj, ((debug_pt_object_t *)dset[0])->under_vol_id, mem_type_id,
                                  mem_space_id, file_space_id, plist_id, buf, req);

    /* Check for async request */
    if (req && *req)
        *req = debug__pt_new_obj(*req, ((debug_pt_object_t *)dset[0])->under_vol_id);

    /* Free memory */
    if (obj != &obj_local)
        free(obj);

    return ret_value;
} /* end debug_pt_dataset_write() */

/*-------------------------------------------------------------------------
 * Function:    debug_pt_dataset_get
 *
 * Purpose:     Gets information about a dataset
 *
 * Return:      Success:    0
 *              Failure:    -1
 *
 *-------------------------------------------------------------------------
 */
static herr_t
debug_pt_dataset_get(void *dset, H5VL_dataset_get_args_t *args, hid_t dxpl_id, void **req)
{
    debug_pt_object_t *o = (debug_pt_object_t *)dset;
    herr_t               ret_value;

    ret_value = H5VLdataset_get(o->under_object, o->under_vol_id, args, dxpl_id, req);

    /* Check for async request */
    if (req && *req)
        *req = debug__pt_new_obj(*req, o->under_vol_id);

    return ret_value;
} /* end debug_pt_dataset_get() */

/*-------------------------------------------------------------------------
 * Function:    debug_pt_dataset_specific
 *
 * Purpose:     Specific operation on a dataset
 *
 * Return:      Success:    0
 *              Failure:    -1
 *
 *-------------------------------------------------------------------------
 */
static herr_t
debug_pt_dataset_specific(void *obj, H5VL_dataset_specific_args_t *args, hid_t dxpl_id, void **req)
{
    debug_pt_object_t *o = (debug_pt_object_t *)obj;
    hid_t                under_vol_id;
    herr_t               ret_value;

#ifdef ENABLE_PASSTHRU_LOGGING
    printf("------- PASS THROUGH VOL H5Dspecific\n");
#endif

    /* Save copy of underlying VOL connector ID, in case of
     * 'refresh' operation destroying the current object
     */
    under_vol_id = o->under_vol_id;

    ret_value = H5VLdataset_specific(o->under_object, o->under_vol_id, args, dxpl_id, req);

    /* Check for async request */
    if (req && *req)
        *req = debug__pt_new_obj(*req, under_vol_id);

    return ret_value;
} /* end debug_pt_dataset_specific() */

/*-------------------------------------------------------------------------
 * Function:    debug_pt_dataset_optional
 *
 * Purpose:     Perform a connector-specific operation on a dataset
 *
 * Return:      Success:    0
 *              Failure:    -1
 *
 *-------------------------------------------------------------------------
 */
static herr_t
debug_pt_dataset_optional(void *obj, H5VL_optional_args_t *args, hid_t dxpl_id, void **req)
{
    debug_pt_object_t *o = (debug_pt_object_t *)obj;
    herr_t               ret_value;

    ret_value = H5VLdataset_optional(o->under_object, o->under_vol_id, args, dxpl_id, req);

    /* Check for async request */
    if (req && *req)
        *req = debug__pt_new_obj(*req, o->under_vol_id);

    return ret_value;
} /* end debug_pt_dataset_optional() */

/*-------------------------------------------------------------------------
 * Function:    debug_pt_dataset_close
 *
 * Purpose:     Closes a dataset.
 *
 * Return:      Success:    0
 *              Failure:    -1, dataset not closed.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
debug_pt_dataset_close(void *dset, hid_t dxpl_id, void **req)
{
    debug_pt_object_t *o = (debug_pt_object_t *)dset;
    herr_t               ret_value;

    ret_value = H5VLdataset_close(o->under_object, o->under_vol_id, dxpl_id, req);

    /* Check for async request */
    if (req && *req)
        *req = debug__pt_new_obj(*req, o->under_vol_id);

    /* Release our wrapper, if underlying dataset was closed */
    if (ret_value >= 0)
        debug__pt_free_obj(o);

    return ret_value;
} /* end debug_pt_dataset_close() */

/*-------------------------------------------------------------------------
 * Function:    debug_pt_datatype_commit
 *
 * Purpose:     Commits a datatype inside a container.
 *
 * Return:      Success:    Pointer to datatype object
 *              Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
void *
debug_pt_datatype_commit(void *obj, const H5VL_loc_params_t *loc_params, const char *name,
                                  hid_t type_id, hid_t lcpl_id, hid_t tcpl_id, hid_t tapl_id, hid_t dxpl_id,
                                  void **req)
{
    debug_pt_object_t *dt;
    debug_pt_object_t *o = (debug_pt_object_t *)obj;
    void                *under;

    under = H5VLdatatype_commit(o->under_object, loc_params, o->under_vol_id, name, type_id, lcpl_id, tcpl_id,
                                tapl_id, dxpl_id, req);
    if (under) {
        dt = debug__pt_new_obj(under, o->under_vol_id);

        /* Check for async request */
        if (req && *req)
            *req = debug__pt_new_obj(*req, o->under_vol_id);
    } /* end if */
    else
        dt = NULL;

    return (void *)dt;
} /* end debug_pt_datatype_commit() */

/*-------------------------------------------------------------------------
 * Function:    debug_pt_datatype_open
 *
 * Purpose:     Opens a named datatype inside a container.
 *
 * Return:      Success:    Pointer to datatype object
 *              Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
void *
debug_pt_datatype_open(void *obj, const H5VL_loc_params_t *loc_params, const char *name,
                                hid_t tapl_id, hid_t dxpl_id, void **req)
{
    debug_pt_object_t *dt;
    debug_pt_object_t *o = (debug_pt_object_t *)obj;
    void                *under;

    under = H5VLdatatype_open(o->under_object, loc_params, o->under_vol_id, name, tapl_id, dxpl_id, req);
    if (under) {
        dt = debug__pt_new_obj(under, o->under_vol_id);

        /* Check for async request */
        if (req && *req)
            *req = debug__pt_new_obj(*req, o->under_vol_id);
    } /* end if */
    else
        dt = NULL;

    return (void *)dt;
} /* end debug_pt_datatype_open() */

/*-------------------------------------------------------------------------
 * Function:    debug_pt_datatype_get
 *
 * Purpose:     Get information about a datatype
 *
 * Return:      Success:    0
 *              Failure:    -1
 *
 *-------------------------------------------------------------------------
 */
static herr_t
debug_pt_datatype_get(void *dt, H5VL_datatype_get_args_t *args, hid_t dxpl_id, void **req)
{
    debug_pt_object_t *o = (debug_pt_object_t *)dt;
    herr_t               ret_value;

    ret_value = H5VLdatatype_get(o->under_object, o->under_vol_id, args, dxpl_id, req);

    /* Check for async request */
    if (req && *req)
        *req = debug__pt_new_obj(*req, o->under_vol_id);

    return ret_value;
} /* end debug_pt_datatype_get() */

/*-------------------------------------------------------------------------
 * Function:    debug_pt_datatype_specific
 *
 * Purpose:     Specific operations for datatypes
 *
 * Return:      Success:    0
 *              Failure:    -1
 *
 *-------------------------------------------------------------------------
 */
static herr_t
debug_pt_datatype_specific(void *obj, H5VL_datatype_specific_args_t *args, hid_t dxpl_id, void **req)
{
    debug_pt_object_t *o = (debug_pt_object_t *)obj;
    hid_t                under_vol_id;
    herr_t               ret_value;

    /* Save copy of underlying VOL connector ID, in case of
     * 'refresh' operation destroying the current object
     */
    under_vol_id = o->under_vol_id;

    ret_value = H5VLdatatype_specific(o->under_object, o->under_vol_id, args, dxpl_id, req);

    /* Check for async request */
    if (req && *req)
        *req = debug__pt_new_obj(*req, under_vol_id);

    return ret_value;
} /* end debug_pt_datatype_specific() */

/*-------------------------------------------------------------------------
 * Function:    debug_pt_datatype_optional
 *
 * Purpose:     Perform a connector-specific operation on a datatype
 *
 * Return:      Success:    0
 *              Failure:    -1
 *
 *-------------------------------------------------------------------------
 */
static herr_t
debug_pt_datatype_optional(void *obj, H5VL_optional_args_t *args, hid_t dxpl_id, void **req)
{
    debug_pt_object_t *o = (debug_pt_object_t *)obj;
    herr_t               ret_value;

    ret_value = H5VLdatatype_optional(o->under_object, o->under_vol_id, args, dxpl_id, req);

    /* Check for async request */
    if (req && *req)
        *req = debug__pt_new_obj(*req, o->under_vol_id);

    return ret_value;
} /* end debug_pt_datatype_optional() */

/*-------------------------------------------------------------------------
 * Function:    debug_pt_datatype_close
 *
 * Purpose:     Closes a datatype.
 *
 * Return:      Success:    0
 *              Failure:    -1, datatype not closed.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
debug_pt_datatype_close(void *dt, hid_t dxpl_id, void **req)
{
    debug_pt_object_t *o = (debug_pt_object_t *)dt;
    herr_t               ret_value;

    assert(o->under_object);

    ret_value = H5VLdatatype_close(o->under_object, o->under_vol_id, dxpl_id, req);

    /* Check for async request */
    if (req && *req)
        *req = debug__pt_new_obj(*req, o->under_vol_id);

    /* Release our wrapper, if underlying datatype was closed */
    if (ret_value >= 0)
        debug__pt_free_obj(o);

    return ret_value;
} /* end debug_pt_datatype_close() */

/*-------------------------------------------------------------------------
 * Function:    debug_pt_file_create
 *
 * Purpose:     Creates a container using this connector
 *
 * Return:      Success:    Pointer to a file object
 *              Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
void *
debug_pt_file_create(const char *name, unsigned flags, hid_t fcpl_id, hid_t fapl_id, hid_t dxpl_id,
                              void **req)
{
    debug_pt_info_t *info;
    debug_pt_object_t      *file;
    hid_t                     under_fapl_id;
    void                     *under;
    char under_name[2048];

    /* Get copy of our VOL info from FAPL */
    H5Pget_vol_info(fapl_id, (void **)&info);

    /* Make sure we have info about the underlying VOL to be used */
    if (!info)
        return NULL;

    /* Copy the FAPL */
    under_fapl_id = H5Pcopy(fapl_id);

    /* Set the VOL ID and info for the underlying FAPL */
    H5Pset_vol(under_fapl_id, info->under_vol_id, info->under_vol_info);

    strcpy(under_name, name);
    strcpy(under_name + strlen(name), PT_SUFFIX);

    /* Open the file with the underlying VOL connector */
    under = H5VLfile_create(under_name, flags, fcpl_id, under_fapl_id, dxpl_id, req);
    if (under) {
        file = debug__pt_new_obj(under, info->under_vol_id);

        /* Check for async request */
        if (req && *req)
            *req = debug__pt_new_obj(*req, info->under_vol_id);
    } /* end if */
    else
        file = NULL;

    /* Close underlying FAPL */
    H5Pclose(under_fapl_id);

    /* Release copy of our VOL info */
    debug_pt_info_free(info);

    return (void *)file;
} /* end debug_pt_file_create() */

/*-------------------------------------------------------------------------
 * Function:    debug_pt_file_open
 *
 * Purpose:     Opens a container created with this connector
 *
 * Return:      Success:    Pointer to a file object
 *              Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
void *
debug_pt_file_open(const char *name, unsigned flags, hid_t fapl_id, hid_t dxpl_id, void **req)
{
    debug_pt_info_t *info = NULL;
    debug_pt_object_t      *file = NULL;
    hid_t                     under_fapl_id = H5I_INVALID_HID;
    void                     *under = NULL;
    char under_name[2048];
    bool req_created = false;

    memset(under_name, 0, 2048);
    /* Get copy of our VOL info from FAPL */
    if (H5Pget_vol_info(fapl_id, (void **)&info) < 0) {
        goto err;
    }

    /* Make sure we have info about the underlying VOL to be used */
    if (!info) {
        goto err;
    }

    /* Copy the FAPL */
    if ((under_fapl_id = H5Pcopy(fapl_id)) < 0) {
        goto err;
    }

    /* Set the VOL ID and info for the underlying FAPL */
    if (H5Pset_vol(under_fapl_id, info->under_vol_id, info->under_vol_info) < 0) {
        goto err;
    }

    strcpy(under_name, name);
    strcpy(under_name + strlen(name), PT_SUFFIX);

    /* Open the file with the underlying VOL connector */
    if ((under = H5VLfile_open(under_name, flags, under_fapl_id, dxpl_id, req)) == NULL) {
        goto err;
    }

    if ((file = debug__pt_new_obj(under, info->under_vol_id)) == NULL) {
        goto err;
    }

    /* Check for async request */
    if (req && *req) {
        if ((*req = debug__pt_new_obj(*req, info->under_vol_id)) == NULL) {
            goto err;
        }
        req_created = true;
    }

    /* Close underlying FAPL */
    if (under_fapl_id > 0)
        H5Pclose(under_fapl_id);

    /* Release copy of our VOL info */
    if (info)
        debug_pt_info_free(info);

    return (void *)file;

err:
    /* Close underlying FAPL */
    if (under_fapl_id > 0)
        H5Pclose(under_fapl_id);

    /* Release copy of our VOL info */
    if (info)
        debug_pt_info_free(info);
    
    if (under)
        H5VLfile_close(under, info->under_vol_id, dxpl_id, req);
    
    if (file)
        debug__pt_free_obj(file);
    
    if (req_created)
        debug__pt_free_obj(*req);

    return (void*) NULL;
} /* end debug_pt_file_open() */

/*-------------------------------------------------------------------------
 * Function:    debug_pt_file_get
 *
 * Purpose:     Get info about a file
 *
 * Return:      Success:    0
 *              Failure:    -1
 *
 *-------------------------------------------------------------------------
 */
static herr_t
debug_pt_file_get(void *file, H5VL_file_get_args_t *args, hid_t dxpl_id, void **req)
{
    debug_pt_object_t *o = (debug_pt_object_t *)file;
    herr_t               ret_value;

    ret_value = H5VLfile_get(o->under_object, o->under_vol_id, args, dxpl_id, req);

    if (args->op_type == H5VL_FILE_GET_NAME) {
        *args->args.get_name.file_name_len -= PT_SUFFIX_LEN;
        /* Length-only */
        if (args->args.get_name.buf_size != 0) {
            args->args.get_name.buf[*args->args.get_name.file_name_len ] = '\0';
        }
        
    }

    /* Check for async request */
    if (req && *req)
        *req = debug__pt_new_obj(*req, o->under_vol_id);

    return ret_value;
} /* end debug_pt_file_get() */

/*-------------------------------------------------------------------------
 * Function:    debug_pt_file_specific
 *
 * Purpose:     Specific operation on file
 *
 * Return:      Success:    0
 *              Failure:    -1
 *
 *-------------------------------------------------------------------------
 */
static herr_t
debug_pt_file_specific(void *file, H5VL_file_specific_args_t *args, hid_t dxpl_id, void **req)
{
    debug_pt_object_t       *o = (debug_pt_object_t *)file;
    debug_pt_object_t       *new_o;
    H5VL_file_specific_args_t  my_args;
    H5VL_file_specific_args_t *new_args;
    debug_pt_info_t  *info         = NULL;
    hid_t                      under_vol_id = -1;
    herr_t                     ret_value;
    char                       under_name[2048];

    if (args->op_type == H5VL_FILE_IS_ACCESSIBLE) {
        /* Shallow copy the args */
        memcpy(&my_args, args, sizeof(my_args));

        strcpy(under_name, args->args.is_accessible.filename);
        strcpy(under_name + strlen(args->args.is_accessible.filename), PT_SUFFIX);
        my_args.args.is_accessible.filename = under_name;


        /* Get copy of our VOL info from FAPL */
        H5Pget_vol_info(args->args.is_accessible.fapl_id, (void **)&info);

        /* Make sure we have info about the underlying VOL to be used */
        if (!info)
            return (-1);

        /* Keep the correct underlying VOL ID for later */
        under_vol_id = info->under_vol_id;

        /* Copy the FAPL */
        my_args.args.is_accessible.fapl_id = H5Pcopy(args->args.is_accessible.fapl_id);

        /* Set the VOL ID and info for the underlying FAPL */
        H5Pset_vol(my_args.args.is_accessible.fapl_id, info->under_vol_id, info->under_vol_info);

        /* Set argument pointer to new arguments */
        new_args = &my_args;

        /* Set object pointer for operation */
        new_o = NULL;
    } /* end else-if */
    else if (args->op_type == H5VL_FILE_DELETE) {
        /* Shallow copy the args */
        memcpy(&my_args, args, sizeof(my_args));
        strcpy(under_name, args->args.del.filename);
        strcpy(under_name + strlen(args->args.del.filename), PT_SUFFIX);
        my_args.args.del.filename = under_name;

        /* Get copy of our VOL info from FAPL */
        H5Pget_vol_info(args->args.del.fapl_id, (void **)&info);

        /* Make sure we have info about the underlying VOL to be used */
        if (!info)
            return (-1);

        /* Keep the correct underlying VOL ID for later */
        under_vol_id = info->under_vol_id;

        /* Copy the FAPL */
        my_args.args.del.fapl_id = H5Pcopy(args->args.del.fapl_id);

        /* Set the VOL ID and info for the underlying FAPL */
        H5Pset_vol(my_args.args.del.fapl_id, info->under_vol_id, info->under_vol_info);

        /* Set argument pointer to new arguments */
        new_args = &my_args;

        /* Set object pointer for operation */
        new_o = NULL;
    } /* end else-if */
    else {
        /* Keep the correct underlying VOL ID for later */
        under_vol_id = o->under_vol_id;

        /* Set argument pointer to current arguments */
        new_args = args;

        /* Set object pointer for operation */
        new_o = o->under_object;
    } /* end else */

    ret_value = H5VLfile_specific(new_o, under_vol_id, new_args, dxpl_id, req);

    /* Check for async request */
    if (req && *req)
        *req = debug__pt_new_obj(*req, under_vol_id);

    if (args->op_type == H5VL_FILE_IS_ACCESSIBLE) {
        /* Close underlying FAPL */
        H5Pclose(my_args.args.is_accessible.fapl_id);

        /* Release copy of our VOL info */
        debug_pt_info_free(info);
    } /* end else-if */
    else if (args->op_type == H5VL_FILE_DELETE) {
        /* Close underlying FAPL */
        H5Pclose(my_args.args.del.fapl_id);

        /* Release copy of our VOL info */
        debug_pt_info_free(info);
    } /* end else-if */
    else if (args->op_type == H5VL_FILE_REOPEN) {
        /* Wrap file struct pointer for 'reopen' operation, if we reopened one */
        if (ret_value >= 0 && *args->args.reopen.file)
            *args->args.reopen.file = debug__pt_new_obj(*args->args.reopen.file, under_vol_id);
    } /* end else */

    return ret_value;
} /* end debug_pt_file_specific() */

/*-------------------------------------------------------------------------
 * Function:    debug_pt_file_optional
 *
 * Purpose:     Perform a connector-specific operation on a file
 *
 * Return:      Success:    0
 *              Failure:    -1
 *
 *-------------------------------------------------------------------------
 */
static herr_t
debug_pt_file_optional(void *file, H5VL_optional_args_t *args, hid_t dxpl_id, void **req)
{
    debug_pt_object_t *o = (debug_pt_object_t *)file;
    herr_t               ret_value;

    ret_value = H5VLfile_optional(o->under_object, o->under_vol_id, args, dxpl_id, req);

    /* Check for async request */
    if (req && *req)
        *req = debug__pt_new_obj(*req, o->under_vol_id);

    return ret_value;
} /* end debug_pt_file_optional() */

/*-------------------------------------------------------------------------
 * Function:    debug_pt_file_close
 *
 * Purpose:     Closes a file.
 *
 * Return:      Success:    0
 *              Failure:    -1, file not closed.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
debug_pt_file_close(void *file, hid_t dxpl_id, void **req)
{
    debug_pt_object_t *o = (debug_pt_object_t *)file;
    herr_t               ret_value;

    ret_value = H5VLfile_close(o->under_object, o->under_vol_id, dxpl_id, req);

    /* Check for async request */
    if (req && *req)
        *req = debug__pt_new_obj(*req, o->under_vol_id);

    /* Release our wrapper, if underlying file was closed */
    if (ret_value >= 0)
        debug__pt_free_obj(o);

    return ret_value;
} /* end debug_pt_file_close() */

/*-------------------------------------------------------------------------
 * Function:    debug_pt_group_create
 *
 * Purpose:     Creates a group inside a container
 *
 * Return:      Success:    Pointer to a group object
 *              Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
void *
debug_pt_group_create(void *obj, const H5VL_loc_params_t *loc_params, const char *name,
                               hid_t lcpl_id, hid_t gcpl_id, hid_t gapl_id, hid_t dxpl_id, void **req)
{
    debug_pt_object_t *group;
    debug_pt_object_t *o = (debug_pt_object_t *)obj;
    void                *under;

    under = H5VLgroup_create(o->under_object, loc_params, o->under_vol_id, name, lcpl_id, gcpl_id, gapl_id,
                             dxpl_id, req);
    if (under) {
        group = debug__pt_new_obj(under, o->under_vol_id);

        /* Check for async request */
        if (req && *req)
            *req = debug__pt_new_obj(*req, o->under_vol_id);
    } /* end if */
    else
        group = NULL;

    return (void *)group;
} /* end debug_pt_group_create() */

/*-------------------------------------------------------------------------
 * Function:    debug_pt_group_open
 *
 * Purpose:     Opens a group inside a container
 *
 * Return:      Success:    Pointer to a group object
 *              Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
void *
debug_pt_group_open(void *obj, const H5VL_loc_params_t *loc_params, const char *name, hid_t gapl_id,
                             hid_t dxpl_id, void **req)
{
    debug_pt_object_t *group;
    debug_pt_object_t *o = (debug_pt_object_t *)obj;
    void                *under;

    under = H5VLgroup_open(o->under_object, loc_params, o->under_vol_id, name, gapl_id, dxpl_id, req);
    if (under) {
        group = debug__pt_new_obj(under, o->under_vol_id);

        /* Check for async request */
        if (req && *req)
            *req = debug__pt_new_obj(*req, o->under_vol_id);
    } /* end if */
    else
        group = NULL;

    return (void *)group;
} /* end debug_pt_group_open() */

/*-------------------------------------------------------------------------
 * Function:    debug_pt_group_get
 *
 * Purpose:     Get info about a group
 *
 * Return:      Success:    0
 *              Failure:    -1
 *
 *-------------------------------------------------------------------------
 */
static herr_t
debug_pt_group_get(void *obj, H5VL_group_get_args_t *args, hid_t dxpl_id, void **req)
{
    debug_pt_object_t *o = (debug_pt_object_t *)obj;
    herr_t               ret_value;

    ret_value = H5VLgroup_get(o->under_object, o->under_vol_id, args, dxpl_id, req);

    /* Check for async request */
    if (req && *req)
        *req = debug__pt_new_obj(*req, o->under_vol_id);

    return ret_value;
} /* end debug_pt_group_get() */

/*-------------------------------------------------------------------------
 * Function:    debug_pt_group_specific
 *
 * Purpose:     Specific operation on a group
 *
 * Return:      Success:    0
 *              Failure:    -1
 *
 *-------------------------------------------------------------------------
 */
static herr_t
debug_pt_group_specific(void *obj, H5VL_group_specific_args_t *args, hid_t dxpl_id, void **req)
{
    debug_pt_object_t *o = (debug_pt_object_t *)obj;
    hid_t                under_vol_id;
    herr_t               ret_value;

    /* Save copy of underlying VOL connector ID, in case of
     * 'refresh' operation destroying the current object
     */
    under_vol_id = o->under_vol_id;

    /* Unpack arguments to get at the child file pointer when mounting a file */
    if (args->op_type == H5VL_GROUP_MOUNT) {
        H5VL_group_specific_args_t vol_cb_args; /* New group specific arg struct */

        /* Set up new VOL callback arguments */
        vol_cb_args.op_type         = H5VL_GROUP_MOUNT;
        vol_cb_args.args.mount.name = args->args.mount.name;
        vol_cb_args.args.mount.child_file =
            ((debug_pt_object_t *)args->args.mount.child_file)->under_object;
        vol_cb_args.args.mount.fmpl_id = args->args.mount.fmpl_id;

        /* Re-issue 'group specific' call, using the unwrapped pieces */
        ret_value = H5VLgroup_specific(o->under_object, under_vol_id, &vol_cb_args, dxpl_id, req);
    } /* end if */
    else
        ret_value = H5VLgroup_specific(o->under_object, under_vol_id, args, dxpl_id, req);

    /* Check for async request */
    if (req && *req)
        *req = debug__pt_new_obj(*req, under_vol_id);

    return ret_value;
} /* end debug_pt_group_specific() */

/*-------------------------------------------------------------------------
 * Function:    debug_pt_group_optional
 *
 * Purpose:     Perform a connector-specific operation on a group
 *
 * Return:      Success:    0
 *              Failure:    -1
 *
 *-------------------------------------------------------------------------
 */
static herr_t
debug_pt_group_optional(void *obj, H5VL_optional_args_t *args, hid_t dxpl_id, void **req)
{
    debug_pt_object_t *o = (debug_pt_object_t *)obj;
    herr_t               ret_value;

    ret_value = H5VLgroup_optional(o->under_object, o->under_vol_id, args, dxpl_id, req);

    /* Check for async request */
    if (req && *req)
        *req = debug__pt_new_obj(*req, o->under_vol_id);

    return ret_value;
} /* end debug_pt_group_optional() */

/*-------------------------------------------------------------------------
 * Function:    debug_pt_group_close
 *
 * Purpose:     Closes a group.
 *
 * Return:      Success:    0
 *              Failure:    -1, group not closed.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
debug_pt_group_close(void *grp, hid_t dxpl_id, void **req)
{
    debug_pt_object_t *o = (debug_pt_object_t *)grp;
    herr_t               ret_value;

#ifdef ENABLE_PASSTHRU_LOGGING
    printf("------- PASS THROUGH VOL H5Gclose\n");
#endif

    ret_value = H5VLgroup_close(o->under_object, o->under_vol_id, dxpl_id, req);

    /* Check for async request */
    if (req && *req)
        *req = debug__pt_new_obj(*req, o->under_vol_id);

    /* Release our wrapper, if underlying file was closed */
    if (ret_value >= 0)
        debug__pt_free_obj(o);

    return ret_value;
} /* end debug_pt_group_close() */

/*-------------------------------------------------------------------------
 * Function:    debug_pt_link_create
 *
 * Purpose:     Creates a hard / soft / UD / external link.
 *
 * Return:      Success:    0
 *              Failure:    -1
 *
 *-------------------------------------------------------------------------
 */
static herr_t
debug_pt_link_create(H5VL_link_create_args_t *args, void *obj, const H5VL_loc_params_t *loc_params,
                              hid_t lcpl_id, hid_t lapl_id, hid_t dxpl_id, void **req)
{
    debug_pt_object_t *o            = (debug_pt_object_t *)obj;
    hid_t                under_vol_id = -1;
    herr_t               ret_value = 0;
    char under_name[2048];
    char *new_ext_link_buffer = NULL;
    const char *filename = NULL;
    const void *temp = NULL;
    size_t temp_size = 0;
    memset(under_name, 0, sizeof(under_name));

    /* Try to retrieve the "under" VOL id */
    if (o)
        under_vol_id = o->under_vol_id;

    /* Fix up the link target object for hard link creation */
    if (H5VL_LINK_CREATE_HARD == args->op_type) {
        void *cur_obj = args->args.hard.curr_obj;

        /* If cur_obj is a non-NULL pointer, find its 'under object' and update the pointer */
        if (cur_obj) {
            /* Check if we still haven't set the "under" VOL ID */
            if (under_vol_id < 0)
                under_vol_id = ((debug_pt_object_t *)cur_obj)->under_vol_id;

            /* Update the object for the link target */
            args->args.hard.curr_obj = ((debug_pt_object_t *)cur_obj)->under_object;
        } /* end if */
    }     /* end if */

    if (H5VL_LINK_CREATE_UD == args->op_type && args->args.ud.type == H5L_TYPE_EXTERNAL) {
        /* Read filename, skip flag byte in buf */
        filename = (const char*)args->args.ud.buf + 1;
        strcpy(under_name, filename);
        strcat(under_name, PT_SUFFIX);

        /* Assemble a new ext link buffer that includes this longer name */
        const char *obj_name_ptr = filename + strlen(filename) + 1;
        size_t under_len = strlen(under_name);
        size_t obj_name_len = strlen(obj_name_ptr);
        new_ext_link_buffer = malloc(1 + under_len + 1 + obj_name_len + 1);

        /* Flag byte */
        memcpy(new_ext_link_buffer, args->args.ud.buf, 1);
        /* Name with suffix */
        strcpy(new_ext_link_buffer + 1, under_name);
        /* Obj name */
        strcpy(new_ext_link_buffer + 1 + strlen(under_name) + 1, obj_name_ptr);

        temp = args->args.ud.buf;
        temp_size = args->args.ud.buf_size;
        args->args.ud.buf = new_ext_link_buffer;
        args->args.ud.buf_size = 1 + strlen(under_name) + 1 + strlen(obj_name_ptr) + 1;
    }

    ret_value = H5VLlink_create(args, (o ? o->under_object : NULL), loc_params, under_vol_id, lcpl_id,
                                lapl_id, dxpl_id, req);

    if (H5VL_LINK_CREATE_UD == args->op_type) {
        args->args.ud.buf = temp;
        args->args.ud.buf_size = temp_size;
        free(new_ext_link_buffer);
    }
    /* Check for async request */
    if (req && *req)
        *req = debug__pt_new_obj(*req, under_vol_id);

    return ret_value;
} /* end debug_pt_link_create() */

/*-------------------------------------------------------------------------
 * Function:    debug_pt_link_copy
 *
 * Purpose:     Renames an object within an HDF5 container and copies it to a new
 *              group.  The original name SRC is unlinked from the group graph
 *              and then inserted with the new name DST (which can specify a
 *              new path for the object) as an atomic operation. The names
 *              are interpreted relative to SRC_LOC_ID and
 *              DST_LOC_ID, which are either file IDs or group ID.
 *
 * Return:      Success:    0
 *              Failure:    -1
 *
 *-------------------------------------------------------------------------
 */
static herr_t
debug_pt_link_copy(void *src_obj, const H5VL_loc_params_t *loc_params1, void *dst_obj,
                            const H5VL_loc_params_t *loc_params2, hid_t lcpl_id, hid_t lapl_id, hid_t dxpl_id,
                            void **req)
{
    debug_pt_object_t *o_src        = (debug_pt_object_t *)src_obj;
    debug_pt_object_t *o_dst        = (debug_pt_object_t *)dst_obj;
    hid_t                under_vol_id = -1;
    herr_t               ret_value;

    /* Retrieve the "under" VOL id */
    if (o_src)
        under_vol_id = o_src->under_vol_id;
    else if (o_dst)
        under_vol_id = o_dst->under_vol_id;
    assert(under_vol_id > 0);

    ret_value =
        H5VLlink_copy((o_src ? o_src->under_object : NULL), loc_params1, (o_dst ? o_dst->under_object : NULL),
                      loc_params2, under_vol_id, lcpl_id, lapl_id, dxpl_id, req);

    /* Check for async request */
    if (req && *req)
        *req = debug__pt_new_obj(*req, under_vol_id);

    return ret_value;
} /* end debug_pt_link_copy() */

/*-------------------------------------------------------------------------
 * Function:    debug_pt_link_move
 *
 * Purpose:     Moves a link within an HDF5 file to a new group.  The original
 *              name SRC is unlinked from the group graph
 *              and then inserted with the new name DST (which can specify a
 *              new path for the object) as an atomic operation. The names
 *              are interpreted relative to SRC_LOC_ID and
 *              DST_LOC_ID, which are either file IDs or group ID.
 *
 * Return:      Success:    0
 *              Failure:    -1
 *
 *-------------------------------------------------------------------------
 */
static herr_t
debug_pt_link_move(void *src_obj, const H5VL_loc_params_t *loc_params1, void *dst_obj,
                            const H5VL_loc_params_t *loc_params2, hid_t lcpl_id, hid_t lapl_id, hid_t dxpl_id,
                            void **req)
{
    debug_pt_object_t *o_src        = (debug_pt_object_t *)src_obj;
    debug_pt_object_t *o_dst        = (debug_pt_object_t *)dst_obj;
    hid_t                under_vol_id = -1;
    herr_t               ret_value;

    /* Retrieve the "under" VOL id */
    if (o_src)
        under_vol_id = o_src->under_vol_id;
    else if (o_dst)
        under_vol_id = o_dst->under_vol_id;
    assert(under_vol_id > 0);

    ret_value =
        H5VLlink_move((o_src ? o_src->under_object : NULL), loc_params1, (o_dst ? o_dst->under_object : NULL),
                      loc_params2, under_vol_id, lcpl_id, lapl_id, dxpl_id, req);

    /* Check for async request */
    if (req && *req)
        *req = debug__pt_new_obj(*req, under_vol_id);

    return ret_value;
} /* end debug_pt_link_move() */

static herr_t
get_link_type(debug_pt_object_t *o, const H5VL_loc_params_t *loc_params, hid_t dxpl_id, void **req, H5L_type_t *type) {
    H5L_info2_t linfo;
    H5VL_link_get_args_t gargs;

    gargs.op_type = H5VL_LINK_GET_INFO;
    gargs.args.get_info.linfo = &linfo;

    if (H5VLlink_get(o->under_object, loc_params, o->under_vol_id, &gargs, dxpl_id, req) < 0) {
        return -1;
    }

    *type = linfo.type;
    return 0;
}
/*-------------------------------------------------------------------------
 * Function:    debug_pt_link_get
 *
 * Purpose:     Get info about a link
 *
 * Return:      Success:    0
 *              Failure:    -1
 *
 *-------------------------------------------------------------------------
 */
static herr_t
debug_pt_link_get(void *obj, const H5VL_loc_params_t *loc_params, H5VL_link_get_args_t *args,
                           hid_t dxpl_id, void **req)
{
    debug_pt_object_t *o = (debug_pt_object_t *)obj;
    herr_t               ret_value;
    void *temp = NULL;
    size_t temp_size = 0;

    void *under_buf = NULL;
    size_t under_size = 0;

    H5L_type_t type = H5L_TYPE_ERROR;

    if (args->op_type == H5VL_LINK_GET_VAL) {
        if (get_link_type(o, loc_params, dxpl_id, req, &type) < 0) {
            fprintf(stderr, "failed to check link type\n");
        }

        if (type == H5L_TYPE_EXTERNAL) {
            under_buf = calloc(1, args->args.get_val.buf_size + PT_SUFFIX_LEN + 1);
            under_size = args->args.get_val.buf_size + PT_SUFFIX_LEN + 1;

            temp = args->args.get_val.buf;
            temp_size = args->args.get_val.buf_size;

            args->args.get_val.buf = under_buf;
            args->args.get_val.buf_size = under_size;
        }
        
    }

    ret_value = H5VLlink_get(o->under_object, loc_params, o->under_vol_id, args, dxpl_id, req);

    if (args->op_type == H5VL_LINK_GET_VAL) {
        if (type == H5L_TYPE_EXTERNAL) {
            /* Extract original name without suffix */
            char *under_filename = (char*)under_buf + 1;
            char *under_obj_name = (char*)under_buf + 1 + strlen(under_filename) + 1;
            /* Restore output to user-assigned buf, to set up user-visible return values */
            args->args.get_val.buf = temp;
            args->args.get_val.buf_size = temp_size;
            /* Copy the flag into user buffer */
            ((uint8_t*)args->args.get_val.buf)[0] = ((uint8_t*)under_buf)[0];

            /* Copy the original filename into user buffer */
            memcpy((char*) args->args.get_val.buf + 1, under_filename, strlen(under_filename) - PT_SUFFIX_LEN);
            /* Add null byte */
            ((uint8_t*)args->args.get_val.buf)[1 + strlen(under_filename) - PT_SUFFIX_LEN] = '\0';

            /* Obj name */
            strcpy(((char*)args->args.get_val.buf) + 1 + strlen(under_filename) - PT_SUFFIX_LEN + 1, under_obj_name);
            free(under_buf);
        }
       
    } else if (args->op_type == H5VL_LINK_GET_INFO && args->args.get_info.linfo->type == H5L_TYPE_EXTERNAL) {
        args->args.get_info.linfo->u.val_size -= PT_SUFFIX_LEN;
    }

    /* Check for async request */
    if (req && *req)
        *req = debug__pt_new_obj(*req, o->under_vol_id);

    return ret_value;
} /* end debug_pt_link_get() */

/*-------------------------------------------------------------------------
 * Function:    debug_pt_link_specific
 *
 * Purpose:     Specific operation on a link
 *
 * Return:      Success:    0
 *              Failure:    -1
 *
 *-------------------------------------------------------------------------
 */
static herr_t
debug_pt_link_specific(void *obj, const H5VL_loc_params_t *loc_params,
                                H5VL_link_specific_args_t *args, hid_t dxpl_id, void **req)
{
    debug_pt_object_t *o = (debug_pt_object_t *)obj;
    herr_t               ret_value;

    ret_value = H5VLlink_specific(o->under_object, loc_params, o->under_vol_id, args, dxpl_id, req);

    /* Check for async request */
    if (req && *req)
        *req = debug__pt_new_obj(*req, o->under_vol_id);

    return ret_value;
} /* end debug_pt_link_specific() */

/*-------------------------------------------------------------------------
 * Function:    debug_pt_link_optional
 *
 * Purpose:     Perform a connector-specific operation on a link
 *
 * Return:      Success:    0
 *              Failure:    -1
 *
 *-------------------------------------------------------------------------
 */
static herr_t
debug_pt_link_optional(void *obj, const H5VL_loc_params_t *loc_params, H5VL_optional_args_t *args,
                                hid_t dxpl_id, void **req)
{
    debug_pt_object_t *o = (debug_pt_object_t *)obj;
    herr_t               ret_value;

    ret_value = H5VLlink_optional(o->under_object, loc_params, o->under_vol_id, args, dxpl_id, req);

    /* Check for async request */
    if (req && *req)
        *req = debug__pt_new_obj(*req, o->under_vol_id);

    return ret_value;
} /* end debug_pt_link_optional() */

/*-------------------------------------------------------------------------
 * Function:    debug_pt_object_open
 *
 * Purpose:     Opens an object inside a container.
 *
 * Return:      Success:    Pointer to object
 *              Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
void *
debug_pt_object_open(void *obj, const H5VL_loc_params_t *loc_params, H5I_type_t *opened_type,
                              hid_t dxpl_id, void **req)
{
    debug_pt_object_t *new_obj;
    debug_pt_object_t *o = (debug_pt_object_t *)obj;
    void                *under;

    under = H5VLobject_open(o->under_object, loc_params, o->under_vol_id, opened_type, dxpl_id, req);
    if (under) {
        new_obj = debug__pt_new_obj(under, o->under_vol_id);

        /* Check for async request */
        if (req && *req)
            *req = debug__pt_new_obj(*req, o->under_vol_id);
    } /* end if */
    else
        new_obj = NULL;

    return (void *)new_obj;
} /* end debug_pt_object_open() */

/*-------------------------------------------------------------------------
 * Function:    debug_pt_object_copy
 *
 * Purpose:     Copies an object inside a container.
 *
 * Return:      Success:    0
 *              Failure:    -1
 *
 *-------------------------------------------------------------------------
 */
static herr_t
debug_pt_object_copy(void *src_obj, const H5VL_loc_params_t *src_loc_params, const char *src_name,
                              void *dst_obj, const H5VL_loc_params_t *dst_loc_params, const char *dst_name,
                              hid_t ocpypl_id, hid_t lcpl_id, hid_t dxpl_id, void **req)
{
    debug_pt_object_t *o_src = (debug_pt_object_t *)src_obj;
    debug_pt_object_t *o_dst = (debug_pt_object_t *)dst_obj;
    herr_t               ret_value;

    ret_value =
        H5VLobject_copy(o_src->under_object, src_loc_params, src_name, o_dst->under_object, dst_loc_params,
                        dst_name, o_src->under_vol_id, ocpypl_id, lcpl_id, dxpl_id, req);

    /* Check for async request */
    if (req && *req)
        *req = debug__pt_new_obj(*req, o_src->under_vol_id);

    return ret_value;
} /* end debug_pt_object_copy() */

/*-------------------------------------------------------------------------
 * Function:    debug_pt_object_get
 *
 * Purpose:     Get info about an object
 *
 * Return:      Success:    0
 *              Failure:    -1
 *
 *-------------------------------------------------------------------------
 */
static herr_t
debug_pt_object_get(void *obj, const H5VL_loc_params_t *loc_params, H5VL_object_get_args_t *args,
                             hid_t dxpl_id, void **req)
{
    debug_pt_object_t *o = (debug_pt_object_t *)obj;
    herr_t               ret_value;

    ret_value = H5VLobject_get(o->under_object, loc_params, o->under_vol_id, args, dxpl_id, req);

    /* Check for async request */
    if (req && *req)
        *req = debug__pt_new_obj(*req, o->under_vol_id);

    return ret_value;
} /* end debug_pt_object_get() */

/*-------------------------------------------------------------------------
 * Function:    debug_pt_object_specific
 *
 * Purpose:     Specific operation on an object
 *
 * Return:      Success:    0
 *              Failure:    -1
 *
 *-------------------------------------------------------------------------
 */
static herr_t
debug_pt_object_specific(void *obj, const H5VL_loc_params_t *loc_params,
                                  H5VL_object_specific_args_t *args, hid_t dxpl_id, void **req)
{
    debug_pt_object_t *o = (debug_pt_object_t *)obj;
    hid_t                under_vol_id;
    herr_t               ret_value;

    /* Save copy of underlying VOL connector ID, in case of
     * 'refresh' operation destroying the current object
     */
    under_vol_id = o->under_vol_id;

    ret_value = H5VLobject_specific(o->under_object, loc_params, o->under_vol_id, args, dxpl_id, req);

    /* Check for async request */
    if (req && *req)
        *req = debug__pt_new_obj(*req, under_vol_id);

    return ret_value;
} /* end debug_pt_object_specific() */

/*-------------------------------------------------------------------------
 * Function:    debug_pt_object_optional
 *
 * Purpose:     Perform a connector-specific operation for an object
 *
 * Return:      Success:    0
 *              Failure:    -1
 *
 *-------------------------------------------------------------------------
 */
static herr_t
debug_pt_object_optional(void *obj, const H5VL_loc_params_t *loc_params, H5VL_optional_args_t *args,
                                  hid_t dxpl_id, void **req)
{
    debug_pt_object_t *o = (debug_pt_object_t *)obj;
    herr_t               ret_value;

    ret_value = H5VLobject_optional(o->under_object, loc_params, o->under_vol_id, args, dxpl_id, req);

    /* Check for async request */
    if (req && *req)
        *req = debug__pt_new_obj(*req, o->under_vol_id);

    return ret_value;
} /* end debug_pt_object_optional() */

/*-------------------------------------------------------------------------
 * Function:    debug_pt_introspect_get_conn_cls
 *
 * Purpose:     Query the connector class.
 *
 * Return:      SUCCEED / FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
debug_pt_introspect_get_conn_cls(void *obj, H5VL_get_conn_lvl_t lvl, const H5VL_class_t **conn_cls)
{
    debug_pt_object_t *o = (debug_pt_object_t *)obj;
    herr_t               ret_value;

    /* Check for querying this connector's class */
    if (H5VL_GET_CONN_LVL_CURR == lvl) {
        *conn_cls = &debug_pt_g;
        ret_value = 0;
    } /* end if */
    else
        ret_value = H5VLintrospect_get_conn_cls(o->under_object, o->under_vol_id, lvl, conn_cls);

    return ret_value;
} /* end debug_pt_introspect_get_conn_cls() */

/*-------------------------------------------------------------------------
 * Function:    debug_pt_introspect_get_cap_flags
 *
 * Purpose:     Query the capability flags for this connector and any
 *              underlying connector(s).
 *
 * Return:      SUCCEED / FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
debug_pt_introspect_get_cap_flags(const void *_info, uint64_t *cap_flags)
{
    const debug_pt_info_t *info = (const debug_pt_info_t *)_info;
    herr_t                          ret_value;

    /* Make sure the underneath VOL of this pass-through VOL is specified */
    if (!info) {
        printf("\n%s line %d in %s: info for pass-through VOL can't be null\n",
               __FILE__, __LINE__, __func__);
        return -1;
    }

    if (H5Iis_valid(info->under_vol_id) <= 0) {
        printf("\n%s line %d in %s: not a valid underneath VOL ID for pass-through VOL\n",
               __FILE__, __LINE__, __func__);
        return -1;
    }

    /* Invoke the query on the underlying VOL connector */
    ret_value = H5VLintrospect_get_cap_flags(info->under_vol_info, info->under_vol_id, cap_flags);

    /* Bitwise OR our capability flags in */
    if (ret_value >= 0)
        *cap_flags |= debug_pt_g.cap_flags;

    return ret_value;
} /* end debug_pt_introspect_get_cap_flags() */

/*-------------------------------------------------------------------------
 * Function:    debug_pt_introspect_opt_query
 *
 * Purpose:     Query if an optional operation is supported by this connector
 *
 * Return:      SUCCEED / FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
debug_pt_introspect_opt_query(void *obj, H5VL_subclass_t cls, int opt_type, uint64_t *flags)
{
    debug_pt_object_t *o = (debug_pt_object_t *)obj;
    herr_t               ret_value;

    ret_value = H5VLintrospect_opt_query(o->under_object, o->under_vol_id, cls, opt_type, flags);

    return ret_value;
} /* end debug_pt_introspect_opt_query() */

/*-------------------------------------------------------------------------
 * Function:    debug_pt_request_wait
 *
 * Purpose:     Wait (with a timeout) for an async operation to complete
 *
 * Note:        Releases the request if the operation has completed and the
 *              connector callback succeeds
 *
 * Return:      Success:    0
 *              Failure:    -1
 *
 *-------------------------------------------------------------------------
 */
static herr_t
debug_pt_request_wait(void *obj, uint64_t timeout, H5VL_request_status_t *status)
{
    debug_pt_object_t *o = (debug_pt_object_t *)obj;
    herr_t               ret_value;

    ret_value = H5VLrequest_wait(o->under_object, o->under_vol_id, timeout, status);

    if (ret_value >= 0 && *status != H5ES_STATUS_IN_PROGRESS)
        debug__pt_free_obj(o);

    return ret_value;
} /* end debug_pt_request_wait() */

/*-------------------------------------------------------------------------
 * Function:    debug_pt_request_notify
 *
 * Purpose:     Registers a user callback to be invoked when an asynchronous
 *              operation completes
 *
 * Note:        Releases the request, if connector callback succeeds
 *
 * Return:      Success:    0
 *              Failure:    -1
 *
 *-------------------------------------------------------------------------
 */
static herr_t
debug_pt_request_notify(void *obj, H5VL_request_notify_t cb, void *ctx)
{
    debug_pt_object_t *o = (debug_pt_object_t *)obj;
    herr_t               ret_value;

    ret_value = H5VLrequest_notify(o->under_object, o->under_vol_id, cb, ctx);

    if (ret_value >= 0)
        debug__pt_free_obj(o);

    return ret_value;
} /* end debug_pt_request_notify() */

/*-------------------------------------------------------------------------
 * Function:    debug_pt_request_cancel
 *
 * Purpose:     Cancels an asynchronous operation
 *
 * Note:        Releases the request, if connector callback succeeds
 *
 * Return:      Success:    0
 *              Failure:    -1
 *
 *-------------------------------------------------------------------------
 */
static herr_t
debug_pt_request_cancel(void *obj, H5VL_request_status_t *status)
{
    debug_pt_object_t *o = (debug_pt_object_t *)obj;
    herr_t               ret_value;

    ret_value = H5VLrequest_cancel(o->under_object, o->under_vol_id, status);

    if (ret_value >= 0)
        debug__pt_free_obj(o);

    return ret_value;
} /* end debug_pt_request_cancel() */

/*-------------------------------------------------------------------------
 * Function:    debug_pt_request_specific
 *
 * Purpose:     Specific operation on a request
 *
 * Return:      Success:    0
 *              Failure:    -1
 *
 *-------------------------------------------------------------------------
 */
static herr_t
debug_pt_request_specific(void *obj, H5VL_request_specific_args_t *args)
{
    debug_pt_object_t *o         = (debug_pt_object_t *)obj;
    herr_t               ret_value = -1;

    ret_value = H5VLrequest_specific(o->under_object, o->under_vol_id, args);

    return ret_value;
} /* end debug_pt_request_specific() */

/*-------------------------------------------------------------------------
 * Function:    debug_pt_request_optional
 *
 * Purpose:     Perform a connector-specific operation for a request
 *
 * Return:      Success:    0
 *              Failure:    -1
 *
 *-------------------------------------------------------------------------
 */
static herr_t
debug_pt_request_optional(void *obj, H5VL_optional_args_t *args)
{
    debug_pt_object_t *o = (debug_pt_object_t *)obj;
    herr_t               ret_value;

    ret_value = H5VLrequest_optional(o->under_object, o->under_vol_id, args);

    return ret_value;
} /* end debug_pt_request_optional() */

/*-------------------------------------------------------------------------
 * Function:    debug_pt_request_free
 *
 * Purpose:     Releases a request, allowing the operation to complete without
 *              application tracking
 *
 * Return:      Success:    0
 *              Failure:    -1
 *
 *-------------------------------------------------------------------------
 */
static herr_t
debug_pt_request_free(void *obj)
{
    debug_pt_object_t *o = (debug_pt_object_t *)obj;
    herr_t               ret_value;

    ret_value = H5VLrequest_free(o->under_object, o->under_vol_id);

    if (ret_value >= 0)
        debug__pt_free_obj(o);

    return ret_value;
} /* end debug_pt_request_free() */

/*-------------------------------------------------------------------------
 * Function:    debug_pt_blob_put
 *
 * Purpose:     Handles the blob 'put' callback
 *
 * Return:      SUCCEED / FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
debug_pt_blob_put(void *obj, const void *buf, size_t size, void *blob_id, void *ctx)
{
    debug_pt_object_t *o = (debug_pt_object_t *)obj;
    herr_t               ret_value;

    ret_value = H5VLblob_put(o->under_object, o->under_vol_id, buf, size, blob_id, ctx);

    return ret_value;
} /* end debug_pt_blob_put() */

/*-------------------------------------------------------------------------
 * Function:    debug_pt_blob_get
 *
 * Purpose:     Handles the blob 'get' callback
 *
 * Return:      SUCCEED / FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
debug_pt_blob_get(void *obj, const void *blob_id, void *buf, size_t size, void *ctx)
{
    debug_pt_object_t *o = (debug_pt_object_t *)obj;
    herr_t               ret_value;

    ret_value = H5VLblob_get(o->under_object, o->under_vol_id, blob_id, buf, size, ctx);

    return ret_value;
} /* end debug_pt_blob_get() */

/*-------------------------------------------------------------------------
 * Function:    debug_pt_blob_specific
 *
 * Purpose:     Handles the blob 'specific' callback
 *
 * Return:      SUCCEED / FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
debug_pt_blob_specific(void *obj, void *blob_id, H5VL_blob_specific_args_t *args)
{
    debug_pt_object_t *o = (debug_pt_object_t *)obj;
    herr_t               ret_value;

    ret_value = H5VLblob_specific(o->under_object, o->under_vol_id, blob_id, args);

    return ret_value;
} /* end debug_pt_blob_specific() */

/*-------------------------------------------------------------------------
 * Function:    debug_pt_blob_optional
 *
 * Purpose:     Handles the blob 'optional' callback
 *
 * Return:      SUCCEED / FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
debug_pt_blob_optional(void *obj, void *blob_id, H5VL_optional_args_t *args)
{
    debug_pt_object_t *o = (debug_pt_object_t *)obj;
    herr_t               ret_value;

    ret_value = H5VLblob_optional(o->under_object, o->under_vol_id, blob_id, args);

    return ret_value;
} /* end debug_pt_blob_optional() */

/*---------------------------------------------------------------------------
 * Function:    debug_pt_object_token_cmp
 *
 * Purpose:     Compare two of the connector's object tokens, setting
 *              *cmp_value, following the same rules as strcmp().
 *
 * Return:      Success:    0
 *              Failure:    -1
 *
 *---------------------------------------------------------------------------
 */
static herr_t
debug_pt_token_cmp(void *obj, const H5O_token_t *token1, const H5O_token_t *token2, int *cmp_value)
{
    debug_pt_object_t *o = (debug_pt_object_t *)obj;
    herr_t               ret_value;

    /* Sanity checks */
    assert(obj);
    assert(token1);
    assert(token2);
    assert(cmp_value);

    ret_value = H5VLtoken_cmp(o->under_object, o->under_vol_id, token1, token2, cmp_value);

    return ret_value;
} /* end debug_pt_object_token_cmp() */

/*---------------------------------------------------------------------------
 * Function:    debug_pt_object_token_to_str
 *
 * Purpose:     Serialize the connector's object token into a string.
 *
 * Return:      Success:    0
 *              Failure:    -1
 *
 *---------------------------------------------------------------------------
 */
static herr_t
debug_pt_token_to_str(void *obj, H5I_type_t obj_type, const H5O_token_t *token, char **token_str)
{
    debug_pt_object_t *o = (debug_pt_object_t *)obj;
    herr_t               ret_value;

    /* Sanity checks */
    assert(obj);
    assert(token);
    assert(token_str);

    ret_value = H5VLtoken_to_str(o->under_object, obj_type, o->under_vol_id, token, token_str);

    return ret_value;
} /* end debug_pt_object_token_to_str() */

/*---------------------------------------------------------------------------
 * Function:    debug_pt_object_token_from_str
 *
 * Purpose:     Deserialize the connector's object token from a string.
 *
 * Return:      Success:    0
 *              Failure:    -1
 *
 *---------------------------------------------------------------------------
 */
static herr_t
debug_pt_token_from_str(void *obj, H5I_type_t obj_type, const char *token_str, H5O_token_t *token)
{
    debug_pt_object_t *o = (debug_pt_object_t *)obj;
    herr_t               ret_value;

    /* Sanity checks */
    assert(obj);
    assert(token);
    assert(token_str);

    ret_value = H5VLtoken_from_str(o->under_object, obj_type, o->under_vol_id, token_str, token);

    return ret_value;
} /* end debug_pt_object_token_from_str() */

/*-------------------------------------------------------------------------
 * Function:    debug_pt_optional
 *
 * Purpose:     Handles the generic 'optional' callback
 *
 * Return:      SUCCEED / FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
debug_pt_optional(void *obj, H5VL_optional_args_t *args, hid_t dxpl_id, void **req)
{
    debug_pt_object_t *o = (debug_pt_object_t *)obj;
    herr_t               ret_value;

    ret_value = H5VLoptional(o->under_object, o->under_vol_id, args, dxpl_id, req);

    return ret_value;
} /* end debug_pt_optional() */

/* These two functions are necessary to load this plugin using
 * the HDF5 library.
 */

H5PL_type_t H5PLget_plugin_type(void) { return H5PL_TYPE_VOL; }
const void *H5PLget_plugin_info(void) { return &debug_pt_g; }
