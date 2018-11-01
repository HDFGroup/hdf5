/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Purpose:     The Virtual Object Layer as described in documentation.
 *              The pupose is to provide an abstraction on how to access the
 *              underlying HDF5 container, whether in a local file with
 *              a specific file format, or remotely on other machines, etc...
 */


/****************/
/* Module Setup */
/****************/

#include "H5VLmodule.h"         /* This source code file is part of the H5VL module */


/***********/
/* Headers */
/***********/

#include "H5private.h"          /* Generic Functions                                */
#include "H5Eprivate.h"         /* Error handling                                   */
#include "H5Fprivate.h"		/* File access				            */
#include "H5Iprivate.h"         /* IDs                                              */
#include "H5MMprivate.h"        /* Memory management                                */
#include "H5Pprivate.h"         /* Property lists                                   */
#include "H5VLpkg.h"            /* Virtual Object Layer                             */


/****************/
/* Local Macros */
/****************/


/******************/
/* Local Typedefs */
/******************/


/********************/
/* Package Typedefs */
/********************/


/********************/
/* Local Prototypes */
/********************/
static void *H5VL__attr_create(void *obj, H5VL_loc_params_t loc_params,
    const H5VL_class_t *cls, const char *name, hid_t acpl_id, hid_t aapl_id,
    hid_t dxpl_id, void **req);
static void *H5VL__attr_open(void *obj, H5VL_loc_params_t loc_params,
    const H5VL_class_t *cls, const char *name, hid_t aapl_id, hid_t dxpl_id,
    void **req);
static herr_t H5VL__attr_read(void *obj, const H5VL_class_t *cls, hid_t mem_type_id,
    void *buf, hid_t dxpl_id, void **req);
static herr_t H5VL__attr_write(void *obj, const H5VL_class_t *cls, hid_t mem_type_id,
    const void *buf, hid_t dxpl_id, void **req);
static herr_t H5VL__attr_get(void *obj, const H5VL_class_t *cls, H5VL_attr_get_t get_type, 
    hid_t dxpl_id, void **req, va_list arguments);
static herr_t H5VL__attr_specific(void *obj, H5VL_loc_params_t loc_params,
    const H5VL_class_t *cls, H5VL_attr_specific_t specific_type, hid_t dxpl_id,
    void **req, va_list arguments);
static herr_t H5VL__attr_optional(void *obj, const H5VL_class_t *cls, hid_t dxpl_id,
    void **req, va_list arguments);
static herr_t H5VL__attr_close(void *obj, const H5VL_class_t *cls, hid_t dxpl_id,
    void **req);
static void *H5VL__dataset_create(void *obj, H5VL_loc_params_t loc_params,
    const H5VL_class_t *cls, const char *name, hid_t dcpl_id, hid_t dapl_id,
    hid_t dxpl_id, void **req);
static void *H5VL__dataset_open(void *obj, H5VL_loc_params_t loc_params,
    const H5VL_class_t *cls, const char *name, hid_t dapl_id, hid_t dxpl_id,
    void **req);
static herr_t H5VL__dataset_read(void *dset, const H5VL_class_t *cls,
    hid_t mem_type_id, hid_t mem_space_id, hid_t file_space_id, hid_t plist_id,
    void *buf, void **req);
static herr_t H5VL__dataset_write(void *obj, const H5VL_class_t *cls,
    hid_t mem_type_id, hid_t mem_space_id, hid_t file_space_id, hid_t plist_id,
    const void *buf, void **req);
static herr_t H5VL__dataset_get(void *obj, const H5VL_class_t *cls, H5VL_dataset_get_t get_type, 
    hid_t dxpl_id, void **req, va_list arguments);
static herr_t H5VL__dataset_specific(void *obj, const H5VL_class_t *cls,
    H5VL_dataset_specific_t specific_type, hid_t dxpl_id, void **req, va_list arguments);
static herr_t H5VL__dataset_optional(void *obj, const H5VL_class_t *cls,
    hid_t dxpl_id, void **req, va_list arguments);
static herr_t H5VL__dataset_close(void *obj, const H5VL_class_t *cls, hid_t dxpl_id,
    void **req);
static herr_t H5VL__file_get(void *obj, const H5VL_class_t *cls, H5VL_file_get_t get_type, 
    hid_t dxpl_id, void **req, va_list arguments);
static herr_t H5VL__file_specific(void *obj, const H5VL_class_t *cls, H5VL_file_specific_t specific_type, 
    hid_t dxpl_id, void **req, va_list arguments);
static herr_t H5VL__file_optional(void *obj, const H5VL_class_t *cls, hid_t dxpl_id,
    void **req, va_list arguments);
static herr_t H5VL__file_close(void *obj, const H5VL_class_t *cls, hid_t dxpl_id,
    void **req);
static void *H5VL__group_create(void *obj, H5VL_loc_params_t loc_params,
    const H5VL_class_t *cls, const char *name, hid_t gcpl_id, hid_t gapl_id,
    hid_t dxpl_id, void **req);
static void *H5VL__group_open(void *obj, H5VL_loc_params_t loc_params,
    const H5VL_class_t *cls, const char *name, hid_t gapl_id, hid_t dxpl_id,
    void **req);
static herr_t H5VL__group_get(void *obj, const H5VL_class_t *cls, H5VL_group_get_t get_type, 
    hid_t dxpl_id, void **req, va_list arguments);
static herr_t H5VL__group_specific(void *obj, const H5VL_class_t *cls, H5VL_group_specific_t specific_type, 
    hid_t dxpl_id, void **req, va_list arguments);
static herr_t H5VL__group_optional(void *obj, const H5VL_class_t *cls, hid_t dxpl_id,
    void **req, va_list arguments);
static herr_t H5VL__group_close(void *obj, const H5VL_class_t *cls,
    hid_t dxpl_id, void **req);
static herr_t H5VL__link_create(H5VL_link_create_type_t create_type, void *obj,
    H5VL_loc_params_t loc_params, const H5VL_class_t *cls, hid_t lcpl_id,
    hid_t lapl_id, hid_t dxpl_id, void **req);
static herr_t H5VL__link_copy(void *src_obj, H5VL_loc_params_t loc_params1,
    void *dst_obj, H5VL_loc_params_t loc_params2, const H5VL_class_t *cls,
    hid_t lcpl_id, hid_t lapl_id, hid_t dxpl_id, void **req);
static herr_t H5VL__link_move(void *src_obj, H5VL_loc_params_t loc_params1,
    void *dst_obj, H5VL_loc_params_t loc_params2, const H5VL_class_t *cls,
    hid_t lcpl_id, hid_t lapl_id, hid_t dxpl_id, void **req);
static herr_t H5VL__link_get(void *obj, H5VL_loc_params_t loc_params,
    const H5VL_class_t *cls, H5VL_link_get_t get_type, hid_t dxpl_id,
    void **req, va_list arguments);
static herr_t H5VL__link_specific(void *obj, H5VL_loc_params_t loc_params,
    const H5VL_class_t *cls, H5VL_link_specific_t specific_type, hid_t dxpl_id,
    void **req, va_list arguments);
static herr_t H5VL__link_optional(void *obj, const H5VL_class_t *cls, hid_t dxpl_id,
    void **req, va_list arguments);


/*********************/
/* Package Variables */
/*********************/


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/



/*-------------------------------------------------------------------------
 * Function:    H5VLinitialize
 *
 * Purpose:     Calls the plugin-specific callback to initialize the plugin.
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLinitialize(hid_t plugin_id, hid_t vipl_id)
{
    H5VL_class_t *cls;                  /* VOL plugin's class struct */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API_NOINIT
    H5TRACE2("e", "ii", plugin_id, vipl_id);

    /* Check args */
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    /* Invoke class' callback, if there is one */
    if(cls->initialize && cls->initialize(vipl_id) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTCLOSEOBJ, FAIL, "VOL plugin did not initialize")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLinitialize() */


/*-------------------------------------------------------------------------
 * Function:    H5VLterminate
 *
 * Purpose:     Calls the plugin-specific callback to terminate the plugin.
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLterminate(hid_t plugin_id)
{
    H5VL_class_t *cls;                  /* VOL plugin's class struct */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API_NOINIT
    H5TRACE1("e", "i", plugin_id);

    /* Check args */
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    /* Invoke class' callback, if there is one */
    if(cls->terminate && cls->terminate() < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTCLOSEOBJ, FAIL, "VOL plugin did not terminate cleanly")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLterminate() */


/*---------------------------------------------------------------------------
 * Function:    H5VLget_cap_flags
 *
 * Purpose:     Retrieves the capability flag for a plugin
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *---------------------------------------------------------------------------
 */
herr_t
H5VLget_cap_flags(hid_t plugin_id, unsigned *cap_flags)
{
    H5VL_class_t *cls;                  /* VOL plugin's class struct */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API_NOINIT
    H5TRACE2("e", "i*Iu", plugin_id, cap_flags);

    /* Check args */
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    /* Retrieve capability flags */
    if(cap_flags)
        *cap_flags = cls->cap_flags;

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* H5VLget_cap_flags */


/*-------------------------------------------------------------------------
 * Function:    H5VL_copy_plugin_info
 *
 * Purpose:     Copy the VOL info for a plugin
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_copy_plugin_info(const H5VL_class_t *plugin, void **dst_info,
    const void *src_info)
{
    void *new_plugin_info = NULL;       /* Copy of plugin info */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(plugin);

    /* Check for actual source info */
    if(src_info) {
        /* Allow the plugin to copy or do it ourselves */
        if(plugin->info_copy) {
            if(NULL == (new_plugin_info = (plugin->info_copy)(src_info)))
                HGOTO_ERROR(H5E_VOL, H5E_CANTCOPY, FAIL, "plugin info copy callback failed")
        } /* end if */
        else if(plugin->info_size > 0) {
            if(NULL == (new_plugin_info = H5MM_malloc(plugin->info_size)))
                HGOTO_ERROR(H5E_VOL, H5E_CANTALLOC, FAIL, "plugin info allocation failed")
            HDmemcpy(new_plugin_info, src_info, plugin->info_size);
        } /* end else-if */
        else
            HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "no way to copy plugin info")
    } /* end if */

    /* Set the plugin info for the copy */
    *dst_info = new_plugin_info;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_copy_plugin_info() */


/*---------------------------------------------------------------------------
 * Function:    H5VLcopy_plugin_info
 *
 * Purpose:     Copies a VOL plugin's info object
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *---------------------------------------------------------------------------
 */
herr_t
H5VLcopy_plugin_info(hid_t plugin_id, void **dst_vol_info, void *src_vol_info)
{
    H5VL_class_t *cls;                  /* VOL plugin's class struct */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API_NOINIT
    H5TRACE3("e", "i**x*x", plugin_id, dst_vol_info, src_vol_info);

    /* Check args and get class pointer */
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    /* Copy the VOL plugin's info object */
    if(H5VL_copy_plugin_info(cls, dst_vol_info, src_vol_info) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTCOPY, FAIL, "unable to copy VOL plugin info object")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* H5VLcopy_plugin_info() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_cmp_plugin_info
 *
 * Purpose:     Compare VOL info for a plugin
 *
 * Return:      Positive if VALUE1 is greater than VALUE2, negative if
 *              VALUE2 is greater than VALUE1 and zero if VALUE1 and
 *              VALUE2 are equal.
 *
 *-------------------------------------------------------------------------
 */
int
H5VL_cmp_plugin_info(const H5VL_class_t *plugin, const void *info1,
    const void *info2)
{
    int cmp_value;              /* Value from comparison */
    int ret_value = 0;          /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(plugin);

    /* Use the class's info comparison routine to compare the info objects,
     * if there is a a callback, otherwise just compare the info objects as
     * memory buffers
     */
    if(plugin->info_cmp) {
        if(0 != (cmp_value = (plugin->info_cmp)(info1, info2)))
            HGOTO_DONE(cmp_value);
    } /* end if */
    else {
        if(info1 == NULL && info2 != NULL)
            HGOTO_DONE(-1);
        if(info1 != NULL && info2 == NULL)
            HGOTO_DONE(1);
        if(info1) {
            HDassert(plugin->info_size > 0);
            if(0 != (cmp_value = HDmemcmp(info1, info2, plugin->info_size)))
                HGOTO_DONE(cmp_value);
        } /* end if */
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_cmp_plugin_info() */


/*---------------------------------------------------------------------------
 * Function:    H5VLcmp_plugin_info
 *
 * Purpose:     Compares two plugin info objects
 *
 * Note:	Both info objects must be from the same VOL plugin class
 *
 * Return:      Success:    Non-negative, with *cmp set to positive if
 *			    info1 is greater than info2, negative if info2
 *			    is greater than info1 and zero if info1 and info2
 *			    are equal.
 *              Failure:    Negative
 *
 *---------------------------------------------------------------------------
 */
herr_t
H5VLcmp_plugin_info(int *cmp, hid_t plugin_id, const void *info1, const void *info2)
{
    H5VL_class_t *cls;                  /* VOL plugin's class struct */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE4("e", "*Isi*x*x", cmp, plugin_id, info1, info2);

    /* Check args and get class pointer */
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    /* Compare the two VOL plugin info objects */
    if(cmp)
        *cmp = H5VL_cmp_plugin_info(cls, info1, info2);

done:
    FUNC_LEAVE_API(ret_value)
} /* H5VLcmp_plugin_info() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_free_plugin_info
 *
 * Purpose:     Free VOL info for a plugin
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_free_plugin_info(const H5VL_class_t *plugin, void *info)
{
    herr_t ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(plugin);

    /* Only free info object, if it's non-NULL */
    if(info) {
        /* Allow the plugin to free info or do it ourselves */
        if(plugin->info_free) {
            if((plugin->info_free)(info) < 0)
                HGOTO_ERROR(H5E_VOL, H5E_CANTRELEASE, FAIL, "plugin info free request failed")
        } /* end if */
        else
            H5MM_xfree(info);
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_free_plugin_info() */


/*---------------------------------------------------------------------------
 * Function:    H5VLfree_plugin_info
 *
 * Purpose:     Free VOL plugin info object
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *---------------------------------------------------------------------------
 */
herr_t
H5VLfree_plugin_info(hid_t plugin_id, void *info)
{
    H5VL_class_t *cls;                  /* VOL plugin's class struct */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API_NOINIT
    H5TRACE2("e", "i*x", plugin_id, info);

    /* Check args and get class pointer */
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    /* Free the VOL plugin info object */
    if(H5VL_free_plugin_info(cls, info) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTRELEASE, FAIL, "unable to release VOL plugin info object")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* H5VLfree_plugin_info() */


/*---------------------------------------------------------------------------
 * Function:    H5VLget_object
 *
 * Purpose:     Retrieves an underlying object.
 *
 * Return:      Success:    Non-NULL
 *              Failure:    NULL
 *
 *---------------------------------------------------------------------------
 */
void *
H5VLget_object(void *obj, hid_t plugin_id)
{
    H5VL_class_t *cls;                  /* VOL plugin's class struct */
    void *ret_value = NULL;             /* Return value */

    FUNC_ENTER_API_NOINIT
    H5TRACE2("*x", "*xi", obj, plugin_id);

    /* Check args */
    if(NULL == obj)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "invalid object")
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a VOL plugin ID")

    /* Check for 'get_object' callback in plugin */
    if(cls->get_object)
        ret_value = (cls->get_object)(obj);
    else
        ret_value = obj;

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* H5VLget_object */


/*-------------------------------------------------------------------------
 * Function:    H5VL_get_wrap_ctx
 *
 * Purpose:     Retrieve the VOL object wrapping context for a plugin
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_get_wrap_ctx(const H5VL_class_t *plugin, void *obj, void **wrap_ctx)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(plugin);
    HDassert(obj);
    HDassert(wrap_ctx);

    /* Allow the plugin to copy or do it ourselves */
    if(plugin->get_wrap_ctx) {
        /* Sanity check */
        HDassert(plugin->free_wrap_ctx);

        /* Invoke plugin's callback */
        if((plugin->get_wrap_ctx)(obj, wrap_ctx) < 0)
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "plugin wrap context callback failed")
    } /* end if */
    else
        *wrap_ctx = NULL;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_get_wrap_ctx() */


/*---------------------------------------------------------------------------
 * Function:    H5VLget_wrap_ctx
 *
 * Purpose:     Get a VOL plugin's object wrapping context
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *---------------------------------------------------------------------------
 */
herr_t
H5VLget_wrap_ctx(void *obj, hid_t plugin_id, void **wrap_ctx)
{
    H5VL_class_t *cls;                  /* VOL plugin's class struct */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API_NOINIT
    H5TRACE3("e", "*xi**x", obj, plugin_id, wrap_ctx);

    /* Check args and get class pointer */
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    /* Get the VOL plugin's object wrapper */
    if(H5VL_get_wrap_ctx(cls, obj, wrap_ctx) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "unable to retrieve VOL plugin object wrap context")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* H5VLget_wrap_ctx() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_free_wrap_ctx
 *
 * Purpose:     Free object wrapping context for a plugin
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_free_wrap_ctx(const H5VL_class_t *plugin, void *wrap_ctx)
{
    herr_t ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(plugin);

    /* Only free wrap context, if it's non-NULL */
    if(wrap_ctx) {
        /* Free the plugin's object wrapping context */
        if((plugin->free_wrap_ctx)(wrap_ctx) < 0)
            HGOTO_ERROR(H5E_VOL, H5E_CANTRELEASE, FAIL, "plugin wrap context free request failed")
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_free_plugin_info() */


/*---------------------------------------------------------------------------
 * Function:    H5VLfree_wrap_ctx
 *
 * Purpose:     Release a VOL plugin's object wrapping context
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *---------------------------------------------------------------------------
 */
herr_t
H5VLfree_wrap_ctx(void *wrap_ctx, hid_t plugin_id)
{
    H5VL_class_t *cls;                  /* VOL plugin's class struct */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API_NOINIT
    H5TRACE2("e", "*xi", wrap_ctx, plugin_id);

    /* Check args and get class pointer */
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    /* Release the VOL plugin's object wrapper */
    if(H5VL_free_wrap_ctx(cls, wrap_ctx) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTRELEASE, FAIL, "unable to release VOL plugin object wrap context")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* H5VLfree_wrap_ctx() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_wrap_object
 *
 * Purpose:     Wrap an object with plugin
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
void *
H5VL_wrap_object(const H5VL_class_t *plugin, void *wrap_ctx, void *obj)
{
    void *ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(NULL)

    /* Sanity checks */
    HDassert(plugin);
    HDassert(obj);

    /* Only wrap object if there's a wrap context */
    if(wrap_ctx) {
        /* Ask the plugin to wrap the object */
        if(NULL == (ret_value = (plugin->wrap_object)(obj, wrap_ctx)))
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, NULL, "can't wrap object")
    } /* end if */
    else
        ret_value = obj;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_wrap_object() */


/*---------------------------------------------------------------------------
 * Function:    H5VLwrap_object
 *
 * Purpose:     Asks a plugin to wrap an underlying object.
 *
 * Return:      Success:    Non-NULL
 *              Failure:    NULL
 *
 *---------------------------------------------------------------------------
 */
void *
H5VLwrap_object(void *obj, hid_t plugin_id, void *wrap_ctx)
{
    H5VL_class_t *cls;          /* VOL plugin's class struct */
    void *ret_value = NULL;     /* Return value */

    FUNC_ENTER_API_NOINIT
    H5TRACE3("*x", "*xi*x", obj, plugin_id, wrap_ctx);

    /* Check args and get class pointer */
    if(NULL == obj)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "invalid object")
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a VOL plugin ID")

    /* Wrap the object */
    if(NULL == (ret_value = H5VL_wrap_object(cls, wrap_ctx, obj)))
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, NULL, "unable to wrap object")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* H5VLwrap_object */


/*-------------------------------------------------------------------------
 * Function:    H5VL__attr_create
 *
 * Purpose:     Creates an attribute through the VOL
 *
 * Return:      Success: Pointer to the new attribute 
 *              Failure: NULL
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL__attr_create(void *obj, H5VL_loc_params_t loc_params, const H5VL_class_t *cls,
    const char *name, hid_t acpl_id, hid_t aapl_id, hid_t dxpl_id, void **req)
{
    void *ret_value = NULL;     /* Return value */

    FUNC_ENTER_STATIC

    /* Check if the corresponding VOL callback exists */
    if(NULL == cls->attr_cls.create)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, NULL, "VOL plugin has no 'attr create' method")

    /* Call the corresponding VOL callback */
    if(NULL == (ret_value = (cls->attr_cls.create)(obj, loc_params, name, acpl_id, aapl_id, dxpl_id, req)))
        HGOTO_ERROR(H5E_VOL, H5E_CANTCREATE, NULL, "attribute create failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_attr_create() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_attr_create
 *
 * Purpose:     Creates an attribute through the VOL
 *
 * Return:      Success: Pointer to the new attribute 
 *              Failure: NULL
 *
 *-------------------------------------------------------------------------
 */
void *
H5VL_attr_create(const H5VL_object_t *vol_obj, H5VL_loc_params_t loc_params, 
    const char *name, hid_t acpl_id, hid_t aapl_id, hid_t dxpl_id, void **req)
{
    hbool_t vol_wrapper_set = FALSE;    /* Whether the VOL object wrapping context was set up */
    void *ret_value = NULL;             /* Return value */

    FUNC_ENTER_NOAPI(NULL)

    /* Set wrapper info in API context */
    if(H5VL_set_vol_wrapper(vol_obj->data, vol_obj->plugin) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTSET, NULL, "can't set VOL wrapper info")
    vol_wrapper_set = TRUE;

    /* Call the corresponding internal VOL routine */
    if(NULL == (ret_value = H5VL__attr_create(vol_obj->data, loc_params, vol_obj->plugin->cls, name, acpl_id, aapl_id, dxpl_id, req)))
        HGOTO_ERROR(H5E_VOL, H5E_CANTCREATE, NULL, "attribute create failed")

done:
    /* Reset object wrapping info in API context */
    if(vol_wrapper_set && H5VL_reset_vol_wrapper() < 0)
        HDONE_ERROR(H5E_VOL, H5E_CANTRESET, NULL, "can't reset VOL wrapper info")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_attr_create() */


/*-------------------------------------------------------------------------
 * Function:    H5VLattr_create
 *
 * Purpose:     Creates an attribute
 *
 * Return:      Success:    Pointer to the new attribute
 *              Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
void *
H5VLattr_create(void *obj, H5VL_loc_params_t loc_params, hid_t plugin_id,
    const char *name, hid_t acpl_id, hid_t aapl_id, hid_t dxpl_id, void **req)
{
    H5VL_class_t *cls;          /* VOL plugin's class struct */
    void *ret_value = NULL;     /* Return value */

    FUNC_ENTER_API_NOINIT
    H5TRACE8("*x", "*xxi*siii**x", obj, loc_params, plugin_id, name, acpl_id,
             aapl_id, dxpl_id, req);

    /* Check args and get class pointer */
    if(NULL == obj)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "invalid object")
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a VOL plugin ID")

    /* Call the corresponding internal VOL routine */
    if(NULL == (ret_value = H5VL__attr_create(obj, loc_params, cls, name, acpl_id, aapl_id, dxpl_id, req)))
        HGOTO_ERROR(H5E_VOL, H5E_CANTCREATE, NULL, "unable to create attribute")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLattr_create() */


/*-------------------------------------------------------------------------
 * Function:	H5VL__attr_open
 *
 * Purpose:	Opens an attribute through the VOL
 *
 * Return:      Success: Pointer to the attribute
 *		Failure: NULL
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL__attr_open(void *obj, H5VL_loc_params_t loc_params, const H5VL_class_t *cls, const char *name, 
    hid_t aapl_id, hid_t dxpl_id, void **req)
{
    void *ret_value = NULL;     /* Return value */

    FUNC_ENTER_STATIC

    /* Check if the corresponding VOL callback exists */
    if(NULL == cls->attr_cls.open)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, NULL, "VOL plugin has no 'attr open' method")

    /* Call the corresponding VOL open callback */
    if(NULL == (ret_value = (cls->attr_cls.open)(obj, loc_params, name, aapl_id, dxpl_id, req)))
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPENOBJ, NULL, "attribute open failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__attr_open() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_attr_open
 *
 * Purpose:	Opens an attribute through the VOL
 *
 * Return:      Success: Pointer to the attribute
 *		Failure: NULL
 *
 *-------------------------------------------------------------------------
 */
void *
H5VL_attr_open(const H5VL_object_t *vol_obj, H5VL_loc_params_t loc_params,
    const char *name, hid_t aapl_id, hid_t dxpl_id, void **req)
{
    hbool_t vol_wrapper_set = FALSE;    /* Whether the VOL object wrapping context was set up */
    void *ret_value = NULL;             /* Return value */

    FUNC_ENTER_NOAPI(NULL)

    /* Set wrapper info in API context */
    if(H5VL_set_vol_wrapper(vol_obj->data, vol_obj->plugin) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTSET, NULL, "can't set VOL wrapper info")
    vol_wrapper_set = TRUE;

    /* Call the corresponding internal VOL routine */
    if(NULL == (ret_value = H5VL__attr_open(vol_obj->data, loc_params, vol_obj->plugin->cls, name, aapl_id, dxpl_id, req)))
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPENOBJ, NULL, "attribute open failed")

done:
    /* Reset object wrapping info in API context */
    if(vol_wrapper_set && H5VL_reset_vol_wrapper() < 0)
        HDONE_ERROR(H5E_VOL, H5E_CANTRESET, NULL, "can't reset VOL wrapper info")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_attr_open() */


/*-------------------------------------------------------------------------
 * Function:	H5VLattr_open
 *
 * Purpose:     Opens an attribute
 *
 * Return:      Success:    Pointer to the attribute
 *              Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
void *
H5VLattr_open(void *obj, H5VL_loc_params_t loc_params, hid_t plugin_id,
    const char *name, hid_t aapl_id, hid_t dxpl_id, void **req)
{
    H5VL_class_t *cls;          /* VOL plugin's class struct */
    void *ret_value = NULL;     /* Return value */

    FUNC_ENTER_API_NOINIT
    H5TRACE7("*x", "*xxi*sii**x", obj, loc_params, plugin_id, name, aapl_id,
             dxpl_id, req);

    /* Check args and get class pointer */
    if(NULL == obj)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "invalid object")
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a VOL plugin ID")

    /* Call the corresponding internal VOL routine */
    if(NULL == (ret_value = H5VL__attr_open(obj, loc_params, cls, name, aapl_id, dxpl_id, req)))
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPENOBJ, NULL, "unable to open attribute")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLattr_open() */


/*-------------------------------------------------------------------------
 * Function:	H5VL__attr_read
 *
 * Purpose:	Reads data from attr through the VOL
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL__attr_read(void *obj, const H5VL_class_t *cls, hid_t mem_type_id, void *buf, 
    hid_t dxpl_id, void **req)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_STATIC

    /* Check if the corresponding VOL callback exists */
    if(NULL == cls->attr_cls.read)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no 'attr read' method")

    /* Call the corresponding VOL callback */
    if((cls->attr_cls.read)(obj, mem_type_id, buf, dxpl_id, req) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_READERROR, FAIL, "attribute read failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__attr_read() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_attr_read
 *
 * Purpose:	Reads data from attr through the VOL
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_attr_read(const H5VL_object_t *vol_obj, hid_t mem_type_id, void *buf, 
    hid_t dxpl_id, void **req)
{
    hbool_t vol_wrapper_set = FALSE;    /* Whether the VOL object wrapping context was set up */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Set wrapper info in API context */
    if(H5VL_set_vol_wrapper(vol_obj->data, vol_obj->plugin) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTSET, FAIL, "can't set VOL wrapper info")
    vol_wrapper_set = TRUE;

    /* Call the corresponding internal VOL routine */
    if(H5VL__attr_read(vol_obj->data, vol_obj->plugin->cls, mem_type_id, buf, dxpl_id, req) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_READERROR, FAIL, "attribute read failed")

done:
    /* Reset object wrapping info in API context */
    if(vol_wrapper_set && H5VL_reset_vol_wrapper() < 0)
        HDONE_ERROR(H5E_VOL, H5E_CANTRESET, FAIL, "can't reset VOL wrapper info")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_attr_read() */


/*-------------------------------------------------------------------------
 * Function:    H5VLattr_read
 *
 * Purpose:     Reads data from an attribute
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLattr_read(void *obj, hid_t plugin_id, hid_t mem_type_id, void *buf, hid_t dxpl_id, void **req)
{
    H5VL_class_t *cls;                  /* VOL plugin's class struct */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API_NOINIT

    /* Check args and get class pointer */
    if(NULL == obj)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object")
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    /* Call the corresponding internal VOL routine */
    if(H5VL__attr_read(obj, cls, mem_type_id, buf, dxpl_id, req) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_READERROR, FAIL, "unable to read attribute")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLattr_read() */


/*-------------------------------------------------------------------------
 * Function:	H5VL__attr_write
 *
 * Purpose:	Writes data to attr through the VOL
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL__attr_write(void *obj, const H5VL_class_t *cls, hid_t mem_type_id, const void *buf, 
    hid_t dxpl_id, void **req)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_STATIC

    /* Check if the corresponding VOL callback exists */
    if(NULL == cls->attr_cls.write)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no 'attr write' method")

    /* Call the corresponding VOL callback */
    if((cls->attr_cls.write)(obj, mem_type_id, buf, dxpl_id, req) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_WRITEERROR, FAIL, "write failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__attr_write() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_attr_write
 *
 * Purpose:	Writes data to attr through the VOL
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_attr_write(const H5VL_object_t *vol_obj, hid_t mem_type_id, const void *buf, 
    hid_t dxpl_id, void **req)
{
    hbool_t vol_wrapper_set = FALSE;    /* Whether the VOL object wrapping context was set up */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Set wrapper info in API context */
    if(H5VL_set_vol_wrapper(vol_obj->data, vol_obj->plugin) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTSET, FAIL, "can't set VOL wrapper info")
    vol_wrapper_set = TRUE;

    /* Call the corresponding internal VOL routine */
    if(H5VL__attr_write(vol_obj->data, vol_obj->plugin->cls, mem_type_id, buf, dxpl_id, req) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_WRITEERROR, FAIL, "write failed")

done:
    /* Reset object wrapping info in API context */
    if(vol_wrapper_set && H5VL_reset_vol_wrapper() < 0)
        HDONE_ERROR(H5E_VOL, H5E_CANTRESET, FAIL, "can't reset VOL wrapper info")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_attr_write() */


/*-------------------------------------------------------------------------
 * Function:    H5VLattr_write
 *
 * Purpose:     Writes data to an attribute
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLattr_write(void *obj, hid_t plugin_id, hid_t mem_type_id, const void *buf,
    hid_t dxpl_id, void **req)
{
    H5VL_class_t *cls;                  /* VOL plugin's class struct */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API_NOINIT

    /* Check args and get class pointer */
    if(NULL == obj)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object")
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    /* Call the corresponding internal VOL routine */
    if(H5VL__attr_write(obj, cls, mem_type_id, buf, dxpl_id, req) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_WRITEERROR, FAIL, "unable to write attribute")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLattr_write() */


/*-------------------------------------------------------------------------
 * Function:	H5VL__attr_get
 *
 * Purpose:	Get specific information about the attribute through the VOL
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL__attr_get(void *obj, const H5VL_class_t *cls, H5VL_attr_get_t get_type, 
    hid_t dxpl_id, void **req, va_list arguments)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_STATIC

    /* Check if the corresponding VOL callback exists */
    if(NULL == cls->attr_cls.get)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no 'attr get' method")

    /* Call the corresponding VOL callback */
    if((cls->attr_cls.get)(obj, get_type, dxpl_id, req, arguments) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "attribute get failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__attr_get() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_attr_get
 *
 * Purpose:	Get specific information about the attribute through the VOL
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_attr_get(const H5VL_object_t *vol_obj, H5VL_attr_get_t get_type, 
    hid_t dxpl_id, void **req, ...)
{
    va_list arguments;                  /* Argument list passed from the API call */
    hbool_t arg_started = FALSE;        /* Whether the va_list has been started */
    hbool_t vol_wrapper_set = FALSE;    /* Whether the VOL object wrapping context was set up */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Set wrapper info in API context */
    if(H5VL_set_vol_wrapper(vol_obj->data, vol_obj->plugin) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTSET, FAIL, "can't set VOL wrapper info")
    vol_wrapper_set = TRUE;

    /* Call the corresponding internal VOL routine */
    va_start(arguments, req);
    arg_started = TRUE;
    if(H5VL__attr_get(vol_obj->data, vol_obj->plugin->cls, get_type, dxpl_id, req, arguments) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "attribute get failed")

done:
    /* End access to the va_list, if we started it */
    if(arg_started)
        va_end(arguments);

    /* Reset object wrapping info in API context */
    if(vol_wrapper_set && H5VL_reset_vol_wrapper() < 0)
        HDONE_ERROR(H5E_VOL, H5E_CANTRESET, FAIL, "can't reset VOL wrapper info")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_attr_get() */


/*-------------------------------------------------------------------------
 * Function:    H5VLattr_get
 *
 * Purpose:     Gets information about the attribute
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLattr_get(void *obj, hid_t plugin_id, H5VL_attr_get_t get_type, hid_t dxpl_id,
    void **req, va_list arguments)
{
    H5VL_class_t *cls;                  /* VOL plugin's class struct */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API_NOINIT
    H5TRACE6("e", "*xiVai**xx", obj, plugin_id, get_type, dxpl_id, req, arguments);

    /* Check args and get class pointer */
    if(NULL == obj)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object")
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    /* Call the corresponding internal VOL routine */
    if(H5VL__attr_get(obj, cls, get_type, dxpl_id, req, arguments) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "unable to get attribute information")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLattr_get() */


/*-------------------------------------------------------------------------
 * Function:	H5VL__attr_specific
 *
 * Purpose:	Specific operation on attributes through the VOL
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL__attr_specific(void *obj, H5VL_loc_params_t loc_params, const H5VL_class_t *cls, 
    H5VL_attr_specific_t specific_type, hid_t dxpl_id, void **req,
    va_list arguments)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_STATIC

    /* Check if the corresponding VOL callback exists */
    if(NULL == cls->attr_cls.specific)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no 'attr specific' method")

    /* Call the corresponding VOL callback */
    if((ret_value = (cls->attr_cls.specific)(obj, loc_params, specific_type, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "unable to execute attribute specific callback")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__attr_specific() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_attr_specific
 *
 * Purpose:	Specific operation on attributes through the VOL
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_attr_specific(const H5VL_object_t *vol_obj, H5VL_loc_params_t loc_params,
    H5VL_attr_specific_t specific_type, hid_t dxpl_id, void **req, ...)
{
    va_list arguments;                  /* Argument list passed from the API call */
    hbool_t arg_started = FALSE;        /* Whether the va_list has been started */
    hbool_t vol_wrapper_set = FALSE;    /* Whether the VOL object wrapping context was set up */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Set wrapper info in API context */
    if(H5VL_set_vol_wrapper(vol_obj->data, vol_obj->plugin) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTSET, FAIL, "can't set VOL wrapper info")
    vol_wrapper_set = TRUE;

    /* Call the corresponding internal VOL routine */
    va_start(arguments, req);
    arg_started = TRUE;
    if((ret_value = H5VL__attr_specific(vol_obj->data, loc_params, vol_obj->plugin->cls, specific_type, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "unable to execute attribute specific callback")

done:
    /* End access to the va_list, if we started it */
    if(arg_started)
        va_end(arguments);

    /* Reset object wrapping info in API context */
    if(vol_wrapper_set && H5VL_reset_vol_wrapper() < 0)
        HDONE_ERROR(H5E_VOL, H5E_CANTRESET, FAIL, "can't reset VOL wrapper info")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_attr_specific() */


/*-------------------------------------------------------------------------
 * Function:    H5VLattr_specific
 *
 * Purpose:     Performs a plugin-specific operation on an attribute
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLattr_specific(void *obj, H5VL_loc_params_t loc_params, hid_t plugin_id,
    H5VL_attr_specific_t specific_type, hid_t dxpl_id, void **req, va_list arguments)
{
    H5VL_class_t *cls;                  /* VOL plugin's class struct */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API_NOINIT
    H5TRACE7("e", "*xxiVbi**xx", obj, loc_params, plugin_id, specific_type,
             dxpl_id, req, arguments);

    /* Check args and get class pointer */
    if(NULL == obj)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object")
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    /* Call the corresponding internal VOL routine */
    if((ret_value = H5VL__attr_specific(obj, loc_params, cls, specific_type, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "unable to execute attribute specific callback")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLattr_specific() */


/*-------------------------------------------------------------------------
 * Function:	H5VL__attr_optional
 *
 * Purpose:	Optional operation specific to plugins.
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL__attr_optional(void *obj, const H5VL_class_t *cls, hid_t dxpl_id,
    void **req, va_list arguments)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_STATIC

    /* Check if the corresponding VOL callback exists */
    if(NULL == cls->attr_cls.optional)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no 'attr optional' method")

    /* Call the corresponding VOL callback */
    if((cls->attr_cls.optional)(obj, dxpl_id, req, arguments) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "unable to execute attribute optional callback")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__attr_optional() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_attr_optional
 *
 * Purpose:	Optional operation specific to plugins.
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_attr_optional(const H5VL_object_t *vol_obj, hid_t dxpl_id, void **req, ...)
{
    va_list arguments;                  /* Argument list passed from the API call */
    hbool_t arg_started = FALSE;        /* Whether the va_list has been started */
    hbool_t vol_wrapper_set = FALSE;    /* Whether the VOL object wrapping context was set up */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Set wrapper info in API context */
    if(H5VL_set_vol_wrapper(vol_obj->data, vol_obj->plugin) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTSET, FAIL, "can't set VOL wrapper info")
    vol_wrapper_set = TRUE;

    /* Call the corresponding internal VOL routine */
    va_start(arguments, req);
    arg_started = TRUE;
    if(H5VL__attr_optional(vol_obj->data, vol_obj->plugin->cls, dxpl_id, req, arguments) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "unable to execute attribute optional callback")

done:
    /* End access to the va_list, if we started it */
    if(arg_started)
        va_end(arguments);

    /* Reset object wrapping info in API context */
    if(vol_wrapper_set && H5VL_reset_vol_wrapper() < 0)
        HDONE_ERROR(H5E_VOL, H5E_CANTRESET, FAIL, "can't reset VOL wrapper info")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_attr_optional() */


/*-------------------------------------------------------------------------
 * Function:    H5VLattr_optional
 *
 * Purpose:     Performs an optional plugin-specific operation on an attribute
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLattr_optional(void *obj, hid_t plugin_id, hid_t dxpl_id, void **req, va_list arguments)
{
    H5VL_class_t *cls;                  /* VOL plugin's class struct */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API_NOINIT
    H5TRACE5("e", "*xii**xx", obj, plugin_id, dxpl_id, req, arguments);

    /* Check args and get class pointer */
    if(NULL == obj)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object")
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    /* Call the corresponding internal VOL routine */
    if(H5VL__attr_optional(obj, cls, dxpl_id, req, arguments) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "unable to execute attribute optional callback")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLattr_optional() */


/*-------------------------------------------------------------------------
 * Function:    H5VL__attr_close
 *
 * Purpose:     Closes an attribute through the VOL
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL__attr_close(void *obj, const H5VL_class_t *cls, hid_t dxpl_id, void **req)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_STATIC
            
    /* Check if the corresponding VOL callback exists */
    if(NULL == cls->attr_cls.close)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no 'attr close' method")

    /* Call the corresponding VOL callback */
    if((cls->attr_cls.close)(obj, dxpl_id, req) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTCLOSEOBJ, FAIL, "attribute close failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__attr_close() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_attr_close
 *
 * Purpose:     Closes an attribute through the VOL
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_attr_close(const H5VL_object_t *vol_obj, hid_t dxpl_id, void **req)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(FAIL)
            
    /* Sanity check */
    HDassert(vol_obj);

    /* Call the corresponding internal VOL routine */
    if(H5VL__attr_close(vol_obj->data, vol_obj->plugin->cls, dxpl_id, req) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTCLOSEOBJ, FAIL, "attribute close failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_attr_close() */


/*-------------------------------------------------------------------------
 * Function:    H5VLattr_close
 *
 * Purpose:     Closes an attribute
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLattr_close(void *obj, hid_t plugin_id, hid_t dxpl_id, void **req)
{
    H5VL_class_t *cls;                  /* VOL plugin's class struct */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API_NOINIT
    H5TRACE4("e", "*xii**x", obj, plugin_id, dxpl_id, req);

    /* Check args and get class pointer */
    if(NULL == obj)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object")
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    /* Call the corresponding internal VOL routine */
    if(H5VL__attr_close(obj, cls, dxpl_id, req) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTCLOSEOBJ, FAIL, "unable to close attribute")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLattr_close() */


/*-------------------------------------------------------------------------
 * Function:	H5VL__dataset_create
 *
 * Purpose:	Creates a dataset through the VOL
 *
 * Return:      Success: Pointer to new dataset
 *		Failure: NULL
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL__dataset_create(void *obj, H5VL_loc_params_t loc_params, const H5VL_class_t *cls,
    const char *name, hid_t dcpl_id, hid_t dapl_id, hid_t dxpl_id, void **req)
{
    void *ret_value = NULL;     /* Return value */

    FUNC_ENTER_STATIC

    /* Check if the corresponding VOL callback exists */
    if(NULL == cls->dataset_cls.create)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, NULL, "VOL plugin has no 'dataset create' method")

    /* Call the corresponding VOL callback */
    if(NULL == (ret_value = (cls->dataset_cls.create)(obj, loc_params, name, dcpl_id, dapl_id, dxpl_id, req)))
        HGOTO_ERROR(H5E_VOL, H5E_CANTCREATE, NULL, "create failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__dataset_create() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_dataset_create
 *
 * Purpose:	Creates a dataset through the VOL
 *
 * Return:      Success: Pointer to new dataset
 *		Failure: NULL
 *
 *-------------------------------------------------------------------------
 */
void *
H5VL_dataset_create(const H5VL_object_t *vol_obj, H5VL_loc_params_t loc_params,
    const char *name, hid_t dcpl_id, hid_t dapl_id, hid_t dxpl_id, void **req)
{
    hbool_t vol_wrapper_set = FALSE;    /* Whether the VOL object wrapping context was set up */
    void *ret_value = NULL;             /* Return value */

    FUNC_ENTER_NOAPI(NULL)

    /* Set wrapper info in API context */
    if(H5VL_set_vol_wrapper(vol_obj->data, vol_obj->plugin) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTSET, NULL, "can't set VOL wrapper info")
    vol_wrapper_set = TRUE;

    /* Call the corresponding internal VOL routine */
    if(NULL == (ret_value = H5VL__dataset_create(vol_obj->data, loc_params, vol_obj->plugin->cls, name, dcpl_id, dapl_id, dxpl_id, req)))
        HGOTO_ERROR(H5E_VOL, H5E_CANTCREATE, NULL, "create failed")

done:
    /* Reset object wrapping info in API context */
    if(vol_wrapper_set && H5VL_reset_vol_wrapper() < 0)
        HDONE_ERROR(H5E_VOL, H5E_CANTRESET, NULL, "can't reset VOL wrapper info")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_dataset_create() */


/*-------------------------------------------------------------------------
 * Function:    H5VLdataset_create
 *
 * Purpose:     Creates a dataset
 *
 * Return:      Success:    Pointer to the new dataset
 *              Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
void *
H5VLdataset_create(void *obj, H5VL_loc_params_t loc_params, hid_t plugin_id,
    const char *name, hid_t dcpl_id, hid_t dapl_id, hid_t dxpl_id, void **req)
{
    H5VL_class_t *cls;                  /* VOL plugin's class struct */
    void *ret_value = NULL;             /* Return value */

    FUNC_ENTER_API_NOINIT
    H5TRACE8("*x", "*xxi*siii**x", obj, loc_params, plugin_id, name, dcpl_id,
             dapl_id, dxpl_id, req);

    /* Check args and get class pointer */
    if(NULL == obj)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "invalid object")
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a VOL plugin ID")

    /* Call the corresponding internal VOL routine */
    if(NULL == (ret_value = H5VL__dataset_create(obj, loc_params, cls, name, dcpl_id, dapl_id, dxpl_id, req)))
        HGOTO_ERROR(H5E_VOL, H5E_CANTCREATE, NULL, "unable to create dataset")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLdataset_create() */


/*-------------------------------------------------------------------------
 * Function:	H5VL__dataset_open
 *
 * Purpose:	Opens a dataset through the VOL
 *
 * Return:      Success: Pointer to dataset 
 *		Failure: NULL
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL__dataset_open(void *obj, H5VL_loc_params_t loc_params, const H5VL_class_t *cls, const char *name, 
    hid_t dapl_id, hid_t dxpl_id, void **req)
{
    void *ret_value = NULL;             /* Return value */

    FUNC_ENTER_STATIC

    /* Check if the corresponding VOL callback exists */
    if(NULL == cls->dataset_cls.open)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, NULL, "VOL plugin has no 'dataset open' method")

    /* Call the corresponding VOL callback */
    if(NULL == (ret_value = (cls->dataset_cls.open)(obj, loc_params, name, dapl_id, dxpl_id, req)))
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPENOBJ, NULL, "dataset open failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__dataset_open() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_dataset_open
 *
 * Purpose:	Opens a dataset through the VOL
 *
 * Return:      Success: Pointer to dataset 
 *		Failure: NULL
 *
 *-------------------------------------------------------------------------
 */
void *
H5VL_dataset_open(const H5VL_object_t *vol_obj, H5VL_loc_params_t loc_params,
    const char *name, hid_t dapl_id, hid_t dxpl_id, void **req)
{
    hbool_t vol_wrapper_set = FALSE;    /* Whether the VOL object wrapping context was set up */
    void *ret_value = NULL;             /* Return value */

    FUNC_ENTER_NOAPI(NULL)

    /* Set wrapper info in API context */
    if(H5VL_set_vol_wrapper(vol_obj->data, vol_obj->plugin) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTSET, NULL, "can't set VOL wrapper info")
    vol_wrapper_set = TRUE;

    /* Call the corresponding internal VOL routine */
    if(NULL == (ret_value = H5VL__dataset_open(vol_obj->data, loc_params, vol_obj->plugin->cls, name, dapl_id, dxpl_id, req)))
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPENOBJ, NULL, "dataset open failed")

done:
    /* Reset object wrapping info in API context */
    if(vol_wrapper_set && H5VL_reset_vol_wrapper() < 0)
        HDONE_ERROR(H5E_VOL, H5E_CANTRESET, NULL, "can't reset VOL wrapper info")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_dataset_open() */


/*-------------------------------------------------------------------------
 * Function:    H5VLdataset_open
 *
 * Purpose:     Opens a dataset
 *
 * Return:      Success:    Pointer to the new dataset
 *              Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
void *
H5VLdataset_open(void *obj, H5VL_loc_params_t loc_params, hid_t plugin_id,
    const char *name, hid_t dapl_id, hid_t dxpl_id, void **req)
{
    H5VL_class_t *cls;                  /* VOL plugin's class struct */
    void *ret_value = NULL;             /* Return value */

    FUNC_ENTER_API_NOINIT
    H5TRACE7("*x", "*xxi*sii**x", obj, loc_params, plugin_id, name, dapl_id,
             dxpl_id, req);

    /* Check args and get class pointer */
    if(NULL == obj)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "invalid object")
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a VOL plugin ID")

    /* Call the corresponding internal VOL routine */
    if(NULL == (ret_value = H5VL__dataset_open(obj, loc_params, cls, name, dapl_id, dxpl_id, req)))
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPENOBJ, NULL, "unable to open dataset")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLdataset_open() */


/*-------------------------------------------------------------------------
 * Function:	H5VL__dataset_read
 *
 * Purpose:	Reads data from dataset through the VOL
*
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
static herr_t 
H5VL__dataset_read(void *obj, const H5VL_class_t *cls, hid_t mem_type_id,
    hid_t mem_space_id, hid_t file_space_id, hid_t plist_id, void *buf,
    void **req)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_STATIC

    /* Check if the corresponding VOL callback exists */
    if(NULL == cls->dataset_cls.read)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no 'dataset read' method")

    /* Call the corresponding VOL callback */
    if((cls->dataset_cls.read)(obj, mem_type_id, mem_space_id, file_space_id, plist_id, buf, req) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_READERROR, FAIL, "dataset read failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__dataset_read() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_dataset_read
 *
 * Purpose:	Reads data from dataset through the VOL
*
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t 
H5VL_dataset_read(const H5VL_object_t *vol_obj, hid_t mem_type_id,
    hid_t mem_space_id, hid_t file_space_id, hid_t plist_id, void *buf,
    void **req)
{
    hbool_t vol_wrapper_set = FALSE;    /* Whether the VOL object wrapping context was set up */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Set wrapper info in API context */
    if(H5VL_set_vol_wrapper(vol_obj->data, vol_obj->plugin) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTSET, FAIL, "can't set VOL wrapper info")
    vol_wrapper_set = TRUE;

    /* Call the corresponding internal VOL routine */
    if(H5VL__dataset_read(vol_obj->data, vol_obj->plugin->cls, mem_type_id, mem_space_id, file_space_id, plist_id, buf, req) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_READERROR, FAIL, "dataset read failed")

done:
    /* Reset object wrapping info in API context */
    if(vol_wrapper_set && H5VL_reset_vol_wrapper() < 0)
        HDONE_ERROR(H5E_VOL, H5E_CANTRESET, FAIL, "can't reset VOL wrapper info")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_dataset_read() */


/*-------------------------------------------------------------------------
 * Function:    H5VLdataset_read
 *
 * Purpose:     Reads data from a dataset
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLdataset_read(void *obj, hid_t plugin_id, hid_t mem_type_id, hid_t mem_space_id,
    hid_t file_space_id, hid_t plist_id, void *buf, void **req)
{
    H5VL_class_t *cls;                  /* VOL plugin's class struct */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API_NOINIT
    H5TRACE8("e", "*xiiiii*x**x", obj, plugin_id, mem_type_id, mem_space_id,
             file_space_id, plist_id, buf, req);

    /* Check args and get class pointer */
    if(NULL == obj)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object")
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    /* Call the corresponding internal VOL routine */
    if(H5VL__dataset_read(obj, cls, mem_type_id, mem_space_id, file_space_id, plist_id, buf, req) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to read dataset")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLdataset_read() */


/*-------------------------------------------------------------------------
 * Function:	H5VL__dataset_write
 *
 * Purpose:	Writes data from dataset through the VOL
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
static herr_t 
H5VL__dataset_write(void *obj, const H5VL_class_t *cls, hid_t mem_type_id,
    hid_t mem_space_id, hid_t file_space_id, hid_t plist_id, const void *buf,
    void **req)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_STATIC

    /* Check if the corresponding VOL callback exists */
    if(NULL == cls->dataset_cls.write)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no 'dataset write' method")

    /* Call the corresponding VOL callback */
    if((cls->dataset_cls.write)(obj, mem_type_id, mem_space_id, file_space_id, plist_id, buf, req) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_WRITEERROR, FAIL, "dataset write failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__dataset_write() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_dataset_write
 *
 * Purpose:	Writes data from dataset through the VOL
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t 
H5VL_dataset_write(const H5VL_object_t *vol_obj, hid_t mem_type_id,
    hid_t mem_space_id, hid_t file_space_id, hid_t plist_id, const void *buf,
    void **req)
{
    hbool_t vol_wrapper_set = FALSE;    /* Whether the VOL object wrapping context was set up */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Set wrapper info in API context */
    if(H5VL_set_vol_wrapper(vol_obj->data, vol_obj->plugin) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTSET, FAIL, "can't set VOL wrapper info")
    vol_wrapper_set = TRUE;

    /* Call the corresponding internal VOL routine */
    if(H5VL__dataset_write(vol_obj->data, vol_obj->plugin->cls, mem_type_id, mem_space_id, file_space_id, plist_id, buf, req) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_WRITEERROR, FAIL, "dataset write failed")

done:
    /* Reset object wrapping info in API context */
    if(vol_wrapper_set && H5VL_reset_vol_wrapper() < 0)
        HDONE_ERROR(H5E_VOL, H5E_CANTRESET, FAIL, "can't reset VOL wrapper info")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_dataset_write() */


/*-------------------------------------------------------------------------
 * Function:    H5VLdataset_write
 *
 * Purpose:     Writes data to a dataset
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLdataset_write(void *obj, hid_t plugin_id, hid_t mem_type_id, hid_t mem_space_id,
    hid_t file_space_id, hid_t plist_id, const void *buf, void **req)
{
    H5VL_class_t *cls;                  /* VOL plugin's class struct */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API_NOINIT
    H5TRACE8("e", "*xiiiii*x**x", obj, plugin_id, mem_type_id, mem_space_id,
             file_space_id, plist_id, buf, req);

    /* Check args and get class pointer */
    if(NULL == obj)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object")
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    /* Call the corresponding internal VOL routine */
    if(H5VL__dataset_write(obj, cls, mem_type_id, mem_space_id, file_space_id, plist_id, buf, req) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to write dataset")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLdataset_write() */


/*-------------------------------------------------------------------------
 * Function:	H5VL__dataset_get
 *
 * Purpose:	Get specific information about the dataset through the VOL
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL__dataset_get(void *obj, const H5VL_class_t *cls, H5VL_dataset_get_t get_type, 
    hid_t dxpl_id, void **req, va_list arguments)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_STATIC

    /* Check if the corresponding VOL callback exists */
    if(NULL == cls->dataset_cls.get)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no 'dataset get' method")

    /* Call the corresponding VOL callback */
    if((cls->dataset_cls.get)(obj, get_type, dxpl_id, req, arguments) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "dataset get failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__dataset_get() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_dataset_get
 *
 * Purpose:	Get specific information about the dataset through the VOL
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_dataset_get(const H5VL_object_t *vol_obj, H5VL_dataset_get_t get_type, 
    hid_t dxpl_id, void **req, ...)
{
    va_list arguments;                  /* Argument list passed from the API call */
    hbool_t arg_started = FALSE;        /* Whether the va_list has been started */
    hbool_t vol_wrapper_set = FALSE;    /* Whether the VOL object wrapping context was set up */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Set wrapper info in API context */
    if(H5VL_set_vol_wrapper(vol_obj->data, vol_obj->plugin) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTSET, FAIL, "can't set VOL wrapper info")
    vol_wrapper_set = TRUE;

    /* Call the corresponding internal VOL routine */
    va_start(arguments, req);
    arg_started = TRUE;
    if(H5VL__dataset_get(vol_obj->data, vol_obj->plugin->cls, get_type, dxpl_id, req, arguments) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "dataset get failed")

done:
    /* End access to the va_list, if we started it */
    if(arg_started)
        va_end(arguments);

    /* Reset object wrapping info in API context */
    if(vol_wrapper_set && H5VL_reset_vol_wrapper() < 0)
        HDONE_ERROR(H5E_VOL, H5E_CANTRESET, FAIL, "can't reset VOL wrapper info")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_dataset_get() */


/*-------------------------------------------------------------------------
 * Function:    H5VLdataset_get
 *
 * Purpose:     Gets information about a dataset
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLdataset_get(void *obj, hid_t plugin_id, H5VL_dataset_get_t get_type,
    hid_t dxpl_id, void **req, va_list arguments)
{
    H5VL_class_t *cls;                  /* VOL plugin's class struct */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API_NOINIT
    H5TRACE6("e", "*xiVci**xx", obj, plugin_id, get_type, dxpl_id, req, arguments);

    /* Check args and get class pointer */
    if(NULL == obj)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object")
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    /* Call the corresponding internal VOL routine */
    if(H5VL__dataset_get(obj, cls, get_type, dxpl_id, req, arguments) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "unable to execute dataset get callback")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLdataset_get() */


/*-------------------------------------------------------------------------
 * Function:	H5VL__dataset_specific
 *
 * Purpose:	Specific operation on datasets through the VOL
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL__dataset_specific(void *obj, const H5VL_class_t *cls, H5VL_dataset_specific_t specific_type, 
    hid_t dxpl_id, void **req, va_list arguments)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_STATIC

    /* Check if the corresponding VOL callback exists */
    if(NULL == cls->dataset_cls.specific)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no 'dataset specific' method")

    /* Call the corresponding VOL callback */
    if((cls->dataset_cls.specific)(obj, specific_type, dxpl_id, req, arguments) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "unable to execute dataset specific callback")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__dataset_specific() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_dataset_specific
 *
 * Purpose:	Specific operation on datasets through the VOL
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_dataset_specific(const H5VL_object_t *vol_obj, H5VL_dataset_specific_t specific_type, 
    hid_t dxpl_id, void **req, ...)
{
    va_list arguments;                  /* Argument list passed from the API call */
    hbool_t arg_started = FALSE;        /* Whether the va_list has been started */
    hbool_t vol_wrapper_set = FALSE;    /* Whether the VOL object wrapping context was set up */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Set wrapper info in API context */
    if(H5VL_set_vol_wrapper(vol_obj->data, vol_obj->plugin) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTSET, FAIL, "can't set VOL wrapper info")
    vol_wrapper_set = TRUE;

    /* Call the corresponding internal VOL routine */
    va_start(arguments, req);
    arg_started = TRUE;
    if(H5VL__dataset_specific(vol_obj->data, vol_obj->plugin->cls, specific_type, dxpl_id, req, arguments) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "unable to execute dataset specific callback")

done:
    /* End access to the va_list, if we started it */
    if(arg_started)
        va_end(arguments);

    /* Reset object wrapping info in API context */
    if(vol_wrapper_set && H5VL_reset_vol_wrapper() < 0)
        HDONE_ERROR(H5E_VOL, H5E_CANTRESET, FAIL, "can't reset VOL wrapper info")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_dataset_specific() */


/*-------------------------------------------------------------------------
 * Function:    H5VLdataset_specific
 *
 * Purpose:     Performs a plugin-specific operation on a dataset
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLdataset_specific(void *obj, hid_t plugin_id, H5VL_dataset_specific_t specific_type,
    hid_t dxpl_id, void **req, va_list arguments)
{
    H5VL_class_t *cls;                  /* VOL plugin's class struct */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API_NOINIT
    H5TRACE6("e", "*xiVdi**xx", obj, plugin_id, specific_type, dxpl_id, req,
             arguments);

    /* Check args and get class pointer */
    if(NULL == obj)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object")
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    /* Call the corresponding internal VOL routine */
    if(H5VL__dataset_specific(obj, cls, specific_type, dxpl_id, req, arguments) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "unable to execute dataset specific callback")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLdataset_specific() */


/*-------------------------------------------------------------------------
 * Function:	H5VL__dataset_optional
 *
 * Purpose:	Optional operation specific to plugins.
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL__dataset_optional(void *obj, const H5VL_class_t *cls, hid_t dxpl_id,
    void **req, va_list arguments)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_STATIC

    /* Check if the corresponding VOL callback exists */
    if(NULL == cls->dataset_cls.optional)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no 'dataset optional' method")

    /* Call the corresponding VOL callback */
    if((cls->dataset_cls.optional)(obj, dxpl_id, req, arguments) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "unable to execute dataset optional callback")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__dataset_optional() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_dataset_optional
 *
 * Purpose:	Optional operation specific to plugins.
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_dataset_optional(const H5VL_object_t *vol_obj, hid_t dxpl_id,
    void **req, ...)
{
    va_list arguments;                  /* Argument list passed from the API call */
    hbool_t arg_started = FALSE;        /* Whether the va_list has been started */
    hbool_t vol_wrapper_set = FALSE;    /* Whether the VOL object wrapping context was set up */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Call the corresponding VOL callback */
    /* Set wrapper info in API context */
    if(H5VL_set_vol_wrapper(vol_obj->data, vol_obj->plugin) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTSET, FAIL, "can't set VOL wrapper info")
    vol_wrapper_set = TRUE;

    /* Call the corresponding internal VOL routine */
    va_start(arguments, req);
    arg_started = TRUE;
    if(H5VL__dataset_optional(vol_obj->data, vol_obj->plugin->cls, dxpl_id, req, arguments) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "unable to execute dataset optional callback")

done:
    /* End access to the va_list, if we started it */
    if(arg_started)
        va_end(arguments);

    /* Reset object wrapping info in API context */
    if(vol_wrapper_set && H5VL_reset_vol_wrapper() < 0)
        HDONE_ERROR(H5E_VOL, H5E_CANTRESET, FAIL, "can't reset VOL wrapper info")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_dataset_optional() */


/*-------------------------------------------------------------------------
 * Function:    H5VLdataset_optional
 *
 * Purpose:     Performs an optional plugin-specific operation on a dataset
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLdataset_optional(void *obj, hid_t plugin_id, hid_t dxpl_id, void **req,
    va_list arguments)
{
    H5VL_class_t *cls;                  /* VOL plugin's class struct */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API_NOINIT
    H5TRACE5("e", "*xii**xx", obj, plugin_id, dxpl_id, req, arguments);

    /* Check args and get class pointer */
    if(NULL == obj)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object")
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    /* Call the corresponding internal VOL routine */
    if(H5VL__dataset_optional(obj, cls, dxpl_id, req, arguments) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "unable to execute dataset optional callback")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLdataset_optional() */


/*-------------------------------------------------------------------------
 * Function:    H5VL__dataset_close
 *
 * Purpose:     Closes a dataset through the VOL
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL__dataset_close(void *obj, const H5VL_class_t *cls, hid_t dxpl_id, void **req)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_STATIC

    /* Sanity check */
    HDassert(obj);
    HDassert(cls);

    /* Check if the corresponding VOL callback exists */
    if(NULL == cls->dataset_cls.close)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no 'dataset close' method")

    /* Call the corresponding VOL callback */
    if((cls->dataset_cls.close)(obj, dxpl_id, req) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTCLOSEOBJ, FAIL, "dataset close failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__dataset_close() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_dataset_close
 *
 * Purpose:     Closes a dataset through the VOL
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_dataset_close(const H5VL_object_t *vol_obj, hid_t dxpl_id, void **req)
{
    hbool_t vol_wrapper_set = FALSE;    /* Whether the VOL object wrapping context was set up */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity check */
    HDassert(vol_obj);
    HDassert(vol_obj->data);
    HDassert(vol_obj->plugin);
    HDassert(vol_obj->plugin->cls);

    /* Set wrapper info in API context */
    if(H5VL_set_vol_wrapper(vol_obj->data, vol_obj->plugin) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTSET, FAIL, "can't set VOL wrapper info")
    vol_wrapper_set = TRUE;

    /* Call the corresponding internal VOL routine */
    if(H5VL__dataset_close(vol_obj->data, vol_obj->plugin->cls, dxpl_id, req) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTCLOSEOBJ, FAIL, "dataset close failed")

done:
    /* Reset object wrapping info in API context */
    if(vol_wrapper_set && H5VL_reset_vol_wrapper() < 0)
        HDONE_ERROR(H5E_VOL, H5E_CANTRESET, FAIL, "can't reset VOL wrapper info")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_dataset_close() */


/*-------------------------------------------------------------------------
 * Function:    H5VLdataset_close
 *
 * Purpose:     Closes a dataset
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLdataset_close(void *obj, hid_t plugin_id, hid_t dxpl_id, void **req)
{
    H5VL_class_t *cls;                  /* VOL plugin's class struct */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API_NOINIT
    H5TRACE4("e", "*xii**x", obj, plugin_id, dxpl_id, req);

    /* Check args and get class pointer */
    if(NULL == obj)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object")
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    /* Call the corresponding internal VOL routine */
    if(H5VL__dataset_close(obj, cls, dxpl_id, req) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTCLOSEOBJ, FAIL, "unable to close dataset")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLdataset_close() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_file_create
 *
 * Purpose:	Creates a file through the VOL
 *
 * Note:	Does not have a 'static' version of the routine, since there's
 *		no objects in the container before this operation completes.
 *
 * Return:      Success: Pointer to new file
 *		Failure: NULL
 *
 *-------------------------------------------------------------------------
 */
void *
H5VL_file_create(const H5VL_class_t *cls, const char *name, unsigned flags, hid_t fcpl_id, 
    hid_t fapl_id, hid_t dxpl_id, void **req)
{
    void *ret_value = NULL;             /* Return value */

    FUNC_ENTER_NOAPI(NULL)

    /* Check if the corresponding VOL callback exists */
    if(NULL == cls->file_cls.create)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, NULL, "VOL plugin has no 'file create' method")

    /* Call the corresponding VOL callback */
    if(NULL == (ret_value = (cls->file_cls.create)(name, flags, fcpl_id, fapl_id, dxpl_id, req)))
        HGOTO_ERROR(H5E_VOL, H5E_CANTCREATE, NULL, "file create failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_file_create() */


/*-------------------------------------------------------------------------
 * Function:    H5VLfile_create
 *
 * Purpose:     Creates a file
 *
 * Return:      Success:    Pointer to the new file
 *              Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
void *
H5VLfile_create(const char *name, unsigned flags, hid_t fcpl_id, hid_t fapl_id,
    hid_t dxpl_id, void **req)
{
    H5P_genplist_t *plist;              /* Property list pointer */
    H5VL_plugin_prop_t plugin_prop;     /* Property for VOL plugin ID & info */
    H5VL_class_t *cls;                  /* VOL plugin's class struct */
    void *ret_value = NULL;             /* Return value */

    FUNC_ENTER_API_NOINIT
    H5TRACE6("*x", "*sIuiii**x", name, flags, fcpl_id, fapl_id, dxpl_id, req);

    /* Get the VOL info from the fapl */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(fapl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a file access property list")
    if(H5P_peek(plist, H5F_ACS_VOL_DRV_NAME, &plugin_prop) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get VOL plugin info")

    /* Get class pointer */
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_prop.plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a VOL plugin ID")

    /* Call the corresponding internal VOL routine */
    if(NULL == (ret_value = H5VL_file_create(cls, name, flags, fcpl_id, fapl_id, dxpl_id, req)))
        HGOTO_ERROR(H5E_VOL, H5E_CANTCREATE, NULL, "unable to create file")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLfile_create() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_file_open
 *
 * Purpose:	Opens a file through the VOL.
 *
 * Note:	Does not have a 'static' version of the routine, since there's
 *		no objects in the container before this operation completes.
 *
 * Return:      Success: Pointer to file. 
 *		Failure: NULL
 *
 *-------------------------------------------------------------------------
 */
void *
H5VL_file_open(const H5VL_class_t *cls, const char *name, unsigned flags, hid_t fapl_id, 
    hid_t dxpl_id, void **req)
{
    void *ret_value = NULL;             /* Return value */

    FUNC_ENTER_NOAPI(NULL)

    /* Check if the corresponding VOL callback exists */
    if(NULL == cls->file_cls.open)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, NULL, "VOL plugin has no 'file open' method")

    /* Call the corresponding VOL callback */
    if(NULL == (ret_value = (cls->file_cls.open)(name, flags, fapl_id, dxpl_id, req)))
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPENOBJ, NULL, "open failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_file_open() */


/*-------------------------------------------------------------------------
 * Function:    H5VLfile_open
 *
 * Purpose:     Opens a file
 *
 * Return:      Success:    Pointer to the file
 *              Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
void *
H5VLfile_open(const char *name, unsigned flags, hid_t fapl_id, hid_t dxpl_id,
    void **req)
{
    H5P_genplist_t *plist;              /* Property list pointer */
    H5VL_plugin_prop_t plugin_prop;     /* Property for VOL plugin ID & info */
    H5VL_class_t *cls;                  /* VOL plugin's class struct */
    void *ret_value = NULL;             /* Return value */

    FUNC_ENTER_API_NOINIT
    H5TRACE5("*x", "*sIuii**x", name, flags, fapl_id, dxpl_id, req);

    /* Get the VOL info from the fapl */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(fapl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a file access property list")
    if(H5P_peek(plist, H5F_ACS_VOL_DRV_NAME, &plugin_prop) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get VOL plugin info")

    /* Get class pointer */
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_prop.plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a VOL plugin ID")

    /* Call the corresponding internal VOL routine */
    if(NULL == (ret_value = H5VL_file_open(cls, name, flags, fapl_id, dxpl_id, req)))
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPENOBJ, NULL, "unable to open file")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLfile_open() */


/*-------------------------------------------------------------------------
 * Function:	H5VL__file_get
 *
 * Purpose:	Get specific information about the file through the VOL
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL__file_get(void *obj, const H5VL_class_t *cls, H5VL_file_get_t get_type, 
    hid_t dxpl_id, void **req, va_list arguments)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_STATIC

    /* Check if the corresponding VOL callback exists */
    if(NULL == cls->file_cls.get)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no 'file get' method")

    /* Call the corresponding VOL callback */
    if((cls->file_cls.get)(obj, get_type, dxpl_id, req, arguments) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "file get failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__file_get() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_file_get
 *
 * Purpose:	Get specific information about the file through the VOL
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_file_get(const H5VL_object_t *vol_obj, H5VL_file_get_t get_type, 
    hid_t dxpl_id, void **req, ...)
{
    va_list arguments;                  /* Argument list passed from the API call */
    hbool_t arg_started = FALSE;        /* Whether the va_list has been started */
    hbool_t vol_wrapper_set = FALSE;    /* Whether the VOL object wrapping context was set up */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Set wrapper info in API context */
    if(H5VL_set_vol_wrapper(vol_obj->data, vol_obj->plugin) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTSET, FAIL, "can't set VOL wrapper info")
    vol_wrapper_set = TRUE;

    /* Call the corresponding internal VOL routine */
    va_start(arguments, req);
    arg_started = TRUE;
    if(H5VL__file_get(vol_obj->data, vol_obj->plugin->cls, get_type, dxpl_id, req, arguments) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "file get failed")

done:
    /* End access to the va_list, if we started it */
    if(arg_started)
        va_end(arguments);

    /* Reset object wrapping info in API context */
    if(vol_wrapper_set && H5VL_reset_vol_wrapper() < 0)
        HDONE_ERROR(H5E_VOL, H5E_CANTRESET, FAIL, "can't reset VOL wrapper info")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_file_get() */


/*-------------------------------------------------------------------------
 * Function:    H5VLfile_get
 *
 * Purpose:     Gets information about the file
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLfile_get(void *obj, hid_t plugin_id, H5VL_file_get_t get_type,
    hid_t dxpl_id, void **req, va_list arguments)
{
    H5VL_class_t *cls;                  /* VOL plugin's class struct */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API_NOINIT
    H5TRACE6("e", "*xiVgi**xx", obj, plugin_id, get_type, dxpl_id, req, arguments);

    /* Check args and get class pointer */
    if(NULL == obj)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object")
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    /* Call the corresponding internal VOL routine */
    if(H5VL__file_get(obj, cls, get_type, dxpl_id, req, arguments) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "unable to execute file get callback")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLfile_get() */


/*-------------------------------------------------------------------------
 * Function:	H5VL__file_specific
 *
 * Purpose:	Perform File specific operations through the VOL
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL__file_specific(void *obj, const H5VL_class_t *cls, H5VL_file_specific_t specific_type, 
    hid_t dxpl_id, void **req, va_list arguments)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_STATIC

    /* Check if the corresponding VOL callback exists */
    if(NULL == cls->file_cls.specific)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no 'file specific' method")

    /* Call the corresponding VOL callback */
    if((cls->file_cls.specific)(obj, specific_type, dxpl_id, req, arguments) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "file specific failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__file_specific() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_file_specific
 *
 * Purpose:	Perform File specific operations through the VOL
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_file_specific(const H5VL_object_t *vol_obj, H5VL_file_specific_t specific_type, 
    hid_t dxpl_id, void **req, ...)
{
    const H5VL_class_t *cls;            /* VOL plugin's class struct */
    va_list arguments;                  /* Argument list passed from the API call */
    hbool_t arg_started = FALSE;        /* Whether the va_list has been started */
    hbool_t vol_wrapper_set = FALSE;    /* Whether the VOL object wrapping context was set up */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Start access to the varargs, so they are available in all situations below */
    va_start(arguments, req);
    arg_started = TRUE;

    /* Special treatment of file access check */
    if(specific_type == H5VL_FILE_IS_ACCESSIBLE) {
        H5P_genplist_t     *plist;          /* Property list pointer */
        H5VL_plugin_prop_t  plugin_prop;    /* Property for VOL plugin ID & info */
        va_list             tmp_args;       /* argument list passed from the API call */
        hid_t               fapl_id;        /* File access property list for accessing the file */

        /* Get the file access property list to access the file */
        va_copy(tmp_args, arguments);
        fapl_id = va_arg(tmp_args, hid_t);
        va_end(tmp_args);

        /* Get the VOL info from the FAPL */
        if(NULL == (plist = (H5P_genplist_t *)H5I_object(fapl_id)))
            HGOTO_ERROR(H5E_VOL, H5E_BADTYPE, FAIL, "not a file access property list")
        if(H5P_peek(plist, H5F_ACS_VOL_DRV_NAME, &plugin_prop) < 0)
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't get VOL plugin info")

        /* Get class pointer */
        if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_prop.plugin_id, H5I_VOL)))
            HGOTO_ERROR(H5E_VOL, H5E_BADTYPE, FAIL, "not a VOL plugin ID")
    } /* end if */
    /* Set wrapper info in API context, for all other operations */
    else {
        /* Sanity check */
        HDassert(vol_obj);

        if(H5VL_set_vol_wrapper(vol_obj->data, vol_obj->plugin) < 0)
            HGOTO_ERROR(H5E_VOL, H5E_CANTSET, FAIL, "can't set VOL wrapper info")
        vol_wrapper_set = TRUE;

        /* Set the VOL plugin class pointer */
        cls = vol_obj->plugin->cls;
    } /* end else */


    /* Call the corresponding internal VOL routine */
    if(H5VL__file_specific(vol_obj ? vol_obj->data : NULL, cls, specific_type, dxpl_id, req, arguments) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "file specific failed")

done:
    /* End access to the va_list, if we started it */
    if(arg_started)
        va_end(arguments);

    /* Reset object wrapping info in API context */
    if(vol_wrapper_set && H5VL_reset_vol_wrapper() < 0)
        HDONE_ERROR(H5E_VOL, H5E_CANTRESET, FAIL, "can't reset VOL wrapper info")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_file_specific() */


/*-------------------------------------------------------------------------
 * Function:    H5VLfile_specific
 *
 * Purpose:     Performs a plugin-specific operation on a file
 *
 * Note:	The 'obj' parameter is allowed to be NULL
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLfile_specific(void *obj, hid_t plugin_id, H5VL_file_specific_t specific_type,
    hid_t dxpl_id, void **req, va_list arguments)
{
    H5VL_class_t *cls;                  /* VOL plugin's class struct */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API_NOINIT
    H5TRACE6("e", "*xiVhi**xx", obj, plugin_id, specific_type, dxpl_id, req,
             arguments);

    /* Check args and get class pointer */
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    /* Call the corresponding internal VOL routine */
    if(H5VL__file_specific(obj, cls, specific_type, dxpl_id, req, arguments) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "unable to execute file specific callback")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLfile_specific() */


/*-------------------------------------------------------------------------
 * Function:	H5VL__file_optional
 *
 * Purpose:	Perform a plugin specific operation
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL__file_optional(void *obj, const H5VL_class_t *cls, hid_t dxpl_id,
    void **req, va_list arguments)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_STATIC

    /* Check if the corresponding VOL callback exists */
    if(NULL == cls->file_cls.optional)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no 'file optional' method")

    /* Call the corresponding VOL callback */
    if((cls->file_cls.optional)(obj, dxpl_id, req, arguments) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "file optional failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__file_optional() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_file_optional
 *
 * Purpose:	Perform a plugin specific operation
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_file_optional(const H5VL_object_t *vol_obj, hid_t dxpl_id, void **req, ...)
{
    va_list arguments;                  /* Argument list passed from the API call */
    hbool_t arg_started = FALSE;        /* Whether the va_list has been started */
    hbool_t vol_wrapper_set = FALSE;    /* Whether the VOL object wrapping context was set up */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Set wrapper info in API context */
    if(H5VL_set_vol_wrapper(vol_obj->data, vol_obj->plugin) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTSET, FAIL, "can't set VOL wrapper info")
    vol_wrapper_set = TRUE;

    /* Call the corresponding internal VOL routine */
    va_start(arguments, req);
    arg_started = TRUE;
    if(H5VL__file_optional(vol_obj->data, vol_obj->plugin->cls, dxpl_id, req, arguments) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "file optional failed")

done:
    /* End access to the va_list, if we started it */
    if(arg_started)
        va_end(arguments);

    /* Reset object wrapping info in API context */
    if(vol_wrapper_set && H5VL_reset_vol_wrapper() < 0)
        HDONE_ERROR(H5E_VOL, H5E_CANTRESET, FAIL, "can't reset VOL wrapper info")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_file_optional() */


/*-------------------------------------------------------------------------
 * Function:    H5VLfile_optional
 *
 * Purpose:     Performs an optional plugin-specific operation on a file
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLfile_optional(void *obj, hid_t plugin_id, hid_t dxpl_id, void **req,
    va_list arguments)
{
    H5VL_class_t *cls;                  /* VOL plugin's class struct */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API_NOINIT
    H5TRACE5("e", "*xii**xx", obj, plugin_id, dxpl_id, req, arguments);

    /* Check args and get class pointer */
    if(NULL == obj)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object")
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    /* Call the corresponding internal VOL routine */
    if(H5VL__file_optional(obj, cls, dxpl_id, req, arguments) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "unable to execute file optional callback")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLfile_optional() */


/*-------------------------------------------------------------------------
 * Function:    H5VL__file_close
 *
 * Purpose:     Closes a file through the VOL
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL__file_close(void *obj, const H5VL_class_t *cls, hid_t dxpl_id, void **req)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_STATIC

    /* Sanity check */
    HDassert(obj);
    HDassert(cls);

    /* Check if the corresponding VOL callback exists */
    if(NULL == cls->file_cls.close)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no 'file close' method")

    /* Call the corresponding VOL callback */
    if((cls->file_cls.close)(obj, dxpl_id, req) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTCLOSEFILE, FAIL, "file close failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__file_close() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_file_close
 *
 * Purpose:     Closes a file through the VOL
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_file_close(const H5VL_object_t *vol_obj, hid_t dxpl_id, void **req)
{
    hbool_t vol_wrapper_set = FALSE;    /* Whether the VOL object wrapping context was set up */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Set wrapper info in API context */
    if(H5VL_set_vol_wrapper(vol_obj->data, vol_obj->plugin) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTSET, FAIL, "can't set VOL wrapper info")
    vol_wrapper_set = TRUE;

    /* Call the corresponding internal VOL routine */
    if(H5VL__file_close(vol_obj->data, vol_obj->plugin->cls, dxpl_id, req) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTCLOSEFILE, FAIL, "file close failed")

done:
    /* Reset object wrapping info in API context */
    if(vol_wrapper_set && H5VL_reset_vol_wrapper() < 0)
        HDONE_ERROR(H5E_VOL, H5E_CANTRESET, FAIL, "can't reset VOL wrapper info")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_file_close() */


/*-------------------------------------------------------------------------
 * Function:    H5VLfile_close
 *
 * Purpose:     Closes a file
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLfile_close(void *obj, hid_t plugin_id, hid_t dxpl_id, void **req)
{
    H5VL_class_t *cls;                  /* VOL plugin's class struct */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API_NOINIT
    H5TRACE4("e", "*xii**x", obj, plugin_id, dxpl_id, req);

    /* Check args and get class pointer */
    if(NULL == obj)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object")
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    /* Call the corresponding internal VOL routine */
    if(H5VL__file_close(obj, cls, dxpl_id, req) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTCLOSEFILE, FAIL, "unable to close file")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLfile_close() */


/*-------------------------------------------------------------------------
 * Function:	H5VL__group_create
 *
 * Purpose:	Creates a group through the VOL
 *
 * Return:      Success: Pointer to new group
 *		Failure: NULL
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL__group_create(void *obj, H5VL_loc_params_t loc_params, const H5VL_class_t *cls,
    const char *name, hid_t gcpl_id, hid_t gapl_id, hid_t dxpl_id, void **req)
{
    void *ret_value = NULL;     /* Return value */

    FUNC_ENTER_STATIC

    /* Check if the corresponding VOL callback exists */
    if(NULL == cls->group_cls.create)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, NULL, "VOL plugin has no 'group create' method")

    /* Call the corresponding VOL callback */
    if(NULL == (ret_value = (cls->group_cls.create)(obj, loc_params, name, gcpl_id, gapl_id, dxpl_id, req)))
        HGOTO_ERROR(H5E_VOL, H5E_CANTCREATE, NULL, "group create failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__group_create() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_group_create
 *
 * Purpose:	Creates a group through the VOL
 *
 * Return:      Success: Pointer to new group
 *		Failure: NULL
 *
 *-------------------------------------------------------------------------
 */
void *
H5VL_group_create(const H5VL_object_t *vol_obj, H5VL_loc_params_t loc_params,
    const char *name, hid_t gcpl_id, hid_t gapl_id, hid_t dxpl_id, void **req)
{
    hbool_t vol_wrapper_set = FALSE;    /* Whether the VOL object wrapping context was set up */
    void *ret_value = NULL;     /* Return value */

    FUNC_ENTER_NOAPI(NULL)

    /* Set wrapper info in API context */
    if(H5VL_set_vol_wrapper(vol_obj->data, vol_obj->plugin) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTSET, NULL, "can't set VOL wrapper info")
    vol_wrapper_set = TRUE;

    /* Call the corresponding internal VOL routine */
    if(NULL == (ret_value = H5VL__group_create(vol_obj->data, loc_params, vol_obj->plugin->cls, name, gcpl_id, gapl_id, dxpl_id, req)))
        HGOTO_ERROR(H5E_VOL, H5E_CANTCREATE, NULL, "group create failed")

done:
    /* Reset object wrapping info in API context */
    if(vol_wrapper_set && H5VL_reset_vol_wrapper() < 0)
        HDONE_ERROR(H5E_VOL, H5E_CANTRESET, NULL, "can't reset VOL wrapper info")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_group_create() */


/*-------------------------------------------------------------------------
 * Function:    H5VLgroup_create
 *
 * Purpose:     Creates a group
 *
 * Return:      Success:    Pointer to the new group
 *              Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
void *
H5VLgroup_create(void *obj, H5VL_loc_params_t loc_params, hid_t plugin_id, const char *name,
    hid_t gcpl_id, hid_t gapl_id, hid_t dxpl_id, void **req)
{
    H5VL_class_t *cls;                  /* VOL plugin's class struct */
    void *ret_value = NULL;             /* Return value */

    FUNC_ENTER_API_NOINIT
    H5TRACE8("*x", "*xxi*siii**x", obj, loc_params, plugin_id, name, gcpl_id,
             gapl_id, dxpl_id, req);

    /* Check args and get class pointer */
    if(NULL == obj)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "invalid object")
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a VOL plugin ID")

    /* Call the corresponding internal VOL routine */
    if(NULL == (ret_value = H5VL__group_create(obj, loc_params, cls, name, gcpl_id, gapl_id, dxpl_id, req)))
        HGOTO_ERROR(H5E_VOL, H5E_CANTCREATE, NULL, "unable to create group")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLgroup_create() */


/*-------------------------------------------------------------------------
 * Function:	H5VL__group_open
 *
 * Purpose:	Opens a group through the VOL
 *
 * Return:      Success: Pointer to group
 *		Failure: NULL
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL__group_open(void *obj, H5VL_loc_params_t loc_params, const H5VL_class_t *cls,
    const char *name, hid_t gapl_id, hid_t dxpl_id, void **req)
{
    void *ret_value = NULL;             /* Return value */

    FUNC_ENTER_STATIC

    /* Check if the corresponding VOL callback exists */
    if(NULL == cls->group_cls.open)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, NULL, "VOL plugin has no 'group open' method")

    /* Call the corresponding VOL callback */
    if(NULL == (ret_value = (cls->group_cls.open)(obj, loc_params, name, gapl_id, dxpl_id, req)))
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPENOBJ, NULL, "group open failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__group_open() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_group_open
 *
 * Purpose:	Opens a group through the VOL
 *
 * Return:      Success: Pointer to group
 *		Failure: NULL
 *
 *-------------------------------------------------------------------------
 */
void *
H5VL_group_open(const H5VL_object_t *vol_obj, H5VL_loc_params_t loc_params,
    const char *name, hid_t gapl_id, hid_t dxpl_id, void **req)
{
    hbool_t vol_wrapper_set = FALSE;    /* Whether the VOL object wrapping context was set up */
    void *ret_value = NULL;             /* Return value */

    FUNC_ENTER_NOAPI(NULL)

    /* Set wrapper info in API context */
    if(H5VL_set_vol_wrapper(vol_obj->data, vol_obj->plugin) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTSET, NULL, "can't set VOL wrapper info")
    vol_wrapper_set = TRUE;

    /* Call the corresponding internal VOL routine */
    if(NULL == (ret_value = H5VL__group_open(vol_obj->data, loc_params, vol_obj->plugin->cls, name, gapl_id, dxpl_id, req)))
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPENOBJ, NULL, "group open failed")

done:
    /* Reset object wrapping info in API context */
    if(vol_wrapper_set && H5VL_reset_vol_wrapper() < 0)
        HDONE_ERROR(H5E_VOL, H5E_CANTRESET, NULL, "can't reset VOL wrapper info")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_group_open() */


/*-------------------------------------------------------------------------
 * Function:    H5VLgroup_open
 *
 * Purpose:     Opens a group
 *
 * Return:      Success:    Pointer to the group
 *              Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
void *
H5VLgroup_open(void *obj, H5VL_loc_params_t loc_params, hid_t plugin_id, const char *name,
    hid_t gapl_id, hid_t dxpl_id, void **req)
{
    H5VL_class_t *cls;                  /* VOL plugin's class struct */
    void *ret_value = NULL;             /* Return value */

    FUNC_ENTER_API_NOINIT
    H5TRACE7("*x", "*xxi*sii**x", obj, loc_params, plugin_id, name, gapl_id,
             dxpl_id, req);

    /* Check args and get class pointer */
    if(NULL == obj)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "invalid object")
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a VOL plugin ID")

    /* Call the corresponding internal VOL routine */
    if(NULL == (ret_value = H5VL__group_open(obj, loc_params, cls, name, gapl_id, dxpl_id, req)))
        HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, NULL, "unable to open group")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLgroup_open() */


/*-------------------------------------------------------------------------
 * Function:	H5VL__group_get
 *
 * Purpose:	Get specific information about the group through the VOL
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL__group_get(void *obj, const H5VL_class_t *cls, H5VL_group_get_t get_type, 
    hid_t dxpl_id, void **req, va_list arguments)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_STATIC

    /* Check if the corresponding VOL callback exists */
    if(NULL == cls->group_cls.get)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no 'group get' method")

    /* Call the corresponding VOL callback */
    if((cls->group_cls.get)(obj, get_type, dxpl_id, req, arguments) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "group get failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__group_get() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_group_get
 *
 * Purpose:	Get specific information about the group through the VOL
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_group_get(const H5VL_object_t *vol_obj, H5VL_group_get_t get_type, 
    hid_t dxpl_id, void **req, ...)
{
    va_list arguments;                  /* Argument list passed from the API call */
    hbool_t arg_started = FALSE;        /* Whether the va_list has been started */
    hbool_t vol_wrapper_set = FALSE;    /* Whether the VOL object wrapping context was set up */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Set wrapper info in API context */
    if(H5VL_set_vol_wrapper(vol_obj->data, vol_obj->plugin) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTSET, FAIL, "can't set VOL wrapper info")
    vol_wrapper_set = TRUE;

    /* Call the corresponding internal VOL routine */
    va_start(arguments, req);
    arg_started = TRUE;
    if(H5VL__group_get(vol_obj->data, vol_obj->plugin->cls, get_type, dxpl_id, req, arguments) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "group get failed")

done:
    /* End access to the va_list, if we started it */
    if(arg_started)
        va_end(arguments);

    /* Reset object wrapping info in API context */
    if(vol_wrapper_set && H5VL_reset_vol_wrapper() < 0)
        HDONE_ERROR(H5E_VOL, H5E_CANTRESET, FAIL, "can't reset VOL wrapper info")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_group_get() */


/*-------------------------------------------------------------------------
 * Function:    H5VLgroup_get
 *
 * Purpose:     Gets information about the group
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLgroup_get(void *obj, hid_t plugin_id, H5VL_group_get_t get_type,
    hid_t dxpl_id, void **req, va_list arguments)
{
    H5VL_class_t *cls;                  /* VOL plugin's class struct */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API_NOINIT
    H5TRACE6("e", "*xiVii**xx", obj, plugin_id, get_type, dxpl_id, req, arguments);

    /* Check args and get class pointer */
    if(NULL == obj)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object")
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    /* Call the corresponding internal VOL routine */
    if(H5VL__group_get(obj, cls, get_type, dxpl_id, req, arguments) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "unable to execute group get callback")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLgroup_get() */


/*-------------------------------------------------------------------------
 * Function:	H5VL__group_specific
 *
 * Purpose:	Specific operation on groups through the VOL
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL__group_specific(void *obj, const H5VL_class_t *cls, H5VL_group_specific_t specific_type, 
    hid_t dxpl_id, void **req, va_list arguments)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_STATIC

    /* Check if the corresponding VOL callback exists */
    if(NULL == cls->group_cls.specific)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no 'group specific' method")

    /* Call the corresponding VOL callback */
    if((cls->group_cls.specific)(obj, specific_type, dxpl_id, req, arguments) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "unable to execute group specific callback")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__group_specific() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_group_specific
 *
 * Purpose:	Specific operation on groups through the VOL
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_group_specific(const H5VL_object_t *vol_obj, H5VL_group_specific_t specific_type, 
    hid_t dxpl_id, void **req, ...)
{
    va_list arguments;                  /* Argument list passed from the API call */
    hbool_t arg_started = FALSE;        /* Whether the va_list has been started */
    hbool_t vol_wrapper_set = FALSE;    /* Whether the VOL object wrapping context was set up */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Set wrapper info in API context */
    if(H5VL_set_vol_wrapper(vol_obj->data, vol_obj->plugin) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTSET, FAIL, "can't set VOL wrapper info")
    vol_wrapper_set = TRUE;

    /* Call the corresponding internal VOL routine */
    va_start(arguments, req);
    arg_started = TRUE;
    if(H5VL__group_specific(vol_obj->data, vol_obj->plugin->cls, specific_type, dxpl_id, req, arguments) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "unable to execute group specific callback")

done:
    /* End access to the va_list, if we started it */
    if(arg_started)
        va_end(arguments);

    /* Reset object wrapping info in API context */
    if(vol_wrapper_set && H5VL_reset_vol_wrapper() < 0)
        HDONE_ERROR(H5E_VOL, H5E_CANTRESET, FAIL, "can't reset VOL wrapper info")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_group_specific() */


/*-------------------------------------------------------------------------
 * Function:    H5VLgroup_specific
 *
 * Purpose:     Performs a plugin-specific operation on a group
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLgroup_specific(void *obj, hid_t plugin_id, H5VL_group_specific_t specific_type,
    hid_t dxpl_id, void **req, va_list arguments)
{
    H5VL_class_t *cls;                  /* VOL plugin's class struct */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API_NOINIT
    H5TRACE6("e", "*xiVji**xx", obj, plugin_id, specific_type, dxpl_id, req,
             arguments);

    /* Check args and get class pointer */
    if(NULL == obj)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object")
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    /* Call the corresponding internal VOL routine */
    if(H5VL__group_specific(obj, cls, specific_type, dxpl_id, req, arguments) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "unable to execute group specific callback")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLgroup_specific() */


/*-------------------------------------------------------------------------
 * Function:	H5VL__group_optional
 *
 * Purpose:	Optional operation specific to plugins.
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL__group_optional(void *obj, const H5VL_class_t *cls, hid_t dxpl_id,
    void **req, va_list arguments)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_STATIC

    /* Check if the corresponding VOL callback exists */
    if(NULL == cls->group_cls.optional)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no 'group optional' method")

    /* Call the corresponding VOL callback */
    if((cls->group_cls.optional)(obj, dxpl_id, req, arguments) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "unable to execute group optional callback")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__group_optional() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_group_optional
 *
 * Purpose:	Optional operation specific to plugins.
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_group_optional(const H5VL_object_t *vol_obj, hid_t dxpl_id, void **req, ...)
{
    va_list arguments;                  /* Argument list passed from the API call */
    hbool_t arg_started = FALSE;        /* Whether the va_list has been started */
    hbool_t vol_wrapper_set = FALSE;    /* Whether the VOL object wrapping context was set up */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Set wrapper info in API context */
    if(H5VL_set_vol_wrapper(vol_obj->data, vol_obj->plugin) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTSET, FAIL, "can't set VOL wrapper info")
    vol_wrapper_set = TRUE;

    /* Call the corresponding internal VOL routine */
    va_start(arguments, req);
    arg_started = TRUE;
    if(H5VL__group_optional(vol_obj->data, vol_obj->plugin->cls, dxpl_id, req, arguments) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "unable to execute group optional callback")

done:
    /* End access to the va_list, if we started it */
    if(arg_started)
        va_end(arguments);

    /* Reset object wrapping info in API context */
    if(vol_wrapper_set && H5VL_reset_vol_wrapper() < 0)
        HDONE_ERROR(H5E_VOL, H5E_CANTRESET, FAIL, "can't reset VOL wrapper info")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_group_optional() */


/*-------------------------------------------------------------------------
 * Function:    H5VLgroup_optional
 *
 * Purpose:     Performs an optional plugin-specific operation on a group
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLgroup_optional(void *obj, hid_t plugin_id, hid_t dxpl_id, void **req,
    va_list arguments)
{
    H5VL_class_t *cls;                  /* VOL plugin's class struct */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API_NOINIT
    H5TRACE5("e", "*xii**xx", obj, plugin_id, dxpl_id, req, arguments);

    /* Check args and get class pointer */
    if(NULL == obj)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object")
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    /* Call the corresponding internal VOL routine */
    if(H5VL__group_optional(obj, cls, dxpl_id, req, arguments) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "unable to execute group optional callback")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLgroup_optional() */


/*-------------------------------------------------------------------------
 * Function:    H5VL__group_close
 *
 * Purpose:     Closes a group through the VOL
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL__group_close(void *obj, const H5VL_class_t *cls, hid_t dxpl_id, void **req)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    /* Sanity check */
    HDassert(obj);
    HDassert(cls);

    FUNC_ENTER_STATIC

    /* Check if the corresponding VOL callback exists */
    if(NULL == cls->group_cls.close)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no 'group close' method")

    /* Call the corresponding VOL callback */
    if((cls->group_cls.close)(obj, dxpl_id, req) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTCLOSEOBJ, FAIL, "group close failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__group_close() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_group_close
 *
 * Purpose:     Closes a group through the VOL
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_group_close(const H5VL_object_t *vol_obj, hid_t dxpl_id, void **req)
{
    hbool_t vol_wrapper_set = FALSE;    /* Whether the VOL object wrapping context was set up */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Set wrapper info in API context */
    if(H5VL_set_vol_wrapper(vol_obj->data, vol_obj->plugin) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTSET, FAIL, "can't set VOL wrapper info")
    vol_wrapper_set = TRUE;

    /* Call the corresponding internal VOL routine */
    if(H5VL__group_close(vol_obj->data, vol_obj->plugin->cls, dxpl_id, req) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTCLOSEOBJ, FAIL, "group close failed")

done:
    /* Reset object wrapping info in API context */
    if(vol_wrapper_set && H5VL_reset_vol_wrapper() < 0)
        HDONE_ERROR(H5E_VOL, H5E_CANTRESET, FAIL, "can't reset VOL wrapper info")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_group_close() */


/*-------------------------------------------------------------------------
 * Function:    H5VLgroup_close
 *
 * Purpose:     Closes a group
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLgroup_close(void *obj, hid_t plugin_id, hid_t dxpl_id, void **req)
{
    H5VL_class_t *cls;                  /* VOL plugin's class struct */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API_NOINIT
    H5TRACE4("e", "*xii**x", obj, plugin_id, dxpl_id, req);

    /* Check args and get class pointer */
    if(NULL == obj)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object")
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    /* Call the corresponding internal VOL routine */
    if(H5VL__group_close(obj, cls, dxpl_id, req) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTCLOSEOBJ, FAIL, "unable to close group")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLgroup_close() */


/*-------------------------------------------------------------------------
 * Function:	H5VL__link_create
 *
 * Purpose:	Creates a link through the VOL
 *
 * Note:	The 'obj' parameter is allowed to be NULL
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL__link_create(H5VL_link_create_type_t create_type, void *obj, H5VL_loc_params_t loc_params, 
    const H5VL_class_t *cls, hid_t lcpl_id, hid_t lapl_id, hid_t dxpl_id, void **req)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_STATIC

    /* Check if the corresponding VOL callback exists */
    if(NULL == cls->link_cls.create)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no 'link create' method")

    /* Call the corresponding VOL callback */
    if((cls->link_cls.create)(create_type, obj, loc_params, lcpl_id, lapl_id, dxpl_id, req) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTCREATE, FAIL, "link create failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__link_create() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_link_create
 *
 * Purpose:	Creates a link through the VOL
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_link_create(H5VL_link_create_type_t create_type, const H5VL_object_t *vol_obj,
    H5VL_loc_params_t loc_params, hid_t lcpl_id, hid_t lapl_id, hid_t dxpl_id,
    void **req)
{
    H5VL_object_t tmp_vol_obj;          /* Temporary object token of */
    hbool_t vol_wrapper_set = FALSE;    /* Whether the VOL object wrapping context was set up */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Special case for hard links */
    if(H5VL_LINK_CREATE_HARD == create_type && NULL == vol_obj->data) {
        H5P_genplist_t *plist;              /* Property list pointer */

        /* Get the VOL data pointer from the fapl */
        if(NULL == (plist = (H5P_genplist_t *)H5I_object(lcpl_id)))
            HGOTO_ERROR(H5E_VOL, H5E_BADTYPE, FAIL, "not a file access property list")
        if(H5P_peek(plist, H5VL_PROP_LINK_TARGET, &tmp_vol_obj.data) < 0)
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't get VOL plugin info")
    } /* end if */
    else
        /* Use the VOL object passed in */
        tmp_vol_obj.data = vol_obj->data;
    tmp_vol_obj.plugin = vol_obj->plugin;

    /* Set wrapper info in API context */
    if(H5VL_set_vol_wrapper(tmp_vol_obj.data, tmp_vol_obj.plugin) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTSET, FAIL, "can't set VOL wrapper info")
    vol_wrapper_set = TRUE;

    /* Call the corresponding internal VOL routine */
    if(H5VL__link_create(create_type, vol_obj->data, loc_params, vol_obj->plugin->cls, lcpl_id, lapl_id, dxpl_id, req) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTCREATE, FAIL, "link create failed")

done:
    /* Reset object wrapping info in API context */
    if(vol_wrapper_set && H5VL_reset_vol_wrapper() < 0)
        HDONE_ERROR(H5E_VOL, H5E_CANTRESET, FAIL, "can't reset VOL wrapper info")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_link_create() */


/*-------------------------------------------------------------------------
 * Function:    H5VLlink_create
 *
 * Purpose:     Creates a link
 *
 * Note:	The 'obj' parameter is allowed to be NULL
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLlink_create(H5VL_link_create_type_t create_type, void *obj, H5VL_loc_params_t loc_params,
    hid_t plugin_id, hid_t lcpl_id, hid_t lapl_id, hid_t dxpl_id, void **req)
{
    H5VL_class_t *cls;                  /* VOL plugin's class struct */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API_NOINIT
    H5TRACE8("e", "Vk*xxiiii**x", create_type, obj, loc_params, plugin_id, lcpl_id,
             lapl_id, dxpl_id, req);

    /* Get class pointer */
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    /* Call the corresponding internal VOL routine */
    if(H5VL__link_create(create_type, obj, loc_params, cls, lcpl_id, lapl_id, dxpl_id, req) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTCREATE, FAIL, "unable to create link")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLlink_create() */


/*-------------------------------------------------------------------------
 * Function:	H5VL__link_copy
 *
 * Purpose:	Copys a link from src to dst.
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
static herr_t 
H5VL__link_copy(void *src_obj, H5VL_loc_params_t loc_params1, void *dst_obj,
    H5VL_loc_params_t loc_params2, const H5VL_class_t *cls, hid_t lcpl_id,
    hid_t lapl_id, hid_t dxpl_id, void **req)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_STATIC

    /* Check if the corresponding VOL callback exists */
    if(NULL == cls->link_cls.copy)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no 'link copy' method")

    /* Call the corresponding VOL callback */
    if((cls->link_cls.copy)(src_obj, loc_params1, dst_obj, loc_params2, lcpl_id, lapl_id, dxpl_id, req) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTCOPY, FAIL, "link copy failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__link_copy() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_link_copy
 *
 * Purpose:	Copys a link from src to dst.
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t 
H5VL_link_copy(const H5VL_object_t *src_vol_obj, H5VL_loc_params_t loc_params1,
    const H5VL_object_t *dst_vol_obj, H5VL_loc_params_t loc_params2, hid_t lcpl_id,
    hid_t lapl_id, hid_t dxpl_id, void **req)
{
    const H5VL_object_t *vol_obj;       /* VOL object for object with data */
    hbool_t vol_wrapper_set = FALSE;    /* Whether the VOL object wrapping context was set up */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Set wrapper info in API context */
    vol_obj = (src_vol_obj->data ? src_vol_obj : dst_vol_obj);
    if(H5VL_set_vol_wrapper(vol_obj->data, vol_obj->plugin) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTSET, FAIL, "can't set VOL wrapper info")
    vol_wrapper_set = TRUE;

    /* Call the corresponding internal VOL routine */
    if(H5VL__link_copy((src_vol_obj->data ? src_vol_obj->data : NULL), loc_params1, (dst_vol_obj ? dst_vol_obj->data : NULL), loc_params2, vol_obj->plugin->cls, lcpl_id, lapl_id, dxpl_id, req) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTCOPY, FAIL, "link copy failed")

done:
    /* Reset object wrapping info in API context */
    if(vol_wrapper_set && H5VL_reset_vol_wrapper() < 0)
        HDONE_ERROR(H5E_VOL, H5E_CANTRESET, FAIL, "can't reset VOL wrapper info")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_link_copy() */


/*-------------------------------------------------------------------------
 * Function:    H5VLlink_copy
 *
 * Purpose:     Copies a link to a new location
 *
 * Note:	The 'src_obj' and 'dst_obj' parameters are allowed to be NULL
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLlink_copy(void *src_obj, H5VL_loc_params_t loc_params1, void *dst_obj,
    H5VL_loc_params_t loc_params2, hid_t plugin_id, hid_t lcpl_id,
    hid_t lapl_id, hid_t dxpl_id, void **req)
{
    H5VL_class_t *cls;                  /* VOL plugin's class struct */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API_NOINIT
    H5TRACE9("e", "*xx*xxiiii**x", src_obj, loc_params1, dst_obj, loc_params2,
             plugin_id, lcpl_id, lapl_id, dxpl_id, req);

    /* Get class pointer */
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    /* Call the corresponding internal VOL routine */
    if(H5VL__link_copy(src_obj, loc_params1, dst_obj, loc_params2, cls, lcpl_id, lapl_id, dxpl_id, req) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTCOPY, FAIL, "unable to copy object")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLlink_copy() */


/*-------------------------------------------------------------------------
 * Function:	H5VL__link_move
 *
 * Purpose:	Moves a link from src to dst.
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
static herr_t 
H5VL__link_move(void *src_obj, H5VL_loc_params_t loc_params1, void *dst_obj,
    H5VL_loc_params_t loc_params2, const H5VL_class_t *cls, hid_t lcpl_id,
    hid_t lapl_id, hid_t dxpl_id, void **req)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_STATIC

    /* Check if the corresponding VOL callback exists */
    if(NULL == cls->link_cls.move)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no 'link move' method")

    /* Call the corresponding VOL callback */
    if((cls->link_cls.move)(src_obj, loc_params1, dst_obj, loc_params2, lcpl_id, lapl_id, dxpl_id, req) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTMOVE, FAIL, "link move failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__link_move() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_link_move
 *
 * Purpose:	Moves a link from src to dst.
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t 
H5VL_link_move(const H5VL_object_t *src_vol_obj, H5VL_loc_params_t loc_params1,
    const H5VL_object_t *dst_vol_obj, H5VL_loc_params_t loc_params2, hid_t lcpl_id,
    hid_t lapl_id, hid_t dxpl_id, void **req)
{
    const H5VL_object_t *vol_obj;       /* VOL object for object with data */
    hbool_t vol_wrapper_set = FALSE;    /* Whether the VOL object wrapping context was set up */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Set wrapper info in API context */
    vol_obj = (src_vol_obj ? src_vol_obj : dst_vol_obj);
    if(H5VL_set_vol_wrapper(vol_obj->data, vol_obj->plugin) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTSET, FAIL, "can't set VOL wrapper info")
    vol_wrapper_set = TRUE;

    /* Call the corresponding internal VOL routine */
    if(H5VL__link_move((src_vol_obj ? src_vol_obj->data : NULL), loc_params1, (dst_vol_obj ? dst_vol_obj->data : NULL), loc_params2, vol_obj->plugin->cls, lcpl_id, lapl_id, dxpl_id, req) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTMOVE, FAIL, "link move failed")

done:
    /* Reset object wrapping info in API context */
    if(vol_wrapper_set && H5VL_reset_vol_wrapper() < 0)
        HDONE_ERROR(H5E_VOL, H5E_CANTRESET, FAIL, "can't reset VOL wrapper info")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_link_move() */


/*-------------------------------------------------------------------------
 * Function:    H5VLlink_move
 *
 * Purpose:     Moves a link to another location
 *
 * Note:	The 'src_obj' and 'dst_obj' parameters are allowed to be NULL
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLlink_move(void *src_obj, H5VL_loc_params_t loc_params1, void *dst_obj,
    H5VL_loc_params_t loc_params2, hid_t plugin_id, hid_t lcpl_id,
    hid_t lapl_id, hid_t dxpl_id, void **req)
{
    H5VL_class_t *cls;                  /* VOL plugin's class struct */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API_NOINIT
    H5TRACE9("e", "*xx*xxiiii**x", src_obj, loc_params1, dst_obj, loc_params2,
             plugin_id, lcpl_id, lapl_id, dxpl_id, req);

    /* Get class pointer */
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    /* Call the corresponding internal VOL routine */
    if(H5VL__link_move(src_obj, loc_params1, dst_obj, loc_params2, cls, lcpl_id, lapl_id, dxpl_id, req) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTMOVE, FAIL, "unable to move object")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLlink_move() */


/*-------------------------------------------------------------------------
 * Function:	H5VL__link_get
 *
 * Purpose:	Get specific information about the link through the VOL
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL__link_get(void *obj, H5VL_loc_params_t loc_params, const H5VL_class_t *cls,
    H5VL_link_get_t get_type, hid_t dxpl_id, void **req, va_list arguments)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_STATIC

    /* Check if the corresponding VOL callback exists */
    if(NULL == cls->link_cls.get)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no 'link get' method")

    /* Call the corresponding VOL callback */
    if((cls->link_cls.get)(obj, loc_params, get_type, dxpl_id, req, arguments) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "link get failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__link_get() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_link_get
 *
 * Purpose:	Get specific information about the link through the VOL
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_link_get(const H5VL_object_t *vol_obj, H5VL_loc_params_t loc_params,
    H5VL_link_get_t get_type, hid_t dxpl_id, void **req, ...)
{
    va_list arguments;                  /* Argument list passed from the API call */
    hbool_t arg_started = FALSE;        /* Whether the va_list has been started */
    hbool_t vol_wrapper_set = FALSE;    /* Whether the VOL object wrapping context was set up */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Set wrapper info in API context */
    if(H5VL_set_vol_wrapper(vol_obj->data, vol_obj->plugin) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTSET, FAIL, "can't set VOL wrapper info")
    vol_wrapper_set = TRUE;

    /* Call the corresponding internal VOL routine */
    va_start(arguments, req);
    arg_started = TRUE;
    if(H5VL__link_get(vol_obj->data, loc_params, vol_obj->plugin->cls, get_type, dxpl_id, req, arguments) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "link get failed")

done:
    /* End access to the va_list, if we started it */
    if(arg_started)
        va_end(arguments);

    /* Reset object wrapping info in API context */
    if(vol_wrapper_set && H5VL_reset_vol_wrapper() < 0)
        HDONE_ERROR(H5E_VOL, H5E_CANTRESET, FAIL, "can't reset VOL wrapper info")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_link_get() */


/*-------------------------------------------------------------------------
 * Function:    H5VLlink_get
 *
 * Purpose:     Gets information about a link
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLlink_get(void *obj, H5VL_loc_params_t loc_params, hid_t plugin_id, H5VL_link_get_t get_type,
    hid_t dxpl_id, void **req, va_list arguments)
{
    H5VL_class_t *cls;                  /* VOL plugin's class struct */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API_NOINIT
    H5TRACE7("e", "*xxiVli**xx", obj, loc_params, plugin_id, get_type, dxpl_id, req,
             arguments);

    /* Check args and get class pointer */
    if(NULL == obj)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object")
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    /* Call the corresponding internal VOL routine */
    if(H5VL__link_get(obj, loc_params, cls, get_type, dxpl_id, req, arguments) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "unable to execute link get callback")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLlink_get() */


/*-------------------------------------------------------------------------
 * Function:	H5VL__link_specific
 *
 * Purpose:	Specific operation on links through the VOL
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL__link_specific(void *obj, H5VL_loc_params_t loc_params, const H5VL_class_t *cls, 
    H5VL_link_specific_t specific_type, hid_t dxpl_id, void **req, va_list arguments)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_STATIC

    /* Check if the corresponding VOL callback exists */
    if(NULL == cls->link_cls.specific)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no 'link specific' method")

    /* Call the corresponding VOL callback */
    if((ret_value = (cls->link_cls.specific)(obj, loc_params, specific_type, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "unable to execute link specific callback")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__link_specific() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_link_specific
 *
 * Purpose:	Specific operation on links through the VOL
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_link_specific(const H5VL_object_t *vol_obj, H5VL_loc_params_t loc_params,
    H5VL_link_specific_t specific_type, hid_t dxpl_id, void **req, ...)
{
    va_list arguments;                  /* Argument list passed from the API call */
    hbool_t arg_started = FALSE;        /* Whether the va_list has been started */
    hbool_t vol_wrapper_set = FALSE;    /* Whether the VOL object wrapping context was set up */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Set wrapper info in API context */
    if(H5VL_set_vol_wrapper(vol_obj->data, vol_obj->plugin) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTSET, FAIL, "can't set VOL wrapper info")
    vol_wrapper_set = TRUE;

    /* Call the corresponding internal VOL routine */
    va_start(arguments, req);
    arg_started = TRUE;
    if((ret_value = H5VL__link_specific(vol_obj->data, loc_params, vol_obj->plugin->cls, specific_type, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "unable to execute link specific callback")

done:
    /* End access to the va_list, if we started it */
    if(arg_started)
        va_end(arguments);

    /* Reset object wrapping info in API context */
    if(vol_wrapper_set && H5VL_reset_vol_wrapper() < 0)
        HDONE_ERROR(H5E_VOL, H5E_CANTRESET, FAIL, "can't reset VOL wrapper info")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_link_specific() */


/*-------------------------------------------------------------------------
 * Function:    H5VLlink_specific
 *
 * Purpose:     Performs a plugin-specific operation on a link
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLlink_specific(void *obj, H5VL_loc_params_t loc_params, hid_t plugin_id,
    H5VL_link_specific_t specific_type, hid_t dxpl_id, void **req, va_list arguments)
{
    H5VL_class_t *cls;                  /* VOL plugin's class struct */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API_NOINIT
    H5TRACE7("e", "*xxiVmi**xx", obj, loc_params, plugin_id, specific_type,
             dxpl_id, req, arguments);

    /* Check args and get class pointer */
    if(NULL == obj)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object")
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    /* Call the corresponding internal VOL routine */
    if((ret_value = H5VL__link_specific(obj, loc_params, cls, specific_type, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "unable to execute link specific callback")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLlink_specific() */


/*-------------------------------------------------------------------------
 * Function:	H5VL__link_optional
 *
 * Purpose:	Optional operation specific to plugins.
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL__link_optional(void *obj, const H5VL_class_t *cls, hid_t dxpl_id,
    void **req, va_list arguments)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_STATIC

    /* Check if the corresponding VOL callback exists */
    if(NULL == cls->link_cls.optional)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no 'link optional' method")

    /* Call the corresponding VOL callback */
    if((cls->link_cls.optional)(obj, dxpl_id, req, arguments) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "unable to execute link optional callback")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__link_optional() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_link_optional
 *
 * Purpose:	Optional operation specific to plugins.
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_link_optional(const H5VL_object_t *vol_obj, hid_t dxpl_id, void **req, ...)
{
    va_list arguments;                  /* Argument list passed from the API call */
    hbool_t arg_started = FALSE;        /* Whether the va_list has been started */
    hbool_t vol_wrapper_set = FALSE;    /* Whether the VOL object wrapping context was set up */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Set wrapper info in API context */
    if(H5VL_set_vol_wrapper(vol_obj->data, vol_obj->plugin) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTSET, FAIL, "can't set VOL wrapper info")
    vol_wrapper_set = TRUE;

    /* Call the corresponding internal VOL routine */
    va_start(arguments, req);
    arg_started = TRUE;
    if(H5VL__link_optional(vol_obj->data, vol_obj->plugin->cls, dxpl_id, req, arguments) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "unable to execute link optional callback")

done:
    /* End access to the va_list, if we started it */
    if(arg_started)
        va_end(arguments);

    /* Reset object wrapping info in API context */
    if(vol_wrapper_set && H5VL_reset_vol_wrapper() < 0)
        HDONE_ERROR(H5E_VOL, H5E_CANTRESET, FAIL, "can't reset VOL wrapper info")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_link_optional() */


/*-------------------------------------------------------------------------
 * Function:    H5VLlink_optional
 *
 * Purpose:     Performs an optional plugin-specific operation on a link
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLlink_optional(void *obj, hid_t plugin_id, hid_t dxpl_id, void **req, va_list arguments)
{
    H5VL_class_t *cls;                  /* VOL plugin's class struct */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API_NOINIT
    H5TRACE5("e", "*xii**xx", obj, plugin_id, dxpl_id, req, arguments);

    /* Check args and get class pointer */
    if(NULL == obj)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object")
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    /* Call the corresponding internal VOL routine */
    if(H5VL__link_optional(obj, cls, dxpl_id, req, arguments) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "unable to execute link optional callback")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLlink_optional() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_object_open
 *
 * Purpose:	Opens a object through the VOL
 *
 * Return:      Success: Pointer to the object
 *		Failure: NULL
 *
 *-------------------------------------------------------------------------
 */
void *
H5VL_object_open(void *obj, H5VL_loc_params_t params, const H5VL_class_t *cls, H5I_type_t *opened_type,
    hid_t dxpl_id, void **req)
{
    void *ret_value = NULL;              /* Return value */

    FUNC_ENTER_NOAPI(NULL)

    /* Check if the corresponding VOL callback exists */
    if(NULL == cls->object_cls.open)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, NULL, "VOL plugin has no 'object open' method")

    /* Call the corresponding VOL callback */
    if(NULL == (ret_value = (cls->object_cls.open)(obj, params, opened_type, dxpl_id, req)))
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPENOBJ, NULL, "object open failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_object_open() */


/*-------------------------------------------------------------------------
 * Function:    H5VLobject_open
 *
 * Purpose:     Opens an object
 *
 * Return:      Success:    Pointer to the object
 *              Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
void *
H5VLobject_open(void *obj, H5VL_loc_params_t params, hid_t plugin_id, H5I_type_t *opened_type,
    hid_t dxpl_id, void **req)
{
    H5VL_class_t *cls;                  /* VOL plugin's class struct */
    void *ret_value = NULL;             /* Return value */

    FUNC_ENTER_API_NOINIT
    H5TRACE6("*x", "*xxi*Iti**x", obj, params, plugin_id, opened_type, dxpl_id, req);

    /* Check args and get class pointer */
    if(NULL == obj)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "invalid object")
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a VOL plugin ID")

    /* Call the corresponding internal VOL routine */
    if(NULL == (ret_value = H5VL_object_open(obj, params, cls, opened_type, dxpl_id, req)))
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPENOBJ, NULL, "unable to open object")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLobject_open() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_object_copy
 *
 * Purpose:	Copies an object to another destination through the VOL
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t 
H5VL_object_copy(void *src_obj, H5VL_loc_params_t loc_params1, const H5VL_class_t *cls1,
    const char *src_name, void *dst_obj, H5VL_loc_params_t loc_params2,
    const H5VL_class_t *cls2, const char *dst_name, hid_t ocpypl_id,
    hid_t lcpl_id, hid_t dxpl_id, void **req)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Make sure that the VOL plugins are the same */
    if(cls1->value != cls2->value)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "Objects are accessed through different VOL plugins and can't be linked")

    /* Check if the corresponding VOL callback exists */
    if(NULL == cls1->object_cls.copy)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no 'object copy' method")

    /* Call the corresponding VOL callback */
    if((cls1->object_cls.copy)(src_obj, loc_params1, src_name, dst_obj, loc_params2, dst_name, ocpypl_id, lcpl_id, dxpl_id, req) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTCOPY, FAIL, "object copy failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_object_copy() */


/*-------------------------------------------------------------------------
 * Function:    H5VLobject_copy
 *
 * Purpose:     Copies an object to another location
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLobject_copy(void *src_obj, H5VL_loc_params_t loc_params1, hid_t plugin_id1,
    const char *src_name, void *dst_obj, H5VL_loc_params_t loc_params2,
    hid_t plugin_id2, const char *dst_name, hid_t ocpypl_id, hid_t lcpl_id,
    hid_t dxpl_id, void **req)
{
    H5VL_class_t *cls1;                 /* VOL plugin's class struct */
    H5VL_class_t *cls2;                 /* VOL plugin's class struct */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API_NOINIT
    H5TRACE12("e", "*xxi*s*xxi*siii**x", src_obj, loc_params1, plugin_id1,
             src_name, dst_obj, loc_params2, plugin_id2, dst_name, ocpypl_id,
             lcpl_id, dxpl_id, req);

    /* Check args and get class pointers */
    if(NULL == src_obj || NULL == dst_obj)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object")
    if(NULL == (cls1 = (H5VL_class_t *)H5I_object_verify(plugin_id1, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")
    if(NULL == (cls2 = (H5VL_class_t *)H5I_object_verify(plugin_id2, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    /* Call the corresponding internal VOL routine */
    if(H5VL_object_copy(src_obj, loc_params1, cls1, src_name, dst_obj, loc_params2, cls2, dst_name, ocpypl_id, lcpl_id, dxpl_id, req) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTCOPY, FAIL, "unable to copy object")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLobject_copy() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_object_get
 *
 * Purpose:	Get specific information about the object through the VOL
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_object_get(void *obj, H5VL_loc_params_t loc_params, const H5VL_class_t *cls,
    H5VL_object_get_t get_type, hid_t dxpl_id, void **req, ...)
{
    va_list arguments;                  /* Argument list passed from the API call */
    hbool_t arg_started = FALSE;        /* Whether the va_list has been started */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Check if the corresponding VOL callback exists */
    if(NULL == cls->object_cls.get)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no 'object get' method")

    /* Call the corresponding VOL callback */
    va_start(arguments, req);
    arg_started = TRUE;
    if((cls->object_cls.get)(obj, loc_params, get_type, dxpl_id, req, arguments) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "get failed")

done:
    /* End access to the va_list, if we started it */
    if(arg_started)
        va_end(arguments);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_object_get() */


/*-------------------------------------------------------------------------
 * Function:    H5VLobject_get
 *
 * Purpose:     Gets information about an object
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLobject_get(void *obj, H5VL_loc_params_t loc_params, hid_t plugin_id, H5VL_object_get_t get_type,
    hid_t dxpl_id, void **req, va_list arguments)
{
    H5VL_class_t *cls;                  /* VOL plugin's class struct */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API_NOINIT
    H5TRACE7("e", "*xxiVni**xx", obj, loc_params, plugin_id, get_type, dxpl_id, req,
             arguments);

    /* Check args and get class pointer */
    if(NULL == obj)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object")
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    /* Check if the corresponding VOL callback exists */
    if(NULL == cls->object_cls.get)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no `object get' method")

    /* Bypass the H5VLint layer, calling the VOL callback directly */
    if((cls->object_cls.get)(obj, loc_params, get_type, dxpl_id, req, arguments) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "unable to execute object get callback")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLobject_get() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_object_specific
 *
 * Purpose:	specific operation on objects through the VOL
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_object_specific(void *obj, H5VL_loc_params_t loc_params, const H5VL_class_t *cls, 
    H5VL_object_specific_t specific_type, hid_t dxpl_id, void **req, ...)
{
    va_list arguments;                  /* Argument list passed from the API call */
    hbool_t arg_started = FALSE;        /* Whether the va_list has been started */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Check if the corresponding VOL callback exists */
    if(NULL == cls->object_cls.specific)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no 'object specific' method")

    /* Call the corresponding VOL callback */
    va_start(arguments, req);
    arg_started = TRUE;
    if((ret_value = (cls->object_cls.specific)(obj, loc_params, specific_type, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "object specific failed")

done:
    /* End access to the va_list, if we started it */
    if(arg_started)
        va_end(arguments);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_object_specific() */


/*-------------------------------------------------------------------------
 * Function:    H5VLobject_specific
 *
 * Purpose:     Performs a plugin-specific operation on an object
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLobject_specific(void *obj, H5VL_loc_params_t loc_params, hid_t plugin_id,
    H5VL_object_specific_t specific_type, hid_t dxpl_id, void **req, va_list arguments)
{
    H5VL_class_t *cls;                  /* VOL plugin's class struct */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API_NOINIT
    H5TRACE7("e", "*xxiVoi**xx", obj, loc_params, plugin_id, specific_type,
             dxpl_id, req, arguments);

    /* Check args and get class pointer */
    if(NULL == obj)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object")
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    /* Check if the corresponding VOL callback exists */
    if(NULL == cls->object_cls.specific)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no `object specific' method")

    /* Bypass the H5VLint layer, calling the VOL callback directly */
    if((ret_value = (cls->object_cls.specific)(obj, loc_params, specific_type, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "unable to execute object specific callback")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLobject_specific() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_object_optional
 *
 * Purpose:	optional operation specific to plugins.
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_object_optional(void *obj, const H5VL_class_t *cls, hid_t dxpl_id,
    void **req, ...)
{
    va_list arguments;                  /* Argument list passed from the API call */
    hbool_t arg_started = FALSE;        /* Whether the va_list has been started */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Check if the corresponding VOL callback exists */
    if(NULL == cls->object_cls.optional)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no 'object optional' method")

    /* Call the corresponding VOL callback */
    va_start(arguments, req);
    arg_started = TRUE;
    if((cls->object_cls.optional)(obj, dxpl_id, req, arguments) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "unable to execute object optional callback")

done:
    /* End access to the va_list, if we started it */
    if(arg_started)
        va_end(arguments);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_object_optional() */


/*-------------------------------------------------------------------------
 * Function:    H5VLobject_optional
 *
 * Purpose:     Performs an optional plugin-specific operation on an object
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLobject_optional(void *obj, hid_t plugin_id, hid_t dxpl_id, void **req, va_list arguments)
{
    H5VL_class_t *cls;                  /* VOL plugin's class struct */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API_NOINIT
    H5TRACE5("e", "*xii**xx", obj, plugin_id, dxpl_id, req, arguments);

    /* Check args and get class pointer */
    if(NULL == obj)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object")
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    /* Check if the corresponding VOL callback exists */
    if(NULL == cls->object_cls.optional)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no `object optional' method")

    /* Bypass the H5VLint layer, calling the VOL callback directly */
    if((cls->object_cls.optional)(obj, dxpl_id, req, arguments) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "unable to execute object optional callback")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLobject_optional() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_datatype_commit
 *
 * Purpose:	Commits a datatype to the file through the VOL
 *
 * Return:      Success:    Pointer to the new datatype
 *              Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
void *
H5VL_datatype_commit(void *obj, H5VL_loc_params_t loc_params, const H5VL_class_t *cls,
    const char *name, hid_t type_id, hid_t lcpl_id, hid_t tcpl_id, hid_t tapl_id, 
    hid_t dxpl_id, void **req)
{
    void *ret_value = NULL;              /* Return value */

    FUNC_ENTER_NOAPI(NULL)

    /* Check if the corresponding VOL callback exists */
    if(NULL == cls->datatype_cls.commit)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, NULL, "VOL plugin has no 'datatype commit' method")

    /* Call the corresponding VOL callback */
    if(NULL == (ret_value = (cls->datatype_cls.commit)(obj, loc_params, name, type_id, lcpl_id, tcpl_id, tapl_id, dxpl_id, req)))
        HGOTO_ERROR(H5E_VOL, H5E_CANTCREATE, NULL, "datatype commit failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_datatype_commit() */


/*-------------------------------------------------------------------------
 * Function:    H5VLdatatype_commit
 *
 * Purpose:     Commits a datatype to the file
 *
 * Return:      Success:    Pointer to the new datatype
 *              Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
void *
H5VLdatatype_commit(void *obj, H5VL_loc_params_t loc_params, hid_t plugin_id,
    const char *name, hid_t type_id, hid_t lcpl_id, hid_t tcpl_id,
    hid_t tapl_id, hid_t dxpl_id, void **req)
{
    H5VL_class_t *cls;                  /* VOL plugin's class struct */
    void *ret_value = NULL;             /* Return value */

    FUNC_ENTER_API_NOINIT
    H5TRACE10("*x", "*xxi*siiiii**x", obj, loc_params, plugin_id, name, type_id,
             lcpl_id, tcpl_id, tapl_id, dxpl_id, req);

    /* Check args and get class pointer */
    if(NULL == obj)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "invalid object")
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a VOL plugin ID")

    /* Call the corresponding internal VOL routine */
    if(NULL == (ret_value = H5VL_datatype_commit(obj, loc_params, cls, name, type_id, lcpl_id, tcpl_id, tapl_id, dxpl_id, req)))
        HGOTO_ERROR(H5E_VOL, H5E_CANTCREATE, NULL, "unable to commit datatype")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLdatatype_commit() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_datatype_open
 *
 * Purpose:	Opens a named datatype through the VOL
 *
 * Return:      Success:    Pointer to the datatype
 *              Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
void *
H5VL_datatype_open(void *obj, H5VL_loc_params_t loc_params, const H5VL_class_t *cls,
    const char *name, hid_t tapl_id, hid_t dxpl_id, void **req)
{
    void *ret_value = NULL;              /* Return value */

    FUNC_ENTER_NOAPI(NULL)

    /* Check if the corresponding VOL callback exists */
    if(NULL == cls->datatype_cls.open)
        HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, NULL, "no datatype open callback")

    /* Call the corresponding VOL callback */
    if(NULL == (ret_value = (cls->datatype_cls.open) (obj, loc_params, name, tapl_id, dxpl_id, req)))
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPENOBJ, NULL, "open failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_datatype_open() */


/*-------------------------------------------------------------------------
 * Function:    H5VLdatatype_open
 *
 * Purpose:     Opens a named datatype
 *
 * Return:      Success:    Pointer to the datatype
 *              Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
void *
H5VLdatatype_open(void *obj, H5VL_loc_params_t loc_params, hid_t plugin_id,
    const char *name, hid_t tapl_id, hid_t dxpl_id, void **req)
{
    H5VL_class_t *cls;                  /* VOL plugin's class struct */
    void *ret_value = NULL;             /* Return value */

    FUNC_ENTER_API_NOINIT
    H5TRACE7("*x", "*xxi*sii**x", obj, loc_params, plugin_id, name, tapl_id,
             dxpl_id, req);

    /* Check args and get class pointer */
    if(NULL == obj)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "invalid object")
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a VOL plugin ID")

    /* Call the corresponding internal VOL routine */
    if(NULL == (ret_value = H5VL_datatype_open(obj, loc_params, cls, name, tapl_id, dxpl_id, req)))
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPENOBJ, NULL, "unable to open datatype")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLdatatype_open() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_datatype_get
 *
 * Purpose:     Get specific information about the datatype through the VOL
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_datatype_get(void *obj, const H5VL_class_t *cls, H5VL_datatype_get_t get_type, 
    hid_t dxpl_id, void **req, ...)
{
    va_list arguments;                  /* Argument list passed from the API call */
    hbool_t arg_started = FALSE;        /* Whether the va_list has been started */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Check if the corresponding VOL callback exists */
    if(NULL == cls->datatype_cls.get)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no 'datatype get' method")

    /* Call the corresponding VOL callback */
    va_start(arguments, req);
    arg_started = TRUE;
    if((cls->datatype_cls.get)(obj, get_type, dxpl_id, req, arguments) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "get failed")

done:
    /* End access to the va_list, if we started it */
    if(arg_started)
        va_end(arguments);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_datatype_get() */


/*-------------------------------------------------------------------------
 * Function:    H5VLdatatype_get
 *
 * Purpose:     Gets information about the datatype
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLdatatype_get(void *obj, hid_t plugin_id, H5VL_datatype_get_t get_type,
    hid_t dxpl_id, void **req, va_list arguments)
{
    H5VL_class_t *cls;                  /* VOL plugin's class struct */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API_NOINIT
    H5TRACE6("e", "*xiVei**xx", obj, plugin_id, get_type, dxpl_id, req, arguments);

    /* Check args and get class pointer */
    if(NULL == obj)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object")
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    /* Check if the corresponding VOL callback exists */
    if(NULL == cls->datatype_cls.get)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no `datatype get' method")

    /* Bypass the H5VLint layer, calling the VOL callback directly */
    if((cls->datatype_cls.get)(obj, get_type, dxpl_id, req, arguments) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "unable to execute datatype get callback")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLdatatype_get() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_datatype_specific
 *
 * Purpose:	specific operation on datatypes through the VOL
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_datatype_specific(void *obj, const H5VL_class_t *cls, H5VL_datatype_specific_t specific_type, 
    hid_t dxpl_id, void **req, ...)
{
    va_list arguments;                  /* Argument list passed from the API call */
    hbool_t arg_started = FALSE;        /* Whether the va_list has been started */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Check if the corresponding VOL callback exists */
    if(NULL == cls->datatype_cls.specific)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no 'datatype specific' method")

    /* Call the corresponding VOL callback */
    va_start(arguments, req);
    arg_started = TRUE;
    if((cls->datatype_cls.specific)(obj, specific_type, dxpl_id, req, arguments) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "unable to execute datatype specific callback")

done:
    /* End access to the va_list, if we started it */
    if(arg_started)
        va_end(arguments);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_datatype_specific() */


/*-------------------------------------------------------------------------
 * Function:    H5VLdatatype_specific
 *
 * Purpose:     Performs a plugin-specific operation on a datatype
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLdatatype_specific(void *obj, hid_t plugin_id, H5VL_datatype_specific_t specific_type,
    hid_t dxpl_id, void **req, va_list arguments)
{
    H5VL_class_t *cls;                  /* VOL plugin's class struct */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API_NOINIT
    H5TRACE6("e", "*xiVfi**xx", obj, plugin_id, specific_type, dxpl_id, req,
             arguments);

    /* Check args and get class pointer */
    if(NULL == obj)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object")
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    /* Check if the corresponding VOL callback exists */
    if(NULL == cls->datatype_cls.specific)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no `datatype specific' method")

    /* Bypass the H5VLint layer, calling the VOL callback directly */
    if((cls->datatype_cls.specific)(obj, specific_type, dxpl_id, req, arguments) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "unable to execute datatype specific callback")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLdatatype_specific() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_datatype_optional
 *
 * Purpose:	optional operation specific to plugins.
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_datatype_optional(void *obj, const H5VL_class_t *cls, hid_t dxpl_id,
    void **req, ...)
{
    va_list arguments;                  /* Argument list passed from the API call */
    hbool_t arg_started = FALSE;        /* Whether the va_list has been started */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Check if the corresponding VOL callback exists */
    if(NULL == cls->datatype_cls.optional)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no 'datatype optional' method")

    /* Call the corresponding VOL callback */
    va_start(arguments, req);
    arg_started = TRUE;
    if((cls->datatype_cls.optional)(obj, dxpl_id, req, arguments) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "unable to execute datatype optional callback")

done:
    /* End access to the va_list, if we started it */
    if(arg_started)
        va_end(arguments);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_datatype_optional() */


/*-------------------------------------------------------------------------
 * Function:    H5VLdatatype_optional
 *
 * Purpose:     Performs an optional plugin-specific operation on a datatype
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLdatatype_optional(void *obj, hid_t plugin_id, hid_t dxpl_id, void **req,
    va_list arguments)
{
    H5VL_class_t *cls;                  /* VOL plugin's class struct */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API_NOINIT
    H5TRACE5("e", "*xii**xx", obj, plugin_id, dxpl_id, req, arguments);

    /* Check args and get class pointer */
    if(NULL == obj)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object")
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    /* Check if the corresponding VOL callback exists */
    if(NULL == cls->datatype_cls.optional)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no `datatype optional' method")

    /* Bypass the H5VLint layer, calling the VOL callback directly */
    if((cls->datatype_cls.optional)(obj, dxpl_id, req, arguments) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "unable to execute datatype optional callback")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLdatatype_optional() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_datatype_close
 *
 * Purpose:     Closes a datatype through the VOL
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_datatype_close(void *dt, const H5VL_class_t *cls, hid_t dxpl_id, void **req)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Check if the corresponding VOL callback exists */
    if(NULL == cls->datatype_cls.close)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no 'datatype close' method")

    /* Call the corresponding VOL callback */
    if((cls->datatype_cls.close)(dt, dxpl_id, req) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTCLOSEOBJ, FAIL, "datatype close failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_datatype_close() */


/*-------------------------------------------------------------------------
 * Function:    H5VLdatatype_close
 *
 * Purpose:     Closes a datatype
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLdatatype_close(void *dt, hid_t plugin_id, hid_t dxpl_id, void **req)
{
    H5VL_class_t *cls;                  /* VOL plugin's class struct */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API_NOINIT
    H5TRACE4("e", "*xii**x", dt, plugin_id, dxpl_id, req);

    /* Check args and get class pointer */
    if(NULL == dt)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object")
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    /* Call the corresponding internal VOL routine */
    if(H5VL_datatype_close(dt, cls, dxpl_id, req) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTCLOSEOBJ, FAIL, "unable to close datatype")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLdatatype_close() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_request_cancel
 *
 * Purpose:     Cancels an asynchronous request through the VOL
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_request_cancel(void **req, const H5VL_class_t *cls, H5ES_status_t *status)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity check */
    HDassert(req);
    HDassert(cls);
    HDassert(status);

    /* Check if the corresponding VOL callback exists */
    if(NULL == cls->async_cls.cancel)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no 'async cancel' method")

    /* Call the corresponding VOL callback */
    if((cls->async_cls.cancel)(req, status) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTRELEASE, FAIL, "request cancel failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_request_cancel() */


/*-------------------------------------------------------------------------
 * Function:    H5VLrequest_cancel
 *
 * Purpose:     Cancels a request
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLrequest_cancel(void **req, hid_t plugin_id, H5ES_status_t *status)
{
    H5VL_class_t *cls;                  /* VOL plugin's class struct */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API_NOINIT
    H5TRACE3("e", "**xi*Es", req, plugin_id, status);

    /* Get class pointer */
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    /* Call the corresponding internal VOL routine */
    if(H5VL_request_cancel(req, cls, status) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTRELEASE, FAIL, "unable to cancel request")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLrequest_cancel() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_request_test
 *
 * Purpose:     Tests an asynchronous request through the VOL
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_request_test(void **req, const H5VL_class_t *cls, H5ES_status_t *status)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(req);
    HDassert(cls);
    HDassert(status);

    /* Check if the corresponding VOL callback exists */
    if(NULL == cls->async_cls.test)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no 'async test' method")

    /* Call the corresponding VOL callback */
    if((cls->async_cls.test)(req, status) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "request test failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_request_test() */


/*-------------------------------------------------------------------------
 * Function:    H5VLrequest_test
 *
 * Purpose:     Tests a request
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLrequest_test(void **req, hid_t plugin_id, H5ES_status_t *status)
{
    H5VL_class_t *cls;                  /* VOL plugin's class struct */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API_NOINIT
    H5TRACE3("e", "**xi*Es", req, plugin_id, status);

    /* Get class pointer */
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    /* Call the corresponding internal VOL routine */
    if(H5VL_request_test(req, cls, status) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "unable to test request")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLrequest_test() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_request_wait
 *
 * Purpose:     Waits on an asychronous request through the VOL
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_request_wait(void **req, const H5VL_class_t *cls, H5ES_status_t *status)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(req);
    HDassert(cls);
    HDassert(status);

    /* Check if the corresponding VOL callback exists */
    if(NULL == cls->async_cls.wait)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no 'async wait' method")

    /* Call the corresponding VOL callback */
    if((cls->async_cls.wait)(req, status) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTRELEASE, FAIL, "request wait failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_request_wait() */


/*-------------------------------------------------------------------------
 * Function:    H5VLrequest_wait
 *
 * Purpose:     Waits on a request
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLrequest_wait(void **req, hid_t plugin_id, H5ES_status_t *status)
{
    H5VL_class_t *cls;                  /* VOL plugin's class struct */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API_NOINIT
    H5TRACE3("e", "**xi*Es", req, plugin_id, status);

    /* Get class pointer */
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    /* Call the corresponding internal VOL routine */
    if(H5VL_request_wait(req, cls, status) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTRELEASE, FAIL, "unable to wait on request")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLrequest_wait() */

