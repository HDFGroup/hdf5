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
 * Purpose:	The Virtual Object Layer as described in documentation.
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

#include "H5private.h"          /* Generic Functions                    */
#include "H5Aprivate.h"         /* Attributes                           */
#include "H5Eprivate.h"         /* Error handling                       */
#include "H5Iprivate.h"         /* IDs                                  */
#include "H5MMprivate.h"        /* Memory management                    */
#include "H5PLprivate.h"        /* Plugins                              */
#include "H5VLpkg.h"            /* Virtual Object Layer                 */


/****************/
/* Local Macros */
/****************/

/******************/
/* Local Typedefs */
/******************/

/* Information needed for iterating over the registered VOL plugin hid_t IDs.
 * The name of the new VOL plugin that is being registered is stored in the
 * name field and the found_id field is initialized to H5I_INVALID_HID (-1).
 * If we find a VOL plugin with the same name, we set the found_id field to
 * the existing ID for return to the function.
 */
typedef struct {
    const char *name;           /* The name of the VOL plugin to check */
    hid_t found_id;             /* The library ID if we found a match */
} H5VL_get_plugin_ud_t;

/********************/
/* Local Prototypes */
/********************/
static int H5VL__get_plugin_cb(void *obj, hid_t id, void *_op_data);

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
 * Function:    H5VL__get_plugin_cb
 *
 * Purpose:     Callback routine to search through registered VOLs
 *
 * Return:      Success:    H5_ITER_STOP if the class and op_data name
 *                          members match. H5_ITER_CONT otherwise.
 *
 *              Failure:    Can't fail
 *
 *-------------------------------------------------------------------------
 */
static int
H5VL__get_plugin_cb(void *obj, hid_t id, void *_op_data)
{
    H5VL_get_plugin_ud_t *op_data = (H5VL_get_plugin_ud_t *)_op_data; /* User data for callback */
    H5VL_class_t *cls = (H5VL_class_t *)obj;
    int ret_value = H5_ITER_CONT;     /* Callback return value */

    FUNC_ENTER_STATIC_NOERR

    if (0 == HDstrcmp(cls->name, op_data->name)) {
        op_data->found_id = id;
        ret_value = H5_ITER_STOP;
    }

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__get_plugin_cb() */


/*-------------------------------------------------------------------------
 * Function:    H5VLregister_plugin
 *
 * Purpose:     Registers a new VOL plugin as a member of the virtual object
 *              layer class.
 *
 * Return:      Success:    A VOL plugin ID which is good until the
 *                          library is closed or the plugin is
 *                          unregistered.
 *
 *              Failure:    H5I_INVALID_HID
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5VLregister_plugin(const H5VL_class_t *cls, hid_t vipl_id)
{
    H5VL_get_plugin_ud_t op_data;       /* Callback info for plugin search */
    hid_t ret_value = H5I_INVALID_HID;

    FUNC_ENTER_API(H5I_INVALID_HID)
    H5TRACE2("i", "*xi", cls, vipl_id);

    /* Check arguments */
    if (!cls)
        HGOTO_ERROR(H5E_ARGS, H5E_UNINITIALIZED, H5I_INVALID_HID, "VOL plugin class pointer cannot be NULL")
    if (!cls->name)
        HGOTO_ERROR(H5E_VOL, H5E_CANTREGISTER, H5I_INVALID_HID, "VOL plugin class name cannot be the NULL pointer")
    if (0 == HDstrlen(cls->name))
        HGOTO_ERROR(H5E_VOL, H5E_CANTREGISTER, H5I_INVALID_HID, "VOL plugin class name cannot be the empty string")
    if (cls->info_copy && !cls->info_free)
        HGOTO_ERROR(H5E_VOL, H5E_CANTREGISTER, H5I_INVALID_HID, "VOL plugin must provide free callback for VOL info objects when a copy callback is provided")
    if (cls->get_wrap_ctx && !cls->free_wrap_ctx)
        HGOTO_ERROR(H5E_VOL, H5E_CANTREGISTER, H5I_INVALID_HID, "VOL plugin must provide free callback for object wrapping contexts when a get callback is provided")

    op_data.found_id = H5I_INVALID_HID;
    op_data.name = cls->name;

    /* check if plugin is already registered */
    if (H5I_iterate(H5I_VOL, H5VL__get_plugin_cb, &op_data, TRUE) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_BADITER, H5I_INVALID_HID, "can't iterate over VOL IDs")

    /* Increment the ref count on the existing VOL plugin ID, if it's already registered */
    if(op_data.found_id != H5I_INVALID_HID) {
        if (H5I_inc_ref(op_data.found_id, TRUE) < 0)
            HGOTO_ERROR(H5E_VOL, H5E_CANTINC, H5I_INVALID_HID, "unable to increment ref count on VOL plugin")
        ret_value = op_data.found_id;
    } /* end if */
    else {
        /* Create a new class ID */
        if ((ret_value = H5VL_register_plugin(cls, TRUE, vipl_id)) < 0)
            HGOTO_ERROR(H5E_VOL, H5E_CANTREGISTER, H5I_INVALID_HID, "unable to register VOL plugin")
    } /* end else */

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLregister_driver() */


/*-------------------------------------------------------------------------
 * Function:    H5VLregister_plugin_by_name
 *
 * Purpose:     Registers a new VOL plugin as a member of the virtual object
 *              layer class.
 *
 * Return:      Success:    A VOL plugin ID which is good until the
 *                          library is closed or the plugin is
 *                          unregistered.
 *
 *              Failure:    H5I_INVALID_HID
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5VLregister_plugin_by_name(const char *name, hid_t vipl_id)
{
    H5VL_get_plugin_ud_t op_data;       /* Callback info for plugin search */
    hid_t ret_value = H5I_INVALID_HID;

    FUNC_ENTER_API(H5I_INVALID_HID)
    H5TRACE2("i", "*si", name, vipl_id);

    /* Check arguments */
    if (!name)
        HGOTO_ERROR(H5E_ARGS, H5E_UNINITIALIZED, H5I_INVALID_HID, "null VOL plugin name is disallowed")
    if (0 == HDstrlen(name))
        HGOTO_ERROR(H5E_ARGS, H5E_UNINITIALIZED, H5I_INVALID_HID, "zero-length VOL plugin name is disallowed")

    op_data.found_id = H5I_INVALID_HID;
    op_data.name = name;

    /* Check if plugin is already registered */
    if (H5I_iterate(H5I_VOL, H5VL__get_plugin_cb, &op_data, TRUE) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_BADITER, H5I_INVALID_HID, "can't iterate over VOL ids")

    /* If plugin alread registered, increment ref count on ID and return ID */
    if (op_data.found_id != H5I_INVALID_HID) {
        if (H5I_inc_ref(op_data.found_id, TRUE) < 0)
            HGOTO_ERROR(H5E_VOL, H5E_CANTINC, H5I_INVALID_HID, "unable to increment ref count on VOL plugin")
        ret_value = op_data.found_id;
    } /* end if */
    else {
        H5PL_key_t key;
        const H5VL_class_t *cls;

        /* Try loading the plugin */
        key.name = name;
        if (NULL == (cls = (const H5VL_class_t *)H5PL_load(H5PL_TYPE_VOL, key)))
            HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, H5I_INVALID_HID, "unable to load VOL plugin")

        /* Register the plugin we loaded */
        if ((ret_value = H5VL_register_plugin(cls, TRUE, vipl_id)) < 0)
            HGOTO_ERROR(H5E_VOL, H5E_CANTREGISTER, H5I_INVALID_HID, "unable to register VOL plugin ID")
    } /* end else */

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLregister_driver_by_name() */


/*-------------------------------------------------------------------------
 * Function:    H5VLclose
 *
 * Purpose:     Closes a VOL plugin ID.  This in no way affects
 *              file access property lists which have been defined to use
 *              this VOL plugin or files which are already opened under with
 *              this plugin.
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLclose(hid_t vol_id)
{
    herr_t ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE1("e", "i", vol_id);

    /* Check args */
    if(NULL == H5I_object_verify(vol_id, H5I_VOL))
        HGOTO_ERROR(H5E_VOL, H5E_BADTYPE, FAIL, "not a VOL plugin")

    /* Decrement the ref count on the ID, possibly releasing the VOL plugin */
    if(H5I_dec_app_ref(vol_id) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTDEC, FAIL, "unable to close VOL plugin ID")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLclose() */


/*-------------------------------------------------------------------------
 * Function:    H5VLunregister_plugin
 *
 * Purpose:     Removes a VOL plugin ID from the library. This in no way affects
 *              file access property lists which have been defined to use
 *              this VOL plugin or files which are already opened under with
 *              this plugin.
 *
 * Return:      Success:    Non-negative
 *
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLunregister_driver(hid_t vol_id)
{
    herr_t ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE1("e", "i", vol_id);

    /* Check arguments */
    if(NULL == H5I_object_verify(vol_id, H5I_VOL))
        HGOTO_ERROR(H5E_VOL, H5E_BADTYPE, FAIL, "not a VOL plugin")

    /* The H5VL_class_t struct will be freed by this function */
    if (H5I_dec_app_ref(vol_id) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTDEC, FAIL, "unable to unregister VOL plugin")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLunregister_driver() */


/*-------------------------------------------------------------------------
 * Function:    H5VLis_driver_registered
 *
 * Purpose:     Tests whether a VOL class has been registered or not
 *
 * Return:      Positive if the VOL class has been registered
 *
 *              Zero if it is unregistered
 *
 *              Negative on error (if the class is not a valid class ID)
 *
 *-------------------------------------------------------------------------
 */
htri_t
H5VLis_driver_registered(const char *name)
{
    H5VL_get_plugin_ud_t op_data;       /* Callback info for plugin search */
    htri_t ret_value = FALSE;           /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE1("t", "*s", name);

    op_data.found_id = H5I_INVALID_HID;
    op_data.name = name;

    /* Check arguments */
    if (H5I_iterate(H5I_VOL, H5VL__get_plugin_cb, &op_data, TRUE) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_BADITER, FAIL, "can't iterate over VOL ids")

    if (op_data.found_id != H5I_INVALID_HID)
        ret_value = TRUE;

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLis_driver_registered() */


/*-------------------------------------------------------------------------
 * Function:    H5VLget_plugin_id
 *
 * Purpose:     Retrieves the ID for a registered VOL plugin.
 *
 * Return:      Positive if the VOL class has been registered
 *              Negative on error (if the class is not a valid class or not registered)
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5VLget_plugin_id(const char *name)
{
    H5VL_get_plugin_ud_t op_data;       /* Callback info for plugin search */
    hid_t ret_value = H5I_INVALID_HID;  /* Return value */

    FUNC_ENTER_API(H5I_INVALID_HID)
    H5TRACE1("i", "*s", name);

    op_data.found_id    = H5I_INVALID_HID;
    op_data.name        = name;

    /* Check arguments */
    if (H5I_iterate(H5I_VOL, H5VL__get_plugin_cb, &op_data, TRUE) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_BADITER, H5I_INVALID_HID, "can't iterate over VOL plugin IDs")

    if (op_data.found_id != H5I_INVALID_HID) {
        if (H5I_inc_ref(op_data.found_id, TRUE) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTINC, H5I_INVALID_HID, "unable to increment ref count on VOL plugin")
        ret_value = op_data.found_id;
    } /* end if */

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLget_plugin_id() */


/*-------------------------------------------------------------------------
 * Function:    H5VLget_plugin_name
 *
 * Purpose:     Returns the plugin name for the VOL associated with the
 *              object or file ID
 *
 * Return:      Success:        The length of the plugin name
 *
 *              Failure:        Negative
 *
 *-------------------------------------------------------------------------
 */
ssize_t
H5VLget_plugin_name(hid_t obj_id, char *name/*out*/, size_t size)
{
    ssize_t    ret_value = -1;

    FUNC_ENTER_API(FAIL)
    H5TRACE3("Zs", "ixz", obj_id, name, size);

    if ((ret_value = H5VL_get_plugin_name(obj_id, name, size)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "Can't get plugin name")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLget_plugin_name() */


/*---------------------------------------------------------------------------
 * Function:    H5VLcmp_plugin_cls
 *
 * Purpose:     Compares two plugin classes
 *
 * Return:      Success:    Non-negative, with *cmp set to positive if
 *		plugin_id1 is greater than plugin_id2, negative if plugin_id2
 *		is greater than plugin_id1 and zero if plugin_id1 and plugin_id2
 *		are equal.
 *
 *              Failure:    Negative
 *
 *---------------------------------------------------------------------------
 */
herr_t
H5VLcmp_plugin_cls(int *cmp, hid_t plugin_id1, hid_t plugin_id2)
{
    H5VL_class_t *cls1, *cls2;  /* Plugins for IDs */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE3("e", "*Isii", cmp, plugin_id1, plugin_id2);

    /* Check args and get class pointers */
    if(NULL == (cls1 = (H5VL_class_t *)H5I_object_verify(plugin_id1, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")
    if(NULL == (cls2 = (H5VL_class_t *)H5I_object_verify(plugin_id2, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    /* Compare the two VOL plugin classes */
    *cmp = H5VL_cmp_plugin_cls(cls1, cls2);

done:
    FUNC_LEAVE_API(ret_value)
} /* H5VLcmp_plugin_cls() */


/*---------------------------------------------------------------------------
 * Function:    H5VLcmp_plugin_info
 *
 * Purpose:     Compares two plugin info objects
 *
 * Note:	Both info objects must be from the same VOL plugin class
 *
 * Return:      Success:    Non-negative, with *cmp set to positive if
 *		info1 is greater than info2, negative if info2
 *		is greater than info1 and zero if info1 and info2
 *		are equal.
 *
 *              Failure:    Negative
 *
 *---------------------------------------------------------------------------
 */
herr_t
H5VLcmp_plugin_info(int *cmp, hid_t plugin_id, const void *info1, const void *info2)
{
    H5VL_class_t *cls;          /* Plugins for ID */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE4("e", "*Isi*x*x", cmp, plugin_id, info1, info2);

    /* Check args and get class pointer */
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    /* Compare the two VOL plugin info objects */
    *cmp = H5VL_cmp_plugin_info(cls, info1, info2);

done:
    FUNC_LEAVE_API(ret_value)
} /* H5VLcmp_plugin_info() */

/*-------------------------------------------------------------------------
 * Routines below this are public wrappers for stackable VOL plugins, and
 * should not be called by applications.
 *-----------------------------------------------------------------------*/


/*-------------------------------------------------------------------------
 * Function:    H5VLinitialize
 *
 * Purpose:     Calls the plugin-specific callback to initialize the plugin.
 *
 * Return:      Success:    Non-negative
 *
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLinitialize(hid_t plugin_id, hid_t vipl_id)
{
    H5VL_class_t *cls = NULL;
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API_NOINIT
    H5TRACE2("e", "ii", plugin_id, vipl_id);

    /* Check args */
    if (NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    if (cls->initialize && cls->initialize(vipl_id) < 0)
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
 *
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLterminate(hid_t plugin_id)
{
    H5VL_class_t *cls = NULL;
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API_NOINIT
    H5TRACE1("e", "i", plugin_id);

    /* Check args */
    if (NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    if (cls->terminate && cls->terminate() < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTCLOSEOBJ, FAIL, "VOL plugin did not terminate cleanly")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLterminate() */


/*---------------------------------------------------------------------------
 * Function:    H5VLget_cap_flags
 *
 * Purpose:     Retrieves the capability flag for a plugin
 *
 * Return:      Success:    Non-NULL
 *              Failure:    NULL
 *
 *---------------------------------------------------------------------------
 */
herr_t
H5VLget_cap_flags(hid_t plugin_id, unsigned *cap_flags)
{
    H5VL_class_t *cls;                  /* Plugin class struct */
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
    H5VL_class_t *cls;          /* Plugins for ID */
    herr_t ret_value = SUCCEED; /* Return value */

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
    H5VL_class_t *cls;          /* Plugins for ID */
    herr_t ret_value = SUCCEED; /* Return value */

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
    H5VL_class_t *cls;
    void *ret_value = NULL;

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
    H5VL_class_t *cls;          /* Plugins for ID */
    herr_t ret_value = SUCCEED; /* Return value */

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
    H5VL_class_t *cls;          /* Plugins for ID */
    herr_t ret_value = SUCCEED; /* Return value */

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
    H5VL_class_t *cls;          /* VOL plugin class */
    void *ret_value = NULL;     /* Return value */

    FUNC_ENTER_API_NOINIT

    H5TRACE3("*x", "*xi*x", obj, plugin_id, wrap_ctx);
    /* Check args */
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
 * Function:    H5VLattr_create
 *
 * Purpose:     Creates an attribute
 *
 * Return:      Success:    Pointer to the new attribute
 *
 *              Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
void *
H5VLattr_create(void *obj, H5VL_loc_params_t loc_params, hid_t plugin_id, const char *name,
                hid_t acpl_id, hid_t aapl_id, hid_t dxpl_id, void **req)
{
    H5VL_class_t *cls = NULL;
    void *ret_value = NULL; /* Return value */

    FUNC_ENTER_API_NOINIT
    H5TRACE8("*x", "*xxi*siii**x", obj, loc_params, plugin_id, name, acpl_id,
             aapl_id, dxpl_id, req);

    if (NULL == obj)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "invalid object")
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a VOL plugin ID")

    if(NULL == (ret_value = H5VL_attr_create(obj, loc_params, cls, name, 
                                             acpl_id, aapl_id, dxpl_id, req)))
        HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, NULL, "unable to create attribute")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLattr_create() */


/*-------------------------------------------------------------------------
 * Function:	H5VLattr_open
 *
 * Purpose:     Opens an attribute
 *
 * Return:      Success:    Pointer to the new attribute
 *
 *              Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
void *
H5VLattr_open(void *obj, H5VL_loc_params_t loc_params, hid_t plugin_id, const char *name,
              hid_t aapl_id, hid_t dxpl_id, void **req)
{
    H5VL_class_t *cls = NULL;
    void *ret_value = NULL; /* Return value */

    FUNC_ENTER_API_NOINIT
    H5TRACE7("*x", "*xxi*sii**x", obj, loc_params, plugin_id, name, aapl_id,
             dxpl_id, req);

    if (NULL == obj)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "invalid object")
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a VOL plugin ID")

    if(NULL == (ret_value = H5VL_attr_open(obj, loc_params, cls, name, aapl_id, 
                                           dxpl_id, req)))
        HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, NULL, "unable to open attribute")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLattr_open() */


/*-------------------------------------------------------------------------
 * Function:    H5VLattr_read
 *
 * Purpose:     Reads data from an attribute
 *
 * Return:      Success:    Non-negative
 *
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t H5VLattr_read(void *attr, hid_t plugin_id, hid_t mem_type_id, void *buf, hid_t dxpl_id, void **req)
{
    H5VL_class_t *cls = NULL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API_NOINIT

    if (NULL == attr)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object")
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    if((ret_value = H5VL_attr_read(attr, cls, mem_type_id, buf, dxpl_id, req)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to read attribute")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLattr_read() */


/*-------------------------------------------------------------------------
 * Function:    H5VLattr_write
 *
 * Purpose:     Writes data to an attribute
 *
 * Return:      Success:    Non-negative
 *
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t H5VLattr_write(void *attr, hid_t plugin_id, hid_t mem_type_id, const void *buf, hid_t dxpl_id, void **req)
{
    H5VL_class_t *cls = NULL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API_NOINIT

    if (NULL == attr)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object")
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    if((ret_value = H5VL_attr_write(attr, cls, mem_type_id, buf, dxpl_id, req)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to write attribute")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLattr_write() */


/*-------------------------------------------------------------------------
 * Function:    H5VLattr_get
 *
 * Purpose:     Gets information about the attribute
 *
 * Return:      Success:    Non-negative
 *
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLattr_get(void *obj, hid_t plugin_id, H5VL_attr_get_t get_type,
             hid_t dxpl_id, void **req, va_list arguments)
{
    H5VL_class_t *cls = NULL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API_NOINIT
    H5TRACE6("e", "*xiVai**xx", obj, plugin_id, get_type, dxpl_id, req, arguments);

    if (NULL == obj)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object")
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    /* Bypass the H5VLint layer */
    if(NULL == cls->attr_cls.get)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no `attr get' method")
    if((ret_value = (cls->attr_cls.get)(obj, get_type, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "Unable to get attribute information")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLattr_get() */


/*-------------------------------------------------------------------------
 * Function:    H5VLattr_specific
 *
 * Purpose:     Performs a plugin-specific operation on an attribute
 *
 * Return:      Success:    Non-negative
 *
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLattr_specific(void *obj, H5VL_loc_params_t loc_params, hid_t plugin_id,
                  H5VL_attr_specific_t specific_type, hid_t dxpl_id, void **req, va_list arguments)
{
    H5VL_class_t *cls = NULL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API_NOINIT
    H5TRACE7("e", "*xxiVbi**xx", obj, loc_params, plugin_id, specific_type,
             dxpl_id, req, arguments);

    if (NULL == obj)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object")
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    /* Bypass the H5VLint layer */
    if(NULL == cls->attr_cls.specific)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no `attr specific' method")
    if((ret_value = (cls->attr_cls.specific)
        (obj, loc_params, specific_type, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "Unable to execute attribute specific callback")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLattr_specific() */


/*-------------------------------------------------------------------------
 * Function:    H5VLattr_optional
 *
 * Purpose:     Performs an optional plugin-specific operation on an attribute
 *
 * Return:      Success:    Non-negative
 *
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLattr_optional(void *obj, hid_t plugin_id, hid_t dxpl_id, void **req, va_list arguments)
{
    H5VL_class_t *cls = NULL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API_NOINIT
    H5TRACE5("e", "*xii**xx", obj, plugin_id, dxpl_id, req, arguments);

    if (NULL == obj)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object")
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    /* Have to bypass the H5VLint layer due to unknown val_list arguments */
    if(NULL == cls->attr_cls.optional)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no `attr optional' method")
    if((ret_value = (cls->attr_cls.optional)(obj, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "Unable to execute attribute optional callback")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLattr_optional() */


/*-------------------------------------------------------------------------
 * Function:    H5VLattr_close
 *
 * Purpose:     Closes an attribute
 *
 * Return:      Success:    Non-negative
 *
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLattr_close(void *attr, hid_t plugin_id, hid_t dxpl_id, void **req)
{
    H5VL_class_t *cls = NULL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API_NOINIT
    H5TRACE4("e", "*xii**x", attr, plugin_id, dxpl_id, req);

    if (NULL == attr)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object")
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    if((ret_value = H5VL_attr_close(attr, cls, dxpl_id, req)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTRELEASE, FAIL, "unable to close attribute")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLattr_close() */


/*-------------------------------------------------------------------------
 * Function:    H5VLdataset_create
 *
 * Purpose:     Creates a dataset
 *
 * Return:      Success:    Pointer to the new dataset
 *
 *              Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
void *
H5VLdataset_create(void *obj, H5VL_loc_params_t loc_params, hid_t plugin_id, const char *name,
                    hid_t dcpl_id, hid_t dapl_id, hid_t dxpl_id, void **req)
{
    H5VL_class_t *cls = NULL;
    void *ret_value = NULL; /* Return value */

    FUNC_ENTER_API_NOINIT
    H5TRACE8("*x", "*xxi*siii**x", obj, loc_params, plugin_id, name, dcpl_id,
             dapl_id, dxpl_id, req);

    if (NULL == obj)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "invalid object")
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a VOL plugin ID")

    if(NULL == (ret_value = H5VL_dataset_create(obj, loc_params, cls, name, 
                                                dcpl_id, dapl_id, dxpl_id, req)))
        HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, NULL, "unable to create dataset")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLdataset_create() */


/*-------------------------------------------------------------------------
 * Function:    H5VLdataset_open
 *
 * Purpose:     Opens a dataset
 *
 * Return:      Success:    Pointer to the new dataset
 *
 *              Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
void *
H5VLdataset_open(void *obj, H5VL_loc_params_t loc_params, hid_t plugin_id, const char *name,
                  hid_t dapl_id, hid_t dxpl_id, void **req)
{
    H5VL_class_t *cls = NULL;
    void *ret_value = NULL;

    FUNC_ENTER_API_NOINIT
    H5TRACE7("*x", "*xxi*sii**x", obj, loc_params, plugin_id, name, dapl_id,
             dxpl_id, req);

    if (NULL == obj)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "invalid object")
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a VOL plugin ID")

    if(NULL == (ret_value = H5VL_dataset_open(obj, loc_params, cls, name, dapl_id, dxpl_id, req)))
        HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, NULL, "unable to open dataset")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLdataset_open() */


/*-------------------------------------------------------------------------
 * Function:    H5VLdataset_read
 *
 * Purpose:     Reads data from a dataset
 *
 * Return:      Success:    Non-negative
 *
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLdataset_read(void *dset, hid_t plugin_id, hid_t mem_type_id, hid_t mem_space_id,
                  hid_t file_space_id, hid_t plist_id, void *buf, void **req)
{
    H5VL_class_t *cls = NULL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API_NOINIT
    H5TRACE8("e", "*xiiiii*x**x", dset, plugin_id, mem_type_id, mem_space_id,
             file_space_id, plist_id, buf, req);

    if (NULL == dset)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object")
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    if((ret_value = H5VL_dataset_read(dset, cls, mem_type_id, mem_space_id, file_space_id, 
                                      plist_id, buf, req)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to read dataset")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLdataset_read() */


/*-------------------------------------------------------------------------
 * Function:    H5VLdataset_write
 *
 * Purpose:     Writes data to a dataset
 *
 * Return:      Success:    Non-negative
 *
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLdataset_write(void *dset, hid_t plugin_id, hid_t mem_type_id, hid_t mem_space_id,
                   hid_t file_space_id, hid_t plist_id, const void *buf, void **req)
{
    H5VL_class_t *cls = NULL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API_NOINIT
    H5TRACE8("e", "*xiiiii*x**x", dset, plugin_id, mem_type_id, mem_space_id,
             file_space_id, plist_id, buf, req);

    if (NULL == dset)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object")
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    if((ret_value = H5VL_dataset_write(dset, cls, mem_type_id, mem_space_id, file_space_id, 
                                       plist_id, buf, req)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to write dataset")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLdataset_write() */


/*-------------------------------------------------------------------------
 * Function:    H5VLdataset_get
 *
 * Purpose:     Gets information about a dataset
 *
 * Return:      Success:    Non-negative
 *
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLdataset_get(void *dset, hid_t plugin_id, H5VL_dataset_get_t get_type,
                hid_t dxpl_id, void **req, va_list arguments)
{
    H5VL_class_t *cls = NULL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API_NOINIT
    H5TRACE6("e", "*xiVci**xx", dset, plugin_id, get_type, dxpl_id, req, arguments);

    if (NULL == dset)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object")
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    /* Bypass the H5VLint layer */
    if(NULL == cls->dataset_cls.get)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no `dataset get' method")
    if((ret_value = (cls->dataset_cls.get)(dset, get_type, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "Unable to execute dataset get callback")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLdataset_get() */


/*-------------------------------------------------------------------------
 * Function:    H5VLdataset_specific
 *
 * Purpose:     Performs a plugin-specific operation on a dataset
 *
 * Return:      Success:    Non-negative
 *
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLdataset_specific(void *obj, hid_t plugin_id, H5VL_dataset_specific_t specific_type,
                      hid_t dxpl_id, void **req, va_list arguments)
{
    H5VL_class_t *cls = NULL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API_NOINIT
    H5TRACE6("e", "*xiVdi**xx", obj, plugin_id, specific_type, dxpl_id, req,
             arguments);

    if (NULL == obj)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object")
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    if(NULL == cls->dataset_cls.specific)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no `dataset specific' method")
    if((ret_value = (cls->dataset_cls.specific)
        (obj, specific_type, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "Unable to execute dataset specific callback")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLdataset_specific() */


/*-------------------------------------------------------------------------
 * Function:    H5VLdataset_optional
 *
 * Purpose:     Performs an optional plugin-specific operation on a dataset
 *
 * Return:      Success:    Non-negative
 *
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLdataset_optional(void *obj, hid_t plugin_id, hid_t dxpl_id, void **req, va_list arguments)
{
    H5VL_class_t *cls = NULL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API_NOINIT
    H5TRACE5("e", "*xii**xx", obj, plugin_id, dxpl_id, req, arguments);

    if (NULL == obj)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object")
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    if(NULL == cls->dataset_cls.optional)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no `dataset optional' method")
    if((ret_value = (cls->dataset_cls.optional)(obj, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "Unable to execute dataset optional callback")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLdataset_optional() */


/*-------------------------------------------------------------------------
 * Function:    H5VLdataset_close
 *
 * Purpose:     Closes a dataset
 *
 * Return:      Success:    Non-negative
 *
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLdataset_close(void *dset, hid_t plugin_id, hid_t dxpl_id, void **req)
{
    H5VL_class_t *cls = NULL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API_NOINIT
    H5TRACE4("e", "*xii**x", dset, plugin_id, dxpl_id, req);

    if (NULL == dset)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object")
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    if((ret_value = H5VL_dataset_close(dset, cls, dxpl_id, req)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTRELEASE, FAIL, "unable to close dataset")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLdataset_close() */


/*-------------------------------------------------------------------------
 * Function:    H5VLfile_create
 *
 * Purpose:     Creates a file
 *
 * Return:      Success:    Pointer to the new file
 *
 *              Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
void *
H5VLfile_create(const char *name, unsigned flags, hid_t fcpl_id, hid_t fapl_id,
                hid_t dxpl_id, void **req)
{
    H5P_genplist_t     *plist;                 /* Property list pointer */
    H5VL_plugin_prop_t  plugin_prop;           /* Property for VOL plugin ID & info */
    H5VL_class_t       *cls = NULL;
    void               *ret_value = NULL;

    FUNC_ENTER_API_NOINIT
    H5TRACE6("*x", "*sIuiii**x", name, flags, fcpl_id, fapl_id, dxpl_id, req);

    /* get the VOL info from the fapl */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(fapl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a file access property list")
    if(H5P_peek(plist, H5F_ACS_VOL_DRV_NAME, &plugin_prop) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get VOL plugin info")

    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_prop.plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a VOL plugin ID")

    if(NULL == (ret_value = H5VL_file_create(cls, name, flags, fcpl_id, fapl_id, 
                                             dxpl_id, req)))
        HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, NULL, "unable to create file")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLfile_create() */


/*-------------------------------------------------------------------------
 * Function:    H5VLfile_open
 *
 * Purpose:     Opens a file
 *
 * Return:      Success:    Pointer to the new file
 *
 *              Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
void *
H5VLfile_open(const char *name, unsigned flags, hid_t fapl_id, hid_t dxpl_id, void **req)
{
    H5P_genplist_t     *plist;                 /* Property list pointer */
    H5VL_plugin_prop_t  plugin_prop;           /* Property for VOL plugin ID & info */
    H5VL_class_t       *cls = NULL;
    void               *ret_value = NULL;

    FUNC_ENTER_API_NOINIT
    H5TRACE5("*x", "*sIuii**x", name, flags, fapl_id, dxpl_id, req);

    /* get the VOL info from the fapl */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(fapl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a file access property list")
    if(H5P_peek(plist, H5F_ACS_VOL_DRV_NAME, &plugin_prop) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get VOL plugin info")

    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_prop.plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a VOL plugin ID")

    if(NULL == (ret_value = H5VL_file_open(cls, name, flags, fapl_id, dxpl_id, req)))
        HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, NULL, "unable to create file")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLfile_open() */


/*-------------------------------------------------------------------------
 * Function:    H5VLfile_get
 *
 * Purpose:     Gets information about the file
 *
 * Return:      Success:    Non-negative
 *
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLfile_get(void *file, hid_t plugin_id, H5VL_file_get_t get_type,
             hid_t dxpl_id, void **req, va_list arguments)
{
    H5VL_class_t *cls = NULL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API_NOINIT
    H5TRACE6("e", "*xiVgi**xx", file, plugin_id, get_type, dxpl_id, req, arguments);

    if(NULL == file)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object")
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    /* Bypass the H5VLint layer */
    if(NULL == cls->file_cls.get)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no `file get' method")
    if((ret_value = (cls->file_cls.get)(file, get_type, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "Unable to execute file get callback")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLfile_get() */


/*-------------------------------------------------------------------------
 * Function:    H5VLfile_specific
 *
 * Purpose:     Performs a plugin-specific operation on a file
 *
 * Return:      Success:    Non-negative
 *
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLfile_specific(void *file, hid_t plugin_id, H5VL_file_specific_t specific_type,
                  hid_t dxpl_id, void **req, va_list arguments)
{
    H5VL_class_t *cls = NULL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API_NOINIT
    H5TRACE6("e", "*xiVhi**xx", file, plugin_id, specific_type, dxpl_id, req,
             arguments);

    if(specific_type == H5VL_FILE_IS_ACCESSIBLE) {
        H5P_genplist_t     *plist;          /* Property list pointer */
        H5VL_plugin_prop_t  plugin_prop;            /* Property for VOL plugin ID & info */
        hid_t               fapl_id;

        fapl_id = va_arg (arguments, hid_t);

        /* get the VOL info from the fapl */
        if(NULL == (plist = (H5P_genplist_t *)H5I_object(fapl_id)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access property list")

        if(H5P_peek(plist, H5F_ACS_VOL_DRV_NAME, &plugin_prop) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get VOL plugin info")

        if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_prop.plugin_id, H5I_VOL)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

        if((ret_value = (cls->file_cls.specific)(file, specific_type, dxpl_id, req, arguments)) < 0)
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "specific failed")
    } /* end if */
    else {
        if(NULL == file)
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object")
        if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

        if(NULL == cls->file_cls.specific)
            HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no `file specific' method")
        if((ret_value = (cls->file_cls.specific)(file, specific_type, dxpl_id, req, arguments)) < 0)
            HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "Unable to execute file specific callback")
    } /* end else */

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLfile_specific() */


/*-------------------------------------------------------------------------
 * Function:    H5VLfile_optional
 *
 * Purpose:     Performs an optional plugin-specific operation on a file
 *
 * Return:      Success:    Non-negative
 *
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLfile_optional(void *file, hid_t plugin_id, hid_t dxpl_id, void **req, va_list arguments)
{
    H5VL_class_t *cls = NULL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API_NOINIT
    H5TRACE5("e", "*xii**xx", file, plugin_id, dxpl_id, req, arguments);

    if(NULL == file)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object")
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    if(NULL == cls->file_cls.optional)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no `file optional' method")
    if((ret_value = (cls->file_cls.optional)(file, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "Unable to execute file optional callback")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLfile_optional() */


/*-------------------------------------------------------------------------
 * Function:    H5VLfile_close
 *
 * Purpose:     Closes a file
 *
 * Return:      Success:    Non-negative
 *
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLfile_close(void *file, hid_t plugin_id, hid_t dxpl_id, void **req)
{
    H5VL_class_t *cls = NULL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API_NOINIT
    H5TRACE4("e", "*xii**x", file, plugin_id, dxpl_id, req);

    if(NULL == file)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object")
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    if((ret_value = H5VL_file_close(file, cls, dxpl_id, req)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTRELEASE, FAIL, "unable to close file")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLfile_close() */


/*-------------------------------------------------------------------------
 * Function:    H5VLgroup_create
 *
 * Purpose:     Creates a group
 *
 * Return:      Success:    Pointer to the new group
 *
 *              Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
void *
H5VLgroup_create(void *obj, H5VL_loc_params_t loc_params, hid_t plugin_id, const char *name,
                 hid_t gcpl_id, hid_t gapl_id, hid_t dxpl_id, void **req)
{
    H5VL_class_t *cls = NULL;
    void *ret_value = NULL;

    FUNC_ENTER_API_NOINIT
    H5TRACE8("*x", "*xxi*siii**x", obj, loc_params, plugin_id, name, gcpl_id,
             gapl_id, dxpl_id, req);

    if (NULL == obj)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "invalid object")
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a VOL plugin ID")

    if(NULL == (ret_value = H5VL_group_create(obj, loc_params, cls, name, 
                                              gcpl_id, gapl_id, dxpl_id, req)))
        HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, NULL, "unable to create group")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLgroup_create() */


/*-------------------------------------------------------------------------
 * Function:    H5VLgroup_open
 *
 * Purpose:     Opens a group
 *
 * Return:      Success:    Pointer to the new group
 *
 *              Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
void *
H5VLgroup_open(void *obj, H5VL_loc_params_t loc_params, hid_t plugin_id, const char *name,
                hid_t gapl_id, hid_t dxpl_id, void **req)
{
    H5VL_class_t *cls = NULL;
    void *ret_value = NULL;

    FUNC_ENTER_API_NOINIT
    H5TRACE7("*x", "*xxi*sii**x", obj, loc_params, plugin_id, name, gapl_id,
             dxpl_id, req);

    if (NULL == obj)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "invalid object")
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a VOL plugin ID")

    if(NULL == (ret_value = H5VL_group_open(obj, loc_params, cls, name, 
                                              gapl_id, dxpl_id, req)))
        HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, NULL, "unable to open group")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLgroup_open() */


/*-------------------------------------------------------------------------
 * Function:    H5VLgroup_get
 *
 * Purpose:     Gets information about the group
 *
 * Return:      Success:    Non-negative
 *
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLgroup_get(void *obj, hid_t plugin_id, H5VL_group_get_t get_type,
              hid_t dxpl_id, void **req, va_list arguments)
{
    H5VL_class_t *cls = NULL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API_NOINIT
    H5TRACE6("e", "*xiVii**xx", obj, plugin_id, get_type, dxpl_id, req, arguments);

    if(NULL == obj)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object")
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    /* Bypass the H5VLint layer */
    if(NULL == cls->group_cls.get)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no `group get' method")
    if((ret_value = (cls->group_cls.get)(obj, get_type, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "Unable to execute group get callback")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLgroup_get() */


/*-------------------------------------------------------------------------
 * Function:    H5VLgroup_specific
 *
 * Purpose:     Performs a plugin-specific operation on a group
 *
 * Return:      Success:    Non-negative
 *
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLgroup_specific(void *obj, hid_t plugin_id, H5VL_group_specific_t specific_type,
                      hid_t dxpl_id, void **req, va_list arguments)
{
    H5VL_class_t *cls = NULL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API_NOINIT
    H5TRACE6("e", "*xiVji**xx", obj, plugin_id, specific_type, dxpl_id, req,
             arguments);

    if (NULL == obj)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object")
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    if(NULL == cls->group_cls.specific)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no `group specific' method")
    if((ret_value = (cls->group_cls.specific)
        (obj, specific_type, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "Unable to execute group specific callback")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLgroup_specific() */


/*-------------------------------------------------------------------------
 * Function:    H5VLgroup_optional
 *
 * Purpose:     Performs an optional plugin-specific operation on a group
 *
 * Return:      Success:    Non-negative
 *
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLgroup_optional(void *obj, hid_t plugin_id, hid_t dxpl_id, void **req, va_list arguments)
{
    H5VL_class_t *cls = NULL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API_NOINIT
    H5TRACE5("e", "*xii**xx", obj, plugin_id, dxpl_id, req, arguments);

    if (NULL == obj)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object")
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    if(NULL == cls->group_cls.optional)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no `group optional' method")
    if((ret_value = (cls->group_cls.optional)(obj, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "Unable to execute group optional callback")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLgroup_optional() */


/*-------------------------------------------------------------------------
 * Function:    H5VLgroup_close
 *
 * Purpose:     Closes a group
 *
 * Return:      Success:    Non-negative
 *
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLgroup_close(void *grp, hid_t plugin_id, hid_t dxpl_id, void **req)
{
    H5VL_class_t *cls = NULL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API_NOINIT
    H5TRACE4("e", "*xii**x", grp, plugin_id, dxpl_id, req);

    if(NULL == grp)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object")
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    if((ret_value = H5VL_group_close(grp, cls, dxpl_id, req)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTRELEASE, FAIL, "unable to close group")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLgroup_close() */


/*-------------------------------------------------------------------------
 * Function:    H5VLlink_create
 *
 * Purpose:     Creates a hard link
 *
 * Note:	The 'obj' parameter is allowed to be NULL
 *
 * Return:      Success:    Non-negative
 *
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLlink_create(H5VL_link_create_type_t create_type, void *obj, H5VL_loc_params_t loc_params,
                 hid_t plugin_id, hid_t lcpl_id, hid_t lapl_id, hid_t dxpl_id, void **req)
{
    H5VL_class_t *cls = NULL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API_NOINIT
    H5TRACE8("e", "Vk*xxiiii**x", create_type, obj, loc_params, plugin_id, lcpl_id,
             lapl_id, dxpl_id, req);

    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    if((ret_value = H5VL_link_create(create_type, obj, loc_params, cls, lcpl_id, lapl_id, dxpl_id, req)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to create link")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLlink_create() */


/*-------------------------------------------------------------------------
 * Function:    H5VLlink_copy
 *
 * Purpose:     Copies a link to a new location
 *
 * Note:	The 'src_obj' and 'dst_obj' parameters are allowed to be NULL
 *
 * Return:      Success:    Non-negative
 *
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLlink_copy(void *src_obj, H5VL_loc_params_t loc_params1, void *dst_obj,
              H5VL_loc_params_t loc_params2, hid_t plugin_id,
              hid_t lcpl_id, hid_t lapl_id, hid_t dxpl_id, void **req)
{
    H5VL_class_t *cls = NULL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API_NOINIT
    H5TRACE9("e", "*xx*xxiiii**x", src_obj, loc_params1, dst_obj, loc_params2,
             plugin_id, lcpl_id, lapl_id, dxpl_id, req);

    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    if((ret_value = H5VL_link_copy(src_obj, loc_params1, dst_obj, loc_params2, cls, 
                                   lcpl_id, lapl_id, dxpl_id, req)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTCOPY, FAIL, "unable to copy object")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLlink_copy() */


/*-------------------------------------------------------------------------
 * Function:    H5VLlink_move
 *
 * Purpose:     Moves a link to another location
 *
 * Note:	The 'src_obj' and 'dst_obj' parameters are allowed to be NULL
 *
 * Return:      Success:    Non-negative
 *
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLlink_move(void *src_obj, H5VL_loc_params_t loc_params1, void *dst_obj,
              H5VL_loc_params_t loc_params2, hid_t plugin_id,
              hid_t lcpl_id, hid_t lapl_id, hid_t dxpl_id, void **req)
{
    H5VL_class_t *cls = NULL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API_NOINIT
    H5TRACE9("e", "*xx*xxiiii**x", src_obj, loc_params1, dst_obj, loc_params2,
             plugin_id, lcpl_id, lapl_id, dxpl_id, req);

    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    if((ret_value = H5VL_link_move(src_obj, loc_params1, dst_obj, loc_params2, cls, 
                                   lcpl_id, lapl_id, dxpl_id, req)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to move object")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLlink_move() */


/*-------------------------------------------------------------------------
 * Function:    H5VLlink_get
 *
 * Purpose:     Gets information about a link
 *
 * Return:      Success:    Non-negative
 *
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLlink_get(void *obj, H5VL_loc_params_t loc_params, hid_t plugin_id, H5VL_link_get_t get_type,
              hid_t dxpl_id, void **req, va_list arguments)
{
    H5VL_class_t *cls = NULL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API_NOINIT
    H5TRACE7("e", "*xxiVli**xx", obj, loc_params, plugin_id, get_type, dxpl_id, req,
             arguments);

    if(NULL == obj)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object")
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    if(NULL == cls->link_cls.get)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no `link get' method")
    if((ret_value = (cls->link_cls.get)
        (obj, loc_params, get_type, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "Unable to execute link get callback")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLlink_get() */


/*-------------------------------------------------------------------------
 * Function:    H5VLlink_specific
 *
 * Purpose:     Performs a plugin-specific operation on a link
 *
 * Return:      Success:    Non-negative
 *
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLlink_specific(void *obj, H5VL_loc_params_t loc_params, hid_t plugin_id,
                  H5VL_link_specific_t specific_type, hid_t dxpl_id, void **req, va_list arguments)
{
    H5VL_class_t *cls = NULL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API_NOINIT
    H5TRACE7("e", "*xxiVmi**xx", obj, loc_params, plugin_id, specific_type,
             dxpl_id, req, arguments);

    if (NULL == obj)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object")
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    /* Bypass the H5VLint layer */
    if(NULL == cls->link_cls.specific)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no `link specific' method")
    if((ret_value = (cls->link_cls.specific)
        (obj, loc_params, specific_type, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "Unable to execute link specific callback")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLlink_specific() */


/*-------------------------------------------------------------------------
 * Function:    H5VLlink_optional
 *
 * Purpose:     Performs an optional plugin-specific operation on a link
 *
 * Return:      Success:    Non-negative
 *
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLlink_optional(void *obj, hid_t plugin_id, hid_t dxpl_id, void **req, va_list arguments)
{
    H5VL_class_t *cls = NULL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API_NOINIT
    H5TRACE5("e", "*xii**xx", obj, plugin_id, dxpl_id, req, arguments);

    if (NULL == obj)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object")
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    /* Have to bypass the H5VLint layer due to unknown val_list arguments */
    if(NULL == cls->link_cls.optional)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no `link optional' method")
    if((ret_value = (cls->link_cls.optional)(obj, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "Unable to execute link optional callback")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLlink_optional() */


/*-------------------------------------------------------------------------
 * Function:    H5VLobject_open
 *
 * Purpose:     Opens an object
 *
 * Return:      Success:    Pointer to the new object
 *
 *              Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
void *
H5VLobject_open(void *obj, H5VL_loc_params_t params, hid_t plugin_id, H5I_type_t *opened_type,
                hid_t dxpl_id, void **req)
{
    H5VL_class_t *cls = NULL;
    void *ret_value = NULL;

    FUNC_ENTER_API_NOINIT
    H5TRACE6("*x", "*xxi*Iti**x", obj, params, plugin_id, opened_type, dxpl_id, req);

    if (NULL == obj)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "invalid object")
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a VOL plugin ID")

    if(NULL == (ret_value = H5VL_object_open(obj, params, cls, opened_type, dxpl_id, req)))
        HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, NULL, "unable to create group")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLobject_open() */


/*-------------------------------------------------------------------------
 * Function:    H5VLobject_copy
 *
 * Purpose:     Copies an object to another location
 *
 * Return:      Success:    Non-negative
 *
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLobject_copy(void *src_obj, H5VL_loc_params_t loc_params1, hid_t plugin_id1, const char *src_name,
                void *dst_obj, H5VL_loc_params_t loc_params2, hid_t plugin_id2, const char *dst_name,
                hid_t ocpypl_id, hid_t lcpl_id, hid_t dxpl_id, void **req)
{
    H5VL_class_t *cls1 = NULL;
    H5VL_class_t *cls2 = NULL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API_NOINIT
    H5TRACE12("e", "*xxi*s*xxi*siii**x", src_obj, loc_params1, plugin_id1,
             src_name, dst_obj, loc_params2, plugin_id2, dst_name, ocpypl_id,
             lcpl_id, dxpl_id, req);

    if(NULL == src_obj || NULL == dst_obj)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object")
    if(NULL == (cls1 = (H5VL_class_t *)H5I_object_verify(plugin_id1, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")
    if(NULL == (cls2 = (H5VL_class_t *)H5I_object_verify(plugin_id2, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    if((ret_value = H5VL_object_copy(src_obj, loc_params1, cls1, src_name, 
                                     dst_obj, loc_params2, cls2, dst_name,
                                     ocpypl_id, lcpl_id, dxpl_id, req)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTCOPY, FAIL, "unable to move object")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLobject_copy() */


/*-------------------------------------------------------------------------
 * Function:    H5VLobject_get
 *
 * Purpose:     Gets information about an object
 *
 * Return:      Success:    Non-negative
 *
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLobject_get(void *obj, H5VL_loc_params_t loc_params, hid_t plugin_id, H5VL_object_get_t get_type,
               hid_t dxpl_id, void **req, va_list arguments)
{
    H5VL_class_t *cls = NULL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API_NOINIT
    H5TRACE7("e", "*xxiVni**xx", obj, loc_params, plugin_id, get_type, dxpl_id, req,
             arguments);

    if(NULL == obj)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object")
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    if(NULL == cls->object_cls.get)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no `object get' method")
    if((ret_value = (cls->object_cls.get)
        (obj, loc_params, get_type, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "Unable to execute object get callback")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLobject_get() */


/*-------------------------------------------------------------------------
 * Function:    H5VLobject_specific
 *
 * Purpose:     Performs a plugin-specific operation on an object
 *
 * Return:      Success:    Non-negative
 *
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLobject_specific(void *obj, H5VL_loc_params_t loc_params, hid_t plugin_id,
                    H5VL_object_specific_t specific_type, hid_t dxpl_id, void **req, va_list arguments)
{
    H5VL_class_t *cls = NULL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API_NOINIT
    H5TRACE7("e", "*xxiVoi**xx", obj, loc_params, plugin_id, specific_type,
             dxpl_id, req, arguments);

    if(NULL == obj)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object")
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    /* Bypass the H5VLint layer */
    if(NULL == cls->object_cls.specific)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no `object specific' method")
    if((ret_value = (cls->object_cls.specific)
        (obj, loc_params, specific_type, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "Unable to execute object specific callback")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLobject_specific() */


/*-------------------------------------------------------------------------
 * Function:    H5VLobject_optional
 *
 * Purpose:     Performs an optional plugin-specific operation on an object
 *
 * Return:      Success:    Non-negative
 *
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLobject_optional(void *obj, hid_t plugin_id, hid_t dxpl_id, void **req, va_list arguments)
{
    H5VL_class_t *cls = NULL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API_NOINIT
    H5TRACE5("e", "*xii**xx", obj, plugin_id, dxpl_id, req, arguments);

    if (NULL == obj)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object")
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    /* Have to bypass the H5VLint layer due to unknown val_list arguments */
    if(NULL == cls->object_cls.optional)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no `object optional' method")
    if((ret_value = (cls->object_cls.optional)(obj, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "Unable to execute object optional callback")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLobject_optional() */


/*-------------------------------------------------------------------------
 * Function:    H5VLdatatype_commit
 *
 * Purpose:     Commits a datatype to the file
 *
 * Return:      Success:    Pointer to the new datatype
 *
 *              Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
void *
H5VLdatatype_commit(void *obj, H5VL_loc_params_t loc_params, hid_t plugin_id, const char *name,
                     hid_t type_id, hid_t lcpl_id, hid_t tcpl_id, hid_t tapl_id, hid_t dxpl_id, void **req)
{
    H5VL_class_t *cls = NULL;
    void *ret_value = NULL;

    FUNC_ENTER_API_NOINIT
    H5TRACE10("*x", "*xxi*siiiii**x", obj, loc_params, plugin_id, name, type_id,
             lcpl_id, tcpl_id, tapl_id, dxpl_id, req);

    if (NULL == obj)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "invalid object")
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a VOL plugin ID")

    if(NULL == (ret_value = H5VL_datatype_commit(obj, loc_params, cls, name, type_id, 
                                                 lcpl_id, tcpl_id, tapl_id, dxpl_id, req)))
        HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, NULL, "unable to commit datatype")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLdatatype_commit() */


/*-------------------------------------------------------------------------
 * Function:    H5VLdatatype_open
 *
 * Purpose:     Opens a named datatype
 *
 * Return:      Success:    Pointer to the new datatype
 *
 *              Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
void *
H5VLdatatype_open(void *obj, H5VL_loc_params_t loc_params, hid_t plugin_id, const char *name,
                   hid_t tapl_id, hid_t dxpl_id, void **req)
{
    H5VL_class_t *cls = NULL;
    void *ret_value = NULL;

    FUNC_ENTER_API_NOINIT
    H5TRACE7("*x", "*xxi*sii**x", obj, loc_params, plugin_id, name, tapl_id,
             dxpl_id, req);

    if (NULL == obj)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "invalid object")
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a VOL plugin ID")

    if(NULL == (ret_value = H5VL_datatype_open(obj, loc_params, cls, name, 
                                               tapl_id, dxpl_id, req)))
        HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, NULL, "unable to open datatype")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLdatatype_open() */


/*-------------------------------------------------------------------------
 * Function:    H5VLdatatype_specific
 *
 * Purpose:     Performs a plugin-specific operation on a datatype
 *
 * Return:      Success:    Non-negative
 *
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLdatatype_specific(void *obj, hid_t plugin_id, H5VL_datatype_specific_t specific_type,
                      hid_t dxpl_id, void **req, va_list arguments)
{
    H5VL_class_t *cls = NULL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API_NOINIT
    H5TRACE6("e", "*xiVfi**xx", obj, plugin_id, specific_type, dxpl_id, req,
             arguments);

    if (NULL == obj)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object")
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    if(NULL == cls->datatype_cls.specific)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no `datatype specific' method")
    if((ret_value = (cls->datatype_cls.specific)
        (obj, specific_type, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "Unable to execute datatype specific callback")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLdatatype_specific() */


/*-------------------------------------------------------------------------
 * Function:    H5VLdatatype_optional
 *
 * Purpose:     Performs an optional plugin-specific operation on a datatype
 *
 * Return:      Success:    Non-negative
 *
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLdatatype_optional(void *obj, hid_t plugin_id, hid_t dxpl_id, void **req, va_list arguments)
{
    H5VL_class_t *cls = NULL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API_NOINIT
    H5TRACE5("e", "*xii**xx", obj, plugin_id, dxpl_id, req, arguments);

    if (NULL == obj)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object")
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    if(NULL == cls->datatype_cls.optional)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no `datatype optional' method")
    if((ret_value = (cls->datatype_cls.optional)(obj, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "Unable to execute datatype optional callback")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLdatatype_optional() */


/*-------------------------------------------------------------------------
 * Function:    H5VLdatatype_get
 *
 * Purpose:     Gets information about the datatype
 *
 * Return:      Success:    Non-negative
 *
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLdatatype_get(void *obj, hid_t plugin_id, H5VL_datatype_get_t get_type,
                 hid_t dxpl_id, void **req, va_list arguments)
{
    H5VL_class_t *cls = NULL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API_NOINIT
    H5TRACE6("e", "*xiVei**xx", obj, plugin_id, get_type, dxpl_id, req, arguments);

    if(NULL == obj)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object")
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    /* Bypass the H5VLint layer */
    if(NULL == cls->datatype_cls.get)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL plugin has no `datatype get' method")
    if((ret_value = (cls->datatype_cls.get)(obj, get_type, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "Unable to execute datatype get callback")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLdatatype_get() */


/*-------------------------------------------------------------------------
 * Function:    H5VLdatatype_close
 *
 * Purpose:     Closes a datatype
 *
 * Return:      Success:    Non-negative
 *
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLdatatype_close(void *dt, hid_t plugin_id, hid_t dxpl_id, void **req)
{
    H5VL_class_t *cls = NULL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API_NOINIT
    H5TRACE4("e", "*xii**x", dt, plugin_id, dxpl_id, req);

    if (NULL == dt)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object")
    if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    if((ret_value = H5VL_datatype_close(dt, cls, dxpl_id, req)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTRELEASE, FAIL, "unable to close datatype")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLdatatype_close() */


/*-------------------------------------------------------------------------
 * Function:    H5VLrequest_cancel
 *
 * Purpose:     Cancels a request
 *
 * Return:      Success:    Non-negative
 *
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLrequest_cancel(void **req, hid_t plugin_id, H5ES_status_t *status)
{
    H5VL_class_t    *cls = NULL;
    herr_t          ret_value = SUCCEED;

    FUNC_ENTER_API_NOINIT
    H5TRACE3("e", "**xi*Es", req, plugin_id, status);

    if (NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    if ((ret_value = H5VL_request_cancel(req, cls, status)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTRELEASE, FAIL, "unable to cancel request")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLrequest_cancel() */


/*-------------------------------------------------------------------------
 * Function:    H5VLrequest_test
 *
 * Purpose:     Tests a request
 *
 * Return:      Success:    Non-negative
 *
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLrequest_test(void **req, hid_t plugin_id, H5ES_status_t *status)
{
    H5VL_class_t    *cls = NULL;
    herr_t          ret_value = SUCCEED;

    FUNC_ENTER_API_NOINIT
    H5TRACE3("e", "**xi*Es", req, plugin_id, status);

    if (NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    if ((ret_value = H5VL_request_test(req, cls, status)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTRELEASE, FAIL, "unable to test request")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLrequest_test() */


/*-------------------------------------------------------------------------
 * Function:    H5VLrequest_wait
 *
 * Purpose:     Waits on a request
 *
 * Return:      Success:    Non-negative
 *
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLrequest_wait(void **req, hid_t plugin_id, H5ES_status_t *status)
{
    H5VL_class_t    *cls = NULL;
    herr_t          ret_value = SUCCEED;

    FUNC_ENTER_API_NOINIT
    H5TRACE3("e", "**xi*Es", req, plugin_id, status);

    if (NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    if ((ret_value = H5VL_request_wait(req, cls, status)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTRELEASE, FAIL, "unable to wait on request")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5VLrequest_wait() */

