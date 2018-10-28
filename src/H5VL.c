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
    } /* end if */

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
} /* end H5VLregister_plugin() */


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
} /* end H5VLregister_plugin_by_name() */


/*-------------------------------------------------------------------------
 * Function:    H5VLis_plugin_registered
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
H5VLis_plugin_registered(const char *name)
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
} /* end H5VLis_plugin_registered() */


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
H5VLunregister_plugin(hid_t vol_id)
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
} /* end H5VLunregister_plugin() */


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

