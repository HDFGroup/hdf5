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

/* Information needed for iterating over the registered VOL connector hid_t IDs.
 * The name or value of the new VOL connector that is being registered is
 * stored in the name (or value) field and the found_id field is initialized to
 * H5I_INVALID_HID (-1).  If we find a VOL connector with the same name / value,
 * we set the found_id field to the existing ID for return to the function.
 */
typedef struct {
    /* IN */
    H5VL_get_connector_kind_t kind;     /* Which kind of connector search to make */
    union {
        const char *name;               /* The name of the VOL connector to check */
        H5VL_class_value_t value;       /* The value of the VOL connector to check */
    } u;

    /* OUT */
    hid_t found_id;                     /* The connector ID, if we found a match */
} H5VL_get_connector_ud_t;


/********************/
/* Local Prototypes */
/********************/
static int H5VL__get_connector_cb(void *obj, hid_t id, void *_op_data);


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
 * Function:    H5VL__get_connector_cb
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
H5VL__get_connector_cb(void *obj, hid_t id, void *_op_data)
{
    H5VL_get_connector_ud_t *op_data = (H5VL_get_connector_ud_t *)_op_data; /* User data for callback */
    H5VL_class_t *cls = (H5VL_class_t *)obj;
    int ret_value = H5_ITER_CONT;     /* Callback return value */

    FUNC_ENTER_STATIC_NOERR

    if(H5VL_GET_CONNECTOR_BY_NAME == op_data->kind) {
        if(0 == HDstrcmp(cls->name, op_data->u.name)) {
            op_data->found_id = id;
            ret_value = H5_ITER_STOP;
        } /* end if */
    } /* end if */
    else {
        HDassert(H5VL_GET_CONNECTOR_BY_VALUE == op_data->kind);
        if(cls->value == op_data->u.value) {
            op_data->found_id = id;
            ret_value = H5_ITER_STOP;
        } /* end if */
    } /* end else */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__get_connector_cb() */


/*-------------------------------------------------------------------------
 * Function:    H5VLregister_connector
 *
 * Purpose:     Registers a new VOL connector as a member of the virtual object
 *              layer class.
 *
 * Return:      Success:    A VOL connector ID which is good until the
 *                          library is closed or the connector is
 *                          unregistered.
 *
 *              Failure:    H5I_INVALID_HID
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5VLregister_connector(const H5VL_class_t *cls, hid_t vipl_id)
{
    H5VL_get_connector_ud_t op_data;       /* Callback info for connector search */
    hid_t ret_value = H5I_INVALID_HID;

    FUNC_ENTER_API(H5I_INVALID_HID)
    H5TRACE2("i", "*xi", cls, vipl_id);

    /* Check arguments */
    if (!cls)
        HGOTO_ERROR(H5E_ARGS, H5E_UNINITIALIZED, H5I_INVALID_HID, "VOL connector class pointer cannot be NULL")
    if (!cls->name)
        HGOTO_ERROR(H5E_VOL, H5E_CANTREGISTER, H5I_INVALID_HID, "VOL connector class name cannot be the NULL pointer")
    if (0 == HDstrlen(cls->name))
        HGOTO_ERROR(H5E_VOL, H5E_CANTREGISTER, H5I_INVALID_HID, "VOL connector class name cannot be the empty string")
    if (cls->info_copy && !cls->info_free)
        HGOTO_ERROR(H5E_VOL, H5E_CANTREGISTER, H5I_INVALID_HID, "VOL connector must provide free callback for VOL info objects when a copy callback is provided")
    if (cls->get_wrap_ctx && !cls->free_wrap_ctx)
        HGOTO_ERROR(H5E_VOL, H5E_CANTREGISTER, H5I_INVALID_HID, "VOL connector must provide free callback for object wrapping contexts when a get callback is provided")

    op_data.kind = H5VL_GET_CONNECTOR_BY_NAME;
    op_data.u.name = cls->name;
    op_data.found_id = H5I_INVALID_HID;

    /* check if connector is already registered */
    if (H5I_iterate(H5I_VOL, H5VL__get_connector_cb, &op_data, TRUE) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_BADITER, H5I_INVALID_HID, "can't iterate over VOL IDs")

    /* Increment the ref count on the existing VOL connector ID, if it's already registered */
    if(op_data.found_id != H5I_INVALID_HID) {
        if (H5I_inc_ref(op_data.found_id, TRUE) < 0)
            HGOTO_ERROR(H5E_VOL, H5E_CANTINC, H5I_INVALID_HID, "unable to increment ref count on VOL connector")
        ret_value = op_data.found_id;
    } /* end if */
    else {
        /* Create a new class ID */
        if ((ret_value = H5VL_register_connector(cls, TRUE, vipl_id)) < 0)
            HGOTO_ERROR(H5E_VOL, H5E_CANTREGISTER, H5I_INVALID_HID, "unable to register VOL connector")
    } /* end else */

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLregister_connector() */


/*-------------------------------------------------------------------------
 * Function:    H5VLregister_connector_by_name
 *
 * Purpose:     Registers a new VOL connector as a member of the virtual object
 *              layer class.
 *
 * Return:      Success:    A VOL connector ID which is good until the
 *                          library is closed or the connector is
 *                          unregistered.
 *
 *              Failure:    H5I_INVALID_HID
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5VLregister_connector_by_name(const char *name, hid_t vipl_id)
{
    H5VL_get_connector_ud_t op_data;       /* Callback info for connector search */
    hid_t ret_value = H5I_INVALID_HID;

    FUNC_ENTER_API(H5I_INVALID_HID)
    H5TRACE2("i", "*si", name, vipl_id);

    /* Check arguments */
    if (!name)
        HGOTO_ERROR(H5E_ARGS, H5E_UNINITIALIZED, H5I_INVALID_HID, "null VOL connector name is disallowed")
    if (0 == HDstrlen(name))
        HGOTO_ERROR(H5E_ARGS, H5E_UNINITIALIZED, H5I_INVALID_HID, "zero-length VOL connector name is disallowed")

    op_data.kind = H5VL_GET_CONNECTOR_BY_NAME;
    op_data.u.name = name;
    op_data.found_id = H5I_INVALID_HID;

    /* Check if connector is already registered */
    if(H5I_iterate(H5I_VOL, H5VL__get_connector_cb, &op_data, TRUE) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_BADITER, H5I_INVALID_HID, "can't iterate over VOL ids")

    /* If connector alread registered, increment ref count on ID and return ID */
    if(op_data.found_id != H5I_INVALID_HID) {
        if(H5I_inc_ref(op_data.found_id, TRUE) < 0)
            HGOTO_ERROR(H5E_VOL, H5E_CANTINC, H5I_INVALID_HID, "unable to increment ref count on VOL connector")
        ret_value = op_data.found_id;
    } /* end if */
    else {
        H5PL_key_t key;
        const H5VL_class_t *cls;

        /* Try loading the connector */
        key.vol.kind = H5VL_GET_CONNECTOR_BY_NAME;
        key.vol.u.name = name;
        if(NULL == (cls = (const H5VL_class_t *)H5PL_load(H5PL_TYPE_VOL, &key)))
            HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, H5I_INVALID_HID, "unable to load VOL connector")

        /* Register the connector we loaded */
        if((ret_value = H5VL_register_connector(cls, TRUE, vipl_id)) < 0)
            HGOTO_ERROR(H5E_VOL, H5E_CANTREGISTER, H5I_INVALID_HID, "unable to register VOL connector ID")
    } /* end else */

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLregister_connector_by_name() */


/*-------------------------------------------------------------------------
 * Function:    H5VLregister_connector_by_value
 *
 * Purpose:     Registers a new VOL connector as a member of the virtual object
 *              layer class.
 *
 * Return:      Success:    A VOL connector ID which is good until the
 *                          library is closed or the connector is
 *                          unregistered.
 *
 *              Failure:    H5I_INVALID_HID
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5VLregister_connector_by_value(H5VL_class_value_t value, hid_t vipl_id)
{
    H5VL_get_connector_ud_t op_data;       /* Callback info for connector search */
    hid_t ret_value = H5I_INVALID_HID;

    FUNC_ENTER_API(H5I_INVALID_HID)

    /* Check arguments */
    if(value < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_UNINITIALIZED, H5I_INVALID_HID, "negative VOL connector value is disallowed")

    op_data.kind = H5VL_GET_CONNECTOR_BY_VALUE;
    op_data.u.value = value;
    op_data.found_id = H5I_INVALID_HID;

    /* Check if connector is already registered */
    if(H5I_iterate(H5I_VOL, H5VL__get_connector_cb, &op_data, TRUE) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_BADITER, H5I_INVALID_HID, "can't iterate over VOL ids")

    /* If connector alread registered, increment ref count on ID and return ID */
    if(op_data.found_id != H5I_INVALID_HID) {
        if(H5I_inc_ref(op_data.found_id, TRUE) < 0)
            HGOTO_ERROR(H5E_VOL, H5E_CANTINC, H5I_INVALID_HID, "unable to increment ref count on VOL connector")
        ret_value = op_data.found_id;
    } /* end if */
    else {
        H5PL_key_t key;
        const H5VL_class_t *cls;

        /* Try loading the connector */
        key.vol.kind = H5VL_GET_CONNECTOR_BY_NAME;
        key.vol.u.value = value;
        if(NULL == (cls = (const H5VL_class_t *)H5PL_load(H5PL_TYPE_VOL, &key)))
            HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, H5I_INVALID_HID, "unable to load VOL connector")

        /* Register the connector we loaded */
        if((ret_value = H5VL_register_connector(cls, TRUE, vipl_id)) < 0)
            HGOTO_ERROR(H5E_VOL, H5E_CANTREGISTER, H5I_INVALID_HID, "unable to register VOL connector ID")
    } /* end else */

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLregister_connector_by_value() */


/*-------------------------------------------------------------------------
 * Function:    H5VLis_connector_registered
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
H5VLis_connector_registered(const char *name)
{
    H5VL_get_connector_ud_t op_data;       /* Callback info for connector search */
    htri_t ret_value = FALSE;           /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE1("t", "*s", name);

    op_data.kind = H5VL_GET_CONNECTOR_BY_NAME;
    op_data.u.name = name;
    op_data.found_id = H5I_INVALID_HID;

    /* Check arguments */
    if (H5I_iterate(H5I_VOL, H5VL__get_connector_cb, &op_data, TRUE) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_BADITER, FAIL, "can't iterate over VOL ids")

    if (op_data.found_id != H5I_INVALID_HID)
        ret_value = TRUE;

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLis_connector_registered() */


/*-------------------------------------------------------------------------
 * Function:    H5VLget_connector_id
 *
 * Purpose:     Retrieves the ID for a registered VOL connector.
 *
 * Return:      Positive if the VOL class has been registered
 *              Negative on error (if the class is not a valid class or not registered)
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5VLget_connector_id(const char *name)
{
    H5VL_get_connector_ud_t op_data;       /* Callback info for connector search */
    hid_t ret_value = H5I_INVALID_HID;  /* Return value */

    FUNC_ENTER_API(H5I_INVALID_HID)
    H5TRACE1("i", "*s", name);

    op_data.kind = H5VL_GET_CONNECTOR_BY_NAME;
    op_data.u.name = name;
    op_data.found_id = H5I_INVALID_HID;

    /* Check arguments */
    if (H5I_iterate(H5I_VOL, H5VL__get_connector_cb, &op_data, TRUE) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_BADITER, H5I_INVALID_HID, "can't iterate over VOL connector IDs")

    if (op_data.found_id != H5I_INVALID_HID) {
        if (H5I_inc_ref(op_data.found_id, TRUE) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTINC, H5I_INVALID_HID, "unable to increment ref count on VOL connector")
        ret_value = op_data.found_id;
    } /* end if */

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLget_connector_id() */


/*-------------------------------------------------------------------------
 * Function:    H5VLget_connector_name
 *
 * Purpose:     Returns the connector name for the VOL associated with the
 *              object or file ID
 *
 * Return:      Success:        The length of the connector name
 *
 *              Failure:        Negative
 *
 *-------------------------------------------------------------------------
 */
ssize_t
H5VLget_connector_name(hid_t obj_id, char *name/*out*/, size_t size)
{
    ssize_t    ret_value = -1;

    FUNC_ENTER_API(FAIL)
    H5TRACE3("Zs", "ixz", obj_id, name, size);

    if ((ret_value = H5VL_get_connector_name(obj_id, name, size)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "Can't get connector name")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLget_connector_name() */


/*-------------------------------------------------------------------------
 * Function:    H5VLclose
 *
 * Purpose:     Closes a VOL connector ID.  This in no way affects
 *              file access property lists which have been defined to use
 *              this VOL connector or files which are already opened under with
 *              this connector.
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
        HGOTO_ERROR(H5E_VOL, H5E_BADTYPE, FAIL, "not a VOL connector")

    /* Decrement the ref count on the ID, possibly releasing the VOL connector */
    if(H5I_dec_app_ref(vol_id) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTDEC, FAIL, "unable to close VOL connector ID")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLclose() */


/*-------------------------------------------------------------------------
 * Function:    H5VLunregister_connector
 *
 * Purpose:     Removes a VOL connector ID from the library. This in no way affects
 *              file access property lists which have been defined to use
 *              this VOL connector or files which are already opened under with
 *              this connector.
 *
 * Return:      Success:    Non-negative
 *
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLunregister_connector(hid_t vol_id)
{
    herr_t ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE1("e", "i", vol_id);

    /* Check arguments */
    if(NULL == H5I_object_verify(vol_id, H5I_VOL))
        HGOTO_ERROR(H5E_VOL, H5E_BADTYPE, FAIL, "not a VOL connector")

    /* The H5VL_class_t struct will be freed by this function */
    if (H5I_dec_app_ref(vol_id) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTDEC, FAIL, "unable to unregister VOL connector")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLunregister_connector() */


/*---------------------------------------------------------------------------
 * Function:    H5VLcmp_connector_cls
 *
 * Purpose:     Compares two connector classes
 *
 * Return:      Success:    Non-negative, with *cmp set to positive if
 *		connector_id1 is greater than connector_id2, negative if connector_id2
 *		is greater than connector_id1 and zero if connector_id1 and connector_id2
 *		are equal.
 *
 *              Failure:    Negative
 *
 *---------------------------------------------------------------------------
 */
herr_t
H5VLcmp_connector_cls(int *cmp, hid_t connector_id1, hid_t connector_id2)
{
    H5VL_class_t *cls1, *cls2;  /* connectors for IDs */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE3("e", "*Isii", cmp, connector_id1, connector_id2);

    /* Check args and get class pointers */
    if(NULL == (cls1 = (H5VL_class_t *)H5I_object_verify(connector_id1, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL connector ID")
    if(NULL == (cls2 = (H5VL_class_t *)H5I_object_verify(connector_id2, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL connector ID")

    /* Compare the two VOL connector classes */
    *cmp = H5VL_cmp_connector_cls(cls1, cls2);

done:
    FUNC_LEAVE_API(ret_value)
} /* H5VLcmp_connector_cls() */

