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
    if (cls->info_cls.copy && !cls->info_cls.free)
        HGOTO_ERROR(H5E_VOL, H5E_CANTREGISTER, H5I_INVALID_HID, "VOL connector must provide free callback for VOL info objects when a copy callback is provided")
    if (cls->wrap_cls.get_wrap_ctx && !cls->wrap_cls.free_wrap_ctx)
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
    H5TRACE2("i", "VCi", value, vipl_id);

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
        key.vol.kind = H5VL_GET_CONNECTOR_BY_VALUE;
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
 * Purpose:     Compares two connector classes (based on their value field)
 *
 * Note:	This routine is _only_ for HDF5 VOL connector authors!  It is
 *		_not_ part of the public API for HDF5 application developers.
 *
 * Return:      Success:    Non-negative, *cmp set to a value like strcmp
 *
 *              Failure:    Negative, *cmp unset
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
    if(H5VL_cmp_connector_cls(cmp, cls1, cls2) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTCOMPARE, FAIL, "can't compare connector classes")

done:
    FUNC_LEAVE_API(ret_value)
} /* H5VLcmp_connector_cls() */


/*---------------------------------------------------------------------------
 * Function:    H5VLwrap_register
 *
 * Purpose:     Wrap an internal object with a "wrap context" and register an
 *		hid_t for the resulting object.
 *
 * Note:	This routine is mainly targeted toward wrapping objects for
 *		iteration routine callbacks (i.e. the callbacks from H5Aiterate*,
 *		H5Literate* / H5Lvisit*, and H5Ovisit* ).
 *
 * Return:      Success:    Non-negative hid_t for the object.
 *              Failure:    Negative (H5I_INVALID_HID)
 *
 *---------------------------------------------------------------------------
 */
hid_t
H5VLwrap_register(void *obj, H5I_type_t type)
{
    hid_t ret_value;            /* Return value */

    /* Use FUNC_ENTER_API_NOINIT here, so the API context doesn't get reset */
    FUNC_ENTER_API_NOINIT
    H5TRACE2("i", "*xIt", obj, type);

    /* Check args */
    if(type <= H5I_BADID || type >= H5I_NTYPES)
        HGOTO_ERROR(H5E_VOL, H5E_BADRANGE, H5I_INVALID_HID, "invalid type number")
    if(NULL == obj)
        HGOTO_ERROR(H5E_VOL, H5E_BADVALUE, H5I_INVALID_HID, "obj is NULL")

    /* Wrap the object and register an ID for it */
    if((ret_value = H5VL_wrap_register(type, obj, TRUE)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTREGISTER, H5I_INVALID_HID, "unable to wrap object")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* H5VLwrap_register() */


/*---------------------------------------------------------------------------
 * Function:    H5VLobject
 *
 * Purpose:     Retrieve the object pointer associated with an hid_t for a.
 *		VOL object.
 *
 * Note:	This routine is mainly targeted toward unwrapping objects for
 *		testing.
 *
 * Return:      Success:    Object pointer
 *              Failure:    NULL
 *
 *---------------------------------------------------------------------------
 */
void *
H5VLobject(hid_t id)
{
    void *ret_value;            /* Return value */

    FUNC_ENTER_API(NULL)
    H5TRACE1("*x", "i", id);

    /* Retrieve the object pointer for the ID */
    if(NULL == (ret_value = H5VL_object(id)))
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, NULL, "unable to retrieve object")

done:
    FUNC_LEAVE_API(ret_value)
} /* H5VLobject() */


/*---------------------------------------------------------------------------
 * Function:    H5VLretrieve_lib_state
 *
 * Purpose:     Retrieves a copy of the internal state of the HDF5 library,
 *		so that it can be restored later.
 *
 * Note:	This routine is _only_ for HDF5 VOL connector authors!  It is
 *		_not_ part of the public API for HDF5 application developers.
 *
 * Return:      Success:    Non-negative, *state set
 *              Failure:    Negative, *state unset
 *
 * Programmer:	Quincey Koziol
 *		Thursday, January 10, 2019
 *
 *---------------------------------------------------------------------------
 */
herr_t
H5VLretrieve_lib_state(void **state)
{
    herr_t ret_value = SUCCEED; /* Return value */

    /* Must use this, to avoid modifying the API context stack in FUNC_ENTER */
    FUNC_ENTER_API_NOINIT
    H5TRACE1("e", "**x", state);

    /* Check args */
    if(NULL == state)
        HGOTO_ERROR(H5E_VOL, H5E_BADVALUE, FAIL, "invalid state pointer")

    /* Retrieve the library state */
    if(H5VL_retrieve_lib_state(state) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't retrieve library state")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* H5VLretrieve_lib_state() */


/*---------------------------------------------------------------------------
 * Function:    H5VLrestore_lib_state
 *
 * Purpose:     Restores the internal state of the HDF5 library.
 *
 * Note:	This routine is _only_ for HDF5 VOL connector authors!  It is
 *		_not_ part of the public API for HDF5 application developers.
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 * Programmer:	Quincey Koziol
 *		Thursday, January 10, 2019
 *
 *---------------------------------------------------------------------------
 */
herr_t
H5VLrestore_lib_state(const void *state)
{
    herr_t ret_value = SUCCEED; /* Return value */

    /* Must use this, to avoid modifying the API context stack in FUNC_ENTER */
    FUNC_ENTER_API_NOINIT
    H5TRACE1("e", "*x", state);

    /* Check args */
    if(NULL == state)
        HGOTO_ERROR(H5E_VOL, H5E_BADVALUE, FAIL, "invalid state pointer")

    /* Restore the library state */
    if(H5VL_restore_lib_state(state) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTSET, FAIL, "can't restore library state")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* H5VLrestore_lib_state() */


/*---------------------------------------------------------------------------
 * Function:    H5VLreset_lib_state
 *
 * Purpose:     Resets the internal state of the HDF5 library, undoing the
 *		affects of H5VLrestore_lib_state.
 *
 * Note:	This routine is _only_ for HDF5 VOL connector authors!  It is
 *		_not_ part of the public API for HDF5 application developers.
 *
 * Note:	This routine must be called as a "pair" with
 * 		H5VLrestore_lib_state.  It can be called before / after /
 * 		independently of H5VLfree_lib_state.
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 * Programmer:	Quincey Koziol
 *		Saturday, February 23, 2019
 *
 *---------------------------------------------------------------------------
 */
herr_t
H5VLreset_lib_state(void)
{
    herr_t ret_value = SUCCEED; /* Return value */

    /* Must use this, to avoid modifying the API context stack in FUNC_ENTER */
    FUNC_ENTER_API_NOINIT
    H5TRACE0("e","");

    /* Reset the library state */
    if(H5VL_reset_lib_state() < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTRESET, FAIL, "can't reset library state")

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* H5VLreset_lib_state() */


/*---------------------------------------------------------------------------
 * Function:    H5VLfree_lib_state
 *
 * Purpose:     Free a retrieved library state.
 *
 * Note:	This routine is _only_ for HDF5 VOL connector authors!  It is
 *		_not_ part of the public API for HDF5 application developers.
 *
 * Note:	This routine must be called as a "pair" with
 * 		H5VLretrieve_lib_state.
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 * Programmer:	Quincey Koziol
 *		Thursday, January 10, 2019
 *
 *---------------------------------------------------------------------------
 */
herr_t
H5VLfree_lib_state(void *state)
{
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE1("e", "*x", state);

    /* Check args */
    if(NULL == state)
        HGOTO_ERROR(H5E_VOL, H5E_BADVALUE, FAIL, "invalid state pointer")

    /* Free the library state */
    if(H5VL_free_lib_state(state) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTRELEASE, FAIL, "can't free library state")

done:
    FUNC_LEAVE_API(ret_value)
} /* H5VLfree_lib_state() */

