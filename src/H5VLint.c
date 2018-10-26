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
#include "H5CXprivate.h"        /* API Contexts                                     */
#include "H5Eprivate.h"         /* Error handling                                   */
#include "H5FLprivate.h"        /* Free lists                                       */
#include "H5Iprivate.h"         /* IDs                                              */
#include "H5MMprivate.h"        /* Memory management                                */
#include "H5Tprivate.h"         /* Datatypes                                        */
#include "H5VLpkg.h"            /* Virtual Object Layer                             */


/****************/
/* Local Macros */
/****************/

/******************/
/* Local Typedefs */
/******************/

/* Object wrapping context info */
typedef struct H5VL_wrap_ctx_t {
    const H5VL_t *plugin;       /* VOL plugin for "outermost" class to start wrap */
    void *obj_wrap_ctx;         /* "wrap context" for outermost plugin */
} H5VL_wrap_ctx_t;


/********************/
/* Package Typedefs */
/********************/

/********************/
/* Local Prototypes */
/********************/
static herr_t H5VL__free_cls(H5VL_class_t *cls);
static void *H5VL__object(hid_t id, H5I_type_t obj_type);

/*********************/
/* Package Variables */
/*********************/

/* Package initialization variable */
hbool_t H5_PKG_INIT_VAR = FALSE;

/*****************************/
/* Library Private Variables */
/*****************************/

/*******************/
/* Local Variables */
/*******************/

/* VOL ID class */
static const H5I_class_t H5I_VOL_CLS[1] = {{
    H5I_VOL,                    /* ID class value */
    0,                          /* Class flags */
    0,                          /* # of reserved IDs for class */
    (H5I_free_t)H5VL__free_cls  /* Callback routine for closing objects of this class */
}};

/* Declare a free list to manage the H5VL_class_t struct */
H5FL_DEFINE_STATIC(H5VL_class_t);

/* Declare a free list to manage the H5VL_t struct */
H5FL_DEFINE(H5VL_t);

/* Declare a free list to manage the H5VL_object_t struct */
H5FL_DEFINE(H5VL_object_t);

/* Declare a free list to manage the H5VL_wrap_ctx_t struct */
H5FL_DEFINE_STATIC(H5VL_wrap_ctx_t);



/*-------------------------------------------------------------------------
 * Function:    H5VL_init
 *
 * Purpose:     Initialize the interface from some other package
 *
 * Return:      Success:    Non-negative
 *
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_init(void)
{
    herr_t ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* FUNC_ENTER() does all the work */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_init() */


/*-------------------------------------------------------------------------
 * Function:	H5VL__init_package
 *
 * Purpose:     Initialize interface-specific information
 *
 * Return:      Success:    Non-negative
 *
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL__init_package(void)
{
    herr_t ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_PACKAGE

    /* Initialize the atom group for the VL IDs */
    if (H5I_register_type(H5I_VOL_CLS) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to initialize H5VL interface")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__init_package() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_term_package
 *
 * Purpose:     Terminate various H5VL objects
 *
 * Return:      Success:    Positive if anything was done that might
 *                          affect other interfaces; zero otherwise.
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
int
H5VL_term_package(void)
{
    int	n = 0;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    if (H5_PKG_INIT_VAR) {
        if (H5I_nmembers(H5I_VOL) > 0) {
            (void)H5I_clear_type(H5I_VOL, FALSE, FALSE);
            n++;
        }
        else {
            /* Destroy the VOL plugin ID group */
            n += (H5I_dec_type_ref(H5I_VOL) > 0);

            /* Mark interface as closed */
            if (0 == n)
                H5_PKG_INIT_VAR = FALSE;
        }
    }

    FUNC_LEAVE_NOAPI(n)
} /* end H5VL_term_package() */


/*-------------------------------------------------------------------------
 * Function:    H5VL__free_cls
 *
 * Purpose:     Frees a file VOL class struct and returns an indication of
 *              success. This function is used as the free callback for the
 *              virtual object layer object identifiers
 *              (c.f.: H5VL_init_interface).
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL__free_cls(H5VL_class_t *cls)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_STATIC

    /* Sanity check */
    HDassert(cls);

    /* Shut down the VOL plugin */
    if(cls->terminate && cls->terminate() < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTCLOSEOBJ, FAIL, "VOL plugin did not terminate cleanly")

    /* XXX (VOL MERGE): We'll leak memory if the name string was dynamically allocated. */
    H5FL_FREE(H5VL_class_t, cls);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__free_cls() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_register
 *
 * Purpose:     VOL-aware version of H5I_register. Constructs an H5VL_object_t
 *              from the passed-in object and registers that. Does the right
 *              thing with datatypes, which are complicated under the VOL.
 *
 * Return:      Success:    A valid HDF5 ID
 *              Failure:    H5I_INVALID_HID
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5VL_register(H5I_type_t type, void *object, H5VL_t *vol_plugin, hbool_t app_ref)
{
    H5VL_object_t  *vol_obj     = NULL;
    hid_t           ret_value   = H5I_INVALID_HID;

    FUNC_ENTER_NOAPI(H5I_INVALID_HID)

    /* Check arguments */
    HDassert(object);
    HDassert(vol_plugin);

    /* Set up VOL object to wrap the passed-in data */
    if (NULL == (vol_obj = H5FL_CALLOC(H5VL_object_t)))
        HGOTO_ERROR(H5E_VOL, H5E_CANTALLOC, H5I_INVALID_HID, "can't allocate top object structure")
    vol_obj->plugin = vol_plugin;
    vol_obj->data = object;

    /* Increment ref count, for new object */
    vol_plugin->nrefs++;

    /* Datatypes need special handling under the VOL, since they have a non-VOL aspect */
    if (H5I_DATATYPE == type) {
        H5T_t *dt;

        /* Wrap "real" (non-named) datatype around VOL object, so it's compatible with H5T interface */
        if (NULL == (dt = H5T_construct_datatype(vol_obj)))
            HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, H5I_INVALID_HID, "can't construct datatype object")

        /* New object is _actually_ the datatype */
        vol_obj = (H5VL_object_t *)dt;
    } /* end if */
    
    /* Register VOL object as _object_ type, for future object API calls */
    /* (Except for named datatypes, as above) */
    if ((ret_value = H5I_register(type, vol_obj, app_ref)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTREGISTER, H5I_INVALID_HID, "unable to atomize handle")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_register() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_register_using_existing_id
 *
 * Purpose:     Registers an OBJECT in a TYPE with the supplied ID for it.
 *              This routine will check to ensure the supplied ID is not already
 *              in use, and ensure that it is a valid ID for the given type, 
 *              but will NOT check to ensure the OBJECT is not already
 *              registered (thus, it is possible to register one object under
 *              multiple IDs).
 *
 * NOTE:        Intended for use in refresh calls, where we have to close
 *              and re-open the underlying data, then hook the VOL object back
 *              up to the original ID.
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_register_using_existing_id(H5I_type_t type, void *object, H5VL_t *vol_plugin, hbool_t app_ref, hid_t existing_id)
{
    H5VL_object_t  *new_vol_obj = NULL;     /* Pointer to new VOL object                    */
    void           *stored_obj = NULL;      /* Pointer to the object that will be stored    */
    herr_t          ret_value = SUCCEED;    /* Return value                                 */

    FUNC_ENTER_NOAPI(FAIL)

    /* Check arguments */
    HDassert(object);
    HDassert(vol_plugin);

    /* Make sure type number is valid */
    if(type != H5I_ATTR && type != H5I_DATASET && type != H5I_DATATYPE && type != H5I_FILE && type != H5I_GROUP)
        HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, FAIL, "invalid type number")

    /* Set up the new VOL object */
    if(NULL == (new_vol_obj = H5FL_CALLOC(H5VL_object_t)))
        HGOTO_ERROR(H5E_VOL, H5E_CANTALLOC, FAIL, "can't allocate memory for VOL object");
    new_vol_obj->plugin = vol_plugin;
    new_vol_obj->data = object;

    /* Bump the reference count on the VOL plugin */
    vol_plugin->nrefs++;

    /* If this is a datatype, we have to hide the VOL object under the H5T_t pointer */
    if(H5I_DATATYPE == type) {
        if(NULL == (stored_obj = (void *)H5T_construct_datatype(new_vol_obj)))
            HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "can't construct datatype object");
    }
    else
        stored_obj = (void *)new_vol_obj;

    /* Call the underlying H5I function to complete the registration */
    if(H5I_register_using_existing_id(type, stored_obj, app_ref, existing_id) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTREGISTER, FAIL, "can't register object under existing ID")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_register_using_existing_id() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_register_using_vol_id
 *
 * Purpose:     Utility function to create a user ID for an object created
 *              or opened through the VOL. Uses the VOL plugin's ID to
 *              get the plugin information instead of it being passed in.
 *
 * Return:      Success:    A valid HDF5 ID
 *              Failure:    H5I_INVALID_HID 
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5VL_register_using_vol_id(H5I_type_t type, void *obj, hid_t plugin_id, hbool_t app_ref)
{
    H5VL_class_t    *cls = NULL;
    H5VL_t          *plugin = NULL;       /* VOL plugin struct */
    hid_t           ret_value = H5I_INVALID_HID;

    FUNC_ENTER_NOAPI(FAIL)

    /* Get the VOL class object from the plugin's ID */
    if (NULL == (cls = (H5VL_class_t *)H5I_object_verify(plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_VOL, H5E_BADTYPE, H5I_INVALID_HID, "not a VOL plugin ID")

    /* Setup VOL info struct */
    if (NULL == (plugin = H5FL_CALLOC(H5VL_t)))
        HGOTO_ERROR(H5E_VOL, H5E_CANTALLOC, H5I_INVALID_HID, "can't allocate VOL info struct")
    plugin->cls = cls;
    plugin->id = plugin_id;
    if (H5I_inc_ref(plugin->id, FALSE) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTINC, H5I_INVALID_HID, "unable to increment ref count on VOL plugin")

    /* Get an ID for the VOL object */
    if ((ret_value = H5VL_register(type, obj, plugin, app_ref)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTREGISTER, H5I_INVALID_HID, "unable to register object handle")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_register_using_vol_id() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_free_object
 *
 * Purpose:     Wrapper to unregister an object ID with a VOL aux struct
 *              and decrement ref count on VOL plugin ID
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_free_object(H5VL_object_t *vol_obj)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(SUCCEED)

    /* Check arguments */
    HDassert(vol_obj);

    vol_obj->plugin->nrefs --;

    if(0 == vol_obj->plugin->nrefs) {
        if(H5I_dec_ref(vol_obj->plugin->id) < 0)
            HGOTO_ERROR(H5E_VOL, H5E_CANTDEC, FAIL, "unable to decrement ref count on VOL plugin")
        vol_obj->plugin = H5FL_FREE(H5VL_t, vol_obj->plugin);
    } /* end if */

    vol_obj = H5FL_FREE(H5VL_object_t, vol_obj);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_free_object() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_register_plugin
 *
 * Purpose:     Registers a new VOL plugin as a member of the virtual object
 *              layer class.
 *
 * Return:      Success:    A VOL plugin ID which is good until the
 *                          library is closed or the plugin is unregistered.
 *
 *              Failure:    H5I_INVALID_HID 
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5VL_register_plugin(const void *_cls, hbool_t app_ref, hid_t vipl_id)
{
    const H5VL_class_t	*cls = (const H5VL_class_t *)_cls;
    H5VL_class_t	*saved = NULL;
    hid_t		ret_value = H5I_INVALID_HID;

    FUNC_ENTER_NOAPI(H5I_INVALID_HID)

    /* Check arguments */
    HDassert(cls);

    /* Copy the class structure so the caller can reuse or free it */
    if (NULL == (saved = H5FL_CALLOC(H5VL_class_t)))
        HGOTO_ERROR(H5E_VOL, H5E_NOSPACE, H5I_INVALID_HID, "memory allocation failed for VOL plugin class struct")
    HDmemcpy(saved, cls, sizeof(H5VL_class_t));

    /* Initialize the VOL plugin */
    if(cls->initialize && cls->initialize(vipl_id) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, H5I_INVALID_HID, "unable to init VOL plugin")

    /* Create the new class ID */
    if ((ret_value = H5I_register(H5I_VOL, saved, app_ref)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTREGISTER, H5I_INVALID_HID, "unable to register VOL plugin ID")

done:
    if (ret_value < 0)
        if (saved)
            H5FL_FREE(H5VL_class_t, saved);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_register_plugin() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_get_plugin_name
 *
 * Purpose:     Private version of H5VLget_plugin_name
 *
 * Return:      Success:        The length of the plugin name
 *              Failure:        Negative
 *
 *-------------------------------------------------------------------------
 */
ssize_t
H5VL_get_plugin_name(hid_t id, char *name /*out*/, size_t size)
{
    H5VL_object_t       *vol_obj;
    const H5VL_class_t  *cls;
    size_t              len;
    ssize_t             ret_value = -1;

    FUNC_ENTER_NOAPI(FAIL)

    /* get the object pointer */
    if (NULL == (vol_obj = H5VL_vol_object(id)))
        HGOTO_ERROR(H5E_VOL, H5E_BADTYPE, FAIL, "invalid VOL identifier")

    cls = vol_obj->plugin->cls;

    len = HDstrlen(cls->name);
    if(name) {
        HDstrncpy(name, cls->name, MIN(len + 1, size));
        if(len >= size)
            name[size - 1] = '\0';
    } /* end if */

    /* Set the return value for the API call */
    ret_value = (ssize_t)len;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_get_plugin_name() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_vol_object
 *
 * Purpose:     Utility function to return the object pointer associated with
 *              a hid_t. This routine is the same as H5I_object for all types
 *              except for named datatypes, where the vol_obj is returned that 
 *              is attached to the H5T_t struct.
 *
 * Return:      Success:        object pointer
 *              Failure:        NULL
 *
 *-------------------------------------------------------------------------
 */
H5VL_object_t *
H5VL_vol_object(hid_t id)
{
    void            *obj = NULL;
    H5I_type_t      obj_type;
    H5VL_object_t   *ret_value = NULL;

    FUNC_ENTER_NOAPI(NULL)

    obj_type = H5I_get_type(id);
    if (H5I_FILE == obj_type || H5I_GROUP == obj_type || H5I_ATTR == obj_type || 
            H5I_DATASET == obj_type || H5I_DATATYPE == obj_type) {
        /* Get the object */
        if (NULL == (obj = H5I_object(id)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "invalid identifier")

        /* if this is a datatype, get the VOL object attached to the H5T_t struct */
        if (H5I_DATATYPE == obj_type)
            if (NULL == (obj = H5T_get_named_type((H5T_t *)obj)))
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a named datatype")
    } /* end if */
    else
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "invalid identifier type to function")

    ret_value = (H5VL_object_t *)obj;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_vol_object() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_object_data
 *
 * Purpose:     Correctly retrieve the 'data' field for a VOL object (H5VL_object),
 *              even for nested / stacked VOL plugins.
 *
 * Return:      Success:        object pointer
 *              Failure:        NULL
 *
 *-------------------------------------------------------------------------
 */
void *
H5VL_object_data(const H5VL_object_t *vol_obj)
{
    void *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Check for 'get_object' callback in plugin */
    if(vol_obj->plugin->cls->get_object)
        ret_value = (vol_obj->plugin->cls->get_object)(vol_obj->data);
    else
        ret_value = vol_obj->data;

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_object_data() */


/*-------------------------------------------------------------------------
 * Function:    H5VL__object
 *
 * Purpose:     Internal function to return the VOL object pointer associated
 *              with an hid_t.
 *
 * Return:      Success:    object pointer
 *              Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL__object(hid_t id, H5I_type_t obj_type)
{
    H5VL_object_t   *vol_obj = NULL;    
    void            *ret_value = NULL;

    FUNC_ENTER_STATIC

    /* Get the underlying object */
    switch(obj_type) {
        case H5I_GROUP:
        case H5I_DATASET:
        case H5I_FILE:            
        case H5I_ATTR:
            /* get the object */
            if (NULL == (vol_obj = (H5VL_object_t *)H5I_object(id)))
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "invalid identifier")
            break;

        case H5I_DATATYPE:
            {
                H5T_t *dt = NULL;

                /* get the object */
                if (NULL == (dt = (H5T_t *)H5I_object(id)))
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "invalid identifier")

                /* Get the actual datatype object that should be the vol_obj */
                if (NULL == (vol_obj = H5T_get_named_type(dt)))
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a named datatype")
                break;
            }

        case H5I_UNINIT:
        case H5I_BADID:
        case H5I_DATASPACE:
        case H5I_REFERENCE:
        case H5I_VFL:
        case H5I_VOL:
        case H5I_GENPROP_CLS:
        case H5I_GENPROP_LST:
        case H5I_ERROR_CLASS:
        case H5I_ERROR_MSG:
        case H5I_ERROR_STACK:
        case H5I_NTYPES:
        default:
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "unknown data object type")
    } /* end switch */

    /* Set the return value */
    ret_value = H5VL_object_data(vol_obj);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__object() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_object
 *
 * Purpose:     Utility function to return the VOL object pointer associated with
 *              a hid_t.
 *
 * Return:      Success:    object pointer
 *              Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
void *
H5VL_object(hid_t id)
{
    void            *ret_value = NULL;

    FUNC_ENTER_NOAPI(NULL)

    /* Get the underlying object */
    if(NULL == (ret_value = H5VL__object(id, H5I_get_type(id))))
        HGOTO_ERROR(H5E_ARGS, H5E_CANTGET, NULL, "can't retrieve object for ID")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_object() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_object_verify
 *
 * Purpose:     Utility function to return the VOL object pointer associated
 *              with an identifier.
 *
 * Return:      Success:    object pointer
 *              Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
void *
H5VL_object_verify(hid_t id, H5I_type_t obj_type)
{
    void            *ret_value = NULL;

    FUNC_ENTER_NOAPI(NULL)

    /* Check of ID of correct type */
    if(obj_type != H5I_get_type(id))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "invalid identifier")

    /* Get the underlying object */
    if(NULL == (ret_value = H5VL__object(id, obj_type)))
        HGOTO_ERROR(H5E_ARGS, H5E_CANTGET, NULL, "can't retrieve object for ID")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_object_verify() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_cmp_plugin_cls
 *
 * Purpose:     Compare VOL class for a plugin
 *
 * Return:      Positive if VALUE1 is greater than VALUE2, negative if
 *              VALUE2 is greater than VALUE1 and zero if VALUE1 and
 *              VALUE2 are equal.
 *
 *-------------------------------------------------------------------------
 */
int
H5VL_cmp_plugin_cls(const H5VL_class_t *cls1, const H5VL_class_t *cls2)
{
    int cmp_value;              /* Value from comparison */
    int ret_value = 0;          /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(cls1);
    HDassert(cls1);

    /* Compare plugin "values" */
    if(cls1->value < cls2->value)
        HGOTO_DONE(-1)
    if(cls1->value > cls2->value)
        HGOTO_DONE(1)
    HDassert(cls1->value == cls2->value);

    /* Compare plugin names */
    if(cls1->name == NULL && cls2->name != NULL)
        HGOTO_DONE(-1);
    if(cls1->name != NULL && cls2->name == NULL)
        HGOTO_DONE(1);
    if(0 != (cmp_value = HDstrcmp(cls1->name, cls2->name)))
        HGOTO_DONE(cmp_value);

    /* Compare plugin VOL API versions */
    if(cls1->version < cls2->version)
        HGOTO_DONE(-1)
    if(cls1->version > cls2->version)
        HGOTO_DONE(1)
    HDassert(cls1->version == cls2->version);

    /* Compare plugin info */
    if(cls1->info_size < cls2->info_size)
        HGOTO_DONE(-1)
    if(cls1->info_size > cls2->info_size)
        HGOTO_DONE(1)
    HDassert(cls1->info_size == cls2->info_size);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_cmp_plugin_cls() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_set_vol_wrapper
 *
 * Purpose:     Set up object wrapping context for current VOL plugin
 *
 * Return:      SUCCEED / FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_set_vol_wrapper(void *obj, const H5VL_t *plugin)
{
    H5VL_wrap_ctx_t *vol_wrap_ctx = NULL;       /* Object wrapping context */
    void *obj_wrap_ctx = NULL;          /* VOL plugin's wrapping context */
    herr_t ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(obj);
    HDassert(plugin);

    /* Check if the plugin can create a wrap context */
    if(plugin->cls->get_wrap_ctx) {
        /* Sanity check */
        HDassert(plugin->cls->free_wrap_ctx);

        /* Get the wrap context from the plugin */
        if((plugin->cls->get_wrap_ctx)(obj, &obj_wrap_ctx) < 0)
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't retrieve VOL plugin's object wrap context")
    } /* end if */

    /* Allocate VOL object wrapper context */
    if(NULL == (vol_wrap_ctx = H5FL_MALLOC(H5VL_wrap_ctx_t)))
        HGOTO_ERROR(H5E_VOL, H5E_CANTALLOC, FAIL, "can't allocate VOL wrap context")

    /* Set up VOL object wrapper context */
    vol_wrap_ctx->plugin = plugin;
    vol_wrap_ctx->obj_wrap_ctx = obj_wrap_ctx;

    /* Save the wrapper context */
    if(H5CX_set_vol_wrap_ctx(vol_wrap_ctx) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTSET, FAIL, "can't set VOL object wrap context")

done:
    if(ret_value < 0 && vol_wrap_ctx)
        /* Release object wrapping context */
        H5FL_FREE(H5VL_wrap_ctx_t, vol_wrap_ctx);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_set_vol_wrapper() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_reset_vol_wrapper
 *
 * Purpose:     Reset object wrapping context for current VOL plugin
 *
 * Return:      SUCCEED / FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_reset_vol_wrapper(void)
{
    H5VL_wrap_ctx_t *vol_wrap_ctx = NULL;       /* Object wrapping context */
    herr_t ret_value = SUCCEED;                 /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Retrieve the VOL object wrap context */
    if(H5CX_get_vol_wrap_ctx((void **)&vol_wrap_ctx) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't get VOL object wrap context")

    /* If there is a VOL plugin object wrapping context, release it */
    if(vol_wrap_ctx->obj_wrap_ctx) {
        /* Release the VOL plugin's object wrapping context */
        if((*vol_wrap_ctx->plugin->cls->free_wrap_ctx)(vol_wrap_ctx->obj_wrap_ctx) < 0)
            HGOTO_ERROR(H5E_VOL, H5E_CANTRELEASE, FAIL, "unable to release plugin's object wrapping context")
    } /* end if */

    /* Release object wrapping context */
    H5FL_FREE(H5VL_wrap_ctx_t, vol_wrap_ctx);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_reset_vol_wrapper() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_wrap_register
 *
 * Purpose:     Wrap an object and register an ID for it
 *
 * Return:      SUCCEED / FAIL
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5VL_wrap_register(H5I_type_t type, void *obj, hbool_t app_ref)
{
    H5VL_wrap_ctx_t *vol_wrap_ctx = NULL;       /* Object wrapping context */
    void *new_obj;                      /* Newly wrapped object */
    hid_t ret_value = H5I_INVALID_HID;  /* Return value */

    FUNC_ENTER_NOAPI(H5I_INVALID_HID)

    /* Sanity check */
    HDassert(obj);

    /* If the datatype is already VOL-managed, the datatype's vol_obj
     * field will get clobbered later, so disallow this.
     */
    if(type == H5I_DATATYPE)
        if(TRUE == H5T_already_vol_managed((const H5T_t *)obj))
            HGOTO_ERROR(H5E_VOL, H5E_BADTYPE, H5I_INVALID_HID, "can only get an ID for an uncommitted datatype")

    /* Retrieve the VOL object wrap context */
    if(H5CX_get_vol_wrap_ctx((void **)&vol_wrap_ctx) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, H5I_INVALID_HID, "can't get VOL object wrap context")

    /* If there is a VOL object wrapping context, wrap the object */
    if(vol_wrap_ctx) {
        /* Wrap object */
        if(NULL == (new_obj = H5VL_wrap_object(vol_wrap_ctx->plugin->cls, vol_wrap_ctx->obj_wrap_ctx, obj)))
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, H5I_INVALID_HID, "can't wrap object")
    } /* end if */
    else
        new_obj = obj;

    /* Get an ID for the object */
    if((ret_value = H5VL_register_using_vol_id(type, new_obj, vol_wrap_ctx->plugin->id, app_ref)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTREGISTER, H5I_INVALID_HID, "unable to get an ID for the object")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_wrap_register() */

