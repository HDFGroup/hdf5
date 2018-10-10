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
#include "H5FLprivate.h"        /* Free lists                                       */
#include "H5Iprivate.h"         /* IDs                                              */
#include "H5MMprivate.h"        /* Memory management                                */
#include "H5Pprivate.h"         /* Property lists                                   */
#include "H5Tprivate.h"         /* Datatypes                                        */
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

static herr_t H5VL_free_cls(H5VL_class_t *cls);

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
    (H5I_free_t)H5VL_free_cls   /* Callback routine for closing objects of this class */
}};

/* Declare a free list to manage the H5VL_t struct */
H5FL_DEFINE(H5VL_t);

/* Declare a free list to manage the H5VL_object_t struct */
H5FL_DEFINE(H5VL_object_t);


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
        HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to initialize H5VL interface");

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
            /* Destroy the VOL driver ID group */
            n += (H5I_dec_type_ref(H5I_VOL) > 0);

            /* Mark interface as closed */
            if (0 == n)
                H5_PKG_INIT_VAR = FALSE;
        }
    }

    FUNC_LEAVE_NOAPI(n)
} /* end H5VL_term_package() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_free_cls
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
H5VL_free_cls(H5VL_class_t *cls)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* Sanity check */
    HDassert(cls);

    /* XXX: Need to retrieve the VOL termination property list for the
     * terminate operation - JTH
     */
    if (cls->terminate && cls->terminate(H5P_DEFAULT) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTCLOSEOBJ, FAIL, "VOL driver did not terminate cleanly");

    /* XXX (VOL MERGE): We'll leak memory if the name string was dynamically allocated. */
    H5MM_xfree(cls);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_free_cls() */


/*-------------------------------------------------------------------------
 * Function:   H5VL_register_id
 *
 * Purpose:    Wrapper to register an object ID with a VOL aux struct 
 *             and increment ref count on VOL driver ID
 *
 * Return:     Success:     A valid HDF5 ID
 *              Failure:    H5I_INVALID_HID
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5VL_register_id(H5I_type_t type, void *object, H5VL_t *vol_driver, hbool_t app_ref)
{
    H5VL_object_t  *new_obj     = NULL;
    hid_t           ret_value   = H5I_INVALID_HID;

    FUNC_ENTER_NOAPI(H5I_INVALID_HID)

    /* Check arguments */
    HDassert(object);
    HDassert(vol_driver);

    /* setup VOL object */
    if (NULL == (new_obj = H5FL_CALLOC(H5VL_object_t)))
        HGOTO_ERROR(H5E_VOL, H5E_CANTALLOC, H5I_INVALID_HID, "can't allocate top object structure");
    new_obj->driver = vol_driver;
    new_obj->data = object;

    vol_driver->nrefs++;

    if (H5I_DATATYPE == type) {

        H5T_t *dt = NULL;

        if (NULL == (dt = H5T_construct_datatype(new_obj)))
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, H5I_INVALID_HID, "can't construct datatype object");

        if ((ret_value = H5I_register(type, dt, app_ref)) < 0)
            HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, H5I_INVALID_HID, "unable to atomize handle");
    }
    else {
        if ((ret_value = H5I_register(type, new_obj, app_ref)) < 0)
            HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, H5I_INVALID_HID, "unable to atomize handle");
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_register_id() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_free_object
 *
 * Purpose:     Wrapper to unregister an object ID with a VOL aux struct
 *              and decrement ref count on VOL driver ID
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

    vol_obj->driver->nrefs --;

    if (0 == vol_obj->driver->nrefs) {
        if (H5I_dec_ref(vol_obj->driver->id) < 0)
            HGOTO_ERROR(H5E_VOL, H5E_CANTDEC, FAIL, "unable to decrement ref count on VOL driver");
        vol_obj->driver = H5FL_FREE(H5VL_t, vol_obj->driver);
    }

    vol_obj = H5FL_FREE(H5VL_object_t, vol_obj);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_free_object() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_register
 *
 * Purpose:     Registers a new VOL driver as a member of the virtual object
 *              layer class.
 *
 * Return:      Success:    A VOL driver ID which is good until the
 *                          library is closed or the driver is unregistered.
 *
 *              Failure:    H5I_INVALID_HID 
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5VL_register(const void *_cls, size_t size, hbool_t app_ref)
{
    const H5VL_class_t	*cls = (const H5VL_class_t *)_cls;
    H5VL_class_t	*saved = NULL;
    hid_t		ret_value = H5I_INVALID_HID;

    FUNC_ENTER_NOAPI(H5I_INVALID_HID)

    /* Check arguments */
    HDassert(cls);

    /* Copy the class structure so the caller can reuse or free it */
    if (NULL == (saved = (H5VL_class_t *)H5MM_calloc(size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, H5I_INVALID_HID, "memory allocation failed for VOL driver class struct");
    HDmemcpy(saved, cls, size);

    /* Create the new class ID */
    if ((ret_value = H5I_register(H5I_VOL, saved, app_ref)) < 0)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, H5I_INVALID_HID, "unable to register VOL driver ID");

done:
    if (ret_value < 0)
        if (saved)
            H5MM_xfree(saved);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_register() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_object_register
 *
 * Purpose:     Utility function to create a user ID for an object created
 *              or opened through the VOL
 *
 * Return:      Success:    A valid HDF5 ID
 *              Failure:    H5I_INVALID_HID 
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5VL_object_register(void *obj, H5I_type_t obj_type, hid_t driver_id, hbool_t app_ref)
{
    H5VL_class_t    *cls = NULL;
    H5VL_t          *driver = NULL;       /* VOL driver struct */
    hid_t           ret_value = H5I_INVALID_HID;

    FUNC_ENTER_NOAPI(FAIL)

    if (NULL == (cls = (H5VL_class_t *)H5I_object_verify(driver_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, H5I_INVALID_HID, "not a VOL driver ID");

    /* Setup VOL info struct */
    if (NULL == (driver = H5FL_CALLOC(H5VL_t)))
        HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, H5I_INVALID_HID, "can't allocate VOL info struct");
    driver->cls = cls;
    driver->id = driver_id;
    if (H5I_inc_ref(driver->id, FALSE) < 0)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTINC, H5I_INVALID_HID, "unable to increment ref count on VOL driver");

    /* Get an ID for the VOL object */
    if ((ret_value = H5VL_register_id(obj_type, obj, driver, app_ref)) < 0)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, H5I_INVALID_HID, "unable to register object handle");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_object_register() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_get_driver_name
 *
 * Purpose:     Private version of H5VLget_driver_name
 *
 * Return:      Success:        The length of the driver name
 *              Failure:        Negative
 *
 *-------------------------------------------------------------------------
 */
ssize_t
H5VL_get_driver_name(hid_t id, char *name /*out*/, size_t size)
{
    H5VL_object_t       *vol_obj = NULL;
    const H5VL_class_t  *cls = NULL;
    size_t              len;
    ssize_t             ret_value = -1;

    FUNC_ENTER_NOAPI(FAIL)

    /* get the object pointer */
    if (NULL == (vol_obj = H5VL_get_object(id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid file identifier");

    cls = vol_obj->driver->cls;

    len = HDstrlen(cls->name);
    if (name) {
        HDstrncpy(name, cls->name, MIN(len + 1,size));
        if (len >= size)
            name[size-1]='\0';
    }

    /* Set the return value for the API call */
    ret_value = (ssize_t)len;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_get_driver_name() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_get_object
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
H5VL_get_object(hid_t id)
{
    void            *obj = NULL;
    H5I_type_t      obj_type = H5I_get_type(id);
    H5VL_object_t   *ret_value = NULL;

    FUNC_ENTER_NOAPI(NULL)

    if (H5I_FILE == obj_type || H5I_GROUP == obj_type || H5I_ATTR == obj_type || 
       H5I_DATASET == obj_type || H5I_DATATYPE == obj_type) {
        /* get the object */
        if (NULL == (obj = H5I_object(id)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "invalid identifier");

        /* if this is a datatype, get the VOL object attached to the H5T_t struct */
        if (H5I_DATATYPE == obj_type) {
            if (NULL == (obj = H5T_get_named_type((H5T_t *)obj)))
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a named datatype");
        }
    }
    else
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "invalid identifier type to function");


    ret_value = (H5VL_object_t *)obj;
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_get_object() */


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
    H5VL_object_t   *vol_obj = NULL;    
    void            *ret_value = NULL;

    FUNC_ENTER_NOAPI(NULL)

    /* Get the symbol table entry */
    switch (H5I_get_type(id)) {
        case H5I_GROUP:
        case H5I_DATASET:
        case H5I_FILE:            
        case H5I_ATTR:
            /* get the object */
            if (NULL == (vol_obj = (H5VL_object_t *)H5I_object(id)))
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "invalid identifier");

            ret_value = vol_obj->data;
            break;
        case H5I_DATATYPE:
            {
                H5T_t *dt = NULL;

                /* get the object */
                if (NULL == (dt = (H5T_t *)H5I_object(id)))
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "invalid identifier");

                /* Get the actual datatype object that should be the vol_obj */
                if (NULL == (vol_obj = H5T_get_named_type(dt)))
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a named datatype");

                ret_value = vol_obj->data;
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
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "unknown data object type");
    }
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
    H5VL_object_t   *vol_obj = NULL;    
    void            *ret_value = NULL;

    FUNC_ENTER_NOAPI(NULL)

    /* Get the symbol table entry */
    switch (obj_type) {
        case H5I_GROUP:
        case H5I_DATASET:
        case H5I_FILE:            
        case H5I_ATTR:
            /* get the object */
            if (NULL == (vol_obj = (H5VL_object_t *)H5I_object_verify(id, obj_type)))
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "invalid identifier");

            ret_value = vol_obj->data;
            break;
        case H5I_DATATYPE:
            {
                H5T_t *dt = NULL;

                /* get the object */
                if (NULL == (dt = (H5T_t *)H5I_object_verify(id, obj_type)))
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "invalid identifier");

                /* Get the actual datatype object that should be the vol_obj */
                if (NULL == (vol_obj = H5T_get_named_type(dt)))
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a named datatype");

                ret_value = vol_obj->data;
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
    }
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_object_verify() */


/* XXX (VOL MERGE): This could be a macro like in H5F */
void *
H5VL_driver_object(H5VL_object_t *vol_obj)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    FUNC_LEAVE_NOAPI(vol_obj->data)
} /* end H5VL_driver_object() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_attr_create
 *
 * Purpose:     Creates an attribute through the VOL
 *
 * Return:      Success: pointer to the new attribute 
 *
 *              Failure: NULL
 *
 *-------------------------------------------------------------------------
 */
void *
H5VL_attr_create(void *obj, H5VL_loc_params_t loc_params, const H5VL_class_t *cls, const char *name, 
                 hid_t acpl_id, hid_t aapl_id, hid_t dxpl_id, void **req)
{
    void *ret_value;  /* Return value */

    FUNC_ENTER_NOAPI(NULL)

    /* check if the corresponding VOL create callback exists */
    if (NULL == cls->attr_cls.create)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, NULL, "VOL driver has no 'attr create' method")

    /* call the corresponding VOL create callback */
    if (NULL == (ret_value = (cls->attr_cls.create) 
                (obj, loc_params, name, acpl_id, aapl_id, dxpl_id, req)))
        HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, NULL, "create failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_attr_create() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_attr_open
 *
 * Purpose:	Opens an attribute through the VOL
 *
 * Return:      Success: pointer to the new attr. 
 *
 *		Failure: NULL
 *
 *-------------------------------------------------------------------------
 */
void *
H5VL_attr_open(void *obj, H5VL_loc_params_t loc_params, const H5VL_class_t *cls, const char *name, 
               hid_t aapl_id, hid_t dxpl_id, void **req)
{
    void *ret_value;  /* Return value */

    FUNC_ENTER_NOAPI(NULL)

    /* check if the type specific corresponding VOL open callback exists */
    if(NULL == cls->attr_cls.open)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, NULL, "VOL driver has no 'attr open' method")

    /* call the corresponding VOL open callback */
    if(NULL == (ret_value = (cls->attr_cls.open) 
                (obj, loc_params, name, aapl_id, dxpl_id, req)))
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPENOBJ, NULL, "attribute open failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_attr_open() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_attr_read
 *
 * Purpose:	Reads data from attr through the VOL
 *
 * Return:	Success:	Non Negative
 *
 *		Failure:	Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t H5VL_attr_read(void *attr, const H5VL_class_t *cls, hid_t mem_type_id, void *buf, 
                      hid_t dxpl_id, void **req)
{
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if(NULL == cls->attr_cls.read)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL driver has no 'attr read' method")
    if((ret_value = (cls->attr_cls.read)(attr, mem_type_id, buf, dxpl_id, req)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTRELEASE, FAIL, "read failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_attr_read() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_attr_write
 *
 * Purpose:	Writes data to attr through the VOL
 *
 * Return:	Success:	Non Negative
 *
 *		Failure:	Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t H5VL_attr_write(void *attr, const H5VL_class_t *cls, hid_t mem_type_id, const void *buf, 
                       hid_t dxpl_id, void **req)
{
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if(NULL == cls->attr_cls.write)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL driver has no 'attr write' method")
    if((ret_value = (cls->attr_cls.write)(attr, mem_type_id, buf, dxpl_id, req)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTRELEASE, FAIL, "write failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_attr_write() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_attr_get
 *
 * Purpose:	Get specific information about the attribute through the VOL
 *
 * Return:	Success:        non negative
 *
 *		Failure:	negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_attr_get(void *obj, const H5VL_class_t *cls, H5VL_attr_get_t get_type, 
              hid_t dxpl_id, void **req, ...)
{
    va_list           arguments;             /* argument list passed from the API call */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if(NULL == cls->attr_cls.get)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL driver has no 'attr get' method")

    va_start (arguments, req);
    if((ret_value = (cls->attr_cls.get)(obj, get_type, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "get failed")
    va_end (arguments);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_attr_get() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_attr_specific
 *
 * Purpose:	specific operation on attributes through the VOL
 *
 * Return:	Success:        non negative
 *
 *		Failure:	negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_attr_specific(void *obj, H5VL_loc_params_t loc_params, const H5VL_class_t *cls, 
                   H5VL_attr_specific_t specific_type, hid_t dxpl_id, void **req, ...)
{
    va_list           arguments;             /* argument list passed from the API call */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if(NULL == cls->attr_cls.specific)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL driver has no 'attr specific' method")

    va_start (arguments, req);
    if((ret_value = (cls->attr_cls.specific)(obj, loc_params, specific_type, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "Unable to execute attribute specific callback")
    va_end (arguments);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_attr_specific() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_attr_optional
 *
 * Purpose:	optional operation specific to drivers.
 *
 * Return:	Success:        non negative
 *
 *		Failure:	negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_attr_optional(void *obj, const H5VL_class_t *cls, hid_t dxpl_id, void **req, ...)
{
    va_list           arguments;             /* argument list passed from the API call */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if(NULL == cls->attr_cls.optional)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL driver has no 'attr optional' method")

    va_start (arguments, req);
    if((ret_value = (cls->attr_cls.optional)(obj, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "Unable to execute attribute optional callback")
    va_end (arguments);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_attr_optional() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_attr_close
 *
 * Purpose:     Closes an attribute through the VOL
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_attr_close(void *attr, const H5VL_class_t *cls, hid_t dxpl_id, void **req)
{
    herr_t ret_value = SUCCEED;

    HDassert(attr);
    HDassert(cls);

    FUNC_ENTER_NOAPI(FAIL)
            
    /* Check if the corresponding VOL callback exists */
    if (NULL == cls->attr_cls.close)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL driver has no 'attr close' method");

    /* Call the corresponding VOL callback */
    if ((ret_value = (cls->attr_cls.close)(attr, dxpl_id, req)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTRELEASE, FAIL, "close failed");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_attr_close() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_dataset_create
 *
 * Purpose:	Creates a dataset through the VOL
 *
 * Return:      Success: pointer to dataset
 *
 *		Failure: NULL
 *
 *-------------------------------------------------------------------------
 */
void *
H5VL_dataset_create(void *obj, H5VL_loc_params_t loc_params, const H5VL_class_t *cls, const char *name, 
                    hid_t dcpl_id, hid_t dapl_id, hid_t dxpl_id, void **req)
{
    void *ret_value; /* Return value */

    FUNC_ENTER_NOAPI(NULL)

    /* check if the corresponding VOL create callback exists */
    if(NULL == cls->dataset_cls.create)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, NULL, "VOL driver has no 'dataset create' method")

    /* call the corresponding VOL create callback */
    if(NULL == (ret_value = (cls->dataset_cls.create)
                (obj, loc_params, name, dcpl_id, dapl_id, dxpl_id, req)))
        HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, NULL, "create failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_dataset_create() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_dataset_open
 *
 * Purpose:	Opens a dataset through the VOL
 *
 * Return:      Success: pointer to dataset 
 *
 *		Failure: NULL
 *
 *-------------------------------------------------------------------------
 */
void *
H5VL_dataset_open(void *obj, H5VL_loc_params_t loc_params, const H5VL_class_t *cls, const char *name, 
                  hid_t dapl_id, hid_t dxpl_id, void **req)
{
    void *ret_value; /* Return value */

    FUNC_ENTER_NOAPI(NULL)

    /* check if the type specific corresponding VOL open callback exists */
    if(NULL == cls->dataset_cls.open)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, NULL, "VOL driver has no 'dset open' method")

    /* call the corresponding VOL open callback */
    if(NULL == (ret_value = (cls->dataset_cls.open)
                (obj, loc_params, name, dapl_id, dxpl_id, req)))
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPENOBJ, NULL, "dataset open failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_dataset_open() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_dataset_read
 *
 * Purpose:	Reads data from dataset through the VOL
*
 * Return:	Success:	Non Negative
 *
 *		Failure:	Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t 
H5VL_dataset_read(void *dset, const H5VL_class_t *cls, hid_t mem_type_id, hid_t mem_space_id, 
                  hid_t file_space_id, hid_t plist_id, void *buf, void **req)
{
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if(NULL == cls->dataset_cls.read)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL driver has no 'dataset read' method")
    if((ret_value = (cls->dataset_cls.read)
        (dset, mem_type_id, mem_space_id, file_space_id, plist_id, buf, req)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_READERROR, FAIL, "read failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_dataset_read() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_dataset_write
 *
 * Purpose:	Writes data from dataset through the VOL
 *
 * Return:	Success:	Non Negative
 *
 *		Failure:	Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t 
H5VL_dataset_write(void *dset, const H5VL_class_t *cls, hid_t mem_type_id, hid_t mem_space_id, 
                   hid_t file_space_id, hid_t plist_id, const void *buf, void **req)
{
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if(NULL == cls->dataset_cls.write)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL driver has no 'dataset write' method")
    if((ret_value = (cls->dataset_cls.write)
        (dset, mem_type_id, mem_space_id, file_space_id, plist_id, buf, req)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_WRITEERROR, FAIL, "write failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_dataset_write() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_dataset_get
 *
 * Purpose:	Get specific information about the dataset through the VOL
 *
 * Return:	Success:        non negative
 *
 *		Failure:	negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_dataset_get(void *dset, const H5VL_class_t *cls, H5VL_dataset_get_t get_type, 
                 hid_t dxpl_id, void **req, ...)
{
    va_list           arguments;             /* argument list passed from the API call */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if(NULL == cls->dataset_cls.get)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL driver has no 'dataset get' method")

    va_start (arguments, req);
    if((ret_value = (cls->dataset_cls.get)(dset, get_type, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "get failed")
    va_end (arguments);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_dataset_get() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_dataset_specific
 *
 * Purpose:	specific operation on datasets through the VOL
 *
 * Return:	Success:        non negative
 *
 *		Failure:	negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_dataset_specific(void *obj, const H5VL_class_t *cls, H5VL_dataset_specific_t specific_type, 
                       hid_t dxpl_id, void **req, ...)
{
    va_list           arguments;             /* argument list passed from the API call */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if(NULL == cls->dataset_cls.specific)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL driver has no 'dataset specific' method")

    va_start (arguments, req);
    if((ret_value = (cls->dataset_cls.specific)
        (obj, specific_type, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "Unable to execute dataset specific callback")
    va_end (arguments);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_dataset_specific() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_dataset_optional
 *
 * Purpose:	optional operation specific to drivers.
 *
 * Return:	Success:        non negative
 *
 *		Failure:	negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_dataset_optional(void *obj, const H5VL_class_t *cls, hid_t dxpl_id, void **req, ...)
{
    va_list           arguments;             /* argument list passed from the API call */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if(NULL == cls->dataset_cls.optional)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL driver has no 'dataset optional' method")

    va_start (arguments, req);
    if((ret_value = (cls->dataset_cls.optional)(obj, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "Unable to execute dataset optional callback")
    va_end (arguments);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_dataset_optional() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_dataset_close
 *
 * Purpose:     Closes a dataset through the VOL
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_dataset_close(void *dset, const H5VL_class_t *cls, hid_t dxpl_id, void **req)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    HDassert(dset);
    HDassert(cls);

    /* Check if the corresponding VOL callback exists */
    if (NULL == cls->dataset_cls.close)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL driver has no 'dset close' method");

    /* Call the corresponding VOL callback */
    if ((ret_value = (cls->dataset_cls.close)(dset, dxpl_id, req)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTRELEASE, FAIL, "close failed");

done:

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_dataset_close() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_file_create
 *
 * Purpose:	Creates a file through the VOL
 *
 * Return:      Success: pointer to file. 
 *
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

    /* check if the corresponding VOL create callback exists */
    if(NULL == cls->file_cls.create)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, NULL, "VOL driver has no 'file create' method")
    /* call the corresponding VOL create callback */
    if(NULL == (ret_value = (cls->file_cls.create)(name, flags, fcpl_id, fapl_id, dxpl_id, req)))
        HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, NULL, "create failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_file_create() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_file_open
 *
 * Purpose:	Opens a file through the VOL.
 *
 * Return:      Success: pointer to file. 
 *
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

    /* check if the corresponding VOL create callback exists */
    if(NULL == cls->file_cls.open)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, NULL, "VOL driver has no 'file open' method")
    /* call the corresponding VOL create callback */
    if(NULL == (ret_value = (cls->file_cls.open)(name, flags, fapl_id, dxpl_id, req)))
        HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, NULL, "open failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_file_open() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_file_get
 *
 * Purpose:	Get specific information about the file through the VOL
 *
 * Return:	Success:        non negative
 *
 *		Failure:	negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_file_get(void *file, const H5VL_class_t *cls, H5VL_file_get_t get_type, 
              hid_t dxpl_id, void **req, ...)
{
    va_list           arguments;             /* argument list passed from the API call */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if(NULL == cls->file_cls.get)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL driver has no 'file get' method")

    va_start(arguments, req);
    if((ret_value = (cls->file_cls.get)(file, get_type, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "get failed")
    va_end(arguments);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_file_get() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_file_specific
 *
 * Purpose:	perform File specific operations through the VOL
 *
 * Return:	Success:        non negative
 *		Failure:	negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_file_specific(void *file, const H5VL_class_t *cls, H5VL_file_specific_t specific_type, 
                   hid_t dxpl_id, void **req, ...)
{
    va_list           arguments;             /* argument list passed from the API call */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if(specific_type == H5VL_FILE_IS_ACCESSIBLE) {
        H5P_genplist_t     *plist;          /* Property list pointer */
        H5VL_driver_prop_t  driver_prop;    /* Property for VOL driver ID & info */
        va_list             tmp_args;       /* argument list passed from the API call */
        hid_t               fapl_id;

        va_start (tmp_args, req);
        fapl_id = va_arg (tmp_args, hid_t);
        va_end (tmp_args);

        /* get the VOL info from the fapl */
        if(NULL == (plist = (H5P_genplist_t *)H5I_object(fapl_id)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access property list")

        if(H5P_peek(plist, H5F_ACS_VOL_DRV_NAME, &driver_prop) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get VOL driver info")

        if(NULL == (cls = (H5VL_class_t *)H5I_object_verify(driver_prop.driver_id, H5I_VOL)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL driver ID")

        va_start (arguments, req);
        if((ret_value = (cls->file_cls.specific)
            (file, specific_type, dxpl_id, req, arguments)) < 0)
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "specific failed")
        va_end (arguments);
    }
    else {
        if(NULL == cls->file_cls.specific)
            HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL driver has no 'file specific' method")

        va_start (arguments, req);
        if((ret_value = (cls->file_cls.specific)
            (file, specific_type, dxpl_id, req, arguments)) < 0)
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "specific failed")
        va_end (arguments);
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_file_specific() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_file_optional
 *
 * Purpose:	perform a driver specific operation
 *
 * Return:	Success:        non negative
 *		Failure:	negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_file_optional(void *file, const H5VL_class_t *cls, hid_t dxpl_id, void **req, ...)
{
    va_list           arguments;             /* argument list passed from the API call */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if(NULL == cls->file_cls.optional)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL driver has no 'file optional' method")

    va_start (arguments, req);
    if((ret_value = (cls->file_cls.optional)(file, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "optional failed")
    va_end (arguments);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_file_optional() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_file_close
 *
 * Purpose:     Closes a file through the VOL
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_file_close(void *file, const H5VL_class_t *cls, hid_t dxpl_id, void **req)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    HDassert(file);
    HDassert(cls);

    /* Check if the corresponding VOL callback exists */
    if (NULL == cls->file_cls.close)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL driver has no 'file close' method");

    /* Call the corresponding VOL callback */
    if ((ret_value = (cls->file_cls.close)(file, dxpl_id, req)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTCLOSEFILE, FAIL, "close failed");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_file_close() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_group_create
 *
 * Purpose:	Creates a group through the VOL
 *
 * Return:      Success: pointer to new group.
 *
 *		Failure: NULL
 *
 *-------------------------------------------------------------------------
 */
void *
H5VL_group_create(void *obj, H5VL_loc_params_t loc_params, const H5VL_class_t *cls, const char *name, 
                  hid_t gcpl_id, hid_t gapl_id, hid_t dxpl_id, void **req)
{
    void *ret_value = NULL; /* Return value */

    FUNC_ENTER_NOAPI(NULL)

    /* check if the corresponding VOL create callback exists */
    if(NULL == cls->group_cls.create)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, NULL, "VOL driver has no 'group create' method")

    /* call the corresponding VOL create callback */
    if(NULL == (ret_value = (cls->group_cls.create)
                (obj, loc_params, name, gcpl_id, gapl_id, dxpl_id, req)))
        HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, NULL, "create failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_group_create() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_group_open
 *
 * Purpose:	Opens a group through the VOL
 *
 * Return:      Success: pointer to new group.
 *
 *		Failure: NULL
 *
 *-------------------------------------------------------------------------
 */
void *
H5VL_group_open(void *obj, H5VL_loc_params_t loc_params, const H5VL_class_t *cls, const char *name, 
                hid_t gapl_id, hid_t dxpl_id, void **req)
{
    void *ret_value; /* Return value */

    FUNC_ENTER_NOAPI(NULL)

    if(NULL == cls->group_cls.open)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, NULL, "VOL driver has no 'group open' method")

    if(NULL == (ret_value = (cls->group_cls.open)
                (obj, loc_params, name, gapl_id, dxpl_id, req)))
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPENOBJ, NULL, "open failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_group_open() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_group_get
 *
 * Purpose:	Get specific information about the group through the VOL
 *
 * Return:	Success:        non negative
 *
 *		Failure:	negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_group_get(void *obj, const H5VL_class_t *cls, H5VL_group_get_t get_type, 
               hid_t dxpl_id, void **req, ...)
{
    va_list           arguments;             /* argument list passed from the API call */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if(NULL == cls->group_cls.get)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL driver has no 'group get' method")

    va_start (arguments, req);
    if((ret_value = (cls->group_cls.get)
        (obj, get_type, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "get failed")
    va_end (arguments);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_group_get() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_group_specific
 *
 * Purpose:	specific operation on groups through the VOL
 *
 * Return:	Success:        non negative
 *
 *		Failure:	negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_group_specific(void *obj, const H5VL_class_t *cls, H5VL_group_specific_t specific_type, 
                       hid_t dxpl_id, void **req, ...)
{
    va_list           arguments;             /* argument list passed from the API call */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if(NULL == cls->group_cls.specific)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL driver has no 'group specific' method")

    va_start (arguments, req);
    if((ret_value = (cls->group_cls.specific)
        (obj, specific_type, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "Unable to execute group specific callback")
    va_end (arguments);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_group_specific() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_group_optional
 *
 * Purpose:	optional operation specific to drivers.
 *
 * Return:	Success:        non negative
 *
 *		Failure:	negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_group_optional(void *obj, const H5VL_class_t *cls, hid_t dxpl_id, void **req, ...)
{
    va_list           arguments;             /* argument list passed from the API call */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if(NULL == cls->group_cls.optional)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL driver has no 'group optional' method")

    va_start (arguments, req);
    if((ret_value = (cls->group_cls.optional)(obj, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "Unable to execute group optional callback")
    va_end (arguments);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_group_optional() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_group_close
 *
 * Purpose:     Closes a group through the VOL
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_group_close(void *grp, const H5VL_class_t *cls, hid_t dxpl_id, void **req)
{
    herr_t ret_value = SUCCEED;

    HDassert(grp);
    HDassert(cls);

    FUNC_ENTER_NOAPI(FAIL)

    /* Check if the corresponding VOL callback exists */
    if (NULL == cls->group_cls.close)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL driver has no 'group close' method");

    /* Call the corresponding VOL callback */
    if ((ret_value = (cls->group_cls.close)(grp, dxpl_id, req)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTRELEASE, FAIL, "close failed");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_group_close() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_link_create
 *
 * Purpose:	Creates a hard link through the VOL
 *
 * Return:	Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_link_create(H5VL_link_create_type_t create_type, void *obj, H5VL_loc_params_t loc_params, 
                 const H5VL_class_t *cls, hid_t lcpl_id, hid_t lapl_id, hid_t dxpl_id, void **req)
{
    herr_t               ret_value = SUCCEED;  /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* check if the corresponding VOL create callback exists */
    if(NULL == cls->link_cls.create)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL driver has no 'link create' method")
    /* call the corresponding VOL create callback */
    if((ret_value = (cls->link_cls.create)
        (create_type, obj, loc_params, lcpl_id, lapl_id, dxpl_id, req)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "link create failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_link_create() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_link_copy
 *
 * Purpose:	Copys a link from src to dst.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t 
H5VL_link_copy(void *src_obj, H5VL_loc_params_t loc_params1, void *dst_obj,
               H5VL_loc_params_t loc_params2, const H5VL_class_t *cls,  
               hid_t lcpl_id, hid_t lapl_id, hid_t dxpl_id, void **req)
{
    herr_t               ret_value = SUCCEED;  /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* check if the corresponding VOL copy callback exists */
    if(NULL == cls->link_cls.copy)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL driver has no 'link copy' method")

    /* call the corresponding VOL copy callback */
    if((ret_value = (cls->link_cls.copy)
        (src_obj, loc_params1, dst_obj, loc_params2, lcpl_id, 
         lapl_id, dxpl_id, req)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "link copy failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_link_copy() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_link_move
 *
 * Purpose:	Moves a link from src to dst.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t 
H5VL_link_move(void *src_obj, H5VL_loc_params_t loc_params1, void *dst_obj,
               H5VL_loc_params_t loc_params2, const H5VL_class_t *cls,  
               hid_t lcpl_id, hid_t lapl_id, hid_t dxpl_id, void **req)
{
    herr_t               ret_value = SUCCEED;  /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* check if the corresponding VOL move callback exists */
    if(NULL == cls->link_cls.move)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL driver has no 'link move' method")

    /* call the corresponding VOL move callback */
    if((ret_value = (cls->link_cls.move)
        (src_obj, loc_params1, dst_obj, loc_params2, lcpl_id, 
         lapl_id, dxpl_id, req)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "link move failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_link_move() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_link_get
 *
 * Purpose:	Get specific information about the link through the VOL
 *
 * Return:	Success:        non negative
 *
 *		Failure:	negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_link_get(void *obj, H5VL_loc_params_t loc_params, const H5VL_class_t *cls, H5VL_link_get_t get_type, 
              hid_t dxpl_id, void **req, ...)
{
    va_list           arguments;             /* argument list passed from the API call */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if(NULL == cls->link_cls.get)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL driver has no 'link get' method")

    va_start (arguments, req);
    if((ret_value = (cls->link_cls.get)
        (obj, loc_params, get_type, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "get failed")
    va_end (arguments);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_link_get() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_link_specific
 *
 * Purpose:	specific operation on links through the VOL
 *
 * Return:	Success:        non negative
 *
 *		Failure:	negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_link_specific(void *obj, H5VL_loc_params_t loc_params, const H5VL_class_t *cls, 
                   H5VL_link_specific_t specific_type, hid_t dxpl_id, void **req, ...)
{
    va_list           arguments;             /* argument list passed from the API call */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if(NULL == cls->link_cls.specific)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL driver has no 'link specific' method")

    va_start (arguments, req);
    if((ret_value = (cls->link_cls.specific)
        (obj, loc_params, specific_type, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "Unable to execute link specific callback")
    va_end (arguments);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_link_specific() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_link_optional
 *
 * Purpose:	optional operation specific to drivers.
 *
 * Return:	Success:        non negative
 *
 *		Failure:	negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_link_optional(void *obj, const H5VL_class_t *cls, hid_t dxpl_id, void **req, ...)
{
    va_list           arguments;             /* argument list passed from the API call */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if(NULL == cls->link_cls.optional)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL driver has no 'link optional' method")

    va_start (arguments, req);
    if((ret_value = (cls->link_cls.optional)(obj, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "Unable to execute link optional callback")
    va_end (arguments);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_link_optional() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_object_open
 *
 * Purpose:	Opens a object through the VOL
 *
 * Return:      Success: User ID of the new object. 
 *
 *		Failure: NULL
 *
 *-------------------------------------------------------------------------
 */
void *
H5VL_object_open(void *obj, H5VL_loc_params_t params, const H5VL_class_t *cls, H5I_type_t *opened_type,
                 hid_t dxpl_id, void **req)
{
    void *ret_value;              /* Return value */

    FUNC_ENTER_NOAPI(NULL)

    /* check if the corresponding VOL open callback exists */
    if(NULL == cls->object_cls.open)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, NULL, "VOL driver has no 'object open' method")

    /* call the corresponding VOL open callback */
    if(NULL == (ret_value = (cls->object_cls.open)
                (obj, params, opened_type, dxpl_id, req)))
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPENOBJ, NULL, "open failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_object_open() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_object_copy
 *
 * Purpose:	Copies an object to another destination through the VOL
 *
 * Return:	Success:	Non Negative
 *		Failure:	Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t 
H5VL_object_copy(void *src_obj, H5VL_loc_params_t loc_params1, const H5VL_class_t *cls1, const char *src_name, 
                 void *dst_obj, H5VL_loc_params_t loc_params2, const H5VL_class_t *cls2, const char *dst_name, 
                 hid_t ocpypl_id, hid_t lcpl_id, hid_t dxpl_id, void **req)
{
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    /* Make sure that the VOL drivers are the same */
    if (cls1->value != cls2->value)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "Objects are accessed through different VOL drivers and can't be linked")

    if(NULL == cls1->object_cls.copy)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL driver has no 'object copy' method")

    if((ret_value = (cls1->object_cls.copy)
        (src_obj, loc_params1, src_name, dst_obj, loc_params2, dst_name, ocpypl_id, 
         lcpl_id, dxpl_id, req)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTRELEASE, FAIL, "copy failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_object_copy() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_object_get
 *
 * Purpose:	Get specific information about the object through the VOL
 *
 * Return:	Success:        non negative
 *
 *		Failure:	negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_object_get(void *obj, H5VL_loc_params_t loc_params, const H5VL_class_t *cls, H5VL_object_get_t get_type, 
                hid_t dxpl_id, void **req, ...)
{
    va_list           arguments;             /* argument list passed from the API call */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if(NULL == cls->object_cls.get)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL driver has no 'object get' method")

    va_start (arguments, req);
    if((ret_value = (cls->object_cls.get)
        (obj, loc_params, get_type, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "get failed")
    va_end (arguments);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_object_get() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_object_specific
 *
 * Purpose:	specific operation on objects through the VOL
 *
 * Return:	Success:        non negative
 *		Failure:	negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_object_specific(void *obj, H5VL_loc_params_t loc_params, const H5VL_class_t *cls, 
                     H5VL_object_specific_t specific_type, hid_t dxpl_id, void **req, ...)
{
    va_list           arguments;             /* argument list passed from the API call */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if(NULL == cls->object_cls.specific)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL driver has no 'object specific' method")

    va_start (arguments, req);
    if((ret_value = (cls->object_cls.specific)
        (obj, loc_params, specific_type, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "specific failed")
    va_end (arguments);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_object_specific() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_object_optional
 *
 * Purpose:	optional operation specific to drivers.
 *
 * Return:	Success:        non negative
 *
 *		Failure:	negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_object_optional(void *obj, const H5VL_class_t *cls, hid_t dxpl_id, void **req, ...)
{
    va_list           arguments;             /* argument list passed from the API call */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if(NULL == cls->object_cls.optional)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL driver has no 'object optional' method")

    va_start (arguments, req);
    if((ret_value = (cls->object_cls.optional)(obj, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "Unable to execute object optional callback")
    va_end (arguments);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_object_optional() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_datatype_commit
 *
 * Purpose:	Commits a datatype to the file through the VOL
 *
 * Return:      Success: Positive
 *
 *		Failure: Negative
 *
 *-------------------------------------------------------------------------
 */
void *
H5VL_datatype_commit(void *obj, H5VL_loc_params_t loc_params, const H5VL_class_t *cls, const char *name, 
                     hid_t type_id, hid_t lcpl_id, hid_t tcpl_id, hid_t tapl_id, 
                     hid_t dxpl_id, void **req)
{
    void *ret_value = NULL;              /* Return value */

    FUNC_ENTER_NOAPI(NULL)

    /* check if the corresponding VOL commit callback exists */
    if(NULL == cls->datatype_cls.commit)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, NULL, "VOL driver has no 'datatype commit' method")

    /* call the corresponding VOL commit callback */
    if(NULL == (ret_value = (cls->datatype_cls.commit) 
        (obj, loc_params, name, type_id, lcpl_id, tcpl_id, tapl_id, dxpl_id, req)))
        HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, NULL, "commit failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_datatype_commit() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_datatype_open
 *
 * Purpose:	Opens a named datatype through the VOL
 *
 * Return:      Success: User ID of the datatype. 
 *
 *		Failure: NULL
 *
 *-------------------------------------------------------------------------
 */
void *
H5VL_datatype_open(void *obj, H5VL_loc_params_t loc_params, const H5VL_class_t *cls, const char *name, 
                   hid_t tapl_id, hid_t dxpl_id, void **req)
{
    void *ret_value = NULL;              /* Return value */

    FUNC_ENTER_NOAPI(NULL)

    /* check if the type specific corresponding VOL open callback exists */
    if(NULL == cls->datatype_cls.open)
        HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, NULL, "no datatype open callback");

    /* call the corresponding VOL open callback */
    if(NULL == (ret_value = (cls->datatype_cls.open)
                (obj, loc_params, name, tapl_id, dxpl_id, req)))
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPENOBJ, NULL, "open failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_datatype_open() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_datatype_get
 *
 * Purpose:     Get specific information about the datatype through the VOL
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_datatype_get(void *obj, const H5VL_class_t *cls, H5VL_datatype_get_t get_type, 
                  hid_t dxpl_id, void **req, ...)
{
    va_list     arguments;             /* argument list passed from the API call */
    herr_t      ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    /* Check if the corresponding VOL callback exists */
    if (NULL == cls->datatype_cls.get)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL driver has no 'datatype get' method");
    va_start (arguments, req);

    /* Call the corresponding VOL callback */
    if ((ret_value = (cls->datatype_cls.get)(obj, get_type, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "get failed");
    va_end (arguments);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_datatype_get() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_datatype_specific
 *
 * Purpose:	specific operation on datatypes through the VOL
 *
 * Return:	Success:        non negative
 *
 *		Failure:	negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_datatype_specific(void *obj, const H5VL_class_t *cls, H5VL_datatype_specific_t specific_type, 
                       hid_t dxpl_id, void **req, ...)
{
    va_list           arguments;             /* argument list passed from the API call */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if(NULL == cls->datatype_cls.specific)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL driver has no 'datatype specific' method")

    va_start (arguments, req);
    if((ret_value = (cls->datatype_cls.specific)
        (obj, specific_type, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "Unable to execute datatype specific callback")
    va_end (arguments);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_datatype_specific() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_datatype_optional
 *
 * Purpose:	optional operation specific to drivers.
 *
 * Return:	Success:        non negative
 *
 *		Failure:	negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_datatype_optional(void *obj, const H5VL_class_t *cls, hid_t dxpl_id, void **req, ...)
{
    va_list           arguments;             /* argument list passed from the API call */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if(NULL == cls->datatype_cls.optional)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL driver has no 'datatype optional' method")

    va_start (arguments, req);
    if((ret_value = (cls->datatype_cls.optional)(obj, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "Unable to execute datatype optional callback")
    va_end (arguments);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_datatype_optional() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_datatype_close
 *
 * Purpose:     Closes a datatype through the VOL
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_datatype_close(void *dt, const H5VL_class_t *cls, hid_t dxpl_id, void **req)
{
    herr_t ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Check if the corresponding VOL callback exists */
    if (NULL == cls->datatype_cls.close)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL driver has no 'datatype close' method");

    /* Call the corresponding VOL callback */
    if ((ret_value = (cls->datatype_cls.close)(dt, dxpl_id, req)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTRELEASE, FAIL, "close failed");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_datatype_close() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_request_cancel
 *
 * Purpose:     Cancels an asynchronous request through the VOL
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_request_cancel(void **req, const H5VL_class_t *cls, H5ES_status_t *status)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    HDassert(req);
    HDassert(cls);
    HDassert(status);

    /* Check if the corresponding VOL callback exists */
    if (NULL == cls->async_cls.cancel)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL driver has no 'async cancel' method");

    /* Call the corresponding VOL callback */
    if ((ret_value = (cls->async_cls.cancel)(req, status)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTRELEASE, FAIL, "request cancel failed");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_request_cancel() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_request_test
 *
 * Purpose:     Tests an asynchronous request through the VOL
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_request_test(void **req, const H5VL_class_t *cls, H5ES_status_t *status)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    HDassert(req);
    HDassert(cls);
    HDassert(status);

    /* Check if the corresponding VOL callback exists */
    if (NULL == cls->async_cls.test)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL driver has no 'async test' method");

    /* Call the corresponding VOL callback */
    if ((ret_value = (cls->async_cls.test)(req, status)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTRELEASE, FAIL, "request test failed");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_request_test() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_request_wait
 *
 * Purpose:     Waits on an asychronous request through the VOL
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_request_wait(void **req, const H5VL_class_t *cls, H5ES_status_t *status)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    HDassert(req);
    HDassert(cls);
    HDassert(status);

    /* Check if the corresponding VOL callback exists */
    if (NULL == cls->async_cls.wait)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL driver has no 'async wait' method");

    /* Call the corresponding VOL callback */
    if ((ret_value = (cls->async_cls.wait)(req, status)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTRELEASE, FAIL, "request wait failed");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_request_wait() */

