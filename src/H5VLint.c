/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
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

    /* XXX: We'll leak memory if the name string was dynamically allocated. */
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
    H5VL_object_t *new_obj = NULL;
    H5T_t *dt = NULL;
    hid_t ret_value = H5I_INVALID_HID;

    FUNC_ENTER_NOAPI(H5I_INVALID_HID)

    /* Check arguments */
    HDassert(object);
    HDassert(vol_driver);

    /* setup VOL object */
    if (NULL == (new_obj = H5FL_CALLOC(H5VL_object_t)))
        HGOTO_ERROR(H5E_VOL, H5E_NOSPACE, H5I_INVALID_HID, "can't allocate top object structure");
    new_obj->vol_info = vol_driver;
    vol_driver->nrefs ++;
    new_obj->vol_obj = object;

    if (H5I_DATATYPE == type) {
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
 * Purpose:     Wrapper to register an object ID with a VOL aux struct
 *              and increment ref count on VOL driver ID
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_free_object(H5VL_object_t *obj)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(SUCCEED)

    /* Check arguments */
    HDassert(obj);

    obj->vol_info->nrefs --;
    if (0 == obj->vol_info->nrefs) {
        if (H5I_dec_ref(obj->vol_info->vol_id) < 0)
            HGOTO_ERROR(H5E_VOL, H5E_CANTDEC, FAIL, "unable to decrement ref count on VOL driver");
        obj->vol_info = H5FL_FREE(H5VL_t, obj->vol_info);
    }

    obj = H5FL_FREE(H5VL_object_t, obj);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_free_object() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_register
 *
 * Purpose:     Registers a new vol driver as a member of the virtual object
 *              layer class.
 *
 * Return:      Success:    A vol driver ID which is good until the
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
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, H5I_INVALID_HID, "memory allocation failed for vol driver class struct");
    HDmemcpy(saved, cls, size);

    /* Create the new class ID */
    if ((ret_value = H5I_register(H5I_VOL, saved, app_ref)) < 0)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, H5I_INVALID_HID, "unable to register vol driver ID");

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
    H5VL_class_t    *vol_cls = NULL;
    H5VL_t          *vol_info = NULL;       /* VOL info struct */
    hid_t           ret_value = H5I_INVALID_HID;

    FUNC_ENTER_NOAPI(FAIL)

    if (NULL == (vol_cls = (H5VL_class_t *)H5I_object_verify(driver_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, H5I_INVALID_HID, "not a VOL driver ID");

    /* setup VOL info struct */
    if (NULL == (vol_info = H5FL_CALLOC(H5VL_t)))
        HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, H5I_INVALID_HID, "can't allocate VL info struct");
    vol_info->vol_cls = vol_cls;
    vol_info->vol_id = driver_id;
    if (H5I_inc_ref(vol_info->vol_id, FALSE) < 0)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTINC, H5I_INVALID_HID, "unable to increment ref count on VOL driver");

    /* Get an atom for the object */
    if ((ret_value = H5VL_register_id(obj_type, obj, vol_info, app_ref)) < 0)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, H5I_INVALID_HID, "unable to atomize object handle");

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
    H5VL_object_t       *obj = NULL;
    const H5VL_class_t  *vol_cls = NULL;
    size_t              len;
    ssize_t             ret_value = -1;

    FUNC_ENTER_NOAPI(FAIL)

    /* get the object pointer */
    if (NULL == (obj = H5VL_get_object(id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid file identifier");

    vol_cls = obj->vol_info->vol_cls;

    len = HDstrlen(vol_cls->name);
    if (name) {
        HDstrncpy(name, vol_cls->name, MIN(len + 1,size));
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
    H5VL_object_t   *obj = NULL;    
    void            *ret_value = NULL;

    FUNC_ENTER_NOAPI(NULL)

    /* Get the symbol table entry */
    switch (H5I_get_type(id)) {
        case H5I_GROUP:
        case H5I_DATASET:
        case H5I_FILE:            
        case H5I_ATTR:
            /* get the object */
            if (NULL == (obj = (H5VL_object_t *)H5I_object(id)))
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "invalid identifier");

            ret_value = obj->vol_obj;
            break;
        case H5I_DATATYPE:
            {
                H5T_t *dt = NULL;

                /* get the object */
                if (NULL == (dt = (H5T_t *)H5I_object(id)))
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "invalid identifier");

                /* Get the actual datatype object that should be the vol_obj */
                if (NULL == (obj = H5T_get_named_type(dt)))
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a named datatype");

                ret_value = obj->vol_obj;
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
 * Purpose:     Utility function to return the VOL object pointer associated with
 *              a hid_t.
 *
 * Return:      Success:    object pointer
 *              Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
void *
H5VL_object_verify(hid_t id, H5I_type_t obj_type)
{
    H5VL_object_t   *obj = NULL;    
    void            *ret_value = NULL;

    FUNC_ENTER_NOAPI(NULL)

    /* Get the symbol table entry */
    switch (obj_type) {
        case H5I_GROUP:
        case H5I_DATASET:
        case H5I_FILE:            
        case H5I_ATTR:
            /* get the object */
            if (NULL == (obj = (H5VL_object_t *)H5I_object_verify(id, obj_type)))
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "invalid identifier");

            ret_value = obj->vol_obj;
            break;
        case H5I_DATATYPE:
            {
                H5T_t *dt = NULL;

                /* get the object */
                if (NULL == (dt = (H5T_t *)H5I_object_verify(id, obj_type)))
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "invalid identifier");

                /* Get the actual datatype object that should be the vol_obj */
                if (NULL == (obj = H5T_get_named_type(dt)))
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a named datatype");

                ret_value = obj->vol_obj;
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


/* XXX: This could be a macro like in H5F */
void *
H5VL_driver_object(H5VL_object_t *obj)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    FUNC_LEAVE_NOAPI(obj->vol_obj)
} /* end H5VL_driver_object() */


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
H5VL_attr_close(void *attr, const H5VL_class_t *vol_cls, hid_t dxpl_id, void **req)
{
    herr_t ret_value = SUCCEED;

    HDassert(attr);
    HDassert(vol_cls);

    FUNC_ENTER_NOAPI(FAIL)
            
    /* Check if the corresponding VOL callback exists */
    if (NULL == vol_cls->attr_cls.close)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL driver has no 'attr close' method");

    /* Call the corresponding VOL callback */
    if ((ret_value = (vol_cls->attr_cls.close)(attr, dxpl_id, req)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTRELEASE, FAIL, "close failed");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_attr_close() */


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
H5VL_dataset_close(void *dset, const H5VL_class_t *vol_cls, hid_t dxpl_id, void **req)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    HDassert(dset);
    HDassert(vol_cls);

    /* Check if the corresponding VOL callback exists */
    if (NULL == vol_cls->dataset_cls.close)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL driver has no 'dset close' method");

    /* Call the corresponding VOL callback */
    if ((ret_value = (vol_cls->dataset_cls.close)(dset, dxpl_id, req)) < 0)
            HGOTO_ERROR(H5E_VOL, H5E_CANTRELEASE, FAIL, "close failed");

done:

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_dataset_close() */


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
H5VL_file_close(void *file, const H5VL_class_t *vol_cls, hid_t dxpl_id, void **req)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    HDassert(file);
    HDassert(vol_cls);

    /* Check if the corresponding VOL callback exists */
    if (NULL == vol_cls->file_cls.close)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL driver has no 'file close' method");

    /* Call the corresponding VOL callback */
    if ((ret_value = (vol_cls->file_cls.close)(file, dxpl_id, req)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTCLOSEFILE, FAIL, "close failed");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_file_close() */


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
H5VL_group_close(void *grp, const H5VL_class_t *vol_cls, hid_t dxpl_id, void **req)
{
    herr_t ret_value = SUCCEED;

    HDassert(grp);
    HDassert(vol_cls);

    FUNC_ENTER_NOAPI(FAIL)

    /* Check if the corresponding VOL callback exists */
    if (NULL == vol_cls->group_cls.close)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL driver has no 'group close' method");

    /* Call the corresponding VOL callback */
    if ((ret_value = (vol_cls->group_cls.close)(grp, dxpl_id, req)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTRELEASE, FAIL, "close failed");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_group_close() */


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
H5VL_datatype_get(void *obj, const H5VL_class_t *vol_cls, H5VL_datatype_get_t get_type, 
                  hid_t dxpl_id, void **req, ...)
{
    va_list     arguments;             /* argument list passed from the API call */
    herr_t      ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    /* Check if the corresponding VOL callback exists */
    if (NULL == vol_cls->datatype_cls.get)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL driver has no 'datatype get' method");
    va_start (arguments, req);

    /* Call the corresponding VOL callback */
    if ((ret_value = (vol_cls->datatype_cls.get)(obj, get_type, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "get failed");
    va_end (arguments);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_datatype_get() */


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
H5VL_datatype_close(void *dt, const H5VL_class_t *vol_cls, hid_t dxpl_id, void **req)
{
    herr_t ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Check if the corresponding VOL callback exists */
    if (NULL == vol_cls->datatype_cls.close)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "VOL driver has no 'datatype close' method");

    /* Call the corresponding VOL callback */
    if ((ret_value = (vol_cls->datatype_cls.close)(dt, dxpl_id, req)) < 0)
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
H5VL_request_cancel(void **req, const H5VL_class_t *vol_cls, H5ES_status_t *status)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    HDassert(req);
    HDassert(vol_cls);
    HDassert(status);

    /* Check if the corresponding VOL callback exists */
    if (NULL == vol_cls->async_cls.cancel)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol driver has no 'async cancel' method");

    /* Call the corresponding VOL callback */
    if ((ret_value = (vol_cls->async_cls.cancel)(req, status)) < 0)
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
H5VL_request_test(void **req, const H5VL_class_t *vol_cls, H5ES_status_t *status)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    HDassert(req);
    HDassert(vol_cls);
    HDassert(status);

    /* Check if the corresponding VOL callback exists */
    if (NULL == vol_cls->async_cls.test)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol driver has no 'async test' method");

    /* Call the corresponding VOL callback */
    if ((ret_value = (vol_cls->async_cls.test)(req, status)) < 0)
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
H5VL_request_wait(void **req, const H5VL_class_t *vol_cls, H5ES_status_t *status)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    HDassert(req);
    HDassert(vol_cls);
    HDassert(status);

    /* Check if the corresponding VOL callback exists */
    if (NULL == vol_cls->async_cls.wait)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol driver has no 'async wait' method");

    /* Call the corresponding VOL callback */
    if ((ret_value = (vol_cls->async_cls.wait)(req, status)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTRELEASE, FAIL, "request wait failed");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_request_wait() */

