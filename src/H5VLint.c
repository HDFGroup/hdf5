/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Programmer:  Mohamad Chaarawi <chaarawi@hdfgroup.gov>
 *              January, 2012
 *
 * Purpose:	The Virtual Object Layer as described in documentation.
 *              The pupose is to provide an abstraction on how to access the
 *              underlying HDF5 container, whether in a local file with 
 *              a specific file format, or remotely on other machines, etc...
 */

/****************/
/* Module Setup */
/****************/

#define H5VL_PACKAGE		/*suppress error about including H5VLpkg  */
#define H5I_PACKAGE		/*suppress error about including H5Ipkg  */

/* Interface initialization */
#define H5_INTERFACE_INIT_FUNC	H5VL_int_init_interface

/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Dprivate.h"		/* Datasets				*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5Ipkg.h"		/* IDs Package header	  		*/
#include "H5Lprivate.h"		/* Links        		  	*/
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Pprivate.h"		/* Property lists			*/
#include "H5Oprivate.h"		/* Object headers		  	*/
#include "H5VLpkg.h"		/* VOL package header		  	*/
#include "H5VLprivate.h"	/* VOL          		  	*/


/*--------------------------------------------------------------------------
NAME
   H5VL_int_init_interface -- Initialize interface-specific information
USAGE
    herr_t H5VL_int_init_interface()

RETURNS
    Non-negative on success/Negative on failure
DESCRIPTION
    Initializes any interface-specific data or routines.  (Just calls
    H5VL_init_iterface currently).

--------------------------------------------------------------------------*/
static herr_t
H5VL_int_init_interface(void)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    FUNC_LEAVE_NOAPI(H5VL_init())
} /* H5VL_int_init_interface() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_register
 *
 * Purpose:	Registers a new vol plugin as a member of the virtual object
 *		layer class.
 *
 * Return:	Success:	A vol plugin ID which is good until the
 *				library is closed or the driver is
 *				unregistered.
 *
 *		Failure:	A negative value.
 *
 * Programmer:	Mohamad Chaarawi
 *              January, 2012
 *-------------------------------------------------------------------------
 */
hid_t
H5VL_register(const void *_cls, size_t size, hbool_t app_ref)
{
    const H5VL_class_t	*cls = (const H5VL_class_t *)_cls;
    H5VL_class_t	*saved = NULL;
    hid_t		ret_value;

    FUNC_ENTER_NOAPI(FAIL)

    /* Check arguments */
    HDassert(cls);

    /* Copy the class structure so the caller can reuse or free it */
    if(NULL == (saved = (H5VL_class_t *)H5MM_malloc(size)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, 
                    "memory allocation failed for vol plugin class struct")
    HDmemcpy(saved, cls, size);

    /* Create the new class ID */
    if((ret_value = H5I_register(H5I_VOL, saved, app_ref)) < 0)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to register vol plugin ID")

done:
    if(ret_value < 0)
        if(saved)
            H5MM_xfree(saved);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_register() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_register_id
 *
 * Purpose:	Wrapper to register an object ID with a VOL aux struct 
 *              and increment ref count on VOL plugin ID
 *
 * Return:	Success:	Positive Identifier
 *		Failure:	A negative value.
 *
 * Programmer:	Mohamad Chaarawi
 *              August, 2014
 *-------------------------------------------------------------------------
 */
hid_t
H5VL_register_id(H5I_type_t type, const void *object, H5VL_t *vol_plugin, hbool_t app_ref)
{
    hid_t ret_value = FAIL;

    FUNC_ENTER_NOAPI(FAIL)

    /* Check arguments */
    HDassert(object);
    HDassert(vol_plugin);

    if((ret_value = H5I_register2(type, object, vol_plugin, app_ref)) < 0)
	HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to atomize handle")

    vol_plugin->nrefs ++;
    if(H5I_inc_ref(vol_plugin->id, FALSE) < 0)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTINC, FAIL, "unable to increment ref count on VOL plugin")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_register_id() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_free_id
 *
 * Purpose:	Wrapper to register an object ID with a VOL aux struct 
 *              and increment ref count on VOL plugin ID
 *
 * Return:	Success:	Positive Identifier
 *		Failure:	A negative value.
 *
 * Programmer:	Mohamad Chaarawi
 *              August, 2014
 *-------------------------------------------------------------------------
 */
hid_t
H5VL_free_id(H5VL_t *vol_plugin)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(SUCCEED)

    /* Check arguments */
    HDassert(vol_plugin);

    vol_plugin->nrefs --;
    if(H5I_dec_ref(vol_plugin->id) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTDEC, FAIL, "unable to decrement ref count on VOL plugin")
    if (0 == vol_plugin->nrefs)
        vol_plugin = (H5VL_t *)H5MM_xfree(vol_plugin);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_free_id() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_fapl_open
 *
 * Purpose:	Mark a vol as used by a file access property list
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Mohamad Chaarawi
 *              January, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_fapl_open(H5P_genplist_t *plist, hid_t vol_id, const void *vol_info)
{
    void  *copied_vol_info = NULL;     /* Temporary VOL driver info */
    herr_t ret_value = SUCCEED;        /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* increment the refcount on the plugin id */
    if(H5I_inc_ref(vol_id, FALSE) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINC, FAIL, "unable to increment ref count on VOL plugin")
    if(H5VL_fapl_copy(vol_id, vol_info, &copied_vol_info) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTCOPY, FAIL, "can't copy VOL plugin info")

    /* Set the vol properties for the list */
    if(H5P_set(plist, H5F_ACS_VOL_ID_NAME, &vol_id) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTSET, FAIL, "can't set vol ID")
    if(H5P_set(plist, H5F_ACS_VOL_INFO_NAME, &copied_vol_info) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTSET, FAIL, "can't set vol info")
    copied_vol_info = NULL;

done:
    if(ret_value < 0)
        if(copied_vol_info && H5VL_fapl_close(vol_id, copied_vol_info) < 0)
            HDONE_ERROR(H5E_FILE, H5E_CANTCLOSEOBJ, FAIL, "can't close copy of driver info")
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_fapl_open() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_fapl_copy
 *
 * Purpose:	copies plugin specific info to the fapl
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Mohamad Chaarawi
 *              July, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_fapl_copy(hid_t vol_id, const void *vol_info, void **copied_info)
{
    H5VL_class_t *vol_cls;
    void *new_info = NULL;
    herr_t ret_value = SUCCEED;        /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (vol_cls = (H5VL_class_t *)H5I_object(vol_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL ID")

    /* Copy old pl, if one exists */
    if(vol_info) {
        /* Allow the driver to copy or do it ourselves */
        if(vol_cls->fapl_copy) {
            new_info = (vol_cls->fapl_copy)(vol_info);
            if(NULL == new_info)
                HGOTO_ERROR(H5E_VOL, H5E_NOSPACE, FAIL, "property list copy failed")
        } 
        else if(vol_cls->fapl_size > 0) {
            if(NULL == (new_info = H5MM_malloc(vol_cls->fapl_size)))
                HGOTO_ERROR(H5E_VOL, H5E_NOSPACE, FAIL, "property list allocation failed")
            HDmemcpy(new_info, vol_info, vol_cls->fapl_size);
        } else
            HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "no way to copy plugin property list")
    } /* end if */

    /* Set copied value */
    *copied_info = new_info;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_fapl_copy() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_fapl_close
 *
 * Purpose:	Closes a vol for a property list
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Mohamad Chaarawi
 *              January, 2012
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_fapl_close(hid_t vol_id, void *vol_info)
{
    H5VL_class_t *vol_cls;
    herr_t       ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    if(vol_id > 0) {
        if(NULL == (vol_cls = (H5VL_class_t *)H5I_object(vol_id)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL ID")

        /* Allow driver to free or do it ourselves */
        if(vol_info && vol_cls->fapl_free) {
            if((vol_cls->fapl_free)(vol_info) < 0)
                HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "plugin free request failed")
        } /* end if */
        else
            H5MM_xfree(vol_info);

        /* Decrement reference count for plugin */
        if(H5I_dec_ref(vol_id) < 0)
            HGOTO_ERROR(H5E_VOL, H5E_CANTDEC, FAIL, "can't decrement reference count for plugin")
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_fapl_close() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_get_plugin_name
 *
 * Purpose:	Private version of H5VLget_plugin_name
 *
 * Return:      Success:        The length of the plugin name
 *              Failure:        Negative
 *
 * Programmer:	Mohamad Chaarawi
 *              June, 2012
 *
 *-------------------------------------------------------------------------
 */
ssize_t
H5VL_get_plugin_name(hid_t id, char *name/*out*/, size_t size)
{
    H5VL_t       *vol_plugin;            /* VOL structure attached to id */
    size_t        len;
    ssize_t       ret_value;

    FUNC_ENTER_NOAPI(FAIL)

    if (NULL == (vol_plugin = (H5VL_t *)H5I_get_aux (id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "Object/File does not contain VOL information")

    len = HDstrlen(vol_plugin->cls->name);

    if(name) {
        HDstrncpy(name, vol_plugin->cls->name, MIN(len + 1,size));
        if(len >= size)
            name[size-1]='\0';
    } /* end if */

    /* Set the return value for the API call */
    ret_value = (ssize_t)len;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_get_plugin_name() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_object_register
 *
 * Purpose:     utility function to create a user id for an object created
 *              or opened through the VOL
 *
 * Return:      Success:        registered ID
 *              Failure:        FAIL
 *
 * Programmer:	Mohamad Chaarawi
 *              June, 2012
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5VL_object_register(void *obj, H5I_type_t obj_type, H5VL_t *vol_plugin, hbool_t app_ref)
{
    hid_t ret_value = FAIL;

    FUNC_ENTER_NOAPI(FAIL)

    /* Get an atom for the object and attach VOL information and free function to the ID */
    switch(obj_type) {
        case H5I_FILE:
            if((ret_value = H5VL_register_id(obj_type, obj, vol_plugin, app_ref)) < 0)
                HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to atomize file handle")
            break;

        case H5I_ATTR:
            if((ret_value = H5VL_register_id(obj_type, obj, vol_plugin, app_ref)) < 0)
                HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to atomize attribute handle")
            break;

        case H5I_GROUP:
            if((ret_value = H5VL_register_id(obj_type, obj, vol_plugin, app_ref)) < 0)
                HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to atomize group handle")
            break;

        case H5I_DATASET:
            if((ret_value = H5VL_register_id(obj_type, obj, vol_plugin, app_ref)) < 0)
                HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to atomize dataset handle")
            break;

        case H5I_DATATYPE:
            if ((ret_value = H5VL_create_datatype(obj, vol_plugin, app_ref)) < 0)
                HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to atomize datatype handle")
            break;

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
            HGOTO_ERROR(H5E_OHDR, H5E_BADTYPE, FAIL, "invalid object type")
    } /* end switch */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_object_register() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_get_object
 *
 * Purpose:     utility function to return the object pointer associated with
 *              an hid_t. This routine is the same as H5I_object for all types
 *              except for named datatypes, where the vol_obj is returned that 
 *              is attached to the H5T_t struct.
 *
 * Return:      Success:        object pointer
 *              Failure:        NULL
 *
 * Programmer:	Mohamad Chaarawi
 *              June, 2012
 *
 *-------------------------------------------------------------------------
 */
void *
H5VL_get_object(hid_t id)
{
    void *ret_value = NULL;

    FUNC_ENTER_NOAPI(NULL)

    /* get the object */
    if(NULL == (ret_value = (void *)H5I_object(id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "invalid identifier")

    /* if this is a datatype, get the VOL object attached to the H5T_t struct */
    if (H5I_DATATYPE == H5I_get_type(id)) {
        if (NULL == (ret_value = H5T_get_named_type((H5T_t *)ret_value)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a named datatype")
    }
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_get_object() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_attr_create
 *
 * Purpose:	Creates an attribute through the VOL
 *
 * Return:      Success: pointer to the new attr. 
 *
 *		Failure: NULL
 *
 * Programmer:	Mohamad Chaarawi
 *              April, 2012
 *
 *-------------------------------------------------------------------------
 */
void *
H5VL_attr_create(void *obj, H5VL_loc_params_t loc_params, const H5VL_class_t *vol_cls, const char *name, 
                 hid_t acpl_id, hid_t aapl_id, hid_t dxpl_id, void **req)
{
    void *ret_value;  /* Return value */

    FUNC_ENTER_NOAPI(NULL)

    /* check if the corresponding VOL create callback exists */
    if(NULL == vol_cls->attr_cls.create)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, NULL, "vol plugin has no `attr create' method")

    /* call the corresponding VOL create callback */
    if(NULL == (ret_value = (vol_cls->attr_cls.create) 
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
 * Programmer:	Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
void *
H5VL_attr_open(void *obj, H5VL_loc_params_t loc_params, const H5VL_class_t *vol_cls, const char *name, 
               hid_t aapl_id, hid_t dxpl_id, void **req)
{
    void *ret_value;  /* Return value */

    FUNC_ENTER_NOAPI(NULL)

    /* check if the type specific corresponding VOL open callback exists */
    if(NULL == vol_cls->attr_cls.open)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, NULL, "vol plugin has no `attr open' method")

    /* call the corresponding VOL open callback */
    if(NULL == (ret_value = (vol_cls->attr_cls.open) 
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
 * Programmer:	Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t H5VL_attr_read(void *attr, const H5VL_class_t *vol_cls, hid_t mem_type_id, void *buf, 
                      hid_t dxpl_id, void **req)
{
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if(NULL == vol_cls->attr_cls.read)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `attr read' method")
    if((ret_value = (vol_cls->attr_cls.read)(attr, mem_type_id, buf, dxpl_id, req)) < 0)
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
 * Programmer:	Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t H5VL_attr_write(void *attr, const H5VL_class_t *vol_cls, hid_t mem_type_id, const void *buf, 
                       hid_t dxpl_id, void **req)
{
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if(NULL == vol_cls->attr_cls.write)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `attr write' method")
    if((ret_value = (vol_cls->attr_cls.write)(attr, mem_type_id, buf, dxpl_id, req)) < 0)
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
 * Programmer:	Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_attr_get(void *obj, const H5VL_class_t *vol_cls, H5VL_attr_get_t get_type, 
              hid_t dxpl_id, void **req, ...)
{
    va_list           arguments;             /* argument list passed from the API call */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if(NULL == vol_cls->attr_cls.get)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `attr get' method")

    va_start (arguments, req);
    if((ret_value = (vol_cls->attr_cls.get)
        (obj, get_type, dxpl_id, req, arguments)) < 0)
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
 * Programmer:	Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_attr_specific(void *obj, H5VL_loc_params_t loc_params, const H5VL_class_t *vol_cls, 
                   H5VL_attr_specific_t specific_type, hid_t dxpl_id, void **req, ...)
{
    va_list           arguments;             /* argument list passed from the API call */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if(NULL == vol_cls->attr_cls.specific)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `attr specific' method")

    va_start (arguments, req);
    if((ret_value = (vol_cls->attr_cls.specific)
        (obj, loc_params, specific_type, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "Unable to execute attribute specific callback")
    va_end (arguments);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_attr_specific() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_attr_optional
 *
 * Purpose:	optional operation specific to plugins.
 *
 * Return:	Success:        non negative
 *
 *		Failure:	negative
 *
 * Programmer:	Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_attr_optional(void *obj, const H5VL_class_t *vol_cls, hid_t dxpl_id, void **req, ...)
{
    va_list           arguments;             /* argument list passed from the API call */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if(NULL == vol_cls->attr_cls.optional)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `attr optional' method")

    va_start (arguments, req);
    if((ret_value = (vol_cls->attr_cls.optional)(obj, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "Unable to execute attribute optional callback")
    va_end (arguments);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_attr_optional() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_attr_close
 *
 * Purpose:	Closes an attribute through the VOL
 *
 * Return:	Success:	Non Negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_attr_close(void *attr, const H5VL_class_t *vol_cls, hid_t dxpl_id, void **req)
{
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)
            
    if(NULL == vol_cls->attr_cls.close)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `attr close' method")
    if((ret_value = (vol_cls->attr_cls.close)(attr, dxpl_id, req)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTRELEASE, FAIL, "close failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_attr_close() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_datatype_commit
 *
 * Purpose:	Commits a datatype to the file through the VOL
 *
 * Return:      Success: Positive
 *
 *		Failure: Negative
 *
 * Programmer:	Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
void *
H5VL_datatype_commit(void *obj, H5VL_loc_params_t loc_params, const H5VL_class_t *vol_cls, const char *name, 
                     hid_t type_id, hid_t lcpl_id, hid_t tcpl_id, hid_t tapl_id, 
                     hid_t dxpl_id, void **req)
{
    void *ret_value = NULL;              /* Return value */

    FUNC_ENTER_NOAPI(NULL)

    /* check if the corresponding VOL commit callback exists */
    if(NULL == vol_cls->datatype_cls.commit)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, NULL, "vol plugin has no `datatype commit' method")

    /* call the corresponding VOL commit callback */
    if(NULL == (ret_value = (vol_cls->datatype_cls.commit) 
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
 * Programmer:	Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
void *
H5VL_datatype_open(void *obj, H5VL_loc_params_t loc_params, const H5VL_class_t *vol_cls, const char *name, 
                   hid_t tapl_id, hid_t dxpl_id, void **req)
{
    void *ret_value = NULL;              /* Return value */

    FUNC_ENTER_NOAPI(NULL)

    /* check if the type specific corresponding VOL open callback exists */
    if(NULL == vol_cls->datatype_cls.open)
        HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, NULL, "no datatype open callback");

    /* call the corresponding VOL open callback */
    if(NULL == (ret_value = (vol_cls->datatype_cls.open)
                (obj, loc_params, name, tapl_id, dxpl_id, req)))
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPENOBJ, NULL, "open failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_datatype_open() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_datatype_get
 *
 * Purpose:	Get specific information about the datatype through the VOL
 *
 * Return:	Success:        non negative
 *
 *		Failure:	negative
 *
 * Programmer:	Mohamad Chaarawi
 *              June, 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_datatype_get(void *obj, const H5VL_class_t *vol_cls, H5VL_datatype_get_t get_type, 
                  hid_t dxpl_id, void **req, ...)
{
    va_list           arguments;             /* argument list passed from the API call */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if(NULL == vol_cls->datatype_cls.get)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `datatype get' method")
    va_start (arguments, req);
    if((ret_value = (vol_cls->datatype_cls.get)(obj, get_type, dxpl_id, 
                                                        req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "get failed")
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
 * Programmer:	Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_datatype_specific(void *obj, const H5VL_class_t *vol_cls, H5VL_datatype_specific_t specific_type, 
                       hid_t dxpl_id, void **req, ...)
{
    va_list           arguments;             /* argument list passed from the API call */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if(NULL == vol_cls->datatype_cls.specific)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `datatype specific' method")

    va_start (arguments, req);
    if((ret_value = (vol_cls->datatype_cls.specific)
        (obj, specific_type, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "Unable to execute datatype specific callback")
    va_end (arguments);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_datatype_specific() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_datatype_optional
 *
 * Purpose:	optional operation specific to plugins.
 *
 * Return:	Success:        non negative
 *
 *		Failure:	negative
 *
 * Programmer:	Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_datatype_optional(void *obj, const H5VL_class_t *vol_cls, hid_t dxpl_id, void **req, ...)
{
    va_list           arguments;             /* argument list passed from the API call */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if(NULL == vol_cls->datatype_cls.optional)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `datatype optional' method")

    va_start (arguments, req);
    if((ret_value = (vol_cls->datatype_cls.optional)(obj, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "Unable to execute datatype optional callback")
    va_end (arguments);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_datatype_optional() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_datatype_close
 *
 * Purpose:	Closes a datatype through the VOL
 *
 * Return:      Success: Positive
 *		Failure: Negative
 *
 * Programmer:	Mohamad Chaarawi
 *              May, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_datatype_close(void *dt, const H5VL_class_t *vol_cls, hid_t dxpl_id, void **req)
{
    herr_t		ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* check if the corresponding VOL close callback exists */
    if(NULL == vol_cls->datatype_cls.close)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `datatype close' method")

    /* call the corresponding VOL close callback */
    if((ret_value = (vol_cls->datatype_cls.close)(dt, dxpl_id, req)) < 0)
	HGOTO_ERROR(H5E_VOL, H5E_CANTRELEASE, FAIL, "close failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_datatype_close() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_dataset_create
 *
 * Purpose:	Creates a dataset through the VOL
 *
 * Return:      Success: pointer to dataset
 *
 *		Failure: NULL
 *
 * Programmer:	Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
void *
H5VL_dataset_create(void *obj, H5VL_loc_params_t loc_params, const H5VL_class_t *vol_cls, const char *name, 
                    hid_t dcpl_id, hid_t dapl_id, hid_t dxpl_id, void **req)
{
    void *ret_value; /* Return value */

    FUNC_ENTER_NOAPI(NULL)

    /* check if the corresponding VOL create callback exists */
    if(NULL == vol_cls->dataset_cls.create)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, NULL, "vol plugin has no `dataset create' method")

    /* call the corresponding VOL create callback */
    if(NULL == (ret_value = (vol_cls->dataset_cls.create)
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
 * Programmer:	Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
void *
H5VL_dataset_open(void *obj, H5VL_loc_params_t loc_params, const H5VL_class_t *vol_cls, const char *name, 
                  hid_t dapl_id, hid_t dxpl_id, void **req)
{
    void *ret_value; /* Return value */

    FUNC_ENTER_NOAPI(NULL)

    /* check if the type specific corresponding VOL open callback exists */
    if(NULL == vol_cls->dataset_cls.open)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, NULL, "vol plugin has no 'dset open' method")

    /* call the corresponding VOL open callback */
    if(NULL == (ret_value = (vol_cls->dataset_cls.open)
                (obj, loc_params, name, dapl_id, dxpl_id, req)))
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPENOBJ, NULL, "open failed")

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
 * Programmer:	Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t 
H5VL_dataset_read(void *dset, const H5VL_class_t *vol_cls, hid_t mem_type_id, hid_t mem_space_id, 
                  hid_t file_space_id, hid_t plist_id, void *buf, void **req)
{
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if(NULL == vol_cls->dataset_cls.read)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `dataset read' method")
    if((ret_value = (vol_cls->dataset_cls.read)
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
 * Programmer:	Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t 
H5VL_dataset_write(void *dset, const H5VL_class_t *vol_cls, hid_t mem_type_id, hid_t mem_space_id, 
                   hid_t file_space_id, hid_t plist_id, const void *buf, void **req)
{
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if(NULL == vol_cls->dataset_cls.write)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `dataset write' method")
    if((ret_value = (vol_cls->dataset_cls.write)
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
 * Programmer:	Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_dataset_get(void *dset, const H5VL_class_t *vol_cls, H5VL_dataset_get_t get_type, 
                 hid_t dxpl_id, void **req, ...)
{
    va_list           arguments;             /* argument list passed from the API call */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if(NULL == vol_cls->dataset_cls.get)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `dataset get' method")

    va_start (arguments, req);
    if((ret_value = (vol_cls->dataset_cls.get)(dset, get_type, dxpl_id, req, arguments)) < 0)
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
 * Programmer:	Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_dataset_specific(void *obj, const H5VL_class_t *vol_cls, H5VL_dataset_specific_t specific_type, 
                       hid_t dxpl_id, void **req, ...)
{
    va_list           arguments;             /* argument list passed from the API call */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if(NULL == vol_cls->dataset_cls.specific)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `dataset specific' method")

    va_start (arguments, req);
    if((ret_value = (vol_cls->dataset_cls.specific)
        (obj, specific_type, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "Unable to execute dataset specific callback")
    va_end (arguments);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_dataset_specific() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_dataset_optional
 *
 * Purpose:	optional operation specific to plugins.
 *
 * Return:	Success:        non negative
 *
 *		Failure:	negative
 *
 * Programmer:	Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_dataset_optional(void *obj, const H5VL_class_t *vol_cls, hid_t dxpl_id, void **req, ...)
{
    va_list           arguments;             /* argument list passed from the API call */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if(NULL == vol_cls->dataset_cls.optional)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `dataset optional' method")

    va_start (arguments, req);
    if((ret_value = (vol_cls->dataset_cls.optional)(obj, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "Unable to execute dataset optional callback")
    va_end (arguments);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_dataset_optional() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_dataset_close
 *
 * Purpose:	Closes a dataset through the VOL
 *
 * Return:	Success:	Non Negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_dataset_close(void *dset, const H5VL_class_t *vol_cls, hid_t dxpl_id, void **req)
{
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if(NULL == vol_cls->dataset_cls.close)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `dset close' method")
    if((ret_value = (vol_cls->dataset_cls.close)(dset, dxpl_id, req)) < 0)
            HGOTO_ERROR(H5E_VOL, H5E_CANTRELEASE, FAIL, "close failed")

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
 * Programmer:	Mohamad Chaarawi
 *              January, 2012
 *
 *-------------------------------------------------------------------------
 */
void *
H5VL_file_create(const H5VL_class_t *vol_cls, const char *name, unsigned flags, hid_t fcpl_id, 
                 hid_t fapl_id, hid_t dxpl_id, void **req)
{
    void *ret_value = NULL;             /* Return value */

    FUNC_ENTER_NOAPI(NULL)

    /* check if the corresponding VOL create callback exists */
    if(NULL == vol_cls->file_cls.create)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, NULL, "vol plugin has no `file create' method")
    /* call the corresponding VOL create callback */
    if(NULL == (ret_value = (vol_cls->file_cls.create)(name, flags, fcpl_id, fapl_id, dxpl_id, req)))
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
 * Programmer:	Mohamad Chaarawi
 *              January, 2012
 *
 *-------------------------------------------------------------------------
 */
void *
H5VL_file_open(const H5VL_class_t *vol_cls, const char *name, unsigned flags, hid_t fapl_id, 
               hid_t dxpl_id, void **req)
{
    void *ret_value = NULL;             /* Return value */

    FUNC_ENTER_NOAPI(NULL)

    /* check if the corresponding VOL create callback exists */
    if(NULL == vol_cls->file_cls.open)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, NULL, "vol plugin has no `file open' method")
    /* call the corresponding VOL create callback */
    if(NULL == (ret_value = (vol_cls->file_cls.open)(name, flags, fapl_id, dxpl_id, req)))
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
 * Programmer:	Mohamad Chaarawi
 *              February, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_file_get(void *file, const H5VL_class_t *vol_cls, H5VL_file_get_t get_type, 
              hid_t dxpl_id, void **req, ...)
{
    va_list           arguments;             /* argument list passed from the API call */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if(NULL == vol_cls->file_cls.get)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `file get' method")

    va_start(arguments, req);
    if((ret_value = (vol_cls->file_cls.get)(file, get_type, dxpl_id, req, arguments)) < 0)
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
 * Programmer:	Mohamad Chaarawi
 *              April, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_file_specific(void *file, const H5VL_class_t *vol_cls, H5VL_file_specific_t specific_type, 
                   hid_t dxpl_id, void **req, ...)
{
    va_list           arguments;             /* argument list passed from the API call */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if(specific_type == H5VL_FILE_IS_ACCESSIBLE) {
        H5P_genplist_t     *plist;          /* Property list pointer */
        hid_t               vol_id;         /* VOL plugin identigier attached to fapl_id */
        va_list             tmp_args;       /* argument list passed from the API call */
        hid_t               fapl_id;

        va_start (tmp_args, req);
        fapl_id = va_arg (tmp_args, hid_t);
        va_end (tmp_args);

        /* get the VOL info from the fapl */
        if(NULL == (plist = (H5P_genplist_t *)H5I_object(fapl_id)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access property list")
        if(H5P_get(plist, H5F_ACS_VOL_ID_NAME, &vol_id) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get vol plugin")

        if(NULL == (vol_cls = (H5VL_class_t *)H5I_object(vol_id)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL ID")

        va_start (arguments, req);
        if((ret_value = (vol_cls->file_cls.specific)
            (file, specific_type, dxpl_id, req, arguments)) < 0)
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "specific failed")
        va_end (arguments);
    }
    else {
        if(NULL == vol_cls->file_cls.specific)
            HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `file specific' method")

        va_start (arguments, req);
        if((ret_value = (vol_cls->file_cls.specific)
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
 * Purpose:	perform a plugin specific operation
 *
 * Return:	Success:        non negative
 *		Failure:	negative
 *
 * Programmer:	Mohamad Chaarawi
 *              April, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_file_optional(void *file, const H5VL_class_t *vol_cls, hid_t dxpl_id, void **req, ...)
{
    va_list           arguments;             /* argument list passed from the API call */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if(NULL == vol_cls->file_cls.optional)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `file optional' method")

    va_start (arguments, req);
    if((ret_value = (vol_cls->file_cls.optional)(file, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "optional failed")
    va_end (arguments);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_file_optional() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_file_close
 *
 * Purpose:	Closes a file through the VOL
 *
 * Return:	Success:	Non Negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Mohamad Chaarawi
 *              January, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_file_close(void *file, const H5VL_class_t *vol_cls, hid_t dxpl_id, void **req)
{
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if(NULL == vol_cls->file_cls.close)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `file close' method")
    if((ret_value = (vol_cls->file_cls.close)(file, dxpl_id, req)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTCLOSEFILE, FAIL, "close failed")

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
 * Programmer:	Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
void *
H5VL_group_create(void *obj, H5VL_loc_params_t loc_params, const H5VL_class_t *vol_cls, const char *name, 
                  hid_t gcpl_id, hid_t gapl_id, hid_t dxpl_id, void **req)
{
    void *ret_value = NULL; /* Return value */

    FUNC_ENTER_NOAPI(NULL)

    /* check if the corresponding VOL create callback exists */
    if(NULL == vol_cls->group_cls.create)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, NULL, "vol plugin has no `group create' method")

    /* call the corresponding VOL create callback */
    if(NULL == (ret_value = (vol_cls->group_cls.create)
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
 * Programmer:	Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
void *
H5VL_group_open(void *obj, H5VL_loc_params_t loc_params, const H5VL_class_t *vol_cls, const char *name, 
                hid_t gapl_id, hid_t dxpl_id, void **req)
{
    void *ret_value; /* Return value */

    FUNC_ENTER_NOAPI(NULL)

    if(NULL == vol_cls->group_cls.open)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, NULL, "vol plugin has no `group open' method")

    if(NULL == (ret_value = (vol_cls->group_cls.open)
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
 * Programmer:	Mohamad Chaarawi
 *              February, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_group_get(void *obj, const H5VL_class_t *vol_cls, H5VL_group_get_t get_type, 
               hid_t dxpl_id, void **req, ...)
{
    va_list           arguments;             /* argument list passed from the API call */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if(NULL == vol_cls->group_cls.get)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `group get' method")

    va_start (arguments, req);
    if((ret_value = (vol_cls->group_cls.get)
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
 * Programmer:	Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_group_specific(void *obj, const H5VL_class_t *vol_cls, H5VL_group_specific_t specific_type, 
                       hid_t dxpl_id, void **req, ...)
{
    va_list           arguments;             /* argument list passed from the API call */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if(NULL == vol_cls->group_cls.specific)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `group specific' method")

    va_start (arguments, req);
    if((ret_value = (vol_cls->group_cls.specific)
        (obj, specific_type, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "Unable to execute group specific callback")
    va_end (arguments);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_group_specific() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_group_optional
 *
 * Purpose:	optional operation specific to plugins.
 *
 * Return:	Success:        non negative
 *
 *		Failure:	negative
 *
 * Programmer:	Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_group_optional(void *obj, const H5VL_class_t *vol_cls, hid_t dxpl_id, void **req, ...)
{
    va_list           arguments;             /* argument list passed from the API call */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if(NULL == vol_cls->group_cls.optional)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `group optional' method")

    va_start (arguments, req);
    if((ret_value = (vol_cls->group_cls.optional)(obj, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "Unable to execute group optional callback")
    va_end (arguments);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_group_optional() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_group_close
 *
 * Purpose:	Closes a group through the VOL
 *
 * Return:	Success:	Non Negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_group_close(void *grp, const H5VL_class_t *vol_cls, hid_t dxpl_id, void **req)
{
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if(NULL == vol_cls->group_cls.close)
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `group close' method")
    if((ret_value = (vol_cls->group_cls.close)(grp, dxpl_id, req)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTRELEASE, FAIL, "close failed")

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
 * Programmer:	Mohamad Chaarawi
 *              April, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_link_create(H5VL_link_create_type_t create_type, void *obj, H5VL_loc_params_t loc_params, 
                 const H5VL_class_t *vol_cls, hid_t lcpl_id, hid_t lapl_id, hid_t dxpl_id, void **req)
{
    herr_t               ret_value = SUCCEED;  /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* check if the corresponding VOL create callback exists */
    if(NULL == vol_cls->link_cls.create)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `link create' method")
    /* call the corresponding VOL create callback */
    if((ret_value = (vol_cls->link_cls.create)
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
 * Programmer:	Mohamad Chaarawi
 *              April, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t 
H5VL_link_copy(void *src_obj, H5VL_loc_params_t loc_params1, void *dst_obj,
               H5VL_loc_params_t loc_params2, const H5VL_class_t *vol_cls,  
               hid_t lcpl_id, hid_t lapl_id, hid_t dxpl_id, void **req)
{
    herr_t               ret_value = SUCCEED;  /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* check if the corresponding VOL copy callback exists */
    if(NULL == vol_cls->link_cls.copy)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `link copy' method")

    /* call the corresponding VOL copy callback */
    if((ret_value = (vol_cls->link_cls.copy)
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
 * Programmer:	Mohamad Chaarawi
 *              April, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t 
H5VL_link_move(void *src_obj, H5VL_loc_params_t loc_params1, void *dst_obj,
               H5VL_loc_params_t loc_params2, const H5VL_class_t *vol_cls,  
               hid_t lcpl_id, hid_t lapl_id, hid_t dxpl_id, void **req)
{
    herr_t               ret_value = SUCCEED;  /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* check if the corresponding VOL move callback exists */
    if(NULL == vol_cls->link_cls.move)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `link move' method")

    /* call the corresponding VOL move callback */
    if((ret_value = (vol_cls->link_cls.move)
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
 * Programmer:	Mohamad Chaarawi
 *              April, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_link_get(void *obj, H5VL_loc_params_t loc_params, const H5VL_class_t *vol_cls, H5VL_link_get_t get_type, 
              hid_t dxpl_id, void **req, ...)
{
    va_list           arguments;             /* argument list passed from the API call */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if(NULL == vol_cls->link_cls.get)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `link get' method")

    va_start (arguments, req);
    if((ret_value = (vol_cls->link_cls.get)
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
 * Programmer:	Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_link_specific(void *obj, H5VL_loc_params_t loc_params, const H5VL_class_t *vol_cls, 
                   H5VL_link_specific_t specific_type, hid_t dxpl_id, void **req, ...)
{
    va_list           arguments;             /* argument list passed from the API call */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if(NULL == vol_cls->link_cls.specific)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `link specific' method")

    va_start (arguments, req);
    if((ret_value = (vol_cls->link_cls.specific)
        (obj, loc_params, specific_type, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "Unable to execute link specific callback")
    va_end (arguments);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_link_specific() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_link_optional
 *
 * Purpose:	optional operation specific to plugins.
 *
 * Return:	Success:        non negative
 *
 *		Failure:	negative
 *
 * Programmer:	Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_link_optional(void *obj, const H5VL_class_t *vol_cls, hid_t dxpl_id, void **req, ...)
{
    va_list           arguments;             /* argument list passed from the API call */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if(NULL == vol_cls->link_cls.optional)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `link optional' method")

    va_start (arguments, req);
    if((ret_value = (vol_cls->link_cls.optional)(obj, dxpl_id, req, arguments)) < 0)
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
 * Programmer:	Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
void *
H5VL_object_open(void *obj, H5VL_loc_params_t params, const H5VL_class_t *vol_cls, H5I_type_t *opened_type,
                 hid_t dxpl_id, void **req)
{
    void *ret_value;              /* Return value */

    FUNC_ENTER_NOAPI(NULL)

    /* check if the corresponding VOL open callback exists */
    if(NULL == vol_cls->object_cls.open)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, NULL, "vol plugin has no `object open' method")

    /* call the corresponding VOL open callback */
    if(NULL == (ret_value = (vol_cls->object_cls.open)
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
 * Programmer:	Mohamad Chaarawi
 *              April, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t 
H5VL_object_copy(void *src_obj, H5VL_loc_params_t loc_params1, const H5VL_class_t *vol_cls1, const char *src_name, 
                 void *dst_obj, H5VL_loc_params_t loc_params2, const H5VL_class_t *vol_cls2, const char *dst_name, 
                 hid_t ocpypl_id, hid_t lcpl_id, hid_t dxpl_id, void **req)
{
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    /* check if both objects are associated with the same VOL plugin */
    if(vol_cls1->value != vol_cls2->value)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "Objects are accessed through different VOL plugins and can't be copied")

    if(NULL == vol_cls1->object_cls.copy)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `object copy' method")

    if((ret_value = (vol_cls1->object_cls.copy)
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
 * Programmer:	Mohamad Chaarawi
 *              February, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_object_get(void *obj, H5VL_loc_params_t loc_params, const H5VL_class_t *vol_cls, H5VL_object_get_t get_type, 
                hid_t dxpl_id, void **req, ...)
{
    va_list           arguments;             /* argument list passed from the API call */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if(NULL == vol_cls->object_cls.get)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `object get' method")

    va_start (arguments, req);
    if((ret_value = (vol_cls->object_cls.get)
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
 * Programmer:	Mohamad Chaarawi
 *              April, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_object_specific(void *obj, H5VL_loc_params_t loc_params, const H5VL_class_t *vol_cls, 
                     H5VL_object_specific_t specific_type, hid_t dxpl_id, void **req, ...)
{
    va_list           arguments;             /* argument list passed from the API call */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if(NULL == vol_cls->object_cls.specific)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `object specific' method")

    va_start (arguments, req);
    if((ret_value = (vol_cls->object_cls.specific)
        (obj, loc_params, specific_type, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "specific failed")
    va_end (arguments);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_object_specific() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_object_optional
 *
 * Purpose:	optional operation specific to plugins.
 *
 * Return:	Success:        non negative
 *
 *		Failure:	negative
 *
 * Programmer:	Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_object_optional(void *obj, const H5VL_class_t *vol_cls, hid_t dxpl_id, void **req, ...)
{
    va_list           arguments;             /* argument list passed from the API call */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if(NULL == vol_cls->object_cls.optional)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `object optional' method")

    va_start (arguments, req);
    if((ret_value = (vol_cls->object_cls.optional)(obj, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "Unable to execute object optional callback")
    va_end (arguments);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_object_optional() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_request_cancel
 *
 * Purpose:	Cancels a request through the VOL
 *
 * Return:	Success:	Non Negative
 *		Failure:	Negative
 *
 * Programmer:	Mohamad Chaarawi
 *              March, 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_request_cancel(void **req, const H5VL_class_t *vol_cls, H5ES_status_t *status)
{
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if(NULL == vol_cls->async_cls.cancel)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `async cancel' method");
    if((ret_value = (vol_cls->async_cls.cancel)(req, status)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTRELEASE, FAIL, "request cancel failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_request_cancel() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_request_test
 *
 * Purpose:	Tests a request through the VOL
 *
 * Return:	Success:	Non Negative
 *		Failure:	Negative
 *
 * Programmer:	Mohamad Chaarawi
 *              March, 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_request_test(void **req, const H5VL_class_t *vol_cls, H5ES_status_t *status)
{
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if(NULL == vol_cls->async_cls.test)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `async test' method");
    if((ret_value = (vol_cls->async_cls.test)(req, status)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTRELEASE, FAIL, "request test failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_request_test() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_request_wait
 *
 * Purpose:	Waits a request through the VOL
 *
 * Return:	Success:	Non Negative
 *		Failure:	Negative
 *
 * Programmer:	Mohamad Chaarawi
 *              March, 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_request_wait(void **req, const H5VL_class_t *vol_cls, H5ES_status_t *status)
{
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if(NULL == vol_cls->async_cls.wait)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `async wait' method");
    if((ret_value = (vol_cls->async_cls.wait)(req, status)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTRELEASE, FAIL, "request wait failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_request_wait() */
