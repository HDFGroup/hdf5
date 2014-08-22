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

/* Interface initialization */
#define H5_INTERFACE_INIT_FUNC	H5VL_init_interface

/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Aprivate.h"		/* Attributes				*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5VLpkg.h"		/* VOL package header		  	*/
#include "H5VLprivate.h"	/* VOL          		  	*/

/********************/
/* Local Prototypes */
/********************/
static herr_t H5VL_free_cls(H5VL_class_t *cls);

/*******************/
/* Local Variables */
/*******************/
typedef struct {
    const H5VL_class_t *vol_cls;
    hid_t ret_id;
} H5VL_is_registered_ud_t;

/* VOL ID class */
static const H5I_class_t H5I_VOL_CLS[1] = {{
    H5I_VOL,			/* ID class value */
    0,				/* Class flags */
    0,				/* # of reserved IDs for class */
    (H5I_free_t)H5VL_free_cls,  /* Callback routine for closing objects of this class */
    NULL,                 	/* Callback routine for closing auxilary objects of this class */
}};


/*-------------------------------------------------------------------------
 * Function:	H5VL_init
 *
 * Purpose:	Initialize the interface from some other package.
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Mohamad Chaarawi
 *              January, 2012
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
 * Function:	H5VL_init_interface
 *
 * Purpose:	Initialize the virtual object layer.
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Mohamad Chaarawi
 *              January, 2012
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_init_interface(void)
{
    herr_t ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* register VOL ID type */
    if(H5I_register_type(H5I_VOL_CLS) < 0)
	HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to initialize interface")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_init_interface() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_term_interface
 *
 * Purpose:	Terminate this interface: free all memory and reset global
 *		variables to their initial values.  Release all ID groups
 *		associated with this interface.
 *
 * Return:	Success:	Positive if anything was done that might
 *				have affected other interfaces; zero
 *				otherwise.
 *
 *		Failure:        Never fails.
 *
 * Programmer:	Mohamad Chaarawi
 *              January, 2012
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
H5VL_term_interface(void)
{
    int	n = 0, n1 = 0;
    hbool_t term = TRUE;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    if(H5_interface_initialize_g) {
	if((n1=H5I_nmembers(H5I_VOL))!=0) {
	    H5I_clear_type(H5I_VOL, FALSE, FALSE);
            term = FALSE;
	} else {
	    H5I_dec_type_ref(H5I_VOL);
	}

        if (term) {
            H5_interface_initialize_g = 0;
            n = 1;
        }
        else {
            n = n1;
        }
    }

    FUNC_LEAVE_NOAPI(n)
}


/*-------------------------------------------------------------------------
 * Function:	H5VL_free_cls
 *
 * Purpose:	Frees a file vol class struct and returns an indication of
 *		success. This function is used as the free callback for the
 *		virtual object layer object identifiers (cf H5VL_init_interface).
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Mohamad Chaarawi
 *              January, 2012
 *
 * Modifications:
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

    if(cls->terminate && cls->terminate() < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTCLOSEOBJ, FAIL, "vol plugin '%s' did not terminate cleanly", cls->name)

    H5MM_free(cls);
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_free_cls() */


/*-------------------------------------------------------------------------
 * Function:	H5VLregister
 *
 * Purpose:	Registers a new vol plugin as a member of the virtual object
 *		layer class.
 *
 * Return:	Success:	A vol plugin ID which is good until the
 *				library is closed or the plugin is
 *				unregistered.
 *
 *		Failure:	A negative value.
 *
 * Programmer:	Mohamad Chaarawi
 *              January, 2012
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5VLregister(const H5VL_class_t *cls)
{
    hid_t ret_value;

    FUNC_ENTER_API(FAIL)
    H5TRACE1("i", "*x", cls);

    /* Check arguments */
    if(!cls)
	HGOTO_ERROR(H5E_ARGS, H5E_UNINITIALIZED, FAIL, "null class pointer is disallowed")

    if(cls->value < MAX_VOL_LIB_VALUE)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, 
                    "registered class value must not be smaller than %d", MAX_VOL_LIB_VALUE)

    /* MSC - check if required callback are defined?? */

    /* Create the new class ID */
    if((ret_value=H5VL_register(cls, sizeof(H5VL_class_t), TRUE)) < 0)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to register vol plugin ID")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLregister() */


/*-------------------------------------------------------------------------
 * Function:	H5VLunregister
 *
 * Purpose:	Removes a vol plugin ID from the library. This in no way affects
 *		file access property lists which have been defined to use
 *		this vol plugin or files which are already opened under with
 *              this plugin.
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Mohamad Chaarawi
 *              January, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLunregister(hid_t vol_id)
{
    herr_t ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE1("e", "i", vol_id);

    /* Check arguments */
    if(NULL == H5I_object_verify(vol_id, H5I_VOL))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a vol plugin")

    /* The H5VL_class_t struct will be freed by this function */
    if(H5I_dec_app_ref(vol_id) < 0)
	HGOTO_ERROR(H5E_VOL, H5E_CANTDEC, FAIL, "unable to unregister vol plugin")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLunregister() */


/*-------------------------------------------------------------------------
 * Function:	H5VL__is_registered_cb
 *
 * Purpose:	Callback routine to search through registered VLs
 *
 * Return:	Success:	The first object in the type for which FUNC
 *				returns non-zero. NULL if FUNC returned zero
 *				for every object in the type.
 *		Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *		Friday, March 30, 2012
 *
 *-------------------------------------------------------------------------
 */
static int
H5VL__is_registered_cb(void *obj, hid_t id, void *_op_data)
{
    H5VL_is_registered_ud_t *op_data = (H5VL_is_registered_ud_t *)_op_data; /* User data for callback */
    H5VL_class_t *cls = (H5VL_class_t *)obj;
    int ret_value;     /* Callback return value */

    FUNC_ENTER_STATIC_NOERR

    if(cls->value == op_data->vol_cls->value) {
        op_data->ret_id = id;
        ret_value = H5_ITER_STOP;
    }

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__is_registered_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5VLis_registered
 *
 * Purpose:	Tests whether a VOL class has been registered or not
 *
 * Return:	Positive if the VOL class has been registered
 *              Zero if it is unregistered
 *              Negative on error (if the class is not a valid class ID)
 *
 * Programmer:	Mohamad Chaarawi
 *              June 2012
 *
 *-------------------------------------------------------------------------
 */
htri_t
H5VLis_registered(const H5VL_class_t *cls)
{
    H5VL_is_registered_ud_t op_data;
    htri_t ret_value = FALSE;     /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE1("t", "*x", cls);

    op_data.ret_id = FAIL;
    op_data.vol_cls = cls;

    /* Check arguments */
    if(H5I_iterate(H5I_VOL, H5VL__is_registered_cb, &op_data, TRUE) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_BADITER, FAIL, "can't iterate over VOL ids")

    if(op_data.ret_id != FAIL)
        ret_value = TRUE;

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLis_registered() */


/*-------------------------------------------------------------------------
 * Function:	H5VLget_plugin_name
 *
 * Purpose:	Returns the plugin name for the VOL associated with the 
 *              object or file ID
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
H5VLget_plugin_name(hid_t id, char *name/*out*/, size_t size)
{
    ssize_t    ret_value;

    FUNC_ENTER_API(FAIL)
    H5TRACE3("Zs", "ixz", id, name, size);

    if((ret_value = H5VL_get_plugin_name(id, name, size)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "Can't get plugin name")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLget_plugin_name() */


/*---------------------------------------------------------------------------
 * Function:	H5VLobject_register
 *
 * Purpose:     Public routine to create an HDF5 hid_t with library
 *              specific types, bypassing the limitation of H5Iregister.
 *
 * Returns:     Non-negative on success or negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              June, 2012
 *
 *---------------------------------------------------------------------------
 */
hid_t
H5VLobject_register(void *obj, H5I_type_t obj_type, const H5VL_class_t *cls)
{
    H5VL_t  *vol_plugin;        /* VOL plugin information */
    hid_t ret_value = FAIL;

    FUNC_ENTER_API(FAIL)
    H5TRACE3("i", "*xIt*x", obj, obj_type, cls);

    /* Build the vol plugin struct */
    if(NULL == (vol_plugin = (H5VL_t *)H5MM_calloc(sizeof(H5VL_t))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")
    vol_plugin->cls = cls;
    vol_plugin->nrefs = 1;

    if ((ret_value = H5VL_object_register(obj, obj_type, vol_plugin, TRUE)) < 0)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to atomize dataset handle")

done:
    FUNC_LEAVE_API(ret_value)
} /* H5VLobject_register */


/*---------------------------------------------------------------------------
 * Function:	H5VLget_object
 *
 * Purpose:	Retrieve the object pointer associated with the ID. This 
 *              also optionally returns the H5VL_t struct that this ID 
 *              belongs to, if the user passes a valid pointer value.
 *
 * Returns:     Non-negative on success or negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              July, 2013
 *
 *---------------------------------------------------------------------------
 */
herr_t
H5VLget_object(hid_t obj_id, void **obj, H5VL_t **vol_plugin)
{
    H5VL_t *temp_vol;
    hid_t ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)
    H5TRACE3("e", "i**x**x", obj_id, obj, vol_plugin);

    /* Check args */
    if(!obj)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object pointer")

    /* get the plugin pointer */
    if (NULL == (temp_vol = (H5VL_t *)H5I_get_aux(obj_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information")

    if(NATIVE == temp_vol->cls->value)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "cannot call public function on library type")

    /* if the user requested the plugin pointer, return it */
    if(vol_plugin)
        *vol_plugin = temp_vol;

    if(NULL == (*obj = H5VL_get_object(obj_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain a valid object")

done:
    FUNC_LEAVE_API(ret_value)
} /* H5VLget_object */


/*-------------------------------------------------------------------------
 * Function:	H5VLattr_create
 *
 * Purpose:	Creates an attribute through the VOL
 *
 * Return:      Success: pointer to the new attr. 
 *		Failure: NULL
 *
 * Programmer:	Mohamad Chaarawi
 *              April, 2012
 *
 *-------------------------------------------------------------------------
 */
void *
H5VLattr_create(void *obj, H5VL_loc_params_t loc_params, H5VL_t *vol_plugin, const char *name, 
                hid_t acpl_id, hid_t aapl_id, hid_t dxpl_id, void **req)
{
    void *ret_value = NULL;  /* Return value */

    FUNC_ENTER_API(NULL)

    if (NULL == obj || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "invalid object/VOL class pointer")
    if(NULL == (ret_value = H5VL_attr_create(obj, loc_params, vol_plugin, name, acpl_id, aapl_id, dxpl_id, req)))
	HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, NULL, "unable to create attribute")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLattr_create() */


/*-------------------------------------------------------------------------
 * Function:	H5VLattr_open
 *
 * Purpose:	Opens an attribute through the VOL
 *
 * Return:      Success: pointer to the new attr. 
 *		Failure: NULL
 *
 * Programmer:	Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
void *
H5VLattr_open(void *obj, H5VL_loc_params_t loc_params, H5VL_t *vol_plugin, const char *name, 
              hid_t aapl_id, hid_t dxpl_id, void **req)
{
    void *ret_value;  /* Return value */

    FUNC_ENTER_API(NULL)

    if (NULL == obj || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "invalid object/VOL class pointer")
    if(NULL == (ret_value = H5VL_attr_open(obj, loc_params, vol_plugin, name, aapl_id, dxpl_id, req)))
	HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, NULL, "unable to open attribute")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLattr_open() */


/*-------------------------------------------------------------------------
 * Function:	H5VLattr_read
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
herr_t H5VLattr_read(void *attr, H5VL_t *vol_plugin, hid_t mem_type_id, void *buf, hid_t dxpl_id, void **req)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)

    if (NULL == attr || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")
    if((ret_value = H5VL_attr_read(attr, vol_plugin, mem_type_id, buf, dxpl_id, req)) < 0)
	HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to read attribute")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLattr_read() */


/*-------------------------------------------------------------------------
 * Function:	H5VLattr_write
 *
 * Purpose:	Writes data to attr through the VOL
 *
 * Return:	Success:	Non Negative
 *		Failure:	Negative
 *
 * Programmer:	Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t H5VLattr_write(void *attr, H5VL_t *vol_plugin, hid_t mem_type_id, const void *buf, hid_t dxpl_id, void **req)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)

    if (NULL == attr || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")
    if((ret_value = H5VL_attr_write(attr, vol_plugin, mem_type_id, buf, dxpl_id, req)) < 0)
	HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to write attribute")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLattr_write() */


/*-------------------------------------------------------------------------
 * Function:	H5VLattr_get
 *
 * Purpose:	Get specific information about the attribute through the VOL
 *
 * Return:	Success:        non negative
 *		Failure:	negative
 *
 * Programmer:	Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLattr_get(void *obj, H5VL_t *vol_plugin, H5VL_attr_get_t get_type, 
             hid_t dxpl_id, void **req, va_list arguments)
{
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)
    H5TRACE6("e", "*x*xVai**xx", obj, vol_plugin, get_type, dxpl_id, req, arguments);

    if (NULL == obj || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")

    /* Bypass the H5VLint layer */
    if(NULL == vol_plugin->cls->attr_cls.get)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `attr get' method")
    if((ret_value = (vol_plugin->cls->attr_cls.get)(obj, get_type, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "Unable to get attribute information")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLattr_get() */


/*-------------------------------------------------------------------------
 * Function:	H5VLattr_specific
 *
 * Purpose:	specific operation on attributes through the VOL
 *
 * Return:	Success:        non negative
 *		Failure:	negative
 *
 * Programmer:	Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLattr_specific(void *obj, H5VL_loc_params_t loc_params, H5VL_t *vol_plugin, 
                  H5VL_attr_specific_t specific_type, hid_t dxpl_id, void **req, va_list arguments)
{
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)
    H5TRACE7("e", "*xx*xVbi**xx", obj, loc_params, vol_plugin, specific_type,
             dxpl_id, req, arguments);

    if (NULL == obj || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")

    /* Bypass the H5VLint layer */
    if(NULL == vol_plugin->cls->attr_cls.specific)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `attr specific' method")
    if((ret_value = (vol_plugin->cls->attr_cls.specific)
        (obj, loc_params, specific_type, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "Unable to execute attribute specific callback")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLattr_specific() */


/*-------------------------------------------------------------------------
 * Function:	H5VLattr_optional
 *
 * Purpose:	optional operation specific to plugins.
 *
 * Return:	Success:        non negative
 *		Failure:	negative
 *
 * Programmer:	Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLattr_optional(void *obj, H5VL_t *vol_plugin, hid_t dxpl_id, void **req, va_list arguments)
{
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)
    H5TRACE5("e", "*x*xi**xx", obj, vol_plugin, dxpl_id, req, arguments);

    if (NULL == obj || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")

    /* Have to bypass the H5VLint layer due to unknown val_list arguments */
    if(NULL == vol_plugin->cls->attr_cls.optional)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `attr optional' method")
    if((ret_value = (vol_plugin->cls->attr_cls.optional)(obj, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "Unable to execute attribute optional callback")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLattr_optional() */


/*-------------------------------------------------------------------------
 * Function:	H5VLattr_close
 *
 * Purpose:	Closes an attribute through the VOL
 *
 * Return:	Success:	Non Negative
 *		Failure:	Negative
 *
 * Programmer:	Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLattr_close(void *attr, H5VL_t *vol_plugin, hid_t dxpl_id, void **req)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)
    H5TRACE4("e", "*x*xi**x", attr, vol_plugin, dxpl_id, req);

    if (NULL == attr || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")
    if((ret_value = H5VL_attr_close(attr, vol_plugin, dxpl_id, req)) < 0)
	HGOTO_ERROR(H5E_VOL, H5E_CANTRELEASE, FAIL, "unable to close attribute")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLattr_close() */


/*-------------------------------------------------------------------------
 * Function:	H5VLdatatype_commit
 *
 * Purpose:	Commits a datatype to the file through the VOL
 *
 * Return:      Success: Positive
 *		Failure: Negative
 *
 * Programmer:	Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
void *
H5VLdatatype_commit(void *obj, H5VL_loc_params_t loc_params, H5VL_t *vol_plugin, const char *name, 
                     hid_t type_id, hid_t lcpl_id, hid_t tcpl_id, hid_t tapl_id, hid_t dxpl_id, void **req)
{
    void *ret_value = NULL;              /* Return value */

    FUNC_ENTER_API(NULL)

    if (NULL == obj || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "invalid object/VOL class pointer")
    if(NULL == (ret_value = H5VL_datatype_commit(obj, loc_params, vol_plugin, name, type_id, 
                                                 lcpl_id, tcpl_id, tapl_id, dxpl_id, req)))
	HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, NULL, "unable to commit datatype")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLdatatype_commit() */


/*-------------------------------------------------------------------------
 * Function:	H5VLdatatype_open
 *
 * Purpose:	Opens a named datatype through the VOL
 *
 * Return:      Success: User ID of the datatype. 
 *		Failure: NULL
 *
 * Programmer:	Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
void *
H5VLdatatype_open(void *obj, H5VL_loc_params_t loc_params, H5VL_t *vol_plugin, const char *name, 
                   hid_t tapl_id, hid_t dxpl_id, void **req)
{
    void *ret_value = NULL;              /* Return value */

    FUNC_ENTER_API(NULL)

    if (NULL == obj || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "invalid object/VOL class pointer")
    if(NULL == (ret_value = H5VL_datatype_open(obj, loc_params, vol_plugin, name, tapl_id, dxpl_id, req)))
	HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, NULL, "unable to open datatype")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLdatatype_open() */


/*-------------------------------------------------------------------------
 * Function:	H5VLdatatype_specific
 *
 * Purpose:	specific operation on datatypes through the VOL
 *
 * Return:	Success:        non negative
 *		Failure:	negative
 *
 * Programmer:	Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLdatatype_specific(void *obj, H5VL_t *vol_plugin, H5VL_datatype_specific_t specific_type, 
                      hid_t dxpl_id, void **req, va_list arguments)
{
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)
    H5TRACE6("e", "*x*xVfi**xx", obj, vol_plugin, specific_type, dxpl_id, req,
             arguments);

    if (NULL == obj || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")

    if(NULL == vol_plugin->cls->datatype_cls.specific)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `datatype specific' method")
    if((ret_value = (vol_plugin->cls->datatype_cls.specific)
        (obj, specific_type, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "Unable to execute datatype specific callback")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLdatatype_specific() */


/*-------------------------------------------------------------------------
 * Function:	H5VLdatatype_optional
 *
 * Purpose:	optional operation specific to plugins.
 *
 * Return:	Success:        non negative
 *		Failure:	negative
 *
 * Programmer:	Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLdatatype_optional(void *obj, H5VL_t *vol_plugin, hid_t dxpl_id, void **req, va_list arguments)
{
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)
    H5TRACE5("e", "*x*xi**xx", obj, vol_plugin, dxpl_id, req, arguments);

    if (NULL == obj || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")

    if(NULL == vol_plugin->cls->datatype_cls.optional)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `datatype optional' method")
    if((ret_value = (vol_plugin->cls->datatype_cls.optional)(obj, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "Unable to execute datatype optional callback")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLdatatype_optional() */


/*-------------------------------------------------------------------------
 * Function:	H5VLdatatype_get
 *
 * Purpose:	Get specific information about the datatype through the VOL
 *
 * Return:	Success:        non negative
 *		Failure:	negative
 *
 * Programmer:	Mohamad Chaarawi
 *              June, 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLdatatype_get(void *obj, H5VL_t *vol_plugin, H5VL_datatype_get_t get_type, 
                 hid_t dxpl_id, void **req, va_list arguments)
{
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)
    H5TRACE6("e", "*x*xVei**xx", obj, vol_plugin, get_type, dxpl_id, req, arguments);

    if(NULL == obj || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")

    /* Bypass the H5VLint layer */
    if(NULL == vol_plugin->cls->datatype_cls.get)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `datatype get' method")
    if((ret_value = (vol_plugin->cls->datatype_cls.get)(obj, get_type, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "Unable to execute datatype get callback")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLdatatype_get() */


/*-------------------------------------------------------------------------
 * Function:	H5VLdatatype_close
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
H5VLdatatype_close(void *dt, H5VL_t *vol_plugin, hid_t dxpl_id, void **req)
{
    herr_t		ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE4("e", "*x*xi**x", dt, vol_plugin, dxpl_id, req);

    if (NULL == dt || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")
    if((ret_value = H5VL_datatype_close(dt, vol_plugin, dxpl_id, req)) < 0)
	HGOTO_ERROR(H5E_VOL, H5E_CANTRELEASE, FAIL, "unable to close datatype")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLdatatype_close() */


/*-------------------------------------------------------------------------
 * Function:	H5VLdataset_create
 *
 * Purpose:	Creates a dataset through the VOL
 *
 * Return:      Success: pointer to dataset
 *		Failure: NULL
 *
 * Programmer:	Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
void *
H5VLdataset_create(void *obj, H5VL_loc_params_t loc_params, H5VL_t *vol_plugin, const char *name, 
                    hid_t dcpl_id, hid_t dapl_id, hid_t dxpl_id, void **req)
{
    void *ret_value; /* Return value */

    FUNC_ENTER_API(NULL)

    if (NULL == obj || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "invalid object/VOL class pointer")
    if(NULL == (ret_value = H5VL_dataset_create(obj, loc_params, vol_plugin, name, 
                                                dcpl_id, dapl_id, dxpl_id, req)))
	HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, NULL, "unable to create dataset")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLdataset_create() */


/*-------------------------------------------------------------------------
 * Function:	H5VLdataset_open
 *
 * Purpose:	Opens a dataset through the VOL
 *
 * Return:      Success: pointer to dataset 
 *		Failure: NULL
 *
 * Programmer:	Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
void *
H5VLdataset_open(void *obj, H5VL_loc_params_t loc_params, H5VL_t *vol_plugin, const char *name, 
                  hid_t dapl_id, hid_t dxpl_id, void **req)
{
    void *ret_value; /* Return value */

    FUNC_ENTER_API(NULL)

    if (NULL == obj || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "invalid object/VOL class pointer")
    if(NULL == (ret_value = H5VL_dataset_open(obj, loc_params, vol_plugin, name, dapl_id, dxpl_id, req)))
	HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, NULL, "unable to open dataset")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLdataset_open() */


/*-------------------------------------------------------------------------
 * Function:	H5VLdataset_read
 *
 * Purpose:	Reads data from dataset through the VOL
 *
 * Return:	Success:	Non Negative
 *		Failure:	Negative
 *
 * Programmer:	Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLdataset_read(void *dset, H5VL_t *vol_plugin, hid_t mem_type_id, hid_t mem_space_id, 
                  hid_t file_space_id, hid_t plist_id, void *buf, void **req)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)
    H5TRACE8("e", "*x*xiiii*x**x", dset, vol_plugin, mem_type_id, mem_space_id,
             file_space_id, plist_id, buf, req);

    if (NULL == dset || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")
    if((ret_value = H5VL_dataset_read(dset, vol_plugin, mem_type_id, mem_space_id, file_space_id, 
                                      plist_id, buf, req)) < 0)
	HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to read dataset")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLdataset_read() */


/*-------------------------------------------------------------------------
 * Function:	H5VLdataset_write
 *
 * Purpose:	Writes data from dataset through the VOL
 *
 * Return:	Success:	Non Negative
 *		Failure:	Negative
 *
 * Programmer:	Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLdataset_write(void *dset, H5VL_t *vol_plugin, hid_t mem_type_id, hid_t mem_space_id, 
                   hid_t file_space_id, hid_t plist_id, const void *buf, void **req)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)
    H5TRACE8("e", "*x*xiiii*x**x", dset, vol_plugin, mem_type_id, mem_space_id,
             file_space_id, plist_id, buf, req);

    if (NULL == dset || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")
    if((ret_value = H5VL_dataset_write(dset, vol_plugin, mem_type_id, mem_space_id, file_space_id, 
                                       plist_id, buf, req)) < 0)
	HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to write dataset")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLdataset_write() */


/*-------------------------------------------------------------------------
 * Function:	H5VLdataset_get
 *
 * Purpose:	Get specific information about the dataset through the VOL
 *
 * Return:	Success:        non negative
 *		Failure:	negative
 *
 * Programmer:	Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLdataset_get(void *dset, H5VL_t *vol_plugin, H5VL_dataset_get_t get_type, 
                hid_t dxpl_id, void **req, va_list arguments)
{
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)
    H5TRACE6("e", "*x*xVci**xx", dset, vol_plugin, get_type, dxpl_id, req,
             arguments);

    if (NULL == dset || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")

    /* Bypass the H5VLint layer */
    if(NULL == vol_plugin->cls->dataset_cls.get)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `dataset get' method")
    if((ret_value = (vol_plugin->cls->dataset_cls.get)(dset, get_type, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "Unable to execute dataset get callback")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLdataset_get() */


/*-------------------------------------------------------------------------
 * Function:	H5VLdataset_specific
 *
 * Purpose:	specific operation on datasets through the VOL
 *
 * Return:	Success:        non negative
 *		Failure:	negative
 *
 * Programmer:	Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLdataset_specific(void *obj, H5VL_t *vol_plugin, H5VL_dataset_specific_t specific_type, 
                      hid_t dxpl_id, void **req, va_list arguments)
{
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)
    H5TRACE6("e", "*x*xVdi**xx", obj, vol_plugin, specific_type, dxpl_id, req,
             arguments);

    if (NULL == obj || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")

    if(NULL == vol_plugin->cls->dataset_cls.specific)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `dataset specific' method")
    if((ret_value = (vol_plugin->cls->dataset_cls.specific)
        (obj, specific_type, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "Unable to execute dataset specific callback")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLdataset_specific() */


/*-------------------------------------------------------------------------
 * Function:	H5VLdataset_optional
 *
 * Purpose:	optional operation specific to plugins.
 *
 * Return:	Success:        non negative
 *		Failure:	negative
 *
 * Programmer:	Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLdataset_optional(void *obj, H5VL_t *vol_plugin, hid_t dxpl_id, void **req, va_list arguments)
{
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)
    H5TRACE5("e", "*x*xi**xx", obj, vol_plugin, dxpl_id, req, arguments);

    if (NULL == obj || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")

    if(NULL == vol_plugin->cls->dataset_cls.optional)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `dataset optional' method")
    if((ret_value = (vol_plugin->cls->dataset_cls.optional)(obj, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "Unable to execute dataset optional callback")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLdataset_optional() */


/*-------------------------------------------------------------------------
 * Function:	H5VLdataset_close
 *
 * Purpose:	Closes a dataset through the VOL
 *
 * Return:	Success:	Non Negative
 *		Failure:	Negative
 *
 * Programmer:	Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLdataset_close(void *dset, H5VL_t *vol_plugin, hid_t dxpl_id, void **req)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)
    H5TRACE4("e", "*x*xi**x", dset, vol_plugin, dxpl_id, req);

    if (NULL == dset || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")
    if((ret_value = H5VL_dataset_close(dset, vol_plugin, dxpl_id, req)) < 0)
	HGOTO_ERROR(H5E_VOL, H5E_CANTRELEASE, FAIL, "unable to close dataset")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLdataset_close() */


/*-------------------------------------------------------------------------
 * Function:	H5VLfile_create
 *
 * Purpose:	Creates a file through the VOL
 *
 * Return:      Success: pointer to file. 
 *		Failure: NULL
 *
 * Programmer:	Mohamad Chaarawi
 *              January, 2012
 *
 *-------------------------------------------------------------------------
 */
void *
H5VLfile_create(H5VL_t **vol_plugin, const char *name, unsigned flags, hid_t fcpl_id, 
                hid_t fapl_id, hid_t dxpl_id, void **req)
{
    void	       *ret_value;             /* Return value */

    FUNC_ENTER_API(NULL)

    if(NULL == (ret_value = H5VL_file_create(vol_plugin, name, flags, fcpl_id, fapl_id, dxpl_id, req)))
	HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, NULL, "unable to create file")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLfile_create() */


/*-------------------------------------------------------------------------
 * Function:	H5VLfile_open
 *
 * Purpose:	Opens a file through the VOL.
 *
 * Return:      Success: pointer to file. 
 *		Failure: NULL
 *
 * Programmer:	Mohamad Chaarawi
 *              January, 2012
 *
 *-------------------------------------------------------------------------
 */
void *
H5VLfile_open(H5VL_t **vol_plugin, const char *name, unsigned flags, hid_t fapl_id, hid_t dxpl_id, void **req)
{
    void	       *ret_value;             /* Return value */

    FUNC_ENTER_API(NULL)

    if(NULL == (ret_value = H5VL_file_open(vol_plugin, name, flags, fapl_id, dxpl_id, req)))
	HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, NULL, "unable to create file")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLfile_open() */


/*-------------------------------------------------------------------------
 * Function:	H5VLfile_get
 *
 * Purpose:	Get specific information about the file through the VOL
 *
 * Return:	Success:        non negative
 *		Failure:	negative
 *
 * Programmer:	Mohamad Chaarawi
 *              February, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLfile_get(void *file, H5VL_t *vol_plugin, H5VL_file_get_t get_type, 
             hid_t dxpl_id, void **req, va_list arguments)
{
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)
    H5TRACE6("e", "*x*xVgi**xx", file, vol_plugin, get_type, dxpl_id, req,
             arguments);

    if(NULL == file || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")

    /* Bypass the H5VLint layer */
    if(NULL == vol_plugin->cls->file_cls.get)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `file get' method")
    if((ret_value = (vol_plugin->cls->file_cls.get)(file, get_type, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "Unable to execute file get callback")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLfile_get() */


/*-------------------------------------------------------------------------
 * Function:	H5VLfile_specific
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
H5VLfile_specific(void *file, H5VL_t *vol_plugin, H5VL_file_specific_t specific_type, 
                  hid_t dxpl_id, void **req, va_list arguments)
{
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)
    H5TRACE6("e", "*x*xVhi**xx", file, vol_plugin, specific_type, dxpl_id, req,
             arguments);

    if(specific_type == H5VL_FILE_IS_ACCESSIBLE) {
        H5P_genplist_t     *plist;          /* Property list pointer */
        hid_t               vol_id;         /* VOL plugin identigier attached to fapl_id */
        H5VL_class_t       *vol_cls;        /* VOL class of vol_id */
        hid_t               fapl_id;

        fapl_id = va_arg (arguments, hid_t);

        /* get the VOL info from the fapl */
        if(NULL == (plist = (H5P_genplist_t *)H5I_object(fapl_id)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access property list")
        if(H5P_get(plist, H5F_ACS_VOL_ID_NAME, &vol_id) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get vol plugin")

        if(NULL == (vol_cls = (H5VL_class_t *)H5I_object(vol_id)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL ID")

        if((ret_value = (vol_cls->file_cls.specific)
            (file, specific_type, dxpl_id, req, arguments)) < 0)
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "specific failed")
    }
    else {
        if(NULL == file || NULL == vol_plugin || NULL == vol_plugin->cls)
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")

        if(NULL == vol_plugin->cls->file_cls.specific)
            HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `file specific' method")
        if((ret_value = (vol_plugin->cls->file_cls.specific)
            (file, specific_type, dxpl_id, req, arguments)) < 0)
            HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "Unable to execute file specific callback")
    }
done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLfile_specific() */


/*-------------------------------------------------------------------------
 * Function:	H5VLfile_optional
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
H5VLfile_optional(void *file, H5VL_t *vol_plugin, hid_t dxpl_id, void **req, va_list arguments)
{
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)
    H5TRACE5("e", "*x*xi**xx", file, vol_plugin, dxpl_id, req, arguments);

    if(NULL == file || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")

    if(NULL == vol_plugin->cls->file_cls.optional)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `file optional' method")
    if((ret_value = (vol_plugin->cls->file_cls.optional)(file, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "Unable to execute file optional callback")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLfile_optional() */


/*-------------------------------------------------------------------------
 * Function:	H5VLfile_close
 *
 * Purpose:	Closes a file through the VOL
 *
 * Return:	Success:	Non Negative
 *		Failure:	Negative
 *
 * Programmer:	Mohamad Chaarawi
 *              January, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLfile_close(void *file, H5VL_t *vol_plugin, hid_t dxpl_id, void **req)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)
    H5TRACE4("e", "*x*xi**x", file, vol_plugin, dxpl_id, req);

    if(NULL == file || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")
    if((ret_value = H5VL_file_close(file, vol_plugin, dxpl_id, req)) < 0)
	HGOTO_ERROR(H5E_VOL, H5E_CANTRELEASE, FAIL, "unable to close file")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLfile_close() */


/*-------------------------------------------------------------------------
 * Function:	H5VLgroup_create
 *
 * Purpose:	Creates a group through the VOL
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
H5VLgroup_create(void *obj, H5VL_loc_params_t loc_params, H5VL_t *vol_plugin, const char *name, 
                 hid_t gcpl_id, hid_t gapl_id, hid_t dxpl_id, void **req)
{
    void *ret_value = NULL; /* Return value */

    FUNC_ENTER_API(NULL)

    if (NULL == obj || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "invalid object/VOL class pointer")
    if(NULL == (ret_value = H5VL_group_create(obj, loc_params, vol_plugin, name, 
                                              gcpl_id, gapl_id, dxpl_id, req)))
	HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, NULL, "unable to create group")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLgroup_create() */


/*-------------------------------------------------------------------------
 * Function:	H5VLgroup_open
 *
 * Purpose:	Opens a group through the VOL
 *
 * Return:      Success: pointer to new group.
 *		Failure: NULL
 *
 * Programmer:	Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
void *
H5VLgroup_open(void *obj, H5VL_loc_params_t loc_params, H5VL_t *vol_plugin, const char *name, 
                hid_t gapl_id, hid_t dxpl_id, void **req)
{
    void *ret_value; /* Return value */

    FUNC_ENTER_API(NULL)

    if (NULL == obj || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "invalid object/VOL class pointer")
    if(NULL == (ret_value = H5VL_group_open(obj, loc_params, vol_plugin, name, 
                                              gapl_id, dxpl_id, req)))
	HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, NULL, "unable to open group")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLgroup_open() */


/*-------------------------------------------------------------------------
 * Function:	H5VLgroup_get
 *
 * Purpose:	Get specific information about the group through the VOL
 *
 * Return:	Success:        non negative
 *		Failure:	negative
 *
 * Programmer:	Mohamad Chaarawi
 *              February, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLgroup_get(void *obj, H5VL_t *vol_plugin, H5VL_group_get_t get_type, 
              hid_t dxpl_id, void **req, va_list arguments)
{
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)
    H5TRACE6("e", "*x*xVii**xx", obj, vol_plugin, get_type, dxpl_id, req, arguments);

    if(NULL == obj || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")

    /* Bypass the H5VLint layer */
    if(NULL == vol_plugin->cls->group_cls.get)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `group get' method")
    if((ret_value = (vol_plugin->cls->group_cls.get)(obj, get_type, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "Unable to execute group get callback")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLgroup_get() */


/*-------------------------------------------------------------------------
 * Function:	H5VLgroup_specific
 *
 * Purpose:	specific operation on groups through the VOL
 *
 * Return:	Success:        non negative
 *		Failure:	negative
 *
 * Programmer:	Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLgroup_specific(void *obj, H5VL_t *vol_plugin, H5VL_group_specific_t specific_type, 
                      hid_t dxpl_id, void **req, va_list arguments)
{
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)
    H5TRACE6("e", "*x*xVji**xx", obj, vol_plugin, specific_type, dxpl_id, req,
             arguments);

    if (NULL == obj || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")

    if(NULL == vol_plugin->cls->group_cls.specific)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `group specific' method")
    if((ret_value = (vol_plugin->cls->group_cls.specific)
        (obj, specific_type, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "Unable to execute group specific callback")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLgroup_specific() */


/*-------------------------------------------------------------------------
 * Function:	H5VLgroup_optional
 *
 * Purpose:	optional operation specific to plugins.
 *
 * Return:	Success:        non negative
 *		Failure:	negative
 *
 * Programmer:	Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLgroup_optional(void *obj, H5VL_t *vol_plugin, hid_t dxpl_id, void **req, va_list arguments)
{
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)
    H5TRACE5("e", "*x*xi**xx", obj, vol_plugin, dxpl_id, req, arguments);

    if (NULL == obj || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")

    if(NULL == vol_plugin->cls->group_cls.optional)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `group optional' method")
    if((ret_value = (vol_plugin->cls->group_cls.optional)(obj, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "Unable to execute group optional callback")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLgroup_optional() */


/*-------------------------------------------------------------------------
 * Function:	H5VLgroup_close
 *
 * Purpose:	Closes a group through the VOL
 *
 * Return:	Success:	Non Negative
 *		Failure:	Negative
 *
 * Programmer:	Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLgroup_close(void *grp, H5VL_t *vol_plugin, hid_t dxpl_id, void **req)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)
    H5TRACE4("e", "*x*xi**x", grp, vol_plugin, dxpl_id, req);

    if(NULL == grp || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")
    if((ret_value = H5VL_group_close(grp, vol_plugin, dxpl_id, req)) < 0)
	HGOTO_ERROR(H5E_VOL, H5E_CANTRELEASE, FAIL, "unable to close group")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLgroup_close() */


/*-------------------------------------------------------------------------
 * Function:	H5VLlink_create
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
H5VLlink_create(H5VL_link_create_type_t create_type, void *obj, H5VL_loc_params_t loc_params, 
                 H5VL_t *vol_plugin, hid_t lcpl_id, hid_t lapl_id, hid_t dxpl_id, void **req)
{
    herr_t               ret_value = SUCCEED;  /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE8("e", "Vk*xx*xiii**x", create_type, obj, loc_params, vol_plugin,
             lcpl_id, lapl_id, dxpl_id, req);

    if(NULL == obj || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")
    if((ret_value = H5VL_link_create(create_type, obj, loc_params, vol_plugin, lcpl_id, lapl_id, dxpl_id, req)) < 0)
	HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to create link")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLlink_create() */


/*-------------------------------------------------------------------------
 * Function:	H5VLlink_copy
 *
 * Purpose:	Copy a link from src to dst.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Mohamad Chaarawi
 *              April, 2012
 *
 *-------------------------------------------------------------------------
 */
H5_DLL herr_t H5VLlink_copy(void *src_obj, H5VL_loc_params_t loc_params1, void *dst_obj, 
                            H5VL_loc_params_t loc_params2, H5VL_t *vol_plugin, 
                            hid_t lcpl_id, hid_t lapl_id, hid_t dxpl_id, void **req)
{
    herr_t               ret_value = SUCCEED;  /* Return value */

    FUNC_ENTER_API(FAIL)

    if(NULL == src_obj || NULL == dst_obj || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")
            if((ret_value = H5VL_link_copy(src_obj, loc_params1, dst_obj, loc_params2, vol_plugin, 
                                           lcpl_id, lapl_id, dxpl_id, req)) < 0)
	HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to copy object")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLlink_copy() */


/*-------------------------------------------------------------------------
 * Function:	H5VLlink_move
 *
 * Purpose:	Move a link from src to dst.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Mohamad Chaarawi
 *              April, 2012
 *
 *-------------------------------------------------------------------------
 */
H5_DLL herr_t H5VLlink_move(void *src_obj, H5VL_loc_params_t loc_params1, void *dst_obj, 
                            H5VL_loc_params_t loc_params2, H5VL_t *vol_plugin, 
                            hid_t lcpl_id, hid_t lapl_id, hid_t dxpl_id, void **req)
{
    herr_t               ret_value = SUCCEED;  /* Return value */

    FUNC_ENTER_API(FAIL)

    if(NULL == src_obj || NULL == dst_obj || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")
    if((ret_value = H5VL_link_move(src_obj, loc_params1, dst_obj, loc_params2, vol_plugin, 
                                   lcpl_id, lapl_id, dxpl_id, req)) < 0)
	HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to move object")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLlink_move() */


/*-------------------------------------------------------------------------
 * Function:	H5VLlink_get
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
H5VLlink_get(void *obj, H5VL_loc_params_t loc_params, H5VL_t *vol_plugin, H5VL_link_get_t get_type, 
              hid_t dxpl_id, void **req, va_list arguments)
{
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)
    H5TRACE7("e", "*xx*xVli**xx", obj, loc_params, vol_plugin, get_type, dxpl_id,
             req, arguments);

    if(NULL == obj || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")

    if(NULL == vol_plugin->cls->link_cls.get)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `link get' method")
    if((ret_value = (vol_plugin->cls->link_cls.get)
        (obj, loc_params, get_type, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "Unable to execute link get callback")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLlink_get() */


/*-------------------------------------------------------------------------
 * Function:	H5VLlink_specific
 *
 * Purpose:	specific operation on links through the VOL
 *
 * Return:	Success:        non negative
 *		Failure:	negative
 *
 * Programmer:	Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLlink_specific(void *obj, H5VL_loc_params_t loc_params, H5VL_t *vol_plugin, 
                  H5VL_link_specific_t specific_type, hid_t dxpl_id, void **req, va_list arguments)
{
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)
    H5TRACE7("e", "*xx*xVmi**xx", obj, loc_params, vol_plugin, specific_type,
             dxpl_id, req, arguments);

    if (NULL == obj || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")

    /* Bypass the H5VLint layer */
    if(NULL == vol_plugin->cls->link_cls.specific)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `link specific' method")
    if((ret_value = (vol_plugin->cls->link_cls.specific)
        (obj, loc_params, specific_type, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "Unable to execute link specific callback")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLlink_specific() */


/*-------------------------------------------------------------------------
 * Function:	H5VLlink_optional
 *
 * Purpose:	optional operation specific to plugins.
 *
 * Return:	Success:        non negative
 *		Failure:	negative
 *
 * Programmer:	Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLlink_optional(void *obj, H5VL_t *vol_plugin, hid_t dxpl_id, void **req, va_list arguments)
{
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)
    H5TRACE5("e", "*x*xi**xx", obj, vol_plugin, dxpl_id, req, arguments);

    if (NULL == obj || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")

    /* Have to bypass the H5VLint layer due to unknown val_list arguments */
    if(NULL == vol_plugin->cls->link_cls.optional)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `link optional' method")
    if((ret_value = (vol_plugin->cls->link_cls.optional)(obj, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "Unable to execute link optional callback")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLlink_optional() */


/*-------------------------------------------------------------------------
 * Function:	H5VLobject_open
 *
 * Purpose:	Opens a object through the VOL
 *
 * Return:      Success: User ID of the new object. 
 *		Failure: NULL
 *
 * Programmer:	Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
void *
H5VLobject_open(void *obj, H5VL_loc_params_t params, H5VL_t *vol_plugin, H5I_type_t *opened_type,
                hid_t dxpl_id, void **req)
{
    void *ret_value;              /* Return value */

    FUNC_ENTER_API(NULL)

    if (NULL == obj || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "invalid object/VOL class pointer")
    if(NULL == (ret_value = H5VL_object_open(obj, params, vol_plugin, opened_type, dxpl_id, req)))
	HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, NULL, "unable to create group")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLobject_open() */


/*-------------------------------------------------------------------------
 * Function:	H5VLobject_copy
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
H5VLobject_copy(void *src_obj, H5VL_loc_params_t loc_params1, H5VL_t *vol_plugin1, const char *src_name, 
                void *dst_obj, H5VL_loc_params_t loc_params2, H5VL_t *vol_plugin2, const char *dst_name, 
                hid_t ocpypl_id, hid_t lcpl_id, hid_t dxpl_id, void **req)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)
    H5TRACE12("e", "*xx*x*s*xx*x*siii**x", src_obj, loc_params1, vol_plugin1,
             src_name, dst_obj, loc_params2, vol_plugin2, dst_name, ocpypl_id,
             lcpl_id, dxpl_id, req);

    if(NULL == src_obj || NULL == dst_obj || NULL == vol_plugin1 || 
       NULL == vol_plugin2 || NULL == vol_plugin1->cls || NULL == vol_plugin2->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")
    if((ret_value = H5VL_object_copy(src_obj, loc_params1, vol_plugin1, src_name, 
                                     dst_obj, loc_params2, vol_plugin2, dst_name,
                                     ocpypl_id, lcpl_id, dxpl_id, req)) < 0)
	HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to move object")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLobject_copy() */


/*-------------------------------------------------------------------------
 * Function:	H5VLobject_get
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
H5VLobject_get(void *obj, H5VL_loc_params_t loc_params, H5VL_t *vol_plugin, H5VL_object_get_t get_type, 
               hid_t dxpl_id, void **req, va_list arguments)
{
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)
    H5TRACE7("e", "*xx*xVni**xx", obj, loc_params, vol_plugin, get_type, dxpl_id,
             req, arguments);

    if(NULL == obj || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")

    if(NULL == vol_plugin->cls->object_cls.get)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `object get' method")
    if((ret_value = (vol_plugin->cls->object_cls.get)
        (obj, loc_params, get_type, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "Unable to execute object get callback")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLobject_get() */


/*-------------------------------------------------------------------------
 * Function:	H5VLobject_specific
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
H5VLobject_specific(void *obj, H5VL_loc_params_t loc_params, H5VL_t *vol_plugin, 
                    H5VL_object_specific_t specific_type, hid_t dxpl_id, void **req, va_list arguments)
{
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)
    H5TRACE7("e", "*xx*xVoi**xx", obj, loc_params, vol_plugin, specific_type,
             dxpl_id, req, arguments);

    if(NULL == obj || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")

    /* Bypass the H5VLint layer */
    if(NULL == vol_plugin->cls->object_cls.specific)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `object specific' method")
    if((ret_value = (vol_plugin->cls->object_cls.specific)
        (obj, loc_params, specific_type, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "Unable to execute object specific callback")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLobject_specific() */


/*-------------------------------------------------------------------------
 * Function:	H5VLobject_optional
 *
 * Purpose:	optional operation specific to plugins.
 *
 * Return:	Success:        non negative
 *		Failure:	negative
 *
 * Programmer:	Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLobject_optional(void *obj, H5VL_t *vol_plugin, hid_t dxpl_id, void **req, va_list arguments)
{
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)
    H5TRACE5("e", "*x*xi**xx", obj, vol_plugin, dxpl_id, req, arguments);

    if (NULL == obj || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")

    /* Have to bypass the H5VLint layer due to unknown val_list arguments */
    if(NULL == vol_plugin->cls->object_cls.optional)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `object optional' method")
    if((ret_value = (vol_plugin->cls->object_cls.optional)(obj, dxpl_id, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTOPERATE, FAIL, "Unable to execute object optional callback")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLobject_optional() */


/*-------------------------------------------------------------------------
 * Function:	H5VLrequest_cancel
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
H5VLrequest_cancel(void **req, H5VL_t *vol_plugin, H5ES_status_t *status)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)
    H5TRACE3("e", "**x*x*Es", req, vol_plugin, status);

    if(NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid request/VOL class pointer")
    if((ret_value = H5VL_request_cancel(req, vol_plugin, status)) < 0)
	HGOTO_ERROR(H5E_VOL, H5E_CANTRELEASE, FAIL, "unable to cancel request")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLrequest_cancel() */


/*-------------------------------------------------------------------------
 * Function:	H5VLrequest_test
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
H5VLrequest_test(void **req, H5VL_t *vol_plugin, H5ES_status_t *status)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)
    H5TRACE3("e", "**x*x*Es", req, vol_plugin, status);

    if(NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid request/VOL class pointer")
    if((ret_value = H5VL_request_test(req, vol_plugin, status)) < 0)
	HGOTO_ERROR(H5E_VOL, H5E_CANTRELEASE, FAIL, "unable to test request")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLrequest_test() */


/*-------------------------------------------------------------------------
 * Function:	H5VLrequest_wait
 *
 * Purpose:	Waits on a request through the VOL
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
H5VLrequest_wait(void **req, H5VL_t *vol_plugin, H5ES_status_t *status)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)
    H5TRACE3("e", "**x*x*Es", req, vol_plugin, status);

    if(NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid request/VOL class pointer")
    if((ret_value = H5VL_request_wait(req, vol_plugin, status)) < 0)
	HGOTO_ERROR(H5E_VOL, H5E_CANTRELEASE, FAIL, "unable to wait on request")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLrequest_wait() */
