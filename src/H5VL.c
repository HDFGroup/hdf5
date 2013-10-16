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
H5VLis_registered(hid_t id)
{
    htri_t ret_value = FALSE;     /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE1("t", "i", id);

    /* Check arguments */
    if(NULL != H5I_object_verify(id, H5I_VOL))
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
 *
 *		Failure: NULL
 *
 * Programmer:	Mohamad Chaarawi
 *              April, 2012
 *
 *-------------------------------------------------------------------------
 */
void *
H5VLattr_create(void *obj, H5VL_loc_params_t loc_params, H5VL_t *vol_plugin, const char *name, 
                hid_t acpl_id, hid_t aapl_id, hid_t dxpl_id, void UNUSED **req)
{
    void *ret_value = NULL;  /* Return value */

    FUNC_ENTER_API(NULL)

    if (NULL == obj || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "invalid object/VOL class pointer")
    if(NULL == (ret_value = H5VL_attr_create(obj, loc_params, vol_plugin, name, acpl_id, aapl_id, dxpl_id, H5_EVENT_QUEUE_NULL)))
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
 *
 *		Failure: NULL
 *
 * Programmer:	Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
void *
H5VLattr_open(void *obj, H5VL_loc_params_t loc_params, H5VL_t *vol_plugin, const char *name, 
              hid_t aapl_id, hid_t dxpl_id, void UNUSED **req)
{
    void *ret_value;  /* Return value */

    FUNC_ENTER_API(NULL)

    if (NULL == obj || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "invalid object/VOL class pointer")
    if(NULL == (ret_value = H5VL_attr_open(obj, loc_params, vol_plugin, name, aapl_id, dxpl_id, H5_EVENT_QUEUE_NULL)))
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
herr_t H5VLattr_read(void *attr, H5VL_t *vol_plugin, hid_t mem_type_id, void *buf, hid_t dxpl_id, void UNUSED **req)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)

    if (NULL == attr || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")
    if((ret_value = H5VL_attr_read(attr, vol_plugin, mem_type_id, buf, dxpl_id, H5_EVENT_QUEUE_NULL)) < 0)
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
 *
 *		Failure:	Negative
 *
 * Programmer:	Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t H5VLattr_write(void *attr, H5VL_t *vol_plugin, hid_t mem_type_id, const void *buf, hid_t dxpl_id, void UNUSED **req)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)

    if (NULL == attr || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")
    if((ret_value = H5VL_attr_write(attr, vol_plugin, mem_type_id, buf, dxpl_id, H5_EVENT_QUEUE_NULL)) < 0)
	HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to write attribute")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLattr_write() */


/*-------------------------------------------------------------------------
 * Function:	H5VLattr_iterate
 *
 * Purpose:	Iterate over attrs in a group
 *
 * Return:	Success:        non negative
 *		Failure:	negative
 *
 * Programmer:	Mohamad Chaarawi
 *              June, 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t H5VLattr_iterate(void *obj, H5VL_loc_params_t loc_params, H5VL_t *vol_plugin, 
                        H5_index_t idx_type, H5_iter_order_t order, hsize_t *n, 
                        H5A_operator2_t op, void *op_data, hid_t dxpl_id, void UNUSED **req)
{
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)

    if(NULL == obj || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")
    if((ret_value = H5VL_attr_iterate(obj, loc_params, vol_plugin, idx_type, order, n,
                                      op, op_data, dxpl_id, H5_EVENT_QUEUE_NULL)) < 0)
	HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "Attr iteration failed")

done:
    FUNC_LEAVE_API(ret_value)    
} /* end H5VLattr_iterate() */


/*-------------------------------------------------------------------------
 * Function:	H5VLattr_get
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
H5VLattr_get(void *obj, H5VL_t *vol_plugin, H5VL_attr_get_t get_type, hid_t dxpl_id, void UNUSED **req, va_list arguments)
{
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)

    if (NULL == obj || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")

    switch (get_type) {
        /* H5Aexists/exists_by_name */
        case H5VL_ATTR_EXISTS:
            {
                H5VL_loc_params_t loc_params  = va_arg (arguments, H5VL_loc_params_t);
                char    *attr_name      = va_arg (arguments, char *);
                htri_t	*ret       = va_arg (arguments, htri_t *);

                if((ret_value = H5VL_attr_get(obj, vol_plugin, get_type, dxpl_id, H5_EVENT_QUEUE_NULL, 
                                              loc_params, attr_name, ret)) < 0)
                    HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to get attribute information")
                break;
            }
        /* H5Aget_space */
        case H5VL_ATTR_GET_SPACE:
            {
                hid_t	*ret_id = va_arg (arguments, hid_t *);

                if((ret_value = H5VL_attr_get(obj, vol_plugin, get_type, dxpl_id, H5_EVENT_QUEUE_NULL, ret_id)) < 0)
                    HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to get attribute information")
                break;
            }
        /* H5Aget_type */
        case H5VL_ATTR_GET_TYPE:
            {
                hid_t	*ret_id = va_arg (arguments, hid_t *);

                if((ret_value = H5VL_attr_get(obj, vol_plugin, get_type, dxpl_id, H5_EVENT_QUEUE_NULL, ret_id)) < 0)
                    HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to get attribute information")
                break;
            }
        /* H5Aget_create_plist */
        case H5VL_ATTR_GET_ACPL:
            {
                hid_t	*ret_id = va_arg (arguments, hid_t *);

                if((ret_value = H5VL_attr_get(obj, vol_plugin, get_type, dxpl_id, H5_EVENT_QUEUE_NULL, ret_id)) < 0)
                    HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to get attribute information")
                break;
            }
        /* H5Aget_name */
        case H5VL_ATTR_GET_NAME:
            {
                H5VL_loc_params_t loc_params = va_arg (arguments, H5VL_loc_params_t);
                size_t	buf_size = va_arg (arguments, size_t);
                char    *buf = va_arg (arguments, char *);
                ssize_t	*ret_val = va_arg (arguments, ssize_t *);

                if((ret_value = H5VL_attr_get(obj, vol_plugin, get_type, dxpl_id, H5_EVENT_QUEUE_NULL, 
                                              loc_params, buf_size, buf, ret_val)) < 0)
                    HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to get attribute information")
                break;
            }
        /* H5Aget_info */
        case H5VL_ATTR_GET_INFO:
            {
                H5VL_loc_params_t loc_params = va_arg (arguments, H5VL_loc_params_t);
                H5A_info_t   *ainfo = va_arg (arguments, H5A_info_t *);

                if(H5VL_OBJECT_BY_SELF == loc_params.type) {
                    if((ret_value = H5VL_attr_get(obj, vol_plugin, get_type, dxpl_id, H5_EVENT_QUEUE_NULL, loc_params, ainfo)) < 0)
                        HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to get attribute information")
                }
                else if(H5VL_OBJECT_BY_NAME == loc_params.type) {
                    char *attr_name = va_arg (arguments, char *);

                    if((ret_value = H5VL_attr_get(obj, vol_plugin, get_type, dxpl_id, H5_EVENT_QUEUE_NULL, loc_params, ainfo, attr_name)) < 0)
                        HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to get attribute information")
                }
                else if(H5VL_OBJECT_BY_IDX == loc_params.type) {
                    if((ret_value = H5VL_attr_get(obj, vol_plugin, get_type, dxpl_id, H5_EVENT_QUEUE_NULL, loc_params, ainfo)) < 0)
                        HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to get attribute information")
                }
                else
                    HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't get name of attr")

                break;
            }
        case H5VL_ATTR_GET_STORAGE_SIZE:
            {
                hsize_t *ret = va_arg (arguments, hsize_t *);
                if((ret_value = H5VL_attr_get(obj, vol_plugin, get_type, dxpl_id, H5_EVENT_QUEUE_NULL, ret)) < 0)
                    HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to get attribute information")
                break;
            }
        default:
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't get this type of information from attr")
    }

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLattr_get() */


/*-------------------------------------------------------------------------
 * Function:	H5VLattr_remove
 *
 * Purpose:	Removes an attribute through the VOL
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
H5VLattr_remove(void *obj, H5VL_loc_params_t loc_params, H5VL_t *vol_plugin, 
                 const char *attr_name, hid_t dxpl_id, void UNUSED **req)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)

    if (NULL == obj || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")
    if((ret_value = H5VL_attr_remove(obj, loc_params, vol_plugin, attr_name, dxpl_id, H5_EVENT_QUEUE_NULL)) < 0)
	HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to remove attribute")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLattr_remove() */


/*-------------------------------------------------------------------------
 * Function:	H5VLattr_close
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
H5VLattr_close(void *attr, H5VL_t *vol_plugin, hid_t dxpl_id, void UNUSED **req)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)

    if (NULL == attr || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")
    if((ret_value = H5VL_attr_close(attr, vol_plugin, dxpl_id, H5_EVENT_QUEUE_NULL)) < 0)
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
 *
 *		Failure: Negative
 *
 * Programmer:	Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
void *
H5VLdatatype_commit(void *obj, H5VL_loc_params_t loc_params, H5VL_t *vol_plugin, const char *name, 
                     hid_t type_id, hid_t lcpl_id, hid_t tcpl_id, hid_t tapl_id, hid_t dxpl_id, void UNUSED **req)
{
    void *ret_value = NULL;              /* Return value */

    FUNC_ENTER_API(NULL)

    if (NULL == obj || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "invalid object/VOL class pointer")
    if(NULL == (ret_value = H5VL_datatype_commit(obj, loc_params, vol_plugin, name, type_id, 
                                                 lcpl_id, tcpl_id, tapl_id, dxpl_id, H5_EVENT_QUEUE_NULL)))
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
 *
 *		Failure: NULL
 *
 * Programmer:	Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
void *
H5VLdatatype_open(void *obj, H5VL_loc_params_t loc_params, H5VL_t *vol_plugin, const char *name, 
                   hid_t tapl_id, hid_t dxpl_id, void UNUSED **req)
{
    void *ret_value = NULL;              /* Return value */

    FUNC_ENTER_API(NULL)

    if (NULL == obj || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "invalid object/VOL class pointer")
    if(NULL == (ret_value = H5VL_datatype_open(obj, loc_params, vol_plugin, name, tapl_id, dxpl_id, H5_EVENT_QUEUE_NULL)))
	HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, NULL, "unable to open datatype")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLdatatype_open() */


/*-------------------------------------------------------------------------
 * Function:	H5VLdatatype_get_binary
 *
 * Purpose:	gets required size to serialize datatype description
 *
 * Return:      Success: size needed
 *
 *		Failure: negative
 *
 * Programmer:	Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
ssize_t
H5VLdatatype_get_binary(void *obj, H5VL_t *vol_plugin, unsigned char *buf, size_t size, hid_t dxpl_id, void UNUSED **req)
{
    ssize_t ret_value = FAIL;

    FUNC_ENTER_API(FAIL)

    if (NULL == obj || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")
    if((ret_value = H5VL_datatype_get_binary(obj, vol_plugin, buf, size, dxpl_id, H5_EVENT_QUEUE_NULL)) < 0)
	HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to encode datatype")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLdatatype_get_binary() */


/*-------------------------------------------------------------------------
 * Function:	H5VLdatatype_get
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
H5VLdatatype_get(void *obj, H5VL_t *vol_plugin, H5VL_datatype_get_t get_type, 
                 hid_t dxpl_id, void UNUSED **req, va_list arguments)
{
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)

    if(NULL == obj || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")

    switch (get_type) {
        /* H5Tget_create_plist */
        case H5VL_DATATYPE_GET_TCPL:
            {
                hid_t *new_tcpl_id = va_arg (arguments, hid_t *);

                if((ret_value = H5VL_datatype_get(obj, vol_plugin, get_type, dxpl_id, 
                                                  H5_EVENT_QUEUE_NULL, new_tcpl_id)) < 0)
                    HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to get datatype information")
                break;
            }
        default:
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't get this type of information from datatype")
    }

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
H5VLdatatype_close(void *dt, H5VL_t *vol_plugin, hid_t dxpl_id, void UNUSED **req)
{
    herr_t		ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_API(FAIL)

    if (NULL == dt || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")
    if((ret_value = H5VL_datatype_close(dt, vol_plugin, dxpl_id, H5_EVENT_QUEUE_NULL)) < 0)
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
 *
 *		Failure: NULL
 *
 * Programmer:	Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
void *
H5VLdataset_create(void *obj, H5VL_loc_params_t loc_params, H5VL_t *vol_plugin, const char *name, 
                    hid_t dcpl_id, hid_t dapl_id, hid_t dxpl_id, void UNUSED **req)
{
    void *ret_value; /* Return value */

    FUNC_ENTER_API(NULL)

    if (NULL == obj || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "invalid object/VOL class pointer")
    if(NULL == (ret_value = H5VL_dataset_create(obj, loc_params, vol_plugin, name, 
                                                dcpl_id, dapl_id, dxpl_id, H5_EVENT_QUEUE_NULL)))
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
 *
 *		Failure: NULL
 *
 * Programmer:	Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
void *
H5VLdataset_open(void *obj, H5VL_loc_params_t loc_params, H5VL_t *vol_plugin, const char *name, 
                  hid_t dapl_id, hid_t dxpl_id, void UNUSED **req)
{
    void *ret_value; /* Return value */

    FUNC_ENTER_API(NULL)

    if (NULL == obj || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "invalid object/VOL class pointer")
    if(NULL == (ret_value = H5VL_dataset_open(obj, loc_params, vol_plugin, name, dapl_id, dxpl_id, H5_EVENT_QUEUE_NULL)))
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
 *
 *		Failure:	Negative
 *
 * Programmer:	Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLdataset_read(void *dset, H5VL_t *vol_plugin, hid_t mem_type_id, hid_t mem_space_id, 
                  hid_t file_space_id, hid_t plist_id, void *buf, void UNUSED **req)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)

    if (NULL == dset || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")
    if((ret_value = H5VL_dataset_read(dset, vol_plugin, mem_type_id, mem_space_id, file_space_id, 
                                      plist_id, buf, H5_EVENT_QUEUE_NULL)) < 0)
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
 *
 *		Failure:	Negative
 *
 * Programmer:	Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLdataset_write(void *dset, H5VL_t *vol_plugin, hid_t mem_type_id, hid_t mem_space_id, 
                   hid_t file_space_id, hid_t plist_id, const void *buf, void UNUSED **req)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)

    if (NULL == dset || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")
    if((ret_value = H5VL_dataset_write(dset, vol_plugin, mem_type_id, mem_space_id, file_space_id, 
                                       plist_id, buf, H5_EVENT_QUEUE_NULL)) < 0)
	HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to write dataset")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLdataset_write() */


/*-------------------------------------------------------------------------
 * Function:	H5VLdataset_set_extent
 *
 * Purpose:	Modifies the dimensions of a dataset
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
H5VLdataset_set_extent(void *dset, H5VL_t *vol_plugin, const hsize_t size[], hid_t dxpl_id, void UNUSED **req)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)

    if (NULL == dset || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")
    if((ret_value = H5VL_dataset_set_extent(dset, vol_plugin, size, dxpl_id, H5_EVENT_QUEUE_NULL)) < 0)
	HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to set extent of dataset")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLdataset_set_extent() */


/*-------------------------------------------------------------------------
 * Function:	H5VLdataset_get
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
H5VLdataset_get(void *dset, H5VL_t *vol_plugin, H5VL_dataset_get_t get_type, hid_t dxpl_id, void UNUSED **req, va_list arguments)
{
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)

    if (NULL == dset || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")

    switch (get_type) {
        /* H5Dget_space */
        case H5VL_DATASET_GET_SPACE:
            {
                hid_t	*ret_id = va_arg (arguments, hid_t *);

                if((ret_value = H5VL_dataset_get(dset, vol_plugin, get_type, dxpl_id, H5_EVENT_QUEUE_NULL, ret_id)) < 0)
                    HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to get dataset information")
                break;
            }
            /* H5Dget_space_statuc */
        case H5VL_DATASET_GET_SPACE_STATUS:
            {
                H5D_space_status_t *allocation = va_arg (arguments, H5D_space_status_t *);

                if((ret_value = H5VL_dataset_get(dset, vol_plugin, get_type, dxpl_id, H5_EVENT_QUEUE_NULL, allocation)) < 0)
                    HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to get dataset information")
                break;
            }
            /* H5Dget_type */
        case H5VL_DATASET_GET_TYPE:
            {
                hid_t	*ret_id = va_arg (arguments, hid_t *);

                if((ret_value = H5VL_dataset_get(dset, vol_plugin, get_type, dxpl_id, H5_EVENT_QUEUE_NULL, ret_id)) < 0)
                    HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to get dataset information")
                break;
            }
            /* H5Dget_create_plist */
        case H5VL_DATASET_GET_DCPL:
            {
                hid_t	*ret_id = va_arg (arguments, hid_t *);

                if((ret_value = H5VL_dataset_get(dset, vol_plugin, get_type, dxpl_id, H5_EVENT_QUEUE_NULL, ret_id)) < 0)
                    HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to get dataset information")
                break;
            }
            /* H5Dget_access_plist */
        case H5VL_DATASET_GET_DAPL:
            {
                hid_t	*ret_id = va_arg (arguments, hid_t *);

                if((ret_value = H5VL_dataset_get(dset, vol_plugin, get_type, dxpl_id, H5_EVENT_QUEUE_NULL, ret_id)) < 0)
                    HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to get dataset information")
                break;
            }
            /* H5Dget_storage_size */
        case H5VL_DATASET_GET_STORAGE_SIZE:
            {
                hsize_t *ret = va_arg (arguments, hsize_t *);

                if((ret_value = H5VL_dataset_get(dset, vol_plugin, get_type, dxpl_id, H5_EVENT_QUEUE_NULL, ret)) < 0)
                    HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to get dataset information")
                break;
            }
            /* H5Dget_offset */
        case H5VL_DATASET_GET_OFFSET:
            {
                haddr_t *ret = va_arg (arguments, haddr_t *);

                if((ret_value = H5VL_dataset_get(dset, vol_plugin, get_type, dxpl_id, H5_EVENT_QUEUE_NULL, ret)) < 0)
                    HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to get dataset information")
                break;
            }
        default:
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't get this type of information from dataset")
    }

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLdataset_get() */


/*-------------------------------------------------------------------------
 * Function:	H5VLdataset_close
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
H5VLdataset_close(void *dset, H5VL_t *vol_plugin, hid_t dxpl_id, void UNUSED **req)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)

    if (NULL == dset || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")
    if((ret_value = H5VL_dataset_close(dset, vol_plugin, dxpl_id, H5_EVENT_QUEUE_NULL)) < 0)
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
 *
 *		Failure: NULL
 *
 * Programmer:	Mohamad Chaarawi
 *              January, 2012
 *
 *-------------------------------------------------------------------------
 */
void *
H5VLfile_create(H5VL_t **vol_plugin, const char *name, unsigned flags, hid_t fcpl_id, 
                hid_t fapl_id, hid_t dxpl_id, void UNUSED **req)
{
    void	       *ret_value;             /* Return value */

    FUNC_ENTER_API(NULL)

    if(NULL == (ret_value = H5VL_file_create(vol_plugin, name, flags, fcpl_id, fapl_id, dxpl_id, H5_EVENT_QUEUE_NULL)))
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
 *
 *		Failure: NULL
 *
 * Programmer:	Mohamad Chaarawi
 *              January, 2012
 *
 *-------------------------------------------------------------------------
 */
void *
H5VLfile_open(H5VL_t **vol_plugin, const char *name, unsigned flags, hid_t fapl_id, hid_t dxpl_id, void UNUSED **req)
{
    void	       *ret_value;             /* Return value */

    FUNC_ENTER_API(NULL)

    if(NULL == (ret_value = H5VL_file_open(vol_plugin, name, flags, fapl_id, dxpl_id, H5_EVENT_QUEUE_NULL)))
	HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, NULL, "unable to create file")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLfile_open() */


/*-------------------------------------------------------------------------
 * Function:	H5VLfile_flush
 *
 * Purpose:	Flushes a file through the VOL
 *
 * Return:	Success:	Non Negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Mohamad Chaarawi
 *              February, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLfile_flush(void *obj, H5VL_loc_params_t loc_params, H5VL_t *vol_plugin, 
               H5F_scope_t scope, hid_t dxpl_id, void UNUSED **req)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)

    if (NULL == obj || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")
    if((ret_value = H5VL_file_flush(obj, loc_params, vol_plugin, scope, dxpl_id, H5_EVENT_QUEUE_NULL)) < 0)
	HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to flush file")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLfile_flush() */


/*-------------------------------------------------------------------------
 * Function:	H5VLfile_get
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
H5VLfile_get(void *file, H5VL_t *vol_plugin, H5VL_file_get_t get_type, hid_t dxpl_id, void UNUSED **req, va_list arguments)
{
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)

    if(NULL == file || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")

    switch (get_type) {
        /* H5Fget_access_plist */
        case H5VL_FILE_GET_FAPL:
            {
                hid_t *plist_id = va_arg (arguments, hid_t *);

                if((ret_value = H5VL_file_get(file, vol_plugin, get_type, dxpl_id, H5_EVENT_QUEUE_NULL, plist_id)) < 0)
                    HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to get file information")
                break;
            }
        /* H5Fget_create_plist */
        case H5VL_FILE_GET_FCPL:
            {
                hid_t *plist_id = va_arg (arguments, hid_t *);

                if((ret_value = H5VL_file_get(file, vol_plugin, get_type, dxpl_id, H5_EVENT_QUEUE_NULL, plist_id)) < 0)
                    HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to get file information")
                break;
            }
        /* H5Fget_obj_count */
        case H5VL_FILE_GET_OBJ_COUNT:
            {
                unsigned types = va_arg (arguments, unsigned);
                ssize_t *ret = va_arg (arguments, ssize_t *);

                if((ret_value = H5VL_file_get(file, vol_plugin, get_type, dxpl_id, H5_EVENT_QUEUE_NULL, types, ret)) < 0)
                    HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to get file information")
                break;
            }
        /* H5Fget_obj_ids */
        case H5VL_FILE_GET_OBJ_IDS:
            {
                unsigned types = va_arg (arguments, unsigned);
                size_t max_objs = va_arg (arguments, size_t);
                hid_t *oid_list = va_arg (arguments, hid_t *);
                ssize_t *ret = va_arg (arguments, ssize_t *);

                if((ret_value = H5VL_file_get(file, vol_plugin, get_type, dxpl_id, H5_EVENT_QUEUE_NULL, types, max_objs, 
                                              oid_list, ret)) < 0)
                    HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to get file information")
                break;
            }
        /* H5Fget_intent */
        case H5VL_FILE_GET_INTENT:
            {
                unsigned *ret = va_arg (arguments, unsigned *);

                if((ret_value = H5VL_file_get(file, vol_plugin, get_type, dxpl_id, H5_EVENT_QUEUE_NULL, ret)) < 0)
                    HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to get file information")
                break;
            }
        /* H5Fget_name */
        case H5VL_FILE_GET_NAME:
            {
                H5I_type_t type = va_arg (arguments, H5I_type_t);
                size_t     size = va_arg (arguments, size_t);
                char      *name = va_arg (arguments, char *);
                ssize_t   *ret  = va_arg (arguments, ssize_t *);

                if((ret_value = H5VL_file_get(file, vol_plugin, get_type, dxpl_id, H5_EVENT_QUEUE_NULL, type, size, 
                                              name, ret)) < 0)
                    HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to get file information")
                break;
            }
        /* H5I_get_file_id */
        case H5VL_OBJECT_GET_FILE:
            {
                H5I_type_t type = va_arg (arguments, H5I_type_t);
                void      **ret = va_arg (arguments, void **);

                if((ret_value = H5VL_file_get(file, vol_plugin, get_type, dxpl_id, H5_EVENT_QUEUE_NULL, type, ret)) < 0)
                    HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to get file information")
                break;
            }
        default:
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't get this type of information")
    } /* end switch */

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLfile_get() */


/*-------------------------------------------------------------------------
 * Function:	H5VLfile_misc
 *
 * Purpose:	perform a specified operation through the VOL
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
H5VLfile_misc(void *file, H5VL_t *vol_plugin, H5VL_file_misc_t misc_type, hid_t dxpl_id, void UNUSED **req, va_list arguments)
{
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)

    if(NULL == file || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")

    switch (misc_type) {
        /* H5Fmount */
        case H5VL_FILE_MOUNT:
            {
                H5I_type_t  type       = va_arg (arguments, H5I_type_t);
                const char *name       = va_arg (arguments, const char *);
                H5F_t      *child      = va_arg (arguments, H5F_t *);
                hid_t       plist_id   = va_arg (arguments, hid_t);

                if((ret_value = H5VL_file_misc(file, vol_plugin, misc_type, dxpl_id, H5_EVENT_QUEUE_NULL, 
                                               type, name, child, plist_id)) < 0)
                    HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to operate on the file")

                break;
            }
        /* H5Fmount */
        case H5VL_FILE_UNMOUNT:
            {
                H5I_type_t  type       = va_arg (arguments, H5I_type_t);
                const char *name       = va_arg (arguments, const char *);

                if((ret_value = H5VL_file_misc(file, vol_plugin, misc_type, dxpl_id, H5_EVENT_QUEUE_NULL, type, name)) < 0)
                    HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to operate on the file")
                break;
            }
        /* H5Fis_accessible */
        case H5VL_FILE_IS_ACCESSIBLE:
            {
                hid_t fapl_id  = va_arg (arguments, hid_t);
                const char *name    = va_arg (arguments, const char *);
                htri_t     *ret     = va_arg (arguments, htri_t *);

                if((ret_value = H5VL_file_misc(file, vol_plugin, misc_type, dxpl_id, H5_EVENT_QUEUE_NULL, fapl_id, name, ret)) < 0)
                    HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to operate on the file")
                break;
            }
        default:
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't recognize this operation type")
    }

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLfile_misc() */


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
H5VLfile_optional(void *file, H5VL_t *vol_plugin, H5VL_file_optional_t optional_type, hid_t dxpl_id, void UNUSED **req, va_list arguments)
{
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)

    if(NULL == file || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")

    switch (optional_type) {
        /* H5Fget_filesize */
        case H5VL_FILE_GET_SIZE:
            {
                hsize_t    *ret = va_arg (arguments, hsize_t *);

                if((ret_value = H5VL_file_optional(file, vol_plugin, optional_type, dxpl_id, H5_EVENT_QUEUE_NULL, ret)) < 0)
                    HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to operate on the file")
                break;
            }
        /* H5Fget_file_image */
        case H5VL_FILE_GET_FILE_IMAGE:
            {
                void       *buf_ptr   = va_arg (arguments, void *);
                ssize_t    *ret       = va_arg (arguments, ssize_t *);
                size_t      buf_len   = va_arg (arguments, size_t );

                if((ret_value = H5VL_file_optional(file, vol_plugin, optional_type, dxpl_id, H5_EVENT_QUEUE_NULL, 
                                                   buf_ptr, ret, buf_len)) < 0)
                    HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to operate on the file")
                break;
            }
        /* H5Fget_freespace */
        case H5VL_FILE_GET_FREE_SPACE:
            {
                hssize_t    *ret = va_arg (arguments, hssize_t *);

                if((ret_value = H5VL_file_optional(file, vol_plugin, optional_type, dxpl_id, H5_EVENT_QUEUE_NULL, ret)) < 0)
                    HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to operate on the file")
                break;
            }
        case H5VL_FILE_GET_FREE_SECTIONS:
            {
                H5F_sect_info_t *sect_info = va_arg (arguments, H5F_sect_info_t *);
                ssize_t         *ret       = va_arg (arguments, ssize_t *);
                H5F_mem_t       type       = va_arg (arguments, H5F_mem_t);
                size_t          nsects     = va_arg (arguments, size_t);

                if((ret_value = H5VL_file_optional(file, vol_plugin, optional_type, dxpl_id, H5_EVENT_QUEUE_NULL,
                                                   sect_info, ret, type, nsects)) < 0)
                    HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to operate on the file")
                break;
            }
        /* H5Fget_info2 */
        case H5VL_FILE_GET_INFO:
            {
                H5I_type_t  type   = va_arg (arguments, H5I_type_t);
                H5F_info2_t *finfo = va_arg (arguments, H5F_info2_t *);

                if((ret_value = H5VL_file_optional(file, vol_plugin, optional_type, dxpl_id, H5_EVENT_QUEUE_NULL, type, finfo)) < 0)
                    HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to operate on the file")
                break;
            }
        /* H5Fget_mdc_config */
        case H5VL_FILE_GET_MDC_CONF:
            {
                H5AC_cache_config_t *config_ptr = va_arg (arguments, H5AC_cache_config_t *);

                if((ret_value = H5VL_file_optional(file, vol_plugin, optional_type, dxpl_id, H5_EVENT_QUEUE_NULL, config_ptr)) < 0)
                    HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to operate on the file")
                break;
            }
        /* H5Fget_mdc_hit_rate */
        case H5VL_FILE_GET_MDC_HR:
            {
                double *hit_rate_ptr = va_arg (arguments, double *);

                if((ret_value = H5VL_file_optional(file, vol_plugin, optional_type, dxpl_id, H5_EVENT_QUEUE_NULL, hit_rate_ptr)) < 0)
                    HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to operate on the file")
                break;
            }
        /* H5Fget_mdc_size */
        case H5VL_FILE_GET_MDC_SIZE:
            {
                size_t *max_size_ptr        = va_arg (arguments, size_t *);
                size_t *min_clean_size_ptr  = va_arg (arguments, size_t *);
                size_t *cur_size_ptr        = va_arg (arguments, size_t *); 
                int    *cur_num_entries_ptr = va_arg (arguments, int *); 

                if((ret_value = H5VL_file_optional(file, vol_plugin, optional_type, dxpl_id, H5_EVENT_QUEUE_NULL, max_size_ptr, 
                                                   min_clean_size_ptr, cur_size_ptr, cur_num_entries_ptr)) < 0)
                    HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to operate on the file")
                break;
            }
        /* H5Fget_vfd_handle */
        case H5VL_FILE_GET_VFD_HANDLE:
            {
                void **file_handle = va_arg (arguments, void **);
                hid_t  fapl        = va_arg (arguments, hid_t);

                if((ret_value = H5VL_file_optional(file, vol_plugin, optional_type, dxpl_id, H5_EVENT_QUEUE_NULL, file_handle, fapl)) < 0)
                    HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to operate on the file")
                break;
            }
        /* H5Fclear_elink_file_cache */
        case H5VL_FILE_CLEAR_ELINK_CACHE:
            {
                if((ret_value = H5VL_file_optional(file, vol_plugin, optional_type, dxpl_id, H5_EVENT_QUEUE_NULL)) < 0)
                    HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to operate on the file")
                break;
            }
        /* H5Freopen */
        case H5VL_FILE_REOPEN:
            {
                void   **ret = va_arg (arguments, void **);

                if((ret_value = H5VL_file_optional(file, vol_plugin, optional_type, dxpl_id, H5_EVENT_QUEUE_NULL, ret)) < 0)
                    HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to operate on the file")
                break;
            }
        /* H5Freset_mdc_hit_rate_stats */
        case H5VL_FILE_RESET_MDC_HIT_RATE:
            {
                if((ret_value = H5VL_file_optional(file, vol_plugin, optional_type, dxpl_id, H5_EVENT_QUEUE_NULL)) < 0)
                    HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to operate on the file")
                break;
            }
        case H5VL_FILE_SET_MDC_CONFIG:
            {
                H5AC_cache_config_t *config_ptr = va_arg (arguments, H5AC_cache_config_t *);

                if((ret_value = H5VL_file_optional(file, vol_plugin, optional_type, dxpl_id, H5_EVENT_QUEUE_NULL, config_ptr)) < 0)
                    HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to operate on the file")
                break;
            }
        default:
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't recognize this operation type")
    }

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLfile_optional() */


/*-------------------------------------------------------------------------
 * Function:	H5VLfile_close
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
H5VLfile_close(void *file, H5VL_t *vol_plugin, hid_t dxpl_id, void UNUSED **req)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)

    if(NULL == file || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")
    if((ret_value = H5VL_file_close(file, vol_plugin, dxpl_id, H5_EVENT_QUEUE_NULL)) < 0)
	HGOTO_ERROR(H5E_VOL, H5E_CANTRELEASE, FAIL, "unable to close file")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLfile_close() */


/*-------------------------------------------------------------------------
 * Function:	H5VLgroup_create
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
H5VLgroup_create(void *obj, H5VL_loc_params_t loc_params, H5VL_t *vol_plugin, const char *name, 
                 hid_t gcpl_id, hid_t gapl_id, hid_t dxpl_id, void UNUSED **req)
{
    void *ret_value = NULL; /* Return value */

    FUNC_ENTER_API(NULL)

    if (NULL == obj || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "invalid object/VOL class pointer")
    if(NULL == (ret_value = H5VL_group_create(obj, loc_params, vol_plugin, name, 
                                              gcpl_id, gapl_id, dxpl_id, H5_EVENT_QUEUE_NULL)))
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
 *
 *		Failure: NULL
 *
 * Programmer:	Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
void *
H5VLgroup_open(void *obj, H5VL_loc_params_t loc_params, H5VL_t *vol_plugin, const char *name, 
                hid_t gapl_id, hid_t dxpl_id, void UNUSED **req)
{
    void *ret_value; /* Return value */

    FUNC_ENTER_API(NULL)

    if (NULL == obj || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "invalid object/VOL class pointer")
    if(NULL == (ret_value = H5VL_group_open(obj, loc_params, vol_plugin, name, 
                                              gapl_id, dxpl_id, H5_EVENT_QUEUE_NULL)))
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
 *
 *		Failure:	negative
 *
 * Programmer:	Mohamad Chaarawi
 *              February, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLgroup_get(void *obj, H5VL_t *vol_plugin, H5VL_group_get_t get_type, hid_t dxpl_id, void UNUSED **req, va_list arguments)
{
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)

    if(NULL == obj || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")

    switch (get_type) {
        /* H5Gget_create_plist */
        case H5VL_GROUP_GET_GCPL:
            {
                hid_t *new_gcpl_id = va_arg (arguments, hid_t *);

                if((ret_value = H5VL_group_get(obj, vol_plugin, get_type, dxpl_id, H5_EVENT_QUEUE_NULL, new_gcpl_id)) < 0)
                    HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to get group information")
                break;
            }
        /* H5Gget_info */
        case H5VL_GROUP_GET_INFO:
            {
                H5VL_loc_params_t loc_params = va_arg (arguments, H5VL_loc_params_t);
                H5G_info_t  *grp_info = va_arg (arguments, H5G_info_t *);

                if((ret_value = H5VL_group_get(obj, vol_plugin, get_type, dxpl_id, H5_EVENT_QUEUE_NULL, loc_params, grp_info)) < 0)
                    HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to get group information")
                break;
            }
        default:
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't get this type of information from group")
    }

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLgroup_get() */


/*-------------------------------------------------------------------------
 * Function:	H5VLgroup_close
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
H5VLgroup_close(void *grp, H5VL_t *vol_plugin, hid_t dxpl_id, void UNUSED **req)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)

    if(NULL == grp || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")
    if((ret_value = H5VL_group_close(grp, vol_plugin, dxpl_id, H5_EVENT_QUEUE_NULL)) < 0)
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
                 H5VL_t *vol_plugin, hid_t lcpl_id, hid_t lapl_id, hid_t dxpl_id, void UNUSED **req)
{
    herr_t               ret_value = SUCCEED;  /* Return value */

    FUNC_ENTER_API(FAIL)

    if(NULL == obj || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")
    if((ret_value = H5VL_link_create(create_type, obj, loc_params, vol_plugin, lcpl_id, lapl_id, dxpl_id, H5_EVENT_QUEUE_NULL)) < 0)
	HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to create link")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLlink_create() */


/*-------------------------------------------------------------------------
 * Function:	H5VLlink_move
 *
 * Purpose:	Copy or move a link from src to dst.
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
                             hbool_t copy_flag, hid_t lcpl_id, hid_t lapl_id, hid_t dxpl_id, void UNUSED **req)
{
    herr_t               ret_value = SUCCEED;  /* Return value */

    FUNC_ENTER_API(FAIL)

    if(NULL == src_obj || NULL == dst_obj || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")
    if((ret_value = H5VL_link_move(src_obj, loc_params1, dst_obj, loc_params2, vol_plugin, 
                                   copy_flag, lcpl_id, lapl_id, dxpl_id, H5_EVENT_QUEUE_NULL)) < 0)
	HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to move object")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLlink_move() */


/*-------------------------------------------------------------------------
 * Function:	H5VLlink_iterate
 *
 * Purpose:	Iterate over links in a group
 *
 * Return:	Success:        non negative
 *		Failure:	negative
 *
 * Programmer:	Mohamad Chaarawi
 *              May, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t H5VLlink_iterate(void *obj, H5VL_loc_params_t loc_params, H5VL_t *vol_plugin, 
                         hbool_t recursive, H5_index_t idx_type, H5_iter_order_t order, hsize_t *idx, 
                         H5L_iterate_t op, void *op_data, hid_t dxpl_id, void UNUSED **req)
{
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)

    if(NULL == obj || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")
    if((ret_value = H5VL_link_iterate(obj, loc_params, vol_plugin, recursive, idx_type, order, idx,
                                      op, op_data, dxpl_id, H5_EVENT_QUEUE_NULL)) < 0)
	HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "Link iteration failed")

done:
    FUNC_LEAVE_API(ret_value)    
} /* end H5VLlink_iterate() */


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
              hid_t dxpl_id, void UNUSED **req, va_list arguments)
{
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)

    if(NULL == obj || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")

    switch (get_type) {
        /* H5Lexists */
        case H5VL_LINK_EXISTS:
            {
                htri_t     *ret    = va_arg (arguments, htri_t *);

                if((ret_value = H5VL_link_get(obj, loc_params, vol_plugin, get_type, dxpl_id, H5_EVENT_QUEUE_NULL, ret)) < 0)
                    HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to get link information")
                break;
            }
        /* H5Lget_info/H5Lget_info_by_idx */
        case H5VL_LINK_GET_INFO:
            {
                H5L_info_t *linfo  = va_arg (arguments, H5L_info_t *);

                if((ret_value = H5VL_link_get(obj, loc_params, vol_plugin, get_type, dxpl_id, H5_EVENT_QUEUE_NULL, linfo)) < 0)
                    HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to get link information")
                break;
            }
        /* H5Lget_name_by_idx */
        case H5VL_LINK_GET_NAME:
            {
                char       *name   = va_arg (arguments, char *);
                size_t      size   = va_arg (arguments, size_t);
                ssize_t    *ret    = va_arg (arguments, ssize_t *);

                if((ret_value = H5VL_link_get(obj, loc_params, vol_plugin, get_type, dxpl_id, H5_EVENT_QUEUE_NULL, 
                                              name, size, ret)) < 0)
                    HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to get link information")
                break;
            }
        /* H5Lget_val/H5Lget_val_by_idx */
        case H5VL_LINK_GET_VAL:
            {
                void       *buf    = va_arg (arguments, void *);
                size_t     size    = va_arg (arguments, size_t);

                if((ret_value = H5VL_link_get(obj, loc_params, vol_plugin, get_type, dxpl_id, H5_EVENT_QUEUE_NULL, buf, size)) < 0)
                    HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to get link information")
                break;
            }
        default:
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't get this type of information from link")
    }

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLlink_get() */


/*-------------------------------------------------------------------------
 * Function:	H5VLlink_remove
 *
 * Purpose:	Removes a link through the VOL.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Mohamad Chaarawi
 *              April, 2012
 *
 *-------------------------------------------------------------------------
 */
H5_DLL herr_t H5VLlink_remove(void *obj, H5VL_loc_params_t loc_params, H5VL_t *vol_plugin, hid_t dxpl_id, void UNUSED **req)
{
    herr_t             ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_API(FAIL)

    if(NULL == obj || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")
    if((ret_value = H5VL_link_remove(obj, loc_params, vol_plugin, dxpl_id, H5_EVENT_QUEUE_NULL)) < 0)
	HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to remove link")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLlink_remove() */


/*-------------------------------------------------------------------------
 * Function:	H5VLobject_open
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
H5VLobject_open(void *obj, H5VL_loc_params_t params, H5VL_t *vol_plugin, H5I_type_t *opened_type,
                hid_t dxpl_id, void UNUSED **req)
{
    void *ret_value;              /* Return value */

    FUNC_ENTER_API(NULL)

    if (NULL == obj || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "invalid object/VOL class pointer")
    if(NULL == (ret_value = H5VL_object_open(obj, params, vol_plugin, opened_type, dxpl_id, H5_EVENT_QUEUE_NULL)))
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
                hid_t ocpypl_id, hid_t lcpl_id, hid_t dxpl_id, void UNUSED **req)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)

    if(NULL == src_obj || NULL == dst_obj || NULL == vol_plugin1 || 
       NULL == vol_plugin2 || NULL == vol_plugin1->cls || NULL == vol_plugin2->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")
    if((ret_value = H5VL_object_copy(src_obj, loc_params1, vol_plugin1, src_name, 
                                     dst_obj, loc_params2, vol_plugin2, dst_name,
                                     ocpypl_id, lcpl_id, dxpl_id, H5_EVENT_QUEUE_NULL)) < 0)
	HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to move object")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLobject_copy() */


/*-------------------------------------------------------------------------
 * Function:	H5VLobject_visit
 *
 * Purpose:	Iterate over links in a group
 *
 * Return:	Success:        non negative
 *		Failure:	negative
 *
 * Programmer:	Mohamad Chaarawi
 *              May, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t H5VLobject_visit(void *obj, H5VL_loc_params_t loc_params, H5VL_t *vol_plugin, H5_index_t idx_type,
                         H5_iter_order_t order, H5O_iterate_t op, void *op_data, hid_t dxpl_id, void UNUSED **req)
{
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)

    if(NULL == obj || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")
    if((ret_value = H5VL_object_visit(obj, loc_params, vol_plugin, idx_type, order, 
                                      op, op_data, dxpl_id, H5_EVENT_QUEUE_NULL)) < 0)
	HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "Object Visit Failed")

done:
    FUNC_LEAVE_API(ret_value)    
} /* end H5VLobject_visit() */


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
               hid_t dxpl_id, void UNUSED **req, va_list arguments)
{
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)

    if(NULL == obj || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")

    switch (get_type) {
        /* H5Oexists_by_name */
        case H5VL_OBJECT_EXISTS:
            {
                htri_t	  *ret      = va_arg (arguments, htri_t *);

                if((ret_value = H5VL_object_get(obj, loc_params, vol_plugin, get_type, dxpl_id, H5_EVENT_QUEUE_NULL, ret)) < 0)
                    HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "Object Visit Failed")
                break;
            }
        /* H5Oget_info / H5Oget_info_by_name / H5Oget_info_by_idx */
        case H5VL_OBJECT_GET_INFO:
            {
                H5O_info_t  *obj_info = va_arg (arguments, H5O_info_t *);

                if((ret_value = H5VL_object_get(obj, loc_params, vol_plugin, get_type, dxpl_id, H5_EVENT_QUEUE_NULL, obj_info)) < 0)
                    HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "Object Visit Failed")
                break;
            }
        /* H5Oget_comment / H5Oget_comment_by_name */
        case H5VL_OBJECT_GET_COMMENT:
            {
                char     *comment =  va_arg (arguments, char *);
                size_t   bufsize  =  va_arg (arguments, size_t);
                ssize_t  *ret     =  va_arg (arguments, ssize_t *);

                if((ret_value = H5VL_object_get(obj, loc_params, vol_plugin, get_type, dxpl_id, H5_EVENT_QUEUE_NULL,
                                                comment, bufsize, ret)) < 0)
                    HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "Object Visit Failed")
                break;
            }
        /* H5Rget_region */
        case H5VL_REF_GET_REGION:
            {
                hid_t       *ret     =  va_arg (arguments, hid_t *);
                H5R_type_t  ref_type =  va_arg (arguments, H5R_type_t);
                void        *ref     =  va_arg (arguments, void *);

                if((ret_value = H5VL_object_get(obj, loc_params, vol_plugin, get_type, dxpl_id, H5_EVENT_QUEUE_NULL,
                                                ret, ref_type, ref)) < 0)
                    HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "Object Visit Failed")
                break;
            }
        /* H5Rget_obj_type2 */
        case H5VL_REF_GET_TYPE:
            {
                H5O_type_t  *obj_type  =  va_arg (arguments, H5O_type_t *);
                H5R_type_t  ref_type   =  va_arg (arguments, H5R_type_t);
                void        *ref       =  va_arg (arguments, void *);

                if((ret_value = H5VL_object_get(obj, loc_params, vol_plugin, get_type, dxpl_id, H5_EVENT_QUEUE_NULL,
                                                obj_type, ref_type, ref)) < 0)
                    HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "Object Visit Failed")
                break;
            }
        /* H5Rget_name */
        case H5VL_REF_GET_NAME:
            {
                ssize_t     *ret       = va_arg (arguments, ssize_t *);
                char        *name      = va_arg (arguments, char *);
                size_t      size       = va_arg (arguments, size_t);
                H5R_type_t  ref_type   = va_arg (arguments, H5R_type_t);
                void        *ref       = va_arg (arguments, void *);

                if((ret_value = H5VL_object_get(obj, loc_params, vol_plugin, get_type, dxpl_id, H5_EVENT_QUEUE_NULL,
                                                ret, name, size, ref_type, ref)) < 0)
                    HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "Object Visit Failed")
                break;
            }
        default:
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't get this type of information from object")
    }

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLobject_get() */


/*-------------------------------------------------------------------------
 * Function:	H5VLobject_misc
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
H5VLobject_misc(void *obj, H5VL_loc_params_t loc_params, H5VL_t *vol_plugin, H5VL_object_misc_t misc_type, 
                hid_t dxpl_id, void UNUSED **req, va_list arguments)
{
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)

    if(NULL == obj || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")

    switch (misc_type) {
        /* H5Arename/rename_by_name */
        case H5VL_ATTR_RENAME:
            {
                const char    *old_name  = va_arg (arguments, const char *);
                const char    *new_name  = va_arg (arguments, const char *);

                if((ret_value = H5VL_object_misc(obj, loc_params, vol_plugin, misc_type, dxpl_id, H5_EVENT_QUEUE_NULL,
                                                 old_name, new_name)) < 0)
                    HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "Object Visit Failed")
                break;
            }
        /* H5Oincr_refcount / H5Odecr_refcount */
        case H5VL_OBJECT_CHANGE_REF_COUNT:
            {
                int update_ref  = va_arg (arguments, int);

                if((ret_value = H5VL_object_misc(obj, loc_params, vol_plugin, misc_type, dxpl_id, H5_EVENT_QUEUE_NULL, update_ref)) < 0)
                    HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "Object Visit Failed")
                break;
            }
        /* H5Oset_comment */
        case H5VL_OBJECT_SET_COMMENT:
            {
                const char    *comment  = va_arg (arguments, char *);

                if((ret_value = H5VL_object_misc(obj, loc_params, vol_plugin, misc_type, dxpl_id, H5_EVENT_QUEUE_NULL, comment)) < 0)
                    HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "Object Visit Failed")
                break;
            }
        case H5VL_REF_CREATE:
            {
                void        *ref      = va_arg (arguments, void *);
                const char  *name     = va_arg (arguments, char *);
                H5R_type_t  ref_type  = va_arg (arguments, H5R_type_t);
                hid_t       space_id  = va_arg (arguments, hid_t);

                if((ret_value = H5VL_object_misc(obj, loc_params, vol_plugin, misc_type, dxpl_id, H5_EVENT_QUEUE_NULL,
                                                 ref, name, ref_type, space_id)) < 0)
                    HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "Object Visit Failed")
                break;
            }
        default:
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't recognize this operation type")
    }

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLobject_misc() */


/*-------------------------------------------------------------------------
 * Function:	H5VLobject_close
 *
 * Purpose:	Closes a object through the VOL
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
H5VLobject_close(void *obj, H5VL_loc_params_t loc_params, H5VL_t *vol_plugin, hid_t dxpl_id, void UNUSED **req)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)

    if(NULL == obj || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")
    if((ret_value = H5VL_object_close(obj, loc_params, vol_plugin, dxpl_id, H5_EVENT_QUEUE_NULL)) < 0)
	HGOTO_ERROR(H5E_VOL, H5E_CANTRELEASE, FAIL, "unable to close object")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLobject_close() */


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
H5VLrequest_cancel(void **req, H5VL_t *vol_plugin, H5_status_t *status)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)

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
H5VLrequest_test(void **req, H5VL_t *vol_plugin, H5_status_t *status)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)

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
H5VLrequest_wait(void **req, H5VL_t *vol_plugin, H5_status_t *status)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)

    if(NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid request/VOL class pointer")
    if((ret_value = H5VL_request_wait(req, vol_plugin, status)) < 0)
	HGOTO_ERROR(H5E_VOL, H5E_CANTRELEASE, FAIL, "unable to wait on request")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLrequest_wait() */
