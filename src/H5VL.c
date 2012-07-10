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
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5VLpkg.h"		/* VOL package header		  	*/
#include "H5VLprivate.h"	/* VOL          		  	*/

/********************/
/* Local Prototypes */
/********************/
static herr_t H5VL_free_cls(H5VL_class_t *cls);


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
    herr_t      ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* register VOL ID type */
        if(H5I_register_type(H5I_VOL, (size_t)H5I_VOL_HASHSIZE, 0, 
                              (H5I_free_t)H5VL_free_cls, NULL)<H5I_FILE)
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
    hid_t		ret_value;

    FUNC_ENTER_API(FAIL)
    H5TRACE1("i", "*x", cls);

    /* Check arguments */
    if(!cls)
	HGOTO_ERROR(H5E_ARGS, H5E_UNINITIALIZED, FAIL, "null class pointer is disallowed")
    /* MSC - check if required callback are defined */

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
    herr_t      ret_value = SUCCEED;       /* Return value */

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
    H5TRACE1("t", "Ll", id);

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
    H5TRACE3("i", "*x", id, name, size);

    if((ret_value = H5VL_get_plugin_name(id, name, size)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "Can't get plugin name")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLget_plugin_name() */


/*---------------------------------------------------------------------------
 * Function:	H5VLregister_object
 *
 * Purpose:	utility routine to register the native VOL plugin to an ID
 *
 * Returns:     Non-negative on success or negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              June, 2012
 *
 *---------------------------------------------------------------------------
 */
hid_t
H5VLregister_object(void *obj, H5I_type_t obj_type, const H5VL_class_t *cls)
{
    H5VL_t  *vol_plugin;        /* VOL plugin information */
    hid_t ret_value = FAIL;

    FUNC_ENTER_API(FAIL)

    /* Get an atom for the object */
    if((ret_value = H5I_register(obj_type, obj, TRUE)) < 0)
	HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to atomize object")

    /* Build the vol plugin struct */
    if(NULL == (vol_plugin = (H5VL_t *)H5MM_calloc(sizeof(H5VL_t))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")
    vol_plugin->cls = cls;
    vol_plugin->nrefs = 1;

    switch(obj_type) {
        case H5I_FILE:
            if (H5I_register_aux(ret_value, vol_plugin, (H5I_free2_t)H5F_close_file) < 0)
                HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "can't attach vol info to ID")
            break;
        case H5I_GROUP:
            if (H5I_register_aux(ret_value, vol_plugin, (H5I_free2_t)H5G_close_group) < 0)
                HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "can't attach vol info to ID")
            break;
        case H5I_DATATYPE:
            if (H5I_register_aux(ret_value, vol_plugin, (H5I_free2_t)H5T_close_datatype) < 0)
                HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "can't attach vol info to ID")
            break;
        case H5I_DATASET:
            if (H5I_register_aux(ret_value, vol_plugin, (H5I_free2_t)H5D_close_dataset) < 0)
                HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "can't attach vol info to ID")
            break;
        case H5I_ATTR:
            if (H5I_register_aux(ret_value, vol_plugin, (H5I_free2_t)H5A_close_attr) < 0)
                HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "can't attach vol info to ID")
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
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file or file object")
    } /* end switch */
done:
    FUNC_LEAVE_API(ret_value)
} /* H5VLregister_object */


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
                hid_t acpl_id, hid_t aapl_id, hid_t req)
{
    void *ret_value = NULL;  /* Return value */

    FUNC_ENTER_API(NULL)

    if (NULL == obj || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "invalid object/VOL class pointer")
    if(NULL == (ret_value = H5VL_attr_create(obj, loc_params, vol_plugin, name, acpl_id, aapl_id, req)))
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
              hid_t aapl_id, hid_t req)
{
    void *ret_value;  /* Return value */

    FUNC_ENTER_API(NULL)

    if (NULL == obj || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "invalid object/VOL class pointer")
    if(NULL == (ret_value = H5VL_attr_open(obj, loc_params, vol_plugin, name, aapl_id, req)))
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
herr_t H5VLattr_read(void *attr, H5VL_t *vol_plugin, hid_t mem_type_id, void *buf, hid_t req)
{
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)

    if (NULL == attr || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")
    if((ret_value = H5VL_attr_read(attr, vol_plugin, mem_type_id, buf, req)) < 0)
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
herr_t H5VLattr_write(void *attr, H5VL_t *vol_plugin, hid_t mem_type_id, const void *buf, hid_t req)
{
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)

    if (NULL == attr || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")
    if((ret_value = H5VL_attr_write(attr, vol_plugin, mem_type_id, buf, req)) < 0)
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
 *
 *		Failure:	negative
 *
 * Programmer:	Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLattr_get(void *obj, H5VL_t *vol_plugin, H5VL_attr_get_t get_type, hid_t req, ...)
{
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)

    if (NULL == obj || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")
    if((ret_value = H5VL_attr_get(obj, vol_plugin, get_type, req)) < 0)
	HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to get attribute information")

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
                 const char *attr_name, hid_t req)
{
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)

    if (NULL == obj || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")
    if((ret_value = H5VL_attr_remove(obj, loc_params, vol_plugin, attr_name, req)) < 0)
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
H5VLattr_close(void *attr, H5VL_t *vol_plugin, hid_t req)
{
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)

    if (NULL == attr || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")
    if((ret_value = H5VL_attr_close(attr, vol_plugin, req)) < 0)
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
                     hid_t type_id, hid_t lcpl_id, hid_t tcpl_id, hid_t tapl_id, hid_t req)
{
    void *ret_value = NULL;              /* Return value */

    FUNC_ENTER_API(NULL)

    if (NULL == obj || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "invalid object/VOL class pointer")
    if(NULL == (ret_value = H5VL_datatype_commit(obj, loc_params, vol_plugin, name, type_id, 
                                                 lcpl_id, tcpl_id, tapl_id, req)))
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
                   hid_t tapl_id, hid_t req)
{
    void *ret_value = NULL;              /* Return value */

    FUNC_ENTER_API(NULL)

    if (NULL == obj || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "invalid object/VOL class pointer")
    if(NULL == (ret_value = H5VL_datatype_open(obj, loc_params, vol_plugin, name, tapl_id, req)))
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
H5VLdatatype_get_binary(void *obj, H5VL_t *vol_plugin, unsigned char *buf, size_t size, hid_t req)
{
    ssize_t ret_value = FAIL;

    FUNC_ENTER_API(FAIL)

    if (NULL == obj || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")
    if((ret_value = H5VL_datatype_get_binary(obj, vol_plugin, buf, size, req)))
	HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to encode datatype")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLdatatype_get_binary() */


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
H5VLdatatype_close(void *dt, H5VL_t *vol_plugin, hid_t req)
{
    herr_t		ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_API(FAIL)

    if (NULL == dt || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")
    if((ret_value = H5VL_datatype_close(dt, vol_plugin, req)) < 0)
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
                    hid_t dcpl_id, hid_t dapl_id, hid_t req)
{
    void *ret_value; /* Return value */

    FUNC_ENTER_API(NULL)

    if (NULL == obj || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "invalid object/VOL class pointer")
    if(NULL == (ret_value = H5VL_dataset_create(obj, loc_params, vol_plugin, name, 
                                                dcpl_id, dapl_id, req)))
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
                  hid_t dapl_id, hid_t req)
{
    void *ret_value; /* Return value */

    FUNC_ENTER_API(NULL)

    if (NULL == obj || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "invalid object/VOL class pointer")
    if(NULL == (ret_value = H5VL_dataset_open(obj, loc_params, vol_plugin, name, dapl_id, req)))
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
                  hid_t file_space_id, hid_t plist_id, void *buf, hid_t req)
{
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)

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
                   hid_t file_space_id, hid_t plist_id, const void *buf, hid_t req)
{
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)

    if (NULL == dset || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")
    if((ret_value = H5VL_dataset_write(dset, vol_plugin, mem_type_id, mem_space_id, file_space_id, 
                                       plist_id, buf, req)) < 0)
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
H5VLdataset_set_extent(void *dset, H5VL_t *vol_plugin, const hsize_t size[], hid_t req)
{
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)

    if (NULL == dset || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")
    if((ret_value = H5VL_dataset_set_extent(dset, vol_plugin, size, req)) < 0)
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
H5VLdataset_get(void *dset, H5VL_t *vol_plugin, H5VL_dataset_get_t get_type, hid_t req, ...)
{
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)

    if (NULL == dset || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")
    if((ret_value = H5VL_dataset_get(dset, vol_plugin, get_type, req)) < 0)
	HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to get dataset information")

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
H5VLdataset_close(void *dset, H5VL_t *vol_plugin, hid_t req)
{
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)

    if (NULL == dset || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")
    if((ret_value = H5VL_dataset_close(dset, vol_plugin, req)) < 0)
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
                hid_t fapl_id, hid_t req)
{
    void	       *ret_value;             /* Return value */

    FUNC_ENTER_API(NULL)

    if(NULL == (ret_value = H5VL_file_create(vol_plugin, name, flags, fcpl_id, fapl_id, req)))
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
H5VLfile_open(H5VL_t **vol_plugin, const char *name, unsigned flags, hid_t fapl_id, hid_t req)
{
    void	       *ret_value;             /* Return value */

    FUNC_ENTER_API(NULL)

    if(NULL == (ret_value = H5VL_file_open(vol_plugin, name, flags, fapl_id, req)))
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
               H5F_scope_t scope, hid_t req)
{
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)

    if (NULL == obj || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")
    if((ret_value = H5VL_file_flush(obj, loc_params, vol_plugin, scope, req)) < 0)
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
H5VLfile_get(void *file, H5VL_t *vol_plugin, H5VL_file_get_t get_type, hid_t req, ...)
{
    va_list           arguments;             /* argument list passed from the API call */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)

    if(NULL == file || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")
    if((ret_value = H5VL_file_get(file, vol_plugin, get_type, req)) < 0)
	HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to get file information")

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
H5VLfile_misc(void *file, H5VL_t *vol_plugin, H5VL_file_misc_t misc_type, hid_t req, ...)
{
    va_list           arguments;             /* argument list passed from the API call */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)

    if(NULL == file || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")
    if((ret_value = H5VL_file_misc(file, vol_plugin, misc_type, req)) < 0)
	HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to operate on the file")

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
H5VLfile_optional(void *file, H5VL_t *vol_plugin, H5VL_file_optional_t optional_type, hid_t req, ...)
{
    va_list           arguments;             /* argument list passed from the API call */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)

    if(NULL == file || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")
    if((ret_value = H5VL_file_optional(file, vol_plugin, optional_type, req)) < 0)
	HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to operate on the file")

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
H5VLfile_close(void *file, H5VL_t *vol_plugin, hid_t req)
{
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)

    if(NULL == file || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")
    if((ret_value = H5VL_file_close(file, vol_plugin, req)) < 0)
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
                 hid_t gcpl_id, hid_t gapl_id, hid_t req)
{
    void *ret_value = NULL; /* Return value */

    FUNC_ENTER_API(NULL)

    if (NULL == obj || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "invalid object/VOL class pointer")
    if(NULL == (ret_value = H5VL_group_create(obj, loc_params, vol_plugin, name, 
                                              gcpl_id, gapl_id, req)))
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
                hid_t gapl_id, hid_t req)
{
    void *ret_value; /* Return value */

    FUNC_ENTER_API(NULL)

    if (NULL == obj || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "invalid object/VOL class pointer")
    if(NULL == (ret_value = H5VL_group_open(obj, loc_params, vol_plugin, name, 
                                              gapl_id, req)))
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
H5VLgroup_get(void *obj, H5VL_t *vol_plugin, H5VL_group_get_t get_type, hid_t req, ...)
{
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)

    if(NULL == obj || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")
    if((ret_value = H5VL_group_get(obj, vol_plugin, get_type, req)) < 0)
	HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to get group information")

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
H5VLgroup_close(void *grp, H5VL_t *vol_plugin, hid_t req)
{
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)

    if(NULL == grp || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")
    if((ret_value = H5VL_group_close(grp, vol_plugin, req)) < 0)
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
                 H5VL_t *vol_plugin, hid_t lcpl_id, hid_t lapl_id, hid_t req)
{
    herr_t               ret_value = SUCCEED;  /* Return value */

    FUNC_ENTER_API(FAIL)

    if(NULL == obj || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")
    if((ret_value = H5VL_link_create(create_type, obj, loc_params, vol_plugin, lcpl_id, lapl_id, req)) < 0)
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
                             hbool_t copy_flag, hid_t lcpl_id, hid_t lapl_id, hid_t req)
{
    herr_t               ret_value = SUCCEED;  /* Return value */

    FUNC_ENTER_API(FAIL)

    if(NULL == src_obj || NULL == dst_obj || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")
    if((ret_value = H5VL_link_move(src_obj, loc_params1, dst_obj, loc_params2, vol_plugin, 
                                   copy_flag, lcpl_id, lapl_id, req)) < 0)
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
                         H5L_iterate_t op, void *op_data, hid_t req)
{
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)

    if(NULL == obj || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")
    if((ret_value = H5VL_link_iterate(obj, loc_params, vol_plugin, recursive, idx_type, order, idx,
                                      op, op_data, req)) < 0)
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
              hid_t req, ...)
{
    va_list           arguments;             /* argument list passed from the API call */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)

    if(NULL == obj || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")
    if((ret_value = H5VL_link_get(obj, loc_params, vol_plugin, get_type, req)) < 0)
	HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to get link information")

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
H5_DLL herr_t H5VLlink_remove(void *obj, H5VL_loc_params_t loc_params, H5VL_t *vol_plugin, hid_t req)
{
    herr_t             ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_API(FAIL)

    if(NULL == obj || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")
    if((ret_value = H5VL_link_remove(obj, loc_params, vol_plugin, req)) < 0)
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
                hid_t req)
{
    void *ret_value;              /* Return value */

    FUNC_ENTER_API(NULL)

    if (NULL == obj || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "invalid object/VOL class pointer")
    if(NULL == (ret_value = H5VL_object_open(obj, params, vol_plugin, opened_type, req)))
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
                hid_t ocpypl_id, hid_t lcpl_id, hid_t req)
{
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)

    if(NULL == src_obj || NULL == dst_obj || NULL == vol_plugin1 || 
       NULL == vol_plugin2 || NULL == vol_plugin1->cls || NULL == vol_plugin2->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")
    if((ret_value = H5VL_object_copy(src_obj, loc_params1, vol_plugin1, src_name, 
                                     dst_obj, loc_params2, vol_plugin2, dst_name,
                                     ocpypl_id, lcpl_id, req)) < 0)
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
                         H5_iter_order_t order, H5O_iterate_t op, void *op_data, hid_t req)
{
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)

    if(NULL == obj || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")
    if((ret_value = H5VL_object_visit(obj, loc_params, vol_plugin, idx_type, order, 
                                      op, op_data, req)) < 0)
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
               hid_t req, ...)
{
    va_list           arguments;             /* argument list passed from the API call */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)

    if(NULL == obj || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")
    if((ret_value = H5VL_object_get(obj, loc_params, vol_plugin, get_type, req)) < 0)
	HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "Object Visit Failed")

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
                hid_t req, ...)
{
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)

    if(NULL == obj || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")
    if((ret_value = H5VL_object_misc(obj, loc_params, vol_plugin, misc_type, req)) < 0)
	HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "Object Visit Failed")

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
H5VLobject_close(void *obj, H5VL_loc_params_t loc_params, H5VL_t *vol_plugin, hid_t req)
{
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)

    if(NULL == obj || NULL == vol_plugin || NULL == vol_plugin->cls)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object/VOL class pointer")
    if((ret_value = H5VL_object_close(obj, loc_params, vol_plugin, req)) < 0)
	HGOTO_ERROR(H5E_VOL, H5E_CANTRELEASE, FAIL, "unable to close object")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLobject_close() */
