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
#define H5_INTERFACE_INIT_FUNC	H5VL_init_interface

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

/********************/
/* Local Prototypes */
/********************/
static herr_t H5VL_free_cls(H5VL_class_t *cls);
static herr_t H5VL_free_id_wrapper(H5VL_id_wrapper_t *id_struct);


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
    if(H5I_register_type(H5I_VOL, (size_t)H5I_VOL_HASHSIZE, 0, (H5I_free_t)H5VL_free_cls)<H5I_FILE)
	HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to initialize interface")

    /* Register high level file user id */ 
    if(H5I_register_type(H5I_FILE_PUBLIC, (size_t)H5I_FILE_PUBLIC_HASHSIZE, 0, (H5I_free_t)H5VL_free_id_wrapper)<H5I_FILE)
	HGOTO_ERROR(H5E_ATOM, H5E_CANTINIT, FAIL, "unable to initialize interface")
    /* Register high level group user id */ 
    if(H5I_register_type(H5I_GROUP_PUBLIC, (size_t)H5I_GROUP_PUBLIC_HASHSIZE, 0, (H5I_free_t)H5VL_free_id_wrapper)<H5I_FILE)
	HGOTO_ERROR(H5E_ATOM, H5E_CANTINIT, FAIL, "unable to initialize interface")
    /* Register high level dataset user id */ 
    if(H5I_register_type(H5I_DATASET_PUBLIC, (size_t)H5I_DATASET_PUBLIC_HASHSIZE, 0, (H5I_free_t)H5VL_free_id_wrapper)<H5I_FILE)
	HGOTO_ERROR(H5E_ATOM, H5E_CANTINIT, FAIL, "unable to initialize interface")
    /* Register high level attribute user id */ 
    if(H5I_register_type(H5I_ATTR_PUBLIC, (size_t)H5I_ATTR_PUBLIC_HASHSIZE, 0, (H5I_free_t)H5VL_free_id_wrapper)<H5I_FILE)
	HGOTO_ERROR(H5E_ATOM, H5E_CANTINIT, FAIL, "unable to initialize interface")
    /* Register high level datatype user id */ 
    if(H5I_register_type(H5I_DATATYPE_PUBLIC, (size_t)H5I_DATATYPE_PUBLIC_HASHSIZE, 0, (H5I_free_t)H5VL_free_id_wrapper)<H5I_FILE)
	HGOTO_ERROR(H5E_ATOM, H5E_CANTINIT, FAIL, "unable to initialize interface")

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
    int	n = 0, n1 = 0, n2 = 0, n3 = 0, n4 = 0, n5 = 0, n6 = 0;
    hbool_t term = TRUE;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    if(H5_interface_initialize_g) {
	if((n1=H5I_nmembers(H5I_VOL))!=0) {
	    H5I_clear_type(H5I_VOL, FALSE, FALSE);
            term = FALSE;
	} else {
	    H5I_dec_type_ref(H5I_VOL);
	}

	if((n2=H5I_nmembers(H5I_FILE_PUBLIC))!=0) {
	    H5I_clear_type(H5I_FILE_PUBLIC, TRUE, FALSE);
            term = FALSE;
	} else {
	    H5I_dec_type_ref(H5I_FILE_PUBLIC);
	}

	if((n3=H5I_nmembers(H5I_DATASET_PUBLIC))!=0) {
	    H5I_clear_type(H5I_DATASET_PUBLIC, FALSE, FALSE);
            term = FALSE;
	} else {
	    H5I_dec_type_ref(H5I_DATASET_PUBLIC);
	}

	if((n4=H5I_nmembers(H5I_DATATYPE_PUBLIC))!=0) {
	    H5I_clear_type(H5I_DATATYPE_PUBLIC, FALSE, FALSE);
            term = FALSE;
	} else {
	    H5I_dec_type_ref(H5I_DATATYPE_PUBLIC);
	}

	if((n5=H5I_nmembers(H5I_GROUP_PUBLIC))!=0) {
	    H5I_clear_type(H5I_GROUP_PUBLIC, FALSE, FALSE);
            term = FALSE;
	} else {
	    H5I_dec_type_ref(H5I_GROUP_PUBLIC);
	}

	if((n6=H5I_nmembers(H5I_ATTR_PUBLIC))!=0) {
	    H5I_clear_type(H5I_ATTR_PUBLIC, FALSE, FALSE);
            term = FALSE;
	} else {
	    H5I_dec_type_ref(H5I_ATTR_PUBLIC);
	}

        if (term) {
            H5_interface_initialize_g = 0;
            n = 1;
        }
        else {
            n = n1 + n2 + n3 + n4 + n5 + n6;
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

    H5MM_xfree(cls);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_free_cls() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_free_id_wrapper
 *
 * Purpose:	Frees the structure of a user level ID
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Mohamad Chaarawi
 *              March, 2012
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_free_id_wrapper(H5VL_id_wrapper_t *id_struct)
{
    int               ref_count;
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* Sanity check */
    HDassert(id_struct);

    /* If we are freeing the id wrapper, then we need to call the free
       callback on the object ID in this wrapper, so just keep
       decrementing the ref count on the object id till it gets to 0
       and calls its free function */
    ref_count = H5I_get_ref(id_struct->obj_id, TRUE);
    while (ref_count > 0) {
        if((ref_count = H5I_dec_app_ref(id_struct->obj_id)) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "can't free")
    }

    /* decrement the number of reference on the VOL plugin */
    id_struct->vol_plugin->nrefs --;

    /* free the ID wrapper */
    H5MM_xfree(id_struct);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_free_id_wrapper() */


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
    /*MSC - check required funciton pointers */

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
 * Function:	H5VL_get_class
 *
 * Purpose:	Obtains a pointer to the vol plugin struct containing all the
 *		callback pointers, etc. The PLIST_ID argument can be a file
 *		access property list or a vol plugin identifier.
 *
 * Return:	Success:	Ptr to the vol plugin information. The pointer is
 *				only valid as long as the vol plugin remains
 *				registered or some file or property list
 *				exists which references the vol plugin.
 *
 *		Failure:	NULL
 *
 * Programmer:	Mohamad Chaarawi
 *              January, 2012
 *
 *-------------------------------------------------------------------------
 */
H5VL_class_t *
H5VL_get_class(hid_t id)
{
    H5VL_class_t	*ret_value = NULL;

    FUNC_ENTER_NOAPI(NULL)

    if(H5I_VOL == H5I_get_type(id))
	ret_value = (H5VL_class_t *)H5I_object(id);
    else {
        H5P_genplist_t *plist;      /* Property list pointer */

        /* Get the plist structure */
        if(NULL == (plist = (H5P_genplist_t *)H5I_object(id)))
            HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, NULL, "can't find object for ID")

        if(TRUE == H5P_isa_class(id, H5P_FILE_ACCESS)) {
            if(H5P_get(plist, H5F_ACS_VOL_NAME, &ret_value) < 0)
                HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get vol plugin")
        } else {
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a vol plugin id, file access property list")
        }
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_get_class() */


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
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_fapl_open(H5P_genplist_t *plist, H5VL_class_t *vol_cls)
{
    herr_t ret_value = SUCCEED;     /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Increment the reference count on vol and copy vol info */
    vol_cls->nrefs++;

    /* Set the vol properties for the list */
    if(H5P_set(plist, H5F_ACS_VOL_NAME, &vol_cls) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTSET, FAIL, "can't set vol ID")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_fapl_open() */


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
H5VL_fapl_close(H5VL_class_t *vol_cls)
{
    herr_t      ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    if(NULL != vol_cls) {
        vol_cls->nrefs--;
        //H5MM_xfree(vol_cls);
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_fapl_close() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_attr_create
 *
 * Purpose:	Creates an attribute through the VOL
 *
 * Return:      Success: User ID of the new attr. This ID is of type
 *                       H5I_ATTR_PUBLIC which contains the VOL plugin and the actual attr ID
 *
 *		Failure: FAIL
 *
 * Programmer:	Mohamad Chaarawi
 *              April, 2012
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5VL_attr_create(hid_t uid, const char *name, hid_t acpl_id, hid_t aapl_id)
{
    H5VL_id_wrapper_t    *id_wrapper1;             /* user id structure of the location where the attr will be created */
    H5VL_id_wrapper_t    *id_wrapper2;             /* user id structure of new created attr*/
    H5I_type_t           id_type;
    hid_t                loc_id;            /* actual attr ID */
    hid_t                ret_value;             /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    id_type = H5I_get_type(uid);
    /* Check id */
    if(H5I_FILE_PUBLIC != id_type && H5I_GROUP_PUBLIC != id_type &&
       H5I_DATASET_PUBLIC != id_type && H5I_DATATYPE_PUBLIC != id_type)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a user ID")

    /* get the ID struct */
    if(NULL == (id_wrapper1 = (H5VL_id_wrapper_t *)H5I_object(uid)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid user identifier")

    /* check if the corresponding VOL create callback exists */
    if(NULL == id_wrapper1->vol_plugin->attr_cls.create)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `attr create' method")

    /* call the corresponding VOL create callback */
    if((loc_id = (id_wrapper1->vol_plugin->attr_cls.create)
        (id_wrapper1->obj_id, name, acpl_id, aapl_id)) < 0)
	HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "create failed")

    /* Create a new id that points to a struct that holds the attr id and the VOL plugin */
    /* Allocate new id structure */
    if(NULL == (id_wrapper2 = (H5VL_id_wrapper_t *)H5MM_malloc(sizeof(H5VL_id_wrapper_t))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")
    id_wrapper2->obj_id = loc_id;
    id_wrapper2->vol_plugin = id_wrapper1->vol_plugin;

    if((ret_value = H5I_register(H5I_ATTR_PUBLIC, id_wrapper2, TRUE)) < 0)
	HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to atomize attr handle")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_attr_create() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_attr_open
 *
 * Purpose:	Opens an attribute through the VOL
 *
 * Return:      Success: User ID of the new attr. This ID is of type
 *                       H5I_ATTR_PUBLIC which contains the VOL plugin and the actual attr ID
 *
 *		Failure: FAIL
 *
 * Programmer:	Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5VL_attr_open(hid_t loc_id, void *location, const char *name, hid_t aapl_id)
{
    H5VL_id_wrapper_t   *id_wrapper1;             /* user id structure of the location where the attr will be opend */
    H5VL_id_wrapper_t   *id_wrapper2;             /* user id structure of new opend attr*/
    H5I_type_t          id_type;
    hid_t               attr_id;               /* actual attr ID */
    hid_t		ret_value;              /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    id_type = H5I_get_type(loc_id);
    /* Check id */
    if(H5I_FILE_PUBLIC != id_type && H5I_GROUP_PUBLIC != id_type &&
       H5I_DATASET_PUBLIC != id_type && H5I_DATATYPE_PUBLIC !=  id_type &&
       H5I_ATTR_PUBLIC != id_type)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a user ID")

    /* get the ID struct */
    if(NULL == (id_wrapper1 = (H5VL_id_wrapper_t *)H5I_object(loc_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid user identifier")

    /* check if the type specific corresponding VOL open callback exists */
    if(NULL == id_wrapper1->vol_plugin->attr_cls.open) {
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `attr open' method")
    }

    /* call the corresponding VOL open callback */
    if((attr_id = (id_wrapper1->vol_plugin->attr_cls.open)
        (id_wrapper1->obj_id, location, name, aapl_id)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "open failed")

    /* Create a new id that points to a struct that holds the attr id and the VOL plugin */
    /* Allocate new id structure */
    if(NULL == (id_wrapper2 = (H5VL_id_wrapper_t *)H5MM_malloc(sizeof(H5VL_id_wrapper_t))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")
    id_wrapper2->obj_id = attr_id;
    id_wrapper2->vol_plugin = id_wrapper1->vol_plugin;

    if((ret_value = H5I_register(H5I_ATTR_PUBLIC, id_wrapper2, TRUE)) < 0)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to atomize attr handle")

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
herr_t H5VL_attr_read(hid_t uid, hid_t mem_type_id, void *buf)
{
    H5VL_id_wrapper_t   *id_wrapper;              /* user id structure */
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    /* Check/fix arguments. */
    if(H5I_ATTR_PUBLIC != H5I_get_type(uid))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a user ID")

    /* get the ID struct */
    if(NULL == (id_wrapper = (H5VL_id_wrapper_t *)H5I_object(uid)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid user identifier")

    if(NULL == id_wrapper->vol_plugin->attr_cls.read)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `attr read' method")
    if((ret_value = (id_wrapper->vol_plugin->attr_cls.read)
        (id_wrapper->obj_id, mem_type_id, buf)) < 0)
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
herr_t H5VL_attr_write(hid_t uid, hid_t mem_type_id, const void *buf)
{
    H5VL_id_wrapper_t   *id_wrapper;              /* user id structure */
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    /* Check/fix arguments. */
    if(H5I_ATTR_PUBLIC != H5I_get_type(uid))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a user ID")

    /* get the ID struct */
    if(NULL == (id_wrapper = (H5VL_id_wrapper_t *)H5I_object(uid)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid user identifier")

    if(NULL == id_wrapper->vol_plugin->attr_cls.write)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `attr write' method")
    if((ret_value = (id_wrapper->vol_plugin->attr_cls.write)
        (id_wrapper->obj_id, mem_type_id, buf)) < 0)
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
H5VL_attr_get(hid_t uid, H5VL_attr_get_t get_type, ...)
{
    H5VL_id_wrapper_t *id_wrapper;              /* user id structure */
    va_list           arguments;             /* argument list passed from the API call */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    /* Check id */
    if(H5I_ATTR_PUBLIC != H5I_get_type(uid))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a user ID")

    /* get the ID struct */
    if(NULL == (id_wrapper = (H5VL_id_wrapper_t *)H5I_object(uid)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid user identifier")

    if(NULL == id_wrapper->vol_plugin->attr_cls.get)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `attr get' method")

    va_start (arguments, get_type);
    if((ret_value = (id_wrapper->vol_plugin->attr_cls.get)(id_wrapper->obj_id, get_type, 
                                                           arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "get failed")
    va_end (arguments);

    /* if the get_type is a named datatype, create a wrapper for it */
    if(H5VL_ATTR_GET_TYPE == get_type) {
        H5VL_id_wrapper_t *temp_id_wrapper;              /* user id structure */
        hid_t	        *ret_id;

        va_start (arguments, get_type);
        ret_id = va_arg (arguments, hid_t *);

        if(H5T_committed((H5T_t *)H5I_object_verify(*ret_id, H5I_DATATYPE))) {
            /* Create a new id that points to a struct that holds the attr id and the VOL plugin */
            /* Allocate new id structure */
            if(NULL == (temp_id_wrapper = (H5VL_id_wrapper_t *)H5MM_malloc(sizeof(H5VL_id_wrapper_t))))
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")
            temp_id_wrapper->obj_id = *ret_id;
            temp_id_wrapper->vol_plugin = id_wrapper->vol_plugin;

            if((*ret_id = H5I_register(H5I_DATATYPE_PUBLIC, temp_id_wrapper, TRUE)) < 0)
                HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to atomize attr handle")
        }
        va_end (arguments);
    }
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_attr_get() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_attr_generic
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
H5VL_attr_generic(hid_t uid, H5VL_attr_generic_t generic_type, ...)
{
    H5VL_id_wrapper_t *id_wrapper;              /* user id structure */
    va_list           arguments;             /* argument list passed from the API call */
    H5I_type_t        id_type;
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    id_type = H5I_get_type(uid);
    /* Check id */
    if(H5I_FILE_PUBLIC != id_type && H5I_GROUP_PUBLIC != id_type &&
       H5I_DATASET_PUBLIC != id_type && H5I_DATATYPE_PUBLIC !=  id_type &&
       H5I_ATTR_PUBLIC != id_type)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a user ID")

    /* get the ID struct */
    if(NULL == (id_wrapper = (H5VL_id_wrapper_t *)H5I_object(uid)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid user identifier")

    if(NULL == id_wrapper->vol_plugin->attr_cls.generic)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `attr generic' method")

    va_start (arguments, generic_type);
    if((ret_value = (id_wrapper->vol_plugin->attr_cls.generic)(id_wrapper->obj_id, generic_type, 
                                                               arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "generic failed")
    va_end (arguments);

    if(H5VL_ATTR_OPEN_BY_IDX == generic_type) {
        H5VL_id_wrapper_t *temp_id_wrapper;              /* user id structure */
        hid_t	        *ret_id;

        va_start (arguments, generic_type);
        ret_id = va_arg (arguments, hid_t *);

        /* Create a new id that points to a struct that holds the attr id and the VOL plugin */
        /* Allocate new id structure */
        if(NULL == (temp_id_wrapper = (H5VL_id_wrapper_t *)H5MM_malloc(sizeof(H5VL_id_wrapper_t))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")
        temp_id_wrapper->obj_id = *ret_id;
        temp_id_wrapper->vol_plugin = id_wrapper->vol_plugin;

        if((*ret_id = H5I_register(H5I_ATTR_PUBLIC, temp_id_wrapper, TRUE)) < 0)
            HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to atomize attr handle")
        va_end (arguments);
    }
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_attr_generic() */


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
H5VL_attr_close(hid_t uid)
{
    H5VL_id_wrapper_t   *id_wrapper;              /* user id structure */
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    /* Check/fix arguments. */
    if(H5I_ATTR_PUBLIC != H5I_get_type(uid))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a user ID")

    /* get the ID struct */
    if(NULL == (id_wrapper = (H5VL_id_wrapper_t *)H5I_object(uid)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid user identifier")
            
    /* if the VOL class does not implement a specific attr close
       callback, try the generic object close */    
    if(NULL == id_wrapper->vol_plugin->attr_cls.close){
        if(H5VL_object_close(uid) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTRELEASE, FAIL, "unable to close object")
    }
    else {
        if((ret_value = (id_wrapper->vol_plugin->attr_cls.close)(id_wrapper->obj_id)) < 0)
            HGOTO_ERROR(H5E_VOL, H5E_CANTRELEASE, FAIL, "close failed")

        id_wrapper->vol_plugin->nrefs--;
        if(H5I_dec_app_ref(uid) < 0)
            HGOTO_ERROR(H5E_VOL, H5E_CANTDEC, FAIL, "unable to decrement ref count on user ID")
    }
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
herr_t
H5VL_datatype_commit(hid_t uid, const char *name, hid_t type_id, hid_t lcpl_id, 
                     hid_t tcpl_id, hid_t tapl_id)
{
    H5VL_id_wrapper_t   *id_wrapper1;             /* wrapper object of the location where the datatype will be commitd */
    herr_t		ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* get the ID struct */
    if(NULL == (id_wrapper1 = (H5VL_id_wrapper_t *)H5I_object(uid)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid user identifier")

    /* check if the corresponding VOL commit callback exists */
    if(NULL == id_wrapper1->vol_plugin->datatype_cls.commit)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `datatype commit' method")

    /* call the corresponding VOL commit callback */
    if((ret_value = (id_wrapper1->vol_plugin->datatype_cls.commit)
        (id_wrapper1->obj_id, name, type_id, lcpl_id, tcpl_id, tapl_id)) < 0)
	HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "commit failed")

#if 0
    /* Create a new id that points to a struct that holds the datatype id and the VOL plugin */
    /* Allocate new id structure */
    if(NULL == (id_wrapper2 = (H5VL_id_wrapper_t *)H5MM_malloc(sizeof(H5VL_id_wrapper_t))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")

    if((id_wrapper2->obj_id = H5I_register(H5I_DATATYPE, H5I_object(type_id), TRUE)) < 0)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to atomize datatype handle")
    id_wrapper2->vol_plugin = id_wrapper1->vol_plugin;

    H5I_subst(type_id, id_wrapper2);
    //H5I_remove_verify(type_id, H5I_DATATYPE);
    
#endif

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_datatype_commit() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_datatype_open
 *
 * Purpose:	Opens a named datatype through the VOL
 *
 * Return:      Success: User ID of the datatype. This ID is of type
 *                       H5I_DATATYPE_PUBLIC which contains the VOL plugin and the actual datatype ID
 *
 *		Failure: FAIL
 *
 * Programmer:	Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5VL_datatype_open(hid_t loc_id, const char *name, hid_t tapl_id)
{
    H5VL_id_wrapper_t   *id_wrapper1;             /* user id structure of the location where the datatype will be opend */
    H5VL_id_wrapper_t   *id_wrapper2;             /* user id structure of new opend datatype*/
    hid_t               datatype_id;               /* actual datatype ID */
    hid_t		ret_value;              /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* get the ID struct */
    if(NULL == (id_wrapper1 = (H5VL_id_wrapper_t *)H5I_object(loc_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid user identifier")

    /* check if the type specific corresponding VOL open callback exists */
    if(NULL == id_wrapper1->vol_plugin->datatype_cls.open) {
        void       *location = NULL; /* a pointer to VOL specific token that indicates 
                                        the location of the object */

        /* Get the token for the Object location through the VOL */
        if(H5VL_object_lookup (loc_id, H5VL_OBJECT_LOOKUP_BY_NAME, &location, name, tapl_id) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to locate object")

        /* Open the object through the VOL */
        if((ret_value = H5VL_object_open_by_loc(loc_id, location, tapl_id)) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to open object")
    }
    else {
        /* call the corresponding VOL open callback */
        if((datatype_id = (id_wrapper1->vol_plugin->datatype_cls.open)
            (id_wrapper1->obj_id, name, tapl_id)) < 0)
            HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "open failed")

        /* Create a new id that points to a struct that holds the datatype id and the VOL plugin */
        /* Allocate new id structure */
        if(NULL == (id_wrapper2 = (H5VL_id_wrapper_t *)H5MM_malloc(sizeof(H5VL_id_wrapper_t))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")
        id_wrapper2->obj_id = datatype_id;
        id_wrapper2->vol_plugin = id_wrapper1->vol_plugin;

        if((ret_value = H5I_register(H5I_DATATYPE_PUBLIC, id_wrapper2, TRUE)) < 0)
            HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to atomize datatype handle")
    }
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_datatype_open() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_dataset_create
 *
 * Purpose:	Creates a dataset through the VOL
 *
 * Return:      Success: User ID of the new dataset. This ID is of type
 *                       H5I_DATASET_PUBLIC which contains the VOL plugin and the actual dataset ID
 *
 *		Failure: FAIL
 *
 * Programmer:	Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5VL_dataset_create(hid_t uid, const char *name, hid_t dcpl_id, hid_t dapl_id)
{
    H5VL_id_wrapper_t    *id_wrapper1;             /* user id structure of the location where the dataset will be created */
    H5VL_id_wrapper_t    *id_wrapper2;             /* user id structure of new created dataset*/
    hid_t     dataset_id;            /* actual dataset ID */
    hid_t     ret_value;             /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* unwrap the datatype id if it is a named datatype */
    {
        H5VL_id_wrapper_t    *id_wrapper;
        hid_t                type_id;
        H5P_genplist_t       *plist;
        /* Get the plist structure */
        if(NULL == (plist = (H5P_genplist_t *)H5I_object(dcpl_id)))
            HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

        /* get creation properties */
        if(H5P_get(plist, H5D_CRT_TYPE_ID_NAME, &type_id) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get property value for datatype id")

        if(H5I_DATATYPE_PUBLIC == H5I_get_type(type_id)) {
            /* get the ID struct */
            if(NULL == (id_wrapper = (H5VL_id_wrapper_t *)H5I_object(type_id)))
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid user identifier")        

            if(H5P_set(plist, H5D_CRT_TYPE_ID_NAME, &(id_wrapper->obj_id)) < 0)
                HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't set property value for datatype id")
        }
    }
    /* get the ID struct */
    if(NULL == (id_wrapper1 = (H5VL_id_wrapper_t *)H5I_object(uid)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid user identifier")

    /* check if the corresponding VOL create callback exists */
    if(NULL == id_wrapper1->vol_plugin->dataset_cls.create)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `dataset create' method")

    /* call the corresponding VOL create callback */
    if((dataset_id = (id_wrapper1->vol_plugin->dataset_cls.create)
        (id_wrapper1->obj_id, name, dcpl_id, dapl_id)) < 0)
	HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "create failed")

    /* Create a new id that points to a struct that holds the dataset id and the VOL plugin */
    /* Allocate new id structure */
    if(NULL == (id_wrapper2 = (H5VL_id_wrapper_t *)H5MM_malloc(sizeof(H5VL_id_wrapper_t))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")
    id_wrapper2->obj_id = dataset_id;
    id_wrapper2->vol_plugin = id_wrapper1->vol_plugin;

    if((ret_value = H5I_register(H5I_DATASET_PUBLIC, id_wrapper2, TRUE)) < 0)
	HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to atomize dataset handle")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_dataset_create() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_dataset_open
 *
 * Purpose:	Opens a dataset through the VOL
 *
 * Return:      Success: User ID of the new dataset. This ID is of type
 *                       H5I_DATASET_PUBLIC which contains the VOL plugin and the actual dataset ID
 *
 *		Failure: FAIL
 *
 * Programmer:	Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5VL_dataset_open(hid_t loc_id, const char *name, hid_t dapl_id)
{
    H5VL_id_wrapper_t   *id_wrapper1;             /* user id structure of the location where the dataset will be opend */
    H5VL_id_wrapper_t   *id_wrapper2;             /* user id structure of new opend dataset*/
    hid_t               dataset_id;               /* actual dataset ID */
    hid_t		ret_value;              /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* get the ID struct */
    if(NULL == (id_wrapper1 = (H5VL_id_wrapper_t *)H5I_object(loc_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid user identifier")

    /* check if the type specific corresponding VOL open callback exists */
    if(NULL == id_wrapper1->vol_plugin->dataset_cls.open) {
        void       *location = NULL; /* a pointer to VOL specific token that indicates 
                                        the location of the object */

        /* Get the token for the Object location through the VOL */
        if(H5VL_object_lookup (loc_id, H5VL_OBJECT_LOOKUP_BY_NAME, &location, name, dapl_id) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to locate object")

        /* Open the object through the VOL */
        if((ret_value = H5VL_object_open_by_loc(loc_id, location, dapl_id)) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to open object")
    }
    else {
        /* call the corresponding VOL open callback */
        if((dataset_id = (id_wrapper1->vol_plugin->dataset_cls.open)
            (id_wrapper1->obj_id, name, dapl_id)) < 0)
            HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "open failed")

        /* Create a new id that points to a struct that holds the dataset id and the VOL plugin */
        /* Allocate new id structure */
        if(NULL == (id_wrapper2 = (H5VL_id_wrapper_t *)H5MM_malloc(sizeof(H5VL_id_wrapper_t))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")
        id_wrapper2->obj_id = dataset_id;
        id_wrapper2->vol_plugin = id_wrapper1->vol_plugin;

        if((ret_value = H5I_register(H5I_DATASET_PUBLIC, id_wrapper2, TRUE)) < 0)
            HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to atomize dataset handle")
    }
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
herr_t H5VL_dataset_read(hid_t uid, hid_t mem_type_id, hid_t mem_space_id, 
                         hid_t file_space_id, hid_t plist_id, void *buf)
{
    H5VL_id_wrapper_t               *id_wrapper;              /* user id structure */
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    /* Check/fix arguments. */
    if(H5I_DATASET_PUBLIC != H5I_get_type(uid))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a user ID")

    /* get the ID struct */
    if(NULL == (id_wrapper = (H5VL_id_wrapper_t *)H5I_object(uid)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid user identifier")

    if(NULL == id_wrapper->vol_plugin->dataset_cls.read)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `dataset read' method")
    if((ret_value = (id_wrapper->vol_plugin->dataset_cls.read)
        (id_wrapper->obj_id, mem_type_id, mem_space_id, file_space_id, plist_id, buf)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTRELEASE, FAIL, "read failed")

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
herr_t H5VL_dataset_write(hid_t uid, hid_t mem_type_id, hid_t mem_space_id, 
                          hid_t file_space_id, hid_t plist_id, const void *buf)
{
    H5VL_id_wrapper_t               *id_wrapper;              /* user id structure */
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    /* Check/fix arguments. */
    if(H5I_DATASET_PUBLIC != H5I_get_type(uid))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a user ID")

    /* get the ID struct */
    if(NULL == (id_wrapper = (H5VL_id_wrapper_t *)H5I_object(uid)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid user identifier")

    if(NULL == id_wrapper->vol_plugin->dataset_cls.write)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `dataset write' method")
    if((ret_value = (id_wrapper->vol_plugin->dataset_cls.write)
        (id_wrapper->obj_id, mem_type_id, mem_space_id, file_space_id, plist_id, buf)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTRELEASE, FAIL, "write failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_dataset_write() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_dataset_set_extent
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
herr_t H5VL_dataset_set_extent(hid_t uid, const hsize_t size[])
{
    H5VL_id_wrapper_t   *id_wrapper;              /* user id structure */
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    /* Check/fix arguments. */
    if(H5I_DATASET_PUBLIC != H5I_get_type(uid))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset ID")

    /* get the ID struct */
    if(NULL == (id_wrapper = (H5VL_id_wrapper_t *)H5I_object(uid)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid user identifier")

    if(NULL == id_wrapper->vol_plugin->dataset_cls.set_extent)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `dataset set_extent' method")
    if((ret_value = (id_wrapper->vol_plugin->dataset_cls.set_extent)(id_wrapper->obj_id, size)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "set_extent failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_dataset_set_extent() */


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
H5VL_dataset_get(hid_t uid, H5VL_dataset_get_t get_type, ...)
{
    H5VL_id_wrapper_t            *id_wrapper;              /* user id structure */
    va_list           arguments;             /* argument list passed from the API call */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    /* Check id */
    if(H5I_DATASET_PUBLIC != H5I_get_type(uid))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a user ID")

    /* get the ID struct */
    if(NULL == (id_wrapper = (H5VL_id_wrapper_t *)H5I_object(uid)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid user identifier")

    if(NULL == id_wrapper->vol_plugin->dataset_cls.get)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `dataset get' method")

    va_start (arguments, get_type);
    if((ret_value = (id_wrapper->vol_plugin->dataset_cls.get)(id_wrapper->obj_id, get_type, 
                                                           arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "get failed")
    va_end (arguments);

    /* if the get_type is a named datatype, create a wrapper for it */
    if(H5VL_DATASET_GET_TYPE == get_type) {
        H5VL_id_wrapper_t *temp_id_wrapper;              /* user id structure */
        hid_t	        *ret_id;

        va_start (arguments, get_type);
        ret_id = va_arg (arguments, hid_t *);

        if(H5T_committed((H5T_t *)H5I_object_verify(*ret_id, H5I_DATATYPE))) {
            /* Create a new id that points to a struct that holds the attr id and the VOL plugin */
            /* Allocate new id structure */
            if(NULL == (temp_id_wrapper = (H5VL_id_wrapper_t *)H5MM_malloc(sizeof(H5VL_id_wrapper_t))))
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")
            temp_id_wrapper->obj_id = *ret_id;
            temp_id_wrapper->vol_plugin = id_wrapper->vol_plugin;

            if((*ret_id = H5I_register(H5I_DATATYPE_PUBLIC, temp_id_wrapper, TRUE)) < 0)
                HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to atomize attr handle")
        }
        va_end (arguments);
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_dataset_get() */


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
H5VL_dataset_close(hid_t uid)
{
    H5VL_id_wrapper_t   *id_wrapper;              /* user id structure */
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    /* Check/fix arguments. */
    if(H5I_DATASET_PUBLIC != H5I_get_type(uid))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a user ID")

    /* get the ID struct */
    if(NULL == (id_wrapper = (H5VL_id_wrapper_t *)H5I_object(uid)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid user identifier")
            
    /* if the VOL class does not implement a specific dataset close
       callback, try the generic object close */    
    if(NULL == id_wrapper->vol_plugin->dataset_cls.close){
        if(H5VL_object_close(uid) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTRELEASE, FAIL, "unable to close object")
    }
    else {
        if((ret_value = (id_wrapper->vol_plugin->dataset_cls.close)(id_wrapper->obj_id)) < 0)
            HGOTO_ERROR(H5E_VOL, H5E_CANTRELEASE, FAIL, "close failed")

        id_wrapper->vol_plugin->nrefs--;
        if(H5I_dec_app_ref(uid) < 0)
            HGOTO_ERROR(H5E_VOL, H5E_CANTDEC, FAIL, "unable to decrement ref count on user ID")
    }
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_dataset_close() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_file_open
 *
 * Purpose:	Opens a file through the VOL.
 *
 * Return:      Success: User ID of the new file. This ID is of type
 *                       H5I_FILE_PUBLIC which contains the VOL id and the actual file ID
 *
 *		Failure: FAIL
 *
 * Programmer:	Mohamad Chaarawi
 *              January, 2012
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5VL_file_open(const char *name, unsigned flags, hid_t fapl_id)
{
    H5VL_class_t	*vol_plugin;            /* VOL for file */
    H5VL_id_wrapper_t   *id_wrapper;              /* user id structure */
    H5P_genplist_t      *plist;                 /* Property list pointer */
    hid_t               file_id;
    hid_t		ret_value;              /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* get the VOL info from the fapl */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(fapl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access property list")
    if(H5P_get(plist, H5F_ACS_VOL_NAME, &vol_plugin) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get vol plugin")

    /* check if the corresponding VOL open callback exists */
    if(NULL == vol_plugin->file_cls.open)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `file open' method")
    /* call the corresponding VOL open callback */
    if((file_id = (vol_plugin->file_cls.open)(name, flags, fapl_id)) < 0)
	HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "open failed")

    /* Create a new id that points to a struct that holds the file id and the VOL plugin */
    /* Allocate new id structure */
    if(NULL == (id_wrapper = (H5VL_id_wrapper_t *)H5MM_malloc(sizeof(H5VL_id_wrapper_t))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")
    id_wrapper->obj_id = file_id;
    id_wrapper->vol_plugin = vol_plugin;
    vol_plugin->nrefs ++;

    if((ret_value = H5I_register(H5I_FILE_PUBLIC, id_wrapper, TRUE)) < 0)
	HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to atomize file handle")
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_file_open() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_file_create
 *
 * Purpose:	Creates a file through the VOL
 *
 * Return:      Success: User ID of the new file. This ID is of type
 *                       H5I_FILE_PUBLIC which contains the VOL id and the actual file ID
 *
 *		Failure: FAIL
 *
 * Programmer:	Mohamad Chaarawi
 *              January, 2012
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5VL_file_create(const char *name, unsigned flags, hid_t fcpl_id, hid_t fapl_id)
{
    H5VL_class_t	*vol_plugin;            /* VOL for file */
    H5VL_id_wrapper_t   *id_wrapper;              /* user id structure */
    H5P_genplist_t      *plist;                 /* Property list pointer */
    hid_t               file_id;
    hid_t		ret_value;              /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* get the VOL info from the fapl */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(fapl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access property list")
    if(H5P_get(plist, H5F_ACS_VOL_NAME, &vol_plugin) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get vol plugin ID")

    /* check if the corresponding VOL create callback exists */
    if(NULL == vol_plugin->file_cls.create)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `file create' method")
    /* call the corresponding VOL create callback */
    if((file_id = (vol_plugin->file_cls.create)(name, flags, fcpl_id, fapl_id)) < 0)
	HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "create failed")

    /* Create a new id that points to a struct that holds the file id and the VOL id */
    /* Allocate new id structure */
    if(NULL == (id_wrapper = (H5VL_id_wrapper_t *)H5MM_malloc(sizeof(H5VL_id_wrapper_t))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")
    id_wrapper->obj_id = file_id;
    id_wrapper->vol_plugin = vol_plugin;
    vol_plugin->nrefs ++;

    if((ret_value = H5I_register(H5I_FILE_PUBLIC, id_wrapper, TRUE)) < 0)
	HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to atomize file handle")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_file_create() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_file_flush
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
H5VL_file_flush(hid_t uid, H5F_scope_t scope)
{
    H5VL_id_wrapper_t   *id_wrapper;              /* user id structure */
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    /* Check/fix arguments. */
    if(H5I_FILE_PUBLIC != H5I_get_type(uid))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a user ID")

    /* get the ID struct */
    if(NULL == (id_wrapper = (H5VL_id_wrapper_t *)H5I_object(uid)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid user identifier")

    if(NULL == id_wrapper->vol_plugin->file_cls.flush)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `file flush' method")
    if((ret_value = (id_wrapper->vol_plugin->file_cls.flush)(id_wrapper->obj_id, scope)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTFLUSH, FAIL, "flush failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_file_flush() */


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
H5VL_file_get(hid_t uid, H5VL_file_get_t get_type, ...)
{
    H5VL_id_wrapper_t *id_wrapper;             /* user id structure */
    va_list           arguments;             /* argument list passed from the API call */
    H5I_type_t        id_type;               /* Type of ID */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    id_type = H5I_get_type(uid);
    /* Check/fix arguments. */
    if (H5I_FILE_PUBLIC != id_type && H5I_GROUP_PUBLIC != id_type &&
        H5I_DATATYPE_PUBLIC != id_type && H5I_DATASET_PUBLIC != id_type &&
        H5I_ATTR_PUBLIC != id_type)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a user ID")

    /* get the ID struct */
    if(NULL == (id_wrapper = (H5VL_id_wrapper_t *)H5I_object(uid)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid user identifier")

    if(NULL == id_wrapper->vol_plugin->file_cls.get)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `file get' method")

    va_start(arguments, get_type);
    if((ret_value = (id_wrapper->vol_plugin->file_cls.get)(id_wrapper->obj_id, get_type, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "get failed")
    va_end(arguments);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_file_get() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_file_generic
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
H5VL_file_generic(hid_t uid, H5VL_file_generic_t generic_type, ...)
{
    H5VL_id_wrapper_t *id_wrapper;              /* user id structure */
    va_list           arguments;             /* argument list passed from the API call */
    H5I_type_t        id_type;
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    id_type = H5I_get_type(uid);
    /* Check id */
    if(H5I_FILE_PUBLIC != id_type && H5I_GROUP_PUBLIC != id_type &&
       H5I_DATASET_PUBLIC != id_type && H5I_DATATYPE_PUBLIC !=  id_type &&
       H5I_FILE_PUBLIC != id_type)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a user ID")

    /* get the ID struct */
    if(NULL == (id_wrapper = (H5VL_id_wrapper_t *)H5I_object(uid)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid user identifier")

    if(NULL == id_wrapper->vol_plugin->file_cls.generic)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `file generic' method")

    va_start (arguments, generic_type);
    if((ret_value = (id_wrapper->vol_plugin->file_cls.generic)(id_wrapper->obj_id, generic_type, 
                                                               arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "generic failed")
    va_end (arguments);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_file_generic() */


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
H5VL_file_close(hid_t uid)
{
    H5VL_id_wrapper_t   *id_wrapper;              /* user id structure */
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    /* Check/fix arguments. */
    if(H5I_FILE_PUBLIC != H5I_get_type(uid))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a user ID")

    /* get the ID struct */
    if(NULL == (id_wrapper = (H5VL_id_wrapper_t *)H5I_object(uid)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid user identifier")

    if(NULL == id_wrapper->vol_plugin->file_cls.close)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `file close' method")
    if((ret_value = (id_wrapper->vol_plugin->file_cls.close)(id_wrapper->obj_id)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTCLOSEFILE, FAIL, "close failed")

    id_wrapper->vol_plugin->nrefs--;
    if(H5I_dec_app_ref(uid) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINC, FAIL, "unable to decrement ref count on user ID")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_file_close() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_group_create
 *
 * Purpose:	Creates a group through the VOL
 *
 * Return:      Success: User ID of the new group. This ID is of type
 *                       H5I_GROUP_PUBLIC which contains the VOL plugin and the actual group ID
 *
 *		Failure: FAIL
 *
 * Programmer:	Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5VL_group_create(hid_t uid, const char *name, hid_t gcpl_id, hid_t gapl_id)
{
    H5VL_id_wrapper_t   *id_wrapper1;             /* user id structure of the location where the group will be created */
    H5VL_id_wrapper_t   *id_wrapper2;             /* user id structure of new created group*/
    hid_t               group_id;               /* actual group ID */
    hid_t		ret_value;              /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* get the ID struct */
    if(NULL == (id_wrapper1 = (H5VL_id_wrapper_t *)H5I_object(uid)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid user identifier")

    /* check if the corresponding VOL create callback exists */
    if(NULL == id_wrapper1->vol_plugin->group_cls.create)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `group create' method")

    /* call the corresponding VOL create callback */
    if((group_id = (id_wrapper1->vol_plugin->group_cls.create)
        (id_wrapper1->obj_id, name, gcpl_id, gapl_id)) < 0)
	HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "create failed")

    /* Create a new id that points to a struct that holds the group id and the VOL plugin */
    /* Allocate new id structure */
    if(NULL == (id_wrapper2 = (H5VL_id_wrapper_t *)H5MM_malloc(sizeof(H5VL_id_wrapper_t))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")
    id_wrapper2->obj_id = group_id;
    id_wrapper2->vol_plugin = id_wrapper1->vol_plugin;

    if((ret_value = H5I_register(H5I_GROUP_PUBLIC, id_wrapper2, TRUE)) < 0)
	HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to atomize group handle")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_group_create() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_group_open
 *
 * Purpose:	Opens a group through the VOL
 *
 * Return:      Success: User ID of the new group. This ID is of type
 *                       H5I_GROUP_PUBLIC which contains the VOL plugin and the actual group ID
 *
 *		Failure: FAIL
 *
 * Programmer:	Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5VL_group_open(hid_t loc_id, const char *name, hid_t gapl_id)
{
    H5VL_id_wrapper_t   *id_wrapper1;             /* user id structure of the location where the group will be opend */
    H5VL_id_wrapper_t   *id_wrapper2;             /* user id structure of new opend group*/
    hid_t               group_id;               /* actual group ID */
    hid_t		ret_value;              /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* get the ID struct */
    if(NULL == (id_wrapper1 = (H5VL_id_wrapper_t *)H5I_object(loc_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid user identifier")

    /* check if the type specific corresponding VOL open callback exists */
    if(NULL == id_wrapper1->vol_plugin->group_cls.open) {
        void       *location = NULL; /* a pointer to VOL specific token that indicates 
                                        the location of the object */

        /* Get the token for the Object location through the VOL */
        if(H5VL_object_lookup (loc_id, H5VL_OBJECT_LOOKUP_BY_NAME, &location, name, gapl_id) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to locate object")

        /* Open the object through the VOL */
        if((ret_value = H5VL_object_open_by_loc(loc_id, location, gapl_id)) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to open object")
    }
    else {
        /* call the corresponding VOL open callback */
        if((group_id = (id_wrapper1->vol_plugin->group_cls.open)
            (id_wrapper1->obj_id, name, gapl_id)) < 0)
            HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "open failed")

        /* Create a new id that points to a struct that holds the group id and the VOL plugin */
        /* Allocate new id structure */
        if(NULL == (id_wrapper2 = (H5VL_id_wrapper_t *)H5MM_malloc(sizeof(H5VL_id_wrapper_t))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")
        id_wrapper2->obj_id = group_id;
        id_wrapper2->vol_plugin = id_wrapper1->vol_plugin;

        if((ret_value = H5I_register(H5I_GROUP_PUBLIC, id_wrapper2, TRUE)) < 0)
            HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to atomize group handle")
    }
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
H5VL_group_get(hid_t uid, H5VL_group_get_t get_type, ...)
{
    H5VL_id_wrapper_t            *id_wrapper;              /* user id structure */
    va_list           arguments;             /* argument list passed from the API call */
    H5I_type_t        id_type;               /* Type of ID */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    id_type = H5I_get_type(uid);
    /* Check/fix arguments. */
    if(H5I_GROUP_PUBLIC != id_type && H5I_FILE_PUBLIC != id_type)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a user ID")

    /* get the ID struct */
    if(NULL == (id_wrapper = (H5VL_id_wrapper_t *)H5I_object(uid)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid user identifier")

    if(NULL == id_wrapper->vol_plugin->group_cls.get)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `group get' method")

    va_start (arguments, get_type);
    if((ret_value = (id_wrapper->vol_plugin->group_cls.get)
        (id_wrapper->obj_id, get_type, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "get failed")
    va_end (arguments);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_group_get() */


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
H5VL_group_close(hid_t uid)
{
    H5VL_id_wrapper_t               *id_wrapper;              /* user id structure */
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    /* Check/fix arguments. */
    if(H5I_GROUP_PUBLIC != H5I_get_type(uid))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a user ID")

    /* get the ID struct */
    if(NULL == (id_wrapper = (H5VL_id_wrapper_t *)H5I_object(uid)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid user identifier")

    /* if the VOL class does not implement a specific group close
       callback, try the generic object close */
    if(NULL == id_wrapper->vol_plugin->group_cls.close) {
        if(H5VL_object_close(uid) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTRELEASE, FAIL, "unable to close object")
    }
    else {
        if((ret_value = (id_wrapper->vol_plugin->group_cls.close)(id_wrapper->obj_id)) < 0)
            HGOTO_ERROR(H5E_VOL, H5E_CANTRELEASE, FAIL, "close failed")

        id_wrapper->vol_plugin->nrefs--;

        if(H5I_dec_app_ref(uid) < 0)
            HGOTO_ERROR(H5E_VOL, H5E_CANTDEC, FAIL, "unable to decrement ref count on user ID")
   }
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
H5VL_link_create(H5VL_link_create_type_t create_type, hid_t loc_id, const char *new_name, 
                 hid_t lcpl_id, hid_t lapl_id)
{
    H5VL_class_t	 *vol_plugin = NULL;   /* VOL plugin */
    hid_t                new_id;       /* unwrapped IDs */
    herr_t               ret_value = SUCCEED;  /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* unwrap the target high level ID if the creation call is H5Lcreate_hard */
    if(H5VL_CREATE_HARD_LINK == create_type) {
        H5P_genplist_t *plist;      /* Property list pointer */
        hid_t cur_id;

        /* Get the plist structure */
        if(NULL == (plist = (H5P_genplist_t *)H5I_object(lcpl_id)))
            HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

        if(H5P_get(plist, H5L_CRT_TARGET_ID_NAME, &cur_id) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get property value for current location id")

        /* Only one of the IDs can be H5L_SAME_LOC, and the other one must
           be of the wrapper type. Get the VOL plugin struct in case the 
           link id is H5L_SAME_LOC*/
        if (H5L_SAME_LOC != cur_id && H5I_DATATYPE != H5I_get_type(cur_id)) {
            H5VL_id_wrapper_t    *id_wrapper;

            /* get the ID struct */
            if(NULL == (id_wrapper = (H5VL_id_wrapper_t *)H5I_object(cur_id)))
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid user identifier")        

            vol_plugin = id_wrapper->vol_plugin;
            if(H5P_set(plist, H5L_CRT_TARGET_ID_NAME, &(id_wrapper->obj_id)) < 0)
                HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get property value for current location id")
        }
    }

    /* unwrap the higher level user ids. */ 
    if(H5L_SAME_LOC != loc_id && H5I_DATATYPE != H5I_get_type(loc_id)) {
        H5VL_id_wrapper_t    *id_wrapper;

        /* get the ID struct */
        if(NULL == (id_wrapper = (H5VL_id_wrapper_t *)H5I_object(loc_id)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid user identifier")        

        /* set the vol plugin sturcture if it hasn't been done yet */
        if (NULL == vol_plugin)
            vol_plugin = id_wrapper->vol_plugin;

        new_id = id_wrapper->obj_id;
    }
    else {
        new_id = loc_id;
    }

    /* check if the corresponding VOL create callback exists */
    if(NULL == vol_plugin->link_cls.create)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `link create' method")

    /* call the corresponding VOL create callback */
    if((ret_value = (vol_plugin->link_cls.create)
        (create_type, new_id, new_name, lcpl_id, lapl_id)) < 0)
	HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "link create failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_link_create() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_link_move
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
H5_DLL herr_t H5VL_link_move(hid_t src_loc_id, const char *src_name, hid_t dst_loc_id,
                             const char *dst_name, hbool_t copy_flag, hid_t lcpl_id, hid_t lapl_id)
{
    H5VL_class_t	 *vol_plugin = NULL;   /* VOL plugin */
    H5VL_id_wrapper_t    *id_wrapper;
    hid_t                new_src_id, new_dst_id;       /* unwrapped IDs */
    herr_t               ret_value = SUCCEED;  /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* unwrap the higher level user ids. */ 
    if(H5L_SAME_LOC != src_loc_id) {
        /* get the ID struct */
        if(NULL == (id_wrapper = (H5VL_id_wrapper_t *)H5I_object(src_loc_id)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid user identifier")        

        vol_plugin = id_wrapper->vol_plugin;
        new_src_id = id_wrapper->obj_id;
    }
    else {
        new_src_id = src_loc_id;
    }

    if(H5L_SAME_LOC != dst_loc_id) {
        /* get the ID struct */
        if(NULL == (id_wrapper = (H5VL_id_wrapper_t *)H5I_object(dst_loc_id)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid user identifier")        
        if(NULL == vol_plugin)
            vol_plugin = id_wrapper->vol_plugin;
        new_dst_id = id_wrapper->obj_id;
    }
    else {
        new_dst_id = dst_loc_id;
    }

    /* check if the corresponding VOL move callback exists */
    if(NULL == vol_plugin->link_cls.move)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `link move' method")

    /* call the corresponding VOL move callback */
    if((ret_value = (vol_plugin->link_cls.move)
        (new_src_id, src_name, new_dst_id, dst_name, copy_flag, lcpl_id, lapl_id)) < 0)
	HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "link move failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_link_move() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_link_delete
 *
 * Purpose:	Copy or delete a link from src to dst.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Mohamad Chaarawi
 *              April, 2012
 *
 *-------------------------------------------------------------------------
 */
H5_DLL herr_t H5VL_link_delete(hid_t loc_id, const char *name, hid_t lapl_id)
{
    H5VL_id_wrapper_t    *id_wrapper;
    herr_t               ret_value = SUCCEED;  /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    if(NULL == (id_wrapper = (H5VL_id_wrapper_t *)H5I_object(loc_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid user identifier")        

    /* check if the corresponding VOL delete callback exists */
    if(NULL == id_wrapper->vol_plugin->link_cls.delete)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `link delete' method")

    /* call the corresponding VOL delete callback */
    if((ret_value = (id_wrapper->vol_plugin->link_cls.delete)
        (id_wrapper->obj_id, name, lapl_id)) < 0)
	HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "link delete failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_link_delete() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_object_open_by_loc
 *
 * Purpose:	Opens a object through the VOL
 *
 * Return:      Success: User ID of the new object. This ID is of type
 *                       H5I_OBJECT_PUBLIC which contains the VOL plugin and the actual object ID
 *
 *		Failure: FAIL
 *
 * Programmer:	Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5VL_object_open_by_loc(hid_t uid, void *obj_loc, hid_t lapl_id)
{
    H5VL_id_wrapper_t               *id_wrapper1;             /* user id structure of the location where the object will be opend */
    H5VL_id_wrapper_t               *id_wrapper2;             /* user id structure of new opend object*/
    H5I_type_t          id_type;
    hid_t               object_id;               /* actual object ID */
    hid_t		ret_value;              /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* get the ID struct */
    if(NULL == (id_wrapper1 = (H5VL_id_wrapper_t *)H5I_object(uid)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid user identifier")

    /* check if the corresponding VOL open callback exists */
    if(NULL == id_wrapper1->vol_plugin->object_cls.open)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `object open' method")

    /* call the corresponding VOL open callback */
    if((object_id = (id_wrapper1->vol_plugin->object_cls.open)
        (id_wrapper1->obj_id, obj_loc, lapl_id)) < 0)
	HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "open failed")

    /* Create a new id that points to a struct that holds the object id and the VOL plugin */
    /* Allocate new id structure */
    if(NULL == (id_wrapper2 = (H5VL_id_wrapper_t *)H5MM_malloc(sizeof(H5VL_id_wrapper_t))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")
    id_wrapper2->obj_id = object_id;
    id_wrapper2->vol_plugin = id_wrapper1->vol_plugin;

    id_type = H5I_get_type(object_id);

    if (H5I_GROUP == id_type) {
        if((ret_value = H5I_register(H5I_GROUP_PUBLIC, id_wrapper2, TRUE)) < 0)
            HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to atomize object handle")
    }
    else if (H5I_DATATYPE == id_type) {
        if((ret_value = H5I_register(H5I_DATATYPE_PUBLIC, id_wrapper2, TRUE)) < 0)
            HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to atomize object handle")
    }
    else if (H5I_DATASET == id_type) {
        if((ret_value = H5I_register(H5I_DATASET_PUBLIC, id_wrapper2, TRUE)) < 0)
            HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to atomize object handle")
    }
    else {
        ret_value = object_id;
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_object_open_by_loc() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_object_lookup
 *
 * Purpose:	Lookup the object location in the file
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
H5VL_object_lookup(hid_t uid, H5VL_object_lookup_t lookup_type, ...)
{
    H5VL_id_wrapper_t *id_wrapper;              /* user id structure */
    va_list           arguments;             /* argument list passed from the API call */
    H5I_type_t        id_type;
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    id_type = H5I_get_type(uid);
    /* Check id */
    if(H5I_FILE_PUBLIC != id_type && H5I_GROUP_PUBLIC != id_type &&
       H5I_DATASET_PUBLIC != id_type && H5I_DATATYPE_PUBLIC !=  id_type &&
       H5I_ATTR_PUBLIC != id_type)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a user ID")

    /* lookup the ID struct */
    if(NULL == (id_wrapper = (H5VL_id_wrapper_t *)H5I_object(uid)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid user identifier")

    if(NULL == id_wrapper->vol_plugin->object_cls.lookup)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `object lookup' method")

    va_start (arguments, lookup_type);
    if((ret_value = (id_wrapper->vol_plugin->object_cls.lookup)(id_wrapper->obj_id, lookup_type, 
                                                                arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "lookup of object location failed")
    va_end (arguments);
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_object_lookup() */


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
H5VL_object_get(hid_t uid, H5VL_object_get_t get_type, ...)
{
    H5VL_id_wrapper_t            *id_wrapper;              /* user id structure */
    va_list           arguments;             /* argument list passed from the API call */
    H5I_type_t        id_type;
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    id_type = H5I_get_type(uid);
    /* Check id */
    if(H5I_GROUP_PUBLIC != id_type && H5I_DATASET_PUBLIC != id_type && 
       H5I_DATATYPE_PUBLIC !=  id_type && H5I_FILE_PUBLIC !=  id_type &&
       H5I_ATTR_PUBLIC !=  id_type)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a user ID")

    /* get the ID struct */
    if(NULL == (id_wrapper = (H5VL_id_wrapper_t *)H5I_object(uid)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid user identifier")

    if(NULL == id_wrapper->vol_plugin->object_cls.get)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `object get' method")

    va_start (arguments, get_type);
    if((ret_value = (id_wrapper->vol_plugin->object_cls.get)(id_wrapper->obj_id, get_type, 
                                                             arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "get failed")
    va_end (arguments);
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_object_get() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_object_close
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
H5VL_object_close(hid_t uid)
{
    H5VL_id_wrapper_t   *id_wrapper;              /* user id structure */
    H5I_type_t          id_type;
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    id_type = H5I_get_type(uid);

    /* In case of a named datatype being closed with H5Oopen after an
       H5Tcommit, the id is still of type H5I_DATATYPE and not
       H5I_DATATYPE_PUBLIC. In that case we just fall back to the
       native implementation */
    if(H5I_DATATYPE ==  id_type) {
        if(H5I_object(uid) == NULL)
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a valid object")
        if(H5I_dec_app_ref(uid) < 0)
            HGOTO_ERROR(H5E_OHDR, H5E_CANTRELEASE, FAIL, "unable to close object")
        HGOTO_DONE(ret_value)
    }

    /* Check id */
    if(H5I_GROUP_PUBLIC != H5I_get_type(uid) && H5I_DATASET_PUBLIC != H5I_get_type(uid) && 
       H5I_DATATYPE_PUBLIC !=  H5I_get_type(uid))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a user ID")

    /* get the ID struct */
    if(NULL == (id_wrapper = (H5VL_id_wrapper_t *)H5I_object(uid)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid user identifier")

    if(NULL == id_wrapper->vol_plugin->object_cls.close)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `object close' method")
    if((ret_value = (id_wrapper->vol_plugin->object_cls.close)(id_wrapper->obj_id)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTRELEASE, FAIL, "close failed")

    id_wrapper->vol_plugin->nrefs--;
    if(H5I_dec_app_ref(uid) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTDEC, FAIL, "unable to decrement ref count on user ID")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_object_close() */

