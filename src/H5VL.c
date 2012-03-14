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

#define H5VL_PACKAGE		/*suppress error about including H5FDpkg  */

/* Interface initialization */
#define H5_INTERFACE_INIT_FUNC	H5VL_init_interface

/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Dprivate.h"		/* Datasets				*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5VLpkg.h"		/* VOL package header		  	*/
#include "H5VLprivate.h"	/* VOL          		  	*/
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Pprivate.h"		/* Property lists			*/

/* Declare a free list to manage the H5I_t struct */
H5FL_DEFINE_STATIC(H5I_t);

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

    if(H5I_register_type(H5I_VOL, (size_t)H5I_VOL_HASHSIZE, 0, (H5I_free_t)H5VL_free_cls)<H5I_FILE)
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
    int	n = 0;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    if(H5_interface_initialize_g) {
	if((n=H5I_nmembers(H5I_VOL))!=0) {
	    H5I_clear_type(H5I_VOL, FALSE, FALSE);
	} else {
	    H5I_dec_type_ref(H5I_VOL);
	    H5_interface_initialize_g = 0;
	    n = 1; /*H5I*/
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
        hid_t vol_id = -1;

        /* Get the plist structure */
        if(NULL == (plist = (H5P_genplist_t *)H5I_object(id)))
            HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, NULL, "can't find object for ID")

        if(TRUE == H5P_isa_class(id, H5P_FILE_ACCESS)) {
            if(H5P_get(plist, H5F_ACS_VOL_ID_NAME, &vol_id) < 0)
                HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get vol ID")
            ret_value = H5VL_get_class(vol_id);
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
H5VL_fapl_open(H5P_genplist_t *plist, hid_t vol_id, const void *vol_info)
{
    void *copied_vol_info = NULL;   /* Temporary VOL vol info */
    herr_t ret_value = SUCCEED;     /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Increment the reference count on vol and copy vol info */
    if(H5I_inc_ref(vol_id, FALSE) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINC, FAIL, "unable to increment ref count on vol plugin")
    if(H5VL_fapl_copy(vol_id, vol_info, &copied_vol_info) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTCOPY, FAIL, "can't copy vol info")

    /* Set the vol properties for the list */
    if(H5P_set(plist, H5F_ACS_VOL_ID_NAME, &vol_id) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTSET, FAIL, "can't set vol ID")
    if(H5P_set(plist, H5F_ACS_VOL_INFO_NAME, &copied_vol_info) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTSET, FAIL, "can't set vol info")
    copied_vol_info = NULL;

done:
    if(ret_value < 0)
        if(copied_vol_info && H5VL_fapl_close(vol_id, copied_vol_info) < 0)
            HDONE_ERROR(H5E_FILE, H5E_CANTCLOSEOBJ, FAIL, "can't close copy of vol info")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_fapl_open() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_fapl_copy
 *
 * Purpose:	Copies the vol-specific part of the file access property
 *		list.
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
H5VL_fapl_copy(hid_t vol_id, const void *old_fapl, void **copied_fapl)
{
    H5VL_class_t *vol;
    void *new_pl = NULL;        /* Copy of property list */
    herr_t ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Check args */
    if(NULL == (vol = (H5VL_class_t *)H5I_object(vol_id)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a vol ID")

    /* Copy the file access property list */
    if(old_fapl) {
        if(vol->fapl_copy) {
            new_pl = (vol->fapl_copy)(old_fapl);
            if(new_pl==NULL)
                HGOTO_ERROR(H5E_VOL, H5E_NOSPACE, FAIL, "property list copy failed")
        }
        else if(vol->fapl_size > 0) {
            if((new_pl = H5MM_malloc(vol->fapl_size))==NULL)
                HGOTO_ERROR(H5E_VOL, H5E_NOSPACE, FAIL, "property list allocation failed")
            HDmemcpy(new_pl, old_fapl, vol->fapl_size);
        } else
            HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "no way to copy vol property list")
    } /* end if */

    /* Set copied value */
    *copied_fapl=new_pl;

done:
    FUNC_LEAVE_NOAPI(ret_value)
}


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
H5VL_fapl_close(hid_t vol_id, void *fapl)
{
    H5VL_class_t	*vol = NULL;
    herr_t      ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Check args */
    if(vol_id > 0) {
        if(NULL == (vol = (H5VL_class_t *)H5I_object(vol_id)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a vol ID")

        /* Allow driver to free or do it ourselves */
        if(fapl && vol->fapl_free) {
            if((vol->fapl_free)(fapl) < 0)
                HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "driver free request failed")
        } /* end if */
        else
            H5MM_xfree(fapl);

        /* Decrement reference count for driver */
        if(H5I_dec_ref(vol_id) < 0)
            HGOTO_ERROR(H5E_VOL, H5E_CANTDEC, FAIL, "can't decrement reference count for vol plugin")
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_fapl_close() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_file_open
 *
 * Purpose:	Opens a file through the VOL.
 *
 * Return:      Success: User ID of the new file. This ID is of type
 *                       H5I_UID which contains the VOL id and the actual file ID
 *
 *		Failure: FAIL
 *
 * Programmer:	Mohamad Chaarawi
 *              January, 2012
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5VL_file_open(const char *name, unsigned flags, hid_t fcpl_id, hid_t fapl_id, hid_t dxpl_id)
{
    H5VL_class_t	*vol_plugin;            /* VOL for file */
    H5I_t               *uid_info;                /* user id structure */
    H5P_genplist_t      *plist;                 /* Property list pointer */
    hid_t               plugin_id = -1;         /* VOL ID */
    hid_t               file_id;
    hid_t		ret_value;              /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* get the VOL info from the fapl */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(fapl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access property list")
    if(H5P_get(plist, H5F_ACS_VOL_ID_NAME, &plugin_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get vol plugin ID")
    if(NULL == (vol_plugin = (H5VL_class_t *)H5I_object(plugin_id)))
	HGOTO_ERROR(H5E_VOL, H5E_BADVALUE, FAIL, "invalid vol plugin ID in file access property list")

    /* check if the corresponding VOL open callback exists */
    if(NULL == vol_plugin->file_cls.open)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `file open' method")
    /* call the corresponding VOL open callback */
    if((file_id = (vol_plugin->file_cls.open)(name, flags, fcpl_id, fapl_id, dxpl_id)) < 0)
	HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "open failed")

    /* Create a new id that points to a struct that holds the file id and the VOL id */
    /* Allocate new id structure */
    if(NULL == (uid_info = H5FL_MALLOC(H5I_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")
    uid_info->obj_id = file_id;
    uid_info->vol_id = plugin_id;

    if((ret_value = H5I_register(H5I_UID, uid_info, TRUE)) < 0)
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
 *                       H5I_UID which contains the VOL id and the actual file ID
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
    H5I_t               *uid_info;                /* user id structure */
    H5P_genplist_t      *plist;                 /* Property list pointer */
    hid_t               plugin_id = -1;         /* VOL ID */
    hid_t               file_id;
    hid_t		ret_value;              /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* get the VOL info from the fapl */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(fapl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access property list")
    if(H5P_get(plist, H5F_ACS_VOL_ID_NAME, &plugin_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get vol plugin ID")
            //printf ("VOL REF COUNT: %d\n",H5I_get_ref (plugin_id, FALSE));
    if(NULL == (vol_plugin = (H5VL_class_t *)H5I_object(plugin_id)))
	HGOTO_ERROR(H5E_VOL, H5E_BADVALUE, FAIL, "invalid vol plugin ID in file access property list")

    /* check if the corresponding VOL create callback exists */
    if(NULL == vol_plugin->file_cls.create)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `file create' method")
    /* call the corresponding VOL create callback */
    if((file_id = (vol_plugin->file_cls.create)(name, flags, fcpl_id, fapl_id)) < 0)
	HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "create failed")

    /* Create a new id that points to a struct that holds the file id and the VOL id */
    /* Allocate new id structure */
    if(NULL == (uid_info = H5FL_MALLOC(H5I_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")
    uid_info->obj_id = file_id;
    uid_info->vol_id = plugin_id;

    if((ret_value = H5I_register(H5I_UID, uid_info, TRUE)) < 0)
	HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to atomize file handle")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_file_create() */


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
    H5VL_class_t	*vol_plugin;            /* VOL for file */
    H5I_t               *uid_info;              /* user id structure */
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    /* Check/fix arguments. */
    if(H5I_UID != H5I_get_type(uid))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a user ID")

    /* get the ID struct */
    if(NULL == (uid_info = (H5I_t *)H5I_object(uid)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid user identifier")

    /* get VOL plugin info */
    if(NULL == (vol_plugin = (H5VL_class_t *)H5I_object(uid_info->vol_id)))
	HGOTO_ERROR(H5E_VOL, H5E_BADVALUE, FAIL, "invalid vol plugin ID in file")

    if(NULL == vol_plugin->file_cls.close)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `file close' method")
    if((ret_value = (vol_plugin->file_cls.close)(uid_info->obj_id)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTCLOSEFILE, FAIL, "close failed")

    if(H5I_dec_app_ref(uid) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINC, FAIL, "unable to decrement ref count on user ID")
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_file_close() */


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
    H5VL_class_t	*vol_plugin;            /* VOL for file */
    H5I_t               *uid_info;              /* user id structure */
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    /* Check/fix arguments. */
    if(H5I_UID != H5I_get_type(uid))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a user ID")

    /* get the ID struct */
    if(NULL == (uid_info = (H5I_t *)H5I_object(uid)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid user identifier")

    /* get VOL plugin info */
    if(NULL == (vol_plugin = (H5VL_class_t *)H5I_object(uid_info->vol_id)))
	HGOTO_ERROR(H5E_VOL, H5E_BADVALUE, FAIL, "invalid vol plugin ID in file")

    if(NULL == vol_plugin->file_cls.flush)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `file flush' method")
    if((ret_value = (vol_plugin->file_cls.flush)(uid_info->obj_id, scope)) < 0)
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
H5VL_file_get(hid_t uid, H5VL_file_get_t get_type, void *data, int argc, void **argv)
{
    H5VL_class_t     *vol_plugin;            /* VOL for file */
    H5I_t            *uid_info;              /* user id structure */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    /* Check/fix arguments. */
    if(H5I_UID != H5I_get_type(uid))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a user ID")

    /* get the ID struct */
    if(NULL == (uid_info = (H5I_t *)H5I_object(uid)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid user identifier")

    /* get VOL plugin info */
    if(NULL == (vol_plugin = (H5VL_class_t *)H5I_object(uid_info->vol_id)))
	HGOTO_ERROR(H5E_VOL, H5E_BADVALUE, FAIL, "invalid vol plugin ID in file")

    if(NULL == vol_plugin->file_cls.get)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `file get' method")
    if((ret_value = (vol_plugin->file_cls.get)(uid_info->obj_id, get_type, data, argc, argv)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "get failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_file_get() */
#if 0

/*-------------------------------------------------------------------------
 * Function:	H5VL_group_create
 *
 * Purpose:	Creates a group through the VOL
 *
 * Return:      Success: User ID of the new group. This ID is of type
 *                       H5I_UID which contains the VOL id and the actual group ID
 *
 *		Failure: FAIL
 *
 * Programmer:	Mohamad Chaarawi
 *              January, 2012
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5VL_group_create(hid_t uid, const char *name, hid_t lcpl_id, hid_t gcpl_id, hid_t gapl_id)
{
    H5VL_class_t	*vol_plugin;            /* VOL for group */
    H5I_t               *uid_info1;             /* user id structure of the location where the group will be created */
    H5I_t               *uid_info2;             /* user id structure of new created group*/
    hid_t               group_id;               /* actual group ID */
    hid_t		ret_value;              /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Check/fix arguments. */
    if(H5I_UID != H5I_get_type(uid))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a user ID")

    /* get the ID struct */
    if(NULL == (uid_info1 = (H5I_t *)H5I_object(uid)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid user identifier")

    /* get VOL plugin info */
    if(NULL == (vol_plugin = (H5VL_class_t *)H5I_object(uid_info1->vol_id)))
	HGOTO_ERROR(H5E_VOL, H5E_BADVALUE, FAIL, "invalid vol plugin ID in file")

    /* check if the corresponding VOL create callback exists */
    if(NULL == vol_plugin->object_cls.create)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `group create' method")
    /* call the corresponding VOL create callback */
    if((group_id = (vol_plugin->object_cls.create)
        (uid_info1->obj_id, name, lcpl_id, gcpl_id, gapl_id)) < 0)
	HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "create failed")

    /* Create a new id that points to a struct that holds the group id and the VOL id */
    /* Allocate new id structure */
    if(NULL == (uid_info2 = H5FL_MALLOC(H5I_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")
    uid_info2->obj_id = group_id;
    uid_info2->vol_id = uid_info1->vol_id;

    /* increment ref count on the VOL id */
    if(H5I_inc_ref(uid_info2->vol_id, FALSE) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINC, FAIL, "unable to increment ref count on vol plugin")

    if((ret_value = H5I_register(H5I_UID, uid_info2, TRUE)) < 0)
	HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to atomize group handle")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_group_create() */
#endif
