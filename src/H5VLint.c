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
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_fapl_close() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_close
 *
 * Purpose: decrements the ref count on the number of references
 *          outstanding for a VOL plugin
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Mohamad Chaarawi
 *		May, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_close(H5VL_class_t *vol_plugin)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    vol_plugin->nrefs--;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_close() */


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
    H5VL_class_t       *vol_plugin;            /* VOL structure attached to id */
    size_t              len;
    ssize_t             ret_value;

    FUNC_ENTER_NOAPI(FAIL)

    if (NULL == (vol_plugin = (H5VL_class_t *)H5I_get_aux (id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "Object/File does not contain VOL information")

    len = HDstrlen(vol_plugin->name);

    if(name) {
        HDstrncpy(name, vol_plugin->name, MIN(len + 1,size));
        if(len >= size)
            name[size-1]='\0';
    } /* end if */

    /* Set the return value for the API call */
    ret_value = (ssize_t)len;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_get_plugin_name() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_attr_create
 *
 * Purpose:	Creates an attribute through the VOL
 *
 * Return:      Success: User ID of the new attr. 
 *
 *		Failure: FAIL
 *
 * Programmer:	Mohamad Chaarawi
 *              April, 2012
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5VL_attr_create(hid_t id, const char *name, hid_t acpl_id, hid_t aapl_id, hid_t req)
{
    H5VL_class_t       *vol_plugin;            /* VOL structure attached to id */
    hid_t               ret_value;             /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    if (NULL == (vol_plugin = (H5VL_class_t *)H5I_get_aux (id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information")

    /* check if the corresponding VOL create callback exists */
    if(NULL == vol_plugin->attr_cls.create)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `attr create' method")

    /* call the corresponding VOL create callback */
    if((ret_value = (vol_plugin->attr_cls.create) (id, name, acpl_id, aapl_id, req)) < 0)
	HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "create failed")

    /* attach VOL information to the ID */
    if (H5I_register_aux(ret_value, vol_plugin, (H5I_free_t)H5VL_close) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "can't attach vol info to ID")
    vol_plugin->nrefs++;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_attr_create() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_attr_open
 *
 * Purpose:	Opens an attribute through the VOL
 *
 * Return:      Success: User ID of the new attr. 
 *
 *		Failure: FAIL
 *
 * Programmer:	Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5VL_attr_open(hid_t id, void *location, const char *name, hid_t aapl_id, hid_t req)
{
    H5VL_class_t       *vol_plugin;            /* VOL structure attached to id */
    hid_t		ret_value;             /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    if (NULL == (vol_plugin = (H5VL_class_t *)H5I_get_aux (id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information")

    /* check if the type specific corresponding VOL open callback exists */
    if(NULL == vol_plugin->attr_cls.open) {
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `attr open' method")
    }

    /* call the corresponding VOL open callback */
    if((ret_value = (vol_plugin->attr_cls.open) (id, location, name, aapl_id, req)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "open failed")

    /* attach VOL information to the ID */
    if (H5I_register_aux(ret_value, vol_plugin, (H5I_free_t)H5VL_close) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "can't attach vol info to ID")
    vol_plugin->nrefs++;

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
herr_t H5VL_attr_read(hid_t id, hid_t mem_type_id, void *buf, hid_t req)
{
    H5VL_class_t       *vol_plugin;            /* VOL structure attached to id */
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if (NULL == (vol_plugin = (H5VL_class_t *)H5I_get_aux (id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information")

    if(NULL == vol_plugin->attr_cls.read)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `attr read' method")
    if((ret_value = (vol_plugin->attr_cls.read)(id, mem_type_id, buf, req)) < 0)
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
herr_t H5VL_attr_write(hid_t id, hid_t mem_type_id, const void *buf, hid_t req)
{
    H5VL_class_t       *vol_plugin;            /* VOL structure attached to id */
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if (NULL == (vol_plugin = (H5VL_class_t *)H5I_get_aux (id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information")

    if(NULL == vol_plugin->attr_cls.write)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `attr write' method")
    if((ret_value = (vol_plugin->attr_cls.write)(id, mem_type_id, buf, req)) < 0)
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
H5VL_attr_get(hid_t id, H5VL_attr_get_t get_type, hid_t req, ...)
{
    va_list           arguments;             /* argument list passed from the API call */
    H5VL_class_t     *vol_plugin;            /* VOL structure attached to id */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if (NULL == (vol_plugin = (H5VL_class_t *)H5I_get_aux (id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information")

    if(NULL == vol_plugin->attr_cls.get)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `attr get' method")

    va_start (arguments, req);
    if((ret_value = (vol_plugin->attr_cls.get)(id, get_type, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "get failed")
    va_end (arguments);

    /* if the get_type is a named datatype, attach the vol info to it */
    if(H5VL_ATTR_GET_TYPE == get_type) {
        hid_t           *ret_id;

        va_start (arguments, req);
        ret_id = va_arg (arguments, hid_t *);

        if(H5Tcommitted(*ret_id)) {
            /* attach VOL information to the ID */
            if (H5I_register_aux(*ret_id, vol_plugin, (H5I_free_t)H5VL_close) < 0)
                HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "can't attach vol info to ID")
            vol_plugin->nrefs++;
        }
        va_end (arguments);
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_attr_get() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_attr_remove
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
H5VL_attr_remove(hid_t id, void *location, const char *attr_name, hid_t req)
{
    H5VL_class_t       *vol_plugin;            /* VOL structure attached to id */
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if (NULL == (vol_plugin = (H5VL_class_t *)H5I_get_aux (id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information")

    if((ret_value = (vol_plugin->attr_cls.remove)(id, location, attr_name, req)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTDELETE, FAIL, "remove failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_attr_remove() */


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
H5VL_attr_close(hid_t id, hid_t req)
{
    H5VL_class_t       *vol_plugin;            /* VOL structure attached to id */
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if (NULL == (vol_plugin = (H5VL_class_t *)H5I_get_aux(id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information")
            
    /* if the VOL class does not implement a specific attr close
       callback, try the object close */    
    if(NULL == vol_plugin->attr_cls.close){
        if(H5VL_object_close(id, req) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTRELEASE, FAIL, "unable to close object")
    }
    else {
        if((ret_value = (vol_plugin->attr_cls.close)(id, req)) < 0)
            HGOTO_ERROR(H5E_VOL, H5E_CANTRELEASE, FAIL, "close failed")
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
H5VL_datatype_commit(hid_t id, const char *name, hid_t type_id, hid_t lcpl_id, 
                     hid_t tcpl_id, hid_t tapl_id, hid_t req)
{
    H5VL_class_t       *vol_plugin;            /* VOL structure attached to id */
    herr_t		ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    if (NULL == (vol_plugin = (H5VL_class_t *)H5I_get_aux(id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information")

    /* check if the corresponding VOL commit callback exists */
    if(NULL == vol_plugin->datatype_cls.commit)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `datatype commit' method")

    /* call the corresponding VOL commit callback */
    if((ret_value = (vol_plugin->datatype_cls.commit) 
        (id, name, type_id, lcpl_id, tcpl_id, tapl_id, req)) < 0)
	HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "commit failed")

    if (H5I_register_aux(type_id, vol_plugin, (H5I_free_t)H5VL_close) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "can't attach vol info to ID")
    vol_plugin->nrefs++;

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
 *		Failure: FAIL
 *
 * Programmer:	Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5VL_datatype_open(hid_t id, const char *name, hid_t tapl_id, hid_t req)
{
    H5VL_class_t       *vol_plugin;            /* VOL structure attached to id */
    hid_t		ret_value;              /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    if (NULL == (vol_plugin = (H5VL_class_t *)H5I_get_aux(id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information")
    /* check if the type specific corresponding VOL open callback exists */
    if(NULL == vol_plugin->datatype_cls.open) {
        void       *location = NULL; /* a pointer to VOL specific token that indicates 
                                        the location of the object */

        /* Get the token for the Object location through the VOL */
        if(H5VL_object_lookup (id, H5VL_OBJECT_LOOKUP_BY_NAME, &location, req, name, tapl_id) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to locate object")

        /* Open the object through the VOL */
        if((ret_value = H5VL_object_open_by_loc(id, location, tapl_id, req)) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to open object")

        if (NULL != location) {
            /* free the location token through the VOL */
            if(H5VL_object_free_loc (id, location, H5_REQUEST_NULL) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTRELEASE, FAIL, "unable to free location token")
        }
    }
    else {
        /* call the corresponding VOL open callback */
        if((ret_value = (vol_plugin->datatype_cls.open)(id, name, tapl_id, req)) < 0)
            HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "open failed")

        /* attach VOL information to the ID */
        if (H5I_register_aux(ret_value, vol_plugin, (H5I_free_t)H5VL_close) < 0)
            HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "can't attach vol info to ID")
        vol_plugin->nrefs++;
    }
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_datatype_open() */


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
H5VL_datatype_close(hid_t id, hid_t req)
{
    H5VL_class_t       *vol_plugin;            /* VOL structure attached to id */
    herr_t		ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    if (NULL == (vol_plugin = (H5VL_class_t *)H5I_get_aux(id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information")

    /* check if the corresponding VOL close callback exists */
    if(NULL == vol_plugin->datatype_cls.close)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `datatype close' method")

    /* call the corresponding VOL close callback */
    if((ret_value = (vol_plugin->datatype_cls.close)(id, req)) < 0)
	HGOTO_ERROR(H5E_VOL, H5E_CANTRELEASE, FAIL, "close failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_datatype_close() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_dataset_create
 *
 * Purpose:	Creates a dataset through the VOL
 *
 * Return:      Success: User ID of the new dataset. 
 *
 *		Failure: FAIL
 *
 * Programmer:	Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5VL_dataset_create(hid_t id, const char *name, hid_t dcpl_id, hid_t dapl_id, hid_t req)
{
    H5VL_class_t       *vol_plugin;            /* VOL structure attached to id */
    hid_t               ret_value;             /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    if (NULL == (vol_plugin = (H5VL_class_t *)H5I_get_aux(id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information")

    /* check if the corresponding VOL create callback exists */
    if(NULL == vol_plugin->dataset_cls.create)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `dataset create' method")

    /* call the corresponding VOL create callback */
    if((ret_value = (vol_plugin->dataset_cls.create)(id, name, dcpl_id, dapl_id, req)) < 0)
	HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "create failed")

    /* attach VOL information to the ID */
    if (H5I_register_aux(ret_value, vol_plugin, (H5I_free_t)H5VL_close) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "can't attach vol info to ID")
    vol_plugin->nrefs++;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_dataset_create() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_dataset_open
 *
 * Purpose:	Opens a dataset through the VOL
 *
 * Return:      Success: User ID of the new dataset. 
 *
 *		Failure: FAIL
 *
 * Programmer:	Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5VL_dataset_open(hid_t id, const char *name, hid_t dapl_id, hid_t req)
{
    H5VL_class_t       *vol_plugin;            /* VOL structure attached to id */
    hid_t		ret_value;              /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    if (NULL == (vol_plugin = (H5VL_class_t *)H5I_get_aux(id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information")

    /* check if the type specific corresponding VOL open callback exists */
    if(NULL == vol_plugin->dataset_cls.open) {
        void       *location = NULL; /* a pointer to VOL specific token that indicates 
                                        the location of the object */

        /* Get the token for the Object location through the VOL */
        if(H5VL_object_lookup (id, H5VL_OBJECT_LOOKUP_BY_NAME, &location, req, name, dapl_id) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to locate object")

        /* Open the object through the VOL */
        if((ret_value = H5VL_object_open_by_loc(id, location, dapl_id, req)) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to open object")

        if (NULL != location) {
            /* free the location token through the VOL */
            if(H5VL_object_free_loc (id, location, H5_REQUEST_NULL) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTRELEASE, FAIL, "unable to free location token")
        }
    }
    else {
        /* call the corresponding VOL open callback */
        if((ret_value = (vol_plugin->dataset_cls.open)(id, name, dapl_id, req)) < 0)
            HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "open failed")

        /* attach VOL information to the ID */
        if (H5I_register_aux(ret_value, vol_plugin, (H5I_free_t)H5VL_close) < 0)
            HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "can't attach vol info to ID")
        vol_plugin->nrefs++;
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
herr_t H5VL_dataset_read(hid_t id, hid_t mem_type_id, hid_t mem_space_id, 
                         hid_t file_space_id, hid_t plist_id, void *buf, hid_t req)
{
    H5VL_class_t       *vol_plugin;            /* VOL structure attached to id */
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if (NULL == (vol_plugin = (H5VL_class_t *)H5I_get_aux(id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information")

    if(NULL == vol_plugin->dataset_cls.read)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `dataset read' method")

    if((ret_value = (vol_plugin->dataset_cls.read)
        (id, mem_type_id, mem_space_id, file_space_id, plist_id, buf, req)) < 0)
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
herr_t H5VL_dataset_write(hid_t id, hid_t mem_type_id, hid_t mem_space_id, 
                          hid_t file_space_id, hid_t plist_id, const void *buf, hid_t req)
{
    H5VL_class_t       *vol_plugin;            /* VOL structure attached to id */
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if (NULL == (vol_plugin = (H5VL_class_t *)H5I_get_aux(id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information")

    if(NULL == vol_plugin->dataset_cls.write)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `dataset write' method")

    if((ret_value = (vol_plugin->dataset_cls.write)
        (id, mem_type_id, mem_space_id, file_space_id, plist_id, buf, req)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_WRITEERROR, FAIL, "write failed")

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
herr_t H5VL_dataset_set_extent(hid_t id, const hsize_t size[], hid_t req)
{
    H5VL_class_t       *vol_plugin;            /* VOL structure attached to id */
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if (NULL == (vol_plugin = (H5VL_class_t *)H5I_get_aux(id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information")

    if(NULL == vol_plugin->dataset_cls.set_extent)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `dataset set_extent' method")

    if((ret_value = (vol_plugin->dataset_cls.set_extent)(id, size, req)) < 0)
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
H5VL_dataset_get(hid_t id, H5VL_dataset_get_t get_type, hid_t req, ...)
{
    va_list           arguments;             /* argument list passed from the API call */
    H5VL_class_t     *vol_plugin;            /* VOL structure attached to id */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if (NULL == (vol_plugin = (H5VL_class_t *)H5I_get_aux(id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information")

    if(NULL == vol_plugin->dataset_cls.get)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `dataset get' method")

    va_start (arguments, req);
    if((ret_value = (vol_plugin->dataset_cls.get)(id, get_type, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "get failed")
    va_end (arguments);

    /* if the get_type is a named datatype, create a wrapper for it */
    if(H5VL_DATASET_GET_TYPE == get_type) {
        hid_t           *ret_id;

        va_start (arguments, req);
        ret_id = va_arg (arguments, hid_t *);

        if(H5Tcommitted(*ret_id)) {
            /* attach VOL information to the ID */
            if (H5I_register_aux(*ret_id, vol_plugin, (H5I_free_t)H5VL_close) < 0)
                HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "can't attach vol info to ID")
            vol_plugin->nrefs++;
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
H5VL_dataset_close(hid_t id, hid_t req)
{
    H5VL_class_t       *vol_plugin;            /* VOL structure attached to id */
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if (NULL == (vol_plugin = (H5VL_class_t *)H5I_get_aux(id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information")
            
    /* if the VOL class does not implement a specific dataset close
       callback, try the object close */    
    if(NULL == vol_plugin->dataset_cls.close){
        if(H5VL_object_close(id, req) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTRELEASE, FAIL, "unable to close object")
    }
    else {
        if((ret_value = (vol_plugin->dataset_cls.close)(id, req)) < 0)
            HGOTO_ERROR(H5E_VOL, H5E_CANTRELEASE, FAIL, "close failed")
    }
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_dataset_close() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_file_open
 *
 * Purpose:	Opens a file through the VOL.
 *
 * Return:      Success: User ID of the new file. 
 *
 *		Failure: FAIL
 *
 * Programmer:	Mohamad Chaarawi
 *              January, 2012
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5VL_file_open(const char *name, unsigned flags, hid_t fapl_id, hid_t req)
{
    H5P_genplist_t     *plist;                 /* Property list pointer */
    H5VL_class_t       *vol_plugin;            /* VOL structure attached to id */
    hid_t		ret_value;             /* Return value */

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
    if((ret_value = (vol_plugin->file_cls.open)(name, flags, fapl_id, req)) < 0)
	HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "open failed")

    /* attach VOL information to the ID */
    if (H5I_register_aux(ret_value, vol_plugin, (H5I_free_t)H5VL_close) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "can't attach vol info to ID")
    vol_plugin->nrefs++;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_file_open() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_file_create
 *
 * Purpose:	Creates a file through the VOL
 *
 * Return:      Success: User ID of the new file. 
 *
 *		Failure: FAIL
 *
 * Programmer:	Mohamad Chaarawi
 *              January, 2012
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5VL_file_create(const char *name, unsigned flags, hid_t fcpl_id, hid_t fapl_id, hid_t req)
{
    H5P_genplist_t     *plist;                 /* Property list pointer */
    H5VL_class_t       *vol_plugin;            /* VOL structure attached to id */
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
    if((ret_value = (vol_plugin->file_cls.create)(name, flags, fcpl_id, fapl_id, req)) < 0)
	HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "create failed")

    /* attach VOL information to the ID */
    if (H5I_register_aux(ret_value, vol_plugin, (H5I_free_t)H5VL_close) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "can't attach vol info to ID")
    vol_plugin->nrefs++;

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
H5VL_file_flush(hid_t id, H5F_scope_t scope, hid_t req)
{
    H5VL_class_t       *vol_plugin;            /* VOL structure attached to id */
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if (NULL == (vol_plugin = (H5VL_class_t *)H5I_get_aux(id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information")

    if(NULL == vol_plugin->file_cls.flush)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `file flush' method")
    if((ret_value = (vol_plugin->file_cls.flush)(id, scope, req)) < 0)
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
H5VL_file_get(hid_t id, H5VL_file_get_t get_type, hid_t req, ...)
{
    va_list           arguments;             /* argument list passed from the API call */
    H5VL_class_t     *vol_plugin;            /* VOL structure attached to id */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if (NULL == (vol_plugin = (H5VL_class_t *)H5I_get_aux(id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information")

    if(NULL == vol_plugin->file_cls.get)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `file get' method")

    va_start(arguments, req);
    if((ret_value = (vol_plugin->file_cls.get)(id, get_type, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "get failed")
    va_end(arguments);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_file_get() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_file_misc
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
H5VL_file_misc(hid_t id, H5VL_file_misc_t misc_type, hid_t req, ...)
{
    va_list           arguments;             /* argument list passed from the API call */
    H5VL_class_t     *vol_plugin;            /* VOL structure attached to id */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if (NULL == (vol_plugin = (H5VL_class_t *)H5I_get_aux(id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information")

    if(NULL == vol_plugin->file_cls.misc)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `file misc' method")

    va_start (arguments, req);
    if((ret_value = (vol_plugin->file_cls.misc)(id, misc_type, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "misc failed")
    va_end (arguments);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_file_misc() */


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
H5VL_file_optional(hid_t id, H5VL_file_optional_t optional_type, hid_t req, ...)
{
    va_list           arguments;             /* argument list passed from the API call */
    H5VL_class_t     *vol_plugin;            /* VOL structure attached to id */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if (NULL == (vol_plugin = (H5VL_class_t *)H5I_get_aux(id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information")

    if(NULL == vol_plugin->file_cls.optional)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `file optional' method")

    va_start (arguments, req);
    if((ret_value = (vol_plugin->file_cls.optional)(id, optional_type, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "optional failed")
    va_end (arguments);

    /* if we are re-opening the file ,attach the VOL info to the new id */
    if(H5VL_FILE_REOPEN == optional_type) {
        hid_t           *ret_id;

        va_start (arguments, req);
        ret_id = va_arg (arguments, hid_t *);

        /* attach VOL information to the ID */
        if (H5I_register_aux(*ret_id, vol_plugin, (H5I_free_t)H5VL_close) < 0)
            HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "can't attach vol info to ID")
        vol_plugin->nrefs++;
        va_end (arguments);
    }

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
H5VL_file_close(hid_t id, hid_t req)
{
    H5VL_class_t       *vol_plugin;            /* VOL structure attached to id */
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if (NULL == (vol_plugin = (H5VL_class_t *)H5I_get_aux(id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information")

    if(NULL == vol_plugin->file_cls.close)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `file close' method")
    if((ret_value = (vol_plugin->file_cls.close)(id, req)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTCLOSEFILE, FAIL, "close failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_file_close() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_group_create
 *
 * Purpose:	Creates a group through the VOL
 *
 * Return:      Success: User ID of the new group.
 *
 *		Failure: FAIL
 *
 * Programmer:	Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5VL_group_create(hid_t id, const char *name, hid_t gcpl_id, hid_t gapl_id, hid_t req)
{
    H5VL_class_t       *vol_plugin;            /* VOL structure attached to id */
    hid_t		ret_value;              /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    if (NULL == (vol_plugin = (H5VL_class_t *)H5I_get_aux(id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information")

    /* check if the corresponding VOL create callback exists */
    if(NULL == vol_plugin->group_cls.create)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `group create' method")

    /* call the corresponding VOL create callback */
    if((ret_value = (vol_plugin->group_cls.create)(id, name, gcpl_id, gapl_id, req)) < 0)
	HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "create failed")

    /* attach VOL information to the ID */
    if (H5I_register_aux(ret_value, vol_plugin, (H5I_free_t)H5VL_close) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "can't attach vol info to ID")
    vol_plugin->nrefs++;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_group_create() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_group_open
 *
 * Purpose:	Opens a group through the VOL
 *
 * Return:      Success: User ID of the new group.
 *
 *		Failure: FAIL
 *
 * Programmer:	Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5VL_group_open(hid_t id, const char *name, hid_t gapl_id, hid_t req)
{
    H5VL_class_t       *vol_plugin;            /* VOL structure attached to id */
    hid_t		ret_value;              /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    if (NULL == (vol_plugin = (H5VL_class_t *)H5I_get_aux(id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information")

    /* check if the type specific corresponding VOL open callback exists */
    if(NULL == vol_plugin->group_cls.open) {
        void       *location = NULL; /* a pointer to VOL specific token that indicates 
                                        the location of the object */

        /* Get the token for the Object location through the VOL */
        if(H5VL_object_lookup (id, H5VL_OBJECT_LOOKUP_BY_NAME, &location, req, name, gapl_id) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to locate object")

        /* Open the object through the VOL */
        if((ret_value = H5VL_object_open_by_loc(id, location, gapl_id, req)) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to open object")

        if (NULL != location) {
            /* free the location token through the VOL */
            if(H5VL_object_free_loc (id, location, H5_REQUEST_NULL) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTRELEASE, FAIL, "unable to free location token")
        }
    }
    else {
        /* call the corresponding VOL open callback */
        if((ret_value = (vol_plugin->group_cls.open)(id, name, gapl_id, req)) < 0)
            HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "open failed")

        /* attach VOL information to the ID */
        if (H5I_register_aux(ret_value, vol_plugin, (H5I_free_t)H5VL_close) < 0)
            HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "can't attach vol info to ID")
        vol_plugin->nrefs++;
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
H5VL_group_get(hid_t id, H5VL_group_get_t get_type, hid_t req, ...)
{
    va_list           arguments;             /* argument list passed from the API call */
    H5VL_class_t     *vol_plugin;            /* VOL structure attached to id */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if (NULL == (vol_plugin = (H5VL_class_t *)H5I_get_aux(id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information")

    if(NULL == vol_plugin->group_cls.get)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `group get' method")

    va_start (arguments, req);
    if((ret_value = (vol_plugin->group_cls.get)(id, get_type, req, arguments)) < 0)
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
H5VL_group_close(hid_t id, hid_t req)
{
    H5VL_class_t       *vol_plugin;            /* VOL structure attached to id */
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if (NULL == (vol_plugin = (H5VL_class_t *)H5I_get_aux(id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information")

    /* if the VOL class does not implement a specific group close
       callback, try the object close */
    if(NULL == vol_plugin->group_cls.close) {
        if(H5VL_object_close(id, req) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTRELEASE, FAIL, "unable to close object")
    }
    else {
        if((ret_value = (vol_plugin->group_cls.close)(id, req)) < 0)
            HGOTO_ERROR(H5E_VOL, H5E_CANTRELEASE, FAIL, "close failed")
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
H5VL_link_create(H5VL_link_create_type_t create_type, hid_t id, const char *new_name, 
                 hid_t lcpl_id, hid_t lapl_id, hid_t req)
{
    H5VL_class_t         *vol_plugin = NULL;            /* VOL structure attached to id */
    herr_t               ret_value = SUCCEED;  /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* get VOL plugin */ 
    if(H5L_SAME_LOC != id) {
        if (NULL == (vol_plugin = (H5VL_class_t *)H5I_get_aux(id)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information")
    }
    else if(H5VL_LINK_CREATE_HARD == create_type && NULL == vol_plugin) {
        H5P_genplist_t *plist;      /* Property list pointer */
        hid_t cur_id;

        /* Get the plist structure */
        if(NULL == (plist = (H5P_genplist_t *)H5I_object(lcpl_id)))
            HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

        if(H5P_get(plist, H5VL_LINK_TARGET_ID, &cur_id) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get property value for current location id")

        if (NULL == (vol_plugin = (H5VL_class_t *)H5I_get_aux(cur_id)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information")
    }
    else {
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information")
    }

    /* check if the corresponding VOL create callback exists */
    if(NULL == vol_plugin->link_cls.create)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `link create' method")

    /* call the corresponding VOL create callback */
    if((ret_value = (vol_plugin->link_cls.create)
        (create_type, id, new_name, lcpl_id, lapl_id, req)) < 0)
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
                             const char *dst_name, hbool_t copy_flag, 
                             hid_t lcpl_id, hid_t lapl_id, hid_t req)
{
    H5VL_class_t	 *vol_plugin = NULL;   /* VOL plugin */
    herr_t               ret_value = SUCCEED;  /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* unwrap the higher level user ids. */ 
    if(H5L_SAME_LOC != src_loc_id) {
        if (NULL == (vol_plugin = (H5VL_class_t *)H5I_get_aux(src_loc_id)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information")
    }
    else if(H5L_SAME_LOC != dst_loc_id) {
        if (NULL == (vol_plugin = (H5VL_class_t *)H5I_get_aux(dst_loc_id)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information")
    }
    else {
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information")
    }

    /* check if the corresponding VOL move callback exists */
    if(NULL == vol_plugin->link_cls.move)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `link move' method")

    /* call the corresponding VOL move callback */
    if((ret_value = (vol_plugin->link_cls.move)
        (src_loc_id, src_name, dst_loc_id, dst_name, copy_flag, lcpl_id, lapl_id, req)) < 0)
	HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "link move failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_link_move() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_link_iterate
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
herr_t H5VL_link_iterate(hid_t loc_id, const char *name, hbool_t recursive, 
                         H5_index_t idx_type, H5_iter_order_t order, hsize_t *idx, 
                         H5L_iterate_t op, void *op_data, hid_t lapl_id)
{
    H5VL_class_t      *vol_plugin;            /* VOL structure attached to id */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if (NULL == (vol_plugin = (H5VL_class_t *)H5I_get_aux(loc_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information")

    if(NULL == vol_plugin->link_cls.iterate)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `link iterate' method")

    if((ret_value = (vol_plugin->link_cls.iterate)(loc_id, name, recursive, idx_type,
                                                   order, idx, op, op_data, lapl_id)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_BADITER, FAIL, "iteration failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)    
} /* end H5VL_link_iterate() */


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
H5VL_link_get(hid_t id, H5VL_link_get_t get_type, hid_t req, ...)
{
    va_list           arguments;             /* argument list passed from the API call */
    H5VL_class_t      *vol_plugin;            /* VOL structure attached to id */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if (NULL == (vol_plugin = (H5VL_class_t *)H5I_get_aux(id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information")

    if(NULL == vol_plugin->link_cls.get)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `link get' method")

    va_start (arguments, req);
    if((ret_value = (vol_plugin->link_cls.get)(id, get_type, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "get failed")
    va_end (arguments);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_link_get() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_link_remove
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
H5_DLL herr_t H5VL_link_remove(hid_t id, const char *name, void *udata, hid_t lapl_id, hid_t req)
{
    H5VL_class_t       *vol_plugin;            /* VOL structure attached to id */
    herr_t             ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    if (NULL == (vol_plugin = (H5VL_class_t *)H5I_get_aux(id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information")

    /* check if the corresponding VOL remove callback exists */
    if(NULL == vol_plugin->link_cls.remove)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `link remove' method")

    /* call the corresponding VOL remove callback */
    if((ret_value = (vol_plugin->link_cls.remove)(id, name, udata, lapl_id, req)) < 0)
	HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "link remove failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_link_remove() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_object_open_by_loc
 *
 * Purpose:	Opens a object through the VOL
 *
 * Return:      Success: User ID of the new object. 
 *
 *		Failure: FAIL
 *
 * Programmer:	Mohamad Chaarawi
 *              March, 2012
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5VL_object_open_by_loc(hid_t id, void *obj_loc, hid_t lapl_id, hid_t req)
{
    H5VL_class_t       *vol_plugin;            /* VOL structure attached to id */
    hid_t		ret_value;              /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    if (NULL == (vol_plugin = (H5VL_class_t *)H5I_get_aux(id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information")

    /* check if the corresponding VOL open callback exists */
    if(NULL == vol_plugin->object_cls.open)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `object open' method")

    /* call the corresponding VOL open callback */
    if((ret_value = (vol_plugin->object_cls.open)(obj_loc, lapl_id, req)) < 0)
	HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "open failed")

    /* attach VOL information to the ID */
    if (H5I_register_aux(ret_value, vol_plugin, (H5I_free_t)H5VL_close) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "can't attach vol info to ID")
    vol_plugin->nrefs++;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_object_open_by_loc() */


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
H5VL_object_copy(hid_t src_loc_id, const char *src_name, hid_t dst_loc_id, 
                 const char *dst_name, hid_t ocpypl_id, hid_t lcpl_id, hid_t req)
{
    H5VL_class_t       *vol_plugin1;            /* VOL structure attached to src id */
    H5VL_class_t       *vol_plugin2;            /* VOL structure attached to dest id */
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if (NULL == (vol_plugin1 = (H5VL_class_t *)H5I_get_aux(src_loc_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information")
    if (NULL == (vol_plugin2 = (H5VL_class_t *)H5I_get_aux(dst_loc_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information")

    /* check if both objects are associated with the same VOL plugin */
    if (vol_plugin1 != vol_plugin2)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "Objects are accessed through different VOL plugins and can't be copied")

    if(NULL == vol_plugin1->object_cls.copy)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `object copy' method")
    if((ret_value = (vol_plugin1->object_cls.copy)
        (src_loc_id, src_name, dst_loc_id, dst_name, ocpypl_id, lcpl_id, req)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTRELEASE, FAIL, "copy failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_object_copy() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_object_visit
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
herr_t H5VL_object_visit(hid_t loc_id, const char *obj_name, H5_index_t idx_type,
                         H5_iter_order_t order, H5O_iterate_t op, void *op_data, hid_t lapl_id)
{
    H5VL_class_t      *vol_plugin;            /* VOL structure attached to id */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if (NULL == (vol_plugin = (H5VL_class_t *)H5I_get_aux(loc_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information")

    if(NULL == vol_plugin->object_cls.visit)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `link iterate' method")

    if((ret_value = (vol_plugin->object_cls.visit)(loc_id, obj_name, idx_type, order, op, 
                                                   op_data, lapl_id)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_BADITER, FAIL, "object visitation failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)    
} /* end H5VL_object_visit() */


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
H5VL_object_lookup(hid_t id, H5VL_object_lookup_t lookup_type, void **location, hid_t req, ...)
{
    va_list           arguments;             /* argument list passed from the API call */
    H5VL_class_t      *vol_plugin;            /* VOL structure attached to id */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if (NULL == (vol_plugin = (H5VL_class_t *)H5I_get_aux(id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information")

    if(NULL == vol_plugin->object_cls.lookup)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `object lookup' method")

    va_start (arguments, req);
    if((ret_value = (vol_plugin->object_cls.lookup)(id, lookup_type, location, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "lookup of object location failed")
    va_end (arguments);
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_object_lookup() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_object_free_loc
 *
 * Purpose:	Free the location token
 *
 * Return:	Success:        non negative
 *		Failure:	negative
 *
 * Programmer:	Mohamad Chaarawi
 *              May, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_object_free_loc(hid_t id, void *location, hid_t req)
{
    H5VL_class_t       *vol_plugin;            /* VOL structure attached to id */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if (NULL == (vol_plugin = (H5VL_class_t *)H5I_get_aux(id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information")

    if(NULL == vol_plugin->object_cls.free_loc)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `object free_loc' method")

    if((ret_value = (vol_plugin->object_cls.free_loc)(location, req)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTRELEASE, FAIL, "freeing location token of object location failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_object_free_loc() */


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
H5VL_object_get(hid_t id, H5VL_object_get_t get_type, hid_t req, ...)
{
    va_list           arguments;             /* argument list passed from the API call */
    H5VL_class_t      *vol_plugin;            /* VOL structure attached to id */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if (NULL == (vol_plugin = (H5VL_class_t *)H5I_get_aux(id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information")

    if(NULL == vol_plugin->object_cls.get)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `object get' method")

    va_start (arguments, req);
    if((ret_value = (vol_plugin->object_cls.get)(id, get_type, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "get failed")
    va_end (arguments);
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_object_get() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_object_misc
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
H5VL_object_misc(hid_t id, H5VL_object_misc_t misc_type, hid_t req, ...)
{
    va_list           arguments;             /* argument list passed from the API call */
    H5VL_class_t      *vol_plugin;            /* VOL structure attached to id */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if (NULL == (vol_plugin = (H5VL_class_t *)H5I_get_aux(id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information")

    if(NULL == vol_plugin->object_cls.misc)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `object misc' method")

    va_start (arguments, req);
    if((ret_value = (vol_plugin->object_cls.misc)(id, misc_type, req, arguments)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "misc failed")
    va_end (arguments);

    if(H5VL_ATTR_OPEN_BY_IDX == misc_type) {
        hid_t	        *ret_id;

        va_start (arguments, req);
        ret_id = va_arg (arguments, hid_t *);

        /* attach VOL information to the ID */
        if (H5I_register_aux(*ret_id, vol_plugin, (H5I_free_t)H5VL_close) < 0)
            HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "can't attach vol info to ID")
        vol_plugin->nrefs++;

        va_end (arguments);
    }
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_object_misc() */


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
H5VL_object_close(hid_t id, hid_t req)
{
    H5VL_class_t       *vol_plugin;            /* VOL structure attached to id */
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    if (NULL == (vol_plugin = (H5VL_class_t *)H5I_get_aux(id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information")

    if(NULL == vol_plugin->object_cls.close)
	HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "vol plugin has no `object close' method")
    if((ret_value = (vol_plugin->object_cls.close)(id, req)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTRELEASE, FAIL, "close failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_object_close() */

