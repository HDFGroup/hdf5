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

    FUNC_ENTER_NOAPI_NOINIT

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
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5VLregister_object */
