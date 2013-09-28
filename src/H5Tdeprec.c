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

/*-------------------------------------------------------------------------
 *
 * Created:	H5Tdeprec.c
 *		April 5 2007
 *		Quincey Koziol <koziol@hdfgroup.org>
 *
 * Purpose:	Deprecated functions from the H5T interface.  These
 *              functions are here for compatibility purposes and may be
 *              removed in the future.  Applications should switch to the
 *              newer APIs.
 *
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/

#define H5T_PACKAGE		/*suppress error about including H5Tpkg   */

/* Interface initialization */
#define H5_INTERFACE_INIT_FUNC	H5T_init_deprec_interface


/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5ACprivate.h"        /* Metadata cache                       */
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5FOprivate.h"	/* File objects				*/
#include "H5Iprivate.h"		/* IDs					*/
#include "H5Ppublic.h"		/* Property Lists			*/
#include "H5Tpkg.h"		/* Datatypes				*/
#include "H5VLprivate.h"	/* VOL plugins				*/

#ifndef H5_NO_DEPRECATED_SYMBOLS
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


/*********************/
/* Package Variables */
/*********************/


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/



/*--------------------------------------------------------------------------
NAME
   H5T_init_deprec_interface -- Initialize interface-specific information
USAGE
    herr_t H5T_init_deprec_interface()
RETURNS
    Non-negative on success/Negative on failure
DESCRIPTION
    Initializes any interface-specific data or routines.  (Just calls
    H5T_init() currently).

--------------------------------------------------------------------------*/
static herr_t
H5T_init_deprec_interface(void)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    FUNC_LEAVE_NOAPI(H5T_init())
} /* H5T_init_deprec_interface() */


/*-------------------------------------------------------------------------
 * Function:	H5Tcommit1
 *
 * Purpose:	Save a transient datatype to a file and turn the type handle
 *		into a named, immutable type.
 *
 * Note:	Deprecated in favor of H5Tcommit2
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Monday, June  1, 1998
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Tcommit1(hid_t loc_id, const char *name, hid_t type_id)
{
    void    *dt = NULL;
    H5T_t   *type = NULL;
    void    *obj = NULL;        /* object token of loc_id */
    H5VL_t  *vol_plugin;        /* VOL plugin information */
    H5VL_loc_params_t loc_params;
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE3("e", "i*si", loc_id, name, type_id);

    /* Check arguments */
    if (H5Tcommitted(type_id))
	HGOTO_ERROR(H5E_ARGS, H5E_CANTSET, FAIL, "datatype is already committed")
    if(!name || !*name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name")
    if(NULL == (type = (H5T_t *)H5I_object_verify(type_id, H5I_DATATYPE)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a datatype")

    loc_params.type = H5VL_OBJECT_BY_SELF;
    loc_params.obj_type = H5I_get_type(loc_id);

    /* get the object from the loc_id */
    if(NULL == (obj = (void *)H5I_object(loc_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid object identifier")
    /* get the plugin pointer */
    if (NULL == (vol_plugin = (H5VL_t *)H5I_get_aux(loc_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information")

    /* commit the datatype through the VOL */
    if (NULL == (dt = H5VL_datatype_commit(obj, loc_params, vol_plugin, name, type_id, 
                                           H5P_LINK_CREATE_DEFAULT, H5P_DATATYPE_CREATE_DEFAULT, 
                                           H5P_DATATYPE_ACCESS_DEFAULT, H5AC_dxpl_id, H5_EVENT_STACK_NULL)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to commit datatype")

    /* attach the vol object created using the commit call to the 
       library datatype structure */
    /* set the committed type object to the VOL pluging pointer in the H5T_t struct */
    type->vol_obj = dt;

    /* attach VOL information to the ID */
    if (H5I_register_aux(type_id, vol_plugin) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "can't attach vol info to ID")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Tcommit1() */


/*-------------------------------------------------------------------------
 * Function:	H5Topen1
 *
 * Purpose:	Opens a named datatype.
 *
 * Note:	Deprecated in favor of H5Topen2
 *
 * Return:	Success:	Object ID of the named datatype.
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *              Monday, June  1, 1998
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Topen1(hid_t loc_id, const char *name)
{
    void    *dt = NULL;       /* datatype token from VOL plugin */
    void    *obj = NULL;        /* object token of loc_id */
    H5VL_t  *vol_plugin;        /* VOL plugin information */
    H5VL_loc_params_t loc_params;
    hid_t     ret_value = FAIL;      /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("i", "i*s", loc_id, name);

    /* Check args */
     if(!name || !*name)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name")

    loc_params.type = H5VL_OBJECT_BY_SELF;
    loc_params.obj_type = H5I_get_type(loc_id);

    /* get the file object */
    if(NULL == (obj = (void *)H5I_object(loc_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid file identifier")
    /* get the plugin pointer */
    if (NULL == (vol_plugin = (H5VL_t *)H5I_get_aux(loc_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information")

    /* Create the datatype through the VOL */
    if(NULL == (dt = H5VL_datatype_open(obj, loc_params, vol_plugin, name, 
                                        H5P_DATATYPE_ACCESS_DEFAULT, H5AC_dxpl_id, H5_EVENT_STACK_NULL)))
	HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to open datatype")

    /* Get an atom for the datatype */
    if ((ret_value = H5VL_create_datatype(dt, vol_plugin, TRUE)) < 0)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to atomize datatype handle")

done:
    if (ret_value < 0 && dt)
        if(H5VL_datatype_close (dt, vol_plugin, H5AC_dxpl_id, H5_EVENT_STACK_NULL) < 0)
            HDONE_ERROR(H5E_SYM, H5E_CLOSEERROR, FAIL, "unable to release dataset")
    FUNC_LEAVE_API(ret_value)
} /* end H5Topen1() */
#endif /* H5_NO_DEPRECATED_SYMBOLS */

