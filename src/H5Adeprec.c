/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have     *
 * access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu. *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*-------------------------------------------------------------------------
 *
 * Created:	H5Adeprec.c
 *		November 27 2006
 *		Quincey Koziol <koziol@hdfgroup.org>
 *
 * Purpose:	Deprecated functions from the H5A interface.  These
 *              functions are here for compatibility purposes and may be
 *              removed in the future.  Applications should switch to the
 *              newer APIs.
 *
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/

#define H5A_PACKAGE		/*suppress error about including H5Apkg   */
#define H5O_PACKAGE		/*suppress error about including H5Opkg	*/

/* Interface initialization */
#define H5_INTERFACE_INIT_FUNC	H5A_init_deprec_interface


/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Apkg.h"		/* Attributes				*/
#include "H5Dprivate.h"		/* Datasets				*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5Opkg.h"             /* Object headers			*/


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
   H5A_init_deprec_interface -- Initialize interface-specific information
USAGE
    herr_t H5A_init_deprec_interface()
RETURNS
    Non-negative on success/Negative on failure
DESCRIPTION
    Initializes any interface-specific data or routines.  (Just calls
    H5A_init() currently).

--------------------------------------------------------------------------*/
static herr_t
H5A_init_deprec_interface(void)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5A_init_deprec_interface)

    FUNC_LEAVE_NOAPI(H5A_init())
} /* H5A_init_deprec_interface() */


/*--------------------------------------------------------------------------
 NAME
    H5Aget_num_attrs
 PURPOSE
    Determines the number of attributes attached to an object
 NOTE
    Deprecated in favor of H5Oget_info[_by_idx]
 USAGE
    int H5Aget_num_attrs (loc_id)
        hid_t loc_id;       IN: Object (dataset or group) to be queried
 RETURNS
    Number of attributes on success, negative on failure
 DESCRIPTION
        This function returns the number of attributes attached to a dataset or
    group, 'location_id'.
--------------------------------------------------------------------------*/
int
H5Aget_num_attrs(hid_t loc_id)
{
    H5O_loc_t    	*loc;	/* Object location for attribute */
    void           	*obj;
    int			ret_value;

    FUNC_ENTER_API(H5Aget_num_attrs, FAIL)
    H5TRACE1("Is", "i", loc_id);

    /* check arguments */
    if(H5I_FILE == H5I_get_type(loc_id) || H5I_ATTR == H5I_get_type(loc_id))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "location is not valid for an attribute")
    if(NULL == (obj = H5I_object(loc_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADATOM, FAIL, "illegal object atom")
    switch(H5I_get_type (loc_id)) {
        case H5I_DATASET:
            if(NULL == (loc = H5D_oloc((H5D_t*)obj)))
                HGOTO_ERROR(H5E_ATTR, H5E_CANTGET, FAIL, "can't get location for object")
            break;

        case H5I_DATATYPE:
            if(NULL == (loc = H5T_oloc((H5T_t*)obj)))
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "target datatype is not committed")
            break;

        case H5I_GROUP:
            if(NULL == (loc = H5G_oloc((H5G_t*)obj)))
                HGOTO_ERROR(H5E_ATTR, H5E_CANTGET, FAIL, "can't get location for object")
            break;

        default:
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "inappropriate attribute target")
    } /*lint !e788 All appropriate cases are covered */

    /* Look up the # of attributes for the object */
    if((ret_value = H5O_attr_count(loc, H5AC_ind_dxpl_id)) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTCOUNT, FAIL, "can't get attribute count for object")

done:
    FUNC_LEAVE_API(ret_value)
} /* H5Aget_num_attrs() */

