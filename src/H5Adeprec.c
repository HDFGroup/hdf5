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
#include "H5VLprivate.h"	/* Virtual Object Layer                 */


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
    FUNC_ENTER_NOAPI_NOINIT

    FUNC_LEAVE_NOAPI(H5A_init())
} /* H5A_init_deprec_interface() */

#ifndef H5_NO_DEPRECATED_SYMBOLS

/*--------------------------------------------------------------------------
 NAME
    H5Acreate1
 PURPOSE
    Creates an attribute on an object
 USAGE
    hid_t H5Acreate1(loc_id, name, type_id, space_id, plist_id)
        hid_t loc_id;       IN: Object (dataset or group) to be attached to
        const char *name;   IN: Name of attribute to create
        hid_t type_id;      IN: ID of datatype for attribute
        hid_t space_id;     IN: ID of dataspace for attribute
        hid_t plist_id;     IN: ID of creation property list (currently not used)
 RETURNS
    Non-negative on success/Negative on failure

 DESCRIPTION
        This function creates an attribute which is attached to the object
    specified with 'location_id'.  The name specified with 'name' for each
    attribute for an object must be unique for that object.  The 'type_id'
    and 'space_id' are created with the H5T and H5S interfaces respectively.
    The attribute ID returned from this function must be released with H5Aclose
    or resource leaks will develop.

 NOTE
    Deprecated in favor of H5Acreate2

--------------------------------------------------------------------------*/
hid_t
H5Acreate1(hid_t loc_id, const char *name, hid_t type_id, hid_t space_id,
	  hid_t plist_id)
{
    void *location = NULL;
    H5P_genplist_t      *plist;            /* Property list pointer */
    hid_t		ret_value;              /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE5("i", "i*siii", loc_id, name, type_id, space_id, plist_id);

    /* check arguments */
    if(H5I_ATTR == H5I_get_type(loc_id))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "location is not valid for an attribute")
    if(!name || !*name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name")

    /* Get correct property list */
    if(H5P_DEFAULT == plist_id)
        plist_id = H5P_ATTRIBUTE_CREATE_DEFAULT;

    /* Get the plist structure */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(plist_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* set creation properties */
    if(H5P_set(plist, H5A_CRT_TYPE_ID_NAME, &type_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't set property value for datatype id")
    if(H5P_set(plist, H5A_CRT_SPACE_ID_NAME, &space_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't set property value for space id")
    if(H5P_set(plist, H5A_CRT_LOCATION_NAME, &location) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't set property value for location")

    /* Create the attribute through the VOL */
    if((ret_value = H5VL_attr_create(loc_id, name, plist_id, H5P_DEFAULT, H5_REQUEST_NULL)) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to create attribute")

done:
    FUNC_LEAVE_API(ret_value)
} /* H5Acreate1() */


/*--------------------------------------------------------------------------
 NAME
    H5Aopen_name
 PURPOSE
    Opens an attribute for an object by looking up the attribute name
 USAGE
    hid_t H5Aopen_name (loc_id, name)
        hid_t loc_id;       IN: Object (dataset or group) to be attached to
        const char *name;   IN: Name of attribute to locate and open
 RETURNS
    ID of attribute on success, negative on failure

 DESCRIPTION
        This function opens an existing attribute for access.  The attribute
    name specified is used to look up the corresponding attribute for the
    object.  The attribute ID returned from this function must be released with
    H5Aclose or resource leaks will develop.
        The location object may be either a group or a dataset, both of
    which may have any sort of attribute.
 NOTE
    Deprecated in favor of H5Aopen
--------------------------------------------------------------------------*/
hid_t
H5Aopen_name(hid_t loc_id, const char *name)
{
    hid_t		ret_value;

    FUNC_ENTER_API(FAIL)
    H5TRACE2("i", "i*s", loc_id, name);

    /* check arguments */
    if(H5I_ATTR == H5I_get_type(loc_id))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "location is not valid for an attribute")
    if(!name || !*name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name")

    /* Open the attribute through the VOL */
    if((ret_value = H5VL_attr_open(loc_id, NULL, name, H5P_DEFAULT, H5_REQUEST_NULL)) < 0)
	HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to open attribute")

done:
    FUNC_LEAVE_API(ret_value)
} /* H5Aopen_name() */


/*--------------------------------------------------------------------------
 NAME
    H5Aopen_idx
 PURPOSE
    Opens the n'th attribute for an object
 USAGE
    hid_t H5Aopen_idx (loc_id, idx)
        hid_t loc_id;       IN: Object that attribute is attached to
        unsigned idx;       IN: Index (0-based) attribute to open
 RETURNS
    ID of attribute on success, negative on failure

 DESCRIPTION
        This function opens an existing attribute for access.  The attribute
    index specified is used to look up the corresponding attribute for the
    object.  The attribute ID returned from this function must be released with
    H5Aclose or resource leaks will develop.
        The location object may be either a group or a dataset, both of
    which may have any sort of attribute.
 NOTE
    Deprecated in favor of H5Aopen_by_idx
--------------------------------------------------------------------------*/
hid_t
H5Aopen_idx(hid_t loc_id, unsigned idx)
{
    hid_t	ret_value;

    FUNC_ENTER_API(FAIL)
    H5TRACE2("i", "iIu", loc_id, idx);

    /* check arguments */
    if(H5I_ATTR == H5I_get_type(loc_id))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "location is not valid for an attribute")

    if(H5VL_object_misc(loc_id, H5VL_ATTR_OPEN_BY_IDX, H5_REQUEST_NULL, &ret_value, ".", H5_INDEX_CRT_ORDER, 
                           H5_ITER_INC, (hsize_t)idx, H5P_DEFAULT, H5P_LINK_ACCESS_DEFAULT) < 0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTGET, FAIL, "unable to get dataset access properties")

done:
    FUNC_LEAVE_API(ret_value)
} /* H5Aopen_idx() */


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
 NOTE
    Deprecated in favor of H5Oget_info
--------------------------------------------------------------------------*/
int
H5Aget_num_attrs(hid_t loc_id)
{
    H5O_info_t oinfo;
    int	       ret_value;

    FUNC_ENTER_API(FAIL)
    H5TRACE1("Is", "i", loc_id);

    /* Get the group info through the VOL using the location token */
    if(H5VL_object_get(loc_id, H5VL_OBJECT_GET_INFO, H5_REQUEST_NULL, &oinfo, NULL) < 0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTGET, FAIL, "unable to get group info")

    ret_value = oinfo.num_attrs;

done:
    FUNC_LEAVE_API(ret_value)
} /* H5Aget_num_attrs() */


/*--------------------------------------------------------------------------
 NAME
    H5Aiterate1
 PURPOSE
    Calls a user's function for each attribute on an object
 USAGE
    herr_t H5Aiterate1(loc_id, attr_num, op, data)
        hid_t loc_id;       IN: Object (dataset or group) to be iterated over
        unsigned *attr_num; IN/OUT: Starting (IN) & Ending (OUT) attribute number
        H5A_operator1_t op;  IN: User's function to pass each attribute to
        void *op_data;      IN/OUT: User's data to pass through to iterator operator function
 RETURNS
        Returns a negative value if something is wrong, the return value of the
    last operator if it was non-zero, or zero if all attributes were processed.

 DESCRIPTION
        This function interates over the attributes of dataset or group
    specified with 'loc_id'.  For each attribute of the object, the
    'op_data' and some additional information (specified below) are passed
    to the 'op' function.  The iteration begins with the '*attr_number'
    object in the group and the next attribute to be processed by the operator
    is returned in '*attr_number'.
        The operation receives the ID for the group or dataset being iterated
    over ('loc_id'), the name of the current attribute about the object
    ('attr_name') and the pointer to the operator data passed in to H5Aiterate
    ('op_data').  The return values from an operator are:
        A. Zero causes the iterator to continue, returning zero when all
            attributes have been processed.
        B. Positive causes the iterator to immediately return that positive
            value, indicating short-circuit success.  The iterator can be
            restarted at the next attribute.
        C. Negative causes the iterator to immediately return that value,
            indicating failure.  The iterator can be restarted at the next
            attribute.
 NOTE
    Deprecated in favor of H5Aiterate2
--------------------------------------------------------------------------*/
herr_t
H5Aiterate1(hid_t loc_id, unsigned *attr_num, H5A_operator1_t op, void *op_data)
{
    H5A_attr_iter_op_t  attr_op;        /* Attribute operator */
    hsize_t		start_idx;      /* Index of attribute to start iterating at */
    hsize_t		last_attr;      /* Index of last attribute examined */
    herr_t	        ret_value;      /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE4("e", "i*Iux*x", loc_id, attr_num, op, op_data);

    /* check arguments */
    if(H5I_ATTR == H5I_get_type(loc_id))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "location is not valid for an attribute")

    /* Build attribute operator info */
    attr_op.op_type = H5A_ATTR_OP_APP;
    attr_op.u.app_op = op;

    /* Call attribute iteration routine */
    last_attr = start_idx = (hsize_t)(attr_num ? *attr_num : 0);
    if((ret_value = H5O_attr_iterate(loc_id, H5AC_ind_dxpl_id, H5_INDEX_CRT_ORDER, H5_ITER_INC, start_idx, &last_attr, &attr_op, op_data)) < 0)
        HERROR(H5E_ATTR, H5E_BADITER, "error iterating over attributes");

    /* Set the last attribute information */
    if(attr_num)
        *attr_num = (unsigned)last_attr;

done:
    FUNC_LEAVE_API(ret_value)
} /* H5Aiterate1() */
#endif /* H5_NO_DEPRECATED_SYMBOLS */

