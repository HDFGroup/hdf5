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

/****************/
/* Module Setup */
/****************/

#define H5A_PACKAGE		/*suppress error about including H5Apkg	*/
#define H5O_PACKAGE		/*suppress error about including H5Opkg	*/

/* Interface initialization */
#define H5_INTERFACE_INIT_FUNC	H5A_init_interface


/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Apkg.h"		/* Attributes				*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5FLprivate.h"	/* Free Lists				*/
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Opkg.h"		/* Object headers			*/
#include "H5Sprivate.h"		/* Dataspace functions			*/
#include "H5SMprivate.h"	/* Shared Object Header Messages	*/
#include "H5VLprivate.h"	/* VOL plugins				*/

/****************/
/* Local Macros */
/****************/

/* The number of reserved IDs in dataset ID group */
#define H5A_RESERVED_ATOMS  0


/******************/
/* Local Typedefs */
/******************/

/* Object header iterator callbacks */
/* Data structure for callback for locating the index by name */
typedef struct H5A_iter_cb1 {
    const char *name;
    int idx;
} H5A_iter_cb1;


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

/* Declare the free lists of H5A_t */
H5FL_DEFINE(H5A_t);

/* Declare the free lists for H5A_shared_t's */
H5FL_DEFINE(H5A_shared_t);

/* Declare a free list to manage blocks of type conversion data */
H5FL_BLK_DEFINE(attr_buf);


/*-------------------------------------------------------------------------
 * Function:	H5A_init
 *
 * Purpose:	Initialize the interface from some other package.
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Monday, November 27, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5A_init(void)
{
    herr_t ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(FAIL)
    /* FUNC_ENTER() does all the work */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5A_init() */


/*--------------------------------------------------------------------------
NAME
   H5A_init_interface -- Initialize interface-specific information
USAGE
    herr_t H5A_init_interface()

RETURNS
    Non-negative on success/Negative on failure
DESCRIPTION
    Initializes any interface-specific data or routines.

--------------------------------------------------------------------------*/
static herr_t
H5A_init_interface(void)
{
    herr_t ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /*
     * Create attribute ID type.
     */
    if(H5I_register_type(H5I_ATTR, (size_t)H5I_ATTRID_HASHSIZE, H5A_RESERVED_ATOMS, (H5I_free_t)H5A_close) < H5I_FILE)
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTINIT, FAIL, "unable to initialize interface")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5A_init_interface() */


/*--------------------------------------------------------------------------
 NAME
    H5A_term_interface
 PURPOSE
    Terminate various H5A objects
 USAGE
    void H5A_term_interface()
 RETURNS
 DESCRIPTION
    Release any other resources allocated.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
     Can't report errors...
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
int
H5A_term_interface(void)
{
    int	n = 0;

    FUNC_ENTER_NOAPI_NOINIT

    if(H5_interface_initialize_g) {
	if((n = H5I_nmembers(H5I_ATTR))>0) {
	    (void)H5I_clear_type(H5I_ATTR, FALSE, FALSE);
	} else {
	    (void)H5I_dec_type_ref(H5I_ATTR);
	    H5_interface_initialize_g = 0;
	    n = 1;
	}
    }
    FUNC_LEAVE_NOAPI(n)
} /* H5A_term_interface() */


/*--------------------------------------------------------------------------
 NAME
    H5Acreate2
 PURPOSE
    Creates an attribute on an object
 USAGE
    hid_t H5Acreate2(loc_id, attr_name, type_id, space_id, acpl_id,
            aapl_id)
        hid_t loc_id;       IN: Object (dataset or group) to be attached to
        const char *attr_name;  IN: Name of attribute to locate and open
        hid_t type_id;          IN: ID of datatype for attribute
        hid_t space_id;         IN: ID of dataspace for attribute
        hid_t acpl_id;          IN: ID of creation property list (currently not used)
        hid_t aapl_id;          IN: Attribute access property list
 RETURNS
    Non-negative on success/Negative on failure

 DESCRIPTION
        This function creates an attribute which is attached to the object
    specified with 'loc_id'.  The name specified with 'attr_name' for
    each attribute for an object must be unique for that object.  The 'type_id'
    and 'space_id' are created with the H5T and H5S interfaces respectively.
    The 'aapl_id' property list is currently unused, but will be used in the
    future for optional attribute access properties.  The attribute ID returned
    from this function must be released with H5Aclose or resource leaks will
    develop.

--------------------------------------------------------------------------*/
/* ARGSUSED */
hid_t
H5Acreate2(hid_t loc_id, const char *attr_name, hid_t type_id, hid_t space_id,
    hid_t acpl_id, hid_t UNUSED aapl_id)
{
    H5P_genplist_t      *plist;            /* Property list pointer */
    H5VL_loc_params_t loc_params;
    hid_t		ret_value;              /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE6("i", "i*siiii", loc_id, attr_name, type_id, space_id, acpl_id, aapl_id);

    /* check arguments */
    if(H5I_ATTR == H5I_get_type(loc_id))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "location is not valid for an attribute")
    if(!attr_name || !*attr_name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no attribute name")

    /* Get correct property list */
    if(H5P_DEFAULT == acpl_id)
        acpl_id = H5P_ATTRIBUTE_CREATE_DEFAULT;

    loc_params.type = H5VL_OBJECT_BY_ID;
    loc_params.loc_data.loc_by_id.id = loc_id;

    /* Get the plist structure */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(acpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* set creation properties */
    if(H5P_set(plist, H5VL_ATTR_TYPE_ID, &type_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't set property value for datatype id")
    if(H5P_set(plist, H5VL_ATTR_SPACE_ID, &space_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't set property value for space id")
    if(H5P_set(plist, H5VL_ATTR_LOC_PARAMS, &loc_params) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't set property value for location")

    /* Create the attribute through the VOL */
    if((ret_value = H5VL_attr_create(loc_id, attr_name, acpl_id, aapl_id, H5_REQUEST_NULL)) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to create attribute")

done:
    FUNC_LEAVE_API(ret_value)
} /* H5Acreate2() */


/*--------------------------------------------------------------------------
 NAME
    H5Acreate_by_name
 PURPOSE
    Creates an attribute on an object
 USAGE
    hid_t H5Acreate_by_name(loc_id, obj_name, attr_name, type_id, space_id, acpl_id,
            aapl_id, lapl_id)
        hid_t loc_id;       IN: Object (dataset or group) to be attached to
        const char *obj_name;   IN: Name of object relative to location
        const char *attr_name;  IN: Name of attribute to locate and open
        hid_t type_id;          IN: ID of datatype for attribute
        hid_t space_id;         IN: ID of dataspace for attribute
        hid_t acpl_id;          IN: ID of creation property list (currently not used)
        hid_t aapl_id;          IN: Attribute access property list
        hid_t lapl_id;          IN: Link access property list
 RETURNS
    Non-negative on success/Negative on failure

 DESCRIPTION
        This function creates an attribute which is attached to the object
    specified with 'loc_id/obj_name'.  The name specified with 'attr_name' for
    each attribute for an object must be unique for that object.  The 'type_id'
    and 'space_id' are created with the H5T and H5S interfaces respectively.
    The 'aapl_id' property list is currently unused, but will be used in the
    future for optional attribute access properties.  The attribute ID returned
    from this function must be released with H5Aclose or resource leaks will
    develop.

--------------------------------------------------------------------------*/
/* ARGSUSED */
hid_t
H5Acreate_by_name(hid_t loc_id, const char *obj_name, const char *attr_name,
    hid_t type_id, hid_t space_id, hid_t acpl_id, hid_t aapl_id,
    hid_t lapl_id)
{
    H5P_genplist_t      *plist;            /* Property list pointer */
    H5VL_loc_params_t    loc_params;
    hid_t		 ret_value;        /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE8("i", "i*s*siiiii", loc_id, obj_name, attr_name, type_id, space_id,
             acpl_id, aapl_id, lapl_id);

    /* check arguments */
    if(H5I_ATTR == H5I_get_type(loc_id))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "location is not valid for an attribute")
    if(!obj_name || !*obj_name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no object name")
    if(!attr_name || !*attr_name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no attribute name")

    /* Get correct property list */
    if(H5P_DEFAULT == acpl_id)
        acpl_id = H5P_ATTRIBUTE_CREATE_DEFAULT;

    loc_params.type = H5VL_OBJECT_BY_NAME;
    loc_params.loc_data.loc_by_name.name = obj_name;
    loc_params.loc_data.loc_by_name.plist_id = lapl_id;

    /* Get the plist structure */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(acpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* set creation properties */
    if(H5P_set(plist, H5VL_ATTR_TYPE_ID, &type_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't set property value for datatype id")
    if(H5P_set(plist, H5VL_ATTR_SPACE_ID, &space_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't set property value for space id")
    if(H5P_set(plist, H5VL_ATTR_LOC_PARAMS, &loc_params) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't set property value for location")

    /* Create the attribute through the VOL */
    if((ret_value = H5VL_attr_create(loc_id, attr_name, acpl_id, aapl_id, H5_REQUEST_NULL)) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to create attribute")

done:
    FUNC_LEAVE_API(ret_value)
} /* H5Acreate_by_name() */


/*--------------------------------------------------------------------------
 NAME
    H5Aopen
 PURPOSE
    Opens an attribute for an object by looking up the attribute name
 USAGE
    hid_t H5Aopen(loc_id, attr_name, aapl_id)
        hid_t loc_id;           IN: Object that attribute is attached to
        const char *attr_name;  IN: Name of attribute to locate and open
        hid_t aapl_id;          IN: Attribute access property list
 RETURNS
    ID of attribute on success, negative on failure

 DESCRIPTION
        This function opens an existing attribute for access.  The attribute
    name specified is used to look up the corresponding attribute for the
    object.  The attribute ID returned from this function must be released with
    H5Aclose or resource leaks will develop.
--------------------------------------------------------------------------*/
hid_t
H5Aopen(hid_t loc_id, const char *attr_name, hid_t aapl_id)
{
    H5VL_loc_params_t loc_params; 
    hid_t		ret_value;

    FUNC_ENTER_API(FAIL)
    H5TRACE3("i", "i*si", loc_id, attr_name, aapl_id);

    /* check arguments */
    if(H5I_ATTR == H5I_get_type(loc_id))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "location is not valid for an attribute")
    if(!attr_name || !*attr_name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no attribute name")

    loc_params.type = H5VL_OBJECT_BY_ID;
    loc_params.loc_data.loc_by_id.id = loc_id;

    /* Open the attribute through the VOL */
    if((ret_value = H5VL_attr_open(loc_id, loc_params, attr_name, aapl_id, H5_REQUEST_NULL)) < 0)
	HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, FAIL, "unable to open attribute")

done:
    FUNC_LEAVE_API(ret_value)
} /* H5Aopen() */


/*--------------------------------------------------------------------------
 NAME
    H5Aopen_by_name
 PURPOSE
    Opens an attribute for an object by looking up the attribute name
 USAGE
    hid_t H5Aopen_by_name(loc_id, obj_name, attr_name, aapl_id, lapl_id)
        hid_t loc_id;           IN: Object that attribute is attached to
        const char *obj_name;   IN: Name of object relative to location
        const char *attr_name;  IN: Name of attribute to locate and open
        hid_t aapl_id;          IN: Attribute access property list
        hid_t lapl_id;          IN: Link access property list
 RETURNS
    ID of attribute on success, negative on failure

 DESCRIPTION
        This function opens an existing attribute for access.  The attribute
    name specified is used to look up the corresponding attribute for the
    object.  The attribute ID returned from this function must be released with
    H5Aclose or resource leaks will develop.
--------------------------------------------------------------------------*/
hid_t
H5Aopen_by_name(hid_t loc_id, const char *obj_name, const char *attr_name,
    hid_t aapl_id, hid_t lapl_id)
{
    H5VL_loc_params_t   loc_params;
    hid_t		ret_value;

    FUNC_ENTER_API(FAIL)
    H5TRACE5("i", "i*s*sii", loc_id, obj_name, attr_name, aapl_id, lapl_id);

    /* check arguments */
    if(H5I_ATTR == H5I_get_type(loc_id))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "location is not valid for an attribute")
    if(!obj_name || !*obj_name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no object name")
    if(!attr_name || !*attr_name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no attribute name")
    if(H5P_DEFAULT == lapl_id)
        lapl_id = H5P_LINK_ACCESS_DEFAULT;
    else
        if(TRUE != H5P_isa_class(lapl_id, H5P_LINK_ACCESS))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not link access property list ID")

    loc_params.type = H5VL_OBJECT_BY_NAME;
    loc_params.loc_data.loc_by_name.name = obj_name;
    loc_params.loc_data.loc_by_name.plist_id = lapl_id;

    /* Open the attribute through the VOL */
    if((ret_value = H5VL_attr_open(loc_id, loc_params, attr_name, aapl_id, H5_REQUEST_NULL)) < 0)
	HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, FAIL, "unable to open attribute")

done:
    FUNC_LEAVE_API(ret_value)
} /* H5Aopen_by_name() */


/*--------------------------------------------------------------------------
 NAME
    H5Aopen_by_idx
 PURPOSE
    Opens the n'th attribute for an object, according to the order within
    an index
 USAGE
    hid_t H5Aopen_by_idx(loc_id, obj_ame, idx_type, order, n, aapl_id, lapl_id)
        hid_t loc_id;           IN: Object that attribute is attached to
        const char *obj_name;   IN: Name of object relative to location
        H5_index_t idx_type;    IN: Type of index to use
        H5_iter_order_t order;  IN: Order to iterate over index
        hsize_t n;              IN: Index (0-based) attribute to open
        hid_t aapl_id;          IN: Attribute access property list
        hid_t lapl_id;          IN: Link access property list
 RETURNS
    ID of attribute on success, negative on failure

 DESCRIPTION
        This function opens an existing attribute for access.  The attribute
    index specified is used to look up the corresponding attribute for the
    object.  The attribute ID returned from this function must be released with
    H5Aclose or resource leaks will develop.
--------------------------------------------------------------------------*/
hid_t
H5Aopen_by_idx(hid_t loc_id, const char *obj_name, H5_index_t idx_type,
    H5_iter_order_t order, hsize_t n, hid_t UNUSED aapl_id, hid_t lapl_id)
{
    hid_t	ret_value;      /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE7("i", "i*sIiIohii", loc_id, obj_name, idx_type, order, n, aapl_id,
             lapl_id);

    /* check arguments */
    if(H5I_ATTR == H5I_get_type(loc_id))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "location is not valid for an attribute")
    if(!obj_name || !*obj_name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no object name")
    if(idx_type <= H5_INDEX_UNKNOWN || idx_type >= H5_INDEX_N)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid index type specified")
    if(order <= H5_ITER_UNKNOWN || order >= H5_ITER_N)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid iteration order specified")
    if(H5P_DEFAULT == lapl_id)
        lapl_id = H5P_LINK_ACCESS_DEFAULT;
    else
        if(TRUE != H5P_isa_class(lapl_id, H5P_LINK_ACCESS))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not link access property list ID")

    if(H5VL_object_misc(loc_id, H5VL_ATTR_OPEN_BY_IDX, H5_REQUEST_NULL, &ret_value, 
                         obj_name, idx_type, order, n, aapl_id, lapl_id) < 0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTINIT, FAIL, "unable to open attribute")

done:
    FUNC_LEAVE_API(ret_value)
} /* H5Aopen_by_idx() */


/*--------------------------------------------------------------------------
 NAME
    H5Awrite
 PURPOSE
    Write out data to an attribute
 USAGE
    herr_t H5Awrite (attr_id, dtype_id, buf)
        hid_t attr_id;       IN: Attribute to write
        hid_t dtype_id;       IN: Memory datatype of buffer
        const void *buf;     IN: Buffer of data to write
 RETURNS
    Non-negative on success/Negative on failure

 DESCRIPTION
        This function writes a complete attribute to disk.
--------------------------------------------------------------------------*/
herr_t
H5Awrite(hid_t attr_id, hid_t dtype_id, const void *buf)
{
    herr_t ret_value;           /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE3("e", "ii*x", attr_id, dtype_id, buf);

    /* check arguments */
    if(NULL == buf)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "null attribute buffer")

    /* write the data through the VOL */
    if((ret_value = H5VL_attr_write(attr_id, dtype_id, buf, H5_REQUEST_NULL)) < 0)
	HGOTO_ERROR(H5E_ATTR, H5E_READERROR, FAIL, "can't read data")

done:
    FUNC_LEAVE_API(ret_value)
} /* H5Awrite() */


/*--------------------------------------------------------------------------
 NAME
    H5Aread
 PURPOSE
    Read in data from an attribute
 USAGE
    herr_t H5Aread (attr_id, dtype_id, buf)
        hid_t attr_id;       IN: Attribute to read
        hid_t dtype_id;       IN: Memory datatype of buffer
        void *buf;           IN: Buffer for data to read
 RETURNS
    Non-negative on success/Negative on failure

 DESCRIPTION
        This function reads a complete attribute from disk.
--------------------------------------------------------------------------*/
herr_t
H5Aread(hid_t attr_id, hid_t dtype_id, void *buf)
{
    herr_t ret_value;           /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE3("e", "ii*x", attr_id, dtype_id, buf);

    /* check arguments */
    if(NULL == buf)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "null attribute buffer")

    /* Read the data through the VOL */
    if((ret_value = H5VL_attr_read(attr_id, dtype_id, buf, H5_REQUEST_NULL)) < 0)
	HGOTO_ERROR(H5E_ATTR, H5E_READERROR, FAIL, "can't read data")

done:
    FUNC_LEAVE_API(ret_value)
} /* H5Aread() */


/*--------------------------------------------------------------------------
 NAME
    H5Aget_space
 PURPOSE
    Gets a copy of the dataspace for an attribute
 USAGE
    hid_t H5Aget_space (attr_id)
        hid_t attr_id;       IN: Attribute to get dataspace of
 RETURNS
    A dataspace ID on success, negative on failure

 DESCRIPTION
        This function retrieves a copy of the dataspace for an attribute.
    The dataspace ID returned from this function must be released with H5Sclose
    or resource leaks will develop.
--------------------------------------------------------------------------*/
hid_t
H5Aget_space(hid_t attr_id)
{
    hid_t	ret_value;

    FUNC_ENTER_API(FAIL)
    H5TRACE1("i", "i", attr_id);

    /* get the dataspace through the VOL */
    if(H5VL_attr_get(attr_id, H5VL_ATTR_GET_SPACE, H5_REQUEST_NULL, &ret_value) < 0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTGET, FAIL, "unable to get data space")

done:
    FUNC_LEAVE_API(ret_value)
} /* H5Aget_space() */


/*--------------------------------------------------------------------------
 NAME
    H5Aget_type
 PURPOSE
    Gets a copy of the datatype for an attribute
 USAGE
    hid_t H5Aget_type (attr_id)
        hid_t attr_id;       IN: Attribute to get datatype of
 RETURNS
    A datatype ID on success, negative on failure

 DESCRIPTION
        This function retrieves a copy of the datatype for an attribute.
    The datatype ID returned from this function must be released with H5Tclose
    or resource leaks will develop.
--------------------------------------------------------------------------*/
hid_t
H5Aget_type(hid_t attr_id)
{
    hid_t	 ret_value;     /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE1("i", "i", attr_id);

    /* get the datatype through the VOL */
    if(H5VL_attr_get(attr_id, H5VL_ATTR_GET_TYPE, H5_REQUEST_NULL, &ret_value) < 0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTGET, FAIL, "unable to get type")

done:
    FUNC_LEAVE_API(ret_value)
} /* H5Aget_type() */


/*--------------------------------------------------------------------------
 NAME
    H5Aget_create_plist
 PURPOSE
    Gets a copy of the creation property list for an attribute
 USAGE
    hssize_t H5Aget_create_plist (attr_id, buf_size, buf)
        hid_t attr_id;      IN: Attribute to get name of
 RETURNS
    This function returns the ID of a copy of the attribute's creation
    property list, or negative on failure.

 ERRORS

 DESCRIPTION
        This function returns a copy of the creation property list for
    an attribute.  The resulting ID must be closed with H5Pclose() or
    resource leaks will occur.
--------------------------------------------------------------------------*/
hid_t
H5Aget_create_plist(hid_t attr_id)
{
    hid_t		ret_value;

    FUNC_ENTER_API(FAIL)
    H5TRACE1("i", "i", attr_id);

    HDassert(H5P_LST_ATTRIBUTE_CREATE_g != -1);

    /* get the acpl through the VOL */
    if(H5VL_attr_get(attr_id, H5VL_ATTR_GET_ACPL, H5_REQUEST_NULL, &ret_value) < 0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTGET, FAIL, "unable to get acpl")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Aget_create_plist() */


/*--------------------------------------------------------------------------
 NAME
    H5Aget_name
 PURPOSE
    Gets a copy of the name for an attribute
 USAGE
    hssize_t H5Aget_name (attr_id, buf_size, buf)
        hid_t attr_id;      IN: Attribute to get name of
        size_t buf_size;    IN: The size of the buffer to store the string in.
        char *buf;          IN: Buffer to store name in
 RETURNS
    This function returns the length of the attribute's name (which may be
    longer than 'buf_size') on success or negative for failure.

 DESCRIPTION
        This function retrieves the name of an attribute for an attribute ID.
    Up to 'buf_size' characters are stored in 'buf' followed by a '\0' string
    terminator.  If the name of the attribute is longer than 'buf_size'-1,
    the string terminator is stored in the last position of the buffer to
    properly terminate the string.
--------------------------------------------------------------------------*/
ssize_t
H5Aget_name(hid_t attr_id, size_t buf_size, char *buf)
{
    ssize_t		ret_value;

    FUNC_ENTER_API(FAIL)
    H5TRACE3("Zs", "iz*s", attr_id, buf_size, buf);

    /* check arguments */
    if(!buf && buf_size)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid buffer")

    /* get the name through the VOL */
    if(H5VL_attr_get(attr_id, H5VL_ATTR_GET_NAME, H5_REQUEST_NULL, &ret_value, buf_size, buf) < 0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTGET, FAIL, "unable to get name")

done:
    FUNC_LEAVE_API(ret_value)
} /* H5Aget_name() */


/*-------------------------------------------------------------------------
 * Function:	H5Aget_name_by_idx
 *
 * Purpose:	Retrieve name of an attribute, according to the
 *		order within an index.
 *
 *              Same pattern of behavior as H5Iget_name.
 *
 * Return:	Success:	Non-negative length of name, with information
 *				in NAME buffer
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *              February  8, 2007
 *
 *-------------------------------------------------------------------------
 */
ssize_t
H5Aget_name_by_idx(hid_t loc_id, const char *obj_name, H5_index_t idx_type,
    H5_iter_order_t order, hsize_t n, char *name /*out*/, size_t size,
    hid_t lapl_id)
{
    hid_t       attr_id = -1;
    ssize_t	ret_value;      /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE8("Zs", "i*sIiIohxzi", loc_id, obj_name, idx_type, order, n, name, size,
             lapl_id);

    /* Check args */
    if(H5I_ATTR == H5I_get_type(loc_id))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "location is not valid for an attribute")
    if(!obj_name || !*obj_name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name")
    if(idx_type <= H5_INDEX_UNKNOWN || idx_type >= H5_INDEX_N)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid index type specified")
    if(order <= H5_ITER_UNKNOWN || order >= H5_ITER_N)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid iteration order specified")
    if(H5P_DEFAULT == lapl_id)
        lapl_id = H5P_LINK_ACCESS_DEFAULT;
    else
        if(TRUE != H5P_isa_class(lapl_id, H5P_LINK_ACCESS))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not link access property list ID")

    if(H5VL_object_misc(loc_id, H5VL_ATTR_OPEN_BY_IDX, H5_REQUEST_NULL, &attr_id, 
                         obj_name, idx_type, order, n, H5P_DEFAULT, lapl_id) < 0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTINIT, FAIL, "unable to open attribute")

    /* get the name through the VOL */
    if(H5VL_attr_get(attr_id, H5VL_ATTR_GET_NAME, H5_REQUEST_NULL, &ret_value, size, name) < 0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTGET, FAIL, "unable to get name")

#if 0
    /* Open the attribute on the object header */
    if(NULL == (attr = H5A_open_by_idx(&loc, obj_name, idx_type, order, n, lapl_id, H5AC_ind_dxpl_id)))
        HGOTO_ERROR(H5E_ATTR, H5E_CANTOPENOBJ, FAIL, "can't open attribute")

    /* Get the length of the name */
    ret_value = (ssize_t)HDstrlen(attr->shared->name);

    /* Copy the name into the user's buffer, if given */
    if(name) {
        HDstrncpy(name, attr->shared->name, MIN((size_t)(ret_value + 1), size));
        if((size_t)ret_value >= size)
            name[size - 1]='\0';
    } /* end if */
#endif

done:
    /* Release resources */
    if(attr_id > 0 && H5VL_attr_close(attr_id, H5_REQUEST_NULL) < 0)
        HDONE_ERROR(H5E_ATTR, H5E_CANTFREE, FAIL, "can't close attribute")

    FUNC_LEAVE_API(ret_value)
} /* end H5Aget_name_by_idx() */


/*-------------------------------------------------------------------------
 * Function:	H5Aget_storage_size
 *
 * Purpose:	Returns the amount of storage size that is required for this
 *		attribute.
 *
 * Return:	Success:	The amount of storage size allocated for the
 *				attribute.  The return value may be zero
 *                              if no data has been stored.
 *
 *		Failure:	Zero
 *
 * Programmer:	Raymond Lu
 *              October 23, 2002
 *
 *-------------------------------------------------------------------------
 */
hsize_t
H5Aget_storage_size(hid_t attr_id)
{
    hsize_t	ret_value;      /* Return value */

    FUNC_ENTER_API(0)
    H5TRACE1("h", "i", attr_id);

    /* get the storage size through the VOL */
    if(H5VL_attr_get(attr_id, H5VL_ATTR_GET_STORAGE_SIZE, H5_REQUEST_NULL, &ret_value) < 0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTGET, 0, "unable to get acpl")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Aget_storage_size() */


/*-------------------------------------------------------------------------
 * Function:	H5Aget_info
 *
 * Purpose:	Retrieve information about an attribute.
 *
 * Return:	Success:	Non-negative
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *              February  6, 2007
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Aget_info(hid_t attr_id, H5A_info_t *ainfo)
{
    herr_t	ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "i*x", attr_id, ainfo);

    /* get the attribute info through the VOL */
    if(H5VL_attr_get(attr_id, H5VL_ATTR_GET_INFO, H5_REQUEST_NULL, ainfo) < 0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTGET, FAIL, "unable to get attribute info")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Aget_info() */


/*-------------------------------------------------------------------------
 * Function:	H5Aget_info_by_name
 *
 * Purpose:	Retrieve information about an attribute by name.
 *
 * Return:	Success:	Non-negative
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *              February  6, 2007
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Aget_info_by_name(hid_t loc_id, const char *obj_name, const char *attr_name,
    H5A_info_t *ainfo, hid_t lapl_id)
{
    H5VL_loc_params_t   loc_params;
    hid_t attr_id = -1;
    herr_t	ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE5("e", "i*s*s*xi", loc_id, obj_name, attr_name, ainfo, lapl_id);

    /* Check args */
    if(H5I_ATTR == H5I_get_type(loc_id))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "location is not valid for an attribute")
    if(!obj_name || !*obj_name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no object name")
    if(!attr_name || !*attr_name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no attribute name")
    if(NULL == ainfo)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid info pointer")
    if(H5P_DEFAULT == lapl_id)
        lapl_id = H5P_LINK_ACCESS_DEFAULT;
    else
        if(TRUE != H5P_isa_class(lapl_id, H5P_LINK_ACCESS))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not link access property list ID")

    loc_params.type = H5VL_OBJECT_BY_NAME;
    loc_params.loc_data.loc_by_name.name = obj_name;
    loc_params.loc_data.loc_by_name.plist_id = lapl_id;

    /* Open the attribute through the VOL */
    if((attr_id = H5VL_attr_open(loc_id, loc_params, attr_name, H5P_DEFAULT, H5_REQUEST_NULL)) < 0)
	HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, FAIL, "unable to open attribute")

    /* get the attribute info through the VOL */
    if(H5VL_attr_get(attr_id, H5VL_ATTR_GET_INFO, H5_REQUEST_NULL, ainfo) < 0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTGET, FAIL, "unable to get attribute info")
done:
    /* release resources */
    if(attr_id > 0 && H5VL_attr_close(attr_id, H5_REQUEST_NULL) < 0)
        HDONE_ERROR(H5E_ATTR, H5E_CANTFREE, FAIL, "can't close attribute")

    FUNC_LEAVE_API(ret_value)
} /* end H5Aget_info_by_name() */


/*-------------------------------------------------------------------------
 * Function:	H5Aget_info_by_idx
 *
 * Purpose:	Retrieve information about an attribute, according to the
 *		order within an index.
 *
 * Return:	Success:	Non-negative with information in AINFO
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *              February  8, 2007
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Aget_info_by_idx(hid_t loc_id, const char *obj_name, H5_index_t idx_type,
    H5_iter_order_t order, hsize_t n, H5A_info_t *ainfo, hid_t lapl_id)
{
    hid_t attr_id = FAIL;
    herr_t	ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE7("e", "i*sIiIoh*xi", loc_id, obj_name, idx_type, order, n, ainfo,
             lapl_id);

    /* Check args */
    if(H5I_ATTR == H5I_get_type(loc_id))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "location is not valid for an attribute")
    if(!obj_name || !*obj_name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name")
    if(idx_type <= H5_INDEX_UNKNOWN || idx_type >= H5_INDEX_N)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid index type specified")
    if(order <= H5_ITER_UNKNOWN || order >= H5_ITER_N)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid iteration order specified")
    if(NULL == ainfo)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid info pointer")
    if(H5P_DEFAULT == lapl_id)
        lapl_id = H5P_LINK_ACCESS_DEFAULT;
    else
        if(TRUE != H5P_isa_class(lapl_id, H5P_LINK_ACCESS))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not link access property list ID")

#if 0
    /* Open the attribute on the object header */
    if(NULL == (attr = H5A_open_by_idx(&loc, obj_name, idx_type, order, n, lapl_id, H5AC_ind_dxpl_id)))
        HGOTO_ERROR(H5E_ATTR, H5E_CANTOPENOBJ, FAIL, "can't open attribute")

    /* Get the attribute information */
    if(H5A_get_info(attr, ainfo) < 0)
	HGOTO_ERROR(H5E_ATTR, H5E_CANTGET, FAIL, "unable to get attribute info")
#endif

    /* open the attribute through the VOL */
    if(H5VL_object_misc(loc_id, H5VL_ATTR_OPEN_BY_IDX, H5_REQUEST_NULL, &attr_id, 
                         obj_name, idx_type, order, n, H5P_DEFAULT, lapl_id) < 0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTINIT, FAIL, "unable to open attribute")

    /* get the attribute info through the VOL */
    if(H5VL_attr_get(attr_id, H5VL_ATTR_GET_INFO, H5_REQUEST_NULL, ainfo) < 0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTGET, FAIL, "unable to get attribute info")

done:
    /* Release resources */
    if(attr_id && H5VL_attr_close(attr_id, H5_REQUEST_NULL) < 0)
        HDONE_ERROR(H5E_ATTR, H5E_CANTFREE, FAIL, "can't close attribute")

    FUNC_LEAVE_API(ret_value)
} /* end H5Aget_info_by_idx() */


/*-------------------------------------------------------------------------
 * Function:	H5Arename
 *
 * Purpose:     Rename an attribute
 *
 * Return:	Success:             Non-negative
 *		Failure:             Negative
 *
 * Programmer:	Raymond Lu
 *              October 23, 2002
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Arename(hid_t loc_id, const char *old_name, const char *new_name)
{
    herr_t	ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE3("e", "i*s*s", loc_id, old_name, new_name);

    /* check arguments */
    if(!old_name || !new_name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "name is nil")
    if(H5I_ATTR == H5I_get_type(loc_id))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "location is not valid for an attribute")

    /* Avoid thrashing things if the names are the same */
    if(HDstrcmp(old_name, new_name)) {
        H5VL_loc_params_t loc_params;

        loc_params.type = H5VL_OBJECT_BY_ID;
        loc_params.loc_data.loc_by_id.id = loc_id;

        /* rename the attribute info through the VOL */
        if(H5VL_object_misc(loc_id, H5VL_ATTR_RENAME, H5_REQUEST_NULL, loc_params, old_name, new_name) < 0)
            HGOTO_ERROR(H5E_ATTR, H5E_CANTRENAME, FAIL, "can't rename attribute")
    }
done:
    FUNC_LEAVE_API(ret_value)
} /* H5Arename() */


/*-------------------------------------------------------------------------
 * Function:	H5Arename_by_name
 *
 * Purpose:     Rename an attribute
 *
 * Return:	Success:             Non-negative
 *		Failure:             Negative
 *
 * Programmer:	Quincey Koziol
 *              February 20, 2007
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Arename_by_name(hid_t loc_id, const char *obj_name, const char *old_attr_name,
    const char *new_attr_name, hid_t lapl_id)
{
    herr_t	ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE5("e", "i*s*s*si", loc_id, obj_name, old_attr_name, new_attr_name,
             lapl_id);

    /* check arguments */
    if(H5I_ATTR == H5I_get_type(loc_id))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "location is not valid for an attribute")
    if(!obj_name || !*obj_name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no object name")
    if(!old_attr_name || !*old_attr_name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no old attribute name")
    if(!new_attr_name || !*new_attr_name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no new attribute name")
    if(H5P_DEFAULT == lapl_id)
        lapl_id = H5P_LINK_ACCESS_DEFAULT;
    else
        if(TRUE != H5P_isa_class(lapl_id, H5P_LINK_ACCESS))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not link access property list ID")

    /* Avoid thrashing things if the names are the same */
    if(HDstrcmp(old_attr_name, new_attr_name)) {
        H5VL_loc_params_t loc_params;

        loc_params.type = H5VL_OBJECT_BY_NAME;
        loc_params.loc_data.loc_by_name.name = obj_name;
        loc_params.loc_data.loc_by_name.plist_id = lapl_id;

        /* get the attribute info through the VOL */
        if(H5VL_object_misc(loc_id, H5VL_ATTR_RENAME, H5_REQUEST_NULL, loc_params, old_attr_name, new_attr_name) < 0)
            HGOTO_ERROR(H5E_INTERNAL, H5E_CANTGET, FAIL, "unable to get attribute info")
    } /* end if */

done:
    FUNC_LEAVE_API(ret_value)
} /* H5Arename_by_name() */


/*--------------------------------------------------------------------------
 NAME
    H5Aiterate2
 PURPOSE
    Calls a user's function for each attribute on an object
 USAGE
    herr_t H5Aiterate2(loc_id, idx_type, order, idx, op, op_data)
        hid_t loc_id;           IN: Base location for object
        H5_index_t idx_type;    IN: Type of index to use
        H5_iter_order_t order;  IN: Order to iterate over index
        hsize_t *idx;           IN/OUT: Starting (IN) & Ending (OUT) attribute
                                    in index & order
        H5A_operator2_t op;     IN: User's function to pass each attribute to
        void *op_data;          IN/OUT: User's data to pass through to iterator
                                    operator function
 RETURNS
        Returns a negative value if an error occurs, the return value of the
    last operator if it was non-zero (which can be a negative value), or zero
    if all attributes were processed.

 DESCRIPTION
        This function interates over the attributes of dataset or group
    specified with 'loc_id' & 'obj_name'.  For each attribute of the object,
    the 'op_data' and some additional information (specified below) are passed
    to the 'op' function.  The iteration begins with the '*idx'
    object in the group and the next attribute to be processed by the operator
    is returned in '*idx'.
        The operation receives the ID for the group or dataset being iterated
    over ('loc_id'), the name of the current attribute about the object
    ('attr_name'), the attribute's "info" struct ('ainfo') and the pointer to
    the operator data passed in to H5Aiterate2 ('op_data').  The return values
    from an operator are:
        A. Zero causes the iterator to continue, returning zero when all
            attributes have been processed.
        B. Positive causes the iterator to immediately return that positive
            value, indicating short-circuit success.  The iterator can be
            restarted at the next attribute.
        C. Negative causes the iterator to immediately return that value,
            indicating failure.  The iterator can be restarted at the next
            attribute.
--------------------------------------------------------------------------*/
herr_t
H5Aiterate2(hid_t loc_id, H5_index_t idx_type, H5_iter_order_t order,
    hsize_t *idx, H5A_operator2_t op, void *op_data)
{
    H5A_attr_iter_op_t attr_op; /* Attribute operator */
    hsize_t	start_idx;      /* Index of attribute to start iterating at */
    hsize_t	last_attr;      /* Index of last attribute examined */
    herr_t	ret_value;      /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE6("e", "iIiIo*hx*x", loc_id, idx_type, order, idx, op, op_data);

    /* check arguments */
    if(H5I_ATTR == H5I_get_type(loc_id))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "location is not valid for an attribute")
    if(idx_type <= H5_INDEX_UNKNOWN || idx_type >= H5_INDEX_N)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid index type specified")
    if(order <= H5_ITER_UNKNOWN || order >= H5_ITER_N)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid iteration order specified")

    /* Build attribute operator info */
    attr_op.op_type = H5A_ATTR_OP_APP2;
    attr_op.u.app_op2 = op;

    /* Call attribute iteration routine */
    last_attr = start_idx = (idx ? *idx : 0);
    if((ret_value = H5O_attr_iterate(loc_id, H5AC_ind_dxpl_id, idx_type, order, start_idx, &last_attr, &attr_op, op_data)) < 0)
        HERROR(H5E_ATTR, H5E_BADITER, "error iterating over attributes");

    /* Set the last attribute information */
    if(idx)
        *idx = last_attr;

done:
    FUNC_LEAVE_API(ret_value)
} /* H5Aiterate2() */


/*--------------------------------------------------------------------------
 NAME
    H5Aiterate_by_name
 PURPOSE
    Calls a user's function for each attribute on an object
 USAGE
    herr_t H5Aiterate2(loc_id, obj_name, idx_type, order, idx, op, op_data, lapl_id)
        hid_t loc_id;           IN: Base location for object
        const char *obj_name;   IN: Name of object relative to location
        H5_index_t idx_type;    IN: Type of index to use
        H5_iter_order_t order;  IN: Order to iterate over index
        hsize_t *idx;           IN/OUT: Starting (IN) & Ending (OUT) attribute
                                    in index & order
        H5A_operator2_t op;     IN: User's function to pass each attribute to
        void *op_data;          IN/OUT: User's data to pass through to iterator
                                    operator function
        hid_t lapl_id;          IN: Link access property list
 RETURNS
        Returns a negative value if an error occurs, the return value of the
    last operator if it was non-zero (which can be a negative value), or zero
    if all attributes were processed.

 DESCRIPTION
        This function interates over the attributes of dataset or group
    specified with 'loc_id' & 'obj_name'.  For each attribute of the object,
    the 'op_data' and some additional information (specified below) are passed
    to the 'op' function.  The iteration begins with the '*idx'
    object in the group and the next attribute to be processed by the operator
    is returned in '*idx'.
        The operation receives the ID for the group or dataset being iterated
    over ('loc_id'), the name of the current attribute about the object
    ('attr_name'), the attribute's "info" struct ('ainfo') and the pointer to
    the operator data passed in to H5Aiterate_by_name ('op_data').  The return values
    from an operator are:
        A. Zero causes the iterator to continue, returning zero when all
            attributes have been processed.
        B. Positive causes the iterator to immediately return that positive
            value, indicating short-circuit success.  The iterator can be
            restarted at the next attribute.
        C. Negative causes the iterator to immediately return that value,
            indicating failure.  The iterator can be restarted at the next
            attribute.
--------------------------------------------------------------------------*/
herr_t
H5Aiterate_by_name(hid_t loc_id, const char *obj_name, H5_index_t idx_type,
    H5_iter_order_t order, hsize_t *idx, H5A_operator2_t op, void *op_data,
    hid_t lapl_id)
{
    hid_t       obj_loc_id = (-1);      /* ID for object located */
    H5A_attr_iter_op_t attr_op; /* Attribute operator */
    hsize_t	start_idx;      /* Index of attribute to start iterating at */
    hsize_t	last_attr;      /* Index of last attribute examined */
    H5VL_loc_params_t loc_params;
    herr_t	ret_value;      /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE8("e", "i*sIiIo*hx*xi", loc_id, obj_name, idx_type, order, idx, op,
             op_data, lapl_id);

    /* check arguments */
    if(H5I_ATTR == H5I_get_type(loc_id))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "location is not valid for an attribute")
    if(!obj_name || !*obj_name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no object name")
    if(idx_type <= H5_INDEX_UNKNOWN || idx_type >= H5_INDEX_N)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid index type specified")
    if(order <= H5_ITER_UNKNOWN || order >= H5_ITER_N)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid iteration order specified")
    if(H5P_DEFAULT == lapl_id)
        lapl_id = H5P_LINK_ACCESS_DEFAULT;
    else
        if(TRUE != H5P_isa_class(lapl_id, H5P_LINK_ACCESS))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not link access property list ID")

    loc_params.type = H5VL_OBJECT_BY_NAME;
    loc_params.loc_data.loc_by_name.name = obj_name;
    loc_params.loc_data.loc_by_name.plist_id = lapl_id;

    /* Open the object through the VOL */
    if((obj_loc_id = H5VL_object_open(loc_id, loc_params, H5_REQUEST_NULL)) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to open object")

    /* Build attribute operator info */
    attr_op.op_type = H5A_ATTR_OP_APP2;
    attr_op.u.app_op2 = op;

    /* Call attribute iteration routine */
    last_attr = start_idx = (idx ? *idx : 0);
    if((ret_value = H5O_attr_iterate(obj_loc_id, H5AC_ind_dxpl_id, idx_type, order, start_idx, &last_attr, &attr_op, op_data)) < 0)
        HERROR(H5E_ATTR, H5E_BADITER, "error iterating over attributes");

    /* Set the last attribute information */
    if(idx)
        *idx = last_attr;

done:
    /* Release resources */
    if(obj_loc_id > 0) {
        if(H5I_dec_app_ref(obj_loc_id) < 0)
            HDONE_ERROR(H5E_ATTR, H5E_CANTDEC, FAIL, "unable to close temporary object")
    } /* end if */

    FUNC_LEAVE_API(ret_value)
} /* H5Aiterate_by_name() */


/*--------------------------------------------------------------------------
 NAME
    H5Adelete
 PURPOSE
    Deletes an attribute from a location
 USAGE
    herr_t H5Adelete(loc_id, name)
        hid_t loc_id;       IN: Object (dataset or group) to have attribute deleted from
        const char *name;   IN: Name of attribute to delete
 RETURNS
    Non-negative on success/Negative on failure
 DESCRIPTION
    This function removes the named attribute from a dataset or group.
--------------------------------------------------------------------------*/
herr_t
H5Adelete(hid_t loc_id, const char *name)
{
    H5VL_loc_params_t loc_params;
    herr_t	ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "i*s", loc_id, name);

    /* check arguments */
    if(H5I_ATTR == H5I_get_type(loc_id))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "location is not valid for an attribute")
    if(!name || !*name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name")

    loc_params.type = H5VL_OBJECT_BY_ID;
    loc_params.loc_data.loc_by_id.id = loc_id;

    /* Open the attribute through the VOL */
    if(H5VL_attr_remove(loc_id, loc_params, name, H5_REQUEST_NULL) < 0)
	HGOTO_ERROR(H5E_ATTR, H5E_CANTDELETE, FAIL, "unable to delete attribute")

done:
    FUNC_LEAVE_API(ret_value)
} /* H5Adelete() */


/*--------------------------------------------------------------------------
 NAME
    H5Adelete_by_name
 PURPOSE
    Deletes an attribute from a location
 USAGE
    herr_t H5Adelete_by_name(loc_id, obj_name, attr_name, lapl_id)
        hid_t loc_id;           IN: Base location for object
        const char *obj_name;   IN: Name of object relative to location
        const char *attr_name;  IN: Name of attribute to delete
        hid_t lapl_id;          IN: Link access property list
 RETURNS
    Non-negative on success/Negative on failure
 DESCRIPTION
    This function removes the named attribute from an object.
--------------------------------------------------------------------------*/
herr_t
H5Adelete_by_name(hid_t loc_id, const char *obj_name, const char *attr_name,
    hid_t lapl_id)
{
    H5VL_loc_params_t loc_params;
    herr_t	ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE4("e", "i*s*si", loc_id, obj_name, attr_name, lapl_id);

    /* check arguments */
    if(H5I_ATTR == H5I_get_type(loc_id))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "location is not valid for an attribute")
    if(!obj_name || !*obj_name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no object name")
    if(!attr_name || !*attr_name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no attribute name")
    if(H5P_DEFAULT == lapl_id)
        lapl_id = H5P_LINK_ACCESS_DEFAULT;
    else
        if(TRUE != H5P_isa_class(lapl_id, H5P_LINK_ACCESS))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not link access property list ID")

    loc_params.type = H5VL_OBJECT_BY_NAME;
    loc_params.loc_data.loc_by_name.name = obj_name;
    loc_params.loc_data.loc_by_name.plist_id = lapl_id;

    /* Open the attribute through the VOL */
    if(H5VL_attr_remove(loc_id, loc_params, attr_name, H5_REQUEST_NULL) < 0)
	HGOTO_ERROR(H5E_ATTR, H5E_CANTDELETE, FAIL, "unable to delete attribute")

done:
    FUNC_LEAVE_API(ret_value)
} /* H5Adelete_by_name() */


/*--------------------------------------------------------------------------
 NAME
    H5Adelete_by_idx
 PURPOSE
    Deletes an attribute from a location, according to the order within an index
 USAGE
    herr_t H5Adelete_by_idx(loc_id, obj_name, idx_type, order, n, lapl_id)
        hid_t loc_id;           IN: Base location for object
        const char *obj_name;   IN: Name of object relative to location
        H5_index_t idx_type;    IN: Type of index to use
        H5_iter_order_t order;  IN: Order to iterate over index
        hsize_t n;              IN: Offset within index
        hid_t lapl_id;          IN: Link access property list
 RETURNS
    Non-negative on success/Negative on failure
 DESCRIPTION
        This function removes an attribute from an object, using the IDX_TYPE
    index to delete the N'th attribute in ORDER direction in the index.  The
    object is specified relative to the LOC_ID with the OBJ_NAME path.  To
    remove an attribute on the object specified by LOC_ID, pass in "." for
    OBJ_NAME.  The link access property list, LAPL_ID, controls aspects of
    the group hierarchy traversal when using the OBJ_NAME to locate the final
    object to operate on.
--------------------------------------------------------------------------*/
herr_t
H5Adelete_by_idx(hid_t loc_id, const char *obj_name, H5_index_t idx_type,
    H5_iter_order_t order, hsize_t n, hid_t lapl_id)
{
    H5VL_loc_params_t loc_params;
    herr_t	ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE6("e", "i*sIiIohi", loc_id, obj_name, idx_type, order, n, lapl_id);

    /* check arguments */
    if(H5I_ATTR == H5I_get_type(loc_id))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "location is not valid for an attribute")
    if(!obj_name || !*obj_name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no object name")
    if(idx_type <= H5_INDEX_UNKNOWN || idx_type >= H5_INDEX_N)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid index type specified")
    if(order <= H5_ITER_UNKNOWN || order >= H5_ITER_N)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid iteration order specified")
    if(H5P_DEFAULT == lapl_id)
        lapl_id = H5P_LINK_ACCESS_DEFAULT;
    else
        if(TRUE != H5P_isa_class(lapl_id, H5P_LINK_ACCESS))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not link access property list ID")

    loc_params.type = H5VL_OBJECT_BY_NAME;
    loc_params.loc_data.loc_by_name.name = obj_name;
    loc_params.loc_data.loc_by_name.plist_id = lapl_id;

    /* get the attribute info through the VOL */
    if(H5VL_object_misc(loc_id, H5VL_ATTR_DELETE_BY_IDX, H5_REQUEST_NULL, loc_params, 
                        idx_type, order, n) < 0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTGET, FAIL, "unable to get attribute info")

done:
    FUNC_LEAVE_API(ret_value)
} /* H5Adelete_by_idx() */


/*--------------------------------------------------------------------------
 NAME
    H5Aclose
 PURPOSE
    Close an attribute ID
 USAGE
    herr_t H5Aclose (attr_id)
        hid_t attr_id;       IN: Attribute to release access to
 RETURNS
    Non-negative on success/Negative on failure

 DESCRIPTION
        This function releases an attribute from use.  Further use of the
    attribute ID will result in undefined behavior.
--------------------------------------------------------------------------*/
herr_t
H5Aclose(hid_t attr_id)
{
    herr_t ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE1("e", "i", attr_id);

    /* Close the attribute through the VOL */
    if((ret_value = H5VL_attr_close(attr_id, H5_REQUEST_NULL)) < 0)
	HGOTO_ERROR(H5E_ATTR, H5E_CANTRELEASE, FAIL, "unable to close attribute")

done:
    FUNC_LEAVE_API(ret_value)
} /* H5Aclose() */


/*-------------------------------------------------------------------------
 * Function:	H5Aexists
 *
 * Purpose:	Checks if an attribute with a given name exists on an opened
 *              object.
 *
 * Return:	Success:	TRUE/FALSE
 * 		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *              Thursday, November 1, 2007
 *
 *-------------------------------------------------------------------------
 */
htri_t
H5Aexists(hid_t obj_id, const char *attr_name)
{
    H5VL_loc_params_t loc_params;
    htri_t	ret_value;              /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("t", "i*s", obj_id, attr_name);

    /* check arguments */
    if(H5I_ATTR == H5I_get_type(obj_id))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "location is not valid for an attribute")
    if(!attr_name || !*attr_name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no attribute name")

    loc_params.type = H5VL_OBJECT_BY_ID;
    loc_params.loc_data.loc_by_id.id = obj_id;

    /* get the attribute info through the VOL */
    if(H5VL_attr_get(obj_id, H5VL_ATTR_EXISTS, H5_REQUEST_NULL, attr_name, loc_params, &ret_value) < 0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTGET, FAIL, "unable to get attribute info")

done:
    FUNC_LEAVE_API(ret_value)
} /* H5Aexists() */


/*-------------------------------------------------------------------------
 * Function:	H5Aexists_by_name
 *
 * Purpose:	Checks if an attribute with a given name exists on an object.
 *
 * Return:	Success:	TRUE/FALSE
 * 		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *              Thursday, November 1, 2007
 *
 *-------------------------------------------------------------------------
 */
htri_t
H5Aexists_by_name(hid_t loc_id, const char *obj_name, const char *attr_name,
    hid_t lapl_id)
{
    H5VL_loc_params_t loc_params;
    htri_t	ret_value;              /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE4("t", "i*s*si", loc_id, obj_name, attr_name, lapl_id);

    /* check arguments */
    if(H5I_ATTR == H5I_get_type(loc_id))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "location is not valid for an attribute")
    if(!obj_name || !*obj_name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no object name")
    if(!attr_name || !*attr_name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no attribute name")
    if(H5P_DEFAULT == lapl_id)
        lapl_id = H5P_LINK_ACCESS_DEFAULT;
    else
        if(TRUE != H5P_isa_class(lapl_id, H5P_LINK_ACCESS))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not link access property list ID")

    loc_params.type = H5VL_OBJECT_BY_NAME;
    loc_params.loc_data.loc_by_name.name = obj_name;
    loc_params.loc_data.loc_by_name.plist_id = lapl_id;

    /* get the attribute info through the VOL */
    if(H5VL_attr_get(loc_id, H5VL_ATTR_EXISTS, H5_REQUEST_NULL, attr_name, loc_params, &ret_value) < 0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTGET, FAIL, "unable to get attribute info")

done:
    FUNC_LEAVE_API(ret_value)
} /* H5Aexists_by_name() */
