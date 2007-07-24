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
#include "H5Opkg.h"		/* Object headers			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5FLprivate.h"	/* Free Lists				*/
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Sprivate.h"		/* Dataspace functions			*/
#include "H5SMprivate.h"	/* Shared Object Header Messages	*/

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

static herr_t H5A_open_common(const H5G_loc_t *loc, H5A_t *attr);
static herr_t H5A_write(H5A_t *attr, const H5T_t *mem_type, const void *buf, hid_t dxpl_id);
static herr_t H5A_read(const H5A_t *attr, const H5T_t *mem_type, void *buf, hid_t dxpl_id);


/*********************/
/* Package Variables */
/*********************/


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/

/* Declare the free lists for H5A_t's */
H5FL_DEFINE(H5A_t);

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

    FUNC_ENTER_NOAPI(H5A_init, FAIL)
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

    FUNC_ENTER_NOAPI_NOINIT(H5A_init_interface)

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

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5A_term_interface)

    if(H5_interface_initialize_g) {
	if((n = H5I_nmembers(H5I_ATTR))>0) {
	    (void)H5I_clear_type(H5I_ATTR, FALSE);
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
    hid_t H5Acreate2(loc_id, obj_name, attr_name, type_id, space_id, acpl_id,
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
H5Acreate2(hid_t loc_id, const char *obj_name, const char *attr_name,
    hid_t type_id, hid_t space_id, hid_t acpl_id, hid_t UNUSED aapl_id,
    hid_t lapl_id)
{
    H5G_loc_t           loc;                    /* Object location */
    H5G_loc_t           obj_loc;                /* Location used to open group */
    H5G_name_t          obj_path;            	/* Opened object group hier. path */
    H5O_loc_t           obj_oloc;            	/* Opened object object location */
    hbool_t             loc_found = FALSE;      /* Entry at 'obj_name' found */
    H5T_t		*type;                  /* Datatype to use for attribute */
    H5S_t		*space;                 /* Dataspace to use for attribute */
    hid_t		ret_value;              /* Return value */

    FUNC_ENTER_API(H5Acreate2, FAIL)
    H5TRACE8("i", "i*s*siiiii", loc_id, obj_name, attr_name, type_id, space_id,
             acpl_id, aapl_id, lapl_id);

    /* check arguments */
    if(H5I_ATTR == H5I_get_type(loc_id))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "location is not valid for an attribute")
    if(H5G_loc(loc_id, &loc) < 0)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
    if(0 == (H5F_INTENT(loc.oloc->file) & H5F_ACC_RDWR))
	HGOTO_ERROR(H5E_ARGS, H5E_WRITEERROR, FAIL, "no write intent on file")
    if(!obj_name || !*obj_name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no object name")
    if(!attr_name || !*attr_name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no attribute name")
    if(NULL == (type = (H5T_t *)H5I_object_verify(type_id, H5I_DATATYPE)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a type")
    if(NULL == (space = (H5S_t *)H5I_object_verify(space_id, H5I_DATASPACE)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space")

    /* Set up opened group location to fill in */
    obj_loc.oloc = &obj_oloc;
    obj_loc.path = &obj_path;
    H5G_loc_reset(&obj_loc);

    /* Find the object's location */
    if(H5G_loc_find(&loc, obj_name, &obj_loc/*out*/, lapl_id, H5AC_ind_dxpl_id) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_NOTFOUND, FAIL, "object not found")
    loc_found = TRUE;

    /* Go do the real work for attaching the attribute to the dataset */
    if((ret_value = H5A_create(&obj_loc, attr_name, type, space, acpl_id, H5AC_dxpl_id)) < 0)
	HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, FAIL, "unable to create attribute")

done:
    /* Release resources */
    if(loc_found && H5G_loc_free(&obj_loc) < 0)
        HDONE_ERROR(H5E_ATTR, H5E_CANTRELEASE, FAIL, "can't free location")

    FUNC_LEAVE_API(ret_value)
} /* H5Acreate2() */


/*-------------------------------------------------------------------------
 * Function:	H5A_create
 *
 * Purpose:
 *      This is the guts of the H5Acreate function.
 * Usage:
 *  hid_t H5A_create (ent, name, type, space)
 *      const H5G_entry_t *ent;   IN: Pointer to symbol table entry for object to attribute
 *      const char *name;   IN: Name of attribute
 *      H5T_t *type;        IN: Datatype of attribute
 *      H5S_t *space;       IN: Dataspace of attribute
 *      hid_t acpl_id       IN: Attribute creation property list
 *
 * Return: Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		April 2, 1998
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5A_create(const H5G_loc_t *loc, const char *name, const H5T_t *type,
    const H5S_t *space, hid_t acpl_id, hid_t dxpl_id)
{
    H5A_t	    *attr = NULL;
    htri_t           tri_ret;           /* htri_t return value */
    hid_t	     ret_value;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5A_create)

    /* check args */
    HDassert(loc);
    HDassert(name);
    HDassert(type);
    HDassert(space);

    /* Check for existing attribute with same name */
    /* (technically, the "attribute create" operation will fail for a duplicated
     *  name, but it's going to be hard to unwind all the special cases on
     *  failure, so just check first, for now - QAK)
     */
    if((tri_ret = H5O_attr_exists(loc->oloc, name, H5AC_ind_dxpl_id)) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_NOTFOUND, FAIL, "error checking attributes")
    else if(tri_ret > 0)
        HGOTO_ERROR(H5E_ATTR, H5E_ALREADYEXISTS, FAIL, "attribute already exists")

    /* Check if the dataspace has an extent set (or is NULL) */
    if(!(H5S_has_extent(space)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "dataspace extent has not been set")

    /* Build the attribute information */
    if((attr = H5FL_CALLOC(H5A_t)) == NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for attribute info")

    /* If the creation property list is H5P_DEFAULT, use the default character encoding */
    if(acpl_id == H5P_DEFAULT)
        attr->encoding = H5F_DEFAULT_CSET;
    else {
        H5P_genplist_t  *ac_plist;      /* ACPL Property list */

        /* Get a local copy of the attribute creation property list */
        if(NULL == (ac_plist = (H5P_genplist_t *)H5I_object(acpl_id)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list")

        if(H5P_get(ac_plist, H5P_STRCRT_CHAR_ENCODING_NAME, &(attr->encoding)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get character encoding flag")
    } /* end else */

    /* Copy the attribute name */
    attr->name = H5MM_xstrdup(name);

    /* Copy the attribute's datatype */
    attr->dt = H5T_copy(type, H5T_COPY_ALL);

    /* Mark any datatypes as being on disk now */
    if(H5T_set_loc(attr->dt, loc->oloc->file, H5T_LOC_DISK) < 0)
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "invalid datatype location")

    /* Set the latest format for datatype, if requested */
    if(H5F_USE_LATEST_FORMAT(loc->oloc->file))
        if(H5T_set_latest_version(attr->dt) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTSET, FAIL, "can't set latest version of datatype")

    /* Copy the dataspace for the attribute */
    attr->ds = H5S_copy(space, FALSE);

    /* Set the latest format for dataspace, if requested */
    if(H5F_USE_LATEST_FORMAT(loc->oloc->file))
        if(H5S_set_latest_version(attr->ds) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTSET, FAIL, "can't set latest version of dataspace")

    /* Mark it initially set to initialized */
    attr->initialized = TRUE; /*for now, set to false later*/

    /* Copy the object header information */
    if(H5O_loc_copy(&(attr->oloc), loc->oloc, H5_COPY_DEEP) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTOPENOBJ, FAIL, "unable to copy entry")

    /* Deep copy of the group hierarchy path */
    if(H5G_name_copy(&(attr->path), loc->path, H5_COPY_DEEP) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTOPENOBJ, FAIL, "unable to copy path")

    /* Check if any of the pieces should be (or are already) shared in the
     * SOHM table
     */
    if(H5SM_try_share(attr->oloc.file, dxpl_id, NULL, H5O_DTYPE_ID, attr->dt, NULL) < 0)
	HGOTO_ERROR(H5E_OHDR, H5E_BADMESG, FAIL, "trying to share datatype failed")
    if(H5SM_try_share(attr->oloc.file, dxpl_id, NULL, H5O_SDSPACE_ID, attr->ds, NULL) < 0)
	HGOTO_ERROR(H5E_OHDR, H5E_BADMESG, FAIL, "trying to share dataspace failed")

    /* Check whether datatype is committed & increment ref count
     * (to maintain ref. count incr/decr similarity with "shared message"
     *      type of datatype sharing)
     */
    if(H5T_committed(attr->dt)) {
        /* Increment the reference count on the shared datatype */
        if(H5T_link(attr->dt, 1, dxpl_id) < 0)
            HGOTO_ERROR(H5E_OHDR, H5E_LINKCOUNT, FAIL, "unable to adjust shared datatype link count")
    } /* end if */

    /* Compute the size of pieces on disk.  This is either the size of the
     * datatype and dataspace messages themselves, or the size of the "shared"
     * messages if either or both of them are shared.
     */
    attr->dt_size = H5O_msg_raw_size(attr->oloc.file, H5O_DTYPE_ID, FALSE, attr->dt);
    attr->ds_size = H5O_msg_raw_size(attr->oloc.file, H5O_SDSPACE_ID, FALSE, attr->ds);

    HDassert(attr->dt_size > 0);
    HDassert(attr->ds_size > 0);
    H5_ASSIGN_OVERFLOW(attr->data_size, H5S_GET_EXTENT_NPOINTS(attr->ds) * H5T_get_size(attr->dt), hssize_t, size_t);

    /* Hold the symbol table entry (and file) open */
    if(H5O_open(&(attr->oloc)) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTOPENOBJ, FAIL, "unable to open")
    attr->obj_opened = TRUE;

    /* Set the version to encode the attribute with */
    if(H5A_set_version(attr->oloc.file, attr) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTSET, FAIL, "unable to update attribute version")

    /* Insert the attribute into the object header */
    if(H5O_attr_create(&(attr->oloc), dxpl_id, attr) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTINSERT, FAIL, "unable to create attribute in object header")

    /* Register the new attribute and get an ID for it */
    if((ret_value = H5I_register(H5I_ATTR, attr)) < 0)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to register attribute for ID")

    /* Now it's safe to say it's uninitialized */
    attr->initialized = FALSE;

done:
    /* Cleanup on failure */
    if(ret_value < 0)
        if(attr && H5A_close(attr) < 0)
            HDONE_ERROR(H5E_ATTR, H5E_CANTFREE, FAIL, "can't close attribute")

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5A_create() */


/*--------------------------------------------------------------------------
 NAME
    H5Aopen
 PURPOSE
    Opens an attribute for an object by looking up the attribute name
 USAGE
    hid_t H5Aopen(loc_id, obj_name, attr_name, aapl_id, lapl_id)
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
H5Aopen(hid_t loc_id, const char *obj_name, const char *attr_name,
    hid_t UNUSED aapl_id, hid_t lapl_id)
{
    H5G_loc_t    	loc;            /* Object location */
    H5A_t               *attr = NULL;   /* Attribute opened */
    hid_t		ret_value;

    FUNC_ENTER_API(H5Aopen, FAIL)
    H5TRACE5("i", "i*s*sii", loc_id, obj_name, attr_name, aapl_id, lapl_id);

    /* check arguments */
    if(H5I_ATTR == H5I_get_type(loc_id))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "location is not valid for an attribute")
    if(H5G_loc(loc_id, &loc) < 0)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
    if(!obj_name || !*obj_name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no object name")
    if(!attr_name || !*attr_name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no attribute name")
    if(H5P_DEFAULT == lapl_id)
        lapl_id = H5P_LINK_ACCESS_DEFAULT;
    else
        if(TRUE != H5P_isa_class(lapl_id, H5P_LINK_ACCESS))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not link access property list ID")

    /* Open the attribute on the object header */
    if(NULL == (attr = H5A_open_by_name(&loc, obj_name, attr_name, lapl_id, H5AC_ind_dxpl_id)))
        HGOTO_ERROR(H5E_ATTR, H5E_CANTOPENOBJ, FAIL, "can't open attribute")

    /* Register the attribute and get an ID for it */
    if((ret_value = H5I_register(H5I_ATTR, attr)) < 0)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to register attribute for ID")

done:
    /* Cleanup on failure */
    if(ret_value < 0)
        if(attr && H5A_close(attr) < 0)
            HDONE_ERROR(H5E_ATTR, H5E_CANTFREE, FAIL, "can't close attribute")

    FUNC_LEAVE_API(ret_value)
} /* H5Aopen() */


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
    H5A_t       *attr = NULL;   /* Attribute opened */
    H5G_loc_t	loc;	        /* Object location */
    hid_t	ret_value;      /* Return value */

    FUNC_ENTER_API(H5Aopen_by_idx, FAIL)
    H5TRACE7("i", "i*sIiIohii", loc_id, obj_name, idx_type, order, n, aapl_id,
             lapl_id);

    /* check arguments */
    if(H5I_ATTR == H5I_get_type(loc_id))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "location is not valid for an attribute")
    if(H5G_loc(loc_id, &loc) < 0)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
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

    /* Open the attribute in the object header */
    if(NULL == (attr = H5A_open_by_idx(&loc, obj_name, idx_type, order, n, lapl_id, H5AC_ind_dxpl_id)))
        HGOTO_ERROR(H5E_ATTR, H5E_CANTOPENOBJ, FAIL, "unable to open attribute")

    /* Register the attribute and get an ID for it */
    if((ret_value = H5I_register(H5I_ATTR, attr)) < 0)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to register attribute for ID")

done:
    /* Cleanup on failure */
    if(ret_value < 0)
        if(attr && H5A_close(attr) < 0)
            HDONE_ERROR(H5E_ATTR, H5E_CANTFREE, FAIL, "can't close attribute")

    FUNC_LEAVE_API(ret_value)
} /* H5Aopen_by_idx() */


/*-------------------------------------------------------------------------
 * Function:	H5A_open_common
 *
 * Purpose:
 *      Finishes initializing an attributes the open
 *      
 * Usage:
 *  herr_t H5A_open_common(loc, name, dxpl_id)
 *      const H5G_loc_t *loc;   IN: Pointer to group location for object
 *      H5A_t *attr;            IN/OUT: Pointer to attribute to initialize
 *
 * Return: Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		December 18, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5A_open_common(const H5G_loc_t *loc, H5A_t *attr)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5A_open_common)

    /* check args */
    HDassert(loc);
    HDassert(attr);

#if defined(H5_USING_MEMCHECKER) || !defined(NDEBUG)
    /* Clear object location */
    if(H5O_loc_reset(&(attr->oloc)) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTOPENOBJ, FAIL, "unable to reset location")
#endif /* H5_USING_MEMCHECKER */

    /* Free any previous group hier. path */
    if(H5G_name_free(&(attr->path)) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTRELEASE, FAIL, "can't release group hier. path")

    /* Deep copy of the symbol table entry */
    if(H5O_loc_copy(&(attr->oloc), loc->oloc, H5_COPY_DEEP) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTOPENOBJ, FAIL, "unable to copy entry")

    /* Deep copy of the group hier. path */
    if(H5G_name_copy(&(attr->path), loc->path, H5_COPY_DEEP) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTOPENOBJ, FAIL, "unable to copy entry")

    /* Hold the symbol table entry (and file) open */
    if(H5O_open(&(attr->oloc)) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTOPENOBJ, FAIL, "unable to open")
    attr->obj_opened = TRUE;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5A_open_common() */


/*-------------------------------------------------------------------------
 * Function:	H5A_open_by_idx
 *
 * Purpose: 	Open an attribute according to its index order
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		April 2, 1998
 *
 *-------------------------------------------------------------------------
 */
H5A_t *
H5A_open_by_idx(const H5G_loc_t *loc, const char *obj_name, H5_index_t idx_type,
    H5_iter_order_t order, hsize_t n, hid_t lapl_id, hid_t dxpl_id)
{
    H5G_loc_t   obj_loc;                /* Location used to open group */
    H5G_name_t  obj_path;            	/* Opened object group hier. path */
    H5O_loc_t   obj_oloc;            	/* Opened object object location */
    hbool_t     loc_found = FALSE;      /* Entry at 'obj_name' found */
    H5A_t       *attr = NULL;           /* Attribute from object header */
    H5A_t       *ret_value;             /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5A_open_by_idx)

    /* check args */
    HDassert(loc);
    HDassert(obj_name);

    /* Set up opened group location to fill in */
    obj_loc.oloc = &obj_oloc;
    obj_loc.path = &obj_path;
    H5G_loc_reset(&obj_loc);

    /* Find the object's location */
    if(H5G_loc_find(loc, obj_name, &obj_loc/*out*/, lapl_id, dxpl_id) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_NOTFOUND, NULL, "object not found")
    loc_found = TRUE;

    /* Read in attribute from object header */
    if(NULL == (attr = H5O_attr_open_by_idx(obj_loc.oloc, idx_type, order, n, dxpl_id)))
        HGOTO_ERROR(H5E_ATTR, H5E_CANTOPENOBJ, NULL, "unable to load attribute info from object header")
    attr->initialized = TRUE;

    /* Finish initializing attribute */
    if(H5A_open_common(&obj_loc, attr) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, NULL, "unable to initialize attribute")

    /* Set return value */
    ret_value = attr;

done:
    /* Release resources */
    if(loc_found && H5G_loc_free(&obj_loc) < 0)
        HDONE_ERROR(H5E_ATTR, H5E_CANTRELEASE, NULL, "can't free location")

    /* Cleanup on failure */
    if(ret_value == NULL)
        if(attr && H5A_close(attr) < 0)
            HDONE_ERROR(H5E_ATTR, H5E_CANTFREE, NULL, "can't close attribute")

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5A_open_by_idx() */


/*-------------------------------------------------------------------------
 * Function:	H5A_open_by_name
 *
 * Purpose:	Open an attribute in an object header, according to it's name
 *      
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		December 11, 2006
 *
 *-------------------------------------------------------------------------
 */
H5A_t *
H5A_open_by_name(const H5G_loc_t *loc, const char *obj_name, const char *attr_name,
    hid_t lapl_id, hid_t dxpl_id)
{
    H5G_loc_t   obj_loc;                /* Location used to open group */
    H5G_name_t  obj_path;            	/* Opened object group hier. path */
    H5O_loc_t   obj_oloc;            	/* Opened object object location */
    hbool_t     loc_found = FALSE;      /* Entry at 'obj_name' found */
    H5A_t       *attr = NULL;           /* Attribute from object header */
    H5A_t       *ret_value;             /* Return value */

    FUNC_ENTER_NOAPI(H5A_open_by_name, NULL)

    /* check args */
    HDassert(loc);
    HDassert(obj_name);
    HDassert(attr_name);

    /* Set up opened group location to fill in */
    obj_loc.oloc = &obj_oloc;
    obj_loc.path = &obj_path;
    H5G_loc_reset(&obj_loc);

    /* Find the object's location */
    if(H5G_loc_find(loc, obj_name, &obj_loc/*out*/, lapl_id, dxpl_id) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_NOTFOUND, NULL, "object not found")
    loc_found = TRUE;

    /* Read in attribute from object header */
    if(NULL == (attr = H5O_attr_open_by_name(obj_loc.oloc, attr_name, dxpl_id)))
        HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, NULL, "unable to load attribute info from object header")
    attr->initialized = TRUE;

    /* Finish initializing attribute */
    if(H5A_open_common(loc, attr) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, NULL, "unable to initialize attribute")

    /* Set return value */
    ret_value = attr;

done:
    /* Release resources */
    if(loc_found && H5G_loc_free(&obj_loc) < 0)
        HDONE_ERROR(H5E_ATTR, H5E_CANTRELEASE, NULL, "can't free location")

    /* Cleanup on failure */
    if(ret_value == NULL)
        if(attr && H5A_close(attr) < 0)
            HDONE_ERROR(H5E_ATTR, H5E_CANTFREE, NULL, "can't close attribute")

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5A_open_by_name() */


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
    H5A_t	   *attr;               /* Attribute object for ID */
    const H5T_t    *mem_type = NULL;
    herr_t	    ret_value;

    FUNC_ENTER_API(H5Awrite, FAIL)
    H5TRACE3("e", "ii*x", attr_id, dtype_id, buf);

    /* check arguments */
    if(NULL == (attr = (H5A_t *)H5I_object_verify(attr_id, H5I_ATTR)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not an attribute")
    if(NULL == (mem_type = (H5T_t *)H5I_object_verify(dtype_id, H5I_DATATYPE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a datatype")
    if(NULL == buf)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "null attribute buffer")

    /* Go write the actual data to the attribute */
    if((ret_value = H5A_write(attr, mem_type, buf, H5AC_dxpl_id)) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_WRITEERROR, FAIL, "unable to write attribute")

done:
    FUNC_LEAVE_API(ret_value)
} /* H5Awrite() */


/*--------------------------------------------------------------------------
 NAME
    H5A_write
 PURPOSE
    Actually write out data to an attribute
 USAGE
    herr_t H5A_write (attr, mem_type, buf)
        H5A_t *attr;         IN: Attribute to write
        const H5T_t *mem_type;     IN: Memory datatype of buffer
        const void *buf;           IN: Buffer of data to write
 RETURNS
    Non-negative on success/Negative on failure

 DESCRIPTION
    This function writes a complete attribute to disk.
--------------------------------------------------------------------------*/
static herr_t
H5A_write(H5A_t *attr, const H5T_t *mem_type, const void *buf, hid_t dxpl_id)
{
    uint8_t		*tconv_buf = NULL;	/* datatype conv buffer */
    hbool_t             tconv_owned = FALSE;    /* Whether the datatype conv buffer is owned by attribute */
    uint8_t		*bkg_buf = NULL;	/* temp conversion buffer */
    hssize_t		snelmts;		/* elements in attribute */
    size_t		nelmts;		    	/* elements in attribute */
    H5T_path_t		*tpath = NULL;		/* conversion information*/
    hid_t		src_id = -1, dst_id = -1;/* temporary type atoms */
    size_t		src_type_size;		/* size of source type	*/
    size_t		dst_type_size;		/* size of destination type*/
    size_t		buf_size;		/* desired buffer size	*/
    herr_t		ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT(H5A_write)

    HDassert(attr);
    HDassert(mem_type);
    HDassert(buf);

    /* Get # of elements for attribute's dataspace */
    if((snelmts = H5S_GET_EXTENT_NPOINTS(attr->ds)) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTCOUNT, FAIL, "dataspace is invalid")
    H5_ASSIGN_OVERFLOW(nelmts, snelmts, hssize_t, size_t);

    /* If there's actually data elements for the attribute, make a copy of the data passed in */
    if(nelmts > 0) {
        /* Get the memory and file datatype sizes */
        src_type_size = H5T_get_size(mem_type);
        dst_type_size = H5T_get_size(attr->dt);

        /* Convert memory buffer into disk buffer */
        /* Set up type conversion function */
        if(NULL == (tpath = H5T_path_find(mem_type, attr->dt, NULL, NULL, dxpl_id, FALSE)))
            HGOTO_ERROR(H5E_ATTR, H5E_UNSUPPORTED, FAIL, "unable to convert between src and dst datatypes")

        /* Check for type conversion required */
        if(!H5T_path_noop(tpath)) {
            if((src_id = H5I_register(H5I_DATATYPE, H5T_copy(mem_type, H5T_COPY_ALL))) < 0 ||
                    (dst_id = H5I_register(H5I_DATATYPE, H5T_copy(attr->dt, H5T_COPY_ALL))) < 0)
                HGOTO_ERROR(H5E_ATTR, H5E_CANTREGISTER, FAIL, "unable to register types for conversion")

            /* Get the maximum buffer size needed and allocate it */
            buf_size = nelmts * MAX(src_type_size, dst_type_size);
            if(NULL == (tconv_buf = H5FL_BLK_MALLOC (attr_buf, buf_size)) || NULL == (bkg_buf = H5FL_BLK_CALLOC(attr_buf, buf_size)))
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")

            /* Copy the user's data into the buffer for conversion */
            HDmemcpy(tconv_buf, buf, (src_type_size * nelmts));

            /* Perform datatype conversion */
            if(H5T_convert(tpath, src_id, dst_id, nelmts, (size_t)0, (size_t)0, tconv_buf, bkg_buf, dxpl_id) < 0)
                HGOTO_ERROR(H5E_ATTR, H5E_CANTENCODE, FAIL, "datatype conversion failed")

            /* Free the previous attribute data buffer, if there is one */
            if(attr->data)
                attr->data = H5FL_BLK_FREE(attr_buf, attr->data);

            /* Set the pointer to the attribute data to the converted information */
            attr->data = tconv_buf;
            tconv_owned = TRUE;
        } /* end if */
        /* No type conversion necessary */
        else {
            HDassert(dst_type_size == src_type_size);

            /* Allocate the attribute buffer, if there isn't one */
            if(attr->data == NULL)
                if(NULL == (attr->data = H5FL_BLK_MALLOC(attr_buf, dst_type_size * nelmts)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")

            /* Copy the attribute data into the user's buffer */
            HDmemcpy(attr->data, buf, (dst_type_size * nelmts));
        } /* end else */

        /* Modify the attribute in the object header */
        if(H5O_attr_write(&(attr->oloc), dxpl_id, attr) < 0)
            HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, FAIL, "unable to modify attribute")
    } /* end if */

    /* Indicate the the attribute doesn't need fill-values */
    attr->initialized = TRUE;

done:
    /* Release resources */
    if(src_id >= 0)
        (void)H5I_dec_ref(src_id);
    if(dst_id >= 0)
        (void)H5I_dec_ref(dst_id);
    if(tconv_buf && !tconv_owned)
        H5FL_BLK_FREE(attr_buf, tconv_buf);
    if(bkg_buf)
        H5FL_BLK_FREE(attr_buf, bkg_buf);

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5A_write() */


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
    H5A_t		*attr;                  /* Attribute object for ID */
    const H5T_t    	*mem_type = NULL;
    herr_t		ret_value;

    FUNC_ENTER_API(H5Aread, FAIL)
    H5TRACE3("e", "ii*x", attr_id, dtype_id, buf);

    /* check arguments */
    if(NULL == (attr = (H5A_t *)H5I_object_verify(attr_id, H5I_ATTR)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not an attribute")
    if(NULL == (mem_type = (H5T_t *)H5I_object_verify(dtype_id, H5I_DATATYPE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a datatype")
    if(NULL == buf)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "null attribute buffer")

    /* Go write the actual data to the attribute */
    if((ret_value = H5A_read(attr, mem_type, buf, H5AC_dxpl_id)) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_READERROR, FAIL, "unable to read attribute")

done:
    FUNC_LEAVE_API(ret_value)
} /* H5Aread() */


/*--------------------------------------------------------------------------
 NAME
    H5A_read
 PURPOSE
    Actually read in data from an attribute
 USAGE
    herr_t H5A_read (attr, mem_type, buf)
        H5A_t *attr;         IN: Attribute to read
        const H5T_t *mem_type;     IN: Memory datatype of buffer
        void *buf;           IN: Buffer for data to read
 RETURNS
    Non-negative on success/Negative on failure

 DESCRIPTION
    This function reads a complete attribute from disk.
--------------------------------------------------------------------------*/
static herr_t
H5A_read(const H5A_t *attr, const H5T_t *mem_type, void *buf, hid_t dxpl_id)
{
    uint8_t		*tconv_buf = NULL;	/* datatype conv buffer*/
    uint8_t		*bkg_buf = NULL;	/* background buffer */
    hssize_t		snelmts;		/* elements in attribute */
    size_t		nelmts;			/* elements in attribute*/
    H5T_path_t		*tpath = NULL;		/* type conversion info	*/
    hid_t		src_id = -1, dst_id = -1;/* temporary type atoms*/
    size_t		src_type_size;		/* size of source type 	*/
    size_t		dst_type_size;		/* size of destination type */
    size_t		buf_size;		/* desired buffer size	*/
    herr_t		ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT(H5A_read)

    HDassert(attr);
    HDassert(mem_type);
    HDassert(buf);

    /* Create buffer for data to store on disk */
    if((snelmts = H5S_GET_EXTENT_NPOINTS(attr->ds)) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTCOUNT, FAIL, "dataspace is invalid")
    H5_ASSIGN_OVERFLOW(nelmts, snelmts, hssize_t, size_t);

    if(nelmts > 0) {
        /* Get the memory and file datatype sizes */
        src_type_size = H5T_get_size(attr->dt);
        dst_type_size = H5T_get_size(mem_type);

        /* Check if the attribute has any data yet, if not, fill with zeroes */
        if(attr->obj_opened && !attr->initialized)
            HDmemset(buf, 0, (dst_type_size * nelmts));
        else {  /* Attribute exists and has a value */
            /* Convert memory buffer into disk buffer */
            /* Set up type conversion function */
            if(NULL == (tpath = H5T_path_find(attr->dt, mem_type, NULL, NULL, dxpl_id, FALSE)))
                HGOTO_ERROR(H5E_ATTR, H5E_UNSUPPORTED, FAIL, "unable to convert between src and dst datatypes")

            /* Check for type conversion required */
            if(!H5T_path_noop(tpath)) {
                if((src_id = H5I_register(H5I_DATATYPE, H5T_copy(attr->dt, H5T_COPY_ALL))) < 0 ||
                        (dst_id = H5I_register(H5I_DATATYPE, H5T_copy(mem_type, H5T_COPY_ALL))) < 0)
                    HGOTO_ERROR(H5E_ATTR, H5E_CANTREGISTER, FAIL, "unable to register types for conversion")

                /* Get the maximum buffer size needed and allocate it */
                buf_size = nelmts * MAX(src_type_size, dst_type_size);
                if(NULL == (tconv_buf = H5FL_BLK_MALLOC(attr_buf, buf_size)) || NULL == (bkg_buf = H5FL_BLK_CALLOC(attr_buf, buf_size)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")

                /* Copy the attribute data into the buffer for conversion */
                HDmemcpy(tconv_buf, attr->data, (src_type_size * nelmts));

                /* Perform datatype conversion.  */
                if(H5T_convert(tpath, src_id, dst_id, nelmts, (size_t)0, (size_t)0, tconv_buf, bkg_buf, dxpl_id) < 0)
                    HGOTO_ERROR(H5E_ATTR, H5E_CANTENCODE, FAIL, "datatype conversion failed")

                /* Copy the converted data into the user's buffer */
                HDmemcpy(buf, tconv_buf, (dst_type_size * nelmts));
            } /* end if */
            /* No type conversion necessary */
            else {
                HDassert(dst_type_size == src_type_size);

                /* Copy the attribute data into the user's buffer */
                HDmemcpy(buf, attr->data, (dst_type_size * nelmts));
            } /* end else */
        } /* end else */
    } /* end if */

done:
    /* Release resources */
    if(src_id >= 0)
        (void)H5I_dec_ref(src_id);
    if(dst_id >= 0)
        (void)H5I_dec_ref(dst_id);
    if(tconv_buf)
        H5FL_BLK_FREE(attr_buf, tconv_buf);
    if(bkg_buf)
	H5FL_BLK_FREE(attr_buf, bkg_buf);

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5A_read() */


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
    H5A_t	*attr;                  /* Attribute object for ID */
    H5S_t	*ds = NULL;             /* Copy of dataspace for attribute */
    hid_t	ret_value;

    FUNC_ENTER_API(H5Aget_space, FAIL)
    H5TRACE1("i", "i", attr_id);

    /* check arguments */
    if(NULL == (attr = (H5A_t *)H5I_object_verify(attr_id, H5I_ATTR)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not an attribute")

    /* Copy the attribute's dataspace */
    if(NULL == (ds = H5S_copy (attr->ds, FALSE)))
	HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, FAIL, "unable to copy dataspace")

    /* Atomize */
    if((ret_value = H5I_register(H5I_DATASPACE, ds)) < 0)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to register dataspace atom")

done:
    if(ret_value < 0 && ds)
        (void)H5S_close(ds);

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
    H5A_t	*attr;          /* Attribute object for ID */
    H5T_t	*dt = NULL;     /* Copy of attribute's datatype */
    hid_t	 ret_value;

    FUNC_ENTER_API(H5Aget_type, FAIL)
    H5TRACE1("i", "i", attr_id);

    /* check arguments */
    if(NULL == (attr = (H5A_t *)H5I_object_verify(attr_id, H5I_ATTR)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not an attribute")

    /*
     * Copy the attribute's datatype.  If the type is a named type then
     * reopen the type before returning it to the user. Make the type
     * read-only.
     */
    if(NULL == (dt = H5T_copy(attr->dt, H5T_COPY_REOPEN)))
	HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, FAIL, "unable to copy datatype")

    /* Mark any datatypes as being in memory now */
    if(H5T_set_loc(dt, NULL, H5T_LOC_MEMORY) < 0)
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "invalid datatype location")
    if(H5T_lock(dt, FALSE) < 0)
	HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "unable to lock transient datatype")

    /* Atomize */
    if((ret_value = H5I_register(H5I_DATATYPE, dt)) < 0)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to register datatype atom")

done:
    if(ret_value < 0 && dt)
        (void)H5T_close(dt);

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
    H5A_t		*attr;               /* Attribute object for ID */
    H5P_genplist_t      *plist;              /* Default property list */
    hid_t               new_plist_id;        /* ID of ACPL to return */
    H5P_genplist_t      *new_plist;          /* ACPL to return */
    hid_t		ret_value;

    FUNC_ENTER_API(H5Aget_create_plist, FAIL)
    H5TRACE1("i", "i", attr_id);

    HDassert(H5P_LST_ATTRIBUTE_CREATE_g != -1);

    /* Get attribute and default attribute creation property list*/
    if(NULL == (attr = (H5A_t *)H5I_object_verify(attr_id, H5I_ATTR)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not an attribute")
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(H5P_LST_ATTRIBUTE_CREATE_g)))
        HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "can't get default ACPL")

    /* Create the property list object to return */
    if((new_plist_id = H5P_copy_plist(plist)) < 0)
	HGOTO_ERROR(H5E_PLIST, H5E_CANTINIT, FAIL, "unable to copy attribute creation properties")
    if(NULL == (new_plist = (H5P_genplist_t *)H5I_object(new_plist_id)))
        HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "can't get property list")

    /* Set the character encoding on the new property list */
    if(H5P_set(new_plist, H5P_STRCRT_CHAR_ENCODING_NAME, &(attr->encoding)) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set character encoding")

    ret_value = new_plist_id;

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
    H5A_t		*attr;               /* Attribute object for ID */
    size_t              copy_len, nbytes;
    ssize_t		ret_value;

    FUNC_ENTER_API(H5Aget_name, FAIL)
    H5TRACE3("Zs", "iz*s", attr_id, buf_size, buf);

    /* check arguments */
    if(NULL == (attr = (H5A_t *)H5I_object_verify(attr_id, H5I_ATTR)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not an attribute")
    if(!buf && buf_size)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid buffer")

    /* get the real attribute length */
    nbytes = HDstrlen(attr->name);
    HDassert((ssize_t)nbytes >= 0); /*overflow, pretty unlikely --rpm*/

    /* compute the string length which will fit into the user's buffer */
    copy_len = MIN(buf_size - 1, nbytes);

    /* Copy all/some of the name */
    if(buf && copy_len > 0) {
        HDmemcpy(buf, attr->name, copy_len);

        /* Terminate the string */
        buf[copy_len]='\0';
    } /* end if */

    /* Set return value */
    ret_value = (ssize_t)nbytes;

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
    H5G_loc_t   loc;            /* Object location */
    H5A_t	*attr = NULL;   /* Attribute object for name */
    ssize_t	ret_value;      /* Return value */

    FUNC_ENTER_API(H5Aget_name_by_idx, FAIL)
    H5TRACE8("Zs", "i*sIiIohxzi", loc_id, obj_name, idx_type, order, n, name, size,
             lapl_id);

    /* Check args */
    if(H5I_ATTR == H5I_get_type(loc_id))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "location is not valid for an attribute")
    if(H5G_loc(loc_id, &loc) < 0)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
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

    /* Open the attribute on the object header */
    if(NULL == (attr = H5A_open_by_idx(&loc, obj_name, idx_type, order, n, lapl_id, H5AC_ind_dxpl_id)))
        HGOTO_ERROR(H5E_ATTR, H5E_CANTOPENOBJ, FAIL, "can't open attribute")

    /* Get the length of the name */
    ret_value = (ssize_t)HDstrlen(attr->name);

    /* Copy the name into the user's buffer, if given */
    if(name) {
        HDstrncpy(name, attr->name, MIN((size_t)(ret_value + 1), size));
        if((size_t)ret_value >= size)
            name[size - 1]='\0';
    } /* end if */

done:
    /* Release resources */
    if(attr && H5A_close(attr) < 0)
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
    H5A_t	*attr;               /* Attribute object for ID */
    hsize_t	ret_value;      /* Return value */

    FUNC_ENTER_API(H5Aget_storage_size, 0)
    H5TRACE1("h", "i", attr_id);

    /* Check args */
    if(NULL == (attr = (H5A_t *)H5I_object_verify(attr_id, H5I_ATTR)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, 0, "not an attribute")

    /* Set return value */
    ret_value = attr->data_size;

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
    H5A_t	*attr;                  /* Attribute object for name */
    herr_t	ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_API(H5Aget_info, FAIL)
    H5TRACE2("e", "i*x", attr_id, ainfo);

    /* Check args */
    if(NULL == (attr = (H5A_t *)H5I_object_verify(attr_id, H5I_ATTR)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not an attribute")

    /* Get the attribute information */
    if(H5A_get_info(attr, ainfo) < 0)
	HGOTO_ERROR(H5E_ATTR, H5E_CANTGET, FAIL, "unable to get attribute info")

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
    H5G_loc_t   loc;                    /* Object location */
    H5A_t	*attr = NULL;           /* Attribute object for name */
    herr_t	ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_API(H5Aget_info_by_name, FAIL)
    H5TRACE5("e", "i*s*s*xi", loc_id, obj_name, attr_name, ainfo, lapl_id);

    /* Check args */
    if(H5I_ATTR == H5I_get_type(loc_id))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "location is not valid for an attribute")
    if(H5G_loc(loc_id, &loc) < 0)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
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

    /* Open the attribute on the object header */
    if(NULL == (attr = H5A_open_by_name(&loc, obj_name, attr_name, lapl_id, H5AC_ind_dxpl_id)))
        HGOTO_ERROR(H5E_ATTR, H5E_CANTOPENOBJ, FAIL, "can't open attribute")

    /* Get the attribute information */
    if(H5A_get_info(attr, ainfo) < 0)
	HGOTO_ERROR(H5E_ATTR, H5E_CANTGET, FAIL, "unable to get attribute info")

done:
    /* Cleanup on failure */
    if(attr && H5A_close(attr) < 0)
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
    H5G_loc_t   loc;                    /* Object location */
    H5A_t	*attr = NULL;           /* Attribute object for name */
    herr_t	ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_API(H5Aget_info_by_idx, FAIL)
    H5TRACE7("e", "i*sIiIoh*xi", loc_id, obj_name, idx_type, order, n, ainfo,
             lapl_id);

    /* Check args */
    if(H5I_ATTR == H5I_get_type(loc_id))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "location is not valid for an attribute")
    if(H5G_loc(loc_id, &loc) < 0)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
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

    /* Open the attribute on the object header */
    if(NULL == (attr = H5A_open_by_idx(&loc, obj_name, idx_type, order, n, lapl_id, H5AC_ind_dxpl_id)))
        HGOTO_ERROR(H5E_ATTR, H5E_CANTOPENOBJ, FAIL, "can't open attribute")

    /* Get the attribute information */
    if(H5A_get_info(attr, ainfo) < 0)
	HGOTO_ERROR(H5E_ATTR, H5E_CANTGET, FAIL, "unable to get attribute info")

done:
    /* Release resources */
    if(attr && H5A_close(attr) < 0)
        HDONE_ERROR(H5E_ATTR, H5E_CANTFREE, FAIL, "can't close attribute")

    FUNC_LEAVE_API(ret_value)
} /* end H5Aget_info_by_idx() */


/*-------------------------------------------------------------------------
 * Function:	H5A_get_info
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
H5A_get_info(const H5A_t *attr, H5A_info_t *ainfo)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(H5A_get_info, FAIL)

    /* Check args */
    HDassert(attr);
    HDassert(ainfo);

    /* Set info for attribute */
    ainfo->cset = attr->encoding;
    ainfo->data_size = attr->data_size;
    if(attr->crt_idx == H5O_MAX_CRT_ORDER_IDX) {
        ainfo->corder_valid = FALSE;
        ainfo->corder = 0;
    } /* end if */
    else {
        ainfo->corder_valid = TRUE;
        ainfo->corder = attr->crt_idx;
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5A_get_info() */


/*-------------------------------------------------------------------------
 * Function:	H5Arename2
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
H5Arename2(hid_t loc_id, const char *obj_name, const char *old_attr_name,
    const char *new_attr_name, hid_t lapl_id)
{
    H5G_loc_t	loc;	                /* Object location */
    H5G_loc_t   obj_loc;                /* Location used to open group */
    H5G_name_t  obj_path;            	/* Opened object group hier. path */
    H5O_loc_t   obj_oloc;            	/* Opened object object location */
    hbool_t     loc_found = FALSE;      /* Entry at 'obj_name' found */
    herr_t	ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_API(H5Arename2, FAIL)
    H5TRACE5("e", "i*s*s*si", loc_id, obj_name, old_attr_name, new_attr_name,
             lapl_id);

    /* check arguments */
    if(H5I_ATTR == H5I_get_type(loc_id))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "location is not valid for an attribute")
    if(H5G_loc(loc_id, &loc) < 0)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
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
        /* Set up opened group location to fill in */
        obj_loc.oloc = &obj_oloc;
        obj_loc.path = &obj_path;
        H5G_loc_reset(&obj_loc);

        /* Find the object's location */
        if(H5G_loc_find(&loc, obj_name, &obj_loc/*out*/, lapl_id, H5AC_ind_dxpl_id) < 0)
            HGOTO_ERROR(H5E_ATTR, H5E_NOTFOUND, FAIL, "object not found")
        loc_found = TRUE;

        /* Call attribute rename routine */
        if(H5O_attr_rename(obj_loc.oloc, H5AC_dxpl_id, old_attr_name, new_attr_name) < 0)
            HGOTO_ERROR(H5E_ATTR, H5E_CANTRENAME, FAIL, "can't rename attribute")
    } /* end if */

done:
    /* Release resources */
    if(loc_found && H5G_loc_free(&obj_loc) < 0)
        HDONE_ERROR(H5E_ATTR, H5E_CANTRELEASE, FAIL, "can't free location")

    FUNC_LEAVE_API(ret_value)
} /* H5Arename2() */


/*--------------------------------------------------------------------------
 NAME
    H5Aiterate2
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
H5Aiterate2(hid_t loc_id, const char *obj_name, H5_index_t idx_type,
    H5_iter_order_t order, hsize_t *idx, H5A_operator2_t op, void *op_data,
    hid_t lapl_id)
{
    H5G_loc_t	loc;	        /* Object location */
    H5G_loc_t   obj_loc;        /* Location used to open group */
    H5G_name_t  obj_path;       /* Opened object group hier. path */
    H5O_loc_t   obj_oloc;       /* Opened object object location */
    hbool_t     loc_found = FALSE;      /* Entry at 'obj_name' found */
    hid_t       obj_loc_id = (-1);      /* ID for object located */
    H5A_attr_iter_op_t attr_op; /* Attribute operator */
    hsize_t	start_idx;      /* Index of attribute to start iterating at */
    hsize_t	last_attr;      /* Index of last attribute examined */
    herr_t	ret_value;      /* Return value */

    FUNC_ENTER_API(H5Aiterate2, FAIL)
    H5TRACE8("e", "i*sIiIo*hx*xi", loc_id, obj_name, idx_type, order, idx, op,
             op_data, lapl_id);

    /* check arguments */
    if(H5I_ATTR == H5I_get_type(loc_id))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "location is not valid for an attribute")
    if(H5G_loc(loc_id, &loc) < 0)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
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

    /* Set up opened group location to fill in */
    obj_loc.oloc = &obj_oloc;
    obj_loc.path = &obj_path;
    H5G_loc_reset(&obj_loc);

    /* Find the object's location */
    if(H5G_loc_find(&loc, obj_name, &obj_loc/*out*/, lapl_id, H5AC_ind_dxpl_id) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_NOTFOUND, FAIL, "object not found")
    loc_found = TRUE;

    /* Open the object */
    if((obj_loc_id = H5O_open_by_loc(&obj_loc, H5AC_ind_dxpl_id)) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTOPENOBJ, FAIL, "unable to open object")

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
        if(H5I_dec_ref(obj_loc_id) < 0)
            HDONE_ERROR(H5E_ATTR, H5E_CANTRELEASE, FAIL, "unable to close temporary object")
    } /* end if */
    else if(loc_found && H5G_loc_free(&obj_loc) < 0)
        HDONE_ERROR(H5E_ATTR, H5E_CANTRELEASE, FAIL, "can't free location")

    FUNC_LEAVE_API(ret_value)
} /* H5Aiterate2() */


/*--------------------------------------------------------------------------
 NAME
    H5Adelete2
 PURPOSE
    Deletes an attribute from a location
 USAGE
    herr_t H5Adelete2(loc_id, obj_name, attr_name, lapl_id)
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
H5Adelete2(hid_t loc_id, const char *obj_name, const char *attr_name,
    hid_t lapl_id)
{
    H5G_loc_t	loc;		        /* Object location */
    H5G_loc_t   obj_loc;                /* Location used to open group */
    H5G_name_t  obj_path;            	/* Opened object group hier. path */
    H5O_loc_t   obj_oloc;            	/* Opened object object location */
    hbool_t     loc_found = FALSE;      /* Entry at 'obj_name' found */
    herr_t	ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_API(H5Adelete2, FAIL)
    H5TRACE4("e", "i*s*si", loc_id, obj_name, attr_name, lapl_id);

    /* check arguments */
    if(H5I_ATTR == H5I_get_type(loc_id))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "location is not valid for an attribute")
    if(H5G_loc(loc_id, &loc) < 0)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
    if(!obj_name || !*obj_name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no object name")
    if(!attr_name || !*attr_name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no attribute name")
    if(H5P_DEFAULT == lapl_id)
        lapl_id = H5P_LINK_ACCESS_DEFAULT;
    else
        if(TRUE != H5P_isa_class(lapl_id, H5P_LINK_ACCESS))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not link access property list ID")

    /* Set up opened group location to fill in */
    obj_loc.oloc = &obj_oloc;
    obj_loc.path = &obj_path;
    H5G_loc_reset(&obj_loc);

    /* Find the object's location */
    if(H5G_loc_find(&loc, obj_name, &obj_loc/*out*/, lapl_id, H5AC_ind_dxpl_id) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_NOTFOUND, FAIL, "object not found")
    loc_found = TRUE;

    /* Delete the attribute from the location */
    if(H5O_attr_remove(obj_loc.oloc, attr_name, H5AC_dxpl_id) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTDELETE, FAIL, "unable to delete attribute")

done:
    /* Release resources */
    if(loc_found && H5G_loc_free(&obj_loc) < 0)
        HDONE_ERROR(H5E_ATTR, H5E_CANTRELEASE, FAIL, "can't free location")

    FUNC_LEAVE_API(ret_value)
} /* H5Adelete2() */


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
    H5G_loc_t	loc;		        /* Object location */
    H5G_loc_t   obj_loc;                /* Location used to open group */
    H5G_name_t  obj_path;            	/* Opened object group hier. path */
    H5O_loc_t   obj_oloc;            	/* Opened object object location */
    hbool_t     loc_found = FALSE;      /* Entry at 'obj_name' found */
    herr_t	ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_API(H5Adelete_by_idx, FAIL)
    H5TRACE6("e", "i*sIiIohi", loc_id, obj_name, idx_type, order, n, lapl_id);

    /* check arguments */
    if(H5I_ATTR == H5I_get_type(loc_id))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "location is not valid for an attribute")
    if(H5G_loc(loc_id, &loc) < 0)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
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

    /* Set up opened group location to fill in */
    obj_loc.oloc = &obj_oloc;
    obj_loc.path = &obj_path;
    H5G_loc_reset(&obj_loc);

    /* Find the object's location */
    if(H5G_loc_find(&loc, obj_name, &obj_loc/*out*/, lapl_id, H5AC_dxpl_id) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_NOTFOUND, FAIL, "object not found")
    loc_found = TRUE;

    /* Delete the attribute from the location */
    if(H5O_attr_remove_by_idx(obj_loc.oloc, idx_type, order, n, H5AC_dxpl_id) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTDELETE, FAIL, "unable to delete attribute")

done:
    /* Release resources */
    if(loc_found && H5G_loc_free(&obj_loc) < 0)
        HDONE_ERROR(H5E_ATTR, H5E_CANTRELEASE, FAIL, "can't free location")

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

    FUNC_ENTER_API(H5Aclose, FAIL)
    H5TRACE1("e", "i", attr_id);

    /* check arguments */
    if(NULL == H5I_object_verify(attr_id, H5I_ATTR))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not an attribute")

    /* Decrement references to that atom (and close it) */
    if(H5I_dec_ref(attr_id) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTDEC, FAIL, "can't close attribute")

done:
    FUNC_LEAVE_API(ret_value)
} /* H5Aclose() */


/*-------------------------------------------------------------------------
 * Function:	H5A_copy
 *
 * Purpose:	Copies attribute OLD_ATTR.
 *
 * Return:	Success:	Pointer to a new copy of the OLD_ATTR argument.
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *		Thursday, December  4, 1997
 *
 *-------------------------------------------------------------------------
 */
H5A_t *
H5A_copy(H5A_t *_new_attr, const H5A_t *old_attr)
{
    H5A_t	*new_attr = NULL;
    hbool_t     allocated_attr = FALSE;   /* Whether the attribute was allocated */
    H5A_t	*ret_value = NULL;        /* Return value */

    FUNC_ENTER_NOAPI(H5A_copy, NULL)

    /* check args */
    HDassert(old_attr);

    /* Allocate attribute structure */
    if(_new_attr == NULL) {
        if(NULL == (new_attr = H5FL_MALLOC(H5A_t)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")
        allocated_attr = TRUE;
    } /* end if */
    else
        new_attr = _new_attr;

    /* Copy the top level of the attribute */
    *new_attr = *old_attr;

    /* Don't open the object header for a copy */
    new_attr->obj_opened = FALSE;

    /* Copy the guts of the attribute */
    if(NULL == (new_attr->name = H5MM_xstrdup(old_attr->name)))
	HGOTO_ERROR(H5E_ATTR, H5E_CANTCOPY, NULL, "unable to copy attribute name")
    if(NULL == (new_attr->dt = H5T_copy(old_attr->dt, H5T_COPY_ALL)))
	HGOTO_ERROR(H5E_ATTR, H5E_CANTCOPY, NULL, "unable to copy attribute datatype")
    if(NULL == (new_attr->ds = H5S_copy(old_attr->ds, FALSE)))
	HGOTO_ERROR(H5E_ATTR, H5E_CANTCOPY, NULL, "unable to copy attribute dataspace")

#if defined(H5_USING_MEMCHECKER) || !defined(NDEBUG)
    /* Clear object location */
    if(H5O_loc_reset(&(new_attr->oloc)) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTOPENOBJ, NULL, "unable to reset location")

    /* Clear path name */
    if(H5G_name_reset(&(new_attr->path)) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTOPENOBJ, NULL, "unable to reset path")
#endif /* H5_USING_MEMCHECKER */

    /* Copy the object location and group path */
    if(H5O_loc_copy(&(new_attr->oloc), &(old_attr->oloc), H5_COPY_DEEP) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTCOPY, NULL, "can't copy object location")
    if(H5G_name_copy(&(new_attr->path), &(old_attr->path), H5_COPY_DEEP) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTCOPY, NULL, "can't copy path")

    /* Copy the attribute data, if there is any */
    if(old_attr->data) {
        /* Allocate data buffer for new attribute */
        if(NULL == (new_attr->data = H5FL_BLK_MALLOC(attr_buf, old_attr->data_size)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

        /* Copy the attribute data */
        HDmemcpy(new_attr->data, old_attr->data, old_attr->data_size);
    } /* end if */

    /* Set the return value */
    ret_value = new_attr;

done:
    if(ret_value == NULL)
        if(allocated_attr && new_attr && H5A_close(new_attr) < 0)
            HDONE_ERROR(H5E_ATTR, H5E_CANTFREE, NULL, "can't close attribute")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5A_copy() */


/*-------------------------------------------------------------------------
 * Function:	H5A_free
 *
 * Purpose:	Frees all memory associated with an attribute, but does not
 *              free the H5A_t structure (which should be done in H5T_close).
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Monday, November 15, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5A_free(H5A_t *attr)
{
    herr_t ret_value = SUCCEED;           /* Return value */

    FUNC_ENTER_NOAPI(H5A_free, FAIL)

    HDassert(attr);

    /* Free dynamicly allocated items */
    if(attr->name)
        H5MM_xfree(attr->name);
    if(attr->dt)
        if(H5T_close(attr->dt) < 0)
	    HGOTO_ERROR(H5E_ATTR, H5E_CANTRELEASE, FAIL, "can't release datatype info")
    if(attr->ds)
        if(H5S_close(attr->ds) < 0)
	    HGOTO_ERROR(H5E_ATTR, H5E_CANTRELEASE, FAIL, "can't release dataspace info")
    if(attr->data)
        attr->data = H5FL_BLK_FREE(attr_buf, attr->data);
    if(H5G_name_free(&(attr->path)) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTRELEASE, FAIL, "can't release group hier. path")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5A_free() */


/*-------------------------------------------------------------------------
 * Function:	H5A_close
 *
 * Purpose:	Frees an attribute and all associated memory.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Monday, December  8, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5A_close(H5A_t *attr)
{
    herr_t ret_value = SUCCEED;           /* Return value */

    FUNC_ENTER_NOAPI(H5A_close, FAIL)

    HDassert(attr);

    /* Check if the attribute has any data yet, if not, fill with zeroes */
    if(attr->obj_opened && !attr->initialized && attr->data_size) {
        uint8_t *tmp_buf = H5FL_BLK_CALLOC(attr_buf, attr->data_size);
        if(NULL == tmp_buf)
            HGOTO_ERROR(H5E_ATTR, H5E_NOSPACE, FAIL, "memory allocation failed for attribute fill-value")

        /* Go write the fill data to the attribute */
        if(H5A_write(attr, attr->dt, tmp_buf, H5AC_dxpl_id) < 0)
            HGOTO_ERROR(H5E_ATTR, H5E_WRITEERROR, FAIL, "unable to write attribute")

        /* Free temporary buffer */
        H5FL_BLK_FREE(attr_buf, tmp_buf);
    } /* end if */

    /* Free dynamicly allocated items */
    if(H5A_free(attr) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTRELEASE, FAIL, "can't release attribute info")

    /* Close the object's symbol-table entry */
    if(attr->obj_opened)
        if(H5O_close(&(attr->oloc)) < 0)
	    HGOTO_ERROR(H5E_ATTR, H5E_CANTRELEASE, FAIL, "can't release object header info")

    H5FL_FREE(H5A_t, attr);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5A_close() */


/*-------------------------------------------------------------------------
 * Function:	H5A_oloc
 *
 * Purpose:	Return the object location for an attribute.  It's the
 *		object location for the object to which the attribute
 *		belongs, not the attribute itself.
 *
 * Return:	Success:	Ptr to entry
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *              Thursday, August  6, 1998
 *
 *-------------------------------------------------------------------------
 */
H5O_loc_t *
H5A_oloc(H5A_t *attr)
{
    H5O_loc_t *ret_value;   /* Return value */

    FUNC_ENTER_NOAPI(H5A_oloc, NULL)

    HDassert(attr);

    /* Set return value */
    ret_value = &(attr->oloc);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5A_oloc() */


/*-------------------------------------------------------------------------
 * Function:	H5A_nameof
 *
 * Purpose:	Return the group hier. path for an attribute.  It's the
 *		group hier. path for the object to which the attribute
 *		belongs, not the attribute itself.
 *
 * Return:	Success:	Ptr to entry
 *		Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *              Monday, September 12, 2005
 *
 *-------------------------------------------------------------------------
 */
H5G_name_t *
H5A_nameof(H5A_t *attr)
{
    H5G_name_t *ret_value;   /* Return value */

    FUNC_ENTER_NOAPI(H5A_nameof, NULL)

    HDassert(attr);

    /* Set return value */
    ret_value=&(attr->path);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5A_nameof() */

