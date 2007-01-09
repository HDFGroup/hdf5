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
 * Created:		H5O.c
 *			Aug  5 1997
 *			Robb Matzke <matzke@llnl.gov>
 *
 * Purpose:		Object header routines.
 *
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/

#define H5F_PACKAGE		/*suppress error about including H5Fpkg	  */
#define H5O_PACKAGE		/*suppress error about including H5Opkg	  */

/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Aprivate.h"		/* Attributes			  	*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Fpkg.h"		/* File access				*/
#include "H5FLprivate.h"	/* Free lists                           */
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5MFprivate.h"	/* File memory management		*/
#include "H5Opkg.h"             /* Object headers			*/
#include "H5SMprivate.h"        /* Shared object header messages        */


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

static hid_t H5O_open_by_loc(H5G_loc_t *obj_loc, hid_t dxpl_id);
static herr_t H5O_new(H5F_t *f, hid_t dxpl_id, haddr_t header, size_t chunk_size,
    hid_t ocpl_id, H5O_loc_t *loc/*out*/);
static herr_t H5O_delete_oh(H5F_t *f, hid_t dxpl_id, H5O_t *oh);
static const H5O_obj_class_t *H5O_obj_class(H5O_loc_t *loc, hid_t dxpl_id);
static herr_t H5O_obj_type_real(H5O_t *oh, H5O_type_t *obj_type);


/*********************/
/* Package Variables */
/*********************/

/* Header message ID to class mapping */
const H5O_msg_class_t *const H5O_msg_class_g[] = {
    H5O_MSG_NULL,		/*0x0000 Null				*/
    H5O_MSG_SDSPACE,		/*0x0001 Simple Dimensionality		*/
    H5O_MSG_LINFO,		/*0x0002 Link information		*/
    H5O_MSG_DTYPE,		/*0x0003 Data Type			*/
    H5O_MSG_FILL,       	/*0x0004 Old data storage -- fill value */
    H5O_MSG_FILL_NEW,		/*0x0005 New Data storage -- fill value */
    H5O_MSG_LINK,		/*0x0006 Link 				*/
    H5O_MSG_EFL,		/*0x0007 Data storage -- external data files */
    H5O_MSG_LAYOUT,		/*0x0008 Data Layout			*/
#ifdef H5O_ENABLE_BOGUS
    H5O_MSG_BOGUS,		/*0x0009 "Bogus"			*/
#else /* H5O_ENABLE_BOGUS */
    NULL,			/*0x0009 "Bogus"			*/
#endif /* H5O_ENABLE_BOGUS */
    H5O_MSG_GINFO,		/*0x000A Group Information		*/
    H5O_MSG_PLINE,		/*0x000B Data storage -- filter pipeline */
    H5O_MSG_ATTR,		/*0x000C Attribute list			*/
    H5O_MSG_NAME,		/*0x000D Object name			*/
    H5O_MSG_MTIME,		/*0x000E Object modification date and time */
    H5O_MSG_SHARED,		/*0x000F Shared header message		*/
    H5O_MSG_CONT,		/*0x0010 Object header continuation	*/
    H5O_MSG_STAB,		/*0x0011 Symbol table			*/
    H5O_MSG_MTIME_NEW,		/*0x0012 New Object modification date and time */
};

/* Header object ID to class mapping */
/*
 * Initialize the object class info table.  Begin with the most general types
 * and end with the most specific. For instance, any object that has a
 * datatype message is a datatype but only some of them are datasets.
 */
const H5O_obj_class_t *const H5O_obj_class_g[] = {
    H5O_OBJ_DATATYPE,		/* Datatype object (H5G_TYPE - 2) */
    H5O_OBJ_DATASET,		/* Dataset object (H5G_DATASET - 1) */
    H5O_OBJ_GROUP,		/* Group object (H5G_GROUP - 0) */
};

/* Declare a free list to manage the H5O_t struct */
H5FL_DEFINE(H5O_t);

/* Declare a free list to manage the H5O_mesg_t sequence information */
H5FL_SEQ_DEFINE(H5O_mesg_t);

/* Declare a free list to manage the H5O_chunk_t sequence information */
H5FL_SEQ_DEFINE(H5O_chunk_t);

/* Declare a free list to manage the chunk image information */
H5FL_BLK_DEFINE(chunk_image);


/*****************************/
/* Library Private Variables */
/*****************************/

/* Declare external the free list for time_t's */
H5FL_EXTERN(time_t);


/*******************/
/* Local Variables */
/*******************/



/*-------------------------------------------------------------------------
 * Function:	H5Oopen
 *
 * Purpose:	Opens an object within an HDF5 file.
 *
 *              This function opens an object in the same way that H5Gopen,
 *              H5Topen, and H5Dopen do. However, H5Oopen doesn't require
 *              the type of object to be known beforehand. This can be
 *              useful in user-defined links, for instance, when only a
 *              path is known.
 *
 *              The opened object should be closed again with H5Oclose
 *              or H5Gclose, H5Tclose, or H5Dclose.
 *
 * Return:	Success:	An open object identifier
 *		Failure:	Negative
 *
 * Programmer:	James Laird
 *		July 14 2006
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Oopen(hid_t loc_id, const char *name, hid_t lapl_id)
{
    H5G_loc_t	loc;
    H5G_loc_t   obj_loc;                /* Location used to open group */
    H5G_name_t  obj_path;            	/* Opened object group hier. path */
    H5O_loc_t   obj_oloc;            	/* Opened object object location */
    hbool_t     loc_found = FALSE;      /* Entry at 'name' found */
    hid_t       ret_value = FAIL;

    FUNC_ENTER_API(H5Oopen, FAIL)
    H5TRACE3("i", "isi", loc_id, name, lapl_id);

    /* Check args */
    if(H5G_loc(loc_id, &loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
    if(!name || !*name)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name")

    /* Set up opened group location to fill in */
    obj_loc.oloc = &obj_oloc;
    obj_loc.path = &obj_path;
    H5G_loc_reset(&obj_loc);

    /* Find the object's location */
    if(H5G_loc_find(&loc, name, &obj_loc/*out*/, lapl_id, H5AC_dxpl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "object not found")
    loc_found = TRUE;

    /* Open the object */
    if((ret_value = H5O_open_by_loc(&obj_loc, H5AC_dxpl_id)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, FAIL, "unable to open object")

done:
    if(ret_value < 0 && loc_found)
        if(H5G_loc_free(&obj_loc) < 0)
            HDONE_ERROR(H5E_SYM, H5E_CANTRELEASE, FAIL, "can't free location")

    FUNC_LEAVE_API(ret_value)
} /* end H5Oopen() */


/*-------------------------------------------------------------------------
 * Function:	H5Oopen_by_idx
 *
 * Purpose:	Opens an object within an HDF5 file, according to the offset
 *              within an index.
 *
 *              This function opens an object in the same way that H5Gopen,
 *              H5Topen, and H5Dopen do. However, H5Oopen doesn't require
 *              the type of object to be known beforehand. This can be
 *              useful in user-defined links, for instance, when only a
 *              path is known.
 *
 *              The opened object should be closed again with H5Oclose
 *              or H5Gclose, H5Tclose, or H5Dclose.
 *
 * Return:	Success:	An open object identifier
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *		November 20 2006
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Oopen_by_idx(hid_t loc_id, const char *group_name, H5_index_t idx_type,
    H5_iter_order_t order, hsize_t n, hid_t lapl_id)
{
    H5G_loc_t	loc;
    H5G_loc_t   obj_loc;                /* Location used to open group */
    H5G_name_t  obj_path;            	/* Opened object group hier. path */
    H5O_loc_t   obj_oloc;            	/* Opened object object location */
    hbool_t     loc_found = FALSE;      /* Entry at 'name' found */
    hid_t       ret_value = FAIL;

    FUNC_ENTER_API(H5Oopen_by_idx, FAIL)
    H5TRACE6("i", "isIiIohi", loc_id, group_name, idx_type, order, n, lapl_id);

    /* Check args */
    if(H5G_loc(loc_id, &loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
    if(!group_name || !*group_name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name specified")
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

    /* Find the object's location, according to the order in the index */
    if(H5G_loc_find_by_idx(&loc, group_name, idx_type, order, n, &obj_loc/*out*/, lapl_id, H5AC_dxpl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "group not found")
    loc_found = TRUE;

    /* Open the object */
    if((ret_value = H5O_open_by_loc(&obj_loc, H5AC_dxpl_id)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, FAIL, "unable to open object")

done:
    /* Release the object location if we failed after copying it */
    if(ret_value < 0 && loc_found)
        if(H5G_loc_free(&obj_loc) < 0)
            HDONE_ERROR(H5E_SYM, H5E_CANTRELEASE, FAIL, "can't free location")

    FUNC_LEAVE_API(ret_value)
} /* end H5Oopen_by_idx() */


/*-------------------------------------------------------------------------
 * Function:	H5Oopen_by_addr
 *
 * Purpose:	Warning! This function is EXTREMELY DANGEROUS!
 *              Improper use can lead to FILE CORRUPTION, INACCESSIBLE DATA,
 *              and other VERY BAD THINGS!
 *
 *              This function opens an object using its address within the
 *              HDF5 file, similar to an HDF5 hard link. The open object
 *              is identical to an object opened with H5Oopen() and should
 *              be closed with H5Oclose() or a type-specific closing
 *              function (such as H5Gclose() ).
 *
 *              This function is very dangerous if called on an invalid
 *              address. For this reason, H5Oincr_refcount() should be
 *              used to prevent HDF5 from deleting any object that is
 *              referenced by address (e.g. by a user-defined link).
 *              H5Odecr_refcount() should be used when the object is
 *              no longer being referenced by address (e.g. when the UD link
 *              is deleted).
 *         
 *              The address of the HDF5 file on disk has no effect on
 *              H5Oopen_by_addr(), nor does the use of any unusual file
 *              drivers. The "address" is really the offset within the
 *              HDF5 file, and HDF5's file drivers will transparently
 *              map this to an address on disk for the filesystem.
 *
 * Return:	Success:	An open object identifier
 *		Failure:	Negative
 *
 * Programmer:	James Laird
 *		July 14 2006
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Oopen_by_addr(hid_t loc_id, haddr_t addr)
{
    H5G_loc_t	loc;
    H5G_loc_t   obj_loc;                /* Location used to open group */
    H5G_name_t  obj_path;            	/* Opened object group hier. path */
    H5O_loc_t   obj_oloc;            	/* Opened object object location */
    hbool_t     loc_found = FALSE;      /* Location at 'name' found */
    hid_t       ret_value = FAIL;

    FUNC_ENTER_API(H5Oopen_by_addr, FAIL)
    H5TRACE2("i", "ia", loc_id, addr);

    /* Check args */
    if(H5G_loc(loc_id, &loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
    if(!H5F_addr_defined(addr))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no address supplied")

    /* Set up opened group location to fill in */
    obj_loc.oloc = &obj_oloc;
    obj_loc.path = &obj_path;
    H5G_loc_reset(&obj_loc);
    obj_loc.oloc->addr = addr;
    obj_loc.oloc->file = loc.oloc->file;
    H5G_name_reset(obj_loc.path);       /* objects opened through this routine don't have a path name */

    /* Open the object */
    if((ret_value = H5O_open_by_loc(&obj_loc, H5AC_dxpl_id)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, FAIL, "unable to open object")

done:
    if(ret_value < 0 && loc_found)
        if(H5G_loc_free(&obj_loc) < 0)
            HDONE_ERROR(H5E_SYM, H5E_CANTRELEASE, FAIL, "can't free location")

    FUNC_LEAVE_API(ret_value)
} /* end H5Oopen_by_addr() */


/*-------------------------------------------------------------------------
 * Function:	H5Oincr_refcount
 *
 * Purpose:	Warning! This function is EXTREMELY DANGEROUS!
 *              Improper use can lead to FILE CORRUPTION, INACCESSIBLE DATA,
 *              and other VERY BAD THINGS!
 *
 *              This function increments the "hard link" reference count
 *              for an object. It should be used when a user-defined link
 *              that references an object by address is created. When the
 *              link is deleted, H5Odecr_refcount should be used.
 *
 * Return:	Success:	Non-negative
 *		Failure:	Negative
 *
 * Programmer:	James Laird
 *		July 14 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Oincr_refcount(hid_t object_id)
{
    H5O_loc_t  *oloc;
    herr_t      ret_value = SUCCEED;

    FUNC_ENTER_API(H5Oincr_refcount, FAIL)
    H5TRACE1("e", "i", object_id);

    /* Get the object's oloc so we can adjust its link count */
    if((oloc = H5O_get_loc(object_id)) == NULL)
        HGOTO_ERROR(H5E_ATOM, H5E_BADVALUE, FAIL, "unable to get object location from ID")

    if(H5O_link(oloc, 1, H5AC_dxpl_id) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_LINKCOUNT, FAIL, "modifying object link count failed")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5O_incr_refcount() */


/*-------------------------------------------------------------------------
 * Function:	H5Odecr_refcount
 *
 * Purpose:	Warning! This function is EXTREMELY DANGEROUS!
 *              Improper use can lead to FILE CORRUPTION, INACCESSIBLE DATA,
 *              and other VERY BAD THINGS!
 *
 *              This function decrements the "hard link" reference count
 *              for an object. It should be used when user-defined links
 *              that reference an object by address are deleted, and only
 *              after H5Oincr_refcount has already been used.
 *
 * Return:	Success:	Non-negative
 *		Failure:	Negative
 *
 * Programmer:	James Laird
 *		July 14 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Odecr_refcount(hid_t object_id)
{
    H5O_loc_t  *oloc;
    herr_t      ret_value = SUCCEED;

    FUNC_ENTER_API(H5Odecr_refcount, FAIL)
    H5TRACE1("e", "i", object_id);

    /* Get the object's oloc so we can adjust its link count */
    if((oloc = H5O_get_loc(object_id)) == NULL)
        HGOTO_ERROR(H5E_ATOM, H5E_BADVALUE, FAIL, "unable to get object location from ID")

    if(H5O_link(oloc, -1, H5AC_dxpl_id) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_LINKCOUNT, FAIL, "modifying object link count failed")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Odecr_refcount() */


/*-------------------------------------------------------------------------
 * Function:	H5Oget_info
 *
 * Purpose:	Retrieve information about an object.
 *
 * Return:	Success:	Non-negative
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *		November 21 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Oget_info(hid_t loc_id, const char *name, H5O_info_t *oinfo, hid_t lapl_id)
{
    H5G_loc_t	loc;                    /* Location of group */
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_API(H5Oget_info, FAIL)

    /* Check args */
    if(H5G_loc(loc_id, &loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
    if(!name || !*name)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name")
    if(!oinfo)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no info struct")
    if(H5P_DEFAULT == lapl_id)
        lapl_id = H5P_LINK_ACCESS_DEFAULT;
    else
        if(TRUE != H5P_isa_class(lapl_id, H5P_LINK_ACCESS))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not link access property list ID")

    /* Retrieve the object's information */
    if(H5G_loc_info(&loc, name, oinfo/*out*/, lapl_id, H5AC_ind_dxpl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "object not found")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Oget_info() */


/*-------------------------------------------------------------------------
 * Function:	H5Oget_info_by_idx
 *
 * Purpose:	Retrieve information about an object, according to the order
 *              of an index.
 *
 * Return:	Success:	Non-negative
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *		November 26 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Oget_info_by_idx(hid_t loc_id, const char *group_name, H5_index_t idx_type,
    H5_iter_order_t order, hsize_t n, H5O_info_t *oinfo, hid_t lapl_id)
{
    H5G_loc_t	loc;                    /* Location of group */
    H5G_loc_t   obj_loc;                /* Location used to open group */
    H5G_name_t  obj_path;            	/* Opened object group hier. path */
    H5O_loc_t   obj_oloc;            	/* Opened object object location */
    hbool_t     loc_found = FALSE;      /* Entry at 'name' found */
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_API(H5Oget_info_by_idx, FAIL)

    /* Check args */
    if(H5G_loc(loc_id, &loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
    if(!group_name || !*group_name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name specified")
    if(idx_type <= H5_INDEX_UNKNOWN || idx_type >= H5_INDEX_N)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid index type specified")
    if(order <= H5_ITER_UNKNOWN || order >= H5_ITER_N)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid iteration order specified")
    if(!oinfo)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no info struct")
    if(H5P_DEFAULT == lapl_id)
        lapl_id = H5P_LINK_ACCESS_DEFAULT;
    else
        if(TRUE != H5P_isa_class(lapl_id, H5P_LINK_ACCESS))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not link access property list ID")

    /* Set up opened group location to fill in */
    obj_loc.oloc = &obj_oloc;
    obj_loc.path = &obj_path;
    H5G_loc_reset(&obj_loc);

    /* Find the object's location, according to the order in the index */
    if(H5G_loc_find_by_idx(&loc, group_name, idx_type, order, n, &obj_loc/*out*/, lapl_id, H5AC_ind_dxpl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "group not found")
    loc_found = TRUE;

    /* Retrieve the object's information */
    if(H5O_get_info(obj_loc.oloc, oinfo, H5AC_ind_dxpl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't retrieve object info")

done:
    /* Release the object location */
    if(loc_found && H5G_loc_free(&obj_loc) < 0)
        HDONE_ERROR(H5E_SYM, H5E_CANTRELEASE, FAIL, "can't free location")

    FUNC_LEAVE_API(ret_value)
} /* end H5Oget_info_by_idx() */


/*-------------------------------------------------------------------------
 * Function:	H5Oclose
 *
 * Purpose:	Close an open file object.
 *
 *              This is the companion to H5Oopen. It is used to close any
 *              open object in an HDF5 file (but not IDs are that not file
 *              objects, such as property lists and dataspaces). It has
 *              the same effect as calling H5Gclose, H5Dclose, or H5Tclose.
 *
 * Return:	Success:	Non-negative
 *		Failure:	Negative
 *
 * Programmer:	James Laird
 *		July 14 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Oclose(hid_t object_id)
{
    herr_t       ret_value = SUCCEED;

    FUNC_ENTER_API(H5Oclose, FAIL)
    H5TRACE1("e", "i", object_id);

    /* Get the type of the object and close it in the correct way */
    switch(H5I_get_type(object_id))
    {
        case(H5I_GROUP):
        case(H5I_DATATYPE):
        case(H5I_DATASET):
            if(H5I_object(object_id) == NULL)
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a valid object")
            if(H5I_dec_ref(object_id) < 0)
                HGOTO_ERROR(H5E_OHDR, H5E_CANTRELEASE, FAIL, "unable to close object")
            break;

        default:
            HGOTO_ERROR(H5E_ARGS, H5E_CANTRELEASE, FAIL, "not a valid file object ID (dataset, group, or datatype)")
        break;
    } /* end switch */

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Oclose() */


/*-------------------------------------------------------------------------
 * Function:	H5O_create
 *
 * Purpose:	Creates a new object header. Allocates space for it and
 *              then calls an initialization function. The object header
 *              is opened for write access and should eventually be
 *              closed by calling H5O_close().
 *
 * Return:	Success:	Non-negative, the ENT argument contains
 *				information about the object header,
 *				including its address.
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug  5 1997
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_create(H5F_t *f, hid_t dxpl_id, size_t size_hint, hid_t ocpl_id,
    H5O_loc_t *loc/*out*/)
{
    haddr_t	header;                 /* Address of object header */
    herr_t      ret_value = SUCCEED;    /* return value */

    FUNC_ENTER_NOAPI(H5O_create, FAIL)

    /* check args */
    HDassert(f);
    HDassert(loc);

    /* Make certain we allocate at least a reasonable size for the object header */
    size_hint = H5O_ALIGN_F(f, MAX(H5O_MIN_SIZE, size_hint));

    /* allocate disk space for header and first chunk */
    if(HADDR_UNDEF == (header = H5MF_alloc(f, H5FD_MEM_OHDR, dxpl_id,
            (hsize_t)H5O_SIZEOF_HDR_F(f) + size_hint)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "file allocation failed for object header header")

    /* initialize the object header */
    if(H5O_new(f, dxpl_id, header, size_hint, ocpl_id, loc) != SUCCEED)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, FAIL, "unable to initialize object header")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_create() */


/*-------------------------------------------------------------------------
 * Function:	H5O_new
 *
 * Purpose:	Initialize a new object header, sets the link count to 0,
 *              and caches the header. The object header is opened for
 *              write access and should eventually be closed by calling
 *              H5O_close().
 *
 * Return:	Success:    SUCCEED, the LOC argument contains
 *                          information about the object header,
 *                          including its address.
 *		Failure:    FAIL
 *
 * Programmer:	Bill Wendling
 *		1, November 2002
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_new(H5F_t *f, hid_t dxpl_id, haddr_t header, size_t chunk_size,
    hid_t ocpl_id, H5O_loc_t *loc/*out*/)
{
    H5O_t      *oh = NULL;
    size_t      oh_size;                /* Size of initial object header */
    herr_t      ret_value = SUCCEED;    /* return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_new)

    /* check args */
    HDassert(f);
    HDassert(loc);
    HDassert(TRUE == H5P_isa_class(ocpl_id, H5P_OBJECT_CREATE));

    /* Set up object location */
    loc->file = f;
    loc->addr = header;

    /* Allocate the object header and fill in header fields */
    if(NULL == (oh = H5FL_MALLOC(H5O_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")

    /* Initialize rudimentary information object object header */
    oh->version = H5F_USE_LATEST_FORMAT(f) ? H5O_VERSION_LATEST : H5O_VERSION_1;
    oh->nlink = 0;
    oh->skipped_mesg_size = 0;
    oh->sizeof_size = H5F_SIZEOF_SIZE(f);
    oh->sizeof_addr = H5F_SIZEOF_ADDR(f);

    /* Initialize version-specific fields */
    if(oh->version > H5O_VERSION_1) {
        H5P_genplist_t  *oc_plist;          /* Object creation property list */

        /* Initialize all time fields with current time */
        oh->atime = oh->mtime = oh->ctime = oh->btime = H5_now();

        /* Get the property list */
        if(NULL == (oc_plist = H5I_object(ocpl_id)))
            HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a property list")

        /* Initialize attribute tracking fields */

        /* Retrieve phase change values from property list */
        if(H5P_get(oc_plist, H5O_CRT_ATTR_MAX_COMPACT_NAME, &oh->max_compact) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get max. # of compact attributes")
        if(H5P_get(oc_plist, H5O_CRT_ATTR_MIN_DENSE_NAME, &oh->min_dense) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get min. # of dense attributes")

        /* Set starting values for attribute info */
        oh->nattrs = 0;
        oh->attr_fheap_addr = HADDR_UNDEF;
        oh->name_bt2_addr = HADDR_UNDEF;
    } /* end if */
    else {
        /* Reset unused time fields */
        oh->atime = oh->mtime = oh->ctime = oh->btime = 0;

        /* Reset unused attribute fields */
        oh->max_compact = 0;
        oh->min_dense = 0;
        oh->nattrs = 0;
        oh->attr_fheap_addr = HADDR_UNDEF;
        oh->name_bt2_addr = HADDR_UNDEF;
    } /* end else */

    /* Compute total size of initial object header */
    /* (i.e. object header prefix and first chunk) */
    oh_size = H5O_SIZEOF_HDR_OH(oh) + chunk_size;

    /* Create the chunk list */
    oh->nchunks = oh->alloc_nchunks = 1;
    if(NULL == (oh->chunk = H5FL_SEQ_MALLOC(H5O_chunk_t, (size_t)oh->alloc_nchunks)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")

    /* Initialize the first chunk */
    oh->chunk[0].dirty = TRUE;
    oh->chunk[0].addr = loc->addr;
    oh->chunk[0].size = oh_size;
    oh->chunk[0].gap = 0;

    /* Allocate enough space for the first chunk */
    /* (including space for serializing the object header prefix */
    if(NULL == (oh->chunk[0].image = H5FL_BLK_CALLOC(chunk_image, oh_size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")

    /* Put magic # for object header in first chunk */
    if(oh->version > H5O_VERSION_1)
        HDmemcpy(oh->chunk[0].image, H5O_HDR_MAGIC, (size_t)H5O_SIZEOF_MAGIC);

    /* Create the message list */
    oh->nmesgs = 1;
    oh->alloc_nmesgs = H5O_NMESGS;
    if(NULL == (oh->mesg = H5FL_SEQ_CALLOC(H5O_mesg_t, oh->alloc_nmesgs)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")

    /* Initialize the first message */
    oh->mesg[0].type = H5O_MSG_NULL;
    oh->mesg[0].dirty = TRUE;
    oh->mesg[0].native = NULL;
    oh->mesg[0].raw = oh->chunk[0].image + (H5O_SIZEOF_HDR_OH(oh) - H5O_SIZEOF_CHKSUM_OH(oh)) + H5O_SIZEOF_MSGHDR_OH(oh);
    oh->mesg[0].raw_size = chunk_size - H5O_SIZEOF_MSGHDR_OH(oh);
    oh->mesg[0].chunkno = 0;

    /* Cache object header */
    if(H5AC_set(f, dxpl_id, H5AC_OHDR, loc->addr, oh, H5AC__NO_FLAGS_SET) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, FAIL, "unable to cache object header")

    /* Open it */
    if(H5O_open(loc) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTOPENOBJ, FAIL, "unable to open object header")

done:
    if(ret_value < 0 && oh) {
        if(H5O_dest(f, oh) < 0)
	    HDONE_ERROR(H5E_OHDR, H5E_CANTFREE, FAIL, "unable to destroy object header data")
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_new() */


/*-------------------------------------------------------------------------
 * Function:	H5O_open
 *
 * Purpose:	Opens an object header which is described by the symbol table
 *		entry OBJ_ENT.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Monday, January	 5, 1998
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_open(const H5O_loc_t *loc)
{
    FUNC_ENTER_NOAPI_NOFUNC(H5O_open)

    /* Check args */
    HDassert(loc);
    HDassert(loc->file);

#ifdef H5O_DEBUG
    if (H5DEBUG(O))
	HDfprintf(H5DEBUG(O), "> %a\n", loc->addr);
#endif

    /* Increment open-lock counters */
    loc->file->nopen_objs++;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5O_open() */


/*-------------------------------------------------------------------------
 * Function:	H5O_open_by_loc
 *
 * Purpose:	Opens an object and returns an ID given its group loction.
 *
 * Return:	Success:	Open object identifier
 *		Failure:	Negative
 *
 * Programmer:	James Laird
 *		July 25 2006
 *
 *-------------------------------------------------------------------------
 */
static hid_t
H5O_open_by_loc(H5G_loc_t *obj_loc, hid_t dxpl_id)
{
    const H5O_obj_class_t *obj_class;   /* Class of object for location */
    hid_t      ret_value;               /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_open_by_loc)

    HDassert(obj_loc);

    /* Get the object class for this location */
    if(NULL == (obj_class = H5O_obj_class(obj_loc->oloc, dxpl_id)))
        HGOTO_ERROR(H5E_OHDR, H5E_CANTGET, FAIL, "unable to determine object class")

    /* Call the object class's 'open' routine */
    HDassert(obj_class->open);
    if((ret_value = obj_class->open(obj_loc, dxpl_id)) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTOPENOBJ, FAIL, "unable to open object")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_open_by_loc() */


/*-------------------------------------------------------------------------
 * Function:	H5O_close
 *
 * Purpose:	Closes an object header that was previously open.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Monday, January	 5, 1998
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_close(H5O_loc_t *loc)
{
    herr_t ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(H5O_close, FAIL)

    /* Check args */
    HDassert(loc);
    HDassert(loc->file);
    HDassert(loc->file->nopen_objs > 0);

    /* Decrement open-lock counters */
    --loc->file->nopen_objs;

#ifdef H5O_DEBUG
    if(H5DEBUG(O)) {
	if(loc->file->file_id < 0 && 1 == loc->file->shared->nrefs)
	    HDfprintf(H5DEBUG(O), "< %a auto %lu remaining\n",
		      loc->addr,
		      (unsigned long)(loc->file->nopen_objs));
	else
	    HDfprintf(H5DEBUG(O), "< %a\n", loc->addr);
    } /* end if */
#endif

    /*
     * If the file open object count has reached the number of open mount points
     * (each of which has a group open in the file) attempt to close the file.
     */
    if(loc->file->nopen_objs == loc->file->mtab.nmounts)
        /* Attempt to close down the file hierarchy */
        if(H5F_try_close(loc->file) < 0)
            HGOTO_ERROR(H5E_OHDR, H5E_CANTCLOSEFILE, FAIL, "problem attempting file close")

    /* Release location information */
    if(H5O_loc_free(loc) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTRELEASE, FAIL, "problem attempting to free location")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_close() */


/*-------------------------------------------------------------------------
 * Function:	H5O_link
 *
 * Purpose:	Adjust the link count for an object header by adding
 *		ADJUST to the link count.
 *
 * Return:	Success:	New link count
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug  5 1997
 *
 *-------------------------------------------------------------------------
 */
int
H5O_link(const H5O_loc_t *loc, int adjust, hid_t dxpl_id)
{
    H5O_t	*oh = NULL;
    unsigned oh_flags = H5AC__NO_FLAGS_SET; /* Whether the object was deleted  */
    int	ret_value;                          /* Return value */

    FUNC_ENTER_NOAPI(H5O_link, FAIL)

    /* check args */
    HDassert(loc);
    HDassert(loc->file);
    HDassert(H5F_addr_defined(loc->addr));
    if(adjust != 0 && 0 == (H5F_INTENT(loc->file) & H5F_ACC_RDWR))
	HGOTO_ERROR(H5E_OHDR, H5E_WRITEERROR, FAIL, "no write intent on file")

    /* get header */
    if(NULL == (oh = H5AC_protect(loc->file, dxpl_id, H5AC_OHDR, loc->addr,
				   NULL, NULL, H5AC_WRITE)))
	HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, FAIL, "unable to load object header")

    /* adjust link count */
    if(adjust < 0) {
        /* Check for too large of an adjustment */
        if((unsigned)(-adjust) > oh->nlink)
	    HGOTO_ERROR(H5E_OHDR, H5E_LINKCOUNT, FAIL, "link count would be negative")
	oh->nlink += adjust;
        oh_flags |= H5AC__DIRTIED_FLAG;

        /* Check if the object should be deleted */
        if(oh->nlink == 0) {
            /* Check if the object is still open by the user */
            if(H5FO_opened(loc->file, loc->addr) != NULL) {
                /* Flag the object to be deleted when it's closed */
                if(H5FO_mark(loc->file, loc->addr, TRUE) < 0)
                    HGOTO_ERROR(H5E_OHDR, H5E_CANTDELETE, FAIL, "can't mark object for deletion")
            } /* end if */
            else {
                /* Delete object right now */
                if(H5O_delete_oh(loc->file, dxpl_id, oh) < 0)
                    HGOTO_ERROR(H5E_OHDR, H5E_CANTDELETE, FAIL, "can't delete object from file")

                /* Mark the object header as deleted */
                oh_flags = H5C__DELETED_FLAG;
            } /* end else */
        } /* end if */
    } else if (adjust > 0) {
        /* A new object, or one that will be deleted */
        if(oh->nlink == 0) {
            /* Check if the object is current open, but marked for deletion */
            if(H5FO_marked(loc->file, loc->addr) > 0) {
                /* Remove "delete me" flag on the object */
                if(H5FO_mark(loc->file, loc->addr, FALSE) < 0)
                    HGOTO_ERROR(H5E_OHDR, H5E_CANTDELETE, FAIL, "can't mark object for deletion")
            } /* end if */
        } /* end if */

	oh->nlink += adjust;
        oh_flags |= H5AC__DIRTIED_FLAG;
    } /* end if */

    /* Set return value */
    ret_value = oh->nlink;

done:
    if(oh && H5AC_unprotect(loc->file, dxpl_id, H5AC_OHDR, loc->addr, oh, oh_flags) < 0)
	HDONE_ERROR(H5E_OHDR, H5E_PROTECT, FAIL, "unable to release object header")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_link() */


/*-------------------------------------------------------------------------
 * Function:	H5O_protect
 *
 * Purpose:	Wrapper around H5AC_protect for use during a H5O_protect->
 *              H5O_msg_append->...->H5O_msg_append->H5O_unprotect sequence of calls
 *              during an object's creation.
 *
 * Return:	Success:	Pointer to the object header structure for the
 *                              object.
 *
 *		Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Dec 31 2002
 *
 *-------------------------------------------------------------------------
 */
H5O_t *
H5O_protect(H5O_loc_t *loc, hid_t dxpl_id)
{
    H5O_t	       *ret_value;      /* Return value */

    FUNC_ENTER_NOAPI(H5O_protect, NULL)

    /* check args */
    HDassert(loc);
    HDassert(loc->file);
    HDassert(H5F_addr_defined(loc->addr));

    /* Check for write access on the file */
    if(0 == (H5F_INTENT(loc->file) & H5F_ACC_RDWR))
	HGOTO_ERROR(H5E_OHDR, H5E_WRITEERROR, NULL, "no write intent on file")

    /* Lock the object header into the cache */
    if(NULL == (ret_value = H5AC_protect(loc->file, dxpl_id, H5AC_OHDR, loc->addr, NULL, NULL, H5AC_WRITE)))
	HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, NULL, "unable to load object header")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_protect() */


/*-------------------------------------------------------------------------
 * Function:	H5O_unprotect
 *
 * Purpose:	Wrapper around H5AC_unprotect for use during a H5O_protect->
 *              H5O_msg_append->...->H5O_msg_append->H5O_unprotect sequence of calls
 *              during an object's creation.
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Dec 31 2002
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_unprotect(H5O_loc_t *loc, H5O_t *oh, hid_t dxpl_id, unsigned oh_flags)
{
    herr_t ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(H5O_unprotect, FAIL)

    /* check args */
    HDassert(loc);
    HDassert(loc->file);
    HDassert(H5F_addr_defined(loc->addr));
    HDassert(oh);

    /* Release the object header from the cache */
    if(H5AC_unprotect(loc->file, dxpl_id, H5AC_OHDR, loc->addr, oh, oh_flags) < 0)
	HGOTO_ERROR(H5E_OHDR, H5E_PROTECT, FAIL, "unable to release object header")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_unprotect() */


/*-------------------------------------------------------------------------
 * Function:	H5O_touch_oh
 *
 * Purpose:	If FORCE is non-zero then create a modification time message
 *		unless one already exists.  Then update any existing
 *		modification time message with the current time.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Monday, July 27, 1998
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_touch_oh(H5F_t *f,
             hid_t dxpl_id,
	     H5O_t *oh,
	     hbool_t force,
             unsigned * oh_flags_ptr)
{
    time_t	now;                    /* Current time */
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_touch_oh)

    HDassert(oh);
    HDassert(oh_flags_ptr);

    /* Get current time */
    now = H5_now();

    /* Check version, to determine how to store time information */
    if(oh->version == H5O_VERSION_1) {
        unsigned	idx;                    /* Index of modification time message to update */

        /* Look for existing message */
        for(idx = 0; idx < oh->nmesgs; idx++)
            if(H5O_MSG_MTIME == oh->mesg[idx].type || H5O_MSG_MTIME_NEW == oh->mesg[idx].type)
                break;

        /* Create a new message, if necessary */
        if(idx == oh->nmesgs) {
            size_t	size;           /* New modification time message size */

            /* If we would have to create a new message, but we aren't 'forcing' it, get out now */
            if(!force)
                HGOTO_DONE(SUCCEED);        /*nothing to do*/

            size = (H5O_MSG_MTIME_NEW->raw_size)(f, &now);
            if((idx = H5O_alloc(f, dxpl_id, oh, H5O_MSG_MTIME_NEW, size, oh_flags_ptr)) == UFAIL)
                HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, FAIL, "unable to allocate space for modification time message")
        } /* end if */

        /* Allocate 'native' space, if necessary */
        if(NULL == oh->mesg[idx].native) {
            if(NULL == (oh->mesg[idx].native = H5FL_MALLOC(time_t)))
                HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, FAIL, "memory allocation failed for modification time message")
        } /* end if */

        /* Update the message */
        *((time_t *)(oh->mesg[idx].native)) = now;

        /* Mark the message as dirty */
        oh->mesg[idx].dirty = TRUE;
    } /* end if */
    else {
        /* XXX: For now, update access time & change fields in the object header */
        /* (will need to add some code to update modification time appropriately) */
        oh->atime = oh->ctime = now;
    } /* end else */

    /* Mark the object header as dirty */
    *oh_flags_ptr |= H5AC__DIRTIED_FLAG;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_touch_oh() */


/*-------------------------------------------------------------------------
 * Function:	H5O_touch
 *
 * Purpose:	Touch an object by setting the modification time to the
 *		current time and marking the object as dirty.  Unless FORCE
 *		is non-zero, nothing happens if there is no MTIME message in
 *		the object header.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Monday, July 27, 1998
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_touch(H5O_loc_t *loc, hbool_t force, hid_t dxpl_id)
{
    H5O_t	*oh = NULL;
    unsigned 	oh_flags = H5AC__NO_FLAGS_SET;
    herr_t      ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5O_touch, FAIL)

    /* check args */
    HDassert(loc);
    HDassert(loc->file);
    HDassert(H5F_addr_defined(loc->addr));
    if(0 == (H5F_INTENT(loc->file) & H5F_ACC_RDWR))
	HGOTO_ERROR(H5E_OHDR, H5E_WRITEERROR, FAIL, "no write intent on file")

    /* Get the object header */
    if(NULL == (oh = H5AC_protect(loc->file, dxpl_id, H5AC_OHDR, loc->addr, NULL, NULL, H5AC_WRITE)))
	HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, FAIL, "unable to load object header")

    /* Create/Update the modification time message */
    if(H5O_touch_oh(loc->file, dxpl_id, oh, force, &oh_flags) < 0)
	HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, FAIL, "unable to update object modificaton time")

done:
    if(oh && H5AC_unprotect(loc->file, dxpl_id, H5AC_OHDR, loc->addr, oh, oh_flags) < 0)
	HDONE_ERROR(H5E_OHDR, H5E_PROTECT, FAIL, "unable to release object header")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_touch() */

#ifdef H5O_ENABLE_BOGUS

/*-------------------------------------------------------------------------
 * Function:	H5O_bogus_oh
 *
 * Purpose:	Create a "bogus" message unless one already exists.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              <koziol@ncsa.uiuc.edu>
 *              Tuesday, January 21, 2003
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_bogus_oh(H5F_t *f, hid_t dxpl_id, H5O_t *oh, hbool_t * oh_flags_ptr)
{
    int	idx;
    herr_t      ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER(H5O_bogus_oh, FAIL)

    HDassert(f);
    HDassert(oh);
    HDassert(oh_flags_ptr);

    /* Look for existing message */
    for(idx = 0; idx < oh->nmesgs; idx++)
	if(H5O_MSG_BOGUS == oh->mesg[idx].type)
            break;

    /* Create a new message */
    if(idx == oh->nmesgs) {
        size_t	size = (H5O_MSG_BOGUS->raw_size)(f, NULL);

	if((idx = H5O_alloc(f, dxpl_id, oh, H5O_MSG_BOGUS, size, oh_flags_ptr)) < 0)
	    HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, FAIL, "unable to allocate space for 'bogus' message")

        /* Allocate the native message in memory */
	if(NULL == (oh->mesg[idx].native = H5MM_malloc(sizeof(H5O_bogus_t))))
	    HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, FAIL, "memory allocation failed for 'bogus' message")

        /* Update the native part */
        ((H5O_bogus_t *)(oh->mesg[idx].native))->u = H5O_BOGUS_VALUE;

        /* Mark the message and object header as dirty */
	*oh_flags_ptr = TRUE;
        oh->mesg[idx].dirty = TRUE;
        oh->dirty = TRUE;
    } /* end if */

done:
    FUNC_LEAVE(ret_value)
} /* end H5O_bogus_oh() */


/*-------------------------------------------------------------------------
 * Function:	H5O_bogus
 *
 * Purpose:	Create a "bogus" message in an object.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              <koziol@ncsa.uiuc.edu>
 *              Tuesday, January 21, 2003
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_bogus(H5O_loc_t *loc, hid_t dxpl_id)
{
    H5O_t	*oh = NULL;
    unsigned	oh_flags = H5AC__NO_FLAGS_SET;
    herr_t	ret_value = SUCCEED;

    FUNC_ENTER(H5O_bogus, FAIL)

    /* check args */
    HDassert(loc);
    HDassert(loc->file);
    HDassert(H5F_addr_defined(loc->addr));

    /* Verify write access to the file */
    if(0 == (H5F_INTENT(loc->file) & H5F_ACC_RDWR))
	HGOTO_ERROR(H5E_OHDR, H5E_WRITEERROR, FAIL, "no write intent on file")

    /* Get the object header */
    if(NULL == (oh = H5AC_protect(loc->file, dxpl_id, H5AC_OHDR, loc->addr, NULL, NULL, H5AC_WRITE)))
	HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, FAIL, "unable to load object header")

    /* Create the "bogus" message */
    if(H5O_bogus_oh(ent->file, dxpl_id, oh, &oh_flags) < 0)
	HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, FAIL, "unable to update object 'bogus' message")

done:
    if(oh && H5AC_unprotect(ent->file, dxpl_id, H5AC_OHDR, ent->header, oh, oh_flags) < 0)
	HDONE_ERROR(H5E_OHDR, H5E_PROTECT, FAIL, "unable to release object header")

    FUNC_LEAVE(ret_value)
} /* end H5O_bogus() */
#endif /* H5O_ENABLE_BOGUS */


/*-------------------------------------------------------------------------
 * Function:	H5O_delete
 *
 * Purpose:	Delete an object header from a file.  This frees the file
 *              space used for the object header (and it's continuation blocks)
 *              and also walks through each header message and asks it to
 *              remove all the pieces of the file referenced by the header.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Mar 19 2003
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_delete(H5F_t *f, hid_t dxpl_id, haddr_t addr)
{
    H5O_t *oh = NULL;           /* Object header information */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI(H5O_delete,FAIL)

    /* Check args */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));

    /* Get the object header information */
    if(NULL == (oh = H5AC_protect(f, dxpl_id, H5AC_OHDR, addr, NULL, NULL, H5AC_WRITE)))
	HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, FAIL, "unable to load object header")

    /* Delete object */
    if(H5O_delete_oh(f, dxpl_id, oh) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTDELETE, FAIL, "can't delete object from file")

done:
    if(oh && H5AC_unprotect(f, dxpl_id, H5AC_OHDR, addr, oh, H5AC__DIRTIED_FLAG | H5C__DELETED_FLAG) < 0)
	HDONE_ERROR(H5E_OHDR, H5E_PROTECT, FAIL, "unable to release object header")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_delete() */


/*-------------------------------------------------------------------------
 * Function:	H5O_delete_oh
 *
 * Purpose:	Internal function to:
 *              Delete an object header from a file.  This frees the file
 *              space used for the object header (and it's continuation blocks)
 *              and also walks through each header message and asks it to
 *              remove all the pieces of the file referenced by the header.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Mar 19 2003
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_delete_oh(H5F_t *f, hid_t dxpl_id, H5O_t *oh)
{
    H5O_mesg_t *curr_msg;       /* Pointer to current message being operated on */
    unsigned	u;
    herr_t ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_delete_oh)

    /* Check args */
    HDassert(f);
    HDassert(oh);

    /* Walk through the list of object header messages, asking each one to
     * delete any file space used
     */
    for(u = 0, curr_msg = &oh->mesg[0]; u < oh->nmesgs; u++, curr_msg++) {
        /* Free any space referred to in the file from this message */
        if(H5O_delete_mesg(f, dxpl_id, curr_msg, TRUE) < 0)
            HGOTO_ERROR(H5E_OHDR, H5E_CANTDELETE, FAIL, "unable to delete file space for object header message")
    } /* end for */

    /* Check for dense attribute storage & delete it if necessary */
    if(oh->version > H5O_VERSION_1 && H5F_addr_defined(oh->attr_fheap_addr)) {
        if(H5A_dense_delete(f, dxpl_id, oh) < 0)
            HGOTO_ERROR(H5E_OHDR, H5E_CANTDELETE, FAIL, "unable to delete dense attribute storage")
    } /* end if */

    /* Free main (first) object header "chunk" */
    if(H5MF_xfree(f, H5FD_MEM_OHDR, dxpl_id, oh->chunk[0].addr, (hsize_t)oh->chunk[0].size) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTFREE, FAIL, "unable to free object header")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_delete_oh() */


/*-------------------------------------------------------------------------
 * Function:	H5O_obj_type
 *
 * Purpose:	Retrieves the type of object pointed to by `loc'.
 *
 * Return:	Success:	An object type defined in H5Gpublic.h
 *		Failure:	H5G_UNKNOWN
 *
 * Programmer:	Robb Matzke
 *              Wednesday, November  4, 1998
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_obj_type(const H5O_loc_t *loc, H5O_type_t *obj_type, hid_t dxpl_id)
{
    H5O_t	*oh = NULL;             /* Object header for location */
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI(H5O_obj_type, FAIL)

    /* Load the object header */
    if(NULL == (oh = H5AC_protect(loc->file, dxpl_id, H5AC_OHDR, loc->addr, NULL, NULL, H5AC_READ)))
	HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, FAIL, "unable to load object header")

    /* Retrieve the type of the object */
    if(H5O_obj_type_real(oh, obj_type) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, FAIL, "unable to determine object type")

done:
    if(oh && H5AC_unprotect(loc->file, dxpl_id, H5AC_OHDR, loc->addr, oh, H5AC__NO_FLAGS_SET) != SUCCEED)
	HDONE_ERROR(H5E_OHDR, H5E_PROTECT, FAIL, "unable to release object header")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_obj_type() */


/*-------------------------------------------------------------------------
 * Function:	H5O_obj_type_real
 *
 * Purpose:	Returns the type of object pointed to by `oh'.
 *
 * Return:	Success:	An object type defined in H5Opublic.h
 *		Failure:	H5G_UNKNOWN
 *
 * Programmer:	Quincey Koziol
 *              Monday, November 21, 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_obj_type_real(H5O_t *oh, H5O_type_t *obj_type)
{
    const H5O_obj_class_t *obj_class;           /* Class of object for header */
    herr_t ret_value = SUCCEED;                 /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_obj_type_real)

    /* Sanity check */
    HDassert(oh);
    HDassert(obj_type);

    /* Look up class for object header */
    if(NULL == (obj_class = H5O_obj_class_real(oh)))
        HGOTO_ERROR(H5E_OHDR, H5E_CANTGET, FAIL, "unable to determine object type")

    /* Set object type */
    *obj_type = obj_class->type;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_obj_type_real() */


/*-------------------------------------------------------------------------
 * Function:	H5O_obj_class
 *
 * Purpose:	Returns the class of object pointed to by `loc'.
 *
 * Return:	Success:	An object class
 *		Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *              Monday, November  6, 2006
 *
 *-------------------------------------------------------------------------
 */
static const H5O_obj_class_t *
H5O_obj_class(H5O_loc_t *loc, hid_t dxpl_id)
{
    H5O_t	*oh = NULL;                     /* Object header for location */
    const H5O_obj_class_t *ret_value;           /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_obj_class)

    /* Load the object header */
    if(NULL == (oh = H5AC_protect(loc->file, dxpl_id, H5AC_OHDR, loc->addr, NULL, NULL, H5AC_READ)))
	HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, NULL, "unable to load object header")

    /* Test whether entry qualifies as a particular type of object */
    if(NULL == (ret_value = H5O_obj_class_real(oh)))
        HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, NULL, "unable to determine object type")

done:
    if(oh && H5AC_unprotect(loc->file, dxpl_id, H5AC_OHDR, loc->addr, oh, H5AC__NO_FLAGS_SET) != SUCCEED)
	HDONE_ERROR(H5E_OHDR, H5E_PROTECT, NULL, "unable to release object header")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_obj_class() */


/*-------------------------------------------------------------------------
 * Function:	H5O_obj_class_real
 *
 * Purpose:	Returns the class of object pointed to by `oh'.
 *
 * Return:	Success:	An object class
 *		Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *              Monday, November 21, 2005
 *
 *-------------------------------------------------------------------------
 */
const H5O_obj_class_t *
H5O_obj_class_real(H5O_t *oh)
{
    size_t	i;                      /* Local index variable */
    const H5O_obj_class_t *ret_value;   /* Return value */

    FUNC_ENTER_NOAPI(H5O_obj_class_real, NULL)

    /* Sanity check */
    HDassert(oh);

    /* Test whether entry qualifies as a particular type of object */
    /* (Note: loop is in reverse order, to test specific objects first) */
    for(i = NELMTS(H5O_obj_class_g); i > 0; --i) {
        htri_t	isa;            /* Is entry a particular type? */

	if((isa = (H5O_obj_class_g[i - 1]->isa)(oh)) < 0)
	    HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, NULL, "unable to determine object type")
	else if(isa)
	    HGOTO_DONE(H5O_obj_class_g[i - 1])
    } /* end for */

    if(0 == i)
	HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, NULL, "unable to determine object type")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_obj_class_real() */


/*-------------------------------------------------------------------------
 * Function:	H5O_get_loc
 *
 * Purpose:	Gets the object location for an object given its ID.
 *
 * Return:	Success:	Pointer to H5O_loc_t
 *		Failure:	NULL
 *
 * Programmer:	James Laird
 *		July 25 2006
 *
 *-------------------------------------------------------------------------
 */
H5O_loc_t *
H5O_get_loc(hid_t object_id)
{
    H5O_loc_t   *ret_value;     /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_get_loc)

    switch(H5I_get_type(object_id))
    {
        case(H5I_GROUP):
            if(NULL == (ret_value = H5O_OBJ_GROUP->get_oloc(object_id)))
                HGOTO_ERROR(H5E_OHDR, H5E_CANTGET, NULL, "unable to get object location from group ID")
            break;

        case(H5I_DATASET):
            if(NULL == (ret_value = H5O_OBJ_DATASET->get_oloc(object_id)))
                HGOTO_ERROR(H5E_OHDR, H5E_CANTGET, NULL, "unable to get object location from dataset ID")
            break;

        case(H5I_DATATYPE):
            if(NULL == (ret_value = H5O_OBJ_DATATYPE->get_oloc(object_id)))
                HGOTO_ERROR(H5E_OHDR, H5E_CANTGET, NULL, "unable to get object location from datatype ID")
            break;

        default:
            HGOTO_ERROR(H5E_OHDR, H5E_BADTYPE, NULL, "invalid object type")
    } /* end switch */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_get_loc() */


/*-------------------------------------------------------------------------
 * Function:	H5O_loc_reset
 *
 * Purpose:	Reset a object location to an empty state
 *
 * Return:	Success:	Non-negative
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *              Monday, September 19, 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_loc_reset(H5O_loc_t *loc)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_loc_reset)

    /* Check arguments */
    HDassert(loc);

    /* Clear the object location to an empty state */
    HDmemset(loc, 0, sizeof(H5O_loc_t));
    loc->addr = HADDR_UNDEF;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5O_loc_reset() */


/*-------------------------------------------------------------------------
 * Function:    H5O_loc_copy
 *
 * Purpose:     Copy object location information
 *
 * Return:	Success:	Non-negative
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *              koziol@ncsa.uiuc.edu
 *              Monday, September 19, 2005
 *
 * Notes:       'depth' parameter determines how much of the group entry
 *              structure we want to copy.  The values are:
 *                  H5_COPY_SHALLOW - Copy all the field values from the source
 *                      to the destination, but not copying objects pointed to.
 *                      (Destination "takes ownership" of objects pointed to)
 *                  H5_COPY_DEEP - Copy all the fields from the source to
 *                      the destination, deep copying objects pointed to.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_loc_copy(H5O_loc_t *dst, const H5O_loc_t *src, H5_copy_depth_t depth)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_loc_copy)

    /* Check arguments */
    HDassert(src);
    HDassert(dst);
    HDassert(depth == H5_COPY_SHALLOW || depth == H5_COPY_DEEP);

    /* Copy the top level information */
    HDmemcpy(dst, src, sizeof(H5O_loc_t));

    /* Deep copy the names */
    if(depth == H5_COPY_DEEP) {
        /* If the original entry was holding open the file, this one should
         * hold it open, too.
         */
        if(src->holding_file)
            dst->file->nopen_objs++;
    } else if(depth == H5_COPY_SHALLOW) {
        /* Discarding 'const' qualifier OK - QAK */
        H5O_loc_reset((H5O_loc_t *)src);
    } /* end if */

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5O_loc_copy() */


/*-------------------------------------------------------------------------
 * Function:	H5O_loc_hold_file
 *
 * Purpose:	Have this object header hold a file open until it is
 *              released.
 *
 * Return:	Success:	Non-negative
 *		Failure:	Negative
 *
 * Programmer:	James Laird
 *              Wednesday, August 16, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_loc_hold_file(H5O_loc_t *loc)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_loc_hold_file)

    /* Check arguments */
    HDassert(loc);
    HDassert(loc->file);

    /* If this location is not already holding its file open, do so. */
    if(!loc->holding_file)
    {
        loc->file->nopen_objs++;
        loc->holding_file = TRUE;
    }

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5O_loc_hold_file() */


/*-------------------------------------------------------------------------
 * Function:	H5O_loc_free
 *
 * Purpose:	Release resources used by this object header location.
 *              Not to be confused with H5O_close; this is used on
 *              locations that don't correspond to open objects.
 *
 * Return:	Success:	Non-negative
 *		Failure:	Negative
 *
 * Programmer:	James Laird
 *              Wednesday, August 16, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_loc_free(H5O_loc_t *loc)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT(H5O_loc_free)

    /* Check arguments */
    HDassert(loc);

    /* If this location is holding its file open try to close the file. */
    if(loc->holding_file) {
        loc->file->nopen_objs--;
        loc->holding_file = FALSE;
        if(loc->file->nopen_objs <= 0) {
            if(H5F_try_close(loc->file) < 0)
                HGOTO_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, FAIL, "can't close file")
        } /* end if */
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_loc_free() */


/*-------------------------------------------------------------------------
 * Function:	H5O_get_info
 *
 * Purpose:	Retrieve the information for an object
 *
 * Return:	Success:	Non-negative
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *		November 21 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_get_info(H5O_loc_t *oloc, H5O_info_t *oinfo, hid_t dxpl_id)
{
    H5O_t *oh = NULL;                   /* Object header */
    H5O_chunk_t *curr_chunk;	        /* Pointer to current message being operated on */
    H5O_mesg_t *curr_msg;               /* Pointer to current message being operated on */
    unsigned u;                         /* Local index variable */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(H5O_get_info, FAIL)

    /* Check args */
    HDassert(oloc);
    HDassert(oinfo);

    /* Get the object header */
    if(NULL == (oh = H5AC_protect(oloc->file, dxpl_id, H5AC_OHDR, oloc->addr, NULL, NULL, H5AC_READ)))
	HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, FAIL, "unable to load object header")

    /* Retrieve the file's fileno */
    if(H5F_get_fileno(oloc->file, &oinfo->fileno) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_BADVALUE, FAIL, "unable to read fileno")

    /* Set the object's address */
    oinfo->addr = oloc->addr;

    /* Retrieve the type of the object */
    if(H5O_obj_type_real(oh, &oinfo->type) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, FAIL, "unable to determine object type")

    /* Set the object's reference count */
    oinfo->rc = oh->nlink;

    /* Get modification time for object */
    if(oh->version > H5O_VERSION_1) {
        oinfo->atime = oh->atime;
        oinfo->mtime = oh->mtime;
        oinfo->ctime = oh->ctime;
        oinfo->btime = oh->btime;
    } /* end if */
    else {
        /* No information for access & modification fields */
        /* (we stopped updating the "modification time" header message for
         *      raw data changes, so the "modification time" header message
         *      is closest to the 'change time', in POSIX terms - QAK)
         */
        oinfo->atime = 0;
        oinfo->mtime = 0;
        oinfo->btime = 0;

        /* Might be information for modification time */
        if(NULL == H5O_msg_read_real(oloc->file, oh, H5O_MTIME_ID, 0, &oinfo->ctime, dxpl_id)) {
            H5E_clear_stack(NULL);
            if(NULL == H5O_msg_read_real(oloc->file, oh, H5O_MTIME_NEW_ID, 0, &oinfo->ctime, dxpl_id)) {
                H5E_clear_stack(NULL);
                oinfo->ctime = 0;
            } /* end if */
        } /* end if */
    } /* end else */

    /* Set the version for the object header */
    oinfo->hdr.version = oh->version;

    /* Set the number of messages & chunks */
    oinfo->hdr.nmesgs = oh->nmesgs;
    oinfo->hdr.nchunks = oh->nchunks;

    /* Iterate over all the messages, accumulating message size & type information */
    oinfo->num_attrs = 0;
    oinfo->hdr.meta_space = H5O_SIZEOF_HDR_OH(oh) + (H5O_SIZEOF_CHKHDR_OH(oh) * (oh->nchunks - 1));
    oinfo->hdr.mesg_space = 0;
    oinfo->hdr.free_space = 0;
    oinfo->hdr.msg_present = 0;
    oinfo->hdr.msg_shared = 0;
    for(u = 0, curr_msg = &oh->mesg[0]; u < oh->nmesgs; u++, curr_msg++) {
        uint64_t type_flag;             /* Flag for message type */

        /* Check for attribute message */
	if(H5O_ATTR_ID == curr_msg->type->id)
            oinfo->num_attrs++;

        /* Accumulate information, based on the type of message */
	if(H5O_NULL_ID == curr_msg->type->id)
            oinfo->hdr.free_space += H5O_SIZEOF_MSGHDR_OH(oh) + curr_msg->raw_size;
        else if(H5O_CONT_ID == curr_msg->type->id)
            oinfo->hdr.meta_space += H5O_SIZEOF_MSGHDR_OH(oh) + curr_msg->raw_size;
        else {
            oinfo->hdr.meta_space += H5O_SIZEOF_MSGHDR_OH(oh);
            oinfo->hdr.mesg_space += curr_msg->raw_size;
        } /* end else */

        /* Set flag to indicate present of message type */
        type_flag = ((uint64_t)1) << curr_msg->type->id;
        oinfo->hdr.msg_present |= type_flag;

        /* Set flag if the message is shared in some way */
        if(curr_msg->flags & H5O_MSG_FLAG_SHARED)                                   \
            oinfo->hdr.msg_shared |= type_flag;
    } /* end for */

    /* Sanity checking, etc. for # of attributes */
    if(oh->version > H5O_VERSION_1) {
        if(H5F_addr_defined(oh->attr_fheap_addr)) {
            HDassert(oinfo->num_attrs == 0);
            oinfo->num_attrs = oh->nattrs;
        } /* end if */
        else
            HDassert(oh->nattrs == oinfo->num_attrs);
    } /* end if */

    /* Iterate over all the chunks, adding any gaps to the free space */
    oinfo->hdr.hdr_size = 0;
    for(u = 0, curr_chunk = &oh->chunk[0]; u < oh->nchunks; u++, curr_chunk++) {
        /* Accumulate the size of the header on header */
        oinfo->hdr.hdr_size += curr_chunk->size;

        /* If the chunk has a gap, add it to the free space */
        oinfo->hdr.free_space += curr_chunk->gap;
    } /* end for */

    /* Sanity check that all the bytes are accounted for */
    HDassert(oinfo->hdr.hdr_size == (oinfo->hdr.free_space + oinfo->hdr.meta_space + oinfo->hdr.mesg_space + oh->skipped_mesg_size));

done:
    if(oh && H5AC_unprotect(oloc->file, dxpl_id, H5AC_OHDR, oloc->addr, oh, H5AC__NO_FLAGS_SET) < 0)
	HDONE_ERROR(H5E_OHDR, H5E_PROTECT, FAIL, "unable to release object header")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_get_info() */


/*-------------------------------------------------------------------------
 * Function:	H5O_get_create_plist
 *
 * Purpose:	Retrieve the object creation properties for an object
 *
 * Return:	Success:	Non-negative
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *		November 28 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_get_create_plist(const H5O_loc_t *oloc, hid_t dxpl_id, H5P_genplist_t *oc_plist)
{
    H5O_t *oh = NULL;                   /* Object header */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(H5O_get_create_plist, FAIL)

    /* Check args */
    HDassert(oloc);
    HDassert(oc_plist);

    /* Get the object header */
    if(NULL == (oh = H5AC_protect(oloc->file, dxpl_id, H5AC_OHDR, oloc->addr, NULL, NULL, H5AC_READ)))
	HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, FAIL, "unable to load object header")

    /* Set property values, if they were used for the object */
    if(oh->version > H5O_VERSION_1) {
        if(H5P_set(oc_plist, H5O_CRT_ATTR_MAX_COMPACT_NAME, &oh->max_compact) < 0)
            HGOTO_ERROR(H5E_OHDR, H5E_CANTSET, FAIL, "can't set max. # of compact attributes in property list")
        if(H5P_set(oc_plist, H5O_CRT_ATTR_MIN_DENSE_NAME, &oh->min_dense) < 0)
            HGOTO_ERROR(H5E_OHDR, H5E_CANTSET, FAIL, "can't set min. # of dense attributes in property list")
    } /* end if */

done:
    if(oh && H5AC_unprotect(oloc->file, dxpl_id, H5AC_OHDR, oloc->addr, oh, H5AC__NO_FLAGS_SET) < 0)
	HDONE_ERROR(H5E_OHDR, H5E_PROTECT, FAIL, "unable to release object header")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_get_create_plist() */

