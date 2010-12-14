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
 * Created:		H5Ocompare.c
 *			Oct 12 2010
 *			Neil Fortner <nfortne2@hdfgroup.org>
 *
 * Purpose:		Object comparison routines.
 *
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/

#define H5O_PACKAGE		/*suppress error about including H5Opkg	  */

/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Opkg.h"             /* Object headers			*/
#include "H5Pprivate.h"         /* Property lists                       */


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

static herr_t H5O_compare_obj(const H5O_loc_t *oloc1, const H5O_loc_t *oloc2,
    hid_t ocmppl_id);

/*********************/
/* Package Variables */
/*********************/


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/



/*-------------------------------------------------------------------------
 * Function:    H5Ocompare
 *
 * Purpose:     fnord
 *
 * Usage:       fnord
 *
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Neil Fortner
 *              October 12, 2010
 *
 *-------------------------------------------------------------------------
 */
htri_t
H5Ocompare(hid_t loc1_id, const char *name1, hid_t loc2_id, const char *name2,
    hid_t ocmppl_id)
{
    H5G_loc_t	base_loc1;              /* Base location for object 1 */
    H5G_loc_t	base_loc2;              /* Base location for object 2 */
    H5G_loc_t	loc1;                   /* Object location for object 1 */
    H5G_loc_t   loc2;                   /* Object location for object 2 */

    /* for opening the objects */
    H5G_name_t  path1;                  /* Opened object 1 hier. path */
    H5G_name_t  path2;                  /* Opened object 2 hier. path */
    H5O_loc_t   oloc1;                  /* Opened object 1 object location */
    H5O_loc_t   oloc2;                  /* Opened object 2 object location */
    hbool_t     loc1_found = FALSE;     /* Location at 'name1' found */
    hbool_t     loc2_found = FALSE;     /* Location at 'name2' found */
    hbool_t     obj1_open = FALSE;      /* Entry at 'name1' found */
    hbool_t     obj2_open = FALSE;      /* Entry at 'name2' found */

    htri_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_API(H5Ocompare, FAIL)

    /* Check arguments */
    if(H5G_loc(loc1_id, &base_loc1) < 0)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "loc1_id not a location")
    if(H5G_loc(loc2_id, &base_loc2) < 0)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "loc2_id not a location")
    if(!name1 || !*name2)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name specified for object 1")
    if(!name2 || !*name2)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name specified for object 2")

    /* Set up opened group locations to fill in */
    loc1.oloc = &oloc1;
    loc1.path = &path1;
    H5G_loc_reset(&loc1);
    loc2.oloc = &oloc2;
    loc2.path = &path2;
    H5G_loc_reset(&loc2);

    /* Find the objects to compare */
    if(H5G_loc_find(&base_loc1, name1, &loc1/*out*/, H5P_DEFAULT, H5AC_dxpl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "object 1 not found")
    loc1_found = TRUE;
    if(H5G_loc_find(&base_loc2, name2, &loc2/*out*/, H5P_DEFAULT, H5AC_dxpl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "object 2 not found")
    loc2_found = TRUE;

    /* Open the objects' object headers */
    if(H5O_open(&oloc1) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTOPENOBJ, FAIL, "unable to open object 1")
    obj1_open = TRUE;
    if(H5O_open(&oloc2) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTOPENOBJ, FAIL, "unable to open object 2")
    obj2_open = TRUE;
#if 0
    /* Get correct property lists */
    if(H5P_DEFAULT == lcpl_id) {
        if((lcpl_id = H5L_get_default_lcpl()) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTINIT, FAIL, "unable to get default lcpl")
    } /* end if */
    else
        if(TRUE != H5P_isa_class(lcpl_id, H5P_LINK_CREATE))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not link creation property list")

    /* Get object copy property list */
    if(H5P_DEFAULT == ocpypl_id)
        ocpypl_id = H5P_OBJECT_COPY_DEFAULT;
    else
        if(TRUE != H5P_isa_class(ocpypl_id, H5P_OBJECT_COPY))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not object copy property list")
#endif
    /* Do the actual copying of the object */
    if((ret_value = H5O_compare_obj(&oloc1, &oloc2, ocmppl_id)) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTCOMPARE, FAIL, "unable to compare object")

done:
    if(loc1_found && H5G_loc_free(&loc1) < 0)
        HDONE_ERROR(H5E_SYM, H5E_CANTRELEASE, FAIL, "can't free location 1")
    if(loc2_found && H5G_loc_free(&loc2) < 0)
        HDONE_ERROR(H5E_SYM, H5E_CANTRELEASE, FAIL, "can't free location 2")
    if(obj1_open)
        H5O_close(&oloc1);
    if(obj2_open)
        H5O_close(&oloc2);

    FUNC_LEAVE_API(ret_value)
} /* end H5Ocompare() */


/*-------------------------------------------------------------------------
 * Function:    H5O_compare_obj
 *
 * Purpose:     fnord
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Neil Fortner
 *              Novermeber 23, 2010
 *
 *-------------------------------------------------------------------------
 */
static htri_t
H5O_compare_obj(const H5O_loc_t *oloc1, const H5O_loc_t *oloc2, hid_t ocmppl_id)
{
    H5O_t                  *oh1 = NULL;             /* Object header for object 1 */
    H5O_t                  *oh2 = NULL;             /* Object header for object 2 */
    const H5O_obj_class_t  *obj1_class = NULL;      /* Type of object 1 */
    const H5O_obj_class_t  *obj2_class = NULL;      /* Type of object 2 */
    hid_t                  dxpl1_id = H5AC_dxpl_id;
    hid_t                  dxpl2_id = -1;
    H5P_genplist_t         *dxpl = NULL;
    haddr_t                prev_tag1 = HADDR_UNDEF;
    haddr_t                prev_tag2 = HADDR_UNDEF;
    htri_t                 ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT(H5O_compare_obj)

    HDassert(oloc1);
    HDassert(oloc1->file);
    HDassert(H5F_addr_defined(oloc1->addr));
    HDassert(oloc2);
    HDassert(oloc2->file);
    HDassert(H5F_addr_defined(oloc2->addr));

    /* Copy dxpl1 to dxpl2 */
    if(NULL == (dxpl = (H5P_genplist_t *)H5I_object_verify(dxpl1_id, H5I_GENPROP_LST)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list")
    if((dxpl2_id = H5P_copy_plist((H5P_genplist_t *)dxpl, FALSE)) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTCOPY, FAIL, "can't copy property list");

    /* Set up metadata tagging */
    if(H5AC_tag(dxpl1_id, oloc1->addr, &prev_tag1) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTTAG, FAIL, "unable to apply metadata tag")
    if(H5AC_tag(dxpl2_id, oloc2->addr, &prev_tag2) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTTAG, FAIL, "unable to apply metadata tag")

    /* Get object headers */
    /*FIXME fnord Set up tagging here? */
    if(NULL == (oh1 = H5O_protect(oloc1, dxpl1_id, H5AC_READ)))
        HGOTO_ERROR(H5E_OHDR, H5E_CANTPROTECT, FAIL, "unable to load object header")
    if(NULL == (oh2 = H5O_protect(oloc2, dxpl2_id, H5AC_READ)))
        HGOTO_ERROR(H5E_OHDR, H5E_CANTPROTECT, FAIL, "unable to load object header")

    /* Get pointer to object class for objects */
    if(NULL == (obj1_class = H5O_obj_class_real(oh1)))
        HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, FAIL, "unable to determine object type")
    if(NULL == (obj2_class = H5O_obj_class_real(oh1)))
        HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, FAIL, "unable to determine object type")
    if(obj1_class->type != obj2_class->type)
        HGOTO_DONE(TRUE)

    /* Do the comparison */
    HDassert(obj1_class->compare);
    if((ret_value = obj1_class->compare(oloc1->file, oloc2->file, oh1, oh2,
            oloc1->addr, oloc2->addr, dxpl1_id, dxpl2_id, NULL)) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTCOMPARE, FAIL, "unable to compare objects")

done:
    /* Release pointer to object headers and their derived objects */
    if(oh1 && H5O_unprotect(oloc1, dxpl1_id, oh1, H5AC__NO_FLAGS_SET) < 0)
        HDONE_ERROR(H5E_OHDR, H5E_CANTUNPROTECT, FAIL, "unable to release object header")
    if(oh2 && H5O_unprotect(oloc2, dxpl2_id, oh2, H5AC__NO_FLAGS_SET) < 0)
        HDONE_ERROR(H5E_OHDR, H5E_CANTUNPROTECT, FAIL, "unable to release object header")

    if(H5AC_tag(dxpl1_id, prev_tag1, NULL) < 0)
        HDONE_ERROR(H5E_CACHE, H5E_CANTTAG, FAIL, "unable to apply metadata tag")
    if(H5AC_tag(dxpl2_id, prev_tag2, NULL) < 0)
        HDONE_ERROR(H5E_CACHE, H5E_CANTTAG, FAIL, "unable to apply metadata tag")

    if(dxpl2_id >= 0 && H5I_dec_ref(dxpl2_id) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTDEC, FAIL, "can't decrement ID ref count")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_compare_obj() */

#if 0

/*-------------------------------------------------------------------------
 * Function:    H5O_copy_header_map
 *
 * Purpose:     Copy header object from one location to another, detecting
 *              already mapped objects, etc.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              November 1, 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_copy_header_map(const H5O_loc_t *oloc_src, H5O_loc_t *oloc_dst /*out */,
    hid_t dxpl_id, H5O_copy_t *cpy_info, hbool_t inc_depth,
    H5O_type_t *obj_type, void **udata)
{
    H5O_addr_map_t      *addr_map = NULL;       /* Address mapping of object copied */
    H5_obj_t            src_obj_pos;            /* Position of source object */
    hbool_t             inc_link;               /* Whether to increment the link count for the object */
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5O_copy_header_map, FAIL)

    /* Sanity check */
    HDassert(oloc_src);
    HDassert(oloc_src->file);
    HDassert(oloc_dst);
    HDassert(oloc_dst->file);
    HDassert(cpy_info);

    /* Create object "position" struct */
    H5F_GET_FILENO(oloc_src->file, src_obj_pos.fileno);
    src_obj_pos.addr = oloc_src->addr;

    /* Search for the object in the skip list of copied objects */
    addr_map = (H5O_addr_map_t *)H5SL_search(cpy_info->map_list,
            &src_obj_pos);

    /* Check if address is already in list of objects copied */
    if(addr_map == NULL) {
        /* Copy object for the first time */

        /* Check for incrementing the depth of copy */
        /* (Can't do this for all copies, since committed datatypes should always be copied) */
        if(inc_depth)
            cpy_info->curr_depth++;

        /* Copy object referred to */
        if(H5O_copy_header_real(oloc_src, oloc_dst, dxpl_id, cpy_info, obj_type,
                udata) < 0)
            HGOTO_ERROR(H5E_OHDR, H5E_CANTCOPY, FAIL, "unable to copy object")

        /* Check for incrementing the depth of copy */
        if(inc_depth)
            cpy_info->curr_depth--;

        /* When an object is copied for the first time, increment it's link */
        inc_link = TRUE;

        /* indicate that a new object is created */
        ret_value++;
    } /* end if */
    else {
        /* Object has already been copied, set its address in destination file */
        oloc_dst->addr = addr_map->dst_addr;

        /* Return saved obj_type and udata, if requested */
        if(obj_type) {
            HDassert(udata);
            *obj_type = addr_map->obj_class->type;
            *udata = addr_map->udata;
        } /* end if */

        /* If the object is locked currently (because we are copying a group
         * hierarchy and this is a link to a group higher in the hierarchy),
         * increment it's deferred reference count instead of incrementing the
         * reference count now.
         */
        if(addr_map->is_locked) {
            addr_map->inc_ref_count++;
            inc_link = FALSE;
        } /* end if */
        else
            inc_link = TRUE;
    } /* end else */

    /* Increment destination object's link count, if allowed */
    if(inc_link)
        if(H5O_link(oloc_dst, 1, dxpl_id) < 0)
            HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, FAIL, "unable to increment object link count")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_copy_header_map() */


/*--------------------------------------------------------------------------
 NAME
    H5O_copy_free_addrmap_cb
 PURPOSE
    Internal routine to free address maps from the skip list for copying objects
 USAGE
    herr_t H5O_copy_free_addrmap_cb(item, key, op_data)
        void *item;             IN/OUT: Pointer to addr
        void *key;              IN/OUT: (unused)
        void *op_data;          IN: (unused)
 RETURNS
    Returns zero on success, negative on failure.
 DESCRIPTION
        Releases the memory for the address.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static herr_t
H5O_copy_free_addrmap_cb(void *_item, void UNUSED *key, void UNUSED *op_data)
{
    H5O_addr_map_t *item = (H5O_addr_map_t *)_item;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_copy_free_addrmap_cb)

    HDassert(item);

    /* Release user data for particular type of object */
    if(item->udata) {
        HDassert(item->obj_class);
        HDassert(item->obj_class->free_copy_file_udata);
        (item->obj_class->free_copy_file_udata)(item->udata);
    } /* end if */

    /* Release the item */
    item = H5FL_FREE(H5O_addr_map_t, item);

    FUNC_LEAVE_NOAPI(0)
}   /* H5O_copy_free_addrmap_cb() */


/*-------------------------------------------------------------------------
 * Function:    H5O_copy_header
 *
 * Purpose:     copy header object from one location to another.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Peter Cao
 *              May 30, 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_copy_header(const H5O_loc_t *oloc_src, H5O_loc_t *oloc_dst /*out */,
    hid_t dxpl_id, unsigned cpy_option)
{
    H5O_copy_t  cpy_info;               /* Information for copying object */
    herr_t      ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT(H5O_copy_header)

    HDassert(oloc_src);
    HDassert(oloc_src->file);
    HDassert(H5F_addr_defined(oloc_src->addr));
    HDassert(oloc_dst->file);

    /* Convert copy flags into copy struct */
    HDmemset(&cpy_info, 0, sizeof(H5O_copy_t));
    if((cpy_option & H5O_COPY_SHALLOW_HIERARCHY_FLAG) > 0) {
        cpy_info.copy_shallow = TRUE;
        cpy_info.max_depth = 1;
    } /* end if */
    else
        cpy_info.max_depth = -1;        /* Current default is for full, recursive hier. copy */
    cpy_info.curr_depth = 0;
    if((cpy_option & H5O_COPY_EXPAND_SOFT_LINK_FLAG) > 0)
        cpy_info.expand_soft_link = TRUE;
    if((cpy_option & H5O_COPY_EXPAND_EXT_LINK_FLAG) > 0)
        cpy_info.expand_ext_link = TRUE;
    if((cpy_option & H5O_COPY_EXPAND_REFERENCE_FLAG) > 0)
        cpy_info.expand_ref = TRUE;
    if((cpy_option & H5O_COPY_WITHOUT_ATTR_FLAG) > 0)
        cpy_info.copy_without_attr = TRUE;
    if((cpy_option & H5O_COPY_PRESERVE_NULL_FLAG) > 0)
        cpy_info.preserve_null = TRUE;

    /* Create a skip list to keep track of which objects are copied */
    if((cpy_info.map_list = H5SL_create(H5SL_TYPE_OBJ)) == NULL)
        HGOTO_ERROR(H5E_SLIST, H5E_CANTCREATE, FAIL, "cannot make skip list")

    /* copy the object from the source file to the destination file */
    if(H5O_copy_header_real(oloc_src, oloc_dst, dxpl_id, &cpy_info, NULL, NULL)
            < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTCOPY, FAIL, "unable to copy object")

done:
    if(cpy_info.map_list)
        H5SL_destroy(cpy_info.map_list, H5O_copy_free_addrmap_cb, NULL);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_copy_header() */


/*-------------------------------------------------------------------------
 * Function:    H5O_copy_obj
 *
 * Purpose:     Copy an object to destination location
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Peter Cao
 *              June 4, 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_copy_obj(H5G_loc_t *src_loc, H5G_loc_t *dst_loc, const char *dst_name,
    hid_t ocpypl_id, hid_t lcpl_id)
{
    H5P_genplist_t  *ocpy_plist=NULL;           /* Object copy property list created */
    hid_t           dxpl_id=H5AC_dxpl_id;
    H5G_name_t      new_path;                   /* Copied object group hier. path */
    H5O_loc_t       new_oloc;                   /* Copied object object location */
    H5G_loc_t       new_loc;                    /* Group location of object copied */
    H5F_t           *cached_dst_file;           /* Cached destination file */
    hbool_t         entry_inserted=FALSE;       /* Flag to indicate that the new entry was inserted into a group */
    unsigned        cpy_option = 0;             /* Copy options */
    herr_t          ret_value = SUCCEED;        /* Return value */

    FUNC_ENTER_NOAPI(H5O_copy_obj, FAIL)

    HDassert(src_loc);
    HDassert(src_loc->oloc->file);
    HDassert(dst_loc);
    HDassert(dst_loc->oloc->file);
    HDassert(dst_name);

    /* Get the copy property list */
    if(NULL == (ocpy_plist = (H5P_genplist_t *)H5I_object(ocpypl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list")

    /* Retrieve the copy parameters */
    if(H5P_get(ocpy_plist, H5O_CPY_OPTION_NAME, &cpy_option) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get object copy flag")

    /* Set up copied object location to fill in */
    new_loc.oloc = &new_oloc;
    new_loc.path = &new_path;
    H5G_loc_reset(&new_loc);
    new_oloc.file = dst_loc->oloc->file;

    /* Make a copy of the destination file, in case the original is changed by
     * H5O_copy_header.  If and when oloc's point to the shared file struct,
     * this will no longer be necessary, so this code can be removed. */
    cached_dst_file = dst_loc->oloc->file;

    /* Copy the object from the source file to the destination file */
    if(H5O_copy_header(src_loc->oloc, &new_oloc, dxpl_id, cpy_option) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTCOPY, FAIL, "unable to copy object")

    /* Patch dst_loc.  Again, this can be removed once oloc's point to shared
     * file structs. */
    dst_loc->oloc->file = cached_dst_file;

    /* Insert the new object in the destination file's group */
    if(H5L_link(dst_loc, dst_name, &new_loc, lcpl_id, H5P_DEFAULT, dxpl_id) < 0)
	HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "unable to insert link")
    entry_inserted = TRUE;

done:
    /* Free the ID to name buffers */
    if(entry_inserted)
        H5G_loc_free(&new_loc);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_copy_obj() */


/*-------------------------------------------------------------------------
 * Function:    H5O_copy_obj_by_ref
 *
 * Purpose:     Copy the object pointed by _src_ref.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Peter Cao
 *              Aug 7 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_copy_obj_by_ref(H5O_loc_t *src_oloc, hid_t dxpl_id, H5O_loc_t *dst_oloc,
    H5G_loc_t *dst_root_loc, H5O_copy_t *cpy_info)
{
    herr_t  ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5O_copy_obj_by_ref, FAIL)

    HDassert(src_oloc);
    HDassert(dst_oloc);

    /* Perform the copy, or look up existing copy */
    if((ret_value = H5O_copy_header_map(src_oloc, dst_oloc, dxpl_id, cpy_info,
            FALSE, NULL, NULL)) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTCOPY, FAIL, "unable to copy object")

    /* Check if a new valid object is copied to the destination */
    if(H5F_addr_defined(dst_oloc->addr) && (ret_value > SUCCEED)) {
        char    tmp_obj_name[80];
        H5G_name_t      new_path;
        H5O_loc_t       new_oloc;
        H5G_loc_t       new_loc;

        /* Set up group location for new object */
        new_loc.oloc = &new_oloc;
        new_loc.path = &new_path;
        H5G_loc_reset(&new_loc);
        new_oloc.file = dst_oloc->file;
        new_oloc.addr = dst_oloc->addr;

        /* Pick a default name for the new object */
        sprintf(tmp_obj_name, "~obj_pointed_by_%llu", (unsigned long long)dst_oloc->addr);

        /* Create a link to the newly copied object */
        /* Note: since H5O_copy_header_map actually copied the target object, it
         * must exist either in cache or on disk, therefore it is is safe to not
         * pass the obj_type and udata fields returned by H5O_copy_header_map.
         * This could be changed in the future to slightly improve performance
         * --NAF */
        if(H5L_link(dst_root_loc, tmp_obj_name, &new_loc, H5P_DEFAULT, H5P_DEFAULT, dxpl_id) < 0)
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "unable to insert link")

        H5G_loc_free(&new_loc);
    } /* if (H5F_addr_defined(dst_oloc.addr)) */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_copy_obj_by_ref() */


/*-------------------------------------------------------------------------
 * Function:	H5O_copy_expand_ref
 *
 * Purpose:	Copy the object pointed by _src_ref.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:  Peter Cao
 *		Aug 7 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_copy_expand_ref(H5F_t *file_src, void *_src_ref, hid_t dxpl_id,
    H5F_t *file_dst, void *_dst_ref, size_t ref_count, H5R_type_t ref_type,
    H5O_copy_t *cpy_info)
{
    H5O_loc_t 	dst_oloc;         	/* Copied object object location */
    H5O_loc_t	src_oloc;          	/* Temporary object location for source object */
    H5G_loc_t   dst_root_loc;           /* The location of root group of the destination file */
    uint8_t     *p;                     /* Pointer to OID to store */
    size_t      i;                      /* Local index variable */
    herr_t	ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5O_copy_expand_ref, FAIL)

    /* Sanity checks */
    HDassert(file_src);
    HDassert(_src_ref);
    HDassert(file_dst);
    HDassert(_dst_ref);
    HDassert(ref_count);
    HDassert(cpy_info);

    /* Initialize object locations */
    H5O_loc_reset(&src_oloc);
    H5O_loc_reset(&dst_oloc);
    src_oloc.file = file_src;
    dst_oloc.file = file_dst;

    /* Set up the root group in the destination file */
    if(NULL == (dst_root_loc.oloc = H5G_oloc(H5G_rootof(file_dst))))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "unable to get object location for root group")
    if(NULL == (dst_root_loc.path = H5G_nameof(H5G_rootof(file_dst))))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "unable to get path for root group")

    /* Copy object references */
    if(H5R_OBJECT == ref_type) {
        hobj_ref_t *src_ref = (hobj_ref_t *)_src_ref;
        hobj_ref_t *dst_ref = (hobj_ref_t *)_dst_ref;

        /* Making equivalent references in the destination file */
        for(i = 0; i < ref_count; i++) {
            /* Set up for the object copy for the reference */
            p = (uint8_t *)(&src_ref[i]);
            H5F_addr_decode(src_oloc.file, (const uint8_t **)&p, &(src_oloc.addr));
            dst_oloc.addr = HADDR_UNDEF;

            /* Attempt to copy object from source to destination file */
            if(src_oloc.addr != (haddr_t)0) {
                if(H5O_copy_obj_by_ref(&src_oloc, dxpl_id, &dst_oloc, &dst_root_loc, cpy_info) < 0)
                    HGOTO_ERROR(H5E_OHDR, H5E_CANTCOPY, FAIL, "unable to copy object")
            } /* end if */
            else
                /* Set parameters so the reference is written as all 0's */
                HDmemset(&dst_oloc.addr, 0, sizeof(dst_oloc.addr));

            /* Set the object reference info for the destination file */
            p = (uint8_t *)(&dst_ref[i]);
            H5F_addr_encode(dst_oloc.file, &p, dst_oloc.addr);
	} /* end for */
    }  /* end if */
    /* Copy region references */
    else if(H5R_DATASET_REGION == ref_type) {
        hdset_reg_ref_t *src_ref = (hdset_reg_ref_t *)_src_ref;
        hdset_reg_ref_t *dst_ref = (hdset_reg_ref_t *)_dst_ref;
        uint8_t *buf = NULL;    /* Buffer to store serialized selection in */
        H5HG_t hobjid;          /* Heap object ID */
        size_t buf_size;        /* Length of object in heap */

        /* Making equivalent references in the destination file */
        for(i = 0; i < ref_count; i++) {
            /* Get the heap ID for the dataset region */
            p = (uint8_t *)(&src_ref[i]);
            H5F_addr_decode(src_oloc.file, (const uint8_t **)&p, &(hobjid.addr));
            INT32DECODE(p, hobjid.idx);

            if(hobjid.addr != (haddr_t)0) {
                /* Get the dataset region from the heap (allocate inside routine) */
                if((buf = (uint8_t *)H5HG_read(src_oloc.file, dxpl_id, &hobjid, NULL, &buf_size)) == NULL)
                    HGOTO_ERROR(H5E_REFERENCE, H5E_READERROR, FAIL, "Unable to read dataset region information")

                /* Get the object oid for the dataset */
                p = (uint8_t *)buf;
                H5F_addr_decode(src_oloc.file, (const uint8_t **)&p, &(src_oloc.addr));
                dst_oloc.addr = HADDR_UNDEF;

                /* copy the object pointed by the ref to the destination */
                if(H5O_copy_obj_by_ref(&src_oloc, dxpl_id, &dst_oloc, &dst_root_loc, cpy_info) < 0) {
                    H5MM_xfree(buf);
                    HGOTO_ERROR(H5E_OHDR, H5E_CANTCOPY, FAIL, "unable to copy object")
                } /* end if */

                /* Serialize object ID */
                p = (uint8_t *)buf;
                H5F_addr_encode(dst_oloc.file, &p, dst_oloc.addr);

                /* Save the serialized buffer to the destination */
                if(H5HG_insert(dst_oloc.file, dxpl_id, buf_size, buf, &hobjid) < 0) {
                    H5MM_xfree(buf);
                    HGOTO_ERROR(H5E_OHDR, H5E_CANTCOPY, FAIL, "Unable to write dataset region information")
                } /* end if */
            } /* end if */
            else
                /* Set parameters so the reference is written as all 0's */
                HDmemset(&hobjid, 0, sizeof(hobjid));

            /* Set the dataset region reference info for the destination file */
            p = (uint8_t *)(&dst_ref[i]);
            H5F_addr_encode(dst_oloc.file, &p, hobjid.addr);
            INT32ENCODE(p, hobjid.idx);

            /* Free the buffer allocated in H5HG_read() */
            H5MM_xfree(buf);
        } /* end for */
    } /* end if */
    else
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid reference type")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_copy_expand_ref() */
#endif

