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
 * Created:		H5Ocopy.c
 *			Nov  6 2006
 *			Quincey Koziol <koziol@hdfgroup.org>
 *
 * Purpose:		Object copying routines.
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
#include "H5FLprivate.h"	/* Free lists                           */
#include "H5HGprivate.h"        /* Global Heaps                         */
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5Lprivate.h"         /* Links			  	*/
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

static herr_t H5O_copy_obj(H5G_loc_t *src_loc, H5G_loc_t *dst_loc,
    const char *dst_name, hid_t ocpypl_id, hid_t lcpl_id);
static herr_t H5O_copy_obj_by_ref(H5O_loc_t *src_oloc, hid_t dxpl_id,
    H5O_loc_t *dst_oloc, H5G_loc_t *dst_root_loc, H5O_copy_t *cpy_info);

/*********************/
/* Package Variables */
/*********************/

/* Declare a free list to manage the H5O_addr_map_t struct */
H5FL_DEFINE(H5O_addr_map_t);


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/



/*-------------------------------------------------------------------------
 * Function:    H5Ocopy
 *
 * Purpose:     Copy an object (group or dataset) to destination location
 *              within a file or cross files. PLIST_ID is a property list
 *              which is used to pass user options and properties to the
 *              copy. The name, dst_name, must not already be taken by some 
 *              other object in the destination group.
 *
 *              H5Ocopy() will fail if the name of the destination object
 *                  exists in the destination group.  For example,  
 *                  H5Ocopy(fid_src, "/dset", fid_dst, "/dset", ...)
 *                  will fail if "/dset" exists in the destination file
 *
 *              OPTIONS THAT HAVE BEEN IMPLEMENTED.
 *                  H5O_COPY_SHALLOW_HIERARCHY_FLAG
 *                      If this flag is specified, only immediate members of
 *                      the group are copied. Otherwise (default), it will
 *                      recursively copy all objects below the group
 *                  H5O_COPY_EXPAND_SOFT_LINK_FLAG
 *                      If this flag is specified, it will copy the objects 
 *                      pointed by the soft links. Otherwise (default), it 
 *                      will copy the soft link as they are
 *                  H5O_COPY_WITHOUT_ATTR_FLAG
 *                      If this flag is specified, it will copy object without 
 *                      copying attributes. Otherwise (default), it will
 *                      copy object along with all its attributes
 *                  H5O_COPY_EXPAND_REFERENCE_FLAG
 *                      1) Copy object between two different files:
 *                          When this flag is specified, it will copy objects that 
 *                          are pointed by the references and update the values of
 *                          references in the destination file.  Otherwise (default)
 *                          the values of references in the destination will set to
 *                          zero
 *                          The current implementation does not handle references 
 *                          inside of other datatype structure. For example, if
 *                          a member of compound datatype is reference, H5Ocopy()
 *                          will copy that field as it is. It will not set the
 *                          value to zero as default is used nor copy the object
 *                          pointed by that field the flag is set
 *                      2) Copy object within the same file:
 *                          This flag does not have any effect to the H5Ocopy().
 *                          Datasets or attributes of references are copied as they
 *                          are, i.e. values of references of the destination object
 *                          are the same as the values of the source object
 *  
 *              OPTIONS THAT MAY APPLY TO COPY IN THE FUTURE.
 *                  H5O_COPY_EXPAND_EXT_LINK_FLAG
 *                      If this flag is specified, it will expand the external links
 *                      into new objects, Otherwise (default), it will keep external 
 *                      links as they are (default)
 *
 *              PROPERTIES THAT MAY APPLY TO COPY IN FUTURE
 *                  Change data layout such as chunk size
 *                  Add filter such as data compression.
 *                  Add an attribute to the copied object(s) that say the  date/time
 *                      for the copy or other information about the source file.
 *
 *              The intermediate group creation property should be passed in
 *              using the lcpl instead of the ocpypl.
 *
 * Usage:      H5Ocopy(src_loc_id, src_name, dst_loc_id, dst_name, ocpypl_id, lcpl_id)
 *             hid_t src_loc_id         IN: Source file or group identifier.
 *             const char *src_name     IN: Name of the source object to be copied
 *             hid_t dst_loc_id         IN: Destination file or group identifier
 *             const char *dst_name     IN: Name of the destination object
 *             hid_t ocpypl_id          IN: Properties which apply to the copy
 *             hid_t lcpl_id            IN: Properties which apply to the new hard link
 *
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Peter Cao
 *              June 4, 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Ocopy(hid_t src_loc_id, const char *src_name, hid_t dst_loc_id,
        const char *dst_name, hid_t ocpypl_id, hid_t lcpl_id)
{
    H5G_loc_t	loc;                    /* Source group group location */
    H5G_loc_t	src_loc;                /* Source object group location */
    H5G_loc_t	dst_loc;                /* Destination group location */

    /* for opening the destination object */
    H5G_name_t  src_path;               /* Opened source object hier. path */
    H5O_loc_t   src_oloc;               /* Opened source object object location */
    hbool_t     loc_found = FALSE;      /* Location at 'name' found */
    hbool_t     obj_open = FALSE;       /* Entry at 'name' found */

    herr_t      ret_value = SUCCEED;        /* Return value */

    FUNC_ENTER_API(H5Ocopy, FAIL)
    H5TRACE6("e","isisii",src_loc_id,src_name,dst_loc_id,dst_name,ocpypl_id,
             lcpl_id);

    /* Check arguments */
    if(H5G_loc(src_loc_id, &loc) < 0)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
    if(H5G_loc(dst_loc_id, &dst_loc) < 0)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
    if(!src_name || !*src_name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no source name specified")
    if(!dst_name || !*dst_name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no destination name specified")

    /* check if destination name already exists */
    {
        H5G_name_t  tmp_path;
        H5O_loc_t   tmp_oloc;
        H5G_loc_t   tmp_loc;

        /* Set up group location */
        tmp_loc.oloc = &tmp_oloc;
        tmp_loc.path = &tmp_path;
        H5G_loc_reset(&tmp_loc);

        /* Check if object already exists in destination */
        if(H5G_loc_find(&dst_loc, dst_name, &tmp_loc, H5P_DEFAULT, H5AC_dxpl_id) >= 0) {
            H5G_name_free(&tmp_path);
            HGOTO_ERROR(H5E_SYM, H5E_EXISTS, FAIL, "destination object already exists")
        } /* end if */
    }

    /* Set up opened group location to fill in */
    src_loc.oloc = &src_oloc;
    src_loc.path = &src_path;
    H5G_loc_reset(&src_loc);

    /* Find the source object to copy */
    if(H5G_loc_find(&loc, src_name, &src_loc/*out*/, H5P_DEFAULT, H5AC_dxpl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "source object not found")
    loc_found = TRUE;

    /* Open source object's object header */
    if(H5O_open(&src_oloc) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTOPENOBJ, FAIL, "unable to open object")
    obj_open = TRUE;

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

    /* Do the actual copying of the object */
    if(H5O_copy_obj(&src_loc, &dst_loc, dst_name, ocpypl_id, lcpl_id) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTCOPY, FAIL, "unable to copy object")

done:
    if(loc_found) {
        if(H5G_loc_free(&src_loc) < 0)
            HDONE_ERROR(H5E_SYM, H5E_CANTRELEASE, FAIL, "can't free location")
    }
    if(obj_open)
        H5O_close(&src_oloc);

    FUNC_LEAVE_API(ret_value)
} /* end H5Ocopy() */


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
    if(NULL == (ocpy_plist = H5I_object(ocpypl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list")

    /* Retrieve the copy parameters */
    if(H5P_get(ocpy_plist, H5O_CPY_OPTION_NAME, &cpy_option) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get object copy flag")

    /* Set up copied object location to fill in */
    new_loc.oloc = &new_oloc;
    new_loc.path = &new_path;
    H5G_loc_reset(&new_loc);
    new_oloc.file = dst_loc->oloc->file;

    /* Copy the object from the source file to the destination file */
    if(H5O_copy_header(src_loc->oloc, &new_oloc, dxpl_id, cpy_option) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTCOPY, FAIL, "unable to copy object")

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
    if((ret_value = H5O_copy_header_map(src_oloc, dst_oloc, dxpl_id, cpy_info, FALSE)) < 0)
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
        sprintf(tmp_obj_name, "~obj_pointed_by_%llu", (unsigned long_long)dst_oloc->addr);

        /* Create a link to the newly copied object */
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
            if(H5O_copy_obj_by_ref(&src_oloc, dxpl_id, &dst_oloc, &dst_root_loc, cpy_info) < 0)
                HGOTO_ERROR(H5E_OHDR, H5E_CANTCOPY, FAIL, "unable to copy object")

            /* Set the object reference info for the destination file */
            p = (uint8_t *)(&dst_ref[i]);
            H5F_addr_encode(dst_oloc.file, &p, dst_oloc.addr);
	} /* end for */
    }  /* end if */
    /* Copy region references */
    else if(H5R_DATASET_REGION == ref_type) {
        hdset_reg_ref_t *src_ref = (hdset_reg_ref_t *)_src_ref;
        hdset_reg_ref_t *dst_ref = (hdset_reg_ref_t *)_dst_ref;
        uint8_t *buf;           /* Buffer to store serialized selection in */
        H5HG_t hobjid;          /* Heap object ID */
        size_t buf_size;        /* Length of object in heap */
  
        /* Making equivalent references in the destination file */
        for(i = 0; i < ref_count; i++) {
            /* Get the heap ID for the dataset region */
            p = (uint8_t *)(&src_ref[i]);
            H5F_addr_decode(src_oloc.file, (const uint8_t **)&p, &(hobjid.addr));
            INT32DECODE(p, hobjid.idx);

            /* Get the dataset region from the heap (allocate inside routine) */
            if((buf = H5HG_read(src_oloc.file, dxpl_id, &hobjid, NULL, &buf_size)) == NULL)
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

