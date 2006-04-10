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
 * Created:		H5Gtraverse.c
 *			Sep 13 2005
 *			Quincey Koziol <koziol@ncsa.uiuc.edu>
 *
 * Purpose:		Functions for traversing group hierarchy
 *
 *-------------------------------------------------------------------------
 */
#define H5F_PACKAGE		/*suppress error about including H5Fpkg	  */
#define H5G_PACKAGE		/*suppress error about including H5Gpkg	  */


/* Packages needed by this file... */
#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Fpkg.h"		/* File access				*/
#include "H5Gpkg.h"		/* Groups		  		*/
#include "H5HLprivate.h"	/* Local Heaps				*/
#include "H5MMprivate.h"	/* Memory management			*/

/* Private typedefs */

/* User data for path traversal routine */
typedef struct {
    H5G_loc_t *obj_loc;         /* Object location */
} H5G_trav_ud1_t;

/* Private macros */

/* Local variables */
static char *H5G_comp_g = NULL;                 /*component buffer      */
static size_t H5G_comp_alloc_g = 0;             /*sizeof component buffer */

/* PRIVATE PROTOTYPES */
static herr_t H5G_traverse_link_cb(H5G_loc_t *grp_loc/*in*/, const char *name,
    const H5O_link_t *lnk, H5G_loc_t *obj_loc, void *_udata/*in,out*/);
static herr_t H5G_traverse_slink(H5G_loc_t *grp_loc/*in,out*/, H5O_link_t *lnk,
    H5G_loc_t *obj_loc/*in,out*/, int *nlinks/*in,out*/, hid_t dxpl_id);
static herr_t H5G_traverse_mount(H5G_loc_t *loc/*in,out*/);
static herr_t H5G_traverse_real(const H5G_loc_t *loc, const char *name,
    unsigned target, int *nlinks, H5G_traverse_t op, void *op_data,
    hid_t dxpl_id);


/*-------------------------------------------------------------------------
 * Function:	H5G_traverse_term_interface
 *
 * Purpose:	Terminates part  of the H5G interface - free the global
 *              component buffer.
 *
 * Return:	Success:	Non-negative.
 *
 * 		Failure:	Negative.
 *
 * Programmer:	Quincey Koziol
 *		Monday, September	26, 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_traverse_term_interface(void)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5G_traverse_term_interface)

    /* Free the global component buffer */
    H5G_comp_g = H5MM_xfree(H5G_comp_g);
    H5G_comp_alloc_g = 0;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5G_traverse_term_interface() */


/*-------------------------------------------------------------------------
 * Function:	H5G_traverse_link_cb
 *
 * Purpose:	Callback for link traversal.  This routine sets the
 *              correct information for the object location.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, September 13, 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5G_traverse_link_cb(H5G_loc_t UNUSED *grp_loc, const char UNUSED *name, const H5O_link_t UNUSED *lnk,
    H5G_loc_t *obj_loc, void *_udata/*in,out*/)
{
    H5G_trav_ud1_t *udata = (H5G_trav_ud1_t *)_udata;   /* User data passed in */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5G_traverse_link_cb)

    /* Check for dangling soft link */
    if(obj_loc == NULL)
        HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "component not found")

    /* Copy new location information for resolved object */
    H5O_loc_copy(udata->obj_loc->oloc, obj_loc->oloc, H5_COPY_DEEP);

done:
    /* Release the group location for the object */
    /* (Group traversal callbacks are responsible for either taking ownership
     *  of the group location for the object, or freeing it. - QAK)
     */
    if(obj_loc)
        H5G_loc_free(obj_loc);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_traverse_link_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5G_traverse_slink
 *
 * Purpose:	Traverses symbolic link.  The link head appears in the group
 *		whose entry is GRP_LOC and the link tail entry is OBJ_LOC.
 *
 * Return:	Success:	Non-negative, OBJ_LOC will contain information
 *				about the object to which the link points and
 *				GRP_LOC will contain the information about
 *				the group in which the link tail appears.
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *              Friday, April 10, 1998
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5G_traverse_slink(H5G_loc_t *grp_loc/*in,out*/, H5O_link_t *lnk,
    H5G_loc_t *obj_loc/*in,out*/, int *nlinks/*in,out*/, hid_t dxpl_id)
{
    H5G_trav_ud1_t      udata;                  /* User data to pass to link traversal callback */
    H5G_name_t          tmp_obj_path;           /* Temporary copy of object's path */
    hbool_t             tmp_obj_path_set = FALSE;       /* Flag to indicate that tmp object path is initialized */
    H5O_loc_t           tmp_grp_oloc;           /* Temporary copy of group entry */
    H5G_name_t          tmp_grp_path;           /* Temporary copy of group's path */
    H5G_loc_t           tmp_grp_loc;            /* Temporary copy of group's location */
    hbool_t             tmp_grp_path_set = FALSE;       /* Flag to indicate that tmp group path is initialized */
    herr_t              ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5G_traverse_slink)

    /* Sanity check */
    HDassert(grp_loc);
    HDassert(lnk);
    HDassert(lnk->type == H5G_LINK_SOFT);
    HDassert(nlinks);

    /* Set up temporary location */
    tmp_grp_loc.oloc = &tmp_grp_oloc;
    tmp_grp_loc.path = &tmp_grp_path;

    /* Portably initialize the temporary objects */
    H5G_loc_reset(&tmp_grp_loc);
    H5G_name_reset(&tmp_obj_path);

    /* Clone the group location, so we can track the names properly */
    /* ("tracking the names properly" means to ignore the effects of the
     *  link traversal on the object's & group's paths - QAK)
     */
    H5G_loc_copy(&tmp_grp_loc, grp_loc, H5_COPY_DEEP);
    tmp_grp_path_set = TRUE;

    /* Hold the object's group hier. path to restore later */
    /* (Part of "tracking the names properly") */
    H5G_name_copy(&tmp_obj_path, obj_loc->path, H5_COPY_SHALLOW);
    tmp_obj_path_set = TRUE;

    /* Set up user data for traversal callback */
    udata.obj_loc = obj_loc;

    /* Traverse the link */
    if(H5G_traverse_real(&tmp_grp_loc, lnk->u.soft.name, H5G_TARGET_NORMAL, nlinks, H5G_traverse_link_cb, &udata, dxpl_id) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "unable to follow symbolic link")

done:
    /* Restore object's group hier. path */
    if(tmp_obj_path_set) {
        H5G_name_free(obj_loc->path);
        H5G_name_copy(obj_loc->path, &tmp_obj_path, H5_COPY_SHALLOW);
    } /* end if */

    /* Release cloned copy of group path */
    if(tmp_grp_path_set)
        H5G_name_free(&tmp_grp_path);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_traverse_slink() */


/*-------------------------------------------------------------------------
 * Function:	H5G_traverse_mount
 *
 * Purpose:	If LNK is a mount point then copy the entry for the root
 *		group of the mounted file into LNK.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Tuesday, October  6, 1998
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5G_traverse_mount(H5G_loc_t *obj_loc/*in,out*/)
{
    H5F_t	*parent = obj_loc->oloc->file;       /* File of object */
    unsigned	lt, rt, md = 0;                 /* Binary search indices */
    int cmp;
    H5O_loc_t	*oloc = NULL;           /* Object location for mount points */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(H5G_traverse_mount, FAIL)

    /* Sanity check */
    HDassert(obj_loc);

    /*
     * The loop is necessary because we might have file1 mounted at the root
     * of file2, which is mounted somewhere in file3.
     */
    do {
	/*
	 * Use a binary search to find the potential mount point in the mount
	 * table for the parent
	 */
	lt = 0;
	rt = parent->mtab.nmounts;
	cmp = -1;
	while(lt < rt && cmp) {
	    md = (lt + rt) / 2;
	    oloc = H5G_oloc(parent->mtab.child[md].group);
	    cmp = H5F_addr_cmp(obj_loc->oloc->addr, oloc->addr);
	    if(cmp < 0)
		rt = md;
	    else
		lt = md + 1;
	} /* end while */

	/* Copy root info over to ENT */
	if(0 == cmp) {
            /* Get the location for the root group in the child's file */
	    oloc = H5G_oloc(parent->mtab.child[md].file->shared->root_grp);

            /* Copy the entry for the root group */
            if(H5O_loc_copy(obj_loc->oloc, oloc, H5_COPY_DEEP) < 0)
                HGOTO_ERROR(H5E_FILE, H5E_CANTCOPY, FAIL, "unable to copy object location")

            /* Switch to child's file */
	    parent = oloc->file;
	} /* end if */
    } while(!cmp);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_traverse_mount() */


/*-------------------------------------------------------------------------
 * Function:	H5G_traverse_real
 *
 * Purpose:	Internal version of path traversal routine
 *
 * Return:	Success:	Non-negative if name can be fully resolved.
 *
 *		Failure:	Negative if the name could not be fully
 *				resolved.
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug 11 1997
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5G_traverse_real(const H5G_loc_t *_loc, const char *name, unsigned target,
    int *nlinks, H5G_traverse_t op, void *op_data, hid_t dxpl_id)
{
    H5G_loc_t           loc;            /* Location of start object     */
    H5O_loc_t           grp_oloc;	/* Object loc. for current group */
    H5G_name_t		grp_path;	/* Path for current group	*/
    H5G_loc_t           grp_loc;        /* Location of group            */
    H5O_loc_t		obj_oloc;	/* Object found			*/
    H5G_name_t		obj_path;	/* Path for object found	*/
    H5G_loc_t           obj_loc;        /* Location of object           */
    size_t		nchars;		/* component name length	*/
    H5O_link_t          lnk;            /* Link information for object  */
    hbool_t link_valid = FALSE;         /* Flag to indicate that the link information is valid */
    hbool_t obj_loc_valid = FALSE;      /* Flag to indicate that the object location is valid */
    hbool_t group_copy = FALSE;         /* Flag to indicate that the group entry is copied */
    hbool_t last_comp = FALSE;          /* Flag to indicate that a component is the last component in the name */
    herr_t              ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5G_traverse_real)

    /* Check parameters */
    HDassert(_loc);
    HDassert(name);
    HDassert(nlinks);
    HDassert(op);

    /*
     * Where does the searching start?  For absolute names it starts at the
     * root of the file; for relative names it starts at CWG.
     */
    /* Check if we need to get the root group's entry */
    if('/' == *name) {
        H5G_t *root_grp;         /* Temporary pointer to root group of file */

        /* Look up root group for starting location */
        root_grp = H5G_rootof(_loc->oloc->file);
        HDassert(root_grp);

        /* Set the location entry to the root group's info */
        loc.oloc=&(root_grp->oloc);
        loc.path=&(root_grp->path);
    } /* end if */
    else {
        loc.oloc = _loc->oloc;
        loc.path = _loc->path;
    } /* end else */

    /* Set up group & object locations */
    grp_loc.oloc = &grp_oloc;
    grp_loc.path = &grp_path;
    obj_loc.oloc = &obj_oloc;
    obj_loc.path = &obj_path;

#if defined(H5_USING_PURIFY) || !defined(NDEBUG)
    /* Clear group location */
    if(H5G_loc_reset(&grp_loc) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, FAIL, "unable to reset location")
#endif /* H5_USING_PURIFY */

    /* Deep copy of the starting location to group location */
    if(H5G_loc_copy(&grp_loc, &loc, H5_COPY_DEEP) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, FAIL, "unable to copy location")
    group_copy = TRUE;

    /* Clear object location */
    if(H5G_loc_reset(&obj_loc) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, FAIL, "unable to reset location")

    /* Check for needing a larger buffer for the individual path name components */
    if(HDstrlen(name) + 1 > H5G_comp_alloc_g) {
        H5G_comp_alloc_g = MAX3(1024, 2 * H5G_comp_alloc_g, HDstrlen(name) + 1);
        H5G_comp_g = H5MM_realloc(H5G_comp_g, H5G_comp_alloc_g);
        if(!H5G_comp_g) {
            H5G_comp_alloc_g = 0;
            HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "unable to allocate component buffer")
        } /* end if */
    } /* end if */

    /* Traverse the path */
    while((name = H5G_component(name, &nchars)) && *name) {
        const char *s;                  /* Temporary string pointer */
        herr_t lookup_status;           /* Status from object lookup */

	/*
	 * Copy the component name into a null-terminated buffer so
	 * we can pass it down to the other symbol table functions.
	 */
	HDmemcpy(H5G_comp_g, name, nchars);
	H5G_comp_g[nchars] = '\0';

	/*
	 * The special name `.' is a no-op.
	 */
	if('.' == H5G_comp_g[0] && !H5G_comp_g[1]) {
	    name += nchars;
	    continue;
	} /* end if */

        /* Check if this is the last component of the name */
        if(!((s = H5G_component(name + nchars, NULL)) && *s))
            last_comp = TRUE;

        /* If there's valid information in the link, reset it */
        if(link_valid) {
#ifdef H5_GROUP_REVISION
            H5O_reset(H5O_LINK_ID, &lnk);
#else /* H5_GROUP_REVISION */
            /* Free information for link (but don't free link pointer) */
            if(lnk.type == H5G_LINK_SOFT)
                lnk.u.soft.name = H5MM_xfree(lnk.u.soft.name);
            lnk.name = H5MM_xfree(lnk.name);
#endif /* H5_GROUP_REVISION */
            link_valid = FALSE;
        } /* end if */

        /* Get information for object in current group */
        /* (Defer issuing error for bad lookup until later) */
        lookup_status = H5G_obj_lookup(grp_loc.oloc, H5G_comp_g, &lnk/*out*/, dxpl_id);

        /* If the lookup was OK, try traversing soft links and mount points, if allowed */
        if(lookup_status >= 0) {
            /* Indicate that the link info is valid */
            link_valid = TRUE;

            /* Build object's group hier. location */
            if(H5G_name_set(grp_loc.path, obj_loc.path, H5G_comp_g) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "cannot set name")

            /* Set the object location, if it's a hard link set the address also */
            obj_loc.oloc->file = grp_loc.oloc->file;
            if(lnk.type == H5G_LINK_HARD)
                obj_loc.oloc->addr = lnk.u.hard.addr;
            obj_loc_valid = TRUE;

            /*
             * If we found a symbolic link then we should follow it.  But if this
             * is the last component of the name and the H5G_TARGET_SLINK bit of
             * TARGET is set then we don't follow it.
             */
            if(H5G_LINK_SOFT == lnk.type &&
                    (0 == (target & H5G_TARGET_SLINK) || !last_comp)) {
                if((*nlinks)-- <= 0)
                    HGOTO_ERROR(H5E_SYM, H5E_LINK, FAIL, "too many links")
                if(H5G_traverse_slink(&grp_loc/*in,out*/, &lnk/*in*/, &obj_loc, nlinks, dxpl_id) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_SLINK, FAIL, "symbolic link traversal failed")
            } /* end if */

            /*
             * Resolve mount points to the mounted group.  Do not do this step if
             * the H5G_TARGET_MOUNT bit of TARGET is set and this is the last
             * component of the name.
             *
             * (If this link is a hard link, try to perform mount point traversal)
             *
             * (Note that the soft link traversal above can change the status of
             *  the object (into a hard link), so don't use an 'else' statement
             *  here. -QAK)
             */
            if(H5F_addr_defined(obj_loc.oloc->addr) &&
                    (0 == (target & H5G_TARGET_MOUNT) || !last_comp)) {
                if(H5G_traverse_mount(&obj_loc/*in,out*/) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "mount point traversal failed")
            } /* end if */
        } /* end if */

        /* Check for last component in name provided */
        if(last_comp) {
            H5O_link_t *tmp_lnk;        /* Pointer to link info for callback */
            H5G_loc_t *tmp_loc;         /* Pointer to object location for callback */

            /* Set callback parameters appropriately, based on link being found */
            if(lookup_status < 0) {
                tmp_lnk = NULL;
                tmp_loc = NULL;
            } /* end if */
            else {
                tmp_lnk = &lnk;
                tmp_loc = &obj_loc;
            } /* end else */

            /* Operator routine will take care of object location, succeed or fail */
            obj_loc_valid = FALSE;

            /* Call 'operator' routine */
            if((op)(&grp_loc, H5G_comp_g, tmp_lnk, tmp_loc, op_data) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CALLBACK, FAIL, "traversal operator failed")
            HGOTO_DONE(SUCCEED)
        } /* end if */

        /* Handle lookup failures now */
        if(lookup_status < 0) {
            /* If an intermediate group doesn't exist & flag is set, create the group */
            if(target & H5G_CRT_INTMD_GROUP) {
#ifdef H5_GROUP_REVISION
                H5O_ginfo_t	ginfo;		/* Group info message for parent group */

                /* Get the group info for parent group */
                if(NULL == H5O_read(grp_loc.oloc, H5O_GINFO_ID, 0, &ginfo, dxpl_id))
                    HGOTO_ERROR(H5E_SYM, H5E_BADMESG, FAIL, "can't get group info")
#endif /* H5_GROUP_REVISION */

                /* Create the intermediate group */
/* XXX: Should we allow user to control the group creation params here? -QAK */
                if(H5G_obj_create(grp_oloc.file, dxpl_id,
#ifdef H5_GROUP_REVISION
                        &ginfo,
#endif /* H5_GROUP_REVISION */
                        obj_loc.oloc/*out*/) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to create group entry")

                /* Insert new group into current group's symbol table */
                if(H5G_loc_insert(&grp_loc, H5G_comp_g, &obj_loc, TRUE, dxpl_id) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINSERT, FAIL, "unable to insert intermediate group")

                /* Close new group */
                if(H5O_close(obj_loc.oloc) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to close")
            } /* end if */
            else
                HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "component not found")
        } /* end if */

	/*
	 * Advance to the next component of the path.
	 */

        /* Transfer "ownership" of the object's information to the group object */
        H5G_loc_free(&grp_loc);
        H5G_loc_copy(&grp_loc, &obj_loc, H5_COPY_SHALLOW);
        H5G_loc_reset(&obj_loc);
        obj_loc_valid = FALSE;

	/* Advance to next component in string */
	name += nchars;
    } /* end while */

    /* If we've fallen through to here, the name must be something like just '.'
     * and we should issue the callback on that. -QAK
     */
    /* Reset "group copied" flag */
    /* (callback will take ownership of group location, succeed or fail) */
    HDassert(group_copy);
    group_copy = FALSE;

    /* Call 'operator' routine */
    if((op)(&grp_loc, ".", NULL, &grp_loc, op_data) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTNEXT, FAIL, "traversal operator failed")
    HGOTO_DONE(SUCCEED)

done:
    /* If the object location is still valid (usually in an error situation), reset it */
    if(obj_loc_valid)
        H5G_loc_free(&obj_loc);
    /* If there's valid information in the link, reset it */
    if(link_valid) {
#ifdef H5_GROUP_REVISION
        H5O_reset(H5O_LINK_ID, &lnk);
#else /* H5_GROUP_REVISION */
        /* Free information for link (but don't free link pointer) */
        if(lnk.type == H5G_LINK_SOFT)
            lnk.u.soft.name = H5MM_xfree(lnk.u.soft.name);
        lnk.name = H5MM_xfree(lnk.name);
#endif /* H5_GROUP_REVISION */
    } /* end if */
    /* If we copied something into the group location, free it */
    if(group_copy)
        H5G_loc_free(&grp_loc);

   FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_traverse_real() */


/*-------------------------------------------------------------------------
 * Function:	H5G_traverse
 *
 * Purpose:	Traverse a path from a location & perform an operation when
 *              the last component of the name is reached.
 *
 * Return:	Success:	Non-negative if path can be fully traversed.
 *		Failure:	Negative if the path could not be fully
 *				traversed.
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Sep 13 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_traverse(const H5G_loc_t *loc, const char *name, unsigned target, H5G_traverse_t op,
    void *op_data, hid_t dxpl_id)
{
    int		nlinks = H5G_NLINKS;    /* Link countdown value */
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI(H5G_traverse, FAIL)

    /* Check args */
    if(!name || !*name)
        HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "no name given")
    if(!loc)
        HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "no starting location")
    if(!op)
        HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "no operation provided")

    /* Go perform "real" traversal */
    if(H5G_traverse_real(loc, name, target, &nlinks, op, op_data, dxpl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "path traversal failed")

done:
   FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_traverse() */

