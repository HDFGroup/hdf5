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
 * Created:		H5Gtraverse.c
 *			Dec 19 2005
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
#ifdef QAK
#include "H5Iprivate.h"		/* IDs			  		*/
#endif /* QAK */
#include "H5MMprivate.h"	/* Memory management			*/

/* Private typedefs */

/* Private macros */

/* Local variables */
static char *H5G_comp_g = NULL;                 /*component buffer      */
static size_t H5G_comp_alloc_g = 0;             /*sizeof component buffer */

/* PRIVATE PROTOTYPES */
static herr_t H5G_traverse_slink(H5G_entry_t *grp_ent/*in,out*/,
    H5G_entry_t *obj_ent/*in,out*/, int *nlinks/*in,out*/, hid_t dxpl_id);


/*-------------------------------------------------------------------------
 * Function:	H5G_namei_term_interface
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
H5G_namei_term_interface(void)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5G_namei_term_interface)

    /* Free the global component buffer */
    H5G_comp_g = H5MM_xfree(H5G_comp_g);
    H5G_comp_alloc_g = 0;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5G_namei_term_interface() */


/*-------------------------------------------------------------------------
 * Function:	H5G_traverse_slink
 *
 * Purpose:	Traverses symbolic link.  The link head appears in the group
 *		whose entry is GRP_ENT and the link head entry is OBJ_ENT.
 *
 * Return:	Success:	Non-negative, OBJ_ENT will contain information
 *				about the object to which the link points and
 *				GRP_ENT will contain the information about
 *				the group in which the link tail appears.
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *              Friday, April 10, 1998
 *
 * Modifications:
 *
 *      Pedro Vicente, <pvn@ncsa.uiuc.edu> 22 Aug 2002
 *      Added `id to name' support.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5G_traverse_slink (H5G_entry_t *grp_ent/*in,out*/,
		    H5G_entry_t *obj_ent/*in,out*/,
		    int *nlinks/*in,out*/, hid_t dxpl_id)
{
    H5O_stab_t		stab_mesg;		/*info about local heap	*/
    const char		*clv = NULL;		/*cached link value	*/
    char		*linkval = NULL;	/*the copied link value	*/
    H5G_entry_t         tmp_grp_ent;            /* Temporary copy of group entry */
    H5RS_str_t          *tmp_full_path_r = NULL, *tmp_user_path_r = NULL; /* Temporary pointer to object's user path & canonical path */
    const H5HL_t        *heap;
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5G_traverse_slink);

    /* Portably initialize the temporary group entry */
    H5G_ent_reset(&tmp_grp_ent);

    /* Get the link value */
    if (NULL==H5O_read (grp_ent, H5O_STAB_ID, 0, &stab_mesg, dxpl_id))
	HGOTO_ERROR (H5E_SYM, H5E_NOTFOUND, FAIL, "unable to determine local heap address");

    if (NULL == (heap = H5HL_protect(grp_ent->file, dxpl_id, stab_mesg.heap_addr)))
	HGOTO_ERROR (H5E_SYM, H5E_NOTFOUND, FAIL, "unable to read protect link value")

    clv = H5HL_offset_into(grp_ent->file, heap, obj_ent->cache.slink.lval_offset);

    linkval = H5MM_xstrdup (clv);
    assert(linkval);

    if (H5HL_unprotect(grp_ent->file, dxpl_id, heap, stab_mesg.heap_addr) < 0)
	HGOTO_ERROR (H5E_SYM, H5E_NOTFOUND, FAIL, "unable to read unprotect link value")

    /* Hold the entry's name (& old_name) to restore later */
    tmp_full_path_r = obj_ent->full_path_r;
    obj_ent->full_path_r = NULL;
    tmp_user_path_r = obj_ent->user_path_r;
    obj_ent->user_path_r = NULL;

    /* Free the names for the group entry */
    H5G_name_free(grp_ent);

    /* Clone the group entry, so we can track the names properly */
    H5G_ent_copy(&tmp_grp_ent,grp_ent,H5_COPY_DEEP);

    /* Traverse the link */
    if (H5G_namei (&tmp_grp_ent, linkval, NULL, grp_ent, obj_ent, H5G_TARGET_NORMAL, nlinks, H5G_NAMEI_TRAVERSE, NULL, dxpl_id))
	HGOTO_ERROR (H5E_SYM, H5E_NOTFOUND, FAIL, "unable to follow symbolic link");

    /* Free the entry's names, we will use the original name for the object */
    H5G_name_free(obj_ent);

    /* Restore previous name for object */
    obj_ent->full_path_r = tmp_full_path_r;
    tmp_full_path_r = NULL;
    obj_ent->user_path_r = tmp_user_path_r;
    tmp_user_path_r = NULL;

done:
    /* Error cleanup */
    if(tmp_full_path_r)
        H5RS_decr(tmp_full_path_r);
    if(tmp_user_path_r)
        H5RS_decr(tmp_user_path_r);

    /* Release cloned copy of group entry */
    H5G_name_free(&tmp_grp_ent);

    H5MM_xfree (linkval);
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5G_namei
 *
 * Purpose:	Translates a name to a symbol table entry.
 *
 *		If the specified name can be fully resolved, then this
 *		function returns the symbol table entry for the named object
 *		through the OBJ_ENT argument. The symbol table entry for the
 *		group containing the named object is returned through the
 *		GRP_ENT argument if it is non-null.  However, if the name
 *		refers to the root object then the GRP_ENT will be
 *		initialized with an undefined object header address.  The
 *		REST argument, if present, will point to the null terminator
 *		of NAME.
 *
 *		If the specified name cannot be fully resolved, then OBJ_ENT
 *		is initialized with the undefined object header address. The
 *		REST argument will point into the NAME argument to the start
 *		of the component that could not be located.  The GRP_ENT will
 *		contain the entry for the symbol table that was being
 *		searched at the time of the failure and will have an
 *		undefined object header address if the search failed at the
 *		root object. For instance, if NAME is `/foo/bar/baz' and the
 *		root directory exists and contains an entry for `foo', and
 *		foo is a group that contains an entry for bar, but bar is not
 *		a group, then the results will be that REST points to `baz',
 *		OBJ_ENT has an undefined object header address, and GRP_ENT
 *		is the symbol table entry for `bar' in `/foo'.
 *
 *		Every file has a root group whose name is `/'.  Components of
 *		a name are separated from one another by one or more slashes
 *		(/).  Slashes at the end of a name are ignored.  If the name
 *		begins with a slash then the search begins at the root group
 *		of the file containing LOC_ENT. Otherwise it begins at
 *		LOC_ENT.  The component `.' is a no-op, but `..' is not
 *		understood by this function (unless it appears as an entry in
 *		the symbol table).
 *
 *		Symbolic links are followed automatically, but if TARGET
 *		includes the H5G_TARGET_SLINK bit and the last component of
 *		the name is a symbolic link then that link is not followed.
 *		The *NLINKS value is decremented each time a link is followed
 *		and link traversal fails if the value would become negative.
 *		If NLINKS is the null pointer then a default value is used.
 *
 *		Mounted files are handled by calling H5F_mountpoint() after
 *		each step of the translation.  If the input argument to that
 *		function is a mount point then the argument shall be replaced
 *		with information about the root group of the mounted file.
 *		But if TARGET includes the H5G_TARGET_MOUNT bit and the last
 *		component of the name is a mount point then H5F_mountpoint()
 *		is not called and information about the mount point itself is
 *		returned.
 *
 * Errors:
 *
 * Return:	Success:	Non-negative if name can be fully resolved.
 *				See above for values of REST, GRP_ENT, and
 *				OBJ_ENT.  NLINKS has been decremented for
 *				each symbolic link that was followed.
 *
 *		Failure:	Negative if the name could not be fully
 *				resolved. See above for values of REST,
 *				GRP_ENT, and OBJ_ENT.
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug 11 1997
 *
 * Modifications:
 *      Robb Matzke, 2002-03-28
 *      The component name buffer on the stack has been replaced by
 *      a dynamically allocated buffer on the heap in order to
 *      remove limitations on the length of a name component.
 *      There are two reasons that the buffer pointer is global:
 *        (1) We want to be able to reuse the buffer without
 *            allocating and freeing it each time this function is
 *            called.
 *        (2) We need to be able to free it from H5G_term_interface()
 *            when the library terminates.
 *
 *      Pedro Vicente, <pvn@ncsa.uiuc.edu> 22 Aug 2002
 *      Modified to deep copies of symbol table entries
 *      Added `id to name' support.
 *
 *      Quincey Koziol, 2003-01-06
 *      Added "action" and "ent" parameters to allow different actions when
 *      working on the last component of a name.  (Specifically, this allows
 *      inserting an entry into a group, instead of trying to look it up)
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_namei(const H5G_entry_t *loc_ent, const char *name, const char **rest/*out*/,
	  H5G_entry_t *grp_ent/*out*/, H5G_entry_t *obj_ent/*out*/,
	  unsigned target, int *nlinks/*out*/, H5G_namei_act_t action,
          H5G_entry_t *ent, hid_t dxpl_id)
{
    H5G_entry_t		  _grp_ent;     /*entry for current group	*/
    H5G_entry_t		  _obj_ent;     /*entry found			*/
    size_t		  nchars;	/*component name length		*/
    int			  _nlinks = H5G_NLINKS;
    const char		   *s = NULL;
    unsigned null_obj;          /* Flag to indicate this function was called with obj_ent set to NULL */
    unsigned null_grp;          /* Flag to indicate this function was called with grp_ent set to NULL */
    unsigned obj_copy = 0;      /* Flag to indicate that the object entry is copied */
    unsigned group_copy = 0;    /* Flag to indicate that the group entry is copied */
    unsigned last_comp = 0;     /* Flag to indicate that a component is the last component in the name */
    unsigned did_insert = 0;    /* Flag to indicate that H5G_stab_insert was called */
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5G_namei);

    /* Set up "out" parameters */
    if (rest)
        *rest = name;
    if (!grp_ent) {
        grp_ent = &_grp_ent;
        null_grp = 1;
    } /* end if */
    else
        null_grp = 0;
    if (!obj_ent) {
        obj_ent = &_obj_ent;
        null_obj = 1;
    } /* end if */
    else
        null_obj = 0;
    if (!nlinks)
        nlinks = &_nlinks;

    /* Check args */
    if (!name || !*name)
        HGOTO_ERROR (H5E_SYM, H5E_NOTFOUND, FAIL, "no name given");
    if (!loc_ent)
        HGOTO_ERROR (H5E_SYM, H5E_NOTFOUND, FAIL, "no current working group");

    /*
     * Where does the searching start?  For absolute names it starts at the
     * root of the file; for relative names it starts at CWG.
     */
    /* Check if we need to get the root group's entry */
    if('/' == *name) {
        H5G_t *tmp_grp;         /* Temporary pointer to root group of file */

        /* Look up root group for starting location */
        tmp_grp = H5G_rootof(loc_ent->file);
        HDassert(tmp_grp);

        /* Set the location entry to the root group's entry*/
        loc_ent = &(tmp_grp->ent);
    } /* end if */

    /* Deep copy of the symbol table entry (duplicates strings) */
    if(H5G_ent_copy(obj_ent, loc_ent, H5_COPY_DEEP) < 0)
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTOPENOBJ, FAIL, "unable to copy entry")
    obj_copy = 1;

    H5G_ent_reset(grp_ent);

    /* Check for needing a larger buffer for the individual path name components */
    if((HDstrlen(name) + 1) > H5G_comp_alloc_g) {
        char *new_comp;                 /* New component buffer */
        size_t new_alloc;               /* New component buffer size */

        new_alloc = MAX3(1024, (2 * H5G_comp_alloc_g), (HDstrlen(name) + 1));
        if(NULL == (new_comp = H5MM_realloc(H5G_comp_g, new_alloc)))
            HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "unable to allocate component buffer")
        H5G_comp_g = new_comp;
        H5G_comp_alloc_g = new_alloc;
    } /* end if */

    /* traverse the name */
    while ((name = H5G_component(name, &nchars)) && *name) {
        /* Update the "rest of name" pointer */
	if(rest)
            *rest = name;

	/*
	 * Copy the component name into a null-terminated buffer so
	 * we can pass it down to the other symbol table functions.
	 */
	HDmemcpy(H5G_comp_g, name, nchars);
	H5G_comp_g[nchars] = '\0';

	/*
	 * The special name `.' is a no-op.
	 */
	if ('.' == H5G_comp_g[0] && !H5G_comp_g[1]) {
	    name += nchars;
	    continue;
	} /* end if */

	/*
	 * Advance to the next component of the name.
	 */
        /* If we've already copied a new entry into the group entry,
         * it needs to be freed before overwriting it with another entry
         */
	if(group_copy)
            H5G_name_free(grp_ent);

        /* Transfer "ownership" of the entry's information to the group entry */
        H5G_ent_copy(grp_ent, obj_ent, H5_COPY_SHALLOW);
        H5G_ent_reset(obj_ent);

	/* Set flag that we've copied a new entry into the group entry */
	group_copy = 1;

        /* Check if this is the last component of the name */
        if(!((s=H5G_component(name+nchars, NULL)) && *s))
            last_comp=1;

        switch(action) {
            case H5G_NAMEI_TRAVERSE:
                if (H5G_stab_find(grp_ent, H5G_comp_g, obj_ent/*out*/, dxpl_id )<0) {
                    /*
                     * Component was not found in the current symbol table, possibly
                     * because GRP_ENT isn't a symbol table.
                     */
                    HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "component not found");
                }
                break;

            case H5G_NAMEI_INSERT:
                if(!last_comp) {
                    if (H5G_stab_find(grp_ent, H5G_comp_g, obj_ent/*out*/, dxpl_id )<0) {
                        /*
                         * Component was not found in the current symbol table, possibly
                         * because GRP_ENT isn't a symbol table.
                         */
                        HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "component not found");
                    }
                } /* end if */
                else {
                    did_insert = 1;
                    if(H5G_stab_insert(grp_ent, H5G_comp_g, ent, TRUE, dxpl_id) < 0)
                        HGOTO_ERROR(H5E_SYM, H5E_CANTINSERT, FAIL, "unable to insert name")
                    HGOTO_DONE(SUCCEED);
                } /* end else */
                break;
        } /* end switch */

	/*
	 * If we found a symbolic link then we should follow it.  But if this
	 * is the last component of the name and the H5G_TARGET_SLINK bit of
	 * TARGET is set then we don't follow it.
	 */
	if(H5G_CACHED_SLINK==obj_ent->type &&
                (0==(target & H5G_TARGET_SLINK) || !last_comp)) {
	    if ((*nlinks)-- <= 0)
		HGOTO_ERROR (H5E_SYM, H5E_SLINK, FAIL, "too many links");
	    if (H5G_traverse_slink (grp_ent, obj_ent, nlinks, dxpl_id)<0)
		HGOTO_ERROR (H5E_SYM, H5E_NOTFOUND, FAIL, "symbolic link traversal failed");
	}

	/*
	 * Resolve mount points to the mounted group.  Do not do this step if
	 * the H5G_TARGET_MOUNT bit of TARGET is set and this is the last
	 * component of the name.
	 */
	if (0==(target & H5G_TARGET_MOUNT) || !last_comp)
	    H5F_mountpoint(obj_ent/*in,out*/);

	/* next component */
	name += nchars;
    } /* end while */

    /* Update the "rest of name" pointer */
    if (rest)
        *rest = name; /*final null */

    /* If this was an insert, make sure that the insert function was actually
     * called (this catches no-op names like "." and "/") */
     if(action == H5G_NAMEI_INSERT && !did_insert)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "group already exists");

done:
    /* If we started with a NULL obj_ent, free the entry information */
    if(null_obj || (ret_value < 0 && obj_copy))
        H5G_name_free(obj_ent);
    /* If we started with a NULL grp_ent and we copied something into it, free the entry information */
    if(null_grp && group_copy)
        H5G_name_free(grp_ent);

   FUNC_LEAVE_NOAPI(ret_value);
}

