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
 * Created:		H5Gobj.c
 *			Apr  8 2009
 *			Neil Fortner <nfortne2@hdfgroup.org>
 *
 * Purpose:		Functions for operating on the root group.
 *
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/

#define H5F_PACKAGE		/*suppress error about including H5Fpkg	  */
#define H5G_PACKAGE		/*suppress error about including H5Gpkg  */


/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Fpkg.h"		/* File access				*/
#include "H5Gpkg.h"		/* Groups		  		*/
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Pprivate.h"         /* Property Lists			*/



/*-------------------------------------------------------------------------
 * Function:	H5G_rootof
 *
 * Purpose:	Return a pointer to the root group of the file.  If the file
 *		is part of a virtual file then the root group of the virtual
 *		file is returned.
 *
 * Return:	Success:	Ptr to the root group of the file.  Do not
 *				free the pointer -- it points directly into
 *				the file struct.
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *              Tuesday, October 13, 1998
 *
 *-------------------------------------------------------------------------
 */
H5G_t *
H5G_rootof(H5F_t *f)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5G_rootof)

    while(f->parent)
        f = f->parent;

    FUNC_LEAVE_NOAPI(f->shared->root_grp)
} /* end H5G_rootof() */


/*-------------------------------------------------------------------------
 * Function:    H5G_root_ent_decode
 *
 * Purpose:     Decodes the root group symbol table entry into the file
 *              structure, and updates the root group address in the file
 *              structure.
 *
 * Return:      Success:        Non-negative with *pp pointing to the first byte
 *                              following the symbol table entry.
 *
 *              Failure:        Negative
 *
 * Programmer:  Quincey Koziol
 *              koziol@ncsa.uiuc.edu
 *              Sep 26 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_root_ent_decode(H5F_t *f, const uint8_t **pp)
{
    const uint8_t	*p_ret = *pp + H5G_SIZEOF_ENTRY(f);
    herr_t          ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(H5G_root_ent_decode, FAIL)

    /* check arguments */
    HDassert(f);
    HDassert(pp);

    /* Allocate space for the root group symbol table entry */
    HDassert(!f->shared->root_ent);
    if(NULL == (f->shared->root_ent = (H5G_entry_t *) H5MM_calloc(sizeof(H5G_entry_t))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "can't allocate space for symbol table entry")

    /* decode the root group symbol table entry */
    if(H5G_ent_decode_vec(f, pp, f->shared->root_ent, 1) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTDECODE, FAIL, "can't decode symbol table entry")

    /* Set the root group address to the correct value */
    f->shared->root_addr = f->shared->root_ent->header;

    /* Set decode pointer */
    *pp = p_ret;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_root_ent_decode() */


/*-------------------------------------------------------------------------
 * Function:    H5G_root_ent_encode
 *
 * Purpose:     Encodes the root group symbol table entry into the buffer
 *              pointed to by *pp.
 *
 * Return:      Success:        Non-negative, with *pp pointing to the first byte
 *                              after the symbol table entry.
 *
 *              Failure:        Negative
 *
 * Programmer:  Quincey Koziol
 *              koziol@ncsa.uiuc.edu
 *              Sep 26 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_root_ent_encode(H5F_t *f, uint8_t **pp)
{
    uint8_t		*p_ret = *pp + H5G_SIZEOF_ENTRY(f);
    herr_t              ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(H5G_root_ent_encode, FAIL)

    /* check arguments */
    HDassert(f);
    HDassert(f->shared);
    HDassert(f->shared->root_ent);
    HDassert(pp);

    /* Encode entry */
    if(H5G_ent_encode_vec(f, pp, f->shared->root_ent, 1) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "can't encode symbol table entry")

    /* Set encode pointer */
    *pp = p_ret;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_root_ent_encode() */


/*-------------------------------------------------------------------------
 * Function:	H5G_mkroot
 *
 * Purpose:	Creates a root group in an empty file and opens it.  If a
 *		root group is already open then this function immediately
 *		returns.   If ENT is non-null then it's the symbol table
 *		entry for an existing group which will be opened as the root
 *		group.  Otherwise a new root group is created and then
 *		opened.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug 11 1997
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_mkroot(H5F_t *f, hid_t dxpl_id, hbool_t create_root)
{
    H5G_loc_t   root_loc;               /* Root location information */
    htri_t      stab_exists = -1;       /* Whether the symbol table exists */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(H5G_mkroot, FAIL)

    /* check args */
    HDassert(f);

    /* Check if the root group is already initialized */
    if(f->shared->root_grp)
        HGOTO_DONE(SUCCEED)

    /* Create information needed for group nodes */
    if(H5G_node_init(f) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to create group node info")

    /*
     * Create the group pointer
     */
    if(NULL == (f->shared->root_grp = H5FL_CALLOC(H5G_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")
    if(NULL == (f->shared->root_grp->shared = H5FL_CALLOC(H5G_shared_t))) {
        (void)H5FL_FREE(H5G_t, f->shared->root_grp);
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")
    } /* end if */

    /* Initialize the root_loc structure to point to fields in the newly created
     * f->shared->root_grp structure */
    root_loc.oloc = &(f->shared->root_grp->oloc);
    root_loc.path = &(f->shared->root_grp->path);
    H5G_loc_reset(&root_loc);

    /*
     * If there is no root object then create one. The root group always starts
     * with a hard link count of one since it's pointed to by the superblock.
     */
    if(create_root) {
        H5P_genplist_t *fc_plist;       /* File creation property list */
        H5O_ginfo_t     ginfo;          /* Group info parameters */
        H5O_linfo_t     linfo;          /* Link info parameters */
        unsigned        super_vers;     /* Superblock version */

        /* Get the file creation property list */
        /* (Which is a sub-class of the group creation property class) */
        if(NULL == (fc_plist = (H5P_genplist_t *)H5I_object(f->shared->fcpl_id)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list")

        /* Get the group info property */
        if(H5P_get(fc_plist, H5G_CRT_GROUP_INFO_NAME, &ginfo) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get group info")

        /* Get the link info property */
        if(H5P_get(fc_plist, H5G_CRT_LINK_INFO_NAME, &linfo) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get link info")

        /* Get the superblock version */
        if(H5P_get(fc_plist, H5F_CRT_SUPER_VERS_NAME, &super_vers) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "unable to get superblock version")

        /* Create root group */
	if(H5G_obj_create(f, dxpl_id, &ginfo, &linfo, f->shared->fcpl_id, root_loc.oloc/*out*/) < 0)
	    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to create group entry")
	if(1 != H5O_link(root_loc.oloc, 1, dxpl_id))
	    HGOTO_ERROR(H5E_SYM, H5E_LINKCOUNT, FAIL, "internal error (wrong link count)")

        /* Create the root group symbol table entry */
        HDassert(!f->shared->root_ent);
        if(super_vers < HDF5_SUPERBLOCK_VERSION_2) {
            /* Allocate space for the root group symbol table entry */
            if(NULL == (f->shared->root_ent = (H5G_entry_t *) H5MM_calloc(sizeof(H5G_entry_t))))
                HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "can't allocate space for symbol table entry")

            /* Initialize the root group symbol table entry */
            f->shared->root_ent->dirty = TRUE;
            f->shared->root_ent->type = H5G_NOTHING_CACHED; /* We will cache the stab later */
            f->shared->root_ent->name_off = 0;  /* No name (yet) */
            f->shared->root_ent->header = root_loc.oloc->addr;
            f->shared->root_ent->file = root_loc.oloc->file;
        } /* end if */
    } /* end if */
    else {
        /* Create root group object location from f */
        root_loc.oloc->addr = f->shared->root_addr;
        root_loc.oloc->file = f;

	/*
	 * Open the root object as a group.
	 */
	if(H5O_open(root_loc.oloc) < 0)
	    HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, FAIL, "unable to open root group")

        /* Actions to take if the symbol table information is cached */
        if(f->shared->root_ent && f->shared->root_ent->type == H5G_CACHED_STAB) {
            /* Check for the situation where the symbol table is cached but does
             * not exist.  This can happen if, for example, an external link is
             * added to the root group. */
            if((stab_exists = H5O_msg_exists(root_loc.oloc, H5O_STAB_ID, dxpl_id)) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't check if symbol table message exists")

            /* Remove the cache if the stab does not exist */
            if(!stab_exists)
                f->shared->root_ent->type = H5G_NOTHING_CACHED;
#ifndef H5_STRICT_FORMAT_CHECKS
            /* If symbol table information is cached, check if we should replace the
            * symbol table message with the cached symbol table information */
            else if(H5F_INTENT(f) & H5F_ACC_RDWR) {
                H5O_stab_t      cached_stab;

                /* Retrieve the cached symbol table information */
                cached_stab.btree_addr = f->shared->root_ent->cache.stab.btree_addr;
                cached_stab.heap_addr = f->shared->root_ent->cache.stab.heap_addr;

                /* Check if the symbol table message is valid, and replace with the
                * cached symbol table if necessary */
                if(H5G_stab_valid(root_loc.oloc, dxpl_id, &cached_stab) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "unable to verify symbol table")
            } /* end if */
#endif /* H5_STRICT_FORMAT_CHECKS */
        } /* end if */
    } /* end else */

    /* Cache the root group's symbol table information in the root group symbol
     * table entry.  It will have been allocated by now if it needs to be
     * present, so we don't need to check the superblock version.  We do this if
     * we have write access, the root entry has been allocated (i.e.
     * super_vers < 2) and the stab info is not already cached. */
    if((H5F_INTENT(f) & H5F_ACC_RDWR) && stab_exists != FALSE && f->shared->root_ent
            && f->shared->root_ent->type != H5G_CACHED_STAB) {
        H5O_stab_t      stab;           /* Symbol table */

        /* Check if the stab message exists.  It's possible for the root group
         * to use the latest version while the superblock is an old version.
         * If stab_exists is not -1 then we have already checked. */
        if(stab_exists == -1 && (stab_exists = H5O_msg_exists(root_loc.oloc, H5O_STAB_ID, dxpl_id)) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't check if symbol table message exists")

        if(stab_exists) {
            /* Read the root group's symbol table message */
            if(NULL == H5O_msg_read(root_loc.oloc, H5O_STAB_ID, &stab, dxpl_id))
                HGOTO_ERROR(H5E_SYM, H5E_BADMESG, FAIL, "unable to read symbol table message")

            /* Update the root group symbol table entry */
            f->shared->root_ent->type = H5G_CACHED_STAB;
            f->shared->root_ent->cache.stab.btree_addr = stab.btree_addr;
            f->shared->root_ent->cache.stab.heap_addr = stab.heap_addr;
        } /* end if */
    } /* end if */

    /* Create the path names for the root group's entry */
    H5G_name_init(root_loc.path, "/");

    f->shared->root_grp->shared->fo_count = 1;
    /* The only other open object should be the superblock extension, if it
     * exists.  Don't count either the superblock extension or the root group
     * in the number of open objects in the file.
     */
    HDassert((1 == f->nopen_objs) ||
            (2 == f->nopen_objs && HADDR_UNDEF != f->shared->extension_addr));
    f->nopen_objs--;

done:
    /* In case of error, free various memory locations that may have been
     * allocated */
    if(ret_value < 0) {
        if(f->shared->root_grp) {
            if(f->shared->root_grp->shared)
                f->shared->root_grp->shared = H5FL_FREE(H5G_shared_t, f->shared->root_grp->shared);
            f->shared->root_grp = H5FL_FREE(H5G_t, f->shared->root_grp);
        } /* end if */
        f->shared->root_ent = (H5G_entry_t *) H5MM_xfree(f->shared->root_ent);
        H5G_name_free(root_loc.path);
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_mkroot() */

