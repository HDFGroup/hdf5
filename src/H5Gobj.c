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
 * Created:		H5Gobj.c
 *			Sep  5 2005
 *			Quincey Koziol <koziol@ncsa.uiuc.edu>
 *
 * Purpose:		Functions for abstract handling of objects in groups.
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
#include "H5HLprivate.h"	/* Local Heaps				*/
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5Lprivate.h"         /* Links			  	*/
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Pprivate.h"         /* Property Lists			*/


/****************/
/* Local Macros */
/****************/


/******************/
/* Local Typedefs */
/******************/

/* User data for object header iterator when converting link messages to dense
 * link storage
 */
typedef struct {
    H5F_t      *f;              /* Pointer to file for insertion */
    hid_t       dxpl_id;        /* DXPL during insertion */
    H5O_linfo_t *linfo;         /* Pointer to link info */
} H5G_obj_oh_it_ud1_t;

/* User data for link iterator when converting dense link storage to link
 * messages
 */
typedef struct {
    H5O_link_t *lnk_table;              /* Array of links to convert */
    size_t nlinks;                      /* Number of links converted */
    size_t alloc_links;                 /* Size of link table        */
} H5G_obj_lnk_it_ud1_t;

/* User data for symbol table iterator when converting old-format group to
 * a new-format group
 */
typedef struct {
    H5O_loc_t   *grp_oloc;              /* Pointer to group for insertion */
    hid_t       dxpl_id;                /* DXPL during insertion */
} H5G_obj_stab_it_ud1_t;

/********************/
/* Package Typedefs */
/********************/


/********************/
/* Local Prototypes */
/********************/
static herr_t H5G_obj_link_to_dense_cb(const void *_mesg, unsigned idx,
    void *_udata);


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
 * Function:	H5G_obj_cmp_name_inc
 *
 * Purpose:	Callback routine for comparing two link names, in
 *              increasing alphabetic order
 *
 * Return:	An integer less than, equal to, or greater than zero if the
 *              first argument is considered to be respectively less than,
 *              equal to, or greater than the second.  If two members compare
 *              as equal, their order in the sorted array is undefined.
 *              (i.e. same as strcmp())
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Sep  5 2005
 *
 *-------------------------------------------------------------------------
 */
int
H5G_obj_cmp_name_inc(const void *lnk1, const void *lnk2)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5G_obj_cmp_name_inc)

    FUNC_LEAVE_NOAPI(HDstrcmp(((const H5O_link_t *)lnk1)->name, ((const H5O_link_t *)lnk2)->name))
} /* end H5G_obj_cmp_name_inc() */


/*-------------------------------------------------------------------------
 * Function:	H5G_obj_cmp_name_dec
 *
 * Purpose:	Callback routine for comparing two link names, in
 *              decreasing alphabetic order
 *
 * Return:	An integer less than, equal to, or greater than zero if the
 *              second argument is considered to be respectively less than,
 *              equal to, or greater than the first.  If two members compare
 *              as equal, their order in the sorted array is undefined.
 *              (i.e. opposite strcmp())
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Sep 25 2006
 *
 *-------------------------------------------------------------------------
 */
int
H5G_obj_cmp_name_dec(const void *lnk1, const void *lnk2)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5G_obj_cmp_name_dec)

    FUNC_LEAVE_NOAPI(HDstrcmp(((const H5O_link_t *)lnk2)->name, ((const H5O_link_t *)lnk1)->name))
} /* end H5G_obj_cmp_name_dec() */


/*-------------------------------------------------------------------------
 * Function:	H5G_obj_release_table
 *
 * Purpose:     Release table containing a list of links for a group
 *
 * Return:	Success:        Non-negative
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *	        Sep  6, 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_obj_release_table(H5G_link_table_t *ltable)
{
    size_t      u;                      /* Local index variable */
    herr_t	ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5G_obj_release_table)

    /* Sanity check */
    HDassert(ltable);

    /* Release link info, if any */
    if(ltable->nlinks > 0) {
        /* Free link message information */
        for(u = 0; u < ltable->nlinks; u++)
            if(H5O_reset(H5O_LINK_ID, &(ltable->lnks[u])) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "unable to release link message")

        /* Free table of links */
        H5MM_xfree(ltable->lnks);
    } /* end if */
    else
        HDassert(ltable->lnks == NULL);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_obj_release_table() */


/*-------------------------------------------------------------------------
 * Function:    H5G_obj_create
 *
 * Purpose:     Create an object header for a group and update object location info
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              koziol@ncsa.uiuc.edu
 *              Sep 29 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_obj_create(H5F_t *f, hid_t dxpl_id, const H5O_ginfo_t *ginfo,
    H5O_loc_t *oloc/*out*/)
{
    H5O_linfo_t linfo;                  /* Link information */
    size_t hdr_size;                    /* Size of object header to request */
    hbool_t use_latest_format;          /* Flag indicating the new group format should be used */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(H5G_obj_create, FAIL)

    /*
     * Check arguments.
     */
    HDassert(f);
    HDassert(ginfo);
    HDassert(oloc);

    /* Check for using the latest version of the group format */
    /* (add more checks for creating "new format" groups when needed) */
    if(H5F_USE_LATEST_FORMAT(f) || ginfo->track_corder)
        use_latest_format = TRUE;
    else
        use_latest_format = FALSE;

    /* Check if we should be using the latest version of the group format */
    if(use_latest_format) {
        H5O_linfo_t def_linfo = H5G_CRT_LINK_INFO_DEF;  /* Default link info */
        H5O_link_t lnk;                     /* Temporary link message info for computing message size */
        char null_char = '\0';              /* Character for creating null string */
        size_t ginfo_size;                  /* Size of the group info message */
        size_t linfo_size;                  /* Size of the link info message */
        size_t link_size;                   /* Size of a link message */

        /* Initialize message information */
        HDmemcpy(&linfo, &def_linfo, sizeof(H5O_linfo_t));

        /* Calculate message size infomation, for creating group's object header */
        linfo_size = H5O_mesg_size(H5O_LINFO_ID, f, &linfo, (size_t)0);
        HDassert(linfo_size);

        ginfo_size = H5O_mesg_size(H5O_GINFO_ID, f, ginfo, (size_t)0);
        HDassert(ginfo_size);

        lnk.type = H5L_TYPE_HARD;
        lnk.corder = 0;
        lnk.corder_valid = ginfo->track_corder;
        lnk.name = &null_char;
        link_size = H5O_mesg_size(H5O_LINK_ID, f, &lnk, (size_t)ginfo->est_name_len);
        HDassert(link_size);

        /* Compute size of header to use for creation */
        hdr_size = linfo_size +
                    ginfo_size +
                    (ginfo->est_num_entries * link_size);
    } /* end if */
    else 
        hdr_size = 4 + 2 * H5F_SIZEOF_ADDR(f);

    /*
     * Create group's object header.  It has a zero link count
     * since nothing refers to it yet.	The link count will be
     * incremented if the object is added to the group directed graph.
     */
    if(H5O_create(f, dxpl_id, hdr_size, oloc/*out*/) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't create header")

    /* Check for format of group to create */
    if(use_latest_format) {
        /* Insert link info message */
        if(H5O_modify(oloc, H5O_LINFO_ID, H5O_NEW_MESG, 0, 0, &linfo, dxpl_id) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't create message")

        /* Insert group info message */
        if(H5O_modify(oloc, H5O_GINFO_ID, H5O_NEW_MESG, H5O_FLAG_CONSTANT, H5O_UPDATE_TIME, ginfo, dxpl_id) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't create message")
    } /* end if */
    else {
        H5O_stab_t	stab;		/* Symbol table message	*/

        /* The group doesn't currently have a 'stab' message, go create one */
        if(H5G_stab_create(oloc, dxpl_id, ginfo, &stab) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to create symbol table")
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_obj_create() */


/*-------------------------------------------------------------------------
 * Function:    H5G_obj_ent_decode
 *
 * Purpose:     Decodes a symbol table entry into a object location
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
H5G_obj_ent_decode(H5F_t *f, const uint8_t **pp, H5O_loc_t *oloc)
{
    const uint8_t	*p_ret = *pp;

    FUNC_ENTER_NOAPI_NOFUNC(H5G_obj_ent_decode)

    /* check arguments */
    HDassert(f);
    HDassert(pp);
    HDassert(oloc);

    /* Set file pointer for root object location */
    oloc->file = f;
    oloc->holding_file = FALSE;

    /* decode header */
    *pp += H5F_SIZEOF_SIZE(f);          /* Skip over local heap address */
    H5F_addr_decode(f, pp, &(oloc->addr));
    *pp += 4;                           /* Skip over "cache type" */
    *pp += 4;                           /* Reserved */

    /* Set decode pointer */
    *pp = p_ret + H5G_SIZEOF_ENTRY(f);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5G_obj_ent_decode() */


/*-------------------------------------------------------------------------
 * Function:    H5G_obj_ent_encode
 *
 * Purpose:     Encodes the specified object location into a symbol table
 *              entry in the buffer pointed to by *pp.
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
H5G_obj_ent_encode(H5F_t *f, uint8_t **pp, const H5O_loc_t *oloc)
{
    uint8_t		*p_ret = *pp + H5G_SIZEOF_ENTRY(f);

    FUNC_ENTER_NOAPI_NOFUNC(H5G_obj_ent_encode)

    /* check arguments */
    HDassert(f);
    HDassert(pp);

    /* encode header */
    H5F_ENCODE_LENGTH(f, *pp, 0);           /* No name for root group */
    if(oloc)
        H5F_addr_encode(f, pp, oloc->addr);
    else
        H5F_addr_encode(f, pp, HADDR_UNDEF);
    UINT32ENCODE(*pp, H5G_NOTHING_CACHED);
    UINT32ENCODE(*pp, 0); /*reserved*/

    /* fill with zero */
    while(*pp < p_ret)
        *(*pp)++ = 0;
    *pp = p_ret;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5G_obj_ent_encode() */


/*-------------------------------------------------------------------------
 * Function:	H5G_obj_link_to_dense_cb
 *
 * Purpose:	Callback routine for converting 'link' messages to "dense"
 *              link storage form.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Aug 30 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5G_obj_link_to_dense_cb(const void *_mesg, unsigned UNUSED idx, void *_udata)
{
    const H5O_link_t *lnk = (const H5O_link_t *)_mesg;  /* Pointer to link */
    H5G_obj_oh_it_ud1_t *udata = (H5G_obj_oh_it_ud1_t *)_udata;     /* 'User data' passed in */
    herr_t ret_value = H5O_ITER_CONT;   /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5G_obj_link_to_dense_cb)

    /* check arguments */
    HDassert(lnk);
    HDassert(udata);

    /* Insert link into dense link storage */
    if(H5G_dense_insert(udata->f, udata->dxpl_id, udata->linfo, lnk) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINSERT, FAIL, "unable to insert link into dense storage")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_obj_link_to_dense_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5G_obj_stab_to_new_cb
 *
 * Purpose:	Callback routine for converting "symbol table" link storage to
 *              "new format" storage (either link messages or "dense" storage).
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Sept 16 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5G_obj_stab_to_new_cb(const H5O_link_t *lnk, void *_udata)
{
    H5G_obj_stab_it_ud1_t *udata = (H5G_obj_stab_it_ud1_t *)_udata;     /* 'User data' passed in */
    herr_t ret_value = H5B_ITER_CONT;   /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5G_obj_stab_to_new_cb)

    /* check arguments */
    HDassert(lnk);
    HDassert(udata);

    /* Insert link into group */
    /* (Casting away const OK - QAK) */
    if(H5G_obj_insert(udata->grp_oloc, lnk->name, (H5O_link_t *)lnk, FALSE, udata->dxpl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINSERT, H5B_ITER_ERROR, "can't insert link into group")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_obj_stab_to_new_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5G_obj_insert
 *
 * Purpose:	Insert a new symbol into the group described by GRP_OLOC.
 *		file F.	 The name of the new symbol is NAME and its symbol
 *		table entry is OBJ_LNK.  Increment the reference
 *              count for the object the link points if OBJ_LNK is a hard link
 *              and ADJ_LINK is true.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Sep  6 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_obj_insert(H5O_loc_t *grp_oloc, const char *name, H5O_link_t *obj_lnk,
    hbool_t adj_link, hid_t dxpl_id)
{
    H5O_linfo_t linfo;		/* Link info message */
    H5O_ginfo_t ginfo;		/* Group info message */
    hbool_t     use_old_format; /* Whether to use 'old format' (symbol table) for insertions or not */
    hbool_t     use_new_dense = FALSE;      /* Whether to use "dense" form of 'new format' group */
    herr_t     ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5G_obj_insert, FAIL)

    /* check arguments */
    HDassert(grp_oloc && grp_oloc->file);
    HDassert(name && *name);
    HDassert(obj_lnk);

    /* Check if we have information about the number of objects in this group */
    /* (by attempting to get the link info message for this group) */
    if(H5O_read(grp_oloc, H5O_LINFO_ID, 0, &linfo, dxpl_id)) {
        size_t link_msg_size;   /* Size of new link message in the file */

        /* Using the new format for groups */
        use_old_format = FALSE;

        /* Get the group info */
        if(NULL == H5O_read(grp_oloc, H5O_GINFO_ID, 0, &ginfo, dxpl_id))
            HGOTO_ERROR(H5E_SYM, H5E_BADMESG, FAIL, "can't get group info")

        /* Check for tracking creation order on this group's links */
        if(ginfo.track_corder) {
            /* Set the creation order for the new link & indicate that it's valid */
            obj_lnk->corder = linfo.max_corder;
            obj_lnk->corder_valid = TRUE;

            /* Increment the max. creation order used in the group */
            linfo.max_corder++;
        } /* end if */

        /* Get the link's message size */
        if((link_msg_size = H5O_raw_size(H5O_LINK_ID, grp_oloc->file, obj_lnk)) == 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTGETSIZE, FAIL, "can't get link size")

        /* If there's still a small enough number of links, use the 'link' message */
        /* (If the encoded form of the link is too large to fit into an object
         *  header message, convert to using dense link storage instead of link messages)
         */
        if(H5F_addr_defined(linfo.link_fheap_addr))
            use_new_dense = TRUE;
        else if(linfo.nlinks < ginfo.max_compact && link_msg_size < H5O_MAX_SIZE)
            use_new_dense = FALSE;
        else {
            H5G_obj_oh_it_ud1_t	udata;          /* User data for iteration */

            /* The group doesn't currently have "dense" storage for links */
            if(H5G_dense_create(grp_oloc->file, dxpl_id, &linfo) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to create 'dense' form of new format group")

            /* Set up user data for object header message iteration */
            udata.f = grp_oloc->file;
            udata.dxpl_id = dxpl_id;
            udata.linfo = &linfo;

            /* Iterate over the 'link' messages, inserting them into the dense link storage  */
            if(H5O_iterate(grp_oloc, H5O_LINK_ID, H5G_obj_link_to_dense_cb, &udata, dxpl_id) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "error iterating over links")

            /* Remove all the 'link' messages */
            if(H5O_remove(grp_oloc, H5O_LINK_ID, H5O_ALL, FALSE, dxpl_id) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTDELETE, FAIL, "unable to delete link messages")

            use_new_dense = TRUE;
        } /* end else */
    } /* end if */
    else {
        /* Clear error stack from not finding the link info message */
        H5E_clear_stack(NULL);

        /* Check for new-style link information */
        if(obj_lnk->cset != H5T_CSET_ASCII || obj_lnk->type > H5L_TYPE_BUILTIN_MAX) {
            H5O_linfo_t new_linfo = H5G_CRT_LINK_INFO_DEF;  /* Link information */
            H5O_ginfo_t new_ginfo = H5G_CRT_GROUP_INFO_DEF; /* Group information */
            H5G_obj_stab_it_ud1_t udata;        /* User data for iteration */
            H5G_link_iterate_t lnk_op;          /* Link operator */

            /* Convert group to "new format" group, in order to hold the information */

            /* Insert link info message */
            if(H5O_modify(grp_oloc, H5O_LINFO_ID, H5O_NEW_MESG, 0, 0, &new_linfo, dxpl_id) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't create message")

            /* Insert group info message */
            if(H5O_modify(grp_oloc, H5O_GINFO_ID, H5O_NEW_MESG, H5O_FLAG_CONSTANT, 0, &new_ginfo, dxpl_id) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't create message")

            /* Set up user data for iteration */
            udata.grp_oloc = grp_oloc;
            udata.dxpl_id = dxpl_id;

            /* Build iterator operator */
            lnk_op.lib_op = H5G_obj_stab_to_new_cb;

            /* Iterate through all links in "old format" group and insert them into new format */
            if(H5G_stab_iterate(grp_oloc, H5_ITER_NATIVE, 0, TRUE, 0, NULL, lnk_op, &udata, dxpl_id) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTNEXT, FAIL, "error iterating over old format links")

            /* Remove the symbol table message from the group */
            if(H5O_remove(grp_oloc, H5O_STAB_ID, 0, FALSE, dxpl_id) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTDELETE, FAIL, "unable to delete old format link storage")

            /* Recursively call this routine to insert the new link, since the
             *  group is in the "new format" now and the link info should be
             *  set up, etc.
             */
            if(H5G_obj_insert(grp_oloc, name, obj_lnk, adj_link, dxpl_id) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTINSERT, FAIL, "unable to insert link into group")

            /* Done with insertion now */
            HGOTO_DONE(SUCCEED)
        } /* end if */
        else
            use_old_format = TRUE;
    } /* end if */

    /* Insert into symbol table or "dense" storage */
    if(use_old_format) {
        /* Insert into symbol table */
        if(H5G_stab_insert(grp_oloc, name, obj_lnk, dxpl_id) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINSERT, FAIL, "unable to insert entry into symbol table")
    } /* end if */
    else {
        if(use_new_dense) {
            /* Insert into dense link storage */
            if(H5G_dense_insert(grp_oloc->file, dxpl_id, &linfo, obj_lnk) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTINSERT, FAIL, "unable to insert link into dense storage")
        } /* end if */
        else {
            /* Insert with link message */
            if(H5G_link_insert(grp_oloc, obj_lnk, dxpl_id) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTINSERT, FAIL, "unable to insert link as link message")
        } /* end else */
    } /* end else */

    /* Increment the number of objects in this group */
    if(!use_old_format) {
        linfo.nlinks++;
        if(H5O_modify(grp_oloc, H5O_LINFO_ID, 0, 0, H5O_UPDATE_TIME, &linfo, dxpl_id) < 0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTINIT, FAIL, "can't update link info message")
    } /* end if */

    /* Increment link count on object, if requested and it's a hard link */
    if(adj_link && obj_lnk->type == H5L_TYPE_HARD) {
        H5O_loc_t obj_oloc;             /* Object location */
        H5O_loc_reset(&obj_oloc);

        /* Create temporary object location */
        obj_oloc.file = grp_oloc->file;
        obj_oloc.addr = obj_lnk->u.hard.addr;

        /* Increment reference count for object */
        if(H5O_link(&obj_oloc, 1, dxpl_id) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_LINKCOUNT, FAIL, "unable to increment hard link count")
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_obj_insert() */


/*-------------------------------------------------------------------------
 * Function:	H5G_obj_iterate
 *
 * Purpose:     Private function for H5Giterate.
 *              Iterates over objects in a group
 *
 * Return:	Success:        Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *	        Oct  3, 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_obj_iterate(hid_t loc_id, const char *name, H5_iter_order_t order,
    int skip, int *last_lnk, H5G_iterate_t op, void *op_data, hid_t dxpl_id)
{
    H5O_linfo_t	linfo;		/* Link info message */
    H5G_link_iterate_t lnk_op;  /* Link operator */
    hid_t gid = -1;             /* ID of group to iterate over */
    H5G_t *grp;                 /* Pointer to group data structure to iterate over */
    herr_t ret_value;           /* Return value */

    FUNC_ENTER_NOAPI(H5G_obj_iterate, FAIL)

    /* Sanity check */
    HDassert(name);
    HDassert(last_lnk);
    HDassert(op);

    /*
     * Open the group on which to operate.  We also create a group ID which
     * we can pass to the application-defined operator.
     */
    if((gid = H5Gopen(loc_id, name)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, FAIL, "unable to open group")
    if((grp = H5I_object(gid)) == NULL)
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "bad group ID")

    /* Set up link operator */
    lnk_op.app_op = op;

    /* Attempt to get the link info for this group */
    if(H5O_read(&(grp->oloc), H5O_LINFO_ID, 0, &linfo, dxpl_id)) {
        /* Check for going out of bounds */
        if(skip > 0 && (size_t)skip >= linfo.nlinks)
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "index out of bound")

        if(H5F_addr_defined(linfo.link_fheap_addr)) {
            /* Iterate over the links in the group, building a table of the link messages */
            if((ret_value = H5G_dense_iterate(grp->oloc.file, dxpl_id, order, gid, &linfo, FALSE, skip, last_lnk, lnk_op, op_data)) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTNEXT, FAIL, "error iterating over links")
        } /* end if */
        else {
            /* Get the object's name from the link messages */
            if((ret_value = H5G_link_iterate(&(grp->oloc), dxpl_id, &linfo, order, gid, FALSE, skip, last_lnk, lnk_op, op_data)) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_BADITER, FAIL, "can't iterate over links")
        } /* end else */
    } /* end if */
    else {
        /* Clear error stack from not finding the link info message */
        H5E_clear_stack(NULL);

        /* Iterate over symbol table */
        if((ret_value = H5G_stab_iterate(&(grp->oloc), order, gid, FALSE, skip, last_lnk, lnk_op, op_data, dxpl_id)) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_BADITER, FAIL, "can't iterate over symbol table")
    } /* end else */

done:
    if(gid > 0)
        H5I_dec_ref(gid); /*also closes 'grp'*/

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_obj_iterate() */


/*-------------------------------------------------------------------------
 * Function:	H5G_obj_count
 *
 * Purpose:	Check the number of objects in a group
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Sep  6 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_obj_count(H5O_loc_t *oloc, hsize_t *num_objs, hid_t dxpl_id)
{
    H5O_linfo_t	linfo;		        /* Link info message */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(H5G_obj_count, FAIL)

    /* Sanity check */
    HDassert(oloc);
    HDassert(num_objs);

    /* Attempt to get the link info for this group */
    if(H5O_read(oloc, H5O_LINFO_ID, 0, &linfo, dxpl_id)) {
        /* Set the number of objects */
        *num_objs = linfo.nlinks;
    } /* end if */
    else {
        /* Clear error stack from not finding the link info message */
        H5E_clear_stack(NULL);

        /* Get the number of objects in this group by iterating over symbol table */
        if(H5G_stab_count(oloc, num_objs, dxpl_id) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTCOUNT, FAIL, "can't count objects")
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_obj_count() */


/*-------------------------------------------------------------------------
 * Function:	H5G_obj_get_name_by_idx
 *
 * Purpose:     Private function for H5Gget_objname_by_idx.
 *              Returns the name of objects in the group by giving index.
 *
 * Return:	Success:        Non-negative, length of name
 *		Failure:	Negative
 *
 * Programmer:	Raymond Lu
 *	        Nov 20, 2002
 *
 *-------------------------------------------------------------------------
 */
ssize_t
H5G_obj_get_name_by_idx(H5O_loc_t *oloc, hsize_t idx, char* name, size_t size,
    hid_t dxpl_id)
{
    H5O_linfo_t	linfo;		/* Link info message */
    ssize_t ret_value;          /* Return value */

    FUNC_ENTER_NOAPI(H5G_obj_get_name_by_idx, FAIL)

    /* Sanity check */
    HDassert(oloc);

    /* Attempt to get the link info for this group */
    if(H5O_read(oloc, H5O_LINFO_ID, 0, &linfo, dxpl_id)) {
        if(H5F_addr_defined(linfo.link_fheap_addr)) {
            /* Get the object's name from the dense link storage */
            if((ret_value = H5G_dense_get_name_by_idx(oloc->file, dxpl_id, &linfo, idx, name, size)) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "can't locate name")
        } /* end if */
        else {
            /* Get the object's name from the link messages */
            if((ret_value = H5G_link_get_name_by_idx(oloc, dxpl_id, &linfo, idx, name, size)) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "can't locate name")
        } /* end else */
    } /* end if */
    else {
        /* Clear error stack from not finding the link info message */
        H5E_clear_stack(NULL);

        /* Get the object's name from the symbol table */
        if((ret_value = H5G_stab_get_name_by_idx(oloc, idx, name, size, dxpl_id)) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "can't locate name")
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_obj_get_name_by_idx() */


/*-------------------------------------------------------------------------
 * Function:	H5G_obj_get_type_by_idx
 *
 * Purpose:     Private function for H5Gget_objtype_by_idx.
 *              Returns the type of objects in the group by giving index.
 *
 * Return:	Success:        H5G_GROUP(1), H5G_DATASET(2), H5G_TYPE(3)
 *
 *		Failure:	Negative
 *
 * Programmer:	Raymond Lu
 *	        Nov 20, 2002
 *
 *-------------------------------------------------------------------------
 */
H5G_obj_t
H5G_obj_get_type_by_idx(H5O_loc_t *oloc, hsize_t idx, hid_t dxpl_id)
{
    H5O_linfo_t	linfo;		/* Link info message */
    H5G_obj_t ret_value;        /* Return value */

    FUNC_ENTER_NOAPI(H5G_obj_get_type_by_idx, H5G_UNKNOWN)

    /* Sanity check */
    HDassert(oloc);

    /* Attempt to get the link info for this group */
    if(H5O_read(oloc, H5O_LINFO_ID, 0, &linfo, dxpl_id)) {
        if(H5F_addr_defined(linfo.link_fheap_addr)) {
            /* Get the object's name from the dense link storage */
            if((ret_value = H5G_dense_get_type_by_idx(oloc->file, dxpl_id, &linfo, idx)) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, H5G_UNKNOWN, "can't locate type")
        } /* end if */
        else {
            /* Get the object's type from the link messages */
            if((ret_value = H5G_link_get_type_by_idx(oloc, dxpl_id, &linfo, idx)) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, H5G_UNKNOWN, "can't locate type")
        } /* end else */
    } /* end if */
    else {
        /* Clear error stack from not finding the link info message */
        H5E_clear_stack(NULL);

        /* Get the object's type from the symbol table */
        if((ret_value = H5G_stab_get_type_by_idx(oloc, idx, dxpl_id)) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, H5G_UNKNOWN, "can't locate type")
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_obj_get_type_by_idx() */


/*-------------------------------------------------------------------------
 * Function:	H5G_obj_remove
 *
 * Purpose:     Remove an object from a group.
 *
 * Note:	Needs to hand up the type of the object removed
 *
 * Return:	Success:        Non-negative
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *	        Sep 19, 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_obj_remove(H5O_loc_t *oloc, const char *name, H5G_obj_t *obj_type, hid_t dxpl_id)
{
    H5O_linfo_t	linfo;		/* Link info message            */
    hbool_t     use_old_format; /* Whether to use 'old format' (symbol table) for deletion or not */
    hbool_t     use_new_dense = FALSE;      /* Whether to use "dense" form of 'new format' group */
    herr_t	ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI(H5G_obj_remove, FAIL)

    /* Sanity check */
    HDassert(oloc);
    HDassert(name && *name);
    HDassert(obj_type);

    /* Attempt to get the link info for this group */
    if(H5O_read(oloc, H5O_LINFO_ID, 0, &linfo, dxpl_id)) {
        /* Using the new format for groups */
        use_old_format = FALSE;

        /* Check for deleting enough links from group to go back to link messages */
        if(H5F_addr_defined(linfo.link_fheap_addr)) {
            H5O_ginfo_t ginfo;		/* Group info message            */

            /* Get the group info */
            if(NULL == H5O_read(oloc, H5O_GINFO_ID, 0, &ginfo, dxpl_id))
                HGOTO_ERROR(H5E_SYM, H5E_BADMESG, FAIL, "can't get group info")

            /* Check if we should switch from dense storage back to link messages */
            if(linfo.nlinks <= ginfo.min_dense) {
                H5G_link_table_t ltable;        /* Table of links */
                hbool_t can_convert = TRUE;     /* Whether converting to link messages is possible */
                size_t u;                       /* Local index */

                /* Build the table of links for this group */
                if(H5G_dense_build_table(oloc->file, dxpl_id, &linfo, H5_ITER_NATIVE, &ltable) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTNEXT, FAIL, "error iterating over links")

                /* Inspect links in table for ones that can't be converted back
                 * into link message form (currently only links which can't fit
                 * into an object header message)
                 */
                for(u = 0; u < linfo.nlinks; u++)
                    if(H5O_mesg_size(H5O_LINK_ID, oloc->file, &(ltable.lnks[u]), (size_t)0) >= H5O_MAX_SIZE) {
                        can_convert = FALSE;
                        break;
                    } /* end if */

                /* If ok, insert links as link messages */
                if(can_convert) {
                    /* Insert link messages into group */
                    for(u = 0; u < linfo.nlinks; u++)
                        if(H5O_modify(oloc, H5O_LINK_ID, H5O_NEW_MESG, 0, H5O_UPDATE_TIME, &(ltable.lnks[u]), dxpl_id) < 0)
                            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't create message")

                    /* Remove the dense storage */
                    if(H5G_dense_delete(oloc->file, dxpl_id, &linfo, FALSE) < 0)
                        HGOTO_ERROR(H5E_SYM, H5E_CANTDELETE, FAIL, "unable to delete dense link storage")

                    use_new_dense = FALSE;
                } /* end if */
                else
                    use_new_dense = TRUE;

                /* Free link table information */
                if(H5G_obj_release_table(&ltable) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "unable to release link table")
            } /* end if */
            else
                use_new_dense = TRUE;
        } /* end if */
        else
            use_new_dense = FALSE;
    } /* end if */
    else {
        H5E_clear_stack(NULL);  /* Clear error stack from not finding the link info message */
        use_old_format = TRUE;
    } /* end else */

    /* If the symbol table doesn't exist, search link messages */
    if(use_old_format) {
        /* Remove object from the symbol table */
        if(H5G_stab_remove(oloc, name, obj_type, dxpl_id) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "can't remove object")
    } /* end if */
    else {
        if(use_new_dense) {
            /* Remove object from the dense link storage */
            if(H5G_dense_remove(oloc->file, dxpl_id, &linfo, name, obj_type) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "can't remove object")
        } /* end if */
        else {
            /* Remove object from the link messages */
            if(H5G_link_remove(oloc, name, obj_type, dxpl_id) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "can't remove object")
        } /* end else */
    } /* end else */

    /* Update link info for a new-style group */
    if(!use_old_format) {
        /* Decrement # of links in group */
        linfo.nlinks--;

        /* Remove the dense link storage, if we are using it and the number of links drops to zero */
        if(linfo.nlinks == 0 && use_new_dense) {
            if(H5G_dense_delete(oloc->file, dxpl_id, &linfo, FALSE) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTDELETE, FAIL, "unable to delete dense link storage")
        } /* end if */

        /* Update link info in the object header */
        if(H5O_modify(oloc, H5O_LINFO_ID, 0, 0, H5O_UPDATE_TIME, &linfo, dxpl_id) < 0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTINIT, FAIL, "can't update link info message")
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_obj_remove() */


/*-------------------------------------------------------------------------
 * Function:	H5G_obj_lookup
 *
 * Purpose:	Look up a link in a group.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Sep 26 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_obj_lookup(H5O_loc_t *grp_oloc, const char *name, H5O_link_t *lnk,
    hid_t dxpl_id)
{
    H5O_linfo_t linfo;		        /* Link info message */
    herr_t     ret_value = SUCCEED;     /* Return value */

    FUNC_ENTER_NOAPI(H5G_obj_lookup, FAIL)

    /* check arguments */
    HDassert(grp_oloc && grp_oloc->file);
    HDassert(name && *name);

    /* Attempt to get the link info message for this group */
    if(H5O_read(grp_oloc, H5O_LINFO_ID, 0, &linfo, dxpl_id)) {
        /* Check for dense link storage */
        if(H5F_addr_defined(linfo.link_fheap_addr)) {
            /* Get the object's info from the dense link storage */
            if(H5G_dense_lookup(grp_oloc->file, dxpl_id, &linfo, name, lnk) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "can't locate object")
        } /* end if */
        else {
            /* Get the object's info from the link messages */
            if(H5G_link_lookup(grp_oloc, name, lnk, dxpl_id) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "can't locate object")
        } /* end else */
    } /* end if */
    else {
        /* Clear error stack from not finding the link info message */
        H5E_clear_stack(NULL);

        /* Get the object's info from the symbol table */
        if(H5G_stab_lookup(grp_oloc, name, lnk, dxpl_id) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "can't locate object")
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_obj_lookup() */

