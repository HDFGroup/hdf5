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
static herr_t H5G_obj_compact_to_dense_cb(const void *_mesg, unsigned idx,
    void *_udata);
static herr_t H5G_obj_remove_update_linfo(H5O_loc_t *oloc, H5O_linfo_t *linfo,
    hid_t dxpl_id);


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
    const H5O_linfo_t *linfo, hid_t gcpl_id, H5O_loc_t *oloc/*out*/)
{
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

    /* Make certain that the creation order is being tracked if an index is
     *  going to be built on it.
     */
    if(linfo->index_corder && !ginfo->track_corder)
	HGOTO_ERROR(H5E_SYM, H5E_BADVALUE, FAIL, "must track creation order to create index for it")

    /* Check if we should be using the latest version of the group format */
    if(use_latest_format) {
        H5O_link_t lnk;                     /* Temporary link message info for computing message size */
        char null_char = '\0';              /* Character for creating null string */
        size_t ginfo_size;                  /* Size of the group info message */
        size_t linfo_size;                  /* Size of the link info message */
        size_t link_size;                   /* Size of a link message */

        /* Calculate message size infomation, for creating group's object header */
        linfo_size = H5O_mesg_size(H5O_LINFO_ID, f, linfo, (size_t)0);
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
    if(H5O_create(f, dxpl_id, hdr_size, gcpl_id, oloc/*out*/) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't create header")

    /* Check for format of group to create */
    if(use_latest_format) {
        /* Insert link info message */
        if(H5O_msg_create(oloc, H5O_LINFO_ID, 0, 0, linfo, dxpl_id) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't create message")

        /* Insert group info message */
        if(H5O_msg_create(oloc, H5O_GINFO_ID, H5O_MSG_FLAG_CONSTANT, H5O_UPDATE_TIME, ginfo, dxpl_id) < 0)
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
 * Function:	H5G_obj_compact_to_dense_cb
 *
 * Purpose:	Callback routine for converting "compact" to "dense"
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
H5G_obj_compact_to_dense_cb(const void *_mesg, unsigned UNUSED idx, void *_udata)
{
    const H5O_link_t *lnk = (const H5O_link_t *)_mesg;  /* Pointer to link */
    H5G_obj_oh_it_ud1_t *udata = (H5G_obj_oh_it_ud1_t *)_udata;     /* 'User data' passed in */
    herr_t ret_value = H5_ITER_CONT;   /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5G_obj_compact_to_dense_cb)

    /* check arguments */
    HDassert(lnk);
    HDassert(udata);

    /* Insert link into dense link storage */
    if(H5G_dense_insert(udata->f, udata->dxpl_id, udata->linfo, lnk) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINSERT, FAIL, "unable to insert link into dense storage")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_obj_compact_to_dense_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5G_obj_stab_to_new_cb
 *
 * Purpose:	Callback routine for converting "symbol table" link storage to
 *              "new format" storage (either "compact" or "dense" storage).
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
    herr_t ret_value = H5_ITER_CONT;   /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5G_obj_stab_to_new_cb)

    /* check arguments */
    HDassert(lnk);
    HDassert(udata);

    /* Insert link into group */
    /* (Casting away const OK - QAK) */
    if(H5G_obj_insert(udata->grp_oloc, lnk->name, (H5O_link_t *)lnk, FALSE, udata->dxpl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINSERT, H5_ITER_ERROR, "can't insert link into group")

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
        else if(linfo.nlinks < ginfo.max_compact && link_msg_size < H5O_MESG_MAX_SIZE)
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
            if(H5O_iterate(grp_oloc, H5O_LINK_ID, H5G_obj_compact_to_dense_cb, &udata, dxpl_id) < 0)
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
            if(H5O_msg_create(grp_oloc, H5O_LINFO_ID, 0, 0, &new_linfo, dxpl_id) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't create message")

            /* Insert group info message */
            if(H5O_msg_create(grp_oloc, H5O_GINFO_ID, H5O_MSG_FLAG_CONSTANT, H5O_UPDATE_TIME, &new_ginfo, dxpl_id) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't create message")

            /* Set up user data for iteration */
            udata.grp_oloc = grp_oloc;
            udata.dxpl_id = dxpl_id;

            /* Build iterator operator */
            lnk_op.op_type = H5G_LINK_OP_LIB;
            lnk_op.u.lib_op = H5G_obj_stab_to_new_cb;

            /* Iterate through all links in "old format" group and insert them into new format */
            if(H5G_stab_iterate(grp_oloc, dxpl_id, H5_ITER_NATIVE, (hsize_t)0, NULL, (hid_t)0, &lnk_op, &udata) < 0)
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
            if(H5G_compact_insert(grp_oloc, obj_lnk, dxpl_id) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTINSERT, FAIL, "unable to insert link as link message")
        } /* end else */
    } /* end else */

    /* Increment the number of objects in this group */
    if(!use_old_format) {
        linfo.nlinks++;
        if(H5O_write(grp_oloc, H5O_LINFO_ID, 0, 0, H5O_UPDATE_TIME, &linfo, dxpl_id) < 0)
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
H5G_obj_iterate(hid_t loc_id, const char *group_name,
    H5L_index_t idx_type, H5_iter_order_t order, hsize_t skip, hsize_t *last_lnk,
    H5G_link_iterate_t *lnk_op, void *op_data, hid_t dxpl_id)
{
    H5O_linfo_t	linfo;		/* Link info message */
    hid_t gid = -1;             /* ID of group to iterate over */
    H5G_t *grp;                 /* Pointer to group data structure to iterate over */
    herr_t ret_value;           /* Return value */

    FUNC_ENTER_NOAPI(H5G_obj_iterate, FAIL)

    /* Sanity check */
    HDassert(group_name);
    HDassert(last_lnk);
    HDassert(lnk_op && lnk_op->u.lib_op);

    /*
     * Open the group on which to operate.  We also create a group ID which
     * we can pass to the application-defined operator.
     */
    if((gid = H5Gopen(loc_id, group_name)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, FAIL, "unable to open group")
    if((grp = H5I_object(gid)) == NULL)
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "bad group ID")

    /* Attempt to get the link info for this group */
    if(H5O_read(&(grp->oloc), H5O_LINFO_ID, 0, &linfo, dxpl_id)) {
        /* Check for going out of bounds */
        if(skip > 0 && (size_t)skip >= linfo.nlinks)
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "index out of bound")

        /* Check for creation order tracking, if creation order index lookup requested */
        if(idx_type == H5L_INDEX_CRT_ORDER) {
            H5O_ginfo_t ginfo;		        /* Group info message */

            /* Get group info message, to see if creation order is tracked for links in this group */
            if(NULL == H5O_read(&(grp->oloc), H5O_GINFO_ID, 0, &ginfo, dxpl_id))
                HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't retrieve group info message for group")

            /* Check if creation order is tracked */
            if(!ginfo.track_corder)
                HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "creation order not tracked for links in group")
        } /* end if */

        if(H5F_addr_defined(linfo.link_fheap_addr)) {
            /* Iterate over the links in the group, building a table of the link messages */
            if((ret_value = H5G_dense_iterate(grp->oloc.file, dxpl_id, &linfo, idx_type, order, skip, last_lnk, gid, lnk_op, op_data)) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTNEXT, FAIL, "error iterating over links")
        } /* end if */
        else {
            /* Get the object's name from the link messages */
            if((ret_value = H5G_compact_iterate(&(grp->oloc), dxpl_id, &linfo, idx_type, order, skip, last_lnk, gid, lnk_op, op_data)) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_BADITER, FAIL, "can't iterate over links")
        } /* end else */
    } /* end if */
    else {
        /* Clear error stack from not finding the link info message */
        H5E_clear_stack(NULL);

        /* Can only perform name lookups on groups with symbol tables */
        if(idx_type != H5L_INDEX_NAME)
            HGOTO_ERROR(H5E_SYM, H5E_BADVALUE, FAIL, "no creation order index to query")

        /* Iterate over symbol table */
        if((ret_value = H5G_stab_iterate(&(grp->oloc), dxpl_id, order, skip, last_lnk, gid, lnk_op, op_data)) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_BADITER, FAIL, "can't iterate over symbol table")
    } /* end else */

done:
    if(gid > 0)
        H5I_dec_ref(gid); /*also closes 'grp'*/

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_obj_iterate() */


/*-------------------------------------------------------------------------
 * Function:	H5G_obj_info
 *
 * Purpose:	Retrieve information about a group
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Nov 27 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_obj_info(H5O_loc_t *oloc, H5G_info_t *grp_info, hid_t dxpl_id)
{
    H5O_linfo_t	linfo;		        /* Link info message */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(H5G_obj_info, FAIL)

    /* Sanity check */
    HDassert(oloc);
    HDassert(grp_info);

    /* Attempt to get the link info for this group */
    if(H5O_read(oloc, H5O_LINFO_ID, 0, &linfo, dxpl_id)) {
        /* Retrieve the information about the links */
        grp_info->nlinks = linfo.nlinks;
        grp_info->min_corder = linfo.min_corder;
        grp_info->max_corder = linfo.max_corder;

        /* Check if the group is using compact or dense storage for its links */
        if(H5F_addr_defined(linfo.link_fheap_addr))
            grp_info->storage_type = H5G_STORAGE_TYPE_DENSE;
        else
            grp_info->storage_type = H5G_STORAGE_TYPE_COMPACT;
    } /* end if */
    else {
        /* Clear error stack from not finding the link info message */
        H5E_clear_stack(NULL);

        /* Get the number of objects in this group by iterating over symbol table */
        if(H5G_stab_count(oloc, &grp_info->nlinks, dxpl_id) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTCOUNT, FAIL, "can't count objects")

        /* Set the other information about the group */
        grp_info->storage_type = H5G_STORAGE_TYPE_SYMBOL_TABLE;
        grp_info->min_corder = 0;
        grp_info->max_corder = 0;
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_obj_info() */


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
H5G_obj_get_name_by_idx(H5O_loc_t *oloc, H5L_index_t idx_type,
    H5_iter_order_t order, hsize_t n, char* name, size_t size, hid_t dxpl_id)
{
    H5O_linfo_t	linfo;		/* Link info message */
    ssize_t ret_value;          /* Return value */

    FUNC_ENTER_NOAPI(H5G_obj_get_name_by_idx, FAIL)

    /* Sanity check */
    HDassert(oloc && oloc->file);

    /* Attempt to get the link info for this group */
    if(H5O_read(oloc, H5O_LINFO_ID, 0, &linfo, dxpl_id)) {
        /* Check for creation order tracking, if creation order index lookup requested */
        if(idx_type == H5L_INDEX_CRT_ORDER) {
            H5O_ginfo_t ginfo;		        /* Group info message */

            /* Get group info message, to see if creation order is tracked for links in this group */
            if(NULL == H5O_read(oloc, H5O_GINFO_ID, 0, &ginfo, dxpl_id))
                HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't retrieve group info message for group")

            /* Check if creation order is tracked */
            if(!ginfo.track_corder)
                HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "creation order not tracked for links in group")
        } /* end if */

        /* Check for dense link storage */
        if(H5F_addr_defined(linfo.link_fheap_addr)) {
            /* Get the object's name from the dense link storage */
            if((ret_value = H5G_dense_get_name_by_idx(oloc->file, dxpl_id, &linfo, idx_type, order, n, name, size)) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "can't locate name")
        } /* end if */
        else {
            /* Get the object's name from the link messages */
            if((ret_value = H5G_compact_get_name_by_idx(oloc, dxpl_id, &linfo, idx_type, order, n, name, size)) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "can't locate name")
        } /* end else */
    } /* end if */
    else {
        /* Clear error stack from not finding the link info message */
        H5E_clear_stack(NULL);

        /* Can only perform name lookups on groups with symbol tables */
        if(idx_type != H5L_INDEX_NAME)
            HGOTO_ERROR(H5E_SYM, H5E_BADVALUE, FAIL, "no creation order index to query")

        /* Get the object's name from the symbol table */
        if((ret_value = H5G_stab_get_name_by_idx(oloc, order, n, name, size, dxpl_id)) < 0)
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
            if((ret_value = H5G_compact_get_type_by_idx(oloc, dxpl_id, &linfo, idx)) < 0)
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
 * Function:	H5G_obj_remove_update_linfo
 *
 * Purpose:     Update the link info after removing a link from a group
 *
 * Return:	Success:        Non-negative
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *	        Nov 14, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5G_obj_remove_update_linfo(H5O_loc_t *oloc, H5O_linfo_t *linfo, hid_t dxpl_id)
{
    herr_t	ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5G_obj_remove_update_linfo)

    /* Sanity check */
    HDassert(oloc);
    HDassert(linfo);

    /* Decrement # of links in group */
    linfo->nlinks--;

    /* Reset the creation order min/max if there's no more links in group */
    if(linfo->nlinks == 0)
        linfo->min_corder = linfo->max_corder = 0;

    /* Check for transitioning out of dense storage, if we are using it */
    if(H5F_addr_defined(linfo->link_fheap_addr)) {
        /* Check if there's no more links */
        if(linfo->nlinks == 0) {
            /* Delete the dense storage */
            if(H5G_dense_delete(oloc->file, dxpl_id, linfo, FALSE) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTDELETE, FAIL, "unable to delete dense link storage")
        } /* end if */
        /* Check for switching back to compact storage */
        else {
            H5O_ginfo_t ginfo;		/* Group info message            */

            /* Get the group info */
            if(NULL == H5O_read(oloc, H5O_GINFO_ID, 0, &ginfo, dxpl_id))
                HGOTO_ERROR(H5E_SYM, H5E_BADMESG, FAIL, "can't get group info")

            /* Check if we should switch from dense storage back to link messages */
            if(linfo->nlinks < ginfo.min_dense) {
                H5G_link_table_t ltable;        /* Table of links */
                hbool_t can_convert = TRUE;     /* Whether converting to link messages is possible */
                size_t u;                       /* Local index */

                /* Build the table of links for this group */
                if(H5G_dense_build_table(oloc->file, dxpl_id, linfo, H5L_INDEX_NAME, H5_ITER_NATIVE, &ltable) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTNEXT, FAIL, "error iterating over links")

                /* Inspect links in table for ones that can't be converted back
                 * into link message form (currently only links which can't fit
                 * into an object header message)
                 */
                for(u = 0; u < linfo->nlinks; u++)
                    if(H5O_mesg_size(H5O_LINK_ID, oloc->file, &(ltable.lnks[u]), (size_t)0) >= H5O_MESG_MAX_SIZE) {
                        can_convert = FALSE;
                        break;
                    } /* end if */

                /* If ok, insert links as link messages */
                if(can_convert) {
                    struct H5O_t *oh = NULL;      /* Pointer to group's object header */
                    unsigned oh_flags = H5AC__DIRTIED_FLAG;

                    /* Get a pointer to the object header itself */
                    if((oh = H5O_protect(oloc, dxpl_id)) == NULL)
                        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to protect dataset object header")

                    /* Insert link messages into group */
                    for(u = 0; u < linfo->nlinks; u++)
                        if(H5O_append(oloc->file, dxpl_id, oh, H5O_LINK_ID, 0, H5O_UPDATE_TIME, &(ltable.lnks[u]), &oh_flags) < 0) {
                            /* Release object header */
                            if(H5O_unprotect(oloc, oh, dxpl_id, oh_flags) < 0)
                                HDONE_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to unprotect dataset object header")

                            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't create message")
                        } /* end if */

                    /* Release object header */
                    if(H5O_unprotect(oloc, oh, dxpl_id, oh_flags) < 0)
                        HDONE_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to unprotect dataset object header")

                    /* Remove the dense storage */
                    if(H5G_dense_delete(oloc->file, dxpl_id, linfo, FALSE) < 0)
                        HGOTO_ERROR(H5E_SYM, H5E_CANTDELETE, FAIL, "unable to delete dense link storage")
                } /* end if */

                /* Free link table information */
                if(H5G_link_release_table(&ltable) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "unable to release link table")
            } /* end if */
        } /* end else */
    } /* end if */

    /* Update link info in the object header */
    if(H5O_write(oloc, H5O_LINFO_ID, 0, 0, H5O_UPDATE_TIME, linfo, dxpl_id) < 0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTINIT, FAIL, "can't update link info message")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_obj_remove_update_linfo() */


/*-------------------------------------------------------------------------
 * Function:	H5G_obj_remove
 *
 * Purpose:     Remove a link from a group.
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
H5G_obj_remove(H5O_loc_t *oloc, H5RS_str_t *grp_full_path_r, const char *name, hid_t dxpl_id)
{
    H5O_linfo_t	linfo;		/* Link info message            */
    hbool_t     use_old_format; /* Whether to use 'old format' (symbol table) for deletion or not */
    herr_t	ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI(H5G_obj_remove, FAIL)

    /* Sanity check */
    HDassert(oloc);
    HDassert(name && *name);

    /* Attempt to get the link info for this group */
    if(H5O_read(oloc, H5O_LINFO_ID, 0, &linfo, dxpl_id)) {
        /* Using the new format for groups */
        use_old_format = FALSE;

        /* Check for dense or compact storage */
        if(H5F_addr_defined(linfo.link_fheap_addr)) {
            /* Remove object from the dense link storage */
            if(H5G_dense_remove(oloc->file, dxpl_id, &linfo, grp_full_path_r, name) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "can't remove object")
        } /* end if */
        else {
            /* Remove object from the link messages */
            if(H5G_compact_remove(oloc, dxpl_id, grp_full_path_r, name) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "can't remove object")
        } /* end else */
    } /* end if */
    else {
        /* Clear error stack from not finding the link info message */
        H5E_clear_stack(NULL);

        /* Using the old format for groups */
        use_old_format = TRUE;

        /* Remove object from the symbol table */
        if(H5G_stab_remove(oloc, dxpl_id, grp_full_path_r, name) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "can't remove object")
    } /* end else */

    /* Update link info for a new-style group */
    if(!use_old_format) {
        if(H5G_obj_remove_update_linfo(oloc, &linfo, dxpl_id) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTUPDATE, FAIL, "unable to update link info")
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_obj_remove() */


/*-------------------------------------------------------------------------
 * Function:	H5G_obj_remove_by_idx
 *
 * Purpose:     Remove a link from a group, according to the order within an index.
 *
 * Return:	Success:        Non-negative
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *	        Nov 14, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_obj_remove_by_idx(H5O_loc_t *grp_oloc, H5RS_str_t *grp_full_path_r,
    H5L_index_t idx_type, H5_iter_order_t order, hsize_t n, hid_t dxpl_id)
{
    H5O_linfo_t	linfo;		/* Link info message            */
    hbool_t     use_old_format; /* Whether to use 'old format' (symbol table) for deletion or not */
    herr_t	ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI(H5G_obj_remove_by_idx, FAIL)

    /* Sanity check */
    HDassert(grp_oloc && grp_oloc->file);

    /* Attempt to get the link info for this group */
    if(H5O_read(grp_oloc, H5O_LINFO_ID, 0, &linfo, dxpl_id)) {
        /* Check for creation order tracking, if creation order index lookup requested */
        if(idx_type == H5L_INDEX_CRT_ORDER) {
            H5O_ginfo_t ginfo;		        /* Group info message */

            /* Get group info message, to see if creation order is tracked for links in this group */
            if(NULL == H5O_read(grp_oloc, H5O_GINFO_ID, 0, &ginfo, dxpl_id))
                HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't retrieve group info message for group")

            /* Check if creation order is tracked */
            if(!ginfo.track_corder)
                HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "creation order not tracked for links in group")
        } /* end if */

        /* Using the new format for groups */
        use_old_format = FALSE;

        /* Check for dense or compact storage */
        if(H5F_addr_defined(linfo.link_fheap_addr)) {
            /* Remove object from the dense link storage */
            if(H5G_dense_remove_by_idx(grp_oloc->file, dxpl_id, &linfo, grp_full_path_r, idx_type, order, n) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "can't remove object")
        } /* end if */
        else {
            /* Remove object from compact link storage */
            if(H5G_compact_remove_by_idx(grp_oloc, dxpl_id, &linfo, grp_full_path_r, idx_type, order, n) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "can't remove object")
        } /* end else */
    } /* end if */
    else {
        /* Clear error stack from not finding the link info message */
        H5E_clear_stack(NULL);

        /* Can only perform name lookups on groups with symbol tables */
        if(idx_type != H5L_INDEX_NAME)
            HGOTO_ERROR(H5E_SYM, H5E_BADVALUE, FAIL, "no creation order index to query")

        /* Using the old format for groups */
        use_old_format = TRUE;

        /* Remove object from the symbol table */
        if(H5G_stab_remove_by_idx(grp_oloc, dxpl_id, grp_full_path_r, order, n) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "can't remove object")
    } /* end else */

    /* Update link info for a new-style group */
    if(!use_old_format) {
        if(H5G_obj_remove_update_linfo(grp_oloc, &linfo, dxpl_id) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTUPDATE, FAIL, "unable to update link info")
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_obj_remove() */


/*-------------------------------------------------------------------------
 * Function:	H5G_obj_lookup
 *
 * Purpose:	Look up a link in a group, using the name as the key.
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
            if(H5G_compact_lookup(grp_oloc, name, lnk, dxpl_id) < 0)
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


/*-------------------------------------------------------------------------
 * Function:	H5G_obj_lookup_by_idx
 *
 * Purpose:	Look up link info in a group, according to an order within an
 *              index.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Nov  6 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_obj_lookup_by_idx(H5O_loc_t *grp_oloc, H5L_index_t idx_type,
    H5_iter_order_t order, hsize_t n, H5O_link_t *lnk, hid_t dxpl_id)
{
    H5O_linfo_t linfo;		        /* Link info message */
    herr_t     ret_value = SUCCEED;     /* Return value */

    FUNC_ENTER_NOAPI(H5G_obj_lookup_by_idx, FAIL)

    /* check arguments */
    HDassert(grp_oloc && grp_oloc->file);

    /* Attempt to get the link info message for this group */
    if(H5O_read(grp_oloc, H5O_LINFO_ID, 0, &linfo, dxpl_id)) {
        /* Check for creation order tracking, if creation order index lookup requested */
        if(idx_type == H5L_INDEX_CRT_ORDER) {
            H5O_ginfo_t ginfo;		        /* Group info message */

            /* Get group info message, to see if creation order is tracked for links in this group */
            if(NULL == H5O_read(grp_oloc, H5O_GINFO_ID, 0, &ginfo, dxpl_id))
                HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't retrieve group info message for group")

            /* Check if creation order is tracked */
            if(!ginfo.track_corder)
                HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "creation order not tracked for links in group")
        } /* end if */

        /* Check for dense link storage */
        if(H5F_addr_defined(linfo.link_fheap_addr)) {
            /* Get the link from the dense storage */
            if(H5G_dense_lookup_by_idx(grp_oloc->file, dxpl_id, &linfo, idx_type, order, n, lnk) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "can't locate object")
        } /* end if */
        else {
            /* Get the link from the link messages */
            if(H5G_compact_lookup_by_idx(grp_oloc, dxpl_id, &linfo, idx_type, order, n, lnk) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "can't locate object")
        } /* end else */
    } /* end if */
    else {
        /* Clear error stack from not finding the link info message */
        H5E_clear_stack(NULL);

        /* Can only perform name lookups on groups with symbol tables */
        if(idx_type != H5L_INDEX_NAME)
            HGOTO_ERROR(H5E_SYM, H5E_BADVALUE, FAIL, "no creation order index to query")

        /* Get the object's info from the symbol table */
        if(H5G_stab_lookup_by_idx(grp_oloc, order, n, lnk, dxpl_id) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "can't locate object")
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_obj_lookup_by_idx() */

