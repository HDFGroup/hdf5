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
#define H5F_PACKAGE		/*suppress error about including H5Fpkg	  */
#define H5G_PACKAGE		/*suppress error about including H5Gpkg   */


/* Packages needed by this file... */
#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Fpkg.h"		/* File access				*/
#include "H5Gpkg.h"		/* Groups		  		*/
#include "H5HLprivate.h"	/* Local Heaps				*/
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5MMprivate.h"	/* Memory management			*/

/* Private typedefs */

/* User data for converting link messages to symbol table */
typedef struct {
    H5F_t      *f;              /* Pointer to file for insertion */
    haddr_t     btree_addr;     /* Address of symbol table B-tree */
    haddr_t     heap_addr;      /* Address of symbol table local heap */
    hid_t       dxpl_id;        /* DXPL during insertion */
} H5G_obj_ud1_t;

/* User data for looking up an object in a group */
typedef struct {
    H5O_link_t *lnk;            /* Link information to set for object */
    H5O_loc_t  *oloc;           /* Object location to set */
} H5G_obj_ud2_t;

/* Private macros */

/* PRIVATE PROTOTYPES */
#ifdef H5_GROUP_REVISION
static herr_t
H5G_obj_link_to_stab_cb(const void *_mesg, unsigned idx, void *_udata);
#endif /* H5_GROUP_REVISION */


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
H5G_obj_create(H5F_t *f, hid_t dxpl_id,
#ifdef H5_GROUP_REVISION
    H5O_ginfo_t *ginfo,
#endif /* H5_GROUP_REVISION */
    H5O_loc_t *oloc/*out*/)
{
#ifdef H5_GROUP_REVISION
    H5O_linfo_t linfo;                  /* Link information */
    H5O_link_t lnk;                     /* Temporary link message info for computing message size */
    char null_char = '\0';              /* Character for creating null string */
    size_t ginfo_size;                  /* Size of the group info message */
    size_t linfo_size;                  /* Size of the link info message */
    size_t link_size;                   /* Size of a link message */
#endif /* H5_GROUP_REVISION */
    size_t hdr_size;                    /* Size of object header to request */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(H5G_obj_create, FAIL)

    /*
     * Check arguments.
     */
    HDassert(f);
#ifdef H5_GROUP_REVISION
    HDassert(ginfo);
#endif /* H5_GROUP_REVISION */
    HDassert(oloc);

#ifdef H5_GROUP_REVISION
    /* Initialize message information */
    linfo.nlinks = 0;

    /* Calculate message size infomation, for creating group's object header */
    linfo_size = H5O_mesg_size(H5O_LINFO_ID, f, &linfo);
    HDassert(linfo_size);

    ginfo_size = H5O_mesg_size(H5O_GINFO_ID, f, ginfo);
    HDassert(ginfo_size);

    lnk.type = H5G_LINK_HARD;
    lnk.name = &null_char;
    link_size = H5O_mesg_size(H5O_LINK_ID, f, &lnk);
    HDassert(link_size);

    /* Compute size of header to use for creation */
    hdr_size = linfo_size +
                ginfo_size +
                (ginfo->est_num_entries * (link_size + ginfo->est_name_len));
#else /* H5_GROUP_REVISION */
    hdr_size = 4 + 2 * H5F_SIZEOF_ADDR(f);
#endif /* H5_GROUP_REVISION */

    /*
     * Create symbol table object header.  It has a zero link count
     * since nothing refers to it yet.	The link count will be
     * incremented if the object is added to the group directed graph.
     */
    if(H5O_create(f, dxpl_id, hdr_size, oloc/*out*/) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't create header")

#ifdef H5_GROUP_REVISION
    /* Insert link info message */
    if(H5O_modify(oloc, H5O_LINFO_ID, H5O_NEW_MESG, 0, 0, &linfo, dxpl_id) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't create message")

    /* Insert group info message */
    if(H5O_modify(oloc, H5O_GINFO_ID, H5O_NEW_MESG, H5O_FLAG_CONSTANT, H5O_UPDATE_TIME, ginfo, dxpl_id) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't create message")
#else /* H5_GROUP_REVISION */
    {
        H5O_stab_t	stab;		/* Symbol table message	*/

        /* The group doesn't currently have a 'stab' message, go create one */
        if(H5G_stab_create(oloc, &stab, dxpl_id) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to create symbol table")

    }
#endif /* H5_GROUP_REVISION */

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

#ifdef H5_GROUP_REVISION

/*-------------------------------------------------------------------------
 * Function:	H5G_obj_link_to_stab_cb
 *
 * Purpose:	Callback routine for converting 'link' messages to symbol table
 *              form.
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
H5G_obj_link_to_stab_cb(const void *_mesg, unsigned UNUSED idx, void *_udata)
{
    const H5O_link_t *lnk = (const H5O_link_t *)_mesg;  /* Pointer to link */
    H5G_obj_ud1_t *udata = (H5G_obj_ud1_t *)_udata;     /* 'User data' passed in */
    H5G_bt_ud1_t bt_udata;	        /* Data to pass through B-tree	*/
    herr_t ret_value = H5O_ITER_CONT;   /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5G_obj_link_to_stab_cb)

    /* check arguments */
    HDassert(lnk);
    HDassert(udata);

    /* Construct user data to pass through B-tree routines */
    bt_udata.common.name = lnk->name;
    bt_udata.common.heap_addr = udata->heap_addr;
    bt_udata.lnk = lnk;

    /* Insert entry into symbol table */
    if(H5B_insert(udata->f, udata->dxpl_id, H5B_SNODE, udata->btree_addr, &bt_udata) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTINSERT, FAIL, "unable to insert entry")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_obj_link_to_stab_cb() */
#endif /* H5_GROUP_REVISION */


/*-------------------------------------------------------------------------
 * Function:	H5G_obj_insert
 *
 * Purpose:	Insert a new symbol into the group described by GRP_OLOC.
 *		file F.	 The name of the new symbol is NAME and its symbol
 *		table entry is OBJ_LNK.  Optionally, increment the reference
 *              count for the object the link points to with INC_LINK.
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
    hbool_t inc_link, hid_t dxpl_id)
{
#ifdef H5_GROUP_REVISION
    H5O_linfo_t linfo;		/* Link info message */
    htri_t      linfo_exists;   /* Whether the link info is present */
    hbool_t     use_stab;       /* Whether to use symbol table for insertions or not */
#endif /* H5_GROUP_REVISION */
    herr_t     ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5G_obj_insert, FAIL)

    /* check arguments */
    HDassert(grp_oloc && grp_oloc->file);
    HDassert(name && *name);
    HDassert(obj_lnk);

#ifdef H5_GROUP_REVISION
    /* Check if we have information about the number of objects in this group */
    if((linfo_exists = H5O_exists(grp_oloc, H5O_LINFO_ID, 0, dxpl_id)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "unable to check for link info")
    if(linfo_exists) {
        htri_t stab_exists;     /* Whether the symbol table info is present */

        /* Get the number of objects in this group */
        if(NULL == H5O_read(grp_oloc, H5O_LINFO_ID, 0, &linfo, dxpl_id))
            HGOTO_ERROR(H5E_SYM, H5E_BADMESG, FAIL, "can't get link info")

        /* Check if there is already a 'stab' message */
        if((stab_exists = H5O_exists(grp_oloc, H5O_STAB_ID, 0, dxpl_id)) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "unable to check for symbol table")
        if(stab_exists)
            use_stab = TRUE;
        else {
            H5O_ginfo_t ginfo;		/* Group info message            */
            size_t link_msg_size;       /* Size of link message in the file */

            /* Get the link message size */
            if((link_msg_size = H5O_raw_size(H5O_LINK_ID, grp_oloc->file, obj_lnk)) == 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTGETSIZE, FAIL, "can't get link size")

            /* Get the group info */
            if(NULL == H5O_read(grp_oloc, H5O_GINFO_ID, 0, &ginfo, dxpl_id))
                HGOTO_ERROR(H5E_SYM, H5E_BADMESG, FAIL, "can't get group info")

            /* If there's still a small enough number of links, use the 'link' message */
            /* (If the encoded form of the link is too large to fit into an object
             *  header message, convert to using symbol table instead of link messages)
             */
            if(linfo.nlinks < ginfo.max_compact && link_msg_size < H5O_MAX_SIZE)
                use_stab = FALSE;
            else {
                H5G_obj_ud1_t	udata;          /* User data for iteration */
                H5O_stab_t	stab;		/* Symbol table message	*/

                /* The group doesn't currently have a 'stab' message, go create one */
                if(H5G_stab_create(grp_oloc, &stab, dxpl_id) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to create symbol table")

                /* Set up user data for object header message iteration */
                udata.f = grp_oloc->file;
                udata.btree_addr = stab.btree_addr;
                udata.heap_addr = stab.heap_addr;
                udata.dxpl_id = dxpl_id;

                /* Iterate over the 'link' messages, inserting them into the symbol table */
                if(H5O_iterate(grp_oloc, H5O_LINK_ID, H5G_obj_link_to_stab_cb, &udata, dxpl_id) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "error iterating over links")

                /* Remove all the 'link' messages */
                if(H5O_remove(grp_oloc, H5O_LINK_ID, H5O_ALL, FALSE, dxpl_id) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTDELETE, FAIL, "unable to delete link messages")

                use_stab = TRUE;
            } /* end else */
        } /* end else */
    } /* end if */
    else
        use_stab = TRUE;
#endif /* H5_GROUP_REVISION */

    /* Insert into symbol table or create link object */
#ifdef H5_GROUP_REVISION
    if(use_stab) {
#endif /* H5_GROUP_REVISION */
        /* Insert into symbol table */
        if(H5G_stab_insert(grp_oloc, name, obj_lnk, dxpl_id) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINSERT, FAIL, "unable to insert entry")
#ifdef H5_GROUP_REVISION
    } /* end if */
    else {
        /* Insert with link message */
        if(H5G_link_insert(grp_oloc, obj_lnk, dxpl_id) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINSERT, FAIL, "unable to insert entry")
    } /* end else */

    /* Increment the number of objects in this group */
    if(linfo_exists) {
        linfo.nlinks++;
        if(H5O_modify(grp_oloc, H5O_LINFO_ID, 0, 0, H5O_UPDATE_TIME, &linfo, dxpl_id) < 0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTINIT, FAIL, "can't update link info message")
    } /* end if */
#endif /* H5_GROUP_REVISION */

    /* Increment link count on object, if appropriate */
    if(inc_link) {
        H5O_loc_t obj_oloc;             /* Object location */

        /* Convert to object location */
        obj_oloc.file = grp_oloc->file;
        obj_oloc.addr = obj_lnk->u.hard.addr;

        /* Increment reference count for object */
        if(H5O_link(&obj_oloc, 1, dxpl_id) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_LINK, FAIL, "unable to increment hard link count")
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
H5G_obj_iterate(hid_t loc_id, const char *name, int skip, int *last_obj,
	    H5G_iterate_t op, void *op_data, hid_t dxpl_id)
{
#ifdef H5_GROUP_REVISION
    htri_t              stab_exists;    /* Whether the symbol table info is present */
#endif /* H5_GROUP_REVISION */
    hid_t               gid = -1;       /* ID of group to iterate over */
    H5G_t		*grp;           /* Pointer to group data structure to iterate over */
    herr_t		ret_value;      /* Return value */

    FUNC_ENTER_NOAPI(H5G_obj_iterate, FAIL)

    /* Sanity check */
    HDassert(name);
    HDassert(last_obj);
    HDassert(op);

    /*
     * Open the group on which to operate.  We also create a group ID which
     * we can pass to the application-defined operator.
     */
    if((gid = H5Gopen (loc_id, name)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, FAIL, "unable to open group")
    if((grp = H5I_object(gid)) == NULL)
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "bad group ID")

#ifdef H5_GROUP_REVISION
    /* Check if we have information about the number of objects in this group */
    if((stab_exists = H5O_exists(&(grp->oloc), H5O_STAB_ID, 0, dxpl_id)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "unable to check for symbol table")

    /* If the symbol table doesn't exist, iterate over link messages */
    if(!stab_exists) {
        /* Get the object's name from the link messages */
        if((ret_value = H5G_link_iterate(&(grp->oloc), gid, skip, last_obj, op, op_data, dxpl_id)) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_BADITER, FAIL, "can't iterate over links")
    } /* end if */
    else {
#endif /* H5_GROUP_REVISION */
        /* Iterate over symbol table */
        if((ret_value = H5G_stab_iterate(&(grp->oloc), gid, skip, last_obj, op, op_data, dxpl_id)) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_BADITER, FAIL, "can't iterate over symbol table")
#ifdef H5_GROUP_REVISION
    } /* end else */
#endif /* H5_GROUP_REVISION */

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
#ifdef H5_GROUP_REVISION
    htri_t linfo_exists;                /* Whether the link info is present */
#endif /* H5_GROUP_REVISION */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(H5G_obj_count, FAIL)

    /* Sanity check */
    HDassert(oloc);
    HDassert(num_objs);

#ifdef H5_GROUP_REVISION
    /* Check if we have information about the number of objects in this group */
    if((linfo_exists = H5O_exists(oloc, H5O_LINFO_ID, 0, dxpl_id)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "unable to check for link info")

    /* If the link info exists, then it has the number of objects in the group */
    if(linfo_exists > 0) {
        H5O_linfo_t		linfo;		/* Link info message            */

        /* Get the link info for this group */
        if(NULL == H5O_read(oloc, H5O_LINFO_ID, 0, &linfo, dxpl_id))
            HGOTO_ERROR(H5E_SYM, H5E_BADMESG, FAIL, "can't get link info")

        /* Set the number of objects */
        *num_objs = linfo.nlinks;
    } /* end if */
    else {
#endif /* H5_GROUP_REVISION */
        /* Get the number of objects in this group by iterating over symbol table */
        if(H5G_stab_count(oloc, num_objs, dxpl_id) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTCOUNT, FAIL, "can't count objects")
#ifdef H5_GROUP_REVISION
    } /* end else */
#endif /* H5_GROUP_REVISION */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_obj_count() */


/*-------------------------------------------------------------------------
 * Function:	H5G_obj_get_name_by_idx
 *
 * Purpose:     Private function for H5Gget_objname_by_idx.
 *              Returns the name of objects in the group by giving index.
 *
 * Return:	Success:        Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Raymond Lu
 *	        Nov 20, 2002
 *
 *-------------------------------------------------------------------------
 */
ssize_t
H5G_obj_get_name_by_idx(H5O_loc_t *oloc, hsize_t idx, char* name, size_t size, hid_t dxpl_id)
{
#ifdef H5_GROUP_REVISION
    htri_t              stab_exists;    /* Whether the symbol table info is present */
#endif /* H5_GROUP_REVISION */
    ssize_t		ret_value;      /* Return value */

    FUNC_ENTER_NOAPI(H5G_obj_get_name_by_idx, FAIL)

    /* Sanity check */
    HDassert(oloc);

#ifdef H5_GROUP_REVISION
    /* Check if we have information about the number of objects in this group */
    if((stab_exists = H5O_exists(oloc, H5O_STAB_ID, 0, dxpl_id)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "unable to check for symbol table")

    /* If the symbol table doesn't exist, search link messages */
    if(!stab_exists) {
        /* Get the object's name from the link messages */
        if((ret_value = H5G_link_get_name_by_idx(oloc, idx, name, size, dxpl_id)) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "can't locate name")
    } /* end if */
    else {
#endif /* H5_GROUP_REVISION */
        /* Get the object's name from the symbol table */
        if((ret_value = H5G_stab_get_name_by_idx(oloc, idx, name, size, dxpl_id)) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "can't locate name")
#ifdef H5_GROUP_REVISION
    } /* end else */
#endif /* H5_GROUP_REVISION */

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
#ifdef H5_GROUP_REVISION
    htri_t              stab_exists;    /* Whether the symbol table info is present */
#endif /* H5_GROUP_REVISION */
    H5G_obj_t		ret_value;      /* Return value */

    FUNC_ENTER_NOAPI(H5G_obj_get_type_by_idx, H5G_UNKNOWN)

    /* Sanity check */
    HDassert(oloc);

#ifdef H5_GROUP_REVISION
    /* Check if we have information about the number of objects in this group */
    if((stab_exists = H5O_exists(oloc, H5O_STAB_ID, 0, dxpl_id)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, H5G_UNKNOWN, "unable to check for symbol table")

    /* If the symbol table doesn't exist, search link messages */
    if(!stab_exists) {
        /* Get the object's type from the link messages */
        if((ret_value = H5G_link_get_type_by_idx(oloc, idx, dxpl_id)) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, H5G_UNKNOWN, "can't locate type")
    } /* end if */
    else {
#endif /* H5_GROUP_REVISION */
        /* Get the object's type from the symbol table */
        if((ret_value = H5G_stab_get_type_by_idx(oloc, idx, dxpl_id)) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, H5G_UNKNOWN, "can't locate type")
#ifdef H5_GROUP_REVISION
    } /* end else */
#endif /* H5_GROUP_REVISION */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_obj_get_type_by_idx() */


/*-------------------------------------------------------------------------
 * Function:	H5G_obj_remove
 *
 * Purpose:     Remove an object from a group.
 *              (Needs to hand up the type of the object removed)
 *
 * Return:	Success:        Non-negative
 *
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
#ifdef H5_GROUP_REVISION
    htri_t              linfo_exists;   /* Whether the link info is present */
    H5O_linfo_t		linfo;		/* Link info message            */
    htri_t              stab_exists;    /* Whether the symbol table info is present */
    hbool_t             use_stab;       /* Whether to use symbol table for deletions or not */
#endif /* H5_GROUP_REVISION */
    H5G_obj_t		ret_value;      /* Return value */

    FUNC_ENTER_NOAPI(H5G_obj_remove, FAIL)

    /* Sanity check */
    HDassert(oloc);
    HDassert(obj_type);

#ifdef H5_GROUP_REVISION
    /* Check if we have information about the number of objects in this group */
    if((stab_exists = H5O_exists(oloc, H5O_STAB_ID, 0, dxpl_id)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "unable to check for symbol table")

    /* Check if we have information about the number of objects in this group */
    if((linfo_exists = H5O_exists(oloc, H5O_LINFO_ID, 0, dxpl_id)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "unable to check for link info message")
    if(linfo_exists) {
        H5O_ginfo_t ginfo;		/* Group info message            */

        /* Get the number of objects in this group */
        if(NULL == H5O_read(oloc, H5O_LINFO_ID, 0, &linfo, dxpl_id))
            HGOTO_ERROR(H5E_SYM, H5E_BADMESG, FAIL, "can't get link info")

        /* Check for deleting enough links from group to go back to link messages */
        if(stab_exists) {
            /* Get the group info */
            if(NULL == H5O_read(oloc, H5O_GINFO_ID, 0, &ginfo, dxpl_id))
                HGOTO_ERROR(H5E_SYM, H5E_BADMESG, FAIL, "can't get group info")

            /* Switch from symbol table back to link messages */
            if(linfo.nlinks <= ginfo.min_dense) {
                H5G_bt_it_ud4_t	udata;
                H5O_stab_t stab;	        /* Info about local heap & B-tree */
                H5O_link_t *lnk_table;          /* Array of links to convert */
                hbool_t can_convert = TRUE;     /* Whether converting to link messages is possible */
                size_t u;                       /* Local index */

                /* Get the B-tree & local heap info */
                if(NULL == H5O_read(oloc, H5O_STAB_ID, 0, &stab, dxpl_id))
                    HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "unable to determine local heap address")

                /* Allocate space for the link table */
                H5_CHECK_OVERFLOW(linfo.nlinks, /* From: */ hsize_t, /* To: */size_t);
                if(NULL == (lnk_table = H5MM_malloc(sizeof(H5O_link_t) * (size_t)linfo.nlinks)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for link table")

                /* Build udata to pass through H5B_iterate() */
                udata.heap_addr = stab.heap_addr;
                udata.lnk_table = lnk_table;
                udata.nlinks = 0;
                H5_CHECK_OVERFLOW(linfo.nlinks, hsize_t, size_t);
                udata.max_links = (size_t)linfo.nlinks;

                /* Iterate over the group members, building a table of equivalent link messages */
                if((ret_value = H5B_iterate(oloc->file, dxpl_id, H5B_SNODE,
                          H5G_node_stab_convert, stab.btree_addr, &udata)) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTNEXT, FAIL, "error iterating over entries")

                /* Inspect links in table for ones that can't be converted back
                 * into link message form (currently only links which can't fit
                 * into an object header message)
                 */
                for(u = 0; u < linfo.nlinks; u++)
                    if(H5O_mesg_size(H5O_LINK_ID, oloc->file, &(lnk_table[u])) >= H5O_MAX_SIZE) {
                        can_convert = FALSE;
                        break;
                    } /* end if */

                /* If ok, insert links as link messages */
                if(can_convert) {
                    for(u = 0; u < linfo.nlinks; u++) {
                        /* Insert link message into group */
                        if(H5O_modify(oloc, H5O_LINK_ID, H5O_NEW_MESG, 0, H5O_UPDATE_TIME, &(lnk_table[u]), dxpl_id) < 0)
                            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't create message")
                    } /* end for */

                    /* Remove the 'stab' message */
                    if(H5O_remove(oloc, H5O_STAB_ID, H5O_ALL, FALSE, dxpl_id) < 0)
                        HGOTO_ERROR(H5E_SYM, H5E_CANTDELETE, FAIL, "unable to delete symbol table message")

                    use_stab = FALSE;
                } /* end if */
                else
                    use_stab = TRUE;

                /* Release memory for link names (and memory for soft link values) */
                for(u = 0; u < linfo.nlinks; u++) {
                    H5MM_xfree(lnk_table[u].name);
                    if(lnk_table[u].type == H5G_LINK_SOFT)
                        H5MM_xfree(lnk_table[u].u.soft.name);
                } /* end for */

                /* Release memory for link table */
                H5MM_xfree(lnk_table);
            } /* end if */
            else
                use_stab = TRUE;
        } /* end if */
        else
            use_stab = FALSE;
    } /* end if */
    else
        use_stab = TRUE;
#endif /* H5_GROUP_REVISION */

    /* If the symbol table doesn't exist, search link messages */
#ifdef H5_GROUP_REVISION
    if(!use_stab) {
        /* Remove object from the link messages */
        if((ret_value = H5G_link_remove(oloc, name, obj_type, dxpl_id)) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "can't remove object")
    } /* end if */
    else {
#endif /* H5_GROUP_REVISION */
        /* Remove object from the symbol table */
        if((ret_value = H5G_stab_remove(oloc, name, obj_type, dxpl_id)) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "can't remove object")
#ifdef H5_GROUP_REVISION
    } /* end else */
#endif /* H5_GROUP_REVISION */

#ifdef H5_GROUP_REVISION
    /* Decrement the number of objects in this group */
    if(linfo_exists) {
        linfo.nlinks--;
        if(H5O_modify(oloc, H5O_LINFO_ID, 0, 0, H5O_UPDATE_TIME, &linfo, dxpl_id) < 0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTINIT, FAIL, "can't update link info message")

        /* Remove the symbol table, if we are using one and the number of links drops to zero */
        if(linfo.nlinks == 0 && use_stab) {
            if(H5O_remove(oloc, H5O_STAB_ID, H5O_ALL, FALSE, dxpl_id) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTDELETE, FAIL, "unable to delete symbol table message")
        } /* end if */
    } /* end if */
#endif /* H5_GROUP_REVISION */

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
#ifdef H5_GROUP_REVISION
    htri_t     stab_exists;             /* Whether the symbol table info is present */
#endif /* H5_GROUP_REVISION */
    herr_t     ret_value = SUCCEED;     /* Return value */

    FUNC_ENTER_NOAPI(H5G_obj_lookup, FAIL)

    /* check arguments */
    HDassert(grp_oloc && grp_oloc->file);
    HDassert(name && *name);

#ifdef H5_GROUP_REVISION
    /* Check if we have information about the number of objects in this group */
    if((stab_exists = H5O_exists(grp_oloc, H5O_STAB_ID, 0, dxpl_id)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "unable to check for symbol table")

    /* If the symbol table doesn't exist, search link messages */
    if(!stab_exists) {
        /* Get the object's info from the link messages */
        if(H5G_link_lookup(grp_oloc, name, lnk, dxpl_id) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "can't locate object")
    } /* end if */
    else {
#endif /* H5_GROUP_REVISION */
        /* Get the object's info from the symbol table */
        if(H5G_stab_lookup(grp_oloc, name, lnk, dxpl_id) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "can't locate object")
#ifdef H5_GROUP_REVISION
    } /* end else */
#endif /* H5_GROUP_REVISION */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_obj_lookup() */


/*-------------------------------------------------------------------------
 * Function:	H5G_obj_find_cb
 *
 * Purpose:	Callback for retrieving object location for an object in a group
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, September 20, 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5G_obj_find_cb(H5G_loc_t UNUSED *grp_loc/*in*/, const char UNUSED *name, const H5O_link_t *lnk,
    H5G_loc_t *obj_loc, void *_udata/*in,out*/)
{
    H5G_obj_ud2_t *udata = (H5G_obj_ud2_t *)_udata;   /* User data passed in */
    herr_t ret_value = SUCCEED;              /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5G_obj_find_cb)

    /* Copy link for object */
    if(udata->lnk) {
        /* Check if the name in this group resolved to a valid link */
        if(lnk == NULL)
            HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "name doesn't exist")

        /* Copy link information */
#ifdef H5_GROUP_REVISION
        if(H5O_copy(H5O_LINK_ID, lnk, udata->lnk) == NULL)
            HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, H5O_ITER_ERROR, "can't copy link message")
#else /* H5_GROUP_REVISION */
        *udata->lnk = *lnk;
        HDassert(lnk->name);
        udata->lnk->name = H5MM_xstrdup(lnk->name);
        if(lnk->type == H5G_LINK_SOFT)
            udata->lnk->u.soft.name = H5MM_xstrdup(lnk->u.soft.name);
#endif /* H5_GROUP_REVISION */
    } /* end if */

    /* Copy object location for object */
    if(udata->oloc) {
        /* Check if the name in this group resolved to a valid object */
        if(obj_loc == NULL)
            HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "name doesn't exist")

        H5O_loc_copy(udata->oloc, obj_loc->oloc, H5_COPY_DEEP);
    } /* end if */

done:
    /* Release the group location for the object */
    /* (Group traversal callbacks are responsible for either taking ownership
     *  of the group location for the object, or freeing it. - QAK)
     */
    if(obj_loc)
        H5G_loc_free(obj_loc);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_obj_find_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5G_obj_find
 *
 * Purpose:	Look up an object relative to a group.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Sep 20 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_obj_find(H5G_loc_t *loc, const char *name, unsigned traverse_flags,
    H5O_link_t *lnk, H5O_loc_t *obj_oloc, hid_t dxpl_id)
{
    H5G_obj_ud2_t udata;                /* User data for traversal callback */
    herr_t     ret_value = SUCCEED;     /* Return value */

    FUNC_ENTER_NOAPI(H5G_obj_find, FAIL)

    /* check arguments */
    HDassert(loc && loc->oloc->file);
    HDassert(name && *name);
    HDassert(obj_oloc);

    /* Set up user data for locating object */
    udata.lnk = lnk;
    udata.oloc = obj_oloc;

    /* Traverse group hierarchy to locate object */
    if(H5G_traverse(loc, name, traverse_flags, H5G_obj_find_cb, &udata, dxpl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "can't find object")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_obj_find() */

