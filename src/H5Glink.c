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
 * Created:		H5Glink.c
 *			Sep  5 2005
 *			Quincey Koziol <koziol@ncsa.uiuc.edu>
 *
 * Purpose:		Functions for handling link messages.
 *
 *-------------------------------------------------------------------------
 */
#define H5G_PACKAGE		/*suppress error about including H5Gpkg	  */


/* Packages needed by this file... */
#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Gpkg.h"		/* Groups		  		*/
#include "H5HLprivate.h"	/* Local Heaps				*/
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5MMprivate.h"	/* Memory management			*/

#ifdef H5_GROUP_REVISION
/* Private typedefs */

/* Data structure to hold table of links for a group */
typedef struct {
    size_t      nlinks;         /* # of links in table */
    H5O_link_t *lnks;           /* Pointer to array of links */
} H5G_link_table_t;

/* User data for link message iteration when building link table */
typedef struct {
    H5G_link_table_t *ltable;   /* Pointer to link table to build */
    size_t curr_lnk;            /* Current link to operate on */
} H5G_link_ud1_t;

/* User data for deleting a link in the link messages */
typedef struct {
    /* downward */
    const char *name;           /* Name to search for */
    H5F_t       *file;          /* File that object header is located within */
    hid_t       dxpl_id;        /* DXPL during insertion */

    /* upward */
    H5G_obj_t *obj_type;        /* Type of object deleted */
} H5G_link_ud2_t;

/* User data for link message iteration when querying object info */
typedef struct {
    /* downward */
    const char *name;           /* Name to search for */
    H5F_t       *file;          /* File that object header is located within */
    hid_t       dxpl_id;        /* DXPL during insertion */

    /* upward */
    H5G_stat_t *statbuf;        /* Stat buffer for info */
} H5G_link_ud3_t;

/* User data for link message iteration when querying object location */
typedef struct {
    /* downward */
    const char *name;           /* Name to search for */

    /* upward */
    H5O_link_t *lnk;            /* Link struct to fill in */
    hbool_t found;              /* Flag to indicate that the object was found */
} H5G_link_ud4_t;

/* User data for link message iteration when querying soft link value */
typedef struct {
    /* downward */
    const char *name;           /* Name to search for */
    size_t size;                /* Buffer size for link value */

    /* upward */
    char *buf;                  /* Buffer to fill with link value */
} H5G_link_ud5_t;

/* Private macros */

/* PRIVATE PROTOTYPES */
static int H5G_link_cmp_name(const void *lnk1, const void *lnk2);
static herr_t H5G_link_build_table_cb(const void *_mesg, unsigned idx, void *_udata);
static herr_t H5G_link_build_table(H5O_loc_t *oloc, hid_t dxpl_id,
    H5G_link_table_t *ltable);
static herr_t H5G_link_release_table(H5G_link_table_t *ltable);


/*-------------------------------------------------------------------------
 * Function:	H5G_link_cmp_name
 *
 * Purpose:	Callback routine for comparing two link messages.
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
static int
H5G_link_cmp_name(const void *lnk1, const void *lnk2)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5G_link_cmp_name)

    FUNC_LEAVE_NOAPI(HDstrcmp(((const H5O_link_t *)lnk1)->name, ((const H5O_link_t *)lnk2)->name))
} /* end H5G_link_cmp_name() */


/*-------------------------------------------------------------------------
 * Function:	H5G_link_build_table_cb
 *
 * Purpose:	Callback routine for searching 'link' messages for a particular
 *              name.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Sep  5 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5G_link_build_table_cb(const void *_mesg, unsigned UNUSED idx, void *_udata)
{
    const H5O_link_t *lnk = (const H5O_link_t *)_mesg;  /* Pointer to link */
    H5G_link_ud1_t *udata = (H5G_link_ud1_t *)_udata;     /* 'User data' passed in */
    herr_t ret_value=H5O_ITER_CONT;             /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5G_link_build_table_cb)

    /* check arguments */
    HDassert(lnk);
    HDassert(udata);
    HDassert(udata->curr_lnk < udata->ltable->nlinks);

    /* Copy link message into table */
    if(H5O_copy(H5O_LINK_ID, lnk, &(udata->ltable->lnks[udata->curr_lnk])) == NULL)
        HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, H5O_ITER_ERROR, "can't copy link message")

    /* Increment current link entry to operate on */
    udata->curr_lnk++;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_link_build_table_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5G_link_build_table
 *
 * Purpose:     Builds a table containing a sorted (alphabetically) list of
 *              links for a group
 *
 * Return:	Success:        Non-negative
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *	        Sep  6, 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5G_link_build_table(H5O_loc_t *oloc, hid_t dxpl_id, H5G_link_table_t *ltable)
{
    H5O_linfo_t	linfo;		        /* Link info message */
    herr_t	ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5G_link_build_table)

    /* Sanity check */
    HDassert(oloc);

    /* Retrieve the link info */
    if(NULL == H5O_read(oloc, H5O_LINFO_ID, 0, &linfo, dxpl_id))
        HGOTO_ERROR(H5E_SYM, H5E_BADMESG, FAIL, "can't get link info")

    /* Set size of table */
    H5_CHECK_OVERFLOW(linfo.nlinks, hsize_t, size_t);
    ltable->nlinks = (size_t)linfo.nlinks;

    /* Allocate space for the table entries */
    if(ltable->nlinks > 0) {
        H5G_link_ud1_t udata;               /* User data for iteration callback */

        if((ltable->lnks = H5MM_malloc(sizeof(H5O_link_t) * ltable->nlinks)) == NULL)
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")

        /* Set up user data for iteration */
        udata.ltable = ltable;
        udata.curr_lnk = 0;

        /* Iterate through the link messages, adding them to the table */
        if(H5O_iterate(oloc, H5O_LINK_ID, H5G_link_build_table_cb, &udata, dxpl_id) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "error iterating over link messages")

        /* Sort link table (XXX: alphabetically, for now) */
        HDqsort(ltable->lnks, ltable->nlinks, sizeof(H5O_link_t), H5G_link_cmp_name);
    } /* end if */
    else
        ltable->lnks = NULL;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_link_build_table() */


/*-------------------------------------------------------------------------
 * Function:	H5G_link_release_table
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
static herr_t
H5G_link_release_table(H5G_link_table_t *ltable)
{
    size_t              u;              /* Local index variable */
    herr_t	ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5G_link_release_table)

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
} /* end H5G_link_release_table() */


/*-------------------------------------------------------------------------
 * Function:	H5G_link_convert
 *
 * Purpose:	Converts a group entry into a link object.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Sep  5 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_link_convert(H5O_link_t *lnk, const H5G_entry_t *ent, const H5HL_t *heap,
    const char *name)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(H5G_link_convert, FAIL)

    /* Check arguments. */
    HDassert(lnk);
    HDassert(ent);
    HDassert(name);

    /* Create link message from object entry */
    HDassert(ent->type == H5G_NOTHING_CACHED || ent->type == H5G_CACHED_SLINK);
/* XXX: Set character set & creation time for real? */
    lnk->cset = H5F_CRT_DEFAULT_CSET;
    lnk->ctime = 0;
    lnk->name = H5MM_xstrdup(name);   /* Casting away const OK -QAK */
    HDassert(lnk->name);
    switch(ent->type) {
        case H5G_NOTHING_CACHED:
            lnk->type = H5G_LINK_HARD;
            lnk->u.hard.addr = ent->header;
            break;

        case H5G_CACHED_SLINK:
            {
                const char *s;          /* Pointer to link value in heap */

                lnk->type = H5G_LINK_SOFT;

                s = H5HL_offset_into(ent->file, heap, ent->cache.slink.lval_offset);
                HDassert(s);

                /* Copy to link */
                lnk->u.soft.name = H5MM_xstrdup(s);
                HDassert(lnk->u.soft.name);
            }
            break;

        default:
            HGOTO_ERROR(H5E_SYM, H5E_BADVALUE, FAIL, "unrecognized link type")
    } /* end switch */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_link_convert() */


/*-------------------------------------------------------------------------
 * Function:	H5G_link_insert
 *
 * Purpose:	Insert a new symbol into the table described by GRP_ENT in
 *		file F.	 The name of the new symbol is NAME and its symbol
 *		table entry is OBJ_ENT.
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
H5G_link_insert(H5O_loc_t *grp_oloc, H5O_link_t *obj_lnk,
    hid_t dxpl_id)
{
    herr_t     ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5G_link_insert, FAIL)

    /* check arguments */
    HDassert(grp_oloc && grp_oloc->file);
    HDassert(obj_lnk);

    /* Insert link message into group */
    if(H5O_modify(grp_oloc, H5O_LINK_ID, H5O_NEW_MESG, 0, H5O_UPDATE_TIME, obj_lnk, dxpl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't create message")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_link_insert() */


/*-------------------------------------------------------------------------
 * Function:	H5G_link_get_name_by_idx
 *
 * Purpose:     Returns the name of objects in the group by giving index.
 *
 * Return:	Success:        Non-negative
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *	        Sep  6, 2005
 *
 *-------------------------------------------------------------------------
 */
ssize_t
H5G_link_get_name_by_idx(H5O_loc_t *oloc, hsize_t idx, char* name,
    size_t size, hid_t dxpl_id)
{
    H5G_link_table_t    ltable = {0, NULL};         /* Link table */
    ssize_t		ret_value;      /* Return value */

    FUNC_ENTER_NOAPI(H5G_link_get_name_by_idx, FAIL)

    /* Sanity check */
    HDassert(oloc);

    /* Build table of all link messages */
    if(H5G_link_build_table(oloc, dxpl_id, &ltable) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't create link message table")

    /* Check for going out of bounds */
    if(idx >= ltable.nlinks)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "index out of bound")

    /* Get the length of the name */
    ret_value = (ssize_t)HDstrlen(ltable.lnks[idx].name);

    /* Copy the name into the user's buffer, if given */
    if(name) {
        HDstrncpy(name, ltable.lnks[idx].name, MIN((size_t)(ret_value+1),size));
        if((size_t)ret_value >= size)
            name[size-1]='\0';
    } /* end if */

done:
    /* Release link table */
    if(ltable.lnks) {
        /* Free link table information */
        if(H5G_link_release_table(&ltable) < 0)
            HDONE_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "unable to release link table")
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_link_get_name_by_idx() */


/*-------------------------------------------------------------------------
 * Function:	H5G_link_get_type_by_idx
 *
 * Purpose:     Returns the type of objects in the group by giving index.
 *
 * Return:	Success:        Non-negative
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *	        Sep 12, 2005
 *
 *-------------------------------------------------------------------------
 */
H5G_obj_t
H5G_link_get_type_by_idx(H5O_loc_t *oloc, hsize_t idx, hid_t dxpl_id)
{
    H5G_link_table_t    ltable = {0, NULL};         /* Link table */
    H5G_obj_t		ret_value;      /* Return value */

    FUNC_ENTER_NOAPI(H5G_link_get_type_by_idx, H5G_UNKNOWN)

    /* Sanity check */
    HDassert(oloc);

    /* Build table of all link messages */
    if(H5G_link_build_table(oloc, dxpl_id, &ltable) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, H5G_UNKNOWN, "can't create link message table")

    /* Check for going out of bounds */
    if(idx >= ltable.nlinks)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, H5G_UNKNOWN, "index out of bound")

    /* Determine type of object */
    if(ltable.lnks[idx].type == H5G_LINK_SOFT)
        ret_value = H5G_LINK;
    else {
        H5O_loc_t tmp_oloc;             /* Temporary object location */

        /* Build temporary object location */
        tmp_oloc.file = oloc->file;
        tmp_oloc.addr = ltable.lnks[idx].u.hard.addr;

        /* Get the type of the object */
        if((ret_value = H5O_obj_type(&tmp_oloc, dxpl_id)) == H5G_UNKNOWN)
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, H5G_UNKNOWN, "can't determine object type")
    } /* end else */

done:
    /* Release link table */
    if(ltable.lnks) {
        /* Free link table information */
        if(H5G_link_release_table(&ltable) < 0)
            HDONE_ERROR(H5E_SYM, H5E_CANTFREE, H5G_UNKNOWN, "unable to release link table")
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_link_get_type_by_idx() */


/*-------------------------------------------------------------------------
 * Function:	H5G_link_remove_cb
 *
 * Purpose:	Callback routine for deleting 'link' message for a particular
 *              name.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Sep  5 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5G_link_remove_cb(const void *_mesg, unsigned UNUSED idx, void *_udata)
{
    const H5O_link_t *lnk = (const H5O_link_t *)_mesg;  /* Pointer to link */
    H5G_link_ud2_t *udata = (H5G_link_ud2_t *)_udata;     /* 'User data' passed in */
    herr_t ret_value = H5O_ITER_CONT;           /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5G_link_remove_cb)

    /* check arguments */
    HDassert(lnk);
    HDassert(udata);

    /* If we've found the right link, get the object type */
    if(HDstrcmp(lnk->name, udata->name) == 0) {
        if(lnk->type == H5G_LINK_SOFT)
            *(udata->obj_type) = H5G_LINK;
        else {
            H5O_loc_t tmp_oloc;             /* Temporary object location */

            /* Build temporary object location */
            tmp_oloc.file = udata->file;
            tmp_oloc.addr = lnk->u.hard.addr;

            /* Get the type of the object */
            /* Note: no way to check for error :-( */
            *(udata->obj_type) = H5O_obj_type(&tmp_oloc, udata->dxpl_id);
        } /* end else */

        /* Stop the iteration, we found the correct link */
        HGOTO_DONE(H5O_ITER_STOP)
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_link_remove_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5G_link_remove
 *
 * Purpose:	Remove NAME from links.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Monday, September 19, 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_link_remove(const H5O_loc_t *oloc, const char *name, H5G_obj_t *obj_type,
    hid_t dxpl_id)
{
    H5G_link_ud2_t udata;               /* Data to pass through OH iteration */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(H5G_link_remove, FAIL)

    HDassert(oloc && oloc->file);
    HDassert(name && *name);

    /* Initialize data to pass through object header iteration */
    udata.name = name;
    udata.file = oloc->file;
    udata.dxpl_id = dxpl_id;
    udata.obj_type = obj_type;

    /* Iterate over the link messages to delete the right one */
    if(H5O_remove_op(oloc, H5O_LINK_ID, H5O_FIRST, H5G_link_remove_cb, &udata, TRUE, dxpl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTDELETE, FAIL, "unable to delete link message")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_link_remove() */


/*-------------------------------------------------------------------------
 * Function:	H5G_link_iterate
 *
 * Purpose:	Iterate over the objects in a group
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Monday, October  3, 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_link_iterate(H5O_loc_t *oloc, hid_t gid, int skip, int *last_obj,
    H5G_iterate_t op, void *op_data, hid_t dxpl_id)
{
    H5G_link_table_t    ltable = {0, NULL};     /* Link table */
    size_t              u;                      /* Local index variable */
    herr_t		ret_value;

    FUNC_ENTER_NOAPI(H5G_link_iterate, FAIL)

    /* Sanity check */
    HDassert(oloc);
    HDassert(H5I_GROUP == H5I_get_type(gid));
    HDassert(last_obj);
    HDassert(op);

    /* Build table of all link messages */
    if(H5G_link_build_table(oloc, dxpl_id, &ltable) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, H5B_ITER_ERROR, "can't create link message table")

    /* Check for going out of bounds */
    if(skip > 0 && (size_t)skip >= ltable.nlinks)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, H5B_ITER_ERROR, "index out of bound")

    /* Iterate over link messages */
    for(u = 0, ret_value = H5B_ITER_CONT; u < ltable.nlinks && !ret_value; u++) {
        if(skip > 0)
            --skip;
        else
            ret_value = (op)(gid, ltable.lnks[u].name, op_data);

        /* Increment the number of entries passed through */
        /* (whether we skipped them or not) */
        (*last_obj)++;
    } /* end for */

    /* Check for callback failure and pass along return value */
    if(ret_value < 0)
        HERROR(H5E_SYM, H5E_CANTNEXT, "iteration operator failed");

done:
    /* Release link table */
    if(ltable.lnks) {
        /* Free link table information */
        if(H5G_link_release_table(&ltable) < 0)
            HDONE_ERROR(H5E_SYM, H5E_CANTFREE, H5B_ITER_ERROR, "unable to release link table")
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_link_iterate() */


/*-------------------------------------------------------------------------
 * Function:	H5G_link_lookup_cb
 *
 * Purpose:	Callback routine for searching 'link' messages for a particular
 *              name & gettting object location for it
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Sep 20 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5G_link_lookup_cb(const void *_mesg, unsigned UNUSED idx, void *_udata)
{
    const H5O_link_t *lnk = (const H5O_link_t *)_mesg;  /* Pointer to link */
    H5G_link_ud4_t *udata = (H5G_link_ud4_t *)_udata;     /* 'User data' passed in */
    herr_t ret_value = H5O_ITER_CONT;           /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5G_link_lookup_cb)

    /* check arguments */
    HDassert(lnk);
    HDassert(udata);

    /* Check for name to get information */
    if(HDstrcmp(lnk->name, udata->name) == 0) {
        if(udata->lnk) {
            /* Copy link information */
            if(H5O_copy(H5O_LINK_ID, lnk, udata->lnk) == NULL)
                HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, H5O_ITER_ERROR, "can't copy link message")
        } /* end if */

        /* Indicate that the correct link was found */
        udata->found = TRUE;

        /* Stop iteration now */
        HGOTO_DONE(H5O_ITER_STOP)
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_link_lookup_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5G_link_lookup
 *
 * Purpose:	Look up an object relative to a group, using link messages.
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
H5G_link_lookup(H5O_loc_t *oloc, const char *name, H5O_link_t *lnk,
    hid_t dxpl_id)
{
    H5G_link_ud4_t udata;               /* User data for iteration callback */
    herr_t     ret_value = SUCCEED;     /* Return value */

    FUNC_ENTER_NOAPI(H5G_link_lookup, FAIL)

    /* check arguments */
    HDassert(lnk && oloc->file);
    HDassert(name && *name);

    /* Set up user data for iteration */
    udata.name = name;
    udata.lnk = lnk;
    udata.found = FALSE;

    /* Iterate through the link messages, adding them to the table */
    if(H5O_iterate(oloc, H5O_LINK_ID, H5G_link_lookup_cb, &udata, dxpl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "error iterating over link messages")

    /* Check if we found the link we were looking for */
    if(!udata.found)
        HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "object not found")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_link_lookup() */
#endif /* H5_GROUP_REVISION */

