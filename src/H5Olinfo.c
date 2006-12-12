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
 * Created:             H5Olinfo.c
 *                      Aug 23 2005
 *                      Quincey Koziol <koziol@ncsa.uiuc.edu>
 *
 * Purpose:             Link Information messages.
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

#define H5G_PACKAGE	/*suppress error about including H5Gpkg	  */
#define H5O_PACKAGE	/*suppress error about including H5Opkg	  */

#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5FLprivate.h"	/* Free lists                           */
#include "H5Gpkg.h"		/* Groups		  		*/
#include "H5Opkg.h"             /* Object headers			*/


/* PRIVATE PROTOTYPES */
static void *H5O_linfo_decode(H5F_t *f, hid_t dxpl_id, const uint8_t *p);
static herr_t H5O_linfo_encode(H5F_t *f, uint8_t *p, const void *_mesg);
static void *H5O_linfo_copy(const void *_mesg, void *_dest);
static size_t H5O_linfo_size(const H5F_t *f, const void *_mesg);
static herr_t H5O_linfo_free(void *_mesg);
static herr_t H5O_linfo_delete(H5F_t *f, hid_t dxpl_id, const void *_mesg, hbool_t adj_link);
static void *H5O_linfo_copy_file(H5F_t *file_src, const H5O_msg_class_t *mesg_type,
    void *native_src, H5F_t *file_dst, hid_t dxpl_id, H5O_copy_t *cpy_info,
    void *udata);
static herr_t H5O_linfo_post_copy_file(const H5O_loc_t *parent_src_oloc, const void *mesg_src, H5O_loc_t *dst_oloc,
    void *mesg_dst, hid_t dxpl_id, H5O_copy_t *cpy_info);
static herr_t H5O_linfo_debug(H5F_t *f, hid_t dxpl_id, const void *_mesg,
			     FILE * stream, int indent, int fwidth);

/* This message derives from H5O message class */
const H5O_msg_class_t H5O_MSG_LINFO[1] = {{
    H5O_LINFO_ID,            	/*message id number             */
    "linfo",                 	/*message name for debugging    */
    sizeof(H5O_linfo_t),     	/*native message size           */
    H5O_linfo_decode,        	/*decode message                */
    H5O_linfo_encode,        	/*encode message                */
    H5O_linfo_copy,          	/*copy the native value         */
    H5O_linfo_size,          	/*size of symbol table entry    */
    NULL,                   	/*default reset method          */
    H5O_linfo_free,	        /* free method			*/
    H5O_linfo_delete,	        /* file delete method		*/
    NULL,			/* link method			*/
    NULL,		    	/*get share method		*/
    NULL, 			/*set share method		*/
    NULL, 			/*is shared method		*/
    NULL,			/* pre copy native value to file */
    H5O_linfo_copy_file,	/* copy native value to file    */
    H5O_linfo_post_copy_file,	/* post copy native value to file */
    H5O_linfo_debug          	/*debug the message             */
}};

/* Current version of link info information */
#define H5O_LINFO_VERSION 	1

/* Flags for link info index flag encoding */
#define H5O_LINFO_INDEX_NAME          0x01
#define H5O_LINFO_INDEX_CORDER        0x02

/* Data exchange structure to use when copying links from src to dst */
typedef struct {
    const H5O_loc_t *src_oloc;          /* Source object location */
    H5O_loc_t *dst_oloc;                /* Destination object location */
    H5O_linfo_t *dst_linfo;             /* Destination object's link info message */
    hid_t dxpl_id;                      /* DXPL for operation */
    H5O_copy_t  *cpy_info;              /* Information for copy operation */
} H5O_linfo_postcopy_ud_t;

/* Declare a free list to manage the hsize_t struct */
H5FL_DEFINE_STATIC(H5O_linfo_t);


/*-------------------------------------------------------------------------
 * Function:    H5O_linfo_decode
 *
 * Purpose:     Decode a message and return a pointer to
 *              a newly allocated one.
 *
 * Return:      Success:        Ptr to new message in native order.
 *
 *              Failure:        NULL
 *
 * Programmer:  Quincey Koziol
 *              koziol@ncsa.uiuc.edu
 *              Aug 23 2005
 *
 *-------------------------------------------------------------------------
 */
static void *
H5O_linfo_decode(H5F_t *f, hid_t UNUSED dxpl_id, const uint8_t *p)
{
    unsigned char index_flags;  /* Flags for encoding link index info */
    H5O_linfo_t	*linfo = NULL;  /* Link info */
    void        *ret_value;     /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_linfo_decode)

    /* check args */
    HDassert(f);
    HDassert(p);

    /* Version of message */
    if(*p++ != H5O_LINFO_VERSION)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, NULL, "bad version number for message")

    /* Allocate space for message */
    if(NULL == (linfo = H5FL_MALLOC(H5O_linfo_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* Get the index flags for the group */
    index_flags = *p++;
    HDassert(index_flags & H5O_LINFO_INDEX_NAME);
    linfo->index_corder = (index_flags & H5O_LINFO_INDEX_CORDER) ? TRUE : FALSE;

    /* Number of links in the group */
    H5F_DECODE_LENGTH(f, p, linfo->nlinks)

    /* Min. & max creation order value for the group */
    INT64DECODE(p, linfo->min_corder)
    INT64DECODE(p, linfo->max_corder)

    /* Address of fractal heap to store "dense" links */
    H5F_addr_decode(f, &p, &(linfo->link_fheap_addr));

    /* Address of v2 B-tree to index names of links (names are always indexed) */
    H5F_addr_decode(f, &p, &(linfo->name_bt2_addr));

    /* Address of v2 B-tree to index creation order of links, if there is one */
    if(linfo->index_corder)
        H5F_addr_decode(f, &p, &(linfo->corder_bt2_addr));
    else
        linfo->corder_bt2_addr = HADDR_UNDEF;

    /* Set return value */
    ret_value = linfo;

done:
    if(ret_value == NULL)
        if(linfo != NULL)
            H5FL_FREE(H5O_linfo_t, linfo);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_linfo_decode() */


/*-------------------------------------------------------------------------
 * Function:    H5O_linfo_encode
 *
 * Purpose:     Encodes a message.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              koziol@ncsa.uiuc.edu
 *              Aug 23 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_linfo_encode(H5F_t *f, uint8_t *p, const void *_mesg)
{
    const H5O_linfo_t   *linfo = (const H5O_linfo_t *)_mesg;
    unsigned char       index_flags;          /* Flags for encoding link index info */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_linfo_encode)

    /* check args */
    HDassert(f);
    HDassert(p);
    HDassert(linfo);

    /* Message version */
    *p++ = H5O_LINFO_VERSION;

    /* The flags for the link indices */
    index_flags = H5O_LINFO_INDEX_NAME;       /* Names are always indexed */
    index_flags |= linfo->index_corder ? H5O_LINFO_INDEX_CORDER : 0;
    *p++ = index_flags;

    /* Number of links in the group */
    H5F_ENCODE_LENGTH(f, p, linfo->nlinks)

    /* Min. & max creation order value for the group */
    INT64ENCODE(p, linfo->min_corder)
    INT64ENCODE(p, linfo->max_corder)

    /* Address of fractal heap to store "dense" links */
    H5F_addr_encode(f, &p, linfo->link_fheap_addr);

    /* Address of v2 B-tree to index names of links */
    H5F_addr_encode(f, &p, linfo->name_bt2_addr);

    /* Address of v2 B-tree to index creation order of links, if they are indexed */
    if(linfo->index_corder)
        H5F_addr_encode(f, &p, linfo->corder_bt2_addr);
    else
        HDassert(!H5F_addr_defined(linfo->corder_bt2_addr));

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5O_linfo_encode() */


/*-------------------------------------------------------------------------
 * Function:    H5O_linfo_copy
 *
 * Purpose:     Copies a message from _MESG to _DEST, allocating _DEST if
 *              necessary.
 *
 * Return:      Success:        Ptr to _DEST
 *
 *              Failure:        NULL
 *
 * Programmer:  Quincey Koziol
 *              koziol@ncsa.uiuc.edu
 *              Aug 23 2005
 *
 *-------------------------------------------------------------------------
 */
static void *
H5O_linfo_copy(const void *_mesg, void *_dest)
{
    const H5O_linfo_t   *linfo = (const H5O_linfo_t *)_mesg;
    H5O_linfo_t         *dest = (H5O_linfo_t *) _dest;
    void                *ret_value;     /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_linfo_copy)

    /* check args */
    HDassert(linfo);
    if(!dest && NULL == (dest = H5FL_MALLOC(H5O_linfo_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* copy */
    *dest = *linfo;

    /* Set return value */
    ret_value = dest;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_linfo_copy() */


/*-------------------------------------------------------------------------
 * Function:    H5O_linfo_size
 *
 * Purpose:     Returns the size of the raw message in bytes not counting
 *              the message type or size fields, but only the data fields.
 *              This function doesn't take into account alignment.
 *
 * Return:      Success:        Message data size in bytes without alignment.
 *
 *              Failure:        zero
 *
 * Programmer:  Quincey Koziol
 *              koziol@ncsa.uiuc.edu
 *              Aug 23 2005
 *
 *-------------------------------------------------------------------------
 */
static size_t
H5O_linfo_size(const H5F_t *f, const void *_mesg)
{
    const H5O_linfo_t   *linfo = (const H5O_linfo_t *)_mesg;
    size_t ret_value;   /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_linfo_size)

    /* Set return value */
    ret_value = 1                       /* Version */
                + 1                     /* Index flags */
                + H5F_SIZEOF_SIZE(f)    /* Number of links */
                + 8                     /* Curr. min. creation order value */
                + 8                     /* Curr. max. creation order value */
                + H5F_SIZEOF_ADDR(f)    /* Address of fractal heap to store "dense" links */
                + H5F_SIZEOF_ADDR(f)    /* Address of v2 B-tree for indexing names of links */
                + (linfo->index_corder ? H5F_SIZEOF_ADDR(f) : 0);   /* Address of v2 B-tree for indexing creation order values of links */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_linfo_size() */


/*-------------------------------------------------------------------------
 * Function:	H5O_linfo_free
 *
 * Purpose:	Free's the message
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, August 23, 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_linfo_free(void *mesg)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_linfo_free)

    HDassert(mesg);

    H5FL_FREE(H5O_linfo_t, mesg);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5O_linfo_free() */


/*-------------------------------------------------------------------------
 * Function:    H5O_linfo_delete
 *
 * Purpose:     Free file space referenced by message
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              Saturday, September 16, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_linfo_delete(H5F_t *f, hid_t dxpl_id, const void *_mesg, hbool_t adj_link)
{
    const H5O_linfo_t *linfo = (const H5O_linfo_t *)_mesg;
    herr_t ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_linfo_delete)

    /* check args */
    HDassert(f);
    HDassert(linfo);

    /* If the group is using "dense" link storage, delete it */
    if(H5F_addr_defined(linfo->link_fheap_addr))
        if(H5G_dense_delete(f, dxpl_id, (H5O_linfo_t *)linfo, adj_link) < 0)   /* Casting away const OK - QAK */
            HGOTO_ERROR(H5E_OHDR, H5E_CANTFREE, FAIL, "unable to free dense link storage")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_linfo_delete() */


/*-------------------------------------------------------------------------
 * Function:    H5O_linfo_copy_file
 *
 * Purpose:     Copies a message from _MESG to _DEST in file
 *
 * Return:      Success:        Ptr to _DEST
 *
 *              Failure:        NULL
 *
 * Programmer:  Quincey Koziol
 *              June 26, 2006
 *
 *-------------------------------------------------------------------------
 */
static void *
H5O_linfo_copy_file(H5F_t UNUSED *file_src, const H5O_msg_class_t UNUSED *mesg_type, 
    void *native_src, H5F_t *file_dst, hid_t dxpl_id, H5O_copy_t *cpy_info,
    void UNUSED *udata)
{
    H5O_linfo_t          *linfo_src = (H5O_linfo_t *) native_src;
    H5O_linfo_t          *linfo_dst = NULL;
    void                 *ret_value;          /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_linfo_copy_file)

    /* check args */
    HDassert(linfo_src);
    HDassert(cpy_info);

    /* Copy the source message */
    if(NULL == (linfo_dst = H5O_linfo_copy(linfo_src, NULL)))
        HGOTO_ERROR(H5E_OHDR, H5E_CANTCOPY, NULL, "memory allocation failed")

    /* If we are performing a 'shallow hierarchy' copy, and the links in this
     *  group won't be included in the destination, reset the link info for
     *  this group.
     */
    if(cpy_info->max_depth >= 0 && cpy_info->curr_depth >= cpy_info->max_depth) {
        linfo_dst->nlinks = 0;
        linfo_dst->min_corder = linfo_dst->max_corder = 0;
        linfo_dst->link_fheap_addr = HADDR_UNDEF;
        linfo_dst->name_bt2_addr = HADDR_UNDEF;
        linfo_dst->corder_bt2_addr = HADDR_UNDEF;
    } /* end if */
    else {
        /* Create the components of the dense link storage for the destination group */
        /* (XXX: should probably get the "creation" parameters for the source group's
         *      dense link storage components and use those - QAK)
         */
        if(H5F_addr_defined(linfo_src->link_fheap_addr)) {
            if(H5G_dense_create(file_dst, dxpl_id, linfo_dst) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "unable to create 'dense' form of new format group")
        } /* end if */
    } /* end else */

    /* Set return value */
    ret_value = linfo_dst;

done:
    if(!ret_value)
        if(linfo_dst)
            H5FL_FREE(H5O_linfo_t, linfo_dst);

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5O_linfo_copy_file() */


/*-------------------------------------------------------------------------
 * Function:	H5O_linfo_post_copy_file_cb
 *
 * Purpose:	Callback routine for copying links from src to dst file
 *              during "post copy" routine
 *
 * Return:	Success:        Non-negative
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Sept 26 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_linfo_post_copy_file_cb(const H5O_link_t *src_lnk, void *_udata)
{
    H5O_linfo_postcopy_ud_t *udata = (H5O_linfo_postcopy_ud_t *)_udata;     /* 'User data' passed in */
    H5O_link_t dst_lnk;                 /* Destination link to insert */
    hbool_t dst_lnk_init = FALSE;       /* Whether the destination link is initialized */
    herr_t ret_value = H5_ITER_CONT;  /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_linfo_post_copy_file_cb)

    /* Check arguments */
    HDassert(src_lnk);
    HDassert(udata);

    /* Copy the link (and the object it points to) */
    if(H5G_link_copy_file(udata->dst_oloc->file, udata->dxpl_id, src_lnk,
            udata->src_oloc, &dst_lnk, udata->cpy_info) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTCOPY, H5_ITER_ERROR, "unable to copy link")
    dst_lnk_init = TRUE;

    /* Insert the new object in the destination file's group */
    /* (Doesn't increment the link count - that's already been taken care of for hard links) */
    if(H5G_dense_insert(udata->dst_oloc->file, udata->dxpl_id, udata->dst_linfo, &dst_lnk) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTINSERT, H5_ITER_ERROR, "unable to insert destination link")

done:
    /* Check if the destination link has been initialized */
    if(dst_lnk_init)
        H5O_msg_reset(H5O_LINK_ID, &dst_lnk);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_linfo_post_copy_file_cb() */


/*-------------------------------------------------------------------------
 * Function:    H5O_linfo_post_copy_file
 *
 * Purpose:     Finish copying a message from between files
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              September 26, 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_linfo_post_copy_file(const H5O_loc_t *src_oloc, const void *mesg_src,
    H5O_loc_t *dst_oloc, void *mesg_dst, hid_t dxpl_id, H5O_copy_t *cpy_info)
{
    const H5O_linfo_t   *linfo_src = (const H5O_linfo_t *)mesg_src;
    H5O_linfo_t         *linfo_dst = (H5O_linfo_t *)mesg_dst;
    herr_t ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_linfo_post_copy_file)

    /* check args */
    HDassert(src_oloc && src_oloc->file);
    HDassert(linfo_src);
    HDassert(dst_oloc && dst_oloc->file);
    HDassert(H5F_addr_defined(dst_oloc->addr));
    HDassert(linfo_dst);
    HDassert(cpy_info);

    /* If we are performing a 'shallow hierarchy' copy, get out now */
    if(cpy_info->max_depth >= 0 && cpy_info->curr_depth >= cpy_info->max_depth)
        HGOTO_DONE(SUCCEED)

    /* Check for copying dense link storage */
    if(H5F_addr_defined(linfo_src->link_fheap_addr)) {
        H5O_linfo_postcopy_ud_t udata;          /* User data for iteration callback */
        H5G_link_iterate_t lnk_op;              /* Link operator */

        /* Set up dense link iteration user data */
        udata.src_oloc = src_oloc;
        udata.dst_oloc = dst_oloc;
        udata.dst_linfo = linfo_dst;
        udata.dxpl_id = dxpl_id;
        udata.cpy_info = cpy_info;

        /* Build iterator operator */
        lnk_op.op_type = H5G_LINK_OP_LIB;
        lnk_op.u.lib_op = H5O_linfo_post_copy_file_cb;

        /* Iterate over the links in the group, building a table of the link messages */
        if(H5G_dense_iterate(src_oloc->file, dxpl_id, linfo_src, H5_INDEX_NAME, H5_ITER_NATIVE, (hsize_t)0, NULL, (hid_t)0, &lnk_op, &udata) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTNEXT, FAIL, "error iterating over links")
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5O_linfo_post_copy_file() */


/*-------------------------------------------------------------------------
 * Function:    H5O_linfo_debug
 *
 * Purpose:     Prints debugging info for a message.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              koziol@ncsa.uiuc.edu
 *              Aug 23 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_linfo_debug(H5F_t UNUSED *f, hid_t UNUSED dxpl_id, const void *_mesg, FILE * stream,
	       int indent, int fwidth)
{
    const H5O_linfo_t       *linfo = (const H5O_linfo_t *) _mesg;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_linfo_debug)

    /* check args */
    HDassert(f);
    HDassert(linfo);
    HDassert(stream);
    HDassert(indent >= 0);
    HDassert(fwidth >= 0);

    HDfprintf(stream, "%*s%-*s %t\n", indent, "", fwidth,
	      "Index creation order of links:", linfo->index_corder);
    HDfprintf(stream, "%*s%-*s %Hu\n", indent, "", fwidth,
	      "Number of links:", linfo->nlinks);
    HDfprintf(stream, "%*s%-*s %Hd\n", indent, "", fwidth,
	      "Min. creation order value:", linfo->min_corder);
    HDfprintf(stream, "%*s%-*s %Hd\n", indent, "", fwidth,
	      "Max. creation order value:", linfo->max_corder);
    HDfprintf(stream, "%*s%-*s %a\n", indent, "", fwidth,
	      "'Dense' link storage fractal heap address:", linfo->link_fheap_addr);
    HDfprintf(stream, "%*s%-*s %a\n", indent, "", fwidth,
	      "'Dense' link storage name index v2 B-tree address:", linfo->name_bt2_addr);
    HDfprintf(stream, "%*s%-*s %a\n", indent, "", fwidth,
	      "'Dense' link storage creation order index v2 B-tree address:", linfo->corder_bt2_addr);


    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5O_linfo_debug() */

