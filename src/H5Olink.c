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
 * Created:             H5Olink.c
 *                      Aug 29 2005
 *                      Quincey Koziol <koziol@ncsa.uiuc.edu>
 *
 * Purpose:             Link messages.
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

#define H5O_PACKAGE	/*suppress error about including H5Opkg	  */

#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5FLprivate.h"	/* Free lists                           */
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Opkg.h"             /* Object headers			*/


/* PRIVATE PROTOTYPES */
static void *H5O_link_decode(H5F_t *f, hid_t dxpl_id, const uint8_t *p);
static herr_t H5O_link_encode(H5F_t *f, uint8_t *p, const void *_mesg);
static void *H5O_link_copy(const void *_mesg, void *_dest, unsigned update_flags);
static size_t H5O_link_size(const H5F_t *f, const void *_mesg);
static herr_t H5O_link_reset(void *_mesg);
static herr_t H5O_link_free(void *_mesg);
static herr_t H5O_link_delete(H5F_t *f, hid_t dxpl_id, const void *_mesg, hbool_t adj_link);
static void *H5O_link_copy_file(H5F_t *file_src, void *native_src,
    H5F_t *file_dst, hid_t dxpl_id, H5SL_t *map_list, void *udata);
static herr_t H5O_link_post_copy_file(H5F_t *file_src, const void *mesg_src,
    H5O_loc_t *dst_oloc, void *mesg_dst, hbool_t *modified, hid_t dxpl_id, H5SL_t *map_list);
static herr_t H5O_link_debug(H5F_t *f, hid_t dxpl_id, const void *_mesg,
			     FILE * stream, int indent, int fwidth);

/* This message derives from H5O */
const H5O_class_t H5O_LINK[1] = {{
    H5O_LINK_ID,            	/*message id number             */
    "link",                 	/*message name for debugging    */
    sizeof(H5O_link_t),     	/*native message size           */
    H5O_link_decode,        	/*decode message                */
    H5O_link_encode,        	/*encode message                */
    H5O_link_copy,          	/*copy the native value         */
    H5O_link_size,          	/*size of symbol table entry    */
    H5O_link_reset,		/* reset method			*/
    H5O_link_free,	        /* free method			*/
    H5O_link_delete,	       	/* file delete method		*/
    NULL,			/* link method			*/
    NULL,		    	/*get share method		*/
    NULL, 			/*set share method		*/
    H5O_link_copy_file,		/* copy native value to file    */
    H5O_link_post_copy_file,	/* post copy native value to file    */
    H5O_link_debug          	/*debug the message             */
}};

/* Current version of link information */
#define H5O_LINK_VERSION 	1

/* Declare a free list to manage the H5O_link_t struct */
H5FL_DEFINE_STATIC(H5O_link_t);


/*-------------------------------------------------------------------------
 * Function:    H5O_link_decode
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
 *              Aug 29 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void *
H5O_link_decode(H5F_t *f, hid_t UNUSED dxpl_id, const uint8_t *p)
{
    H5O_link_t          *lnk = NULL;    /* Pointer to link message */
    uint16_t            len;            /* Length of a string in the message */
    uint32_t            tmp_time;       /* Temporary copy of the time */
    void                *ret_value;     /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_link_decode)

    /* check args */
    HDassert(f);
    HDassert(p);

    /* decode */
    if(*p++ != H5O_LINK_VERSION)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, NULL, "bad version number for message")

    /* Allocate space for message */
    if(NULL == (lnk = H5FL_CALLOC(H5O_link_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* Get the type of the link */
    lnk->type = *p++;
    if(lnk->type < H5G_LINK_HARD || lnk->type > H5G_LINK_SOFT)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, NULL, "bad link type")

    /* Get the link creation time from the file */
    UINT32DECODE(p, tmp_time)
    lnk->ctime = (time_t)tmp_time;

    /* Get the link name's character set */
    lnk->cset = (H5T_cset_t)*p++;
    if(lnk->cset < H5T_CSET_ASCII || lnk->cset > H5T_CSET_UTF8)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, NULL, "bad cset type")

    /* Get the link's name */
    UINT16DECODE(p, len)
    if(len == 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, NULL, "invalid name length")
    if(NULL == (lnk->name = H5MM_malloc((size_t)len + 1)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")
    HDmemcpy(lnk->name, p, len);
    lnk->name[len] = '\0';
    p += len;

    /* Get the appropriate information for each type of link */
    switch(lnk->type) {
        case H5G_LINK_HARD:
            /* Get the address of the object the link points to */
            H5F_addr_decode(f, &p, &(lnk->u.hard.addr));
            break;

        case H5G_LINK_SOFT:
            /* Get the link value */
            UINT16DECODE(p, len)
            if(len == 0)
                HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, NULL, "invalid link length")
            if(NULL == (lnk->u.soft.name = H5MM_malloc((size_t)len + 1)))
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")
            HDmemcpy(lnk->u.soft.name, p, len);
            lnk->u.soft.name[len] = '\0';
            p += len;
            break;

        default:
            HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, NULL, "unknown link type")
            break;
    } /* end switch */

    /* Set return value */
    ret_value=lnk;

done:
    if(ret_value == NULL)
        if(lnk != NULL) {
            if(lnk->name != NULL)
                H5MM_xfree(lnk->name);
            if(lnk->type == H5G_LINK_SOFT && lnk->u.soft.name != NULL)
                H5MM_xfree(lnk->u.soft.name);
            H5FL_FREE(H5O_link_t, lnk);
        } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_link_decode() */


/*-------------------------------------------------------------------------
 * Function:    H5O_link_encode
 *
 * Purpose:     Encodes a link message.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              koziol@ncsa.uiuc.edu
 *              Aug 29 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_link_encode(H5F_t *f, uint8_t *p, const void *_mesg)
{
    const H5O_link_t       *lnk = (const H5O_link_t *) _mesg;
    uint16_t                len;            /* Length of a string in the message */
    uint32_t                tmp_time;       /* Temporary copy of the time */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_link_encode)

    /* check args */
    HDassert(f);
    HDassert(p);
    HDassert(lnk);

    /* encode */
    *p++ = H5O_LINK_VERSION;

    /* Store the type of the link */
    *p++ = lnk->type;

    /* Store the link creation time from the file */
    tmp_time = lnk->ctime;
    UINT32ENCODE(p, tmp_time)

    /* Store the link name's character set */
    *p++ = (uint8_t)lnk->cset;

    /* Store the link's name */
    len = (uint16_t)HDstrlen(lnk->name);
    HDassert(len > 0);
    UINT16ENCODE(p, len)
    HDmemcpy(p, lnk->name, len);
    p += len;

    /* Store the appropriate information for each type of link */
    switch(lnk->type) {
        case H5G_LINK_HARD:
            /* Store the address of the object the link points to */
            H5F_addr_encode(f, &p, lnk->u.hard.addr);
            break;

        case H5G_LINK_SOFT:
            /* Store the link value */
            len = (uint16_t)HDstrlen(lnk->u.soft.name);
            HDassert(len > 0);
            UINT16ENCODE(p, len)
            HDmemcpy(p, lnk->u.soft.name, len);
            p += len;
            break;

        default:
            HDassert((lnk->type == H5G_LINK_HARD) || (lnk->type == H5G_LINK_SOFT));
            break;
    } /* end switch */

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5O_link_encode() */


/*-------------------------------------------------------------------------
 * Function:    H5O_link_copy
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
 *              Aug 29 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void *
H5O_link_copy(const void *_mesg, void *_dest, unsigned UNUSED update_flags)
{
    const H5O_link_t    *lnk = (const H5O_link_t *) _mesg;
    H5O_link_t          *dest = (H5O_link_t *) _dest;
    void                *ret_value;     /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_link_copy)

    /* check args */
    HDassert(lnk);
    if(!dest && NULL == (dest = H5FL_MALLOC(H5O_link_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* copy */
    *dest = *lnk;
    HDassert(lnk->name);
    dest->name = H5MM_xstrdup(lnk->name);
    if(lnk->type == H5G_LINK_SOFT)
        dest->u.soft.name = H5MM_xstrdup(lnk->u.soft.name);

    /* Set return value */
    ret_value=dest;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_link_copy() */


/*-------------------------------------------------------------------------
 * Function:    H5O_link_size
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
 *              Aug 29 2005
 *
 *-------------------------------------------------------------------------
 */
static size_t
H5O_link_size(const H5F_t *f, const void *_mesg)
{
    const H5O_link_t *lnk = (const H5O_link_t *)_mesg;
    size_t ret_value;   /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_link_size)

    /* Set return value */
    ret_value = 1 +                     /* Version */
                1 +                     /* Link type */
                4 +                     /* Creation time */
                1 +                     /* Character set */
                2 +                     /* Name length */
                HDstrlen(lnk->name);    /* Name */

    /* Add the appropriate length for each type of link */
    switch(lnk->type) {
        case H5G_LINK_HARD:
            ret_value += H5F_SIZEOF_ADDR(f);
            break;

        case H5G_LINK_SOFT:
            ret_value += 2 +                            /* Link value length */
                        HDstrlen(lnk->u.soft.name);     /* Link value */
            break;

        default:
            HDassert((lnk->type == H5G_LINK_HARD) || (lnk->type == H5G_LINK_SOFT));
            break;
    } /* end switch */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_link_size() */


/*-------------------------------------------------------------------------
 * Function:	H5O_link_reset
 *
 * Purpose:	Frees resources within a message, but doesn't free
 *		the message itself.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Monday, August 29, 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_link_reset(void *_mesg)
{
    H5O_link_t *lnk = (H5O_link_t *)_mesg;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_link_reset)

    if(lnk) {
        /* Free information for link (but don't free link pointer) */
        if(lnk->type == H5G_LINK_SOFT)
            lnk->u.soft.name = H5MM_xfree(lnk->u.soft.name);
        lnk->name = H5MM_xfree(lnk->name);
    } /* end if */

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5O_link_reset() */


/*-------------------------------------------------------------------------
 * Function:	H5O_link_free
 *
 * Purpose:	Free's the message contents and the message itself
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Monday, August 29, 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_link_free(void *_mesg)
{
    H5O_link_t *lnk = (H5O_link_t *)_mesg;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_link_free)

    HDassert(lnk);

    /* Free information for link */
    if(lnk->type == H5G_LINK_SOFT)
        lnk->u.soft.name = H5MM_xfree(lnk->u.soft.name);
    lnk->name = H5MM_xfree(lnk->name);
    H5FL_FREE(H5O_link_t, lnk);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5O_link_free() */


/*-------------------------------------------------------------------------
 * Function:    H5O_link_delete
 *
 * Purpose:     Free file space referenced by message
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              Monday, August 29, 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_link_delete(H5F_t *f, hid_t dxpl_id, const void *_mesg, hbool_t adj_link)
{
    const H5O_link_t *lnk = (const H5O_link_t *)_mesg;
    herr_t ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_link_delete)

    /* check args */
    HDassert(f);
    HDassert(lnk);

    /* Decrement reference count to the object, for hard links */
    if(lnk->type == H5G_LINK_HARD) {
        H5O_loc_t oloc;

        /* Construct object location for object, in order to decrement it's ref count */
        H5O_loc_reset(&oloc);
        oloc.file = f;
        HDassert(H5F_addr_defined(lnk->u.hard.addr));
        oloc.addr = lnk->u.hard.addr;

        /* Decrement the ref count for the object, if requested */
        if(adj_link)
            if(H5O_link(&oloc, -1, dxpl_id)<0)
                HGOTO_ERROR(H5E_OHDR, H5E_CANTFREE, FAIL, "unable to decrement object link count")
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_link_delete() */


/*-------------------------------------------------------------------------
 * Function:    H5O_link_copy_file
 *
 * Purpose:     Copies a message from _MESG to _DEST in file
 *
 * Return:      Success:        Ptr to _DEST
 *
 *              Failure:        NULL
 *
 * Programmer:  Quincey Koziol
 *              November  7, 2005 
 *
 *-------------------------------------------------------------------------
 */
static void *
H5O_link_copy_file(H5F_t UNUSED *file_src, void *native_src,
       H5F_t UNUSED *file_dst, hid_t UNUSED dxpl_id, H5SL_t UNUSED *map_list, void UNUSED *udata)
{
    H5O_link_t           *link_src = (H5O_link_t *) native_src;
    H5O_link_t           *link_dst = NULL;
    void                 *ret_value;          /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_link_copy_file)

    /* check args */
    HDassert(link_src);
    HDassert(file_dst);

    /* Allocate space for the destination stab */
    if(NULL == (link_dst = H5FL_MALLOC(H5O_link_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* Copy top-level information */
    *link_dst = *link_src;

    /* Deep copy the link's name */
    if(NULL == (link_dst->name = H5MM_xstrdup(link_src->name)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* "Deep copy" other information for each kind of link */
    switch(link_src->type) {
        case H5G_LINK_HARD:
            /* Set link's address undefined here, will be fixed up in "post copy" callback */
            link_dst->u.hard.addr = HADDR_UNDEF;
            break;

        case H5G_LINK_SOFT:
            /* Copy the soft link's value */
            if(NULL == (link_dst->u.soft.name = H5MM_xstrdup(link_src->u.soft.name)))
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")
            break;

        default:
            HGOTO_ERROR(H5E_SYM, H5E_BADVALUE, NULL, "unrecognized link type")
    } /* end switch */

    /* Set return value */
    ret_value = link_dst;

done:
    if(!ret_value)
        if(link_dst)
            H5FL_FREE(H5O_link_t, link_dst);

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5O_link_copy_file() */


/*-------------------------------------------------------------------------
 * Function:    H5O_link_post_copy_file
 *
 * Purpose:     Finish copying a message from between files
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              November  7, 2005 
 *
 *-------------------------------------------------------------------------
 */
static herr_t 
H5O_link_post_copy_file(H5F_t *file_src, const void *mesg_src,
    H5O_loc_t *dst_oloc, void *mesg_dst, hbool_t *modified, hid_t dxpl_id, H5SL_t *map_list)
{
    const H5O_link_t     *link_src = (const H5O_link_t *)mesg_src;
    H5O_link_t           *link_dst = (H5O_link_t *)mesg_dst;
    herr_t               ret_value = SUCCEED;          /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_link_post_copy_file)

    /* check args */
    HDassert(link_src);
    HDassert(dst_oloc);
    HDassert(H5F_addr_defined(dst_oloc->addr));
    HDassert(dst_oloc->file);
    HDassert(link_dst);
    HDassert(modified && *modified == FALSE);
    HDassert(map_list);

    /* Additional "deep copy" for each kind of link */
    switch(link_src->type) {
        case H5G_LINK_HARD:
            /* Copy the object pointed to */
            {
                H5O_loc_t src_oloc;             /* Temporary object location for source object */
                H5O_loc_t new_oloc;             /* Temporary object location for source object */

                /* Build temporary object location for source */
                H5O_loc_reset(&src_oloc);
                src_oloc.file = file_src;
                HDassert(H5F_addr_defined(link_src->u.hard.addr));
                src_oloc.addr = link_src->u.hard.addr;

                /* Build temporary object location for destination */
                H5O_loc_reset(&new_oloc);
                new_oloc.file = dst_oloc->file;

                /* Copy the shared object from source to destination */
                /* (Increments link count on destination) */
                if(H5O_copy_header_map(&src_oloc, &new_oloc, dxpl_id, map_list) < 0)
                    HGOTO_ERROR(H5E_OHDR, H5E_CANTCOPY, FAIL, "unable to copy object")

                /* Update link information with new destination object's address */
                link_dst->u.hard.addr = new_oloc.addr;

                /* Indicate that the destination message was modified */
                *modified = TRUE;
            } /* end case */
            break;

        case H5G_LINK_SOFT:
            HGOTO_DONE(SUCCEED)
            break;

        default:
            HGOTO_ERROR(H5E_SYM, H5E_BADVALUE, FAIL, "unrecognized link type")
    } /* end switch */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5O_link_post_copy_file() */


/*-------------------------------------------------------------------------
 * Function:    H5O_link_debug
 *
 * Purpose:     Prints debugging info for a message.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              koziol@ncsa.uiuc.edu
 *              Aug 29 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_link_debug(H5F_t UNUSED *f, hid_t UNUSED dxpl_id, const void *_mesg, FILE * stream,
	       int indent, int fwidth)
{
    const H5O_link_t       *lnk = (const H5O_link_t *) _mesg;
    struct tm		*tm;
    char		buf[128];

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_link_debug)

    /* check args */
    HDassert(f);
    HDassert(lnk);
    HDassert(stream);
    HDassert(indent >= 0);
    HDassert(fwidth >= 0);

    HDfprintf(stream, "%*s%-*s %s\n", indent, "", fwidth,
	      "Link Type:", (lnk->type == H5G_LINK_HARD ? "Hard" :
                  (lnk->type == H5G_LINK_SOFT ? "Soft" : "Unknown")));

    tm = HDlocaltime(&(lnk->ctime));
    HDstrftime(buf, sizeof(buf), "%Y-%m-%d %H:%M:%S %Z", tm);
    HDfprintf(stream, "%*s%-*s %s\n", indent, "", fwidth,
	      "Creation Time:", buf);

    HDfprintf(stream, "%*s%-*s %s\n", indent, "", fwidth,
	      "Link Name Character Set:", (lnk->cset == H5T_CSET_ASCII ?
                "ASCII" : (lnk->cset == H5T_CSET_UTF8 ? "UTF-8" : "Unknown")));
    HDfprintf(stream, "%*s%-*s %s\n", indent, "", fwidth,
	      "Link Name:", lnk->name);

    switch(lnk->type) {
        case H5G_LINK_HARD:
            HDfprintf(stream, "%*s%-*s %a\n", indent, "", fwidth,
                      "Object address:", lnk->u.hard.addr);
            break;

        case H5G_LINK_SOFT:
            HDfprintf(stream, "%*s%-*s %s\n", indent, "", fwidth,
                      "Link Value:", lnk->u.soft.name);
            break;

        default:
            break;
    } /* end switch */

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5O_link_debug() */

