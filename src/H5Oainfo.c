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
 * Created:             H5Oainfo.c
 *                      Mar  6 2007
 *                      Quincey Koziol <koziol@hdfgroup.org>
 *
 * Purpose:             Attribute Information messages.
 *
 *-------------------------------------------------------------------------
 */

#define H5O_PACKAGE		/*suppress error about including H5Opkg	  */

#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5FLprivate.h"	/* Free lists                           */
#include "H5Opkg.h"             /* Object headers			*/


/* PRIVATE PROTOTYPES */
static void *H5O_ainfo_decode(H5F_t *f, hid_t dxpl_id, unsigned mesg_flags, const uint8_t *p);
static herr_t H5O_ainfo_encode(H5F_t *f, hbool_t disable_shared, uint8_t *p, const void *_mesg);
static void *H5O_ainfo_copy(const void *_mesg, void *_dest);
static size_t H5O_ainfo_size(const H5F_t *f, hbool_t disable_shared, const void *_mesg);
static herr_t H5O_ainfo_free(void *_mesg);
static herr_t H5O_ainfo_delete(H5F_t *f, hid_t dxpl_id, const void *_mesg);
static herr_t H5O_ainfo_debug(H5F_t *f, hid_t dxpl_id, const void *_mesg,
			     FILE * stream, int indent, int fwidth);

/* This message derives from H5O message class */
const H5O_msg_class_t H5O_MSG_AINFO[1] = {{
    H5O_AINFO_ID,            	/*message id number             */
    "ainfo",                 	/*message name for debugging    */
    sizeof(H5O_ainfo_t),     	/*native message size           */
    FALSE,			/* messages are sharable?       */
    H5O_ainfo_decode,        	/*decode message                */
    H5O_ainfo_encode,        	/*encode message                */
    H5O_ainfo_copy,          	/*copy the native value         */
    H5O_ainfo_size,          	/*size of symbol table entry    */
    NULL,                   	/*default reset method          */
    H5O_ainfo_free,	        /* free method			*/
    H5O_ainfo_delete,	        /* file delete method		*/
    NULL,			/* link method			*/
    NULL, 			/*set share method		*/
    NULL,		    	/*can share method		*/
    NULL,			/* pre copy native value to file */
    NULL,			/* copy native value to file    */
    NULL,			/* post copy native value to file */
    NULL,			/* get creation index		*/
    NULL,			/* set creation index		*/
    H5O_ainfo_debug          	/*debug the message             */
}};

/* Current version of attribute info information */
#define H5O_AINFO_VERSION 	0

/* Flags for attribute info index flag encoding */
#define H5O_AINFO_INDEX_NAME            0x01
#define H5O_AINFO_INDEX_CORDER          0x02
#define H5O_AINFO_ALL_FLAGS             (H5O_AINFO_INDEX_NAME | H5O_AINFO_INDEX_CORDER)

/* Declare a free list to manage the H5O_ainfo_t struct */
H5FL_DEFINE_STATIC(H5O_ainfo_t);


/*-------------------------------------------------------------------------
 * Function:    H5O_ainfo_decode
 *
 * Purpose:     Decode a message and return a pointer to a newly allocated one.
 *
 * Return:      Success:        Ptr to new message in native form.
 *              Failure:        NULL
 *
 * Programmer:  Quincey Koziol
 *              koziol@hdfgroup.org
 *              Mar  6 2007
 *
 *-------------------------------------------------------------------------
 */
static void *
H5O_ainfo_decode(H5F_t *f, hid_t UNUSED dxpl_id, unsigned UNUSED mesg_flags,
    const uint8_t *p)
{
    H5O_ainfo_t	*ainfo = NULL;  /* Attribute info */
    unsigned char index_flags;  /* Flags for encoding link index info */
    void        *ret_value;     /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_ainfo_decode)

    /* check args */
    HDassert(f);
    HDassert(p);

    /* Version of message */
    if(*p++ != H5O_AINFO_VERSION)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, NULL, "bad version number for message")

    /* Allocate space for message */
    if(NULL == (ainfo = H5FL_MALLOC(H5O_ainfo_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* Get the index flags for the object */
    index_flags = *p++;
    HDassert(index_flags & H5O_AINFO_INDEX_NAME);
    if(index_flags & ~H5O_AINFO_ALL_FLAGS)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, NULL, "bad flag value for message")
    ainfo->index_corder = (index_flags & H5O_AINFO_INDEX_CORDER) ? TRUE : FALSE;

    /* Number of attributes on the object */
    H5F_DECODE_LENGTH(f, p, ainfo->nattrs)

    /* Max. creation order value for the object */
    UINT16DECODE(p, ainfo->max_crt_idx);

    /* Address of fractal heap to store "dense" attributes */
    H5F_addr_decode(f, &p, &(ainfo->fheap_addr));

    /* Address of v2 B-tree to index names of attributes (names are always indexed) */
    H5F_addr_decode(f, &p, &(ainfo->name_bt2_addr));

    /* Address of v2 B-tree to index creation order of links, if there is one */
    if(ainfo->index_corder)
        H5F_addr_decode(f, &p, &(ainfo->corder_bt2_addr));
    else
        ainfo->corder_bt2_addr = HADDR_UNDEF;

    /* Set return value */
    ret_value = ainfo;

done:
    if(ret_value == NULL)
        if(ainfo != NULL)
            H5FL_FREE(H5O_ainfo_t, ainfo);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_ainfo_decode() */


/*-------------------------------------------------------------------------
 * Function:    H5O_ainfo_encode
 *
 * Purpose:     Encodes a message.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              koziol@hdfgroup.org
 *              Mar  6 2007
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_ainfo_encode(H5F_t *f, hbool_t UNUSED disable_shared, uint8_t *p, const void *_mesg)
{
    const H5O_ainfo_t   *ainfo = (const H5O_ainfo_t *)_mesg;
    unsigned char       index_flags;          /* Flags for encoding attribute index info */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_ainfo_encode)

    /* check args */
    HDassert(f);
    HDassert(p);
    HDassert(ainfo);

    /* Message version */
    *p++ = H5O_AINFO_VERSION;

    /* The flags for the attribute indices */
    index_flags = H5O_AINFO_INDEX_NAME;       /* Names are always indexed */
    index_flags |= ainfo->index_corder ? H5O_AINFO_INDEX_CORDER : 0;
    *p++ = index_flags;

    /* Number of attributes on the object */
    H5F_ENCODE_LENGTH(f, p, ainfo->nattrs)

    /* Max. creation order value for the object */
    UINT16ENCODE(p, ainfo->max_crt_idx);

    /* Address of fractal heap to store "dense" attributes */
    H5F_addr_encode(f, &p, ainfo->fheap_addr);

    /* Address of v2 B-tree to index names of attributes */
    H5F_addr_encode(f, &p, ainfo->name_bt2_addr);

    /* Address of v2 B-tree to index creation order of attributes, if they are indexed */
    if(ainfo->index_corder)
        H5F_addr_encode(f, &p, ainfo->corder_bt2_addr);
    else
        HDassert(!H5F_addr_defined(ainfo->corder_bt2_addr));

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5O_ainfo_encode() */


/*-------------------------------------------------------------------------
 * Function:    H5O_ainfo_copy
 *
 * Purpose:     Copies a message from _MESG to _DEST, allocating _DEST if
 *              necessary.
 *
 * Return:      Success:        Ptr to _DEST
 *              Failure:        NULL
 *
 * Programmer:  Quincey Koziol
 *              koziol@hdfgroup.org
 *              Mar  6 2007
 *
 *-------------------------------------------------------------------------
 */
static void *
H5O_ainfo_copy(const void *_mesg, void *_dest)
{
    const H5O_ainfo_t   *ainfo = (const H5O_ainfo_t *)_mesg;
    H5O_ainfo_t         *dest = (H5O_ainfo_t *) _dest;
    void                *ret_value;     /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_ainfo_copy)

    /* check args */
    HDassert(ainfo);
    if(!dest && NULL == (dest = H5FL_MALLOC(H5O_ainfo_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* copy */
    *dest = *ainfo;

    /* Set return value */
    ret_value = dest;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_ainfo_copy() */


/*-------------------------------------------------------------------------
 * Function:    H5O_ainfo_size
 *
 * Purpose:     Returns the size of the raw message in bytes not counting
 *              the message type or size fields, but only the data fields.
 *              This function doesn't take into account alignment.
 *
 * Return:      Success:        Message data size in bytes without alignment.
 *              Failure:        zero
 *
 * Programmer:  Quincey Koziol
 *              koziol@hdfgroup.org
 *              Mar  6 2007
 *
 *-------------------------------------------------------------------------
 */
static size_t
H5O_ainfo_size(const H5F_t *f, hbool_t UNUSED disable_shared, const void *_mesg)
{
    const H5O_ainfo_t   *ainfo = (const H5O_ainfo_t *)_mesg;
    size_t ret_value;   /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_ainfo_size)

    /* Set return value */
    ret_value = 1                       /* Version */
                + 1                     /* Index flags */
                + H5F_SIZEOF_SIZE(f)    /* Number of attributes */
                + 2                     /* Curr. max. creation order value */
                + H5F_SIZEOF_ADDR(f)    /* Address of fractal heap to store "dense" attributes */
                + H5F_SIZEOF_ADDR(f)    /* Address of v2 B-tree for indexing names of attributes */
                + (ainfo->index_corder ? H5F_SIZEOF_ADDR(f) : 0);   /* Address of v2 B-tree for indexing creation order values of attributes */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_ainfo_size() */


/*-------------------------------------------------------------------------
 * Function:	H5O_ainfo_free
 *
 * Purpose:	Free's the message
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, March  6, 2007
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_ainfo_free(void *mesg)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_ainfo_free)

    HDassert(mesg);

    H5FL_FREE(H5O_ainfo_t, mesg);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5O_ainfo_free() */


/*-------------------------------------------------------------------------
 * Function:    H5O_ainfo_delete
 *
 * Purpose:     Free file space referenced by message
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, March  6, 2007
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_ainfo_delete(H5F_t *f, hid_t dxpl_id, const void *_mesg)
{
    const H5O_ainfo_t *ainfo = (const H5O_ainfo_t *)_mesg;
    herr_t ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_ainfo_delete)

    /* check args */
    HDassert(f);
    HDassert(ainfo);

    /* If the object is using "dense" attribute storage, delete it */
#ifdef NOT_YET
    if(H5F_addr_defined(ainfo->fheap_addr))
        if(H5G_dense_delete(f, dxpl_id, (H5O_linfo_t *)linfo, TRUE) < 0)   /* Casting away const OK - QAK */
            HGOTO_ERROR(H5E_OHDR, H5E_CANTFREE, FAIL, "unable to free dense link storage")
#endif /* NOT_YET */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_ainfo_delete() */


/*-------------------------------------------------------------------------
 * Function:    H5O_ainfo_debug
 *
 * Purpose:     Prints debugging info for a message.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              koziol@hdfgroup.org
 *              Mar  6 2007
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_ainfo_debug(H5F_t UNUSED *f, hid_t UNUSED dxpl_id, const void *_mesg, FILE * stream,
	       int indent, int fwidth)
{
    const H5O_ainfo_t       *ainfo = (const H5O_ainfo_t *) _mesg;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_ainfo_debug)

    /* check args */
    HDassert(f);
    HDassert(ainfo);
    HDassert(stream);
    HDassert(indent >= 0);
    HDassert(fwidth >= 0);

    HDfprintf(stream, "%*s%-*s %t\n", indent, "", fwidth,
	      "Index creation order of links:", ainfo->index_corder);
    HDfprintf(stream, "%*s%-*s %Hu\n", indent, "", fwidth,
	      "Number of attributes:", ainfo->nattrs);
    HDfprintf(stream, "%*s%-*s %u\n", indent, "", fwidth,
	      "Max. creation index value:", (unsigned)ainfo->max_crt_idx);
    HDfprintf(stream, "%*s%-*s %a\n", indent, "", fwidth,
	      "'Dense' attribute storage fractal heap address:", ainfo->fheap_addr);
    HDfprintf(stream, "%*s%-*s %a\n", indent, "", fwidth,
	      "'Dense' attribute storage name index v2 B-tree address:", ainfo->name_bt2_addr);
    HDfprintf(stream, "%*s%-*s %a\n", indent, "", fwidth,
	      "'Dense' attribute storage creation order index v2 B-tree address:", ainfo->corder_bt2_addr);


    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5O_ainfo_debug() */

