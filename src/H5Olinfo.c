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

#define H5O_PACKAGE	/*suppress error about including H5Opkg	  */

#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5FLprivate.h"	/* Free lists                           */
#include "H5Opkg.h"             /* Object headers			*/


/* PRIVATE PROTOTYPES */
static void *H5O_linfo_decode(H5F_t *f, hid_t dxpl_id, const uint8_t *p);
static herr_t H5O_linfo_encode(H5F_t *f, uint8_t *p, const void *_mesg);
static void *H5O_linfo_copy(const void *_mesg, void *_dest, unsigned update_flags);
static size_t H5O_linfo_size(const H5F_t *f, const void *_mesg);
static herr_t H5O_linfo_free(void *_mesg);
static herr_t H5O_linfo_debug(H5F_t *f, hid_t dxpl_id, const void *_mesg,
			     FILE * stream, int indent, int fwidth);

/* This message derives from H5O */
const H5O_class_t H5O_LINFO[1] = {{
    H5O_LINFO_ID,            	/*message id number             */
    "linfo",                 	/*message name for debugging    */
    sizeof(H5O_linfo_t),     	/*native message size           */
    H5O_linfo_decode,        	/*decode message                */
    H5O_linfo_encode,        	/*encode message                */
    H5O_linfo_copy,          	/*copy the native value         */
    H5O_linfo_size,          	/*size of symbol table entry    */
    NULL,                   	/*default reset method          */
    H5O_linfo_free,	        /* free method			*/
    NULL,	        	/* file delete method		*/
    NULL,			/* link method			*/
    NULL,		    	/*get share method		*/
    NULL, 			/*set share method		*/
    NULL,			/* copy native value to file    */
    NULL,			/* post copy native value to file    */
    H5O_linfo_debug          	/*debug the message             */
}};

/* Current version of link info information */
#define H5O_LINFO_VERSION 	1

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
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void *
H5O_linfo_decode(H5F_t *f, hid_t UNUSED dxpl_id, const uint8_t *p)
{
    H5O_linfo_t	*linfo = NULL;  /* Link info */
    void        *ret_value;     /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_linfo_decode)

    /* check args */
    HDassert(f);
    HDassert(p);

    /* decode */
    if(*p++ != H5O_LINFO_VERSION)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, NULL, "bad version number for message")

    /* Allocate space for message */
    if (NULL == (linfo = H5FL_MALLOC(H5O_linfo_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* Get the number of links in the group */
    H5F_DECODE_LENGTH(f, p, linfo->nlinks)

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
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_linfo_encode(H5F_t *f, uint8_t *p, const void *_mesg)
{
    const H5O_linfo_t       *linfo = (const H5O_linfo_t *) _mesg;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_linfo_encode)

    /* check args */
    HDassert(f);
    HDassert(p);
    HDassert(linfo);

    /* encode */
    *p++ = H5O_LINFO_VERSION;

    /* Store the number of links in the group */
    H5F_ENCODE_LENGTH(f, p, linfo->nlinks)

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
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void *
H5O_linfo_copy(const void *_mesg, void *_dest, unsigned UNUSED update_flags)
{
    const H5O_linfo_t   *linfo = (const H5O_linfo_t *) _mesg;
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
    ret_value=dest;

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
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static size_t
H5O_linfo_size(const H5F_t *f, const void UNUSED *_mesg)
{
    size_t ret_value;   /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_linfo_size)

    /* Set return value */
    ret_value = 1 +                     /* Version */
                H5F_SIZEOF_SIZE(f);     /* Number of links */

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
 * Modifications:
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
 * Modifications:
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

    HDfprintf(stream, "%*s%-*s %Hu\n", indent, "", fwidth,
	      "Number of links:", linfo->nlinks);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5O_linfo_debug() */

