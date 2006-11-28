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
 * Created:             H5Oginfo.c
 *                      Aug 23 2005
 *                      Quincey Koziol <koziol@ncsa.uiuc.edu>
 *
 * Purpose:             Group Information messages.
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
static void *H5O_ginfo_decode(H5F_t *f, hid_t dxpl_id, const uint8_t *p);
static herr_t H5O_ginfo_encode(H5F_t *f, uint8_t *p, const void *_mesg);
static void *H5O_ginfo_copy(const void *_mesg, void *_dest, unsigned update_flags);
static size_t H5O_ginfo_size(const H5F_t *f, const void *_mesg);
static herr_t H5O_ginfo_free(void *_mesg);
static herr_t H5O_ginfo_debug(H5F_t *f, hid_t dxpl_id, const void *_mesg,
			     FILE * stream, int indent, int fwidth);

/* This message derives from H5O message class */
const H5O_msg_class_t H5O_MSG_GINFO[1] = {{
    H5O_GINFO_ID,            	/*message id number             */
    "ginfo",                 	/*message name for debugging    */
    sizeof(H5O_ginfo_t),     	/*native message size           */
    H5O_ginfo_decode,        	/*decode message                */
    H5O_ginfo_encode,        	/*encode message                */
    H5O_ginfo_copy,          	/*copy the native value         */
    H5O_ginfo_size,          	/*size of symbol table entry    */
    NULL,                   	/*default reset method          */
    H5O_ginfo_free,	        /* free method			*/
    NULL,	        	/* file delete method		*/
    NULL,			/* link method			*/
    NULL,		    	/*get share method		*/
    NULL, 			/*set share method		*/
    NULL, 			/*is shared method		*/
    NULL,			/* pre copy native value to file */
    NULL,			/* copy native value to file    */
    NULL,			/* post copy native value to file    */
    H5O_ginfo_debug          	/*debug the message             */
}};

/* Current version of group info information */
#define H5O_GINFO_VERSION 	1

/* Flags for group info flag encoding */
#define H5O_GINFO_FLAG_TRACK_NAME          0x01
#define H5O_GINFO_FLAG_TRACK_CORDER        0x02

/* Declare a free list to manage the H5O_ginfo_t struct */
H5FL_DEFINE_STATIC(H5O_ginfo_t);


/*-------------------------------------------------------------------------
 * Function:    H5O_ginfo_decode
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
 *              Aug 30 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void *
H5O_ginfo_decode(H5F_t UNUSED *f, hid_t UNUSED dxpl_id, const uint8_t *p)
{
    H5O_ginfo_t         *ginfo = NULL;  /* Pointer to group information message */
    unsigned char       flags;          /* Flags for encoding group info */
    void                *ret_value;     /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_ginfo_decode)

    /* check args */
    HDassert(p);

    /* Version of message */
    if(*p++ != H5O_GINFO_VERSION)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, NULL, "bad version number for message")

    /* Allocate space for message */
    if(NULL == (ginfo = H5FL_CALLOC(H5O_ginfo_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* Get the flags for the group */
    flags = *p++;
    HDassert(flags & H5O_GINFO_FLAG_TRACK_NAME);
    ginfo->track_corder = (flags & H5O_GINFO_FLAG_TRACK_CORDER) ? TRUE : FALSE;

    /* Get the max. # of links to store compactly & the min. # of links to store densely */
    UINT16DECODE(p, ginfo->max_compact)
    UINT16DECODE(p, ginfo->min_dense)

    /* Get the estimated # of entries & name lengths */
    UINT16DECODE(p, ginfo->est_num_entries)
    UINT16DECODE(p, ginfo->est_name_len)

    /* Set return value */
    ret_value = ginfo;

done:
    if(ret_value == NULL)
        if(ginfo != NULL)
            H5FL_FREE(H5O_ginfo_t, ginfo);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_ginfo_decode() */


/*-------------------------------------------------------------------------
 * Function:    H5O_ginfo_encode
 *
 * Purpose:     Encodes a message.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              koziol@ncsa.uiuc.edu
 *              Aug 30 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_ginfo_encode(H5F_t UNUSED *f, uint8_t *p, const void *_mesg)
{
    const H5O_ginfo_t  *ginfo = (const H5O_ginfo_t *) _mesg;
    unsigned char       flags;          /* Flags for encoding group info */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_ginfo_encode)

    /* check args */
    HDassert(p);
    HDassert(ginfo);

    /* Message version */
    *p++ = H5O_GINFO_VERSION;

    /* The flags for the group info */
    flags = H5O_GINFO_FLAG_TRACK_NAME;          /* Names are always tracked */
    flags |= ginfo->track_corder ? H5O_GINFO_FLAG_TRACK_CORDER : 0;
    *p++ = flags;

    /* Store the max. # of links to store compactly & the min. # of links to store densely */
    UINT16ENCODE(p, ginfo->max_compact)
    UINT16ENCODE(p, ginfo->min_dense)

    /* Estimated # of entries & name lengths */
    UINT16ENCODE(p, ginfo->est_num_entries)
    UINT16ENCODE(p, ginfo->est_name_len)

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5O_ginfo_encode() */


/*-------------------------------------------------------------------------
 * Function:    H5O_ginfo_copy
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
 *              Aug 30 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void *
H5O_ginfo_copy(const void *_mesg, void *_dest, unsigned UNUSED update_flags)
{
    const H5O_ginfo_t   *ginfo = (const H5O_ginfo_t *)_mesg;
    H5O_ginfo_t         *dest = (H5O_ginfo_t *)_dest;
    void                *ret_value;     /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_ginfo_copy)

    /* check args */
    HDassert(ginfo);
    if(!dest && NULL == (dest = H5FL_MALLOC(H5O_ginfo_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* copy */
    *dest = *ginfo;

    /* Set return value */
    ret_value = dest;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_ginfo_copy() */


/*-------------------------------------------------------------------------
 * Function:    H5O_ginfo_size
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
 *              Aug 30 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static size_t
H5O_ginfo_size(const H5F_t UNUSED *f, const void UNUSED *_mesg)
{
    size_t ret_value;   /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_ginfo_size)

    /* Set return value */
    ret_value = 1 +                     /* Version */
                1 +                     /* Flags */
                2 +                     /* "Max compact" links */
                2 +                     /* "Min dense" links */
                2 +                     /* Estimated # of entries in group */
                2;                      /* Estimated length of name of entry in group */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_ginfo_size() */


/*-------------------------------------------------------------------------
 * Function:	H5O_ginfo_free
 *
 * Purpose:	Free's the message
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, August 30, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_ginfo_free(void *mesg)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_ginfo_free)

    HDassert(mesg);

    H5FL_FREE(H5O_ginfo_t, mesg);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5O_ginfo_free() */


/*-------------------------------------------------------------------------
 * Function:    H5O_ginfo_debug
 *
 * Purpose:     Prints debugging info for a message.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              koziol@ncsa.uiuc.edu
 *              Aug 30 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_ginfo_debug(H5F_t UNUSED *f, hid_t UNUSED dxpl_id, const void *_mesg, FILE * stream,
	       int indent, int fwidth)
{
    const H5O_ginfo_t       *ginfo = (const H5O_ginfo_t *) _mesg;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_ginfo_debug)

    /* check args */
    HDassert(f);
    HDassert(ginfo);
    HDassert(stream);
    HDassert(indent >= 0);
    HDassert(fwidth >= 0);

    HDfprintf(stream, "%*s%-*s %t\n", indent, "", fwidth,
	      "Track creation order of links:", ginfo->track_corder);
    HDfprintf(stream, "%*s%-*s %u\n", indent, "", fwidth,
	      "Max. compact links:", ginfo->max_compact);
    HDfprintf(stream, "%*s%-*s %u\n", indent, "", fwidth,
	      "Min. dense links:", ginfo->min_dense);
    HDfprintf(stream, "%*s%-*s %u\n", indent, "", fwidth,
	      "Estimated # of objects in group:", ginfo->est_num_entries);
    HDfprintf(stream, "%*s%-*s %u\n", indent, "", fwidth,
	      "Estimated length of object in group's name:", ginfo->est_name_len);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5O_ginfo_debug() */

