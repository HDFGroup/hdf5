/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/* Programmer:  Houjun Tang <htang4@lbl.gov>
 *              May, 2017
 *
 * Purpose:	A message holding SWMR delta t value
 *              information in the superblock extension.
 */

#include "H5Omodule.h"          /* This source code file is part of the H5O module */


#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Opkg.h"             /* Object headers			*/
#include "H5MMprivate.h"	/* Memory management			*/

static void  *H5O_swmr_deltat_decode(H5F_t *f, hid_t dxpl_id, H5O_t *open_oh,
    unsigned mesg_flags, unsigned *ioflags, const uint8_t *p);
static herr_t H5O_swmr_deltat_encode(H5F_t *f, hbool_t disable_shared, uint8_t *p, const void *_mesg);
static void  *H5O_swmr_deltat_copy(const void *_mesg, void *_dest);
static size_t H5O_swmr_deltat_size(const H5F_t *f, hbool_t disable_shared, const void *_mesg);
static herr_t H5O_swmr_deltat_debug(H5F_t *f, hid_t dxpl_id, const void *_mesg, FILE *stream,
			     int indent, int fwidth);

/* This message derives from H5O message class */
const H5O_msg_class_t H5O_MSG_SWMR_DELTAT[1] = {{
    H5O_SWMR_DELTAT_ID,         /* message id number                     */
    "SWMR delta t",             /* message name for debugging            */
    sizeof(unsigned),		/* native message size                   */
    0,				/* messages are sharable?       */
    H5O_swmr_deltat_decode,     /* decode message                        */
    H5O_swmr_deltat_encode,     /* encode message                        */
    H5O_swmr_deltat_copy,       /* copy the native value                 */
    H5O_swmr_deltat_size,       /* raw message size			*/
    NULL,                       /* free internal memory			*/
    NULL,                       /* free method				*/
    NULL,			/* file delete method			*/
    NULL,			/* link method				*/
    NULL,			/* set share method		        */
    NULL,		    	/* can share method		        */
    NULL,			/* pre copy native value to file	*/
    NULL,			/* copy native value to file		*/
    NULL,			/* post copy native value to file	*/
    NULL,			/* get creation index		        */
    NULL,			/* set creation index		        */
    H5O_swmr_deltat_debug            /*debug the message			*/
}};

/* Current version of SWMR delta t value information */
#define H5O_SWMR_DELTAT_VERSION 	0


/*-------------------------------------------------------------------------
 * Function:	H5O_swmr_deltat_decode
 *
 * Purpose:	Decode a shared message table message and return a pointer
 *              to a newly allocated H5O_swmr_deltat_t struct.
 *
 * Return:	Success:	Ptr to new message in native struct.
 *		Failure:	NULL
 *
 * Programmer:  Houjun Tang
 *              May 5, 2017
 *
 *-------------------------------------------------------------------------
 */
static void *
H5O_swmr_deltat_decode(H5F_t H5_ATTR_UNUSED *f, hid_t H5_ATTR_UNUSED dxpl_id, H5O_t H5_ATTR_UNUSED *open_oh,
    unsigned H5_ATTR_UNUSED mesg_flags, unsigned H5_ATTR_UNUSED *ioflags, const uint8_t *p)
{

    H5O_swmr_deltat_t	*swmr_deltat;          /* Native message */
    void		*ret_value = NULL;      /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Sanity check */
    HDassert(f);
    HDassert(p);

    /* Version of message */
    if(*p++ != H5O_SWMR_DELTAT_VERSION)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, NULL, "bad version number for message")

    /* Allocate space for message */
    if(NULL == (swmr_deltat = (H5O_swmr_deltat_t *)H5MM_calloc(sizeof(H5O_swmr_deltat_t))))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed for v1 B-tree 'K' message")

    /* Retrieve non-default SWMR delta t values */
    UINT32DECODE(p, *swmr_deltat);

    /* Set return value */
    ret_value = (void *)swmr_deltat;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_swmr_deltat_decode() */


/*-------------------------------------------------------------------------
 * Function:	H5O_swmr_deltat_encode
 *
 * Purpose:	Encode a SWMR delta t value message.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:  Houjun Tang
 *              May 5, 2017
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_swmr_deltat_encode(H5F_t H5_ATTR_UNUSED *f, hbool_t H5_ATTR_UNUSED disable_shared, uint8_t *p, const void *_mesg)
{
    const H5O_swmr_deltat_t *swmr_deltat = (const H5O_swmr_deltat_t *)_mesg;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Sanity check */
    HDassert(f);
    HDassert(p);
    HDassert(swmr_deltat);

    /* Store version and non-default v1 B-tree 'K' values */
    *p++ = H5O_SWMR_DELTAT_VERSION;
    UINT32ENCODE(p, *swmr_deltat);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5O_swmr_deltat_encode() */


/*-------------------------------------------------------------------------
 * Function:	H5O_swmr_deltat_copy
 *
 * Purpose:	Copies a message from _MESG to _DEST, allocating _DEST if
 *		necessary.
 *
 * Return:	Success:	Ptr to _DEST
 *		Failure:	NULL
 *
 * Programmer:  Houjun Tang
 *              May 5, 2017
 *
 *-------------------------------------------------------------------------
 */
static void *
H5O_swmr_deltat_copy(const void *_mesg, void *_dest)
{
    const H5O_swmr_deltat_t	*swmr_deltat = (const H5O_swmr_deltat_t *)_mesg;
    H5O_swmr_deltat_t	        *dest = (H5O_swmr_deltat_t *)_dest;
    void		        *ret_value = NULL;      /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Sanity check */
    HDassert(swmr_deltat);

    if(!dest && NULL == (dest = (H5O_swmr_deltat_t *)H5MM_malloc(sizeof(H5O_swmr_deltat_t))))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed for shared message table message")

    /* All this message requires is a shallow copy */
    *dest = *swmr_deltat;

    /* Set return value */
    ret_value = dest;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_swmr_deltat_copy() */


/*-------------------------------------------------------------------------
 * Function:	H5O_swmr_deltat_size
 *
 * Purpose:	Returns the size of the raw message in bytes not counting the
 *		message type or size fields, but only the data fields.
 *
 * Return:	Success:	Message data size in bytes w/o alignment.
 *		Failure:	0
 *
 * Programmer:  Houjun Tang
 *              May 5, 2017
 *
 *-------------------------------------------------------------------------
 */
static size_t
H5O_swmr_deltat_size(const H5F_t H5_ATTR_UNUSED *f, hbool_t H5_ATTR_UNUSED disable_shared, const void H5_ATTR_UNUSED *_mesg)
{
    size_t                   ret_value;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Sanity check */
    HDassert(f);

    ret_value = 1 +             /* Version number */
		4;              /* delta t is of 32 bit unsigned type*/

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_swmr_deltat_size() */


/*-------------------------------------------------------------------------
 * Function:	H5O_swmr_deltat_debug
 *
 * Purpose:	Prints debugging info for the message.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:  Houjun Tang
 *              May 5, 2017
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_swmr_deltat_debug(H5F_t H5_ATTR_UNUSED *f, hid_t H5_ATTR_UNUSED dxpl_id, const void *_mesg, FILE *stream,
    int indent, int fwidth)
{
    const H5O_swmr_deltat_t *mesg = (const H5O_swmr_deltat_t *)_mesg;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Sanity check */
    HDassert(f);
    HDassert(mesg);
    HDassert(stream);
    HDassert(indent >= 0);
    HDassert(fwidth >= 0);

    HDfprintf(stream, "%*s%-*s %u\n", indent, "", fwidth,
	      "SWMR delta t value:", mesg);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5O_swmr_deltat_debug() */

