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


/* Programmer:  James Laird <jlaird@hdfgroup.com>
 *              Monday, January 29, 2007
 *
 * Purpose:	A message holding "implicitly shared object header message"
 *              information in the superblock extension.
 */

#define H5O_PACKAGE		/*suppress error about including H5Opkg	  */

#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Opkg.h"             /* Object headers			*/
#include "H5MMprivate.h"	/* Memory management			*/

static void  *H5O_shmesg_decode(H5F_t *f, hid_t dxpl_id, unsigned mesg_flags, const uint8_t *p);
static herr_t H5O_shmesg_encode(H5F_t *f, hbool_t disable_shared, uint8_t *p, const void *_mesg);
static void *H5O_shmesg_copy(const void *_mesg, void *_dest);
static size_t H5O_shmesg_size(const H5F_t *f, hbool_t disable_shared, const void *_mesg);
static herr_t H5O_shmesg_debug(H5F_t *f, hid_t dxpl_id, const void *_mesg, FILE *stream,
			     int indent, int fwidth);

/* This message derives from H5O message class, for old fill value before version 1.5 */
const H5O_msg_class_t H5O_MSG_SHMESG[1] = {{
    H5O_SHMESG_ID,              /*message id number                     */
    "shared message table",     /*message name for debugging            */
    sizeof(H5O_shmesg_table_t),	/*native message size                   */
    FALSE,			/* messages are sharable?       */
    H5O_shmesg_decode,		/*decode message                        */
    H5O_shmesg_encode,		/*encode message                        */
    H5O_shmesg_copy,            /*copy the native value                 */
    H5O_shmesg_size,		/*raw message size			*/
    NULL,                       /*free internal memory			*/
    NULL,                       /* free method				*/
    NULL,			/* file delete method			*/
    NULL,			/* link method				*/
    NULL,	                /* set share method			*/
    NULL,		    	/*can share method		        */
    NULL,			/* pre copy native value to file	*/
    NULL,			/* copy native value to file		*/
    NULL,			/* post copy native value to file	*/
    NULL,			/* get creation index		        */
    NULL,			/* set creation index		        */
    H5O_shmesg_debug              /*debug the message			*/
}};


/*-------------------------------------------------------------------------
 * Function:	H5O_shmesg_decode
 *
 * Purpose:	Decode a shared message table message and return a pointer
 *              to a newly allocated H5O_shmesg_table_t struct.
 *
 * Return:	Success:	Ptr to new message in native struct.
 *		Failure:	NULL
 *
 * Programmer:  James Laird
 *              Jan 29, 2007
 *
 *-------------------------------------------------------------------------
 */
static void *
H5O_shmesg_decode(H5F_t *f, hid_t UNUSED dxpl_id, unsigned UNUSED mesg_flags,
    const uint8_t *p)
{
    H5O_shmesg_table_t	*mesg = NULL;
    void                *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT(H5O_shmesg_decode)

    HDassert(f);
    HDassert(p);

    if(NULL == (mesg = H5MM_calloc(sizeof(H5O_shmesg_table_t))))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed for shared message table message")

    /* Read version, table address, and number of indexes */
    mesg->version = *p++;
    H5F_addr_decode(f, &p, &(mesg->addr));
    mesg->nindexes = *p++;

    /* Set return value */
    ret_value = (void*)mesg;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_shmesg_decode() */


/*-------------------------------------------------------------------------
 * Function:	H5O_shmesg_encode
 *
 * Purpose:	Encode a shared message table message and return a pointer
 *              to a newly allocated H5O_shmesg_table_t struct.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:  James Laird
 *              Jan 29, 2007
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_shmesg_encode(H5F_t *f, hbool_t UNUSED disable_shared, uint8_t *p, const void *_mesg)
{
    const H5O_shmesg_table_t *mesg = (const H5O_shmesg_table_t *)_mesg;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_shmesg_encode)

    HDassert(f);
    HDassert(p);
    HDassert(mesg);

    /* Read version, table address, and number of indexes */
    *p++ = mesg->version;
    H5F_addr_encode(f, &p, mesg->addr);
    *p++ = mesg->nindexes;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5O_shmesg_encode() */


/*-------------------------------------------------------------------------
 * Function:	H5O_shmesg_copy
 *
 * Purpose:	Copies a message from _MESG to _DEST, allocating _DEST if
 *		necessary.
 *
 * Return:	Success:	Ptr to _DEST
 *		Failure:	NULL
 *
 * Programmer:  James Laird
 *              Jan 29, 2007
 *
 *-------------------------------------------------------------------------
 */
static void *
H5O_shmesg_copy(const void *_mesg, void *_dest)
{
    const H5O_shmesg_table_t	*mesg = (const H5O_shmesg_table_t *)_mesg;
    H5O_shmesg_table_t		*dest = (H5O_shmesg_table_t *)_dest;
    void		*ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT(H5O_shmesg_copy)

    HDassert(mesg);

    if(!dest && NULL == (dest = H5MM_malloc(sizeof(H5O_shmesg_table_t))))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed for shared message table message")

    /* All this message requires is a shallow copy */
    *dest = *mesg;

    /* Set return value */
    ret_value = dest;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_shmesg_copy() */


/*-------------------------------------------------------------------------
 * Function:	H5O_shmesg_size
 *
 * Purpose:	Returns the size of the raw message in bytes not counting the
 *		message type or size fields, but only the data fields.  This
 *		function doesn't take into account alignment.  The new fill
 *		value message is fill value plus space allocation time and
 *              fill value writing time and whether fill value is defined.
 *
 * Return:	Success:	Message data size in bytes w/o alignment.
 *		Failure:	0
 *
 * Programmer:  James Laird
 *              Jan 29, 2007
 *
 *-------------------------------------------------------------------------
 */
static size_t
H5O_shmesg_size(const H5F_t *f, hbool_t UNUSED disable_shared, const void *_mesg)
{
    const H5O_shmesg_table_t *mesg = (const H5O_shmesg_table_t *)_mesg;
    size_t                   ret_value;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_shmesg_size)

    HDassert(f);
    HDassert(mesg);

    ret_value = 1 +                     /* Version number        */
		H5F_SIZEOF_ADDR(f) +    /* Table address */
		1;                      /* Number of indexes */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_shmesg_size() */


/*-------------------------------------------------------------------------
 * Function:	H5O_shmesg_debug
 *
 * Purpose:	Prints debugging info for the message.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:  James Laird
 *              Jan 29, 2007
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_shmesg_debug(H5F_t UNUSED *f, hid_t UNUSED dxpl_id, const void *_mesg, FILE *stream,
    int indent, int fwidth)
{
    const H5O_shmesg_table_t *mesg = (const H5O_shmesg_table_t *)_mesg;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_shmesg_debug)

    HDassert(f);
    HDassert(mesg);
    HDassert(stream);
    HDassert(indent >= 0);
    HDassert(fwidth >= 0);

    HDfprintf(stream, "%*s%-*s %u\n", indent, "", fwidth,
	      "Version:", mesg->version);
    HDfprintf(stream, "%*s%-*s %a (rel)\n", indent, "", fwidth,
	      "Shared message table address:", mesg->addr);
    HDfprintf(stream, "%*s%-*s %u\n", indent, "", fwidth,
	      "Number of indexes:", mesg->nindexes);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5O_shmesg_debug() */

