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


/* Programmer:  Neil Fortner <nfortne2@hdfgroup.org>
 *              Tuesday, December  13, 2011
 *
 * Purpose:     A message holding persistent file alignment
 *              information in the superblock extension.
 */

#define H5O_PACKAGE             /*suppress error about including H5Opkg */

#include "H5private.h"          /* Generic Functions                    */
#include "H5Eprivate.h"         /* Error handling                       */
#include "H5Opkg.h"             /* Object headers                       */
#include "H5MMprivate.h"        /* Memory management                    */

static void  *H5O_align_decode(H5F_t *f, hid_t dxpl_id, H5O_t *open_oh,
    unsigned mesg_flags, unsigned *ioflags, const uint8_t *p);
static herr_t H5O_align_encode(H5F_t *f, hbool_t disable_shared, uint8_t *p,
    const void *_mesg);
static void  *H5O_align_copy(const void *_mesg, void *_dest);
static size_t H5O_align_size(const H5F_t *f, hbool_t disable_shared,
    const void *_mesg);
static herr_t H5O_align_debug(H5F_t *f, hid_t dxpl_id, const void *_mesg,
    FILE *stream, int indent, int fwidth);

/* This message derives from H5O message class */
const H5O_msg_class_t H5O_MSG_ALIGN[1] = {{
    H5O_ALIGN_ID,               /*message id number                     */
    "alignment values",         /*message name for debugging            */
    sizeof(H5O_align_t),        /*native message size                   */
    0,                          /* messages are sharable?               */
    H5O_align_decode,           /*decode message                        */
    H5O_align_encode,           /*encode message                        */
    H5O_align_copy,             /*copy the native value                 */
    H5O_align_size,             /*raw message size                      */
    NULL,                       /*free internal memory                  */
    NULL,                       /* free method                          */
    NULL,                       /* file delete method                   */
    NULL,                       /* link method                          */
    NULL,                       /*set share method                      */
    NULL,                       /*can share method                      */
    NULL,                       /* pre copy native value to file        */
    NULL,                       /* copy native value to file            */
    NULL,                       /* post copy native value to file       */
    NULL,                       /* get creation index                   */
    NULL,                       /* set creation index                   */
    H5O_align_debug             /*debug the message                     */
}};

/* Current version of persistent alignment information */
#define H5O_ALIGN_VERSION 	0


/*-------------------------------------------------------------------------
 * Function:    H5O_align_decode
 *
 * Purpose:     Decode a file alignment message and return a pointer to a
 *              newly allocated H5O_align_t struct.
 *
 * Return:      Success: Ptr to new message in native struct.
 *              Failure: NULL
 *
 * Programmer:  Neil Fortner
 *              Dec  13, 2011
 *
 *-------------------------------------------------------------------------
 */
static void *
H5O_align_decode(H5F_t *f, hid_t UNUSED dxpl_id, H5O_t UNUSED *open_oh,
    unsigned mesg_flags, unsigned UNUSED *ioflags, const uint8_t *p)
{
    H5O_align_t         *mesg;         /* Native message */
    void                *ret_value;     /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Sanity check */
    HDassert(f);
    HDassert(p);

    /* Version of message */
    if(*p++ != H5O_ALIGN_VERSION)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, NULL, "bad version number for message")

    /* Allocate space for message */
    if(NULL == (mesg = (H5O_align_t *)H5MM_calloc(sizeof(H5O_align_t))))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed for file alignment message")

    /* Retrieve alignment values */
    H5F_DECODE_LENGTH(f, p, mesg->threshold);
    H5F_DECODE_LENGTH(f, p, mesg->alignment);

    /* Set other fields */
    mesg->persistent = TRUE;
    mesg->strict = !(mesg_flags & H5O_MSG_FLAG_WAS_UNKNOWN);

    /* Set return value */
    ret_value = (void *)mesg;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_align_decode() */


/*-------------------------------------------------------------------------
 * Function:    H5O_align_encode
 *
 * Purpose:     Encode a file alignment message.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Neil Fortner
 *              Dec  14, 2011
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_align_encode(H5F_t *f, hbool_t UNUSED disable_shared, uint8_t *p,
    const void *_mesg)
{
    const H5O_align_t *mesg = (const H5O_align_t *)_mesg;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Sanity check */
    HDassert(f);
    HDassert(p);
    HDassert(mesg);
    HDassert(mesg->persistent);

    /* Store version and non-default v1 B-tree 'K' values */
    *p++ = H5O_ALIGN_VERSION;
    H5F_ENCODE_LENGTH(f, p, mesg->threshold);
    H5F_ENCODE_LENGTH(f, p, mesg->alignment);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5O_align_encode() */


/*-------------------------------------------------------------------------
 * Function:    H5O_align_copy
 *
 * Purpose:     Copies a message from _MESG to _DEST, allocating _DEST if
 *              necessary.
 *
 * Return:      Success: Ptr to _DEST
 *              Failure: NULL
 *
 * Programmer:  Neil Fortner
 *              Dec  14, 2011
 *
 *-------------------------------------------------------------------------
 */
static void *
H5O_align_copy(const void *_mesg, void *_dest)
{
    const H5O_align_t	*mesg = (const H5O_align_t *)_mesg;
    H5O_align_t	*dest = (H5O_align_t *)_dest;
    void		*ret_value;

    FUNC_ENTER_NOAPI_NOINIT

    /* Sanity check */
    HDassert(mesg);

    if(!dest && NULL == (dest = (H5O_align_t *)H5MM_malloc(sizeof(H5O_align_t))))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed for file alignment message")

    /* All this message requires is a shallow copy */
    *dest = *mesg;

    /* Set return value */
    ret_value = dest;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_align_copy() */


/*-------------------------------------------------------------------------
 * Function:    H5O_align_size
 *
 * Purpose:     Returns the size of the raw message in bytes not counting the
 *              message type or size fields, but only the data fields.
 *
 * Return:      Success: Message data size in bytes w/o alignment.
 *              Failure: 0
 *
 * Programmer:  Neil Fortner
 *              Dec  14, 2011
 *
 *-------------------------------------------------------------------------
 */
static size_t
H5O_align_size(const H5F_t *f, hbool_t UNUSED disable_shared,
    const void UNUSED *_mesg)
{
    size_t                   ret_value;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Sanity check */
    HDassert(f);

    ret_value = (size_t)1 +             /* Version number */
                H5F_SIZEOF_SIZE(f) +    /* Alignment value */
                H5F_SIZEOF_SIZE(f);     /* Threshold value */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_align_size() */


/*-------------------------------------------------------------------------
 * Function:    H5O_align_debug
 *
 * Purpose:     Prints debugging info for the message.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Neil Fortner
 *              Dec  14, 2011
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_align_debug(H5F_t UNUSED *f, hid_t UNUSED dxpl_id, const void *_mesg, FILE *stream,
    int indent, int fwidth)
{
    const H5O_align_t *mesg = (const H5O_align_t *)_mesg;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Sanity check */
    HDassert(f);
    HDassert(mesg);
    HDassert(stream);
    HDassert(indent >= 0);
    HDassert(fwidth >= 0);

    HDfprintf(stream, "%*s%-*s %u\n", indent, "", fwidth,
            "Persistent file alignment value:", mesg->alignment);
    HDfprintf(stream, "%*s%-*s %u\n", indent, "", fwidth,
            "Persistent file alignment threshold:", mesg->threshold);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5O_align_debug() */

