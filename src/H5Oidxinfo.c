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
 * Created:             H5Oidxinfo.c
 *                      Jul 30 2014
 *                      Jerome Soumagne <jsoumagne@hdfgroup.org>
 *
 * Purpose:             Index info message.
 *
 *-------------------------------------------------------------------------
 */

#define H5O_PACKAGE         /* suppress error about including H5Opkg    */

#include "H5private.h"      /* Generic Functions    */
#include "H5Eprivate.h"     /* Error handling       */
#include "H5MMprivate.h"    /* Memory management    */
#include "H5Opkg.h"         /* Object headers       */
#include "H5Fprivate.h"


/* PRIVATE PROTOTYPES */
static void *H5O_idxinfo_decode(H5F_t *f, hid_t dxpl_id, H5O_t *open_oh,
        unsigned mesg_flags, unsigned *ioflags, const uint8_t *p);
static herr_t H5O_idxinfo_encode(H5F_t *f, hbool_t disable_shared, uint8_t *p,
        const void *_mesg);
static void *H5O_idxinfo_copy(const void *_mesg, void *_dest);
static size_t H5O_idxinfo_size(const H5F_t *f, hbool_t disable_shared,
        const void *_mesg);
static herr_t H5O_idxinfo_reset(void *_mesg);
static herr_t H5O_idxinfo_debug(H5F_t *f, hid_t dxpl_id, const void *_mesg,
        FILE * stream, int indent, int fwidth);

/* This message derives from H5O message class */
const H5O_msg_class_t H5O_MSG_IDXINFO[1] = {{
    H5O_IDXINFO_ID,             /* message id number                */
    "idxinfo",                  /* message name for debugging       */
    sizeof(H5O_idxinfo_t),      /* native message size              */
    0,                          /* messages are sharable?           */
    H5O_idxinfo_decode,         /* decode message                   */
    H5O_idxinfo_encode,         /* encode message                   */
    H5O_idxinfo_copy,           /* copy the native value            */
    H5O_idxinfo_size,           /* raw message size                 */
    H5O_idxinfo_reset,          /* free internal memory             */
    NULL,                       /* free method                      */
    NULL,                       /* file delete method               */
    NULL,                       /* link method                      */
    NULL,                       /* set share method                 */
    NULL,                       /* can share method                 */
    NULL,                       /* pre copy native value to file    */
    NULL,                       /* copy native value to file        */
    NULL,                       /* post copy native value to file   */
    NULL,                       /* get creation index               */
    NULL,                       /* set creation index               */
    H5O_idxinfo_debug         	/* debug the message                */
}};

/*-------------------------------------------------------------------------
 * Function:    H5O_idxinfo_decode
 *
 * Purpose:     Decode a idxinfo message and return a pointer to a new
 *              native message struct.
 *
 * Return:      Success:        Ptr to new message in native struct.
 *
 *              Failure:        NULL
 *
 *-------------------------------------------------------------------------
 */
static void *
H5O_idxinfo_decode(H5F_t UNUSED *f, hid_t UNUSED dxpl_id, H5O_t UNUSED *open_oh,
    unsigned UNUSED mesg_flags, unsigned UNUSED *ioflags, const uint8_t *p)
{
    H5O_idxinfo_t *mesg;
    void *ret_value; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* check args */
    HDassert(f);
    HDassert(p);

    /* decode */
    if (NULL == (mesg = (H5O_idxinfo_t *)H5MM_calloc(sizeof(H5O_idxinfo_t))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");
    if (0 == (mesg->plugin_id = *p++))
        HGOTO_ERROR(H5E_RESOURCE, H5E_BADVALUE, NULL, "NULL plugin ID");
    if (0 == (mesg->metadata_size = (uint64_t) *p++))
        HGOTO_ERROR(H5E_RESOURCE, H5E_BADVALUE, NULL, "NULL metadata size");
    if (NULL == (mesg->metadata = H5MM_malloc(mesg->metadata_size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");
    HDmemcpy(mesg->metadata, (const char *) p, mesg->metadata_size);

    /* Set return value */
    ret_value = mesg;

done:
    if (NULL == ret_value) {
        if (mesg) {
            mesg->metadata = H5MM_xfree(mesg->metadata);
            mesg = (H5O_idxinfo_t *)H5MM_xfree(mesg);
        }
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_idxinfo_decode() */

/*-------------------------------------------------------------------------
 * Function:    H5O_idxinfo_encode
 *
 * Purpose:     Encodes a idxinfo message.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_idxinfo_encode(H5F_t *f, hbool_t UNUSED disable_shared, uint8_t *p,
        const void *_mesg)
{
    const H5O_idxinfo_t *mesg = (const H5O_idxinfo_t *) _mesg;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* check args */
    HDassert(f);
    HDassert(p);
    HDassert(mesg && mesg->s);

    /* encode */
    *p++ = mesg->plugin_id;
    *p = mesg->metadata_size;
    p += sizeof(uint64_t);
    HDmemcpy((char *) p, mesg->metadata, mesg->metadata_size);

    FUNC_LEAVE_NOAPI(SUCCEED)
}

/*-------------------------------------------------------------------------
 * Function:    H5O_idxinfo_copy
 *
 * Purpose:     Copies a message from _MESG to _DEST, allocating _DEST if
 *              necessary.
 *
 * Return:      Success:        Ptr to _DEST
 *
 *              Failure:        NULL
 *
 *-------------------------------------------------------------------------
 */
static void *
H5O_idxinfo_copy(const void *_mesg, void *_dest)
{
    const H5O_idxinfo_t *mesg = (const H5O_idxinfo_t *) _mesg;
    H5O_idxinfo_t *dest = (H5O_idxinfo_t *) _dest;
    void *ret_value; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* check args */
    HDassert(mesg);

    if (!dest && NULL == (dest = (H5O_idxinfo_t *) H5MM_calloc(sizeof(H5O_idxinfo_t))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");

    /* copy */
    *dest = *mesg;
    if (NULL == (dest->metadata = H5MM_malloc(dest->metadata_size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");
    HDmemcpy(dest->metadata, mesg->metadata, dest->metadata_size);

    /* Set return value */
    ret_value = dest;

done:
    if(NULL == ret_value)
        if(dest && NULL == _dest)
            dest = (H5O_idxinfo_t *)H5MM_xfree(dest);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_idxinfo_copy() */

/*-------------------------------------------------------------------------
 * Function:    H5O_idxinfo_size
 *
 * Purpose:     Returns the size of the raw message in bytes not
 *              counting the message typ or size fields, but only the data
 *              fields. This function doesn't take into account alignment.
 *
 * Return:      Success:        Message data size in bytes w/o alignment.
 *
 *              Failure:        Negative
 *
 *-------------------------------------------------------------------------
 */
static size_t
H5O_idxinfo_size(const H5F_t UNUSED *f, hbool_t UNUSED disable_shared, const void *_mesg)
{
    const H5O_idxinfo_t *mesg = (const H5O_idxinfo_t *) _mesg;
    size_t ret_value;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* check args */
    HDassert(f);
    HDassert(mesg);

    ret_value = sizeof(mesg->plugin_id) + sizeof(mesg->metadata_size) +
            mesg->metadata_size;

    FUNC_LEAVE_NOAPI(ret_value)
}

/*-------------------------------------------------------------------------
 * Function:    H5O_idxinfo_reset
 *
 * Purpose:     Frees internal pointers and resets the message to an
 *              initial state.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Robb Matzke
 *              matzke@llnl.gov
 *              Aug 12 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_idxinfo_reset(void *_mesg)
{
    H5O_idxinfo_t             *mesg = (H5O_idxinfo_t *) _mesg;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* check args */
    HDassert(mesg);

    /* reset */
    mesg->plugin_id = 0;
    mesg->metadata_size = 0;
    mesg->metadata = H5MM_xfree(mesg->metadata);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5O_idxinfo_reset() */

/*-------------------------------------------------------------------------
 * Function:    H5O_idxinfo_debug
 *
 * Purpose:     Prints debugging info for the message.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_idxinfo_debug(H5F_t UNUSED *f, hid_t UNUSED dxpl_id, const void *_mesg, FILE *stream,
	       int indent, int fwidth)
{
    const H5O_idxinfo_t	*mesg = (const H5O_idxinfo_t *)_mesg;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* check args */
    HDassert(f);
    HDassert(mesg);
    HDassert(stream);
    HDassert(indent >= 0);
    HDassert(fwidth >= 0);

    fprintf(stream, "%*s%-*s %d\n", indent, "", fwidth, "Plugin ID:",
            mesg->plugin_id);
    fprintf(stream, "%*s%-*s %d\n", indent, "", fwidth, "Metadata Size:",
            mesg->metadata_size);

    FUNC_LEAVE_NOAPI(SUCCEED)
}
