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

/* Programmer:  Mike McGreevy
 *              December 9, 2010
 *              
 * Purpose:     The EOA message.
 */

#define H5O_PACKAGE /* Suppress error about including H5Opkg */

#include "H5private.h"   /* Generic Functions */
#include "H5Eprivate.h"  /* Error Handling    */
#include "H5FLprivate.h" /* Free Lists        */
#include "H5Opkg.h"      /* Object Headers    */

static void *H5O_eoa_new_decode(H5F_t *f, hid_t dxpl_id, H5O_t *open_oh,
    unsigned mesg_flags, unsigned *ioflags, const uint8_t *p);

static herr_t H5O_eoa_new_encode(H5F_t *f, hbool_t disable_shared, uint8_t *p, const void *_mesg);

static size_t H5O_eoa_new_size(const H5F_t *f, hbool_t disable_shared, const void *_mesg);

static void *H5O_eoa_decode(H5F_t *f, hid_t dxpl_id, H5O_t *open_oh,
    unsigned mesg_flags, unsigned *ioflags, const uint8_t *p);

static herr_t H5O_eoa_encode(H5F_t *f, hbool_t disable_shared, uint8_t *p, const void *_mesg);

static void *H5O_eoa_copy(const void *_mesg, void *_dest);

static size_t H5O_eoa_size(const H5F_t *f, hbool_t disable_shared, const void *_mesg);

static herr_t H5O_eoa_reset(void *_mesg);

static herr_t H5O_eoa_free(void *_mesg);

static herr_t H5O_eoa_debug(H5F_t *f, hid_t dxpl_id, const void *_mesg, FILE *stream,
                            int indent, int fwidth);

/* This message derives from H5O message class */
const H5O_msg_class_t H5O_MSG_EOA[1] = {{
    H5O_EOA_ID,         /* Message ID number                */
    "'EOA' value",      /* Message name for debugging       */
    sizeof(haddr_t),    /* Native message size              */
    0,                  /* Messages are sharable?           */
    H5O_eoa_decode,     /* Decode message                   */
    H5O_eoa_encode,     /* Encode message                   */
    H5O_eoa_copy,       /* Copy the native value            */
    H5O_eoa_size,       /* Raw message size                 */
    H5O_eoa_reset,      /* Free internal memory             */
    H5O_eoa_free,       /* Free method                      */
    NULL,               /* File delete method               */
    NULL,               /* Link method                      */
    NULL,               /* Set share method                 */
    NULL,               /* Can share method                 */
    NULL,               /* Pre copy native value to file    */
    NULL,               /* Copy native value to file        */
    NULL,               /* Post copy native value to file   */
    NULL,               /* get creation index               */
    NULL,               /* set creation index               */
    H5O_eoa_debug       /* Debug the message                */
}};

/* Current version of 'EOA' message */
#define H5O_EOA_VERSION 0

/* Declare a free list to manage the haddr_t struct */
H5FL_DEFINE(haddr_t);


/*-------------------------------------------------------------------------
 * Function:    H5O_eoa_decode
 *
 * Purpose:     Decode an 'EOA' message a return a pointer to a 
 *              new haddr_t value.
 *
 * Return:      Success:    PTR to a new message in native struct.
 *              Failure:    NULL
 *
 * Programmer:  Mike McGreevy
 *              December 9, 2010
 *
 *-------------------------------------------------------------------------
 */
static void *
H5O_eoa_decode(H5F_t UNUSED *f, hid_t UNUSED dxpl_id, H5O_t UNUSED *open_oh,
    unsigned UNUSED mesg_flags, unsigned UNUSED *ioflags, const uint8_t *p)
{
    haddr_t *mesg = NULL;   /* Native message */
    haddr_t tmp_eoa;        /* Temporary EOA */
    void    *ret_value;     /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_eoa_decode);

    /* Check Arguments */
    HDassert(f);
    HDassert(p);
    
    /* Version of the message */
    if(*p++ != H5O_EOA_VERSION)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, NULL, "bad version number for message")
    
    /* Get the 'EOA' message from the file */
    H5F_addr_decode(f, (const uint8_t **)&p, &tmp_eoa);

    /* The return value */
    if (NULL == (mesg = H5FL_MALLOC(haddr_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");
    *mesg = (haddr_t)tmp_eoa;

    /* Set return value */
    ret_value = (void *)mesg;

done:
    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5O_eoa_decode() */


/*-------------------------------------------------------------------------
 * Function:    H5O_eoa_encode
 *
 * Purpose:     Encode an 'EOA' message. 
 *
 * Return:      Success:    Non-Negative
 *              Failure:    Negative
 *
 * Programmer:  Mike McGreevy
 *              December 9, 2010
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_eoa_encode(H5F_t UNUSED *f, hbool_t UNUSED disable_shared, uint8_t *p, const void *_mesg)
{
    const haddr_t   *mesg = (const haddr_t *)   _mesg;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_eoa_encode);

    /* Check Arguments */
    HDassert(f);
    HDassert(p);
    HDassert(mesg);

    /* Version */
    *p++ = H5O_EOA_VERSION;

    /* Encode 'EOA' Message */
    H5F_addr_encode(f, &p, *mesg);

    FUNC_LEAVE_NOAPI(SUCCEED);
} /* end H5O_eoa_encode() */


/*-------------------------------------------------------------------------
 * Function:    H5O_eoa_copy
 *
 * Purpose:     Copies a message from _MESG to _DEST, allocating _DEST if
 *              necessary.
 *
 * Return:      Success:    Ptr to _DEST
 *              Failure:    NULL
 *
 * Programmer:  Mike McGreevy
 *              December 9, 2010
 *
 *-------------------------------------------------------------------------
 */
static void *
H5O_eoa_copy(const void *_mesg, void *_dest)
{
    const haddr_t   *mesg = (const haddr_t *) _mesg;
    haddr_t         *dest = (haddr_t *)_dest;
    void            *ret_value;

    FUNC_ENTER_NOAPI_NOINIT(H5O_eoa_copy);

    /* Check Arguments */
    HDassert(mesg);
    if (!dest && NULL == (dest = H5FL_MALLOC(haddr_t)))
        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");

    /* Copy */
    *dest = *mesg;

    /* Set return value */
    ret_value = dest;

done:
    FUNC_LEAVE_NOAPI(ret_value);
} /* H5O_eoa_copy */


/*-------------------------------------------------------------------------
 * Function:    H5O_eoa_size
 *
 * Purpose:     Returns the size of the raw message in bytes. This function
 *              doesn't take into account alignment.
 *
 * Return:      Success:    Mesage data size in bytes w/o alignment.
 *              Failure:    0
 *
 * Programmer:  Mike McGreevy
 *              December 9, 2010
 *
 *-------------------------------------------------------------------------
 */
static size_t
H5O_eoa_size(const H5F_t UNUSED * f, hbool_t UNUSED disable_shared, const void UNUSED * mesg)
{
    size_t  ret_value;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_eoa_size);

    /* Check Arguments */
    HDassert(f);
    HDassert(mesg);
    
    /* Determine Size */
    ret_value = 1 +     /* 8-bit Version Number (int)   */
                H5F_SIZEOF_ADDR(f); /* EOA Address (haddr_t) */

    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5O_eoa_size() */


/*-------------------------------------------------------------------------
 * Function:    H5O_eoa_reset
 *
 * Purpose:     Frees resources within an 'EOA' message, but doesn't free
 *              the message itself.
 *
 * Return:      Success:    Non-Negative
 *              Failure:    Negative
 *
 * Programmer:  Mike McGreevy
 *              December 9, 2010
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_eoa_reset(void UNUSED *_mesg)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_eoa_reset);

    FUNC_LEAVE_NOAPI(SUCCEED);
} /* H5O_eoa_reset */


/*-------------------------------------------------------------------------
 * Function:    H5O_eoa_free
 *
 * Purpose:     Frees the 'EOA' message
 *
 * Return:      Success:    Non-Negative
 *              Failure:    Negative
 *
 * Programmer:  Mike McGreevy
 *              December 9, 2010
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_eoa_free(void *mesg)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_eoa_free)

    HDassert(mesg);

    mesg = H5FL_FREE(haddr_t, mesg);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5O_eoa_free() */


/*-------------------------------------------------------------------------
 * Function:    H5O_eoa_debug
 *
 * Purpose:     Prints debugging info for the 'EOA' message.
 *
 * Return:      Success:    Non-Negative
 *              Failure:    Negative
 *
 * Programmer:  Mike McGreevy
 *              December 9, 2010
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_eoa_debug(H5F_t UNUSED *f, hid_t UNUSED dxpl_id, const void *_mesg, FILE *stream,
              int indent, int fwidth)
{
    const haddr_t   *mesg = (const haddr_t *)_mesg;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_eoa_debug);
HDassert(0);

    /* Check Arguments */
    HDassert(f);
    HDassert(mesg);
    HDassert(stream);
    HDassert(indent >= 0);
    HDassert(fwidth >= 0);

    HDfprintf(stream, "%*s%-*s %ld\n", indent, "", fwidth, "EOA value:", *mesg);

    FUNC_LEAVE_NOAPI(SUCCEED);
} /* H5O_eoa_debug */
