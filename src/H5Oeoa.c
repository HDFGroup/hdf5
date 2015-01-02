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

/* PRIVATE PROTOTYPES */
static void *H5O__eoa_decode(H5F_t *f, hid_t dxpl_id, H5O_t *open_oh, unsigned mesg_flags, unsigned *ioflags, const uint8_t *p);
static herr_t H5O__eoa_encode(H5F_t *f, hbool_t disable_shared, uint8_t *p, const void *_mesg);
static void *H5O__eoa_copy(const void *_mesg, void *_dest);
static size_t H5O__eoa_size(const H5F_t *f, hbool_t disable_shared, const void *_mesg);
static herr_t H5O__eoa_reset(void *_mesg);
static herr_t H5O__eoa_free(void *_mesg);
static herr_t H5O__eoa_debug(H5F_t *f, hid_t dxpl_id, const void *_mesg,
    FILE *stream, int indent, int fwidth);

/* This message derives from H5O message class */
const H5O_msg_class_t H5O_MSG_EOA[1] = {{
    H5O_EOA_ID,         /* Message ID number                */
    "'EOA' message",    /* Message name for debugging       */
    sizeof(H5O_eoa_t),  /* Native message size              */
    0,                  /* Messages are sharable?           */
    H5O__eoa_decode,    /* Decode message                   */
    H5O__eoa_encode,    /* Encode message                   */
    H5O__eoa_copy,      /* Copy the native value            */
    H5O__eoa_size,      /* Raw message size                 */
    H5O__eoa_reset,     /* Free internal memory             */
    H5O__eoa_free,      /* Free method                      */
    NULL,               /* File delete method               */
    NULL,               /* Link method                      */
    NULL,               /* Set share method                 */
    NULL,               /* Can share method                 */
    NULL,               /* Pre copy native value to file    */
    NULL,               /* Copy native value to file        */
    NULL,               /* Post copy native value to file   */
    NULL,               /* get creation index               */
    NULL,               /* set creation index               */
    H5O__eoa_debug      /* Debug the message                */
}};

/* Current version of 'EOA' message */
#define H5O_EOA_VERSION 0

/* Declare a free list to manage the haddr_t struct */
H5FL_DEFINE(H5O_eoa_t);


/*-------------------------------------------------------------------------
 * Function:    H5O__eoa_decode
 *
 * Purpose:     Decode an 'EOA' message a return a pointer to a 
 *              new H5O_eoa_t structure.
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
H5O__eoa_decode(H5F_t UNUSED *f, hid_t UNUSED dxpl_id, H5O_t UNUSED *open_oh,
    unsigned UNUSED mesg_flags, unsigned UNUSED *ioflags, const uint8_t *p)
{
    H5O_eoa_t *mesg;    /* Native message */
    H5FD_mem_t mt;      /* Memory type iterator */
    void *ret_value;    /* Return value */

    FUNC_ENTER_STATIC

    /* Check Arguments */
    HDassert(f);
    HDassert(p);

    /* Allocate new message */
    if(NULL == (mesg = (H5O_eoa_t *)H5FL_MALLOC(H5O_eoa_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* Version of the message */
    if(*p++ != H5O_EOA_VERSION)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, NULL, "bad version number for message")

    /* Get the 'avoid truncate' setting */
    mesg->avoid_truncate = (H5F_avoid_truncate_t)*p++; /* avoid truncate setting */

    /* Get the 'EOAs' message from the file */
    for(mt = H5FD_MEM_SUPER; mt < H5FD_MEM_NTYPES; mt = (H5FD_mem_t)(mt + 1))
        H5F_addr_decode(f, (const uint8_t **)&p, &(mesg->memb_eoa[mt]));

    /* Set return value */
    ret_value = (void *)mesg;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O__eoa_decode() */


/*-------------------------------------------------------------------------
 * Function:    H5O__eoa_encode
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
H5O__eoa_encode(H5F_t UNUSED *f, hbool_t UNUSED disable_shared, uint8_t *p, const void *_mesg)
{
    const H5O_eoa_t *mesg = (const H5O_eoa_t *) _mesg;    
    H5FD_mem_t mt;      /* Memory type iterator */

    FUNC_ENTER_STATIC_NOERR

    /* Check Arguments */
    HDassert(f);
    HDassert(p);
    HDassert(mesg);

    /* Version */
    *p++ = H5O_EOA_VERSION;

    /* Encode 'Avoid Truncate' setting */
    *p++ = mesg->avoid_truncate;

    /* Encode 'EOAs' Message */
    for(mt = H5FD_MEM_SUPER; mt < H5FD_MEM_NTYPES; mt = (H5FD_mem_t)(mt + 1))
        H5F_addr_encode(f, &p, mesg->memb_eoa[mt]);    

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5O__eoa_encode() */


/*-------------------------------------------------------------------------
 * Function:    H5O__eoa_copy
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
H5O__eoa_copy(const void *_mesg, void *_dest)
{
    const H5O_eoa_t   *mesg = (const H5O_eoa_t *) _mesg;
    H5O_eoa_t         *dest = (H5O_eoa_t *)_dest;
    H5FD_mem_t         mt;      /* Memory type iterator */
    void              *ret_value;

    FUNC_ENTER_STATIC

    /* Check Arguments */
    HDassert(mesg);
    if(!dest && NULL == (dest = H5FL_MALLOC(H5O_eoa_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* Copy */
    dest->avoid_truncate = mesg->avoid_truncate;
    for(mt = H5FD_MEM_SUPER; mt < H5FD_MEM_NTYPES; mt = (H5FD_mem_t)(mt + 1))
        dest->memb_eoa[mt] = mesg->memb_eoa[mt];

    /* Set return value */
    ret_value = dest;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5O__eoa_copy */


/*-------------------------------------------------------------------------
 * Function:    H5O__eoa_size
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
H5O__eoa_size(const H5F_t *f, hbool_t UNUSED disable_shared, const void UNUSED * mesg)
{
    size_t  ret_value;

    FUNC_ENTER_STATIC_NOERR

    /* Check Arguments */
    HDassert(f);
    HDassert(mesg);
    
    /* Determine Size */
    ret_value = (size_t)(1 +     /* Version */
                H5FD_MEM_NTYPES * H5F_SIZEOF_ADDR(f) + /* EOA Address (haddr_t) */
                1); /* truncation avoidance mode */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O__eoa_size() */


/*-------------------------------------------------------------------------
 * Function:    H5O__eoa_reset
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
H5O__eoa_reset(void UNUSED *_mesg)
{
    FUNC_ENTER_STATIC_NOERR

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5O__eoa_reset */


/*-------------------------------------------------------------------------
 * Function:    H5O__eoa_free
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
H5O__eoa_free(void *mesg)
{
    FUNC_ENTER_STATIC_NOERR

    HDassert(mesg);

    mesg = H5FL_FREE(H5O_eoa_t, mesg);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5O__eoa_free() */


/*-------------------------------------------------------------------------
 * Function:    H5O__eoa_debug
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
H5O__eoa_debug(H5F_t UNUSED *f, hid_t UNUSED dxpl_id, const void *_mesg,
    FILE *stream, int indent, int fwidth)
{
    const H5O_eoa_t *mesg = (const H5O_eoa_t *) _mesg;
    H5FD_mem_t mt;      /* Memory type iterator */

    FUNC_ENTER_STATIC_NOERR

    /* Check Arguments */
    HDassert(f);
    HDassert(mesg);
    HDassert(stream);
    HDassert(indent >= 0);
    HDassert(fwidth >= 0);

    HDfprintf(stream, "%*s%-*s %s\n", indent, "", fwidth,
            "Avoid truncate:", ((mesg->avoid_truncate == H5F_AVOID_TRUNCATE_OFF) ?  "H5F_AVOID_TRUNCATE_OFF" :
                ((mesg->avoid_truncate == H5F_AVOID_TRUNCATE_EXTEND) ?  "H5F_AVOID_TRUNCATE_EXTEND" : "H5F_AVOID_TRUNCATE_ALL")));

    for(mt = H5FD_MEM_SUPER; mt < H5FD_MEM_NTYPES; mt = (H5FD_mem_t)(mt + 1)) {
        char temp[32];      /* Temporary string, for sprintf */

        HDsnprintf(temp, sizeof(temp), "EOA value[%u]:", (unsigned)mt);
        HDfprintf(stream, "%*s%-*s %Hu\n", indent, "", fwidth, temp, mesg->memb_eoa[mt]);
    } /* end for */

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5O__eoa_debug */

