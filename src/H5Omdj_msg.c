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
 * Created:     H5Omdj_msg.c
 *              Dec 6 2007
 *              John Mainzer
 *
 * Purpose:     A message detailing whether metadata jouraling is enabled,
 * 		and if so, the base address in file and length of the block
 * 		that contains the journaling configuration data.
 *
 * 		The mdj_msg only appears in the superblock extension.
 *
 * Modifications:
 *
 *              None.
 *
 *-------------------------------------------------------------------------
 */

#define H5O_PACKAGE             /*suppress error about including H5Opkg   */

#include "H5private.h"          /* Generic Functions                    */
#include "H5Eprivate.h"         /* Error handling                       */
#include "H5Opkg.h"             /* Object headers                       */
#include "H5MMprivate.h"        /* Memory management                    */

#define MDJ_MSG_LEN(f)	( 1 +                  /* Version number */               \
                	  2 +                  /* flags */                        \
			  H5F_SIZEOF_ADDR(f) + /* addr of journal config block */ \
                	  H5F_SIZEOF_SIZE(f) ) /* journal config block len */ 

static void * H5O_mdj_msg_decode(H5F_t UNUSED *f,
                                 hid_t UNUSED dxpl_id,
                                 unsigned UNUSED mesg_flags,
                                 const uint8_t *p);

static herr_t H5O_mdj_msg_encode(H5F_t *f,
                                 hbool_t UNUSED disable_shared,
                                 uint8_t *p,
                                 const void *_mesg);

static void * H5O_mdj_msg_copy(const void *_mesg,
                               void *_dest);

static size_t H5O_mdj_msg_size(const H5F_t UNUSED *f,
                               hbool_t UNUSED disable_shared,
                               const void *_mesg);

static herr_t H5O_mdj_msg_reset(void *_mesg);

static herr_t H5O_mdj_msg_debug(H5F_t UNUSED *f,
                                hid_t UNUSED dxpl_id,
                                const void *_mesg,
                                FILE *stream,
                                int indent,
                                int fwidth);


/* This message derives from H5O message class */
const H5O_msg_class_t H5O_MSG_MDJ_CONF[1] = {{
    H5O_MDJ_MSG_ID,              /* message id number                    */
    "metadata journaling config", /* message name for debugging           */
    sizeof(H5O_mdj_msg_t),       /* native message size                  */
    0,                            /* messages are sharable?               */
    H5O_mdj_msg_decode,           /* decode message                       */
    H5O_mdj_msg_encode,           /* encode message                       */
    H5O_mdj_msg_copy,             /* copy the native value                */
    H5O_mdj_msg_size,             /* raw message size                     */
    H5O_mdj_msg_reset,            /* free internal memory                 */
    NULL,                         /* free method                          */
    NULL,                         /* file delete method                   */
    NULL,                         /* link method                          */
    NULL,                         /* set share method                     */
    NULL,                         /* can share method                     */
    NULL,                         /* pre copy native value to file        */
    NULL,                         /* copy native value to file            */
    NULL,                         /* post copy native value to file       */
    NULL,                         /* get creation index                   */
    NULL,                         /* set creation index                   */
    H5O_mdj_msg_debug            /* debug the message                    */
}};


/* Current version of the metadata journaling configuration information */
#define H5O_MDJ_CONF_VERSION      0

#define MDJ_MSG__JOURNALING_ENABLED_FLAG	0x0001


/*-------------------------------------------------------------------------
 * Function:    H5O_mdj_msg_decode
 *
 * Purpose:     Decode a journaling configuration message and return a 
 * 		pointer to a newly allocated H5O_mdj_msg_t struct.
 *
 * Return:      Success:        Ptr to new message in native struct.
 *              Failure:        NULL
 *
 * Programmer:  John Mainzer
 *              Dec. 14, 2007
 *
 *-------------------------------------------------------------------------
 */

static void *
H5O_mdj_msg_decode(H5F_t *f, 
		    hid_t UNUSED dxpl_id, 
		    unsigned UNUSED mesg_flags,
		    const uint8_t *p)
{
    uint16_t            flags = 0;      /* packed boolean fields */
    H5O_mdj_msg_t      *mesg;          /* Native message        */
    void                *ret_value;     /* Return value          */

    FUNC_ENTER_NOAPI_NOINIT(H5O_mdj_msg_decode)

    /* Sanity check */
    HDassert(f);
    HDassert(p);

    /* Version of message */
    if ( *p++ != H5O_MDJ_CONF_VERSION ) {

        HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, NULL, \
		    "bad version number for message")
    }

    /* Allocate space for message */

    if( NULL == ( mesg = H5MM_calloc(sizeof(H5O_mdj_msg_t)))) {

        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, \
	           "memory allocation failed for metadata journaling message.");

    }

    /* retrieve packed boolean flags, upack and load them. */
    UINT16DECODE(p, flags);

    if ( (flags & MDJ_MSG__JOURNALING_ENABLED_FLAG) != 0 ) {

	mesg->mdc_jrnl_enabled = TRUE;

    } else {

	mesg->mdc_jrnl_enabled = FALSE;

    }

    /* retrieve the journal block location */

    H5F_addr_decode(f, &p, &(mesg->mdc_jrnl_block_loc));

    /* retrieve the journal block length */

    H5F_DECODE_LENGTH(f, p, mesg->mdc_jrnl_block_len);

    /* Set return value */
    ret_value = (void *)mesg;

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5O_mdj_msg_decode() */


/*-------------------------------------------------------------------------
 * Function:    H5O_mdj_msg_encode
 *
 * Purpose:     Encode metadata journaling configuration message.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  John Mainzer
 *              Dec. 6, 2007
 *
 *-------------------------------------------------------------------------
 */

static herr_t
H5O_mdj_msg_encode(H5F_t *f, 
		    hbool_t UNUSED disable_shared, 
		    uint8_t *p, 
		    const void *_mesg)
{
    const H5O_mdj_msg_t *mesg = (const H5O_mdj_msg_t *)_mesg;
    uint16_t flags = 0;
    herr_t ret_value;

    FUNC_ENTER_NOAPI_NOINIT(H5O_mdj_msg_encode)

    /* Sanity check */
    HDassert(f);
    HDassert(p);
    HDassert(mesg);
    
    /* this error check exists to keep the compiler happy */
    if ( ( f == NULL ) || ( p == NULL ) || ( mesg == NULL ) ) {

	HGOTO_ERROR(H5E_SYSTEM, H5E_SYSERRSTR, FAIL, "Bad params on entry.");
    }

    /* setup the flags */
    if ( mesg->mdc_jrnl_enabled ) {

	flags |= MDJ_MSG__JOURNALING_ENABLED_FLAG;

    }

    /* Store version, flags, internal_loc, path_len, & path buffer */

    *p++ = H5O_MDJ_CONF_VERSION;

    UINT16ENCODE(p, flags);

    H5F_addr_encode(f, &p, mesg->mdc_jrnl_block_loc);
    
    H5F_ENCODE_LENGTH(f, p, mesg->mdc_jrnl_block_len);

done:

    FUNC_LEAVE_NOAPI(SUCCEED)

} /* H5O_mdj_msg_encode() */


/*-------------------------------------------------------------------------
 * Function:    H5O_mdj_msg_copy
 *
 * Purpose:     Copies a message from _MESG to _DEST, allocating _DEST if
 *              necessary.
 *
 * Return:      Success:        Ptr to _DEST
 *              Failure:        NULL
 *
 * Programmer:  John Mainzer
 *              Dec  6, 2007
 *
 *-------------------------------------------------------------------------
 */

static void *
H5O_mdj_msg_copy(const void *_mesg, void *_dest)
{
    const H5O_mdj_msg_t  *mesg = (const H5O_mdj_msg_t *)_mesg;
    H5O_mdj_msg_t        *dest = (H5O_mdj_msg_t *)_dest;
    void                  *ret_value;

    FUNC_ENTER_NOAPI_NOINIT(H5O_mdj_msg_copy)

    /* Sanity check */
    HDassert(mesg);

    if ( ( ! dest ) &&
         ( NULL == (dest = H5MM_malloc(sizeof(H5O_mdj_msg_t))) ) ) {

        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, \
	    "memory allocation failed for metadata journaling conf message")

    }

    /* now copy the message */
    dest->mdc_jrnl_enabled   = mesg->mdc_jrnl_enabled;
    dest->mdc_jrnl_block_loc = mesg->mdc_jrnl_block_loc;
    dest->mdc_jrnl_block_len = mesg->mdc_jrnl_block_len;

    /* Set return value */
    ret_value = dest;

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5O_mdj_msg_copy() */


/*-------------------------------------------------------------------------
 * Function:    H5O_mdj_msg_size
 *
 * Purpose:     Returns the size of the raw message in bytes not counting the
 *              message type or size fields, but only the data fields.
 *
 * Return:      Success:        Message data size in bytes w/o alignment.
 *              Failure:        0
 *
 * Programmer:  John Mainzer
 *              Dec. 12, 2007
 *
 *-------------------------------------------------------------------------
 */
static size_t
H5O_mdj_msg_size(const H5F_t *f, 
		  hbool_t UNUSED disable_shared, 
		  const void *_mesg)
{
    const H5O_mdj_msg_t * mesg = (const H5O_mdj_msg_t *)_mesg;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_mdj_msg_size)

    /* Sanity check */
    HDassert(f);
    HDassert(mesg);

    FUNC_LEAVE_NOAPI(MDJ_MSG_LEN(f))
} /* end H5O_mdj_msg_size() */


/*-------------------------------------------------------------------------
 * Function:    H5O_mdj_msg_reset
 *
 * Purpose:     Frees internal pointers and resets the message to an
 *              initial state.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  John Mainzer
 *              Dec. 13, 2007
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static herr_t
H5O_mdj_msg_reset(void *_mesg)
{
    H5O_mdj_msg_t *mesg = (H5O_mdj_msg_t *) _mesg;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_mdj_msg_reset)

    /* check args */
    HDassert(mesg);

    /* reset */
    mesg->mdc_jrnl_enabled = FALSE;
    mesg->mdc_jrnl_block_loc = HADDR_UNDEF;
    mesg->mdc_jrnl_block_len = 0;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5O_mdj_msg_reset() */


/*-------------------------------------------------------------------------
 * Function:    H5O_mdj_msg_debug
 *
 * Purpose:     Prints debugging info for the message.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  John Mainzer
 *              Dec. 7, 2007
 *
 *-------------------------------------------------------------------------
 */

static herr_t
H5O_mdj_msg_debug(H5F_t UNUSED *f, 
		   hid_t UNUSED dxpl_id, 
		   const void *_mesg, 
		   FILE *stream,
		   int indent, 
		   int fwidth)
{
    const H5O_mdj_msg_t *mesg = (const H5O_mdj_msg_t *)_mesg;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_mdj_msg_debug)

    /* Sanity check */
    HDassert(f);
    HDassert(mesg);
    HDassert(stream);
    HDassert(indent >= 0);
    HDassert(fwidth >= 0);

    HDfprintf(stream, "%*s%-*s %d\n", indent, "", fwidth,
             "mdc_jrnl_enabled:", 
	     (int)(mesg->mdc_jrnl_enabled));

    HDfprintf(stream, "%*s%-*s %a\n", indent, "", fwidth,
             "mdc_jrnl_bloc_loc:", 
	     mesg->mdc_jrnl_block_loc);

    HDfprintf(stream, "%*s%-*s %d\n", indent, "", fwidth,
              "mdc_jrnl_block_len:", 
	      (int)(mesg->mdc_jrnl_block_len));

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5O_mdj_msg_debug() */

