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
 * 		and if so, the journal file magic and journal file path.
 *
 *		Note that the size of this message is variable.
 *
 * 		The mdj_msg only appears in the superblock extension.
 *
 * Modifications:
 *
 *              Re-worked message to include the journal file name and 
 *		magic, instead of simply containing a pointer to a 
 *		journal configuration block containing this data.
 *
 *-------------------------------------------------------------------------
 */

#define H5O_PACKAGE             /* suppress error about including H5Opkg */

#include "H5private.h"          /* Generic Functions                     */
#include "H5Eprivate.h"         /* Error handling                        */
#include "H5Opkg.h"             /* Object headers                        */
#include "H5MMprivate.h"        /* Memory management                     */

#define MDJ_MSG_LEN(f, pathlen)	                                                 \
                        ( 1 +      /* Version number */                      \
                	  2 +      /* flags */                               \
                          4 +      /* magic -- sizeof(int32_t) */            \
                          4 +      /* jnl file path len - sizeof(int32_t) */ \
                          pathlen + 1 ) /* jnl file path */

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
    char                ch;
    uint16_t            flags = 0;      /* packed boolean fields */
    int                 i;
    int32_t		journal_magic;  /* magic number -- if defined */
    int32_t		path_len;       /* journal file path length */
    H5O_mdj_msg_t      *mesg;           /* Native message        */
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

    if ( NULL == 
         (mesg = (H5O_mdj_msg_t *)H5MM_calloc(sizeof(H5O_mdj_msg_t))) ) {

        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, \
	           "memory allocation failed for metadata journaling message.");

    }

    /* retrieve packed boolean flags, upack and load them. */
    UINT16DECODE(p, flags);

    if ( (flags & MDJ_MSG__JOURNALING_ENABLED_FLAG) != 0 ) {

	mesg->mdc_jnl_enabled = TRUE;

    } else {

	mesg->mdc_jnl_enabled = FALSE;

    }


    /* get the journal file magic number */
    INT32DECODE(p, journal_magic);

    mesg->mdc_jnl_magic = journal_magic;


    /* get the journal file path length */
    INT32DECODE(p, path_len);

    if ( path_len > H5C2__MAX_JOURNAL_FILE_NAME_LEN ) {

        HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, NULL, "path length too big.")
    }

    mesg->mdc_jnl_file_name_len = (size_t)path_len;


    /* copy out the journal file path -- check length in passing.
     *
     * we could probably do this faster with a memcpy(), but this 
     * operation happens very infrequently, and doing it this way 
     * adds a bit of sanity checking.
     */
    i = 0;
    do {

	ch = (char)(*p++);
        mesg->mdc_jnl_file_name[i++] = ch;

    } while ( ( ch != '\0' ) && ( i <= path_len ) );

    if ( ( ch != '\0' ) || ( i != path_len + 1 ) ) {
    
        HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, NULL, \
		    "bad path and/or path len")
    }

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
    int32_t magic;
    int32_t path_len;
    herr_t ret_value;

    FUNC_ENTER_NOAPI_NOINIT(H5O_mdj_msg_encode)

    /* Sanity check */
    HDassert(f);
    HDassert(p);
    HDassert(mesg);
    
    if ( ( f == NULL ) || ( p == NULL ) || ( mesg == NULL ) ) {

	HGOTO_ERROR(H5E_SYSTEM, H5E_SYSERRSTR, FAIL, "Bad params on entry.");
    }

    if ( mesg->mdc_jnl_file_name_len > H5C2__MAX_JOURNAL_FILE_NAME_LEN ) {

        HGOTO_ERROR(H5E_SYSTEM, H5E_SYSERRSTR, FAIL, \
        	"Bad params on entry -- path len too long.");
    }

    /* setup the flags */
    if ( mesg->mdc_jnl_enabled ) {

	flags |= MDJ_MSG__JOURNALING_ENABLED_FLAG;

    }

    magic = mesg->mdc_jnl_magic;

    path_len = (int32_t)(mesg->mdc_jnl_file_name_len);

    /* Store version, flags, magic, path_len, & path */

    *p++ = H5O_MDJ_CONF_VERSION;

    UINT16ENCODE(p, flags);

    INT32ENCODE(p, magic);

    INT32ENCODE(p, path_len);

    HDmemcpy(p, mesg->mdc_jnl_file_name, path_len + 1);
    p += path_len + 1;

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

    if ( mesg->mdc_jnl_file_name_len > H5C2__MAX_JOURNAL_FILE_NAME_LEN ) {

        HGOTO_ERROR(H5E_SYSTEM, H5E_SYSERRSTR, FAIL, "path len too long.");
    }

    if ( ( ! dest ) &&
         ( NULL == (dest = H5MM_malloc(sizeof(H5O_mdj_msg_t))) ) ) {

        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, \
	    "memory allocation failed for metadata journaling conf message")

    }

    /* now copy the message */
    dest->mdc_jnl_enabled       = mesg->mdc_jnl_enabled;
    dest->mdc_jnl_magic         = mesg->mdc_jnl_magic;
    dest->mdc_jnl_file_name_len = mesg->mdc_jnl_file_name_len;

    /* copy the journal file path -- check length in passing.
     *
     * we could probably do this faster with a memcpy(), but this 
     * operation happens very infrequently, and doing it this way 
     * adds a bit of sanity checking.
     */
    if ( mesg->mdc_jnl_file_name_len == 0 ) {

        (dest->mdc_jnl_file_name)[0] = '\0';

    } else {

        char ch;
        int i = 0;
        size_t path_len;

        path_len = mesg->mdc_jnl_file_name_len;
    
        do {

	    ch = (mesg->mdc_jnl_file_name)[i];
            (dest->mdc_jnl_file_name)[i++] = ch;

        } while ( ( ch != '\0' ) && ( i <= path_len ) );

        if ( ( ch != '\0' ) || ( i != path_len + 1 ) ) {
    
            HGOTO_ERROR(H5E_OHDR, H5E_CANTCOPY, NULL, \
		    "bad path and/or path len???")
        }
    }

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
    HDassert( f );
    HDassert( mesg );
    HDassert( mesg->mdc_jnl_file_name_len >= 0 );

    FUNC_LEAVE_NOAPI(MDJ_MSG_LEN(f, mesg->mdc_jnl_file_name_len))

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
    mesg->mdc_jnl_enabled        = FALSE;
    mesg->mdc_jnl_magic          = 0;
    mesg->mdc_jnl_file_name_len  = 0;
    (mesg->mdc_jnl_file_name)[0] = '\0';

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
             "mdc_jnl_enabled:", 
	     (int)(mesg->mdc_jnl_enabled));

    HDfprintf(stream, "%*s%-*s %d\n", indent, "", fwidth,
             "mdc_jnl_magic:", 
	     (int)(mesg->mdc_jnl_magic));

    HDfprintf(stream, "%*s%-*s %d\n", indent, "", fwidth,
             "mdc_jnl_file_name_len:", 
	     (int)(mesg->mdc_jnl_file_name_len));

    HDfprintf(stream, "%*s%-*s \"%s\"\n", indent, "", fwidth,
             "mdc_jnl_file_name:", 
	     (char *)(mesg->mdc_jnl_file_name));

    FUNC_LEAVE_NOAPI(SUCCEED)

} /* end H5O_mdj_msg_debug() */

