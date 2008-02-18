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
 * Created:     H5C2journal.c
 *              Dec 6 2007
 *              John Mainzer
 *
 * Purpose:     This file is a general catchall for functions supporting
 *              metadata journaling.  Note that journaling must be tighly
 *              integrated with the metadata cache, and thus this file only
 *              contains only that code that can be easily separated from 
 *              the rest of the cache code.
 *
 *              Observe also that to minimize overhead, it is quite possible
 *              that many of the functions in this file will be converted
 *              into macros at some point in the future.  
 *
 * Modifications:
 *
 *              None.
 *
 *-------------------------------------------------------------------------
 */

#define H5O_PACKAGE             /*suppress error about including H5Opkg   */
#define H5C2_PACKAGE            /*suppress error about including H5C2pkg  */

#include "H5private.h"          /* Generic Functions                    */
#include "H5Eprivate.h"         /* Error handling                       */
#include "H5Opkg.h"             /* Object headers                       */
#include "H5MMprivate.h"        /* Memory management                    */
#include "H5C2pkg.h"            /* Cache                                */

/**************************************************************************/
/********************** super block message support ***********************/
/**************************************************************************/

static void * H5O_mdj_conf_decode(H5F_t UNUSED *f,
                                  hid_t UNUSED dxpl_id,
                                  unsigned UNUSED mesg_flags,
                                  const uint8_t *p);

static herr_t H5O_mdj_conf_encode(H5F_t *f,
                                  hbool_t UNUSED disable_shared,
                                  uint8_t *p,
                                  const void *_mesg);

static void * H5O_mdj_conf_copy(const void *_mesg,
                                void *_dest);

static size_t H5O_mdj_conf_size(const H5F_t UNUSED *f,
                                hbool_t UNUSED disable_shared,
                                const void *_mesg);

static herr_t H5O_mdj_conf_reset(void *_mesg);

static herr_t H5O_mdj_conf_debug(H5F_t UNUSED *f,
                                 hid_t UNUSED dxpl_id,
                                 const void *_mesg,
                                 FILE *stream,
                                 int indent,
                                 int fwidth);


/* This message derives from H5O message class */
const H5O_msg_class_t H5O_MSG_MDJ_CONF[1] = {{
    H5O_MDJ_CONF_ID,              /* message id number                    */
    "metadata journaling config", /* message name for debugging           */
    sizeof(H5O_mdj_conf_t),       /* native message size                  */
    0,                            /* messages are sharable?               */
    H5O_mdj_conf_decode,          /* decode message                       */
    H5O_mdj_conf_encode,          /* encode message                       */
    H5O_mdj_conf_copy,            /* copy the native value                */
    H5O_mdj_conf_size,            /* raw message size                     */
    H5O_mdj_conf_reset,           /* free internal memory                 */
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
    H5O_mdj_conf_debug            /* debug the message                    */
}};


/* Current version of the metadata journaling configuration information */
#define H5O_MDJ_CONF_VERSION      0

#define MDJ_CONF__JOURNALING_ENABLED_FLAG	0x0001
#define MDJ_CONF__JOURNAL_IS_EXTERNAL_FLAG	0x0002


/*-------------------------------------------------------------------------
 * Function:    H5O_mdj_conf_decode
 *
 * Purpose:     Decode a journaling configuration message and return a 
 * 		pointer to a newly allocated H5O_mdj_conf_t struct.
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
H5O_mdj_conf_decode(H5F_t *f, 
		    hid_t UNUSED dxpl_id, 
		    unsigned UNUSED mesg_flags,
		    const uint8_t *p)
{
    const char * fcn_name = "H5O_mdj_conf_decode()";
    uint16_t            flags = 0;      /* packed boolean fields */
    H5O_mdj_conf_t      *mesg;          /* Native message        */
    void                *ret_value;     /* Return value          */

    FUNC_ENTER_NOAPI_NOINIT(H5O_btreek_decode)

    HDfprintf(stdout, "%s: entering.\n", fcn_name);

    /* Sanity check */
    HDassert(f);
    HDassert(p);

    /* Version of message */
    if ( *p++ != H5O_MDJ_CONF_VERSION ) {

        HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, NULL, \
		    "bad version number for message")
    }

    /* Allocate space for message */

    if( NULL == ( mesg = H5MM_calloc(sizeof(H5O_mdj_conf_t)))) {

        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, \
	    "memory allocation failed for metadata journaling config message.");

    }

    /* retrieve packed boolean flags, upack and load them. */
    UINT16DECODE(p, flags);

    if ( (flags & MDJ_CONF__JOURNALING_ENABLED_FLAG) != 0 ) {

	mesg->journaling_enabled = TRUE;

    } else {

	mesg->journaling_enabled = FALSE;

    }

    if ( (flags & MDJ_CONF__JOURNAL_IS_EXTERNAL_FLAG) != 0 ) {

	mesg->journal_is_external = TRUE;

    } else {

	mesg->journal_is_external = FALSE;

    }


    /* retrieve the internal journal location */

    H5F_addr_decode(f, &p, &(mesg->internal_journal_loc));


    /* retrieve the size of the external journal path buffer */

    UINT16DECODE(p, mesg->path_len);

    if ( ( ! mesg->journal_is_external ) &&
         ( mesg->path_len != 0 ) ) {

        HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, NULL, \
		    "internal journal and positive path len?!?");

    }

    if ( mesg->path_len > 0 ) {

        /* Allocate space for buffer */
        if ( NULL == 
	     (mesg->external_journal_file_path_ptr = 
	      H5MM_malloc(mesg->path_len + 1)) ) {

	    mesg = H5MM_xfree(mesg);

	    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, \
			"memory allocation failed for journal file path  buffer")

        } /* end if */

        /* Copy encoded journal file path info into buffer */
        HDmemcpy(mesg->external_journal_file_path_ptr, p, mesg->path_len);

    } else {

	mesg->external_journal_file_path_ptr = NULL;

    }

    /* Set return value */
    ret_value = (void *)mesg;

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5O_mdj_conf_decode() */


/*-------------------------------------------------------------------------
 * Function:    H5O_mdj_conf_encode
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
H5O_mdj_conf_encode(H5F_t *f, 
		    hbool_t UNUSED disable_shared, 
		    uint8_t *p, 
		    const void *_mesg)
{
    const char * fcn_name = "H5O_mdj_conf_encode()";
    const H5O_mdj_conf_t *mesg = (const H5O_mdj_conf_t *)_mesg;
    uint16_t flags = 0;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_mdj_conf_encode)

    HDfprintf(stdout, "%s: entering.\n", fcn_name);

    /* Sanity check */
    HDassert(f);
    HDassert(p);
    HDassert(mesg);

    /* setup the flags */
    if ( mesg->journaling_enabled ) {

	flags |= MDJ_CONF__JOURNALING_ENABLED_FLAG;

    }

    if ( mesg->journal_is_external ) {

	flags |= MDJ_CONF__JOURNAL_IS_EXTERNAL_FLAG;

    }

    /* Store version, flags, internal_loc, path_len, & path buffer */

    *p++ = H5O_MDJ_CONF_VERSION;

    UINT16ENCODE(p, flags);

    H5F_addr_encode(f, &p, mesg->internal_journal_loc);
    
    HDassert(mesg->path_len <= 65535);

    UINT16ENCODE(p, mesg->path_len);

    if ( mesg->path_len > 0 ) {

        HDmemcpy(p, mesg->external_journal_file_path_ptr, mesg->path_len);

    }

    FUNC_LEAVE_NOAPI(SUCCEED)

} /* H5O_mdj_conf_encode() */


/*-------------------------------------------------------------------------
 * Function:    H5O_mdj_conf_copy
 *
 * Purpose:     Copies a message from _MESG to _DEST, allocating _DEST if
 *              necessary.
 *
 * Return:      Success:        Ptr to _DEST
 *              Failure:        NULL
 *
 * Programmer:  Quincey Koziol
 *              Mar  1, 2007
 *
 *-------------------------------------------------------------------------
 */

static void *
H5O_mdj_conf_copy(const void *_mesg, void *_dest)
{
    const char * fcn_name = "H5O_mdj_conf_copy()";
    const H5O_mdj_conf_t  *mesg = (const H5O_mdj_conf_t *)_mesg;
    H5O_mdj_conf_t        *dest = (H5O_mdj_conf_t *)_dest;
    void                  *ret_value;

    FUNC_ENTER_NOAPI_NOINIT(H5O_mdj_conf_copy)

    HDfprintf(stdout, "%s: entering.\n", fcn_name);

    /* Sanity check */
    HDassert(mesg);

    if ( ( dest == NULL ) && 
         ( ( NULL == (dest = H5MM_malloc(sizeof(H5O_mdj_conf_t))) ) ||
	   ( ( mesg->path_len > 0 ) &&
	     ( ( NULL == (dest->external_journal_file_path_ptr = 
			  H5MM_malloc(mesg->path_len + 1)) ) ) ) ) ) {

        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, \
	    "memory allocation failed for metadata journaling conf message")

    }

    /* now copy the message */
    dest->journaling_enabled   = mesg->journaling_enabled;
    dest->journal_is_external  = mesg->journal_is_external;
    dest->internal_journal_loc = mesg->internal_journal_loc;
    dest->path_len             = mesg->path_len;

    if ( dest->path_len > 0 ) {

	HDmemcpy(dest->external_journal_file_path_ptr, 
		 mesg->external_journal_file_path_ptr, 
		 mesg->path_len);

    } else {

	dest->external_journal_file_path_ptr = NULL;
    }
    

    /* Set return value */
    ret_value = dest;

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5O_mdj_conf_copy() */


/*-------------------------------------------------------------------------
 * Function:    H5O_mdj_conf_size
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
H5O_mdj_conf_size(const H5F_t *f, 
		  hbool_t UNUSED disable_shared, 
		  const void *_mesg)
{
    const char * fcn_name = "H5O_mdj_conf_size()";
    const H5O_mdj_conf_t * mesg = (const H5O_mdj_conf_t *)_mesg;
    size_t                 ret_value;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_mdj_conf_size)

    HDfprintf(stdout, "%s: entering.\n", fcn_name);

    /* Sanity check */
    HDassert(f);
    HDassert(mesg);

    ret_value = 1 +                  /* Version number */
                2 +                  /* flags */
		H5F_SIZEOF_ADDR(f) + /* addr of internal journal */
                2 +                  /* length of external journal path */
                mesg->path_len;      /* external journal path buffer */

   FUNC_LEAVE_NOAPI(ret_value)

} /* end H5O_mdj_conf_size() */


/*-------------------------------------------------------------------------
 * Function:    H5O_mdj_conf_reset
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
H5O_mdj_conf_reset(void *_mesg)
{
    const char * fcn_name = "H5O_mdj_conf_reset()";
    H5O_mdj_conf_t *mesg = (H5O_mdj_conf_t *) _mesg;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_mdj_conf_reset);

    HDfprintf(stdout, "%s: entering.\n", fcn_name);

    /* check args */
    assert(mesg);

    /* reset */
    if ( mesg->external_journal_file_path_ptr != NULL )
    {
	HDassert( mesg->path_len > 0 );
	HDassert( mesg->journal_is_external );
        mesg->external_journal_file_path_ptr = 
		H5MM_xfree(mesg->external_journal_file_path_ptr);
	mesg->path_len = 0;
    }

    FUNC_LEAVE_NOAPI(SUCCEED);

} /* H5O_mdj_conf_reset() */


/*-------------------------------------------------------------------------
 * Function:    H5O_mdj_conf_debug
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
H5O_mdj_conf_debug(H5F_t UNUSED *f, 
		   hid_t UNUSED dxpl_id, 
		   const void *_mesg, 
		   FILE *stream,
		   int indent, 
		   int fwidth)
{
    const char * fcn_name = "H5O_mdj_conf_debug()";
    const H5O_mdj_conf_t *mesg = (const H5O_mdj_conf_t *)_mesg;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_btreek_debug)

    HDfprintf(stdout, "%s: entering.\n", fcn_name);

    /* Sanity check */
    HDassert(f);
    HDassert(mesg);
    HDassert(stream);
    HDassert(indent >= 0);
    HDassert(fwidth >= 0);

    HDfprintf(stream, "%*s%-*s %d\n", indent, "", fwidth,
             "journaling_enabled:", 
	     (int)(mesg->journaling_enabled));

    HDfprintf(stream, "%*s%-*s %d\n", indent, "", fwidth,
             "journal_is_external:", 
	     (int)(mesg->journal_is_external));
    
    HDfprintf(stream, "%*s%-*s %a\n", indent, "", fwidth,
             "internal_journal_loc:", 
	     mesg->internal_journal_loc);

    HDfprintf(stream, "%*s%-*s %d\n", indent, "", fwidth,
              "path_len:", 
	      (int)(mesg->path_len));

    if ( mesg->path_len > 0 ) {

        HDfprintf(stream, "%*s%-*s \"%s\"\n", indent, "", fwidth,
                  "external_journal_file_path_ptr:", 
	          (char *)(mesg->external_journal_file_path_ptr));

    } 

    FUNC_LEAVE_NOAPI(SUCCEED)

} /* end H5O_mdj_conf_debug() */
