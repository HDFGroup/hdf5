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
/**************** super block message support declarations ****************/
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


/**************************************************************************/
/***************** super block message support functions ******************/
/**************************************************************************/


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


/**************************************************************************/
/********************** journal file management code **********************/
/**************************************************************************/

/******************************************************************************
 *
 * Function:		H5C2_jb__flush_full_buffers
 *
 * Programmer:		Mike McGreevy <mcgreevy@hdfgroup.org>
 *			Wednesday, February 6, 2008
 *
 * Purpose:		Flush the specified number of buffers to disk, starting
 *			from the buffer indexed by struct_ptr->get.
 *
 * Returns:		SUCCEED on success.
 *
 ******************************************************************************/

herr_t 
H5C2_jb__flush_full_buffers(H5C2_jbrb_t * struct_ptr)	
{
    int result;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5C2_jb__flush_full_buffers, FAIL)
	
    /* this asserts that at least one buffer is in use */
    HDassert(struct_ptr->bufs_in_use > 0);
    /* write an assert to verify that at least one buffer is full */
    /* code */

    /* flush all full, dirtied journal buffers to disk */
    if (struct_ptr->get < struct_ptr->put) {

	/* can write solid chunk from get up to, but not 
	 * including, put 
	 */
	HDwrite(struct_ptr->journal_file_fd, 
	        (*struct_ptr->buf)[struct_ptr->get], 
	        (struct_ptr->put - struct_ptr->get) * struct_ptr->buf_size);

	struct_ptr->bufs_in_use -= (struct_ptr->put - struct_ptr->get);

    } else {

	/* write from get through end of buffer */
/* JRM: need to check return on this call, and flag an error if it fails */
	result = HDwrite(struct_ptr->journal_file_fd, 
	      (*struct_ptr->buf)[struct_ptr->get], 
	      (struct_ptr->num_bufs - struct_ptr->get) * struct_ptr->buf_size);

	if ( result == -1 ) {

            HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, \
		        "Journal file write failed.")
        }

	struct_ptr->bufs_in_use -= (struct_ptr->num_bufs - struct_ptr->get);

	/* write from start of buffer up to, but not including, put */
	/* note that when get = put = 0, the full ring buffer has 
	 * already written at this point, and this write does nothing 
	 */
	result = HDwrite(struct_ptr->journal_file_fd, 
	      (*struct_ptr->buf)[0], 
	      (struct_ptr->put) * struct_ptr->buf_size);

	if ( result == -1 ) {

            HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, \
		        "Journal file write failed.")
        }

	struct_ptr->bufs_in_use -= struct_ptr->put;
    }
	
    HDassert(struct_ptr->bufs_in_use <= 1);

    /* perform a sync to ensure everything gets to disk before continuing */
    if ( fsync(struct_ptr->journal_file_fd) < 0 ) {

        HGOTO_ERROR(H5E_IO, H5E_SYNCFAIL, FAIL, "Jounal file sync failed.")

    } /* end if */
	
    /* update get pointer */
    struct_ptr->get = struct_ptr->put;
	
    /* record last transaction number that made it to disk */
    if (struct_ptr->put == 0) {

	struct_ptr->last_trans_on_disk = 
		(*struct_ptr->trans_on_disk_record)[struct_ptr->num_bufs - 1];

    } else {

	struct_ptr->last_trans_on_disk = 
		(*struct_ptr->trans_on_disk_record)[struct_ptr->put - 1];
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5C2_jb__flush_full_buffers */


/******************************************************************************
 *
 * Function:		H5C2_jb__flush
 *
 * Programmer:		Mike McGreevy <mcgreevy@hdfgroup.org>
 *			Wednesday, February 6, 2008
 *
 * Purpose:		Verify that there is no transaction in progress. Then
 * 			flush all journal entries in the journal buffers to the
 * 			journal file. Do not return until all entries are on
 * 			disk.
 *
 * Returns:		SUCCEED on success.
 *
 ******************************************************************************/

herr_t 
H5C2_jb__flush(H5C2_jbrb_t * struct_ptr)
{
    int result;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5C2_jb__flush, FAIL)

    /* Check Arguments */
    HDassert(struct_ptr);
    HDassert(struct_ptr->magic == H5C2__H5C2_JBRB_T_MAGIC);
	
    /* Check if transaction is in progress */

    if (struct_ptr->trans_in_prog != FALSE) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                    "Attempt to flush buffers with transaction in progress.")
    } /* end if */

    /* flush all full, dirtied journal buffers to disk */
    if (struct_ptr->get < struct_ptr->put) {

	/* can write solid chunk from get up to, but not including, put */
	result = HDwrite(struct_ptr->journal_file_fd, 
	            (*struct_ptr->buf)[struct_ptr->get], 
	            (struct_ptr->put - struct_ptr->get) * struct_ptr->buf_size);

	if ( result == -1 ) {

            HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, \
		        "Journal file write failed.")
        }

	struct_ptr->bufs_in_use -= (struct_ptr->put - struct_ptr->get);

    } else if ( ( struct_ptr->get == struct_ptr->put ) &&
		( struct_ptr->cur_buf_free_space != 0 ) ) {

	/* do nothing for now ... this is the case when no buffer is full. */

    } else {
	/* write from get through end of buffer */
	result = HDwrite(struct_ptr->journal_file_fd, 
	      (*struct_ptr->buf)[struct_ptr->get], 
	      (struct_ptr->num_bufs - struct_ptr->get) * struct_ptr->buf_size);

	if ( result == -1 ) {

            HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, \
		        "Journal file write failed.")
        }

	struct_ptr->bufs_in_use -= (struct_ptr->num_bufs - struct_ptr->get);

	/* write from start of buffer up to, but not including, put */
	/* note that when get = put = 0, the full ring buffer has already 
	 * written at this point, and this write does nothing 
	 */
	result = HDwrite(struct_ptr->journal_file_fd, 
	               (*struct_ptr->buf)[0], 
	               (struct_ptr->put) * struct_ptr->buf_size);

	if ( result == -1 ) {

            HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, \
		        "Journal file write failed.")
        }

	struct_ptr->bufs_in_use -= struct_ptr->put;
    }

    /* flush partially filled portion of current journal buffer to disk */
    if ( ( struct_ptr->cur_buf_free_space != 0 ) &&
         ( struct_ptr->cur_buf_free_space != struct_ptr->buf_size ) ) {

	result = HDwrite(struct_ptr->journal_file_fd, 
	               (*struct_ptr->buf)[struct_ptr->put], 
	               struct_ptr->buf_size - struct_ptr->cur_buf_free_space);

	if ( result == -1 ) {

            HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, \
		        "Journal file write failed.")
        }

	struct_ptr->bufs_in_use--;
    }

    HDassert(struct_ptr->bufs_in_use == 0);

    /* perform sync to ensure everything gets to disk before returning */
    if ( fsync(struct_ptr->journal_file_fd) < 0 ) {

        HGOTO_ERROR(H5E_IO, H5E_SYNCFAIL, FAIL, "Jounal file sync failed.")
    } /* end if */

    /* MIKE: optimization note: don't reset to top of ring buffer. 
     * instead, keep filling out current buffer so we can keep writes 
     * on block boundaries. 
     */
    struct_ptr->cur_buf_free_space = struct_ptr->buf_size;
    struct_ptr->rb_free_space = struct_ptr->num_bufs * struct_ptr->buf_size;
    struct_ptr->head = (*struct_ptr->buf)[0];
    struct_ptr->put = 0;

    /* update get pointer */
    struct_ptr->get = struct_ptr->put;

    /* record last transaction number that made it to disk */
    if (struct_ptr->put == 0) {

	struct_ptr->last_trans_on_disk = 
		(*struct_ptr->trans_on_disk_record)[struct_ptr->num_bufs - 1];

    } else {

	struct_ptr->last_trans_on_disk = 
		(*struct_ptr->trans_on_disk_record)[struct_ptr->put - 1];
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5C2_jb__flush */


/******************************************************************************
 *
 * Function:		H5C2_jb__write_to_buffer
 *
 * Programmer:		Mike McGreevy <mcgreevy@hdfgroup.org>
 *			Wednesday, February 6, 2008
 *
 * Purpose:		Put the contents of data into the journal buffers. This
 * 			is done as follows: While the data to be written is 
 * 			larger than the amount of space left in the ring buffer,
 * 			the ring buffer is filled to capacity with data and
 *			flushed. This repeats until the unwritten data remaining
 * 			can fit in the ring buffer without having to loop around
 *			to the top.
 *
 *			At this point, the rest of the data can just be written
 *			without having to break it up further. In the event
 *			the data covers more than one journal buffer, the get and
 *			put indices are updated to state this fact. Any journal
 *			buffers that were filled during the write are flushed.
 *
 * Returns:		SUCCEED on success.
 *
 ******************************************************************************/

herr_t 
H5C2_jb__write_to_buffer(H5C2_jbrb_t * struct_ptr,	
			size_t size,			
			const char * data)
{
    herr_t ret_value = SUCCEED;
	
    FUNC_ENTER_NOAPI(H5C2_jb__write_to_buffer, FAIL)

    /* Check Arguments */
    HDassert(struct_ptr);
    HDassert(data);
    HDassert(struct_ptr->magic == H5C2__H5C2_JBRB_T_MAGIC);
    HDassert(strlen(data) == size);
    HDassert(struct_ptr->rb_free_space <= 
		    struct_ptr->num_bufs * struct_ptr->buf_size);
    HDassert(struct_ptr->rb_free_space > 0); /* JOHN: will remain true in interesting ways */

    /* If the data size exceeds the bounds of the ring buffer's allocated 
     * memory, loop around to top 
     */
    if (size >= struct_ptr->rb_free_space) {

	while (size >= struct_ptr->rb_free_space) {
			
	    /* Assertions */
	    HDassert(size != 0);
	    HDassert(strlen(data) >= struct_ptr->rb_free_space);

	    /* fill up remaining space in the ring buffer */
	    memcpy(struct_ptr->head, data, struct_ptr->rb_free_space);
			
	    /* move head to point to start of ring buffer */
	    struct_ptr->head = (*struct_ptr->buf)[0];

	    /* reset put index */
	    struct_ptr->put = 0;

	    /* update bufs_in_use as necessary */
	    struct_ptr->bufs_in_use = struct_ptr->num_bufs - struct_ptr->get;

	    /* flush buffers */
	    if ( H5C2_jb__flush_full_buffers(struct_ptr) < 0 ) {

		 HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                             "H5C2_jb__flush_full_buffers() failed.\n")
            }

	    /* update remaining size of data to be written */
	    size = size - struct_ptr->rb_free_space;

	    /* update the data pointer to point to the remaining data to be 
	     * written 
	     */
	    data = &data[struct_ptr->rb_free_space];

	    /* update the amount of space that is available to write to */
	    struct_ptr->rb_free_space = 
		    struct_ptr->buf_size * struct_ptr->num_bufs;

	    /* update the amount of space in the current buffer */
	    struct_ptr->cur_buf_free_space = struct_ptr->buf_size;

	} /* end while */
    } /* end if */
	
    /* If the size of the data exceeds the bounds of a single journal 
     * buffer, will write into multiple 
     */
    if (size > struct_ptr->cur_buf_free_space) {

	HDassert(struct_ptr->cur_buf_free_space != 0);

	/* write data into journal buffers */
	memcpy(struct_ptr->head, data, size);

	/* update head pointer */
	struct_ptr->head = &struct_ptr->head[size];

	/* update put index */
	struct_ptr->put += 
		(size-struct_ptr->cur_buf_free_space)/(struct_ptr->buf_size) + 1; 

	/* update current buffer usage */
	struct_ptr->cur_buf_free_space = 
		struct_ptr->rb_free_space - size - 
		(struct_ptr->num_bufs - (struct_ptr->put + 1)) * 
			(struct_ptr->buf_size );

	/* update bufs_in_use as necessary */
	struct_ptr->bufs_in_use = struct_ptr->put - struct_ptr->get;
	if (struct_ptr->cur_buf_free_space < struct_ptr->buf_size) {

	    struct_ptr->bufs_in_use++;
        }

	/* flush buffers */
	if ( H5C2_jb__flush_full_buffers(struct_ptr) < 0 ) {

            HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                        "H5C2_jb__flush_full_buffers() failed.\n")
        }

	/* update ring buffer usage */
	struct_ptr->rb_free_space -= size;

    } /* end if */

    /* if the data can fit in the remaining space in the current journal 
     * buffer indexed by put 
     */
    else if (size > 0)  {

	HDassert(size <= struct_ptr->cur_buf_free_space);
		
	/* write data into journal buffer */
	memcpy(struct_ptr->head, data, size);

	/* increment bufs_in_use as necessary */
	if ( ( struct_ptr->bufs_in_use == 0 ) ) {

	    struct_ptr->bufs_in_use++;
        }

	/* update head pointer */
	struct_ptr->head = &struct_ptr->head[size];

	/* update current buffer usage */
	struct_ptr->cur_buf_free_space -= size;

	/* update ring buffer usage */
	struct_ptr->rb_free_space -= size;
		
	/* if buffer is full, flush it, and loop to the top of the 
	 * ring buffer if at the end. 
	 */
	if (struct_ptr->cur_buf_free_space == 0) {

	    if ( struct_ptr->put != (struct_ptr->num_bufs - 1) ) {

		struct_ptr->put += 1;

	    } else {
		struct_ptr->put = 0;
		struct_ptr->head = (*struct_ptr->buf)[0];
		struct_ptr->rb_free_space = 
			struct_ptr->buf_size * struct_ptr->num_bufs;
	    }

	    if ( H5C2_jb__flush_full_buffers(struct_ptr) < 0 ) {

                HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                            "H5C2_jb__flush_full_buffers() failed.\n")
            }

	    struct_ptr->cur_buf_free_space = struct_ptr->buf_size;
	}

    } /* end else */
	
    HDassert(struct_ptr->bufs_in_use <= struct_ptr->num_bufs);

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5C2_jb__write_to_buffer */


/******************************************************************************
 *
 * Function:		H5C2_jb__init
 *
 * Programmer:		Mike McGreevy <mcgreevy@hdfgroup.org>
 *			Tuesday, February 5, 2008
 *
 * Purpose:		Initialize the supplied instance of H5C2_jbrb_t as
 *			specified by the buf_size and num_bufs fields. Open the
 *			journal file whose name is supplied in journal_file_name
 *			for either synchronous or asynchronous I/O as specified
 * 			by use_aio.
 *
 * Returns:		SUCCEED on success.
 *
 ******************************************************************************/

herr_t 
H5C2_jb__init(H5C2_jbrb_t * struct_ptr,  	
	      char * HDF5_file_name,	 	
	      char * journal_file_name, 	
	      size_t buf_size,		
	      int num_bufs,		 	
	      hbool_t use_aio,		
 	      hbool_t human_readable)
{
    char 	temp[150];
    int 	i;
    herr_t 	ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5C2_jb__init, FAIL)

    /* Check Arguments */
    HDassert(struct_ptr);
    HDassert(HDF5_file_name);
    HDassert(journal_file_name);
    HDassert(buf_size > 0);
    HDassert(num_bufs > 0);
    HDassert(struct_ptr->magic == H5C2__H5C2_JBRB_T_MAGIC);
	
    /* Open journal file */
    struct_ptr->journal_file_fd = 
	    open(journal_file_name, O_WRONLY|O_CREAT|O_EXCL, 0777);

    if ( struct_ptr->journal_file_fd  == -1) {

	/* JRM -- why are we trying to close a file we have just failed to
	 *        create?  Commenting this out for now -- put it back in 
	 *        if there is a good reason to.
	 */
	/* HDclose(struct_ptr->journal_file_fd); */
        HGOTO_ERROR(H5E_FILE, H5E_CANTCREATE, FAIL, \
                    "Can't create journal file.  Does it already exist?")
    } /* end if */

    /* Initialize Fields of H5C2_jbrb_t structure */
    struct_ptr->jname = journal_file_name;
    struct_ptr->hdf5_file_name = HDF5_file_name;
    struct_ptr->buf_size = buf_size;
    struct_ptr->num_bufs = num_bufs;
    struct_ptr->use_aio = use_aio;
    struct_ptr->human_readable = human_readable;
    struct_ptr->bufs_in_use = 0;
    struct_ptr->trans_in_prog = FALSE;
    struct_ptr->get = 0;
    struct_ptr->put = 0;
    struct_ptr->jvers = H5C2__JOURNAL_VERSION;
    struct_ptr->cur_trans = 0;
    struct_ptr->last_trans_on_disk = 0;
    struct_ptr->cur_buf_free_space = struct_ptr->buf_size;
    struct_ptr->rb_free_space = struct_ptr->num_bufs * struct_ptr->buf_size;
    struct_ptr->jentry_written = FALSE;
    struct_ptr->journal_is_empty = TRUE;
	
    /* Allocate space for the ring buffer's journal buffer pointers */
    struct_ptr->buf = H5MM_malloc(struct_ptr->num_bufs * sizeof(char *));

    if ( struct_ptr->buf == NULL ) {

	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, \
                    "allocation of buf pointer array failed.");
    } /* end if */
	
    /* Allocate space for journal buffers */
    (*struct_ptr->buf)[0] = 
            H5MM_malloc(struct_ptr->buf_size * struct_ptr->num_bufs);

    if ( (*struct_ptr->buf)[0] == NULL ) {

	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, \
                    "allocation of buffers failed.");
    } /* end if */

    /* Allocate space for the purposes of tracking the last 
     * transaction on disk 
     */
    struct_ptr->trans_on_disk_record = 
    	H5MM_malloc(struct_ptr->num_bufs * sizeof(unsigned long));

    if ( struct_ptr->trans_on_disk_record == NULL ) {

	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, \
                    "allocation of trans_on_disk_record failed.");
    } /* end if */
	
    /* Initialize last transaction on disk record array */
    for (i=0; i<struct_ptr->num_bufs; i++)
    {
	(*struct_ptr->trans_on_disk_record)[i] = 0;
    }
	
    /* Make journal buffer pointers point to the right location in 
     * chunk of allocated memory above 
     */
    for (i=1; i<struct_ptr->num_bufs; i++)
    {
	(*struct_ptr->buf)[i] = 
		&((*struct_ptr->buf)[0])[i * struct_ptr->buf_size];
    }

    /* Define head pointer to point at where we are writing to in the buffer */
    struct_ptr->head = (*struct_ptr->buf)[struct_ptr->put];
	
    /* Format the header message into a temporary buffer */
    HDsnprintf(temp, 
        (size_t)150,
	"0 ver_num %ld target_file_name %s creation_date %s human_readable %d\n",
	struct_ptr->jvers, 
	struct_ptr->hdf5_file_name, 
	__DATE__, 
	struct_ptr->human_readable);

    /* Write the header message into the ring buffer */
    if ( H5C2_jb__write_to_buffer(struct_ptr, strlen(temp), temp ) < 0) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                    "H5C2_jb__write_to_buffer() failed.\n")
    } /* end if */

    /* Update boolean flags */
    struct_ptr->header_present = TRUE;
    struct_ptr->journal_is_empty = FALSE;

done:
	
    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5C2_jb__init */


/******************************************************************************
 *
 * Function:		H5C2_jb__start_transaction
 *
 * Programmer:		Mike McGreevy <mcgreevy@hdfgroup.org>
 *			Wednesday, February 6, 2008
 *
 * Purpose:		Verify that there is no transaction in progress, and
 *			that the supplied transaction number is next in
 *			sequence. Then construct a start transaction message, 
 *			and write it to the current journal buffer. Make note
 *			of the fact that the supplied transaction is in
 *			progress.
 *
 * Returns:		SUCCEED on success.
 *
 ******************************************************************************/

herr_t 
H5C2_jb__start_transaction(H5C2_jbrb_t * struct_ptr,
			   unsigned long trans_num)

{
    char temp[150];
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5C2_jb__start_transaction, FAIL)
	
    /* Check Arguments */
    HDassert(struct_ptr);
    HDassert(struct_ptr->magic == H5C2__H5C2_JBRB_T_MAGIC);
	
    /* Verify that there is no transaction in progress */
    if ( struct_ptr->trans_in_prog != FALSE ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                    "Transaction already in progress.")
    } /* end if */

    /* Verify that the supplied transaction number is next in sequence */
    if ( (struct_ptr->cur_trans + 1) != trans_num ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                    "New transaction out of sequence.")
    } /* end if */

    /* Verify that header message is present in journal file or ring buffer. 
     * If not, write it. 
     */
    if ( struct_ptr->header_present == FALSE ) {

	HDsnprintf(temp, 
	(size_t)150,
	"0 ver_num %ld target_file_name %s creation_date %s human_readable %d\n",
	    struct_ptr->jvers, 
	    struct_ptr->hdf5_file_name, 
	    __DATE__, 
	    struct_ptr->human_readable);

        if ( H5C2_jb__write_to_buffer(struct_ptr, strlen(temp), temp) < 0 ) {

            HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                        "H5C2_jb__write_to_buffer() failed.\n")
        } /* end if */

	struct_ptr->header_present = 1;
	struct_ptr->journal_is_empty = 0;
    } /* end if */

    /* Write start transaction message */
    HDsnprintf(temp, (size_t)150, "1 bgn_trans %ld\n", trans_num);
    if ( H5C2_jb__write_to_buffer(struct_ptr, strlen(temp), temp) < 0 ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                    "H5C2_jb__write_to_buffer() failed.\n")
    } /* end if */
		
    /* Make note of the fact that supplied transaction is in progress */
    struct_ptr->trans_in_prog = TRUE;
    struct_ptr->cur_trans = trans_num;

done:
	
    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5C2_jb__start_transaction */


/******************************************************************************
 *
 * Function:		H5C2_jb__journal_entry
 *
 * Programmer:		Mike McGreevy <mcgreevy@hdfgroup.org>
 *			Wednesday, February 6, 2008
 *
 * Purpose:		Verify that the specified transaction is open. Then
 *			construct a journal entry recording the supplied base
 *			address, length, and body, and write it to the current
 *			journal buffer.
 *
 * Returns:		SUCCEED on success.
 *
 ******************************************************************************/

herr_t 
H5C2_jb__journal_entry(H5C2_jbrb_t * struct_ptr,
			unsigned long trans_num,
			haddr_t base_addr,
			size_t length,
			char * body)
{
    char * temp = NULL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5C2_jb__journal_entry, FAIL)
	
    /* Check Arguments */
    HDassert(struct_ptr);
    HDassert(body);
    HDassert(struct_ptr->magic == H5C2__H5C2_JBRB_T_MAGIC);
	
    /* Verify that the supplied transaction is in progress */
    if ( ( struct_ptr->trans_in_prog != TRUE ) ||
         ( struct_ptr->cur_trans != trans_num ) ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                    "Transaction not in progress or bad transaction number.")
    } /* end if */

    if ( (temp = H5MM_malloc(length + 100)) == NULL ) {

	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, \
                   "allocation of assembly buffer failed.");
    }

    /* Write journal entry */
    /* JRM -- Modified to printf base addr in HEX 
     *     -- Must check with Quincey about using the %a option in 
     *        format string for haddr_t.  Casting to unsigned long 
     *        for now.
     */
    HDsnprintf(temp, 
               (size_t)(length + 100),
               "2 trans_num %ld length %d base_addr Ox%lx body ", 
 	       trans_num, 
	       length, 
	       (unsigned long)base_addr); /* <== fix this */

    if ( H5C2_jb__write_to_buffer(struct_ptr, strlen(temp), temp) < 0 ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                    "H5C2_jb__write_to_buffer() failed.\n")
    } /* end if */

    /* JRM -- the buffer will contain binary -- need to conver to hex */
    if ( H5C2_jb__write_to_buffer(struct_ptr, length, body) < 0 ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                    "H5C2_jb__write_to_buffer() failed.\n")
    } /* end if */

    if (H5C2_jb__write_to_buffer(struct_ptr, 1, "\n") < 0) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                    "H5C2_jb__write_to_buffer() failed.\n")
    } /* end if */
	
    /* Indicate that at least one journal entry has been written under 
     * this transaction 
     */
    if ( struct_ptr->jentry_written == FALSE ) {

	struct_ptr->jentry_written = TRUE;
    }

done:

    if ( temp != NULL )
    {
        temp = H5MM_xfree(temp);
        if ( temp != NULL ) {

            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTFREE, FAIL, \
                        "free of assembly buffer failed.");
        }
    }

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5C2_jb__journal_entry */


/******************************************************************************
 *
 * Function:		H5C2_jb__end_transaction
 *
 * Programmer:		Mike McGreevy <mcgreevy@hdfgroup.org>
 *			Wednesday, February 6, 2008
 *
 * Purpose:		Verify that the supplied transaction is in progress,
 *			and that at least one journal entry has been written 
 *			under it. Then construct an end transaction message,
 *			and write it to the current journal buffer. Make note
 *			that the supplied transaction is closed, and that no
 *			transaction is in progress.
 *
 * Returns:		SUCCEED on success.
 *
 ******************************************************************************/

herr_t 
H5C2_jb__end_transaction(H5C2_jbrb_t * struct_ptr,
			 unsigned long trans_num)
{
    char temp[25];
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5C2_jb__end_transaction, FAIL)
	
    /* Check Arguments */
    HDassert(struct_ptr);
    HDassert(struct_ptr->magic == H5C2__H5C2_JBRB_T_MAGIC);
	
    /* Verify that the supplied transaction is in progress */
    if ( ( struct_ptr->trans_in_prog != TRUE ) ||
         ( struct_ptr->cur_trans != trans_num ) ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                    "Transaction not in progress or bad transaction number.")
    } /* end if */
	
    /* Verify that at least one journal entry has been written under 
     * the current transaction 
     */
    if ( struct_ptr->jentry_written != TRUE ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                    "Empty transaction -- at least one journal entry required.")
    } /* end if */

    /* Write end transaction message */
    HDsnprintf(temp, (size_t)25, "3 end_trans %ld\n", trans_num);

    if ( H5C2_jb__write_to_buffer(struct_ptr, strlen(temp), temp) < 0 ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                    "H5C2_jb__write_to_buffer() failed.\n")
    } /* end if */

    /* record the transaction number in the appropriate buffer index */
    if ( ( struct_ptr->cur_buf_free_space == struct_ptr->buf_size ) &&
         ( struct_ptr->put != 0 ) ) {

	(*struct_ptr->trans_on_disk_record)[struct_ptr->put - 1] = trans_num;

    } else if ( (struct_ptr->cur_buf_free_space == struct_ptr->buf_size ) &&
                (struct_ptr->put == 0 ) ) {

	(*struct_ptr->trans_on_disk_record)[struct_ptr->num_bufs-1] = trans_num;

    } else {

	(*struct_ptr->trans_on_disk_record)[struct_ptr->put] = trans_num;
    }

    /* reset boolean flag indicating if at least one journal entry has 
     * been written under transaction 
     */
    struct_ptr->jentry_written = FALSE;

    /* Close current transaction */
    struct_ptr->trans_in_prog = FALSE;

done:
	
    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5C2_jb__end_transaction */


/******************************************************************************
 *
 * Function:		H5C2_jb__comment
 *
 * Programmer:		Mike McGreevy <mcgreevy@hdfgroup.org>
 *			Wednesday, February 6, 2008
 *
 * Purpose:		Insert the supplied comment in the journal file. This 
 * 			call may be ignored if the journal file is machine 
 *			readable.
 *
 * Returns:		SUCCEED on success.
 *
 ******************************************************************************/

herr_t 
H5C2_jb__comment(H5C2_jbrb_t * struct_ptr,
		 char * comment_ptr)
{
    char * temp = NULL;
    size_t temp_len;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5C2_jb__comment, FAIL)
	
    /* Check Arguments */
    HDassert(struct_ptr);
    HDassert(comment_ptr);
    HDassert(struct_ptr->magic == H5C2__H5C2_JBRB_T_MAGIC);

    temp_len = strlen(comment_ptr) + 11;
    if ( ( temp = H5MM_malloc(strlen(comment_ptr) + 11) ) == NULL ) {

	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, \
                   "allocation of temp buffer failed.");
    } /* end if */

    /* Write comment message */
    HDsnprintf(temp, temp_len, "C comment %s\n", comment_ptr);

    if ( H5C2_jb__write_to_buffer(struct_ptr, strlen(temp), temp) < 0 ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                    "H5C2_jb__write_to_buffer() failed.\n")
    } /* end if */

done:

    if ( temp != NULL ) {

        temp = H5MM_xfree(temp);
        if ( temp != NULL ) {

            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTFREE, FAIL, \
                        "free of assembly buffer failed.");
        }
    }

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5C2_jb__comment */


/******************************************************************************
 *
 * Function:		H5C2_jb__get_last_transaction_on_disk
 *
 * Programmer:		Mike McGreevy <mcgreevy@hdfgroup.org>
 *			Wednesday, February 6, 2008
 *
 * Purpose:		Lookup the number of the last transaction to have been
 *			fully written to disk, and place its transaction
 *			number in *trans_num_ptr. If no transaction has made
 *			it to disk, load zero into *trans_num_ptr.
 *
 * Returns:		SUCCEED on success.
 *
 ******************************************************************************/

herr_t 
H5C2_jb__get_last_transaction_on_disk(H5C2_jbrb_t * struct_ptr,
				      unsigned long * trans_num_ptr)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5C2_jb__get_last_transaction_on_disk, FAIL)
	
    /* Check Arguments */
    HDassert( trans_num_ptr != NULL );

    /* This should really be an assert, but the func enter/exit 
     * macros get testy if there isn't at least one goto error 
     * macro call in the funtion.
     */
    if ( ( struct_ptr == NULL ) ||
         ( struct_ptr->magic != H5C2__H5C2_JBRB_T_MAGIC ) ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "bad struct_ptr.")
    }

    * trans_num_ptr = struct_ptr->last_trans_on_disk;

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5C2_jb__get_last_transaction_on_disk */


/******************************************************************************
 *
 * Function:		H5C2_jb__trunc
 *
 * Programmer:		Mike McGreevy <mcgreevy@hdfgroup.org>
 *			Thursday, February 7, 2008
 *
 * Purpose:		Verify that there is no transaction in progress, and 
 *			that the journal entry buffers are empty. Truncate
 *			the journal file, and reset the last transaction
 *			number to zero. Does not return until the file
 *			is truncated on disk.
 *
 * Returns:		SUCCEED on success.
 *
 ******************************************************************************/

herr_t 
H5C2_jb__trunc(H5C2_jbrb_t * struct_ptr)

{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5C2_jb__trunc, FAIL)

    /* Check Arguments */
    HDassert(struct_ptr);
    HDassert(struct_ptr->magic == H5C2__H5C2_JBRB_T_MAGIC);
	
    /* Verify that there is no transaction in progress */
    if ( struct_ptr->trans_in_prog != FALSE ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
	     "Attempt to truncate journal file while transaction in progress.")
    } /* end if */

    /* Verify that the journal buffers are empty */
    if ( struct_ptr->bufs_in_use != 0 ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
	            "Attempt to truncate with non-empty buffers.")
    } /* end if */	

    /* Truncate the journal file */
    if ( HDftruncate(struct_ptr->journal_file_fd, (off_t)0) < 0 ) {

        HGOTO_ERROR(H5E_IO, H5E_SYNCFAIL, FAIL, "Jounal file truncate failed.")
    } /* end if */

    /* Start back to top of journal buffer and journal file */
    struct_ptr->header_present = FALSE;
    struct_ptr->journal_is_empty = TRUE;

    if ( HDlseek(struct_ptr->journal_file_fd, (off_t)0, SEEK_SET) == (off_t)-1 )
    {
        HGOTO_ERROR(H5E_IO, H5E_SEEKERROR, FAIL, "Jounal file seek failed.")
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5C2_jb__trunc */


/******************************************************************************
 *
 * Function:		H5C2_jb__takedown
 *
 * Programmer:		Mike McGreevy <mcgreevy@hdfgroup.org>
 *			Thursday, February 7, 2008
 *
 * Purpose:		Verify that the journal buffers are empty, and that the
 *			journal file has been truncated. Then close and delete
 *			the journal file associated with *struct_ptr, and free
 *			all dynamically allocated memory associated with 
 *			*struct_ptr.
 *
 * Returns:		SUCCEED on success.
 *
 ******************************************************************************/

herr_t 
H5C2_jb__takedown(H5C2_jbrb_t * struct_ptr)

{
    herr_t ret_value = SUCCEED;
	
    FUNC_ENTER_NOAPI(H5C2_jb__takedown, FAIL)

    /* Check Arguments */
    HDassert(struct_ptr);
    HDassert(struct_ptr->magic == H5C2__H5C2_JBRB_T_MAGIC);
	
    /* Verify that the journal buffers are empty */
    if ( struct_ptr->bufs_in_use != 0 ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
	            "Attempt to takedown with non-empty buffers.")
    } /* end if */	

    /* Verify that the journal file has been truncated */
    if (struct_ptr->journal_is_empty != TRUE) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
	            "Attempt to takedown with journal file not truncated.")
    } /* end if */

    /* Close and delete the journal file associated with struct_ptr */
    if ( HDclose(struct_ptr->journal_file_fd) < 0 ) {

        HGOTO_ERROR(H5E_IO, H5E_CLOSEERROR, FAIL, "Jounal file close failed.")
    } /* end if */

    if (remove(struct_ptr->jname) < 0) {

        HGOTO_ERROR(H5E_IO, H5E_REMOVEFAIL, FAIL, "Jounal file close failed.")
    } /* end if */

    /* Free all memory associated with struct_ptr */

    if ( (*struct_ptr->buf)[0] != NULL ) {

        (*struct_ptr->buf)[0] = H5MM_xfree((*struct_ptr->buf)[0]);
        if ( (*struct_ptr->buf)[0] != NULL ) {

            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTFREE, FAIL, \
                        "free of buffers failed.");
        }
    }

    if ( struct_ptr->buf != NULL ) {

        struct_ptr->buf = H5MM_xfree(struct_ptr->buf);
        if ( struct_ptr->buf != NULL ) {

            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTFREE, FAIL, \
                        "free of buffer pointer array failed.");
        }
    }

    /* JRM: Shouldn't you free struct_ptr->trans_on_disk_record?? */

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5C2_jb__takedown */


/******************************************************************************
 *
 * Function:		H5C2_jb__reconfigure
 *
 * Programmer:		Mike McGreevy <mcgreevy@hdfgroup.org>
 *			Wednesday, February 6, 2008
 *
 * Purpose:		Re-configure the specified journal buffer ring buffer
 *			to use the supplied parameters.
 *
 * Returns:		SUCCEED on success.
 *
 ******************************************************************************/

herr_t 
H5C2_jb__reconfigure(H5C2_jbrb_t * struct_ptr,
	             size_t new_buf_size,
                     int new_num_bufs,
                     hbool_t new_use_aio)
{
#if 0 /* body commented out pending implementation */
    herr_t ret_value = SUCCEED;
	
    FUNC_ENTER_NOAPI(H5C2_jb__reconfigure, FAIL)

		/* code */
		/* code */
		/* code */

done:

    FUNC_LEAVE_NOAPI(ret_value)
#else
    return FAIL;
#endif
} /* end H5C2_jb__reconfigure */

