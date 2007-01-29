/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have     *
 * access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu. *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/* Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Wednesday, September 30, 1998
 *
 * Purpose:	The fill message indicates a bit pattern to use for
 *		uninitialized data points of a dataset.
 */

#define H5O_PACKAGE		/*suppress error about including H5Opkg	  */

#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5FLprivate.h"	/* Free Lists				*/
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Opkg.h"             /* Object headers			*/
#include "H5Pprivate.h"		/* Property lists			*/


static void  *H5O_fill_decode(H5F_t *f, hid_t dxpl_id, unsigned mesg_flags, const uint8_t *p);
static herr_t H5O_fill_encode(H5F_t *f, uint8_t *p, const void *_mesg);
static size_t H5O_fill_size(const H5F_t *f, const void *_mesg);
static herr_t H5O_fill_debug(H5F_t *f, hid_t dxpl_id, const void *_mesg, FILE *stream,
			     int indent, int fwidth);
static void  *H5O_fill_new_decode(H5F_t *f, hid_t dxpl_id, unsigned mesg_flags, const uint8_t *p);
static herr_t H5O_fill_new_encode(H5F_t *f, uint8_t *p, const void *_mesg);
static void  *H5O_fill_new_copy(const void *_mesg, void *_dest);
static size_t H5O_fill_new_size(const H5F_t *f, const void *_mesg);
static herr_t H5O_fill_new_reset(void *_mesg);
static herr_t H5O_fill_new_free(void *_mesg);
static void  *H5O_fill_new_get_share(const void *_mesg, H5O_shared_t *sh);
static herr_t H5O_fill_new_set_share(void *_mesg, const H5O_shared_t *sh);
static htri_t H5O_fill_new_is_shared(const void *_mesg);
static herr_t H5O_fill_new_debug(H5F_t *f, hid_t dxpl_id, const void *_mesg, FILE *stream,
			     int indent, int fwidth);

/* Set up & include shared message "interface" info */
#define H5O_SHARED_TYPE			H5O_MSG_FILL
#define H5O_SHARED_DECODE		H5O_fill_shared_decode
#define H5O_SHARED_DECODE_REAL		H5O_fill_decode
#define H5O_SHARED_ENCODE		H5O_fill_shared_encode
#define H5O_SHARED_ENCODE_REAL		H5O_fill_encode
#define H5O_SHARED_SIZE			H5O_fill_shared_size
#define H5O_SHARED_SIZE_REAL		H5O_fill_size
#define H5O_SHARED_DELETE		H5O_fill_shared_delete
#undef H5O_SHARED_DELETE_REAL
#define H5O_SHARED_LINK			H5O_fill_shared_link
#undef H5O_SHARED_LINK_REAL
#define H5O_SHARED_COPY_FILE		H5O_fill_shared_copy_file
#undef H5O_SHARED_COPY_FILE_REAL
#include "H5Oshared.h"			/* Shared Object Header Message Callbacks */

/* Set up & include shared message "interface" info */
/* (Kludgy 'undef's in order to re-include the H5Oshared.h header) */
#undef H5O_SHARED_TYPE
#define H5O_SHARED_TYPE			H5O_MSG_FILL_NEW
#undef H5O_SHARED_DECODE
#define H5O_SHARED_DECODE		H5O_fill_new_shared_decode
#undef H5O_SHARED_DECODE_REAL
#define H5O_SHARED_DECODE_REAL		H5O_fill_new_decode
#undef H5O_SHARED_ENCODE
#define H5O_SHARED_ENCODE		H5O_fill_new_shared_encode
#undef H5O_SHARED_ENCODE_REAL
#define H5O_SHARED_ENCODE_REAL		H5O_fill_new_encode
#undef H5O_SHARED_SIZE
#define H5O_SHARED_SIZE			H5O_fill_new_shared_size
#undef H5O_SHARED_SIZE_REAL
#define H5O_SHARED_SIZE_REAL		H5O_fill_new_size
#undef H5O_SHARED_DELETE
#define H5O_SHARED_DELETE		H5O_fill_new_shared_delete
#undef H5O_SHARED_DELETE_REAL
#undef H5O_SHARED_LINK
#define H5O_SHARED_LINK			H5O_fill_new_shared_link
#undef H5O_SHARED_LINK_REAL
#undef H5O_SHARED_COPY_FILE
#define H5O_SHARED_COPY_FILE		H5O_fill_new_shared_copy_file
#undef H5O_SHARED_COPY_FILE_REAL
#undef H5Oshared_H
#include "H5Oshared.h"			/* Shared Object Header Message Callbacks */

/* This message derives from H5O message class, for old fill value before version 1.5 */
const H5O_msg_class_t H5O_MSG_FILL[1] = {{
    H5O_FILL_ID,                /*message id number                     */
    "fill",                     /*message name for debugging            */
    sizeof(H5O_fill_t),		/*native message size                   */
    H5O_fill_shared_decode,		/*decode message                        */
    H5O_fill_shared_encode,		/*encode message                        */
    H5O_fill_new_copy,          /*copy the native value                 */
    H5O_fill_shared_size,		/*raw message size			*/
    H5O_fill_new_reset,         /*free internal memory			*/
    H5O_fill_new_free,		/* free method				*/
    H5O_fill_shared_delete,	/* file delete method			*/
    H5O_fill_shared_link,	/* link method				*/
    H5O_fill_new_get_share,	/* get share method			*/
    H5O_fill_new_set_share,	/* set share method			*/
    NULL,		    	/*can share method		*/
    H5O_fill_new_is_shared,	/* is shared method			*/
    NULL,			/* pre copy native value to file	*/
    H5O_fill_shared_copy_file,	/* copy native value to file		*/
    NULL,			/* post copy native value to file	*/
    NULL,			/* get creation index		*/
    NULL,			/* set creation index		*/
    H5O_fill_debug              /*debug the message			*/
}};

/* This message derives from H5O message class, for new fill value after version 1.4  */
const H5O_msg_class_t H5O_MSG_FILL_NEW[1] = {{
    H5O_FILL_NEW_ID,		/*message id number			*/
    "fill_new", 		/*message name for debugging		*/
    sizeof(H5O_fill_t),		/*native message size			*/
    H5O_fill_new_shared_decode,	/*decode message			*/
    H5O_fill_new_shared_encode,	/*encode message			*/
    H5O_fill_new_copy,		/*copy the native value			*/
    H5O_fill_new_shared_size,	/*raw message size			*/
    H5O_fill_new_reset,		/*free internal memory			*/
    H5O_fill_new_free,		/* free method				*/
    H5O_fill_new_shared_delete,	/* file delete method			*/
    H5O_fill_new_shared_link,	/* link method				*/
    H5O_fill_new_get_share,	/* get share method			*/
    H5O_fill_new_set_share,	/* set share method			*/
    NULL,		    	/*can share method		*/
    H5O_fill_new_is_shared,	/* is shared method			*/
    NULL,			/* pre copy native value to file	*/
    H5O_fill_new_shared_copy_file, /* copy native value to file		*/
    NULL,			/* post copy native value to file	*/
    NULL,			/* get creation index		*/
    NULL,			/* set creation index		*/
    H5O_fill_new_debug		/*debug the message			*/
}};

/* Initial version of the "old" fill value information */
#define H5O_FILL_VERSION 	1
/* Revised version of the "new" fill value information */
#define H5O_FILL_VERSION_2 	2

/* Declare a free list to manage the H5O_fill_t struct */
H5FL_DEFINE(H5O_fill_t);


/*-------------------------------------------------------------------------
 * Function:	H5O_fill_new_decode
 *
 * Purpose:	Decode a new fill value message.  The new fill value
 * 		message is fill value plus space allocation time and
 * 		fill value writing time and whether fill value is defined.
 *
 * Return:	Success:	Ptr to new message in native struct.
 *
 *		Failure:	NULL
 *
 * Programmer:  Raymond Lu
 *              Feb 26, 2002
 *
 *-------------------------------------------------------------------------
 */
static void *
H5O_fill_new_decode(H5F_t UNUSED *f, hid_t UNUSED dxpl_id, unsigned UNUSED mesg_flags,
    const uint8_t *p)
{
    H5O_fill_t	*mesg = NULL;
    int		version;
    void	*ret_value;

    FUNC_ENTER_NOAPI_NOINIT(H5O_fill_new_decode)

    HDassert(f);
    HDassert(p);

    if(NULL == (mesg = H5FL_CALLOC(H5O_fill_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed for fill value message")

    /* Version */
    version = *p++;
    if(version != H5O_FILL_VERSION && version !=H5O_FILL_VERSION_2)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, NULL, "bad version number for fill value message")

    /* Space allocation time */
    mesg->alloc_time = (H5D_alloc_time_t)*p++;

    /* Fill value write time */
    mesg->fill_time = (H5D_fill_time_t)*p++;

    /* Whether fill value is defined */
    mesg->fill_defined = *p++;

    /* Only decode fill value information if one is defined */
    if(mesg->fill_defined) {
        INT32DECODE(p, mesg->size);
        if(mesg->size > 0) {
            H5_CHECK_OVERFLOW(mesg->size,ssize_t,size_t);
            if(NULL == (mesg->buf = H5MM_malloc((size_t)mesg->size)))
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed for fill value")
            HDmemcpy(mesg->buf, p, (size_t)mesg->size);
        } /* end if */
    } /* end if */
    else
        mesg->size = (-1);

    /* Set return value */
    ret_value = (void*)mesg;

done:
    if(!ret_value && mesg) {
        if(mesg->buf)
            H5MM_xfree(mesg->buf);
	H5FL_FREE(H5O_fill_t, mesg);
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_fill_new_decode() */


/*-------------------------------------------------------------------------
 * Function:    H5O_fill_decode
 *
 * Purpose:     Decode a fill value message.
 *
 * Return:      Success:        Ptr to new message in native struct.
 *
 *              Failure:        NULL
 *
 * Programmer:  Robb Matzke
 *              Wednesday, September 30, 1998
 *
 *-------------------------------------------------------------------------
 */
static void *
H5O_fill_decode(H5F_t UNUSED *f, hid_t UNUSED dxpl_id, unsigned UNUSED mesg_flags,
    const uint8_t *p)
{
    H5O_fill_t *mesg = NULL;		/* Decoded fill value message */
    void *ret_value;                    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_fill_decode)

    HDassert(f);
    HDassert(p);

    if(NULL == (mesg = H5FL_CALLOC(H5O_fill_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed for fill value message")

    /* Set non-zero default fields */
    mesg->alloc_time = H5D_ALLOC_TIME_LATE;
    mesg->fill_time = H5D_FILL_TIME_IFSET;

    /* Fill value size */
    UINT32DECODE(p, mesg->size);

    /* Only decode the fill value itself if there is one */
    if(mesg->size > 0) {
        if(NULL == (mesg->buf = H5MM_malloc((size_t)mesg->size)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed for fill value")
        HDmemcpy(mesg->buf, p, (size_t)mesg->size);
        mesg->fill_defined = TRUE;
    } /* end if */
    else
        mesg->size = (-1);

    /* Set return value */
    ret_value = (void*)mesg;

done:
    if(!ret_value && mesg) {
        if(mesg->buf)
            H5MM_xfree(mesg->buf);
	H5FL_FREE(H5O_fill_t, mesg);
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_fill_decode() */


/*-------------------------------------------------------------------------
 * Function:	H5O_fill_new_encode
 *
 * Purpose:	Encode a new fill value message.  The new fill value
 *              message is fill value plus space allocation time and
 *              fill value writing time and whether fill value is defined.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:  Raymond Lu
 *              Feb 26, 2002
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_fill_new_encode(H5F_t UNUSED *f, uint8_t *p, const void *_mesg)
{
    const H5O_fill_t	*mesg = (const H5O_fill_t *)_mesg;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_fill_new_encode)

    HDassert(f);
    HDassert(p);
    HDassert(mesg && NULL == mesg->type);

    /* Version */
    *p++ = H5O_FILL_VERSION_2;

    /* Space allocation time */
    *p++ = mesg->alloc_time;

    /* Fill value writing time */
    *p++ = mesg->fill_time;

    /* Whether fill value is defined */
    *p++ = mesg->fill_defined;

    /* Only write out the size and fill value if it is defined */
    if(mesg->fill_defined) {
        INT32ENCODE(p, mesg->size);
        if(mesg->size > 0)
            if(mesg->buf) {
                H5_CHECK_OVERFLOW(mesg->size, ssize_t, size_t);
                HDmemcpy(p, mesg->buf, (size_t)mesg->size);
            } /* end if */
    } /* end if */

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5O_fill_new_encode() */


/*-------------------------------------------------------------------------
 * Function:    H5O_fill_encode
 *
 * Purpose:     Encode a fill value message.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Robb Matzke
 *              Thursday, October  1, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_fill_encode(H5F_t UNUSED *f, uint8_t *p, const void *_mesg)
{
    const H5O_fill_t *mesg = (const H5O_fill_t *)_mesg;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_fill_encode)

    HDassert(f);
    HDassert(p);
    HDassert(mesg && NULL == mesg->type);

    UINT32ENCODE(p, mesg->size);
    if(mesg->buf)
        HDmemcpy(p, mesg->buf, (size_t)mesg->size);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5O_fill_encode() */


/*-------------------------------------------------------------------------
 * Function:	H5O_fill_new_copy
 *
 * Purpose:	Copies a message from _MESG to _DEST, allocating _DEST if
 *		necessary.  The new fill value message is fill value plus
 *		space allocation time and fill value writing time and
 *		whether fill value is defined.
 *
 * Return:	Success:	Ptr to _DEST
 *
 *		Failure:	NULL
 *
 * Programmer:  Raymond Lu
 *              Feb 26, 2002
 *
 *-------------------------------------------------------------------------
 */
static void *
H5O_fill_new_copy(const void *_mesg, void *_dest)
{
    const H5O_fill_t	*mesg = (const H5O_fill_t *)_mesg;
    H5O_fill_t		*dest = (H5O_fill_t *)_dest;
    void		*ret_value;

    FUNC_ENTER_NOAPI_NOINIT(H5O_fill_new_copy)

    HDassert(mesg);

    if(!dest && NULL == (dest = H5FL_MALLOC(H5O_fill_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed for fill message")

    /* Shallow copy basic fields */
    *dest = *mesg;

    /* Copy data type of fill value */
    if(mesg->type) {
        if(NULL == (dest->type = H5T_copy(mesg->type, H5T_COPY_TRANSIENT)))
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, NULL, "unable to copy fill value data type")
    } /* end if */
    else
        dest->type = NULL;

    /* Copy fill value and its size */
    if(mesg->buf) {
        H5_CHECK_OVERFLOW(mesg->size, ssize_t, size_t);
	if(NULL == (dest->buf = H5MM_malloc((size_t)mesg->size)))
	    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed for fill value")
	HDmemcpy(dest->buf, mesg->buf, (size_t)mesg->size);
    } /* end if */
    else
        dest->buf = NULL;

    /* Set return value */
    ret_value = dest;

done:
    if(!ret_value && dest) {
        if(dest->buf)
            H5MM_xfree(dest->buf);
	if(dest->type)
            H5T_close(dest->type);
	if(!_dest)
            H5FL_FREE(H5O_fill_t, dest);
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_fill_new_copy() */


/*-------------------------------------------------------------------------
 * Function:	H5O_fill_new_size
 *
 * Purpose:	Returns the size of the raw message in bytes not counting the
 *		message type or size fields, but only the data fields.  This
 *		function doesn't take into account alignment.  The new fill
 *		value message is fill value plus space allocation time and
 *              fill value writing time and whether fill value is defined.
 *
 * Return:	Success:	Message data size in bytes w/o alignment.
 *
 *		Failure:	0
 *
 * Programmer:	Robb Matzke
 *              Thursday, October  1, 1998
 *
 *-------------------------------------------------------------------------
 */
static size_t
H5O_fill_new_size(const H5F_t UNUSED *f, const void *_mesg)
{
    const H5O_fill_t	*mesg = (const H5O_fill_t *)_mesg;
    size_t			ret_value;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_fill_new_size)

    HDassert(f);
    HDassert(mesg);

    ret_value = 1 + 		/* Version number        */
		1 + 		/* Space allocation time */
		1 + 		/* Fill value write time */
		1; 		/* Fill value defined    */
    if(mesg->fill_defined)
        ret_value += 4 +	/* Fill value size	 */
		(mesg->size > 0 ? mesg->size : 0);	/* Size of fill value	 */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_fill_new_size() */


/*-------------------------------------------------------------------------
 * Function:    H5O_fill_size
 *
 * Purpose:     Returns the size of the raw message in bytes not counting the
 *              message type or size fields, but only the data fields.  This
 *              function doesn't take into account alignment.
 *
 * Return:      Success:        Message data size in bytes w/o alignment.
 *
 *              Failure:        0
 *
 * Programmer:  Robb Matzke
 *              Thursday, October  1, 1998
 *
 *-------------------------------------------------------------------------
 */
static size_t
H5O_fill_size(const H5F_t UNUSED *f, const void *_mesg)
{
    const H5O_fill_t *mesg = (const H5O_fill_t *)_mesg;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_fill_size)

    HDassert(mesg);

    FUNC_LEAVE_NOAPI(4 + mesg->size)
} /* end H5O_fill_size() */


/*-------------------------------------------------------------------------
 * Function:	H5O_fill_reset_dyn
 *
 * Purpose:	Resets dynamic fill value fields
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Monday, January 22, 2007
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_fill_reset_dyn(H5O_fill_t *fill)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_fill_reset_dyn)

    HDassert(fill);

    if(fill->buf)
        fill->buf = H5MM_xfree(fill->buf);
    fill->size = 0;
    if(fill->type) {
	H5T_close(fill->type);
	fill->type = NULL;
    } /* end if */

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5O_fill_reset_dyn() */


/*-------------------------------------------------------------------------
 * Function:	H5O_fill_new_reset
 *
 * Purpose:	Resets a new message to an initial state.  The new fill value
 *              message is fill value plus space allocation time and
 *              fill value writing time and whether fill value is defined.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Thursday, October  1, 1998
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_fill_new_reset(void *_mesg)
{
    H5O_fill_t	*mesg = (H5O_fill_t *)_mesg;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_fill_new_reset)

    HDassert(mesg);

    /* Reset dynamic fields */
    H5O_fill_reset_dyn(mesg);

    /* Reset value fields */
    mesg->alloc_time   = H5D_ALLOC_TIME_LATE;
    mesg->fill_time    = H5D_FILL_TIME_IFSET;
    mesg->fill_defined = FALSE;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5O_fill_new_reset() */


/*-------------------------------------------------------------------------
 * Function:	H5O_fill_new_free
 *
 * Purpose:	Frees the message
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Thursday, December 5, 2002
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_fill_new_free(void *mesg)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_fill_new_free)

    HDassert(mesg);

    H5FL_FREE(H5O_fill_t, mesg);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5O_fill_new_free() */


/*-------------------------------------------------------------------------
 * Function:	H5O_fill_new_get_share
 *
 * Purpose:	Gets sharing information from the message
 *
 * Return:	Shared message on success/NULL on failure
 *
 * Programmer:	James Laird
 *              Tuesday, October 10, 2006
 *
 *-------------------------------------------------------------------------
 */
static void *
H5O_fill_new_get_share(const void *_mesg, H5O_shared_t *sh /*out*/)
{
    const H5O_fill_t  *mesg = (const H5O_fill_t *)_mesg;
    void       *ret_value;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_fill_new_get_share)

    HDassert(mesg);

    ret_value = H5O_msg_copy(H5O_SHARED_ID, &(mesg->sh_loc), sh);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_fill_new_get_share() */


/*-------------------------------------------------------------------------
 * Function:	H5O_fill_new_set_share
 *
 * Purpose:	Sets sharing information for the message
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	James Laird
 *              Tuesday, October 10, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_fill_new_set_share(void *_mesg/*in,out*/, const H5O_shared_t *sh)
{
    H5O_fill_t  *mesg = (H5O_fill_t *)_mesg;
    herr_t       ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_fill_new_set_share)

    HDassert(mesg);
    HDassert(sh);

    if(NULL == H5O_msg_copy(H5O_SHARED_ID, sh, &(mesg->sh_loc)))
        ret_value = FAIL;

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_fill_new_set_share() */


/*-------------------------------------------------------------------------
 * Function:	H5O_fill_new_is_shared
 *
 * Purpose:	Determines if this fill value is shared (committed or a SOHM)
 *              or not.
 *
 * Return:	TRUE if fill value is shared
 *              FALSE if fill value is not shared
 *              Negative on failure
 *
 * Programmer:	James Laird
 *		Monday, October 16, 2006
 *
 *-------------------------------------------------------------------------
 */
static htri_t
H5O_fill_new_is_shared(const void *_mesg)
{
    const H5O_fill_t  *mesg = (const H5O_fill_t *)_mesg;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_fill_new_is_shared)

    HDassert(mesg);

    /* Fill values can't currently be committed, but this should let the
     * library read a "committed fill value" if we ever create one in
     * the future.
     */
    FUNC_LEAVE_NOAPI(H5O_IS_SHARED(mesg->sh_loc.flags))
} /* end H5O_fill_new_is_shared() */


/*-------------------------------------------------------------------------
 * Function:	H5O_fill_new_debug
 *
 * Purpose:	Prints debugging info for the message.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Thursday, October  1, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_fill_new_debug(H5F_t UNUSED *f, hid_t UNUSED dxpl_id, const void *_mesg, FILE *stream,
	       int indent, int fwidth)
{
    const H5O_fill_t	*mesg = (const H5O_fill_t *)_mesg;
    H5D_fill_value_t fill_status;       /* Whether the fill value is defined */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_fill_new_debug)

    HDassert(f);
    HDassert(mesg);
    HDassert(stream);
    HDassert(indent>=0);
    HDassert(fwidth>=0);

    HDfprintf(stream, "%*s%-*s %u\n", indent, "", fwidth,
	      "Version:", (unsigned)H5O_FILL_VERSION);
    fprintf(stream, "%*s%-*s ", indent, "", fwidth, "Space Allocation Time:");
    switch(mesg->alloc_time) {
        case H5D_ALLOC_TIME_EARLY:
            fprintf(stream,"Early\n");
            break;

        case H5D_ALLOC_TIME_LATE:
            fprintf(stream,"Late\n");
            break;

        case H5D_ALLOC_TIME_INCR:
            fprintf(stream,"Incremental\n");
            break;

        default:
            fprintf(stream,"Unknown!\n");
            break;

    } /* end switch */
    fprintf(stream, "%*s%-*s ", indent, "", fwidth, "Fill Time:");
    switch(mesg->fill_time) {
        case H5D_FILL_TIME_ALLOC:
            fprintf(stream,"On Allocation\n");
            break;

        case H5D_FILL_TIME_NEVER:
            fprintf(stream,"Never\n");
            break;

        case H5D_FILL_TIME_IFSET:
            fprintf(stream,"If Set\n");
            break;

        default:
            fprintf(stream,"Unknown!\n");
            break;

    } /* end switch */
    fprintf(stream, "%*s%-*s ", indent, "", fwidth, "Fill Value Defined:");
    H5P_is_fill_value_defined((const H5O_fill_t *)mesg, &fill_status);
    switch(fill_status) {
        case H5D_FILL_VALUE_UNDEFINED:
            fprintf(stream,"Undefined\n");
            break;

        case H5D_FILL_VALUE_DEFAULT:
            fprintf(stream,"Default\n");
            break;

        case H5D_FILL_VALUE_USER_DEFINED:
            fprintf(stream,"User Defined\n");
            break;

        default:
            fprintf(stream,"Unknown!\n");
            break;

    } /* end switch */
    HDfprintf(stream, "%*s%-*s %Zd\n", indent, "", fwidth,
	      "Size:", mesg->size);
    fprintf(stream, "%*s%-*s ", indent, "", fwidth, "Data type:");
    if(mesg->type) {
	H5T_debug(mesg->type, stream);
	fprintf(stream, "\n");
    } /* end if */
    else
	fprintf(stream, "<dataset type>\n");

    FUNC_LEAVE_NOAPI(SUCCEED);
} /* end H5O_fill_new_debug() */


/*-------------------------------------------------------------------------
 * Function:	H5O_fill_debug
 *
 * Purpose:	Prints debugging info for the message.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Thursday, October  1, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_fill_debug(H5F_t UNUSED *f, hid_t UNUSED dxpl_id, const void *_mesg, FILE *stream,
    int indent, int fwidth)
{
    const H5O_fill_t *mesg = (const H5O_fill_t *)_mesg;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_fill_debug)

    HDassert(f);
    HDassert(mesg);
    HDassert(stream);
    HDassert(indent >= 0);
    HDassert(fwidth >= 0);

    HDfprintf(stream, "%*s%-*s %Zd\n", indent, "", fwidth,
	      "Bytes:", mesg->size);
    fprintf(stream, "%*s%-*s ", indent, "", fwidth, "Data type:");
    if(mesg->type) {
	H5T_debug(mesg->type, stream);
	fprintf(stream, "\n");
    } /* end if */
    else
	fprintf(stream, "<dataset type>\n");

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5O_fill_debug() */


/*-------------------------------------------------------------------------
 * Function:	H5O_fill_convert
 *
 * Purpose:	Convert a fill value from whatever data type it currently has
 *		to the specified dataset type.  The `type' field of the fill
 *		value struct will be set to NULL to indicate that it has the
 *		same type as the dataset.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Thursday, October  1, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_fill_convert(H5O_fill_t *fill, H5T_t *dset_type, hbool_t *fill_changed, hid_t dxpl_id)
{
    H5T_path_t		*tpath;			    /* Type conversion info	*/
    void		*buf = NULL, *bkg = NULL;   /* Conversion buffers	*/
    hid_t		src_id = -1, dst_id = -1;   /* Datatype identifiers	*/
    herr_t      	ret_value = SUCCEED;        /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_fill_convert)

    HDassert(fill);
    HDassert(dset_type);
    HDassert(fill_changed);

    /* No-op cases */
    if(!fill->buf || !fill->type || 0 == H5T_cmp(fill->type, dset_type, FALSE)) {
        /* Don't need datatype for fill value */
	if(fill->type)
            H5T_close(fill->type);
	fill->type = NULL;

        /* Note that the fill value info has changed */
        *fill_changed = TRUE;

	HGOTO_DONE(SUCCEED);
    } /* end if */

    /*
     * Can we convert between source and destination data types?
     */
    if(NULL == (tpath = H5T_path_find(fill->type, dset_type, NULL, NULL, dxpl_id, FALSE)))
	HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "unable to convert between src and dst datatypes")

    /* Don't bother doing anything if there will be no actual conversion */
    if(!H5T_path_noop(tpath)) {
        if((src_id = H5I_register(H5I_DATATYPE, H5T_copy(fill->type, H5T_COPY_ALL))) < 0 ||
                (dst_id = H5I_register(H5I_DATATYPE, H5T_copy(dset_type, H5T_COPY_ALL))) < 0)
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "unable to copy/register data type")

        /*
         * Datatype conversions are always done in place, so we need a buffer
         * that is large enough for both source and destination.
         */
        if(H5T_get_size(fill->type) >= H5T_get_size(dset_type))
            buf = fill->buf;
        else {
            if(NULL == (buf = H5MM_malloc(H5T_get_size(dset_type))))
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for type conversion")
            HDmemcpy(buf, fill->buf, H5T_get_size(fill->type));
        } /* end else */
        if(H5T_path_bkg(tpath) && NULL == (bkg = H5MM_malloc(H5T_get_size(dset_type))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for type conversion")

        /* Do the conversion */
        if(H5T_convert(tpath, src_id, dst_id, (size_t)1, (size_t)0, (size_t)0, buf, bkg, dxpl_id) < 0)
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "datatype conversion failed")

        /* Update the fill message */
        if(buf != fill->buf) {
            H5MM_xfree(fill->buf);
            fill->buf = buf;
        } /* end if */
        H5T_close(fill->type);
        fill->type = NULL;
        H5_ASSIGN_OVERFLOW(fill->size, H5T_get_size(dset_type), size_t, ssize_t);

        /* Note that the fill value info has changed */
        *fill_changed = TRUE;
    } /* end if */

done:
    if(src_id >= 0)
        H5I_dec_ref(src_id);
    if(dst_id >= 0)
        H5I_dec_ref(dst_id);
    if(buf != fill->buf)
        H5MM_xfree(buf);
    if(bkg)
        H5MM_xfree(bkg);

    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5O_fill_convert() */

