/*
 * Copyright (C) 1998 NCSA
 *                    All rights reserved.
 *
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Wednesday, September 30, 1998
 *
 * Purpose:	The fill message indicates a bit pattern to use for
 *		uninitialized data points of a dataset.
 */
#include <H5private.h>
#include <H5Eprivate.h>
#include <H5MMprivate.h>
#include <H5Oprivate.h>

#define PABLO_MASK	H5O_fill_mask

static void *H5O_fill_decode(H5F_t *f, const uint8 *p, H5O_shared_t *sh);
static herr_t H5O_fill_encode(H5F_t *f, uint8 *p, const void *_mesg);
static void *H5O_fill_copy(const void *_mesg, void *_dest);
static size_t H5O_fill_size(H5F_t *f, const void *_mesg);
static herr_t H5O_fill_reset(void *_mesg);
static herr_t H5O_fill_debug(H5F_t *f, const void *_mesg, FILE *stream,
			     intn indent, intn fwidth);

/* This message derives from H5O */
const H5O_class_t H5O_FILL[1] = {{
    H5O_FILL_ID,		/*message id number			*/
    "fill", 			/*message name for debugging		*/
    sizeof(H5O_fill_t),		/*native message size			*/
    H5O_fill_decode,		/*decode message			*/
    H5O_fill_encode,		/*encode message			*/
    H5O_fill_copy,		/*copy the native value			*/
    H5O_fill_size,		/*raw message size			*/
    H5O_fill_reset,		/*free internal memory			*/
    NULL,			/*get share method			*/
    NULL,			/*set share method			*/
    H5O_fill_debug,		/*debug the message			*/
}};

/* Interface initialization */
static hbool_t interface_initialize_g = FALSE;
#define INTERFACE_INIT NULL


/*-------------------------------------------------------------------------
 * Function:	H5O_fill_decode
 *
 * Purpose:	Decode a fill value message.
 *
 * Return:	Success:	Ptr to new message in native struct.
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *              Wednesday, September 30, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void *
H5O_fill_decode(H5F_t *f, const uint8 *p, H5O_shared_t *sh)
{
    H5O_fill_t	*mesg=NULL;
    void	*ret_value = NULL;
    
    FUNC_ENTER(H5O_fill_decode, NULL);
    assert(f);
    assert(p);
    assert(!sh);

    if (NULL==(mesg=H5MM_calloc(sizeof(H5O_fill_t)))) {
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL,
		    "memory allocation failed for fill value message");
    }
    UINT32DECODE(p, mesg->size);
    if (mesg->size>0) {
	if (NULL==(mesg->buf=H5MM_malloc(mesg->size))) {
	    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL,
			"memory allocation failed for fill value");
	}
	HDmemcpy(mesg->buf, p, mesg->size);
    }
    
    ret_value = (void*)mesg;
    
 done:
    if (!ret_value && mesg) {
	H5MM_xfree(mesg->buf);
	H5MM_xfree(mesg);
    }
    FUNC_LEAVE(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5O_fill_encode
 *
 * Purpose:	Encode a fill value message.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Thursday, October  1, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_fill_encode(H5F_t *f, uint8 *p, const void *_mesg)
{
    const H5O_fill_t	*mesg = (const H5O_fill_t *)_mesg;
    
    FUNC_ENTER(H5O_fill_encode, FAIL);
    assert(f);
    assert(p);
    assert(mesg && NULL==mesg->type);

    UINT32ENCODE(p, mesg->size);
    HDmemcpy(p, mesg->buf, mesg->size);

    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5O_fill_copy
 *
 * Purpose:	Copies a message from _MESG to _DEST, allocating _DEST if
 *		necessary.
 *
 * Return:	Success:	Ptr to _DEST
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *              Thursday, October  1, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void *
H5O_fill_copy(const void *_mesg, void *_dest)
{
    const H5O_fill_t	*mesg = (const H5O_fill_t *)_mesg;
    H5O_fill_t		*dest = (H5O_fill_t *)_dest;
    void		*ret_value = NULL;

    FUNC_ENTER(H5O_fill_copy, NULL);
    assert(mesg);

    if (!dest && NULL==(dest=H5MM_calloc(sizeof(H5O_fill_t)))) {
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL,
		    "memory allocation failed for fill message");
    }
    if (mesg->type &&
	NULL==(dest->type=H5T_copy(mesg->type, H5T_COPY_TRANSIENT))) {
	HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, NULL,
		    "unable to copy fill value data type");
    }
    if (mesg->buf) {
	if (NULL==(dest->buf=H5MM_malloc(mesg->size))) {
	    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL,
			"memory allocation failed for fill value");
	}
	dest->size = mesg->size;
	HDmemcpy(dest->buf, mesg->buf, mesg->size);
    }
    ret_value = dest;

 done:
    if (!ret_value && dest) {
	H5MM_xfree(dest->buf);
	if (dest->type) H5T_close(dest->type);
	if (!_dest) H5MM_xfree(dest);
    }
    FUNC_LEAVE(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5O_fill_size
 *
 * Purpose:	Returns the size of the raw message in bytes not counting the
 *		message type or size fields, but only the data fields.  This
 *		function doesn't take into account alignment.
 *
 * Return:	Success:	Message data size in bytes w/o alignment.
 *
 *		Failure:	0
 *
 * Programmer:	Robb Matzke
 *              Thursday, October  1, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static size_t
H5O_fill_size(H5F_t *f, const void *_mesg)
{
    const H5O_fill_t	*mesg = (const H5O_fill_t *)_mesg;
    
    FUNC_ENTER(H5O_fill_size, 0);
    assert(f);
    assert(mesg);

    FUNC_LEAVE(4+mesg->size);
}


/*-------------------------------------------------------------------------
 * Function:	H5O_fill_reset
 *
 * Purpose:	Resets a message to an initial state
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Thursday, October  1, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_fill_reset(void *_mesg)
{
    H5O_fill_t	*mesg = (H5O_fill_t *)_mesg;

    FUNC_ENTER(H5O_fill_reset, FAIL);
    assert(mesg);
    
    mesg->buf = H5MM_xfree(mesg->buf);
    mesg->size = 0;
    if (mesg->type) {
	H5T_close(mesg->type);
	mesg->type = NULL;
    }
    
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5O_fill_debug
 *
 * Purpose:	Prints debugging info for the message.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Thursday, October  1, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_fill_debug(H5F_t *f, const void *_mesg, FILE *stream, intn indent,
	       intn fwidth)
{
    const H5O_fill_t	*mesg = (const H5O_fill_t *)_mesg;
    
    FUNC_ENTER(H5O_fill_debug, FAIL);
    assert(f);
    assert(mesg);
    assert(stream);
    assert(indent>=0);
    assert(fwidth>=0);

    HDfprintf(stream, "%*s%-*s %Zu\n", indent, "", fwidth,
	      "Bytes:", mesg->size);
    fprintf(stream, "%*s%-*s ", indent, "", fwidth, "Data type:");
    if (mesg->type) {
	H5T_debug(mesg->type, stream);
	fprintf(stream, "\n");
    } else {
	fprintf(stream, "<dataset type>\n");
    }
    
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5O_fill_convert
 *
 * Purpose:	Convert a fill value from whatever data type it currently has
 *		to the specified data type.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Thursday, October  1, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_fill_convert(H5O_fill_t *fill, H5T_t *dset_type)
{
    FUNC_ENTER(H5O_fill_convert, FAIL);
    assert(fill);
    assert(dset_type);

    /* No-op cases */
    if (!fill->buf || !fill->type || 0==H5T_cmp(fill->type, dset_type)) {
	if (fill->type) H5T_close(fill->type);
	fill->type = NULL;
	HRETURN(SUCCEED);
    }
    
    
    HRETURN_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL,
		  "fill value conversion not supported yet");

    FUNC_LEAVE(SUCCEED);
}
