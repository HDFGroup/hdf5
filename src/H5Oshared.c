/*
 * Copyright (C) 1998 NCSA
 *		      All rights reserved.
 *
 * Programmer:	Robb Matzke <matzke@llnl.gov>
 *		Wednesday, April  1, 1998
 *
 * Purpose:	Functions that operate on a shared message.  The shared
 *		message doesn't ever actually appear in the object header as
 *		a normal message.  Instead, if a message is shared, the
 *		H5O_FLAG_SHARED bit is set and the message body is that
 *		defined here for H5O_SHARED.  The message ID is the ID of the
 *		pointed-to message and the pointed-to message is stored in
 *		the global heap.
 */
#include <H5private.h>
#include <H5Eprivate.h>
#include <H5MMprivate.h>
#include <H5Oprivate.h>

static void *H5O_shared_decode (H5F_t*, const uint8*, H5HG_t *hobj);
static herr_t H5O_shared_encode (H5F_t*, uint8*, const void*);
static size_t H5O_shared_size (H5F_t*, const void*);
static herr_t H5O_shared_debug (H5F_t*, const void*, FILE*, intn, intn);

/* This message derives from H5O */
const H5O_class_t H5O_SHARED[1] = {{
    H5O_SHARED_ID,	    	/*message id number			*/
    "shared",		    	/*message name for debugging		*/
    sizeof(H5O_shared_t), 	/*native message size			*/
    H5O_shared_decode,	    	/*decode method				*/
    H5O_shared_encode,	    	/*encode method				*/
    NULL,		    	/*no copy method			*/
    H5O_shared_size,	    	/*size method				*/
    NULL,		    	/*no reset method			*/
    NULL,			/*no share method			*/
    H5O_shared_debug,	    	/*debug method				*/
}};

/* Interface initialization */
#define PABLO_MASK	H5O_shared_mask
static hbool_t interface_initialize_g = FALSE;
#define INTERFACE_INIT  NULL


/*-------------------------------------------------------------------------
 * Function:	H5O_shared_decode
 *
 * Purpose:	Decodes a shared object message and returns it.
 *
 * Return:	Success:	Ptr to a new shared object message.
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *              Thursday, April  2, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void *
H5O_shared_decode (H5F_t *f, const uint8 *buf, H5HG_t *hobj)
{
    H5O_shared_t	*mesg;
    
    FUNC_ENTER (H5O_shared_decode, NULL);

    /* Check args */
    assert (f);
    assert (buf);
    assert (!hobj || !H5HG_defined (hobj));

    /* Decode */
    mesg = H5MM_xcalloc (1, sizeof *mesg);
    H5F_addr_decode (f, &buf, &(mesg->addr));
    INT32DECODE (buf, mesg->idx);

    FUNC_LEAVE (mesg);
}


/*-------------------------------------------------------------------------
 * Function:	H5O_shared_encode
 *
 * Purpose:	Encodes message _MESG into buffer BUF.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Thursday, April  2, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_shared_encode (H5F_t *f, uint8 *buf/*out*/, const void *_mesg)
{
    const H5O_shared_t	*mesg = (const H5O_shared_t *)_mesg;
    
    FUNC_ENTER (H5O_shared_encode, FAIL);

    /* Check args */
    assert (f);
    assert (buf);
    assert (mesg);

    /* Encode */
    H5F_addr_encode (f, &buf, &(mesg->addr));
    INT32ENCODE (buf, mesg->idx);

    FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5O_shared_size
 *
 * Purpose:	Returns the length of a shared object message.
 *
 * Return:	Success:	Length
 *
 *		Failure:	0
 *
 * Programmer:	Robb Matzke
 *              Thursday, April  2, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static size_t
H5O_shared_size (H5F_t *f, const void *_mesg)
{
    FUNC_ENTER (H5O_shared_size, 0);
    FUNC_LEAVE (H5F_SIZEOF_ADDR(f)+4);
}


/*-------------------------------------------------------------------------
 * Function:	H5O_shared_debug
 *
 * Purpose:	Prints debugging info for the message
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Thursday, April  2, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_shared_debug (H5F_t *f, const void *_mesg, FILE *stream, intn indent,
		  intn fwidth)
{
    const H5O_shared_t	*mesg = (const H5O_shared_t *)_mesg;

    FUNC_ENTER (H5O_shared_debug, FAIL);

    /* Check args */
    assert (f);
    assert (mesg);
    assert (stream);
    assert (indent>=0);
    assert (fwidth>=0);

    fprintf (stream, "%*s%-*s ", indent, "", fwidth,
	     "Collection address:");
    H5F_addr_print (stream, &(mesg->addr));
    fprintf (stream, "\n");

    fprintf (stream, "%*s%-*s %d\n", indent, "", fwidth,
	     "Object ID within collection:",
	     mesg->idx);

    FUNC_LEAVE (SUCCEED);
}
