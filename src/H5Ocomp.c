/*
 * Copyright (C) 1998 NCSA
 *		      All rights reserved.
 *
 * Programmer:	Robb Matzke <matzke@llnl.gov>
 *		Wednesday, April 15, 1998
 *
 * Purpose:	Data compression message.
 */
#include <H5private.h>
#include <H5Eprivate.h>
#include <H5MMprivate.h>
#include <H5Oprivate.h>

#define PABLO_MASK	H5O_comp_mask

/* PRIVATE PROTOTYPES */
static herr_t H5O_comp_encode (H5F_t *f, uint8 *p, const void *mesg);
static void *H5O_comp_decode (H5F_t *f, const uint8 *p, H5O_shared_t *sh);
static void *H5O_comp_copy (const void *_mesg, void *_dest);
static size_t H5O_comp_size (H5F_t *f, const void *_mesg);
static herr_t H5O_comp_reset (void *_mesg);
static herr_t H5O_comp_debug (H5F_t *f, const void *_mesg,
			      FILE * stream, intn indent, intn fwidth);

/* This message derives from H5O */
const H5O_class_t H5O_COMPRESS[1] = {{
    H5O_COMPRESS_ID,		/* message id number		*/
    "compression",		/* message name for debugging	*/
    sizeof(H5O_compress_t),	/* native message size		*/
    H5O_comp_decode,		/* decode message		*/
    H5O_comp_encode,		/* encode message		*/
    H5O_comp_copy,		/* copy the native value	*/
    H5O_comp_size,		/* size of raw message		*/
    H5O_comp_reset,		/* reset method			*/
    NULL,			/* get share method		*/
    NULL, 			/* set share method		*/
    H5O_comp_debug,		/* debug the message		*/
}};

/* Interface initialization */
static hbool_t		interface_initialize_g = FALSE;
#define INTERFACE_INIT	NULL


/*-------------------------------------------------------------------------
 * Function:	H5O_comp_decode
 *
 * Purpose:	Decodes a compression message.
 *
 * Return:	Success:	Ptr to the native message.
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *              Wednesday, April 15, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void *
H5O_comp_decode(H5F_t __unused__ *f, const uint8 *p,
		H5O_shared_t __unused__ *sh)
{
    H5O_compress_t	*comp = NULL;

    FUNC_ENTER(H5O_comp_decode, NULL);

    /* check args */
    assert(p);

    /* Decode */
    comp = H5MM_xcalloc(1, sizeof *comp);
    comp->method = *p++;
    comp->flags = *p++;
    UINT16DECODE (p, comp->cd_size);

    if (comp->cd_size>0) {
	comp->client_data = H5MM_xmalloc (comp->cd_size);
	HDmemcpy (comp->client_data, p, comp->cd_size);
    }
    
    FUNC_LEAVE(comp);
}


/*-------------------------------------------------------------------------
 * Function:	H5O_comp_encode
 *
 * Purpose:	Encodes message MESG into buffer P.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Wednesday, April 15, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_comp_encode (H5F_t __unused__ *f, uint8 *p/*out*/, const void *mesg)
{
    const H5O_compress_t	*comp = (const H5O_compress_t*)mesg;
    
    FUNC_ENTER (H5O_comp_encode, FAIL);

    /* Check args */
    assert (p);
    assert (mesg);

    *p++ = comp->method;
    *p++ = comp->flags;
    UINT16ENCODE (p, comp->cd_size);
    if (comp->cd_size) {
	HDmemcpy (p, comp->client_data, comp->cd_size);
    }

    FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5O_comp_copy
 *
 * Purpose:	Copies a compression message from SRC to DST allocating DST
 *		if necessary.  If DST is already allocated then we assume
 *		that it isn't initialized.
 *
 * Return:	Success:	Ptr to DST or allocated result.
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *              Wednesday, April 15, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void *
H5O_comp_copy (const void *_src, void *_dst/*out*/)
{
    const H5O_compress_t	*src = (const H5O_compress_t *)_src;
    H5O_compress_t		*dst = (H5O_compress_t *)_dst;
    
    FUNC_ENTER (H5O_comp_copy, NULL);

    if (!dst) dst = H5MM_xmalloc (sizeof *dst);
    *dst = *src;
    if (src->cd_size>0) {
	dst->client_data = H5MM_xmalloc (src->cd_size);
	HDmemcpy (dst->client_data, src->client_data, src->cd_size);
    }

    FUNC_LEAVE (dst);
}


/*-------------------------------------------------------------------------
 * Function:	H5O_comp_size
 *
 * Purpose:	Determines the size of a raw compression message.
 *
 * Return:	Success:	Size of message.
 *
 *		Failure:	zero
 *
 * Programmer:	Robb Matzke
 *              Wednesday, April 15, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static size_t
H5O_comp_size (H5F_t __unused__ *f, const void *mesg)
{
    const H5O_compress_t	*comp = (const H5O_compress_t*)mesg;
    
    FUNC_ENTER (H5O_comp_size, 0);
    FUNC_LEAVE (4+comp->cd_size);
}


/*-------------------------------------------------------------------------
 * Function:	H5O_comp_reset
 *
 * Purpose:	Resets a compression message by freeing the client data and
 *		setting all fields to zero.  The MESG buffer is not freed.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Wednesday, April 15, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_comp_reset (void *mesg)
{
    H5O_compress_t	*comp = (H5O_compress_t*)mesg;
    
    FUNC_ENTER (H5O_comp_reset, FAIL);

    assert (comp);
    H5MM_xfree (comp->client_data);
    HDmemset (comp, 0, sizeof *comp);

    FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5O_comp_debug
 *
 * Purpose:	Prints debugging information for compression message MESG on
 *		output stream STREAM.  Each line is indented INDENT
 *		characters and the field name takes up FWIDTH characters.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Wednesday, April 15, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_comp_debug (H5F_t __unused__ *f, const void *mesg, FILE *stream,
		intn indent, intn fwidth)
{
    const H5O_compress_t   	*comp = (const H5O_compress_t *)mesg;
    size_t			i, j;

    FUNC_ENTER(H5O_comp_debug, FAIL);

    /* check args */
    assert(f);
    assert(comp);
    assert(stream);
    assert(indent >= 0);
    assert(fwidth >= 0);

    fprintf (stream, "%*s%-*s %d\n", indent, "", fwidth,
	     "Method:",
	     (int)(comp->method));
    fprintf (stream, "%*s%-*s 0x%02x\n", indent, "", fwidth,
	     "Flags:",
	     (unsigned)(comp->flags));
    fprintf (stream, "%*s%-*s %lu\n", indent, "", fwidth,
	     "Size of client data:",
	     (unsigned long)(comp->cd_size));

    if (comp->cd_size>0) {
	fprintf (stream, "%*s%s\n", indent, "", "Client Data:");
	for (i=0; i<comp->cd_size; i+=16) {
	    fprintf (stream, "%*s%04d: ", indent+3, "", i);
	    for (j=0; j<16; j++) {
		if (8==j) putc (' ', stream);
		if (i+j<comp->cd_size) {
		    fprintf (stream, "%02x ", comp->client_data[i+j]);
		} else {
		    fputs ("   ", stream);
		}
	    }
	    for (j=0; j<16 && i+j<comp->cd_size; j++) {
		if (8==j) putc (' ', stream);
		if (comp->client_data[i+j]>' ' &&
		    comp->client_data[i+j]<='~') {
		    putc (comp->client_data[i+j], stream);
		} else {
		    putc ('.', stream);
		}
		putc ('\n', stream);
	    }
	}
    } else {
	fprintf (stream, "%*s%-*s None\n", indent, "", fwidth, "Client Data:");
    }
    
    FUNC_LEAVE(SUCCEED);
}
