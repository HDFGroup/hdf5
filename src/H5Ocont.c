/*-------------------------------------------------------------------------
 * Copyright (C) 1997	National Center for Supercomputing Applications.
 *                      All rights reserved.
 *
 *-------------------------------------------------------------------------
 *
 * Created:		H5Ocont.c
 * 			Aug  6 1997
 * 			Robb Matzke <matzke@llnl.gov>
 *
 * Purpose:		The object header continuation message.  This
 *			message is only generated and read from within
 *			the H5O package.  Therefore, do not change
 *			any definitions in this file!
 *
 * Modifications:	
 *
 *-------------------------------------------------------------------------
 */
#include <H5private.h>
#include <H5Eprivate.h>
#include <H5MMprivate.h>
#include <H5Oprivate.h>

#define PABLO_MASK	H5O_cont_mask

/* PRIVATE PROTOTYPES */
static void *H5O_cont_decode (H5F_t *f, size_t raw_size, const uint8 *p);
static herr_t H5O_cont_encode (H5F_t *f, size_t size, uint8 *p,
			       const void *_mesg);
static herr_t H5O_cont_debug (H5F_t *f, const void *_mesg, FILE *stream,
			      intn indent, intn fwidth);

/* This message derives from H5O */
const H5O_class_t H5O_CONT[1] = {{
   H5O_CONT_ID,				/*message id number		*/
   "hdr continuation",			/*message name for debugging	*/
   sizeof (H5O_cont_t),			/*native message size		*/
   H5G_NOTHING_CACHED, 			/*symbol table type field	*/   
   H5O_cont_decode,			/*decode message		*/
   H5O_cont_encode,			/*encode message		*/
   NULL,				/*no fast method		*/
   NULL,				/*no cache method		*/
   NULL,				/*no copy method		*/
   NULL,				/*no size method		*/
   NULL,				/*default reset method		*/
   H5O_cont_debug,			/*debugging			*/
}};

/* Is the interface initialized? */
static intn interface_initialize_g = FALSE;


/*-------------------------------------------------------------------------
 * Function:	H5O_cont_decode
 *
 * Purpose:	Decode the raw header continuation message.
 *
 * Return:	Success:	Ptr to the new native message
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug  6 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void *
H5O_cont_decode (H5F_t *f, size_t raw_size, const uint8 *p)
{
   H5O_cont_t	*cont = NULL;
   
   FUNC_ENTER (H5O_cont_decode, NULL, NULL);

   /* check args */
   assert (f);
   assert (raw_size == H5F_SIZEOF_OFFSET(f) + H5F_SIZEOF_SIZE(f));
   assert (p);

   /* decode */
   cont = H5MM_xcalloc (1, sizeof(H5O_cont_t));
   H5F_decode_offset (f, p, cont->addr);
   H5F_decode_length (f, p, cont->size);

   FUNC_LEAVE ((void*)cont);
}


/*-------------------------------------------------------------------------
 * Function:	H5O_cont_encode
 *
 * Purpose:	Encodes a continuation message.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug  7 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_cont_encode (H5F_t *f, size_t size, uint8 *p, const void *_mesg)
{
   const H5O_cont_t	*cont = (const H5O_cont_t *)_mesg;

   FUNC_ENTER (H5O_cont_encode, NULL, FAIL);

   /* check args */
   assert (f);
   assert (size == H5F_SIZEOF_OFFSET(f) + H5F_SIZEOF_SIZE(f));
   assert (p);
   assert (cont);

   /* encode */
   H5F_encode_offset (f, p, cont->addr);
   H5F_encode_length (f, p, cont->size);

   FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5O_cont_debug
 *
 * Purpose:	Prints debugging info.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug  6 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_cont_debug (H5F_t *f, const void *_mesg, FILE *stream,
		intn indent, intn fwidth)
{
   const H5O_cont_t	*cont = (const H5O_cont_t *)_mesg;
   
   FUNC_ENTER (H5O_cont_debug, NULL, FAIL);

   /* check args */
   assert (f);
   assert (cont);
   assert (stream);
   assert (indent>=0);
   assert (fwidth>=0);

   fprintf (stream, "%*s%-*s %lu\n", indent, "", fwidth,
	    "Continuation address:",
	    (unsigned long)(cont->addr));
   fprintf (stream, "%*s%-*s %lu\n", indent, "", fwidth,
	    "Continuation size in bytes:",
	    (unsigned long)(cont->size));
   fprintf (stream, "%*s%-*s %d\n", indent, "", fwidth,
	    "Points to chunk number:",
	    (int)(cont->chunkno));

   FUNC_LEAVE (SUCCEED);
}
