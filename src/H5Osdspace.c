/****************************************************************************
* NCSA HDF								   *
* Software Development Group						   *
* National Center for Supercomputing Applications			   *
* University of Illinois at Urbana-Champaign				   *
* 605 E. Springfield, Champaign IL 61820				   *
*									   *
* For conditions of distribution and use, see the accompanying		   *
* hdf/COPYING file.							   *
*									   *
****************************************************************************/

#ifdef RCSID
static char		RcsId[] = "@(#)$Revision$";
#endif

/* $Id$ */

#include <H5private.h>
#include <H5Eprivate.h>
#include <H5Gprivate.h>
#include <H5MMprivate.h>
#include <H5Oprivate.h>

#define PABLO_MASK	H5O_sdspace_mask

/* PRIVATE PROTOTYPES */
static void *H5O_sdspace_decode(H5F_t *f, const uint8 *p, H5O_shared_t *sh);
static herr_t H5O_sdspace_encode(H5F_t *f, uint8 *p, const void *_mesg);
static void *H5O_sdspace_copy(const void *_mesg, void *_dest);
static size_t H5O_sdspace_size(H5F_t *f, const void *_mesg);
static herr_t H5O_sdspace_debug(H5F_t *f, const void *_mesg,
				FILE * stream, intn indent, intn fwidth);
static herr_t H5O_sdspace_reset(void *_mesg);

/* This message derives from H5O */
const H5O_class_t H5O_SDSPACE[1] = {{
    H5O_SDSPACE_ID,	    	/* message id number		    	*/
    "simple_dspace",	    	/* message name for debugging	   	*/
    sizeof(H5S_simple_t),   	/* native message size		    	*/
    H5O_sdspace_decode,	    	/* decode message			*/
    H5O_sdspace_encode,	    	/* encode message			*/
    H5O_sdspace_copy,	    	/* copy the native value		*/
    H5O_sdspace_size,	    	/* size of symbol table entry	    	*/
    H5O_sdspace_reset,	    	/* default reset method		    	*/
    NULL,		    	/* get share method			*/
    NULL, 			/* set share method			*/
    H5O_sdspace_debug,	        /* debug the message		    	*/
}};

#define H5O_SDSPACE_VERSION	1

/* Is the interface initialized? */
static hbool_t interface_initialize_g = FALSE;
#define INTERFACE_INIT NULL

/*--------------------------------------------------------------------------
 NAME
    H5O_sdspace_decode
 PURPOSE
    Decode a simple dimensionality message and return a pointer to a memory
	struct with the decoded information
 USAGE
    void *H5O_sdspace_decode(f, raw_size, p)
	H5F_t *f;	  IN: pointer to the HDF5 file struct
	size_t raw_size;	IN: size of the raw information buffer
	const uint8 *p;		IN: the raw information buffer
 RETURNS
    Pointer to the new message in native order on success, NULL on failure
 DESCRIPTION
	This function decodes the "raw" disk form of a simple dimensionality
    message into a struct in memory native format.  The struct is allocated
    within this function using malloc() and is returned to the caller.

 MODIFICATIONS
	Robb Matzke, 1998-04-09
	The current and maximum dimensions are now H5F_SIZEOF_SIZET bytes
	instead of just four bytes.
 
  	Robb Matzke, 1998-07-20
        Added a version number and reformatted the message for aligment.
--------------------------------------------------------------------------*/
static void *
H5O_sdspace_decode(H5F_t *f, const uint8 *p, H5O_shared_t __unused__ *sh)
{
    H5S_simple_t	*sdim = NULL;/* New simple dimensionality structure */
    intn		u;		/* local counting variable */
    uintn		flags, version;
    
    FUNC_ENTER(H5O_sdspace_decode, NULL);

    /* check args */
    assert(f);
    assert(p);
    assert (!sh);

    /* decode */
    if ((sdim = H5MM_calloc(sizeof(H5S_simple_t))) != NULL) {
	version = *p++;
	if (version!=H5O_SDSPACE_VERSION) {
	    HRETURN_ERROR(H5E_OHDR, H5E_CANTINIT, NULL,
			  "wrong version number in data space message");
	}
	sdim->rank = *p++;
	flags = *p++;
	p += 5; /*reserved*/

	if (sdim->rank > 0) {
	    if (NULL==(sdim->size=H5MM_malloc(sizeof(sdim->size[0])*
					      sdim->rank))) {
		HRETURN_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL,
			       "memory allocation failed");
	    }
	    for (u = 0; u < sdim->rank; u++) {
		H5F_decode_length (f, p, sdim->size[u]);
	    }
	    if (flags & H5S_VALID_MAX) {
		if (NULL==(sdim->max=H5MM_malloc(sizeof(sdim->max[0])*
						 sdim->rank))) {
		    HRETURN_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL,
				   "memory allocation failed");
		}
		for (u = 0; u < sdim->rank; u++) {
		    H5F_decode_length (f, p, sdim->max[u]);
		}
	    }
#ifdef LATER
	    if (flags & H5S_VALID_PERM) {
		if (NULL==(sdim->perm=H5MM_malloc(sizeof(sdim->perm[0])*
						  sdim->rank))) {
		    HRETURN_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL,
				   "memory allocation failed");
		}
		for (u = 0; u < sdim->rank; u++)
		    UINT32DECODE(p, sdim->perm[u]);
	    }
#endif
	}
    }
    
#ifdef LATER
  done:
#endif /* LATER */
    if (sdim == NULL) {		/* Error condition cleanup */

    }
    /* Normal function cleanup */
    FUNC_LEAVE(sdim);
}

/*--------------------------------------------------------------------------
 NAME
    H5O_sdspace_encode
 PURPOSE
    Encode a simple dimensionality message 
 USAGE
    herr_t H5O_sdspace_encode(f, raw_size, p, mesg)
	H5F_t *f;	  IN: pointer to the HDF5 file struct
	size_t raw_size;	IN: size of the raw information buffer
	const uint8 *p;		IN: the raw information buffer
	const void *mesg;	IN: Pointer to the simple dimensionality struct
 RETURNS
    SUCCEED/FAIL
 DESCRIPTION
	This function encodes the native memory form of the simple
    dimensionality message in the "raw" disk form.

 MODIFICATIONS
	Robb Matzke, 1998-04-09
	The current and maximum dimensions are now H5F_SIZEOF_SIZET bytes
	instead of just four bytes.
 
  	Robb Matzke, 1998-07-20
        Added a version number and reformatted the message for aligment.
--------------------------------------------------------------------------*/
static herr_t
H5O_sdspace_encode(H5F_t *f, uint8 *p, const void *mesg)
{
    const H5S_simple_t	*sdim = (const H5S_simple_t *) mesg;
    intn		u;  /* Local counting variable */
    uintn		flags = 0;

    FUNC_ENTER(H5O_sdspace_encode, FAIL);

    /* check args */
    assert(f);
    assert(p);
    assert(sdim);

    /* set flags */
    if (sdim->max) flags |= H5S_VALID_MAX;
#ifdef LATER
    if (sdim->perm) flags |= H5S_VALID_PERM;
#endif

    /* encode */
    *p++ = H5O_SDSPACE_VERSION;
    *p++ = sdim->rank;
    *p++ = flags;
    *p++ = 0; /*reserved*/
    *p++ = 0; /*reserved*/
    *p++ = 0; /*reserved*/
    *p++ = 0; /*reserved*/
    *p++ = 0; /*reserved*/

    if (sdim->rank > 0) {
	for (u = 0; u < sdim->rank; u++) {
	    H5F_encode_length (f, p, sdim->size[u]);
	}
	if (flags & H5S_VALID_MAX) {
	    for (u = 0; u < sdim->rank; u++) {
		H5F_encode_length (f, p, sdim->max[u]);
	    }
	}
#ifdef LATER
	if (flags & H5S_VALID_PERM) {
	    for (u = 0; u < sdim->rank; u++)
		UINT32ENCODE(p, sdim->perm[u]);
	}
#endif
    }
    FUNC_LEAVE(SUCCEED);
}

/*--------------------------------------------------------------------------
 NAME
    H5O_sdspace_copy
 PURPOSE
    Copies a message from MESG to DEST, allocating DEST if necessary.
 USAGE
    void *H5O_sdspace_copy(mesg, dest)
	const void *mesg;	IN: Pointer to the source simple dimensionality struct
	const void *dest;	IN: Pointer to the destination simple dimensionality struct
 RETURNS
    Pointer to DEST on success, NULL on failure
 DESCRIPTION
	This function copies a native (memory) simple dimensionality message,
    allocating the destination structure if necessary.
--------------------------------------------------------------------------*/
static void *
H5O_sdspace_copy(const void *mesg, void *dest)
{
    const H5S_simple_t	   *src = (const H5S_simple_t *) mesg;
    H5S_simple_t	   *dst = (H5S_simple_t *) dest;

    FUNC_ENTER(H5O_sdspace_copy, NULL);

    /* check args */
    assert(src);
    if (!dst && NULL==(dst = H5MM_calloc(sizeof(H5S_simple_t)))) {
	HRETURN_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL,
		       "memory allocation failed");
    }

    /* deep copy -- pointed-to values are copied also */
    HDmemcpy(dst, src, sizeof(H5S_simple_t));
    
    if (src->size) {
	if (NULL==(dst->size = H5MM_calloc(src->rank*sizeof(src->size[0])))) {
	    HRETURN_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL,
			   "memory allocation failed");
	}
	HDmemcpy (dst->size, src->size, src->rank*sizeof(src->size[0]));
    }
    if (src->max) {
	if (NULL==(dst->max=H5MM_calloc(src->rank*sizeof(src->max[0])))) {
	    HRETURN_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL,
			   "memory allocation failed");
	}
	HDmemcpy (dst->max, src->max, src->rank*sizeof(src->max[0]));
    }
#ifdef LATER
    if (src->perm) {
	if (NULL==(dst->perm=H5MM_calloc(src->rank*sizeof(src->perm[0])))) {
	    HRETURN_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL,
			   "memory allocation failed");
	}
	HDmemcpy (dst->perm, src->perm, src->rank*sizeof(src->perm[0]));
    }
#endif

    FUNC_LEAVE((void *) dst);
}


/*-------------------------------------------------------------------------
 * Function:	H5O_sdspace_reset
 *
 * Purpose:	Frees the inside of a dataspace message and resets it to some
 *		initial value.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Thursday, April 30, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_sdspace_reset(void *_mesg)
{
    H5S_simple_t	*mesg = (H5S_simple_t*)_mesg;
    
    FUNC_ENTER (H5O_sdspace_reset, FAIL);
    mesg->size = H5MM_xfree (mesg->size);
    mesg->max = H5MM_xfree (mesg->max);
#ifdef LATER
    mesg->perm = H5MM_xfree (mesg->perm);
#endif
    FUNC_LEAVE (SUCCEED);
}

/*--------------------------------------------------------------------------
 NAME
    H5O_sdspace_size
 PURPOSE
    Return the raw message size in bytes
 USAGE
    void *H5O_sdspace_copy(f, mesg)
	H5F_t *f;	  IN: pointer to the HDF5 file struct
	const void *mesg;	IN: Pointer to the source simple dimensionality struct
 RETURNS
    Size of message on success, zero on failure
 DESCRIPTION
	This function returns the size of the raw simple dimensionality message on
    success.  (Not counting the message type or size fields, only the data
    portion of the message).  It doesn't take into account alignment.

 MODIFICATIONS
	Robb Matzke, 1998-04-09
	The current and maximum dimensions are now H5F_SIZEOF_SIZET bytes
	instead of just four bytes.
--------------------------------------------------------------------------*/
static size_t
H5O_sdspace_size(H5F_t *f, const void *mesg)
{
    const H5S_simple_t	   *sdim = (const H5S_simple_t *) mesg;
    
    /*
     * All dimensionality messages are at least 8 bytes long.
     */
    size_t		    ret_value = 8;

    FUNC_ENTER(H5O_sdspace_size, 0);

    /* add in the dimension sizes */
    ret_value += sdim->rank * H5F_SIZEOF_SIZE (f);

    /* add in the space for the maximum dimensions, if they are present */
    ret_value += sdim->max ? sdim->rank * H5F_SIZEOF_SIZE (f) : 0;

#ifdef LATER
    /* add in the space for the dimension permutations, if they are present */
    ret_value += sdim->perm ? sdim->rank * 4 : 0;
#endif

    FUNC_LEAVE(ret_value);
}

/*--------------------------------------------------------------------------
 NAME
    H5O_sdspace_debug
 PURPOSE
    Prints debugging information for a simple dimensionality message
 USAGE
    void *H5O_sdspace_debug(f, mesg, stream, indent, fwidth)
	H5F_t *f;	  IN: pointer to the HDF5 file struct
	const void *mesg;	IN: Pointer to the source simple dimensionality struct
	FILE *stream;		IN: Pointer to the stream for output data
	intn indent;		IN: Amount to indent information by
	intn fwidth;		IN: Field width (?)
 RETURNS
    SUCCEED/FAIL
 DESCRIPTION
	This function prints debugging output to the stream passed as a 
    parameter.
--------------------------------------------------------------------------*/
static herr_t
H5O_sdspace_debug(H5F_t __unused__ *f, const void *mesg,
		  FILE * stream, intn indent, intn fwidth)
{
    const H5S_simple_t	   *sdim = (const H5S_simple_t *) mesg;
    intn		    u;	/* local counting variable */

    FUNC_ENTER(H5O_sdspace_debug, FAIL);

    /* check args */
    assert(f);
    assert(sdim);
    assert(stream);
    assert(indent >= 0);
    assert(fwidth >= 0);

    HDfprintf(stream, "%*s%-*s %lu\n", indent, "", fwidth,
	    "Rank:",
	    (unsigned long) (sdim->rank));
    
    HDfprintf(stream, "%*s%-*s {", indent, "", fwidth, "Dim Size:");
    for (u = 0; u < sdim->rank; u++) {
	HDfprintf (stream, "%s%Hu", u?", ":"", sdim->size[u]);
    }
    HDfprintf (stream, "}\n");
    
    HDfprintf(stream, "%*s%-*s ", indent, "", fwidth, "Dim Max:");
    if (sdim->max) {
	HDfprintf (stream, "{");
	for (u = 0; u < sdim->rank; u++) {
	    if (H5S_UNLIMITED==sdim->max[u]) {
		HDfprintf (stream, "%sINF", u?", ":"");
	    } else {
		HDfprintf (stream, "%s%Hu", u?", ":"", sdim->max[u]);
	    }
	}
	HDfprintf (stream, "}\n");
    } else {
	HDfprintf (stream, "CONSTANT\n");
    }

#ifdef LATER
    if (sdim->perm) {
	HDfprintf(stream, "%*s%-*s {", indent, "", fwidth, "Dim Perm:");
	for (u = 0; u < sdim->rank; u++) {
	    HDfprintf (stream, "%s%lu", u?", ":"",
		     (unsigned long) (sdim->perm[u]));
	}
    }
#endif

    FUNC_LEAVE(SUCCEED);
}
