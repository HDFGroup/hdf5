/*
 * Copyright (C) 1997 NCSA
 *		      All rights reserved.
 *
 * Programmer: Robb Matzke <matzke@llnl.gov>
 *	       Tuesday, November 25, 1997
 */
#include <H5private.h>
#include <H5Eprivate.h>
#include <H5Hprivate.h>
#include <H5MMprivate.h>
#include <H5Oprivate.h>

#define PABLO_MASK	H5O_efl_mask

/* PRIVATE PROTOTYPES */
static void *H5O_efl_decode(H5F_t *f, size_t raw_size, const uint8 *p);
static herr_t H5O_efl_encode(H5F_t *f, size_t size, uint8 *p,
			     const void *_mesg);
static void *H5O_efl_copy(const void *_mesg, void *_dest);
static size_t H5O_efl_size(H5F_t *f, const void *_mesg);
static herr_t H5O_efl_reset(void *_mesg);
static herr_t H5O_efl_debug(H5F_t *f, const void *_mesg, FILE * stream,
			    intn indent, intn fwidth);

/* This message derives from H5O */
const H5O_class_t H5O_EFL[1] = {{
    H5O_EFL_ID,		    /*message id number		    */
    "external file list",   /*message name for debugging    */
    sizeof(H5O_efl_t),	    /*native message size	    */
    H5O_efl_decode,	    /*decode message		    */
    H5O_efl_encode,	    /*encode message		    */
    H5O_efl_copy,	    /*copy native value		    */
    H5O_efl_size,	    /*size of message on disk	    */
    H5O_efl_reset,	    /*reset method		    */
    H5O_efl_debug,	    /*debug the message		    */
}};

/* Interface initialization */
static hbool_t interface_initialize_g = FALSE;
#define INTERFACE_INIT	NULL


/*-------------------------------------------------------------------------
 * Function:	H5O_efl_decode
 *
 * Purpose:	Decode an external file list message and return a pointer to
 *		the message (and some other data).
 *
 * Return:	Success:	Ptr to a new message struct.
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *		Tuesday, November 25, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void *
H5O_efl_decode(H5F_t *f, size_t raw_size, const uint8 *p)
{
    H5O_efl_t		*mesg = NULL;
    int			i;
    const char		*s = NULL;

    FUNC_ENTER(H5O_efl_decode, NULL);

    /* Check args */
    assert(f);
    assert(p);

    /* Decode the header */
    mesg = H5MM_xcalloc(1, sizeof(H5O_efl_t));
    H5F_addr_decode(f, &p, &(mesg->heap_addr));
#ifndef NDEBUG
    assert (H5F_addr_defined (&(mesg->heap_addr)));
    s = H5H_peek (f, &(mesg->heap_addr), 0);
    assert (s && !*s);
#endif
    UINT16DECODE(p, mesg->nalloc);
    assert(mesg->nalloc>0);
    UINT16DECODE(p, mesg->nused);
    assert(mesg->nused <= mesg->nalloc);
    p += 4; /*reserved*/

    /* Decode the file list */
    mesg->slot = H5MM_xcalloc(mesg->nalloc, sizeof(H5O_efl_entry_t));
    for (i=0; i<mesg->nused; i++) {
	/* Name */
	H5F_decode_length (f, p, mesg->slot[i].name_offset);
	s = H5H_peek (f, &(mesg->heap_addr), mesg->slot[i].name_offset);
	assert (s && *s);
	mesg->slot[i].name = H5MM_xstrdup (s);
	
	/* File offset */
	H5F_decode_length (f, p, mesg->slot[i].offset);

	/* Size */
	H5F_decode_length (f, p, mesg->slot[i].size);
	assert (mesg->slot[i].size>0);
    }

    FUNC_LEAVE(mesg);
}

/*-------------------------------------------------------------------------
 * Function:	H5O_efl_encode
 *
 * Purpose:	Encodes a message.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Tuesday, November 25, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_efl_encode(H5F_t *f, size_t raw_size, uint8 *p, const void *_mesg)
{
    const H5O_efl_t	*mesg = (const H5O_efl_t *)_mesg;
    int			i;
    size_t		offset;
    

    FUNC_ENTER(H5O_efl_encode, FAIL);

    /* check args */
    assert(f);
    assert(mesg);
    assert(raw_size == H5O_ALIGN (H5O_efl_size(f, _mesg)));
    assert(p);

    /* Encode header */
    assert (H5F_addr_defined (&(mesg->heap_addr)));
    H5F_addr_encode(f, &p, &(mesg->heap_addr));
    assert (mesg->nalloc>0);
    UINT16ENCODE(p, mesg->nused); /*yes, twice*/
    assert (mesg->nused>0 && mesg->nused<=mesg->nalloc);
    UINT16ENCODE(p, mesg->nused);
    *p++ = 0;
    *p++ = 0;
    *p++ = 0;
    *p++ = 0;

    /* Encode file list */
    for (i=0; i<mesg->nused; i++) {
	/*
	 * If the name has not been added to the heap yet then do so now.
	 */
	if (0==mesg->slot[i].name_offset) {
	    offset = H5H_insert (f, &(mesg->heap_addr),
				 strlen (mesg->slot[i].name)+1,
				 mesg->slot[i].name);
	    if ((size_t)(-1)==offset) {
		HRETURN_ERROR (H5E_EFL, H5E_CANTINIT, FAIL,
			       "unable to insert URL into name heap");
	    }
	    mesg->slot[i].name_offset = offset;
	}

	/* Encode the file info */
	H5F_encode_length (f, p, mesg->slot[i].name_offset);
	H5F_encode_length (f, p, mesg->slot[i].offset);
	H5F_encode_length (f, p, mesg->slot[i].size);
    }

    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5O_efl_copy
 *
 * Purpose:	Copies a message from _MESG to _DEST, allocating _DEST if
 *		necessary.
 *
 * Return:	Success:	Ptr to _DEST
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *		Tuesday, November 25, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void *
H5O_efl_copy(const void *_mesg, void *_dest)
{
    const H5O_efl_t	*mesg = (const H5O_efl_t *) _mesg;
    H5O_efl_t		*dest = (H5O_efl_t *) _dest;
    int			i;

    FUNC_ENTER(H5O_efl_copy, NULL);

    /* check args */
    assert(mesg);
    if (!dest) {
	dest = H5MM_xcalloc(1, sizeof(H5O_efl_t));
	dest->slot = H5MM_xmalloc(mesg->nalloc * sizeof(H5O_efl_entry_t));
    } else if (NULL==dest->slot || dest->nalloc<mesg->nalloc) {
	H5MM_xfree(dest->slot);
	dest->slot = H5MM_xmalloc(mesg->nalloc * sizeof(H5O_efl_entry_t));
    }
    dest->heap_addr = mesg->heap_addr;
    dest->nalloc = mesg->nalloc;
    dest->nused = mesg->nused;

    for (i = 0; i < mesg->nused; i++) {
	dest->slot[i] = mesg->slot[i];
	dest->slot[i].name = H5MM_xstrdup (mesg->slot[i].name);
    }

    FUNC_LEAVE((void *)dest);
}


/*-------------------------------------------------------------------------
 * Function:	H5O_efl_size
 *
 * Purpose:	Returns the size of the raw message in bytes not counting the
 *		message type or size fields, but only the data fields.	This
 *		function doesn't take into account message alignment. This
 *		function doesn't count unused slots.
 *
 * Return:	Success:	Message data size in bytes.
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Tuesday, November 25, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static size_t
H5O_efl_size(H5F_t *f, const void *_mesg)
{
    const H5O_efl_t	*mesg = (const H5O_efl_t *) _mesg;
    size_t		ret_value = FAIL;

    FUNC_ENTER(H5O_efl_size, FAIL);

    /* check args */
    assert(f);
    assert(mesg);

    ret_value = H5F_SIZEOF_ADDR(f) +			/*heap address	*/
		2 +					/*slots allocated*/
		2 +					/*num slots used*/
		4 +					/*reserved	*/
		mesg->nused * (H5F_SIZEOF_SIZE(f) +	/*name offset	*/
			       H5F_SIZEOF_SIZE(f) +	/*file offset	*/
			       H5F_SIZEOF_SIZE(f));	/*file size	*/

    FUNC_LEAVE(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5O_efl_reset
 *
 * Purpose:	Frees internal pointers and resets the message to an
 *		initialial state.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Tuesday, November 25, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_efl_reset(void *_mesg)
{
    H5O_efl_t	*mesg = (H5O_efl_t *) _mesg;
    int		i;
    

    FUNC_ENTER(H5O_efl_reset, FAIL);

    /* check args */
    assert(mesg);

    /* reset */
    for (i=0; i<mesg->nused; i++) {
	mesg->slot[i].name = H5MM_xfree (mesg->slot[i].name);
    }
    H5F_addr_undef (&(mesg->heap_addr));
    mesg->nused = mesg->nalloc = 0;
    mesg->slot = H5MM_xfree(mesg->slot);

    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5O_efl_total_size
 *
 * Purpose:	Return the total size of the external file list by summing
 *		the sizes of all of the files.
 *
 * Return:	Success:	Total reserved size for external data.
 *
 *		Failure:	0
 *
 * Programmer:	Robb Matzke
 *              Tuesday, March  3, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
size_t
H5O_efl_total_size (H5O_efl_t *efl)
{
    int		i;
    size_t	ret_value = 0, tmp;
    
    FUNC_ENTER (H5O_efl_total_size, 0);

    if (efl->nused>0 &&
	H5O_EFL_UNLIMITED==efl->slot[efl->nused-1].size) {
	ret_value = H5O_EFL_UNLIMITED;
    } else {
	for (i=0; i<efl->nused; i++, ret_value=tmp) {
	    tmp = ret_value + efl->slot[i].size;
	    if (tmp<=ret_value) {
		HRETURN_ERROR (H5E_EFL, H5E_OVERFLOW, 0,
			       "total external storage size overflowed");
	    }
	}
    }

    FUNC_LEAVE (ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5O_efl_debug
 *
 * Purpose:	Prints debugging info for a message.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Tuesday, November 25, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_efl_debug(H5F_t *f, const void *_mesg, FILE * stream, intn indent,
	      intn fwidth)
{
    const H5O_efl_t	   *mesg = (const H5O_efl_t *) _mesg;
    char		    buf[64];
    intn		    i;

    FUNC_ENTER(H5O_efl_debug, FAIL);

    /* check args */
    assert(f);
    assert(mesg);
    assert(stream);
    assert(indent >= 0);
    assert(fwidth >= 0);

    fprintf(stream, "%*s%-*s ", indent, "", fwidth,
	    "Heap address:");
    H5F_addr_print(stream, &(mesg->heap_addr));
    fprintf(stream, "\n");

    fprintf(stream, "%*s%-*s %u/%u\n", indent, "", fwidth,
	    "Slots used/allocated:",
	    mesg->nused, mesg->nalloc);

    for (i = 0; i < mesg->nused; i++) {
	sprintf (buf, "File %d", i);
	fprintf (stream, "%*s%s:\n", indent, "", buf);
	
	fprintf(stream, "%*s%-*s \"%s\"\n", indent+3, "", MAX (fwidth-3, 0),
		"Name:",
		mesg->slot[i].name);
	
	fprintf(stream, "%*s%-*s %lu\n", indent+3, "", MAX (fwidth-3, 0),
		"Name offset:",
		(unsigned long)(mesg->slot[i].name_offset));

	fprintf (stream, "%*s%-*s %lu\n", indent+3, "", MAX (fwidth-3, 0),
		 "Offset of data in file:",
		 (unsigned long)(mesg->slot[i].offset));

	fprintf (stream, "%*s%-*s %lu\n", indent+3, "", MAX (fwidth-3, 0),
		 "Bytes reserved for data:",
		 (unsigned long)(mesg->slot[i].size));
    }

    FUNC_LEAVE(SUCCEED);
}
