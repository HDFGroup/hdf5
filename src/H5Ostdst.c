/****************************************************************************
* NCSA HDF                                                                 *
* Software Development Group                                               *
* National Center for Supercomputing Applications                          *
* University of Illinois at Urbana-Champaign                               *
* 605 E. Springfield, Champaign IL 61820                                   *
*                                                                          *
* For conditions of distribution and use, see the accompanying             *
* hdf/COPYING file.                                                        *
*                                                                          *
****************************************************************************/

#ifdef RCSID
static char RcsId[] = "@(#)$Revision$";
#endif

/* $Id$ */

/*LINTLIBRARY */
/*+
   FILE
       H5Ostdst.c
   HDF5 Standard Data Storage Object Header Message routines

   EXPORTED ROUTINES

   LIBRARY-SCOPED ROUTINES

   LOCAL ROUTINES
   + */

#include <H5private.h>
#include <H5Eprivate.h>
#include <H5Gprivate.h>
#include <H5MMprivate.h>
#include <H5Oprivate.h>

#define PABLO_MASK	H5O_std_store_mask

/* PRIVATE PROTOTYPES */
static void *H5O_std_store_decode (H5F_t *f, size_t raw_size, const uint8 *p);
static herr_t H5O_std_store_encode (H5F_t *f, size_t size, uint8 *p,
			       const void *_mesg);
static void *H5O_std_store_copy (const void *_mesg, void *_dest);
static size_t H5O_std_store_size (H5F_t *f, const void *_mesg);
static herr_t H5O_std_store_debug (H5F_t *f, const void *_mesg,
			      FILE *stream, intn indent, intn fwidth);

/* This message derives from H5O */
const H5O_class_t H5O_STD_STORE[1] = {{
   H5O_STD_STORE_ID,        /* message id number */
   "std_store",             /* message name for debugging */
   sizeof (H5O_std_store_t),/* native message size */
   H5G_NOTHING_CACHED, 	    /* symtab entry `type' field */
   H5O_std_store_decode,    /* decode message */
   H5O_std_store_encode,    /* encode message */
   NULL,                    /* get message from stab entry */
   NULL,                    /* put message into stab entry */
   H5O_std_store_copy,      /* copy the native value */
   H5O_std_store_size,      /* size of symbol table entry */
   NULL,                    /* default reset method */
   H5O_std_store_debug,     /* debug the message */
}};

/* Is the interface initialized? */
static hbool_t interface_initialize_g = FALSE;

/*--------------------------------------------------------------------------
 NAME
    H5O_std_store_decode
 PURPOSE
    Decode a standard data storage and return a pointer to a memory
        struct with the decoded information
 USAGE
    void *H5O_std_store_decode(f, raw_size, p)
        H5F_t *f;         IN: pointer to the HDF5 file struct
        size_t raw_size;        IN: size of the raw information buffer
        const uint8 *p;         IN: the raw information buffer
 RETURNS
    Pointer to the new message in native order on success, NULL on failure
 DESCRIPTION
        This function decodes the "raw" disk form of a standard data storage
    message into a struct in memory native format.  The struct is allocated
    within this function using malloc() and is returned to the caller.
--------------------------------------------------------------------------*/
static void *
H5O_std_store_decode (H5F_t *f, size_t raw_size, const uint8 *p)
{
    H5O_std_store_t *store=NULL;   /* New standard storage structure */
   
    FUNC_ENTER (H5O_std_store_decode, NULL, NULL);

    /* check args */
    assert (f);
    assert (raw_size == H5F_SIZEOF_OFFSET(f)+H5F_SIZEOF_SIZE(f));
    assert (p);

    /* decode */
    if((store = H5MM_xcalloc (1, sizeof(H5O_std_store_t)))!=NULL)
      {
        H5F_addr_decode (f, &p,&(store->off));
        H5F_decode_length(f,p,store->len);
      } /* end if */

#ifdef LATER
done:
#endif /* LATER */
  if(store == NULL)   
    { /* Error condition cleanup */

    } /* end if */

    /* Normal function cleanup */

    FUNC_LEAVE (store);
} /* end H5O_std_store_decode() */

/*--------------------------------------------------------------------------
 NAME
    H5O_std_store_encode
 PURPOSE
    Encode a standard data storage message 
 USAGE
    herr_t H5O_std_store_encode(f, raw_size, p, mesg)
        H5F_t *f;         IN: pointer to the HDF5 file struct
        size_t raw_size;        IN: size of the raw information buffer
        const uint8 *p;         IN: the raw information buffer
        const void *mesg;       IN: Pointer to the standard storage struct
 RETURNS
    SUCCEED/FAIL
 DESCRIPTION
        This function encodes the native memory form of the standard data
    storage message in the "raw" disk form.
--------------------------------------------------------------------------*/
static herr_t
H5O_std_store_encode (H5F_t *f, size_t raw_size, uint8 *p, const void *mesg)
{
    const H5O_std_store_t *store = (const H5O_std_store_t *)mesg;

    FUNC_ENTER (H5O_std_store_encode, NULL, FAIL);

    /* check args */
    assert (f);
    assert (raw_size == H5F_SIZEOF_OFFSET(f)+H5F_SIZEOF_SIZE(f));
    assert (p);
    assert (store);

    /* encode */
    H5F_addr_encode (f, &p, &(store->off));
    H5F_encode_length(f,p,store->len);

    FUNC_LEAVE (SUCCEED);
} /* end H5O_std_store_encode() */

/*--------------------------------------------------------------------------
 NAME
    H5O_std_store_copy
 PURPOSE
    Copies a message from MESG to DEST, allocating DEST if necessary.
 USAGE
    void *H5O_std_store_copy(mesg, dest)
        const void *mesg;       IN: Pointer to the source standard storage struct
        const void *dest;       IN: Pointer to the destination standard storage struct
 RETURNS
    Pointer to DEST on success, NULL on failure
 DESCRIPTION
        This function copies a native (memory) standard storage message,
    allocating the destination structure if necessary.
--------------------------------------------------------------------------*/
static void *
H5O_std_store_copy (const void *mesg, void *dest)
{
   const H5O_std_store_t	*src = (const H5O_std_store_t *)mesg;
   H5O_std_store_t		*dst = (H5O_std_store_t *)dest;
   
   FUNC_ENTER (H5O_std_store_copy, NULL, NULL);

   /* check args */
   assert (src);
   if (!dst)
       dst = H5MM_xcalloc (1, sizeof(H5O_std_store_t));

   /* copy */
   HDmemcpy(dst,src,sizeof(H5O_std_store_t));

   FUNC_LEAVE ((void*)dst);
} /* end H5O_std_store_copy() */

/*--------------------------------------------------------------------------
 NAME
    H5O_std_store_size
 PURPOSE
    Return the raw message size in bytes
 USAGE
    void *H5O_std_store_copy(f, mesg)
        H5F_t *f;         IN: pointer to the HDF5 file struct
        const void *mesg;       IN: Pointer to the source standard storage struct
 RETURNS
    Size of message on success, FAIL on failure
 DESCRIPTION
        This function returns the size of the raw standard storage message on
    success.  (Not counting the message type or size fields, only the data
    portion of the message).  It doesn't take into account alignment.
--------------------------------------------------------------------------*/
static size_t
H5O_std_store_size (H5F_t *f, const void *mesg)
{
   size_t ret_value;

   FUNC_ENTER (H5O_std_store_size, NULL, FAIL);

   /* All standard data storage messages have the same data */
   ret_value=H5F_SIZEOF_OFFSET(f)+H5F_SIZEOF_SIZE(f);

   FUNC_LEAVE (ret_value);
} /* end H5O_std_store_size() */

/*--------------------------------------------------------------------------
 NAME
    H5O_std_store_debug
 PURPOSE
    Prints debugging information for a standard storage message
 USAGE
    void *H5O_std_store_debug(f, mesg, stream, indent, fwidth)
        H5F_t *f;         IN: pointer to the HDF5 file struct
        const void *mesg;       IN: Pointer to the source standard storage struct
        FILE *stream;           IN: Pointer to the stream for output data
        intn indent;            IN: Amount to indent information by
        intn fwidth;            IN: Field width (?)
 RETURNS
    SUCCEED/FAIL
 DESCRIPTION
        This function prints debugging output to the stream passed as a 
    parameter.
--------------------------------------------------------------------------*/
static herr_t
H5O_std_store_debug (H5F_t *f, const void *mesg, FILE *stream,
		intn indent, intn fwidth)
{
   const H5O_std_store_t	*store = (const H5O_std_store_t *)mesg;
   
   FUNC_ENTER (H5O_std_store_debug, NULL, FAIL);

   /* check args */
   assert (f);
   assert (store);
   assert (stream);
   assert (indent>=0);
   assert (fwidth>=0);

   fprintf (stream, "%*s%-*s ", indent, "", fwidth,
	    "Offset:");
   H5F_addr_print (stream, &(store->off));
   fprintf (stream, "\n");

   fprintf (stream, "%*s%-*s %lu\n", indent, "", fwidth,
	    "Length:",
	    (unsigned long)(store->len));

   FUNC_LEAVE (SUCCEED);
} /* end H5O_std_store_debug() */

