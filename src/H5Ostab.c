/*-------------------------------------------------------------------------
 * Copyright (C) 1997	National Center for Supercomputing Applications.
 *                      All rights reserved.
 *
 *-------------------------------------------------------------------------
 *
 * Created:		H5Ostab.c
 * 			Aug  6 1997
 * 			Robb Matzke <matzke@llnl.gov>
 *
 * Purpose:		Symbol table messages.
 *
 * Modifications:	
 *
 *-------------------------------------------------------------------------
 */
#include <H5private.h>
#include <H5Eprivate.h>
#include <H5Gprivate.h>
#include <H5MMprivate.h>
#include <H5Oprivate.h>

#define PABLO_MASK	H5O_stab_mask

/* PRIVATE PROTOTYPES */
static void *H5O_stab_decode (hdf5_file_t *f, size_t raw_size, const uint8 *p);
static herr_t H5O_stab_encode (hdf5_file_t *f, size_t size, uint8 *p,
			       const void *_mesg);
static void *H5O_stab_fast (const H5G_entry_t *ent, void *_mesg);
static hbool_t H5O_stab_cache (H5G_entry_t *ent, const void *_mesg);
static void *H5O_stab_copy (const void *_mesg, void *_dest);
static size_t H5O_stab_size (hdf5_file_t *f, const void *_mesg);
static herr_t H5O_stab_debug (hdf5_file_t *f, const void *_mesg,
			      FILE *stream, intn indent, intn fwidth);

/* This message derives from H5O */
const H5O_class_t H5O_STAB[1] = {{
   H5O_STAB_ID,				/*message id number		*/
   "stab",				/*message name for debugging	*/
   sizeof (H5O_stab_t),			/*native message size		*/
   H5O_stab_decode,			/*decode message		*/
   H5O_stab_encode,			/*encode message		*/
   H5O_stab_fast,			/*get message from stab entry	*/
   H5O_stab_cache,			/*put message into stab entry	*/
   H5O_stab_copy,			/*copy the native value		*/
   H5O_stab_size,			/*size of symbol table entry	*/
   NULL,				/*default reset method		*/
   H5O_stab_debug,			/*debug the message		*/
}};

/* Is the interface initialized? */
static hbool_t interface_initialize_g = FALSE;


/*-------------------------------------------------------------------------
 * Function:	H5O_stab_decode
 *
 * Purpose:	Decode a symbol table message and return a pointer to
 *		a new one created with malloc().
 *
 * Return:	Success:	Ptr to new message in native order.
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
H5O_stab_decode (hdf5_file_t *f, size_t raw_size, const uint8 *p)
{
   H5O_stab_t	*stab;
   
   FUNC_ENTER (H5O_stab_decode, NULL, NULL);

   /* check args */
   assert (f);
   assert (raw_size == 2*H5F_SIZEOF_OFFSET(f));
   assert (p);

   /* decode */
   stab = H5MM_xcalloc (1, sizeof(H5O_stab_t));
   H5F_decode_offset (f, p, stab->btree_addr);
   H5F_decode_offset (f, p, stab->heap_addr);

   FUNC_LEAVE (stab);
}


/*-------------------------------------------------------------------------
 * Function:	H5O_stab_encode
 *
 * Purpose:	Encodes a symbol table message.
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
H5O_stab_encode (hdf5_file_t *f, size_t raw_size, uint8 *p, const void *_mesg)
{
   const H5O_stab_t	*stab = (const H5O_stab_t *)_mesg;

   FUNC_ENTER (H5O_stab_encode, NULL, FAIL);

   /* check args */
   assert (f);
   assert (raw_size == 2 * H5F_SIZEOF_OFFSET(f));
   assert (p);
   assert (stab);

   /* encode */
   H5F_encode_offset (f, p, stab->btree_addr);
   H5F_encode_offset (f, p, stab->heap_addr);

   FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5O_stab_fast
 *
 * Purpose:	Initializes a new message struct with info from a symbol
 *		table entry.
 *
 * Return:	Success:	Ptr to message struct, allocated if none
 *				supplied.
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
H5O_stab_fast (const H5G_entry_t *ent, void *_mesg)
{
   H5O_stab_t	*stab = (H5O_stab_t *)_mesg;
   
   FUNC_ENTER (H5O_stab_fast, NULL, NULL);

   /* check args */
   assert (ent);

   if (H5G_CACHED_STAB==ent->type) {
      if (!stab) stab = H5MM_xcalloc (1, sizeof(H5O_stab_t));
      stab->btree_addr = ent->cache.stab.btree_addr;
      stab->heap_addr = ent->cache.stab.heap_addr;
   } else {
      stab = NULL;
   }

   FUNC_LEAVE (stab);
}


/*-------------------------------------------------------------------------
 * Function:	H5O_stab_cache
 *
 * Purpose:	Copies a message into the cache portion of a symbol table
 *		entry.
 *
 * Return:	Success:	TRUE if modified.
 *				FALSE if not modified.
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
static hbool_t
H5O_stab_cache (H5G_entry_t *ent, const void *_mesg)
{
   const H5O_stab_t	*stab = (const H5O_stab_t *)_mesg;
   hbool_t		modified = FALSE;
   
   FUNC_ENTER (H5O_stab_cache, NULL, FAIL);

   /* check args */
   assert (ent);
   assert (stab);

   /*
    * We do this in two steps so Purify doesn't complain about
    * uninitialized memory reads even though they don't bother
    * anything.
    */
   if (H5G_CACHED_STAB != ent->type) {
      modified = TRUE;
      ent->type = H5G_CACHED_STAB;
      ent->cache.stab.btree_addr = stab->btree_addr;
      ent->cache.stab.heap_addr = stab->heap_addr;
   } else {
      if (ent->cache.stab.btree_addr != stab->btree_addr) {
	 modified = TRUE;
	 ent->cache.stab.btree_addr = stab->btree_addr;
      }

      if (ent->cache.stab.heap_addr != stab->heap_addr) {
	 modified = TRUE;
	 ent->cache.stab.heap_addr = stab->heap_addr;
      }
   }

   FUNC_LEAVE (modified);
}


/*-------------------------------------------------------------------------
 * Function:	H5O_stab_copy
 *
 * Purpose:	Copies a message from _MESG to _DEST, allocating _DEST if
 *		necessary.
 *
 * Return:	Success:	Ptr to _DEST
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
H5O_stab_copy (const void *_mesg, void *_dest)
{
   const H5O_stab_t	*stab = (const H5O_stab_t *)_mesg;
   H5O_stab_t		*dest = (H5O_stab_t *)_dest;
   
   FUNC_ENTER (H5O_stab_copy, NULL, NULL);

   /* check args */
   assert (stab);
   if (!dest) dest = H5MM_xcalloc (1, sizeof(H5O_stab_t));

   /* copy */
   *dest = *stab;

   FUNC_LEAVE ((void*)dest);
}


/*-------------------------------------------------------------------------
 * Function:	H5O_stab_size
 *
 * Purpose:	Returns the size of the raw message in bytes not counting
 *		the message type or size fields, but only the data fields.
 *		This function doesn't take into account alignment.
 *
 * Return:	Success:	Message data size in bytes without alignment.
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
static size_t
H5O_stab_size (hdf5_file_t *f, const void *_mesg)
{
   FUNC_ENTER (H5O_stab_size, NULL, FAIL);
   FUNC_LEAVE (2 * H5F_SIZEOF_OFFSET(f));
}


/*-------------------------------------------------------------------------
 * Function:	H5O_stab_debug
 *
 * Purpose:	Prints debugging info for a symbol table message.
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
H5O_stab_debug (hdf5_file_t *f, const void *_mesg, FILE *stream,
		intn indent, intn fwidth)
{
   const H5O_stab_t	*stab = (const H5O_stab_t *)_mesg;
   
   FUNC_ENTER (H5O_stab_debug, NULL, FAIL);

   /* check args */
   assert (f);
   assert (stab);
   assert (stream);
   assert (indent>=0);
   assert (fwidth>=0);

   fprintf (stream, "%*s%-*s %lu\n", indent, "", fwidth,
	    "B-tree address:",
	    (unsigned long)(stab->btree_addr));
   fprintf (stream, "%*s%-*s %lu\n", indent, "", fwidth,
	    "Name heap address:",
	    (unsigned long)(stab->heap_addr));

   FUNC_LEAVE (SUCCEED);
}
	    
