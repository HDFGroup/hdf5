/*
 * Copyright (C) 1997 National Center for Supercomputing Applications.
 *                    All rights reserved.
 *
 * Programmer: Robb Matzke <matzke@llnl.gov>
 *             Friday, September 19, 1997
 */
#define H5G_PACKAGE

#include <H5private.h>
#include <H5Eprivate.h>
#include <H5Gpkg.h>
#include <H5MMprivate.h>

#define PABLO_MASK	H5G_ent_mask

static hbool_t interface_initialize_g = FALSE;


/*-------------------------------------------------------------------------
 * Function:	H5G_ent_calloc
 *
 * Purpose:	Returns a pointer to a malloc'd, zeroed symbol table entry.
 *
 * Return:	Success:	Ptr to entry
 *
 *		Failure:	never fails
 *
 * Programmer:	Robb Matzke
 *              Friday, September 19, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
H5G_entry_t *
H5G_ent_calloc (void)
{
   return H5MM_xcalloc (1, sizeof(H5G_entry_t));
}



/*-------------------------------------------------------------------------
 * Function:	H5G_ent_invalidate
 *
 * Purpose:	Invalidates the cache in a symbol table entry.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Friday, September 19, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_ent_invalidate (H5G_entry_t *ent)
{
   FUNC_ENTER (H5G_ent_invalidate, NULL, FAIL);
   
   if (ent && H5G_NOTHING_CACHED!=ent->type) {
      ent->dirty = TRUE;
      ent->type = H5G_NOTHING_CACHED;
   }

   FUNC_LEAVE (SUCCEED);
}



/*-------------------------------------------------------------------------
 * Function:	H5G_ent_addr
 *
 * Purpose:	Returns the header address associated with a symbol table
 *		entry.
 *
 * Return:	Success:	Address
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Friday, September 19, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
haddr_t
H5G_ent_addr (H5G_entry_t *ent)
{
   FUNC_ENTER (H5G_ent_addr, NULL, FAIL);
   assert (ent);
   
   FUNC_LEAVE (ent->header);
}


/*-------------------------------------------------------------------------
 * Function:	H5G_ent_cache
 *
 * Purpose:	Returns a pointer to the cache associated with the symbol
 *		table entry.  You should modify the cache directly, then call
 *		H5G_modified() with the new cache type (even if the type is
 *		still the same).
 *
 * Return:	Success:	Ptr to the cache in the symbol table entry.
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *              Friday, September 19, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
H5G_cache_t *
H5G_ent_cache (H5G_entry_t *ent, H5G_type_t *cache_type)
{
   FUNC_ENTER (H5G_ent_cache, NULL, NULL);
   if (!ent) {
      HRETURN_ERROR (H5E_SYM, H5E_BADVALUE, NULL);
   }
   if (cache_type) *cache_type = ent->type;
   
   FUNC_LEAVE (&(ent->cache));
}



/*-------------------------------------------------------------------------
 * Function:	H5G_ent_modified
 *
 * Purpose:	This function should be called after you make any
 *		modifications to a symbol table entry cache.  Supply the new
 *		type for the cache.  If CACHE_TYPE is the constant
 *		H5G_NO_CHANGE then the cache type isn't changed--just the
 *		dirty bit is set.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Friday, September 19, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_ent_modified (H5G_entry_t *ent, H5G_type_t cache_type)
{
   FUNC_ENTER (H5G_ent_modified, NULL, FAIL);
   assert (ent);
   if (H5G_NO_CHANGE!=ent->type) ent->type = cache_type;
   ent->dirty = TRUE;
   FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5G_ent_decode_vec
 *
 * Purpose:	Same as H5G_ent_decode() except it does it for an array of
 *		symbol table entries.
 *
 * Errors:
 *		SYM       CANTDECODE    Can't decode. 
 *
 * Return:	Success:	SUCCEED, with *pp pointing to the first byte
 *				after the last symbol.
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Jul 18 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_ent_decode_vec (H5F_t *f, uint8 **pp, H5G_entry_t *ent, intn n)
{
   intn		i;

   FUNC_ENTER (H5G_ent_decode_vec, NULL, FAIL);

   /* check arguments */
   assert (f);
   assert (pp);
   assert (ent);
   assert (n>=0);

   /* decode entries */
   for (i=0; i<n; i++) {
      if (H5G_ent_decode (f, pp, ent+i)<0) {
	 HRETURN_ERROR (H5E_SYM, H5E_CANTDECODE, FAIL); /*can't decode*/
      }
   }

   FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5G_ent_decode
 *
 * Purpose:	Decodes a symbol table entry pointed to by `*pp'.
 *
 * Errors:
 *
 * Return:	Success:	SUCCEED with *pp pointing to the first byte
 *				following the symbol table entry.
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Jul 18 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_ent_decode (H5F_t *f, uint8 **pp, H5G_entry_t *ent)
{
   uint8	*p_ret = *pp;

   FUNC_ENTER (H5G_ent_decode, NULL, FAIL);

   /* check arguments */
   assert (f);
   assert (pp);
   assert (ent);

   /* decode header */
   H5F_decode_offset (f, *pp, ent->name_off);
   H5F_decode_offset (f, *pp, ent->header);
   UINT32DECODE (*pp, ent->type);

   /* decode scratch-pad */
   switch (ent->type) {
   case H5G_NOTHING_CACHED:
      break;

   case H5G_CACHED_SDATA:
      ent->cache.sdata.nt.length= *(*pp)++;
      ent->cache.sdata.nt.arch= *(*pp)++;
      UINT16DECODE (*pp, ent->cache.sdata.nt.type);
      UINT32DECODE (*pp, ent->cache.sdata.ndim);
      UINT32DECODE (*pp, ent->cache.sdata.dim[0]);
      UINT32DECODE (*pp, ent->cache.sdata.dim[1]);
      UINT32DECODE (*pp, ent->cache.sdata.dim[2]);
      UINT32DECODE (*pp, ent->cache.sdata.dim[3]);
      break;

   case H5G_CACHED_STAB:
      UINT32DECODE (*pp, ent->cache.stab.btree_addr);
      UINT32DECODE (*pp, ent->cache.stab.heap_addr);
      break;

   default:
      HDabort();
   }

   *pp = p_ret + H5G_SIZEOF_ENTRY(f);
   FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5G_ent_encode_vec
 *
 * Purpose:	Same as H5G_ent_encode() except it does it for an array of
 *		symbol table entries.
 *
 * Errors:
 *		SYM       CANTENCODE    Can't encode. 
 *
 * Return:	Success:	SUCCEED, with *pp pointing to the first byte
 *				after the last symbol.
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Jul 18 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_ent_encode_vec (H5F_t *f, uint8 **pp, H5G_entry_t *ent, intn n)
{
   intn		i;

   FUNC_ENTER (H5G_ent_encode_vec, NULL, FAIL);

   /* check arguments */
   assert (f);
   assert (pp);
   assert (ent);
   assert (n>=0);

   /* encode entries */
   for (i=0; i<n; i++) {
      if (H5G_ent_encode (f, pp, ent+i)<0) {
	 HRETURN_ERROR (H5E_SYM, H5E_CANTENCODE, FAIL); /*can't encode*/
      }
   }

   FUNC_LEAVE (SUCCEED);
}



/*-------------------------------------------------------------------------
 * Function:	H5G_ent_encode
 *
 * Purpose:	Encodes the specified symbol table entry into the buffer
 *		pointed to by *pp.
 *
 * Errors:
 *
 * Return:	Success:	SUCCEED, with *pp pointing to the first byte
 *				after the symbol table entry.
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Jul 18 1997
 *
 * Modifications:
 *
 * 	Robb Matzke, 8 Aug 1997
 *	Writes zeros for the bytes that aren't used so the file doesn't
 *	contain junk.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_ent_encode (H5F_t *f, uint8 **pp, H5G_entry_t *ent)
{
   uint8	*p_ret = *pp + H5G_SIZEOF_ENTRY(f);

   FUNC_ENTER (H5G_ent_encode, NULL, FAIL);

   /* check arguments */
   assert (f);
   assert (pp);
   assert (ent);

   /* encode header */
   H5F_encode_offset (f, *pp, ent->name_off);
   H5F_encode_offset (f, *pp, ent->header);
   UINT32ENCODE (*pp, ent->type);

   /* encode scratch-pad */
   switch (ent->type) {
   case H5G_NOTHING_CACHED:
      break;

   case H5G_CACHED_SDATA:
      *(*pp)++= ent->cache.sdata.nt.length;
      *(*pp)++= ent->cache.sdata.nt.arch;
      UINT16ENCODE (*pp, ent->cache.sdata.nt.type);
      UINT32ENCODE (*pp, ent->cache.sdata.ndim);
      UINT32ENCODE (*pp, ent->cache.sdata.dim[0]);
      UINT32ENCODE (*pp, ent->cache.sdata.dim[1]);
      UINT32ENCODE (*pp, ent->cache.sdata.dim[2]);
      UINT32ENCODE (*pp, ent->cache.sdata.dim[3]);
      break;

   case H5G_CACHED_STAB:
      UINT32ENCODE (*pp, ent->cache.stab.btree_addr);
      UINT32ENCODE (*pp, ent->cache.stab.heap_addr);
      break;

   default:
      HDabort();
   }

   /* fill with zero */
   while (*pp<p_ret) *(*pp)++ = 0;
   
   *pp = p_ret;
   FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5G_ent_debug
 *
 * Purpose:	Prints debugging information about a symbol table entry.
 *
 * Errors:
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		robb@maya.nuance.com
 *		Aug 29 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_ent_debug (H5F_t *f, H5G_entry_t *ent, FILE *stream, intn indent,
	       intn fwidth)
{
   int		i;
   char		buf[64];
   
   FUNC_ENTER (H5G_ent_debug, NULL, FAIL);

   fprintf (stream, "%*s%-*s %lu\n", indent, "", fwidth,
	    "Name offset into private heap:",
	    (unsigned long)(ent->name_off));
   fprintf (stream, "%*s%-*s %lu\n", indent, "", fwidth,
	    "Object header address:",
	    (unsigned long)(ent->header));
   fprintf (stream, "%*s%-*s %s\n", indent, "", fwidth,
	    "Dirty:",
	    ent->dirty ? "Yes" : "No");
   fprintf (stream, "%*s%-*s %s\n", indent, "", fwidth,
	    "Has a shadow:",
	    H5G_shadow_p (ent)?"This is a shadow!" :
	       (ent->shadow ? "Yes" : "No"));
      
   fprintf (stream, "%*s%-*s ", indent, "", fwidth,
	    "Symbol type:");
   switch (ent->type) {
   case H5G_NOTHING_CACHED:
      fprintf (stream, "Nothing Cached\n");
      break;
	 
   case H5G_CACHED_SDATA:
      fprintf (stream, "S-data\n");
      fprintf (stream, "%*s%-*s %u\n", indent, "", fwidth,
	       "Number type length:",
	       (unsigned)(ent->cache.sdata.nt.length));
      fprintf (stream, "%*s%-*s %u\n", indent, "", fwidth,
	       "Number type architecture:",
	       (unsigned)(ent->cache.sdata.nt.arch));
      fprintf (stream, "%*s%-*s %u\n", indent, "", fwidth,
	       "Number type type:",
	       (unsigned)(ent->cache.sdata.nt.type));
      fprintf (stream, "%*s%-*s %u\n", indent, "", fwidth,
	       "Dimensionality:",
	       (unsigned)(ent->cache.sdata.ndim));
      for (i=0; i<ent->cache.sdata.ndim && i<4; i++) {
	 sprintf (buf, "Dimension %d", i);
	 fprintf (stream, "%*s%-*s %u\n", indent, "", fwidth,
		  buf,
		  (unsigned)(ent->cache.sdata.dim[i]));
      }
      break;
	 
   case H5G_CACHED_STAB:
      fprintf (stream, "Symbol Table\n");
      fprintf (stream, "%*s%-*s %lu\n", indent, "", fwidth,
	       "B-tree address:",
	       (unsigned long)(ent->cache.stab.btree_addr));
      fprintf (stream, "%*s%-*s %lu\n", indent, "", fwidth,
	       "Heap address:",
	       (unsigned long)(ent->cache.stab.heap_addr));
      break;

   default:
      fprintf (stream, "*** Unknown symbol type %d\n", ent->type);
      break;
   }

   FUNC_LEAVE (SUCCEED);
}
