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
#define INTERFACE_INIT	NULL


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
   H5G_entry_t *ent;
   
   ent = H5MM_xcalloc (1, sizeof(H5G_entry_t));
   H5F_addr_undef (&(ent->header));
   return ent;
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
   FUNC_ENTER (H5G_ent_invalidate, FAIL);
   
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
 * Return:	Success:	SUCCED with the address returned in the ADDR
 *				argument.
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
H5G_ent_addr (H5G_entry_t *ent, haddr_t *addr/*out*/)
{
   FUNC_ENTER (H5G_ent_addr, FAIL);
   
   assert (ent);
   *addr = ent->header;
   
   FUNC_LEAVE (SUCCEED);
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
   FUNC_ENTER (H5G_ent_cache, NULL);
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
   FUNC_ENTER (H5G_ent_modified, FAIL);
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
H5G_ent_decode_vec (H5F_t *f, const uint8 **pp, H5G_entry_t *ent, intn n)
{
   intn		i;

   FUNC_ENTER (H5G_ent_decode_vec, FAIL);

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
H5G_ent_decode (H5F_t *f, const uint8 **pp, H5G_entry_t *ent)
{
   const uint8	*p_ret = *pp;

   FUNC_ENTER (H5G_ent_decode, FAIL);

   /* check arguments */
   assert (f);
   assert (pp);
   assert (ent);

   /* decode header */
   H5F_decode_length (f, *pp, ent->name_off);
   H5F_addr_decode (f, pp, &(ent->header));
   UINT32DECODE (*pp, ent->type);

   /* decode scratch-pad */
   switch (ent->type) {
   case H5G_NOTHING_CACHED:
      break;

   case H5G_CACHED_SDSPACE:
      assert (5*4 <= H5G_SIZEOF_SCRATCH);
      UINT32DECODE (*pp, ent->cache.sdspace.ndim);
      UINT32DECODE (*pp, ent->cache.sdspace.dim[0]);
      UINT32DECODE (*pp, ent->cache.sdspace.dim[1]);
      UINT32DECODE (*pp, ent->cache.sdspace.dim[2]);
      UINT32DECODE (*pp, ent->cache.sdspace.dim[3]);
      break;

   case H5G_CACHED_STAB:
      assert (2*H5F_SIZEOF_ADDR (f) <= H5G_SIZEOF_SCRATCH);
      H5F_addr_decode (f, pp, &(ent->cache.stab.btree_addr));
      H5F_addr_decode (f, pp, &(ent->cache.stab.heap_addr));
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

   FUNC_ENTER (H5G_ent_encode_vec, FAIL);

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

   FUNC_ENTER (H5G_ent_encode, FAIL);

   /* check arguments */
   assert (f);
   assert (pp);
   assert (ent);

   /* encode header */
   H5F_encode_length (f, *pp, ent->name_off);
   H5F_addr_encode (f, pp, &(ent->header));
   UINT32ENCODE (*pp, ent->type);

   /* encode scratch-pad */
   switch (ent->type) {
   case H5G_NOTHING_CACHED:
      break;

   case H5G_CACHED_SDSPACE:
      assert (5*4 <= H5G_SIZEOF_SCRATCH);
      UINT32ENCODE (*pp, ent->cache.sdspace.ndim);
      UINT32ENCODE (*pp, ent->cache.sdspace.dim[0]);
      UINT32ENCODE (*pp, ent->cache.sdspace.dim[1]);
      UINT32ENCODE (*pp, ent->cache.sdspace.dim[2]);
      UINT32ENCODE (*pp, ent->cache.sdspace.dim[3]);
      break;

   case H5G_CACHED_STAB:
      assert (2*H5F_SIZEOF_ADDR (f) <= H5G_SIZEOF_SCRATCH);
      H5F_addr_encode (f, pp, &(ent->cache.stab.btree_addr));
      H5F_addr_encode (f, pp, &(ent->cache.stab.heap_addr));
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
 *		matzke@llnl.gov
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
   
   FUNC_ENTER (H5G_ent_debug, FAIL);

   fprintf (stream, "%*s%-*s %lu\n", indent, "", fwidth,
	    "Name offset into private heap:",
	    (unsigned long)(ent->name_off));
   
   fprintf (stream, "%*s%-*s ", indent, "", fwidth,
	    "Object header address:");
   H5F_addr_print (stream, &(ent->header));
   fprintf (stream, "\n");

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
	 
   case H5G_CACHED_SDSPACE:
      fprintf (stream, "Simple data space\n");
      fprintf (stream, "%*s%-*s %u\n", indent, "", fwidth,
	       "Dimensionality:",
	       (unsigned)(ent->cache.sdspace.ndim));
      for (i=0; i<ent->cache.sdspace.ndim && i<4; i++) {
	 sprintf (buf, "Dimension %d", i);
	 fprintf (stream, "%*s%-*s %u\n", indent, "", fwidth,
		  buf,
		  (unsigned)(ent->cache.sdspace.dim[i]));
      }
      break;
	 
   case H5G_CACHED_STAB:
      fprintf (stream, "Symbol Table\n");
      
      fprintf (stream, "%*s%-*s ", indent, "", fwidth,
	       "B-tree address:");
      H5F_addr_print (stream, &(ent->cache.stab.btree_addr));
      fprintf (stream, "\n");
      
      fprintf (stream, "%*s%-*s ", indent, "", fwidth,
	       "Heap address:");
      H5F_addr_print (stream, &(ent->cache.stab.heap_addr));
      fprintf (stream, "\n");
      break;

   default:
      fprintf (stream, "*** Unknown symbol type %d\n", ent->type);
      break;
   }

   FUNC_LEAVE (SUCCEED);
}
