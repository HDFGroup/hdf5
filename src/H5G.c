/*-------------------------------------------------------------------------
 * Copyright (C) 1997	National Center for Supercomputing Applications.
 *                      All rights reserved.
 *
 *-------------------------------------------------------------------------
 *
 * Created:		H5G.c
 * 			Jul 18 1997
 * 			Robb Matzke <robb@maya.nuance.com>
 *
 * Purpose:		
 *
 * Modifications:	
 *
 *-------------------------------------------------------------------------
 */
#include <assert.h>

#include "hdf5.h"

/* Packages needed by this file... */
#include "H5Gprivate.h"

/* PRIVATE PROTOTYPES */


/*-------------------------------------------------------------------------
 * Function:	H5G_decode_vec
 *
 * Purpose:	Same as H5G_decode() except it does it for an array of
 *		symbol table entries.
 *
 * Return:	Success:	0, with *pp pointing to the first byte
 *				after the last symbol.
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *		robb@maya.nuance.com
 *		Jul 18 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_decode_vec (hdf5_file_t *f, uint8 **pp, H5G_entry_t *ent, intn n)
{
   intn		i;

   for (i=0; i<n; i++) {
      if (H5G_decode (f, pp, ent+i)<0) return -1;
   }
   return 0;
}


/*-------------------------------------------------------------------------
 * Function:	H5G_decode
 *
 * Purpose:	Decodes a symbol table entry pointed to by `*pp'.
 *
 * Return:	Success:	0 with *pp pointing to the first byte
 *				following the symbol table entry.
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *		robb@maya.nuance.com
 *		Jul 18 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_decode (hdf5_file_t *f, uint8 **pp, H5G_entry_t *ent)
{
   uint8	*p_ret = *pp;
   
   H5F_decode_offset (f, *pp, ent->name_off);
   H5F_decode_offset (f, *pp, ent->header);
   UINT32DECODE (*pp, ent->type);

   switch (ent->type) {
   case H5G_NOTHING_CACHED:
      break;

   case H5G_CACHED_SDATA:
      UINT32DECODE (*pp, ent->cache.sdata.nt);
      UINT32DECODE (*pp, ent->cache.sdata.ndim);
      UINT32DECODE (*pp, ent->cache.sdata.dim[0]);
      UINT32DECODE (*pp, ent->cache.sdata.dim[1]);
      UINT32DECODE (*pp, ent->cache.sdata.dim[2]);
      UINT32DECODE (*pp, ent->cache.sdata.dim[3]);
      break;

   case H5G_CACHED_SYMTAB:
      UINT32DECODE (*pp, ent->cache.symtab.btree);
      UINT32DECODE (*pp, ent->cache.symtab.heap);
      break;

   default:
      abort();
   }

   *pp = p_ret + 2*SIZEOF_OFFSET(f) + 4 + 24;
   return 0;
}


/*-------------------------------------------------------------------------
 * Function:	H5G_encode_vec
 *
 * Purpose:	Same as H5G_encode() except it does it for an array of
 *		symbol table entries.
 *
 * Return:	Success:	0, with *pp pointing to the first byte
 *				after the last symbol.
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *		robb@maya.nuance.com
 *		Jul 18 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_encode_vec (hdf5_file_t *f, uint8 **pp, H5G_entry_t *ent, intn n)
{
   intn		i;

   for (i=0; i<n; i++) {
      if (H5G_encode (f, pp, ent+i)<0) return -1;
   }
   return 0;
}


/*-------------------------------------------------------------------------
 * Function:	H5G_encode
 *
 * Purpose:	Encodes the specified symbol table entry into the buffer
 *		pointed to by *pp.
 *
 * Return:	Success:	0, with *pp pointing to the first byte
 *				after the symbol table entry.
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *		robb@maya.nuance.com
 *		Jul 18 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_encode (hdf5_file_t *f, uint8 **pp, H5G_entry_t *ent)
{
   uint8	*p_ret = *pp;

   H5F_encode_offset (f, *pp, ent->name_off);
   H5F_encode_offset (f, *pp, ent->header);
   UINT32ENCODE (*pp, ent->type);

   switch (ent->type) {
   case H5G_NOTHING_CACHED:
      break;

   case H5G_CACHED_SDATA:
      UINT32ENCODE (*pp, ent->cache.sdata.nt);
      UINT32ENCODE (*pp, ent->cache.sdata.ndim);
      UINT32ENCODE (*pp, ent->cache.sdata.dim[0]);
      UINT32ENCODE (*pp, ent->cache.sdata.dim[1]);
      UINT32ENCODE (*pp, ent->cache.sdata.dim[2]);
      UINT32ENCODE (*pp, ent->cache.sdata.dim[3]);
      break;

   case H5G_CACHED_SYMTAB:
      UINT32ENCODE (*pp, ent->cache.symtab.btree);
      UINT32ENCODE (*pp, ent->cache.symtab.heap);
      break;

   default:
      abort();
   }

   *pp = p_ret + 2*SIZEOF_OFFSET(f) + 4 + 24;
   return 0;
}

