/*-------------------------------------------------------------------------
 * Copyright (C) 1997	National Center for Supercomputing Applications.
 *                      All rights reserved.
 *
 *-------------------------------------------------------------------------
 *
 * Created:		H5G.c
 * 			Jul 18 1997
 * 			Robb Matzke <matzke@llnl.gov>
 *
 * Purpose:		Symbol table functions.  The functions that
 *			begin with `H5G_stab_' don't understand the
 *			directory hierarchy; they operate on a single
 *			symbol table at a time.
 *
 * 			The functions that begin with `H5G_node_' operate
 *			on the leaf nodes of a symbol table B-tree.  They
 *			should be defined in the H5Gnode.c file.
 *
 * 			The remaining functions know about the directory
 *			hierarchy.
 *
 * Modifications:
 *
 *	Robb Matzke, 5 Aug 1997
 *	Added calls to H5E.
 *
 *-------------------------------------------------------------------------
 */
#include <assert.h>

#include "hdf5.h"

#define H5G_INIT_HEAP		8192

/* Packages needed by this file... */
#include "H5private.h"
#include "H5Bprivate.h"
#include "H5Gprivate.h"
#include "H5Hprivate.h"
#include "H5MMprivate.h"
#include "H5Oprivate.h"

#define PABLO_MASK	H5G_mask


/* Is the interface initialized? */
static intn interface_initialize_g = FALSE;


/*-------------------------------------------------------------------------
 * Function:	H5G_stab_new
 *
 * Purpose:	Creates a new empty symbol table (object header, name heap,
 *		and B-tree).  The caller can specify an initial size for the
 *		name heap.  If no size is specified then a default heap is
 *		created when the first symbol is added to the table.
 *
 * 		The B-tree is created when the first symbol is added to
 *		the table.
 *
 * 		In order for the B-tree to operate correctly, the first
 *		item in the heap is the empty string, and must appear at
 *		heap offset zero.
 *
 * Return:	Success:	Address of new symbol table header.  If
 *				the caller supplies a symbol table entry
 *				SELF then it will be initialized to point to
 *				this symbol table.
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug  1 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
haddr_t
H5G_stab_new (hdf5_file_t *f, H5G_entry_t *self, size_t init)
{
   off_t	name;				/*offset of "" name	*/
   haddr_t	addr;				/*object header address	*/
   H5O_stab_t	stab;				/*symbol table message	*/

   FUNC_ENTER (H5G_stab_new, NULL, FAIL);

   /*
    * Check arguments.
    */
   assert (f);

   /* Create symbol table private heap */
   if (init>0) {
      if ((stab.heap = H5H_new (f, H5H_LOCAL, init))<0) {
	 HRETURN_ERROR (H5E_SYM, H5E_CANTINIT, FAIL);
      }
      if ((name = H5H_insert (f, stab.heap, 1, "")<0)) {
	 HRETURN_ERROR (H5E_SYM, H5E_CANTINIT, FAIL);
      }
      if (0!=name) {
	 /*
	  * B-tree's won't work if the first name isn't at the beginning
	  * of the heap.
	  */
	 HRETURN_ERROR (H5E_INTERNAL, H5E_CANTINIT, FAIL);
      }
   } else {
      stab.heap = 0; /*we'll create it later*/
   }

   /* The B-tree is created on demand later */
   stab.btree = 0;

   /* Create symbol table object header with a single link */
   if ((addr = H5O_new (f, 1, 4+2*H5F_SIZEOF_OFFSET(f)))<0) {
      HRETURN_ERROR (H5E_SYM, H5E_CANTINIT, FAIL);
   }
   if (self) {
      self->name_off = 0;
      self->header = addr;
      self->type = H5G_NOTHING_CACHED;
   }

   /* insert the symbol table message */
   if (H5O_modify(f, addr, self, NULL, H5O_STAB, H5O_NEW_MESG, &stab)<0) {
      HRETURN_ERROR (H5E_SYM, H5E_CANTINIT, FAIL);
   }

   FUNC_LEAVE (addr);
}


/*-------------------------------------------------------------------------
 * Function:	H5G_stab_find
 *
 * Purpose:	Finds a symbol named NAME in the symbol table whose
 *		description is stored in SELF in file F and returns a
 *		copy of the symbol table entry through the ENT argument.
 *
 * Return:	Success:	Address corresponding to the name.
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug  1 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
haddr_t
H5G_stab_find (hdf5_file_t *f, H5G_entry_t *self, const char *name,
	       H5G_entry_t *ent)
{
   H5G_node_ud1_t       udata;		/*data to pass through B-tree	*/
   H5O_stab_t		stab;		/*symbol table message		*/

   FUNC_ENTER (H5G_stab_find, NULL, FAIL);

   /* Check arguments */
   assert (f);
   assert (self && self->header>=0);
   assert (name && *name);
   assert (ent);

   /* set up the udata */
   if (NULL==H5O_read (f, self->header, self, H5O_STAB, 0, &stab)) {
      HRETURN_ERROR (H5E_SYM, H5E_BADMESG, FAIL);
   }
   if (stab.btree<=0 || stab.heap<=0) {
      HRETURN_ERROR (H5E_SYM, H5E_NOTFOUND, FAIL); /*empty symbol table*/
   }
   udata.operation = H5G_OPER_FIND;
   udata.name = name;
   udata.heap = stab.heap;

   /* search the B-tree */
   if (H5B_find (f, H5B_SNODE, stab.btree, &udata)<0) {
      HRETURN_ERROR (H5E_SYM, H5E_NOTFOUND, FAIL);
   }

   /* return the result */
   if (ent) *ent = udata.entry;
   FUNC_LEAVE (udata.entry.header);
}


/*-------------------------------------------------------------------------
 * Function:	H5G_stab_modify
 *
 * Purpose:	Modifies the entry for an existing symbol.  The name of the
 *		symbol is NAME in the symbol table described by SELF in
 *		file F.  ENT is the new symbol table entry to use for the
 *		symbol.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug  1 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_stab_modify (hdf5_file_t *f, H5G_entry_t *self, const char *name,
		 H5G_entry_t *ent)
{
   H5G_node_ud1_t	udata;		/*data to pass through B-tree	*/
   H5O_stab_t		stab;		/*symbol table message		*/

   FUNC_ENTER (H5G_stab_modify, NULL, FAIL);

   /* check arguments */
   assert (f);
   assert (self && self->header>=0);
   assert (name && *name);
   assert (ent);

   /* set up the udata */
   if (NULL==H5O_read (f, self->header, self, H5O_STAB, 0, &stab)) {
      HRETURN_ERROR (H5E_SYM, H5E_BADMESG, FAIL);
   }
   if (stab.btree<=0 || stab.heap<=0) {
      HRETURN_ERROR (H5E_SYM, H5E_NOTFOUND, FAIL); /*empty symbol table*/
   }
   udata.operation = H5G_OPER_MODIFY;
   udata.name = name;
   udata.heap = stab.heap;
   udata.entry = *ent;

   /* search and modify the B-tree */
   if (H5B_find (f, H5B_SNODE, stab.btree, &udata)<0) {
      HRETURN_ERROR (H5E_SYM, H5E_NOTFOUND, FAIL);
   }

   FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5G_stab_insert
 *
 * Purpose:	Insert a new symbol into the table described by SELF in
 *		file F.  The name of the new symbol is NAME and its symbol
 *		table entry is ENT.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug  1 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_stab_insert (hdf5_file_t *f, H5G_entry_t *self, const char *name,
		 H5G_entry_t *ent)
{
   H5O_stab_t		stab;		/*symbol table message		*/
   H5G_node_ud1_t	udata;		/*data to pass through B-tree	*/
   off_t		offset;		/*offset of name within heap	*/

   FUNC_ENTER (H5G_stab_insert, NULL, FAIL);

   /* check arguments */
   assert (f);
   assert (self && self->header>=0);
   assert (name && *name);
   assert (ent);
   
   /* make sure we have a B-tree and a heap */
   if (NULL==H5O_read (f, self->header, self, H5O_STAB, 0, &stab)) {
      HRETURN_ERROR (H5E_SYM, H5E_BADMESG, FAIL);
   }
   if (stab.btree<=0 || stab.heap<=0) {
      if (stab.btree<=0 &&
	  (stab.btree = H5B_new (f, H5B_SNODE, sizeof(H5G_node_key_t)))<0) {
	 HRETURN_ERROR (H5E_SYM, H5E_CANTINIT, FAIL);
      }
      if (stab.heap<=0) {
	 stab.heap = H5H_new (f, H5H_LOCAL,
			      MAX(strlen(name)+1, H5G_INIT_HEAP));
	 if (stab.heap<0) {
	    HRETURN_ERROR (H5E_SYM, H5E_CANTINIT, FAIL);
	 }
	 if (0!=(offset = H5H_insert (f, stab.heap, 1, ""))) {
	    HRETURN_ERROR (H5E_INTERNAL, H5E_CANTINIT, FAIL);
	 }
      }
      if (H5O_modify (f, self->header, self, NULL, H5O_STAB, 0, &stab)<0) {
	 HRETURN_ERROR (H5E_SYM, H5E_CANTINIT, FAIL);
      }
   }

   /* initialize data to pass through B-tree */
   udata.name = name;
   udata.heap = stab.heap;
   udata.entry = *ent;
   if (H5B_insert (f, H5B_SNODE, stab.btree, &udata)<0) {
      HRETURN_ERROR (H5E_SYM, H5E_CANTINSERT, FAIL);
   }

   FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5G_stab_list
 *
 * Purpose:	Returns a list of all the symbols in a symbol table.
 *		The caller allocates an array of pointers which this
 *		function will fill in with malloc'd names.  The caller
 *		also allocates an array of symbol table entries which will
 *		be filled in with data from the symbol table.  Each of these
 *		arrays should have at least MAXENTRIES elements.
 *
 * Return:	Success:	The total number of symbols in the
 *				symbol table.  This may exceed MAXENTRIES,
 *				but at most MAXENTRIES values are copied
 *				into the NAMES and ENTRIES arrays.
 *
 *		Failure:	FAIL, the pointers in NAMES are undefined but
 *			 	no memory is allocated.  The values in
 *				ENTRIES are undefined.
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug  1 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
intn
H5G_stab_list (hdf5_file_t *f, H5G_entry_t *self, intn maxentries,
	       char *names[], H5G_entry_t entries[])
{
   H5G_node_list_t	udata;
   H5O_stab_t		stab;
   intn			i;

   FUNC_ENTER (H5G_stab_list, NULL, FAIL);

   /* check args */
   assert (f);
   assert (self && self->header>=0);
   assert (maxentries>=0);

   if (NULL==H5O_read (f, self->header, self, H5O_STAB, 0, &stab)) {
      HRETURN_ERROR (H5E_SYM, H5E_BADMESG, FAIL);
   }
   if (stab.btree<=0 || stab.heap<=0) HRETURN (0); /*empty directory*/

   udata.entry = entries;
   udata.name = names;
   udata.heap = stab.heap;
   udata.maxentries = maxentries;
   udata.nsyms = 0;

   if (names) HDmemset (names, 0, maxentries);
   if (H5B_list (f, H5B_SNODE, stab.btree, &udata)<0) {
      if (names) {
	 for (i=0; i<maxentries; i++) H5MM_xfree (names[i]);
      }
      HRETURN_ERROR (H5E_SYM, H5E_CANTLIST, FAIL);
   }

   FUNC_LEAVE (udata.nsyms);
}
   

/*-------------------------------------------------------------------------
 * Function:	H5G_decode_vec
 *
 * Purpose:	Same as H5G_decode() except it does it for an array of
 *		symbol table entries.
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
H5G_decode_vec (hdf5_file_t *f, uint8 **pp, H5G_entry_t *ent, intn n)
{
   intn		i;

   FUNC_ENTER (H5G_decode_vec, NULL, FAIL);

   /* check arguments */
   assert (f);
   assert (pp);
   assert (ent);
   assert (n>=0);

   /* decode entries */
   for (i=0; i<n; i++) {
      if (H5G_decode (f, pp, ent+i)<0) {
	 HRETURN_ERROR (H5E_SYM, H5E_CANTDECODE, FAIL);
      }
   }

   FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5G_decode
 *
 * Purpose:	Decodes a symbol table entry pointed to by `*pp'.
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
H5G_decode (hdf5_file_t *f, uint8 **pp, H5G_entry_t *ent)
{
   uint8	*p_ret = *pp;

   FUNC_ENTER (H5G_decode, NULL, FAIL);

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
      UINT32DECODE (*pp, ent->cache.sdata.nt);
      UINT32DECODE (*pp, ent->cache.sdata.ndim);
      UINT32DECODE (*pp, ent->cache.sdata.dim[0]);
      UINT32DECODE (*pp, ent->cache.sdata.dim[1]);
      UINT32DECODE (*pp, ent->cache.sdata.dim[2]);
      UINT32DECODE (*pp, ent->cache.sdata.dim[3]);
      break;

   case H5G_CACHED_STAB:
      UINT32DECODE (*pp, ent->cache.stab.btree);
      UINT32DECODE (*pp, ent->cache.stab.heap);
      break;

   default:
      abort();
   }

   *pp = p_ret + H5G_SIZEOF_ENTRY(f);
   FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5G_encode_vec
 *
 * Purpose:	Same as H5G_encode() except it does it for an array of
 *		symbol table entries.
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
H5G_encode_vec (hdf5_file_t *f, uint8 **pp, H5G_entry_t *ent, intn n)
{
   intn		i;

   FUNC_ENTER (H5G_encode_vec, NULL, FAIL);

   /* check arguments */
   assert (f);
   assert (pp);
   assert (ent);
   assert (n>=0);

   /* encode entries */
   for (i=0; i<n; i++) {
      if (H5G_encode (f, pp, ent+i)<0) {
	 HRETURN_ERROR (H5E_SYM, H5E_CANTENCODE, FAIL);
      }
   }

   FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5G_encode
 *
 * Purpose:	Encodes the specified symbol table entry into the buffer
 *		pointed to by *pp.
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
H5G_encode (hdf5_file_t *f, uint8 **pp, H5G_entry_t *ent)
{
   uint8	*p_ret = *pp + H5G_SIZEOF_ENTRY(f);

   FUNC_ENTER (H5G_encode, NULL, FAIL);

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
      UINT32ENCODE (*pp, ent->cache.sdata.nt);
      UINT32ENCODE (*pp, ent->cache.sdata.ndim);
      UINT32ENCODE (*pp, ent->cache.sdata.dim[0]);
      UINT32ENCODE (*pp, ent->cache.sdata.dim[1]);
      UINT32ENCODE (*pp, ent->cache.sdata.dim[2]);
      UINT32ENCODE (*pp, ent->cache.sdata.dim[3]);
      break;

   case H5G_CACHED_STAB:
      UINT32ENCODE (*pp, ent->cache.stab.btree);
      UINT32ENCODE (*pp, ent->cache.stab.heap);
      break;

   default:
      abort();
   }

   /* fill with zero */
   while (*pp<p_ret) *(*pp)++ = 0;
   
   *pp = p_ret;
   FUNC_LEAVE (SUCCEED);
}


