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

#define H5G_INIT_HEAP		8192

/* Packages needed by this file... */
#include "H5Bprivate.h"
#include "H5Gprivate.h"
#include "H5Hprivate.h"
#include "H5MMprivate.h"

/* PRIVATE PROTOTYPES */


/*-------------------------------------------------------------------------
 * Function:	H5G_new
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
 * Return:	Success:	Address of new symbol table.
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *		robb@maya.nuance.com
 *		Aug  1 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
haddr_t
H5G_new (hdf5_file_t *f, size_t init)
{
   haddr_t	addr;				/*symtab object header	*/
   haddr_t	heap;				/*private heap		*/
   off_t	name;				/*offset of "" name	*/

   /* Create symbol table private heap */
   if (init>0) {
      if ((heap = H5H_new (f, init))<0) return -1;
      if ((name = H5H_insert (f, heap, 1, "")<0)) return -1;
      assert (0==name); /*or B-tree's won't work*/
   } else {
      heap = 0; /*we'll create it later*/
   }

   /* Create symbol table object header */
   addr = not_implemented_yet__create_object_header();
   if (addr<0) return -1;

   /* Insert the heap and B-tree addresses */
   not_implemented_yet__insert_symtab_message (f, addr, heap, 0);

   return addr;
}


/*-------------------------------------------------------------------------
 * Function:	H5G_find
 *
 * Purpose:	Finds a symbol named NAME in the symbol table whose address
 *		is ADDR and returns a copy of the symbol table entry through
 *		the ENTRY argument.
 *
 * Return:	Success:	Address corresponding to the name.
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *		robb@maya.nuance.com
 *		Aug  1 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
haddr_t
H5G_find (hdf5_file_t *f, haddr_t addr, const char *name, H5G_entry_t *entry)
{
   H5G_node_ud1_t       udata;		/*data to pass through B-tree	*/
   haddr_t		btree;		/*address of B-tree		*/

   /* set up the udata */
   not_implemented_yet__get_symtab_message (f, addr, &(udata.heap), &btree);
   if (btree<=0 || udata.heap<=0) return -1; /*empty symbol table*/
   udata.operation = H5G_OPER_FIND;
   udata.name = name;

   /* search the B-tree */
   if (H5B_find (f, H5B_SNODE, btree, &udata)<0) return -1;

   /* return the result */
   if (entry) *entry = udata.entry;
   return udata.entry.header;
}


/*-------------------------------------------------------------------------
 * Function:	H5G_modify
 *
 * Purpose:	Modifies the entry for an existing symbol.  The name of the
 *		symbol is NAME in the symbol table whose address is ADDR in
 *		file F.  ENTRY is the new symbol table entry to use for the
 *		symbol.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *		robb@maya.nuance.com
 *		Aug  1 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_modify (hdf5_file_t *f, haddr_t addr, const char *name, H5G_entry_t *entry)
{
   H5G_node_ud1_t	udata;		/*data to pass through B-tree	*/
   haddr_t		btree;		/*address of B-tree		*/

   /* set up the udata */
   not_implemented_yet__get_symtab_message (f, addr, &(udata.heap), &btree);
   if (btree<=0 || udata.heap<=0) return -1; /*empty symbol table*/
   udata.operation = H5G_OPER_MODIFY;
   udata.name = name;
   udata.entry = *entry;

   /* search and modify the B-tree */
   if (H5B_find (f, H5B_SNODE, btree, &udata)<0) return -1;
   return 0;
}


/*-------------------------------------------------------------------------
 * Function:	H5G_insert
 *
 * Purpose:	Insert a new symbol into the table whose address is ADDR in
 *		file F.  The name of the new symbol is NAME and its symbol
 *		table entry is ENTRY.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *		robb@maya.nuance.com
 *		Aug  1 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_insert (hdf5_file_t *f, haddr_t addr, const char *name, H5G_entry_t *entry)
{
   haddr_t		btree;		/*file address of B-tree	*/
   H5G_node_ud1_t	udata;		/*data to pass through B-tree	*/
   off_t		offset;		/*offset of name within heap	*/

   /* make sure we have a B-tree and a heap */
   not_implemented_yet__get_symtab_message (f, addr, &btree, &(udata.heap));
   if (btree<=0 || udata.heap<=0) {
      if (btree<=0 &&
	  (btree = H5B_new (f, H5B_SNODE, sizeof(H5G_node_key_t)))<0) {
	 return -1;
      }
      if (udata.heap<=0) {
	 udata.heap = H5H_new (f, MAX(strlen(name)+1, H5G_INIT_HEAP));
	 if (udata.heap<0) return -1;
	 if (0!=(offset = H5H_insert (f, udata.heap, 1, ""))) return -1;
      }
      not_implemented_yet__update_symtab_message (f, addr, udata.heap, btree);
   }

   /* initialize data to pass through B-tree */
   udata.name = name;
   udata.entry = *entry;
   if (H5B_insert (f, H5B_SNODE, btree, &udata)<0) return -1;

   return 0;
}


/*-------------------------------------------------------------------------
 * Function:	H5G_list
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
 *		Failure:	-1, the pointers in NAMES are undefined but
 *			 	no memory is allocated.  The values in
 *				ENTRIES are undefined.
 *
 * Programmer:	Robb Matzke
 *		robb@maya.nuance.com
 *		Aug  1 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
intn
H5G_list (hdf5_file_t *f, haddr_t addr, int maxentries,
	  char *names[], H5G_entry_t entries[])
{
   H5G_node_list_t	udata;
   haddr_t		btree;
   intn			i;
   
   not_implemented_yet__get_symtab_message (f, addr, &btree, &(udata.heap));
   if (btree<=0 || udata.heap<=0) return 0; /*empty directory*/

   udata.entry = entries;
   udata.name = names;
   udata.maxentries = maxentries;
   udata.nsyms = 0;

   if (names) HDmemset (names, 0, maxentries);
   if (H5B_list (f, H5B_SNODE, btree, &udata)<0) {
      if (names) {
	 for (i=0; i<maxentries; i++) H5MM_xfree (names[i]);
      }
      return -1;
   }

   return udata.nsyms;
}
   

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

   *pp = p_ret + H5G_SIZEOF_ENTRY(f);
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

   *pp = p_ret + H5G_SIZEOF_ENTRY(f);
   return 0;
}


herr_t not_implemented_yet__create_object_header (void) {return FAIL;}
herr_t not_implemented_yet__insert_symtab_message (hdf5_file_t* f, haddr_t addr, haddr_t heap, haddr_t btree) {return FAIL;}
herr_t not_implemented_yet__get_symtab_message (hdf5_file_t *f, haddr_t addr, haddr_t *heap, haddr_t *btree) {return FAIL;}
herr_t not_implemented_yet__update_symtab_message (hdf5_file_t *f, haddr_t addr, haddr_t heap, haddr_t btree) {return FAIL;}
