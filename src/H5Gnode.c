/*-------------------------------------------------------------------------
 * Copyright (C) 1997	National Center for Supercomputing Applications.
 *                      All rights reserved.
 *
 *-------------------------------------------------------------------------
 *
 * Created:		snode.c
 * 			Jun 26 1997
 * 			Robb Matzke <matzke@llnl.gov>
 *
 * Purpose:		Functions for handling symbol table nodes.  A
 *			symbol table node is a small collection of symbol
 *			table entries.  A B-tree usually points to the
 *			symbol table nodes for any given symbol table.
 *
 * Modifications:	
 *
 *-------------------------------------------------------------------------
 */
#include <assert.h>
#include <stdio.h>
#include <string.h>

#include "hdf5.h"

/* Packages needed by this file... */
#include "H5ACprivate.h"		/*cache				*/
#include "H5Bprivate.h"			/*B-link trees			*/
#include "H5Gprivate.h"			/*me				*/
#include "H5Hprivate.h"			/*heap				*/
#include "H5MFprivate.h"		/*file memory management	*/
#include "H5MMprivate.h"		/*core memory management	*/

/* PRIVATE PROTOTYPES */
static void H5G_node_decode_key (hdf5_file_t *f, uint8 *raw, void *_key);
static void H5G_node_encode_key (hdf5_file_t *f, uint8 *raw, void *_key);
static size_t H5G_node_size (hdf5_file_t *f);
static haddr_t H5G_node_new (hdf5_file_t *f, void *_lt_key, void *_udata,
			     void *_rt_key);
static herr_t H5G_node_flush (hdf5_file_t *f, hbool_t destroy, haddr_t addr,
			      H5G_node_t *sym);
static H5G_node_t *H5G_node_load (hdf5_file_t *f, haddr_t addr,
				  const void *_data);
static intn H5G_node_cmp (hdf5_file_t *f, void *_lt_key, void *_udata,
			  void *_rt_key);
static herr_t H5G_node_found (hdf5_file_t *f, haddr_t addr, void *_lt_key,
			      void *_udata, void *_rt_key);
static haddr_t H5G_node_insert (hdf5_file_t *f, haddr_t addr, intn *anchor,
				void *_lt_key, hbool_t *lt_key_changed,
				void *_md_key, void *_udata,
				void *_rt_key, hbool_t *rt_key_changed);
static herr_t H5G_node_list (hdf5_file_t *f, haddr_t addr, void *_udata);
static size_t H5G_node_sizeof_rkey (hdf5_file_t *f);

/* H5G inherits cache-like properties from H5AC */
static const H5AC_class_t H5AC_SNODE[1] = {{
      (void*(*)(hdf5_file_t*,haddr_t,const void*))H5G_node_load,
      (herr_t(*)(hdf5_file_t*,hbool_t,haddr_t,void*))H5G_node_flush,
}};

/* H5G inherits B-tree like properties from H5B */
static const H5B_class_t H5B_SNODE[1] = {{
   0,						/*id			*/
   64,						/*k			*/
   sizeof (H5G_node_key_t),			/*sizeof_nkey		*/
   H5G_node_sizeof_rkey,			/*get_sizeof_rkey	*/
   H5G_node_new,				/*new			*/
   H5G_node_cmp,				/*cmp			*/
   H5G_node_found,				/*found			*/
   H5G_node_insert,				/*insert		*/
   H5G_node_list,				/*list			*/
   H5G_node_decode_key,				/*decode		*/
   H5G_node_encode_key,				/*encode		*/
}};


/*-------------------------------------------------------------------------
 * Function:	H5G_node_sizeof_rkey
 *
 * Purpose:	Returns the size of a raw B-link tree key for the specified
 *		file.
 *
 * Return:	Success:	Size of the key.
 *
 *		Failure:	never fails
 *
 * Programmer:	Robb Matzke
 *		robb@maya.nuance.com
 *		Jul 14 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static size_t
H5G_node_sizeof_rkey (hdf5_file_t *f)
{
   return H5F_SIZEOF_OFFSET(f);
}


/*-------------------------------------------------------------------------
 * Function:	H5G_node_decode_key
 *
 * Purpose:	Decodes a raw key into a native key.
 *
 * Return:	void
 *
 * Programmer:	Robb Matzke
 *		robb@maya.nuance.com
 *		Jul  8 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
H5G_node_decode_key (hdf5_file_t *f, uint8 *raw, void *_key)
{
   H5G_node_key_t	*key = (H5G_node_key_t *)_key;

   H5F_decode_offset (f, raw, key->offset);
}


/*-------------------------------------------------------------------------
 * Function:	H5G_node_encode_key
 *
 * Purpose:	Encodes a native key into a raw key.
 *
 * Return:	void
 *
 * Programmer:	Robb Matzke
 *		robb@maya.nuance.com
 *		Jul  8 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
H5G_node_encode_key (hdf5_file_t *f, uint8 *raw, void *_key)
{
   H5G_node_key_t	*key = (H5G_node_key_t *)_key;

   H5F_encode_offset (f, raw, key->offset);
}


/*-------------------------------------------------------------------------
 * Function:	H5G_node_size
 *
 * Purpose:	Returns the total size of a symbol table node.
 *
 * Return:	Success:	Total size of the node in bytes.
 *
 *		Failure:	Never fails.
 *
 * Programmer:	Robb Matzke
 *		robb@maya.nuance.com
 *		Jun 23 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static size_t
H5G_node_size (hdf5_file_t *f)
{
   return H5G_HDR_SIZE(f) + (2*H5G_NODE_K) * (2*H5F_SIZEOF_OFFSET(f));
}


/*-------------------------------------------------------------------------
 * Function:	H5G_node_new
 *
 * Purpose:	Creates a new empty symbol table.  This function is called
 *		by the B-tree insert function for an empty tree.  It is
 *		also called internally to split a symbol node with
 *		LT_KEY and RT_KEY null pointers.
 *
 * Return:	Success:	Address of symbol table node.
 *
 *		Failure:	0
 *
 * Programmer:	Robb Matzke
 *		robb@maya.nuance.com
 *		Jun 23 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static haddr_t
H5G_node_new (hdf5_file_t *f, void *_lt_key, void *_udata, void *_rt_key)
{
   H5G_node_key_t	*lt_key = (H5G_node_key_t*)_lt_key;
   H5G_node_key_t	*rt_key = (H5G_node_key_t*)_rt_key;
   H5G_node_t		*sym = H5MM_xcalloc (1, sizeof(H5G_node_t));
   size_t		size = H5G_node_size (f);
   haddr_t		addr = H5MF_alloc (f, size);

   sym->dirty = 1;
   sym->entry = H5MM_xcalloc (2 * H5G_NODE_K, sizeof(H5G_entry_t));
   H5AC_set (f, H5AC_SNODE, addr, sym);

   /*
    * The left and right symbols in an empty tree are both the
    * empty string stored at offset zero by the symtab.c functions. This
    * allows the comparison functions to work correctly without knowing
    * that there are no symbols.
    */
   if (lt_key) lt_key->offset = 0;
   if (rt_key) rt_key->offset = 0;
   return addr;
}


/*-------------------------------------------------------------------------
 * Function:	H5G_node_flush
 *
 * Purpose:	Flush a symbol table node to disk.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *		robb@maya.nuance.com
 *		Jun 23 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5G_node_flush (hdf5_file_t *f, hbool_t destroy, haddr_t addr, H5G_node_t *sym)
{
   uint8	*buf=NULL, *p=NULL;
   size_t	size;

   if (sym->dirty) {
      size = H5G_node_size (f);
      buf = p = H5MM_xmalloc (size);

      /* magic number */
      HDmemcpy (p, H5G_NODE_MAGIC, 4);
      p += 4;

      /* version number */
      *p++ = H5G_NODE_VERS;

      /* reserved */
      *p++ = 0;

      /* number of symbols */
      UINT16ENCODE (p, sym->nsyms);

      /* entries */
      H5G_encode_vec (f, &p, sym->entry, sym->nsyms);

      
      H5F_block_write (f, addr, p-buf, buf);
      buf = H5MM_xfree (buf);
   }

   if (destroy) {
      sym->entry = H5MM_xfree (sym->entry);
      H5MM_xfree (sym);
   }

   return 0;
}


/*-------------------------------------------------------------------------
 * Function:	H5G_node_load
 *
 * Purpose:	Loads a symbol table from the file.
 *
 * Return:	Success:	Ptr to the new table.
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *		robb@maya.nuance.com
 *		Jun 23 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static H5G_node_t *
H5G_node_load (hdf5_file_t *f, haddr_t addr, const void *_udata)
{
   H5G_node_t	*sym = H5MM_xcalloc (1, sizeof(H5G_node_t));
   size_t	size = H5G_node_size (f);
   uint8	*buf = H5MM_xmalloc (size);
   uint8	*p = buf;
   

   sym->entry = H5MM_xcalloc (2*H5G_NODE_K, sizeof(H5G_entry_t));
   H5F_block_read (f, addr, size, buf);

   /* magic */
   if (HDmemcmp (p, H5G_NODE_MAGIC, 4)) goto error;
   p += 4;

   /* version */
   if (H5G_NODE_VERS!=*p++) goto error;

   /* reserved */
   p++;

   /* number of symbols */
   UINT16DECODE (p, sym->nsyms);

   /* entries */
   H5G_decode_vec (f, &p, sym->entry, sym->nsyms);

   H5MM_xfree (buf);
   return sym;

error:
   H5MM_xfree (buf);
   H5MM_xfree (sym);
   return NULL;
}


/*-------------------------------------------------------------------------
 * Function:	H5G_node_cmp
 *
 * Purpose:	Compares two keys from a B-tree node (LEFT and RIGHT)
 *		against another key (not necessarily the same type)
 *		pointed to by UDATA.
 *
 * Return:	Success:	negative if the UDATA key is less than
 *				or equal to the LEFT key.
 *
 * 				positive if the UDATA key is greater
 *				than the RIGHT key.
 *
 * 				zero if the UDATA key falls between
 *				the LEFT key (exclusive) and the
 *				RIGHT key (inclusive).
 *
 *		Failure:	Never fails
 *
 * Programmer:	Robb Matzke
 *		robb@maya.nuance.com
 *		Jun 23 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static intn
H5G_node_cmp (hdf5_file_t *f, void *_lt_key, void *_udata, void *_rt_key)
{
   H5G_node_ud1_t	*udata = (H5G_node_ud1_t *)_udata;
   H5G_node_key_t	*lt_key = (H5G_node_key_t *)_lt_key;
   H5G_node_key_t	*rt_key = (H5G_node_key_t *)_rt_key;
   const char		*s;

   /* left side */
   s = H5H_peek (f, udata->heap, lt_key->offset);
   if (HDstrcmp (udata->name, s)<=0) return -1;

   /* right side */
   s = H5H_peek (f, udata->heap, rt_key->offset);
   if (HDstrcmp (udata->name, s)>0) return 1;

   return 0;
}


/*-------------------------------------------------------------------------
 * Function:	H5G_node_found
 *
 * Purpose:	The B-tree search engine has found the symbol table node
 *		which contains the requested symbol if the symbol exists.
 *		This function should examine that node for the symbol and
 *		return information about the symbol through the UDATA
 *		structure which contains the symbol name on function
 *		entry.
 *
 * Return:	Success:	0 if found and data returned through the
 *				UDATA pointer.
 *
 *		Failure:	-1 if not found.
 *
 * Programmer:	Robb Matzke
 *		robb@maya.nuance.com
 *		Jun 23 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5G_node_found (hdf5_file_t *f, haddr_t addr, void *_lt_key, void *_udata,
		void *_rt_key)
{
   H5G_node_ud1_t	*udata = (H5G_node_ud1_t *)_udata;
   H5G_node_t		*sn;
   intn			lt=0, idx=0, rt, cmp=1;
   const char		*s;

   sn = H5AC_find (f, H5AC_SNODE, addr, NULL);
   if (!sn) return -1;
   rt = sn->nsyms;

   /*
    * Binary search.
    */
   while (lt<rt && cmp) {
      idx = (lt + rt) / 2;
      sn = H5AC_find (f, H5AC_SNODE, addr, NULL);
      s = H5H_peek (f, udata->heap, sn->entry[idx].name_off);
      cmp = HDstrcmp (udata->name, s);
      
      if (cmp<0) {
	 rt = idx;
      } else {
	 lt = idx+1;
      }
   }
   if (cmp) return -1;

   udata->entry = sn->entry[idx];
   return 0;
}


/*-------------------------------------------------------------------------
 * Function:	H5G_node_insert
 *
 * Purpose:	The B-tree insertion engine has found the symbol table node
 *		which should receive the new symbol/address pair.  This
 *		function adds it to that node unless it already existed.
 *
 * 		If the node has no room for the symbol then the node is
 *		split into two nodes.  The original node contains the
 *		low values and the new node contains the high values.
 *		The new symbol table entry is added to either node as
 *		appropriate.  When a split occurs, this function will
 *	        write the maximum key of the low node to the MID buffer
 *		and return the address of the new node.
 *
 * 		If the new key is larger than RIGHT then update RIGHT
 *		with the new key.
 *
 * Return:	Success:	Address of new node if the node was
 *				split.  MID has been initialized with
 *				the high key of the left node, RIGHT
 *				has the high key of the right node.
 *
 * 				Zero if the node didn't split.  RIGHT has the
 *				high key of the right node.
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *		robb@maya.nuance.com
 *		Jun 24 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static haddr_t
H5G_node_insert (hdf5_file_t *f, haddr_t addr, intn *anchor,
		 void *_lt_key, hbool_t *lt_key_changed,
		 void *_md_key, void *_udata,
		 void *_rt_key, hbool_t *rt_key_changed)
{
   H5G_node_key_t	*md_key = (H5G_node_key_t *)_md_key;
   H5G_node_key_t	*rt_key = (H5G_node_key_t *)_rt_key;
   H5G_node_ud1_t 	*udata = (H5G_node_ud1_t *)_udata;
   
   H5G_node_t		*sn;
   H5G_entry_t 		ent[2*H5G_NODE_K];
   haddr_t		new_node=0, offset;
   const char		*s;
   intn			idx=-1, nsyms, cmp=1;
   intn			lt=0, rt;		/*binary search cntrs	*/

   /*
    * Symbol tables are always split so the new symbol table node is
    * to the right of the old one.
    */
   *anchor = H5B_ANCHOR_LT;
   *lt_key_changed = FALSE;
   *rt_key_changed = FALSE;

   /*
    * Load the symbol node and buffer the entries so we don't have to
    * worry about the cached value disappearing.
    */
   sn = H5AC_find (f, H5AC_SNODE, addr, NULL);
   if (!sn) return -1;
   HDmemcpy (ent, sn->entry, sn->nsyms * sizeof(H5G_entry_t));
   rt = nsyms = sn->nsyms;
   sn = NULL;

   /*
    * Where does the new symbol get inserted?  We use a binary search.
    */
   while (lt<rt) {
      idx = (lt + rt) / 2;
      s = H5H_peek (f, udata->heap, ent[idx].name_off);
      if (0==(cmp=HDstrcmp (udata->name, s))) return -1; /*already present*/
      if (cmp<0) {
	 rt = idx;
      } else {
	 lt = idx+1;
      }
   }
   idx += cmp>0 ? 1 : 0;

   /*
    * Add the new name to the heap.  The caller will check if the
    * heap address changed and update the symbol table object header
    * with the new heap address.
    */
   offset = H5H_insert (f, udata->heap, strlen(udata->name)+1, udata->name);

   if (nsyms>=2*H5G_NODE_K) {
      /*
       * The node is full.  Split it into a left and right
       * node and return the address of the new right node (the
       * left node is at the same address as the original node).
       */
      
      /* The left node */
      sn = H5AC_find (f, H5AC_SNODE, addr, NULL);
      HDmemset (sn->entry+H5G_NODE_K, 0, H5G_NODE_K*sizeof(H5G_entry_t));
      sn->nsyms = H5G_NODE_K;
      sn->dirty += 1;
      
      if (idx<=H5G_NODE_K) {
	 memmove (sn->entry+idx+1, sn->entry+idx,
		  (H5G_NODE_K-idx) * sizeof(H5G_entry_t));
	 sn->entry[idx] = udata->entry;
	 sn->entry[idx].name_off = offset;
	 sn->nsyms += 1;
      }
      
      /* The middle key */
      md_key->offset = sn->entry[sn->nsyms-1].name_off;

      /* The right node */
      new_node = H5G_node_new (f, NULL, NULL, NULL);
      sn = H5AC_find (f, H5AC_SNODE, new_node, NULL);
      HDmemcpy (sn->entry, ent+H5G_NODE_K,
		H5G_NODE_K*sizeof(H5G_entry_t));
      sn->nsyms = H5G_NODE_K;
      sn->dirty += 1;

      if (idx>H5G_NODE_K) {
	 idx -= H5G_NODE_K;
	 HDmemmove (sn->entry+idx+1, sn->entry+idx,
		    (H5G_NODE_K-idx) * sizeof (H5G_entry_t));
	 sn->entry[idx] = udata->entry;
	 sn->entry[idx].name_off = offset;
	 sn->nsyms += 1;

	 if (idx+1==sn->nsyms) {
	    rt_key->offset = offset;
	    *rt_key_changed = TRUE;
	 }
      }
      
   } else {
      /*
       * Add the new symbol to the node.
       */
      sn = H5AC_find (f, H5AC_SNODE, addr, NULL);
      sn->dirty += 1;
      HDmemmove (sn->entry+idx+1, sn->entry+idx,
		 (sn->nsyms-idx) * sizeof (H5G_entry_t));
      sn->nsyms += 1;
      sn->entry[idx] = udata->entry;
      sn->entry[idx].name_off = offset;

      if (idx+1==sn->nsyms) {
	 rt_key->offset = offset;
	 *rt_key_changed = TRUE;
      }
   }

   return new_node;
}


/*-------------------------------------------------------------------------
 * Function:	H5G_node_list
 *
 * Purpose:	This function gets called during a directory list operation.
 *		It should fill in data in the UDATA struct.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *		robb@maya.nuance.com
 *		Jun 24 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5G_node_list (hdf5_file_t *f, haddr_t addr, void *_udata)
{
   H5G_node_list_t	*udata = (H5G_node_list_t *)_udata;
   H5G_entry_t 		*ent;
   intn			i, nsyms;
   const char 		*s;
   H5G_node_t		*sn;

   /*
    * Read the symbol table and save the entry info.  We save it
    * in a separate buffer if names are requested because the name
    * lookup may invalidate the symbol table.
    */
   sn = H5AC_find (f, H5AC_SNODE, addr, NULL);
   if (!sn) return -1;
   nsyms = sn->nsyms;
   if (udata->name) {
      ent = H5MM_xmalloc (nsyms * sizeof(H5G_entry_t));
      HDmemcpy (ent, sn->entry, nsyms*sizeof(H5G_entry_t));
      sn = NULL;
   } else {
      ent = sn->entry;
   }

   /*
    * Gather the info.
    */
   if (udata->name || udata->entry) {
      for (i=0; i<nsyms && udata->nused<udata->nentries; i++) {
	 if (udata->name) {
	    s = H5H_peek (f, udata->heap, ent[i].name_off);
	    udata->name[udata->nused] = H5MM_xstrdup (s);
	 }
	 if (udata->entry) {
	    udata->entry[udata->nused] = ent[i];
	 }
	 udata->nused += 1;
      }
   } else {
      udata->nused += sn->nsyms;
   }

   if (!sn) H5MM_xfree (ent);
   return 0;
}

