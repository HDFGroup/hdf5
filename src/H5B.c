/*-------------------------------------------------------------------------
 * Copyright (C) 1997	National Center for Supercomputing Applications.
 *                      All rights reserved.
 *
 *-------------------------------------------------------------------------
 *
 * Created:		hdf5btree.c
 * 			Jul 10 1997
 * 			Robb Matzke <robb@maya.nuance.com>
 *
 * Purpose:		Implements balanced, sibling-linked, N-ary trees
 *			capable of storing any type of data with unique key
 *			values.
 *
 * 			A B-link-tree is a balanced tree where each node has
 *			a pointer to its left and right siblings.  A
 *			B-link-tree is a rooted tree having the following
 *			properties:
 *
 * 			1. Every node, x, has the following fields:
 *
 * 			   a. level[x], the level in the tree at which node
 *			      x appears.  Leaf nodes are at level zero.
 *
 * 			   b. n[x], the number of children pointed to by the
 *			      node.  Internal nodes point to subtrees while
 *			      leaf nodes point to arbitrary data.
 *
 * 			   c. The child pointers themselves, child[x,i] such
 *			      that 0 <= i < n[x].
 *
 * 			   d. n[x]+1 key values stored in increasing
 *			      order:
 *
 * 				key[x,0] < key[x,1] < ... < key[x,n[x]].
 *
 * 			   e. left[x] is a pointer to the node's left sibling
 *			      or the null pointer if this is the left-most
 *			      node at this level in the tree.
 *			      
 * 			   f. right[x] is a pointer to the node's right
 *			      sibling or the null pointer if this is the
 *			      right-most node at this level in the tree.
 *
 * 			3. The keys key[x,i] partition the key spaces of the
 *			   children of x:
 *
 * 			      key[x,i] <= key[child[x,i],j] <= key[x,i+1]
 *
 *			   for any valid combination of i and j.
 *
 * 			4. There are lower and upper bounds on the number of
 *			   child pointers a node can contain.  These bounds
 *			   can be expressed in terms of a fixed integer k>=2
 *			   called the `minimum degree' of the B-tree.
 *
 * 			   a. Every node other than the root must have at least
 *			      k child pointers and k+1 keys.  If the tree is
 *			      nonempty, the root must have at least one child
 *			      pointer and two keys.
 *
 *  			   b. Every node can contain at most 2k child pointers
 *			      and 2k+1 keys.  A node is `full' if it contains
 *			      exactly 2k child pointers and 2k+1 keys.
 *
 * 			5. When searching for a particular value, V, and
 *			   key[V] = key[x,i] for some node x and entry i,
 *			   then:
 *
 * 			   a. If i=0 the child[0] is followed.
 *
 * 			   b. If i=n[x] the child[n[x]-1] is followed.
 *
 * 			   c. Otherwise, the child that is followed
 *			      (either child[x,i-1] or child[x,i]) is
 *			      determined by the type of object to which the
 *			      leaf nodes of the tree point and is controlled
 *			      by the key comparison function registered for
 *			      that type of B-tree.
 *
 *
 * Modifications:	
 *
 *-------------------------------------------------------------------------
 */

/*
 * Define this if the root address of a B-link tree should never change.
 * 
 * If this isn't defined and the root node of a tree splits, then the
 * new root (which points to the old root plus the new node from the
 * split) will be at a new file address.
 *
 * But if this is defined, then the old root will be copied to a new
 * location and the new root will occupy the file memory vacated by the
 * old root.
 */
#define H5B_ANCHOR_ROOT

/* system headers */
#include <assert.h>
#include "hdf5.h"

/* private headers */
#include "H5ACprivate.h"		/*cache				*/
#include "H5Bprivate.h"			/*B-link trees			*/
#include "H5MFprivate.h"		/*File memory management	*/
#include "H5MMprivate.h"		/*Core memory management       	*/

#define BOUND(MIN,X,MAX) ((MIN)<(X)?(MIN):((MAX)>(X)?(MAX):(X)))
#define false 0
#define true 1

/* PRIVATE PROTOTYPES */
static haddr_t H5B_insert_helper (hdf5_file_t *f, haddr_t addr,
				  const H5B_class_t *type,
				  uint8 *lt_key, intn *lt_key_changed,
				  uint8 *md_key, void *udata,
				  uint8 *rt_key, intn *rt_key_changed);
static herr_t H5B_flush (hdf5_file_t *f, hbool_t destroy, haddr_t addr,
			 H5B_t *b);
static H5B_t *H5B_load (hdf5_file_t *f, haddr_t addr, const void *_data);
static herr_t H5B_decode_key (hdf5_file_t *f, H5B_t *bt, intn idx);
static size_t H5B_nodesize (hdf5_file_t *f, const H5B_class_t *type,
			    size_t *total_nkey_size, size_t sizeof_rkey);

/* H5B inherits cache-like properties from H5AC */
static const H5AC_class_t H5AC_BT[1] = {{
   (void*(*)(hdf5_file_t*,haddr_t,const void*))H5B_load,
   (herr_t(*)(hdf5_file_t*,hbool_t,haddr_t,void*))H5B_flush,
}};


/*-------------------------------------------------------------------------
 * Function:	H5B_new
 *
 * Purpose:	Creates a new empty B-tree leaf node.
 *
 * Return:	Success:	address of new node.
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
haddr_t
H5B_new (hdf5_file_t *f, const H5B_class_t *type, size_t sizeof_rkey)
{
   H5B_t	*bt=NULL;
   haddr_t	addr;
   size_t	size;
   size_t	total_native_keysize;
   intn		offset, i;

   /*
    * Allocate file and memory data structures.
    */
   size = H5B_nodesize (f, type, &total_native_keysize, sizeof_rkey);
   if ((addr = H5MF_alloc (f, size))<=0) return -1;
   bt = H5MM_xmalloc (sizeof(H5B_t));
   bt->type = type;
   bt->sizeof_rkey = sizeof_rkey;
   bt->dirty = 1;
   bt->ndirty = 0;
   bt->type = type;
   bt->level = 0;
   bt->left = bt->right = 0;
   bt->nchildren = 0;
   bt->page = H5MM_xmalloc (size);
   bt->native = H5MM_xmalloc (total_native_keysize);
   bt->child = H5MM_xmalloc (2*type->k * sizeof(haddr_t));
   bt->key = H5MM_xmalloc ((2*type->k+1) * sizeof(H5B_key_t));

   /*
    * Initialize each entry's raw child and key pointers to point into the
    * `page' buffer.  Each native key pointer should be null until the key is
    * translated to native format.
    */
   for (i=0,offset=H5B_HDR_SIZE(f);
	i<2*type->k;
	i++,offset+=bt->sizeof_rkey+H5F_SIZEOF_OFFSET(f)) {

      bt->key[i].dirty = 0;
      bt->key[i].rkey = bt->page + offset;
      bt->key[i].nkey = NULL;
      bt->child[i] = 0;
   }

   /*
    * The last possible key...
    */
   bt->key[2*type->k].dirty = 0;
   bt->key[2*type->k].rkey = bt->page + offset;
   bt->key[2*type->k].nkey = NULL;

   /*
    * Cache the new B-tree node.
    */
   H5AC_set (f, H5AC_BT, addr, bt);
   return addr;
}


/*-------------------------------------------------------------------------
 * Function:	H5B_load
 *
 * Purpose:	Loads a B-tree node from the disk.
 *
 * Return:	Success:	Pointer to a new B-tree node.
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
static H5B_t *
H5B_load (hdf5_file_t *f, haddr_t addr, const void *_data)
{
   const H5B_class_t 	*type = (const H5B_class_t *)_data;
   size_t		size, total_nkey_size;
   H5B_t		*bt = H5MM_xmalloc (sizeof(H5B_t));
   intn			i;
   uint8		*p;

   assert (type);
   assert (type->get_sizeof_rkey);


   bt->sizeof_rkey = (type->get_sizeof_rkey)(f);
   size = H5B_nodesize (f, type, &total_nkey_size, bt->sizeof_rkey);
   bt->type = type;
   bt->dirty = 0;
   bt->ndirty = 0;
   bt->page = H5MM_xmalloc (size);
   bt->native = H5MM_xmalloc (total_nkey_size);
   bt->key = H5MM_xmalloc ((2*type->k+1) * sizeof(H5B_key_t));
   bt->child = H5MM_xmalloc (2 * type->k * sizeof(haddr_t));
   H5F_block_read (f, addr, size, bt->page);
   p = bt->page;

   /* magic number */
   if (memcmp (p, H5B_MAGIC, 4)) goto error;
   p += 4;

   /* node type and level */
   if (*p++ != type->id) goto error;
   bt->level = *p++;

   /* entries used */
   UINT16DECODE (p, bt->nchildren);

   /* sibling pointers */
   H5F_decode_offset (f, p, bt->left);
   H5F_decode_offset (f, p, bt->right);

   /* the child/key pairs */
   for (i=0; i<2*type->k; i++) {

      bt->key[i].dirty = 0;
      bt->key[i].rkey = p;
      p += bt->sizeof_rkey;
      bt->key[i].nkey = NULL;

      if (i<bt->nchildren) {
	 H5F_decode_offset (f, p, bt->child[i]);
      } else {
	 bt->child[i] = 0;
	 p += H5F_SIZEOF_OFFSET(f);
      }
   }

   bt->key[2*type->k].dirty = 0;
   bt->key[2*type->k].rkey = p;
   bt->key[2*type->k].nkey = NULL;
   return bt;

error:
   if (bt) {
      H5MM_xfree (bt->child);
      H5MM_xfree (bt->key);
      H5MM_xfree (bt->page);
      H5MM_xfree (bt->native);
      H5MM_xfree (bt);
   }
   return NULL;
}


/*-------------------------------------------------------------------------
 * Function:	H5B_flush
 *
 * Purpose:	Flushes a dirty B-tree node to disk.
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
H5B_flush (hdf5_file_t *f, hbool_t destroy, haddr_t addr, H5B_t *bt)
{
   intn		i;
   size_t	size = H5B_nodesize (f, bt->type, NULL, bt->sizeof_rkey);
   uint8	*p = bt->page;

   assert (bt->type);
   assert (bt->type->encode);
   
   if (bt->dirty) {
      
      /* magic number */
      memcpy (p, H5B_MAGIC, 4);
      p += 4;

      /* node type and level */
      *p++ = bt->type->id;
      *p++ = bt->level;

      /* entries used */
      UINT16ENCODE (p, bt->nchildren);

      /* sibling pointers */
      H5F_encode_offset (f, p, bt->left);
      H5F_encode_offset (f, p, bt->right);

      /* child keys and pointers */
      for (i=0; i<=bt->nchildren; i++) {
	 
	 /* encode the key */
	 assert (bt->key[i].rkey == p);
	 if (bt->key[i].dirty) {
	    if (bt->key[i].nkey) {
	       (bt->type->encode)(f, bt->key[i].rkey, bt->key[i].nkey);
	    }
	    bt->key[i].dirty = 0;
	 }
	 p += bt->sizeof_rkey;

	 /* encode the child address */
	 if (i<bt->ndirty) {
	    H5F_encode_offset (f, p, bt->child[i]);
	 } else {
	    p += H5F_SIZEOF_OFFSET(f);
	 }
      }

      /*
       * Write the disk page.  We always write the header, but we don't
       * bother writing data for the child entries that don't exist or
       * for the final unchanged children.
       */
      H5F_block_write (f, addr, size, bt->page);
      bt->dirty = 0;
      bt->ndirty = 0;
   }

   if (destroy) {
      H5MM_xfree (bt->child);
      H5MM_xfree (bt->key);
      H5MM_xfree (bt->page);
      H5MM_xfree (bt->native);
      H5MM_xfree (bt);
   }
   return 0;
}


/*-------------------------------------------------------------------------
 * Function:	H5B_find
 *
 * Purpose:	Locate the specified information in a B-tree and return
 *		that information by filling in fields of the caller-supplied
 *		UDATA pointer depending on the type of leaf node
 *		requested.  The UDATA can point to additional data passed
 *		to the key comparison function.
 *
 * Note:	This function does not follow the left/right sibling
 *		pointers since it assumes that all nodes can be reached
 *		from the parent node.
 *
 * Return:	Success:	0 if found, values returned through the
 *				RETVAL argument.
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
herr_t
H5B_find (hdf5_file_t *f, const H5B_class_t *type, haddr_t addr, void *udata)
{
   H5B_t	*bt=NULL;
   uint8	lt_key[256], rt_key[256];
   intn		idx=-1, lt=0, rt, cmp=1;

   assert (type);
   assert (type->sizeof_nkey < sizeof lt_key);
   assert (type->decode);
   assert (type->cmp);
   assert (type->found);
   
   /*
    * Perform a binary search to locate the child which contains
    * the thing for which we're searching.  The comparison function
    * may preempt the B-tree node from the cache.
    */
   bt = H5AC_find (f, H5AC_BT, addr, type);
   rt = bt->nchildren;

   while (lt<rt && cmp) {
      idx = (lt + rt) / 2;
      bt = H5AC_find (f, H5AC_BT, addr, type);
      if (!bt) return -1;

      /* the left key */
      if (!bt->key[idx].nkey) H5B_decode_key (f, bt, idx);
      HDmemcpy (lt_key, bt->key[idx].nkey, type->sizeof_nkey);

      /* the right key */
      if (!bt->key[idx+1].nkey) H5B_decode_key (f, bt, idx+1);
      HDmemcpy (rt_key, bt->key[idx+1].nkey, type->sizeof_nkey);

      /* compare */
      if ((cmp=(type->cmp)(f, lt_key, udata, rt_key))<0) {
	 rt = idx;
      } else {
	 lt = idx+1;
      }
   }
   if (cmp) return -1;

   /*
    * Follow the link to the subtree or to the data node.
    */
   bt = H5AC_find (f, H5AC_BT, addr, type);
   assert (idx>=0 && idx<bt->nchildren);
   if (bt->level > 0) {
      return H5B_find (f, type, bt->child[idx], udata);
   }
   
   return (type->found)(f, bt->child[idx], lt_key, udata, rt_key);
}


/*-------------------------------------------------------------------------
 * Function:	H5B_split
 *
 * Purpose:	Split a single node into two nodes.  If anchor is
 *		H5B_ANCHOR_LT then the new node gets the right half of
 *		the old node.  If anchor is H5B_ANCHOR_RT then the
 *		new node gets the left half of the old node.
 *
 * Return:	Success:	Address of the new node.
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *		robb@maya.nuance.com
 *		Jul  3 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static haddr_t
H5B_split (hdf5_file_t *f, const H5B_class_t *type, haddr_t addr, intn anchor)
{
   H5B_t	*old = H5AC_find (f, H5AC_BT, addr, type);
   H5B_t 	*bt = H5MM_xmalloc (sizeof(H5B_t));
   size_t	total_nkey_size, size;
   intn		i, offset;
   intn		delta = H5B_ANCHOR_LT==anchor ? type->k : 0;
   size_t	recsize = old->sizeof_rkey + H5F_SIZEOF_OFFSET(f);
   haddr_t	tmp_addr, new_addr;
   H5B_t	*tmp=NULL;

   /*
    * Create the new B-tree node.
    */
   size = H5B_nodesize (f, type, &total_nkey_size, old->sizeof_rkey);
   bt->dirty = 1;
   bt->ndirty = BOUND (0, old->ndirty-delta, type->k);
   bt->type = type;
   bt->level = old->level;
   bt->nchildren = type->k;
   bt->page = H5MM_xmalloc (size);
   bt->native = H5MM_xmalloc (total_nkey_size);
   bt->child = H5MM_xmalloc (2*type->k * sizeof(haddr_t));
   bt->key = H5MM_xmalloc ((2*type->k+1) * sizeof(H5B_key_t));

   /*
    * Copy data into the new node from the old node.
    */
   memcpy (bt->page + H5B_HDR_SIZE(f),
	   old->page + H5B_HDR_SIZE(f) + delta*recsize,
	   type->k * recsize);
   memcpy (bt->native,
	   old->native + delta * type->sizeof_nkey,
	   (type->k+1) * type->sizeof_nkey);

   for (i=0,offset=H5B_HDR_SIZE(f); i<=2*type->k; i++,offset+=recsize) {

      /* key */
      if (i<=type->k) {
	 bt->key[i].dirty = old->key[delta+i].dirty;
	 bt->key[i].rkey = bt->native + offset;
	 if (old->key[delta+i].nkey) {
	    bt->key[i].nkey = bt->native + i*type->sizeof_nkey;
	 } else {
	    bt->key[i].nkey = NULL;
	 }
      } else {
	 bt->key[i].dirty = 0;
	 bt->key[i].rkey = bt->native + offset;
	 bt->key[i].nkey = NULL;
      }

      /* child */
      if (i<type->k) {
	 bt->child[i] = old->child[delta+i];
      } else if (i<2*type->k) {
	 bt->child[i] = 0;
      }
   }


   /*
    * Truncate the old node.
    */
   delta = H5B_ANCHOR_LT ? 0 : type->k;
   old->dirty += 1;
   old->ndirty = BOUND (0, old->ndirty-delta, type->k);
   old->nchildren = type->k;
   
   if (H5B_ANCHOR_RT==anchor) {
      memcpy (old->page + H5B_HDR_SIZE(f),
	      old->page + H5B_HDR_SIZE(f) + delta*recsize,
	      type->k * recsize);
      memmove (old->native,
	       old->native + delta * type->sizeof_nkey,
	       (type->k+1) * type->sizeof_nkey);

      for (i=0; i<=2*type->k; i++) {

	 if (i<=type->k) {
	    old->key[i].dirty = old->key[delta+i].dirty;
	    if (old->key[delta+i].nkey) {
	       old->key[i].nkey = old->native + i * type->sizeof_nkey;
	    } else {
	       old->key[i].nkey = NULL;
	    }
	 } else {
	    old->key[i].nkey = NULL;
	 }
	 if (i<type->k) {
	    old->child[i] = old->child[delta+i];
	 } else if (i<2*type->k) {
	    old->child[i] = 0;
	 }
      }
   }

   /*
    * Update sibling pointers of new node.
    */
   if (H5B_ANCHOR_LT==anchor) {
      bt->left = addr;
      bt->right = old->right;
   } else {
      bt->left = old->left;
      bt->right = addr;
   }

   /*
    * Add the new node to the cache.
    */
   new_addr = H5MF_alloc (f, size);
   H5AC_set (f, H5AC_BT, new_addr, bt);

   /*
    * Update sibling pointers of old nodes.
    */
   old = H5AC_find (f, H5AC_BT, addr, type);
   if (H5B_ANCHOR_LT==anchor) {
      old->dirty += 1;
      tmp_addr = old->right;
      old->right = new_addr;
      if (tmp_addr) {
	 tmp = H5AC_find (f, H5AC_BT, tmp_addr, type);
	 tmp->dirty += 1;
	 tmp->left = new_addr;
      }
   } else {
      old->dirty += 1;
      tmp_addr = old->left;
      old->left = new_addr;
      if (tmp_addr) {
	 tmp = H5AC_find (f, H5AC_BT, tmp_addr, type);
	 tmp->dirty += 1;
	 tmp->right = new_addr;
      }
   }

   return new_addr;
}


/*-------------------------------------------------------------------------
 * Function:	H5B_decode_key
 *
 * Purpose:	Decode the specified key into native format.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *		robb@maya.nuance.com
 *		Jul  8 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5B_decode_key (hdf5_file_t *f, H5B_t *bt, intn idx)
{
   bt->key[idx].nkey = bt->native + idx * bt->type->sizeof_nkey;
   (bt->type->decode)(f, bt->key[idx].rkey, bt->key[idx].nkey);
   return 0;
}


/*-------------------------------------------------------------------------
 * Function:	H5B_insert
 *
 * Purpose:	Adds a new item to the B-tree.  If the root node of
 *		the B-tree splits then the B-tree gets a new address.
 *
 * Return:	Success:	Address of the root of the B-tree.  The
 *				B-tree root address may change if the old
 *				root is split.
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
haddr_t
H5B_insert (hdf5_file_t *f, const H5B_class_t *type, haddr_t addr, void *udata)
{
   uint8	lt_key[256], md_key[256], rt_key[256];
   intn		lt_key_changed=false, rt_key_changed=false;
   haddr_t	child, new_root;
   intn		level;
   H5B_t	*bt;

   assert (type);
   assert (type->sizeof_nkey < sizeof lt_key);

   child = H5B_insert_helper (f, addr, type, lt_key, &lt_key_changed,
			      md_key, udata, rt_key, &rt_key_changed);
   if (child<0) return -1;
   if (0==child) return addr;

   /* the current root */
   bt = H5AC_find (f, H5AC_BT, addr, type);
   level = bt->level;
   if (!lt_key_changed) {
      if (!bt->key[0].nkey) H5B_decode_key (f, bt, 0);
      memcpy (lt_key, bt->key[0].nkey, type->sizeof_nkey);
   }

   /* the new node */
   bt = H5AC_find (f, H5AC_BT, child, type);
   if (!rt_key_changed) {
      if (!bt->key[bt->nchildren].nkey) H5B_decode_key (f, bt, bt->nchildren);
      memcpy (rt_key, bt->key[bt->nchildren].nkey, type->sizeof_nkey);
   }

#ifdef H5B_ANCHOR_ROOT
   {
      /*
       * Copy the old root node to some other file location and make the new
       * root at the old root's previous address.  This prevents the B-tree
       * from "moving".
       */
      size_t size = H5B_nodesize (f, type, NULL, bt->sizeof_rkey);
      uint8 *buf = H5MM_xmalloc (size);
      haddr_t tmp_addr = H5MF_alloc (f, size);

      H5AC_flush (f, H5AC_BT, addr, FALSE);
      H5F_block_read (f, addr, size, buf);
      H5F_block_write (f, tmp_addr, size, buf);
      H5AC_rename (f, H5AC_BT, addr, tmp_addr);

      buf = H5MM_xfree (buf);
      new_root = addr;
      addr = tmp_addr;

      /* update the new child's left pointer */
      bt = H5AC_find (f, H5AC_BT, child, type);
      bt->dirty += 1;
      bt->left = addr;

      /* clear the old root at the old address */
      bt = H5AC_find (f, H5AC_BT, new_root, type);
      bt->dirty += 1;
      bt->ndirty = 0;
      bt->left = 0;
      bt->right = 0;
      bt->nchildren = 0;
   }
#else
   /*
    * The new root is created at a new file location.
    */
   new_root = H5B_new (f, type, bt->sizeof_rkey);
#endif

   /* the new root */
   bt = H5AC_find (f, H5AC_BT, new_root, type);
   bt->dirty += 1;
   bt->ndirty = 2;
   bt->level = level+1;
   bt->nchildren = 2;
   
   bt->child[0] = addr;
   bt->key[0].dirty = 1;
   bt->key[0].nkey = bt->native;
   memcpy (bt->key[0].nkey, lt_key, type->sizeof_nkey);

   bt->child[1] = child;
   bt->key[1].dirty = 1;
   bt->key[1].nkey = bt->native + type->sizeof_nkey;
   memcpy (bt->key[1].nkey, md_key, type->sizeof_nkey);

   bt->key[2].dirty = 1;
   bt->key[2].nkey = bt->native + 2 * type->sizeof_nkey;
   memcpy (bt->key[2].nkey, rt_key, type->sizeof_nkey);

   return new_root;
}


/*-------------------------------------------------------------------------
 * Function:	H5B_insert_child
 *
 * Purpose:	Insert a child at the specified address with the
 *		specified left or right key.
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
H5B_insert_child (hdf5_file_t *f, const H5B_class_t *type, haddr_t addr,
		  intn idx, haddr_t child, intn anchor, void *md_key)
{
   H5B_t	*bt;
   size_t	recsize;
   intn		i;

   bt = H5AC_find (f, H5AC_BT, addr, type);
   assert (bt);
   bt->dirty += 1;
   recsize = bt->sizeof_rkey + H5F_SIZEOF_OFFSET(f);
   
   if (H5B_ANCHOR_LT==anchor) {
      /*
       * The MD_KEY is the left key of the new node.
       */
      memmove (bt->page + H5B_HDR_SIZE(f) + (idx+1)*recsize,
	       bt->page + H5B_HDR_SIZE(f) + idx*recsize,
	       (bt->nchildren-idx)*recsize + bt->sizeof_rkey);

      memmove (bt->native + (idx+1) * type->sizeof_nkey,
	       bt->native + idx * type->sizeof_nkey,
	       ((bt->nchildren-idx)+1) * type->sizeof_nkey);

      for (i=bt->nchildren; i>=idx; --i) {
	 bt->key[i+1].dirty = bt->key[i].dirty;
      }
      bt->key[idx].dirty = 1;
      bt->key[idx].nkey = bt->native + idx * type->sizeof_nkey;
      memcpy (bt->key[idx].nkey, md_key, type->sizeof_nkey);

   } else {
      /*
       * The MD_KEY is the right key of the new node.
       */
      memmove (bt->page + (H5B_HDR_SIZE(f) +
			   (idx+1)*recsize + bt->sizeof_rkey),
	       bt->page + (H5B_HDR_SIZE(f) +
			   idx*recsize + bt->sizeof_rkey),
	       (bt->nchildren-idx) * recsize);

      memmove (bt->native + idx + 2,
	       bt->native + idx + 1,
	       (bt->nchildren-idx) * type->sizeof_nkey);

      for (i=bt->nchildren; i>idx; --i) {
	 bt->key[i+1].dirty = bt->key[i].dirty;
      }
      bt->key[idx+1].dirty = 1;
      bt->key[idx+1].nkey = bt->native +
			    (idx+1) * type->sizeof_nkey;
      memcpy (bt->key[idx+1].nkey, md_key, type->sizeof_nkey);
   }

   memmove (bt->child + idx + 1,
	    bt->child + idx,
	    (bt->nchildren - idx) * sizeof(haddr_t));

   bt->child[idx] = child;
   bt->nchildren += 1;
   bt->ndirty = bt->nchildren;
}	       


/*-------------------------------------------------------------------------
 * Function:	H5B_insert_helper
 *
 * Purpose:	Inserts the item UDATA into the tree rooted at ADDR and having
 *		the specified type.
 *
 * 		On return, if LT_KEY_CHANGED is non-zero, then LT_KEY is
 *		the new native left key.  Similarily for RT_KEY_CHANGED
 *		and RT_KEY.
 *
 * 		If the node splits, then MD_KEY contains the key that
 *		was split between the two nodes (that is, the key that
 *		appears as the max key in the left node and the min key
 *		in the right node).
 *
 * Return:	Success:	Address of the new node if the node
 *				splits.  The new node is always to the
 *				right of the previous node.
 *
 * 				0 if the node didn't split.  The MD_KEY
 *				buffer is undefined.
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *		robb@maya.nuance.com
 *		Jul  9 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static haddr_t
H5B_insert_helper (hdf5_file_t *f, haddr_t addr, const H5B_class_t *type,
		   uint8 *lt_key, intn *lt_key_changed,
		   uint8 *md_key, void *udata,
		   uint8 *rt_key, intn *rt_key_changed)
{
   H5B_t	*bt;
   intn		lt=0, idx=-1, rt, cmp=-1;
   intn		anchor;
   haddr_t	child, twin=0;

   assert (type);
   assert (type->decode);
   assert (type->cmp);
   assert (type->new);

   /*
    * Use a binary search to find the child that will receive the new
    * data.  The comparison function may preempt the B-tree node from
    * the cache each time through the loop.  When the search completes
    * IDX points to the child that should get the new data.
    */
   bt = H5AC_find (f, H5AC_BT, addr, type);
   if (!bt) goto error;
   rt = bt->nchildren;

   while (lt<rt && cmp) {
      idx = (lt + rt) / 2;
      bt = H5AC_find (f, H5AC_BT, addr, type);
      if (!bt) goto error;

      /* left key */
      if (!bt->key[idx].nkey) H5B_decode_key (f, bt, idx);
      memcpy (lt_key, bt->key[idx].nkey, type->sizeof_nkey);

      /* right key */
      if (!bt->key[idx+1].nkey) H5B_decode_key (f, bt, idx+1);
      memcpy (rt_key, bt->key[idx+1].nkey, type->sizeof_nkey);

      /* compare */
      if ((cmp=(type->cmp)(f, lt_key, udata, rt_key))<0) {
	 rt = idx;
      } else {
	 lt = idx+1;
      }
   }

   /*
    * Adjust for boundary conditions.  If adjusting at the leaf level
    * update the left and right key buffers since the data node insert
    * function needs them as input.  Don't worry about it at higher node
    * levels because this function uses them for output only.
    */
   bt = H5AC_find (f, H5AC_BT, addr, type);
   if (cmp<0 && idx<=0) {
      idx = 0;
      cmp = 0;
      if (0==bt->level && bt->nchildren) {
	 if (!bt->key[idx].nkey) H5B_decode_key (f, bt, idx);
	 memcpy (lt_key, bt->key[idx].nkey, type->sizeof_nkey);
	 if (!bt->key[idx+1].nkey) H5B_decode_key (f, bt, idx+1);
	 memcpy (rt_key, bt->key[idx+1].nkey, type->sizeof_nkey);
      }
   } else if (cmp>0 && idx+1>=bt->nchildren) {
      idx = bt->nchildren-1;
      cmp = 0;
      if (0==bt->level && bt->nchildren) {
	 if (!bt->key[idx].nkey) H5B_decode_key (f, bt, idx);
	 memcpy (lt_key, bt->key[idx].nkey, type->sizeof_nkey);
	 if (!bt->key[idx+1].nkey) H5B_decode_key (f, bt, idx+1);
	 memcpy (rt_key, bt->key[idx+1].nkey, type->sizeof_nkey);
      }
   }
   assert (0==cmp);
   
   /*
    * If there are no children, then create a new child. This can only
    * happen at the root of the B-tree.  Creating a new child may
    * preempt the B-tree node from the cache.  The left and right key
    * buffers are output values.
    */
   if (0==bt->nchildren) {
      child = (type->new)(f, lt_key, udata, rt_key);
      if (child<=0) goto error;
      bt = H5AC_find (f, H5AC_BT, addr, type);
      bt->nchildren = 1;
      bt->dirty += 1;
      bt->ndirty = 1;
      bt->child[0] = child;
      
      bt->key[0].dirty = 1;
      bt->key[0].nkey = bt->native;
      memcpy (bt->key[0].nkey, lt_key, type->sizeof_nkey);

      bt->key[1].dirty = 1;
      bt->key[1].nkey = bt->native + type->sizeof_nkey;
      memcpy (bt->key[1].nkey, rt_key, type->sizeof_nkey);
      idx = 0;
   }

   /*
    * Insert the new data in the child B-tree node or in the data node.
    */
   if (bt->level > 0) {
      child = H5B_insert_helper (f, bt->child[idx], type,
				 lt_key, lt_key_changed,
				 md_key, udata,
				 rt_key, rt_key_changed);
      anchor = H5B_ANCHOR_LT;
   } else {
      child = (type->insert)(f, bt->child[idx], &anchor,
			     lt_key, lt_key_changed,
			     md_key, udata,
			     rt_key, rt_key_changed);
   }
   if (child<0) goto error;
   bt = H5AC_find (f, H5AC_BT, addr, type);

   /*
    * Update the left and right keys.
    */
   if (*lt_key_changed) {
      bt->key[idx].nkey = bt->native + idx * type->sizeof_nkey;
      memcpy (bt->key[idx].nkey, lt_key, type->sizeof_nkey);
      bt->dirty += 1;
      bt->key[idx].dirty = 1;
      if (idx>0) *lt_key_changed = false;
   }
   if (*rt_key_changed) {
      bt->key[idx+1].nkey = bt->native +
			    (idx+1) * type->sizeof_nkey;
      memcpy (bt->key[idx+1].nkey, rt_key, type->sizeof_nkey);
      bt->dirty += 1;
      bt->key[idx+1].dirty = 1;
      if (idx+1<bt->nchildren) *rt_key_changed = false;
   }

   /*
    * If the child split and the left node is anchored, then the new
    * child node gets inserted to the right of our current position.
    */
   if (child && H5B_ANCHOR_LT==anchor) idx++;
      
   /*
    * Split this node if the child node split and this node is full.
    * Make sure `addr' points to the node that gets the new child
    * and that `idx' is adjusted appropriately.
    */
   if (child && bt->nchildren==2*type->k) {
      twin = H5B_split (f, type, addr, anchor);
      if (idx<=type->k) {
	 addr = H5B_ANCHOR_LT==anchor ? addr : twin;
      } else {
	 idx -= type->k;
	 addr = H5B_ANCHOR_LT==anchor ? twin : addr;
      }
   }
   
   /*
    * If the child split, then insert the new child.
    */
   if (child) {
      H5B_insert_child (f, type, addr, idx, child, anchor, md_key);
   }

   /*
    * If this node split, return the mid key (the one that is shared
    * by the left and right node).
    */
   if (twin) {
      bt = H5AC_find (f, H5AC_BT, twin, type);
      if (H5B_ANCHOR_LT==anchor) {
	 if (!bt->key[0].nkey) {
	    H5B_decode_key (f, bt, 0);
	 }
	 memcpy (md_key, bt->key[0].nkey, type->sizeof_nkey);
      } else {
	 if (!bt->key[bt->nchildren].nkey) {
	    H5B_decode_key (f, bt, bt->nchildren);
	 }
	 memcpy (md_key, bt->key[bt->nchildren].nkey, type->sizeof_nkey);
      }
   }

   return twin;

error:
   return -1;
}


/*-------------------------------------------------------------------------
 * Function:	H5B_list
 *
 * Purpose:	Calls the list callback for each leaf node of the
 *		B-tree, passing it the UDATA structure.
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
herr_t
H5B_list (hdf5_file_t *f, const H5B_class_t *type, haddr_t addr, void *udata)
{
   H5B_t	*bt;
   haddr_t	*child=NULL;
   haddr_t	twin;
   intn		i, nchildren, status;
   herr_t	(*list)(hdf5_file_t*,haddr_t,void*);

   assert (type);
   assert (type->list);
   
   bt = H5AC_find (f, H5AC_BT, addr, type);
   if (!bt) return -1;

   if (bt->level>0) {
      return H5B_list (f, type, bt->child[0], udata);
   } else {
      child = H5MM_xmalloc (2 * type->k * sizeof(haddr_t));
      list = type->list;
      twin = addr;
      
      while (twin) { /*for each leaf node*/
	 bt = H5AC_find (f, H5AC_BT, twin, type);
	 if (!bt) {
	    H5MM_xfree (child);
	    return -1;
	 }
	 nchildren = bt->nchildren;
	 twin = bt->right;
	 HDmemcpy (child, bt->child, nchildren * sizeof(haddr_t));
	 bt = NULL; /*list callback may invalidate the cache*/
	 
	 for (i=0; i<nchildren; i++) {
	    status = (list)(f, child[i], udata);
	    if (status<0) {
	       H5MM_xfree (child);
	       return -1;
	    }
	 }
      }
      H5MM_xfree (child);
   }
   return 0;
}


/*-------------------------------------------------------------------------
 * Function:	H5B_nodesize
 *
 * Purpose:	Returns the number of bytes needed for this type of
 *		B-tree node.  The size is the size of the header plus
 *		enough space for 2t child pointers and 2t+1 keys.
 *
 * 		If TOTAL_NKEY_SIZE is non-null, what it points to will
 *		be initialized with the total number of bytes required to
 *		hold all the key values in native order.
 *
 * Return:	Success:	Size of node in file.
 *
 *		Failure:	never fails.
 *
 * Programmer:	Robb Matzke
 *		robb@maya.nuance.com
 *		Jul  3 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static size_t
H5B_nodesize (hdf5_file_t *f, const H5B_class_t *type,
	      size_t *total_nkey_size, size_t sizeof_rkey)
{
   if (total_nkey_size) {
      *total_nkey_size = (2 * type->k + 1) * type->sizeof_nkey;
   }

   return (H5B_HDR_SIZE(f) +				/*node header	*/
	   2 * type->k * H5F_SIZEOF_OFFSET(f) +		/*child pointers*/
	   (2*type->k+1) * sizeof_rkey);		/*keys		*/
}

