/*-------------------------------------------------------------------------
 * Copyright (C) 1997	National Center for Supercomputing Applications.
 *                      All rights reserved.
 *
 *-------------------------------------------------------------------------
 *
 * Created:		hdf5btree.c
 * 			Jul 10 1997
 * 			Robb Matzke <matzke@llnl.gov>
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
 * 	Robb Matzke, 4 Aug 1997
 *	Added calls to H5E.
 *
 *-------------------------------------------------------------------------
 */
/* private headers */
#include <H5private.h>			/*library			*/
#include <H5ACprivate.h>		/*cache				*/
#include <H5Bprivate.h>			/*B-link trees			*/
#include <H5Eprivate.h>			/*error handling		*/
#include <H5MFprivate.h>		/*File memory management	*/
#include <H5MMprivate.h>		/*Core memory management       	*/

/*
 * Define this constant if you want to check B-tree consistency after each
 * B-tree operation.  Note that this slows down the library considerably!
 * Debugging the B-tree depends on assert() being enabled.
 */
#ifdef NDEBUG
#  undef H5B_DEBUG
#endif

#define PABLO_MASK	H5B_mask

#define BOUND(MIN,X,MAX) ((X)<(MIN)?(MIN):((X)>(MAX)?(MAX):(X)))

/* PRIVATE PROTOTYPES */
static haddr_t H5B_insert_helper (H5F_t *f, haddr_t addr, H5B_class_t *type,
				  H5B_ins_t *anchor, 
				  uint8 *lt_key, hbool_t *lt_key_changed,
				  uint8 *md_key, void *udata,
				  uint8 *rt_key, hbool_t *rt_key_changed);
static herr_t H5B_insert_child (H5F_t *f, const H5B_class_t *type,
				H5B_t *bt, intn idx, haddr_t child,
				H5B_ins_t anchor, void *md_key);
static herr_t H5B_flush (H5F_t *f, hbool_t destroy, haddr_t addr, H5B_t *b);
static H5B_t *H5B_load (H5F_t *f, haddr_t addr, const void *_type,
			void *udata);
static herr_t H5B_decode_key (H5F_t *f, H5B_t *bt, intn idx);
static herr_t H5B_decode_keys (H5F_t *f, H5B_t *bt, intn idx);
static size_t H5B_nodesize (H5F_t *f, const H5B_class_t *type,
			    size_t *total_nkey_size, size_t sizeof_rkey);
#ifdef H5B_DEBUG
static herr_t H5B_assert (H5F_t *f, haddr_t addr, const H5B_class_t *type,
			  void *udata);
#endif

/* H5B inherits cache-like properties from H5AC */
static const H5AC_class_t H5AC_BT[1] = {{
   H5AC_BT_ID,
   (void*(*)(H5F_t*,haddr_t,const void*,void*))H5B_load,
   (herr_t(*)(H5F_t*,hbool_t,haddr_t,void*))H5B_flush,
}};

/* Is the H5B interface initialized? */
static interface_initialize_g = FALSE;


/*-------------------------------------------------------------------------
 * Function:	H5B_new
 *
 * Purpose:	Creates a new empty B-tree leaf node.  The UDATA pointer is
 *		passed as an argument to the sizeof_rkey() method for the
 *		B-tree.
 *
 * Return:	Success:	address of new node.
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Jun 23 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
haddr_t
H5B_new (H5F_t *f, const H5B_class_t *type, void *udata)
{
   H5B_t	*bt=NULL;
   haddr_t	addr;
   size_t	size, sizeof_rkey;
   size_t	total_native_keysize;
   intn		offset, i;

   FUNC_ENTER (H5B_new, NULL, FAIL);

   /*
    * Check arguments.
    */
   assert (f);
   assert (type);

   /*
    * Allocate file and memory data structures.
    */
   sizeof_rkey = (type->get_sizeof_rkey)(f, udata);
   size = H5B_nodesize (f, type, &total_native_keysize, sizeof_rkey);
   if ((addr = H5MF_alloc (f, size))<0) {
      HRETURN_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL);
   }
   bt = H5MM_xmalloc (sizeof(H5B_t));
   bt->type = type;
   bt->sizeof_rkey = sizeof_rkey;
   bt->dirty = TRUE;
   bt->ndirty = 0;
   bt->type = type;
   bt->level = 0;
   bt->left = bt->right = 0;
   bt->nchildren = 0;
   bt->page = H5MM_xcalloc (1, size); /*use calloc() to keep file clean*/
   bt->native = H5MM_xmalloc (total_native_keysize);
   bt->child = H5MM_xmalloc (2*H5B_K(f,type) * sizeof(haddr_t));
   bt->key = H5MM_xmalloc ((2*H5B_K(f,type)+1) * sizeof(H5B_key_t));

   /*
    * Initialize each entry's raw child and key pointers to point into the
    * `page' buffer.  Each native key pointer should be null until the key is
    * translated to native format.
    */
   for (i=0,offset=H5B_SIZEOF_HDR(f);
	i<2*H5B_K(f,type);
	i++,offset+=bt->sizeof_rkey+H5F_SIZEOF_OFFSET(f)) {

      bt->key[i].dirty = FALSE;
      bt->key[i].rkey = bt->page + offset;
      bt->key[i].nkey = NULL;
      bt->child[i] = 0;
   }

   /*
    * The last possible key...
    */
   bt->key[2*H5B_K(f,type)].dirty = FALSE;
   bt->key[2*H5B_K(f,type)].rkey = bt->page + offset;
   bt->key[2*H5B_K(f,type)].nkey = NULL;

   /*
    * Cache the new B-tree node.
    */
   if (H5AC_set (f, H5AC_BT, addr, bt)<0) {
      HRETURN_ERROR (H5E_BTREE, H5E_CANTINIT, FAIL);
   }

#ifdef H5B_DEBUG
   H5B_assert (f, addr, type, udata);
#endif
   FUNC_LEAVE (addr);
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
 *		matzke@llnl.gov
 *		Jun 23 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static H5B_t *
H5B_load (H5F_t *f, haddr_t addr, const void *_type, void *udata)
{
   const H5B_class_t 	*type = (const H5B_class_t *)_type;
   size_t		size, total_nkey_size;
   H5B_t		*bt = NULL;
   intn			i;
   uint8		*p;
   H5B_t		*ret_value = NULL;

   FUNC_ENTER (H5B_load, NULL, NULL);

   /* Check arguments */
   assert (f);
   assert (addr>=0);
   assert (type);
   assert (type->get_sizeof_rkey);
   
   bt = H5MM_xmalloc (sizeof(H5B_t));
   bt->sizeof_rkey = (type->get_sizeof_rkey)(f, udata);
   size = H5B_nodesize (f, type, &total_nkey_size, bt->sizeof_rkey);
   bt->type = type;
   bt->dirty = FALSE;
   bt->ndirty = 0;
   bt->page = H5MM_xmalloc (size);
   bt->native = H5MM_xmalloc (total_nkey_size);
   bt->key = H5MM_xmalloc ((2*H5B_K(f,type)+1) * sizeof(H5B_key_t));
   bt->child = H5MM_xmalloc (2 * H5B_K(f,type) * sizeof(haddr_t));
   if (H5F_block_read (f, addr, size, bt->page)<0) {
      HRETURN_ERROR (H5E_BTREE, H5E_READERROR, NULL);
   }
   p = bt->page;

   /* magic number */
   if (HDmemcmp (p, H5B_MAGIC, H5B_SIZEOF_MAGIC)) {
      HGOTO_ERROR (H5E_BTREE,  H5E_CANTLOAD, NULL);
   }
   p += 4;

   /* node type and level */
   if (*p++ != type->id) {
      HGOTO_ERROR (H5E_BTREE,  H5E_CANTLOAD, NULL);
   }
   bt->level = *p++;

   /* entries used */
   UINT16DECODE (p, bt->nchildren);

   /* sibling pointers */
   H5F_decode_offset (f, p, bt->left);
   H5F_decode_offset (f, p, bt->right);

   /* the child/key pairs */
   for (i=0; i<2*H5B_K(f,type); i++) {

      bt->key[i].dirty = FALSE;
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

   bt->key[2*H5B_K(f,type)].dirty = FALSE;
   bt->key[2*H5B_K(f,type)].rkey = p;
   bt->key[2*H5B_K(f,type)].nkey = NULL;
   ret_value = bt;

 done:
   if (!ret_value && bt) {
      H5MM_xfree (bt->child);
      H5MM_xfree (bt->key);
      H5MM_xfree (bt->page);
      H5MM_xfree (bt->native);
      H5MM_xfree (bt);
   }

   FUNC_LEAVE (ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5B_flush
 *
 * Purpose:	Flushes a dirty B-tree node to disk.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Jun 23 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5B_flush (H5F_t *f, hbool_t destroy, haddr_t addr, H5B_t *bt)
{
   intn		i;
   size_t	size = 0;
   uint8	*p = bt->page;

   FUNC_ENTER (H5B_flush, NULL, FAIL);

   /*
    * Check arguments.
    */
   assert (f);
   assert (addr>=0);
   assert (bt);
   assert (bt->type);
   assert (bt->type->encode);

   size = H5B_nodesize (f, bt->type, NULL, bt->sizeof_rkey);
   
   if (bt->dirty) {
      
      /* magic number */
      HDmemcpy (p, H5B_MAGIC, H5B_SIZEOF_MAGIC);
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
	       if ((bt->type->encode)(f, bt, bt->key[i].rkey,
				      bt->key[i].nkey)<0) {
		  HRETURN_ERROR (H5E_BTREE, H5E_CANTENCODE, FAIL);
	       }
	    }
	    bt->key[i].dirty = FALSE;
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
      if (H5F_block_write (f, addr, size, bt->page)<0) {
	 HRETURN_ERROR (H5E_BTREE, H5E_CANTFLUSH, FAIL);
      }
      bt->dirty = FALSE;
      bt->ndirty = 0;
   }

   if (destroy) {
      H5MM_xfree (bt->child);
      H5MM_xfree (bt->key);
      H5MM_xfree (bt->page);
      H5MM_xfree (bt->native);
      H5MM_xfree (bt);
   }

   FUNC_LEAVE (SUCCEED);
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
 *		matzke@llnl.gov
 *		Jun 23 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5B_find (H5F_t *f, H5B_class_t *type, haddr_t addr, void *udata)
{
   H5B_t	*bt=NULL;
   intn		idx=-1, lt=0, rt, cmp=1;
   int		ret_value = FAIL;

   FUNC_ENTER (H5B_find, NULL, FAIL);

   /*
    * Check arguments.
    */
   assert (f);
   assert (type);
   assert (type->decode);
   assert (type->cmp3);
   assert (type->found);
   assert (addr>=0);
   
   /*
    * Perform a binary search to locate the child which contains
    * the thing for which we're searching.
    */
   if (NULL==(bt=H5AC_protect (f, H5AC_BT, addr, type, udata))) {
      HGOTO_ERROR (H5E_BTREE, H5E_CANTLOAD, FAIL);
   }
   rt = bt->nchildren;

   while (lt<rt && cmp) {
      idx = (lt + rt) / 2;
      if (H5B_decode_keys (f, bt, idx)<0) {
	 HGOTO_ERROR (H5E_BTREE, H5E_CANTDECODE, FAIL);
      }

      /* compare */
      if ((cmp=(type->cmp3)(f, bt->key[idx].nkey, udata,
			    bt->key[idx+1].nkey))<0) {
	 rt = idx;
      } else {
	 lt = idx+1;
      }
   }
   if (cmp) {
      HGOTO_ERROR (H5E_BTREE, H5E_NOTFOUND, FAIL);
   }

   /*
    * Follow the link to the subtree or to the data node.
    */
   assert (idx>=0 && idx<bt->nchildren);
   if (bt->level > 0) {
      if ((ret_value = H5B_find (f, type, bt->child[idx], udata))<0) {
	 HGOTO_ERROR (H5E_BTREE, H5E_NOTFOUND, FAIL);
      }
   } else {
      ret_value = (type->found)(f, bt->child[idx], bt->key[idx].nkey,
				udata, bt->key[idx+1].nkey);
      if (ret_value<0) {
	 HGOTO_ERROR (H5E_BTREE, H5E_NOTFOUND, FAIL);
      }
   }

done:
   if (bt && H5AC_unprotect (f, H5AC_BT, addr, bt)<0) {
      HRETURN_ERROR (H5E_BTREE, H5E_PROTECT, FAIL);
   }
   FUNC_LEAVE (ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5B_split
 *
 * Purpose:	Split a single node into two nodes.  The old node will
 *		contain the left children and the new node will contain the
 *		right children.
 *
 * 		The UDATA pointer is passed to the sizeof_rkey() method but is
 * 		otherwise unused.
 *
 * 		The OLD_BT argument is a pointer to a protected B-tree
 *		node.
 *
 * Return:	Success:	Address of the new node.
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Jul  3 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static haddr_t
H5B_split (H5F_t *f, H5B_class_t *type, H5B_t *old_bt, haddr_t old_addr,
	   void *udata)
{
   H5B_t	*new_bt=NULL, *tmp_bt=NULL;
   haddr_t	ret_value=FAIL, new_addr=FAIL;
   intn		i, k;
   size_t	recsize = 0;

   FUNC_ENTER (H5B_split, NULL, FAIL);

   /*
    * Check arguments.
    */
   assert (f);
   assert (type);
   assert (old_addr>=0);

   /*
    * Initialize variables.
    */
   assert (old_bt->nchildren == 2*H5B_K(f,type));
   recsize = old_bt->sizeof_rkey + H5F_SIZEOF_OFFSET(f);
   k = H5B_K(f,type);

   /*
    * Create the new B-tree node.
    */
   if ((new_addr = H5B_new (f, type, udata))<0) {
      HGOTO_ERROR (H5E_BTREE, H5E_CANTINIT, FAIL);
   }
   if (NULL==(new_bt=H5AC_protect (f, H5AC_BT, new_addr, type, udata))) {
      HGOTO_ERROR (H5E_BTREE, H5E_CANTLOAD, FAIL);
   }
   new_bt->level = old_bt->level;

   /*
    * Copy data from the old node to the new node.
    */
   HDmemcpy (new_bt->page + H5B_SIZEOF_HDR(f),
	     old_bt->page + H5B_SIZEOF_HDR(f) + k*recsize,
	     k*recsize + new_bt->sizeof_rkey);
   HDmemcpy (new_bt->native,
	     old_bt->native + k*type->sizeof_nkey,
	     (k+1) * type->sizeof_nkey);

   for (i=0; i<=k; i++) {
      /* key */
      new_bt->key[i].dirty = old_bt->key[k+i].dirty;
      if (old_bt->key[k+i].nkey) {
	 new_bt->key[i].nkey = new_bt->native + i*type->sizeof_nkey;
      }
      /* child */
      if (i<k) {
	 new_bt->child[i] = old_bt->child[k+i];
      }
   }
   new_bt->ndirty = new_bt->nchildren = k;

   /*
    * Truncate the old node.
    */
   old_bt->dirty = TRUE;
   old_bt->ndirty = old_bt->nchildren = k;
   
   /*
    * Update sibling pointers.
    */
   new_bt->left = old_addr;
   new_bt->right = old_bt->right;

   if (old_bt->right) {
      if (NULL==(tmp_bt=H5AC_find (f, H5AC_BT, old_bt->right, type,
				   udata))) {
	 HGOTO_ERROR (H5E_BTREE, H5E_CANTLOAD, FAIL);
      }
      tmp_bt->dirty = TRUE;
      tmp_bt->left = new_addr;
   }
   old_bt->right = new_addr;

   HGOTO_DONE (new_addr);

done:
   {
      if (new_bt && H5AC_unprotect (f, H5AC_BT, new_addr, new_bt)<0) {
	 HRETURN_ERROR (H5E_BTREE, H5E_PROTECT, FAIL);
      }
   }
   FUNC_LEAVE (ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5B_decode_key
 *
 * Purpose:	Decode the specified key into native format.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Jul  8 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5B_decode_key (H5F_t *f, H5B_t *bt, intn idx)
{
   FUNC_ENTER (H5B_decode_key, NULL, FAIL);
   
   bt->key[idx].nkey = bt->native + idx * bt->type->sizeof_nkey;
   if ((bt->type->decode)(f, bt, bt->key[idx].rkey,
			  bt->key[idx].nkey)<0) {
      HRETURN_ERROR (H5E_BTREE, H5E_CANTDECODE, FAIL);
   }

   FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5B_decode_keys
 *
 * Purpose:	Decode keys on either side of the specified branch.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Tuesday, October 14, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5B_decode_keys (H5F_t *f, H5B_t *bt, intn idx)
{
   FUNC_ENTER (H5B_decode_keys, NULL, FAIL);

   assert (f);
   assert (bt);
   assert (idx>=0 && idx<bt->nchildren);

   if (!bt->key[idx].nkey && H5B_decode_key (f, bt, idx)<0) {
      HRETURN_ERROR (H5E_BTREE, H5E_CANTDECODE, FAIL);
   }
   if (!bt->key[idx+1].nkey && H5B_decode_key (f, bt, idx+1)<0) {
      HRETURN_ERROR (H5E_BTREE, H5E_CANTDECODE, FAIL);
   }

   FUNC_LEAVE (SUCCEED);
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
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Jun 23 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
haddr_t
H5B_insert (H5F_t *f, H5B_class_t *type, haddr_t addr, void *udata)
{
   uint8	lt_key[256], md_key[256], rt_key[256];
   hbool_t	lt_key_changed=FALSE, rt_key_changed=FALSE;
   haddr_t	child, new_root;
   intn		level;
   H5B_t	*bt;
   size_t 	size;
   uint8 	*buf;
   haddr_t	tmp_addr;
   H5B_ins_t	anchor = H5B_INS_ERROR;

   FUNC_ENTER (H5B_insert, NULL, FAIL);

   /*
    * Check arguments.
    */
   assert (f);
   assert (type);
   assert (type->sizeof_nkey <= sizeof lt_key);

   child = H5B_insert_helper (f, addr, type, &anchor, lt_key, &lt_key_changed,
			      md_key, udata, rt_key, &rt_key_changed);
   if (child<0 || anchor<0) {
      HRETURN_ERROR (H5E_BTREE, H5E_CANTINIT, FAIL);
   }
   if (H5B_INS_NOOP==anchor) HRETURN (addr);
   assert (H5B_INS_RIGHT==anchor);

   /* the current root */
   if (NULL==(bt = H5AC_find (f, H5AC_BT, addr, type, udata))) {
      HRETURN_ERROR (H5E_BTREE, H5E_CANTLOAD, FAIL);
   }
   level = bt->level;
   if (!lt_key_changed) {
      if (!bt->key[0].nkey && H5B_decode_key (f, bt, 0)<0) {
	 HRETURN_ERROR (H5E_BTREE, H5E_CANTDECODE, FAIL);
      }
      HDmemcpy (lt_key, bt->key[0].nkey, type->sizeof_nkey);
   }

   /* the new node */
   if (NULL==(bt = H5AC_find (f, H5AC_BT, child, type, udata))) {
      HRETURN_ERROR (H5E_BTREE, H5E_CANTLOAD, FAIL);
   }
   if (!rt_key_changed) {
      if (!bt->key[bt->nchildren].nkey &&
	  H5B_decode_key (f, bt, bt->nchildren)<0) {
	 HRETURN_ERROR (H5E_BTREE, H5E_CANTDECODE, FAIL);
      }
      HDmemcpy (rt_key, bt->key[bt->nchildren].nkey, type->sizeof_nkey);
   }

   /*
    * Copy the old root node to some other file location and make the new
    * root at the old root's previous address.  This prevents the B-tree
    * from "moving".
    */
   size = H5B_nodesize (f, type, NULL, bt->sizeof_rkey);
   buf = H5MM_xmalloc (size);
   tmp_addr = H5MF_alloc (f, size);

   if (tmp_addr<0) {
      HRETURN_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL);
   }
   if (H5AC_flush (f, H5AC_BT, addr, FALSE)<0) {
      HRETURN_ERROR (H5E_BTREE, H5E_CANTFLUSH, FAIL);
   }
   if (H5F_block_read (f, addr, size, buf)<0) {
      HRETURN_ERROR (H5E_BTREE, H5E_READERROR, FAIL);
   }
   if (H5F_block_write (f, tmp_addr, size, buf)<0) {
      HRETURN_ERROR (H5E_BTREE, H5E_WRITEERROR, FAIL);
   }
   if (H5AC_rename (f, H5AC_BT, addr, tmp_addr)<0) {
      HRETURN_ERROR (H5E_BTREE, H5E_CANTSPLIT, FAIL);
   }

   buf = H5MM_xfree (buf);
   new_root = addr;
   addr = tmp_addr;

   /* update the new child's left pointer */
   if (NULL==(bt=H5AC_find (f, H5AC_BT, child, type, udata))) {
      HRETURN_ERROR (H5E_BTREE, H5E_CANTLOAD, FAIL);
   }
   bt->dirty = TRUE;
   bt->left = addr;

   /* clear the old root at the old address */
   if (NULL==(bt=H5AC_find (f, H5AC_BT, new_root, type, udata))) {
      HRETURN_ERROR (H5E_BTREE, H5E_CANTLOAD, FAIL);
   }
   bt->dirty = TRUE;
   bt->ndirty = 0;
   bt->left = 0;
   bt->right = 0;
   bt->nchildren = 0;


   /* the new root */
   if (NULL==(bt = H5AC_find (f, H5AC_BT, new_root, type, udata))) {
      HRETURN_ERROR (H5E_BTREE, H5E_CANTLOAD, FAIL);
   }
   bt->dirty = TRUE;
   bt->ndirty = 2;
   bt->level = level+1;
   bt->nchildren = 2;
   
   bt->child[0] = addr;
   bt->key[0].dirty = TRUE;
   bt->key[0].nkey = bt->native;
   HDmemcpy (bt->key[0].nkey, lt_key, type->sizeof_nkey);

   bt->child[1] = child;
   bt->key[1].dirty = TRUE;
   bt->key[1].nkey = bt->native + type->sizeof_nkey;
   HDmemcpy (bt->key[1].nkey, md_key, type->sizeof_nkey);

   bt->key[2].dirty = TRUE;
   bt->key[2].nkey = bt->native + 2 * type->sizeof_nkey;
   HDmemcpy (bt->key[2].nkey, rt_key, type->sizeof_nkey);

#ifdef H5B_DEBUG
   H5B_assert (f, new_root, type, udata);
#endif
   FUNC_LEAVE (new_root);
}


/*-------------------------------------------------------------------------
 * Function:	H5B_insert_child
 *
 * Purpose:	Insert a child at the specified address with the
 *		specified left or right key.  The BT argument is a pointer
 *		to a protected B-tree node.
 *
 * Return:	Success:	SUCCEED
 *
 * 		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Jul  8 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5B_insert_child (H5F_t *f, const H5B_class_t *type, H5B_t *bt,
		  intn idx, haddr_t child, H5B_ins_t anchor, void *md_key)
{
   size_t	recsize;
   intn		i;

   FUNC_ENTER (H5B_insert_child, NULL, FAIL);
   assert (bt);

   bt->dirty = TRUE;
   recsize = bt->sizeof_rkey + H5F_SIZEOF_OFFSET(f);
   
   if (H5B_INS_RIGHT==anchor) {
      /*
       * The MD_KEY is the left key of the new node.
       */
      HDmemmove (bt->page + H5B_SIZEOF_HDR(f) + (idx+1)*recsize,
		 bt->page + H5B_SIZEOF_HDR(f) + idx*recsize,
		 (bt->nchildren-idx)*recsize + bt->sizeof_rkey);

      HDmemmove (bt->native + (idx+1) * type->sizeof_nkey,
		 bt->native + idx * type->sizeof_nkey,
		 ((bt->nchildren-idx)+1) * type->sizeof_nkey);

      for (i=bt->nchildren; i>=idx; --i) {
	 bt->key[i+1].dirty = bt->key[i].dirty;
	 if (bt->key[i].nkey) {
	    bt->key[i+1].nkey = bt->native + (i+1) * type->sizeof_nkey;
	 } else {
	    bt->key[i+1].nkey = NULL;
	 }
      }
      bt->key[idx].dirty = TRUE;
      bt->key[idx].nkey = bt->native + idx * type->sizeof_nkey;
      HDmemcpy (bt->key[idx].nkey, md_key, type->sizeof_nkey);

   } else {
      /*
       * The MD_KEY is the right key of the new node.
       */
      HDmemmove (bt->page + (H5B_SIZEOF_HDR(f) +
			     (idx+1)*recsize + bt->sizeof_rkey),
		 bt->page + (H5B_SIZEOF_HDR(f) +
			     idx*recsize + bt->sizeof_rkey),
		 (bt->nchildren-idx) * recsize);

      HDmemmove (bt->native + (idx+2)*type->sizeof_nkey,
		 bt->native + (idx+1)*type->sizeof_nkey,
		 (bt->nchildren-idx) * type->sizeof_nkey);

      for (i=bt->nchildren; i>idx; --i) {
	 bt->key[i+1].dirty = bt->key[i].dirty;
	 if (bt->key[i].nkey) {
	    bt->key[i+1].nkey = bt->native + (i+1) * type->sizeof_nkey;
	 } else {
	    bt->key[i+1].nkey = NULL;
	 }
      }
      bt->key[idx+1].dirty = TRUE;
      bt->key[idx+1].nkey = bt->native + (idx+1) * type->sizeof_nkey;
      HDmemcpy (bt->key[idx+1].nkey, md_key, type->sizeof_nkey);
   }

   HDmemmove (bt->child + idx + 1,
	      bt->child + idx,
	      (bt->nchildren - idx) * sizeof(haddr_t));

   bt->child[idx] = child;
   bt->nchildren += 1;
   bt->ndirty = bt->nchildren;

   FUNC_LEAVE (SUCCEED);
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
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Jul  9 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static haddr_t
H5B_insert_helper (H5F_t *f, haddr_t addr, H5B_class_t *type,
		   H5B_ins_t *parent_ins, 
		   uint8 *lt_key, hbool_t *lt_key_changed,
		   uint8 *md_key, void *udata,
		   uint8 *rt_key, hbool_t *rt_key_changed)
{
   H5B_t	*bt=NULL, *twin=NULL, *tmp_bt=NULL;
   intn		lt=0, idx=-1, rt, cmp=-1;
   haddr_t	child_addr=0, twin_addr=0, ret_value=FAIL;
   H5B_ins_t	my_ins = H5B_INS_ERROR;

   FUNC_ENTER (H5B_insert_helper, NULL, FAIL);

   /*
    * Check arguments
    */
   assert (f);
   assert (addr>=0);
   assert (type);
   assert (type->decode);
   assert (type->cmp3);
   assert (type->new);
   assert (parent_ins && H5B_INS_ERROR==*parent_ins);
   assert (lt_key);
   assert (lt_key_changed);
   assert (rt_key);
   assert (rt_key_changed);

   *lt_key_changed = FALSE;
   *rt_key_changed = FALSE;

   /*
    * Use a binary search to find the child that will receive the new
    * data.  When the search completes IDX points to the child that
    * should get the new data.
    */
   if (NULL==(bt=H5AC_protect (f, H5AC_BT, addr, type, udata))) {
      HGOTO_ERROR (H5E_BTREE, H5E_CANTLOAD, FAIL);
   }
   rt = bt->nchildren;

   while (lt<rt && cmp) {
      idx = (lt + rt) / 2;
      if (H5B_decode_keys (f, bt, idx)<0) {
	 HRETURN_ERROR (H5E_BTREE, H5E_CANTDECODE, FAIL);
      }
      if ((cmp=(type->cmp3)(f, bt->key[idx].nkey, udata,
			    bt->key[idx+1].nkey))<0) {
	 rt = idx;
      } else {
	 lt = idx+1;
      }
   }

   if (0==bt->nchildren) {
      /*
       * The value being inserted will be the only value in this tree. We
       * must necessarily be at level zero.
       */
      assert (0==bt->level);
      bt->key[0].nkey = bt->native;
      bt->key[1].nkey = bt->native + type->sizeof_nkey;
      if ((child_addr=(type->new)(f, H5B_INS_FIRST, bt->key[0].nkey, udata,
				  bt->key[1].nkey))<0) {
	 bt->key[0].nkey = bt->key[1].nkey = NULL;
	 HGOTO_ERROR (H5E_BTREE, H5E_CANTINIT, FAIL);
      }
      bt->nchildren = 1;
      bt->dirty = TRUE;
      bt->ndirty = 1;
      bt->child[0] = child_addr;
      bt->key[0].dirty = TRUE;
      bt->key[1].dirty = TRUE;
      idx = 0;

      if (type->follow_min) {
	 child_addr = (type->insert)(f, bt->child[idx], &my_ins,
				     bt->key[idx].nkey, lt_key_changed,
				     md_key, udata,
				     bt->key[idx+1].nkey, rt_key_changed);
      } else {
	 my_ins = H5B_INS_NOOP;
      }
      
   } else if (cmp<0 && idx<=0 && bt->level>0) {
      /*
       * The value being inserted is less than any value in this tree.  Follow
       * the minimum branch out of this node to a subtree.
       */
      idx = 0;
      if (H5B_decode_keys (f, bt, idx)<0) {
	 HGOTO_ERROR (H5E_BTREE, H5E_CANTDECODE, FAIL);
      }
      child_addr = H5B_insert_helper (f, bt->child[idx], type, &my_ins, 
				      bt->key[idx].nkey, lt_key_changed,
				      md_key, udata,
				      bt->key[idx+1].nkey, rt_key_changed);
      
   } else if (cmp<0 && idx<=0 && type->follow_min) {
      /*
       * The value being inserted is less than any leaf node out of this
       * current node.  Follow the minimum branch to a leaf node and let the
       * subclass handle the problem.
       */
      idx = 0;
      if (H5B_decode_keys (f, bt, idx)<0) {
	 HGOTO_ERROR (H5E_BTREE, H5E_CANTDECODE, FAIL);
      }
      child_addr = (type->insert)(f, bt->child[idx], &my_ins,
				  bt->key[idx].nkey, lt_key_changed,
				  md_key, udata,
				  bt->key[idx+1].nkey, rt_key_changed);
      
   } else if (cmp<0 && idx<=0) {
      /*
       * The value being inserted is less than any leaf node out of the
       * current node. Create a new minimum leaf node out of this B-tree
       * node. This node is not empty (handled above).
       */
      idx = 0;
      if (H5B_decode_keys (f, bt, idx)<0) {
	 HGOTO_ERROR (H5E_BTREE, H5E_CANTDECODE, FAIL);
      }
      my_ins = H5B_INS_LEFT;
      HDmemcpy (md_key, bt->key[idx].nkey, type->sizeof_nkey);
      child_addr = (type->new)(f, H5B_INS_LEFT, bt->key[idx].nkey,
			       udata, md_key);
      *lt_key_changed = TRUE;
      
   } else if (cmp>0 && idx+1>=bt->nchildren && bt->level>0) {
      /*
       * The value being inserted is larger than any value in this tree.
       * Follow the maximum branch out of this node to a subtree.
       */
      idx = bt->nchildren - 1;
      if (H5B_decode_keys (f, bt, idx)<0) {
	 HGOTO_ERROR (H5E_BTREE, H5E_CANTDECODE, FAIL);
      }
      child_addr = H5B_insert_helper (f, bt->child[idx], type, &my_ins, 
				      bt->key[idx].nkey, lt_key_changed,
				      md_key, udata,
				      bt->key[idx+1].nkey, rt_key_changed);
      
   } else if (cmp>0 && idx+1>=bt->nchildren && type->follow_max) {
      /*
       * The value being inserted is larger than any leaf node out of the
       * current node.  Follow the maximum branch to a leaf node and let the
       * subclass handle the problem.
       */
      idx = bt->nchildren - 1;
      if (H5B_decode_keys (f, bt, idx)<0) {
	 HGOTO_ERROR (H5E_BTREE, H5E_CANTDECODE, FAIL);
      }
      child_addr = (type->insert)(f, bt->child[idx], &my_ins,
				  bt->key[idx].nkey, lt_key_changed,
				  md_key, udata,
				  bt->key[idx+1].nkey, rt_key_changed);
      
   } else if (cmp>0 && idx+1>=bt->nchildren) {
      /*
       * The value being inserted is larger than any leaf node out of the
       * current node.  Create a new maximum leaf node out of this B-tree
       * node.
       */
      idx = bt->nchildren - 1;
      if (H5B_decode_keys (f, bt, idx)<0) {
	 HGOTO_ERROR (H5E_BTREE, H5E_CANTDECODE, FAIL);
      }
      my_ins = H5B_INS_RIGHT;
      HDmemcpy (md_key, bt->key[idx+1].nkey, type->sizeof_nkey);
      child_addr = (type->new)(f, H5B_INS_RIGHT, md_key,
			       udata, bt->key[idx+1].nkey);
      *rt_key_changed = TRUE;
      
   } else if (cmp) {
      /*
       * We couldn't figure out which branch to follow out of this node. THIS
       * IS A MAJOR PROBLEM THAT NEEDS TO BE FIXED --rpm.
       */
      assert ("INTERNAL HDF5 ERROR (see rpm)" && 0);
      
   } else if (bt->level>0) {
      /*
       * Follow a branch out of this node to another subtree.
       */
      assert (idx>=0 && idx<bt->nchildren);
      child_addr = H5B_insert_helper (f, bt->child[idx], type, &my_ins, 
				      bt->key[idx].nkey, lt_key_changed,
				      md_key, udata,
				      bt->key[idx+1].nkey, rt_key_changed);
      
      
   } else {
      /*
       * Follow a branch out of this node to a leaf node of some other type.
       */
      assert (idx>=0 && idx<bt->nchildren);
      child_addr = (type->insert)(f, bt->child[idx], &my_ins,
				  bt->key[idx].nkey, lt_key_changed,
				  md_key, udata,
				  bt->key[idx+1].nkey, rt_key_changed);
      
   }

   if (child_addr<0 || my_ins<0) {
      /* Insertion failed */
      HGOTO_ERROR (H5E_BTREE, H5E_CANTINSERT, FAIL);
   }
   

   /*
    * Update the left and right keys of the current node.
    */
   if (*lt_key_changed) {
      bt->dirty = TRUE;
      bt->key[idx].dirty = TRUE;
      if (idx>0) {
	 *lt_key_changed = FALSE;
      } else {
	 HDmemcpy (lt_key, bt->key[idx].nkey, type->sizeof_nkey);
      }
   }
   if (*rt_key_changed) {
      bt->dirty = TRUE;
      bt->key[idx+1].dirty = TRUE;
      if (idx+1<bt->nchildren) {
	 *rt_key_changed = FALSE;
      } else {
	 HDmemcpy (rt_key, bt->key[idx+1].nkey, type->sizeof_nkey);
      }
   }

   if (H5B_INS_CHANGE==my_ins) {
      /*
       * The insertion simply changed the address for the child.
       */
      bt->child[idx] = child_addr;
      bt->dirty = TRUE;
      bt->ndirty = MAX (bt->ndirty, idx+1);
      *parent_ins = H5B_INS_NOOP;

   } else if (H5B_INS_LEFT==my_ins || H5B_INS_RIGHT==my_ins) {
      /* Make sure IDX is the slot number for the new node. */
      if (H5B_INS_RIGHT==my_ins) idx++;

      /* If this node is full then split it before inserting the new child. */
      if (bt->nchildren==2*H5B_K (f, type)) {
	 if ((twin_addr=H5B_split (f, type, bt, addr, udata))<0) {
	    HGOTO_ERROR (H5E_BTREE, H5E_CANTSPLIT, FAIL);/*can't split node*/
	 }
	 if (NULL==(twin=H5AC_protect (f, H5AC_BT, twin_addr, type, udata))) {
	    HGOTO_ERROR (H5E_BTREE, H5E_CANTLOAD, FAIL);/*can't load B-tree*/
	 }
	 if (idx<=H5B_K (f, type)) {
	    tmp_bt = bt;
	 } else {
	    idx -= H5B_K (f, type);
	    tmp_bt = twin;
	 }
      } else {
	 tmp_bt = bt;
      }

      /* Insert the child */
      if (H5B_insert_child (f, type, tmp_bt, idx, child_addr, my_ins,
			    md_key)<0) {
	 HGOTO_ERROR (H5E_BTREE, H5E_CANTINSERT, FAIL);/*can't insert child*/
      }
   }


   /*
    * If this node split, return the mid key (the one that is shared
    * by the left and right node).
    */
   if (twin) {
      if (!twin->key[0].nkey && H5B_decode_key (f, twin, 0)<0) {
	 HGOTO_ERROR (H5E_BTREE, H5E_CANTDECODE, FAIL);
      }
      HDmemcpy (md_key, twin->key[0].nkey, type->sizeof_nkey);
      *parent_ins = H5B_INS_RIGHT;
#ifndef NDEBUG
      /*
       * The max key in the original left node must be equal to the min key
       * in the new node.
       */
      if (!bt->key[bt->nchildren].nkey) {
	 herr_t status = H5B_decode_key (f, bt, bt->nchildren);
	 assert (status>=0);
	 cmp = (type->cmp2)(f, bt->key[bt->nchildren].nkey, udata,
			    twin->key[0].nkey);
	 assert (0==cmp);
      }
#endif
   } else {
      *parent_ins = H5B_INS_NOOP;
   }
   
   HGOTO_DONE (twin_addr);

done:
   {
      herr_t e1 = (bt && H5AC_unprotect (f, H5AC_BT, addr, bt)<0);
      herr_t e2 = (twin && H5AC_unprotect (f, H5AC_BT, twin_addr, twin)<0);
      if (e1 || e2) { /*use vars to prevent short-circuit of side effects*/
	 HRETURN_ERROR (H5E_BTREE, H5E_PROTECT, FAIL);
      }
   }

   FUNC_LEAVE (ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5B_list
 *
 * Purpose:	Calls the list callback for each leaf node of the
 *		B-tree, passing it the UDATA structure.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Jun 23 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5B_list (H5F_t *f, H5B_class_t *type, haddr_t addr, void *udata)
{
   H5B_t	*bt=NULL;
   haddr_t	next_addr;
   intn		i;
   herr_t	ret_value = FAIL;

   FUNC_ENTER (H5B_list, NULL, FAIL);

   /*
    * Check arguments.
    */
   assert (f);
   assert (type);
   assert (type->list);
   assert (addr>=0);
   assert (udata);
   
   if (NULL==(bt = H5AC_find (f, H5AC_BT, addr, type, udata))) {
      HRETURN_ERROR (H5E_BTREE, H5E_CANTLOAD, FAIL);
   }
   
   if (bt->level>0) {
      if (H5B_list (f, type, bt->child[0], udata)<0) {
	 HRETURN_ERROR (H5E_BTREE, H5E_CANTLIST, FAIL);
      } else {
	 HRETURN (SUCCEED);
      }
   } else {

      for (/*void*/; addr>0; addr=next_addr) {
	 if (NULL==(bt=H5AC_protect (f, H5AC_BT, addr, type, udata))) {
	    HGOTO_ERROR (H5E_BTREE, H5E_CANTLOAD, FAIL);
	 }
	 
	 for (i=0; i<bt->nchildren; i++) {
	    if ((type->list)(f, bt->child[i], udata)<0) {
	       HGOTO_ERROR (H5E_BTREE, H5E_CANTLOAD, FAIL);
	    }
	 }

	 next_addr = bt->right;
	 if (H5AC_unprotect (f, H5AC_BT, addr, bt)<0) {
	    HRETURN_ERROR (H5E_BTREE, H5E_PROTECT, FAIL);
	 }
	 bt = NULL;
      }
   }
   HGOTO_DONE (SUCCEED);

done:
   if (bt && H5AC_unprotect (f, H5AC_BT, addr, bt)<0) {
      HRETURN_ERROR (H5E_BTREE, H5E_PROTECT, FAIL);
   }
   FUNC_LEAVE (ret_value);
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
 *		Failure:	0
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Jul  3 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static size_t
H5B_nodesize (H5F_t *f, const H5B_class_t *type,
	      size_t *total_nkey_size, size_t sizeof_rkey)
{
   size_t	size;
   
   FUNC_ENTER (H5B_nodesize, NULL, (size_t)0);

   /*
    * Check arguments.
    */
   assert (f);
   assert (type);
   assert (sizeof_rkey>0);
   assert (H5B_K (f, type)>0);

   /*
    * Total native key size.
    */
   if (total_nkey_size) {
      *total_nkey_size = (2 * H5B_K(f,type) + 1) * type->sizeof_nkey;
   }

   /*
    * Total node size.
    */
   size = (H5B_SIZEOF_HDR(f) +				/*node header	*/
	   2 * H5B_K(f,type) * H5F_SIZEOF_OFFSET(f) +	/*child pointers*/
	   (2*H5B_K(f,type)+1) * sizeof_rkey);		/*keys		*/

   FUNC_LEAVE (size);
}


/*-------------------------------------------------------------------------
 * Function:	H5B_debug
 *
 * Purpose:	Prints debugging info about a B-tree.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug  4 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5B_debug (H5F_t *f, haddr_t addr, FILE *stream, intn indent,
	   intn fwidth, H5B_class_t *type, void *udata)
{
   H5B_t		*bt = NULL;
   int			i;

   FUNC_ENTER (H5B_debug, NULL, FAIL);

   /*
    * Check arguments.
    */
   assert (f);
   assert (addr>=0);
   assert (stream);
   assert (indent>=0);
   assert (fwidth>=0);
   assert (type);

   /*
    * Load the tree node.
    */
   if (NULL==(bt=H5AC_find (f, H5AC_BT, addr, type, udata))) {
      HRETURN_ERROR (H5E_BTREE, H5E_CANTLOAD, FAIL);
   }

   /*
    * Print the values.
    */
   fprintf (stream, "%*s%-*s %d\n", indent, "", fwidth,
	    "Tree type ID:",
	    (int)(bt->type->id));
   fprintf (stream, "%*s%-*s %lu\n", indent, "", fwidth,
	    "Size of raw (disk) key:",
	    (unsigned long)(bt->sizeof_rkey));
   fprintf (stream, "%*s%-*s %s\n", indent, "", fwidth,
	    "Dirty flag:",
	    bt->dirty?"True":"False");
   fprintf (stream, "%*s%-*s %d\n", indent, "", fwidth,
	    "Number of initial dirty children:",
	    (int)(bt->ndirty));
   fprintf (stream, "%*s%-*s %d\n", indent, "", fwidth,
	    "Level:",
	    (int)(bt->level));
   fprintf (stream, "%*s%-*s %lu\n", indent, "", fwidth,
	    "Address of left sibling:",
	    (unsigned long)(bt->left));
   fprintf (stream, "%*s%-*s %lu\n", indent, "", fwidth,
	    "Address of right sibling:",
	    (unsigned long)(bt->right));
   fprintf (stream, "%*s%-*s %d (%d)\n", indent, "", fwidth,
	    "Number of children (max):",
	    (int)(bt->nchildren),
	    (int)(2*H5B_K(f,type)));

   /*
    * Print the child addresses
    */
   for (i=0; i<bt->nchildren; i++) {
      fprintf (stream, "%*sChild %d...\n", indent, "", i);
      fprintf (stream, "%*s%-*s %lu\n", indent+3, "", MAX(0,fwidth-3),
	       "Address:",
	       (unsigned long)(bt->child[i]));
   }

   FUNC_LEAVE (SUCCEED);
}



/*-------------------------------------------------------------------------
 * Function:	H5B_assert
 *
 * Purpose:	Verifies that the tree is structured correctly.
 *
 * Return:	Success:	SUCCEED
 *
 * 		Failure:	aborts if something is wrong.
 *
 * Programmer:	Robb Matzke
 *              Tuesday, November  4, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
#ifdef H5B_DEBUG
static herr_t
H5B_assert (H5F_t *f, haddr_t addr, const H5B_class_t *type, void *udata)
{
   H5B_t	*bt = NULL;
   intn		i, ncell, cmp;
   static int	ncalls=0;
   herr_t	status;

   /* A queue of child data */
   struct child_t {
      haddr_t	addr;
      int	level;
      struct child_t *next;
   } *head=NULL, *tail=NULL, *prev=NULL, *cur=NULL, *tmp=NULL;

   FUNC_ENTER (H5B_assert, NULL, FAIL);
   if (0==ncalls++) {
      fprintf (stderr, "HDF5-DIAG: debugging B-trees (expensive)\n");
   }
   
   /* Initialize the queue */
   bt = H5AC_find (f, H5AC_BT, addr, type, udata);
   assert (bt);
   cur = H5MM_xcalloc (1, sizeof(struct child_t));
   cur->addr = addr;
   cur->level = bt->level;
   head = tail = cur;
   
   /*
    * Do a breadth-first search of the tree.  New nodes are added to the end
    * of the queue as the `cur' pointer is advanced toward the end.  We don't
    * remove any nodes from the queue because we need them in the uniqueness
    * test.
    */
   for (ncell=0; cur; ncell++) {
      bt = H5AC_protect (f, H5AC_BT, cur->addr, type, udata);
      assert (bt);

      /* Check node header */
      assert (bt->ndirty>=0 && bt->ndirty<=bt->nchildren);
      assert (bt->level==cur->level);
      if (cur->next && cur->next->level==bt->level) {
	 assert (bt->right==cur->next->addr);
      } else {
	 assert (bt->right==0);
      }
      if (prev && prev->level==bt->level) {
	 assert (bt->left==prev->addr);
      } else {
	 assert (bt->left==0);
      }
      
      if (cur->level>0) {
	 for (i=0; i<bt->nchildren; i++) {

	    /*
	     * Check that child nodes haven't already been seen.  If they
	     * have then the tree has a cycle.
	     */
	    for (tmp=head; tmp; tmp=tmp->next) {
	       assert (tmp->addr != bt->child[i]);
	    }

	    /* Add the child node to the end of the queue */
	    tmp = H5MM_xcalloc (1, sizeof(struct child_t));
	    tmp->addr = bt->child[i];
	    tmp->level = bt->level - 1;
	    tail->next = tmp;
	    tail = tmp;

	    /* Check that the keys are monotonically increasing */
	    status = H5B_decode_keys (f, bt, i);
	    assert (status>=0);
	    cmp = (type->cmp2)(f, bt->key[i].nkey, udata, bt->key[i+1].nkey);
	    assert (cmp<0);
	 }
      }

      /* Release node */
      status = H5AC_unprotect (f, H5AC_BT, cur->addr, bt);
      assert (status>=0);

      /* Advance current location in queue */
      prev = cur;
      cur = cur->next;
   }

   /* Free all entries from queue */
   while (head) {
      tmp = head->next;
      H5MM_xfree (head);
      head = tmp;
   }

   FUNC_LEAVE (SUCCEED);
}
#endif /* H5B_DEBUG */
