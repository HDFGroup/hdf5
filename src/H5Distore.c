/*
 * Copyright (C) 1997 Spizella Software
 *                    All rights reserved.
 *
 * Programmer: Robb Matzke <robb@arborea.spizella.com>
 *             Wednesday, October  8, 1997
 */
#include <H5private.h>
#include <H5Eprivate.h>
#include <H5Fprivate.h>
#include <H5MFprivate.h>
#include <H5MMprivate.h>
#include <H5Oprivate.h>
#include <H5Vprivate.h>

typedef enum H5F_isop_t {
   H5F_ISTORE_READ, 			/*read from file to memory	*/
   H5F_ISTORE_WRITE			/*write from memory to file	*/
} H5F_isop_t;

/* Does the array domain include negative indices? */
#undef H5F_ISTORE_NEGATIVE_DOMAIN


#define PABLO_MASK	H5F_istore_mask

/* Is the interface initialized? */
static hbool_t interface_initialize_g = FALSE;

/* PRIVATE PROTOTYPES */
static size_t H5F_istore_sizeof_rkey (H5F_t *f, const void *_udata);
static haddr_t H5F_istore_new (H5F_t *f, void *_lt_key, void *_udata,
			       void *_rt_key);
static intn H5F_istore_cmp (H5F_t *f, void *_lt_key, void *_udata,
			    void *_rt_key);
static herr_t H5F_istore_found (H5F_t *f, haddr_t addr, const void *_lt_key,
				void *_udata, const void *_rt_key);
static haddr_t H5F_istore_insert (H5F_t *f, haddr_t addr, H5B_ins_t *anchor,
				  void *_lt_key, hbool_t *lt_key_changed,
				  void *_md_key, void *_udata,
				  void *_rt_key, hbool_t *rt_key_changed);
static herr_t H5F_istore_decode_key (H5F_t *f, H5B_t *bt, uint8 *raw,
				     void *_key);
static herr_t H5F_istore_encode_key (H5F_t *f, H5B_t *bt, uint8 *raw,
				     void *_key);
static herr_t H5F_istore_copy_hyperslab (H5F_t *f, H5O_istore_t *istore,
					 H5F_isop_t op, size_t offset_f[],
					 size_t size[], size_t offset_m[],
					 size_t size_m[], void *buf);


/*
 * B-tree key.  A key contains the minimum logical N-dimensional address and
 * the logical size of the chunk to which this key refers.  The
 * fastest-varying dimension is assumed to reference individual bytes of the
 * array, so a 100-element 1-d array of 4-byte integers would really be a 2-d
 * array with the slow varying dimension of size 100 and the fast varying
 * dimension of size 4 (the storage dimensionality has very little to do with
 * the real dimensionality).
 *
 * Only the first few values of the OFFSET and SIZE fields are actually
 * stored on disk, depending on the dimensionality.
 *
 * The storage file address is part of the B-tree and not part of the key.
 */
typedef struct H5F_istore_key_t {
   size_t	offset[H5O_ISTORE_NDIMS];	/*logical offset to start*/
   size_t	size[H5O_ISTORE_NDIMS];		/*logical chunk size	*/
} H5F_istore_key_t;

typedef struct H5F_istore_ud1_t {
   H5F_istore_key_t key;			/*key values		*/
   haddr_t	addr;				/*file address of chunk	*/
   H5O_istore_t	mesg;				/*storage message	*/
} H5F_istore_ud1_t;
   
/* inherits B-tree like properties from H5B */
H5B_class_t H5B_ISTORE[1] = {{
   H5B_ISTORE_ID,				/*id			*/
   sizeof (H5F_istore_key_t),			/*sizeof_nkey		*/
   H5F_istore_sizeof_rkey,			/*get_sizeof_rkey	*/
   H5F_istore_new,				/*new			*/
   H5F_istore_cmp,				/*cmp			*/
   H5F_istore_found,				/*found			*/
   H5F_istore_insert,				/*insert		*/
   FALSE, 					/*follow min branch?	*/
   FALSE, 					/*follow max branch?	*/
   NULL,					/*list			*/
   H5F_istore_decode_key,			/*decode		*/
   H5F_istore_encode_key,			/*encode		*/
}};



/*-------------------------------------------------------------------------
 * Function:	H5F_istore_sizeof_rkey
 *
 * Purpose:	Returns the size of a raw key for the specified UDATA.  The
 *		size of the key is dependent on the number of dimensions for
 *		the object to which this B-tree points.  The dimensionality
 *		of the UDATA is the only portion that's referenced here.
 *
 * Return:	Success:	Size of raw key in bytes.
 *
 *		Failure:	abort()
 *
 * Programmer:	Robb Matzke
 *              Wednesday, October  8, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static size_t
H5F_istore_sizeof_rkey (H5F_t *f, const void *_udata)
{
   const H5F_istore_ud1_t	*udata = (const H5F_istore_ud1_t *)_udata;

   assert (udata);
   assert (udata->mesg.ndims>0 && udata->mesg.ndims<=H5O_ISTORE_NDIMS);

   return udata->mesg.ndims * (4 + 4);
}



/*-------------------------------------------------------------------------
 * Function:	H5F_istore_decode_key
 *
 * Purpose:	Decodes a raw key into a native key for the B-tree
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Friday, October 10, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5F_istore_decode_key (H5F_t *f, H5B_t *bt, uint8 *raw, void *_key)
{
   H5F_istore_key_t	*key = (H5F_istore_key_t *)_key;
   int			i;
   int			ndims = bt->sizeof_rkey / 8;

   FUNC_ENTER (H5F_istore_decode_key, NULL, FAIL);

   /* check args */
   assert (f);
   assert (bt);
   assert (raw);
   assert (key);
   assert (ndims>0 && ndims<=H5O_ISTORE_NDIMS && 8*ndims==bt->sizeof_rkey);

   /* decode */
   for (i=0; i<ndims; i++) {
      UINT32DECODE (raw, key->offset[i]);
      UINT32DECODE (raw, key->size[i]);
   }

   FUNC_LEAVE (SUCCEED);

}


/*-------------------------------------------------------------------------
 * Function:	H5F_istore_encode_key
 *
 * Purpose:	Encode a key from native format to raw format.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Friday, October 10, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5F_istore_encode_key (H5F_t *f, H5B_t *bt, uint8 *raw, void *_key)
{
   H5F_istore_key_t	*key = (H5F_istore_key_t *)_key;
   intn			ndims = bt->sizeof_rkey / 8;
   intn			i;
   
   FUNC_ENTER (H5F_istore_encode_key, NULL, FAIL);

   /* check args */
   assert (f);
   assert (bt);
   assert (raw);
   assert (key);
   assert (ndims>0 && ndims<=H5O_ISTORE_NDIMS && 8*ndims==bt->sizeof_rkey);

   /* encode */
   for (i=0; i<ndims; i++) {
      UINT32ENCODE (raw, key->offset[i]);
      UINT32ENCODE (raw, key->size[i]);
   }

   FUNC_LEAVE (SUCCEED);
}
   

/*-------------------------------------------------------------------------
 * Function:	H5F_istore_cmp
 *
 * Purpose:	Compare the requested datum UDATA with the left and right
 *		keys of the B-tree.
 *
 * Return:	Success:	negative if the min_corner of UDATA is less
 *				than the min_corner of LT_KEY.
 *
 *				positive if the min_corner of UDATA is
 *				greater than or equal the min_corner of
 *				RT_KEY.
 *
 *				zero otherwise.  The min_corner of UDATA is
 *				not necessarily contained within the address
 *				space represented by LT_KEY, but a key that
 *				would describe the UDATA min_corner address
 *				would fall lexicographically between LT_KEY
 *				and RT_KEY.
 *				
 *		Failure:	FAIL (same as UDATA < LT_KEY)
 *
 * Programmer:	Robb Matzke
 *              Wednesday, October  8, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static intn
H5F_istore_cmp (H5F_t *f, void *_lt_key, void *_udata, void *_rt_key)
{
   H5F_istore_key_t	*lt_key = (H5F_istore_key_t *)_lt_key;
   H5F_istore_key_t	*rt_key = (H5F_istore_key_t *)_rt_key;
   H5F_istore_ud1_t	*udata = (H5F_istore_ud1_t *)_udata;

   assert (lt_key);
   assert (rt_key);
   assert (udata);
   assert (udata->mesg.ndims>0 && udata->mesg.ndims<=H5O_ISTORE_NDIMS);

   if (H5V_vector_lt (udata->mesg.ndims, udata->key.offset, lt_key->offset)) {
      return -1;
   } else if (H5V_vector_ge (udata->mesg.ndims, udata->key.offset,
			     rt_key->offset)) {
      return 1;
   } else {
      return 0;
   }
}


/*-------------------------------------------------------------------------
 * Function:	H5F_istore_new
 *
 * Purpose:	Adds a new entry to an i-storage B-tree.  We can assume that
 *		the domain represented by UDATA doesn't intersect the domain
 *		already represented by the B-tree.
 *
 * Return:	Success:	Address of leaf, which is passed in from the
 *				UDATA pointer.
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
static haddr_t
H5F_istore_new (H5F_t *f, void *_lt_key, void *_udata, void *_rt_key)
{
   H5F_istore_key_t	*lt_key = (H5F_istore_key_t *)_lt_key;
   H5F_istore_key_t	*rt_key = (H5F_istore_key_t *)_rt_key;
   H5F_istore_ud1_t	*udata = (H5F_istore_ud1_t *)_udata;
   size_t		nbytes;
   intn			i;

   FUNC_ENTER (H5F_istore_new, NULL, FAIL);

   /* check args */
   assert (f);
   assert (lt_key);
   assert (rt_key);
   assert (udata);
   assert (udata->mesg.ndims>=0 && udata->mesg.ndims<H5O_ISTORE_NDIMS);

   /* Allocate new storage */
   nbytes = H5V_vector_reduce_product (udata->mesg.ndims, udata->key.size);
   assert (nbytes>0);
   if ((udata->addr=H5MF_alloc (f, nbytes))<0) {
      /* Couldn't allocate new file storage */
      HRETURN_ERROR (H5E_IO, H5E_CANTINIT, FAIL);
   }
   
   /* left key describes the UDATA, right key is a zero-size "edge" */
   for (i=0; i<udata->mesg.ndims; i++) {
      lt_key->offset[i] = udata->key.offset[i];
      lt_key->size[i] = udata->key.size[i];
      assert (udata->key.size[i]>0);
      
      rt_key->offset[i] = udata->key.offset[i] + udata->key.size[i];
      rt_key->size[i] = 0;
   }


   FUNC_LEAVE (udata->addr);
}
      

/*-------------------------------------------------------------------------
 * Function:	H5F_istore_found
 *
 * Purpose:	This function is called when the B-tree search engine has
 *		found the leaf entry that points to a chunk of storage that
 *		contains the beginning of the logical address space
 *		represented by UDATA.  The LT_KEY is the left key (the one
 *		that describes the chunk) and RT_KEY is the right key (the
 *		one that describes the next or last chunk).
 *
 * Return:	Success:	SUCCEED with information about the chunk
 *				returned through the UDATA argument.
 *
 *		Failure:	FAIL if not found.
 *
 * Programmer:	Robb Matzke
 *              Thursday, October  9, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5F_istore_found (H5F_t *f, haddr_t addr, const void *_lt_key,
		  void *_udata, const void *_rt_key)
{
   H5F_istore_ud1_t	*udata = (H5F_istore_ud1_t *)_udata;
   const H5F_istore_key_t *lt_key = (const H5F_istore_key_t *)_lt_key;
   const H5F_istore_key_t *rt_key = (const H5F_istore_key_t *)_rt_key;
   int			i;

   FUNC_ENTER (H5F_istore_found, NULL, FAIL);

   /* Check arguments */
   assert (f);
   assert (addr>=0);
   assert (udata);
   assert (lt_key);
   assert (rt_key);

   /* Initialize return values */
   udata->addr = addr;
   for (i=0; i<udata->mesg.ndims; i++) {
      udata->key.offset[i] = lt_key->offset[i];
      udata->key.size[i] = lt_key->size[i];
      assert (lt_key->size[i]>0);
   }

   FUNC_LEAVE (SUCCEED);
}



/*-------------------------------------------------------------------------
 * Function:	H5F_istore_insert
 *
 * Purpose:	This function is called when the B-tree insert engine finds
 *		the node to use to insert new data.  The UDATA argument
 *		points to a struct that describes the logical addresses being
 *		added to the file.  This function allocates space for the
 *		data and returns information through UDATA describing a
 *		file chunk to receive (part of) the data.
 *
 *		The LT_KEY is always the key describing the chunk of file
 *		memory at address ADDR. On entry, UDATA describes the logical
 *		addresses for which storage is being requested (through the
 *		`offset' and `size' fields). On return, UDATA describes the
 *		logical addresses contained in a chunk on disk.
 *
 * Return:	Success:	SUCCEED, with UDATA containing information
 *				about the (newly allocated) chunk.
 *
 *				If the storage address has changed then the
 *				new address is returned.
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Thursday, October  9, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static haddr_t
H5F_istore_insert (H5F_t *f, haddr_t addr, H5B_ins_t *parent_ins,
		   void *_lt_key, hbool_t *lt_key_changed,
		   void *_md_key, void *_udata,
		   void *_rt_key, hbool_t *rt_key_changed)
{
   H5F_istore_key_t	*lt_key = (H5F_istore_key_t *)_lt_key;
   H5F_istore_key_t	*md_key = (H5F_istore_key_t *)_md_key;
   H5F_istore_key_t	*rt_key = (H5F_istore_key_t *)_rt_key;
   H5F_istore_ud1_t	*udata  = (H5F_istore_ud1_t *)_udata;
   intn			i, cmp;
   haddr_t		ret_value = 0;
   size_t		nbytes;

   FUNC_ENTER (H5F_istore_insert, NULL, FAIL);

   /* check args */
   assert (f);
   assert (addr>=0);
   assert (parent_ins);
   assert (lt_key);
   assert (lt_key_changed);
   assert (md_key);
   assert (udata);
   assert (rt_key);
   assert (rt_key_changed);

   cmp = H5F_istore_cmp (f, lt_key, udata, rt_key);
   assert (cmp<=0);

   if (cmp<0) {
      /* Negative indices not supported yet */
      assert ("HDF5 INTERNAL ERROR -- see rpm" && 0);
      HRETURN_ERROR (H5E_STORAGE, H5E_UNSUPPORTED, FAIL);
      
   } else if (H5V_hyper_eq (udata->mesg.ndims,
			    udata->key.offset,  udata->key.size, 
			    lt_key->offset,  lt_key->size)) {
      /*
       * Already exists.  Just return the info.
       */
      udata->addr = addr;
      *parent_ins = H5B_INS_NOOP;
      
   } else if (H5V_hyper_disjointp (udata->mesg.ndims,
				   lt_key->offset, lt_key->size,
				   udata->key.offset, udata->key.size)) {
      assert (H5V_hyper_disjointp (udata->mesg.ndims,
				   rt_key->offset, rt_key->size,
				   udata->key.offset, udata->key.size));

      /*
       * Split this node, inserting the new new node to the right of the
       * current node.  The MD_KEY is where the split occurs.
       */
      for (i=0, nbytes=1; i<udata->mesg.ndims; i++) {
	 assert (0==udata->key.offset[i] % udata->mesg.alignment[i]);
	 assert (udata->key.size[i] == udata->mesg.alignment[i]);
	 md_key->offset[i] = udata->key.offset[i];
	 md_key->size[i] = udata->key.size[i];
	 nbytes *= udata->key.size[i];
      }

      /*
       * Allocate storage for the new chunk
       */
      if ((udata->addr=ret_value=H5MF_alloc (f, nbytes))<=0) {
	 HRETURN_ERROR (H5E_IO, H5E_CANTINIT, FAIL);
      }
      
      *parent_ins = H5B_INS_RIGHT;
      
   } else {
      assert ("HDF5 INTERNAL ERROR -- see rpm" && 0);
      HRETURN_ERROR (H5E_IO, H5E_UNSUPPORTED, FAIL);
   }
   
   FUNC_LEAVE (ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5F_istore_copy_hyperslab
 *
 * Purpose: 	Reads or writes a hyperslab to disk depending on whether OP
 * 		is H5F_ISTORE_READ or H5F_ISTORE_WRITE.  The hyperslab
 * 		storage is described with ISTORE and exists in file F. The
 * 		file hyperslab begins at location OFFSET_F[] (an N-dimensional
 *		point in the domain in terms of elements) in the file and
 *		OFFSET_M[] in memory pointed to by BUF.  Its size is SIZE[]
 *		elements.  The dimensionality of memory is assumed to be the
 *		same as the file and the total size of the multi-dimensional
 *		memory buffer is SIZE_M[].
 *
 *		The slowest varying dimension is always listed first in the
 *		various offset and size arrays.
 *
 *		A `chunk' is a hyperslab of the disk array which is stored
 *		contiguously. I/O occurs in units of chunks where the size of
 *		a chunk is determined by the alignment constraints specified
 *		in ISTORE.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Friday, October 17, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5F_istore_copy_hyperslab (H5F_t *f, H5O_istore_t *istore, H5F_isop_t op,
			   size_t offset_f[], size_t size[],
			   size_t offset_m[], size_t size_m[], void *buf)
{
   intn			i, carry;
   size_t		idx_cur[H5O_ISTORE_NDIMS];
   size_t		idx_min[H5O_ISTORE_NDIMS];
   size_t		idx_max[H5O_ISTORE_NDIMS];
   size_t		sub_size[H5O_ISTORE_NDIMS];
   size_t		sub_offset_f[H5O_ISTORE_NDIMS];
   size_t		sub_offset_m[H5O_ISTORE_NDIMS];
   size_t		sub_offset_ch[H5O_ISTORE_NDIMS];
   size_t		chunk_size;
   uint8		*chunk=NULL;
   H5F_istore_ud1_t	udata;
   herr_t		status;
   herr_t		ret_value = FAIL;
   
   FUNC_ENTER (H5F_istore_copy_hyperslab, NULL, FAIL);

   /* check args */
   assert (f);
   assert (istore);
   assert (istore->ndims>0 && istore->ndims<=H5O_ISTORE_NDIMS);
   assert (H5F_ISTORE_READ==op || H5F_ISTORE_WRITE==op);
   assert (size);
   assert (size_m);
   assert (buf);
#ifndef NDEBUG
   for (i=0; i<istore->ndims; i++) {
      assert (!offset_f || offset_f[i]>=0);/*neg domains unsupported	*/
      assert (!offset_m || offset_m[i]>=0);/*mem array offset never neg	*/
      assert (size[i]>=0);	/*size may be zero, implies no-op	*/
      assert (size_m[i]>0);	/*destination must exist		*/
      /*hyperslab must fit in BUF*/
      assert ((offset_m?offset_m[i]:0)+size[i]<=size_m[i]);
      assert (istore->alignment[i]>0);
   }
#endif

   /*
    * Does the B-tree exist?
    */
   if (istore->btree_addr<=0) {
      if (H5F_ISTORE_WRITE==op) {
	 udata.mesg.ndims = istore->ndims;
	 if ((istore->btree_addr=H5B_new (f, H5B_ISTORE, &udata))<0) {
	    /* Can't create B-tree */
	    HGOTO_ERROR (H5E_IO, H5E_CANTINIT, FAIL);
	 }
      } else {
	 H5V_hyper_fill (istore->ndims, size, size_m, offset_m, buf, 0);
	 HRETURN (SUCCEED);
      }
   }

   /* Initialize indices */
   for (i=0; i<istore->ndims; i++) {
      idx_min[i] = (offset_f?offset_f[i]:0) / istore->alignment[i];
      idx_max[i] = ((offset_f?offset_f[i]:0)+size[i]-1)/istore->alignment[i]+1;
      idx_cur[i] = idx_min[i];
   }

   /* Allocate buffers */
   for (i=0, chunk_size=1; i<istore->ndims; i++) {
      chunk_size *= istore->alignment[i];
   }
   chunk = H5MM_xmalloc (chunk_size);

   /* Initialize non-changing part of udata */
   udata.mesg = *istore;
   
   /* Loop over all chunks */
   while (1) {

      /* Read/Write chunk  or create it if it doesn't exist */
      udata.mesg.ndims = istore->ndims;
      for (i=0; i<istore->ndims; i++) {
	 udata.key.offset[i] = idx_cur[i] * istore->alignment[i];
	 udata.key.size[i] = istore->alignment[i];
	 sub_offset_f[i] = MAX ((offset_f?offset_f[i]:0), udata.key.offset[i]);
	 sub_offset_m[i] = (offset_m?offset_m[i]:0) +
			   sub_offset_f[i] - (offset_f?offset_f[i]:0);
	 sub_size[i] = (idx_cur[i]+1)*istore->alignment[i]-sub_offset_f[i];
	 sub_offset_ch[i] = sub_offset_f[i] - udata.key.offset[i];
      }
      if (H5F_ISTORE_WRITE==op) {
	 status = H5B_insert (f, H5B_ISTORE, istore->btree_addr, &udata);
	 assert (status>=0);
      } else {
	 status = H5B_find (f, H5B_ISTORE, istore->btree_addr, &udata);
      }

      /*
       * If the operation is reading from the disk or if we are writing a
       * partial chunk then load the chunk from disk. 
       */
      if (H5F_ISTORE_READ==op ||
	  !H5V_hyper_eq (istore->ndims,
			 udata.key.offset, udata.key.size,
			 sub_offset_f, sub_size)) {
	 if (status>=0) {
	    if (H5F_block_read (f, udata.addr, chunk_size, chunk)<0) {
	       HGOTO_ERROR (H5E_IO, H5E_READERROR, FAIL);
	    }
	 } else {
	    HDmemset (chunk, 0, chunk_size);
	 }
      }

      /* Transfer data to/from the chunk */
      if (H5F_ISTORE_WRITE==op) {
	 H5V_hyper_copy (istore->ndims, sub_size,
			 udata.key.size, sub_offset_ch, chunk,
			 size_m, sub_offset_m, buf);
	 if (H5F_block_write (f, udata.addr, chunk_size, chunk)<0) {
	    HGOTO_ERROR (H5E_IO, H5E_WRITEERROR, FAIL);
	 }
      } else {
	 H5V_hyper_copy (istore->ndims, sub_size,
			 size_m, sub_offset_m, buf,
			 udata.key.size, sub_offset_ch, chunk);
      }
	 
      /* Increment indices */
      for (i=istore->ndims-1, carry=1; i>=0 && carry; --i) {
	 if (++idx_cur[i]>=idx_max[i]) idx_cur[i] = idx_min[i];
	 else carry = 0;
      }
      if (carry) break;
   }
   ret_value = SUCCEED;
   

 done:
   chunk = H5MM_xfree (chunk);
   FUNC_LEAVE (ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5F_istore_read
 *
 * Purpose:	Reads a multi-dimensional buffer from (part of) an indexed raw
 *		storage array.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Wednesday, October 15, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_istore_read (H5F_t *f, struct H5O_istore_t *istore,
		 size_t offset[], size_t size[], void *buf)
{
   FUNC_ENTER (H5F_istore_read, NULL, FAIL);

   /* Check args */
   assert (f);
   assert (istore);
   assert (istore->ndims>0 && istore->ndims<=H5O_ISTORE_NDIMS);
   assert (size);
   assert (buf);

   if (H5F_istore_copy_hyperslab (f, istore, H5F_ISTORE_READ,
				  offset, size, H5V_ZERO, size, buf)<0) {
      /* hyperslab output failure */
      HRETURN_ERROR (H5E_IO, H5E_READERROR, FAIL);
   }

   FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5F_istore_write
 *
 * Purpose:	Writes a multi-dimensional buffer to (part of) an indexed raw
 *		storage array.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Wednesday, October 15, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_istore_write (H5F_t *f, struct H5O_istore_t *istore,
		  size_t offset[], size_t size[], void *buf)
{
   FUNC_ENTER (H5F_istore_write, NULL, FAIL);

   /* Check args */
   assert (f);
   assert (istore);
   assert (istore->ndims>0 && istore->ndims<=H5O_ISTORE_NDIMS);
   assert (size);
   assert (buf);

   if (H5F_istore_copy_hyperslab (f, istore, H5F_ISTORE_WRITE,
				  offset, size, H5V_ZERO, size, buf)<0) {
      /* hyperslab output failure */
      HRETURN_ERROR (H5E_IO, H5E_WRITEERROR, FAIL);
   }

   FUNC_LEAVE (SUCCEED);
}

