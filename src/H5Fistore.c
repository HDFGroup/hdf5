/*
 * Copyright (C) 1997 NCSA
 *		      All rights reserved.
 *
 * Programmer: Robb Matzke <matzke@llnl.gov>
 *	       Wednesday, October  8, 1997
 */
#include <H5private.h>
#include <H5Dprivate.h>
#include <H5Eprivate.h>
#include <H5Fprivate.h>
#include <H5MFprivate.h>
#include <H5MMprivate.h>
#include <H5Oprivate.h>
#include <H5Vprivate.h>

typedef enum H5F_isop_t {
    H5F_ISTORE_READ,		/*read from file to memory	*/
    H5F_ISTORE_WRITE		/*write from memory to file	*/
} H5F_isop_t;

/* Does the array domain include negative indices? */
#undef H5F_ISTORE_NEGATIVE_DOMAIN

#define PABLO_MASK	H5F_istore_mask

/* Interface initialization */
static hbool_t		interface_initialize_g = FALSE;
#define INTERFACE_INIT NULL

/* PRIVATE PROTOTYPES */
static size_t H5F_istore_sizeof_rkey(H5F_t *f, const void *_udata);
static herr_t H5F_istore_new_node(H5F_t *f, H5B_ins_t, void *_lt_key,
				  void *_udata, void *_rt_key, haddr_t *);
static intn H5F_istore_cmp2(H5F_t *f, void *_lt_key, void *_udata,
			    void *_rt_key);
static intn H5F_istore_cmp3(H5F_t *f, void *_lt_key, void *_udata,
			    void *_rt_key);
static herr_t H5F_istore_found(H5F_t *f, const haddr_t *addr,
			       const void *_lt_key, void *_udata,
			       const void *_rt_key);
static H5B_ins_t H5F_istore_insert(H5F_t *f, const haddr_t *addr,
				   void *_lt_key, hbool_t *lt_key_changed,
				   void *_md_key, void *_udata,
				   void *_rt_key, hbool_t *rt_key_changed,
				   haddr_t *new_node/*out*/);
static herr_t H5F_istore_decode_key(H5F_t *f, H5B_t *bt, uint8 *raw,
				    void *_key);
static herr_t H5F_istore_encode_key(H5F_t *f, H5B_t *bt, uint8 *raw,
				    void *_key);
static herr_t H5F_istore_copy_hyperslab(H5F_t *f, const H5O_layout_t *layout,
					H5F_isop_t op,
					const hssize_t offset_f[],
					const hsize_t size[],
					const hssize_t offset_m[],
					const hsize_t size_m[],
					void *buf);

/*
 * B-tree key.	A key contains the minimum logical N-dimensional address and
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
    uintn	file_number;			/*external file number	*/
    hssize_t	offset[H5O_LAYOUT_NDIMS];	/*logical offset to start*/
    hsize_t	size[H5O_LAYOUT_NDIMS];		/*logical chunk size	*/
} H5F_istore_key_t;

typedef struct H5F_istore_ud1_t {
    H5F_istore_key_t	key;			/*key values		*/
    haddr_t		addr;			/*file address of chunk */
    H5O_layout_t	mesg;			/*layout message	*/
} H5F_istore_ud1_t;

/* inherits B-tree like properties from H5B */
H5B_class_t H5B_ISTORE[1] = {{
    H5B_ISTORE_ID,				/*id			*/
    sizeof(H5F_istore_key_t),			/*sizeof_nkey		*/
    H5F_istore_sizeof_rkey, 			/*get_sizeof_rkey	*/
    H5F_istore_new_node,			/*new			*/
    H5F_istore_cmp2,				/*cmp2			*/
    H5F_istore_cmp3,				/*cmp3			*/
    H5F_istore_found,				/*found			*/
    H5F_istore_insert,				/*insert		*/
    FALSE,					/*follow min branch?	*/
    FALSE,					/*follow max branch?	*/
    NULL,					/*list			*/
    H5F_istore_decode_key,			/*decode		*/
    H5F_istore_encode_key,			/*encode		*/
}};


/*-------------------------------------------------------------------------
 * Function:	H5F_istore_sizeof_rkey
 *
 * Purpose:	Returns the size of a raw key for the specified UDATA.	The
 *		size of the key is dependent on the number of dimensions for
 *		the object to which this B-tree points.	 The dimensionality
 *		of the UDATA is the only portion that's referenced here.
 *
 * Return:	Success:	Size of raw key in bytes.
 *
 *		Failure:	abort()
 *
 * Programmer:	Robb Matzke
 *		Wednesday, October  8, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static size_t
H5F_istore_sizeof_rkey(H5F_t __unused__ *f, const void *_udata)
{
    const H5F_istore_ud1_t *udata = (const H5F_istore_ud1_t *) _udata;
    size_t		    nbytes;

    assert(udata);
    assert(udata->mesg.ndims > 0 && udata->mesg.ndims <= H5O_LAYOUT_NDIMS);

    nbytes = 4 +			/*external file number	*/
	     udata->mesg.ndims * 4 + 	/*dimension indices	*/
	     udata->mesg.ndims * 4;	/*dimension sizes	*/

    return nbytes;
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
 *		Friday, October 10, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5F_istore_decode_key(H5F_t __unused__ *f, H5B_t *bt, uint8 *raw, void *_key)
{
    H5F_istore_key_t	*key = (H5F_istore_key_t *) _key;
    intn		i;
    intn		ndims = (intn)(bt->sizeof_rkey/8);

    FUNC_ENTER(H5F_istore_decode_key, FAIL);

    /* check args */
    assert(f);
    assert(bt);
    assert(raw);
    assert(key);
    assert(ndims > 0 && ndims <= H5O_LAYOUT_NDIMS);

    /* decode */
    UINT32DECODE(raw, key->file_number);
    assert(0 == key->file_number);
    for (i = 0; i < ndims; i++) {
	UINT32DECODE(raw, key->offset[i]);
	UINT32DECODE(raw, key->size[i]);
    }

    FUNC_LEAVE(SUCCEED);
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
 *		Friday, October 10, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5F_istore_encode_key(H5F_t __unused__ *f, H5B_t *bt, uint8 *raw, void *_key)
{
    H5F_istore_key_t	*key = (H5F_istore_key_t *) _key;
    intn		ndims = (intn)(bt->sizeof_rkey / 8);
    intn		i;

    FUNC_ENTER(H5F_istore_encode_key, FAIL);

    /* check args */
    assert(f);
    assert(bt);
    assert(raw);
    assert(key);
    assert(ndims > 0 && ndims <= H5O_LAYOUT_NDIMS);

    /* encode */
    UINT32ENCODE(raw, key->file_number);
    assert(0 == key->file_number);
    for (i = 0; i < ndims; i++) {
	UINT32ENCODE(raw, key->offset[i]);
	UINT32ENCODE(raw, key->size[i]);
    }

    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5F_istore_cmp2
 *
 * Purpose:	Compares two keys sort of like strcmp().  The UDATA pointer
 *		is only to supply extra information not carried in the keys
 *		(in this case, the dimensionality) and is not compared
 *		against the keys.
 *
 * Return:	Success:	-1 if LT_KEY is less than RT_KEY;
 *				1 if LT_KEY is greater than RT_KEY;
 *				0 if LT_KEY and RT_KEY are equal.
 *
 *		Failure:	FAIL (same as LT_KEY<RT_KEY)
 *
 * Programmer:	Robb Matzke
 *		Thursday, November  6, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static intn
H5F_istore_cmp2(H5F_t __unused__ *f, void *_lt_key, void *_udata,
		void *_rt_key)
{
    H5F_istore_key_t	*lt_key = (H5F_istore_key_t *) _lt_key;
    H5F_istore_key_t	*rt_key = (H5F_istore_key_t *) _rt_key;
    H5F_istore_ud1_t	*udata = (H5F_istore_ud1_t *) _udata;
    intn		cmp;

    FUNC_ENTER(H5F_istore_cmp2, FAIL);

    assert(lt_key);
    assert(rt_key);
    assert(udata);
    assert(udata->mesg.ndims > 0 && udata->mesg.ndims <= H5O_LAYOUT_NDIMS);

    /* Compare the offsets but ignore the other fields */
    cmp = H5V_vector_cmp_s(udata->mesg.ndims, lt_key->offset, rt_key->offset);

    FUNC_LEAVE(cmp);
}


/*-------------------------------------------------------------------------
 * Function:	H5F_istore_cmp3
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
 *				zero otherwise.	 The min_corner of UDATA is
 *				not necessarily contained within the address
 *				space represented by LT_KEY, but a key that
 *				would describe the UDATA min_corner address
 *				would fall lexicographically between LT_KEY
 *				and RT_KEY.
 *				
 *		Failure:	FAIL (same as UDATA < LT_KEY)
 *
 * Programmer:	Robb Matzke
 *		Wednesday, October  8, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static intn
H5F_istore_cmp3(H5F_t __unused__ *f, void *_lt_key, void *_udata,
		void *_rt_key)
{
    H5F_istore_key_t	*lt_key = (H5F_istore_key_t *) _lt_key;
    H5F_istore_key_t	*rt_key = (H5F_istore_key_t *) _rt_key;
    H5F_istore_ud1_t	*udata = (H5F_istore_ud1_t *) _udata;
    intn		cmp = 0;

    FUNC_ENTER(H5F_istore_cmp3, FAIL);

    assert(lt_key);
    assert(rt_key);
    assert(udata);
    assert(udata->mesg.ndims > 0 && udata->mesg.ndims <= H5O_LAYOUT_NDIMS);

    if (H5V_vector_lt_s(udata->mesg.ndims, udata->key.offset,
			lt_key->offset)) {
	cmp = -1;
    } else if (H5V_vector_ge_s(udata->mesg.ndims, udata->key.offset,
			     rt_key->offset)) {
	cmp = 1;
    }
    FUNC_LEAVE(cmp);
}


/*-------------------------------------------------------------------------
 * Function:	H5F_istore_new_node
 *
 * Purpose:	Adds a new entry to an i-storage B-tree.  We can assume that
 *		the domain represented by UDATA doesn't intersect the domain
 *		already represented by the B-tree.
 *
 * Return:	Success:	SUCCEED.  The address of leaf is returned
 *				through the ADDR argument.  It is also added
 *				to the UDATA.
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Tuesday, October 14, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5F_istore_new_node(H5F_t *f, H5B_ins_t op,
		    void *_lt_key, void *_udata, void *_rt_key,
		    haddr_t *addr/*out*/)
{
    H5F_istore_key_t	*lt_key = (H5F_istore_key_t *) _lt_key;
    H5F_istore_key_t	*rt_key = (H5F_istore_key_t *) _rt_key;
    H5F_istore_ud1_t	*udata = (H5F_istore_ud1_t *) _udata;
    hsize_t		nbytes;
    intn		i;

    FUNC_ENTER(H5F_istore_new_node, FAIL);

    /* check args */
    assert(f);
    assert(lt_key);
    assert(rt_key);
    assert(udata);
    assert(udata->mesg.ndims > 0 && udata->mesg.ndims < H5O_LAYOUT_NDIMS);
    assert(addr);

    /* Allocate new storage */
    nbytes = H5V_vector_reduce_product(udata->mesg.ndims, udata->key.size);
    assert(nbytes > 0);
    if (H5MF_alloc(f, H5MF_RAW, nbytes, addr /*out */ ) < 0) {
	HRETURN_ERROR(H5E_IO, H5E_CANTINIT, FAIL,
		      "couldn't allocate new file storage");
    }
    udata->addr = *addr;
    udata->key.file_number = 0;
    lt_key->file_number = udata->key.file_number;
    if (H5B_INS_LEFT != op)
	rt_key->file_number = 0;

    /* Initialize the key(s) */
    for (i = 0; i < udata->mesg.ndims; i++) {
	/*
	 * The left key describes the storage of the UDATA chunk being
	 * inserted into the tree.
	 */
	assert(udata->key.size[i] > 0);
	lt_key->offset[i] = udata->key.offset[i];
	lt_key->size[i] = udata->key.size[i];

	/*
	 * The right key might already be present.  If not, then add
	 * a zero-width chunk.
	 */
	if (H5B_INS_LEFT != op) {
	    assert (udata->key.size[i] < MAX_HSSIZET);
	    rt_key->offset[i] = udata->key.offset[i] +
				(hssize_t)udata->key.size[i];
	    rt_key->size[i] = 0;
	}
    }

    FUNC_LEAVE(SUCCEED);
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
 *		Thursday, October  9, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5F_istore_found(H5F_t __unused__ *f, const haddr_t *addr,
		 const void *_lt_key, void *_udata,
		 const void __unused__ *_rt_key)
{
    H5F_istore_ud1_t	   *udata = (H5F_istore_ud1_t *) _udata;
    const H5F_istore_key_t *lt_key = (const H5F_istore_key_t *) _lt_key;
    int			   i;

    FUNC_ENTER(H5F_istore_found, FAIL);

    /* Check arguments */
    assert(f);
    assert(addr && H5F_addr_defined(addr));
    assert(udata);
    assert(lt_key);

    /* Initialize return values */
    udata->addr = *addr;
    udata->key.file_number = lt_key->file_number;
    assert(0 == lt_key->file_number);
    for (i = 0; i < udata->mesg.ndims; i++) {
	udata->key.offset[i] = lt_key->offset[i];
	udata->key.size[i] = lt_key->size[i];
	assert(lt_key->size[i] > 0);
    }

    FUNC_LEAVE(SUCCEED);
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
 * Return:	Success:	An insertion command for the caller, one of
 *				the H5B_INS_* constants.  The address of the
 *				new chunk is returned through the NEW_NODE
 *				argument.
 *
 *		Failure:	H5B_INS_ERROR
 *
 * Programmer:	Robb Matzke
 *		Thursday, October  9, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static H5B_ins_t
H5F_istore_insert(H5F_t *f, const haddr_t *addr, void *_lt_key,
		  hbool_t __unused__ *lt_key_changed,
		  void *_md_key, void *_udata, void *_rt_key,
		  hbool_t __unused__ *rt_key_changed,
		  haddr_t *new_node/*out*/)
{
    H5F_istore_key_t	*lt_key = (H5F_istore_key_t *) _lt_key;
    H5F_istore_key_t	*md_key = (H5F_istore_key_t *) _md_key;
    H5F_istore_key_t	*rt_key = (H5F_istore_key_t *) _rt_key;
    H5F_istore_ud1_t	*udata = (H5F_istore_ud1_t *) _udata;
    intn		i, cmp;
    H5B_ins_t		ret_value = H5B_INS_ERROR;
    hsize_t		nbytes;

    FUNC_ENTER(H5F_istore_insert, H5B_INS_ERROR);

    /* check args */
    assert(f);
    assert(addr && H5F_addr_defined(addr));
    assert(lt_key);
    assert(lt_key_changed);
    assert(md_key);
    assert(udata);
    assert(rt_key);
    assert(rt_key_changed);
    assert(new_node);

    cmp = H5F_istore_cmp3(f, lt_key, udata, rt_key);
    assert(cmp <= 0);

    if (cmp < 0) {
	/* Negative indices not supported yet */
	assert("HDF5 INTERNAL ERROR -- see rpm" && 0);
	HRETURN_ERROR(H5E_STORAGE, H5E_UNSUPPORTED, H5B_INS_ERROR,
		      "internal error");

    } else if (H5V_hyper_eq(udata->mesg.ndims,
			    udata->key.offset, udata->key.size,
			    lt_key->offset, lt_key->size)) {
	/*
	 * Already exists.  Just return the info.
	 */
	udata->addr = *addr;
	udata->key.file_number = lt_key->file_number;
	ret_value = H5B_INS_NOOP;

    } else if (H5V_hyper_disjointp(udata->mesg.ndims,
				   lt_key->offset, lt_key->size,
				   udata->key.offset, udata->key.size)) {
	assert(H5V_hyper_disjointp(udata->mesg.ndims,
				   rt_key->offset, rt_key->size,
				   udata->key.offset, udata->key.size));

	/*
	 * Split this node, inserting the new new node to the right of the
	 * current node.  The MD_KEY is where the split occurs.
	 */
	md_key->file_number = udata->key.file_number;
	for (i=0, nbytes=1; i<udata->mesg.ndims; i++) {
	    assert(0 == udata->key.offset[i] % udata->mesg.dim[i]);
	    assert(udata->key.size[i] == udata->mesg.dim[i]);
	    md_key->offset[i] = udata->key.offset[i];
	    md_key->size[i] = udata->key.size[i];
	    nbytes *= udata->key.size[i];
	}

	/*
	 * Allocate storage for the new chunk
	 */
	if (H5MF_alloc(f, H5MF_RAW, nbytes, new_node /*out */ ) < 0) {
	    HRETURN_ERROR(H5E_IO, H5E_CANTINIT, H5B_INS_ERROR,
			  "file allocation failed");
	}
	udata->addr = *new_node;
	udata->key.file_number = 0;
	ret_value = H5B_INS_RIGHT;

    } else {
	assert("HDF5 INTERNAL ERROR -- see rpm" && 0);
	HRETURN_ERROR(H5E_IO, H5E_UNSUPPORTED, H5B_INS_ERROR,
		      "internal error");
    }

    FUNC_LEAVE(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5F_istore_copy_hyperslab
 *
 * Purpose:	Reads or writes a hyperslab to disk depending on whether OP
 *		is H5F_ISTORE_READ or H5F_ISTORE_WRITE.	 The hyperslab
 *		storage is described with ISTORE and exists in file F. The
 *		file hyperslab begins at location OFFSET_F[] (an N-dimensional
 *		point in the domain in terms of elements) in the file and
 *		OFFSET_M[] in memory pointed to by BUF.	 Its size is SIZE[]
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
 *		Friday, October 17, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5F_istore_copy_hyperslab(H5F_t *f, const H5O_layout_t *layout, H5F_isop_t op,
			  const hssize_t offset_f[], const hsize_t size[],
			  const hssize_t offset_m[], const hsize_t size_m[],
			  void *buf/*in or out*/)
{
    intn		i, carry;
    hsize_t		idx_cur[H5O_LAYOUT_NDIMS];
    hsize_t		idx_min[H5O_LAYOUT_NDIMS];
    hsize_t		idx_max[H5O_LAYOUT_NDIMS];
    hsize_t		sub_size[H5O_LAYOUT_NDIMS];
    hssize_t		offset_wrt_chunk[H5O_LAYOUT_NDIMS];
    hssize_t		sub_offset_m[H5O_LAYOUT_NDIMS];
    size_t		chunk_size;
    hsize_t		acc;
    uint8		*chunk = NULL;
    H5F_istore_ud1_t	udata;
    herr_t		status;
    herr_t		ret_value = FAIL;

    FUNC_ENTER(H5F_istore_copy_hyperslab, FAIL);

    /* check args */
    assert(f);
    assert(layout && H5D_CHUNKED == layout->type);
    assert(H5F_addr_defined(&(layout->addr)));
    assert(layout->ndims > 0 && layout->ndims <= H5O_LAYOUT_NDIMS);
    assert(H5F_ISTORE_READ == op || H5F_ISTORE_WRITE == op);
    assert(size);
    assert(size_m);
    assert(buf);
#ifndef NDEBUG
    for (i=0; i<layout->ndims; i++) {
	assert(size_m[i] > 0);	/*destination must exist		*/
	/*hyperslab must fit in BUF */
	assert((offset_m?offset_m[i]:0) + size[i] <= size_m[i]);
	assert(layout->dim[i] > 0);
    }
#endif

    /*
     * As a special case of H5F_ISTORE_READ, if the source is aligned on
     * a chunk boundary and is the same size as a chunk, and the destination
     * is the same size as a chunk, then instead of reading into a temporary
     * buffer and then into the destination, we read directly into the
     * destination.
     */
    if (H5F_ISTORE_READ==op) {
	for (i=0, acc=1; i<layout->ndims; i++) {
	    if (offset_f[i] % layout->dim[i]) break; /*src not aligned*/
	    if (size[i]!=layout->dim[i]) break; /*src not a chunk*/
	    if (size_m[i]!=layout->dim[i]) break; /*dst not a chunk*/
	    udata.key.offset[i] = offset_f[i];
	    udata.key.size[i] = layout->dim[i];
	    acc *= layout->dim[i];
	}
	chunk_size = acc;
	assert ((hsize_t)chunk_size==acc);

	if (i==layout->ndims) {
	    udata.mesg = *layout;
	    H5F_addr_undef (&(udata.addr));
	    udata.key.file_number = 0;
	    status = H5B_find (f, H5B_ISTORE, &(layout->addr), &udata);
	    if (status>=0 && H5F_addr_defined (&(udata.addr))) {
		assert (0==udata.key.file_number);
		if (H5F_block_read (f, &(udata.addr), (hsize_t)chunk_size,
				    buf)<0) {
		    HGOTO_ERROR (H5E_IO, H5E_READERROR, FAIL,
				 "unable to read raw storage chunk");
		}
	    } else {
		HDmemset (buf, 0, chunk_size);
	    }
	    HRETURN (SUCCEED);
	}
    }

    /*
     * This is the general case.  We set up multi-dimensional counters
     * (idx_min, idx_max, and idx_cur) and loop through the chunks copying
     * each chunk into a temporary buffer, compressing or decompressing, and
     * then copying it to it's destination.
     */
    for (i=0; i<layout->ndims; i++) {
	idx_min[i] = (offset_f?offset_f[i]:0) / layout->dim[i];
	idx_max[i] = ((offset_f?offset_f[i]:0) + size[i]-1) / layout->dim[i]+1;
	idx_cur[i] = idx_min[i];
    }

    /* Allocate buffers */
    for (i=0, chunk_size=1; i<layout->ndims; i++) {
	chunk_size *= layout->dim[i];
    }
    chunk = H5MM_xmalloc(chunk_size);

    /* Initialize non-changing part of udata */
    udata.mesg = *layout;

    /* Loop over all chunks */
    while (1) {

	/* Read/Write chunk  or create it if it doesn't exist */
	udata.mesg.ndims = layout->ndims;
	H5F_addr_undef(&(udata.addr));
	udata.key.file_number = 0;

	for (i=0; i<layout->ndims; i++) {

	    /* The location and size of the chunk being accessed */
	    assert (layout->dim[i] < MAX_HSSIZET);
	    udata.key.offset[i] = idx_cur[i] * (hssize_t)(layout->dim[i]);
	    udata.key.size[i] = layout->dim[i];

	    /* The offset and size wrt the chunk */
	    offset_wrt_chunk[i] = MAX((offset_f?offset_f[i]:0),
				      udata.key.offset[i]) -
				  udata.key.offset[i];
	    sub_size[i] = MIN((idx_cur[i] + 1) * layout->dim[i],
			      (offset_f ? offset_f[i] : 0) + size[i]) -
			  (udata.key.offset[i] + offset_wrt_chunk[i]);
	    
	    /* Offset into mem buffer */
	    sub_offset_m[i] = udata.key.offset[i] + offset_wrt_chunk[i] +
			      (offset_m ? offset_m[i] : 0) -
			      (offset_f ? offset_f[i] : 0);
	}

	if (H5F_ISTORE_WRITE == op) {
	    status = H5B_insert(f, H5B_ISTORE, &(layout->addr), &udata);
	    assert(status >= 0);
	} else {
	    status = H5B_find(f, H5B_ISTORE, &(layout->addr), &udata);
	}

	/*
	 * If the operation is reading from the disk or if we are writing a
	 * partial chunk then load the chunk from disk. 
	 */
	if (H5F_ISTORE_READ == op ||
	    !H5V_vector_zerop_s(layout->ndims, offset_wrt_chunk) ||
	    !H5V_vector_eq_u(layout->ndims, sub_size, udata.key.size)) {
	    if (status>=0 && H5F_addr_defined(&(udata.addr))) {
		assert(0==udata.key.file_number);
		if (H5F_block_read(f, &(udata.addr), (hsize_t)chunk_size,
				   chunk) < 0) {
		    HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL,
				"unable to read raw storage chunk");
		}
	    } else {
		HDmemset(chunk, 0, chunk_size);
	    }
	}
	/* Transfer data to/from the chunk */
	if (H5F_ISTORE_WRITE==op) {
	    H5V_hyper_copy(layout->ndims, sub_size,
			   udata.key.size, offset_wrt_chunk, chunk,
			   size_m, sub_offset_m, buf);
	    assert(0 == udata.key.file_number);
	    if (H5F_block_write(f, &(udata.addr), (hsize_t)chunk_size,
				chunk) < 0) {
		HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL,
			    "unable to write raw storage chunk");
	    }
	} else {
	    H5V_hyper_copy(layout->ndims, sub_size,
			   size_m, sub_offset_m, (void *)buf,
			   udata.key.size, offset_wrt_chunk, chunk);
	}

	/* Increment indices */
	for (i=layout->ndims-1, carry=1; i>=0 && carry; --i) {
	    if (++idx_cur[i]>=idx_max[i]) idx_cur[i] = idx_min[i];
	    else carry = 0;
	}
	if (carry) break;
    }
    ret_value = SUCCEED;

  done:
    chunk = H5MM_xfree(chunk);
    FUNC_LEAVE(ret_value);
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
 *		Wednesday, October 15, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_istore_read(H5F_t *f, const H5O_layout_t *layout,
		const hssize_t offset[], const hsize_t size[], void *buf)
{
    FUNC_ENTER(H5F_istore_read, FAIL);

    /* Check args */
    assert(f);
    assert(layout && H5D_CHUNKED == layout->type);
    assert(layout->ndims > 0 && layout->ndims <= H5O_LAYOUT_NDIMS);
    assert(size);
    assert(buf);

    if (H5F_istore_copy_hyperslab(f, layout, H5F_ISTORE_READ,
				  offset, size, H5V_ZERO, size, buf) < 0) {
	HRETURN_ERROR(H5E_IO, H5E_READERROR, FAIL,
		      "hyperslab output failure");
    }
    FUNC_LEAVE(SUCCEED);
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
 *		Wednesday, October 15, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_istore_write(H5F_t *f, const H5O_layout_t *layout,
		 const hssize_t offset[], const hsize_t size[],
		 const void *buf)
{
    FUNC_ENTER(H5F_istore_write, FAIL);

    /* Check args */
    assert(f);
    assert(layout && H5D_CHUNKED == layout->type);
    assert(layout->ndims > 0 && layout->ndims <= H5O_LAYOUT_NDIMS);
    assert(size);
    assert(buf);

    if (H5F_istore_copy_hyperslab(f, layout, H5F_ISTORE_WRITE,
				  offset, size, H5V_ZERO, size,
				  (void*)buf) < 0) {
	HRETURN_ERROR(H5E_IO, H5E_WRITEERROR, FAIL,
		      "hyperslab output failure");
    }
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5F_istore_create
 *
 * Purpose:	Creates a new indexed-storage B-tree and initializes the
 *		istore struct with information about the storage.  The
 *		struct should be immediately written to the object header.
 *
 *		This function must be called before passing ISTORE to any of
 *		the other indexed storage functions!
 *
 * Return:	Success:	SUCCEED with the ISTORE argument initialized
 *				and ready to write to an object header.
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Tuesday, October 21, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_istore_create(H5F_t *f, H5O_layout_t *layout /*out */ )
{
    H5F_istore_ud1_t	udata;
#ifndef NDEBUG
    int			i;
#endif

    FUNC_ENTER(H5F_istore_create, FAIL);

    /* Check args */
    assert(f);
    assert(layout && H5D_CHUNKED == layout->type);
    assert(layout->ndims > 0 && layout->ndims <= H5O_LAYOUT_NDIMS);
#ifndef NDEBUG
    for (i = 0; i < layout->ndims; i++) {
	assert(layout->dim[i] > 0);
    }
#endif

    udata.mesg.ndims = layout->ndims;
    if (H5B_create(f, H5B_ISTORE, &udata, &(layout->addr) /*out */ ) < 0) {
	HRETURN_ERROR(H5E_IO, H5E_CANTINIT, FAIL, "can't create B-tree");
    }
    
    FUNC_LEAVE(SUCCEED);
}
