/*
 * Copyright (C) 1997 NCSA
 *		      All rights reserved.
 *
 * Programmer: 	Robb Matzke <matzke@llnl.gov>
 *	       	Wednesday, October  8, 1997
 *
 * Purpose:	Indexed (chunked) I/O functions.  The logical
 *		multi-dimensional data space is regularly partitioned into
 *		same-sized "chunks", the first of which is aligned with the
 *		logical origin.  The chunks are given a multi-dimensional
 *		index which is used as a lookup key in a B-tree that maps
 *		chunk index to disk address.  Each chunk can be compressed
 *		independently and the chunks may move around in the file as
 *		their storage requirements change.
 */
#include <H5private.h>
#include <H5Dprivate.h>
#include <H5Eprivate.h>
#include <H5Fprivate.h>
#include <H5MFprivate.h>
#include <H5MMprivate.h>
#include <H5Oprivate.h>
#include <H5Vprivate.h>

/* Interface initialization */
#define PABLO_MASK	H5F_istore_mask
static hbool_t		interface_initialize_g = FALSE;
#define INTERFACE_INIT NULL

/* Raw data chunks are cached.  Each entry in the cache is: */
typedef struct H5F_rdcc_ent_t {
    hbool_t	locked;		/*entry is locked in cache		*/
    hbool_t	dirty;		/*needs to be written to disk?		*/
    H5O_layout_t *layout;	/*the layout message			*/
    H5O_compress_t *comp;	/*compression message			*/
    hssize_t	offset[H5O_LAYOUT_NDIMS]; /*chunk name			*/
    size_t	chunk_size;	/*size of a chunk			*/
    size_t	rd_count;	/*bytes remaining to be read		*/
    size_t	wr_count;	/*bytes remaining to be written		*/
    uint8	*chunk;		/*the uncompressed chunk data		*/
} H5F_rdcc_ent_t;

/* Private prototypes */
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
static herr_t H5F_istore_debug_key (FILE *stream, intn indent, intn fwidth,
				    const void *key, const void *udata);
#ifdef HAVE_PARALLEL
static herr_t H5F_istore_get_addr (H5F_t *f, const H5O_layout_t *layout,
				const hssize_t offset[], void *_udata/*out*/);
#endif

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
 * The chunk's file address is part of the B-tree and not part of the key.
 */
typedef struct H5F_istore_key_t {
    hsize_t	nbytes;				/*size of stored data	*/
    hssize_t	offset[H5O_LAYOUT_NDIMS];	/*logical offset to start*/
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
    H5F_istore_debug_key,			/*debug			*/
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

    nbytes = 4 +			/*storage size		*/
	     udata->mesg.ndims * 4; 	/*dimension indices	*/

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
    intn		ndims = (intn)((bt->sizeof_rkey-4)/4);

    FUNC_ENTER(H5F_istore_decode_key, FAIL);

    /* check args */
    assert(f);
    assert(bt);
    assert(raw);
    assert(key);
    assert(ndims > 0 && ndims <= H5O_LAYOUT_NDIMS);

    /* decode */
    UINT32DECODE (raw, key->nbytes);
    for (i = 0; i < ndims; i++) {
	UINT32DECODE(raw, key->offset[i]);
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
    intn		ndims = (intn)((bt->sizeof_rkey-4) / 4);
    intn		i;

    FUNC_ENTER(H5F_istore_encode_key, FAIL);

    /* check args */
    assert(f);
    assert(bt);
    assert(raw);
    assert(key);
    assert(ndims > 0 && ndims <= H5O_LAYOUT_NDIMS);

    /* encode */
    UINT32ENCODE (raw, key->nbytes);
    for (i = 0; i < ndims; i++) {
	UINT32ENCODE(raw, key->offset[i]);
    }

    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5F_istore_debug_key
 *
 * Purpose:	Prints a key.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Thursday, April 16, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5F_istore_debug_key (FILE *stream, intn indent, intn fwidth,
		      const void *_key, const void *_udata)
{
    const H5F_istore_key_t	*key = (const H5F_istore_key_t *)_key;
    const H5F_istore_ud1_t	*udata = (const H5F_istore_ud1_t *)_udata;
    int				i;
    
    FUNC_ENTER (H5F_istore_debug_key, FAIL);
    assert (key);

    HDfprintf (stream, "%*s%-*s %Hd bytes\n", indent, "", fwidth,
	       "Chunk size:", key->nbytes);
    HDfprintf (stream, "%*s%-*s {", indent, "", fwidth,
	       "Logical offset:");
    for (i=0; i<udata->mesg.ndims; i++) {
	HDfprintf (stream, "%s%Hd", i?", ":"", key->offset[i]);
    }
    fputs ("}\n", stream);

    FUNC_LEAVE (SUCCEED);
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
    intn		i;

    FUNC_ENTER(H5F_istore_new_node, FAIL);
#ifdef AKC
    printf("%s: Called\n", FUNC);
#endif
    /* check args */
    assert(f);
    assert(lt_key);
    assert(rt_key);
    assert(udata);
    assert(udata->mesg.ndims > 0 && udata->mesg.ndims < H5O_LAYOUT_NDIMS);
    assert(addr);

    /* Allocate new storage */
    assert (udata->key.nbytes > 0);
#ifdef AKC
    printf("calling H5MF_alloc for new chunk\n");
#endif
    if (H5MF_alloc(f, H5MF_RAW, udata->key.nbytes, addr /*out */ ) < 0) {
	HRETURN_ERROR(H5E_IO, H5E_CANTINIT, FAIL,
		      "couldn't allocate new file storage");
    }
    udata->addr = *addr;

    /*
     * The left key describes the storage of the UDATA chunk being
     * inserted into the tree.
     */
    lt_key->nbytes = udata->key.nbytes;
    for (i=0; i<udata->mesg.ndims; i++) {
	lt_key->offset[i] = udata->key.offset[i];
    }

    /*
     * The right key might already be present.  If not, then add a zero-width
     * chunk.
     */
    if (H5B_INS_LEFT != op) {
	rt_key->nbytes = 0;
	for (i=0; i<udata->mesg.ndims; i++) {
	    assert (udata->mesg.dim[i] < MAX_HSSIZET);
	    assert (udata->key.offset[i]+(hssize_t)(udata->mesg.dim[i]) >
		    udata->key.offset[i]);
	    rt_key->offset[i] = udata->key.offset[i] +
				(hssize_t)(udata->mesg.dim[i]);
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
    udata->key.nbytes = lt_key->nbytes;
    assert (lt_key->nbytes>0);
    for (i = 0; i < udata->mesg.ndims; i++) {
	udata->key.offset[i] = lt_key->offset[i];
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

    FUNC_ENTER(H5F_istore_insert, H5B_INS_ERROR);
#ifdef AKC
    printf("%s: Called\n", FUNC);
#endif

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
	
    } else if (H5V_vector_eq_s (udata->mesg.ndims,
				udata->key.offset, lt_key->offset) &&
	       lt_key->nbytes>0) {
	/*
	 * Already exists.  If the new size is not the same as the old size
	 * then we should reallocate storage.
	 */
	if (lt_key->nbytes != udata->key.nbytes) {
#ifdef AKC
	    printf("calling H5MF_realloc for new chunk\n");
#endif
	    if (H5MF_realloc (f, H5MF_RAW, lt_key->nbytes, addr,
			      udata->key.nbytes, new_node/*out*/)<0) {
		HRETURN_ERROR (H5E_STORAGE, H5E_WRITEERROR, H5B_INS_ERROR,
			       "unable to reallocate chunk storage");
	    }
	    lt_key->nbytes = udata->key.nbytes;
	    *lt_key_changed = TRUE;
	    udata->addr = *new_node;
	    ret_value = H5B_INS_CHANGE;
	} else {
	    udata->addr = *addr;
	    ret_value = H5B_INS_NOOP;
	}

    } else if (H5V_hyper_disjointp(udata->mesg.ndims,
				   lt_key->offset, udata->mesg.dim,
				   udata->key.offset, udata->mesg.dim)) {
	assert(H5V_hyper_disjointp(udata->mesg.ndims,
				   rt_key->offset, udata->mesg.dim,
				   udata->key.offset, udata->mesg.dim));
	/*
	 * Split this node, inserting the new new node to the right of the
	 * current node.  The MD_KEY is where the split occurs.
	 */
	md_key->nbytes = udata->key.nbytes;
	for (i=0; i<udata->mesg.ndims; i++) {
	    assert(0 == udata->key.offset[i] % udata->mesg.dim[i]);
	    md_key->offset[i] = udata->key.offset[i];
	}

	/*
	 * Allocate storage for the new chunk
	 */
#ifdef AKC
	printf("calling H5MF_alloc for new chunk\n");
#endif
	if (H5MF_alloc(f, H5MF_RAW, udata->key.nbytes, new_node/*out*/)<0) {
	    HRETURN_ERROR(H5E_IO, H5E_CANTINIT, H5B_INS_ERROR,
			  "file allocation failed");
	}
	udata->addr = *new_node;
	ret_value = H5B_INS_RIGHT;

    } else {
	assert("HDF5 INTERNAL ERROR -- see rpm" && 0);
	HRETURN_ERROR(H5E_IO, H5E_UNSUPPORTED, H5B_INS_ERROR,
		      "internal error");
    }

    FUNC_LEAVE(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5F_istore_init
 *
 * Purpose:	Initialize the raw data chunk cache for a file.  This is
 *		called when the file handle is initialized.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Monday, May 18, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_istore_init (H5F_t *f)
{
    H5F_rdcc_t	*rdcc = &(f->shared->rdcc);
    
    FUNC_ENTER (H5F_istore_init, FAIL);

    HDmemset (rdcc, 0, sizeof(H5F_rdcc_t));
    if (f->shared->access_parms->rdcc_nbytes>0) {
	rdcc->nslots = 25; /*some initial number of slots*/
	rdcc->slot = H5MM_calloc (rdcc->nslots*sizeof(H5F_rdcc_ent_t));
	if (NULL==rdcc->slot) {
	    HRETURN_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL,
			   "memory allocation failed");
	}
    }

    FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5F_istore_flush_entry
 *
 * Purpose:	Writes a chunk to disk.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Thursday, May 21, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5F_istore_flush_entry (H5F_t *f, H5F_rdcc_ent_t *ent)
{
    void	*c_buf = NULL;		/*temp compression buffer	*/
    void	*out_ptr = NULL;	/*ptr to output buffer		*/
    size_t	nbytes;			/*size of output buffer		*/
    herr_t	ret_value = FAIL;	/*return value			*/
    H5F_istore_ud1_t udata;		/*pass through B-tree		*/
    intn	i;
    
    FUNC_ENTER (H5F_istore_flush_entry, FAIL);
    assert (ent);
    assert (!ent->locked);
    if (!ent->dirty) HRETURN (SUCCEED);

    /* Should the chunk be compressed before writing it to disk? */
    if (ent->comp && H5Z_NONE!=ent->comp->method) {
	if (NULL==(c_buf = H5MM_malloc (ent->chunk_size))) {
	    HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL,
			 "memory allocation failed for data compression");
	}
	nbytes = H5Z_compress (ent->comp, ent->chunk_size, ent->chunk, c_buf);
	if (nbytes && nbytes<ent->chunk_size) {
	    out_ptr = c_buf;
	} else {
	    out_ptr = ent->chunk;
	    nbytes = ent->chunk_size;
	}
    } else {
	out_ptr = ent->chunk;
	nbytes = ent->chunk_size;
    }
    
    /*
     * Create the chunk it if it doesn't exist, or reallocate the chunk if its
     * size changed.  Then write the data into the file.
     */
    udata.mesg = *(ent->layout);
    H5F_addr_undef(&(udata.addr));
    udata.key.nbytes = nbytes;
    for (i=0; i<ent->layout->ndims; i++) {
	udata.key.offset[i] = ent->offset[i];
    }
    
    if (H5B_insert(f, H5B_ISTORE, &(ent->layout->addr), &udata)<0) {
	HGOTO_ERROR (H5E_IO, H5E_WRITEERROR, FAIL,
		     "unable to allocate chunk");
    }
    if (H5F_block_write (f, &(udata.addr), nbytes, H5D_XFER_DFLT, out_ptr)<0) {
	HGOTO_ERROR (H5E_IO, H5E_WRITEERROR, FAIL,
		     "unable to write raw data to file");
    }

    /* Mark cache entry as clean */
    ent->dirty = FALSE;
    f->shared->rdcc.nflushes++;
    ret_value = SUCCEED;

 done:
    H5MM_xfree (c_buf);
    FUNC_LEAVE (ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5F_istore_flush
 *
 * Purpose:	Writes all dirty chunks to disk but does not remove them from
 *		the cache.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Thursday, May 21, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_istore_flush (H5F_t *f)
{
    H5F_rdcc_t	*rdcc = &(f->shared->rdcc);
    intn	i, nerrors=0;
    
    FUNC_ENTER (H5F_istore_flush, FAIL);

    for (i=0; i<rdcc->nused; i++) {
	if (H5F_istore_flush_entry (f, rdcc->slot+i)<0) {
	    nerrors++;
	}
    }
    if (nerrors) {
	HRETURN_ERROR (H5E_IO, H5E_CANTFLUSH, FAIL,
		       "unable to flush one or more raw data chunks");
    }
    FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5F_istore_preempt
 *
 * Purpose:	Preempts the specified entry from the cache, flushing it to
 *		disk if necessary.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Thursday, May 21, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5F_istore_preempt (H5F_t *f, intn idx)
{
    H5F_rdcc_t		*rdcc = &(f->shared->rdcc);
    H5F_rdcc_ent_t	*ent = rdcc->slot + idx;
    
    FUNC_ENTER (H5F_istore_preempt, FAIL);
    assert (idx>=0 && idx<rdcc->nused);
    assert (!ent->locked);

    if (ent->dirty) H5F_istore_flush_entry (f, ent);
    H5O_free (H5O_LAYOUT, ent->layout);
    H5O_free (H5O_COMPRESS, ent->comp);
    H5MM_xfree (ent->chunk);
    rdcc->nused -= 1;
    rdcc->nbytes -= ent->chunk_size;
    HDmemmove (rdcc->slot+idx, rdcc->slot+idx+1,
	       (rdcc->nused-idx) * sizeof(H5F_rdcc_ent_t));
    
    FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5F_istore_dest
 *
 * Purpose:	Destroy the entire chunk cache by flushing dirty entries,
 *		preempting all entries, and freeing the cache itself.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Thursday, May 21, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_istore_dest (H5F_t *f)
{
    H5F_rdcc_t	*rdcc = &(f->shared->rdcc);
    intn	i, nerrors=0;
    
    FUNC_ENTER (H5F_istore_dest, FAIL);

    for (i=rdcc->nused-1; i>=0; --i) {
	if (H5F_istore_flush_entry (f, rdcc->slot+i)<0) {
	    nerrors++;
	}
	if (H5F_istore_preempt (f, i)<0) {
	    nerrors++;
	}
    }
    if (nerrors) {
	HRETURN_ERROR (H5E_IO, H5E_CANTFLUSH, FAIL,
		       "unable to flush one or more raw data chunks");
    }

    H5MM_xfree (rdcc->slot);
    HDmemset (rdcc, 0, sizeof(H5F_rdcc_t));
    FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5F_istore_prune
 *
 * Purpose:	Prune the cache by preempting some things until the cache has
 *		room for something which is SIZE bytes.  Only unlocked
 *		entries are considered for preemption.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Thursday, May 21, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5F_istore_prune (H5F_t *f, size_t size)
{
    intn		i, meth0, meth1, nerrors=0;
    H5F_rdcc_t		*rdcc = &(f->shared->rdcc);
    H5F_rdcc_ent_t	*ent0, *ent1;
    double		w0 = f->shared->access_parms->rdcc_w0;
    size_t		total = f->shared->access_parms->rdcc_nbytes;

    FUNC_ENTER (H5F_istore_prune, FAIL);

    /*
     * We have two pointers that slide down the cache beginning at the least
     * recently used entry.  The distance between the pointers represents the
     * relative weight.  A weight of 50% for the first pointer means that the
     * second pointer is half the cache length behind the first pointer.
     */
    meth0 = rdcc->nused;
    meth1 = rdcc->nused * (1.0+w0);
    for (i=MAX(meth0, meth1)-1;
	 rdcc->nbytes+size>total && i>=0;
	 --i, --meth0, --meth1) {

	ent0 = rdcc->slot+meth0; /*might be a bad pointer!*/
	ent1 = rdcc->slot+meth1; /*might be a bad pointer!*/
	
	if (meth0>=0 && meth0<rdcc->nused && !ent0->locked &&
	    (0==ent0->rd_count || ent0->chunk_size==ent0->rd_count) &&
	    (0==ent0->wr_count || ent0->chunk_size==ent0->wr_count)) {
	    /*
	     * Method 0: Preempt entries that have a zero rd_count.  If the
	     * application is accessing a dataset with a set of
	     * non-overlapping partial I/O requests then chunks with a zero
	     * rd_count will probably not be accessed in the near future.
	     */
	    if (H5F_istore_preempt (f, meth0)<0) nerrors++;
	    
	} else if (meth1>=0 && meth1<rdcc->nused && !ent1->locked) {
	    /*
	     * Method 1: Discard the least recently used members from the
	     * cache.  This is a catch-all.
	     */
	    if (H5F_istore_preempt (f, meth1)<0) nerrors++;
	}
    }
    if (nerrors) {
	HRETURN_ERROR (H5E_IO, H5E_CANTFLUSH, FAIL,
		       "unable to preempt one or more raw data cache entry");
    }

    FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5F_istore_lock
 *
 * Purpose:	Return a pointer to an uncompressed chunk.  The pointer
 *		points directly into the chunk cache and should not be freed
 *		by the caller but will be valid until it is unlocked.  The
 *		input value IDX_HINT is used to speed up cache lookups and
 *		it's output value should be given to H5F_rdcc_unlock().
 *
 *		If RELAX is non-zero and the chunk isn't in the cache then
 *		don't try to read it from the file, but just allocate an
 *		uninitialized buffer to hold the result.  This is indented
 *		for output functions that are about to overwrite the entire
 *		chunk.
 *
 * Return:	Success:	Ptr to an uncompressed chunk.
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *              Thursday, May 21, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void *
H5F_istore_lock (H5F_t *f, const H5O_layout_t *layout,
		 const H5O_compress_t *comp, const hssize_t offset[],
		 hbool_t relax, intn *idx_hint/*in,out*/)
{
    H5F_rdcc_t		*rdcc = &(f->shared->rdcc);
    H5F_rdcc_ent_t	*ent = NULL;
    intn		i, j, found = -1;
    H5F_istore_ud1_t	udata;			/*B-tree pass-through	*/
    size_t		chunk_size=0;		/*size of a chunk	*/
    herr_t		status;			/*func return status	*/
    void		*chunk=NULL;		/*the uncompressed chunk*/
    void		*temp=NULL;		/*temporary chunk buffer*/
    void		*ret_value=NULL;	/*return value		*/
    
    FUNC_ENTER (H5F_istore_lock, NULL);

    /* First use the hint */
    if (idx_hint && *idx_hint>=0 && *idx_hint<rdcc->nused) {
	ent = rdcc->slot + *idx_hint;
	if (layout->ndims==ent->layout->ndims &&
	    H5F_addr_eq(&(layout->addr), &(ent->layout->addr))) {
	    for (i=0, found=*idx_hint; found>=0 && i<ent->layout->ndims; i++) {
		if (offset[i]!=ent->offset[i]) found = -1;
	    }
	}
    }

    /* Then look at all the entries */
    for (i=0; found<0 && i<rdcc->nused; i++) {
	ent = rdcc->slot + i;
	if (layout->ndims==ent->layout->ndims &&
	    H5F_addr_eq(&(layout->addr), &(ent->layout->addr))) {
	    for (j=0, found=i; found>=0 && j<ent->layout->ndims; j++) {
		if (offset[j]!=ent->offset[j]) found = -1;
	    }
	}
    }


    if (found>=0) {
	/*
	 * Already in the cache.  Count a hit.
	 */
	rdcc->nhits++;
	
    } else if (found<0 && relax) {
	/*
	 * Not in the cache, but we're about to overwrite the whole thing
	 * anyway, so just allocate a buffer for it but don't initialize that
	 * buffer with the file contents. Count this as a hit instead of a
	 * miss because we saved ourselves lots of work.
	 */
	rdcc->nhits++;
	for (i=0, chunk_size=1; i<layout->ndims; i++) {
	    chunk_size *= layout->dim[i];
	}
	if (NULL==(chunk=H5MM_malloc (chunk_size))) {
	    HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL,
			 "memory allocation failed for raw data chunk");
	}
	
    } else {
	/*
	 * Not in the cache.  Read it from the file and count this as a miss
	 * if it's in the file or an init if it isn't.
	 */
	for (i=0, chunk_size=1; i<layout->ndims; i++) {
	    udata.key.offset[i] = offset[i];
	    chunk_size *= layout->dim[i];
	}
	udata.mesg = *layout;
	H5F_addr_undef (&(udata.addr));
	status = H5B_find (f, H5B_ISTORE, &(layout->addr), &udata);
	H5E_clear ();
	if (NULL==(chunk = H5MM_malloc (chunk_size))) {
	    HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL,
			 "memory allocation failed for raw data chunk");
	}
	if (status>=0 && H5F_addr_defined (&(udata.addr))) {
	    /*
	     * The chunk exists on disk but might be compressed.  Instead of
	     * allocating the exact size for the compressed chunk we allocate
	     * the entire chunk size -- it reduces strain on the malloc()
	     * subsystem.
	     */
	    if (H5F_block_read (f, &(udata.addr), udata.key.nbytes,
	        H5D_XFER_DFLT, chunk)<0) {
		HGOTO_ERROR (H5E_IO, H5E_READERROR, NULL,
			     "unable to read raw data chunk");
	    }
	    if (udata.key.nbytes<chunk_size) {
		if (NULL==(temp = H5MM_malloc (chunk_size))) {
		    HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL,
				 "memory allocation failed for uncompress");
		}
		if (chunk_size!=H5Z_uncompress (comp, udata.key.nbytes,
						chunk, chunk_size, temp)) {
		    HGOTO_ERROR (H5E_IO, H5E_READERROR, NULL,
				 "unable to uncompress raw data chunk"); 
		}
		H5MM_xfree (chunk);
		chunk = temp;
		temp = NULL;
	    }
	    rdcc->nmisses++;
	} else {
	    /*
	     * The chunk doesn't exist in the file.  Assume all zeros.
	     */
	    HDmemset (chunk, 0, chunk_size);
	    rdcc->ninits++;
	}
    }

    assert (found>=0 || chunk_size>0);
    if (found<0 && chunk_size<=f->shared->access_parms->rdcc_nbytes) {
	/*
	 * Add the chunk to the beginning of the cache after pruning the cache
	 * to make room.
	 */
	if (H5F_istore_prune (f, chunk_size)<0) {
	    H5E_clear ();
	}
	if (rdcc->nused>=rdcc->nslots) {
	    size_t na = MAX (25, 2*rdcc->nslots);
	    H5F_rdcc_ent_t *x = H5MM_realloc (rdcc->slot,
					      na*sizeof(H5F_rdcc_ent_t));
	    if (NULL==x) {
		HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL,
			     "memory allocation failed");
	    }
	    rdcc->nslots = (intn)na;
	    rdcc->slot = x;
	}
	HDmemmove (rdcc->slot+1, rdcc->slot,
		   rdcc->nused*sizeof(H5F_rdcc_ent_t));
	rdcc->nused++;
	rdcc->nbytes += chunk_size;
	ent = rdcc->slot;
	ent->locked = 0;
	ent->dirty = FALSE;
	ent->chunk_size = chunk_size;
	ent->layout = H5O_copy (H5O_LAYOUT, layout);
	ent->comp = H5O_copy (H5O_COMPRESS, comp);
	for (i=0; i<layout->ndims; i++) {
	    ent->offset[i] = offset[i];
	}
	ent->rd_count = chunk_size;
	ent->wr_count = chunk_size;
	ent->chunk = chunk;
	found = 0;
	    
    } else if (found<0) {
	/*
	 * The chunk is larger than the entire cache so we don't cache it.
	 * This is the reason all those arguments have to be repeated for the
	 * unlock function.
	 */
	ent = NULL;
	found = -999;
	
    } else if (found>0) {
	/*
	 * The chunk is not at the beginning of the cache; move it forward by
	 * one slot.  This is how we implement the LRU preemption algorithm.
	 */
	H5F_rdcc_ent_t x = rdcc->slot[found];
	rdcc->slot[found] = rdcc->slot[found-1];
	rdcc->slot[found-1] = x;
	ent = rdcc->slot + --found;
    }

    /* Lock the chunk into the cache */
    if (ent) {
	assert (!ent->locked);
	ent->locked = TRUE;
	if (idx_hint) *idx_hint = found;
	chunk = ent->chunk;
    }

    ret_value = chunk;
 done:
    if (!ret_value) H5MM_xfree (chunk);
    H5MM_xfree (temp);
    FUNC_LEAVE (ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5F_istore_unlock
 *
 * Purpose:	Unlocks a previously locked chunk. The LAYOUT, COMP, and
 *		OFFSET arguments should be the same as for H5F_rdcc_lock().
 *		The DIRTY argument should be set to non-zero if the chunk has
 *		been modified since it was locked. The IDX_HINT argument is
 *		the returned index hint from the lock operation and BUF is
 *		the return value from the lock.
 *
 *		The NACCESSED argument should be the number of bytes accessed
 *		for reading or writing (depending on the value of DIRTY).
 *		It's only purpose is to provide additional information to the
 *		preemption policy.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Thursday, May 21, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5F_istore_unlock (H5F_t *f, const H5O_layout_t *layout,
		   const H5O_compress_t *comp, hbool_t dirty,
		   const hssize_t offset[], intn *idx_hint,
		   uint8 *chunk, size_t naccessed)
{
    H5F_rdcc_t		*rdcc = &(f->shared->rdcc);
    H5F_rdcc_ent_t	*ent = NULL;
    intn		i, found = -1;
    
    FUNC_ENTER (H5F_istore_unlock, FAIL);

    /* First look at the hint */
    if (idx_hint && *idx_hint>=0 && *idx_hint<rdcc->nused) {
	if (rdcc->slot[*idx_hint].chunk==chunk) found = *idx_hint;
    }

    /* Then look at all the entries */
    for (i=0; found<0 && i<rdcc->nused; i++) {
	if (rdcc->slot[i].chunk==chunk) found = i;
    }

    if (found<0) {
	/*
	 * It's not in the cache, probably because it's too big.  If it's
	 * dirty then flush it to disk.  In any case, free the chunk.
	 * Note: we have to copy the layout and compression messages so we
	 *	 don't discard the `const' qualifier.
	 */
	if (dirty) {
	    H5F_rdcc_ent_t x;
	    HDmemset (&x, 0, sizeof x);
	    x.dirty = TRUE;
	    x.layout = H5O_copy (H5O_LAYOUT, layout);
	    x.comp = H5O_copy (H5O_COMPRESS, comp);
	    for (i=0, x.chunk_size=1; i<layout->ndims; i++) {
		x.offset[i] = offset[i];
		x.chunk_size *= layout->dim[i];
	    }
	    x.chunk = chunk;
	    H5F_istore_flush_entry (f, &x);
	    H5O_free (H5O_LAYOUT, x.layout);
	    H5O_free (H5O_COMPRESS, x.comp);
	}
	H5MM_xfree (chunk);
    } else {
	/*
	 * It's in the cache so unlock it.
	 */
	ent = rdcc->slot + found;
	assert (ent->locked);
	if (dirty) {
	    ent->dirty = TRUE;
	    ent->wr_count -= MIN (ent->wr_count, naccessed);
	} else {
	    ent->rd_count -= MIN (ent->rd_count, naccessed);
	}
	ent->locked = FALSE;
    }
    
    FUNC_LEAVE (SUCCEED);
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
		const H5O_compress_t *comp, const hssize_t offset_f[],
		const hsize_t size[], void *buf)
{
    hssize_t		offset_m[H5O_LAYOUT_NDIMS];
    hsize_t		size_m[H5O_LAYOUT_NDIMS];
    hsize_t		idx_cur[H5O_LAYOUT_NDIMS];
    hsize_t		idx_min[H5O_LAYOUT_NDIMS];
    hsize_t		idx_max[H5O_LAYOUT_NDIMS];
    hsize_t		sub_size[H5O_LAYOUT_NDIMS];
    hssize_t		offset_wrt_chunk[H5O_LAYOUT_NDIMS];
    hssize_t		sub_offset_m[H5O_LAYOUT_NDIMS];
    hssize_t		chunk_offset[H5O_LAYOUT_NDIMS];
    intn		i, carry;
    size_t		naccessed;		/*bytes accessed in chnk*/
    uint8		*chunk=NULL;		/*ptr to a chunk buffer	*/
    intn		idx_hint=0;		/*cache index hint	*/

    FUNC_ENTER(H5F_istore_read, FAIL);

    /* Check args */
    assert (f);
    assert (layout && H5D_CHUNKED==layout->type);
    assert (layout->ndims>0 && layout->ndims<=H5O_LAYOUT_NDIMS);
    assert (H5F_addr_defined(&(layout->addr)));
    assert (offset_f);
    assert (size);
    assert (buf);

    /*
     * For now, a hyperslab of the file must be read into an array in
     * memory.We do not yet support reading into a hyperslab of memory.
     */
    for (i=0; i<layout->ndims; i++) {
	offset_m[i] = 0;
	size_m[i] = size[i];
    }
    
#ifndef NDEBUG
    for (i=0; i<layout->ndims; i++) {
	assert (offset_f[i]>=0); /*negative offsets not supported*/
	assert (offset_m[i]>=0); /*negative offsets not supported*/
	assert (size[i]<MAX_SIZET);
	assert(offset_m[i]+(hssize_t)size[i]<=(hssize_t)size_m[i]);
	assert(layout->dim[i]>0);
    }
#endif

    /*
     * Set up multi-dimensional counters (idx_min, idx_max, and idx_cur) and
     * loop through the chunks copying each to its final destination in the
     * application buffer.
     */
    for (i=0; i<layout->ndims; i++) {
	idx_min[i] = offset_f[i] / layout->dim[i];
	idx_max[i] = (offset_f[i]+size[i]-1) / layout->dim[i] + 1;
	idx_cur[i] = idx_min[i];
    }

    /* Loop over all chunks */
    while (1) {
	for (i=0, naccessed=1; i<layout->ndims; i++) {
	    /* The location and size of the chunk being accessed */
	    assert (layout->dim[i] < MAX_HSSIZET);
	    chunk_offset[i] = idx_cur[i] * (hssize_t)(layout->dim[i]);

	    /* The offset and size wrt the chunk */
	    offset_wrt_chunk[i] = MAX(offset_f[i], chunk_offset[i]) -
				  chunk_offset[i];
	    sub_size[i] = MIN((idx_cur[i]+1)*layout->dim[i],
			      offset_f[i]+size[i]) -
			  (chunk_offset[i] + offset_wrt_chunk[i]);
	    naccessed *= sub_size[i];
	    
	    /* Offset into mem buffer */
	    sub_offset_m[i] = chunk_offset[i] + offset_wrt_chunk[i] +
			      offset_m[i] - offset_f[i];
	}
#ifdef HAVE_PARALLEL
	/*
	 * If MPIO is used, must bypass the chunk-cache scheme
	 * because other MPI processes could be writing to other
	 * elements in the same chunk.
	 * Do a direct write-through of only the elements requested.
	 */
	 if (f->shared->access_parms->driver==H5F_LOW_MPIO){
	    H5F_istore_ud1_t	udata;
	    H5O_layout_t	l;	/* temporary layout */
	    if (H5F_istore_get_addr(f, layout, chunk_offset, &udata)==FAIL){
		HRETURN_ERROR (H5E_IO, H5E_WRITEERROR, FAIL,
				"unable to locate raw data chunk");
	    };
	    /*
	     * use default transfer mode as we do not support collective
	     * transfer mode since each data write could decompose into
	     * multiple chunk writes and we are not doing the calculation
	     * yet.
	     */
	    l.type = H5D_CONTIGUOUS;
	    l.ndims = layout->ndims;
	    for (i=l.ndims; i-- > 0;)
		l.dim[i] = layout->dim[i];
	    l.addr = udata.addr;
	    if (H5F_arr_read(f, &l,
			    comp, NULL /* no efl */,
			    sub_size, size_m,
			    sub_offset_m, offset_wrt_chunk,
			    H5D_XFER_DFLT, buf)==FAIL){
		HRETURN_ERROR (H5E_IO, H5E_READERROR, FAIL,
			     "unable to read raw data from file");
	    };
	}else{
#endif

#ifdef AKC
	    printf("Locking chunk( ");
	    for (i=0; i<layout->ndims; i++){
		printf("%ld ", chunk_offset[i]);
	    }
	    printf(")\n");
#endif

	/*
	 * Lock the chunk, transfer data to the application, then unlock the
	 * chunk.
	 */
	if (NULL==(chunk=H5F_istore_lock (f, layout, comp, chunk_offset,
					  FALSE, &idx_hint))) {
	    HRETURN_ERROR (H5E_IO, H5E_READERROR, FAIL,
			   "unable to read raw data chunk");
	}
	H5V_hyper_copy(layout->ndims, sub_size, size_m, sub_offset_m,
		       (void*)buf, layout->dim, offset_wrt_chunk, chunk);
	if (H5F_istore_unlock (f, layout, comp, FALSE, chunk_offset, &idx_hint,
			       chunk, naccessed)<0) {
	    HRETURN_ERROR (H5E_IO, H5E_READERROR, FAIL,
			   "unable to unlock raw data chunk");
	}
#ifdef HAVE_PARALLEL
	}
#endif

	/* Increment indices */
	for (i=layout->ndims-1, carry=1; i>=0 && carry; --i) {
	    if (++idx_cur[i]>=idx_max[i]) idx_cur[i] = idx_min[i];
	    else carry = 0;
	}
	if (carry) break;
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
		 const H5O_compress_t *comp, const hssize_t offset_f[],
		 const hsize_t size[], const void *buf)
{
    hssize_t		offset_m[H5O_LAYOUT_NDIMS];
    hsize_t		size_m[H5O_LAYOUT_NDIMS];
    intn		i, carry;
    hsize_t		idx_cur[H5O_LAYOUT_NDIMS];
    hsize_t		idx_min[H5O_LAYOUT_NDIMS];
    hsize_t		idx_max[H5O_LAYOUT_NDIMS];
    hsize_t		sub_size[H5O_LAYOUT_NDIMS];
    hssize_t		chunk_offset[H5O_LAYOUT_NDIMS];
    hssize_t		offset_wrt_chunk[H5O_LAYOUT_NDIMS];
    hssize_t		sub_offset_m[H5O_LAYOUT_NDIMS];
    uint8		*chunk=NULL;
    intn		idx_hint=0;
    size_t		chunk_size, naccessed;
    
    FUNC_ENTER(H5F_istore_write, FAIL);

    /* Check args */
    assert(f);
    assert(layout && H5D_CHUNKED==layout->type);
    assert(layout->ndims>0 && layout->ndims<=H5O_LAYOUT_NDIMS);
    assert(H5F_addr_defined(&(layout->addr)));
    assert (offset_f);
    assert(size);
    assert(buf);

    /*
     * For now the source must not be a hyperslab.  It must be an entire
     * memory buffer.
     */
    for (i=0, chunk_size=1; i<layout->ndims; i++) {
	offset_m[i] = 0;
	size_m[i] = size[i];
	chunk_size *= layout->dim[i];
    }

#ifndef NDEBUG
    for (i=0; i<layout->ndims; i++) {
	assert (offset_f[i]>=0); /*negative offsets not supported*/
	assert (offset_m[i]>=0); /*negative offsets not supported*/
	assert(size[i]<MAX_SIZET);
	assert(offset_m[i]+(hssize_t)size[i]<=(hssize_t)size_m[i]);
	assert(layout->dim[i]>0);
    }
#endif

    /*
     * Set up multi-dimensional counters (idx_min, idx_max, and idx_cur) and
     * loop through the chunks copying each chunk from the application to the
     * chunk cache.
     */
    for (i=0; i<layout->ndims; i++) {
	idx_min[i] = offset_f[i] / layout->dim[i];
	idx_max[i] = (offset_f[i]+size[i]-1) / layout->dim[i] + 1;
	idx_cur[i] = idx_min[i];
    }


    /* Loop over all chunks */
    while (1) {
	
	for (i=0, naccessed=1; i<layout->ndims; i++) {
	    /* The location and size of the chunk being accessed */
	    assert (layout->dim[i] < MAX_HSSIZET);
	    chunk_offset[i] = idx_cur[i] * (hssize_t)(layout->dim[i]);

	    /* The offset and size wrt the chunk */
	    offset_wrt_chunk[i] = MAX(offset_f[i], chunk_offset[i]) -
				  chunk_offset[i];
	    sub_size[i] = MIN((idx_cur[i]+1)*layout->dim[i],
			      offset_f[i]+size[i]) -
			  (chunk_offset[i] + offset_wrt_chunk[i]);
	    naccessed *= sub_size[i];
	    
	    /* Offset into mem buffer */
	    sub_offset_m[i] = chunk_offset[i] + offset_wrt_chunk[i] +
			      offset_m[i] - offset_f[i];
	}

#ifdef HAVE_PARALLEL
	/*
	 * If MPIO is used, must bypass the chunk-cache scheme
	 * because other MPI processes could be writing to other
	 * elements in the same chunk.
	 * Do a direct write-through of only the elements requested.
	 */
	 if (f->shared->access_parms->driver==H5F_LOW_MPIO){
	    H5F_istore_ud1_t	udata;
	    H5O_layout_t	l;	/* temporary layout */
	    if (H5F_istore_get_addr(f, layout, chunk_offset, &udata)==FAIL){
		HRETURN_ERROR (H5E_IO, H5E_WRITEERROR, FAIL,
				"unable to locate raw data chunk");
	    };
	    /*
	     * use default transfer mode as we do not support collective
	     * transfer mode since each data write could decompose into
	     * multiple chunk writes and we are not doing the calculation
	     * yet.
	     */
	    l.type = H5D_CONTIGUOUS;
	    l.ndims = layout->ndims;
	    for (i=l.ndims; i-- > 0;)
		l.dim[i] = layout->dim[i];
	    l.addr = udata.addr;
	    if (H5F_arr_write(f, &l,
			    comp, NULL /* no efl */,
			    sub_size, size_m,
			    sub_offset_m, offset_wrt_chunk,
			    H5D_XFER_DFLT, buf)==FAIL){
		HRETURN_ERROR (H5E_IO, H5E_WRITEERROR, FAIL,
			     "unable to write raw data to file");
	    };
	}else{
#endif

#ifdef AKC
	    printf("Locking chunk( ");
	    for (i=0; i<layout->ndims; i++){
		printf("%ld ", chunk_offset[i]);
	    }
	    printf(")\n");
#endif
	    /*
	     * Lock the chunk, copy from application to chunk, then unlock the
	     * chunk.
	     */
	    if (NULL==(chunk=H5F_istore_lock (f, layout, comp, chunk_offset,
					      naccessed==chunk_size,
					      &idx_hint))) {
		HRETURN_ERROR (H5E_IO, H5E_WRITEERROR, FAIL,
			       "unable to read raw data chunk");
	    }
	    H5V_hyper_copy(layout->ndims, sub_size,
			   layout->dim, offset_wrt_chunk, chunk,
			   size_m, sub_offset_m, buf);
	    if (H5F_istore_unlock (f, layout, comp, TRUE, chunk_offset,
				   &idx_hint, chunk, naccessed)<0) {
		HRETURN_ERROR (H5E_IO, H5E_WRITEERROR, FAIL,
			       "uanble to unlock raw data chunk");
	    }
#ifdef HAVE_PARALLEL
	}
#endif
	
	/* Increment indices */
	for (i=layout->ndims-1, carry=1; i>=0 && carry; --i) {
	    if (++idx_cur[i]>=idx_max[i]) idx_cur[i] = idx_min[i];
	    else carry = 0;
	}
	if (carry) break;
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
    if (H5B_create(f, H5B_ISTORE, &udata, &(layout->addr)/*out*/) < 0) {
	HRETURN_ERROR(H5E_IO, H5E_CANTINIT, FAIL, "can't create B-tree");
    }
    
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5F_istore_stats
 *
 * Purpose:	Print raw data cache statistics to the stderr stream.  If
 *		HEADERS is non-zero then print table column headers,
 *		otherwise assume that the H5AC layer has already printed them.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Thursday, May 21, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_istore_stats (H5F_t *f, hbool_t headers)
{
    H5F_rdcc_t	*rdcc = &(f->shared->rdcc);
    double	miss_rate;
    char	ascii[32];
    
    FUNC_ENTER (H5F_istore_stats, FAIL);

    if (headers) {
	fprintf(stderr, "H5F: raw data cache statistics for file %s\n",
		f->name);
	fprintf(stderr, "   %-18s %8s %8s %8s %8s+%-8s\n",
		"Layer", "Hits", "Misses", "MissRate", "Inits", "Flushes");
	fprintf(stderr, "   %-18s %8s %8s %8s %8s-%-8s\n",
		"-----", "----", "------", "--------", "-----", "-------");
    }

#ifndef H5AC_DEBUG
    /*
     * If we're not debugging the H5AC layer then print these statistics only
     * if we printed the headers that go with them.
     */
    if (headers) {
#endif
	if (rdcc->nhits>0 || rdcc->nmisses>0) {
	    miss_rate = 100.0 * rdcc->nmisses /
			    (rdcc->nhits + rdcc->nmisses);
	} else {
	    miss_rate = 0.0;
	}
	if (miss_rate > 100) {
	    sprintf(ascii, "%7d%%", (int) (miss_rate + 0.5));
	} else {
	    sprintf(ascii, "%7.2f%%", miss_rate);
	}

	fprintf(stderr, "   %-18s %8u %8u %7s %8d+%-9ld\n",
		"raw data chunks", rdcc->nhits, rdcc->nmisses, ascii,
		rdcc->ninits, (long)(rdcc->nflushes)-(long)(rdcc->ninits));
#ifndef H5AC_DEBUG
    }
#endif

    FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5F_istore_debug
 *
 * Purpose:	Debugs a B-tree node for indexed raw data storage.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Thursday, April 16, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_istore_debug(H5F_t *f, const haddr_t *addr, FILE * stream, intn indent,
		 intn fwidth, int ndims)
{
    H5F_istore_ud1_t	udata;
    
    FUNC_ENTER (H5F_istore_debug, FAIL);

    HDmemset (&udata, 0, sizeof udata);
    udata.mesg.ndims = ndims;

    H5B_debug (f, addr, stream, indent, fwidth, H5B_ISTORE, &udata);

    FUNC_LEAVE (SUCCEED);
}


#ifdef HAVE_PARALLEL
/*-------------------------------------------------------------------------
 * Function:	H5F_istore_get_addr
 *
 * Purpose:	Get the file address of a chunk if file space has been
 *		assigned.  Save the retrieved information in the udata
 *		supplied.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Albert Cheng
 *              June 27, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5F_istore_get_addr (H5F_t *f, const H5O_layout_t *layout,
		 const hssize_t offset[], void *_udata/*out*/)
{
    H5F_istore_ud1_t	*udata = _udata;
    intn		i;
    herr_t		status;			/*func return status	*/
    
    FUNC_ENTER (H5F_istore_get_addr, FAIL);

    assert(f);
    assert(layout && (layout->ndims > 0));
    assert(offset);
    assert(udata);


    for (i=0; i<layout->ndims; i++) {
	udata->key.offset[i] = offset[i];
    }
    udata->mesg = *layout;
    H5F_addr_undef (&(udata->addr));
    status = H5B_find (f, H5B_ISTORE, &(layout->addr), udata);
    H5E_clear ();
    if (status>=0 && H5F_addr_defined (&(udata->addr)))
	HRETURN(SUCCEED);

    FUNC_LEAVE (FAIL);
}


/*-------------------------------------------------------------------------
 * Function:	H5F_istore_allocate
 *
 * Purpose:	Allocate file space for all chunks that are not allocated yet.
 *		Return SUCCEED if all needed allocation succeed, otherwise
 *		FAIL.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Note:	Current implementation relies on cache_size being 0,
 *		thus no chunk is cashed and written to disk immediately
 *		when a chunk is unlocked (via H5F_istore_unlock)
 *		This should be changed to do a direct flush independent
 *		of the cache value.
 *
 * Programmer:	Albert Cheng
 *		June 26, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_istore_allocate (H5F_t *f, const H5O_layout_t *layout,
		const hsize_t *space_dim, const H5O_compress_t *comp)
{

    intn		i, carry;
    hssize_t		chunk_offset[H5O_LAYOUT_NDIMS];
    uint8		*chunk=NULL;
    intn		idx_hint=0;
    size_t		chunk_size;
#ifdef AKC
    H5F_istore_ud1_t	udata;
#endif
    
    FUNC_ENTER(H5F_istore_allocate, FAIL);
#ifdef AKC
    printf("Enter %s:\n", FUNC);
#endif

    /* Check args */
    assert(f);
    assert(space_dim);
    assert(comp);
    assert(layout && H5D_CHUNKED==layout->type);
    assert(layout->ndims>0 && layout->ndims<=H5O_LAYOUT_NDIMS);
    assert(H5F_addr_defined(&(layout->addr)));

    /*
     * Setup indice to go through all chunks. (Future improvement
     * should allocate only chunks that have no file space assigned yet.
     */
    for (i=0, chunk_size=1; i<layout->ndims; i++) {
	chunk_offset[i]=0;
	chunk_size *= layout->dim[i];
    }

    /* Loop over all chunks */
    while (1) {
	
#ifdef AKC
	printf("Checking allocation for chunk( ");
	for (i=0; i<layout->ndims; i++){
	    printf("%ld ", chunk_offset[i]);
	}
	printf(")\n");
#endif
#ifdef NO
	if (H5F_istore_get_addr(f, layout, chunk_offset, &udata)==FAIL){
#endif
	    /* No file space assigned yet.  Allocate it. */
	    /* The following needs improvement like calling the */
	    /* allocation directly rather than indirectly using the */
	    /* allocation effect in the unlock process. */

#ifdef AKC
	    printf("need allocation\n");
#endif
	    /*
	     * Lock the chunk, copy from application to chunk, then unlock the
	     * chunk.
	     */
	    if (NULL==(chunk=H5F_istore_lock (f, layout, comp, chunk_offset,
					      FALSE, &idx_hint))) {
		HRETURN_ERROR (H5E_IO, H5E_WRITEERROR, FAIL,
			       "unable to read raw data chunk");
	    }
	    if (H5F_istore_unlock (f, layout, comp, TRUE, chunk_offset, &idx_hint,
				   chunk, chunk_size)<0) {
		HRETURN_ERROR (H5E_IO, H5E_WRITEERROR, FAIL,
			       "uanble to unlock raw data chunk");
	    }
#ifdef NO
	} else {
#ifdef AKC
	    printf("NO need for allocation\n");
	    printf("udata.addr.offset=%d\n", udata.addr.offset);
#endif
	}
#endif
	
	/* Increment indices */
	for (i=layout->ndims-1, carry=1; i>=0 && carry; --i) {
	    chunk_offset[i] += layout->dim[i];
	    if (chunk_offset[i] >= space_dim[i]) chunk_offset[i] = 0;
	    else carry = 0;
	}
	if (carry) break;
    }

    FUNC_LEAVE(SUCCEED);
}
#endif
