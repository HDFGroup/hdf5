/*-------------------------------------------------------------------------
 * Copyright (C) 1997	National Center for Supercomputing Applications.
 *			All rights reserved.
 *
 *-------------------------------------------------------------------------
 *
 * Created:		snode.c
 *			Jun 26 1997
 *			Robb Matzke <matzke@llnl.gov>
 *
 * Purpose:		Functions for handling symbol table nodes.  A
 *			symbol table node is a small collection of symbol
 *			table entries.	A B-tree usually points to the
 *			symbol table nodes for any given symbol table.
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
#define H5G_PACKAGE /*suppress error message about including H5Gpkg.h */

/* Packages needed by this file... */
#include <H5private.h>		/*library		  */
#include <H5ACprivate.h>	/*cache				  */
#include <H5Bprivate.h>		/*B-link trees			  */
#include <H5Eprivate.h>		/*error handling	  */
#include <H5Fmpioprivate.h>	/*mpi low-level i/o	  */
#include <H5Gpkg.h>		/*me				  */
#include <H5HLprivate.h>		/*heap				  */
#include <H5MFprivate.h>	/*file memory management  */
#include <H5MMprivate.h>	/*core memory management  */
#include <H5Oprivate.h>		/*header messages	  */

#define PABLO_MASK	H5G_node_mask

/* PRIVATE PROTOTYPES */
static herr_t H5G_node_decode_key(H5F_t *f, H5B_t *bt, uint8 *raw,
				  void *_key);
static herr_t H5G_node_encode_key(H5F_t *f, H5B_t *bt, uint8 *raw,
				  void *_key);
static size_t H5G_node_size(H5F_t *f);
static herr_t H5G_node_create(H5F_t *f, H5B_ins_t op, void *_lt_key,
			      void *_udata, void *_rt_key,
			      haddr_t *addr/*out*/);
static herr_t H5G_node_flush(H5F_t *f, hbool_t destroy, const haddr_t *addr,
			     H5G_node_t *sym);
static H5G_node_t *H5G_node_load(H5F_t *f, const haddr_t *addr,
				 const void *_udata1, void *_udata2);
static intn H5G_node_cmp2(H5F_t *f, void *_lt_key, void *_udata,
			  void *_rt_key);
static intn H5G_node_cmp3(H5F_t *f, void *_lt_key, void *_udata,
			  void *_rt_key);
static herr_t H5G_node_found(H5F_t *f, const haddr_t *addr,
			     const void *_lt_key, void *_udata,
			     const void *_rt_key);
static H5B_ins_t H5G_node_insert(H5F_t *f, const haddr_t *addr,
				 void *_lt_key, hbool_t *lt_key_changed,
				 void *_md_key, void *_udata,
				 void *_rt_key, hbool_t *rt_key_changed,
				 haddr_t *new_node/*out*/);
static herr_t H5G_node_iterate(H5F_t *f, const haddr_t *addr, void *_udata);
static size_t H5G_node_sizeof_rkey(H5F_t *f, const void *_udata);

/* H5G inherits cache-like properties from H5AC */
const H5AC_class_t H5AC_SNODE[1] = {{
    H5AC_SNODE_ID,
    (void *(*)(H5F_t*, const haddr_t*, const void*, void*))H5G_node_load,
    (herr_t (*)(H5F_t*, hbool_t, const haddr_t*, void*))H5G_node_flush,
}};

/* H5G inherits B-tree like properties from H5B */
H5B_class_t H5B_SNODE[1] = {{
    H5B_SNODE_ID,		/*id			*/
    sizeof(H5G_node_key_t), 	/*sizeof_nkey		*/
    H5G_node_sizeof_rkey,	/*get_sizeof_rkey	*/
    H5G_node_create,		/*new			*/
    H5G_node_cmp2,		/*cmp2			*/
    H5G_node_cmp3,		/*cmp3			*/
    H5G_node_found,		/*found			*/
    H5G_node_insert,		/*insert		*/
    TRUE,			/*follow min branch?	*/
    TRUE,			/*follow max branch?	*/
    H5G_node_iterate,		/*list			*/
    H5G_node_decode_key,	/*decode		*/
    H5G_node_encode_key,	/*encode		*/
    NULL,			/*debug key		*/
}};

/* Interface initialization */
static intn interface_initialize_g = FALSE;
#define INTERFACE_INIT	NULL


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
 *		matzke@llnl.gov
 *		Jul 14 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static size_t
H5G_node_sizeof_rkey(H5F_t *f, const void __unused__ *udata)
{
    return H5F_SIZEOF_SIZE(f);	/*the name offset */
}


/*-------------------------------------------------------------------------
 * Function:	H5G_node_decode_key
 *
 * Purpose:	Decodes a raw key into a native key.
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
H5G_node_decode_key(H5F_t *f, H5B_t __unused__ *bt, uint8 *raw, void *_key)
{
    H5G_node_key_t	   *key = (H5G_node_key_t *) _key;

    FUNC_ENTER(H5G_node_decode_key, FAIL);

    assert(f);
    assert(raw);
    assert(key);

    H5F_decode_length(f, raw, key->offset);

    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5G_node_encode_key
 *
 * Purpose:	Encodes a native key into a raw key.
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
H5G_node_encode_key(H5F_t *f, H5B_t __unused__ *bt, uint8 *raw, void *_key)
{
    H5G_node_key_t	   *key = (H5G_node_key_t *) _key;

    FUNC_ENTER(H5G_node_encode_key, FAIL);

    assert(f);
    assert(raw);
    assert(key);

    H5F_encode_length(f, raw, key->offset);

    FUNC_LEAVE(SUCCEED);
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
 *		matzke@llnl.gov
 *		Jun 23 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static size_t
H5G_node_size(H5F_t *f)
{
    return H5G_NODE_SIZEOF_HDR(f) +
	(2 * H5G_NODE_K(f)) * H5G_SIZEOF_ENTRY(f);
}


/*-------------------------------------------------------------------------
 * Function:	H5G_node_create
 *
 * Purpose:	Creates a new empty symbol table node.	This function is
 *		called by the B-tree insert function for an empty tree.	 It
 *		is also called internally to split a symbol node with LT_KEY
 *		and RT_KEY null pointers.
 *
 * Return:	Success:	SUCCEED.  The address of symbol table node is
 *				returned through the ADDR argument.
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
H5G_node_create(H5F_t *f, H5B_ins_t __unused__ op, void *_lt_key,
		void __unused__ *_udata, void *_rt_key, haddr_t *addr/*out*/)
{
    H5G_node_key_t	   *lt_key = (H5G_node_key_t *) _lt_key;
    H5G_node_key_t	   *rt_key = (H5G_node_key_t *) _rt_key;
    H5G_node_t		   *sym = NULL;
    hsize_t		    size = 0;

    FUNC_ENTER(H5G_node_create, FAIL);

    /*
     * Check arguments.
     */
    assert(f);
    assert(H5B_INS_FIRST == op);

    if (NULL==(sym = H5MM_calloc(sizeof(H5G_node_t)))) {
	HRETURN_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL,
		       "memory allocation failed");
    }
    size = H5G_node_size(f);
    if (H5MF_alloc(f, H5MF_META, size, addr/*out*/) < 0) {
	H5MM_xfree(sym);
	HRETURN_ERROR(H5E_SYM, H5E_CANTINIT, FAIL,
		      "unable to allocate file space");
    }
    sym->dirty = TRUE;
    sym->entry = H5MM_calloc(2*H5G_NODE_K(f)*sizeof(H5G_entry_t));
    if (NULL==sym->entry) {
	H5MM_xfree (sym);
	HRETURN_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL,
		       "memory allocation failed");
    }
    if (H5AC_set(f, H5AC_SNODE, addr, sym) < 0) {
	H5MM_xfree(sym->entry);
	H5MM_xfree(sym);
	HRETURN_ERROR(H5E_SYM, H5E_CANTINIT, FAIL,
		      "unable to cache symbol table leaf node");
    }
    /*
     * The left and right symbols in an empty tree are both the
     * empty string stored at offset zero by the H5G functions. This
     * allows the comparison functions to work correctly without knowing
     * that there are no symbols.
     */
    if (lt_key) lt_key->offset = 0;
    if (rt_key) rt_key->offset = 0;

    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5G_node_flush
 *
 * Purpose:	Flush a symbol table node to disk.
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
 *              rky 980828 Only p0 writes metadata to disk.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5G_node_flush(H5F_t *f, hbool_t destroy, const haddr_t *addr,
	       H5G_node_t *sym)
{
    uint8	*buf = NULL, *p = NULL;
    size_t	size;
    herr_t	status;
    int		i;

    FUNC_ENTER(H5G_node_flush, FAIL);

    /*
     * Check arguments.
     */
    assert(f);
    assert(addr && H5F_addr_defined(addr));
    assert(sym);

    /*
     * Look for dirty entries and set the node dirty flag.
     */
    for (i=0; i<sym->nsyms; i++) {
	if (sym->entry[i].dirty) sym->dirty = TRUE;
    }

    /*
     * Write the symbol node to disk.
     */
    if (sym->dirty) {
	size = H5G_node_size(f);
	if (NULL==(buf = p = H5MM_malloc(size))) {
	    HRETURN_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL,
			   "memory allocation failed");
	}

	/* magic number */
	HDmemcpy(p, H5G_NODE_MAGIC, H5G_NODE_SIZEOF_MAGIC);
	p += 4;

	/* version number */
	*p++ = H5G_NODE_VERS;

	/* reserved */
	*p++ = 0;

	/* number of symbols */
	UINT16ENCODE(p, sym->nsyms);

	/* entries */
	H5G_ent_encode_vec(f, &p, sym->entry, sym->nsyms);
	HDmemset(p, 0, size - (p - buf));

#ifdef HAVE_PARALLEL
	H5F_mpio_tas_allsame( f->shared->lf, TRUE );	/* only p0 will write */
#endif /* HAVE_PARALLEL */
	status = H5F_block_write(f, addr, (hsize_t)size, H5D_XFER_DFLT, buf);
	buf = H5MM_xfree(buf);
	if (status < 0)
	    HRETURN_ERROR(H5E_SYM, H5E_WRITEERROR, FAIL,
			  "unable to write symbol table node to "
			  "the file");
    }
    /*
     * Destroy the symbol node?	 This might happen if the node is being
     * preempted from the cache.
     */
    if (destroy) {
	sym->entry = H5MM_xfree(sym->entry);
	H5MM_xfree(sym);
    }
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5G_node_load
 *
 * Purpose:	Loads a symbol table node from the file.
 *
 * Return:	Success:	Ptr to the new table.
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
static H5G_node_t *
H5G_node_load(H5F_t *f, const haddr_t *addr, const void __unused__ *_udata1,
	      void __unused__ *_udata2)
{
    H5G_node_t		   *sym = NULL;
    size_t		    size = 0;
    uint8		   *buf = NULL;
    const uint8		   *p = NULL;
    H5G_node_t		   *ret_value = NULL;	/*for error handling */

    FUNC_ENTER(H5G_node_load, NULL);

    /*
     * Check arguments.
     */
    assert(f);
    assert(addr && H5F_addr_defined(addr));
    assert(!_udata1);
    assert(NULL == _udata2);

    /*
     * Initialize variables.
     */
    size = H5G_node_size(f);
    if (NULL==(p = buf = H5MM_malloc(size))) {
	HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL,
		     "memory allocation failed for symbol table node");
    }
    if (NULL==(sym = H5MM_calloc(sizeof(H5G_node_t))) ||
	NULL==(sym->entry=H5MM_calloc(2*H5G_NODE_K(f)*sizeof(H5G_entry_t)))) {
	HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL,
		     "memory allocation failed");
    }
    if (H5F_block_read(f, addr, (hsize_t)size, H5D_XFER_DFLT, buf) < 0) {
	HGOTO_ERROR(H5E_SYM, H5E_READERROR, NULL,
		    "unabel to read symbol table node");
    }
    /* magic */
    if (HDmemcmp(p, H5G_NODE_MAGIC, H5G_NODE_SIZEOF_MAGIC)) {
	HGOTO_ERROR(H5E_SYM, H5E_CANTLOAD, NULL,
		    "bad symbol table node signature");
    }
    p += 4;

    /* version */
    if (H5G_NODE_VERS != *p++) {
	HGOTO_ERROR(H5E_SYM, H5E_CANTLOAD, NULL,
		    "bad symbol table node version");
    }
    /* reserved */
    p++;

    /* number of symbols */
    UINT16DECODE(p, sym->nsyms);

    /* entries */
    if (H5G_ent_decode_vec(f, &p, sym->entry, sym->nsyms) < 0) {
	HGOTO_ERROR(H5E_SYM, H5E_CANTLOAD, NULL,
		    "unable to decode symbol table entries");
    }
    buf = H5MM_xfree(buf);

    ret_value = sym;

  done:
    if (!ret_value) {
	buf = H5MM_xfree(buf);
	if (sym) {
	    sym->entry = H5MM_xfree(sym->entry);
	    sym = H5MM_xfree(sym);
	}
    }
    FUNC_LEAVE(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5G_node_cmp2
 *
 * Purpose:	Compares two keys from a B-tree node (LT_KEY and RT_KEY).
 *		The UDATA pointer supplies extra data not contained in the
 *		keys (in this case, the heap address).
 *
 * Return:	Success:	negative if LT_KEY is less than RT_KEY.
 *
 *				positive if LT_KEY is greater than RT_KEY.
 *
 *				zero if LT_KEY and RT_KEY are equal.
 *
 *		Failure:	FAIL (same as LT_KEY<RT_KEY)
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Jun 23 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static intn
H5G_node_cmp2(H5F_t *f, void *_lt_key, void *_udata, void *_rt_key)
{
    H5G_bt_ud1_t	   *udata = (H5G_bt_ud1_t *) _udata;
    H5G_node_key_t	   *lt_key = (H5G_node_key_t *) _lt_key;
    H5G_node_key_t	   *rt_key = (H5G_node_key_t *) _rt_key;
    const char		   *s1, *s2;
    intn		    cmp;

    FUNC_ENTER(H5G_node_cmp2, FAIL);

    assert(udata);
    assert(lt_key);
    assert(rt_key);

    if (NULL == (s1 = H5HL_peek(f, &(udata->heap_addr), lt_key->offset))) {
	HRETURN_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL,
		      "unable to read symbol name");
    }
    if (NULL == (s2 = H5HL_peek(f, &(udata->heap_addr), rt_key->offset))) {
	HRETURN_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL,
		      "unable to read symbol name");
    }
    cmp = HDstrcmp(s1, s2);

    FUNC_LEAVE(cmp);
}


/*-------------------------------------------------------------------------
 * Function:	H5G_node_cmp3
 *
 * Purpose:	Compares two keys from a B-tree node (LT_KEY and RT_KEY)
 *		against another key (not necessarily the same type)
 *		pointed to by UDATA.
 *
 * Return:	Success:	negative if the UDATA key is less than
 *				or equal to the LT_KEY
 *
 *				positive if the UDATA key is greater
 *				than the RT_KEY.
 *
 *				zero if the UDATA key falls between
 *				the LT_KEY (exclusive) and the
 *				RT_KEY (inclusive).
 *
 *		Failure:	FAIL (same as UDATA < LT_KEY)
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Jun 23 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static intn
H5G_node_cmp3(H5F_t *f, void *_lt_key, void *_udata, void *_rt_key)
{
    H5G_bt_ud1_t	   *udata = (H5G_bt_ud1_t *) _udata;
    H5G_node_key_t	   *lt_key = (H5G_node_key_t *) _lt_key;
    H5G_node_key_t	   *rt_key = (H5G_node_key_t *) _rt_key;
    const char		   *s;

    FUNC_ENTER(H5G_node_cmp3, FAIL);

    /* left side */
    if (NULL == (s = H5HL_peek(f, &(udata->heap_addr), lt_key->offset))) {
	HRETURN_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL,
		      "unable to read symbol name");
    }
    if (HDstrcmp(udata->name, s) <= 0)
	HRETURN(-1);

    /* right side */
    if (NULL == (s = H5HL_peek(f, &(udata->heap_addr), rt_key->offset))) {
	HRETURN_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL,
		      "unable to read symbol name");
    }
    if (HDstrcmp(udata->name, s) > 0)
	HRETURN(1);

    FUNC_LEAVE(0);
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
 *		If the operation flag in UDATA is H5G_OPER_FIND, then
 *		the entry is copied from the symbol table to the UDATA
 *		entry field.  Otherwise the entry is copied from the
 *		UDATA entry field to the symbol table.
 *
 * Return:	Success:	SUCCEED if found and data returned through
 *				the UDATA pointer.
 *
 *		Failure:	FAIL if not found.
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
H5G_node_found(H5F_t *f, const haddr_t *addr, const void __unused__ *_lt_key,
	       void *_udata, const void __unused__ *_rt_key)
{
    H5G_bt_ud1_t	*bt_udata = (H5G_bt_ud1_t *) _udata;
    H5G_node_t		*sn = NULL;
    intn		lt = 0, idx = 0, rt, cmp = 1;
    const char		*s;
    herr_t		ret_value = FAIL;

    FUNC_ENTER(H5G_node_found, FAIL);

    /*
     * Check arguments.
     */
    assert(f);
    assert(addr && H5F_addr_defined(addr));
    assert(bt_udata);

    /*
     * Load the symbol table node for exclusive access.
     */
    if (NULL == (sn = H5AC_protect(f, H5AC_SNODE, addr, NULL, NULL))) {
	HGOTO_ERROR(H5E_SYM, H5E_CANTLOAD, FAIL,
		    "unable to protect symbol table node");
    }

    /*
     * Binary search.
     */
    rt = sn->nsyms;
    while (lt < rt && cmp) {
	idx = (lt + rt) / 2;
	if (NULL == (s = H5HL_peek(f, &(bt_udata->heap_addr),
				  sn->entry[idx].name_off))) {
	    HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL,
			"unable to read symbol name");
	}
	cmp = HDstrcmp(bt_udata->name, s);

	if (cmp < 0) {
	    rt = idx;
	} else {
	    lt = idx + 1;
	}
    }
    if (cmp)
	HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "not found");

    switch (bt_udata->operation) {
    case H5G_OPER_FIND:
	/*
	 * The caller is querying the symbol entry.  Return just a pointer to
	 * the entry.  The pointer is valid until the next call to H5AC.
	 */
	bt_udata->ent = sn->entry[idx];
	break;

    default:
	HRETURN_ERROR(H5E_SYM, H5E_UNSUPPORTED, FAIL,
		      "internal erorr (unknown symbol find operation)");
    }
    ret_value = SUCCEED;

  done:
    if (sn && H5AC_unprotect(f, H5AC_SNODE, addr, sn) < 0) {
	HRETURN_ERROR(H5E_SYM, H5E_PROTECT, FAIL,
		      "unable to release symbol table node");
    }
    FUNC_LEAVE(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5G_node_insert
 *
 * Purpose:	The B-tree insertion engine has found the symbol table node
 *		which should receive the new symbol/address pair.  This
 *		function adds it to that node unless it already existed.
 *
 *		If the node has no room for the symbol then the node is
 *		split into two nodes.  The original node contains the
 *		low values and the new node contains the high values.
 *		The new symbol table entry is added to either node as
 *		appropriate.  When a split occurs, this function will
 *		write the maximum key of the low node to the MID buffer
 *		and return the address of the new node.
 *
 *		If the new key is larger than RIGHT then update RIGHT
 *		with the new key.
 *
 * Return:	Success:	An insertion command for the caller, one of
 *				the H5B_INS_* constants.  The address of the
 *				new node, if any, is returned through the
 *				NEW_NODE argument.  NEW_NODE might not be
 *				initialized if the return value is
 *				H5B_INS_NOOP.
 *
 *		Failure:	H5B_INS_ERROR, NEW_NODE might not be
 *				initialized.
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Jun 24 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static H5B_ins_t
H5G_node_insert(H5F_t *f, const haddr_t *addr, void __unused__ *_lt_key,
		hbool_t __unused__ *lt_key_changed, void *_md_key,
		void *_udata, void *_rt_key,
		hbool_t __unused__ *rt_key_changed, haddr_t *new_node)
{
    H5G_node_key_t	*md_key = (H5G_node_key_t *) _md_key;
    H5G_node_key_t	*rt_key = (H5G_node_key_t *) _rt_key;
    H5G_bt_ud1_t	*bt_udata = (H5G_bt_ud1_t *) _udata;

    H5G_node_t		*sn = NULL, *snrt = NULL;
    size_t		offset;			/*offset of name in heap */
    const char		*s;
    intn		idx = -1, cmp = 1;
    intn		lt = 0, rt;		/*binary search cntrs	*/
    H5B_ins_t		ret_value = H5B_INS_ERROR;
    H5G_node_t		*insert_into = NULL;	/*node that gets new entry*/

    FUNC_ENTER(H5G_node_insert, H5B_INS_ERROR);

    /*
     * Check arguments.
     */
    assert(f);
    assert(addr && H5F_addr_defined(addr));
    assert(md_key);
    assert(rt_key);
    assert(bt_udata);
    assert(new_node);

    /*
     * Load the symbol node.
     */
    if (NULL == (sn = H5AC_protect(f, H5AC_SNODE, addr, NULL, NULL))) {
	HGOTO_ERROR(H5E_SYM, H5E_CANTLOAD, H5B_INS_ERROR,
		    "unable to protect symbol table node");
    }

    /*
     * Where does the new symbol get inserted?	We use a binary search.
     */
    rt = sn->nsyms;
    while (lt < rt) {
	idx = (lt + rt) / 2;
	if (NULL == (s = H5HL_peek(f, &(bt_udata->heap_addr),
				  sn->entry[idx].name_off))) {
	    HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, H5B_INS_ERROR,
			"unable to read symbol name");
	}
	if (0 == (cmp = HDstrcmp(bt_udata->name, s))) {
	    /*already present */
	    HGOTO_ERROR(H5E_SYM, H5E_CANTINSERT, H5B_INS_ERROR,
			"symbol is already present in symbol table");
	}
	if (cmp < 0) {
	    rt = idx;
	} else {
	    lt = idx + 1;
	}
    }
    idx += cmp > 0 ? 1 : 0;

    /*
     * Add the new name to the heap.
     */
    offset = H5HL_insert(f, &(bt_udata->heap_addr), HDstrlen(bt_udata->name)+1,
			bt_udata->name);
    bt_udata->ent.name_off = offset;
    if (0==offset || (size_t)(-1)==offset) {
	HGOTO_ERROR(H5E_SYM, H5E_CANTINSERT, H5B_INS_ERROR,
		    "unable to insert symbol name into heap");
    }
    if ((size_t)(sn->nsyms) >= 2*H5G_NODE_K(f)) {
	/*
	 * The node is full.  Split it into a left and right
	 * node and return the address of the new right node (the
	 * left node is at the same address as the original node).
	 */
	ret_value = H5B_INS_RIGHT;

	/* The right node */
	if (H5G_node_create(f, H5B_INS_FIRST, NULL, NULL, NULL,
			    new_node /*out */ ) < 0) {
	    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, H5B_INS_ERROR,
			"unable to split symbol table node");
	}
	if (NULL == (snrt = H5AC_find(f, H5AC_SNODE, new_node, NULL, NULL))) {
	    HGOTO_ERROR(H5E_SYM, H5E_CANTLOAD, H5B_INS_ERROR,
			"unable to split symbol table node");
	}
	HDmemcpy(snrt->entry, sn->entry + H5G_NODE_K(f),
		 H5G_NODE_K(f) * sizeof(H5G_entry_t));
	snrt->nsyms = H5G_NODE_K(f);
	snrt->dirty = TRUE;

	/* The left node */
	HDmemset(sn->entry + H5G_NODE_K(f), 0,
		 H5G_NODE_K(f) * sizeof(H5G_entry_t));
	sn->nsyms = H5G_NODE_K(f);
	sn->dirty = TRUE;

	/* The middle key */
	md_key->offset = sn->entry[sn->nsyms - 1].name_off;

	/* Where to insert the new entry? */
	if (idx <= (intn)H5G_NODE_K(f)) {
	    insert_into = sn;
	    if (idx == (intn)H5G_NODE_K(f))
		md_key->offset = offset;
	} else {
	    idx -= H5G_NODE_K(f);
	    insert_into = snrt;
	    if (idx == (intn)H5G_NODE_K (f)) {
		rt_key->offset = offset;
		*rt_key_changed = TRUE;
	    }
	}

    } else {
	/* Where to insert the new entry? */
	ret_value = H5B_INS_NOOP;
	sn->dirty = TRUE;
	insert_into = sn;
	if (idx == sn->nsyms) {
	    rt_key->offset = offset;
	    *rt_key_changed = TRUE;
	}
    }

    /* Move entries */
    HDmemmove(insert_into->entry + idx + 1,
	      insert_into->entry + idx,
	      (insert_into->nsyms - idx) * sizeof(H5G_entry_t));
    insert_into->entry[idx] = bt_udata->ent;
    insert_into->entry[idx].dirty = TRUE;
    insert_into->nsyms += 1;

  done:
    if (sn && H5AC_unprotect(f, H5AC_SNODE, addr, sn) < 0) {
	HRETURN_ERROR(H5E_SYM, H5E_PROTECT, H5B_INS_ERROR,
		      "unable to release symbol table node");
    }
    FUNC_LEAVE(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5G_node_iterate
 *
 * Purpose:	This function gets called during a group iterate operation.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Jun 24 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5G_node_iterate (H5F_t *f, const haddr_t *addr, void *_udata)
{
    H5G_bt_ud2_t	*bt_udata = (H5G_bt_ud2_t *)_udata;
    H5G_node_t		*sn = NULL;
    intn		i, nsyms;
    size_t		n, *name_off=NULL;
    const char		*name;
    char		buf[1024], *s;
    herr_t		ret_value = FAIL;

    FUNC_ENTER(H5G_node_iterate, FAIL);

    /*
     * Check arguments.
     */
    assert(f);
    assert(addr && H5F_addr_defined(addr));
    assert(bt_udata);

    /*
     * Save information about the symbol table node since we can't lock it
     * because we're about to call an application function.
     */
    if (NULL == (sn = H5AC_find(f, H5AC_SNODE, addr, NULL, NULL))) {
	HGOTO_ERROR(H5E_SYM, H5E_CANTLOAD, FAIL,
		    "unable to load symbol table node");
    }
    nsyms = sn->nsyms;
    if (NULL==(name_off = H5MM_malloc (nsyms*sizeof(name_off[0])))) {
	HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL,
		     "memory allocation failed");
    }
    for (i=0; i<nsyms; i++) name_off[i] = sn->entry[i].name_off;
    sn = NULL;

    /*
     * Iterate over the symbol table node entries.
     */
    for (i=0, ret_value=0; i<nsyms && 0==ret_value; i++) {
	if (bt_udata->skip>0) {
	    --bt_udata->skip;
	} else {
	    name = H5HL_peek (f, &(bt_udata->group->ent.cache.stab.heap_addr),
			     name_off[i]);
	    assert (name);
	    n = strlen (name);
	    if (n+1>sizeof(buf)) {
		if (NULL==(s = H5MM_malloc (n+1))) {
		    HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL,
				 "memory allocation failed");
		}
	    } else {
		s = buf;
	    }
	    strcpy (s, name);
	    ret_value = (bt_udata->op)(bt_udata->group_id, s,
				       bt_udata->op_data);
	    if (s!=buf) H5MM_xfree (s);
	}
    }
    if (ret_value<0) {
	HERROR (H5E_SYM, H5E_CANTINIT, "iteration operator failed");
    }

  done:
    name_off = H5MM_xfree (name_off);
    FUNC_LEAVE(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5G_node_debug
 *
 * Purpose:	Prints debugging information about a symbol table node
 *		or a B-tree node for a symbol table B-tree.
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
H5G_node_debug(H5F_t *f, const haddr_t *addr, FILE * stream, intn indent,
	       intn fwidth, const haddr_t *heap)
{
    int			    i;
    H5G_node_t		   *sn = NULL;
    herr_t		    status;
    const char		   *s;

    FUNC_ENTER(H5G_node_debug, FAIL);

    /*
     * Check arguments.
     */
    assert(f);
    assert(addr && H5F_addr_defined(addr));
    assert(stream);
    assert(indent >= 0);
    assert(fwidth >= 0);

    /*
     * If we couldn't load the symbol table node, then try loading the
     * B-tree node.
     */
    if (NULL == (sn = H5AC_protect(f, H5AC_SNODE, addr, NULL, NULL))) {
	H5E_clear(); /*discard that error */
	status = H5B_debug(f, addr, stream, indent, fwidth, H5B_SNODE, NULL);
	if (status < 0) {
	    HRETURN_ERROR(H5E_SYM, H5E_CANTLOAD, FAIL,
			  "unable to debug B-tree node");
	}
	HRETURN(SUCCEED);
    }
    fprintf(stream, "%*sSymbol Table Node...\n", indent, "");
    fprintf(stream, "%*s%-*s %s\n", indent, "", fwidth,
	    "Dirty:",
	    sn->dirty ? "Yes" : "No");
    fprintf(stream, "%*s%-*s %d of %d\n", indent, "", fwidth,
	    "Number of Symbols:",
	    sn->nsyms, 2 * H5G_NODE_K(f));

    indent += 3;
    fwidth = MAX(0, fwidth - 3);
    for (i = 0; i < sn->nsyms; i++) {
	fprintf(stream, "%*sSymbol %d:\n", indent - 3, "", i);
	if (H5F_addr_defined(heap) &&
	    (s = H5HL_peek(f, heap, sn->entry[i].name_off))) {
	    fprintf(stream, "%*s%-*s `%s'\n", indent, "", fwidth,
		    "Name:",
		    s);
	}
	H5G_ent_debug(f, sn->entry + i, stream, indent, fwidth, heap);
    }

    H5AC_unprotect(f, H5AC_SNODE, addr, sn);
    FUNC_LEAVE(SUCCEED);
}
