/*
 * Copyright (C) 1998 NCSA
 *                    All rights reserved.
 *
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Friday, March 27, 1998
 *
 * Purpose:	Operations on the global heap.  The global heap is the set of
 *		all collections and each collection contains one or more
 *		global heap objects.  An object belongs to exactly one
 *		collection.  A collection is treated as an atomic entity for
 *		the purposes of I/O and caching.
 *
 *		Each file has a small cache of global heap collections called
 *		the CWFS list and recently accessed collections with free
 *		space appear on this list.  As collections are accessed the
 *		collection is moved toward the front of the list.  New
 *		collections are added to the front of the list while old
 *		collections are added to the end of the list.
 *
 *		The collection model reduces the overhead which would be
 *		incurred if the global heap were a single object, and the
 *		CWFS list allows the library to cheaply choose a collection
 *		for a new object based on object size, amount of free space
 *		in the collection, and temporal locality.
 */
#include <H5private.h>		/*library		  		*/
#include <H5ACprivate.h>	/*caching				*/
#include <H5Eprivate.h>		/*error handling			*/
#include <H5HGprivate.h>	/*global heaps				*/
#include <H5MFprivate.h>	/*file memory management		*/
#include <H5MMprivate.h>	/*memory management			*/

#define PABLO_MASK	H5HG_mask

typedef struct H5HG_obj_t {
    intn		nrefs;		/*reference count		*/
    size_t		size;		/*total size of object		*/
    uint8		*begin;		/*ptr to object into heap->chunk*/
} H5HG_obj_t;

struct H5HG_heap_t {
    haddr_t		addr;		/*collection address		*/
    hbool_t		dirty;		/*does heap need to be saved?	*/
    size_t		size;		/*total size of collection	*/
    uint8		*chunk;		/*the collection, incl. header	*/
    intn		nalloc;		/*numb object slots allocated	*/
    H5HG_obj_t		*obj;		/*array of object descriptions	*/
};

/* PRIVATE PROTOTYPES */
static H5HG_heap_t *H5HG_load(H5F_t *f, const haddr_t *addr,
			      const void *udata1, void *udata2);
static herr_t H5HG_flush(H5F_t *f, hbool_t dest, const haddr_t *addr,
			 H5HG_heap_t *heap);

/*
 * H5HG inherits cache-like properties from H5AC
 */
static const H5AC_class_t H5AC_GHEAP[1] = {{
    H5AC_GHEAP_ID,
    (void *(*)(H5F_t*, const haddr_t*, const void*, void*))H5HG_load,
    (herr_t (*)(H5F_t*, hbool_t, const haddr_t*, void*))H5HG_flush,
}};

/* Interface initialization */
static intn interface_initialize_g = FALSE;
#define INTERFACE_INIT NULL


/*-------------------------------------------------------------------------
 * Function:	H5HG_create
 *
 * Purpose:	Creates a global heap collection of the specified size.  If
 *		SIZE is less than some minimum it will be readjusted.  The
 *		new collection is allocated in the file and added to the
 *		beginning of the CWFS list.
 *
 * Return:	Success:	Ptr to a cached heap.  The pointer is valid
 *				only until some other hdf5 library function
 *				is called.
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *              Friday, March 27, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
H5HG_heap_t *
H5HG_create (H5F_t *f, size_t size)
{
    H5HG_heap_t	*heap = NULL;
    H5HG_heap_t	*ret_value = NULL;
    uint8	*p = NULL;
    haddr_t	addr;
    
    FUNC_ENTER (H5HG_create, NULL);

    /* Check args */
    assert (f);
    if (size<H5HG_MINSIZE) size = H5HG_MINSIZE;

    /* Create it */
    if (H5MF_alloc (f, H5MF_META, (hsize_t)size, &addr/*out*/)<0) {
	HGOTO_ERROR (H5E_HEAP, H5E_CANTINIT, NULL,
		     "unable to allocate file space for global heap");
    }
    heap = H5MM_xcalloc (1, sizeof(H5HG_heap_t));
    heap->addr = addr;
    heap->size = size;
    heap->dirty = TRUE;
    heap->chunk = H5MM_xmalloc (size);
    heap->nalloc = H5HG_NOBJS (f, size);
    heap->obj = H5MM_xcalloc (heap->nalloc, sizeof(H5HG_obj_t));

    /* Initialize the header */
    HDmemcpy (heap->chunk, H5HG_MAGIC, H5HG_SIZEOF_MAGIC);
    p = heap->chunk + H5HG_SIZEOF_MAGIC;
    *p++ = H5HG_VERSION;
    *p++ = 0; /*reserved*/
    *p++ = 0; /*reserved*/
    *p++ = 0; /*reserved*/
    H5F_encode_length (f, p, size);

    /* The freespace object */
    heap->obj[0].size = size - H5HG_SIZEOF_HDR (f);
    heap->obj[0].begin = p;
    UINT16ENCODE (p, 0);	/*object ID*/
    UINT16ENCODE (p, 0);	/*reference count*/
    H5F_encode_length (f, p, heap->obj[0].size);
    HDmemset (p, 0, (size_t)((heap->chunk+heap->nalloc) - p));

    /* Add the heap to the cache */
    if (H5AC_set (f, H5AC_GHEAP, &addr, heap)<0) {
	HGOTO_ERROR (H5E_HEAP, H5E_CANTINIT, NULL,
		     "unable to cache global heap collection");
    }

    /* Add this heap to the beginning of the CWFS list */
    if (NULL==f->shared->cwfs) {
	f->shared->cwfs = H5MM_xmalloc (H5HG_NCWFS * sizeof(H5HG_heap_t*));
	f->shared->cwfs[0] = heap;
	f->shared->ncwfs = 1;
    } else {
	HDmemmove (f->shared->cwfs+1, f->shared->cwfs,
		   MIN (f->shared->ncwfs, H5HG_NCWFS-1)*sizeof(H5HG_heap_t*));
	f->shared->cwfs[0] = heap;
	f->shared->ncwfs = MIN (H5HG_NCWFS, f->shared->ncwfs+1);
    }

    ret_value = heap;

 done:
    if (!ret_value && heap) {
	H5MM_xfree (heap->chunk);
	H5MM_xfree (heap->obj);
	H5MM_xfree (heap);
    }
    FUNC_LEAVE (ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5HG_load
 *
 * Purpose:	Loads a global heap collection from disk.
 *
 * Return:	Success:	Ptr to a global heap collection.
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *              Friday, March 27, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static H5HG_heap_t *
H5HG_load (H5F_t *f, const haddr_t *addr, const void __unused__ *udata1,
	   void __unused__ *udata2)
{
    H5HG_heap_t	*heap = NULL;
    H5HG_heap_t	*ret_value = NULL;
    uint8	*p = NULL;
    intn	i;
    
    FUNC_ENTER (H5HG_load, NULL);

    /* check arguments */
    assert (f);
    assert (addr && H5F_addr_defined (addr));
    assert (!udata1);
    assert (!udata2);

    /* Read the initial 4k page */
    heap = H5MM_xcalloc (1, sizeof(H5HG_heap_t));
    heap->addr = *addr;
    heap->chunk = H5MM_xmalloc (H5HG_MINSIZE);
    if (H5F_block_read (f, addr, (hsize_t)H5HG_MINSIZE,
			H5D_XFER_DFLT, heap->chunk)<0) {
	HGOTO_ERROR (H5E_HEAP, H5E_READERROR, NULL,
		     "unable to read global heap collection");
    }

    /* Magic number */
    if (HDmemcmp (heap->chunk, H5HG_MAGIC, H5HG_SIZEOF_MAGIC)) {
	HGOTO_ERROR (H5E_HEAP, H5E_CANTLOAD, NULL,
		     "bad global heap collection signature");
    }
    p = heap->chunk + H5HG_SIZEOF_MAGIC;

    /* Version */
    if (H5HG_VERSION!=*p++) {
	HGOTO_ERROR (H5E_HEAP, H5E_CANTLOAD, NULL,
		     "wrong version number in global heap");
    }

    /* Reserved */
    p += 3;

    /* Size */
    H5F_decode_length (f, p, heap->size);
    assert (heap->size>=H5HG_MINSIZE);

    /*
     * If we didn't read enough in the first try, then read the rest of the
     * collection now.
     */
    if (heap->size > H5HG_MINSIZE) {
	haddr_t next_addr = *addr;
	H5F_addr_inc (&next_addr, (hsize_t)H5HG_MINSIZE);
	heap->chunk = H5MM_xrealloc (heap->chunk, heap->size);
	if (H5F_block_read (f, &next_addr, (hsize_t)(heap->size-H5HG_MINSIZE),
			    H5D_XFER_DFLT, heap->chunk+H5HG_MINSIZE)<0) {
	    HGOTO_ERROR (H5E_HEAP, H5E_READERROR, NULL,
			 "unable to read global heap collection");
	}
    }

    /* Decode each object */
    p = heap->chunk + H5HG_SIZEOF_HDR (f);
    heap->nalloc = H5HG_NOBJS (f, heap->size);
    heap->obj = H5MM_xcalloc (heap->nalloc, sizeof(H5HG_obj_t));
    while (p<heap->chunk+heap->size) {
	if (p+H5HG_SIZEOF_OBJHDR(f)>heap->chunk+heap->size) {
	    /*
	     * The last bit of space is too tiny for an object header, so we
	     * assume that it's free space.
	     */
	    assert (NULL==heap->obj[0].begin);
	    heap->obj[0].size = (heap->chunk+heap->size) - p;
	    heap->obj[0].begin = p;
	    p += heap->obj[0].size;
	} else {
	    intn idx;
	    uint8 *begin = p;
	    UINT16DECODE (p, idx);
	    assert (idx<heap->nalloc);
	    assert (NULL==heap->obj[idx].begin);
	    UINT16DECODE (p, heap->obj[idx].nrefs);
	    H5F_decode_length (f, p, heap->obj[idx].size);
	    heap->obj[idx].begin = begin;
	    p = begin + heap->obj[idx].size;
	}
    }
    assert (p==heap->chunk+heap->size);

    /*
     * Add the new heap to the CWFS list, removing some other entry if
     * necessary to make room. We remove the right-most entry that has less
     * free space than this heap.
     */
    if (heap->obj[0].size>0) {
	if (!f->shared->cwfs) {
	    f->shared->cwfs = H5MM_xmalloc (H5HG_NCWFS*sizeof(H5HG_heap_t*));
	    f->shared->ncwfs = 1;
	    f->shared->cwfs[0] = heap;
	} else if (H5HG_NCWFS==f->shared->ncwfs) {
	    for (i=H5HG_NCWFS-1; i>=0; --i) {
		if (f->shared->cwfs[i]->obj[0].size < heap->obj[0].size) {
		    HDmemcpy (f->shared->cwfs+1, f->shared->cwfs,
			      i * sizeof(H5HG_heap_t*));
		    f->shared->cwfs[0] = heap;
		    break;
		}
	    }
	} else {
	    HDmemcpy (f->shared->cwfs+1, f->shared->cwfs,
		      f->shared->ncwfs*sizeof(H5HG_heap_t*));
	    f->shared->ncwfs += 1;
	    f->shared->cwfs[0] = heap;
	}
    }
    
    ret_value = heap;
    
 done:
    if (!ret_value && heap) {
	H5MM_xfree (heap->chunk);
	H5MM_xfree (heap->obj);
	H5MM_xfree (heap);
    }
    FUNC_LEAVE (ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5HG_flush
 *
 * Purpose:	Flushes a global heap collection from memory to disk if it's
 *		dirty.  Optionally deletes teh heap from memory.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Friday, March 27, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HG_flush (H5F_t *f, hbool_t destroy, const haddr_t *addr, H5HG_heap_t *heap)
{
    int		i;
    
    FUNC_ENTER (H5HG_flush, FAIL);

    /* Check arguments */
    assert (f);
    assert (addr && H5F_addr_defined (addr));
    assert (H5F_addr_eq (addr, &(heap->addr)));
    assert (heap);

    if (heap->dirty) {
	if (H5F_block_write (f, addr, (hsize_t)(heap->size),
			     H5D_XFER_DFLT, heap->chunk)<0) {
	    HRETURN_ERROR (H5E_HEAP, H5E_WRITEERROR, FAIL,
			   "unable to write global heap collection to file");
	}
	heap->dirty = 0;
    }

    if (destroy) {
	for (i=0; i<f->shared->ncwfs; i++) {
	    if (f->shared->cwfs[i]==heap) {
		f->shared->ncwfs -= 1;
		HDmemmove (f->shared->cwfs+i, f->shared->cwfs+i+1,
			   (f->shared->ncwfs-i) * sizeof(H5HG_heap_t*));
		break;
	    }
	}
	heap->chunk = H5MM_xfree (heap->chunk);
	heap->obj = H5MM_xfree (heap->obj);
	H5MM_xfree (heap);
    }

    FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5HG_alloc
 *
 * Purpose:	Given a heap with enough free space, this function will split
 *		the free space to make a new empty heap object and initialize
 *		the header.
 *
 * Return:	Success:	The heap object ID of the new object.
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Friday, March 27, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static intn
H5HG_alloc (H5F_t *f, H5HG_heap_t *heap, int cwfsno, size_t size)
{
    int		idx;
    uint8	*p = NULL;
    
    FUNC_ENTER (H5HG_alloc, FAIL);

    /* Check args */
    assert (heap);
    assert (heap->obj[0].size>=size);

    /*
     * Find an ID for the new object. ID zero is reserved for the free space
     * object.
     */
    for (idx=1; idx<heap->nalloc; idx++) {
	if (NULL==heap->obj[idx].begin) break;
    }
    assert (idx < heap->nalloc);

    /* Initialize the new object */
    heap->obj[idx].nrefs = 0;
    heap->obj[idx].size = size;
    heap->obj[idx].begin = heap->obj[0].begin;
    p = heap->obj[idx].begin;
    UINT16ENCODE (p, idx);
    UINT16ENCODE (p, 0); /*nrefs*/
    H5F_encode_length (f, p, size);

    /* Fix the free space object */
    if (size==heap->obj[0].size) {
	/*
	 * All free space has been exhausted from this collection. Remove the
	 * heap from the CWFS list.
	 */
	heap->obj[0].size = 0;
	heap->obj[0].begin = NULL;
	if (cwfsno>=0) {
	    f->shared->ncwfs -= 1;
	    HDmemmove (f->shared->cwfs+cwfsno, f->shared->cwfs+cwfsno+1,
		       (f->shared->ncwfs-cwfsno)*sizeof(H5HG_heap_t*));
	}
		
    } else if (heap->obj[0].size-size >= H5HG_SIZEOF_OBJHDR (f)) {
	/*
	 * Some free space remains and it's larger than a heap object header,
	 * so write the new free heap object header to the heap.
	 */
	heap->obj[0].size -= size;
	heap->obj[0].begin += size;
	p = heap->obj[0].begin;
	UINT16ENCODE (p, 0);	/*id*/
	UINT16ENCODE (p, 0);	/*nrefs*/
	H5F_encode_length (f, p, heap->obj[0].size);
		
    } else {
	/*
	 * Some free space remains but it's smaller than a heap object header,
	 * so we don't write the header.
	 */
	heap->obj[0].size -= size;
	heap->obj[0].begin += size;
    }

    heap->dirty = 1;
    FUNC_LEAVE (idx);
}


/*-------------------------------------------------------------------------
 * Function:	H5HG_insert
 *
 * Purpose:	A new object is inserted into the global heap.  It will be
 *		placed in the first collection on the CWFS list which has
 *		enough free space and that collection will be advanced one
 *		position in the list.  If no collection on the CWFS list has
 *		enough space then  a new collection will be created.
 *
 *		It is legal to push a zero-byte object onto the heap to get
 *		the reference count features of heap objects.
 *
 * Return:	Success:	SUCCEED, and a heap object handle returned
 *				through the HOBJ pointer.
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Friday, March 27, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HG_insert (H5F_t *f, size_t size, void *obj, H5HG_t *hobj/*out*/)
{
    size_t	need;		/*total space needed for object		*/
    intn	cwfsno, idx;
    H5HG_heap_t	*heap = NULL;
    
    FUNC_ENTER (H5HG_insert, FAIL);

    /* Check args */
    assert (f);
    assert (0==size || obj);
    assert (hobj);

    if (0==(f->intent & H5F_ACC_RDWR)) {
	HRETURN_ERROR (H5E_HEAP, H5E_WRITEERROR, FAIL,
		       "no write intent on file");
    }

    /* Find a large enough collection on the CWFS list */
    need = size + H5HG_SIZEOF_OBJHDR (f);
    for (cwfsno=0; cwfsno<f->shared->ncwfs; cwfsno++) {
	if (f->shared->cwfs[cwfsno]->obj[0].size>=need) {
	    /*
	     * Found. Move the collection forward in the CWFS list.
	     */
	    heap = f->shared->cwfs[cwfsno];
	    if (cwfsno>0) {
		H5HG_heap_t *tmp = f->shared->cwfs[cwfsno];
		f->shared->cwfs[cwfsno] = f->shared->cwfs[cwfsno-1];
		f->shared->cwfs[cwfsno-1] = tmp;
		--cwfsno;
	    }
	    break;
	}
    }

    /*
     * If we didn't find any collection with enough free space then allocate a
     * new collection large enough for the message plus the collection header.
     */
    if (cwfsno>=f->shared->ncwfs) {
	if (NULL==(heap=H5HG_create (f, need+H5HG_SIZEOF_HDR (f)))) {
	    HRETURN_ERROR (H5E_HEAP, H5E_CANTINIT, FAIL,
			   "unable to allocate a global heap collection");
	}
	assert (f->shared->ncwfs>0);
	assert (f->shared->cwfs[0]==heap);
	assert (f->shared->cwfs[0]->obj[0].size >= need+H5HG_SIZEOF_HDR(f));
	cwfsno = 0;
    }
    
    /* Split the free space to make room for the new object */
    idx = H5HG_alloc (f, heap, cwfsno, need);
    assert (idx>0);
    
    /* Copy data into the heap */
    HDmemcpy (heap->obj[idx].begin+H5HG_SIZEOF_OBJHDR(f), obj, size);
    heap->dirty = TRUE;

    /* Return value */
    hobj->addr = heap->addr;
    hobj->idx = idx;
    FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5HG_peek
 *
 * Purpose:	Given an ID for a global heap object return a pointer to the
 *		beginning of that object.  This is intended for quick and
 *		dirty access to the object; otherwise use H5HG_read().
 *
 * Return:	Success:	Ptr directly into the H5AC layer for the
 *				specified object of the global heap.  The
 *				pointer is guaranteed to be valid only until
 *				some other hdf5 library function is called.
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *              Monday, March 30, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void *
H5HG_peek (H5F_t *f, H5HG_t *hobj)
{
    H5HG_heap_t	*heap = NULL;
    void	*retval = NULL;
    intn	i;
    
    FUNC_ENTER (H5HG_peek, NULL);

    /* Check args */
    assert (f);
    assert (hobj);

    /* Load the heap and return a pointer to the object */
    if (NULL==(heap=H5AC_find (f, H5AC_GHEAP, &(hobj->addr), NULL, NULL))) {
	HRETURN_ERROR (H5E_HEAP, H5E_CANTLOAD, NULL, "unable to load heap");
    }
    assert (hobj->idx>0 && hobj->idx<heap->nalloc);
    retval = heap->obj[hobj->idx].begin + H5HG_SIZEOF_OBJHDR (f);
    assert (retval);

    /*
     * Advance the heap in the CWFS list.  We might have done this already
     * with the H5AC_find(), but it won't hurt to do it twice.
     */
    if (heap->obj[0].begin) {
	for (i=0; i<f->shared->ncwfs; i++) {
	    if (f->shared->cwfs[i]==heap) {
		if (i) {
		    f->shared->cwfs[i] = f->shared->cwfs[i-1];
		    f->shared->cwfs[i-1] = heap;
		}
		break;
	    }
	}
    }
    
    FUNC_LEAVE (retval);
}


/*-------------------------------------------------------------------------
 * Function:	H5HG_read
 *
 * Purpose:	Reads the specified global heap object into the buffer OBJECT
 *		supplied by the caller.  If the caller doesn't supply a
 *		buffer then one will be allocated.  The buffer should be
 *		large enough to hold the result.
 *
 * Return:	Success:	The buffer containing the result.
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *              Monday, March 30, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void *
H5HG_read (H5F_t *f, H5HG_t *hobj, void *object/*out*/)
{
    H5HG_heap_t	*heap = NULL;
    intn	i;
    size_t	size;
    uint8	*p = NULL;
    
    FUNC_ENTER (H5HG_read, NULL);

    /* Check args */
    assert (f);
    assert (hobj);

    /* Load the heap */
    if (NULL==(heap=H5AC_find (f, H5AC_GHEAP, &(hobj->addr), NULL, NULL))) {
	HRETURN_ERROR (H5E_HEAP, H5E_CANTLOAD, NULL, "unable to load heap");
    }
    assert (hobj->idx>0 && hobj->idx<heap->nalloc);
    assert (heap->obj[hobj->idx].begin);
    size = heap->obj[hobj->idx].size - H5HG_SIZEOF_OBJHDR (f);
    p = heap->obj[hobj->idx].begin + H5HG_SIZEOF_OBJHDR (f);
    if (!object) object = H5MM_xmalloc (size);
    HDmemcpy (object, p, size);

    /*
     * Advance the heap in the CWFS list.  We might have done this already
     * with the H5AC_find(), but it won't hurt to do it twice.
     */
    if (heap->obj[0].begin) {
	for (i=0; i<f->shared->ncwfs; i++) {
	    if (f->shared->cwfs[i]==heap) {
		if (i) {
		    f->shared->cwfs[i] = f->shared->cwfs[i-1];
		    f->shared->cwfs[i-1] = heap;
		}
		break;
	    }
	}
    }

    FUNC_LEAVE (object);
}


/*-------------------------------------------------------------------------
 * Function:	H5HG_link
 *
 * Purpose:	Adjusts the link count for a global heap object by adding
 *		ADJUST to the current value.  This function will fail if the
 *		new link count would overflow.  Nothing special happens when
 *		the link count reaches zero; in order for a heap object to be
 *		removed one must call H5HG_remove().
 *
 * Return:	Success:	Number of links present after the adjustment.
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Monday, March 30, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
intn
H5HG_link (H5F_t *f, H5HG_t *hobj, intn adjust)
{
    H5HG_heap_t *heap = NULL;
    
    FUNC_ENTER (H5HG_link, FAIL);
    
    /* Check args */
    assert (f);
    assert (hobj);
    if (0==(f->intent & H5F_ACC_RDWR)) {
	HRETURN_ERROR (H5E_HEAP, H5E_WRITEERROR, FAIL,
		       "no write intent on file");
    }

    /* Load the heap */
    if (NULL==(heap=H5AC_find (f, H5AC_GHEAP, &(hobj->addr), NULL, NULL))) {
	HRETURN_ERROR (H5E_HEAP, H5E_CANTLOAD, FAIL, "unable to load heap");
    }
    assert (hobj->idx>0 && hobj->idx<heap->nalloc);
    assert (heap->obj[hobj->idx].begin);
    if (heap->obj[hobj->idx].nrefs+adjust<0) {
	HRETURN_ERROR (H5E_HEAP, H5E_BADRANGE, FAIL,
		       "new link count would be out of range");
    }
    if (heap->obj[hobj->idx].nrefs+adjust>H5HG_MAXLINK) {
	HRETURN_ERROR (H5E_HEAP, H5E_BADVALUE, FAIL,
		       "new link count would be out of range");
    }
    heap->obj[hobj->idx].nrefs += adjust;
    if (adjust) heap->dirty = TRUE;

    FUNC_LEAVE (heap->obj[hobj->idx].nrefs);
}


/*-------------------------------------------------------------------------
 * Function:	H5HG_remove
 *
 * Purpose:	Removes the specified object from the global heap.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Monday, March 30, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HG_remove (H5F_t *f, H5HG_t *hobj)
{
    uint8	*p=NULL, *obj_start=NULL;
    H5HG_heap_t	*heap = NULL;
    size_t	size;
    intn	i;
    
    FUNC_ENTER (H5HG_remove, FAIL);

    /* Check args */
    assert (f);
    assert (hobj);
    if (0==(f->intent & H5F_ACC_RDWR)) {
	HRETURN_ERROR (H5E_HEAP, H5E_WRITEERROR, FAIL,
		       "no write intent on file");
    }

    /* Load the heap */
    if (NULL==(heap=H5AC_find (f, H5AC_GHEAP, &(hobj->addr), NULL, NULL))) {
	HRETURN_ERROR (H5E_HEAP, H5E_CANTLOAD, FAIL, "unable to load heap");
    }
    assert (hobj->idx>0 && hobj->idx<heap->nalloc);
    assert (heap->obj[hobj->idx].begin);
    obj_start = heap->obj[hobj->idx].begin;
    size = heap->obj[hobj->idx].size;

    /* Move the new free space to the end of the heap */
    for (i=0; i<heap->nalloc; i++) {
	if (heap->obj[i].begin > heap->obj[hobj->idx].begin) {
	    heap->obj[i].begin -= size;
	}
    }
    if (NULL==heap->obj[0].begin) {
	heap->obj[0].begin = heap->chunk + (heap->size-size);
	heap->obj[0].size = size;
	heap->obj[0].nrefs = 0;
    } else {
	heap->obj[0].size += size;
    }
    HDmemmove (obj_start, obj_start+size,
	       heap->size-((obj_start+size)-heap->chunk));
    if (heap->obj[0].size>=H5HG_SIZEOF_OBJHDR (f)) {
	p = heap->obj[0].begin;
	UINT32ENCODE (p, 0); /*id*/
	UINT32ENCODE (p, 0); /*nrefs*/
	H5F_encode_length (f, p, size);
    }
    HDmemset (heap->obj+hobj->idx, 0, sizeof(H5HG_obj_t));
    heap->dirty = 1;

    if (heap->obj[0].size+H5HG_SIZEOF_HDR(f)==heap->size) {
	/*
	 * The collection is empty. Remove it from the CWFS list and return it
	 * to the file free list.
	 */
	heap->dirty = FALSE;
	H5MF_free (f, &(heap->addr), (hsize_t)(heap->size));
	H5AC_flush (f, H5AC_GHEAP, &(heap->addr), TRUE);
	heap = NULL;
    } else {
	/*
	 * If the heap is in the CWFS list then advance it one position.  The
	 * H5AC_find() might have done that too, but that's okay.  If the
	 * heap isn't on the CWFS list then add it to the end.
	 */
	for (i=0; i<f->shared->ncwfs; i++) {
	    if (f->shared->cwfs[i]==heap) {
		if (i) {
		    f->shared->cwfs[i] = f->shared->cwfs[i-1];
		    f->shared->cwfs[i-1] = heap;
		}
		break;
	    }
	}
	if (i>=f->shared->ncwfs) {
	    f->shared->ncwfs = MIN (f->shared->ncwfs+1, H5HG_NCWFS);
	    f->shared->cwfs[f->shared->ncwfs-1] = heap;
	}
    }
    
    FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5HG_debug
 *
 * Purpose:	Prints debugging information about a global heap collection.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Mar 27, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HG_debug(H5F_t *f, const haddr_t *addr, FILE *stream, intn indent,
	  intn fwidth)
{
    int			i, nused, maxobj;
    uintn		j, k;
    H5HG_heap_t		*h = NULL;
    char		buf[64];
    size_t		size;
    uint8		*p = NULL;

    FUNC_ENTER(H5HG_debug, FAIL);

    /* check arguments */
    assert(f);
    assert(addr && H5F_addr_defined (addr));
    assert(stream);
    assert(indent >= 0);
    assert(fwidth >= 0);

    if (NULL == (h = H5AC_find(f, H5AC_GHEAP, addr, NULL, NULL))) {
	HRETURN_ERROR(H5E_HEAP, H5E_CANTLOAD, FAIL,
		      "unable to load global heap collection");
    }
    fprintf(stream, "%*sGlobal Heap Collection...\n", indent, "");
    fprintf(stream, "%*s%-*s %d\n", indent, "", fwidth,
	    "Dirty:",
	    (int)(h->dirty));
    fprintf(stream, "%*s%-*s %lu\n", indent, "", fwidth,
	    "Total collection size in file:",
	    (unsigned long)(h->size));

    for (i=1, nused=0, maxobj=-1; i<h->nalloc; i++) {
	if (h->obj[i].begin) {
	    nused++;
	    if (i>maxobj) maxobj = i;
	}
    }
    fprintf (stream, "%*s%-*s %d/%d/", indent, "", fwidth,
	     "Objects defined/allocated/max:",
	     nused, h->nalloc);
    fprintf (stream, nused?"%d\n":"NA\n", maxobj);

    fprintf (stream, "%*s%-*s %lu\n", indent, "", fwidth,
	     "Free space:",
	     (unsigned long)(h->obj[0].size));

    for (i=1; i<h->nalloc; i++) {
	if (h->obj[i].begin) {
	    sprintf (buf, "Object %d", i);
	    fprintf (stream, "%*s%s\n", indent, "", buf);
	    fprintf (stream, "%*s%-*s %d\n", indent+3, "", MIN(fwidth-3, 0),
		     "Reference count:",
		     h->obj[i].nrefs);
	    fprintf (stream, "%*s%-*s %lu\n", indent+3, "", MIN(fwidth-3, 0),
		     "Size of object body:",
		     (unsigned long)(h->obj[i].size));
	    size = h->obj[i].size - H5HG_SIZEOF_OBJHDR (f);
	    p = h->obj[i].begin + H5HG_SIZEOF_OBJHDR (f);
	    for (j=0; j<size; j+=16) {
		fprintf (stream, "%*s%04d: ", indent+6, "", j);
		for (k=0; k<16; k++) {
		    if (8==k) fprintf (stream, " ");
		    if (j+k<size) {
			fprintf (stream, "%02x ", p[j+k]);
		    } else {
			fputs ("   ", stream);
		    }
		}
		for (k=0; k<16 && j+k<size; k++) {
		    if (8==k) fprintf (stream, " ");
		    fputc (p[j+k]>' ' && p[j+k]<='~' ? p[j+k] : '.', stream);
		}
		fprintf (stream, "\n");
	    }
	}
    }
    
    FUNC_LEAVE(SUCCEED);
}
