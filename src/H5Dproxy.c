/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*-------------------------------------------------------------------------
 *
 * Created:		H5Dproxy.c
 *			May 19 2009
 *			Quincey Koziol <koziol@hdfgroup.org>
 *
 * Purpose:		Implement dataset's metadata cache proxy cache methods.
 *
 * Note:		Chunk proxies exist only to make integrating the chunk
 *                      cache with the metadata cache's flush dependencies
 *                      easier and less coupled than directly tying them
 *                      together.
 *
 *                      Chunk proxies never exist on disk (hence their lack of
 *                      a 'load' callback) and their 'flush' callback just
 *                      triggers a flush of the chunk it's a a proxy for.
 *
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/

#define H5D_PACKAGE		/*suppress error about including H5Dpkg  */


/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Dpkg.h"		/* Dataset functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5FLprivate.h"	/* Free Lists                           */
#include "H5MFprivate.h"	/* File memory management		*/


/****************/
/* Local Macros */
/****************/


/******************/
/* Local Typedefs */
/******************/


/********************/
/* Package Typedefs */
/********************/


/********************/
/* Local Prototypes */
/********************/

/* Local routines */
static herr_t H5D__chunk_proxy_destroy(H5D_chunk_proxy_t *proxy);

/* Metadata cache (H5AC) callbacks */
static H5D_chunk_proxy_t *H5D__cache_proxy_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, const void *udata);
static herr_t H5D__cache_proxy_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr, H5D_chunk_proxy_t *proxy, unsigned UNUSED * flags_ptr);
static herr_t H5D__cache_proxy_dest(H5F_t *f, H5D_chunk_proxy_t *proxy);
static herr_t H5D__cache_proxy_clear(H5F_t *f, H5D_chunk_proxy_t *proxy, hbool_t destroy);
static herr_t H5D__cache_proxy_size(const H5F_t *f, const H5D_chunk_proxy_t *proxy, size_t *size_ptr);


/*********************/
/* Package Variables */
/*********************/

/* H5D chunk proxy inherits cache-like properties from H5AC */
const H5AC_class_t H5AC_CHUNK_PROXY[1] = {{
    H5AC_CHUNK_PROXY_ID,
    (H5AC_load_func_t)H5D__cache_proxy_load,
    (H5AC_flush_func_t)H5D__cache_proxy_flush,
    (H5AC_dest_func_t)H5D__cache_proxy_dest,
    (H5AC_clear_func_t)H5D__cache_proxy_clear,
    (H5AC_notify_func_t)NULL,
    (H5AC_size_func_t)H5D__cache_proxy_size,
}};


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/

/* Declare a free list to manage H5D_chunk_proxy_t objects */
H5FL_DEFINE_STATIC(H5D_chunk_proxy_t);



/*-------------------------------------------------------------------------
 * Function:	H5D__cache_proxy_load
 *
 * Purpose:	Loads a chunk proxy from the disk.
 *
 * Note:	This routine should never be invoked
 *
 * Return:	Success:	Pointer to a new chunk proxy
 *		Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		May 21 2009
 *
 *-------------------------------------------------------------------------
 */
static H5D_chunk_proxy_t *
H5D__cache_proxy_load(H5F_t UNUSED *f, hid_t UNUSED dxpl_id, haddr_t UNUSED addr,
    const void UNUSED *udata)
{
    H5D_chunk_proxy_t *ret_value;               /* Return value */

    FUNC_ENTER_STATIC

    /* This routine should never be invoked! */
    HDassert(0 && "H5D__cache_proxy_load called!?!");
    HGOTO_ERROR(H5E_DATASET, H5E_CANTLOAD, NULL, "unable to load chunk proxy")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5D__cache_proxy_load() */


/*-------------------------------------------------------------------------
 * Function:	H5D__cache_proxy_flush
 *
 * Purpose:	Proxy for flushing a chunk in chunk cache under control
 *              of the metadata cache.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		May 19 2009
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__cache_proxy_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr,
    H5D_chunk_proxy_t *proxy, unsigned UNUSED * flags_ptr)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_STATIC
#ifdef QAK
HDfprintf(stderr, "%s: Flushing chunk proxy, addr = %a, destroy = %u\n", FUNC, addr, (unsigned)destroy);
#endif /* QAK */

    /* check arguments */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(proxy);

    if(proxy->cache_info.is_dirty) {
        H5D_dxpl_cache_t _dxpl_cache;       /* Data transfer property cache buffer */
        H5D_dxpl_cache_t *dxpl_cache = &_dxpl_cache;   /* Data transfer property cache */

        /* Fill the DXPL cache values for later use */
        if(H5D__get_dxpl_cache(dxpl_id, &dxpl_cache) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't fill dxpl cache")

        /* Flush the chunk for the proxy */
        /* (This must be safe from actually performing I/O when the chunk is
         *      clean - QAK, 5/21/2009)
         */
        if(H5D__chunk_flush_entry(proxy->dset, dxpl_id, dxpl_cache, proxy->ent, FALSE) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTFLUSH, FAIL, "can't flush chunk via proxy")

        /* Mark the chunk proxy as clean now */
	proxy->cache_info.is_dirty = FALSE;
    } /* end if */

    if(destroy)
        if(H5D__cache_proxy_dest(f, proxy) < 0)
	    HGOTO_ERROR(H5E_DATASET, H5E_CANTFREE, FAIL, "unable to destroy chunk proxy")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5D__cache_proxy_flush() */


/*-------------------------------------------------------------------------
 * Function:	H5D__cache_proxy_dest
 *
 * Purpose:	Destroys a chunk proxy in memory.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		May 19 2009
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__cache_proxy_dest(H5F_t UNUSED *f, H5D_chunk_proxy_t *proxy)
{
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_STATIC

    /*
     * Check arguments.
     */
    HDassert(proxy);

    /* Free the chunk proxy itself */
    if(H5D__chunk_proxy_destroy(proxy) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTFREE, FAIL, "unable to destroy chunk proxy")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__cache_proxy_dest() */


/*-------------------------------------------------------------------------
 * Function:	H5D__cache_proxy_clear
 *
 * Purpose:	Mark a chunk proxy in memory as non-dirty.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		May 19 2009
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__cache_proxy_clear(H5F_t *f, H5D_chunk_proxy_t *proxy, hbool_t destroy)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_STATIC

    /*
     * Check arguments.
     */
    HDassert(proxy);

    /* Reset the dirty flag.  */
    proxy->cache_info.is_dirty = FALSE;

    if(destroy)
        if(H5D__cache_proxy_dest(f, proxy) < 0)
	    HGOTO_ERROR(H5E_DATASET, H5E_CANTFREE, FAIL, "unable to destroy chunk proxy")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__cache_proxy_clear() */


/*-------------------------------------------------------------------------
 * Function:	H5D__cache_proxy_size
 *
 * Purpose:	Compute the size in bytes of a chunk proxy
 *		on disk, and return it in *size_ptr.  On failure,
 *		the value of *size_ptr is undefined.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		May 19 2009
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__cache_proxy_size(const H5F_t UNUSED *f, const H5D_chunk_proxy_t UNUSED *proxy,
    size_t *size_ptr)
{
    FUNC_ENTER_STATIC_NOERR

    /* check arguments */
    HDassert(f);
    HDassert(proxy);
    HDassert(size_ptr);

    /* Chunk proxies are represented as 1 byte in cache */
    /* (would be 0 bytes, but cache won't allow it currently -QAK, 5/19/09) */
    *size_ptr = 1;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5D__cache_proxy_size() */


/*-------------------------------------------------------------------------
 * Function:	H5D__chunk_proxy_create
 *
 * Purpose:	Create a proxy for the chunk and insert it into the metadata cache.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, May 19, 2009
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D__chunk_proxy_create(H5D_t *dset, hid_t dxpl_id, H5D_chunk_ud_t *udata,
    H5D_rdcc_ent_t *ent)
{
    H5D_chunk_proxy_t *proxy = NULL;    /* Chunk proxy */
    H5D_chk_idx_info_t idx_info;        /* Chunked index info */
    htri_t supported;                   /* Return value from "support" callback */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_PACKAGE

    HDassert(dset);
    HDassert(ent);
    HDassert(dset->shared->layout.storage.u.chunk.ops->support);

    /* Get a temp. address for chunk proxy */
    if(HADDR_UNDEF == (ent->proxy_addr = H5MF_alloc_tmp(dset->oloc.file, (hsize_t)1)))
	HGOTO_ERROR(H5E_DATASET, H5E_NOSPACE, FAIL, "file allocation failed for chunk proxy")
#ifdef QAK
HDfprintf(stderr, "%s: ent->proxy_addr = %a\n", FUNC, ent->proxy_addr);
#endif /* QAK */

    /* Create chunk proxy object & initialize fields to zero */
    if(NULL == (proxy = H5FL_CALLOC(H5D_chunk_proxy_t)))
        HGOTO_ERROR(H5E_DATASET, H5E_NOSPACE, FAIL, "can't allocate chunk proxy")
    ent->proxy = proxy;

    /* Point chunk proxy to chunk cache entry it's representing and dataset
     *  it's related to
     */
    proxy->dset = dset;
    proxy->ent = ent;

    /* Insert chunk proxy into metadata cache, pinned */
    if(H5AC_insert_entry(dset->oloc.file, dxpl_id, H5AC_CHUNK_PROXY, ent->proxy_addr, proxy, H5AC__PIN_ENTRY_FLAG) < 0)
	HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "can't add chunk proxy to cache")

    /* Compose chunked index info struct */
    idx_info.f = dset->oloc.file;
    idx_info.dxpl_id = dxpl_id;
    idx_info.pline = &(dset->shared->dcpl_cache.pline);
    idx_info.layout = &(dset->shared->layout.u.chunk);
    idx_info.storage = &(dset->shared->layout.storage.u.chunk);

    /* Create a flush dependency between the proxy (as the child) and the
     *  metadata object in the index (as the parent).
     */
    if((supported = (dset->shared->layout.storage.u.chunk.ops->support)(&idx_info, (H5D_chunk_common_ud_t *)udata, (H5AC_info_t *)proxy)) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTDEPEND, FAIL, "unable to create flush dependency for chunk proxy")
    proxy->supported = (hbool_t)supported;

done:
    if(ret_value < 0) {
        if(proxy)
            proxy = H5FL_FREE(H5D_chunk_proxy_t, proxy);
        ent->proxy_addr = HADDR_UNDEF;
        ent->proxy = NULL;
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__chunk_proxy_create() */


/*-------------------------------------------------------------------------
 * Function:	H5D__chunk_proxy_remove
 *
 * Purpose:	Remove a proxy for the chunk from the metadata cache.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, May 19, 2009
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D__chunk_proxy_remove(const H5D_t *dset, hid_t dxpl_id, H5D_rdcc_ent_t *ent)
{
    H5D_chk_idx_info_t idx_info;        /* Chunked index info */
    H5D_chunk_ud_t udata;               /* User-data for chunk */
    H5D_chunk_proxy_t *proxy = NULL;    /* Chunk proxy */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_PACKAGE

    HDassert(dset);
    HDassert(ent);
    HDassert(dset->shared->layout.storage.u.chunk.ops->unsupport);
#ifdef QAK
HDfprintf(stderr, "%s: ent->proxy_addr = %a\n", FUNC, ent->proxy_addr);
#endif /* QAK */

    /* Protect the chunk proxy */
    if(NULL == (proxy = (H5D_chunk_proxy_t *)H5AC_protect(dset->oloc.file, dxpl_id, H5AC_CHUNK_PROXY, ent->proxy_addr, NULL, H5AC_WRITE)))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTPROTECT, FAIL, "unable to protect chunk proxy");

    /* Compose chunked index info struct */
    idx_info.f = dset->oloc.file;
    idx_info.dxpl_id = dxpl_id;
    idx_info.pline = &(dset->shared->dcpl_cache.pline);
    idx_info.layout = &(dset->shared->layout.u.chunk);
    idx_info.storage = &(dset->shared->layout.storage.u.chunk);

    /* Compose user-data for chunk */
    udata.common.layout = &(dset->shared->layout.u.chunk);
    udata.common.storage = &(dset->shared->layout.storage.u.chunk);
    udata.common.offset = ent->offset;
    udata.common.rdcc = &(dset->shared->cache.chunk);
    /* Non-"common" data is not actually needed, except to provide a space to
     * store this information if it is to be filled in as a side-effect of the
     * unsupport callback (i.e. in H5D_btree_found) */

    /* Remove flush dependency between the proxy (as the child) and the
     *  metadata object in the index (as the parent).
     */
    if(proxy->supported) {
        if((dset->shared->layout.storage.u.chunk.ops->unsupport)(&idx_info, &udata, (H5AC_info_t *)proxy) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTDEPEND, FAIL, "unable to remove flush dependency for chunk proxy")
        proxy->supported = FALSE;
    } /* end if */

    /* Unpin & delete chunk proxy from metadata cache, taking ownership of it */
    if(H5AC_unprotect(dset->oloc.file, dxpl_id, H5AC_CHUNK_PROXY, ent->proxy_addr, proxy, (H5AC__UNPIN_ENTRY_FLAG | H5AC__DELETED_FLAG | H5AC__TAKE_OWNERSHIP_FLAG)) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTUNPROTECT, FAIL, "unable to release chunk proxy");

    /* Reset information in chunk cache entry */
    ent->proxy_addr = HADDR_UNDEF;
    ent->proxy = NULL;

    /* Release the chunk proxy object */
    if(H5D__chunk_proxy_destroy(proxy) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTFREE, FAIL, "unable to destroy chunk proxy")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__chunk_proxy_remove() */


/*-------------------------------------------------------------------------
 * Function:	H5D__chunk_proxy_mark
 *
 * Purpose:	Mark a proxy for the chunk in the metadata cache as clean or
 *              dirty.
 *
 * Note:	Currently, this code does not mark a chunk proxy as clean in
 *              the metadata cache.  Doing so would require that this routine
 *              be invoked collectively when operating in parallel I/O mode
 *              and it's possible that this routine can be invoked during
 *              indepedent raw data I/O.
 *
 *              So, the chunk proxy's dirty state in the metadata cache may 
 *              be out of sync with the chunk itself, but only in the direction
 *              of being dirty when the chunk itself is clean.  We'll call
 *              call the 'flush' callback for the chunk proxies more often
 *              than necessary, but as long as the chunk entry flushing
 *              routine doesn't actually flush the chunk when it's clean,
 *              there won't be too much overhead. - QAK, 5/21/2009
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, May 19, 2009
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D__chunk_proxy_mark(H5D_rdcc_ent_t *ent, hbool_t dirty)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_PACKAGE

    HDassert(ent);
#ifdef QAK
HDfprintf(stderr, "%s: ent->proxy_addr = %a, dirty = %t\n", FUNC, ent->proxy_addr, dirty);
#endif /* QAK */

    /* Check whether to mark the proxy as dirty */
    if(dirty) {
        if(H5AC_mark_entry_dirty(ent->proxy) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTMARKDIRTY, FAIL, "can't mark chunk proxy entry in metadata cache as dirty")
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__chunk_proxy_mark() */


/*-------------------------------------------------------------------------
 * Function:	H5D__chunk_proxy_destroy
 *
 * Purpose:	Destroy a chunk proxy object
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Thursday, May 21, 2009
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__chunk_proxy_destroy(H5D_chunk_proxy_t *proxy)
{
    FUNC_ENTER_STATIC_NOERR

    HDassert(proxy);

    /* Free the chunk proxy object */
    proxy->dset = NULL;
    proxy->ent = NULL;
    proxy = H5FL_FREE(H5D_chunk_proxy_t, proxy);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5D__chunk_proxy_destroy() */


/*-------------------------------------------------------------------------
 * Function:    H5D__chunk_proxy_create_flush_dep
 *
 * Purpose:     Creates a flush dependency between the specified chunk
 *              (child) and parent, if not already present.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Neil Fortner
 *              21 Sept 2010
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D__chunk_proxy_create_flush_dep(H5D_rdcc_ent_t *ent, void *parent)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_PACKAGE

    HDassert(ent);
    HDassert(parent);

    /* If the proxy already has a parent, do nothing. */
    if(!(ent->proxy->supported)) {
        /* Create the flush dependency */
        if(H5AC_create_flush_dependency(parent, ent->proxy) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTDEPEND, FAIL, "unable to create flush dependency")
        ent->proxy->supported = TRUE;
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__chunk_proxy_create_flush_dep() */


/*-------------------------------------------------------------------------
 * Function:    H5D__chunk_proxy_update_flush_dep
 *
 * Purpose:     Updates the flush dependency of the specified chunk from
 *              old_parent to new_parent, if the dependency exists.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Neil Fortner
 *              7 Sept 2010
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D__chunk_proxy_update_flush_dep(H5D_rdcc_ent_t *ent, void *old_parent,
    void *new_parent)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_PACKAGE

    HDassert(ent);
    HDassert(old_parent);
    HDassert(new_parent);

    /* It is guaranteed that the proxy has a parent, because the dependency
     * should always be present if the parent object exists in the index, and
     * this should only be called when updating the parent object */
    HDassert(ent->proxy->supported);

    /* Update the flush dependencies */
    if(H5AC_destroy_flush_dependency(old_parent, ent->proxy) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTUNDEPEND, FAIL, "unable to destroy flush dependency")
    if(H5AC_create_flush_dependency(new_parent, ent->proxy) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTDEPEND, FAIL, "unable to create flush dependency")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__chunk_proxy_update_flush_dep() */

