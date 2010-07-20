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
 * Created:		H5EAdblock.c
 *			Sep 11 2008
 *			Quincey Koziol <koziol@hdfgroup.org>
 *
 * Purpose:		Data block routines for extensible arrays.
 *
 *-------------------------------------------------------------------------
 */

/**********************/
/* Module Declaration */
/**********************/

#define H5EA_MODULE


/***********************/
/* Other Packages Used */
/***********************/


/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5EApkg.h"		/* Extensible Arrays			*/
#include "H5FLprivate.h"	/* Free Lists                           */
#include "H5MFprivate.h"	/* File memory management		*/
#include "H5Vprivate.h"		/* Vectors and arrays 			*/


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


/*********************/
/* Package Variables */
/*********************/


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/

/* Declare a free list to manage the H5EA_dblock_t struct */
H5FL_DEFINE_STATIC(H5EA_dblock_t);



/*-------------------------------------------------------------------------
 * Function:	H5EA__dblock_alloc
 *
 * Purpose:	Allocate extensible array data block
 *
 * Return:	Non-NULL pointer to data block on success/NULL on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Sep 11 2008
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(PKG, ERR,
H5EA_dblock_t *, NULL, NULL,
H5EA__dblock_alloc(H5EA_hdr_t *hdr, void *parent, size_t nelmts))

    /* Local variables */
    H5EA_dblock_t *dblock = NULL;          /* Extensible array data block */

    /* Check arguments */
    HDassert(hdr);
    HDassert(parent);
    HDassert(nelmts > 0);

    /* Allocate memory for the data block */
    if(NULL == (dblock = H5FL_CALLOC(H5EA_dblock_t)))
	H5E_THROW(H5E_CANTALLOC, "memory allocation failed for extensible array data block")

    /* Share common array information */
    if(H5EA__hdr_incr(hdr) < 0)
	H5E_THROW(H5E_CANTINC, "can't increment reference count on shared array header")
    dblock->hdr = hdr;

    /* Set non-zero internal fields */
    dblock->parent = parent;
    dblock->nelmts = nelmts;

    /* Check if the data block is not going to be paged */
    if(nelmts > hdr->dblk_page_nelmts) {
        /* Set the # of pages in the direct block */
        dblock->npages = nelmts / hdr->dblk_page_nelmts;
        HDassert(nelmts == (dblock->npages * hdr->dblk_page_nelmts));
    } /* end if */
    else {
        /* Allocate buffer for elements in data block */
        if(NULL == (dblock->elmts = H5EA__hdr_alloc_elmts(hdr, nelmts)))
            H5E_THROW(H5E_CANTALLOC, "memory allocation failed for data block element buffer")
    } /* end else */

    /* Set the return value */
    ret_value = dblock;

CATCH
    if(!ret_value)
        if(dblock && H5EA__dblock_dest(dblock) < 0)
            H5E_THROW(H5E_CANTFREE, "unable to destroy extensible array data block")

END_FUNC(PKG)   /* end H5EA__dblock_alloc() */


/*-------------------------------------------------------------------------
 * Function:	H5EA__dblock_create
 *
 * Purpose:	Creates a new extensible array data block in the file
 *
 * Return:	Valid file address on success/HADDR_UNDEF on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Sep  9 2008
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(PKG, ERR,
haddr_t, HADDR_UNDEF, HADDR_UNDEF,
H5EA__dblock_create(H5EA_hdr_t *hdr, hid_t dxpl_id, void *parent,
    hbool_t *stats_changed, hsize_t dblk_off, size_t nelmts))

    /* Local variables */
    H5EA_dblock_t *dblock = NULL;       /* Extensible array data block */
    haddr_t dblock_addr;                /* Extensible array data block address */

#ifdef QAK
HDfprintf(stderr, "%s: Called, hdr->dblk_page_nelmts = %Zu, nelmts = %Zu\n", FUNC, hdr->dblk_page_nelmts, nelmts);
#endif /* QAK */

    /* Sanity check */
    HDassert(hdr);
    HDassert(stats_changed);
    HDassert(nelmts > 0);

    /* Allocate the data block */
    if(NULL == (dblock = H5EA__dblock_alloc(hdr, parent, nelmts)))
	H5E_THROW(H5E_CANTALLOC, "memory allocation failed for extensible array data block")

    /* Set size of data block on disk */
    dblock->size = H5EA_DBLOCK_SIZE(dblock);
#ifdef QAK
HDfprintf(stderr, "%s: dblock->size = %Zu\n", FUNC, dblock->size);
#endif /* QAK */

    /* Set offset of block in array's address space */
    dblock->block_off = dblk_off;
#ifdef QAK
HDfprintf(stderr, "%s: dblock->block_off = %Hu\n", FUNC, dblock->block_off);
#endif /* QAK */

    /* Allocate space for the data block on disk */
    if(HADDR_UNDEF == (dblock_addr = H5MF_alloc(hdr->f, H5FD_MEM_EARRAY_DBLOCK, dxpl_id, (hsize_t)dblock->size)))
	H5E_THROW(H5E_CANTALLOC, "file allocation failed for extensible array data block")
    dblock->addr = dblock_addr;

    /* Don't initialize elements if paged */
    if(!dblock->npages)
        /* Clear any elements in data block to fill value */
        if((hdr->cparam.cls->fill)(dblock->elmts, (size_t)dblock->nelmts) < 0)
            H5E_THROW(H5E_CANTSET, "can't set extensible array data block elements to class's fill value")

    /* Cache the new extensible array data block */
    if(H5AC_insert_entry(hdr->f, dxpl_id, H5AC_EARRAY_DBLOCK, dblock_addr, dblock, H5AC__NO_FLAGS_SET) < 0)
	H5E_THROW(H5E_CANTINSERT, "can't add extensible array data block to cache")

    /* Update extensible array data block statistics */
    hdr->stats.stored.ndata_blks++;
    hdr->stats.stored.data_blk_size += dblock->size;

    /* Increment count of elements "realized" */
    hdr->stats.stored.nelmts += nelmts;

    /* Mark the statistics as changed */
    *stats_changed = TRUE;

    /* Set address of data block to return */
    ret_value = dblock_addr;

CATCH
    if(!H5F_addr_defined(ret_value))
        if(dblock) {
            /* Release data block's disk space */
            if(H5F_addr_defined(dblock->addr) && H5MF_xfree(hdr->f, H5FD_MEM_EARRAY_DBLOCK, dxpl_id, dblock->addr, (hsize_t)dblock->size) < 0)
                H5E_THROW(H5E_CANTFREE, "unable to release extensible array data block")

            /* Destroy data block */
            if(H5EA__dblock_dest(dblock) < 0)
                H5E_THROW(H5E_CANTFREE, "unable to destroy extensible array data block")
        } /* end if */

END_FUNC(PKG)   /* end H5EA__dblock_create() */


/*-------------------------------------------------------------------------
 * Function:	H5EA__dblock_sblk_idx
 *
 * Purpose:	Compute the index of the super block where the element is
 *              located.
 *
 * Return:	Super block index on success/Can't fail
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Sep 11 2008
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(PKG, NOERR,
unsigned, 0, -,
H5EA__dblock_sblk_idx(const H5EA_hdr_t *hdr, hsize_t idx))

    /* Local variables */
    unsigned sblk_idx;      /* Which superblock does this index fall in? */

    /* Sanity check */
    HDassert(hdr);
    HDassert(idx >= hdr->cparam.idx_blk_elmts);

#ifdef QAK
HDfprintf(stderr, "%s: Entering - idx = %Hu\n", FUNC, idx);
#endif /* QAK */
    /* Adjust index for elements in index block */
    idx -= hdr->cparam.idx_blk_elmts;
#ifdef QAK
HDfprintf(stderr, "%s: after adjusting for index block elements, idx = %Hu\n", FUNC, idx);
#endif /* QAK */

    /* Determine the superblock information for the index */
    H5_CHECK_OVERFLOW(idx, /*From:*/hsize_t, /*To:*/uint64_t);
#ifdef QAK
HDfprintf(stderr, "%s: hdr->cparam.data_blk_min_elmts = %u\n", FUNC, (unsigned)hdr->cparam.data_blk_min_elmts);
#endif /* QAK */
    sblk_idx = H5V_log2_gen((uint64_t)((idx / hdr->cparam.data_blk_min_elmts) + 1));
#ifdef QAK
HDfprintf(stderr, "%s: sblk_idx = %u\n", FUNC, sblk_idx);
HDfprintf(stderr, "%s: hdr->sblk_info[%u] = {%Hu, %Zu, %Hu, %Hu}\n", FUNC, sblk_idx, hdr->sblk_info[sblk_idx].ndblks, hdr->sblk_info[sblk_idx].dblk_nelmts, hdr->sblk_info[sblk_idx].start_idx, hdr->sblk_info[sblk_idx].start_dblk);
#endif /* QAK */

    /* Set return value */
    ret_value = sblk_idx;

END_FUNC(PKG)   /* end H5EA__dblock_sblk_idx() */


/*-------------------------------------------------------------------------
 * Function:	H5EA__dblock_protect
 *
 * Purpose:	Convenience wrapper around protecting extensible array data block
 *
 * Return:	Non-NULL pointer to data block on success/NULL on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Sep 18 2008
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(PKG, ERR,
H5EA_dblock_t *, NULL, NULL,
H5EA__dblock_protect(H5EA_hdr_t *hdr, hid_t dxpl_id, void *parent,
    haddr_t dblk_addr, size_t dblk_nelmts, H5AC_protect_t rw))

    /* Local variables */
    H5EA_dblock_cache_ud_t udata;      /* Information needed for loading data block */

#ifdef QAK
HDfprintf(stderr, "%s: Called\n", FUNC);
#endif /* QAK */

    /* Sanity check */
    HDassert(hdr);
    HDassert(H5F_addr_defined(dblk_addr));
    HDassert(dblk_nelmts);

    /* Set up user data */
    udata.hdr = hdr;
    udata.parent = parent;
    udata.nelmts = dblk_nelmts;

    /* Protect the data block */
    if(NULL == (ret_value = (H5EA_dblock_t *)H5AC_protect(hdr->f, dxpl_id, H5AC_EARRAY_DBLOCK, dblk_addr, &udata, rw)))
        H5E_THROW(H5E_CANTPROTECT, "unable to protect extensible array data block, address = %llu", (unsigned long long)dblk_addr)

CATCH

END_FUNC(PKG)   /* end H5EA__dblock_protect() */


/*-------------------------------------------------------------------------
 * Function:	H5EA__dblock_unprotect
 *
 * Purpose:	Convenience wrapper around unprotecting extensible array data block
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Sep 11 2008
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(PKG, ERR,
herr_t, SUCCEED, FAIL,
H5EA__dblock_unprotect(H5EA_dblock_t *dblock, hid_t dxpl_id, unsigned cache_flags))

    /* Local variables */

#ifdef QAK
HDfprintf(stderr, "%s: Called\n", FUNC);
#endif /* QAK */

    /* Sanity check */
    HDassert(dblock);

    /* Unprotect the data block */
    if(H5AC_unprotect(dblock->hdr->f, dxpl_id, H5AC_EARRAY_DBLOCK, dblock->addr, dblock, cache_flags) < 0)
        H5E_THROW(H5E_CANTUNPROTECT, "unable to unprotect extensible array data block, address = %llu", (unsigned long long)dblock->addr)

CATCH

END_FUNC(PKG)   /* end H5EA__dblock_unprotect() */


/*-------------------------------------------------------------------------
 * Function:	H5EA__dblock_delete
 *
 * Purpose:	Delete a data block
 *
 * Return:	SUCCEED/FAIL
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Sep 22 2008
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(PKG, ERR,
herr_t, SUCCEED, FAIL,
H5EA__dblock_delete(H5EA_hdr_t *hdr, hid_t dxpl_id, void *parent,
    haddr_t dblk_addr, size_t dblk_nelmts))

    /* Local variables */
    H5EA_dblock_t *dblock = NULL;       /* Pointer to data block */

#ifdef QAK
HDfprintf(stderr, "%s: Called\n", FUNC);
#endif /* QAK */

    /* Sanity check */
    HDassert(hdr);
    HDassert(parent);
    HDassert(H5F_addr_defined(dblk_addr));
    HDassert(dblk_nelmts > 0);

    /* Protect data block */
    if(NULL == (dblock = H5EA__dblock_protect(hdr, dxpl_id, parent, dblk_addr, dblk_nelmts, H5AC_WRITE)))
        H5E_THROW(H5E_CANTPROTECT, "unable to protect extensible array data block, address = %llu", (unsigned long long)dblk_addr)

    /* Check if this is a paged data block */
    if(dblk_nelmts > hdr->dblk_page_nelmts) {
        size_t npages = dblk_nelmts / hdr->dblk_page_nelmts;    /* Number of pages in data block */
        haddr_t dblk_page_addr;         /* Address of each data block page */
        size_t dblk_page_size;          /* Size of each data block page */
        size_t u;                       /* Local index variable */

        /* Set up initial state */
        dblk_page_addr = dblk_addr + H5EA_DBLOCK_PREFIX_SIZE(dblock);
        dblk_page_size = (hdr->dblk_page_nelmts * hdr->cparam.raw_elmt_size)
                + H5EA_SIZEOF_CHKSUM;

        /* Iterate over pages in data block */
        for(u = 0; u < npages; u++) {
#ifdef QAK
HDfprintf(stderr, "%s: Expunging data block page from cache\n", FUNC);
#endif /* QAK */
            /* Evict the data block page from the metadata cache */
            /* (OK to call if it doesn't exist in the cache) */
            if(H5AC_expunge_entry(hdr->f, dxpl_id, H5AC_EARRAY_DBLK_PAGE, dblk_page_addr, H5AC__NO_FLAGS_SET) < 0)
                H5E_THROW(H5E_CANTEXPUNGE, "unable to remove array data block page from metadata cache")
#ifdef QAK
HDfprintf(stderr, "%s: Done expunging data block page from cache\n", FUNC);
#endif /* QAK */

            /* Advance to next page address */
            dblk_page_addr += dblk_page_size;
        } /* end for */
    } /* end if */

CATCH
    /* Finished deleting data block in metadata cache */
    if(dblock && H5EA__dblock_unprotect(dblock, dxpl_id, H5AC__DIRTIED_FLAG | H5AC__DELETED_FLAG | H5AC__FREE_FILE_SPACE_FLAG) < 0)
        H5E_THROW(H5E_CANTUNPROTECT, "unable to release extensible array data block")

END_FUNC(PKG)   /* end H5EA__dblock_delete() */


/*-------------------------------------------------------------------------
 * Function:	H5EA__dblock_dest
 *
 * Purpose:	Destroys an extensible array data block in memory.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Sep 11 2008
 *
 *-------------------------------------------------------------------------
 */
/* ARGSUSED */
BEGIN_FUNC(PKG, ERR,
herr_t, SUCCEED, FAIL,
H5EA__dblock_dest(H5EA_dblock_t *dblock))

    /* Sanity check */
    HDassert(dblock);

    /* Check if shared header field has been initialized */
    if(dblock->hdr) {
        /* Check if we've got elements in the data block */
        if(dblock->elmts && !dblock->npages) {
            /* Free buffer for data block elements */
            HDassert(dblock->nelmts > 0);
            if(H5EA__hdr_free_elmts(dblock->hdr, dblock->nelmts, dblock->elmts) < 0)
                H5E_THROW(H5E_CANTFREE, "unable to free extensible array data block element buffer")
            dblock->elmts = NULL;
            dblock->nelmts = 0;
        } /* end if */

        /* Decrement reference count on shared info */
        if(H5EA__hdr_decr(dblock->hdr) < 0)
            H5E_THROW(H5E_CANTDEC, "can't decrement reference count on shared array header")
        dblock->hdr = NULL;
    } /* end if */

    /* Free the data block itself */
    dblock = H5FL_FREE(H5EA_dblock_t, dblock);

CATCH

END_FUNC(PKG)   /* end H5EA__dblock_dest() */

