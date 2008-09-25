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
H5EA__dblock_alloc(H5EA_hdr_t *hdr, size_t nelmts))

    /* Local variables */
    H5EA_dblock_t *dblock = NULL;          /* Extensible array index block */

    /* Check arguments */
    HDassert(hdr);
    HDassert(nelmts > 0);

    /* Allocate memory for the data block */
    if(NULL == (dblock = H5FL_CALLOC(H5EA_dblock_t)))
	H5E_THROW(H5E_CANTALLOC, "memory allocation failed for extensible array data block")

    /* Set non-zero internal fields */
    dblock->nelmts = nelmts;

    /* Allocate buffer for elements in index block */
    if(NULL == (dblock->elmts = H5EA__hdr_alloc_elmts(hdr, nelmts)))
        H5E_THROW(H5E_CANTALLOC, "memory allocation failed for data block element buffer")

    /* Share common array information */
    dblock->hdr = hdr;
    if(H5EA__hdr_incr(hdr) < 0)
	H5E_THROW(H5E_CANTINC, "can't increment reference count on shared array header")

    /* Set the return value */
    ret_value = dblock;

CATCH
    if(!ret_value)
        if(dblock && H5EA__dblock_dest(hdr->f, dblock) < 0)
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
H5EA__dblock_create(H5EA_iblock_t *iblock, hid_t dxpl_id, size_t nelmts))

    /* Local variables */
    H5EA_dblock_t *dblock = NULL;      /* Extensible array data block */

#ifdef QAK
HDfprintf(stderr, "%s: Called\n", FUNC);
#endif /* QAK */

    /* Sanity check */
    HDassert(iblock);
    HDassert(nelmts > 0);

    /* Allocate the data block */
    if(NULL == (dblock = H5EA__dblock_alloc(iblock->hdr, nelmts)))
	H5E_THROW(H5E_CANTALLOC, "memory allocation failed for extensible array data block")

    /* Set size of data block on disk */
    dblock->size = H5EA_DBLOCK_SIZE(dblock);
#ifdef QAK
HDfprintf(stderr, "%s: dblock->size = %Zu\n", FUNC, dblock->size);
#endif /* QAK */

    /* Allocate space for the data block on disk */
    if(HADDR_UNDEF == (dblock->addr = H5MF_alloc(iblock->hdr->f, H5FD_MEM_EARRAY_DBLOCK, dxpl_id, (hsize_t)dblock->size)))
	H5E_THROW(H5E_CANTALLOC, "file allocation failed for extensible array data block")

    /* Clear any elements in index block to fill value */
    if((iblock->hdr->cparam.cls->fill)(dblock->elmts, (size_t)dblock->nelmts) < 0)
        H5E_THROW(H5E_CANTSET, "can't set extensible array data block elements to class's fill value")

    /* Cache the new extensible array data block */
    if(H5AC_set(iblock->hdr->f, dxpl_id, H5AC_EARRAY_DBLOCK, dblock->addr, dblock, H5AC__NO_FLAGS_SET) < 0)
	H5E_THROW(H5E_CANTINSERT, "can't add extensible array index block to cache")

    /* Set address of index block to return */
    ret_value = dblock->addr;

CATCH
    if(!H5F_addr_defined(ret_value))
        if(dblock) {
            /* Release data block's disk space */
            if(H5F_addr_defined(dblock->addr) && H5MF_xfree(iblock->hdr->f, H5FD_MEM_EARRAY_DBLOCK, dxpl_id, dblock->addr, (hsize_t)dblock->size) < 0)
                H5E_THROW(H5E_CANTFREE, "unable to release extensible array data block")

            /* Destroy data block */
            if(H5EA__dblock_dest(iblock->hdr->f, dblock) < 0)
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
H5EA__dblock_protect(H5EA_hdr_t *hdr, hid_t dxpl_id, haddr_t dblk_addr,
    size_t dblk_nelmts, H5AC_protect_t rw))

    /* Local variables */

#ifdef QAK
HDfprintf(stderr, "%s: Called\n", FUNC);
#endif /* QAK */

    /* Sanity check */
    HDassert(hdr);
    HDassert(H5F_addr_defined(dblk_addr));
    HDassert(dblk_nelmts);

    /* Protect the data block */
    if(NULL == (ret_value = (H5EA_dblock_t *)H5AC_protect(hdr->f, dxpl_id, H5AC_EARRAY_DBLOCK, dblk_addr, &dblk_nelmts, hdr, rw)))
        H5E_THROW(H5E_CANTPROTECT, "unable to protect extensible array data block, address = %llu", (unsigned long_long)dblk_addr)

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
        H5E_THROW(H5E_CANTUNPROTECT, "unable to unprotect extensible array data block, address = %llu", (unsigned long_long)dblock->addr)

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
H5EA__dblock_delete(H5EA_hdr_t *hdr, hid_t dxpl_id, haddr_t dblk_addr,
    size_t dblk_nelmts))

    /* Local variables */
    H5EA_dblock_t *dblock = NULL;       /* Pointer to data block */

#ifdef QAK
HDfprintf(stderr, "%s: Called\n", FUNC);
#endif /* QAK */

    /* Sanity check */
    HDassert(hdr);
    HDassert(H5F_addr_defined(dblk_addr));
    HDassert(dblk_nelmts > 0);

    /* Protect data block */
    if(NULL == (dblock = H5EA__dblock_protect(hdr, dxpl_id, dblk_addr, dblk_nelmts, H5AC_WRITE)))
        H5E_THROW(H5E_CANTPROTECT, "unable to protect extensible array data block, address = %llu", (unsigned long_long)dblk_addr)

    /* Release data block's disk space */
    if(H5MF_xfree(hdr->f, H5FD_MEM_EARRAY_DBLOCK, dxpl_id, dblk_addr, (hsize_t)dblock->size) < 0)
        H5E_THROW(H5E_CANTFREE, "unable to free extensible array data block")

CATCH
    /* Finished deleting data block in metadata cache */
    if(dblock && H5EA__dblock_unprotect(dblock, dxpl_id, H5AC__DIRTIED_FLAG | H5AC__DELETED_FLAG) < 0)
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
H5EA__dblock_dest(H5F_t UNUSED *f, H5EA_dblock_t *dblock))

    /* Sanity check */
    HDassert(dblock);
    HDassert(dblock->hdr);

    /* Check if we've got elements in the index block */
    if(dblock->nelmts > 0) {
        /* Free buffer for data block elements */
        HDassert(dblock->elmts);
        if(H5EA__hdr_free_elmts(dblock->hdr, dblock->nelmts, dblock->elmts) < 0)
            H5E_THROW(H5E_CANTFREE, "unable to free extensible array data block element buffer")
        dblock->elmts = NULL;
    } /* end if */

    /* Decrement reference count on shared info */
    if(H5EA__hdr_decr(dblock->hdr) < 0)
        H5E_THROW(H5E_CANTDEC, "can't decrement reference count on shared array header")

    /* Free the data block itself */
    (void)H5FL_FREE(H5EA_dblock_t, dblock);

CATCH

END_FUNC(PKG)   /* end H5EA__dblock_dest() */

