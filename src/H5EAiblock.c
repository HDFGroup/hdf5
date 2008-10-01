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
 * Created:		H5EAiblock.c
 *			Sep  9 2008
 *			Quincey Koziol <koziol@hdfgroup.org>
 *
 * Purpose:		Index block routines for extensible arrays.
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

/* Declare a free list to manage the H5EA_iblock_t struct */
H5FL_DEFINE_STATIC(H5EA_iblock_t);

/* Declare a free list to manage the index block elements */
H5FL_BLK_DEFINE_STATIC(idx_blk_elmt_buf);

/* Declare a free list to manage the haddr_t sequence information */
H5FL_SEQ_DEFINE_STATIC(haddr_t);



/*-------------------------------------------------------------------------
 * Function:	H5EA__iblock_alloc
 *
 * Purpose:	Allocate extensible array index block
 *
 * Return:	Non-NULL pointer to index block on success/NULL on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Sep  9 2008
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(PKG, ERR,
H5EA_iblock_t *, NULL, NULL,
H5EA__iblock_alloc(H5EA_hdr_t *hdr))

    /* Local variables */
    H5EA_iblock_t *iblock = NULL;          /* Extensible array index block */

    /* Check arguments */
    HDassert(hdr);

    /* Allocate memory for the index block */
    if(NULL == (iblock = H5FL_CALLOC(H5EA_iblock_t)))
	H5E_THROW(H5E_CANTALLOC, "memory allocation failed for extensible array index block")

    /* Set non-zero internal fields */
    iblock->addr = HADDR_UNDEF;

    /* Compute information */
    iblock->nsblks = 2 * H5V_log2_of2((uint32_t)hdr->cparam.sup_blk_min_data_ptrs);
    iblock->ndblk_addrs = 2 * ((size_t)hdr->cparam.sup_blk_min_data_ptrs - 1);
    iblock->nsblk_addrs = hdr->nsblks - iblock->nsblks;
#ifdef QAK
HDfprintf(stderr, "%s: iblock->nsblks = %u\n", FUNC, iblock->nsblks);
HDfprintf(stderr, "%s: iblock->ndblk_addrs = %Zu\n", FUNC, iblock->ndblk_addrs);
HDfprintf(stderr, "%s: iblock->nsblk_addrs = %Zu\n", FUNC, iblock->nsblk_addrs);
#endif /* QAK */

    /* Allocate buffer for elements in index block */
    if(hdr->cparam.idx_blk_elmts > 0)
        if(NULL == (iblock->elmts = H5FL_BLK_MALLOC(idx_blk_elmt_buf, (size_t)(hdr->cparam.idx_blk_elmts * hdr->cparam.cls->nat_elmt_size))))
            H5E_THROW(H5E_CANTALLOC, "memory allocation failed for index block data element buffer")

    /* Allocate buffer for data block addresses in index block */
    if(iblock->ndblk_addrs > 0)
        if(NULL == (iblock->dblk_addrs = H5FL_SEQ_MALLOC(haddr_t, iblock->ndblk_addrs)))
            H5E_THROW(H5E_CANTALLOC, "memory allocation failed for index block data block addresses")

    /* Allocate buffer for super block addresses in index block */
    if(iblock->nsblk_addrs > 0)
        if(NULL == (iblock->sblk_addrs = H5FL_SEQ_MALLOC(haddr_t, iblock->nsblk_addrs)))
            H5E_THROW(H5E_CANTALLOC, "memory allocation failed for index block super block addresses")

    /* Share common array information */
    iblock->hdr = hdr;
    if(H5EA__hdr_incr(hdr) < 0)
	H5E_THROW(H5E_CANTINC, "can't increment reference count on shared array header")

    /* Set the return value */
    ret_value = iblock;

CATCH
    if(!ret_value)
        if(iblock && H5EA__iblock_dest(hdr->f, iblock) < 0)
            H5E_THROW(H5E_CANTFREE, "unable to destroy extensible array index block")

END_FUNC(PKG)   /* end H5EA__iblock_alloc() */


/*-------------------------------------------------------------------------
 * Function:	H5EA__iblock_create
 *
 * Purpose:	Creates a new extensible array index block in the file
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
H5EA__iblock_create(H5EA_hdr_t *hdr, hid_t dxpl_id))

    /* Local variables */
    H5EA_iblock_t *iblock = NULL;      /* Extensible array index block */

#ifdef QAK
HDfprintf(stderr, "%s: Called\n", FUNC);
#endif /* QAK */

    /* Sanity check */
    HDassert(hdr);

    /* Allocate the index block */
    if(NULL == (iblock = H5EA__iblock_alloc(hdr)))
	H5E_THROW(H5E_CANTALLOC, "memory allocation failed for extensible array index block")

    /* Set size of index block on disk */
    iblock->size = H5EA_IBLOCK_SIZE(iblock);
#ifdef QAK
HDfprintf(stderr, "%s: iblock->size = %Zu\n", FUNC, iblock->size);
#endif /* QAK */

    /* Allocate space for the index block on disk */
    if(HADDR_UNDEF == (iblock->addr = H5MF_alloc(hdr->f, H5FD_MEM_EARRAY_IBLOCK, dxpl_id, (hsize_t)iblock->size)))
	H5E_THROW(H5E_CANTALLOC, "file allocation failed for extensible array index block")

    /* Clear any elements in index block to fill value */
    if(hdr->cparam.idx_blk_elmts > 0) {
        /* Call the class's 'fill' callback */
        if((hdr->cparam.cls->fill)(iblock->elmts, (size_t)hdr->cparam.idx_blk_elmts) < 0)
            H5E_THROW(H5E_CANTSET, "can't set extensible array index block elements to class's fill value")
    } /* end if */

    /* Reset any data block addresses in the index block */
    if(iblock->ndblk_addrs > 0) {
        haddr_t tmp_addr = HADDR_UNDEF;         /* Address value to fill data block addresses with */

        /* Set all the data block addresses to "undefined" address value */
        H5V_array_fill(iblock->dblk_addrs, &tmp_addr, sizeof(haddr_t), iblock->ndblk_addrs);
    } /* end if */

    /* Reset any super block addresses in the index block */
    if(iblock->nsblk_addrs > 0) {
        haddr_t tmp_addr = HADDR_UNDEF;         /* Address value to fill super block addresses with */

        /* Set all the super block addresses to "undefined" address value */
        H5V_array_fill(iblock->sblk_addrs, &tmp_addr, sizeof(haddr_t), iblock->nsblk_addrs);
    } /* end if */

    /* Cache the new extensible array index block */
    if(H5AC_set(hdr->f, dxpl_id, H5AC_EARRAY_IBLOCK, iblock->addr, iblock, H5AC__NO_FLAGS_SET) < 0)
	H5E_THROW(H5E_CANTINSERT, "can't add extensible array index block to cache")

    /* Set address of index block to return */
    ret_value = iblock->addr;

CATCH
    if(!H5F_addr_defined(ret_value))
        if(iblock) {
            /* Release index block's disk space */
            if(H5F_addr_defined(iblock->addr) && H5MF_xfree(hdr->f, H5FD_MEM_EARRAY_IBLOCK, dxpl_id, iblock->addr, (hsize_t)iblock->size) < 0)
                H5E_THROW(H5E_CANTFREE, "unable to release extensible array index block")

            /* Destroy index block */
            if(H5EA__iblock_dest(hdr->f, iblock) < 0)
                H5E_THROW(H5E_CANTFREE, "unable to destroy extensible array index block")
        } /* end if */

END_FUNC(PKG)   /* end H5EA__iblock_create() */


/*-------------------------------------------------------------------------
 * Function:	H5EA__iblock_protect
 *
 * Purpose:	Convenience wrapper around protecting extensible array index block
 *
 * Return:	Non-NULL pointer to index block on success/NULL on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Sep  9 2008
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(PKG, ERR,
H5EA_iblock_t *, NULL, NULL,
H5EA__iblock_protect(H5EA_hdr_t *hdr, hid_t dxpl_id, H5AC_protect_t rw))

    /* Local variables */

#ifdef QAK
HDfprintf(stderr, "%s: Called\n", FUNC);
#endif /* QAK */

    /* Sanity check */
    HDassert(hdr);

    /* Protect the index block */
    if(NULL == (ret_value = (H5EA_iblock_t *)H5AC_protect(hdr->f, dxpl_id, H5AC_EARRAY_IBLOCK, hdr->idx_blk_addr, NULL, hdr, rw)))
        H5E_THROW(H5E_CANTPROTECT, "unable to protect extensible array index block, address = %llu", (unsigned long_long)hdr->idx_blk_addr)

CATCH

END_FUNC(PKG)   /* end H5EA__iblock_protect() */


/*-------------------------------------------------------------------------
 * Function:	H5EA__iblock_unprotect
 *
 * Purpose:	Convenience wrapper around unprotecting extensible array index block
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Sep  9 2008
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(PKG, ERR,
herr_t, SUCCEED, FAIL,
H5EA__iblock_unprotect(H5EA_iblock_t *iblock, hid_t dxpl_id, unsigned cache_flags))

    /* Local variables */

#ifdef QAK
HDfprintf(stderr, "%s: Called\n", FUNC);
#endif /* QAK */

    /* Sanity check */
    HDassert(iblock);

    /* Unprotect the index block */
    if(H5AC_unprotect(iblock->hdr->f, dxpl_id, H5AC_EARRAY_IBLOCK, iblock->addr, iblock, cache_flags) < 0)
        H5E_THROW(H5E_CANTUNPROTECT, "unable to unprotect extensible array index block, address = %llu", (unsigned long_long)iblock->addr)

CATCH

END_FUNC(PKG)   /* end H5EA__iblock_unprotect() */


/*-------------------------------------------------------------------------
 * Function:	H5EA__iblock_delete
 *
 * Purpose:	Delete index block
 *
 * Return:	SUCCEED/FAIL
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Sep  9 2008
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(PKG, ERR,
herr_t, SUCCEED, FAIL,
H5EA__iblock_delete(H5EA_hdr_t *hdr, hid_t dxpl_id))

    /* Local variables */
    H5EA_iblock_t *iblock = NULL;       /* Pointer to index block */

#ifdef QAK
HDfprintf(stderr, "%s: Called\n", FUNC);
#endif /* QAK */

    /* Sanity check */
    HDassert(hdr);
    HDassert(H5F_addr_defined(hdr->idx_blk_addr));

    /* Protect index block */
    if(NULL == (iblock = H5EA__iblock_protect(hdr, dxpl_id, H5AC_WRITE)))
        H5E_THROW(H5E_CANTPROTECT, "unable to protect extensible array index block, address = %llu", (unsigned long_long)hdr->idx_blk_addr)

    /* Check for index block having data block pointers */
    if(iblock->ndblk_addrs > 0) {
        unsigned sblk_idx;      /* Current super block index */
        unsigned dblk_idx;      /* Current data block index w/in super block */
        size_t u;               /* Local index variable */

        /* Iterate over data blocks */
        sblk_idx = dblk_idx = 0;
        for(u = 0; u < iblock->ndblk_addrs; u++) {
            /* Check for data block existing */
            if(H5F_addr_defined(iblock->dblk_addrs[u])) {
                /* Delete data block */
                if(H5EA__dblock_delete(hdr, dxpl_id, iblock->dblk_addrs[u], hdr->sblk_info[sblk_idx].dblk_nelmts) < 0)
                    H5E_THROW(H5E_CANTDELETE, "unable to delete extensible array data block")
                iblock->dblk_addrs[u] = HADDR_UNDEF;
            } /* end if */

            /* Advance to next data block w/in super block */
            dblk_idx++;

            /* Check for moving to next super block */
            if(dblk_idx >= hdr->sblk_info[sblk_idx].ndblks) {
                sblk_idx++;
                dblk_idx = 0;
            } /* end if */
        } /* end for */
    } /* end if */

    /* Check for index block having data block pointers (not yet) */
    if(iblock->nsblk_addrs > 0) {
        size_t u;               /* Local index variable */

        /* Iterate over super blocks */
        for(u = 0; u < iblock->nsblk_addrs; u++) {
            /* Check for data block existing */
            if(H5F_addr_defined(iblock->sblk_addrs[u])) {
                /* Delete super block */
                if(H5EA__sblock_delete(hdr, dxpl_id, iblock->sblk_addrs[u], (unsigned)(u + iblock->nsblks)) < 0)
                    H5E_THROW(H5E_CANTDELETE, "unable to delete extensible array super block")
                iblock->sblk_addrs[u] = HADDR_UNDEF;
HDfprintf(stderr, "%s: Deleting super blocks not tested yet!\n", FUNC);
HDassert(0 && "Deleting super blocks not tested!");
            } /* end if */
        } /* end for */
    } /* end if */

    /* Release index block's disk space */
    if(H5MF_xfree(hdr->f, H5FD_MEM_EARRAY_IBLOCK, dxpl_id, hdr->idx_blk_addr, (hsize_t)iblock->size) < 0)
        H5E_THROW(H5E_CANTFREE, "unable to free extensible array index block")

CATCH
    /* Finished deleting index block in metadata cache */
    if(iblock && H5EA__iblock_unprotect(iblock, dxpl_id, H5AC__DIRTIED_FLAG | H5AC__DELETED_FLAG) < 0)
        H5E_THROW(H5E_CANTUNPROTECT, "unable to release extensible array index block")

END_FUNC(PKG)   /* end H5EA__iblock_delete() */


/*-------------------------------------------------------------------------
 * Function:	H5EA__iblock_dest
 *
 * Purpose:	Destroys an extensible array index block in memory.
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
H5EA__iblock_dest(H5F_t *f, H5EA_iblock_t *iblock))

    /* Sanity check */
    HDassert(iblock);
    HDassert(iblock->rc == 0);

    /* Set the shared array header's file context for this operation */
    iblock->hdr->f = f;

    /* Check if we've got elements in the index block */
    if(iblock->hdr->cparam.idx_blk_elmts > 0) {
        /* Free buffer for index block elements */
        HDassert(iblock->elmts);
        (void)H5FL_BLK_FREE(idx_blk_elmt_buf, iblock->elmts);
    } /* end if */

    /* Check if we've got data block addresses in the index block */
    if(iblock->ndblk_addrs > 0) {
        /* Free buffer for index block data block addresses */
        HDassert(iblock->dblk_addrs);
        (void)H5FL_SEQ_FREE(haddr_t, iblock->dblk_addrs);
    } /* end if */

    /* Check if we've got super block addresses in the index block */
    if(iblock->nsblk_addrs > 0) {
        /* Free buffer for index block super block addresses */
        HDassert(iblock->sblk_addrs);
        (void)H5FL_SEQ_FREE(haddr_t, iblock->sblk_addrs);
    } /* end if */

    /* Decrement reference count on shared info */
    if(H5EA__hdr_decr(iblock->hdr) < 0)
        H5E_THROW(H5E_CANTDEC, "can't decrement reference count on shared array header")

    /* Free the index block itself */
    (void)H5FL_FREE(H5EA_iblock_t, iblock);

CATCH

END_FUNC(PKG)   /* end H5EA__iblock_dest() */

