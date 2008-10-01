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
 * Created:		H5EAsblock.c
 *			Sep 30 2008
 *			Quincey Koziol <koziol@hdfgroup.org>
 *
 * Purpose:		Super block routines for extensible arrays.
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
H5FL_DEFINE_STATIC(H5EA_sblock_t);

/* Declare a free list to manage the haddr_t sequence information */
H5FL_SEQ_DEFINE_STATIC(haddr_t);



/*-------------------------------------------------------------------------
 * Function:	H5EA__sblock_alloc
 *
 * Purpose:	Allocate extensible array super block
 *
 * Return:	Non-NULL pointer to super block on success/NULL on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Sep 30 2008
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(PKG, ERR,
H5EA_sblock_t *, NULL, NULL,
H5EA__sblock_alloc(H5EA_hdr_t *hdr, unsigned sblk_idx))

    /* Local variables */
    H5EA_sblock_t *sblock = NULL;          /* Extensible array super block */

    /* Check arguments */
    HDassert(hdr);

    /* Allocate memory for the index block */
    if(NULL == (sblock = H5FL_CALLOC(H5EA_sblock_t)))
	H5E_THROW(H5E_CANTALLOC, "memory allocation failed for extensible array super block")

    /* Set non-zero internal fields */
    sblock->addr = HADDR_UNDEF;

    /* Compute/cache information */
    sblock->idx = sblk_idx;
    sblock->ndblks = hdr->sblk_info[sblk_idx].ndblks;
    sblock->dblk_nelmts = hdr->sblk_info[sblk_idx].dblk_nelmts;
#ifdef QAK
HDfprintf(stderr, "%s: sblock->ndblks = %Zu\n", FUNC, sblock->ndblks);
#endif /* QAK */

    /* Allocate buffer for data block addresses in super block */
    if(sblock->ndblks > 0)
        if(NULL == (sblock->dblk_addrs = H5FL_SEQ_MALLOC(haddr_t, sblock->ndblks)))
            H5E_THROW(H5E_CANTALLOC, "memory allocation failed for super block data block addresses")

    /* Share common array information */
    sblock->hdr = hdr;
    if(H5EA__hdr_incr(hdr) < 0)
	H5E_THROW(H5E_CANTINC, "can't increment reference count on shared array header")

    /* Set the return value */
    ret_value = sblock;

CATCH
    if(!ret_value)
        if(sblock && H5EA__sblock_dest(hdr->f, sblock) < 0)
            H5E_THROW(H5E_CANTFREE, "unable to destroy extensible array super block")

END_FUNC(PKG)   /* end H5EA__sblock_alloc() */


/*-------------------------------------------------------------------------
 * Function:	H5EA__sblock_create
 *
 * Purpose:	Creates a new extensible array super block in the file
 *
 * Return:	Valid file address on success/HADDR_UNDEF on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Sep 30 2008
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(PKG, ERR,
haddr_t, HADDR_UNDEF, HADDR_UNDEF,
H5EA__sblock_create(H5EA_hdr_t *hdr, hid_t dxpl_id, unsigned sblk_idx))

    /* Local variables */
    H5EA_sblock_t *sblock = NULL;      /* Extensible array super block */

#ifdef QAK
HDfprintf(stderr, "%s: Called\n", FUNC);
#endif /* QAK */

    /* Sanity check */
    HDassert(hdr);

    /* Allocate the super block */
    if(NULL == (sblock = H5EA__sblock_alloc(hdr, sblk_idx)))
	H5E_THROW(H5E_CANTALLOC, "memory allocation failed for extensible array super block")

    /* Set size of super block on disk */
    sblock->size = H5EA_SBLOCK_SIZE(sblock);
#ifdef QAK
HDfprintf(stderr, "%s: sblock->size = %Zu\n", FUNC, sblock->size);
#endif /* QAK */

    /* Allocate space for the super block on disk */
    if(HADDR_UNDEF == (sblock->addr = H5MF_alloc(hdr->f, H5FD_MEM_EARRAY_SBLOCK, dxpl_id, (hsize_t)sblock->size)))
	H5E_THROW(H5E_CANTALLOC, "file allocation failed for extensible array super block")

    /* Reset data block addresses */
    if(sblock->ndblks > 0) {
        haddr_t tmp_addr = HADDR_UNDEF;         /* Address value to fill data block addresses with */

        /* Set all the data block addresses to "undefined" address value */
        H5V_array_fill(sblock->dblk_addrs, &tmp_addr, sizeof(haddr_t), sblock->ndblks);
    } /* end if */

    /* Cache the new extensible array super block */
    if(H5AC_set(hdr->f, dxpl_id, H5AC_EARRAY_SBLOCK, sblock->addr, sblock, H5AC__NO_FLAGS_SET) < 0)
	H5E_THROW(H5E_CANTINSERT, "can't add extensible array super block to cache")

    /* Set address of super block to return */
    ret_value = sblock->addr;

CATCH
    if(!H5F_addr_defined(ret_value))
        if(sblock) {
            /* Release super block's disk space */
            if(H5F_addr_defined(sblock->addr) && H5MF_xfree(hdr->f, H5FD_MEM_EARRAY_SBLOCK, dxpl_id, sblock->addr, (hsize_t)sblock->size) < 0)
                H5E_THROW(H5E_CANTFREE, "unable to release extensible array super block")

            /* Destroy super block */
            if(H5EA__sblock_dest(hdr->f, sblock) < 0)
                H5E_THROW(H5E_CANTFREE, "unable to destroy extensible array super block")
        } /* end if */

END_FUNC(PKG)   /* end H5EA__sblock_create() */


/*-------------------------------------------------------------------------
 * Function:	H5EA__sblock_protect
 *
 * Purpose:	Convenience wrapper around protecting extensible array super block
 *
 * Return:	Non-NULL pointer to data block on success/NULL on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Sep 30 2008
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(PKG, ERR,
H5EA_sblock_t *, NULL, NULL,
H5EA__sblock_protect(H5EA_hdr_t *hdr, hid_t dxpl_id, haddr_t sblk_addr,
    unsigned sblk_idx, H5AC_protect_t rw))

    /* Local variables */

#ifdef QAK
HDfprintf(stderr, "%s: Called\n", FUNC);
#endif /* QAK */

    /* Sanity check */
    HDassert(hdr);
    HDassert(H5F_addr_defined(sblk_addr));

    /* Protect the super block */
    if(NULL == (ret_value = (H5EA_sblock_t *)H5AC_protect(hdr->f, dxpl_id, H5AC_EARRAY_SBLOCK, sblk_addr, &sblk_idx, hdr, rw)))
        H5E_THROW(H5E_CANTPROTECT, "unable to protect extensible array super block, address = %llu", (unsigned long_long)sblk_addr)

CATCH

END_FUNC(PKG)   /* end H5EA__sblock_protect() */


/*-------------------------------------------------------------------------
 * Function:	H5EA__sblock_unprotect
 *
 * Purpose:	Convenience wrapper around unprotecting extensible array super block
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Sep 30 2008
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(PKG, ERR,
herr_t, SUCCEED, FAIL,
H5EA__sblock_unprotect(H5EA_sblock_t *sblock, hid_t dxpl_id, unsigned cache_flags))

    /* Local variables */

#ifdef QAK
HDfprintf(stderr, "%s: Called\n", FUNC);
#endif /* QAK */

    /* Sanity check */
    HDassert(sblock);

    /* Unprotect the super block */
    if(H5AC_unprotect(sblock->hdr->f, dxpl_id, H5AC_EARRAY_SBLOCK, sblock->addr, sblock, cache_flags) < 0)
        H5E_THROW(H5E_CANTUNPROTECT, "unable to unprotect extensible array super block, address = %llu", (unsigned long_long)sblock->addr)

CATCH

END_FUNC(PKG)   /* end H5EA__sblock_unprotect() */


/*-------------------------------------------------------------------------
 * Function:	H5EA__sblock_delete
 *
 * Purpose:	Delete a super block
 *
 * Return:	SUCCEED/FAIL
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Sep 30 2008
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(PKG, ERR,
herr_t, SUCCEED, FAIL,
H5EA__sblock_delete(H5EA_hdr_t *hdr, hid_t dxpl_id, haddr_t sblk_addr,
    unsigned sblk_idx))

    /* Local variables */
    H5EA_sblock_t *sblock = NULL;       /* Pointer to super block */

#ifdef QAK
HDfprintf(stderr, "%s: Called\n", FUNC);
#endif /* QAK */

    /* Sanity check */
    HDassert(hdr);
    HDassert(H5F_addr_defined(sblk_addr));

    /* Protect super block */
    if(NULL == (sblock = H5EA__sblock_protect(hdr, dxpl_id, sblk_addr, sblk_idx, H5AC_WRITE)))
        H5E_THROW(H5E_CANTPROTECT, "unable to protect extensible array super block, address = %llu", (unsigned long_long)sblk_addr)

    /* Check for super block having data block pointers */
    if(sblock->ndblks > 0) {
        size_t u;               /* Local index variable */

        /* Iterate over data blocks */
        for(u = 0; u < sblock->ndblks; u++) {
            /* Check for data block existing */
            if(H5F_addr_defined(sblock->dblk_addrs[u])) {
                /* Delete data block */
                if(H5EA__dblock_delete(hdr, dxpl_id, sblock->dblk_addrs[u], sblock->dblk_nelmts) < 0)
                    H5E_THROW(H5E_CANTDELETE, "unable to delete extensible array data block")
                sblock->dblk_addrs[u] = HADDR_UNDEF;
            } /* end if */
        } /* end for */
    } /* end if */

    /* Release super block's disk space */
    if(H5MF_xfree(hdr->f, H5FD_MEM_EARRAY_SBLOCK, dxpl_id, sblk_addr, (hsize_t)sblock->size) < 0)
        H5E_THROW(H5E_CANTFREE, "unable to free extensible array super block")

CATCH
    /* Finished deleting super block in metadata cache */
    if(sblock && H5EA__sblock_unprotect(sblock, dxpl_id, H5AC__DIRTIED_FLAG | H5AC__DELETED_FLAG) < 0)
        H5E_THROW(H5E_CANTUNPROTECT, "unable to release extensible array super block")

END_FUNC(PKG)   /* end H5EA__sblock_delete() */


/*-------------------------------------------------------------------------
 * Function:	H5EA__sblock_dest
 *
 * Purpose:	Destroys an extensible array super block in memory.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Sep 30 2008
 *
 *-------------------------------------------------------------------------
 */
/* ARGSUSED */
BEGIN_FUNC(PKG, ERR,
herr_t, SUCCEED, FAIL,
H5EA__sblock_dest(H5F_t *f, H5EA_sblock_t *sblock))

    /* Sanity check */
    HDassert(sblock);
    HDassert(sblock->rc == 0);

    /* Set the shared array header's file context for this operation */
    sblock->hdr->f = f;

    /* Check if we've got data block addresses in the super block */
    if(sblock->ndblks > 0) {
        /* Free buffer for super block data block addresses */
        HDassert(sblock->dblk_addrs);
        (void)H5FL_SEQ_FREE(haddr_t, sblock->dblk_addrs);
    } /* end if */

    /* Decrement reference count on shared info */
    if(H5EA__hdr_decr(sblock->hdr) < 0)
        H5E_THROW(H5E_CANTDEC, "can't decrement reference count on shared array header")

    /* Free the super block itself */
    (void)H5FL_FREE(H5EA_sblock_t, sblock);

CATCH

END_FUNC(PKG)   /* end H5EA__sblock_dest() */

