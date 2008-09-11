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
H5FL_DEFINE(H5EA_iblock_t);

/* Declare a free list to manage the index block elements */
H5FL_BLK_DEFINE(elmt_buf);



/*-------------------------------------------------------------------------
 * Function:	H5EA__iblock_alloc
 *
 * Purpose:	Allocate shared extensible array index block
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

    /* Allocate buffer for elements in index block */
    if(hdr->idx_blk_elmts > 0)
        if(NULL == (iblock->elmts = H5FL_BLK_MALLOC(elmt_buf, (size_t)(hdr->idx_blk_elmts * hdr->cls->nat_elmt_size))))
            H5E_THROW(H5E_CANTALLOC, "memory allocation failed for index block data element buffer")

    /* Set the return value */
    ret_value = iblock;

CATCH
    if(!ret_value)
        if(iblock && H5EA__cache_iblock_dest(hdr->f, iblock) < 0)
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
HDfprintf(stderr, "%s: Called\n", __func__);
#endif /* QAK */

    /* Sanity check */
    HDassert(hdr);

    /* Allocate the index block */
    if(NULL == (iblock = H5EA__iblock_alloc(hdr)))
	H5E_THROW(H5E_CANTALLOC, "memory allocation failed for extensible array index block")

    /* Set size of header on disk */
    iblock->size = H5EA_IBLOCK_SIZE(hdr);
#ifdef QAK
HDfprintf(stderr, "%s: iblock->size = %Zu\n", __func__, iblock->size);
#endif /* QAK */

    /* Allocate space for the index block on disk */
    if(HADDR_UNDEF == (iblock->addr = H5MF_alloc(hdr->f, H5FD_MEM_EARRAY_IBLOCK, dxpl_id, iblock->size)))
	H5E_THROW(H5E_CANTALLOC, "file allocation failed for extensible array index block")

    /* Share common array information */
    iblock->hdr = hdr;
    if(H5EA__hdr_incr(hdr) < 0)
	H5E_THROW(H5E_CANTINC, "can't increment reference count on shared array header")

    /* Clear any elements in index block to fill value */
    if(hdr->idx_blk_elmts > 0) {
        /* Call the class's 'fill' callback */
        if((hdr->cls->fill)(iblock->elmts, (size_t)hdr->idx_blk_elmts) < 0)
            H5E_THROW(H5E_CANTSET, "can't set extensible array index block elements to class's fill value")
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
            if(H5EA__cache_iblock_dest(hdr->f, iblock) < 0)
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
HDfprintf(stderr, "%s: Called\n", __func__);
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
H5EA__iblock_unprotect(H5EA_iblock_t *iblock, hid_t dxpl_id, unsigned cache_flags))

    /* Local variables */

#ifdef QAK
HDfprintf(stderr, "%s: Called\n", __func__);
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
HDfprintf(stderr, "%s: Called\n", __func__);
#endif /* QAK */

    /* Sanity check */
    HDassert(hdr);
    HDassert(H5F_addr_defined(hdr->idx_blk_addr));

    /* Protect index block */
    if(NULL == (iblock = H5EA__iblock_protect(hdr, dxpl_id, H5AC_WRITE)))
        H5E_THROW(H5E_CANTPROTECT, "unable to protect extensible array index block, address = %llu", (unsigned long_long)hdr->idx_blk_addr)

    /* Iterate over data blocks & super blocks (not yet) */

    /* Release index block's disk space */
    if(H5MF_xfree(hdr->f, H5FD_MEM_EARRAY_IBLOCK, dxpl_id, hdr->idx_blk_addr, (hsize_t)iblock->size) < 0)
        H5E_THROW(H5E_CANTFREE, "unable to free extensible array index block")

CATCH
    /* Finished deleting index block in metadata cache */
    if(iblock && H5EA__iblock_unprotect(iblock, dxpl_id, H5AC__DIRTIED_FLAG | H5AC__DELETED_FLAG) < 0)
        H5E_THROW(H5E_CANTUNPROTECT, "unable to release extensible array index block")

END_FUNC(PKG)   /* end H5EA__iblock_delete() */

