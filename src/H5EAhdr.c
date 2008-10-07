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
 * Created:		H5EAhdr.c
 *			Aug 26 2008
 *			Quincey Koziol <koziol@hdfgroup.org>
 *
 * Purpose:		Array header routines for extensible arrays.
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
#include "H5Vprivate.h"		/* Vectors and arrays 			*/


/****************/
/* Local Macros */
/****************/

/* Max. # of bits for max. nelmts index */
#define H5EA_MAX_NELMTS_IDX_MAX 64


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

/* Alias for pointer to factory, for use when allocating sequences of them */
typedef H5FL_fac_head_t *H5FL_fac_head_ptr_t;


/*******************/
/* Local Variables */
/*******************/

/* Declare a free list to manage the H5EA_hdr_t struct */
H5FL_DEFINE_STATIC(H5EA_hdr_t);

/* Declare a free list to manage the H5FL_fac_head_ptr_t sequence information */
H5FL_SEQ_DEFINE_STATIC(H5FL_fac_head_ptr_t);

/* Declare a free list to manage the H5EA_sblk_info_t sequence information */
H5FL_SEQ_DEFINE_STATIC(H5EA_sblk_info_t);



/*-------------------------------------------------------------------------
 * Function:	H5EA__hdr_alloc
 *
 * Purpose:	Allocate shared extensible array header
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Aug 26 2008
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(PKG, ERR,
H5EA_hdr_t *, NULL, NULL,
H5EA__hdr_alloc(H5F_t *f, const H5EA_class_t *cls))

    /* Local variables */
    H5EA_hdr_t *hdr = NULL;          /* Shared extensible array header */

    /* Check arguments */
    HDassert(f);
    HDassert(cls);

    /* Allocate space for the shared information */
    if(NULL == (hdr = H5FL_CALLOC(H5EA_hdr_t)))
	H5E_THROW(H5E_CANTALLOC, "memory allocation failed for extensible array shared header")

    /* Set the internal parameters for the array */
    hdr->f = f;
    hdr->sizeof_addr = H5F_SIZEOF_ADDR(f);
    hdr->sizeof_size = H5F_SIZEOF_SIZE(f);

    /* Set the class of the array */
    hdr->cparam.cls = cls;

    /* Set the return value */
    ret_value = hdr;

CATCH
    if(!ret_value)
        if(hdr && H5EA__hdr_dest(hdr) < 0)
            H5E_THROW(H5E_CANTFREE, "unable to destroy extensible array header")

END_FUNC(PKG)   /* end H5EA__hdr_alloc() */


/*-------------------------------------------------------------------------
 * Function:	H5EA__hdr_init
 *
 * Purpose:	Compute useful information for extensible array, based on
 *              "creation" information.
 *
 * Notes:	The equations for variables below are based on this information:
 *
 *	<sblk idx>  <# of dblks>  <size of dblks>       Range of elements in sblk
 *	==========  ============  ===============       =========================
 *	      0          1         1 * <dblk min elmts>   0 * <dblk min elmts> <->   1 * <dblk min elmts> - 1
 *	      1          1         2 * <dblk min elmts>   1 * <dblk min elmts> <->   3 * <dblk min elmts> - 1
 *	      2          2         2 * <dblk min elmts>   3 * <dblk min elmts> <->   7 * <dblk min elmts> - 1
 *	      3          2         4 * <dblk min elmts>   7 * <dblk min elmts> <->  15 * <dblk min elmts> - 1
 *	      4          4         4 * <dblk min elmts>  15 * <dblk min elmts> <->  31 * <dblk min elmts> - 1
 *	      5          4         8 * <dblk min elmts>  31 * <dblk min elmts> <->  63 * <dblk min elmts> - 1
 *	      6          8         8 * <dblk min elmts>  63 * <dblk min elmts> <-> 127 * <dblk min elmts> - 1
 *	      7          8        16 * <dblk min elmts> 127 * <dblk min elmts> <-> 255 * <dblk min elmts> - 1
 *	      .          .         . * <dblk min elmts>   . * <dblk min elmts> <->   . * <dblk min elmts> - 1
 *	      .          .         . * <dblk min elmts>   . * <dblk min elmts> <->   . * <dblk min elmts> - 1
 *	      .          .         . * <dblk min elmts>   . * <dblk min elmts> <->   . * <dblk min elmts> - 1
 *
 *	Therefore:
 *		<sblk idx>(<elmt idx>) = lg2((<elmt idx> / <dblk min elmts>) + 1)
 *		<# of dblks>(<sblk idx>) = 2 ^ (<sblk idx> / 2)
 *		<size of dblk>(<sblk idx>) = 2 ^ ((<sblk idx> + 1) / 2)
 *		<total # of sblks>(<max. # of elmts>) = 1 + (lg2(<max. # of elmts>) - lg2(<dblk min_elmts>))
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Sep 18 2008
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(PKG, ERR,
herr_t, SUCCEED, FAIL,
H5EA__hdr_init(H5EA_hdr_t *hdr))

    /* Local variables */
    hsize_t start_idx;          /* First element index for each super block */
    hsize_t start_dblk;         /* First data block index for each super block */
    size_t u;                   /* Local index variable */

    /* Sanity check */
    HDassert(hdr);
    HDassert(hdr->cparam.max_nelmts_bits);
    HDassert(hdr->cparam.data_blk_min_elmts);
    HDassert(hdr->cparam.sup_blk_min_data_ptrs);

    /* Compute general information */
    hdr->nsblks = 1 + (hdr->cparam.max_nelmts_bits - H5V_log2_of2(hdr->cparam.data_blk_min_elmts));
#ifdef QAK
HDfprintf(stderr, "%s: hdr->nsblks = %Zu\n", FUNC, hdr->nsblks);
#endif /* QAK */

    /* Allocate information for each super block */
    if(NULL == (hdr->sblk_info = H5FL_SEQ_MALLOC(H5EA_sblk_info_t, hdr->nsblks)))
        H5E_THROW(H5E_CANTALLOC, "memory allocation failed for super block info array")

    /* Compute information about each super block */
    start_idx = 0;
    start_dblk = 0;
    for(u = 0; u < hdr->nsblks; u++) {
        hdr->sblk_info[u].ndblks = (size_t)H5_EXP2(u / 2);
        hdr->sblk_info[u].dblk_nelmts = (size_t)H5_EXP2((u + 1) / 2) * hdr->cparam.data_blk_min_elmts;
        hdr->sblk_info[u].start_idx = start_idx;
        hdr->sblk_info[u].start_dblk = start_dblk;
#ifdef QAK
HDfprintf(stderr, "%s: hdr->sblk_info[%Zu] = {%Zu, %Zu, %Hu, %Hu}\n", FUNC, u, hdr->sblk_info[u].ndblks, hdr->sblk_info[u].dblk_nelmts, hdr->sblk_info[u].start_idx, hdr->sblk_info[u].start_dblk);
#endif /* QAK */

        /* Advance starting indices for next super block */
        start_idx += (hsize_t)hdr->sblk_info[u].ndblks * (hsize_t)hdr->sblk_info[u].dblk_nelmts;
        start_dblk += (hsize_t)hdr->sblk_info[u].ndblks;
    } /* end for */

    /* Set size of header on disk */
    hdr->size = H5EA_HEADER_SIZE(hdr);

CATCH

END_FUNC(PKG)   /* end H5EA__hdr_init() */


/*-------------------------------------------------------------------------
 * Function:	H5EA__hdr_alloc_elmts
 *
 * Purpose:	Allocate extensible array data block elements
 *
 * Return:	Non-NULL pointer to buffer for elements on success/NULL on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Sep 16 2008
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(PKG, ERR,
void *, NULL, NULL,
H5EA__hdr_alloc_elmts(H5EA_hdr_t *hdr, size_t nelmts))

    /* Local variables */
    void *elmts = NULL;         /* Element buffer allocated */
    unsigned idx;               /* Index of element buffer factory in header */

    /* Check arguments */
    HDassert(hdr);
    HDassert(nelmts > 0);

    /* Compute the index of the element buffer factory */
    H5_CHECK_OVERFLOW(nelmts, /*From:*/size_t, /*To:*/uint32_t);
    idx = H5V_log2_of2((uint32_t)nelmts) - H5V_log2_of2((uint32_t)hdr->cparam.data_blk_min_elmts);
#ifdef QAK
HDfprintf(stderr, "%s: nelmts = %Zu, hdr->data_blk_min_elmts = %u, idx = %u\n", FUNC, nelmts, (unsigned)hdr->data_blk_min_elmts, idx);
#endif /* QAK */

    /* Check for needing to increase size of array of factories */
    if(idx >= hdr->elmt_fac.nalloc) {
        H5FL_fac_head_t **new_fac;      /* New array of element factories */
        size_t new_nalloc = MAX3(1, (idx + 1), (2 * hdr->elmt_fac.nalloc));   /* New number of factories allocated */

        /* Re-allocate array of element factories */
        if(NULL == (new_fac = H5FL_SEQ_REALLOC(H5FL_fac_head_ptr_t, hdr->elmt_fac.fac, new_nalloc)))
            H5E_THROW(H5E_CANTALLOC, "memory allocation failed for data block data element buffer factory array")

        /* Zero out new elements allocated */
        HDmemset(new_fac + hdr->elmt_fac.nalloc, 0, (new_nalloc - hdr->elmt_fac.nalloc) * sizeof(H5FL_fac_head_ptr_t));

        /* Update information about element factories in header */
        hdr->elmt_fac.nalloc = new_nalloc;
        hdr->elmt_fac.fac = new_fac;
    } /* end if */

    /* Check for un-initialized factory at index */
    if(NULL == hdr->elmt_fac.fac[idx]) {
        if(NULL == (hdr->elmt_fac.fac[idx] = H5FL_fac_init(nelmts * (size_t)hdr->cparam.cls->nat_elmt_size)))
            H5E_THROW(H5E_CANTINIT, "can't create data block data element buffer factory")
    } /* end if */

    /* Allocate buffer for elements in index block */
    if(NULL == (elmts = H5FL_FAC_MALLOC(hdr->elmt_fac.fac[idx])))
        H5E_THROW(H5E_CANTALLOC, "memory allocation failed for data block data element buffer")

    /* Set the return value */
    ret_value = elmts;

CATCH
    if(!ret_value)
        if(elmts)
            (void)H5FL_fac_free(hdr->elmt_fac.fac[idx], elmts);

END_FUNC(PKG)   /* end H5EA__hdr_alloc_elmts() */


/*-------------------------------------------------------------------------
 * Function:	H5EA__hdr_free_elmts
 *
 * Purpose:	Free extensible array data block elements
 *
 * Return:	SUCCEED/FAIL
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Sep 18 2008
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(PKG, NOERR,
herr_t, SUCCEED, -,
H5EA__hdr_free_elmts(H5EA_hdr_t *hdr, size_t nelmts, void *elmts))

    /* Local variables */
    unsigned idx;               /* Index of element buffer factory in header */

    /* Check arguments */
    HDassert(hdr);
    HDassert(nelmts > 0);
    HDassert(elmts);

    /* Compute the index of the element buffer factory */
    H5_CHECK_OVERFLOW(nelmts, /*From:*/size_t, /*To:*/uint32_t);
    idx = H5V_log2_of2((uint32_t)nelmts) - H5V_log2_of2((uint32_t)hdr->cparam.data_blk_min_elmts);
#ifdef QAK
HDfprintf(stderr, "%s: nelmts = %Zu, hdr->data_blk_min_elmts = %u, idx = %u\n", FUNC, nelmts, (unsigned)hdr->data_blk_min_elmts, idx);
#endif /* QAK */

    /* Free buffer for elements in index block */
    HDassert(idx < hdr->elmt_fac.nalloc);
    HDassert(hdr->elmt_fac.fac[idx]);
    (void)H5FL_FAC_FREE(hdr->elmt_fac.fac[idx], elmts);

END_FUNC(PKG)   /* end H5EA__hdr_free_elmts() */


/*-------------------------------------------------------------------------
 * Function:	H5EA__hdr_create
 *
 * Purpose:	Creates a new extensible array header in the file
 *
 * Return:	SUCCEED/FAIL
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Jun 17 2008
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(PKG, ERR,
haddr_t, HADDR_UNDEF, HADDR_UNDEF,
H5EA__hdr_create(H5F_t *f, hid_t dxpl_id, const H5EA_create_t *cparam))

    /* Local variables */
    H5EA_hdr_t *hdr;            /* Extensible array header */

#ifdef QAK
HDfprintf(stderr, "%s: Called\n", FUNC);
#endif /* QAK */

    /* Check arguments */
    HDassert(f);
    HDassert(cparam);

#ifndef NDEBUG
    /* Check for valid parameters */
    if(cparam->raw_elmt_size == 0)
	H5E_THROW(H5E_BADVALUE, "element size must be greater than zero")
    if(cparam->max_nelmts_bits == 0)
	H5E_THROW(H5E_BADVALUE, "max. # of nelmts bitsmust be greater than zero")
    if(cparam->max_nelmts_bits > H5EA_MAX_NELMTS_IDX_MAX)
	H5E_THROW(H5E_BADVALUE, "element size must be <= %u", (unsigned)H5EA_MAX_NELMTS_IDX_MAX)
    if(cparam->sup_blk_min_data_ptrs < 2)
	H5E_THROW(H5E_BADVALUE, "min # of data block pointers in super block must be >= two")
    if(!POWER_OF_TWO(cparam->sup_blk_min_data_ptrs))
	H5E_THROW(H5E_BADVALUE, "min # of data block pointers in super block not power of two")
    if(!POWER_OF_TWO(cparam->data_blk_min_elmts))
	H5E_THROW(H5E_BADVALUE, "min # of elements per data block not power of two")
#endif /* NDEBUG */

    /* Allocate space for the shared information */
    if(NULL == (hdr = H5EA__hdr_alloc(f, cparam->cls)))
	H5E_THROW(H5E_CANTALLOC, "memory allocation failed for extensible array shared header")

    /* Set the internal parameters for the array */
    hdr->idx_blk_addr = HADDR_UNDEF;

    /* Set the creation parameters for the array */
    HDmemcpy(&hdr->cparam, cparam, sizeof(hdr->cparam));

    /* Finish initializing extensible array header */
    if(H5EA__hdr_init(hdr) < 0)
	H5E_THROW(H5E_CANTINIT, "initialization failed for extensible array header")

    /* Allocate space for the header on disk */
    if(HADDR_UNDEF == (hdr->addr = H5MF_alloc(f, H5FD_MEM_EARRAY_HDR, dxpl_id, (hsize_t)hdr->size)))
	H5E_THROW(H5E_CANTALLOC, "file allocation failed for extensible array header")

    /* Cache the new extensible array header */
    if(H5AC_set(f, dxpl_id, H5AC_EARRAY_HDR, hdr->addr, hdr, H5AC__NO_FLAGS_SET) < 0)
	H5E_THROW(H5E_CANTINSERT, "can't add extensible array header to cache")

    /* Set address of array header to return */
    ret_value = hdr->addr;

CATCH

END_FUNC(PKG)   /* end H5EA__hdr_create() */


/*-------------------------------------------------------------------------
 * Function:	H5EA__hdr_incr
 *
 * Purpose:	Increment component reference count on shared array header
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Aug 26 2008
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(PKG, ERR,
herr_t, SUCCEED, FAIL,
H5EA__hdr_incr(H5EA_hdr_t *hdr))

    /* Sanity check */
    HDassert(hdr);

    /* Mark header as un-evictable when something is depending on it */
    if(hdr->rc == 0)
        if(H5AC_pin_protected_entry(hdr->f, hdr) < 0)
            H5E_THROW(H5E_CANTPIN, "unable to pin extensible array header")

    /* Increment reference count on shared header */
    hdr->rc++;

CATCH

END_FUNC(PKG)   /* end H5EA__hdr_incr() */


/*-------------------------------------------------------------------------
 * Function:	H5EA__hdr_decr
 *
 * Purpose:	Decrement component reference count on shared array header
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Aug 26 2008
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(PKG, ERR,
herr_t, SUCCEED, FAIL,
H5EA__hdr_decr(H5EA_hdr_t *hdr))

    /* Sanity check */
    HDassert(hdr);
    HDassert(hdr->rc);

    /* Decrement reference count on shared header */
    hdr->rc--;

    /* Mark header as evictable again when nothing depend on it */
    if(hdr->rc == 0) {
        HDassert(hdr->file_rc == 0);
        if(H5AC_unpin_entry(hdr->f, hdr) < 0)
            H5E_THROW(H5E_CANTUNPIN, "unable to unpin extensible array header")
    } /* end if */

CATCH

END_FUNC(PKG)   /* end H5EA__hdr_decr() */


/*-------------------------------------------------------------------------
 * Function:	H5EA__hdr_fuse_incr
 *
 * Purpose:	Increment file reference count on shared array header
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Aug 26 2008
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(PKG, NOERR,
herr_t, SUCCEED, -,
H5EA__hdr_fuse_incr(H5EA_hdr_t *hdr))

    /* Sanity check */
    HDassert(hdr);

    /* Increment file reference count on shared header */
    hdr->file_rc++;

END_FUNC(PKG)   /* end H5EA__hdr_fuse_incr() */


/*-------------------------------------------------------------------------
 * Function:	H5EA__hdr_fuse_decr
 *
 * Purpose:	Decrement file reference count on shared array header
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Aug 26 2008
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(PKG, NOERR,
size_t, 0, -,
H5EA__hdr_fuse_decr(H5EA_hdr_t *hdr))

    /* Sanity check */
    HDassert(hdr);
    HDassert(hdr->file_rc);

    /* Decrement file reference count on shared header */
    hdr->file_rc--;

    /* Set return value */
    ret_value = hdr->file_rc;

END_FUNC(PKG)   /* end H5EA__hdr_fuse_decr() */


/*-------------------------------------------------------------------------
 * Function:	H5EA__hdr_modified
 *
 * Purpose:	Mark an extensible array as modified
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
H5EA__hdr_modified(H5EA_hdr_t *hdr))

    /* Sanity check */
    HDassert(hdr);
    HDassert(hdr->f);

    /* Mark header as dirty in cache */
    if(H5AC_mark_pinned_or_protected_entry_dirty(hdr->f, hdr) < 0)
        H5E_THROW(H5E_CANTMARKDIRTY, "unable to mark extensible array header as dirty")

CATCH

END_FUNC(PKG)   /* end H5EA__hdr_modified() */


/*-------------------------------------------------------------------------
 * Function:	H5EA__hdr_delete
 *
 * Purpose:	Delete an extensible array, starting with the header
 *
 * Return:	SUCCEED/FAIL
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Aug 26 2008
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(PKG, ERR,
herr_t, SUCCEED, FAIL,
H5EA__hdr_delete(H5EA_hdr_t *hdr, hid_t dxpl_id))

    /* Sanity check */
    HDassert(hdr);
    HDassert(!hdr->file_rc);

#ifndef NDEBUG
{
    unsigned hdr_status = 0;         /* Array header's status in the metadata cache */

    /* Check the array header's status in the metadata cache */
    if(H5AC_get_entry_status(hdr->f, hdr->addr, &hdr_status) < 0)
        H5E_THROW(H5E_CANTGET, "unable to check metadata cache status for array header")

    /* Sanity checks on array header */
    HDassert(hdr_status & H5AC_ES__IN_CACHE);
    HDassert(hdr_status & H5AC_ES__IS_PROTECTED);
} /* end block */
#endif /* NDEBUG */

    /* Check for index block */
    if(H5F_addr_defined(hdr->idx_blk_addr)) {
#ifdef QAK
HDfprintf(stderr, "%s: hdr->idx_blk_addr = %a\n", FUNC, hdr->idx_blk_addr);
#endif /* QAK */
        /* Delete index block */
        if(H5EA__iblock_delete(hdr, dxpl_id) < 0)
            H5E_THROW(H5E_CANTDELETE, "unable to delete extensible array index block")
    } /* end if */

    /* Finished deleting header */
    if(H5AC_unprotect(hdr->f, dxpl_id, H5AC_EARRAY_HDR, hdr->addr, hdr, H5AC__DIRTIED_FLAG | H5AC__DELETED_FLAG | H5AC__FREE_FILE_SPACE_FLAG) < 0)
        H5E_THROW(H5E_CANTUNPROTECT, "unable to release extensible array header")
    hdr = NULL;

CATCH

    /* Unprotect the header, if an error occurred */
    if(hdr && H5AC_unprotect(hdr->f, dxpl_id, H5AC_EARRAY_HDR, hdr->addr, hdr, H5AC__NO_FLAGS_SET) < 0)
        H5E_THROW(H5E_CANTUNPROTECT, "unable to release extensible array header")

END_FUNC(PKG)   /* end H5EA__hdr_delete() */


/*-------------------------------------------------------------------------
 * Function:	H5EA__hdr_dest
 *
 * Purpose:	Destroys an extensible array header in memory.
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
H5EA__hdr_dest(H5EA_hdr_t *hdr))

    /* Check arguments */
    HDassert(hdr);
    HDassert(hdr->rc == 0);

    /* Check for data block element buffer factory info to free */
    if(hdr->elmt_fac.fac) {
        unsigned u;         /* Local index variable */

        /* Sanity check */
        HDassert(hdr->elmt_fac.nalloc > 0);

        /* Iterate over factories, shutting them down */
        for(u = 0; u < hdr->elmt_fac.nalloc; u++) {
            /* Check if this factory has been initialized */
            if(hdr->elmt_fac.fac[u]) {
                if(H5FL_fac_term(hdr->elmt_fac.fac[u]) < 0)
                    H5E_THROW(H5E_CANTRELEASE, "unable to destroy extensible array header factory")
                hdr->elmt_fac.fac[u] = NULL;
            } /* end if */
        } /* end for */

        /* Free factory array */
        hdr->elmt_fac.fac = (H5FL_fac_head_t **)H5FL_SEQ_FREE(H5FL_fac_head_ptr_t, hdr->elmt_fac.fac);
    } /* end if */

    /* Free the super block info array */
    if(hdr->sblk_info)
        hdr->sblk_info = (H5EA_sblk_info_t *)H5FL_SEQ_FREE(H5EA_sblk_info_t, hdr->sblk_info);

    /* Free the shared info itself */
    (void)H5FL_FREE(H5EA_hdr_t, hdr);

CATCH

END_FUNC(PKG)   /* end H5EA__hdr_dest() */

