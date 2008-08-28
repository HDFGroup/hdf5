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

/* Declare a free list to manage the H5EA_hdr_t struct */
H5FL_DEFINE(H5EA_hdr_t);



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
H5EA__hdr_alloc(H5F_t *f))

    H5EA_hdr_t *hdr = NULL;          /* Shared fractal heap header */

    /*
     * Check arguments.
     */
    HDassert(f);

    /* Allocate space for the shared information */
    if(NULL == (hdr = H5FL_CALLOC(H5EA_hdr_t)))
	H5E_THROW(H5E_CANTALLOC, "memory allocation failed for extensible array shared header")

    /* Set the internal parameters for the array */
    hdr->f = f;

    /* Set the return value */
    ret_value = hdr;

CATCH
    if(!ret_value)
        if(hdr && H5EA__cache_hdr_dest(f, hdr) < 0)
            H5E_THROW(H5E_CANTFREE, "unable to destroy extensible array header")

END_FUNC(PKG)   /* end H5EA__hdr_alloc() */


/*-------------------------------------------------------------------------
 * Function:	H5EA_hdr_create
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

    H5EA_hdr_t *hdr;            /* Extensible array header */

#ifdef QAK
HDfprintf(stderr, "%s: Called\n", FUNC);
#endif /* QAK */

    /*
     * Check arguments.
     */
    HDassert(f);
    HDassert(cparam);

#ifndef NDEBUG
    /* Check for valid parameters */
    if(cparam->elmt_size == 0)
	H5E_THROW(H5E_BADVALUE, "element size must be greater than zero")
    if(!POWER_OF_TWO(cparam->sup_blk_min_data_ptrs))
	H5E_THROW(H5E_BADVALUE, "min # of data block pointers in super block not power of two")
    if(!POWER_OF_TWO(cparam->data_blk_min_elmts))
	H5E_THROW(H5E_BADVALUE, "min # of elements per data block not power of two")
#endif /* NDEBUG */

    /* Allocate space for the shared information */
    if(NULL == (hdr = H5EA__hdr_alloc(f)))
	H5E_THROW(H5E_CANTALLOC, "memory allocation failed for extensible array shared header")

    /* Set the internal parameters for the array */

    /* Set the creation parameters for the array */
    hdr->elmt_size = cparam->elmt_size;
    hdr->idx_blk_elmts = cparam->idx_blk_elmts;
    hdr->sup_blk_min_data_ptrs = cparam->sup_blk_min_data_ptrs;
    hdr->data_blk_min_elmts = cparam->data_blk_min_elmts;

    /* Set size of header on disk */
    hdr->size = H5EA_HEADER_SIZE(hdr);

    /* Allocate space for the header on disk */
    if(HADDR_UNDEF == (hdr->addr = H5MF_alloc(f, H5FD_MEM_EARRAY_HDR, dxpl_id, hdr->size)))
	H5E_THROW(H5E_CANTALLOC, "file allocation failed for extensible array header")

    /* Cache the new extensible array header */
    if(H5AC_set(f, dxpl_id, H5AC_EARRAY_HDR, hdr->addr, hdr, H5AC__NO_FLAGS_SET) < 0)
	H5E_THROW(H5E_CANTINSERT, "can't add extensible array header to cache")

    /* Set address of array header to return */
    ret_value = hdr->addr;


CATCH

END_FUNC(PKG)   /* end H5EA__hdr_create() */


/*-------------------------------------------------------------------------
 * Function:	H5EA_hdr_incr
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
 * Function:	H5EA_hdr_decr
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
 * Function:	H5EA_hdr_fuse_incr
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
 * Function:	H5EA_hdr_fuse_decr
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

    /*
     * Check arguments.
     */
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

#ifdef LATER
    /* Check for root direct/indirect block */
    if(H5F_addr_defined(hdr->man_dtable.table_addr)) {
#ifdef QAK
HDfprintf(stderr, "%s: hdr->man_dtable.table_addr = %a\n", FUNC, hdr->man_dtable.table_addr);
#endif /* QAK */
        if(hdr->man_dtable.curr_root_rows == 0) {
            hsize_t dblock_size;        /* Size of direct block */

            /* Check for I/O filters on this heap */
            if(hdr->filter_len > 0) {
                dblock_size = (hsize_t)hdr->pline_root_direct_size;
#ifdef QAK
HDfprintf(stderr, "%s: hdr->pline_root_direct_size = %Zu\n", FUNC, hdr->pline_root_direct_size);
#endif /* QAK */

                /* Reset the header's pipeline information */
                hdr->pline_root_direct_size = 0;
                hdr->pline_root_direct_filter_mask = 0;
            } /* end else */
            else
                dblock_size = (hsize_t)hdr->man_dtable.cparam.start_block_size;

            /* Delete root direct block */
            if(H5HF_man_dblock_delete(hdr->f, dxpl_id, hdr->man_dtable.table_addr, dblock_size) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTFREE, FAIL, "unable to release fractal heap root direct block")
        } /* end if */
        else {
            /* Delete root indirect block */
            if(H5HF_man_iblock_delete(hdr, dxpl_id, hdr->man_dtable.table_addr, hdr->man_dtable.curr_root_rows, NULL, 0) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTFREE, FAIL, "unable to release fractal heap root indirect block")
        } /* end else */
    } /* end if */
#endif /* LATER */

    /* Release header's disk space */
    if(H5MF_xfree(hdr->f, H5FD_MEM_EARRAY_HDR, dxpl_id, hdr->addr, (hsize_t)hdr->size) < 0)
        H5E_THROW(H5E_CANTFREE, "unable to release extensible array header")

    /* Finished deleting header */
    if(H5AC_unprotect(hdr->f, dxpl_id, H5AC_EARRAY_HDR, hdr->addr, hdr, H5AC__DIRTIED_FLAG | H5AC__DELETED_FLAG) < 0)
        H5E_THROW(H5E_CANTUNPROTECT, "unable to release extensible array header")
    hdr = NULL;

CATCH

    /* Unprotect the header, if an error occurred */
    if(hdr && H5AC_unprotect(hdr->f, dxpl_id, H5AC_EARRAY_HDR, hdr->addr, hdr, H5AC__NO_FLAGS_SET) < 0)
        H5E_THROW(H5E_CANTUNPROTECT, "unable to release extensible array header")

END_FUNC(PKG)   /* end H5EA__hdr_delete() */

