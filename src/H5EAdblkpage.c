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
 * Created:		H5EAdblkpage.c
 *			Nov 20 2008
 *			Quincey Koziol <koziol@hdfgroup.org>
 *
 * Purpose:		Data block page routines for extensible arrays.
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

/* Declare a free list to manage the H5EA_dblk_page_t struct */
H5FL_DEFINE_STATIC(H5EA_dblk_page_t);



/*-------------------------------------------------------------------------
 * Function:	H5EA__dblk_page_alloc
 *
 * Purpose:	Allocate extensible array data block page
 *
 * Return:	Non-NULL pointer to data block on success/NULL on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Nov 20 2008
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(PKG, ERR,
H5EA_dblk_page_t *, NULL, NULL,
H5EA__dblk_page_alloc(H5EA_hdr_t *hdr, H5EA_sblock_t *parent))

    /* Local variables */
    H5EA_dblk_page_t *dblk_page = NULL;          /* Extensible array data block page */

    /* Check arguments */
    HDassert(hdr);

    /* Allocate memory for the data block */
    if(NULL == (dblk_page = H5FL_CALLOC(H5EA_dblk_page_t)))
	H5E_THROW(H5E_CANTALLOC, "memory allocation failed for extensible array data block page")

    /* Share common array information */
    if(H5EA__hdr_incr(hdr) < 0)
	H5E_THROW(H5E_CANTINC, "can't increment reference count on shared array header")
    dblk_page->hdr = hdr;

    /* Set non-zero internal fields */
    dblk_page->parent = parent;

    /* Allocate buffer for elements in data block page */
    if(NULL == (dblk_page->elmts = H5EA__hdr_alloc_elmts(hdr, hdr->dblk_page_nelmts)))
        H5E_THROW(H5E_CANTALLOC, "memory allocation failed for data block page element buffer")

    /* Set the return value */
    ret_value = dblk_page;

CATCH
    if(!ret_value)
        if(dblk_page && H5EA__dblk_page_dest(dblk_page) < 0)
            H5E_THROW(H5E_CANTFREE, "unable to destroy extensible array data block page")

END_FUNC(PKG)   /* end H5EA__dblk_page_alloc() */


/*-------------------------------------------------------------------------
 * Function:	H5EA__dblk_page_create
 *
 * Purpose:	Creates a new extensible array data block page in the file
 *
 * Return:	Valid file address on success/HADDR_UNDEF on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Nov 20 2008
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(PKG, ERR,
herr_t, SUCCEED, FAIL,
H5EA__dblk_page_create(H5EA_hdr_t *hdr, hid_t dxpl_id, H5EA_sblock_t *parent,
    haddr_t addr))

    /* Local variables */
    H5EA_dblk_page_t *dblk_page = NULL;      /* Extensible array data block page */

#ifdef QAK
HDfprintf(stderr, "%s: Called, addr = %a\n", FUNC, addr);
#endif /* QAK */

    /* Sanity check */
    HDassert(hdr);

    /* Allocate the data block page */
    if(NULL == (dblk_page = H5EA__dblk_page_alloc(hdr, parent)))
	H5E_THROW(H5E_CANTALLOC, "memory allocation failed for extensible array data block page")

    /* Set info about data block page on disk */
    dblk_page->addr = addr;
    dblk_page->size = H5EA_DBLK_PAGE_SIZE(dblk_page);
#ifdef QAK
HDfprintf(stderr, "%s: dblk_page->size = %Zu\n", FUNC, dblk_page->size);
#endif /* QAK */

    /* Clear any elements in data block page to fill value */
    if((hdr->cparam.cls->fill)(dblk_page->elmts, (size_t)hdr->dblk_page_nelmts) < 0)
        H5E_THROW(H5E_CANTSET, "can't set extensible array data block page elements to class's fill value")

    /* Cache the new extensible array data block page */
    if(H5AC_insert_entry(hdr->f, dxpl_id, H5AC_EARRAY_DBLK_PAGE, dblk_page->addr, dblk_page, H5AC__NO_FLAGS_SET) < 0)
	H5E_THROW(H5E_CANTINSERT, "can't add extensible array data block page to cache")

CATCH
    if(ret_value < 0)
        if(dblk_page) {
            /* Destroy data block page */
            if(H5EA__dblk_page_dest(dblk_page) < 0)
                H5E_THROW(H5E_CANTFREE, "unable to destroy extensible array data block page")
        } /* end if */

END_FUNC(PKG)   /* end H5EA__dblk_page_create() */


/*-------------------------------------------------------------------------
 * Function:	H5EA__dblk_page_protect
 *
 * Purpose:	Convenience wrapper around protecting extensible array data
 *              block page
 *
 * Return:	Non-NULL pointer to data block page on success/NULL on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Nov 20 2008
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(PKG, ERR,
H5EA_dblk_page_t *, NULL, NULL,
H5EA__dblk_page_protect(H5EA_hdr_t *hdr, hid_t dxpl_id, H5EA_sblock_t *parent,
    haddr_t dblk_page_addr, H5AC_protect_t rw))

    /* Local variables */
    H5EA_dblk_page_cache_ud_t udata;      /* Information needed for loading data block page */

#ifdef QAK
HDfprintf(stderr, "%s: Called\n", FUNC);
#endif /* QAK */

    /* Sanity check */
    HDassert(hdr);
    HDassert(H5F_addr_defined(dblk_page_addr));

    /* Set up user data */
    udata.hdr = hdr;
    udata.parent = parent;

    /* Protect the data block page */
    if(NULL == (ret_value = (H5EA_dblk_page_t *)H5AC_protect(hdr->f, dxpl_id, H5AC_EARRAY_DBLK_PAGE, dblk_page_addr, &udata, rw)))
        H5E_THROW(H5E_CANTPROTECT, "unable to protect extensible array data block page, address = %llu", (unsigned long long)dblk_page_addr)

CATCH

END_FUNC(PKG)   /* end H5EA__dblk_page_protect() */


/*-------------------------------------------------------------------------
 * Function:	H5EA__dblk_page_unprotect
 *
 * Purpose:	Convenience wrapper around unprotecting extensible array
 *              data block page
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Nov 20 2008
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(PKG, ERR,
herr_t, SUCCEED, FAIL,
H5EA__dblk_page_unprotect(H5EA_dblk_page_t *dblk_page, hid_t dxpl_id,
    unsigned cache_flags))

    /* Local variables */

#ifdef QAK
HDfprintf(stderr, "%s: Called\n", FUNC);
#endif /* QAK */

    /* Sanity check */
    HDassert(dblk_page);

    /* Unprotect the data block page */
    if(H5AC_unprotect(dblk_page->hdr->f, dxpl_id, H5AC_EARRAY_DBLK_PAGE, dblk_page->addr, dblk_page, cache_flags) < 0)
        H5E_THROW(H5E_CANTUNPROTECT, "unable to unprotect extensible array data block page, address = %llu", (unsigned long long)dblk_page->addr)

CATCH

END_FUNC(PKG)   /* end H5EA__dblk_page_unprotect() */


/*-------------------------------------------------------------------------
 * Function:	H5EA__dblk_page_dest
 *
 * Purpose:	Destroys an extensible array data block page in memory.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Nov 20 2008
 *
 *-------------------------------------------------------------------------
 */
/* ARGSUSED */
BEGIN_FUNC(PKG, ERR,
herr_t, SUCCEED, FAIL,
H5EA__dblk_page_dest(H5EA_dblk_page_t *dblk_page))

    /* Sanity check */
    HDassert(dblk_page);

    /* Check if header field has been initialized */
    if(dblk_page->hdr) {
        /* Check if buffer for data block page elements has been initialized */
        if(dblk_page->elmts) {
            /* Free buffer for data block page elements */
            if(H5EA__hdr_free_elmts(dblk_page->hdr, dblk_page->hdr->dblk_page_nelmts, dblk_page->elmts) < 0)
                H5E_THROW(H5E_CANTFREE, "unable to free extensible array data block element buffer")
            dblk_page->elmts = NULL;
        } /* end if */

        /* Decrement reference count on shared info */
        if(H5EA__hdr_decr(dblk_page->hdr) < 0)
            H5E_THROW(H5E_CANTDEC, "can't decrement reference count on shared array header")
        dblk_page->hdr = NULL;
    } /* end if */

    /* Free the data block page itself */
    dblk_page = H5FL_FREE(H5EA_dblk_page_t, dblk_page);

CATCH

END_FUNC(PKG)   /* end H5EA__dblk_page_dest() */

