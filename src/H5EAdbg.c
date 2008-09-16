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
 * Created:		H5EAdbg.c
 *			Sep 11 2008
 *			Quincey Koziol <koziol@hdfgroup.org>
 *
 * Purpose:		Dump debugging information about an extensible array.
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



/*-------------------------------------------------------------------------
 * Function:	H5EA__hdr_debug
 *
 * Purpose:	Prints debugging info about a extensible array header.
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
H5EA__hdr_debug(H5F_t *f, hid_t dxpl_id, haddr_t addr, FILE *stream, int indent,
    int fwidth, const H5EA_class_t *cls))

    /* Local variables */
    H5EA_hdr_t *hdr = NULL;          /* Shared extensible array header */

    /* Check arguments */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(stream);
    HDassert(indent >= 0);
    HDassert(fwidth >= 0);
    HDassert(cls);

    /* Load the extensible array header */
    if(NULL == (hdr = (H5EA_hdr_t *)H5AC_protect(f, dxpl_id, H5AC_EARRAY_HDR, addr, cls, NULL, H5AC_READ)))
	H5E_THROW(H5E_CANTPROTECT, "unable to load extensible array header")

    /* Print opening message */
    HDfprintf(stream, "%*sExtensible Array Header...\n", indent, "");

    /* Print the values */
    HDfprintf(stream, "%*s%-*s %s\n", indent, "", fwidth,
	      "Array class ID:",
	      (hdr->cls->id == H5EA_CLS_TEST_ID ? "H5EA_CLS_TEST_ID" :
              "Unknown!"));
    HDfprintf(stream, "%*s%-*s %Zu\n", indent, "", fwidth,
	      "Header size:",
	      hdr->size);
    HDfprintf(stream, "%*s%-*s %u\n", indent, "", fwidth,
	      "Raw Element Size:",
	      (unsigned)hdr->raw_elmt_size);
    HDfprintf(stream, "%*s%-*s %Zu\n", indent, "", fwidth,
	      "Native Element Size (on this platform):",
	      hdr->cls->nat_elmt_size);
    HDfprintf(stream, "%*s%-*s %u\n", indent, "", fwidth,
	      "Log2(Max. # of elements in array):",
	      (unsigned)hdr->idx_blk_elmts);
    HDfprintf(stream, "%*s%-*s %u\n", indent, "", fwidth,
	      "Min. # of elements per data block:",
	      (unsigned)hdr->data_blk_min_elmts);
    HDfprintf(stream, "%*s%-*s %u\n", indent, "", fwidth,
	      "Min. # of data block pointers for a super block:",
	      (unsigned)hdr->sup_blk_min_data_ptrs);
    HDfprintf(stream, "%*s%-*s %Hu\n", indent, "", fwidth,
	      "Highest element index stored (+1):",
	      hdr->max_idx_set);
    HDfprintf(stream, "%*s%-*s %a\n", indent, "", fwidth,
	      "Index Block Address:",
	      hdr->idx_blk_addr);

CATCH
    if(hdr && H5AC_unprotect(f, dxpl_id, H5AC_EARRAY_HDR, addr, hdr, H5AC__NO_FLAGS_SET) < 0)
	H5E_THROW(H5E_CANTUNPROTECT, "unable to release extensible array header")

END_FUNC(PKG)   /* end H5EA__hdr_debug() */


/*-------------------------------------------------------------------------
 * Function:	H5EA__iblock_debug
 *
 * Purpose:	Prints debugging info about a extensible array index block.
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
H5EA__iblock_debug(H5F_t *f, hid_t dxpl_id, haddr_t addr, FILE *stream, int indent,
    int fwidth, const H5EA_class_t *cls, haddr_t hdr_addr))

    /* Local variables */
    H5EA_hdr_t *hdr = NULL;          /* Shared extensible array header */
    H5EA_iblock_t *iblock = NULL;    /* Extensible array index block */

    /* Check arguments */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(stream);
    HDassert(indent >= 0);
    HDassert(fwidth >= 0);
    HDassert(cls);
    HDassert(H5F_addr_defined(hdr_addr));

    /* Load the extensible array header */
    if(NULL == (hdr = (H5EA_hdr_t *)H5AC_protect(f, dxpl_id, H5AC_EARRAY_HDR, hdr_addr, cls, NULL, H5AC_READ)))
	H5E_THROW(H5E_CANTPROTECT, "unable to load extensible array header")

    /* Sanity check */
    HDassert(H5F_addr_eq(hdr->idx_blk_addr, addr));

    /* Protect index block */
    if(NULL == (iblock = H5EA__iblock_protect(hdr, dxpl_id, H5AC_READ)))
        H5E_THROW(H5E_CANTPROTECT, "unable to protect extensible array index block, address = %llu", (unsigned long_long)hdr->idx_blk_addr)

    /* Print opening message */
    HDfprintf(stream, "%*sExtensible Array Index Block...\n", indent, "");

    /* Print the values */
    HDfprintf(stream, "%*s%-*s %s\n", indent, "", fwidth,
	      "Array class ID:",
	      (hdr->cls->id == H5EA_CLS_TEST_ID ? "H5EA_CLS_TEST_ID" :
              "Unknown!"));
    HDfprintf(stream, "%*s%-*s %Zu\n", indent, "", fwidth,
	      "Index Block size:",
	      iblock->size);

    /* Check if there are any elements in array */
    if(hdr->max_idx_set > 0) {
        unsigned u;             /* Local index variable */

        /* Print the elements in the index block */
        HDfprintf(stream, "%*sElements in Index Block:\n", indent, "");
        for(u = 0; u < MIN(hdr->idx_blk_elmts, hdr->max_idx_set); u++) {
            /* Call the class's 'debug' callback */
            if((hdr->cls->debug)(stream, (indent + 6), MAX(0, (fwidth - 6)),
                    (hsize_t)u,
                    ((uint8_t *)iblock->elmts) + (hdr->cls->nat_elmt_size * u)) < 0)
                H5E_THROW(H5E_CANTGET, "can't get element for debugging")
        } /* end for */
    } /* end if */

CATCH
    if(iblock && H5EA__iblock_unprotect(iblock, dxpl_id, H5AC__NO_FLAGS_SET) < 0)
        H5E_THROW(H5E_CANTUNPROTECT, "unable to release extensible array index block")
    if(hdr && H5AC_unprotect(f, dxpl_id, H5AC_EARRAY_HDR, hdr_addr, hdr, H5AC__NO_FLAGS_SET) < 0)
	H5E_THROW(H5E_CANTUNPROTECT, "unable to release extensible array header")

END_FUNC(PKG)   /* end H5EA__hdr_debug() */

