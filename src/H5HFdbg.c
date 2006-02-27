/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have     *
 * access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu. *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*-------------------------------------------------------------------------
 *
 * Created:		H5HFdbg.c
 *			Feb 24 2006
 *			Quincey Koziol <koziol@ncsa.uiuc.edu>
 *
 * Purpose:		Dump debugging information about a fractal heap
 *
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/

#define H5HF_PACKAGE		/*suppress error about including H5HFpkg  */

/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5HFpkg.h"		/* Fractal heaps			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5FLprivate.h"	/* Free Lists                           */

/****************/
/* Local Macros */
/****************/


/******************/
/* Local Typedefs */
/******************/


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
 * Function:	H5HF_hdr_debug
 *
 * Purpose:	Prints debugging info about a fractal heap header.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb 24 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_hdr_debug(H5F_t *f, hid_t dxpl_id, haddr_t addr, FILE *stream, int indent, int fwidth)
{
    H5HF_t	*fh = NULL;             /* Fractal heap header info */
    H5HF_shared_t *shared;              /* Shared fractal heap information */
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI(H5HF_hdr_debug, FAIL)

    /*
     * Check arguments.
     */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(stream);
    HDassert(indent >= 0);
    HDassert(fwidth >= 0);

    /*
     * Load the fractal heap header.
     */
    if(NULL == (fh = H5AC_protect(f, dxpl_id, H5AC_FHEAP_HDR, addr, NULL, NULL, H5AC_READ)))
	HGOTO_ERROR(H5E_BTREE, H5E_CANTLOAD, FAIL, "unable to load fractal heap header")

    /* Get the pointer to the shared fractal heap info */
    shared = H5RC_GET_OBJ(fh->shared);
    HDassert(shared);

    /* Print opening message */
    HDfprintf(stream, "%*sFractal Heap Header...\n", indent, "");

    /*
     * Print the values.
     */
    HDfprintf(stream, "%*s%-*s %s\n", indent, "", fwidth,
	      "Heap address mapping method:",
	      ((shared->addrmap) == H5HF_ABSOLUTE ? "Absolute" :
	      ((shared->addrmap) == H5HF_MAPPED ? "Mapped" :
              "Unknown!")));
    HDfprintf(stream, "%*s%-*s %lu\n", indent, "", fwidth,
	      "Min. size of standalone object:",
	      (unsigned long)shared->standalone_size);
    HDfprintf(stream, "%*s%-*s %lu\n", indent, "", fwidth,
	      "Fixed length object size:",
	      (unsigned long)shared->fixed_len_size);

done:
    if(fh && H5AC_unprotect(f, dxpl_id, H5AC_FHEAP_HDR, addr, fh, H5AC__NO_FLAGS_SET) < 0)
        HDONE_ERROR(H5E_BTREE, H5E_PROTECT, FAIL, "unable to release fractal heap header")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_hdr_debug() */

