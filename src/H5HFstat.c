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

/* Programmer:  Quincey Koziol <koziol@ncsa.uiuc.edu>
 *              Monday, March  6, 2006
 *
 * Purpose:	Fractal heap metadata statistics functions.
 *
 */

/****************/
/* Module Setup */
/****************/

#define H5HF_PACKAGE		/*suppress error about including H5HFpkg  */

/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5HFpkg.h"		/* Fractal heaps			*/

/****************/
/* Local Macros */
/****************/


/********************/
/* Package Typedefs */
/********************/


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
 * Function:	H5HF_stat_info
 *
 * Purpose:	Retrieve metadata statistics for the fractal heap
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Monday, March  6, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_stat_info(H5F_t *f, hid_t dxpl_id, haddr_t fh_addr, H5HF_stat_t *stats)
{
    H5HF_t	*fh = NULL;             /* Pointer to the B-tree header */
    H5HF_shared_t *shared;              /* Shared fractal heap information */
    herr_t	ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_stat_info)

    /* Check arguments. */
    HDassert(f);
    HDassert(H5F_addr_defined(fh_addr));
    HDassert(stats);

    /* Look up the fractal heap header */
    if(NULL == (fh = H5AC_protect(f, dxpl_id, H5AC_FHEAP_HDR, fh_addr, NULL, NULL, H5AC_READ)))
	HGOTO_ERROR(H5E_HEAP, H5E_CANTPROTECT, FAIL, "unable to load fractal heap header")

    /* Get the pointer to the shared fractal heap info */
    shared = H5RC_GET_OBJ(fh->shared);
    HDassert(shared);

    /* Report statistics for fractal heap */
    stats->total_size = shared->total_size;
    stats->man_size = shared->man_size;
    stats->std_size = shared->std_size;
    stats->man_free_space = shared->total_man_free;
    stats->nobjs = shared->nobjs;
/* XXX: Add more metadata statistics for the heap */

done:
    /* Release fractal heap header node */
    if(fh && H5AC_unprotect(f, dxpl_id, H5AC_FHEAP_HDR, fh_addr, fh, H5AC__NO_FLAGS_SET) < 0)
        HDONE_ERROR(H5E_HEAP, H5E_CANTUNPROTECT, FAIL, "unable to release fractal heap header info")

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5HF_stat_info() */

