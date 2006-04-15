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
 *              Thursday, February  3, 2006
 *
 * Purpose:	Fractal heap testing functions.
 *
 */

/****************/
/* Module Setup */
/****************/

#define H5HF_PACKAGE		/*suppress error about including H5HFpkg  */
#define H5HF_TESTING		/*suppress warning about H5HF testing funcs*/

/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5HFpkg.h"		/* Fractal heaps			*/

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
 * Function:	H5HF_get_cparam_test
 *
 * Purpose:	Retrieve the parameters used to create the fractal heap
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Friday, February 24, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_get_cparam_test(H5F_t *f, hid_t dxpl_id, haddr_t fh_addr, H5HF_create_t *cparam)
{
    H5HF_t	*hdr = NULL;            /* Pointer to the fractal heap header */
    herr_t	ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_get_cparam_test)

    /* Check arguments. */
    HDassert(f);
    HDassert(H5F_addr_defined(fh_addr));
    HDassert(cparam);

    /* Look up the fractal heap header */
    if(NULL == (hdr = H5AC_protect(f, dxpl_id, H5AC_FHEAP_HDR, fh_addr, NULL, NULL, H5AC_READ)))
	HGOTO_ERROR(H5E_HEAP, H5E_CANTPROTECT, FAIL, "unable to load fractal heap header")

    /* Get fractal heap creation parameters */
    cparam->addrmap = hdr->addrmap;
    cparam->standalone_size = hdr->standalone_size;
    HDmemcpy(&(cparam->managed), &(hdr->man_dtable.cparam), sizeof(H5HF_dtable_cparam_t));

done:
    /* Release fractal heap header node */
    if(hdr && H5AC_unprotect(f, dxpl_id, H5AC_FHEAP_HDR, fh_addr, hdr, H5AC__NO_FLAGS_SET) < 0)
        HDONE_ERROR(H5E_HEAP, H5E_CANTUNPROTECT, FAIL, "unable to release fractal heap header info")

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5HF_get_cparam_test() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_get_dblock_free_test
 *
 * Purpose:	Retrieve the size of direct block free space for a given
 *              direct or indirect block size
 *
 * Return:	Success:	Size of direct block free space
 *
 *		Failure:	0
 *
 * Programmer:	Quincey Koziol
 *              Monday, April 10, 2006
 *
 *-------------------------------------------------------------------------
 */
hsize_t
H5HF_get_dblock_free_test(H5F_t *f, hid_t dxpl_id, haddr_t fh_addr, unsigned row)
{
    H5HF_t	*hdr = NULL;            /* Pointer to the fractal heap header */
    hsize_t	ret_value;              /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_get_dblock_free_test)

    /* Check arguments. */
    HDassert(f);
    HDassert(H5F_addr_defined(fh_addr));

    /* Look up the fractal heap header */
    if(NULL == (hdr = H5AC_protect(f, dxpl_id, H5AC_FHEAP_HDR, fh_addr, NULL, NULL, H5AC_READ)))
	HGOTO_ERROR(H5E_HEAP, H5E_CANTPROTECT, 0, "unable to load fractal heap header")

    /* Return direct block free space */
    ret_value = hdr->man_dtable.row_dblock_free[row];

done:
    /* Release fractal heap header node */
    if(hdr && H5AC_unprotect(f, dxpl_id, H5AC_FHEAP_HDR, fh_addr, hdr, H5AC__NO_FLAGS_SET) < 0)
        HDONE_ERROR(H5E_HEAP, H5E_CANTUNPROTECT, 0, "unable to release fractal heap header info")

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5HF_get_dblock_free_test() */

