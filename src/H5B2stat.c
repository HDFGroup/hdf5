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
 * Purpose:	v2 B-tree metadata statistics functions.
 *
 */

/****************/
/* Module Setup */
/****************/

#define H5B2_PACKAGE		/*suppress error about including H5B2pkg  */

/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5B2pkg.h"		/* v2 B-trees				*/
#include "H5Eprivate.h"		/* Error handling		  	*/

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
 * Function:	H5B2_stat_info
 *
 * Purpose:	Retrieve metadata statistics for a v2 B-tree
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
H5B2_stat_info(H5F_t *f, hid_t dxpl_id, const H5B2_class_t *type,
    haddr_t addr, H5B2_stat_t *info)
{
    H5B2_t	*bt2 = NULL;            /* Pointer to the B-tree header */
    H5B2_shared_t *shared;              /* Pointer to B-tree's shared information */
    herr_t	ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5B2_stat_info)

    /* Check arguments. */
    HDassert(f);
    HDassert(type);
    HDassert(H5F_addr_defined(addr));
    HDassert(info);

    /* Look up the B-tree header */
    if(NULL == (bt2 = H5AC_protect(f, dxpl_id, H5AC_BT2_HDR, addr, type, NULL, H5AC_READ)))
	HGOTO_ERROR(H5E_BTREE, H5E_CANTPROTECT, FAIL, "unable to load B-tree header")

    /* Get pointer to reference counted shared B-tree info */
    shared = H5RC_GET_OBJ(bt2->shared);

    /* Get information about the B-tree */
    info->depth = bt2->depth;
    info->nrecords = bt2->root.all_nrec;
    info->branch_nrec = shared->branch_nrec;
    info->twig_nrec = shared->twig_nrec;
    info->leaf_nrec = shared->leaf_nrec;

done:
    /* Release B-tree header node */
    if(bt2 && H5AC_unprotect(f, dxpl_id, H5AC_BT2_HDR, addr, bt2, H5AC__NO_FLAGS_SET) < 0)
        HDONE_ERROR(H5E_BTREE, H5E_CANTUNPROTECT, FAIL, "unable to release B-tree header info")

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5B2_stat_info() */

