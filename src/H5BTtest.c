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
 *              Thursday, March 10, 2005
 *
 * Purpose:	Block tracker testing functions.
 */

#define H5BT_PACKAGE		/*suppress error about including H5BTpkg  */
#define H5BT_TESTING		/*suppress warning about H5BT testing funcs*/

/* Private headers */
#include "H5private.h"		/* Generic Functions			*/
#include "H5BTpkg.h"		/* Block tracker			*/
#include "H5Eprivate.h"		/* Error handling		  	*/

/* Static Prototypes */

/* Package variables */


/*-------------------------------------------------------------------------
 * Function:	H5B2_get_max_info
 *
 * Purpose:	Retrieve the information about the maximum size block tracked
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Thursday, March 10, 2005
 *
 * Modifications:
 *
 *              John Mainzer, 6/17/05
 *              Modified the function to use the new dirtied parameter of
 *              of H5AC_unprotect() instead of modifying the is_dirty
 *              field of the cache info.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5BT_get_max_info(H5F_t *f, hid_t dxpl_id, haddr_t addr, hsize_t *size,
    uint32_t *count, hbool_t *valid)
{
    H5BT_t	*bt=NULL;               /* Pointer to the block tracker info */
    herr_t	ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT(H5BT_get_max_info)

    /* Check arguments. */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(size);
    HDassert(count);
    HDassert(valid);

    /* Look up the block tracker header */
    if (NULL == (bt = H5AC_protect(f, dxpl_id, H5AC_BLTR, addr, NULL, NULL, H5AC_READ)))
	HGOTO_ERROR(H5E_BLKTRK, H5E_CANTPROTECT, FAIL, "unable to load block tracker info")

    /* Save max. block size info */
    *size = bt->max_block_size;
    *count = bt->max_block_cnt;
    *valid = (bt->status & H5BT_STATUS_MAX_VALID) > 0;

done:
    /* Release the block tracker info */
    if (bt && H5AC_unprotect(f, dxpl_id, H5AC_BLTR, addr, bt, H5AC__NO_FLAGS_SET) < 0)
        HDONE_ERROR(H5E_BLKTRK, H5E_CANTUNPROTECT, FAIL, "unable to release block tracker info")

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5BT_get_max_info() */


/*-------------------------------------------------------------------------
 * Function:	H5B2_get_min_info
 *
 * Purpose:	Retrieve the information about the minimum size block tracked
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Thursday, March 10, 2005
 *
 * Modifications:
 *
 *              John Mainzer, 6/17/05
 *              Modified the function to use the new dirtied parameter of
 *              of H5AC_unprotect() instead of modifying the is_dirty
 *              field of the cache info.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5BT_get_min_info(H5F_t *f, hid_t dxpl_id, haddr_t addr, hsize_t *size,
    uint32_t *count, hbool_t *valid)
{
    H5BT_t	*bt=NULL;               /* Pointer to the block tracker info */
    herr_t	ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT(H5BT_get_min_info)

    /* Check arguments. */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(size);
    HDassert(count);
    HDassert(valid);

    /* Look up the block tracker header */
    if (NULL == (bt = H5AC_protect(f, dxpl_id, H5AC_BLTR, addr, NULL, NULL, H5AC_READ)))
	HGOTO_ERROR(H5E_BLKTRK, H5E_CANTPROTECT, FAIL, "unable to load block tracker info")

    /* Save min. block size info */
    *size = bt->min_block_size;
    *count = bt->min_block_cnt;
    *valid = (bt->status & H5BT_STATUS_MIN_VALID) > 0;

done:
    /* Release the block tracker info */
    if (bt && H5AC_unprotect(f, dxpl_id, H5AC_BLTR, addr, bt, H5AC__NO_FLAGS_SET) < 0)
        HDONE_ERROR(H5E_BLKTRK, H5E_CANTUNPROTECT, FAIL, "unable to release block tracker info")

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5BT_get_min_info() */

