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
 * Created:		H5BTdbg.c
 *			Mar 10 2005
 *			Quincey Koziol <koziol@ncsa.uiuc.edu>
 *
 * Purpose:		Dump debugging information about a block tracker
 *
 *-------------------------------------------------------------------------
 */

#define H5BT_PACKAGE		/*suppress error about including H5BTpkg  */

/* Private headers */
#include "H5private.h"		/* Generic Functions			*/
#include "H5BTpkg.h"		/* Block tracker			*/
#include "H5Eprivate.h"		/* Error handling		  	*/


/*-------------------------------------------------------------------------
 * Function:	H5BT_debug
 *
 * Purpose:	Prints debugging info about a block tracker info
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Mar 10 2005
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
H5BT_hdr_debug(H5F_t *f, hid_t dxpl_id, haddr_t addr, FILE *stream, int indent, int fwidth)
{
    H5BT_t	*bt = NULL;
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5BT_hdr_debug, FAIL)

    /*
     * Check arguments.
     */
    assert(f);
    assert(H5F_addr_defined(addr));
    assert(stream);
    assert(indent >= 0);
    assert(fwidth >= 0);

    /*
     * Load the block tracker info
     */
    if (NULL == (bt = H5AC_protect(f, dxpl_id, H5AC_BLTR, addr, NULL, NULL, H5AC_READ)))
	HGOTO_ERROR(H5E_BTREE, H5E_CANTLOAD, FAIL, "unable to load block tracker info")

    /*
     * Print the values.
     */
    HDfprintf(stream, "%*s%-*s %s\n", indent, "", fwidth,
	      "Dirty flag:",
	      bt->cache_info.is_dirty ? "True" : "False");
    HDfprintf(stream, "%*s%-*s %x\n", indent, "", fwidth,
	      "Status:",
	      (unsigned)bt->status);
    HDfprintf(stream, "%*s%-*s %Hu\n", indent, "", fwidth,
	      "Max. Size of Block Tracked:",
	      bt->max_block_size);
    HDfprintf(stream, "%*s%-*s %u\n", indent, "", fwidth,
	      "Ref. Count of Max. Block Tracked:",
	      (unsigned)bt->max_block_cnt);
    HDfprintf(stream, "%*s%-*s %Hu\n", indent, "", fwidth,
	      "Min. Size of Block Tracked:",
	      bt->min_block_size);
    HDfprintf(stream, "%*s%-*s %u\n", indent, "", fwidth,
	      "Ref. Count of Min. Block Tracked:",
	      (unsigned)bt->min_block_cnt);
    HDfprintf(stream, "%*s%-*s %Hu\n", indent, "", fwidth,
	      "Total Size of All Blocks Tracked:",
	      bt->tot_block_size);
    HDfprintf(stream, "%*s%-*s %a\n", indent, "", fwidth,
	      "Address of v2 B-tree Storing Block Info Records:",
	      bt->bt2_addr);

done:
    if (bt && H5AC_unprotect(f, dxpl_id, H5AC_BLTR, addr, bt, H5AC__NO_FLAGS_SET) < 0)
        HDONE_ERROR(H5E_BTREE, H5E_PROTECT, FAIL, "unable to release block tracker info")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5BT_hdr_debug() */

