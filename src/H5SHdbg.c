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
 * Created:		H5SHdbg.c
 *			Mar 24 2005
 *			Quincey Koziol <koziol@ncsa.uiuc.edu>
 *
 * Purpose:		Dump debugging information about a segmented heap
 *
 *-------------------------------------------------------------------------
 */

#define H5SH_PACKAGE		/*suppress error about including H5SHpkg  */

/* Private headers */
#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5SHpkg.h"		/* Segmented heap			*/


/*-------------------------------------------------------------------------
 * Function:	H5SH_debug
 *
 * Purpose:	Prints debugging info about a segmented heap
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Mar 24 2005
 *
 * Modifications:
 *
 *              John Mainzer, 6/16/05
 *              Modified the function to use the new dirtied parameter of
 *              of H5AC_unprotect() instead of modifying the is_dirty
 *              field of the cache info.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5SH_debug(H5F_t *f, hid_t dxpl_id, haddr_t addr, FILE *stream, int indent, int fwidth)
{
    hbool_t	sh_dirtied = FALSE;
    H5SH_t	*sh = NULL;
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5SH_debug, FAIL)

    /*
     * Check arguments.
     */
    assert(f);
    assert(H5F_addr_defined(addr));
    assert(stream);
    assert(indent >= 0);
    assert(fwidth >= 0);

    /*
     * Load the segmented heap info
     */
    if (NULL == (sh = H5AC_protect(f, dxpl_id, H5AC_SGHP, addr, NULL, NULL, H5AC_READ)))
	HGOTO_ERROR(H5E_BTREE, H5E_CANTLOAD, FAIL, "unable to load segmented heap info")

    /*
     * Print the values.
     */
    HDfprintf(stream, "%*s%-*s %s\n", indent, "", fwidth,
	      "Dirty flag:",
	      sh->cache_info.is_dirty ? "True" : "False");
    HDfprintf(stream, "%*s%-*s %Hu\n", indent, "", fwidth,
	      "Minimum Size Of Heap Blocks:",
	      sh->min_size);
    HDfprintf(stream, "%*s%-*s %Hu\n", indent, "", fwidth,
	      "Maximum Size To Extend Heap Blocks:",
	      sh->max_extend_size);
    HDfprintf(stream, "%*s%-*s %a\n", indent, "", fwidth,
	      "Address of Block Tracker For Heap Blocks:",
	      sh->bt_heap_addr);
    HDfprintf(stream, "%*s%-*s %a\n", indent, "", fwidth,
	      "Address of Block Tracker For Free Space:",
	      sh->bt_free_addr);

done:
    if (sh && H5AC_unprotect(f, dxpl_id, H5AC_SGHP, addr, sh, sh_dirtied, H5AC__NO_FLAGS_SET) < 0)
        HDONE_ERROR(H5E_BTREE, H5E_PROTECT, FAIL, "unable to release segmented heap info")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5SH_debug() */


