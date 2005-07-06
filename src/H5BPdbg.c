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
 * Created:		H5BPdbg.c
 *			Apr 19 2005
 *			Quincey Koziol <koziol@ncsa.uiuc.edu>
 *
 * Purpose:		Dump debugging information about a B+ tree.
 *
 *-------------------------------------------------------------------------
 */

#define H5BP_PACKAGE		/*suppress error about including H5BPpkg  */

/* Private headers */
#include "H5private.h"		/* Generic Functions			*/
#include "H5BPpkg.h"		/* B+ trees				*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5FLprivate.h"	/* Free Lists                           */


/*-------------------------------------------------------------------------
 * Function:	H5BP_hdr_debug
 *
 * Purpose:	Prints debugging info about a B+ tree header.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Apr 19 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5BP_hdr_debug(H5F_t *f, hid_t dxpl_id, haddr_t addr, FILE *stream, int indent, int fwidth,
    const H5BP_class_t *type)
{
    H5BP_t	*bpt = NULL;
    H5BP_shared_t 	*shared;      /* Shared B+ tree information */
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5BP_hdr_debug, FAIL)

    /*
     * Check arguments.
     */
    assert(f);
    assert(H5F_addr_defined(addr));
    assert(stream);
    assert(indent >= 0);
    assert(fwidth >= 0);

    /*
     * Load the B+ tree header.
     */
    if (NULL == (bpt = H5AC_protect(f, dxpl_id, H5AC_BPT_HDR, addr, type, NULL, H5AC_READ)))
	HGOTO_ERROR(H5E_BTREE, H5E_CANTLOAD, FAIL, "unable to load B+ tree header")

    /* Get the pointer to the shared B+ tree info */
    shared=H5RC_GET_OBJ(bpt->shared);
    assert(shared);

    /*
     * Print the values.
     */
    HDfprintf(stream, "%*s%-*s %s\n", indent, "", fwidth,
	      "Tree type ID:",
	      (shared->type->id == H5BP_TEST_ID ? "H5BP_TEST_ID" :
              "Unknown!"));
    HDfprintf(stream, "%*s%-*s %Zu\n", indent, "", fwidth,
	      "Size of node:",
	      shared->node_size);
    HDfprintf(stream, "%*s%-*s %s\n", indent, "", fwidth,
	      "Dirty flag:",
	      bpt->cache_info.is_dirty ? "True" : "False");
    HDfprintf(stream, "%*s%-*s %u\n", indent, "", fwidth,
	      "Depth:",
	      bpt->depth);
    HDfprintf(stream, "%*s%-*s %Hu\n", indent, "", fwidth,
	      "Number of records in tree:",
	      bpt->root.all_nrec);
    if(H5BP_NODE_OVERSIZE(bpt->root.util))
        HDfprintf(stream, "%*s%-*s %u\n", indent, "", fwidth,
                  "Root node size multiplier:",
                  (unsigned)(bpt->root.util & 0x7f));
    else
        HDfprintf(stream, "%*s%-*s %u\n", indent, "", fwidth,
                  "Root node size utilization:",
                  (unsigned)bpt->root.util);
    HDfprintf(stream, "%*s%-*s %a\n", indent, "", fwidth,
	      "Address of root node:",
	      bpt->root.addr);
    HDfprintf(stream, "%*s%-*s %u\n", indent, "", fwidth,
	      "Split percent:",
	      shared->split_percent);
    HDfprintf(stream, "%*s%-*s %u\n", indent, "", fwidth,
	      "Merge percent:",
	      shared->merge_percent);

done:
    if (bpt && H5AC_unprotect(f, dxpl_id, H5AC_BPT_HDR, addr, bpt, H5AC__NO_FLAGS_SET) < 0)
        HDONE_ERROR(H5E_BTREE, H5E_PROTECT, FAIL, "unable to release B+ tree header")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5BP_hdr_debug() */

