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
 * Created:		H5B2dbg.c
 *			Feb  2 2005
 *			Quincey Koziol <koziol@ncsa.uiuc.edu>
 *
 * Purpose:		Dump debugging information about a v2 B-tree.
 *
 *-------------------------------------------------------------------------
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
#include "H5FLprivate.h"	/* Free Lists                           */

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
 * Function:	H5B2_hdr_debug
 *
 * Purpose:	Prints debugging info about a B-tree header.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb  2 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5B2_hdr_debug(H5F_t *f, hid_t dxpl_id, haddr_t addr, FILE *stream, int indent, int fwidth,
    const H5B2_class_t *type)
{
    H5B2_t	*bt2 = NULL;            /* B-tree header info */
    H5B2_shared_t *shared;              /* Shared B-tree information */
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI(H5B2_hdr_debug, FAIL)

    /*
     * Check arguments.
     */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(stream);
    HDassert(indent >= 0);
    HDassert(fwidth >= 0);
    HDassert(type);

    /*
     * Load the B-tree header.
     */
    if(NULL == (bt2 = H5AC_protect(f, dxpl_id, H5AC_BT2_HDR, addr, type, NULL, H5AC_READ)))
	HGOTO_ERROR(H5E_BTREE, H5E_CANTLOAD, FAIL, "unable to load B-tree header")

    /* Get the pointer to the shared B-tree info */
    shared = H5RC_GET_OBJ(bt2->shared);
    HDassert(shared);

    /* Print opening message */
    HDfprintf(stream, "%*sv2 B-tree Header...\n", indent, "");

    /*
     * Print the values.
     */
    HDfprintf(stream, "%*s%-*s %s\n", indent, "", fwidth,
	      "Tree type ID:",
	      ((shared->type->id) == H5B2_TEST_ID ? "H5B2_TEST_ID" :
              "Unknown!"));
    HDfprintf(stream, "%*s%-*s %Zu\n", indent, "", fwidth,
	      "Size of node:",
	      shared->node_size);
    HDfprintf(stream, "%*s%-*s %Zu\n", indent, "", fwidth,
	      "Size of raw (disk) record:",
	      shared->rrec_size);
    HDfprintf(stream, "%*s%-*s %s\n", indent, "", fwidth,
	      "Dirty flag:",
	      bt2->cache_info.is_dirty ? "True" : "False");
    HDfprintf(stream, "%*s%-*s %u\n", indent, "", fwidth,
	      "Depth:",
	      bt2->depth);
    HDfprintf(stream, "%*s%-*s %Hu\n", indent, "", fwidth,
	      "Number of records in tree:",
	      bt2->root.all_nrec);
    HDfprintf(stream, "%*s%-*s %u\n", indent, "", fwidth,
	      "Number of records in root node:",
	      bt2->root.node_nrec);
    HDfprintf(stream, "%*s%-*s %a\n", indent, "", fwidth,
	      "Address of root node:",
	      bt2->root.addr);
    HDfprintf(stream, "%*s%-*s %Zu\n", indent, "", fwidth,
	      "Max. number of records per internal node:",
	      shared->internal_nrec);
    HDfprintf(stream, "%*s%-*s %Zu\n", indent, "", fwidth,
	      "Max. number of records per leaf node:",
	      shared->leaf_nrec);
    HDfprintf(stream, "%*s%-*s %u\n", indent, "", fwidth,
	      "Split percent:",
	      shared->split_percent);
    HDfprintf(stream, "%*s%-*s %u\n", indent, "", fwidth,
	      "Merge percent:",
	      shared->merge_percent);
    HDfprintf(stream, "%*s%-*s %Zu\n", indent, "", fwidth,
	      "Internal records to split at:",
	      shared->split_int_nrec);
    HDfprintf(stream, "%*s%-*s %Zu\n", indent, "", fwidth,
	      "Leaf records to split at:",
	      shared->split_leaf_nrec);
    HDfprintf(stream, "%*s%-*s %Zu\n", indent, "", fwidth,
	      "Internal records to merge at:",
	      shared->merge_int_nrec);
    HDfprintf(stream, "%*s%-*s %Zu\n", indent, "", fwidth,
	      "Leaf records to merge at:",
	      shared->merge_leaf_nrec);

done:
    if(bt2 && H5AC_unprotect(f, dxpl_id, H5AC_BT2_HDR, addr, bt2, H5AC__NO_FLAGS_SET) < 0)
        HDONE_ERROR(H5E_BTREE, H5E_PROTECT, FAIL, "unable to release B-tree header")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5B2_hdr_debug() */


/*-------------------------------------------------------------------------
 * Function:	H5B2_int_debug
 *
 * Purpose:	Prints debugging info about a B-tree internal node
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb  4 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5B2_int_debug(H5F_t *f, hid_t dxpl_id, haddr_t addr, FILE *stream, int indent, int fwidth,
    const H5B2_class_t *type, haddr_t hdr_addr, unsigned nrec)
{
    H5B2_t	*bt2 = NULL;
    H5B2_internal_t	*internal = NULL;
    H5B2_shared_t 	*shared;        /* Shared B-tree information */
    unsigned		u;              /* Local index variable */
    char                temp_str[128];  /* Temporary string, for formatting */
    herr_t      ret_value=SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(H5B2_int_debug, FAIL)

    /*
     * Check arguments.
     */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(stream);
    HDassert(indent >= 0);
    HDassert(fwidth >= 0);
    HDassert(type);
    HDassert(H5F_addr_defined(hdr_addr));
    HDassert(nrec > 0);

    /*
     * Load the B-tree header.
     */
    if(NULL == (bt2 = H5AC_protect(f, dxpl_id, H5AC_BT2_HDR, hdr_addr, type, NULL, H5AC_READ)))
	HGOTO_ERROR(H5E_BTREE, H5E_CANTLOAD, FAIL, "unable to load B-tree header")

    /* Get the pointer to the shared B-tree info */
    shared = H5RC_GET_OBJ(bt2->shared);
    HDassert(shared);

    /*
     * Load the B-tree internal node
     */
    if(NULL == (internal = H5AC_protect(f, dxpl_id, H5AC_BT2_INT, addr, &nrec, bt2->shared, H5AC_READ)))
	HGOTO_ERROR(H5E_BTREE, H5E_CANTLOAD, FAIL, "unable to load B-tree internal node")

    /* Release the B-tree header */
    if(H5AC_unprotect(f, dxpl_id, H5AC_BT2_HDR, hdr_addr, bt2, H5AC__NO_FLAGS_SET) < 0)
        HDONE_ERROR(H5E_BTREE, H5E_PROTECT, FAIL, "unable to release B-tree header")
    bt2 = NULL;

    /* Print opening message */
    HDfprintf(stream, "%*sv2 B-tree Internal Node...\n", indent, "");

    /*
     * Print the values.
     */
    HDfprintf(stream, "%*s%-*s %s\n", indent, "", fwidth,
	      "Tree type ID:",
	      ((shared->type->id)==H5B2_TEST_ID ? "H5B2_TEST_ID" :
              "Unknown!"));
    HDfprintf(stream, "%*s%-*s %Zu\n", indent, "", fwidth,
	      "Size of node:",
	      shared->node_size);
    HDfprintf(stream, "%*s%-*s %Zu\n", indent, "", fwidth,
	      "Size of raw (disk) record:",
	      shared->rrec_size);
    HDfprintf(stream, "%*s%-*s %s\n", indent, "", fwidth,
	      "Dirty flag:",
	      internal->cache_info.is_dirty ? "True" : "False");
    HDfprintf(stream, "%*s%-*s %u\n", indent, "", fwidth,
	      "Number of records in node:",
	      internal->nrec);

    /* Print all node pointers and records */
    for(u = 0; u < internal->nrec; u++) {
        /* Print node pointer */
        sprintf(temp_str, "Node pointer #%u: (all/node/addr)", u);
        HDfprintf(stream, "%*s%-*s (%Hu/%u/%a)\n", indent + 3, "", MAX(0, fwidth - 3),
                  temp_str,
                  internal->node_ptrs[u].all_nrec,
                  internal->node_ptrs[u].node_nrec,
                  internal->node_ptrs[u].addr);

        /* Print record */
        sprintf(temp_str, "Record #%u:", u);
        HDfprintf(stream, "%*s%-*s\n", indent + 3, "", MAX(0, fwidth - 3),
                  temp_str);
        HDassert(H5B2_INT_NREC(internal, shared, u));
        (void)(type->debug)(stream, f, dxpl_id, indent + 6, MAX (0, fwidth-6),
            shared->type, H5B2_INT_NREC(internal,shared,u), NULL);
    } /* end for */

    /* Print final node pointer */
    sprintf(temp_str, "Node pointer #%u: (all/node/addr)", u);
    HDfprintf(stream, "%*s%-*s (%Hu/%u/%a)\n", indent + 3, "", MAX(0, fwidth - 3),
              temp_str,
              internal->node_ptrs[u].all_nrec,
              internal->node_ptrs[u].node_nrec,
              internal->node_ptrs[u].addr);

done:
    if(internal && H5AC_unprotect(f, dxpl_id, H5AC_BT2_INT, addr, internal, H5AC__NO_FLAGS_SET) < 0)
        HDONE_ERROR(H5E_BTREE, H5E_PROTECT, FAIL, "unable to release B-tree internal node")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5B2_int_debug() */


/*-------------------------------------------------------------------------
 * Function:	H5B2_leaf_debug
 *
 * Purpose:	Prints debugging info about a B-tree leaf node
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb  7 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5B2_leaf_debug(H5F_t *f, hid_t dxpl_id, haddr_t addr, FILE *stream, int indent, int fwidth,
    const H5B2_class_t *type, haddr_t hdr_addr, unsigned nrec)
{
    H5B2_t	*bt2 = NULL;
    H5B2_leaf_t	*leaf = NULL;
    H5B2_shared_t 	*shared;        /* Shared B-tree information */
    unsigned		u;              /* Local index variable */
    char                temp_str[128];  /* Temporary string, for formatting */
    herr_t      ret_value=SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(H5B2_leaf_debug, FAIL)

    /*
     * Check arguments.
     */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(stream);
    HDassert(indent >= 0);
    HDassert(fwidth >= 0);
    HDassert(type);
    HDassert(H5F_addr_defined(hdr_addr));
    HDassert(nrec > 0);

    /*
     * Load the B-tree header.
     */
    if(NULL == (bt2 = H5AC_protect(f, dxpl_id, H5AC_BT2_HDR, hdr_addr, type, NULL, H5AC_READ)))
	HGOTO_ERROR(H5E_BTREE, H5E_CANTLOAD, FAIL, "unable to load B-tree header")

    /* Get the pointer to the shared B-tree info */
    shared = H5RC_GET_OBJ(bt2->shared);
    HDassert(shared);

    /*
     * Load the B-tree leaf node
     */
    if(NULL == (leaf = H5AC_protect(f, dxpl_id, H5AC_BT2_LEAF, addr, &nrec, bt2->shared, H5AC_READ)))
	HGOTO_ERROR(H5E_BTREE, H5E_CANTLOAD, FAIL, "unable to load B-tree leaf node")

    /* Release the B-tree header */
    if(H5AC_unprotect(f, dxpl_id, H5AC_BT2_HDR, hdr_addr, bt2, H5AC__NO_FLAGS_SET) < 0)
        HDONE_ERROR(H5E_BTREE, H5E_PROTECT, FAIL, "unable to release B-tree header")
    bt2 = NULL;

    /* Print opening message */
    HDfprintf(stream, "%*sv2 B-tree Leaf Node...\n", indent, "");

    /*
     * Print the values.
     */
    HDfprintf(stream, "%*s%-*s %s\n", indent, "", fwidth,
	      "Tree type ID:",
	      ((shared->type->id)==H5B2_TEST_ID ? "H5B2_TEST_ID" :
              "Unknown!"));
    HDfprintf(stream, "%*s%-*s %Zu\n", indent, "", fwidth,
	      "Size of node:",
	      shared->node_size);
    HDfprintf(stream, "%*s%-*s %Zu\n", indent, "", fwidth,
	      "Size of raw (disk) record:",
	      shared->rrec_size);
    HDfprintf(stream, "%*s%-*s %s\n", indent, "", fwidth,
	      "Dirty flag:",
	      leaf->cache_info.is_dirty ? "True" : "False");
    HDfprintf(stream, "%*s%-*s %u\n", indent, "", fwidth,
	      "Number of records in node:",
	      leaf->nrec);

    /* Print all node pointers and records */
    for(u = 0; u < leaf->nrec; u++) {
        /* Print record */
        sprintf(temp_str, "Record #%u:", u);
        HDfprintf(stream, "%*s%-*s\n", indent + 3, "", MAX(0, fwidth - 3),
                  temp_str);
        HDassert(H5B2_LEAF_NREC(leaf, shared, u));
        (void)(type->debug)(stream, f, dxpl_id, indent+6, MAX (0, fwidth-6),
            shared->type, H5B2_LEAF_NREC(leaf,shared,u), NULL);
    } /* end for */

done:
    if(leaf && H5AC_unprotect(f, dxpl_id, H5AC_BT2_LEAF, addr, leaf, H5AC__NO_FLAGS_SET) < 0)
        HDONE_ERROR(H5E_BTREE, H5E_PROTECT, FAIL, "unable to release B-tree leaf node")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5B2_leaf_debug() */

