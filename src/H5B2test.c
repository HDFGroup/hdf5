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
 *              Thursday, February  3, 2005
 *
 * Purpose:	v2 B-tree testing functions.
 *
 */

/****************/
/* Module Setup */
/****************/

#define H5B2_PACKAGE		/*suppress error about including H5B2pkg  */
#define H5B2_TESTING		/*suppress warning about H5B2 testing funcs*/

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
static herr_t H5B2_test_store(void *nrecord, const void *udata);
static herr_t H5B2_test_retrieve(void *udata, const void *nrecord);
static herr_t H5B2_test_compare(const void *rec1, const void *rec2);
static herr_t H5B2_test_encode(const H5F_t *f, uint8_t *raw,
    const void *nrecord);
static herr_t H5B2_test_decode(const H5F_t *f, const uint8_t *raw,
    void *nrecord);
static herr_t H5B2_test_debug(FILE *stream, const H5F_t *f, hid_t dxpl_id,
    int indent, int fwidth, const void *record, const void *_udata);

/*********************/
/* Package Variables */
/*********************/
const H5B2_class_t H5B2_TEST[1]={{   /* B-tree class information */
    H5B2_TEST_ID,               /* Type of B-tree */
    sizeof(hsize_t),            /* Size of native record */
    H5B2_test_store,            /* Record storage callback */
    H5B2_test_retrieve,         /* Record retrieval callback */
    H5B2_test_compare,          /* Record comparison callback */
    H5B2_test_encode,           /* Record encoding callback */
    H5B2_test_decode,           /* Record decoding callback */
    H5B2_test_debug             /* Record debugging callback */
}};

/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/


/*-------------------------------------------------------------------------
 * Function:	H5B2_test_store
 *
 * Purpose:	Store native information into record for B-tree
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Thursday, February  3, 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5B2_test_store(void *nrecord, const void *udata)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5B2_test_store)

    *(hsize_t *)nrecord = *(const hsize_t *)udata;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5B2_test_store() */


/*-------------------------------------------------------------------------
 * Function:	H5B2_test_retrieve
 *
 * Purpose:	Retrieve native information from record for B-tree
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Friday, February 25, 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5B2_test_retrieve(void *udata, const void *nrecord)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5B2_test_retrieve)

    *(hsize_t *)udata = *(const hsize_t *)nrecord;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5B2_test_retrieve() */


/*-------------------------------------------------------------------------
 * Function:	H5B2_test_compare
 *
 * Purpose:	Compare two native information records, according to some key
 *
 * Return:	<0 if rec1 < rec2
 *              =0 if rec1 == rec2
 *              >0 if rec1 > rec2
 *
 * Programmer:	Quincey Koziol
 *              Thursday, February  3, 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5B2_test_compare(const void *rec1, const void *rec2)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5B2_test_compare)

    FUNC_LEAVE_NOAPI((herr_t)(*(const hssize_t *)rec1-*(const hssize_t *)rec2))
} /* H5B2_test_compare() */


/*-------------------------------------------------------------------------
 * Function:	H5B2_test_encode
 *
 * Purpose:	Encode native information into raw form for storing on disk
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Thursday, February  3, 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5B2_test_encode(const H5F_t *f, uint8_t *raw, const void *nrecord)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5B2_test_encode)

    H5F_ENCODE_LENGTH(f, raw, *(const hsize_t *)nrecord);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5B2_test_encode() */


/*-------------------------------------------------------------------------
 * Function:	H5B2_test_decode
 *
 * Purpose:	Decode raw disk form of record into native form
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Friday, February  4, 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5B2_test_decode(const H5F_t *f, const uint8_t *raw, void *nrecord)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5B2_test_decode)

    H5F_DECODE_LENGTH(f, raw, *(hsize_t *)nrecord);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5B2_test_decode() */


/*-------------------------------------------------------------------------
 * Function:	H5B2_test_debug
 *
 * Purpose:	Debug native form of record
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Friday, February  4, 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5B2_test_debug(FILE *stream, const H5F_t UNUSED *f, hid_t UNUSED dxpl_id,
    int indent, int fwidth, const void *record,
    const void UNUSED *_udata)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5B2_test_debug)

    HDassert (record);

    HDfprintf(stream, "%*s%-*s %Hu\n", indent, "", fwidth, "Record:",
        *(const hsize_t *)record);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5B2_test_debug() */


/*-------------------------------------------------------------------------
 * Function:	H5B2_get_root_addr_test
 *
 * Purpose:	Retrieve the root node's address
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Saturday, February 26, 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5B2_get_root_addr_test(H5F_t *f, hid_t dxpl_id, const H5B2_class_t *type,
    haddr_t addr, haddr_t *root_addr)
{
    H5B2_t	*bt2 = NULL;            /* Pointer to the B-tree header */
    herr_t	ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5B2_get_root_addr_test)

    /* Check arguments. */
    HDassert(f);
    HDassert(type);
    HDassert(H5F_addr_defined(addr));
    HDassert(root_addr);

    /* Look up the B-tree header */
    if(NULL == (bt2 = H5AC_protect(f, dxpl_id, H5AC_BT2_HDR, addr, type, NULL, H5AC_READ)))
	HGOTO_ERROR(H5E_BTREE, H5E_CANTPROTECT, FAIL, "unable to load B-tree header")

    /* Get B-tree root addr */
    *root_addr = bt2->root.addr;

done:
    /* Release B-tree header node */
    if(bt2 && H5AC_unprotect(f, dxpl_id, H5AC_BT2_HDR, addr, bt2, H5AC__NO_FLAGS_SET) < 0)
        HDONE_ERROR(H5E_BTREE, H5E_CANTUNPROTECT, FAIL, "unable to release B-tree header info")

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5B2_get_root_addr_test() */


/*-------------------------------------------------------------------------
 * Function:	H5B2_get_node_info_test
 *
 * Purpose:	Determine information about a node holding a record in the B-tree
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Thursday, August 31, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5B2_get_node_info_test(H5F_t *f, hid_t dxpl_id, const H5B2_class_t *type, haddr_t addr,
    void *udata, H5B2_node_info_test_t *ninfo)
{
    H5B2_t	*bt2=NULL;              /* Pointer to the B-tree header */
    H5RC_t      *bt2_shared=NULL;       /* Pointer to ref-counter for shared B-tree info */
    H5B2_shared_t *shared;              /* Pointer to B-tree's shared information */
    hbool_t     incr_rc=FALSE;          /* Flag to indicate that we've incremented the B-tree's shared info reference count */
    H5B2_node_ptr_t curr_node_ptr;      /* Node pointer info for current node */
    unsigned    depth;                  /* Current depth of the tree */
    int         cmp;                    /* Comparison value of records */
    unsigned    idx;                    /* Location of record which matches key */
    herr_t	ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI(H5B2_get_node_info_test, FAIL)

    /* Check arguments. */
    HDassert(f);
    HDassert(type);
    HDassert(H5F_addr_defined(addr));

    /* Look up the B-tree header */
    if (NULL == (bt2 = H5AC_protect(f, dxpl_id, H5AC_BT2_HDR, addr, type, NULL, H5AC_READ)))
	HGOTO_ERROR(H5E_BTREE, H5E_CANTPROTECT, FAIL, "unable to load B-tree header")

    /* Safely grab pointer to reference counted shared B-tree info, so we can release the B-tree header if necessary */
    bt2_shared=bt2->shared;
    H5RC_INC(bt2_shared);
    incr_rc=TRUE;

    /* Get the pointer to the shared B-tree info */
    shared = H5RC_GET_OBJ(bt2_shared);
    HDassert(shared);

    /* Make copy of the root node pointer to start search with */
    curr_node_ptr = bt2->root;

    /* Current depth of the tree */
    depth = shared->depth;

    /* Release header */
    if(H5AC_unprotect(f, dxpl_id, H5AC_BT2_HDR, addr, bt2, H5AC__NO_FLAGS_SET) < 0)
        HGOTO_ERROR(H5E_BTREE, H5E_CANTUNPROTECT, FAIL, "unable to release B-tree header info")
    bt2 = NULL;

    /* Check for empty tree */
    if(curr_node_ptr.node_nrec==0)
        HGOTO_ERROR(H5E_BTREE, H5E_NOTFOUND, FAIL, "B-tree has no records")

    /* Walk down B-tree to find record or leaf node where record is located */
    cmp = -1;
    while(depth > 0 && cmp != 0) {
        H5B2_internal_t *internal;          /* Pointer to internal node in B-tree */
        H5B2_node_ptr_t next_node_ptr;      /* Node pointer info for next node */

        /* Lock B-tree current node */
        if(NULL == (internal = H5B2_protect_internal(f, dxpl_id, bt2_shared, curr_node_ptr.addr, curr_node_ptr.node_nrec, depth, H5AC_READ)))
            HGOTO_ERROR(H5E_BTREE, H5E_CANTPROTECT, FAIL, "unable to load B-tree internal node")

        /* Locate node pointer for child */
        cmp = H5B2_locate_record(shared->type, internal->nrec, shared->nat_off, internal->int_native, udata, &idx);
        if(cmp > 0)
            idx++;

        if(cmp != 0) {
            /* Get node pointer for next node to search */
            next_node_ptr = internal->node_ptrs[idx];

            /* Unlock current node */
            if(H5AC_unprotect(f, dxpl_id, H5AC_BT2_INT, curr_node_ptr.addr, internal, H5AC__NO_FLAGS_SET) < 0)
                HGOTO_ERROR(H5E_BTREE, H5E_CANTUNPROTECT, FAIL, "unable to release B-tree node")

            /* Set pointer to next node to load */
            curr_node_ptr = next_node_ptr;
        } /* end if */
        else {
            /* Unlock current node */
            if(H5AC_unprotect(f, dxpl_id, H5AC_BT2_INT, curr_node_ptr.addr, internal, H5AC__NO_FLAGS_SET) < 0)
                HGOTO_ERROR(H5E_BTREE, H5E_CANTUNPROTECT, FAIL, "unable to release B-tree node")

            /* Fill in information about the node */
            ninfo->depth = depth;
            ninfo->nrec = curr_node_ptr.node_nrec;

            /* Indicate success */
            HGOTO_DONE(SUCCEED)
        } /* end else */

        /* Decrement depth we're at in B-tree */
        depth--;
    } /* end while */

    {
        H5B2_leaf_t *leaf;          /* Pointer to leaf node in B-tree */

        /* Lock B-tree leaf node */
        if(NULL == (leaf = H5AC_protect(f, dxpl_id, H5AC_BT2_LEAF, curr_node_ptr.addr, &(curr_node_ptr.node_nrec), bt2_shared, H5AC_READ)))
            HGOTO_ERROR(H5E_BTREE, H5E_CANTPROTECT, FAIL, "unable to load B-tree internal node")

        /* Locate record */
        cmp = H5B2_locate_record(shared->type, leaf->nrec, shared->nat_off, leaf->leaf_native, udata, &idx);

        /* Unlock current node */
        if(H5AC_unprotect(f, dxpl_id, H5AC_BT2_LEAF, curr_node_ptr.addr, leaf, H5AC__NO_FLAGS_SET) < 0)
            HGOTO_ERROR(H5E_BTREE, H5E_CANTUNPROTECT, FAIL, "unable to release B-tree node")

        /* Indicate the depth that the record was found */
        if(cmp != 0)
            HGOTO_ERROR(H5E_BTREE, H5E_NOTFOUND, FAIL, "record not in B-tree")
    } /* end block */

    /* Fill in information about the leaf node */
    ninfo->depth = depth;
    ninfo->nrec = curr_node_ptr.node_nrec;

done:
    /* Check if we need to decrement the reference count for the B-tree's shared info */
    if(incr_rc)
        H5RC_DEC(bt2_shared);

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5B2_get_node_info_test() */


/*-------------------------------------------------------------------------
 * Function:	H5B2_get_node_depth_test
 *
 * Purpose:	Determine the depth of a node holding a record in the B-tree
 *
 * Note:	Just a simple wrapper around the H5B2_get_node_info_test() routine
 *
 * Return:	Success:	non-negative depth of the node where the record
 *                              was found
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Saturday, August 26, 2006
 *
 *-------------------------------------------------------------------------
 */
int
H5B2_get_node_depth_test(H5F_t *f, hid_t dxpl_id, const H5B2_class_t *type, haddr_t addr,
    void *udata)
{
    H5B2_node_info_test_t ninfo;        /* Node information */
    int		ret_value;              /* Return information */

    FUNC_ENTER_NOAPI(H5B2_get_node_depth_test, FAIL)

    /* Check arguments. */
    HDassert(f);
    HDassert(type);
    HDassert(H5F_addr_defined(addr));

    /* Get information abou the node */
    if(H5B2_get_node_info_test(f, dxpl_id, type, addr, udata, &ninfo) < 0)
        HGOTO_ERROR(H5E_BTREE, H5E_NOTFOUND, FAIL, "error looking up node info")

    /* Set return value */
    ret_value = ninfo.depth;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5B2_get_node_depth_test() */

