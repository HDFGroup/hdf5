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

/*
 * Programmer:	Quincey Koziol <koziol@ncsa.uiuc.edu>
 *		Thursday, April 14, 2005
 *
 * Purpose:	This file contains declarations which are visible only within
 *		the H5BP package.  Source files outside the H5BP package should
 *		include H5BPprivate.h instead.
 */
#ifndef H5BP_PACKAGE
#error "Do not include this file outside the H5BP package!"
#endif

#ifndef _H5BPpkg_H
#define _H5BPpkg_H

/* Get package's private header */
#include "H5BPprivate.h"

/* Other private headers needed by this file */
#include "H5ACprivate.h"	/* Metadata cache			*/
#include "H5FLprivate.h"	/* Free Lists                           */
#include "H5RCprivate.h"	/* Reference counted object functions	*/

/**************************/
/* Package Private Macros */
/**************************/

/* Size of signature information (on disk) */
#define H5BP_SIZEOF_MAGIC               4

/* B+ tree signatures */
#define H5BP_HDR_MAGIC                  "BPHD"          /* Header */
#define H5BP_INT_MAGIC                  "BPBR"          /* Branch node */
#define H5BP_TWIG_MAGIC                 "BPTW"          /* Twig node */
#define H5BP_LEAF_MAGIC                 "BPLF"          /* Leaf node */
#define H5BP_EXTENT_MAGIC               "BPEX"          /* Extent node */

/* Size of storage for number of separators per node (on disk) */
#define H5BP_SIZEOF_SEPS_PER_NODE       2

/* Size of storage for "node size multiplier" (on disk) */
#define H5BP_SIZEOF_NODE_MULT           1

/* Size of a "full" node pointer (on disk) */
#define H5BP_NODE_POINTER_SIZE(f)       (H5F_SIZEOF_ADDR(f)+H5F_SIZEOF_SIZE(f)+H5BP_SIZEOF_NODE_MULT)

/* Size of the B+ tree header on disk */
#define H5BP_HEADER_SIZE(f)     (                                             \
    4 + /* Signature */                                                       \
    1 + /* Version */                                                         \
    1 + /* Tree type */                                                       \
    4 + /* Node size, in bytes */                                             \
    2 + /* Depth of tree */                                                   \
    2 + /* Split % of full (as integer, ie. "98" means 98%) */                \
    2 + /* Merge % of full (as integer, ie. "98" means 98%) */                \
    H5BP_NODE_POINTER_SIZE(f))  /* Node pointer to root node in tree */

/* Determine whether a node is oversized */
/* The high bit is set in utilization field for oversized nodes */
#define H5BP_NODE_OVERSIZE(u)   ((u) & 0x80)

/* Determine whether a leaf node has variable-length records */
/* The low bit is set in "flag" field for nodes w/variable-length records */
#define H5BP_LEAF_REC_VARLEN(u)   ((u) & 0x01)

/* Size of memory pool pages for leaf nodes */
#define H5BP_LEAF_POOL_PAGE     H5MP_PAGE_SIZE_DEFAULT


/****************************/
/* Package Private Typedefs */
/****************************/

/* A "node pointer" to another B+ tree node */
typedef struct {
    haddr_t     addr;           /* Address of other node */
    hsize_t     all_nrec;       /* Number of records in node pointed to and all it's children */
    unsigned char util;         /* Percent utilization/page node multiple */
} H5BP_node_ptr_t;

/* Each B+ tree has certain information that can be shared across all
 * the instances of nodes in that B-tree.
 */
typedef struct H5BP_shared_t {
    /* Shared internal data structures */
    const H5BP_class_t	*type;	 /* Type of tree			     */
    uint8_t	        *page;	 /* Disk page */

    /* Information set by user */
    unsigned    split_percent;  /* Percent full at which to split the node, when inserting */
    unsigned    merge_percent;  /* Percent full at which to merge the node, when deleting */
    size_t      node_size;      /* Size of all nodes, in bytes                */
} H5BP_shared_t;

/* The B+ tree information */
typedef struct H5BP_t {
    /* Information for H5AC cache functions, _must_ be first field in structure */
    H5AC_info_t cache_info;

    /* Internal B+ tree information */
    unsigned	depth;		/* B+ tree's overall depth                    */
    H5BP_node_ptr_t root;       /* Node pointer to root node in B+ tree       */
    H5RC_t	*shared;	/* Ref-counted shared info	              */
} H5BP_t;

/* B+ tree internal branch node information */
typedef struct H5BP_branch_t {
    /* Information for H5AC cache functions, _must_ be first field in structure */
    H5AC_info_t cache_info;

    /* Internal B+ tree information */
    H5RC_t	*shared;	/* Ref-counted shared info	              */
    uint8_t     *int_native;    /* Pointer to native records                  */
    H5BP_node_ptr_t *node_ptrs; /* Pointer to node pointers                   */
    unsigned    nrec;           /* Number of records in node                  */
} H5BP_branch_t;

/* B+ tree internal twig node information */
typedef struct H5BP_twig_t {
    /* Information for H5AC cache functions, _must_ be first field in structure */
    H5AC_info_t cache_info;

    /* Internal B+ tree information */
    H5RC_t	*shared;	/* Ref-counted shared info	              */
    uint8_t     *int_native;    /* Pointer to native records                  */
    H5BP_node_ptr_t *node_ptrs; /* Pointer to node pointers                   */
    unsigned    nrec;           /* Number of records in node                  */
} H5BP_twig_t;

/* B+ tree leaf node information */
typedef struct H5BP_leaf_t {
    /* Information for H5AC cache functions, _must_ be first field in structure */
    H5AC_info_t cache_info;

    /* Internal B+ tree information */
    H5RC_t	*shared;	/* Ref-counted shared info	              */
    H5MP_pool_t *rec_pool;      /* Pool containing records for node           */
    void        **rec_ptr;      /* Array of pointers to records in pool       */
    unsigned    nrec;           /* Number of records in node                  */
    unsigned char flags;        /* Status flags for leaf node                 */
} H5BP_leaf_t;

#ifdef H5BP_TESTING
/* Record for testing B+ trees */
typedef struct {
    hsize_t     count;
    char        *name;
} H5BP_test_rec_t;
#endif /* H5BP_TESTING */


/*****************************/
/* Package Private Variables */
/*****************************/

/* H5BP header inherits cache-like properties from H5AC */
H5_DLLVAR const H5AC_class_t H5AC_BPT_HDR[1];

/* H5BP branch node inherits cache-like properties from H5AC */
H5_DLLVAR const H5AC_class_t H5AC_BPT_BRANCH[1];

/* H5BP twig node inherits cache-like properties from H5AC */
H5_DLLVAR const H5AC_class_t H5AC_BPT_TWIG[1];

/* H5BP leaf node inherits cache-like properties from H5AC */
H5_DLLVAR const H5AC_class_t H5AC_BPT_LEAF[1];

/* Declare a free list to manage the H5BP_t struct */
H5FL_EXTERN(H5BP_t);

/* Declare a free list to manage the H5BP_branch_t struct */
H5FL_EXTERN(H5BP_branch_t);

/* Declare a free list to manage the H5BP_twig_t struct */
H5FL_EXTERN(H5BP_twig_t);

/* Declare a free list to manage the H5BP_leaf_t struct */
H5FL_EXTERN(H5BP_leaf_t);

/* Internal B+ tree testing class */
#ifdef H5BP_TESTING
H5_DLLVAR const H5BP_class_t H5BP_TEST[1];
#endif /* H5BP_TESTING */


/******************************/
/* Package Private Prototypes */
/******************************/
H5_DLL herr_t H5BP_shared_free (void *_shared);
H5_DLL herr_t H5BP_shared_init (H5BP_t *bpt, const H5BP_class_t *type,
    size_t node_size, unsigned split_percent, unsigned merge_percent);
H5_DLL herr_t H5BP_cache_hdr_dest(H5F_t *f, H5BP_t *bpt);
H5_DLL herr_t H5BP_cache_leaf_dest(H5F_t UNUSED *f, H5BP_leaf_t *leaf);
H5_DLL herr_t H5BP_hdr_debug(H5F_t *f, hid_t dxpl_id, haddr_t addr,
    FILE *stream, int indent, int fwidth, const H5BP_class_t *type);

#endif /* _H5BPpkg_H */

