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
 *		Monday, January 31, 2005
 *
 * Purpose:	This file contains declarations which are visible only within
 *		the H5B2 package.  Source files outside the H5B2 package should
 *		include H5B2private.h instead.
 */
#ifndef H5B2_PACKAGE
#error "Do not include this file outside the H5B2 package!"
#endif

#ifndef _H5B2pkg_H
#define _H5B2pkg_H

/* Get package's private header */
#include "H5B2private.h"

/* Other private headers needed by this file */
#include "H5ACprivate.h"	/* Metadata cache			*/
#include "H5FLprivate.h"	/* Free Lists                           */
#include "H5RCprivate.h"	/* Reference counted object functions	*/

/**************************/
/* Package Private Macros */
/**************************/

/* Size of signature information (on disk) */
#define H5B2_SIZEOF_MAGIC               4

/* B-tree signatures */
#define H5B2_HDR_MAGIC                  "BTHD"          /* Header */
#define H5B2_INT_MAGIC                  "BTND"          /* Internal node */
#define H5B2_LEAF_MAGIC                 "BTLF"          /* Leaf node */

/* Size of storage for number of records per node (on disk) */
#define H5B2_SIZEOF_RECORDS_PER_NODE    2

/* Size of a "node pointer" (on disk) */
#define H5B2_NODE_POINTER_SIZE(f)       (H5F_SIZEOF_ADDR(f)+H5B2_SIZEOF_RECORDS_PER_NODE+H5F_SIZEOF_SIZE(f))

/* Size of the B-tree header on disk */
#define H5B2_HEADER_SIZE(f)     (                                             \
    4   /* Signature */                                                       \
    + 1 /* Version */                                                         \
    + 1 /* Tree type */                                                       \
    + 4 /* Node size, in bytes */                                             \
    + 2 /* Key size, in bytes */                                              \
    + 2 /* Depth of tree */                                                   \
    + 2 /* Split % of full (as integer, ie. "98" means 98%) */                \
    + 2 /* Merge % of full (as integer, ie. "98" means 98%) */                \
    + H5B2_NODE_POINTER_SIZE(f))  /* Node pointer to root node in tree */

/* Macro to retrieve pointer to i'th native record for native record buffer */
#define H5B2_NAT_NREC(b,shared,idx)  (b+(shared)->nat_off[(idx)])

/* Macro to retrieve pointer to i'th native record for internal node */
#define H5B2_INT_NREC(i,shared,idx)  ((i)->int_native+(shared)->nat_off[(idx)])

/* Macro to retrieve pointer to i'th native record for leaf node */
#define H5B2_LEAF_NREC(l,shared,idx)  ((l)->leaf_native+(shared)->nat_off[(idx)])


/****************************/
/* Package Private Typedefs */
/****************************/

/* A "node pointer" to another B-tree node */
typedef struct {
    haddr_t     addr;           /* Address of other node */
    unsigned    node_nrec;      /* Number of records used in node pointed to */
    hsize_t     all_nrec;       /* Number of records in node pointed to and all it's children */
} H5B2_node_ptr_t;

/* Each B-tree has certain information that can be shared across all
 * the instances of nodes in that B-tree.
 */
typedef struct H5B2_shared_t {
    /* Shared internal data structures */
    const H5B2_class_t	*type;	 /* Type of tree			     */
    uint8_t	        *page;	 /* Disk page */
    H5FL_fac_head_t     *int_fac;       /* Factory for internal node native record blocks */
    H5FL_fac_head_t     *leaf_fac;      /* Factory for leaf node native record blocks */
    H5FL_fac_head_t     *node_ptr_fac;  /* Factory for internal node node pointer blocks */
    size_t              *nat_off;       /* Array of offsets of native records */

    /* Information set by user */
    unsigned    split_percent;  /* Percent full at which to split the node, when inserting */
    unsigned    merge_percent;  /* Percent full at which to merge the node, when deleting */
    size_t      node_size;      /* Size of all nodes, in bytes                */
    size_t      rrec_size;      /* Size of "raw" (on disk) record, in bytes   */

    /* Derived information from user's information */
    size_t      internal_nrec;  /* Number of records which fit into an internal node */
    size_t      split_int_nrec; /* Number of records to split an internal node at */
    size_t      merge_int_nrec; /* Number of records to merge an internal node at */
    size_t      leaf_nrec;      /* Number of records which fit into a leaf node */
    size_t      split_leaf_nrec; /* Number of records to split a leaf node at */
    size_t      merge_leaf_nrec; /* Number of records to merge a leaf node at */
} H5B2_shared_t;

/* The B-tree information */
typedef struct H5B2_t {
    /* Information for H5AC cache functions, _must_ be first field in structure */
    H5AC_info_t cache_info;

    /* Internal B-tree information */
    unsigned	depth;		/* B-tree's overall depth                     */
    H5B2_node_ptr_t root;       /* Node pointer to root node in B-tree        */
    H5RC_t	*shared;	/* Ref-counted shared info	              */
} H5B2_t;

/* B-tree leaf node information */
typedef struct H5B2_leaf_t {
    /* Information for H5AC cache functions, _must_ be first field in structure */
    H5AC_info_t cache_info;

    /* Internal B-tree information */
    H5RC_t	*shared;	/* Ref-counted shared info	              */
    uint8_t     *leaf_native;   /* Pointer to native records                  */
    unsigned    nrec;           /* Number of records in node                  */
} H5B2_leaf_t;

/* B-tree internal node information */
typedef struct H5B2_internal_t {
    /* Information for H5AC cache functions, _must_ be first field in structure */
    H5AC_info_t cache_info;

    /* Internal B-tree information */
    H5RC_t	*shared;	/* Ref-counted shared info	              */
    uint8_t     *int_native;    /* Pointer to native records                  */
    H5B2_node_ptr_t *node_ptrs; /* Pointer to node pointers                   */
    unsigned    nrec;           /* Number of records in node                  */
} H5B2_internal_t;


/*****************************/
/* Package Private Variables */
/*****************************/

/* H5B2 header inherits cache-like properties from H5AC */
H5_DLLVAR const H5AC_class_t H5AC_BT2_HDR[1];

/* H5B2 internal node inherits cache-like properties from H5AC */
H5_DLLVAR const H5AC_class_t H5AC_BT2_INT[1];

/* H5B2 leaf node inherits cache-like properties from H5AC */
H5_DLLVAR const H5AC_class_t H5AC_BT2_LEAF[1];

/* Declare a free list to manage the H5B2_t struct */
H5FL_EXTERN(H5B2_t);

/* Declare a free list to manage the H5B2_internal_t struct */
H5FL_EXTERN(H5B2_internal_t);

/* Declare a free list to manage the H5B2_leaf_t struct */
H5FL_EXTERN(H5B2_leaf_t);

/* Internal v2 B-tree testing class */
#ifdef H5B2_TESTING
H5_DLLVAR const H5B2_class_t H5B2_TEST[1];
#endif /* H5B2_TESTING */


/******************************/
/* Package Private Prototypes */
/******************************/

/* Routines for managing shared B-tree info */
H5_DLL herr_t H5B2_shared_init(H5F_t *f, H5B2_t *bt2, const H5B2_class_t *type,
    size_t node_size, size_t rrec_size, unsigned split_percent, unsigned merge_percent);

/* Metadata cache callbacks */
H5_DLL herr_t H5B2_cache_hdr_dest(H5F_t *f, H5B2_t *b);
H5_DLL herr_t H5B2_cache_leaf_dest(H5F_t *f, H5B2_leaf_t *l);
H5_DLL herr_t H5B2_cache_internal_dest(H5F_t *f, H5B2_internal_t *i);

/* Debugging routines for dumping file structures */
H5_DLL herr_t H5B2_hdr_debug(H5F_t *f, hid_t dxpl_id, haddr_t addr,
    FILE *stream, int indent, int fwidth, const H5B2_class_t *type);
H5_DLL herr_t H5B2_int_debug(H5F_t *f, hid_t dxpl_id, haddr_t addr,
    FILE *stream, int indent, int fwidth, const H5B2_class_t *type,
    haddr_t hdr_addr, unsigned nrec);
H5_DLL herr_t H5B2_leaf_debug(H5F_t *f, hid_t dxpl_id, haddr_t addr,
    FILE *stream, int indent, int fwidth, const H5B2_class_t *type,
    haddr_t hdr_addr, unsigned nrec);

/* Testing routines */
#ifdef H5B2_TESTING
H5_DLL herr_t H5B2_get_root_addr_test(H5F_t *f, hid_t dxpl_id,
    const H5B2_class_t *type, haddr_t addr, haddr_t *root_addr);
#endif /* H5B2_TESTING */

#endif /* _H5B2pkg_H */

