/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
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
#define H5B2_INT_MAGIC                  "BTIN"          /* Internal node */
#define H5B2_LEAF_MAGIC                 "BTLF"          /* Leaf node */

/* Size of storage for number of records per node (on disk) */
#define H5B2_SIZEOF_RECORDS_PER_NODE    2

/* Size of a "tree pointer" (on disk) */
/* (essentially, the largest internal pointer allowed) */
#define H5B2_TREE_POINTER_SIZE(f)       (H5F_SIZEOF_ADDR(f)+H5B2_SIZEOF_RECORDS_PER_NODE+H5F_SIZEOF_SIZE(f))

/* Size of a internal node pointer (on disk) */
#define H5B2_INT_POINTER_SIZE(f, s, d) (                                      \
    H5F_SIZEOF_ADDR(f)          /* Address of child node */                   \
    + (s)->max_nrec_size        /* # of records in child node */              \
    + (s)->node_info[(d) - 1].cum_max_nrec_size /* Total # of records in child & below */ \
    )

/* Size of checksum information (on disk) */
#define H5B2_SIZEOF_CHKSUM      4

/* Format overhead for all v2 B-tree metadata in the file */
#define H5B2_METADATA_PREFIX_SIZE (                                           \
    H5B2_SIZEOF_MAGIC   /* Signature */                                       \
    + 1 /* Version */                                                         \
    + H5B2_SIZEOF_CHKSUM /* Metadata checksum */                              \
    )

/* Size of the v2 B-tree header on disk */
#define H5B2_HEADER_SIZE(f)     (                                             \
    /* General metadata fields */                                             \
    H5B2_METADATA_PREFIX_SIZE                                                 \
                                                                              \
    /* Header specific fields */                                              \
    + 1 /* Tree type */                                                       \
    + 4 /* Node size, in bytes */                                             \
    + 2 /* Record size, in bytes */                                           \
    + 2 /* Depth of tree */                                                   \
    + 1 /* Split % of full (as integer, ie. "98" means 98%) */                \
    + 1 /* Merge % of full (as integer, ie. "98" means 98%) */                \
    + H5B2_TREE_POINTER_SIZE(f)  /* Node pointer to root node in tree */      \
    )

/* Size of the v2 B-tree internal node prefix */
#define H5B2_INT_PREFIX_SIZE (                                                \
    /* General metadata fields */                                             \
    H5B2_METADATA_PREFIX_SIZE                                                 \
                                                                              \
    /* Header specific fields */                                              \
    /* <none> */                                                              \
    )

/* Size of the v2 B-tree leaf node prefix */
#define H5B2_LEAF_PREFIX_SIZE (                                               \
    /* General metadata fields */                                             \
    H5B2_METADATA_PREFIX_SIZE                                                 \
                                                                              \
    /* Header specific fields */                                              \
    /* <none> */                                                              \
    )

/* Macro to retrieve pointer to i'th native record for native record buffer */
#define H5B2_NAT_NREC(b, shared, idx)  ((b) + (shared)->nat_off[(idx)])

/* Macro to retrieve pointer to i'th native record for internal node */
#define H5B2_INT_NREC(i, shared, idx)  H5B2_NAT_NREC((i)->int_native, (shared), (idx))

/* Macro to retrieve pointer to i'th native record for leaf node */
#define H5B2_LEAF_NREC(l, shared, idx)  H5B2_NAT_NREC((l)->leaf_native, (shared), (idx))


/****************************/
/* Package Private Typedefs */
/****************************/

/* A "node pointer" to another B-tree node */
typedef struct {
    haddr_t     addr;           /* Address of other node */
    unsigned    node_nrec;      /* Number of records used in node pointed to */
    hsize_t     all_nrec;       /* Number of records in node pointed to and all it's children */
} H5B2_node_ptr_t;

/* Information about a node at a given depth */
typedef struct {
    unsigned    max_nrec;       /* Max. number of records in node */
    unsigned    split_nrec;     /* Number of records to split node at */
    unsigned    merge_nrec;     /* Number of records to merge node at */
    hsize_t     cum_max_nrec;   /* Cumulative max. # of records below this node's depth */
    unsigned char cum_max_nrec_size; /* Size to store cumulative max. # of records for this node (in bytes) */
    H5FL_fac_head_t *nat_rec_fac;   /* Factory for native record blocks */
    H5FL_fac_head_t *node_ptr_fac;  /* Factory for node pointer blocks */
} H5B2_node_info_t;

/* Each B-tree has certain information that can be shared across all
 * the instances of nodes in that B-tree.
 */
typedef struct H5B2_shared_t {
    /* Shared internal data structures */
    const H5B2_class_t	*type;	 /* Type of tree			     */
    uint8_t	        *page;	 /* Common disk page for I/O */
    size_t              *nat_off;   /* Array of offsets of native records */
    H5B2_node_info_t    *node_info; /* Table of node info structs for current depth of B-tree */

    /* Information set by user (stored) */
    unsigned    split_percent;  /* Percent full at which to split the node, when inserting */
    unsigned    merge_percent;  /* Percent full at which to merge the node, when deleting */
    size_t      node_size;      /* Size of B-tree nodes, in bytes             */
    size_t      rrec_size;      /* Size of "raw" (on disk) record, in bytes   */

    /* Dynamic information (stored) */
    unsigned	depth;		/* B-tree's overall depth                     */

    /* Derived information from user's information */
    unsigned char max_nrec_size; /* Size to store max. # of records in any node (in bytes) */
} H5B2_shared_t;

/* The B-tree information */
typedef struct H5B2_t {
    /* Information for H5AC cache functions, _must_ be first field in structure */
    H5AC_info_t cache_info;

    /* Internal B-tree information */
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
    unsigned    depth;          /* Depth of this node in the B-tree           */
} H5B2_internal_t;

/* User data for metadata cache 'load' callback */
typedef struct {
    H5RC_t *bt2_shared;         /* Ref counter for shared B-tree info */
    unsigned nrec;              /* Number of records in node to load */
    unsigned depth;             /* Depth of node to load */
} H5B2_int_load_ud1_t;

#ifdef H5B2_TESTING
/* Node information for testing */
typedef struct {
    unsigned depth;             /* Depth of node */
    unsigned nrec;              /* Number of records in node */
} H5B2_node_info_test_t;
#endif /* H5B2_TESTING */


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
    unsigned depth, size_t node_size, size_t rrec_size,
    unsigned split_percent, unsigned merge_percent);

/* Routines for operating on internal nodes */
H5_DLL H5B2_internal_t *H5B2_protect_internal(H5F_t *f, hid_t dxpl_id,
    H5RC_t *bt2_shared, haddr_t addr, unsigned nrec, unsigned depth,
    H5AC_protect_t rw);

/* Routines for allocating nodes */
H5_DLL herr_t H5B2_split_root(H5F_t *f, hid_t dxpl_id, H5B2_t *bt2,
    unsigned *bt2_flags_ptr);
H5_DLL herr_t H5B2_create_leaf(H5F_t *f, hid_t dxpl_id, H5RC_t *bt2_shared,
    H5B2_node_ptr_t *node_ptr);

/* Routines for inserting records */
H5_DLL herr_t H5B2_insert_internal(H5F_t *f, hid_t dxpl_id, H5RC_t *bt2_shared,
    unsigned depth, unsigned *parent_cache_info_flags_ptr,
    H5B2_node_ptr_t *curr_node_ptr, void *udata);
H5_DLL herr_t H5B2_insert_leaf(H5F_t *f, hid_t dxpl_id, H5RC_t *bt2_shared,
    H5B2_node_ptr_t *curr_node_ptr, void *udata);

/* Routines for iterating over nodes/records */
H5_DLL herr_t H5B2_iterate_node(H5F_t *f, hid_t dxpl_id, H5RC_t *bt2_shared,
    unsigned depth, const H5B2_node_ptr_t *curr_node, H5B2_operator_t op,
    void *op_data);

/* Routines for locating records */
H5_DLL int H5B2_locate_record(const H5B2_class_t *type, unsigned nrec,
    size_t *rec_off, const uint8_t *native, const void *udata, unsigned *idx);
H5_DLL herr_t H5B2_neighbor_internal(H5F_t *f, hid_t dxpl_id, H5RC_t *bt2_shared,
    unsigned depth, H5B2_node_ptr_t *curr_node_ptr, void *neighbor_loc,
    H5B2_compare_t comp, void *udata, H5B2_found_t op, void *op_data);
H5_DLL herr_t H5B2_neighbor_leaf(H5F_t *f, hid_t dxpl_id, H5RC_t *bt2_shared,
    H5B2_node_ptr_t *curr_node_ptr, void *neighbor_loc,
    H5B2_compare_t comp, void *udata, H5B2_found_t op, void *op_data);

/* Routines for removing records */
H5_DLL herr_t H5B2_remove_internal(H5F_t *f, hid_t dxpl_id, H5RC_t *bt2_shared,
    hbool_t *depth_decreased, void *swap_loc, unsigned depth, H5AC_info_t *parent_cache_info,
    hbool_t * parent_cache_info_dirtied_ptr, H5B2_node_ptr_t *curr_node_ptr, void *udata,
    H5B2_remove_t op, void *op_data);
H5_DLL herr_t H5B2_remove_leaf(H5F_t *f, hid_t dxpl_id, H5RC_t *bt2_shared,
    H5B2_node_ptr_t *curr_node_ptr, void *udata, H5B2_remove_t op,
    void *op_data);
H5_DLL herr_t H5B2_remove_internal_by_idx(H5F_t *f, hid_t dxpl_id, H5RC_t *bt2_shared,
    hbool_t *depth_decreased, void *swap_loc, unsigned depth, H5AC_info_t *parent_cache_info,
    hbool_t * parent_cache_info_dirtied_ptr, H5B2_node_ptr_t *curr_node_ptr, hsize_t idx,
    H5B2_remove_t op, void *op_data);
H5_DLL herr_t H5B2_remove_leaf_by_idx(H5F_t *f, hid_t dxpl_id, H5RC_t *bt2_shared,
    H5B2_node_ptr_t *curr_node_ptr, unsigned idx, H5B2_remove_t op,
    void *op_data);

/* Routines for deleting nodes */
H5_DLL herr_t H5B2_delete_node(H5F_t *f, hid_t dxpl_id, H5RC_t *bt2_shared,
    unsigned depth, const H5B2_node_ptr_t *curr_node, H5B2_remove_t op,
    void *op_data);

/* Metadata cache callbacks */
H5_DLL herr_t H5B2_cache_hdr_dest(H5F_t *f, H5B2_t *b);
H5_DLL herr_t H5B2_cache_leaf_dest(H5F_t *f, H5B2_leaf_t *l);
H5_DLL herr_t H5B2_cache_internal_dest(H5F_t *f, H5B2_internal_t *i);

/* Debugging routines for dumping file structures */
H5_DLL herr_t H5B2_hdr_debug(H5F_t *f, hid_t dxpl_id, haddr_t addr,
    FILE *stream, int indent, int fwidth, const H5B2_class_t *type);
H5_DLL herr_t H5B2_int_debug(H5F_t *f, hid_t dxpl_id, haddr_t addr,
    FILE *stream, int indent, int fwidth, const H5B2_class_t *type,
    haddr_t hdr_addr, unsigned nrec, unsigned depth);
H5_DLL herr_t H5B2_leaf_debug(H5F_t *f, hid_t dxpl_id, haddr_t addr,
    FILE *stream, int indent, int fwidth, const H5B2_class_t *type,
    haddr_t hdr_addr, unsigned nrec);

/* Testing routines */
#ifdef H5B2_TESTING
H5_DLL herr_t H5B2_get_root_addr_test(H5F_t *f, hid_t dxpl_id,
    const H5B2_class_t *type, haddr_t addr, haddr_t *root_addr);
H5_DLL int H5B2_get_node_depth_test(H5F_t *f, hid_t dxpl_id,
    const H5B2_class_t *type, haddr_t addr, void *udata);
H5_DLL herr_t H5B2_get_node_info_test(H5F_t *f, hid_t dxpl_id,
    const H5B2_class_t *type, haddr_t addr, void *udata,
    H5B2_node_info_test_t *ninfo);
#endif /* H5B2_TESTING */

#endif /* _H5B2pkg_H */

