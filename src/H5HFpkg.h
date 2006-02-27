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
 *		Friday, February 24, 2006
 *
 * Purpose:	This file contains declarations which are visible only within
 *		the H5HF package.  Source files outside the H5HF package should
 *		include H5HFprivate.h instead.
 */
#ifndef H5HF_PACKAGE
#error "Do not include this file outside the H5HF package!"
#endif

#ifndef _H5HFpkg_H
#define _H5HFpkg_H

/* Get package's private header */
#include "H5HFprivate.h"

/* Other private headers needed by this file */
#include "H5ACprivate.h"	/* Metadata cache			*/
#include "H5FLprivate.h"	/* Free Lists                           */
#include "H5RCprivate.h"	/* Reference counted object functions	*/

/**************************/
/* Package Private Macros */
/**************************/

/* Size of signature information (on disk) */
#define H5HF_SIZEOF_MAGIC               4

/* Fractal heap signatures */
#define H5HF_HDR_MAGIC                  "FRHP"          /* Header */

/* Size of the fractal heap header on disk */
#define H5HF_HEADER_SIZE(f)     (                                             \
    /* General metadata fields */                                             \
    4   /* Signature */                                                       \
    + 1 /* Version */                                                         \
    + 1 /* Metadata flags */                                                  \
    + 4 /* Metadata checksum */                                               \
                                                                              \
    /* Fractal heap header specific fields */                                 \
    + 1 /* Heap type */                                                       \
    )

/****************************/
/* Package Private Typedefs */
/****************************/

/* Doubling-table info */
typedef struct H5HF_dtable_param_t {
    /* Immutable, pre-set information for table */
    unsigned    width;          /* Number of columns in the table (must be power of 2) */
    hsize_t     start_block_size; /* Starting block size for table (must be power of 2) */
    hsize_t     max_direct_size; /* Maximum size of a direct block (must be power of 2) */
    hsize_t     max_index;      /* Maximum ID/offset for table (must be power of 2) */
    unsigned    start_root_rows;        /* Starting number of rows for root indirect block */
                                /* 0 indicates to create the full indirect block for the root,
                                 * right from the start.  Doesn't have to be power of 2
                                 */

    /* Derived information that varies during lifetime of table */
    haddr_t     table_addr;     /* Address of first block for table */
                                /* Undefined if no space allocated for table */
    unsigned    curr_root_rows; /* Current number of rows in the root indirect block */
                                /* 0 indicates that the TABLE_ADDR field points
                                 * to direct block (of START_BLOCK_SIZE) instead
                                 * of indirect root block.
                                 */
} H5HF_dtable_param_t;

/* Each fractal heap has certain information that can be shared across all
 * the instances of blocks in that fractal heap.
 */
typedef struct H5HF_shared_t {
    /* Shared internal information */
    hsize_t     next_man_block; /* Offset of next direct managed block */
    hsize_t     next_std_block; /* Offset of next direct standalone block */
    hsize_t     total_man_free; /* Total amount of free space in managed blocks */
    hsize_t     total_std_free; /* Total # of free standalone ID entries */

    /* Doubling table information */
    /* (Partially set by user, partially derived/updated internally) */
    H5HF_dtable_param_t man_dtable_info;        /* Doubling-table info for managed objects */
    H5HF_dtable_param_t std_dtable_info;        /* Doubling-table info for standalone objects */

    /* Information set by user */
    H5HF_type_t type;           /* Type of address mapping */
    size_t standalone_size;     /* Size of object to store standalone */
    size_t fixed_len_size;      /* Size of objects (only for heaps w/fixed-length objects) */

    /* Information derived from user parameters */
    hbool_t     fixed_len_obj;  /* Are objects in the heap fixed length? */
    hbool_t     ref_count_obj;  /* Are objects in the heap ref. counted? */
    hbool_t     have_io_filter; /* Does the heap have I/O filters for the direct blocks? */
    hbool_t     write_once;     /* Is heap being written in "write once" mode? */
} H5HF_shared_t;

/* The fractal heap information */
typedef struct H5HF_t {
    /* Information for H5AC cache functions, _must_ be first field in structure */
    H5AC_info_t cache_info;

    /* Internal fractal heap information */
    H5RC_t	*shared;	/* Ref-counted shared info	              */
} H5HF_t;


/*****************************/
/* Package Private Variables */
/*****************************/

/* H5HF header inherits cache-like properties from H5AC */
H5_DLLVAR const H5AC_class_t H5AC_FHEAP_HDR[1];

/* Declare a free list to manage the H5HF_t struct */
H5FL_EXTERN(H5HF_t);


/******************************/
/* Package Private Prototypes */
/******************************/

/* Routines for managing shared fractal heap info */
H5_DLL herr_t H5HF_shared_init(H5F_t *f, H5HF_t *fh, H5HF_type_t type);

/* Metadata cache callbacks */
H5_DLL herr_t H5HF_cache_hdr_dest(H5F_t *f, H5HF_t *b);

/* Debugging routines for dumping file structures */
H5_DLL herr_t H5HF_hdr_debug(H5F_t *f, hid_t dxpl_id, haddr_t addr,
    FILE *stream, int indent, int fwidth);

/* Testing routines */
#ifdef H5HF_TESTING
H5_DLL herr_t H5HF_get_addrmap_test(H5F_t *f, hid_t dxpl_id, haddr_t fh_addr,
    H5HF_type_t *heap_type);
#endif /* H5HF_TESTING */

#endif /* _H5HFpkg_H */

