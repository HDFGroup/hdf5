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
 *		Thursday, March 10, 2005
 *
 * Purpose:	This file contains declarations which are visible only within
 *		the H5BT package.  Source files outside the H5BT package should
 *		include H5BTprivate.h instead.
 */
#ifndef H5BT_PACKAGE
#error "Do not include this file outside the H5BT package!"
#endif

#ifndef _H5BTpkg_H
#define _H5BTpkg_H

/* Get package's private header */
#include "H5BTprivate.h"

/* Other private headers needed by this file */
#include "H5ACprivate.h"	/* Metadata cache			*/
#include "H5B2private.h"	/* v2 B-trees				*/
#include "H5FLprivate.h"	/* Free Lists                           */

/**************************/
/* Package Private Macros */
/**************************/

/* Size of signature information (on disk) */
#define H5BT_SIZEOF_MAGIC               4

/* Block Tracker signature */
#define H5BT_MAGIC                      "BLTR"

/* Size of the Block Tracker info on disk */
#define H5BT_SIZE(f) (                                                        \
    4 +                         /* Signature */                               \
    1 +                         /* Version */                                 \
    1 +                         /* Status flags */                            \
    H5F_SIZEOF_SIZE(f) +        /* Max. size block tracked (can be 0 for "unknown") */ \
    4 +                         /* Reference count of max. size block tracked */       \
    H5F_SIZEOF_SIZE(f) +        /* Min. size block tracked (can be 0 for "unknown") */ \
    4 +                         /* Reference count of min. size block tracked */       \
    H5F_SIZEOF_SIZE(f) +        /* Total number of bytes tracked */           \
    H5F_SIZEOF_ADDR(f))         /* Address of v2 B-tree which holds block inf */

/* Bit flags for block tracker size status */
#define H5BT_STATUS_MAX_VALID   0x01    /* Maximum block size valid over all blocks tracked */
    /* If this flag is not set, then only part of the blocks have been */
    /* searched to determine the current maximum block size.  This can happen */
    /* during block shrinks or removals */
#define H5BT_STATUS_MIN_VALID   0x02    /* Minimum block size valid over all blocks tracked */
    /* If this flag is not set, then only part of the blocks have been */
    /* searched to determine the current minimum block size.  This can happen */
    /* during block expansions or removals */


/****************************/
/* Package Private Typedefs */
/****************************/

/* The block tracker information */
typedef struct H5BT_t {
    /* Information for H5AC cache functions, _must_ be first field in structure */
    H5AC_info_t cache_info;

    /* Internal block tracking information */
    unsigned char status;       /* Status flags for max & min values */
    hsize_t     max_block_size; /* Maximum size of a block tracked */
    uint32_t    max_block_cnt;  /* Count of max. block tracked */
    hsize_t     min_block_size; /* Minimum size of a block tracked */
    uint32_t    min_block_cnt;  /* Count of min. block tracked */
    hsize_t     tot_block_size; /* Total size of all blocks tracked */
    haddr_t     bt2_addr;       /* Address of v2 B-tree that holds block info */
} H5BT_t;

/* Info for a single block */
typedef struct H5BT_blk_info_t {
    haddr_t     addr;           /* Address (offset) of block in file */
    hsize_t     len;            /* Length of block in file */
} H5BT_blk_info_t;


/*****************************/
/* Package Private Variables */
/*****************************/

/* H5BT inherits cache-like properties from H5AC */
H5_DLLVAR const H5AC_class_t H5AC_BLTR[1];

/* v2 B-tree class for block info records */
H5_DLLVAR const H5B2_class_t H5B2_BLKTRK[1];

/* Declare a free list to manage the H5BT_t struct */
H5FL_EXTERN(H5BT_t);


/******************************/
/* Package Private Prototypes */
/******************************/
H5_DLL herr_t H5BT_cache_dest(H5F_t *f, H5BT_t *b);
H5_DLL herr_t H5BT_hdr_debug(H5F_t *f, hid_t dxpl_id, haddr_t addr, FILE *stream,
    int indent, int fwidth);
#ifdef H5BT_TESTING
H5_DLL herr_t H5BT_get_max_info(H5F_t *f, hid_t dxpl_id, haddr_t addr,
    hsize_t *size, uint32_t *count, hbool_t *valid);
H5_DLL herr_t H5BT_get_min_info(H5F_t *f, hid_t dxpl_id, haddr_t addr,
    hsize_t *size, uint32_t *count, hbool_t *valid);
#endif /* H5B2_TESTING */

#endif /* _H5BTpkg_H */

