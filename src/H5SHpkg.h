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
 *		the H5SH package.  Source files outside the H5SH package should
 *		include H5SHprivate.h instead.
 */
#ifndef H5SH_PACKAGE
#error "Do not include this file outside the H5SH package!"
#endif

#ifndef _H5SHpkg_H
#define _H5SHpkg_H

/* Get package's private header */
#include "H5ACprivate.h"	/* Metadata cache			*/
#include "H5FLprivate.h"	/* Free Lists                           */
#include "H5SHprivate.h"	/* Segmented heap			*/

/* Other private headers needed by this file */

/**************************/
/* Package Private Macros */
/**************************/

/* Size of signature information (on disk) */
#define H5SH_SIZEOF_MAGIC               4

/* Segmented heap signature */
#define H5SH_MAGIC                      "SGHP"

/* Size of the segmented heap info on disk */
#define H5SH_SIZE(f) (                                                        \
    4 +                         /* Signature */                               \
    1 +                         /* Version */                                 \
    1 +                         /* Heap data type */                          \
    H5F_SIZEOF_SIZE(f) +        /* Min. size of heap blocks */                \
    H5F_SIZEOF_SIZE(f) +        /* Max. expand size of heap blocks */         \
    H5F_SIZEOF_ADDR(f) +        /* Address of block tracker for actual heap blocks */ \
    H5F_SIZEOF_ADDR(f))         /* Address of block tracker for free blocks */

/****************************/
/* Package Private Typedefs */
/****************************/

/* The segmented heap information */
typedef struct H5SH_t {
    /* Information for H5AC cache functions, _must_ be first field in structure */
    H5AC_info_t cache_info;

    /* Information set by user */
    H5SH_data_type_t heap_type; /* Type of data stored in heap */
    hsize_t     min_size;       /* Minimum size of heap block */
    hsize_t     max_extend_size;/* Maximum size to expand heap block to */
                                /* (Objects larger than this size will get
                                 * placed into their own heap block)
                                 */

    /* Derived information from user's information */
    H5FD_mem_t file_mem_type;   /* Type of memory to store heap blocks in the file */

    /* Internal block tracking information */
    haddr_t     bt_heap_addr;   /* Address of block tracker for heap blocks */
    haddr_t     bt_free_addr;   /* Address of block tracker for free space */
} H5SH_t;


/*****************************/
/* Package Private Variables */
/*****************************/

/* H5SH inherits cache-like properties from H5AC */
H5_DLLVAR const H5AC_class_t H5AC_SGHP[1];

/* Declare a free list to manage the H5SH_t struct */
H5FL_EXTERN(H5SH_t);


/******************************/
/* Package Private Prototypes */
/******************************/
H5_DLL herr_t H5SH_cache_dest(H5F_t *f, H5SH_t *b);
H5_DLL herr_t H5SH_debug(H5F_t *f, hid_t dxpl_id, haddr_t addr, FILE *stream,
    int indent, int fwidth);

#endif /* _H5SHpkg_H */


