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
 * Created:		H5HFprivate.h
 *			Feb 24 2006
 *			Quincey Koziol <koziol@ncsa.uiuc.edu>
 *
 * Purpose:		Private header for library accessible fractal heap routines.
 *
 *-------------------------------------------------------------------------
 */

#ifndef _H5HFprivate_H
#define _H5HFprivate_H

/* Include package's public header */
#include "H5HFpublic.h"

/* Private headers needed by this file */
#include "H5Fprivate.h"		/* File access				*/

/**************************/
/* Library Private Macros */
/**************************/


/****************************/
/* Library Private Typedefs */
/****************************/

/* Types of heaps */
typedef enum {
    H5HF_ABSOLUTE,      /* The heap uses absolute internal addressing */
    H5HF_MAPPED         /* The heap maps internal addresses to allow compaction */
} H5HF_addrmap_t;

/* Creation parameters for doubling-tables */
typedef struct H5HF_dtable_cparam_t {
    unsigned    width;          /* Number of columns in the table (must be power of 2) */
    size_t      start_block_size; /* Starting block size for table (must be power of 2) */
    size_t      max_direct_size; /* Maximum size of a direct block (must be power of 2) */
    unsigned    max_index;      /* Maximum ID/offset for table (integer log2 of actual value, ie. the # of bits required) */
    unsigned    start_root_rows;        /* Starting number of rows for root indirect block */
                                /* 0 indicates to create the full indirect block for the root,
                                 * right from the start.  Doesn't have to be power of 2
                                 */
} H5HF_dtable_cparam_t;

/* Fractal heap creation parameters */
typedef struct H5HF_create_t {
    H5HF_dtable_cparam_t managed;/* Mapped object doubling-table creation parameters */
    H5HF_addrmap_t addrmap;     /* Type of address mapping for objects in heap */
    uint32_t standalone_size;   /* Size of object to store standalone */
                                /* (i.e. max. size of object to manage) */
    uint32_t fixed_len_size;    /* Size of objects, in bytes (0 means variable-sized objects) */
                                /* (only for heaps w/fixed-length objects) */
    unsigned char ref_count_size; /* Size of ref. count field for objects, in bytes (0 means no ref. counts for objects) */
                                /* (only for heaps w/ref. counted objects) */
} H5HF_create_t;


/*****************************/
/* Library-private Variables */
/*****************************/
 
/***************************************/
/* Library-private Function Prototypes */
/***************************************/
H5_DLL herr_t H5HF_create(H5F_t *f, hid_t dxpl_id, H5HF_create_t *cparam,
    haddr_t *addr_p);
H5_DLL herr_t H5HF_insert(H5F_t *f, hid_t dxpl_id, haddr_t addr, size_t size,
    const void *obj, void *id/*out*/);

#endif /* _H5HFprivate_H */


