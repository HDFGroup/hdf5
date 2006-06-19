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
 *		Tuesday, May  2, 2006
 *
 * Purpose:	This file contains declarations which are visible only within
 *		the H5FS package.  Source files outside the H5FS package should
 *		include H5FSprivate.h instead.
 */
#ifndef H5FS_PACKAGE
#error "Do not include this file outside the H5FS package!"
#endif

#ifndef _H5FSpkg_H
#define _H5FSpkg_H

/* Get package's private header */
#include "H5FSprivate.h"	/* File free space                      */

/* Other private headers needed by this file */
#include "H5ACprivate.h"	/* Metadata cache			*/

/**************************/
/* Package Private Macros */
/**************************/

/* Size of signature information (on disk) */
#define H5FS_SIZEOF_MAGIC               4

/* Free space signatures */
#define H5FS_HDR_MAGIC                  "FSHD"          /* Header */
#define H5FS_SECTS_MAGIC                "FSSS"          /* Serialized sections */

/* "Standard" size of prefix information for free space metadata */
#define H5FS_METADATA_PREFIX_SIZE (                                           \
    4   /* Signature */                                                       \
    + 1 /* Version */                                                         \
    + 1 /* Metadata flags */                                                  \
    + 4 /* Metadata checksum */                                               \
    )

/* Size of the fractal heap header on disk */
#define H5FS_HEADER_SIZE(f) (                                                 \
    /* General metadata fields */                                             \
    H5FS_METADATA_PREFIX_SIZE                                                 \
                                                                              \
    /* Free space header specific fields */                                   \
    + 1 /* Client ID */                                                       \
    + H5F_SIZEOF_SIZE(f) /* Total free space tracked */                       \
    + H5F_SIZEOF_SIZE(f) /* # of sections tracked */                          \
    + 2 /* Number of section classes */                                       \
    + 2 /* Shrink percent */                                                  \
    + 2 /* Expand percent */                                                  \
    + 2 /* Size of address space for sections (log2 of value) */              \
    + H5F_SIZEOF_SIZE(f) /* Max. size of section to track */                  \
    + H5F_SIZEOF_ADDR(f) /* Address of serialized free space sections */      \
    + H5F_SIZEOF_SIZE(f) /* Size of serialized free space sections used */    \
    + H5F_SIZEOF_SIZE(f) /* Allocated size of serialized free space sections */ \
    )

/****************************/
/* Package Private Typedefs */
/****************************/

/* Free space header info */
typedef struct H5FS_hdr_t {
    /* Information for H5AC cache functions, _must_ be first field in structure */
    H5AC_info_t cache_info;

    /* Statistics */
    hsize_t tot_space;          /* Total amount of space tracked              */
    hsize_t sect_count;         /* # of sections tracked                      */

    /* Creation information */
    H5FS_client_t client;       /* Type of user of this free space manager    */
    unsigned nclasses;          /* Number of section classes handled          */
    unsigned shrink_percent;    /* Percent of "normal" serialized size to shrink serialized space at */
    unsigned expand_percent;    /* Percent of "normal" serialized size to expand serialized space at */
    unsigned max_sect_addr;     /* Size of address space free sections are within (log2 of actual value) */
    hsize_t max_sect_size;      /* Maximum size of section to track */

    /* Serialized space information */
    haddr_t sect_addr;          /* Address of the section info in the file    */
    hsize_t sect_size;          /* Size of the section info in the file       */
    hsize_t alloc_sect_size;    /* Allocated size of the section info in the file */
} H5FS_hdr_t;

/* Free space section bin info */
typedef struct H5FS_bin_t {
    size_t sect_count;          /* Total # of sections in this bin */
    H5SL_t *bin_list;           /* Skip list of differently sized sections */
} H5FS_bin_t;

/* Main free space info */
struct H5FS_t {
    /* Stored values (from header) */
    H5FS_hdr_t *hdr;            /* Pointer to header info                     */

    /* Computed/cached values */
    haddr_t addr;               /* Address of free space header on disk       */
    unsigned nbins;             /* Number of bins                             */
    size_t serial_size;         /* Total serialized size of all section nodes */
    size_t size_count;          /* Number of differently sized sections       */
    unsigned sect_prefix_size;  /* Size of the section serialization prefix (in bytes) */
    unsigned sect_off_size;     /* Size of a section offset (in bytes)        */
    unsigned sect_len_size;     /* Size of a section length (in bytes)        */
    hbool_t dirty;              /* Space information is dirty                 */

    /* Memory data structures (not stored directly) */
    H5FS_section_class_t *sect_cls; /* Array of section classes for this free list */
    H5FS_section_info_t *single; /* Section information when free list has only one free section */
    H5SL_t *merge_list;         /* Skip list to hold sections for detecting merges */
    H5FS_bin_t *bins;           /* Array of lists of lists of free sections   */
};


/*****************************/
/* Package Private Variables */
/*****************************/

/* H5FS header inherits cache-like properties from H5AC */
H5_DLLVAR const H5AC_class_t H5AC_FSPACE_HDR[1];

/* Declare a free list to manage the H5FS_hdr_t struct */
H5FL_EXTERN(H5FS_hdr_t);


/******************************/
/* Package Private Prototypes */
/******************************/

/* Debugging routines for dumping file structures */
H5_DLL herr_t H5FS_hdr_debug(H5F_t *f, hid_t dxpl_id, haddr_t addr,
    FILE *stream, int indent, int fwidth);
H5_DLL herr_t H5FS_sects_debug(H5F_t *f, hid_t dxpl_id, haddr_t addr,
    FILE *stream, int indent, int fwidth, haddr_t fs_addr, haddr_t client_addr);

/* Metadata cache callbacks */
H5_DLL herr_t H5FS_cache_hdr_dest(H5F_t *f, H5FS_hdr_t *hdr);

#endif /* _H5FSpkg_H */

