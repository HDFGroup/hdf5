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
#define H5HF_DBLOCK_MAGIC               "FHDB"          /* Direct block */

/* "Standard" size of prefix information for fractal heap metadata */
#define H5HF_METADATA_PREFIX_SIZE (                                           \
    4   /* Signature */                                                       \
    + 1 /* Version */                                                         \
    + 1 /* Metadata flags */                                                  \
    + 4 /* Metadata checksum */                                               \
    )

/* Size of doubling-table information */
#define H5HF_DTABLE_INFO_SIZE(f) (                                            \
    2   /* Width of table (i.e. # of columns) */                              \
    + H5F_SIZEOF_SIZE(f) /* Starting block size */                            \
    + H5F_SIZEOF_SIZE(f) /* Maximum direct block size */                      \
    + 2 /* Max. size of heap (log2 of actual value - i.e. the # of bits) */   \
    + 2 /* Starting # of rows in root indirect block */                       \
    + H5F_SIZEOF_ADDR(f) /* File address of table managed */                  \
    + 2 /* Current # of rows in root indirect block */                        \
    )

/* Size of the fractal heap header on disk */
#define H5HF_HEADER_SIZE(f)     (                                             \
    /* General metadata fields */                                             \
    H5HF_METADATA_PREFIX_SIZE                                                 \
                                                                              \
    /* Fractal heap header specific fields */                                 \
    + 1 /* Address mapping */                                                 \
    + 4 /* Min. size of standalone object */                                  \
    + 4 /* Length of fixed-size objects */                                    \
    + 1 /* Size of ref. count for objects */                                  \
    + H5HF_DTABLE_INFO_SIZE(f) /* Size of managed obj. doubling-table info */ \
    )

/* Size of header for each object in an absolute managed direct block */
#define H5HF_MAN_ABS_DIRECT_OBJ_PREFIX_LEN_SIZE(s, o) (                       \
    ((s)->fixed_len_obj ? 0 : H5HF_SIZEOF_OFFSET_LEN(o))  /* Length of object in block */ \
    + (s)->ref_count_size       /* Ref. count of object in block */           \
    )
#define H5HF_MAN_ABS_DIRECT_OBJ_PREFIX_LEN_DBLOCK(s, d) (                     \
    ((s)->fixed_len_obj ? 0 : (d)->blk_off_size)  /* Length of object in block */ \
    + (s)->ref_count_size       /* Ref. count of object in block */           \
    )

/* Size of overhead for a direct block */
#define H5HF_MAN_ABS_DIRECT_OVERHEAD_SIZE(s, o) (                             \
    /* General metadata fields */                                             \
    H5HF_METADATA_PREFIX_SIZE                                                 \
                                                                              \
    /* Fractal heap managed, absolutely mapped direct block specific fields */ \
    + H5F_SIZEOF_ADDR((s)->f)   /* File address of heap owning the block */   \
    + (s)->heap_off_size        /* Offset of the block in the heap */         \
    + H5HF_SIZEOF_OFFSET_LEN(o) /* Total free space in a block */             \
    + H5HF_SIZEOF_OFFSET_LEN(o) /* Offset of first descriptor in free list */ \
    )
#define H5HF_MAN_ABS_DIRECT_OVERHEAD_DBLOCK(s, d) (                           \
    /* General metadata fields */                                             \
    H5HF_METADATA_PREFIX_SIZE                                                 \
                                                                              \
    /* Fractal heap managed, absolutely mapped direct block specific fields */ \
    + H5F_SIZEOF_ADDR((s)->f)   /* File address of heap owning the block */   \
    + (s)->heap_off_size        /* Offset of the block in the heap */         \
    + (d)->blk_off_size         /* Total free space in a block */             \
    + (d)->blk_off_size         /* Offset of first descriptor in free list */ \
    )

/* Compute the # of bytes required to store an offset into a given buffer size */
#define H5HF_SIZEOF_OFFSET_BITS(b)   (((b) + 7) / 8)
#define H5HF_SIZEOF_OFFSET_LEN(l)   H5HF_SIZEOF_OFFSET_BITS(H5V_log2_of2((unsigned)(l)))

/****************************/
/* Package Private Typedefs */
/****************************/

/* Doubling-table info */
typedef struct H5HF_dtable_param_t {
    /* Immutable, pre-set information for table */
    H5HF_dtable_cparam_t    cparam;     /* Creation parameters for table */

    /* Derived information (varies during lifetime of table) */
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
    /* Shared internal information (varies during lifetime of heap) */
    hsize_t     next_man_block; /* Offset of next direct managed block */
    hsize_t     next_std_block; /* Offset of next direct standalone block */
    hsize_t     total_man_free; /* Total amount of free space in managed blocks */
    hsize_t     total_std_free; /* Total # of free standalone ID entries */

    /* Cached/computed values */
    haddr_t     heap_addr;      /* Address of heap header in the file */
    H5F_t      *f;              /* Pointer to file for heap */

    /* Doubling table information */
    /* (Partially set by user, partially derived/updated internally) */
    H5HF_dtable_param_t man_dtable_info;        /* Doubling-table info for managed objects */
    H5HF_dtable_param_t std_dtable_info;        /* Doubling-table info for standalone objects */

    /* Information set by user */
    H5HF_addrmap_t addrmap;     /* Type of address mapping */
    uint32_t standalone_size;   /* Size of object to store standalone */
    uint32_t fixed_len_size;    /* Size of objects (only for heaps w/fixed-length objects) */
    unsigned char ref_count_size; /* Size of ref. count for objects (only for heaps w/ref. counted objects) */

    /* Information derived from user parameters */
    hbool_t     fixed_len_obj;  /* Are objects in the heap fixed length? */
    hbool_t     ref_count_obj;  /* Are objects in the heap ref. counted? */
    hbool_t     have_io_filter; /* Does the heap have I/O filters for the direct blocks? */
    hbool_t     write_once;     /* Is heap being written in "write once" mode? */
    unsigned char heap_off_size; /* Size of heap offsets (in bytes) */
} H5HF_shared_t;

/* The fractal heap header information */
typedef struct H5HF_t {
    /* Information for H5AC cache functions, _must_ be first field in structure */
    H5AC_info_t cache_info;

    /* Internal fractal heap information */
    H5RC_t	*shared;	/* Ref-counted shared info	              */
} H5HF_t;

/* Direct block free list node */
typedef struct H5HF_direct_free_node_t {
    size_t      size;                           /* Size of free space */
    size_t      my_offset;                      /* Offset of free space in block */
    size_t      next_offset;                    /* Offset of next free space in block */
    struct H5HF_direct_free_node_t *prev;       /* Previous node in free list */
    struct H5HF_direct_free_node_t *next;       /* Next node in free list */
} H5HF_direct_free_node_t;

/* Direct block free list header */
typedef struct H5HF_direct_free_head_t {
    H5HF_direct_free_node_t *first;     /* First node in free list */
    hbool_t dirty;                      /* Free list is modified */
} H5HF_direct_free_head_t;

/* A fractal heap direct block */
typedef struct H5HF_direct_t {
    /* Information for H5AC cache functions, _must_ be first field in structure */
    H5AC_info_t cache_info;

    /* Internal heap information */
    H5RC_t	*shared;	/* Ref-counted shared info	              */
    size_t      size;           /* Size of direct block                       */
    size_t      blk_off_size;   /* Size of offsets in the block               */
    H5HF_direct_free_head_t *free_list; /* Pointer to free list for block     */
    uint8_t     *blk;           /* Pointer to buffer containing block data    */

    /* Stored values */
    hsize_t     block_off;      /* Offset of the block within the heap's address space */
    size_t      blk_free_space; /* Total amount of free space in block        */
    size_t      free_list_head; /* Offset of head of free list in block       */
} H5HF_direct_t;


/*****************************/
/* Package Private Variables */
/*****************************/

/* H5HF header inherits cache-like properties from H5AC */
H5_DLLVAR const H5AC_class_t H5AC_FHEAP_HDR[1];

/* H5HF direct block inherits cache-like properties from H5AC */
H5_DLLVAR const H5AC_class_t H5AC_FHEAP_DBLOCK[1];

/* Declare a free list to manage the H5HF_t struct */
H5FL_EXTERN(H5HF_t);

/* Declare a free list to manage the H5HF_direct_t struct */
H5FL_EXTERN(H5HF_direct_t);

/* Declare a free list to manage the H5HF_direct_free_head_t struct */
H5FL_EXTERN(H5HF_direct_free_head_t);

/* Declare a free list to manage the H5HF_direct_free_node_t struct */
H5FL_EXTERN(H5HF_direct_free_node_t);

/* Declare a free list to manage heap direct block data to/from disk */
H5FL_BLK_EXTERN(direct_block);



/******************************/
/* Package Private Prototypes */
/******************************/

/* Routines for managing shared fractal heap info */
H5_DLL H5HF_shared_t * H5HF_shared_alloc(H5F_t *f);
H5_DLL herr_t H5HF_shared_create(H5F_t *f, H5HF_t *fh, haddr_t heap_addr, H5HF_create_t *cparam);
H5_DLL herr_t H5HF_shared_own(H5HF_t *fh, H5HF_shared_t *shared);

/* Routines for allocating space */
H5_DLL herr_t H5HF_man_alloc_end(H5RC_t *fh_shared, hid_t dxpl_id, unsigned *fh_flags_ptr,
    size_t size, const void *obj, void *id/*out*/);
H5_DLL herr_t H5HF_man_dblock_build_freelist(H5HF_direct_t *dblock);

/* Metadata cache callbacks */
H5_DLL herr_t H5HF_cache_hdr_dest(H5F_t *f, H5HF_t *fh);
H5_DLL herr_t H5HF_cache_dblock_dest(H5F_t *f, H5HF_direct_t *dblock);

/* Debugging routines for dumping file structures */
H5_DLL herr_t H5HF_hdr_debug(H5F_t *f, hid_t dxpl_id, haddr_t addr,
    FILE *stream, int indent, int fwidth);
H5_DLL herr_t H5HF_dblock_debug(H5F_t *f, hid_t dxpl_id, haddr_t addr,
    FILE *stream, int indent, int fwidth, haddr_t hdr_addr, size_t nrec);

/* Testing routines */
#ifdef H5HF_TESTING
H5_DLL herr_t H5HF_get_cparam_test(H5F_t *f, hid_t dxpl_id, haddr_t fh_addr,
    H5HF_create_t *cparam);
#endif /* H5HF_TESTING */

#endif /* _H5HFpkg_H */

