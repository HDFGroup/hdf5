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
#include "H5SLprivate.h"	/* Skip lists				*/

/**************************/
/* Package Private Macros */
/**************************/

/* Size of signature information (on disk) */
#define H5HF_SIZEOF_MAGIC               4

/* Fractal heap signatures */
#define H5HF_HDR_MAGIC                  "FRHP"          /* Header */
#define H5HF_DBLOCK_MAGIC               "FHDB"          /* Direct block */
#define H5HF_IBLOCK_MAGIC               "FHIB"          /* Indirect block */

/* "Standard" size of prefix information for fractal heap metadata */
#define H5HF_METADATA_PREFIX_SIZE (                                           \
    4   /* Signature */                                                       \
    + 1 /* Version */                                                         \
    + 1 /* Metadata flags */                                                  \
    + 4 /* Metadata checksum */                                               \
    )

/* Size of doubling-table information */
#define H5HF_DTABLE_INFO_SIZE(h) (                                            \
    2   /* Width of table (i.e. # of columns) */                              \
    + (h)->sizeof_size /* Starting block size */                              \
    + (h)->sizeof_size /* Maximum direct block size */                        \
    + 2 /* Max. size of heap (log2 of actual value - i.e. the # of bits) */   \
    + 2 /* Starting # of rows in root indirect block */                       \
    + (h)->sizeof_addr /* File address of table managed */                    \
    + 2 /* Current # of rows in root indirect block */                        \
    )

/* Size of the fractal heap header on disk */
#define H5HF_HEADER_SIZE(h)     (                                             \
    /* General metadata fields */                                             \
    H5HF_METADATA_PREFIX_SIZE                                                 \
                                                                              \
    /* Fractal heap header specific fields */                                 \
    + 1 /* Address mapping mode */                                            \
    + 4 /* Min. size of standalone object */                                  \
    + (h)->sizeof_size /* Total man. free space */                            \
    + (h)->sizeof_size /* Total std. free entries */                          \
    + (h)->sizeof_size /* Total size of heap */                               \
    + (h)->sizeof_size /* Size of man. space in heap */                       \
    + (h)->sizeof_size /* Size of std. space in heap */                       \
    + (h)->sizeof_size /* Number of objects in heap */                        \
    + H5HF_DTABLE_INFO_SIZE(h) /* Size of managed obj. doubling-table info */ \
    )

/* Size of free space description in an absolute managed direct block */
#define H5HF_MAN_ABS_DIRECT_FREE_NODE_SIZE(d) (2 * (d)->blk_off_size)

/* Size of header for each object in an absolute managed direct block */
#define H5HF_MAN_ABS_DIRECT_OBJ_PREFIX_LEN(h) (                               \
    + 1                         /* Free space fragment length */              \
    )

/* Size of overhead for a direct block */
#define H5HF_MAN_ABS_DIRECT_OVERHEAD_SIZE(h, o) (                             \
    /* General metadata fields */                                             \
    H5HF_METADATA_PREFIX_SIZE                                                 \
                                                                              \
    /* Fractal heap managed, absolutely mapped direct block specific fields */ \
    + (h)->sizeof_addr          /* File address of heap owning the block */   \
    + (h)->sizeof_addr          /* File address of parent block */            \
    + 2                         /* Entry within parent block */               \
    + 2                         /* # of rows for parent block */              \
    + (h)->heap_off_size        /* Offset of the block in the heap */         \
    + H5HF_SIZEOF_OFFSET_LEN(o) /* Offset of first descriptor in free list */ \
    )
#define H5HF_MAN_ABS_DIRECT_OVERHEAD_DBLOCK(h, d) (                           \
    /* General metadata fields */                                             \
    H5HF_METADATA_PREFIX_SIZE                                                 \
                                                                              \
    /* Fractal heap managed, absolutely mapped direct block specific fields */ \
    + (h)->sizeof_addr          /* File address of heap owning the block */   \
    + (h)->sizeof_addr          /* File address of parent block */            \
    + 2                         /* Entry within parent block */               \
    + 2                         /* # of rows for parent block */              \
    + (h)->heap_off_size        /* Offset of the block in the heap */         \
    + (d)->blk_off_size         /* Offset of first descriptor in free list */ \
    )

/* Size of managed indirect block (absolute & mapped) */
#define H5HF_MAN_INDIRECT_SIZE(h, i) (                                        \
    /* General metadata fields */                                             \
    H5HF_METADATA_PREFIX_SIZE                                                 \
                                                                              \
    /* Fractal heap managed, absolutely mapped indirect block specific fields */ \
    + (h)->sizeof_addr          /* File address of heap owning the block */   \
    + (h)->sizeof_addr          /* File address of parent block */            \
    + 2                         /* Entry within parent block */               \
    + 2                         /* # of rows for parent block */              \
    + (h)->heap_off_size        /* Offset of the block in the heap */         \
    + 4                         /* Next block entry to allocate from */       \
    + (MIN((i)->nrows, (h)->man_dtable.max_direct_rows) * (h)->man_dtable.cparam.width * ((h)->sizeof_addr + (h)->man_dtable.max_dir_blk_off_size)) /* Size of entries for direct blocks */ \
    + ((((i)->nrows > (h)->man_dtable.max_direct_rows) ? ((i)->nrows - (h)->man_dtable.max_direct_rows) : 0)  * (h)->man_dtable.cparam.width * ((h)->sizeof_addr + (h)->heap_off_size)) /* Size of entries for indirect blocks */ \
    )


/* Compute the # of bytes required to store an offset into a given buffer size */
#define H5HF_SIZEOF_OFFSET_BITS(b)   (((b) + 7) / 8)
#define H5HF_SIZEOF_OFFSET_LEN(l)   H5HF_SIZEOF_OFFSET_BITS(H5V_log2_of2((unsigned)(l)))

/****************************/
/* Package Private Typedefs */
/****************************/

/* Doubling-table info */
typedef struct H5HF_dtable_t {
    /* Immutable, pre-set information for table */
    H5HF_dtable_cparam_t    cparam;     /* Creation parameters for table */

    /* Derived information (stored, vary during lifetime of table) */
    haddr_t     table_addr;     /* Address of first block for table */
                                /* Undefined if no space allocated for table */
    unsigned    curr_root_rows; /* Current number of rows in the root indirect block */
                                /* 0 indicates that the TABLE_ADDR field points
                                 * to direct block (of START_BLOCK_SIZE) instead
                                 * of indirect root block.
                                 */

    /* Computed information (not stored) */
    unsigned    max_root_rows;  /* Maximum # of rows in root indirect block */
    unsigned    max_direct_rows; /* Maximum # of direct rows in any indirect block */
    unsigned    max_dir_blk_off_size;   /* Max. size of offsets in direct blocks */
    unsigned    first_row_bits;     /* # of bits in address of first row */
    hsize_t     num_id_first_row;   /* Number of IDs in first row of table */
    hsize_t     *row_block_size;    /* Block size per row of indirect block */
    hsize_t     *row_block_off;     /* Cumulative offset per row of indirect block */
} H5HF_dtable_t;

/* Fractal heap free list info (forward decl - defined in H5HFflist.c) */
typedef struct H5HF_freelist_t H5HF_freelist_t;

/* Fractal heap free list section info (forward decl - defined in H5HFint.c) */
typedef struct H5HF_section_free_node_t H5HF_section_free_node_t;

/* The fractal heap header information */
/* (Each fractal heap header has certain information that is shared across all
 * the instances of blocks in that fractal heap)
 */
typedef struct H5HF_t {
    /* Information for H5AC cache functions, _must_ be first field in structure */
    H5AC_info_t cache_info;

    /* Shared internal information (varies during lifetime of heap) */
    hsize_t     total_man_free; /* Total amount of free space in managed blocks */
    hsize_t     total_std_free; /* Total # of free standalone ID entries */

    /* Statistics for heap */
    hsize_t     total_size;     /* Total amount of space used by heap (managed & standalone) */
    hsize_t     man_size;       /* Total amount of managed space in heap */
    hsize_t     std_size;       /* Total amount of standalone space in heap */
    hsize_t     nobjs;          /* Number of objects in heap */

    /* Cached/computed values (not stored in header) */
    size_t      rc;             /* Reference count of child blocks */
    hbool_t     dirty;          /* Shared info is modified */
    hbool_t     evicted;        /* Shared info is evicted from cache */
    haddr_t     heap_addr;      /* Address of heap header in the file */
    H5AC_protect_t mode;        /* Access mode for heap */
    H5F_t      *f;              /* Pointer to file for heap */
    size_t      sizeof_size;    /* Size of file sizes */
    size_t      sizeof_addr;    /* Size of file addresses */
    H5HF_freelist_t *flist;     /* Free list for objects in heap */
    size_t      id_len;         /* Size of heap IDs */

    /* Doubling table information */
    /* (Partially set by user, partially derived/updated internally) */
    H5HF_dtable_t man_dtable;   /* Doubling-table info for managed objects */
    H5HF_dtable_t std_dtable;   /* Doubling-table info for standalone objects */

    /* Information set by user */
    H5HF_addrmap_t addrmap;     /* Type of address mapping */
    uint32_t standalone_size;   /* Size of object to store standalone */

    /* Information derived from user parameters (not stored in header) */
    unsigned char heap_off_size; /* Size of heap offsets (in bytes) */
    hbool_t     debug_objs;     /* Is the heap storing objects in 'debug' format */
    hbool_t     have_io_filter; /* Does the heap have I/O filters for the direct blocks? */
    hbool_t     write_once;     /* Is heap being written in "write once" mode? */
} H5HF_t;

/* Indirect block entry */
typedef struct H5HF_indirect_ent_t {
    haddr_t     addr;           /* Direct block's address                     */
    hsize_t     free_space;     /* Amount of free space in block pointed to   */
/* XXX: Will need space for block size, for blocks with I/O filters */
} H5HF_indirect_ent_t;

/* Fractal heap indirect block */
typedef struct H5HF_indirect_t {
    /* Information for H5AC cache functions, _must_ be first field in structure */
    H5AC_info_t cache_info;

    /* Internal heap information */
    size_t      rc;             /* Reference count of child blocks            */
    hbool_t     dirty;          /* Info is modified                           */
    hbool_t     evicted;        /* Info is evicted from cache                 */
    H5HF_t	*shared;	/* Shared heap header info	              */
    struct H5HF_indirect_t *parent;	/* Shared parent indirect block info  */
    hsize_t     child_free_space;   /* Total amount of free space in children */
    haddr_t     addr;           /* Address of this indirect block on disk     */
    unsigned    nrows;          /* Total # of rows in indirect block          */
    unsigned    max_rows;       /* Max. # of rows in indirect block           */
    size_t      size;           /* Size of indirect block on disk             */
    unsigned    next_col;       /* "Column" of next managed block (in doubling table) */
    unsigned    next_row;       /* "Row" of next managed block (in doubling table) */
    hsize_t     next_size;      /* Size of next managed block                 */
    H5HF_indirect_ent_t *ents;  /* Pointer to block entry table               */

    /* Stored values */
    haddr_t     par_addr;       /* Address of parent block on disk            */
    unsigned    par_entry;      /* Entry in parent's table                    */
    unsigned    par_nrows;      /* Total # of rows in parent indirect block   */
    hsize_t     block_off;      /* Offset of the block within the heap's address space */
    size_t      next_entry;     /* Entry of next managed block                */
} H5HF_indirect_t;

/* Direct block free list node */
typedef struct H5HF_direct_free_node_t {
    /* Direct block free list info */
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
    H5HF_t	*shared;	/* Shared heap header info	              */
    H5HF_indirect_t *parent;	/* Shared parent indirect block info          */
    size_t      size;           /* Size of direct block                       */
    unsigned    blk_off_size;   /* Size of offsets in the block               */
    size_t      blk_free_space; /* Total amount of free space in block        */
    H5HF_direct_free_head_t *free_list; /* Pointer to free list for block     */
    uint8_t     *blk;           /* Pointer to buffer containing block data    */

    /* Stored values */
    haddr_t     par_addr;       /* Address of parent block on disk            */
    unsigned    par_entry;      /* Entry in parent's table                    */
    unsigned    par_nrows;      /* Total # of rows in parent indirect block   */
    hsize_t     block_off;      /* Offset of the block within the heap's address space */
    size_t      free_list_head; /* Offset of head of free list in block       */
} H5HF_direct_t;

/* Fractal heap metadata statistics info */
typedef struct H5HF_stat_t {
    hsize_t total_size;         /* Total size of heap allocated (man & std)   */
    hsize_t man_size;           /* Total size of managed space in heap        */
    hsize_t std_size;           /* Total size of standalone space in heap     */
    hsize_t man_free_space;     /* Free space within heap                     */
    hsize_t nobjs;              /* Number of objects in heap                  */
} H5HF_stat_t;


/*****************************/
/* Package Private Variables */
/*****************************/

/* H5HF header inherits cache-like properties from H5AC */
H5_DLLVAR const H5AC_class_t H5AC_FHEAP_HDR[1];

/* H5HF direct block inherits cache-like properties from H5AC */
H5_DLLVAR const H5AC_class_t H5AC_FHEAP_DBLOCK[1];

/* H5HF indirect block inherits cache-like properties from H5AC */
H5_DLLVAR const H5AC_class_t H5AC_FHEAP_IBLOCK[1];

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

/* Declare a free list to manage the H5HF_indirect_t struct */
H5FL_EXTERN(H5HF_indirect_t);

/* Declare a free list to manage the H5HF_indirect_ent_t sequence information */
H5FL_SEQ_EXTERN(H5HF_indirect_ent_t);


/******************************/
/* Package Private Prototypes */
/******************************/

/* Routines for managing shared fractal heap header */
H5_DLL H5HF_t * H5HF_alloc(H5F_t *f);
H5_DLL herr_t H5HF_init(H5HF_t *fh, haddr_t heap_addr, H5HF_create_t *cparam);
H5_DLL herr_t H5HF_finish_init(H5HF_t *fh);

/* Doubling table routines */
H5_DLL herr_t H5HF_dtable_dest(H5HF_dtable_t *dtable);

/* Heap header routines */
H5_DLL herr_t H5HF_cache_hdr_dest_real(H5HF_t *hdr);
H5_DLL herr_t H5HF_hdr_incr(H5HF_t *hdr);
H5_DLL herr_t H5HF_hdr_decr(H5HF_t *hdr);

/* Indirect block routines */
H5_DLL herr_t H5HF_cache_iblock_dest_real(H5HF_indirect_t *iblock);
H5_DLL herr_t H5HF_iblock_incr(H5HF_indirect_t *iblock);
H5_DLL herr_t H5HF_iblock_decr(H5HF_indirect_t *iblock);

/* Routines for allocating space */
H5_DLL herr_t H5HF_man_dblock_build_freelist(H5HF_direct_t *dblock, haddr_t dblock_addr);
H5_DLL herr_t H5HF_man_find(H5HF_t *fh, hid_t dxpl_id, size_t request,
    H5HF_section_free_node_t **sec_node/*out*/);
H5_DLL herr_t H5HF_man_insert(H5HF_t *fh, hid_t dxpl_id,
    H5HF_section_free_node_t *sec_node, size_t obj_size, const void *obj,
    void *id);
H5_DLL herr_t H5HF_man_read(H5HF_t *fh, hid_t dxpl_id, hsize_t obj_off,
    size_t obj_len, void *obj);

/* Metadata cache callbacks */
H5_DLL herr_t H5HF_cache_hdr_dest(H5F_t *f, H5HF_t *fh);
H5_DLL herr_t H5HF_cache_dblock_dest(H5F_t *f, H5HF_direct_t *dblock);
H5_DLL herr_t H5HF_cache_iblock_dest(H5F_t *f, H5HF_indirect_t *iblock);

/* Debugging routines for dumping file structures */
H5_DLL herr_t H5HF_hdr_debug(H5F_t *f, hid_t dxpl_id, haddr_t addr,
    FILE *stream, int indent, int fwidth);
H5_DLL herr_t H5HF_dblock_debug(H5F_t *f, hid_t dxpl_id, haddr_t addr,
    FILE *stream, int indent, int fwidth, haddr_t hdr_addr, size_t nrec);
H5_DLL herr_t H5HF_iblock_debug(H5F_t *f, hid_t dxpl_id, haddr_t addr,
    FILE *stream, int indent, int fwidth, haddr_t hdr_addr, unsigned nrows);

/* Statistics routines */
H5_DLL herr_t H5HF_stat_info(H5F_t *f, hid_t dxpl_id, haddr_t fh_addr,
    H5HF_stat_t *stats);

/* Free list routines */
H5_DLL H5HF_freelist_t * H5HF_flist_create(size_t max_block_size,
    H5SL_operator_t node_free_op);
H5_DLL herr_t H5HF_flist_add(H5HF_freelist_t *flist, void *node, size_t *size_key,
    haddr_t *addr_key);
H5_DLL htri_t H5HF_flist_find(H5HF_freelist_t *flist, size_t request,
    void **node);
H5_DLL herr_t H5HF_flist_free(H5HF_freelist_t *flist);

/* Testing routines */
#ifdef H5HF_TESTING
H5_DLL herr_t H5HF_get_cparam_test(H5F_t *f, hid_t dxpl_id, haddr_t fh_addr,
    H5HF_create_t *cparam);
#endif /* H5HF_TESTING */

#endif /* _H5HFpkg_H */

