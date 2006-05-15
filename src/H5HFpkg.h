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
#include "H5FSprivate.h"	/* File free space                      */
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
    + (h)->sizeof_addr /* File address of free section header */              \
    + (h)->sizeof_size /* Total size of heap */                               \
    + (h)->sizeof_size /* Size of man. space in heap */                       \
    + (h)->sizeof_size /* Size of alloacted man. space in heap */             \
    + (h)->sizeof_size /* Size of std. space in heap */                       \
    + (h)->sizeof_size /* Number of objects in heap */                        \
    + H5HF_DTABLE_INFO_SIZE(h) /* Size of managed obj. doubling-table info */ \
    )

/* Size of overhead for a direct block */
#define H5HF_MAN_ABS_DIRECT_OVERHEAD(h) (                                     \
    /* General metadata fields */                                             \
    H5HF_METADATA_PREFIX_SIZE                                                 \
                                                                              \
    /* Fractal heap managed, absolutely mapped direct block specific fields */ \
    + (h)->sizeof_addr          /* File address of heap owning the block */   \
    + (h)->heap_off_size        /* Offset of the block in the heap */         \
    )

/* Size of managed indirect block (absolute & mapped) */
#define H5HF_MAN_INDIRECT_SIZE(h, i) (                                        \
    /* General metadata fields */                                             \
    H5HF_METADATA_PREFIX_SIZE                                                 \
                                                                              \
    /* Fractal heap managed, absolutely mapped indirect block specific fields */ \
    + (h)->sizeof_addr          /* File address of heap owning the block */   \
    + (h)->heap_off_size        /* Offset of the block in the heap */         \
    + (MIN((i)->nrows, (h)->man_dtable.max_direct_rows) * (h)->man_dtable.cparam.width * (h)->sizeof_addr) /* Size of entries for direct blocks */ \
    + ((((i)->nrows > (h)->man_dtable.max_direct_rows) ? ((i)->nrows - (h)->man_dtable.max_direct_rows) : 0)  * (h)->man_dtable.cparam.width * (h)->sizeof_addr) /* Size of entries for indirect blocks */ \
    )


/* Compute the # of bytes required to store an offset into a given buffer size */
#define H5HF_SIZEOF_OFFSET_BITS(b)   (((b) + 7) / 8)
#define H5HF_SIZEOF_OFFSET_LEN(l)   H5HF_SIZEOF_OFFSET_BITS(H5V_log2_of2((unsigned)(l)))

/* Free space section types for fractal heap */
/* (values stored in free space data structures in file) */
#define H5FS_SECT_FHEAP_SINGLE        0       /* Section is actual bytes in a direct block */
#define H5FS_SECT_FHEAP_RANGE         1       /* Section is a range of direct blocks */
#define H5FS_SECT_FHEAP_INDIRECT      2       /* Section is a range of _indirect_ blocks in an indirect block row */

/****************************/
/* Package Private Typedefs */
/****************************/

/* Doubling-table info */
typedef struct H5HF_dtable_t {
    /* Immutable, pre-set information for table */
    H5HF_dtable_cparam_t    cparam;     /* Creation parameters for table */

    /* Derived information (stored, varies during lifetime of table) */
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
    hsize_t     *row_dblock_free;   /* Free space in dblocks for this row */
                                    /* (For indirect block rows, it's the total
                                     * free space in all direct blocks referenced
                                     * from the indirect block
                                     */
} H5HF_dtable_t;

/* Fractal heap free list info (forward decl - defined in H5HFflist.c) */
typedef struct H5HF_freelist_t H5HF_freelist_t;

/* Forward decl indirect block info */
typedef struct H5HF_indirect_t H5HF_indirect_t;

/* Fractal heap block location */
typedef struct H5HF_block_loc_t {
    /* Necessary table fields */
    unsigned    row;            /* Row of block in doubling table             */
    unsigned    col;            /* Column of block in doubling table          */

    /* Derived/computed/cached table fields */
    unsigned    entry;          /* Entry of block in doubling table           */

    /* Infrastructure */
    H5HF_indirect_t *context;   /* Pointer to the indirect block containing the block */
    struct H5HF_block_loc_t *up;  /* Pointer to next level up in the stack of levels */
} H5HF_block_loc_t;

/* Fractal heap block iterator info */
typedef struct H5HF_block_iter_t {
    hbool_t ready;              /* Set if iterator is finished initializing   */
    H5HF_block_loc_t *curr;     /* Pointer to the current level information for iterator */
} H5HF_block_iter_t;

/* Fractal heap free space section info */
typedef struct H5HF_free_section_t {
    H5FS_section_info_t sect_info;              /* Free space section information (must be first in struct) */
    union {
        struct {
            H5HF_indirect_t *parent;            /* Indirect block parent for free section's direct block */
            unsigned par_entry;                 /* Entry of free section's direct block in parent indirect block */
                                                /* (Needed to retrieve direct block) */

            haddr_t     dblock_addr;            /* Address of direct block for free section */
            size_t      dblock_size;            /* Size of direct block */
                                                /* (Needed to retrieve direct block) */
        } single;
        struct {
            H5HF_indirect_t *iblock;            /* Indirect block for free section */
            unsigned    row;                    /* Row for range of blocks */
            unsigned    col;                    /* Column for range of blocks */
            unsigned    num_entries;            /* Number of entries covered */
        } range;
        struct {
            H5HF_indirect_t *iblock;            /* Indirect block for free section */
            unsigned    row;                    /* Row for range of blocks */
            unsigned    col;                    /* Column for range of blocks */
            unsigned    num_entries;            /* Number of entries covered */
            unsigned    indir_row;              /* Row for indirect range of blocks */
            unsigned    indir_nrows;            /* Number of rows in indirect blocks */
        } indirect;
    } u;
} H5HF_free_section_t;

/* The fractal heap header information */
/* (Each fractal heap header has certain information that is shared across all
 * the instances of blocks in that fractal heap)
 */
typedef struct H5HF_hdr_t {
    /* Information for H5AC cache functions, _must_ be first field in structure */
    H5AC_info_t cache_info;

    /* Shared internal information (varies during lifetime of heap) */
    hsize_t     total_man_free; /* Total amount of free space in managed blocks */
    hsize_t     total_std_free; /* Total # of free standalone ID entries */
    haddr_t     fs_addr;        /* Address of free space header on disk */

    /* Statistics for heap */
    hsize_t     total_size;     /* Total amount of space used by heap (managed & standalone) */
    hsize_t     man_size;       /* Total amount of managed space in heap */
    hsize_t     man_alloc_size; /* Total amount of allocated managed space in heap */
    hsize_t     std_size;       /* Total amount of standalone space in heap */
    hsize_t     nobjs;          /* Number of objects in heap */

    /* Cached/computed values (not stored in header) */
    size_t      rc;             /* Reference count of child blocks */
    hbool_t     dirty;          /* Shared info is modified */
    haddr_t     heap_addr;      /* Address of heap header in the file */
    H5AC_protect_t mode;        /* Access mode for heap */
    H5F_t      *f;              /* Pointer to file for heap */
    size_t      sizeof_size;    /* Size of file sizes */
    size_t      sizeof_addr;    /* Size of file addresses */
    size_t      id_len;         /* Size of heap IDs */
    size_t      nsect_classes;  /* Number of free space section classes */
    H5FS_section_class_t *sect_cls;     /* Array of free space section classes */
    H5FS_t      *fspace;        /* Free space list for objects in heap */
    hbool_t     fspace_open;    /* Whether free space is ready */
    H5HF_block_iter_t next_block;   /* Block iterator for searching for next block with space */

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
} H5HF_hdr_t;

/* Indirect block entry */
typedef struct H5HF_indirect_ent_t {
    haddr_t     addr;           /* Direct block's address                     */
/* XXX: Will need space for block size, for blocks with I/O filters */
} H5HF_indirect_ent_t;

/* Fractal heap indirect block */
struct H5HF_indirect_t {
    /* Information for H5AC cache functions, _must_ be first field in structure */
    H5AC_info_t cache_info;

    /* Internal heap information (not stored) */
    size_t      rc;             /* Reference count of child blocks            */
    hbool_t     dirty;          /* Info is modified                           */
    H5HF_hdr_t	*hdr;	        /* Shared heap header info	              */
    struct H5HF_indirect_t *parent;	/* Shared parent indirect block info  */
    unsigned    par_entry;      /* Entry in parent's table                    */
    haddr_t     addr;           /* Address of this indirect block on disk     */
    unsigned    nrows;          /* Total # of rows in indirect block          */
    unsigned    max_rows;       /* Max. # of rows in indirect block           */
    size_t      size;           /* Size of indirect block on disk             */

    /* Stored values */
    hsize_t     block_off;      /* Offset of the block within the heap's address space */
    H5HF_indirect_ent_t *ents;  /* Pointer to block entry table               */
};

/* A fractal heap direct block */
typedef struct H5HF_direct_t {
    /* Information for H5AC cache functions, _must_ be first field in structure */
    H5AC_info_t cache_info;

    /* Internal heap information */
    H5HF_hdr_t	*hdr;	        /* Shared heap header info	              */
    H5HF_indirect_t *parent;	/* Shared parent indirect block info          */
    unsigned    par_entry;      /* Entry in parent's table                    */
    size_t      size;           /* Size of direct block                       */
    unsigned    blk_off_size;   /* Size of offsets in the block               */
    uint8_t     *blk;           /* Pointer to buffer containing block data    */

    /* Stored values */
    hsize_t     block_off;      /* Offset of the block within the heap's address space */
} H5HF_direct_t;

/* Fractal heap */
struct H5HF_t {
    H5HF_hdr_t  *hdr;           /* Pointer to internal fractal heap header info */
    unsigned    fo_count;       /* Open object count for file                 */
};

/* Fractal heap metadata statistics info */
typedef struct H5HF_stat_t {
    hsize_t total_size;         /* Total size of heap allocated (man & std)   */
    hsize_t man_size;           /* Total size of managed space in heap        */
    hsize_t std_size;           /* Total size of standalone space in heap     */
    hsize_t man_free_space;     /* Free space within heap                     */
    hsize_t nobjs;              /* Number of objects in heap                  */
} H5HF_stat_t;

/* Fractal heap "parent info" (for loading a block) */
typedef struct H5HF_parent_t {
    H5HF_hdr_t *hdr;                /* Pointer to heap header info */
    H5HF_indirect_t *iblock;    /* Pointer to parent indirect block */
    unsigned entry;             /* Location of block in parent's entry table */
} H5HF_parent_t;

/*****************************/
/* Package Private Variables */
/*****************************/

/* H5HF header inherits cache-like properties from H5AC */
H5_DLLVAR const H5AC_class_t H5AC_FHEAP_HDR[1];

/* H5HF direct block inherits cache-like properties from H5AC */
H5_DLLVAR const H5AC_class_t H5AC_FHEAP_DBLOCK[1];

/* H5HF indirect block inherits cache-like properties from H5AC */
H5_DLLVAR const H5AC_class_t H5AC_FHEAP_IBLOCK[1];

/* H5HF single section inherits serializable properties from H5FS_section_class_t */
H5_DLLVAR H5FS_section_class_t H5FS_SECT_CLS_FHEAP_SINGLE[1];

/* H5HF range section inherits serializable properties from H5FS_section_class_t */
H5_DLLVAR H5FS_section_class_t H5FS_SECT_CLS_FHEAP_RANGE[1];

/* H5HF indirect section inherits serializable properties from H5FS_section_class_t */
H5_DLLVAR H5FS_section_class_t H5FS_SECT_CLS_FHEAP_INDIRECT[1];

/* Declare a free list to manage the H5HF_hdr_t struct */
H5FL_EXTERN(H5HF_hdr_t);

/* Declare a free list to manage the H5HF_direct_t struct */
H5FL_EXTERN(H5HF_direct_t);

/* Declare a free list to manage heap direct block data to/from disk */
H5FL_BLK_EXTERN(direct_block);

/* Declare a free list to manage the H5HF_indirect_t struct */
H5FL_EXTERN(H5HF_indirect_t);

/* Declare a free list to manage the H5HF_indirect_ent_t sequence information */
H5FL_SEQ_EXTERN(H5HF_indirect_ent_t);

/* Declare a free list to manage the H5HF_free_section_t struct */
H5FL_EXTERN(H5HF_free_section_t);


/******************************/
/* Package Private Prototypes */
/******************************/

/* Routines for managing shared fractal heap header */
H5_DLL H5HF_hdr_t * H5HF_hdr_alloc(H5F_t *f);
H5_DLL herr_t H5HF_hdr_init(H5HF_hdr_t *hdr, haddr_t fh_addr, H5HF_create_t *cparam);
H5_DLL herr_t H5HF_hdr_finish_init(H5HF_hdr_t *hdr);

/* Doubling table routines */
H5_DLL herr_t H5HF_dtable_init(H5HF_dtable_t *dtable);
H5_DLL herr_t H5HF_dtable_dest(H5HF_dtable_t *dtable);
H5_DLL herr_t H5HF_dtable_lookup(const H5HF_dtable_t *dtable, hsize_t off,
    unsigned *row, unsigned *col);
H5_DLL unsigned H5HF_dtable_size_to_row(H5HF_dtable_t *dtable, size_t block_size);

/* Heap header routines */
H5_DLL herr_t H5HF_hdr_incr(H5HF_hdr_t *hdr);
H5_DLL herr_t H5HF_hdr_decr(H5HF_hdr_t *hdr);
H5_DLL herr_t H5HF_hdr_dirty(H5HF_hdr_t *hdr);
H5_DLL herr_t H5HF_hdr_adj_free(H5HF_hdr_t *hdr, ssize_t amt);
H5_DLL herr_t H5HF_hdr_extend_heap(H5HF_hdr_t *hdr, hsize_t new_size, hsize_t extra_free);
H5_DLL herr_t H5HF_hdr_inc_alloc(H5HF_hdr_t *hdr, hsize_t new_alloc_size,
    unsigned nentries);

/* Indirect block routines */
H5_DLL herr_t H5HF_iblock_incr(H5HF_indirect_t *iblock);
H5_DLL herr_t H5HF_iblock_decr(H5HF_indirect_t *iblock);
H5_DLL herr_t H5HF_iblock_dirty(H5HF_indirect_t *iblock);
H5_DLL H5HF_indirect_t * H5HF_man_iblock_place_dblock(H5HF_hdr_t *fh, hid_t dxpl_id,
    size_t min_dblock_size, size_t *entry_p, size_t *dblock_size);
H5_DLL herr_t H5HF_man_iblock_alloc_single(H5HF_hdr_t *hdr, hid_t dxpl_id,
    H5HF_free_section_t *sec_node);
H5_DLL herr_t H5HF_man_iblock_alloc_range(H5HF_hdr_t *hdr, hid_t dxpl_id,
    H5HF_free_section_t **sec_node);
H5_DLL herr_t H5HF_man_iblock_alloc_indirect(H5HF_hdr_t *hdr, hid_t dxpl_id,
    H5HF_free_section_t **sec_node);
H5_DLL H5HF_indirect_t *H5HF_man_iblock_protect(H5HF_hdr_t *hdr, hid_t dxpl_id,
    haddr_t iblock_addr, unsigned iblock_nrows,
    H5HF_indirect_t *par_iblock, unsigned par_entry,
    H5AC_protect_t rw);

/* Direct block routines */
H5_DLL herr_t H5HF_man_dblock_new(H5HF_hdr_t *fh, hid_t dxpl_id, size_t request);
H5_DLL herr_t H5HF_man_dblock_create(hid_t dxpl_id, H5HF_hdr_t *hdr,
    H5HF_indirect_t *par_iblock, unsigned par_entry, size_t block_size,
    hsize_t block_off, haddr_t *addr_p, H5HF_free_section_t **ret_sec_node);
H5_DLL H5HF_direct_t *H5HF_man_dblock_protect(H5HF_hdr_t *hdr, hid_t dxpl_id,
    haddr_t dblock_addr, size_t dblock_size,
    H5HF_indirect_t *par_iblock, unsigned par_entry,
    H5AC_protect_t rw);

/* Routines for internal operations */
H5_DLL herr_t H5HF_free_section_free_cb(void *item, void UNUSED *key,
    void UNUSED *op_data);
H5_DLL herr_t H5HF_man_locate_block(H5HF_hdr_t *hdr, hid_t dxpl_id,
    hsize_t obj_off, hbool_t locate_indirect, H5HF_indirect_t **par_iblock,
    unsigned *par_entry, H5AC_protect_t rw);
H5_DLL herr_t H5HF_man_find(H5HF_hdr_t *fh, hid_t dxpl_id, size_t request,
    H5HF_free_section_t **sec_node/*out*/);
H5_DLL herr_t H5HF_man_insert(H5HF_hdr_t *fh, hid_t dxpl_id,
    H5HF_free_section_t *sec_node, size_t obj_size, const void *obj,
    void *id);
H5_DLL herr_t H5HF_man_read(H5HF_hdr_t *fh, hid_t dxpl_id, hsize_t obj_off,
    size_t obj_len, void *obj);

/* Metadata cache callbacks */
H5_DLL herr_t H5HF_cache_hdr_dest(H5F_t *f, H5HF_hdr_t *hdr);
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
H5_DLL herr_t H5HF_stat_info(H5HF_t *fh, H5HF_stat_t *stats);

/* Block iteration routines */
H5_DLL herr_t H5HF_man_iter_init(H5HF_block_iter_t *biter);
H5_DLL herr_t H5HF_man_iter_start_offset(H5HF_hdr_t *hdr, hid_t dxpl_id,
    H5HF_block_iter_t *biter, hsize_t offset);
H5_DLL herr_t H5HF_man_iter_start_entry(H5HF_hdr_t *hdr, H5HF_block_iter_t *biter,
    H5HF_indirect_t *iblock, unsigned start_entry);
H5_DLL herr_t H5HF_man_iter_next(H5HF_hdr_t *hdr, H5HF_block_iter_t *biter,
    unsigned nentries);
H5_DLL herr_t H5HF_man_iter_up(H5HF_block_iter_t *biter);
H5_DLL herr_t H5HF_man_iter_down(H5HF_block_iter_t *biter, H5HF_indirect_t *iblock);
H5_DLL herr_t H5HF_man_iter_reset(H5HF_block_iter_t *biter);
H5_DLL herr_t H5HF_man_iter_update_iblock(H5HF_block_iter_t *biter, H5HF_indirect_t *iblock);
H5_DLL herr_t H5HF_man_iter_curr(H5HF_block_iter_t *biter, unsigned *row, unsigned *col,
    unsigned *entry, H5HF_indirect_t **block);
H5_DLL herr_t H5HF_man_iter_offset(H5HF_hdr_t *hdr, H5HF_block_iter_t *biter,
    hsize_t *offset);
H5_DLL hbool_t H5HF_man_iter_ready(H5HF_block_iter_t *biter);

/* Free space manipulation routines */
H5_DLL herr_t H5HF_space_start(H5HF_hdr_t *hdr, hid_t dxpl_id);
H5_DLL htri_t H5HF_space_find(H5HF_hdr_t *hdr, hid_t dxpl_id, hsize_t request,
    H5HF_free_section_t **node);
H5_DLL herr_t H5HF_space_add(H5HF_hdr_t *hdr, hid_t dxpl_id, H5HF_free_section_t *node);
H5_DLL herr_t H5HF_space_close(H5HF_hdr_t *hdr, hid_t dxpl_id);

/* Testing routines */
#ifdef H5HF_TESTING
H5_DLL herr_t H5HF_get_cparam_test(H5HF_t *fh, H5HF_create_t *cparam);
H5_DLL hsize_t H5HF_get_dblock_free_test(H5HF_t *fh, unsigned row);
H5_DLL size_t H5HF_get_dblock_overhead(H5HF_t *fh);
#endif /* H5HF_TESTING */

#endif /* _H5HFpkg_H */

