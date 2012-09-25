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
 * Programmer:
 *
 * Purpose:    This file contains declarations which are visible only within
 *        the H5FA package.  Source files outside the H5FA package should
 *        include H5FAprivate.h instead.
 */
#if !(defined(H5FA_PACKAGE) | defined(H5FA_MODULE))
#error "Do not include this file outside the H5FA package!"
#endif

#ifndef _H5FApkg_H
#define _H5FApkg_H

/* Get package's private header */
#include "H5FAprivate.h"

/* Other private headers needed by this file */
#include "H5ACprivate.h"    /* Metadata cache            */
#include "H5FLprivate.h"    /* Free Lists                           */


/**************************/
/* Package Private Macros */
/**************************/

/* Define this to display debugging information for the Fixed Array layer */
/* #define H5FA_DEBUG */


/* If this package header is being included in one of the H5FA modules, define
 *      the proper control macros for the generic FUNC_ENTER/LEAVE and error
 *      reporting macros.
 */
#ifdef H5FA_MODULE
#define H5_MY_PKG       H5FA
#define H5_MY_PKG_ERR   H5E_FARRAY
#define H5_MY_PKG_INIT  NO
#endif /* H5FA_MODULE */

/* Fill value for fixed array test class */
#ifdef H5FA_TESTING
#define H5FA_TEST_FILL          ((uint64_t)ULLONG_MAX)
#endif /* H5FA_TESTING */

/* Size of checksum information (on disk) */
#define H5FA_SIZEOF_CHKSUM      4

/* "Standard" size of prefix information for fixed array metadata */
#define H5FA_METADATA_PREFIX_SIZE(c) (                                      \
    H5_SIZEOF_MAGIC                     /* Signature            */          \
    + 1                                 /* Version              */          \
    + 1                                 /* Array type           */          \
    + ((c) ? H5FA_SIZEOF_CHKSUM : 0)    /* Metadata checksum    */          \
    )

/* Size of the Fixed Array header on disk */
#define H5FA_HEADER_SIZE(h)     (                                           \
    /* General metadata fields */                                           \
    H5FA_METADATA_PREFIX_SIZE(TRUE)                                         \
                                                                            \
    /* General array information */                                         \
    + 1 /* Element Size */                                                  \
    + 1 /* Log2(Max. # of elements in data block page) - i.e. # of bits needed to store max. # of elements in data block page */    \
                                                                            \
    /* Fixed Array statistics fields */                                     \
    + (h)->sizeof_size /* # of elements in the fixed array */               \
                                                                            \
    /* Fixed Array Header specific fields */                                \
    + (h)->sizeof_addr /* File address of Fixed Array data block */         \
    )

/* Size of the Fixed Array data block prefix on disk */
#define H5FA_DBLOCK_PREFIX_SIZE(d)  (                                       \
    /* General metadata fields */                                           \
    H5FA_METADATA_PREFIX_SIZE(TRUE)                                         \
                                                                            \
    /* Sanity-checking fields */                                            \
    + (d)->hdr->sizeof_addr        /* File address of Fixed Array header owning the data block */   \
                                                                            \
    /* Fixed Array Data Block specific fields */                            \
    + (d)->dblk_page_init_size /* Fixed array data block 'page init' bitmasks (can be 0 if no pages) */ \
    )

/* Size of the Fixed Array data block on disk */
#define H5FA_DBLOCK_SIZE(d)  (                                              \
    /* Data block prefix size  */                                           \
    H5FA_DBLOCK_PREFIX_SIZE(d)                                              \
                                                                            \
    /* Fixed Array Elements|Pages of Elements*/                             \
    + ((d)->hdr->cparam.nelmts * (size_t)(d)->hdr->cparam.raw_elmt_size)    \
    + ((d)->npages * H5FA_SIZEOF_CHKSUM)        /* Checksum */              \
    )

/* Size of the Fixed Array data block page on disk */
#define H5FA_DBLK_PAGE_SIZE(d, nelmts)     (                                \
    /* Fixed Array Data Block Page */                                       \
    + (nelmts * (size_t)(d)->hdr->cparam.raw_elmt_size) /* Elements in data block page  */  \
    + H5FA_SIZEOF_CHKSUM                                /* Checksum for each page       */  \
    )

/****************************/
/* Package Private Typedefs */
/****************************/

/* The Fixed Array header information */
typedef struct H5FA_hdr_t {
    /* Information for H5AC cache functions, _must_ be first field in structure */
    H5AC_info_t cache_info;

    /* Fixed array configuration/creation parameters (stored in header) */
    H5FA_create_t cparam;               /* Creation parameters for Fixed Array */

    /* Fixed Array data block information (stored in header) */
    haddr_t dblk_addr;                   /* Address of Fixed Array Data block */

    /* Statistics for Fixed Array (stored in header) */
    H5FA_stat_t stats;                  /* Statistcs for Fixed Array */

    /* Computed/cached values (not stored in header) */
    size_t rc;                          /* Reference count of the header                                */
    haddr_t addr;                       /* Address of header in file                                    */
    size_t size;                        /* Size of header in file                                       */
    H5F_t *f;                           /* Pointer to file for fixed array                              */
    hbool_t swmr_write;                 /* Flag indicating the file is opened with SWMR-write access    */
    size_t file_rc;                     /* Reference count of files using array header                  */
    hbool_t pending_delete;             /* Array is pending deletion                                    */
    size_t sizeof_addr;                 /* Size of file addresses                                       */
    size_t sizeof_size;                 /* Size of file sizes                                           */

    /* Client information (not stored) */
    void *cb_ctx;                       /* Callback context */
} H5FA_hdr_t;

/* The fixed array data block information */
typedef struct H5FA_dblock_t {
    /* Information for H5AC cache functions, _must_ be first field in structure */
    H5AC_info_t cache_info;

    /* Fixed array information (stored) */
    uint8_t     *dblk_page_init;/* Bitmap of whether a data block page is initialized       */
    void        *elmts;         /* Buffer for elements stored in data block                 */

    /* Internal array information (not stored) */
    H5FA_hdr_t    *hdr;            /* Shared array header info                              */

    /* Computed/cached values (not stored) */
    haddr_t     addr;               /* Address of this data block on disk                   */
    hsize_t     size;               /* Size of data block on disk                           */
    size_t      npages;             /* Nummber of pages in data block (zero if not paged)   */
    size_t      last_page_nelmts;   /* Nummber of elements in last page, if paged           */

    /* Fixed Array data block information (not stored) */
    size_t dblk_page_nelmts;        /* # of elements per data block page                    */
    size_t dblk_page_size;          /* Size of a data block page                            */
    size_t dblk_page_init_size;     /* Size of 'page init' bitmask                          */
} H5FA_dblock_t;

/* The fixed array data block page information */
typedef struct H5FA_dbk_page_t {
    /* Information for H5AC cache functions, _must_ be first field in structure */
    H5AC_info_t cache_info;

    /* Fixed array information (stored) */
    void        *elmts;         /* Buffer for elements stored in data block page */

    /* Internal array information (not stored) */
    H5FA_hdr_t    *hdr;         /* Shared array header info                     */

    /* Computed/cached values (not stored) */
    haddr_t     addr;           /* Address of this data block page on disk      */
    size_t      size;           /* Size of data block page on disk              */
    size_t      nelmts;         /* Number of elements in data block page        */
} H5FA_dblk_page_t;

/* Fixed array */
struct H5FA_t {
    H5FA_hdr_t  *hdr;           /* Pointer to internal fixed array header info  */
    H5F_t      *f;              /* Pointer to file for fixed array              */
};


/* Metadata cache callback user data types */

/* Info needed for loading data block */
typedef struct H5FA_dblock_cache_ud_t {
    H5FA_hdr_t *hdr;            /* Shared fixed array information   */
    hsize_t nelmts;             /* Number of elements in data block */
} H5FA_dblock_cache_ud_t;

/* Info needed for loading data block page */
typedef struct H5FA_dblk_page_cache_ud_t {
    H5FA_hdr_t *hdr;            /* Shared fixed array information           */
    size_t nelmts;              /* Number of elements in data block page    */
} H5FA_dblk_page_cache_ud_t;


/*****************************/
/* Package Private Variables */
/*****************************/

/* H5FA header inherits cache-like properties from H5AC */
H5_DLLVAR const H5AC_class_t H5AC_FARRAY_HDR[1];

/* H5FA data block inherits cache-like properties from H5AC */
H5_DLLVAR const H5AC_class_t H5AC_FARRAY_DBLOCK[1];

/* H5FA data block page inherits cache-like properties from H5AC */
H5_DLLVAR const H5AC_class_t H5AC_FARRAY_DBLK_PAGE[1];

/* Internal fixed array testing class */
H5_DLLVAR const H5FA_class_t H5FA_CLS_TEST[1];

/* Array of fixed array client ID -> client class mappings */
H5_DLLVAR const H5FA_class_t *const H5FA_client_class_g[H5FA_NUM_CLS_ID];


/******************************/
/* Package Private Prototypes */
/******************************/

/* Generic routines */
H5_DLL herr_t H5FA__create_flush_depend(H5AC_info_t *parent_entry,
    H5AC_info_t *child_entry);
H5_DLL herr_t H5FA__destroy_flush_depend(H5AC_info_t *parent_entry,
    H5AC_info_t *child_entry);

/* Header routines */
H5_DLL H5FA_hdr_t *H5FA__hdr_alloc(H5F_t *f);
H5_DLL herr_t H5FA__hdr_init(H5FA_hdr_t *hdr, void *ctx_udata);
H5_DLL haddr_t H5FA__hdr_create(H5F_t *f, hid_t dxpl_id, const H5FA_create_t *cparam, void *ctx_udata);
H5_DLL void *H5FA__hdr_alloc_elmts(H5FA_hdr_t *hdr, size_t nelmts);
H5_DLL herr_t H5FA__hdr_free_elmts(H5FA_hdr_t *hdr, size_t nelmts, void *elmts);
H5_DLL herr_t H5FA__hdr_incr(H5FA_hdr_t *hdr);
H5_DLL herr_t H5FA__hdr_decr(H5FA_hdr_t *hdr);
H5_DLL herr_t H5FA__hdr_fuse_incr(H5FA_hdr_t *hdr);
H5_DLL size_t H5FA__hdr_fuse_decr(H5FA_hdr_t *hdr);
H5_DLL herr_t H5FA__hdr_modified(H5FA_hdr_t *hdr);
H5_DLL herr_t H5FA__hdr_delete(H5FA_hdr_t *hdr, hid_t dxpl_id);
H5_DLL herr_t H5FA__hdr_dest(H5FA_hdr_t *hdr);

/* Data block routines */
H5_DLL H5FA_dblock_t *H5FA__dblock_alloc(H5FA_hdr_t *hdr, hsize_t nelmts);
H5_DLL haddr_t H5FA__dblock_create(H5FA_hdr_t *hdr, hid_t dxpl_id, hbool_t *hdr_dirty, hsize_t nelmts);
H5_DLL unsigned H5FA__dblock_sblk_idx(const H5FA_hdr_t *hdr, hsize_t idx);
H5_DLL H5FA_dblock_t *H5FA__dblock_protect(H5FA_hdr_t *hdr, hid_t dxpl_id,
    haddr_t dblk_addr, hsize_t dblk_nelmts, H5AC_protect_t rw);
H5_DLL herr_t H5FA__dblock_unprotect(H5FA_dblock_t *dblock, hid_t dxpl_id,
    unsigned cache_flags);
H5_DLL herr_t H5FA__dblock_delete(H5FA_hdr_t *hdr, hid_t dxpl_id,
    haddr_t dblk_addr, hsize_t dblk_nelmts);
H5_DLL herr_t H5FA__dblock_dest(H5FA_dblock_t *dblock);

/* Data block page routines */
H5_DLL herr_t H5FA__dblk_page_create(H5FA_hdr_t *hdr, hid_t dxpl_id,
    haddr_t addr, size_t nelmts);
H5_DLL H5FA_dblk_page_t *H5FA__dblk_page_alloc(H5FA_hdr_t *hdr, size_t nelmts);
H5_DLL H5FA_dblk_page_t *H5FA__dblk_page_protect(H5FA_hdr_t *hdr, hid_t dxpl_id,
    haddr_t dblk_page_addr, size_t dblk_page_nelmts, H5AC_protect_t rw);
H5_DLL herr_t H5FA__dblk_page_unprotect(H5FA_dblk_page_t *dblk_page,
    hid_t dxpl_id, unsigned cache_flags);
H5_DLL herr_t H5FA__dblk_page_dest(H5FA_dblk_page_t *dblk_page);

/* Debugging routines for dumping file structures */
H5_DLL herr_t H5FA__hdr_debug(H5F_t *f, hid_t dxpl_id, haddr_t addr,
    FILE *stream, int indent, int fwidth, const H5FA_class_t *cls, haddr_t obj_addr);
H5_DLL herr_t H5FA__dblock_debug(H5F_t *f, hid_t dxpl_id, haddr_t addr,
    FILE *stream, int indent, int fwidth, const H5FA_class_t *cls,
    haddr_t hdr_addr, haddr_t obj_addr);

/* Testing routines */
#ifdef H5FA_TESTING
H5_DLL herr_t H5FA_get_cparam_test(const H5FA_t *ea, H5FA_create_t *cparam);
H5_DLL int H5FA_cmp_cparam_test(const H5FA_create_t *cparam1, const H5FA_create_t *cparam2);
#endif /* H5FA_TESTING */

#endif /* _H5FApkg_H */

