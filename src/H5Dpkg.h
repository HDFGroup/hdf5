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
 *		Monday, April 14, 2003
 *
 * Purpose:	This file contains declarations which are visible only within
 *		the H5D package.  Source files outside the H5D package should
 *		include H5Dprivate.h instead.
 */
#ifndef H5D_PACKAGE
#error "Do not include this file outside the H5D package!"
#endif

#ifndef _H5Dpkg_H
#define _H5Dpkg_H

/* Get package's private header */
#include "H5Dprivate.h"

/* Other private headers needed by this file */
#include "H5Gprivate.h"		/* Groups 			  	*/
#include "H5Oprivate.h"		/* Object headers		  	*/
#include "H5Sprivate.h"		/* Dataspaces 				*/
#include "H5Tprivate.h"		/* Datatype functions			*/

/**************************/
/* Package Private Macros */
/**************************/

/* The number of reserved IDs in dataset ID group */
#define H5D_RESERVED_ATOMS  0

/* Set the minimum object header size to create objects with */
#define H5D_MINHDR_SIZE 256

/****************************/
/* Package Private Typedefs */
/****************************/

/* The raw data chunk cache */
typedef struct H5D_rdcc_t {
    unsigned		ninits;	/* Number of chunk creations		*/
    unsigned		nhits;	/* Number of cache hits			*/
    unsigned		nmisses;/* Number of cache misses		*/
    unsigned		nflushes;/* Number of cache flushes		*/
    size_t		nbytes;	/* Current cached raw data in bytes	*/
    size_t		nslots;	/* Number of chunk slots allocated	*/
    struct H5D_rdcc_ent_t *head; /* Head of doubly linked list		*/
    struct H5D_rdcc_ent_t *tail; /* Tail of doubly linked list		*/
    int		nused;	/* Number of chunk slots in use		*/
    struct H5D_rdcc_ent_t **slot; /* Chunk slots, each points to a chunk*/
} H5D_rdcc_t;

/* The raw data contiguous data cache */
typedef struct H5D_rdcdc_t {
    unsigned char *sieve_buf;   /* Buffer to hold data sieve buffer */
    haddr_t sieve_loc;          /* File location (offset) of the data sieve buffer */
    size_t sieve_size;          /* Size of the data sieve buffer used (in bytes) */
    size_t sieve_buf_size;      /* Size of the data sieve buffer allocated (in bytes) */
    unsigned sieve_dirty;       /* Flag to indicate that the data sieve buffer is dirty */
} H5D_rdcdc_t;

/*
 * A dataset is the following struct.
 */
struct H5D_t {
    H5G_entry_t         ent;            /* cached object header stuff   */
    hid_t               type_id;        /* ID for dataset's datatype    */
    H5T_t              *type;           /* datatype of this dataset     */
    H5S_t              *space;          /* dataspace of this dataset    */
    hid_t               dcpl_id;        /* dataset creation property id */
    H5D_dcpl_cache_t    dcpl_cache;     /* Cached DCPL values */
    H5O_layout_t        layout;         /* data layout                  */
    hbool_t             checked_filters;/* TRUE if dataset passes can_apply check */

    /* Cache some frequently accessed values from the DCPL */
    H5O_efl_t           efl;            /* External file list information */
    H5D_alloc_time_t    alloc_time;     /* Dataset allocation time      */
    H5D_fill_time_t	fill_time;	/* Dataset fill value writing time */
    H5O_fill_t          fill;           /* Dataset fill value information */

    /* Buffered/cached information for types of raw data storage*/
    struct {
        H5D_rdcdc_t     contig;         /* Information about contiguous data */
                                        /* (Note that the "contig" cache
                                         * information can be used by a chunked
                                         * dataset in certain circumstances)
                                         */
        H5D_rdcc_t      chunk;          /* Information about chunked data */
    }cache;
};

/* Enumerated type for allocating dataset's storage */
typedef enum {
    H5D_ALLOC_CREATE,           /* Dataset is being created */
    H5D_ALLOC_OPEN,             /* Dataset is being opened */
    H5D_ALLOC_EXTEND,           /* Dataset's dataspace is being extended */
    H5D_ALLOC_WRITE             /* Dataset is being extended */
} H5D_time_alloc_t;

/*****************************/
/* Package Private Variables */
/*****************************/
extern H5D_dxpl_cache_t H5D_def_dxpl_cache;

/******************************/
/* Package Private Prototypes */
/******************************/

H5_DLL herr_t H5D_alloc_storage (H5F_t *f, hid_t dxpl_id, H5D_t *dset, H5D_time_alloc_t time_alloc,
                        hbool_t update_time, hbool_t full_overwrite);

/* Functions that operate on contiguous storage */
H5_DLL herr_t H5D_contig_create(H5F_t *f, hid_t dxpl_id, H5D_t *dset);
H5_DLL herr_t H5D_contig_fill(H5F_t *f, hid_t dxpl_id, H5D_t *dset);

/* Functions that operate on indexed storage */
H5_DLL herr_t H5D_istore_init (H5F_t *f, H5D_t *dset);
H5_DLL herr_t H5D_istore_flush (H5F_t *f, hid_t dxpl_id, H5D_t *dset, unsigned flags);
H5_DLL herr_t H5D_istore_create(H5F_t *f, hid_t dxpl_id,
				 H5O_layout_t *layout/*in,out*/);
H5_DLL herr_t H5D_istore_dest (H5F_t *f, hid_t dxpl_id, H5D_t *dset);
H5_DLL herr_t H5D_istore_allocate (H5F_t *f, hid_t dxpl_id,
    const H5D_t *dset, hbool_t full_overwrite);
H5_DLL hsize_t H5D_istore_allocated(H5F_t *f, hid_t dxpl_id, H5D_t *dset);
H5_DLL herr_t H5D_istore_prune_by_extent( H5F_t *f,
        const H5D_dxpl_cache_t *dxpl_cache, hid_t dxpl_id, H5D_t *dset);
H5_DLL herr_t H5D_istore_initialize_by_extent( H5F_t *f,
        const H5D_dxpl_cache_t *dxpl_cache, hid_t dxpl_id, H5D_t *dset);
H5_DLL herr_t H5D_istore_update_cache(H5F_t *f, hid_t dxpl_id, H5D_t *dset);
H5_DLL herr_t H5D_istore_dump_btree(H5F_t *f, hid_t dxpl_id, FILE *stream, unsigned ndims,
        haddr_t addr);
#ifdef H5D_ISTORE_DEBUG
H5_DLL herr_t H5D_istore_stats (H5D_t *dset, hbool_t headers);
#endif /* H5D_ISTORE_DEBUG */

/* Testing functions */
#ifdef H5D_TESTING
H5_DLL herr_t H5D_layout_version_test(hid_t did, unsigned *version);
H5_DLL herr_t H5D_layout_contig_size_test(hid_t did, hsize_t *size);
#endif /* H5D_TESTING */

#endif /*_H5Dpkg_H*/
