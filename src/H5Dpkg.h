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

/* [Simple] Macro to construct a H5D_io_info_t from it's components */
#define H5D_BUILD_IO_INFO(io_info,ds,dxpl_c,dxpl_i,str)                 \
    (io_info)->dset=ds;                                                 \
    (io_info)->dxpl_cache=dxpl_c;                                       \
    (io_info)->dxpl_id=dxpl_i;                                          \
    (io_info)->store=str

/****************************/
/* Package Private Typedefs */
/****************************/

/* The raw data chunk cache */
typedef struct H5D_rdcc_t {
#ifdef H5D_ISTORE_DEBUG
    unsigned		ninits;	/* Number of chunk creations		*/
    unsigned		nhits;	/* Number of cache hits			*/
    unsigned		nmisses;/* Number of cache misses		*/
    unsigned		nflushes;/* Number of cache flushes		*/
#endif /* H5D_ISTORE_DEBUG */
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
 * A dataset is made of two layers, an H5D_t struct that is unique to
 * each instance of an opened datset, and a shared struct that is only
 * created once for a given dataset.  Thus, if a dataset is opened twice,
 * there will be two IDs and two H5D_t structs, both sharing one H5D_shared_t.
 */
typedef struct H5D_shared_t {
    size_t              fo_count;       /* reference count */
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
} H5D_shared_t;

struct H5D_t {
    H5G_entry_t         ent;            /* cached object header stuff   */
    H5D_shared_t        *shared;        /* cached information from file */
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
H5_DLL herr_t H5D_contig_create(H5F_t *f, hid_t dxpl_id, H5O_layout_t *layout);
H5_DLL herr_t H5D_contig_fill(H5D_t *dset, hid_t dxpl_id);

/* Functions that operate on indexed storage */
H5_DLL herr_t H5D_istore_init (const H5F_t *f, H5D_t *dset);
H5_DLL herr_t H5D_istore_flush (H5D_t *dset, hid_t dxpl_id, unsigned flags);
H5_DLL herr_t H5D_istore_create(H5F_t *f, hid_t dxpl_id, H5O_layout_t *layout);
H5_DLL herr_t H5D_istore_dest (H5D_t *dset, hid_t dxpl_id);
H5_DLL herr_t H5D_istore_allocate (H5D_t *dset, hid_t dxpl_id,
        hbool_t full_overwrite);
H5_DLL hsize_t H5D_istore_allocated(H5D_t *dset, hid_t dxpl_id);
H5_DLL herr_t H5D_istore_prune_by_extent(H5D_io_info_t *io_info);
H5_DLL herr_t H5D_istore_initialize_by_extent(H5D_io_info_t *io_info);
H5_DLL herr_t H5D_istore_update_cache(H5D_t *dset, hid_t dxpl_id);
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
