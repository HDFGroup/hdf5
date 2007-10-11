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
#include "H5SLprivate.h"	/* Skip lists				*/
#include "H5Tprivate.h"		/* Datatypes         			*/

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

/*
 * If there is no data type conversion then it might be possible to
 * transfer data points between application memory and the file in one
 * step without going through the data type conversion buffer.
 */

/* Read from file to application w/o intermediate scratch buffer */
struct H5D_io_info_t;
typedef herr_t (*H5D_io_read_func_t)(struct H5D_io_info_t *io_info,
    size_t nelmts, size_t elmt_size,
    const H5S_t *file_space, const H5S_t *mem_space, haddr_t addr,
    void *chunk, void *buf/*out*/);


/* Write directly from app buffer to file */
typedef herr_t (*H5D_io_write_func_t)(struct H5D_io_info_t *io_info,
    size_t nelmts, size_t elmt_size,
    const H5S_t *file_space, const H5S_t *mem_space, haddr_t addr,
    void *chunk, const void *buf);

/* Function pointers for I/O on particular types of dataset layouts */
typedef ssize_t (*H5D_io_readvv_func_t)(const struct H5D_io_info_t *io_info,
    size_t dset_max_nseq, size_t *dset_curr_seq, size_t dset_len_arr[], hsize_t dset_offset_arr[],
    size_t mem_max_nseq, size_t *mem_curr_seq, size_t mem_len_arr[], hsize_t mem_offset_arr[],
    haddr_t chunk_addr, void *chunk, void *buf);
typedef ssize_t (*H5D_io_writevv_func_t)(const struct H5D_io_info_t *io_info,
    size_t dset_max_nseq, size_t *dset_curr_seq, size_t dset_len_arr[], hsize_t dset_offset_arr[],
    size_t mem_max_nseq, size_t *mem_curr_seq, size_t mem_len_arr[], hsize_t mem_offset_arr[],
    haddr_t chunk_addr, void *chunk, const void *buf);

/* Typedef for raw data I/O framework info */
typedef struct H5D_io_ops_t {
    H5D_io_read_func_t read;            /* Direct I/O routine for reading */
    H5D_io_write_func_t write;          /* Direct I/O routine for writing */
    H5D_io_readvv_func_t readvv;        /* I/O routine for reading data */
    H5D_io_writevv_func_t writevv;      /* I/O routine for writing data */
} H5D_io_ops_t;

/* Typedef for raw data I/O operation info */
typedef struct H5D_io_info_t {
    H5D_t *dset;                /* Pointer to dataset being operated on */
#ifndef H5_HAVE_PARALLEL
    const
#endif /* H5_HAVE_PARALLEL */
        H5D_dxpl_cache_t *dxpl_cache; /* Pointer to cache DXPL info */
    hid_t dxpl_id;              /* Original DXPL ID */
#ifdef H5_HAVE_PARALLEL
    MPI_Comm comm;              /* MPI communicator for file */
    hbool_t xfer_mode_changed;  /* Whether the transfer mode was changed */
    hbool_t xfer_opt_mode_changed;
#endif /* H5_HAVE_PARALLEL */
    const H5D_storage_t *store; /* Dataset storage info */
    H5D_io_ops_t ops;           /* I/O operation function pointers */
#ifdef H5S_DEBUG
    H5S_iostats_t *stats;       /* I/O statistics */
#endif /* H5S_DEBUG */
} H5D_io_info_t;

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
    H5D_io_ops_t        io_ops;         /* I/O operations */
    H5O_layout_t        layout;         /* data layout                  */
    hbool_t             checked_filters;/* TRUE if dataset passes can_apply check */

    /* Buffered/cached information for types of raw data storage*/
    struct {
        H5D_rdcdc_t     contig;         /* Information about contiguous data */
                                        /* (Note that the "contig" cache
                                         * information can be used by a chunked
                                         * dataset in certain circumstances)
                                         */
        H5D_rdcc_t      chunk;          /* Information about chunked data */
    } cache;
} H5D_shared_t;

struct H5D_t {
    H5O_loc_t           oloc;           /* Object header location       */
    H5G_name_t          path;           /* Group hierarchy path         */
    H5D_shared_t        *shared;        /* cached information from file */
};

/* Enumerated type for allocating dataset's storage */
typedef enum {
    H5D_ALLOC_CREATE,           /* Dataset is being created */
    H5D_ALLOC_OPEN,             /* Dataset is being opened */
    H5D_ALLOC_EXTEND,           /* Dataset's dataspace is being extended */
    H5D_ALLOC_WRITE             /* Dataset is being extended */
} H5D_time_alloc_t;


/* Structure holding information about a chunk's selection for mapping */
typedef struct H5D_chunk_info_t {
    hsize_t index;              /* "Index" of chunk in dataset */
    size_t chunk_points;        /* Number of elements selected in chunk */
    H5S_t *fspace;              /* Dataspace describing chunk & selection in it */
    hsize_t coords[H5O_LAYOUT_NDIMS];   /* Coordinates of chunk in file dataset's dataspace */
    H5S_t *mspace;              /* Dataspace describing selection in memory corresponding to this chunk */
    unsigned mspace_shared;     /* Indicate that the memory space for a chunk is shared and shouldn't be freed */
} H5D_chunk_info_t;

/* Main structure holding the mapping between file chunks and memory */
typedef struct fm_map {
    H5SL_t *fsel;               /* Skip list containing file dataspaces for all chunks */
    hsize_t last_index;         /* Index of last chunk operated on */
    H5D_chunk_info_t *last_chunk_info;  /* Pointer to last chunk's info */
    const H5S_t *file_space;    /* Pointer to the file dataspace */
    const H5S_t *mem_space;     /* Pointer to the memory dataspace */
    unsigned mem_space_copy;    /* Flag to indicate that the memory dataspace must be copied */
    hsize_t f_dims[H5O_LAYOUT_NDIMS];   /* File dataspace dimensions */
    H5S_t *mchunk_tmpl;         /* Dataspace template for new memory chunks */
    unsigned f_ndims;           /* Number of dimensions for file dataspace */
    H5S_sel_iter_t mem_iter;    /* Iterator for elements in memory selection */
    unsigned m_ndims;           /* Number of dimensions for memory dataspace */
    hsize_t chunks[H5O_LAYOUT_NDIMS];   /* Number of chunks in each dimension */
    hsize_t chunk_dim[H5O_LAYOUT_NDIMS];    /* Size of chunk in each dimension */
    hsize_t down_chunks[H5O_LAYOUT_NDIMS];   /* "down" size of number of chunks in each dimension */
    H5O_layout_t *layout;       /* Dataset layout information*/
    H5S_sel_type msel_type;     /* Selection type in memory */
    hsize_t total_chunks;       /* Number of total chunks */
    hbool_t *select_chunk;         /* store the information about whether this chunk is selected or not */
} fm_map;

/* Typedef for dataset creation operation */
typedef struct {
    hid_t type_id;              /* Datatype for dataset */
    const H5S_t *space;         /* Dataspace for dataset */
    hid_t dcpl_id;              /* Dataset creation property list */
} H5D_obj_create_t;

/* Typedef for filling a buffer with a fill value */
typedef struct {
    hbool_t     alloc_vl_during_refill; /* Whether to allocate VL-datatype fill buffer during refill */
    H5MM_allocate_t fill_alloc_func;    /* Routine to call for allocating fill buffer */
    void        *fill_alloc_info;       /* Extra info for allocation routine */
    H5MM_free_t fill_free_func;         /* Routine to call for freeing fill buffer */
    void        *fill_free_info;        /* Extra info for free routine */
    H5T_path_t *fill_to_mem_tpath;      /* Datatype conversion path for converting the fill value to the memory buffer */
    H5T_path_t *mem_to_dset_tpath;      /* Datatype conversion path for converting the memory buffer to the dataset elements */
    const H5O_fill_t *fill;             /* Pointer to fill value */
    void       *fill_buf;               /* Fill buffer */
    size_t      fill_buf_size;          /* Size of fill buffer */
    hbool_t     use_caller_fill_buf;    /* Whether the caller provided the fill buffer */
    void       *bkg_buf;                /* Background conversion buffer */
    size_t      bkg_buf_size;           /* Size of background buffer */
    H5T_t      *mem_type;               /* Pointer to memory datatype */
    const H5T_t *file_type;             /* Pointer to file datatype */
    hid_t       mem_tid;                /* ID for memory version of disk datatype */
    hid_t       file_tid;               /* ID for disk datatype */
    size_t      mem_elmt_size, file_elmt_size;       /* Size of element in memory and on disk */
    size_t      max_elmt_size;          /* Max. size of memory or file datatype */
    size_t      elmts_per_buf;          /* # of elements that fit into a buffer */
    hbool_t     has_vlen_fill_type;     /* Whether the datatype for the fill value has a variable-length component */
} H5D_fill_buf_info_t;

/*************************/
/* For chunk lock        */
/*************************/
/*
 * B-tree key.	A key contains the minimum logical N-dimensional address and
 * the logical size of the chunk to which this key refers.  The
 * fastest-varying dimension is assumed to reference individual bytes of the
 * array, so a 100-element 1-d array of 4-byte integers would really be a 2-d
 * array with the slow varying dimension of size 100 and the fast varying
 * dimension of size 4 (the storage dimensionality has very little to do with
 * the real dimensionality).
 *
 * Only the first few values of the OFFSET and SIZE fields are actually
 * stored on disk, depending on the dimensionality.
 *
 * The chunk's file address is part of the B-tree and not part of the key.
 */
typedef struct H5D_istore_key_t {
    size_t	nbytes;				/*size of stored data	*/
    hsize_t	offset[H5O_LAYOUT_NDIMS];	/*logical offset to start*/
    unsigned	filter_mask;			/*excluded filters	*/
} H5D_istore_key_t;


/*
 * Common data exchange structure for indexed storage nodes.  This structure is
 * passed through the B-link tree layer to the methods for the objects
 * to which the B-link tree points.
 */
typedef struct H5D_istore_bt_ud_common_t {
    /* downward */
    H5D_istore_key_t	key;	                /*key values		*/
    const H5O_layout_t	*mesg;		        /*layout message	*/
} H5D_istore_bt_ud_common_t;

/* B-tree callback info for various operations */
typedef struct H5D_istore_ud1_t {
    H5D_istore_bt_ud_common_t common;           /* Common info for B-tree user data (must be first) */
    haddr_t		addr;			/*file address of chunk */
} H5D_istore_ud1_t; 

/* Internal data structure for computing variable-length dataset's total size */
typedef struct {
    hid_t dataset_id;   /* ID of the dataset we are working on */
    hid_t fspace_id;    /* ID of the file dataset's dataspace we are working on */
    hid_t mspace_id;    /* ID of the memory dataset's dataspace we are working on */
    void *fl_tbuf;      /* Ptr to the temporary buffer we are using for fixed-length data */
    void *vl_tbuf;      /* Ptr to the temporary buffer we are using for VL data */
    hid_t xfer_pid;     /* ID of the dataset xfer property list */
    hsize_t size;       /* Accumulated number of bytes for the selection */
} H5D_vlen_bufsize_t;


/*****************************/
/* Package Private Variables */
/*****************************/
extern H5D_dxpl_cache_t H5D_def_dxpl_cache;


/******************************/
/* Package Private Prototypes */
/******************************/

H5_DLL H5D_t *H5D_create(H5F_t *file, hid_t type_id, const H5S_t *space,
    hid_t dcpl_id, hid_t dxpl_id);
H5_DLL H5D_t *H5D_create_named(const H5G_loc_t *loc, const char *name,
    hid_t type_id, const H5S_t *space, hid_t lcpl_id, hid_t dcpl_id,
    hid_t dapl_id, hid_t dxpl_id);
H5_DLL herr_t H5D_get_space_status(H5D_t *dset, H5D_space_status_t *allocation,
    hid_t dxpl_id);
H5_DLL herr_t H5D_alloc_storage(H5F_t *f, hid_t dxpl_id, H5D_t *dset, H5D_time_alloc_t time_alloc,
    hbool_t update_time, hbool_t full_overwrite);
H5_DLL hsize_t H5D_get_storage_size(H5D_t *dset, hid_t dxpl_id);
H5_DLL haddr_t H5D_get_offset(const H5D_t *dset);
H5_DLL herr_t H5D_iterate(void *buf, hid_t type_id, const H5S_t *space,
    H5D_operator_t op, void *operator_data);
H5_DLL void * H5D_vlen_get_buf_size_alloc(size_t size, void *info);
H5_DLL herr_t H5D_vlen_get_buf_size(void *elem, hid_t type_id, unsigned ndim,
    const hsize_t *point, void *op_data);
H5_DLL herr_t H5D_check_filters(H5D_t *dataset);
H5_DLL herr_t H5D_set_extent(H5D_t *dataset, const hsize_t *size, hid_t dxpl_id);

/* Functions that perform serial I/O operations */
H5_DLL herr_t H5D_select_fscat(H5D_io_info_t *io_info,
    const H5S_t *file_space, H5S_sel_iter_t *file_iter, size_t nelmts,
    haddr_t chunk_addr, void *chunk, const void *_buf);
H5_DLL size_t H5D_select_fgath(H5D_io_info_t *io_info,
    const H5S_t *file_space, H5S_sel_iter_t *file_iter, size_t nelmts,
    haddr_t chunk_addr, void *chunk, void *buf);
H5_DLL herr_t H5D_select_mscat(const void *_tscat_buf,
    const H5S_t *space, H5S_sel_iter_t *iter, size_t nelmts,
    const H5D_dxpl_cache_t *dxpl_cache, void *_buf/*out*/);
H5_DLL size_t H5D_select_mgath(const void *_buf,
    const H5S_t *space, H5S_sel_iter_t *iter, size_t nelmts,
    const H5D_dxpl_cache_t *dxpl_cache, void *_tgath_buf/*out*/);
H5_DLL herr_t H5D_select_read(H5D_io_info_t *io_info,
    size_t nelmts, size_t elmt_size,
    const H5S_t *file_space, const H5S_t *mem_space,
    haddr_t addr, void *chunk/*in*/, void *buf/*out*/);
H5_DLL herr_t H5D_select_write(H5D_io_info_t *io_info,
    size_t nelmts, size_t elmt_size,
    const H5S_t *file_space, const H5S_t *mem_space,
    haddr_t addr, void *chunk/*in*/, const void *buf/*out*/);

/* Functions that operate on contiguous storage */
H5_DLL herr_t H5D_contig_create(H5F_t *f, hid_t dxpl_id, H5O_layout_t *layout);
H5_DLL herr_t H5D_contig_fill(H5D_t *dset, hid_t dxpl_id);
H5_DLL haddr_t H5D_contig_get_addr(const H5D_t *dset);
H5_DLL ssize_t H5D_contig_readvv(const H5D_io_info_t *io_info,
    size_t dset_max_nseq, size_t *dset_curr_seq, size_t dset_len_arr[], hsize_t dset_offset_arr[],
    size_t mem_max_nseq, size_t *mem_curr_seq, size_t mem_len_arr[], hsize_t mem_offset_arr[],
    haddr_t UNUSED address, void UNUSED *pointer, void *buf);
H5_DLL ssize_t H5D_contig_writevv(const H5D_io_info_t *io_info,
    size_t dset_max_nseq, size_t *dset_curr_seq, size_t dset_len_arr[], hsize_t dset_offset_arr[],
    size_t mem_max_nseq, size_t *mem_curr_seq, size_t mem_len_arr[], hsize_t mem_offset_arr[],
    haddr_t UNUSED address, void UNUSED *pointer, const void *buf);
H5_DLL herr_t H5D_contig_copy(H5F_t *f_src, const H5O_layout_t *layout_src, H5F_t *f_dst, 
    H5O_layout_t *layout_dst, H5T_t *src_dtype, H5O_copy_t *cpy_info, hid_t dxpl_id);

/* Functions that operate on compact dataset storage */
H5_DLL herr_t H5D_compact_fill(H5D_t *dset, hid_t dxpl_id);
H5_DLL ssize_t H5D_compact_readvv(const H5D_io_info_t *io_info,
    size_t dset_max_nseq, size_t *dset_curr_seq, size_t dset_size_arr[], hsize_t dset_offset_arr[],
    size_t mem_max_nseq, size_t *mem_curr_seq, size_t mem_size_arr[], hsize_t mem_offset_arr[],
    haddr_t UNUSED addr, void UNUSED *pointer/*in*/, void *buf);
H5_DLL ssize_t H5D_compact_writevv(const H5D_io_info_t *io_info,
    size_t dset_max_nseq, size_t *dset_curr_seq, size_t dset_size_arr[], hsize_t dset_offset_arr[],
    size_t mem_max_nseq, size_t *mem_curr_seq, size_t mem_size_arr[], hsize_t mem_offset_arr[],
    haddr_t UNUSED addr, void UNUSED *pointer/*in*/, const void *buf);
H5_DLL herr_t H5D_compact_copy(H5F_t *f_src, H5O_layout_t *layout_src,
    H5F_t *f_dst, H5O_layout_t *layout_dst, H5T_t *src_dtype, H5O_copy_t *cpy_info, hid_t dxpl_id);

/* Functions that operate on indexed storage */
/* forward reference for collective-chunk IO use */
struct H5D_istore_ud1_t; /*define in H5Distore.c*/
H5_DLL herr_t H5D_istore_init (const H5F_t *f, const H5D_t *dset);
H5_DLL herr_t H5D_istore_flush (H5D_t *dset, hid_t dxpl_id, unsigned flags);
H5_DLL herr_t H5D_istore_create(H5F_t *f, hid_t dxpl_id, H5O_layout_t *layout);
H5_DLL herr_t H5D_istore_dest (H5D_t *dset, hid_t dxpl_id);
H5_DLL herr_t H5D_istore_allocate (H5D_t *dset, hid_t dxpl_id,
        hbool_t full_overwrite);
H5_DLL hsize_t H5D_istore_allocated(H5D_t *dset, hid_t dxpl_id);
H5_DLL herr_t H5D_istore_bh_info(H5F_t *f, hid_t dxpl_id, H5O_layout_t *layout,
    hsize_t *btree_size);
H5_DLL herr_t H5D_istore_prune_by_extent(const H5D_io_info_t *io_info,
    const hsize_t *old_dims);
H5_DLL herr_t H5D_istore_initialize_by_extent(H5D_io_info_t *io_info);
H5_DLL herr_t H5D_istore_update_cache(H5D_t *dset, hid_t dxpl_id);
H5_DLL herr_t H5D_istore_dump_btree(H5F_t *f, hid_t dxpl_id, FILE *stream, unsigned ndims,
        haddr_t addr);
H5_DLL herr_t H5D_istore_chunkmap(const H5D_io_info_t *io_info, haddr_t chunk_addr[],hsize_t down_chunks[] );
#ifdef H5D_ISTORE_DEBUG
H5_DLL herr_t H5D_istore_stats (H5D_t *dset, hbool_t headers);
#endif /* H5D_ISTORE_DEBUG */
H5_DLL ssize_t H5D_istore_readvv(const H5D_io_info_t *io_info,
    size_t chunk_max_nseq, size_t *chunk_curr_seq, size_t chunk_len_arr[],
    hsize_t chunk_offset_arr[], size_t mem_max_nseq, size_t *mem_curr_seq,
    size_t mem_len_arr[], hsize_t mem_offset_arr[], haddr_t chunk_addr, void *chunk, void *buf);
H5_DLL ssize_t H5D_istore_writevv(const H5D_io_info_t *io_info,
    size_t chunk_max_nseq, size_t *chunk_curr_seq, size_t chunk_len_arr[],
    hsize_t chunk_offset_arr[], size_t mem_max_nseq, size_t *mem_curr_seq,
    size_t mem_len_arr[], hsize_t mem_offset_arr[], haddr_t chunk_addr, void *chunk, 
    const void *buf);
H5_DLL haddr_t H5D_istore_get_addr(const H5D_io_info_t *io_info,
    struct H5D_istore_ud1_t *_udata);
H5_DLL herr_t H5D_istore_copy(H5F_t *f_src, H5O_layout_t *layout_src,
    H5F_t *f_dst, H5O_layout_t *layout_dst, H5T_t *src_dtype,
    H5O_copy_t *cpy_info, H5O_pline_t *pline, hid_t dxpl_id);
H5_DLL void * H5D_istore_lock(const H5D_io_info_t *io_info, H5D_istore_ud1_t *udata,
    hbool_t relax, unsigned *idx_hint/*in,out*/);
H5_DLL herr_t H5D_istore_unlock(const H5D_io_info_t *io_info,
    hbool_t dirty, unsigned idx_hint, void *chunk, size_t naccessed);
H5_DLL hbool_t H5D_istore_if_load(H5D_t *dataset, haddr_t caddr);

/* Functions that operate on external file list (efl) storage */
H5_DLL ssize_t H5D_efl_readvv(const H5D_io_info_t *io_info,
    size_t dset_max_nseq, size_t *dset_curr_seq, size_t dset_len_arr[], hsize_t dset_offset_arr[],
    size_t mem_max_nseq, size_t *mem_curr_seq, size_t mem_len_arr[], hsize_t mem_offset_arr[],
    haddr_t UNUSED addr, void UNUSED *pointer/*in*/, void *buf);
H5_DLL ssize_t H5D_efl_writevv(const H5D_io_info_t *io_info,
    size_t dset_max_nseq, size_t *dset_curr_seq, size_t dset_len_arr[], hsize_t dset_offset_arr[],
    size_t mem_max_nseq, size_t *mem_curr_seq, size_t mem_len_arr[], hsize_t mem_offset_arr[],
    haddr_t UNUSED addr, void UNUSED *pointer/*in*/, const void *buf);

/* Functions that perform fill value operations on datasets */
H5_DLL herr_t H5D_fill(const void *fill, const H5T_t *fill_type, void *buf,
    const H5T_t *buf_type, const H5S_t *space, hid_t dxpl_id);
H5_DLL herr_t H5D_fill_init(H5D_fill_buf_info_t *fb_info, void *caller_fill_buf,
    hbool_t alloc_vl_during_refill,
    H5MM_allocate_t alloc_func, void *alloc_info,
    H5MM_free_t free_func, void *free_info,
    const H5O_fill_t *fill, const H5T_t *dset_type, hid_t dset_type_id,
    size_t nelmts, size_t min_buf_size, hid_t dxpl_id);
H5_DLL herr_t H5D_fill_refill_vl(H5D_fill_buf_info_t *fb_info, size_t nelmts,
    hid_t dxpl_id);
H5_DLL herr_t H5D_fill_release(H5D_fill_buf_info_t *fb_info);
H5_DLL herr_t H5D_fill_term(H5D_fill_buf_info_t *fb_info);

#ifdef H5_HAVE_PARALLEL

#ifdef H5S_DEBUG
#ifndef H5Dmpio_DEBUG
#define H5Dmpio_DEBUG
#endif /*H5Dmpio_DEBUG*/
#endif/*H5S_DEBUG*/
/* MPI-IO function to read , it will select either regular or irregular read */
H5_DLL herr_t H5D_mpio_select_read(H5D_io_info_t *io_info,
    size_t nelmts, size_t elmt_size,
    const struct H5S_t *file_space, const struct H5S_t *mem_space,
    haddr_t addr, void UNUSED *pointer/*in*/, void *buf/*out*/);

/* MPI-IO function to read , it will select either regular or irregular read */
H5_DLL herr_t H5D_mpio_select_write(H5D_io_info_t *io_info,
    size_t nelmts, size_t elmt_size,
    const struct H5S_t *file_space, const struct H5S_t *mem_space,
    haddr_t addr, void UNUSED *pointer/*in*/, const void *buf);

/* MPI-IO function to handle contiguous collective IO */
H5_DLL herr_t
H5D_contig_collective_io(H5D_io_info_t *io_info,
			 const H5S_t *file_space,const H5S_t *mem_space,
			 const void *_buf,hbool_t do_write);

/* MPI-IO function to handle chunked collective IO */
H5_DLL herr_t
H5D_chunk_collective_io(H5D_io_info_t * io_info,fm_map *fm, const void*buf,
			hbool_t do_write);
/* MPI-IO function to check if a direct I/O transfer is possible between
 * memory and the file */
H5_DLL htri_t H5D_mpio_opt_possible(const H5D_io_info_t *io_info, const H5S_t *mem_space,
    const H5S_t *file_space, const H5T_path_t *tpath);

#ifndef H5_MPI_SPECIAL_COLLECTIVE_IO_WORKS
H5_DLL herr_t  H5D_mpio_chunk_adjust_iomode(H5D_io_info_t *io_info,const fm_map *fm);
#endif /* H5_MPI_SPECIAL_COLLECTIVE_IO_WORKS */

#endif /* H5_HAVE_PARALLEL */

/* Testing functions */
#ifdef H5D_TESTING
H5_DLL herr_t H5D_layout_version_test(hid_t did, unsigned *version);
H5_DLL herr_t H5D_layout_contig_size_test(hid_t did, hsize_t *size);
#endif /* H5D_TESTING */

#endif /*_H5Dpkg_H*/

