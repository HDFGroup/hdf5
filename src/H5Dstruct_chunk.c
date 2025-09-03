/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/* Purpose: Abstract indexed (chunked) I/O functions.  The logical
 *          multi-dimensional dataspace is regularly partitioned into
 *          same-sized "chunks", the first of which is aligned with the
 *          logical origin.  The chunks are indexed by different methods,
 *          that map a chunk index to disk address.  Each chunk can be
 *          compressed independently and the chunks may move around in the
 *          file as their storage requirements change.
 *
 * Cache:   Disk I/O is performed in units of chunks and H5MF_alloc()
 *          contains code to optionally align chunks on disk block
 *          boundaries for performance.
 *
 *          The chunk cache is an extendible hash indexed by a function
 *          of storage B-tree address and chunk N-dimensional offset
 *          within the dataset.  Collisions are not resolved -- one of
 *          the two chunks competing for the hash slot must be preempted
 *          from the cache.  All entries in the hash also participate in
 *          a doubly-linked list and entries are penalized by moving them
 *          toward the front of the list.  When a new chunk is about to
 *          be added to the cache the heap is pruned by preempting
 *          entries near the front of the list to make room for the new
 *          entry which is added to the end of the list.
 */

/****************/
/* Module Setup */
/****************/

#include "H5Dmodule.h" /* This source code file is part of the H5D module */
#define H5SC_FRIEND    /*suppress error about including H5SCpkg    */

/***********/
/* Headers */
/***********/
#include "H5private.h" /* Generic Functions            */
#ifdef H5_HAVE_PARALLEL
#include "H5ACprivate.h" /* Metadata cache            */
#endif                   /* H5_HAVE_PARALLEL */
#include "H5CXprivate.h" /* API Contexts                         */
#include "H5Dpkg.h"      /* Dataset functions            */
#include "H5SCpkg.h"     /* Shared chunk cache functions            */
#include "H5Eprivate.h"  /* Error handling              */
#include "H5Fprivate.h"  /* File functions            */
#include "H5FLprivate.h" /* Free Lists                           */
#include "H5Iprivate.h"  /* IDs                      */
#include "H5MMprivate.h" /* Memory management            */
#include "H5MFprivate.h" /* File memory management               */
#include "H5PBprivate.h" /* Page Buffer	                         */
#include "H5SLprivate.h" /* Skip Lists                               */
#include "H5VMprivate.h" /* Vector and array functions        */

/****************/
/* Local Macros */
/****************/

/* Length of sequence lists requested from dataspace selections */
#define SEQ_LIST_LEN 128

/* Length of allocated arrays for building vector I/O operations */
#define VECTOR_LEN 8

/* Sanity check on chunk index types */
#define H5D_STRUCT_CHUNK_STORAGE_INDEX_CHK(storage)                                                          \
    do {                                                                                                     \
        assert((H5D_CHUNK_IDX_EARRAY == (storage)->idx_type &&                                               \
                H5D_COPS_STRUCT_CHUNK_EARRAY == (storage)->ops) ||                                           \
               (H5D_CHUNK_IDX_FARRAY == (storage)->idx_type &&                                               \
                H5D_COPS_STRUCT_CHUNK_FARRAY == (storage)->ops) ||                                           \
               (H5D_CHUNK_IDX_BT2 == (storage)->idx_type && H5D_COPS_STRUCT_CHUNK_BT2 == (storage)->ops) ||  \
               (H5D_CHUNK_IDX_SINGLE == (storage)->idx_type &&                                               \
                H5D_COPS_STRUCT_CHUNK_SINGLE == (storage)->ops));                                            \
    } while (0)

/******************/
/* Local Typedefs */
/******************/

/* Intermediate struct for the chunk cache memory format */
typedef struct H5D_chunk_cache_mem_t {
    void  *data_buf;  /* Buffer pointer to the data values */
    void  *sel_buf;   /* Buffer pointer to the encoded selection */
    H5S_t *sel_space; /* Dataspace for encoded selection */
    /* size tracking */
    size_t  sel_nbytes;      /* nbytes for selection */
    size_t  sel_alloc_size;  /* alloc_size for selection */
    size_t data_nbytes;      /* nbytes for data values */
    size_t data_alloc_size;  /* alloc_size for data values */
} H5D_chunk_cache_mem_t;

/********************/
/* Local Prototypes */
/********************/

/*
 * Layout I/O callbacks for structured chunk
 */
static herr_t H5D__struct_chunk_construct(H5F_t H5_ATTR_UNUSED *f, H5D_t *dset);
static herr_t H5D__struct_chunk_init(H5F_t *f, const H5D_t *const dset, hid_t dapl_id);
static bool   H5D__struct_chunk_is_space_alloc(const H5O_storage_t *storage);
static herr_t H5D__struct_chunk_io_init(H5D_io_info_t *io_info, H5D_dset_io_info_t *dinfo);
static herr_t H5D__struct_chunk_mdio_init(H5D_io_info_t *io_info, H5D_dset_io_info_t *dinfo);
static herr_t H5D__struct_chunk_io_term(H5D_io_info_t H5_ATTR_UNUSED *io_info, H5D_dset_io_info_t *di);
static herr_t H5D__struct_chunk_dest(H5D_t *dset);

/* Helper routines for above layout callbacks */
static herr_t H5D__struct_chunk_may_use_select_io(H5D_io_info_t            *io_info,
                                                  const H5D_dset_io_info_t *dset_info);
static herr_t H5D__struct_chunk_io_init_selections(H5D_io_info_t *io_info, H5D_dset_io_info_t *dinfo);
static herr_t H5D__struct_chunk_set_info_real(H5O_layout_struct_chunk_t *layout, unsigned ndims,
                                              const hsize_t *curr_dims, const hsize_t *max_dims);
static herr_t H5D__struct_chunk_set_info(const H5D_t *dset);

/*
 *  Shared chunk cache layout callbacks for structured chunk
 *
 */
static herr_t H5D__struct_chunk_lookup(H5D_t *dset, size_t count, const hsize_t *scaled[] /*in*/,
                                       haddr_t *addr[] /*out*/, hsize_t *size[] /*out*/,
                                       hsize_t *defined_values_size[] /*out*/, size_t *size_hint[] /*out*/,
                                       size_t *defined_values_size_hint[] /*out*/, void **udata[] /*out*/);

static herr_t H5D__struct_chunk_decode(H5D_t *dset, size_t *nbytes /*in,out*/, size_t *alloc_size /*in,out*/,
                                       bool partial_bound, void **chunk /*in,out*/, void *udata);

static herr_t H5D__struct_chunk_decode_defined_values(H5D_t *dset, size_t *nbytes /*in,out*/,
                                                      size_t *alloc_size /*in,out*/, bool partial_bound,
                                                      void **chunk /*in,out*/, void *udata);

static herr_t H5D__struct_chunk_new_chunk(H5D_t *dset, bool fill, size_t *nbytes /*out*/,
                                          size_t *buf_size /*out*/, void **chunk /*chunk*/,
                                          void **udata /*out*/);

static herr_t H5D__struct_chunk_condense(H5D_t *dset, size_t *nbytes /*in, out*/, void **chunk /*in, out*/,
                                         void *udata);

static herr_t H5D__struct_chunk_encode(H5D_t *dset, hsize_t *write_size /*out*/,
                                       hsize_t *write_buf_alloc /*out*/, bool partial_bound,
                                       const void *chunk, void *udata, void **write_buf /*out*/);

static herr_t H5D__struct_chunk_encode_in_place(H5D_t *dset, size_t *write_size /*out*/, bool partial_bound,
                                                void **chunk /*in,out*/, void *udata);

static herr_t H5D__struct_chunk_evict(H5D_t *dset, void *chunk, void *udata);

static herr_t H5D__struct_chunk_insert(H5D_t *dset, size_t count, const hsize_t *scaled[] /*in*/,
                                       haddr_t *addr[] /*in,out*/, hsize_t old_disk_size[],
                                       hsize_t new_disk_size[], void *chunk[] /*in*/, void *udata[]);

static herr_t H5D__struct_chunk_vector_read(H5D_t *dset, haddr_t addr, const H5S_t *file_space_in,
                                            bool partial_bound, void *chunk /*in*/, size_t *vec_count /*out*/,
                                            haddr_t **offsets /*out*/, size_t **sizes /*out*/,
                                            bool *vector_possible /*out*/, bool *require_values /*out*/,
                                            void *udata);

static herr_t H5D__struct_chunk_vector_write(H5D_t *dset, haddr_t addr, const H5S_t *file_space_in,
                                             bool partial_bound, void *chunk /*in*/,
                                             size_t *vec_count /*out*/, haddr_t **offsets /*out*/,
                                             size_t **sizes /*out*/, bool *vector_possible /*out*/,
                                             bool *require_values /*out*/, void *udata);

static herr_t H5D__struct_chunk_scatter_mem(H5D_dset_io_info_t *dset_info, H5D_io_type_info_t *io_type_info,
                                            const H5S_t *mem_space, const H5S_t *file_space,
                                            const void *chunk, void *udata);

static herr_t H5D__struct_chunk_gather_mem(H5D_dset_io_info_t *dset_info, H5D_io_type_info_t *io_type_info,
                                           const H5S_t *mem_space, const H5S_t *file_space,
                                           size_t *nbytes /*in,out*/, size_t *alloc_size /*in,out*/,
                                           size_t *alloc_size_total /*in,out*/, void *chunk, void *udata);

static herr_t H5D__struct_chunk_fill(H5D_dset_io_info_t *dset_info, H5D_io_type_info_t *io_type_info,
                                     H5S_t *space, size_t *nbytes /*in,out*/, size_t *alloc_size /*in,out*/,
                                     size_t *alloc_size_total /*in,out*/, void *chunk, void *udata);

static herr_t H5D__struct_chunk_defined_values(H5D_t *dset, const H5S_t *selection, void *chunk,
                                               H5S_t **defined_values /*out*/, void *udata);

static herr_t H5D__struct_chunk_erase_values(H5D_t *dset, const H5S_t *selection, size_t *nbytes /*in,out*/,
                                             size_t *alloc_size /*in,out*/, void *chunk,
                                             bool *delete_chunk /*out*/, void *udata);

static herr_t H5D__struct_chunk_evict_values(H5D_t *dset, size_t *nbytes /*in,out*/,
                                             size_t *alloc_size /*in,out*/, void *chunk, void *udata);

static herr_t H5D__struct_chunk_layout_query(H5D_t *dset, hsize_t *chunk_dims, bool *encode_decode_necessary,
                                             bool *partial_bound_chunks_different_encoding);

static herr_t H5D__struct_chunk_delete_chunk(H5D_t *dset, const hsize_t *scaled /*in*/, haddr_t addr,
                                             hsize_t disk_size);

/*********************/
/* Package Variables */
/*********************/
/* Layout I/O callbacks for structured chunk */
const H5D_layout_ops_t H5D_LOPS_STRUCT_CHUNK[1] = {{
    H5D__struct_chunk_construct,      /* construct */
    H5D__struct_chunk_init,           /* init */
    H5D__struct_chunk_is_space_alloc, /* is_space_alloc */
    NULL,                             /* is_data_cached */
    H5D__struct_chunk_io_init,        /* io_init */
    H5D__struct_chunk_mdio_init,      /* mdio_init */
    NULL,                             /* ser_read */
    NULL,                             /* ser_write */
    NULL,                             /* readvv */
    NULL,                             /* writevv */
    NULL,                             /* flush */
    H5D__struct_chunk_io_term,        /* io_term */
    H5D__struct_chunk_dest            /* dest */
}};

/* Shared Chunk Cache layout callbacks for structured chunked */
const H5SC_layout_ops_t H5SC_LOPS_STRUCT_CHUNK[1] = {{
    H5D__struct_chunk_lookup,                /* lookup */
    H5D__struct_chunk_decode,                /* decode */
    H5D__struct_chunk_decode_defined_values, /* decode_defined_values */
    H5D__struct_chunk_new_chunk,             /* new_chunk */
    H5D__struct_chunk_condense,              /* condense */
    H5D__struct_chunk_encode,                /* encode */
    H5D__struct_chunk_encode_in_place,       /* encode_in_place */
    H5D__struct_chunk_evict,                 /* evict */
    H5D__struct_chunk_insert,                /* insert */
    NULL,                                    /* selection_read */
    H5D__struct_chunk_vector_read,           /* vector_read */
    NULL,                                    /* selection_write */
    H5D__struct_chunk_vector_write,          /* vector_write */
    H5D__struct_chunk_scatter_mem,           /* scatter_mem */
    H5D__struct_chunk_gather_mem,            /* gather_mem */
    H5D__struct_chunk_fill,                  /* fill */
    H5D__struct_chunk_defined_values,        /* defined_values */
    H5D__struct_chunk_erase_values,          /* erase_values */
    H5D__struct_chunk_evict_values,          /* evict_values */
    H5D__struct_chunk_layout_query,          /* layout_query */
    H5D__struct_chunk_delete_chunk           /* delete_chunk */
}};

/*******************/
/* Local Variables */
/*******************/

/* Declare extern free list to manage the H5S_sel_iter_t struct */
H5FL_EXTERN(H5S_sel_iter_t);

/* Declare extern free list to manage sequences of size_t */
H5FL_SEQ_EXTERN(size_t);

/* Declare extern free list to manage sequences of hsize_t */
H5FL_SEQ_EXTERN(hsize_t);

/* Declare extern free list to manage the H5D_piece_info_t struct */
H5FL_EXTERN(H5D_piece_info_t);

/* Declare extern free list to manage the H5D_chunk_info_t struct */
H5FL_DEFINE(H5D_chunk_map_t);

/* Declare a free list to manage blocks of scat_buf data */
H5FL_BLK_DEFINE(scat_buf);

/*
 * Helper routines for layout callbacks
 */

/*-------------------------------------------------------------------------
 * Function:    H5D__struct_chunk_create
 *
 * Purpose:    Creates a new chunked storage index and initializes the
 *        layout information with information about the storage.  The
 *        layout info should be immediately written to the object header.
 *
 * Return:    Non-negative on success (with the layout information initialized
 *        and ready to write to an object header). Negative on failure.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D__struct_chunk_create(const H5D_t *dset /*in,out*/)
{
    H5D_chk_idx_info_t          idx_info; /* Chunked index info */
    H5O_storage_struct_chunk_t *store     = &(dset->shared->layout.storage.u.struct_chunk);
    herr_t                      ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_PACKAGE

    /* Check args */
    assert(dset);
    assert(H5D_STRUCT_CHUNK == dset->shared->layout.type);
    assert(dset->shared->layout.u.struct_chunk.ndims > 0 &&
           dset->shared->layout.u.struct_chunk.ndims <= H5O_LAYOUT_NDIMS);
    H5D_STRUCT_CHUNK_STORAGE_INDEX_CHK(store);

#ifndef NDEBUG
    {
        unsigned u; /* Local index variable */

        for (u = 0; u < dset->shared->layout.u.struct_chunk.ndims; u++)
            assert(dset->shared->layout.u.struct_chunk.dim[u] > 0);
    }
#endif

    /* Compose chunked index info struct */
    idx_info.f           = dset->oloc.file;
    idx_info.pline       = &dset->shared->dcpl_cache.pline;
    idx_info.stc_layout  = &dset->shared->layout.u.struct_chunk;
    idx_info.stc_storage = store;

    /* Create the index for the chunks */
    if ((store->ops->create)(&idx_info) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "can't create chunk index");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__chunk_create() */

/*-------------------------------------------------------------------------
 * Function:    H5D__struct_chunk_may_use_select_io
 *
 * Purpose:    A small internal function to if it may be possible to use
 *             selection I/O.
 *
 * Return:    true or false
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__struct_chunk_may_use_select_io(H5D_io_info_t *io_info, const H5D_dset_io_info_t *dset_info)
{
    const H5D_t *dataset   = NULL;    /* Local pointer to dataset info */
    herr_t       ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_PACKAGE

    /* Sanity check */
    assert(io_info);
    assert(dset_info);

    dataset = dset_info->dset;
    assert(dataset);

    /* Don't use selection I/O if there are filters on the dataset (for now) */
    if (dataset->shared->dcpl_cache.pline.nused > 0) {
        io_info->use_select_io = H5D_SELECTION_IO_MODE_OFF;
        io_info->no_selection_io_cause |= H5D_SEL_IO_DATASET_FILTER;
    }
    else {
        bool page_buf_enabled;

        /* Check if the page buffer is enabled */
        if (H5PB_enabled(io_info->f_sh, H5FD_MEM_DRAW, &page_buf_enabled) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't check if page buffer is enabled");
        if (page_buf_enabled) {
            /* Note that page buffer is disabled in parallel */
            io_info->use_select_io = H5D_SELECTION_IO_MODE_OFF;
            io_info->no_selection_io_cause |= H5D_SEL_IO_PAGE_BUFFER;
        }
    }

    /* Remove coding for checking if chunks in this dataset may be cached */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__struct_chunk_may_use_select_io() */

/*-------------------------------------------------------------------------
 * Function:    H5D__struct_chunk_io_init_selections
 *
 * Purpose:        Initialize the chunk mappings
 *
 * Return:        Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__struct_chunk_io_init_selections(H5D_io_info_t *io_info, H5D_dset_io_info_t *dinfo)
{
    H5D_chunk_map_t   *fm;                 /* Convenience pointer to chunk map */
    const H5D_t       *dataset;            /* Local pointer to dataset info */
    const H5T_t       *mem_type;           /* Local pointer to memory datatype */
    H5S_t             *tmp_mspace = NULL;  /* Temporary memory dataspace */
    bool               iter_init  = false; /* Selection iteration info has been initialized */
    char               bogus;              /* "bogus" buffer to pass to selection iterator */
    H5D_io_info_wrap_t io_info_wrap;
    herr_t             ret_value = SUCCEED; /* Return value        */

    FUNC_ENTER_PACKAGE

    assert(io_info);
    assert(dinfo);

    /* Set convenience pointers */
    fm = dinfo->layout_io_info.chunk_map;
    assert(fm);
    dataset  = dinfo->dset;
    mem_type = dinfo->type_info.mem_type;

    /* Special case for only one element in selection */
    /* (usually appending a record) */
    if (dinfo->nelmts == 1
#ifdef H5_HAVE_PARALLEL
        && !(io_info->using_mpi_vfd)
#endif /* H5_HAVE_PARALLEL */
        && H5S_SEL_ALL != H5S_GET_SELECT_TYPE(dinfo->file_space)) {
        /* Initialize skip list for chunk selections */
        fm->use_single = true;

        /* Remove coding to use/setup the chunk cache's copy of single_piece_info */

        /* Initialize single chunk dataspace */
        if (NULL == dataset->shared->struct_chunk.single_space) {
            /* Make a copy of the dataspace for the dataset */
            if ((dataset->shared->struct_chunk.single_space = H5S_copy(dinfo->file_space, true, false)) ==
                NULL)
                HGOTO_ERROR(H5E_DATASPACE, H5E_CANTCOPY, FAIL, "unable to copy file space");

            /* Resize chunk's dataspace dimensions to size of chunk */
            if (H5S_set_extent_real(dataset->shared->struct_chunk.single_space, fm->chunk_dim) < 0)
                HGOTO_ERROR(H5E_DATASPACE, H5E_CANTSET, FAIL, "can't adjust chunk dimensions");

            /* Set the single chunk dataspace to 'all' selection */
            if (H5S_select_all(dataset->shared->struct_chunk.single_space, true) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_CANTSELECT, FAIL, "unable to set all selection");
        } /* end if */
        fm->single_space = dataset->shared->struct_chunk.single_space;
        assert(fm->single_space);

        /* Allocate the single chunk information */
        if (NULL == dataset->shared->struct_chunk.single_piece_info)
            if (NULL == (dataset->shared->struct_chunk.single_piece_info = H5FL_MALLOC(H5D_piece_info_t)))
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate chunk info");
        fm->single_piece_info = dataset->shared->struct_chunk.single_piece_info;
        assert(fm->single_piece_info);

        /* Reset chunk template information */
        fm->mchunk_tmpl = NULL;

        /* Set up chunk mapping for single element */
        if (H5D__create_piece_map_single(dinfo, io_info) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL,
                        "unable to create chunk selections for single element");
    } /* end if */
    else {
        bool sel_hyper_flag; /* Whether file selection is a hyperslab */

        /* Initialize skip list for chunk selections */
        if (NULL == dataset->shared->struct_chunk.sel_chunks)
            if (NULL == (dataset->shared->struct_chunk.sel_chunks = H5SL_create(H5SL_TYPE_HSIZE, NULL)))
                HGOTO_ERROR(H5E_DATASET, H5E_CANTCREATE, FAIL, "can't create skip list for chunk selections");
        fm->dset_sel_pieces = dataset->shared->struct_chunk.sel_chunks;
        assert(fm->dset_sel_pieces);

        /* We are not using single element mode */
        fm->use_single = false;

        /* Get type of selection on disk & in memory */
        if ((fm->fsel_type = H5S_GET_SELECT_TYPE(dinfo->file_space)) < H5S_SEL_NONE)
            HGOTO_ERROR(H5E_DATASET, H5E_BADSELECT, FAIL, "unable to get type of selection");
        if ((fm->msel_type = H5S_GET_SELECT_TYPE(dinfo->mem_space)) < H5S_SEL_NONE)
            HGOTO_ERROR(H5E_DATASET, H5E_BADSELECT, FAIL, "unable to get type of selection");

        /* If the selection is NONE or POINTS, set the flag to false */
        if (fm->fsel_type == H5S_SEL_POINTS || fm->fsel_type == H5S_SEL_NONE)
            sel_hyper_flag = false;
        else
            sel_hyper_flag = true;

        /* Check if file selection is a not a hyperslab selection */
        if (sel_hyper_flag) {
            /* Build the file selection for each chunk */
            if (H5S_SEL_ALL == fm->fsel_type) {
                if (H5D__create_piece_file_map_all(dinfo, io_info) < 0)
                    HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to create file chunk selections");
            } /* end if */
            else {
                /* Sanity check */
                assert(fm->fsel_type == H5S_SEL_HYPERSLABS);

                if (H5D__create_piece_file_map_hyper(dinfo, io_info) < 0)
                    HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to create file chunk selections");
            } /* end else */
        }     /* end if */
        else {
            H5S_sel_iter_op_t iter_op; /* Operator for iteration */

            /* set opdata for H5D__piece_mem_cb */
            io_info_wrap.io_info = io_info;
            io_info_wrap.dinfo   = dinfo;
            iter_op.op_type      = H5S_SEL_ITER_OP_LIB;
            iter_op.u.lib_op     = H5D__piece_file_cb;

            /* Spaces might not be the same shape, iterate over the file selection directly */
            if (H5S_select_iterate(&bogus, dataset->shared->type, dinfo->file_space, &iter_op,
                                   &io_info_wrap) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to create file chunk selections");

            /* Reset "last piece" info */
            fm->last_index      = (hsize_t)-1;
            fm->last_piece_info = NULL;
        } /* end else */

        /* Build the memory selection for each chunk */
        if (sel_hyper_flag && H5S_SELECT_SHAPE_SAME(dinfo->file_space, dinfo->mem_space) == true) {
            /* Reset chunk template information */
            fm->mchunk_tmpl = NULL;

            /* If the selections are the same shape, use the file chunk
             * information to generate the memory chunk information quickly.
             */
            if (H5D__create_piece_mem_map_hyper(dinfo) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to create memory chunk selections");
        } /* end if */
        else if (sel_hyper_flag && fm->f_ndims == 1 && fm->m_ndims == 1 &&
                 H5S_SELECT_IS_REGULAR(dinfo->mem_space) && H5S_SELECT_IS_SINGLE(dinfo->mem_space)) {
            if (H5D__create_piece_mem_map_1d(dinfo) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to create file chunk selections");
        } /* end else-if */
        else {
            H5S_sel_iter_op_t iter_op;   /* Operator for iteration */
            size_t            elmt_size; /* Memory datatype size */

            /* Make a copy of equivalent memory space */
            if ((tmp_mspace = H5S_copy(dinfo->mem_space, true, false)) == NULL)
                HGOTO_ERROR(H5E_DATASPACE, H5E_CANTCOPY, FAIL, "unable to copy memory space");

            /* De-select the mem space copy */
            if (H5S_select_none(tmp_mspace) < 0)
                HGOTO_ERROR(H5E_DATASPACE, H5E_CANTINIT, FAIL, "unable to de-select memory space");

            /* Save chunk template information */
            fm->mchunk_tmpl = tmp_mspace;

            /* Create selection iterator for memory selection */
            if (0 == (elmt_size = H5T_get_size(mem_type)))
                HGOTO_ERROR(H5E_DATATYPE, H5E_BADSIZE, FAIL, "datatype size invalid");
            if (H5S_select_iter_init(&(fm->mem_iter), dinfo->mem_space, elmt_size, 0) < 0)
                HGOTO_ERROR(H5E_DATASPACE, H5E_CANTINIT, FAIL, "unable to initialize selection iterator");
            iter_init = true; /* Selection iteration info has been initialized */

            /* set opdata for H5D__piece_mem_cb */
            io_info_wrap.io_info = io_info;
            io_info_wrap.dinfo   = dinfo;
            iter_op.op_type      = H5S_SEL_ITER_OP_LIB;
            iter_op.u.lib_op     = H5D__piece_mem_cb;

            /* Spaces aren't the same shape, iterate over the memory selection directly */
            if (H5S_select_iterate(&bogus, dataset->shared->type, dinfo->file_space, &iter_op,
                                   &io_info_wrap) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to create memory chunk selections");
        } /* end else */
    }     /* end else */

done:
    /* Release the [potentially partially built] chunk mapping information if an error occurs */
    if (ret_value < 0) {
        if (tmp_mspace && !fm->mchunk_tmpl)
            if (H5S_close(tmp_mspace) < 0)
                HDONE_ERROR(H5E_DATASPACE, H5E_CANTRELEASE, FAIL,
                            "can't release memory chunk dataspace template");
        if (H5D__struct_chunk_io_term(io_info, dinfo) < 0)
            HDONE_ERROR(H5E_DATASPACE, H5E_CANTRELEASE, FAIL, "unable to release chunk mapping");
    } /* end if */

    if (iter_init && H5S_SELECT_ITER_RELEASE(&(fm->mem_iter)) < 0)
        HDONE_ERROR(H5E_DATASPACE, H5E_CANTRELEASE, FAIL, "unable to release selection iterator");

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__struct_chunk_io_init_selections() */

/*-------------------------------------------------------------------------
 * Function:    H5D__struct_chunk_set_info_real
 *
 * Purpose:     Internal routine to set the information about chunks for a dataset
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__struct_chunk_set_info_real(H5O_layout_struct_chunk_t *layout, unsigned ndims, const hsize_t *curr_dims,
                                const hsize_t *max_dims)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE

    assert(layout);
    assert(curr_dims);

    /* Can happen when corrupt files are parsed */
    if (ndims == 0)
        HGOTO_ERROR(H5E_DATASET, H5E_BADVALUE, FAIL, "number of dimensions cannot be zero");

    /* Compute the # of chunks in dataset dimensions */
    layout->nchunks     = 1;
    layout->max_nchunks = 1;
    for (unsigned u = 0; u < ndims; u++) {
        /* Round up to the next integer # of chunks, to accommodate partial chunks */
        layout->chunks[u] = ((curr_dims[u] + layout->dim[u]) - 1) / layout->dim[u];
        if (H5S_UNLIMITED == max_dims[u])
            layout->max_chunks[u] = H5S_UNLIMITED;
        else {
            /* Sanity check */
            if (layout->dim[u] == 0)
                HGOTO_ERROR(H5E_DATASET, H5E_BADVALUE, FAIL, "dimension size must be > 0, dim = %u ", u);

            layout->max_chunks[u] = ((max_dims[u] + layout->dim[u]) - 1) / layout->dim[u];
        }

        /* Accumulate the # of chunks */
        layout->nchunks *= layout->chunks[u];
        layout->max_nchunks *= layout->max_chunks[u];
    }

    /* Get the "down" sizes for each dimension */
    H5VM_array_down(ndims, layout->chunks, layout->down_chunks);
    H5VM_array_down(ndims, layout->max_chunks, layout->max_down_chunks);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__struct_chunk_set_info_real() */

/*-------------------------------------------------------------------------
 * Function:    H5D__struct_chunk_set_info
 *
 * Purpose:    Sets the information about chunks for a dataset
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__struct_chunk_set_info(const H5D_t *dset)
{
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_PACKAGE

    /* Sanity checks */
    assert(dset);

    /* Set the base layout information */
    if (H5D__struct_chunk_set_info_real(&dset->shared->layout.u.struct_chunk, dset->shared->ndims,
                                        dset->shared->curr_dims, dset->shared->max_dims) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTSET, FAIL, "can't set layout's chunk info");

    /* Call the index's "resize" callback */
    if (dset->shared->layout.storage.u.struct_chunk.ops->resize &&
        (dset->shared->layout.storage.u.struct_chunk.ops->resize)(&dset->shared->layout.u.struct_chunk) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTSET, FAIL, "unable to resize chunk index information");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__struct_chunk_set_info() */

/*-------------------------------------------------------------------------
 * Function:    H5D__struct_chunk_set_sizes
 *
 * Purpose:     Sets chunk and type sizes.
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D__struct_chunk_set_sizes(H5D_t *dset)
{
    uint64_t chunk_size;            /* Size of chunk in bytes */
    unsigned max_enc_bytes_per_dim; /* Max. number of bytes required to encode this dimension */
    unsigned u;                     /* Iterator */
    htri_t   has_vlen_type;
    herr_t   ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_PACKAGE

    /* Sanity checks */
    assert(dset);

    /* Increment # of chunk dimensions, to account for datatype size as last element */
    dset->shared->layout.u.struct_chunk.ndims++;

    /* Set the last dimension of the chunk size to the size of the datatype */
    dset->shared->layout.u.struct_chunk.dim[dset->shared->layout.u.struct_chunk.ndims - 1] =
        (uint32_t)H5T_GET_SIZE(dset->shared->type);

    /* Compute number of bytes to use for encoding chunk dimensions */
    max_enc_bytes_per_dim = 0;
    for (u = 0; u < (unsigned)dset->shared->layout.u.struct_chunk.ndims; u++) {
        unsigned enc_bytes_per_dim; /* Number of bytes required to encode this dimension */

        /* Get encoded size of dim, in bytes */
        enc_bytes_per_dim = (H5VM_log2_gen(dset->shared->layout.u.struct_chunk.dim[u]) + 8) / 8;

        /* Check if this is the largest value so far */
        if (enc_bytes_per_dim > max_enc_bytes_per_dim)
            max_enc_bytes_per_dim = enc_bytes_per_dim;
    } /* end for */
    assert(max_enc_bytes_per_dim > 0 && max_enc_bytes_per_dim <= 8);
    dset->shared->layout.u.struct_chunk.enc_bytes_per_dim = max_enc_bytes_per_dim;

    /* Compute and store the total size of a chunk */
    /* (Use 64-bit value to ensure that we can detect >4GB chunks) */
    for (u = 1, chunk_size = (uint64_t)dset->shared->layout.u.struct_chunk.dim[0];
         u < dset->shared->layout.u.struct_chunk.ndims; u++)
        chunk_size *= (uint64_t)dset->shared->layout.u.struct_chunk.dim[u];

    dset->shared->layout.u.struct_chunk.size = chunk_size;

    /* Remove the following check: */
    /* Check for chunk larger than can be represented in 32-bits */
    /* (Chunk size is encoded in 32-bit value in v1 B-tree records) */
    /* if (chunk_size > (uint64_t)0xffffffff)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "chunk size must be < 4GB");
    H5_CHECKED_ASSIGN(dset->shared->layout.u.chunk.size, uint32_t, chunk_size, uint64_t); */

    /* Detect whether the datatype has a VL component */
    if ((has_vlen_type = H5T_detect_class(dset->shared->type, H5T_VLEN, false)) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_BADVALUE, FAIL, "unable to detect vlen datatypes?");

    /* Set up info for structured chunk composition */
    if (has_vlen_type) {
        /* TBD: not handled yet for structured chunk */
        assert("not implemented yet" && 0);
    }
    else { /* Fixed-size data */
        dset->shared->layout.storage.u.struct_chunk.nsects          = 2;
        dset->shared->layout.storage.u.struct_chunk.nsects_md       = 1;
        dset->shared->layout.storage.u.struct_chunk.seq_sects_md[0] = 0;
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__struct_chunk_set_sizes */

/*-------------------------------------------------------------------------
 * Function:    H5D_struct_chunk_idx_reset
 *
 * Purpose:    Reset index information
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D_struct_chunk_idx_reset(H5O_storage_struct_chunk_t *storage, bool reset_addr)
{
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    assert(storage);
    assert(storage->ops);
    H5D_STRUCT_CHUNK_STORAGE_INDEX_CHK(storage);

    /* Reset index structures */
    if ((storage->ops->reset)(storage, reset_addr) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTFREE, FAIL, "unable to reset chunk index info");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D_chunk_idx_reset() */

/*
 * Layout I/O callbacks for structured chunk
 */

/*-------------------------------------------------------------------------
 * Function:    H5D__struct_chunk_construct
 *
 * Purpose:    Constructs new chunked layout information for dataset
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__struct_chunk_construct(H5F_t H5_ATTR_UNUSED *f, H5D_t *dset)
{
    unsigned u;                   /* Local index variable */
    herr_t   ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_PACKAGE

    /* Sanity checks */
    assert(f);
    assert(dset);

    /* Check for invalid chunk dimension rank */
    if (0 == dset->shared->layout.u.struct_chunk.ndims)
        HGOTO_ERROR(H5E_DATASET, H5E_BADVALUE, FAIL, "no chunk information set?");
    if (dset->shared->layout.u.struct_chunk.ndims != dset->shared->ndims)
        HGOTO_ERROR(H5E_DATASET, H5E_BADVALUE, FAIL, "dimensionality of chunks doesn't match the dataspace");

    /* Set chunk sizes */
    H5D__struct_chunk_set_sizes(dset);
    assert((unsigned)(dset->shared->layout.u.struct_chunk.ndims) <=
           NELMTS(dset->shared->layout.u.struct_chunk.dim));

    /* Chunked storage is not compatible with external storage (currently) */
    if (dset->shared->dcpl_cache.efl.nused > 0)
        HGOTO_ERROR(H5E_DATASET, H5E_BADVALUE, FAIL, "external storage not supported with chunked layout");

    /* Sanity check dimensions */
    for (u = 0; u < dset->shared->layout.u.struct_chunk.ndims - 1; u++) {
        /* Don't allow zero-sized chunk dimensions */
        if (0 == dset->shared->layout.u.struct_chunk.dim[u])
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "chunk size must be > 0, dim = %u ", u);

        /*
         * The chunk size of a dimension with a fixed size cannot exceed
         * the maximum dimension size. If any dimension size is zero, there
         * will be no such restriction.
         */
        if (dset->shared->curr_dims[u] && dset->shared->max_dims[u] != H5S_UNLIMITED &&
            dset->shared->max_dims[u] < dset->shared->layout.u.struct_chunk.dim[u])
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL,
                        "chunk size must be <= maximum dimension size for fixed-sized dimensions");
    } /* end for */

    /* Reset address and pointer of the array struct for the chunked storage index */
    if (H5D_struct_chunk_idx_reset(&dset->shared->layout.storage.u.struct_chunk, true) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to reset chunked storage index");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__struct_chunk_construct() */

/*-------------------------------------------------------------------------
 *
 * Purpose:    Called when the dataset is initialized.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__struct_chunk_init(H5F_t *f, const H5D_t *const dset, hid_t H5_ATTR_UNUSED dapl_id)
{
    H5D_chk_idx_info_t          idx_info; /* Chunked index info */
    H5O_storage_struct_chunk_t *storage   = &(dset->shared->layout.storage.u.struct_chunk);
    bool                        idx_init  = false;
    herr_t                      ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_PACKAGE

    /* Sanity check */
    assert(f);
    assert(dset);
    H5D_STRUCT_CHUNK_STORAGE_INDEX_CHK(storage);

    /* Coding for raw data chunk cache for a dataset is removed */

    /* Compose chunked index info struct */
    idx_info.f           = f;
    idx_info.pline       = &dset->shared->dcpl_cache.pline;
    idx_info.stc_layout  = &dset->shared->layout.u.struct_chunk;
    idx_info.stc_storage = storage;

    /* Allocate any indexing structures */
    if (storage->ops->init && (storage->ops->init)(&idx_info, dset->shared->space, dset->oloc.addr) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "can't initialize indexing information");
    idx_init = true;

    /* Set the number of chunks in dataset, etc. */
    if (H5D__struct_chunk_set_info(dset) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to set # of chunks for dataset");

done:
    if (FAIL == ret_value) {

        if (idx_init && storage->ops->dest && (storage->ops->dest)(&idx_info) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CANTFREE, FAIL, "unable to release chunk index info");
    }
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__struct_chunk_init() */

/*-------------------------------------------------------------------------
 * Function:    H5D__struct_chunk_is_space_alloc
 *
 * Purpose:    Query if space is allocated for layout
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
bool
H5D__struct_chunk_is_space_alloc(const H5O_storage_t *store)
{
    const H5O_storage_struct_chunk_t *storage   = &(store->u.struct_chunk);
    bool                              ret_value = false; /* Return value */

    FUNC_ENTER_PACKAGE_NOERR

    /* Sanity checks */
    assert(store);
    H5D_STRUCT_CHUNK_STORAGE_INDEX_CHK(storage);

    /* Query index layer */
    ret_value = (storage->ops->is_space_alloc)(storage);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__struct_chunk_is_space_alloc() */

/*-------------------------------------------------------------------------
 * Function:    H5D__struct_chunk_io_init
 *
 * Purpose:    Performs initialization before any sort of I/O on the raw data
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__struct_chunk_io_init(H5D_io_info_t *io_info, H5D_dset_io_info_t *dinfo)
{
    const H5D_t     *dataset = dinfo->dset;         /* Local pointer to dataset info */
    H5D_chunk_map_t *fm;                            /* Convenience pointer to chunk map */
    hssize_t         old_offset[H5O_LAYOUT_NDIMS];  /* Old selection offset */
    htri_t           file_space_normalized = false; /* File dataspace was normalized */
    unsigned         f_ndims;                       /* The number of dimensions of the file's dataspace */
    int              sm_ndims; /* The number of dimensions of the memory buffer's dataspace (signed) */
    unsigned         u;        /* Local index variable */
    herr_t           ret_value = SUCCEED; /* Return value        */

    FUNC_ENTER_PACKAGE

    /* Allocate chunk map */
    if (NULL == (dinfo->layout_io_info.chunk_map = H5FL_MALLOC(H5D_chunk_map_t)))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "unable to allocate chunk map");
    fm = dinfo->layout_io_info.chunk_map;

    /* Get layout for dataset */
    dinfo->layout = &(dataset->shared->layout);

    /* Initialize "last chunk" information */
    fm->last_index      = (hsize_t)-1;
    fm->last_piece_info = NULL;

    /* Clear other fields */
    fm->mchunk_tmpl       = NULL;
    fm->dset_sel_pieces   = NULL;
    fm->single_space      = NULL;
    fm->single_piece_info = NULL;

    /* Initialize selection type in memory and file */
    fm->msel_type = H5S_SEL_ERROR;
    fm->fsel_type = H5S_SEL_ERROR;

    /* Check if the memory space is scalar & make equivalent memory space */
    if ((sm_ndims = H5S_GET_EXTENT_NDIMS(dinfo->mem_space)) < 0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTGET, FAIL, "unable to get dimension number");
    /* Set the number of dimensions for the memory dataspace */
    H5_CHECKED_ASSIGN(fm->m_ndims, unsigned, sm_ndims, int);

    /* Get rank for file dataspace */
    fm->f_ndims = f_ndims = dataset->shared->layout.u.struct_chunk.ndims - 1;

    /* Normalize hyperslab selections by adjusting them by the offset */
    /* (It might be worthwhile to normalize both the file and memory dataspaces
     * before any (contiguous, chunked, etc) file I/O operation, in order to
     * speed up hyperslab calculations by removing the extra checks and/or
     * additions involving the offset and the hyperslab selection -QAK)
     */
    if ((file_space_normalized = H5S_hyper_normalize_offset(dinfo->file_space, old_offset)) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTSET, FAIL, "unable to normalize selection");

    /* Decide the number of chunks in each dimension */
    for (u = 0; u < f_ndims; u++)
        /* Keep the size of the chunk dimensions as hsize_t for various routines */
        fm->chunk_dim[u] = dinfo->layout->u.struct_chunk.dim[u];

    if (H5D__struct_chunk_io_init_selections(io_info, dinfo) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to create file and memory chunk selections");

    /* Check if we're performing selection I/O and save the result if it hasn't
     * been disabled already */
    if (io_info->use_select_io != H5D_SELECTION_IO_MODE_OFF)
        if (H5D__struct_chunk_may_use_select_io(io_info, dinfo) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't check if selection I/O is possible");

    /* Calculate type conversion buffer size if necessary.  Currently only implemented for selection I/O. */
    if (io_info->use_select_io != H5D_SELECTION_IO_MODE_OFF &&
        !(dinfo->type_info.is_xform_noop && dinfo->type_info.is_conv_noop)) {
        H5SL_node_t *chunk_node; /* Current node in chunk skip list */

        /* Iterate through nodes in chunk skip list */
        chunk_node = H5D_CHUNK_GET_FIRST_NODE(dinfo);
        while (chunk_node) {
            H5D_piece_info_t *piece_info; /* Chunk information */

            /* Get the actual chunk information from the skip list node */
            piece_info = H5D_CHUNK_GET_NODE_INFO(dinfo, chunk_node);

            /* Handle type conversion buffer */
            H5D_INIT_PIECE_TCONV(io_info, dinfo, piece_info)

            /* Advance to next chunk in list */
            chunk_node = H5D_CHUNK_GET_NEXT_NODE(dinfo, chunk_node);
        }
    }

#ifdef H5_HAVE_PARALLEL
    /*
     * If collective metadata reads are enabled, ensure all ranks
     * have the dataset's chunk index open (if it was created) to
     * prevent possible metadata inconsistency issues or unintentional
     * independent metadata reads later on.
     */
    if (H5F_SHARED_HAS_FEATURE(io_info->f_sh, H5FD_FEAT_HAS_MPI) &&
        H5F_shared_get_coll_metadata_reads(io_info->f_sh) &&
        H5D__chunk_is_space_alloc(&dataset->shared->layout.storage)) {
        H5O_storage_chunk_t *sc = &(dataset->shared->layout.storage.u.chunk);
        H5D_chk_idx_info_t   idx_info;
        bool                 index_is_open;

        idx_info.f       = dataset->oloc.file;
        idx_info.pline   = &dataset->shared->dcpl_cache.pline;
        idx_info.layout  = &dataset->shared->layout.u.chunk;
        idx_info.storage = sc;

        assert(sc && sc->ops && sc->ops->is_open);
        if (sc->ops->is_open(&idx_info, &index_is_open) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "unable to check if dataset chunk index is open");

        if (!index_is_open) {
            assert(sc->ops->open);
            if (sc->ops->open(&idx_info) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to open dataset chunk index");
        }

        /*
         * Load any other chunk index metadata that we can,
         * such as fixed array data blocks, while we know all
         * MPI ranks will do so with collective metadata reads
         * enabled
         */
        if (sc->ops->load_metadata && sc->ops->load_metadata(&idx_info) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to load additional chunk index metadata");
    }
#endif

done:
    if (file_space_normalized == true)
        if (H5S_hyper_denormalize_offset(dinfo->file_space, old_offset) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CANTSET, FAIL, "can't denormalize selection");

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__struct_chunk_io_init() */

/*-------------------------------------------------------------------------
 * Function:   H5D__struct_chunk_mdio_init
 *
 * Purpose:    Performs second phase of initialization for multi-dataset
 *             I/O.  Currently looks up chunk addresses and adds chunks to
 *             sel_pieces.
 *
 * Return:     Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__struct_chunk_mdio_init(H5D_io_info_t *io_info, H5D_dset_io_info_t *dinfo)
{
    H5SL_node_t      *piece_node; /* Current node in chunk skip list */
    H5D_piece_info_t *piece_info; /* Piece information for current piece */
    H5D_chunk_ud_t  **udata[1];   /* Chunk data from index */
    haddr_t          *addr[1];
    const hsize_t    *scaled[1];
    herr_t            ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_PACKAGE

    /* Get first node in skip list.  Note we don't check for failure since NULL
     * simply indicates an empty skip list. */
    piece_node = H5D_CHUNK_GET_FIRST_NODE(dinfo);

    /* Iterate over skip list */
    while (piece_node) {
        /* Get piece info */
        if (NULL == (piece_info = (H5D_piece_info_t *)H5D_CHUNK_GET_NODE_INFO(dinfo, piece_node)))
            HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "couldn't get piece info from list");

        /* Get the info for the chunk in the file */
        scaled[0] = piece_info->scaled;
        addr[0]   = &piece_info->faddr;

        if (H5D__struct_chunk_lookup(dinfo->dset, 1, scaled, addr, NULL, NULL, NULL, NULL, (void ***)udata) <
            0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "error looking up chunk address");

        /* Add piece to MDIO operation if it has a file address */
        if (H5_addr_defined(piece_info->faddr)) {
            assert(io_info->sel_pieces);
            assert(io_info->pieces_added < io_info->piece_count);

            /* Add to sel_pieces and update pieces_added */
            io_info->sel_pieces[io_info->pieces_added++] = piece_info;

            if (piece_info->filtered_dset)
                io_info->filtered_pieces_added++;
        }

        /* Advance to next skip list node */
        piece_node = H5D_CHUNK_GET_NEXT_NODE(dinfo, piece_node);
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__struct_chunk_mdio_init() */

/*-------------------------------------------------------------------------
 * Function:    H5D__struct_chunk_io_term
 *
 * Purpose:    Destroy I/O operation information.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 * NOTE: No change from the legacy chunk version
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__struct_chunk_io_term(H5D_io_info_t H5_ATTR_UNUSED *io_info, H5D_dset_io_info_t *di)
{
    H5D_chunk_map_t *fm;                  /* Convenience pointer to chunk map */
    herr_t           ret_value = SUCCEED; /*return value        */

    FUNC_ENTER_PACKAGE

    assert(di);

    /* Set convenience pointer */
    fm = di->layout_io_info.chunk_map;

    /* Single element I/O vs. multiple element I/O cleanup */
    if (fm->use_single) {
        /* Sanity checks */
        assert(fm->dset_sel_pieces == NULL);
        assert(fm->last_piece_info == NULL);
        assert(fm->single_piece_info);
        assert(fm->single_piece_info->fspace_shared);
        assert(fm->single_piece_info->mspace_shared);

        /* Reset the selection for the single element I/O */
        H5S_select_all(fm->single_space, true);
    } /* end if */
    else {
        /* Release the nodes on the list of selected pieces, or the last (only)
         * piece if the skiplist is not available */
        if (fm->dset_sel_pieces) {
            if (H5SL_free(fm->dset_sel_pieces, H5D__free_piece_info, NULL) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_CANTNEXT, FAIL, "can't free dataset skip list");
        } /* end if */
        else if (fm->last_piece_info) {
            if (H5D__free_piece_info(fm->last_piece_info, NULL, NULL) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_CANTFREE, FAIL, "can't free piece info");
            fm->last_piece_info = NULL;
        } /* end if */
    }     /* end else */

    /* Free the memory piece dataspace template */
    if (fm->mchunk_tmpl)
        if (H5S_close(fm->mchunk_tmpl) < 0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTRELEASE, FAIL,
                        "can't release memory chunk dataspace template");

    /* Free chunk map */
    di->layout_io_info.chunk_map = H5FL_FREE(H5D_chunk_map_t, di->layout_io_info.chunk_map);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__struct_chunk_io_term() */

/*-------------------------------------------------------------------------
 * Function:    H5D__struct_chunk_dest
 *
 * Purpose:     Free index structure
 *
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__struct_chunk_dest(H5D_t *dset)
{
    H5D_chk_idx_info_t          idx_info; /* Chunked index info */
    H5O_storage_struct_chunk_t *storage   = &(dset->shared->layout.storage.u.struct_chunk);
    herr_t                      ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_PACKAGE_TAG(dset->oloc.addr)

    /* Sanity checks */
    assert(dset);
    H5D_STRUCT_CHUNK_STORAGE_INDEX_CHK(storage);

    /* Compose chunked index info struct */
    idx_info.f           = dset->oloc.file;
    idx_info.pline       = &dset->shared->dcpl_cache.pline;
    idx_info.stc_layout  = &dset->shared->layout.u.struct_chunk;
    idx_info.stc_storage = storage;

    /* Free any index structures */
    if (storage->ops->dest && (storage->ops->dest)(&idx_info) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTFREE, FAIL, "unable to release chunk index info");

done:
    FUNC_LEAVE_NOAPI_TAG(ret_value)
} /* end H5D__struct_chunk_dest() */

/*
 * Shared chunk cache layout callbacks for structured chunks
 */

/*-------------------------------------------------------------------------
 * Function:    H5D__struct_chunk_lookup
 *
 * Purpose:     Looks up chunk address and size on disk.
 *
 *              defined_values_size is the number of bytes to read if only
 *              the list of defined values is needed.
 *
 *              size_hint is the suggested allocation size for the chunk
 *              (could be larger if the chunk might expand when decoded).
 *
 *              defined_values_size_hint is the suggested allocation size if only
 *              the list of defined values is needed.
 *              If *defined_values_size is returned as 0, then all values
 *              are defined for the chunk.
 *              In this case, the chunk may still be decoded without reading
 *              from disk, by allocating a buffer of size defined_valued_size_hint and
 *              passing it to H5SC_chunk_decode_t with *nbytes_used set to 0.
 *
 *              *udata can be set to anything and will be passed through to
 *              H5SC_chunk_decode_t and/or the selection or vector I/O routines,
 *              (we will create an H5SC_free_udata_t callback if necessary).
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__struct_chunk_lookup(H5D_t *dset, size_t count, const hsize_t *scaled[] /*in*/, haddr_t *addr[] /*out*/,

                         hsize_t *size[] /*out*/, hsize_t *defined_values_size[] /*out*/,

                         size_t *size_hint[] /*out*/, size_t *defined_values_size_hint[] /*out*/,

                         void **_udata[] /*out*/)
{
    H5D_chunk_ud_t             *udata;
    H5O_storage_struct_chunk_t *storage = &(dset->shared->layout.storage.u.struct_chunk);
    H5O_layout_struct_chunk_t  *layout  = &dset->shared->layout.u.struct_chunk;
    H5D_chk_idx_info_t          idx_info; /* Chunked index info */
    H5O_pline_t                *pline;    /* I/O pipeline info */
    hbool_t                     filtered        = false;
    size_t                      tot_unfilt_size = 0;
    size_t                      i;
    herr_t                      ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_PACKAGE

    /* Sanity checks */
    assert(dset);
    assert(dset->shared->layout.type == H5D_STRUCT_CHUNK);

    pline = &(dset->shared->dcpl_cache.pline);
    if (pline && pline->tot_filt_nsects)
        filtered = true;

    /* Compose chunked index info struct */
    idx_info.f           = dset->oloc.file;
    idx_info.pline       = pline;
    idx_info.stc_layout  = layout;
    idx_info.stc_storage = storage;

    for (i = 0; i < count; i++) {

        /* Allocate udata */
        udata = (H5D_chunk_ud_t *)H5MM_malloc(sizeof(H5D_chunk_ud_t));
        if (udata == NULL)
            HGOTO_ERROR(H5E_ARGS, H5E_CANTALLOC, FAIL, "could not malloc space for udata");

        /* Set up udata */
        udata->common.stc_layout  = layout;
        udata->common.stc_storage = storage;
        udata->common.scaled      = scaled[i];

        /* Reset information about the chunk we are looking for */
        udata->chunk_block.offset = HADDR_UNDEF;
        udata->chunk_block.length = 0;

        /* chunk_idx is calculated in get_addr callback */
        if ((storage->ops->get_addr)(&idx_info, udata) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't query chunk address");

        if (H5_addr_defined(udata->chunk_block.offset)) {

            if (addr[i])
                *addr[i] = udata->chunk_block.offset;
            if (size[i])
                *size[i] = udata->chunk_block.length;

            if (filtered)
                /* For now: assume two sections for fixed data */
                tot_unfilt_size = udata->unfilt_size[0] + udata->unfilt_size[1];

            if (size_hint[i])
                *size_hint[i] = filtered ? tot_unfilt_size : *size[i];

            /* Size of defined values */
            if (defined_values_size[i])
                *defined_values_size[i] = filtered ? udata->unfilt_size[0] : udata->offset[1];

            if (defined_values_size_hint[i])
                *defined_values_size_hint[i] = filtered ? udata->unfilt_size[0] : *defined_values_size[i];
        }

        _udata[i] = (void *)udata;

    } /* end count */

done:
    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5D__struct_chunk_lookup() */

/*-------------------------------------------------------------------------
 * Function:    H5D__struct_chunk_decode
 *
 * Purpose:     Decompresses/decodes the chunk from file format to memory cache format if necessary.
 *              Reallocs chunk buffer if necessary.
 *
 *              On entry, nbytes is the number of bytes used in the chunk buffer.
 *              On exit, it shall be set to the total number of bytes used (not allocated)
 *              across all buffers for this chunk.
 *
 *              On entry, alloc_size is the size of the chunk buffer.
 *              On exit, it shall be set to the total number of bytes allocated across all
 *              buffers for this chunk.
 *
 *              Optional, if not present, chunk is the same in cache as on disk.
 *
 *              partial_bound is true if the chunk was encoded with partial_bound set to true.
 *              If the dataset reported partial_bound_chunks_different_encoding as false,
 *              the setting of partial_bound is undefined.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 * NOTE: On entry: [chunk] is the pointer to the on disk file format chunk buffer
 *       On exit: [chunk] is the pointer to the chunk intermediate struct
 *
 * NOTE: Only handle two sections for now
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__struct_chunk_decode(H5D_t *dset, size_t *nbytes /*in,out*/, size_t *alloc_size /*in,out*/,
                         bool partial_bound, void **chunk /*in,out*/, void *_udata)
{
    H5D_chunk_ud_t        *udata = (H5D_chunk_ud_t *)_udata;
    H5D_chunk_cache_mem_t *chk;   /* Chunk's intermediate struct */
    H5O_pline_t           *pline; /* I/O pipeline info */
    hbool_t                filtered = false;
    uint32_t               stored_chksum;   /* Stored metadata checksum value */
    uint32_t               computed_chksum; /* Computed metadata checksum value */
    void                  *tmp;
    const unsigned char   *sel_p;
    herr_t                 ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_PACKAGE

    /* Sanity checks */
    assert(dset);

    pline = &(dset->shared->dcpl_cache.pline);
    if (pline && pline->tot_filt_nsects)
        filtered = true;

    /* Allocate the chunk intermediate struct */
    if (NULL == (chk = H5MM_malloc(sizeof(H5D_chunk_cache_mem_t))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL,
                    "memory allocation failed for intermediate chunk struct");

    /* nbytes and alloc_size for encoded selection */
    chk->sel_nbytes = chk->sel_alloc_size = udata->offset[1];

    /* nbytes and alloc_size for data values */
    chk->data_nbytes     = *nbytes - chk->sel_nbytes;
    chk->data_alloc_size = *alloc_size - chk->sel_alloc_size;

    /* Allocate a buffer for the encoded selection */
    if (NULL == (chk->sel_buf = H5MM_malloc(chk->sel_alloc_size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for encoded selection buffer");

    /* Copy over the encoded selection */
    H5MM_memcpy(chk->sel_buf, *chunk, chk->sel_nbytes);

    /* Allocate a buffer for the data values */
    if (NULL == (chk->data_buf = H5MM_malloc(chk->data_alloc_size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for data buffer");

    /* Copy over the data values */
    H5MM_memcpy(chk->data_buf, (uint8_t *)(*chunk) + chk->sel_nbytes, chk->data_nbytes);

    /* Decompress the encoded selection  & data values */
    if (filtered && !partial_bound) {
        H5Z_EDC_t              err_detect; /* Error detection info */
        H5Z_cb_t               filter_cb;  /* I/O filter callback function */
        unsigned               i;
        H5Z_stc_filter_sect_t *filt_sect;

        /* Retrieve filter settings from API context */
        if (H5CX_get_err_detect(&err_detect) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't get error detection info");
        if (H5CX_get_filter_cb(&filter_cb) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't get I/O filter callback function");

        for (i = 0, filt_sect = &pline->filt_sects[0]; i < pline->tot_filt_nsects; i++, filt_sect++) {

            if (filt_sect->nused) {
                switch (filt_sect->seq_sect) {
                    case H5_SECTION_SELECTION:
                        if (H5Z_apply_filters(filt_sect->nused, filt_sect->filter, H5Z_FLAG_REVERSE,
                                              &udata->filt_mask[0], err_detect, filter_cb, &chk->sel_nbytes,
                                              &chk->sel_alloc_size, &chk->sel_buf) < 0)
                            HGOTO_ERROR(H5E_DATASET, H5E_CANTFILTER, FAIL, "output pipeline failed");
                        break;

                    case H5_SECTION_FIXED:
                        if (H5Z_apply_filters(filt_sect->nused, filt_sect->filter, H5Z_FLAG_REVERSE,
                                              &udata->filt_mask[1], err_detect, filter_cb, &chk->data_nbytes,
                                              &chk->data_alloc_size, &chk->data_buf) < 0)
                            HGOTO_ERROR(H5E_DATASET, H5E_CANTFILTER, FAIL, "output pipeline failed");
                        break;

                    case H5_SECTION_VL:
                    case H5_SECTION_NUM:
                    default:
                        assert(0 && "Unknown action?!?");
                }
            } /* end if nused */

        } /* end for */
    }
    /* Get stored and computed checksums */
    if (H5F_get_checksums(chk->sel_buf, chk->sel_nbytes, &stored_chksum, &computed_chksum) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't get checksums");
    if (stored_chksum != computed_chksum)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "checksums verification failed");

    chk->sel_nbytes -= H5_SIZEOF_CHKSUM;
    chk->sel_alloc_size -= H5_SIZEOF_CHKSUM;

    sel_p = chk->sel_buf;

    sel_p = chk->sel_buf;

    /* Decode the encoded selection to dataspace sel_space */
    if (NULL == (chk->sel_space = H5S_decode(&sel_p)))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTDECODE, FAIL, "unable to decode dataspace");

    /* Return values on exit */
    *nbytes     = (chk->sel_nbytes + chk->data_nbytes);
    *alloc_size = *nbytes;

    tmp    = *chunk;
    *chunk = chk;
    tmp    = H5MM_xfree(tmp);

done:
    FUNC_LEAVE_NOAPI(ret_value)

} /* H5D__struct_chunk_decode() */

/*-------------------------------------------------------------------------
 * Function:    H5D__struct_chunk_decode_defined_values
 *
 * Purpose:     The same as H5SC_chunk_decode_t but only decodes the defined values.
 *              Optional, if not present, all values are defined.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * NOTE: On entry: [chunk] is the pointer to the on disk file format chunk buffer
 *       On exit: [chunk] is the pointer to the chunk intermediate struct
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__struct_chunk_decode_defined_values(H5D_t *dset, size_t *nbytes /*in,out*/, size_t *alloc_size /*in,out*/,
                                        bool partial_bound, void **chunk /*in,out*/, void *_udata)
{
    H5D_chunk_ud_t        *udata = (H5D_chunk_ud_t *)_udata;
    H5D_chunk_cache_mem_t *chk;   /* Chunk's intermediate struct */
    H5O_pline_t           *pline; /* I/O pipeline info */
    hbool_t                filtered = false;
    uint32_t               stored_chksum;   /* Stored metadata checksum value */
    uint32_t               computed_chksum; /* Computed metadata checksum value */
    void                  *tmp;
    const unsigned char   *sel_p;
    herr_t                 ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_PACKAGE

    /* Sanity checks */
    assert(dset);

    pline = &(dset->shared->dcpl_cache.pline);
    if (pline && pline->tot_filt_nsects)
        filtered = true;

    /* Allocate the chunk intermediate struct */
    if (NULL == (chk = H5MM_malloc(sizeof(H5D_chunk_cache_mem_t))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL,
                    "memory allocation failed for intermediate chunk struct");

    /* nbytes and alloc_size for encoded selection */
    chk->sel_nbytes = chk->sel_alloc_size = udata->offset[1];

    /* Allocate a buffer for the encoded selection */
    if (NULL == (chk->sel_buf = H5MM_malloc(chk->sel_alloc_size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for encoded selection buffer");

    /* Copy over the encoded selection */
    H5MM_memcpy(chk->sel_buf, *chunk, chk->sel_nbytes);

    /* Decompress the encoded selection */
    if (filtered && !partial_bound) {
        H5Z_EDC_t              err_detect; /* Error detection info */
        H5Z_cb_t               filter_cb;  /* I/O filter callback function */
        unsigned               i;
        H5Z_stc_filter_sect_t *filt_sect;

        /* Retrieve filter settings from API context */
        if (H5CX_get_err_detect(&err_detect) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't get error detection info");
        if (H5CX_get_filter_cb(&filter_cb) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't get I/O filter callback function");

        for (i = 0, filt_sect = &pline->filt_sects[0]; i < pline->tot_filt_nsects; i++, filt_sect++) {

            if (filt_sect->nused) {
                switch (filt_sect->seq_sect) {
                    case H5_SECTION_SELECTION:
                        if (H5Z_apply_filters(filt_sect->nused, filt_sect->filter, H5Z_FLAG_REVERSE,
                                              &udata->filt_mask[0], err_detect, filter_cb, &chk->sel_nbytes,
                                              &chk->sel_alloc_size, &chk->sel_buf) < 0)
                            HGOTO_ERROR(H5E_DATASET, H5E_CANTFILTER, FAIL, "output pipeline failed");
                        break;

                    case H5_SECTION_FIXED:
                        break;

                    case H5_SECTION_VL:
                    case H5_SECTION_NUM:
                    default:
                        assert(0 && "Unknown action?!?");
                }
            } /* end if nused */

        } /* end for */
    }

    /* Get stored and computed checksums */
    if (H5F_get_checksums(*chunk, chk->sel_nbytes, &stored_chksum, &computed_chksum) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't get checksums");
    if (stored_chksum != computed_chksum)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "checksums verification failed");

    chk->sel_nbytes -= H5_SIZEOF_CHKSUM;
    chk->sel_alloc_size -= H5_SIZEOF_CHKSUM;

    sel_p = chk->sel_buf;

    /* Decode the encoded selection to dataspace sel_space */
    if (NULL == (chk->sel_space = H5S_decode(&sel_p)))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTDECODE, FAIL, "unable to decode dataspace");

    /* Return values on exit */
    *nbytes     = chk->sel_nbytes;
    *alloc_size = *nbytes;

    tmp    = *chunk;
    *chunk = chk;
    tmp    = H5MM_xfree(tmp);

done:
    FUNC_LEAVE_NOAPI(ret_value)

} /* H5D__struct_chunk_decode_defined_values() */

/*-------------------------------------------------------------------------
 * Function:    H5D__struct_chunk_new_chunk
 *
 * Purpose:    Creates a new empty chunk.
 *             Does not insert into on disk chunk index.
 *
 *             If fill is true, writes the fill value to the chunk
 *             (unless this is a sparse chunk).
 *
 *             The number of bytes used is returned in *nbytes
 *             and the size of the chunk buffer is returned in *buf_size
 *
 * Return:    Non-negative on success/Negative on failure
 *
 * NOTE: On exit: [chunk] is the pointer to the chunk intermedidate struct
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__struct_chunk_new_chunk(H5D_t *dset, bool fill, size_t *nbytes /*out*/, size_t *buf_size /*out*/,
                            void **chunk /*out*/, void **udata /*out*/)
{
    H5D_chunk_cache_mem_t *chk; /* Chunk's intermediate struct */
    H5D_chunk_ud_t        *uptr;

    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_PACKAGE

    /* Sanity checks */
    assert(dset);
    assert(dset->shared->layout.u.struct_chunk.stc_type == H5D_SPARSE_CHUNK);
    assert(!fill);

    /* Allocate the chunk's intermediate struct */
    if (NULL == (chk = H5MM_malloc(sizeof(H5D_chunk_cache_mem_t))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL,
                    "memory allocation failed for intermediate chunk struct");

    chk->sel_space = NULL;
    chk->sel_buf   = NULL;
    chk->data_buf  = NULL;

    chk->sel_nbytes      = 0;
    chk->sel_alloc_size  = 0;
    chk->data_nbytes     = 0;
    chk->data_alloc_size = 0;

    *nbytes = *buf_size = 0;
    *chunk              = chk;

    /* Allocate udata */
    uptr = (H5D_chunk_ud_t *)H5MM_malloc(sizeof(H5D_chunk_ud_t));
    if (uptr == NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_CANTALLOC, FAIL, "could not malloc space for udata");

    memset(uptr, 0, sizeof(H5D_chunk_ud_t));

    *udata = uptr;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5D__struct_chunk_new_chunk() */

/*-------------------------------------------------------------------------
 * Function:    H5D__struct_chunk_condense
 *
 * Purpose:    Reallocates buffers as necessary so the total allocated size of buffers
 *             for the chunk (alloc_size) is equal to the total number of bytes
 *             used (nbytes).
 *
 *             Optional, if not present the chunk cache will be more likely to
 *             evict chunks if there is wasted space in the buffers.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__struct_chunk_condense(H5D_t *dset, size_t *nbytes /*in, out*/, void **chunk /*in, out*/,
                           void H5_ATTR_UNUSED *udata)
{
    H5D_chunk_cache_mem_t *chk       = (H5D_chunk_cache_mem_t *)*chunk; /* Chunk's memory cache info */
    herr_t                 ret_value = SUCCEED;                         /* Return value */

    FUNC_ENTER_PACKAGE

    /* Sanity checks */
    assert(dset);

    if ((chk->sel_alloc_size + chk->data_alloc_size) == (chk->sel_nbytes + chk->data_nbytes))
        /* Nothing to condense */
        HGOTO_DONE(SUCCEED);
    if (NULL == (chk->sel_buf = H5MM_realloc(chk->sel_buf, chk->sel_nbytes)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory reallocation failed for raw data chunk");
    if (NULL == (chk->data_buf = H5MM_realloc(chk->data_buf, chk->data_nbytes)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory reallocation failed for raw data chunk");

    chk->sel_alloc_size  = chk->sel_nbytes;
    chk->data_alloc_size = chk->data_nbytes;
    *nbytes              = chk->sel_nbytes + chk->data_nbytes;
    *chunk               = chk;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5D__struct_chunk_new_chunk() */

/*-------------------------------------------------------------------------
 * Function:    H5D__struct_chunk_encode
 *
 * Purpose:     Compresses/encodes the chunk as necessary.
 *              If chunk is the same as cache_buf, leaves *write_buf as NULL.
 *
 *              This function leaves chunk alone and allocates write_buf if necessary
 *              to hold compressed data, sets *write_size to the size of the data
 *              in write_buf, and sets *write_size_alloc to the size of write_buf,
 *              if it was allocated.
 *
 *              partial_bound is true if the chunk is partially outside the bounds
 *              of the dataset. If the dataset reported partial_bound_chunks_different_encoding
 *              as false, the setting of partial_bound is undefined.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 * NOTE: On entry: [chunk] points to the chunk intermediate struct
 * NOTE: On exit: [write_buf] points to the on disk file format chunk buffer
 *
 * NOTE: --chunk_encode callback: fill in udata: offset, unfilt_size, filt_mask,
 * NOTE: --chunk_insert callback: fill in udata: addr, nbytes, chunk_idx
 *
 * NOTE: Only handle two sections for now
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__struct_chunk_encode(H5D_t *dset, hsize_t *write_size /*out*/, hsize_t *write_buf_alloc /*out*/,
                         bool partial_bound, const void *chunk, void *_udata, void **write_buf /*out*/)
{
    const H5D_chunk_cache_mem_t *chk   = (const H5D_chunk_cache_mem_t *)chunk; /* Chunk memory cache info */
    H5D_chunk_ud_t              *udata = (H5D_chunk_ud_t *)_udata;
    void                        *data_buf = NULL;
    uint8_t                     *p     = NULL;
    unsigned char               *sel_p = NULL;
    size_t                       sel_nbytes, sel_alloc_size;
    size_t                       data_nbytes, data_alloc_size;
    H5O_pline_t                 *pline    = NULL; /* I/O pipeline info */
    hbool_t                      filtered = false;
    void                        *tot_buf  = NULL;
    hsize_t                      nelmts;
    size_t                       type_size;
    uint32_t                     metadata_chksum;
    herr_t                       ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_PACKAGE

    /* Sanity checks */
    assert(dset);

    pline = &(dset->shared->dcpl_cache.pline);
    if (pline && pline->tot_filt_nsects)
        filtered = true;

    /* Determine size of selection dataspace */
    if (H5S_encode(chk->sel_space, &sel_p, &sel_nbytes) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "unable to get encoded dataspace size");

    /* Allocate buffer for selection */
    sel_alloc_size = sel_nbytes;
    if (NULL == (tot_buf = H5MM_malloc(sel_alloc_size + H5_SIZEOF_CHKSUM)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for the chunk");

    sel_p = tot_buf;

    /* Encode the selection */
    if (H5S_encode(chk->sel_space, &sel_p, &sel_nbytes) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "unable to encode dataspace");

    /* Compute metadata checksum for sel_space */
    metadata_chksum = H5_checksum_metadata(tot_buf, (size_t)sel_nbytes, 0);

    /* Encode metadata checksum for the selection */
    p = (uint8_t *)tot_buf + sel_nbytes;
    UINT32ENCODE(p, metadata_chksum);

    sel_nbytes += H5_SIZEOF_CHKSUM;
    sel_alloc_size += H5_SIZEOF_CHKSUM;

    /* Get the number of elements in the selection */
    nelmts    = H5S_GET_SELECT_NPOINTS(chk->sel_space);
    type_size = H5T_GET_SIZE(dset->shared->type);

    assert(nelmts * type_size == chk->data_nbytes);
    assert(chk->data_alloc_size >= chk->data_nbytes);

    data_nbytes     = chk->data_nbytes;
    data_alloc_size = chk->data_alloc_size;

    if (NULL == (data_buf = H5MM_malloc(data_alloc_size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for the chunk");

    H5MM_memcpy(data_buf, chk->data_buf, chk->data_nbytes);

    /* Compression */
    if (filtered) {
        H5Z_EDC_t              err_detect; /* Error detection info */
        H5Z_cb_t               filter_cb;  /* I/O filter callback function */
        unsigned               i;
        H5Z_stc_filter_sect_t *filt_sect;

        udata->unfilt_size[0] = sel_nbytes;
        udata->unfilt_size[1] = data_nbytes;

        if (!partial_bound) {

            /* Retrieve filter settings from API context */
            if (H5CX_get_err_detect(&err_detect) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't get error detection info");
            if (H5CX_get_filter_cb(&filter_cb) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't get I/O filter callback function");

            for (i = 0, filt_sect = &pline->filt_sects[0]; i < pline->tot_filt_nsects; i++, filt_sect++) {

                if (filt_sect->nused) {
                    switch (filt_sect->seq_sect) {
                        case H5_SECTION_SELECTION:
                            if (H5Z_apply_filters(filt_sect->nused, filt_sect->filter, 0,
                                                  &udata->filt_mask[0], err_detect, filter_cb, &sel_nbytes,
                                                  &sel_alloc_size, &tot_buf) < 0)
                                HGOTO_ERROR(H5E_DATASET, H5E_CANTFILTER, FAIL, "output pipeline failed");
                            break;

                        case H5_SECTION_FIXED:
                            if (H5Z_apply_filters(filt_sect->nused, filt_sect->filter, 0,
                                                  &udata->filt_mask[1], err_detect, filter_cb, &data_nbytes,
                                                  &data_alloc_size, &data_buf) < 0)
                                HGOTO_ERROR(H5E_DATASET, H5E_CANTFILTER, FAIL, "output pipeline failed");
                            break;

                        case H5_SECTION_VL:
                        case H5_SECTION_NUM:
                        default:
                            assert(0 && "Unknown action?!?");
                    }
                } /* end if nused */

            } /* end for */
        }
    }

    /* Re-allocate write_buf to include + data */
    if (NULL == (tot_buf = H5MM_realloc(tot_buf, sel_alloc_size + data_alloc_size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for the chunk");

    /* Copy data to tot_buf */
    H5MM_memcpy((uint8_t *)tot_buf + sel_nbytes, data_buf, data_nbytes);

    udata->offset[0] = 0; /* Filler */
    udata->offset[1] = sel_nbytes;

    *write_size      = sel_nbytes + data_nbytes;
    *write_buf_alloc = sel_alloc_size + data_alloc_size;
    *write_buf       = tot_buf;

done:
    if (data_buf)
        data_buf = H5MM_xfree(data_buf);

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5D__struct_chunk_encode() */

/*------------------------------------------------------------------------
 * Function:    H5D__struct_chunk_encode_in_place
 *
 * Purpose:     The same as H5D_struct_chunk_encode()  but does not preserve
 *              chunk buffer, encoding is performed in-place.
 *              Must free all other data used.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * NOTE: On entry: [chunk] points to the chunk intermediate struct
 * NOTE: On exit: [chunk] points to the on disk file format chunk buffer
 *
 * NOTE:  --chunk_encode: fill in udata: offset, unfilt_size, filt_mask,
 * NOTE:  --chunk_insert: fill in udata: addr, nbytes, chunk_idx
 *
 * NOTE: Only handle two sections for now
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__struct_chunk_encode_in_place(H5D_t *dset, size_t *write_size /*out*/, bool partial_bound,
                                  void **chunk /*in,out*/, void *_udata)
{
    H5D_chunk_cache_mem_t *chk   = (H5D_chunk_cache_mem_t *)*chunk; /* Chunk memory cache info */
    H5D_chunk_ud_t        *udata = (H5D_chunk_ud_t *)_udata;
    H5O_pline_t           *pline; /* I/O pipeline info */
    hbool_t                filtered = false;
    uint32_t               metadata_chksum;
    uint8_t               *p;
    unsigned char         *sel_p = NULL;
    H5D_chunk_cache_mem_t *tmp;
    hsize_t                nelmts;
    size_t                 type_size;
    herr_t                 ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_PACKAGE

    /* Sanity checks */
    assert(dset);

    pline = &(dset->shared->dcpl_cache.pline);
    if (pline && pline->tot_filt_nsects)
        filtered = true;

    /* Determine size of selection dataspace */

    /* Determine size of selection dataspace */
    if (H5S_encode(chk->sel_space, &sel_p, &chk->sel_nbytes) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "unable to get encoded dataspace size");

    chk->sel_alloc_size = chk->sel_nbytes;

    if (NULL == (chk->sel_buf = H5MM_realloc(chk->sel_buf, chk->sel_alloc_size + H5_SIZEOF_CHKSUM)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL,
                    "memory allocation failed for intermediate chunk struct");

    sel_p = chk->sel_buf;

    /* Encode the selection */
    if (H5S_encode(chk->sel_space, &sel_p, &chk->sel_nbytes) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "unable to encode dataspace");

    /* Compute metadata checksum for chk->sel_space into chk->data_buf */
    metadata_chksum = H5_checksum_metadata(chk->sel_buf, (size_t)chk->sel_nbytes, 0);

    /* Encode metadata checksum for the selection */
    p = (uint8_t *)chk->sel_buf + chk->sel_nbytes;
    UINT32ENCODE(p, metadata_chksum);

    chk->sel_nbytes += H5_SIZEOF_CHKSUM;
    chk->sel_alloc_size += H5_SIZEOF_CHKSUM;

    /* Get the number of elements in the selection */
    nelmts    = H5S_GET_SELECT_NPOINTS(chk->sel_space);
    type_size = H5T_GET_SIZE(dset->shared->type);

    assert(nelmts * type_size == chk->data_nbytes);
    assert(chk->data_alloc_size >= chk->data_nbytes);

    /* Compression */
    if (filtered) {
        H5Z_EDC_t              err_detect; /* Error detection info */
        H5Z_cb_t               filter_cb;  /* I/O filter callback function */
        unsigned               i;
        H5Z_stc_filter_sect_t *filt_sect;

        udata->unfilt_size[0] = chk->sel_nbytes;
        udata->unfilt_size[1] = chk->data_nbytes;

        if (!partial_bound) {

            /* Retrieve filter settings from API context */
            if (H5CX_get_err_detect(&err_detect) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't get error detection info");
            if (H5CX_get_filter_cb(&filter_cb) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't get I/O filter callback function");

            for (i = 0, filt_sect = &pline->filt_sects[0]; i < pline->tot_filt_nsects; i++, filt_sect++) {

                if (filt_sect->nused) {
                    switch (filt_sect->seq_sect) {
                        case H5_SECTION_SELECTION:
                            if (H5Z_apply_filters(filt_sect->nused, filt_sect->filter, 0,
                                                  &udata->filt_mask[0], err_detect, filter_cb,
                                                  &chk->sel_nbytes, &chk->sel_alloc_size, &chk->sel_buf) < 0)
                                HGOTO_ERROR(H5E_DATASET, H5E_CANTFILTER, FAIL, "output pipeline failed");
                            break;

                        case H5_SECTION_FIXED:
                            if (H5Z_apply_filters(
                                    filt_sect->nused, filt_sect->filter, 0, &udata->filt_mask[1], err_detect,
                                    filter_cb, &chk->data_nbytes, &chk->data_alloc_size, &chk->data_buf) < 0)
                                HGOTO_ERROR(H5E_DATASET, H5E_CANTFILTER, FAIL, "output pipeline failed");
                            break;

                        case H5_SECTION_VL:
                        case H5_SECTION_NUM:
                        default:
                            assert(0 && "Unknown action?!?");
                    }
                } /* end if nused */

            } /* end for */
        }
    }

    /* Realloc chk->data_buf to provide space for encoded selection and data */
    if (NULL == (chk->data_buf = H5MM_realloc(chk->data_buf, chk->sel_nbytes + chk->data_nbytes)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory reallocation failed for data chunk");

    /* Shift data values to the right to provide space for encoded selection */
    memmove((uint8_t *)(chk->data_buf) + chk->sel_nbytes, chk->data_buf, chk->data_nbytes);

    H5MM_memcpy(chk->data_buf, chk->sel_buf, chk->sel_nbytes);

    tmp = chk;

    *chunk           = chk->data_buf;
    *write_size      = (chk->sel_nbytes + chk->data_nbytes);
    udata->offset[1] = chk->sel_nbytes;

    /* Free chk->sel_buf */
    chk->sel_buf    = H5MM_xfree(chk->sel_buf);
    chk->sel_nbytes = chk->sel_alloc_size = 0;

    /* Close chk->sel_space */
    if (chk->sel_space && H5S_close(chk->sel_space) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTRELEASE, FAIL, "can't release dataspace for encoded selection");

    tmp = H5MM_xfree(tmp);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5D__struct_chunk_encode_in_place() */

/*-------------------------------------------------------------------------
 * Function:    H5D__struct_chunk_evict
 *
 * Purpose:     Frees chunk and all memory referenced by it.
 *              Optional, if not present free() is simply used.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * NOTE: [udata] is not used??
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__struct_chunk_evict(H5D_t *dset, void *chunk, void *udata)
{
    H5D_chunk_cache_mem_t *chk       = (H5D_chunk_cache_mem_t *)chunk; /* Chunk memory cache info */
    herr_t                 ret_value = SUCCEED;                        /* Return value */

    FUNC_ENTER_PACKAGE

    /* Sanity checks */
    assert(dset);

    /* Free the sel_buf + data_buffer */
    chk->sel_buf  = H5MM_xfree(chk->sel_buf);
    chk->data_buf = H5MM_xfree(chk->data_buf);

    /* Close the encoded dataspace */
    if (chk->sel_space && H5S_close(chk->sel_space) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTRELEASE, FAIL, "can't release dataspace for encoded selection");

    /* Free the chunk memory cache info structure */
    chk = H5MM_xfree(chk);

    udata = H5MM_xfree(udata);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5D__struct_chunk_evict() */

/*-------------------------------------------------------------------------
 * Function:    H5D__struct_chunk_insert
 *
 * Purpose:     Inserts (or reinserts) count chunks into the chunk index if necessary.
 *              Old address and size (if any) of the chunks on disk are passed
 *              as addr and old_disk_size, the new size is passed in as new_disk_size.
 *
 *              This function resizes and reallocates on disk if necessary,
 *              returning the address of the chunks on disk in *addr.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 * NOTE: --chunk_encode callback: fill in udata: offset, unfilt_size, filt_mask,
 * NOTE: --chunk_insert callback: fill in udata: addr, nbytes, chunk_idx
 *
 * NOTE: [chunk] not used??
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__struct_chunk_insert(H5D_t *dset, size_t count, const hsize_t *scaled[] /*in*/,
                         haddr_t *addr[] /*in,out*/, hsize_t old_disk_size[], hsize_t new_disk_size[],
                         void H5_ATTR_UNUSED *chunk[] /*in*/, void *_udata[])
{
    H5D_chunk_ud_t             *udata;
    H5D_chk_idx_info_t          idx_info; /* Chunked index info */
    H5O_storage_struct_chunk_t *storage     = &(dset->shared->layout.storage.u.struct_chunk);
    H5O_layout_struct_chunk_t  *layout      = &(dset->shared->layout.u.struct_chunk);
    bool                        need_alloc  = true;
    bool                        need_insert = true;
    size_t                      i;
    H5D_chunk_ud_t             *my_udata;
    herr_t                      ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_PACKAGE

    /* Sanity checks */
    assert(dset);
    assert(storage->idx_type != H5D_CHUNK_IDX_NONE);
    assert(storage->idx_type != H5D_CHUNK_IDX_BTREE);

    /* Compose chunked index info struct */
    idx_info.f           = dset->oloc.file;
    idx_info.pline       = &dset->shared->dcpl_cache.pline;
    idx_info.stc_layout  = layout;
    idx_info.stc_storage = storage;

    /* Allocage  my_udata */
    my_udata = (H5D_chunk_ud_t *)H5MM_malloc(sizeof(H5D_chunk_ud_t));
    if (my_udata == NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_CANTALLOC, FAIL, "could not malloc space for udata");

    for (i = 0; i < count; i++) {

        memset(my_udata, 0, sizeof(H5D_chunk_ud_t));

        my_udata->common.stc_layout  = layout;
        my_udata->common.stc_storage = storage;
        my_udata->common.scaled      = scaled[i];

        my_udata->chunk_block.offset = HADDR_UNDEF;
        my_udata->chunk_block.length = 0;

        /* chunk_idx is calculated in get_addr callback */
        if ((storage->ops->get_addr)(&idx_info, my_udata) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't query chunk address");

        if (H5_addr_defined(*addr[i])) {
            assert(*addr[i] == my_udata->chunk_block.offset);
            assert(old_disk_size[i] == my_udata->chunk_block.length);

            if (old_disk_size[i] == new_disk_size[i]) {
                need_alloc  = false;
                need_insert = false;
            }
            else {
                if (H5MF_xfree(dset->oloc.file, H5FD_MEM_DRAW, *addr[i], old_disk_size[i]) < 0)
                    HGOTO_ERROR(H5E_DATASET, H5E_CANTFREE, FAIL, "unable to free chunk");
            }
        }
        else
            assert(!H5_addr_defined(my_udata->chunk_block.offset));

        if (need_alloc) {
            *addr[i] = H5MF_alloc(dset->oloc.file, H5FD_MEM_DRAW, new_disk_size[i]);
            if (!H5_addr_defined(*addr[i]))
                HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "file allocation failed");
        }

        if (need_insert) {

            udata = (H5D_chunk_ud_t *)_udata[i];

            udata->chunk_block.offset = *(addr[i]);
            udata->chunk_block.length = new_disk_size[i];
            udata->chunk_idx          = my_udata->chunk_idx;
            udata->common.scaled      = scaled[i];

            if (storage->ops->insert) {
                if ((storage->ops->insert)(&idx_info, udata, dset) < 0)
                    HGOTO_ERROR(H5E_DATASET, H5E_CANTINSERT, FAIL, "unable to insert chunk addr into index");
            }
        }

    } /* end for */

done:
    if (my_udata)
        H5MM_xfree(my_udata);

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5D__struct_chunk_insert() */

/*-------------------------------------------------------------------------
 * Function:    H5D__struct_chunk_vector_read
 *
 * Purpose:     Called when the chunk cache wants to read data directly from the
 *              disk to the user buffer via vector I/O.
 *              If not possible due to compression, etc, returns vector_possible=false.
 *              Otherwise returns the vector of selected elements in offsets
 *              (within the file, not the chunk, this is why addr is passed in)
 *              and sizes, with the number of vectors returned in vec_count.
 *
 *              chunk may be passed as NULL, and may also be an in-cache chunk that
 *              only contains information on defined values.
 *
 *              If chunk is passed as NULL and the callback requires a chunk to be
 *              passed with (at least) the defined values selection, this callback
 *              shall return *require_values=true and
 *              *vec_count=0, *offsets=NULL, and *sizes=NULL.
 *
 *              Optional, if not present, chunk I/O is only performed on entire chunks
 *              or with selection I/O. The H5SC code checks for type conversion before
 *              calling this.
 *
 *              partial_bound is true if the on-disk chunk was encoded with partial_bound
 *              set to true. If the dataset reported partial_bound_chunks_different_encoding
 *              as false, the setting of partial_bound is undefined.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * NOTE: [udata] is not not used ??
 *
 * NOTE: looks like vector_read and vector write callbacks are the same??
 * NOTE: modified from H5FD__read/write_selection_translate()
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__struct_chunk_vector_read(H5D_t *dset, haddr_t addr, const H5S_t *file_space_in,
                              bool H5_ATTR_UNUSED partial_bound, void *chunk /*in*/,
                              size_t *vec_count /*out*/, haddr_t **offsets /*out*/, size_t **sizes /*out*/,
                              bool *vector_possible /*out*/, bool *require_values /*out*/,
                              void H5_ATTR_UNUSED *udata)
{
    H5D_chunk_cache_mem_t  *chk = (H5D_chunk_cache_mem_t *)chunk; /* Chunk memory cache info */
    H5O_pline_t            *pline;                                /* I/O pipeline info */
    size_t                  elmt_size = 0;
    haddr_t                *vec_addrs = NULL;
    size_t                 *vec_sizes = NULL;
    hsize_t                 file_off[SEQ_LIST_LEN];
    size_t                  file_len[SEQ_LIST_LEN];
    size_t                  file_seq_i;
    size_t                  file_nseq;
    size_t                  io_len;
    size_t                  file_nelmts;
    hsize_t                 chk_nelmts;
    hssize_t                hss_nelmts;
    size_t                  seq_nelem;
    H5S_sel_iter_t         *file_iter      = NULL;
    bool                    file_iter_init = false;
    size_t                  vec_arr_nused  = 0;
    size_t                  vec_arr_nalloc = VECTOR_LEN;
    H5S_t                  *serial_values_space;
    H5S_t                  *serial_file_space;
    H5_flexible_const_ptr_t flex_selection;
    herr_t                  ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE

    /* Sanity checks */
    assert(dset);

    if (chk == NULL) {
        *require_values = true;
        *vec_count      = 0;
        *offsets        = NULL;
        *sizes          = NULL;
        HGOTO_DONE(SUCCEED);
    }

    pline = &(dset->shared->dcpl_cache.pline);
    if (pline && pline->tot_filt_nsects) {
        /* true: a NOT-to-be-filtered-partial-edge chunk */
        /* false : a to-be-filtered-partial-edge-chunk */
        if (!partial_bound) {
            *vector_possible = false;
            HGOTO_DONE(SUCCEED);
        }
    }
    *vector_possible = true;

    assert(chk != NULL);
    assert(chk->sel_space = NULL);

    /* Get the number of elements in chk->sel_space */
    if ((hss_nelmts = (hssize_t)H5S_GET_SELECT_NPOINTS(chk->sel_space)) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_CANTCOUNT, FAIL, "can't get number of elements selected");
    H5_CHECKED_ASSIGN(chk_nelmts, hsize_t, hss_nelmts, hssize_t);

    /* Get the number of elements in file_space_in */
    if ((hss_nelmts = (hssize_t)H5S_GET_SELECT_NPOINTS(file_space_in)) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_CANTCOUNT, FAIL, "can't get number of elements selected");
    H5_CHECKED_ASSIGN(file_nelmts, size_t, hss_nelmts, hssize_t);

    if (NULL == (serial_values_space = H5S_create_simple(1, &chk_nelmts, NULL)))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTCREATE, FAIL, "unable to create simple memory dataspace");

    flex_selection.cvp = file_space_in;
    if (H5S_select_project_intersection(chk->sel_space, serial_values_space, flex_selection.vp,
                                        &serial_file_space, true) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTCLIP, FAIL,
                    "can't project the intersection of erased space and src_space");

    if (0 == (elmt_size = H5T_get_size(dset->shared->type)))
        HGOTO_ERROR(H5E_DATATYPE, H5E_BADSIZE, FAIL, "datatype size invalid");

    if (NULL == (file_iter = H5FL_MALLOC(H5S_sel_iter_t)))
        HGOTO_ERROR(H5E_VFL, H5E_CANTALLOC, FAIL, "couldn't allocate file selection iterator");

    /* Initialize sequence lists for file space */
    if (H5S_select_iter_init(file_iter, serial_file_space, elmt_size, H5S_SEL_ITER_GET_SEQ_LIST_SORTED) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_CANTINIT, FAIL, "can't initialize sequence list for file space");
    file_iter_init = true;

    /* Initialize values so sequence lists are retrieved on the first
     * iteration */
    file_seq_i = SEQ_LIST_LEN;
    file_nseq  = 0;

    if (NULL == (vec_addrs = H5MM_malloc(VECTOR_LEN * sizeof(haddr_t *))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "memory allocation failed for vector addrs");

    if (NULL == (vec_sizes = H5MM_malloc(VECTOR_LEN * sizeof(size_t *))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "memory allocation failed for vector sizes");

    /* Loop until all elements are processed */
    while (file_seq_i < file_nseq || file_nelmts > 0) {
        /* Fill/refill file sequence list if necessary */
        if (file_seq_i == SEQ_LIST_LEN) {
            if (H5S_SELECT_ITER_GET_SEQ_LIST(file_iter, SEQ_LIST_LEN, SIZE_MAX, &file_nseq, &seq_nelem,
                                             file_off, file_len) < 0)
                HGOTO_ERROR(H5E_INTERNAL, H5E_UNSUPPORTED, FAIL, "sequence length generation failed");
            assert(file_nseq > 0);

            file_nelmts -= seq_nelem;
            file_seq_i = 0;
        }
        assert(file_seq_i < file_nseq);

        /* Calculate length of this IO */
        io_len = file_len[file_seq_i];

        if (vec_arr_nused == vec_arr_nalloc) {
            void *tmp_ptr;

            /* Reallocate arrays */
            if (NULL == (tmp_ptr = H5MM_realloc(vec_addrs, vec_arr_nalloc * 2 * sizeof(*vec_addrs))))
                HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "memory reallocation failed for address list");
            vec_addrs = tmp_ptr;

            if (NULL == (vec_sizes = H5MM_realloc(vec_sizes, vec_arr_nalloc * 2 * sizeof(*vec_sizes) * 2)))
                HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "memory reallocation failed for size list");
            vec_sizes = tmp_ptr;

            /* Record that we've doubled the array sizes */
            vec_arr_nalloc *= 2;
        }

        /* Add this segment to vector read list */
        vec_addrs[vec_arr_nused] = addr + file_off[file_seq_i];
        vec_sizes[vec_arr_nused] = io_len;

        vec_arr_nused++;

        /* Update file sequence */
        if (io_len == file_len[file_seq_i])
            file_seq_i++;
        else {
            file_off[file_seq_i] += io_len;
            file_len[file_seq_i] -= io_len;
        }
    }

    *vec_count = vec_arr_nused;
    *offsets   = vec_addrs;
    *sizes     = vec_sizes;

done:
    /* Terminate and free iterators */
    if (file_iter) {
        if (file_iter_init && H5S_SELECT_ITER_RELEASE(file_iter) < 0)
            HGOTO_ERROR(H5E_INTERNAL, H5E_CANTFREE, FAIL, "can't release file selection iterator");
        file_iter = H5FL_FREE(H5S_sel_iter_t, file_iter);
    }

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5D__struct_chunk_vector_read() */

/*-------------------------------------------------------------------------
 * Function:    H5D__struct_chunk_vector_write
 *
 * Purpose:     Called when the chunk cache wants to write data directly from the
 *              user buffer to the cache via vector I/O.
 *              If not possible due to compression, etc, returns vector_possible=false.
 *              Otherwise returns the vector of selected elements in offsets
 *              (within the file, not the chunk, this is why addr is passed in)
 *              and sizes, with the number of vectors returned in vec_count.
 *
 *              chunk may be passed as NULL, and may also be an in-cache chunk
 *              that only contains information on defined values.
 *
 *              If chunk is passed as NULL and the callback requires a chunk to be
 *              passed with (at least) the defined values selection, this callback
 *              shall return *require_values=true and
 *              *vec_count=0, *offsets=NULL, and *sizes=NULL.
 *
 *              Optional, if not present, chunk I/O is only performed on entire chunks
 *              or with selection I/O.
 *              The H5SC code checks for type conversion before calling this.
 *
 *              partial_bound is true if the on-disk chunk was encoded with partial_bound
 *              set to true. If the dataset reported partial_bound_chunks_different_encoding
 *              as false, the setting of partial_bound is undefined.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * NOTE: [udata] is not not used ??
 *
 * NOTE: looks like vector_read and vector write callbacks are the same??
 * NOTE: modified from H5FD__read/write_selection_translate()
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__struct_chunk_vector_write(H5D_t *dset, haddr_t addr, const H5S_t *file_space_in, bool partial_bound,
                               void *chunk /*in*/, size_t *vec_count /*out*/, haddr_t **offsets /*out*/,
                               size_t **sizes /*out*/, bool *vector_possible /*out*/,
                               bool *require_values /*out*/, void H5_ATTR_UNUSED *udata)
{
    H5D_chunk_cache_mem_t  *chk       = (H5D_chunk_cache_mem_t *)chunk; /* Chunk memory cache info */
    size_t                  elmt_size = 0;
    haddr_t                *vec_addrs = NULL;
    size_t                 *vec_sizes = NULL;
    hsize_t                 file_off[SEQ_LIST_LEN];
    size_t                  file_len[SEQ_LIST_LEN];
    size_t                  file_seq_i;
    size_t                  file_nseq;
    size_t                  io_len;
    size_t                  file_nelmts;
    hsize_t                 chk_nelmts;
    hssize_t                hss_nelmts;
    size_t                  seq_nelem;
    H5S_sel_iter_t         *file_iter      = NULL;
    bool                    file_iter_init = false;
    size_t                  vec_arr_nused  = 0;
    size_t                  vec_arr_nalloc = VECTOR_LEN;
    H5O_pline_t            *pline = NULL; /* I/O pipeline info */
    H5S_t                  *serial_values_space;
    H5S_t                  *serial_file_space;
    H5_flexible_const_ptr_t flex_selection;
    herr_t                  ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE

    /* Sanity checks */
    assert(dset);

    if (chk == NULL) {
        *require_values = true;
        *vec_count      = 0;
        *offsets        = NULL;
        *sizes          = NULL;
        HGOTO_DONE(SUCCEED);
    }

    if (pline && pline->tot_filt_nsects) {
        /* true: a NOT-to-be-filtered-partial-edge chunk */
        /* false : a to-be-filtered-partial-edge-chunk */
        if (!partial_bound) {
            *vector_possible = false;
            HGOTO_DONE(SUCCEED);
        }
    }

    *vector_possible = true;

    assert(chk != NULL);
    assert(chk->sel_space != NULL);

    /* Get the number of elements in chk->sel_space */
    if ((hss_nelmts = (hssize_t)H5S_GET_SELECT_NPOINTS(chk->sel_space)) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_CANTCOUNT, FAIL, "can't get number of elements selected");
    H5_CHECKED_ASSIGN(chk_nelmts, hsize_t, hss_nelmts, hssize_t);

    /* Get the number of elements in file_space_in */
    if ((hss_nelmts = (hssize_t)H5S_GET_SELECT_NPOINTS(file_space_in)) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_CANTCOUNT, FAIL, "can't get number of elements selected");
    H5_CHECKED_ASSIGN(file_nelmts, size_t, hss_nelmts, hssize_t);

    if (NULL == (serial_values_space = H5S_create_simple(1, &chk_nelmts, NULL)))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTCREATE, FAIL, "unable to create simple memory dataspace");

    flex_selection.cvp = file_space_in;
    if (H5S_select_project_intersection(chk->sel_space, serial_values_space, flex_selection.vp,
                                        &serial_file_space, true) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTCLIP, FAIL,
                    "can't project the intersection of erased space and src_space");

    if (0 == (elmt_size = H5T_get_size(dset->shared->type)))
        HGOTO_ERROR(H5E_DATATYPE, H5E_BADSIZE, FAIL, "datatype size invalid");

    if (NULL == (file_iter = H5FL_MALLOC(H5S_sel_iter_t)))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "can't allocate file iterator");

    if (H5S_select_iter_init(file_iter, serial_file_space, elmt_size, H5S_SEL_ITER_GET_SEQ_LIST_SORTED) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to initialize file selection information");
    file_iter_init = true; /* file selection iteration info has been initialized */

    /* Initialize values so sequence lists are retrieved on the first
     * iteration */
    file_seq_i = SEQ_LIST_LEN;
    file_nseq  = 0;

    if (NULL == (vec_addrs = H5MM_malloc(VECTOR_LEN * sizeof(haddr_t *))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "memory allocation failed for vector addrs");

    if (NULL == (vec_sizes = H5MM_malloc(VECTOR_LEN * sizeof(size_t *))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "memory allocation failed for vector sizes");

    /* Loop until all elements are processed */
    while (file_seq_i < file_nseq || file_nelmts > 0) {
        /* Fill/refill file sequence list if necessary */
        if (file_seq_i == SEQ_LIST_LEN) {
            if (H5S_SELECT_ITER_GET_SEQ_LIST(file_iter, SEQ_LIST_LEN, SIZE_MAX, &file_nseq, &seq_nelem,
                                             file_off, file_len) < 0)
                HGOTO_ERROR(H5E_INTERNAL, H5E_UNSUPPORTED, FAIL, "sequence length generation failed");
            assert(file_nseq > 0);

            file_nelmts -= seq_nelem;
            file_seq_i = 0;
        }
        assert(file_seq_i < file_nseq);

        /* Calculate length of this IO */
        io_len = file_len[file_seq_i];

        if (vec_arr_nused == vec_arr_nalloc) {
            void *tmp_ptr;

            /* Reallocate arrays */
            if (NULL == (tmp_ptr = H5MM_realloc(vec_addrs, vec_arr_nalloc * 2 * sizeof(*vec_addrs))))
                HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "memory reallocation failed for address list");
            vec_addrs = tmp_ptr;

            if (NULL == (vec_sizes = H5MM_realloc(vec_sizes, vec_arr_nalloc * 2 * sizeof(*vec_sizes) * 2)))
                HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "memory reallocation failed for size list");
            vec_sizes = tmp_ptr;

            /* Record that we've doubled the array sizes */
            vec_arr_nalloc *= 2;
        }

        /* Add this segment to vector read list */
        vec_addrs[vec_arr_nused] = addr + file_off[file_seq_i];
        vec_sizes[vec_arr_nused] = io_len;

        vec_arr_nused++;

        /* Update file sequence */
        if (io_len == file_len[file_seq_i])
            file_seq_i++;
        else {
            file_off[file_seq_i] += io_len;
            file_len[file_seq_i] -= io_len;
        }
    }

    *vec_count = vec_arr_nused;
    *offsets   = vec_addrs;
    *sizes     = vec_sizes;

done:
    /* Terminate and free iterators */
    if (file_iter) {
        if (file_iter_init && H5S_SELECT_ITER_RELEASE(file_iter) < 0)
            HGOTO_ERROR(H5E_INTERNAL, H5E_CANTFREE, FAIL, "can't release file selection iterator");
        file_iter = H5FL_FREE(H5S_sel_iter_t, file_iter);
    }

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5D__struct_chunk_vector_write() */

/*-------------------------------------------------------------------------
 * Function:    H5D__struct_chunk_scatter_mem
 *
 * Purpose:     Scatters data from the chunk buffer into the memory buffer (in dset_info),
 *              performing type conversion if necessary.
 *              file_space's extent matches the chunk dimensions and the selection is within the chunk.
 *              mem_space's extent matches the entire memory buffer's and the selection within it is
 *              the selected values within the chunk, offset appropriately within the full extent.
 *              Optional, if not present, chunk is the same in memory as it is in cache, with the
 *              exception of type conversion (which will be handled by the H5SC layer).
 *              If the layout stores variable length data within the chunk this callback must be defined.
 *              TBD: the following description probably should not be here in the RFC:
 *              [partial_bound is true if the on-disk chunk was encoded with partial_bound set to true.
 *              If the dataset reported partial_bound_chunks_different_encoding as false,
 *              the setting of partial_bound is undefined.]
 *
 * Return:    Non-negative on success/Negative on failure
 *
 * NOTE: [chunk] is the pointer to the chunk intermediate struct
 * NOTE: This routine is modified from H5D__scatgath_read()
 *
 * NOTE: [udata] not used??
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__struct_chunk_scatter_mem(H5D_dset_io_info_t *dset_info, H5D_io_type_info_t *io_type_info,
                              const H5S_t *mem_space, const H5S_t *file_space, const void *chunk,
                              void H5_ATTR_UNUSED *udata)
{
    void           *buf;                    /* Local pointer to application buffer */
    void           *tmp_buf;                /* Buffer to use for type conversion */
    H5S_sel_iter_t *file_iter      = NULL;  /* Memory selection iteration info*/
    bool            file_iter_init = false; /* Memory selection iteration info has been initialized */
    H5S_sel_iter_t *mem_iter       = NULL;  /* Memory selection iteration info*/
    bool            mem_iter_init  = false; /* Memory selection iteration info has been initialized */
    H5S_sel_iter_t *bkg_iter       = NULL;  /* Background iteration info*/
    bool            bkg_iter_init  = false; /* Background iteration info has been initialized */
    H5S_sel_iter_t *sel_iter       = NULL;  /* Memory selection iteration info*/
    bool            sel_iter_init  = false; /* Memory selection iteration info has been initialized */
    hsize_t         nelmts = 0;                 /* Number of elements selected in file & memory dataspaces */
    hsize_t         smine_start;            /* Strip mine start loc */
    size_t          smine_nelmts;           /* Elements per strip   */
    bool            in_place_tconv = false;     /* Whether to perform in-place type_conversion */
    size_t          mem_type_size;
    size_t          file_type_size;
    size_t          buf_off          = 0; /* Buffer offset for in-place type conversion */
    const H5D_chunk_cache_mem_t *chk = (const H5D_chunk_cache_mem_t *)chunk; /* Chunk's memory cache info */
    void                        *data_scat_buf = NULL;
    hsize_t                      scat_buf_size;
    H5_flexible_const_ptr_t      flex_mspace;
    H5_flexible_const_ptr_t      flex_fspace;
    herr_t                       ret_value = SUCCEED; /* Return value     */

    FUNC_ENTER_PACKAGE

    assert(dset_info);
    assert(io_type_info);
    assert(mem_space);
    assert(file_space);
    assert(chk);

    /* Make certain that the number of elements in each selection is the same */
    nelmts = H5S_GET_SELECT_NPOINTS(mem_space);
    if (nelmts != H5S_GET_SELECT_NPOINTS(file_space))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
                    "src and dest dataspaces have different number of elements selected");

    /* Check for NOOP read */
    if (nelmts == 0)
        HGOTO_DONE(SUCCEED);

    mem_type_size  = dset_info->type_info.dst_type_size;
    file_type_size = dset_info->type_info.src_type_size;

    flex_mspace.cvp = mem_space;
    flex_fspace.cvp = file_space;

    /* Set buf pointer (memory buffer in dset_info) it's the application buffer */
    buf = dset_info->buf.vp;

    /* Allocate the data_scat_buf: chunk size * element size */
    scat_buf_size = dset_info->layout->u.struct_chunk.size;
    if (NULL == (data_scat_buf = H5FL_BLK_MALLOC(scat_buf, scat_buf_size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for scattered data buffer");
    memset(data_scat_buf, 0, scat_buf_size);

    /* Scatter dato to data_scat_buf according to chk->sel_space */
    if (chk->sel_space != NULL) {
        hsize_t sel_nelmts;

        /* Get the number of elements in the selection */
        sel_nelmts = H5S_GET_SELECT_NPOINTS(chk->sel_space);

        if (NULL == (sel_iter = H5FL_MALLOC(H5S_sel_iter_t)))
            HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "can't allocate selection iterator");

        if (H5S_select_iter_init(sel_iter, chk->sel_space, file_type_size, H5S_SEL_ITER_GET_SEQ_LIST_SORTED) <
            0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to initialize selection iter information");
        sel_iter_init = true;

        /* Scatter data values from chk->data_buf to data_scat_buf according to sel_space */
        if (H5D__scatter_mem(chk->data_buf, sel_iter, sel_nelmts, data_scat_buf /*out*/) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "mem scatter failed");

        if (sel_iter_init && H5S_SELECT_ITER_RELEASE(sel_iter) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CANTFREE, FAIL, "Can't release selection iterator");
        if (sel_iter)
            sel_iter = H5FL_FREE(H5S_sel_iter_t, sel_iter);
    }

    /*
     * If there is no data transform or type conversion then read directly
     * into the application's buffer.
     */
    if (dset_info->type_info.is_xform_noop && dset_info->type_info.is_conv_noop) {

        if (H5D_select_io_mem(buf, flex_mspace.vp, data_scat_buf, flex_fspace.vp, mem_type_size, nelmts))
            HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "couldn't copy chunk data to read buffer");
    }
    else { /* With type conversion */

        /* Check for in-place type conversion */
        if (io_type_info->may_use_in_place_tconv) {

            /* Make sure the memory type is not smaller than the file type, otherwise the memory buffer
               won't be big enough to serve as the type conversion buffer */
            if (mem_type_size >= file_type_size) {
                bool    is_contig;
                hsize_t sel_off;

                /* Check if the space is contiguous */
                if (H5S_select_contig_block(flex_mspace.vp, &is_contig, &sel_off, NULL) < 0)
                    HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "can't check if dataspace is contiguous");

                /* If the first sequence includes all the elements selected in this piece, it it contiguous */
                if (is_contig) {
                    H5_CHECK_OVERFLOW(sel_off, hsize_t, size_t);
                    in_place_tconv = true;
                    buf_off        = (size_t)sel_off * mem_type_size;
                }
            }
        }

        /* Check if we should disable in-place type conversion for performance.  Do so if we can use the
         * optimized compound read function, and the either entire I/O operation can fit in the type
         * conversion buffer or we need to use a background buffer (and therefore could not do the I/O in one
         * operation with in-place conversion * anyways). */
        if (in_place_tconv && H5D__SCATGATH_USE_CMPD_OPT_READ(dset_info, false) &&
            (dset_info->type_info.need_bkg || (nelmts <= dset_info->type_info.request_nelmts)))
            in_place_tconv = false;

        /* Allocate the iterators */
        if (NULL == (file_iter = H5FL_MALLOC(H5S_sel_iter_t)))
            HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "can't allocate file iterator");
        if (NULL == (mem_iter = H5FL_MALLOC(H5S_sel_iter_t)))
            HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "can't allocate memory iterator");
        if (NULL == (bkg_iter = H5FL_MALLOC(H5S_sel_iter_t)))
            HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "can't allocate background iterator");

        /* Figure out the strip mine size. */
        if (H5S_select_iter_init(file_iter, flex_fspace.vp, dset_info->type_info.src_type_size,
                                 H5S_SEL_ITER_GET_SEQ_LIST_SORTED) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to initialize file selection information");
        file_iter_init = true; /*file selection iteration info has been initialized */
        if (H5S_select_iter_init(mem_iter, flex_mspace.vp, dset_info->type_info.dst_type_size, 0) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to initialize memory selection information");
        mem_iter_init = true; /*file selection iteration info has been initialized */
        if (H5S_select_iter_init(bkg_iter, flex_mspace.vp, dset_info->type_info.dst_type_size, 0) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL,
                        "unable to initialize background selection information");
        bkg_iter_init = true; /*file selection iteration info has been initialized */

        /* Start strip mining... */
        for (smine_start = 0; smine_start < nelmts; smine_start += smine_nelmts) {
            size_t n; /* Elements operated on */

            /* Determine strip mine size. First check if we're doing in-place type conversion */
            if (in_place_tconv) {
                /* If there is a background buffer, we cannot exceed request_nelmts. */
                assert(!H5D__SCATGATH_USE_CMPD_OPT_READ(dset_info, in_place_tconv));
                if (dset_info->type_info.need_bkg)
                    smine_nelmts = (size_t)MIN(dset_info->type_info.request_nelmts, (nelmts - smine_start));
                else {
                    assert(smine_start == 0);
                    smine_nelmts = nelmts;
                }

                /* Calculate buffer position in user buffer */
                tmp_buf = (uint8_t *)buf + buf_off + (smine_start * dset_info->type_info.dst_type_size);
            }
            else {
                /* Do type conversion using intermediate buffer */
                tmp_buf = io_type_info->tconv_buf;

                /* Go figure out how many elements to read from the file */
                smine_nelmts = (size_t)MIN(dset_info->type_info.request_nelmts, (nelmts - smine_start));
            }

            /*
             * Gather the data from disk into the datatype conversion
             * buffer. Also gather data from application to background buffer
             * if necessary.
             */

            /* Fill background buffer here unless we will use H5D__compound_opt_read().  Must do this before
             * the read so the read buffer doesn't get wiped out if we're using in-place type conversion */
            if ((H5T_BKG_YES == dset_info->type_info.need_bkg) &&
                !H5D__SCATGATH_USE_CMPD_OPT_READ(dset_info, in_place_tconv)) {
                n = H5D__gather_mem(buf, bkg_iter, smine_nelmts, io_type_info->bkg_buf /*out*/);
                if (n != smine_nelmts)
                    HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, "mem gather failed");
            }

            /*
             * Gather data from data_scat_buf to tmp_buf
             */
            n = H5D__gather_mem(data_scat_buf, file_iter, smine_nelmts, tmp_buf /*out*/);
            if (n != smine_nelmts)
                HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, "mem gather failed");

            /* If the source and destination are compound types and subset of each other
             * and no conversion is needed, copy the data directly into user's buffer and
             * bypass the rest of steps.
             */
            if (H5D__SCATGATH_USE_CMPD_OPT_READ(dset_info, in_place_tconv)) {
                if (H5D__compound_opt_read(smine_nelmts, mem_iter, &dset_info->type_info, tmp_buf,
                                           buf /*out*/) < 0)
                    HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "datatype conversion failed");
            } /* end if */
            else {
                /*
                 * Perform datatype conversion.
                 */
                if (H5T_convert(dset_info->type_info.tpath, dset_info->type_info.src_type,
                                dset_info->type_info.dst_type, smine_nelmts, (size_t)0, (size_t)0, tmp_buf,
                                io_type_info->bkg_buf) < 0)
                    HGOTO_ERROR(H5E_DATASET, H5E_CANTCONVERT, FAIL, "datatype conversion failed");

                /* Do the data transform after the conversion (since we're using type mem_type) */
                if (!dset_info->type_info.is_xform_noop) {
                    H5Z_data_xform_t *data_transform; /* Data transform info */

                    /* Retrieve info from API context */
                    if (H5CX_get_data_transform(&data_transform) < 0)
                        HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't get data transform info");

                    if (H5Z_xform_eval(data_transform, tmp_buf, smine_nelmts, dset_info->type_info.mem_type) <
                        0)
                        HGOTO_ERROR(H5E_DATASET, H5E_BADVALUE, FAIL, "Error performing data transform");
                }

                /* Scatter the data into memory if this was not an in-place conversion */
                if (!in_place_tconv)
                    if (H5D__scatter_mem(tmp_buf, mem_iter, smine_nelmts, buf /*out*/) < 0)
                        HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "scatter failed");
            } /* end else */

        } /* end for */
    }

done:
    /* Release selection iterators */
    if (file_iter_init && H5S_SELECT_ITER_RELEASE(file_iter) < 0)
        HDONE_ERROR(H5E_DATASET, H5E_CANTFREE, FAIL, "Can't release selection iterator");
    if (file_iter)
        file_iter = H5FL_FREE(H5S_sel_iter_t, file_iter);

    if (mem_iter_init && H5S_SELECT_ITER_RELEASE(mem_iter) < 0)
        HDONE_ERROR(H5E_DATASET, H5E_CANTFREE, FAIL, "Can't release selection iterator");
    if (mem_iter)
        mem_iter = H5FL_FREE(H5S_sel_iter_t, mem_iter);

    if (bkg_iter_init && H5S_SELECT_ITER_RELEASE(bkg_iter) < 0)
        HDONE_ERROR(H5E_DATASET, H5E_CANTFREE, FAIL, "Can't release selection iterator");
    if (bkg_iter)
        bkg_iter = H5FL_FREE(H5S_sel_iter_t, bkg_iter);

    /* Release resources */
    if (data_scat_buf)
        data_scat_buf = H5FL_BLK_FREE(scat_buf, data_scat_buf);

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5D__struct_chunk_scatter_mem() */

/*-------------------------------------------------------------------------
 * Function:    H5D__struct_chunk_gather_mem
 *
 * Purpose:     Gathers data from the memory buffer (in dset_info) into the chunk buffer,
 *              performing type conversion if necessary.
 *              file_space's extent matches the chunk dimensions and the selection is within
 *              the chunk.
 *              mem_space's extent matches the entire memory buffer's and the selection within it
 *              is the selected values within the chunk, offset appropriately within the full extent.
 *              Defines selected values in the chunk.
 *              Optional, if not present, chunk is the same in memory as it is in cache, with the
 *              exception of type conversion (which will be handled by H5SC layer).
 *              If the layout stores variable length data within the chunk this callback must be defined.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *
 * NOTE: [chunk] is the pointer to the chunk intermediate struct
 * NOTE: This routine is modified from H5D__scatgath_write()
 * NOTE: [udata] not used??
 * NOTE: Not sure about my tracking of [nbytes], [alloc_size], and [alloc_size_total]
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__struct_chunk_gather_mem(H5D_dset_io_info_t *dset_info, H5D_io_type_info_t *io_type_info,
                             const H5S_t *mem_space, const H5S_t *file_space, size_t *nbytes /*in,out*/,
                             size_t *alloc_size /*in,out*/, size_t *alloc_size_total /*in,out*/, void *chunk,
                             void H5_ATTR_UNUSED *udata)
{

    const void             *buf;                    /* Local pointer to application buffer */
    void                   *tmp_buf;                /* Buffer to use for type conversion */
    H5S_sel_iter_t         *file_iter      = NULL;  /* Memory selection iteration info*/
    bool                    file_iter_init = false; /* Memory selection iteration info has been initialized */
    H5S_sel_iter_t         *mem_iter       = NULL;  /* Memory selection iteration info*/
    bool                    mem_iter_init  = false; /* Memory selection iteration info has been initialized */
    H5S_sel_iter_t         *bkg_iter       = NULL;  /* Memory selection iteration info*/
    H5S_sel_iter_t         *sel_iter       = NULL;  /* Memory selection iteration info*/
    bool                    sel_iter_init  = false; /* Memory selection iteration info has been initialized */
    bool                    bkg_iter_init  = false; /* Memory selection iteration info has been initialized */
    hsize_t                 smine_start;            /* Strip mine start loc	*/
    size_t                  smine_nelmts;           /* Elements per strip	*/
    hsize_t                 nelmts; /* Number of elements selected in file & memory dataspaces */
    size_t                  mem_type_size;
    size_t                  file_type_size;
    size_t                  buf_off = 0;    /* Buffer offset for in-place type conversion */
    bool                    in_place_tconv = false; /* Whether to perform in-place type_conversion */
    H5D_chunk_cache_mem_t  *chk = (H5D_chunk_cache_mem_t *)chunk; /* Chunk's memory cache info */
    void                   *data_scat_buf;
    hsize_t                 scat_buf_size;
    H5_flexible_const_ptr_t flex_mspace;
    H5_flexible_const_ptr_t flex_fspace;
    herr_t                  ret_value = SUCCEED; /* Return value		*/

    FUNC_ENTER_PACKAGE

    /* Sanity check */
    assert(dset_info);
    assert(io_type_info);
    assert(dset_info->mem_space);
    assert(dset_info->file_space);
    assert(dset_info->buf.cvp);
    assert(chk);

    /* Make certain that the number of elements in each selection is the same */
    nelmts = H5S_GET_SELECT_NPOINTS(mem_space);
    if (nelmts != H5S_GET_SELECT_NPOINTS(file_space))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
                    "src and dest dataspaces have different number of elements selected");

    /* Check for NOOP write */
    if (nelmts == 0)
        HGOTO_DONE(SUCCEED);

    mem_type_size  = dset_info->type_info.src_type_size;
    file_type_size = dset_info->type_info.dst_type_size;

    flex_mspace.cvp = mem_space;
    flex_fspace.cvp = file_space;

    /* Set buf pointer (memory buffer in dset_info) it's the application buffer */
    buf = dset_info->buf.cvp;

    /*
     * Scatter dato in chk->data_buf to data_scat_buf according to chk->sel_space
     */
    {
        hsize_t sel_nelmts;

        /* Allocate the data_scat_buf: chunk size * element size */
        scat_buf_size = dset_info->layout->u.struct_chunk.size;
        if (NULL == (data_scat_buf = H5FL_BLK_MALLOC(scat_buf, scat_buf_size)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL,
                        "memory allocation failed for scattered data buffer");
        memset(data_scat_buf, 0, scat_buf_size);

        if (chk->sel_space != NULL) {
            /* Get the number of elements in the selection */
            sel_nelmts = H5S_GET_SELECT_NPOINTS(chk->sel_space);

            /* Initialize the iterator */
            if (NULL == (sel_iter = H5FL_MALLOC(H5S_sel_iter_t)))
                HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "can't allocate selection iterator");

            if (H5S_select_iter_init(sel_iter, chk->sel_space, file_type_size,
                                     H5S_SEL_ITER_GET_SEQ_LIST_SORTED) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL,
                            "unable to initialize selection iter information");
            sel_iter_init = true;

            /* Scatter data */
            if (H5D__scatter_mem(chk->data_buf, sel_iter, sel_nelmts, data_scat_buf /*out*/) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "mem scatter failed");

            if (sel_iter_init && H5S_SELECT_ITER_RELEASE(sel_iter) < 0)
                HDONE_ERROR(H5E_DATASET, H5E_CANTFREE, FAIL, "Can't release selection iterator");
            if (sel_iter)
                sel_iter = H5FL_FREE(H5S_sel_iter_t, sel_iter);
        }
    }

    /*
     * If there is no data transform or type conversion then write directly
     * into the chunk buffer.
     */
    if (dset_info->type_info.is_xform_noop && dset_info->type_info.is_conv_noop) {

        if (H5D_select_io_mem(data_scat_buf, flex_fspace.vp, buf, flex_mspace.vp, file_type_size, nelmts))
            HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "couldn't copy chunk data to write buffer");
    }
    else { /* with type conversion */

        /* Check for in-place type conversion */
        if (io_type_info->may_use_in_place_tconv) {

            /* Make sure the memory type is not smaller than the file type, otherwise the memory buffer
               won't be big enough to serve as the type conversion buffer */
            if (mem_type_size >= file_type_size) {
                bool    is_contig;
                hsize_t sel_off;

                /* Check if the space is contiguous */
                if (H5S_select_contig_block(flex_mspace.vp, &is_contig, &sel_off, NULL) < 0)
                    HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "can't check if dataspace is contiguous");

                /* If the first sequence includes all the elements selected in this piece, it it contiguous */
                if (is_contig) {
                    H5_CHECK_OVERFLOW(sel_off, hsize_t, size_t);
                    in_place_tconv = true;
                    buf_off        = (size_t)sel_off * mem_type_size;
                }
            }
        }

        /* Check if we should disable in-place type conversion for performance.  Do so if we can use the
         * optimized compound write function, and either entire I/O operation can fit in the type conversion
         * buffer or we need to use a background buffer (and therefore could not do the I/O in one operation
         * with in-place conversion * anyways). */
        if (in_place_tconv && H5D__SCATGATH_USE_CMPD_OPT_WRITE(dset_info, false) &&
            (dset_info->type_info.need_bkg || (nelmts <= dset_info->type_info.request_nelmts)))
            in_place_tconv = false;

        /* Allocate the iterators */
        if (NULL == (mem_iter = H5FL_MALLOC(H5S_sel_iter_t)))
            HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "can't allocate memory iterator");

        if (NULL == (file_iter = H5FL_MALLOC(H5S_sel_iter_t)))
            HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "can't allocate file iterator");

        if (NULL == (bkg_iter = H5FL_MALLOC(H5S_sel_iter_t)))
            HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "can't allocate file iterator");

        /* Figure out the strip mine size. */
        if (H5S_select_iter_init(mem_iter, flex_mspace.vp, dset_info->type_info.src_type_size, 0) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to initialize memory selection information");
        mem_iter_init = true; /*file selection iteration info has been initialized */

        if (H5S_select_iter_init(file_iter, flex_fspace.vp, file_type_size,
                                 H5S_SEL_ITER_GET_SEQ_LIST_SORTED) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL,
                        "unable to initialize background selection information");
        file_iter_init = true; /*file selection iteration info has been initialized */

        if (H5S_select_iter_init(bkg_iter, flex_fspace.vp, file_type_size, H5S_SEL_ITER_GET_SEQ_LIST_SORTED) <
            0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL,
                        "unable to initialize background selection information");
        bkg_iter_init = true; /*file selection iteration info has been initialized */

        /* Start strip mining... */
        for (smine_start = 0; smine_start < nelmts; smine_start += smine_nelmts) {
            size_t n; /* Elements operated on */

            /* Determine strip mine size. First check if we're doing in-place type conversion */
            if (in_place_tconv) {
                /* If there is a background buffer, we cannot exceed request_nelmts.  */
                assert(!H5D__SCATGATH_USE_CMPD_OPT_WRITE(dset_info, in_place_tconv));
                if (dset_info->type_info.need_bkg)
                    smine_nelmts = (size_t)MIN(dset_info->type_info.request_nelmts, (nelmts - smine_start));
                else {
                    assert(smine_start == 0);
                    smine_nelmts = nelmts;
                }

                /* Calculate buffer position in user buffer */
                /* Use "vp" field of union to twiddle away const.  OK because if we're doing this it means the
                 * user explicitly allowed us to modify this buffer via H5Pset_modify_write_buf(). */
                tmp_buf = (uint8_t *)dset_info->buf.vp + buf_off + (smine_start * mem_type_size);
            }
            else {
                /* Do type conversion using intermediate buffer */
                tmp_buf = io_type_info->tconv_buf;

                /* Go figure out how many elements to read from the file */
                smine_nelmts = (size_t)MIN(dset_info->type_info.request_nelmts, (nelmts - smine_start));

                /*
                 * Gather data from application buffer into the datatype conversion
                 * buffer. Also gather data from the file into the background buffer
                 * if necessary.
                 */
                n = H5D__gather_mem(buf, mem_iter, smine_nelmts, tmp_buf /*out*/);
                if (n != smine_nelmts)
                    HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "mem gather failed");
            }

            /* If the source and destination are compound types and the destination is
             * is a subset of the source and no conversion is needed, copy the data
             * directly from user's buffer and bypass the rest of steps.  If the source
             * is a subset of the destination, the optimization is done in conversion
             * function H5T_conv_struct_opt to protect the background data.
             */
            if (H5D__SCATGATH_USE_CMPD_OPT_WRITE(dset_info, in_place_tconv)) {
                if (H5D__compound_opt_write(smine_nelmts, &dset_info->type_info, tmp_buf) < 0)
                    HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "datatype conversion failed");

            } /* end if */
            else {
                if (H5T_BKG_YES == dset_info->type_info.need_bkg) {
                    n = H5D__gather_mem(data_scat_buf, bkg_iter, smine_nelmts, io_type_info->bkg_buf /*out*/);
                    if (n != smine_nelmts)
                        HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, "file gather failed");
                } /* end if */

                /* Do the data transform before the type conversion (since
                 * transforms must be done in the memory type). */
                if (!dset_info->type_info.is_xform_noop) {
                    H5Z_data_xform_t *data_transform; /* Data transform info */

                    /* Retrieve info from API context */
                    if (H5CX_get_data_transform(&data_transform) < 0)
                        HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't get data transform info");

                    if (H5Z_xform_eval(data_transform, tmp_buf, smine_nelmts, dset_info->type_info.mem_type) <
                        0)
                        HGOTO_ERROR(H5E_DATASET, H5E_BADVALUE, FAIL, "Error performing data transform");
                }

                /*
                 * Perform datatype conversion.
                 */
                if (H5T_convert(dset_info->type_info.tpath, dset_info->type_info.src_type,
                                dset_info->type_info.dst_type, smine_nelmts, (size_t)0, (size_t)0, tmp_buf,
                                io_type_info->bkg_buf) < 0)
                    HGOTO_ERROR(H5E_DATASET, H5E_CANTCONVERT, FAIL, "datatype conversion failed");
            } /* end else */

            /*
             * Scatter the data out to the data_scat_buffer.
             */
            if (H5D__scatter_mem(tmp_buf, file_iter, smine_nelmts, data_scat_buf /*out*/) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "scatter failed");

        } /* end for */

    } /* end if */

    /*
     *  Gather data in data_scat_buf to chk->data_buf according to chk->sel_space
     */
    {
        H5S_t  *sel_space;
        hsize_t sel_nelmts;
        hsize_t n;

        /* Combine selections */
        if (chk->sel_space) {
            if (NULL == (sel_space = H5S__combine_select(chk->sel_space, H5S_SELECT_OR, flex_fspace.vp)))
                HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to get dataspace");
            if (H5S_close(chk->sel_space) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_CANTRELEASE, FAIL, "can't release dataspace");

            if (NULL == (chk->sel_space = H5S_copy(sel_space, false, true)))
                HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to get dataspace");
            if (H5S_close(sel_space) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_CANTRELEASE, FAIL, "can't release dataspace");
        }
        else {
            if (NULL == (chk->sel_space = H5S_copy(file_space, false, true)))
                HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to get dataspace");
        }

        /* Get the number of elements in the selection */
        sel_nelmts = H5S_GET_SELECT_NPOINTS(chk->sel_space);

        /* Initialize the iterator */
        if (NULL == (sel_iter = H5FL_MALLOC(H5S_sel_iter_t)))
            HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "can't allocate selection iterator");

        if (H5S_select_iter_init(sel_iter, chk->sel_space, file_type_size, H5S_SEL_ITER_GET_SEQ_LIST_SORTED) <
            0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to initialize selection iter information");
        sel_iter_init = true;

        /* Re-allocate the chk->data_buf */
        chk->data_nbytes     = sel_nelmts * file_type_size;
        chk->data_alloc_size = chk->data_nbytes;

        if (NULL == (chk->data_buf = H5MM_realloc(chk->data_buf, chk->data_alloc_size)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for the chunk");

        /* Gather elements to chk->data_buf */
        n = H5D__gather_mem(data_scat_buf, sel_iter, sel_nelmts, chk->data_buf /*out*/);
        if (n != sel_nelmts)
            HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "mem gather failed");

        /* Free the iterator */
        if (sel_iter_init && H5S_SELECT_ITER_RELEASE(sel_iter) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CANTFREE, FAIL, "Can't release selection iterator");
        if (sel_iter)
            sel_iter = H5FL_FREE(H5S_sel_iter_t, sel_iter);

        /* Free the buffer */
        if (data_scat_buf)
            data_scat_buf = H5FL_BLK_FREE(scat_buf, data_scat_buf);
    }

    *nbytes += chk->sel_nbytes + chk->data_nbytes;
    *alloc_size += chk->sel_alloc_size + chk->data_alloc_size;
    *alloc_size_total += (*nbytes + *alloc_size);

done:
    /* Release selection iterators */
    if (mem_iter_init && H5S_SELECT_ITER_RELEASE(mem_iter) < 0)
        HDONE_ERROR(H5E_DATASET, H5E_CANTFREE, FAIL, "Can't release selection iterator");
    if (mem_iter)
        mem_iter = H5FL_FREE(H5S_sel_iter_t, mem_iter);

    if (file_iter_init && H5S_SELECT_ITER_RELEASE(file_iter) < 0)
        HDONE_ERROR(H5E_DATASET, H5E_CANTFREE, FAIL, "Can't release selection iterator");
    if (file_iter)
        file_iter = H5FL_FREE(H5S_sel_iter_t, file_iter);

    if (bkg_iter_init && H5S_SELECT_ITER_RELEASE(bkg_iter) < 0)
        HDONE_ERROR(H5E_DATASET, H5E_CANTFREE, FAIL, "Can't release selection iterator");
    if (bkg_iter)
        bkg_iter = H5FL_FREE(H5S_sel_iter_t, bkg_iter);

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5D__struct_chunk_gather_mem() */

/*-------------------------------------------------------------------------
 * Function:    H5D__struct_chunk_fill
 *
 * Purpose:     Propagates the fill value into the selected elements of the chunk buffer,
 *              performing type conversion if necessary.
 *
 *              space's extent matches the chunk dimensions and the selection is
 *              within the chunk.
 *
 *              Optional, if not present, chunk is the same in memory as it is in cache,
 *              with the exception of type conversion (which will be handled
 *              by H5SC layer).
 *
 *              If the layout stores variable length data within the chunk
 *              this callback must be defined.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 * NOTE: chunk is pointer to the chunk intermediate struct
 *
 * NOTE: [io_type_info] not used??
 * NOTE: [udata] not used??
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__struct_chunk_fill(H5D_dset_io_info_t *dset_info, H5D_io_type_info_t H5_ATTR_UNUSED *io_type_info,
                       H5S_t *space, size_t *nbytes /*in,out*/, size_t *alloc_size /*in,out*/,
                       size_t *alloc_size_total /*in,out*/, void *chunk, void H5_ATTR_UNUSED *udata)
{
    const H5O_fill_t      *fill = &(dset_info->dset->shared->dcpl_cache.fill); /* Fill value info */
    H5D_chunk_cache_mem_t *chk  = (H5D_chunk_cache_mem_t *)chunk;              /* Chunk's memory cache info */
    uint8_t                elmt_buf[H5T_ELEM_BUF_SIZE];                        /* Buffer for element data */
    uint8_t                bkg_elmt_buf[H5T_ELEM_BUF_SIZE]; /* Buffer for background data */
    size_t                 buf_size;
    size_t                 src_type_size;
    size_t                 dst_type_size;
    size_t                 tot_buf_size;
    htri_t                 has_vlen_type;
    hsize_t                nelmts;
    herr_t                 ret_value = SUCCEED; /* Return value		*/

    FUNC_ENTER_PACKAGE

    /* Sanity check */
    /* Check args */
    assert(space);

    src_type_size = dset_info->type_info.src_type_size;
    dst_type_size = dset_info->type_info.dst_type_size;

    buf_size = MAX(src_type_size, dst_type_size);

    nelmts = H5S_GET_SELECT_NPOINTS(space);
    H5_CHECK_OVERFLOW(nelmts, hsize_t, size_t);

    tot_buf_size = nelmts * buf_size;

    if (NULL == (chk->data_buf = H5MM_realloc(chk->data_buf, tot_buf_size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory reallocation failed for data buffer");

    /* Detect whether the datatype has a VL component */
    if ((has_vlen_type = H5T_detect_class(dset_info->type_info.src_type, H5T_VLEN, false)) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_BADVALUE, FAIL, "unable to detect vlen datatypes?");

    if (fill->buf == NULL)
        memset(chk->data_buf, 0, tot_buf_size);
    else if (!has_vlen_type) { /* has fill value && not handling VL type yet */

        void *elmt_ptr = elmt_buf;     /* Pointer to element to use for fill value */
        void *bkg_ptr  = bkg_elmt_buf; /* Pointer to element to use for fill value */

        /* Copy the fill value to the buffer for conversion */
        H5MM_memcpy(elmt_ptr, fill->buf, buf_size);

        /* Perform datatype conversion */
        if (H5T_convert(dset_info->type_info.tpath, dset_info->type_info.src_type,
                        dset_info->type_info.dst_type, (size_t)1, (size_t)0, (size_t)0, elmt_ptr,
                        bkg_ptr) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTCONVERT, FAIL, "data type conversion failed");

        /* Replicate the fill value into the chunk buffer */
        H5VM_array_fill(chk->data_buf, elmt_ptr, buf_size, (size_t)nelmts);
    }

    if (chk->sel_space) {
        if (H5S_close(chk->sel_space) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTRELEASE, FAIL, "can't release dataspace");
    }

    if (NULL == (chk->sel_space = H5S_copy(space, false, true)))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to get dataspace");

    chk->data_nbytes = chk->data_alloc_size = tot_buf_size;

    /* Return values */
    *nbytes += chk->data_nbytes;
    *alloc_size += chk->data_alloc_size;
    *alloc_size_total += *alloc_size;

done:
    FUNC_LEAVE_NOAPI(ret_value)

} /* H5D__struct_chunk_fill() */

/*-------------------------------------------------------------------------
 * Function:    H5D__struct_chunk_defined values
 *
 * Purpose:     Queries the defined elements in the chunk.
 *              Selection may be passed as H5S_ALL.
 *              These selections are within the logical chunk.
 *
 *              Optional, if not present, all values are defined.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 * NOTE: [chunk] is pointer to the chunk intermediate struct
 *
 * NOTE: [udata] not used??
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__struct_chunk_defined_values(H5D_t *dset, const H5S_t *selection, void *chunk,
                                 H5S_t **defined_values /*out*/, void H5_ATTR_UNUSED *udata)
{

    H5D_chunk_cache_mem_t  *chk = (H5D_chunk_cache_mem_t *)chunk; /* Chunk's memory cache info */
    H5_flexible_const_ptr_t flex_sel;
    herr_t                  ret_value = SUCCEED; /* Return value		*/

    FUNC_ENTER_PACKAGE

    /* Sanity check */
    assert(dset);

    flex_sel.cvp = selection;

    if (H5S_GET_SELECT_TYPE(selection) == H5S_ALL) {
        if (NULL == (*defined_values = H5S_copy(chk->sel_space, false, true)))
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to get dataspace");
    }
    else if (H5S_GET_SELECT_TYPE(selection) == H5S_SEL_HYPERSLABS)

        *defined_values = H5S__combine_select(flex_sel.vp, H5S_SELECT_AND, chk->sel_space);
    else
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, H5I_INVALID_HID, "dataspaces don't have hyperslab selections");

done:

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5D__struct_chunk_defined_values() */

/*-------------------------------------------------------------------------
 * Function:    H5D__struct_chunk_erase_values
 *
 * Purpose:     Erases the selected elements in the chunk, causing them to
 *              no longer be defined. If all values in the chunk are erased and
 *              the chunk should be deleted, sets *delete_chunk to true,
 *              causing the cache to delete the chunk from cache, free it in memory
 *              using H5SC_chunk_evict_t, and delete it on disk using H5SC_chunk_delete_t.
 *              These selections are within the logical chunk.
 *
 *              Optional, if not present, the fill value will be written to the selection
 *              using H5SC_chunk_fill_t.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * NOTE: chunk is pointer to the chunk intermediate struct
 *
 * NOTE: [udata] not used?? alloc_size not used??
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__struct_chunk_erase_values(H5D_t *dset, const H5S_t *selection, size_t *nbytes /*in,out*/,
                               size_t H5_ATTR_UNUSED *alloc_size /*in,out*/, void *chunk,
                               bool *delete_chunk /*out*/, void H5_ATTR_UNUSED *udata)
{
    H5D_chunk_cache_mem_t  *chk = (H5D_chunk_cache_mem_t *)chunk; /* Chunk memory cache info */
    void                   *buf = chk->data_buf;
    H5S_t                  *serial_values_space = NULL;
    H5S_t                  *serial_erase_space  = NULL;
    hsize_t                 chk_nelmts;
    hsize_t                 erase_nelmts;
    H5_flexible_const_ptr_t flex_selection;

    H5S_sel_iter_t *erase_iter      = NULL;  /* Erase selection iteration info*/
    bool            erase_iter_init = false; /* Erase selection iteration info has been initialized */
    size_t          elmt_size;
    size_t          dxpl_vec_size; /* Vector length from API context's DXPL */
    size_t          vec_size;      /* Vector length */
    size_t         *len = NULL;    /* Pointer to sequence lengths */
    hsize_t        *off = NULL;    /* Pointer to sequence offsets */
    size_t          curr_len;      /* Length of bytes left to process in sequence */
    hsize_t         curr_off;      /* Length of bytes left to process in sequence */
    size_t          nseq;          /* Number of sequences generated */
    size_t          curr_seq;      /* Current sequence being processed */
    size_t          nelem;         /* Number of elements used in sequences */
    hssize_t        hss_nelmts;
    size_t          new_nelmts;
    hsize_t         dst_off;
    hsize_t         persist_off;
    hsize_t         prev_persist_off;
    hsize_t         persist_end_off;
    hsize_t         prev_persist_end_off;
    hsize_t         tmp_off;
    hsize_t         num_bytes;
    hsize_t         tot_erased_bytes = 0;
    H5S_t          *new_space;

    herr_t ret_value = SUCCEED; /* Return value		*/

    FUNC_ENTER_PACKAGE

    /* Sanity check */
    assert(dset);

    /* Get the number of elements in chk->sel_space */
    if ((hss_nelmts = (hssize_t)H5S_GET_SELECT_NPOINTS(chk->sel_space)) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_CANTCOUNT, FAIL, "can't get number of elements selected");
    H5_CHECKED_ASSIGN(chk_nelmts, size_t, hss_nelmts, hssize_t);

    /* Get the number of elements in selection */
    if ((hss_nelmts = (hssize_t)H5S_GET_SELECT_NPOINTS(selection)) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_CANTCOUNT, FAIL, "can't get number of elements selected");
    H5_CHECKED_ASSIGN(erase_nelmts, size_t, hss_nelmts, hssize_t);

    if (chk_nelmts == erase_nelmts) {
        *delete_chunk = true;
        HGOTO_DONE(SUCCEED);
    }

    if (NULL == (serial_values_space = H5S_create_simple(1, &chk_nelmts, NULL)))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTCREATE, FAIL, "unable to create simple memory dataspace");

    flex_selection.cvp = selection;
    if (H5S_select_project_intersection(chk->sel_space, serial_values_space, flex_selection.vp,
                                        &serial_erase_space, true) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTCLIP, FAIL,
                    "can't project the intersection of erased space and src_space");

    if (0 == (elmt_size = H5T_get_size(dset->shared->type)))
        HGOTO_ERROR(H5E_DATATYPE, H5E_BADSIZE, FAIL, "datatype size invalid");

    if (NULL == (erase_iter = H5FL_MALLOC(H5S_sel_iter_t)))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "can't allocate file iterator");

    if (H5S_select_iter_init(erase_iter, serial_erase_space, elmt_size, H5S_SEL_ITER_GET_SEQ_LIST_SORTED) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to initialize file selection information");
    erase_iter_init = true; /*erase selection iteration info has been initialized */

    /* Get info from API context */
    if (H5CX_get_vec_size(&dxpl_vec_size) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't retrieve I/O vector size");

    /* Allocate the vector I/O arrays */
    if (dxpl_vec_size > H5D_IO_VECTOR_SIZE)
        vec_size = dxpl_vec_size;
    else
        vec_size = H5D_IO_VECTOR_SIZE;

    if (NULL == (len = H5FL_SEQ_MALLOC(size_t, vec_size)))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "can't allocate I/O length vector array");
    if (NULL == (off = H5FL_SEQ_MALLOC(hsize_t, vec_size)))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "can't allocate I/O offset vector array");

    /* Loop until all elements are erased */
    while (erase_nelmts > 0) {
        /* Get list of sequences for selection to erase */
        if (H5S_SELECT_ITER_GET_SEQ_LIST(erase_iter, vec_size, erase_nelmts, &nseq, &nelem, off, len) < 0)
            HGOTO_ERROR(H5E_INTERNAL, H5E_UNSUPPORTED, 0, "sequence length generation failed");

        /* Loop, while sequences left to process */
        for (curr_seq = 0; curr_seq < nseq; curr_seq++) {

            /* Get the number of bytes in sequence */
            curr_len = len[curr_seq];
            curr_off = off[curr_seq];

            /* Move down remaining elements (if any) beyond tot_erased_bytes but before the next erased block
             */
            if (tot_erased_bytes != 0 && (prev_persist_end_off < curr_off)) {
                num_bytes = curr_off - prev_persist_end_off;
                memmove((uint8_t *)buf + prev_persist_off, (uint8_t *)buf + prev_persist_end_off, num_bytes);
            }

            tot_erased_bytes += curr_len;

            persist_off      = curr_off + curr_len;
            prev_persist_off = persist_off;

            dst_off = persist_off - tot_erased_bytes;

            persist_end_off      = persist_off + tot_erased_bytes;
            prev_persist_end_off = persist_end_off;

            /* Move down each block of elements not erased to the previous block of elements not erased */
            if (tot_erased_bytes != 0 && persist_off < chk_nelmts) {
                memmove((uint8_t *)buf + dst_off, (uint8_t *)buf + persist_off, tot_erased_bytes);
            }
            else
                memset((uint8_t *)buf + dst_off, 0, tot_erased_bytes);
        }

        /* Decrement number of elements left to process */
        erase_nelmts -= nelem;
    }

    /* Move down the last block of elements after the last erased block in the buffer */
    if (tot_erased_bytes != 0 && (persist_end_off < chk_nelmts))
        memmove((uint8_t *)buf + persist_off, (uint8_t *)buf + persist_end_off, tot_erased_bytes);

    /* Take care of the last block of elements beyond tot_erased_bytes in the buffer */
    tmp_off = persist_end_off + tot_erased_bytes;
    if (tmp_off < chk_nelmts) {
        num_bytes = chk_nelmts - tmp_off;
        memmove((uint8_t *)buf + persist_end_off, (uint8_t *)buf + tmp_off, num_bytes);

        tmp_off = persist_end_off + num_bytes;
        if (tmp_off < chk_nelmts) {
            num_bytes = chk_nelmts - tmp_off;
            memset((uint8_t *)buf + tmp_off, 0, num_bytes);
        }
    }

    chk->data_nbytes -= tot_erased_bytes;

    /* Create dataspace without the erased elements */
    if (NULL == (new_space = H5S__combine_select(chk->sel_space, H5S_SELECT_NOTB, flex_selection.vp)))
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTINIT, FAIL, "unable to create hyperslab selection");

    new_nelmts = H5S_GET_SELECT_NPOINTS(new_space);
    assert(chk->data_nbytes == (new_nelmts * elmt_size));

    if (H5S_close(chk->sel_space) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTRELEASE, FAIL, "can't release dataspace");
    chk->sel_space = new_space;

    *nbytes = chk->data_nbytes;

done:
    if (erase_iter_init && H5S_SELECT_ITER_RELEASE(erase_iter) < 0)
        HDONE_ERROR(H5E_DATASET, H5E_CANTFREE, FAIL, "Can't release selection iterator");
    if (erase_iter)
        erase_iter = H5FL_FREE(H5S_sel_iter_t, erase_iter);

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5D__struct_chunk_erase_values() */

/*-------------------------------------------------------------------------
 * Function:    H5D__struct_chunk_evict_values
 *
 * Purpose:     Frees the data values in the cached chunk and memory used by them
 *              (but does not reallocate - see H5SC_chunk_condense_t),
 *              but leaves the defined values intact.
 *
 *              Optional, if not present the entire chunk will be evicted.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * NOTE: chunk is pointer to the chunk intermediate struct
 *
 * NOTE: [udata] not used??
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__struct_chunk_evict_values(H5D_t *dset, size_t *nbytes /*in,out*/, size_t *alloc_size /*in,out*/,
                               void *chunk, void H5_ATTR_UNUSED *udata)
{
    H5D_chunk_cache_mem_t *chk = (H5D_chunk_cache_mem_t *)chunk; /* Chunk memory cache info */

    FUNC_ENTER_PACKAGE_NOERR

    /* Sanity check */
    assert(dset);

    chk->data_buf = H5MM_xfree(chk->data_buf);

    chk->data_nbytes     = 0;
    chk->data_alloc_size = 0;

    *nbytes -= chk->data_nbytes;
    *alloc_size -= chk->data_alloc_size;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5D__struct_chunk_evict_values() */

/*-------------------------------------------------------------------------
 * Function:    H5D__struct_chunk_layout_query
 *
 * Purpose:     Queries data about the dataset from the layout client.
 *              The callback shall set the chunk dimensions in the chunk_dims array
 *              (the number of dimensions is the same as the rank of the dataset),
 *              whether encoding and decoding is necessary for chunks between cache
 *              and disk, and shall set whether chunks that are partially outside the bounds
 *              of the dataset are encoded differently (for example, they may not have
 *              filters applied).
 *              If *partial_bound_chunks_different_encoding is set to true,
 *              then chunks whose partial bound state changes will be re-encoded and
 *              re-inserted as necessary after the dataset extent changes to ensure
 *              they are encoded appropriately.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__struct_chunk_layout_query(H5D_t *dset, hsize_t *chunk_dims, bool *encode_decode_necessary,
                               bool *partial_bound_chunks_different_encoding)
{
    herr_t ret_value = SUCCEED; /* Return value		*/

    FUNC_ENTER_PACKAGE

    /* Sanity check */
    assert(dset);

    /* Check for invalid chunk dimension rank */
    if (0 == dset->shared->layout.u.struct_chunk.ndims)
        HGOTO_ERROR(H5E_DATASET, H5E_BADVALUE, FAIL, "no chunk information set?");
    if ((dset->shared->layout.u.struct_chunk.ndims - 1) != dset->shared->ndims)
        HGOTO_ERROR(H5E_DATASET, H5E_BADVALUE, FAIL, "dimensionality of chunks doesn't match the dataspace");

    if (chunk_dims) {
        /* Get the chunk dimension sizes */
        for (unsigned u = 0; u < (dset->shared->layout.u.struct_chunk.ndims - 1); u++)
            chunk_dims[u] = dset->shared->layout.u.struct_chunk.dim[u];
    }

    /* For structured chunk:
         encoding and decoding is necessary for chunks between cache and disk */

    if (encode_decode_necessary)
        *encode_decode_necessary = true;

    if (partial_bound_chunks_different_encoding)
        *partial_bound_chunks_different_encoding =
            (dset->shared->layout.u.struct_chunk.flags & H5O_LAYOUT_CHUNK_DONT_FILTER_PARTIAL_BOUND_CHUNKS);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5D__struct_chunk_layout_query() */

/*-------------------------------------------------------------------------
 * Function:    H5D__struct_chunk_delete_chunk
 *
 * Purpose:     Removes the chunk from the index and deletes it on disk.
 *
 *              Only called if a chunk goes out of scope due to H5Dset_extent() or
 *              of H5SC_chunk_erase_values_t returns *delete_chunk == true.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__struct_chunk_delete_chunk(H5D_t *dset, const hsize_t *scaled /*in*/, haddr_t addr, hsize_t disk_size)
{
    H5D_chunk_ud_t              udata;
    H5D_chunk_common_ud_t       idx_udata; /* User data for index removal routine */
    H5O_storage_struct_chunk_t *storage = &(dset->shared->layout.storage.u.struct_chunk);
    H5D_chk_idx_info_t          idx_info;            /* Chunked index info */
    herr_t                      ret_value = SUCCEED; /* Return value		*/

    FUNC_ENTER_PACKAGE

    /* Sanity check */
    assert(dset);

    /* Compose chunked index info struct */
    idx_info.f           = dset->oloc.file;
    idx_info.pline       = &dset->shared->dcpl_cache.pline;
    idx_info.stc_layout  = &dset->shared->layout.u.struct_chunk;
    idx_info.stc_storage = &dset->shared->layout.storage.u.struct_chunk;

    /* Set up udata */
    udata.common.stc_layout  = idx_info.stc_layout;
    udata.common.stc_storage = idx_info.stc_storage;
    udata.common.scaled      = scaled;

    /* Reset information about the chunk we are looking for */
    udata.chunk_block.offset = HADDR_UNDEF;
    udata.chunk_block.length = 0;

    /* chunk_idx is calculated in get_addr callback */
    if ((storage->ops->get_addr)(&idx_info, &udata) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't query chunk address");

    /* Remove the chunk from disk, if present */
    if (H5_addr_defined(udata.chunk_block.offset) && H5_addr_eq(addr, udata.chunk_block.offset) &&
        udata.chunk_block.length == disk_size) {

        /* Chunk index is calculated in idx_remove() based on scaled */
        idx_udata.scaled = udata.common.scaled;

        /* Remove the chunk from disk and index */
        if ((storage->ops->remove)(&idx_info, &idx_udata) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTDELETE, FAIL, "unable to remove chunk entry from index");
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5D__struct_chunk_delete_chunk() */
