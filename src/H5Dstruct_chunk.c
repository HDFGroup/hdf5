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
#define H5SC_FRIEND     /*suppress error about including H5SCpkg    */


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

/* Sanity check on chunk index types */
#define H5D_STRUCT_CHUNK_STORAGE_INDEX_CHK(storage)                                                         \
    do {                                                                                                    \
        assert((H5D_CHUNK_IDX_EARRAY == (storage)->idx_type && H5D_COPS_EARRAY == (storage)->ops) ||        \
               (H5D_CHUNK_IDX_FARRAY == (storage)->idx_type && H5D_COPS_STRUCT_CHUNK_FARRAY == (storage)->ops) ||        \
               (H5D_CHUNK_IDX_BT2 == (storage)->idx_type && H5D_COPS_BT2 == (storage)->ops) ||              \
               (H5D_CHUNK_IDX_SINGLE == (storage)->idx_type && H5D_COPS_SINGLE == (storage)->ops));         \
    } while (0)

/******************/
/* Local Typedefs */
/******************/


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
static herr_t H5D__struct_chunk_may_use_select_io(H5D_io_info_t *io_info, const H5D_dset_io_info_t *dset_info);
static herr_t H5D__struct_chunk_io_init_selections(H5D_io_info_t *io_info, H5D_dset_io_info_t *dinfo);
static herr_t H5D__struct_chunk_set_info_real(H5O_layout_struct_chunk_t *layout, unsigned ndims, const hsize_t *curr_dims,
                         const hsize_t *max_dims);
static herr_t H5D__struct_chunk_set_info(const H5D_t *dset);
static herr_t H5D__struct_chunk_set_sizes(H5D_t *dset);

/*
 *  Shared chunk cache layout callbacks for structured chunk
 *   
 */
static herr_t H5D__struct_chunk_lookup(H5D_t *dset, size_t count, const hsize_t *scaled[] /*in*/,
    haddr_t *addr[] /*out*/, hsize_t *size[] /*out*/, hsize_t *defined_values_size[] /*out*/,
    size_t *size_hint[] /*out*/, size_t *defined_values_size_hint[] /*out*/,
    void **udata[] /*out*/);



/*********************/
/* Package Variables */
/*********************/
/* Layout I/O callbacks for structured chunk */
const H5D_layout_ops_t H5D_LOPS_STRUCT_CHUNK[1] = {{
    H5D__struct_chunk_construct,        /* construct */
    H5D__struct_chunk_init,             /* init */
    H5D__struct_chunk_is_space_alloc,   /* is_space_alloc */
    NULL,           /* is_data_cached */
    H5D__struct_chunk_io_init,          /* io_init */
    H5D__struct_chunk_mdio_init,        /* mdio_init */
    NULL,           /* ser_read */
    NULL,           /* ser_write */
    NULL,           /* readvv */
    NULL,           /* writevv */
    NULL,           /* flush */
    H5D__struct_chunk_io_term,          /* io_term */
    H5D__struct_chunk_dest              /* dest */
}};

/* Shared Chunk Cache layout callbacks for structured chunked */
const H5SC_layout_ops_t H5SC_LOPS_STRUCT_CHUNK[1] = {{
    H5D__struct_chunk_lookup,   /* lookup */
    NULL,   /* decode */
    NULL,   /* decode_defined_values */
    NULL,   /* new_chunk */
    NULL,   /* condense */
    NULL,   /* encode */
    NULL,   /* evict */
    NULL,   /* encode_in_place */
    NULL,   /* insert */
    NULL,   /* selection_read */    
    NULL,   /* vector_read */
    NULL,   /* selection_write */
    NULL,   /* vector_write */
    NULL,   /* scatter_mem */
    NULL,   /* gather_mem */
    NULL,   /* fill */
    NULL,   /* defined_values */
    NULL,   /* erase_values */
    NULL,   /* evict_values */
    NULL,   /* layout_query */
    NULL    /* delete_chunk */
}};

/*******************/
/* Local Variables */
/*******************/

/* Declare extern free list to manage the H5S_sel_iter_t struct */
H5FL_EXTERN(H5S_sel_iter_t);

/* Declare extern free list to manage the H5D_piece_info_t struct */
H5FL_EXTERN(H5D_piece_info_t);

/* Declare extern free list to manage the H5D_chunk_info_t struct */
H5FL_DEFINE(H5D_chunk_map_t);

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
    H5D_chk_idx_info_t   idx_info; /* Chunked index info */
    H5O_storage_struct_chunk_t *store        = &(dset->shared->layout.storage.u.struct_chunk);
    herr_t               ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_PACKAGE

    /* Check args */
    assert(dset);
    assert(H5D_STRUCT_CHUNK == dset->shared->layout.type);
    assert(dset->shared->layout.u.struct_chunk.ndims > 0 && dset->shared->layout.u.struct_chunk.ndims <= H5O_LAYOUT_NDIMS);
    H5D_STRUCT_CHUNK_STORAGE_INDEX_CHK(store);

#ifndef NDEBUG
    {
        unsigned u; /* Local index variable */

        for (u = 0; u < dset->shared->layout.u.struct_chunk.ndims; u++)
            assert(dset->shared->layout.u.struct_chunk.dim[u] > 0);
    }
#endif

    /* Compose chunked index info struct */
    idx_info.f       = dset->oloc.file;
    idx_info.pline   = &dset->shared->dcpl_cache.pline;
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
    if (H5D__struct_chunk_set_info_real(&dset->shared->layout.u.struct_chunk, dset->shared->ndims, dset->shared->curr_dims,
                                 dset->shared->max_dims) < 0)
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
static herr_t
H5D__struct_chunk_set_sizes(H5D_t *dset)
{
    uint64_t chunk_size;            /* Size of chunk in bytes */
    unsigned max_enc_bytes_per_dim; /* Max. number of bytes required to encode this dimension */
    unsigned u;                     /* Iterator */
    htri_t has_vlen_type;
    herr_t ret_value = SUCCEED;     /* Return value */

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
    }  else { /* Fixed-size data */
        dset->shared->layout.storage.u.struct_chunk.nsects = 2;
        dset->shared->layout.storage.u.struct_chunk.nsects_md = 1;
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
    assert((unsigned)(dset->shared->layout.u.struct_chunk.ndims) <= NELMTS(dset->shared->layout.u.struct_chunk.dim));

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
    H5D_chk_idx_info_t idx_info;                            /* Chunked index info */
    H5O_storage_struct_chunk_t *storage        = &(dset->shared->layout.storage.u.struct_chunk);
    bool                 idx_init  = false;
    herr_t               ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_PACKAGE

    /* Sanity check */
    assert(f);
    assert(dset);
    H5D_STRUCT_CHUNK_STORAGE_INDEX_CHK(storage);

    /* Coding for raw data chunk cache for a dataset is removed */

    /* Compose chunked index info struct */
    idx_info.f       = f;
    idx_info.pline   = &dset->shared->dcpl_cache.pline;
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
    const H5O_storage_struct_chunk_t *storage        = &(store->u.struct_chunk);
    bool                       ret_value = false; /* Return value */

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
    H5SL_node_t      *piece_node;          /* Current node in chunk skip list */
    H5D_piece_info_t *piece_info;          /* Piece information for current piece */
    H5D_chunk_ud_t    **udata[1];               /* Chunk data from index */
    haddr_t *addr[1];
    const hsize_t *scaled[1];
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
        addr[0] = &piece_info->faddr;

        if (H5D__struct_chunk_lookup(dinfo->dset, 1, scaled, addr, NULL, NULL, NULL, NULL, (void ***)udata) < 0)
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
    H5D_chk_idx_info_t   idx_info;                            /* Chunked index info */
    H5O_storage_struct_chunk_t *storage        = &(dset->shared->layout.storage.u.struct_chunk);
    herr_t               ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_PACKAGE_TAG(dset->oloc.addr)

    /* Sanity checks */
    assert(dset);
    H5D_STRUCT_CHUNK_STORAGE_INDEX_CHK(storage);

    /* Compose chunked index info struct */
    idx_info.f       = dset->oloc.file;
    idx_info.pline   = &dset->shared->dcpl_cache.pline;
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
H5D__struct_chunk_lookup(H5D_t *dset, size_t count, const hsize_t *scaled[] /*in*/, 
    haddr_t *addr[] /*out*/, hsize_t *size[] /*out*/, hsize_t *defined_values_size[] /*out*/, 
    size_t *size_hint[] /*out*/, size_t *defined_values_size_hint[] /*out*/, 
    void **_udata[] /*out*/)
{
    H5D_chunk_ud_t *udata;
    H5O_storage_struct_chunk_t *storage  = &(dset->shared->layout.storage.u.struct_chunk);
    H5O_layout_struct_chunk_t *layout = &dset->shared->layout.u.struct_chunk;
    H5D_chk_idx_info_t idx_info;    /* Chunked index info */
    H5O_pline_t   *pline;           /* I/O pipeline info */
    hbool_t filtered = false;
    size_t tot_unfilt_size = 0;
    size_t i;
    herr_t ret_value = SUCCEED;     /* Return value */

    FUNC_ENTER_PACKAGE

    /* Sanity checks */
    assert(dset);
    assert(dset->shared->layout.type == H5D_STRUCT_CHUNK);

    pline = &(dset->shared->dcpl_cache.pline);
    if (pline && pline->nused)
        filtered = true;

    /* Compose chunked index info struct */
    idx_info.f       = dset->oloc.file;
    idx_info.pline   = pline;
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
        udata->common.scaled  = scaled[i];

        /* Reset information about the chunk we are looking for */
        udata->chunk_block.offset = HADDR_UNDEF;
        udata->chunk_block.length = 0;

        /* chunk_idx is calculated in get_addr callback */
        if ((storage->ops->get_addr)(&idx_info, udata) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't query chunk address");

    
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

        _udata[i] = (void **)&udata;

    } /* end count */

done:
    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5D__struct_chunk_lookup() */
