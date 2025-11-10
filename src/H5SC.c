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

/****************/
/* Module Setup */
/****************/

#include "H5SCmodule.h" /* This source code file is part of the H5SC module */
#define H5D_FRIEND      /* Suppress error about including H5Dpkg */

/***********/
/* Headers */
/***********/
#include "H5private.h"   /* Generic Functions            */
#include "H5ACprivate.h" /* Metadata Cache*/
#include "H5Dpkg.h"      /* Datasets                     */
#include "H5Eprivate.h"  /* Error handling               */
#include "H5Fprivate.h"  /* Files                        */
#include "H5MMprivate.h" /* Memory management            */
#include "H5SCpkg.h"     /* Shared chunk cache           */

/****************/
/* Local Macros */
/****************/

/* Initial size of sel_chunks array in H5SC_io_info_t */
#define H5SC_INIT_CHUNK_LIST_SIZE 128

/******************/
/* Local Typedefs */
/******************/

/* Structure to describe a single chunk involved in I/O */
typedef struct {
    H5D_dset_io_info_t *dset_info;            /* Dataset I/O info for the dataset the chunk belongs to */
    hsize_t             coords[H5S_MAX_RANK]; /* Coordinates of the chunk in the dataset */
    hsize_t scaled[H5S_MAX_RANK]; /* Scaled (chunk dims = 1) coordinates of the chunk in the dataset.  This is
                                     essentially the offset divided by the chunk dimension (for each rank). */
    H5S_t *file_space;      /* Dataspace for the chunk where the extent matches the chunk dimensions and the
                               selection contains elements in the chunk selected for I/O */
    H5S_t *mem_space;       /* Memory dataspace where the extent matches the overall memory dataspace and the
                               selection contains elements in the chunk selected for I/O */
    bool file_space_shared; /* Whether file_space is shared with another owner and therefore does not need to
                               be closed */
    bool mem_space_shared; /* Whether mem_space is shared with another owner and therefore does not need to be
                              closed */
    H5_flexible_const_ptr_t buf; /* Memory I/O buffer for the dataset */
} H5SC_io_sel_chunk_t;

/* Structure containing information on a single I/O operation */
typedef struct {
    H5SC_io_sel_chunk_t *sel_chunks;       /* Array of chunks selected for I/O */
    size_t               num_sel_chunks;   /* Number of valid chunks in sel_chunks */
    size_t               sel_chunks_alloc; /* Number of elements allocated in sel_chunks */
} H5SC_io_info_t;

/********************/
/* Local Prototypes */
/********************/

static herr_t H5SC__io_info_init(H5SC_t *cache, H5SC_io_info_t *sc_io_info, size_t count,
                                 H5D_dset_io_info_t *dset_info);
static herr_t H5SC__io_info_term(H5SC_io_info_t *sc_io_info);

/*****************************/
/* Library Private Variables */
/*****************************/

/*********************/
/* Package Variables */
/*********************/

/* Package initialization variable */
bool H5_PKG_INIT_VAR = false;

/*******************/
/* Local Variables */
/*******************/

/*-------------------------------------------------------------------------
 * Function: H5SC_create
 *
 * Purpose:  Creates a new, empty shared chunk cache.
 *
 * Return:   Pointer to newly created cache on success, NULL on failure
 *-------------------------------------------------------------------------
 */
H5SC_t *
H5SC_create(H5F_t *file, H5P_genplist_t *fa_plist)
{
    H5SC_t *cache     = NULL;
    H5SC_t *ret_value = NULL;

    FUNC_ENTER_NOAPI(NULL)

    assert(file);
    assert(fa_plist);

    /* Allocated cache struct */
    if (NULL == (cache = H5MM_malloc(sizeof(H5SC_t))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, NULL, "unable to allocate buffer for shared chunk cache");

    /* Success */
    ret_value = cache;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5SC_create() */

/*-------------------------------------------------------------------------
 * Function: H5SC_destroy
 *
 * Purpose:  Destroys a shared chunk cache, freeing all data used. Does
 *           not flush chunks.
 *
 * Return:   SUCCEED on success, FAIL on failure
 *-------------------------------------------------------------------------
 */
herr_t
H5SC_destroy(H5SC_t *cache)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    assert(cache);

    H5MM_free(cache);
    cache = NULL;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5SC_destroy() */

/*-------------------------------------------------------------------------
 * Function: H5SC_flush
 *
 * Purpose:  Flushes all cached data from a shared chunk cache.
 *
 * Return:   SUCCEED on success, FAIL on failure
 *-------------------------------------------------------------------------
 */
herr_t
H5SC_flush(H5SC_t *cache)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    assert(cache);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5SC_flush() */

/*-------------------------------------------------------------------------
 * Function: H5SC_flush_dset
 *
 * Purpose:  Flushes all data cached for a single dataset. If evict is
 *           true, also evicts all cached data.
 *
 * Return:   SUCCEED on success, FAIL on failure
 *-------------------------------------------------------------------------
 */
herr_t
H5SC_flush_dset(H5SC_t *cache, H5D_t *dset, bool evict)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    assert(cache);
    assert(dset);
    assert(dset->shared->layout.sc_ops);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5SC_flush_dset() */

/*-------------------------------------------------------------------------
 * Function: H5SC__io_info_init
 *
 * Purpose:  Initializes sc_io_info for the I/O operation, computing the
 *           per-chunk memory and file dataspaces.  Allocates and fills in
 *           the sc_io_info->sel_chunks array , which is an array of chunks
 *           (across all datasets) that are involved in this I/O operation.
 *           The algorithm is basically:
 *
 *           - For each dataset:
 *            - Calculate the file selection bounding box and the chunks
 *              that intersect it
 *            - Check for file and memory selections having the same shape
 *              (shapesame) and calculate the offset adjustment for the
 *              shapesame algorithm if so
 *            - For each chunk that intersects the bounding box:
 *             - Calculate the chunk's file dataspace by intersecting (AND)
 *               the file selection with a hyperslab selecting the entire
 *               chunk, then resizing the dataspace to only include the
 *               chunk
 *             - If this is the only chunk selected, the chunk's memory
 *               space is the same as the memory space for the entire
 *               dataset
 *             - Otherwise, if (shapesame), copy the chunk dataspace's
 *               selection to the chunk's memory dataspace, then adjust
 *               the selection offset using the previously computed
 *               adjustment and the offset of the chunk.  For "all"
 *               selections simply select the chunk in the memory dataspace
 *               taking into account the adjustment and chunk offset.
 *             - Otherwise, compute the chunk's memory dataspace by using
 *               H5S_select_project_intersection() to project the
 *               intersection of the file selection and the chunk from the
 *               file selection to the memory selection
 *
 *           In the future this function may be extended to handle
 *           previously cached chunks immediately, in order to minimize the
 *           size of the array in sc_io_info->sel_chunks.  This is why the
 *           cache parameter is included even though it's unused.
 *
 * Return:   SUCCEED on success, FAIL on failure
 *-------------------------------------------------------------------------
 */
static herr_t
H5SC__io_info_init(H5SC_t H5_ATTR_NDEBUG_UNUSED *cache, H5SC_io_info_t *sc_io_info, size_t count,
                   H5D_dset_io_info_t *dset_info)
{
    H5S_t *tmp_dset_space     = NULL;
    H5S_t *single_chunk_space = NULL;
    size_t i;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE

    assert(cache);
    assert(sc_io_info);
    assert(count == 0 || dset_info);

    /* Initialize sc_io_info */
    sc_io_info->sel_chunks       = NULL;
    sc_io_info->num_sel_chunks   = 0;
    sc_io_info->sel_chunks_alloc = 0;

    /* Iterate over datasets */
    for (i = 0; i < count; i++) {
        H5S_sel_type file_sel_type;
        H5S_sel_type mem_sel_type;
        hsize_t      sel_points;
        hsize_t      chunk_dims[H5S_MAX_RANK];
        hsize_t      file_dims[H5S_MAX_RANK];
        hsize_t      mem_dims[H5S_MAX_RANK];
        unsigned     file_ndims;
        unsigned     mem_ndims;
        hsize_t      start_coords[H5O_LAYOUT_NDIMS]; /* Starting coordinates of selection */
        hsize_t      coords[H5S_MAX_RANK];           /* Current coordinates of chunk */
        hsize_t      end[H5S_MAX_RANK];              /* Final coordinates of chunk */
        hsize_t      start_scaled[H5S_MAX_RANK];     /* Starting scaled coordinates of selection */
        hsize_t      scaled[H5S_MAX_RANK];           /* Scaled coordinates for this chunk */
        hsize_t      file_sel_start[H5S_MAX_RANK];   /* Offset of low bound of file selection */
        hsize_t      file_sel_end[H5S_MAX_RANK];     /* Offset of high bound of file selection */
        unsigned     num_partial_dims;
        hsize_t      curr_partial_clip[H5S_MAX_RANK]; /* Current partial dimension sizes to clip against */
        hsize_t      partial_dim_size[H5S_MAX_RANK];  /* Size of a partial dimension */
        bool is_partial_dim[H5S_MAX_RANK] = {false};  /* Whether a dimension is currently a partial chunk */
        int  curr_dim;                                /* Current dimension to increment */
        hssize_t adjust[H5S_MAX_RANK]; /* Adjustment to make to all file chunks (for shape same algorithm) */
        hsize_t  zeros[H5S_MAX_RANK];  /* All zero vector (for start parameter to setting hyperslab on partial
                                          chunks for "all" selection) */
        hsize_t  dset_sel_chunks;
        bool     shape_same;
        unsigned u;

        /* Get number of elements selected in file */
        sel_points = dset_info[i].nelmts;

        /* Nothing to do if no points selected, I/O is skipped, or no shared chunk cache client */
        if (sel_points == 0 || dset_info[i].skip_io || !dset_info[i].dset->shared->layout.sc_ops)
            continue;

        dset_sel_chunks = 0;

        /* Get chunk dimensions */
        assert(dset_info[i].dset->shared->layout.sc_ops->layout_query);
        if (dset_info[i].dset->shared->layout.sc_ops->layout_query(dset_info[i].dset, chunk_dims, NULL,
                                                                   NULL) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "unable to query chunk dimensions");

        /* Get dataspace ranks */
        file_ndims = (unsigned)H5S_GET_EXTENT_NDIMS(dset_info[i].file_space);
        mem_ndims  = (unsigned)H5S_GET_EXTENT_NDIMS(dset_info[i].mem_space);

        /* Get the file and memory selection types */
        if ((file_sel_type = H5S_GET_SELECT_TYPE(dset_info[i].file_space)) < H5S_SEL_NONE)
            HGOTO_ERROR(H5E_DATASET, H5E_BADSELECT, FAIL, "unable to get type of selection");
        if ((mem_sel_type = H5S_GET_SELECT_TYPE(dset_info[i].mem_space)) < H5S_SEL_NONE)
            HGOTO_ERROR(H5E_DATASET, H5E_BADSELECT, FAIL, "unable to get type of selection");

        /* Get file dataspace dimensions */
        if (H5S_get_simple_extent_dims(dset_info[i].file_space, file_dims, NULL) < 0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTGET, FAIL, "can't get file dataspace dimensions");

        /*
         * Set up bounding box and initial chunk coordinates for chunk iteration
         */

        /* Initialize num_partial_dims.  Only needed for all selection, but put it outside of that block to
         * stop compiler warnings */
        num_partial_dims = 0;

        /* Check for "all" selection */
        if (H5S_SEL_ALL == file_sel_type) {
            /* Set up partial chunk tracking and set the bounding box to the extent */
            memset(zeros, 0, sizeof(zeros));
            for (u = 0; u < file_ndims; u++) {
                /* Validate this chunk dimension */
                if (chunk_dims[u] == 0)
                    HGOTO_ERROR(H5E_DATASET, H5E_BADVALUE, FAIL, "chunk size must be > 0, dim = %u ", u);

                /* Set up start / end coordinates for first chunk */
                scaled[u] = start_scaled[u] = 0;
                coords[u] = start_coords[u] = 0;
                end[u]                      = chunk_dims[u] - 1;

                /* Set up selection bounds */
                file_sel_start[u] = 0;
                file_sel_end[u]   = file_dims[u];

                /* Initialize partial chunk dimension information */
                partial_dim_size[u] = file_dims[u] % chunk_dims[u];
                if (file_dims[u] < chunk_dims[u]) {
                    curr_partial_clip[u] = partial_dim_size[u];
                    is_partial_dim[u]    = true;
                    num_partial_dims++;
                }
                else {
                    curr_partial_clip[u] = chunk_dims[u];
                    is_partial_dim[u]    = false;
                }
            }

            /* Create "temporary" chunk for selection operations (copy file space) */
            if (NULL == (single_chunk_space = H5S_create_simple(file_ndims, chunk_dims, NULL)))
                HGOTO_ERROR(H5E_DATASET, H5E_CANTCREATE, FAIL, "unable to create dataspace for chunk");
        }
        else {
            /* Get bounding box for file selection */
            if (H5S_SELECT_BOUNDS(dset_info[i].file_space, file_sel_start, file_sel_end) < 0)
                HGOTO_ERROR(H5E_DATASPACE, H5E_CANTGET, FAIL, "can't get file selection bound info");

            /* Iterate over dimensions */
            for (u = 0; u < file_ndims; u++) {
                /* Validate this chunk dimension */
                if (chunk_dims[u] == 0)
                    HGOTO_ERROR(H5E_DATASET, H5E_BADVALUE, FAIL, "chunk size must be > 0, dim = %u ", u);

                /* Set initial chunk location & hyperslab size */
                scaled[u] = start_scaled[u] = file_sel_start[u] / chunk_dims[u];
                coords[u] = start_coords[u] = scaled[u] * chunk_dims[u];
                end[u]                      = (coords[u] + chunk_dims[u]) - 1;
            }
        }

        /*
         * Check for shape same and patch up differing dimensions
         */

        if ((shape_same = H5S_SELECT_SHAPE_SAME(dset_info[i].file_space, dset_info[i].mem_space)) == true) {
            hsize_t mem_sel_start[H5S_MAX_RANK]; /* Offset of low bound of memory selection */
            hsize_t mem_sel_end[H5S_MAX_RANK];   /* Offset of high bound of memory selection */

            /* The shapes are the same, compute the adjustment offset to use for the memory dataspace
             * calculation */

            /* H5D__read()/H5D__write() should have made sure the ranks are the same */
            assert(file_ndims == mem_ndims);

            /* Get bounding box for memory selection */
            if (H5S_SELECT_BOUNDS(dset_info[i].mem_space, mem_sel_start, mem_sel_end) < 0)
                HGOTO_ERROR(H5E_DATASPACE, H5E_CANTGET, FAIL, "can't get memory selection bound info");

            for (u = 0; u < file_ndims; u++) {
                /* Calculate the adjustment for memory selection from file selection */
                H5_CHECK_OVERFLOW(file_sel_start[u], hsize_t, hssize_t);
                H5_CHECK_OVERFLOW(mem_sel_start[u], hsize_t, hssize_t);
                adjust[u] = (hssize_t)file_sel_start[u] - (hssize_t)mem_sel_start[u];
            }

            /* Get memory dataspace dimensions */
            if (H5S_get_simple_extent_dims(dset_info[i].mem_space, mem_dims, NULL) < 0) {
                HGOTO_ERROR(H5E_DATASPACE, H5E_CANTGET, FAIL, "can't get memory dataspace dimensions");
            }
        }
        else
            /* Create a dataspace with the same extent as the file dataspace */
            if (NULL == (tmp_dset_space = H5S_create_simple(file_ndims, file_dims, NULL)))
                HGOTO_ERROR(H5E_DATASPACE, H5E_CANTCREATE, FAIL, "unable to copy file dataspace");

        /*
         * Iterate through each chunk in the file selection's bounding box
         */

        while (sel_points) {
            /* Check for intersection of current chunk and file selection */
            if ((H5S_SEL_ALL == file_sel_type) ||
                (true == H5S_SELECT_INTERSECT_BLOCK(dset_info[i].file_space, coords, end))) {
                H5SC_io_sel_chunk_t *sel_chunk;

                /*
                 * Once the cache is implemented, we could check for cached chunks here and handle I/O
                 * immediately to/from the cached chunk, avoiding the need to extend the array.  We will still
                 * need to calculate the dataspaces.
                 */

                /*
                 * Make room in sel_chunks array
                 */

                /* Check for no array allocated */
                if (!sc_io_info->sel_chunks) {
                    assert(sc_io_info->num_sel_chunks == 0);
                    assert(sc_io_info->sel_chunks_alloc == 0);

                    /* Allocate initial array */
                    if (NULL == (sc_io_info->sel_chunks =
                                     malloc(H5SC_INIT_CHUNK_LIST_SIZE * sizeof(sc_io_info->sel_chunks[0]))))
                        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL,
                                    "can't allocate array of selected chunks");
                    sc_io_info->sel_chunks_alloc = H5SC_INIT_CHUNK_LIST_SIZE;
                }
                else if (sc_io_info->num_sel_chunks == sc_io_info->sel_chunks_alloc) {
                    /* Out of space, double array size */
                    if (NULL == (sc_io_info->sel_chunks =
                                     realloc(sc_io_info->sel_chunks, 2 * sc_io_info->sel_chunks_alloc *
                                                                         sizeof(sc_io_info->sel_chunks[0]))))
                        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL,
                                    "can't reallocate array of selected chunks");
                    sc_io_info->sel_chunks_alloc *= 2;
                }
                assert(sc_io_info->num_sel_chunks < sc_io_info->sel_chunks_alloc);

                /* Set convenience pointer */
                sel_chunk = &(sc_io_info->sel_chunks[sc_io_info->num_sel_chunks]);

                /* Set up selected chunk struct */
                sel_chunk->dset_info = &dset_info[i];
                sel_chunk->buf       = dset_info[i].buf;
                H5MM_memcpy(sel_chunk->coords, coords, sizeof(hsize_t) * file_ndims);
                H5MM_memcpy(sel_chunk->scaled, scaled, sizeof(hsize_t) * file_ndims);
                sel_chunk->file_space        = NULL;
                sel_chunk->mem_space         = NULL;
                sel_chunk->file_space_shared = false;
                sel_chunk->mem_space_shared  = false;

                sc_io_info->num_sel_chunks++;

                /*
                 * Set up chunk file dataspace including selection
                 */

                /* Different actions for different file selection types */
                if (H5S_SEL_ALL == file_sel_type) {
                    /* "all" selection in file, simply reuse single chunk dataspace, select valid elements if
                     * it's a partial edge chunk */
                    /* Set the file chunk dataspace */
                    if (NULL == (sel_chunk->file_space = H5S_copy(single_chunk_space, true, false)))
                        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTCOPY, FAIL, "unable to copy chunk dataspace");

                    /* If there are partial dimensions for this chunk, set the hyperslab for them */
                    if (num_partial_dims > 0)
                        if (H5S_select_hyperslab(sel_chunk->file_space, H5S_SELECT_SET, zeros, NULL,
                                                 curr_partial_clip, NULL) < 0)
                            HGOTO_ERROR(H5E_DATASET, H5E_CANTSELECT, FAIL, "can't create chunk selection");
                }
                else {
                    if (H5S_SEL_HYPERSLABS == file_sel_type) {
                        /* Hyperslab selection in file, create dataspace for chunk, 'AND'ing the overall
                         * selection with the current chunk */
                        if (H5S_combine_hyperslab(dset_info[i].file_space, H5S_SELECT_AND, coords, NULL,
                                                  chunk_dims, NULL, &sel_chunk->file_space) < 0)
                            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTCOPY, FAIL,
                                        "unable to combine file space selection with chunk block");
                    }
                    else {
                        /* H5S_SEL_POINTS */
                        /* Iterate over the file selection to create a new selection using only the points
                         * that are in this chunk.  This algorithm is probably less efficient than the one in
                         * H5Dchunk.c that iterates over the points once and adds chunks involved to a skip
                         * list.  We may want to change it to that in the future. */
                        assert(0 && "incomplete");
                    }

                    /* Resize chunk's dataspace dimensions to size of chunk */
                    if (H5S_set_extent_real(sel_chunk->file_space, chunk_dims) < 0)
                        HGOTO_ERROR(H5E_DATASET, H5E_CANTSELECT, FAIL, "can't adjust chunk dimensions");

                    /* Move selection back to have correct offset in chunk */
                    if (H5S_SELECT_ADJUST_U(sel_chunk->file_space, coords) < 0)
                        HGOTO_ERROR(H5E_DATASET, H5E_CANTSELECT, FAIL, "can't adjust chunk selection");
                }

                /* Decrement # of points left in file selection */
                sel_points -= H5S_GET_SELECT_NPOINTS(sel_chunk->file_space);

                /* Increment number of chunks selected in dataset */
                dset_sel_chunks++;

                /*
                 * Set up chunk memory dataspace including selection
                 */

                /* Check for only one chunk selected in this dataset */
                if (sel_points == 0 && dset_sel_chunks == 1) {
                    /* Since only one chunk is selected, no complicated transformation is necessary to get the
                     * matching memory space. Just point at the entire memory dataspace & selection */
                    sel_chunk->mem_space = dset_info[i].mem_space;

                    /* Indicate that the chunk's memory space is shared */
                    sel_chunk->mem_space_shared = true;
                }
                else {
                    if (shape_same) {
                        H5S_sel_type chunk_sel_type; /* Chunk's selection type */
                        /* Dataspace selections are the same shape in memory and the file, copy the file
                         * selection to memory and offset it as necessary to match */
                        /* Create chunk memory dataspace with the same extent as the overall memory dataspace
                         */
                        if ((sel_chunk->mem_space = H5S_create_simple(mem_ndims, mem_dims, NULL)) == NULL)
                            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTCREATE, FAIL, "unable to create memory space");

                        /* Get the chunk's selection type */
                        if ((chunk_sel_type = H5S_GET_SELECT_TYPE(sel_chunk->file_space)) < H5S_SEL_NONE)
                            HGOTO_ERROR(H5E_DATASET, H5E_BADSELECT, FAIL, "unable to get type of selection");

                        /* Set memory selection */
                        if (H5S_SEL_ALL == chunk_sel_type) {
                            hsize_t mem_coords[H5S_MAX_RANK];

                            /* "all" selection in chunk, simply select the entire chunk within the memory
                             * space, offset as necessary */

                            /* Adjust the chunk coordinates */
                            for (u = 0; u < file_ndims; u++)
                                mem_coords[u] = (hsize_t)((hssize_t)coords[u] - adjust[u]);

                            /* Set to same shape as chunk */
                            if (H5S_select_hyperslab(sel_chunk->mem_space, H5S_SELECT_SET, mem_coords, NULL,
                                                     chunk_dims, NULL) < 0)
                                HGOTO_ERROR(H5E_DATASET, H5E_CANTSELECT, FAIL,
                                            "can't create chunk memory selection");
                        }
                        else {
                            hssize_t piece_adjust[H5S_MAX_RANK];

                            /* Hyperslab or point selection, copy chunk file selection to memory and offset */

                            /* Sanity check */
                            assert(H5S_SEL_HYPERSLABS == file_sel_type);

                            /* Copy the file chunk's selection */
                            if (H5S_SELECT_COPY(sel_chunk->mem_space, sel_chunk->file_space, false) < 0)
                                HGOTO_ERROR(H5E_DATASPACE, H5E_CANTCOPY, FAIL, "unable to copy selection");

                            /* Compute the adjustment for this chunk */
                            for (u = 0; u < file_ndims; u++) {
                                /* Compensate for the chunk offset */
                                H5_CHECK_OVERFLOW(coords[u], hsize_t, hssize_t);
                                piece_adjust[u] = adjust[u] - (hssize_t)coords[u];
                            } /* end for */

                            /* Adjust the selection */
                            if (H5S_SELECT_ADJUST_S(sel_chunk->mem_space, piece_adjust) < 0)
                                HGOTO_ERROR(H5E_DATASET, H5E_CANTSET, FAIL, "unable to adjust selection");
                        }
                    }
                    else {
                        /* Create dataspace for entire current chunk within the file space.  Shouldn't matter
                         * if it goes beyond the extent since we're not doing I/O with this space */
                        if (H5S_select_hyperslab(tmp_dset_space, H5S_SELECT_SET, coords, NULL, chunk_dims,
                                                 NULL) < 0)
                            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTSELECT, FAIL, "unable to select chunk block");

                        /* Calculate memory selection for this chunk by projecting intersection of full file
                         * selection and file chunk to full memory selection.  Note that we share the
                         * selection so we cannot further modify sel_chunk->mem_space (it can be closed). */
                        if (H5S_select_project_intersection(dset_info[i].file_space, dset_info[i].mem_space,
                                                            tmp_dset_space, &sel_chunk->mem_space, true) < 0)
                            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTINIT, FAIL, "can't project intersection");
                    }
                }
            }

            /*
             * Advance to next chunk within bounding box
             */

            /* Set current increment dimension */
            curr_dim = (int)file_ndims - 1;

            /* Increment chunk location in fastest changing dimension */
            coords[curr_dim] += chunk_dims[curr_dim];
            end[curr_dim] += chunk_dims[curr_dim];
            scaled[curr_dim]++;

            /* Bring chunk location back into bounds, if necessary */
            if (coords[curr_dim] > file_sel_end[curr_dim]) {
                do {
                    /* Reset current dimension's location to 0 */
                    scaled[curr_dim] = start_scaled[curr_dim];
                    coords[curr_dim] = start_coords[curr_dim];
                    end[curr_dim]    = (coords[curr_dim] + chunk_dims[curr_dim]) - 1;

                    /* Check for previous partial chunk in this dimension */
                    if (is_partial_dim[curr_dim] && end[curr_dim] < file_dims[curr_dim]) {
                        /* Sanity checks */
                        assert(num_partial_dims > 0);
                        assert(H5S_SEL_ALL == file_sel_type);

                        /* Reset partial chunk information for this dimension */
                        curr_partial_clip[curr_dim] = chunk_dims[curr_dim];
                        is_partial_dim[curr_dim]    = false;
                        num_partial_dims--;
                    } /* end if */

                    /* Decrement current dimension */
                    curr_dim--;

                    /* Check for valid current dim */
                    if (curr_dim >= 0) {
                        /* Increment chunk location in current dimension */
                        scaled[curr_dim]++;
                        coords[curr_dim] += chunk_dims[curr_dim];
                        end[curr_dim] = (coords[curr_dim] + chunk_dims[curr_dim]) - 1;
                    } /* end if */
                } while (curr_dim >= 0 && (coords[curr_dim] > file_sel_end[curr_dim]));

                /* Check for new partial chunk in this dimension for "all" selection. First check for valid
                 * current dim */
                if ((H5S_SEL_ALL == file_sel_type) && curr_dim >= 0) {
                    /* Check for partial chunk in this dimension */
                    if (!is_partial_dim[curr_dim] && file_dims[curr_dim] <= end[curr_dim]) {
                        /* Set partial chunk information for this dimension */
                        curr_partial_clip[curr_dim] = partial_dim_size[curr_dim];
                        is_partial_dim[curr_dim]    = true;
                        num_partial_dims++;

                        /* Sanity check */
                        assert(num_partial_dims <= file_ndims);
                    } /* end if */
                }     /* end if */
            }
        }

        /* Close temporary dataspaces */
        if (tmp_dset_space && H5S_close(tmp_dset_space) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTRELEASE, FAIL, "can't release temporary file dataspace");
        tmp_dset_space = NULL;
        if (single_chunk_space && H5S_close(single_chunk_space) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTRELEASE, FAIL, "can't release temporary file dataspace");
        single_chunk_space = NULL;
    }

done:
    /* Clean up on failure */
    if (ret_value < 0) {
        /* Close temporary dataspaces */
        if (tmp_dset_space && H5S_close(tmp_dset_space) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CANTRELEASE, FAIL, "can't release temporary file dataspace");
        if (single_chunk_space && H5S_close(single_chunk_space) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CANTRELEASE, FAIL, "can't release temporary file dataspace");

        /* Terminate io info */
        if (H5SC__io_info_term(sc_io_info) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CANTRELEASE, FAIL, "can't close I/O info");
    }
    else {
        assert(!tmp_dset_space);
        assert(!single_chunk_space);
    }

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5SC__io_info_init() */

/*-------------------------------------------------------------------------
 * Function: H5SC__io_info_term
 *
 * Purpose:  Frees all memory referenced by sc_io_info.  Does not free
 *           sc_io_info itself.
 *
 * Return:   SUCCEED on success, FAIL on failure
 *-------------------------------------------------------------------------
 */
static herr_t
H5SC__io_info_term(H5SC_io_info_t *sc_io_info)
{
    size_t i;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE

    assert(sc_io_info);

    /* Iterate over selected chunks, freeing any info contained */
    for (i = 0; i < sc_io_info->num_sel_chunks; i++) {
        if (sc_io_info->sel_chunks[i].file_space && !sc_io_info->sel_chunks[i].file_space_shared)
            if (H5S_close(sc_io_info->sel_chunks[i].file_space) < 0)
                HDONE_ERROR(H5E_DATASET, H5E_CANTRELEASE, FAIL, "can't release temporary file dataspace");
        if (sc_io_info->sel_chunks[i].mem_space && !sc_io_info->sel_chunks[i].mem_space_shared)
            if (H5S_close(sc_io_info->sel_chunks[i].mem_space) < 0)
                HDONE_ERROR(H5E_DATASET, H5E_CANTRELEASE, FAIL, "can't release temporary memory dataspace");
    }

    /* Free sel_chunks array */
    free(sc_io_info->sel_chunks);
    sc_io_info->num_sel_chunks   = 0;
    sc_io_info->sel_chunks_alloc = 0;

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5SC__io_info_term() */

/*-------------------------------------------------------------------------
 * Function: H5SC_read
 *
 * Purpose:  Reads raw data through a shared chunk cache. There may be
 * datasets in the dset_info array that do not support the shared chunk
 * cache. These datasets must be ignored by the shared chunk cache. There
 * may also be datasets that have skip_io set. These datasets must also be
 * skipped.
 *
 * Return:   SUCCEED on success, FAIL on failure
 *-------------------------------------------------------------------------
 */
#ifdef OUT
herr_t
H5SC_read(H5SC_t *cache, size_t count, H5D_dset_io_info_t *dset_info)
{
    H5SC_io_info_t sc_io_info;
    herr_t         ret_value = SUCCEED;
    hsize_t       *scaled[H5S_MAX_RANK];                   /* First used in the lookup callback */
    haddr_t       *addr[H5S_MAX_RANK];                     /* First used in the lookup callback */
    haddr_t        addr0;                                  /* Instanced addr_t */
    hsize_t       *size[H5S_MAX_RANK];                     /* First used in the lookup callback */
    hsize_t        size0;                                  /* Instanced hsize_t */
    hsize_t       *defined_values_size[H5S_MAX_RANK];      /* First used in the lookup callback */
    size_t        *size_hint[H5S_MAX_RANK];                /* First used in the lookup callback*/
    size_t        *defined_values_size_hint[H5S_MAX_RANK]; /* First used in the lookup callback */
    void          *udata_arr[H5S_MAX_RANK];                /* First used in the lookup callback */
    void *chunk = NULL; /* First used in block read; eventually becomes the chunk intermediate struct */
    H5D_io_type_info_t my_io_type_info;    /* First used in the scatter_mem callback */
    const H5S_t       *scatter_mem_space;  /* Used in the scatter_mem callback */
    const H5S_t       *scatter_file_space; /* Used in the scatter_mem callback */
    haddr_t            md_tag                                  = HADDR_UNDEF;
    bool               partial_bound_chunks_different_encoding = false;
    H5O_pline_t       *pline                                   = NULL; /* I/O pipeline info */
    hbool_t            filtered                                = false;

    FUNC_ENTER_NOAPI(FAIL)

    assert(cache);
    assert(count == 0 || dset_info);

    /*
     * *** VO: Pass the I/O request (for a single chunk) "through" the cache ***
     * Since the cache isn't implemented yet, we'll simulate this using the following
     * process:
     * 0. Perform initial setup (completed by `H5SC_io_info_init`)
     * For each chunk in `sc_io_info`:
     *
     * 1. Cache emulation for Raw Data Read
     *  - Check if the chunk is in cache (it won't be at this point);
     *    if not, look up the chunk with `H5D__struct_chunk_lookup()`
     *  - If the chunk is found on disk, initiate a chunk read from disk
     *    In the initial version, we know the chunk is on disk, so this
     *    callback will return the address and size of the chunk.
     *
     * 2. Read the chunk into memory
     *  - Use `H5F_block_read()` to read with the address/size previously
     *    returned from the lookup callback
     *  - Afterward, use `H5D__struct_chunk_decode()` to decode the chunk
     *    (from on disk memory to in cache memory)
     *  - The chunk data then needs to be scattered into the user buffer
     *    using `H5D__struct_chunk_scatter_mem()`
     */

    /* Set up selections in sc_io_info */
    if (H5SC__io_info_init(cache, &sc_io_info, count, dset_info) < 0) {
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "can't initialize selections for I/O");
    }

    /* Loop through the datasets */
    for (int i = 0; i < count; i++) {

        /* Set metadata tagging for this dataset */
        H5AC_tag(dset_info[i].dset->oloc.addr, &md_tag);

        size_t chunk_count = sc_io_info.num_sel_chunks;

        /* Chunk lookup (in file) */
        assert(dset_info[i].dset->shared->layout.sc_ops->lookup);

        for (int j = 0; j < chunk_count; j++) {
            /* Setup scaled for the jth chunk */
            scaled[j] = sc_io_info.sel_chunks[j].scaled;

            /* Setup the initial address with a unique pointer */
            haddr_t *tmp_addr = malloc(sizeof(haddr_t));
            addr[j]           = tmp_addr;

            /* Setup the initial size with a unique pointer */
            hsize_t *tmp_size = malloc(sizeof(hsize_t));
            size[j]           = tmp_size;

            hsize_t *tmp_def_val_size = malloc(sizeof(hsize_t));
            defined_values_size[j]    = tmp_def_val_size;

            size_t *tmp_size_hint = malloc(sizeof(size_t));
            *tmp_size_hint        = 0;
            size_hint[j]          = tmp_size_hint;

            size_t *tmp_def_val_size_hint = malloc(sizeof(size_t));
            defined_values_size_hint[j]   = tmp_def_val_size_hint;
        }

        if (dset_info[i].dset->shared->layout.sc_ops->lookup(
                dset_info[i].dset,   /* INPUT: Pointer to the dataset (in file) */
                chunk_count,         /* INPUT: The number of chunks in this datasets */
                scaled,              /* INPUT: Scaled coordinate(s) of the chunk */
                addr,                /* OUTPUT: Address of the chunk (on disk) */
                size,                /* OUTPUT: Chunk size (on disk) */
                defined_values_size, /* OUTPUT: The number of bytes to read (if the list of defined values is
                                        needed) */
                size_hint,           /* OUTPUT: The suggested allocation size for the chunk (pre-decode) */
                defined_values_size_hint, /* OUTPUT: Suggested allocation size (if the list of defined values
                                             is needed) */
                &udata_arr /* OUTPUT: Buffer to be passed through to H5D__struct_chunk_decode(_in_place) */
                ) < 0) {
            HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "unable to lookup chunk (SCC)");
        }

        /*
         * ***Read the data from file (disk)***
         */

        void *chunk_arr[chunk_count];

        /* partial_bound_chunks_different_encoding:
         *  When enabled, filters are not applied to partial edge chunks.
         *  When disabled, partial edge chunks are filtered.
         *  Enabling this option will improve performance when appending to the dataset and, when
         *  compression filters are used, prevent reallocation of these chunks.
         */
        if (dset_info[i].dset->shared->layout.sc_ops->layout_query(
                dset_info[i].dset, NULL, NULL, &partial_bound_chunks_different_encoding) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "unable to query chunk dimensions");

        /* Filtered or not */
        pline = &(dset_info[i].dset->shared->dcpl_cache.pline);
        if (pline && pline->tot_filt_nsects)
            filtered = true;

        for (int j = 0; j < chunk_count; j++) {
            /* true: a NOT-to-be-filtered-partial-edge chunk */
            /* false : a to-be-filtered-partial-edge-chunk */
            bool partial_bound = false;

            if (filtered && partial_bound_chunks_different_encoding &&
                H5D__chunk_is_partial_edge_chunk(dset_info[i].dset->shared->ndims,
                                                 dset_info[i].dset->shared->layout.u.struct_chunk.dim,
                                                 scaled[j], dset_info[i].dset->shared->curr_dims))
                partial_bound = true;

            /* Allocate buffer for the chunk data */
            if (NULL == (chunk_arr[j] = H5MM_malloc(*size_hint[j]))) {
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, H5_ITER_ERROR,
                            "memory allocation failed for raw data chunk (SCC)");
            }

            if (H5F_block_read(dset_info[i].dset->oloc.file, /* INPUT: Current file ID*/
                               H5FD_MEM_DRAW, /* INPUT: Set based on the definitions in the H5F_mem_t type */
                               *addr[j],      /* INPUT: Address returned from the lookup callback */
                               *size[j],      /* INPUT: Size returned from the lookup callback */
                               chunk_arr[j] /* INPUT/OUTPUT: Chunk buffer (which will be transitioned into the
                                               intermediate format) */
                               ) < 0) {
                HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "unable to block read from file (SCC)");
            }

            assert(dset_info[i].dset->shared->layout.sc_ops->decode);

            size_t nbytes =
                *size[j]; /*Prior to decode, this is the number of bytes used in the chunk buffer*/
            size_t alloc_size = *size_hint[j]; /*Prior to decode, this is the size of the chunk buffer*/

            if (dset_info[i].dset->shared->layout.sc_ops->decode(
                    dset_info[i].dset, /* INPUT: Pointer to the dataset in memory*/
                    &nbytes, /* INPUT/OUTPUT: nbytes; entry: number of bytes used in the chunk buffer; exit:
                                total number of bytes used */
                    &alloc_size,   /* INPUT/OUTPUT: alloc_size*/
                    partial_bound, /* UNUSED*/
                    &chunk_arr[j], /* INPUT/OUTPUT: chunk; On entry: the pointer tot he on disk formatted
                                      chunk buffer; On exit: the pointer to the chunk intermediate struct */
                    udata_arr[j]   /* INPUT: Used to store some chunk properties*/
                    ) < 0) {
                HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "unable to decode chunk in place (SCC)");
            }
            assert(dset_info[i].dset->shared->layout.sc_ops->scatter_mem);

            size_t src_type_size = 0; /* Manually taken from `*dset_info->type_info`, looking at the contents
                                         of the `layout_io_info` section */
            size_t dst_type_size = 0; /* Manually taken from `*dset_info->type_info`, looking at the contents
                                         of the `layout_io_info` section */
            my_io_type_info.tconv_buf      = NULL;          /* Datatype conv buffer (pointer) */
            my_io_type_info.tconv_buf_size = src_type_size; /* Size of type conversion buffer */
            my_io_type_info.bkg_buf        = NULL;          /* Pointer to background buffer */
            my_io_type_info.bkg_buf_size   = dst_type_size; /* Size of the background buffer */

            /* There is also a need to set the mem/file spaces */
            scatter_mem_space =
                sc_io_info.sel_chunks[j]
                    .mem_space; /* Pointer to the memory space ID for the chunk (derived from sc_io_info) */
            scatter_file_space =
                sc_io_info.sel_chunks[j]
                    .file_space; /* Pointer to the file space ID for the chunk (derived from sc_io_info) */

            if (dset_info[i].dset->shared->layout.sc_ops->scatter_mem(
                    &dset_info[i],      /* INPUT: Pointer to the dataset in memory */
                    &my_io_type_info,   /* INPUT: Localized version of H5D_io_type_info_t; derived from
                                           information available in the sc_io_info struct */
                    scatter_mem_space,  /* INPUT: Pointer to the appropriate memory space ID */
                    scatter_file_space, /* INPUT: Pointer to the appropriate file space space ID */
                    chunk_arr[j],       /* INPUT/OUTPUT: Memory-formatted chunk buffer */
                    udata_arr[j]        /* UNUSED: Udata, which is modified by previous callbacks */
                    ) < 0) {
                HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "unable to scatter mem for read chunk (SCC)");
            }

            /* Free the buffers via callback after scattering data to user buffer */
            if (dset_info[i].dset->shared->layout.sc_ops->evict(dset_info[i].dset, chunk_arr[j],
                                                                udata_arr[j]) < 0) {
                HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "unable to evict the chunk (SCC)");
            }

        } /* Chunk Processing Loop End */

        /* Free the allocated components prior to processing the next dataset */
        for (int j = 0; j < chunk_count; j++) {
            free(addr[j]);
            free(size[j]);
            free(defined_values_size[j]);
            free(size_hint[j]);
            free(defined_values_size_hint[j]);
        }

        H5AC_tag(md_tag, NULL); /* Reset the metadata tag for the next dataset */
    }                           /* Dataset Loop End */

done:
    /* Terminate sc_io_info */
    if (H5SC__io_info_term(&sc_io_info) < 0)
        HDONE_ERROR(H5E_DATASET, H5E_CANTRELEASE, FAIL, "can't close I/O info");

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5SC_read() */
#endif
herr_t
H5SC_read(H5SC_t *cache, size_t count, H5D_dset_io_info_t *dset_info)
{
    H5SC_io_info_t     sc_io_info;
    herr_t             ret_value = SUCCEED;
    H5D_io_type_info_t my_io_type_info;    /* First used in the scatter_mem callback */
    const H5S_t       *scatter_mem_space;  /* Used in the scatter_mem callback */
    const H5S_t       *scatter_file_space; /* Used in the scatter_mem callback */
    haddr_t            md_tag                                  = HADDR_UNDEF;
    bool               partial_bound_chunks_different_encoding = false;
    H5O_pline_t       *pline                                   = NULL; /* I/O pipeline info */
    hbool_t            filtered                                = false;
    // size_t             nbytes;
    // size_t             buf_size;
    size_t alloc_size;
    size_t alloc_size_total;

    FUNC_ENTER_NOAPI(FAIL)

    assert(cache);
    assert(count == 0 || dset_info);

    /*
     * Set up selections for the provided I/O request in the sc_io_info structure. This structure will contain
     * the per-chunk selection which is relevant to the provided I/O request.
     */
    if (H5SC__io_info_init(cache, &sc_io_info, count, dset_info) < 0) {
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTINIT, FAIL, "can't initialize selections for I/O");
    }

    /* Loop through the datasets */
    for (size_t i = 0; i < count; i++) {

        /* Sanity checks to ensure the SCC callbacks are defined for this dataset. */
        assert(dset_info[i].dset->shared->layout.sc_ops->lookup);
        assert(dset_info[i].dset->shared->layout.sc_ops->decode);
        assert(dset_info[i].dset->shared->layout.sc_ops->scatter_mem);

        /* Set metadata tagging for this dataset */
        H5AC_tag(dset_info[i].dset->oloc.addr, &md_tag);

        size_t chunk_count = sc_io_info.num_sel_chunks;

        /* Chunk lookup (in file) */

        /* Create arrays to hold information necessary for callback operations relevant to the read operation.
         */
        const hsize_t *scaled[chunk_count];
        haddr_t       *addr[chunk_count];
        hsize_t       *size[chunk_count];
        hsize_t       *defined_values_size[chunk_count];
        size_t        *size_hint[chunk_count];
        size_t        *defined_values_size_hint[chunk_count];
        void         **udata_arr[chunk_count];

        /* For each chunk in this dataset, initialize each necessary array value*/
        for (size_t j = 0; j < chunk_count; j++) {
            /* Setup scaled for the jth chunk */
            scaled[j] = sc_io_info.sel_chunks[j].scaled;

            /* Setup the initial address with a unique pointer */
            haddr_t *tmp_addr = malloc(sizeof(haddr_t));
            addr[j]           = tmp_addr;

            /* Setup the initial size with a unique pointer */
            hsize_t *tmp_size = malloc(sizeof(hsize_t));
            size[j]           = tmp_size;

            hsize_t *tmp_def_val_size = malloc(sizeof(hsize_t));
            defined_values_size[j]    = tmp_def_val_size;

            size_t *tmp_size_hint = malloc(sizeof(size_t));
            *tmp_size_hint        = 0;
            size_hint[j]          = tmp_size_hint;

            size_t *tmp_def_val_size_hint = malloc(sizeof(size_t));
            defined_values_size_hint[j]   = tmp_def_val_size_hint;
        }

        if (dset_info[i].dset->shared->layout.sc_ops->lookup(dset_info[i].dset, chunk_count, scaled, addr,
                                                             size, defined_values_size, size_hint,
                                                             defined_values_size_hint, &udata_arr) < 0) {
            HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "unable to lookup chunk (SCC)");
        }

        void *chunk_arr[chunk_count];

        /* partial_bound_chunks_different_encoding:
         *  When enabled, filters are not applied to partial edge chunks.
         *  When disabled, partial edge chunks are filtered.
         *  Enabling this option will improve performance when appending to the dataset and, when
         *  compression filters are used, prevent reallocation of these chunks.
         */
        if (dset_info[i].dset->shared->layout.sc_ops->layout_query(
                dset_info[i].dset, NULL, NULL, &partial_bound_chunks_different_encoding) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "unable to query chunk dimensions");

        /* Filtered or not */
        pline = &(dset_info[i].dset->shared->dcpl_cache.pline);
        if (pline && pline->tot_filt_nsects)
            filtered = true;

        /* For each chunk in this dataset: */
        for (size_t j = 0; j < chunk_count; j++) {
            /* true: a NOT-to-be-filtered-partial-edge chunk */
            /* false : a to-be-filtered-partial-edge-chunk */
            bool partial_bound = false;

            // Needed for all chunks (check whether this is should be generated for each chunk or each
            // dataset)
            my_io_type_info.tconv_buf      = NULL; /* Pointer to the datatype conv buffer */
            my_io_type_info.tconv_buf_size = dset_info[i].type_info.src_type_size;
            my_io_type_info.bkg_buf        = NULL; /* Pointer to background buffer */
            my_io_type_info.bkg_buf_size   = dset_info[i].type_info.dst_type_size;

            /* Pointer to the memory space ID for the chunk (derived from sc_io_info) */
            scatter_mem_space = sc_io_info.sel_chunks[j].mem_space;

            /* Pointer to the file space ID for the chunk (derived from sc_io_info) */
            scatter_file_space = sc_io_info.sel_chunks[j].file_space;

            /*
             * If the chunk address is invalid (i.e., has a value of HADDR_UNDEF), the chunk is not present in
             * the file; to ensure the write buffer is not disrupted, a new chunk should be created and the
             * fill value should be propagated into this new chunk using the appropriate callback.
             *
             * NOTE: This chunk should not be written to file by default.
             */
            if (!H5_addr_defined(*addr[j])) {
                /* Free the invalid udata created by the lookup callback. */
                udata_arr[j] = H5MM_xfree(udata_arr[j]);

                /* Create a new chunk that will then have the fill value written to it. */
                if (dset_info[i].dset->shared->layout.sc_ops->new_chunk(
                        dset_info[i].dset, false, size[j], size_hint[j], &chunk_arr[j], &udata_arr[j]) < 0) {
                    HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "unable to create new chunk (SCC)");
                }

                if (dset_info[i].dset->shared->layout.sc_ops->fill(
                        &dset_info[i], &my_io_type_info, sc_io_info.sel_chunks->file_space, size[j],
                        size_hint[j], &alloc_size_total, chunk_arr[j], udata_arr[j]) < 0) {
                    HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "unable to fill chunk (SCC)");
                }
                /* If the chunk addr is valid: */
            }
            else {
                /* If the chunk lookup is successful: */
                if (filtered && partial_bound_chunks_different_encoding &&
                    H5D__chunk_is_partial_edge_chunk(dset_info[i].dset->shared->ndims,
                                                     dset_info[i].dset->shared->layout.u.struct_chunk.dim,
                                                     scaled[j], dset_info[i].dset->shared->curr_dims))
                    partial_bound = true;

                /* Allocate buffer for the chunk data */
                if (NULL == (chunk_arr[j] = H5MM_malloc(*size_hint[j]))) {
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, H5_ITER_ERROR,
                                "memory allocation failed for raw data chunk (SCC)");
                }

                /*
                 * Block read the current chunk using the file ID, the type of allocation request
                 * (H5FD_MEM_DRAW based on the definitions given in the H5F_mem_t type), the on-disk address
                 * of the chunk, the size of the request, and the chunk buffer (which will be transitioned
                 * into the intermediate format.)
                 */
                if (H5F_block_read(dset_info[i].dset->oloc.file, H5FD_MEM_DRAW, *addr[j], *size[j],
                                   chunk_arr[j]) < 0) {
                    HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "unable to block read from file (SCC)");
                }

                // Skip for invalid addr
                if (dset_info[i].dset->shared->layout.sc_ops->decode(dset_info[i].dset, size[j], size_hint[j],
                                                                     partial_bound, &chunk_arr[j],
                                                                     udata_arr[j]) < 0) {
                    HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "unable to decode chunk in place (SCC)");
                }
            }

            // Double check that the scatter_mem_space and scatter_file_space are correctly set.
            if (dset_info[i].dset->shared->layout.sc_ops->scatter_mem(&dset_info[i], &my_io_type_info,
                                                                      scatter_mem_space, scatter_file_space,
                                                                      chunk_arr[j], udata_arr[j]) < 0) {
                HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "unable to scatter mem for read chunk (SCC)");
            }

            /* Free the buffers via callback after scattering data to user buffer */
            if (dset_info[i].dset->shared->layout.sc_ops->evict(dset_info[i].dset, chunk_arr[j],
                                                                udata_arr[j]) < 0) {
                HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "unable to evict the chunk (SCC)");
            }

        } /* Chunk Processing Loop End */

        /* Free the allocated components prior to processing the next dataset */
        for (size_t j = 0; j < chunk_count; j++) {
            free(addr[j]);
            free(size[j]);
            free(defined_values_size[j]);
            free(size_hint[j]);
            free(defined_values_size_hint[j]);
        }

        H5AC_tag(md_tag, NULL); /* Reset the metadata tag for the next dataset */
    }                           /* Dataset Loop End */

done:
    /* Terminate sc_io_info */
    if (H5SC__io_info_term(&sc_io_info) < 0)
        HDONE_ERROR(H5E_DATASET, H5E_CANTRELEASE, FAIL, "can't close I/O info");

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5SC_read() */

/*-------------------------------------------------------------------------
 * Function: H5SC_write
 *
 * Purpose:  Writes raw data through a shared chunk cache. There may be
 * datasets in the dset_info array that do not support the shared chunk
 * cache. These datasets must be ignored by the shared chunk cache.
 *
 * Return:   SUCCEED on success, FAIL on failure
 *-------------------------------------------------------------------------
 */
#ifdef OUT
herr_t
H5SC_write(H5SC_t *cache, size_t count, H5D_dset_io_info_t *dset_info)
{
    H5SC_io_info_t     sc_io_info;
    herr_t             ret_value = SUCCEED;
    size_t             nbytes;   /* Used in the new chunk callback */
    size_t             buf_size; /* Used in the new chunk callback */
    void              *chunk;
    size_t             alloc_size;
    size_t             alloc_size_total;
    size_t             write_size;
    hsize_t           *scaled[H5S_MAX_RANK];
    H5D_io_type_info_t my_io_type_info; /* Used in gather_mem callback */
    const H5S_t       *gather_mem_space;
    const H5S_t       *gather_file_space;
    haddr_t           *addr[H5S_MAX_RANK];
    haddr_t            addr0;
    hsize_t           *size[H5S_MAX_RANK];
    hsize_t            size0;
    hsize_t           *defined_values_size[H5S_MAX_RANK];
    size_t            *size_hint[H5S_MAX_RANK];
    size_t            *defined_values_size_hint[H5S_MAX_RANK];
    void              *udata_arr[H5S_MAX_RANK];
    hsize_t            write_size_arr[H5S_MAX_RANK];
    haddr_t            md_tag                                  = HADDR_UNDEF;
    bool               partial_bound_chunks_different_encoding = false;
    H5O_pline_t       *pline                                   = NULL; /* I/O pipeline info */
    hbool_t            filtered                                = false;

    FUNC_ENTER_NOAPI(FAIL)

    assert(cache);
    assert(count == 0 || dset_info);

    /*
     * *** V0: Pass the I/O request (for a single chunk) "through" the cache ***
     * Since the cache isn't implemented yet, we'll simulate this using the following
     * process:
     * For each chunk in `sc_io_info`:
     *
     * 1. Cache emulation for Raw Data Write
     *  - Look up the chunk (expected failure to find; no cache to search)
     *  - The chunk is not yet written to file (assumed for version 0 prototype),
     *    so it should be fully overwritten
     *  - Call `H5SC_new_chunk_t` to create a new chunk (fill = true)
     *  - Call H5SC_chunk_gather_mem_t
     *
     * 2. Immediately evict the chunk from the "cache"
     *  - The chunk will be dirty (new chunk not in file)
     *  - The chunk is not encoded; encode (diagram says in-place,
     *    need to look at encode as well)
     *  - Call H5SC_chunk_insert_t to insert the chunk into the file indexing
     *  - Call H5F_block_write (in H5Fio.c) to write the chunk (single buffer) to the file
     *  - Free the chunk from the "cache"
     */

    /* Set up selections in sc_io_info */
    if (H5SC__io_info_init(cache, &sc_io_info, count, dset_info) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "can't initialize selections for I/O");

    /* Loop through the datasets */
    for (int i = 0; i < count; i++) {
        /*
         * To avoid issues with metadata tagging while doing multi-dataset I/O,
         * we need to handle tagging within the SCC
         */
        H5AC_tag(dset_info[i].dset->oloc.addr, &md_tag); /* Set the metadata tag */

        /* Throw an error if no chunks were selected */
        if (sc_io_info.num_sel_chunks == 0)
            HDONE_ERROR(H5E_DATASET, H5E_CANTGETSIZE, FAIL,
                        "The number of selected structured chunks should be non-zero (SCC)");

        size_t chunk_count = sc_io_info.num_sel_chunks;

        /*
         * Begin the write process (cache emulation)
         * Look up the chunk in file (for now, simply assert that the callback is present)
         */
        assert(dset_info[i].dset->shared->layout.sc_ops->lookup);

        /* Loop to initialize the components necessary for the SCC */
        for (int j = 0; j < chunk_count; j++) {
            /* Set up scaled for the jth chunk */
            scaled[j] = sc_io_info.sel_chunks[j].scaled;

            /* Setup the address with a unique pointer (will need to be freed after looping through the
             * dataset) */
            haddr_t *tmp_addr = malloc(sizeof(haddr_t));
            *tmp_addr         = HADDR_UNDEF;
            addr[j]           = tmp_addr;

            /* Setup the size with a unique pointer (will need to be freed after looping through the dataset)
             */
            hsize_t *tmp_size = malloc(sizeof(size_t));
            size[j]           = tmp_size;

            hsize_t *tmp_def_val_size = malloc(sizeof(hsize_t));
            defined_values_size[j]    = tmp_def_val_size;

            size_t *tmp_size_hint = malloc(sizeof(size_t));
            size_hint[j]          = tmp_size_hint;

            size_t *tmp_def_val_size_hint = malloc(sizeof(size_t));
            defined_values_size_hint[j]   = tmp_def_val_size_hint;
        }

        /* Pointers up to this point (with n=2 chunks) have been verified to be unique */

        if (dset_info[i].dset->shared->layout.sc_ops->lookup(
                dset_info[i].dset,   /* INPUT: Pointer to the dataset in memory */
                chunk_count,         /* INPUT: The number of chunks in the dataset being processed */
                scaled,              /* INPUT: Scaled coordinate(s) for the chunk */
                addr,                /* OUTPUT: Address of the chunk (on disk) */
                size,                /* OUTPUT: Chunk size (on disk) */
                defined_values_size, /* OUTPUT: The number of bytes to read (if the list of defined values is
                                        needed) */
                size_hint,           /* OUTPUT: The suggested allocation size for the chunk (pre-encode) */
                defined_values_size_hint, /* OUTPUT: Suggested allocation size (if on the list of defined
                                             values is needed) */
                &udata_arr /* OUTPUT: Buffer to be passed through to H5__struct_chunk_encode(_in_place()) */
                ) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "unable to lookup chunk (SCC)");

        /* Free udata since the chunks do not exist (decode will not be called) */
        for (int j = 0; j < chunk_count; j++) {
            udata_arr[j] = H5MM_xfree(udata_arr[j]);
        }

        /* Create a new (empty) chunk (not inserted into the disk chunk index) */
        assert(dset_info[i].dset->shared->layout.sc_ops->new_chunk);

        /*
         * The `new_chunk` callback will need to loop through each chunk here;
         * we will need to have an array of chunk pointers. It *should* be sufficient
         * to simply create an array of void pointers, pass in the elements one-by-one,
         * and then ensure that in the future (when `chunk` is uses), the proper chunk
         * is being iterated on. As with the previous callback, we'll start by simply doing
         * one loop through with the array input and go from there.
         */

        void *chunk_arr[chunk_count];

        for (int j = 0; j < chunk_count; j++) {

            if (dset_info[i].dset->shared->layout.sc_ops->new_chunk(
                    dset_info[i].dset, /* INPUT: Pointer to the dataset in memory */
                    false,             /* INPUT: Bool to set whether to write the fill value to the chunk */
                    &nbytes,           /* OUTPUT: Number of bytes used */
                    &buf_size,         /* OUTPUT: Size of the chunk buffer */
                    &chunk_arr[j],     /* OUTPUT: Pointer to the chunk intermediate struct */
                    &udata_arr[j]      /* OUTPUT: Udata initialization */
                    ) < 0) {
                HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "unable to create new chunk (SCC)");
            }
        }

        /* The gather_mem callback is used to collect data from the memory buffer into the chunk buffer */
        assert(dset_info[i].dset->shared->layout.sc_ops->gather_mem);

        for (int j = 0; j < chunk_count; j++) {

            /*
             * Set the io_type_info struct parameters.
             * NOTE: Since vlen_buf_info isn't required for the v0, we simply avoided
             * initialize it (passing NULL causes an error and should be avoided)
             */
            size_t src_type_size = 0; /* Manually taken from `*dset_info->type_info`, looking at the contents
                                         of the `layout_io_info` section */
            size_t dst_type_size = 0; /* Manually taken from `*dset_info->type_info`, looking at the contents
                                         of the `layout_io_info` section */
            my_io_type_info.tconv_buf              = NULL;          /* Datatype conv buffer (pointer) */
            my_io_type_info.tconv_buf_size         = src_type_size; /* Size of type conversion buffer */
            my_io_type_info.bkg_buf                = NULL;          /* Pointer to background buffer */
            my_io_type_info.bkg_buf_size           = dst_type_size; /* Size of the background buffer */
            my_io_type_info.may_use_in_place_tconv = true;          /* Use in-place if possible */

            gather_mem_space = sc_io_info.sel_chunks[j]
                                   .mem_space; /* Pointer to the memory space ID, derived from sc_io_info */
            gather_file_space = sc_io_info.sel_chunks[j]
                                    .file_space; /* Pointer to the file space ID, derived from sc_io_info */

            /* nbytes, alloc_size, and alloc_size_total are embedded in the chunk intermediate struct and
             * don't need to be adjusted/corrected for each chunk that is looped over */
            if (dset_info[i].dset->shared->layout.sc_ops->gather_mem(
                    &dset_info[i],     /* INPUT: Pointer to the dataset in memory*/
                    &my_io_type_info,  /* INPUT: Localized version of the H5D_io_type_info_t; derived from
                                          information available in the sc_io_info struct */
                    gather_mem_space,  /* INPUT: Pointer to the appropriate memory space ID */
                    gather_file_space, /* INPUT: Pointer to the appropriate file space ID */
                    &nbytes,     /* INPUT/OUTPUT Size of the chunk; will be reallocated in this callback and
                                    returned */
                    &alloc_size, /* INPUT/OUTPUT: Allocated size of the chunk buffer */
                    &alloc_size_total, /* INPUT/OUTPUT: Size of nbytes + alloc_size */
                    chunk_arr[j],      /*INPUT/OUTPUT: Intermediate chunk structure */
                    udata_arr[j]       /* UNUSED */
                    ) < 0) {
                HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "unable to gather mem for new chunk (SCC)");
            }
        }
        /*
         * Start the "eviction" process
         * First, we encode in place (chunk becomes disk formatted chunk buffer)
         */
        assert(dset_info[i].dset->shared->layout.sc_ops->encode_in_place);

        /* partial_bound_chunks_different_encoding:
         *  When enabled, filters are not applied to partial edge chunks.
         *  When disabled, partial edge chunks are filtered.
         *  Enabling this option will improve performance when appending to the dataset and, when
         *  compression filters are used, prevent reallocation of these chunks.
         */
        if (dset_info[i].dset->shared->layout.sc_ops->layout_query(
                dset_info[i].dset, NULL, NULL, &partial_bound_chunks_different_encoding) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "unable to query chunk dimensions");

        pline = &(dset_info[i].dset->shared->dcpl_cache.pline);
        if (pline && pline->tot_filt_nsects)
            filtered = true;

        for (int j = 0; j < chunk_count; j++) {
            /* true: a NOT-to-be-filtered-partial-edge chunk */
            /* false : a to-be-filtered-partial-edge-chunk */
            bool partial_bound = false;

            if (partial_bound_chunks_different_encoding && filtered &&
                H5D__chunk_is_partial_edge_chunk(dset_info[i].dset->shared->ndims,
                                                 dset_info[i].dset->shared->layout.u.struct_chunk.dim,
                                                 scaled[j], dset_info[i].dset->shared->curr_dims))
                partial_bound = true;

            if (dset_info[i].dset->shared->layout.sc_ops->encode_in_place(
                    dset_info[i].dset, /* INPUT: Pointer to the dataset in memory */
                    &write_size,       /* OUTPUT: Sum of the size of the selected bytes and the data bytes */
                    partial_bound,     /* INPUT: Indicate whether to specify if a chunk has a partial boundary
                                  (unsupported     in v0) */
                    &chunk_arr[j], /* INPUT/OUTPUT On entry, points to the chunk intermediate struct; on exit,
                                      points to the on disk file format chunk buffer */
                    udata_arr[j]   /* INPUT/OUTPUT Udata, which has been utilized by previous callbacks */
                    ) < 0) {
                HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "unable to encode chunk in place (SCC)");
            }

            /* Add the computed write size to the appropriate array */
            write_size_arr[j] = write_size;
        }
        /* Insert the chunk into the chunk index within the file */
        assert(dset_info[i].dset->shared->layout.sc_ops->insert);

        if (dset_info[i].dset->shared->layout.sc_ops->insert(
                dset_info[i].dset, /* INPUT: Pointer to the dataset in memory */
                chunk_count,       /* INPUT: Number of chunks in the I/O request */
                &scaled,           /* INPUT: Scaled coordinate for the chunk; derived from `sc_io_info` */
                addr,              /* INPUT/OUTPUT: Array of addrs */
                NULL, /* INPUT: Old disk size array; likely derived from the insert callback (if chunk is on
                         disk) */
                write_size_arr, /* INPUT: Write size, computed from the encode callback */
                chunk_arr,      /* UNUSED: Intermediate chunk structure (on-disk formatted); full array */
                udata_arr       /* INPUT: Array of udata (which are modified by previous callbacks) */
                ) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINSERT, FAIL, "unable to insert chunk into file (SCC)");

        for (int j = 0; j < chunk_count; j++) {
            /* Write the chunk to file using H5F_block_write */
            if (H5F_block_write(
                    dset_info[i].dset->oloc.file, /* INPUT: Current file ID */
                    H5FD_MEM_DRAW, /* INPUT: Based on the definitions in H5F_mem_t, this option seemed the
                                      most appropriate */
                    *addr[j], /* INPUT: This is the address in the file where data will be written; set by the
                                 insert callback */
                    write_size_arr[j], /* INPUT: Encoded write size from the `encode_in_place` callback */
                    chunk_arr[j] /* INPUT: Should be the data buffer (encode last modified the chunk buffer)
                                  */
                    ) < 0) {
                HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "unable to block write to file(SCC)");
            }

            chunk_arr[j] = H5MM_xfree(chunk_arr[j]);
            udata_arr[j] = H5MM_xfree(udata_arr[j]);

            free(addr[j]);
            free(size[j]);
            free(defined_values_size[j]);
            free(size_hint[j]);
            free(defined_values_size_hint[j]);
        }
        H5AC_tag(md_tag, NULL); /* Reset the metadata tag for the next dataset */
    }                           /* End Dataset Loop */
done:
    /* Terminate sc_io_info */
    if (H5SC__io_info_term(&sc_io_info) < 0)
        HDONE_ERROR(H5E_DATASET, H5E_CANTRELEASE, FAIL, "can't close I/O info");

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5SC_write() */
#endif
herr_t
H5SC_write(H5SC_t *cache, size_t count, H5D_dset_io_info_t *dset_info)
{
    H5SC_io_info_t     sc_io_info;
    herr_t             ret_value = SUCCEED;
    size_t             nbytes;   /* Used in the new chunk callback */
    size_t             buf_size; /* Used in the new chunk callback */
    size_t             alloc_size;
    size_t             alloc_size_total;
    size_t             write_size;
    H5D_io_type_info_t my_io_type_info; /* Used in gather_mem callback */
    const H5S_t       *gather_mem_space;
    const H5S_t       *gather_file_space;
    haddr_t            md_tag                                  = HADDR_UNDEF;
    bool               partial_bound_chunks_different_encoding = false;
    H5O_pline_t       *pline                                   = NULL; /* I/O pipeline info */
    hbool_t            filtered                                = false;

    FUNC_ENTER_NOAPI(FAIL)

    assert(cache);
    assert(count == 0 || dset_info);

    /* Set up selections in sc_io_info */
    if (H5SC__io_info_init(cache, &sc_io_info, count, dset_info) < 0) {
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "can't initialize selections for I/O");
    }

    /* Throw an error if no chunks were selected */
    if (sc_io_info.num_sel_chunks == 0) {
        HDONE_ERROR(H5E_DATASET, H5E_CANTGETSIZE, FAIL,
                    "The number of selected structured chunks should be non-zero (SCC)");
    }

    /* Loop through the datasets */
    for (size_t i = 0; i < count; i++) {

        /* Sanity checks for the SCC callbacks for this dataset. */
        assert(dset_info[i].dset->shared->layout.sc_ops->lookup);
        assert(dset_info[i].dset->shared->layout.sc_ops->new_chunk);
        assert(dset_info[i].dset->shared->layout.sc_ops->new_chunk);
        assert(dset_info[i].dset->shared->layout.sc_ops->gather_mem);
        assert(dset_info[i].dset->shared->layout.sc_ops->encode_in_place);
        assert(dset_info[i].dset->shared->layout.sc_ops->insert);

        /*
         * Set the io_type_info struct parameters.
         * NOTE: Since vlen_buf_info isn't required for the v0, we simply avoided
         * initialize it (passing NULL causes an error and should be avoided)
         */

        my_io_type_info.tconv_buf              = NULL; /* Datatype conv buffer (pointer) */
        my_io_type_info.tconv_buf_size         = dset_info[i].type_info.src_type_size;
        my_io_type_info.bkg_buf                = NULL; /* Pointer to background buffer */
        my_io_type_info.bkg_buf_size           = dset_info[i].type_info.dst_type_size;
        my_io_type_info.may_use_in_place_tconv = true; /* Use in-place if possible */

        /*
         * To avoid issues with metadata tagging while doing multi-dataset I/O,
         * we need to handle tagging within the SCC
         */
        H5AC_tag(dset_info[i].dset->oloc.addr, &md_tag); /* Set the metadata tag */

        size_t chunk_count = sc_io_info.num_sel_chunks;

        /*
         * Begin the write process (cache emulation)
         * Look up the chunk in file (for now, simply assert that the callback is present)
         */

        /* Create the arrays necessary for write operation. */
        const hsize_t *scaled[chunk_count];
        haddr_t       *addr[chunk_count];
        hsize_t       *size[chunk_count];
        hsize_t        old_disk_size[chunk_count];
        hsize_t       *defined_values_size[chunk_count];
        size_t        *size_hint[chunk_count];
        size_t        *defined_values_size_hint[chunk_count];
        void          *udata_arr[chunk_count];
        hsize_t        write_size_arr[chunk_count];

        /*
         * Loop to initialize the components necessary for processing the I/O request through the SCC
         * callbacks.
         */
        for (size_t j = 0; j < chunk_count; j++) {
            /* Set up scaled for the jth chunk */
            scaled[j] = sc_io_info.sel_chunks[j].scaled;

            /* Setup the address with a unique pointer (will need to be freed after looping through the
             * dataset)
             */
            haddr_t *tmp_addr = malloc(sizeof(haddr_t));
            *tmp_addr         = HADDR_UNDEF;
            addr[j]           = tmp_addr;

            /*
             * Setup the size with a unique pointer (will need to be freed after looping through the
             * dataset).
             */
            hsize_t *tmp_size = malloc(sizeof(hsize_t));
            size[j]           = tmp_size;

            hsize_t *tmp_def_val_size = malloc(sizeof(hsize_t));
            defined_values_size[j]    = tmp_def_val_size;

            size_t *tmp_size_hint = malloc(sizeof(size_t));
            size_hint[j]          = tmp_size_hint;

            size_t *tmp_def_val_size_hint = malloc(sizeof(size_t));
            defined_values_size_hint[j]   = tmp_def_val_size_hint;
        }

        if (dset_info[i].dset->shared->layout.sc_ops->lookup(dset_info[i].dset, chunk_count, scaled, addr,
                                                             size, defined_values_size, size_hint,
                                                             defined_values_size_hint, udata_arr) < 0) {
            HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "unable to lookup chunk (SCC)");
        }

        /* partial_bound_chunks_different_encoding:
         *  When enabled, filters are not applied to partial edge chunks.
         *  When disabled, partial edge chunks are filtered.
         *  Enabling this option will improve performance when appending to the dataset and, when
         *  compression filters are used, prevent reallocation of these chunks.
         */
        if (dset_info[i].dset->shared->layout.sc_ops->layout_query(
                dset_info[i].dset, NULL, NULL, &partial_bound_chunks_different_encoding) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "unable to query chunk dimensions");

        pline = &(dset_info[i].dset->shared->dcpl_cache.pline);
        if (pline && pline->tot_filt_nsects)
            filtered = true;

        /* Create the array that will hold the pointers to the chunk data structure */
        void *chunk_arr[chunk_count];

        for (size_t j = 0; j < chunk_count; j++) {
            bool partial_bound = false;
            /* if assert(addr[j]); free the old udata, then create a new chunk*/
            if (!H5_addr_defined(*addr[j])) {
                /* As a consequence of how the lookup callback functions, it is necessary to free the udata
                 * for each chunk not found on disk.
                 */
                udata_arr[j] = H5MM_xfree(udata_arr[j]);

                /* When not found on disk, a new (empty) chunk needs to be created using this callback. Note
                 * that this callback does not insert this newly created chunk into the on-disk index.
                 */
                // Set the size from sc_io_info struct
                if (dset_info[i].dset->shared->layout.sc_ops->new_chunk(
                        dset_info[i].dset, false, size[j], size_hint[j], &chunk_arr[j], &udata_arr[j]) < 0) {
                    HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "unable to create new chunk (SCC)");
                }
            }
            else {
                old_disk_size[j] = *size[j];

                /* If the chunk lookup is successful: */
                if (filtered && partial_bound_chunks_different_encoding &&
                    H5D__chunk_is_partial_edge_chunk(dset_info[i].dset->shared->ndims,
                                                     dset_info[i].dset->shared->layout.u.struct_chunk.dim,
                                                     scaled[j], dset_info[i].dset->shared->curr_dims))
                    partial_bound = true;

                /* Allocate buffer for the chunk data */
                if (NULL == (chunk_arr[j] = H5MM_malloc(*size_hint[j]))) {
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, H5_ITER_ERROR,
                                "memory allocation failed for raw data chunk (SCC)");
                }

                /* Read in the chunk, then decode the chunk */

                if (H5F_block_read(dset_info[i].dset->oloc.file, H5FD_MEM_DRAW, *addr[j], *size[j],
                                   chunk_arr[j]) < 0) {
                    HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "unable to block read from file (SCC)");
                }
                if (dset_info[i].dset->shared->layout.sc_ops->decode(dset_info[i].dset, size[j], size_hint[j],
                                                                     partial_bound, &chunk_arr[j],
                                                                     udata_arr[j]) < 0) {
                    HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "unable to decode chunk in place (SCC)");
                }
            }
        }

        for (size_t j = 0; j < chunk_count; j++) {

            /* Pointer to the memory space ID, derived from sc_io_info */
            gather_mem_space = sc_io_info.sel_chunks[j].mem_space;
            // gather_mem_space = dset_info[i].mem_space;
            /* Pointer to the file space ID, derived from sc_io_info */
            gather_file_space = sc_io_info.sel_chunks[j].file_space;
            // gather_file_space = dset_info[i].file_space;

            /* The gather_mem callback is used to collect data from the memory buffer into the chunk buffer.
             */
            if (dset_info[i].dset->shared->layout.sc_ops->gather_mem(
                    &dset_info[i], &my_io_type_info, gather_mem_space, gather_file_space, size[j],
                    size_hint[j], &alloc_size_total, chunk_arr[j], udata_arr[j]) < 0) {
                HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "unable to gather mem for new chunk (SCC)");
            }
        }
        /*
         * Start the "eviction" process
         * First, we encode in place (chunk becomes disk formatted chunk buffer)
         */

        for (int j = 0; j < chunk_count; j++) {
            /* true: a NOT-to-be-filtered-partial-edge chunk */
            /* false : a to-be-filtered-partial-edge-chunk */
            bool partial_bound = false;

            if (partial_bound_chunks_different_encoding && filtered &&
                H5D__chunk_is_partial_edge_chunk(dset_info[i].dset->shared->ndims,
                                                 dset_info[i].dset->shared->layout.u.struct_chunk.dim,
                                                 scaled[j], dset_info[i].dset->shared->curr_dims))
                partial_bound = true;

            if (dset_info[i].dset->shared->layout.sc_ops->encode_in_place(
                    dset_info[i].dset, &write_size, partial_bound, &chunk_arr[j], udata_arr[j]) < 0) {
                HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "unable to encode chunk in place (SCC)");
            }

            /* Add the computed write size to the appropriate array */
            write_size_arr[j] = write_size;
        }

        /* Insert the chunk into the chunk index within the file */
        if (dset_info[i].dset->shared->layout.sc_ops->insert(dset_info[i].dset, chunk_count, &scaled, addr,
                                                             old_disk_size, write_size_arr, chunk_arr,
                                                             udata_arr) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINSERT, FAIL, "unable to insert chunk into file (SCC)");

        for (size_t j = 0; j < chunk_count; j++) {
            /* Write the chunk to file using H5F_block_write */
            if (H5F_block_write(dset_info[i].dset->oloc.file, H5FD_MEM_DRAW, *addr[j], write_size_arr[j],
                                chunk_arr[j]) < 0) {
                HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "unable to block write to file (SCC)");
            }

            chunk_arr[j] = H5MM_xfree(chunk_arr[j]);
            udata_arr[j] = H5MM_xfree(udata_arr[j]);

            free(addr[j]);
            free(size[j]);
            free(defined_values_size[j]);
            free(size_hint[j]);
            free(defined_values_size_hint[j]);
        }
        H5AC_tag(md_tag, NULL); /* Reset the metadata tag for the next dataset */
    }                           /* End Dataset Loop */
done:
    /* Terminate sc_io_info */
    if (H5SC__io_info_term(&sc_io_info) < 0)
        HDONE_ERROR(H5E_DATASET, H5E_CANTRELEASE, FAIL, "can't close I/O info");

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5SC_write() */

/*-------------------------------------------------------------------------
 * Function: H5SC_direct_chunk_read
 *
 * Purpose:  Reads the chunk that starts at coordinates give by offset
 *           directly from disk to buf, without any decoding or
 *           conversion. First flushes that chunk if it is dirty in the
 *           cache.
 *
 * Return:   SUCCEED on success, FAIL on failure
 *-------------------------------------------------------------------------
 */
herr_t
H5SC_direct_chunk_read(H5SC_t *cache, H5D_t *dset, const hsize_t *offset, void *udata, void *buf,
                       size_t *buf_size)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    assert(cache);
    assert(dset);
    assert(dset->shared->layout.sc_ops);
    assert(offset);
    assert(buf);
    assert(buf_size);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5SC_direct_chunk_read() */

/*-------------------------------------------------------------------------
 * Function: H5SC_direct_chunk_write
 *
 * Purpose:  Writes the chunk that starts at coordinates give by offset
 *           directly from buf to disk, without any decoding or
 *           conversion. First evicts that chunk from cache if it is
 *           present.
 *
 * Return:   SUCCEED on success, FAIL on failure
 *-------------------------------------------------------------------------
 */
herr_t
H5SC_direct_chunk_write(H5SC_t *cache, H5D_t *dset, const hsize_t *offset, void *udata, const void *buf)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    assert(cache);
    assert(dset);
    assert(dset->shared->layout.sc_ops);
    assert(offset);
    assert(buf);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5SC_direct_chunk_write() */

/*-------------------------------------------------------------------------
 * Function: H5SC_get_defined
 *
 * Purpose:  Returns a copy of file_space with only elements selected that are both selected in file_space and
 *defined in dset. If file_space uses a point selection, the ordering of selected points will be preserved in
 *the returned dataspace.
 *
 * Return:   SUCCEED on success, FAIL on failure
 *-------------------------------------------------------------------------
 */
H5S_t *
H5SC_get_defined(H5SC_t *cache, H5D_t *dset, const H5S_t *file_space)
{
    H5S_t *defined   = NULL;
    H5S_t *ret_value = NULL;

    FUNC_ENTER_NOAPI(NULL)

    assert(cache);
    assert(dset);
    assert(dset->shared->layout.sc_ops);
    assert(file_space);

    /* FOR NOW: just return copy of file_space */
    if (NULL == (defined = H5S_copy(file_space, false, true)))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, NULL, "unable to copy dataspace");

    /* Set return value */
    ret_value = defined;
    defined   = NULL;

done:
    if (defined) {
        assert(!ret_value);
        if (H5S_close(defined) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CLOSEERROR, NULL, "unable to release dataspace");
    }

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5SC_get_defined() */

/*-------------------------------------------------------------------------
 * Function: H5SC_erase
 *
 * Purpose:  Causes the elements selected in file_space to become undefined in dset. If dset does not support
 *tracking defined elements, returns an error.
 *
 * Return:   SUCCEED on success, FAIL on failure
 *-------------------------------------------------------------------------
 */
herr_t
H5SC_erase(H5SC_t *cache, H5D_t *dset, const H5S_t *file_space)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    assert(cache);
    assert(dset);
    assert(dset->shared->layout.sc_ops);
    assert(file_space);

    /* Check for support for erasing values */
    if (!dset->shared->layout.sc_ops->erase_values)
        HGOTO_ERROR(H5E_DATASET, H5E_UNSUPPORTED, FAIL, "dataset does not support erasing values");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5SC_erase() */

/*-------------------------------------------------------------------------
 * Function: H5SC_prune_by_extent
 *
 * Purpose:  TBD: just FAIL for now
 *
 * Return:   SUCCEED on success, FAIL on failure
 *-------------------------------------------------------------------------
 */
static herr_t
H5SC_prune_by_extent(H5SC_t *cache, H5D_t *dset, const hsize_t H5_ATTR_UNUSED *old_dims)
{
    FUNC_ENTER_NOAPI_NOERR

    assert(cache);
    assert(dset);
    assert(dset->shared->layout.sc_ops);

    FUNC_LEAVE_NOAPI(FAIL)

} /* end H5SC_prune_by_extent() */

/*-------------------------------------------------------------------------
 * Function: H5SC_set_extent_notify
 *
 * Purpose:  Called after H5Dset_extent() has been called for a dataset,
 *           so the cache can recompute chunk indices, delete chunks,
 *           clear unused sections of chunks, etc. as needed for structured chunk.
 *
 *           This routine follows the logic in H5D__set_extent() for dense chunks
 *           when "changed" is true.
 *
 * Return:   SUCCEED on success, FAIL on failure
 *-------------------------------------------------------------------------
 */
herr_t
H5SC_set_extent_notify(H5SC_t *cache, H5D_t *dset, const hsize_t *old_dims)
{
    hsize_t  ext_dims[H5S_MAX_RANK]; /* The extended dimension sizes */
    unsigned dim_idx;                /* Dimension index */
    bool     shrink    = false;      /* Flag to indicate a dimension has shrank */
    bool     expand    = false;      /* Flag to indicate a dimension has grown */
    herr_t   ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    assert(cache);
    assert(dset);
    assert(dset->shared->layout.sc_ops);
    assert(old_dims);

    /* This should be the extended space which is done in H5D__set_extent() */
    if (H5S_get_simple_extent_dims(dset->shared->space, ext_dims, NULL) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't cache dataspace dimensions");

    /* Determine if we are shrinking and/or expanding any dimensions */
    for (dim_idx = 0; dim_idx < dset->shared->ndims; dim_idx++) {
        /* Check for various status changes */
        if (ext_dims[dim_idx] < old_dims[dim_idx])
            shrink = true;
        if (ext_dims[dim_idx] > old_dims[dim_idx])
            expand = true;

        /* Update the cached copy of the dataset's dimensions */
        dset->shared->curr_dims[dim_idx] = ext_dims[dim_idx];
    } /* end for */

    /*-------------------------------------------------------------------------
     * Modify the dataset storage
     *-------------------------------------------------------------------------
     */
    /* Update the index values for the cached chunks for this dataset */
    if (H5D_STRUCT_CHUNK == dset->shared->layout.type) {
        /* Set the cached chunk info */
        if (H5D__struct_chunk_set_info(dset) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTSET, FAIL, "unable to update # of chunks");
    }

    /* NOTE for structured chunk:
     * --for [shrink/expand] case:
     *   Let H5SC_prune_by_extent() decide what to do later, it fails for now
     */
    /*-------------------------------------------------------------------------
     * Remove chunk information in the case of chunked datasets
     * This removal takes place only in case we are shrinking the dataset
     * and if the chunks are written
     *-------------------------------------------------------------------------
     */
    if (H5D_STRUCT_CHUNK == dset->shared->layout.type) {
        if ((expand || shrink) &&
            ((*dset->shared->layout.ops->is_space_alloc)(&dset->shared->layout.storage)))
            /* This routine just fails for now. */
            if (H5SC_prune_by_extent(cache, dset, old_dims) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "unable to prune chunks");

        /* NOTE for structured chunk:
         * --for [expand] case: nothing to be done
         *      if (expand &&
         *          (dset->shared->layout.u.chunk.flags & H5O_LAYOUT_CHUNK_DONT_FILTER_PARTIAL_BOUND_CHUNKS)
         * && (dset->shared->dcpl_cache.pline.nused > 0))
         */
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5SC_set_extent_notify() */
