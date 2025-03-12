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
 *            - Calculate the file selection boudning box and the chunks
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
        bool         is_partial_dim[H5S_MAX_RANK];    /* Whether a dimension is currently a partial chunk */
        int          curr_dim;                        /* Current dimension to increment */
        hssize_t adjust[H5S_MAX_RANK]; /* Adjustment to make to all file chunks (for shape same algorithm) */
        hsize_t  zeros[H5S_MAX_RANK];  /* All zero vector (for start parameter to setting hyperslab on partial
                                          chunks for "all" selection) */
        hsize_t  dset_sel_chunks;
        bool     shape_same;
        unsigned u;

        /* Get number of elements selected in file */
        sel_points = dset_info[i].nelmts;

        /* Nothing to do if no points selected, I/O is skipped, or no shared chunk cache client */
        if (sel_points == 0 || dset_info[i].skip_io || !dset_info[i].layout->sc_ops)
            continue;

        dset_sel_chunks = 0;

        /* Get chunk dimensions */
        assert(dset_info[i].layout->sc_ops->layout_query);
        if (dset_info[i].layout->sc_ops->layout_query(dset_info[i].dset, chunk_dims, NULL, NULL) < 0)
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
            if (H5S_get_simple_extent_dims(dset_info[i].mem_space, mem_dims, NULL) < 0)
                HGOTO_ERROR(H5E_DATASPACE, H5E_CANTGET, FAIL, "can't get memory dataspace dimensions");
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
        if (tmp_dset_space && H5S_close(single_chunk_space) < 0)
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
herr_t
H5SC_read(H5SC_t *cache, size_t count, H5D_dset_io_info_t *dset_info)
{
    H5SC_io_info_t sc_io_info;
    herr_t         ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    assert(cache);
    assert(count == 0 || dset_info);

    /* Set up selections in sc_io_info */
    if (H5SC__io_info_init(cache, &sc_io_info, count, dset_info) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "can't initialize selections for I/O");

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
herr_t
H5SC_write(H5SC_t *cache, size_t count, H5D_dset_io_info_t *dset_info)
{
    H5SC_io_info_t sc_io_info;
    herr_t         ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    assert(cache);
    assert(count == 0 || dset_info);

    /* Set up selections in sc_io_info */
    if (H5SC__io_info_init(cache, &sc_io_info, count, dset_info) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "can't initialize selections for I/O");

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
 * Function: H5SC_set_extent_notify
 *
 * Purpose:  Called after H5Dset_extent() has been called for a dataset,
 *           so the cache can recompute chunk indices, delete chunks,
 *           clear unused sections of chunks, etc.
 *
 * Return:   SUCCEED on success, FAIL on failure
 *-------------------------------------------------------------------------
 */
herr_t
H5SC_set_extent_notify(H5SC_t *cache, H5D_t *dset, const hsize_t *old_dims)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    assert(cache);
    assert(dset);
    assert(dset->shared->layout.sc_ops);
    assert(old_dims);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5SC_set_extent_notify() */
