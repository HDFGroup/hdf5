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

/****************/
/* Module Setup */
/****************/

#define H5D_PACKAGE		/*suppress error about including H5Dpkg	  */


/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Dpkg.h"		/* Dataset functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5FLprivate.h"	/* Free Lists                           */
#include "H5Iprivate.h"		/* IDs			  		*/
#ifdef H5_HAVE_PARALLEL
#include "H5MMprivate.h"	/* Memory management			*/
#endif /* H5_HAVE_PARALLEL */
#include "H5Vprivate.h"		/* Vector and array functions		*/


/****************/
/* Local Macros */
/****************/

/* Default skip list height for storing list of chunks */
#define H5D_DEFAULT_SKIPLIST_HEIGHT     8

/* Macros for iterating over chunks to operate on */
#define H5D_CHUNK_GET_FIRST_NODE(map) (map->use_single ? (H5SL_node_t *)(1) : H5SL_first(map->sel_chunks))
#define H5D_CHUNK_GET_NODE_INFO(map, node)  (map->use_single ? map->single_chunk_info : (H5D_chunk_info_t *)H5SL_item(node))
#define H5D_CHUNK_GET_NEXT_NODE(map, node)  (map->use_single ? (H5SL_node_t *)NULL : H5SL_next(node))


/******************/
/* Local Typedefs */
/******************/


/********************/
/* Local Prototypes */
/********************/

/* Chunked layout operation callbacks */
static herr_t H5D_chunk_io_init(const H5D_io_info_t *io_info,
    const H5D_type_info_t *type_info, hsize_t nelmts, const H5S_t *file_space,
    const H5S_t *mem_space, H5D_chunk_map_t *fm);
static herr_t H5D_chunk_read(H5D_io_info_t *io_info, const H5D_type_info_t *type_info,
    hsize_t nelmts, const H5S_t *file_space, const H5S_t *mem_space,
    H5D_chunk_map_t *fm);
static herr_t H5D_chunk_write(H5D_io_info_t *io_info, const H5D_type_info_t *type_info,
    hsize_t nelmts, const H5S_t *file_space, const H5S_t *mem_space,
    H5D_chunk_map_t *fm);
static herr_t H5D_chunk_io_term(const H5D_chunk_map_t *fm);

/* "Null" layout operation callbacks */
static ssize_t H5D_null_readvv(const H5D_io_info_t *io_info,
    size_t dset_max_nseq, size_t *dset_curr_seq, size_t dset_len_arr[], hsize_t dset_offset_arr[],
    size_t mem_max_nseq, size_t *mem_curr_seq, size_t mem_len_arr[], hsize_t mem_offset_arr[]);

/* Helper routines */
static herr_t H5D_free_chunk_info(void *item, void *key, void *opdata);
static herr_t H5D_create_chunk_map_single(H5D_chunk_map_t *fm,
    const H5D_io_info_t *io_info);
static herr_t H5D_create_chunk_file_map_hyper(H5D_chunk_map_t *fm,
    const H5D_io_info_t *io_info);
static herr_t H5D_create_chunk_mem_map_hyper(const H5D_chunk_map_t *fm);
static herr_t H5D_chunk_file_cb(void *elem, hid_t type_id, unsigned ndims,
    const hsize_t *coords, void *fm);
static herr_t H5D_chunk_mem_cb(void *elem, hid_t type_id, unsigned ndims,
    const hsize_t *coords, void *fm);



/*********************/
/* Package Variables */
/*********************/

/* Compact storage layout I/O ops */
const H5D_layout_ops_t H5D_LOPS_CHUNK[1] = {{
    H5D_chunk_io_init,
    H5D_chunk_read,
    H5D_chunk_write,
#ifdef H5_HAVE_PARALLEL
    H5D_chunk_collective_read,
    H5D_chunk_collective_write,
#endif /* H5_HAVE_PARALLEL */
    NULL,
    NULL,
    H5D_chunk_io_term
}};


/*******************/
/* Local Variables */
/*******************/

/* "null" storage layout I/O ops */
const H5D_layout_ops_t H5D_LOPS_NULL[1] = {{
    NULL,
    NULL,
    NULL,
#ifdef H5_HAVE_PARALLEL
    NULL,
    NULL,
#endif /* H5_HAVE_PARALLEL */
    H5D_null_readvv,
    NULL,
    NULL
}};

/* Declare a free list to manage the H5D_chunk_info_t struct */
H5FL_DEFINE(H5D_chunk_info_t);



/*-------------------------------------------------------------------------
 * Function:	H5D_chunk_io_init
 *
 * Purpose:	Performs initialization before any sort of I/O on the raw data
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Thursday, March 20, 2008
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_chunk_io_init(const H5D_io_info_t *io_info, const H5D_type_info_t *type_info,
    hsize_t nelmts, const H5S_t *file_space, const H5S_t *mem_space,
    H5D_chunk_map_t *fm)
{
    H5D_t *dataset = io_info->dset;     /* Local pointer to dataset info */
    const H5T_t *mem_type = type_info->mem_type;        /* Local pointer to memory datatype */
    H5S_t *tmp_mspace = NULL;   /* Temporary memory dataspace */
    hssize_t old_offset[H5O_LAYOUT_NDIMS];  /* Old selection offset */
    htri_t file_space_normalized = FALSE;   /* File dataspace was normalized */
    hid_t f_tid = (-1);           /* Temporary copy of file datatype for iteration */
    hbool_t iter_init = FALSE;  /* Selection iteration info has been initialized */
    unsigned f_ndims;           /* The number of dimensions of the file's dataspace */
    int sm_ndims;               /* The number of dimensions of the memory buffer's dataspace (signed) */
    H5SL_node_t *curr_node;     /* Current node in skip list */
    H5S_sel_type fsel_type;     /* Selection type on disk */
    char bogus;                 /* "bogus" buffer to pass to selection iterator */
    unsigned u;                 /* Local index variable */
    hbool_t sel_hyper_flag;
    herr_t ret_value = SUCCEED;	/* Return value		*/

    FUNC_ENTER_NOAPI_NOINIT(H5D_chunk_io_init)

    /* Get layout for dataset */
    fm->layout = &(dataset->shared->layout);
    fm->nelmts = nelmts;

    /* Check if the memory space is scalar & make equivalent memory space */
    if((sm_ndims = H5S_GET_EXTENT_NDIMS(mem_space)) < 0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTGET, FAIL, "unable to get dimension number")
    /* Set the number of dimensions for the memory dataspace */
    H5_ASSIGN_OVERFLOW(fm->m_ndims, sm_ndims, int, unsigned);

    /* Get dim number and dimensionality for each dataspace */
    fm->f_ndims = f_ndims = dataset->shared->layout.u.chunk.ndims - 1;
    if(H5S_get_simple_extent_dims(file_space, fm->f_dims, NULL) < 0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTGET, FAIL, "unable to get dimensionality")

    /* Normalize hyperslab selections by adjusting them by the offset */
    /* (It might be worthwhile to normalize both the file and memory dataspaces
     * before any (contiguous, chunked, etc) file I/O operation, in order to
     * speed up hyperslab calculations by removing the extra checks and/or
     * additions involving the offset and the hyperslab selection -QAK)
     */
    if((file_space_normalized = H5S_hyper_normalize_offset((H5S_t *)file_space, old_offset)) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_BADSELECT, FAIL, "unable to normalize dataspace by offset")

    /* Decide the number of chunks in each dimension*/
    for(u=0; u<f_ndims; u++) {
        /* Keep the size of the chunk dimensions as hsize_t for various routines */
        fm->chunk_dim[u]=fm->layout->u.chunk.dim[u];

        /* Round up to the next integer # of chunks, to accomodate partial chunks */
        fm->chunks[u] = ((fm->f_dims[u]+dataset->shared->layout.u.chunk.dim[u])-1) / dataset->shared->layout.u.chunk.dim[u];
    } /* end for */

    /* Compute the "down" size of 'chunks' information */
    if(H5V_array_down(f_ndims,fm->chunks,fm->down_chunks) < 0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_BADVALUE, FAIL, "can't compute 'down' sizes")

#ifdef H5_HAVE_PARALLEL
    /* Calculate total chunk in file map*/
    fm->select_chunk = NULL;
    fm->total_chunks = 1;
    for(u = 0; u < fm->f_ndims; u++)
       fm->total_chunks = fm->total_chunks * fm->chunks[u];
    if(io_info->using_mpi_vfd) {
        H5_CHECK_OVERFLOW(fm->total_chunks, hsize_t, size_t);
        if(NULL == (fm->select_chunk = (H5D_chunk_info_t **)H5MM_calloc((size_t)fm->total_chunks * sizeof(H5D_chunk_info_t *))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate chunk info")
    } /* end if */
#endif /* H5_HAVE_PARALLEL */


    /* Initialize "last chunk" information */
    fm->last_index = (hsize_t)-1;
    fm->last_chunk_info = NULL;

    /* Point at the dataspaces */
    fm->file_space = file_space;
    fm->mem_space = mem_space;

    /* Special case for only one element in selection */
    /* (usually appending a record) */
    if(nelmts == 1
#ifdef H5_HAVE_PARALLEL
            && !(io_info->using_mpi_vfd)
#endif /* H5_HAVE_PARALLEL */
        ) {
        /* Initialize skip list for chunk selections */
        fm->sel_chunks = NULL;
        fm->use_single = TRUE;

        /* Initialize single chunk dataspace */
        if(NULL == dataset->shared->cache.chunk.single_space) {
            /* Make a copy of the dataspace for the dataset */
            if((dataset->shared->cache.chunk.single_space = H5S_copy(file_space, TRUE, FALSE)) == NULL)
                HGOTO_ERROR(H5E_DATASPACE, H5E_CANTCOPY, FAIL, "unable to copy file space")

            /* Resize chunk's dataspace dimensions to size of chunk */
            if(H5S_set_extent_real(dataset->shared->cache.chunk.single_space, fm->chunk_dim) < 0)
                HGOTO_ERROR(H5E_DATASPACE, H5E_CANTSET, FAIL, "can't adjust chunk dimensions")

            /* Set the single chunk dataspace to 'all' selection */
            if(H5S_select_all(dataset->shared->cache.chunk.single_space, TRUE) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_CANTSELECT, FAIL, "unable to set all selection")
        } /* end if */
        fm->single_space = dataset->shared->cache.chunk.single_space;
        HDassert(fm->single_space);

        /* Allocate the single chunk information */
        if(NULL == dataset->shared->cache.chunk.single_chunk_info) {
            if(NULL == (dataset->shared->cache.chunk.single_chunk_info = H5FL_MALLOC(H5D_chunk_info_t)))
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate chunk info")
        } /* end if */
        fm->single_chunk_info = dataset->shared->cache.chunk.single_chunk_info;
        HDassert(fm->single_chunk_info);

        /* Reset chunk template information */
        fm->mchunk_tmpl = NULL;

        /* Set up chunk mapping for single element */
        if(H5D_create_chunk_map_single(fm, io_info) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to create chunk selections for single element")
    } /* end if */
    else {
        /* Initialize skip list for chunk selections */
        if(NULL == dataset->shared->cache.chunk.sel_chunks) {
            if(NULL == (dataset->shared->cache.chunk.sel_chunks = H5SL_create(H5SL_TYPE_HSIZE, 0.5, (size_t)H5D_DEFAULT_SKIPLIST_HEIGHT)))
                HGOTO_ERROR(H5E_DATASET, H5E_CANTCREATE, FAIL, "can't create skip list for chunk selections")
        } /* end if */
        fm->sel_chunks = dataset->shared->cache.chunk.sel_chunks;
        HDassert(fm->sel_chunks);

        /* We are not using single element mode */
        fm->use_single = FALSE;

        /* Get type of selection on disk & in memory */
        if((fsel_type = H5S_GET_SELECT_TYPE(file_space)) < H5S_SEL_NONE)
            HGOTO_ERROR(H5E_DATASET, H5E_BADSELECT, FAIL, "unable to get type of selection")
        if((fm->msel_type = H5S_GET_SELECT_TYPE(mem_space)) < H5S_SEL_NONE)
            HGOTO_ERROR(H5E_DATASET, H5E_BADSELECT, FAIL, "unable to get type of selection")

        /* If the selection is NONE or POINTS, set the flag to FALSE */
        if(fsel_type == H5S_SEL_POINTS || fsel_type == H5S_SEL_NONE)
            sel_hyper_flag = FALSE;
        else
            sel_hyper_flag = TRUE;

        /* Check if file selection is a not a hyperslab selection */
        if(sel_hyper_flag) {
            /* Build the file selection for each chunk */
            if(H5D_create_chunk_file_map_hyper(fm, io_info) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to create file chunk selections")

            /* Clean file chunks' hyperslab span "scratch" information */
            curr_node = H5SL_first(fm->sel_chunks);
            while(curr_node) {
                H5D_chunk_info_t *chunk_info;   /* Pointer chunk information */

                /* Get pointer to chunk's information */
                chunk_info = (H5D_chunk_info_t *)H5SL_item(curr_node);
                HDassert(chunk_info);

                /* Clean hyperslab span's "scratch" information */
                if(H5S_hyper_reset_scratch(chunk_info->fspace) < 0)
                    HGOTO_ERROR(H5E_DATASET, H5E_CANTFREE, FAIL, "unable to reset span scratch info")

                /* Get the next chunk node in the skip list */
                curr_node = H5SL_next(curr_node);
            } /* end while */
        } /* end if */
        else {
            /* Create temporary datatypes for selection iteration */
            if((f_tid = H5I_register(H5I_DATATYPE, H5T_copy(dataset->shared->type, H5T_COPY_ALL))) < 0)
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTREGISTER, FAIL, "unable to register file datatype")

            /* Spaces might not be the same shape, iterate over the file selection directly */
            if(H5S_select_iterate(&bogus, f_tid, file_space,  H5D_chunk_file_cb, fm) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to create file chunk selections")

            /* Reset "last chunk" info */
            fm->last_index = (hsize_t)-1;
            fm->last_chunk_info = NULL;
        } /* end else */

        /* Build the memory selection for each chunk */
        if(sel_hyper_flag && H5S_select_shape_same(file_space, mem_space) == TRUE) {
            /* Reset chunk template information */
            fm->mchunk_tmpl = NULL;

            /* If the selections are the same shape, use the file chunk information
             * to generate the memory chunk information quickly.
             */
            if(H5D_create_chunk_mem_map_hyper(fm) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to create memory chunk selections")
        } /* end if */
        else {
            size_t elmt_size;           /* Memory datatype size */

            /* Make a copy of equivalent memory space */
            if((tmp_mspace = H5S_copy(mem_space, TRUE, FALSE)) == NULL)
                HGOTO_ERROR(H5E_DATASPACE, H5E_CANTCOPY, FAIL, "unable to copy memory space")

            /* De-select the mem space copy */
            if(H5S_select_none(tmp_mspace) < 0)
                HGOTO_ERROR(H5E_DATASPACE, H5E_CANTINIT, FAIL, "unable to de-select memory space")

            /* Save chunk template information */
            fm->mchunk_tmpl = tmp_mspace;

            /* Create temporary datatypes for selection iteration */
            if(f_tid < 0) {
                if((f_tid = H5I_register(H5I_DATATYPE, H5T_copy(dataset->shared->type, H5T_COPY_ALL))) < 0)
                    HGOTO_ERROR(H5E_DATATYPE, H5E_CANTREGISTER, FAIL, "unable to register file datatype")
            } /* end if */

            /* Create selection iterator for memory selection */
            if(0 == (elmt_size = H5T_get_size(mem_type)))
                HGOTO_ERROR(H5E_DATATYPE, H5E_BADSIZE, FAIL, "datatype size invalid")
            if(H5S_select_iter_init(&(fm->mem_iter), mem_space, elmt_size) < 0)
                HGOTO_ERROR(H5E_DATASPACE, H5E_CANTINIT, FAIL, "unable to initialize selection iterator")
            iter_init = TRUE;	/* Selection iteration info has been initialized */

            /* Spaces aren't the same shape, iterate over the memory selection directly */
            if(H5S_select_iterate(&bogus, f_tid, file_space, H5D_chunk_mem_cb, fm) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to create memory chunk selections")

            /* Clean up hyperslab stuff, if necessary */
            if(fm->msel_type != H5S_SEL_POINTS) {
                /* Clean memory chunks' hyperslab span "scratch" information */
                curr_node = H5SL_first(fm->sel_chunks);
                while(curr_node) {
                    H5D_chunk_info_t *chunk_info;   /* Pointer chunk information */

                    /* Get pointer to chunk's information */
                    chunk_info = (H5D_chunk_info_t *)H5SL_item(curr_node);
                    HDassert(chunk_info);

                    /* Clean hyperslab span's "scratch" information */
                    if(H5S_hyper_reset_scratch(chunk_info->mspace) < 0)
                        HGOTO_ERROR(H5E_DATASET, H5E_CANTFREE, FAIL, "unable to reset span scratch info")

                    /* Get the next chunk node in the skip list */
                    curr_node = H5SL_next(curr_node);
                } /* end while */
            } /* end if */
        } /* end else */
    } /* end else */

done:
    /* Release the [potentially partially built] chunk mapping information if an error occurs */
    if(ret_value < 0) {
        if(tmp_mspace && !fm->mchunk_tmpl) {
            if(H5S_close(tmp_mspace) < 0)
                HDONE_ERROR(H5E_DATASPACE, H5E_CANTRELEASE, FAIL, "can't release memory chunk dataspace template")
        } /* end if */

        if(H5D_chunk_io_term(fm) < 0)
            HDONE_ERROR(H5E_DATASPACE, H5E_CANTRELEASE, FAIL, "unable to release chunk mapping")
    } /* end if */

    /* Reset the global dataspace info */
    fm->file_space = NULL;
    fm->mem_space = NULL;

    if(iter_init) {
        if(H5S_SELECT_ITER_RELEASE(&(fm->mem_iter)) < 0)
            HDONE_ERROR(H5E_DATASPACE, H5E_CANTRELEASE, FAIL, "unable to release selection iterator")
    } /* end if */
    if(f_tid!=(-1)) {
        if(H5I_dec_ref(f_tid) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CANTFREE, FAIL, "Can't decrement temporary datatype ID")
    } /* end if */
    if(file_space_normalized) {
        if(H5S_hyper_denormalize_offset((H5S_t *)file_space, old_offset) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_BADSELECT, FAIL, "unable to normalize dataspace by offset")
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D_chunk_io_init() */


/*--------------------------------------------------------------------------
 NAME
    H5D_free_chunk_info
 PURPOSE
    Internal routine to destroy a chunk info node
 USAGE
    void H5D_free_chunk_info(chunk_info)
        void *chunk_info;    IN: Pointer to chunk info to destroy
 RETURNS
    No return value
 DESCRIPTION
    Releases all the memory for a chunk info node.  Called by H5SL_free
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static herr_t
H5D_free_chunk_info(void *item, void UNUSED *key, void UNUSED *opdata)
{
    H5D_chunk_info_t *chunk_info = (H5D_chunk_info_t *)item;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5D_free_chunk_info)

    HDassert(chunk_info);

    /* Close the chunk's file dataspace, if it's not shared */
    if(!chunk_info->fspace_shared)
        (void)H5S_close(chunk_info->fspace);
    else
        H5S_select_all(chunk_info->fspace, TRUE);

    /* Close the chunk's memory dataspace, if it's not shared */
    if(!chunk_info->mspace_shared)
        (void)H5S_close(chunk_info->mspace);

    /* Free the actual chunk info */
    H5FL_FREE(H5D_chunk_info_t, chunk_info);

    FUNC_LEAVE_NOAPI(0)
}   /* H5D_free_chunk_info() */


/*-------------------------------------------------------------------------
 * Function:	H5D_create_chunk_map_single
 *
 * Purpose:	Create chunk selections when appending a single record
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Tuesday, November 20, 2007
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_create_chunk_map_single(H5D_chunk_map_t *fm, const H5D_io_info_t
#ifndef H5_HAVE_PARALLEL
    UNUSED
#endif /* H5_HAVE_PARALLEL */
    *io_info)
{
    H5D_chunk_info_t *chunk_info;           /* Chunk information to insert into skip list */
    hsize_t     sel_start[H5O_LAYOUT_NDIMS]; /* Offset of low bound of file selection */
    hsize_t     sel_end[H5O_LAYOUT_NDIMS];  /* Offset of high bound of file selection */
    unsigned    u;                          /* Local index variable */
    herr_t	ret_value = SUCCEED;        /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5D_create_chunk_map_single)

    /* Sanity check */
    HDassert(fm->f_ndims > 0);

    /* Get coordinate for selection */
    if(H5S_SELECT_BOUNDS(fm->file_space, sel_start, sel_end) < 0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTGET, FAIL, "can't get file selection bound info")

    /* Initialize the 'single chunk' file & memory chunk information */
    chunk_info = fm->single_chunk_info;
    chunk_info->chunk_points = 1;

    /* Set chunk location & hyperslab size */
    for(u = 0; u < fm->f_ndims; u++) {
        HDassert(sel_start[u] == sel_end[u]);
        chunk_info->coords[u] = (sel_start[u] / fm->layout->u.chunk.dim[u]) * fm->layout->u.chunk.dim[u];
    } /* end for */
    chunk_info->coords[fm->f_ndims] = 0;

    /* Calculate the index of this chunk */
    if(H5V_chunk_index(fm->f_ndims, chunk_info->coords, fm->layout->u.chunk.dim, fm->down_chunks, &chunk_info->index) < 0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "can't get chunk index")

    /* Copy selection for file's dataspace into chunk dataspace */
    if(H5S_select_copy(fm->single_space, fm->file_space, FALSE) < 0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTCOPY, FAIL, "unable to copy file selection")

    /* Move selection back to have correct offset in chunk */
    if(H5S_SELECT_ADJUST_U(fm->single_space, chunk_info->coords) < 0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTSELECT, FAIL, "can't adjust chunk selection")

#ifdef H5_HAVE_PARALLEL
    /* store chunk selection information */
    if(io_info->using_mpi_vfd)
        fm->select_chunk[chunk_info->index] = chunk_info;
#endif /* H5_HAVE_PARALLEL */

    /* Set the file dataspace for the chunk to the shared 'single' dataspace */
    chunk_info->fspace = fm->single_space;

    /* Indicate that the chunk's file dataspace is shared */
    chunk_info->fspace_shared = TRUE;

    /* Just point at the memory dataspace & selection */
    /* (Casting away const OK -QAK) */
    chunk_info->mspace = (H5S_t *)fm->mem_space;

    /* Indicate that the chunk's memory dataspace is shared */
    chunk_info->mspace_shared = TRUE;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D_create_chunk_map_single() */


/*-------------------------------------------------------------------------
 * Function:	H5D_create_chunk_file_map_hyper
 *
 * Purpose:	Create all chunk selections in file.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Thursday, May 29, 2003
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_create_chunk_file_map_hyper(H5D_chunk_map_t *fm, const H5D_io_info_t
#ifndef H5_HAVE_PARALLEL
    UNUSED
#endif /* H5_HAVE_PARALLEL */
    *io_info)
{
    hsize_t     sel_start[H5O_LAYOUT_NDIMS];   /* Offset of low bound of file selection */
    hsize_t     sel_end[H5O_LAYOUT_NDIMS];   /* Offset of high bound of file selection */
    hsize_t     sel_points;                 /* Number of elements in file selection */
    hsize_t     start_coords[H5O_LAYOUT_NDIMS];   /* Starting coordinates of selection */
    hsize_t     coords[H5O_LAYOUT_NDIMS];   /* Current coordinates of chunk */
    hsize_t     end[H5O_LAYOUT_NDIMS];      /* Current coordinates of chunk */
    hsize_t     chunk_index;                /* Index of chunk */
    int         curr_dim;                   /* Current dimension to increment */
    unsigned    u;                          /* Local index variable */
    herr_t	ret_value = SUCCEED;        /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5D_create_chunk_file_map_hyper)

    /* Sanity check */
    assert(fm->f_ndims>0);

    /* Get number of elements selected in file */
    sel_points = fm->nelmts;

    /* Get bounding box for selection (to reduce the number of chunks to iterate over) */
    if(H5S_SELECT_BOUNDS(fm->file_space, sel_start, sel_end) < 0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTGET, FAIL, "can't get file selection bound info")

    /* Set initial chunk location & hyperslab size */

    for(u=0; u<fm->f_ndims; u++) {
        start_coords[u]=(sel_start[u]/fm->layout->u.chunk.dim[u])*fm->layout->u.chunk.dim[u];
        coords[u]=start_coords[u];
        end[u]=(coords[u]+fm->chunk_dim[u])-1;
    } /* end for */


    /* Calculate the index of this chunk */
    if(H5V_chunk_index(fm->f_ndims,coords,fm->layout->u.chunk.dim,fm->down_chunks,&chunk_index) < 0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "can't get chunk index")

    /* Iterate through each chunk in the dataset */
    while(sel_points) {
        /* Check for intersection of temporary chunk and file selection */
        /* (Casting away const OK - QAK) */
        if(H5S_hyper_intersect_block((H5S_t *)fm->file_space,coords,end)==TRUE) {
            H5S_t *tmp_fchunk;                  /* Temporary file dataspace */
            H5D_chunk_info_t *new_chunk_info;   /* chunk information to insert into skip list */
            hssize_t    schunk_points;          /* Number of elements in chunk selection */

            /* Create "temporary" chunk for selection operations (copy file space) */
            if((tmp_fchunk = H5S_copy(fm->file_space, TRUE, FALSE)) == NULL)
                HGOTO_ERROR(H5E_DATASPACE, H5E_CANTCOPY, FAIL, "unable to copy memory space")

            /* Make certain selections are stored in span tree form (not "optimized hyperslab" or "all") */
            if(H5S_hyper_convert(tmp_fchunk) < 0) {
                (void)H5S_close(tmp_fchunk);
                HGOTO_ERROR(H5E_DATASPACE, H5E_CANTINIT, FAIL, "unable to convert selection to span trees")
            } /* end if */

            /* "AND" temporary chunk and current chunk */
            if(H5S_select_hyperslab(tmp_fchunk,H5S_SELECT_AND,coords,NULL,fm->chunk_dim,NULL) < 0) {
                (void)H5S_close(tmp_fchunk);
                HGOTO_ERROR(H5E_DATASPACE, H5E_CANTSELECT, FAIL, "can't create chunk selection")
            } /* end if */

            /* Resize chunk's dataspace dimensions to size of chunk */
            if(H5S_set_extent_real(tmp_fchunk,fm->chunk_dim) < 0) {
                (void)H5S_close(tmp_fchunk);
                HGOTO_ERROR(H5E_DATASPACE, H5E_CANTSELECT, FAIL, "can't adjust chunk dimensions")
            } /* end if */

            /* Move selection back to have correct offset in chunk */
            if(H5S_SELECT_ADJUST_U(tmp_fchunk, coords) < 0) {
                (void)H5S_close(tmp_fchunk);
                HGOTO_ERROR(H5E_DATASPACE, H5E_CANTSELECT, FAIL, "can't adjust chunk selection")
            } /* end if */

            /* Add temporary chunk to the list of chunks */

            /* Allocate the file & memory chunk information */
            if (NULL==(new_chunk_info = H5FL_MALLOC (H5D_chunk_info_t))) {
                (void)H5S_close(tmp_fchunk);
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate chunk info")
            } /* end if */

            /* Initialize the chunk information */

            /* Set the chunk index */
            new_chunk_info->index=chunk_index;

#ifdef H5_HAVE_PARALLEL
            /* store chunk selection information */
            if(io_info->using_mpi_vfd)
                fm->select_chunk[chunk_index] = new_chunk_info;
#endif /* H5_HAVE_PARALLEL */

            /* Set the file chunk dataspace */
            new_chunk_info->fspace = tmp_fchunk;
            new_chunk_info->fspace_shared = FALSE;

            /* Set the memory chunk dataspace */
            new_chunk_info->mspace=NULL;
            new_chunk_info->mspace_shared = FALSE;

            /* Copy the chunk's coordinates */
            for(u=0; u<fm->f_ndims; u++)
                new_chunk_info->coords[u]=coords[u];
            new_chunk_info->coords[fm->f_ndims]=0;

            /* Insert the new chunk into the skip list */
            if(H5SL_insert(fm->sel_chunks,new_chunk_info,&new_chunk_info->index) < 0) {
                H5D_free_chunk_info(new_chunk_info,NULL,NULL);
                HGOTO_ERROR(H5E_DATASPACE,H5E_CANTINSERT,FAIL,"can't insert chunk into skip list")
            } /* end if */

            /* Get number of elements selected in chunk */
            if((schunk_points=H5S_GET_SELECT_NPOINTS(tmp_fchunk)) < 0)
                HGOTO_ERROR(H5E_DATASPACE, H5E_CANTGET, FAIL, "can't get file selection # of elements")
            H5_ASSIGN_OVERFLOW(new_chunk_info->chunk_points,schunk_points,hssize_t,size_t);

            /* Decrement # of points left in file selection */
            sel_points-=(hsize_t)schunk_points;

            /* Leave if we are done */
            if(sel_points==0)
                HGOTO_DONE(SUCCEED)
            assert(sel_points>0);
        } /* end if */

        /* Increment chunk index */
        chunk_index++;

        /* Set current increment dimension */
        curr_dim=(int)fm->f_ndims-1;

        /* Increment chunk location in fastest changing dimension */
        H5_CHECK_OVERFLOW(fm->chunk_dim[curr_dim],hsize_t,hssize_t);
        coords[curr_dim]+=fm->chunk_dim[curr_dim];
        end[curr_dim]+=fm->chunk_dim[curr_dim];

        /* Bring chunk location back into bounds, if necessary */
        if(coords[curr_dim]>sel_end[curr_dim]) {
            do {
                /* Reset current dimension's location to 0 */
                coords[curr_dim]=start_coords[curr_dim]; /*lint !e771 The start_coords will always be initialized */
                end[curr_dim]=(coords[curr_dim]+(hssize_t)fm->chunk_dim[curr_dim])-1;

                /* Decrement current dimension */
                curr_dim--;

                /* Increment chunk location in current dimension */
                coords[curr_dim]+=fm->chunk_dim[curr_dim];
                end[curr_dim]=(coords[curr_dim]+fm->chunk_dim[curr_dim])-1;
            } while(coords[curr_dim]>sel_end[curr_dim]);

            /* Re-Calculate the index of this chunk */
            if(H5V_chunk_index(fm->f_ndims,coords,fm->layout->u.chunk.dim,fm->down_chunks,&chunk_index) < 0)
                HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "can't get chunk index")
        } /* end if */
    } /* end while */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D_create_chunk_file_map_hyper() */


/*-------------------------------------------------------------------------
 * Function:	H5D_create_chunk_mem_map_hyper
 *
 * Purpose:	Create all chunk selections in memory by copying the file
 *              chunk selections and adjusting their offsets to be correct
 *              for the memory.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Thursday, May 29, 2003
 *
 * Assumptions: That the file and memory selections are the same shape.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_create_chunk_mem_map_hyper(const H5D_chunk_map_t *fm)
{
    H5SL_node_t *curr_node;                 /* Current node in skip list */
    hsize_t    file_sel_start[H5O_LAYOUT_NDIMS];    /* Offset of low bound of file selection */
    hsize_t    file_sel_end[H5O_LAYOUT_NDIMS];  /* Offset of high bound of file selection */
    hsize_t    mem_sel_start[H5O_LAYOUT_NDIMS]; /* Offset of low bound of file selection */
    hsize_t    mem_sel_end[H5O_LAYOUT_NDIMS];   /* Offset of high bound of file selection */
    hssize_t adjust[H5O_LAYOUT_NDIMS];      /* Adjustment to make to all file chunks */
    hssize_t chunk_adjust[H5O_LAYOUT_NDIMS];  /* Adjustment to make to a particular chunk */
    unsigned    u;                          /* Local index variable */
    herr_t	ret_value = SUCCEED;        /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5D_create_chunk_mem_map_hyper)

    /* Sanity check */
    assert(fm->f_ndims>0);

    /* Check for all I/O going to a single chunk */
    if(H5SL_count(fm->sel_chunks)==1) {
        H5D_chunk_info_t *chunk_info;   /* Pointer to chunk information */

        /* Get the node */
        curr_node=H5SL_first(fm->sel_chunks);

        /* Get pointer to chunk's information */
        chunk_info = (H5D_chunk_info_t *)H5SL_item(curr_node);
        assert(chunk_info);

        /* Just point at the memory dataspace & selection */
        /* (Casting away const OK -QAK) */
        chunk_info->mspace=(H5S_t *)fm->mem_space;

        /* Indicate that the chunk's memory space is shared */
        chunk_info->mspace_shared = TRUE;
    } /* end if */
    else {
        /* Get bounding box for file selection */
        if(H5S_SELECT_BOUNDS(fm->file_space, file_sel_start, file_sel_end) < 0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTGET, FAIL, "can't get file selection bound info")

        /* Get bounding box for memory selection */
        if(H5S_SELECT_BOUNDS(fm->mem_space, mem_sel_start, mem_sel_end) < 0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTGET, FAIL, "can't get file selection bound info")

        /* Calculate the adjustment for memory selection from file selection */
        assert(fm->m_ndims==fm->f_ndims);
        for(u=0; u<fm->f_ndims; u++) {
            H5_CHECK_OVERFLOW(file_sel_start[u],hsize_t,hssize_t);
            H5_CHECK_OVERFLOW(mem_sel_start[u],hsize_t,hssize_t);
            adjust[u]=(hssize_t)file_sel_start[u]-(hssize_t)mem_sel_start[u];
        } /* end for */

        /* Iterate over each chunk in the chunk list */
        curr_node=H5SL_first(fm->sel_chunks);
        while(curr_node) {
            H5D_chunk_info_t *chunk_info;   /* Pointer to chunk information */

            /* Get pointer to chunk's information */
            chunk_info = (H5D_chunk_info_t *)H5SL_item(curr_node);
            assert(chunk_info);

            /* Copy the information */

            /* Copy the memory dataspace */
            if((chunk_info->mspace = H5S_copy(fm->mem_space, TRUE, FALSE)) == NULL)
                HGOTO_ERROR(H5E_DATASPACE, H5E_CANTCOPY, FAIL, "unable to copy memory space")

            /* Release the current selection */
            if(H5S_SELECT_RELEASE(chunk_info->mspace) < 0)
                HGOTO_ERROR(H5E_DATASPACE, H5E_CANTRELEASE, FAIL, "unable to release selection")

            /* Copy the file chunk's selection */
            if(H5S_select_copy(chunk_info->mspace,chunk_info->fspace,FALSE) < 0)
                HGOTO_ERROR(H5E_DATASPACE, H5E_CANTCOPY, FAIL, "unable to copy selection")

            /* Compensate for the chunk offset */
            for(u=0; u<fm->f_ndims; u++) {
                H5_CHECK_OVERFLOW(chunk_info->coords[u],hsize_t,hssize_t);
                chunk_adjust[u]=adjust[u]-(hssize_t)chunk_info->coords[u]; /*lint !e771 The adjust array will always be initialized */
            } /* end for */

            /* Adjust the selection */
            if(H5S_hyper_adjust_s(chunk_info->mspace,chunk_adjust) < 0) /*lint !e772 The chunk_adjust array will always be initialized */
                HGOTO_ERROR(H5E_DATASPACE, H5E_CANTSELECT, FAIL, "can't adjust chunk selection")

            /* Get the next chunk node in the skip list */
            curr_node=H5SL_next(curr_node);
        } /* end while */
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D_create_chunk_mem_map_hyper() */


/*-------------------------------------------------------------------------
 * Function:	H5D_chunk_file_cb
 *
 * Purpose:	Callback routine for file selection iterator.  Used when
 *              creating selections in file for each point selected.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Wednesday, July 23, 2003
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_chunk_file_cb(void UNUSED *elem, hid_t UNUSED type_id, unsigned ndims, const hsize_t *coords, void *_fm)
{
    H5D_chunk_map_t      *fm = (H5D_chunk_map_t *)_fm;  /* File<->memory chunk mapping info */
    H5D_chunk_info_t *chunk_info;               /* Chunk information for current chunk */
    hsize_t    coords_in_chunk[H5O_LAYOUT_NDIMS];        /* Coordinates of element in chunk */
    hsize_t     chunk_index;                    /* Chunk index */
    unsigned    u;                              /* Local index variable */
    herr_t	ret_value = SUCCEED;            /* Return value		*/

    FUNC_ENTER_NOAPI_NOINIT(H5D_chunk_file_cb)

    /* Calculate the index of this chunk */
    if(H5V_chunk_index(ndims,coords,fm->layout->u.chunk.dim,fm->down_chunks,&chunk_index) < 0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "can't get chunk index")

    /* Find correct chunk in file & memory skip list */
    if(chunk_index==fm->last_index) {
        /* If the chunk index is the same as the last chunk index we used,
         * get the cached info to operate on.
         */
        chunk_info=fm->last_chunk_info;
    } /* end if */
    else {
        /* If the chunk index is not the same as the last chunk index we used,
         * find the chunk in the skip list.
         */
        /* Get the chunk node from the skip list */
        if(NULL == (chunk_info = (H5D_chunk_info_t *)H5SL_search(fm->sel_chunks, &chunk_index))) {
            H5S_t *fspace;                      /* Memory chunk's dataspace */

            /* Allocate the file & memory chunk information */
            if (NULL==(chunk_info = H5FL_MALLOC (H5D_chunk_info_t)))
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate chunk info")

            /* Initialize the chunk information */

            /* Set the chunk index */
            chunk_info->index=chunk_index;

            /* Create a dataspace for the chunk */
            if((fspace = H5S_create_simple(fm->f_ndims,fm->chunk_dim,NULL))==NULL) {
                H5FL_FREE(H5D_chunk_info_t,chunk_info);
                HGOTO_ERROR(H5E_DATASPACE, H5E_CANTCREATE, FAIL, "unable to create dataspace for chunk")
            } /* end if */

            /* De-select the chunk space */
            if(H5S_select_none(fspace) < 0) {
                (void)H5S_close(fspace);
                H5FL_FREE(H5D_chunk_info_t,chunk_info);
                HGOTO_ERROR(H5E_DATASPACE, H5E_CANTINIT, FAIL, "unable to de-select dataspace")
            } /* end if */

            /* Set the file chunk dataspace */
            chunk_info->fspace = fspace;
            chunk_info->fspace_shared = FALSE;

            /* Set the memory chunk dataspace */
            chunk_info->mspace=NULL;
            chunk_info->mspace_shared = FALSE;

            /* Set the number of selected elements in chunk to zero */
            chunk_info->chunk_points=0;

            /* Compute the chunk's coordinates */
            for(u=0; u<fm->f_ndims; u++) {
                H5_CHECK_OVERFLOW(fm->layout->u.chunk.dim[u],hsize_t,hssize_t);
                chunk_info->coords[u]=(coords[u]/(hssize_t)fm->layout->u.chunk.dim[u])*(hssize_t)fm->layout->u.chunk.dim[u];
            } /* end for */
            chunk_info->coords[fm->f_ndims]=0;

            /* Insert the new chunk into the skip list */
            if(H5SL_insert(fm->sel_chunks,chunk_info,&chunk_info->index) < 0) {
                H5D_free_chunk_info(chunk_info,NULL,NULL);
                HGOTO_ERROR(H5E_DATASPACE,H5E_CANTINSERT,FAIL,"can't insert chunk into skip list")
            } /* end if */
        } /* end if */

        /* Update the "last chunk seen" information */
        fm->last_index=chunk_index;
        fm->last_chunk_info=chunk_info;
    } /* end else */

    /* Get the coordinates of the element in the chunk */
    for(u=0; u<fm->f_ndims; u++)
        coords_in_chunk[u]=coords[u]%fm->layout->u.chunk.dim[u];

    /* Add point to file selection for chunk */
    if(H5S_select_elements(chunk_info->fspace, H5S_SELECT_APPEND, (size_t)1, coords_in_chunk) < 0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTSELECT, FAIL, "unable to select element")

    /* Increment the number of elemented selected in chunk */
    chunk_info->chunk_points++;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D_chunk_file_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5D_chunk_mem_cb
 *
 * Purpose:	Callback routine for file selection iterator.  Used when
 *              creating selections in memory for each chunk.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *		Thursday, April 10, 2003
 *
 *-------------------------------------------------------------------------
 */
/* ARGSUSED */
static herr_t
H5D_chunk_mem_cb(void UNUSED *elem, hid_t UNUSED type_id, unsigned ndims, const hsize_t *coords, void *_fm)
{
    H5D_chunk_map_t      *fm = (H5D_chunk_map_t *)_fm;  /* File<->memory chunk mapping info */
    H5D_chunk_info_t *chunk_info;               /* Chunk information for current chunk */
    hsize_t    coords_in_mem[H5O_LAYOUT_NDIMS];        /* Coordinates of element in memory */
    hsize_t     chunk_index;                    /* Chunk index */
    herr_t	ret_value = SUCCEED;            /* Return value		*/

    FUNC_ENTER_NOAPI_NOINIT(H5D_chunk_mem_cb)

    /* Calculate the index of this chunk */
    if(H5V_chunk_index(ndims,coords,fm->layout->u.chunk.dim,fm->down_chunks,&chunk_index) < 0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "can't get chunk index")

    /* Find correct chunk in file & memory skip list */
    if(chunk_index==fm->last_index) {
        /* If the chunk index is the same as the last chunk index we used,
         * get the cached spaces to operate on.
         */
        chunk_info=fm->last_chunk_info;
    } /* end if */
    else {
        /* If the chunk index is not the same as the last chunk index we used,
         * find the chunk in the skip list.
         */
        /* Get the chunk node from the skip list */
        if(NULL == (chunk_info = (H5D_chunk_info_t *)H5SL_search(fm->sel_chunks, &chunk_index)))
            HGOTO_ERROR(H5E_DATASPACE, H5E_NOTFOUND, FAIL, "can't locate chunk in skip list")

        /* Check if the chunk already has a memory space */
        if(chunk_info->mspace==NULL) {
            /* Copy the template memory chunk dataspace */
            if((chunk_info->mspace = H5S_copy(fm->mchunk_tmpl, FALSE, FALSE)) == NULL)
                HGOTO_ERROR(H5E_DATASPACE, H5E_CANTCOPY, FAIL, "unable to copy file space")
        } /* end else */

        /* Update the "last chunk seen" information */
        fm->last_index=chunk_index;
        fm->last_chunk_info=chunk_info;
    } /* end else */

    /* Get coordinates of selection iterator for memory */
    if(H5S_SELECT_ITER_COORDS(&fm->mem_iter,coords_in_mem) < 0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTGET, FAIL, "unable to get iterator coordinates")

    /* Add point to memory selection for chunk */
    if(fm->msel_type==H5S_SEL_POINTS) {
        if(H5S_select_elements(chunk_info->mspace, H5S_SELECT_APPEND, (size_t)1, coords_in_mem) < 0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTSELECT, FAIL, "unable to select element")
    } /* end if */
    else {
        if(H5S_hyper_add_span_element(chunk_info->mspace, fm->m_ndims, coords_in_mem) < 0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTSELECT, FAIL, "unable to select element")
    } /* end else */

    /* Move memory selection iterator to next element in selection */
    if(H5S_SELECT_ITER_NEXT(&fm->mem_iter, (size_t)1) < 0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTNEXT, FAIL, "unable to move to next iterator location")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D_chunk_mem_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5D_chunk_cacheable
 *
 * Purpose:	A small internal function to if it's possible to load the
 *              chunk into cache.
 *
 * Return:	TRUE or FALSE
 *
 * Programmer:	Raymond Lu
 *		17 July 2007
 *
 *-------------------------------------------------------------------------
 */
hbool_t 
H5D_chunk_cacheable(const H5D_io_info_t *io_info, haddr_t caddr)
{
    const H5D_t *dataset = io_info->dset;
    hbool_t ret_value;
 
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5D_chunk_cacheable)

    HDassert(io_info);
    HDassert(dataset);

    /* Must bring the whole chunk in if there are any filters */
    if(dataset->shared->dcpl_cache.pline.nused > 0)
        ret_value = TRUE;
    else
#ifdef H5_HAVE_PARALLEL
         /* If MPI based VFD is used and the file is opened for write access, must
          *         bypass the chunk-cache scheme because other MPI processes could
          *         be writing to other elements in the same chunk.  Do a direct
          *         write-through of only the elements requested.
          */
        if(io_info->using_mpi_vfd && (H5F_ACC_RDWR & H5F_INTENT(dataset->oloc.file)))
            ret_value = FALSE;
        else
#endif /* H5_HAVE_PARALLEL */
            /* If the chunk is too large to keep in the cache and if the address
             * for the chunk has been defined, then don't load the chunk into the
             * cache, just write the data to it directly.
             */
            if(dataset->shared->layout.u.chunk.size > dataset->shared->cache.chunk.nbytes
                    && H5F_addr_defined(caddr))
                ret_value = FALSE;
            else
                ret_value = TRUE;

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D_chunk_cacheable() */


/*-------------------------------------------------------------------------
 * Function:	H5D_chunk_in_cache
 *
 * Purpose:	Check if a chunk is in the cache.
 *
 * Return:	TRUE or FALSE
 *
 * Programmer:	Quincey Koziol
 *		1 April 2008
 *
 *-------------------------------------------------------------------------
 */
static hbool_t 
H5D_chunk_in_cache(const H5D_io_info_t *io_info)
{
    H5D_rdcc_t	*rdcc = &(io_info->dset->shared->cache.chunk);/*raw data chunk cache*/
    hbool_t	found = FALSE;		/*already in cache?	*/
 
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5D_chunk_in_cache)

    HDassert(io_info);

    /* Check if the chunk is in the cache (but hasn't been written to disk yet) */
    if(rdcc->nslots > 0) {
        unsigned idx = H5D_CHUNK_HASH(io_info->dset->shared, io_info->store->chunk.index); /* Cache entry index */
        H5D_rdcc_ent_t	*ent = rdcc->slot[idx]; /* Cache entry */

        /* Potential match... */
        if(ent) {
            size_t u;           /* Local index variable */

            for(u = 0, found = TRUE; u < io_info->dset->shared->layout.u.chunk.ndims; u++) {
                if(io_info->store->chunk.offset[u] != ent->offset[u]) {
                    found = FALSE;
                    break;
                } /* end if */
            } /* end for */
        } /* end if */
    } /* end if */

    FUNC_LEAVE_NOAPI(found)
} /* end H5D_chunk_in_cache() */


/*-------------------------------------------------------------------------
 * Function:	H5D_chunk_read
 *
 * Purpose:	Read from a chunked dataset.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *		Thursday, April 10, 2003
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_chunk_read(H5D_io_info_t *io_info, const H5D_type_info_t *type_info,
    hsize_t UNUSED nelmts, const H5S_t UNUSED *file_space, const H5S_t UNUSED *mem_space,
    H5D_chunk_map_t *fm)
{
    H5SL_node_t *chunk_node;            /* Current node in chunk skip list */
    H5D_io_info_t nul_io_info;          /* "null" I/O info object */
    H5D_io_info_t ctg_io_info;          /* Contiguous I/O info object */
    H5D_storage_t ctg_store;            /* Chunk storage information as contiguous dataset */
    H5D_io_info_t cpt_io_info;          /* Compact I/O info object */
    H5D_storage_t cpt_store;            /* Chunk storage information as compact dataset */
    hbool_t     cpt_dirty;              /* Temporary placeholder for compact storage "dirty" flag */
    size_t      src_accessed_bytes = 0; /* Total accessed size in a chunk */
    hbool_t     skip_missing_chunks = FALSE;    /* Whether to skip missing chunks */
    unsigned    idx_hint = 0;           /* Cache index hint      */
    herr_t	ret_value = SUCCEED;	/*return value		*/

    FUNC_ENTER_NOAPI_NOINIT(H5D_chunk_read)

    /* Sanity check */
    HDassert(io_info);
    HDassert(io_info->u.rbuf);
    HDassert(type_info);
    HDassert(fm);

    /* Set up "null" I/O info object */
    HDmemcpy(&nul_io_info, io_info, sizeof(nul_io_info));
    nul_io_info.layout_ops = *H5D_LOPS_NULL;

    /* Set up contiguous I/O info object */
    HDmemcpy(&ctg_io_info, io_info, sizeof(ctg_io_info));
    ctg_io_info.store = &ctg_store;
    ctg_io_info.layout_ops = *H5D_LOPS_CONTIG;

    /* Initialize temporary contiguous storage info */
    ctg_store.contig.dset_size = (hsize_t)io_info->dset->shared->layout.u.chunk.size;

    /* Set up compact I/O info object */
    HDmemcpy(&cpt_io_info, io_info, sizeof(cpt_io_info));
    cpt_io_info.store = &cpt_store;
    cpt_io_info.layout_ops = *H5D_LOPS_COMPACT;

    /* Initialize temporary compact storage info */
    cpt_store.compact.dirty = &cpt_dirty;

    {
        const H5O_fill_t *fill = &(io_info->dset->shared->dcpl_cache.fill);    /* Fill value info */
        H5D_fill_value_t fill_status;       /* Fill value status */

        /* Check the fill value status */
        if(H5P_is_fill_value_defined(fill, &fill_status) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't tell if fill value defined")

        /* If we are never to return fill values, or if we would return them
         * but they aren't set, set the flag to skip missing chunks.
         */
        if(fill->fill_time == H5D_FILL_TIME_NEVER ||
                (fill->fill_time == H5D_FILL_TIME_IFSET && fill_status != H5D_FILL_VALUE_USER_DEFINED))
            skip_missing_chunks = TRUE;
    }

    /* Iterate through nodes in chunk skip list */
    chunk_node = H5D_CHUNK_GET_FIRST_NODE(fm);
    while(chunk_node) {
        H5D_chunk_info_t *chunk_info;   /* Chunk information */
        H5D_io_info_t *chk_io_info;     /* Pointer to I/O info object for this chunk */
        void *chunk;                    /* Pointer to locked chunk buffer */
        haddr_t	chunk_addr;             /* Chunk address on disk */
        H5D_istore_ud1_t udata;		/* B-tree pass-through	*/

        /* Get the actual chunk information from the skip list node */
        chunk_info = H5D_CHUNK_GET_NODE_INFO(fm, chunk_node);

        /* Pass in chunk's coordinates in a union. */
        io_info->store->chunk.offset = chunk_info->coords;
        io_info->store->chunk.index = chunk_info->index;

        /* Get the address of the chunk in the file */
        chunk_addr = H5D_istore_get_addr(io_info, &udata);

        /* Check for non-existant chunk & skip it if appropriate */
        if(!H5F_addr_defined(chunk_addr) && !H5D_chunk_in_cache(io_info)
                && skip_missing_chunks) {
            /* No chunk cached */
            chunk = NULL;

            /* Point I/O info at "null" I/O info for this chunk */
            chk_io_info = &nul_io_info;
        } /* end if */
        else {
            /* Load the chunk into cache and lock it. */
            if(H5D_chunk_cacheable(io_info, chunk_addr)) {
                /* Compute # of bytes accessed in chunk */
                src_accessed_bytes = chunk_info->chunk_points * type_info->src_type_size;

                /* Lock the chunk into the cache */
                if(NULL == (chunk = H5D_istore_lock(io_info, &udata, FALSE, &idx_hint)))
                    HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, "unable to read raw data chunk")

                /* Set up the storage buffer information for this chunk */
                cpt_store.compact.buf = chunk;

                /* Point I/O info at contiguous I/O info for this chunk */
                chk_io_info = &cpt_io_info;
            } /* end if */
            else {
                /* Sanity check */
                HDassert(H5F_addr_defined(chunk_addr));

                /* Set up the storage address information for this chunk */
                ctg_store.contig.dset_addr = chunk_addr;

                /* No chunk cached */
                chunk = NULL;

                /* Point I/O info at temporary I/O info for this chunk */
                chk_io_info = &ctg_io_info;
            } /* end else */
        } /* end else */

        /* Perform the actual read operation */
        if((io_info->io_ops.single_read)(chk_io_info, type_info,
                (hsize_t)chunk_info->chunk_points, chunk_info->fspace, chunk_info->mspace) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "chunked read failed")

        /* Release the cache lock on the chunk. */
        if(chunk && H5D_istore_unlock(io_info, FALSE, idx_hint, chunk, src_accessed_bytes) < 0)
            HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, "unable to unlock raw data chunk")

        /* Advance to next chunk in list */
        chunk_node = H5D_CHUNK_GET_NEXT_NODE(fm, chunk_node);
    } /* end while */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5D_chunk_read() */


/*-------------------------------------------------------------------------
 * Function:	H5D_chunk_write
 *
 * Purpose:	Writes to a chunked dataset.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *		Thursday, April 10, 2003
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_chunk_write(H5D_io_info_t *io_info, const H5D_type_info_t *type_info,
    hsize_t UNUSED nelmts, const H5S_t UNUSED *file_space, const H5S_t UNUSED *mem_space,
    H5D_chunk_map_t *fm)
{
    H5SL_node_t *chunk_node;            /* Current node in chunk skip list */
    H5D_io_info_t ctg_io_info;          /* Contiguous I/O info object */
    H5D_storage_t ctg_store;            /* Chunk storage information as contiguous dataset */
    H5D_io_info_t cpt_io_info;          /* Compact I/O info object */
    H5D_storage_t cpt_store;            /* Chunk storage information as compact dataset */
    hbool_t     cpt_dirty;              /* Temporary placeholder for compact storage "dirty" flag */
    size_t      dst_accessed_bytes = 0; /* Total accessed size in a chunk */
    unsigned    idx_hint = 0;           /* Cache index hint      */
    herr_t	ret_value = SUCCEED;	/* Return value		*/

    FUNC_ENTER_NOAPI_NOINIT(H5D_chunk_write)

    /* Sanity check */
    HDassert(io_info);
    HDassert(io_info->u.wbuf);
    HDassert(type_info);
    HDassert(fm);

    /* Set up contiguous I/O info object */
    HDmemcpy(&ctg_io_info, io_info, sizeof(ctg_io_info));
    ctg_io_info.store = &ctg_store;
    ctg_io_info.layout_ops = *H5D_LOPS_CONTIG;

    /* Initialize temporary contiguous storage info */
    ctg_store.contig.dset_size = (hsize_t)io_info->dset->shared->layout.u.chunk.size;

    /* Set up compact I/O info object */
    HDmemcpy(&cpt_io_info, io_info, sizeof(cpt_io_info));
    cpt_io_info.store = &cpt_store;
    cpt_io_info.layout_ops = *H5D_LOPS_COMPACT;

    /* Initialize temporary compact storage info */
    cpt_store.compact.dirty = &cpt_dirty;

    /* Iterate through nodes in chunk skip list */
    chunk_node = H5D_CHUNK_GET_FIRST_NODE(fm);
    while(chunk_node) {
        H5D_chunk_info_t *chunk_info;   /* Chunk information */
        H5D_io_info_t *chk_io_info;     /* Pointer to I/O info object for this chunk */
        void *chunk;                    /* Pointer to locked chunk buffer */
        haddr_t	chunk_addr;             /* Chunk address on disk */
        H5D_istore_ud1_t udata;		/* B-tree pass-through	*/

        /* Get the actual chunk information from the skip list node */
        chunk_info = H5D_CHUNK_GET_NODE_INFO(fm, chunk_node);

        /* Pass in chunk's coordinates in a union. */
        io_info->store->chunk.offset = chunk_info->coords;
        io_info->store->chunk.index = chunk_info->index;

        /* Load the chunk into cache.  But if the whole chunk is written,
         * simply allocate space instead of load the chunk. */
        chunk_addr = H5D_istore_get_addr(io_info, &udata);
        if(H5D_chunk_cacheable(io_info, chunk_addr)) {
            hbool_t entire_chunk = TRUE;       /* Whether whole chunk is selected */

            /* Compute # of bytes accessed in chunk */
            dst_accessed_bytes = chunk_info->chunk_points * type_info->dst_type_size;

            /* Determine if we will access all the data in the chunk */
            if(dst_accessed_bytes != ctg_store.contig.dset_size ||
                    (chunk_info->chunk_points * type_info->src_type_size) != ctg_store.contig.dset_size)
                entire_chunk = FALSE;

            /* Lock the chunk into the cache */
            if(NULL == (chunk = H5D_istore_lock(io_info, &udata, entire_chunk, &idx_hint)))
                HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, "unable to read raw data chunk")

            /* Set up the storage buffer information for this chunk */
            cpt_store.compact.buf = chunk;

            /* Point I/O info at main I/O info for this chunk */
            chk_io_info = &cpt_io_info;
        } /* end if */
        else {
            /* Sanity check */
            HDassert(H5F_addr_defined(chunk_addr));

            /* Set up the storage address information for this chunk */
            ctg_store.contig.dset_addr = chunk_addr;

            /* No chunk cached */
            chunk = NULL;

            /* Point I/O info at temporary I/O info for this chunk */
            chk_io_info = &ctg_io_info;
        } /* end else */

        /* Perform the actual write operation */
        if((io_info->io_ops.single_write)(chk_io_info, type_info,
                (hsize_t)chunk_info->chunk_points, chunk_info->fspace, chunk_info->mspace) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "chunked write failed")

        /* Release the cache lock on the chunk. */
        if(chunk && H5D_istore_unlock(io_info, TRUE, idx_hint, chunk, dst_accessed_bytes) < 0)
            HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, "unable to unlock raw data chunk")

        /* Advance to next chunk in list */
        chunk_node = H5D_CHUNK_GET_NEXT_NODE(fm, chunk_node);
    } /* end while */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5D_chunk_write() */


/*-------------------------------------------------------------------------
 * Function:	H5D_chunk_io_term
 *
 * Purpose:	Destroy I/O operation information.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Saturday, May 17, 2003
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_chunk_io_term(const H5D_chunk_map_t *fm)
{
    herr_t	ret_value = SUCCEED;	/*return value		*/

    FUNC_ENTER_NOAPI_NOINIT(H5D_chunk_io_term)

    /* Single element I/O vs. multiple element I/O cleanup */
    if(fm->use_single) {
        /* Sanity checks */
        HDassert(fm->sel_chunks == NULL);
        HDassert(fm->single_chunk_info);
        HDassert(fm->single_chunk_info->fspace_shared);
        HDassert(fm->single_chunk_info->mspace_shared);

        /* Reset the selection for the single element I/O */
        H5S_select_all(fm->single_space, TRUE);
    } /* end if */
    else {
        /* Release the nodes on the list of selected chunks */
        if(fm->sel_chunks)
            if(H5SL_free(fm->sel_chunks, H5D_free_chunk_info, NULL) < 0)
                HGOTO_ERROR(H5E_PLIST, H5E_CANTNEXT, FAIL, "can't iterate over chunks")
    } /* end else */

    /* Free the memory chunk dataspace template */
    if(fm->mchunk_tmpl)
        if(H5S_close(fm->mchunk_tmpl) < 0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTRELEASE, FAIL, "can't release memory chunk dataspace template")
#ifdef H5_HAVE_PARALLEL
    if(fm->select_chunk)
        H5MM_xfree(fm->select_chunk);
#endif /* H5_HAVE_PARALLEL */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D_chunk_io_term() */


/*-------------------------------------------------------------------------
 * Function:	H5D_null_readvv
 *
 * Purpose:	Performs "no-op" I/O operation, advancing through two I/O
 *              vectors, until one runs out.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Tuesday, April  1, 2008
 *
 *-------------------------------------------------------------------------
 */
static ssize_t
H5D_null_readvv(const H5D_io_info_t UNUSED *io_info,
    size_t chunk_max_nseq, size_t *chunk_curr_seq, size_t chunk_len_arr[], hsize_t chunk_offset_arr[],
    size_t mem_max_nseq, size_t *mem_curr_seq, size_t mem_len_arr[], hsize_t mem_offset_arr[])
{
    size_t u, v;                /* Local index variables */
    size_t size;                /* Size of sequence in bytes */
    ssize_t bytes_processed = 0; /* Eventual return value */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5D_null_readvv)

    /* Check args */
    HDassert(chunk_len_arr);
    HDassert(chunk_offset_arr);
    HDassert(mem_len_arr);
    HDassert(mem_offset_arr);

    /* Work through all the sequences */
    for(u = *mem_curr_seq, v = *chunk_curr_seq; u < mem_max_nseq && v < chunk_max_nseq; ) {
        /* Choose smallest buffer to write */
        if(chunk_len_arr[v] < mem_len_arr[u])
            size = chunk_len_arr[v];
        else
            size = mem_len_arr[u];

        /* Update source information */
        chunk_len_arr[v] -= size;
        chunk_offset_arr[v] += size;
        if(chunk_len_arr[v] == 0)
            v++;

        /* Update destination information */
        mem_len_arr[u] -= size;
        mem_offset_arr[u] += size;
        if(mem_len_arr[u] == 0)
            u++;

        /* Increment number of bytes copied */
        bytes_processed += (ssize_t)size;
    } /* end for */

    /* Update current sequence vectors */
    *mem_curr_seq = u;
    *chunk_curr_seq = v;

    FUNC_LEAVE_NOAPI(bytes_processed)
} /* H5D_null_readvv() */

