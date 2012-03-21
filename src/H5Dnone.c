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

/* Programmer: 	Vailin Choi <vchoi@hdfgroup.org>
 *	       	September 2010
 *
 * Purpose:	Non Index chunked I/O functions.  
 *		This is used when the dataset is:
 *			extendible but with fixed max. dims
 *			with early allocation
 *			without filter
 *		The chunk coordinate is mapped into the actual disk addresses
 *		for the chunk without indexing.
 *
 */

/****************/
/* Module Setup */
/****************/

#define H5D_PACKAGE		/*suppress error about including H5Dpkg	  */


/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Dpkg.h"		/* Datasets				*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5FLprivate.h"	/* Free Lists                           */
#include "H5MFprivate.h"	/* File space management		*/
#include "H5Vprivate.h"         /* Vector functions			*/


/****************/
/* Local Macros */
/****************/

/******************/
/* Local Typedefs */
/******************/

/********************/
/* Local Prototypes */
/********************/

/* Non Index chunking I/O ops */
static herr_t H5D_none_create(const H5D_chk_idx_info_t *idx_info);
static hbool_t H5D_none_is_space_alloc(const H5O_storage_chunk_t *storage);
static herr_t H5D_none_insert(const H5D_chk_idx_info_t *idx_info,
    H5D_chunk_ud_t *udata);
static herr_t H5D_none_get_addr(const H5D_chk_idx_info_t *idx_info,
    H5D_chunk_ud_t *udata);
static int H5D_none_iterate(const H5D_chk_idx_info_t *idx_info,
    H5D_chunk_cb_func_t chunk_cb, void *chunk_udata);
static herr_t H5D_none_remove(const H5D_chk_idx_info_t *idx_info,
    H5D_chunk_common_ud_t *udata);
static herr_t H5D_none_delete(const H5D_chk_idx_info_t *idx_info);
static herr_t H5D_none_copy_setup(const H5D_chk_idx_info_t *idx_info_src,
    const H5D_chk_idx_info_t *idx_info_dst);
static herr_t H5D_none_size(const H5D_chk_idx_info_t *idx_info,
    hsize_t *size);
static herr_t H5D_none_reset(H5O_storage_chunk_t *storage, hbool_t reset_addr);
static herr_t H5D_none_dump(const H5O_storage_chunk_t *storage, FILE *stream);

/*********************/
/* Package Variables */
/*********************/

/* Non Index chunk I/O ops */
const H5D_chunk_ops_t H5D_COPS_NONE[1] = {{
    FALSE,                      /* Non-indexed chunking don't current support SWMR access */
    NULL,			/* init */
    H5D_none_create,		/* create */
    H5D_none_is_space_alloc, 	/* is_space_alloc */
    H5D_none_insert,		/* insert */
    H5D_none_get_addr,		/* get_addr */
    NULL,			/* resize */
    H5D_none_iterate,		/* iterate */
    H5D_none_remove,		/* remove */
    H5D_none_delete,		/* delete */
    H5D_none_copy_setup,	/* copy_setup */
    NULL,			/* copy_shutdown */
    H5D_none_size,		/* size */
    H5D_none_reset,		/* reset */
    NULL,			/* support */
    NULL,			/* unsupport */
    H5D_none_dump,		/* dump */
    NULL			/* dest */
}};

/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/


/*-------------------------------------------------------------------------
 * Function:	H5D_none_create
 *
 * Purpose:	Allocate memory for the maximum # of chunks in the dataset.
 *		
 * Return:	Non-negative on success 
 *		Negative on failure.
 *
 * Programmer:	Vailin Choi; September 2010
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_none_create(const H5D_chk_idx_info_t *idx_info)
{
    hsize_t 	nbytes;                 /* Total size of dataset chunks */
    haddr_t	addr;			/* The address of dataset chunks */
    herr_t 	ret_value = SUCCEED; 	/* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Check args */
    HDassert(idx_info);
    HDassert(idx_info->f);
    HDassert(idx_info->pline);
    HDassert(idx_info->pline->nused == 0); /* Shouldn't have filter defined on entering here */
    HDassert(idx_info->layout);
    HDassert(idx_info->storage);
    HDassert(idx_info->layout->max_nchunks);
    HDassert(!H5F_addr_defined(idx_info->storage->idx_addr));	/* address of data shouldn't be defined */

    /* Calculate size of max dataset chunks */
    nbytes = idx_info->layout->max_nchunks * idx_info->layout->size;

    /* Allocate space for max dataset chunks */
    addr = H5MF_alloc(idx_info->f, H5FD_MEM_DRAW, idx_info->dxpl_id, nbytes);
    if(!H5F_addr_defined(addr))
	HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "file allocation failed")

    /* This is the address of the dataset chunks */
    idx_info->storage->idx_addr = addr;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D_none_create() */


/*-------------------------------------------------------------------------
 * Function:	H5D_none_is_space_alloc
 *
 * Purpose:	Query if space for the dataset chunks is allocated
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Vailin Choi; September 2010
 *
 *-------------------------------------------------------------------------
 */
static hbool_t
H5D_none_is_space_alloc(const H5O_storage_chunk_t *storage)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Check args */
    HDassert(storage);

    FUNC_LEAVE_NOAPI((hbool_t)H5F_addr_defined(storage->idx_addr))
} /* end H5D_none_is_space_alloc() */


/*-------------------------------------------------------------------------
 * Function:	H5D_none_insert
 *
 * Purpose:	Calculate the address of the chunk
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Vailin Choi; Sept 2010
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_none_insert(const H5D_chk_idx_info_t *idx_info, H5D_chunk_ud_t *udata)
{
    hsize_t     idx;			/* Array index of chunk */
    herr_t	ret_value = SUCCEED;	/* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(idx_info);
    HDassert(idx_info->f);
    HDassert(idx_info->pline);
    HDassert(idx_info->pline->nused == 0);
    HDassert(idx_info->layout);
    HDassert(idx_info->storage);
    HDassert(H5F_addr_defined(idx_info->storage->idx_addr));
    HDassert(udata);

    /* Calculate the index of this chunk */
    if(H5V_chunk_index((idx_info->layout->ndims - 1), udata->common.offset, idx_info->layout->dim, idx_info->layout->max_down_chunks, &idx) < 0)
	HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "can't get chunk index")

    HDassert(!H5F_addr_defined(udata->addr));
    HDassert(udata->nbytes == idx_info->layout->size);

    /* Storage is already allocated, just calculate the file address of the chunk */
    H5_CHECK_OVERFLOW(udata->nbytes, /*From: */uint32_t, /*To: */hsize_t);
    udata->addr = idx_info->storage->idx_addr + idx * udata->nbytes;

    HDassert(H5F_addr_defined(udata->addr));

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5D_none_insert() */


/*-------------------------------------------------------------------------
 * Function:	H5D_none_get_addr
 *
 * Purpose:	Get the file address of a chunk.
 *		Save the retrieved information in the udata supplied.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Vailin Choi; Sept 2010
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_none_get_addr(const H5D_chk_idx_info_t *idx_info, H5D_chunk_ud_t *udata)
{
    hsize_t     idx;   	                /* Array index of chunk */
    herr_t	ret_value = SUCCEED;	/* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(idx_info);
    HDassert(idx_info->f);
    HDassert(idx_info->pline);
    HDassert(idx_info->pline->nused == 0);
    HDassert(idx_info->layout);
    HDassert(idx_info->storage);
    HDassert(udata);
    HDassert(H5F_addr_defined(idx_info->storage->idx_addr));

    /* Calculate the index of this chunk */
    if(H5V_chunk_index((idx_info->layout->ndims - 1), udata->common.offset, idx_info->layout->dim, idx_info->layout->max_down_chunks, &idx) < 0)
	HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "can't get chunk index")

    /* Calculate the address of the chunk */
    udata->addr = idx_info->storage->idx_addr + idx * idx_info->layout->size;

    /* Update the other (constant) information for the chunk */
    udata->nbytes = idx_info->layout->size;
    udata->filter_mask = 0;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5D_none_get_addr() */


/*-------------------------------------------------------------------------
 * Function:	H5D_none_iterate
 *
 * Purpose:	Iterate over the chunks in an index, making a callback
 *              for each one.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Vailin Choi; September 2010
 *
 *-------------------------------------------------------------------------
 */
static int
H5D_none_iterate(const H5D_chk_idx_info_t *idx_info,
    H5D_chunk_cb_func_t chunk_cb, void *chunk_udata)
{
    H5D_chunk_rec_t chunk_rec;			/* generic chunk record  */
    hsize_t chunk_offset[H5O_LAYOUT_NDIMS]; 	/* Offset for the chunk */
    unsigned ndims; 	/* Rank of chunk */
    unsigned u;		/* Local index variable */
    int curr_dim;       /* Current rank */
    hsize_t idx;    	/* Array index of chunk */
    int ret_value;    	/* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(idx_info);
    HDassert(idx_info->f);
    HDassert(idx_info->pline);
    HDassert(!idx_info->pline->nused);
    HDassert(idx_info->layout);
    HDassert(idx_info->storage);
    HDassert(chunk_cb);
    HDassert(chunk_udata);
    HDassert(H5F_addr_defined(idx_info->storage->idx_addr));

    /* Initialize generic chunk record */
    HDmemset(&chunk_rec, 0, sizeof(chunk_rec));
    chunk_rec.nbytes = idx_info->layout->size;
    chunk_rec.filter_mask = 0;

    HDmemset(&chunk_offset, 0, sizeof(chunk_offset));

    ndims = idx_info->layout->ndims - 1;
    HDassert(ndims > 0);

    /* Iterate over all the chunks in the dataset's dataspace */
    for(u = 0; u < idx_info->layout->nchunks; u++) {
	/* Calculate the index of this chunk */
	if(H5V_chunk_index(ndims, chunk_rec.offset, idx_info->layout->dim, idx_info->layout->max_down_chunks, &idx) < 0)
	    HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "can't get chunk index")

	/* Calculate the address of the chunk */
	chunk_rec.chunk_addr = idx_info->storage->idx_addr + idx * idx_info->layout->size;

	/* Make "generic chunk" callback */
	if((ret_value = (*chunk_cb)(&chunk_rec, chunk_udata)) < 0)
	    HERROR(H5E_DATASET, H5E_CALLBACK, "failure in generic chunk iterator callback");

	/* Update coordinates of chunk in dataset */
	curr_dim = (int)(ndims - 1);
	while(curr_dim >= 0) {
	    /* Increment coordinate in current dimension */
	    chunk_offset[curr_dim]++;
	    chunk_rec.offset[curr_dim] += idx_info->layout->dim[curr_dim];

	    /* Check if we went off the end of the current dimension */
	    if(chunk_offset[curr_dim] >= idx_info->layout->chunks[curr_dim]) {
		/* Reset coordinate & move to next faster dimension */
		chunk_offset[curr_dim] = 0;
		chunk_rec.offset[curr_dim] = 0;
		curr_dim--;
	    } /* end if */
	    else
		break;
	} /* end while */
    } /* end for */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D_none_iterate() */


/*-------------------------------------------------------------------------
 * Function:	H5D_none_remove
 *
 * Purpose:	Remove chunk from index.
 *
 * Note:	Chunks can't be removed (or added) to datasets with this
 *		form of index - all the space for all the chunks is always
 *		allocated in the file.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Vailin Choi; Sept 2010
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_none_remove(const H5D_chk_idx_info_t UNUSED *idx_info, H5D_chunk_common_ud_t UNUSED *udata)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* NO OP */

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5D_none_remove() */


/*-------------------------------------------------------------------------
 * Function:	H5D_none_delete
 *
 * Purpose:	Delete raw data storage for entire dataset (i.e. all chunks)
 *
 * Return:	Success:	Non-negative
 *		Failure:	negative
 *
 * Programmer:	Vailin Choi; Sept 2010
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_none_delete(const H5D_chk_idx_info_t *idx_info)
{
    hsize_t nbytes;                 /* Size of all chunks */
    herr_t ret_value = SUCCEED;     /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(idx_info);
    HDassert(idx_info->f);
    HDassert(idx_info->pline);
    HDassert(!idx_info->pline->nused); /* Shouldn't have filter defined on entering here */
    HDassert(idx_info->layout);
    HDassert(idx_info->storage);
    HDassert(H5F_addr_defined(idx_info->storage->idx_addr));  /* should be defined */

    /* chunk size * max # of chunks */
    nbytes = idx_info->layout->max_nchunks * idx_info->layout->size;
    if(H5MF_xfree(idx_info->f, H5FD_MEM_DRAW, idx_info->dxpl_id, idx_info->storage->idx_addr, nbytes) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTFREE, H5_ITER_ERROR, "unable to free dataset chunks")

    idx_info->storage->idx_addr = HADDR_UNDEF;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D_none_delete() */


/*-------------------------------------------------------------------------
 * Function:	H5D_none_copy_setup
 *
 * Purpose:	Set up any necessary information for copying chunks
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Vailin Choi; Sept 2010
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_none_copy_setup(const H5D_chk_idx_info_t *idx_info_src,
    const H5D_chk_idx_info_t *idx_info_dst)
{
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Check args */
    HDassert(idx_info_src);
    HDassert(idx_info_src->f);
    HDassert(idx_info_src->pline);
    HDassert(!idx_info_src->pline->nused);
    HDassert(idx_info_src->layout);
    HDassert(idx_info_src->storage);
    HDassert(H5F_addr_defined(idx_info_src->storage->idx_addr));

    HDassert(idx_info_dst);
    HDassert(idx_info_dst->f);
    HDassert(idx_info_dst->pline);
    HDassert(!idx_info_dst->pline->nused);
    HDassert(idx_info_dst->layout);
    HDassert(idx_info_dst->storage);

    /* Set copied metadata tag */
    H5_BEGIN_TAG(idx_info_dst->dxpl_id, H5AC__COPIED_TAG, FAIL);

    /* Allocate dataset chunks in the dest. file */
    if(H5D_none_create(idx_info_dst) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to initialize chunked storage")

    /* Reset metadata tag */
    H5_END_TAG(FAIL);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D_none_copy_setup() */


/*-------------------------------------------------------------------------
 * Function:    H5D_none_size
 *
 * Purpose:     Retrieve the amount of index storage for chunked dataset
 *
 * Return:      Success:        Non-negative
 *              Failure:        negative
 *
 * Programmer:	Vailin Choi; Sept 2010
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_none_size(const H5D_chk_idx_info_t UNUSED *idx_info, hsize_t *index_size)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Check args */
    HDassert(index_size);

    *index_size = 0;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5D_none_size() */


/*-------------------------------------------------------------------------
 * Function:	H5D_none_reset
 *
 * Purpose:	Reset indexing information.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Vailin Choi; Sept 2010
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_none_reset(H5O_storage_chunk_t *storage, hbool_t reset_addr)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Check args */
    HDassert(storage);

    /* Reset index info */
    if(reset_addr)
	storage->idx_addr = HADDR_UNDEF;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5D_none_reset() */


/*-------------------------------------------------------------------------
 * Function:	H5D_none_dump
 *
 * Purpose:	Dump 
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Vailin Choi; September 2010
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_none_dump(const H5O_storage_chunk_t *storage, FILE *stream)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Check args */
    HDassert(storage);
    HDassert(stream);

    HDfprintf(stream, "    Address: %a\n", storage->idx_addr);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5D_none_dump() */

