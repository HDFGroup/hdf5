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
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Sprivate.h"		/* Dataspace functions			*/
#include "H5SLprivate.h"	/* Skip lists				*/
#include "H5Vprivate.h"		/* Vector and array functions		*/
#include "H5AC2private.h"       /* Metadata cache                       */

#ifdef H5_HAVE_PARALLEL
/* Remove this if H5R_DATASET_REGION is no longer used in this file */
#   include "H5Rpublic.h"
#endif /*H5_HAVE_PARALLEL*/

/****************/
/* Local Macros */
/****************/

#define H5D_DEFAULT_SKIPLIST_HEIGHT     8

/* Macros for iterating over chunks to operate on */
#define H5D_CHUNK_GET_FIRST_NODE(map) (map.use_single ? (H5SL_node_t *)(1) : H5SL_first(map.sel_chunks))
#define H5D_CHUNK_GET_NODE_INFO(map, node)  (map.use_single ? map.single_chunk_info : (H5D_chunk_info_t *)H5SL_item(node))
#define H5D_CHUNK_GET_NEXT_NODE(map, node)  (map.use_single ? (H5SL_node_t *)NULL : H5SL_next(node))


/******************/
/* Local Typedefs */
/******************/


/********************/
/* Local Prototypes */
/********************/

static herr_t H5D_read(H5D_t *dataset, hid_t mem_type_id,
			const H5S_t *mem_space, const H5S_t *file_space,
			hid_t dset_xfer_plist, void *buf/*out*/);
static herr_t H5D_write(H5D_t *dataset, hid_t mem_type_id,
			 const H5S_t *mem_space, const H5S_t *file_space,
			 hid_t dset_xfer_plist, const void *buf);
static herr_t H5D_contig_read(H5D_io_info_t *io_info, hsize_t nelmts,
            const H5T_t *mem_type, const H5S_t *mem_space,
            const H5S_t *file_space, H5T_path_t *tpath,
            hid_t src_id, hid_t dst_id,  void *buf/*out*/);
static herr_t H5D_contig_write(H5D_io_info_t *io_info, hsize_t nelmts,
            const H5T_t *mem_type, const H5S_t *mem_space,
	    const H5S_t *file_space, H5T_path_t *tpath,
            hid_t src_id, hid_t dst_id, const void *buf);
static herr_t H5D_chunk_read(H5D_io_info_t *io_info, hsize_t nelmts,
            const H5T_t *mem_type, const H5S_t *mem_space,
            const H5S_t *file_space, H5T_path_t *tpath,
            hid_t src_id, hid_t dst_id, void *buf/*out*/);
static herr_t H5D_chunk_write(H5D_io_info_t *io_info, hsize_t nelmts,
            const H5T_t *mem_type, const H5S_t *mem_space,
	    const H5S_t *file_space, H5T_path_t *tpath,
            hid_t src_id, hid_t dst_id, const void *buf);
static herr_t H5D_compound_opt_read(size_t nelmts, const H5S_t *mem_space,
            H5S_sel_iter_t *iter, const H5D_dxpl_cache_t *dxpl_cache, 
            hid_t src_id, hid_t dst_id, H5T_subset_t subset, void *data_buf, 
            void *user_buf/*out*/);
static herr_t H5D_compound_opt_write(size_t nelmts, hid_t src_id, hid_t dst_id, 
            void *data_buf);

#ifdef H5_HAVE_PARALLEL
static herr_t H5D_ioinfo_term(H5D_io_info_t *io_info);
#endif /* H5_HAVE_PARALLEL */

/* I/O info operations */
static herr_t H5D_ioinfo_init(H5D_t *dset, const H5D_dxpl_cache_t *dxpl_cache,
    hid_t dxpl_id, const H5S_t *mem_space, const H5S_t *file_space,
    H5T_path_t *tpath, H5D_io_info_t *io_info);

/* Chunk operations */
static herr_t H5D_create_chunk_map(H5D_chunk_map_t *fm, const H5D_io_info_t *io_info,
    hsize_t nelmts, const H5S_t *file_space, const H5S_t *mem_space,
    const H5T_t *mem_type);
static herr_t H5D_destroy_chunk_map(const H5D_chunk_map_t *fm);
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

/*******************/
/* Local Variables */
/*******************/

/* Declare a free list to manage blocks of type conversion data */
H5FL_BLK_DEFINE(type_conv);

/* Declare a free list to manage the H5D_chunk_info_t struct */
H5FL_DEFINE(H5D_chunk_info_t);

/* Declare a free list to manage sequences of size_t */
H5FL_SEQ_DEFINE_STATIC(size_t);

/* Declare a free list to manage sequences of hsize_t */
H5FL_SEQ_DEFINE_STATIC(hsize_t);



/*--------------------------------------------------------------------------
 NAME
    H5D_get_dxpl_cache_real
 PURPOSE
    Get all the values for the DXPL cache.
 USAGE
    herr_t H5D_get_dxpl_cache_real(dxpl_id, cache)
        hid_t dxpl_id;          IN: DXPL to query
        H5D_dxpl_cache_t *cache;IN/OUT: DXPL cache to fill with values
 RETURNS
    Non-negative on success/Negative on failure.
 DESCRIPTION
    Query all the values from a DXPL that are needed by internal routines
    within the library.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t
H5D_get_dxpl_cache_real(hid_t dxpl_id, H5D_dxpl_cache_t *cache)
{
    H5P_genplist_t *dx_plist;   /* Data transfer property list */
    herr_t ret_value=SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(H5D_get_dxpl_cache_real,FAIL)

    /* Check args */
    assert(cache);

    /* Get the dataset transfer property list */
    if (NULL == (dx_plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset transfer property list")

    /* Get maximum temporary buffer size */
    if(H5P_get(dx_plist, H5D_XFER_MAX_TEMP_BUF_NAME, &cache->max_temp_buf) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "Can't retrieve maximum temporary buffer size")

    /* Get temporary buffer pointer */
    if(H5P_get(dx_plist, H5D_XFER_TCONV_BUF_NAME, &cache->tconv_buf) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "Can't retrieve temporary buffer pointer")

    /* Get background buffer pointer */
    if(H5P_get(dx_plist, H5D_XFER_BKGR_BUF_NAME, &cache->bkgr_buf) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "Can't retrieve background buffer pointer")

    /* Get background buffer type */
    if(H5P_get(dx_plist, H5D_XFER_BKGR_BUF_TYPE_NAME, &cache->bkgr_buf_type) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "Can't retrieve background buffer type")

    /* Get B-tree split ratios */
    if(H5P_get(dx_plist, H5D_XFER_BTREE_SPLIT_RATIO_NAME, &cache->btree_split_ratio) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "Can't retrieve B-tree split ratios")

    /* Get I/O vector size */
    if(H5P_get(dx_plist, H5D_XFER_HYPER_VECTOR_SIZE_NAME, &cache->vec_size) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "Can't retrieve I/O vector size")

#ifdef H5_HAVE_PARALLEL
    /* Collect Parallel I/O information for possible later use */
     if(H5P_get(dx_plist, H5D_XFER_IO_XFER_MODE_NAME, &cache->xfer_mode) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "Can't retrieve parallel transfer method")
     if(H5P_get(dx_plist, H5D_XFER_IO_XFER_OPT_MODE_NAME, &cache->xfer_opt_mode) < 0)
       HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "Can't retrieve parallel transfer method")
#endif /*H5_HAVE_PARALLEL*/

    /* Get error detection properties */
    if(H5P_get(dx_plist, H5D_XFER_EDC_NAME, &cache->err_detect) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "Can't retrieve error detection info")

    /* Get filter callback function */
    if(H5P_get(dx_plist, H5D_XFER_FILTER_CB_NAME, &cache->filter_cb) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "Can't retrieve filter callback function")

    /* Get the data transform property */
    if(H5P_get(dx_plist, H5D_XFER_XFORM_NAME, &cache->data_xform_prop) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "Can't retrieve data transform info")

done:
    FUNC_LEAVE_NOAPI(ret_value)
}   /* H5D_get_dxpl_cache_real() */


/*--------------------------------------------------------------------------
 NAME
    H5D_get_dxpl_cache
 PURPOSE
    Get all the values for the DXPL cache.
 USAGE
    herr_t H5D_get_dxpl_cache(dxpl_id, cache)
        hid_t dxpl_id;          IN: DXPL to query
        H5D_dxpl_cache_t *cache;IN/OUT: DXPL cache to fill with values
 RETURNS
    Non-negative on success/Negative on failure.
 DESCRIPTION
    Query all the values from a DXPL that are needed by internal routines
    within the library.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
    The CACHE pointer should point at already allocated memory to place
    non-default property list info.  If a default property list is used, the
    CACHE pointer will be changed to point at the default information.
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t
H5D_get_dxpl_cache(hid_t dxpl_id, H5D_dxpl_cache_t **cache)
{
    herr_t ret_value=SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(H5D_get_dxpl_cache,FAIL)

    /* Check args */
    assert(cache);

    /* Check for the default DXPL */
    if(dxpl_id==H5P_DATASET_XFER_DEFAULT)
        *cache=&H5D_def_dxpl_cache;
    else
        if(H5D_get_dxpl_cache_real(dxpl_id,*cache) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "Can't retrieve DXPL values")

done:
    FUNC_LEAVE_NOAPI(ret_value)
}   /* H5D_get_dxpl_cache() */


/*-------------------------------------------------------------------------
 * Function:	H5Dread
 *
 * Purpose:	Reads (part of) a DSET from the file into application
 *		memory BUF. The part of the dataset to read is defined with
 *		MEM_SPACE_ID and FILE_SPACE_ID.	 The data points are
 *		converted from their file type to the MEM_TYPE_ID specified.
 *		Additional miscellaneous data transfer properties can be
 *		passed to this function with the PLIST_ID argument.
 *
 *		The FILE_SPACE_ID can be the constant H5S_ALL which indicates
 *		that the entire file data space is to be referenced.
 *
 *		The MEM_SPACE_ID can be the constant H5S_ALL in which case
 *		the memory data space is the same as the file data space
 *		defined when the dataset was created.
 *
 *		The number of elements in the memory data space must match
 *		the number of elements in the file data space.
 *
 *		The PLIST_ID can be the constant H5P_DEFAULT in which
 *		case the default data transfer properties are used.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Thursday, December  4, 1997
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Dread(hid_t dset_id, hid_t mem_type_id, hid_t mem_space_id,
	hid_t file_space_id, hid_t plist_id, void *buf/*out*/)
{
    H5D_t		   *dset = NULL;
    const H5S_t		   *mem_space = NULL;
    const H5S_t		   *file_space = NULL;
    char                    fake_char;
    herr_t                  ret_value=SUCCEED;  /* Return value */

    FUNC_ENTER_API(H5Dread, FAIL)
    H5TRACE6("e", "iiiiix", dset_id, mem_type_id, mem_space_id, file_space_id,
             plist_id, buf);

    /* check arguments */
    if(NULL == (dset = (H5D_t *)H5I_object_verify(dset_id, H5I_DATASET)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset")
    if(NULL == dset->oloc.file)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset")
    if(H5S_ALL != mem_space_id) {
	if(NULL == (mem_space = (const H5S_t *)H5I_object_verify(mem_space_id, H5I_DATASPACE)))
	    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space")

	/* Check for valid selection */
	if(H5S_SELECT_VALID(mem_space)!=TRUE)
	    HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "selection+offset not within extent")
    } /* end if */
    if(H5S_ALL != file_space_id) {
	if(NULL == (file_space = (const H5S_t *)H5I_object_verify(file_space_id, H5I_DATASPACE)))
	    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space")

	/* Check for valid selection */
	if(H5S_SELECT_VALID(file_space)!=TRUE)
	    HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "selection+offset not within extent")
    }

    /* Get the default dataset transfer property list if the user didn't provide one */
    if (H5P_DEFAULT == plist_id)
        plist_id= H5P_DATASET_XFER_DEFAULT;
    else
        if (TRUE!=H5P_isa_class(plist_id,H5P_DATASET_XFER))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not xfer parms")
    if (!buf && H5S_GET_SELECT_NPOINTS(file_space)!=0)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no output buffer")

    /* If the buffer is nil, and 0 element is selected, make a fake buffer.
     * This is for some MPI package like ChaMPIon on NCSA's tungsten which 
     * doesn't support this feature. 
     */ 
    if (!buf)
        buf = &fake_char;

    /* read raw data */
    if (H5D_read(dset, mem_type_id, mem_space, file_space, plist_id, buf/*out*/) < 0)
	HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "can't read data")

done:
    FUNC_LEAVE_API(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5Dwrite
 *
 * Purpose:	Writes (part of) a DSET from application memory BUF to the
 *		file.  The part of the dataset to write is defined with the
 *		MEM_SPACE_ID and FILE_SPACE_ID arguments. The data points
 *		are converted from their current type (MEM_TYPE_ID) to their
 *		file datatype.	 Additional miscellaneous data transfer
 *		properties can be passed to this function with the
 *		PLIST_ID argument.
 *
 *		The FILE_SPACE_ID can be the constant H5S_ALL which indicates
 *		that the entire file data space is to be referenced.
 *
 *		The MEM_SPACE_ID can be the constant H5S_ALL in which case
 *		the memory data space is the same as the file data space
 *		defined when the dataset was created.
 *
 *		The number of elements in the memory data space must match
 *		the number of elements in the file data space.
 *
 *		The PLIST_ID can be the constant H5P_DEFAULT in which
 *		case the default data transfer properties are used.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Thursday, December  4, 1997
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Dwrite(hid_t dset_id, hid_t mem_type_id, hid_t mem_space_id,
	 hid_t file_space_id, hid_t plist_id, const void *buf)
{
    H5D_t		   *dset = NULL;
    const H5S_t		   *mem_space = NULL;
    const H5S_t		   *file_space = NULL;
    char                    fake_char;
    herr_t                  ret_value=SUCCEED;  /* Return value */

    FUNC_ENTER_API_META(H5Dwrite, dset_id, FAIL)
    H5TRACE6("e", "iiiii*x", dset_id, mem_type_id, mem_space_id, file_space_id,
             plist_id, buf);

    /* check arguments */
    if(NULL == (dset = (H5D_t *)H5I_object_verify(dset_id, H5I_DATASET)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset")
    if(NULL == dset->oloc.file)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset")
    if(H5S_ALL != mem_space_id) {
	if(NULL == (mem_space = (const H5S_t *)H5I_object_verify(mem_space_id, H5I_DATASPACE)))
	    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space")

	/* Check for valid selection */
	if (H5S_SELECT_VALID(mem_space)!=TRUE)
	    HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "memory selection+offset not within extent")
    } /* end if */
    if(H5S_ALL != file_space_id) {
	if(NULL == (file_space = (const H5S_t *)H5I_object_verify(file_space_id, H5I_DATASPACE)))
	    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space")

	/* Check for valid selection */
	if(H5S_SELECT_VALID(file_space)!=TRUE)
	    HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "file selection+offset not within extent")
    } /* end if */

    /* Get the default dataset transfer property list if the user didn't provide one */
    if(H5P_DEFAULT == plist_id)
        plist_id= H5P_DATASET_XFER_DEFAULT;
    else
        if(TRUE!=H5P_isa_class(plist_id,H5P_DATASET_XFER))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not xfer parms")
    if(!buf && H5S_GET_SELECT_NPOINTS(file_space)!=0)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no output buffer")
    
    /* If the buffer is nil, and 0 element is selected, make a fake buffer.
     * This is for some MPI package like ChaMPIon on NCSA's tungsten which 
     * doesn't support this feature. 
     */ 
    if (!buf)
        buf = &fake_char;

    /* write raw data */
    if(H5D_write(dset, mem_type_id, mem_space, file_space, plist_id, buf) < 0)
	HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "can't write data")

done:
    FUNC_LEAVE_API_META(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5D_read
 *
 * Purpose:	Reads (part of) a DATASET into application memory BUF. See
 *		H5Dread() for complete details.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Thursday, December  4, 1997
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_read(H5D_t *dataset, hid_t mem_type_id, const H5S_t *mem_space,
	 const H5S_t *file_space, hid_t dxpl_id, void *buf/*out*/)
{
    hssize_t	snelmts;                /*total number of elmts	(signed) */
    hsize_t	nelmts;                 /*total number of elmts	*/
    H5T_path_t	*tpath = NULL;		/*type conversion info	*/
    const H5T_t	*mem_type = NULL;       /* Memory datatype */
    H5D_io_info_t io_info;              /* Dataset I/O info     */
#ifdef H5_HAVE_PARALLEL
    hbool_t     io_info_init = FALSE;   /* Whether the I/O info has been initialized */
#endif /*H5_HAVE_PARALLEL*/
    H5D_dxpl_cache_t _dxpl_cache;       /* Data transfer property cache buffer */
    H5D_dxpl_cache_t *dxpl_cache=&_dxpl_cache;   /* Data transfer property cache */
    herr_t	ret_value = SUCCEED;	/* Return value	*/

    FUNC_ENTER_NOAPI_NOINIT(H5D_read)

    /* check args */
    HDassert(dataset && dataset->oloc.file);

    /* Get memory datatype */
    if(NULL == (mem_type = (const H5T_t *)H5I_object_verify(mem_type_id, H5I_DATATYPE)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a datatype")

    if(!file_space)
        file_space = dataset->shared->space;
    if(!mem_space)
        mem_space = file_space;
    if((snelmts = H5S_GET_SELECT_NPOINTS(mem_space)) < 0)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "src dataspace has invalid selection")
    H5_ASSIGN_OVERFLOW(nelmts,snelmts,hssize_t,hsize_t);

    /* Fill the DXPL cache values for later use */
    if(H5D_get_dxpl_cache(dxpl_id,&dxpl_cache) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't fill dxpl cache")

#ifdef H5_HAVE_PARALLEL
    /* Collective access is not permissible without a MPI based VFD */
    if (dxpl_cache->xfer_mode==H5FD_MPIO_COLLECTIVE && !IS_H5FD_MPI(dataset->oloc.file))
        HGOTO_ERROR(H5E_DATASET, H5E_UNSUPPORTED, FAIL, "collective access for MPI-based drivers only")
#endif /*H5_HAVE_PARALLEL*/

    /* Make certain that the number of elements in each selection is the same */
    if (nelmts!=(hsize_t)H5S_GET_SELECT_NPOINTS(file_space))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "src and dest data spaces have different sizes")

    /* Make sure that both selections have their extents set */
    if( !(H5S_has_extent(file_space)) )
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "file dataspace does not have extent set")
    if( !(H5S_has_extent(mem_space)) )
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "memory dataspace does not have extent set")

    /* Retrieve dataset properties */
    /* <none needed in the general case> */

    /* If space hasn't been allocated and not using external storage,
     * return fill value to buffer if fill time is upon allocation, or
     * do nothing if fill time is never.  If the dataset is compact and
     * fill time is NEVER, there is no way to tell whether part of data
     * has been overwritten.  So just proceed in reading.
     */
    if(nelmts > 0 && dataset->shared->dcpl_cache.efl.nused == 0 &&
            ((dataset->shared->layout.type == H5D_CONTIGUOUS && !H5F_addr_defined(dataset->shared->layout.u.contig.addr))
                || (dataset->shared->layout.type == H5D_CHUNKED && !H5F_addr_defined(dataset->shared->layout.u.chunk.addr)))) {
        H5D_fill_value_t fill_status;   /* Whether/How the fill value is defined */

        /* Retrieve dataset's fill-value properties */
        if(H5P_is_fill_value_defined(&dataset->shared->dcpl_cache.fill, &fill_status) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't tell if fill value defined")

        /* Should be impossible, but check anyway... */
        if(fill_status == H5D_FILL_VALUE_UNDEFINED &&
                (dataset->shared->dcpl_cache.fill.fill_time == H5D_FILL_TIME_ALLOC || dataset->shared->dcpl_cache.fill.fill_time == H5D_FILL_TIME_IFSET))
            HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "read failed: dataset doesn't exist, no data can be read")

        /* If we're never going to fill this dataset, just leave the junk in the user's buffer */
        if(dataset->shared->dcpl_cache.fill.fill_time == H5D_FILL_TIME_NEVER)
            HGOTO_DONE(SUCCEED)

        /* Go fill the user's selection with the dataset's fill value */
        if(H5D_fill(dataset->shared->dcpl_cache.fill.buf, dataset->shared->type, buf, mem_type, mem_space, dxpl_id) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "filling buf failed")
        else
            HGOTO_DONE(SUCCEED)
    } /* end if */

    /*
     * Locate the type conversion function and data space conversion
     * functions, and set up the element numbering information. If a data
     * type conversion is necessary then register datatype atoms. Data type
     * conversion is necessary if the user has set the `need_bkg' to a high
     * enough value in xfer_parms since turning off datatype conversion also
     * turns off background preservation.
     */
    if (NULL==(tpath=H5T_path_find(dataset->shared->type, mem_type, NULL, NULL, dxpl_id, FALSE)))
        HGOTO_ERROR(H5E_DATASET, H5E_UNSUPPORTED, FAIL, "unable to convert between src and dest datatype")

    /* Set up I/O operation */
    if(H5D_ioinfo_init(dataset,dxpl_cache,dxpl_id,mem_space,file_space,tpath,&io_info) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_UNSUPPORTED, FAIL, "unable to set up I/O operation")
#ifdef H5_HAVE_PARALLEL
    io_info_init = TRUE;
#endif /*H5_HAVE_PARALLEL*/

    /* Determine correct I/O routine to invoke */
    if(dataset->shared->layout.type!=H5D_CHUNKED) {
        if(H5D_contig_read(&io_info, nelmts, mem_type, mem_space, file_space, tpath,
                dataset->shared->type_id, mem_type_id, buf) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "can't read data")
    } /* end if */
    else {
        if(H5D_chunk_read(&io_info, nelmts, mem_type, mem_space, file_space, tpath,
                dataset->shared->type_id, mem_type_id, buf) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "can't read data")
    } /* end else */

done:
#ifdef H5_HAVE_PARALLEL
    /* Shut down io_info struct */
    if (io_info_init)
        if(H5D_ioinfo_term(&io_info) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CANTCLOSEOBJ, FAIL, "can't shut down io_info")
#endif /*H5_HAVE_PARALLEL*/

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D_read() */


/*-------------------------------------------------------------------------
 * Function:	H5D_write
 *
 * Purpose:	Writes (part of) a DATASET to a file from application memory
 *		BUF. See H5Dwrite() for complete details.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Thursday, December  4, 1997
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_write(H5D_t *dataset, hid_t mem_type_id, const H5S_t *mem_space,
	  const H5S_t *file_space, hid_t dxpl_id, const void *buf)
{
    hssize_t	snelmts;                /*total number of elmts	(signed) */
    hsize_t	nelmts;                 /*total number of elmts	*/
    H5T_path_t	*tpath = NULL;		/*type conversion info	*/
    const H5T_t	*mem_type = NULL;       /* Memory datatype */
    H5D_io_info_t io_info;              /* Dataset I/O info     */
#ifdef H5_HAVE_PARALLEL
    hbool_t     io_info_init = FALSE;   /* Whether the I/O info has been initialized */
#endif /*H5_HAVE_PARALLEL*/
    H5D_dxpl_cache_t _dxpl_cache;       /* Data transfer property cache buffer */
    H5D_dxpl_cache_t *dxpl_cache=&_dxpl_cache;   /* Data transfer property cache */
    herr_t	ret_value = SUCCEED;	/* Return value	*/

    FUNC_ENTER_NOAPI_NOINIT(H5D_write)

    /* check args */
    HDassert(dataset && dataset->oloc.file);

    /* Get the memory datatype */
    if(NULL == (mem_type = (const H5T_t *)H5I_object_verify(mem_type_id, H5I_DATATYPE)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a datatype")

    /* All filters in the DCPL must have encoding enabled. */
    if(!dataset->shared->checked_filters) {
        if(H5Z_can_apply(dataset->shared->dcpl_id, dataset->shared->type_id) <0)
            HGOTO_ERROR(H5E_PLINE, H5E_CANAPPLY, FAIL, "can't apply filters")

        dataset->shared->checked_filters = TRUE;
    } /* end if */

    /* Check if we are allowed to write to this file */
    if(0==(H5F_get_intent(dataset->oloc.file) & H5F_ACC_RDWR))
	HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "no write intent on file")

    /* Fill the DXPL cache values for later use */
    if(H5D_get_dxpl_cache(dxpl_id,&dxpl_cache) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't fill dxpl cache")

    /* Various MPI based checks */
#ifdef H5_HAVE_PARALLEL
    if(IS_H5FD_MPI(dataset->oloc.file)) {
        /* If MPI based VFD is used, no VL datatype support yet. */
        /* This is because they use the global heap in the file and we don't */
        /* support parallel access of that yet */
        if(H5T_detect_class(mem_type, H5T_VLEN)>0)
            HGOTO_ERROR(H5E_DATASET, H5E_UNSUPPORTED, FAIL, "Parallel IO does not support writing VL datatypes yet")

        /* If MPI based VFD is used, no VL datatype support yet. */
        /* This is because they use the global heap in the file and we don't */
        /* support parallel access of that yet */
        /* We should really use H5T_detect_class() here, but it will be difficult
         * to detect the type of the reference if it is nested... -QAK
         */
        if (H5T_get_class(mem_type, TRUE)==H5T_REFERENCE &&
                H5T_get_ref_type(mem_type)==H5R_DATASET_REGION)
            HGOTO_ERROR(H5E_DATASET, H5E_UNSUPPORTED, FAIL, "Parallel IO does not support writing region reference datatypes yet")
    } /* end if */
    else {
        /* Collective access is not permissible without a MPI based VFD */
        if (dxpl_cache->xfer_mode==H5FD_MPIO_COLLECTIVE)
            HGOTO_ERROR(H5E_DATASET, H5E_UNSUPPORTED, FAIL, "collective access for MPI-based driver only")
    } /* end else */
#endif /*H5_HAVE_PARALLEL*/

    if (!file_space)
        file_space = dataset->shared->space;
    if (!mem_space)
        mem_space = file_space;
    if((snelmts = H5S_GET_SELECT_NPOINTS(mem_space)) < 0)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "src dataspace has invalid selection")
    H5_ASSIGN_OVERFLOW(nelmts,snelmts,hssize_t,hsize_t);

    /* Make certain that the number of elements in each selection is the same */
    if (nelmts!=(hsize_t)H5S_GET_SELECT_NPOINTS(file_space))
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "src and dest data spaces have different sizes")

    /* Make sure that both selections have their extents set */
    if( !(H5S_has_extent(file_space)) )
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "file dataspace does not have extent set")
    if( !(H5S_has_extent(mem_space)) )
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "memory dataspace does not have extent set")

    /* Retrieve dataset properties */
    /* <none needed currently> */

    /* Allocate data space and initialize it if it hasn't been. */
    if(nelmts > 0 && dataset->shared->dcpl_cache.efl.nused==0 &&
            ((dataset->shared->layout.type==H5D_CONTIGUOUS && !H5F_addr_defined(dataset->shared->layout.u.contig.addr))
                || (dataset->shared->layout.type==H5D_CHUNKED && !H5F_addr_defined(dataset->shared->layout.u.chunk.addr)))) {
        hssize_t file_nelmts;   /* Number of elements in file dataset's dataspace */
        hbool_t full_overwrite; /* Whether we are over-writing all the elements */

        /* Get the number of elements in file dataset's dataspace */
        if((file_nelmts = H5S_GET_EXTENT_NPOINTS(file_space)) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_BADVALUE, FAIL, "can't retrieve number of elements in file dataset")

        /* Always allow fill values to be written if the dataset has a VL datatype */
        if(H5T_detect_class(dataset->shared->type, H5T_VLEN))
            full_overwrite = FALSE;
        else
            full_overwrite = (hsize_t)file_nelmts==nelmts ? TRUE : FALSE;

 	/* Allocate storage */
        if(H5D_alloc_storage(dataset->oloc.file, dxpl_id, dataset, H5D_ALLOC_WRITE, full_overwrite) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to initialize storage")
    } /* end if */

    /*
     * Locate the type conversion function and data space conversion
     * functions, and set up the element numbering information. If a data
     * type conversion is necessary then register datatype atoms. Data type
     * conversion is necessary if the user has set the `need_bkg' to a high
     * enough value in xfer_parms since turning off datatype conversion also
     * turns off background preservation.
     */
    if (NULL==(tpath=H5T_path_find(mem_type, dataset->shared->type, NULL, NULL, dxpl_id, FALSE)))
	HGOTO_ERROR(H5E_DATASET, H5E_UNSUPPORTED, FAIL, "unable to convert between src and dest datatype")

    /* Set up I/O operation */
    if(H5D_ioinfo_init(dataset,dxpl_cache,dxpl_id,mem_space,file_space,tpath,&io_info) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_UNSUPPORTED, FAIL, "unable to set up I/O operation")
#ifdef H5_HAVE_PARALLEL
    io_info_init = TRUE;
#endif /*H5_HAVE_PARALLEL*/

    /* Determine correct I/O routine to invoke */
    if(dataset->shared->layout.type!=H5D_CHUNKED) {
        if(H5D_contig_write(&io_info, nelmts, mem_type, mem_space, file_space, tpath,
                mem_type_id, dataset->shared->type_id, buf) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "can't write data")
    } /* end if */
    else {
        if(H5D_chunk_write(&io_info, nelmts, mem_type, mem_space, file_space, tpath,
                mem_type_id, dataset->shared->type_id, buf) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "can't write data")
    } /* end else */

#ifdef OLD_WAY
/*
 * This was taken out because it can be called in a parallel program with
 * independent access, causing the metadata cache to get corrupted. Its been
 * disabled for all types of access (serial as well as parallel) to make the
 * modification time consistent for all programs. -QAK
 */
    /*
     * Update modification time.  We have to do this explicitly because
     * writing to a dataset doesn't necessarily change the object header.
     */
    if(H5O_touch(&(dataset->oloc), FALSE, dxpl_id) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to update modification time")
#endif /* OLD_WAY */

done:
#ifdef H5_HAVE_PARALLEL
    /* Shut down io_info struct */
    if (io_info_init)
        if(H5D_ioinfo_term(&io_info) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CANTCLOSEOBJ, FAIL, "can't shut down io_info")
#endif /*H5_HAVE_PARALLEL*/

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D_write() */


/*-------------------------------------------------------------------------
 * Function:	H5D_contig_read
 *
 * Purpose:	Read from a contiguous dataset.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *		Thursday, April 10, 2003
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_contig_read(H5D_io_info_t *io_info, hsize_t nelmts,
    const H5T_t *mem_type, const H5S_t *mem_space,
    const H5S_t *file_space, H5T_path_t *tpath,
    hid_t src_id, hid_t dst_id, void *buf/*out*/)
{
    H5D_t *dataset=io_info->dset;       /* Local pointer to dataset info */
    const H5D_dxpl_cache_t *dxpl_cache=io_info->dxpl_cache;     /* Local pointer to dataset transfer info */
    herr_t      status;                 /*function return status*/
#ifdef H5S_DEBUG
    H5_timer_t	timer;
#endif
    size_t	src_type_size;		/*size of source type	*/
    size_t	dst_type_size;	        /*size of destination type*/
    size_t	max_type_size;	        /* Size of largest source/destination type */
    size_t	target_size;		/*desired buffer size	*/
    size_t	request_nelmts;		/*requested strip mine	*/
    H5S_sel_iter_t mem_iter;            /*memory selection iteration info*/
    hbool_t	mem_iter_init=0;	/*memory selection iteration info has been initialized */
    H5S_sel_iter_t bkg_iter;            /*background iteration info*/
    hbool_t	bkg_iter_init=0;	/*background iteration info has been initialized */
    H5S_sel_iter_t file_iter;           /*file selection iteration info*/
    hbool_t	file_iter_init=0;	/*file selection iteration info has been initialized */
    H5T_bkg_t	need_bkg;		/*type of background buf*/
    uint8_t	*tconv_buf = NULL;	/*datatype conv buffer	*/
    uint8_t	*bkg_buf = NULL;	/*background buffer	*/
    hsize_t	smine_start;		/*strip mine start loc	*/
    size_t	n, smine_nelmts;	/*elements per strip	*/
    H5D_storage_t store;                /*union of storage info for dataset */


    herr_t	ret_value = SUCCEED;	/*return value		*/

    FUNC_ENTER_NOAPI_NOINIT(H5D_contig_read)

    assert (buf);

    /* Initialize storage info for this dataset */
    if (dataset->shared->dcpl_cache.efl.nused > 0)
        HDmemcpy(&store.efl, &(dataset->shared->dcpl_cache.efl), sizeof(H5O_efl_t));
    else {
        store.contig.dset_addr = dataset->shared->layout.u.contig.addr;
        store.contig.dset_size = dataset->shared->layout.u.contig.size;
    } /* end if */

    /* Set dataset storage for I/O info */
    io_info->store=&store;

    /*
     * If there is no type conversion then read directly into the
     * application's buffer.  This saves at least one mem-to-mem copy.
     */
    if ( H5Z_xform_noop(dxpl_cache->data_xform_prop) && H5T_path_noop(tpath)) {
#ifdef H5S_DEBUG
	H5_timer_begin(&timer);
#endif
  	/* Sanity check dataset, then read it */
        assert(((dataset->shared->layout.type == H5D_CONTIGUOUS && H5F_addr_defined(dataset->shared->layout.u.contig.addr))
                || (dataset->shared->layout.type == H5D_CHUNKED && H5F_addr_defined(dataset->shared->layout.u.chunk.addr)))
                || dataset->shared->dcpl_cache.efl.nused > 0 || 0 == nelmts
                || dataset->shared->layout.type==H5D_COMPACT);
        H5_CHECK_OVERFLOW(nelmts,hsize_t,size_t);

#ifdef H5_HAVE_PARALLEL
	if(io_info->dxpl_cache->xfer_mode == H5FD_MPIO_COLLECTIVE) {
	  if(H5D_contig_collective_io(io_info,file_space,mem_space,buf,FALSE) < 0)
	    HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "contiguous read failed in collective mode");
	}
	else
#endif
        {
            if((io_info->ops.read)(io_info, (size_t)nelmts,
                    H5T_get_size(dataset->shared->type), file_space, mem_space,
                    (haddr_t)0, NULL, buf/*out*/) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "contiguous read failed ");
	}

#ifdef H5S_DEBUG
        H5_timer_end(&(io_info->stats->stats[1].read_timer), &timer);
        io_info->stats->stats[1].read_nbytes += nelmts * H5T_get_size(dataset->shared->type);
        io_info->stats->stats[1].read_ncalls++;
#endif

            /* direct xfer accomplished successfully */
            HGOTO_DONE(SUCCEED)
    } /* end if */

    /*
     * This is the general case (type conversion, usually).
     */
    if(nelmts==0)
        HGOTO_DONE(SUCCEED)

    /* Compute element sizes and other parameters */
    src_type_size = H5T_get_size(dataset->shared->type);
    dst_type_size = H5T_get_size(mem_type);
    max_type_size = MAX(src_type_size, dst_type_size);
    target_size = dxpl_cache->max_temp_buf;
    /* XXX: This could cause a problem if the user sets their buffer size
     * to the same size as the default, and then the dataset elements are
     * too large for the buffer... - QAK
     */
    if(target_size == H5D_TEMP_BUF_SIZE) {
        /* If the buffer is too small to hold even one element, make it bigger */
        if(target_size<max_type_size)
            target_size = max_type_size;
        /* If the buffer is too large to hold all the elements, make it smaller */
        else if(target_size>(nelmts*max_type_size))
            target_size=(size_t)(nelmts*max_type_size);
    } /* end if */
    request_nelmts = target_size / max_type_size;

    /* Sanity check elements in temporary buffer */
    if (request_nelmts==0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "temporary buffer max size is too small")

    /* Figure out the strip mine size. */
    if (H5S_select_iter_init(&file_iter, file_space, src_type_size) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to initialize file selection information")
    file_iter_init=1;	/*file selection iteration info has been initialized */
    if (H5S_select_iter_init(&mem_iter, mem_space, dst_type_size) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to initialize memory selection information")
    mem_iter_init=1;	/*file selection iteration info has been initialized */
    if (H5S_select_iter_init(&bkg_iter, mem_space, dst_type_size) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to initialize background selection information")
    bkg_iter_init=1;	/*file selection iteration info has been initialized */

    /*
     * Get a temporary buffer for type conversion unless the app has already
     * supplied one through the xfer properties. Instead of allocating a
     * buffer which is the exact size, we allocate the target size.  The
     * malloc() is usually less resource-intensive if we allocate/free the
     * same size over and over.
     */
    if (H5T_path_bkg(tpath)) {
        H5T_bkg_t path_bkg;     /* Type conversion's background info */

        /* Retrieve the bkgr buffer property */
        need_bkg=dxpl_cache->bkgr_buf_type;
        path_bkg = H5T_path_bkg(tpath);
        need_bkg = MAX(path_bkg, need_bkg);
    } else {
        need_bkg = H5T_BKG_NO; /*never needed even if app says yes*/
    } /* end else */
    if(NULL == (tconv_buf = (uint8_t *)dxpl_cache->tconv_buf)) {
        /* Allocate temporary buffer */
        if(NULL == (tconv_buf = H5FL_BLK_MALLOC(type_conv, target_size)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for type conversion")
    } /* end if */
    if(need_bkg && NULL == (bkg_buf = (uint8_t *)dxpl_cache->bkgr_buf)) {
        /* Allocate background buffer */
        /* (Need calloc()-like call since memory needs to be initialized) */
        if(NULL == (bkg_buf = H5FL_BLK_CALLOC(type_conv, (request_nelmts * dst_type_size))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for background conversion")
    } /* end if */

    /* Start strip mining... */
    for (smine_start=0; smine_start<nelmts; smine_start+=smine_nelmts) {
        /* Go figure out how many elements to read from the file */
        assert(H5S_SELECT_ITER_NELMTS(&file_iter)==(nelmts-smine_start));
        smine_nelmts = (size_t)MIN(request_nelmts, (nelmts-smine_start));

        /*
         * Gather the data from disk into the datatype conversion
         * buffer. Also gather data from application to background buffer
         * if necessary.
         */
#ifdef H5S_DEBUG
	H5_timer_begin(&timer);
#endif
	/* Sanity check that space is allocated, then read data from it */
        HDassert(((dataset->shared->layout.type == H5D_CONTIGUOUS && H5F_addr_defined(dataset->shared->layout.u.contig.addr))
                || (dataset->shared->layout.type == H5D_CHUNKED && H5F_addr_defined(dataset->shared->layout.u.chunk.addr)))
            || dataset->shared->dcpl_cache.efl.nused > 0 ||
             dataset->shared->layout.type == H5D_COMPACT);
        n = H5D_select_fgath(io_info, file_space, &file_iter, smine_nelmts, (haddr_t)0, NULL, tconv_buf/*out*/);

#ifdef H5S_DEBUG
	H5_timer_end(&(io_info->stats->stats[1].gath_timer), &timer);
	io_info->stats->stats[1].gath_nbytes += n * src_type_size;
	io_info->stats->stats[1].gath_ncalls++;
#endif
	if (n!=smine_nelmts)
            HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, "file gather failed")

        /* If the source and destination are compound types and subset of each other
         * and no conversion is needed, copy the data directly into user's buffer and 
         * bypass the rest of steps.  This optimization is for Chicago company */
        if(H5T_SUBSET_SRC==H5T_path_compound_subset(tpath)) {
            if(H5D_compound_opt_read(smine_nelmts, mem_space, &mem_iter, dxpl_cache,
                src_id, dst_id, H5T_SUBSET_SRC, tconv_buf, buf /*out*/)<0)
                HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "datatype conversion failed")

            continue;
        } else if(H5T_SUBSET_DST==H5T_path_compound_subset(tpath)) {
            if(H5D_compound_opt_read(smine_nelmts, mem_space, &mem_iter, dxpl_cache,
                src_id, dst_id, H5T_SUBSET_DST, tconv_buf, buf /*out*/)<0)
                HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "datatype conversion failed")

            continue;
        }

        if (H5T_BKG_YES==need_bkg) {
#ifdef H5S_DEBUG
            H5_timer_begin(&timer);
#endif
            n = H5D_select_mgath(buf, mem_space, &bkg_iter,
				 smine_nelmts, dxpl_cache, bkg_buf/*out*/);
#ifdef H5S_DEBUG
            H5_timer_end(&(io_info->stats->stats[1].bkg_timer), &timer);
            io_info->stats->stats[1].bkg_nbytes += n * dst_type_size;
            io_info->stats->stats[1].bkg_ncalls++;
#endif
            if (n!=smine_nelmts)
                HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, "mem gather failed")
        } /* end if */

	/*
         * Perform datatype conversion.
         */
        if (H5T_convert(tpath, src_id, dst_id, smine_nelmts, (size_t)0, (size_t)0, tconv_buf, bkg_buf, io_info->dxpl_id) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "datatype conversion failed")

	/* Do the data transform after the conversion (since we're using type mem_type) */
        if(!H5Z_xform_noop(dxpl_cache->data_xform_prop))
             if( H5Z_xform_eval(dxpl_cache->data_xform_prop, tconv_buf, smine_nelmts, mem_type) < 0)
		    HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "Error performing data transform")

        /*
         * Scatter the data into memory.
         */
#ifdef H5S_DEBUG
	H5_timer_begin(&timer);
#endif
        status = H5D_select_mscat(tconv_buf, mem_space,
                          &mem_iter, smine_nelmts, dxpl_cache, buf/*out*/);
#ifdef H5S_DEBUG
	H5_timer_end(&(io_info->stats->stats[1].scat_timer), &timer);
	io_info->stats->stats[1].scat_nbytes += smine_nelmts * dst_type_size;
	io_info->stats->stats[1].scat_ncalls++;
#endif
	if (status<0)
            HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "scatter failed")
    } /* end for */

done:
    /* Release selection iterators */
    if(file_iter_init) {
        if(H5S_SELECT_ITER_RELEASE(&file_iter) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CANTFREE, FAIL, "Can't release selection iterator")
    } /* end if */
    if(mem_iter_init) {
        if(H5S_SELECT_ITER_RELEASE(&mem_iter) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CANTFREE, FAIL, "Can't release selection iterator")
    } /* end if */
    if(bkg_iter_init) {
        if(H5S_SELECT_ITER_RELEASE(&bkg_iter) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CANTFREE, FAIL, "Can't release selection iterator")
    } /* end if */

    if (tconv_buf && NULL==dxpl_cache->tconv_buf)
        (void)H5FL_BLK_FREE(type_conv,tconv_buf);
    if (bkg_buf && NULL==dxpl_cache->bkgr_buf)
        (void)H5FL_BLK_FREE(type_conv,bkg_buf);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D_contig_read() */


/*-------------------------------------------------------------------------
 * Function:	H5D_contig_write
 *
 * Purpose:	Write to a contiguous dataset.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *		Thursday, April 10, 2003
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_contig_write(H5D_io_info_t *io_info, hsize_t nelmts,
    const H5T_t *mem_type, const H5S_t *mem_space,
    const H5S_t *file_space, H5T_path_t *tpath,
    hid_t src_id, hid_t dst_id, const void *buf)
{
    H5D_t *dataset=io_info->dset;       /* Local pointer to dataset info */
    const H5D_dxpl_cache_t *dxpl_cache=io_info->dxpl_cache;     /* Local pointer to dataset transfer info */
    herr_t      status;                 /*function return status*/
#ifdef H5S_DEBUG
    H5_timer_t	timer;
#endif
    size_t	src_type_size;		/*size of source type	*/
    size_t	dst_type_size;	        /*size of destination type*/
    size_t	max_type_size;	        /* Size of largest source/destination type */
    size_t	target_size;		/*desired buffer size	*/
    size_t	request_nelmts;		/*requested strip mine	*/
    H5S_sel_iter_t mem_iter;            /*memory selection iteration info*/
    hbool_t	mem_iter_init=0;	/*memory selection iteration info has been initialized */
    H5S_sel_iter_t bkg_iter;            /*background iteration info*/
    hbool_t	bkg_iter_init=0;	/*background iteration info has been initialized */
    H5S_sel_iter_t file_iter;           /*file selection iteration info*/
    hbool_t	file_iter_init=0;	/*file selection iteration info has been initialized */
    H5T_bkg_t	need_bkg;		/*type of background buf*/
    uint8_t	*tconv_buf = NULL;	/*datatype conv buffer	*/
    uint8_t	*bkg_buf = NULL;	/*background buffer	*/
    hsize_t	smine_start;		/*strip mine start loc	*/
    size_t	n, smine_nelmts;	/*elements per strip	*/
    H5D_storage_t store;                /*union of storage info for dataset */

    herr_t	ret_value = SUCCEED;	/*return value		*/

    FUNC_ENTER_NOAPI_NOINIT(H5D_contig_write)

    assert (buf);

    /* Initialize storage info for this dataset */
    if(dataset->shared->dcpl_cache.efl.nused > 0)
        HDmemcpy(&store.efl, &(dataset->shared->dcpl_cache.efl), sizeof(H5O_efl_t));
    else {
        store.contig.dset_addr = dataset->shared->layout.u.contig.addr;
        store.contig.dset_size = dataset->shared->layout.u.contig.size;
    } /* end if */

    /* Set dataset storage for I/O info */
    io_info->store = &store;

    /*
     * If there is no type conversion then write directly from the
     * application's buffer.  This saves at least one mem-to-mem copy.
     */
    if(H5Z_xform_noop(dxpl_cache->data_xform_prop) && H5T_path_noop(tpath)) {
#ifdef H5S_DEBUG
	H5_timer_begin(&timer);
#endif
        H5_CHECK_OVERFLOW(nelmts,hsize_t,size_t);
#ifdef H5_HAVE_PARALLEL
	if(io_info->dxpl_cache->xfer_mode == H5FD_MPIO_COLLECTIVE) {
	  if(H5D_contig_collective_io(io_info, file_space, mem_space, buf, TRUE) < 0)
	    HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "contiguous write failed in collective mode")
	}
	else
#endif
        {
            if((io_info->ops.write)(io_info, (size_t)nelmts,
                    H5T_get_size(dataset->shared->type), file_space, mem_space,
                    (haddr_t)0, NULL, buf/*out*/) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "contiguous write failed ")
	}

#ifdef H5S_DEBUG
        H5_timer_end(&(io_info->stats->stats[0].write_timer), &timer);
        io_info->stats->stats[0].write_nbytes += nelmts * H5T_get_size(mem_type);
        io_info->stats->stats[0].write_ncalls++;
#endif

	    /* direct xfer accomplished successfully */
	    HGOTO_DONE(SUCCEED)
    } /* end if */

    /*
     * This is the general case.
     */
    if(nelmts==0)
        HGOTO_DONE(SUCCEED)

    /* Compute element sizes and other parameters */
    src_type_size = H5T_get_size(mem_type);
    dst_type_size = H5T_get_size(dataset->shared->type);
    max_type_size = MAX(src_type_size, dst_type_size);
    target_size = dxpl_cache->max_temp_buf;
    /* XXX: This could cause a problem if the user sets their buffer size
     * to the same size as the default, and then the dataset elements are
     * too large for the buffer... - QAK
     */
    if(target_size == H5D_TEMP_BUF_SIZE) {
        /* If the buffer is too small to hold even one element, make it bigger */
        if(target_size<max_type_size)
            target_size = max_type_size;
        /* If the buffer is too large to hold all the elements, make it smaller */
        else if(target_size>(nelmts*max_type_size))
            target_size=(size_t)(nelmts*max_type_size);
    } /* end if */
    request_nelmts = target_size / max_type_size;

    /* Sanity check elements in temporary buffer */
    if (request_nelmts==0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "temporary buffer max size is too small")

    /* Figure out the strip mine size. */
    if (H5S_select_iter_init(&file_iter, file_space, dst_type_size) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to initialize file selection information")
    file_iter_init=1;	/*file selection iteration info has been initialized */
    if (H5S_select_iter_init(&mem_iter, mem_space, src_type_size) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to initialize memory selection information")
    mem_iter_init=1;	/*file selection iteration info has been initialized */
    if (H5S_select_iter_init(&bkg_iter, file_space, dst_type_size) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to initialize background selection information")
    bkg_iter_init=1;	/*file selection iteration info has been initialized */

    /*
     * Get a temporary buffer for type conversion unless the app has already
     * supplied one through the xfer properties.  Instead of allocating a
     * buffer which is the exact size, we allocate the target size. The
     * malloc() is usually less resource-intensive if we allocate/free the
     * same size over and over.
     */
    if(H5T_detect_class(dataset->shared->type, H5T_VLEN)) {
	/* Old data is retrieved into background buffer for VL datatype.  The
	 * data is used later for freeing heap objects. */
        need_bkg = H5T_BKG_YES;
    } else if (H5T_path_bkg(tpath)) {
        H5T_bkg_t path_bkg;     /* Type conversion's background info */

        /* Retrieve the bkgr buffer property */
        need_bkg=dxpl_cache->bkgr_buf_type;
        path_bkg = H5T_path_bkg(tpath);
        need_bkg = MAX (path_bkg, need_bkg);
    } else {
        need_bkg = H5T_BKG_NO; /*never needed even if app says yes*/
    } /* end else */
    if(NULL == (tconv_buf = (uint8_t *)dxpl_cache->tconv_buf)) {
        /* Allocate temporary buffer */
        if(NULL == (tconv_buf = H5FL_BLK_MALLOC(type_conv, target_size)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for type conversion")
    } /* end if */
    if(need_bkg && NULL == (bkg_buf = (uint8_t *)dxpl_cache->bkgr_buf)) {
        /* Allocate background buffer */
        /* (Don't need calloc()-like call since file data is already initialized) */
        if(NULL == (bkg_buf = H5FL_BLK_MALLOC(type_conv, (request_nelmts * dst_type_size))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for background conversion")
    } /* end if */

    /* Start strip mining... */
    for (smine_start=0; smine_start<nelmts; smine_start+=smine_nelmts) {
        /* Go figure out how many elements to read from the file */
        assert(H5S_SELECT_ITER_NELMTS(&file_iter)==(nelmts-smine_start));
        smine_nelmts = (size_t)MIN(request_nelmts, (nelmts-smine_start));

        /*
         * Gather data from application buffer into the datatype conversion
         * buffer. Also gather data from the file into the background buffer
         * if necessary.
         */
#ifdef H5S_DEBUG
	H5_timer_begin(&timer);
#endif
        n = H5D_select_mgath(buf, mem_space, &mem_iter,
			     smine_nelmts, dxpl_cache, tconv_buf/*out*/);
#ifdef H5S_DEBUG
	H5_timer_end(&(io_info->stats->stats[0].gath_timer), &timer);
	io_info->stats->stats[0].gath_nbytes += n * src_type_size;
	io_info->stats->stats[0].gath_ncalls++;
#endif
        if (n!=smine_nelmts)
            HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "mem gather failed")

        /* If the source and destination are compound types and the destination is
         * is a subset of the source and no conversion is needed, copy the data 
         * directly into user's buffer and bypass the rest of steps.  If the source
         * is a subset of the destination, the optimization is done in conversion
         * function H5T_conv_struct_opt to protect the background data.  This
         * optimization is for Chicago company */
        if(H5T_SUBSET_DST==H5T_path_compound_subset(tpath)) {
            if(H5D_compound_opt_write(smine_nelmts, src_id, dst_id, tconv_buf)<0)
                HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "datatype conversion failed")
        } else {
            if (H5T_BKG_YES==need_bkg) {
#ifdef H5S_DEBUG
                H5_timer_begin(&timer);
#endif
                n = H5D_select_fgath(io_info, file_space, &bkg_iter, smine_nelmts,
                                    (haddr_t)0, NULL, bkg_buf/*out*/);

#ifdef H5S_DEBUG
                H5_timer_end(&(io_info->stats->stats[0].bkg_timer), &timer);
                io_info->stats->stats[0].bkg_nbytes += n * dst_type_size;
                io_info->stats->stats[0].bkg_ncalls++;
#endif
                if (n!=smine_nelmts)
                    HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "file gather failed")
            } /* end if */

	    /*
             * Perform datatype conversion.
             */
            if(H5T_convert(tpath, src_id, dst_id, smine_nelmts, (size_t)0, (size_t)0, tconv_buf, bkg_buf, io_info->dxpl_id) < 0)
                 HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "datatype conversion failed")

	    /* Do the data transform after the type conversion (since we're using dataset->shared->type). */
	    if(!H5Z_xform_noop(dxpl_cache->data_xform_prop))
	        if( H5Z_xform_eval(dxpl_cache->data_xform_prop, tconv_buf, smine_nelmts, dataset->shared->type) < 0)
		    HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "Error performing data transform")
        }

        /*
         * Scatter the data out to the file.
         */
#ifdef H5S_DEBUG
        H5_timer_begin(&timer);
#endif
	status = H5D_select_fscat(io_info, file_space, &file_iter, smine_nelmts,
                    (haddr_t)0, NULL, tconv_buf);
#ifdef H5S_DEBUG
        H5_timer_end(&(io_info->stats->stats[0].scat_timer), &timer);
        io_info->stats->stats[0].scat_nbytes += smine_nelmts * dst_type_size;
        io_info->stats->stats[0].scat_ncalls++;
#endif
        if (status<0)
            HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "scatter failed")
    } /* end for */

done:
    /* Release selection iterators */
    if(file_iter_init) {
        if(H5S_SELECT_ITER_RELEASE(&file_iter) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CANTFREE, FAIL, "Can't release selection iterator")
    } /* end if */
    if(mem_iter_init) {
        if(H5S_SELECT_ITER_RELEASE(&mem_iter) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CANTFREE, FAIL, "Can't release selection iterator")
    } /* end if */
    if(bkg_iter_init) {
        if(H5S_SELECT_ITER_RELEASE(&bkg_iter) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CANTFREE, FAIL, "Can't release selection iterator")
    } /* end if */

    if (tconv_buf && NULL==dxpl_cache->tconv_buf)
        (void)H5FL_BLK_FREE(type_conv,tconv_buf);
    if (bkg_buf && NULL==dxpl_cache->bkgr_buf)
        (void)H5FL_BLK_FREE(type_conv,bkg_buf);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D_contig_write() */


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
 * Modification: 
 *              Raymond Lu
 *              20 July 2007
 *              Moved H5D_istore_lock and H5D_istore_unlock to this level
 *              from H5D_istore_readvv to avoid frequent lock and unlock
 *              and to improve performance.
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_chunk_read(H5D_io_info_t *io_info, hsize_t nelmts,
    const H5T_t *mem_type, const H5S_t *mem_space,
    const H5S_t *file_space, H5T_path_t *tpath,
    hid_t src_id, hid_t dst_id, void *buf/*out*/)
{
    H5D_t *dataset=io_info->dset;       /* Local pointer to dataset info */
    const H5D_dxpl_cache_t *dxpl_cache=io_info->dxpl_cache;     /* Local pointer to dataset transfer info */
    H5D_chunk_map_t      fm;            /* File<->memory mapping */
    H5SL_node_t *chunk_node;            /* Current node in chunk skip list */
    herr_t      status;                 /*function return status*/
#ifdef H5S_DEBUG
    H5_timer_t	timer;
#endif
    size_t	src_type_size;		/*size of source type	*/
    size_t	dst_type_size;	        /*size of destination type*/
    size_t	max_type_size;	        /* Size of largest source/destination type */
    size_t	target_size;		/*desired buffer size	*/
    size_t	request_nelmts;		/*requested strip mine	*/
    hsize_t     smine_start;            /*strip mine start loc  */
    size_t      n, smine_nelmts;        /*elements per strip    */
    size_t      accessed_bytes = 0;     /*total accessed size in a chunk */
    H5S_sel_iter_t mem_iter;            /*memory selection iteration info*/
    hbool_t	mem_iter_init = FALSE;	/*memory selection iteration info has been initialized */
    H5S_sel_iter_t bkg_iter;            /*background iteration info*/
    hbool_t	bkg_iter_init = FALSE;	/*background iteration info has been initialized */
    H5S_sel_iter_t file_iter;           /*file selection iteration info*/
    hbool_t	file_iter_init = FALSE;	/*file selection iteration info has been initialized */
    H5T_bkg_t	need_bkg;		/*type of background buf*/
    uint8_t	*tconv_buf = NULL;	/*datatype conv buffer	*/
    uint8_t	*bkg_buf = NULL;	/*background buffer	*/
    H5D_storage_t store;                /*union of EFL and chunk pointer in file space */
    void        *chunk = NULL;
    haddr_t	chunk_addr;             /* Chunk address on disk */
    H5D_istore_ud1_t udata;		/*B-tree pass-through	*/
    unsigned    idx_hint=0;             /* Cache index hint      */
    herr_t	ret_value = SUCCEED;	/*return value		*/

    FUNC_ENTER_NOAPI_NOINIT(H5D_chunk_read)

    assert (buf);

    /* Map elements between file and memory for each chunk*/
    if(H5D_create_chunk_map(&fm, io_info, nelmts, file_space, mem_space, mem_type) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "can't build chunk mapping")

    /* Set dataset storage for I/O info */
    io_info->store=&store;

    /* Compute element sizes */
    src_type_size = H5T_get_size(dataset->shared->type);
    dst_type_size = H5T_get_size(mem_type);
    max_type_size = MAX(src_type_size, dst_type_size);

    /*
     * If there is no type conversion then read directly into the
     * application's buffer.  This saves at least one mem-to-mem copy.
     */
    if ( H5Z_xform_noop(dxpl_cache->data_xform_prop) && H5T_path_noop(tpath)) {
#ifdef H5S_DEBUG
	H5_timer_begin(&timer);
#endif
  	/* Sanity check dataset, then read it */
        HDassert(((dataset->shared->layout.type == H5D_CONTIGUOUS && H5F_addr_defined(dataset->shared->layout.u.contig.addr))
                || (dataset->shared->layout.type == H5D_CHUNKED && H5F_addr_defined(dataset->shared->layout.u.chunk.addr)))
                || dataset->shared->dcpl_cache.efl.nused > 0 || 0 == nelmts
                || dataset->shared->layout.type == H5D_COMPACT);

#ifdef H5_HAVE_PARALLEL
        /* Check whether the collective mode can be turned off globally*/
#ifndef H5_MPI_SPECIAL_COLLECTIVE_IO_WORKS
        if(io_info->dxpl_cache->xfer_mode == H5FD_MPIO_COLLECTIVE) {
            if(H5D_mpio_chunk_adjust_iomode(io_info ,&fm))
                HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't adjust collective I/O")
        }
#endif /* H5_MPI_SPECIAL_COLLECTIVE_IO_WORKS */
	/* Temporarily shut down collective IO for chunking */
	if(io_info->dxpl_cache->xfer_mode == H5FD_MPIO_COLLECTIVE) {
            if(H5D_chunk_collective_io(io_info, &fm, buf, FALSE) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "chunked read failed in collective mode")
	}
	else {/* sequential or independent read */
#endif
            /* Get first node in chunk skip list */
            chunk_node = H5D_CHUNK_GET_FIRST_NODE(fm);

            while(chunk_node) {
                H5D_chunk_info_t *chunk_info;   /* chunk information */

                /* Get the actual chunk information from the skip list node */
                chunk_info = H5D_CHUNK_GET_NODE_INFO(fm, chunk_node);

                /* Pass in chunk's coordinates in a union. */
                store.chunk.offset = chunk_info->coords;
                store.chunk.index = chunk_info->index;

                /* Load the chunk into cache and lock it. */
		chunk_addr = H5D_istore_get_addr(io_info, &udata);
                if(H5D_istore_if_load(io_info, chunk_addr)) {
		    if(NULL == (chunk = H5D_istore_lock(io_info, &udata, FALSE, &idx_hint)))
			HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, "unable to read raw data chunk")
		} else
		    chunk = NULL;

                /* Perform the actual read operation */
                if((io_info->ops.read)(io_info, chunk_info->chunk_points,
                        src_type_size, chunk_info->fspace,
                        chunk_info->mspace, chunk_addr, chunk, buf) < 0)
                    HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, " chunked read failed")
  
                /* Release the cache lock on the chunk. */
                if(chunk) {
                    accessed_bytes = chunk_info->chunk_points * src_type_size;
		    if(H5D_istore_unlock(io_info, FALSE, idx_hint, chunk, accessed_bytes) < 0)
			HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, "unable to unlock raw data chunk")
		} /* end if */

                /* Advance to next chunk in list */
                chunk_node = H5D_CHUNK_GET_NEXT_NODE(fm, chunk_node);
            } /* end while */
#ifdef H5_HAVE_PARALLEL
	}
#endif


#ifdef H5S_DEBUG
        H5_timer_end(&(io_info->stats->stats[1].read_timer), &timer);
        io_info->stats->stats[1].read_nbytes += nelmts * src_type_size;
        io_info->stats->stats[1].read_ncalls++;
#endif

	/* direct xfer accomplished successfully */
        HGOTO_DONE(SUCCEED)
    } /* end if */

    /*
     * This is the general case (type conversion, usually).
     */
    if(nelmts==0)
        HGOTO_DONE(SUCCEED)

    /* Compute buffer  sizes and other parameters */
    target_size = dxpl_cache->max_temp_buf;
    /* XXX: This could cause a problem if the user sets their buffer size
     * to the same size as the default, and then the dataset elements are
     * too large for the buffer... - QAK
     */
    if(target_size == H5D_TEMP_BUF_SIZE) {
        /* If the buffer is too small to hold even one element, make it bigger */
        if(target_size<max_type_size)
            target_size = max_type_size;
        /* If the buffer is too large to hold all the elements, make it smaller */
        else if(target_size>(nelmts*max_type_size))
            target_size=(size_t)(nelmts*max_type_size);
    } /* end if */
    request_nelmts = target_size / max_type_size;

    /* Sanity check elements in temporary buffer */
    if (request_nelmts==0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "temporary buffer max size is too small")

    /*
     * Get a temporary buffer for type conversion unless the app has already
     * supplied one through the xfer properties. Instead of allocating a
     * buffer which is the exact size, we allocate the target size.  The
     * malloc() is usually less resource-intensive if we allocate/free the
     * same size over and over.
     */
    if (H5T_path_bkg(tpath)) {
        H5T_bkg_t path_bkg;     /* Type conversion's background info */

        /* Retrieve the bkgr buffer property */
        need_bkg=dxpl_cache->bkgr_buf_type;
        path_bkg = H5T_path_bkg(tpath);
        need_bkg = MAX(path_bkg, need_bkg);
    } else {
        need_bkg = H5T_BKG_NO; /*never needed even if app says yes*/
    } /* end else */
    if(NULL == (tconv_buf = (uint8_t *)dxpl_cache->tconv_buf)) {
        /* Allocate temporary buffer */
        if(NULL == (tconv_buf = H5FL_BLK_MALLOC(type_conv, target_size)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for type conversion")
    } /* end if */
    if(need_bkg && NULL == (bkg_buf = (uint8_t *)dxpl_cache->bkgr_buf)) {
        /* Allocate background buffer */
        /* (Need calloc()-like call since memory needs to be initialized) */
        if(NULL == (bkg_buf = H5FL_BLK_CALLOC(type_conv, (request_nelmts * dst_type_size))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for background conversion")
    } /* end if */

    /* Loop over all the chunks, performing I/O on each */

    /* Get first node in chunk skip list */
    chunk_node = H5D_CHUNK_GET_FIRST_NODE(fm);

    /* Iterate through chunks to be operated on */
    while(chunk_node) {
        H5D_chunk_info_t *chunk_info;   /* chunk information */

        /* Get the actual chunk information from the skip list nodes */
        chunk_info = H5D_CHUNK_GET_NODE_INFO(fm, chunk_node);

        /* initialize selection iterator */
        if (H5S_select_iter_init(&file_iter, chunk_info->fspace, src_type_size) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to initialize file selection information")
        file_iter_init = TRUE;	/*file selection iteration info has been initialized */
        if (H5S_select_iter_init(&mem_iter, chunk_info->mspace, dst_type_size) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to initialize memory selection information")
        mem_iter_init = TRUE;	/*file selection iteration info has been initialized */
        if (H5S_select_iter_init(&bkg_iter, chunk_info->mspace, dst_type_size) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to initialize background selection information")
        bkg_iter_init = TRUE;	/*file selection iteration info has been initialized */

        /* Pass in chunk's coordinates in a union*/
        store.chunk.offset = chunk_info->coords;
        store.chunk.index = chunk_info->index;

        /* Load the chunk into cache and lock it. */
        chunk_addr = H5D_istore_get_addr(io_info, &udata);

        if(H5D_istore_if_load(io_info, chunk_addr)) {
            if(NULL == (chunk = H5D_istore_lock(io_info, &udata, FALSE, &idx_hint)))
                HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, "unable to read raw data chunk")
        } else
            chunk = NULL;

        for(smine_start=0; smine_start<chunk_info->chunk_points; smine_start+=smine_nelmts) {
            /* Go figure out how many elements to read from the file */
            assert(H5S_SELECT_ITER_NELMTS(&file_iter)==(chunk_info->chunk_points-smine_start));
            smine_nelmts = (size_t)MIN(request_nelmts, (chunk_info->chunk_points-smine_start));

            /*
             * Gather the data from disk into the datatype conversion
             * buffer. Also gather data from application to background buffer
             * if necessary.
             */
#ifdef H5S_DEBUG
            H5_timer_begin(&timer);
#endif
            /* Sanity check that space is allocated, then read data from it */
            HDassert(((dataset->shared->layout.type == H5D_CONTIGUOUS && H5F_addr_defined(dataset->shared->layout.u.contig.addr))
                    || (dataset->shared->layout.type == H5D_CHUNKED && H5F_addr_defined(dataset->shared->layout.u.chunk.addr)))
                || dataset->shared->dcpl_cache.efl.nused > 0 || dataset->shared->layout.type == H5D_COMPACT);

            if(chunk) {
                n = H5D_select_mgath(chunk, chunk_info->fspace, &file_iter, 
                                 smine_nelmts, dxpl_cache, tconv_buf/*out*/);
            } else {
                n = H5D_select_fgath(io_info, chunk_info->fspace, &file_iter, smine_nelmts, 
                                 chunk_addr, NULL, tconv_buf/*out*/);
            }

#ifdef H5S_DEBUG
            H5_timer_end(&(io_info->stats->stats[1].gath_timer), &timer);
            io_info->stats->stats[1].gath_nbytes += n * src_type_size;
            io_info->stats->stats[1].gath_ncalls++;
#endif
            if(n != smine_nelmts)
                HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, "file gather failed")

            /* If the source and destination are compound types and subset of each other
             * and no conversion is needed, copy the data directly into user's buffer and 
             * bypass the rest of steps.  This optimization is for Chicago company */
            if(H5T_SUBSET_SRC==H5T_path_compound_subset(tpath)) {
                if(H5D_compound_opt_read(smine_nelmts, chunk_info->mspace, &mem_iter, dxpl_cache,
                    src_id, dst_id, H5T_SUBSET_SRC, tconv_buf, buf /*out*/)<0)
                    HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "datatype conversion failed")

                continue;
            } else if(H5T_SUBSET_DST==H5T_path_compound_subset(tpath)) {
                if(H5D_compound_opt_read(smine_nelmts, chunk_info->mspace, &mem_iter, dxpl_cache,
                    src_id, dst_id, H5T_SUBSET_DST, tconv_buf, buf /*out*/)<0)
                    HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "datatype conversion failed")

                continue;
            }

            if(H5T_BKG_YES == need_bkg) {
#ifdef H5S_DEBUG
                H5_timer_begin(&timer);
#endif
                n = H5D_select_mgath(buf, chunk_info->mspace, &bkg_iter, smine_nelmts, dxpl_cache, bkg_buf/*out*/);
#ifdef H5S_DEBUG
                H5_timer_end(&(io_info->stats->stats[1].bkg_timer), &timer);
                io_info->stats->stats[1].bkg_nbytes += n * dst_type_size;
                io_info->stats->stats[1].bkg_ncalls++;
#endif
                if(n != smine_nelmts)
                    HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, "mem gather failed")
            } /* end if */

            /*
             * Perform datatype conversion.
             */
            if(H5T_convert(tpath, src_id, dst_id, smine_nelmts, (size_t)0, (size_t)0, tconv_buf, bkg_buf, io_info->dxpl_id) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "datatype conversion failed")

           /* Do the data transform after the conversion (since we're using type mem_type) */
            if(!H5Z_xform_noop(dxpl_cache->data_xform_prop))
	        if(H5Z_xform_eval(dxpl_cache->data_xform_prop, tconv_buf, smine_nelmts, mem_type) < 0)
                    HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "Error performing data transform")

            /*
             * Scatter the data into memory.
             */
#ifdef H5S_DEBUG
            H5_timer_begin(&timer);
#endif
            status = H5D_select_mscat(tconv_buf, chunk_info->mspace, &mem_iter, smine_nelmts, dxpl_cache, buf/*out*/);
#ifdef H5S_DEBUG
            H5_timer_end(&(io_info->stats->stats[1].scat_timer), &timer);
            io_info->stats->stats[1].scat_nbytes += smine_nelmts * dst_type_size;
            io_info->stats->stats[1].scat_ncalls++;
#endif
            if(status < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "scatter failed")
        } /* end for */

        /* Release the cache lock on the chunk. */
        if(chunk) {
            accessed_bytes = chunk_info->chunk_points * src_type_size;
            if(H5D_istore_unlock(io_info, FALSE, idx_hint, chunk, accessed_bytes) < 0)
                HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, "unable to unlock raw data chunk")
        } /* end if */

        /* Release selection iterators */
        if(file_iter_init) {
            if(H5S_SELECT_ITER_RELEASE(&file_iter) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_CANTFREE, FAIL, "Can't release selection iterator")
            file_iter_init = FALSE;
        } /* end if */
        if(mem_iter_init) {
            if(H5S_SELECT_ITER_RELEASE(&mem_iter) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_CANTFREE, FAIL, "Can't release selection iterator")
            mem_iter_init = FALSE;
        } /* end if */
        if(bkg_iter_init) {
            if(H5S_SELECT_ITER_RELEASE(&bkg_iter) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_CANTFREE, FAIL, "Can't release selection iterator")
            bkg_iter_init = FALSE;
        } /* end if */

        /* Get the next chunk node in the skip list */
        chunk_node = H5D_CHUNK_GET_NEXT_NODE(fm, chunk_node);
    } /* end while */

done:
    /* Release selection iterators, if necessary */
    if(file_iter_init) {
        if(H5S_SELECT_ITER_RELEASE(&file_iter) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CANTFREE, FAIL, "Can't release selection iterator")
    } /* end if */
    if(mem_iter_init) {
        if(H5S_SELECT_ITER_RELEASE(&mem_iter) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CANTFREE, FAIL, "Can't release selection iterator")
    } /* end if */
    if(bkg_iter_init) {
        if(H5S_SELECT_ITER_RELEASE(&bkg_iter) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CANTFREE, FAIL, "Can't release selection iterator")
    } /* end if */

    if(tconv_buf && NULL == dxpl_cache->tconv_buf)
        (void)H5FL_BLK_FREE(type_conv, tconv_buf);
    if(bkg_buf && NULL == dxpl_cache->bkgr_buf)
        (void)H5FL_BLK_FREE(type_conv, bkg_buf);

    /* Release chunk mapping information */
    if(H5D_destroy_chunk_map(&fm) < 0)
        HDONE_ERROR(H5E_DATASET, H5E_CANTRELEASE, FAIL, "can't release chunk mapping")

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
 * Modification: 
 *              Raymond Lu
 *              20 July 2007
 *              Moved H5D_istore_lock and H5D_istore_unlock to this level
 *              from H5D_istore_writevv to avoid frequent lock and unlock
 *              and to improve performance.
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_chunk_write(H5D_io_info_t *io_info, hsize_t nelmts,
    const H5T_t *mem_type, const H5S_t *mem_space,
    const H5S_t *file_space, H5T_path_t *tpath,
    hid_t src_id, hid_t dst_id, const void *buf)
{
    H5D_t *dataset=io_info->dset;       /* Local pointer to dataset info */
    const H5D_dxpl_cache_t *dxpl_cache=io_info->dxpl_cache;     /* Local pointer to dataset transfer info */
    H5D_chunk_map_t      fm;            /* File<->memory mapping */
    H5SL_node_t *chunk_node;            /* Current node in chunk skip list */
    herr_t      status;                 /*function return status*/
#ifdef H5S_DEBUG
    H5_timer_t	timer;
#endif
    size_t	src_type_size;		/*size of source type	*/
    size_t	dst_type_size;	        /*size of destination type*/
    size_t	max_type_size;	        /* Size of largest source/destination type */
    size_t	target_size;		/*desired buffer size	*/
    size_t	request_nelmts;		/*requested strip mine	*/
    size_t      accessed_bytes;         /*total accessed size in a chunk */
    hsize_t     smine_start;            /*strip mine start loc  */
    size_t      n, smine_nelmts;        /*elements per strip    */
    H5S_sel_iter_t mem_iter;            /*memory selection iteration info*/
    hbool_t	mem_iter_init=0;	/*memory selection iteration info has been initialized */
    H5S_sel_iter_t bkg_iter;            /*background iteration info*/
    hbool_t	bkg_iter_init=0;	/*background iteration info has been initialized */
    H5S_sel_iter_t file_iter;           /*file selection iteration info*/
    hbool_t	file_iter_init=0;	/*file selection iteration info has been initialized */
    H5T_bkg_t	need_bkg;		/*type of background buf*/
    uint8_t	*tconv_buf = NULL;	/*datatype conv buffer	*/
    uint8_t	*bkg_buf = NULL;	/*background buffer	*/
    H5D_storage_t store;                /*union of EFL and chunk pointer in file space */
    void     *chunk = NULL;
    haddr_t	chunk_addr;             /* Chunk address on disk */
    H5D_istore_ud1_t udata;		/*B-tree pass-through	*/
    unsigned    idx_hint=0;             /* Cache index hint      */
    hbool_t     relax=TRUE;             /* Whether whole chunk is selected */
    herr_t	ret_value = SUCCEED;	/*return value		*/

    FUNC_ENTER_NOAPI_NOINIT(H5D_chunk_write)

    assert (buf);

    /* Map elements between file and memory for each chunk*/
    if(H5D_create_chunk_map(&fm, io_info, nelmts, file_space, mem_space, mem_type) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "can't build chunk mapping")

    /* Set dataset storage for I/O info */
    io_info->store=&store;

#ifdef H5_HAVE_PARALLEL
    /* Additional sanity checks when operating in parallel */
    if(io_info->using_mpi_vfd) {
        if (chunk_addr==HADDR_UNDEF)
            HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "unable to locate raw data chunk")
        if (dataset->shared->dcpl_cache.pline.nused>0)
            HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "cannot write to chunked storage with filters in parallel")
    } /* end if */
#endif /* H5_HAVE_PARALLEL */

    /* Compute element sizes and other parameters */
    src_type_size = H5T_get_size(mem_type);
    dst_type_size = H5T_get_size(dataset->shared->type);
    max_type_size = MAX(src_type_size, dst_type_size);

    /*
     * If there is no type conversion then write directly from the
     * application's buffer.  This saves at least one mem-to-mem copy.
     */
    if ( H5Z_xform_noop(dxpl_cache->data_xform_prop) && H5T_path_noop(tpath)) {
#ifdef H5S_DEBUG
	H5_timer_begin(&timer);
#endif

#ifdef H5_HAVE_PARALLEL
        /* Check whether the collective mode can be turned off globally*/
#ifndef H5_MPI_SPECIAL_COLLECTIVE_IO_WORKS
        if(io_info->dxpl_cache->xfer_mode == H5FD_MPIO_COLLECTIVE) {
            if(H5D_mpio_chunk_adjust_iomode(io_info,&fm))
                HGOTO_ERROR(H5E_DATASET,H5E_CANTGET,FAIL,"can't adjust collective I/O")
        }
#endif /* H5_MPI_SPECIAL_COLLECTIVE_IO_WORKS */
	/* Temporarily shut down collective IO for chunking */
        if(io_info->dxpl_cache->xfer_mode == H5FD_MPIO_COLLECTIVE) {
            if(H5D_chunk_collective_io(io_info,&fm,buf,TRUE) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "chunked write failed in collective mode");
	}
	else {/* sequential or independent write */
#endif /* H5_HAVE_PARALLEL */
            /* Get first node in chunk skip list */
            chunk_node = H5D_CHUNK_GET_FIRST_NODE(fm);

            while(chunk_node) {
                H5D_chunk_info_t *chunk_info;       /* Chunk information */

                /* Get the actual chunk information from the skip list node */
                chunk_info = H5D_CHUNK_GET_NODE_INFO(fm, chunk_node);

                /* Pass in chunk's coordinates in a union. */
                store.chunk.offset = chunk_info->coords;
                store.chunk.index = chunk_info->index;

                /* Load the chunk into cache.  But if the whole chunk is written,
                 * simply allocate space instead of load the chunk. */
                chunk_addr = H5D_istore_get_addr(io_info, &udata);

                if(H5D_istore_if_load(io_info, chunk_addr)) {
                    accessed_bytes = chunk_info->chunk_points * dst_type_size;
                    if(accessed_bytes != dataset->shared->layout.u.chunk.size)
                        relax = FALSE;

		    if(NULL == (chunk = H5D_istore_lock(io_info, &udata, relax, &idx_hint)))
			HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, "unable to read raw data chunk")
		} else
		    chunk = NULL;

                /* Perform the actual read operation */
                if((io_info->ops.write)(io_info, chunk_info->chunk_points,
                        dst_type_size, chunk_info->fspace,
                        chunk_info->mspace, chunk_addr, chunk, buf) < 0)
                    HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, " chunked write failed")

                /* Release the cache lock on the chunk. */
                if(chunk) {
		    if(H5D_istore_unlock(io_info, TRUE, idx_hint, chunk, accessed_bytes) < 0)
			HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, "unable to unlock raw data chunk")
		} /* end if */
                relax = TRUE;

                /* Advance to next chunk in list */
                chunk_node = H5D_CHUNK_GET_NEXT_NODE(fm, chunk_node);
            } /* end while */
#ifdef H5_HAVE_PARALLEL
	}
#endif

#ifdef H5S_DEBUG
	H5_timer_end(&(io_info->stats->stats[0].write_timer), &timer);
	io_info->stats->stats[0].write_nbytes += nelmts * src_type_size;
	io_info->stats->stats[0].write_ncalls++;
#endif

	/* direct xfer accomplished successfully */
	HGOTO_DONE(SUCCEED)
    } /* end if */

    /*
     * This is the general case (type conversion, usually).
     */
    if(nelmts==0)
        HGOTO_DONE(SUCCEED)

    /* Compute buffer sizes and other parameters */
    target_size = dxpl_cache->max_temp_buf;
    /* XXX: This could cause a problem if the user sets their buffer size
     * to the same size as the default, and then the dataset elements are
     * too large for the buffer... - QAK
     */
    if(target_size == H5D_TEMP_BUF_SIZE) {
        /* If the buffer is too small to hold even one element, make it bigger */
        if(target_size<max_type_size)
            target_size = max_type_size;
        /* If the buffer is too large to hold all the elements, make it smaller */
        else if(target_size>(nelmts*max_type_size))
            target_size=(size_t)(nelmts*max_type_size);
    } /* end if */
    request_nelmts = target_size / max_type_size;

    /* Sanity check elements in temporary buffer */
    if (request_nelmts==0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "temporary buffer max size is too small")

    /*
     * Get a temporary buffer for type conversion unless the app has already
     * supplied one through the xfer properties.  Instead of allocating a
     * buffer which is the exact size, we allocate the target size. The
     * malloc() is usually less resource-intensive if we allocate/free the
     * same size over and over.
     */
    if(H5T_detect_class(dataset->shared->type, H5T_VLEN)) {
	/* Old data is retrieved into background buffer for VL datatype.  The
	 * data is used later for freeing heap objects. */
        need_bkg = H5T_BKG_YES;
    } else if (H5T_path_bkg(tpath)) {
        H5T_bkg_t path_bkg;     /* Type conversion's background info */

        /* Retrieve the bkgr buffer property */
        need_bkg=dxpl_cache->bkgr_buf_type;
        path_bkg = H5T_path_bkg(tpath);
        need_bkg = MAX (path_bkg, need_bkg);
    } else {
        need_bkg = H5T_BKG_NO; /*never needed even if app says yes*/
    } /* end else */
    if(NULL == (tconv_buf = (uint8_t *)dxpl_cache->tconv_buf)) {
        /* Allocate temporary buffer */
        if(NULL == (tconv_buf = H5FL_BLK_MALLOC(type_conv, target_size)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for type conversion")
    } /* end if */
    if(need_bkg && NULL == (bkg_buf = (uint8_t *)dxpl_cache->bkgr_buf)) {
        /* Allocate background buffer */
        /* (Don't need calloc()-like call since file data is already initialized) */
        if(NULL == (bkg_buf = H5FL_BLK_MALLOC(type_conv, (request_nelmts * dst_type_size))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for background conversion")
    } /* end if */

    /* Loop over all the chunks, performing I/O on each */

    /* Get first node in chunk skip list */
    chunk_node = H5D_CHUNK_GET_FIRST_NODE(fm);

    /* Iterate through chunks to be operated on */
    while(chunk_node) {
        H5D_chunk_info_t *chunk_info;   /* chunk information */

        /* Get the actual chunk information from the skip list node */
        chunk_info = H5D_CHUNK_GET_NODE_INFO(fm, chunk_node);

        /* initialize selection iterator */
        if (H5S_select_iter_init(&file_iter, chunk_info->fspace, dst_type_size) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to initialize file selection information")
        file_iter_init=1;	/*file selection iteration info has been initialized */
        if (H5S_select_iter_init(&mem_iter, chunk_info->mspace, src_type_size) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to initialize memory selection information")
        mem_iter_init=1;	/*file selection iteration info has been initialized */
        if (H5S_select_iter_init(&bkg_iter, chunk_info->fspace, dst_type_size) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to initialize background selection information")
        bkg_iter_init=1;	/*file selection iteration info has been initialized */

        /*pass in chunk's coordinates in a union*/
        store.chunk.offset = chunk_info->coords;
        store.chunk.index = chunk_info->index;

        /* Load the chunk into cache.  But if the whole chunk is written,
         * simply allocate space instead of load the chunk. */
        chunk_addr = H5D_istore_get_addr(io_info, &udata);

        if(H5D_istore_if_load(io_info, chunk_addr)) {
            accessed_bytes = chunk_info->chunk_points * dst_type_size;
            if(accessed_bytes != dataset->shared->layout.u.chunk.size)
                relax=FALSE;
            if(relax) {
                accessed_bytes = H5S_GET_SELECT_NPOINTS(chunk_info->mspace) * src_type_size;
                if(accessed_bytes != dataset->shared->layout.u.chunk.size)
                    relax = FALSE;
            }

	    if(NULL == (chunk = H5D_istore_lock(io_info, &udata, relax, &idx_hint)))
		HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, "unable to read raw data chunk")
	} else
	    chunk = NULL;

        for(smine_start=0; smine_start<chunk_info->chunk_points; smine_start+=smine_nelmts) {
            /* Go figure out how many elements to read from the file */
            assert(H5S_SELECT_ITER_NELMTS(&file_iter)==(chunk_info->chunk_points-smine_start));
            smine_nelmts = (size_t)MIN(request_nelmts, (chunk_info->chunk_points-smine_start));

            /*
             * Gather the data from disk into the datatype conversion
             * buffer. Also gather data from application to background buffer
             * if necessary.
             */
#ifdef H5S_DEBUG
            H5_timer_begin(&timer);
#endif
            n = H5D_select_mgath(buf, chunk_info->mspace, &mem_iter,
                                 smine_nelmts, dxpl_cache, tconv_buf/*out*/);

#ifdef H5S_DEBUG
            H5_timer_end(&(io_info->stats->stats[1].gath_timer), &timer);
            io_info->stats->stats[1].gath_nbytes += n * src_type_size;
            io_info->stats->stats[1].gath_ncalls++;
#endif
            if (n!=smine_nelmts)
                HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, "file gather failed")

            /* If the source and destination are compound types and the destination is
             * is a subset of the source and no conversion is needed, copy the data 
             * directly into user's buffer and bypass the rest of steps.  If the source
             * is a subset of the destination, the optimization is done in conversion
             * function H5T_conv_struct_opt to protect the background data.  This
             * optimization is for Chicago company */
            if(H5T_SUBSET_DST==H5T_path_compound_subset(tpath)) {
                if(H5D_compound_opt_write(smine_nelmts, src_id, dst_id, tconv_buf)<0)
                    HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "datatype conversion failed")
            } else {
                if (H5T_BKG_YES==need_bkg) {
#ifdef H5S_DEBUG
                    H5_timer_begin(&timer);
#endif
                    if(chunk) {
                        n = H5D_select_mgath(chunk, chunk_info->fspace, &bkg_iter, 
                                 smine_nelmts, dxpl_cache, bkg_buf/*out*/);
                    } else {
                        n = H5D_select_fgath(io_info, chunk_info->fspace, &bkg_iter, smine_nelmts, 
                                 chunk_addr, NULL, bkg_buf/*out*/);
                    }

#ifdef H5S_DEBUG
                    H5_timer_end(&(io_info->stats->stats[0].bkg_timer), &timer);
                    io_info->stats->stats[0].bkg_nbytes += n * dst_type_size;
                    io_info->stats->stats[0].bkg_ncalls++;
#endif
                    if (n!=smine_nelmts)
                        HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "file gather failed")
                } /* end if */

	        /*
                 * Perform datatype conversion.
                 */
                if(H5T_convert(tpath, src_id, dst_id, smine_nelmts, (size_t)0, (size_t)0, tconv_buf, bkg_buf, io_info->dxpl_id) < 0)
                    HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "datatype conversion failed")

 	        /* Do the data transform after the type conversion (since we're using dataset->shared->type) */
                if(!H5Z_xform_noop(dxpl_cache->data_xform_prop))
	            if( H5Z_xform_eval(dxpl_cache->data_xform_prop, tconv_buf, smine_nelmts, dataset->shared->type) < 0)
		        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "Error performing data transform")
            }

            /*
             * Scatter the data out to the file.
             */
#ifdef H5S_DEBUG
            H5_timer_begin(&timer);
#endif
            status = H5D_select_fscat(io_info,
                chunk_info->fspace, &file_iter, smine_nelmts,
                chunk_addr, chunk, tconv_buf);

#ifdef H5S_DEBUG
            H5_timer_end(&(io_info->stats->stats[0].scat_timer), &timer);
            io_info->stats->stats[0].scat_nbytes += n * dst_type_size;
            io_info->stats->stats[0].scat_ncalls++;
#endif
            if (status<0)
                HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "scatter failed")
        } /* end for */

        /* Release the cache lock on the chunk. */
        if(chunk) {
            accessed_bytes = chunk_info->chunk_points * dst_type_size;
	    if(H5D_istore_unlock(io_info, TRUE, idx_hint, chunk, accessed_bytes) < 0)
		HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, "unable to unlock raw data chunk")
	} /* end if */
        relax = TRUE;

        /* Release selection iterators */
        if(file_iter_init) {
            if(H5S_SELECT_ITER_RELEASE(&file_iter) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_CANTFREE, FAIL, "Can't release selection iterator")
            file_iter_init=0;
        } /* end if */
        if(mem_iter_init) {
            if(H5S_SELECT_ITER_RELEASE(&mem_iter) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_CANTFREE, FAIL, "Can't release selection iterator")
            mem_iter_init=0;
        } /* end if */
        if(bkg_iter_init) {
            if(H5S_SELECT_ITER_RELEASE(&bkg_iter) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_CANTFREE, FAIL, "Can't release selection iterator")
            bkg_iter_init=0;
        } /* end if */

        /* Get the next chunk node in the skip list */
        chunk_node = H5D_CHUNK_GET_NEXT_NODE(fm, chunk_node);
    } /* end while */

done:
    /* Release selection iterators, if necessary */
    if(file_iter_init) {
        if(H5S_SELECT_ITER_RELEASE(&file_iter) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CANTFREE, FAIL, "Can't release selection iterator")
    } /* end if */
    if(mem_iter_init) {
        if(H5S_SELECT_ITER_RELEASE(&mem_iter) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CANTFREE, FAIL, "Can't release selection iterator")
    } /* end if */
    if(bkg_iter_init) {
        if(H5S_SELECT_ITER_RELEASE(&bkg_iter) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CANTFREE, FAIL, "Can't release selection iterator")
    } /* end if */

    if (tconv_buf && NULL==dxpl_cache->tconv_buf)
        (void)H5FL_BLK_FREE(type_conv,tconv_buf);
    if (bkg_buf && NULL==dxpl_cache->bkgr_buf)
        (void)H5FL_BLK_FREE(type_conv,bkg_buf);

    /* Release chunk mapping information */
    if(H5D_destroy_chunk_map(&fm) < 0)
        HDONE_ERROR(H5E_DATASET, H5E_CANTRELEASE, FAIL, "can't release chunk mapping")

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5D_chunk_write() */


/*-------------------------------------------------------------------------
 * Function:	H5D_compound_opt_read
 *
 * Purpose:	A shortcut optimization for the Chicago company for 
 *              a special optimization case when the source and 
 *              destination members are a subset of each other, and 
 *              the order is the same, and no conversion is needed.  
 *              For example:
 *                  struct source {            struct destination {
 *                      TYPE1 A;      -->          TYPE1 A;
 *                      TYPE2 B;      -->          TYPE2 B;
 *                      TYPE3 C;      -->          TYPE3 C;
 *                  };                             TYPE4 D;
 *                                                 TYPE5 E;
 *                                             };
 *              or
 *                  struct destination {       struct source {
 *                      TYPE1 A;      <--          TYPE1 A;
 *                      TYPE2 B;      <--          TYPE2 B;
 *                      TYPE3 C;      <--          TYPE3 C;
 *                  };                             TYPE4 D;
 *                                                 TYPE5 E;
 *                                             };
 *              The optimization is simply moving data to the appropriate 
 *              places in the buffer.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *		11 June 2007
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_compound_opt_read(size_t nelmts, const H5S_t *space,
    H5S_sel_iter_t *iter, const H5D_dxpl_cache_t *dxpl_cache,
    hid_t src_id, hid_t dst_id, H5T_subset_t subset, 
    void *data_buf, void *user_buf/*out*/)
{
    uint8_t    *dbuf = (uint8_t *)data_buf;    /*cast for pointer arithmetic	*/
    uint8_t    *ubuf = (uint8_t *)user_buf;    /*cast for pointer arithmetic	*/
    uint8_t    *xdbuf;
    uint8_t    *xubuf;

    hsize_t    _off[H5D_IO_VECTOR_SIZE];       /* Array to store sequence offsets */
    hsize_t    *off=NULL;                      /* Pointer to sequence offsets */
    size_t     _len[H5D_IO_VECTOR_SIZE];       /* Array to store sequence lengths */
    size_t     *len=NULL;                      /* Pointer to sequence lengths */
    size_t     nseq;                           /* Number of sequences generated */
    size_t     curr_off;                       /* offset of bytes left to process in sequence */
    size_t     curr_seq;                       /* Current sequence being processed */
    size_t     curr_len;                       /* Length of bytes left to process in sequence */
    size_t     curr_nelmts;		       /* number of elements to process in sequence   */
    size_t     i;

    H5T_t      *src, *dst;
    size_t     src_stride, dst_stride, type_size = 0;
    size_t     elmtno;			/*element counter		*/

    herr_t     ret_value = SUCCEED;	       /*return value		*/

    FUNC_ENTER_NOAPI_NOINIT(H5D_compound_opt_read)

    /* Check args */
    assert (data_buf);
    assert (user_buf);
    assert (space);
    assert (iter);
    assert (nelmts>0);

    /* Allocate the vector I/O arrays */
    if(dxpl_cache->vec_size != H5D_IO_VECTOR_SIZE) {
        if((len = H5FL_SEQ_MALLOC(size_t,dxpl_cache->vec_size))==NULL)
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate I/O length vector array");
        if((off = H5FL_SEQ_MALLOC(hsize_t,dxpl_cache->vec_size))==NULL)
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate I/O offset vector array");
    } /* end if */
    else {
        len=_len;
        off=_off;
    } /* end else */

    if (NULL == (src = (H5T_t *)H5I_object(src_id)) || NULL == (dst = (H5T_t *)H5I_object(dst_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type");

    src_stride = H5T_get_size(src);
    dst_stride = H5T_get_size(dst);

    if(H5T_SUBSET_SRC == subset)
        type_size = src_stride;
    else if(H5T_SUBSET_DST == subset)
        type_size = dst_stride;

    xdbuf = dbuf;

    /* Loop until all elements are written */
    while(nelmts>0) {
        /* Get list of sequences for selection to write */
        if(H5S_SELECT_GET_SEQ_LIST(space,0,iter,dxpl_cache->vec_size,nelmts,&nseq,&elmtno,off,len)<0)
            HGOTO_ERROR (H5E_INTERNAL, H5E_UNSUPPORTED, 0, "sequence length generation failed");

        /* Loop, while sequences left to process */
        for(curr_seq=0; curr_seq<nseq; curr_seq++) {
            /* Get the number of bytes and offset in sequence */
            curr_len=len[curr_seq];
            curr_off=off[curr_seq];

            /* Decide the number of elements and position in the buffer. */
            curr_nelmts = curr_len/dst_stride;
            xubuf = ubuf + curr_off;

            /* Copy the data into the right place. */
            for(i=0; i<curr_nelmts; i++) {
                HDmemmove(xubuf, xdbuf, type_size); 

                /* Update pointers */
                xdbuf += src_stride;
                xubuf += dst_stride;
            }

        } /* end for */

        /* Decrement number of elements left to process */
        nelmts -= elmtno;
    } /* end while */

done:
    if(dxpl_cache->vec_size != H5D_IO_VECTOR_SIZE) {
        if(len!=NULL)
            H5FL_SEQ_FREE(size_t,len);
        if(off!=NULL)
            H5FL_SEQ_FREE(hsize_t,off);
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D_compound_opt_read() */


/*-------------------------------------------------------------------------
 * Function:	H5D_compound_opt_write
 *
 * Purpose:	A shortcut optimization for the Chicago company for 
 *              a special optimization case when the source and 
 *              destination members are a subset of each other, and 
 *              the order is the same, and no conversion is needed.  
 *              For example:
 *                  struct source {            struct destination {
 *                      TYPE1 A;      -->          TYPE1 A;
 *                      TYPE2 B;      -->          TYPE2 B;
 *                      TYPE3 C;      -->          TYPE3 C;
 *                  };                             TYPE4 D;
 *                                                 TYPE5 E;
 *                                             };
 *              or
 *                  struct destination {       struct source {
 *                      TYPE1 A;      <--          TYPE1 A;
 *                      TYPE2 B;      <--          TYPE2 B;
 *                      TYPE3 C;      <--          TYPE3 C;
 *                  };                             TYPE4 D;
 *                                                 TYPE5 E;
 *                                             };
 *              The optimization is simply moving data to the appropriate 
 *              places in the buffer.
 *
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *		11 June 2007
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_compound_opt_write(size_t nelmts, hid_t src_id, hid_t dst_id, void *data_buf)
{
    uint8_t    *dbuf = (uint8_t *)data_buf;    /*cast for pointer arithmetic	*/
    uint8_t    *xsbuf, *xdbuf;
    size_t     i;
    H5T_t      *src, *dst;
    size_t     src_stride, dst_stride;
    herr_t     ret_value = SUCCEED;	       /*return value		*/

    FUNC_ENTER_NOAPI_NOINIT(H5D_compound_opt_write)

    /* Check args */
    assert (data_buf);
    assert (nelmts>0);

    if (NULL == (src = (H5T_t *)H5I_object(src_id)) || NULL == (dst = (H5T_t *)H5I_object(dst_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type");

    src_stride = H5T_get_size(src);
    dst_stride = H5T_get_size(dst);

    xsbuf = dbuf;
    xdbuf = dbuf;

    /* Loop until all elements are written */
    for(i=0; i<nelmts; i++) {
        HDmemmove(xdbuf, xsbuf, dst_stride); 

        /* Update pointers */
        xsbuf += src_stride;
        xdbuf += dst_stride;
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D_compound_opt_write() */


/*-------------------------------------------------------------------------
 * Function:	H5D_create_chunk_map
 *
 * Purpose:	Creates the mapping between elements selected in each chunk
 *              and the elements in the memory selection.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *		Thursday, April 10, 2003
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_create_chunk_map(H5D_chunk_map_t *fm, const H5D_io_info_t *io_info,
    hsize_t nelmts, const H5S_t *file_space, const H5S_t *mem_space,
    const H5T_t *mem_type)
{
    H5D_t *dataset=io_info->dset;       /* Local pointer to dataset info */
    H5S_t *tmp_mspace = NULL;     /* Temporary memory dataspace */
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

    FUNC_ENTER_NOAPI_NOINIT(H5D_create_chunk_map)

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
        if(NULL == (fm->select_chunk = (hbool_t *)H5MM_calloc((size_t)fm->total_chunks * sizeof(hbool_t))))
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
            curr_node=H5SL_first(fm->sel_chunks);
            while(curr_node) {
                H5D_chunk_info_t *chunk_info;   /* Pointer chunk information */

                /* Get pointer to chunk's information */
                chunk_info = (H5D_chunk_info_t *)H5SL_item(curr_node);
                assert(chunk_info);

                /* Clean hyperslab span's "scratch" information */
                if(H5S_hyper_reset_scratch(chunk_info->fspace) < 0)
                    HGOTO_ERROR(H5E_DATASET, H5E_CANTFREE, FAIL, "unable to reset span scratch info")

                /* Get the next chunk node in the skip list */
                curr_node=H5SL_next(curr_node);
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
            fm->last_index=(hsize_t)-1;
            fm->last_chunk_info=NULL;
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
            fm->mchunk_tmpl=tmp_mspace;

            /* Create temporary datatypes for selection iteration */
            if(f_tid<0) {
                if((f_tid = H5I_register(H5I_DATATYPE, H5T_copy(dataset->shared->type, H5T_COPY_ALL))) < 0)
                    HGOTO_ERROR(H5E_DATATYPE, H5E_CANTREGISTER, FAIL, "unable to register file datatype")
            } /* end if */

            /* Create selection iterator for memory selection */
            if((elmt_size=H5T_get_size(mem_type))==0)
                HGOTO_ERROR(H5E_DATATYPE, H5E_BADSIZE, FAIL, "datatype size invalid")
            if(H5S_select_iter_init(&(fm->mem_iter), mem_space, elmt_size) < 0)
                HGOTO_ERROR(H5E_DATASPACE, H5E_CANTINIT, FAIL, "unable to initialize selection iterator")
            iter_init = TRUE;	/* Selection iteration info has been initialized */

            /* Spaces aren't the same shape, iterate over the memory selection directly */
            if(H5S_select_iterate(&bogus, f_tid, file_space,  H5D_chunk_mem_cb, fm) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to create memory chunk selections")

            /* Clean up hyperslab stuff, if necessary */
            if(fm->msel_type != H5S_SEL_POINTS) {
                /* Clean memory chunks' hyperslab span "scratch" information */
                curr_node=H5SL_first(fm->sel_chunks);
                while(curr_node) {
                    H5D_chunk_info_t *chunk_info;   /* Pointer chunk information */

                    /* Get pointer to chunk's information */
                    chunk_info = (H5D_chunk_info_t *)H5SL_item(curr_node);
                    assert(chunk_info);

                    /* Clean hyperslab span's "scratch" information */
                    if(H5S_hyper_reset_scratch(chunk_info->mspace) < 0)
                        HGOTO_ERROR(H5E_DATASET, H5E_CANTFREE, FAIL, "unable to reset span scratch info")

                    /* Get the next chunk node in the skip list */
                    curr_node=H5SL_next(curr_node);
                } /* end while */
            } /* end if */
        } /* end else */
    } /* end else */

done:
    /* Release the [potentially partially built] chunk mapping information if an error occurs */
    if(ret_value<0) {
        if(tmp_mspace && !fm->mchunk_tmpl) {
            if(H5S_close(tmp_mspace) < 0)
                HDONE_ERROR(H5E_DATASPACE, H5E_CANTRELEASE, FAIL, "can't release memory chunk dataspace template")
        } /* end if */

        if (H5D_destroy_chunk_map(fm) < 0)
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
} /* end H5D_create_chunk_map() */


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

    FUNC_LEAVE_NOAPI(0);
}   /* H5D_free_chunk_info() */


/*-------------------------------------------------------------------------
 * Function:	H5D_destroy_chunk_map
 *
 * Purpose:	Destroy chunk mapping information.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Saturday, May 17, 2003
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_destroy_chunk_map(const H5D_chunk_map_t *fm)
{
    herr_t	ret_value = SUCCEED;	/*return value		*/

    FUNC_ENTER_NOAPI_NOINIT(H5D_destroy_chunk_map)

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
} /* end H5D_destroy_chunk_map() */


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
        fm->select_chunk[chunk_info->index] = TRUE;
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
                fm->select_chunk[chunk_index] = TRUE;
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
 * Function:	H5D_ioinfo_init
 *
 * Purpose:	Routine for determining correct I/O operations for
 *              each I/O action.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Thursday, September 30, 2004
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_ioinfo_init(H5D_t *dset, const H5D_dxpl_cache_t *dxpl_cache, hid_t dxpl_id,
    const H5S_t
#if !(defined H5_HAVE_PARALLEL || defined H5S_DEBUG)
    UNUSED
#endif /* H5_HAVE_PARALLEL */
    *mem_space, const H5S_t
#if !(defined H5_HAVE_PARALLEL || defined H5S_DEBUG)
    UNUSED
#endif /* H5_HAVE_PARALLEL */
    *file_space, H5T_path_t
#ifndef H5_HAVE_PARALLEL
    UNUSED
#endif /* H5_HAVE_PARALLEL */
    *tpath,
    H5D_io_info_t *io_info)
{
    herr_t	ret_value = SUCCEED;	/* Return value	*/

#if defined H5_HAVE_PARALLEL || defined H5S_DEBUG
    FUNC_ENTER_NOAPI_NOINIT(H5D_ioinfo_init)
#else /* defined H5_HAVE_PARALLEL || defined H5S_DEBUG */
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5D_ioinfo_init)
#endif /* defined H5_HAVE_PARALLEL || defined H5S_DEBUG */

    /* check args */
    HDassert(dset);
    HDassert(dset->oloc.file);
    HDassert(mem_space);
    HDassert(file_space);
    HDassert(tpath);
    HDassert(io_info);

    /* Set up "normal" I/O fields */
    io_info->dset=dset;
    io_info->dxpl_cache=dxpl_cache;
    io_info->dxpl_id=dxpl_id;
    io_info->store=NULL;        /* Set later in I/O routine? */

    /* Set I/O operations to initial values */
    io_info->ops=dset->shared->io_ops;

#ifdef H5_HAVE_PARALLEL
    /* Start in the "not modified" xfer_mode state */
    io_info->xfer_mode_changed = FALSE;
    io_info->using_mpi_vfd = IS_H5FD_MPI(dset->oloc.file);

    if(io_info->using_mpi_vfd) {
        htri_t opt;         /* Flag whether a selection is optimizable */

        /* Get MPI communicator */
        if((io_info->comm = H5F_mpi_get_comm(dset->oloc.file)) == MPI_COMM_NULL)
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTGET, FAIL, "can't retrieve MPI communicator")

        /*
         * Check if we can set direct MPI-IO read/write functions
         */
        opt=H5D_mpio_opt_possible(io_info, mem_space, file_space, tpath);
        if(opt==FAIL)
            HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "invalid check for direct IO dataspace ");

        /* Check if we can use the optimized parallel I/O routines */
        if(opt==TRUE) {
            /* Set the pointers to the MPI-specific routines */
            io_info->ops.read = H5D_mpio_select_read;
            io_info->ops.write = H5D_mpio_select_write;
        } /* end if */
        else {
            /* Set the pointers to the non-MPI-specific routines */
            io_info->ops.read = H5D_select_read;
            io_info->ops.write = H5D_select_write;

            /* If we won't be doing collective I/O, but the user asked for
             * collective I/O, change the request to use independent I/O, but
             * mark it so that we remember to revert the change.
             */
            if(io_info->dxpl_cache->xfer_mode==H5FD_MPIO_COLLECTIVE) {
                H5P_genplist_t *dx_plist;           /* Data transer property list */

                /* Get the dataset transfer property list */
                if (NULL == (dx_plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset creation property list")

                /* Change the xfer_mode to independent for handling the I/O */
                io_info->dxpl_cache->xfer_mode = H5FD_MPIO_INDEPENDENT;
                if(H5P_set (dx_plist, H5D_XFER_IO_XFER_MODE_NAME, &io_info->dxpl_cache->xfer_mode) < 0)
                    HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set transfer mode")

                /* Indicate that the transfer mode should be restored before returning
                 * to user.
                 */
                io_info->xfer_mode_changed = TRUE;
            } /* end if */
        } /* end else */
    } /* end if */
    else {
        /* Set the pointers to the non-MPI-specific routines */
        io_info->ops.read = H5D_select_read;
        io_info->ops.write = H5D_select_write;
    } /* end else */
#else /* H5_HAVE_PARALLEL */
    io_info->ops.read = H5D_select_read;
    io_info->ops.write = H5D_select_write;
#endif /* H5_HAVE_PARALLEL */

#ifdef H5S_DEBUG
    /* Get the information for the I/O statistics */
    if((io_info->stats=H5S_find(mem_space,file_space))==NULL)
        HGOTO_ERROR(H5E_DATASET, H5E_BADSELECT, FAIL, "can't set up selection statistics");
#endif /* H5S_DEBUG */

#if defined H5_HAVE_PARALLEL || defined H5S_DEBUG
done:
#endif /* H5_HAVE_PARALLEL || H5S_DEBUG */
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D_ioinfo_init() */

#ifdef H5_HAVE_PARALLEL

/*-------------------------------------------------------------------------
 * Function:	H5D_ioinfo_term
 *
 * Purpose:	Common logic for terminating an I/O info object
 *              (Only used for restoring MPI transfer mode currently)
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Friday, February  6, 2004
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_ioinfo_term(H5D_io_info_t *io_info)
{
    herr_t	ret_value = SUCCEED;	/*return value		*/

    FUNC_ENTER_NOAPI_NOINIT(H5D_ioinfo_term)

    /* Check if we need to revert the change to the xfer mode */
    if (io_info->xfer_mode_changed) {
        H5P_genplist_t *dx_plist;           /* Data transer property list */

        /* Get the dataset transfer property list */
        if (NULL == (dx_plist = (H5P_genplist_t *)H5I_object(io_info->dxpl_id)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset transfer property list")

        /* Restore the original parallel I/O mode */
        io_info->dxpl_cache->xfer_mode = H5FD_MPIO_COLLECTIVE;
        if(H5P_set (dx_plist, H5D_XFER_IO_XFER_MODE_NAME, &io_info->dxpl_cache->xfer_mode) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set transfer mode")
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D_ioinfo_term() */

#endif

