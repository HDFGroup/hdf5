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

#define H5D_PACKAGE		/*suppress error about including H5Dpkg	  */

/* Pablo information */
/* (Put before include files to avoid problems with inline functions) */
#define PABLO_MASK	H5Dio_mask

#include "H5private.h"		/* Generic Functions			*/
#include "H5Dpkg.h"		/* Dataset functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5FLprivate.h"	/* Free Lists                           */
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Sprivate.h"		/* Dataspace functions			*/
#include "H5TBprivate.h"	/* Threaded, balanced, binary trees (TBBTs) */
#include "H5Vprivate.h"		/* Vector and array functions		*/

/*#define H5D_DEBUG*/

#ifdef H5_HAVE_PARALLEL
/* Remove this if H5R_DATASET_REGION is no longer used in this file */
#   include "H5Rpublic.h"
#endif /*H5_HAVE_PARALLEL*/

/* Local typedefs */

/* Information for mapping between file space and memory space */

/* Structure holding information about a chunk's selection for mapping */
typedef struct H5D_chunk_info_t {
    hsize_t index;              /* "Index" of chunk in dataset (must be first for TBBT routines) */
    size_t chunk_points;        /* Number of elements selected in chunk */
    H5S_t *fspace;              /* Dataspace describing chunk & selection in it */
    hssize_t coords[H5O_LAYOUT_NDIMS];    /* Coordinates of chunk in file dataset's dataspace */
    H5S_t *mspace;              /* Dataspace describing selection in memory corresponding to this chunk */
    unsigned mspace_shared;     /* Indicate that the memory space for a chunk is shared and shouldn't be freed */
} H5D_chunk_info_t;

/* Main structure holding the mapping between file chunks and memory */
typedef struct fm_map {
    H5TB_TREE *fsel;            /* TBBT containing file dataspaces for all chunks */
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
} fm_map;

/* Interface initialization */
static int interface_initialize_g = 0;
#define INTERFACE_INIT NULL 

/* Local functions */
static herr_t H5D_read(H5D_t *dataset, const H5T_t *mem_type,
			const H5S_t *mem_space, const H5S_t *file_space,
			hid_t dset_xfer_plist, void *buf/*out*/);
static herr_t H5D_write(H5D_t *dataset, const H5T_t *mem_type,
			 const H5S_t *mem_space, const H5S_t *file_space,
			 hid_t dset_xfer_plist, const void *buf);
static herr_t 
H5D_contig_read(hsize_t nelmts, H5D_t *dataset, const H5T_t *mem_type, const H5S_t *mem_space,
            const H5S_t *file_space, H5T_path_t *tpath, H5S_conv_t *sconv,
            const H5D_dxpl_cache_t *dxpl_cache, hid_t dxpl_id,
            hid_t src_id, hid_t dst_id, void *buf/*out*/);
static herr_t
H5D_contig_write(hsize_t nelmts, H5D_t *dataset, const H5T_t *mem_type, const H5S_t *mem_space,
	    const H5S_t *file_space, H5T_path_t *tpath, H5S_conv_t *sconv,
            const H5D_dxpl_cache_t *dxpl_cache, hid_t dxpl_id,
            hid_t src_id, hid_t dst_id, const void *buf);
static herr_t
H5D_chunk_read(hsize_t nelmts, H5D_t *dataset, const H5T_t *mem_type, const H5S_t *mem_space,
            const H5S_t *file_space, H5T_path_t *tpath, H5S_conv_t *sconv,
            const H5D_dxpl_cache_t *dxpl_cache, hid_t dxpl_id,
            hid_t src_id, hid_t dst_id, void *buf/*out*/);
static herr_t
H5D_chunk_write(hsize_t nelmts, H5D_t *dataset, const H5T_t *mem_type, const H5S_t *mem_space,
	    const H5S_t *file_space, H5T_path_t *tpath, H5S_conv_t *sconv,
            const H5D_dxpl_cache_t *dxpl_cache, hid_t dxpl_id,
            hid_t src_id, hid_t dst_id, const void *buf);
#ifdef H5_HAVE_PARALLEL
static herr_t 
H5D_io_assist_mpio(hid_t dxpl_id, H5D_dxpl_cache_t *dxpl_cache, 
            hbool_t *xfer_mode_changed);
static herr_t 
H5D_io_restore_mpio(hid_t dxpl_id);
#endif /*H5_HAVE_PARALLEL*/
static herr_t H5D_create_chunk_map(H5D_t *dataset, const H5T_t *mem_type,
        const H5S_t *file_space, const H5S_t *mem_space, fm_map *fm);
static herr_t H5D_destroy_chunk_map(const fm_map *fm);
static void H5D_free_chunk_info(void *chunk_info);
static herr_t H5D_create_chunk_file_map_hyper(fm_map *fm);
static herr_t H5D_create_chunk_mem_map_hyper(const fm_map *fm);
static herr_t H5D_chunk_file_cb(void *elem, hid_t type_id, hsize_t ndims,
    hssize_t *coords, void *fm);
static herr_t H5D_chunk_mem_cb(void *elem, hid_t type_id, hsize_t ndims,
    hssize_t *coords, void *fm);
static herr_t H5D_fill(const void *fill, const H5T_t *fill_type, void *buf,
    const H5T_t *buf_type, const H5S_t *space, hid_t dxpl_id);


/* Declare a free list to manage blocks of single datatype element data */
H5FL_BLK_DEFINE(type_elem);

/* Declare a free list to manage blocks of type conversion data */
H5FL_BLK_DEFINE(type_conv);

/* Declare a free list to manage the H5D_chunk_info_t struct */
H5FL_DEFINE_STATIC(H5D_chunk_info_t);


/*--------------------------------------------------------------------------
 NAME
    H5Dfill
 PURPOSE
    Fill a selection in memory with a value
 USAGE
    herr_t H5Dfill(fill, fill_type, space, buf, buf_type)
        const void *fill;       IN: Pointer to fill value to use
        hid_t fill_type_id;     IN: Datatype of the fill value
        void *buf;              IN/OUT: Memory buffer to fill selection within
        hid_t buf_type_id;      IN: Datatype of the elements in buffer
        hid_t space_id;         IN: Dataspace describing memory buffer &
                                    containing selection to use.
 RETURNS
    Non-negative on success/Negative on failure.
 DESCRIPTION
    Use the selection in the dataspace to fill elements in a memory buffer.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
    If "fill" parameter is NULL, use all zeros as fill value
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t
H5Dfill(const void *fill, hid_t fill_type_id, void *buf, hid_t buf_type_id, hid_t space_id)
{
    H5S_t *space;               /* Dataspace */
    H5T_t *fill_type;           /* Fill-value datatype */
    H5T_t *buf_type;            /* Buffer datatype */
    herr_t ret_value=SUCCEED;   /* Return value */

    FUNC_ENTER_API(H5Dfill, FAIL)
    H5TRACE5("e","xixii",fill,fill_type_id,buf,buf_type_id,space_id);

    /* Check args */
    if (buf==NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid buffer")
    if (NULL == (space=H5I_object_verify(space_id, H5I_DATASPACE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, 0, "not a dataspace")
    if (NULL == (fill_type=H5I_object_verify(fill_type_id, H5I_DATATYPE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, 0, "not a datatype")
    if (NULL == (buf_type=H5I_object_verify(buf_type_id, H5I_DATATYPE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, 0, "not a datatype")

    /* Fill the selection in the memory buffer */
    if(H5D_fill(fill,fill_type,buf,buf_type,space, H5AC_dxpl_id)<0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTENCODE, FAIL, "filling selection failed")

done:
    FUNC_LEAVE_API(ret_value)
}   /* H5Dfill() */


/*--------------------------------------------------------------------------
 NAME
    H5D_fill
 PURPOSE
    Fill a selection in memory with a value (internal version)
 USAGE
    herr_t H5D_fill(fill, fill_type, buf, buf_type, space)
        const void *fill;       IN: Pointer to fill value to use
        H5T_t *fill_type;       IN: Datatype of the fill value
        void *buf;              IN/OUT: Memory buffer to fill selection within
        H5T_t *buf_type;        IN: Datatype of the elements in buffer
        H5S_t *space;           IN: Dataspace describing memory buffer &
                                    containing selection to use.
 RETURNS
    Non-negative on success/Negative on failure.
 DESCRIPTION
    Use the selection in the dataspace to fill elements in a memory buffer.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
    If "fill" parameter is NULL, use all zeros as fill value.  If "fill_type"
    parameter is NULL, use "buf_type" for the fill value datatype.
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static herr_t
H5D_fill(const void *fill, const H5T_t *fill_type, void *buf, const H5T_t *buf_type, const H5S_t *space, hid_t dxpl_id)
{
    H5T_path_t *tpath = NULL;   /* Conversion information*/
    uint8_t *tconv_buf = NULL;  /* Data type conv buffer */
    uint8_t *bkg_buf = NULL;    /* Temp conversion buffer */
    hid_t src_id = -1, dst_id = -1;     /* Temporary type IDs */
    size_t src_type_size;       /* Size of source type	*/
    size_t dst_type_size;       /* Size of destination type*/
    size_t buf_size;            /* Desired buffer size	*/
    herr_t ret_value=SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5D_fill)

    /* Check args */
    assert(fill_type);
    assert(buf);
    assert(buf_type);
    assert(space);

    /* Get the memory and file datatype sizes */
    src_type_size = H5T_get_size(fill_type);
    dst_type_size = H5T_get_size(buf_type);

    /* Get the maximum buffer size needed and allocate it */
    buf_size=MAX(src_type_size,dst_type_size);
    if (NULL==(tconv_buf = H5FL_BLK_MALLOC(type_elem,buf_size)) || NULL==(bkg_buf = H5FL_BLK_CALLOC(type_elem,buf_size)))
        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")

    /* Check for actual fill value to replicate */
    if(fill==NULL)
        /* If there's no fill value, just use zeros */
        HDmemset(tconv_buf,0,dst_type_size);
    else {
        /* Copy the user's data into the buffer for conversion */
        HDmemcpy(tconv_buf,fill,src_type_size);

        /* Set up type conversion function */
        if (NULL == (tpath = H5T_path_find(fill_type, buf_type, NULL, NULL, dxpl_id)))
            HGOTO_ERROR(H5E_DATASET, H5E_UNSUPPORTED, FAIL, "unable to convert between src and dest data types")

        /* Convert memory buffer into disk buffer */
        if (!H5T_path_noop(tpath)) {
            if ((src_id = H5I_register(H5I_DATATYPE, H5T_copy(fill_type, H5T_COPY_ALL)))<0 ||
                    (dst_id = H5I_register(H5I_DATATYPE, H5T_copy(buf_type, H5T_COPY_ALL)))<0)
                HGOTO_ERROR(H5E_DATASET, H5E_CANTREGISTER, FAIL, "unable to register types for conversion")

            /* Perform data type conversion */
            if (H5T_convert(tpath, src_id, dst_id, 1, 0, 0, tconv_buf, bkg_buf, dxpl_id)<0)
                HGOTO_ERROR(H5E_DATASET, H5E_CANTCONVERT, FAIL, "data type conversion failed")
        } /* end if */
    } /* end if */

    /* Fill the selection in the memory buffer */
    if(H5S_select_fill(tconv_buf, dst_type_size, space, buf)<0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTENCODE, FAIL, "filling selection failed")

done:
    if (tconv_buf)
        H5FL_BLK_FREE(type_elem,tconv_buf);
    if (bkg_buf)
        H5FL_BLK_FREE(type_elem,bkg_buf);
    FUNC_LEAVE_NOAPI(ret_value)
}   /* H5D_fill() */


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
    if (NULL == (dx_plist = H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset transfer property list")

    /* Get maximum temporary buffer size */
    if(H5P_get(dx_plist, H5D_XFER_MAX_TEMP_BUF_NAME, &cache->max_temp_buf)<0)
        HGOTO_ERROR (H5E_PLIST, H5E_CANTGET, FAIL, "Can't retrieve maximum temporary buffer size")

    /* Get temporary buffer pointer */
    if(H5P_get(dx_plist, H5D_XFER_TCONV_BUF_NAME, &cache->tconv_buf)<0)
        HGOTO_ERROR (H5E_PLIST, H5E_CANTGET, FAIL, "Can't retrieve temporary buffer pointer")

    /* Get background buffer pointer */
    if(H5P_get(dx_plist, H5D_XFER_BKGR_BUF_NAME, &cache->bkgr_buf)<0)
        HGOTO_ERROR (H5E_PLIST, H5E_CANTGET, FAIL, "Can't retrieve background buffer pointer")

    /* Get background buffer type */
    if(H5P_get(dx_plist, H5D_XFER_BKGR_BUF_TYPE_NAME, &cache->bkgr_buf_type)<0)
        HGOTO_ERROR (H5E_PLIST, H5E_CANTGET, FAIL, "Can't retrieve background buffer type")

    /* Get B-tree split ratios */
    if(H5P_get(dx_plist, H5D_XFER_BTREE_SPLIT_RATIO_NAME, &cache->btree_split_ratio)<0)
        HGOTO_ERROR (H5E_PLIST, H5E_CANTGET, FAIL, "Can't retrieve background buffer type")

    /* Get B-tree split ratios */
    if(H5P_get(dx_plist, H5D_XFER_BTREE_SPLIT_RATIO_NAME, &cache->btree_split_ratio)<0)
        HGOTO_ERROR (H5E_PLIST, H5E_CANTGET, FAIL, "Can't retrieve background buffer type")

    /* Get I/O vector size */
    if(H5P_get(dx_plist, H5D_XFER_HYPER_VECTOR_SIZE_NAME, &cache->vec_size)<0)
        HGOTO_ERROR (H5E_PLIST, H5E_CANTGET, FAIL, "Can't retrieve I/O vector size")

#ifdef H5_HAVE_PARALLEL
    /* Collect Parallel I/O information for possible later use */
    if(H5P_get(dx_plist, H5D_XFER_IO_XFER_MODE_NAME, &cache->xfer_mode)<0)
        HGOTO_ERROR (H5E_PLIST, H5E_CANTGET, FAIL, "Can't retrieve parallel transfer method")
#endif /*H5_HAVE_PARALLEL*/

    /* Get error detection properties */
    if(H5P_get(dx_plist, H5D_XFER_EDC_NAME, &cache->err_detect)<0)
        HGOTO_ERROR (H5E_PLIST, H5E_CANTGET, FAIL, "Can't retrieve error detection info")

    /* Get filter callback function */
    if(H5P_get(dx_plist, H5D_XFER_FILTER_CB_NAME, &cache->filter_cb)<0)
        HGOTO_ERROR (H5E_PLIST, H5E_CANTGET, FAIL, "Can't retrieve filter callback function")

    /* Get the data transform property */
    if(H5P_get(dx_plist, H5D_XFER_XFORM_NAME, &cache->data_xform_prop)<0)
        HGOTO_ERROR (H5E_PLIST, H5E_CANTGET, FAIL, "Can't retrieve data transform info")
            
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
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t
H5D_get_dxpl_cache(hid_t dxpl_id, H5D_dxpl_cache_t *cache)
{
    herr_t ret_value=SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(H5D_get_dxpl_cache,FAIL)

    /* Check args */
    assert(cache);

    /* Check for the default DXPL */
    if(dxpl_id==H5P_DATASET_XFER_DEFAULT)
        HDmemcpy(cache,&H5D_def_dxpl_cache,sizeof(H5D_dxpl_cache_t));
    else
        if(H5D_get_dxpl_cache_real(dxpl_id,cache)<0)
            HGOTO_ERROR (H5E_DATASET, H5E_CANTGET, FAIL, "Can't retrieve DXPL values")

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
 * Errors:
 *		ARGS	  BADTYPE	Not a data space. 
 *		ARGS	  BADTYPE	Not a data type. 
 *		ARGS	  BADTYPE	Not a dataset. 
 *		ARGS	  BADTYPE	Not xfer parms. 
 *		ARGS	  BADVALUE	No output buffer. 
 *		DATASET	  READERROR	Can't read data. 
 *
 * Programmer:	Robb Matzke
 *		Thursday, December  4, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Dread(hid_t dset_id, hid_t mem_type_id, hid_t mem_space_id,
	hid_t file_space_id, hid_t plist_id, void *buf/*out*/)
{
    H5D_t		   *dset = NULL;
    const H5T_t		   *mem_type = NULL;
    const H5S_t		   *mem_space = NULL;
    const H5S_t		   *file_space = NULL;
    herr_t                  ret_value=SUCCEED;  /* Return value */

    FUNC_ENTER_API(H5Dread, FAIL)
    H5TRACE6("e","iiiiix",dset_id,mem_type_id,mem_space_id,file_space_id,
             plist_id,buf);

    /* check arguments */
    if (NULL == (dset = H5I_object_verify(dset_id, H5I_DATASET)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset")
    if (NULL == dset->ent.file)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset")
    if (NULL == (mem_type = H5I_object_verify(mem_type_id, H5I_DATATYPE)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type")
    if (H5S_ALL != mem_space_id) {
	if (NULL == (mem_space = H5I_object_verify(mem_space_id, H5I_DATASPACE)))
	    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space")

	/* Check for valid selection */
	if(H5S_SELECT_VALID(mem_space)!=TRUE)
	    HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "selection+offset not within extent")
    }
    if (H5S_ALL != file_space_id) {
	if (NULL == (file_space = H5I_object_verify(file_space_id, H5I_DATASPACE)))
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
    if (!buf)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no output buffer")

    /* read raw data */
    if (H5D_read(dset, mem_type, mem_space, file_space, plist_id, buf/*out*/) < 0)
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
 *		file data type.	 Additional miscellaneous data transfer
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
 * Errors:
 *
 * Programmer:	Robb Matzke
 *		Thursday, December  4, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Dwrite(hid_t dset_id, hid_t mem_type_id, hid_t mem_space_id,
	 hid_t file_space_id, hid_t plist_id, const void *buf)
{
    H5D_t		   *dset = NULL;
    const H5T_t		   *mem_type = NULL;
    const H5S_t		   *mem_space = NULL;
    const H5S_t		   *file_space = NULL;
    herr_t                  ret_value=SUCCEED;  /* Return value */

    FUNC_ENTER_API(H5Dwrite, FAIL)
    H5TRACE6("e","iiiiix",dset_id,mem_type_id,mem_space_id,file_space_id,
             plist_id,buf);

    /* check arguments */
    if (NULL == (dset = H5I_object_verify(dset_id, H5I_DATASET)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset")
    if (NULL == dset->ent.file)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset")
    if (NULL == (mem_type = H5I_object_verify(mem_type_id, H5I_DATATYPE)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type")
    if (H5S_ALL != mem_space_id) {
	if (NULL == (mem_space = H5I_object_verify(mem_space_id, H5I_DATASPACE)))
	    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space")

	/* Check for valid selection */
	if (H5S_SELECT_VALID(mem_space)!=TRUE)
	    HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "selection+offset not within extent")
    }
    if (H5S_ALL != file_space_id) {
	if (NULL == (file_space = H5I_object_verify(file_space_id, H5I_DATASPACE)))
	    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space")

	/* Check for valid selection */
	if (H5S_SELECT_VALID(file_space)!=TRUE)
	    HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "selection+offset not within extent")
    }

    /* Get the default dataset transfer property list if the user didn't provide one */
    if (H5P_DEFAULT == plist_id)
        plist_id= H5P_DATASET_XFER_DEFAULT;
    else
        if (TRUE!=H5P_isa_class(plist_id,H5P_DATASET_XFER))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not xfer parms")
    if (!buf)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no output buffer")

    /* write raw data */
    if (H5D_write(dset, mem_type, mem_space, file_space, plist_id, buf) < 0)
	HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "can't write data")

done:
    FUNC_LEAVE_API(ret_value)
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
 * Modifications:
 *	Robb Matzke, 1998-06-09
 *	The data space is no longer cached in the dataset struct.
 *
 * 	Robb Matzke, 1998-08-11
 *	Added timing calls around all the data space I/O functions.
 *
 * 	rky, 1998-09-18
 *	Added must_convert to do non-optimized read when necessary.
 *
 *  	Quincey Koziol, 1999-07-02
 *	Changed xfer_parms parameter to xfer plist parameter, so it
 *	could be passed to H5T_convert.
 *
 *	Albert Cheng, 2000-11-21
 *	Added the code that when it detects it is not safe to process a
 *	COLLECTIVE read request without hanging, it changes it to
 *	INDEPENDENT calls.
 *
 *	Albert Cheng, 2000-11-27
 *	Changed to use the optimized MPIO transfer for Collective calls only.
 *
 *      Raymond Lu, 2001-10-2
 *      Changed the way to retrieve property for generic property list.
 *
 *	Raymond Lu, 2002-2-26
 *	For the new fill value design, data space can either be allocated 
 *	or not allocated at this stage.  Fill value or data from space is
 *	returned to outgoing buffer.
 *
 *      QAK - 2002/04/02
 *      Removed the must_convert parameter and move preconditions to
 *      H5S_<foo>_opt_possible() routine
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_read(H5D_t *dataset, const H5T_t *mem_type, const H5S_t *mem_space,
	 const H5S_t *file_space, hid_t dxpl_id, void *buf/*out*/)
{
    hssize_t	snelmts;                /*total number of elmts	(signed) */
    hsize_t	nelmts;                 /*total number of elmts	*/
    H5T_path_t	*tpath = NULL;		/*type conversion info	*/
    hid_t	src_id = -1, dst_id = -1;/*temporary type atoms */
    H5S_conv_t	*sconv=NULL;	        /*space conversion funcs*/
    hbool_t     use_par_opt_io=FALSE;   /* Whether the 'optimized' I/O routines with be parallel */
#ifdef H5_HAVE_PARALLEL
    hbool_t     xfer_mode_changed=FALSE;    /* Whether the transfer mode was changed */
#endif /*H5_HAVE_PARALLEL*/
    H5D_dxpl_cache_t dxpl_cache;        /* Data transfer property cache */
    unsigned	sconv_flags=0;	        /* Flags for the space conversion */
    herr_t	ret_value = SUCCEED;	/* Return value	*/

    FUNC_ENTER_NOAPI_NOINIT(H5D_read)

    /* check args */
    assert(dataset && dataset->ent.file);
    assert(mem_type);
    assert(buf);

    if (!file_space)
        file_space = dataset->space;
    if (!mem_space)
        mem_space = file_space;
    if((snelmts = H5S_GET_SELECT_NPOINTS(mem_space))<0)
	HGOTO_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL, "src dataspace has invalid selection")
    H5_ASSIGN_OVERFLOW(nelmts,snelmts,hssize_t,hsize_t);

    /* Fill the DXPL cache values for later use */
    if (H5D_get_dxpl_cache(dxpl_id,&dxpl_cache)<0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't fill dxpl cache")

#ifdef H5_HAVE_PARALLEL
    /* Collective access is not permissible without a MPI based VFD */
    if (dxpl_cache.xfer_mode==H5FD_MPIO_COLLECTIVE && !IS_H5FD_MPI(dataset->ent.file))
        HGOTO_ERROR (H5E_DATASET, H5E_UNSUPPORTED, FAIL, "collective access for MPI-based drivers only")

    /* Set the "parallel I/O possible" flag, for H5S_find(), if we are doing collective I/O */
    /* (Don't set the parallel I/O possible flag for the MPI-posix driver, since it doesn't do real collective I/O) */
    if (H5S_mpi_opt_types_g && dxpl_cache.xfer_mode==H5FD_MPIO_COLLECTIVE && !IS_H5FD_MPIPOSIX(dataset->ent.file))
        sconv_flags |= H5S_CONV_PAR_IO_POSSIBLE;
#endif /*H5_HAVE_PARALLEL*/

    /* Make certain that the number of elements in each selection is the same */
    if (nelmts!=(hsize_t)H5S_GET_SELECT_NPOINTS(file_space))
        HGOTO_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL, "src and dest data spaces have different sizes")

    /* Retrieve dataset properties */
    /* <none needed in the general case> */

    /* If space hasn't been allocated and not using external storage, 
     * return fill value to buffer if fill time is upon allocation, or
     * do nothing if fill time is never.  If the dataset is compact and 
     * fill time is NEVER, there is no way to tell whether part of data
     * has been overwritten.  So just proceed in reading.     
     */ 
    if(nelmts > 0 && dataset->efl.nused==0 &&
            ((dataset->layout.type==H5D_CONTIGUOUS && !H5F_addr_defined(dataset->layout.u.contig.addr))
                || (dataset->layout.type==H5D_CHUNKED && !H5F_addr_defined(dataset->layout.u.chunk.addr)))) {
        H5D_fill_value_t fill_status;   /* Whether/How the fill value is defined */

        /* Retrieve dataset's fill-value properties */
        if(H5P_is_fill_value_defined(&dataset->dcpl_cache.fill, &fill_status)<0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't tell if fill value defined")

        /* Should be impossible, but check anyway... */
        if(fill_status == H5D_FILL_VALUE_UNDEFINED &&
                (dataset->dcpl_cache.fill_time == H5D_FILL_TIME_ALLOC || dataset->dcpl_cache.fill_time == H5D_FILL_TIME_IFSET))
            HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "read failed: dataset doesn't exist, no data can be read")

        /* If we're never going to fill this dataset, just leave the junk in the user's buffer */
        if(dataset->dcpl_cache.fill_time == H5D_FILL_TIME_NEVER)
            HGOTO_DONE(SUCCEED)

        /* Go fill the user's selection with the dataset's fill value */
        if(H5D_fill(dataset->dcpl_cache.fill.buf,dataset->type,buf,mem_type,mem_space,dxpl_id)<0)
            HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "filling buf failed")
        else
            HGOTO_DONE(SUCCEED)
    } /* end if */

    /*
     * Locate the type conversion function and data space conversion
     * functions, and set up the element numbering information. If a data
     * type conversion is necessary then register data type atoms. Data type
     * conversion is necessary if the user has set the `need_bkg' to a high
     * enough value in xfer_parms since turning off data type conversion also
     * turns off background preservation.
     */
    if (NULL==(tpath=H5T_path_find(dataset->type, mem_type, NULL, NULL, dxpl_id))) {
        HGOTO_ERROR(H5E_DATASET, H5E_UNSUPPORTED, FAIL, "unable to convert between src and dest data types")
    } else if (!H5T_path_noop(tpath)) {
        if ((src_id=H5I_register(H5I_DATATYPE, H5T_copy(dataset->type, H5T_COPY_ALL)))<0 ||
                (dst_id=H5I_register(H5I_DATATYPE, H5T_copy(mem_type, H5T_COPY_ALL)))<0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTREGISTER, FAIL, "unable to register types for conversion")
    } /* end if */

    /* Set the storage flags for the space conversion check */
    switch(dataset->layout.type) {
        case H5D_COMPACT:
            sconv_flags |= H5S_CONV_STORAGE_COMPACT;
            break;

        case H5D_CONTIGUOUS:
            sconv_flags |= H5S_CONV_STORAGE_CONTIGUOUS;
            break;

        case H5D_CHUNKED:
            sconv_flags |= H5S_CONV_STORAGE_CHUNKED;
            break;

        default:
            assert(0 && "Unhandled layout type!");
    } /* end switch */

    /* Get dataspace functions */
    if (NULL==(sconv=H5S_find(mem_space, file_space, sconv_flags, &use_par_opt_io)))
        HGOTO_ERROR (H5E_DATASET, H5E_UNSUPPORTED, FAIL, "unable to convert from file to memory data space")

#ifdef H5_HAVE_PARALLEL
    /* Don't reset the transfer mode if we can't or won't use it */
    if(!use_par_opt_io || !H5T_path_noop(tpath))
        H5D_io_assist_mpio(dxpl_id, &dxpl_cache, &xfer_mode_changed);
#endif /*H5_HAVE_PARALLEL*/

    /* Determine correct I/O routine to invoke */
    if(dataset->layout.type!=H5D_CHUNKED) {
        if(H5D_contig_read(nelmts, dataset, mem_type, mem_space, file_space, tpath, sconv,
                &dxpl_cache, dxpl_id, src_id, dst_id, buf)<0)
            HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "can't read data")
    } /* end if */
    else {
        if(H5D_chunk_read(nelmts, dataset, mem_type, mem_space, file_space, tpath, sconv,
                &dxpl_cache, dxpl_id, src_id, dst_id, buf)<0)
            HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "can't read data")
    } /* end else */

done:
#ifdef H5_HAVE_PARALLEL
    /* Restore xfer_mode due to the kludge */
    if (xfer_mode_changed)
        H5D_io_restore_mpio(dxpl_id);
#endif /*H5_HAVE_PARALLEL*/
    if (src_id >= 0) {
        if(H5I_dec_ref(src_id)<0)
            HDONE_ERROR (H5E_DATASET, H5E_CANTFREE, FAIL, "Can't decrement temporary datatype ID")
    } /* end if */
    if (dst_id >= 0) {
        if(H5I_dec_ref(dst_id)<0)
            HDONE_ERROR (H5E_DATASET, H5E_CANTFREE, FAIL, "Can't decrement temporary datatype ID")
    } /* end if */

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
 * Modifications:
 * 	Robb Matzke, 9 Jun 1998
 *	The data space is no longer cached in the dataset struct.
 *
 * 	rky 980918
 *	Added must_convert to do non-optimized read when necessary.
 *
 *      Quincey Koziol, 2 July 1999
 *      Changed xfer_parms parameter to xfer plist parameter, so it could
 *      be passed to H5T_convert
 *
 *	Albert Cheng, 2000-11-21
 *	Added the code that when it detects it is not safe to process a
 *	COLLECTIVE write request without hanging, it changes it to
 *	INDEPENDENT calls.
 *      
 *	Albert Cheng, 2000-11-27
 *	Changed to use the optimized MPIO transfer for Collective calls only.
 *
 *      Raymond Lu, 2001-10-2
 *      Changed the way to retrieve property for generic property list.
 *
 *	Raymond Lu, 2002-2-26
 *	For the new fill value design, space may not be allocated until 
 *	this function is called.  Allocate and initialize space if it 
 *	hasn't been.
 *
 *      QAK - 2002/04/02
 *      Removed the must_convert parameter and move preconditions to
 *      H5S_<foo>_opt_possible() routine
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_write(H5D_t *dataset, const H5T_t *mem_type, const H5S_t *mem_space,
	  const H5S_t *file_space, hid_t dxpl_id, const void *buf)
{
    hssize_t	snelmts;                /*total number of elmts	(signed) */
    hsize_t	nelmts;                 /*total number of elmts	*/
    H5T_path_t	*tpath = NULL;		/*type conversion info	*/
    hid_t	src_id = -1, dst_id = -1;/*temporary type atoms */
    H5S_conv_t	*sconv=NULL;		/*space conversion funcs*/
    hbool_t     use_par_opt_io=FALSE;   /* Whether the 'optimized' I/O routines with be parallel */
#ifdef H5_HAVE_PARALLEL
    hbool_t     xfer_mode_changed=FALSE;    /* Whether the transfer mode was changed */
#endif /*H5_HAVE_PARALLEL*/
    H5D_dxpl_cache_t dxpl_cache;        /* Data transfer property cache */
    unsigned	sconv_flags=0;	        /* Flags for the space conversion */
    herr_t	ret_value = SUCCEED;	/* Return value	*/

    FUNC_ENTER_NOAPI_NOINIT(H5D_write)

    /* check args */
    assert(dataset && dataset->ent.file);
    assert(mem_type);
    assert(buf);

    /* If MPI based VFD is used, no VL datatype support yet. */
    /* This is because they use the global heap in the file and we don't */
    /* support parallel access of that yet */
    if (IS_H5FD_MPI(dataset->ent.file) && H5T_detect_class(mem_type, H5T_VLEN)>0)
        HGOTO_ERROR (H5E_DATASET, H5E_UNSUPPORTED, FAIL, "Parallel IO does not support writing VL datatypes yet")
    /* If MPI based VFD is used, no VL datatype support yet. */
    /* This is because they use the global heap in the file and we don't */
    /* support parallel access of that yet */
    /* We should really use H5T_detect_class() here, but it will be difficult
     * to detect the type of the reference if it is nested... -QAK
     */
    if (IS_H5FD_MPI(dataset->ent.file) &&
            H5T_get_class(mem_type)==H5T_REFERENCE &&
            H5T_get_ref_type(mem_type)==H5R_DATASET_REGION)
        HGOTO_ERROR (H5E_DATASET, H5E_UNSUPPORTED, FAIL, "Parallel IO does not support writing region reference datatypes yet")

    /* Check if we are allowed to write to this file */
    if (0==(H5F_get_intent(dataset->ent.file) & H5F_ACC_RDWR))
	HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "no write intent on file")

    /* Fill the DXPL cache values for later use */
    if (H5D_get_dxpl_cache(dxpl_id,&dxpl_cache)<0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't fill dxpl cache")

    if (!file_space)
        file_space = dataset->space;
    if (!mem_space)                                                                                                                      
        mem_space = file_space;                                                                                                         
    if((snelmts = H5S_GET_SELECT_NPOINTS(mem_space))<0)
	HGOTO_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL, "src dataspace has invalid selection")
    H5_ASSIGN_OVERFLOW(nelmts,snelmts,hssize_t,hsize_t);

#ifdef H5_HAVE_PARALLEL
    /* Collective access is not permissible without a MPI based VFD */
    if (dxpl_cache.xfer_mode==H5FD_MPIO_COLLECTIVE && !IS_H5FD_MPI(dataset->ent.file))
        HGOTO_ERROR (H5E_DATASET, H5E_UNSUPPORTED, FAIL, "collective access for MPI-based driver only")

    /* Set the "parallel I/O possible" flag, for H5S_find(), if we are doing collective I/O */
    /* (Don't set the parallel I/O possible flag for the MPI-posix driver, since it doesn't do real collective I/O) */
    if (H5S_mpi_opt_types_g && dxpl_cache.xfer_mode==H5FD_MPIO_COLLECTIVE && !IS_H5FD_MPIPOSIX(dataset->ent.file))
        sconv_flags |= H5S_CONV_PAR_IO_POSSIBLE;
#endif /*H5_HAVE_PARALLEL*/

    /* Make certain that the number of elements in each selection is the same */
    if (nelmts!=(hsize_t)H5S_GET_SELECT_NPOINTS(file_space))
	HGOTO_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL, "src and dest data spaces have different sizes")

    /* Retrieve dataset properties */
    /* <none needed currently> */

    /* Allocate data space and initialize it if it hasn't been. */
    if(nelmts > 0 && dataset->efl.nused==0 && 
            ((dataset->layout.type==H5D_CONTIGUOUS && !H5F_addr_defined(dataset->layout.u.contig.addr))
                || (dataset->layout.type==H5D_CHUNKED && !H5F_addr_defined(dataset->layout.u.chunk.addr)))) {
        hssize_t file_nelmts;   /* Number of elements in file dataset's dataspace */
        hbool_t full_overwrite; /* Whether we are over-writing all the elements */

        /* Get the number of elements in file dataset's dataspace */
        if((file_nelmts=H5S_GET_EXTENT_NPOINTS(file_space))<0)
            HGOTO_ERROR (H5E_DATASET, H5E_BADVALUE, FAIL, "can't retrieve number of elements in file dataset")

        /* Always allow fill values to be written if the dataset has a VL datatype */
        if(H5T_detect_class(dataset->type, H5T_VLEN))
            full_overwrite=FALSE;
        else
            full_overwrite=(hsize_t)file_nelmts==nelmts ? TRUE : FALSE;

 	/* Allocate storage */
        if(H5D_alloc_storage(dataset->ent.file,dxpl_id,dataset,H5D_ALLOC_WRITE, TRUE, full_overwrite)<0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to initialize storage")
    } /* end if */

    /*
     * Locate the type conversion function and data space conversion
     * functions, and set up the element numbering information. If a data
     * type conversion is necessary then register data type atoms. Data type
     * conversion is necessary if the user has set the `need_bkg' to a high
     * enough value in xfer_parms since turning off data type conversion also
     * turns off background preservation.
     */
    if (NULL==(tpath=H5T_path_find(mem_type, dataset->type, NULL, NULL, dxpl_id))) {
	HGOTO_ERROR(H5E_DATASET, H5E_UNSUPPORTED, FAIL, "unable to convert between src and dest data types")
    } else if (!H5T_path_noop(tpath)) {
	if ((src_id = H5I_register(H5I_DATATYPE, H5T_copy(mem_type, H5T_COPY_ALL)))<0 ||
                (dst_id = H5I_register(H5I_DATATYPE, H5T_copy(dataset->type, H5T_COPY_ALL)))<0)
	    HGOTO_ERROR(H5E_DATASET, H5E_CANTREGISTER, FAIL, "unable to register types for conversion")
    } /* end if */

    /* Set the storage flags for the space conversion check */
    switch(dataset->layout.type) {
        case H5D_COMPACT:
            sconv_flags |= H5S_CONV_STORAGE_COMPACT;
            break;

        case H5D_CONTIGUOUS:
            sconv_flags |= H5S_CONV_STORAGE_CONTIGUOUS;
            break;

        case H5D_CHUNKED:
            sconv_flags |= H5S_CONV_STORAGE_CHUNKED;
            break;

        default:
            assert(0 && "Unhandled layout type!");
    } /* end switch */

    /* Get dataspace functions */
    if (NULL==(sconv=H5S_find(mem_space, file_space, sconv_flags, &use_par_opt_io)))
	HGOTO_ERROR (H5E_DATASET, H5E_UNSUPPORTED, FAIL, "unable to convert from memory to file data space")
        
#ifdef H5_HAVE_PARALLEL
    /* Don't reset the transfer mode if we can't or won't use it */
    if(!use_par_opt_io || !H5T_path_noop(tpath))
        H5D_io_assist_mpio(dxpl_id, &dxpl_cache, &xfer_mode_changed);
#endif /*H5_HAVE_PARALLEL*/

    /* Determine correct I/O routine to invoke */
    if(dataset->layout.type!=H5D_CHUNKED) {
        if(H5D_contig_write(nelmts, dataset, mem_type, mem_space, file_space, tpath, sconv,
                &dxpl_cache, dxpl_id, src_id, dst_id, buf)<0)
            HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "can't write data")
    } /* end if */
    else {
        if(H5D_chunk_write(nelmts, dataset, mem_type, mem_space, file_space, tpath, sconv,
                &dxpl_cache, dxpl_id, src_id, dst_id, buf)<0)
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
    if (H5O_touch(&(dataset->ent), FALSE, dxpl_id)<0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to update modification time")
#endif /* OLD_WAY */

done:
#ifdef H5_HAVE_PARALLEL
    /* Restore xfer_mode due to the kludge */
    if (xfer_mode_changed)
        H5D_io_restore_mpio(dxpl_id);
#endif /*H5_HAVE_PARALLEL*/
    if (src_id >= 0) {
        if(H5I_dec_ref(src_id)<0)
            HDONE_ERROR (H5E_DATASET, H5E_CANTFREE, FAIL, "Can't decrement temporary datatype ID")
    } /* end if */
    if (dst_id >= 0) {
        if(H5I_dec_ref(dst_id)<0)
            HDONE_ERROR (H5E_DATASET, H5E_CANTFREE, FAIL, "Can't decrement temporary datatype ID")
    } /* end if */

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
 * Modifications:
 *      QAK - 2003/04/17
 *      Hacked on it a lot. :-)
 *      Leon Arber: 4/20/04
 *      Added support for data transforms.
 *
 *-------------------------------------------------------------------------
 */
static herr_t 
H5D_contig_read(hsize_t nelmts, H5D_t *dataset,
    const H5T_t *mem_type, const H5S_t *mem_space,
    const H5S_t *file_space, H5T_path_t *tpath, H5S_conv_t *sconv,
    const H5D_dxpl_cache_t *dxpl_cache, hid_t dxpl_id,
    hid_t src_id, hid_t dst_id, void *buf/*out*/)
{
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
    uint8_t	*tconv_buf = NULL;	/*data type conv buffer	*/
    uint8_t	*bkg_buf = NULL;	/*background buffer	*/
    hsize_t	smine_start;		/*strip mine start loc	*/
    size_t	n, smine_nelmts;	/*elements per strip	*/
    herr_t	ret_value = SUCCEED;	/*return value		*/

    FUNC_ENTER_NOAPI_NOINIT(H5D_contig_read)

    /*
     * If there is no type conversion then read directly into the
     * application's buffer.  This saves at least one mem-to-mem copy.
     */
    if ( H5Z_xform_noop(dxpl_cache->data_xform_prop) && H5T_path_noop(tpath)) {
#ifdef H5S_DEBUG
	H5_timer_begin(&timer);
#endif
  	/* Sanity check dataset, then read it */
        assert(((dataset->layout.type==H5D_CONTIGUOUS && H5F_addr_defined(dataset->layout.u.contig.addr))
                || (dataset->layout.type==H5D_CHUNKED && H5F_addr_defined(dataset->layout.u.chunk.addr)))
            || dataset->efl.nused>0 || 
            H5S_NULL == H5S_GET_EXTENT_TYPE(file_space) ||
             dataset->layout.type==H5D_COMPACT);
        H5_CHECK_OVERFLOW(nelmts,hsize_t,size_t);
        status = (sconv->read)(dataset->ent.file, dxpl_cache, dxpl_id,
            dataset, (H5D_storage_t *)&(dataset->efl),
            (size_t)nelmts, H5T_get_size(dataset->type), 
            file_space, mem_space,
            buf/*out*/);
#ifdef H5S_DEBUG
        H5_timer_end(&(sconv->stats[1].read_timer), &timer);
        sconv->stats[1].read_nbytes += nelmts * H5T_get_size(dataset->type);
        sconv->stats[1].read_ncalls++;
#endif

        /* Check return value from optimized read */
        if (status<0) {
            HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "optimized read failed")
        } else
            /* direct xfer accomplished successfully */
            HGOTO_DONE(SUCCEED)
    } /* end if */

    /*
     * This is the general case (type conversion, usually).
     */
    if(nelmts==0)
        HGOTO_DONE(SUCCEED)

    /* Compute element sizes and other parameters */
    src_type_size = H5T_get_size(dataset->type);
    dst_type_size = H5T_get_size(mem_type);
    max_type_size = MAX(src_type_size, dst_type_size);
    target_size = dxpl_cache->max_temp_buf;
    /* XXX: This could cause a problem if the user sets their buffer size
     * to the same size as the default, and then the dataset elements are
     * too large for the buffer... - QAK
     */
    if(target_size==H5D_XFER_MAX_TEMP_BUF_DEF) {
        /* If the buffer is too small to hold even one element, make it bigger */
        if(target_size<max_type_size)
            target_size = max_type_size;
        /* If the buffer is too large to hold all the elements, make it smaller */
        else if(target_size>(nelmts*max_type_size))
            target_size=(nelmts*max_type_size);
    } /* end if */
    request_nelmts = target_size / max_type_size;
    
    /* Sanity check elements in temporary buffer */
    if (request_nelmts==0)
        HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL, "temporary buffer max size is too small")

    /* Figure out the strip mine size. */
    if (H5S_select_iter_init(&file_iter, file_space, src_type_size)<0)
        HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL, "unable to initialize file selection information")
    file_iter_init=1;	/*file selection iteration info has been initialized */
    if (H5S_select_iter_init(&mem_iter, mem_space, dst_type_size)<0)
        HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL, "unable to initialize memory selection information")
    mem_iter_init=1;	/*file selection iteration info has been initialized */
    if (H5S_select_iter_init(&bkg_iter, mem_space, dst_type_size)<0)
        HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL, "unable to initialize background selection information")
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
    if (NULL==(tconv_buf=dxpl_cache->tconv_buf)) {
        /* Allocate temporary buffer */
        if((tconv_buf=H5FL_BLK_MALLOC(type_conv,target_size))==NULL)
            HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for type conversion")
    } /* end if */
    if (need_bkg && NULL==(bkg_buf=dxpl_cache->bkgr_buf)) {
        /* Allocate background buffer */
        if((bkg_buf=H5FL_BLK_CALLOC(type_conv,(request_nelmts*dst_type_size)))==NULL)
            HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for background conversion")
    } /* end if */

    /* Start strip mining... */
    for (smine_start=0; smine_start<nelmts; smine_start+=smine_nelmts) {
        /* Go figure out how many elements to read from the file */
        assert(H5S_SELECT_ITER_NELMTS(&file_iter)==(nelmts-smine_start));
        smine_nelmts = MIN(request_nelmts, (nelmts-smine_start));
	
        /*
         * Gather the data from disk into the data type conversion
         * buffer. Also gather data from application to background buffer
         * if necessary.
         */
#ifdef H5S_DEBUG
	H5_timer_begin(&timer);
#endif
	/* Sanity check that space is allocated, then read data from it */ 
        assert(((dataset->layout.type==H5D_CONTIGUOUS && H5F_addr_defined(dataset->layout.u.contig.addr))
                || (dataset->layout.type==H5D_CHUNKED && H5F_addr_defined(dataset->layout.u.chunk.addr)))
            || dataset->efl.nused>0 || 
             dataset->layout.type==H5D_COMPACT);
        n = H5S_select_fgath(dataset->ent.file, dxpl_cache, dxpl_id,
            dataset, (H5D_storage_t *)&(dataset->efl),
            file_space, &file_iter, smine_nelmts,
            tconv_buf/*out*/);

#ifdef H5S_DEBUG
	H5_timer_end(&(sconv->stats[1].gath_timer), &timer);
	sconv->stats[1].gath_nbytes += n * src_type_size;
	sconv->stats[1].gath_ncalls++;
#endif
	if (n!=smine_nelmts)
            HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, "file gather failed")
	
        if (H5T_BKG_YES==need_bkg) {
#ifdef H5S_DEBUG
            H5_timer_begin(&timer);
#endif
            n = H5S_select_mgath(buf, mem_space, &bkg_iter,
				 smine_nelmts, dxpl_cache, bkg_buf/*out*/);
#ifdef H5S_DEBUG
            H5_timer_end(&(sconv->stats[1].bkg_timer), &timer);
            sconv->stats[1].bkg_nbytes += n * dst_type_size;
            sconv->stats[1].bkg_ncalls++;
#endif
            if (n!=smine_nelmts)
                HGOTO_ERROR (H5E_IO, H5E_READERROR, FAIL, "mem gather failed")
        } /* end if */
	
	/*
         * Perform data type conversion.
         */
        if (H5T_convert(tpath, src_id, dst_id, smine_nelmts, 0, 0, tconv_buf, bkg_buf, dxpl_id)<0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "data type conversion failed")

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
        status = H5S_select_mscat(tconv_buf, mem_space,
                          &mem_iter, smine_nelmts, dxpl_cache, buf/*out*/);
#ifdef H5S_DEBUG
	H5_timer_end(&(sconv->stats[1].scat_timer), &timer);
	sconv->stats[1].scat_nbytes += smine_nelmts * dst_type_size;
	sconv->stats[1].scat_ncalls++;
#endif
	if (status<0)
            HGOTO_ERROR (H5E_DATASET, H5E_READERROR, FAIL, "scatter failed")
	
    } /* end for */

done:
    /* Release selection iterators */
    if(file_iter_init) {
        if(H5S_SELECT_ITER_RELEASE(&file_iter)<0)
            HDONE_ERROR (H5E_DATASET, H5E_CANTFREE, FAIL, "Can't release selection iterator")
    } /* end if */
    if(mem_iter_init) {
        if(H5S_SELECT_ITER_RELEASE(&mem_iter)<0)
            HDONE_ERROR (H5E_DATASET, H5E_CANTFREE, FAIL, "Can't release selection iterator")
    } /* end if */
    if(bkg_iter_init) {
        if(H5S_SELECT_ITER_RELEASE(&bkg_iter)<0)
            HDONE_ERROR (H5E_DATASET, H5E_CANTFREE, FAIL, "Can't release selection iterator")
    } /* end if */

    if (tconv_buf && NULL==dxpl_cache->tconv_buf)
        H5FL_BLK_FREE(type_conv,tconv_buf);
    if (bkg_buf && NULL==dxpl_cache->bkgr_buf)
        H5FL_BLK_FREE(type_conv,bkg_buf);
        
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
 * Modifications:
 *      QAK - 2003/04/17
 *      Hacked on it a lot. :-)
 *      Leon Arber: 4/20/04
 *      Added support for data transforms.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_contig_write(hsize_t nelmts, H5D_t *dataset,
    const H5T_t *mem_type, const H5S_t *mem_space,
    const H5S_t *file_space, H5T_path_t *tpath, H5S_conv_t *sconv,
    const H5D_dxpl_cache_t *dxpl_cache, hid_t dxpl_id,
    hid_t src_id, hid_t dst_id, const void *buf)
{
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
    uint8_t	*tconv_buf = NULL;	/*data type conv buffer	*/
    uint8_t	*bkg_buf = NULL;	/*background buffer	*/
    hsize_t	smine_start;		/*strip mine start loc	*/
    size_t	n, smine_nelmts;	/*elements per strip	*/
    herr_t	ret_value = SUCCEED;	/*return value		*/

    FUNC_ENTER_NOAPI_NOINIT(H5D_contig_write)

    /*
     * If there is no type conversion then write directly from the
     * application's buffer.  This saves at least one mem-to-mem copy.
     */
    if ( H5Z_xform_noop(dxpl_cache->data_xform_prop) && H5T_path_noop(tpath)) {
#ifdef H5S_DEBUG
	H5_timer_begin(&timer);
#endif
        H5_CHECK_OVERFLOW(nelmts,hsize_t,size_t);
        status = (sconv->write)(dataset->ent.file, dxpl_cache, dxpl_id,
            dataset, (H5D_storage_t *)&(dataset->efl),
            (size_t)nelmts, H5T_get_size(dataset->type),
            file_space, mem_space,
            buf);
#ifdef H5S_DEBUG
        H5_timer_end(&(sconv->stats[0].write_timer), &timer);
        sconv->stats[0].write_nbytes += nelmts * H5T_get_size(mem_type);
        sconv->stats[0].write_ncalls++;
#endif

        /* Check return value from optimized write */
        if (status<0) {
	    HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "optimized write failed")
	} else
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
    dst_type_size = H5T_get_size(dataset->type);
    max_type_size = MAX(src_type_size, dst_type_size);
    target_size = dxpl_cache->max_temp_buf;
    /* XXX: This could cause a problem if the user sets their buffer size
     * to the same size as the default, and then the dataset elements are
     * too large for the buffer... - QAK
     */
    if(target_size==H5D_XFER_MAX_TEMP_BUF_DEF) {
        /* If the buffer is too small to hold even one element, make it bigger */
        if(target_size<max_type_size)
            target_size = max_type_size;
        /* If the buffer is too large to hold all the elements, make it smaller */
        else if(target_size>(nelmts*max_type_size))
            target_size=(nelmts*max_type_size);
    } /* end if */
    request_nelmts = target_size / max_type_size;

    /* Sanity check elements in temporary buffer */
    if (request_nelmts==0)
        HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL, "temporary buffer max size is too small")

    /* Figure out the strip mine size. */
    if (H5S_select_iter_init(&file_iter, file_space, dst_type_size)<0)
        HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL, "unable to initialize file selection information")
    file_iter_init=1;	/*file selection iteration info has been initialized */
    if (H5S_select_iter_init(&mem_iter, mem_space, src_type_size)<0)
        HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL, "unable to initialize memory selection information")
    mem_iter_init=1;	/*file selection iteration info has been initialized */
    if (H5S_select_iter_init(&bkg_iter, file_space, dst_type_size)<0)
        HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL, "unable to initialize background selection information")
    bkg_iter_init=1;	/*file selection iteration info has been initialized */
     
    /*
     * Get a temporary buffer for type conversion unless the app has already
     * supplied one through the xfer properties.  Instead of allocating a
     * buffer which is the exact size, we allocate the target size. The
     * malloc() is usually less resource-intensive if we allocate/free the
     * same size over and over.
     */
    if(H5T_detect_class(dataset->type, H5T_VLEN)) {
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
    if (NULL==(tconv_buf=dxpl_cache->tconv_buf)) {
        /* Allocate temporary buffer */
        if((tconv_buf=H5FL_BLK_MALLOC(type_conv,target_size))==NULL)
            HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for type conversion")
    } /* end if */
    if (need_bkg && NULL==(bkg_buf=dxpl_cache->bkgr_buf)) {
        /* Allocate background buffer */
        if((bkg_buf=H5FL_BLK_CALLOC(type_conv,(request_nelmts*dst_type_size)))==NULL)
            HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for background conversion")
    } /* end if */

    /* Start strip mining... */
    for (smine_start=0; smine_start<nelmts; smine_start+=smine_nelmts) {
        /* Go figure out how many elements to read from the file */
        assert(H5S_SELECT_ITER_NELMTS(&file_iter)==(nelmts-smine_start));
        smine_nelmts = MIN(request_nelmts, (nelmts-smine_start));
	
        /*
         * Gather data from application buffer into the data type conversion
         * buffer. Also gather data from the file into the background buffer
         * if necessary.
         */
#ifdef H5S_DEBUG
	H5_timer_begin(&timer);
#endif
        n = H5S_select_mgath(buf, mem_space, &mem_iter,
			     smine_nelmts, dxpl_cache, tconv_buf/*out*/);
#ifdef H5S_DEBUG
	H5_timer_end(&(sconv->stats[0].gath_timer), &timer);
	sconv->stats[0].gath_nbytes += n * src_type_size;
	sconv->stats[0].gath_ncalls++;
#endif
        if (n!=smine_nelmts)
            HGOTO_ERROR (H5E_IO, H5E_WRITEERROR, FAIL, "mem gather failed")

        if (H5T_BKG_YES==need_bkg) {
#ifdef H5S_DEBUG
            H5_timer_begin(&timer);
#endif
            n = H5S_select_fgath(dataset->ent.file, dxpl_cache, dxpl_id,
                dataset, (H5D_storage_t *)&(dataset->efl),
                file_space, &bkg_iter, smine_nelmts,
                bkg_buf/*out*/); 

#ifdef H5S_DEBUG
            H5_timer_end(&(sconv->stats[0].bkg_timer), &timer);
            sconv->stats[0].bkg_nbytes += n * dst_type_size;
            sconv->stats[0].bkg_ncalls++;
#endif
            if (n!=smine_nelmts)
                HGOTO_ERROR (H5E_IO, H5E_WRITEERROR, FAIL, "file gather failed")
        } /* end if */
	
	/*
         * Perform data type conversion.
         */
        if (H5T_convert(tpath, src_id, dst_id, smine_nelmts, 0, 0, tconv_buf, bkg_buf, dxpl_id)<0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "data type conversion failed")

	/* Do the data transform after the type conversion (since we're using dataset->type). */ 
	if(!H5Z_xform_noop(dxpl_cache->data_xform_prop))
	    if( H5Z_xform_eval(dxpl_cache->data_xform_prop, tconv_buf, smine_nelmts, dataset->type) < 0)
		    HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "Error performing data transform")

        /*
         * Scatter the data out to the file.
         */
#ifdef H5S_DEBUG
        H5_timer_begin(&timer);
#endif
	status = H5S_select_fscat(dataset->ent.file, dxpl_cache, dxpl_id,
            dataset, (H5D_storage_t *)&(dataset->efl),
            file_space, &file_iter, smine_nelmts,
            tconv_buf);
#ifdef H5S_DEBUG
        H5_timer_end(&(sconv->stats[0].scat_timer), &timer);
        sconv->stats[0].scat_nbytes += smine_nelmts * dst_type_size;
        sconv->stats[0].scat_ncalls++;
#endif
        if (status<0)
            HGOTO_ERROR (H5E_IO, H5E_WRITEERROR, FAIL, "scatter failed")
    } /* end for */

done:
    /* Release selection iterators */
    if(file_iter_init) {
        if(H5S_SELECT_ITER_RELEASE(&file_iter)<0)
            HDONE_ERROR (H5E_DATASET, H5E_CANTFREE, FAIL, "Can't release selection iterator")
    } /* end if */
    if(mem_iter_init) {
        if(H5S_SELECT_ITER_RELEASE(&mem_iter)<0)
            HDONE_ERROR (H5E_DATASET, H5E_CANTFREE, FAIL, "Can't release selection iterator")
    } /* end if */
    if(bkg_iter_init) {
        if(H5S_SELECT_ITER_RELEASE(&bkg_iter)<0)
            HDONE_ERROR (H5E_DATASET, H5E_CANTFREE, FAIL, "Can't release selection iterator")
    } /* end if */

    if (tconv_buf && NULL==dxpl_cache->tconv_buf)
        H5FL_BLK_FREE(type_conv,tconv_buf);
    if (bkg_buf && NULL==dxpl_cache->bkgr_buf)
        H5FL_BLK_FREE(type_conv,bkg_buf);

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
 * Modifications:
 *      QAK - 2003/04/17
 *      Hacked on it a lot. :-)
 *      Leon Arber: 4/20/04
 *      Added support for data transforms.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_chunk_read(hsize_t nelmts, H5D_t *dataset,
    const H5T_t *mem_type, const H5S_t *mem_space,
    const H5S_t *file_space, H5T_path_t *tpath, H5S_conv_t *sconv,
    const H5D_dxpl_cache_t *dxpl_cache, hid_t dxpl_id,
    hid_t src_id, hid_t dst_id, void *buf/*out*/)
{
    fm_map      fm;                     /* File<->memory mapping */
    H5TB_NODE   *chunk_node;           /* Current node in chunk TBBT */
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
    H5S_sel_iter_t mem_iter;            /*memory selection iteration info*/
    hbool_t	mem_iter_init=0;	/*memory selection iteration info has been initialized */
    H5S_sel_iter_t bkg_iter;            /*background iteration info*/
    hbool_t	bkg_iter_init=0;	/*background iteration info has been initialized */
    H5S_sel_iter_t file_iter;           /*file selection iteration info*/
    hbool_t	file_iter_init=0;	/*file selection iteration info has been initialized */
    H5T_bkg_t	need_bkg;		/*type of background buf*/
    uint8_t	*tconv_buf = NULL;	/*data type conv buffer	*/
    uint8_t	*bkg_buf = NULL;	/*background buffer	*/
    H5D_storage_t store;                /*union of EFL and chunk pointer in file space */
    herr_t	ret_value = SUCCEED;	/*return value		*/

    FUNC_ENTER_NOAPI_NOINIT(H5D_chunk_read)
    
    /* Map elements between file and memory for each chunk*/
    if(H5D_create_chunk_map(dataset, mem_type, file_space, mem_space, &fm)<0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "can't build chunk mapping")

    /*
     * If there is no type conversion then read directly into the
     * application's buffer.  This saves at least one mem-to-mem copy.
     */
    if ( H5Z_xform_noop(dxpl_cache->data_xform_prop) && H5T_path_noop(tpath)) {
#ifdef H5S_DEBUG
	H5_timer_begin(&timer);
#endif
  	/* Sanity check dataset, then read it */
        assert(((dataset->layout.type==H5D_CONTIGUOUS && H5F_addr_defined(dataset->layout.u.contig.addr))
                || (dataset->layout.type==H5D_CHUNKED && H5F_addr_defined(dataset->layout.u.chunk.addr)))
            || dataset->efl.nused>0 || dataset->layout.type==H5D_COMPACT);

        /* Get first node in chunk tree */
        chunk_node=H5TB_first(fm.fsel->root);

        /* Iterate through chunks to be operated on */
        while(chunk_node) {
            H5D_chunk_info_t *chunk_info;   /* chunk information */

            /* Get the actual chunk information from the tree node */
            chunk_info=chunk_node->data;

            /* Pass in chunk's coordinates in a union. */
            store.chunk.offset = chunk_info->coords;
            store.chunk.index = chunk_info->index;

            /* Perform the actual read operation */
            status = (sconv->read)(dataset->ent.file, dxpl_cache, dxpl_id,
                dataset, &store,
                chunk_info->chunk_points, H5T_get_size(dataset->type),
                chunk_info->fspace, chunk_info->mspace,
                buf);
        
            /* Check return value from optimized read */
            if (status<0)
                HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "optimized read failed")

            /* Get the next chunk node in the tree */
            chunk_node=H5TB_next(chunk_node);
        } /* end while */
        
#ifdef H5S_DEBUG
        H5_timer_end(&(sconv->stats[1].read_timer), &timer);
        sconv->stats[1].read_nbytes += nelmts * H5T_get_size(dataset->type);
        sconv->stats[1].read_ncalls++;
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
    src_type_size = H5T_get_size(dataset->type);
    dst_type_size = H5T_get_size(mem_type);
    max_type_size = MAX(src_type_size, dst_type_size);
    target_size = dxpl_cache->max_temp_buf;
    /* XXX: This could cause a problem if the user sets their buffer size
     * to the same size as the default, and then the dataset elements are
     * too large for the buffer... - QAK
     */
    if(target_size==H5D_XFER_MAX_TEMP_BUF_DEF) {
        /* If the buffer is too small to hold even one element, make it bigger */
        if(target_size<max_type_size)
            target_size = max_type_size;
        /* If the buffer is too large to hold all the elements, make it smaller */
        else if(target_size>(nelmts*max_type_size))
            target_size=(nelmts*max_type_size);
    } /* end if */
    request_nelmts = target_size / max_type_size;
        
    /* Sanity check elements in temporary buffer */
    if (request_nelmts==0)
        HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL, "temporary buffer max size is too small")

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
    if (NULL==(tconv_buf=dxpl_cache->tconv_buf)) {
        /* Allocate temporary buffer */
        if((tconv_buf=H5FL_BLK_MALLOC(type_conv,target_size))==NULL)
            HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for type conversion")
    } /* end if */
    if (need_bkg && NULL==(bkg_buf=dxpl_cache->bkgr_buf)) {
        /* Allocate background buffer */
        if((bkg_buf=H5FL_BLK_CALLOC(type_conv,(request_nelmts*dst_type_size)))==NULL)
            HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for background conversion")
    } /* end if */
    
    /* Loop over all the chunks, performing I/O on each */

    /* Get first node in chunk tree */
    chunk_node=H5TB_first(fm.fsel->root);

    /* Iterate through chunks to be operated on */
    while(chunk_node) {
        H5D_chunk_info_t *chunk_info;   /* chunk information */

        /* Get the actual chunk information from the tree nodes */
        chunk_info=chunk_node->data;

        /* initialize selection iterator */
        if (H5S_select_iter_init(&file_iter, chunk_info->fspace, src_type_size)<0)
            HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL, "unable to initialize file selection information")
        file_iter_init=1;	/*file selection iteration info has been initialized */
        if (H5S_select_iter_init(&mem_iter, chunk_info->mspace, dst_type_size)<0)
            HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL, "unable to initialize memory selection information")
        mem_iter_init=1;	/*file selection iteration info has been initialized */
        if (H5S_select_iter_init(&bkg_iter, chunk_info->mspace, dst_type_size)<0)
            HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL, "unable to initialize background selection information")
        bkg_iter_init=1;	/*file selection iteration info has been initialized */
        
        /* Pass in chunk's coordinates in a union*/
        store.chunk.offset = chunk_info->coords;
        store.chunk.index = chunk_info->index;
           
        for (smine_start=0; smine_start<chunk_info->chunk_points; smine_start+=smine_nelmts) {
            /* Go figure out how many elements to read from the file */
            assert(H5S_SELECT_ITER_NELMTS(&file_iter)==(chunk_info->chunk_points-smine_start));
            smine_nelmts = MIN(request_nelmts, (chunk_info->chunk_points-smine_start));
        
            /*
             * Gather the data from disk into the data type conversion
             * buffer. Also gather data from application to background buffer
             * if necessary.
             */
#ifdef H5S_DEBUG
            H5_timer_begin(&timer);
#endif
            /* Sanity check that space is allocated, then read data from it */ 
            assert(((dataset->layout.type==H5D_CONTIGUOUS && H5F_addr_defined(dataset->layout.u.contig.addr))
                    || (dataset->layout.type==H5D_CHUNKED && H5F_addr_defined(dataset->layout.u.chunk.addr)))
                || dataset->efl.nused>0 || dataset->layout.type==H5D_COMPACT);
            n = H5S_select_fgath(dataset->ent.file, dxpl_cache, dxpl_id,
                dataset, &store,
                chunk_info->fspace, &file_iter, smine_nelmts,
                tconv_buf/*out*/);

#ifdef H5S_DEBUG
            H5_timer_end(&(sconv->stats[1].gath_timer), &timer);
            sconv->stats[1].gath_nbytes += n * src_type_size;
            sconv->stats[1].gath_ncalls++;
#endif
            if (n!=smine_nelmts)
                HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, "file gather failed")

            if (H5T_BKG_YES==need_bkg) {
#ifdef H5S_DEBUG
                H5_timer_begin(&timer);
#endif
                n = H5S_select_mgath(buf, chunk_info->mspace, &bkg_iter,
                                 smine_nelmts, dxpl_cache, bkg_buf/*out*/);
#ifdef H5S_DEBUG
                H5_timer_end(&(sconv->stats[1].bkg_timer), &timer);
                sconv->stats[1].bkg_nbytes += n * dst_type_size;
                sconv->stats[1].bkg_ncalls++;
#endif
                if (n!=smine_nelmts)
                    HGOTO_ERROR (H5E_IO, H5E_READERROR, FAIL, "mem gather failed")
            } /* end if */
    
            /*
             * Perform data type conversion.
             */
            if (H5T_convert(tpath, src_id, dst_id, smine_nelmts, 0, 0, 
                    tconv_buf, bkg_buf, dxpl_id)<0)
                HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "data type conversion failed")

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
            status = H5S_select_mscat(tconv_buf, chunk_info->mspace,
                        &mem_iter, smine_nelmts, dxpl_cache, buf/*out*/);
#ifdef H5S_DEBUG
            H5_timer_end(&(sconv->stats[1].scat_timer), &timer);
            sconv->stats[1].scat_nbytes += smine_nelmts * dst_type_size;
            sconv->stats[1].scat_ncalls++;
#endif
            if (status<0)
                HGOTO_ERROR (H5E_DATASET, H5E_READERROR, FAIL, "scatter failed")
        } /* end for */

        /* Release selection iterators */
        if(file_iter_init) {
            if(H5S_SELECT_ITER_RELEASE(&file_iter)<0)
                HGOTO_ERROR (H5E_DATASET, H5E_CANTFREE, FAIL, "Can't release selection iterator")
            file_iter_init=0;
        } /* end if */
        if(mem_iter_init) {
            if(H5S_SELECT_ITER_RELEASE(&mem_iter)<0)
                HGOTO_ERROR (H5E_DATASET, H5E_CANTFREE, FAIL, "Can't release selection iterator")
            mem_iter_init=0;
        } /* end if */
        if(bkg_iter_init) {
            if(H5S_SELECT_ITER_RELEASE(&bkg_iter)<0)
                HGOTO_ERROR (H5E_DATASET, H5E_CANTFREE, FAIL, "Can't release selection iterator")
            bkg_iter_init=0;
        } /* end if */

        /* Get the next chunk node in the tree */
        chunk_node=H5TB_next(chunk_node);
    } /* end while */
     
done:
    /* Release selection iterators, if necessary */
    if(file_iter_init) {
        if(H5S_SELECT_ITER_RELEASE(&file_iter)<0)
            HDONE_ERROR (H5E_DATASET, H5E_CANTFREE, FAIL, "Can't release selection iterator")
    } /* end if */
    if(mem_iter_init) {
        if(H5S_SELECT_ITER_RELEASE(&mem_iter)<0)
            HDONE_ERROR (H5E_DATASET, H5E_CANTFREE, FAIL, "Can't release selection iterator")
    } /* end if */
    if(bkg_iter_init) {
        if(H5S_SELECT_ITER_RELEASE(&bkg_iter)<0)
            HDONE_ERROR (H5E_DATASET, H5E_CANTFREE, FAIL, "Can't release selection iterator")
    } /* end if */

    if (tconv_buf && NULL==dxpl_cache->tconv_buf)
        H5FL_BLK_FREE(type_conv,tconv_buf);
    if (bkg_buf && NULL==dxpl_cache->bkgr_buf)
        H5FL_BLK_FREE(type_conv,bkg_buf);
        
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
 * Modifications:
 *      QAK - 2003/04/17
 *      Hacked on it a lot. :-)
 *      Leon Arber: 4/20/04
 *      Added support for data transforms.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_chunk_write(hsize_t nelmts, H5D_t *dataset,
    const H5T_t *mem_type, const H5S_t *mem_space,
    const H5S_t *file_space, H5T_path_t *tpath, H5S_conv_t *sconv,
    const H5D_dxpl_cache_t *dxpl_cache, hid_t dxpl_id,
    hid_t src_id, hid_t dst_id, const void *buf)
{
    fm_map      fm;                     /* File<->memory mapping */
    H5TB_NODE   *chunk_node;           /* Current node in chunk TBBT */
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
    H5S_sel_iter_t mem_iter;            /*memory selection iteration info*/
    hbool_t	mem_iter_init=0;	/*memory selection iteration info has been initialized */
    H5S_sel_iter_t bkg_iter;            /*background iteration info*/
    hbool_t	bkg_iter_init=0;	/*background iteration info has been initialized */
    H5S_sel_iter_t file_iter;           /*file selection iteration info*/
    hbool_t	file_iter_init=0;	/*file selection iteration info has been initialized */
    H5T_bkg_t	need_bkg;		/*type of background buf*/
    uint8_t	*tconv_buf = NULL;	/*data type conv buffer	*/
    uint8_t	*bkg_buf = NULL;	/*background buffer	*/
    H5D_storage_t store;                /*union of EFL and chunk pointer in file space */
    herr_t	ret_value = SUCCEED;	/*return value		*/

    FUNC_ENTER_NOAPI_NOINIT(H5D_chunk_write)
    
#ifdef QAK
{
    int mpi_rank;
    double time;
    MPI_Comm_rank(MPI_COMM_WORLD,&mpi_rank);
    time = MPI_Wtime();
    HDfprintf(stderr,"%s: rank=%d - Entering, time=%f\n",FUNC,mpi_rank,time);
}
#endif /* QAK */
    /* Map elements between file and memory for each chunk*/
    if(H5D_create_chunk_map(dataset, mem_type, file_space, mem_space, &fm)<0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "can't build chunk mapping")
#ifdef QAK
{
    int mpi_rank;
    double time;
    MPI_Comm_rank(MPI_COMM_WORLD,&mpi_rank);
    time = MPI_Wtime();
    HDfprintf(stderr,"%s: rank=%d - After creating chunk map, time=%f\n",FUNC,mpi_rank,time);
}
#endif /* QAK */
   
    /*
     * If there is no type conversion then write directly from the
     * application's buffer.  This saves at least one mem-to-mem copy.
     */
    if ( H5Z_xform_noop(dxpl_cache->data_xform_prop) && H5T_path_noop(tpath)) {
#ifdef H5S_DEBUG
	H5_timer_begin(&timer);
#endif
#ifdef QAK
{
    int mpi_rank;
    double time;
    MPI_Comm_rank(MPI_COMM_WORLD,&mpi_rank);
    time = MPI_Wtime();
    HDfprintf(stderr,"%s: rank=%d - Performing optimized I/O, time=%f\n",FUNC,mpi_rank,time);
}
#endif /* QAK */
        /* Get first node in chunk tree */
        chunk_node=H5TB_first(fm.fsel->root);

        /* Iterate through chunks to be operated on */
        while(chunk_node) {
            H5D_chunk_info_t *chunk_info;   /* chunk information */

            /* Get the actual chunk information from the tree node */
            chunk_info=chunk_node->data;

            /* Pass in chunk's coordinates in a union. */
            store.chunk.offset = chunk_info->coords;
            store.chunk.index = chunk_info->index;

            /* Perform the actual write operation */
            status = (sconv->write)(dataset->ent.file, dxpl_cache, dxpl_id,
                dataset, &store,
                chunk_info->chunk_points, H5T_get_size(dataset->type),
                chunk_info->fspace, chunk_info->mspace,
                buf);
        
            /* Check return value from optimized write */
            if (status<0)
                HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "optimized write failed")

            /* Get the next chunk node in the tree */
            chunk_node=H5TB_next(chunk_node);
        } /* end while */
#ifdef QAK
{
    int mpi_rank;
    double time;
    MPI_Comm_rank(MPI_COMM_WORLD,&mpi_rank);
    time = MPI_Wtime();
    HDfprintf(stderr,"%s: rank=%d - Done performing optimized I/O, time=%f\n",FUNC,mpi_rank,time);
}
#endif /* QAK */
        
#ifdef H5S_DEBUG
	H5_timer_end(&(sconv->stats[0].write_timer), &timer);
	sconv->stats[0].write_nbytes += nelmts * H5T_get_size(mem_type);
	sconv->stats[0].write_ncalls++;
#endif

	/* direct xfer accomplished successfully */
	HGOTO_DONE(SUCCEED)
    } /* end if */
#ifdef QAK
{
    int mpi_rank;
    MPI_Comm_rank(MPI_COMM_WORLD,&mpi_rank);
    HDfprintf(stderr,"%s: rank=%d - Performing NON-optimized I/O\n",FUNC,mpi_rank);
}
#endif /* QAK */
   
    /*
     * This is the general case (type conversion, usually).
     */
    if(nelmts==0)
        HGOTO_DONE(SUCCEED)
     
    /* Compute element sizes and other parameters */
    src_type_size = H5T_get_size(mem_type);
    dst_type_size = H5T_get_size(dataset->type);
    max_type_size = MAX(src_type_size, dst_type_size);
    target_size = dxpl_cache->max_temp_buf;
    /* XXX: This could cause a problem if the user sets their buffer size
     * to the same size as the default, and then the dataset elements are
     * too large for the buffer... - QAK
     */
    if(target_size==H5D_XFER_MAX_TEMP_BUF_DEF) {
        /* If the buffer is too small to hold even one element, make it bigger */
        if(target_size<max_type_size)
            target_size = max_type_size;
        /* If the buffer is too large to hold all the elements, make it smaller */
        else if(target_size>(nelmts*max_type_size))
            target_size=(nelmts*max_type_size);
    } /* end if */
    request_nelmts = target_size / max_type_size;

    /* Sanity check elements in temporary buffer */
    if (request_nelmts==0)
        HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL, "temporary buffer max size is too small")

    /*
     * Get a temporary buffer for type conversion unless the app has already
     * supplied one through the xfer properties.  Instead of allocating a
     * buffer which is the exact size, we allocate the target size. The
     * malloc() is usually less resource-intensive if we allocate/free the
     * same size over and over.
     */
    if(H5T_detect_class(dataset->type, H5T_VLEN)) {
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
    if (NULL==(tconv_buf=dxpl_cache->tconv_buf)) {
        /* Allocate temporary buffer */
        if((tconv_buf=H5FL_BLK_MALLOC(type_conv,target_size))==NULL)
            HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for type conversion")
    } /* end if */
    if (need_bkg && NULL==(bkg_buf=dxpl_cache->bkgr_buf)) {
        /* Allocate background buffer */
        if((bkg_buf=H5FL_BLK_CALLOC(type_conv,(request_nelmts*dst_type_size)))==NULL)
            HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for background conversion")
    } /* end if */

    /* Loop over all the chunks, performing I/O on each */

    /* Get first node in chunk tree */
    chunk_node=H5TB_first(fm.fsel->root);

    /* Iterate through chunks to be operated on */
    while(chunk_node) {
        H5D_chunk_info_t *chunk_info;   /* chunk information */

        /* Get the actual chunk information from the tree node */
        chunk_info=chunk_node->data;

        /* initialize selection iterator */
        if (H5S_select_iter_init(&file_iter, chunk_info->fspace, dst_type_size)<0)
            HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL, "unable to initialize file selection information")
        file_iter_init=1;	/*file selection iteration info has been initialized */
        if (H5S_select_iter_init(&mem_iter, chunk_info->mspace, src_type_size)<0)
            HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL, "unable to initialize memory selection information")
        mem_iter_init=1;	/*file selection iteration info has been initialized */
        if (H5S_select_iter_init(&bkg_iter, chunk_info->fspace, dst_type_size)<0)
            HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL, "unable to initialize background selection information")
        bkg_iter_init=1;	/*file selection iteration info has been initialized */
        
        /*pass in chunk's coordinates in a union*/
        store.chunk.offset = chunk_info->coords;
        store.chunk.index = chunk_info->index;
       
        for (smine_start=0; smine_start<chunk_info->chunk_points; smine_start+=smine_nelmts) {
            /* Go figure out how many elements to read from the file */
            assert(H5S_SELECT_ITER_NELMTS(&file_iter)==(chunk_info->chunk_points-smine_start));
            smine_nelmts = MIN(request_nelmts, (chunk_info->chunk_points-smine_start));
            
            /*
             * Gather the data from disk into the data type conversion
             * buffer. Also gather data from application to background buffer
             * if necessary.
             */
#ifdef H5S_DEBUG
            H5_timer_begin(&timer);
#endif
            n = H5S_select_mgath(buf, chunk_info->mspace, &mem_iter,
                                 smine_nelmts, dxpl_cache, tconv_buf/*out*/);
    
#ifdef H5S_DEBUG
            H5_timer_end(&(sconv->stats[1].gath_timer), &timer);
            sconv->stats[1].gath_nbytes += n * src_type_size;
            sconv->stats[1].gath_ncalls++;
#endif
            if (n!=smine_nelmts)
                HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, "file gather failed")

            if (H5T_BKG_YES==need_bkg) {
#ifdef H5S_DEBUG
                H5_timer_begin(&timer);
#endif
                n = H5S_select_fgath(dataset->ent.file, dxpl_cache, dxpl_id,
                    dataset, &store,
                    chunk_info->fspace, &bkg_iter, smine_nelmts,
                    bkg_buf/*out*/); 

#ifdef H5S_DEBUG
                H5_timer_end(&(sconv->stats[0].bkg_timer), &timer);
                sconv->stats[0].bkg_nbytes += n * dst_type_size;
                sconv->stats[0].bkg_ncalls++;
#endif
                if (n!=smine_nelmts)
                    HGOTO_ERROR (H5E_IO, H5E_WRITEERROR, FAIL, "file gather failed")
            } /* end if */

	    /*
             * Perform data type conversion.
             */
            if (H5T_convert(tpath, src_id, dst_id, smine_nelmts, 0, 0, 
                    tconv_buf, bkg_buf, dxpl_id)<0)
                HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "data type conversion failed")

 	    /* Do the data transform after the type conversion (since we're using dataset->type) */
            if(!H5Z_xform_noop(dxpl_cache->data_xform_prop))
	      if( H5Z_xform_eval(dxpl_cache->data_xform_prop, tconv_buf, smine_nelmts, dataset->type) < 0)
		     HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "Error performing data transform")

            /*
             * Scatter the data out to the file.
             */
#ifdef H5S_DEBUG
            H5_timer_begin(&timer);
#endif
            status = H5S_select_fscat(dataset->ent.file, dxpl_cache, dxpl_id,
                dataset, &store,
                chunk_info->fspace, &file_iter, smine_nelmts,
                tconv_buf);

#ifdef H5S_DEBUG
            H5_timer_end(&(sconv->stats[0].scat_timer), &timer);
            sconv->stats[0].scat_nbytes += n * dst_type_size;
            sconv->stats[0].scat_ncalls++;
#endif
            if (status<0)
                HGOTO_ERROR (H5E_IO, H5E_WRITEERROR, FAIL, "scatter failed")
        } /* end for */

        /* Release selection iterators */
        if(file_iter_init) {
            if(H5S_SELECT_ITER_RELEASE(&file_iter)<0)
                HGOTO_ERROR (H5E_DATASET, H5E_CANTFREE, FAIL, "Can't release selection iterator")
            file_iter_init=0;
        } /* end if */
        if(mem_iter_init) {
            if(H5S_SELECT_ITER_RELEASE(&mem_iter)<0)
                HGOTO_ERROR (H5E_DATASET, H5E_CANTFREE, FAIL, "Can't release selection iterator")
            mem_iter_init=0;
        } /* end if */
        if(bkg_iter_init) {
            if(H5S_SELECT_ITER_RELEASE(&bkg_iter)<0)
                HGOTO_ERROR (H5E_DATASET, H5E_CANTFREE, FAIL, "Can't release selection iterator")
            bkg_iter_init=0;
        } /* end if */

        /* Get the next chunk node in the tree */
        chunk_node=H5TB_next(chunk_node);
    } /* end while */

done:
    /* Release selection iterators, if necessary */
    if(file_iter_init) {
        if(H5S_SELECT_ITER_RELEASE(&file_iter)<0)
            HDONE_ERROR (H5E_DATASET, H5E_CANTFREE, FAIL, "Can't release selection iterator")
    } /* end if */
    if(mem_iter_init) {
        if(H5S_SELECT_ITER_RELEASE(&mem_iter)<0)
            HDONE_ERROR (H5E_DATASET, H5E_CANTFREE, FAIL, "Can't release selection iterator")
    } /* end if */
    if(bkg_iter_init) {
        if(H5S_SELECT_ITER_RELEASE(&bkg_iter)<0)
            HDONE_ERROR (H5E_DATASET, H5E_CANTFREE, FAIL, "Can't release selection iterator")
    } /* end if */
 
    if (tconv_buf && NULL==dxpl_cache->tconv_buf)
        H5FL_BLK_FREE(type_conv,tconv_buf);
    if (bkg_buf && NULL==dxpl_cache->bkgr_buf)
        H5FL_BLK_FREE(type_conv,bkg_buf);
        
    /* Release chunk mapping information */
    if(H5D_destroy_chunk_map(&fm) < 0)
        HDONE_ERROR(H5E_DATASET, H5E_CANTRELEASE, FAIL, "can't release chunk mapping")

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5D_chunk_write() */

#ifdef H5_HAVE_PARALLEL

/*-------------------------------------------------------------------------
 * Function:	H5D_io_assist_mpio
 *
 * Purpose:	Common logic for determining if the MPI transfer mode should
 *              be changed.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *		Thursday, April 10, 2003
 *
 * Modifications:
 *      QAK - 2003/04/17
 *      Hacked on it a lot. :-)
 *
 *-------------------------------------------------------------------------
 */
static herr_t 
H5D_io_assist_mpio(hid_t dxpl_id, H5D_dxpl_cache_t *dxpl_cache, 
            hbool_t *xfer_mode_changed)
{ 
    herr_t	ret_value = SUCCEED;	/*return value		*/

    FUNC_ENTER_NOAPI_NOINIT(H5D_io_assist_mpio)
    
    /* The following may not handle a collective call correctly
     * since it does not ensure all processes can handle the write
     * request according to the MPI collective specification.
     * Do the collective request via independent mode.
     */
    if (dxpl_cache->xfer_mode==H5FD_MPIO_COLLECTIVE) {
        H5P_genplist_t *dx_plist;           /* Data transer property list */

        /* Get the dataset transfer property list */
        if (NULL == (dx_plist = H5I_object(dxpl_id)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset creation property list")

	/* Kludge: change the xfer_mode to independent, handle the request,
	 * then xfer_mode before return.
	 * Better way is to get a temporary data_xfer property with
	 * INDEPENDENT xfer_mode and pass it downwards.
	 */
	dxpl_cache->xfer_mode = H5FD_MPIO_INDEPENDENT;
        if(H5P_set (dx_plist, H5D_XFER_IO_XFER_MODE_NAME, &dxpl_cache->xfer_mode) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set transfer mode")
	*xfer_mode_changed=TRUE;	/* restore it before return */
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D_io_assist_mpio() */


/*-------------------------------------------------------------------------
 * Function:	H5D_io_restore_mpio
 *
 * Purpose:	Common logic for restoring MPI transfer mode 
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Friday, February  6, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t 
H5D_io_restore_mpio(hid_t dxpl_id)
{ 
    H5P_genplist_t *dx_plist;           /* Data transer property list */
    H5FD_mpio_xfer_t xfer_mode;         /*xfer_mode for this request */
    herr_t	ret_value = SUCCEED;	/*return value		*/

    FUNC_ENTER_NOAPI_NOINIT(H5D_io_restore_mpio)
    
    /* Get the dataset transfer property list */
    if (NULL == (dx_plist = H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset creation property list")

    /* Kludge: change the xfer_mode to independent, handle the request,
     * then xfer_mode before return.
     * Better way is to get a temporary data_xfer property with
     * INDEPENDENT xfer_mode and pass it downwards.
     */
    xfer_mode = H5FD_MPIO_COLLECTIVE;
    if(H5P_set (dx_plist, H5D_XFER_IO_XFER_MODE_NAME, &xfer_mode) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set transfer mode")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D_io_restore_mpio() */
#endif /*H5_HAVE_PARALLEL*/


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
 * Modifications:
 *      QAK - 2003/04/17
 *      Hacked on it a lot. :-)
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_create_chunk_map(H5D_t *dataset, const H5T_t *mem_type, const H5S_t *file_space, 
            const H5S_t *mem_space, fm_map *fm)
{
    H5S_t *tmp_mspace=NULL;     /* Temporary memory dataspace */
    H5S_t *equiv_mspace=NULL;   /* Equivalent memory dataspace */
    hbool_t equiv_mspace_init=0;/* Equivalent memory dataspace was created */
    hid_t f_tid=(-1);           /* Temporary copy of file datatype for iteration */
    hbool_t iter_init=0;        /* Selection iteration info has been initialized */
    unsigned f_ndims;           /* The number of dimensions of the file's dataspace */
    int sm_ndims;               /* The number of dimensions of the memory buffer's dataspace (signed) */
    H5TB_NODE *curr_node;       /* Current node in TBBT */
    H5S_sel_type fsel_type;     /* Selection type on disk */
    char bogus;                 /* "bogus" buffer to pass to selection iterator */
    unsigned u;                 /* Local index variable */
    herr_t ret_value = SUCCEED;	/* Return value		*/
     
    FUNC_ENTER_NOAPI_NOINIT(H5D_create_chunk_map)
#ifdef QAK
{
    int mpi_rank;
    double time;
    MPI_Comm_rank(MPI_COMM_WORLD,&mpi_rank);
    time = MPI_Wtime();
    HDfprintf(stderr,"%s: rank=%d - Entering, time=%f\n",FUNC,mpi_rank,time);
}
#endif /* QAK */

    /* Get layout for dataset */
    fm->layout = &(dataset->layout);
    
    /* Check if the memory space is scalar & make equivalent memory space */
    if((sm_ndims = H5S_GET_EXTENT_NDIMS(mem_space))<0)
        HGOTO_ERROR (H5E_DATASPACE, H5E_CANTGET, FAIL, "unable to get dimension number")
    if(sm_ndims==0) {
        hsize_t dims[H5O_LAYOUT_NDIMS];    /* Temporary dimension information */

        /* Set up "equivalent" n-dimensional dataspace with size '1' in each dimension */
        for(u=0; u<dataset->layout.u.chunk.ndims-1; u++)
            dims[u]=1;
        if((equiv_mspace = H5S_create_simple(dataset->layout.u.chunk.ndims-1,dims,NULL))==NULL)
            HGOTO_ERROR (H5E_DATASPACE, H5E_CANTCREATE, FAIL, "unable to create equivalent dataspace for scalar space")

        /* Indicate that this space needs to be released */
        equiv_mspace_init=1;

        /* Set the number of dimensions for the memory dataspace */
        fm->m_ndims=dataset->layout.u.chunk.ndims-1;
    } /* end else */
    else {
        equiv_mspace=(H5S_t *)mem_space; /* Casting away 'const' OK... */

        /* Set the number of dimensions for the memory dataspace */
        H5_ASSIGN_OVERFLOW(fm->m_ndims,sm_ndims,int,unsigned);
    } /* end else */
 
    /* Get dim number and dimensionality for each dataspace */
    fm->f_ndims=f_ndims=dataset->layout.u.chunk.ndims-1;

    if(H5S_get_simple_extent_dims(file_space, fm->f_dims, NULL)<0)
        HGOTO_ERROR (H5E_DATASPACE, H5E_CANTGET, FAIL, "unable to get dimensionality")

    /* Decide the number of chunks in each dimension*/
    for(u=0; u<f_ndims; u++) {
        /* Keep the size of the chunk dimensions as hsize_t for various routines */
        fm->chunk_dim[u]=fm->layout->u.chunk.dim[u];

        /* Round up to the next integer # of chunks, to accomodate partial chunks */
        fm->chunks[u] = ((fm->f_dims[u]+dataset->layout.u.chunk.dim[u])-1) / dataset->layout.u.chunk.dim[u];
    } /* end for */
        
    /* Compute the "down" size of 'chunks' information */
    if(H5V_array_down(f_ndims,fm->chunks,fm->down_chunks)<0)
        HGOTO_ERROR (H5E_INTERNAL, H5E_BADVALUE, FAIL, "can't compute 'down' sizes")

    /* Initialize TBBT for chunk selections */
    if((fm->fsel=H5TB_fast_dmake(H5TB_FAST_HSIZE_COMPARE))==NULL)
        HGOTO_ERROR(H5E_DATASET,H5E_CANTMAKETREE,FAIL,"can't create TBBT for chunk selections")

    /* Initialize "last chunk" information */
    fm->last_index=(hsize_t)-1;
    fm->last_chunk_info=NULL;

    /* Point at the dataspaces */
    fm->file_space=file_space;
    fm->mem_space=equiv_mspace;
    fm->mem_space_copy=equiv_mspace_init;       /* Make certain to copy memory dataspace if necessary */

    /* Get type of selection on disk & in memory */
    if((fsel_type=H5S_GET_SELECT_TYPE(file_space))<0)
        HGOTO_ERROR (H5E_DATASET, H5E_BADSELECT, FAIL, "unable to convert from file to memory data space")
    if((fm->msel_type=H5S_GET_SELECT_TYPE(equiv_mspace))<0)
        HGOTO_ERROR (H5E_DATASET, H5E_BADSELECT, FAIL, "unable to convert from file to memory data space")

    /* Check if file selection is a point selection */
    if(fsel_type==H5S_SEL_POINTS) {
        /* Create temporary datatypes for selection iteration */
        if((f_tid = H5I_register(H5I_DATATYPE, H5T_copy(dataset->type, H5T_COPY_ALL)))<0)
            HGOTO_ERROR (H5E_DATATYPE, H5E_CANTREGISTER, FAIL, "unable to register file datatype")
        
        /* Spaces aren't the same shape, iterate over the memory selection directly */
        if(H5S_select_iterate(&bogus, f_tid, file_space,  H5D_chunk_file_cb, fm)<0)
            HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL, "unable to create file chunk selections")

        /* Reset "last chunk" info */
        fm->last_index=(hsize_t)-1;
        fm->last_chunk_info=NULL;
    } /* end if */
    else {
#ifdef QAK
    {
        int mpi_rank;
        double time;
        MPI_Comm_rank(MPI_COMM_WORLD,&mpi_rank);
        time = MPI_Wtime();
        HDfprintf(stderr,"%s: rank=%d - Before creating chunk selections, time=%f\n",FUNC,mpi_rank,time);
    }
#endif /* QAK */
        /* Build the file selection for each chunk */
        if(H5D_create_chunk_file_map_hyper(fm)<0)
            HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL, "unable to create file chunk selections")
#ifdef QAK
    {
        int mpi_rank;
        double time;
        MPI_Comm_rank(MPI_COMM_WORLD,&mpi_rank);
        time = MPI_Wtime();
        HDfprintf(stderr,"%s: rank=%d - After creating file chunk selections, time=%f\n",FUNC,mpi_rank,time);
        HDfprintf(stderr,"%s: rank=%d - H5S_select_shape_same=%d\n",FUNC,mpi_rank,H5S_select_shape_same(file_space,equiv_mspace));
    }
#endif /* QAK */

        /* Clean file chunks' hyperslab span "scratch" information */
        curr_node=H5TB_first(fm->fsel->root);
        while(curr_node) {
            H5D_chunk_info_t *chunk_info;   /* Pointer chunk information */

            /* Get pointer to chunk's information */
            chunk_info=curr_node->data;
            assert(chunk_info);

            /* Clean hyperslab span's "scratch" information */
            if(H5S_hyper_reset_scratch(chunk_info->fspace)<0)
                HGOTO_ERROR (H5E_DATASET, H5E_CANTFREE, FAIL, "unable to reset span scratch info")

            /* Get the next chunk node in the TBBT */
            curr_node=H5TB_next(curr_node);
        } /* end while */
    } /* end else */

    /* Build the memory selection for each chunk */
    if(fsel_type!=H5S_SEL_POINTS && H5S_select_shape_same(file_space,equiv_mspace)==TRUE) {
        /* Reset chunk template information */
        fm->mchunk_tmpl=NULL;

        /* If the selections are the same shape, use the file chunk information
         * to generate the memory chunk information quickly.
         */
        if(H5D_create_chunk_mem_map_hyper(fm)<0)
            HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL, "unable to create memory chunk selections")
    } /* end if */
    else {
        size_t elmt_size;           /* Memory datatype size */

        /* Make a copy of equivalent memory space */
        if((tmp_mspace = H5S_copy(equiv_mspace,TRUE))==NULL)
            HGOTO_ERROR (H5E_DATASPACE, H5E_CANTCOPY, FAIL, "unable to copy memory space")
     
        /* De-select the mem space copy */
        if(H5S_select_none(tmp_mspace)<0)
            HGOTO_ERROR (H5E_DATASPACE, H5E_CANTINIT, FAIL, "unable to de-select memory space")

        /* Save chunk template information */
        fm->mchunk_tmpl=tmp_mspace;

        /* Create temporary datatypes for selection iteration */
        if(f_tid<0) {
            if((f_tid = H5I_register(H5I_DATATYPE, H5T_copy(dataset->type, H5T_COPY_ALL)))<0)
                HGOTO_ERROR (H5E_DATATYPE, H5E_CANTREGISTER, FAIL, "unable to register file datatype")
        } /* end if */
        
        /* Create selection iterator for memory selection */
        if((elmt_size=H5T_get_size(mem_type))==0)
            HGOTO_ERROR(H5E_DATATYPE, H5E_BADSIZE, FAIL, "datatype size invalid")
        if (H5S_select_iter_init(&(fm->mem_iter), equiv_mspace, elmt_size)<0)
            HGOTO_ERROR (H5E_DATASPACE, H5E_CANTINIT, FAIL, "unable to initialize selection iterator")
        iter_init=1;	/* Selection iteration info has been initialized */

        /* Spaces aren't the same shape, iterate over the memory selection directly */
        if(H5S_select_iterate(&bogus, f_tid, file_space,  H5D_chunk_mem_cb, fm)<0)
            HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL, "unable to create memory chunk selections")

        /* Clean up hyperslab stuff, if necessary */
        if(fm->msel_type!=H5S_SEL_POINTS) {
            /* Clean memory chunks' hyperslab span "scratch" information */
            curr_node=H5TB_first(fm->fsel->root);
            while(curr_node) {
                H5D_chunk_info_t *chunk_info;   /* Pointer chunk information */

                /* Get pointer to chunk's information */
                chunk_info=curr_node->data;
                assert(chunk_info);

                /* Clean hyperslab span's "scratch" information */
                if(H5S_hyper_reset_scratch(chunk_info->mspace)<0)
                    HGOTO_ERROR (H5E_DATASET, H5E_CANTFREE, FAIL, "unable to reset span scratch info")

                /* Get the next chunk node in the TBBT */
                curr_node=H5TB_next(curr_node);
            } /* end while */
        } /* end if */
    } /* end else */

#ifdef QAK
{
    int mpi_rank;
    double time;
    MPI_Comm_rank(MPI_COMM_WORLD,&mpi_rank);
    time = MPI_Wtime();
    HDfprintf(stderr,"%s: rank=%d - After creating chunk selections, time=%f\n",FUNC,mpi_rank,time);
}
#endif /* QAK */

done:
    /* Release the [potentially partially built] chunk mapping information if an error occurs */
    if(ret_value<0) {
        if(tmp_mspace && !fm->mchunk_tmpl) {
            if(H5S_close(tmp_mspace)<0)
                HDONE_ERROR(H5E_DATASPACE, H5E_CANTRELEASE, FAIL, "can't release memory chunk dataspace template")
        } /* end if */

        if (H5D_destroy_chunk_map(fm)<0)
            HDONE_ERROR (H5E_DATASPACE, H5E_CANTRELEASE, FAIL, "unable to release chunk mapping")
    } /* end if */

    /* Reset the global dataspace info */
    fm->file_space=NULL;
    fm->mem_space=NULL;

    if(equiv_mspace_init && equiv_mspace) {
        if(H5S_close(equiv_mspace)<0)
            HDONE_ERROR(H5E_DATASPACE, H5E_CANTRELEASE, FAIL, "can't release memory chunk dataspace template")
    } /* end if */
    if(iter_init) {
        if (H5S_SELECT_ITER_RELEASE(&(fm->mem_iter))<0)
            HDONE_ERROR (H5E_DATASPACE, H5E_CANTRELEASE, FAIL, "unable to release selection iterator")
    }
    if(f_tid!=(-1)) {
        if(H5I_dec_ref(f_tid)<0)
            HDONE_ERROR (H5E_DATASET, H5E_CANTFREE, FAIL, "Can't decrement temporary datatype ID")
    } /* end if */
   
#ifdef QAK
{
    int mpi_rank;
    double time;
    MPI_Comm_rank(MPI_COMM_WORLD,&mpi_rank);
    time = MPI_Wtime();
    HDfprintf(stderr,"%s: rank=%d - Leaving, time=%f\n",FUNC,mpi_rank,time);
}
#endif /* QAK */
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D_create_chunk_map() */


/*--------------------------------------------------------------------------
 NAME
    H5D_free_chunk_info
 PURPOSE
    Internal routine to destroy a chunk info node  (Wrapper for compatibility
    with H5TB_dfree)
 USAGE
    void H5D_free_chunk_info(chunk_info)
        void *chunk_info;    IN: Pointer to chunk info to destroy
 RETURNS
    No return value
 DESCRIPTION
    Releases all the memory for a chunk info node.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static void
H5D_free_chunk_info(void *_chunk_info)
{
    H5D_chunk_info_t *chunk_info=(H5D_chunk_info_t *)_chunk_info;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5D_free_chunk_info)

    assert(chunk_info);

    /* Close the chunk's file dataspace */
    (void)H5S_close(chunk_info->fspace);

    /* Close the chunk's memory dataspace, if it's not shared */
    if(!chunk_info->mspace_shared)
        (void)H5S_close(chunk_info->mspace);

    /* Free the actual chunk info */
    H5FL_FREE(H5D_chunk_info_t,chunk_info);

    FUNC_LEAVE_NOAPI_VOID
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
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_destroy_chunk_map(const fm_map *fm)
{
    herr_t	ret_value = SUCCEED;	/*return value		*/
     
    FUNC_ENTER_NOAPI_NOINIT(H5D_destroy_chunk_map)

    /* Free the chunk info tree */
    if(fm->fsel)
        H5TB_dfree(fm->fsel,H5D_free_chunk_info,NULL);

    /* Free the memory chunk dataspace template */
    if(fm->mchunk_tmpl)
        if(H5S_close(fm->mchunk_tmpl)<0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTRELEASE, FAIL, "can't release memory chunk dataspace template")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D_destroy_chunk_map() */


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
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t 
H5D_create_chunk_file_map_hyper(fm_map *fm)
{
    hssize_t    sel_points;                 /* Number of elements in file selection */
    hssize_t    sel_start[H5O_LAYOUT_NDIMS];   /* Offset of low bound of file selection */
    hssize_t    sel_end[H5O_LAYOUT_NDIMS];   /* Offset of high bound of file selection */
    hssize_t    start_coords[H5O_LAYOUT_NDIMS];   /* Starting coordinates of selection */
    hssize_t    coords[H5O_LAYOUT_NDIMS];   /* Current coordinates of chunk */
    hssize_t    end[H5O_LAYOUT_NDIMS];      /* Current coordinates of chunk */
    hsize_t     chunk_index;                /* Index of chunk */
    int         curr_dim;                   /* Current dimension to increment */
    unsigned    u;                          /* Local index variable */
    herr_t	ret_value = SUCCEED;        /* Return value */
    
    FUNC_ENTER_NOAPI_NOINIT(H5D_create_chunk_file_map_hyper)

    /* Sanity check */
    assert(fm->f_ndims>0);

    /* Get number of elements selected in file */
    if((sel_points=H5S_GET_SELECT_NPOINTS(fm->file_space))<0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTGET, FAIL, "can't get file selection # of elements")

    /* Get bounding box for selection (to reduce the number of chunks to iterate over) */
    if(H5S_SELECT_BOUNDS(fm->file_space, sel_start, sel_end)<0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTGET, FAIL, "can't get file selection bound info")

    /* Set initial chunk location & hyperslab size */
    for(u=0; u<fm->f_ndims; u++) {
        H5_CHECK_OVERFLOW(fm->layout->u.chunk.dim[u],hsize_t,hssize_t);
        start_coords[u]=(sel_start[u]/(hssize_t)fm->layout->u.chunk.dim[u])*(hssize_t)fm->layout->u.chunk.dim[u];
        coords[u]=start_coords[u];
        end[u]=(coords[u]+fm->chunk_dim[u])-1;
    } /* end for */

    /* Calculate the index of this chunk */
    if(H5V_chunk_index(fm->f_ndims,coords,fm->layout->u.chunk.dim,fm->down_chunks,&chunk_index)<0)
        HGOTO_ERROR (H5E_DATASPACE, H5E_BADRANGE, FAIL, "can't get chunk index")

    /* Iterate through each chunk in the dataset */
    while(sel_points) {
        /* Check for intersection of temporary chunk and file selection */
        /* (Casting away const OK - QAK) */
        if(H5S_hyper_intersect_block((H5S_t *)fm->file_space,coords,end)==TRUE) {
            H5S_t *tmp_fchunk;                  /* Temporary file dataspace */
            H5D_chunk_info_t *new_chunk_info;   /* chunk information to insert into tree */
            hssize_t    schunk_points;          /* Number of elements in chunk selection */

            /* Create "temporary" chunk for selection operations (copy file space) */
            if((tmp_fchunk = H5S_copy(fm->file_space,TRUE))==NULL)
                HGOTO_ERROR (H5E_DATASPACE, H5E_CANTCOPY, FAIL, "unable to copy memory space")

            /* Make certain selections are stored in span tree form (not "optimized hyperslab" or "all") */
            if(H5S_hyper_convert(tmp_fchunk)<0)
                HGOTO_ERROR (H5E_DATASPACE, H5E_CANTINIT, FAIL, "unable to convert selection to span trees")

            /* Normalize hyperslab selections by adjusting them by the offset */
            if(H5S_hyper_normalize_offset(tmp_fchunk)<0)
                HGOTO_ERROR (H5E_DATASET, H5E_BADSELECT, FAIL, "unable to normalize dataspace by offset")

            /* "AND" temporary chunk and current chunk */
            if(H5S_select_hyperslab(tmp_fchunk,H5S_SELECT_AND,coords,NULL,fm->chunk_dim,NULL)<0) {
                (void)H5S_close(tmp_fchunk);
                HGOTO_ERROR(H5E_DATASPACE, H5E_CANTSELECT, FAIL, "can't create chunk selection")
            } /* end if */
            
            /* Resize chunk's dataspace dimensions to size of chunk */
            if(H5S_set_extent_real(tmp_fchunk,fm->chunk_dim)<0) {
                (void)H5S_close(tmp_fchunk);
                HGOTO_ERROR(H5E_DATASPACE, H5E_CANTSELECT, FAIL, "can't adjust chunk dimensions")
            } /* end if */

            /* Move selection back to have correct offset in chunk */
            if(H5S_hyper_adjust(tmp_fchunk,coords)<0) {
                (void)H5S_close(tmp_fchunk);
                HGOTO_ERROR(H5E_DATASPACE, H5E_CANTSELECT, FAIL, "can't adjust chunk selection")
            } /* end if */

            /* Add temporary chunk to the list of chunks */

            /* Allocate the file & memory chunk information */
            if (NULL==(new_chunk_info = H5FL_MALLOC (H5D_chunk_info_t))) {
                (void)H5S_close(tmp_fchunk);
                HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate chunk info")
            } /* end if */

            /* Initialize the chunk information */

            /* Set the chunk index */
            new_chunk_info->index=chunk_index;

            /* Set the file chunk dataspace */
            new_chunk_info->fspace=tmp_fchunk;

            /* Set the memory chunk dataspace */
            new_chunk_info->mspace=NULL;
            new_chunk_info->mspace_shared=0;

            /* Copy the chunk's coordinates */
            HDmemcpy(new_chunk_info->coords,coords,fm->f_ndims*sizeof(new_chunk_info->coords[0]));
            new_chunk_info->coords[fm->f_ndims]=0;

            /* Insert the new chunk into the TBBT tree */
            if(H5TB_dins(fm->fsel,new_chunk_info,new_chunk_info)==NULL) {
                H5D_free_chunk_info(new_chunk_info);
                HGOTO_ERROR(H5E_DATASPACE,H5E_CANTINSERT,FAIL,"can't insert chunk into TBBT")
            } /* end if */

            /* Get number of elements selected in chunk */
            if((schunk_points=H5S_GET_SELECT_NPOINTS(tmp_fchunk))<0)
                HGOTO_ERROR(H5E_DATASPACE, H5E_CANTGET, FAIL, "can't get file selection # of elements")
            H5_ASSIGN_OVERFLOW(new_chunk_info->chunk_points,schunk_points,hssize_t,size_t);

            /* Decrement # of points left in file selection */
            sel_points-=schunk_points;

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
        coords[curr_dim]+=(hssize_t)fm->chunk_dim[curr_dim];
        end[curr_dim]+=(hssize_t)fm->chunk_dim[curr_dim];

        /* Bring chunk location back into bounds, if necessary */
        if(coords[curr_dim]>sel_end[curr_dim]) {
            do {
                /* Reset current dimension's location to 0 */
                coords[curr_dim]=start_coords[curr_dim]; /*lint !e771 The start_coords will always be initialized */

                /* Decrement current dimension */
                curr_dim--;

                /* Increment chunk location in current dimension */
                coords[curr_dim]+=(hssize_t)fm->chunk_dim[curr_dim];
                end[curr_dim]=(coords[curr_dim]+(hssize_t)fm->chunk_dim[curr_dim])-1;
            } while(coords[curr_dim]>sel_end[curr_dim]);

            /* Re-Calculate the index of this chunk */
            if(H5V_chunk_index(fm->f_ndims,coords,fm->layout->u.chunk.dim,fm->down_chunks,&chunk_index)<0)
                HGOTO_ERROR (H5E_DATASPACE, H5E_BADRANGE, FAIL, "can't get chunk index")
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
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t 
H5D_create_chunk_mem_map_hyper(const fm_map *fm)
{
    H5TB_NODE *curr_node;                   /* Current node in TBBT */
    hssize_t    file_sel_start[H5O_LAYOUT_NDIMS];   /* Offset of low bound of file selection */
    hssize_t    file_sel_end[H5O_LAYOUT_NDIMS];   /* Offset of high bound of file selection */
    hssize_t    mem_sel_start[H5O_LAYOUT_NDIMS];   /* Offset of low bound of file selection */
    hssize_t    mem_sel_end[H5O_LAYOUT_NDIMS];   /* Offset of high bound of file selection */
    hssize_t adjust[H5O_LAYOUT_NDIMS];      /* Adjustment to make to all file chunks */
    hssize_t chunk_adjust[H5O_LAYOUT_NDIMS];  /* Adjustment to make to a particular chunk */
    unsigned    u;                          /* Local index variable */
    herr_t	ret_value = SUCCEED;        /* Return value */
    
    FUNC_ENTER_NOAPI_NOINIT(H5D_create_chunk_mem_map_hyper)
#ifdef QAK
{
    hsize_t mem_dims[H5O_LAYOUT_NDIMS];   /* Dimensions of memory space */

    if(H5S_get_simple_extent_dims(fm->mem_space, mem_dims, NULL)<0)
        HGOTO_ERROR (H5E_DATASPACE, H5E_CANTGET, FAIL, "unable to get dimensionality")

    HDfprintf(stderr,"%s: mem_dims={",FUNC);
    for(u=0; u<fm->m_ndims; u++)
        HDfprintf(stderr,"%Hd%s",mem_dims[u],(u<(fm->m_ndims-1) ? ", " : "}\n"));
}
#endif /* QAK */

    /* Sanity check */
    assert(fm->f_ndims>0);

    /* Check for all I/O going to a single chunk */
    if(H5TB_count(fm->fsel)==1) {
        H5D_chunk_info_t *chunk_info;   /* Pointer to chunk information */

        /* Get the node */
        curr_node=H5TB_first(fm->fsel->root);

        /* Get pointer to chunk's information */
        chunk_info=curr_node->data;
        assert(chunk_info);

        /* Check if it's OK to share dataspace */
        if(fm->mem_space_copy) {
            /* Copy the memory dataspace & selection to be the chunk's dataspace & selection */
            if((chunk_info->mspace = H5S_copy(fm->mem_space,FALSE))==NULL)
                HGOTO_ERROR (H5E_DATASPACE, H5E_CANTCOPY, FAIL, "unable to copy memory space")
        } /* end if */
        else {
            /* Just point at the memory dataspace & selection */
            /* (Casting away const OK -QAK) */
            chunk_info->mspace=(H5S_t *)fm->mem_space;

            /* Indicate that the chunk's memory space is shared */
            chunk_info->mspace_shared=1;
        } /* end else */
    } /* end if */
    else {
        /* Get bounding box for file selection */
        if(H5S_SELECT_BOUNDS(fm->file_space, file_sel_start, file_sel_end)<0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTGET, FAIL, "can't get file selection bound info")

        /* Get bounding box for memory selection */
        if(H5S_SELECT_BOUNDS(fm->mem_space, mem_sel_start, mem_sel_end)<0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTGET, FAIL, "can't get file selection bound info")

        /* Calculate the adjustment for memory selection from file selection */
        assert(fm->m_ndims==fm->f_ndims);
        for(u=0; u<fm->f_ndims; u++)
            adjust[u]=file_sel_start[u]-mem_sel_start[u];
#ifdef QAK
    {
        int mpi_rank;
        MPI_Comm_rank(MPI_COMM_WORLD,&mpi_rank);
        if(mpi_rank==1) {
            HDfprintf(stderr,"%s: rank=%d - adjust={",FUNC,mpi_rank);
            for(u=0; u<fm->f_ndims; u++)
                HDfprintf(stderr,"%Hd%s",adjust[u],(u<(fm->f_ndims-1) ? ", " : "}\n"));
        } /* end if */
    }
#endif /* QAK */
#ifdef QAK
    HDfprintf(stderr,"%s: adjust={",FUNC);
    for(u=0; u<fm->f_ndims; u++)
        HDfprintf(stderr,"%Hd%s",adjust[u],(u<(fm->f_ndims-1) ? ", " : "}\n"));
#endif /* QAK */

        /* Iterate over each chunk in the chunk list */
        curr_node=H5TB_first(fm->fsel->root);
        while(curr_node) {
            H5D_chunk_info_t *chunk_info;   /* Pointer to chunk information */

            /* Get pointer to chunk's information */
            chunk_info=curr_node->data;
            assert(chunk_info);

            /* Copy the information */

            /* Copy the memory dataspace */
            if((chunk_info->mspace = H5S_copy(fm->mem_space,TRUE))==NULL)
                HGOTO_ERROR (H5E_DATASPACE, H5E_CANTCOPY, FAIL, "unable to copy memory space")

            /* Release the current selection */
            if(H5S_SELECT_RELEASE(chunk_info->mspace)<0)
                HGOTO_ERROR (H5E_DATASPACE, H5E_CANTRELEASE, FAIL, "unable to release selection")

            /* Copy the file chunk's selection */
            if(H5S_select_copy(chunk_info->mspace,chunk_info->fspace,FALSE)<0)
                HGOTO_ERROR (H5E_DATASPACE, H5E_CANTCOPY, FAIL, "unable to copy selection")

            /* Compensate for the chunk offset */
            for(u=0; u<fm->f_ndims; u++)
                chunk_adjust[u]=adjust[u]-chunk_info->coords[u]; /*lint !e771 The adjust array will always be initialized */
#ifdef QAK
    {
        int mpi_rank;
        MPI_Comm_rank(MPI_COMM_WORLD,&mpi_rank);
        if(mpi_rank==1) {
            HDfprintf(stderr,"%s: rank=%d - Before adjusting memory selection\n",FUNC,mpi_rank);
            HDfprintf(stderr,"%s: rank=%d - chunk_adjust={",FUNC,mpi_rank);
            for(u=0; u<fm->f_ndims; u++)
                HDfprintf(stderr,"%Hd%s",chunk_adjust[u],(u<(fm->f_ndims-1) ? ", " : "}\n"));
        } /* end if */
    }
#endif /* QAK */
#ifdef QAK
    HDfprintf(stderr,"%s: Before adjusting memory selection\n",FUNC);
    HDfprintf(stderr,"%s: chunk_adjust={",FUNC);
    for(u=0; u<fm->f_ndims; u++)
        HDfprintf(stderr,"%Hd%s",chunk_adjust[u],(u<(fm->f_ndims-1) ? ", " : "}\n"));
#endif /* QAK */
            /* Adjust the selection */
            if(H5S_hyper_adjust(chunk_info->mspace,chunk_adjust)<0) /*lint !e772 The chunk_adjust array will always be initialized */
                HGOTO_ERROR(H5E_DATASPACE, H5E_CANTSELECT, FAIL, "can't adjust chunk selection")
#ifdef QAK
    {
        int mpi_rank;
        MPI_Comm_rank(MPI_COMM_WORLD,&mpi_rank);
        if(mpi_rank==1)
            HDfprintf(stderr,"%s: rank=%d - After adjusting memory selection\n",FUNC,mpi_rank);
    }
#endif /* QAK */
#ifdef QAK
    HDfprintf(stderr,"%s: After adjusting memory selection\n",FUNC);

    {
        hsize_t mem_dims[H5O_LAYOUT_NDIMS];   /* Dimensions of memory space */

        if(H5S_get_simple_extent_dims(chunk_info->mspace, mem_dims, NULL)<0)
            HGOTO_ERROR (H5E_DATASPACE, H5E_CANTGET, FAIL, "unable to get dimensionality")

        HDfprintf(stderr,"%s: mem_dims={",FUNC);
        for(u=0; u<fm->m_ndims; u++)
            HDfprintf(stderr,"%Hd%s",mem_dims[u],(u<(fm->m_ndims-1) ? ", " : "}\n"));
    }
#endif /* QAK */
            /* Get the next chunk node in the TBBT */
            curr_node=H5TB_next(curr_node);
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
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t 
H5D_chunk_file_cb(void UNUSED *elem, hid_t UNUSED type_id, hsize_t ndims, hssize_t *coords, void *_fm)
{
    fm_map      *fm = (fm_map*)_fm;             /* File<->memory chunk mapping info */
    H5D_chunk_info_t *chunk_info;               /* Chunk information for current chunk */
    hssize_t    coords_in_chunk[H5O_LAYOUT_NDIMS];        /* Coordinates of element in chunk */
    hsize_t     chunk_index;                    /* Chunk index */
    unsigned    u;                              /* Local index variable */
    herr_t	ret_value = SUCCEED;            /* Return value		*/
    
    FUNC_ENTER_NOAPI_NOINIT(H5D_chunk_file_cb)

    /* Calculate the index of this chunk */
    if(H5V_chunk_index((unsigned)ndims,coords,fm->layout->u.chunk.dim,fm->down_chunks,&chunk_index)<0)
        HGOTO_ERROR (H5E_DATASPACE, H5E_BADRANGE, FAIL, "can't get chunk index")

    /* Find correct chunk in file & memory TBBTs */
    if(chunk_index==fm->last_index) {
        /* If the chunk index is the same as the last chunk index we used,
         * get the cached info to operate on.
         */
        chunk_info=fm->last_chunk_info;
    } /* end if */
    else {
        H5TB_NODE *chunk_node;       /* TBBT node holding chunk information */

        /* If the chunk index is not the same as the last chunk index we used,
         * find the chunk in the tree.
         */
        /* Get the chunk node from the TBBT */
        if((chunk_node=H5TB_dfind(fm->fsel,&chunk_index,NULL))==NULL) {
            H5D_chunk_info_t *new_chunk_info;   /* Chunk information to insert into tree */
            H5S_t *fspace;                      /* Memory chunk's dataspace */

            /* Allocate the file & memory chunk information */
            if (NULL==(new_chunk_info = H5FL_MALLOC (H5D_chunk_info_t)))
                HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate chunk info")

            /* Initialize the chunk information */

            /* Set the chunk index */
            new_chunk_info->index=chunk_index;

            /* Create a dataspace for the chunk */
            if((fspace = H5S_create_simple(fm->f_ndims,fm->chunk_dim,NULL))==NULL) {
                H5FL_FREE(H5D_chunk_info_t,new_chunk_info);
                HGOTO_ERROR (H5E_DATASPACE, H5E_CANTCREATE, FAIL, "unable to create dataspace for chunk")
            } /* end if */

            /* De-select the chunk space */
            if(H5S_select_none(fspace)<0) {
                (void)H5S_close(fspace);
                H5FL_FREE(H5D_chunk_info_t,new_chunk_info);
                HGOTO_ERROR (H5E_DATASPACE, H5E_CANTINIT, FAIL, "unable to de-select dataspace")
            } /* end if */

            /* Set the file chunk dataspace */
            new_chunk_info->fspace=fspace;

            /* Set the memory chunk dataspace */
            new_chunk_info->mspace=NULL;
            new_chunk_info->mspace_shared=0;

            /* Set the number of selected elements in chunk to zero */
            new_chunk_info->chunk_points=0;

            /* Compute the chunk's coordinates */
            for(u=0; u<fm->f_ndims; u++) {
                H5_CHECK_OVERFLOW(fm->layout->u.chunk.dim[u],hsize_t,hssize_t);
                new_chunk_info->coords[u]=(coords[u]/(hssize_t)fm->layout->u.chunk.dim[u])*(hssize_t)fm->layout->u.chunk.dim[u];
            } /* end for */
            new_chunk_info->coords[fm->f_ndims]=0;

            /* Insert the new chunk into the TBBT tree */
            if(H5TB_dins(fm->fsel,new_chunk_info,new_chunk_info)==NULL) {
                H5D_free_chunk_info(new_chunk_info);
                HGOTO_ERROR(H5E_DATASPACE,H5E_CANTINSERT,FAIL,"can't insert chunk into TBBT")
            } /* end if */

            /* Save the chunk info pointer */
            chunk_info=new_chunk_info;
        } /* end if */
        else
            /* Get the information from the node */
            chunk_info=(H5D_chunk_info_t *)(chunk_node->data);

        /* Update the "last chunk seen" information */
        fm->last_index=chunk_index;
        fm->last_chunk_info=chunk_info;
    } /* end else */

    /* Get the coordinates of the element in the chunk */
    for(u=0; u<fm->f_ndims; u++)
        coords_in_chunk[u]=coords[u]%fm->layout->u.chunk.dim[u];

    /* Add point to file selection for chunk */
    if(H5S_select_elements(chunk_info->fspace,H5S_SELECT_APPEND,1,(const hssize_t **)coords_in_chunk)<0)
        HGOTO_ERROR (H5E_DATASPACE, H5E_CANTSELECT, FAIL, "unable to select element")

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
 * Modifications:
 *      QAK - 2003/04/17
 *      Hacked on it a lot. :-)
 *
 *-------------------------------------------------------------------------
 */
/* ARGSUSED */
static herr_t 
H5D_chunk_mem_cb(void UNUSED *elem, hid_t UNUSED type_id, hsize_t ndims, hssize_t *coords, void *_fm)
{
    fm_map      *fm = (fm_map*)_fm;             /* File<->memory chunk mapping info */
    H5D_chunk_info_t *chunk_info;               /* Chunk information for current chunk */
    hssize_t    coords_in_mem[H5O_LAYOUT_NDIMS];        /* Coordinates of element in memory */
    hsize_t     chunk_index;                    /* Chunk index */
    herr_t	ret_value = SUCCEED;            /* Return value		*/
    
    FUNC_ENTER_NOAPI_NOINIT(H5D_chunk_mem_cb)

    /* Calculate the index of this chunk */
    if(H5V_chunk_index((unsigned)ndims,coords,fm->layout->u.chunk.dim,fm->down_chunks,&chunk_index)<0)
        HGOTO_ERROR (H5E_DATASPACE, H5E_BADRANGE, FAIL, "can't get chunk index")

    /* Find correct chunk in file & memory TBBTs */
    if(chunk_index==fm->last_index) {
        /* If the chunk index is the same as the last chunk index we used,
         * get the cached spaces to operate on.
         */
        chunk_info=fm->last_chunk_info;
    } /* end if */
    else {
        H5TB_NODE *chunk_node;       /* TBBT node holding chunk information */

        /* If the chunk index is not the same as the last chunk index we used,
         * find the chunk in the tree.
         */
        /* Get the chunk node from the TBBT */
        if((chunk_node=H5TB_dfind(fm->fsel,&chunk_index,NULL))==NULL)
            HGOTO_ERROR(H5E_DATASPACE,H5E_NOTFOUND,FAIL,"can't locate chunk in TBBT")

        /* Get the chunk info pointer */
        chunk_info=(H5D_chunk_info_t *)(chunk_node->data);

        /* Check if the chunk already has a memory space */
        if(chunk_info->mspace==NULL) {
            /* Copy the template memory chunk dataspace */
            if((chunk_info->mspace = H5S_copy(fm->mchunk_tmpl,FALSE))==NULL)
                HGOTO_ERROR (H5E_DATASPACE, H5E_CANTCOPY, FAIL, "unable to copy file space")
        } /* end else */

        /* Update the "last chunk seen" information */
        fm->last_index=chunk_index;
        fm->last_chunk_info=chunk_info;
    } /* end else */

    /* Get coordinates of selection iterator for memory */
    if(H5S_SELECT_ITER_COORDS(&fm->mem_iter,coords_in_mem)<0)
        HGOTO_ERROR (H5E_DATASPACE, H5E_CANTGET, FAIL, "unable to get iterator coordinates")

    /* Add point to memory selection for chunk */
    if(fm->msel_type==H5S_SEL_POINTS) {
        if(H5S_select_elements(chunk_info->mspace,H5S_SELECT_APPEND,1,(const hssize_t **)coords_in_mem)<0)
            HGOTO_ERROR (H5E_DATASPACE, H5E_CANTSELECT, FAIL, "unable to select element")
    } /* end if */
    else {
        if(H5S_hyper_add_span_element(chunk_info->mspace, fm->m_ndims, coords_in_mem)<0)
            HGOTO_ERROR (H5E_DATASPACE, H5E_CANTSELECT, FAIL, "unable to select element")
    } /* end else */

    /* Move memory selection iterator to next element in selection */
    if(H5S_SELECT_ITER_NEXT(&fm->mem_iter,1)<0)
        HGOTO_ERROR (H5E_DATASPACE, H5E_CANTNEXT, FAIL, "unable to move to next iterator location")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D_chunk_mem_cb() */
