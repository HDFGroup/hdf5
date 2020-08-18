/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * This file contains public declarations for the H5D module.
 */
#ifndef _H5Dpublic_H
#define _H5Dpublic_H

/* System headers needed by this file */

/* Public headers needed by this file */
#include "H5public.h"
#include "H5Ipublic.h"

/*****************/
/* Public Macros */
/*****************/

/* Macros used to "unset" chunk cache configuration parameters */
#define H5D_CHUNK_CACHE_NSLOTS_DEFAULT      ((size_t) -1)
#define H5D_CHUNK_CACHE_NBYTES_DEFAULT      ((size_t) -1)
#define H5D_CHUNK_CACHE_W0_DEFAULT          (-1.0f)

/** Bit flags for the H5Pset_chunk_opts() and H5Pget_chunk_opts() */
#define H5D_CHUNK_DONT_FILTER_PARTIAL_CHUNKS      (0x0002u)

/*******************/
/* Public Typedefs */
/*******************/

/** Values for the H5D_LAYOUT property */
typedef enum H5D_layout_t {
    H5D_LAYOUT_ERROR	= -1,

    H5D_COMPACT		= 0,	/**< raw data is very small		     */
    H5D_CONTIGUOUS	= 1,	/**< the default				     */
    H5D_CHUNKED		= 2,	/**< slow and fancy			     */
    H5D_VIRTUAL         = 3,    /**< actual data is stored in other datasets     */
    H5D_NLAYOUTS	= 4	/*this one must be last!		     */
} H5D_layout_t;

/** Types of chunk index data structures */
typedef enum H5D_chunk_index_t {
    H5D_CHUNK_IDX_BTREE	= 0,    /**< v1 B-tree index (default)                */
    H5D_CHUNK_IDX_SINGLE = 1,   /**< Single Chunk index (cur dims[]=max dims[]=chunk dims[]; filtered & non-filtered) */
    H5D_CHUNK_IDX_NONE = 2,     /**< Implicit: No Index (H5D_ALLOC_TIME_EARLY, non-filtered, fixed dims) */
    H5D_CHUNK_IDX_FARRAY = 3,   /**< Fixed array (for 0 unlimited dims)       */
    H5D_CHUNK_IDX_EARRAY = 4,   /**< Extensible array (for 1 unlimited dim)   */
    H5D_CHUNK_IDX_BT2 = 5,      /**< v2 B-tree index (for >1 unlimited dims)  */
    H5D_CHUNK_IDX_NTYPES        /**< This one must be last!                   */
} H5D_chunk_index_t;

/** Values for the space allocation time property */
typedef enum H5D_alloc_time_t {
    H5D_ALLOC_TIME_ERROR	= -1,
    H5D_ALLOC_TIME_DEFAULT  	= 0,
    H5D_ALLOC_TIME_EARLY	= 1,
    H5D_ALLOC_TIME_LATE		= 2,
    H5D_ALLOC_TIME_INCR		= 3
} H5D_alloc_time_t;

/** Values for the status of space allocation */
typedef enum H5D_space_status_t {
    H5D_SPACE_STATUS_ERROR		= -1,
    H5D_SPACE_STATUS_NOT_ALLOCATED	= 0,
    H5D_SPACE_STATUS_PART_ALLOCATED	= 1,
    H5D_SPACE_STATUS_ALLOCATED		= 2
} H5D_space_status_t;

/** Values for time of writing fill value property */
typedef enum H5D_fill_time_t {
    H5D_FILL_TIME_ERROR	= -1,
    H5D_FILL_TIME_ALLOC = 0,
    H5D_FILL_TIME_NEVER	= 1,
    H5D_FILL_TIME_IFSET	= 2
} H5D_fill_time_t;

/** Values for fill value status */
typedef enum H5D_fill_value_t {
    H5D_FILL_VALUE_ERROR        =-1,
    H5D_FILL_VALUE_UNDEFINED    =0,
    H5D_FILL_VALUE_DEFAULT      =1,
    H5D_FILL_VALUE_USER_DEFINED =2
} H5D_fill_value_t;

/** Values for VDS bounds option */
typedef enum H5D_vds_view_t {
    H5D_VDS_ERROR               = -1,
    H5D_VDS_FIRST_MISSING       = 0,
    H5D_VDS_LAST_AVAILABLE      = 1
} H5D_vds_view_t;

/** Callback for H5Pset_append_flush() in a dataset access property list */
typedef herr_t (*H5D_append_cb_t)(hid_t dataset_id, hsize_t *cur_dims, void *op_data);

/** Define the operator function pointer for H5Diterate() */
typedef herr_t (*H5D_operator_t)(void *elem, hid_t type_id, unsigned ndim,
				 const hsize_t *point, void *operator_data);

/** Define the operator function pointer for H5Dscatter() */
typedef herr_t (*H5D_scatter_func_t)(const void **src_buf/*out*/,
                                     size_t *src_buf_bytes_used/*out*/,
                                     void *op_data);

/** Define the operator function pointer for H5Dgather() */
typedef herr_t (*H5D_gather_func_t)(const void *dst_buf,
                                    size_t dst_buf_bytes_used, void *op_data);


/********************/
/* Public Variables */
/********************/

/*********************/
/* Public Prototypes */
/*********************/
#ifdef __cplusplus
extern "C" {
#endif

/* --------------------------------------------------------------------------*/
/**\ingroup H5D
 *
 * \brief Creates a new dataset and links it into the file
 *
 * \fgdta_loc_id
 * \param[in] name      Name of the dataset to open
 * \type_id
 * \space_id
 * \lcpl_id
 * \dcpl_id
 * \dapl_id
 *
 * \return \hid_t{dataset}
 *
 * \details H5Dcreate2() creates a new dataset named \p name at the
 * location specified by \p loc_id, and associates constant and initial
 * persistent properties with that dataset, including \p dtype_id, the
 * datatype of each data element as stored in the file; \p space_id, the
 * dataspace of the dataset; and other initial properties as defined in
 * the dataset creation property and access property lists, \p dcpl_id and
 * \p dapl_id, respectively. Once created, the dataset is opened for access.
 *
 * \p loc_id may be a file, group, dataset, named datatype, or attribute.
 * If an attribute, dataset, or named datatype is specified for \p loc_id
 * then the dataset will be created at the location where the attribute,
 * dataset, or named datatype is attached. \p name may be either an absolute
 * path in the file or a relative path from \p loc_id naming the dataset.
 *
 * If \p dtype_id is either a fixed-length or variable-length string, it is
 * important to set the string length when defining the datatype. String
 * datatypes are derived from #H5T_C_S1 (or #H5T_FORTRAN_S1 for Fortran
 * codes), which defaults to 1 character in size.
 *
 * If \p dtype_id is a committed datatype, and if the file location
 * associated with the committed datatype is different from the file location
 * where the dataset will be created, the datatype is copied and converted
 * to a transient type.
 *
 * The link creation property list, \p lcpl_id, governs creation of the
 * link(s) by which the new dataset is accessed and the creation of any
 * intermediate groups that may be missing.
 *
 * The datatype and dataspace properties and the dataset creation and
 * access property lists are attached to the dataset, so the caller may
 * derive new datatypes, dataspaces, and creation and access properties from
 * the old ones and reuse them in calls to create additional datasets.
 * Once created, the dataset can be read from or written to. Reading data
 * from a datatset that was not previously written, the HDF5 library will
 * return default or user-defined fill values.
 *
 * To conserve and release resources, the dataset should be closed when
 * access is no longer required.
 *
 * \since 1.8.0
 *
 * \see H5Dopen2(), H5Dclose(), H5Tset_size()
 *
 *-------------------------------------------------------------------------
 */
H5_DLL hid_t H5Dcreate2(hid_t loc_id, const char *name, hid_t type_id,
    hid_t space_id, hid_t lcpl_id, hid_t dcpl_id, hid_t dapl_id);
/* --------------------------------------------------------------------------*/
/**\ingroup H5D
 *
 * \brief Creates a new dataset and links it into the file
 *
 * \fgdta_loc_id
 * \type_id
 * \space_id
 * \dcpl_id
 * \dapl_id
 *
 * \return \hid_t{dataset}
 *
 * H5Dcreate_anon() creates a dataset in the file specified by \p loc_id.
 *
 * \p loc_id may be a file, group, dataset, named datatype or attribute.
 * If an attribute, dataset, or named datatype is specified for \p loc_id
 * then the dataset will be created at the location where the attribute,
 * dataset, or named datatype is attached.
 *
 * The dataset’s datatype and dataspace are specified by \p type_id and
 * \p space_id, respectively. These are the datatype and dataspace of the
 * dataset as it will exist in the file, which may differ from the datatype
 * and dataspace in application memory.
 *
 * Dataset creation properties are specified in the dataset creation property
 * list \p dcpl_id. Dataset access properties are specified in the dataset
 * access property list \p dapl_id.
 *
 * H5Dcreate_anon() returns a new dataset identifier. Using this identifier,
 * the new dataset must be linked into the HDF5 file structure with H5Olink()
 * or it will be deleted from the file when the file is closed.
 *
 * See H5Dcreate() for further details and considerations on the use of
 * H5Dcreate and H5Dcreate_anon.
 *
 * The differences between this function and H5Dcreate() are as follows:
 *
 * - H5Dcreate_anon() explicitly includes a dataset access property
 * list. H5Dcreate() always uses default dataset access properties.
 * - H5Dcreate_anon() neither provides the new dataset’s name nor links it
 * into the HDF5 file structure; those actions must be performed separately
 * through a call to H5Olink(), which offers greater control over linking.
 *
 * A dataset created with this function should be closed with H5Dclose() when
 * the dataset is no longer needed so that resource leaks will not develop.
 *
 * \since 1.8.0
 *
 * \see H5Olink(), H5Dcreate(), Using Identifiers
 *
 *-------------------------------------------------------------------------
 */
H5_DLL hid_t H5Dcreate_anon(hid_t file_id, hid_t type_id, hid_t space_id,
    hid_t plist_id, hid_t dapl_id);
/* --------------------------------------------------------------------------*/
/**\ingroup H5D
 *
 * \brief Creates a new dataset and links it into the file
 *
 * \fgdta_loc_id
 * \param[in] name      Name of the dataset to open
 * \param[in] dapl_id   Dataset access property list
 *
 * \return \hid_t{dataset}
 *
 * \details H5Dopen2() opens the existing dataset specified by a
 * location identifier and name, \p loc_id and \p name, respectively. \p
 * loc_id may be a file, group, dataset, named datatype, or attribute.
 * If an attribute, dataset, or named datatype is specified for \p loc_id
 * then the dataset will be opened at the location where the attribute,
 * dataset, or named datatype is attached.
 *
 * The dataset access property list, \p dapl_id, provides information
 * regarding access to the dataset.
 *
 * To conserve and release resources, the dataset should be closed when
 * access is no longer required.
 *
 * \since 1.8.0
 *
 * \see H5Dcreate2(), H5Dclose()
 *
 *-------------------------------------------------------------------------
 */
H5_DLL hid_t H5Dopen2(hid_t file_id, const char *name, hid_t dapl_id);
/*-------------------------------------------------------------------------*/
/**\ingroup H5D
 *
 * \brief Closes the specified dataset
 *
 * \dset_id
 *
 * \return \herr_t
 *
 * \details H5Dclose() ends access to a dataset specified by \p dset_id
 * and releases resources used by it.
 *
 * \attention Further use of a released dataset identifier is illegal; a
 *            function using such an identifier will generate an error.
 *
 * \since 1.0.0
 *
 * \see H5Dcreate2(), H5Dopen2()
 *-------------------------------------------------------------------------
 */
H5_DLL herr_t H5Dclose(hid_t dset_id);
/* --------------------------------------------------------------------------*/
/**\ingroup H5D
 *
 * \brief Returns an identifier for a copy of the dataspace for a dataset
 *
 * \dset_id
 *
 * \return \hid_t{dataspace}
 *
 * \details H5Dget_space() makes a copy of the dataspace of the dataset
 * specified by \p dataset_id. The function returns an identifier for the
 * new copy of the dataspace.

 * A dataspace identifier returned from this function should be released
 * with H5Sclose() when the identifier is no longer needed so that resource
 * leaks will not occur.
 *
 * \see H5Sclose()
 *
 *-------------------------------------------------------------------------
 */
H5_DLL hid_t H5Dget_space(hid_t dset_id);
H5_DLL herr_t H5Dget_space_status(hid_t dset_id, H5D_space_status_t *allocation);
/* --------------------------------------------------------------------------*/
/**\ingroup H5D
 *
 * \brief Returns an identifier for a copy of the datatype for a dataset
 *
 * \dset_id
 *
 * \return \hid_t{datatype}
 *
 * \details H5Dget_type() returns an identifier for a copy of the datatype
 * for a dataset.
 *
 * If a dataset has a named datatype, then an identifier to the opened
 * datatype is returned. Otherwise, the returned datatype is read-only. If
 * atomization of the datatype fails, then the datatype is closed.
 *
 * A datatype identifier returned from this function should be released
 * with H5Tclose() when the identifier is no longer needed so that resource
 * leaks will not occur.
 *
 * \note Datatype Identifiers
 *
 * \note Please note that a datatype is actually an object identifier or
 * handle returned from opening the datatype. It is not persistent and its
 * value can be different from one HDF5 session to the next.
 *
 * \note H5T_EQUAL can be used to compare datatypes.
 *
 * \note HDF5 High Level APIs that may also be of interest are:
 *
 * \note H5LT_DTYPE_TO_TEXT creates a text description of a datatype.  \note
 * H5LT_TEXT_TO_DTYPE creates an HDF5 datatype given a text description.
 *
 * \see H5Tclose()
 *
 *-------------------------------------------------------------------------
 */
H5_DLL hid_t H5Dget_type(hid_t dset_id);
/* --------------------------------------------------------------------------*/
/**\ingroup H5D
 *
 * \brief Returns an identifier for a copy of the dataset creation
 *  property list for a dataset
 *
 * \dset_id
 *
 * \return \hid_t{dataset creation property list}
 *
 * \details H5Dget_create_plist() returns an identifier for a copy of
 * the dataset creation property list associated with the specified dataset
 *
 * The creation property list identifier should be released with H5Pclose().
 *
 *-------------------------------------------------------------------------
 */
H5_DLL hid_t H5Dget_create_plist(hid_t dset_id);
H5_DLL hid_t H5Dget_access_plist(hid_t dset_id);
H5_DLL hsize_t H5Dget_storage_size(hid_t dset_id);
H5_DLL herr_t H5Dget_chunk_storage_size(hid_t dset_id, const hsize_t *offset, hsize_t *chunk_bytes);
H5_DLL herr_t H5Dget_num_chunks(hid_t dset_id, hid_t fspace_id, hsize_t *nchunks);
H5_DLL herr_t H5Dget_chunk_info_by_coord(hid_t dset_id, const hsize_t *coord, unsigned *filter_mask, haddr_t *addr, hsize_t *size);
H5_DLL herr_t H5Dget_chunk_info(hid_t dset_id, hid_t fspace_id, hsize_t chk_idx, hsize_t *coord, unsigned *filter_mask, haddr_t *addr, hsize_t *size);
H5_DLL haddr_t H5Dget_offset(hid_t dset_id);
H5_DLL herr_t H5Dread(hid_t dset_id, hid_t mem_type_id, hid_t mem_space_id,
			hid_t file_space_id, hid_t plist_id, void *buf/*out*/);
H5_DLL herr_t H5Dwrite(hid_t dset_id, hid_t mem_type_id, hid_t mem_space_id,
			 hid_t file_space_id, hid_t plist_id, const void *buf);
H5_DLL herr_t H5Dwrite_chunk(hid_t dset_id, hid_t dxpl_id, uint32_t filters,
            const hsize_t *offset, size_t data_size, const void *buf);
H5_DLL herr_t H5Dread_chunk(hid_t dset_id, hid_t dxpl_id,
            const hsize_t *offset, uint32_t *filters, void *buf);
H5_DLL herr_t H5Diterate(void *buf, hid_t type_id, hid_t space_id,
            H5D_operator_t op, void *operator_data);
H5_DLL herr_t H5Dvlen_get_buf_size(hid_t dataset_id, hid_t type_id, hid_t space_id, hsize_t *size);
H5_DLL herr_t H5Dfill(const void *fill, hid_t fill_type, void *buf,
        hid_t buf_type, hid_t space);
/* --------------------------------------------------------------------------*/
/**\ingroup H5D
 *
 * \brief Changes the sizes of a dataset’s dimensions
 *
 * \dset_id
 * \param[in] size[]   Array containing the new magnitude of each dimension
 *                     of the dataset
 *
 * \return \herr_t
 *
 * \details H5Dset_extent() sets the current dimensions of the chunked
 * dataset \p dset_id to the sizes specified in size.
 *
 * \p size is a 1-dimensional array with n elements, where n is the rank of
 * the dataset’s current dataspace.
 *
 * This function can be applied to the following datasets:
 * - A chunked dataset with unlimited dimensions
 * - A chunked dataset with fixed dimensions if the new dimension sizes
 * are less than the maximum sizes set with maxdims (see H5Screate_simple())
 * - An external dataset with unlimited dimensions
 * - An external dataset with fixed dimensions if the new dimension
 * sizes are less than the maximum sizes set with \p maxdims
 *
 * Note that external datasets are always contiguous and can be extended
 * only along the first dimension.
 *
 * Space on disk is immediately allocated for the new dataset extent if
 * the dataset’s space allocation time is set to #H5D_ALLOC_TIME_EARLY.
 *
 * Fill values will be written to the dataset in either of the following
 * situations, but not otherwise:
 *
 * - If the dataset’s fill time is set to #H5D_FILL_TIME_IFSET and a fill
 * value is defined (see H5Pset_fill_time() and H5Pset_fill_value())
 * - If the dataset’s fill time is set to #H5D_FILL_TIME_ALLOC
 * (see H5Pset_alloc_time())
 *
 * \note
 * + If the sizes specified in size are smaller than
 * the dataset’s current dimension sizes, H5Dset_extent() will reduce
 * the dataset’s dimension sizes to the specified values. It is the user
 * application’s responsibility to ensure that valuable data is not lost
 * as H5Dset_extent() does not check.
 * + Except for external datasets, H5Dset_extent() is for use with chunked
 * datasets only, not contiguous datasets.
 * + A call to H5Dset_extent affects the dataspace of a dataset. If a
 * dataspace handle was opened for a dataset prior to a call to H5Dset_extent()
 * then that dataspace handle will no longer reflect the correct dataspace
 * extent of the dataset. H5Dset_space() must be called (after closing the
 * previous handle) to obtain the current dataspace extent.
 *
 * \since 1.8.0
 *
 *-------------------------------------------------------------------------
 */
H5_DLL herr_t H5Dset_extent(hid_t dset_id, const hsize_t size[]);
H5_DLL herr_t H5Dflush(hid_t dset_id);
H5_DLL herr_t H5Drefresh(hid_t dset_id);
H5_DLL herr_t H5Dscatter(H5D_scatter_func_t op, void *op_data, hid_t type_id,
    hid_t dst_space_id, void *dst_buf);
H5_DLL herr_t H5Dgather(hid_t src_space_id, const void *src_buf, hid_t type_id,
    size_t dst_buf_size, void *dst_buf, H5D_gather_func_t op, void *op_data);
H5_DLL herr_t H5Ddebug(hid_t dset_id);

/* Internal API routines */
H5_DLL herr_t H5Dformat_convert(hid_t dset_id);
H5_DLL herr_t H5Dget_chunk_index_type(hid_t did, H5D_chunk_index_t *idx_type);

/* Symbols defined for compatibility with previous versions of the HDF5 API.
 *
 * Use of these symbols is deprecated.
 */
#ifndef H5_NO_DEPRECATED_SYMBOLS

/* Macros */
#define H5D_CHUNK_BTREE H5D_CHUNK_IDX_BTREE

/* Formerly used to support the H5DOread/write_chunk() API calls.
 * These symbols are no longer used in the library.
 */
/* Property names for H5DOwrite_chunk */
#define H5D_XFER_DIRECT_CHUNK_WRITE_FLAG_NAME       "direct_chunk_flag"
#define H5D_XFER_DIRECT_CHUNK_WRITE_FILTERS_NAME    "direct_chunk_filters"
#define H5D_XFER_DIRECT_CHUNK_WRITE_OFFSET_NAME     "direct_chunk_offset"
#define H5D_XFER_DIRECT_CHUNK_WRITE_DATASIZE_NAME   "direct_chunk_datasize"
/* Property names for H5DOread_chunk */
#define H5D_XFER_DIRECT_CHUNK_READ_FLAG_NAME        "direct_chunk_read_flag"
#define H5D_XFER_DIRECT_CHUNK_READ_OFFSET_NAME      "direct_chunk_read_offset"
#define H5D_XFER_DIRECT_CHUNK_READ_FILTERS_NAME     "direct_chunk_read_filters"

/* Typedefs */


/* Function prototypes */
H5_DLL hid_t H5Dcreate1(hid_t file_id, const char *name, hid_t type_id,
    hid_t space_id, hid_t dcpl_id);
H5_DLL hid_t H5Dopen1(hid_t file_id, const char *name);
H5_DLL herr_t H5Dextend(hid_t dset_id, const hsize_t size[]);
H5_DLL herr_t H5Dvlen_reclaim(hid_t type_id, hid_t space_id, hid_t plist_id, void *buf);

#endif /* H5_NO_DEPRECATED_SYMBOLS */

#ifdef __cplusplus
}
#endif
#endif /* _H5Dpublic_H */
