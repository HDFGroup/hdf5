/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * This file contains function prototypes for each exported function in the
 * H5P module.
 */
#ifndef H5Ppublic_H
#define H5Ppublic_H

/* System headers needed by this file */

/* Public headers needed by this file */
#include "H5public.h"
#include "H5ACpublic.h"
#include "H5Dpublic.h"
#include "H5Fpublic.h"
#include "H5FDpublic.h"
#include "H5Ipublic.h"
#include "H5Lpublic.h"
#include "H5Opublic.h"
#include "H5MMpublic.h"
#include "H5Tpublic.h"
#include "H5Zpublic.h"

/*****************/
/* Public Macros */
/*****************/

/* When this header is included from a private HDF5 header, don't make calls to H5open() */
#undef H5OPEN
#ifndef H5private_H
#define H5OPEN H5open(),
#else /* H5private_H */
#define H5OPEN
#endif /* H5private_H */

/*
 * The library's property list classes
 */

#define H5P_ROOT             (H5OPEN H5P_CLS_ROOT_ID_g)
#define H5P_OBJECT_CREATE    (H5OPEN H5P_CLS_OBJECT_CREATE_ID_g)
#define H5P_FILE_CREATE      (H5OPEN H5P_CLS_FILE_CREATE_ID_g)
#define H5P_FILE_ACCESS      (H5OPEN H5P_CLS_FILE_ACCESS_ID_g)
#define H5P_DATASET_CREATE   (H5OPEN H5P_CLS_DATASET_CREATE_ID_g)
#define H5P_DATASET_ACCESS   (H5OPEN H5P_CLS_DATASET_ACCESS_ID_g)
#define H5P_DATASET_XFER     (H5OPEN H5P_CLS_DATASET_XFER_ID_g)
#define H5P_FILE_MOUNT       (H5OPEN H5P_CLS_FILE_MOUNT_ID_g)
#define H5P_GROUP_CREATE     (H5OPEN H5P_CLS_GROUP_CREATE_ID_g)
#define H5P_GROUP_ACCESS     (H5OPEN H5P_CLS_GROUP_ACCESS_ID_g)
#define H5P_DATATYPE_CREATE  (H5OPEN H5P_CLS_DATATYPE_CREATE_ID_g)
#define H5P_DATATYPE_ACCESS  (H5OPEN H5P_CLS_DATATYPE_ACCESS_ID_g)
#define H5P_MAP_CREATE       (H5OPEN H5P_CLS_MAP_CREATE_ID_g)
#define H5P_MAP_ACCESS       (H5OPEN H5P_CLS_MAP_ACCESS_ID_g)
#define H5P_STRING_CREATE    (H5OPEN H5P_CLS_STRING_CREATE_ID_g)
#define H5P_ATTRIBUTE_CREATE (H5OPEN H5P_CLS_ATTRIBUTE_CREATE_ID_g)
#define H5P_ATTRIBUTE_ACCESS (H5OPEN H5P_CLS_ATTRIBUTE_ACCESS_ID_g)
#define H5P_OBJECT_COPY      (H5OPEN H5P_CLS_OBJECT_COPY_ID_g)
#define H5P_LINK_CREATE      (H5OPEN H5P_CLS_LINK_CREATE_ID_g)
#define H5P_LINK_ACCESS      (H5OPEN H5P_CLS_LINK_ACCESS_ID_g)
#define H5P_VOL_INITIALIZE   (H5OPEN H5P_CLS_VOL_INITIALIZE_ID_g)
#define H5P_REFERENCE_ACCESS (H5OPEN H5P_CLS_REFERENCE_ACCESS_ID_g)

/*
 * The library's default property lists
 */
#define H5P_FILE_CREATE_DEFAULT      (H5OPEN H5P_LST_FILE_CREATE_ID_g)
#define H5P_FILE_ACCESS_DEFAULT      (H5OPEN H5P_LST_FILE_ACCESS_ID_g)
#define H5P_DATASET_CREATE_DEFAULT   (H5OPEN H5P_LST_DATASET_CREATE_ID_g)
#define H5P_DATASET_ACCESS_DEFAULT   (H5OPEN H5P_LST_DATASET_ACCESS_ID_g)
#define H5P_DATASET_XFER_DEFAULT     (H5OPEN H5P_LST_DATASET_XFER_ID_g)
#define H5P_FILE_MOUNT_DEFAULT       (H5OPEN H5P_LST_FILE_MOUNT_ID_g)
#define H5P_GROUP_CREATE_DEFAULT     (H5OPEN H5P_LST_GROUP_CREATE_ID_g)
#define H5P_GROUP_ACCESS_DEFAULT     (H5OPEN H5P_LST_GROUP_ACCESS_ID_g)
#define H5P_DATATYPE_CREATE_DEFAULT  (H5OPEN H5P_LST_DATATYPE_CREATE_ID_g)
#define H5P_DATATYPE_ACCESS_DEFAULT  (H5OPEN H5P_LST_DATATYPE_ACCESS_ID_g)
#define H5P_MAP_CREATE_DEFAULT       (H5OPEN H5P_LST_MAP_CREATE_ID_g)
#define H5P_MAP_ACCESS_DEFAULT       (H5OPEN H5P_LST_MAP_ACCESS_ID_g)
#define H5P_ATTRIBUTE_CREATE_DEFAULT (H5OPEN H5P_LST_ATTRIBUTE_CREATE_ID_g)
#define H5P_ATTRIBUTE_ACCESS_DEFAULT (H5OPEN H5P_LST_ATTRIBUTE_ACCESS_ID_g)
#define H5P_OBJECT_COPY_DEFAULT      (H5OPEN H5P_LST_OBJECT_COPY_ID_g)
#define H5P_LINK_CREATE_DEFAULT      (H5OPEN H5P_LST_LINK_CREATE_ID_g)
#define H5P_LINK_ACCESS_DEFAULT      (H5OPEN H5P_LST_LINK_ACCESS_ID_g)
#define H5P_VOL_INITIALIZE_DEFAULT   (H5OPEN H5P_LST_VOL_INITIALIZE_ID_g)
#define H5P_REFERENCE_ACCESS_DEFAULT (H5OPEN H5P_LST_REFERENCE_ACCESS_ID_g)

/* Common creation order flags (for links in groups and attributes on objects) */
#define H5P_CRT_ORDER_TRACKED 0x0001
#define H5P_CRT_ORDER_INDEXED 0x0002

/* Default value for all property list classes */
#define H5P_DEFAULT (hid_t)0

#ifdef __cplusplus
extern "C" {
#endif

/*******************/
/* Public Typedefs */
/*******************/

/* Define property list class callback function pointer types */
typedef herr_t (*H5P_cls_create_func_t)(hid_t prop_id, void *create_data);
typedef herr_t (*H5P_cls_copy_func_t)(hid_t new_prop_id, hid_t old_prop_id, void *copy_data);
typedef herr_t (*H5P_cls_close_func_t)(hid_t prop_id, void *close_data);

/* Define property list callback function pointer types */
typedef herr_t (*H5P_prp_cb1_t)(const char *name, size_t size, void *value);
typedef herr_t (*H5P_prp_cb2_t)(hid_t prop_id, const char *name, size_t size, void *value);
typedef H5P_prp_cb1_t H5P_prp_create_func_t;
typedef H5P_prp_cb2_t H5P_prp_set_func_t;
typedef H5P_prp_cb2_t H5P_prp_get_func_t;
typedef herr_t (*H5P_prp_encode_func_t)(const void *value, void **buf, size_t *size);
typedef herr_t (*H5P_prp_decode_func_t)(const void **buf, void *value);
typedef H5P_prp_cb2_t H5P_prp_delete_func_t;
typedef H5P_prp_cb1_t H5P_prp_copy_func_t;
typedef int (*H5P_prp_compare_func_t)(const void *value1, const void *value2, size_t size);
typedef H5P_prp_cb1_t H5P_prp_close_func_t;

/* Define property list iteration function type */
typedef herr_t (*H5P_iterate_t)(hid_t id, const char *name, void *iter_data);

/* Actual IO mode property */
typedef enum H5D_mpio_actual_chunk_opt_mode_t {
    /* The default value, H5D_MPIO_NO_CHUNK_OPTIMIZATION, is used for all I/O
     * operations that do not use chunk optimizations, including non-collective
     * I/O and contiguous collective I/O.
     */
    H5D_MPIO_NO_CHUNK_OPTIMIZATION = 0,
    H5D_MPIO_LINK_CHUNK,
    H5D_MPIO_MULTI_CHUNK
} H5D_mpio_actual_chunk_opt_mode_t;

typedef enum H5D_mpio_actual_io_mode_t {
    /* The following four values are conveniently defined as a bit field so that
     * we can switch from the default to independent or collective and then to
     * mixed without having to check the original value.
     *
     * NO_COLLECTIVE means that either collective I/O wasn't requested or that
     * no I/O took place.
     *
     * CHUNK_INDEPENDENT means that collective I/O was requested, but the
     * chunk optimization scheme chose independent I/O for each chunk.
     */
    H5D_MPIO_NO_COLLECTIVE     = 0x0,
    H5D_MPIO_CHUNK_INDEPENDENT = 0x1,
    H5D_MPIO_CHUNK_COLLECTIVE  = 0x2,
    H5D_MPIO_CHUNK_MIXED       = 0x1 | 0x2,

    /* The contiguous case is separate from the bit field. */
    H5D_MPIO_CONTIGUOUS_COLLECTIVE = 0x4
} H5D_mpio_actual_io_mode_t;

/* Broken collective IO property */
typedef enum H5D_mpio_no_collective_cause_t {
    H5D_MPIO_COLLECTIVE                               = 0x00,
    H5D_MPIO_SET_INDEPENDENT                          = 0x01,
    H5D_MPIO_DATATYPE_CONVERSION                      = 0x02,
    H5D_MPIO_DATA_TRANSFORMS                          = 0x04,
    H5D_MPIO_MPI_OPT_TYPES_ENV_VAR_DISABLED           = 0x08,
    H5D_MPIO_NOT_SIMPLE_OR_SCALAR_DATASPACES          = 0x10,
    H5D_MPIO_NOT_CONTIGUOUS_OR_CHUNKED_DATASET        = 0x20,
    H5D_MPIO_PARALLEL_FILTERED_WRITES_DISABLED        = 0x40,
    H5D_MPIO_ERROR_WHILE_CHECKING_COLLECTIVE_POSSIBLE = 0x80,
    H5D_MPIO_NO_COLLECTIVE_MAX_CAUSE                  = 0x100
} H5D_mpio_no_collective_cause_t;

/********************/
/* Public Variables */
/********************/

/* Property list class IDs */
/* (Internal to library, do not use!  Use macros above) */
H5_DLLVAR hid_t H5P_CLS_ROOT_ID_g;
H5_DLLVAR hid_t H5P_CLS_OBJECT_CREATE_ID_g;
H5_DLLVAR hid_t H5P_CLS_FILE_CREATE_ID_g;
H5_DLLVAR hid_t H5P_CLS_FILE_ACCESS_ID_g;
H5_DLLVAR hid_t H5P_CLS_DATASET_CREATE_ID_g;
H5_DLLVAR hid_t H5P_CLS_DATASET_ACCESS_ID_g;
H5_DLLVAR hid_t H5P_CLS_DATASET_XFER_ID_g;
H5_DLLVAR hid_t H5P_CLS_FILE_MOUNT_ID_g;
H5_DLLVAR hid_t H5P_CLS_GROUP_CREATE_ID_g;
H5_DLLVAR hid_t H5P_CLS_GROUP_ACCESS_ID_g;
H5_DLLVAR hid_t H5P_CLS_DATATYPE_CREATE_ID_g;
H5_DLLVAR hid_t H5P_CLS_DATATYPE_ACCESS_ID_g;
H5_DLLVAR hid_t H5P_CLS_MAP_CREATE_ID_g;
H5_DLLVAR hid_t H5P_CLS_MAP_ACCESS_ID_g;
H5_DLLVAR hid_t H5P_CLS_STRING_CREATE_ID_g;
H5_DLLVAR hid_t H5P_CLS_ATTRIBUTE_CREATE_ID_g;
H5_DLLVAR hid_t H5P_CLS_ATTRIBUTE_ACCESS_ID_g;
H5_DLLVAR hid_t H5P_CLS_OBJECT_COPY_ID_g;
H5_DLLVAR hid_t H5P_CLS_LINK_CREATE_ID_g;
H5_DLLVAR hid_t H5P_CLS_LINK_ACCESS_ID_g;
H5_DLLVAR hid_t H5P_CLS_VOL_INITIALIZE_ID_g;
H5_DLLVAR hid_t H5P_CLS_REFERENCE_ACCESS_ID_g;

/* Default roperty list IDs */
/* (Internal to library, do not use!  Use macros above) */
H5_DLLVAR hid_t H5P_LST_FILE_CREATE_ID_g;
H5_DLLVAR hid_t H5P_LST_FILE_ACCESS_ID_g;
H5_DLLVAR hid_t H5P_LST_DATASET_CREATE_ID_g;
H5_DLLVAR hid_t H5P_LST_DATASET_ACCESS_ID_g;
H5_DLLVAR hid_t H5P_LST_DATASET_XFER_ID_g;
H5_DLLVAR hid_t H5P_LST_FILE_MOUNT_ID_g;
H5_DLLVAR hid_t H5P_LST_GROUP_CREATE_ID_g;
H5_DLLVAR hid_t H5P_LST_GROUP_ACCESS_ID_g;
H5_DLLVAR hid_t H5P_LST_DATATYPE_CREATE_ID_g;
H5_DLLVAR hid_t H5P_LST_DATATYPE_ACCESS_ID_g;
H5_DLLVAR hid_t H5P_LST_MAP_CREATE_ID_g;
H5_DLLVAR hid_t H5P_LST_MAP_ACCESS_ID_g;
H5_DLLVAR hid_t H5P_LST_ATTRIBUTE_CREATE_ID_g;
H5_DLLVAR hid_t H5P_LST_ATTRIBUTE_ACCESS_ID_g;
H5_DLLVAR hid_t H5P_LST_OBJECT_COPY_ID_g;
H5_DLLVAR hid_t H5P_LST_LINK_CREATE_ID_g;
H5_DLLVAR hid_t H5P_LST_LINK_ACCESS_ID_g;
H5_DLLVAR hid_t H5P_LST_VOL_INITIALIZE_ID_g;
H5_DLLVAR hid_t H5P_LST_REFERENCE_ACCESS_ID_g;

/*********************/
/* Public Prototypes */
/*********************/

/* Generic property list routines */

/**
 * \ingroup GPLO
 *
 * \brief Terminates access to a property list
 *
 * \plist_id
 *
 * \return \herr_t
 *
 * \details H5Pclose() terminates access to a property list. All property
 *          lists should be closed when the application is finished
 *          accessing them. This frees resources used by the property
 *          list.
 *
 * \since 1.0.0
 *
 */
H5_DLL herr_t H5Pclose(hid_t plist_id);
/**
 * \ingroup GPLO
 *
 * \brief Creates a new property list as an instance of a property list class
 *
 * \plistcls_id{cls_id}
 *
 * \return \hid_t{property list}
 *
 * \details H5Pcreate() creates a new property list as an instance of
 *          some property list class. The new property list is initialized
 *          with default values for the specified class. The classes are as
 *          follows:
 *
 * <table>
 *   <tr>
 *     <th>Class Identifier</th>
 *     <th>Class Name</th>
 *     <th>Comments</th>
 *   </tr>
 *   <tr>
 *     <td>#H5P_ATTRIBUTE_CREATE</td>
 *     <td>attribute create</td>
 *     <td>Properties for attribute creation</td>
 *   </tr>
 *   <tr>
 *     <td>#H5P_DATASET_ACCESS</td>
 *     <td>dataset access</td>
 *     <td>Properties for dataset access</td>
 *   </tr>
 *   <tr>
 *     <td>#H5P_DATASET_CREATE</td>
 *     <td>dataset create</td>
 *     <td>Properties for dataset creation</td>
 *   </tr>
 *   <tr>
 *     <td>#H5P_DATASET_XFER</td>
 *     <td>data transfer</td>
 *     <td>Properties for raw data transfer</td>
 *   </tr>
 *   <tr>
 *     <td>#H5P_DATATYPE_ACCESS</td>
 *     <td>datatype access</td>
 *     <td>Properties for datatype access</td>
 *   </tr>
 *   <tr>
 *     <td>#H5P_DATATYPE_CREATE</td>
 *     <td>datatype create</td>
 *     <td>Properties for datatype creation</td>
 *   </tr>
 *   <tr>
 *     <td>#H5P_FILE_ACCESS</td>
 *     <td>file access</td>
 *     <td>Properties for file access</td>
 *   </tr>
 *   <tr>
 *     <td>#H5P_FILE_CREATE</td>
 *     <td>file create</td>
 *     <td>Properties for file creation</td>
 *   </tr>
 *   <tr>
 *     <td>#H5P_FILE_MOUNT</td>
 *     <td>file mount</td>
 *     <td>Properties for file mounting</td>
 *   </tr>
 *   <tr valign="top">
 *     <td>#H5P_GROUP_ACCESS</td>
 *     <td>group access</td>
 *     <td>Properties for group access</td>
 *   </tr>
 *   <tr>
 *     <td>#H5P_GROUP_CREATE</td>
 *     <td>group create</td>
 *     <td>Properties for group creation</td>
 *   </tr>
 *   <tr>
 *     <td>#H5P_LINK_ACCESS</td>
 *     <td>link access</td>
 *     <td>Properties governing link traversal when accessing objects</td>
 *   </tr>
 *   <tr>
 *     <td>#H5P_LINK_CREATE</td>
 *     <td>link create</td>
 *     <td>Properties governing link creation</td>
 *   </tr>
 *   <tr>
 *     <td>#H5P_OBJECT_COPY</td>
 *     <td>object copy</td>
 *     <td>Properties governing the object copying process</td>
 *   </tr>
 *   <tr>
 *     <td>#H5P_OBJECT_CREATE</td>
 *     <td>object create</td>
 *     <td>Properties for object creation</td>
 *   </tr>
 *   <tr>
 *     <td>#H5P_STRING_CREATE</td>
 *     <td>string create</td>
 *     <td>Properties for character encoding when encoding strings or
 *       object names</td>
 *   </tr>
 *   <tr>
 *     <td>#H5P_VOL_INITIALIZE</td>
 *     <td>vol initialize</td>
 *     <td>Properties for VOL initialization</td>
 *   </tr>
 * </table>
 *
 * This property list must eventually be closed with H5Pclose();
 * otherwise, errors are likely to occur.
 *
 * \version 1.12.0 The #H5P_VOL_INITIALIZE property list class was added
 * \version 1.8.15 For each class, the class name returned by
 *                 H5Pget_class_name() was added.
 *                 The list of possible Fortran values was updated.
 * \version 1.8.0 The following property list classes were added at this
 *                release: #H5P_DATASET_ACCESS, #H5P_GROUP_CREATE,
 *                #H5P_GROUP_ACCESS, #H5P_DATATYPE_CREATE,
 *                #H5P_DATATYPE_ACCESS, #H5P_ATTRIBUTE_CREATE
 *
 * \since 1.0.0
 *
 */
H5_DLL hid_t  H5Pcreate(hid_t cls_id);
H5_DLL hid_t  H5Pcreate_class(hid_t parent, const char *name, H5P_cls_create_func_t cls_create,
                              void *create_data, H5P_cls_copy_func_t cls_copy, void *copy_data,
                              H5P_cls_close_func_t cls_close, void *close_data);
H5_DLL char * H5Pget_class_name(hid_t pclass_id);
H5_DLL herr_t H5Pregister2(hid_t cls_id, const char *name, size_t size, void *def_value,
                           H5P_prp_create_func_t prp_create, H5P_prp_set_func_t prp_set,
                           H5P_prp_get_func_t prp_get, H5P_prp_delete_func_t prp_del,
                           H5P_prp_copy_func_t prp_copy, H5P_prp_compare_func_t prp_cmp,
                           H5P_prp_close_func_t prp_close);
H5_DLL herr_t H5Pinsert2(hid_t plist_id, const char *name, size_t size, void *value,
                         H5P_prp_set_func_t prp_set, H5P_prp_get_func_t prp_get,
                         H5P_prp_delete_func_t prp_delete, H5P_prp_copy_func_t prp_copy,
                         H5P_prp_compare_func_t prp_cmp, H5P_prp_close_func_t prp_close);
H5_DLL herr_t H5Pset(hid_t plist_id, const char *name, const void *value);
H5_DLL htri_t H5Pexist(hid_t plist_id, const char *name);
H5_DLL herr_t H5Pencode2(hid_t plist_id, void *buf, size_t *nalloc, hid_t fapl_id);
H5_DLL hid_t  H5Pdecode(const void *buf);
H5_DLL herr_t H5Pget_size(hid_t id, const char *name, size_t *size);
H5_DLL herr_t H5Pget_nprops(hid_t id, size_t *nprops);
H5_DLL hid_t  H5Pget_class(hid_t plist_id);
H5_DLL hid_t  H5Pget_class_parent(hid_t pclass_id);
H5_DLL herr_t H5Pget(hid_t plist_id, const char *name, void *value);
H5_DLL htri_t H5Pequal(hid_t id1, hid_t id2);
H5_DLL htri_t H5Pisa_class(hid_t plist_id, hid_t pclass_id);
H5_DLL int    H5Piterate(hid_t id, int *idx, H5P_iterate_t iter_func, void *iter_data);
H5_DLL herr_t H5Pcopy_prop(hid_t dst_id, hid_t src_id, const char *name);
H5_DLL herr_t H5Premove(hid_t plist_id, const char *name);
H5_DLL herr_t H5Punregister(hid_t pclass_id, const char *name);
H5_DLL herr_t H5Pclose_class(hid_t plist_id);
H5_DLL hid_t  H5Pcopy(hid_t plist_id);

/* Object creation property list (OCPL) routines */

/**
 * \ingroup OCPL
 *
 * \brief Returns information about a filter in a pipeline
 *
 * \todo Signature for H5Pget_filter2 is different in H5Pocpl.c than in
 *       H5Ppublic.h
 *
 * \plist_id{plist_id}
 * \param[in] idx    Sequence number within the filter pipeline of the filter
 *                   for which information is sought
 * \param[out] flags Bit vector specifying certain general properties of the
 *                   filter
 * \param[in,out] cd_nelmts Number of elements in \p cd_values
 * \param[out]    cd_values Auxiliary data for the filter
 * \param[in]     namelen   Anticipated number of characters in \p name
 * \param[out]    name      Name of the filter
 * \param[out] filter_config Bit field, as described in H5Zget_filter_info()
 *
 * \return Returns a negative value on failure, and the filter identifier
 *         if successful (see #H5Z_filter_t):
 *         - #H5Z_FILTER_DEFLATE     Data compression filter,
 *                                    employing the gzip algorithm
 *         - #H5Z_FILTER_SHUFFLE     Data shuffling filter
 *         - #H5Z_FILTER_FLETCHER32  Error detection filter, employing the
 *                                     Fletcher32 checksum algorithm
 *         - #H5Z_FILTER_SZIP        Data compression filter, employing the
 *                                     SZIP algorithm
 *         - #H5Z_FILTER_NBIT        Data compression filter, employing the
 *                                     N-bit algorithm
 *         - #H5Z_FILTER_SCALEOFFSET Data compression filter, employing the
 *                                     scale-offset algorithm
 *
 * \details H5Pget_filter2() returns information about a filter specified by
 *          its filter number, in a filter pipeline specified by the property
 *          list with which it is associated.
 *
 *          \p plist_id must be a dataset or group creation property list.
 *
 *          \p idx is a value between zero and N-1, as described in
 *          H5Pget_nfilters(). The function will return a negative value if
 *          the filter number is out of range.
 *
 *          The structure of the \p flags argument is discussed in
 *          H5Pset_filter().
 *
 *          On input, \p cd_nelmts indicates the number of entries in the
 *          \p cd_values array, as allocated by the caller; on return,
 *          \p cd_nelmts contains the number of values defined by the filter.
 *
 *          If \p name is a pointer to an array of at least \p namelen bytes,
 *          the filter name will be copied into that array. The name will be
 *          null terminated if \p namelen is large enough. The filter name
 *          returned will be the name appearing in the file, the name
 *          registered for the filter, or an empty string.
 *
 *          \p filter_config is the bit field described in
 *          H5Zget_filter_info().
 *
 * \version 1.8.5 Function extended to work with group creation property
 *                lists.
 * \since 1.8.0
 *
 */
H5_DLL H5Z_filter_t H5Pget_filter2(hid_t plist_id, unsigned idx, unsigned int *flags /*out*/,
                                   size_t *cd_nelmts /*out*/, unsigned cd_values[] /*out*/, size_t namelen,
                                   char name[], unsigned *filter_config /*out*/);
H5_DLL herr_t       H5Pset_attr_phase_change(hid_t plist_id, unsigned max_compact, unsigned min_dense);
H5_DLL herr_t       H5Pget_attr_phase_change(hid_t plist_id, unsigned *max_compact, unsigned *min_dense);
H5_DLL herr_t       H5Pset_attr_creation_order(hid_t plist_id, unsigned crt_order_flags);
H5_DLL herr_t       H5Pget_attr_creation_order(hid_t plist_id, unsigned *crt_order_flags);
H5_DLL herr_t       H5Pset_obj_track_times(hid_t plist_id, hbool_t track_times);
H5_DLL herr_t       H5Pget_obj_track_times(hid_t plist_id, hbool_t *track_times);
H5_DLL herr_t H5Pmodify_filter(hid_t plist_id, H5Z_filter_t filter, unsigned int flags, size_t cd_nelmts,
                               const unsigned int cd_values[/*cd_nelmts*/]);
H5_DLL herr_t H5Pset_filter(hid_t plist_id, H5Z_filter_t filter, unsigned int flags, size_t cd_nelmts,
                            const unsigned int c_values[]);
H5_DLL int    H5Pget_nfilters(hid_t plist_id);
H5_DLL herr_t H5Pget_filter_by_id2(hid_t plist_id, H5Z_filter_t id, unsigned int *flags /*out*/,
                                   size_t *cd_nelmts /*out*/, unsigned cd_values[] /*out*/, size_t namelen,
                                   char name[] /*out*/, unsigned *filter_config /*out*/);
H5_DLL htri_t H5Pall_filters_avail(hid_t plist_id);
H5_DLL herr_t H5Premove_filter(hid_t plist_id, H5Z_filter_t filter);
H5_DLL herr_t H5Pset_deflate(hid_t plist_id, unsigned aggression);
H5_DLL herr_t H5Pset_fletcher32(hid_t plist_id);

/* File creation property list (FCPL) routines */
H5_DLL herr_t H5Pset_userblock(hid_t plist_id, hsize_t size);
H5_DLL herr_t H5Pget_userblock(hid_t plist_id, hsize_t *size);
H5_DLL herr_t H5Pset_sizes(hid_t plist_id, size_t sizeof_addr, size_t sizeof_size);
H5_DLL herr_t H5Pget_sizes(hid_t plist_id, size_t *sizeof_addr /*out*/, size_t *sizeof_size /*out*/);
H5_DLL herr_t H5Pset_sym_k(hid_t plist_id, unsigned ik, unsigned lk);
H5_DLL herr_t H5Pget_sym_k(hid_t plist_id, unsigned *ik /*out*/, unsigned *lk /*out*/);
H5_DLL herr_t H5Pset_istore_k(hid_t plist_id, unsigned ik);
H5_DLL herr_t H5Pget_istore_k(hid_t plist_id, unsigned *ik /*out*/);
H5_DLL herr_t H5Pset_shared_mesg_nindexes(hid_t plist_id, unsigned nindexes);
H5_DLL herr_t H5Pget_shared_mesg_nindexes(hid_t plist_id, unsigned *nindexes);
H5_DLL herr_t H5Pset_shared_mesg_index(hid_t plist_id, unsigned index_num, unsigned mesg_type_flags,
                                       unsigned min_mesg_size);
H5_DLL herr_t H5Pget_shared_mesg_index(hid_t plist_id, unsigned index_num, unsigned *mesg_type_flags,
                                       unsigned *min_mesg_size);
H5_DLL herr_t H5Pset_shared_mesg_phase_change(hid_t plist_id, unsigned max_list, unsigned min_btree);
H5_DLL herr_t H5Pget_shared_mesg_phase_change(hid_t plist_id, unsigned *max_list, unsigned *min_btree);
H5_DLL herr_t H5Pset_file_space_strategy(hid_t plist_id, H5F_fspace_strategy_t strategy, hbool_t persist,
                                         hsize_t threshold);
H5_DLL herr_t H5Pget_file_space_strategy(hid_t plist_id, H5F_fspace_strategy_t *strategy, hbool_t *persist,
                                         hsize_t *threshold);
H5_DLL herr_t H5Pset_file_space_page_size(hid_t plist_id, hsize_t fsp_size);
H5_DLL herr_t H5Pget_file_space_page_size(hid_t plist_id, hsize_t *fsp_size);

/* File access property list (FAPL) routines */
H5_DLL herr_t      H5Pset_alignment(hid_t fapl_id, hsize_t threshold, hsize_t alignment);
H5_DLL herr_t      H5Pget_alignment(hid_t fapl_id, hsize_t *threshold /*out*/, hsize_t *alignment /*out*/);
H5_DLL herr_t      H5Pset_driver(hid_t plist_id, hid_t driver_id, const void *driver_info);
H5_DLL hid_t       H5Pget_driver(hid_t plist_id);
H5_DLL const void *H5Pget_driver_info(hid_t plist_id);
H5_DLL herr_t      H5Pset_vol(hid_t plist_id, hid_t new_vol_id, const void *new_vol_info);
H5_DLL herr_t      H5Pget_vol_id(hid_t plist_id, hid_t *vol_id);
H5_DLL herr_t      H5Pget_vol_info(hid_t plist_id, void **vol_info);
H5_DLL herr_t      H5Pset_family_offset(hid_t fapl_id, hsize_t offset);
H5_DLL herr_t      H5Pget_family_offset(hid_t fapl_id, hsize_t *offset);
H5_DLL herr_t      H5Pset_multi_type(hid_t fapl_id, H5FD_mem_t type);
H5_DLL herr_t      H5Pget_multi_type(hid_t fapl_id, H5FD_mem_t *type);
H5_DLL herr_t      H5Pset_cache(hid_t plist_id, int mdc_nelmts, size_t rdcc_nslots, size_t rdcc_nbytes,
                                double rdcc_w0);
H5_DLL herr_t      H5Pget_cache(hid_t plist_id, int *mdc_nelmts, /* out */
                                size_t *rdcc_nslots /*out*/, size_t *rdcc_nbytes /*out*/, double *rdcc_w0);
H5_DLL herr_t      H5Pset_mdc_config(hid_t plist_id, H5AC_cache_config_t *config_ptr);
H5_DLL herr_t      H5Pget_mdc_config(hid_t plist_id, H5AC_cache_config_t *config_ptr); /* out */
H5_DLL herr_t      H5Pset_gc_references(hid_t fapl_id, unsigned gc_ref);
H5_DLL herr_t      H5Pget_gc_references(hid_t fapl_id, unsigned *gc_ref /*out*/);
H5_DLL herr_t      H5Pset_fclose_degree(hid_t fapl_id, H5F_close_degree_t degree);
H5_DLL herr_t      H5Pget_fclose_degree(hid_t fapl_id, H5F_close_degree_t *degree);
H5_DLL herr_t      H5Pset_meta_block_size(hid_t fapl_id, hsize_t size);
H5_DLL herr_t      H5Pget_meta_block_size(hid_t fapl_id, hsize_t *size /*out*/);
H5_DLL herr_t      H5Pset_sieve_buf_size(hid_t fapl_id, size_t size);
H5_DLL herr_t      H5Pget_sieve_buf_size(hid_t fapl_id, size_t *size /*out*/);
H5_DLL herr_t      H5Pset_small_data_block_size(hid_t fapl_id, hsize_t size);
H5_DLL herr_t      H5Pget_small_data_block_size(hid_t fapl_id, hsize_t *size /*out*/);
H5_DLL herr_t      H5Pset_libver_bounds(hid_t plist_id, H5F_libver_t low, H5F_libver_t high);
H5_DLL herr_t      H5Pget_libver_bounds(hid_t plist_id, H5F_libver_t *low, H5F_libver_t *high);
H5_DLL herr_t      H5Pset_elink_file_cache_size(hid_t plist_id, unsigned efc_size);
H5_DLL herr_t      H5Pget_elink_file_cache_size(hid_t plist_id, unsigned *efc_size);
H5_DLL herr_t      H5Pset_file_image(hid_t fapl_id, void *buf_ptr, size_t buf_len);
H5_DLL herr_t      H5Pget_file_image(hid_t fapl_id, void **buf_ptr_ptr, size_t *buf_len_ptr);
H5_DLL herr_t      H5Pset_file_image_callbacks(hid_t fapl_id, H5FD_file_image_callbacks_t *callbacks_ptr);
H5_DLL herr_t      H5Pget_file_image_callbacks(hid_t fapl_id, H5FD_file_image_callbacks_t *callbacks_ptr);
H5_DLL herr_t      H5Pset_core_write_tracking(hid_t fapl_id, hbool_t is_enabled, size_t page_size);
H5_DLL herr_t      H5Pget_core_write_tracking(hid_t fapl_id, hbool_t *is_enabled, size_t *page_size);
H5_DLL herr_t      H5Pset_metadata_read_attempts(hid_t plist_id, unsigned attempts);
H5_DLL herr_t      H5Pget_metadata_read_attempts(hid_t plist_id, unsigned *attempts);
H5_DLL herr_t      H5Pset_object_flush_cb(hid_t plist_id, H5F_flush_cb_t func, void *udata);
H5_DLL herr_t      H5Pget_object_flush_cb(hid_t plist_id, H5F_flush_cb_t *func, void **udata);
H5_DLL herr_t      H5Pset_mdc_log_options(hid_t plist_id, hbool_t is_enabled, const char *location,
                                          hbool_t start_on_access);
H5_DLL herr_t      H5Pget_mdc_log_options(hid_t plist_id, hbool_t *is_enabled, char *location,
                                          size_t *location_size, hbool_t *start_on_access);
H5_DLL herr_t      H5Pset_evict_on_close(hid_t fapl_id, hbool_t evict_on_close);
H5_DLL herr_t      H5Pget_evict_on_close(hid_t fapl_id, hbool_t *evict_on_close);
H5_DLL herr_t      H5Pset_file_locking(hid_t fapl_id, hbool_t use_file_locking, hbool_t ignore_when_disabled);
H5_DLL herr_t H5Pget_file_locking(hid_t fapl_id, hbool_t *use_file_locking, hbool_t *ignore_when_disabled);
#ifdef H5_HAVE_PARALLEL
H5_DLL herr_t H5Pset_all_coll_metadata_ops(hid_t plist_id, hbool_t is_collective);
H5_DLL herr_t H5Pget_all_coll_metadata_ops(hid_t plist_id, hbool_t *is_collective);
H5_DLL herr_t H5Pset_coll_metadata_write(hid_t plist_id, hbool_t is_collective);
H5_DLL herr_t H5Pget_coll_metadata_write(hid_t plist_id, hbool_t *is_collective);
H5_DLL herr_t H5Pget_mpi_params(hid_t fapl_id, MPI_Comm *comm, MPI_Info *info);
H5_DLL herr_t H5Pset_mpi_params(hid_t fapl_id, MPI_Comm comm, MPI_Info info);
#endif /* H5_HAVE_PARALLEL */
H5_DLL herr_t H5Pset_mdc_image_config(hid_t plist_id, H5AC_cache_image_config_t *config_ptr);
H5_DLL herr_t H5Pget_mdc_image_config(hid_t plist_id, H5AC_cache_image_config_t *config_ptr /*out*/);
H5_DLL herr_t H5Pset_page_buffer_size(hid_t plist_id, size_t buf_size, unsigned min_meta_per,
                                      unsigned min_raw_per);
H5_DLL herr_t H5Pget_page_buffer_size(hid_t plist_id, size_t *buf_size, unsigned *min_meta_per,
                                      unsigned *min_raw_per);

/* Dataset creation property list (DCPL) routines */

/**
 * \ingroup DCPL
 *
 * \brief Retrieves the size of chunks for the raw data of a chunked
 *        layout dataset
 *
 * \dcpl_id{plist_id}
 * \param[in]  max_ndims Size of the \p dims array
 * \param[out] dim Array to store the chunk dimensions
 *
 * \return Returns chunk dimensionality if successful;
 *         otherwise returns a negative value.
 *
 * \details H5Pget_chunk() retrieves the size of chunks for the raw data
 *          of a chunked layout dataset. This function is only valid for
 *          dataset creation property lists. At most, \p max_ndims elements
 *          of \p dim will be initialized.
 *
 * \since 1.0.0
 *
 */
H5_DLL int H5Pget_chunk(hid_t plist_id, int max_ndims, hsize_t dim[] /*out*/);
/**
 *-------------------------------------------------------------------------
 *
 * \ingroup DCPL
 *
 * \brief Retrieves the edge chunk option setting from a dataset creation
 *        property list
 *
 * \dcpl_id{plist_id}
 * \param[out] opts  Edge chunk option flag. Valid values are described in
 *                   H5Pset_chunk_opts(). The option status can be
 *                   retrieved using the bitwise AND operator ( & ). For
 *                   example, the expression
 *                   (opts&#H5D_CHUNK_DONT_FILTER_PARTIAL_CHUNKS) will
 *                   evaluate to #H5D_CHUNK_DONT_FILTER_PARTIAL_CHUNKS if
 *                   that option has been enabled. Otherwise, it will
 *                   evaluate to 0 (zero).
 *
 * \return \herr_t
 *
 * \details H5Pget_chunk_opts() retrieves the edge chunk option setting
 *          stored in the dataset creation property list \p plist_id.
 *
 * \since 1.10.0
 *
 */
H5_DLL herr_t H5Pget_chunk_opts(hid_t plist_id, unsigned *opts);
/**
 *-------------------------------------------------------------------------
 *
 * \ingroup DCPL
 *
 * \brief Retrieves the time when fill values are written to a dataset
 *
 * \dcpl_id{plist_id}
 * \param[out] fill_time Setting for the timing of writing fill values to
 *                       the dataset
 *
 * \return \herr_t
 *
 * \details H5Pget_fill_time() examines the dataset creation property list
 *          \p plist_id to determine when fill values are to be written to
 *          a dataset. Valid values returned in \p fill_time are as
 *          follows:
 *
 *          <table>
 *           <tr>
 *            <td>#H5D_FILL_TIME_IFSET</td>
 *            <td>Fill values are written to the dataset when storage
 *                space is allocated only if there is a user-defined fill
 *                value, i.e., one set with H5Pset_fill_value(). (Default)
 *             </td>
 *           </tr>
 *           <tr>
 *            <td>#H5D_FILL_TIME_ALLOC</td>
 *            <td>Fill values are written to the dataset when storage
 *                space is allocated.</td>
 *           </tr>
 *           <tr>
 *            <td>#H5D_FILL_TIME_NEVER</td>
 *            <td>Fill values are never written to the dataset.</td>
 *           </tr>
 *          </table>
 *
 * \note H5Pget_fill_time() is designed to work in coordination with the
 *       dataset fill value and dataset storage allocation time properties,
 *       retrieved with the functions H5Pget_fill_value() and
 *       H5Pget_alloc_time().
 *
 * \since 1.6.0
 *
 */
H5_DLL herr_t H5Pget_fill_time(hid_t plist_id, H5D_fill_time_t *fill_time /*out*/);
/**
 *-------------------------------------------------------------------------
 *
 * \ingroup DCPL
 *
 * \brief Retrieves a dataset fill value
 *
 * \dcpl_id{plist_id}
 * \param[in]  type_id Datatype identifier for the value passed via
 *                     \p value
 * \param[out] value   Pointer to buffer to contain the returned
 *                     fill value
 *
 * \return \herr_t
 *
 * \details H5Pget_fill_value() returns the dataset fill value defined in
 *          the dataset creation property list \p plist_id. The fill value
 *          is returned through the \p value pointer and will be converted
 *          to the datatype specified  by \p type_id. This datatype may
 *          differ from the fill value datatype in the property list, but
 *          the HDF5 library must be able to convert between the two
 *          datatypes.
 *
 *          If the fill value is undefined, i.e., set to NULL in the
 *          property list, H5Pget_fill_value() will return an error.
 *          H5Pfill_value_defined() should be used to check for this
 *          condition before H5Pget_fill_value() is called.
 *
 *          Memory must be allocated by the calling application.
 *
 * \note H5Pget_fill_value() is designed to coordinate with the dataset
 *       storage allocation time and fill value write time properties,
 *       which can be retrieved with the functions H5Pget_alloc_time()
 *       and H5Pget_fill_time(), respectively.
 *
 * \since 1.0.0
 *
 */
H5_DLL herr_t H5Pget_fill_value(hid_t plist_id, hid_t type_id, void *value /*out*/);
/**
 *-------------------------------------------------------------------------
 * \ingroup DCPL
 *
 * \brief Returns the layout of the raw data for a dataset
 *
 * \dcpl_id{plist_id}
 *
 * \return Returns the layout type (a non-negative value) of a dataset
 *         creation property list if successful. Valid return values are:
 *         - #H5D_COMPACT: Raw data is stored in the object header in the
 *                        file.
 *         - #H5D_CONTIGUOUS: Raw data is stored separately from the object
 *                           header in one contiguous chunk in the file.
 *         - #H5D_CHUNKED: Raw data is stored separately from the object
 *                        header in chunks in separate locations in the
 *                        file.
 *         - #H5D_VIRTUAL: Raw data is drawn from multiple datasets in
 *                        different files.
 * \return
 *         Otherwise, returns a negative value indicating failure.
 *
 * \details H5Pget_layout() returns the layout of the raw data for a
 *          dataset. This function is only valid for dataset creation
 *          property lists.
 *
 *          Note that a compact storage layout may affect writing data to
 *          the dataset with parallel applications. See the H5Dwrite()
 *          documentation for details.
 *
 * \version 1.10.0 #H5D_VIRTUAL added in this release.
 *
 * \since 1.0.0
 *
 */
H5_DLL H5D_layout_t H5Pget_layout(hid_t plist_id);
/**
 *-------------------------------------------------------------------------
 * \ingroup DCPL
 *
 * \brief Sets the size of the chunks used to store a chunked layout
 *        dataset
 *
 * \dcpl_id{plist_id}
 * \param[in] ndims  The number of dimensions of each chunk
 * \param[in] dim    An array defining the size, in dataset elements, of
 *                   each chunk
 *
 * \return \herr_t
 * \details H5Pset_chunk() sets the size of the chunks used to store a
 *          chunked layout dataset. This function is only valid for dataset
 *          creation property lists.
 *
 *          The \p ndims parameter currently must be the same size as the
 *          rank of the dataset.
 *
 *          The values of the \p dim array define the size of the chunks
 *          to store the dataset's raw data. The unit of measure for \p dim
 *          values is dataset elements.
 *
 *          As a side-effect of this function, the layout of the dataset is
 *          changed to #H5D_CHUNKED, if it is not already so set.
 *
 * \note Chunk size cannot exceed the size of a fixed-size dataset. For
 *       example, a dataset consisting of a 5x4 fixed-size array cannot be
 *       defined with 10x10 chunks. Chunk maximums:
 *       - The maximum number of elements in a chunk is 2<sup>32</sup>-1 which
 *         is equal to 4,294,967,295. If the number of elements in a chunk is
 *         set via H5Pset_chunk() to a value greater than 2<sup>32</sup>-1,
 *         then H5Pset_chunk() will fail.
 *       - The maximum size for any chunk is 4GB. If a chunk that is larger
 *         than 4GB attempts to be written with H5Dwrite(), then H5Dwrite()
 *         will fail.
 *
 * \see H5Pset_layout(), H5Dwrite()
 *
 * \since 1.0.0
 *
 */
H5_DLL herr_t H5Pset_chunk(hid_t plist_id, int ndims, const hsize_t dim[/*ndims*/]);
/**
 *-------------------------------------------------------------------------
 *
 * \ingroup DCPL
 *
 * \brief Sets the edge chunk option in a dataset creation property list
 *
 * \dcpl_id{plist_id}
 * \param[in] opts Edge chunk option flag. Valid values are:
 *                 \li #H5D_CHUNK_DONT_FILTER_PARTIAL_CHUNKS
 *                     When enabled, filters are not applied to partial
 *                     edge chunks. When disabled, partial edge chunks are
 *                     filtered. Enabling this option will improve
 *                     performance when appending to the dataset and, when
 *                     compression filters are used, prevent reallocation
 *                     of these chunks. Datasets created with this option
 *                     enabled will be inaccessible with HDF5 library
 *                     versions before Release 1.10. Default: \e Disabled
 *                 \li 0 (zero) Disables option; partial edge chunks
 *                     will be compressed.
 *
 * \return \herr_t
 *
 * \details H5Pset_chunk_opts() sets the edge chunk option in the
 *          dataset creation property list \p dcpl_id.
 *
 *          The available option is detailed in the parameters section.
 *          Only chunks that are not completely filled by the dataset’s
 *          dataspace are affected by this option. Such chunks are
 *          referred to as partial edge chunks.
 *
 * \note \b Motivation: H5Pset_chunk_opts() is used to specify storage
 *       options for chunks on the edge of a dataset’s dataspace. This
 *       capability allows the user to tune performance in cases where
 *       the dataset size may not be a multiple of the chunk size and
 *       the handling of partial edge chunks can impact performance.
 *
 * \since 1.10.0
 *
 */
H5_DLL herr_t H5Pset_chunk_opts(hid_t plist_id, unsigned opts);
/**
 *-------------------------------------------------------------------------
 *
 * \ingroup DCPL
 *
 * \brief Sets the time when fill values are written to a dataset
 *
 * \dcpl_id{plist_id}
 * \param[in] fill_time When to write fill values to a dataset
 *
 * \return \herr_t
 *
 * \details H5Pset_fill_time() sets up the timing for writing fill values
 *          to a dataset. This property is set in the dataset creation
 *          property list \p plist_id. Timing is specified in \p fill_time
 *          with one of the following values:
 *
 *          <table>
 *           <tr>
 *            <td>#H5D_FILL_TIME_IFSET</td>
 *            <td>Write fill values to the dataset when storage space is
 *                allocated only if there is a user-defined fill value,
 *                i.e.,one set with H5Pset_fill_value(). (Default)</td>
 *           </tr>
 *           <tr>
 *            <td>#H5D_FILL_TIME_ALLOC</td>
 *            <td>Write fill values to the dataset when storage space is
 *                allocated.</td>
 *           </tr>
 *           <tr>
 *            <td>#H5D_FILL_TIME_NEVER</td>
 *            <td>Never write fill values to the dataset.</td>
 *           </tr>
 *          </table>
 *
 * \note H5Pset_fill_time() is designed for coordination with the dataset
 *      fill value and dataset storage allocation time properties, set
 *      with the functions H5Pset_fill_value() and H5Pset_alloc_time().
 *      See H5Dcreate() for further cross-references.
 *
 * \since 1.6.0
 *
 */
H5_DLL herr_t H5Pset_fill_time(hid_t plist_id, H5D_fill_time_t fill_time);
/**
 *-------------------------------------------------------------------------
 *
 * \ingroup DCPL
 *
 * \brief Sets the fill value for a dataset
 *
 * \dcpl_id{plist_id}
 * \param[in] type_id Datatype of \p value
 * \param[in] value Pointer to buffer containing value to use as
 *            fill value
 *
 * \return \herr_t
 *
 * \details H5Pset_fill_value() sets the fill value for a dataset in the
 *          dataset creation property list. \p value is interpreted as
 *          being of datatype \p type_id. This datatype may differ from
 *          that of the dataset, but the HDF5 library must be able to
 *          convert \p value to the dataset datatype when the dataset is
 *          created.
 *
 *          The default fill value is 0 (zero), which is interpreted
 *          according to the actual dataset datatype.
 *
 *          Setting \p value to NULL indicates that the fill value is to
 *          be undefined.
 *
 * \note Applications sometimes write data only to portions of an allocated
 *       dataset. It is often useful in such cases to fill the unused space
 *       with a known fill value. This function allows the user application
 *       to set that fill value; the functions H5Dfill() and
 *       H5Pset_fill_time(), respectively, provide the ability to apply the
 *       fill value on demand or to set up its automatic application.
 *
 * \note A fill value should be defined so that it is appropriate for the
 *       application. While the HDF5 default fill value is 0 (zero), it is
 *       often appropriate to use another value. It might be useful, for
 *       example, to use a value that is known to be impossible for the
 *       application to legitimately generate.
 *
 * \note H5Pset_fill_value() is designed to work in concert with
 *       H5Pset_alloc_time() and H5Pset_fill_time(). H5Pset_alloc_time()
 *       and H5Pset_fill_time() govern the timing of dataset storage
 *       allocation and fill value write operations and can be important in
 *       tuning application performance.
 *
 * \note See H5Dcreate() for further cross-references.
 *
 * \since 1.0.0
 *
 */
H5_DLL herr_t H5Pset_fill_value(hid_t plist_id, hid_t type_id, const void *value);
/**
 *-------------------------------------------------------------------------
 *
 * \ingroup DCPL
 *
 * \brief Sets up use of the shuffle filter
 *
 * \dcpl_id{plist_id}
 *
 * \return \herr_t
 *
 * \details H5Pset_shuffle() sets the shuffle filter, #H5Z_FILTER_SHUFFLE,
 *          in the dataset creation property list \p plist_id. The shuffle
 *          filter de-interlaces a block of data by reordering the bytes.
 *          All the bytes from one consistent byte position of each data
 *          element are placed together in one block; all bytes from a
 *          second consistent byte position of each data element are placed
 *          together a second block; etc. For example, given three data
 *          elements of a 4-byte datatype stored as 012301230123, shuffling
 *          will re-order data as 000111222333. This can be a valuable step
 *          in an effective compression algorithm because the bytes in each
 *          byte position are often closely related to each other and
 *          putting them together can increase the compression ratio.
 *
 *          As implied above, the primary value of the shuffle filter lies
 *          in its coordinated use with a compression filter; it does not
 *          provide data compression when used alone. When the shuffle
 *          filter is applied to a dataset immediately prior to the use of
 *          a compression filter, the compression ratio achieved is often
 *          superior to that achieved by the use of a compression filter
 *          without the shuffle filter.
 *
 * \since 1.6.0
 *
 */
H5_DLL herr_t H5Pset_shuffle(hid_t plist_id);
/**
 *-------------------------------------------------------------------------
 * \ingroup DCPL
 *
 * \brief Sets the type of storage used to store the raw data for a dataset
 *
 * \dcpl_id{plist_id}
 * \param[in] layout Type of storage layout for raw data
 *
 * \return \herr_t
 * \details H5Pset_layout() sets the type of storage used to store the raw
 *          data for a dataset. This function is only valid for dataset
 *          creation property lists.
 *
 *          Valid values for \p layout are:
 *           - #H5D_COMPACT: Store raw data in the dataset object header
 *                           in file. This should only be used for datasets
 *                           with small amounts of raw data. The raw data
 *                           size limit is 64K (65520 bytes). Attempting
 *                           to create a dataset with raw data larger than
 *                           this limit will cause the H5Dcreate() call to
 *                           fail.
 *           - #H5D_CONTIGUOUS: Store raw data separately from the object
 *                              header in one large chunk in the file.
 *           - #H5D_CHUNKED: Store raw data separately from the object header
 *                           as chunks of data in separate locations in
 *                           the file.
 *           - #H5D_VIRTUAL: Draw raw data from multiple datasets in
 *                           different files.
 *
 *          Note that a compact storage layout may affect writing data to
 *          the dataset with parallel applications. See the note in
 *          H5Dwrite() documentation for details.
 * \version 1.10.0 #H5D_VIRTUAL added in this release.
 * \since 1.0.0
 *
 */
H5_DLL herr_t H5Pset_layout(hid_t plist_id, H5D_layout_t layout);
/**
 *-------------------------------------------------------------------------
 * \ingroup DCPL
 *
 * \brief Sets up use of the SZIP compression filter
 *
 * \dcpl_id{plist_id}
 * \param[in] options_mask A bit-mask conveying the desired SZIP options;
 *                         Valid values are #H5_SZIP_EC_OPTION_MASK and
 *                         #H5_SZIP_NN_OPTION_MASK.
 * \param[in] pixels_per_block The number of pixels or data elements in each
 *            data block
 *
 * \return \herr_t
 *
 * \details H5Pset_szip() sets an SZIP compression filter, #H5Z_FILTER_SZIP,
 *          for a dataset. SZIP is a compression method designed for use with
 *          scientific data.
 *
 *          Before proceeding, all users should review the “Limitations”
 *          section below.
 *
 *          Users familiar with SZIP outside the HDF5 context may benefit
 *          from reviewing the Note “For Users Familiar with SZIP in Other
 *          Contexts” below.
 *
 *          In the text below, the term pixel refers to an HDF5 data element.
 *          This terminology derives from SZIP compression's use with image
 *          data, where pixel referred to an image pixel.
 *
 *          The SZIP \p bits_per_pixel value (see Note, below) is automatically
 *          set, based on the HDF5 datatype. SZIP can be used with atomic
 *          datatypes that may have size of 8, 16, 32, or 64 bits.
 *          Specifically, a dataset with a datatype that is 8-, 16-, 32-, or
 *          64-bit signed or unsigned integer; char; or 32- or 64-bit float
 *          can be compressed with SZIP. See Note, below, for further
 *          discussion of the the SZIP \p bits_per_pixel setting.
 *
 *          SZIP options are passed in an options mask, \p options_mask,
 *          as follows.
 *
 *          <table>
 *            <tr>
 *             <th>Option</th>
 *             <th>Description (Mutually exclusive; select one.)</th>
 *            </tr>
 *            <tr>
 *             <td>#H5_SZIP_EC_OPTION_MASK</td>
 *             <td>Selects entropy coding method</td>
 *            </tr>
 *            <tr>
 *             <td>#H5_SZIP_NN_OPTION_MASK</td>
 *             <td>Selects nearest neighbor coding method</td>
 *            </tr>
 *           </table>
 *
 *           The following guidelines can be used in determining which
 *           option to select:
 *
 *           - The entropy coding method, the EC option specified by
 *             #H5_SZIP_EC_OPTION_MASK, is best suited for data that has been
 *             processed. The EC method works best for small numbers.
 *           - The nearest neighbor coding method, the NN option specified
 *             by #H5_SZIP_NN_OPTION_MASK, preprocesses the data then the
 *             applies EC method as above.
 *
 *           Other factors may affect results, but the above criteria
 *           provides a good starting point for optimizing data compression.
 *
 *           SZIP compresses data block by block, with a user-tunable block
 *           size. This block size is passed in the parameter
 *           \p pixels_per_block and must be even and not greater than 32,
 *           with typical values being 8, 10, 16, or 32. This parameter
 *           affects compression ratio; the more pixel values vary, the
 *           smaller this number should be to achieve better performance.
 *
 *           In HDF5, compression can be applied only to chunked datasets.
 *           If \p pixels_per_block is bigger than the total number of
 *           elements in a dataset chunk, H5Pset_szip() will succeed but
 *           the subsequent call to H5Dcreate() will fail; the conflict
 *           can be detected only when the property list is used.
 *
 *           To achieve optimal performance for SZIP compression, it is
 *           recommended that a chunk's fastest-changing dimension be equal
 *           to N times \p pixels_per_block where N is the maximum number of
 *           blocks per scan line allowed by the SZIP library. In the
 *           current version of SZIP, N is set to 128.
 *
 *           SZIP compression is an optional HDF5 filter.
 *
 *           \b Limitations:
 *
 *           - SZIP compression cannot be applied to compound, array,
 *             variable-length, enumeration, or any other user-defined
 *             datatypes. If an SZIP filter is set in a dataset creation
 *             property list used to create a dataset containing a
 *             non-allowed datatype, the call to H5Dcreate() will fail; the
 *             conflict can be detected only when the property list is used.
 *           - Users should be aware that there are factors that affect one’s
 *             rights and ability to use SZIP compression by reviewing the
 *             SZIP copyright notice.
 *
 * \note \b For \b Users \b Familiar \b with \b SZIP \b in \b Other \b Contexts:
 *
 * \note  The following notes are of interest primarily to those who have
 *        used SZIP compression outside of the HDF5 context.
 *        In non-HDF5 applications, SZIP typically requires that the user
 *        application supply additional parameters:
 *        - \p pixels_in_object, the number of pixels in the object to
 *          be compressed
 *        - \p bits_per_pixel, the number of bits per pixel
 *        - \p pixels_per_scanline, the number of pixels per scan line
 *
 * \note  These values need not be independently supplied in the HDF5
 *        environment as they are derived from the datatype and dataspace,
 *        which are already known. In particular, HDF5 sets
 *        \p pixels_in_object to the number of elements in a chunk and
 *        \p bits_per_pixel to the size of the element or pixel datatype.
 *
 * \note  The following algorithm is used to set \p pixels_per_scanline:
 *        - If the size of a chunk's fastest-changing dimension, size,
 *          is greater than 4K, set \p pixels_per_scanline to 128 times
 *          \p pixels_per_block.
 *        - If size is less than 4K but greater than \p pixels_per_block,
 *          set \p pixels_per_scanline to the minimum of size and 128
 *          times \p pixels_per_block.
 *        - If size is less than \p pixels_per_block but greater than the
 *          number elements in the chunk, set \p pixels_per_scanline to
 *          the minimum of the number elements in the chunk and 128 times
 *          \p pixels_per_block.
 *
 * \note  The HDF5 datatype may have precision that is less than the full
 *        size of the data element, e.g., an 11-bit integer can be defined
 *        using H5Tset_precision(). To a certain extent, SZIP can take
 *        advantage of the precision of the datatype to improve compression:
 *        - If the HDF5 datatype size is 24-bit or less and the offset of
 *          the bits in the HDF5 datatype is zero (see H5Tset_offset() or
 *          H5Tget_offset()), the data is the in lowest N bits of the data
 *          element. In this case, the SZIP \p bits_per_pixel is set to the
 *          precision of the HDF5 datatype.
 *        - If the offset is not zero, the SZIP \p bits_per_pixel will be
 *          set to the number of bits in the full size of the data element.
 *        - If the HDF5 datatype precision is 25-bit to 32-bit, the SZIP
 *          \p bits_per_pixel will be set to 32.
 *        - If the HDF5 datatype precision is 33-bit to 64-bit, the SZIP
 *          \p bits_per_pixel will be set to 64.
 *
 * \note HDF5 always modifies the options mask provided by the user to set up
 *       usage of RAW_OPTION_MASK, ALLOW_K13_OPTION_MASK, and one of
 *       LSB_OPTION_MASK or MSB_OPTION_MASK, depending on endianness of the
 *       datatype.
 *
 * \since 1.6.0
 *
 *--------------------------------------------------------------------------
 */
H5_DLL herr_t  H5Pset_szip(hid_t plist_id, unsigned options_mask, unsigned pixels_per_block);
H5_DLL herr_t  H5Pset_virtual(hid_t dcpl_id, hid_t vspace_id, const char *src_file_name,
                              const char *src_dset_name, hid_t src_space_id);
H5_DLL herr_t  H5Pget_virtual_count(hid_t dcpl_id, size_t *count /*out*/);
H5_DLL hid_t   H5Pget_virtual_vspace(hid_t dcpl_id, size_t index);
H5_DLL hid_t   H5Pget_virtual_srcspace(hid_t dcpl_id, size_t index);
H5_DLL ssize_t H5Pget_virtual_filename(hid_t dcpl_id, size_t index, char *name /*out*/, size_t size);
H5_DLL ssize_t H5Pget_virtual_dsetname(hid_t dcpl_id, size_t index, char *name /*out*/, size_t size);
H5_DLL herr_t  H5Pset_external(hid_t plist_id, const char *name, off_t offset, hsize_t size);
H5_DLL int     H5Pget_external_count(hid_t plist_id);
H5_DLL herr_t  H5Pget_external(hid_t plist_id, unsigned idx, size_t name_size, char *name /*out*/,
                               off_t *offset /*out*/, hsize_t *size /*out*/);
H5_DLL herr_t  H5Pset_nbit(hid_t plist_id);
H5_DLL herr_t  H5Pset_scaleoffset(hid_t plist_id, H5Z_SO_scale_type_t scale_type, int scale_factor);
H5_DLL herr_t  H5Pfill_value_defined(hid_t plist, H5D_fill_value_t *status);
H5_DLL herr_t  H5Pset_alloc_time(hid_t plist_id, H5D_alloc_time_t alloc_time);
H5_DLL herr_t  H5Pget_alloc_time(hid_t plist_id, H5D_alloc_time_t *alloc_time /*out*/);
H5_DLL herr_t  H5Pget_dset_no_attrs_hint(hid_t dcpl_id, hbool_t *minimize);
H5_DLL herr_t  H5Pset_dset_no_attrs_hint(hid_t dcpl_id, hbool_t minimize);

/* Dataset access property list (DAPL) routines */
H5_DLL herr_t  H5Pset_chunk_cache(hid_t dapl_id, size_t rdcc_nslots, size_t rdcc_nbytes, double rdcc_w0);
H5_DLL herr_t  H5Pget_chunk_cache(hid_t dapl_id, size_t *rdcc_nslots /*out*/, size_t *rdcc_nbytes /*out*/,
                                  double *rdcc_w0 /*out*/);
H5_DLL herr_t  H5Pset_virtual_view(hid_t plist_id, H5D_vds_view_t view);
H5_DLL herr_t  H5Pget_virtual_view(hid_t plist_id, H5D_vds_view_t *view);
H5_DLL herr_t  H5Pset_virtual_printf_gap(hid_t plist_id, hsize_t gap_size);
H5_DLL herr_t  H5Pget_virtual_printf_gap(hid_t plist_id, hsize_t *gap_size);
H5_DLL herr_t  H5Pset_virtual_prefix(hid_t dapl_id, const char *prefix);
H5_DLL ssize_t H5Pget_virtual_prefix(hid_t dapl_id, char *prefix /*out*/, size_t size);
H5_DLL herr_t  H5Pset_append_flush(hid_t plist_id, unsigned ndims, const hsize_t boundary[],
                                   H5D_append_cb_t func, void *udata);
H5_DLL herr_t  H5Pget_append_flush(hid_t plist_id, unsigned dims, hsize_t boundary[], H5D_append_cb_t *func,
                                   void **udata);
H5_DLL herr_t  H5Pset_efile_prefix(hid_t dapl_id, const char *prefix);
H5_DLL ssize_t H5Pget_efile_prefix(hid_t dapl_id, char *prefix /*out*/, size_t size);

/* Dataset xfer property list (DXPL) routines */
H5_DLL herr_t    H5Pset_data_transform(hid_t plist_id, const char *expression);
H5_DLL ssize_t   H5Pget_data_transform(hid_t plist_id, char *expression /*out*/, size_t size);
H5_DLL herr_t    H5Pset_buffer(hid_t plist_id, size_t size, void *tconv, void *bkg);
H5_DLL size_t    H5Pget_buffer(hid_t plist_id, void **tconv /*out*/, void **bkg /*out*/);
H5_DLL herr_t    H5Pset_preserve(hid_t plist_id, hbool_t status);
H5_DLL int       H5Pget_preserve(hid_t plist_id);
H5_DLL herr_t    H5Pset_edc_check(hid_t plist_id, H5Z_EDC_t check);
H5_DLL H5Z_EDC_t H5Pget_edc_check(hid_t plist_id);
H5_DLL herr_t    H5Pset_filter_callback(hid_t plist_id, H5Z_filter_func_t func, void *op_data);
H5_DLL herr_t    H5Pset_btree_ratios(hid_t plist_id, double left, double middle, double right);
H5_DLL herr_t    H5Pget_btree_ratios(hid_t plist_id, double *left /*out*/, double *middle /*out*/,
                                     double *right /*out*/);
H5_DLL herr_t    H5Pset_vlen_mem_manager(hid_t plist_id, H5MM_allocate_t alloc_func, void *alloc_info,
                                         H5MM_free_t free_func, void *free_info);
H5_DLL herr_t    H5Pget_vlen_mem_manager(hid_t plist_id, H5MM_allocate_t *alloc_func, void **alloc_info,
                                         H5MM_free_t *free_func, void **free_info);
H5_DLL herr_t    H5Pset_hyper_vector_size(hid_t fapl_id, size_t size);
H5_DLL herr_t    H5Pget_hyper_vector_size(hid_t fapl_id, size_t *size /*out*/);
H5_DLL herr_t    H5Pset_type_conv_cb(hid_t dxpl_id, H5T_conv_except_func_t op, void *operate_data);
H5_DLL herr_t    H5Pget_type_conv_cb(hid_t dxpl_id, H5T_conv_except_func_t *op, void **operate_data);
#ifdef H5_HAVE_PARALLEL
H5_DLL herr_t H5Pget_mpio_actual_chunk_opt_mode(hid_t                             plist_id,
                                                H5D_mpio_actual_chunk_opt_mode_t *actual_chunk_opt_mode);
H5_DLL herr_t H5Pget_mpio_actual_io_mode(hid_t plist_id, H5D_mpio_actual_io_mode_t *actual_io_mode);
H5_DLL herr_t H5Pget_mpio_no_collective_cause(hid_t plist_id, uint32_t *local_no_collective_cause,
                                              uint32_t *global_no_collective_cause);
#endif /* H5_HAVE_PARALLEL */

/* Link creation property list (LCPL) routines */
H5_DLL herr_t H5Pset_create_intermediate_group(hid_t plist_id, unsigned crt_intmd);
H5_DLL herr_t H5Pget_create_intermediate_group(hid_t plist_id, unsigned *crt_intmd /*out*/);

/* Group creation property list (GCPL) routines */
H5_DLL herr_t H5Pset_local_heap_size_hint(hid_t plist_id, size_t size_hint);
H5_DLL herr_t H5Pget_local_heap_size_hint(hid_t plist_id, size_t *size_hint /*out*/);
H5_DLL herr_t H5Pset_link_phase_change(hid_t plist_id, unsigned max_compact, unsigned min_dense);
H5_DLL herr_t H5Pget_link_phase_change(hid_t plist_id, unsigned *max_compact /*out*/,
                                       unsigned *min_dense /*out*/);
H5_DLL herr_t H5Pset_est_link_info(hid_t plist_id, unsigned est_num_entries, unsigned est_name_len);
H5_DLL herr_t H5Pget_est_link_info(hid_t plist_id, unsigned *est_num_entries /* out */,
                                   unsigned *est_name_len /* out */);
H5_DLL herr_t H5Pset_link_creation_order(hid_t plist_id, unsigned crt_order_flags);
H5_DLL herr_t H5Pget_link_creation_order(hid_t plist_id, unsigned *crt_order_flags /* out */);

/* Map access property list (MAPL) routines */
#ifdef H5_HAVE_MAP_API
H5_DLL herr_t H5Pset_map_iterate_hints(hid_t mapl_id, size_t key_prefetch_size, size_t key_alloc_size);
H5_DLL herr_t H5Pget_map_iterate_hints(hid_t mapl_id, size_t *key_prefetch_size /*out*/,
                                       size_t *key_alloc_size /*out*/);
#endif /*  H5_HAVE_MAP_API */

/* String creation property list (STRCPL) routines */
H5_DLL herr_t H5Pset_char_encoding(hid_t plist_id, H5T_cset_t encoding);
H5_DLL herr_t H5Pget_char_encoding(hid_t plist_id, H5T_cset_t *encoding /*out*/);

/* Link access property list (LAPL) routines */
H5_DLL herr_t  H5Pset_nlinks(hid_t plist_id, size_t nlinks);
H5_DLL herr_t  H5Pget_nlinks(hid_t plist_id, size_t *nlinks);
H5_DLL herr_t  H5Pset_elink_prefix(hid_t plist_id, const char *prefix);
H5_DLL ssize_t H5Pget_elink_prefix(hid_t plist_id, char *prefix, size_t size);
H5_DLL hid_t   H5Pget_elink_fapl(hid_t lapl_id);
H5_DLL herr_t  H5Pset_elink_fapl(hid_t lapl_id, hid_t fapl_id);
H5_DLL herr_t  H5Pset_elink_acc_flags(hid_t lapl_id, unsigned flags);
H5_DLL herr_t  H5Pget_elink_acc_flags(hid_t lapl_id, unsigned *flags);
H5_DLL herr_t  H5Pset_elink_cb(hid_t lapl_id, H5L_elink_traverse_t func, void *op_data);
H5_DLL herr_t  H5Pget_elink_cb(hid_t lapl_id, H5L_elink_traverse_t *func, void **op_data);

/* Object copy property list (OCPYPL) routines */
H5_DLL herr_t H5Pset_copy_object(hid_t plist_id, unsigned crt_intmd);
H5_DLL herr_t H5Pget_copy_object(hid_t plist_id, unsigned *crt_intmd /*out*/);
H5_DLL herr_t H5Padd_merge_committed_dtype_path(hid_t plist_id, const char *path);
H5_DLL herr_t H5Pfree_merge_committed_dtype_paths(hid_t plist_id);
H5_DLL herr_t H5Pset_mcdt_search_cb(hid_t plist_id, H5O_mcdt_search_cb_t func, void *op_data);
H5_DLL herr_t H5Pget_mcdt_search_cb(hid_t plist_id, H5O_mcdt_search_cb_t *func, void **op_data);

/* Symbols defined for compatibility with previous versions of the HDF5 API.
 *
 * Use of these symbols is deprecated.
 */
#ifndef H5_NO_DEPRECATED_SYMBOLS

/* Macros */

/* We renamed the "root" of the property list class hierarchy */
#define H5P_NO_CLASS H5P_ROOT

/* Typedefs */

/* Function prototypes */
H5_DLL herr_t H5Pregister1(hid_t cls_id, const char *name, size_t size, void *def_value,
                           H5P_prp_create_func_t prp_create, H5P_prp_set_func_t prp_set,
                           H5P_prp_get_func_t prp_get, H5P_prp_delete_func_t prp_del,
                           H5P_prp_copy_func_t prp_copy, H5P_prp_close_func_t prp_close);
H5_DLL herr_t H5Pinsert1(hid_t plist_id, const char *name, size_t size, void *value,
                         H5P_prp_set_func_t prp_set, H5P_prp_get_func_t prp_get,
                         H5P_prp_delete_func_t prp_delete, H5P_prp_copy_func_t prp_copy,
                         H5P_prp_close_func_t prp_close);
H5_DLL herr_t H5Pencode1(hid_t plist_id, void *buf, size_t *nalloc);
/**
 *-------------------------------------------------------------------------
 * \ingroup OCPL
 *
 * \brief Returns information about a filter in a pipeline (DEPRECATED)
 *
 * \todo H5Pget_filter1() prototype does not match source in H5Pocpl.c.
 *       Also, it is not in a deprecated file. Is that okay?
 *
 * \plist_id{plist_id}
 * \param[in] filter Sequence number within the filter pipeline of the filter
 *                   for which information is sought
 * \param[out] flags Bit vector specifying certain general properties of
 *                the filter
 * \param[in,out] cd_nelmts Number of elements in \p cd_values
 * \param[out] cd_values Auxiliary data for the filter
 * \param[in] namelen Anticipated number of characters in \p name
 * \param[out] name Name of the filter
 *
 * \return Returns the filter identifier if successful;  Otherwise returns
 *         a negative value. See: #H5Z_filter_t
 *
 * \details H5Pget_filter1() returns information about a filter, specified
 *          by its filter number, in a filter pipeline, specified by the
 *          property list with which it is associated.
 *
 *          \p plist_id must be a dataset or group creation property list.
 *
 *          \p filter is a value between zero and N-1, as described in
 *          H5Pget_nfilters(). The function will return a negative value
 *          if the filter number is out of range.
 *
 *          The structure of the \p flags argument is discussed in
 *          H5Pset_filter().
 *
 *          On input, \p cd_nelmts indicates the number of entries in the
 *          \p cd_values array, as allocated by the caller; on return,
 *          \p cd_nelmts contains the number of values defined by the filter.
 *
 *          If \p name is a pointer to an array of at least \p namelen
 *          bytes, the filter name will be copied into that array. The name
 *          will be null terminated if \p namelen is large enough. The
 *          filter name returned will be the name appearing in the file, the
 *          name registered for the filter, or an empty string.
 *
 * \version 1.8.5 Function extended to work with group creation property
 *                lists.
 * \version 1.8.0 N-bit and scale-offset filters added.
 * \version 1.8.0 Function H5Pget_filter() renamed to H5Pget_filter1() and
 *                deprecated in this release.
 * \version 1.6.4 \p filter parameter type changed to unsigned.
 *
 */
H5_DLL H5Z_filter_t H5Pget_filter1(hid_t plist_id, unsigned filter, unsigned int *flags /*out*/,
                                   size_t *cd_nelmts /*out*/, unsigned cd_values[] /*out*/, size_t namelen,
                                   char name[]);
H5_DLL herr_t       H5Pget_filter_by_id1(hid_t plist_id, H5Z_filter_t id, unsigned int *flags /*out*/,
                                         size_t *cd_nelmts /*out*/, unsigned cd_values[] /*out*/, size_t namelen,
                                         char name[] /*out*/);
H5_DLL herr_t       H5Pget_version(hid_t plist_id, unsigned *boot /*out*/, unsigned *freelist /*out*/,
                                   unsigned *stab /*out*/, unsigned *shhdr /*out*/);
H5_DLL herr_t       H5Pset_file_space(hid_t plist_id, H5F_file_space_type_t strategy, hsize_t threshold);
H5_DLL herr_t       H5Pget_file_space(hid_t plist_id, H5F_file_space_type_t *strategy, hsize_t *threshold);
#endif /* H5_NO_DEPRECATED_SYMBOLS */

#ifdef __cplusplus
}
#endif
#endif /* H5Ppublic_H */
