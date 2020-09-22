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
 * This file contains function prototypes for each exported function in the
 * H5P module.
 */
#ifndef _H5Ppublic_H
#define _H5Ppublic_H

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
#ifndef _H5private_H
#define H5OPEN        H5open(),
#else   /* _H5private_H */
#define H5OPEN
#endif  /* _H5private_H */

/*
 * The library's property list classes
 */

#define H5P_ROOT                (H5OPEN H5P_CLS_ROOT_ID_g)
#define H5P_OBJECT_CREATE       (H5OPEN H5P_CLS_OBJECT_CREATE_ID_g)
#define H5P_FILE_CREATE         (H5OPEN H5P_CLS_FILE_CREATE_ID_g)
#define H5P_FILE_ACCESS         (H5OPEN H5P_CLS_FILE_ACCESS_ID_g)
#define H5P_DATASET_CREATE      (H5OPEN H5P_CLS_DATASET_CREATE_ID_g)
#define H5P_DATASET_ACCESS      (H5OPEN H5P_CLS_DATASET_ACCESS_ID_g)
#define H5P_DATASET_XFER        (H5OPEN H5P_CLS_DATASET_XFER_ID_g)
#define H5P_FILE_MOUNT          (H5OPEN H5P_CLS_FILE_MOUNT_ID_g)
#define H5P_GROUP_CREATE        (H5OPEN H5P_CLS_GROUP_CREATE_ID_g)
#define H5P_GROUP_ACCESS        (H5OPEN H5P_CLS_GROUP_ACCESS_ID_g)
#define H5P_DATATYPE_CREATE     (H5OPEN H5P_CLS_DATATYPE_CREATE_ID_g)
#define H5P_DATATYPE_ACCESS     (H5OPEN H5P_CLS_DATATYPE_ACCESS_ID_g)
#define H5P_MAP_CREATE          (H5OPEN H5P_CLS_MAP_CREATE_ID_g)
#define H5P_MAP_ACCESS          (H5OPEN H5P_CLS_MAP_ACCESS_ID_g)
#define H5P_STRING_CREATE       (H5OPEN H5P_CLS_STRING_CREATE_ID_g)
#define H5P_ATTRIBUTE_CREATE    (H5OPEN H5P_CLS_ATTRIBUTE_CREATE_ID_g)
#define H5P_ATTRIBUTE_ACCESS    (H5OPEN H5P_CLS_ATTRIBUTE_ACCESS_ID_g)
#define H5P_OBJECT_COPY         (H5OPEN H5P_CLS_OBJECT_COPY_ID_g)
#define H5P_LINK_CREATE         (H5OPEN H5P_CLS_LINK_CREATE_ID_g)
#define H5P_LINK_ACCESS         (H5OPEN H5P_CLS_LINK_ACCESS_ID_g)
#define H5P_VOL_INITIALIZE      (H5OPEN H5P_CLS_VOL_INITIALIZE_ID_g)
#define H5P_REFERENCE_ACCESS    (H5OPEN H5P_CLS_REFERENCE_ACCESS_ID_g)

/*
 * The library's default property lists
 */
#define H5P_FILE_CREATE_DEFAULT        (H5OPEN H5P_LST_FILE_CREATE_ID_g)
#define H5P_FILE_ACCESS_DEFAULT        (H5OPEN H5P_LST_FILE_ACCESS_ID_g)
#define H5P_DATASET_CREATE_DEFAULT     (H5OPEN H5P_LST_DATASET_CREATE_ID_g)
#define H5P_DATASET_ACCESS_DEFAULT     (H5OPEN H5P_LST_DATASET_ACCESS_ID_g)
#define H5P_DATASET_XFER_DEFAULT       (H5OPEN H5P_LST_DATASET_XFER_ID_g)
#define H5P_FILE_MOUNT_DEFAULT         (H5OPEN H5P_LST_FILE_MOUNT_ID_g)
#define H5P_GROUP_CREATE_DEFAULT       (H5OPEN H5P_LST_GROUP_CREATE_ID_g)
#define H5P_GROUP_ACCESS_DEFAULT       (H5OPEN H5P_LST_GROUP_ACCESS_ID_g)
#define H5P_DATATYPE_CREATE_DEFAULT    (H5OPEN H5P_LST_DATATYPE_CREATE_ID_g)
#define H5P_DATATYPE_ACCESS_DEFAULT    (H5OPEN H5P_LST_DATATYPE_ACCESS_ID_g)
#define H5P_MAP_CREATE_DEFAULT         (H5OPEN H5P_LST_MAP_CREATE_ID_g)
#define H5P_MAP_ACCESS_DEFAULT         (H5OPEN H5P_LST_MAP_ACCESS_ID_g)
#define H5P_ATTRIBUTE_CREATE_DEFAULT   (H5OPEN H5P_LST_ATTRIBUTE_CREATE_ID_g)
#define H5P_ATTRIBUTE_ACCESS_DEFAULT   (H5OPEN H5P_LST_ATTRIBUTE_ACCESS_ID_g)
#define H5P_OBJECT_COPY_DEFAULT        (H5OPEN H5P_LST_OBJECT_COPY_ID_g)
#define H5P_LINK_CREATE_DEFAULT        (H5OPEN H5P_LST_LINK_CREATE_ID_g)
#define H5P_LINK_ACCESS_DEFAULT        (H5OPEN H5P_LST_LINK_ACCESS_ID_g)
#define H5P_VOL_INITIALIZE_DEFAULT     (H5OPEN H5P_LST_VOL_INITIALIZE_ID_g)
#define H5P_REFERENCE_ACCESS_DEFAULT   (H5OPEN H5P_LST_REFERENCE_ACCESS_ID_g)

/* Common creation order flags (for links in groups and attributes on objects) */
#define H5P_CRT_ORDER_TRACKED           0x0001
#define H5P_CRT_ORDER_INDEXED           0x0002

/* Default value for all property list classes */
#define H5P_DEFAULT     (hid_t)0

#ifdef __cplusplus
extern "C" {
#endif

/*******************/
/* Public Typedefs */
/*******************/


/* Define property list class callback function pointer types */
//! [H5P_cls_create_func_t_snip]
typedef herr_t (*H5P_cls_create_func_t)(hid_t prop_id, void *create_data);
//! [H5P_cls_create_func_t_snip]

//! [H5P_cls_copy_func_t_snip]
typedef herr_t (*H5P_cls_copy_func_t)(hid_t new_prop_id, hid_t old_prop_id,
                                      void *copy_data);
//! [H5P_cls_copy_func_t_snip]

//! [H5P_cls_close_func_t_snip]
typedef herr_t (*H5P_cls_close_func_t)(hid_t prop_id, void *close_data);
//! [H5P_cls_close_func_t_snip]

/* Define property list callback function pointer types */
//! [H5P_prp_cb1_t_snip]
typedef herr_t (*H5P_prp_cb1_t)(const char *name, size_t size, void *value);
//! [H5P_prp_cb1_t_snip]

//! [H5P_prp_cb2_t_snip]
typedef herr_t (*H5P_prp_cb2_t)(hid_t prop_id, const char *name, size_t size, void *value);
//! [H5P_prp_cb2_t_snip]

typedef H5P_prp_cb1_t H5P_prp_create_func_t;
typedef H5P_prp_cb2_t H5P_prp_set_func_t;
typedef H5P_prp_cb2_t H5P_prp_get_func_t;
typedef herr_t (*H5P_prp_encode_func_t)(const void *value, void **buf, size_t *size);
typedef herr_t (*H5P_prp_decode_func_t)(const void **buf, void *value);
typedef H5P_prp_cb2_t H5P_prp_delete_func_t;
typedef H5P_prp_cb1_t H5P_prp_copy_func_t;
//! [H5P_prp_compare_func_t_snip]
typedef int (*H5P_prp_compare_func_t)(const void *value1, const void *value2, size_t size);
//! [H5P_prp_compare_func_t_snip]
typedef H5P_prp_cb1_t H5P_prp_close_func_t;

/* Define property list iteration function type */
//! [H5P_iterate_t_snip]
typedef herr_t (*H5P_iterate_t)(hid_t id, const char *name, void *iter_data);
//! [H5P_iterate_t_snip]

/* Actual IO mode property */
typedef enum H5D_mpio_actual_chunk_opt_mode_t {
    /* The default value, H5D_MPIO_NO_CHUNK_OPTIMIZATION, is used for all I/O
     * operations that do not use chunk optimizations, including non-collective
     * I/O and contiguous collective I/O.
     */
    H5D_MPIO_NO_CHUNK_OPTIMIZATION = 0,
    H5D_MPIO_LINK_CHUNK,
    H5D_MPIO_MULTI_CHUNK
}  H5D_mpio_actual_chunk_opt_mode_t;

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
    H5D_MPIO_NO_COLLECTIVE = 0x0,
    H5D_MPIO_CHUNK_INDEPENDENT = 0x1,
    H5D_MPIO_CHUNK_COLLECTIVE = 0x2,
    H5D_MPIO_CHUNK_MIXED = 0x1 | 0x2,

    /* The contiguous case is separate from the bit field. */
    H5D_MPIO_CONTIGUOUS_COLLECTIVE = 0x4
} H5D_mpio_actual_io_mode_t;

/* Broken collective IO property */
typedef enum H5D_mpio_no_collective_cause_t {
    H5D_MPIO_COLLECTIVE = 0x00,
    H5D_MPIO_SET_INDEPENDENT = 0x01,
    H5D_MPIO_DATATYPE_CONVERSION = 0x02,
    H5D_MPIO_DATA_TRANSFORMS = 0x04,
    H5D_MPIO_MPI_OPT_TYPES_ENV_VAR_DISABLED = 0x08,
    H5D_MPIO_NOT_SIMPLE_OR_SCALAR_DATASPACES = 0x10,
    H5D_MPIO_NOT_CONTIGUOUS_OR_CHUNKED_DATASET = 0x20,
    H5D_MPIO_PARALLEL_FILTERED_WRITES_DISABLED = 0x40,
    H5D_MPIO_ERROR_WHILE_CHECKING_COLLECTIVE_POSSIBLE = 0x80,
    H5D_MPIO_NO_COLLECTIVE_MAX_CAUSE = 0x100
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
 *-------------------------------------------------------------------------
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
 * -------------------------------------------------------------------------
 */
H5_DLL herr_t H5Pclose(hid_t plist_id);
/**
 *--------------------------------------------------------------------------
 * \ingroup GPLOA
 *
 * \brief Closes an existing property list class
 *
 * \plistcls_id{class}
 *
 * \return \herr_t
 *
 * \details H5Pclose_class() removes a property list class from the library.
 *          Existing property lists of this class will continue to exist,
 *          but new ones are not able to be created.
 *
 * \since 1.4.0
 *
 */
H5_DLL herr_t H5Pclose_class(hid_t class);
/**
 *-------------------------------------------------------------------------
 * \ingroup GPLO
 *
 * \brief Copies an existing property list to create a new property list
 *
 * \plist_id
 *
 * \return \hid_t{property list}
 *
 * \details H5Pcopy() copies an existing property list to create a new
 *          property list. The new property list has the same properties
 *          and values as the original property list.
 *
 * \since 1.0.0
 *
 */
H5_DLL hid_t H5Pcopy(hid_t plist_id);
/**
 *-------------------------------------------------------------------------
 * \ingroup GPLOA
 *
 * \brief Copies a property from one list or class to another
 *
 * \param[in] dst_id Identifier of the destination property list or class
 * \param[in] src_id Identifier of the source property list or class
 * \param[in] name Name of the property to copy
 *
 * \return \herr_t
 *
 * \details H5Pcopy_prop() copies a property from one property list or
 *          class to another.
 *
 *          If a property is copied from one class to another, all the
 *          property information will be first deleted from the destination
 *          class and then the property information will be copied from the
 *          source class into the destination class.
 *
 *          If a property is copied from one list to another, the property
 *          will be first deleted from the destination list (generating a
 *          call to the close callback for the property, if one exists)
 *          and then the property is copied from the source list to the
 *          destination list (generating a call to the copy callback for
 *          the property, if one exists).
 *
 *          If the property does not exist in the class or list, this
 *          call is equivalent to calling H5Pregister() or H5Pinsert() (for
 *          a class or list, as appropriate) and the create callback will
 *          be called in the case of the property being copied into a list
 *          (if such a callback exists for the property).
 *
 * \since 1.6.0
 *
 */
H5_DLL herr_t H5Pcopy_prop(hid_t dst_id, hid_t src_id, const
char *name);
/**
 *-------------------------------------------------------------------------
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
 * --------------------------------------------------------------------------
 */
H5_DLL hid_t H5Pcreate(hid_t cls_id);
/**
 *-------------------------------------------------------------------------
 * \ingroup GPLOA
 *
 * \brief Creates a new property list class
 *
 * \plistcls_id{parent}
 * \param[in] name        Name of property list class to register
 * \param[in] create      Callback routine called when a property list is
 *                        created
 * \param[in] create_data Pointer to user-defined class create data, to be
 *                        passed along to class create callback
 * \param[in] copy        Callback routine called when a property list is
 *                        copied
 * \param[in] copy_data   Pointer to user-defined class copy data, to be
 *                        passed along to class copy callback
 * \param[in] close       Callback routine called when a property list is
 *                        being closed
 * \param[in] close_data  Pointer to user-defined class close data, to be
 *                        passed along to class close callback
 *
 * \return \hid_t{property list class}
 *
 * \details H5Pcreate_class() registers a new property list class with the
 *          library. The new property list class can inherit from an
 *          existing property list class, \p parent, or may be derived
 *          from the default “empty” class, NULL. New classes with
 *          inherited properties from existing classes may not remove
 *          those existing properties, only add or remove their own class
 *          properties. Property list classes defined and supported in the
 *          HDF5 library distribution are listed and briefly described in
 *          H5Pcreate(). The \p create routine is called when a new property
 *          list of this class is being created. The #H5P_cls_create_func_t
 *          callback function is defined as follows:
 *
 *          \todo fix snippets to work, when you click on them.
 *
 *          \snippet this H5P_cls_create_func_t_snip
 *
 *          The parameters to this callback function are defined as follows:
 *          <table>
 *            <tr>
 *              <td>\ref hid_t \c prop_id</td>
 *              <td>IN: The identifier of the property list being created</td>
 *            </tr>
 *            <tr>
 *              <td>\Code{void * create_data}</td>
 *              <td>IN: User pointer to any class creation data required</td>
 *            </tr>
 *          </table>
 *
 *          The \p create routine is called after any registered
 *          \p create function is called for each property value. If the
 *          \p create routine returns a negative value, the new list is not
 *          returned to the user and the property list creation routine returns
 *          an error value.
 *
 *          The \p copy routine is called when an existing property
 *          list of this class is copied. The #H5P_cls_copy_func_t callback
 *          function is defined as follows:
 *          \snippet this H5P_cls_copy_func_t_snip
 *
 *          The parameters to this callback function are defined as follows:
 *          <table>
 *            <tr>
 *              <td>\ref hid_t \c prop_id</td>
 *              <td>IN: The identifier of the property list created by copying</td>
 *            </tr>
 *            <tr>
 *              <td>\Code{void * copy_data}</td>
 *              <td>IN: User pointer to any class copy data required</td>
 *            </tr>
 *          </table>
 *
 *          The \p copy routine is called after any registered \p copy function
 *          is called for each property value. If the \p copy routine returns a
 *          negative value, the new list is not returned to the user and the
 *          property list \p copy routine returns an error value.
 *
 *           The \p close routine is called when a property list of this class
 *           is being closed. The #H5P_cls_close_func_t callback function is
 *           defined as follows:
 *           \snippet this H5P_cls_close_func_t_snip
 *
 *           The parameters to this callback function are defined as follows:
 *           <table>
 *            <tr>
 *              <td>\ref hid_t \c prop_id</td>
 *              <td>IN: The identifier of the property list being closed</td>
 *            </tr>
 *            <tr>
 *              <td>\Code{void * close_data}</td>
 *              <td>IN: User pointer to any class close data required</td>
 *            </tr>
 *          </table>
 *
 *          The \p close routine is called before any registered \p close
 *          function is called for each property value. If the \p close routine
 *          returns a negative value, the property list close routine returns
 *          an error value but the property list is still closed.
 *
 *          H5Pclose_class() can be used to release the property list class
 *          identifier returned by this function so that resources leaks will
 *          not develop.
 *
 * \since 1.4.0
 *
 */
H5_DLL hid_t H5Pcreate_class(hid_t parent, const char *name,
    H5P_cls_create_func_t create, void *create_data,
    H5P_cls_copy_func_t copy, void *copy_data,
    H5P_cls_close_func_t close, void *close_data);
/**
 *-------------------------------------------------------------------------
 * \ingroup GPLO
 *
 * \brief Decodes property list received in a binary object buffer and
 *        returns a new property list identifier
 *
 * \param[in] buf Buffer holding the encoded property list
 *
 * \return \hid_tv{object}
 *
 * \details Given a binary property list description in a buffer, H5Pdecode()
 *          reconstructs the HDF5 property list and returns an identifier
 *          for the new property list. The binary description of the property
 *          list is encoded by H5Pencode().
 *
 *          The user is responsible for passing in the correct buffer.
 *
 *          The property list identifier returned by this function should be
 *          released with H5Pclose() when the identifier is no longer needed
 *          so that resource leaks will not develop.
 *
 * \note Some properties cannot be encoded and therefore will not be available
 *       in the decoded property list. These properties are discussed in
 *       H5Pencode().
 *
 * \since 1.10.0
 *
 */
H5_DLL hid_t  H5Pdecode(const void *buf);
/**
 *-------------------------------------------------------------------------
 * \ingroup GPLO
 *
 * \brief Encodes the property values in a property list into a binary
 *        buffer
 *
 * \plist_id
 * \param[out] buf    Buffer into which the property list will be encoded.
 *                    If the provided buffer is NULL, the size of the
 *                    buffer required is returned through \p nalloc; the
 *                    function does nothing more.
 * \param[out] nalloc The size of the required buffer
 * \fapl_id
 *
 * \return \herr_t
 *
 * \details H5Pencode2() encodes the property list \p plist_id into the
 *          binary buffer \p buf, according to the file format setting
 *          specified by the file access property list \p fapl_id.
 *
 *          If the required buffer size is unknown, \p buf can be passed
 *          in as NULL and the function will set the required buffer size
 *          in \p nalloc. The buffer can then be created and the property
 *          list encoded with a subsequent H5Pencode2() call.
 *
 *          If the buffer passed in is not big enough to hold the encoded
 *          properties, the H5Pencode2() call can be expected to fail with
 *          a segmentation fault.
 *
 *          The file access property list \p fapl_id is used to
 *          control the encoding via the \a libver_bounds property
 *          (see H5Pset_libver_bounds()). If the \a libver_bounds
 *          property is missing, H5Pencode2() proceeds as if the \a
 *          libver_bounds property were set to (#H5F_LIBVER_EARLIEST,
 *          #H5F_LIBVER_LATEST). (Functionally, H5Pencode1() is identical to
 *          H5Pencode2() with \a libver_bounds set to (#H5F_LIBVER_EARLIEST,
 *          #H5F_LIBVER_LATEST).)
 *          Properties that do not have encode callbacks will be skipped.
 *          There is currently no mechanism to register an encode callback for
 *          a user-defined property, so user-defined properties cannot currently
 *          be encoded.
 *
 *          Some properties cannot be encoded, particularly properties that are
 *          reliant on local context.
 *
 * \note \a Motivation:
 *       This function was introduced in HDF5-1.12 as part of the \a H5Sencode
 *       format change to enable 64-bit selection encodings and a dataspace
 *       selection that is tied to a file.
 *
 * \since 1.12.0
 *
 */
H5_DLL herr_t H5Pencode2(hid_t plist_id, void *buf, size_t *nalloc, hid_t fapl_id);
/**
 *-------------------------------------------------------------------------
 * \ingroup GPLOA
 *
 * \brief Compares two property lists or classes for equality
 *
 * \param[in] id1 First property object to be compared
 * \param[in] id2 Second property object to be compared
 *
 * \return \htri_t
 *
 * \details H5Pequal() compares two property lists or classes to determine
 *          whether they are equal to one another.
 *
 *          Either both \p id1 and \p id2 must be property lists or both
 *          must be classes; comparing a list to a class is an error.
 *
 * \since 1.4.0
 *
 */
H5_DLL htri_t H5Pequal(hid_t id1, hid_t id2);
/**
 *-------------------------------------------------------------------------
 * \ingroup GPLOA
 *
 * \brief Queries whether a property name exists in a property list or
 *       class
 *
 * \param[in] plist_id   Identifier for the property list or class to query
 * \param[in] name       Name of property to check for
 *
 * \return \htri_t
 *
 * \details  H5Pexist() determines whether a property exists within a
 *           property list or class.
 *
 * \since 1.4.0
 *
 */
H5_DLL htri_t H5Pexist(hid_t plist_id, const char *name);
/**
 *-------------------------------------------------------------------------
 * \ingroup GPLOA
 *
 * \brief Queries the value of a property
 *
 * \plist_id
 * \param[in]  name  Name of property to query
 * \param[out] value Pointer to a location to which to copy the value of
 *                   the property
 *
 * \return \herr_t
 *
 * \details H5Pget() retrieves a copy of the value for a property in a
 *          property list. If there is a \p get callback routine registered
 *          for this property, the copy of the value of the property will
 *          first be passed to that routine and any changes to the copy of
 *          the value will be used when returning the property value from
 *          this routine.
 *
 *          This routine may be called for zero-sized properties with the
 *          \p value set to NULL. The \p get routine will be called with
 *          a NULL value if the callback exists.
 *
 *          The property name must exist or this routine will fail.
 *
 *          If the \p get callback routine returns an error, \ value will
 *          not be modified.
 *
 * \since 1.4.0
 *
 */
H5_DLL herr_t H5Pget(hid_t plist_id, const char *name, void * value);
/**
 *-------------------------------------------------------------------------
 *\ingroup GPLO
 *
 * \brief Returns the property list class identifier for a property list
 *
 * \plist_id
 *
 * \return \hid_t{property list class}
 *
 * \details H5Pget_class() returns the property list class identifier for
 *          the property list identified by the \p plist_id parameter.
 *
 *          Note that H5Pget_class() returns a value of #hid_t type, an
 *          internal HDF5 identifier, rather than directly returning a
 *          property list class. That identifier can then be used with
 *          either H5Pequal() or H5Pget_class_name() to determine which
 *          predefined HDF5 property list class H5Pget_class() has returned.
 *
 *          A full list of valid predefined property list classes appears
 *          in the description of H5Pcreate().
 *
 *          Determining the HDF5 property list class name with H5Pequal()
 *          requires a series of H5Pequal() calls in an if-else sequence.
 *          An iterative sequence of H5Pequal() calls can compare the
 *          identifier returned by H5Pget_class() to members of the list of
 *          valid property list class names. A pseudo-code snippet might
 *          read as follows:
 *
 *          \code
 *          plist_class_id = H5Pget_class (dsetA_plist);
 *
 *          if H5Pequal (plist_class_id, H5P_OBJECT_CREATE) = TRUE;
 *              [ H5P_OBJECT_CREATE is the property list class    ]
 *              [ returned by H5Pget_class.                        ]
 *
 *          else if H5Pequal (plist_class_id, H5P_DATASET_CREATE) = TRUE;
 *              [ H5P_DATASET_CREATE is the property list class.  ]
 *
 *          else if H5Pequal (plist_class_id, H5P_DATASET_XFER) = TRUE;
 *              [ H5P_DATASET_XFER is the property list class.    ]
 *
 *          .
 *          .   [ Continuing the iteration until a match is found. ]
 *          .
 *          \endcode
 *
 *          H5Pget_class_name() returns the property list class name directly
 *          as a string:
 *
 *          \code
 *          plist_class_id = H5Pget_class (dsetA_plist);
 *          plist_class_name = H5Pget_class_name (plist_class_id)
 *          \endcode
 *
 *          Note that frequent use of H5Pget_class_name() can become a
 *          performance problem in a high-performance environment. The
 *          H5Pequal() approach is generally much faster.
 *
 * \version 1.6.0 Return type changed in this release.
 * \since 1.0.0
 *
 */
H5_DLL hid_t H5Pget_class(hid_t plist_id);
/**
 *-------------------------------------------------------------------------
 * \ingroup GPLOA
 *
 * \brief Retrieves the name of a class
 *
 * \plistcls_id{pclass_id}
 *
 * \return Returns a pointer to an allocated string containing the class
 *         name if successful, and NULL if not successful.
 *
 * \details H5Pget_class_name() retrieves the name of a generic property
 *          list class. The pointer to the name must be freed by the user
 *          with a call to H5free_memory() after each successful call.
 *
 *          <table>
 *           <tr>
 *            <th>Class Name (class identifier) Returned</th>
 *            <th>Property List Class</th>
 *            <th>Expanded Name of the Property List Class</th>
 *            <th>The Class Identifier Used with H5Pcreate</th>
 *            <th>Comments</th>
 *           </tr>
 *           <tr>
 *            <td>attribute create</td>
 *            <td>acpl</td>
 *            <td>Attribute Creation Property List</td>
 *            <td>H5P_ATTRIBUTE_CREATE</td>
 *            <td> </td>
 *           </tr>
 *           <tr>
 *            <td>dataset access</td>
 *            <td>dapl</td>
 *            <td>Dataset Access Property List</td>
 *            <td>H5P_DATASET_ACCESS</td>
 *            <td> </td>
 *           </tr>
 *           <tr>
 *            <td>dataset create</td>
 *            <td>dcpl</td>
 *            <td>Dataset Creation Property List</td>
 *            <td>H5P_DATASET_CREATE</td>
 *            <td> </td>
 *           </tr>
 *           <tr>
 *            <td>data transfer</td>
 *            <td>dxpl</td>
 *            <td>Data Transfer Property List</td>
 *            <td>H5P_DATASET_XFER</td>
 *            <td> </td>
 *           </tr>
 *           <tr>
 *            <td>datatype access</td>
 *            <td> </td>
 *            <td> </td>
 *            <td>H5P_DATATYPE_ACCESS</td>
 *            <td>This class can be created, but there are no properties
 *                in the class currently.
 *            </td>
 *           </tr>
 *           <tr>
 *            <td>datatype create</td>
 *            <td> </td>
 *            <td> </td>
 *            <td>H5P_DATATYPE_CREATE</td>
 *            <td>This class can be created, but there
 *                are no properties in the class currently.</td>
 *           </tr>
 *           <tr>
 *            <td>file access</td>
 *            <td>fapl</td>
 *            <td>File Access Property List</td>
 *            <td>H5P_FILE_ACCESS</td>
 *            <td> </td>
 *           </tr>
 *           <tr>
 *            <td>file create</td>
 *            <td>fcpl</td>
 *            <td>File Creation Property List</td>
 *            <td>H5P_FILE_CREATE</td>
 *            <td> </td>
 *           </tr>
 *           <tr>
 *            <td>file mount</td>
 *            <td>fmpl</td>
 *            <td>File Mount Property List</td>
 *            <td>H5P_FILE_MOUNT</td>
 *            <td> </td>
 *           </tr>
 *           <tr>
 *            <td>group access</td>
 *            <td> </td>
 *            <td> </td>
 *            <td>H5P_GROUP_ACCESS</td>
 *            <td>This class can be created, but there
 *                are no properties in the class currently.</td>
 *           </tr>
 *           <tr>
 *            <td>group create</td>
 *            <td>gcpl</td>
 *            <td>Group Creation Property List</td>
 *            <td>H5P_GROUP_CREATE</td>
 *            <td> </td>
 *           </tr>
 *           <tr>
 *             <td>link access</td>
 *             <td>lapl</td>
 *             <td>Link Access Property List</td>
 *             <td>H5P_LINK_ACCESS</td>
 *             <td> </td>
 *           </tr>
 *           <tr>
 *            <td>link create</td>
 *            <td>lcpl</td>
 *            <td>Link Creation Property List</td>
 *            <td>H5P_LINK_CREATE</td>
 *            <td> </td>
 *           </tr>
 *           <tr>
 *            <td>object copy</td>
 *            <td>ocpypl</td>
 *            <td>Object Copy Property List</td>
 *            <td>H5P_OBJECT_COPY</td>
 *            <td> </td>
 *           </tr>
 *           <tr>
 *            <td>object create</td>
 *            <td>ocpl</td>
 *            <td>Object Creation Property List</td>
 *            <td>H5P_OBJECT_CREATE</td>
 *            <td> </td>
 *           </tr>
 *           <tr>
 *            <td>string create</td>
 *            <td>strcpl</td>
 *            <td>String Creation Property List</td>
 *            <td>H5P_STRING_CREATE</td>
 *            <td> </td>
 *           </tr>
 *          </table>
 *
 * \since 1.4.0
 *
 */
H5_DLL char *H5Pget_class_name(hid_t pclass_id);
/**
 *-------------------------------------------------------------------------
 * \ingroup GPLOA
 *
 * \brief Retrieves the parent class of a property class
 *
 * \plistcls_id{pclass_id}
 *
 * \return \hid_t{parent class object}
 *
 * \details H5Pget_class_parent() retrieves an identifier for the parent
 *          class of a property class.
 *
 * \since 1.4.0
 *
 */
H5_DLL hid_t H5Pget_class_parent(hid_t pclass_id);
/**
 *-------------------------------------------------------------------------
 * \ingroup GPLOA
 *
 * \brief  Queries the number of properties in a property list or class
 *
 * \param[in]  id     Identifier for property object to query
 * \param[out] nprops Number of properties in object
 *
 * \return \herr_t
 *
 * \details H5Pget_nprops() retrieves the number of properties in a
 *          property list or property list class.
 *
 *          If \p id is a property list identifier, the current number of
 *          properties in the list is returned in \p nprops.
 *
 *          If \p id is a property list class identifier, the number of
 *          registered properties in the class is returned in \p nprops.
 *
 * \since 1.4.0
 *
 */
H5_DLL herr_t H5Pget_nprops(hid_t id, size_t *nprops);
/**
 *-------------------------------------------------------------------------
 * \ingroup GPLOA
 *
 * \brief Queries the size of a property value in bytes
 *
 * \param[in]  id   Identifier of property object to query
 * \param[in]  name Name of property to query
 * \param[out] size Size of property in bytes
 *
 * \return  \herr_t
 *
 * \details H5Pget_size() retrieves the size of a property's value in
 *          bytes. This function operates on both property lists and
 *          property classes.
 *
 *          Zero-sized properties are allowed and return 0.
 *
 * \since 1.4.0
 *
 */
H5_DLL herr_t H5Pget_size(hid_t id, const char *name, size_t *size);
/**
 *-------------------------------------------------------------------------
 * \ingroup GPLOA
 *
 * \brief Registers a temporary property with a property list
 *
 * \plist_id
 * \param[in] name    Name of property to create
 * \param[in] size    Size of property in bytes
 * \param[in] value   Initial value for the property
 * \param[in] set     Callback routine called before a new value is copied
 *                    into the property's value
 * \param[in] get     Callback routine called when a property value is
 *                    retrieved from the property
 * \param[in] delete  Callback routine called when a property is deleted
 *                    from a property list
 * \param[in] copy    Callback routine called when a property is copied
 *                    from an existing property list
 * \param[in] compare Callback routine called when a property is compared
 *                    with another property list
 * \param[in] close   Callback routine called when a property list is
 *                    being closed and the property value will be disposed
 *                    of
 *
 * \return \herr_t
 *
 * \details H5Pinsert2() creates a new property in a property
 *          list. The property will exist only in this property list and
 *          copies made from it.
 *
 *          The initial property value must be provided in \p value and
 *          the property value will be set accordingly.
 *
 *          The name of the property must not already exist in this list,
 *          or this routine will fail.
 *
 *          The \p set and \p get callback routines may be set to NULL
 *          if they are not needed.
 *
 *          Zero-sized properties are allowed and do not store any data
 *          in the property list. The default value of a zero-size
 *          property may be set to NULL. They may be used to indicate the
 *          presence or absence of a particular piece of information.
 *
 *          The \p set routine is called before a new value is copied
 *          into the property. The #H5P_prp_set_func_t callback function
 *          is defined as follows:
 *          \snippet this H5P_prp_cb2_t_snip
 *
 *          The parameters to the callback function are defined as follows:
 *          <table>
 *           <tr>
 *            <td>\ref hid_t \c prop_id</td>
 *            <td>IN: The identifier of the property list being
 *                modified</td>
 *           </tr>
 *           <tr>
 *            <td>\Code{const char * name}</td>
 *            <td>IN: The name of the property being modified</td>
 *           </tr>
 *           <tr>
 *            <td>\Code{size_t size}</td>
 *            <td>IN: The size of the property in bytes</td>
 *           </tr>
 *           <tr>
 *             <td>\Code{void * value}</td>
 *             <td>IN: Pointer to new value pointer for the property
 *                 being modified</td>
 *           </tr>
 *          </table>
 *
 *          The \p set routine may modify the value pointer to be set and
 *          those changes will be used when setting the property's value.
 *          If the \p set routine returns a negative value, the new property
 *          value is not copied into the property and the \p  set routine
 *          returns an error value. The \p set routine will be called for
 *          the initial value.
 *
 *          \b Note: The \p set callback function may be useful to range
 *          check the value being set for the property or may perform some
 *          transformation or translation of the value set. The \p get
 *          callback would then reverse the transformation or translation.
 *          A single \p get or \p set callback could handle multiple
 *          properties by performing different actions based on the
 *          property name or other properties in the property list.
 *
 *          The \p get routine is called when a value is retrieved from
 *          a property value. The #H5P_prp_get_func_t callback function
 *          is defined as follows:
 *
 *          \snippet this H5P_prp_cb2_t_snip
 *
 *          The parameters to the above callback function are:
 *
 *          <table>
 *           <tr>
 *            <td>\ref hid_t \c prop_id</td>
 *            <td>IN: The identifier of the property list being queried</td>
 *           </tr>
 *           <tr>
 *            <td>\Code{const char * name}</td>
 *            <td>IN: The name of the property being queried</td>
 *           </tr>
 *           <tr>
 *            <td>\Code{size_t  size}</td>
 *            <td>IN: The size of the property in bytes</td>
 *           </tr>
 *           <tr>
 *            <td>\Code{void *  value}</td>
 *            <td>IN: The value of the property being returned</td>
 *           </tr>
 *          </table>
 *
 *          The \p get routine may modify the value to be returned from
 *          the query and those changes will be preserved. If the \p get
 *          routine returns a negative value, the query routine returns
 *          an error value.
 *
 *          The \p delete routine is called when a property is being
 *          deleted from a property list. The #H5P_prp_delete_func_t
 *          callback function is defined as follows:
 *
 *          \snippet this H5P_prp_cb2_t_snip
 *
 *          The parameters to the above callback function are:
 *
 *          <table>
 *           <tr>
 *            <td>\ref hid_t \c prop_id</td>
 *            <td>IN: The identifier of the property list the property is
 *                being deleted from</td>
 *           </tr>
 *           <tr>
 *            <td>\Code{const char * name}</td>
 *            <td>IN: The name of the property in the list</td>
 *           </tr>
 *           <tr>
 *            <td>\Code{size_t size}</td>
 *            <td>IN: The size of the property in bytes</td>
 *           </tr>
 *           <tr>
 *            <td>\Code{void * value}</td>
 *            <td>IN: The value for the property being deleted</td>
 *           </tr>
 *          </table>
 *
 *          The \p delete routine may modify the value passed in, but the
 *          value is not used by the library when the \p delete routine
 *          returns. If the \p delete routine returns a negative value,
 *          the property list \p delete routine returns an error value but
 *          the property is still deleted.
 *
 *          The \p copy routine is called when a new property list with
 *          this property is being created through a \p copy operation.
 *
 *          The #H5P_prp_copy_func_t callback function is defined as follows:
 *
 *          \snippet this H5P_prp_cb1_t_snip
 *
 *          The parameters to the above callback function are:
 *          <table>
 *           <tr>
 *            <td>\Code{const char * name}</td>
 *            <td>IN: The name of the property being copied</td>
 *           </tr>
 *           <tr>
 *            <td>\Code{size_t size}</td>
 *            <td>IN: The size of the property in bytes</td>
 *           </tr>
 *           <tr>
 *            <td>\Code{void * value}</td>
 *            <td>IN/OUT: The value for the property being copied</td>
 *           </tr>
 *          </table>
 *
 *          The \p copy routine may modify the value to be set and those
 *          changes will be stored as the new value of the property. If the
 *          \p copy routine returns a negative value, the new property value
 *          is not copied into the property and the copy routine returns an
 *          error value.
 *
 *          The \p compare routine is called when a property list with this
 *          property is compared to another property list with the same
 *          property.
 *
 *          The #H5P_prp_compare_func_t callback function is defined as
 *          follows:
 *
 *          \snippet this H5P_prp_compare_func_t_snip
 *
 *          The parameters to the callback function are defined as follows:
 *
 *          <table>
 *           <tr>
 *            <td>\Code{const void * value1}</td>
 *            <td>IN: The value of the first property to compare</td>
 *           </tr>
 *           <tr>
 *            <td>\Code{const void * value2}</td>
 *            <td>IN: The value of the second property to compare</td>
 *           </tr>
 *           <tr>
 *            <td>\Code{size_t size}</td>
 *            <td>IN: The size of the property in bytes</td>
 *           </tr>
 *          </table>
 *
 *          The \p compare routine may not modify the values. The \p compare
 *          routine should return a positive value if \p value1 is greater
 *          than \p value2, a negative value if \p value2 is greater than
 *          \p value1 and zero if \p value1 and \p value2 are equal.
 *
 *          The \p close routine is called when a property list with this
 *          property is being closed.
 *
 *          The #H5P_prp_close_func_t callback function is defined as follows:
 *          \snippet this H5P_prp_cb1_t_snip
 *
 *          The parameters to the callback function are defined as follows:
 *
 *          <table>
 *           <tr>
 *            <td>\Code{const char * name}</td>
 *            <td>IN: The name of the property in the list</td>
 *           </tr>
 *           <tr>
 *            <td>\Code{size_t size}</td>
 *            <td>IN: The size of the property in bytes</td>
 *           </tr>
 *           <tr>
 *            <td>\Code{void * value}</td>
 *            <td>IN: The value for the property being closed</td>
 *           </tr>
 *          </table>
 *
 *          The \p close routine may modify the value passed in, the
 *          value is not used by the library when the close routine
 *          returns. If the \p close routine returns a negative value,
 *          the property list \p close routine returns an error value
 *          but the property list is still closed.
 *
 *          \b Note: There is no \p create callback routine for temporary
 *          property list objects; the initial value is assumed to
 *          have any necessary setup already performed on it.
 *
 * \todo "cpp_note" goes here
 *
 * \since 1.8.0
 *
 */
H5_DLL herr_t H5Pinsert2(hid_t plist_id, const char *name, size_t size,
    void *value, H5P_prp_set_func_t set, H5P_prp_get_func_t get,
    H5P_prp_delete_func_t delete, H5P_prp_copy_func_t copy,
    H5P_prp_compare_func_t compare, H5P_prp_close_func_t close);
/**
 *-------------------------------------------------------------------------
 * \ingroup GPLOA
 *
 * \brief Determines whether a property list is a member of a class
 *
 * \plist_id
 * \plistcls_id{pclass_id}
 *
 * \return \htri_t
 *
 * \details H5Pisa_class() checks to determine whether the property list
 *          \p plist_id is a member of the property list class
 *          \p pclass_id.
 *
 * \see H5Pcreate()
 *
 * \since  1.6.0
 *
 */
H5_DLL htri_t H5Pisa_class(hid_t plist_id, hid_t pclass_id);
/**
 *-------------------------------------------------------------------------
 * \ingroup GPLOA
 *
 * \brief Iterates over properties in a property class or list
 *
 * \param[in]     id  Identifier of property object to iterate over
 * \param[in,out] idx Index of the property to begin with
 * \param[in]     iter_func  Function pointer to function to be called
 *                           with each property iterated over
 * \param[in,out] iter_data  Pointer to iteration data from user
 *
 * \return On success: the return value of the last call to \p iter_func if
 *         it was non-zero; zero if all properties have been processed.
 *         On Failure, a negative value
 *
 * \details H5Piterate() iterates over the properties in the property
 *          object specified in \p id, which may be either a property
 *          list or a property class, performing a specified operation
 *          on each property in turn.
 *
 *          For each property in the object, \p iter_func and the
 *          additional information specified below are passed to the
 *          #H5P_iterate_t operator function.
 *
 *          The iteration begins with the \p idx-th property in the
 *          object; the next element to be processed by the operator
 *          is returned in \p idx. If \p idx is NULL, the iterator
 *          starts at the first property; since no stopping point is
 *          returned in this case, the iterator cannot be restarted if
 *          one of the calls to its operator returns non-zero.
 *
 *          The prototype for the #H5P_iterate_t operator is as follows:
 *          \snippet this H5P_iterate_t_snip
 *
 *          The operation receives the property list or class
 *          identifier for the object being iterated over, \p id, the
 *          name of the current property within the object, \p name,
 *          and the pointer to the operator data passed in to H5Piterate(),
 *          \p iter_data. The valid return values from an operator are
 *          as follows:
 *
 *          <table>
 *           <tr>
 *            <td>Zero</td>
 *            <td>Causes the iterator to continue, returning zero when all
 *                properties have been processed</td>
 *           </tr>
 *           <tr>
 *            <td>Positive</td>
 *            <td>Causes the iterator to immediately return that positive
 *                value, indicating short-circuit success. The iterator
 *                can be restarted at the index of the next property</td>
 *           </tr>
 *           <tr>
 *            <td>Negative</td>
 *            <td>Causes the iterator to immediately return that value,
 *                indicating failure. The iterator can be restarted at the
 *                index of the next property</td>
 *           </tr>
 *          </table>
 *          H5Piterate() assumes that the properties in the object
 *          identified by \p id remain unchanged through the iteration.
 *          If the membership changes during the iteration, the function's
 *          behavior is undefined.
 *
 *
 * \todo "cpp_note" goes here
 *
 * \since 1.4.0
 *
 */
H5_DLL int H5Piterate(hid_t id, int *idx, H5P_iterate_t iter_func,
            void *iter_data);
/**
 *-------------------------------------------------------------------------
 * \ingroup GPLOA
 *
 * \brief Registers a permanent property with a property list class
 *
 * \plistcls_id{cls_id}
 * \param[in] name       Name of property to register
 * \param[in] size       Size of property in bytes
 * \param[in] def_value  Default value for property in newly created
 *                       property lists
 * \param[in] create     Callback routine called when a property list is
 *                       being created and the property value will be
 *                       initialized
 * \param[in] set        Callback routine called before a new value is
 *                       copied into the property's value
 * \param[in] get        Callback routine called when a property value is
 *                       retrieved from the property
 * \param[in] delete     Callback routine called when a property is deleted
 *                       from a property list
 * \param[in] copy       Callback routine called when a property is copied
 *                       from a property list
 * \param[in] compare    Callback routine called when a property is compared
 *                       with another property list
 * \param[in] close      Callback routine called when a property list is
 *                       being closed and the property value will be
 *                       disposed of
 *
 * \return  \herr_t
 *
 * \details H5Pregister2() registers a new property with a property list
 *          class. The \p cls_id identifier can be obtained by calling
 *          H5Pcreate_class(). The property will exist in all property
 *          list objects of \p cl_id created after this routine finishes. The
 *          name of the property must not already exist, or this routine
 *          will fail. The default property value must be provided and all
 *          new property lists created with this property will have the
 *          property value set to the default value. Any of the callback
 *          routines may be set to NULL if they are not needed.
 *
 *          Zero-sized properties are allowed and do not store any data in
 *          the property list. These may be used as flags to indicate the
 *          presence or absence of a particular piece of information. The
 *          default pointer for a zero-sized property may be set to NULL.
 *          The property \p create and \p close callbacks are called for
 *          zero-sized properties, but the \p set and \p get callbacks are
 *          never called.
 *
 *          The \p create routine is called when a new property list with
 *          this property is being created. The #H5P_prp_create_func_t
 *          callback function is defined as follows:
 *
 *          \snippet this H5P_prp_cb1_t_snip
 *
 *          The parameters to this callback function are defined as follows:
 *
 *          <table>
 *           <tr>
 *            <td>\Code{const char * name}</td>
 *            <td>IN: The name of the property being modified</td>
 *           </tr>
 *           <tr>
 *            <td>\Code{size_t size}</td>
 *            <td>IN: The size of the property in bytes</td>
 *           </tr>
 *           <tr>
 *            <td>\Code{void * value}</td>
 *            <td>IN/OUT: The default value for the property being created,
 *                which will be passed to H5Pregister2()</td>
 *           </tr>
 *          </table>
 *
 *          The \p create routine may modify the value to be set and those
 *          changes will be stored as the initial value of the property.
 *          If the \p create routine returns a negative value, the new
 *          property value is not copied into the property and the
 *          \p create routine returns an error value.
 *
 *          The \p set routine is called before a new value is copied into
 *          the property. The #H5P_prp_set_func_t callback function is defined
 *          as follows:
 *
 *          \snippet this H5P_prp_cb2_t_snip
 *
 *          The parameters to this callback function are defined as follows:
 *
 *          <table>
 *           <tr>
 *            <td>\ref hid_t \c prop_id</td>
 *            <td>IN: The identifier of the property list being modified</td>
 *           </tr>
 *           <tr>
 *            <td>\Code{const char * name}</td>
 *            <td>IN: The name of the property being modified</td>
 *           </tr>
 *           <tr>
 *            <td>\Code{size_t size}</td>
 *            <td>IN: The size of the property in bytes</td>
 *           </tr>
 *           <tr>
 *            <td>\Code{void *value}</td>
 *            <td>IN/OUT: Pointer to new value pointer for the property
 *                being modified</td>
 *           </tr>
 *          </table>
 *
 *          The \p set routine may modify the value pointer to be set and
 *          those changes will be used when setting the property's value.
 *          If the \p set routine returns a negative value, the new property
 *          value is not copied into the property and the \p set routine
 *          returns an error value. The \p set routine will not be called
 *          for the initial value; only the \p create routine will be called.
 *
 *          \b Note: The \p set callback function may be useful to range
 *          check the value being set for the property or may perform some
 *          transformation or translation of the value set. The \p get
 *          callback would then reverse the transformation or translation.
 *          A single \p get or \p set callback could handle multiple
 *          properties by performing different actions based on the property
 *          name or other properties in the property list.
 *
 *          The \p get routine is called when a value is retrieved from a
 *          property value. The #H5P_prp_get_func_t callback function is
 *          defined as follows:
 *
 *          \snippet this H5P_prp_cb2_t_snip
 *
 *          The parameters to the callback function are defined as follows:
 *
 *          <table>
 *           <tr>
 *            <td>\ref hid_t \c prop_id</td>
 *            <td>IN: The identifier of the property list being
 *                queried</td>
 *           </tr>
 *           <tr>
 *            <td>\Code{const char * name}</td>
 *            <td>IN: The name of the property being queried</td>
 *           </tr>
 *           <tr>
 *            <td>\Code{size_t size}</td>
 *            <td>IN: The size of the property in bytes</td>
 *           </tr>
 *           <tr>
 *            <td>\Code{void * value}</td>
 *            <td>IN/OUT: The value of the property being returned</td>
 *           </tr>
 *          </table>
 *
 *          The \p get routine may modify the value to be returned from the
 *          query and those changes will be returned to the calling routine.
 *          If the \p set routine returns a negative value, the query
 *          routine returns an error value.
 *
 *          The \p delete routine is called when a property is being
 *          deleted from a property list. The #H5P_prp_delete_func_t
 *          callback function is defined as follows:
 *
 *          \snippet this H5P_prp_cb2_t_snip
 *
 *          The parameters to the callback function are defined as follows:
 *
 *          <table>
 *           <tr>
 *            <td>\ref hid_t \c prop_id</td>
 *            <td>IN: The identifier of the property list the property is
 *                being deleted from</td>
 *           </tr>
 *           <tr>
 *            <td>\Code{const char * name}</td>
 *            <td>IN: The name of the property in the list</td>
 *           </tr>
 *           <tr>
 *            <td>\Code{size_t size}</td>
 *            <td>IN: The size of the property in bytes</td>
 *           </tr>
 *           <tr>
 *            <td>\Code{void * value}</td>
 *            <td>IN: The value for the property being deleted</td>
 *           </tr>
 *          </table>
 *
 *          The \p delete routine may modify the value passed in, but the
 *          value is not used by the library when the \p delete routine
 *          returns. If the \p delete routine returns a negative value,
 *          the property list  delete routine returns an error value but
 *          the property is still deleted.
 *
 *          The \p copy routine is called when a new property list with
 *          this property is being created through a \p copy operation.
 *          The #H5P_prp_copy_func_t callback function is defined as follows:
 *
 *          \snippet this H5P_prp_cb1_t_snip
 *
 *          The parameters to the callback function are defined as follows:
 *
 *          <table>
 *           <tr>
 *            <td>\Code{const char * name}</td>
 *            <td>IN: The name of the property being copied</td>
 *           </tr>
 *           <tr>
 *            <td>\Code{size_t size}</td>
 *            <td>IN: The size of the property in bytes</td>
 *           </tr>
 *           <tr>
 *            <td>\Code{void * value}</td>
 *            <td>IN/OUT: The value for the property being copied</td>
 *           </tr>
 *          </table>
 *
 *          The \p copy routine may modify the value to be set and those
 *          changes will be stored as the new value of the property. If
 *          the \p copy routine returns a negative value, the new
 *          property value is not copied into the property and the \p copy
 *          routine returns an error value.
 *
 *          The \p compare routine is called when a property list with this
 *          property is compared to another property list with the same
 *          property. The #H5P_prp_compare_func_t callback function is
 *          defined as follows:
 *
 *          \snippet this H5P_prp_compare_func_t_snip
 *
 *          The parameters to the callback function are defined as follows:
 *
 *          <table>
 *           <tr>
 *            <td>\Code{const void * value1}</td>
 *            <td>IN: The value of the first property to compare</td>
 *           </tr>
 *           <tr>
 *            <td>\Code{const void * value2}</td>
 *            <td>IN: The value of the second property to compare</td>
 *           </tr>
 *           <tr>
 *            <td>\Code{size_t size}</td>
 *            <td>IN: The size of the property in bytes</td>
 *           </tr>
 *          </table>
 *
 *          The \p compare routine may not modify the values. The \p compare
 *          routine should return a positive value if \p value1 is greater
 *          than \p value2, a negative value if \p value2 is greater than
 *          \p value1 and zero if \p value1 and \p value2 are equal.
 *
 *          The \p close routine is called when a property list with this
 *          property is being closed. The #H5P_prp_close_func_t callback
 *          function is defined as follows:
 *
 *          \snippet this H5P_prp_cb2_t_snip
 *
 *          The parameters to the callback function are defined as follows:
 *
 *          <table>
 *           <tr>
 *            <td>\ref hid_t \c prop_id</td>
 *            <td>IN: The identifier of the property list being closed</td>
 *           </tr>
 *           <tr>
 *            <td>\Code{const char * name}</td>
 *            <td>IN: The name of the property in the list</td>
 *           </tr>
 *           <tr>
 *            <td>\Code{size_t size}</td>
 *            <td>IN: The size of the property in bytes</td>
 *           </tr>
 *           <tr>
 *            <td>\Code{void * value}</td>
 *            <td>IN: The value for the property being closed</td>
 *           </tr>
 *          </table>
 *
 *          The \p close routine may modify the value passed in, but the
 *          value is not used by the library when the \p close routine returns.
 *          If the \p close routine returns a negative value, the property
 *          list close routine returns an error value but the property list is
 *          still closed.
 *
 * \todo "cpp_note" goes here
 *
 * \since 1.8.0
 *
 */
H5_DLL herr_t H5Pregister2(hid_t cls_id, const char *name, size_t size,
    void *def_value, H5P_prp_create_func_t create,
    H5P_prp_set_func_t set, H5P_prp_get_func_t get,
    H5P_prp_delete_func_t delete, H5P_prp_copy_func_t copy,
    H5P_prp_compare_func_t compare, H5P_prp_close_func_t close);
/**
 *-------------------------------------------------------------------------
 * \ingroup GPLOA
 *
 * \brief Removes a property from a property list
 *
 * \plist_id
 * \param[in] name Name of property to remove
 *
 * \return \herr_t
 *
 * \details H5Premove() removes a property from a property list. Both
 *          properties which were in existence when the property list was
 *          created (i.e. properties registered with H5Pregister()) and
 *          properties added to the list after it was created (i.e. added
 *          with H5Pinsert1() may be removed from a property list.
 *          Properties do not need to be removed from a property list
 *          before the list itself is closed; they will be released
 *          automatically when H5Pclose() is called.
 *
 *          If a \p close callback exists for the removed property, it
 *          will be called before the property is released.
 *
 * \since 1.4.0
 *
 */
H5_DLL herr_t H5Premove(hid_t plist_id, const char *name);
/**
 *-------------------------------------------------------------------------
 *
 * \ingroup GPLOA
 *
 * \brief Sets a property list value
 *
 * \plist_id
 * \param[in] name  Name of property to modify
 * \param[in] value Pointer to value to set the property to
 *
 * \return \herr_t
 *
 * \details H5Pset() sets a new value for a property in a property list.
 *          If there is a \p set callback routine registered for this
 *          property, the \p value will be passed to that routine and any
 *          changes to the \p value will be used when setting the property
 *          value. The information pointed to by the \p value pointer
 *          (possibly modified by the \p set callback) is copied into the
 *          property list value and may be changed by the application
 *          making the H5Pset() call without affecting the property value.
 *
 *          The property name must exist or this routine will fail.
 *
 *          If the \p set callback routine returns an error, the property
 *          value will not be modified.
 *
 *          This routine may not be called for zero-sized properties and
 *          will return an error in that case.
 *
 * \since 1.4.0
 *
 */
H5_DLL herr_t H5Pset(hid_t plist_id, const char *name, const void *value);
/**
 *-------------------------------------------------------------------------
 *
 * \ingroup GPLOA
 *
 * \brief Removes a property from a property list class
 *
 * \plistcls_id{pclass_id}
 * \param[in] name Name of property to remove
 *
 * \return \herr_t
 *
 * \details H5Punregister() removes a property from a property list class.
 *          Future property lists created of that class will not contain
 *          this property; existing property lists containing this property
 *          are not affected.
 *
 * \since 1.4.0
 *
 */
H5_DLL herr_t H5Punregister(hid_t pclass_id, const char *name);

/* Object creation property list (OCPL) routines */
/**
 *--------------------------------------------------------------------------
 *
 * \ingroup OCPL
 *
 * \brief Verifies that all required filters are available
 *
 * \plist_id
 *
 * \return \htri_t
 *
 * \details H5Pall_filters_avail() verifies that all of the filters set in
 *         the dataset or group creation property list \p plist_id are
 *         currently available.
 *
 * \version 1.8.5 Function extended to work with group creation property
 *                lists.
 * \since 1.6.0
 *
 */
H5_DLL htri_t H5Pall_filters_avail(hid_t plist_id);
/**
 *--------------------------------------------------------------------------
 *
 * \ingroup OCPL
 *
 * \brief Retrieves tracking and indexing settings for attribute creation
 *        order
 *
 * \plist_id
 * \param[out] crt_order_flags Flags specifying whether to track and
 *             index attribute creation order
 *
 * \return \herr_t
 *
 * \details H5Pget_attr_creation_order() retrieves the settings for
 *          tracking and indexing attribute creation order on an object.
 *
 *          \p plist_id is an object creation property list (\p ocpl),
 *          as it can be a dataset or group creation property list
 *          identifier. The term \p ocpl is used when different types
 *          of objects may be involved.
 *
 *          \p crt_order_flags returns flags with the following meanings:
 *
 *          <table>
 *           <tr>
 *            <td>#H5P_CRT_ORDER_TRACKED</td>
 *            <td>Attribute creation order is tracked but not necessarily
 *                indexed.</td>
 *           </tr>
 *           <tr>
 *            <td>#H5P_CRT_ORDER_INDEXED </td>
 *            <td>Attribute creation order is indexed (requires
 *                #H5P_CRT_ORDER_TRACKED).</td>
 *           </tr>
 *          </table>
 *
 *          If \p crt_order_flags is returned with a value of 0 (zero),
 *          attribute creation order is neither tracked nor indexed.
 *
 * \since 1.8.0
 *
 */
H5_DLL herr_t H5Pget_attr_creation_order(hid_t plist_id, unsigned *crt_order_flags);
/**
 *--------------------------------------------------------------------------
 *
 * \ingroup OCPL
 *
 * \brief Retrieves attribute storage phase change thresholds
 *
 * \plist_id
 * \param[out] max_compact Maximum number of attributes to be stored in
 *                         compact storage (Default: 8)
 * \param[out] min_dense   Minimum number of attributes to be stored in
 *                         dense storage (Default: 6)
 *
 * \return \herr_t
 *
 * \details H5Pget_attr_phase_change() retrieves threshold values for
 *          attribute storage on an object. These thresholds determine the
 *          point at which attribute storage changes from compact storage
 *          (i.e., storage in the object header) to dense storage (i.e.,
 *          storage in a heap and indexed with a B-tree).
 *
 *          In the general case, attributes are initially kept in compact
 *          storage. When the number of attributes exceeds \p max_compact,
 *          attribute storage switches to dense storage. If the number of
 *          attributes subsequently falls below \p min_dense, the
 *          attributes are returned to compact storage.
 *
 *          If \p max_compact is set to 0 (zero), dense storage always used.
 *
 *          \p plist_id is an object creation property list (\p ocpl), as it
 *          can be a dataset or group creation property list identifier.
 *          The term \p ocpl is used when different types of objects may be
 *          involved.
 *
 * \since 1.8.0
 *
 */
H5_DLL herr_t H5Pget_attr_phase_change(hid_t plist_id, unsigned *max_compact, unsigned *min_dense);
/**
 *--------------------------------------------------------------------------
 *
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
 * \return #H5Z_filter_t
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
 *--------------------------------------------------------------------------
 */
H5_DLL H5Z_filter_t H5Pget_filter2(hid_t plist_id, unsigned idx,
       unsigned int *flags/*out*/,
       size_t *cd_nelmts/*out*/,
       unsigned cd_values[]/*out*/,
       size_t namelen, char name[],
       unsigned *filter_config /*out*/);
/**
 *-------------------------------------------------------------------------
 *
 * \ingroup OCPL
 *
 * \brief Returns information about the specified filter
 *
 * \plist_id
 * \param[in]     filter_id     Filter identifier
 * \param[out]    flags         Bit vector specifying certain general
 *                              properties of the filter
 * \param[in,out] cd_nelmts     Number of elements in \p cd_values
 * \param[out]    cd_values[]   Auxiliary data for the filter
 * \param[in]     namelen       Length of filter name and number of
 *                              elements in \p name
 * \param[out]    name[]        Name of filter
 * \param[out]    filter_config Bit field, as described in
 *                              H5Zget_filter_info()
 *
 * \return \herr_t
 *
 * \details H5Pget_filter_by_id2() returns information about the filter
 *          specified in \p filter_id, a filter identifier.
 *
 *          \p plist_id must be a dataset or group creation property list
 *          and \p filter_id must be in the associated filter pipeline.
 *
 *          The \p filter_id and \p flags parameters are used in the same
 *          manner as described in the discussion of H5Pset_filter().
 *
 *          Aside from the fact that they are used for output, the
 *          parameters \p cd_nelmts and \p cd_values[] are used in the same
 *          manner as described in the discussion of H5Pset_filter(). On
 *          input, the \p cd_nelmts parameter indicates the number of
 *          entries in the \p cd_values[] array allocated by the calling
 *          program; on exit it contains the number of values defined by
 *          the filter.
 *
 *          On input, the \p namelen parameter indicates the number of
 *          characters allocated for the filter name by the calling program
 *          in the array \p name[]. On exit \p name[] contains the name of the
 *          filter with one character of the name in each element of the
 *          array.
 *
 *          \p filter_config is the bit field described in
 *          H5Zget_filter_info().
 *
 *          If the filter specified in \p filter_id is not set for the
 *          property list, an error will be returned and
 *          H5Pget_filter_by_id2() will fail.
 *
 * \version 1.8.5 Function extended to work with group creation property
 *                lists.
 *
 * \since 1.8.0
 *
 */
H5_DLL herr_t H5Pget_filter_by_id2(hid_t plist_id, H5Z_filter_t filter_id,
       unsigned int *flags/*out*/, size_t *cd_nelmts/*out*/,
       unsigned cd_values[]/*out*/, size_t namelen, char name[]/*out*/,
       unsigned *filter_config/*out*/);
/**
 *-------------------------------------------------------------------------
 * \ingroup OCPL
 *
 * \brief Returns the number of filters in the pipeline
 *
 * \todo Signature for H5Pget_nfilters() is different in H5Pocpl.c than in
 *       H5Ppublic.h.
 *
 * \plist_id
 *
 * \return  Returns the number of filters in the pipeline if successful;
 *          otherwise returns a negative value.
 *
 * \details H5Pget_nfilters() returns the number of filters defined in the
 *          filter pipeline associated with the property list \p plist_id.
 *
 *          In each pipeline, the filters are numbered from 0 through N-1,
 *          where N is the value returned by this function. During output to
 *          the file, the filters are applied in increasing order; during
 *          input from the file, they are applied in decreasing order.
 *
 *          H5Pget_nfilters() returns the number of filters in the pipeline,
 *          including zero (0) if there are none.
 *
 * \since 1.0.0
 *
 *--------------------------------------------------------------------------
 */
H5_DLL int H5Pget_nfilters(hid_t plist_id);
/**
 *-------------------------------------------------------------------------
 *
 * \ingroup OCPL
 *
 * \brief Determines whether times associated with an object
 *       are being recorded
 *
 * \plist_id
 * \param[out] track_times Boolean value, 1 (TRUE) or 0 (FALSE),
 *             specifying whether object times are being recorded
 *
 * \return \herr_t
 *
 * \details H5Pget_obj_track_times() queries the object creation property
 *          list, \p plist_id, to determine whether object times are being
 *          recorded.
 *
 *          If \p track_times is returned as 1, times are being recorded;
 *          if \p track_times is returned as 0, times are not being
 *          recorded.
 *
 *          Time data can be retrieved with H5Oget_info(), which will return
 *          it in the #H5O_info_t struct.
 *
 *          If times are not tracked, they will be reported as follows
 *          when queried: 12:00 AM UDT, Jan. 1, 1970
 *
 *          See H5Pset_obj_track_times() for further discussion.
 *
 * \since 1.8.0
 *
 */
H5_DLL herr_t H5Pget_obj_track_times(hid_t plist_id, hbool_t *track_times);

H5_DLL herr_t H5Pmodify_filter(hid_t plist_id, H5Z_filter_t filter,
        unsigned int flags, size_t cd_nelmts,
        const unsigned int cd_values[/*cd_nelmts*/]);
H5_DLL herr_t H5Premove_filter(hid_t plist_id, H5Z_filter_t filter);
H5_DLL herr_t H5Pset_attr_creation_order(hid_t plist_id, unsigned crt_order_flags);
H5_DLL herr_t H5Pset_attr_phase_change(hid_t plist_id, unsigned max_compact, unsigned min_dense);
/**
 *-------------------------------------------------------------------------
 * \ingroup OCPL
 *
 * \brief Sets deflate (GNU gzip) compression method and compression level
 *
 * \todo H5Pset_deflate prototype does not match source code
 * \plist_id
 * \param[in] level Compression level
 *
 * \return \herr_t
 *
 * \details H5Pset_deflate() sets the deflate compression method and the
 *          compression level, \p level, for a dataset or group creation
 *          property list, \p plist_id.
 *
 *          The filter identifier set in the property list is
 *          #H5Z_FILTER_DEFLATE.
 *
 *          The compression level, \p level, is a value from zero to nine,
 *          inclusive. A compression level of 0 (zero) indicates no
 *          compression; compression improves but speed slows progressively
 *          from levels 1 through 9:
 *
 *          <table>
 *            <tr>
 *              <th>Compression Level</th>
 *              <th>Gzip Action</th>
 *            </tr>
 *            <tr>
 *              <td>0</td>
 *              <td>No compression</td>
 *            </tr>
 *            <tr>
 *              <td>1</td>
 *              <td>Best compression speed; least compression</td>
 *            </tr>
 *           <tr>
 *             <td>2 through 8</td>
 *             <td>Compression improves; speed degrades</td>
 *           </tr>
 *           <tr>
 *             <td>9</td>
 *             <td>Best compression ratio; slowest speed</td>
 *           </tr>
 *          </table>
 *
 *          Note that setting the compression level to 0 (zero) does not turn
 *          off use of the gzip filter; it simply sets the filter to perform
 *          no compression as it processes the data.
 *
 *          HDF5 relies on GNU gzip for this compression.
 *
 * \version 1.8.5 Function extended to work with group creation property lists.
 * \since 1.0.0
 *
 *--------------------------------------------------------------------------
 */
H5_DLL herr_t H5Pset_deflate(hid_t plist_id, unsigned level);
H5_DLL herr_t H5Pset_filter(hid_t plist_id, H5Z_filter_t filter,
        unsigned int flags, size_t cd_nelmts,
        const unsigned int c_values[]);
H5_DLL herr_t H5Pset_fletcher32(hid_t plist_id);
H5_DLL herr_t H5Pset_obj_track_times(hid_t plist_id, hbool_t track_times);

/* File creation property list (FCPL) routines */
H5_DLL herr_t H5Pget_file_space_page_size(hid_t plist_id, hsize_t *fsp_size);
H5_DLL herr_t H5Pget_file_space_strategy(hid_t plist_id, H5F_fspace_strategy_t *strategy, hbool_t *persist, hsize_t *threshold);
H5_DLL herr_t H5Pget_istore_k(hid_t plist_id, unsigned *ik/*out*/);
H5_DLL herr_t H5Pget_shared_mesg_index(hid_t plist_id, unsigned index_num, unsigned *mesg_type_flags, unsigned *min_mesg_size);
H5_DLL herr_t H5Pget_shared_mesg_nindexes(hid_t plist_id, unsigned *nindexes);
H5_DLL herr_t H5Pget_shared_mesg_phase_change(hid_t plist_id, unsigned *max_list, unsigned *min_btree);
/**
 *-------------------------------------------------------------------------
 *
 * \ingroup FCPL
 *
 * \brief Retrieves the size of the offsets and lengths used in an HDF5
 *        file
 *
 * \fcpl_id{plist_id}
 * \param[out] sizeof_addr Pointer to location to return offset size in
 *             bytes
 * \param[out] sizeof_size Pointer to location to return length size in
 *             bytes
 *
 * \return \herr_t
 *
 * \details H5Pget_sizes() retrieves the size of the offsets and lengths
 *          used in an HDF5 file. This function is only valid for file
 *          creation property lists.
 *
 * \since  1.0.0
 *
 */
H5_DLL herr_t H5Pget_sizes(hid_t plist_id, size_t *sizeof_addr/*out*/,
       size_t *sizeof_size/*out*/);
/**
 *-------------------------------------------------------------------------
 *
 * \ingroup FCPL
 *
 * \brief Retrieves the size of a user block
 *
 * \fcpl_id{plist_id}
 * \param[out] size  Pointer to location to return user-block size
 *
 * \return \herr_t
 *
 * \details H5Pget_userblock() retrieves the size of a user block in a
 *          file creation property list.
 *
 * \since 1.0.0
 *
 */
H5_DLL herr_t H5Pget_userblock(hid_t plist_id, hsize_t *size);
H5_DLL herr_t H5Pget_sym_k(hid_t plist_id, unsigned *ik/*out*/, unsigned *lk/*out*/);
H5_DLL herr_t H5Pset_file_space_strategy(hid_t plist_id, H5F_fspace_strategy_t strategy, hbool_t persist, hsize_t threshold);
H5_DLL herr_t H5Pset_file_space_page_size(hid_t plist_id, hsize_t fsp_size);
H5_DLL herr_t H5Pset_istore_k(hid_t plist_id, unsigned ik);
H5_DLL herr_t H5Pset_shared_mesg_index(hid_t plist_id, unsigned index_num, unsigned mesg_type_flags, unsigned min_mesg_size);
H5_DLL herr_t H5Pset_shared_mesg_nindexes(hid_t plist_id, unsigned nindexes);
H5_DLL herr_t H5Pset_shared_mesg_phase_change(hid_t plist_id, unsigned max_list, unsigned min_btree);
/**
 *-------------------------------------------------------------------------
 *
 * \ingroup FCPL
 *
 * \brief Sets the byte size of the offsets and lengths used to address
 *        objects in an HDF5 file
 *
 * \fcpl_id{plist_id}
 * \param[in] sizeof_addr Size of an object offset in bytes
 * \param[in] sizeof_size Size of an object length in bytes
 *
 * \return \herr_t
 *
 * \details H5Pset_sizes() sets the byte size of the offsets and lengths
 *          used to address objects in an HDF5 file. This function is only
 *          valid for file creation property lists. Passing in a value
 *          of 0 for one of the parameters retains the current value. The
 *          default value for both values is the same as sizeof(hsize_t)
 *          in the library (normally 8 bytes). Valid values currently
 *          are 2, 4, 8 and 16.
 *
 * \since 1.0.0
 *
 */
H5_DLL herr_t H5Pset_sizes(hid_t plist_id, size_t sizeof_addr,
       size_t sizeof_size);
H5_DLL herr_t H5Pset_sym_k(hid_t plist_id, unsigned ik, unsigned lk);
/**
 *-------------------------------------------------------------------------
 *
 * \ingroup FCPL
 *
 * \brief Sets user block size
 *
 * \fcpl_id{plist_id}
 * \param[in] size Size of the user-block in bytes
 *
 * \return  \herr_t
 *
 * \details H5Pset_userblock() sets the user block size of a file creation
 *          property list. The default user block size is 0; it may be set
 *          to any power of 2 equal to 512 or greater (512, 1024, 2048, etc.).
 *
 * \since 1.0.0
 *
 */
H5_DLL herr_t H5Pset_userblock(hid_t plist_id, hsize_t size);

/* File access property list (FAPL) routines */
/**
 *-------------------------------------------------------------------------
 *
 * \ingroup FAPL
 *
 * \brief Retrieves the current settings for alignment properties from a
 *        file access property list
 *
 * \fapl_id
 * \param[out] threshold Pointer to location of return threshold value
 * \param[out] alignment Pointer to location of return alignment value
 *
 * \return \herr_t
 *
 * \details H5Pget_alignment() retrieves the current settings for
 *          alignment properties from a file access property list. The
 *          \p threshold and/or \p alignment pointers may be null
 *          pointers (NULL).
 *
 * \since 1.0.0
 *
 */
H5_DLL herr_t H5Pget_alignment(hid_t fapl_id, hsize_t *threshold/*out*/,
    hsize_t *alignment/*out*/);
/**
 *-------------------------------------------------------------------------
 *
 * \ingroup FAPL
 *
 * \brief Queries the raw data chunk cache parameters
 *
 * \fapl_id{plist_id}
 * \param[in,out] mdc_nelmts  <i>No longer used</i>
 * \param[in,out] rdcc_nslots Number of elements (objects) in the raw data
 *                            chunk cache
 * \param[in,out] rdcc_nbytes Total size of the raw data chunk cache, in
 *                            bytes
 * \param[in,out] rdcc_w0     Preemption policy
 *
 * \return \herr_t
 *
 * \details H5Pget_cache() retrieves the maximum possible number of
 *          elements in the raw data chunk cache, the maximum possible
 *          number of bytes in the raw data chunk cache, and the
 *          preemption policy value.
 *
 *          Any (or all) arguments may be null pointers, in which case
 *          the corresponding datum is not returned.
 *
 *          Note that the \p mdc_nelmts parameter is no longer used.
 *
 * \version 1.8.0 Use of the \p mdc_nelmts parameter discontinued.
 *                Metadata cache configuration is managed with
 *                H5Pset_mdc_config() and H5Pget_mdc_config()
 * \version 1.6.0 The \p rdcc_nbytes and \p rdcc_nslots parameters changed
 *                from type int to size_t.
 *
 * \since 1.0.0
 *
 */
H5_DLL herr_t H5Pget_cache(hid_t plist_id,
       int *mdc_nelmts, /* out */
       size_t *rdcc_nslots/*out*/,
       size_t *rdcc_nbytes/*out*/, double *rdcc_w0);
/**
 *-------------------------------------------------------------------------
 *
 * \ingroup FAPL
 *
 * \brief Returns low-lever driver identifier
 *
 * \plist_id
 *
 * \return \hid_t{low level driver}
 *
 * \details H5Pget_driver() returns the identifier of the low-level file
 *          driver associated with the file access property list or
 *          data transfer property list \p plist_id.
 *
 *          Valid driver identifiers distributed with HDF5 are listed and
 *          described in the following table.
 *
 *          <table>
 *           <tr>
 *            <th>Driver Name</th>
 *            <th>Driver Identifier</th>
 *            <th>Description</th>
 *            <th>Related Function</th>
 *           </tr>
 *           <tr>
 *            <td>POSIX</td>
 *            <td>#H5FD_SEC2</td>
 *            <td>This driver uses POSIX file-system functions like read and
 *                write to perform I/O to a single, permanent file on local disk
 *                with no system buffering. This driver is POSIX-compliant and is
 *                the default file driver for all systems.</td>
 *            <td>H5Pset_fapl_sec2()</td>
 *           </tr>
 *           <tr>
 *            <td>Direct</td>
 *            <td>#H5FD_DIRECT</td>
 *            <td>This is the #H5FD_SEC2 driver except data is written to or
 *                read from the file synchronously without being cached by the
 *                system.</td>
 *            <td>H5Pset_fapl_direct()</td>
 *           </tr>
 *           <tr>
 *            <td>Log</td>
 *            <td>#H5FD_LOG</td>
 *            <td>This is the #H5FD_SEC2 driver with logging capabilities.</td>
 *            <td>H5Pset_fapl_log()</td>
 *           </tr>
 *           <tr>
 *            <td>Windows</td>
 *            <td>#H5FD_WINDOWS</td>
 *            <td>This driver was modified in HDF5-1.8.8 to be a wrapper of the
 *                POSIX driver, #H5FD_SEC2. This change should not affect user
 *                applications.</td>
 *            <td>H5Pset_fapl_windows()</td>
 *           </tr>
 *           <tr>
 *            <td>STDIO</td>
 *            <td>#H5FD_STDIO</td>
 *            <td>This driver uses functions from the standard C stdio.h to
 *                perform I/O to a single, permanent file on local disk with
 *                additional system buffering.</td>
 *            <td>H5Pset_fapl_stdio()</td>
 *           </tr>
 *           <tr>
 *            <td>Memory</td>
 *            <td>#H5FD_CORE</td>
 *            <td>With this driver, an application can work with a file in
 *                memory for faster reads and writes. File contents are kept in
 *                memory until the file is closed. At closing, the memory version
 *                of the file can be written back to disk or abandoned.</td>
 *            <td>H5Pset_fapl_core()</td>
 *           </tr>
 *           <tr>
 *            <td>Family</td>
 *            <td>#H5FD_FAMILY</td>
 *            <td>With this driver, the HDF5 file’s address space is partitioned
 *                into pieces and sent to separate storage files using an
 *                underlying driver of the user’s choice. This driver is for
 *                systems that do not support files larger than 2 gigabytes.</td>
 *            <td>H5Pset_fapl_family()</td>
 *           </tr>
 *           <tr>
 *            <td>Multi</td>
 *            <td>#H5FD_MULTI</td>
 *            <td>With this driver, data can be stored in multiple files
 *                according to the type of the data. I/O might work better if
 *                data is stored in separate files based on the type of data.
 *                The Split driver is a special case of this driver.</td>
 *            <td>H5Pset_fapl_multi()</td>
 *           </tr>
 *           <tr>
 *            <td>Parallel</td>
 *            <td>#H5FD_MPIO</td>
 *            <td>This is the standard HDF5 file driver for parallel file systems.
 *                This driver uses the MPI standard for both communication and
 *                file I/O.</td>
 *            <td>H5Pset_fapl_mpio()</td>
 *           </tr>
 *           <tr>
 *            <td>Parallel POSIX</td>
 *            <td>H5FD_MPIPOSIX</td>
 *            <td>This driver is no longer available.</td>
 *            <td></td>
 *           </tr>
 *           <tr>
 *            <td>Stream</td>
 *            <td>H5FD_STREAM</td>
 *            <td>This driver is no longer available.</td>
 *            <td></td>
 *           </tr>
 *          </table>
 *
 *          This list does not include custom drivers that might be defined and
 *          registered by a user.
 *
 *          The returned driver identifier is only valid as long as the file driver
 *          remains registered.
 *
 *
 * \since 1.4.0
 *
 */
H5_DLL hid_t H5Pget_driver(hid_t plist_id);
H5_DLL const void *H5Pget_driver_info(hid_t plist_id);
/**
 *-------------------------------------------------------------------------
 *
 * \ingroup FAPL
 *
 * \brief Returns the file close degree
 *
 * \fapl_id
 * \param[out] degree Pointer to a location to which to return the file
 *                    close degree property, the value of \p degree
 *
 * \return \herr_t
 *
 * \details H5Pget_fclose_degree() returns the current setting of the file
 *          close degree property \p degree in the file access property
 *          list \p fapl_id. The value of \p degree determines how
 *          aggressively H5Fclose() deals with objects within a file that
 *          remain open when H5Fclose() is called to close that file.
 *
 * \since 1.6.0
 *
 */
H5_DLL herr_t H5Pget_fclose_degree(hid_t fapl_id, H5F_close_degree_t *degree);
/**
 *-------------------------------------------------------------------------
 *
 * \ingroup FAPL
 *
 * \brief Retrieves library version bounds settings that indirectly control
 *        the format versions used when creating objects
 *
 * \fapl_id{plist_id}
 * \param[out] low  The earliest version of the library that will be used
 *                  for writing objects
 * \param[out] high The latest version of the library that will be used for
 *                  writing objects
 *
 * \return \herr_t
 *
 * \details H5Pget_libver_bounds() retrieves the lower and upper bounds on
 *          the HDF5 library release versions that indirectly determine the
 *          object format versions used when creating objects in the file.
 *
 *          This property is retrieved from the file access property list
 *          specified by the parameter \p fapl_id.
 *
 *          The value returned in the parameters \p low and \p high is one
 *          of the enumerated values in the #H5F_libver_t struct, which is
 *          defined in H5Fpublic.h.
 *
 * \version 1.10.2 Add #H5F_LIBVER_V18 to the enumerated defines in
 *                 #H5F_libver_t
 * \since 1.8.0
 *
 */
H5_DLL herr_t H5Pget_libver_bounds(hid_t plist_id, H5F_libver_t *low,
    H5F_libver_t *high);
H5_DLL herr_t H5Pset_vol(hid_t plist_id, hid_t new_vol_id, const void *new_vol_info);
H5_DLL herr_t H5Pget_vol_id(hid_t plist_id, hid_t *vol_id);
H5_DLL herr_t H5Pget_vol_info(hid_t plist_id, void **vol_info);
H5_DLL herr_t H5Pset_family_offset(hid_t fapl_id, hsize_t offset);
H5_DLL herr_t H5Pget_family_offset(hid_t fapl_id, hsize_t *offset);
H5_DLL herr_t H5Pset_multi_type(hid_t fapl_id, H5FD_mem_t type);
H5_DLL herr_t H5Pget_multi_type(hid_t fapl_id, H5FD_mem_t *type);
/**
 *-------------------------------------------------------------------------
 *
 * \ingroup FAPL
 *
 * \brief Sets alignment properties of a file access property list
 *
 * \fapl_id
 * \param[in] threshold Threshold value. Note that setting the threshold
 *                      value to 0 (zero) has the effect of a special case,
 *                      forcing everything to be aligned
 * \param[in] alignment Alignment value
 *
 * \return \herr_t
 *
 * \details H5Pset_alignment() sets the alignment properties of a
 *          file access property list so that any file object greater
 *          than or equal in size to \p threshold bytes will be aligned
 *          on an address which is a multiple of \p alignment. The
 *          addresses are relative to the end of the user block; the
 *          alignment is calculated by subtracting the user block size
 *          from the absolute file address and then adjusting the address
 *          to be a multiple of \p alignment.
 *
 *          Default values for \p threshold and \p alignment are one,
 *          implying no alignment. Generally the default values will
 *          result in the best performance for single-process access to
 *          the file. For MPI IO and other parallel systems, choose an
 *          alignment which is a multiple of the disk block size.
 *
 *          If the file space handling strategy is set to
 *          #H5F_FSPACE_STRATEGY_PAGE, then the alignment set via this
 *          routine is ignored. The file space handling strategy is set
 *          by H5Pset_file_space_strategy().
 *
 * \since 1.0.0
 *
 */
H5_DLL herr_t H5Pset_alignment(hid_t fapl_id, hsize_t threshold,
    hsize_t alignment);
/**
 *-------------------------------------------------------------------------
 *
 * \ingroup FAPL
 *
 * \brief Sets the raw data chunk cache parameters
 *
 * \fapl_id{plist_id}
 * \param[in] mdc_nelmts No longer used; any value passed is ignored
 * \param[in] rdcc_nslots The number of chunk slots in the raw data chunk
 *                        cache for this dataset. Increasing this value
 *                        reduces the number of cache collisions, but
 *                        slightly increases the memory used. Due to the
 *                        hashing strategy, this value should ideally be a
 *                        prime number. As a rule of thumb, this value
 *                        should be at least 10 times the number of chunks
 *                        that can fit in \p rdcc_nbytes bytes. For
 *                        maximum performance, this value should be set
 *                        approximately 100 times that number of chunks.
 *                        The default value is 521.
 * \param[in] rdcc_nbytes Total size of the raw data chunk cache in bytes.
 *                        The default size is 1 MB per dataset.
 * \param[in] rdcc_w0     The chunk preemption policy for all datasets.
 *                        This must be between 0 and 1 inclusive and
 *                        indicates the weighting according to which chunks
 *                        which have been fully read or written are
 *                        penalized when determining which chunks to flush
 *                        from cache. A value of 0 means fully read or
 *                        written chunks are treated no differently than
 *                        other chunks (the preemption is strictly LRU)
 *                        while a value of 1 means fully read or written
 *                        chunks are always preempted before other chunks.
 *                        If your application only reads or writes data once,
 *                        this can be safely set to 1. Otherwise, this should
 *                        be set lower depending on how often you re-read or
 *                        re-write the same data. The default value is 0.75.
 *                        If the value passed is #H5D_CHUNK_CACHE_W0_DEFAULT,
 *                        then the property will not be set on the dataset
 *                        access property list, and the parameter will come
 *                        from the file access property list.
 *
 * \return \herr_t
 *
 * \details H5Pset_cache() sets the number of elements, the total number of
 *          bytes, and the preemption policy value for all datasets in a file
 *          on the file’s file access property list.
 *
 *          The raw data chunk cache inserts chunks into the cache by first
 *          computing a hash value using the address of a chunk and then by
 *          using that hash value as the chunk’s index into the table of
 *          cached chunks. In other words, the size of this hash table and the
 *          number of possible hash values is determined by the \p rdcc_nslots
 *          parameter. If a different chunk in the cache has the same hash value,
 *          a collision will occur, which will reduce efficiency. If inserting
 *          the chunk into the cache would cause the cache to be too big, then
 *          the cache will be pruned according to the \p rdcc_w0 parameter.
 *
 *          The \p mdc_nelmts parameter is no longer used; any value passed
 *          in that parameter will be ignored.
 *
 * \note \b Motivation: Setting raw data chunk cache parameters
 *       can be done with H5Pset_cache(), H5Pset_chunk_cache(),
 *       or a combination of both. H5Pset_cache() is used to
 *       adjust the chunk cache parameters for all datasets via
 *       a global setting for the file, and H5Pset_chunk_cache()
 *       is used to adjust the chunk cache parameters for
 *       individual datasets. When both are used, parameters
 *       set with H5Pset_chunk_cache() will override any parameters
 *       set with H5Pset_cache().
 *
 * \note Optimum chunk cache parameters may vary widely depending
 *       on different data layout and access patterns. For datasets
 *       with low performance requirements for example, changing
 *       the cache settings can save memory.
 *
 * \note Note: Raw dataset chunk caching is not currently
 *       supported when using the MPI I/O and MPI POSIX file drivers
 *       in read/write mode; see H5Pset_fapl_mpio() and
 *       H5Pset_fapl_mpiposix(), respectively. When using one of these
 *       file drivers, all calls to H5Dread() and H5Dwrite() will access
 *       the disk directly, and H5Pset_cache() will have no effect on
 *       performance.
 *
 * \note Raw dataset chunk caching is supported when these drivers are
 *       used in read-only mode.
 *
 * \todo Check on H5Pset_fapl_mpio() and H5Pset_fapl_mpiposix().
 *
 * \version 1.8.0 The use of the \p mdc_nelmts parameter was discontinued.
 *                Metadata cache configuration is managed with
 *                H5Pset_mdc_config() and H5Pget_mdc_config().
 * \version 1.6.0 The \p rdcc_nbytes and \p rdcc_nelmts parameters
 *                changed from type int to size_t.
 * \since 1.0.0
 *
 */
H5_DLL herr_t H5Pset_cache(hid_t plist_id, int mdc_nelmts,
       size_t rdcc_nslots, size_t rdcc_nbytes,
       double rdcc_w0);
/**
 *-------------------------------------------------------------------------
 *
 * \ingroup FAPL
 *
 * \brief Sets a file driver
 *
 * \plist_id
 * \param[in] driver_id   The new driver identifier
 * \param[in] driver_info Optional struct containing driver properties
 *
 * \return \herr_t
 *
 * \details H5Pset_driver() sets the file driver, driver_id, for a file
 *          access or data transfer property list, \p plist_id, and
 *          supplies an optional struct containing the driver-specific
 *          properties, \p driver_info.
 *
 *          The driver properties will be copied into the property list
 *          and the reference count on the driver will be incremented,
 *          allowing the caller to close the driver identifier but still
 *          use the property list.
 *
 * \version 1.8.2 Function publicized in this release; previous releases
 *                described this function only in the virtual file driver
 *                documentation.
 *
 */
H5_DLL herr_t H5Pset_driver(hid_t plist_id, hid_t driver_id,
        const void *driver_info);
/**
 *-------------------------------------------------------------------------
 *
 * \ingroup FAPL
 *
 * \brief Sets the file close degree
 *
 * \fapl_id
 * \param[in] degree Pointer to a location containing the file close
 *           degree property, the value of \p degree
 *
 * \return \herr_t
 *
 * \details H5Pset_fclose_degree() sets the file close degree property
 *          \p degree in the file access property list \p fapl_id.
 *
 *          The value of \p degree determines how aggressively
 *          H5Fclose() deals with objects within a file that remain open
 *          when H5Fclose() is called to close that file. \p degree can
 *          have any one of four valid values:
 *
 *          <table>
 *           <tr>
 *            <th>Degree name</th>
 *            <th>H5Fclose behavior with no open object in file</th>
 *            <th>H5Fclose behavior with open object(s) in file</th>
 *           </tr>
 *           <tr>
 *            <td>#H5F_CLOSE_WEAK</td>
 *            <td>Actual file is closed.</td>
 *            <td>Access to file identifier is terminated; actual file
 *                close is delayed until all objects in file are closed
 *            </td>
 *           </tr>
 *           <tr>
 *            <td>#H5F_CLOSE_SEMI</td>
 *            <td>Actual file is closed.</td>
 *            <td>Function returns FAILURE</td>
 *           </tr>
 *           <tr>
 *            <td>#H5F_CLOSE_STRONG</td>
 *            <td>Actual file is closed.</td>
 *            <td>All open objects remaining in the file are closed then
 *                file is closed</td>
 *           </tr>
 *           <tr>
 *            <td>#H5F_CLOSE_DEFAULT</td>
 *            <td>The VFL driver chooses the behavior. Currently, all VFL
 *            drivers set this value to #H5F_CLOSE_WEAK, except for the
 *            MPI-I/O driver, which sets it to #H5F_CLOSE_SEMI.</td>
 *            <td></td>
 *           </tr>
 *
 *          </table>
 * \warning If a file is opened multiple times without being closed, each
 *          open operation must use the same file close degree setting.
 *          For example, if a file is already open with #H5F_CLOSE_WEAK,
 *          an H5Fopen() call with #H5F_CLOSE_STRONG will fail.
 *
 * \since 1.6.0
 *
 */
H5_DLL herr_t H5Pset_fclose_degree(hid_t fapl_id, H5F_close_degree_t degree);
/**
 *-------------------------------------------------------------------------
 *
 * \ingroup FAPL
 *
 * \brief Controls the range of library release versions used when creating
 *        objects in a file
 *
 * \fapl_id{plist_id}
 * \param[in] low  The earliest version of the library that will be used
 *                 for writing objects
 * \param[in] high The latest version of the library that will be used for
 *                 writing objects
 *
 * \return \herr_t
 *
 * \details H5Pset_libver_bounds() controls the range of library release
 *          versions that will be used when creating objects in a file.
 *          The object format versions are determined indirectly from the
 *          library release versions specified in the call.
 *
 *          This property is set in the file access property list
 *          specified by the parameter \p fapl_id.
 *
 *          The parameter \p low sets the earliest possible format
 *          versions that the library will use when creating objects in
 *          the file.  Note that earliest possible is different from
 *          earliest, as some features introduced in library versions
 *          later than 1.0.0 resulted in updates to object formats.
 *          The parameter \p high sets the latest format versions that
 *          the library will be allowed to use when creating objects in
 *          the file.
 *
 *          The parameters \p low and \p high must be one of the
 *          enumerated values in the #H5F_libver_t struct, which is
 *          defined in H5Fpublic.h.
 *
 *          The macro #H5F_LIBVER_LATEST is aliased to the highest
 *          enumerated value in #H5F_libver_t, indicating that this is
 *          currently the latest format available.
 *
 *          The library supports the following five pairs of
 *          (\p low, \p high) combinations as derived from the values
 *          in #H5F_libver_t:
 *
 *          <table>
 *           <tr>
 *            <th>Value of \p low and \p high</th>
 *            <th>Result</th>
 *           </tr>
 *           <tr>
 *            <td>\p low=#H5F_LIBVER_EARLIEST<br />
 *                \p high=#H5F_LIBVER_V18</td>
 *            <td>
 *              \li The library will create objects with the earliest
 *                  possible format versions.
 *              \li The library will allow objects to be created with the
 *                  latest format versions available to library release 1.8.x.
 *              \li API calls that create objects or features that are
 *                  available to versions of the library greater than 1.8.x
 *                  release will fail.
 *             </td>
 *           </tr>
 *           <tr>
 *            <td>\p low=#H5F_LIBVER_EARLIEST<br />
 *                \p high=#H5F_LIBVER_V110</td>
 *            <td>
 *             \li The library will create objects with the earliest possible
 *                 format versions.
 *             \li The library will allow objects to be created with the latest
 *                 format versions available to library release 1.10.x.
 *                 Since 1.10.x is also #H5F_LIBVER_LATEST, there is no upper
 *                 limit on the format versions to use.  For example, if a newer
 *                 format version is required to support a feature e.g. virtual
 *                 dataset, this setting will allow the object to be created.
 *             \li This is the library default setting and provides the greatest
 *                 format compatibility.
 *            </td>
 *           </tr>
 *           <tr>
 *            <td>\p low=#H5F_LIBVER_V18<br />
 *                \p high=#H5F_LIBVER_V18</td>
 *            <td>
 *             \li The library will create objects with the latest format versions
 *                 available to library release 1.8.x.
 *             \li API calls that create objects or features that are available
 *                 to versions of the library greater than 1.8.x release will fail.
 *             \li Earlier versions of the library may not be able to access
 *                 objects created with this setting.</td>
 *           </tr>
 *           <tr>
 *            <td>\p low=#H5F_LIBVER_V18<br />
 *                \p high=#H5F_LIBVER_V110</td>
 *            <td>
 *              \li The library will create objects with the latest format versions
 *                  available to library release 1.8.x.
 *              \li The library will allow objects to be created with the latest
 *                  format versions available to library release 1.10.x.
 *                  Since 1.10.x is also #H5F_LIBVER_LATEST, there is no upper limit
 *                  on the format versions to use.  For example, if a newer format
 *                  version is required to support a feature e.g. virtual dataset,
 *                  this setting will allow the object to be created.
 *              \li Earlier versions of the library may not be able to access
 *                  objects created with this setting.</td>
 *           </tr>
 *           <tr>
 *            <td>\p low=#H5F_LIBVER_V110<br />
 *                \p high=#H5F_LIBVER_V110
 *             </td>
 *             <td>
 *              \li The library will create objects with the latest format versions
 *                  available to library release 1.10.x.
 *              \li The library will allow objects to be created with the latest
 *                  format versions available to library release 1.10.x.
 *                  Since 1.10.x is also #H5F_LIBVER_LATEST, there is no upper limit
 *                  on the format versions to use. For example, if a newer format
 *                  version is required to support a feature e.g. virtual dataset,
 *                  this setting will allow the object to be created.
 *              \li This setting allows users to take advantage of the latest
 *                  features and performance enhancements in the library. However,
 *                  objects written with this setting may be accessible to a smaller
 *                  range of library versions than would be the case if low is set
 *                  to #H5F_LIBVER_EARLIEST.
 *              \li Earlier versions of the library may not be able to access objects created with this setting.
 *            </td>
 *           </tr>
 *          </table>
 *
 * \version 1.10.2 #H5F_LIBVER_V18 added to the enumerated defines in #H5F_libver_t.
 *
 * \since 1.8.0
 *
 */
H5_DLL herr_t H5Pset_libver_bounds(hid_t plist_id, H5F_libver_t low,
    H5F_libver_t high);
H5_DLL herr_t H5Pset_mdc_config(hid_t    plist_id,
       H5AC_cache_config_t * config_ptr);
H5_DLL herr_t H5Pget_mdc_config(hid_t     plist_id,
       H5AC_cache_config_t * config_ptr);    /* out */
H5_DLL herr_t H5Pset_gc_references(hid_t fapl_id, unsigned gc_ref);
H5_DLL herr_t H5Pget_gc_references(hid_t fapl_id, unsigned *gc_ref/*out*/);
H5_DLL herr_t H5Pset_meta_block_size(hid_t fapl_id, hsize_t size);
H5_DLL herr_t H5Pget_meta_block_size(hid_t fapl_id, hsize_t *size/*out*/);
H5_DLL herr_t H5Pset_sieve_buf_size(hid_t fapl_id, size_t size);
H5_DLL herr_t H5Pget_sieve_buf_size(hid_t fapl_id, size_t *size/*out*/);
H5_DLL herr_t H5Pset_small_data_block_size(hid_t fapl_id, hsize_t size);
H5_DLL herr_t H5Pget_small_data_block_size(hid_t fapl_id, hsize_t *size/*out*/);
H5_DLL herr_t H5Pset_elink_file_cache_size(hid_t plist_id, unsigned efc_size);
H5_DLL herr_t H5Pget_elink_file_cache_size(hid_t plist_id, unsigned *efc_size);
H5_DLL herr_t H5Pset_file_image(hid_t fapl_id, void *buf_ptr, size_t buf_len);
H5_DLL herr_t H5Pget_file_image(hid_t fapl_id, void **buf_ptr_ptr, size_t *buf_len_ptr);
H5_DLL herr_t H5Pset_file_image_callbacks(hid_t fapl_id,
       H5FD_file_image_callbacks_t *callbacks_ptr);
H5_DLL herr_t H5Pget_file_image_callbacks(hid_t fapl_id,
       H5FD_file_image_callbacks_t *callbacks_ptr);
H5_DLL herr_t H5Pset_core_write_tracking(hid_t fapl_id, hbool_t is_enabled, size_t page_size);
H5_DLL herr_t H5Pget_core_write_tracking(hid_t fapl_id, hbool_t *is_enabled, size_t *page_size);
H5_DLL herr_t H5Pset_metadata_read_attempts(hid_t plist_id, unsigned attempts);
H5_DLL herr_t H5Pget_metadata_read_attempts(hid_t plist_id, unsigned *attempts);
H5_DLL herr_t H5Pset_object_flush_cb(hid_t plist_id, H5F_flush_cb_t func, void *udata);
H5_DLL herr_t H5Pget_object_flush_cb(hid_t plist_id, H5F_flush_cb_t *func, void **udata);
H5_DLL herr_t H5Pset_mdc_log_options(hid_t plist_id, hbool_t is_enabled, const char *location, hbool_t start_on_access);
H5_DLL herr_t H5Pget_mdc_log_options(hid_t plist_id, hbool_t *is_enabled, char *location, size_t *location_size, hbool_t *start_on_access);
H5_DLL herr_t H5Pset_evict_on_close(hid_t fapl_id, hbool_t evict_on_close);
H5_DLL herr_t H5Pget_evict_on_close(hid_t fapl_id, hbool_t *evict_on_close);
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
H5_DLL herr_t H5Pset_page_buffer_size(hid_t plist_id, size_t buf_size, unsigned min_meta_per, unsigned min_raw_per);
H5_DLL herr_t H5Pget_page_buffer_size(hid_t plist_id, size_t *buf_size, unsigned *min_meta_per, unsigned *min_raw_per);

/* Dataset creation property list (DCPL) routines */
/**
 *-------------------------------------------------------------------------
 *
 * \ingroup DCPL
 *
 * \brief Determines whether fill value is defined
 *
 * \dcpl_id{plist}
 * \param[out] status Status of fill value in property list
 *
 * \return \herr_t
 *
 * \details H5Pfill_value_defined() determines whether a fill value is
 *          defined in the dataset creation property list \p plist. Valid
 *          values returned in status are as follows:
 *
 *          <table>
 *           <tr>
 *            <td>#H5D_FILL_VALUE_UNDEFINED</td>
 *            <td>Fill value is undefined.</td>
 *           </tr>
 *           <tr>
 *            <td>#H5D_FILL_VALUE_DEFAULT</td>
 *            <td>Fill value is the library default.</td>
 *           </tr>
 *           <tr>
 *            <td>#H5D_FILL_VALUE_USER_DEFINED</td>
 *            <td>Fill value is defined by the application.</td>
 *           </tr>
 *          </table>
 *
 * \since 1.6.0
 *
 */
H5_DLL herr_t H5Pfill_value_defined(hid_t plist, H5D_fill_value_t *status);
/**
 *-------------------------------------------------------------------------
 *
 * \ingroup DCPL
 *
 * \brief Retrieves the timing for storage space allocation
 *
 * \dcpl_id{plist_id}
 * \param[out] alloc_time The timing setting for allocating dataset
 *                        storage space
 *
 * \return \herr_t
 *
 * \details H5Pget_alloc_time() retrieves the timing for allocating storage
 *          space for a dataset's raw data. This property is set in the
 *          dataset creation property list \p plist_id. The timing setting
 *          is returned in \p alloc_time as one of the following values:
 *
 *          <table>
 *           <tr>
 *            <td>#H5D_ALLOC_TIME_DEFAULT</td>
 *            <td>Uses the default allocation time, based on the dataset
 *                storage method. <br />See the \p alloc_time description in
 *                H5Pset_alloc_time() for default allocation times for
 *                various storage methods.</td>
 *           </tr>
 *           <tr>
 *            <td>#H5D_ALLOC_TIME_EARLY</td>
 *            <td>All space is allocated when the dataset is created.</td>
 *           </tr>
 *           <tr>
 *            <td>#H5D_ALLOC_TIME_INCR</td>
 *            <td>Space is allocated incrementally as data is written
 *                to the dataset.</td>
 *           </tr>
 *           <tr>
 *            <td>#H5D_ALLOC_TIME_LATE</td>
 *            <td>All space is allocated when data is first written to
 *                the dataset.</td>
 *           </tr>
 *          </table>
 *
 * \note H5Pget_alloc_time() is designed to work in concert with the
 *       dataset fill value and fill value write time properties, set
 *       with the functions H5Pget_fill_value() and H5Pget_fill_time().
 *
 * \since 1.6.0
 *
 */
H5_DLL herr_t H5Pget_alloc_time(hid_t plist_id, H5D_alloc_time_t
    *alloc_time/*out*/);
/**
 *-------------------------------------------------------------------------
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
H5_DLL int H5Pget_chunk(hid_t plist_id, int max_ndims, hsize_t dim[]/*out*/);
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
 * \brief Retrieves the setting for whether or not to create minimized
 *        dataset object headers
 *
 * \dcpl_id
 * \param[out] minimize  Flag indicating whether the library will or will
 *                       not create minimized dataset object headers
 *
 * \return \herr_t
 *
 * \details H5Pget_dset_no_attrs_hint() retrieves the
 *          <i>no dataset attributes</i> hint setting for the dataset
 *          creation property list \p dcpl_id. This setting is used to
 *          inform the library to create minimized dataset object headers
 *          when TRUE. The setting value is returned in the boolean pointer
 *          \p minimize.
 *
 * \since 1.10.5
 *
 */
H5_DLL herr_t H5Pget_dset_no_attrs_hint(hid_t dcpl_id, hbool_t *minimize);
/**
 *-------------------------------------------------------------------------
 *
 * \ingroup DCPL
 *
 * \brief Returns information about an external file
 *
 * \dcpl_id{plist_id}
 * \param[in]  idx       External file index
 * \param[in]  name_size Maximum length of \p name array
 * \param[out] name      Name of the external file
 * \param[out] offset    Pointer to a location to return an offset value
 * \param[out] size      Pointer to a location to return the size of the
 *                       external file data
 *
 * \return \herr_t
 *
 * \details H5Pget_external() returns information about an external file.
 *          The external file is specified by its index, \p idx, which
 *          is a number from zero to N-1, where N is the value returned
 *          by H5Pget_external_count(). At most \p name_size characters
 *          are copied into the \p name array. If the external file name
 *          is longer than \p name_size with the null terminator, the
 *          return value is not null terminated (similar to strncpy()).
 *
 *          If \p name_size is zero or \p name is the null pointer, the
 *          external file name is not returned. If \p offset or \p size
 *          are null pointers then the corresponding information is not
 *          returned.
 *
 * \version 1.6.4 \p idx parameter type changed to unsigned.
 * \since 1.0.0
 *
 */
H5_DLL herr_t H5Pget_external(hid_t plist_id, unsigned idx, size_t name_size,
          char *name/*out*/, off_t *offset/*out*/,
          hsize_t *size/*out*/);
/**
 *-------------------------------------------------------------------------
 *
 * \ingroup DCPL
 *
 * \brief Returns the number of external files for a dataset
 *
 * \dcpl_id{plist_id}
 *
 * \return Returns the number of external files if successful; otherwise
 *         returns a negative value.
 *
 * \details H5Pget_external_count() returns the number of external files
 *          for the specified dataset.
 *
 * \since 1.0.0
 *
 */
H5_DLL int H5Pget_external_count(hid_t plist_id);
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
H5_DLL herr_t H5Pget_fill_time(hid_t plist_id, H5D_fill_time_t
    *fill_time/*out*/);
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
H5_DLL herr_t H5Pget_fill_value(hid_t plist_id, hid_t type_id,
     void *value/*out*/);
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
 *
 * \ingroup DCPL
 *
 * \brief Gets the number of mappings for the virtual dataset
 *
 * \dcpl_id
 * \param[out] count The number of mappings
 *
 * \return \herr_t
 *
 * \details H5Pget_virtual_count() gets the number of mappings for a
 *          virtual dataset that has the creation property list specified
 *          by \p dcpl_id.
 *
 * \virtual
 *
 * \since 1.10.0
 *
 */
H5_DLL herr_t H5Pget_virtual_count(hid_t dcpl_id, size_t *count/*out*/);
/**
 *-------------------------------------------------------------------------
 *
 * \ingroup DCPL
 *
 * \brief Gets the name of a source dataset used in the mapping
 *
 * \dcpl_id
 * \param[in]  index Mapping index. The value of \p index is 0 (zero) or
 *                   greater and less than \p count
 *                   (0 ≤ \p index < \p count), where \p count is the
 *                   number of mappings returned by H5Pget_virtual_count().
 * \param[out] name  A buffer containing the name of the source dataset
 * \param[in]  size  The size, in bytes, of the name buffer. Must be the
 *                   size of the dataset name in bytes plus 1 for a NULL
 *                   terminator
 *
 * \return Returns the length of the dataset name if successful;
 *         otherwise returns a negative value.
 *
 * \details H5Pget_virtual_dsetname() takes the dataset creation property
 *          list for the virtual dataset, \p dcpl_id, the mapping index,
 *          \p index, the size of the dataset name for a source dataset,
 *          \p size, and retrieves the name of the source dataset used in
 *          the mapping.
 *
 *          Up to \p size characters of the dataset name are returned in
 *          \p name; additional characters, if any, are not returned to
 *          the user application.
 *
 *          If the length of the dataset name, which determines the
 *          required value of \p size, is unknown, a preliminary call
 *          to H5Pget_virtual_dsetname() with the last two parameters
 *          set to NULL and zero respectively can be made. The return
 *          value of this call will be the size in bytes of the dataset
 *          name. That value, plus 1 for a NULL terminator, must then be
 *          assigned to \p size for a second H5Pget_virtual_dsetname()
 *          call, which will retrieve the actual dataset name.
 *
 * \virtual
 *
 * \since 1.10.0
 *
 */
H5_DLL ssize_t H5Pget_virtual_dsetname(hid_t dcpl_id, size_t index,
    char *name/*out*/, size_t size);
/**
 *-------------------------------------------------------------------------
 *
 * \ingroup DCPL
 *
 * \brief Gets the filename of a source dataset used in the mapping
 *
 * \dcpl_id
 * \param[in]  index Mapping index. The value of \p index is 0 (zero) or
 *                   greater and less than \p count
 *                   (0 ≤ \p index < \p count), where \p count is the
 *                   number of mappings returned by H5Pget_virtual_count().
 * \param[out] name  A buffer containing the name of the file containing
 *                   the source dataset
 * \param[in]  size  The size, in bytes, of the name buffer. Must be the
 *                   size of the filename in bytes plus 1 for a NULL
 *                   terminator
 *
 * \return Returns the length of the filename if successful; otherwise
 *         returns a negative value.
 *
 * \details H5Pget_virtual_filename() takes the dataset creation property
 *          list for the virtual dataset, \p dcpl_id, the mapping index,
 *          \p index, the size of the filename for a source dataset,
 *          \p size, and retrieves the name of the file for a source dataset
 *          used in the mapping.
 *
 *          Up to \p size characters of the filename are returned in
 *          \p name; additional characters, if any, are not returned to
 *          the user application.
 *
 *          If the length of the filename, which determines the required
 *          value of \p size, is unknown, a preliminary call to
 *          H5Pget_virtual_filename() with the last two parameters set
 *          to NULL and zero respectively can be made. The return value
 *          of this call will be the size in bytes of the filename. That
 *          value, plus 1 for a NULL terminator, must then be assigned to
 *          \p size for a second H5Pget_virtual_filename() call, which
 *          will retrieve the actual filename.
 *
 * \virtual
 *
 * \since 1.10.0
 *
 */
H5_DLL ssize_t H5Pget_virtual_filename(hid_t dcpl_id, size_t index,
    char *name/*out*/, size_t size);
/**
 *-------------------------------------------------------------------------
 *
 * \ingroup DCPL
 *
 * \brief Gets a dataspace identifier for the selection within the source
 *        dataset used in the mapping
 *
 * \dcpl_id
 * \param[in] index Mapping index. The value of \p index is 0 (zero) or
 *                  greater and less than \p count
 *                  (0 ≤ \p index < \p count), where \p count is the number
 *                  of mappings returned by H5Pget_virtual_count().
 *
 * \return \hid_t{valid dataspace identifier}
 *
 * \details H5Pget_virtual_srcspace() takes the dataset creation property
 *          list for the virtual dataset, \p dcpl_id, and the mapping
 *          index, \p index, and returns a dataspace identifier for the
 *          selection within the source dataset used in the mapping.
 *
 * \virtual
 *
 * \since 1.10.0
 *
 */
H5_DLL hid_t H5Pget_virtual_srcspace(hid_t dcpl_id, size_t index);
/**
 *-------------------------------------------------------------------------
 *
 * \ingroup DCPL
 *
 * \brief Gets a dataspace identifier for the selection within the virtual
 *        dataset used in the mapping
 *
 * \dcpl_id
 * \param[in] index Mapping index. The value of \p index is 0 (zero) or
 *                  greater and less than \p count
 *                  (0 ≤ \p index < \p count), where \p count is the number
 *                  of mappings returned by H5Pget_virtual_count()
 *
 * \return \hid_t{valid dataspace identifier}
 *
 * \details H5Pget_virtual_vspace() takes the dataset creation property
 *          list for the virtual dataset, \p dcpl_id, and the mapping
 *          index, \p index, and returns a dataspace identifier for the
 *          selection within the virtual dataset used in the mapping.
 *
 * \virtual
 *
 * \since 1.10.0
 *
 */
H5_DLL hid_t H5Pget_virtual_vspace(hid_t dcpl_id, size_t index);
/**
 *-------------------------------------------------------------------------
 *
 * \ingroup DCPL
 *
 * \brief Sets the timing for storage space allocation
 *
 * \dcpl_id{plist_id}
 * \param[in] alloc_time When to allocate dataset storage space
 *
 * \return \herr_t
 *
 * \details H5Pset_alloc_time() sets up the timing for the allocation of
 *          storage space for a dataset's raw data. This property is set
 *          in the dataset creation property list \p plist_id. Timing is
 *          specified in \p alloc_time with one of the following values:
 *
 *          <table>
 *           <tr>
 *            <td>#H5D_ALLOC_TIME_DEFAULT</td>
 *            <td>Allocate dataset storage space at the default time<br />
 *                (Defaults differ by storage method.)</td>
 *           </tr>
 *           <tr>
 *            <td>#H5D_ALLOC_TIME_EARLY</td>
 *            <td>Allocate all space when the dataset is created<br />
 *            (Default for compact datasets.)</td>
 *           </tr>
 *           <tr>
 *            <td>#H5D_ALLOC_TIME_INCR</td>
 *            <td>Allocate space incrementally, as data is written to
 *                the dataset<br />(Default for chunked storage datasets.)
 *
 *                \li Chunked datasets: Storage space allocation for each
 *                    chunk is deferred until data is written to the chunk.
 *                \li Contiguous datasets: Incremental storage space
 *                    allocation for contiguous data is treated as late
 *                    allocation.
 *                \li Compact datasets: Incremental allocation is not
 *                    allowed with compact datasets; H5Pset_alloc_time()
 *                    will return an error.</td>
 *           </tr>
 *           <tr>
 *            <td>#H5D_ALLOC_TIME_LATE</td>
 *            <td>Allocate all space when data is first written to the
 *                dataset<br />
 *                (Default for contiguous datasets.)</td>
 *           </tr>
 *          </table>
 *
 * \note H5Pset_alloc_time() is designed to work in concert with the
 *       dataset fill value and fill value write time properties, set
 *       with the functions H5Pset_fill_value() and H5Pset_fill_time().
 *
 * \note See H5Dcreate() for further cross-references.
 *
 * \since 1.6.0
 *
 */
H5_DLL herr_t H5Pset_alloc_time(hid_t plist_id, H5D_alloc_time_t
    alloc_time);
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
 * \brief Sets the flag to create minimized dataset object headers
 *
 * \dcpl_id
 * \param[in] minimize Flag for indicating whether or not a dataset's
 *                     object header will be minimized
 *
 * \return \herr_t
 *
 * \details H5Pset_dset_no_attrs_hint() sets the no dataset attributes
 *          hint setting for the dataset creation property list \p dcpl_id.
 *          Datasets created with the dataset creation property list
 *          \p dcpl_id will have their object headers minimized if the
 *          boolean flag \p minimize is set to TRUE. By setting \p minimize
 *          to TRUE, the library expects that no attributes will be added
 *          to the dataset. Attributes can be added, but they are appended
 *          with a continuation message, which can reduce performance.
 *
 *          This setting interacts with H5Fset_dset_no_attrs_hint(): if
 *          either is set to TRUE, then the created dataset's object header
 *          will be minimized.
 *
 * \since 1.10.5
 *
 */
H5_DLL herr_t H5Pset_dset_no_attrs_hint(hid_t dcpl_id, hbool_t minimize);
/**
 *-------------------------------------------------------------------------
 *
 * \ingroup DCPL
 *
 * \brief Adds an external file to the list of external files
 *
 * \dcpl_id{plist_id}
 * \param[in] name   Name of an external file
 * \param[in] offset Offset, in bytes, from the beginning of the file to
 *                   the location in the file where the data starts
 * \param[in] size   Number of bytes reserved in the file for the data
 *
 * \return \herr_t
 *
 * \details The first call to H5Pset_external() sets the external
 *          storage property in the property list, thus designating that
 *          the dataset will be stored in one or more non-HDF5 file(s)
 *          external to the HDF5 file. This call also adds the file
 *          \p name as the first file in the list of external files.
 *          Subsequent calls to the function add the named file as the
 *          next file in the list.
 *
 *          If a dataset is split across multiple files, then the files
 *          should be defined in order. The total size of the dataset is
 *          the sum of the \p size arguments for all the external files.
 *          If the total size is larger than the size of a dataset then
 *          the dataset can be extended (provided the data space also
 *          allows the extending).
 *
 *         The \p size argument specifies the number of bytes reserved
 *         for data in the external file. If \p size is set to
 *         #H5F_UNLIMITED, the external file can be of unlimited size
 *         and no more files can be added to the external files list.
 *         If \p size is set to 0 (zero), no external file will actually
 *         be created.
 *
 *         All of the external files for a given dataset must be specified
 *         with H5Pset_external() before H5Dcreate() is called to create
 *         the dataset. If one these files does not exist on the system
 *         when H5Dwrite() is called to write data to it, the library
 *         will create the file.
 *
 * \since 1.0.0
 *
 */
H5_DLL herr_t H5Pset_external(hid_t plist_id, const char *name, off_t offset,
          hsize_t size);
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
H5_DLL herr_t H5Pset_virtual(hid_t dcpl_id, hid_t vspace_id,
    const char *src_file_name, const char *src_dset_name, hid_t src_space_id);
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
H5_DLL herr_t H5Pset_szip(hid_t plist_id, unsigned options_mask, unsigned pixels_per_block);
H5_DLL herr_t H5Pset_shuffle(hid_t plist_id);
H5_DLL herr_t H5Pset_nbit(hid_t plist_id);
H5_DLL herr_t H5Pset_scaleoffset(hid_t plist_id, H5Z_SO_scale_type_t scale_type, int scale_factor);
H5_DLL herr_t H5Pset_fill_value(hid_t plist_id, hid_t type_id,
     const void *value);

/* Dataset access property list (DAPL) routines */
H5_DLL herr_t H5Pset_chunk_cache(hid_t dapl_id, size_t rdcc_nslots,
       size_t rdcc_nbytes, double rdcc_w0);
H5_DLL herr_t H5Pget_chunk_cache(hid_t dapl_id,
       size_t *rdcc_nslots/*out*/,
       size_t *rdcc_nbytes/*out*/,
       double *rdcc_w0/*out*/);
H5_DLL herr_t H5Pset_virtual_view(hid_t plist_id, H5D_vds_view_t view);
H5_DLL herr_t H5Pget_virtual_view(hid_t plist_id, H5D_vds_view_t *view);
H5_DLL herr_t H5Pset_virtual_printf_gap(hid_t plist_id, hsize_t gap_size);
H5_DLL herr_t H5Pget_virtual_printf_gap(hid_t plist_id, hsize_t *gap_size);
H5_DLL herr_t H5Pset_virtual_prefix(hid_t dapl_id, const char* prefix);
H5_DLL ssize_t H5Pget_virtual_prefix(hid_t dapl_id, char* prefix /*out*/, size_t size);
H5_DLL herr_t H5Pset_append_flush(hid_t plist_id, unsigned ndims,
    const hsize_t boundary[], H5D_append_cb_t func, void *udata);
H5_DLL herr_t H5Pget_append_flush(hid_t plist_id, unsigned dims,
    hsize_t boundary[], H5D_append_cb_t *func, void **udata);
H5_DLL herr_t H5Pset_efile_prefix(hid_t dapl_id, const char* prefix);
H5_DLL ssize_t H5Pget_efile_prefix(hid_t dapl_id, char* prefix /*out*/, size_t size);

/* Dataset xfer property list (DXPL) routines */
H5_DLL herr_t H5Pset_data_transform(hid_t plist_id, const char* expression);
H5_DLL ssize_t H5Pget_data_transform(hid_t plist_id, char* expression /*out*/, size_t size);
H5_DLL herr_t H5Pset_buffer(hid_t plist_id, size_t size, void *tconv,
        void *bkg);
H5_DLL size_t H5Pget_buffer(hid_t plist_id, void **tconv/*out*/,
        void **bkg/*out*/);
H5_DLL herr_t H5Pset_preserve(hid_t plist_id, hbool_t status);
H5_DLL int H5Pget_preserve(hid_t plist_id);
H5_DLL herr_t H5Pset_edc_check(hid_t plist_id, H5Z_EDC_t check);
H5_DLL H5Z_EDC_t H5Pget_edc_check(hid_t plist_id);
H5_DLL herr_t H5Pset_filter_callback(hid_t plist_id, H5Z_filter_func_t func,
                                     void* op_data);
H5_DLL herr_t H5Pset_btree_ratios(hid_t plist_id, double left, double middle,
       double right);
H5_DLL herr_t H5Pget_btree_ratios(hid_t plist_id, double *left/*out*/,
       double *middle/*out*/,
       double *right/*out*/);
H5_DLL herr_t H5Pset_vlen_mem_manager(hid_t plist_id,
                                       H5MM_allocate_t alloc_func,
                                       void *alloc_info, H5MM_free_t free_func,
                                       void *free_info);
H5_DLL herr_t H5Pget_vlen_mem_manager(hid_t plist_id,
                                       H5MM_allocate_t *alloc_func,
                                       void **alloc_info,
                                       H5MM_free_t *free_func,
                                       void **free_info);
H5_DLL herr_t H5Pset_hyper_vector_size(hid_t fapl_id, size_t size);
H5_DLL herr_t H5Pget_hyper_vector_size(hid_t fapl_id, size_t *size/*out*/);
H5_DLL herr_t H5Pset_type_conv_cb(hid_t dxpl_id, H5T_conv_except_func_t op, void* operate_data);
H5_DLL herr_t H5Pget_type_conv_cb(hid_t dxpl_id, H5T_conv_except_func_t *op, void** operate_data);
#ifdef H5_HAVE_PARALLEL
H5_DLL herr_t H5Pget_mpio_actual_chunk_opt_mode(hid_t plist_id, H5D_mpio_actual_chunk_opt_mode_t *actual_chunk_opt_mode);
H5_DLL herr_t H5Pget_mpio_actual_io_mode(hid_t plist_id, H5D_mpio_actual_io_mode_t *actual_io_mode);
H5_DLL herr_t H5Pget_mpio_no_collective_cause(hid_t plist_id, uint32_t *local_no_collective_cause, uint32_t *global_no_collective_cause);
#endif /* H5_HAVE_PARALLEL */

/* Link creation property list (LCPL) routines */
H5_DLL herr_t H5Pset_create_intermediate_group(hid_t plist_id, unsigned crt_intmd);
H5_DLL herr_t H5Pget_create_intermediate_group(hid_t plist_id, unsigned *crt_intmd /*out*/);

/* Group creation property list (GCPL) routines */
H5_DLL herr_t H5Pset_local_heap_size_hint(hid_t plist_id, size_t size_hint);
H5_DLL herr_t H5Pget_local_heap_size_hint(hid_t plist_id, size_t *size_hint /*out*/);
H5_DLL herr_t H5Pset_link_phase_change(hid_t plist_id, unsigned max_compact, unsigned min_dense);
H5_DLL herr_t H5Pget_link_phase_change(hid_t plist_id, unsigned *max_compact /*out*/, unsigned *min_dense /*out*/);
H5_DLL herr_t H5Pset_est_link_info(hid_t plist_id, unsigned est_num_entries, unsigned est_name_len);
H5_DLL herr_t H5Pget_est_link_info(hid_t plist_id, unsigned *est_num_entries /* out */, unsigned *est_name_len /* out */);
H5_DLL herr_t H5Pset_link_creation_order(hid_t plist_id, unsigned crt_order_flags);
H5_DLL herr_t H5Pget_link_creation_order(hid_t plist_id, unsigned *crt_order_flags /* out */);

/* Map access property list (MAPL) routines */
#ifdef H5_HAVE_MAP_API
H5_DLL herr_t H5Pset_map_iterate_hints(hid_t mapl_id, size_t key_prefetch_size, size_t key_alloc_size);
H5_DLL herr_t H5Pget_map_iterate_hints(hid_t mapl_id, size_t *key_prefetch_size /*out*/, size_t *key_alloc_size /*out*/);
#endif /*  H5_HAVE_MAP_API */

/* String creation property list (STRCPL) routines */
H5_DLL herr_t H5Pset_char_encoding(hid_t plist_id, H5T_cset_t encoding);
H5_DLL herr_t H5Pget_char_encoding(hid_t plist_id, H5T_cset_t *encoding /*out*/);

/* Link access property list (LAPL) routines */
H5_DLL herr_t H5Pset_nlinks(hid_t plist_id, size_t nlinks);
H5_DLL herr_t H5Pget_nlinks(hid_t plist_id, size_t *nlinks);
H5_DLL herr_t H5Pset_elink_prefix(hid_t plist_id, const char *prefix);
H5_DLL ssize_t H5Pget_elink_prefix(hid_t plist_id, char *prefix, size_t size);
H5_DLL hid_t H5Pget_elink_fapl(hid_t lapl_id);
H5_DLL herr_t H5Pset_elink_fapl(hid_t lapl_id, hid_t fapl_id);
H5_DLL herr_t H5Pset_elink_acc_flags(hid_t lapl_id, unsigned flags);
H5_DLL herr_t H5Pget_elink_acc_flags(hid_t lapl_id, unsigned *flags);
H5_DLL herr_t H5Pset_elink_cb(hid_t lapl_id, H5L_elink_traverse_t func, void *op_data);
H5_DLL herr_t H5Pget_elink_cb(hid_t lapl_id, H5L_elink_traverse_t *func, void **op_data);

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
#define H5P_NO_CLASS            H5P_ROOT


/* Typedefs */


/* Function prototypes */
H5_DLL herr_t H5Pregister1(hid_t cls_id, const char *name, size_t size,
    void *def_value, H5P_prp_create_func_t prp_create,
    H5P_prp_set_func_t prp_set, H5P_prp_get_func_t prp_get,
    H5P_prp_delete_func_t prp_del, H5P_prp_copy_func_t prp_copy,
    H5P_prp_close_func_t prp_close);
H5_DLL herr_t H5Pinsert1(hid_t plist_id, const char *name, size_t size,
    void *value, H5P_prp_set_func_t prp_set, H5P_prp_get_func_t prp_get,
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
 * \param[in] idx Sequence number within the filter pipeline of the filter
 *                for which information is sought
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
 *          \p idx is a value between zero and N-1, as described in
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
H5_DLL H5Z_filter_t H5Pget_filter1(hid_t plist_id, unsigned idx,
    unsigned int *flags/*out*/, size_t *cd_nelmts/*out*/,
    unsigned cd_values[]/*out*/, size_t namelen, char name[]);
H5_DLL herr_t H5Pget_filter_by_id1(hid_t plist_id, H5Z_filter_t id,
    unsigned int *flags/*out*/, size_t *cd_nelmts/*out*/,
    unsigned cd_values[]/*out*/, size_t namelen, char name[]/*out*/);
H5_DLL herr_t H5Pget_version(hid_t plist_id, unsigned *boot/*out*/,
         unsigned *freelist/*out*/, unsigned *stab/*out*/,
         unsigned *shhdr/*out*/);
H5_DLL herr_t H5Pset_file_space(hid_t plist_id, H5F_file_space_type_t strategy, hsize_t threshold);
H5_DLL herr_t H5Pget_file_space(hid_t plist_id, H5F_file_space_type_t *strategy, hsize_t *threshold);
#endif /* H5_NO_DEPRECATED_SYMBOLS */

#ifdef __cplusplus
}
#endif
#endif /* _H5Ppublic_H */
