/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
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
 * Purpose:	This file contains declarations which are visible only within
 *          the H5VL package.  Source files outside the H5VL package should
 *          include H5VLprivate.h instead.
 */

#if !(defined H5VL_FRIEND || defined H5VL_MODULE)
#error "Do not include this file outside the H5VL package!"
#endif

#ifndef _H5VLpkg_H
#define _H5VLpkg_H

/* Get package's private header */
#include "H5VLprivate.h" /* Generic Functions                    */

/* Other private headers needed by this file */

/**************************/
/* Package Private Macros */
/**************************/

/* # of optional operations reserved for the native VOL connector */
#define H5VL_RESERVED_NATIVE_OPTIONAL   1024

/****************************/
/* Package Private Typedefs */
/****************************/

/*****************************/
/* Package Private Variables */
/*****************************/

/******************************/
/* Package Private Prototypes */
/******************************/
H5_DLL hid_t   H5VL__register_connector(const H5VL_class_t *cls, hbool_t app_ref, hid_t vipl_id);
H5_DLL hid_t   H5VL__register_connector_by_name(const char *name, hbool_t app_ref, hid_t vipl_id);
H5_DLL hid_t   H5VL__register_connector_by_value(H5VL_class_value_t value, hbool_t app_ref, hid_t vipl_id);
H5_DLL htri_t  H5VL__is_connector_registered_by_name(const char *name);
H5_DLL htri_t  H5VL__is_connector_registered_by_value(H5VL_class_value_t value);
H5_DLL hid_t   H5VL__get_connector_id(hid_t obj_id, hbool_t is_api);
H5_DLL hid_t   H5VL__get_connector_id_by_name(const char *name, hbool_t is_api);
H5_DLL hid_t   H5VL__get_connector_id_by_value(H5VL_class_value_t value, hbool_t is_api);
H5_DLL hid_t   H5VL__peek_connector_id_by_name(const char *name);
H5_DLL hid_t   H5VL__peek_connector_id_by_value(H5VL_class_value_t value);
H5_DLL herr_t  H5VL__connector_str_to_info(const char *str, hid_t connector_id, void **info);
H5_DLL ssize_t H5VL__get_connector_name(hid_t id, char *name /*out*/, size_t size);
H5_DLL herr_t H5VL__register_opt_operation(H5VL_subclass_t subcls, const char *op_name, int *op_val);
H5_DLL size_t H5VL__num_opt_operation(void);
H5_DLL herr_t H5VL__find_opt_operation(H5VL_subclass_t subcls, const char *op_name,
    int *op_val);
H5_DLL herr_t H5VL__unregister_opt_operation(H5VL_subclass_t subcls, const char *op_name);
H5_DLL herr_t H5VL__term_opt_operation(void);

#endif /* _H5VLpkg_H */
