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

/*
 * Purpose: The public header file for the null testing VFD plugin.
 */
#ifndef H5FDnull_H
#define H5FDnull_H

#define H5FD_NULL      (H5FD_null_init())
#define NULL_VFD_NAME  "null_vfd_plugin"
#define NULL_VFD_VALUE ((H5FD_class_value_t)200)

#ifdef __cplusplus
extern "C" {
#endif

H5_DLL hid_t H5FD_null_init(void);

#ifdef __cplusplus
}
#endif

#endif
