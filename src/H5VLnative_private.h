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
 * Purpose:	The private header file for the native VOL driver.
 */

#ifndef _H5VLnative_private_H
#define _H5VLnative_private_H

/* Include driver's public header */
#include "H5VLnative.h"

/* Initializer function for native VOL driver */
#define H5VL_NATIVE             (H5VL_native_init())


#ifdef __cplusplus
extern "C" {
#endif

H5_DLL hid_t H5VL_native_get_driver_id(void);
H5_DLL hid_t H5VL_native_init(void);
H5_DLL hid_t H5VL_native_register(H5I_type_t type, const void *obj, hbool_t app_ref);
H5_DLL herr_t H5VL_native_unregister(hid_t obj_id);

#ifdef __cplusplus
}
#endif

#endif /* _H5VLnative_private_H */
