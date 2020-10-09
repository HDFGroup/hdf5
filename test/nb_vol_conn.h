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
 * Purpose:	The public header file for the non-blocking VOL connector.
 */

#ifndef _nb_vol_conn_H
#define _nb_vol_conn_H

/* Public headers needed by this file */
#include "H5VLpublic.h" /* Virtual Object Layer                 */

/* Identifier for the non-blocking VOL connector */
#define H5VL_NONBLOCK (H5VL_nonblock_register())

/* Characteristics of the non-blocking VOL connector */
#define H5VL_NONBLOCK_NAME    "non_block"
#define H5VL_NONBLOCK_VALUE   506 /* VOL connector ID */
#define H5VL_NONBLOCK_VERSION 0

/* Non-blocking VOL connector info */
typedef struct H5VL_nonblock_info_t {
    hid_t under_vol_id;   /* VOL ID for under VOL */
    void *under_vol_info; /* VOL info for under VOL */
} H5VL_nonblock_info_t;

#ifdef __cplusplus
extern "C" {
#endif

H5_DLL hid_t H5VL_nonblock_register(void);

#ifdef __cplusplus
}
#endif

#endif /* _nb_vol_conn_H */
