/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
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

/*
 * This file contains function prototypes for each exported function in the
 * H5RC module.
 */
#ifndef _H5RCpublic_H
#define _H5RCpublic_H

/* System headers needed by this file */

/* Public headers needed by this file */
#include "H5public.h"
#include "H5Ipublic.h"

/*****************/
/* Public Macros */
/*****************/
#define H5RC_ACQUIRE_CV_REQUEST_NAME "acquire_request_name"

/*******************/
/* Public Typedefs */
/*******************/
typedef enum H5RC_request_t{
    H5RC_EXACT, /* default */
    H5RC_NEXT,
    H5RC_LAST
} H5RC_request_t;

/********************/
/* Public Variables */
/********************/


#ifdef __cplusplus
extern "C" {
#endif

/*********************/
/* Public Prototypes */
/*********************/

/* API wrappers */
H5_DLL hid_t H5RCcreate(hid_t file_id, uint64_t container_version);
H5_DLL hid_t H5RCacquire(hid_t file_id, /*IN/OUT*/ uint64_t *container_version, 
                         hid_t rcapl_id, hid_t eq_id);
H5_DLL herr_t H5RCrelease(hid_t rc_id , hid_t eq_id);
H5_DLL herr_t H5RCclose(hid_t rc_id);

H5_DLL herr_t H5Pset_rcapl_version_request(hid_t rcapl_id, H5RC_request_t acquire_req);

#if 0
/* Those are not set for now */
H5_DLL herr_t H5Fskip_trans(hid_t file_id, uint64_t start_trans_num, uint64_t count, hid_t eq_id);
H5_DLL herr_t H5Fpersist(hid_t file_id, uint64_t rc_num, hid_t eq_id);
H5_DLL herr_t H5Fsnapshot(hid_t file_id, uint64_t container_version, 
                          const char* snapshot_name, hid_t eq_id);
#endif

#ifdef __cplusplus
}
#endif
#endif /* _H5RCpublic_H */
