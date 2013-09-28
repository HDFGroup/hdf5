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
 * H5ES module.
 */
#ifndef _H5ESpublic_H
#define _H5ESpublic_H

/* System headers needed by this file */

/* Public headers needed by this file */
#include "H5public.h"
#include "H5Ipublic.h"

/*****************/
/* Public Macros */
/*****************/

/*******************/
/* Public Typedefs */
/*******************/

/* Asynchronous operation status */
typedef enum H5ES_status_t {
    H5ES_STATUS_IN_PROGRESS,   /* Operation has not yet completed */
    H5ES_STATUS_SUCCEED,       /* Operation has completed, successfully */
    H5ES_STATUS_FAIL,          /* Operation has completed, but failed */
    H5ES_STATUS_CANCEL         /* Operation has not completed and has been cancelled */
} H5ES_status_t;

/********************/
/* Public Variables */
/********************/


#define H5_REQUEST_NULL NULL
#define H5_EVENT_STACK_NULL -1

#ifdef __cplusplus
extern "C" {
#endif

/*********************/
/* Public Prototypes */
/*********************/

H5_DLL hid_t H5EScreate(void);
H5_DLL herr_t H5ESget_count(hid_t es_id, size_t *count);
    //H5_DLL herr_t H5ESget_event_info(hid_t es_id,  size_t start_idx,  size_t count, 
    //const char *ev_trace_str_arr[], H5ES_status_t ev_status_arr[], 
    //H5E_stack_id ev_err_stack_id_arr[]);
H5_DLL herr_t H5EStest(hid_t es_id, size_t event_idx, H5ES_status_t *status);
H5_DLL herr_t H5EStest_all(hid_t es_id,  H5ES_status_t *status);
H5_DLL herr_t H5ESwait(hid_t es_id, size_t event_idx, H5ES_status_t *status);
H5_DLL herr_t H5ESwait_all(hid_t es_id,  H5ES_status_t *status);
H5_DLL herr_t H5EScancel(hid_t es_id, size_t event_idx, H5ES_status_t *status);
H5_DLL herr_t H5EScancel_all(hid_t es_id,  H5ES_status_t *status);
H5_DLL herr_t H5ESclear(hid_t es_id);
H5_DLL herr_t H5ESclose(hid_t es_id);

#ifdef __cplusplus
}
#endif
#endif /* _H5ESpublic_H */
