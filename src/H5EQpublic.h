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
 * H5EQ module.
 */
#ifndef _H5EQpublic_H
#define _H5EQpublic_H

/* System headers needed by this file */

/* Public headers needed by this file */

/*****************/
/* Public Macros */
/*****************/

#ifdef __cplusplus
extern "C" {
#endif

/*******************/
/* Public Typedefs */
/*******************/

/* An asynchronous request object */
typedef void * H5_request_t;

/********************/
/* Public Variables */
/********************/

/* Asynchronous operation status */
typedef enum {
    H5AO_PENDING,       /* Operation has not yet completed */
    H5AO_SUCCEEDED,     /* Operation has completed, successfully */
    H5AO_FAILED,        /* Operation has completed, but failed */
    H5AO_CANCELLED      /* Operation has not completed and has been cancelled */
} H5_status_t;

#define H5_REQUEST_NULL NULL
#define H5_EVENT_QUEUE_NULL -1

/*********************/
/* Public Prototypes */
/*********************/

/* API wrappers */
H5_DLL hid_t  H5EQcreate(hid_t fapl_id);
H5_DLL herr_t H5EQinsert(hid_t event_q, H5_request_t req);
H5_DLL herr_t H5EQpop(hid_t event_q, H5_request_t *req);
H5_DLL herr_t H5EQwait(hid_t event_q, int *num_requests, H5_status_t **status);
H5_DLL herr_t H5EQtest(hid_t event_q, int *num_remaining);
H5_DLL herr_t H5EQclose(hid_t event_q);

/* Asynchronous test & wait operations */
H5_DLL herr_t H5AOcancel(H5_request_t req, H5_status_t *status);
H5_DLL herr_t H5AOtest(H5_request_t req, H5_status_t *status);
H5_DLL herr_t H5AOwait(H5_request_t req, H5_status_t *status);

#ifdef __cplusplus
}
#endif

#endif /* _H5EQpublic_H */
