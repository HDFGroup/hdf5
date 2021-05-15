/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5. The full HDF5 copyright notice, including      *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * This file contains public declarations for the H5ES (event set) module.
 */

#ifndef H5ESpublic_H
#define H5ESpublic_H

/* Public headers needed by this file */
#include "H5public.h" /* Generic Functions                    */

/*****************/
/* Public Macros */
/*****************/

/* Default value for "no event set" / synchronous execution */
#define H5ES_NONE 0 /* (hid_t) */

/* Special "wait" timeout values */
#define H5ES_WAIT_FOREVER (UINT64_MAX) /* Wait until all operations complete */
#define H5ES_WAIT_NONE                                                                                       \
    (0) /* Don't wait for operations to complete,                                                            \
         *  just check their status.                                                                         \
         *  (this allows H5ESwait to behave                                                                  \
         *   like a 'test' operation)                                                                        \
         */

/*******************/
/* Public Typedefs */
/*******************/

/* Asynchronous operation status */
typedef enum H5ES_status_t {
    H5ES_STATUS_IN_PROGRESS, /* Operation has not yet completed                       */
    H5ES_STATUS_SUCCEED,     /* Operation has completed, successfully                 */
    H5ES_STATUS_FAIL,        /* Operation has completed, but failed                   */
    H5ES_STATUS_CANCELED     /* Operation has not completed and was canceled          */
} H5ES_status_t;

/********************/
/* Public Variables */
/********************/

/*********************/
/* Public Prototypes */
/*********************/

#ifdef __cplusplus
extern "C" {
#endif

#ifdef __cplusplus
}
#endif

#endif /* H5ESpublic_H */
