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
 * This file contains public declarations for the H5TS (threadsafety) module.
 */

#ifndef H5TSpublic_H
#define H5TSpublic_H

/* Public headers needed by this file */
#include "H5public.h" /* Generic Functions                    */

/*****************/
/* Public Macros */
/*****************/

/*******************/
/* Public Typedefs */
/*******************/

/********************/
/* Public Variables */
/********************/

/*********************/
/* Public Prototypes */
/*********************/

#ifdef __cplusplus
extern "C" {
#endif

/* HDF5 global library lock routines */
H5_DLL herr_t H5TSmutex_acquire(unsigned int lock_count, hbool_t *acquired);
H5_DLL herr_t H5TSmutex_release(unsigned int *lock_count);
H5_DLL herr_t H5TSmutex_get_attempt_count(unsigned int *count);

#ifdef __cplusplus
}
#endif

#endif /* H5TSpublic_H */
