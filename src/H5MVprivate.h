/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by Akadio, Inc.                                                 *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*-------------------------------------------------------------------------
 *
 * Created:             H5MVprivate.h
 *
 * Purpose:             Private header file for VFD SWMR free space management
 *
 *-------------------------------------------------------------------------
 */

#ifndef _H5MVprivate_H
#define _H5MVprivate_H

/* Private headers needed by this file */
#include "H5Fprivate.h" /* File access				*/

/**************************/
/* Library Private Macros */
/**************************/

/****************************/
/* Library Private Typedefs */
/****************************/

/*****************************/
/* Library-private Variables */
/*****************************/

/***************************************/
/* Library-private Function Prototypes */
/***************************************/

/* File space manager routines */

H5_DLL herr_t H5MV_close(H5F_t *f);

/* File space allocation routines */
H5_DLL haddr_t H5MV_alloc(H5F_t *f, hsize_t size);
H5_DLL herr_t  H5MV_free(H5F_t *f, haddr_t addr, hsize_t size);
H5_DLL herr_t  H5MV_try_extend(H5F_t *f, haddr_t addr, hsize_t size, hsize_t extra_requested);
H5_DLL htri_t  H5MV_try_shrink(H5F_t *f, haddr_t addr, hsize_t size);

#endif /* end _H5MVprivate_H */
