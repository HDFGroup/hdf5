/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
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
 * Programmer:	Mohamad Chaarawi <chaarawi@hdfgroup.org>
 *		January, 2012
 *
 * Purpose:	This file contains declarations which are visible only within
 *		the H5VL package.  Source files outside the H5VL package should
 *		include H5VLprivate.h instead.
 */
#ifndef H5VL_PACKAGE
#error "Do not include this file outside the H5VL package!"
#endif

#ifndef _H5VLpkg_H
#define _H5VLpkg_H

/* Get package's private header */
#include "H5VLprivate.h"	/* File drivers				*/

/* Other private headers needed by this file */
#include "H5FLprivate.h"	/* Free lists                           */

/**************************/
/* Package Private Macros */
/**************************/


/****************************/
/* Package Private Typedefs */
/****************************/


/*****************************/
/* Package Private Variables */
/*****************************/


/******************************/
/* Package Private Prototypes */
/******************************/
H5_DLL herr_t H5VL_init(void);

/*
H5_DLL haddr_t H5VL_alloc_real(H5VL_t *file, hid_t dxpl_id, H5VL_mem_t type,
    hsize_t size, haddr_t *align_addr, hsize_t *align_size);
H5_DLL herr_t H5VL_free_real(H5VL_t *file, hid_t dxpl_id, H5VL_mem_t type,
    haddr_t addr, hsize_t size);
*/

#endif /* _H5VLpkg_H */

