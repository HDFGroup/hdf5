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
 * Programmer:	Quincey Koziol <koziol@hdfgroup.org>
 *		Tuesday, April 14, 2009
 *
 * Purpose:	This file contains declarations which are visible only within
 *		the H5LR package.  Source files outside the H5LR package should
 *		include H5LRprivate.h instead.
 */
#if !(defined(H5LR_PACKAGE) | defined(H5LR_MODULE))
#error "Do not include this file outside the H5LR package!"
#endif

#ifndef _H5LRpkg_H
#define _H5LRpkg_H

/* If this package header is being included in one of the H5LR modules, define
 *      the proper control macros for the generic FUNC_ENTER/LEAVE and error
 *      reporting macros.
 */
#ifdef H5LR_MODULE
#define H5_MY_PKG       H5LR
#define H5_MY_PKG_ERR   H5E_HL
#define H5_MY_PKG_INIT  YES
#endif /* H5LR_MODULE */

/* Get package's private header */
#include "H5LRprivate.h"

/* Other private headers needed by this file */


/**************************/
/* Package Private Macros */
/**************************/

/* If this package header is being included in one of the H5LR modules, define
 *      the proper control macros for the generic FUNC_ENTER/LEAVE and error
 *      reporting macros.
 */

/****************************/
/* Package Private Typedefs */
/****************************/


/*****************************/
/* Package Private Variables */
/*****************************/


/******************************/
/* Package Private Prototypes */
/******************************/


#endif /* _H5LRpkg_H */

