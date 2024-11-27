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
 * Purpose:     This file contains declarations which are visible only within
 *              the H5SC package.  Source files outside the H5SC package should
 *              include H5SCprivate.h instead.
 */
#if !(defined H5SC_FRIEND || defined H5SC_MODULE)
#error "Do not include this file outside the H5SC package!"
#endif

#ifndef H5SCpkg_H
#define H5SCpkg_H

/* Get package's private header */
#include "H5SCprivate.h"

/* Other private headers needed by this file */

/**************************/
/* Package Private Macros */
/**************************/

/****************************/
/* Package Private Typedefs */
/****************************/

/* Main struct for a shared chunk cache */
struct H5SC_t {
    int placeholder;
};

/*****************************/
/* Package Private Variables */
/*****************************/

/******************************/
/* Package Private Prototypes */
/******************************/

/* Generic routines */

#endif /* H5SCpkg_H */
