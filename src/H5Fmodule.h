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
 * Programmer:	Quincey Koziol
 *		Saturday, September 12, 2015
 *
 * Purpose:	This file contains declarations which define macros for the
 *		H5F package.  Including this header means that the source file
 *		is part of the H5F package.
 */
#ifndef H5Fmodule_H
#define H5Fmodule_H

/* Define the proper control macros for the generic FUNC_ENTER/LEAVE and error
 *      reporting macros.
 */
#define H5F_MODULE
#define H5_MY_PKG      H5F
#define H5_MY_PKG_ERR  H5E_FILE
#define H5_MY_PKG_INIT YES

/**
 * \defgroup H5F H5F
 * \brief File Interface
 * \todo Describe concisely what the functions in this module are about.
 *
 * \defgroup MDC Metadata Cache
 * \ingroup H5F
 * \defgroup PH5F Parallel
 * \ingroup H5F
 * \defgroup SWMR Single Writer Multiple Readers
 * \ingroup H5F
 */

#endif /* H5Fmodule_H */
