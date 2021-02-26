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
 *		H5G package.  Including this header means that the source file
 *		is part of the H5G package.
 */
#ifndef H5Gmodule_H
#define H5Gmodule_H

/* Define the proper control macros for the generic FUNC_ENTER/LEAVE and error
 *      reporting macros.
 */
#define H5G_MODULE
#define H5_MY_PKG      H5G
#define H5_MY_PKG_ERR  H5E_SYM
#define H5_MY_PKG_INIT YES

/**
 * \defgroup H5G H5G
 * \brief Group Interface
 * \details The HDF5 Group Interface, H5G, provides a mechanism for managing
 *          HDF5 groups and their members, which are other HDF5 objects.
 */

#endif /* H5Gmodule_H */
