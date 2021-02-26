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
 *		H5D package.  Including this header means that the source file
 *		is part of the H5D package.
 */
#ifndef H5Dmodule_H
#define H5Dmodule_H

/* Define the proper control macros for the generic FUNC_ENTER/LEAVE and error
 *      reporting macros.
 */
#define H5D_MODULE
#define H5_MY_PKG      H5D
#define H5_MY_PKG_ERR  H5E_DATASET
#define H5_MY_PKG_INIT YES

/**
 * \defgroup H5D H5D
 * \brief Group Interface
 * \details The HDF5 Dataset Interface, H5D, provides a mechanism for managing
 *          HDF5 datasets, including the transfer of data between memory and
 *          disk and the description of dataset properties.
 *
 *          A Dataset is used by other HDF5 APIs, either by name or by a handle,
 *          which is obtained by either creating or opening the dataset.
 */

#endif /* H5Dmodule_H */
