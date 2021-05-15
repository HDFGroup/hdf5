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
 *		H5O package.  Including this header means that the source file
 *		is part of the H5O package.
 */
#ifndef H5Omodule_H
#define H5Omodule_H

/* Define the proper control macros for the generic FUNC_ENTER/LEAVE and error
 *      reporting macros.
 */
#define H5O_MODULE
#define H5_MY_PKG      H5O
#define H5_MY_PKG_ERR  H5E_OHDR
#define H5_MY_PKG_INIT YES

/**\defgroup H5O H5O
 *
 * Use the functions in this module to manage HDF5 objects.
 *
 * <table>
 * <tr><th>Create</th><th>Read</th></tr>
 * <tr valign="top">
 *   <td>
 *   \snippet H5O_examples.c create
 *   </td>
 *   <td>
 *   \snippet H5O_examples.c read
 *   </td>
 * <tr><th>Update</th><th>Delete</th></tr>
 * <tr valign="top">
 *   <td>
 *   \snippet H5O_examples.c update
 *   </td>
 *   <td>
 *   \snippet H5O_examples.c delete
 *   </td>
 * </tr>
 * </table>
 *
 */
#endif /* H5Omodule_H */
