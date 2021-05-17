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
 * Programmer:  Quincey Koziol
 *              Saturday, September 12, 2015
 *
 * Purpose:     This file contains declarations which define macros for the
 *              H5I package.  Including this header means that the source file
 *              is part of the H5I package.
 */
#ifndef H5Imodule_H
#define H5Imodule_H

/* Define the proper control macros for the generic FUNC_ENTER/LEAVE and error
 * reporting macros.
 */
#define H5I_MODULE
#define H5_MY_PKG      H5I
#define H5_MY_PKG_ERR  H5E_ID
#define H5_MY_PKG_INIT NO

/**\defgroup H5I H5I
 *
 * Use the functions in this module to manage HDF5 identifiers and identifier
 * types.
 *
 * HDF5 identifiers are usually created as a "side-effect" of creating HDF5
 * entities such as groups, datasets, attributes, or property lists.
 * Developers may find it convenient to define their own identifier types
 * for application objects of interest.
 *
 * Notice that identifiers (of type \ref hid_t) are run-time auxiliaries and
 * not persisted in the file.
 *
 * <table>
 * <tr><th>Create</th><th>Read</th></tr>
 * <tr valign="top">
 *   <td>
 *   \snippet H5I_examples.c create
 *   </td>
 *   <td>
 *   \snippet H5I_examples.c read
 *   </td>
 * <tr><th>Update</th><th>Delete</th></tr>
 * <tr valign="top">
 *   <td>
 *   \snippet H5I_examples.c update
 *   </td>
 *   <td>
 *   \snippet H5I_examples.c delete
 *   </td>
 * </tr>
 * </table>
 *
 */

#endif /* H5Imodule_H */
