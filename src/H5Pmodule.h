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
 *		H5P package.  Including this header means that the source file
 *		is part of the H5P package.
 */
#ifndef H5Pmodule_H
#define H5Pmodule_H

/* Define the proper control macros for the generic FUNC_ENTER/LEAVE and error
 *      reporting macros.
 */
#define H5P_MODULE
#define H5_MY_PKG      H5P
#define H5_MY_PKG_ERR  H5E_PLIST
#define H5_MY_PKG_INIT YES

/**\defgroup H5P H5P
 *
 * Use the functions in this module to manage HDF5 property lists and property
 * list classes.
 *
 * <table>
 * <tr><th>Create</th><th>Read</th></tr>
 * <tr valign="top">
 *   <td>
 *   \snippet H5P_examples.c create
 *   </td>
 *   <td>
 *   \snippet H5P_examples.c read
 *   </td>
 * <tr><th>Update</th><th>Delete</th></tr>
 * <tr valign="top">
 *   <td>
 *   \snippet H5P_examples.c update
 *   </td>
 *   <td>
 *   \snippet H5P_examples.c delete
 *   </td>
 * </tr>
 * </table>
 *
 * The HDF5 Property List Interface provides a mechanism to take advantage of
 * more powerful or unusual features in HDF5.
 *
 * HDF5 objects have properties or characteristics associated with them, and
 * there are default properties that handle the most common needs. These default
 * properties can be modified using the HDF5 Property List Interface. For
 * example, the data storage layout property of a dataset is contiguous by
 * default. For better performance, the layout can be modified to be chunked or
 * chunked and compressed.
 *
 * \defgroup GPLO General Property List Operations
 * \ingroup H5P
 * \defgroup GPLOA General Property List Operations (Advanced)
 * \ingroup H5P
 * \defgroup FCPL File Creation Properties
 * \ingroup H5P
 * \defgroup FAPL File Access Properties
 * \ingroup H5P
 * \defgroup GCPL Group Creation Properties
 * \ingroup H5P
 *
 * \defgroup ALCAPL Attribute and Link Creation Properties
 * \ingroup H5P
 * Currently, there are only two creation properties that you can use to control
 * the creation of HDF5 attributes and links. The first creation property, the
 * choice of a character encoding, applies to both attributes and links.
 * The second creation property applies to links only, and advises the library
 * to automatically create missing intermediate groups when creating new objects.
 *
 * \defgroup LAPL Link Access Properties
 * \ingroup H5P
 *
 * \defgroup DCPL Dataset Creation Properties
 * \ingroup H5P
 * Use dataset creation properties to control aspects of dataset creation such
 * as fill time, storage layout, compression methods, etc.
 * Unlike dataset access and transfer properties, creation properties \Emph{are}
 * stored with the dataset, and cannot be changed once a dataset has been
 * created.
 *
 * \defgroup DAPL Dataset Access Properties
 * \ingroup H5P
 * Use dataset access properties to modify the behavior of the HDF5
 * library when accessing datasets. The properties include adjusting the size
 * of the chunk cache, providing prefixes for external content and virtual
 * dataset file paths, and controlling flush behavior, etc. These properties
 * are \Emph{not} persisted with datasets, and can be adjusted at runtime.
 *
 * \defgroup DXPL Dataset Transfer Properties
 * \ingroup H5P
 * Use dataset transfer properties to customize certain aspects of reading
 * and writing datasets such as transformations, MPI-IO I/O mode, error
 * detection, etc. These properties are \Emph{not} persisted with datasets,
 * and can be adjusted at runtime.
 *
 * \defgroup OCPL Object Creation Properties
 * \ingroup H5P
 * \defgroup OCPPL Object Copy Properties
 * \ingroup H5P
 * \defgroup GACPL General Access Properties
 * \ingroup H5P
 * \defgroup MAPL Map Access Properties
 * \ingroup H5P
 */

#endif /* H5Pmodule_H */
