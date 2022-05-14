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
 *		H5S package.  Including this header means that the source file
 *		is part of the H5S package.
 */
#ifndef H5Smodule_H
#define H5Smodule_H

/* Define the proper control macros for the generic FUNC_ENTER/LEAVE and error
 *      reporting macros.
 */
#define H5S_MODULE
#define H5_MY_PKG      H5S
#define H5_MY_PKG_ERR  H5E_DATASPACE
#define H5_MY_PKG_INIT YES

/** \page H5S_UG Dataspaces and Partial I/O
 *
 *
 * \section sec_dataspace HDF5 Dataspaces and Partial I/O
 * \subsection subsec_dataspace_intro Introduction
 * \subsection subsec_dataspace_function Dataspace Function Summaries
 * \subsection subsec_dataspace_program  Definition of Dataspace Objects and the Dataspace Programming Model
 * \subsubsection subsubsec_dataspace_program_object Dataspace Objects
 * \subsubsection subsubsec_dataspace_program_model Dataspace Programming Model
 * \subsection subsec_dataspace_transfer Dataspaces and Data Transfer
 * \subsubsection subsubsec_dataspace_transfer_select Data Selection
 * \subsubsection subsubsec_dataspace_transfer_model Programming Model
 * \subsection subsec_dataspace_select Dataspace Selection Operations and Data Transfer
 * \subsection subsec_dataspace_refer References to Dataset Regions
 * \subsubsection subsubsec_dataspace_refer_use Example Uses for Region References
 * \subsubsection subsubsec_dataspace_refer_create Creating References to Regions
 * \subsubsection subsubsec_dataspace_refer_read Reading References to Regions
 * \subsection subsec_dataspace_sample Sample Programs
 *
 */

/**\defgroup H5S H5S Dataspaces
 *
 * Use the functions in this module to manage HDF5 dataspaces \Emph{and} selections.
 *
 * HDF5 dataspaces describe the \Emph{shape} of datasets in memory or in HDF5
 * files. Dataspaces can be empty (#H5S_NULL), a singleton (#H5S_SCALAR), or
 * a multi-dimensional, regular grid (#H5S_SIMPLE). Dataspaces can be re-shaped.
 *
 * Subsets of dataspaces can be "book-marked" or used to restrict I/O operations
 * using \Emph{selections}. Furthermore, certain set operations are supported
 * for selections.
 *
 * <table>
 * <tr><th>Create</th><th>Read</th></tr>
 * <tr valign="top">
 *   <td>
 *   \snippet{lineno} H5S_examples.c create
 *   </td>
 *   <td>
 *   \snippet{lineno} H5S_examples.c read
 *   </td>
 * <tr><th>Update</th><th>Delete</th></tr>
 * <tr valign="top">
 *   <td>
 *   \snippet{lineno} H5S_examples.c update
 *   </td>
 *   <td>
 *   \snippet{lineno} H5S_examples.c delete
 *   </td>
 * </tr>
 * </table>
 *
 */

#endif /* H5Smodule_H */
