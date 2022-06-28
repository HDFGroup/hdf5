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
 * Purpose:    This file contains declarations which define macros for the
 *        H5 package.  Including this header means that the source file
 *        is part of the H5 package.
 */
#ifndef H5module_H
#define H5module_H

/* Define the proper control macros for the generic FUNC_ENTER/LEAVE and error
 *      reporting macros.
 */
#define H5_MODULE
#define H5_MY_PKG      H5
#define H5_MY_PKG_ERR  H5E_LIB
#define H5_MY_PKG_INIT NO

/** \page H5DM_UG The HDF5 Data Model and File Structure
 *
 * \section sec_data_model The HDF5 Data Model and File Structure
 * \subsection subsec_data_model_intro Introduction
 * \subsection subsec_data_model_abstract The Abstract Data Model
 * \subsubsection subsubsec_data_model_abstract_file File
 * \subsubsection subsubsec_data_model_abstract_group Group
 * \subsubsection subsubsec_data_model_abstract_dataset Dataset
 * \subsubsection subsubsec_data_model_abstract_space Dataspace
 * \subsubsection subsubsec_data_model_abstract_type Datatype
 * \subsubsection subsubsec_data_model_abstract_attr Attribute
 * \subsubsection subsubsec_data_model_abstract_plist Property List
 * \subsubsection subsubsec_data_model_abstract_link Link
 * \subsection subsec_data_model_storage The HDF5 Storage Model
 * \subsubsection subsubsec_data_model_storage_spec The Abstract Storage Model: the HDF5 Format Specification
 * \subsubsection subsubsec_data_model_storage_imple Concrete Storage Model
 * \subsection subsec_data_model_structure The Structure of an HDF5 File
 * \subsubsection subsubsec_data_model_structure_file Overall File Structure
 * \subsubsection subsubsec_data_model_structure_path HDF5 Path Names and Navigation
 * \subsubsection subsubsec_data_model_structure_example Examples of HDF5 File Structures
 *
 */

/** \page H5_UG The HDF5 Library and Programming Model
 *
 * \section sec_program The HDF5 Library and Programming Model
 * \subsection subsec_program_intro Introduction
 * \subsection subsec_program_model The HDF5 Programming Model
 * \subsubsection subsubsec_program_model_create Creating an HDF5 File
 * \subsubsection subsubsec_program_model_dset Creating and Initializing a Dataset
 * \subsubsection subsubsec_program_model_close Closing an Object
 * \subsubsection subsubsec_program_model_data Writing or Reading a Dataset to or from a File
 * \subsubsection subsubsec_program_model_partial Reading and Writing a Portion of a Dataset
 * \subsubsection subsubsec_program_model_info Getting Information about a Dataset
 * \subsubsection subsubsec_program_model_compound Creating and Defining Compound Datatypes
 * \subsubsection subsubsec_program_model_extend Creating and Writing Extendable Datasets
 * \subsubsection subsubsec_program_model_group Creating and Working with Groups
 * \subsubsection subsubsec_program_model_attr Working with Attributes
 * \subsection subsec_program_transfer_pipeline The Data Transfer Pipeline
 *
 */

/**
 * \defgroup H5 H5
 *
 * Use the functions in this module to manage the life cycle of HDF5 library
 * instances.
 *
 * <table>
 * <tr><th>Create</th><th>Read</th></tr>
 * <tr valign="top">
 *   <td>
 *   \snippet{lineno} H5_examples.c create
 *   </td>
 *   <td>
 *   \snippet{lineno} H5_examples.c read
 *   </td>
 * <tr><th>Update</th><th>Delete</th></tr>
 * <tr valign="top">
 *   <td>
 *   \snippet{lineno} H5_examples.c update
 *   </td>
 *   <td>
 *   \snippet{lineno} H5_examples.c closing_shop
 *   \snippet{lineno} H5_examples.c delete
 *   </td>
 * </tr>
 * </table>
 *
 */

#endif /* H5module_H */
