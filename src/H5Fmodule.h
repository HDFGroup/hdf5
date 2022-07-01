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

/** \page H5F_UG  The HDF5 File
 *
 * \section sec_file The HDF5 File
 * \subsection subsec_file_intro Introduction
 * \subsection subsec_file_access_modes File Access Modes
 * \subsection subsec_file_creation_access File Creation and File Access Properties
 * \subsection subsec_file_drivers Low-level File Drivers
 * \subsection subsec_file_program_model Programming Model for Files
 * \subsubsection subsubsec_file_program_model_create Creating a New File
 * \subsubsection subsubsec_file_program_model_open Opening an Existing File
 * \subsubsection subsubsec_file_program_model_close Closing a File
 * \subsection subsec_file_h5dump Using h5dump to View a File
 * \subsection subsec_file_summary File Function Summaries
 * \subsection subsec_file_create Creating or Opening an HDF5 File
 * \subsection subsec_file_closes Closing an HDF5 File
 * \subsection subsec_file_property_lists File Property Lists
 * \subsubsection subsubsec_file_property_lists_create Creating a Property List
 * \subsubsection subsubsec_file_property_lists_props File Creation Properties
 * \subsubsection subsubsec_file_property_lists_access File Access Properties
 * \subsection subsec_file_alternate_drivers Alternate File Storage Layouts and Low-level File Drivers
 * \subsubsection subsubsec_file_alternate_drivers_id Identifying the Previously‐used File Driver
 * \subsubsection subsubsec_file_alternate_drivers_sec2 The POSIX (aka SEC2) Driver
 * \subsubsection subsubsec_file_alternate_drivers_direct The Direct Driver
 * \subsubsection subsubsec_file_alternate_drivers_log The Log Driver
 * \subsubsection subsubsec_file_alternate_drivers_win The Windows Driver
 * \subsubsection subsubsec_file_alternate_drivers_stdio The STDIO Driver
 * \subsubsection subsubsec_file_alternate_drivers_mem The Memory (aka Core) Driver
 * \subsubsection subsubsec_file_alternate_drivers_family The Family Driver
 * \subsubsection subsubsec_file_alternate_drivers_multi The Multi Driver
 * \subsubsection subsubsec_file_alternate_drivers_split The Split Driver
 * \subsubsection subsubsec_file_alternate_drivers_par The Parallel Driver
 * \subsection subsec_file_examples Code Examples for Opening and Closing Files
 * \subsubsection subsubsec_file_examples_trunc Example Using the H5F_ACC_TRUNC Flag
 * \subsubsection subsubsec_file_examples_props Example with the File Creation Property List
 * \subsubsection subsubsec_file_examples_access Example with the File Access Property List
 * \subsection subsec_file_multiple Working with Multiple HDF5 Files
 *
 */

/**
 * \defgroup H5F H5F
 *
 * Use the functions in this module to manage HDF5 files.
 *
 * In the code snippets below, we show the skeletal life cycle of an HDF5 file,
 * when creating a new file (left) or when opening an existing file (right).
 * File creation is essentially controlled through \ref FCPL, and file access to
 * new and existing files is controlled through \ref FAPL. The file \c name and
 * creation or access \c mode control the interaction with the underlying
 * storage such as file systems.
 *
 * <table>
 * <tr><th>Create</th><th>Read</th></tr>
 * <tr valign="top">
 *   <td>
 *   \snippet{lineno} H5F_examples.c create
 *   </td>
 *   <td>
 *   \snippet{lineno} H5F_examples.c read
 *   </td>
 * </tr>
 * <tr><th>Update</th><th>Delete</th></tr>
 * <tr valign="top">
 *   <td>
 *   \snippet{lineno} H5F_examples.c update
 *   </td>
 *   <td>
 *   \snippet{lineno} H5F_examples.c delete
 *   </td>
 * </tr>
 * </table>
 *
 * In addition to general file management functions, there are three categories
 * of functions that deal with advanced file management tasks and use cases:
 * 1. The control of the HDF5 \ref MDC
 * 2. The use of (MPI-) \ref PH5F HDF5
 * 3. The \ref SWMR pattern
 *
 * \defgroup MDC Metadata Cache
 * \ingroup H5F
 * \defgroup PH5F Parallel
 * \ingroup H5F
 * \defgroup SWMR Single Writer Multiple Readers
 * \ingroup H5F
 */

#endif /* H5Fmodule_H */
