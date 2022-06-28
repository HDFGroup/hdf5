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

/** \page H5D_UG HDF5 Datasets
 *
 * \section sec_dataset HDF5 Datasets
 * \subsection subsec_dataset_intro Introduction
 * \subsection subsec_dataset_function Dataset Function Summaries
 * \subsection subsec_dataset_program Programming Model for Datasets
 * \subsubsection subsubsec_dataset_program_general General Model
 * \subsubsection subsubsec_dataset_program_create Create Dataset
 * \subsubsection subsubsec_dataset_program_transfer Data Transfer Operations on a Dataset
 * \subsubsection subsubsec_dataset_program_read Retrieve the Properties of a Dataset
 * \subsection subsec_dataset_transfer Data Transfer
 * \subsubsection subsubsec_dataset_transfer_pipe The Data Pipeline
 * \subsubsection subsubsec_dataset_transfer_filter Data Pipeline Filters
 * \subsubsection subsubsec_dataset_transfer_drive File Drivers
 * \subsubsection subsubsec_dataset_transfer_props Data Transfer Properties to Manage the Pipeline
 * \subsubsection subsubsec_dataset_transfer_store Storage Strategies
 * \subsubsection subsubsec_dataset_transfer_partial Partial I/O Sub‐setting and Hyperslabs
 * \subsection subsec_dataset_allocation Allocation of Space in the File
 * \subsubsection subsubsec_dataset_allocation_store Storage Allocation in the File: Early, Incremental, Late
 * \subsubsection subsubsec_dataset_allocation_delete Deleting a Dataset from a File and Reclaiming Space
 * \subsubsection subsubsec_dataset_allocation_release Releasing Memory Resources
 * \subsubsection subsubsec_dataset_allocation_ext External Storage Properties
 * \subsection subsec_dataset_filters Using HDF5 Filters
 * \subsubsection subsubsec_dataset_filters_nbit N‐bit Filter
 * \subsubsection subsubsec_dataset_filters_scale Scale‐offset Filter
 * \subsubsection subsubsec_dataset_filters_szip Szip Filter
 */

/**
 * \defgroup H5D H5D Datasets
 *
 * Use the functions in this module to manage HDF5 datasets, including the
 * transfer of data between memory and disk and the description of dataset
 * properties. Datasets are used by other HDF5 APIs and referenced either by
 * name or by a handle. Such handles can be obtained by either creating or
 * opening the dataset.
 *
 * Typical stages in the HDF5 dataset life cycle are shown below in introductory
 * examples.
 *
 * <table>
 * <tr><th>Create</th><th>Read</th></tr>
 * <tr valign="top">
 *   <td>
 *   \snippet{lineno} H5D_examples.c create
 *   </td>
 *   <td>
 *   \snippet{lineno} H5D_examples.c read
 *   </td>
 * <tr><th>Update</th><th>Delete</th></tr>
 * <tr valign="top">
 *   <td>
 *   \snippet{lineno} H5D_examples.c update
 *   </td>
 *   <td>
 *   \snippet{lineno} H5D_examples.c delete
 *   </td>
 * </tr>
 * </table>
 *
 */

#endif /* H5Dmodule_H */
