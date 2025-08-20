/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the LICENSE file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
#ifndef H5FORMAT_CONVERT_H
#define H5FORMAT_CONVERT_H

/** \page H5TOOL_FC_UG The HDF5 h5format_convert Tool
 *
 * Navigate back: \ref index "Main" / \ref UG / \ref CommandTools
 * <hr>
 *
 * \section sec_cltools_h5format_convert h5format_convert
 *
 * \subsection subsec_cltools_h5format_convert_intro Introduction
 *  With h5format_convert, you can convert a datasets format in an HDF5 file.
 *
 * \subsection subsec_cltools_h5format_convert_usage Usage
 *  <h4>h5format_convert [OPTIONS] file_name</h4>
 *
 * \subsection subsec_cltools_h5format_convert_error Error Report Option
 * \li <strong>--enable-error-stack</strong> Prints messages from the HDF5 error stack as they occur.
 *             Optional value 2 also prints file open errors, --enable-error-stack=2.
 *
 * \subsection subsec_cltools_h5format_convert_options Options
 * \li <strong>--help</strong>                Print a usage message and exit
 * \li <strong>--version</strong>             Print the library version number and exit
 * \li <strong>--verbose</strong>             Turn on verbose mode
 * \li <strong>--dname=dataset_name</strong>  Pathname for the dataset
 * \li <strong>--noop</strong>                Perform all the steps except the actual conversion
 *
 * \subsubsection subsubsec_cltools_h5format_convert_examples Usage Examples
 * \li 1) h5format_convert --dname=/group/dataset file_name
 *
 *      Convert the dataset </group/dataset> in the HDF5 file <file_name>:
 *      - chunked dataset: convert the chunk indexing type to version 1 B-tree
 *      - compact/contiguous dataset: downgrade the layout version to 3
 *      - virtual dataset: no action
 *
 * \li 2) h5format_convert file_name
 *
 *      Convert all datasets in the HDF5 file <file_name>:
 *      - chunked dataset: convert the chunk indexing type to version 1 B-tree
 *      - compact/contiguous dataset: downgrade the layout version to 3
 *      - virtual dataset: no action
 *
 * \li 3) h5format_convert --noop --dname=/group/dataset file_name
 *
 *      Go through all the steps except the actual conversion when
 *      converting the dataset </group/dataset> in the HDF5 file <file_name>.
 *
 * Previous Chapter \ref sec_cltools_h5dump - Next Chapter \ref sec_cltools_h5import
 *
 * <hr>
 * Navigate back: \ref index "Main" / \ref UG / \ref CommandTools
 *
 */

#endif /* H5FORMAT_CONVERT_H */
