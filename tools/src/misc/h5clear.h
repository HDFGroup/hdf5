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
#ifndef H5CLEAR_H
#define H5CLEAR_H

/** \page H5TOOL_CR_UG The HDF5 h5clear Tool
 *
 * \section sec_cltools_h5clear h5clear
 *
 * \subsection subsec_cltools_h5clear_intro Introduction
 *  With h5clear, you can clear the superblock status flag field, remove the metadata cache image, print
 *  the EOA and EOF, or set the EOA of a file. It is not a general repair tool and should not
 *  be used to fix file corruption. If a process doesn't shut down cleanly, the
 *  superblock mark can be left that prevents opening a file without SWMR. Then,
 *  h5clear can be used to remove this superblock mark so that the file can be inspected
 *  and appropriate actions can be taken.
 *
 * \subsection subsec_cltools_h5clear_usage Usage
 *  <h4>h5clear [OPTIONS] file_name</h4>
 *
 * \subsection subsec_cltools_h5clear_error Error Report Option
 * \li <strong>--enable-error-stack</strong> Prints messages from the HDF5 error stack as they occur.
 *             Optional value 2 also prints file open errors, --enable-error-stack=2.
 *
 * \subsection subsec_cltools_h5clear_options Options
 * \li <strong>--help</strong>        Print a usage message and exit
 * \li <strong>--version</strong>     Print the library version number and exit
 * \li <strong>--status</strong>      Clear the status_flags field in the file's superblock
 * \li <strong>--image</strong>       Remove the metadata cache image from the file
 * \li <strong>--filesize </strong>   Print the file's EOA and EOF
 * \li <strong>--increment=C</strong> Set the file's EOA to the maximum of (EOA, EOF) + C for
 *                               the file \<file_name\>.
 *                               C is >= 0; C is optional and will default to 1M when not set.
 *                               This option helps to repair a crashed SWMR file when the stored
 *                               EOA in the superblock is different from the actual EOF.
 *                               The file's EOA and EOF will be the same after applying
 *                               this option to the file.
 *
 * \subsection subsec_cltools_h5clear_examples Usage Examples
 * \li 1) h5clear -s file_name
 *
 *      Clear the status_flags field in the superblock of the HDF5 file <file_name>.
 *
 * \li 2) h5clear -m file_name
 *
 *      Remove the metadata cache image from the HDF5 file <file_name>.
 *
 * \li 3) h5clear --increment file_name
 *
 *      Set the EOA to the maximum of (EOA, EOF) + 1M for the file <file_name>.
 *
 * \li 4) h5clear --increment=512 file_name
 *
 *      Set the EOA to the maximum of (EOA, EOF) + 512 for the file\<file_name>.
 *
 */

#endif /* H5CLEAR_H */
