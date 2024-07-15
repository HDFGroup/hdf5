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
#ifndef H5WATCH_H
#define H5WATCH_H

/** \page H5TOOL_WH_UG The HDF5 h5watch Tool
 *
 * \section sec_cltools_h5watch h5watch
 *
 * \subsection subsec_cltools_h5watch_intro Introduction
 *  With h5watch, you can dump stats from an HDF5 file.
 *
 * \subsection subsec_cltools_h5watch_usage Usage
 *  <h4>h5watch [OPTIONS] [OBJECT]</h4>
 *
 * \subsection subsec_cltools_h5watch_error Error Report Option
 * \li <strong>--enable-error-stack</strong> Prints messages from the HDF5 error stack as they occur.
 *             Optional value 2 also prints file open errors, --enable-error-stack=2.
 *
 * \subsection subsec_cltools_h5watch_options Options
 * \li <strong>--help</strong>    Print a usage message and exit
 * \li <strong>--version</strong> Print the library version number and exit
 * \li <strong>--label</strong>   Label members of compound typed dataset.
 * \li <strong>--simple</strong>  Use a machine-readable output format.
 * \li <strong>--dim</strong>     Monitor changes in size of dataset dimensions only.
 * \li <strong>--width=N</strong> Set the number of columns to N for output.<br />
 *                                A value of 0 sets the number of columns to the
 *                                maximum (65535). The default width is 80 columns.
 * \li <strong>--polling=N</strong> Set the polling interval to N (in seconds) when the
 *                                dataset will be checked for appended data.
 *                                The default polling interval is 1.
 * \li <strong>--fields=\<list_of_fields\></strong>
 *     Display data for the fields specified in \<list_of_fields\>
 *     for a compound data type.
 *     \<list_of_fields\> can be specified as follows:
 *     <ul><li>1) A comma-separated list of field names in a
 *     compound data type. "," is the separator for field names while "." is the separator
 *     for a nested field.</li>
 *     <li>2) A single field name in a compound data type.
 *     This option can be used multiple times.</li></ul>
 *     Note that backslash is the escape character to avoid
 *     characters in field names that conflict with the tool's separators.
 *
 * \subsection subsec_cltools_h5watch_objs Object
 *  <strong>OBJECT</strong> is specified as [\<filename\>/\<path_to_dataset\>/\<dsetname\>]
 * \li <strong>\<filename\></strong>  Name of the HDF5 file. It may be preceded by path
 *                                    separated by slashes to the specified HDF5 file.
 * \li <strong>\<path_to_dataset\></strong> Path separated by slashes to the specified dataset
 * \li <strong>\<dsetname\></strong>  Name of the dataset
 *
 */

#endif /* H5WATCH_H */
