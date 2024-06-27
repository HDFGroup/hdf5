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

#ifndef H5COPY_H
#define H5COPY_H

/** \page H5TOOL_CP_UG The HDF5 h5copy Tool
 *
 * \section sec_cltools_h5copy h5copy
 *
 * \subsection subsec_cltools_h5copy_intro Introduction
 *  With h5copy, you can copy objects from an HDF5 file to another file.
 *
 * \subsection subsec_cltools_h5copy_usage Usage
 *  <h4>h5copy [OPTIONS] [OBJECTS...]</h4>
 *
 * \subsection subsec_cltools_h5copy_objs Objects
 * \li <strong>--input</strong>        input file name
 * \li <strong>--output</strong>       output file name
 * \li <strong>--source</strong>       source object name
 * \li <strong>--destination</strong>  destination object name
 *
 * \subsection subsec_cltools_h5copy_error Error Report Option
 * \li <strong>--enable-error-stack</strong> Prints messages from the HDF5 error stack as they occur.
                          Optional value 2 also prints file open errors, --enable-error-stack=2.
 *
 * \subsection subsec_cltools_h5copy_options Options
 * \li <strong>--help</strong>    Print a usage message and exit
 * \li <strong>--parents</strong> No error if existing, make parent groups as needed
 * \li <strong>--verbose</strong> Print information about OBJECTS and OPTIONS
 * \li <strong>--version</strong> Print the library version number and exit
 * \li <strong>--flag</strong>    Flag type
 *
 * \subsubsection subsubsec_cltools_h5copy_options_args Flag Type Options
 *  Flag type is one of the following strings:
 * \li <strong>shallow</strong> Copy only immediate members for groups
 * \li <strong>soft</strong>    Expand soft links into new objects
 * \li <strong>ext</strong>     Expand external links into new objects
 * \li <strong>ref</strong>     Copy references and any referenced objects, i.e., objects
 *                  that the references point to.<br />
 *                  Referenced objects are copied in addition to the objects
 *                  specified on the command line and reference datasets are
 *                  populated with correct reference values. Copies of referenced
 *                  datasets outside the copy range specified on the command line
 *                  will normally have a different name from the original.<br />
 *                  (Default: Without this option, reference value(s) in any
 *                  reference datasets are set to NULL and referenced objects are
 *                  not copied unless they are otherwise within the copy range
 *                  specified on the command line.)
 * \li <strong>noattr</strong>   Copy object without copying attributes
 * \li <strong>allflags</strong> Switches all flags from the default to the non-default setting
 *
 * These flag types correspond to the following API symbols
 * \li <strong>#H5O_COPY_SHALLOW_HIERARCHY_FLAG</strong>
 * \li <strong>#H5O_COPY_EXPAND_SOFT_LINK_FLAG</strong>
 * \li <strong>#H5O_COPY_EXPAND_EXT_LINK_FLAG</strong>
 * \li <strong>#H5O_COPY_EXPAND_REFERENCE_FLAG</strong>
 * \li <strong>#H5O_COPY_WITHOUT_ATTR_FLAG</strong>
 * \li <strong>#H5O_COPY_ALL</strong>
 *
 */

#endif /* H5COPY_H */
