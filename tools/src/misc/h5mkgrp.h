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
#ifndef H5MKGRP_H
#define H5MKGRP_H

/** \page H5TOOL_MG_UG The HDF5 h5mkgrp Tool
 *
 * \section sec_cltools_h5mkgrp h5mkgrp
 *
 * \subsection subsec_cltools_h5mkgrp_intro Introduction
 *  With h5mkgrp, you can create group(s) in an HDF5 file
 *
 * \subsection subsec_cltools_h5mkgrp_usage Usage
 *  <h4>h5mkgrp [OPTIONS] FILE GROUP</h4>
 *
 * \subsection subsec_cltools_h5mkgrp_error Error Report Option
 * \li<strong>--enable-error-stack</strong> Prints messages from the HDF5 error stack as they occur.
 *            Optional value 2 also prints file open errors, --enable-error-stack=2.
 *
 * \subsection subsec_cltools_h5mkgrp_options Options
 * \li <strong>--help</strong>      Print a usage message and exit
 * \li <strong>--version</strong>   Print the library version number and exit
 * \li <strong>--verbose</strong>   Print information about OBJECTS and OPTIONS
 * \li <strong>--latest</strong>    Use latest version of file format to create groups
 * \li <strong>--parents</strong>   No error if existing, make parent groups as needed
 * \li <strong>--vol-value</strong> Value (ID) of the VOL connector to use for opening the
 *                   HDF5 file specified
 * \li <strong>--vol-name</strong>  Name of the VOL connector to use for opening the
 *                   HDF5 file specified
 * \li <strong>--vol-info</strong>  VOL-specific info to pass to the VOL connector used for
 *                   opening the HDF5 file specified.<br />
 *                   If none of the above options are used to specify a VOL, then
 *                   the VOL named by HDF5_VOL_CONNECTOR (or the native VOL connector,
 *                   if that environment variable is unset) will be used
 * \li <strong>--vfd-value</strong> Value (ID) of the VFL driver to use for opening the
 *                   HDF5 file specified
 * \li <strong>--vfd-name</strong>  Name of the VFL driver to use for opening the
 *                   HDF5 file specified
 * \li <strong>--vfd-info</strong>  VFD-specific info to pass to the VFL driver used for
 *                   opening the HDF5 file specified
 *
 */

#endif /* H5MKGRP_H */
