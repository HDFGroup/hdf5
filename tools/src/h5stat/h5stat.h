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
#ifndef H5STAT_H
#define H5STAT_H

/** \page H5TOOL_ST_UG The HDF5 h5stat Tool
 *
 * \section sec_cltools_h5stat h5stat
 *
 * \subsection subsec_cltools_h5stat_intro Introduction
 *  With h5stat, you can dump stats from an HDF5 file.
 *
 * \subsection subsec_cltools_h5stat_usage Usage
 *  <h4>h5stat [OPTIONS] file</h4>
 *
 * \subsection subsec_cltools_h5stat_error Error Report Option
 * \li <strong>--enable-error-stack</strong> Prints messages from the HDF5 error stack as they occur.
 *             Optional value 2 also prints file open errors, --enable-error-stack=2.
 *
 * \subsection subsec_cltools_h5stat_options Options
 * \li <strong>--help</strong>    Print a usage message and exit
 * \li <strong>--version</strong> Print the library version number and exit
 * \li <strong>--file</strong>    Print file information
 * \li <strong>--filemetadata</strong> Print file space information for file's metadata
 * \li <strong>--group</strong>   Print group information
 * \li <strong>--links=N</strong> Set the threshold for the # of links when printing
 *                           information for small groups. N is an integer greater
 *                           than 0. The default threshold is 10.
 * \li <strong>--groupmetadata </strong>  Print file space information for groups' metadata
 * \li <strong>--dset</strong>   Print dataset information
 * \li <strong>--dims=N</strong> Set the threshold for the dimension sizes when printing
 *                           information for small datasets. N is an integer greater
 *                           than 0. The default threshold is 10.
 * \li <strong>--dsetmetadata</strong>  Print file space information for datasets' metadata
 * \li <strong>--dtypemetadata</strong> Print datasets' datatype information
 * \li <strong>--attribute</strong>  Print attribute information
 * \li <strong>--numattrs=N</strong> Set the threshold for the number of attributes when printing
 *                           information for small number of attributes. N is an integer greater
 *                           than 0. The default threshold is 10.
 * \li <strong>--freespace</strong> Print free space information
 * \li <strong>--summary</strong> Print summary of file space information
 * \li <strong>--page-buffer-size=N</strong> Set the page buffer cache size, N=non-negative integers
 * \li <strong>--s3-cred=C</strong> Supply S3 authentication information to "ros3" vfd.
 *                   Accepts tuple of \code (\<aws-region\>,\<access-id\>,\<access-key\>) \endcode.
 *                   If absent or C = \code (,,) \endcode defaults to no-authentication.
 *                   Has no effect if vfd flag not set to "ros3".
 * \li <strong>--hdfs-attrs=A</strong> Supply configuration information to Hadoop VFD.
 *                   Accepts tuple of \code (\<namenode name\>,\<namenode port\>,
 *                   ...\<kerberos cache path\>,\<username\>,\<buffer size\>) \endcode
 *                   If absent or A == \code (,,,,) \endcode all default values are used.
 *                   Has no effect if vfd flag is not 'hdfs'.<br />
 *                   If an attribute is empty, a default value will be used.
 *
 */

#endif /* H5STAT_H */
