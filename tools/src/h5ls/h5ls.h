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
#ifndef H5LS_H
#define H5LS_H

/** \page H5TOOL_LS_UG The HDF5 h5ls Tool
 *
 * Navigate back: \ref index "Main" / \ref UG / \ref CommandTools
 * <hr>
 *
 * \section sec_cltools_h5ls h5ls
 *
 * \subsection subsec_cltools_h5ls_intro Introduction
 *  With h5ls, you can dump objects from an HDF5 file.
 *
 * \subsection subsec_cltools_h5ls_usage Usage
 *  <h4>h5ls [OPTIONS] file[/OBJECT] [file[/[OBJECT]...]</h4>
 *
 * \subsection subsec_cltools_h5ls_objs file/OBJECT
 *    Each object consists of an HDF5 file name optionally followed by a
 *    slash and an object name within the file (if no object is specified
 *    within the file then the contents of the root group are displayed).
 *    The file name may include a printf(3C) integer format such as
 *    "%05d" to open a file family.
 *
 * \subsection subsec_cltools_h5ls_error Error Report Option
 * \li <strong>--enable-error-stack</strong> Prints messages from the HDF5 error stack as they occur.
 *             Optional value 2 also prints file open errors, --enable-error-stack=2.
 *
 * \subsection subsec_cltools_h5ls_options Options
 * \li <strong>--help</strong>    Print a usage message and exit
 * \li <strong>--address</strong> Print raw data address. If dataset is contiguous, address
 *                   is offset in file of beginning of raw data. If chunked,
 *                   returned list of addresses indicates offset of each chunk.
 *                   Must be used with <strong>--verbose option</strong>.
 *                   Provides no information for non-dataset objects.
 * \li <strong>--data</strong> Print the values of datasets
 * \li <strong>--follow-symlinks</strong>
 *                   Follow symbolic links (soft links and external links)
 *                   to display target object information.<br />
 *                   Without this option, h5ls identifies a symbolic link
 *                   as a soft link or external link and prints the value
 *                   assigned to the symbolic link; it does not provide any
 *                   information regarding the target object or determine
 *                   whether the link is a dangling link.
 * \li <strong>--no-dangling-links</strong>
 *                   Must be used with <strong>--follow-symlinks</strong> option;
 *                   otherwise, h5ls shows error message and returns an exit
 *                   code of 1.<br />
 *                   Check for any symbolic links (soft links or external links)
 *                   that do not resolve to an existing object (dataset, group,
 *                   or named datatype).<br />
 *                   If any dangling link is found, this situation is treated
 *                   as an error and h5ls returns an exit code of 1.
 * \li <strong>--full</strong>      Print full path names instead of base names
 * \li <strong>--group</strong>     Show information about a group, not its contents
 * \li <strong>--label</strong>     Label members of compound datasets
 * \li <strong>--recursive</strong> List all groups recursively, avoiding cycles
 * \li <strong>--string</strong>    Print 1-byte integer datasets as ASCII
 * \li <strong>--simple</strong>    Use a machine-readable output format
 * \li <strong>--width=N</strong>   Set the number of columns of output
 * \li <strong>--verbose</strong>   Generate more verbose output
 * \li <strong>--version</strong>   Print the library version number and exit
 * \li <strong>--page-buffer-size=N</strong> Set the page buffer cache size, N=non-negative integers
 * \li <strong>--vfd=DRIVER</strong>   Use the specified virtual file driver
 * \li <strong>--hexdump</strong>      Show raw data in hexadecimal format
 * \li <strong>--s3-cred=C</strong>    Supply S3 authentication information to "ros3" vfd.
 *                   Accepts tuple of \code (<aws-region>,<access-id>,<access-key>) \endcode
 *                   or \code (<aws-region>,<access-id>,<access-key>,<session-token>) \endcode.
 *                   If absent or C = \code (,,) \endcode or C = \code (,,,) \endcode defaults to
 * no-authentication. Has no effect if vfd flag not set to "ros3". \li <strong>--hdfs-attrs=A</strong> Supply
 * configuration information to Hadoop VFD. Accepts tuple of \code (<namenode name>,<namenode port>,
 *                   ...<kerberos cache path>,<username>,<buffer size>) \endcode
 *                   If absent or A == \code (,,,,) \endcode all default values are used.
 *                   Has no effect if vfd flag is not 'hdfs'.
 * \li <strong>--vol-value</strong> Value (ID) of the VOL connector to use for opening the
 *                   HDF5 file specified
 * \li <strong>--vol-name</strong>  Name of the VOL connector to use for opening the
 *                   HDF5 file specified
 * \li <strong>--vol-info</strong>  VOL-specific info to pass to the VOL connector used for
 *                   opening the HDF5 file specified
 *                   If none of the above options are used to specify a VOL, then
 *                   the VOL named by HDF5_VOL_CONNECTOR (or the native VOL connector,
 *                   if that environment variable is unset) will be used
 * \li <strong>--vfd-value</strong>  Value (ID) of the VFL driver to use for opening the
 *                   HDF5 file specified
 * \li <strong>--vfd-name</strong>   Name of the VFL driver to use for opening the
 *                   HDF5 file specified
 * \li <strong>--vfd-info</strong>   VFD-specific info to pass to the VFL driver used for
 *                   opening the HDF5 file specified
 *
 * \subsubsection subsubsec_cltools_h5ls_options_depre Deprecated Options
 *    The following options have been removed in HDF5 1.12. Use the indicated
 *    replacement option in all work.
 * \li <strong>--external</strong>  Follow external links.<br />
 *                      Replaced by <strong>--follow-symlinks</strong>.
 * \li <strong>--errors</strong>    Show all HDF5 error reporting<br />
 *                      Replaced by <strong>--enable-error-stack</strong>.
 *
 * Previous Chapter \ref sec_cltools_h5jam - Next Chapter \ref sec_cltools_h5repack
 *
 * <hr>
 * Navigate back: \ref index "Main" / \ref UG / \ref CommandTools
 *
 */

#endif /* H5LS_H */
