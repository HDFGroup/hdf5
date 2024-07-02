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

#ifndef H5DIFF_H
#define H5DIFF_H

/** \page H5TOOL_DF_UG The HDF5 h5diff Tool
 *
 * \section sec_cltools_h5diff h5diff
 *
 * \subsection subsec_cltools_h5diff_intro Introduction
 * With h5diff, you can compare objects between an HDF5 file and objects in another or the same HDF5 file.
 *
 * \subsection subsec_cltools_h5diff_usage Usage
 * <h4> h5diff [OPTIONS] file1 file2 [obj1[ obj2]]</h4>
 * \li <strong>file1</strong>  File name of the first HDF5 file
 * \li <strong>file2 </strong> File name of the second HDF5 file
 * \li <strong>[obj1]</strong> Name of an HDF5 object, in absolute path
 * \li <strong>[obj2]</strong> Name of an HDF5 object, in absolute path
 *
 * \subsection subsec_cltools_h5diff_error Error Report
 * \li <strong>--enable-error-stack</strong> Prints messages from the HDF5 error stack as they occur.
 *            Optional value 2 also prints file open errors, --enable-error-stack=2.
 *
 * \subsection subsec_cltools_h5diff_options Options
 * \li  <strong>--help</strong> Print a usage message and exit.
 * \li <strong>--version</strong> Print the library version number and exit.
 * \li <strong>--report</strong> Report mode. Print differences.
 * \li <strong>--verbose</strong> Verbose mode. Print differences information and list of objects.
 * \li <strong>--verbose=N</strong> Verbose mode with level. Print differences and list of objects.
 *         Level of detail depends on value of N:
 *          <ul><li> <strong>0</strong> Identical to '-v' or '--verbose'.</li>
 *          <li> <strong>1</strong> All level 0 information plus one-line attribute status summary.</li>
 *          <li> <strong>2</strong> All level 1 information plus extended attribute status report.</li>
 *          <li> <strong>3</strong> All level 2 information plus file names.</li></ul>
 * \li <strong>--quiet</strong> Quiet mode. Do not produce output.
 * \li <strong>--page-buffer-size=N</strong> Set the page buffer cache size, N=non-negative integers
 * \li <strong>--vol-value-1</strong> Value (ID) of the VOL connector to use for opening the
 *                           first HDF5 file specified
 * \li <strong>--vol-name-1</strong> Name of the VOL connector to use for opening the first
 *                           HDF5 file specified
 * \li <strong>--vol-info-1</strong> VOL-specific info to pass to the VOL connector used for
 *                           opening the first HDF5 file specified
 * \li <strong>--vol-value-2</strong> Value (ID) of the VOL connector to use for opening the
 *                           second HDF5 file specified
 * \li <strong>--vol-name-2</strong> Name of the VOL connector to use for opening the second
 *                           HDF5 file specified
 * \li <strong>--vol-info-2</strong> VOL-specific info to pass to the VOL connector used for
 *                           opening the second HDF5 file specified.<br />
 *                           If none of the above options are used to specify a VOL for a file, then
 *                           the VOL named by HDF5_VOL_CONNECTOR (or the native VOL connector,
 *                           if that environment variable is unset) will be used
 * \li <strong>--vfd-value-1</strong> Value (ID) of the VFL driver to use for opening the
 *                           first HDF5 file specified
 * \li <strong>--vfd-name-1</strong> Name of the VFL driver to use for opening the first
 *                           HDF5 file specified
 * \li <strong>--vfd-info-1 </strong> VFD-specific info to pass to the VFL driver used for
 *                           opening the first HDF5 file specified
 * \li <strong>--vfd-value-2 </strong> Value (ID) of the VFL driver to use for opening the
 *                           second HDF5 file specified
 * \li <strong>--vfd-name-2</strong> Name of the VFL driver to use for opening the second
 *                           HDF5 file specified
 * \li <strong>--vfd-info-2</strong> VFD-specific info to pass to the VFL driver used for
 *                           opening the second HDF5 file specified
 * \li <strong>--follow-symlinks</strong>
 *         Follow symbolic links (soft links and external links) and compare the
 *         links' target objects.<br />
 *         If symbolic link(s) with the same name exist in the files being
 *         compared, then determine whether the target of each link is an existing
 *         object (dataset, group, or named datatype) or the link is a dangling
 *         link (a soft or external link pointing to a target object that does
 *         not yet exist).
 *         <ul><li> If both symbolic links are dangling links, they are treated as being
 *           the same; by default, h5diff returns an exit code of 0.
 *           If, however, --no-dangling-links is used with --follow-symlinks,
 *           this situation is treated as an error and h5diff returns an
 *           exit code of 2.</li>
 *         <li> If only one of the two links is a dangling link,they are treated as
 *           being different and h5diff returns an exit code of 1.
 *           If, however, --no-dangling-links is used with --follow-symlinks,
 *           this situation is treated as an error and h5diff returns an
 *           exit code of 2.</li>
 *         <li> If both symbolic links point to existing objects, h5diff compares the
 *           two objects.</li></ul>
 *         If any symbolic link specified in the call to h5diff does not exist,
 *         h5diff treats it as an error and returns an exit code of 2.
 * \li <strong>--no-dangling-links</strong>
 *         Must be used with <strong>--follow-symlinks</strong> option; otherwise, h5diff shows
 *         error message and returns an exit code of 2.<br />
 *         Check for any symbolic links (soft links or external links) that do not
 *         resolve to an existing object (dataset, group, or named datatype).
 *         If any dangling link is found, this situation is treated as an error
 *         and h5diff returns an exit code of 2.
 * \li <strong>--compare</strong> List objects that are not comparable
 * \li <strong>--nan</strong> Avoid NaNs detection
 * \li <strong>--count=C</strong> Print differences up to \b C. \b C must be a positive integer.
 * \li <strong>--delta=D</strong>
 *         Print difference if (<strong>|a-b| > D</strong>). \b D must be a positive number, where \b a
 *         is the data point value in file1 and b is the data point value in file2.
 *         Can not use with '--relative' or '--use-system-epsilon'.
 * \li <strong>--relative=R</strong>
 *         Print difference if (<strong>|(a-b)/b| > R</strong>). \b R must be a positive number, where \b a
 *         is the data point value in file1 and \b b is the data point value in file2.
 *         Can not use with '--delta' or '--use-system-epsilon'.
 * \li <strong>--use-system-epsilon</strong>
 *         Print difference if (<strong>|a-b| > EPSILON</strong>), \b EPSILON is system defined value, where
 * \b a is the data point value in file1 and \b b is the data point value in file2. If the system epsilon is
 * not defined,one of the following predefined values will be used: <ul><li><code
 * style="background-color:whitesmoke;">FLT_EPSILON = 1.19209E-07</code> for floating-point type</li>
 *             <li><code style="background-color:whitesmoke;">DBL_EPSILON = 2.22045E-16</code>
 *             for double precision</li></ul>
 *         type Can not use with '--relative' or '--delta'.
 * \li <strong>--exclude-path "path"</strong> Exclude the specified path to an object when
 *  comparing files or groups. If a group is excluded, all member objects will also be excluded.
 *  The specified path is excluded wherever it occurs. This flexibility enables the same option
 *  to exclude either objects that exist only in one file or common objects that are known to differ.<br />
 *  When comparing files, "path" is the absolute path to the excluded object;
 *  when comparing groups, "path" is similar to the relative path from the group to the excluded object. This
 *  "path" can be taken from the first section of the output of the <strong>--verbose</strong> option.<br />
 *  For example, if you are comparing the group <code style="background-color:whitesmoke;">/groupA</code>
 *  in two files and you want to exclude
 *  <code style="background-color:whitesmoke;">/groupA/groupB/groupC</code> in both files,
 *  the exclude option would read as follows:<br />
 *  <code style="background-color:whitesmoke;">--exclude-path "/groupB/groupC"</code> <br />
 *  If there are multiple paths to an object, only the specified path(s) will be excluded; the
 *  comparison will include any path not explicitly excluded.<br />
 *  This option can be used repeatedly to
 *  exclude multiple paths.
 * \li <strong>--exclude-attribute "path/to/object/with/attribute"</strong> Exclude
 *  attributes on the specified path to an object when comparing files or groups.<br />
 *  If there are multiple paths to an object, only the specified path(s) will be excluded;
 *  the comparison will include any path not explicitly excluded.<br />
 *  This option can be used repeatedly to exclude multiple paths.
 *
 * \subsubsection subsubsec_cltools_h5diff_modee Modes of output
 * \li <strong>Default mode</strong> print the number of differences found and where they occurred
 * \li <strong>Report mode</strong> print the above plus the differences
 * \li <strong>Verbose mode</strong> print the above plus a list of objects and warnings
 * \li <strong>Quiet mode</strong> do not print output
 *
 * \subsubsection subsubsec_cltools_h5diff_file File comparison
 *  If no objects [obj1[ obj2]] are specified, the h5diff comparison proceeds as
 *  a comparison of the two files' root groups.  That is, h5diff first compares
 *  the names of root group members, generates a report of root group objects
 *  that appear in only one file or in both files, and recursively compares
 *  common objects.
 *
 * \subsubsection subsubsec_cltools_h5diff_object Object comparison
 * \li 1) <strong>Groups</strong>
 *      First compares the names of member objects (relative path, from the
 *      specified group) and generates a report of objects that appear in only
 *      one group or in both groups. Common objects are then compared recursively.
 * \li 2) <strong>Attributes and Datasets</strong>
 *      Array rank and dimensions, datatypes, and data values are compared.
 * \li 3) <strong>Datatypes</strong>
 *      The comparison is based on the return value of H5Tequal.
 * \li 4) <strong>Symbolic links</strong>
 *      The paths to the target objects are compared.
 *      (The option --follow-symlinks overrides the default behavior when
 *       symbolic links are compared.)
 *
 * \subsubsection subsubsec_cltools_h5diff_subset Subsetting Options
 * \li <strong>--no-compact-subset</strong>  Disable compact form of subsetting and allow the use
 *                          of "[" in dataset names.
 *
 *  Subsetting is available by using the fcompact form of subsetting, as follows:
 *  <code style="background-color:whitesmoke;">obj1 /foo/mydataset[START;STRIDE;COUNT;BLOCK]</code>
 *
 *  It is not required to use all parameters, but until the last parameter value used,
 *  all of the semicolons (;) are required, even when a parameter value is not specified.
 *
 *  Example:
 *  <code style="background-color:whitesmoke;">obj1 /foo/mydataset[START;;COUNT;BLOCK]</code>
 *  <code style="background-color:whitesmoke;">obj1 /foo/mydataset[START]</code>
 *
 *  The \b STRIDE, \b COUNT, and \b BLOCK parameters are optional and will default to 1 in
 *  each dimension. \b START is optional and will default to 0 in each dimension.
 *  Each of \b START, \b STRIDE, \b COUNT, and \b BLOCK must be a comma-separated list of integers with
 *  one integer for each dimension of the dataset.
 *
 * \subsubsection subsubsec_cltools_h5diff_exit Exit code
 * \li 0 if no differences
 * \li 1 if differences found
 * \li 2 if error
 *
 * \subsubsection subsubsec_cltools_h5diff_examples Examples
 * \li 1) h5diff file1 file2 /g1/dset1 /g1/dset2
 *
 *     Compares object '/g1/dset1' in file1 with '/g1/dset2' in file2
 *
 * \li 2) h5diff file1 file2 /g1/dset1
 *
 *     Compares object '/g1/dset1' in both files
 *
 * \li 3) h5diff file1 file2
 *
 *     Compares all objects in both files
 *
 * Notes:
 *  file1 and file2 can be the same file.
 *  Use h5diff file1 file1 /g1/dset1 /g1/dset2 to compare '/g1/dset1' and '/g1/dset2' in the same file
 *
 */

#endif /* H5DIFF_H */
