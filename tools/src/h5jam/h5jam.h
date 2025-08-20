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
#ifndef H5JAM_H
#define H5JAM_H

/** \page H5TOOL_JAM_UG The HDF5 h5jam/h5unjam Tool
 *
 * Navigate back: \ref index "Main" / \ref UG / \ref CommandTools
 * <hr>
 *
 * \section sec_cltools_h5jam h5jam
 *
 * \subsection subsec_cltools_h5jam_intro Introduction
 * \li h5jam Adds user block to the front of an HDF5 file and creates a new concatenated file.
 * \li h5unjam Splits an HDF5 file into two files, one containing the user block data and the other the HDF5
 * data.
 *
 * \subsection subsec_cltools_h5jam_usage Usage
 *  <h4>h5jam -i \<in_file.h5\> -u \<in_user_file\> [-o \<out_file.h5\>] [--clobber]</h4>
 *  <h4>h5unjam -i \<in_file.h5\> [-o \<out_file.h5\> ] [-u \<out_user_file\> | --delete]</h4>
 *
 * \subsection subsec_cltools_h5jam_options h5jam Options
 * \li <strong>-i in_file.h5</strong>   Specifies the input HDF5 file.
 * \li <strong>-u in_user_file</strong> Specifies the file to be inserted into the user block.
 *                   Can be any file format except an HDF5 format.
 * \li <strong>-o out_file.h5</strong>  Specifies the output HDF5 file.
 *                   If not specified, the user block will be concatenated in
 *                   place to the input HDF5 file.
 * \li <strong>--clobber</strong> Wipes out any existing user block before concatenating
 *                   the given user block.
 *                   The size of the new user block will be the larger of:
 *                    - the size of existing user block in the input HDF5 file
 *                    - the size of user block required by new input user file
 *                   (size = 512 x 2N,  N is positive integer.)
 * \li <strong>-h</strong> Prints a usage message and exits.
 * \li <strong>-V</strong> Prints the HDF5 library version and exits.
 *
 * \section sec_cltools_h5unjam h5unjam
 * \subsection subsec_cltools_h5unjam_options h5unjam Options
 * \li <strong>-i in_file.h5</strong> Specifies the HDF5 as input.  If the input HDF5 file
 *                  contains no user block, exit with an error message.
 * \li <strong>-o out_file.h5</strong> Specifies output HDF5 file without a user block.
 *                  If not specified, the user block will be removed from the
 *                  input HDF5 file.
 * \li <strong>-u out_user_file</strong>
 *                  Specifies the output file containing the data from the
 *                  user block.
 *                  Cannot be used with --delete option.
 * \li <strong>--delete</strong> Remove the user block from the input HDF5 file. The content
 *                  of the user block is discarded.
 *                  Cannot be used with the -u option.
 * \li <strong>-h</strong> Prints a usage message and exits.
 * \li <strong>-V</strong> Prints the HDF5 library version and exits.
 *
 * If neither <strong>--delete</strong> nor <strong>-u</strong> is specified, the user block from the input
 * file will be displayed to stdout stream.
 *
 * Previous Chapter \ref sec_cltools_h5import - Next Chapter \ref sec_cltools_h5ls
 *
 * <hr>
 * Navigate back: \ref index "Main" / \ref UG / \ref CommandTools
 *
 */

#endif /* H5JAM_H */
