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
#ifndef H5REPART_H
#define H5REPART_H

/** \page H5TOOL_RT_UG The HDF5 h5repart Tool
 *
 * \section sec_cltools_h5repart h5repart
 *
 * \subsection subsec_cltools_h5repart_intro Introduction
 *  With h5repart, you can repartition a file family. This program can be used to
 *        split a single file into a family of files, join a family of
 *        files into a single file, or copy one family to another while
 *        changing the size of the family members. It can also be used
 *        to copy a single file to a single file with holes.
 *
 * \subsection subsec_cltools_h5repart_usage Usage
 *  <h4>h5repart [OPTIONS] SRC DST</h4>
 *
 * \subsection subsec_cltools_h5repart_objs SRC/DST
 * \li <strong>SRC</strong> The name of the source file
 * \li <strong>DST</strong> The name of the destination files
 *
 * \subsection subsec_cltools_h5repart_options Options
 * \li <strong>-v</strong>   Produce verbose output
 * \li <strong>-V</strong>   Print version number and exit
 * \li <strong>-b N</strong> The I/O block size, defaults to 1kB
 * \li <strong>-m N</strong> The destination member size or 1GB
 * \li <strong>-family_to_sec2</strong>   Deprecated version of -family_to_single (below)
 * \li <strong>-family_to_single</strong> Change file driver from family to the default single-file
 *                      VFD (windows or sec2)
 *
 *  Sizes may be suffixed with 'g' for GB, 'm' for MB or 'k' for kB.
 *  File family names include an integer printf format such as '%%d'
 *
 */

#endif /* H5REPART_H */
