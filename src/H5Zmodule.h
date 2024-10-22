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

/*
 * Purpose: This file contains declarations which define macros for the
 *          H5Z package.  Including this header means that the source file
 *          is part of the H5Z package.
 */
#ifndef H5Zmodule_H
#define H5Zmodule_H

/* Define the proper control macros for the generic FUNC_ENTER/LEAVE and error
 *      reporting macros.
 */
#define H5Z_MODULE
#define H5_MY_PKG     H5Z
#define H5_MY_PKG_ERR H5E_PLINE

/** \page H5Z_UG HDF5 Filters
 * @todo Under Construction
 */

/**
 * \defgroup H5Z Filters (H5Z)
 *
 * Use the functions in this module to manage HDF5 filters.
 *
 * User-defined filters are created by registering a filter descriptor of
 * type #H5Z_class_t with the library.
 *
 * Available filters can be read or examined at runtime.
 *
 * It is conceivable that filters are stateful and that that state be
 * updated at runtime.
 *
 * Filters are deleted by unregistering.
 *
 * <table>
 * <tr><th>Create</th><th>Read</th></tr>
 * <tr valign="top">
 *   <td>
 *   \snippet{lineno} H5Z_examples.c filter
 *   \snippet{lineno} H5Z_examples.c create
 *   </td>
 *   <td>
 *   \snippet{lineno} H5Z_examples.c read
 *   </td>
 * </tr>
 * <tr><th>Update</th><th>Delete</th></tr>
 * <tr valign="top">
 *   <td>
 *   \snippet{lineno} H5Z_examples.c update
 *   </td>
 *   <td>
 *   \snippet{lineno} H5Z_examples.c delete
 * </tr>
 * </table>
 *
 * HDF5 supports a filter pipeline that provides the capability for standard and
 * customized raw data processing during I/O operations.  HDF5 is distributed
 * with a small set of standard filters such as compression (gzip, SZIP, and a
 * shuffling algorithm) and error checking (Fletcher32 checksum). For further
 * flexibility, the library allows a user application to extend the pipeline
 * through the creation and registration of customized filters.
 * See \ref sec_filter_plugins
 *
 * The flexibility of the filter pipeline implementation enables the definition
 * of additional filters by a user application. A filter
 * \li is associated with a dataset when the dataset is created,
 * \li can be used only with chunked data (i.e., datasets stored in the
 *     #H5D_CHUNKED storage layout), and
 * \li is applied independently to each chunk of the dataset.
 *
 * The HDF5 library does not support filters for contiguous datasets because of
 * the difficulty of implementing random access for partial I/O. Compact dataset
 * filters are not supported because they would not produce significant results.
 *
 * HDF5 allows chunked data to pass through user-defined filters
 * on the way to or from disk.  The filters operate on chunks of an
 * #H5D_CHUNKED dataset can be arranged in a pipeline
 * so output of one filter becomes the input of the next filter.
 *
 * Each filter has a two-byte identification number (type
 * #H5Z_filter_t) allocated by The HDF Group and can also be
 * passed application-defined integer resources to control its
 * behavior.  Each filter also has an optional ASCII comment
 * string.
 *
 *     <table>
 *     <tr>
 *       <th>Values for <code>#H5Z_filter_t</code></th><th>Description</th>
 *     </tr>
 *     <tr>
 *       <td><code>0-255</code></td>
 *       <td>These values are reserved for filters predefined and
 *            registered by the HDF5 library and of use to the general
 *            public.</td>
 *     </tr>
 *     <tr>
 *       <td><code>256-511</code></td>
 *       <td>Filter numbers in this range are used for testing only
 *            and can be used temporarily by any organization.  No
 *            attempt is made to resolve numbering conflicts since all
 *            definitions are by nature temporary.</td>
 *     </tr>
 *     <tr>
 *       <td><code>512-65535</code></td>
 *       <td>Reserved for future assignment. Please contact the
 *            <a href="mailto:help@hdfgroup.org">HDF5 development team</a>
 *            to reserve a value or range of values for
 *            use by your filters.</td>
 *    </tr>
 *    </table>
 *
 * Filter identifiers for the filters distributed with the HDF5
 * Library are as follows:
 * <table>
 *   <tr><td>#H5Z_FILTER_DEFLATE</td><td>The gzip compression, or
 *           deflation, filter</td></tr>
 *   <tr><td>#H5Z_FILTER_SZIP</td><td>The SZIP compression
 *           filter</td></tr>
 *   <tr><td>#H5Z_FILTER_NBIT</td><td>The N-bit compression
 *           filter</td></tr>
 *   <tr><td>#H5Z_FILTER_SCALEOFFSET</td><td>The scale-offset
 *           compression filter</td></tr>
 *   <tr><td>#H5Z_FILTER_SHUFFLE</td><td>The shuffle algorithm
 *           filter</td></tr>
 *   <tr><td>#H5Z_FILTER_FLETCHER32</td><td>The Fletcher32 checksum,
 *           or error checking, filter</td></tr>
 * </table>
 * Custom filters that have been registered with the library will have
 * additional unique identifiers.
 *
 * See \ref sec_filter_plugins for more information on how an HDF5 application can
 * apply a filter that is not registered with the HDF5 library.
 *
 * \defgroup H5ZPRE Predefined Filters
 * \ingroup H5Z
 * \defgroup FLETCHER32 Checksum Filter
 * \ingroup H5ZPRE
 * \defgroup SCALEOFFSET Scale-Offset Filter
 * \ingroup H5ZPRE
 * \defgroup SHUFFLE Shuffle Filter
 * \ingroup H5ZPRE
 * \defgroup SZIP Szip Filter
 * \ingroup H5ZPRE
 *
 */

#endif /* H5Zmodule_H */
