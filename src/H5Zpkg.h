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

#if !(defined H5Z_FRIEND || defined H5Z_MODULE)
#error "Do not include this file outside the H5Z package!"
#endif

#ifndef H5Zpkg_H
#define H5Zpkg_H

/* Include private header file */
#include "H5Zprivate.h" /* Filter functions                */

/* The built-in filter class structs are static in each filter's source file
 * and reached through these functions, so that libhdf5 does not export
 * symbols such as H5Z_DEFLATE that a plugin built from a copy of the filter
 * source might also define.
 */

/********************/
/* Internal filters */
/********************/

/* Shuffle filter */
H5_DLL const H5Z_class3_t *H5Z__shuffle_class(void);

/* Fletcher32 filter */
H5_DLL const H5Z_class3_t *H5Z__fletcher32_class(void);

/* n-bit filter */
H5_DLL const H5Z_class3_t *H5Z__nbit_class(void);

/* Scale/offset filter */
H5_DLL const H5Z_class3_t *H5Z__scaleoffset_class(void);

/********************/
/* External filters */
/********************/

/* Deflate filter */
#ifdef H5_HAVE_FILTER_DEFLATE
H5_DLL const H5Z_class3_t *H5Z__deflate_class(void);
#endif /* H5_HAVE_FILTER_DEFLATE */

/* szip filter */
#ifdef H5_HAVE_FILTER_SZIP
H5_DLL H5Z_class3_t *H5Z__szip_class(void);
#endif /* H5_HAVE_FILTER_SZIP */

/* Package internal routines */
H5_DLL herr_t H5Z__unregister(H5Z_filter_t filter_id);

#endif /* H5Zpkg_H */
