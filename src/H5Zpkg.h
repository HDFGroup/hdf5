/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#ifndef H5Z_PACKAGE
#error "Do not include this file outside the H5Z package!"
#endif

#ifndef _H5Zpkg_H
#define _H5Zpkg_H

/* Include private header file */
#include "H5Zprivate.h"          /* Filter functions                */


/* Package-local function prototypes */
H5_DLL void *H5Z_aligned_malloc(size_t size, void *lib_data);
H5_DLL herr_t H5Z_aligned_free(void *buf, void *lib_data);
H5_DLL void *H5Z_aligned_realloc(void *buf, size_t old_size, size_t new_size,
    void *lib_data);

#ifdef H5_HAVE_FILTER_DEFLATE
/*
 * Deflate filter initialization routine
 */
H5_DLL herr_t H5Z_init_deflate(void);
#endif /* H5_HAVE_FILTER_DEFLATE */

#ifdef H5_HAVE_FILTER_SHUFFLE
/*
 * Shuffle filter initialization routine
 */
H5_DLL herr_t H5Z_init_shuffle(void);
#endif /* H5_HAVE_FILTER_SHUFFLE */

#ifdef H5_HAVE_FILTER_FLETCHER32
/*
 * Fletcher32 filter initialization routine
 */
H5_DLL herr_t H5Z_init_fletcher32(void);
#endif /* H5_HAVE_FILTER_FLETCHER32 */

#ifdef H5_HAVE_FILTER_SZIP
/*
 * szip filter initialization routine
 */
H5_DLL herr_t H5Z_init_szip(void);
#endif /* H5_HAVE_FILTER_SZIP */

#ifdef H5_HAVE_FILTER_NBIT
/*
 * nbit filter initialization routine
 */
H5_DLL herr_t H5Z_init_nbit(void);
#endif /* H5_HAVE_FILTER_NBIT */

#ifdef H5_HAVE_FILTER_SCALEOFFSET
/*
 * scaleoffset filter initialization routine
 */
H5_DLL herr_t H5Z_init_scaleoffset(void);
#endif /* H5_HAVE_FILTER_SCALEOFFSET */

#endif /* _H5Zpkg_H */

