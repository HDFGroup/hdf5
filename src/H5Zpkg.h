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
#include "H5Oprivate.h"          /* Object headers                  */
#include "H5Iprivate.h"          /* IDs                             */
#include "H5Sprivate.h"          /* Space                           */

/* The initial version of the format */
#define H5O_PLINE_VERSION_1	1

/* This version encodes the message fields more efficiently */
/* (Drops the reserved bytes, doesn't align the name and doesn't encode the
 *      filter name at all if it's a filter provided by the library)
 */
#define H5O_PLINE_VERSION_2	2

/* The latest version of the format.  Look through the 'encode' and 'size'
 *      callbacks for places to change when updating this. */
#define H5O_PLINE_VERSION_LATEST H5O_PLINE_VERSION_2


#ifdef H5_HAVE_FILTER_DEFLATE
/*
 * Deflate filter
 */
H5_DLLVAR const H5Z_class_t H5Z_DEFLATE[1];
#endif /* H5_HAVE_FILTER_DEFLATE */

#ifdef H5_HAVE_FILTER_SHUFFLE
/*
 * Shuffle filter
 */
H5_DLLVAR const H5Z_class_t H5Z_SHUFFLE[1];
#endif /* H5_HAVE_FILTER_SHUFFLE */

#ifdef H5_HAVE_FILTER_FLETCHER32
/*
 * Fletcher32 filter
 */
H5_DLLVAR const H5Z_class_t H5Z_FLETCHER32[1];
#endif /* H5_HAVE_FILTER_FLETCHER32 */

#ifdef H5_HAVE_FILTER_SZIP
/*
 * szip filter
 */
H5_DLLVAR H5Z_class_t H5Z_SZIP[1];
#endif /* H5_HAVE_FILTER_SZIP */

#ifdef H5_HAVE_FILTER_NBIT
/*
 * nbit filter
 */
H5_DLLVAR H5Z_class_t H5Z_NBIT[1];
#endif /* H5_HAVE_FILTER_NBIT */

#ifdef H5_HAVE_FILTER_SCALEOFFSET
/*
 * scaleoffset filter
 */
H5_DLLVAR H5Z_class_t H5Z_SCALEOFFSET[1];
#endif /* H5_HAVE_FILTER_SCALEOFFSET */

#ifdef H5_HAVE_FILTER_DTYPE_MODIFY
/*
 * datatype modification filter
 */
H5_DLLVAR H5Z_class_t H5Z_DTYPE_MODIFY[1];
#endif /* H5_HAVE_FILTER_DTYPE_MODIFY */

/* Package-local function prototypes */
H5_DLL void H5Z_update_class_vers(H5Z_class_t * old_vers, H5Z_class_t * curr_vers);

#endif /* _H5Zpkg_H */

