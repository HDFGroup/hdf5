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

/* Generated automatically by bin/make_vers -- do not edit */
/* Add new versioned symbols to H5vers.txt file */


#ifndef _H5version_H
#define _H5version_H

/* Issue error if contradicting macros have been defined. */
#if defined(H5_USE_16_API) && defined(H5_NO_DEPRECATED_SYMBOLS)
#error "Can't choose old API versions when deprecated APIs are disabled"
#endif /* defined(H5_USE_16_API) && defined(H5_NO_DEPRECATED_SYMBOLS) */

/* If a particular "global" version of the library's interfaces is chosen,
 *      set the versions for the API routines affected.
 *
 * Note: If an application has already chosen a particular version for an
 *      API routine, the individual API version macro takes priority.
 */
#ifdef H5_USE_16_API
#endif /* H5_USE_16_API */

/* Choose the correct version of each API routine, defaulting to the latest
 *      version of each API routine.  The "best" name for API parameters/data
 *      structures that have changed definitions is also set.  An error is
 *      issued for specifying an invalid API version.
 */

#endif /* H5version_H */

