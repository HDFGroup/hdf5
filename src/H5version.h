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
 *      set the versions for the API symbols affected.
 *
 * Note: If an application has already chosen a particular version for an
 *      API symbol, the individual API version macro takes priority.
 */
#ifdef H5_USE_16_API

/*************/
/* Functions */
/*************/

#if !defined(H5Eclear_vers)
#define H5Eclear_vers 1
#endif /* !defined(H5Eclear_vers) */

#if !defined(H5Eget_auto_vers)
#define H5Eget_auto_vers 1
#endif /* !defined(H5Eget_auto_vers) */

#if !defined(H5Eprint_vers)
#define H5Eprint_vers 1
#endif /* !defined(H5Eprint_vers) */

#if !defined(H5Epush_vers)
#define H5Epush_vers 1
#endif /* !defined(H5Epush_vers) */

#if !defined(H5Eset_auto_vers)
#define H5Eset_auto_vers 1
#endif /* !defined(H5Eset_auto_vers) */

#if !defined(H5Ewalk_vers)
#define H5Ewalk_vers 1
#endif /* !defined(H5Ewalk_vers) */

#if !defined(H5Gcreate_vers)
#define H5Gcreate_vers 1
#endif /* !defined(H5Gcreate_vers) */

#if !defined(H5Gopen_vers)
#define H5Gopen_vers 1
#endif /* !defined(H5Gopen_vers) */

#if !defined(H5Rget_obj_type_vers)
#define H5Rget_obj_type_vers 1
#endif /* !defined(H5Rget_obj_type_vers) */

/************/
/* Typedefs */
/************/

#if !defined(H5E_auto_t_vers)
#define H5E_auto_t_vers 1
#endif /* !defined(H5E_auto_t_vers) */

#endif /* H5_USE_16_API */


/* Choose the correct version of each API symbol, defaulting to the latest
 *      version of each.  The "best" name for API parameters/data structures
 *      that have changed definitions is also set.  An error is issued for
 *      specifying an invalid API version.
 */

/*************/
/* Functions */
/*************/

#if !defined(H5Eclear_vers) || H5Eclear_vers == 2
#ifndef H5Eclear_vers
#define H5Eclear_vers 2
#endif /* H5Eclear_vers */
#define H5Eclear H5Eclear2
#elif H5Eclear_vers == 1
#define H5Eclear H5Eclear1
#else /* H5Eclear_vers */
#error "H5Eclear_vers set to invalid value"
#endif /* H5Eclear_vers */

#if !defined(H5Eget_auto_vers) || H5Eget_auto_vers == 2
#ifndef H5Eget_auto_vers
#define H5Eget_auto_vers 2
#endif /* H5Eget_auto_vers */
#define H5Eget_auto H5Eget_auto2
#elif H5Eget_auto_vers == 1
#define H5Eget_auto H5Eget_auto1
#else /* H5Eget_auto_vers */
#error "H5Eget_auto_vers set to invalid value"
#endif /* H5Eget_auto_vers */

#if !defined(H5Eprint_vers) || H5Eprint_vers == 2
#ifndef H5Eprint_vers
#define H5Eprint_vers 2
#endif /* H5Eprint_vers */
#define H5Eprint H5Eprint2
#elif H5Eprint_vers == 1
#define H5Eprint H5Eprint1
#else /* H5Eprint_vers */
#error "H5Eprint_vers set to invalid value"
#endif /* H5Eprint_vers */

#if !defined(H5Epush_vers) || H5Epush_vers == 2
#ifndef H5Epush_vers
#define H5Epush_vers 2
#endif /* H5Epush_vers */
#define H5Epush H5Epush2
#elif H5Epush_vers == 1
#define H5Epush H5Epush1
#else /* H5Epush_vers */
#error "H5Epush_vers set to invalid value"
#endif /* H5Epush_vers */

#if !defined(H5Eset_auto_vers) || H5Eset_auto_vers == 2
#ifndef H5Eset_auto_vers
#define H5Eset_auto_vers 2
#endif /* H5Eset_auto_vers */
#define H5Eset_auto H5Eset_auto2
#elif H5Eset_auto_vers == 1
#define H5Eset_auto H5Eset_auto1
#else /* H5Eset_auto_vers */
#error "H5Eset_auto_vers set to invalid value"
#endif /* H5Eset_auto_vers */

#if !defined(H5Ewalk_vers) || H5Ewalk_vers == 2
#ifndef H5Ewalk_vers
#define H5Ewalk_vers 2
#endif /* H5Ewalk_vers */
#define H5Ewalk H5Ewalk2
#define H5E_error_t H5E_error2_t
#define H5E_walk_t H5E_walk2_t
#elif H5Ewalk_vers == 1
#define H5Ewalk H5Ewalk1
#define H5E_error_t H5E_error1_t
#define H5E_walk_t H5E_walk1_t
#else /* H5Ewalk_vers */
#error "H5Ewalk_vers set to invalid value"
#endif /* H5Ewalk_vers */

#if !defined(H5Gcreate_vers) || H5Gcreate_vers == 2
#ifndef H5Gcreate_vers
#define H5Gcreate_vers 2
#endif /* H5Gcreate_vers */
#define H5Gcreate H5Gcreate2
#elif H5Gcreate_vers == 1
#define H5Gcreate H5Gcreate1
#else /* H5Gcreate_vers */
#error "H5Gcreate_vers set to invalid value"
#endif /* H5Gcreate_vers */

#if !defined(H5Gopen_vers) || H5Gopen_vers == 2
#ifndef H5Gopen_vers
#define H5Gopen_vers 2
#endif /* H5Gopen_vers */
#define H5Gopen H5Gopen2
#elif H5Gopen_vers == 1
#define H5Gopen H5Gopen1
#else /* H5Gopen_vers */
#error "H5Gopen_vers set to invalid value"
#endif /* H5Gopen_vers */

#if !defined(H5Rget_obj_type_vers) || H5Rget_obj_type_vers == 2
#ifndef H5Rget_obj_type_vers
#define H5Rget_obj_type_vers 2
#endif /* H5Rget_obj_type_vers */
#define H5Rget_obj_type H5Rget_obj_type2
#elif H5Rget_obj_type_vers == 1
#define H5Rget_obj_type H5Rget_obj_type1
#else /* H5Rget_obj_type_vers */
#error "H5Rget_obj_type_vers set to invalid value"
#endif /* H5Rget_obj_type_vers */

/************/
/* Typedefs */
/************/

#if !defined(H5E_auto_t_vers) || H5E_auto_t_vers == 2
#ifndef H5E_auto_t_vers
#define H5E_auto_t_vers 2
#endif /* H5E_auto_t_vers */
#define H5E_auto_t H5E_auto2_t
#elif H5E_auto_t_vers == 1
#define H5E_auto_t H5E_auto1_t
#else /* H5E_auto_t_vers */
#error "H5E_auto_t_vers set to invalid value"
#endif /* H5E_auto_t_vers */

#endif /* H5version_H */

