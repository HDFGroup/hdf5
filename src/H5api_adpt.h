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
 * H5api_adpt.h
 *
 * API decorations for exported symbols
 */
#ifndef H5API_ADPT_H
#define H5API_ADPT_H

#ifdef H5_BUILT_AS_DYNAMIC_LIB

/* When building with MSVC, we need to decorate the functions. NOTE that
 * _MSC_VER is also defined by clang + Visual Studio.
 */
#if defined(_MSC_VER)
#if defined(hdf5_shared_EXPORTS)
#define H5_DLL    __declspec(dllexport)
#define H5_DLLVAR extern __declspec(dllexport)
#else
#define H5_DLL    __declspec(dllimport)
#define H5_DLLVAR __declspec(dllimport)
#endif /* hdf5_shared_EXPORTS */
#endif /* _MSC_VER */

/* gcc (and clang, which also declares __GNUC__) supports visibility attributes.
 * Build with -fvisibility=hidden to hide everything else.
 */
#if defined(__GNUC__)
#define H5_DLL    __attribute__((visibility("default")))
#define H5_DLLVAR extern __attribute__((visibility("default")))
#endif

/* Shared library API decorations for anything not covered above */
#ifndef H5_DLL
#define H5_DLL
#define H5_DLLVAR extern
#endif

#else /* H5_BUILT_AS_DYNAMIC_LIB */

/* Static library decorations */
#define H5_DLL
#define H5_DLLVAR extern

#endif /* H5_BUILT_AS_DYNAMIC_LIB */

#endif /* H5API_ADPT_H */
