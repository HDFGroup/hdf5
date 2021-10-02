/*
 * Copyright (C) 2013-2020 Argonne National Laboratory, Department of Energy,
 *                    UChicago Argonne, LLC and The HDF Group.
 * All rights reserved.
 *
 * The full copyright notice, including terms governing use, modification,
 * and redistribution, is contained in the COPYING file that can be
 * found at the root of the source code distribution tree.
 */

/* Generated file. Only edit na_config.h.in. */

#ifndef NA_CONFIG_H
#define NA_CONFIG_H

/*************************************/
/* Public Type and Struct Definition */
/*************************************/

/* Type definitions */
#ifdef _WIN32
typedef signed __int64   na_int64_t;
typedef signed __int32   na_int32_t;
typedef signed __int16   na_int16_t;
typedef signed __int8    na_int8_t;
typedef unsigned __int64 na_uint64_t;
typedef unsigned __int32 na_uint32_t;
typedef unsigned __int16 na_uint16_t;
typedef unsigned __int8  na_uint8_t;
#else
#include <stddef.h>
#include <stdint.h>
typedef int64_t  na_int64_t;
typedef int32_t  na_int32_t;
typedef int16_t  na_int16_t;
typedef int8_t   na_int8_t;
typedef uint64_t na_uint64_t;
typedef uint32_t na_uint32_t;
typedef uint16_t na_uint16_t;
typedef uint8_t  na_uint8_t;
#endif
typedef na_uint8_t  na_bool_t;
typedef na_uint64_t na_ptr_t;

/* True / false */
#define NA_TRUE  1
#define NA_FALSE 0

/*****************/
/* Public Macros */
/*****************/

/* Visibility of symbols */
#if defined(_WIN32)
#define NA_ABI_IMPORT __declspec(dllimport)
#define NA_ABI_EXPORT __declspec(dllexport)
#define NA_ABI_HIDDEN
#elif defined(__GNUC__) && (__GNUC__ >= 4)
#define NA_ABI_IMPORT __attribute__((visibility("default")))
#define NA_ABI_EXPORT __attribute__((visibility("default")))
#define NA_ABI_HIDDEN __attribute__((visibility("hidden")))
#else
#define NA_ABI_IMPORT
#define NA_ABI_EXPORT
#define NA_ABI_HIDDEN
#endif

/* Inline macro */
#ifdef _WIN32
#define NA_INLINE __inline
#else
#define NA_INLINE __inline__
#endif

/* Unused return values */
#if defined(__GNUC__)
#define NA_WARN_UNUSED_RESULT __attribute__((warn_unused_result))
#else
#define NA_WARN_UNUSED_RESULT
#endif

/* Fallthrough macro */
#if defined(__GNUC__) && (__GNUC__ >= 7)
#define NA_FALLTHROUGH() __attribute__((fallthrough))
#else
#define NA_FALLTHROUGH()
#endif

/* Shared libraries */
/* #undef NA_BUILD_SHARED_LIBS */
#ifdef NA_BUILD_SHARED_LIBS
#ifdef na_EXPORTS
#define NA_PUBLIC NA_ABI_EXPORT
#else
#define NA_PUBLIC NA_ABI_IMPORT
#endif
#define NA_PRIVATE NA_ABI_HIDDEN
#else
#define NA_PUBLIC
#define NA_PRIVATE
#endif

/* Build Options */
#define NA_HAS_MULTI_PROGRESS
/* #undef NA_HAS_DEBUG */

/* BMI */
/* #undef NA_HAS_BMI */

/* MPI */
/* #undef NA_HAS_MPI */
/* #undef NA_MPI_HAS_GNI_SETUP */

/* CCI */
/* #undef NA_HAS_CCI */

/* OFI */
/* #undef NA_HAS_OFI */
/* #undef NA_OFI_HAS_EXT_GNI_H */
/* #undef NA_OFI_GNI_HAS_UDREG */

/* NA SM */
#define NA_HAS_SM
/* #undef NA_SM_HAS_UUID */
#define NA_SM_HAS_CMA
#define NA_SM_SHM_PREFIX    "na_sm"
#define NA_SM_TMP_DIRECTORY "/tmp"

/* UCX */
/* #undef NA_HAS_UCX */
/* #undef NA_UCX_HAS_LIB_QUERY */
/* #undef NA_UCX_HAS_THREAD_MODE_NAMES */

#endif /* NA_CONFIG_H */
