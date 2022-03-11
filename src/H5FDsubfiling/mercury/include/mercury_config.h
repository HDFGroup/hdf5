/*
 * Copyright (C) 2013-2020 Argonne National Laboratory, Department of Energy,
 *                    UChicago Argonne, LLC and The HDF Group.
 * All rights reserved.
 *
 * The full copyright notice, including terms governing use, modification,
 * and redistribution, is contained in the COPYING file that can be
 * found at the root of the source code distribution tree.
 */

/* Generated file. Only edit mercury_config.h.in. */

#ifndef MERCURY_CONFIG_H
#define MERCURY_CONFIG_H

/*************************************/
/* Public Type and Struct Definition */
/*************************************/

/* Type definitions */
#ifdef _WIN32
typedef signed __int64   hg_int64_t;
typedef signed __int32   hg_int32_t;
typedef signed __int16   hg_int16_t;
typedef signed __int8    hg_int8_t;
typedef unsigned __int64 hg_uint64_t;
typedef unsigned __int32 hg_uint32_t;
typedef unsigned __int16 hg_uint16_t;
typedef unsigned __int8  hg_uint8_t;
/* Limits on Integer Constants */
#define UINT64_MAX _UI64_MAX
#else
#include <stddef.h>
#include <stdint.h>
typedef int64_t  hg_int64_t;
typedef int32_t  hg_int32_t;
typedef int16_t  hg_int16_t;
typedef int8_t   hg_int8_t;
typedef uint64_t hg_uint64_t;
typedef uint32_t hg_uint32_t;
typedef uint16_t hg_uint16_t;
typedef uint8_t  hg_uint8_t;
#endif
typedef hg_uint64_t hg_ptr_t;
typedef hg_uint8_t  hg_bool_t;

/* True / false */
#define HG_TRUE  1
#define HG_FALSE 0

/*****************/
/* Public Macros */
/*****************/

/* Reflects major releases of Mercury */
#define HG_VERSION_MAJOR 2
/* Reflects any API changes */
#define HG_VERSION_MINOR 1
/* Reflects any library code changes */
#define HG_VERSION_PATCH 0

/* Visibility of symbols */
#if defined(_WIN32)
#define HG_ABI_IMPORT __declspec(dllimport)
#define HG_ABI_EXPORT __declspec(dllexport)
#define HG_ABI_HIDDEN
#elif defined(__GNUC__) && (__GNUC__ >= 4)
#define HG_ABI_IMPORT __attribute__((visibility("default")))
#define HG_ABI_EXPORT __attribute__((visibility("default")))
#define HG_ABI_HIDDEN __attribute__((visibility("hidden")))
#else
#define HG_ABI_IMPORT
#define HG_ABI_EXPORT
#define HG_ABI_HIDDEN
#endif

/* Inline macro */
#ifdef _WIN32
#define HG_INLINE __inline
#else
#define HG_INLINE __inline__
#endif

/* Fallthrough macro */
#if defined(__GNUC__) && (__GNUC__ >= 7)
#define HG_FALLTHROUGH() __attribute__((fallthrough))
#else
#define HG_FALLTHROUGH()
#endif

/* Shared libraries */
/* #undef HG_BUILD_SHARED_LIBS */
#ifdef HG_BUILD_SHARED_LIBS
#ifdef mercury_EXPORTS
#define HG_PUBLIC HG_ABI_EXPORT
#else
#define HG_PUBLIC HG_ABI_IMPORT
#endif
#define HG_PRIVATE HG_ABI_HIDDEN
#else
#define HG_PUBLIC
#define HG_PRIVATE
#endif

/* Build Options */
/* #undef HG_HAS_BOOST */
/* #undef HG_HAS_CHECKSUMS */
/* #undef HG_HAS_XDR */
/* #undef HG_HAS_COLLECT_STATS */

/* #undef HG_HAS_DEBUG */

#endif /* MERCURY_CONFIG_H */
