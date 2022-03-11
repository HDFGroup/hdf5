/*
 * Copyright (C) 2013-2020 Argonne National Laboratory, Department of Energy,
 *                    UChicago Argonne, LLC and The HDF Group.
 * All rights reserved.
 *
 * The full copyright notice, including terms governing use, modification,
 * and redistribution, is contained in the COPYING file that can be
 * found at the root of the source code distribution tree.
 */

/* Generated file. Only edit mercury_util_config.h.in. */

#ifndef MERCURY_UTIL_CONFIG_H
#define MERCURY_UTIL_CONFIG_H

/*************************************/
/* Public Type and Struct Definition */
/*************************************/

/* Type definitions */
#ifdef _WIN32
typedef signed __int64   hg_util_int64_t;
typedef signed __int32   hg_util_int32_t;
typedef signed __int16   hg_util_int16_t;
typedef signed __int8    hg_util_int8_t;
typedef unsigned __int64 hg_util_uint64_t;
typedef unsigned __int32 hg_util_uint32_t;
typedef unsigned __int16 hg_util_uint16_t;
typedef unsigned __int8  hg_util_uint8_t;
#else
#include <stddef.h>
#include <stdint.h>
typedef int64_t  hg_util_int64_t;
typedef int32_t  hg_util_int32_t;
typedef int16_t  hg_util_int16_t;
typedef int8_t   hg_util_int8_t;
typedef uint64_t hg_util_uint64_t;
typedef uint32_t hg_util_uint32_t;
typedef uint16_t hg_util_uint16_t;
typedef uint8_t  hg_util_uint8_t;
#endif
typedef hg_util_uint8_t  hg_util_bool_t;
typedef hg_util_uint64_t hg_util_ptr_t;

/* True / false */
#define HG_UTIL_TRUE  1
#define HG_UTIL_FALSE 0

/* Return codes */
#define HG_UTIL_SUCCESS 0
#define HG_UTIL_FAIL    -1

/*****************/
/* Public Macros */
/*****************/

/* Visibility of symbols */
#if defined(_WIN32)
#define HG_UTIL_ABI_IMPORT __declspec(dllimport)
#define HG_UTIL_ABI_EXPORT __declspec(dllexport)
#define HG_UTIL_ABI_HIDDEN
#elif defined(__GNUC__) && (__GNUC__ >= 4)
#define HG_UTIL_ABI_IMPORT __attribute__((visibility("default")))
#define HG_UTIL_ABI_EXPORT __attribute__((visibility("default")))
#define HG_UTIL_ABI_HIDDEN __attribute__((visibility("hidden")))
#else
#define HG_UTIL_ABI_IMPORT
#define HG_UTIL_ABI_EXPORT
#define HG_UTIL_ABI_HIDDEN
#endif

/* Inline macro */
#ifdef _WIN32
#define HG_UTIL_INLINE __inline
#else
#define HG_UTIL_INLINE __inline__
#endif

/* Check format arguments */
#if defined(__GNUC__)
#define HG_UTIL_PRINTF_LIKE(_fmt, _firstarg) __attribute__((format(printf, _fmt, _firstarg)))
#else
#define HG_UTIL_PRINTF_LIKE(_fmt, _firstarg)
#endif

/* Shared libraries */
/* #undef HG_UTIL_BUILD_SHARED_LIBS */
#ifdef HG_UTIL_BUILD_SHARED_LIBS
#ifdef mercury_util_EXPORTS
#define HG_UTIL_PUBLIC HG_UTIL_ABI_EXPORT
#else
#define HG_UTIL_PUBLIC HG_UTIL_ABI_IMPORT
#endif
#define HG_UTIL_PRIVATE HG_UTIL_ABI_HIDDEN
#else
#define HG_UTIL_PUBLIC
#define HG_UTIL_PRIVATE
#endif

/* Define if has __attribute__((constructor)) */
#define HG_UTIL_HAS_ATTR_CONSTRUCTOR

/* Define if has __attribute__((constructor(priority))) */
#define HG_UTIL_HAS_ATTR_CONSTRUCTOR_PRIORITY

/* Define if has 'clock_gettime()' */
#define HG_UTIL_HAS_CLOCK_GETTIME

/* Define if has CLOCK_MONOTONIC_COARSE */
#define HG_UTIL_HAS_CLOCK_MONOTONIC_COARSE

/* Define is has debug */
/* #undef HG_UTIL_HAS_DEBUG */

/* Define if has eventfd_t type */
#define HG_UTIL_HAS_EVENTFD_T

/* Define if has colored output */
/* #undef HG_UTIL_HAS_LOG_COLOR */

/* Define if has <opa_primitives.h> */
/* #undef HG_UTIL_HAS_OPA_PRIMITIVES_H */

/* Define if has 'pthread_condattr_setclock()' */
#define HG_UTIL_HAS_PTHREAD_CONDATTR_SETCLOCK

/* Define if has PTHREAD_MUTEX_ADAPTIVE_NP */
#define HG_UTIL_HAS_PTHREAD_MUTEX_ADAPTIVE_NP

/* Define if has pthread_spinlock_t type */
#define HG_UTIL_HAS_PTHREAD_SPINLOCK_T

/* Define if has <stdatomic.h> */
#define HG_UTIL_HAS_STDATOMIC_H

/* Define type size of atomic_long */
#define HG_UTIL_ATOMIC_LONG_WIDTH 8

/* Define if has <sys/epoll.h> */
#define HG_UTIL_HAS_SYSEPOLL_H

/* Define if has <sys/event.h> */
/* #undef HG_UTIL_HAS_SYSEVENT_H */

/* Define if has <sys/eventfd.h> */
#define HG_UTIL_HAS_SYSEVENTFD_H

/* Define if has <sys/time.h> */
#define HG_UTIL_HAS_SYSTIME_H

/* Define if has <time.h> */
#define HG_UTIL_HAS_TIME_H

#endif /* MERCURY_UTIL_CONFIG_H */
