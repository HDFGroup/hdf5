/**
 * Copyright (c) 2013-2021 UChicago Argonne, LLC and The HDF Group.
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#ifndef MERCURY_COMPILER_ATTRIBUTES_H
#define MERCURY_COMPILER_ATTRIBUTES_H

/*************************************/
/* Public Type and Struct Definition */
/*************************************/

/*****************/
/* Public Macros */
/*****************/

/*
 * __has_attribute is supported on gcc >= 5, clang >= 2.9 and icc >= 17.
 * In the meantime, to support gcc < 5, we implement __has_attribute
 * by hand.
 */
#if !defined(__has_attribute) && defined(__GNUC__) && (__GNUC__ >= 4)
#define __has_attribute(x)                          __GCC4_has_attribute_##x
#define __GCC4_has_attribute___visibility__         1
#define __GCC4_has_attribute___warn_unused_result__ 1
#define __GCC4_has_attribute___unused__             1
#define __GCC4_has_attribute___format__             1
#define __GCC4_has_attribute___fallthrough__        0
#endif

/* Visibility of symbols */
#if defined(_WIN32)
#define HG_ATTR_ABI_IMPORT __declspec(dllimport)
#define HG_ATTR_ABI_EXPORT __declspec(dllexport)
#define HG_ATTR_ABI_HIDDEN
#elif __has_attribute(__visibility__)
#define HG_ATTR_ABI_IMPORT __attribute__((__visibility__("default")))
#define HG_ATTR_ABI_EXPORT __attribute__((__visibility__("default")))
#define HG_ATTR_ABI_HIDDEN __attribute__((__visibility__("hidden")))
#else
#define HG_ATTR_ABI_IMPORT
#define HG_ATTR_ABI_EXPORT
#define HG_ATTR_ABI_HIDDEN
#endif

/* Unused return values */
#if defined(_WIN32)
#define HG_ATTR_WARN_UNUSED_RESULT _Check_return_
#elif __has_attribute(__warn_unused_result__)
#define HG_ATTR_WARN_UNUSED_RESULT __attribute__((__warn_unused_result__))
#else
#define HG_ATTR_WARN_UNUSED_RESULT
#endif

/* Remove warnings when plugin does not use callback arguments */
#if defined(_WIN32)
#define HG_ATTR_UNUSED
#elif __has_attribute(__unused__)
#define HG_ATTR_UNUSED __attribute__((__unused__))
#else
#define HG_ATTR_UNUSED
#endif

/* Alignment (not optional) */
#if defined(_WIN32)
#define HG_ATTR_ALIGNED(x, a) __declspec(align(a)) x
#else
#define HG_ATTR_ALIGNED(x, a) x __attribute__((__aligned__(a)))
#endif

/* Packed (not optional) */
#if defined(_WIN32)
#define HG_ATTR_PACKED_PUSH __pragma(pack(push, 1))
#define HG_ATTR_PACKED_POP  __pragma(pack(pop))
#else
#define HG_ATTR_PACKED_PUSH
#define HG_ATTR_PACKED_POP __attribute__((__packed__))
#endif
#define HG_ATTR_PACKED(x) HG_ATTR_PACKED_PUSH x HG_ATTR_PACKED_POP

/* Check format arguments */
#if defined(_WIN32)
#define HG_ATTR_PRINTF(_fmt, _firstarg)
#elif __has_attribute(__format__)
#define HG_ATTR_PRINTF(_fmt, _firstarg) __attribute__((__format__(printf, _fmt, _firstarg)))
#else
#define HG_ATTR_PRINTF(_fmt, _firstarg)
#endif

/* Constructor (not optional) */
#if defined(_WIN32)
#define HG_ATTR_CONSTRUCTOR
#define HG_ATTR_CONSTRUCTOR_PRIORITY(x)
#else
#define HG_ATTR_CONSTRUCTOR             __attribute__((__constructor__))
#define HG_ATTR_CONSTRUCTOR_PRIORITY(x) __attribute__((__constructor__(x)))
#endif

/* Destructor (not optional) */
#if defined(_WIN32)
#define HG_ATTR_DESTRUCTOR
#else
#define HG_ATTR_DESTRUCTOR __attribute__((__destructor__))
#endif

/* Fallthrough (prevent icc from throwing warnings) */
#if defined(_WIN32) /* clang-format off */
#    define HG_ATTR_FALLTHROUGH do {} while (0) /* fallthrough */ /* clang-format on */
#elif __has_attribute(__fallthrough__) && !defined(__INTEL_COMPILER)
#define HG_ATTR_FALLTHROUGH __attribute__((__fallthrough__))
#else /* clang-format off */
#    define HG_ATTR_FALLTHROUGH do {} while (0) /* fallthrough */
#endif /* clang-format on */

#endif /* MERCURY_COMPILER_ATTRIBUTES_H */
