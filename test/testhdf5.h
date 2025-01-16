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
 * This header file contains information required for testing the HDF5 library.
 */

#ifndef TESTHDF5_H
#define TESTHDF5_H

/* Include generic testing header */
#include "h5test.h"

/* Include testing framework functionality */
#include "testframe.h"

/* Use %ld to print the value because long should cover most cases. */
/* Used to make certain a return value _is_not_ a value */
#define CHECK(ret, val, where)                                                                               \
    do {                                                                                                     \
        if (VERBOSE_HI) {                                                                                    \
            printf("   Call to routine: %15s at line %4d "                                                   \
                   "in %s returned %ld \n",                                                                  \
                   where, (int)__LINE__, __FILE__, (long)(ret));                                             \
        }                                                                                                    \
        if ((ret) == (val)) {                                                                                \
            TestErrPrintf("*** UNEXPECTED RETURN from %s is %ld at line %4d "                                \
                          "in %s\n",                                                                         \
                          where, (long)(ret), (int)__LINE__, __FILE__);                                      \
            H5Eprint2(H5E_DEFAULT, stdout);                                                                  \
        }                                                                                                    \
    } while (0)

#define CHECK_I(ret, where)                                                                                  \
    do {                                                                                                     \
        if (VERBOSE_HI) {                                                                                    \
            printf("   Call to routine: %15s at line %4d in %s returned %ld\n", (where), (int)__LINE__,      \
                   __FILE__, (long)(ret));                                                                   \
        }                                                                                                    \
        if ((ret) < 0) {                                                                                     \
            TestErrPrintf("*** UNEXPECTED RETURN from %s is %ld line %4d in %s\n", (where), (long)(ret),     \
                          (int)__LINE__, __FILE__);                                                          \
            H5Eprint2(H5E_DEFAULT, stdout);                                                                  \
        }                                                                                                    \
    } while (0)

/* Check that a pointer is valid (i.e.: not NULL) */
#define CHECK_PTR(ret, where)                                                                                \
    do {                                                                                                     \
        if (VERBOSE_HI) {                                                                                    \
            printf("   Call to routine: %15s at line %4d in %s returned %p\n", (where), (int)__LINE__,       \
                   __FILE__, ((const void *)ret));                                                           \
        }                                                                                                    \
        if (!(ret)) {                                                                                        \
            TestErrPrintf("*** UNEXPECTED RETURN from %s is NULL line %4d in %s\n", (where), (int)__LINE__,  \
                          __FILE__);                                                                         \
            H5Eprint2(H5E_DEFAULT, stdout);                                                                  \
        }                                                                                                    \
    } while (0)

/* Check that a pointer is NULL */
#define CHECK_PTR_NULL(ret, where)                                                                           \
    do {                                                                                                     \
        if (VERBOSE_HI) {                                                                                    \
            printf("   Call to routine: %15s at line %4d in %s returned %p\n", (where), (int)__LINE__,       \
                   __FILE__, ((const void *)ret));                                                           \
        }                                                                                                    \
        if (ret) {                                                                                           \
            TestErrPrintf("*** UNEXPECTED RETURN from %s is not NULL line %4d in %s\n", (where),             \
                          (int)__LINE__, __FILE__);                                                          \
            H5Eprint2(H5E_DEFAULT, stdout);                                                                  \
        }                                                                                                    \
    } while (0)

/* Check that two pointers are equal */
#define CHECK_PTR_EQ(ret, val, where)                                                                        \
    do {                                                                                                     \
        if (VERBOSE_HI) {                                                                                    \
            printf("   Call to routine: %15s at line %4d in %s returned %p\n", (where), (int)__LINE__,       \
                   __FILE__, (const void *)(ret));                                                           \
        }                                                                                                    \
        if (ret != val) {                                                                                    \
            TestErrPrintf(                                                                                   \
                "*** UNEXPECTED RETURN from %s: returned value of %p is not equal to %p line %4d in %s\n",   \
                (where), (const void *)(ret), (const void *)(val), (int)__LINE__, __FILE__);                 \
            H5Eprint2(H5E_DEFAULT, stdout);                                                                  \
        }                                                                                                    \
    } while (0)

/* Used to make certain a return value _is_ a value */
#define VERIFY(_x, _val, where)                                                                              \
    do {                                                                                                     \
        long __x = (long)_x, __val = (long)_val;                                                             \
        if (VERBOSE_HI) {                                                                                    \
            printf("   Call to routine: %15s at line %4d in %s had value "                                   \
                   "%ld \n",                                                                                 \
                   (where), (int)__LINE__, __FILE__, __x);                                                   \
        }                                                                                                    \
        if ((__x) != (__val)) {                                                                              \
            TestErrPrintf("*** UNEXPECTED VALUE from %s should be %ld, but is %ld at line %4d "              \
                          "in %s\n",                                                                         \
                          (where), __val, __x, (int)__LINE__, __FILE__);                                     \
            H5Eprint2(H5E_DEFAULT, stdout);                                                                  \
        }                                                                                                    \
    } while (0)

/* Used to make certain a (non-'long' type's) return value _is_ a value */
#define VERIFY_TYPE(_x, _val, _type, _format, where)                                                         \
    do {                                                                                                     \
        _type __x = (_type)_x, __val = (_type)_val;                                                          \
        if (VERBOSE_HI) {                                                                                    \
            printf("   Call to routine: %15s at line %4d in %s had value " _format " \n", (where),           \
                   (int)__LINE__, __FILE__, __x);                                                            \
        }                                                                                                    \
        if ((__x) != (__val)) {                                                                              \
            TestErrPrintf("*** UNEXPECTED VALUE from %s should be " _format ", but is " _format              \
                          " at line %4d "                                                                    \
                          "in %s\n",                                                                         \
                          (where), __val, __x, (int)__LINE__, __FILE__);                                     \
            H5Eprint2(H5E_DEFAULT, stdout);                                                                  \
        }                                                                                                    \
    } while (0)

/* Used to make certain a string return value _is_ a value */
#define VERIFY_STR(x, val, where)                                                                            \
    do {                                                                                                     \
        if (VERBOSE_HI) {                                                                                    \
            printf("   Call to routine: %15s at line %4d in %s had value "                                   \
                   "%s \n",                                                                                  \
                   (where), (int)__LINE__, __FILE__, x);                                                     \
        }                                                                                                    \
        if (strcmp(x, val) != 0) {                                                                           \
            TestErrPrintf("*** UNEXPECTED VALUE from %s should be %s, but is %s at line %4d "                \
                          "in %s\n",                                                                         \
                          where, val, x, (int)__LINE__, __FILE__);                                           \
            H5Eprint2(H5E_DEFAULT, stdout);                                                                  \
        }                                                                                                    \
    } while (0)

/* Used to document process through a test and to check for errors */
#define RESULT(ret, func)                                                                                    \
    do {                                                                                                     \
        if (VERBOSE_MED) {                                                                                   \
            printf("   Call to routine: %15s at line %4d in %s returned "                                    \
                   "%ld\n",                                                                                  \
                   func, (int)__LINE__, __FILE__, (long)(ret));                                              \
        }                                                                                                    \
        if (VERBOSE_HI)                                                                                      \
            H5Eprint2(H5E_DEFAULT, stdout);                                                                  \
        if ((ret) == FAIL) {                                                                                 \
            TestErrPrintf("*** UNEXPECTED RETURN from %s is %ld at line %4d "                                \
                          "in %s\n",                                                                         \
                          func, (long)(ret), (int)__LINE__, __FILE__);                                       \
            H5Eprint2(H5E_DEFAULT, stdout);                                                                  \
        }                                                                                                    \
    } while (0)

/* Used to indicate an error that is complex to check for */
#define ERROR(where)                                                                                         \
    do {                                                                                                     \
        if (VERBOSE_HI)                                                                                      \
            printf("   Call to routine: %15s at line %4d in %s returned "                                    \
                   "invalid result\n",                                                                       \
                   where, (int)__LINE__, __FILE__);                                                          \
        TestErrPrintf("*** UNEXPECTED RESULT from %s at line %4d in %s\n", where, (int)__LINE__, __FILE__);  \
    } while (0)

#ifdef __cplusplus
extern "C" {
#endif

/* Prototypes for the test routines */
void test_metadata(void *params);
void test_checksum(void *params);
void test_refstr(void *params);
void test_file(void *params);
void test_h5o(void *params);
void test_h5t(void *params);
void test_h5s(void *params);
void test_coords(void *params);
void test_h5d(void *params);
void test_attr(void *params);
void test_select(void *params);
void test_time(void *params);
void test_reference(void *params);
void test_reference_deprec(void *params);
void test_vltypes(void *params);
void test_vlstrings(void *params);
void test_iterate(void *params);
void test_array(void *params);
void test_genprop(void *params);
void test_configure(void *params);
void test_h5_system(void *params);
void test_misc(void *params);
void test_ids(void *params);
void test_skiplist(void *params);
void test_sohm(void *params);
void test_unicode(void *params);

/* Prototypes for the cleanup routines */
void cleanup_metadata(void *params);
void cleanup_checksum(void *params);
void cleanup_file(void *params);
void cleanup_h5o(void *params);
void cleanup_h5s(void *params);
void cleanup_coords(void *params);
void cleanup_attr(void *params);
void cleanup_select(void *params);
void cleanup_time(void *params);
void cleanup_reference(void *params);
void cleanup_reference_deprec(void *params);
void cleanup_vltypes(void *params);
void cleanup_vlstrings(void *params);
void cleanup_iterate(void *params);
void cleanup_array(void *params);
void cleanup_genprop(void *params);
void cleanup_configure(void *params);
void cleanup_h5_system(void *params);
void cleanup_sohm(void *params);
void cleanup_misc(void *params);
void cleanup_unicode(void *params);

#ifdef __cplusplus
}
#endif
#endif /* TESTHDF5_H */
