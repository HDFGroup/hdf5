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
 * Macros for suppressing warnings
 */

#ifndef H5warnings_H
#define H5warnings_H

/* Macros for enabling/disabling particular GCC / clang warnings
 *
 * These are duplicated in H5FDmulti.c (we don't want to put them in the
 * public header and the multi VFD can't use private headers). If you make
 * changes here, be sure to update those as well.
 *
 * (see the following web-sites for more info:
 *      http://www.dbp-consulting.com/tutorials/SuppressingGCCWarnings.html
 *      http://gcc.gnu.org/onlinedocs/gcc/Diagnostic-Pragmas.html#Diagnostic-Pragmas
 */
#define H5_DIAG_JOINSTR(x, y) x y
#define H5_DIAG_DO_PRAGMA(x)  _Pragma(#x)
#define H5_DIAG_PRAGMA(x)     H5_DIAG_DO_PRAGMA(GCC diagnostic x)

/* Allow suppression of compiler diagnostics unless H5_SHOW_ALL_WARNINGS is
 *      defined (enabled with '--enable-show-all-warnings' configure option).
 */
#ifndef H5_SHOW_ALL_WARNINGS
#define H5_DIAG_OFF(x) H5_DIAG_PRAGMA(push) H5_DIAG_PRAGMA(ignored H5_DIAG_JOINSTR("-W", x))
#define H5_DIAG_ON(x)  H5_DIAG_PRAGMA(pop)
#else
#define H5_DIAG_OFF(x)
#define H5_DIAG_ON(x)
#endif

/* Macros for enabling/disabling particular GCC-only warnings.
 * These pragmas are only implemented usefully in gcc 4.6+
 */
#if (((__GNUC__ * 100) + __GNUC_MINOR__) >= 406)
#define H5_GCC_DIAG_OFF(x) H5_DIAG_OFF(x)
#define H5_GCC_DIAG_ON(x)  H5_DIAG_ON(x)
#else
#define H5_GCC_DIAG_OFF(x)
#define H5_GCC_DIAG_ON(x)
#endif

/* Macros for enabling/disabling particular clang-only warnings.
 */
#if defined(__clang__)
#define H5_CLANG_DIAG_OFF(x) H5_DIAG_OFF(x)
#define H5_CLANG_DIAG_ON(x)  H5_DIAG_ON(x)
#else
#define H5_CLANG_DIAG_OFF(x)
#define H5_CLANG_DIAG_ON(x)
#endif

/* Macros for enabling/disabling particular GCC / clang warnings.
 * These macros should be used for warnings supported by both gcc and clang.
 */
#if (((__GNUC__ * 100) + __GNUC_MINOR__) >= 406) || defined(__clang__)
#define H5_GCC_CLANG_DIAG_OFF(x) H5_DIAG_OFF(x)
#define H5_GCC_CLANG_DIAG_ON(x)  H5_DIAG_ON(x)
#else
#define H5_GCC_CLANG_DIAG_OFF(x)
#define H5_GCC_CLANG_DIAG_ON(x)
#endif

#endif /* H5warnings_H */
