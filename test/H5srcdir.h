/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Purpose:     srcdir querying support.
 */
#ifndef H5SRCDIR_H
#define H5SRCDIR_H

#ifdef __cplusplus
extern "C" {
#endif
/* Just return the srcdir path */
H5TEST_DLL const char *H5_get_srcdir(void);

/* Append the test file name to the srcdir path and return the whole string */
H5TEST_DLL const char *H5_get_srcdir_filename(const char *filename);

#ifdef __cplusplus
}
#endif

#endif /* H5SRCDIR_H */
