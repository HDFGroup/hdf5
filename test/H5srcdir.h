/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Programmer:  Quincey Koziol <koziol@hdfgroup.org>
 *              Wednesday, March 17, 2010
 *
 * Purpose:     srcdir querying support.
 */
#ifndef _H5SRCDIR_H
#define _H5SRCDIR_H

/* Include the header file with the correct relative path for the srcdir string */
#include "H5srcdir_str.h"

/* Buffer to construct path in and return pointer to */
static char srcdir_path[1024] = "";

/* Buffer to construct file in and return pointer to */
static char srcdir_testpath[1024] = "";

/* Just return the srcdir path */
static const char *
H5_get_srcdir(void)
{
    const char *srcdir = HDgetenv("srcdir");

    /* Check for using the srcdir from configure time */
    if (NULL == srcdir)
        srcdir = config_srcdir;

    /* Build path to all test files */
    if ((HDstrlen(srcdir) + 2) < sizeof(srcdir_path)) {
        HDsnprintf(srcdir_path, sizeof(srcdir_path), "%s/", srcdir);
        return (srcdir_path);
    } /* end if */
    else
        return (NULL);
} /* end H5_get_srcdir() */

/* Append the test file name to the srcdir path and return the whole string */
static const char *
H5_get_srcdir_filename(const char *filename)
{
    const char *srcdir = H5_get_srcdir();

    /* Check for error */
    if (NULL == srcdir)
        return (NULL);
    else {
        /* Build path to test file */
        if ((HDstrlen(srcdir) + HDstrlen(filename) + 1) < sizeof(srcdir_testpath)) {
            HDsnprintf(srcdir_testpath, sizeof(srcdir_testpath), "%s%s", srcdir, filename);
            return (srcdir_testpath);
        } /* end if */
        else
            return (NULL);
    } /* end else */
} /* end H5_get_srcdir_filename() */
#endif /* _H5SRCDIR_H */
