/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by Akadio, Inc.                                                 *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#include "nbcompat.h"

size_t
strlcpy(char *dst, const char *src, size_t size)
{
    char *      d;
    const char *s;

    for (d = dst, s = src; (s - src) < size; d++, s++) {
        *d = *s;
        if (*s == '\0')
            return s - src;
    }

    dst[size - 1] = '\0';
    return size;
}
