/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*-------------------------------------------------------------------------
 *
 * Created:             H5MMprivate.h
 *                      Jul 10 1997
 *                      Robb Matzke
 *
 * Purpose:             Private header for memory management.
 *
 *-------------------------------------------------------------------------
 */
#ifndef _H5MMprivate_H
#define _H5MMprivate_H

#include "H5MMpublic.h"

/* Private headers needed by this file */
#include "H5private.h"

#define H5MM_free(Z) HDfree(Z)

/*
 * Library prototypes...
 */
H5_DLL void *H5MM_malloc(size_t size);
H5_DLL void *H5MM_calloc(size_t size);
H5_DLL void *H5MM_realloc(void *mem, size_t size);
H5_DLL char *H5MM_xstrdup(const char *s);
H5_DLL char *H5MM_strdup(const char *s);
H5_DLL void *H5MM_xfree(void *mem);

#endif /* _H5MMprivate_H */
