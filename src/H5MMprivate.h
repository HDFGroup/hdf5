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
#ifndef H5MMprivate_H
#define H5MMprivate_H

#include "H5MMpublic.h"

/* Private headers needed by this file */
#include "H5private.h"

#if defined H5_MEMORY_ALLOC_SANITY_CHECK

/*
 * Define this to have the library print out memory
 * statistics during its final memory sanity checking
 */
/*#define H5MM_PRINT_MEMORY_STATS*/

#define H5MM_free(ptr) H5MM_xfree(ptr)

/*
 * Library prototypes...
 */
H5_DLL void *H5MM_malloc(size_t size) H5_ATTR_MALLOC;
H5_DLL void *H5MM_calloc(size_t size) H5_ATTR_MALLOC;
H5_DLL void *H5MM_realloc(void *mem, size_t size);
H5_DLL char *H5MM_strdup(const char *s);
H5_DLL void *H5MM_memcpy(void *dest, const void *src, size_t n);
H5_DLL void  H5MM_sanity_check_all(void);
H5_DLL void  H5MM_final_sanity_check(void);

#else

#define H5MM_malloc(size)        HDmalloc(size)
#define H5MM_calloc(size)        HDcalloc(1, size)
#define H5MM_realloc(ptr, size)  HDrealloc(ptr, size)
#define H5MM_free(ptr)           HDfree(ptr)
#define H5MM_strdup(s)           HDstrdup(s)
#define H5MM_memcpy(dst, src, n) HDmemcpy(dst, src, n)

#endif

/*
 * Library prototypes...
 */
H5_DLL void * H5MM_xfree(void *mem);
H5_DLL void * H5MM_xfree_const(const void *mem);
H5_DLL char * H5MM_xstrdup(const char *s);
H5_DLL herr_t H5MM_get_alloc_stats(H5_alloc_stats_t *stats);

#endif /* H5MMprivate_H */
