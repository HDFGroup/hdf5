/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#ifndef H5PL_PACKAGE
#error "Do not include this file outside the H5PL package!"
#endif

#ifndef _H5PLpkg_H
#define _H5PLpkg_H

/* Include private header file */
#include "H5PLprivate.h"       

#define   MAX_PATH_NUM              16

/****************************/
/* Macros for supporting 
 * both Windows and Unix */
/****************************/

/* Handle for dynamic library */
#ifdef H5_HAVE_WIN32_API
/* Handle for dynamic library */
#define H5PL_HANDLE HINSTANCE

/* Get a handle to a plugin library.  Windows: TEXT macro handles Unicode strings */
#define H5PL_OPEN_DLIB(S) LoadLibraryEx(TEXT(S), NULL, LOAD_WITH_ALTERED_SEARCH_PATH)

/* Get the address of a symbol in dynamic library */
#define H5PL_GET_LIB_FUNC(H,N) GetProcAddress(H,N)

/* Close dynamic library */
#define H5PL_CLOSE_LIB(H) FreeLibrary(H)

/* Declare error string */
#define H5PL_ERROR DWORD dw; char *error

/* Clear error - nothing to do */
#define H5PLG_CLR_ERROR

/* Print error message */
#define H5PL_CHECK_ERR(R) { \
    if(R==NULL) { \
        dw = GetLastError(); \
        sprintf(error, "%ld", dw); \
    } \
    else error = NULL; \
}

#else
/* Handle for dynamic library */
#define H5PL_HANDLE void *

/* Get a handle to a plugin library.  Windows: TEXT macro handles Unicode strings */
#define H5PL_OPEN_DLIB(S) dlopen(S, RTLD_NOW|RTLD_LAZY)

/* Get the address of a symbol in dynamic library */
#define H5PL_GET_LIB_FUNC(H,N) dlsym(H,N)

/* Close dynamic library */
#define H5PL_CLOSE_LIB(H) dlclose(H)

/* Declare error string */
#define H5PL_ERROR char *error

/* Clear error */
#define H5PL_CLR_ERR dlerror()

/* Print error message */
#define H5PL_CHECK_ERR(R) { \
    if(R==NULL) { \
        error = dlerror(); \
    } \
    else error = NULL; \
}

#endif

/****************************/
/* Local typedefs */
/****************************/

/* Type for the list of info for opened plugin libraries */
typedef struct H5PL_table_t {
    H5PL_type_t pl_type;			/* plugin type	     */
    int         pl_id;                          /* ID for the plugin */
    H5PL_HANDLE handle;			/* plugin handle     */
} H5PL_table_t;

/****************************/
/* Local variables */
/****************************/

/* Table for opened plugin libraries */
static size_t		H5PL_table_alloc_g = 0;
static size_t		H5PL_table_used_g = 0;
static H5PL_table_t     *H5PL_table_g = NULL;

/* Table of location paths for plugin libraries */
static char             *path_table[MAX_PATH_NUM];
static size_t           num_paths = 0;
static htri_t           path_found = FALSE;

/******************************/
/* Package Private Prototypes */
/******************************/

/* Function prototypes for H5PL package scope */
H5_DLL htri_t H5PL_find(H5PL_type_t plugin_type, int type_id, char *dir, void **info);
H5_DLL htri_t H5PL_open(H5PL_type_t pl_type, char *libname, int plugin_id, void **pl_info);
H5_DLL htri_t H5PL_search_table(H5PL_type_t plugin_type, int type_id, void **info);
H5_DLL herr_t H5PL_close(H5PL_HANDLE handle);

#endif /* _H5PLpkg_H */

