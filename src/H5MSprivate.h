/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*-------------------------------------------------------------------------
 *
 * Created:		H5MSprivate.h
 *			Sept 18 2020
 *			Quincey Koziol
 *
 * Purpose:		Private header for managed string routines
 *
 *-------------------------------------------------------------------------
 */

#ifndef _H5MSprivate_H
#define _H5MSprivate_H

/* Include package's public header */

/* Private headers needed by this file */

/**************************/
/* Library Private Macros */
/**************************/

/****************************/
/* Library Private Typedefs */
/****************************/

/* Typedef for managed string objects */
typedef struct H5MS_t {
    size_t len; /* Current length of the string */
    size_t max; /* Size of allocated buffer */
    char * s;   /* Pointer to buffer / beginning of string */
    char * end; /* Pointer to terminating NUL character at the end of the string */
} H5MS_t;

/*****************************/
/* Library-private Variables */
/*****************************/

/***************************************/
/* Library-private Function Prototypes */
/***************************************/
herr_t H5MS_asprintf_cat(H5MS_t *ms, const char *fmt, ...);
herr_t H5MS_acat(H5MS_t *ms, const char *s);
herr_t H5MS_aputc(H5MS_t *ms, int c);

#endif /* _H5MSprivate_H */
