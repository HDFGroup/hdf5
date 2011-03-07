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

#ifndef _H5HLprivate2_H
#define _H5HLprivate2_H

/* Public HDF5 header */
#include "hdf5.h"

/* Public High-Level header */
#include "hdf5_hl.h"

/* The following is copied from src/H5private.h */

/*
 * Status return values for the `herr_t' type.
 * Since some unix/c routines use 0 and -1 (or more precisely, non-negative
 * vs. negative) as their return code, and some assumption had been made in
 * the code about that, it is important to keep these constants the same
 * values.  When checking the success or failure of an integer-valued
 * function, remember to compare against zero and not one of these two
 * values.
 */
#define SUCCEED		0
#define FAIL		(-1)
#define UFAIL		(unsigned)(-1)

/* minimum of two, three, or four values */
#undef MIN
#define MIN(a,b)		(((a)<(b)) ? (a) : (b))
#define MIN2(a,b)		MIN(a,b)
#define MIN3(a,b,c)		MIN(a,MIN(b,c))
#define MIN4(a,b,c,d)		MIN(MIN(a,b),MIN(c,d))

/* maximum of two, three, or four values */
#undef MAX
#define MAX(a,b)		(((a)>(b)) ? (a) : (b))
#define MAX2(a,b)		MAX(a,b)
#define MAX3(a,b,c)		MAX(a,MAX(b,c))
#define MAX4(a,b,c,d)		MAX(MAX(a,b),MAX(c,d))

/*
 * HDF Boolean type.
 */
#ifndef FALSE
#   define FALSE 0
#endif
#ifndef TRUE
#   define TRUE 1
#endif
#ifndef HDcalloc
    #define HDcalloc(N,Z)               calloc(N,Z)
#endif /* HDcalloc */
#ifndef HDrealloc
    #define HDrealloc(M,Z)              realloc(M,Z)
#endif /* HDrealloc */
#ifndef HDfree
    #define HDfree(M)           free(M)
#endif /* HDfree */
#ifndef HDmemcpy
    #define HDmemcpy(X,Y,Z)             memcpy((char*)(X),(const char*)(Y),Z)
#endif /* HDmemcpy */
#ifndef HDmemset
    #define HDmemset(X,C,Z)             memset(X,C,Z)
#endif /* HDmemset */
#ifndef HDassert
    #define HDassert(X)         assert(X)
#endif /* HDassert */
#ifndef HDstrlen
    #define HDstrlen(S)         strlen(S)
#endif /* HDstrlen */
#ifndef HDstrcat
    #define HDstrcat(X,Y)               strcat(X,Y)
#endif /* HDstrcat */
#ifndef HDstrrchr
    #define HDstrrchr(S,C)              strrchr(S,C)
#endif /* HDstrrchr */
#ifndef HDstrtol
    #define HDstrtol(S,R,N)             strtol(S,R,N)
#endif /* HDstrtol */
#ifndef HDstrtod
    #define HDstrtod(S,R)               strtod(S,R)
#endif /* HDstrtod */
#ifndef HDsleep
    #define HDsleep(N)          sleep(N)
#endif /* HDsleep */
#ifndef HDfflush
    #define HDfflush(F)         fflush(F)
#endif /* HDfflush */
#ifndef HDstrcmp
    #define HDstrcmp(X,Y)       strcmp(X,Y)
#endif /* HDstrcmp */
/*
 * And now for a couple non-Posix functions...  Watch out for systems that
 * define these in terms of macros.
 */
#if !defined strdup && !defined H5_HAVE_STRDUP
extern char *strdup(const char *s);
#endif

#ifndef HDstrdup
    #define HDstrdup(S)     strdup(S)
#endif /* HDstrdup */

#endif /* _H5HLprivate2_H */

