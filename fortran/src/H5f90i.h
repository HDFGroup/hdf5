/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have     *
 * access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu. *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */


#ifndef _H5f90i_H
#define _H5f90i_H

/* 
 * Standard header files needed all the time 
 */

#include "H5private.h"

/*
 * Include generated header.  This header defines integer types,
 * so this file only needs to define _fcd and real_f.
 */
#include "H5f90i_gen.h"

#if (defined (UNICOS) || (defined (_UNICOS)))

#include <fortran.h>

/*typedef char*              _fcd;*/
typedef double             real_f;

#endif /* UNICOS */

#if defined(IBM6000) || defined(_AIX)

typedef char              *_fcd;
typedef float             real_f;
#define _fcdtocp(desc) (desc)
#endif /*IBM6000*/

/* MAC APPLE definitions with IBM XL compiler*/
#if defined(__APPLE__)
typedef char              *_fcd;
typedef float             real_f;
#define _fcdtocp(desc) (desc)

#endif /*APPLE*/

/* LINUX definitions */
#if (defined(linux) || defined(__gnu_linux__) || defined(__linux__))

/* Common definitions */
typedef char              *_fcd;
typedef float             real_f;
#define _fcdtocp(desc) (desc)

/* IA32 specific definitions */
#if (defined(i386) || defined(__i386) || defined(__i386__))

/* AMD64 specific definitions */
#elif defined __x86_64__

/* IA64 specific definitions */
#elif defined __ia64

#endif /* IA64 */
#endif /* LINUX*/

#if defined(IRIX) || defined(IRIS4) || defined(sgi) || defined(__sgi__) || defined(__sgi)

typedef char          *_fcd;
typedef float         real_f;
#define _fcdtocp(desc) (desc)
#endif /* IRIX */

#if (defined(SUN) || defined(sun) || defined(__sun__) || defined(__SUNPRO_C)) & !defined(__i386)

typedef char              *_fcd;
typedef float             real_f;
#define _fcdtocp(desc) (desc)

#endif     /*SUN*/

#if defined DEC_ALPHA || (defined __alpha && defined __unix__ && !defined __linux__)

typedef char             *_fcd;
typedef float            real_f;
#define _fcdtocp(desc) (desc)

#endif /* DEC_ALPHA */

#if defined __alpha__ && defined __linux__

typedef char             *_fcd;
typedef float            real_f;
#define _fcdtocp(desc) (desc)

#endif /* DEC_ALPHA_LINUX */

#if defined(HP9000) || (!defined(__convexc__) && (defined(hpux) || defined(__hpux)))

typedef char           *_fcd;
typedef float          real_f;
#define _fcdtocp(desc) (desc)

#endif /* HP9000 */

#if defined _WINDOWS || defined WIN32

typedef char              *_fcd;
typedef float             real_f;

#define _fcdtocp(desc) (desc)

#endif /*WINDOWS */

/* FreeBSD definitions */
#if (defined(__FreeBSD) || defined(__FreeBSD__))

/* Common definitions */
typedef char              *_fcd;
typedef float             real_f;
#define _fcdtocp(desc) (desc)

#endif /* FreeBSD */

#endif /* _H5f90i_H */
