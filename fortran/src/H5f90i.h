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


#ifndef _H5f90i_H
#define _H5f90i_H

/*
 * Standard header files needed all the time
 */

#include "H5private.h"

#if (defined (UNICOS) || defined (_UNICOS)) && !defined(__crayx1)

#include <fortran.h>

/*typedef char*              _fcd;*/
typedef long               haddr_t_f;
typedef long               hsize_t_f;
typedef long               hssize_t_f;
typedef long               size_t_f;
typedef long               int_f;
typedef long               hid_t_f;
typedef double             real_f;
#define DF_CAPFNAMES
/*#define _fcdtocp(desc) (desc)*/

#endif /* UNICOS */

#if defined(IBM6000) || defined(_AIX)

typedef char              *_fcd;
typedef long long         haddr_t_f;
typedef long long         hsize_t_f;
typedef long long         hssize_t_f;
typedef int               size_t_f;
typedef int               int_f;
typedef int               hid_t_f;
typedef float             real_f;
#define _fcdtocp(desc) (desc)
#endif /*IBM6000*/

/* MAC APPLE definitions with IBM XL compiler*/
#if defined(__APPLE__)
typedef char              *_fcd;
typedef long long         haddr_t_f;
typedef long long         hsize_t_f;
typedef long long         hssize_t_f;
typedef int               size_t_f;
typedef int               int_f;
typedef int               hid_t_f;
typedef float             real_f;
#if defined H5_G95
#define FNAME_POST2_UNDERSCORE
#endif
#if defined H5_ABSOFT
#define DF_CAPFNAMES
#endif /*H5_ABSOFT*/
#define _fcdtocp(desc) (desc)

#endif /*APPLE*/

/* LINUX definitions */
#if (defined(linux) || defined(__gnu_linux__) || defined(__linux__))

/* Common definitions */
typedef char              *_fcd;
typedef int               int_f;
typedef int               hid_t_f;
typedef float             real_f;
#define _fcdtocp(desc) (desc)

/* IA32 specific definitions */
#if (defined(i386) || defined(__i386) || defined(__i386__))

typedef long long         haddr_t_f;
typedef long long         hsize_t_f;
typedef long long         hssize_t_f;
typedef int               size_t_f;
#if defined H5_ABSOFT
#define DF_CAPFNAMES
#elif defined H5_G95
#define FNAME_POST2_UNDERSCORE
#else
#define FNAME_POST_UNDERSCORE
#endif /*H5_ABSOFT*/

/* AMD64 specific definitions */
#elif defined __x86_64__

typedef long long         haddr_t_f;
typedef long long         hsize_t_f;
typedef long long         hssize_t_f;
typedef int               size_t_f;
#define FNAME_POST_UNDERSCORE

/* IA64 specific definitions */
#elif defined __ia64

typedef long              haddr_t_f;
typedef long              hsize_t_f;
typedef long              hssize_t_f;
typedef long              size_t_f;
#define FNAME_POST_UNDERSCORE

#endif /* IA64 */
#endif /* LINUX*/

#if defined(IRIX) || defined(IRIS4) || defined(sgi) || defined(__sgi__) || defined(__sgi)

typedef char          *_fcd;
typedef long          haddr_t_f;
typedef long          hsize_t_f;
typedef long          hssize_t_f;
typedef long          size_t_f;
typedef int           int_f;
typedef int           hid_t_f;
typedef float         real_f;
#define FNAME_POST_UNDERSCORE
#define _fcdtocp(desc) (desc)
#endif /* IRIX */

#if defined(__crayx1)

typedef char          *_fcd;
typedef long          haddr_t_f;
typedef long          hsize_t_f;
typedef long          hssize_t_f;
typedef long          size_t_f;
typedef int           int_f;
typedef int           hid_t_f;
typedef float         real_f;
#define FNAME_POST_UNDERSCORE
#define _fcdtocp(desc) (desc)
#endif /* Cray X1 */

#if (defined(SUN) || defined(sun) || defined(__sun__) || defined(__SUNPRO_C)) & !defined(__i386)

typedef char              *_fcd;
typedef long long         haddr_t_f;
typedef long long         hssize_t_f;
typedef long long         hsize_t_f;
typedef int               size_t_f;
typedef int               int_f;
typedef int               hid_t_f;
typedef float             real_f;
#define FNAME_POST_UNDERSCORE
#define _fcdtocp(desc) (desc)

#endif     /*SUN*/

#if defined DEC_ALPHA || (defined __alpha && defined __unix__ && !defined __linux__)

typedef char             *_fcd;
typedef long             haddr_t_f;
typedef long             hsize_t_f;
typedef long             hssize_t_f;
typedef long             size_t_f;
typedef int              int_f;
typedef int              hid_t_f;
typedef float            real_f;
#define FNAME_POST_UNDERSCORE
#define _fcdtocp(desc) (desc)

#endif /* DEC_ALPHA */

#if defined __alpha__ && defined __linux__

typedef char             *_fcd;
typedef long long        haddr_t_f;
typedef long long        hsize_t_f;
typedef long long        hssize_t_f;
typedef long long        size_t_f;
typedef int              int_f;
typedef int              hid_t_f;
typedef float            real_f;
#define FNAME_POST2_UNDERSCORE
#define _fcdtocp(desc) (desc)

#endif /* DEC_ALPHA_LINUX */

#if defined(HP9000) || (!defined(__convexc__) && (defined(hpux) || defined(__hpux)))

typedef char           *_fcd;
typedef long long      haddr_t_f;
typedef long long      hsize_t_f;
typedef long long      hssize_t_f;
typedef long           size_t_f;
typedef int            int_f;
typedef int            hid_t_f;
typedef float          real_f;
#define _fcdtocp(desc) (desc)

#if defined __ia64
#define FNAME_POST_UNDERSCORE
#endif
#endif /* HP9000 */


#if defined _WINDOWS || defined WIN32

typedef char              *_fcd;
typedef long double       haddr_t_f;
typedef int               hsize_t_f;
typedef int               hssize_t_f;
typedef int               size_t_f;
typedef int               int_f;
typedef int               hid_t_f;
typedef float             real_f;

#define DF_CAPFNAMES
#define _fcdtocp(desc) (desc)

#endif /*WINDOWS */

/*----------------------------------------------------------------
** MACRO FNAME for any fortran callable routine name.
**
**  This macro prepends, appends, or does not modify a name
**  passed as a macro parameter to it based on the FNAME_PRE_UNDERSCORE,
**  FNAME_POST_UNDERSCORE macros set for a specific system.
**
**---------------------------------------------------------------*/
#if defined(FNAME_PRE_UNDERSCORE) && defined(FNAME_POST_UNDERSCORE)
#   define FNAME(x)     _##x##_
#endif
#if defined(FNAME_PRE_UNDERSCORE) && !defined(FNAME_POST_UNDERSCORE)
#   define FNAME(x)     _##x
#endif
#if !defined(FNAME_PRE_UNDERSCORE) && defined(FNAME_POST_UNDERSCORE)
#   define FNAME(x)     x##_
#endif
#if !defined(FNAME_PRE_UNDERSCORE) && !defined(FNAME_POST_UNDERSCORE) && !defined(FNAME_POST2_UNDERSCORE)
#   define FNAME(x)     x
#endif
#if !defined(FNAME_PRE_UNDERSCORE) && defined(FNAME_POST2_UNDERSCORE)
#   define FNAME(x)     x##__
#endif

#endif /* _H5f90i_H */
