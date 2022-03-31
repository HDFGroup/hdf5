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

/* Programmer:  Robb Matzke
 *    Friday, October 30, 1998
 *
 * Purpose:  This file is included by all HDF5 library source files to
 *    define common things which are not defined in the HDF5 API.
 *    The configuration constants like H5_HAVE_UNISTD_H etc. are
 *    defined in H5config.h which is included by H5public.h.
 *
 */

#ifndef H5private_H
#define H5private_H

#include "H5public.h" /* Include Public Definitions    */

/* include the pthread header */
#ifdef H5_HAVE_THREADSAFE
#ifdef H5_HAVE_WIN32_API
#ifndef H5_HAVE_WIN_THREADS
#ifdef H5_HAVE_PTHREAD_H
#include <pthread.h>
#endif /* H5_HAVE_PTHREAD_H */
#endif /* H5_HAVE_WIN_THREADS */
#else  /* H5_HAVE_WIN32_API */
#ifdef H5_HAVE_PTHREAD_H
#include <pthread.h>
#endif /* H5_HAVE_PTHREAD_H */
#endif /* H5_HAVE_WIN32_API */
#endif /* H5_HAVE_THREADSAFE */

/*
 * Include ANSI-C header files.
 */
#ifdef H5_STDC_HEADERS
#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <float.h>
#include <limits.h>
#include <math.h>
#include <signal.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#endif

/*
 * If _POSIX_VERSION is defined in unistd.h then this system is Posix.1
 * compliant. Otherwise all bets are off.
 */
#ifdef H5_HAVE_UNISTD_H
#include <sys/types.h>
#include <unistd.h>
#endif
#ifdef _POSIX_VERSION
#include <sys/wait.h>
#include <pwd.h>
#endif

/*
 * C9x integer types
 */
#ifndef __cplusplus
#ifdef H5_HAVE_STDINT_H
#include <stdint.h>
#endif
#endif

/*
 * The `struct stat' data type for stat() and fstat(). This is a POSIX file
 * but often apears on non-POSIX systems also.  The `struct stat' is required
 * for HDF5 to compile, although only a few fields are actually used.
 */
#ifdef H5_HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

/*
 * If a program may include both `time.h' and `sys/time.h' then
 * TIME_WITH_SYS_TIME is defined (see AC_HEADER_TIME in configure.ac).
 * On some older systems, `sys/time.h' includes `time.h' but `time.h' is not
 * protected against multiple inclusion, so programs should not explicitly
 * include both files. This macro is useful in programs that use, for example,
 * `struct timeval' or `struct timezone' as well as `struct tm'.  It is best
 * used in conjunction with `HAVE_SYS_TIME_H', whose existence is checked
 * by `AC_CHECK_HEADERS(sys/time.h)' in configure.ac.
 */
#if defined(H5_TIME_WITH_SYS_TIME)
#include <sys/time.h>
#include <time.h>
#elif defined(H5_HAVE_SYS_TIME_H)
#include <sys/time.h>
#else
#include <time.h>
#endif

/*
 * Longjumps are used to detect alignment constrants
 */
#ifdef H5_HAVE_SETJMP_H
#include <setjmp.h>
#endif

/*
 * Resource usage is not Posix.1 but HDF5 uses it anyway for some performance
 * and debugging code if available.
 */
#ifdef H5_HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#endif

/*
 * Unix ioctls.   These are used by h5ls (and perhaps others) to determine a
 * reasonable output width.
 */
#ifdef H5_HAVE_SYS_IOCTL_H
#include <sys/ioctl.h>
#endif

/*
 * System information. These are needed on the DEC Alpha to turn off fixing
 * of unaligned accesses by the operating system during detection of
 * alignment constraints in H5detect.c:main().
 */
#ifdef H5_HAVE_SYS_SYSINFO_H
#include <sys/sysinfo.h>
#endif
#ifdef H5_HAVE_SYS_PROC_H
#include <sys/proc.h>
#endif
#ifdef H5_HAVE_IO_H
#include <io.h>
#endif

/*
 * Dynamic library handling.  These are needed for dynamically loading I/O
 * filters and VFDs.
 */
#ifdef H5_HAVE_DLFCN_H
#include <dlfcn.h>
#endif
#ifdef H5_HAVE_DIRENT_H
#include <dirent.h>
#endif

/* Define the default VFD for this platform.
 * Since the removal of the Windows VFD, this is sec2 for all platforms.
 */
#define H5_DEFAULT_VFD H5FD_SEC2

#ifdef H5_HAVE_WIN32_API

/* The following two defines must be before any windows headers are included */
#define WIN32_LEAN_AND_MEAN /* Exclude rarely-used stuff from Windows headers */
#define NOGDI               /* Exclude Graphic Display Interface macros */

#ifdef H5_HAVE_WINSOCK2_H
#include <winsock2.h>
#endif

#ifdef H5_HAVE_THREADSAFE
#include <process.h> /* For _beginthread() */
#endif

#include <windows.h>
#include <direct.h> /* For _getcwd() */

#endif /*H5_HAVE_WIN32_API*/

/* Various ways that inline functions can be declared */
#if defined(H5_HAVE___INLINE__)
/* GNU (alternative form) */
#define H5_INLINE __inline__
#elif defined(H5_HAVE___INLINE)
/* Visual Studio */
#define H5_INLINE __inline
#elif defined(H5_HAVE_INLINE)
/* GNU, C++
 * Use "inline" as a last resort on the off-chance that there will
 * be C++ problems.
 */
#define H5_INLINE inline
#else
#define H5_INLINE
#endif /* inline choices */

#ifndef F_OK
#define F_OK 00
#define W_OK 02
#define R_OK 04
#endif

/*
 * MPE Instrumentation support
 */
#ifdef H5_HAVE_MPE
/*------------------------------------------------------------------------
 * Purpose:    Begin to collect MPE log information for a function. It should
 *             be ahead of the actual function's process.
 *
 * Programmer: Long Wang
 *
 *------------------------------------------------------------------------
 */
#include "mpe.h"
/*
 * #define eventa(func_name)   h5_mpe_ ## func_name ## _a
 * #define eventb(func_name)   h5_mpe_ ## func_name ## _b
 */
#define eventa(func_name) h5_mpe_eventa
#define eventb(func_name) h5_mpe_eventb
#define MPE_LOG_VARS                                                                                         \
    static int eventa(FUNC) = -1;                                                                            \
    static int eventb(FUNC) = -1;                                                                            \
    char       p_event_start[128];

/* Hardwire the color to "red", since that's what all the routines are using
 * now.  In the future, if we want to change that color for a given routine,
 * we should define a "FUNC_ENTER_API_COLOR" macro which takes an extra 'color'
 * parameter and then make additional FUNC_ENTER_<foo>_COLOR macros to get that
 * color information down to the BEGIN_MPE_LOG macro (which should have a new
 * BEGIN_MPE_LOG_COLOR variant). -QAK
 */
#define BEGIN_MPE_LOG                                                                                        \
    if (H5_MPEinit_g) {                                                                                      \
        sprintf(p_event_start, "start %s", FUNC);                                                            \
        if (eventa(FUNC) == -1 && eventb(FUNC) == -1) {                                                      \
            const char *p_color = "red";                                                                     \
            eventa(FUNC)        = MPE_Log_get_event_number();                                                \
            eventb(FUNC)        = MPE_Log_get_event_number();                                                \
            MPE_Describe_state(eventa(FUNC), eventb(FUNC), FUNC, p_color);                                   \
        }                                                                                                    \
        MPE_Log_event(eventa(FUNC), 0, p_event_start);                                                       \
    }

/*------------------------------------------------------------------------
 * Purpose:   Finish the collection of MPE log information for a function.
 *            It should be after the actual function's process.
 *
 * Programmer: Long Wang
 */
#define FINISH_MPE_LOG                                                                                       \
    if (H5_MPEinit_g) {                                                                                      \
        MPE_Log_event(eventb(FUNC), 0, FUNC);                                                                \
    }

#else                  /* H5_HAVE_MPE */
#define MPE_LOG_VARS   /* void */
#define BEGIN_MPE_LOG  /* void */
#define FINISH_MPE_LOG /* void */

#endif /* H5_HAVE_MPE */

/*
 * dmalloc (debugging malloc) support
 */
#ifdef H5_HAVE_DMALLOC_H
#include "dmalloc.h"
#endif /* H5_HAVE_DMALLOC_H */

/*
 * NT doesn't define SIGBUS, but since NT only runs on processors
 * that do not have alignment constraints a SIGBUS would never be
 * raised, so we just replace it with SIGILL (which also should
 * never be raised by the hdf5 library).
 */
#ifndef SIGBUS
#define SIGBUS SIGILL
#endif

/*
 * Does the compiler support the __attribute__(()) syntax?  It's no
 * big deal if we don't.
 */
#ifdef __cplusplus
#define __attribute__(X) /*void*/
#else                    /* __cplusplus */
#ifndef H5_HAVE_ATTRIBUTE
#define __attribute__(X) /*void*/
#endif
#endif /* __cplusplus */

/*
 * Status return values for the `herr_t' type.
 * Since some unix/c routines use 0 and -1 (or more precisely, non-negative
 * vs. negative) as their return code, and some assumption had been made in
 * the code about that, it is important to keep these constants the same
 * values.  When checking the success or failure of an integer-valued
 * function, remember to compare against zero and not one of these two
 * values.
 */
#define SUCCEED 0
#define FAIL    (-1)
#define UFAIL   (unsigned)(-1)

/* The HDF5 library uses the symbol `ERR` frequently.  So do
 * header files for libraries such as curses(3), terminfo(3), etc.
 * Remove its definition here to avoid clashes with HDF5.
 */
#ifdef ERR
#undef ERR
#endif

/* number of members in an array */
#ifndef NELMTS
#define NELMTS(X) (sizeof(X) / sizeof(X[0]))
#endif

/* minimum of two, three, or four values */
#undef MIN
#define MIN(a, b)        (((a) < (b)) ? (a) : (b))
#define MIN2(a, b)       MIN(a, b)
#define MIN3(a, b, c)    MIN(a, MIN(b, c))
#define MIN4(a, b, c, d) MIN(MIN(a, b), MIN(c, d))

/* maximum of two, three, or four values */
#undef MAX
#define MAX(a, b)        (((a) > (b)) ? (a) : (b))
#define MAX2(a, b)       MAX(a, b)
#define MAX3(a, b, c)    MAX(a, MAX(b, c))
#define MAX4(a, b, c, d) MAX(MAX(a, b), MAX(c, d))

/* limit the middle value to be within a range (inclusive) */
#define RANGE(LO, X, HI) MAX(LO, MIN(X, HI))

/* absolute value */
#ifndef ABS
#define ABS(a) (((a) >= 0) ? (a) : -(a))
#endif

/* sign of argument */
#ifndef SIGN
#define SIGN(a) ((a) > 0 ? 1 : (a) < 0 ? -1 : 0)
#endif

/* test for number that is a power of 2 */
/* (from: http://graphics.stanford.edu/~seander/bithacks.html#DetermineIfPowerOf2) */
#define POWER_OF_TWO(n) (!(n & (n - 1)) && n)

/* Raise an integer to a power of 2 */
#define H5_EXP2(n) (1 << (n))

/*
 * HDF Boolean type.
 */
#ifndef FALSE
#define FALSE 0
#endif
#ifndef TRUE
#define TRUE 1
#endif

/*
 * Numeric data types.  Some of these might be defined in Posix.1g, otherwise
 * we define them with the closest available type which is at least as large
 * as the number of bits indicated in the type name.  The `int8' types *must*
 * be exactly one byte wide because we use it for pointer calculations to
 * void* memory.
 */
#if H5_SIZEOF_INT8_T == 0
typedef signed char int8_t;
#undef H5_SIZEOF_INT8_T
#define H5_SIZEOF_INT8_T H5_SIZEOF_CHAR
#elif H5_SIZEOF_INT8_T == 1
#else
#error "the int8_t type must be 1 byte wide"
#endif

#if H5_SIZEOF_UINT8_T == 0
typedef unsigned char uint8_t;
#undef H5_SIZEOF_UINT8_T
#define H5_SIZEOF_UINT8_T H5_SIZEOF_CHAR
#elif H5_SIZEOF_UINT8_T == 1
#else
#error "the uint8_t type must be 1 byte wide"
#endif

#if H5_SIZEOF_INT16_T >= 2
#elif H5_SIZEOF_SHORT >= 2
typedef short          int16_t;
#undef H5_SIZEOF_INT16_T
#define H5_SIZEOF_INT16_T H5_SIZEOF_SHORT
#elif H5_SIZEOF_INT >= 2
typedef int      int16_t;
#undef H5_SIZEOF_INT16_T
#define H5_SIZEOF_INT16_T H5_SIZEOF_INT
#else
#error "nothing appropriate for int16_t"
#endif

#if H5_SIZEOF_UINT16_T >= 2
#elif H5_SIZEOF_SHORT >= 2
typedef unsigned short uint16_t;
#undef H5_SIZEOF_UINT16_T
#define H5_SIZEOF_UINT16_T H5_SIZEOF_SHORT
#elif H5_SIZEOF_INT >= 2
typedef unsigned uint16_t;
#undef H5_SIZEOF_UINT16_T
#define H5_SIZEOF_UINT16_T H5_SIZEOF_INT
#else
#error "nothing appropriate for uint16_t"
#endif

#if H5_SIZEOF_INT32_T >= 4
#elif H5_SIZEOF_SHORT >= 4
typedef short          int32_t;
#undef H5_SIZEOF_INT32_T
#define H5_SIZEOF_INT32_T H5_SIZEOF_SHORT
#elif H5_SIZEOF_INT >= 4
typedef int      int32_t;
#undef H5_SIZEOF_INT32_T
#define H5_SIZEOF_INT32_T H5_SIZEOF_INT
#elif H5_SIZEOF_LONG >= 4
typedef long int32_t;
#undef H5_SIZEOF_INT32_T
#define H5_SIZEOF_INT32_T H5_SIZEOF_LONG
#else
#error "nothing appropriate for int32_t"
#endif

/*
 * Maximum and minimum values.  These should be defined in <limits.h> for the
 * most part.
 */
#ifndef LLONG_MAX
#define LLONG_MAX ((long long)(((unsigned long long)1 << (8 * sizeof(long long) - 1)) - 1))
#define LLONG_MIN ((long long)(-LLONG_MAX) - 1)
#endif
#ifndef ULLONG_MAX
#define ULLONG_MAX ((unsigned long long)((long long)(-1)))
#endif
#ifndef SIZET_MAX
#define SIZET_MAX  ((size_t)(ssize_t)(-1))
#define SSIZET_MAX ((ssize_t)(((size_t)1 << (8 * sizeof(ssize_t) - 1)) - 1))
#endif

/*
 * Maximum & minimum values for our typedefs.
 */
#define HSIZET_MAX  ((hsize_t)ULLONG_MAX)
#define HSSIZET_MAX ((hssize_t)LLONG_MAX)
#define HSSIZET_MIN (~(HSSIZET_MAX))

/*
 * Types and max sizes for POSIX I/O.
 * OS X (Darwin) is odd since the max I/O size does not match the types.
 */
#if defined(H5_HAVE_WIN32_API)
#define h5_posix_io_t         unsigned int
#define h5_posix_io_ret_t     int
#define H5_POSIX_MAX_IO_BYTES INT_MAX
#elif defined(H5_HAVE_DARWIN)
#define h5_posix_io_t         size_t
#define h5_posix_io_ret_t     ssize_t
#define H5_POSIX_MAX_IO_BYTES INT_MAX
#else
#define h5_posix_io_t         size_t
#define h5_posix_io_ret_t     ssize_t
#define H5_POSIX_MAX_IO_BYTES SSIZET_MAX
#endif

/* POSIX I/O mode used as the third parameter to open/_open
 * when creating a new file (O_CREAT is set).
 */
#if defined(H5_HAVE_WIN32_API)
#define H5_POSIX_CREATE_MODE_RW (_S_IREAD | _S_IWRITE)
#else
#define H5_POSIX_CREATE_MODE_RW 0666
#endif

/*
 * A macro to portably increment enumerated types.
 */
#ifndef H5_INC_ENUM
#define H5_INC_ENUM(TYPE, VAR) (VAR) = ((TYPE)((VAR) + 1))
#endif

/*
 * A macro to portably decrement enumerated types.
 */
#ifndef H5_DEC_ENUM
#define H5_DEC_ENUM(TYPE, VAR) (VAR) = ((TYPE)((VAR)-1))
#endif

/* Double constant wrapper
 *
 * Quiets gcc warnings from -Wunsuffixed-float-constants.
 *
 * This is a really annoying warning since the standard specifies that
 * constants of type double do NOT get a suffix so there's no way
 * to specify a constant of type double. To quiet gcc, we specify floating
 * point constants as type long double and cast to double.
 *
 * Note that this macro only needs to be used where using a double
 * is important. For most code, suffixing constants with F will quiet the
 * compiler and not produce erroneous code.
 */
#define H5_DOUBLE(S) ((double)S##L)

/*
 * Methods to compare the equality of floating-point values:
 *
 *    1. H5_XXX_ABS_EQUAL - check if the difference is smaller than the
 *       Epsilon value.  The Epsilon values, FLT_EPSILON, DBL_EPSILON,
 *       and LDBL_EPSILON, are defined by compiler in float.h.
 *
 *    2. H5_XXX_REL_EQUAL - check if the relative difference is smaller than a
 *       predefined value M.  See if two values are relatively equal.
 *       It's the developer's responsibility not to pass in the value 0, which
 *       may cause the equation to fail.
 */
#define H5_FLT_ABS_EQUAL(X, Y)  (HDfabsf((X) - (Y)) < FLT_EPSILON)
#define H5_DBL_ABS_EQUAL(X, Y)  (HDfabs((X) - (Y)) < DBL_EPSILON)
#define H5_LDBL_ABS_EQUAL(X, Y) (HDfabsl((X) - (Y)) < LDBL_EPSILON)

#define H5_FLT_REL_EQUAL(X, Y, M)  (HDfabsf(((Y) - (X)) / (X)) < (M))
#define H5_DBL_REL_EQUAL(X, Y, M)  (HDfabs(((Y) - (X)) / (X)) < (M))
#define H5_LDBL_REL_EQUAL(X, Y, M) (HDfabsl(((Y) - (X)) / (X)) < (M))

/* KiB, MiB, GiB, TiB, EiB - Used in profiling and timing code */
#define H5_KB (1024.0F)
#define H5_MB (1024.0F * 1024.0F)
#define H5_GB (1024.0F * 1024.0F * 1024.0F)
#define H5_TB (1024.0F * 1024.0F * 1024.0F * 1024.0F)
#define H5_EB (1024.0F * 1024.0F * 1024.0F * 1024.0F * 1024.0F)

/*
 * Data types and functions for timing certain parts of the library.
 */
typedef struct {
    double utime; /*user time      */
    double stime; /*system time      */
    double etime; /*elapsed wall-clock time  */
} H5_timer_t;

H5_DLL void   H5_timer_reset(H5_timer_t *timer);
H5_DLL void   H5_timer_begin(H5_timer_t *timer);
H5_DLL void   H5_timer_end(H5_timer_t *sum /*in,out*/, H5_timer_t *timer /*in,out*/);
H5_DLL void   H5_bandwidth(char *buf /*out*/, double nbytes, double nseconds);
H5_DLL time_t H5_now(void);

/* Depth of object copy */
typedef enum {
    H5_COPY_SHALLOW, /* Shallow copy from source to destination, just copy field pointers */
    H5_COPY_DEEP     /* Deep copy from source to destination, including duplicating fields pointed to */
} H5_copy_depth_t;

/* Common object copying udata (right now only used for groups and datasets) */
typedef struct H5O_copy_file_ud_common_t {
    struct H5O_pline_t *src_pline; /* Copy of filter pipeline for object */
} H5O_copy_file_ud_common_t;

/* Unique object "position" */
typedef struct {
    unsigned long fileno; /* The unique identifier for the file of the object */
    haddr_t       addr;   /* The unique address of the object's header in that file */
} H5_obj_t;

/*
 * Redefine all the POSIX functions.  We should never see a POSIX
 * function (or any other non-HDF5 function) in the source!
 */

/* Put all platform-specific definitions in the following file */
/* so that the following definitions are platform free. */
#include "H5win32defs.h" /* For Windows-specific definitions */

#ifndef HDabort
#define HDabort() abort()
#endif
#ifndef HDabs
#define HDabs(X) abs(X)
#endif
#ifndef HDaccess
#define HDaccess(F, M) access(F, M)
#endif
#ifndef HDacos
#define HDacos(X) acos(X)
#endif
#ifndef HDalarm
#ifdef H5_HAVE_ALARM
#define HDalarm(N) alarm(N)
#else
#define HDalarm(N) (0)
#endif
#endif
#ifndef HDasctime
#define HDasctime(T) asctime(T)
#endif
#ifndef HDasin
#define HDasin(X) asin(X)
#endif
#ifndef HDasprintf
#define HDasprintf asprintf /*varargs*/
#endif
#ifndef HDassert
#define HDassert(X) assert(X)
#endif
#ifndef HDatan
#define HDatan(X) atan(X)
#endif
#ifndef HDatan2
#define HDatan2(X, Y) atan2(X, Y)
#endif
#ifndef HDatexit
#define HDatexit(F) atexit(F)
#endif
#ifndef HDatof
#define HDatof(S) atof(S)
#endif
#ifndef HDatoi
#define HDatoi(S) atoi(S)
#endif
#ifndef HDatol
#define HDatol(S) atol(S)
#endif
#ifndef HDatoll
#define HDatoll(S) atoll(S)
#endif
#ifndef HDbsearch
#define HDbsearch(K, B, N, Z, F) bsearch(K, B, N, Z, F)
#endif
#ifndef HDcalloc
#define HDcalloc(N, Z) calloc(N, Z)
#endif
#ifndef HDceil
#define HDceil(X) ceil(X)
#endif
#ifndef HDcfgetispeed
#define HDcfgetispeed(T) cfgetispeed(T)
#endif
#ifndef HDcfgetospeed
#define HDcfgetospeed(T) cfgetospeed(T)
#endif
#ifndef HDcfsetispeed
#define HDcfsetispeed(T, S) cfsetispeed(T, S)
#endif
#ifndef HDcfsetospeed
#define HDcfsetospeed(T, S) cfsetospeed(T, S)
#endif
#ifndef HDchdir
#define HDchdir(S) chdir(S)
#endif
#ifndef HDchmod
#define HDchmod(S, M) chmod(S, M)
#endif
#ifndef HDchown
#define HDchown(S, O, G) chown(S, O, G)
#endif
#ifndef HDclearerr
#define HDclearerr(F) clearerr(F)
#endif
#ifndef HDclock
#define HDclock() clock()
#endif
#ifndef HDclose
#define HDclose(F) close(F)
#endif
#ifndef HDclosedir
#define HDclosedir(D) closedir(D)
#endif
#ifndef HDcos
#define HDcos(X) cos(X)
#endif
#ifndef HDcosh
#define HDcosh(X) cosh(X)
#endif
#ifndef HDcreat
#define HDcreat(S, M) creat(S, M)
#endif
#ifndef HDctermid
#define HDctermid(S) ctermid(S)
#endif
#ifndef HDctime
#define HDctime(T) ctime(T)
#endif
#ifndef HDcuserid
#define HDcuserid(S) cuserid(S)
#endif
#ifndef HDdifftime
#ifdef H5_HAVE_DIFFTIME
#define HDdifftime(X, Y) difftime(X, Y)
#else
#define HDdifftime(X, Y) ((double)(X) - (double)(Y))
#endif
#endif
#ifndef HDdiv
#define HDdiv(X, Y) div(X, Y)
#endif
#ifndef HDdup
#define HDdup(F) dup(F)
#endif
#ifndef HDdup2
#define HDdup2(F, I) dup2(F, I)
#endif
/* execl() variable arguments */
/* execle() variable arguments */
/* execlp() variable arguments */
#ifndef HDexecv
#define HDexecv(S, AV) execv(S, AV)
#endif
#ifndef HDexecve
#define HDexecve(S, AV, E) execve(S, AV, E)
#endif
#ifndef HDexecvp
#define HDexecvp(S, AV) execvp(S, AV)
#endif
#ifndef HDexit
#define HDexit(N) exit(N)
#endif
#ifndef HD_exit
#define HD_exit(N) _exit(N)
#endif
#ifndef HDexp
#define HDexp(X) exp(X)
#endif
#ifndef HDexp2
#define HDexp2(X) exp2(X)
#endif
#ifndef HDfabs
#define HDfabs(X) fabs(X)
#endif
/* use ABS() because fabsf() fabsl() are not common yet. */
#ifndef HDfabsf
#define HDfabsf(X) ABS(X)
#endif
#ifndef HDfabsl
#define HDfabsl(X) ABS(X)
#endif
#ifndef HDfclose
#define HDfclose(F) fclose(F)
#endif
/* fcntl() variable arguments */
#ifndef HDfdopen
#define HDfdopen(N, S) fdopen(N, S)
#endif
#ifndef HDfeof
#define HDfeof(F) feof(F)
#endif
#ifndef HDferror
#define HDferror(F) ferror(F)
#endif
#ifndef HDfflush
#define HDfflush(F) fflush(F)
#endif
#ifndef HDfgetc
#define HDfgetc(F) fgetc(F)
#endif
#ifndef HDfgetpos
#define HDfgetpos(F, P) fgetpos(F, P)
#endif
#ifndef HDfgets
#define HDfgets(S, N, F) fgets(S, N, F)
#endif
#ifndef HDfileno
#define HDfileno(F) fileno(F)
#endif
#ifndef HDfloor
#define HDfloor(X) floor(X)
#endif
#ifndef HDfmod
#define HDfmod(X, Y) fmod(X, Y)
#endif
#ifndef HDfopen
#define HDfopen(S, M) fopen(S, M)
#endif
#ifndef HDfork
#define HDfork() fork()
#endif
#ifndef HDfpathconf
#define HDfpathconf(F, N) fpathconf(F, N)
#endif
H5_DLL int HDfprintf(FILE *stream, const char *fmt, ...);
#ifndef HDfputc
#define HDfputc(C, F) fputc(C, F)
#endif
#ifndef HDfputs
#define HDfputs(S, F) fputs(S, F)
#endif
#ifndef HDfread
#define HDfread(M, Z, N, F) fread(M, Z, N, F)
#endif
#ifndef HDfree
#define HDfree(M) free(M)
#endif
#ifndef HDfreopen
#define HDfreopen(S, M, F) freopen(S, M, F)
#endif
#ifndef HDfrexp
#define HDfrexp(X, N) frexp(X, N)
#endif
/* Check for Cray-specific 'frexpf()' and 'frexpl()' routines */
#ifndef HDfrexpf
#ifdef H5_HAVE_FREXPF
#define HDfrexpf(X, N) frexpf(X, N)
#else
#define HDfrexpf(X, N) frexp(X, N)
#endif
#endif
#ifndef HDfrexpl
#ifdef H5_HAVE_FREXPL
#define HDfrexpl(X, N) frexpl(X, N)
#else
#define HDfrexpl(X, N) frexp(X, N)
#endif
#endif
/* fscanf() variable arguments */
#ifndef HDfseek
#define HDfseek(F, O, W) fseeko(F, O, W)
#endif
#ifndef HDfsetpos
#define HDfsetpos(F, P) fsetpos(F, P)
#endif
#ifndef HDfstat
#define HDfstat(F, B) fstat(F, B)
#endif
#ifndef HDlstat
#define HDlstat(S, B) lstat(S, B)
#endif
#ifndef HDstat
#define HDstat(S, B) stat(S, B)
#endif

#ifndef H5_HAVE_WIN32_API
/* These definitions differ in Windows and are defined in
 * H5win32defs for that platform.
 */
typedef struct stat h5_stat_t;
typedef off_t       h5_stat_size_t;
#define HDoff_t off_t
#endif /* H5_HAVE_WIN32_API */

#define H5_SIZEOF_H5_STAT_SIZE_T H5_SIZEOF_OFF_T

#ifndef HDftell
#define HDftell(F) ftell(F)
#endif
#ifndef HDftruncate
#define HDftruncate(F, L) ftruncate(F, L)
#endif
#ifndef HDfwrite
#define HDfwrite(M, Z, N, F) fwrite(M, Z, N, F)
#endif
#ifndef HDgetc
#define HDgetc(F) getc(F)
#endif
#ifndef HDgetchar
#define HDgetchar() getchar()
#endif
#ifndef HDgetcwd
#define HDgetcwd(S, Z) getcwd(S, Z)
#endif
#ifndef HDgetdcwd
#define HDgetdcwd(D, S, Z) getcwd(S, Z)
#endif

/* Windows only - set to zero on other systems */
#ifndef HDgetdrive
#define HDgetdrive() 0
#endif

#ifndef HDgetegid
#define HDgetegid() getegid()
#endif
#ifndef HDgetenv
#define HDgetenv(S) getenv(S)
#endif
#ifndef HDgeteuid
#define HDgeteuid() geteuid()
#endif
#ifndef HDgetgid
#define HDgetgid() getgid()
#endif
#ifndef HDgetgrgid
#define HDgetgrgid(G) getgrgid(G)
#endif
#ifndef HDgetgrnam
#define HDgetgrnam(S) getgrnam(S)
#endif
#ifndef HDgetgroups
#define HDgetgroups(Z, G) getgroups(Z, G)
#endif
#ifndef HDgethostname
#define HDgethostname(N, L) gethostname(N, L)
#endif
#ifndef HDgetlogin
#define HDgetlogin() getlogin()
#endif
#ifndef HDgetpgrp
#define HDgetpgrp() getpgrp()
#endif
#ifndef HDgetpid
#define HDgetpid() getpid()
#endif
#ifndef HDgetppid
#define HDgetppid() getppid()
#endif
#ifndef HDgetpwnam
#define HDgetpwnam(S) getpwnam(S)
#endif
#ifndef HDgetpwuid
#define HDgetpwuid(U) getpwuid(U)
#endif
#ifndef HDgetrusage
#define HDgetrusage(X, S) getrusage(X, S)
#endif
#ifndef HDgets
#define HDgets(S) gets(S)
#endif

#ifndef HDgettimeofday
#define HDgettimeofday(S, P) gettimeofday(S, P)
#endif
#ifndef HDgetuid
#define HDgetuid() getuid()
#endif
#ifndef HDgmtime
#define HDgmtime(T) gmtime(T)
#endif
#ifndef HDisalnum
#define HDisalnum(C) isalnum((int)(C)) /* Cast for Solaris warning */
#endif
#ifndef HDisalpha
#define HDisalpha(C) isalpha((int)(C)) /* Cast for Solaris warning */
#endif
#ifndef HDisatty
#define HDisatty(F) isatty(F)
#endif
#ifndef HDiscntrl
#define HDiscntrl(C) iscntrl((int)(C)) /* Cast for solaris warning */
#endif
#ifndef HDisdigit
#define HDisdigit(C) isdigit((int)(C)) /* Cast for Solaris warning */
#endif
#ifndef HDisgraph
#define HDisgraph(C) isgraph((int)(C)) /* Cast for Solaris warning*/
#endif
#ifndef HDislower
#define HDislower(C) islower((int)(C)) /* Cast for Solaris warning */
#endif
#ifndef HDisnan
#define HDisnan(X) isnan(X)
#endif
#ifndef HDisprint
#define HDisprint(C) isprint((int)(C)) /* Cast for Solaris warning */
#endif
#ifndef HDispunct
#define HDispunct(C) ispunct((int)(C)) /* Cast for Solaris warning */
#endif
#ifndef HDisspace
#define HDisspace(C) isspace((int)(C)) /* Cast for Solaris warning */
#endif
#ifndef HDisupper
#define HDisupper(C) isupper((int)(C)) /* Cast for Solaris warning */
#endif
#ifndef HDisxdigit
#define HDisxdigit(C) isxdigit((int)(C)) /* Cast for Solaris warning */
#endif
#ifndef HDkill
#define HDkill(P, S) kill(P, S)
#endif
#ifndef HDlabs
#define HDlabs(X) labs(X)
#endif
#ifndef HDldexp
#define HDldexp(X, N) ldexp(X, N)
#endif
#ifndef HDldiv
#define HDldiv(X, Y) ldiv(X, Y)
#endif
#ifndef HDlink
#define HDlink(OLD, NEW) link(OLD, NEW)
#endif
#ifndef HDllround
#define HDllround(V) llround(V)
#endif
#ifndef HDllroundf
#define HDllroundf(V) llroundf(V)
#endif
#ifndef HDllroundl
#define HDllroundl(V) llroundl(V)
#endif
#ifndef HDlocaleconv
#define HDlocaleconv() localeconv()
#endif
#ifndef HDlocaltime
#define HDlocaltime(T) localtime(T)
#endif
#ifndef HDlog
#define HDlog(X) log(X)
#endif
#ifndef HDlog10
#define HDlog10(X) log10(X)
#endif
#ifndef HDlongjmp
#define HDlongjmp(J, N) longjmp(J, N)
#endif
#ifndef HDlround
#define HDlround(V) lround(V)
#endif
#ifndef HDlroundf
#define HDlroundf(V) lroundf(V)
#endif
#ifndef HDlroundl
#define HDlroundl(V) lroundl(V)
#endif
#ifndef HDlseek
#define HDlseek(F, O, W) lseek(F, O, W)
#endif
#ifndef HDmalloc
#define HDmalloc(Z) malloc(Z)
#endif
#ifndef HDposix_memalign
#define HDposix_memalign(P, A, Z) posix_memalign(P, A, Z)
#endif
#ifndef HDmblen
#define HDmblen(S, N) mblen(S, N)
#endif
#ifndef HDmbstowcs
#define HDmbstowcs(P, S, Z) mbstowcs(P, S, Z)
#endif
#ifndef HDmbtowc
#define HDmbtowc(P, S, Z) mbtowc(P, S, Z)
#endif
#ifndef HDmemchr
#define HDmemchr(S, C, Z) memchr(S, C, Z)
#endif
#ifndef HDmemcmp
#define HDmemcmp(X, Y, Z) memcmp(X, Y, Z)
#endif
/*
 * The (char*) casts are required for the DEC when optimizations are turned
 * on and the source and/or destination are not aligned.
 */
#ifndef HDmemcpy
#define HDmemcpy(X, Y, Z) memcpy((char *)(X), (const char *)(Y), Z)
#endif
#ifndef HDmemmove
#define HDmemmove(X, Y, Z) memmove((char *)(X), (const char *)(Y), Z)
#endif
#ifndef HDmemset
#define HDmemset(X, C, Z) memset(X, C, Z)
#endif
#ifndef HDmkdir
#define HDmkdir(S, M) mkdir(S, M)
#endif
#ifndef HDmkfifo
#define HDmkfifo(S, M) mkfifo(S, M)
#endif
#ifndef HDmktime
#define HDmktime(T) mktime(T)
#endif
#ifndef HDmodf
#define HDmodf(X, Y) modf(X, Y)
#endif
#ifndef HDnanosleep
#define HDnanosleep(N, O) nanosleep(N, O)
#endif
#ifndef HDopen
#define HDopen(F, ...) open(F, __VA_ARGS__)
#endif
#ifndef HDopendir
#define HDopendir(S) opendir(S)
#endif
#ifndef HDpathconf
#define HDpathconf(S, N) pathconf(S, N)
#endif
#ifndef HDpause
#define HDpause() pause()
#endif
#ifndef HDperror
#define HDperror(S) perror(S)
#endif
#ifndef HDpipe
#define HDpipe(F) pipe(F)
#endif
#ifndef HDpow
#define HDpow(X, Y) pow(X, Y)
#endif
/* printf() variable arguments */
#ifndef HDprintf
#define HDprintf(...) HDfprintf(stdout, __VA_ARGS__)
#endif
#ifndef HDputc
#define HDputc(C, F) putc(C, F)
#endif
#ifndef HDputchar
#define HDputchar(C) putchar(C)
#endif
#ifndef HDputs
#define HDputs(S) puts(S)
#endif
#ifndef HDqsort
#define HDqsort(M, N, Z, F) qsort(M, N, Z, F)
#endif
#ifndef HDraise
#define HDraise(N) raise(N)
#endif

/* clang-format off */
#ifdef H5_HAVE_RAND_R
#   ifndef HDrandom
#   define HDrandom() HDrand()
#   endif
    H5_DLL int HDrand(void);
#   ifndef HDsrandom
#   define HDsrandom(S) HDsrand(S)
#   endif
    H5_DLL void HDsrand(unsigned int seed);
#elif defined(H5_HAVE_RANDOM)
#   ifndef HDrand
#   define HDrand() random()
#   endif
#   ifndef HDrandom
#   define HDrandom() random()
#   endif
#   ifndef HDsrand
#   define HDsrand(S) srandom(S)
#   endif
#   ifndef HDsrandom
#   define HDsrandom(S) srandom(S)
#   endif
#else
#   ifndef HDrand
#   define HDrand() rand()
#   endif
#   ifndef HDrandom
#   define HDrandom() rand()
#   endif
#   ifndef HDsrand
#   define HDsrand(S) srand(S)
#   endif
#   ifndef HDsrandom
#   define HDsrandom(S) srand(S)
#   endif
#endif
/* clang-format on */

#ifndef HDread
#define HDread(F, M, Z) read(F, M, Z)
#endif
#ifndef HDreaddir
#define HDreaddir(D) readdir(D)
#endif
#ifndef HDrealloc
#define HDrealloc(M, Z) realloc(M, Z)
#endif
#ifndef HDrealpath
#define HDrealpath(F1, F2) realpath(F1, F2)
#endif
#ifndef HDremove
#define HDremove(S) remove(S)
#endif
#ifndef HDrename
#define HDrename(OLD, NEW) rename(OLD, NEW)
#endif
#ifndef HDrewind
#define HDrewind(F) rewind(F)
#endif
#ifndef HDrewinddir
#define HDrewinddir(D) rewinddir(D)
#endif
#ifndef HDround
#define HDround(V) round(V)
#endif
#ifndef HDroundf
#define HDroundf(V) roundf(V)
#endif
#ifndef HDroundl
#define HDroundl(V) roundl(V)
#endif
#ifndef HDrmdir
#define HDrmdir(S) rmdir(S)
#endif
/* scanf() variable arguments */
#ifndef HDselect
#define HDselect(N, RD, WR, ER, T) select(N, RD, WR, ER, T)
#endif
#ifndef HDsetbuf
#define HDsetbuf(F, S) setbuf(F, S)
#endif
#ifndef HDsetenv
#define HDsetenv(N, V, O) setenv(N, V, O)
#endif
#ifndef HDsetgid
#define HDsetgid(G) setgid(G)
#endif
#ifndef HDsetjmp
#define HDsetjmp(J) setjmp(J)
#endif
#ifndef HDsetlocale
#define HDsetlocale(N, S) setlocale(N, S)
#endif
#ifndef HDsetpgid
#define HDsetpgid(P, PG) setpgid(P, PG)
#endif
#ifndef HDsetsid
#define HDsetsid() setsid()
#endif
#ifndef HDsetuid
#define HDsetuid(U) setuid(U)
#endif
#ifndef HDsetvbuf
#define HDsetvbuf(F, S, M, Z) setvbuf(F, S, M, Z)
#endif
#ifndef HDsigaddset
#define HDsigaddset(S, N) sigaddset(S, N)
#endif
#ifndef HDsigdelset
#define HDsigdelset(S, N) sigdelset(S, N)
#endif
#ifndef HDsigemptyset
#define HDsigemptyset(S) sigemptyset(S)
#endif
#ifndef HDsigfillset
#define HDsigfillset(S) sigfillset(S)
#endif
#ifndef HDsigismember
#define HDsigismember(S, N) sigismember(S, N)
#endif
#ifndef HDsiglongjmp
#define HDsiglongjmp(J, N) siglongjmp(J, N)
#endif
#ifndef HDsignal
#define HDsignal(N, F) signal(N, F)
#endif
#ifndef HDsigpending
#define HDsigpending(S) sigpending(S)
#endif
#ifndef HDsigprocmask
#define HDsigprocmask(H, S, O) sigprocmask(H, S, O)
#endif
#ifndef HDsigsetjmp
#define HDsigsetjmp(J, N) sigsetjmp(J, N)
#endif
#ifndef HDsigsuspend
#define HDsigsuspend(S) sigsuspend(S)
#endif
#ifndef HDsin
#define HDsin(X) sin(X)
#endif
#ifndef HDsinh
#define HDsinh(X) sinh(X)
#endif
#ifndef HDsleep
#define HDsleep(N) sleep(N)
#endif
#ifndef HDsnprintf
#define HDsnprintf snprintf /*varargs*/
#endif
#ifndef HDsprintf
#define HDsprintf sprintf /*varargs*/
#endif
#ifndef HDsqrt
#define HDsqrt(X) sqrt(X)
#endif
#ifndef HDsscanf
#define HDsscanf(S, FMT, ...) sscanf(S, FMT, __VA_ARGS__)
#endif
#ifndef HDstrcat
#define HDstrcat(X, Y) strcat(X, Y)
#endif
#ifndef HDstrchr
#define HDstrchr(S, C) strchr(S, C)
#endif
#ifndef HDstrcmp
#define HDstrcmp(X, Y) strcmp(X, Y)
#endif
#ifndef HDstrcasecmp
#define HDstrcasecmp(X, Y) strcasecmp(X, Y)
#endif
#ifndef HDstrcoll
#define HDstrcoll(X, Y) strcoll(X, Y)
#endif
#ifndef HDstrcpy
#define HDstrcpy(X, Y) strcpy(X, Y)
#endif
#ifndef HDstrcspn
#define HDstrcspn(X, Y) strcspn(X, Y)
#endif
#ifndef HDstrerror
#define HDstrerror(N) strerror(N)
#endif
#ifndef HDstrftime
#define HDstrftime(S, Z, F, T) strftime(S, Z, F, T)
#endif
#ifndef HDstrlen
#define HDstrlen(S) strlen(S)
#endif
#ifndef HDstrncat
#define HDstrncat(X, Y, Z) strncat(X, Y, Z)
#endif
#ifndef HDstrncmp
#define HDstrncmp(X, Y, Z) strncmp(X, Y, Z)
#endif
#ifndef HDstrncpy
#define HDstrncpy(X, Y, Z) strncpy(X, Y, Z)
#endif
#ifndef HDstrpbrk
#define HDstrpbrk(X, Y) strpbrk(X, Y)
#endif
#ifndef HDstrrchr
#define HDstrrchr(S, C) strrchr(S, C)
#endif
#ifndef HDstrspn
#define HDstrspn(X, Y) strspn(X, Y)
#endif
#ifndef HDstrstr
#define HDstrstr(X, Y) strstr(X, Y)
#endif
#ifndef HDstrtod
#define HDstrtod(S, R) strtod(S, R)
#endif
#ifndef HDstrtok
#define HDstrtok(X, Y) strtok(X, Y)
#endif
#ifndef HDstrtok_r
#define HDstrtok_r(X, Y, Z) strtok_r(X, Y, Z)
#endif
#ifndef HDstrtol
#define HDstrtol(S, R, N) strtol(S, R, N)
#endif
#ifndef HDstrtoll
#ifdef H5_HAVE_STRTOLL
#define HDstrtoll(S, R, N) strtoll(S, R, N)
#else
H5_DLL int64_t HDstrtoll(const char *s, const char **rest, int base);
#endif
#endif
#ifndef HDstrtoul
#define HDstrtoul(S, R, N) strtoul(S, R, N)
#endif
#ifndef HDstrtoull
#define HDstrtoull(S, R, N) strtoull(S, R, N)
#endif
#ifndef HDstrtoumax
#define HDstrtoumax(S, R, N) strtoumax(S, R, N)
#endif
#ifndef HDstrxfrm
#define HDstrxfrm(X, Y, Z) strxfrm(X, Y, Z)
#endif
#ifdef H5_HAVE_SYMLINK
#ifndef HDsymlink
#define HDsymlink(F1, F2) symlink(F1, F2)
#endif
#endif
#ifndef HDsysconf
#define HDsysconf(N) sysconf(N)
#endif
#ifndef HDsystem
#define HDsystem(S) system(S)
#endif
#ifndef HDtan
#define HDtan(X) tan(X)
#endif
#ifndef HDtanh
#define HDtanh(X) tanh(X)
#endif
#ifndef HDtcdrain
#define HDtcdrain(F) tcdrain(F)
#endif
#ifndef HDtcflow
#define HDtcflow(F, A) tcflow(F, A)
#endif
#ifndef HDtcflush
#define HDtcflush(F, N) tcflush(F, N)
#endif
#ifndef HDtcgetattr
#define HDtcgetattr(F, T) tcgetattr(F, T)
#endif
#ifndef HDtcgetpgrp
#define HDtcgetpgrp(F) tcgetpgrp(F)
#endif
#ifndef HDtcsendbreak
#define HDtcsendbreak(F, N) tcsendbreak(F, N)
#endif
#ifndef HDtcsetattr
#define HDtcsetattr(F, O, T) tcsetattr(F, O, T)
#endif
#ifndef HDtcsetpgrp
#define HDtcsetpgrp(F, N) tcsetpgrp(F, N)
#endif
#ifndef HDtime
#define HDtime(T) time(T)
#endif
#ifndef HDtimes
#define HDtimes(T) times(T)
#endif
#ifndef HDtmpfile
#define HDtmpfile() tmpfile()
#endif
#ifndef HDtmpnam
#define HDtmpnam(S) tmpnam(S)
#endif
#ifndef HDtolower
#define HDtolower(C) tolower(C)
#endif
#ifndef HDtoupper
#define HDtoupper(C) toupper(C)
#endif
#ifndef HDttyname
#define HDttyname(F) ttyname(F)
#endif
#ifndef HDtzset
#define HDtzset() tzset()
#endif
#ifndef HDumask
#define HDumask(N) umask(N)
#endif
#ifndef HDuname
#define HDuname(S) uname(S)
#endif
#ifndef HDungetc
#define HDungetc(C, F) ungetc(C, F)
#endif
#ifndef HDunlink
#define HDunlink(S) unlink(S)
#endif
#ifndef HDutime
#define HDutime(S, T) utime(S, T)
#endif
#ifndef HDva_arg
#define HDva_arg(A, T) va_arg(A, T)
#endif
#ifndef HDva_copy
#define HDva_copy(D, S) va_copy(D, S)
#endif
#ifndef HDva_end
#define HDva_end(A) va_end(A)
#endif
#ifndef HDva_start
#define HDva_start(A, P) va_start(A, P)
#endif
#ifndef HDvasprintf
#define HDvasprintf(RET, FMT, A) vasprintf(RET, FMT, A)
#endif
#ifndef HDvfprintf
#define HDvfprintf(F, FMT, A) vfprintf(F, FMT, A)
#endif
#ifndef HDvprintf
#define HDvprintf(FMT, A) vprintf(FMT, A)
#endif
#ifndef HDvsprintf
#define HDvsprintf(S, FMT, A) vsprintf(S, FMT, A)
#endif
#ifndef HDvsnprintf
#define HDvsnprintf(S, N, FMT, A) vsnprintf(S, N, FMT, A)
#endif
#ifndef HDwait
#define HDwait(W) wait(W)
#endif
#ifndef HDwaitpid
#define HDwaitpid(P, W, O) waitpid(P, W, O)
#endif
#ifndef HDwcstombs
#define HDwcstombs(S, P, Z) wcstombs(S, P, Z)
#endif
#ifndef HDwctomb
#define HDwctomb(S, C) wctomb(S, C)
#endif
#ifndef HDwrite
#define HDwrite(F, M, Z) write(F, M, Z)
#endif

/*
 * And now for a couple non-Posix functions...  Watch out for systems that
 * define these in terms of macros.
 */
#if !defined strdup && !defined H5_HAVE_STRDUP
extern char *                   strdup(const char *s);
#endif

#ifndef HDstrdup
#define HDstrdup(S) strdup(S)
#endif

#ifndef HDpthread_self
#define HDpthread_self() pthread_self()
#endif

/* Use this version of pthread_self for printing the thread ID */
#ifndef HDpthread_self_ulong
#define HDpthread_self_ulong() ((unsigned long)pthread_self())
#endif

/* Macro for "stringizing" an integer in the C preprocessor (use H5_TOSTRING) */
/* (use H5_TOSTRING, H5_STRINGIZE is just part of the implementation) */
#define H5_STRINGIZE(x) #x
#define H5_TOSTRING(x)  H5_STRINGIZE(x)

/* Macro for "glueing" together items, for re-scanning macros */
#define H5_GLUE(x, y)        x##y
#define H5_GLUE3(x, y, z)    x##y##z
#define H5_GLUE4(w, x, y, z) w##x##y##z

/*
 * A macro for detecting over/under-flow when casting between types
 */
#ifndef NDEBUG
#define H5_CHECK_OVERFLOW(var, vartype, casttype)                                                            \
    {                                                                                                        \
        casttype _tmp_overflow = (casttype)(var);                                                            \
        HDassert((var) == (vartype)_tmp_overflow);                                                           \
    }
#else /* NDEBUG */
#define H5_CHECK_OVERFLOW(var, vartype, casttype)
#endif /* NDEBUG */

/*
 * A macro for detecting over/under-flow when assigning between types
 */
#ifndef NDEBUG
#define ASSIGN_TO_SMALLER_SIZE(dst, dsttype, src, srctype)                                                   \
    {                                                                                                        \
        srctype _tmp_src = (srctype)(src);                                                                   \
        dsttype _tmp_dst = (dsttype)(_tmp_src);                                                              \
        HDassert(_tmp_src == (srctype)_tmp_dst);                                                             \
        (dst) = _tmp_dst;                                                                                    \
    }

#define ASSIGN_TO_LARGER_SIZE_SAME_SIGNED(dst, dsttype, src, srctype) (dst) = (dsttype)(src);

#define ASSIGN_TO_LARGER_SIZE_SIGNED_TO_UNSIGNED(dst, dsttype, src, srctype)                                 \
    {                                                                                                        \
        srctype _tmp_src = (srctype)(src);                                                                   \
        dsttype _tmp_dst = (dsttype)(_tmp_src);                                                              \
        HDassert(_tmp_src >= 0);                                                                             \
        HDassert(_tmp_src == (srctype)_tmp_dst);                                                             \
        (dst) = _tmp_dst;                                                                                    \
    }

#define ASSIGN_TO_LARGER_SIZE_UNSIGNED_TO_SIGNED(dst, dsttype, src, srctype) (dst) = (dsttype)(src);

#define ASSIGN_TO_SAME_SIZE_UNSIGNED_TO_SIGNED(dst, dsttype, src, srctype)                                   \
    {                                                                                                        \
        srctype _tmp_src = (srctype)(src);                                                                   \
        dsttype _tmp_dst = (dsttype)(_tmp_src);                                                              \
        HDassert(_tmp_dst >= 0);                                                                             \
        HDassert(_tmp_src == (srctype)_tmp_dst);                                                             \
        (dst) = _tmp_dst;                                                                                    \
    }

#define ASSIGN_TO_SAME_SIZE_SIGNED_TO_UNSIGNED(dst, dsttype, src, srctype)                                   \
    {                                                                                                        \
        srctype _tmp_src = (srctype)(src);                                                                   \
        dsttype _tmp_dst = (dsttype)(_tmp_src);                                                              \
        HDassert(_tmp_src >= 0);                                                                             \
        HDassert(_tmp_src == (srctype)_tmp_dst);                                                             \
        (dst) = _tmp_dst;                                                                                    \
    }

#define ASSIGN_TO_SAME_SIZE_SAME_SIGNED(dst, dsttype, src, srctype) (dst) = (dsttype)(src);

/* Include the generated overflow header file */
#include "H5overflow.h"

/* Assign a variable to one of a different size (think safer dst = (dsttype)src").
 * The code generated by the macro checks for overflows.
 *
 * Use w##x##y##z instead of H5_GLUE4(w, x, y, z) because srctype
 * or dsttype on some systems (e.g., NetBSD 8 and earlier) may
 * supply some standard types using a macro---e.g.,
 * #define uint8_t __uint8_t.  The preprocessor will expand the
 * macros before it evaluates H5_GLUE4(), and that will generate
 * an unexpected name such as ASSIGN___uint8_t_TO___uint16_t.
 * The preprocessor does not expand macros in w##x##y##z, so
 * that will always generate the expected name.
 */
#define H5_CHECKED_ASSIGN(dst, dsttype, src, srctype)                                                        \
    ASSIGN_##srctype##_TO_##dsttype(dst, dsttype, src, srctype)

#else /* NDEBUG */
#define H5_CHECKED_ASSIGN(dst, dsttype, src, srctype) (dst) = (dsttype)(src);
#endif /* NDEBUG */

#if defined(H5_HAVE_WINDOW_PATH)

/* directory delimiter for Windows: slash and backslash are acceptable on Windows */
#define H5_DIR_SLASH_SEPC        '/'
#define H5_DIR_SEPC              '\\'
#define H5_DIR_SEPS              "\\"
#define H5_CHECK_DELIMITER(SS)   ((SS == H5_DIR_SEPC) || (SS == H5_DIR_SLASH_SEPC))
#define H5_CHECK_ABSOLUTE(NAME)  ((HDisalpha(NAME[0])) && (NAME[1] == ':') && (H5_CHECK_DELIMITER(NAME[2])))
#define H5_CHECK_ABS_DRIVE(NAME) ((HDisalpha(NAME[0])) && (NAME[1] == ':'))
#define H5_CHECK_ABS_PATH(NAME)  (H5_CHECK_DELIMITER(NAME[0]))

#define H5_GET_LAST_DELIMITER(NAME, ptr)                                                                     \
    {                                                                                                        \
        char *slash, *backslash;                                                                             \
                                                                                                             \
        slash     = HDstrrchr(NAME, H5_DIR_SLASH_SEPC);                                                      \
        backslash = HDstrrchr(NAME, H5_DIR_SEPC);                                                            \
        if (backslash > slash)                                                                               \
            (ptr = backslash);                                                                               \
        else                                                                                                 \
            (ptr = slash);                                                                                   \
    }

#else /* H5_HAVE_WINDOW_PATH */

#define H5_DIR_SEPC                      '/'
#define H5_DIR_SEPS                      "/"
#define H5_CHECK_DELIMITER(SS)           (SS == H5_DIR_SEPC)
#define H5_CHECK_ABSOLUTE(NAME)          (H5_CHECK_DELIMITER(*NAME))
#define H5_CHECK_ABS_DRIVE(NAME)         (0)
#define H5_CHECK_ABS_PATH(NAME)          (0)
#define H5_GET_LAST_DELIMITER(NAME, ptr) ptr = HDstrrchr(NAME, H5_DIR_SEPC);

#endif /* H5_HAVE_WINDOW_PATH */

#define H5_COLON_SEPC ':'

/* Use FUNC to safely handle variations of C99 __func__ keyword handling */
#ifdef H5_HAVE_C99_FUNC
#define FUNC __func__
#elif defined(H5_HAVE_FUNCTION)
#define FUNC __FUNCTION__
#else
#error "We need __func__ or __FUNCTION__ to test function names!"
#endif

/*
 * These macros check whether debugging has been requested for a certain
 * package at run-time.   Code for debugging is conditionally compiled by
 * defining constants like `H5X_DEBUG'.   In order to see the output though
 * the code must be enabled at run-time with an environment variable
 * HDF5_DEBUG which is a list of packages to debug.
 *
 * Note:  If you add/remove items from this enum then be sure to update the
 *    information about the package in H5_init_library().
 */
typedef enum {
    H5_PKG_A,  /* Attributes               */
    H5_PKG_AC, /* Metadata cache           */
    H5_PKG_B,  /* B-trees                  */
    H5_PKG_D,  /* Datasets                 */
    H5_PKG_E,  /* Error handling           */
    H5_PKG_F,  /* Files                    */
    H5_PKG_G,  /* Groups                   */
    H5_PKG_HG, /* Global heaps             */
    H5_PKG_HL, /* Local heaps              */
    H5_PKG_I,  /* IDs                      */
    H5_PKG_MF, /* File memory management   */
    H5_PKG_MM, /* Core memory management   */
    H5_PKG_O,  /* Object headers           */
    H5_PKG_P,  /* Property lists           */
    H5_PKG_S,  /* Dataspaces               */
    H5_PKG_T,  /* Datatypes                */
    H5_PKG_V,  /* Vector functions         */
    H5_PKG_Z,  /* Raw data filters         */
    H5_NPKGS   /* Must be last             */
} H5_pkg_t;

typedef struct H5_debug_open_stream_t {
    FILE *                         stream; /* Open output stream */
    struct H5_debug_open_stream_t *next;   /* Next open output stream */
} H5_debug_open_stream_t;

typedef struct H5_debug_t {
    FILE *  trace;  /*API trace output stream  */
    hbool_t ttop;   /*Show only top-level calls?    */
    hbool_t ttimes; /*Show trace event times?       */
    struct {
        const char *name;   /*package name      */
        FILE *      stream; /*output stream  or NULL    */
    } pkg[H5_NPKGS];
    H5_debug_open_stream_t *open_stream; /* Stack of open output streams */
} H5_debug_t;

extern H5_debug_t H5_debug_g;
#define H5DEBUG(X) (H5_debug_g.pkg[H5_PKG_##X].stream)
/* Do not use const else AIX strings does not show it. */
extern char H5libhdf5_settings[]; /* embedded library information */

/*-------------------------------------------------------------------------
 * Purpose: These macros are inserted automatically just after the
 *          FUNC_ENTER() macro of API functions and are used to trace
 *          application program execution. Unless H5_DEBUG_API has been
 *          defined they are no-ops.
 *
 * Arguments:   R  - Return type encoded as a string
 *              T  - Argument types encoded as a string
 *              A0-An  - Arguments.  The number at the end of the macro name
 *                                   indicates the number of arguments.
 *
 *-------------------------------------------------------------------------
 */
#ifdef H5_DEBUG_API

#define H5TRACE_DECL                                                                                         \
    const char *RTYPE = NULL;                                                                                \
    double      CALLTIME;

#define H5TRACE0(R, T)                                                                                       \
    RTYPE    = R;                                                                                            \
    CALLTIME = H5_trace(NULL, FUNC, T)
#define H5TRACE1(R, T, A0)                                                                                   \
    RTYPE    = R;                                                                                            \
    CALLTIME = H5_trace(NULL, FUNC, T, #A0, A0)
#define H5TRACE2(R, T, A0, A1)                                                                               \
    RTYPE    = R;                                                                                            \
    CALLTIME = H5_trace(NULL, FUNC, T, #A0, A0, #A1, A1)
#define H5TRACE3(R, T, A0, A1, A2)                                                                           \
    RTYPE    = R;                                                                                            \
    CALLTIME = H5_trace(NULL, FUNC, T, #A0, A0, #A1, A1, #A2, A2)
#define H5TRACE4(R, T, A0, A1, A2, A3)                                                                       \
    RTYPE    = R;                                                                                            \
    CALLTIME = H5_trace(NULL, FUNC, T, #A0, A0, #A1, A1, #A2, A2, #A3, A3)
#define H5TRACE5(R, T, A0, A1, A2, A3, A4)                                                                   \
    RTYPE    = R;                                                                                            \
    CALLTIME = H5_trace(NULL, FUNC, T, #A0, A0, #A1, A1, #A2, A2, #A3, A3, #A4, A4)
#define H5TRACE6(R, T, A0, A1, A2, A3, A4, A5)                                                               \
    RTYPE    = R;                                                                                            \
    CALLTIME = H5_trace(NULL, FUNC, T, #A0, A0, #A1, A1, #A2, A2, #A3, A3, #A4, A4, #A5, A5)
#define H5TRACE7(R, T, A0, A1, A2, A3, A4, A5, A6)                                                           \
    RTYPE    = R;                                                                                            \
    CALLTIME = H5_trace(NULL, FUNC, T, #A0, A0, #A1, A1, #A2, A2, #A3, A3, #A4, A4, #A5, A5, #A6, A6)
#define H5TRACE8(R, T, A0, A1, A2, A3, A4, A5, A6, A7)                                                       \
    RTYPE    = R;                                                                                            \
    CALLTIME = H5_trace(NULL, FUNC, T, #A0, A0, #A1, A1, #A2, A2, #A3, A3, #A4, A4, #A5, A5, #A6, A6, #A7, A7)
#define H5TRACE9(R, T, A0, A1, A2, A3, A4, A5, A6, A7, A8)                                                   \
    RTYPE    = R;                                                                                            \
    CALLTIME = H5_trace(NULL, FUNC, T, #A0, A0, #A1, A1, #A2, A2, #A3, A3, #A4, A4, #A5, A5, #A6, A6, #A7,   \
                        A7, #A8, A8)
#define H5TRACE10(R, T, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9)                                              \
    RTYPE    = R;                                                                                            \
    CALLTIME = H5_trace(NULL, FUNC, T, #A0, A0, #A1, A1, #A2, A2, #A3, A3, #A4, A4, #A5, A5, #A6, A6, #A7,   \
                        A7, #A8, A8, #A9, A9)
#define H5TRACE11(R, T, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)                                         \
    RTYPE    = R;                                                                                            \
    CALLTIME = H5_trace(NULL, FUNC, T, #A0, A0, #A1, A1, #A2, A2, #A3, A3, #A4, A4, #A5, A5, #A6, A6, #A7,   \
                        A7, #A8, A8, #A9, A9, #A10, A10)
#define H5TRACE12(R, T, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11)                                    \
    RTYPE    = R;                                                                                            \
    CALLTIME = H5_trace(NULL, FUNC, T, #A0, A0, #A1, A1, #A2, A2, #A3, A3, #A4, A4, #A5, A5, #A6, A6, #A7,   \
                        A7, #A8, A8, #A9, A9, #A10, A10, #A11, A11)

#define H5TRACE_RETURN(V)                                                                                    \
    if (RTYPE) {                                                                                             \
        H5_trace(&CALLTIME, FUNC, RTYPE, NULL, V);                                                           \
        RTYPE = NULL;                                                                                        \
    }
#else
#define H5TRACE_DECL                                                      /*void*/
#define H5TRACE0(R, T)                                                    /*void*/
#define H5TRACE1(R, T, A0)                                                /*void*/
#define H5TRACE2(R, T, A0, A1)                                            /*void*/
#define H5TRACE3(R, T, A0, A1, A2)                                        /*void*/
#define H5TRACE4(R, T, A0, A1, A2, A3)                                    /*void*/
#define H5TRACE5(R, T, A0, A1, A2, A3, A4)                                /*void*/
#define H5TRACE6(R, T, A0, A1, A2, A3, A4, A5)                            /*void*/
#define H5TRACE7(R, T, A0, A1, A2, A3, A4, A5, A6)                        /*void*/
#define H5TRACE8(R, T, A0, A1, A2, A3, A4, A5, A6, A7)                    /*void*/
#define H5TRACE9(R, T, A0, A1, A2, A3, A4, A5, A6, A7, A8)                /*void*/
#define H5TRACE10(R, T, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9)           /*void*/
#define H5TRACE11(R, T, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)      /*void*/
#define H5TRACE12(R, T, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11) /*void*/
#define H5TRACE_RETURN(V)                                                 /*void*/
#endif                                                                    /* H5_DEBUG_API */

H5_DLL double H5_trace(const double *calltime, const char *func, const char *type, ...);

/*-------------------------------------------------------------------------
 * Purpose:  Register function entry for library initialization and code
 *    profiling.
 *
 * Notes:  Every file must have a file-scope variable called
 *    `initialize_interface_g' of type hbool_t which is initialized
 *    to FALSE.
 *
 *    Don't use local variable initializers which contain
 *    calls to other library functions since the initializer
 *    would happen before the FUNC_ENTER() gets called.  Don't
 *    use initializers that require special cleanup code to
 *    execute if FUNC_ENTER() fails since a failing FUNC_ENTER()
 *    returns immediately without branching to the `done' label.
 *
 * Programmer:  Quincey Koziol
 *
 *-------------------------------------------------------------------------
 */

/* `S' is the name of a function which is being tested to check if it's
 *  an API function.
 *
 *  BADNESS:
 *      - Underscore at positions 2 or 3 (0-indexed string). Handles
 *        H5_ and H5X_.
 *      - Underscore at position 4 if position 3 is uppercase or a digit.
 *        Handles H5XY_.
 */
#define H5_IS_API(S)                                                                                         \
    ('_' != ((const char *)S)[2]                  /* underscore at position 2     */                         \
     && '_' != ((const char *)S)[3]               /* underscore at position 3     */                         \
     && !(                                        /* NOT              */                                     \
          ((const char *)S)[4]                    /* pos 4 exists     */                                     \
          && (HDisupper(S[3]) || HDisdigit(S[3])) /* pos 3 dig | uc   */                                     \
          && '_' == ((const char *)S)[4]          /* pos 4 underscore */                                     \
          ))

/* `S' is the name of a function which is being tested to check if it's */
/*      a public API function */
#define H5_IS_PUB(S)                                                                                         \
    (((HDisdigit(S[1]) || HDisupper(S[1])) && HDislower(S[2])) ||                                            \
     ((HDisdigit(S[2]) || HDisupper(S[2])) && HDislower(S[3])) ||                                            \
     (!S[4] || ((HDisdigit(S[3]) || HDisupper(S[3])) && HDislower(S[4]))))

/* `S' is the name of a function which is being tested to check if it's */
/*      a private library function */
#define H5_IS_PRIV(S)                                                                                        \
    (((HDisdigit(S[1]) || HDisupper(S[1])) && '_' == S[2] && HDislower(S[3])) ||                             \
     ((HDisdigit(S[2]) || HDisupper(S[2])) && '_' == S[3] && HDislower(S[4])) ||                             \
     ((HDisdigit(S[3]) || HDisupper(S[3])) && '_' == S[4] && HDislower(S[5])))

/* `S' is the name of a function which is being tested to check if it's */
/*      a package private function */
#define H5_IS_PKG(S)                                                                                         \
    (((HDisdigit(S[1]) || HDisupper(S[1])) && '_' == S[2] && '_' == S[3] && HDislower(S[4])) ||              \
     ((HDisdigit(S[2]) || HDisupper(S[2])) && '_' == S[3] && '_' == S[4] && HDislower(S[5])) ||              \
     ((HDisdigit(S[3]) || HDisupper(S[3])) && '_' == S[4] && '_' == S[5] && HDislower(S[6])))

/* global library version information string */
extern char H5_lib_vers_info_g[];

#include "H5TSprivate.h"

/* Lock headers */
#ifdef H5_HAVE_THREADSAFE

/* replacement structure for original global variable */
typedef struct H5_api_struct {
    H5TS_mutex_t init_lock;    /* API entrance mutex */
    hbool_t      H5_libinit_g; /* Has the library been initialized? */
} H5_api_t;

/* Macros for accessing the global variables */
#define H5_INIT_GLOBAL H5_g.H5_libinit_g

/* Macro for first thread initialization */
#ifdef H5_HAVE_WIN_THREADS
#define H5_FIRST_THREAD_INIT InitOnceExecuteOnce(&H5TS_first_init_g, H5TS_win32_process_enter, NULL, NULL);
#else
#define H5_FIRST_THREAD_INIT pthread_once(&H5TS_first_init_g, H5TS_pthread_first_thread_init);
#endif

/* Macros for threadsafe HDF-5 Phase I locks */
#define H5_API_LOCK   H5TS_mutex_lock(&H5_g.init_lock);
#define H5_API_UNLOCK H5TS_mutex_unlock(&H5_g.init_lock);

/* Macros for thread cancellation-safe mechanism */
#define H5_API_UNSET_CANCEL H5TS_cancel_count_inc();

#define H5_API_SET_CANCEL H5TS_cancel_count_dec();

extern H5_api_t H5_g;

#else /* H5_HAVE_THREADSAFE */

/* disable any first thread init mechanism */
#define H5_FIRST_THREAD_INIT

/* disable locks (sequential version) */
#define H5_API_LOCK
#define H5_API_UNLOCK

/* disable cancellability (sequential version) */
#define H5_API_UNSET_CANCEL
#define H5_API_SET_CANCEL

/* extern global variables */
extern hbool_t H5_libinit_g; /* Has the library been initialized? */

/* Macros for accessing the global variables */
#define H5_INIT_GLOBAL H5_libinit_g

#endif /* H5_HAVE_THREADSAFE */

#ifdef H5_HAVE_CODESTACK

/* Include required function stack header */
#include "H5CSprivate.h"

#define H5_PUSH_FUNC H5CS_push(FUNC);
#define H5_POP_FUNC  H5CS_pop();
#else                /* H5_HAVE_CODESTACK */
#define H5_PUSH_FUNC /* void */
#define H5_POP_FUNC  /* void */
#endif               /* H5_HAVE_CODESTACK */

#ifdef H5_HAVE_MPE
extern hbool_t H5_MPEinit_g; /* Has the MPE Library been initialized? */
#endif

/* Macros for defining interface initialization routines */
#ifdef H5_INTERFACE_INIT_FUNC
static int    H5_interface_initialize_g = 0;
static herr_t H5_INTERFACE_INIT_FUNC(void);
#define H5_INTERFACE_INIT(err)                                                                               \
    /* Initialize this interface or bust */                                                                  \
    if (!H5_interface_initialize_g) {                                                                        \
        H5_interface_initialize_g = 1;                                                                       \
        if (H5_INTERFACE_INIT_FUNC() < 0) {                                                                  \
            H5_interface_initialize_g = 0;                                                                   \
            HGOTO_ERROR(H5E_FUNC, H5E_CANTINIT, err, "interface initialization failed")                      \
        }                                                                                                    \
    }
#else /* H5_INTERFACE_INIT_FUNC */
#define H5_INTERFACE_INIT(err)
#endif /* H5_INTERFACE_INIT_FUNC */

#ifndef NDEBUG
#define FUNC_ENTER_CHECK_NAME(asrt)                                                                          \
    {                                                                                                        \
        static hbool_t func_check = FALSE;                                                                   \
                                                                                                             \
        if (!func_check) {                                                                                   \
            /* Check function naming status */                                                               \
            HDassert(asrt);                                                                                  \
                                                                                                             \
            /* Don't check again */                                                                          \
            func_check = TRUE;                                                                               \
        } /* end if */                                                                                       \
    }     /* end scope */
#else     /* NDEBUG */
#define FUNC_ENTER_CHECK_NAME(asrt)
#endif /* NDEBUG */

#define FUNC_ENTER_COMMON(asrt)                                                                              \
    hbool_t err_occurred = FALSE;                                                                            \
                                                                                                             \
    FUNC_ENTER_CHECK_NAME(asrt);

#define FUNC_ENTER_COMMON_NOERR(asrt) FUNC_ENTER_CHECK_NAME(asrt);

/* Threadsafety initialization code for API routines */
#define FUNC_ENTER_API_THREADSAFE                                                                            \
    /* Initialize the thread-safe code */                                                                    \
    H5_FIRST_THREAD_INIT                                                                                     \
                                                                                                             \
    /* Grab the mutex for the library */                                                                     \
    H5_API_UNSET_CANCEL                                                                                      \
    H5_API_LOCK

/* Local variables for API routines */
#define FUNC_ENTER_API_VARS                                                                                  \
    MPE_LOG_VARS                                                                                             \
    H5TRACE_DECL

#define FUNC_ENTER_API_COMMON                                                                                \
    FUNC_ENTER_API_VARS                                                                                      \
    FUNC_ENTER_COMMON(H5_IS_API(FUNC));                                                                      \
    FUNC_ENTER_API_THREADSAFE;

#define FUNC_ENTER_API_INIT(err)                                                                             \
    /* Initialize the library */                                                                             \
    if (!(H5_INIT_GLOBAL)) {                                                                                 \
        H5_INIT_GLOBAL = TRUE;                                                                               \
        if (H5_init_library() < 0)                                                                           \
            HGOTO_ERROR(H5E_FUNC, H5E_CANTINIT, err, "library initialization failed")                        \
    }                                                                                                        \
                                                                                                             \
    /* Initialize the interface, if appropriate */                                                           \
    H5_INTERFACE_INIT(err)                                                                                   \
                                                                                                             \
    /* Push the name of this function on the function stack */                                               \
    H5_PUSH_FUNC                                                                                             \
                                                                                                             \
    BEGIN_MPE_LOG

/* Use this macro for all "normal" API functions */
#define FUNC_ENTER_API(err)                                                                                  \
    {                                                                                                        \
        {                                                                                                    \
            FUNC_ENTER_API_COMMON                                                                            \
            FUNC_ENTER_API_INIT(err);                                                                        \
            /* Clear thread error stack entering public functions */                                         \
            H5E_clear_stack(NULL);                                                                           \
            {

/*
 * Use this macro for API functions that shouldn't clear the error stack
 *      like H5Eprint and H5Ewalk.
 */
#define FUNC_ENTER_API_NOCLEAR(err)                                                                          \
    {                                                                                                        \
        {                                                                                                    \
            FUNC_ENTER_API_COMMON                                                                            \
            FUNC_ENTER_API_INIT(err);                                                                        \
            {

/*
 * Use this macro for API functions that shouldn't perform _any_ initialization
 *      of the library or an interface, just perform tracing, etc.  Examples
 *      are: H5check_version, etc.
 *
 */
#define FUNC_ENTER_API_NOINIT                                                                                \
    {                                                                                                        \
        {                                                                                                    \
            FUNC_ENTER_API_COMMON                                                                            \
            H5_PUSH_FUNC                                                                                     \
            BEGIN_MPE_LOG                                                                                    \
            {

/*
 * Use this macro for API functions that shouldn't perform _any_ initialization
 *      of the library or an interface or push themselves on the function
 *      stack, just perform tracing, etc.  Examples
 *      are: H5close, H5check_version, etc.
 *
 */
#define FUNC_ENTER_API_NOINIT_NOERR_NOFS                                                                     \
    {                                                                                                        \
        {                                                                                                    \
            FUNC_ENTER_API_VARS                                                                              \
            FUNC_ENTER_COMMON_NOERR(H5_IS_API(FUNC));                                                        \
            FUNC_ENTER_API_THREADSAFE;                                                                       \
            BEGIN_MPE_LOG                                                                                    \
            {

/* Note: this macro only works when there's _no_ interface initialization routine for the module */
#define FUNC_ENTER_NOAPI_INIT(err)                                                                           \
    /* Initialize the interface, if appropriate */                                                           \
    H5_INTERFACE_INIT(err)                                                                                   \
                                                                                                             \
    /* Push the name of this function on the function stack */                                               \
    H5_PUSH_FUNC

/* Use this macro for all "normal" non-API functions */
#define FUNC_ENTER_NOAPI(err)                                                                                \
    {                                                                                                        \
        FUNC_ENTER_COMMON(!H5_IS_API(FUNC));                                                                 \
        FUNC_ENTER_NOAPI_INIT(err)                                                                           \
        {

/* Use this macro for all "normal" package-level functions */
#define FUNC_ENTER_PACKAGE                                                                                   \
    {                                                                                                        \
        FUNC_ENTER_COMMON(H5_IS_PKG(FUNC));                                                                  \
        H5_PUSH_FUNC                                                                                         \
        {

/* Use this macro for package-level functions which propgate errors, but don't issue them */
#define FUNC_ENTER_PACKAGE_NOERR                                                                             \
    {                                                                                                        \
        FUNC_ENTER_COMMON_NOERR(H5_IS_PKG(FUNC));                                                            \
        H5_PUSH_FUNC                                                                                         \
        {

/* Use this macro for all "normal" staticly-scoped functions */
#define FUNC_ENTER_STATIC                                                                                    \
    {                                                                                                        \
        FUNC_ENTER_COMMON(H5_IS_PKG(FUNC));                                                                  \
        H5_PUSH_FUNC                                                                                         \
        {

/* Use this macro for staticly-scoped functions which propgate errors, but don't issue them */
#define FUNC_ENTER_STATIC_NOERR                                                                              \
    {                                                                                                        \
        FUNC_ENTER_COMMON_NOERR(H5_IS_PKG(FUNC));                                                            \
        H5_PUSH_FUNC                                                                                         \
        {

/* Use this macro for all non-API functions, which propagate errors, but don't issue them */
#define FUNC_ENTER_NOAPI_NOERR                                                                               \
    {                                                                                                        \
        FUNC_ENTER_COMMON_NOERR(!H5_IS_API(FUNC));                                                           \
        FUNC_ENTER_NOAPI_INIT(-)                                                                             \
        {

/*
 * Use this macro for non-API functions which fall into these categories:
 *      - static functions, since they must be called from a function in the
 *              interface, the library and interface must already be
 *              initialized.
 *      - functions which are called during library shutdown, since we don't
 *              want to re-initialize the library.
 */
#define FUNC_ENTER_NOAPI_NOINIT                                                                              \
    {                                                                                                        \
        FUNC_ENTER_COMMON(!H5_IS_API(FUNC));                                                                 \
        H5_PUSH_FUNC                                                                                         \
        {

/*
 * Use this macro for non-API functions which fall into these categories:
 *      - static functions, since they must be called from a function in the
 *              interface, the library and interface must already be
 *              initialized.
 *      - functions which are called during library shutdown, since we don't
 *              want to re-initialize the library.
 *      - functions that propagate, but don't issue errors
 */
#define FUNC_ENTER_NOAPI_NOINIT_NOERR                                                                        \
    {                                                                                                        \
        FUNC_ENTER_COMMON_NOERR(!H5_IS_API(FUNC));                                                           \
        H5_PUSH_FUNC                                                                                         \
        {

/*
 * Use this macro for non-API functions which fall into these categories:
 *      - functions which shouldn't push their name on the function stack
 *              (so far, just the H5CS routines themselves)
 *
 * This macro is used for functions which fit the above categories _and_
 * also don't use the 'FUNC' variable (i.e. don't push errors on the error stack)
 *
 */
#define FUNC_ENTER_NOAPI_NOERR_NOFS                                                                          \
    {                                                                                                        \
        FUNC_ENTER_COMMON_NOERR(!H5_IS_API(FUNC));                                                           \
        {

/*-------------------------------------------------------------------------
 * Purpose:  Register function exit for code profiling.  This should be
 *    the last statement executed by a function.
 *
 * Programmer:  Quincey Koziol
 *
 *-------------------------------------------------------------------------
 */
/* Threadsafety termination code for API routines */
#define FUNC_LEAVE_API_THREADSAFE                                                                            \
    H5_API_UNLOCK                                                                                            \
    H5_API_SET_CANCEL

#define FUNC_LEAVE_API(ret_value)                                                                            \
    FINISH_MPE_LOG                                                                                           \
    H5TRACE_RETURN(ret_value);                                                                               \
    H5_POP_FUNC                                                                                              \
    if (err_occurred)                                                                                        \
        (void)H5E_dump_api_stack(TRUE);                                                                      \
    FUNC_LEAVE_API_THREADSAFE                                                                                \
    return (ret_value);                                                                                      \
    } /*end scope from end of FUNC_ENTER*/                                                                   \
    }                                                                                                        \
    } /*end scope from beginning of FUNC_ENTER*/

/* Use this macro to match the FUNC_ENTER_API_NOFS macro */
#define FUNC_LEAVE_API_NOFS(ret_value)                                                                       \
    FINISH_MPE_LOG                                                                                           \
    H5TRACE_RETURN(ret_value);                                                                               \
    FUNC_LEAVE_API_THREADSAFE                                                                                \
    return (ret_value);                                                                                      \
    } /*end scope from end of FUNC_ENTER*/                                                                   \
    }                                                                                                        \
    } /*end scope from beginning of FUNC_ENTER*/

#define FUNC_LEAVE_NOAPI(ret_value)                                                                          \
    H5_POP_FUNC                                                                                              \
    return (ret_value);                                                                                      \
    } /*end scope from end of FUNC_ENTER*/                                                                   \
    } /*end scope from beginning of FUNC_ENTER*/

#define FUNC_LEAVE_NOAPI_VOID                                                                                \
    H5_POP_FUNC                                                                                              \
    return;                                                                                                  \
    } /*end scope from end of FUNC_ENTER*/                                                                   \
    } /*end scope from beginning of FUNC_ENTER*/

/*
 * Use this macro for non-API functions which fall into these categories:
 *      - functions which didn't push their name on the function stack
 *              (so far, just the H5CS routines themselves)
 */
#define FUNC_LEAVE_NOAPI_NOFS(ret_value)                                                                     \
    return (ret_value);                                                                                      \
    } /*end scope from end of FUNC_ENTER*/                                                                   \
    } /*end scope from beginning of FUNC_ENTER*/

/* Compile-time "assert" macro */
#define HDcompile_assert(e) ((void)sizeof(char[!!(e) ? 1 : -1]))
/* Variants that are correct, but generate compile-time warnings in some circumstances:
  #define HDcompile_assert(e)     do { enum { compile_assert__ = 1 / (e) }; } while(0)
  #define HDcompile_assert(e)     do { typedef struct { unsigned int b: (e); } x; } while(0)
*/

/* Private functions, not part of the publicly documented API */
H5_DLL herr_t H5_init_library(void);
H5_DLL void   H5_term_library(void);

/* Functions to terminate interfaces */
H5_DLL int H5A_term_interface(void);
H5_DLL int H5AC_term_interface(void);
H5_DLL int H5D_term_interface(void);
H5_DLL int H5E_term_interface(void);
H5_DLL int H5F_term_interface(void);
H5_DLL int H5FS_term_interface(void);
H5_DLL int H5G_term_interface(void);
H5_DLL int H5I_term_interface(void);
H5_DLL int H5L_term_interface(void);
H5_DLL int H5P_term_interface(void);
H5_DLL int H5PL_term_interface(void);
H5_DLL int H5R_term_interface(void);
H5_DLL int H5S_term_interface(void);
H5_DLL int H5T_term_interface(void);
H5_DLL int H5Z_term_interface(void);

/* Checksum functions */
H5_DLL uint32_t H5_checksum_fletcher32(const void *data, size_t len);
H5_DLL uint32_t H5_checksum_crc(const void *data, size_t len);
H5_DLL uint32_t H5_checksum_lookup3(const void *data, size_t len, uint32_t initval);
H5_DLL uint32_t H5_checksum_metadata(const void *data, size_t len, uint32_t initval);
H5_DLL uint32_t H5_hash_string(const char *str);

/* Time related routines */
H5_DLL time_t H5_make_time(struct tm *tm);
H5_DLL void   H5_nanosleep(uint64_t nanosec);
H5_DLL double H5_get_time(void);

/* Functions for building paths, etc. */
H5_DLL herr_t H5_build_extpath(const char *name, char **extpath /*out*/);
H5_DLL herr_t H5_combine_path(const char *path1, const char *path2, char **full_name /*out*/);

/* Functions for debugging */
H5_DLL herr_t H5_buffer_dump(FILE *stream, int indent, const uint8_t *buf, const uint8_t *marker,
                             size_t buf_offset, size_t buf_size);

#endif /* H5private_H */
