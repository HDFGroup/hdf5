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

#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <float.h>
#include <math.h>
#include <setjmp.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

/* POSIX headers */
#ifdef H5_HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#ifdef H5_HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef H5_HAVE_PWD_H
#include <pwd.h>
#endif
#ifdef H5_HAVE_WAITPID
#include <sys/wait.h>
#endif

/* Include the Pthreads header, if necessary */
#if defined(H5_HAVE_THREADSAFE) && defined(H5_HAVE_PTHREAD_H)
#include <pthread.h>
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
 * flock() in sys/file.h is used for the implementation of file locking.
 */
#if defined(H5_HAVE_FLOCK) && defined(H5_HAVE_SYS_FILE_H)
#include <sys/file.h>
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
 * Dynamic library handling.  These are needed for dynamically loading I/O
 * filters and VFDs.
 */
#ifdef H5_HAVE_DLFCN_H
#include <dlfcn.h>
#endif
#ifdef H5_HAVE_DIRENT_H
#include <dirent.h>
#endif

/* Define the default VFD for this platform.  Since the removal of the
 * Windows VFD, this is sec2 for all platforms.
 *
 * Note well: if you change the default, then be sure to change
 * H5_default_vfd_init() to call that default's initializer.  Also,
 * make sure that the initializer for each *non*-default VFD calls
 * H5_init_library(); also, make sure that the initializer for default
 * VFD does *not* call H5_init_library().
 */
#define H5_DEFAULT_VFD H5FD_SEC2

/* Define the default VOL driver */
#define H5_DEFAULT_VOL H5VL_NATIVE

#ifdef H5_HAVE_WIN32_API

/* The following two defines must be before any windows headers are included */
#define WIN32_LEAN_AND_MEAN /* Exclude rarely-used stuff from Windows headers */
#define NOGDI               /* Exclude Graphic Display Interface macros */

#include <windows.h>

#include <direct.h>   /* For _getcwd() */
#include <io.h>       /* POSIX I/O */
#include <winsock2.h> /* For GetUserName() */

#ifdef H5_HAVE_THREADSAFE
#include <process.h> /* For _beginthread() */
#endif

#endif /*H5_HAVE_WIN32_API*/

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
    static int eventa(__func__) = -1;                                                                        \
    static int eventb(__func__) = -1;                                                                        \
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
        snprintf(p_event_start, sizeof(p_event_start), "start %s", __func__);                                \
        if (eventa(__func__) == -1 && eventb(__func__) == -1) {                                              \
            const char *p_color = "red";                                                                     \
            eventa(__func__)    = MPE_Log_get_event_number();                                                \
            eventb(__func__)    = MPE_Log_get_event_number();                                                \
            MPE_Describe_state(eventa(__func__), eventb(__func__), __func__, p_color);                       \
        }                                                                                                    \
        MPE_Log_event(eventa(__func__), 0, p_event_start);                                                   \
    }

/*------------------------------------------------------------------------
 * Purpose:   Finish the collection of MPE log information for a function.
 *            It should be after the actual function's process.
 *
 * Programmer: Long Wang
 */
#define FINISH_MPE_LOG                                                                                       \
    if (H5_MPEinit_g) {                                                                                      \
        MPE_Log_event(eventb(__func__), 0, __func__);                                                        \
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
 *
 * Note that Solaris Studio supports attribute, but does not support the
 * attributes we use.
 *
 * When using H5_ATTR_FALLTHROUGH, you should also include a comment that
 * says FALLTHROUGH to reduce warnings on compilers that don't use
 * attributes but do respect fall-through comments.
 *
 * H5_ATTR_CONST is redefined in tools/h5repack/dynlib_rpk.c to quiet
 * gcc warnings (it has to use the public API and can't include this
 * file). Be sure to update that file if the #ifdefs change here.
 */
#if defined(H5_HAVE_ATTRIBUTE) && !defined(__SUNPRO_C)
#define H5_ATTR_FORMAT(X, Y, Z) __attribute__((format(X, Y, Z)))
#define H5_ATTR_UNUSED          __attribute__((unused))
#ifdef H5_HAVE_PARALLEL
#define H5_ATTR_PARALLEL_UNUSED __attribute__((unused))
#define H5_ATTR_PARALLEL_USED   /*void*/
#else
#define H5_ATTR_PARALLEL_UNUSED /*void*/
#define H5_ATTR_PARALLEL_USED   __attribute__((unused))
#endif
#ifdef H5_NO_DEPRECATED_SYMBOLS
#define H5_ATTR_DEPRECATED_USED H5_ATTR_UNUSED
#else                           /* H5_NO_DEPRECATED_SYMBOLS */
#define H5_ATTR_DEPRECATED_USED /*void*/
#endif                          /* H5_NO_DEPRECATED_SYMBOLS */
#ifdef H5_DEBUG_API
#define H5_ATTR_DEBUG_API_USED /*void*/
#else                          /* H5_DEBUG_API */
#define H5_ATTR_DEBUG_API_USED H5_ATTR_UNUSED
#endif /* H5_DEBUG_API */
#ifndef NDEBUG
#define H5_ATTR_NDEBUG_UNUSED /*void*/
#else                         /* NDEBUG */
#define H5_ATTR_NDEBUG_UNUSED H5_ATTR_UNUSED
#endif /* NDEBUG */
#define H5_ATTR_NORETURN __attribute__((noreturn))
#define H5_ATTR_CONST    __attribute__((const))
#define H5_ATTR_PURE     __attribute__((pure))
#if defined(__clang__) || defined(__GNUC__) && __GNUC__ >= 7 && !defined(__INTEL_COMPILER)
#define H5_ATTR_FALLTHROUGH __attribute__((fallthrough));
#else
#define H5_ATTR_FALLTHROUGH /* FALLTHROUGH */
#endif
#else
#define H5_ATTR_FORMAT(X, Y, Z) /*void*/
#define H5_ATTR_UNUSED          /*void*/
#define H5_ATTR_NDEBUG_UNUSED   /*void*/
#define H5_ATTR_DEBUG_API_USED  /*void*/
#define H5_ATTR_DEPRECATED_USED /*void*/
#define H5_ATTR_PARALLEL_UNUSED /*void*/
#define H5_ATTR_PARALLEL_USED   /*void*/
#define H5_ATTR_NORETURN        /*void*/
#define H5_ATTR_CONST           /*void*/
#define H5_ATTR_PURE            /*void*/
#define H5_ATTR_FALLTHROUGH     /*void*/
#endif

/*
 * Networking headers used by the mirror VFD and related tests and utilities.
 */
#ifdef H5_HAVE_ARPA_INET_H
#include <arpa/inet.h>
#endif
#ifdef H5_HAVE_NETDB_H
#include <netdb.h>
#endif
#ifdef H5_HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif
#ifdef H5_HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif

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
#define FALSE false
#endif
#ifndef TRUE
#define TRUE true
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

#ifdef H5_HAVE_PARALLEL

/* Define a type for safely sending size_t values with MPI */
#if SIZE_MAX == UCHAR_MAX
#define H5_SIZE_T_AS_MPI_TYPE MPI_UNSIGNED_CHAR
#elif SIZE_MAX == USHRT_MAX
#define H5_SIZE_T_AS_MPI_TYPE MPI_UNSIGNED_SHORT
#elif SIZE_MAX == UINT_MAX
#define H5_SIZE_T_AS_MPI_TYPE MPI_UNSIGNED
#elif SIZE_MAX == ULONG_MAX
#define H5_SIZE_T_AS_MPI_TYPE MPI_UNSIGNED_LONG
#elif SIZE_MAX == ULLONG_MAX
#define H5_SIZE_T_AS_MPI_TYPE MPI_UNSIGNED_LONG_LONG
#else
#error "no suitable MPI type for size_t"
#endif

#endif /* H5_HAVE_PARALLEL */

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

/* Represents an empty asynchronous request handle.
 * Used in the VOL code.
 */
#define H5_REQUEST_NULL NULL

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
#define H5_FLT_ABS_EQUAL(X, Y)  (fabsf((X) - (Y)) < FLT_EPSILON)
#define H5_DBL_ABS_EQUAL(X, Y)  (fabs((X) - (Y)) < DBL_EPSILON)
#define H5_LDBL_ABS_EQUAL(X, Y) (fabsl((X) - (Y)) < LDBL_EPSILON)

#define H5_FLT_REL_EQUAL(X, Y, M)  (fabsf(((Y) - (X)) / (X)) < (M))
#define H5_DBL_REL_EQUAL(X, Y, M)  (fabs(((Y) - (X)) / (X)) < (M))
#define H5_LDBL_REL_EQUAL(X, Y, M) (fabsl(((Y) - (X)) / (X)) < (M))

/* KiB, MiB, GiB, TiB, PiB, EiB - Used in profiling and timing code */
#define H5_KB (1024.0F)
#define H5_MB (1024.0F * 1024.0F)
#define H5_GB (1024.0F * 1024.0F * 1024.0F)
#define H5_TB (1024.0F * 1024.0F * 1024.0F * 1024.0F)
#define H5_PB (1024.0F * 1024.0F * 1024.0F * 1024.0F * 1024.0F)
#define H5_EB (1024.0F * 1024.0F * 1024.0F * 1024.0F * 1024.0F * 1024.0F)

#ifndef H5_HAVE_FLOCK
/* flock() operations. Used in the source so we have to define them when
 * the call is not available (e.g.: Windows). These should NOT be used
 * with system-provided flock() calls since the values will come from the
 * header file.
 */
#define LOCK_SH 0x01
#define LOCK_EX 0x02
#define LOCK_NB 0x04
#define LOCK_UN 0x08
#endif /* H5_HAVE_FLOCK */

/* Macros for enabling/disabling particular GCC / clang warnings
 *
 * These are duplicated in H5FDmulti.c (we don't want to put them in the
 * public header and the multi VFD can't use private headers). If you make
 * changes here, be sure to update those as well.
 *
 * (see the following web-sites for more info:
 *      http://www.dbp-consulting.com/tutorials/SuppressingGCCWarnings.html
 *      http://gcc.gnu.org/onlinedocs/gcc/Diagnostic-Pragmas.html#Diagnostic-Pragmas
 */
#define H5_DIAG_JOINSTR(x, y) x y
#define H5_DIAG_DO_PRAGMA(x)  _Pragma(#x)
#define H5_DIAG_PRAGMA(x)     H5_DIAG_DO_PRAGMA(GCC diagnostic x)

#define H5_DIAG_OFF(x) H5_DIAG_PRAGMA(push) H5_DIAG_PRAGMA(ignored H5_DIAG_JOINSTR("-W", x))
#define H5_DIAG_ON(x)  H5_DIAG_PRAGMA(pop)

/* Macros for enabling/disabling particular GCC-only warnings.
 * These pragmas are only implemented usefully in gcc 4.6+
 */
#if (((__GNUC__ * 100) + __GNUC_MINOR__) >= 406)
#define H5_GCC_DIAG_OFF(x) H5_DIAG_OFF(x)
#define H5_GCC_DIAG_ON(x)  H5_DIAG_ON(x)
#else
#define H5_GCC_DIAG_OFF(x)
#define H5_GCC_DIAG_ON(x)
#endif

/* Macros for enabling/disabling particular clang-only warnings.
 */
#if defined(__clang__)
#define H5_CLANG_DIAG_OFF(x) H5_DIAG_OFF(x)
#define H5_CLANG_DIAG_ON(x)  H5_DIAG_ON(x)
#else
#define H5_CLANG_DIAG_OFF(x)
#define H5_CLANG_DIAG_ON(x)
#endif

/* Macros for enabling/disabling particular GCC / clang warnings.
 * These macros should be used for warnings supported by both gcc and clang.
 */
#if (((__GNUC__ * 100) + __GNUC_MINOR__) >= 406) || defined(__clang__)
#define H5_GCC_CLANG_DIAG_OFF(x) H5_DIAG_OFF(x)
#define H5_GCC_CLANG_DIAG_ON(x)  H5_DIAG_ON(x)
#else
#define H5_GCC_CLANG_DIAG_OFF(x)
#define H5_GCC_CLANG_DIAG_ON(x)
#endif

/* Function pointer typedef for qsort */
typedef int (*H5_sort_func_cb_t)(const void *, const void *);

/* Typedefs and functions for timing certain parts of the library. */

/* A set of elapsed/user/system times emitted as a time point by the
 * platform-independent timers.
 */
typedef struct {
    double user;    /* User time in seconds */
    double system;  /* System time in seconds */
    double elapsed; /* Elapsed (wall clock) time in seconds */
} H5_timevals_t;

/* Timer structure for platform-independent timers */
typedef struct {
    H5_timevals_t initial;        /* Current interval start time */
    H5_timevals_t final_interval; /* Last interval elapsed time */
    H5_timevals_t total;          /* Total elapsed time for all intervals */
    hbool_t       is_running;     /* Whether timer is running */
} H5_timer_t;

/* Returns library bandwidth as a pretty string */
H5_DLL void H5_bandwidth(char *buf /*out*/, double nbytes, double nseconds);

/* Timer functionality */
H5_DLL time_t   H5_now(void);
H5_DLL uint64_t H5_now_usec(void);
H5_DLL herr_t   H5_timer_init(H5_timer_t *timer /*in,out*/);
H5_DLL herr_t   H5_timer_start(H5_timer_t *timer /*in,out*/);
H5_DLL herr_t   H5_timer_stop(H5_timer_t *timer /*in,out*/);
H5_DLL herr_t   H5_timer_get_times(H5_timer_t timer, H5_timevals_t *times /*in,out*/);
H5_DLL herr_t   H5_timer_get_total_times(H5_timer_t timer, H5_timevals_t *times /*in,out*/);
H5_DLL char *   H5_timer_get_time_string(double seconds);

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

#define H5_SIZEOF_H5_STAT_SIZE_T H5_SIZEOF_OFF_T

/* Put all Windows-specific definitions in H5win32defs.h so we
 * can (mostly) assume a POSIX platform. Not all of the POSIX calls
 * will have a Windows equivalent so some #ifdef protection is still
 * necessary (e.g., fork()).
 */
#include "H5win32defs.h"

/* Platform-independent definitions for struct stat and off_t */
#ifndef H5_HAVE_WIN32_API
/* These definitions differ in Windows and are defined in
 * H5win32defs for that platform.
 */
typedef struct stat h5_stat_t;
typedef off_t       h5_stat_size_t;
#define HDoff_t off_t
#endif

/*
 * Redefine all the POSIX functions exclusive of functions that are
 * also standard C functions.  We should never see an undecorated POSIX
 * function (or any other non-HDF5 function) in the source.
 */
#ifndef HDaccept
#define HDaccept(A, B, C) accept((A), (B), (C))
#endif
#ifndef HDaccess
#define HDaccess(F, M) access(F, M)
#endif
#ifndef HDbind
#define HDbind(A, B, C) bind((A), (B), (C))
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
#ifndef HDclock_gettime
#define HDclock_gettime(CID, TS) clock_gettime(CID, TS)
#endif
#ifndef HDclose
#define HDclose(F) close(F)
#endif
#ifndef HDclosedir
#define HDclosedir(D) closedir(D)
#endif
#ifndef HDconnect
#define HDconnect(A, B, C) connect((A), (B), (C))
#endif
#ifndef HDcreat
#define HDcreat(S, M) creat(S, M)
#endif
#ifndef HDctermid
#define HDctermid(S) ctermid(S)
#endif
#ifndef HDdup
#define HDdup(F) dup(F)
#endif
#ifndef HDdup2
#define HDdup2(F, I) dup2(F, I)
#endif
#ifndef HDexecv
#define HDexecv(S, AV) execv(S, AV)
#endif
#ifndef HDexecve
#define HDexecve(S, AV, E) execve(S, AV, E)
#endif
#ifndef HDexecvp
#define HDexecvp(S, AV) execvp(S, AV)
#endif
#ifndef HD_exit
#define HD_exit(N) _exit(N)
#endif
#ifndef HDfcntl
#define HDfcntl(F, C, ...) fcntl(F, C, __VA_ARGS__)
#endif
#ifndef HDfdopen
#define HDfdopen(N, S) fdopen(N, S)
#endif
#ifndef HDfileno
#define HDfileno(F) fileno(F)
#endif
#ifndef HDfpathconf
#define HDfpathconf(F, N) fpathconf(F, N)
#endif
#ifndef HDfstat
#define HDfstat(F, B) fstat(F, B)
#endif
#ifndef HDftruncate
#define HDftruncate(F, L) ftruncate(F, L)
#endif
#ifndef HDgetcwd
#define HDgetcwd(S, Z) getcwd(S, Z)
#endif
#ifndef HDgetegid
#define HDgetegid() getegid()
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
#ifndef HDgethostbyaddr
#define HDgethostbyaddr(A, B, C) gethostbyaddr((A), (B), (C))
#endif
#ifndef HDgethostname
#define HDgethostname(N, L) gethostname(N, L)
#endif
#ifndef HDgetlogin
#define HDgetlogin() getlogin()
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
#ifndef HDgetuid
#define HDgetuid() getuid()
#endif
#ifndef HDhtonl
#define HDhtonl(X) htonl((X))
#endif
#ifndef HDhtons
#define HDhtons(X) htons((X))
#endif
#ifndef HDinet_addr
#define HDinet_addr(C) inet_addr((C))
#endif
#ifndef HDinet_ntoa
#define HDinet_ntoa(C) inet_ntoa((C))
#endif
#ifndef HDisatty
#define HDisatty(F) isatty(F)
#endif
#ifndef HDkill
#define HDkill(P, S) kill(P, S)
#endif
#ifndef HDlink
#define HDlink(OLD, NEW) link(OLD, NEW)
#endif
#ifndef HDlisten
#define HDlisten(A, B) listen((A), (B))
#endif
#ifndef HDlseek
#define HDlseek(F, O, W) lseek(F, O, W)
#endif
#ifndef HDlstat
#define HDlstat(S, B) lstat(S, B)
#endif
#ifndef HDposix_memalign
#define HDposix_memalign(P, A, Z) posix_memalign(P, A, Z)
#endif
#ifndef HDmkdir
#define HDmkdir(S, M) mkdir(S, M)
#endif
#ifndef HDmkfifo
#define HDmkfifo(S, M) mkfifo(S, M)
#endif
#ifndef HDnanosleep
#define HDnanosleep(N, O) nanosleep(N, O)
#endif
#ifndef HDntohl
#define HDntohl(A) ntohl((A))
#endif
#ifndef HDntohs
#define HDntohs(A) ntohs((A))
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
#ifndef HDpipe
#define HDpipe(F) pipe(F)
#endif
#ifndef HDpread
#define HDpread(F, B, C, O) pread(F, B, C, O)
#endif
#ifndef HDpthread_attr_destroy
#define HDpthread_attr_destroy(A) pthread_attr_destroy(A)
#endif
#ifndef HDpthread_attr_init
#define HDpthread_attr_init(A) pthread_attr_init(A)
#endif
#ifndef HDpthread_attr_setscope
#define HDpthread_attr_setscope(A, S) pthread_attr_setscope(A, S)
#endif
#ifndef HDpthread_cond_init
#define HDpthread_cond_init(C, A) pthread_cond_init(C, A)
#endif
#ifndef HDpthread_cond_signal
#define HDpthread_cond_signal(C) pthread_cond_signal(C)
#endif
#ifndef HDpthread_cond_wait
#define HDpthread_cond_wait(C, M) pthread_cond_wait(C, M)
#endif
#ifndef HDpthread_create
#define HDpthread_create(R, A, F, U) pthread_create(R, A, F, U)
#endif
#ifndef HDpthread_equal
#define HDpthread_equal(T1, T2) pthread_equal(T1, T2)
#endif
#ifndef HDpthread_getspecific
#define HDpthread_getspecific(K) pthread_getspecific(K)
#endif
#ifndef HDpthread_join
#define HDpthread_join(T, V) pthread_join(T, V)
#endif
#ifndef HDpthread_key_create
#define HDpthread_key_create(K, D) pthread_key_create(K, D)
#endif
#ifndef HDpthread_mutex_init
#define HDpthread_mutex_init(M, A) pthread_mutex_init(M, A)
#endif
#ifndef HDpthread_mutex_lock
#define HDpthread_mutex_lock(M) pthread_mutex_lock(M)
#endif
#ifndef HDpthread_mutex_unlock
#define HDpthread_mutex_unlock(M) pthread_mutex_unlock(M)
#endif
#ifndef HDpthread_self
#define HDpthread_self() pthread_self()
#endif
#ifndef HDpthread_setcancelstate
#define HDpthread_setcancelstate(N, O) pthread_setcancelstate(N, O)
#endif
#ifndef HDpthread_setspecific
#define HDpthread_setspecific(K, V) pthread_setspecific(K, V)
#endif
#ifndef HDpwrite
#define HDpwrite(F, B, C, O) pwrite(F, B, C, O)
#endif
#ifndef HDread
#define HDread(F, M, Z) read(F, M, Z)
#endif
#ifndef HDrealpath
#define HDrealpath(F1, F2) realpath(F1, F2)
#endif
#ifndef HDrewinddir
#define HDrewinddir(D) rewinddir(D)
#endif
#ifndef HDrmdir
#define HDrmdir(S) rmdir(S)
#endif
#ifndef HDselect
#define HDselect(N, RD, WR, ER, T) select(N, RD, WR, ER, T)
#endif
#ifndef HDsetenv
#define HDsetenv(N, V, O) setenv(N, V, O)
#endif
#ifndef HDsetgid
#define HDsetgid(G) setgid(G)
#endif
#ifndef HDsetpgid
#define HDsetpgid(P, PG) setpgid(P, PG)
#endif
#ifndef HDsetsid
#define HDsetsid() setsid()
#endif
#ifndef HDsetsockopt
#define HDsetsockopt(A, B, C, D, E) setsockopt((A), (B), (C), (D), (E))
#endif
#ifndef HDsetuid
#define HDsetuid(U) setuid(U)
#endif
#ifndef HDshutdown
#define HDshutdown(A, B) shutdown((A), (B))
#endif
#ifndef HDsigaction
#define HDsigaction(S, A, O) sigaction((S), (A), (O))
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
#ifndef HDsleep
#define HDsleep(S) sleep(S)
#endif
#ifndef HDsocket
#define HDsocket(A, B, C) socket((A), (B), (C))
#endif
#ifndef HDstat
#define HDstat(S, B) stat(S, B)
#endif
#ifndef HDstrdup
#define HDstrdup(S) strdup(S)
#endif
#ifndef HDstrtok_r
#define HDstrtok_r(X, Y, Z) strtok_r(X, Y, Z)
#endif
#ifndef HDsymlink
#define HDsymlink(F1, F2) symlink(F1, F2)
#endif
#ifndef HDsysconf
#define HDsysconf(N) sysconf(N)
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
#ifndef HDunlink
#define HDunlink(S) unlink(S)
#endif
#ifndef HDunsetenv
#define HDunsetenv(S) unsetenv(S)
#endif
#ifndef HDwait
#define HDwait(W) wait(W)
#endif
#ifndef HDwaitpid
#define HDwaitpid(P, W, O) waitpid(P, W, O)
#endif
#ifndef HDwrite
#define HDwrite(F, M, Z) write(F, M, Z)
#endif

/*
 * Oddball C99 functions: these have strange HDF5 implementations or else
 * they're unlikely to be universally available in quite the form we
 * expect.
 */
#ifndef HDfseek
#define HDfseek(F, O, W) fseeko(F, O, W)
#endif

/* Non-C99 oddball function: this has a strange HDF5 implementation
 * that is unlikely to meet a developer's expectations.
 */
#ifndef HDgetdcwd
#define HDgetdcwd(D, S, Z) getcwd(S, Z)
#endif

/* Windows only - set to zero on other systems */
#ifndef HDgetdrive
#define HDgetdrive() 0
#endif

/* Don't define HDgets - gets() was deprecated in C99 and removed in C11 */
#ifdef HDgets
#undef HDgets
#endif

/* Redefine all Linux & *BSD functions. */
#ifndef HDasprintf
#define HDasprintf asprintf /*varargs*/
#endif
#ifndef HDcuserid
#define HDcuserid(S) cuserid(S)
#endif
/* Since flock is so prevalent, always build these functions
 * when possible to avoid them becoming dead code.
 */
#ifdef H5_HAVE_FCNTL
H5_DLL int Pflock(int fd, int operation);
#endif
H5_DLL H5_ATTR_CONST int Nflock(int fd, int operation);

#ifndef HDflock
/* NOTE: flock(2) is not present on all POSIX systems.
 * If it is not present, we try a flock() equivalent based on
 * fcntl(2), then fall back to a function that always succeeds
 * if it is not present at all (Windows uses a separate Wflock()
 * function).
 */
#if defined(H5_HAVE_FLOCK)
#define HDflock(F, L) flock(F, L)
#elif defined(H5_HAVE_FCNTL)
#define HDflock(F, L) Pflock(F, L)
#else
#define HDflock(F, L) Nflock(F, L)
#endif

#endif /* HDflock */
#ifndef HDgetpgrp
#define HDgetpgrp() getpgrp()
#endif
#ifndef HDgetrusage
#define HDgetrusage(X, S) getrusage(X, S)
#endif

#ifndef HDvasprintf
#ifdef H5_HAVE_VASPRINTF
#define HDvasprintf(RET, FMT, A) vasprintf(RET, FMT, A)
#else
H5_DLL int HDvasprintf(char **bufp, const char *fmt, va_list _ap);
#endif
#endif

#ifndef HDreaddir
#define HDreaddir(D) readdir(D)
#endif

/* Obsolete in POSIX. */
#ifndef HDgettimeofday
#define HDgettimeofday(S, P) gettimeofday(S, P)
#endif

/*
 * C99's rand() and srand() are problematic and should never be used.
 */

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

#ifndef HDstrcasecmp
#define HDstrcasecmp(X, Y) strcasecmp(X, Y)
#endif

/* Marked obsolete in POSIX. */
#ifndef HDutime
#define HDutime(S, T) utime(S, T)
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
        assert((var) == (vartype)_tmp_overflow);                                                             \
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
        assert(_tmp_src == (srctype)_tmp_dst);                                                               \
        (dst) = _tmp_dst;                                                                                    \
    }

#define ASSIGN_TO_LARGER_SIZE_SAME_SIGNED(dst, dsttype, src, srctype) (dst) = (dsttype)(src);

#define ASSIGN_TO_LARGER_SIZE_SIGNED_TO_UNSIGNED(dst, dsttype, src, srctype)                                 \
    {                                                                                                        \
        srctype _tmp_src = (srctype)(src);                                                                   \
        dsttype _tmp_dst = (dsttype)(_tmp_src);                                                              \
        assert(_tmp_src >= 0);                                                                               \
        assert(_tmp_src == (srctype)_tmp_dst);                                                               \
        (dst) = _tmp_dst;                                                                                    \
    }

#define ASSIGN_TO_LARGER_SIZE_UNSIGNED_TO_SIGNED(dst, dsttype, src, srctype) (dst) = (dsttype)(src);

#define ASSIGN_TO_SAME_SIZE_UNSIGNED_TO_SIGNED(dst, dsttype, src, srctype)                                   \
    {                                                                                                        \
        srctype _tmp_src = (srctype)(src);                                                                   \
        dsttype _tmp_dst = (dsttype)(_tmp_src);                                                              \
        assert(_tmp_dst >= 0);                                                                               \
        assert(_tmp_src == (srctype)_tmp_dst);                                                               \
        (dst) = _tmp_dst;                                                                                    \
    }

#define ASSIGN_TO_SAME_SIZE_SIGNED_TO_UNSIGNED(dst, dsttype, src, srctype)                                   \
    {                                                                                                        \
        srctype _tmp_src = (srctype)(src);                                                                   \
        dsttype _tmp_dst = (dsttype)(_tmp_src);                                                              \
        assert(_tmp_src >= 0);                                                                               \
        assert(_tmp_src == (srctype)_tmp_dst);                                                               \
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
#define H5_DIR_SLASH_SEPC      '/'
#define H5_DIR_SEPC            '\\'
#define H5_DIR_SEPS            "\\"
#define H5_CHECK_DELIMITER(SS) ((SS == H5_DIR_SEPC) || (SS == H5_DIR_SLASH_SEPC))
#define H5_CHECK_ABSOLUTE(NAME)                                                                              \
    ((isalpha((int)(unsigned char)NAME[0])) && (NAME[1] == ':') && (H5_CHECK_DELIMITER(NAME[2])))
#define H5_CHECK_ABS_DRIVE(NAME) ((isalpha((int)(unsigned char)NAME[0])) && (NAME[1] == ':'))
#define H5_CHECK_ABS_PATH(NAME)  (H5_CHECK_DELIMITER(NAME[0]))

#define H5_GET_LAST_DELIMITER(NAME, ptr)                                                                     \
    {                                                                                                        \
        char *slash, *backslash;                                                                             \
                                                                                                             \
        slash     = strrchr(NAME, H5_DIR_SLASH_SEPC);                                                        \
        backslash = strrchr(NAME, H5_DIR_SEPC);                                                              \
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
#define H5_GET_LAST_DELIMITER(NAME, ptr) ptr = strrchr(NAME, H5_DIR_SEPC);

#endif /* H5_HAVE_WINDOW_PATH */

#define H5_COLON_SEPC ':'

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
    H5_PKG_M,  /* Maps                     */
    H5_PKG_MF, /* File memory management   */
    H5_PKG_MM, /* Core memory management   */
    H5_PKG_O,  /* Object headers           */
    H5_PKG_P,  /* Property lists           */
    H5_PKG_S,  /* Dataspaces               */
    H5_PKG_T,  /* Datatypes                */
    H5_PKG_V,  /* Vector functions         */
    H5_PKG_VL, /* VOL functions            */
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

#ifdef H5_HAVE_PARALLEL
extern hbool_t H5_coll_api_sanity_check_g;
#endif /* H5_HAVE_PARALLEL */

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
    CALLTIME = H5_trace(NULL, __func__, T)
#define H5TRACE1(R, T, A0)                                                                                   \
    RTYPE    = R;                                                                                            \
    CALLTIME = H5_trace(NULL, __func__, T, #A0, A0)
#define H5TRACE2(R, T, A0, A1)                                                                               \
    RTYPE    = R;                                                                                            \
    CALLTIME = H5_trace(NULL, __func__, T, #A0, A0, #A1, A1)
#define H5TRACE3(R, T, A0, A1, A2)                                                                           \
    RTYPE    = R;                                                                                            \
    CALLTIME = H5_trace(NULL, __func__, T, #A0, A0, #A1, A1, #A2, A2)
#define H5TRACE4(R, T, A0, A1, A2, A3)                                                                       \
    RTYPE    = R;                                                                                            \
    CALLTIME = H5_trace(NULL, __func__, T, #A0, A0, #A1, A1, #A2, A2, #A3, A3)
#define H5TRACE5(R, T, A0, A1, A2, A3, A4)                                                                   \
    RTYPE    = R;                                                                                            \
    CALLTIME = H5_trace(NULL, __func__, T, #A0, A0, #A1, A1, #A2, A2, #A3, A3, #A4, A4)
#define H5TRACE6(R, T, A0, A1, A2, A3, A4, A5)                                                               \
    RTYPE    = R;                                                                                            \
    CALLTIME = H5_trace(NULL, __func__, T, #A0, A0, #A1, A1, #A2, A2, #A3, A3, #A4, A4, #A5, A5)
#define H5TRACE7(R, T, A0, A1, A2, A3, A4, A5, A6)                                                           \
    RTYPE    = R;                                                                                            \
    CALLTIME = H5_trace(NULL, __func__, T, #A0, A0, #A1, A1, #A2, A2, #A3, A3, #A4, A4, #A5, A5, #A6, A6)
#define H5TRACE8(R, T, A0, A1, A2, A3, A4, A5, A6, A7)                                                       \
    RTYPE = R;                                                                                               \
    CALLTIME =                                                                                               \
        H5_trace(NULL, __func__, T, #A0, A0, #A1, A1, #A2, A2, #A3, A3, #A4, A4, #A5, A5, #A6, A6, #A7, A7)
#define H5TRACE9(R, T, A0, A1, A2, A3, A4, A5, A6, A7, A8)                                                   \
    RTYPE    = R;                                                                                            \
    CALLTIME = H5_trace(NULL, __func__, T, #A0, A0, #A1, A1, #A2, A2, #A3, A3, #A4, A4, #A5, A5, #A6, A6,    \
                        #A7, A7, #A8, A8)
#define H5TRACE10(R, T, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9)                                              \
    RTYPE    = R;                                                                                            \
    CALLTIME = H5_trace(NULL, __func__, T, #A0, A0, #A1, A1, #A2, A2, #A3, A3, #A4, A4, #A5, A5, #A6, A6,    \
                        #A7, A7, #A8, A8, #A9, A9)
#define H5TRACE11(R, T, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)                                         \
    RTYPE    = R;                                                                                            \
    CALLTIME = H5_trace(NULL, __func__, T, #A0, A0, #A1, A1, #A2, A2, #A3, A3, #A4, A4, #A5, A5, #A6, A6,    \
                        #A7, A7, #A8, A8, #A9, A9, #A10, A10)
#define H5TRACE12(R, T, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11)                                    \
    RTYPE    = R;                                                                                            \
    CALLTIME = H5_trace(NULL, __func__, T, #A0, A0, #A1, A1, #A2, A2, #A3, A3, #A4, A4, #A5, A5, #A6, A6,    \
                        #A7, A7, #A8, A8, #A9, A9, #A10, A10, #A11, A11)

#define H5TRACE_RETURN(V)                                                                                    \
    if (RTYPE) {                                                                                             \
        H5_trace(&CALLTIME, __func__, RTYPE, NULL, V);                                                       \
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

/* Argument tracing macros (defined all the time) */
#define H5ARG_TRACE0(C, T)                         C, T
#define H5ARG_TRACE1(C, T, A0)                     C, T, #A0, A0
#define H5ARG_TRACE2(C, T, A0, A1)                 C, T, #A0, A0, #A1, A1
#define H5ARG_TRACE3(C, T, A0, A1, A2)             C, T, #A0, A0, #A1, A1, #A2, A2
#define H5ARG_TRACE4(C, T, A0, A1, A2, A3)         C, T, #A0, A0, #A1, A1, #A2, A2, #A3, A3
#define H5ARG_TRACE5(C, T, A0, A1, A2, A3, A4)     C, T, #A0, A0, #A1, A1, #A2, A2, #A3, A3, #A4, A4
#define H5ARG_TRACE6(C, T, A0, A1, A2, A3, A4, A5) C, T, #A0, A0, #A1, A1, #A2, A2, #A3, A3, #A4, A4, #A5, A5
#define H5ARG_TRACE7(C, T, A0, A1, A2, A3, A4, A5, A6)                                                       \
    C, T, #A0, A0, #A1, A1, #A2, A2, #A3, A3, #A4, A4, #A5, A5, #A6, A6
#define H5ARG_TRACE8(C, T, A0, A1, A2, A3, A4, A5, A6, A7)                                                   \
    C, T, #A0, A0, #A1, A1, #A2, A2, #A3, A3, #A4, A4, #A5, A5, #A6, A6, #A7, A7
#define H5ARG_TRACE9(C, T, A0, A1, A2, A3, A4, A5, A6, A7, A8)                                               \
    C, T, #A0, A0, #A1, A1, #A2, A2, #A3, A3, #A4, A4, #A5, A5, #A6, A6, #A7, A7, #A8, A8
#define H5ARG_TRACE10(C, T, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9)                                          \
    C, T, #A0, A0, #A1, A1, #A2, A2, #A3, A3, #A4, A4, #A5, A5, #A6, A6, #A7, A7, #A8, A8, #A9, A9
#define H5ARG_TRACE11(C, T, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)                                     \
    C, T, #A0, A0, #A1, A1, #A2, A2, #A3, A3, #A4, A4, #A5, A5, #A6, A6, #A7, A7, #A8, A8, #A9, A9, #A10, A10
#define H5ARG_TRACE12(C, T, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11)                                \
    C, T, #A0, A0, #A1, A1, #A2, A2, #A3, A3, #A4, A4, #A5, A5, #A6, A6, #A7, A7, #A8, A8, #A9, A9, #A10,    \
        A10, #A11, A11

struct H5RS_str_t;
H5_DLL double H5_trace(const double *calltime, const char *func, const char *type, ...);
H5_DLL herr_t H5_trace_args(struct H5RS_str_t *rs, const char *type, va_list ap);

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
    ('_' != ((const char *)S)[2]    /* underscore at position 2     */                                       \
     && '_' != ((const char *)S)[3] /* underscore at position 3     */                                       \
     && !(                          /* NOT              */                                                   \
          ((const char *)S)[4]      /* pos 4 exists     */                                                   \
          && (isupper((int)(unsigned char)S[3]) || isdigit((int)(unsigned char)S[3])) /* pos 3 dig | uc   */ \
          && '_' == ((const char *)S)[4]                                              /* pos 4 underscore */ \
          ))

/* `S' is the name of a function which is being tested to check if it's */
/*      a public API function */
#define H5_IS_PUB(S)                                                                                         \
    (((isdigit((int)(unsigned char)S[1]) || isupper((int)(unsigned char)S[1])) &&                            \
      islower((int)(unsigned char)S[2])) ||                                                                  \
     ((isdigit((int)(unsigned char)S[2]) || isupper((int)(unsigned char)S[2])) &&                            \
      islower((int)(unsigned char)S[3])) ||                                                                  \
     (!S[4] || ((isdigit((int)(unsigned char)S[3]) || isupper((int)(unsigned char)S[3])) &&                  \
                islower((int)(unsigned char)S[4]))))

/* `S' is the name of a function which is being tested to check if it's */
/*      a private library function */
#define H5_IS_PRIV(S)                                                                                        \
    (((isdigit((int)(unsigned char)S[1]) || isupper((int)(unsigned char)S[1])) && '_' == S[2] &&             \
      islower((int)(unsigned char)S[3])) ||                                                                  \
     ((isdigit((int)(unsigned char)S[2]) || isupper((int)(unsigned char)S[2])) && '_' == S[3] &&             \
      islower((int)(unsigned char)S[4])) ||                                                                  \
     ((isdigit((int)(unsigned char)S[3]) || isupper((int)(unsigned char)S[3])) && '_' == S[4] &&             \
      islower((int)(unsigned char)S[5])))

/* `S' is the name of a function which is being tested to check if it's */
/*      a package private function */
#define H5_IS_PKG(S)                                                                                         \
    (((isdigit((int)(unsigned char)S[1]) || isupper((int)(unsigned char)S[1])) && '_' == S[2] &&             \
      '_' == S[3] && islower((int)(unsigned char)S[4])) ||                                                   \
     ((isdigit((int)(unsigned char)S[2]) || isupper((int)(unsigned char)S[2])) && '_' == S[3] &&             \
      '_' == S[4] && islower((int)(unsigned char)S[5])) ||                                                   \
     ((isdigit((int)(unsigned char)S[3]) || isupper((int)(unsigned char)S[3])) && '_' == S[4] &&             \
      '_' == S[5] && islower((int)(unsigned char)S[6])))

/* global library version information string */
extern char H5_lib_vers_info_g[];

#include "H5TSprivate.h"

/* Lock headers */
#ifdef H5_HAVE_THREADSAFE

/* replacement structure for original global variable */
typedef struct H5_api_struct {
    H5TS_mutex_t init_lock;    /* API entrance mutex */
    hbool_t      H5_libinit_g; /* Has the library been initialized? */
    hbool_t      H5_libterm_g; /* Is the library being shutdown? */
} H5_api_t;

/* Macros for accessing the global variables */
#define H5_INIT_GLOBAL (H5_g.H5_libinit_g)
#define H5_TERM_GLOBAL (H5_g.H5_libterm_g)

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
extern hbool_t H5_libterm_g; /* Is the library being shutdown? */

/* Macros for accessing the global variables */
#define H5_INIT_GLOBAL (H5_libinit_g)
#define H5_TERM_GLOBAL (H5_libterm_g)

#endif /* H5_HAVE_THREADSAFE */

#ifdef H5_HAVE_CODESTACK

/* Include required function stack header */
#include "H5CSprivate.h"

#define H5_PUSH_FUNC H5CS_push(__func__);
#define H5_POP_FUNC  H5CS_pop();
#else                /* H5_HAVE_CODESTACK */
#define H5_PUSH_FUNC /* void */
#define H5_POP_FUNC  /* void */
#endif               /* H5_HAVE_CODESTACK */

#ifdef H5_HAVE_MPE
extern hbool_t H5_MPEinit_g; /* Has the MPE Library been initialized? */
#endif

/* Forward declaration of H5CXpush() / H5CXpop() */
/* (Including H5CXprivate.h creates bad circular dependencies - QAK, 3/18/2018) */
H5_DLL herr_t H5CX_push(void);
H5_DLL herr_t H5CX_pop(hbool_t update_dxpl_props);

#ifndef NDEBUG
#define FUNC_ENTER_CHECK_NAME(asrt)                                                                          \
    {                                                                                                        \
        static hbool_t func_check = FALSE;                                                                   \
                                                                                                             \
        if (!func_check) {                                                                                   \
            /* Check function naming status */                                                               \
            assert(asrt &&                                                                                   \
                   "Function naming conventions are incorrect - check H5_IS_API|PUB|PRIV|PKG macros in "     \
                   "H5private.h (this is usually due to an incorrect number of underscores)");               \
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
    FUNC_ENTER_COMMON(H5_IS_API(__func__));                                                                  \
    FUNC_ENTER_API_THREADSAFE;

#define FUNC_ENTER_API_INIT(err)                                                                             \
    /* Initialize the library */                                                                             \
    if (!H5_INIT_GLOBAL && !H5_TERM_GLOBAL) {                                                                \
        if (H5_init_library() < 0)                                                                           \
            HGOTO_ERROR(H5E_FUNC, H5E_CANTINIT, err, "library initialization failed")                        \
    }

#define FUNC_ENTER_API_PUSH(err)                                                                             \
    /* Push the name of this function on the function stack */                                               \
    H5_PUSH_FUNC                                                                                             \
                                                                                                             \
    /* Push the API context */                                                                               \
    if (H5CX_push() < 0)                                                                                     \
        HGOTO_ERROR(H5E_FUNC, H5E_CANTSET, err, "can't set API context")                                     \
    else                                                                                                     \
        api_ctx_pushed = TRUE;                                                                               \
                                                                                                             \
    BEGIN_MPE_LOG

/* Use this macro for all "normal" API functions */
#define FUNC_ENTER_API(err)                                                                                  \
    {                                                                                                        \
        {                                                                                                    \
            hbool_t api_ctx_pushed = FALSE;                                                                  \
                                                                                                             \
            FUNC_ENTER_API_COMMON                                                                            \
            FUNC_ENTER_API_INIT(err);                                                                        \
            FUNC_ENTER_API_PUSH(err);                                                                        \
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
            hbool_t api_ctx_pushed = FALSE;                                                                  \
                                                                                                             \
            FUNC_ENTER_API_COMMON                                                                            \
            FUNC_ENTER_API_INIT(err);                                                                        \
            FUNC_ENTER_API_PUSH(err);                                                                        \
            {

/*
 * Use this macro for API functions that shouldn't perform _any_ initialization
 *      of the library or an interface, just perform tracing, etc.  Examples
 *      are: H5allocate_memory, H5is_library_threadsafe, public VOL callback
 *      wrappers (e.g. H5VLfile_create, H5VLdataset_read, etc.), etc.
 *
 */
#define FUNC_ENTER_API_NOINIT                                                                                \
    {                                                                                                        \
        {                                                                                                    \
            {                                                                                                \
                FUNC_ENTER_API_COMMON                                                                        \
                H5_PUSH_FUNC                                                                                 \
                BEGIN_MPE_LOG                                                                                \
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
            {                                                                                                \
                {                                                                                            \
                    FUNC_ENTER_API_VARS                                                                      \
                    FUNC_ENTER_COMMON_NOERR(H5_IS_API(__func__));                                            \
                    FUNC_ENTER_API_THREADSAFE;                                                               \
                    BEGIN_MPE_LOG                                                                            \
                    {

/*
 * Use this macro for API functions that should only perform initialization
 *      of the library or an interface, but not push any state (API context,
 *      function name, start MPE logging, etc) examples are: H5open.
 *
 */
#define FUNC_ENTER_API_NOPUSH(err)                                                                           \
    {                                                                                                        \
        {                                                                                                    \
            {                                                                                                \
                {                                                                                            \
                    {                                                                                        \
                        FUNC_ENTER_COMMON(H5_IS_API(__func__));                                              \
                        FUNC_ENTER_API_THREADSAFE;                                                           \
                        FUNC_ENTER_API_INIT(err);                                                            \
                        {

/*
 * Use this macro for API functions that shouldn't perform _any_ initialization
 *      of the library or an interface, or push themselves on the function
 *      stack, or perform tracing, etc.  This macro _only_ sanity checks the
 *    API name itself.  Examples are: H5TSmutex_acquire,
 *
 */
#define FUNC_ENTER_API_NAMECHECK_ONLY                                                                        \
    {                                                                                                        \
        {                                                                                                    \
            {                                                                                                \
                {                                                                                            \
                    {                                                                                        \
                        {                                                                                    \
                            FUNC_ENTER_COMMON_NOERR(H5_IS_API(__func__));                                    \
                            {

/* Use this macro for all "normal" non-API functions */
#define FUNC_ENTER_NOAPI(err)                                                                                \
    {                                                                                                        \
        FUNC_ENTER_COMMON(!H5_IS_API(__func__));                                                             \
        H5_PUSH_FUNC                                                                                         \
        {

/* Use this macro for all non-API functions, which propagate errors, but don't issue them */
#define FUNC_ENTER_NOAPI_NOERR                                                                               \
    {                                                                                                        \
        FUNC_ENTER_COMMON_NOERR(!H5_IS_API(__func__));                                                       \
        H5_PUSH_FUNC                                                                                         \
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
        FUNC_ENTER_COMMON(!H5_IS_API(__func__));                                                             \
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
        FUNC_ENTER_COMMON_NOERR(!H5_IS_API(__func__));                                                       \
        H5_PUSH_FUNC                                                                                         \
        {

/*
 * Use this macro for non-API functions which fall into these categories:
 *      - functions which shouldn't push their name on the function stack
 *              (so far, just the H5CS routines themselves)
 *
 */
#define FUNC_ENTER_NOAPI_NOFS                                                                                \
    {                                                                                                        \
        FUNC_ENTER_COMMON(!H5_IS_API(__func__));                                                             \
                                                                                                             \
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
        FUNC_ENTER_COMMON_NOERR(!H5_IS_API(__func__));                                                       \
        {

/*
 * Use this macro for non-API functions that shouldn't perform _any_ initialization
 *      of the library or an interface, or push themselves on the function
 *      stack, or perform tracing, etc.  This macro _only_ sanity checks the
 *    API name itself.  Examples are private routines in the H5TS package.
 *
 */
#define FUNC_ENTER_NOAPI_NAMECHECK_ONLY                                                                      \
    {                                                                                                        \
        FUNC_ENTER_COMMON_NOERR(!H5_IS_API(__func__));

/* Use the following two macros as replacements for the FUNC_ENTER_NOAPI
 * and FUNC_ENTER_NOAPI_NOINIT macros when the function needs to set
 * up a metadata tag. */
#define FUNC_ENTER_NOAPI_TAG(tag, err)                                                                       \
    {                                                                                                        \
        haddr_t prev_tag = HADDR_UNDEF;                                                                      \
                                                                                                             \
        FUNC_ENTER_COMMON(!H5_IS_API(__func__));                                                             \
        H5AC_tag(tag, &prev_tag);                                                                            \
        H5_PUSH_FUNC                                                                                         \
        {

#define FUNC_ENTER_NOAPI_NOINIT_TAG(tag)                                                                     \
    {                                                                                                        \
        haddr_t prev_tag = HADDR_UNDEF;                                                                      \
                                                                                                             \
        FUNC_ENTER_COMMON(!H5_IS_API(__func__));                                                             \
        H5AC_tag(tag, &prev_tag);                                                                            \
        H5_PUSH_FUNC                                                                                         \
        {

/* Use this macro for all "normal" package-level functions */
#define FUNC_ENTER_PACKAGE                                                                                   \
    {                                                                                                        \
        FUNC_ENTER_COMMON(H5_IS_PKG(__func__));                                                              \
        H5_PUSH_FUNC                                                                                         \
        {

/* Use this macro for package-level functions which propgate errors, but don't issue them */
#define FUNC_ENTER_PACKAGE_NOERR                                                                             \
    {                                                                                                        \
        FUNC_ENTER_COMMON_NOERR(H5_IS_PKG(__func__));                                                        \
        H5_PUSH_FUNC                                                                                         \
        {

/* Use the following macro as replacement for the FUNC_ENTER_PACKAGE
 * macro when the function needs to set up a metadata tag. */
#define FUNC_ENTER_PACKAGE_TAG(tag)                                                                          \
    {                                                                                                        \
        haddr_t prev_tag = HADDR_UNDEF;                                                                      \
                                                                                                             \
        FUNC_ENTER_COMMON(H5_IS_PKG(__func__));                                                              \
        H5AC_tag(tag, &prev_tag);                                                                            \
        H5_PUSH_FUNC                                                                                         \
        {

/* Use this macro for all "normal" staticly-scoped functions */
#define FUNC_ENTER_STATIC                                                                                    \
    {                                                                                                        \
        FUNC_ENTER_COMMON(H5_IS_PKG(__func__));                                                              \
        H5_PUSH_FUNC                                                                                         \
        {

/* Use this macro for staticly-scoped functions which propgate errors, but don't issue them */
#define FUNC_ENTER_STATIC_NOERR                                                                              \
    {                                                                                                        \
        FUNC_ENTER_COMMON_NOERR(H5_IS_PKG(__func__));                                                        \
        H5_PUSH_FUNC                                                                                         \
        {

/* Use this macro for staticly-scoped functions which propgate errors, but don't issue them */
/* And that shouldn't push their name on the function stack */
#define FUNC_ENTER_STATIC_NOERR_NOFS                                                                         \
    {                                                                                                        \
        FUNC_ENTER_COMMON_NOERR(H5_IS_PKG(__func__));                                                        \
        {

/*
 * Use this macro for non-API functions that shouldn't perform _any_ initialization
 *      of the library or an interface, or push themselves on the function
 *      stack, or perform tracing, etc.  This macro _only_ sanity checks the
 *    API name itself.  Examples are static routines in the H5TS package.
 *
 */
#define FUNC_ENTER_STATIC_NAMECHECK_ONLY                                                                     \
    {                                                                                                        \
        FUNC_ENTER_COMMON_NOERR(H5_IS_PKG(__func__));

/* Use the following macro as replacement for the FUNC_ENTER_STATIC
 * macro when the function needs to set up a metadata tag. */
#define FUNC_ENTER_STATIC_TAG(tag)                                                                           \
    {                                                                                                        \
        haddr_t prev_tag = HADDR_UNDEF;                                                                      \
                                                                                                             \
        FUNC_ENTER_COMMON(H5_IS_PKG(__func__));                                                              \
        H5AC_tag(tag, &prev_tag);                                                                            \
        H5_PUSH_FUNC                                                                                         \
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

#define FUNC_LEAVE_API_COMMON(ret_value)                                                                     \
    FINISH_MPE_LOG                                                                                           \
    H5TRACE_RETURN(ret_value);

#define FUNC_LEAVE_API(ret_value)                                                                            \
    ;                                                                                                        \
    } /*end scope from end of FUNC_ENTER*/                                                                   \
    FUNC_LEAVE_API_COMMON(ret_value);                                                                        \
    if (api_ctx_pushed) {                                                                                    \
        (void)H5CX_pop(TRUE);                                                                                \
        api_ctx_pushed = FALSE;                                                                              \
    }                                                                                                        \
    H5_POP_FUNC                                                                                              \
    if (err_occurred)                                                                                        \
        (void)H5E_dump_api_stack(TRUE);                                                                      \
    FUNC_LEAVE_API_THREADSAFE                                                                                \
    return (ret_value);                                                                                      \
    }                                                                                                        \
    } /*end scope from beginning of FUNC_ENTER*/

/* Use this macro to match the FUNC_ENTER_API_NOINIT macro */
#define FUNC_LEAVE_API_NOINIT(ret_value)                                                                     \
    ;                                                                                                        \
    } /*end scope from end of FUNC_ENTER*/                                                                   \
    FUNC_LEAVE_API_COMMON(ret_value);                                                                        \
    H5_POP_FUNC                                                                                              \
    if (err_occurred)                                                                                        \
        (void)H5E_dump_api_stack(TRUE);                                                                      \
    FUNC_LEAVE_API_THREADSAFE                                                                                \
    return (ret_value);                                                                                      \
    }                                                                                                        \
    }                                                                                                        \
    } /*end scope from beginning of FUNC_ENTER*/

/* Use this macro to match the FUNC_ENTER_API_NOINIT_NOERR_NOFS macro */
#define FUNC_LEAVE_API_NOFS(ret_value)                                                                       \
    ;                                                                                                        \
    } /*end scope from end of FUNC_ENTER*/                                                                   \
    FUNC_LEAVE_API_COMMON(ret_value);                                                                        \
    FUNC_LEAVE_API_THREADSAFE                                                                                \
    return (ret_value);                                                                                      \
    }                                                                                                        \
    }                                                                                                        \
    }                                                                                                        \
    } /*end scope from beginning of FUNC_ENTER*/

/* Use this macro to match the FUNC_ENTER_API_NOPUSH macro */
#define FUNC_LEAVE_API_NOPUSH(ret_value)                                                                     \
    ;                                                                                                        \
    } /*end scope from end of FUNC_ENTER*/                                                                   \
    if (err_occurred)                                                                                        \
        (void)H5E_dump_api_stack(TRUE);                                                                      \
    FUNC_LEAVE_API_THREADSAFE                                                                                \
    return (ret_value);                                                                                      \
    }                                                                                                        \
    }                                                                                                        \
    }                                                                                                        \
    }                                                                                                        \
    } /*end scope from beginning of FUNC_ENTER*/

/* Use this macro to match the FUNC_ENTER_API_NAMECHECK_ONLY macro */
#define FUNC_LEAVE_API_NAMECHECK_ONLY(ret_value)                                                             \
    ;                                                                                                        \
    } /*end scope from end of FUNC_ENTER*/                                                                   \
    return (ret_value);                                                                                      \
    }                                                                                                        \
    }                                                                                                        \
    }                                                                                                        \
    }                                                                                                        \
    }                                                                                                        \
    } /*end scope from beginning of FUNC_ENTER*/

#define FUNC_LEAVE_NOAPI(ret_value)                                                                          \
    ;                                                                                                        \
    } /*end scope from end of FUNC_ENTER*/                                                                   \
    H5_POP_FUNC                                                                                              \
    return (ret_value);                                                                                      \
    } /*end scope from beginning of FUNC_ENTER*/

#define FUNC_LEAVE_NOAPI_VOID                                                                                \
    ;                                                                                                        \
    } /*end scope from end of FUNC_ENTER*/                                                                   \
    H5_POP_FUNC                                                                                              \
    return;                                                                                                  \
    } /*end scope from beginning of FUNC_ENTER*/

/*
 * Use this macro for non-API functions which fall into these categories:
 *      - functions which didn't push their name on the function stack
 *              (so far, just the H5CS routines themselves)
 */
#define FUNC_LEAVE_NOAPI_NOFS(ret_value)                                                                     \
    ;                                                                                                        \
    } /*end scope from end of FUNC_ENTER*/                                                                   \
    return (ret_value);                                                                                      \
    } /*end scope from beginning of FUNC_ENTER*/

/* Use these macros to match the FUNC_ENTER_NOAPI_NAMECHECK_ONLY macro */
#define FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)                                                           \
    return (ret_value);                                                                                      \
    } /*end scope from beginning of FUNC_ENTER*/
#define FUNC_LEAVE_NOAPI_VOID_NAMECHECK_ONLY                                                                 \
    return;                                                                                                  \
    } /*end scope from beginning of FUNC_ENTER*/

/* Use this macro when exiting a function that set up a metadata tag */
#define FUNC_LEAVE_NOAPI_TAG(ret_value)                                                                      \
    ;                                                                                                        \
    } /*end scope from end of FUNC_ENTER*/                                                                   \
    H5AC_tag(prev_tag, NULL);                                                                                \
    H5_POP_FUNC                                                                                              \
    return (ret_value);                                                                                      \
    } /*end scope from beginning of FUNC_ENTER*/

/* Macro to begin/end tagging (when FUNC_ENTER_*TAG macros are insufficient).
 * Make sure to use HGOTO_ERROR_TAG and HGOTO_DONE_TAG between these macros! */
#define H5_BEGIN_TAG(tag)                                                                                    \
    {                                                                                                        \
        haddr_t prv_tag = HADDR_UNDEF;                                                                       \
        H5AC_tag(tag, &prv_tag);

#define H5_END_TAG                                                                                           \
    H5AC_tag(prv_tag, NULL);                                                                                 \
    }

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
H5_DLL int H5A_term_package(void);
H5_DLL int H5A_top_term_package(void);
H5_DLL int H5AC_term_package(void);
H5_DLL int H5CX_term_package(void);
H5_DLL int H5D_term_package(void);
H5_DLL int H5D_top_term_package(void);
H5_DLL int H5E_term_package(void);
H5_DLL int H5ES_term_package(void);
H5_DLL int H5F_term_package(void);
H5_DLL int H5FD_term_package(void);
H5_DLL int H5FL_term_package(void);
H5_DLL int H5FS_term_package(void);
H5_DLL int H5G_term_package(void);
H5_DLL int H5G_top_term_package(void);
H5_DLL int H5I_term_package(void);
H5_DLL int H5L_term_package(void);
H5_DLL int H5M_term_package(void);
H5_DLL int H5M_top_term_package(void);
H5_DLL int H5P_term_package(void);
H5_DLL int H5PL_term_package(void);
H5_DLL int H5R_term_package(void);
H5_DLL int H5R_top_term_package(void);
H5_DLL int H5S_term_package(void);
H5_DLL int H5S_top_term_package(void);
H5_DLL int H5SL_term_package(void);
H5_DLL int H5T_term_package(void);
H5_DLL int H5T_top_term_package(void);
H5_DLL int H5VL_term_package(void);
H5_DLL int H5Z_term_package(void);

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

/* getopt(3) equivalent that papers over the lack of long options on BSD
 * and lack of Windows support.
 */
H5_DLLVAR int         H5_opterr; /* get_option prints errors if this is on */
H5_DLLVAR int         H5_optind; /* token pointer */
H5_DLLVAR const char *H5_optarg; /* flag argument (or value) */

enum h5_arg_level {
    no_arg = 0,  /* doesn't take an argument     */
    require_arg, /* requires an argument          */
    optional_arg /* argument is optional         */
};

/*
 * get_option determines which options are specified on the command line and
 * returns a pointer to any arguments possibly associated with the option in
 * the ``H5_optarg'' variable. get_option returns the shortname equivalent of
 * the option. The long options are specified in the following way:
 *
 * struct h5_long_options foo[] = {
 *   { "filename", require_arg, 'f' },
 *   { "append", no_arg, 'a' },
 *   { "width", require_arg, 'w' },
 *   { NULL, 0, 0 }
 * };
 *
 * Long named options can have arguments specified as either:
 *
 *   ``--param=arg'' or ``--param arg''
 *
 * Short named options can have arguments specified as either:
 *
 *   ``-w80'' or ``-w 80''
 *
 * and can have more than one short named option specified at one time:
 *
 *   -aw80
 *
 * in which case those options which expect an argument need to come at the
 * end.
 */
struct h5_long_options {
    const char *      name;     /* Name of the long option */
    enum h5_arg_level has_arg;  /* Whether we should look for an arg */
    char              shortval; /* The shortname equivalent of long arg
                                 * this gets returned from get_option
                                 */
};

H5_DLL int H5_get_option(int argc, const char *const *argv, const char *opt,
                         const struct h5_long_options *l_opt);

#ifdef H5_HAVE_PARALLEL
/* Generic MPI functions */
H5_DLL hsize_t H5_mpi_set_bigio_count(hsize_t new_count);
H5_DLL hsize_t H5_mpi_get_bigio_count(void);
H5_DLL herr_t  H5_mpi_comm_dup(MPI_Comm comm, MPI_Comm *comm_new);
H5_DLL herr_t  H5_mpi_info_dup(MPI_Info info, MPI_Info *info_new);
H5_DLL herr_t  H5_mpi_comm_free(MPI_Comm *comm);
H5_DLL herr_t  H5_mpi_info_free(MPI_Info *info);
H5_DLL herr_t  H5_mpi_comm_cmp(MPI_Comm comm1, MPI_Comm comm2, int *result);
H5_DLL herr_t  H5_mpi_info_cmp(MPI_Info info1, MPI_Info info2, int *result);
H5_DLL herr_t  H5_mpio_create_large_type(hsize_t num_elements, MPI_Aint stride_bytes, MPI_Datatype old_type,
                                         MPI_Datatype *new_type);
H5_DLL herr_t  H5_mpio_gatherv_alloc(void *send_buf, int send_count, MPI_Datatype send_type,
                                     const int recv_counts[], const int displacements[],
                                     MPI_Datatype recv_type, hbool_t allgather, int root, MPI_Comm comm,
                                     int mpi_rank, int mpi_size, void **out_buf, size_t *out_buf_num_entries);
H5_DLL herr_t  H5_mpio_gatherv_alloc_simple(void *send_buf, int send_count, MPI_Datatype send_type,
                                            MPI_Datatype recv_type, hbool_t allgather, int root, MPI_Comm comm,
                                            int mpi_rank, int mpi_size, void **out_buf,
                                            size_t *out_buf_num_entries);
#endif /* H5_HAVE_PARALLEL */

/* Functions for debugging */
H5_DLL herr_t H5_buffer_dump(FILE *stream, int indent, const uint8_t *buf, const uint8_t *marker,
                             size_t buf_offset, size_t buf_size);

#include "H5poison.h"

#endif /* H5private_H */
