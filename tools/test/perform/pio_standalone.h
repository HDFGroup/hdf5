/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#ifndef PIO_STANDALONE_H
#define PIO_STANDALONE_H

/* Header file for building h5perf by standalone mode.
 * Created: Christian Chilan, 2005/5/18.
 */

/** From H5private.h **/

#include "H5public.h" /* Include Public Definitions    */

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

/*
 * Redefine all the POSIX functions.  We should never see a POSIX
 * function (or any other non-HDF5 function) in the source!
 */
#ifdef H5_HAVE_WIN32_API
#define HDaccess(F, M) _access(F, M)
#define R_OK           4 /* Test for read permission.  */
#define W_OK           2 /* Test for write permission.  */
#define X_OK           1 /* Test for execute permission.  */
#define F_OK           0 /* Test for existence.  */
#else                    /* H5_HAVE_WIN32_API */
#define HDaccess(F, M) access(F, M)
#ifndef F_OK
#define F_OK 00
#define W_OK 02
#define R_OK 04
#endif
#endif /* H5_HAVE_WIN32_API */
#define HDasprintf               asprintf /*varargs*/
#define HDcfgetispeed(T)         cfgetispeed(T)
#define HDcfgetospeed(T)         cfgetospeed(T)
#define HDcfsetispeed(T, S)      cfsetispeed(T, S)
#define HDcfsetospeed(T, S)      cfsetospeed(T, S)
#define HDchdir(S)               chdir(S)
#define HDchmod(S, M)            chmod(S, M)
#define HDchown(S, O, G)         chown(S, O, G)
#define HDclock()                clock()
#define HDclose(F)               close(F)
#define HDclosedir(D)            closedir(D)
#define HDcreat(S, M)            creat(S, M)
#define HDctermid(S)             ctermid(S)
#define HDcuserid(S)             cuserid(S)
#define HDdup(F)                 dup(F)
#define HDdup2(F, I)             dup2(F, I)
#define HDexecv(S, AV)           execv(S, AV)
#define HDexecve(S, AV, E)       execve(S, AV, E)
#define HDexecvp(S, AV)          execvp(S, AV)
#define HD_exit(N)               _exit(N)
/* fcntl() variable arguments */
#define HDfdopen(N, S)   fdopen(N, S)
#ifdef H5_HAVE_WIN32_API
#define HDfileno(F) _fileno(F)
#else /* H5_HAVE_WIN32_API */
#define HDfileno(F) fileno(F)
#endif /* H5_HAVE_WIN32_API */
#define HDfork()            fork()
#define HDfpathconf(F, N)   fpathconf(F, N)
/* fscanf() variable arguments */
#ifdef H5_HAVE_FSEEKO
#define HDfseek(F, O, W) fseeko(F, O, W)
#else
#define HDfseek(F, O, W) fseek(F, O, W)
#endif
/* definitions related to the file stat utilities.
 * Windows have its own function names.
 * For Unix, if off_t is not 64bit big, try use the pseudo-standard
 * xxx64 versions if available.
 */
#ifdef H5_HAVE_WIN32_API
#define HDfstat(F, B) _fstati64(F, B)
#define HDlstat(S, B) _lstati64(S, B)
#define HDstat(S, B)  _stati64(S, B)
typedef struct _stati64 h5_stat_t;
typedef __int64         h5_stat_size_t;
#define HDoff_t __int64
#elif H5_SIZEOF_OFF_T != 8 && H5_SIZEOF_OFF64_T == 8 && defined(H5_HAVE_STAT64)
#define HDfstat(F, B) fstat64(F, B)
#define HDlstat(S, B) lstat64(S, B)
#define HDstat(S, B)  stat64(S, B)
typedef struct stat64 h5_stat_t;
typedef off64_t       h5_stat_size_t;
#define HDoff_t       off64_t
#else
#define HDfstat(F, B) fstat(F, B)
#define HDlstat(S, B) lstat(S, B)
#define HDstat(S, B)  stat(S, B)
typedef struct stat h5_stat_t;
typedef off_t       h5_stat_size_t;
#define HDoff_t       off_t
#endif

#define HDftruncate(F, L)    ftruncate(F, L)
#define HDgetcwd(S, Z)       getcwd(S, Z)
#define HDgetegid()          getegid()
#define HDgeteuid()          geteuid()
#define HDgetgid()           getgid()
#define HDgetgrgid(G)        getgrgid(G)
#define HDgetgrnam(S)        getgrnam(S)
#define HDgetgroups(Z, G)    getgroups(Z, G)
#ifdef H5_HAVE_WIN32_API
#define HDgetlogin() Wgetlogin()
#else /* H5_HAVE_WIN32_API */
#define HDgetlogin() getlogin()
#endif /* H5_HAVE_WIN32_API */
#define HDgetpgrp()       getpgrp()
#define HDgetpid()        getpid()
#define HDgetppid()       getppid()
#define HDgetpwnam(S)     getpwnam(S)
#define HDgetpwuid(U)     getpwuid(U)
#define HDgetrusage(X, S) getrusage(X, S)
/* Don't define a macro for gets() - it was removed in C11 */
#ifdef H5_HAVE_WIN32_API
H5_DLL int Wgettimeofday(struct timeval *tv, struct timezone *tz);
#define HDgettimeofday(V, Z) Wgettimeofday(V, Z)
#else /* H5_HAVE_WIN32_API */
#define HDgettimeofday(S, P) gettimeofday(S, P)
#endif /* H5_HAVE_WIN32_API */
#define HDgetuid()       getuid()
#define HDisatty(F)      isatty(F)
#define HDkill(P, S)     kill(P, S)
#define HDlink(OLD, NEW) link(OLD, NEW)
#ifdef H5_HAVE_WIN32_API
#define HDlseek(F, O, W) _lseeki64(F, O, W)
#else
#ifdef H5_HAVE_LSEEK64
#define HDlseek(F, O, W) lseek64(F, O, W)
#else
#define HDlseek(F, O, W) lseek(F, O, W)
#endif
#endif
#define HDposix_memalign(P, A, Z) posix_memalign(P, A, Z)
#ifdef H5_HAVE_WIN32_API
#define HDmkdir(S, M) _mkdir(S)
#else /* H5_HAVE_WIN32_API */
#define HDmkdir(S, M) mkdir(S, M)
#endif /* H5_HAVE_WIN32_API */
#define HDmkfifo(S, M) mkfifo(S, M)
#ifdef _O_BINARY
#define HDopen(S, F, M) open(S, F | _O_BINARY, M)
#else
#define HDopen(S, F, M) open(S, F, M)
#endif
#define HDopendir(S)     opendir(S)
#define HDpathconf(S, N) pathconf(S, N)
#define HDpause()        pause()
#define HDpipe(F)        pipe(F)

#ifdef H5_HAVE_RAND_R
#define HDrandom() HDrand()
H5_DLL int HDrand(void);
#elif H5_HAVE_RANDOM
#define HDrand()   random()
#define HDrandom() random()
#else
#define HDrand()   rand()
#define HDrandom() rand()
#endif

#define HDread(F, M, Z)    read(F, M, Z)
#define HDreaddir(D)       readdir(D)
#define HDrewinddir(D)     rewinddir(D)
#define HDrmdir(S)         rmdir(S)
/* scanf() variable arguments */
#define HDsetgid(G)       setgid(G)
#define HDsetpgid(P, PG)  setpgid(P, PG)
#define HDsetsid()        setsid()
#define HDsetuid(U)       setuid(U)
/* Windows does not permit setting the buffer size to values
   less than 2.  */
#define HDsigaddset(S, N)      sigaddset(S, N)
#define HDsigdelset(S, N)      sigdelset(S, N)
#define HDsigemptyset(S)       sigemptyset(S)
#define HDsigfillset(S)        sigfillset(S)
#define HDsigismember(S, N)    sigismember(S, N)
#define HDsiglongjmp(J, N)     siglongjmp(J, N)
#define HDsigpending(S)        sigpending(S)
#define HDsigprocmask(H, S, O) sigprocmask(H, S, O)
#define HDsigsetjmp(J, N)      sigsetjmp(J, N)
#define HDsigsuspend(S)        sigsuspend(S)
#ifdef H5_HAVE_RAND_R
H5_DLL void HDsrand(unsigned int seed);
#define HDsrandom(S) HDsrand(S)
#elif H5_HAVE_RANDOM
#define HDsrand(S)   srandom(S)
#define HDsrandom(S) srandom(S)
#else
#define HDsrand(S)   srand(S)
#define HDsrandom(S) srand(S)
#endif

#ifdef H5_HAVE_WIN32_API
#define HDstrcasecmp(A, B) _stricmp(A, B)
#else
#define HDstrcasecmp(X, Y) strcasecmp(X, Y)
#endif
H5_DLL int64_t HDstrtoll(const char *s, const char **rest, int base);
#define HDsysconf(N)         sysconf(N)
#define HDtcdrain(F)         tcdrain(F)
#define HDtcflow(F, A)       tcflow(F, A)
#define HDtcflush(F, N)      tcflush(F, N)
#define HDtcgetattr(F, T)    tcgetattr(F, T)
#define HDtcgetpgrp(F)       tcgetpgrp(F)
#define HDtcsendbreak(F, N)  tcsendbreak(F, N)
#define HDtcsetattr(F, O, T) tcsetattr(F, O, T)
#define HDtcsetpgrp(F, N)    tcsetpgrp(F, N)
#define HDtime(T)            time(T)
#define HDtimes(T)           times(T)
#define HDttyname(F)         ttyname(F)
#define HDtzset()            tzset()
#define HDumask(N)           umask(N)
#define HDuname(S)           uname(S)
#ifdef H5_HAVE_WIN32_API
#define HDunlink(S) _unlink(S)
#else
#define HDunlink(S) unlink(S)
#endif
#define HDutime(S, T)             utime(S, T)
#define HDvasprintf(RET, FMT, A)  vasprintf(RET, FMT, A)
#define HDwait(W)                 wait(W)
#define HDwaitpid(P, W, O)        waitpid(P, W, O)
#define HDwrite(F, M, Z)          write(F, M, Z)

/*
 * And now for a couple non-Posix functions...  Watch out for systems that
 * define these in terms of macros.
 */
#ifdef H5_HAVE_WIN32_API
#define HDstrdup(S) _strdup(S)
#else /* H5_HAVE_WIN32_API */

#if !defined strdup && !defined H5_HAVE_STRDUP
extern char *         strdup(const char *s);
#endif

#define HDstrdup(S) strdup(S)

#endif /* H5_HAVE_WIN32_API */

/*
 * HDF Boolean type.
 */
#ifndef FALSE
#define FALSE false
#endif
#ifndef TRUE
#define TRUE true
#endif

/** From h5tools_utils.h **/

H5_DLLVAR int         H5_opterr; /* getoption prints errors if this is on    */
H5_DLLVAR int         H5_optind; /* token pointer                            */
H5_DLLVAR const char *H5_optarg; /* flag argument (or value)                 */

enum h5_arg_level {
    no_arg = 0,  /* doesn't take an argument     */
    require_arg, /* requires an argument          */
    optional_arg /* argument is optional         */
};

struct h5_long_options {
    const char *      name;     /* Name of the long option */
    enum h5_arg_level has_arg;  /* Whether we should look for an arg */
    char              shortval; /* The shortname equivalent of long arg
                                 * this gets returned from get_option
                                 */
};

extern int H5_get_option(int argc, const char *const *argv, const char *opt,
                         const struct h5_long_options *l_opt);

extern int nCols; /*max number of columns for outputting  */

/* Definitions of useful routines */
extern void print_version(const char *progname);

#include "H5poison.h"

#endif
