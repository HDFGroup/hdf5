/****************************************************************************
 * NCSA HDF                                                                 *
 * Software Development Group                                               *
 * National Center for Supercomputing Applications                          *
 * University of Illinois at Urbana-Champaign                               *
 * 605 E. Springfield, Champaign IL 61820                                   *
 *                                                                          *
 * For conditions of distribution and use, see the accompanying             *
 * hdf/COPYING file.                                                        *
 *                                                                          *
 ****************************************************************************/

/* $Id$ */

/*
 * This file contains macros & private information for general HDF5 functions.
 * Every HDF5 source file will include this file immediately after any
 * system include files but before any other private include files.
 */
#ifndef _H5private_H
#define _H5private_H
#include <H5public.h>           /* Include Public Definitions     */

/* Version #'s of library code */
#define HDF5_MAJOR_VERSION      1       /* For major interface changes    */
#define HDF5_MINOR_VERSION      0       /* For minor interface changes    */
#define HDF5_RELEASE_VERSION    0       /* For interface tweaks & bug-fixes */
#define HDF5_PATCH_VERSION      0       /* For small groups of bug fixes */

/* Version #'s of the major components of the file format */
#define HDF5_BOOTBLOCK_VERSION  0       /* of the boot block format       */
#define HDF5_SMALLOBJECT_VERSION 0      /* of the Small-Object Heap       */
#define HDF5_FREESPACE_VERSION  0       /* of the Free-Space Info         */
#define HDF5_OBJECTDIR_VERSION  0       /* of the Object Directory format */
#define HDF5_SHAREDHEADER_VERSION 0     /* of the Shared-Header Info      */

/*
 * Include those things that almost all source files need.
 */
#ifdef STDC_HEADERS
#  include <assert.h>
#  include <fcntl.h>
#  include <stdio.h>
#  include <stdlib.h>
#  include <string.h>
#  include <unistd.h>
#endif

#ifdef HAVE_PARALLEL
/*
 * MPIO headers
 */
#  include <mpi.h>
#  include <mpio.h>
#endif


/*
 * Pablo support files.
 */
#ifdef HAVE_PABLO
#  define IOTRACE
#  include "IOTrace.h"
#  include "ProcIDS.h"
#endif

/* Does the compiler support the __attribute__(()) syntax? */
#ifndef HAVE_ATTRIBUTE
#  define __attribute__(X)      /*void */
#endif

/* Does the compiler expand __FUNCTION__? */
#ifndef HAVE_FUNCTION
#  define __FUNCTION__  "NoFuntionName"
#endif

/* number of members in an array */
#ifndef NELMTS
#   define NELMTS(X)    (sizeof(X)/sizeof(X[0]))
#endif

/* minimum of two values */
#ifndef MIN
#  define MIN(a,b)    (((a)<(b)) ? (a) : (b))
#endif

/* maximum of two values */
#ifndef MAX
#  define MAX(a,b)    (((a)>(b)) ? (a) : (b))
#endif

/* absolute value */
#ifndef ABS
#  define ABS(a)    (((a)>=0) ? (a) : -(a))
#endif

/* sign of argument */
#ifndef SIGN
#  define SIGN(a)    ((a)>0 ? 1 : (a)<0 ? -1 : 0)
#endif

/* maximum of three values */
#ifndef MAX3
#  define MAX3(a,b,c)   MAX(MAX(a,b),c)
#endif

/*
 * HDF Boolean type.
 */
#ifndef FALSE
#   define FALSE 0
#endif
#ifndef TRUE
#   define TRUE (!FALSE)
#endif

/*
 * File addresses.
 */
typedef struct {
    uint64                  offset;     /*offset within an HDF5 file    */
} haddr_t;

#define NO_ADDR         NULL

/*
 * Some compilers have problems declaring auto variables that point
 * to string constants.  Use the CONSTR() macro so it's easy to fix
 * those compilers.
 */
#ifndef CONSTR
#  define CONSTR(VAR,STR) static const char VAR[]=STR
#endif

/*
 * Redefine all the POSIX functions.  We should never see a POSIX
 * function (or any other non-HDF5 function) in the source!
 */
#define HDabort()               abort()
#define HDabs(X)                abs(X)
#define HDaccess(F,M)           access(F, M)
#define HDacos(X)               acos(X)
#define HDalarm(N)              alarm(N)
#define HDasctime(T)            asctime(T)
#define HDasin(X)               asin(X)
#define HDassert(X)             assert(X)
#define HDatan(X)               atan(X)
#define HDatan2(X,Y)            atan2(X,Y)
#define HDatexit(F)             atexit(F)
#define HDatof(S)               atof(S)
#define HDatoi(S)               atoi(S)
#define HDatol(S)               atol(S)
#define HDbsearch(K,B,N,Z,F)    bsearch(K,B,N,Z,F)
#define HDcalloc(N,Z)           calloc(N,Z)
#define HDceil(X)               ceil(X)
#define HDcfgetispeed(T)        cfgetispeed(T)
#define HDcfgetospeed(T)        cfgetospeed(T)
#define HDcfsetispeed(T,S)      cfsetispeed(T,S)
#define HDcfsetospeed(T,S)      cfsetospeed(T,S)
#define HDchdir(S)              chdir(S)
#define HDchmod(S,M)            chmod(S,M)
#define HDchown(S,O,G)          chown(S,O,G)
#define HDclearerr(F)           clearerr(F)
#define HDclock()               clock()
#define HDclose(F)              close(F)
#define HDclosedir(D)           closedir(D)
#define HDcos(X)                cos(X)
#define HDcosh(X)               cosh(X)
#define HDcreat(S,M)            creat(S,M)
#define HDctermid(S)            ctermid(S)
#define HDctime(T)              ctime(T)
#define HDcuserid(S)            cuserid(S)
#define HDdifftime(X,Y)         difftime(X,Y)
#define HDdiv(X,Y)              div(X,Y)
#define HDdup(F)                dup(F)
#define HDdup2(F,I)             dup2(F,I)
/* execl() variable arguments */
/* execle() variable arguments */
/* execlp() variable arguments */
#define HDexecv(S,AV)           execv(S,AV)
#define HDexecve(S,AV,E)        execve(S,AV,E)
#define HDexecvp(S,AV)          execvp(S,AV)
#define HDexit(N)               exit(N)
#define HD_exit(N)              _exit(N)
#define HDexp(X)                exp(X)
#define HDfabs(X)               fabs(X)
#define HDfclose(F)             fclose(F)
/* fcntl() variable arguments */
#define HDfdopen(N,S)           fdopen(N,S)
#define HDfeof(F)               feof(F)
#define HDferror(F)             ferror(F)
#define HDfflush(F)             fflush(F)
#define HDfgetc(F)              fgetc(F)
#define HDfgetpos(F,P)          fgetpos(F,P)
#define HDfgets(S,N,F)          fgets(S,N,F)
#define HDfileno(F)             fileno(F)
#define HDfloor(X)              floor(X)
#define HDfmod(X,Y)             fmod(X,Y)
#define HDfopen(S,M)            fopen(S,M)
#define HDfork()                fork()
#define HDfpathconf(F,N)        fpathconf(F,N)
/* fprintf() variable arguments */
#define HDfputc(C,F)            fputc(C,F)
#define HDfputs(S,F)            fputs(S,F)
#define HDfread(M,Z,N,F)        fread(M,Z,N,F)
#define HDfree(M)               free(M)
#define HDfreopen(S,M,F)        freopen(S,M,F)
#define HDfrexp(X,N)            frexp(X,N)
/* fscanf() variable arguments */
#define HDfseek(F,O,W)          fseek(F,O,W)
#define HDfsetpos(F,P)          fsetpos(F,P)
#define HDfstat(F,B)            fstat(F,B)
#define HDftell(F)              ftell(F)
#define HDfwrite(M,Z,N,F)       fwrite(M,Z,N,F)
#define HDgetc(F)               getc(F)
#define HDgetchar()             getchar()
#define HDgetcwd(S,Z)           getcwd(S,Z)
#define HDgetegid()             getegid()
#define HDgetenv(S)             getenv(S)
#define HDgeteuid()             geteuid()
#define HDgetgid()              getgid()
#define HDgetgrgid(G)           getgrgid(G)
#define HDgetgrnam(S)           getgrnam(S)
#define HDgetgroups(Z,G)        getgroups(Z,G)
#define HDgetlogin()            getlogin()
#define HDgetpgrp()             getpgrp()
#define HDgetpid()              getpid()
#define HDgetppid()             getppid()
#define HDgetpwnam(S)           getpwnam(S)
#define HDgetpwuid(U)           getpwuid(U)
#define HDgets(S)               gets(S)
#define HDgetuid()              getuid()
#define HDgmtime(T)             gmtime(T)
#define HDisalnum(C)            isalnum(C)
#define HDisalpha(C)            isalpha(C)
#define HDisatty(F)             isatty(F)
#define HDiscntrl(C)            iscntrl(C)
#define HDisdigit(C)            isdigit(C)
#define HDisgraph(C)            isgraph(C)
#define HDislower(C)            islower(C)
#define HDisprint(C)            isprint(C)
#define HDispunct(C)            ispunct(C)
#define HDisspace(C)            isspace(C)
#define HDisupper(C)            isupper(C)
#define HDisxdigit(C)           isxdigit(C)
#define HDkill(P,S)             kill(P,S)
#define HDlabs(X)               labs(X)
#define HDldexp(X,N)            ldexp(X,N)
#define HDldiv(X,Y)             ldiv(X,Y)
#define HDlink(OLD,NEW)         link(OLD,NEW)
#define HDlocaleconv()          localeconv()
#define HDlocaltime(T)          localtime(T)
#define HDlog(X)                log(X)
#define HDlog10(X)              log10(X)
#define HDlongjmp(J,N)          longjmp(J,N)
#define HDlseek(F,O,W)          lseek(F,O,W)
#define HDmalloc(Z)             malloc(Z)
#define HDmblen(S,N)            mblen(S,N)
#define HDmbstowcs(P,S,Z)       mbstowcs(P,S,Z)
#define HDmbtowc(P,S,Z)         mbtowc(P,S,Z)
#define HDmemchr(S,C,Z)         memchr(S,C,Z)
#define HDmemcmp(X,Y,Z)         memcmp(X,Y,Z)
#define HDmemcpy(X,Y,Z)         memcpy(X,Y,Z)
#define HDmemmove(X,Y,Z)        memmove(X,Y,Z)
#define HDmemset(X,C,Z)         memset(X,C,Z)
#define HDmkdir(S,M)            mkdir(S,M)
#define HDmkfifo(S,M)           mkfifo(S,M)
#define HDmktime(T)             mktime(T)
#define HDmodf(X,Y)             modf(X,Y)
/* open() variable arguments */
#define HDopendir(S)            opendir(S)
#define HDpathconf(S,N)         pathconf(S,N)
#define HDpause()               pause()
#define HDperror(S)             perror(S)
#define HDpipe(F)               pipe(F)
#define HDpow(X,Y)              pow(X,Y)
/* printf() variable arguments */
#define HDputc(C,F)             putc(C,F)
#define HDputchar(C)            putchar(C)
#define HDputs(S)               puts(S)
#define HDqsort(M,N,Z,F)        qsort(M,N,Z,F)
#define HDraise(N)              raise(N)
#define HDrand()                rand()
#define HDread(F,M,Z)           read(F,M,Z)
#define HDreaddir(D)            readdir(D)
#define HDrealloc(M,Z)          realloc(M,Z)
#define HDremove(S)             remove(S)
#define HDrename(OLD,NEW)       rename(OLD,NEW)
#define HDrewind(F)             rewind(F)
#define HDrewinddir(D)          rewinddir(D)
#define HDrmdir(S)              rmdir(S)
/* scanf() variable arguments */
#define HDsetbuf(F,S)           setbuf(F,S)
#define HDsetgid(G)             setgid(G)
#define HDsetjmp(J)             setjmp(J)
#define HDsetlocale(N,S)        setlocale(N,S)
#define HDsetpgid(P,PG)         setpgid(P,PG)
#define HDsetsid()              setsid()
#define HDsetuid(U)             setuid(U)
#define HDsetvbuf(F,S,M,Z)      setvbuf(F,S,M,Z)
#define HDsigaction(N,A)        sigaction(N,A)
#define HDsigaddset(S,N)        sigaddset(S,N)
#define HDsigdelset(S,N)        sigdelset(S,N)
#define HDsigemptyset(S)        sigemptyset(S)
#define HDsigfillset(S)         sigfillset(S)
#define HDsigismember(S,N)      sigismember(S,N)
#define HDsiglongjmp(J,N)       siglongjmp(J,N)
#define HDsignal(N,F)           signal(N,F)
#define HDsigpending(S)         sigpending(S)
#define HDsigprocmask(H,S,O)    sigprocmask(H,S,O)
#define HDsigsetjmp(J,N)        sigsetjmp(J,N)
#define HDsigsuspend(S)         sigsuspend(S)
#define HDsin(X)                sin(X)
#define HDsinh(X)               sinh(X)
#define HDsleep(N)              sleep(N)
/* sprintf() variable arguments */
#define HDsqrt(X)               sqrt(X)
#define HDsrand(N)              srand(N)
/* sscanf() variable arguments */
#define HDstat(S,B)             stat(S,B)
#define HDstrcat(X,Y)           strcat(X,Y)
#define HDstrchr(S,C)           strchr(S,C)
#define HDstrcmp(X,Y)           strcmp(X,Y)
#define HDstrcoll(X,Y)          strcoll(X,Y)
#define HDstrcpy(X,Y)           strcpy(X,Y)
#define HDstrcspn(X,Y)          strcspn(X,Y)
#define HDstrerror(N)           strerror(N)
#define HDstrftime(S,Z,F,T)     strftime(S,Z,F,T)
#define HDstrlen(S)             strlen(S)
#define HDstrncat(X,Y,Z)        strncat(X,Y,Z)
#define HDstrncmp(X,Y,Z)        strncmp(X,Y,Z)
#define HDstrncpy(X,Y,Z)        strncpy(X,Y,Z)
#define HDstrpbrk(X,Y)          strpbrk(X,Y)
#define HDstrrchr(S,C)          strrchr(S,C)
#define HDstrspn(X,Y)           strspn(X,Y)
#define HDstrstr(X,Y)           strstr(X,Y)
#define HDstrtod(S,R)           strtod(S,R)
#define HDstrtok(X,Y)           strtok(X,Y)
#define HDstrtol(S,R,N)         strtol(S,R,N)
#define HDstrtoul(S,R,N)        strtoul(S,R,N)
#define HDstrxfrm(X,Y,Z)        strxfrm(X,Y,Z)
#define HDsysconf(N)            sysconf(N)
#define HDsystem(S)             system(S)
#define HDtan(X)                tan(X)
#define HDtanh(X)               tanh(X)
#define HDtcdrain(F)            tcdrain(F)
#define HDtcflow(F,A)           tcflow(F,A)
#define HDtcflush(F,N)          tcflush(F,N)
#define HDtcgetattr(F,T)        tcgetattr(F,T)
#define HDtcgetpgrp(F)          tcgetpgrp(F)
#define HDtcsendbreak(F,N)      tcsendbreak(F,N)
#define HDtcsetattr(F,O,T)      tcsetattr(F,O,T)
#define HDtcsetpgrp(F,N)        tcsetpgrp(F,N)
#define HDtime(T)               time(T)
#define HDtimes(T)              times(T)
#define HDtmpfile()             tmpfile()
#define HDtmpnam(S)             tmpnam(S)
#define HDtolower(C)            tolower(C)
#define HDtoupper(C)            toupper(C)
#define HDttyname(F)            ttyname(F)
#define HDtzset()               tzset()
#define HDumask(N)              umask(N)
#define HDuname(S)              uname(S)
#define HDungetc(C,F)           ungetc(C,F)
#define HDunlink(S)             unlink(S)
#define HDutime(S,T)            utime(S,T)
#define HDva_arg(A,T)           va_arg(A,T)
#define HDva_end(A)             va_end(A)
#define HDva_start(A,P)         va_start(A,P)
#define HDvfprintf(F,FMT,A)     vfprintf(F,FMT,A)
#define HDvprintf(FMT,A)        vprintf(FMT,A)
#define HDvsprintf(S,FMT,A)     vsprintf(S,FMT,A)
#define HDwait(W)               wait(W)
#define HDwaitpid(P,W,O)        waitpid(P,W,O)
#define HDwcstombs(S,P,Z)       wcstombs(S,P,Z)
#define HDwctomb(S,C)           wctomb(S,C)
#define HDwrite(F,M,Z)          write(F,M,Z)

/*
 * And now for a couple non-Posix functions...
 */
extern char            *strdup(const char *s);
#define HDstrdup(S)             strdup(S)

/*-------------------------------------------------------------------------
 * Purpose:     Register function entry for library initialization and code
 *              profiling.
 *
 * Notes:       Every file must have a file-scope variable called
 *              `initialize_interface_g' of type hbool_t which is initialized
 *              to FALSE.
 *
 *              Don't use local variable initializers which contain
 *              calls to other library functions since the initializer
 *              would happen before the FUNC_ENTER() gets called.  Don't
 *              use initializers that require special cleanup code to
 *              execute if FUNC_ENTER() fails since a failing FUNC_ENTER()
 *              returns immediately without branching to the `done' label.
 *
 * Programmer:  Quincey Koziol
 *
 * Modifications:
 *
 *      Robb Matzke, 4 Aug 1997
 *      The `interface_init_func' can be the null pointer.  Changed
 *      HGOTO_ERROR() to HRETURN_ERROR() since no clean-up needs to occur
 *      when an error is detected at this point since this must be the
 *      first executable statement in a function.  This allows functions
 *      to omit the `done:' label when convenient to do so.
 *
 *      Robb Matzke, 4 Aug 1997
 *      The pablo mask comes from the constant PABLO_MASK defined on
 *      a per-file basis.  The `pablo_func_id' is generated from the
 *      `func_name' argument by prepending an `ID_' to the name.  The
 *      pablo function identifier should be saved in a local variable
 *      so FUNC_LEAVE() can access it.
 *
 *      Robb Matzke, 4 Aug 1997
 *      It is safe to call this function even inside various library
 *      initializing functions.  Infinite recursion is no longer a
 *      danger.
 *
 *      Robb Matzke, 3 Dec 1997
 *      The interface initialization function is no longer passed as an
 *      argument unless the `FUNC_ENTER_INIT' form is called.  Instead, the
 *      function comes from the `INTERFACE_INIT' constant which must be
 *      defined in every source file.
 *
 *-------------------------------------------------------------------------
 */
extern hbool_t library_initialize_g;   /*good thing C's lazy about extern! */
extern hbool_t thread_initialize_g;    /*don't decl interface_initialize_g */

#define PRINT(N,S) {							      \
       write ((N), (S), strlen((S)));					      \
       write ((N), "\n", 1);						      \
}

#define FUNC_ENTER(func_name,err) FUNC_ENTER_INIT(func_name,INTERFACE_INIT,err)

#define FUNC_ENTER_INIT(func_name,interface_init_func,err) {		      \
   CONSTR (FUNC, #func_name);						      \
   PABLO_SAVE (ID_ ## func_name);					      \
									      \
   PABLO_TRACE_ON (PABLO_MASK, pablo_func_id);				      \
									      \
   /* Initialize the library */						      \
   if (!library_initialize_g) {						      \
      library_initialize_g = TRUE;					      \
      if (H5_init_library()<0) {					      \
         HRETURN_ERROR (H5E_FUNC, H5E_CANTINIT, err,			      \
                        "library initialization failed");		      \
      }									      \
   }									      \
									      \
   /* Initialize this thread */						      \
   if (!thread_initialize_g) {						      \
      thread_initialize_g = TRUE;					      \
      if (H5_init_thread()<0) {						      \
         HRETURN_ERROR (H5E_FUNC, H5E_CANTINIT, err,			      \
                        "thread initialization failed");		      \
      }									      \
   }									      \
									      \
   /* Initialize this interface */					      \
   if (!interface_initialize_g) {					      \
      interface_initialize_g = TRUE;					      \
      if (interface_init_func &&					      \
          ((herr_t(*)(void))interface_init_func)()<0) {			      \
         HRETURN_ERROR (H5E_FUNC, H5E_CANTINIT, err,			      \
                        "interface initialization failed");		      \
      }									      \
   }									      \
									      \
   /* Clear thread error stack entering public functions */		      \
   if (H5E_clearable_g && '_'!=FUNC[2] && '_'!=FUNC[3] &&	              \
       (!FUNC[4] || '_'!=FUNC[4])) {			                      \
       PRINT (55, #func_name);						      \
       H5Eclear (H5E_thrdid_g);						      \
   }									      \
   {

/*-------------------------------------------------------------------------
 * Purpose:     Register function exit for code profiling.  This should be
 *              the last statement executed by a function.
 *
 * Programmer:  Quincey Koziol
 *
 * Modifications:
 *
 *      Robb Matzke, 4 Aug 1997
 *      The pablo mask comes from the constant PABLO_MASK defined on a
 *      per-file basis.  The pablo_func_id comes from an auto variable
 *      defined by FUNC_ENTER.
 *
 *-------------------------------------------------------------------------
 */
#define FUNC_LEAVE(return_value) HRETURN(return_value)}}

/*
 * The FUNC_ENTER() and FUNC_LEAVE() macros make calls to Pablo functions
 * through one of these two sets of macros.
 */
#ifdef HAVE_PABLO
#  define PABLO_SAVE(func_id)   intn pablo_func_id = func_id
#  define PABLO_TRACE_ON(m, f)  TRACE_ON(m,f)
#  define PABLO_TRACE_OFF(m, f) TRACE_OFF(m,f)
#else
#  define PABLO_SAVE(func_id)   /*void */
#  define PABLO_TRACE_ON(m, f)  /*void */
#  define PABLO_TRACE_OFF(m, f) /*void */
#endif

/* Private functions, not part of the publicly documented API */
herr_t H5_init_library(void);
void H5_term_library(void);
herr_t H5_add_exit(void (*func) (void));
herr_t H5_init_thread(void);
void H5_term_thread(void);

#endif
