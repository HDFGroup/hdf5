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
 * This file contains portability macros to ease moving the library between
 * the various platforms.
 */

#ifndef HDF5PORT_H
#define HDF5PORT_H

/**************************************************************************
*  Generally useful macro definitions
**************************************************************************/
#ifndef MIN
#  define MIN(a,b)    (((a)<(b)) ? (a) : (b))
#endif
#ifndef MAX
#  define MAX(a,b)    (((a)>(b)) ? (a) : (b))
#endif
#ifndef MAX3
#  define MAX3(a,b,c)	MAX(MAX(a,b),c)
#endif

/**************************************************************************
*  Macros to work around ANSI C portability problems.
**************************************************************************/
#ifdef DUMBCC
#define CONSTR(v,s) char *v=s
#else
#define CONSTR(v,s) static const char v[]=s
#endif

/* -------------------------- File I/O Functions -------------------------- */
/* FILELIB -- file library to use for file access: 1 stdio, 2 fcntl
   default to stdio library i.e. POSIX buffered I/O */

#ifndef FILELIB
#   define FILELIB POSIXBUFIO    /* POSIX buffered I/O is the default */
#endif /* FILELIB */

#if (FILELIB == POSIXBUFIO)
typedef FILE *hdf_file_t;
#ifdef VMS
/* For VMS, use "mbc=64" to improve performance     */
#   define H5F_OPEN(p, a)      (((a)&H5ACC_WRITE) ? fopen((p),"r+","mbc=64") : fopen((p), "r", "mbc=64"))
#   define H5F_CREATE(p)       (fopen((p), "w+", "mbc=64"))
#else  /*  !VMS  */
#if defined SUN && defined (__GNUC__)
#   define H5F_OPEN(p, a)      (((a)&H5ACC_WRITE) ? fopen((p), "r+") : fopen((p), "r"))
#   define H5F_CREATE(p)       (fopen((p), "w+"))
#else /* !SUN w/ GNU CC */
#   define H5F_OPEN(p, a)      (((a)&H5ACC_WRITE) ? fopen((p), "rb+") : fopen((p), "rb"))
#   define H5F_CREATE(p)       (fopen((p), "wb+"))
#endif /* !SUN w/ GNU CC */
#endif /* VMS */
#   define H5F_READ(f, b, n)   (((size_t)(n) == (size_t)fread((b), 1, (size_t)(n), (f))) ? SUCCEED : FAIL)
#   define H5F_WRITE(f, b, n)  (((size_t)(n) == (size_t)fwrite((b), 1, (size_t)(n), (f))) ? SUCCEED : FAIL)
#   define H5F_CLOSE(f)        (fclose(f))
#   define H5F_FLUSH(f)        (fflush(f)==0 ? SUCCEED : FAIL)
#   define H5F_SEEK(f,o)       (fseek((f), (long)(o), SEEK_SET)==0 ? SUCCEED : FAIL)
#   define H5F_SEEK_CUR(f,o)   (fseek((f), (long)(o), SEEK_CUR)==0 ? SUCCEED : FAIL)
#   define H5F_SEEKEND(f)      (fseek((f), (long)0, SEEK_END)==0 ? SUCCEED : FAIL)
#   define H5F_TELL(f)         (ftell(f))
#   define H5F_OPENERR(f)      ((f) == (FILE *)NULL)
#   define H5F_INVALID_FILE    ((FILE *)NULL)
#endif /* FILELIB == POSIXBUFIO */

#if (FILELIB == POSIXUNBUFIO)
/* using POSIX unbuffered file I/O routines to access files */
typedef int hdf_file_t;
#   define H5F_OPEN(p, a)      (((a) & H5ACC_WRITE) ? open((p), O_RDWR) : open((p), O_RDONLY))
#   define H5F_CREATE(p)       (open((p), O_RDWR | O_CREAT | O_TRUNC))
#   define H5F_CLOSE(f)        (close(f))
#   define H5F_FLUSH(f)        (SUCCEED)
#   define H5F_READ(f, b, n)   (((n)==read((f), (char *)(b), (n))) ? SUCCEED : FAIL)
#   define H5F_WRITE(f, b, n)  (((n)==write((f), (char *)(b), (n))) ? SUCCEED : FAIL)
#   define H5F_SEEK(f, o)      (lseek((f), (off_t)(o), SEEK_SET)!=(-1) ? SUCCEED : FAIL)
#   define H5F_SEEKEND(f)      (lseek((f), (off_t)0, SEEK_END)!=(-1) ? SUCCEED : FAIL)
#   define H5F_TELL(f)         (lseek((f), (off_t)0, SEEK_CUR))
#   define H5F_OPENERR(f)      (f < 0)
#   define H5F_INVALID_FILE    ((int)-1)
#endif /* FILELIB == POSIXUNBUFIO */

#if (FILELIB == MACIO)
/* using special routines to redirect to Mac Toolkit I/O */
typedef short hdf_file_t;
#   define H5F_OPEN(x,y)       mopen(x,y)
#   define H5F_CREATE(name)    mopen(name, H5ACC_CREATE)
#   define H5F_CLOSE(x)        mclose(x)
#   define H5F_FLUSH(a)        (SUCCEED)
#   define H5F_READ(a,b,c)     mread(a, (char *) b, (int32) c)
#   define H5F_WRITE(a,b,c)    mwrite(a, (char *) b, (int32) c)
#   define H5F_SEEK(x,y)       mlseek(x, (int32 )y, 0)
#   define H5F_SEEKEND(x)      mlseek(x, 0L, 2)
#   define H5F_TELL(x)         mlseek(x,0L,1)
#   define H5F_OPENERR(f)      (f < 0)
#   define H5F_INVALID_FILE    ((short)-1)
#endif /* FILELIB == MACIO */

#if (FILELIB == WINNTIO)
/* using special Windows NT functions to enable reading/writing large chunks */
typedef HFILE hdf_file_t;
#   define H5F_OPEN(p, a)      (((a) & H5ACC_WRITE) ? _lopen((p), OF_READWRITE) : _lopen((p), OF_READ))
#   define H5F_CREATE(p)       (_lcreat((p), 0))
#   define H5F_READ(f, b, n)   (((int32)(n) == _hread((f), (b), (n))) ? SUCCEED : FAIL)
#   define H5F_WRITE(f, b, n)  (((int32)(n) == _hwrite((f), (b), (n))) ? SUCCEED : FAIL)
#   define H5F_CLOSE(f)        (_lclose(f)==0 ? SUCCEED : FAIL)
#   define H5F_FLUSH(f)        (0)
#   define H5F_SEEK(f, o)      (_llseek((f), (long)(o), 0))
#   define H5F_SEEKEND(f)      (_llseek((f), (long)0, 2))
#   define H5F_TELL(f)         (_llseek((f),0l,1))
#   define H5F_OPENERR(f)      ((f) == (HFILE)HFILE_ERROR)
#   define H5F_INVALID_FILE    ((HFILE)HFILE_ERROR)
#endif /* FILELIB == WINNTIO */

#if (FILELIB == PAGEBUFIO)
#include "fmpio.h"
/* using page buffered file I/O routines to access files */
typedef MPFILE *hdf_file_t;
#   define H5F_OPEN(p, a)      (MPopen((p), (a)))
#   define H5F_CREATE(p)       (MPopen((p), H5ACC_CREATE))
#   define H5F_CLOSE(f)        (MPclose(f))
#   define H5F_FLUSH(f)        (MPflush(f))
#   define H5F_READ(f, b, n)   (MPread((f), (char *)(b), (n)))
#   define H5F_WRITE(f, b, n)  (MPwrite((f), (char *)(b), (n)))
#   define H5F_SEEK(f, o)      (MPseek((f), (off_t)(o), SEEK_SET))
#   define H5F_SEEKEND(f)      (MPseek((f), (off_t)0, SEEK_END))
#   define H5F_TELL(f)         (MPseek((f), (off_t)0, SEEK_CUR))
#   define H5F_OPENERR(f)      ((f) == (MPFILE *)NULL)
#   define H5F_INVALID_FILE    ((MPFILE *)NULL)
#endif /* FILELIB == PAGEBUFIO */

/**************************************************************************
*  Allocation functions defined differently 
**************************************************************************/
#if !defined MALLOC_CHECK
#  define HDmalloc(s)      (malloc((size_t)s))
#  define HDcalloc(a,b)    (calloc((size_t)a,(size_t)b))
#  define HDfree(p)        (free((void*)p))
#  define HDrealloc(p,s)   (realloc((void*)p,(size_t)s))
#endif /* !defined MALLOC_CHECK */
/* Macro to free space and clear pointer to NULL */
#define HDfreenclear(p) { if((p)!=NULL) HDfree(p); p=NULL; }

/**************************************************************************
*  String functions defined differently 
**************************************************************************/

#  define HDstrcat(s1,s2)   (strcat((s1),(s2)))
#  define HDstrcmp(s,t)     (strcmp((s),(t)))
#  define HDstrcpy(s,d)     (strcpy((s),(d)))
#  define HDstrlen(s)       (strlen((const char *)(s)))
#  define HDstrncmp(s1,s2,n)    (strncmp((s1),(s2),(n)))
#  define HDstrncpy(s1,s2,n)    (strncpy((s1),(s2),(n)))
#  define HDstrchr(s,c)         (strchr((s),(c)))
#  define HDstrrchr(s,c)        (strrchr((s),(c)))
#  define HDstrtol(s,e,b)       (strtol((s),(e),(b)))
/* non-standard function, not defined on the following mahcines - */
#if !(defined VMS || defined macintosh || defined MAC || defined __MWERKS__ || defined SYMANTEC_C || defined MIPSEL || defined NEXT || defined CONVEX || defined IBM6000 || defined ANSISUN || defined IRIX)
#  define HDstrdup(s)      ((char *)strdup((const char *)(s)))
#endif /* !(VMS | etc..) */


/**************************************************************************
*  Memory functions defined differently
**************************************************************************/

# define HDmemcpy(dst,src,n)   (memcpy((void *)(dst),(const void *)(src),(size_t)(n)))
# define HDmemmove(dst,src,n)  (memmove((void*)(dst),(const void *)(src),(size_t)(n)))
# define HDmemset(dst,c,n)     (memset((void *)(dst),(intn)(c),(size_t)(n)))
# define HDmemcmp(dst,src,n)   (memcmp((const void *)(dst),(const void *)(src),(size_t)(n)))


/**************************************************************************
*  Misc. functions
**************************************************************************/
#if defined (MAC) || defined (macintosh) || defined(__MWERKS__) || defined (SYMANTEC_C)
#define HDstat(path, result)	(mstat(path))
#else /* !macintosh */
#define HDstat(path, result)	(stat(path, result))
#endif /* !macintosh */
#define HDgetcwd(s,l)           (getcwd(s,l))
#define HDgetenv(s1)            (getenv(s1))
#define HDputenv(s1)            (putenv(s1))
#define HDltoa(v)               (ltoa(v))
#if defined (SUN) && defined(__GNUC__)
#define HDatexit(f)             (0) /* we punt on the SUN using gcc */
#else /* !SUN & GCC */
#define HDatexit(f)             (atexit(f))
#endif /* !SUN & GCC */

#endif /* HDF5PORT_H */

