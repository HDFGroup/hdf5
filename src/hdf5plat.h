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
 * This file contains platform/CPU/OS detection macros, etc.
 */

#ifndef HDF5PLAT_H
#define HDF5PLAT_H

/*--------------------------------------------------------------------------*/
/*                              MT/NT constants                             */
/*  Six MT nibbles represent float64, float32, int64, int32, int16, and     */
/*      int8 (from most significant to least significant).  The unsigned    */
/*      form of each type is assumed to be in the same format as the signed */
/*      type.  The top two nibbles (8-bits) are currently unused.           */
/*  The values for each nibble are:                                         */
/*      1 - Big Endian                                                      */
/*      2 - VAX                                                             */
/*      3 - Cray                                                            */
/*      4 - Little Endian                                                   */
/*      5 - Convex                                                          */
/*      6 - Fujitsu VP                                                      */
/*--------------------------------------------------------------------------*/
#define     DFMT_SUN            0x00111111
#define     DFMT_ALLIANT        0x00111111
#define     DFMT_IRIX           0x00111111
#define     DFMT_APOLLO         0x00111111
#define     DFMT_IBM6000        0x00111111
#define     DFMT_HP9000         0x00111111
#define     DFMT_CONVEXNATIVE   0x00551111
#define     DFMT_CONVEX         0x00111111
#define     DFMT_UNICOS         0x00333331
#define     DFMT_CTSS           0x00333331
#define     DFMT_VAX            0x00222221
#define     DFMT_MIPSEL         0x00444441
#define     DFMT_PC             0x00444441
#define     DFMT_MAC            0x00111111
#define     DFMT_SUN386         0x00444441
#define     DFMT_NEXT           0x00111111
#define     DFMT_MOTOROLA       0x00111111
#define     DFMT_ALPHA          0x00444441
#define     DFMT_VP             0x00661111
#define     DFMT_I860           0x00444441
#define     DFMT_CRAYMPP        0x00117771

/* I/O library constants */
#define POSIXUNBUFIO 1  /* POSIX-compliant un-buffered I/O */
#define POSIXBUFIO   2  /* POSIX buffered I/O */
#define MACIO        3  /* Macintosh I/O */
#define WINNTIO      4  /* 32-bit Windows File I/O */
#define PAGEBUFIO    5  /* page buffering - fmpool */

#ifdef GOT_MACHINE
#undef GOT_MACHINE
#endif

/*-------------------------------------------------------------------------
 * Define options for each platform
 *-------------------------------------------------------------------------*/

#if (defined(SUN) || defined(sun) || defined(__sun__) || defined(__SUNPRO_C)) & !defined(__i386)
#ifdef __STDC__
#define ANSISUN
#else /* __STDC__ */
#define KNRSUN
#endif /* __STDC__ */
#endif /* SUN || sun */

/*************************************************************************/
/*************************************************************************/
/*************************************************************************/
/*
 * CPU: Sparc
 * OS: Solaris 1.x, SunOS 4.1.x
 */
#if defined(ANSISUN)

#if !defined(SUN)
#define SUN
#endif

#ifdef GOT_MACHINE
If you get an error on this line more than one machine type has been defined.
Please check your Makefile.
#endif
#define GOT_MACHINE

/* Extra include files required for this platform */
#include <unistd.h>                 /* for some file I/O stuff */
#include <sys/time.h>
#include <sys/file.h>               /* for unbuffered i/o stuff */
#include <sys/stat.h>

/* Set machine byte-format */
#define DF_MT             DFMT_SUN

/* Define portable variable types */
typedef void              VOID;
typedef void              *VOIDP;
typedef char              char8;
typedef unsigned char     uchar8;
typedef char              int8;
typedef unsigned char     uint8;
typedef short int         int16;
typedef unsigned short int uint16;
typedef long int          int32;
typedef unsigned long int uint32;
typedef int               intn;
typedef unsigned int      uintn;
typedef float             float32;
typedef double            float64;
typedef char              *_fcd;    /* Fortran character descriptor type */
typedef int               intf;     /* size of INTEGERs in Fortran compiler */

/* Fortran compatibility macros */
#define FNAME_POST_UNDERSCORE       /* Fortran function names require trailing underscore */
#define _fcdtocp(desc) (desc)       /* Macro to convert from Fortran character descriptor to C 'char *' */

/* Choose the I/O package to use when interacting with the file */
#ifdef  HAVE_FMPOOL
#define FILELIB PAGEBUFIO  /* enable page buffering */
#else
#define FILELIB POSIXBUFIO
#endif

/* JPEG #define's - Look in the JPEG docs before changing - (Q) */

/* Determine the memory manager we are going to use. Valid values are: */
/*  MEM_DOS, MEM_ANSI, MEM_NAME, MEM_NOBS.  See the JPEG docs for details on */
/*  what each does */
#define JMEMSYS         MEM_ANSI
#define HAVE_STDC
#define INCLUDES_ARE_ANSI

#endif /* ANSISUN */

/*************************************************************************/
/*************************************************************************/
/*************************************************************************/
/*
 * CPU: IBM RS/6000 chip/PowerPC
 * OS: AIX
 */
#if defined(IBM6000) || defined(_AIX)

#ifndef IBM6000
#define IBM6000
#endif

#ifdef GOT_MACHINE
If you get an error on this line more than one machine type has been defined.
Please check your Makefile.
#endif
#define GOT_MACHINE

/* Extra include files required for this platform */
#   define BSD
#ifndef __GNUC__
#include <memory.h>
#endif /* __GNUC__ */
#include <sys/file.h>               /* for unbuffered i/o stuff */
#include <sys/stat.h>

/* Set machine byte-format */
#define DF_MT             DFMT_IBM6000

/* Define portable variable types */
typedef void              VOID;
typedef void              *VOIDP;
typedef char              *_fcd;    /* Fortran character descriptor type */
typedef char              char8;
typedef unsigned char     uchar8;
typedef char              int8;
typedef unsigned char     uint8;
typedef short int         int16;
typedef unsigned short int uint16;
typedef long int          int32;
typedef unsigned long int uint32;
typedef int               intn;
typedef unsigned int      uintn;
typedef float             float32;
typedef double            float64;
typedef int               intf;     /* size of INTEGERs in Fortran compiler */

/* Fortran compatibility macros */
#define _fcdtocp(desc) (desc)       /* Macro to convert from Fortran character descriptor to C 'char *' */

/* Choose the I/O package to use when interacting with the file */
#ifdef  HAVE_FMPOOL
#define FILELIB PAGEBUFIO  /* enable page buffering */
#else
#define FILELIB POSIXBUFIO
#endif

/* JPEG #define's - Look in the JPEG docs before changing - (Q) */

/* Determine the memory manager we are going to use. Valid values are: */
/*  MEM_DOS, MEM_ANSI, MEM_NAME, MEM_NOBS.  See the JPEG docs for details on */
/*  what each does */
#define JMEMSYS         MEM_ANSI
#define HAVE_STDC
#define INCLUDES_ARE_ANSI

#endif /* IBM6000 */

/*************************************************************************/
/*************************************************************************/
/*************************************************************************/
/*
 * CPU: HP PA-RISC
 * OS: HP/UX (ConvexOS?)
 */
#if defined(HP9000) || (!defined(__convexc__) && (defined(hpux) || defined(__hpux)))

#ifndef HP9000
#define HP9000
#endif

#ifdef GOT_MACHINE
If you get an error on this line more than one machine type has been defined.
Please check your Makefile.
#endif
#define GOT_MACHINE

/* Extra include files required for this platform */
#define HAVE_UNISTD_H  /* unistd.h - close, fork,..etc */
#   define BSD
#ifndef __GNUC__
#include <memory.h>
#endif /* __GNUC__ */
#include <sys/file.h>               /* for unbuffered i/o stuff */
#include <sys/stat.h>

/* Set machine byte-format */
#define DF_MT             DFMT_HP9000

/* Define portable variable types */
typedef void              VOID;
typedef void              *VOIDP;
typedef char              *_fcd;    /* Fortran character descriptor type */
typedef char              char8;
typedef unsigned char     uchar8;
typedef char              int8;
typedef unsigned char     uint8;
typedef short int         int16;
typedef unsigned short int uint16;
typedef long int          int32;
typedef unsigned long int uint32;
typedef int               intn;
typedef unsigned int      uintn;
typedef float             float32;
typedef double            float64;
typedef int               intf;     /* size of INTEGERs in Fortran compiler */

/* Fortran compatibility macros */
#define _fcdtocp(desc) (desc)       /* Macro to convert from Fortran character descriptor to C 'char *' */

/* Choose the I/O package to use when interacting with the file */
#ifdef  HAVE_FMPOOL
#define FILELIB PAGEBUFIO  /* enable page buffering */
#else
#define FILELIB POSIXBUFIO
#endif

/* JPEG #define's - Look in the JPEG docs before changing - (Q) */

/* Determine the memory manager we are going to use. Valid values are: */
/*  MEM_DOS, MEM_ANSI, MEM_NAME, MEM_NOBS.  See the JPEG docs for details on */
/*  what each does */
#define JMEMSYS         MEM_ANSI

#endif /* HP9000 */


/*************************************************************************/
/*************************************************************************/
/*************************************************************************/
/*
 * CPU: MIPS
 * OS: IRIX
 */
#if defined(IRIX) || defined(IRIS4) || defined(sgi) || defined(__sgi__) || defined(__sgi)

#ifndef IRIX
#define IRIX
#endif

#ifdef GOT_MACHINE
If you get an error on this line more than one machine type has been defined.
Please check your Makefile.
#endif
#define GOT_MACHINE 1

/* Extra include files required for this platform */
#   define BSD
#ifndef __GNUC__
#include <memory.h>
#endif /* __GNUC__ */
#include <sys/file.h>               /* for unbuffered i/o stuff */
#include <sys/stat.h>

/* Set machine byte-format */
#define DF_MT              DFMT_IRIX

/* Define portable variable types */
typedef void               VOID;
typedef void              *VOIDP;
typedef char              *_fcd;    /* Fortran character descriptor type */
typedef signed char        char8;
typedef unsigned char      uchar8;
typedef signed char        int8;
typedef unsigned char      uint8;
typedef short int          int16;
typedef unsigned short int uint16;
typedef int                int32;
typedef unsigned int       uint32;
typedef int                intn;
typedef unsigned int       uintn;
typedef float              float32;
typedef double             float64;
typedef int                intf;     /* size of INTEGERs in Fortran compiler */

/* Fortran compatibility macros */
#define FNAME_POST_UNDERSCORE       /* Fortran function names require trailing underscore */
#define _fcdtocp(desc) (desc)       /* Macro to convert from Fortran character descriptor to C 'char *' */

/* Choose the I/O package to use when interacting with the file */
#ifdef  HAVE_FMPOOL
#define FILELIB PAGEBUFIO  /* enable page buffering */
#else
#define FILELIB POSIXBUFIO
#endif

/* JPEG #define's - Look in the JPEG docs before changing - (Q) */

/* Determine the memory manager we are going to use. Valid values are: */
/*  MEM_DOS, MEM_ANSI, MEM_NAME, MEM_NOBS.  See the JPEG docs for details on */
/*  what each does */
#define JMEMSYS         MEM_ANSI
#define HAVE_STDC
#define INCLUDES_ARE_ANSI

#endif /* IRIX */

/*************************************************************************/
/*************************************************************************/
/*************************************************************************/
/*
 * CPU: Cray Vector CPU (is there a name for these? :-)
 * OS: UNICOS
 */
#if (defined(UNICOS) || defined(_UNICOS)) && !defined(_CRAYMPP)

#ifndef UNICOS
#define UNICOS
#endif

#ifdef GOT_MACHINE
If you get an error on this line more than one machine type has been defined.
Please check your Makefile.
#endif
#define GOT_MACHINE 1

/* Extra include files required for this platform */
#include <memory.h>
#include <fortran.h>
#ifndef O_RDONLY
#include <fcntl.h>              /* for unbuffered i/o stuff */
#define L_INCR  1
#include <sys/stat.h>
#endif /*O_RDONLY*/

/* Set machine byte-format */
#define DF_MT   DFMT_UNICOS

/* Define portable variable types */
typedef void            VOID;
typedef void            *VOIDP;
#ifdef OLD_WAY /* May need to be included on other machines than the C-90 */
typedef char            *_fcd;    /* Fortran character descriptor type */
#endif /* OLD_WAY */
typedef signed char     char8;
typedef unsigned char   uchar8;
typedef signed char     int8;
typedef unsigned char   uint8;
typedef int             int16;
typedef unsigned int    uint16;
typedef int             int32;
typedef unsigned int    uint32;
typedef int             intn;
typedef unsigned int    uintn;
typedef float           float32;
typedef double          float64;
typedef int             intf;     /* size of INTEGERs in Fortran compiler */

/* Fortran compatibility macros */
#define DF_CAPFNAMES            /* fortran names are in all caps */
/* fcdtocp(desc) is defined in compiler header files */

/* Choose the I/O package to use when interacting with the file */
#ifdef  HAVE_FMPOOL
#define FILELIB PAGEBUFIO  /* enable page buffering */
#else
#define FILELIB POSIXBUFIO
#endif

/* JPEG #define's - Look in the JPEG docs before changing - (Q) */

/* Determine the memory manager we are going to use. Valid values are: */
/*  MEM_DOS, MEM_ANSI, MEM_NAME, MEM_NOBS.  See the JPEG docs for details on */
/*  what each does */
#define JMEMSYS         MEM_ANSI
#define RIGHT_SHIFT_IS_UNSIGNED
#define CHAR_IS_UNSIGNED

#endif /* UNICOS */

/*************************************************************************/
/*************************************************************************/
/*************************************************************************/
/*
 * CPU: Cray Vector CPU (is there a name for these? :-)
 * OS: UNICOS (on massively parallel systems, like T3D & T3E)
 */
#if defined(_CRAYMPP)

#ifndef CRAYMPP
#define CRAYMPP
#endif

#ifdef GOT_MACHINE
If you get an error on this line more than one machine type has been defined.
Please check your Makefile.
#endif
#define GOT_MACHINE 1

/* Extra include files required for this platform */
#include <memory.h>
#include <fortran.h>
#ifndef O_RDONLY
#include <fcntl.h>              /* for unbuffered i/o stuff */
#define L_INCR  1
#include <sys/stat.h>
#endif /*O_RDONLY*/

/* Set machine byte-format */
#define DF_MT   DFMT_CRAYMPP

/* Define portable variable types */
typedef void            VOID;
typedef void            *VOIDP;
#ifdef OLD_WAY /* May need to be included on other machines than the C-90 */
typedef char            *_fcd;    /* Fortran character descriptor type */
#endif /* OLD_WAY */
typedef signed char     char8;
typedef unsigned char   uchar8;
typedef signed char     int8;
typedef unsigned char   uint8;
typedef short           int16;
typedef unsigned short  uint16;
typedef short           int32;
typedef unsigned short  uint32;
typedef int             intn;
typedef unsigned int    uintn;
typedef float           float32;
typedef double          float64;
typedef int             intf;     /* size of INTEGERs in Fortran compiler */

/* Fortran compatibility macros */
#define DF_CAPFNAMES            /* fortran names are in all caps */
/* fcdtocp(desc) is defined in compiler header files */

/* Choose the I/O package to use when interacting with the file */
#ifdef  HAVE_FMPOOL
#define FILELIB PAGEBUFIO  /* enable page buffering */
#else
#define FILELIB POSIXBUFIO
#endif

/* JPEG #define's - Look in the JPEG docs before changing - (Q) */

/* Determine the memory manager we are going to use. Valid values are: */
/*  MEM_DOS, MEM_ANSI, MEM_NAME, MEM_NOBS.  See the JPEG docs for details on */
/*  what each does */
#define JMEMSYS         MEM_ANSI
#define RIGHT_SHIFT_IS_UNSIGNED
#define CHAR_IS_UNSIGNED

#endif /* CRAYMPP */


/*************************************************************************/
/*************************************************************************/
/*************************************************************************/
/*
 * CPU: Motorola 68K, PowerPC (both?)
 * OS: MacOS
 */
#if defined(MAC) || defined(macintosh) || defined(__MWERKS__) || defined (SYMANTEC_C)

#ifdef GOT_MACHINE
If you get an error on this line more than one machine type has been defined.
Please check your Makefile.
#endif
#define GOT_MACHINE 1

/* Extra include files required for this platform */
#include <memory.h>             /* malloc stuff for MPW */
#include <fcntl.h>              /* unbuffered I/O stuff for MPW */
#ifdef __MWERKS__  /* Metrowerks */
#include <sioux.h>
#include <console.h>
#endif
#ifdef SYMANTEC_C                  /* for SYMANTEC C */
#include <unix.h>
#define isascii(c)  (isprint(c) || iscntrl(c))
#else  /* MPW, possibly others */
#include <Files.h>              /* for unbuffered I/O stuff */
#endif /* SYMANTEC_C*/
#define DF_DYNAMIC              /* use dynamic allocation */

/* Set machine byte-format */
#define DF_MT   DFMT_MAC

/* Define portable variable types */
typedef void              VOID;
typedef void              *VOIDP;
typedef char              char8;
typedef unsigned char     uchar8;
typedef char              int8;
typedef unsigned char     uint8;
typedef short int         int16;
typedef unsigned short int uint16;
typedef long int          int32;
typedef unsigned long int uint32;
typedef int               intn;
typedef unsigned int      uintn;
typedef float             float32;
typedef double            float64;
typedef char              *_fcd;    /* Fortran character descriptor type */
typedef int               intf;     /* size of INTEGERs in Fortran compiler */

/* Fortran compatibility macros */
#ifndef ABSOFT
#define DF_CAPFNAMES            /* fortran names are in all caps */
#endif /* ABSOFT */
#define _fcdtocp(desc) (desc)       /* Macro to convert from Fortran character descriptor to C 'char *' */

void exit(int status);

/* Choose the I/O package to use when interacting with the file */
#define FILELIB MACIO

/* JPEG #define's - Look in the JPEG docs before changing - (Q) */

/* Determine the memory manager we are going to use. Valid values are: */
/*  MEM_DOS, MEM_ANSI, MEM_NAME, MEM_NOBS.  See the JPEG docs for details on */
/*  what each does */
#define JMEMSYS         MEM_ANSI

#endif /*MAC*/

/*************************************************************************/
/*************************************************************************/
/*************************************************************************/
/*
 * CPU: Intel x86
 * OS: MS Windows '95, Windows NT (& Dos32?), also Linux & FreeBSD
 */
/* Metrowerks compilier defines some PC stuff so need to exclude this on the MAC */
#if !(defined(__MWERKS__) || defined(MAC))

#if defined INTEL86 || defined M_I86 || defined M_I386 || defined DOS386 || defined __i386 || defined UNIX386
#ifndef INTEL86
#define INTEL86
#endif /* INTEL86 */

#if !defined UNIX386 && (defined unix || defined __unix)
#define UNIX386
#endif /* UNIX386 */

#if !defined DOS386 && defined M_I386
#define DOS386
#endif /* M_I386 && !DOS386 */

#if defined _WINDOWS || defined WIN32
#define WIN386
#endif  /* _WINDOWS | WIN32 */

#if defined WIN386 || defined DOS386 || defined UNIX386
#define INTEL386
#endif /* WIN386 | DOS386 | UNIX386 */

#ifdef GOT_MACHINE
If you get an error on this line more than one machine type has been defined.
Please check your Makefile.
#endif
#define GOT_MACHINE 1

/* Extra include files required for this platform */
#include <fcntl.h>
#ifdef UNIX386
#include <sys/types.h>      /* for unbuffered file I/O */
#include <sys/stat.h>
#include <unistd.h>
#else /* !UNIX386 */
#include <sys\types.h>      /* for unbuffered file I/O */
#include <sys\stat.h>
#include <io.h>
#include <conio.h>          /* for debugging getch() calls */
#include <malloc.h>
#endif /* UNIX386 */
#include <ctype.h>          /* for character macros */
#ifdef __WATCOMC__
#include <stddef.h>         /* for the 'fortran' pragma */
#endif
#if defined WIN386
#ifndef GMEM_MOVEABLE       /* check if windows header is already included */
#include <windows.h>        /* include the windows headers */
#include <winnt.h>
#define HAVE_BOOLEAN
#endif /* GMEM_MOVEABLE */
#endif /* WIN386 */

/* Set machine byte-format */
#define DF_MT             DFMT_PC

/* Define portable variable types */
#ifndef VOID    /* The stupid windows.h header file uses a #define instead of a typedef */
typedef void              VOID;
#endif  /* end VOID */
typedef void *            VOIDP;
typedef char              char8;
typedef unsigned char     uchar8;
typedef char              int8;
typedef unsigned char     uint8;
typedef short int         int16;
typedef unsigned short int uint16;
typedef long int          int32;
typedef unsigned long int uint32;
#ifdef __GNUC__
#define HDF5_HAVE_NATIVE_INT64
typedef long long int          int64;   /* 64-bit integers! */
typedef unsigned long long int uint64;
#endif
typedef int               intn;
typedef unsigned int      uintn;
typedef float             float32;
typedef double            float64;
typedef char              *_fcd;    /* Fortran character descriptor type */
typedef long              intf;     /* size of INTEGERs in Fortran compiler */

/* Fortran compatibility macros */
#if defined UNIX386
#define FNAME_POST_UNDERSCORE       /* Fortran function names require trailing underscore */
#elif defined INTEL386
#define DF_CAPFNAMES                /* Fortran function names need to be all-caps */
#endif
#define _fcdtocp(desc) (desc)       /* Macro to convert from Fortran character descriptor to C 'char *' */

/* Choose the I/O package to use when interacting with the file */
#if defined WIN386
#define FILELIB WINNTIO
#else
#ifdef  HAVE_FMPOOL
#define FILELIB PAGEBUFIO  /* enable page buffering */
#else
#define FILELIB POSIXBUFIO
#endif
#endif /* WIN386 */

/* JPEG #define's - Look in the JPEG docs before changing - (Q) */

/* Determine the memory manager we are going to use. Valid values are: */
/*  MEM_DOS, MEM_ANSI, MEM_NAME, MEM_NOBS.  See the JPEG docs for details on */
/*  what each does */
#define JMEMSYS         MEM_ANSI
#define HAVE_STDC
#define INCLUDES_ARE_ANSI

#endif /* INTEL86 */
#endif /* !(defined(__MWERKS__) || defined(MAC)) */


/*************************************************************************/
/*************************************************************************/
/*************************************************************************/
/*
 * CPU: Alpha
 * OS: Dec Unix (used to be called OSF/1)
 */
#if defined DEC_ALPHA || (defined __alpha && defined __unix__)

#ifndef DEC_ALPHA
#define DEC_ALPHA
#endif

#ifdef GOT_MACHINE
If you get an error on this line more than one machine type has been defined.
Please check your Makefile.
#endif
#define GOT_MACHINE

/* Extra include files required for this platform */
#include <sys/file.h>               /* for unbuffered i/o stuff */
#include <sys/stat.h>

/* Set machine byte-format */
#define DF_MT             DFMT_ALPHA

/* Define portable variable types */
typedef void              VOID;
typedef void              *VOIDP;
typedef char              char8;
typedef unsigned char     uchar8;
typedef char              int8;
typedef unsigned char     uint8;
typedef short int         int16;
typedef unsigned short int uint16;
#ifndef __rpc_types_h
typedef int               int32;
typedef unsigned int      uint32;
#endif /* __rpc_types_h */
typedef int               intn;
typedef unsigned int      uintn;
typedef float             float32;
typedef double            float64;
typedef char              *_fcd;    /* Fortran character descriptor type */
typedef int               intf;     /* size of INTEGERs in Fortran compiler */

/* Fortran compatibility macros */
#define FNAME_POST_UNDERSCORE       /* Fortran function names require trailing underscore */
#define _fcdtocp(desc) (desc)       /* Macro to convert from Fortran character descriptor to C 'char *' */

/* Choose the I/O package to use when interacting with the file */
#ifdef  HAVE_FMPOOL
#define FILELIB PAGEBUFIO  /* enable page buffering */
#else
#define FILELIB POSIXBUFIO
#endif

/* JPEG #define's - Look in the JPEG docs before changing - (Q) */

/* Determine the memory manager we are going to use. Valid values are: */
/*  MEM_DOS, MEM_ANSI, MEM_NAME, MEM_NOBS.  See the JPEG docs for details on */
/*  what each does */
#define JMEMSYS         MEM_ANSI
#ifdef __GNUC__
#define HAVE_STDC
#define INCLUDES_ARE_ANSI
#endif

#endif /* DEC_ALPHA */

#include "h5oplat.h"       /* include definitions for old, untested platforms */

/* Check if we've correctly determined the platform we are compiling in/for */
#ifndef GOT_MACHINE
No machine type has been defined.  Your Makefile needs to have someing like
-DSUN or -DUNICOS in order for the HDF internal structures to be defined
correctly.
#endif

#endif /* HDF5PLAT_H */

