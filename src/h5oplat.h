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

#ifndef H5OPLAT_H
#define H5OPLAT_H

/*
 *  This file contains machine definitions for older architectures which once
 *  were working, but haven't been tested in a long-time.  If you wish to use
 *  and test an architecture from this file, move the entire machine's
 *  definition from this file into the hdf5plat.h file and make certain to
 *  delete the entry in this file.  -QAK
 */

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

/*
 * CPU: Sparc (possibly Motorola 68K?)
 * OS: Solaris 1.x (?), SunOS 4.x
 */
#if defined(KNRSUN)

#if !defined(SUN)
#define SUN
#endif

#ifdef GOT_MACHINE
If you get an error on this line more than one machine type has been defined.
Please check your Makefile.
#endif
#define GOT_MACHINE

/* Extra include files required for this platform */
#   define BSD
#define DUMBCC 	/* because it is.  for later use in macros */
#ifndef __GNUC__
#include <memory.h>
#endif /* __GNUC__ */
#include <unistd.h>
#include <sys/file.h>               /* for unbuffered i/o stuff */
#include <sys/stat.h>

/* Set machine byte-format */
#define DF_MT             DFMT_SUN

/* Define portable variable types */
typedef void              VOID;
typedef char              *VOIDP;
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
typedef int               intf;     /* size of INTEGERs in Fortran compiler */
typedef float             float32;
typedef double            float64;

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

#endif /* SUN */

/*
 * CPU: Vax & Alpha (yeah, right!  this definition is probably not really that portable! -QAK )
 * OS: VMS, OpenVMS
 */
#if defined(VMS) || defined(vms)

#ifdef GOT_MACHINE
If you get an error on this line more than one machine type has been defined.
Please check your Makefile.
#endif
#define GOT_MACHINE 1

/* Extra include files required for this platform */
#include <file.h>               /* for unbuffered i/o stuff */
#include <sys/stat.h>

/* Set machine byte-format */
#define DF_MT              DFMT_VAX

/* Define portable variable types */
typedef void               VOID;
typedef void              *VOIDP;
typedef char              *_fcd;    /* Fortran character descriptor type */
typedef char               char8;
typedef unsigned char      uchar8;
typedef char               int8;
typedef unsigned char      uint8;
typedef short int          int16;
typedef unsigned short int uint16;
#ifdef __alpha
typedef int                int32;
typedef unsigned int       uint32;
#else
typedef long int           int32;
typedef unsigned long int  uint32;
#endif
typedef int                intn;
typedef unsigned int       uintn;
typedef float              float32;
typedef double             float64;
typedef int                intf;     /* size of INTEGERs in Fortran compiler */

/* Fortran compatibility macros */
#define DF_CAPFNAMES            /* fortran names are in all caps */
#define _fcdtocp(desc)  ((char *) *((char **) &desc[4])) /* Macro to convert from Fortran character descriptor to C 'char *' */

/* Choose the I/O package to use when interacting with the file */
#define FILELIB POSIXBUFIO

/* 
  Redef a couple of C routine names to avoid conflicts
  since the VMS link command is case-insensitive
*/
#include "dfivms.h"


/* JPEG #define's - Look in the JPEG docs before changing - (Q) */

/* Determine the memory manager we are going to use. Valid values are: */
/*  MEM_DOS, MEM_ANSI, MEM_NAME, MEM_NOBS.  See the JPEG docs for details on */
/*  what each does */
#define JMEMSYS         MEM_ANSI

#endif /* VMS */

/*
 * CPU: HP PA-RISC
 * OS: ConvexOS
 */
#if defined(CONVEX) || defined(CONVEXNATIVE) || defined(__convexc__)

#ifndef CONVEX
#define CONVEX
#endif

#ifdef GOT_MACHINE
If you get an error on this line more than one machine type has been defined.
Please check your Makefile.
#endif
#define GOT_MACHINE

/* Extra include files required for this platform */
#include <sys/types.h>
#include <sys/stat.h>
/* Set machine byte-format */
#ifdef CONVEXNATIVE 
/* For Convex machines with native format floats */
#define DF_MT             DFMT_CONVEXNATIVE
#else
#define DF_MT             DFMT_CONVEX
#endif

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
#define RIGHT_SHIFT_IS_UNSIGNED
#define INCLUDES_ARE_ANSI
#define HAVE_STDC

#endif /* CONVEX */


/*
 * CPU: MIPS
 * OS: Ultrix
 */
#if defined(MIPSEL) || ((defined(mips) || defined(__mips)) && (defined(ultrix) || defined(__ultrix)))

#ifndef MIPSEL
#define MIPSEL
#endif

#ifdef GOT_MACHINE
If you get an error on this line more than one machine type has been defined.
Please check your Makefile.
#endif
#define GOT_MACHINE 1

#ifndef __GNUC__
#define DUMBCC 	/* because it is.  for later use in macros -QAK */
#endif /* __GNUC__ */

/* Extra include files required for this platform */
#include <sys/types.h>
#include <sys/file.h>               /* for unbuffered i/o stuff */
#include <sys/stat.h>

/* Set machine byte-format */
#define DF_MT   DFMT_MIPSEL

/* Define portable variable types */
typedef void            VOID;
typedef void            *VOIDP;
typedef char            char8;
typedef unsigned char   uchar8;
typedef char            int8;
typedef unsigned char   uint8;
typedef short           int16;
typedef unsigned short  uint16;
typedef int             int32;
typedef unsigned int    uint32;
typedef int             intn;
typedef unsigned int    uintn;
typedef float           float32;
typedef double          float64;
typedef char              *_fcd;    /* Fortran character descriptor type */
typedef int             intf;     /* size of INTEGERs in Fortran compiler */

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

#endif /* MIPSEL */

/*
 * CPU: Motorola 68K
 * OS: NeXTSTEP
 */
#if defined(NEXT) || defined(NeXT)

#ifndef NEXT
#define NEXT
#endif

#ifdef GOT_MACHINE
If you get an error on this line more than one machine type has been defined.
Please check your Makefile.
#endif
#define GOT_MACHINE

/* Extra include files required for this platform */
#define isascii(c)  (isprint(c) || iscntrl(c))
#ifndef __GNUC__
#include <memory.h>
#endif /* __GNUC__ */
#include <sys/file.h>               /* for unbuffered i/o stuff */
#include <sys/stat.h>

/* Set machine byte-format */
#define DF_MT             DFMT_NEXT

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

#endif /* NEXT */

/*
 * CPU: Motorola 88K (obscure CISC chip)
 * OS: ?
 */
#if defined(MOTOROLA) || defined(m88k)

#ifdef GOT_MACHINE
If you get an error on this line more than one machine type has been defined.
Please check your Makefile.
#endif
#define GOT_MACHINE

/* Extra include files required for this platform */
#ifndef __GNUC__
#include <memory.h>
#endif /* __GNUC__ */
#include <unistd.h>
#include <sys/file.h>               /* for unbuffered i/o stuff */
#include <sys/stat.h>
#ifndef O_RDONLY
#include <fcntl.h>              /* for unbuffered i/o stuff */
#endif /*O_RDONLY*/

/* Set machine byte-format */
#define DF_MT             DFMT_MOTOROLA

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
#define FILELIB POSIXBUFIO

/* JPEG #define's - Look in the JPEG docs before changing - (Q) */

/* Determine the memory manager we are going to use. Valid values are: */
/*  MEM_DOS, MEM_ANSI, MEM_NAME, MEM_NOBS.  See the JPEG docs for details on */
/*  what each does */
#define JMEMSYS         MEM_ANSI

#endif /* MOTOROLA */


/*
 * CPU: ? (Fujitsu VP series)
 * OS: ? (UNIX)
 */
#if defined VP | defined __uxpm__

#ifndef VP
#define VP
#endif

#ifdef GOT_MACHINE
If you get an error on this line more than one machine type has been defined.
Please check your Makefile.
#endif
#define GOT_MACHINE 1

/* Extra include files required for this platform */
#include <memory.h>
#include <sys/types.h>
#include <sys/stat.h>

/* Set machine byte-format */
#define DF_MT              DFMT_VP

/* Define portable variable types */
typedef void                VOID;
typedef void               *VOIDP;
typedef char               char8;
typedef unsigned char      uchar8;
typedef char               int8;
typedef unsigned char      uint8;
typedef short int          int16;
typedef unsigned short int uint16;
typedef long int           int32;
typedef unsigned long int  uint32;
typedef int                intn;
typedef unsigned int       uintn;
typedef float              float32;
typedef double             float64;
typedef char              *_fcd;    /* Fortran character descriptor type */
typedef int               intf;     /* size of INTEGERs in Fortran compiler */

/* Fortran compatibility macros */
#define FNAME_POST_UNDERSCORE       /* Fortran function names require trailing underscore */
#define _fcdtocp(desc) (desc)       /* Macro to convert from Fortran character descriptor to C 'char *' */

/* Choose the I/O package to use when interacting with the file */
#define FILELIB POSIXBUFIO

/* JPEG #define's - Look in the JPEG docs before changing - (Q) */

/* Determine the memory manager we are going to use. Valid values are: */
/*  MEM_DOS, MEM_ANSI, MEM_NAME, MEM_NOBS.  See the JPEG docs for details on */
/*  what each does */
#define JMEMSYS         MEM_ANSI

#endif /* VP */

/*
 * CPU: Intel I860 (in Paragon system)
 * OS: ? (UNIX)
 */
#if defined I860 | defined i860

#ifndef I860
#define I860
#endif

#ifdef GOT_MACHINE
If you get an error on this line more than one machine type has been defined.
Please check your Makefile.
#endif
#define GOT_MACHINE 1

/* Extra include files required for this platform */
#include <sys/types.h>
#include <sys/file.h>           /* for unbuffered i/o stuff */
#include <sys/stat.h>
#include <unistd.h>             /* mis-using def. for SEEK_SET, but oh well */

/* Set machine byte-format */
#define DF_MT   DFMT_I860

/* Define portable variable types */
typedef void            VOID;
typedef void            *VOIDP;
typedef char            char8;
typedef unsigned char   uchar8;
typedef char            int8;
typedef unsigned char   uint8;
typedef short           int16;
typedef unsigned short  uint16;
typedef int             int32;
typedef unsigned int    uint32;
typedef int             intn;
typedef unsigned int    uintn;
typedef float           float32;
typedef double          float64;
typedef char              *_fcd;    /* Fortran character descriptor type */
typedef int             intf;     /* size of INTEGERs in Fortran compiler */

/* Fortran compatibility macros */
#define FNAME_POST_UNDERSCORE       /* Fortran function names require trailing underscore */
#define _fcdtocp(desc) (desc)       /* Macro to convert from Fortran character descriptor to C 'char *' */

/* Choose the I/O package to use when interacting with the file */
#define FILELIB POSIXBUFIO

/* JPEG #define's - Look in the JPEG docs before changing - (Q) */

/* Determine the memory manager we are going to use. Valid values are: */
/*  MEM_DOS, MEM_ANSI, MEM_NAME, MEM_NOBS.  See the JPEG docs for details on */
/*  what each does */
#define JMEMSYS         MEM_ANSI

#endif /* I860 */

#endif /* H5OPLAT_H */

