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
 * This file contains public declarations for the H5 module.
 */

#ifndef _H5public_H
#define _H5public_H
#include <H5config.h>			/* From configure		*/
#include <sys/types.h>

/*
 * Data types
 */
typedef void		VOID;
typedef void		*VOIDP;
typedef char		char8;
typedef signed char	int8;
typedef unsigned char	uchar8, uint8;

#if SIZEOF_SHORT==2
typedef short		int16;
typedef unsigned short	uint16;
#else
typedef int		int16;		/*not really*/
typedef unsigned	uint16;		/*not really*/
#endif

#if SIZEOF_INT==4
typedef int		int32;
typedef unsigned int	uint32;
#elif SIZEOF_LONG==4
typedef long		int32;
typedef unsigned long	uint32;
#else
typedef int		int32;		/*not really*/
typedef unsigned	uint32;		/*not really*/
#endif

#if SIZEOF_INT==8
typedef int		int64;
typedef unsigned	uint64;
#elif SIZEOF_LONG==8
typedef long		int64;
typedef unsigned long	uint64;
#elif SIZEOF_LONG_LONG==8
typedef long long	int64;
typedef unsigned long long uint64;
#else
#  error "no 64-bit integer type"
#endif

#if SIZEOF_FLOAT==4
typedef float		float32;
#else
typedef float		float32;	/*not really*/
#endif

#if SIZEOF_FLOAT==8
typedef float		float64;
#elif SIZEOF_DOUBLE==8
typedef double		float64;
#else
#  error "no 64-bit floating point type"
#endif

/*
 * Define a type for generic integers.  Use this instead of `int' to
 * show that some thought went into the algorithm.
 */
typedef int		intn;
typedef unsigned	uintn;

/*
 * Status return values.
 * Since some unix/c routines use 0 and -1 (or more precisely, non-negative
 * vs. negative) as their return code, and some assumption had been made in
 * the code about that, it is important to keep these constants the same
 * values.  When checking the success or failure of an integer-valued
 * function, remember to compare against zero and not one of these two
 * values.
 */
typedef intn		herr_t;
#define SUCCEED 	0
#define FAIL 		(-1)
#define UFAIL 		(unsigned)(-1)

/*
 * Boolean type.
 */
typedef enum {
   BFAIL	=(-1),		/*error value*/
   BFALSE	=0,
   BTRUE	=1
} hbool_t;

#ifdef __cplusplus
extern "C" {
#endif

/* Functions in H5.c */
herr_t H5init (void);
herr_t H5dont_atexit(void);
herr_t H5version(uintn *majnum, uintn *minnum, uintn *relnum, uintn *patnum);

#ifdef __cplusplus
}
#endif

#endif
