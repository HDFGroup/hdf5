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

#ifdef RCSID
static char             RcsId[] = "@(#)$Revision$";
#endif

/* $Id$ */


/*
 * This file contains public declarations for the HDF5 module.
 */
#ifndef _H5public_H
#define _H5public_H

/*
 * Undefine things that might get redefined in the H5config.h file if hdf5 is
 * being included by other packages that use autoconf. The problem is that
 * if the C preprocessor emits warning messages about redefinitions then
 * autoconf will become confused and think that the hdf5 header file doesn't
 * exist.
 */
#undef SIZEOF_INT8_T
#undef SIZEOF_INT_FAST8_T
#undef SIZEOF_INT_LEAST8_T
#undef SIZEOF_UINT8_T
#undef SIZEOF_UINT_FAST8_T
#undef SIZEOF_UINT_LEAST8_T

#undef SIZEOF_INT16_T
#undef SIZEOF_INT_FAST16_T
#undef SIZEOF_INT_LEAST16_T
#undef SIZEOF_UINT16_T
#undef SIZEOF_UINT_FAST16_T
#undef SIZEOF_UINT_LEAST16_T

#undef SIZEOF_INT32_T
#undef SIZEOF_INT_FAST32_T
#undef SIZEOF_INT_LEAST32_T
#undef SIZEOF_UINT32_T
#undef SIZEOF_UINT_FAST32_T
#undef SIZEOF_UINT_LEAST32_T

#undef SIZEOF_INT64_T
#undef SIZEOF_INT_FAST64_T
#undef SIZEOF_INT_LEAST64_T
#undef SIZEOF_UINT64_T
#undef SIZEOF_UINT_FAST64_T
#undef SIZEOF_UINT_LEAST64_T

#undef SIZEOF___INT64_T


#include <H5config.h>           /*from configure                             */
#include <sys/types.h>
#ifdef STDC_HEADERS
#   include <limits.h>		/*for H5T_NATIVE_CHAR defn in H5Tpublic.h    */
#endif

#ifdef HAVE_STDDEF_H
#   include <stddef.h>
#endif
#ifdef HAVE_PARALLEL
#   include <mpi.h>
#ifndef MPI_FILE_NULL		/*MPIO may be defined in mpi.h already*/
#   include <mpio.h>
#endif
#endif
#include <H5api_adpt.h>

/* Version numbers */
#define H5_VERS_MAJOR	1	/* For major interface/format changes  	     */
#define H5_VERS_MINOR	2	/* For minor interface/format changes  	     */
#define H5_VERS_RELEASE	2	/* For tweaks, bug-fixes, or development     */
#define H5_VERS_SUBRELEASE "-pre0"	/* For pre-releases like -pre1, -beta.       */
				/* Empty string for real releases.           */

#define H5check()	H5check_version(H5_VERS_MAJOR,H5_VERS_MINOR,	      \
				        H5_VERS_RELEASE)

/*
 * Status return values.  Failed integer functions in HDF5 result almost
 * always in a negative value (unsigned failing functions sometimes return
 * zero for failure) while successfull return is non-negative (often zero).
 * The negative failure value is most commonly -1, but don't bet on it.  The
 * proper way to detect failure is something like:
 *
 * 	if ((dset = H5Dopen (file, name))<0) {
 *	    fprintf (stderr, "unable to open the requested dataset\n");
 *	}
 */
typedef int herr_t;


/*
 * Boolean type.  Successful return values are zero (false) or positive
 * (true). The typical true value is 1 but don't bet on it.  Boolean
 * functions cannot fail.  Functions that return `htri_t' however return zero
 * (false), positive (true), or negative (failure). The proper way to test
 * for truth from a htri_t function is:
 *
 * 	if ((retval = H5Tcommitted(type))>0) {
 *	    printf("data type is committed\n");
 *	} else if (!retval) {
 * 	    printf("data type is not committed\n");
 *	} else {
 * 	    printf("error determining whether data type is committed\n");
 *	}
 */
typedef unsigned int hbool_t;
typedef int htri_t;

/*
 * The sizes of file objects have their own types defined here.  If large
 * sizes are enabled then use a 64-bit data type, otherwise use the size of
 * memory objects.
 */
#ifdef HAVE_LARGE_HSIZET
#   if SIZEOF_LONG_LONG>=8
typedef unsigned long long 	hsize_t;
typedef signed long long	hssize_t;
#   elif SIZEOF___INT64>=8
typedef unsigned __int64	hsize_t;
typedef signed __int64		hssize_t;
#   endif
#else
typedef size_t			hsize_t;
typedef ssize_t			hssize_t;
#endif

#ifdef __cplusplus
extern "C" {
#endif

/* Clean up all these symbols */
#undef SIZEOF_INT8_T
#undef SIZEOF_INT_FAST8_T
#undef SIZEOF_INT_LEAST8_T
#undef SIZEOF_UINT8_T
#undef SIZEOF_UINT_FAST8_T
#undef SIZEOF_UINT_LEAST8_T

#undef SIZEOF_INT16_T
#undef SIZEOF_INT_FAST16_T
#undef SIZEOF_INT_LEAST16_T
#undef SIZEOF_UINT16_T
#undef SIZEOF_UINT_FAST16_T
#undef SIZEOF_UINT_LEAST16_T

#undef SIZEOF_INT32_T
#undef SIZEOF_INT_FAST32_T
#undef SIZEOF_INT_LEAST32_T
#undef SIZEOF_UINT32_T
#undef SIZEOF_UINT_FAST32_T
#undef SIZEOF_UINT_LEAST32_T

#undef SIZEOF_INT64_T
#undef SIZEOF_INT_FAST64_T
#undef SIZEOF_INT_LEAST64_T
#undef SIZEOF_UINT64_T
#undef SIZEOF_UINT_FAST64_T
#undef SIZEOF_UINT_LEAST64_T

#undef SIZEOF___INT64_T

/* Functions in H5.c */
__DLL__ herr_t H5open(void);
__DLL__ herr_t H5close(void);
__DLL__ herr_t H5dont_atexit(void);
__DLL__ herr_t H5garbage_collect(void);
__DLL__ herr_t H5get_libversion(unsigned *majnum, unsigned *minnum,
				unsigned *relnum);
__DLL__ herr_t H5check_version(unsigned majnum, unsigned minnum,
			       unsigned relnum);

#ifdef __cplusplus
}
#endif
#endif
