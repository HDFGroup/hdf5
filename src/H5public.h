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

/* Include files for public use... */
#include <H5pubconf.h>		/*from configure                             */
#include <sys/types.h>
#ifdef H5_STDC_HEADERS
#   include <limits.h>		/*for H5T_NATIVE_CHAR defn in H5Tpublic.h    */
#endif
#ifdef H5_HAVE_STDINT_H
#   include <stdint.h>		/*for C9x types				     */
#endif
#ifdef H5_HAVE_STDDEF_H
#   include <stddef.h>
#endif
#ifdef H5_HAVE_PARALLEL
#   include <mpi.h>
#ifndef MPI_FILE_NULL		/*MPIO may be defined in mpi.h already*/
#   include <mpio.h>
#endif
#endif

#ifdef H5_HAVE_GASS
#include "globus_common.h"
#include "globus_gass_file.h"
#endif

#include <H5api_adpt.h>

/* Version numbers */
#define H5_VERS_MAJOR	1	/* For major interface/format changes  	     */
#define H5_VERS_MINOR	3	/* For minor interface/format changes  	     */
#define H5_VERS_RELEASE	12	/* For tweaks, bug-fixes, or development     */
#define H5_VERS_SUBRELEASE ""	/* For pre-releases like -pre1, -beta.       */
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
#ifdef H5_HAVE_LARGE_HSIZET
#   if H5_SIZEOF_LONG_LONG>=8
typedef unsigned long long 	hsize_t;
typedef signed long long	hssize_t;
#   elif H5_SIZEOF___INT64>=8
typedef unsigned __int64	hsize_t;
typedef signed __int64		hssize_t;
#   endif
#else
typedef size_t			hsize_t;
typedef ssize_t			hssize_t;
#endif

/*
 * File addresses have there own types.
 */
#if H5_SIZEOF_UINT64_T>=8
    typedef uint64_t		haddr_t;
#   define HADDR_UNDEF		((haddr_t)(int64_t)(-1))
#elif H5_SIZEOF_INT>=8
    typedef unsigned		haddr_t;
#   define HADDR_UNDEF		((haddr_t)(-1))
#elif H5_SIZEOF_LONG>=8
    typedef unsigned long	haddr_t;
#   define HADDR_UNDEF		((haddr_t)(long)(-1))
#elif H5_SIZEOF_LONG_LONG>=8
    typedef unsigned long long	haddr_t;
#   define HADDR_UNDEF		((haddr_t)(long long)(-1))
#elif H5_SIZEOF___INT64>=8
    typedef unsigned __int64	haddr_t;
#   define HADDR_UNDEF		((haddr_t)(__int64)(-1))
#else
#   error "nothing appropriate for haddr_t"
#endif
#define HADDR_MAX		(HADDR_UNDEF-1)

#ifdef __cplusplus
extern "C" {
#endif

/* Functions in H5.c */
__DLL__ herr_t H5open(void);
__DLL__ herr_t H5close(void);
__DLL__ herr_t H5dont_atexit(void);
__DLL__ herr_t H5get_libversion(unsigned *majnum, unsigned *minnum,
				unsigned *relnum);
__DLL__ herr_t H5check_version(unsigned majnum, unsigned minnum,
			       unsigned relnum);

#ifdef __cplusplus
}
#endif
#endif
