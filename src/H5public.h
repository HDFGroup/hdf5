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

/*
 * This file contains public declarations for the H5 module.
 */
#ifndef _H5public_H
#define _H5public_H

#include <H5config.h>           /*from configure                             */
#include <sys/types.h>
#ifdef HAVE_PARALLEL
#  include <mpi.h>
#  include <mpio.h>
#endif

/*
 * Status return values.  Failed integer functions in HDF5 result almost
 * always in a negative value (unsigned failing functions sometimes return
 * zero for failure) while successfull return is non-negative (often zero).
 * The negative failure value is most commonly -1, but don't bet on it.  The
 * proper way to detect failure is something like:
 *
 * 	if ((dset = H5Dopen (file, name))<0) {
 *	   fprintf (stderr, "unable to open the requested dataset\n");
 *	}
 */
typedef int herr_t;

/*
 * Boolean type.
 */
typedef int hbool_t;

/*
 * The sizes of file-objects in hdf5 have there own types defined here.  On
 * most systems, these are the same as size_t and ssize_t, but on systems
 * with small address spaces these are defined to be larger.
 */
#if 1
typedef size_t hsize_t;
typedef ssize_t hssize_t;
#else
typedef unsigned long long hsize_t;
typedef signed long long hssize_t;
#endif

#ifdef __cplusplus
extern "C" {
#endif

/* Functions in H5.c */
herr_t H5open (void);
herr_t H5close (void);
herr_t H5dont_atexit (void);
herr_t H5version (unsigned *majnum, unsigned *minnum, unsigned *relnum,
		  unsigned *patnum);

#ifdef __cplusplus
}
#endif
#endif
