/****************************************************************************
 * NCSA HDF								    *
 * Software Development Group						    *
 * National Center for Supercomputing Applications			    *
 * University of Illinois at Urbana-Champaign				    *
 * 605 E. Springfield, Champaign IL 61820				    *
 *									    *
 * For conditions of distribution and use, see the accompanying		    *
 * hdf/COPYING file.							    *
 *									    *
 ****************************************************************************/

/*
 * This file contains macros & information for MPI-IO file access
 */

#ifndef _H5Fmpioprivate_H
#define _H5Fmpioprivate_H

#ifdef HAVE_PARALLEL
extern	hbool_t H5_mpi_1_metawrite_g;
hbool_t H5F_mpio_tas_allsame(H5F_low_t *lf, hbool_t newval );
#endif /* HAVE_PARALLEL */

#endif
