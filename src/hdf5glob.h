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
 *  Header file for global & thread-specific data
 */

#ifndef HDF5GLOB_H
#define HDF5GLOB_H

/* Library global variables */
/* There should only be a few of these, don't add more unless you have a d*mn good reason! */

#ifndef HDF5_MASTER
extern
#endif /* HDF5_MASTER */
intn library_initialize_g     /* Whether we've initialized library global information yet */
#ifdef HDF5_MASTER
= FALSE
#endif /* HDF5_MASTER */
;
#ifndef HDF5_MASTER
extern
#endif /* HDF5_MASTER */
intn install_atexit         /* Whether to install the atexit routine */
#ifdef HDF5_MASTER
= TRUE
#endif /* HDF5_MASTER */
;

/* Thread-specific variables */
#ifndef HDF5_MASTER
extern
#endif /* HDF5_MASTER */
intn thread_initialize_g      /* Whether we've initialized thread global information yet */
#ifdef HDF5_MASTER
= FALSE
#endif /* HDF5_MASTER */
;

#ifndef HDF5_MASTER
extern
#endif /* HDF5_MASTER */
int32 thrderrid;     /* Thread-specific "global" error-handler ID */

#endif /* HDF5GLOB_H */

