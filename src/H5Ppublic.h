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
 * This file contains public declarations for the H5P module.
 */

#ifndef _H5pproto_H
#define _H5Pproto_H

/* Public headers needed by this file */
#include <H5public.h>
#include <H5Apublic.h>

/* Define atomic datatypes */
#define H5P_SCALAR  MAKE_ATOM(H5_DATASPACE,0)   /* Atom for scalar dataspace */

/* Different types of dataspaces */
#define H5P_TYPE_UNKNOWN    0 /* Dataspace is not unitialized */
#define H5P_TYPE_SIMPLE     1 /* Dataspace is simple */
#define H5P_TYPE_COMPLEX    2 /* Dataspace is complex */

typedef struct {
    uintn type;     /* Type of dimensionality object */
    struct H5P_sdim_t *s;  /* Pointer to simple dimensionality information */
  } H5P_dim_t;

#ifdef __cplusplus
extern "C" {
#endif

/* Functions in H5P.c */
uintn H5Pnelem(hatom_t dim_id);
herr_t H5Pset_space(hatom_t sid, uint32 rank, uint32 *dims);
void H5P_term_interface (void);

#ifdef __cplusplus
}
#endif

#endif
