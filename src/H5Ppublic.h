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
#define H5P_SCALAR  MAKE_ATOM(H5_DATASPACE,0)

typedef struct {
    uintn rank;
    uint32 *dims;
  } H5P_dim_t;

#ifdef __cplusplus
extern "C" {
#endif

/* Functions in H5P.c */
uintn H5Pnelem(hatom_t dim_id);

#ifdef __cplusplus
}
#endif

#endif
