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
 * This file contains public declarations for the H5D module.
 */

#ifndef _H5Dpublic_H
#define _H5Dpublic_H

/* Public headers needed by this file */
#include <H5public.h>
#include <H5Apublic.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Functions in H5D.c */
herr_t H5Dset_info(hatom_t oid, hatom_t tid, hatom_t did);
herr_t H5Dwrite(hatom_t oid, hatom_t did, VOIDP buf);

#ifdef __cplusplus
}
#endif

#endif
