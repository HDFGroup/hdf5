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
 * This file contains private information about the H5P module
 */

#ifndef _H5Pprivate_H
#define _H5Pprivate_H
#include <H5Ppublic.h>

/* Private headers needed by this file */
#include <H5private.h>
#include <H5Cprivate.h>		/*for hobjtype_t defn*/

#define H5P_RESERVED_ATOMS  1

/* Private functions */
hatom_t H5P_create(hatom_t owner_id, hobjtype_t type, const char *name);
herr_t H5P_release(hatom_t oid);

#endif
