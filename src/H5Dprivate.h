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
 * This file contains private information about the H5D module
 */

#ifndef _H5Dprivate_H
#define _H5Dprivate_H
#include <H5Dpublic.h>

/* Private headers needed by this file */
#include <H5private.h>
#include <H5Cprivate.h>	/* for the hobjtype_t type */


typedef struct {
    hatom_t file;       /* ID of the file-store of this object */
    hatom_t parent;     /* ID of the parent of this object (objects in the root-directory should have the file ID here, otherwise the directory ID is here) */
    char *name;         /* Name of dataset */
    hbool_t modified;   /* Whether the dataset has been modified from version on disk */
    hatom_t type;       /* ID of Datatype of the dataset */
    hatom_t dim;        /* ID of Dimensionality of the dataset */
    haddr_t header;     /* offset of the object header for this dataset */
    haddr_t data;       /* offset of the data in the file */
  } H5D_dataset_t;

#define H5D_RESERVED_ATOMS  0

/*-----------------_-- Local function prototypes ----------------------------*/
hatom_t H5D_create(hatom_t owner_id, hobjtype_t type, const char *name);
herr_t H5D_flush(hatom_t oid);
herr_t H5D_release(hatom_t oid);

#endif
