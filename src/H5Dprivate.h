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
#include <H5Tprivate.h>	/* for the h5_datatype_t type */
#include <H5Pprivate.h>	/* for the H5P_sdim_t type */

typedef struct {
    hatom_t file;       /* ID of the file-store of this object */
    char *name;         /* Name of dataset */
    hbool_t modified;   /* Whether the dataset has been modified from version on disk */
    h5_datatype_t *type;    /* Pointer to datatype of the dataset */
    H5P_sdim_t *dim;    /* Pointer to dimensionality of the dataset */
    haddr_t header;     /* offset of the object header for this dataset */
    haddr_t data;       /* offset of the data in the file */
  } H5D_dataset_t;

#define H5D_RESERVED_ATOMS  0

/* Set the minimum object header size to create objects with */
#define H5D_MINHDR_SIZE 512

/*-----------------_-- Local function prototypes ----------------------------*/
hatom_t H5D_create(hatom_t owner_id, hobjtype_t type, const char *name);
herr_t H5D_flush(hatom_t oid);
herr_t H5D_release(hatom_t oid);

#endif
