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

#ifndef H5DPRIVATE_H
#define H5DPRIVATE_H

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

#include "H5Dproto.h"
#define H5D_RESERVED_ATOMS  0

/*------------------_-- Local function prototypes ----------------------------*/
static herr_t H5D_init_interface(void);

#endif /* H5DPRIVATE_H */

