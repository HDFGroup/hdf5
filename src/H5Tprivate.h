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
 * This file contains private information about the H5T module
 */

#ifndef H5TPRIVATE_H
#define H5TPRIVATE_H

#include "H5Tproto.h"
#define H5T_RESERVED_ATOMS  8

/* Structure for storing information about a field in a compound datatype */
typedef struct {
    char *name;         /* Name of the field */
    uintn name_off;     /* Offset of name in global small-data heap */
    uintn struct_off;   /* Offset of field within structure */
    h5_atomic_type_t dt;  /* Datatype of the field */
    hatom_t dim_id;     /* dimensionality ID of the field */
  } h5_field_info_t;

/* Structure for storing information about a compound datatype */
typedef struct {
    uintn n;            /* Number of fields */
    uintn mem_size;     /* Size of the compound structure in memory */
    uintn disk_size;    /* Size of the compound structure on disk */
    h5_field_info_t *flist;   /* List of fields in the compound object */
  } h5_compound_info_t;

/* Structure for storing information any datatype */
typedef struct {
    h5_atomic_type_t  dt;     /* Atomic type of this object */
    char *name;               /* Name of datatype */
    h5_compound_info_t *ci;   /* Information for compound datatypes */
  } h5_datatype_t;

#endif /* H5TPRIVATE_H */

