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
 * This file contains public declarations for the H5F module.
 */

#ifndef _H5Fpublic_H
#define _H5Fpublic_H

/* Public header files needed by this file */
#include <H5public.h>
#include <H5Apublic.h>

/* file access codes */
#define H5ACC_WRITE         0x0001  /* Use in H5Fopen to open a file with write access */
#define H5ACC_OVERWRITE     0x0002  /* Use in H5Fcreate truncate an existing file */

/* Type of root rymbol-table entry */
typedef enum {
    H5F_ROOT_NONE=0,         /* Root-symbol table is empty, neither a dataset nor a directory is the root object */
    H5F_ROOT_UNKNOWN,        /* Don't know if the root object is a dataset or a directory */
    H5F_ROOT_DATASET,        /* Root object is a dataset */
    H5F_ROOT_DIRECTORY,      /* Root object is a directory */
    H5F_ROOT_ERROR           /* Error value */
  } H5F_root_symtype_t;

#ifdef __cplusplus
extern "C" {
#endif

/* Functions in H5F.c */
hbool_t H5Fis_hdf5(const char *filename);
hatom_t H5Fcreate(const char *filename, uintn flags, hatom_t create_template, hatom_t access_template);
hatom_t H5Fopen(const char *filename, uintn flags, hatom_t access_template);
herr_t H5Fclose(hatom_t fid);
hatom_t H5Fget_create_template(hatom_t fid);

#ifdef __cplusplus
}
#endif

#endif
