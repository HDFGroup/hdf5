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
#define H5ACC_DEFAULT       0x0000  /* Use in H5Fopen & H5Fcreate to open a file with default access */
#define H5ACC_WRITE         0x0001  /* Use in H5Fopen to open a file with write access */
#define H5ACC_OVERWRITE     0x0002  /* Use in H5Fcreate truncate an existing file */

#ifdef __cplusplus
extern "C" {
#endif

/* Functions in H5F.c */
hbool_t H5Fis_hdf5(const char *filename);
hid_t H5Fcreate(const char *filename, uintn flags, hid_t create_template, hid_t access_template);
hid_t H5Fopen(const char *filename, uintn flags, hid_t access_template);
herr_t H5Fclose(hid_t fid);
hid_t H5Fget_create_template (hid_t fid);

#ifdef __cplusplus
}
#endif

#endif
