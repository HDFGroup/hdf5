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

/*
 * This file contains private information about the H5C module
 */
#ifndef _H5Cprivate_H
#define _H5Cprivate_H

#include <H5Cpublic.h>

/* Private headers needed by this file */
#include <H5private.h>
#include <H5Fprivate.h>

hid_t H5C_create (H5C_class_t type, void *tmpl);
void *H5C_copy (H5C_class_t type, const void *src);

#endif
