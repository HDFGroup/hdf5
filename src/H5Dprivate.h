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
#include <H5Gprivate.h> /* Symbol tables */
#include <H5Tprivate.h>	/* for the h5_datatype_t type */
#include <H5Pprivate.h>	/* for the H5P_sdim_t type */
#include <H5Oprivate.h> /* Object Headers */

#define H5D_RESERVED_ATOMS  0

/* Set the minimum object header size to create objects with */
#define H5D_MINHDR_SIZE 512

/*-----------------_-- Local function prototypes ----------------------------*/
hid_t H5D_create(hid_t owner_id, hobjtype_t type, const char *name);
hid_t H5D_access_by_name (hid_t owner_id,  const char *name); 
hid_t H5D_find_name(hid_t owner_id, hobjtype_t type, const char *name);
herr_t H5D_flush(hid_t oid);
herr_t H5D_release(hid_t oid);
/* in H5Dconv.c */
herr_t H5D_convert_buf(void *dst,const void *src,uintn len,uintn size);

#endif
