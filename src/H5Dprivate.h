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
#include <H5Fprivate.h>	/* for the H5F_t type */
#include <H5Gprivate.h> /* Symbol tables */
#include <H5Tprivate.h>	/* for the H5T_t type */
#include <H5Pprivate.h>	/* for the H5P_t type */
#include <H5Oprivate.h> /* Object Headers */

#define H5D_RESERVED_ATOMS  0

/* Set the minimum object header size to create objects with */
#define H5D_MINHDR_SIZE 512

/* Dataset creation template */
typedef struct H5D_create_t {
   H5D_layout_t		layout;
   intn			chunk_ndims;
   size_t		chunk_size[32];
} H5D_create_t;

/* Dataset transfer template */
typedef struct H5D_xfer_t {
   int			_placeholder; /*unused--delete this later*/
} H5D_xfer_t;

typedef struct H5D_t H5D_t;

extern const H5D_create_t H5D_create_dflt;
extern const H5D_xfer_t H5D_xfer_dflt;

/* Functions defined in H5D.c */
H5D_t *H5D_create (H5F_t *f, const char *name, const H5T_t *type,
		const H5P_t *space, const H5D_create_t *create_parms);
H5D_t *H5D_open (H5F_t *f, const char *name);
herr_t H5D_close (H5D_t *dataset);
herr_t H5D_read (H5D_t *dataset, const H5T_t *type, const H5P_t *space,
		 const H5D_xfer_t *xfer_parms, void *buf/*out*/);
herr_t H5D_write (H5D_t *dataset, const H5T_t *type, const H5P_t *space,
		  const H5D_xfer_t *xfer_parms, const void *buf);
hid_t H5D_find_name (hid_t file_id, group_t UNUSED, const char *name);
     

/* Functions defined in in H5Dconv.c */
herr_t H5D_convert_buf(void *dst,const void *src,uintn len,uintn size);

#endif
