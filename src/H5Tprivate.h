/****************************************************************************
 * NCSA HDF								    *
 * Software Development Group						    *
 * National Center for Supercomputing Applications			    *
 * University of Illinois at Urbana-Champaign				    *
 * 605 E. Springfield, Champaign IL 61820				    *
 *									    *
 * For conditions of distribution and use, see the accompanying		    *
 * hdf/COPYING file.							    *
 *									    *
 ****************************************************************************/

/* $Id$ */

/*
 * This file contains private information about the H5T module
 */

#ifndef _H5Tprivate_H
#define _H5Tprivate_H
#include <H5Tpublic.h>

/* Private headers needed by this file */
#include <H5private.h>
#include <H5Gprivate.h>		/*for H5G_entry_t			*/

#define H5T_RESERVED_ATOMS  8

typedef struct H5T_t H5T_t;

herr_t H5T_init (void);


/* Private functions */
herr_t H5T_init_interface (void);
H5T_t *H5T_create (H5T_class_t type, size_t size);
H5T_t *H5T_copy (const H5T_t *old_dt);
herr_t H5T_close (H5T_t *dt);
size_t H5T_get_size (const H5T_t *dt);
intn H5T_cmp (const H5T_t *dt1, const H5T_t *dt2);

herr_t H5T_insert_member (H5T_t *parent, const char *name, off_t offset,
			  const H5T_t *member);
#endif
