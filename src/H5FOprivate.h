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

/*
 * This file contains library private information about the H5FO module
 */
#ifndef _H5FOprivate_H
#define _H5FOprivate_H

#ifdef LATER
#include "H5FOpublic.h"
#endif /* LATER */

/* Private headers needed by this file */
#include "H5private.h"
#include "H5TBprivate.h"	/* TBBTs	  		        */

/* Typedefs */

/* Typedef for open object cache */
typedef H5TB_TREE H5FO_t;       /* Currently, all open objects are stored in TBBT */

/* Macros */

/* Private routines */
H5_DLL herr_t H5FO_create(H5F_t *f);
H5_DLL hid_t H5FO_opened(const H5F_t *f, haddr_t addr);
H5_DLL herr_t H5FO_insert(H5F_t *f, haddr_t addr, hid_t id);
H5_DLL herr_t H5FO_delete(H5F_t *f, haddr_t addr);
H5_DLL herr_t H5FO_dest(H5F_t *f);

#endif /* _H5FOprivate_H */

