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
 * This file contains private information about the H5C module
 */

#ifndef _H5Cprivate_H
#define _H5Cprivate_H
#include <H5Cpublic.h>

/* Private headers needed by this file */
#include <H5private.h>
#include <H5Fprivate.h>

/*
 * Default file-creation template values.
 */
#define H5C_USERBLOCK_DEFAULT  0       /* Default user blocks size in bytes */
#define H5C_OFFSETSIZE_DEFAULT 4       /* Default file offset size in bytes */
#define H5C_LENGTHSIZE_DEFAULT 4       /* Default file length size in bytes */
#define H5C_SYM_LEAF_K_DEFAULT	4	/* Default K for tab leaf nodes      */
	 
#define H5C_BTREE_K_DEFAULT	{					      \
   16,					/* Symbol table internal nodes	*/    \
    0,					/* unused			*/    \
    0,					/* unused			*/    \
    0,					/* unused			*/    \
    0,					/* unused			*/    \
    0,					/* unused			*/    \
    0,					/* unused			*/    \
    0					/* unused			*/    \
}

hatom_t H5C_create(hatom_t owner_id, hobjtype_t type, const char *name);
hatom_t H5C_copy(hatom_t tid);
herr_t H5C_release(hatom_t oid);
hatom_t H5C_get_default_atom(hobjtype_t type);
herr_t H5C_init(hatom_t dst_atm, const file_create_temp_t *src);

#endif
