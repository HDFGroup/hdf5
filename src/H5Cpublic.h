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
 * This file contains function prototypes for each exported function in the H5C module
 */

#ifndef _H5Cpublic_H
#define _H5Cpublic_H

#ifdef __cplusplus
extern "C" {
#endif

/* Public headers needed by this file */
#include <H5public.h>
#include <H5Apublic.h>
   
/* Parameters to use when retrieving file-creation template information */
typedef enum {
    H5_USERBLOCK_SIZE,          /* (uintn) Size of the user block in the file in bytes */
    H5_OFFSET_SIZE,             /* (uint8) Number of bytes for offsets */
    H5_LENGTH_SIZE,             /* (uint8) Number of bytes for lengths */
    H5_SYM_LEAF_K,              /* (uintn) 1/2 rank for symbol table leaf nodes */
    H5_SYM_INTERN_K,            /* (uintn) 1/2 rank for symbol table internal nodes */
    H5_BOOTBLOCK_VER,           /* (uint8) Version # of the boot-block format */
    H5_SMALLOBJECT_VER,         /* (uint8) Version # of the small-object heap format */
    H5_FREESPACE_VER,           /* (uint8) Version # of the free-space info format */
    H5_OBJECTDIR_VER,           /* (uint8) Version # of the object-directory format */
    H5_SHAREDHEADER_VER         /* (uint8) Version # of the shared-header format */
 } file_create_param_t;

/* Object types for "meta" interface */
typedef group_t hobjtype_t;     /* Map the object in the "meta" interface to atom groups */

/* Functions in H5C.c */
herr_t H5Cgetparm(hatom_t tid, file_create_param_t parm, VOIDP buf);
herr_t H5Csetparm(hatom_t tid, file_create_param_t parm, const VOIDP buf);
void H5C_term_interface (void);

#ifdef __cplusplus
}
#endif

#endif
