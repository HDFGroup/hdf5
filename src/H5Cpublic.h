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
 * This file contains function prototypes for each exported function in the
 * H5C module.
 */
#ifndef _H5Cpublic_H
#define _H5Cpublic_H

/* Default Template for creation, access, etc. templates */
#define H5C_DEFAULT	(-2)

/* Public headers needed by this file */
#include <H5public.h>
#include <H5Apublic.h>

/* Template classes */
typedef enum H5C_class_t {
   H5C_NO_CLASS		=-1, 	/* Error return value			*/
   H5C_FILE_CREATE	=0,	/* File creation template		*/
   H5C_FILE_ACCESS	=1,	/* File access template			*/
   H5C_DATASET_CREATE	=2,	/* Dataset creation template		*/
   H5C_DATASET_XFER	=3,	/* Dataset transfer template		*/

   H5C_NCLASSES		=4	/* This must be last!			*/
} H5C_class_t;
   
/* Template properties, grouped by class */
typedef enum H5C_prop_t {
   
   /* File Creation Properties */
   H5F_SIZEOF_USERBLOCK,/* Size of the user block in the file in bytes	*/
   H5F_SIZEOF_ADDR,	/* Number of bytes for addresses		*/
   H5F_SIZEOF_SIZE,	/* Number of bytes for sizes			*/
   H5F_SYM_LEAF_K,	/* 1/2 rank for symbol table leaf nodes		*/
   H5F_SYM_INTERN_K,	/* 1/2 rank for symbol table internal nodes	*/
   H5F_ISTORE_K,	/* 1/2 rank for indexed storage nodes		*/
   H5F_BOOTBLOCK_VER,	/* Version # of the boot-block format		*/
   H5F_SMALLOBJECT_VER,	/* Version # of the small-object heap format	*/
   H5F_FREESPACE_VER,	/* Version # of the free-space info format	*/
   H5F_OBJECTDIR_VER,	/* Version # of the object-directory format	*/
   H5F_SHAREDHEADER_VER,/* Version # of the shared-header format	*/

   /* File Access Properties */
   /* None defined yet */

   /* Dataset Creation Properties */
   H5D_LAYOUT,		/* Storage layout				*/
   H5D_CHUNK_NDIMS, 	/* Chunk dimensionality				*/
   H5D_CHUNK_SIZE, 	/* Chunk size vector				*/
   H5D_COMPRESS, 	/* Raw data compression				*/
   H5D_PRE_OFFSET, 	/* Precompression offset			*/
   H5D_PRE_SCALE, 	/* Precompression scale				*/

   /* Dataset Transfer Properties */
   /* None defined yet */
   
} H5C_prop_t;

#ifdef __cplusplus
extern "C" {
#endif
   
/* Public functions */
hid_t H5Ccreate (H5C_class_t type);
herr_t H5Cclose (hid_t template);
hid_t H5Ccopy (hid_t template);
herr_t H5Cget_prop (hid_t template, H5C_prop_t prop, void *buf);
herr_t H5Cset_prop (hid_t template, H5C_prop_t prop, ...);
H5C_class_t H5Cget_class (hid_t template);

#ifdef __cplusplus
}
#endif

#endif
