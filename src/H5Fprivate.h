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
 * This file contains macros & information for file access
 */

#ifndef HDF5FILE_H
#define HDF5FILE_H

#include "H5Fproto.h"   /* Include Public Definitions */

	     /*****************************************/
	     /*** This is the top level header file ***/
	     /*** and shouldn't depend on any other ***/
	     /*** packages!                         ***/
	     /*****************************************/

/* Maximum size of boot-block */
#define H5F_BOOTBLOCK_SIZE  1024


#define H5F_SIZEOF_OFFSET(F) ((F)->file_create_parms.offset_size)
#define H5F_SIZEOF_SIZE(F)	((F)->file_create_parms.length_size)

/* Define the structure to store the file information for HDF5 files */
typedef struct {
    char *dir;              /* Directory the file is located within */
    char *filename;         /* Filename of file */
    uintn acc_perm;         /* Access Permissions for file */
    hdf_file_t file_handle; /* File handle for actual I/O */
    uintn ref_count;        /* Reference count for number of times file is opened */
    uint32 consist_flags;   /* File Consistency Flags */
    haddr_t smallobj_off;    /* Offset of small-object heap within the file */
    haddr_t freespace_off;   /* Offset of free-space info within the file */
    size_t logical_len;     /* Logical length of file */
    struct H5AC_cache_t *cache; /* The object cache */
    file_create_temp_t file_create_parms;    /* File-creation template parameters */
#ifdef LATER
    file_access_temp_t file_access_parms;    /* File-access template parameters */
#endif /* LATER */
    struct H5G_entry_t *root_sym; /* Extra for the root symbol in the file */
  } hdf5_file_t;


#ifdef NOT_YET
#define H5F_encode_offset(f,p,o) (H5F_SIZEOF_OFFSET(f)==4 ? UINT32ENCODE(p,o) \
    : H5F_SIZEOF_OFFSET(f)==8 ? UINT64ENCODE(p,o) \
    : H5F_SIZEOF_OFFSET(f)==2 ? UINT16ENCODE(p,o) \
    : H5FPencode_unusual_offset(f,&(p),(uint8 *)&(o)))
#else /* NOT_YET */
#define H5F_encode_offset(f,p,o) switch(H5F_SIZEOF_OFFSET(f)) { case 4: UINT32ENCODE(p,o); break;\
    case 8: UINT64ENCODE(p,o); break;\
    case 2: UINT16ENCODE(p,o); break;}
#endif /* NOT_YET */

#define H5F_decode_offset(f,p,o)					      \
   switch (H5F_SIZEOF_OFFSET (f)) {					      \
   case 4:								      \
      UINT32DECODE (p, o);						      \
      break;								      \
   case 8:								      \
      UINT64DECODE (p, o);						      \
      break;								      \
   case 2:								      \
      UINT16DECODE (p, o);						      \
      break;								      \
   }

#ifdef NOT_YET
#define H5F_encode_length(f,p,l) (H5F_SIZEOF_SIZE(f)==4 ? UINT32ENCODE(p,l) \
    : H5F_SIZEOF_SIZE(f)==8 ? UINT64ENCODE(p,l) \
    : H5F_SIZEOF_SIZE(f)==2 ? UINT16ENCODE(p,l) : H5FPencode_unusual_length(f,&(p),(uint8 *)&(l)))
#else /* NOT_YET */
#define H5F_encode_length(f,p,l) switch(H5F_SIZEOF_SIZE(f)) { case 4: UINT32ENCODE(p,l); break;\
    case 8: UINT64ENCODE(p,l); break;\
    case 2: UINT16ENCODE(p,l); break;}
#endif /* NOT_YET */

#define H5F_decode_length(f,p,l) switch(H5F_SIZEOF_SIZE(f)) { case 4: UINT32DECODE(p,l); break;\
    case 8: UINT64DECODE(p,l); break;\
    case 2: UINT16DECODE(p,l); break;}

/* Private functions, not part of the publicly documented API */
void H5F_encode_length_unusual(const hdf5_file_t *f, uint8 **p, uint8 *l);
void H5F_encode_offset_unusual(const hdf5_file_t *f, uint8 **p, uint8 *o);
intn H5F_compare_filename(const VOIDP obj, const VOIDP key);
herr_t H5F_block_read (hdf5_file_t *f, haddr_t addr, size_t size, void *buf);
herr_t H5F_block_write (hdf5_file_t *f, haddr_t addr, size_t size, void *buf);
herr_t H5F_debug (hdf5_file_t *f, haddr_t addr, FILE *stream, intn indent,
		  intn fwidth);

#endif /* HDF5FILE_H */

