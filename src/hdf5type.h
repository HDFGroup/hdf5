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
 *  Header file for library-global generic typedefs
 */

#ifndef HDF5TYPE_H
#define HDF5TYPE_H

/* 
 * Define the standard error-value return type.  This type should be used for
 * functions which return SUCCEED/FAIL, instead of intn...
 */
typedef intn herr_t;    /* Generic error-value type */

/* Object types for "meta" interface */
typedef group_t hobjtype_t;     /* Map the object in the "meta" interface to atom groups */

/* File-creation template information structure */
typedef struct {
    /* These object aren't ref. counted, I can't think of a good reason why you'd access each one more than once */
    /* uintn ref_count;            Reference count for number of times object is accessed */
    uintn userblock_size;       /* Size of the user block in the file in bytes */
    uintn sym_leaf_k;		/* 1/2 rank for symbol table leaf nodes */
    uintn btree_k[8];		/* 1/2 rank for btree internal nodes */
    uint8 offset_size;          /* Number of bytes for offsets */
    uint8 length_size;          /* Number of bytes for lengths */
    uint8 bootblock_ver;        /* Version # of the bootblock */
    uint8 smallobject_ver;      /* Version # of the small-object heap */
    uint8 freespace_ver;        /* Version # of the free-space information */
    uint8 objectdir_ver;        /* Version # of the object directory format */
    uint8 sharedheader_ver;     /* Version # of the shared header format */
 } file_create_temp_t;

/* Parameters to use when retrieving file-creation template information */
typedef enum {
    H5_USERBLOCK_SIZE,          /* (uintn) Size of the user block in the file in bytes */
    H5_OFFSET_SIZE,             /* (uintn) Number of bytes for offsets */
    H5_LENGTH_SIZE,             /* (uintn) Number of bytes for lengths */
    H5_SYM_LEAF_K,		/* (uintn) 1/2 rank for symbol table leaf nodes */
    H5_SYM_INTERN_K,		/* (uintn) 1/2 rank for symbol table internal nodes */
    H5_BOOTBLOCK_VER,           /* (uint8) Version # of the boot-block format */
    H5_SMALLOBJECT_VER,         /* (uint8) Version # of the small-object heap format */
    H5_FREESPACE_VER,           /* (uint8) Version # of the free-space info format */
    H5_OBJECTDIR_VER,           /* (uint8) Version # of the object-directory format */
    H5_SHAREDHEADER_VER         /* (uint8) Version # of the shared-header format */
 } file_create_param_t;

/* HDF boolean type */
typedef enum {
    BFAIL=(-1),                  /* error value */
    BFALSE=0,
    BTRUE=1
 } hbool_t;

/* Unsigned integer error value (don't really know where else to put this - QAK) */
#define UFAIL   (unsigned)(-1)

#endif /* HDF5TYPE_H */

