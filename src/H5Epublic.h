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
 * This file contains public declarations for the H5E module.
 */

#ifndef _H5Epublic_H
#define _H5Epublic_H

/* Public headers needed by this file */
#include <H5public.h>

/* Declare an enumerated type which holds all the valid major HDF error codes */
typedef enum
  {
      H5E_NONE_MAJOR = 0,      /* special zero, no error */
      H5E_ARGS,                /* Invalid arguments to routine */
      H5E_RESOURCE,            /* Resource unavailable */
      H5E_INTERNAL,            /* Internal error (too specific to document in detail) */
      H5E_FILE,                /* File Accessability */
      H5E_IO,                  /* Low-level I/O */
      H5E_FUNC,                /* Function Entry/Exit */
      H5E_ATOM,                /* Object Atom */
      H5E_CACHE,	       /* Object Cache */
      H5E_BTREE,	       /* B-Tree Node */
      H5E_SYM,		       /* Symbol Table */
      H5E_HEAP,		       /* Heap */
      H5E_OHDR,		       /* Object Header */
      H5E_DIRECTORY,       /* Directory */
      H5E_DATATYPE,	       /* Datatype */
      H5E_DATASPACE,       /* Dataspace */
      H5E_DATASET          /* Dataset */
  }
hdf_maj_err_code_t;

/* Declare an enumerated type which holds all the valid minor HDF error codes */
typedef enum
  {
      H5E_NONE_MINOR = 0,      /* special zero, no error */

    /* Argument errors */
      H5E_UNINITIALIZED,       /* Information is unitialized */
      H5E_UNSUPPORTED,         /* Feature is unsupported */
      H5E_BADTYPE,             /* Incorrect type found */
      H5E_BADRANGE,            /* Argument out of range */
      H5E_BADVALUE,            /* Bad value for argument */

    /* Resource errors */
      H5E_NOSPACE,             /* No space available for allocation */

    /* File accessability errors */
      H5E_FILEEXISTS,          /* File already exists */
      H5E_FILEOPEN,            /* File already open */
      H5E_CANTCREATE,          /* Can't create file */
      H5E_CANTOPENFILE,        /* Can't open file */
      H5E_NOTHDF5,             /* Not an HDF5 format file */
      H5E_BADFILE,             /* Bad file ID accessed */

    /* Generic low-level file I/O errors */
      H5E_SEEKERROR,           /* Seek failed */
      H5E_READERROR,           /* Read failed */
      H5E_WRITEERROR,          /* Write failed */

    /* Function entry/exit interface errors */
      H5E_CANTINIT,            /* Can't initialize interface */
      H5E_ALREADYINIT,         /* Object already initialized */

    /* Object atom related errors */
      H5E_BADATOM,             /* Can't find atom information */
      H5E_CANTREGISTER,        /* Can't register new atom */

    /* Cache related errors */
      H5E_CANTFLUSH,	       /* Can't flush object from cache */
      H5E_CANTLOAD,	       /* Can't load object into cache */
      H5E_PROTECT,	       /* Protected object error */
      H5E_NOTCACHED,	       /* Object not currently cached */
      
    /* B-tree related errors */
      H5E_NOTFOUND,	       /* Object not found */
      H5E_EXISTS,	       /* Object already exists */
      H5E_CANTENCODE,	       /* Can't encode value */
      H5E_CANTDECODE,	       /* Can't decode value */
      H5E_CANTSPLIT,	       /* Can't split node */
      H5E_CANTINSERT,	       /* Can't insert object */
      H5E_CANTLIST,	       /* Can't list node */

    /* Object header related errors */
      H5E_LINKCOUNT,	       /* Bad object header link count */
      H5E_VERSION,	       /* Wrong version number */
      H5E_ALIGNMENT,	       /* Alignment error */
      H5E_BADMESG,	       /* Unrecognized message */

    /* Directory related errors */
      H5E_CANTOPENOBJ,	       /* Can't open object */
      H5E_COMPLEN,	       /* Name component is too long */
      H5E_LINK		       /* Link count failure */
  }
hdf_min_err_code_t;

/* Function pointer to report errors through */
typedef herr_t (*H5E_push_func_t)(int32 errid, hdf_maj_err_code_t maj, hdf_min_err_code_t min, const char *function_name, const char *file_name, intn line);

#ifdef __cplusplus
extern "C" {
#endif

/* Functions in H5E.c */
int32 H5Enew_err_stack(uintn initial_stack_size);
intn H5Edelete_err_stack(int32 err_hand);
#ifdef H5_ERROR_DEBUG
H5E_push_func_t H5Eset_push(H5E_push_func_t func);
#endif
herr_t H5Epush(hdf_maj_err_code_t maj, hdf_min_err_code_t min, const char *function_name, const char *file_name, intn line);
herr_t H5Eclear(int32 err_hand);
void H5E_term_interface(void);

#ifdef __cplusplus
}
#endif

#endif
