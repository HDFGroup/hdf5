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
 * This file contains function prototypes for each exported function in the H5E module
 */

#ifndef H5EPROTO_H
#define H5EPROTO_H

/* HERROR macro, used to facilitate error reporting.  Assumes that
   there's a variable called FUNC which holds the function name.
   Assume that func and file are both stored in static space, or at
   least be not corrupted in the meanwhile. */

#define HERROR(maj, min) H5Epush(maj, min, FUNC, __FILE__, __LINE__)

/* HRETURN_ERROR macro, used to facilitate error reporting.  Makes
   same assumptions as HERROR.  IN ADDITION, this macro causes
   a return from the calling routine */

#define HRETURN_ERROR(maj, min, ret_val) {HERROR(maj, min); return(ret_val);}

/* HGOTO_ERROR macro, used to facilitate error reporting.  Makes
   same assumptions as HERROR.  IN ADDITION, this macro causes
   a jump to the label 'done' which should be in every fucntion
   Also there is an assumption of a variable 'ret_value' */

#define HGOTO_ERROR(maj, min, ret_val) { HERROR(maj, min); \
        ret_value = ret_val; goto done; }

/* HGOTO_DONE macro, used to facilitate the new error reporting model.  
   This macro is just a wrapper to set the return value and jump to the 'done'
   label.  Also assumption of a variable 'ret_value' */

#define HGOTO_DONE(ret_val) {ret_value = ret_val; goto done;}

/* H5ECLEAR macro, used to facilitate the new error reporting model.  
   This macro is just a wrapper to clear the error stack with the thread
   error ID */

#define H5ECLEAR H5Eclear(thrderrid)

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
      H5E_ATOM                 /* Object Atom */
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
      H5E_BADRANGE,            /* Incorrect range of argument */

    /* Resource errors */
      H5E_NOSPACE,             /* No space available for allocation */

    /* File accessability errors */
      H5E_FILEEXISTS,          /* File already exists */
      H5E_FILEOPEN,            /* File already open */
      H5E_CANTCREATE,          /* Can't create file */
      H5E_CANTOPEN,            /* Can't open file */
      H5E_NOTHDF5,             /* Not an HDF5 format file */
      H5E_BADFILE,             /* Bad file ID accessed */

    /* Generic low-level file I/O errors */
      H5E_SEEKERROR,           /* File seek failed */
      H5E_READERROR,           /* File read failed */
      H5E_WRITEERROR,          /* File write failed */

    /* Function entry/exit interface errors */
      H5E_CANTINIT,            /* Can't initialize interface */
      H5E_ALREADYINIT,         /* Object already initialized */

    /* Object atom related errors */
      H5E_BADATOM,             /* Can't find atom information */
      H5E_CANTREGISTER         /* Can't register new atom */
  }
hdf_min_err_code_t;

/* Function pointer to report errors through */
typedef void (*H5E_push_func_t)(int32 errid, hdf_maj_err_code_t maj, hdf_min_err_code_t min, const char *function_name, const char *file_name, intn line);

#if defined c_plusplus || defined __cplusplus
extern      "C"
{
#endif                          /* c_plusplus || __cplusplus */

/* Functions in H5E.c */
int32 H5Enew_err_stack(uintn initial_stack_size);
intn H5Edelete_err_stack(int32 err_hand);
H5E_push_func_t H5Eset_push(H5E_push_func_t func);
void H5Epush(hdf_maj_err_code_t maj, hdf_min_err_code_t min, const char *function_name, const char *file_name, intn line);
void H5Eclear(int32 err_hand);

#if defined c_plusplus || defined __cplusplus
}
#endif                          /* c_plusplus || __cplusplus */

#endif /* H5EPROTO_H */

