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
 *  Header file for error values, etc.
 */

#ifndef HDF5ERR_H
#define HDF5ERR_H

#include "H5Eproto.h"

/*
   ======================================================================
   Error codes

   NOTE: Remember to update the error_messages[] structure later in this file
   whenever errors are added/deleted from this list.
   ======================================================================
 */
/* 
 * This section of code is designed to be only accessible to the actual
 * error-management code.
 */
#ifdef HDF5_ERR_MASTER

/* 
 * error_messages is the list of error messages in the system, kept as
 * error_code-message pairs.  
 */
 
typedef struct 
  {
      hdf_maj_err_code_t error_code;
      const char *str;
  }
hdf_maj_error_messages_t;

static const hdf_maj_error_messages_t hdf_maj_error_messages[] =
{
    {H5E_NONE_MAJOR,    "No error"},
    {H5E_ARGS,          "Invalid arguments to routine"},
    {H5E_RESOURCE,      "Resource unavailable"},
    {H5E_INTERNAL,      "Internal HDF5 error (too specific to document in detail)"},
    {H5E_FILE,          "File Accessability"},
    {H5E_IO,            "Low-level I/O"},
    {H5E_FUNC,          "Function Entry/Exit"},
    {H5E_ATOM,          "Object Atom"},
    {H5E_CACHE,		"Object Cache"},
    {H5E_BTREE,		"B-Tree Node"},
    {H5E_SYM,		"Symbol Table"},
    {H5E_HEAP,		"Heap"},
    {H5E_OHDR,		"Object Header"},
    {H5E_DIRECTORY,	"Directory"},
};

typedef struct 
  {
      hdf_min_err_code_t error_code;
      const char *str;
  }
hdf_min_error_messages_t;

static const hdf_min_error_messages_t hdf_min_error_messages[] =
{
    {H5E_NONE_MINOR,    "No error"},
    {H5E_UNINITIALIZED, "Information is uninitialized"},
    {H5E_UNSUPPORTED,   "Feature is unsupported"},
    {H5E_BADTYPE,       "Incorrect type found"},
    {H5E_BADRANGE,      "Argument out of range"},
    {H5E_BADVALUE,      "Bad value for argument"},
    {H5E_NOSPACE,       "No space available for allocation"},
    {H5E_FILEEXISTS,    "File already exists"},
    {H5E_FILEOPEN,      "File already open"},
    {H5E_CANTCREATE,    "Can't create file"},
    {H5E_CANTOPEN,      "Can't open file"},
    {H5E_NOTHDF5,       "Not an HDF5 format file"},
    {H5E_BADFILE,       "Bad file ID accessed"},
    {H5E_SEEKERROR,     "Seek failed"},
    {H5E_READERROR,     "Read failed"},
    {H5E_WRITEERROR,    "Write failed"},
    {H5E_CANTINIT,      "Can't initialize interface"},
    {H5E_ALREADYINIT,   "Object already initialized"},
    {H5E_BADATOM,       "Can't find atom information"},
    {H5E_CANTREGISTER,  "Can't register new atom"},
    {H5E_CANTFLUSH,	"Can't flush object from cache"},
    {H5E_CANTLOAD,	"Can't load object into cache"},
    {H5E_NOTFOUND,	"Object not found"},
    {H5E_EXISTS,	"Object already exists"},
    {H5E_CANTENCODE,	"Can't encode value"},
    {H5E_CANTDECODE,	"Can't decode value"},
    {H5E_CANTSPLIT,	"Can't split node"},
    {H5E_CANTINSERT,	"Can't insert object"},
    {H5E_CANTLIST,	"Can't list node"},
    {H5E_LINKCOUNT,	"Bad object header link count"},
    {H5E_VERSION,	"Wrong version number"},
    {H5E_ALIGNMENT,	"Alignment error"},
    {H5E_BADMESG,	"Unrecognized message"},
    {H5E_COMPLEN,	"Name component is too long"},
};

/* We use a stack to hold the errors plus we keep track of the function,
   file and line where the error occurs. */

/* the structure of the error stack element */
typedef struct error_t
  {
      hdf_maj_err_code_t maj;    /* Major error number */
      hdf_min_err_code_t min;    /* Minor error number */
      char function_name[MAX_FUNC_NAME_LEN];    /* function where error occur */
      const char *file_name;    /* file where error occur */
      intn        line;         /* line in file where error occurs */
      char       *desc;         /* optional supplied description */
  }
H5E_error_t;

/* Structure to store error information for a thread */
typedef struct errstack_t
  {
      uintn stack_size;         /* Number of elements allocated in the stack */
      uintn stack_top;          /* Offset of the next open stack element */
      H5E_error_t *err_stack;   /* Pointer to the error stack */
      H5E_push_func_t push;     /* Function to call when an error is to be reported */
  } H5E_errstack_t;

#endif /* HDF5_ERR_MASTER */

#if defined c_plusplus || defined __cplusplus
extern      "C"
{
#endif                          /* c_plusplus || __cplusplus */

/* Private functions in H5E.c */
herr_t H5E_store(int32 errid, hdf_maj_err_code_t maj, hdf_min_err_code_t min, const char *function_name, const char *file_name, intn line);

#if defined c_plusplus || defined __cplusplus
}
#endif                          /* c_plusplus || __cplusplus */

#endif /* HDF5ERR_H */

