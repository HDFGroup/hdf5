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
#ifndef _H5Eprivate_H
#define _H5Eprivate_H
#include <H5Epublic.h>

/* Private headers needed by this file */
#include <H5private.h>

/*
   ======================================================================
   Error codes

   NOTE: Remember to update the error_messages[] structure later in this file
   whenever errors are added/deleted from this list.
   ======================================================================
 */

/* HERROR macro, used to facilitate error reporting.  Assumes that
   there's a variable called FUNC which holds the function name.
   Assume that func and file are both stored in static space, or at
   least be not corrupted in the meanwhile. */

#define HERROR(maj, min) H5Epush(maj, min, FUNC, __FILE__, __LINE__)

/* HRETURN_ERROR macro, used to facilitate error reporting.  Makes
   same assumptions as HERROR.  IN ADDITION, this macro causes
   a return from the calling routine */

#define HRETURN_ERROR(maj, min, ret_val) {				      \
   HERROR (maj, min);							      \
   PABLO_TRACE_OFF (PABLO_MASK, pablo_func_id);				      \
   return (ret_val);							      \
}

/* HRETURN macro, similar to HRETURN_ERROR() except for success */

#define HRETURN(ret_val) {						      \
   PABLO_TRACE_OFF (PABLO_MASK, pablo_func_id);				      \
   return (ret_val);							      \
}

/* HGOTO_ERROR macro, used to facilitate error reporting.  Makes
   same assumptions as HERROR.  IN ADDITION, this macro causes
   a jump to the label 'done' which should be in every fucntion
   Also there is an assumption of a variable 'ret_value' */

#define HGOTO_ERROR(maj, min, ret_val) {				      \
   HERROR (maj, min);							      \
   ret_value = ret_val;							      \
   goto done;								      \
}

/* HGOTO_DONE macro, used to facilitate the new error reporting model.  
   This macro is just a wrapper to set the return value and jump to the 'done'
   label.  Also assumption of a variable 'ret_value' */

#define HGOTO_DONE(ret_val) {ret_value = ret_val; goto done;}

/* H5ECLEAR macro, used to facilitate the new error reporting model.  
   This macro is just a wrapper to clear the error stack with the thread
   error ID */

#define H5ECLEAR H5Eclear(thrderrid)

/* Maximum length of function name to push onto error stack */
#define MAX_FUNC_NAME_LEN   32

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


typedef struct 
  {
      hdf_min_err_code_t error_code;
      const char *str;
  }
hdf_min_error_messages_t;


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



/* Private global variables in H5E.c */
extern int32 thrderrid;     	/* Thread-specific "global" error-handler ID */
extern hbool_t install_atexit;	/* Whether to install the atexit routine */

/* Private functions in H5E.c */
herr_t H5E_store(int32 errid, hdf_maj_err_code_t maj, hdf_min_err_code_t min, const char *function_name, const char *file_name, intn line);

#endif
