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

#define HERROR(maj, min, str) H5Epush (H5E_thrdid_g, maj, min,                \
                                       FUNC, __FILE__, __LINE__, str)

/* HRETURN_ERROR macro, used to facilitate error reporting.  Makes
   same assumptions as HERROR.  IN ADDITION, this macro causes
   a return from the calling routine */

#define HRETURN_ERROR(maj, min, ret_val, str) {                               \
   HERROR (maj, min, str);                                                    \
   PABLO_TRACE_OFF (PABLO_MASK, pablo_func_id);                               \
   return (ret_val);                                                          \
}

/* HRETURN macro, similar to HRETURN_ERROR() except for success */

#define HRETURN(ret_val) {                                                    \
   PABLO_TRACE_OFF (PABLO_MASK, pablo_func_id);                               \
   return (ret_val);                                                          \
}

/* HGOTO_ERROR macro, used to facilitate error reporting.  Makes
   same assumptions as HERROR.  IN ADDITION, this macro causes
   a jump to the label 'done' which should be in every fucntion
   Also there is an assumption of a variable 'ret_value' */

#define HGOTO_ERROR(maj, min, ret_val, str) {                                 \
   HERROR (maj, min,  str);                                                   \
   ret_value = ret_val;                                                       \
   goto done;                                                                 \
}

/* HGOTO_DONE macro, used to facilitate the new error reporting model.  
   This macro is just a wrapper to set the return value and jump to the 'done'
   label.  Also assumption of a variable 'ret_value' */

#define HGOTO_DONE(ret_val) {ret_value = ret_val; goto done;}

/* H5ECLEAR macro, used to facilitate the new error reporting model.  
   This macro is just a wrapper to clear the error stack with the thread
   error ID */

#define H5ECLEAR H5Eclear(H5E_thrdid_g)

/* Maximum length of function name to push onto error stack */
#define MAX_FUNC_NAME   32

/* 
 * error_messages is the list of error messages in the system, kept as
 * error_code-message pairs.  
 */
typedef struct H5E_major_mesg_t {
    H5E_major_t             error_code;
    const char             *str;
} H5E_major_mesg_t;

typedef struct H5E_minor_mesg_t {
    H5E_minor_t             error_code;
    const char             *str;
} H5E_minor_mesg_t;

/* Function pointer to report errors through */
struct H5E_t;                   /*forward decl */
typedef herr_t          (*H5E_push_t) (struct H5E_t *estack, H5E_major_t maj_num,
                             H5E_minor_t min_num, const char *function_name,
                                       const char *file_name, intn line,
                                       const char *desc);

/*
 * We use a stack to hold the errors plus we keep track of the function, file
 * and line where the error occurs.
 */

/* the structure of the error stack element */
typedef struct H5E_error_t {
    H5E_major_t             maj_num;    /* Major error number                   */
    H5E_minor_t             min_num;    /* Minor error number                   */
    char                    func_name[MAX_FUNC_NAME];   /* function where error occur */
    const char             *file_name;  /* file where error occur               */
    intn                    line;       /* line in file where error occurs      */
    char                   *desc;       /* optional supplied description        */
} H5E_error_t;

/* Structure to store error information for a thread */
typedef struct H5E_t {
    uintn                   nelmts;     /* Num elements allocated in the stack  */
    uintn                   top;        /* Index of the next open stack element */
    H5E_error_t            *stack;      /* Pointer to the error stack           */
    H5E_push_t              push;       /* Func that pushes new error on stack  */
} H5E_t;

/* Private global variables in H5E.c */
extern hid_t            H5E_thrdid_g;   /* Thread-specific "global" error-handler ID */
extern hbool_t          install_atexit;         /* Whether to install the atexit routine */

herr_t                  H5E_close(H5E_t *estack);
herr_t                  H5E_clear(H5E_t *estack);
herr_t                  H5E_print(H5E_t *estack, FILE * file);
herr_t                  H5E_push(H5E_t *estack, H5E_major_t maj_num, H5E_minor_t min_num,
                const char *function_name, const char *file_name, intn line,
                                 const char *desc);

#endif
