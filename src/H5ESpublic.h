/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5. The full HDF5 copyright notice, including      *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * This file contains public declarations for the H5ES (event set) module.
 */

#ifndef H5ESpublic_H
#define H5ESpublic_H

/* Public headers needed by this file */
#include "H5public.h" /* Generic Functions                    */

/*****************/
/* Public Macros */
/*****************/

/* Default value for "no event set" / synchronous execution */
#define H5ES_NONE (hid_t)0

/* Special "wait" timeout values */
#define H5ES_WAIT_FOREVER (UINT64_MAX) /* Wait until all operations complete */
#define H5ES_WAIT_NONE                                                                                       \
    (0) /* Don't wait for operations to complete,                                                            \
         *  just check their status.                                                                         \
         *  (this allows H5ESwait to behave                                                                  \
         *   like a 'test' operation)                                                                        \
         */

/*******************/
/* Public Typedefs */
/*******************/

/* Asynchronous operation status */
typedef enum H5ES_status_t {
    H5ES_STATUS_IN_PROGRESS, /* Operation(s) have not yet completed         */
    H5ES_STATUS_SUCCEED,     /* Operation(s) have completed, successfully   */
    H5ES_STATUS_CANCELED,    /* Operation(s) has been canceled              */
    H5ES_STATUS_FAIL         /* An operation has completed, but failed      */
} H5ES_status_t;

/* Information about operations in an event set */
typedef struct H5ES_op_info_t {
    /* API call info */
    char *api_name; /* Name of HDF5 API routine called */
    char *api_args; /* "Argument string" for arguments to HDF5 API routine called */

    /* Application info */
    char *   app_file_name; /* Name of source file where the HDF5 API routine was called */
    char *   app_func_name; /* Name of function where the HDF5 API routine was called */
    unsigned app_line_num;  /* Line # of source file where the HDF5 API routine was called */

    /* Operation info */
    uint64_t op_ins_count; /* Counter of operation's insertion into event set */
    uint64_t op_ins_ts;    /* Timestamp for when the operation was inserted into the event set */
    uint64_t op_exec_ts;   /* Timestamp for when the operation began execution */
    uint64_t op_exec_time; /* Execution time for operation (in ns) */
} H5ES_op_info_t;

/* Information about failed operations in event set */
typedef struct H5ES_err_info_t {
    /* API call info */
    char *api_name; /* Name of HDF5 API routine called */
    char *api_args; /* "Argument string" for arguments to HDF5 API routine called */

    /* Application info */
    char *   app_file_name; /* Name of source file where the HDF5 API routine was called */
    char *   app_func_name; /* Name of function where the HDF5 API routine was called */
    unsigned app_line_num;  /* Line # of source file where the HDF5 API routine was called */

    /* Operation info */
    uint64_t op_ins_count; /* Counter of operation's insertion into event set */
    uint64_t op_ins_ts;    /* Timestamp for when the operation was inserted into the event set */
    uint64_t op_exec_ts;   /* Timestamp for when the operation began execution */
    uint64_t op_exec_time; /* Execution time for operation (in ns) */

    /* Error info */
    hid_t err_stack_id; /* ID for error stack from failed operation */
} H5ES_err_info_t;

/*
More Possible Info for H5ES_op_info_t:
    Parent Operation's request token (*) -> "parent event count"? -- Could be
        used to "prune" child operations from reported errors, with flag
        to H5ESget_err_info?

Possible debugging routines:  (Should also be configured from Env Var)
    H5ESdebug_signal(hid_t es_id, signal_t sig, uint64_t <event count>);
    H5ESdebug_err_trace_log(hid_t es_id, const char *filename);
    H5ESdebug_err_trace_fh(hid_t es_id, FILE *fh);
    H5ESdebug_err_signal(hid_t es_id, signal_t sig);
[Possibly option to allow operations to be inserted into event set with error?]

    Example usage:
        es_id = H5EScreate();
        H5ESdebug...(es_id, ...);
        ...
        H5Dwrite_async(..., es_id);

How to Trace Async Operations?
    <Example of stacking Logging VOL Connector w/Async VOL Connector>

*/

typedef int (*H5ES_event_insert_func_t)(const H5ES_op_info_t *op_info, void *ctx);
typedef int (*H5ES_event_complete_func_t)(const H5ES_op_info_t *op_info, H5ES_status_t status,
                                          hid_t err_stack, void *ctx);

/********************/
/* Public Variables */
/********************/

/*********************/
/* Public Prototypes */
/*********************/

#ifdef __cplusplus
extern "C" {
#endif

H5_DLL hid_t  H5EScreate(void);
H5_DLL herr_t H5ESwait(hid_t es_id, uint64_t timeout, size_t *num_in_progress, hbool_t *err_occurred);
H5_DLL herr_t H5EScancel(hid_t es_id, size_t *num_not_canceled, hbool_t *err_occurred);
H5_DLL herr_t H5ESget_count(hid_t es_id, size_t *count);
H5_DLL herr_t H5ESget_time_estimate(hid_t es_id, uint64_t *time_estimate);
H5_DLL herr_t H5ESget_op_counter(hid_t es_id, uint64_t *counter);
H5_DLL herr_t H5ESget_err_status(hid_t es_id, hbool_t *err_occurred);
H5_DLL herr_t H5ESget_err_count(hid_t es_id, size_t *num_errs);
H5_DLL herr_t H5ESget_err_info(hid_t es_id, size_t num_err_info, H5ES_err_info_t err_info[],
                               size_t *err_cleared);
H5_DLL herr_t H5ESfree_err_info(size_t num_err_info, H5ES_err_info_t err_info[]);
H5_DLL herr_t H5ESregister_insert_func(hid_t es_id, H5ES_event_insert_func_t func, void *ctx);
H5_DLL herr_t H5ESregister_complete_func(hid_t es_id, H5ES_event_complete_func_t func, void *ctx);
H5_DLL herr_t H5ESclose(hid_t es_id);

#ifdef __cplusplus
}
#endif

#endif /* H5ESpublic_H */
