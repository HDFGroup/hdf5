/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5. The full HDF5 copyright notice, including      *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * This file contains public declarations for the H5ES (event set) module.
 */

#ifndef _H5ESpublic_H
#define _H5ESpublic_H

/* Public headers needed by this file */
#include "H5public.h" /* Generic Functions                    */

/*****************/
/* Public Macros */
/*****************/

/* Default value for "no event set" / synchronous execution */
#define H5ES_NONE (hid_t)0

/* "Wait forever" timeout value */
#define H5ES_WAIT_FOREVER (UINT64_MAX)

/*******************/
/* Public Typedefs */
/*******************/

/* Asynchronous operation status */
typedef enum H5ES_status_t {
    H5ES_STATUS_IN_PROGRESS, /* Operation(s) have not yet completed                       */
    H5ES_STATUS_SUCCEED,     /* Operation(s) have completed, successfully                 */
    H5ES_STATUS_FAIL,        /* An operation has completed, but failed                   */
    H5ES_STATUS_CANCELED     /* An operation has not completed and was canceled          */
} H5ES_status_t;

/*
H5ES_err_info_t:
    const char *: API name (H5Dwrite_async, ...)
    const char *: Arg string
    const char *: Appl. source file name
    const char *: Appl. source function
    const char *: Appl. source file line
    uint64_t: Insert Time Timestamp
    uint64_t: "event count" - n'th event inserted into event set
    hid_t: Error stack (*)
    uint64_t: Execution Time timestamp (*)

More Possible Info for H5ES_err_info_t:
    Parent Operation's request token (*) -> "parent event count"? -- Could be
        used to "prune" child operations from reported errors, with flag
        to H5ESget_err_info?

Possible debugging routines:
    H5ESdebug_signal(hid_t es_id, signal_t sig, uint64_t <event count>);  (Env also)
    H5ESdebug_err_trace_func(hid_t es_id, int (*func)(H5ES_err_info_t *, void *ctx), void *ctx);
    H5ESdebug_err_trace_log(hid_t es_id, const char *filename);  (Env also)
    H5ESdebug_err_trace_fh(hid_t es_id, FILE *fh);               (Env also)
    H5ESdebug_err_signal(hid_t es_id, signal_t sig);             (Env also)
[Possibly option to allow operations to be inserted into event set with error?]

    Example usage:
        es_id = H5EScreate();
        H5ESdebug...(es_id, ...);
        ...
        H5Dwrite_async(..., es_id);

How to Trace Async Operations?
    <Example of stacking Logging VOL Connector w/Async VOL Connector>
*/

/********************/
/* Public Variables */
/********************/

/*********************/
/* Public Prototypes */
/*********************/

#ifdef __cplusplus
extern "C" {
#endif

hid_t  H5EScreate(void);
/* herr_t H5ESinsert(hid_t es_id, <request token?>); (For VOL connector authors only) */
herr_t H5EStest(hid_t es_id, H5ES_status_t *status);
herr_t H5ESwait(hid_t es_id, uint64_t timeout, H5ES_status_t *status);
herr_t H5EScancel(hid_t es_id, H5ES_status_t *status);
herr_t H5ESget_count(hid_t es_id, size_t *count);
herr_t H5ESget_err_status(hid_t es_id, hbool_t *err_occurred);
herr_t H5ESget_err_count(hid_t es_id, size_t *num_errs);
/* herr_t H5ESget_err_info(hid_t es_id, size_t num_err_info,
                            H5ES_err_info_t err_info[], size_t *err_cleared); */
herr_t H5ESclose(hid_t es_id);

#ifdef __cplusplus
}
#endif

#endif /* _H5ESpublic_H */
