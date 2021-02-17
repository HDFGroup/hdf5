/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*-------------------------------------------------------------------------
 *
 * Created:     H5ESint.c
 *              Apr  8 2020
 *              Quincey Koziol
 *
 * Purpose:     Internal "event set" routines for managing asynchronous
 *                      operations.
 *
 *                      Please see the asynchronous I/O RFC document
 *                      for a full description of how they work, etc.
 *
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/

#include "H5ESmodule.h" /* This source code file is part of the H5ES module */

/***********/
/* Headers */
/***********/
#include "H5private.h"   /* Generic Functions			 */
#include "H5Eprivate.h"  /* Error handling		  	 */
#include "H5ESpkg.h"     /* Event Sets                           */
#include "H5FLprivate.h" /* Free Lists                           */
#include "H5Iprivate.h"  /* IDs                                  */
#include "H5MMprivate.h" /* Memory management                    */
#include "H5RSprivate.h" /* Reference-counted strings            */

/****************/
/* Local Macros */
/****************/

/******************/
/* Local Typedefs */
/******************/

/* Callback context for wait operations */
typedef struct H5ES_wait_ctx_t {
    H5ES_t * es;              /* Event set being operated on */
    uint64_t timeout;         /* Timeout for wait operation */
    size_t * num_in_progress; /* Count of # of operations that have not completed */
    hbool_t *op_failed;       /* Flag to indicate an operation failed */
} H5ES_wait_ctx_t;

/* Callback context for get error info (gei) operations */
typedef struct H5ES_gei_ctx_t {
    H5ES_t *         es;            /* Event set being operated on */
    size_t           num_err_info;  /* # of elements in err_info[] array */
    size_t           curr_err;      /* Index of current error in array */
    H5ES_err_info_t *curr_err_info; /* Pointer to current element in err_info[] array */
} H5ES_gei_ctx_t;

/********************/
/* Package Typedefs */
/********************/

/********************/
/* Local Prototypes */
/********************/
static herr_t H5ES__close_cb(void *es, void **request_token);
static herr_t H5ES__handle_fail(H5ES_t *es, H5ES_event_t *ev);
static int    H5ES__wait_cb(H5ES_event_t *ev, void *_ctx);
static int    H5ES__get_err_info_cb(H5ES_event_t *ev, void *_ctx);
static int    H5ES__close_failed_cb(H5ES_event_t *ev, void *_ctx);

/*********************/
/* Package Variables */
/*********************/

/* Package initialization variable */
hbool_t H5_PKG_INIT_VAR = FALSE;

/*****************************/
/* Library Private Variables */
/*****************************/

/*******************/
/* Local Variables */
/*******************/

/* Event Set ID class */
static const H5I_class_t H5I_EVENTSET_CLS[1] = {{
    H5I_EVENTSET,              /* ID class value */
    0,                         /* Class flags */
    0,                         /* # of reserved IDs for class */
    (H5I_free_t)H5ES__close_cb /* Callback routine for closing objects of this class */
}};

/* Declare a static free list to manage H5ES_t structs */
H5FL_DEFINE_STATIC(H5ES_t);

/*-------------------------------------------------------------------------
 * Function:    H5ES__init_package
 *
 * Purpose:     Initializes any interface-specific data or routines.
 *
 * Return:      Non-negative on success / Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              Monday, April 6, 2020
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5ES__init_package(void)
{
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_PACKAGE

    /* Initialize the ID group for the event set IDs */
    if (H5I_register_type(H5I_EVENTSET_CLS) < 0)
        HGOTO_ERROR(H5E_EVENTSET, H5E_CANTINIT, FAIL, "unable to initialize interface")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5ES__init_package() */

/*-------------------------------------------------------------------------
 * Function:    H5ES_term_package
 *
 * Purpose:     Terminate this interface.
 *
 * Return:      Success:    Positive if anything is done that might
 *                          affect other interfaces; zero otherwise.
 *              Failure:    Negative
 *
 * Programmer:  Quincey Koziol
 *              Monday, April 6, 2020
 *
 *-------------------------------------------------------------------------
 */
int
H5ES_term_package(void)
{
    int n = 0;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    if (H5_PKG_INIT_VAR) {
        /* Destroy the event set ID group */
        n += (H5I_dec_type_ref(H5I_EVENTSET) > 0);

        /* Mark closed */
        if (0 == n)
            H5_PKG_INIT_VAR = FALSE;
    } /* end if */

    FUNC_LEAVE_NOAPI(n)
} /* end H5ES_term_package() */

/*-------------------------------------------------------------------------
 * Function:    H5ES__close_cb
 *
 * Purpose:     Called when the ref count reaches zero on an event set's ID
 *
 * Return:      SUCCEED / FAIL
 *
 * Programmer:  Quincey Koziol
 *              Monday, April 6, 2020
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5ES__close_cb(void *_es, void H5_ATTR_UNUSED **rt)
{
    H5ES_t *es        = (H5ES_t *)_es; /* The event set to close */
    herr_t  ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_STATIC

    /* Sanity check */
    HDassert(es);

    /* Close the event set object */
    if (H5ES__close(es) < 0)
        HGOTO_ERROR(H5E_EVENTSET, H5E_CLOSEERROR, FAIL, "unable to close event set");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5ES__close_cb() */

/*-------------------------------------------------------------------------
 * Function:    H5ES__create
 *
 * Purpose:     Private function to create an event set object
 *
 * Return:      Success:    Pointer to an event set struct
 *              Failure:    NULL
 *
 * Programmer:  Quincey Koziol
 *              Wednesday, April 8, 2020
 *
 *-------------------------------------------------------------------------
 */
H5ES_t *
H5ES__create(void)
{
    H5ES_t *es        = NULL; /* Pointer to event set */
    H5ES_t *ret_value = NULL; /* Return value */

    FUNC_ENTER_PACKAGE

    /* Allocate space for new event set */
    if (NULL == (es = H5FL_CALLOC(H5ES_t)))
        HGOTO_ERROR(H5E_EVENTSET, H5E_CANTALLOC, NULL, "can't allocate event set object")

    /* Set the return value */
    ret_value = es;

done:
    if (!ret_value)
        if (es && H5ES__close(es) < 0)
            HDONE_ERROR(H5E_EVENTSET, H5E_CANTRELEASE, NULL, "unable to free event set")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5ES__create() */

/*-------------------------------------------------------------------------
 * Function:    H5ES_insert
 *
 * Purpose:     Insert a request token into an event set
 *
 * Return:      SUCCEED / FAIL
 *
 * Programmer:  Quincey Koziol
 *              Wednesday, April 8, 2020
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5ES_insert(hid_t es_id, H5VL_t *connector, void *token, const char *caller, const char *caller_args, ...)
{
    H5ES_t *      es = NULL;             /* Event set for the operation */
    H5ES_event_t *ev = NULL;             /* Event for request */
    H5RS_str_t *  rs = NULL;             /* Ref-counted string to compose formatted argument string in */
    const char *  app_file;              /* Application source file name */
    const char *  app_func;              /* Application source function name */
    const char *  s;                     /* Pointer to internal string from ref-counted string */
    va_list       ap;                    /* Varargs for caller */
    hbool_t       arg_started = FALSE;   /* Whether the va_list has been started */
    herr_t        ret_value   = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity check */
    HDassert(connector);
    HDassert(token);
    HDassert(caller);
    HDassert(caller_args);

    /* Get event set */
    if (NULL == (es = (H5ES_t *)H5I_object_verify(es_id, H5I_EVENTSET)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not an event set")

    /* Check for errors in event set */
    if (es->err_occurred)
        HGOTO_ERROR(H5E_EVENTSET, H5E_CANTINSERT, FAIL, "event set has failed operations")

    /* Create new event */
    if (NULL == (ev = H5ES__event_new(connector, token)))
        HGOTO_ERROR(H5E_EVENTSET, H5E_CANTCREATE, FAIL, "can't create event object")

    /* Start working on the API routines arguments */
    HDva_start(ap, caller_args);
    arg_started = TRUE;

    /* Copy the app source information */
    (void)HDva_arg(ap, char *); /* Toss the 'app_file' parameter name */
    app_file = HDva_arg(ap, char *);
    if (NULL == (ev->app_file = H5MM_strdup(app_file)))
        HGOTO_ERROR(H5E_EVENTSET, H5E_CANTALLOC, FAIL, "can't copy app source file name")
    (void)HDva_arg(ap, char *); /* Toss the 'app_func' parameter name */
    app_func = HDva_arg(ap, char *);
    if (NULL == (ev->app_func = H5MM_strdup(app_func)))
        HGOTO_ERROR(H5E_EVENTSET, H5E_CANTALLOC, FAIL, "can't copy app source function name")
    (void)HDva_arg(ap, char *); /* Toss the 'app_line' parameter name */
    ev->app_line = HDva_arg(ap, unsigned);

    /* Set the event's operation counter */
    ev->ev_count = es->op_counter++;

    /* Set the event's timestamp */
    ev->ev_time = H5_now_usec();

    /* Copy the API routine's name */
    if (NULL == (ev->api_name = H5MM_strdup(caller)))
        HGOTO_ERROR(H5E_EVENTSET, H5E_CANTALLOC, FAIL, "can't copy API routine name")

    /* Create the string for the API routine's arguments */
    if (NULL == (rs = H5RS_create(NULL)))
        HGOTO_ERROR(H5E_EVENTSET, H5E_CANTALLOC, FAIL, "can't allocate ref-counted string")

    /* Copy the string for the API routine's arguments */
    /* (skip the six characters from the app's file, function and line # arguments) */
    HDassert(0 == HDstrncmp(caller_args, "*s*sIu", 6));
    if (H5_trace_args(rs, caller_args + 6, ap) < 0)
        HGOTO_ERROR(H5E_EVENTSET, H5E_CANTSET, FAIL, "can't create formatted API arguments")
    if (NULL == (s = H5RS_get_str(rs)))
        HGOTO_ERROR(H5E_EVENTSET, H5E_CANTGET, FAIL, "can't get pointer to formatted API arguments")
    if (NULL == (ev->api_args = H5MM_strdup(s)))
        HGOTO_ERROR(H5E_EVENTSET, H5E_CANTALLOC, FAIL, "can't copy API routine arguments")

    /* Append fully initialized event onto the event set's 'active' list */
    H5ES__list_append(&es->active, ev);

done:
    /* Clean up */
    if (arg_started)
        HDva_end(ap);
    if (rs)
        H5RS_decr(rs);

    /* Release resources on error */
    if (ret_value < 0)
        if (ev && H5ES__event_free(ev) < 0)
            HDONE_ERROR(H5E_EVENTSET, H5E_CANTRELEASE, FAIL, "unable to release event")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5ES_insert() */

/*-------------------------------------------------------------------------
 * Function:    H5ES__handle_fail
 *
 * Purpose:     Handle a failed event
 *
 * Return:      SUCCEED / FAIL
 *
 * Programmer:  Quincey Koziol
 *              Thursday, October 15, 2020
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5ES__handle_fail(H5ES_t *es, H5ES_event_t *ev)
{
    FUNC_ENTER_STATIC_NOERR

    /* Sanity check */
    HDassert(es);
    HDassert(es->active.head);
    HDassert(ev);

    /* Set error flag for event set */
    es->err_occurred = TRUE;

    /* Remove event from normal list */
    H5ES__list_remove(&es->active, ev);

    /* Append event onto the event set's error list */
    H5ES__list_append(&es->failed, ev);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5ES__handle_fail() */

/*-------------------------------------------------------------------------
 * Function:    H5ES__wait_cb
 *
 * Purpose:     Common routine for testing / waiting on an operation
 *
 * Return:      SUCCEED / FAIL
 *
 * Programmer:  Quincey Koziol
 *              Sunday, November 7, 2020
 *
 *-------------------------------------------------------------------------
 */
static int
H5ES__wait_cb(H5ES_event_t *ev, void *_ctx)
{
    H5ES_wait_ctx_t *     ctx       = (H5ES_wait_ctx_t *)_ctx;     /* Callback context */
    H5VL_request_status_t ev_status = H5VL_REQUEST_STATUS_SUCCEED; /* Status from event's operation */
    uint64_t start_time = 0, elapsed_time = 0; /* Start and elapsed times for waiting on an operation */
    int      ret_value = H5_ITER_CONT;         /* Return value */

    FUNC_ENTER_STATIC

    /* Sanity check */
    HDassert(ev);
    HDassert(ctx);

    /* Wait on the request */
    if (ctx->timeout != H5ES_WAIT_NONE && ctx->timeout != H5ES_WAIT_FOREVER)
        start_time = H5_now_usec();
    if (H5VL_request_wait(ev->request, ctx->timeout, &ev_status) < 0)
        HGOTO_ERROR(H5E_EVENTSET, H5E_CANTWAIT, H5_ITER_ERROR, "unable to test operation")
    if (ctx->timeout != H5ES_WAIT_NONE && ctx->timeout != H5ES_WAIT_FOREVER)
        elapsed_time = H5_now_usec() - start_time;

    /* Check for status values that indicate we should break out of the loop */
    if (ev_status == H5VL_REQUEST_STATUS_FAIL) {
        /* Handle failure */
        if (H5ES__handle_fail(ctx->es, ev) < 0)
            HGOTO_ERROR(H5E_EVENTSET, H5E_CANTSET, H5_ITER_ERROR, "unable to handle failed event")

        /* Record the error */
        *ctx->op_failed = TRUE;

        /* Exit from the iteration */
        ret_value = H5_ITER_STOP;
    } /* end if */
    else if (ev_status == H5VL_REQUEST_STATUS_SUCCEED) {
        if (H5ES__event_completed(ev, &ctx->es->active) < 0)
            HGOTO_ERROR(H5E_EVENTSET, H5E_CANTRELEASE, H5_ITER_ERROR, "unable to release completed event")
    } /* end else-if */
    else if (ev_status == H5VL_REQUEST_STATUS_CANCELED)
        /* Should never get a status of 'cancel' back from test / wait operation */
        HGOTO_ERROR(H5E_EVENTSET, H5E_BADVALUE, H5_ITER_ERROR, "received 'cancel' status for operation")
    else {
        /* Sanity check */
        HDassert(ev_status == H5VL_REQUEST_STATUS_IN_PROGRESS);

        /* Increment "in progress operation" counter */
        (*ctx->num_in_progress)++;
    } /* end if */

    /* Check for updateable timeout */
    if (ctx->timeout != H5ES_WAIT_NONE && ctx->timeout != H5ES_WAIT_FOREVER) {
        /* Update timeout for next operation */
        if ((elapsed_time * 1000) > ctx->timeout)
            ctx->timeout = H5ES_WAIT_NONE;
        else
            ctx->timeout -= (elapsed_time * 1000); /* Convert us to ns */
    }                                              /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5ES__wait_cb() */

/*-------------------------------------------------------------------------
 * Function:    H5ES__wait
 *
 * Purpose:     Wait for operations in event set to complete
 *
 * Note:        Timeout value is in ns, and is for H5ES__wait itself.
 *
 * Return:      SUCCEED / FAIL
 *
 * Programmer:  Quincey Koziol
 *              Monday, July 13, 2020
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5ES__wait(H5ES_t *es, uint64_t timeout, size_t *num_in_progress, hbool_t *op_failed)
{
    H5ES_wait_ctx_t ctx;                 /* Iterator callback context info */
    herr_t          ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_PACKAGE

    /* Sanity check */
    HDassert(es);
    HDassert(num_in_progress);
    HDassert(op_failed);

    /* Set user's parameters to known values */
    *num_in_progress = 0;
    *op_failed       = FALSE;

    /* Set up context for iterator callbacks */
    ctx.es              = es;
    ctx.timeout         = timeout;
    ctx.num_in_progress = num_in_progress;
    ctx.op_failed       = op_failed;

    /* Iterate over the events in the set, waiting for them to complete */
    if (H5ES__list_iterate(&es->active, H5ES__wait_cb, &ctx) < 0)
        HGOTO_ERROR(H5E_EVENTSET, H5E_BADITER, FAIL, "iteration failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5ES__wait() */

/*-------------------------------------------------------------------------
 * Function:    H5ES__get_err_info_cb
 *
 * Purpose:     Retrieve information about a failed operation
 *
 * Return:      SUCCEED / FAIL
 *
 * Programmer:  Quincey Koziol
 *              Monday, November 11, 2020
 *
 *-------------------------------------------------------------------------
 */
static int
H5ES__get_err_info_cb(H5ES_event_t *ev, void *_ctx)
{
    H5ES_gei_ctx_t *ctx       = (H5ES_gei_ctx_t *)_ctx; /* Callback context */
    int             ret_value = H5_ITER_CONT;           /* Return value */

    FUNC_ENTER_STATIC

    /* Sanity check */
    HDassert(ev);
    HDassert(ctx);

    /* Copy operation info for event */
    if (NULL == (ctx->curr_err_info->api_name = H5MM_strdup(ev->api_name)))
        HGOTO_ERROR(H5E_EVENTSET, H5E_CANTALLOC, H5_ITER_ERROR, "can't copy HDF5 API name")
    if (NULL == (ctx->curr_err_info->api_args = H5MM_strdup(ev->api_args)))
        HGOTO_ERROR(H5E_EVENTSET, H5E_CANTALLOC, H5_ITER_ERROR, "can't copy HDF5 API routine arguments")
    if (NULL == (ctx->curr_err_info->app_file_name = H5MM_strdup(ev->app_file)))
        HGOTO_ERROR(H5E_EVENTSET, H5E_CANTALLOC, H5_ITER_ERROR, "can't copy app source file name")
    if (NULL == (ctx->curr_err_info->app_func_name = H5MM_strdup(ev->app_func)))
        HGOTO_ERROR(H5E_EVENTSET, H5E_CANTALLOC, H5_ITER_ERROR, "can't copy app function name")
    ctx->curr_err_info->app_line_num = ev->app_line;
    ctx->curr_err_info->op_ins_count = ev->ev_count;
    ctx->curr_err_info->op_ins_ts    = ev->ev_time;

    /* Get error stack for event */
    if (H5VL_request_specific(ev->request, H5VL_REQUEST_GET_ERR_STACK, &ctx->curr_err_info->err_stack_id) < 0)
        HGOTO_ERROR(H5E_EVENTSET, H5E_CANTGET, H5_ITER_ERROR, "unable to retrieve error stack for operation")

    /* Remove event from event set's failed list */
    H5ES__list_remove(&ctx->es->failed, ev);

    /* Free event node */
    if (H5ES__event_free(ev) < 0)
        HGOTO_ERROR(H5E_EVENTSET, H5E_CANTRELEASE, H5_ITER_ERROR, "unable to release failed event")

    /* Advance to next element of err_info[] array */
    ctx->curr_err++;
    ctx->curr_err_info++;

    /* Stop iteration if err_info[] array is full */
    if (ctx->curr_err == ctx->num_err_info)
        ret_value = H5_ITER_STOP;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5ES__get_err_info_cb() */

/*-------------------------------------------------------------------------
 * Function:    H5ES__get_err_info
 *
 * Purpose:     Retrieve information about failed operations
 *
 * Return:      SUCCEED / FAIL
 *
 * Programmer:  Quincey Koziol
 *              Friday, November 6, 2020
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5ES__get_err_info(H5ES_t *es, size_t num_err_info, H5ES_err_info_t err_info[], size_t *num_cleared)
{
    H5ES_gei_ctx_t ctx;                 /* Iterator callback context info */
    herr_t         ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_PACKAGE

    /* Sanity check */
    HDassert(es);
    HDassert(num_err_info);
    HDassert(err_info);
    HDassert(num_cleared);

    /* Set up context for iterator callbacks */
    ctx.es            = es;
    ctx.num_err_info  = num_err_info;
    ctx.curr_err      = 0;
    ctx.curr_err_info = &err_info[0];

    /* Iterate over the failed events in the set, copying their error info */
    if (H5ES__list_iterate(&es->failed, H5ES__get_err_info_cb, &ctx) < 0)
        HGOTO_ERROR(H5E_EVENTSET, H5E_BADITER, FAIL, "iteration failed")

    /* Set # of failed events cleared from event set's failed list */
    *num_cleared = ctx.curr_err;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5ES__get_err_info() */

/*-------------------------------------------------------------------------
 * Function:    H5ES__close_failed_cb
 *
 * Purpose:     Release a failed event
 *
 * Return:      SUCCEED / FAIL
 *
 * Programmer:  Quincey Koziol
 *              Monday, November 11, 2020
 *
 *-------------------------------------------------------------------------
 */
static int
H5ES__close_failed_cb(H5ES_event_t *ev, void *_ctx)
{
    H5ES_t *es        = (H5ES_t *)_ctx; /* Callback context */
    int     ret_value = H5_ITER_CONT;   /* Return value */

    FUNC_ENTER_PACKAGE

    /* Sanity check */
    HDassert(ev);
    HDassert(es);

    /* Remove event from event set's failed list */
    H5ES__list_remove(&es->failed, ev);

    /* Free event node */
    if (H5ES__event_free(ev) < 0)
        HGOTO_ERROR(H5E_EVENTSET, H5E_CANTRELEASE, H5_ITER_ERROR, "unable to release failed event")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5ES__close_failed_cb() */

/*-------------------------------------------------------------------------
 * Function:    H5ES__close
 *
 * Purpose:     Destroy an event set object
 *
 * Return:      SUCCEED / FAIL
 *
 * Programmer:  Quincey Koziol
 *              Monday, April 6, 2020
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5ES__close(H5ES_t *es)
{
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_PACKAGE

    /* Sanity check */
    HDassert(es);

    /* Fail if active operations still present */
    if (H5ES__list_count(&es->active) > 0)
        HGOTO_ERROR(
            H5E_EVENTSET, H5E_CANTCLOSEOBJ, FAIL,
            "can't close event set while unfinished operations are present (i.e. wait on event set first)")

    /* Iterate over the failed events in the set, releasing them */
    if (H5ES__list_iterate(&es->failed, H5ES__close_failed_cb, (void *)es) < 0)
        HGOTO_ERROR(H5E_EVENTSET, H5E_BADITER, FAIL, "iteration failed")

    /* Release the event set */
    es = H5FL_FREE(H5ES_t, es);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5ES__close() */
