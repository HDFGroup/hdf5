/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Programmer:  Mohamad Chaarawi <chaarawi@hdfgroup.org>
 *              September, 2013
 *
 * Purpose:	Wrappers around existing HDF5 to support Exascale FastForward
 *              functionality.
 *              
 */


/****************/
/* Module Setup */
/****************/

#include "H5ESmodule.h"          /* This source code file is part of the H5ES module */

/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5ESprivate.h"        /* Event Stacks                         */
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5VLprivate.h"	/* VOL plugins				*/


/****************/
/* Local Macros */
/****************/


/******************/
/* Local Typedefs */
/******************/


/********************/
/* Local Prototypes */
/********************/


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

/* Declare a free list to manage the H5ES_t struct */
H5FL_DEFINE(H5ES_t);

/* Dataspace ID class */
static const H5I_class_t H5I_ES_CLS[1] = {{
    H5I_ES,        		/* ID class value */
    0,				/* Class flags */
    2,				/* # of reserved IDs for class */
    (H5I_free_t)H5ES_close   	/* Callback routine for closing objects of this class */
}};



/*-------------------------------------------------------------------------
 * Function:	H5ES_init
 *
 * Purpose:	Initialize the interface from some other package.
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Mohamad Chaarawi
 *              September 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5ES_init(void)
{
    herr_t ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(FAIL)
    /* FUNC_ENTER() does all the work */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5ES_init() */


/*--------------------------------------------------------------------------
NAME
   H5ES__init_package -- Initialize interface-specific information
USAGE
    herr_t H5ES__init_package()

RETURNS
    Non-negative on success/Negative on failure
DESCRIPTION
    Initializes any interface-specific data or routines.

--------------------------------------------------------------------------*/
herr_t
H5ES__init_package(void)
{
    herr_t ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Initialize the atom group for the ES IDs */
    if(H5I_register_type(H5I_ES_CLS) < 0)
	HGOTO_ERROR(H5E_DATASPACE, H5E_CANTINIT, FAIL, "unable to initialize interface")

    H5_PKG_INIT_VAR = TRUE;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5ES__init_package() */


/*--------------------------------------------------------------------------
 NAME
    H5ES_term_package
 PURPOSE
    Terminate various H5ES objects
 USAGE
    void H5ES_term_package()
 RETURNS
    Non-negative on success/Negative on failure
 DESCRIPTION
    Release the atom group and any other resources allocated.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
     Can't report errors...
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
int
H5ES_term_package(void)
{
    int	n = 0;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    if(H5_PKG_INIT_VAR) {
	if(H5I_nmembers(H5I_ES) > 0) {
	    (void)H5I_clear_type(H5I_ES, FALSE, FALSE);
            n++; /*H5I*/
	} /* end if */
        else {
            n += (H5I_dec_type_ref(H5I_ES) > 0);

            /* Mark closed */
            if(0 == n)
                H5_PKG_INIT_VAR = FALSE;
        } /* end else */
    } /* end if */

    FUNC_LEAVE_NOAPI(n)
} /* end H5ES_term_package() */


/*-------------------------------------------------------------------------
 * Function:	H5EScreate
 *
 * Purpose:	Creates an Event Stack used to manage async requests
 *
 * Return:	Success:	The ID for a new event stack.
 *		Failure:	FAIL
 *
 * Programmer:	Mohamad Chaarawi
 *		September 2013
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5EScreate(void)
{
    H5ES_t *e_stack = NULL;                    /* event stack token */
    hid_t ret_value = SUCCEED;          /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE0("i","");

    /* Allocate the event stack structure */
    if(NULL == (e_stack = H5FL_CALLOC(H5ES_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate space for event stack");

    e_stack->head = e_stack->tail = NULL;
    e_stack->size = 0;

    /* Get an atom for the event stack with the VOL information as the auxilary struct*/
    if((ret_value = H5I_register(H5I_ES, e_stack, TRUE)) < 0)
	HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to atomize event stack handle")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5EScreate() */


/*-------------------------------------------------------------------------
 * Function:	H5ES_insert
 *
 * Purpose:	Inserts an async request into an Event Stack.
 *
 * Return:	Success:	SUCCEED
 *		Failure:	FAIL
 *
 * Programmer:	Mohamad Chaarawi
 *		September 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5ES_insert(hid_t es_id, H5_priv_request_t *req)
{
    H5ES_t *e_stack = NULL;
    herr_t ret_value = SUCCEED;          /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Check arguments. */
    if(NULL == req)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "bad request");

    /* get the e_stack object */
    if(NULL == (e_stack = (H5ES_t *)H5I_object_verify(es_id, H5I_ES)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid event stack identifier")
 
    req->next = NULL;
    req->prev = NULL;

    if(NULL == e_stack->head && NULL == e_stack->tail) {
        e_stack->head = e_stack->tail = req;
    }
    else {
        e_stack->head->prev = req;
        req->next = e_stack->head;
        e_stack->head = req;
    }
    e_stack->size ++;
    e_stack->in_progress ++;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5ES_insert() */


/*-------------------------------------------------------------------------
 * Function:	H5ESwait
 *
 * Purpose:	Waits on an async request with the specified index in 
 *              the Event Stack.
 *
 * Return:	Success:	SUCCEED
 *		Failure:	FAIL
 *
 * Programmer:	Mohamad Chaarawi
 *		September 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5ESwait(hid_t es_id, size_t event_idx, H5ES_status_t *status)
{
    H5ES_t *e_stack = NULL;                    /* event stack token */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE3("e", "iz*Es", es_id, event_idx, status);

    /* get the e_stack object */
    if(NULL == (e_stack = (H5ES_t *)H5I_object_verify(es_id, H5I_ES)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid event stack identifier")

    /* Wait request in the event stack */
    if(H5ES_wait(e_stack, event_idx, status) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "unable to wait request into event stack")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5ESwait() */


/*-------------------------------------------------------------------------
 * Function:	H5ES_wait
 *
 * Purpose:	Private routine for H5ESwait
 *
 * Return:	Success:	SUCCEED
 *		Failure:	FAIL
 *
 * Programmer:	Mohamad Chaarawi
 *		September 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5ES_wait(H5ES_t *e_stack, size_t event_idx, H5ES_status_t *_status)
{
    H5_priv_request_t *cur = NULL;
    size_t u = 0;
    H5ES_status_t status;
    herr_t ret_value = SUCCEED;          /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    if(event_idx >= e_stack->size)
        HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "index is out of range for event stack")

    cur = e_stack->head;
    HDassert(NULL != cur);

    for(u=0 ; u<event_idx ; u++) {
        cur = cur->next;
        HDassert(NULL != cur);
    }

    if(H5ES_STATUS_IN_PROGRESS == cur->status) {
        /* wait on the request */
        if(H5VL_request_wait(&cur->req, cur->vol_cls, &status) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "unable to test request");

        e_stack->in_progress --;
        cur->status = status;
    }

    *_status = cur->status;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5ES_wait() */


/*-------------------------------------------------------------------------
 * Function:	H5ESwait_all
 *
 * Purpose:	Waits on all async requests in an Event Stack.
 *
 * Return:	Success:	SUCCEED
 *		Failure:	FAIL
 *
 * Programmer:	Mohamad Chaarawi
 *		September 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5ESwait_all(hid_t es_id, H5ES_status_t *status)
{
    H5ES_t *e_stack = NULL;                    /* event stack token */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "i*Es", es_id, status);

    /* get the e_stack object */
    if(NULL == (e_stack = (H5ES_t *)H5I_object_verify(es_id, H5I_ES)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid event stack identifier")

    /* Wait on all requests in the event stack */
    if(H5ES_wait_all(e_stack, status) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "unable to wait request into event stack")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5ESwait_all() */


/*-------------------------------------------------------------------------
 * Function:	H5ES_wait_all
 *
 * Purpose:     Private routine for H5ESwait_all
 *
 * Return:	Success:	SUCCEED
 *		Failure:	FAIL
 *
 * Programmer:	Mohamad Chaarawi
 *		September 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5ES_wait_all(H5ES_t *e_stack, H5ES_status_t *_status)
{
    H5_priv_request_t *cur = NULL;
    size_t u = 0;
    size_t succeed_count = 0, fail_count = 0, cancel_count = 0;
    H5ES_status_t status;
    herr_t ret_value = SUCCEED;          /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    if(e_stack->size == 0) {
        HGOTO_DONE(SUCCEED);
    }

    cur = e_stack->tail;

    for(u=0 ; u<e_stack->size ; u++) {
        HDassert(NULL != cur);

        if(cur->status == H5ES_STATUS_IN_PROGRESS) {
            /* wait on the request */
            if(H5VL_request_wait(&cur->req, cur->vol_cls, &status) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "unable to test request");
            e_stack->in_progress --;
            cur->status = status;
        }
        else {
            status = cur->status;
        }

        switch(status) {
        case H5ES_STATUS_SUCCEED:
            succeed_count ++;
            break;
        case H5ES_STATUS_FAIL:
            fail_count ++;
            break;
        case H5ES_STATUS_CANCEL:
            cancel_count ++;
            break;
        case H5ES_STATUS_IN_PROGRESS:
        default:
            HGOTO_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "waiting on request returned invalid status");
        }

        cur = cur->prev;
    }

    if(0 != e_stack->in_progress) {
        HGOTO_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "Failed to wait on all requests");
    }

    if(fail_count)
        status = H5ES_STATUS_FAIL;
    else if(cancel_count == e_stack->size)
        status = H5ES_STATUS_CANCEL;
    else if(succeed_count == e_stack->size ||
            succeed_count+cancel_count == e_stack->size)
        status = H5ES_STATUS_SUCCEED;

    *_status = status;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5ES_wait_all() */


/*-------------------------------------------------------------------------
 * Function:	H5EStest
 *
 * Purpose:	Tests on an async request with the specified index in 
 *              the Event Stack.
 *
 * Return:	Success:	SUCCEED
 *		Failure:	FAIL
 *
 * Programmer:	Mohamad Chaarawi
 *		September 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5EStest(hid_t es_id, size_t event_idx, H5ES_status_t *status)
{
    H5ES_t *e_stack = NULL;                    /* event stack token */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE3("e", "iz*Es", es_id, event_idx, status);

    /* get the e_stack object */
    if(NULL == (e_stack = (H5ES_t *)H5I_object_verify(es_id, H5I_ES)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid event stack identifier")

    /* Test request in the event stack */
    if(H5ES_test(e_stack, event_idx, status) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "unable to test request in event stack")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5EStest() */


/*-------------------------------------------------------------------------
 * Function:	H5ES_test
 *
 * Purpose:	Private routine for H5EStest
 *
 * Return:	Success:	SUCCEED
 *		Failure:	FAIL
 *
 * Programmer:	Mohamad Chaarawi
 *		September 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5ES_test(H5ES_t *e_stack, size_t event_idx, H5ES_status_t *_status)
{
    H5_priv_request_t *cur = NULL;
    size_t u = 0;
    H5ES_status_t status;
    herr_t ret_value = SUCCEED;          /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    if(event_idx >= e_stack->size)
        HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "index is out of range for event stack")

    cur = e_stack->head;

    for(u=0 ; u<event_idx ; u++) {
        HDassert(NULL != cur);
        cur = cur->next;
    }

    if(H5ES_STATUS_IN_PROGRESS == cur->status) {
        /* test on the request */
        if(H5VL_request_test(&cur->req, cur->vol_cls, &status) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "unable to test request");

        cur->status = status;

        if(H5ES_STATUS_IN_PROGRESS != status)
            e_stack->in_progress --;
    }

    *_status = cur->status;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5ES_test() */


/*-------------------------------------------------------------------------
 * Function:	H5EStest_all
 *
 * Purpose:	Tests on all async requests in an Event Stack.
 *
 * Return:	Success:	SUCCEED
 *		Failure:	FAIL
 *
 * Programmer:	Mohamad Chaarawi
 *		September 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5EStest_all(hid_t es_id, H5ES_status_t *status)
{
    H5ES_t *e_stack = NULL;                    /* event stack token */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "i*Es", es_id, status);

    /* get the e_stack object */
    if(NULL == (e_stack = (H5ES_t *)H5I_object_verify(es_id, H5I_ES)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid event stack identifier")

    /* Test on all requests in the event stack */
    if(H5ES_test_all(e_stack, status) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "unable to test request into event stack")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5EStest_all() */


/*-------------------------------------------------------------------------
 * Function:	H5ES_test_all
 *
 * Purpose:     Private routine for H5EStest_all
 *
 * Return:	Success:	SUCCEED
 *		Failure:	FAIL
 *
 * Programmer:	Mohamad Chaarawi
 *		September 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5ES_test_all(H5ES_t *e_stack, H5ES_status_t *_status)
{
    H5_priv_request_t *cur = NULL;
    size_t u = 0;
    size_t succeed_count = 0, fail_count = 0, cancel_count = 0;
    size_t in_progress_count = 0;
    H5ES_status_t status;
    herr_t ret_value = SUCCEED;          /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    if(e_stack->size == 0) {
        HGOTO_DONE(SUCCEED);
    }

    cur = e_stack->tail;

    for(u=0 ; u<e_stack->size ; u++) {
        HDassert(NULL != cur);

        if(cur->status == H5ES_STATUS_IN_PROGRESS) {
            /* test on the request */
            if(H5VL_request_test(&cur->req, cur->vol_cls, &status) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "unable to test request");

            if(status != H5ES_STATUS_IN_PROGRESS) {
                cur->status = status;
                e_stack->in_progress --;
            }
        }
        else {
            status = cur->status;
        }

        switch(status) {
        case H5ES_STATUS_SUCCEED:
            succeed_count ++;
            break;
        case H5ES_STATUS_FAIL:
            fail_count ++;
            break;
        case H5ES_STATUS_CANCEL:
            cancel_count ++;
            break;
        case H5ES_STATUS_IN_PROGRESS:
            in_progress_count ++;
            break;
        default:
            HGOTO_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "testing on request returned invalid status");
        }

        cur = cur->prev;
    }

    HDassert(in_progress_count == e_stack->in_progress);

    if(in_progress_count)
        status = H5ES_STATUS_IN_PROGRESS;
    else if(fail_count)
        status = H5ES_STATUS_FAIL;
    else if(cancel_count == e_stack->size)
        status = H5ES_STATUS_CANCEL;
    else if(succeed_count == e_stack->size ||
            succeed_count+cancel_count == e_stack->size)
        status = H5ES_STATUS_SUCCEED;

    *_status = status;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5ES_test_all() */


/*-------------------------------------------------------------------------
 * Function:	H5EScancel
 *
 * Purpose:	Cancels on an async request with the specified index in 
 *              the Event Stack.
 *
 * Return:	Success:	SUCCEED
 *		Failure:	FAIL
 *
 * Programmer:	Mohamad Chaarawi
 *		September 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5EScancel(hid_t es_id, size_t event_idx, H5ES_status_t *status)
{
    H5ES_t *e_stack = NULL;                    /* event stack token */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE3("e", "iz*Es", es_id, event_idx, status);

    /* get the e_stack object */
    if(NULL == (e_stack = (H5ES_t *)H5I_object_verify(es_id, H5I_ES)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid event stack identifier")

    /* Cancel request in the event stack */
    if(H5ES_cancel(e_stack, event_idx, status) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "unable to cancel request in event stack")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5EScancel() */


/*-------------------------------------------------------------------------
 * Function:	H5ES_cancel
 *
 * Purpose:	Private routine for H5EScancel
 *
 * Return:	Success:	SUCCEED
 *		Failure:	FAIL
 *
 * Programmer:	Mohamad Chaarawi
 *		September 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5ES_cancel(H5ES_t *e_stack, size_t event_idx, H5ES_status_t *_status)
{
    H5_priv_request_t *cur = NULL;
    size_t u = 0;
    H5ES_status_t status;
    herr_t ret_value = SUCCEED;          /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    if(event_idx >= e_stack->size)
        HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "index is out of range for event stack")

    cur = e_stack->head;

    for(u=0 ; u<event_idx ; u++) {
        HDassert(NULL != cur);
        cur = cur->next;
    }

    if(cur->status == H5ES_STATUS_IN_PROGRESS) {
        /* cancel on the request */
        if(H5VL_request_cancel(&cur->req, cur->vol_cls, &status) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "unable to cancel request");

        cur->status = status;
        e_stack->in_progress --;
    }

    *_status = cur->status;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5ES_cancel() */


/*-------------------------------------------------------------------------
 * Function:	H5EScancel_all
 *
 * Purpose:	Cancels on all async requests in an Event Stack.
 *
 * Return:	Success:	SUCCEED
 *		Failure:	FAIL
 *
 * Programmer:	Mohamad Chaarawi
 *		September 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5EScancel_all(hid_t es_id, H5ES_status_t *status)
{
    H5ES_t *e_stack = NULL;                    /* event stack token */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "i*Es", es_id, status);

    /* get the e_stack object */
    if(NULL == (e_stack = (H5ES_t *)H5I_object_verify(es_id, H5I_ES)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid event stack identifier")

    /* Cancel on all requests in the event stack */
    if(H5ES_cancel_all(e_stack, status) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "unable to cancel request into event stack")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5EScancel_all() */


/*-------------------------------------------------------------------------
 * Function:	H5ES_cancel_all
 *
 * Purpose:     Private routine for H5EScancel_all
 *
 * Return:	Success:	SUCCEED
 *		Failure:	FAIL
 *
 * Programmer:	Mohamad Chaarawi
 *		September 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5ES_cancel_all(H5ES_t *e_stack, H5ES_status_t *_status)
{
    H5_priv_request_t *cur = NULL;
    size_t u = 0;
    size_t succeed_count = 0, fail_count = 0, cancel_count = 0;
    H5ES_status_t status;
    herr_t ret_value = SUCCEED;          /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    if(e_stack->size == 0) {
        HGOTO_DONE(SUCCEED);
    }

    cur = e_stack->tail;

    for(u=0 ; u<e_stack->size ; u++) {
        HDassert(NULL != cur);

        if(cur->status == H5ES_STATUS_IN_PROGRESS) {
            /* cancel on the request */
            if(H5VL_request_cancel(&cur->req, cur->vol_cls, &status) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "unable to cancel request");
            e_stack->in_progress --;
            cur->status = status;
        }
        else {
            status = cur->status;
        }

        switch(status) {
        case H5ES_STATUS_SUCCEED:
            succeed_count ++;
            break;
        case H5ES_STATUS_FAIL:
            fail_count ++;
            break;
        case H5ES_STATUS_CANCEL:
            cancel_count ++;
            break;
        case H5ES_STATUS_IN_PROGRESS:
        default:
            HGOTO_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "canceling on request returned invalid status");
        }

        cur = cur->prev;
    }

    if(0 != e_stack->in_progress) {
        HGOTO_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "Failed to wait on all requests");
    }

    if(fail_count)
        status = H5ES_STATUS_FAIL;
    else if(cancel_count == e_stack->size)
        status = H5ES_STATUS_CANCEL;
    else if(succeed_count == e_stack->size ||
            succeed_count+cancel_count == e_stack->size)
        status = H5ES_STATUS_SUCCEED;

    *_status = status;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5ES_cancel_all() */


/*-------------------------------------------------------------------------
 * Function:	H5ESget_count
 *
 * Purpose:	Returns number of events in event stack.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Mohamad Chaarawi
 *		September 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5ESget_count(hid_t es_id, size_t *count)
{
    H5ES_t *e_stack = NULL;
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "i*z", es_id, count);

    /* get the e_stack object */
    if(NULL == (e_stack = (H5ES_t *)H5I_object_verify(es_id, H5I_ES)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid event stack identifier")

    *count = e_stack->size;

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5ESget_count() */


/*-------------------------------------------------------------------------
 * Function:	H5ESclear
 *
 * Purpose:	Clear all events from the event stack.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Mohamad Chaarawi
 *		September 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5ESclear(hid_t es_id)
{
    H5ES_t *e_stack = NULL;
    H5_priv_request_t *tail = NULL, *prev = NULL;
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE1("e", "i", es_id);

    /* get the e_stack object */
    if(NULL == (e_stack = (H5ES_t *)H5I_object_verify(es_id, H5I_ES)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid event stack identifier")

    if(e_stack->size == 0) {
        HGOTO_DONE(SUCCEED);
    }

    if(0 != e_stack->in_progress)
        HGOTO_ERROR(H5E_SYM, H5E_CANTRELEASE, FAIL, "can't clear stack with events in progress")

    while(e_stack->tail) {
        /* Pop the last item and manage the stack accordingly */
        tail = e_stack->tail;
        prev = tail->prev;
        e_stack->tail = prev;

        if(NULL == e_stack->tail)
            e_stack->head = e_stack->tail;

        /* free the request */
        HDassert(H5ES_STATUS_IN_PROGRESS != tail->status);

        tail->vol_cls = NULL;
        tail->req = NULL;
        tail->next = NULL;
        tail->prev = NULL;
        tail = (H5_priv_request_t *)H5MM_xfree(tail);

        e_stack->size --;
    }

    HDassert(0 == e_stack->size);

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5ESclear() */


/*-------------------------------------------------------------------------
 * Function:	H5ESclose
 *
 * Purpose:	Closes the specified event stack.  The ID will no longer be
 *		valid for accessing the event stack.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Mohamad Chaarawi
 *		September 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5ESclose(hid_t es_id)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE1("e", "i", es_id);

    /* Check args */
    if(NULL == H5I_object_verify(es_id,H5I_ES))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not an event stack")

    if(H5I_dec_app_ref(es_id) < 0)
    	HGOTO_ERROR(H5E_SYM, H5E_CANTRELEASE, FAIL, "unable to close event stack")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5ESclose() */


/*-------------------------------------------------------------------------
 * Function:	H5ES_close
 *
 * Purpose:	Close an event stack object
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Mohamad Chaarawi
 *              September 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5ES_close(H5ES_t *e_stack)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    e_stack = H5FL_FREE(H5ES_t, e_stack);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5ES_close() */
