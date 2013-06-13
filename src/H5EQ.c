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
 * Programmer:  Quincey Koziol <koziol@hdfgroup.org>
 *              March, 2013
 *
 * Purpose:	Wrappers around existing HDF5 to support Exascale FastForward
 *              functionality.
 *              
 */


/****************/
/* Module Setup */
/****************/

/* Interface initialization */
#define H5_INTERFACE_INIT_FUNC	H5EQ_init_interface

/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5EQprivate.h"        /* Event Queues                         */
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


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/

/* Declare a free list to manage the H5S_t struct */
H5FL_DEFINE(H5EQ_t);

/* Dataspace ID class */
static const H5I_class_t H5I_EQ_CLS[1] = {{
    H5I_EQ,        		/* ID class value */
    0,				/* Class flags */
    64,				/* Minimum hash size for class */
    2,				/* # of reserved IDs for class */
    NULL,                   	/* Callback routine for closing objects of this class */
    (H5I_free2_t)H5EQ_close     /* Callback routine for closing auxilary objects of this class */
}};



/*-------------------------------------------------------------------------
 * Function:	H5EQ_init
 *
 * Purpose:	Initialize the interface from some other package.
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Mohamad Chaarawi
 *              April 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5EQ_init(void)
{
    herr_t ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(FAIL)
    /* FUNC_ENTER() does all the work */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5EQ_init() */


/*--------------------------------------------------------------------------
NAME
   H5EQ_init_interface -- Initialize interface-specific information
USAGE
    herr_t H5EQ_init_interface()

RETURNS
    Non-negative on success/Negative on failure
DESCRIPTION
    Initializes any interface-specific data or routines.

--------------------------------------------------------------------------*/
static herr_t
H5EQ_init_interface(void)
{
    herr_t ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Initialize the atom group for the file IDs */
    if(H5I_register_type(H5I_EQ_CLS) < 0)
	HGOTO_ERROR(H5E_DATASPACE, H5E_CANTINIT, FAIL, "unable to initialize interface")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5EQ_init_interface() */


/*--------------------------------------------------------------------------
 NAME
    H5EQ_term_interface
 PURPOSE
    Terminate various H5EQ objects
 USAGE
    void H5EQ_term_interface()
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
H5EQ_term_interface(void)
{
    int	n = 0;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    if(H5_interface_initialize_g) {
	if((n = H5I_nmembers(H5I_EQ))) {
	    H5I_clear_type(H5I_EQ, FALSE, FALSE);
	} /* end if */
        else {
	    /* Free data types */
	    H5I_dec_type_ref(H5I_EQ);

	    /* Shut down interface */
	    H5_interface_initialize_g = 0;
	    n = 1; /*H5I*/
	} /* end else */
    } /* end if */

    FUNC_LEAVE_NOAPI(n)
} /* end H5EQ_term_interface() */


/*-------------------------------------------------------------------------
 * Function:	H5EQcreate
 *
 * Purpose:	Creates an Event Queue used to manage async requests
 *
 * Return:	Success:	The ID for a new event queue.
 *		Failure:	FAIL
 *
 * Programmer:	Mohamad Chaarawi
 *		March 2013
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5EQcreate(hid_t fapl_id)
{
    void *eq = NULL;                    /* event queue token */
    H5VL_t *vol_plugin = NULL;          /* VOL plugin information from fapl */
    H5P_genplist_t  *plist;             /* Property list pointer */
    hid_t ret_value = SUCCEED;          /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE1("i", "i", fapl_id);

    /* Check the file access property list */
    if(H5P_DEFAULT == fapl_id)
        fapl_id = H5P_FILE_ACCESS_DEFAULT;
    else
        if(TRUE != H5P_isa_class(fapl_id, H5P_FILE_ACCESS))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not file access property list")

    /* Get the plist structure */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(fapl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADATOM, FAIL, "can't find object for ID")

    /* create a new event queue */
    if(NULL == (eq = H5EQ_create(&vol_plugin, plist)))
	HGOTO_ERROR(H5E_SYM, H5E_CANTCREATE, FAIL, "unable to create event queue")

    /* Get an atom for the event queue with the VOL information as the auxilary struct*/
    if((ret_value = H5I_register2(H5I_EQ, eq, vol_plugin, TRUE)) < 0)
	HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to atomize event queue handle")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5EQcreate() */


/*-------------------------------------------------------------------------
 * Function:	H5EQinsert
 *
 * Purpose:	Inserts an async request into an Event Queue.
 *
 * Return:	Success:	SUCCEED
 *		Failure:	FAIL
 *
 * Programmer:	Mohamad Chaarawi
 *		March 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5EQinsert(hid_t eq_id, H5_request_t req)
{
    H5EQ_t *eq = NULL;                    /* event queue token */
    herr_t ret_value = SUCCEED;          /* Return value */

    FUNC_ENTER_API(FAIL)

    /* Check arguments. */
    if(H5I_EQ != H5I_get_type(eq_id))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not an event queue ID")

    /* get the eq object */
    if(NULL == (eq = (H5EQ_t *)H5I_object(eq_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid event queue identifier")

    /* Insert request in the event queue */
    if(H5EQ_insert(eq, (H5_priv_request_t *)req) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTINSERT, FAIL, "unable to insert request into event queue")
done:
    FUNC_LEAVE_API(ret_value)
} /* end H5EQinsert() */


/*-------------------------------------------------------------------------
 * Function:	H5EQpop
 *
 * Purpose:	retrieves the top async request from an Event Queue.
 *
 * Return:	Success:	SUCCEED
 *		Failure:	FAIL
 *
 * Programmer:	Mohamad Chaarawi
 *		March 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5EQpop(hid_t eq_id, H5_request_t *req/*OUT*/)
{
    H5EQ_t *eq = NULL;                     /* event queue token */
    H5_priv_request_t *priv_req = NULL;  /* internal request struct */
    H5_priv_request_t *head = NULL;
    H5_priv_request_t *next = NULL;
    herr_t ret_value = SUCCEED;          /* Return value */

    FUNC_ENTER_API(FAIL)

    /* Check arguments. */
    if(H5I_EQ != H5I_get_type(eq_id))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not an event queue ID")

    /* get the eq object */
    if(NULL == (eq = (H5EQ_t *)H5I_object(eq_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid event queue identifier");

    priv_req = eq->head;

    head = eq->head;
    next = head->next;

    eq->head = next;
    if(NULL == eq->head)
        eq->tail = eq->head;

    eq->size --;

    *req = (H5_request_t) priv_req;

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5EQpop() */


/*-------------------------------------------------------------------------
 * Function:	H5EQwait
 *
 * Purpose:	Waits on all async requests in an Event Queue.
 *
 * Return:	Success:	SUCCEED
 *		Failure:	FAIL
 *
 * Programmer:	Mohamad Chaarawi
 *		March 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5EQwait(hid_t eq_id, int *num_requests/*OUT*/, H5_status_t **status/*OUT*/)
{
    void *eq = NULL;                    /* event queue token */
    H5VL_t *vol_plugin = NULL;          /* VOL plugin pointer this event queue should use */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API(FAIL)

    /* Check arguments. */
    if(H5I_EQ != H5I_get_type(eq_id))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not an event queue ID")

    /* get the eq object */
    if(NULL == (eq = (void *)H5I_object(eq_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid event queue identifier")

    /* get the plugin pointer */
    if (NULL == (vol_plugin = (H5VL_t *)H5I_get_aux(eq_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information");

    /* Wait request in the event queue */
    if(H5EQ_wait((H5EQ_t *)eq, vol_plugin, num_requests, status) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "unable to wait request into event queue")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5EQwait() */


/*-------------------------------------------------------------------------
 * Function:	H5EQclose
 *
 * Purpose:	Closes the specified event queue.  The ID will no longer be
 *		valid for accessing the event queue.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Mohamad Chaarawi
 *		April 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5EQclose(hid_t eq_id)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE1("e", "i", eq_id);

    /* Check args */
    if(NULL == H5I_object_verify(eq_id,H5I_EQ))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not an event queue")

    if(H5I_dec_app_ref(eq_id) < 0)
    	HGOTO_ERROR(H5E_SYM, H5E_CANTRELEASE, FAIL, "unable to close event queue")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5EQclose() */


/*-------------------------------------------------------------------------
 * Function:	H5AOcancel
 *
 * Purpose:	Cancel an asynchronous operation
 *
 * Return:	Success:	SUCCEED
 *		Failure:	FAIL
 *
 * Programmer:	Quincey Koziol
 *		May, 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5AOcancel(H5_request_t req, H5_status_t *status)
{
    H5_priv_request_t *request = (H5_priv_request_t *)req;
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_API(FAIL)

    if(H5VL_request_cancel(&request->req, request->vol_plugin, status) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "unable to cancel request");

    request->req = NULL;
    request->vol_plugin = NULL;
    request->next = NULL;
    request = (H5_priv_request_t *)H5MM_xfree(request);
    
done:
    FUNC_LEAVE_API(ret_value)
} /* end H5AOcancel() */


/*-------------------------------------------------------------------------
 * Function:	H5AOtest
 *
 * Purpose:	Test for an asynchronous operation's completion
 *
 * Return:	Success:	SUCCEED
 *		Failure:	FAIL
 *
 * Programmer:	Quincey Koziol
 *		Wednesday, March 20, 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5AOtest(H5_request_t req, H5_status_t *status)
{
    H5_priv_request_t *request = (H5_priv_request_t *)req;
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_API(FAIL)

    if(H5VL_request_test(&request->req, request->vol_plugin, status) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "unable to test request");

    if(*status != H5AO_PENDING) {
        request->req = NULL;
        request->vol_plugin = NULL;
        request->next = NULL;
        request = (H5_priv_request_t *)H5MM_xfree(request);
    }
    
done:
    FUNC_LEAVE_API(ret_value)
} /* end H5AOtest() */


/*-------------------------------------------------------------------------
 * Function:	H5AOwait
 *
 * Purpose:	Wait for an asynchronous operation to complete
 *
 * Return:	Success:	SUCCEED
 *		Failure:	FAIL
 *
 * Programmer:	Quincey Koziol
 *		Wednesday, March 20, 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5AOwait(H5_request_t req, H5_status_t *status)
{
    H5_priv_request_t *request = (H5_priv_request_t *)req;
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_API(FAIL)

    if(H5VL_request_wait(&request->req, request->vol_plugin, status) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "unable to test request");

    request->req = NULL;
    request->vol_plugin = NULL;
    request->next = NULL;
    request = (H5_priv_request_t *)H5MM_xfree(request);

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5AOwait() */


/*-------------------------------------------------------------------------
 * Function:	H5EQcreate
 *
 * Purpose:	Creates an Event Queue used to manage async requests
 *
 * Return:	Success:	The ID for a new event queue.
 *		Failure:	FAIL
 *
 * Programmer:	Mohamad Chaarawi
 *		March 2013
 *
 *-------------------------------------------------------------------------
 */
H5EQ_t *
H5EQ_create(H5VL_t **plugin, H5P_genplist_t *plist)
{
    H5EQ_t *eq = NULL;          /* Event Queue object */
    H5VL_class_t  *vol_cls;               /* VOL class attached to fapl_id */
    H5VL_t        *vol_plugin = NULL;     /* the public VOL struct */
    H5EQ_t *ret_value = NULL;   /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    if(H5P_get(plist, H5F_ACS_VOL_NAME, &vol_cls) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get vol plugin ID")

    /* Allocate the event queue structure */
    if(NULL == (eq = H5FL_CALLOC(H5EQ_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate space for event queue");

    eq->head = eq->tail = NULL;
    eq->size = 0;

    /* Build the vol plugin struct */
    if(NULL == (*plugin = (H5VL_t *)H5MM_calloc(sizeof(H5VL_t))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    vol_plugin = *plugin;
    vol_plugin->cls = vol_cls;
    vol_plugin->nrefs = 1;
    vol_plugin->container_name = NULL;

    ret_value = eq;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5EQcreate() */


/*-------------------------------------------------------------------------
 * Function:	H5EQ_insert
 *
 * Purpose:	Inserts an async request into an Event Queue.
 *
 * Return:	Success:	SUCCEED
 *		Failure:	FAIL
 *
 * Programmer:	Mohamad Chaarawi
 *		March 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5EQ_insert(H5EQ_t *eq, H5_priv_request_t *req)
{
    herr_t ret_value = SUCCEED;          /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == req)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "bad request");
    if(NULL == eq)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "event queue not intialized");
 
    req->next = NULL;
    req->prev = NULL;

    if(NULL == eq->head && NULL == eq->tail) {
        eq->head = eq->tail = req;
    }
    else {
        eq->head->prev = req;
        req->next = eq->head;
        eq->head = req;
    }
    eq->size ++;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5EQ_insert() */


/*-------------------------------------------------------------------------
 * Function:	H5EQ_wait
 *
 * Purpose:	Waits on all async requests in an Event Queue.
 *
 * Return:	Success:	SUCCEED
 *		Failure:	FAIL
 *
 * Programmer:	Mohamad Chaarawi
 *		March 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5EQ_wait(H5EQ_t *eq, H5VL_t *vol_plugin, int *num_requests/*OUT*/, H5_status_t **_status/*OUT*/)
{
    H5_priv_request_t *cur = NULL, *tail = NULL, *prev = NULL;
    int i = 0;
    H5_status_t *status = NULL;
    herr_t ret_value = SUCCEED;          /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == eq)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "event queue not intialized");

    if(NULL == (status = (H5_status_t *)HDmalloc (sizeof(H5_status_t) * eq->size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate space for status array");

    *num_requests = eq->size;

    cur = eq->head;

    while(eq->tail) {
        /* Pop the last item and manage the queue accordingly */
        tail = eq->tail;
        prev = tail->prev;
        eq->tail = prev;

        if(NULL == eq->tail)
            eq->head = eq->tail;

        /* wait on and free the request */
        if(tail->req && H5VL_request_wait(&tail->req, vol_plugin, &status[i++]) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "unable to test request");
        tail->req = NULL;
        tail->vol_plugin = NULL;
        tail->next = NULL;
        tail->prev = NULL;
        tail = (H5_priv_request_t *)H5MM_xfree(tail);

        eq->size --;
    }

    *_status = status;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5EQ_wait() */


/*-------------------------------------------------------------------------
 * Function:	H5EQ_close
 *
 * Purpose:	Close an event queue object
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Mohamad Chaarawi
 *              March 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5EQ_close(void *eq, H5VL_t *vol_plugin)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    eq = H5FL_FREE(H5EQ_t, eq);

    /* decrement ref count on VOL plugin */
    vol_plugin->nrefs --;
    /* close if ref count is zero */
    if (0 == vol_plugin->nrefs)
        vol_plugin = (H5VL_t *)H5MM_xfree(vol_plugin);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5EQ_close_group() */
