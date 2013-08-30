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
 *              August, 2013
 *
 * Purpose:	Transaction APIs to support Exascale FastForward
 *              functionality.
 *              
 */


/****************/
/* Module Setup */
/****************/

/* Interface initialization */
#define H5_INTERFACE_INIT_FUNC	H5TR_init_interface

/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5TRprivate.h"        /* Transactions                         */
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

/* Declare a free list to manage the H5TR_t struct */
H5FL_DEFINE(H5TR_t);

/* Dataspace ID class */
static const H5I_class_t H5I_TR_CLS[1] = {{
    H5I_TR,        		/* ID class value */
    0,				/* Class flags */
    64,				/* Minimum hash size for class */
    2,				/* # of reserved IDs for class */
    NULL,                   	/* Callback routine for closing objects of this class */
    (H5I_free2_t)H5TR_close     /* Callback routine for closing auxilary objects of this class */
}};



/*-------------------------------------------------------------------------
 * Function:	H5TR_init
 *
 * Purpose:	Initialize the interface from some other package.
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Mohamad Chaarawi
 *              August 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5TR_init(void)
{
    herr_t ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(FAIL)
    /* FUNC_ENTER() does all the work */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5TR_init() */


/*--------------------------------------------------------------------------
NAME
   H5TR_init_interface -- Initialize interface-specific information
USAGE
    herr_t H5TR_init_interface()

RETURNS
    Non-negative on success/Negative on failure
DESCRIPTION
    Initializes any interface-specific data or routines.

--------------------------------------------------------------------------*/
static herr_t
H5TR_init_interface(void)
{
    herr_t ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Initialize the atom group for the TR IDs */
    if(H5I_register_type(H5I_TR_CLS) < 0)
	HGOTO_ERROR(H5E_DATASPACE, H5E_CANTINIT, FAIL, "unable to initialize interface")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5TR_init_interface() */


/*--------------------------------------------------------------------------
 NAME
    H5TR_term_interface
 PURPOSE
    Terminate various H5TR objects
 USAGE
    void H5TR_term_interface()
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
H5TR_term_interface(void)
{
    int	n = 0;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    if(H5_interface_initialize_g) {
	if((n = H5I_nmembers(H5I_TR))) {
	    H5I_clear_type(H5I_TR, FALSE, FALSE);
	} /* end if */
        else {
	    /* Free data types */
	    H5I_dec_type_ref(H5I_TR);

	    /* Shut down interface */
	    H5_interface_initialize_g = 0;
	    n = 1; /*H5I*/
	} /* end else */
    } /* end if */

    FUNC_LEAVE_NOAPI(n)
} /* end H5TR_term_interface() */


/*-------------------------------------------------------------------------
 * Function:	H5Pset_trspl_num_peers
 *
 * Purpose: Set the number of peers that will call H5TRstart for a
 * transaction. If num_peers=0, the application has appointed a
 * transaction leader to signal the start and finish of the
 * transaction, and to coordinate with processes participating in the
 * transaction regarding when they can begin updating and when their
 * updates are done.  When this is the case, only one process (the
 * transaction leader) can call H5TRstart and H5TRfinish for the given
 * transaction_num.  This mode of operation is typical for
 * tightly-coupled applications.  If num_peers>0, the I/O stack (in
 * particular, IOD) will track the status of the transaction.  All
 * num_peers processes participating in the transaction must call
 * H5TRstart with the same values for num_peers and transaction_num.
 * As each participating process is done with their updates in the
 * transaction, they individually call H5TRfinish for this
 * transaction_num. When all num_peers processes have called
 * H5TRfinish for this transaction_num, the I/O stack detects that the
 * transaction is finished and updates appear atomically when all
 * lower-numbered transactions are finished, aborted, or skipped.
 * This mode of operation is typical for loosely-coupled applications.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              August 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_trspl_num_peers(hid_t trspl_id, unsigned num_peers)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value = SUCCEED; /* return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "iIu", trspl_id, num_peers);

    /* Get the plist structure */
    if(NULL == (plist = H5P_object_verify(trspl_id, H5P_TR_START)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");

    /* Set property */
    if(H5P_set(plist, H5TR_START_NUM_PEERS_NAME, &num_peers) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET,FAIL, "can't set num peers in transaction");

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_trspl_num_peers() */


/*-------------------------------------------------------------------------
 * Function:	H5TRcreate
 *
 * Purpose:	Wraps an hid_t around a transaction number, a file ID, 
 *              and a read context ID on that file that operations using 
 *              the created transaction will read from.
 *
 * Return:	Success:	The ID for a new transaction.
 *		Failure:	FAIL
 *
 * Programmer:	Mohamad Chaarawi
 *		August 2013
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5TRcreate(hid_t file_id, hid_t rc_id, uint64_t trans_num)
{
    void *file = NULL;
    H5VL_t *vol_plugin = NULL;
    H5TR_t *tr = NULL;
    hid_t ret_value;

    FUNC_ENTER_API(FAIL)

    /* get the file object */
    if(NULL == (file = (void *)H5I_object_verify(file_id, H5I_FILE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a file ID")
    /* get the plugin pointer */
    if (NULL == (vol_plugin = (H5VL_t *)H5I_get_aux(file_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information")
    /* get the Read Context object */
    if(NULL == (rc = (H5RC_t *)H5I_object_verify(rc_id, H5I_RC)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a read context ID")

    /* create a new transaction object */
    if(NULL == (tr = H5TR_create(file, rc, trans_num)))
	HGOTO_ERROR(H5E_SYM, H5E_CANTCREATE, FAIL, "unable to create transaction object")

    /* Get an atom for the event queue with the VOL information as the auxilary struct*/
    if((ret_value = H5I_register2(H5I_TR, tr, vol_plugin, TRUE)) < 0)
	HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to atomize transaction handle")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5TRcreate() */


/*-------------------------------------------------------------------------
 * Function:	H5TR_create
 *
 * Purpose:	Private version for H5TRcreate.
 *
 * Return:	Success:	Transaction struct.
 *		Failure:	NULL
 *
 * Programmer:	Mohamad Chaarawi
 *		August 2013
 *
 *-------------------------------------------------------------------------
 */
H5TR_t *
H5TR_create(void *file, H5RC_t *rc, uint64_t trans_num)
{
    void *file = NULL;
    H5VL_t *vol_plugin = NULL;
    H5TR_t *tr = NULL;
    H5TR_t *ret_value = NULL;          /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* allocate transaction struct */
    if(NULL == (tr = H5FL_CALLOC(H5TR_t)))
	HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, NULL, "can't allocate top transaction structure")

    tr->file = file;
    tr->c_version = rc->c_version;
    tr->trans_num = trans_num;

    /* set return value */
    ret_value = tr;

done:
    if(!ret_value && tr)
	tr = H5FL_FREE(H5TR_t, tr);

    FUNC_LEAVE_API(ret_value)
} /* end H5TR_create() */


/*-------------------------------------------------------------------------
 * Function:	H5TRstart
 *
 * Purpose:     Starts a transaction that has been created with H5TRcreate.
 *
 * Return:	Success:	Non-Negative.
 *		Failure:	Negative
 *
 * Programmer:	Mohamad Chaarawi
 *		August 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5TRstart(hid_t tr_id, hid_t trspl_id, hid_t eq_id)
{
    H5TR_t *tr = NULL;
    H5_priv_request_t  *request = NULL; /* private request struct inserted in event queue */
    void **req = NULL; /* pointer to plugin generate requests (Stays NULL if plugin does not support async */
    herr_t ret_value;

    FUNC_ENTER_API(FAIL)
    H5TRACE3("e", "iii", tr_id, trspl_id, eq_id);

    /* get the TR object */
    if(NULL == (tr = (H5TR_t *)H5I_object_verify(tr_id, H5I_TR)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a Transaction ID")

    /* Get correct property list */
    if(H5P_DEFAULT == trspl_id)
        trspl_id = H5P_TR_START_DEFAULT;
    else
        if(TRUE != H5P_isa_class(trspl_id, H5P_TR_START))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not transaction start property list")

    if(eq_id != H5_EVENT_QUEUE_NULL) {
        /* create the private request */
        if(NULL == (request = (H5_priv_request_t *)H5MM_calloc(sizeof(H5_priv_request_t))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
        request->req = NULL;
        req = &request->req;
        request->next = NULL;
        request->vol_plugin = vol_plugin;
    }

    if(H5VL_iod_tr_start(tr, trspl_id, req) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "failed to request a transaction start");

    if(request && *req) {
        if(H5EQinsert(eq_id, request) < 0)
            HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "failed to insert request in event queue");
    }

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5TRstart()*/


/*-------------------------------------------------------------------------
 * Function:	H5TRfinish
 *
 * Purpose:     Finishes/Commits a transaction.
 *
 * Return:	Success:	Non-Negative.
 *		Failure:	Negative
 *
 * Programmer:	Mohamad Chaarawi
 *		August 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5TRfinish(hid_t trans_id, hid_t trfpl_id, hid_t eq_id)
{
    H5TR_t *tr = NULL;
    H5_priv_request_t  *request = NULL; /* private request struct inserted in event queue */
    void **req = NULL; /* pointer to plugin generate requests (Stays NULL if plugin does not support async */
    herr_t ret_value;

    FUNC_ENTER_API(FAIL)
    H5TRACE3("e", "iii", trans_id, trfpl_id, eq_id);

    /* get the TR object */
    if(NULL == (tr = (H5TR_t *)H5I_object_verify(tr_id, H5I_TR)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a Transaction ID")

    /* Get correct property list */
    if(H5P_DEFAULT == trfpl_id)
        trfpl_id = H5P_TR_FINISH_DEFAULT;
    else
        if(TRUE != H5P_isa_class(trfpl_id, H5P_TR_FINISH))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not transaction finish property list")

    if(eq_id != H5_EVENT_QUEUE_NULL) {
        /* create the private request */
        if(NULL == (request = (H5_priv_request_t *)H5MM_calloc(sizeof(H5_priv_request_t))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
        request->req = NULL;
        req = &request->req;
        request->next = NULL;
        request->vol_plugin = vol_plugin;
    }

    if(H5VL_iod_tr_finish(tr, trfpl_id, req) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "failed to request a transaction finish");

    if(request && *req) {
        if(H5EQinsert(eq_id, request) < 0)
            HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "failed to insert request in event queue");
    }

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5TRfinish()*/


/*-------------------------------------------------------------------------
 * Function:	H5TRfinish
 *
 * Purpose:     Set a depedency between two transactions. Transaction
 *              associated with trans_id is declaring a dependency on transaction
 *              numbered trans_num.  trans_num must be < the transaction number
 *              trans_id is associated with.
 *
 * Return:	Success:	Non-Negative.
 *		Failure:	Negative
 *
 * Programmer:	Mohamad Chaarawi
 *		August 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5TRset_dependency(hid_t trans_id, uint64_t trans_num, hid_t eq_id)
{
    H5TR_t *tr = NULL;
    H5_priv_request_t  *request = NULL; /* private request struct inserted in event queue */
    void **req = NULL; /* pointer to plugin generate requests (Stays NULL if plugin does not support async */
    herr_t ret_value;

    FUNC_ENTER_API(FAIL)

    /* get the TR object */
    if(NULL == (tr = (H5TR_t *)H5I_object_verify(tr_id, H5I_TR)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a Transaction ID")

    if(tr->trans_num <= trans_num)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "The dependent transaction must be higher than the one it depends on")

    if(eq_id != H5_EVENT_QUEUE_NULL) {
        /* create the private request */
        if(NULL == (request = (H5_priv_request_t *)H5MM_calloc(sizeof(H5_priv_request_t))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
        request->req = NULL;
        req = &request->req;
        request->next = NULL;
        request->vol_plugin = vol_plugin;
    }

    if(H5VL_iod_tr_set_dependency(tr, trans_num, req) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "failed to request a transaction set_dependency");

    if(request && *req) {
        if(H5EQinsert(eq_id, request) < 0)
            HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "failed to insert request in event queue");
    }

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5TRset_dependency()*/


/*-------------------------------------------------------------------------
 * Function:	H5TRabort
 *
 * Purpose:     Aborts a transaction
 *
 * Return:	Success:	Non-Negative.
 *		Failure:	Negative
 *
 * Programmer:	Mohamad Chaarawi
 *		August 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5TRabort(hid_t trans_id, hid_t eq_id)
{
    H5TR_t *tr = NULL;
    H5_priv_request_t  *request = NULL; /* private request struct inserted in event queue */
    void **req = NULL; /* pointer to plugin generate requests (Stays NULL if plugin does not support async */
    herr_t ret_value;

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "ii", trans_id, eq_id);

    /* get the TR object */
    if(NULL == (tr = (H5TR_t *)H5I_object_verify(tr_id, H5I_TR)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a Transaction ID")

    if(eq_id != H5_EVENT_QUEUE_NULL) {
        /* create the private request */
        if(NULL == (request = (H5_priv_request_t *)H5MM_calloc(sizeof(H5_priv_request_t))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
        request->req = NULL;
        req = &request->req;
        request->next = NULL;
        request->vol_plugin = vol_plugin;
    }

    if(H5VL_iod_tr_abort(tr, req) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "failed to request a transaction abort");

    if(request && *req) {
        if(H5EQinsert(eq_id, request) < 0)
            HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "failed to insert request in event queue");
    }

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5TRabort()*/


/*-------------------------------------------------------------------------
 * Function:	H5TRclose
 *
 * Purpose:	Closes the specified transaction ID.  The ID will no longer be
 *		valid for accessing the transaction.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Mohamad Chaarawi
 *		August 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5TRclose(hid_t tr_id)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE1("e", "i", tr_id);

    /* Check args */
    if(NULL == H5I_object_verify(tr_id,H5I_TR))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not an transaction ID")

    if(H5I_dec_app_ref(tr_id) < 0)
    	HGOTO_ERROR(H5E_SYM, H5E_CANTRELEASE, FAIL, "unable to close transaction")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5TRclose() */


/*-------------------------------------------------------------------------
 * Function:	H5TR_close
 *
 * Purpose:	Closes the specified transaction ID.  The ID will no longer be
 *		valid for accessing the transaction.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Mohamad Chaarawi
 *		August 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5TR_close(H5TR_t *tr)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API(FAIL)

    tr = H5FL_FREE(H5TR_t, tr);

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5TR_close() */

