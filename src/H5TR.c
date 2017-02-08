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

#include "H5TRmodule.h"          /* This source code file is part of the H5TR module */


/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5TRprivate.h"        /* Transactions                         */
#include "H5VLprivate.h"	/* VOL plugins				*/

#ifdef H5_HAVE_EFF

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

/* Declare a free list to manage the H5TR_t struct */
H5FL_DEFINE(H5TR_t);

/* Dataspace ID class */
static const H5I_class_t H5I_TR_CLS[1] = {{
    H5I_TR,        		/* ID class value */
    0,				/* Class flags */
    2,				/* # of reserved IDs for class */
    (H5I_free_t)H5TR_close    	/* Callback routine for closing objects of this class */
}};



/*-------------------------------------------------------------------------
 * Function:    H5TR_init
 *
 * Purpose:     Initialize the interface from some other package.
 *
 * Return:      Success:        non-negative
 *              Failure:        negative
 *
 * Programmer:  Mohamad Chaarawi
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
   H5TR__init_package -- Initialize interface-specific information
USAGE
    herr_t H5TR__init_package()

RETURNS
    Non-negative on success/Negative on failure
DESCRIPTION
    Initializes any interface-specific data or routines.

--------------------------------------------------------------------------*/
herr_t
H5TR__init_package(void)
{
    herr_t ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Initialize the atom group for the TR IDs */
    if(H5I_register_type(H5I_TR_CLS) < 0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTINIT, FAIL, "unable to initialize interface")

    H5_PKG_INIT_VAR = TRUE;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5TR__init_package() */


/*--------------------------------------------------------------------------
 NAME
    H5TR_term_package
 PURPOSE
    Terminate various H5TR objects
 USAGE
    void H5TR_term_package()
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
H5TR_term_package(void)
{
    int	n = 0;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    if(H5_PKG_INIT_VAR) {
        if(H5I_nmembers(H5I_TR) > 0) {
            (void)H5I_clear_type(H5I_TR, FALSE, FALSE);
            n++; /*H5I*/
        } /* end if */
        else {
            n += (H5I_dec_type_ref(H5I_TR) > 0);

            /* Mark closed */
            if(0 == n)
                H5_PKG_INIT_VAR = FALSE;
        } /* end else */
    } /* end if */

    FUNC_LEAVE_NOAPI(n)
} /* end H5TR_term_package() */


/*-------------------------------------------------------------------------
 * Function:    H5TRcreate
 *
 * Purpose:     Wraps an hid_t around a transaction number, a file ID, 
 *              and a read context ID on that file that operations using 
 *              the created transaction will read from.
 *
 * Return:      Success:        The ID for a new transaction.
 *              Failure:        FAIL
 *
 * Programmer:  Neil Fortner
 *              November 2016
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5TRcreate(hid_t obj_id, uint64_t trans_num)
{
    H5VL_object_t *obj = NULL;
    H5TR_t *tr = NULL;
    hid_t ret_value;

    FUNC_ENTER_API(FAIL)

    /* get the location object */
    if(NULL == (obj = H5VL_get_object(obj_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid location identifier")

    /* create a new transaction object */
    if(NULL == (tr = H5TR_create(obj->vol_obj, obj->vol_info->vol_cls, trans_num)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTCREATE, FAIL, "unable to create transaction object")

    /* Get an atom for the TR */
    if((ret_value = H5I_register(H5I_TR, tr, TRUE)) < 0)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to atomize transaction handle")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5TRcreate() */


/*-------------------------------------------------------------------------
 * Function:    H5TR_create
 *
 * Purpose:     Private version for H5TRcreate.
 *
 * Return:      Success:        Transaction struct.
 *              Failure:        NULL
 *
 * Programmer:  Neil Fortner
 *              November 2016
 *
 *-------------------------------------------------------------------------
 */
H5TR_t *
H5TR_create(void *_obj, const H5VL_class_t *vol_cls, daos_epoch_t epoch)
{
    H5VL_daosm_obj_t *obj = (H5VL_daosm_obj_t *)_obj;
    H5TR_t *tr = NULL;
    H5TR_t *ret_value = NULL;          /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* allocate transaction struct */
    if(NULL == (tr = H5FL_CALLOC(H5TR_t)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, NULL, "can't allocate top transaction structure")

    tr->file = obj->item.file;
    tr->epoch = epoch;
    tr->vol_cls = vol_cls;

    /* set return value */
    ret_value = tr;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5TR_create() */

herr_t
H5TRget_trans_num(hid_t trans_id, uint64_t *trans_num)
{
    H5TR_t *tr = NULL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "i*Il", trans_id, trans_num);

    /* get the TR object */
    if(NULL == (tr = (H5TR_t *)H5I_object_verify(trans_id, H5I_TR)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a transaction ID")

    *trans_num = (uint64_t)tr->epoch;

done:
    FUNC_LEAVE_API(ret_value)
} /* H5TRget_trans_num */


/*-------------------------------------------------------------------------
 * Function:    H5TRcommit
 *
 * Purpose:     Finishes/Commits a transaction. If rcxt_id is not NULL,
 *              create a read context from the finished transaction number.
 *              If the finish fails, the user is still reponsible to call
 *              H5RCclose() on the rcxt_id and H5TRclose() on the tr_id 
 *              to free resources.
 *
 * Return:      Success:        Non-Negative.
 *              Failure:        Negative
 *
 * Programmer:  Neil Fortner
 *              November 2016
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5TRcommit(hid_t tr_id)
{
    H5TR_t *tr = NULL;
    int ret;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)
    H5TRACE1("e", "i", tr_id);

    /* get the TR object */
    if(NULL == (tr = (H5TR_t *)H5I_object_verify(tr_id, H5I_TR)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a Transaction ID")

    /* Commit the transaction */
    if(0 != (ret = daos_epoch_commit(tr->file->coh, tr->epoch, NULL /*state*/, NULL /*event*/)))
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "failed to commit epoch: %d", ret)

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5TRcommit()*/


/*-------------------------------------------------------------------------
 * Function:    H5TRclose
 *
 * Purpose:     Closes the specified transaction ID.  The ID will no longer be
 *              valid for accessing the transaction.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              August 2013
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
    if(NULL == H5I_object_verify(tr_id, H5I_TR))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not an transaction ID")

    if(H5I_dec_app_ref(tr_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTRELEASE, FAIL, "unable to close transaction")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5TRclose() */


/*-------------------------------------------------------------------------
 * Function:    H5TR_close
 *
 * Purpose:     Frees the transaction struct.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Neil Fortner
 *              November 2016
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5TR_close(H5TR_t *tr)
{

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /*TODO: Keep track of lowest transaction number (LRE), slip daos epoch as it
     * moves forward. */
    tr = H5FL_FREE(H5TR_t, tr);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5TR_close() */

#endif /* H5_HAVE_EFF */

