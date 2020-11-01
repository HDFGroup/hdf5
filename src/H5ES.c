/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*-------------------------------------------------------------------------
 *
 * Created:		H5ES.c
 *			Apr  6 2020
 *			Quincey Koziol
 *
 * Purpose:		Implements an "event set" for managing asynchronous
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
#include "H5private.h"   /* Generic Functions			*/
#include "H5Eprivate.h"  /* Error handling		  	*/
#include "H5ESpkg.h"     /* Event Sets                           */
#include "H5FLprivate.h" /* Free Lists                           */
#include "H5Iprivate.h"  /* IDs                                  */

/****************/
/* Local Macros */
/****************/

/******************/
/* Local Typedefs */
/******************/

/********************/
/* Package Typedefs */
/********************/

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

/*-------------------------------------------------------------------------
 * Function:    H5EScreate
 *
 * Purpose:     Creates an event set.
 *
 * Return:      Success:    An ID for the event set
 *              Failure:    H5I_INVALID_HID
 *
 * Programmer:	Quincey Koziol
 *              Wednesday, April 8, 2020
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5EScreate(void)
{
    H5ES_t *es;                          /* Pointer to event set object */
    hid_t   ret_value = H5I_INVALID_HID; /* Return value */

    FUNC_ENTER_API(H5I_INVALID_HID)
    H5TRACE0("i", "");

    /* Create the new event set object */
    if (NULL == (es = H5ES__create()))
        HGOTO_ERROR(H5E_EVENTSET, H5E_CANTCREATE, H5I_INVALID_HID, "can't create event set")

    /* Register the new event set to get an ID for it */
    if ((ret_value = H5I_register(H5I_EVENTSET, es, TRUE)) < 0)
        HGOTO_ERROR(H5E_EVENTSET, H5E_CANTREGISTER, H5I_INVALID_HID, "can't register event set")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5EScreate() */

/*-------------------------------------------------------------------------
 * Function:    H5ESget_count
 *
 * Purpose:     Retrieve the # of events in an event set
 *
 * Return:      SUCCEED / FAIL
 *
 * Programmer:	Quincey Koziol
 *              Wednesday, April 8, 2020
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5ESget_count(hid_t es_id, size_t *count /*out*/)
{
    H5ES_t *es;                  /* Event set */
    herr_t  ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "ix", es_id, count);

    /* Check arguments */
    if (NULL == (es = H5I_object_verify(es_id, H5I_EVENTSET)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid event set identifier")

    /* Retrieve the count, if non-NULL */
    if (count)
        *count = es->act_count;

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5ESget_count() */

/*-------------------------------------------------------------------------
 * Function:    H5EStest
 *
 * Purpose:     Test if all operations in event set have completed
 *
 * Return:      SUCCEED / FAIL
 *
 * Programmer:	Quincey Koziol
 *              Monday, July 13, 2020
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5EStest(hid_t es_id, H5ES_status_t *status)
{
    H5ES_t *es;                  /* Event set */
    herr_t  ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "i*Es", es_id, status);

    /* Check arguments */
    if (NULL == (es = H5I_object_verify(es_id, H5I_EVENTSET)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid event set identifier")
    if (NULL == status)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "NULL status pointer")

    /* Check status of events in event set */
    if (H5ES__test(es, status) < 0)
        HGOTO_ERROR(H5E_EVENTSET, H5E_CANTGET, FAIL, "can't check status of operations")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5EStest() */

/*-------------------------------------------------------------------------
 * Function:    H5ESwait
 *
 * Purpose:     Wait (with timeout) for all operations in event set to complete
 *
 * Note:        Timeout value is in ns, and is per-operation, not for H5ESwait
 *              itself.
 *
 * Note:        This call will stop waiting on operations and will return after
 *              the first operation that does not succeed (i.e. FAIL or CANCEL
 *              for its status) or is still in progress when the timeout is
 *              reached (i.e. IN_PROGRESS for its status).
 *
 * Return:      SUCCEED / FAIL
 *
 * Programmer:	Quincey Koziol
 *              Monday, July 13, 2020
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5ESwait(hid_t es_id, uint64_t timeout, H5ES_status_t *status)
{
    H5ES_t *es;                  /* Event set */
    herr_t  ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE3("e", "iUL*Es", es_id, timeout, status);

    /* Check arguments */
    if (NULL == (es = H5I_object_verify(es_id, H5I_EVENTSET)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid event set identifier")
    if (NULL == status)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "NULL status pointer")

    /* Wait for operations */
    if (H5ES__wait(es, timeout, status, TRUE) < 0)
        HGOTO_ERROR(H5E_EVENTSET, H5E_CANTWAIT, FAIL, "can't wait on operations")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5ESwait() */

/*-------------------------------------------------------------------------
 * Function:    H5ESget_err_status
 *
 * Purpose:     Check if event set has failed operations
 *
 * Return:      SUCCEED / FAIL
 *
 * Programmer:	Quincey Koziol
 *              Thursday, October 15, 2020
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5ESget_err_status(hid_t es_id, hbool_t *err_status /*out*/)
{
    H5ES_t *es;                  /* Event set */
    herr_t  ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "ix", es_id, err_status);

    /* Check arguments */
    if (NULL == (es = H5I_object_verify(es_id, H5I_EVENTSET)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid event set identifier")

    /* Retrieve the error flag, if non-NULL */
    if (err_status)
        *err_status = es->err_occurred;

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5ESget_err_status() */

/*-------------------------------------------------------------------------
 * Function:    H5ESget_err_count
 *
 * Purpose:     Retrieve # of failed operations
 *
 * Note:        Does not wait for active operations to complete, so count may
 *              not include all failures.
 *
 * Return:      SUCCEED / FAIL
 *
 * Programmer:	Quincey Koziol
 *              Thursday, October 15, 2020
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5ESget_err_count(hid_t es_id, size_t *num_errs /*out*/)
{
    H5ES_t *es;                  /* Event set */
    herr_t  ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "ix", es_id, num_errs);

    /* Check arguments */
    if (NULL == (es = H5I_object_verify(es_id, H5I_EVENTSET)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid event set identifier")

    /* Retrieve the error flag, if non-NULL */
    if (num_errs) {
        if (es->err_occurred)
            *num_errs = es->err_count;
        else
            *num_errs = 0;
    } /* end if */

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5ESget_err_count() */

/*-------------------------------------------------------------------------
 * Function:    H5ESclose
 *
 * Purpose:     Closes an event set.
 *
 * Note:        Fails if active operations are present.
 *
 * Return:      SUCCEED / FAIL
 *
 * Programmer:	Quincey Koziol
 *              Wednesday, April 8, 2020
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5ESclose(hid_t es_id)
{
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE1("e", "i", es_id);

    /* Check arguments */
    if (H5I_EVENTSET != H5I_get_type(es_id))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not an event set")

    /*
     * Decrement the counter on the object.  It will be freed if the count
     * reaches zero.
     */
    if (H5I_dec_app_ref(es_id) < 0)
        HGOTO_ERROR(H5E_EVENTSET, H5E_CANTDEC, FAIL, "unable to decrement ref count on event set")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5ESclose() */
