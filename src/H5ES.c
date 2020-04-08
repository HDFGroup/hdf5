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
 *			Quincey Koziol <koziol@lbl.gov>
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

#include "H5ESmodule.h"         /* This source code file is part of the H5ES module */


/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5ESpkg.h"            /* Event Sets                           */
#include "H5FLprivate.h"        /* Free Lists                           */
#include "H5Iprivate.h"         /* IDs                                  */


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
    H5ES_t *es;                         /* Pointer to event set object */
    hid_t ret_value = H5I_INVALID_HID;  /* Return value */

    FUNC_ENTER_API(H5I_INVALID_HID)
    H5TRACE0("i","");

    /* Create the new event set object */
    if(NULL == (es = H5ES__create()))
        HGOTO_ERROR(H5E_EVENTSET, H5E_CANTCREATE, H5I_INVALID_HID, "can't create event set")

    /* Register the new event set to get an ID for it */
    if((ret_value = H5I_register(H5I_EVENTSET, es, TRUE)) < 0)
        HGOTO_ERROR(H5E_EVENTSET, H5E_CANTREGISTER, H5I_INVALID_HID, "can't register event set")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5EScreate() */


/*-------------------------------------------------------------------------
 * Function:    H5ESclose
 *
 * Purpose:     Closes an event set.
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
    herr_t       ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE1("e", "i", es_id);

    /* Check arguments */
    if(H5I_EVENTSET != H5I_get_type(es_id))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not an event set")

    /*
     * Decrement the counter on the object.  It will be freed if the count
     * reaches zero.
     */
    if(H5I_dec_app_ref(es_id) < 0)
        HGOTO_ERROR(H5E_EVENTSET, H5E_CANTDEC, FAIL, "unable to decrement ref count on event set")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5ESclose() */

