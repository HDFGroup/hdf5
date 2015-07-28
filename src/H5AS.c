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
 * Purpose: Analysis shipping for local enumeration of data on remote storage.
 */

/****************/
/* Module Setup */
/****************/

/* Interface initialization */
//#define H5_INTERFACE_INIT_FUNC  H5AS_init_interface

/***********/
/* Headers */
/***********/
#include "H5private.h"      /* Generic Functions */
#include "H5ASprivate.h"    /* Analysis shipping */
#include "H5Eprivate.h"     /* Error handling */
#include "H5Iprivate.h"     /* IDs */
#include "H5MMprivate.h"    /* Memory management */
#include "H5Pprivate.h"
#include "H5ESprivate.h"    /* Event Stacks */

#include "H5VLiod_client.h"

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


/*-------------------------------------------------------------------------
 * Function:    H5ASinvoke
 *
 * Purpose: Invoke the query on file_name/obj_name, run split_script
 * on the query result, combine the results and run combine_script.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5ASinvoke(const char *file_name, hid_t query_id, const char *split_script, 
           const char *combine_script, const char *integrate_script, hid_t estack_id)
{
    herr_t ret_value;

    FUNC_ENTER_API(FAIL)
    H5TRACE6("e", "*si*s*s*si", file_name, query_id, split_script, combine_script,
             integrate_script, estack_id);

    /* Check argument and retrieve object */
    if(file_name == NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "NULL file name")

    /* It is allowed to have split_script and combine_script to be NULL */

    /* H5AS_invoke */
    if((ret_value = H5AS_invoke(file_name, query_id, split_script,
                                combine_script, integrate_script, estack_id)) < 0)
        HGOTO_ERROR(H5E_QUERY, H5E_CANTENCODE, FAIL, "can't start analysis"
                "shipping execution")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5ASinvoke() */

/*-------------------------------------------------------------------------
 * Function:    H5AS_invoke
 *
 * Purpose: Private routine for H5ASinvoke.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5AS_invoke(const char *file_name, hid_t query_id, const char *split_script, 
            const char *combine_script, const char *integrate_script, hid_t estack_id)
{
    H5_priv_request_t  *request = NULL; /* private request struct inserted in event queue */
    void    **req = NULL; /* pointer to plugin generate requests (Stays NULL if plugin does not support async */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(file_name);

    if(estack_id != H5_EVENT_STACK_NULL) {
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "works only synchronously for now");
    }

    /* Get the data through the IOD VOL */
    if((ret_value = H5VL_iod_analysis_invoke(file_name, query_id, split_script, 
                                             combine_script, integrate_script, req)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL,
                "can't start iod analysis shipping execution")

done:

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5AS_invoke */
