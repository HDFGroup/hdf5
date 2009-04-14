/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
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

/*-------------------------------------------------------------------------
 *
 * Created:		H5HLerror.c
 *			Apr 14 2009
 *			Quincey Koziol <koziol@hdfgroup.org>
 *
 * Purpose:		Internal error routines for High-Level interfaces.
 *
 *-------------------------------------------------------------------------
 */

/**********************/
/* Module Declaration */
/**********************/


/***********************/
/* Other Packages Used */
/***********************/


/***********/
/* Headers */
/***********/
#include "H5HLprivate2.h"       /* High-level library internal header file */


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

/* HL-HDF5 API Entered variable */
hbool_t H5_api_entered_g = FALSE;


/*******************/
/* Local Variables */
/*******************/



/*-------------------------------------------------------------------------
 * Function:	H5HLE_dump_api_stack
 *
 * Purpose:	Private function to dump the error stack during an error in
 *              an API function if a callback function is defined for the
 *              current error stack.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, April 14, 2009
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HLE_dump_api_stack(hbool_t is_api)
{
    herr_t ret_value = SUCCEED;   /* Return value */

    /* Only dump the error stack during an API call */
    if(is_api) {
        hid_t estack_id;        /* Error stack ID */

        estack_id = H5Eget_current_stack();
        assert(estack_id > 0);

        H5Eprint2(estack_id, stderr);

        H5Eclose_stack(estack_id);
    } /* end if */

    return SUCCEED;
} /* end H5HLE_dump_api_stack() */

