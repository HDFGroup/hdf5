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
 * Created:		H5Ptrspl.c
 *			August 2013
 *			Mohamad Chaarawi <chaarawi@hdfgroup.org>
 *
 * Purpose:		Transaction Start property list class routines
 *
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/
#define H5P_PACKAGE		/*suppress error about including H5Ppkg	  */


/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions */
#include "H5Eprivate.h"		/* Error handling */
#include "H5Iprivate.h"		/* IDs */
#include "H5Ppkg.h"		/* Property lists */
#include "H5TRprivate.h"	/* Read Context */

/****************/
/* Local Macros */
/****************/

/* ========= Read Context properties ============ */
/* Definitions for Transaction being started  */
#define H5TR_START_NUM_PEERS_SIZE      sizeof(unsigned)
#define H5TR_START_NUM_PEERS_DEF       0
#define H5TR_START_NUM_PEERS_ENC       H5P__encode_unsigned
#define H5TR_START_NUM_PEERS_DEC       H5P__decode_unsigned

/******************/
/* Local Typedefs */
/******************/


/********************/
/* Package Typedefs */
/********************/


/********************/
/* Local Prototypes */
/********************/

/* Property class callbacks */
static herr_t H5P__trscc_reg_prop(H5P_genclass_t *pclass);


/*********************/
/* Package Variables */
/*********************/

/* Transaction start property list class library initialization object */
const H5P_libclass_t H5P_CLS_TRSCC[1] = {{
    "transaction start",	        /* Class name for debugging     */
    H5P_TYPE_TRANSACTION_START,         /* Class type                   */
    &H5P_CLS_ROOT_g,            	/* Parent class ID              */
    &H5P_CLS_TRANSACTION_START_g,	/* Pointer to class ID          */
    &H5P_LST_TRANSACTION_START_g,	/* Pointer to default property list ID */
    H5P__trscc_reg_prop,		/* Default property registration routine */
    NULL,		                /* Class creation callback      */
    NULL,		                /* Class creation callback info */
    NULL,		                /* Class copy callback          */
    NULL,		                /* Class copy callback info     */
    NULL,		                /* Class close callback         */
    NULL 		                /* Class close callback info    */
}};


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/



/*-------------------------------------------------------------------------
 * Function:    H5P__trscc_reg_prop
 *
 * Purpose:     Register the transaction start property list class's
 *              properties
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              August 2013
 *-------------------------------------------------------------------------
 */
static herr_t
H5P__trscc_reg_prop(H5P_genclass_t *pclass)
{
    unsigned num_peers = H5TR_START_NUM_PEERS_DEF;
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_STATIC

    /* Register the size of raw data chunk cache (elements) */
    if(H5P_register_real(pclass, H5TR_START_NUM_PEERS_NAME, H5TR_START_NUM_PEERS_SIZE, &num_peers, 
                         NULL, NULL, NULL, H5TR_START_NUM_PEERS_ENC, H5TR_START_NUM_PEERS_DEC, 
                         NULL, NULL, NULL, NULL) < 0)
         HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5P__trscc_reg_prop() */
