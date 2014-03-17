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
 * Created:		H5Pxcpl.c
 *			February 2014
 *
 * Purpose:		Index creation property list class routines
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
#include "H5private.h"      /* Generic Functions    */
#include "H5Eprivate.h"     /* Error handling       */
#include "H5Iprivate.h"     /* IDs                  */
#include "H5Sprivate.h"     /* Dataspaces           */
#include "H5FFprivate.h"    /* Fast Forward routines*/
#include "H5Xprivate.h"     /* Index                */
#include "H5Ppkg.h"         /* Property lists       */

#ifdef H5_HAVE_INDEXING

/****************/
/* Local Macros */
/****************/

/* ========  Index creation properties ======== */

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
static herr_t H5P__xcrt_reg_prop(H5P_genclass_t *pclass);

/*********************/
/* Package Variables */
/*********************/

/* Index creation property list class library initialization object */
const H5P_libclass_t H5P_CLS_XCRT[1] = {{
    "index create",             /* Class name for debugging     */
    H5P_TYPE_INDEX_CREATE,      /* Class type                   */
    &H5P_CLS_OBJECT_CREATE_g,   /* Parent class ID              */
    &H5P_CLS_INDEX_CREATE_g,    /* Pointer to class ID          */
    &H5P_LST_INDEX_CREATE_g,    /* Pointer to default property list ID */
    H5P__xcrt_reg_prop,         /* Default property registration routine */
    NULL,                       /* Class creation callback      */
    NULL,                       /* Class creation callback info */
    NULL,                       /* Class copy callback          */
    NULL,                       /* Class copy callback info     */
    NULL,                       /* Class close callback         */
    NULL                        /* Class close callback info    */
}};

/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/


/*-------------------------------------------------------------------------
 * Function:    H5P__xcrt_reg_prop
 *
 * Purpose:     Register the index creation property list class's
 *              properties
 *
 * Return:      Non-negative on success/Negative on failure
 *-------------------------------------------------------------------------
 */
static herr_t
H5P__xcrt_reg_prop(H5P_genclass_t *pclass)
{
    hid_t trans_id = FAIL;
    hid_t context_id = FAIL;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Register the transaction ID property*/
    if(H5P_register_real(pclass, H5VL_TRANS_ID, sizeof(hid_t), &trans_id,
                         NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class")

    /* Register the context ID property*/
    if(H5P_register_real(pclass, H5VL_CONTEXT_ID, sizeof(hid_t), &context_id,
                         NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class")
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5P__xcrt_reg_prop() */

#endif /* H5_HAVE_INDEXING */
