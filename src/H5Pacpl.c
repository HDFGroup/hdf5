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
 * Created:		H5Pacpl.c
 *			January  2 2006
 *			James Laird <jlaird@ncsa.uiuc.edu>
 *
 * Purpose:		Attribute creation property list class routines
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
#include "H5private.h"		/* Generic Functions			*/
#include "H5Aprivate.h"		/* Attributes				*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5Ppkg.h"		/* Property lists		  	*/


/****************/
/* Local Macros */
/****************/

/* Definitions for UDATA */
#define H5A_CRT_LOCATION_SIZE   sizeof(void *)
#define H5A_CRT_LOCATION_DEF    NULL

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
static herr_t H5P_acrt_reg_prop(H5P_genclass_t *pclass);

/*********************/
/* Package Variables */
/*********************/

/* Attribute creation property list class library initialization object */
const H5P_libclass_t H5P_CLS_ACRT[1] = {{
    "attribute create",		/* Class name for debugging     */
    &H5P_CLS_STRING_CREATE_g,	/* Parent class ID              */
    &H5P_CLS_ATTRIBUTE_CREATE_g, /* Pointer to class ID          */
    &H5P_LST_ATTRIBUTE_CREATE_g, /* Pointer to default property list ID */
    H5P_acrt_reg_prop,		/* Default property registration routine */
    NULL,		        /* Class creation callback      */
    NULL,		        /* Class creation callback info */
    NULL,			/* Class copy callback          */
    NULL,		        /* Class copy callback info     */
    NULL,			/* Class close callback         */
    NULL 		        /* Class close callback info    */
}};


/*****************************/
/* Library Private Variables */
/*****************************/



/*-------------------------------------------------------------------------
 * Function:    H5P_acrt_reg_prop
 *
 * Purpose:     Register the attribute creation property list class's properties
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              April 5, 2012
 *-------------------------------------------------------------------------
 */
static herr_t
H5P_acrt_reg_prop(H5P_genclass_t *pclass)
{
    hid_t type_id = FAIL;
    hid_t space_id = FAIL;
    void *location = H5A_CRT_LOCATION_DEF;
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Register the type ID property*/
    if(H5P_register_real(pclass, H5A_CRT_TYPE_ID_NAME, sizeof(hid_t), &type_id, 
                         NULL, NULL, NULL, NULL, NULL, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class")

    /* Register the space ID property */
    if(H5P_register_real(pclass, H5A_CRT_SPACE_ID_NAME, sizeof(hid_t), &space_id, 
                         NULL, NULL, NULL, NULL, NULL, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class")

    /* Register the lcpl ID property */
    if(H5P_register_real(pclass, H5A_CRT_LOCATION_NAME, H5A_CRT_LOCATION_SIZE, &location, 
                         NULL, NULL, NULL, NULL, NULL, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5P_acrt_reg_prop() */
