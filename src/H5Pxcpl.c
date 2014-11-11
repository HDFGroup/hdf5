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
#include "H5Xprivate.h"     /* Index                */
#include "H5Ppkg.h"         /* Property lists       */


/****************/
/* Local Macros */
/****************/

/* ========  Index creation properties ======== */
/* Definitions for index plugin value */
#define H5X_CRT_READ_ON_CREATE_SIZE   sizeof(hbool_t)
#define H5X_CRT_READ_ON_CREATE_DEF    TRUE
#define H5X_CRT_READ_ON_CREATE_ENC    H5P__encode_hbool_t
#define H5X_CRT_READ_ON_CREATE_DEC    H5P__decode_hbool_t


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
    &H5P_CLS_ROOT_g,            /* Parent class                 */
    &H5P_CLS_INDEX_CREATE_g,    /* Pointer to class             */
    &H5P_CLS_INDEX_CREATE_ID_g, /* Pointer to class ID          */
    &H5P_LST_INDEX_CREATE_ID_g, /* Pointer to default property list ID */
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

/* Property value defaults */
static const hbool_t H5X_def_read_on_create_g = H5X_CRT_READ_ON_CREATE_DEF; /* Default read on create value */



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
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Register the "read data on create" property */
    if(H5P_register_real(pclass, H5X_CRT_READ_ON_CREATE_NAME,
            H5X_CRT_READ_ON_CREATE_SIZE, &H5X_def_read_on_create_g,
            NULL, NULL, NULL, H5X_CRT_READ_ON_CREATE_ENC,
            H5X_CRT_READ_ON_CREATE_DEC, NULL, NULL, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5P__xcrt_reg_prop() */

/*-------------------------------------------------------------------------
 * Function:    H5Pset_index_read_on_create
 *
 * Purpose: Set index to read data when created.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_index_read_on_create(hid_t plist_id, hbool_t value)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    hbool_t read_on_create;     /* Value property to modify */
    herr_t ret_value = SUCCEED; /* return value          */

    FUNC_ENTER_API(FAIL)

    /* Get the property list structure */
    if(NULL == (plist = H5P_object_verify(plist_id, H5P_INDEX_CREATE)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    read_on_create = value;

    /* Set values */
    if(H5P_set(plist, H5X_CRT_READ_ON_CREATE_NAME, &read_on_create) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set property value")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_index_read_on_create() */

/*-------------------------------------------------------------------------
 * Function:    H5Pget_index_read_on_create
 *
 * Purpose: Get boolean to read data on index creation.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_index_read_on_create(hid_t plist_id, hbool_t *value/*out*/)
{
    herr_t ret_value = SUCCEED; /* return value          */

    FUNC_ENTER_API(FAIL)

    /* Set values */
    if(value) {
        H5P_genplist_t *plist;  /* Property list pointer */
        hbool_t read_on_create;     /* Value property to query */

        /* Get the property list structure */
        if(NULL == (plist = H5P_object_verify(plist_id, H5P_INDEX_CREATE)))
            HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

        /* Retrieve fill value settings */
        if(H5P_get(plist, H5X_CRT_READ_ON_CREATE_NAME, &read_on_create) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get property value")

        /* Set user's value */
        *value = read_on_create;
    } /* end if */

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pget_index_read_on_create() */
