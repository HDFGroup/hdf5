/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
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
 * Created:     H5Rdeprec.c
 *
 * Purpose:     Deprecated functions from the H5R interface.  These
 *              functions are here for compatibility purposes and may be
 *              removed in the future.  Applications should switch to the
 *              newer APIs.
 *
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/

#include "H5Rmodule.h"          /* This source code file is part of the H5R module */


/***********/
/* Headers */
/***********/
/* Public headers needed by this file */
#include "H5Ppublic.h"          /* Property lists                           */

/* Private headers needed by this file */
#include "H5private.h"		    /* Generic Functions                        */
#include "H5ACprivate.h"	    /* Metadata cache                           */
#include "H5CXprivate.h"        /* API Contexts                             */
#include "H5Eprivate.h"		    /* Error handling                           */
#include "H5Gprivate.h"		    /* Groups                                   */
#include "H5Iprivate.h"         /* IDs                                      */
#include "H5Oprivate.h"		    /* Object headers                           */
#include "H5Rpkg.h"             /* References                               */


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


#ifndef H5_NO_DEPRECATED_SYMBOLS

/*-------------------------------------------------------------------------
 * Function:    H5Rget_obj_type1
 *
 * Purpose:     Retrieves the type of the object that an object points to.
 *
 * Parameters:
 *      id          IN: Dataset reference object is in or location ID of
 *                      object that the dataset is located within
 *      ref_type    IN: Type of reference to query
 *      ref         IN: Reference to query
 *
 * Return:      Success:	An object type (as defined in H5Gpublic.h)
 *              Failure:    H5G_UNKNOWN
 *
 *-------------------------------------------------------------------------
 */
H5G_obj_t
H5Rget_obj_type1(hid_t id, H5R_type_t ref_type, const void *ref)
{
    H5VL_object_t    *vol_obj = NULL;        /* object token of loc_id */
    H5VL_loc_params_t loc_params;
    H5O_type_t obj_type;        /* Object type */
    H5G_obj_t ret_value;        /* Return value */

    FUNC_ENTER_API(H5G_UNKNOWN)
    H5TRACE3("Go", "iRt*x", id, ref_type, ref);

    /* Check args */
    if (ref_type <= H5R_BADTYPE || ref_type >= H5R_MAXTYPE)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, H5G_UNKNOWN, "invalid reference type")
    if (ref == NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, H5G_UNKNOWN, "invalid reference pointer")

    /* Set location parameters */
    loc_params.type = H5VL_OBJECT_BY_SELF;
    loc_params.obj_type = H5I_get_type(id);

    /* Get the vol object */
    if (NULL == (vol_obj = H5VL_get_object(id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, H5G_UNKNOWN, "invalid file identifier")

    /* Get the object information */
    if (H5VL_object_get(vol_obj->data, loc_params, vol_obj->driver->cls, H5VL_REF_GET_TYPE,
                                    H5P_DATASET_XFER_DEFAULT, H5_REQUEST_NULL, &obj_type, ref_type, ref) < 0)
        HGOTO_ERROR(H5E_REFERENCE, H5E_CANTGET, H5G_UNKNOWN, "unable to determine object type")

    /* Set return value */
    ret_value = H5G_map_obj_type(obj_type);

done:
    FUNC_LEAVE_API(ret_value)
}   /* end H5Rget_obj_type1() */


/*-------------------------------------------------------------------------
 * Function:    H5Rdereference1
 *
 * Purpose:     Opens the HDF5 object referenced.
 *
 * Parameters:
 *      id          IN: Dataset reference object is in or location ID of
 *                      object that the dataset is located within
 *      ref_type    IN: Type of reference to create
 *      ref         IN: Reference to open
 *
 * Return:      Success:	Valid HDF5 ID
 *              Failure:    H5I_INVALID_HID
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Rdereference1(hid_t obj_id, H5R_type_t ref_type, const void *_ref)
{
    H5VL_object_t *vol_obj = NULL;        /* object token of loc_id */
    H5I_type_t opened_type;
    void *opened_obj = NULL;
    H5VL_loc_params_t loc_params;
    hid_t ret_value = H5I_INVALID_HID;	/* Return value */

    FUNC_ENTER_API(H5I_INVALID_HID)
    H5TRACE3("i", "iRt*x", obj_id, ref_type, _ref);

    /* Check args */
    if (ref_type <= H5R_BADTYPE || ref_type >= H5R_MAXTYPE)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, H5I_INVALID_HID, "invalid reference type")
    if (_ref == NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, H5I_INVALID_HID, "invalid reference pointer")

    /* Get the VOL object */
    if (NULL == (vol_obj = H5VL_get_object(obj_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, H5I_INVALID_HID, "invalid file identifier")

    loc_params.type = H5VL_OBJECT_BY_REF;
    loc_params.loc_data.loc_by_ref.ref_type = ref_type;
    loc_params.loc_data.loc_by_ref._ref = _ref;
    loc_params.loc_data.loc_by_ref.lapl_id = H5P_DATASET_ACCESS_DEFAULT;
    loc_params.obj_type = H5I_get_type(obj_id);

    /* Dereference */
    if (NULL == (opened_obj = H5VL_object_open(vol_obj->data, loc_params, vol_obj->driver->cls, &opened_type, 
                                              H5P_DATASET_XFER_DEFAULT, H5_REQUEST_NULL)))
        HGOTO_ERROR(H5E_REFERENCE, H5E_CANTOPENOBJ, H5I_INVALID_HID, "unable to dereference object")

    /* Get an atom for the object */
    if ((ret_value = H5VL_register_id(opened_type, opened_obj, vol_obj->driver, TRUE)) < 0)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, H5I_INVALID_HID, "unable to atomize object handle")

done:
    FUNC_LEAVE_API(ret_value)
}   /* end H5Rdereference1() */

#endif /* H5_NO_DEPRECATED_SYMBOLS */

