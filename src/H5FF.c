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
 * Purpose:	Wrappers around existing HDF5 to support Exascale FastForward
 *              functionality.
 */


/****************/
/* Module Setup */
/****************/

#include "H5FFmodule.h"         /* This source code file is part of the H5FF module */

#define H5A_FRIEND		/*suppress error about including H5Apkg	  */
#define H5D_FRIEND		/*suppress error about including H5Dpkg	  */
#define H5F_FRIEND		/*suppress error about including H5Fpkg	  */
#define H5G_FRIEND		/*suppress error about including H5Gpkg	  */
#define H5T_FRIEND		/*suppress error about including H5Tpkg	  */


/***********/
/* Headers */
/***********/
#include "H5private.h"          /* Generic Functions                    */
//#include "H5Apkg.h"             /* Attribute access                     */
#include "H5Dpkg.h"             /* Dataset access                       */
#include "H5Eprivate.h"         /* Error handling                       */
#include "H5Fpkg.h"             /* File access                          */
#include "H5FFprivate.h"        /* FastForward wrappers                 */
//#include "H5Gpkg.h"             /* Group access                         */
#include "H5Iprivate.h"         /* IDs                                  */
#include "H5MMprivate.h"        /* Memory management                    */
#include "H5Pprivate.h"         /* Property lists                       */
//#include "H5Tpkg.h"             /* Datatype access                      */

#include "H5VLdaosm.h"          /* IOD plugin - tmp                     */

#ifdef H5_HAVE_EFF
/****************/
/* Local Macros */
/****************/
H5FL_EXTERN(H5RC_t);

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

herr_t
H5FF__init_package(void)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_STATIC

    if(H5F_init() < 0)
        HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to init file interface")

    /*if(H5G_init() < 0)
        HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to init group interface")

    if(H5D_init() < 0)
        HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to init dataset interface")

    if(H5A_init() < 0)
        HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to init attribute interface")

    if(H5M_init() < 0)
        HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to init map interface")

    if(H5RC_init() < 0)
        HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to init map interface") DSMINC*/

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FF__init_package() */


/*-------------------------------------------------------------------------
 * Function:    H5Dcreate_ff
 *
 * Purpose:     Asynchronous wrapper around H5Dcreate().
 *
 * Return:      Success:        The placeholder ID for a new dataset.  When
 *                              the asynchronous operation completes, this
 *                              ID will transparently be modified to be a
 *                              "normal" ID.
 *              Failure:        FAIL
 *
 * Programmer:  Neil Fortner
 *              Monday, November 7, 2016
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Dcreate_ff(hid_t loc_id, const char *name, hid_t type_id, hid_t space_id, 
             hid_t lcpl_id, hid_t dcpl_id, hid_t dapl_id, hid_t trans_id,
             hid_t H5_ATTR_UNUSED estack_id)
{
    void    *dset = NULL;       /* dset token from VOL plugin */
    H5VL_object_t *obj = NULL;        /* object token of loc_id */
    hid_t dxpl_id = H5P_DATASET_XFER_DEFAULT; /* transfer property list to pass to the VOL plugin */
    H5VL_loc_params_t loc_params;
    H5P_genplist_t  *plist;     /* Property list pointer */
    hid_t       ret_value;              /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE9("i", "i*siiiiiii", loc_id, name, type_id, space_id, lcpl_id, dcpl_id,
             dapl_id, trans_id, estack_id);

    if(!name || !*name)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name")

    /* Get correct property list */
    if(H5P_DEFAULT == lcpl_id)
        lcpl_id = H5P_LINK_CREATE_DEFAULT;
    else
        if(TRUE != H5P_isa_class(lcpl_id, H5P_LINK_CREATE))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not link creation property list")

    /* Get correct property list */
    if(H5P_DEFAULT == dcpl_id)
        dcpl_id = H5P_DATASET_CREATE_DEFAULT;
    else
        if(TRUE != H5P_isa_class(dcpl_id, H5P_DATASET_CREATE))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not dataset create property list ID")

    /* Get correct property list */
    if(H5P_DEFAULT == dapl_id)
        dapl_id = H5P_DATASET_ACCESS_DEFAULT;
    else
        if(TRUE != H5P_isa_class(dapl_id, H5P_DATASET_ACCESS))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not dataset access property list")

    /* Get the plist structure */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(dcpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* set creation properties */
    if(H5P_set(plist, H5VL_PROP_DSET_TYPE_ID, &type_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't set property value for datatype id")
    if(H5P_set(plist, H5VL_PROP_DSET_SPACE_ID, &space_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't set property value for space id")
    if(H5P_set(plist, H5VL_PROP_DSET_LCPL_ID, &lcpl_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't set property value for lcpl id")

    loc_params.type = H5VL_OBJECT_BY_SELF;
    loc_params.obj_type = H5I_get_type(loc_id);

    /* store the transaction ID in the dxpl */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")
    if(H5P_set(plist, H5VL_TRANS_ID, &trans_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set property value for trans_id")

    /* get the location object */
    if(NULL == (obj = (H5VL_object_t *)H5I_object(loc_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid location identifier")

    /* Create the dataset through the VOL */
    if(NULL == (dset = H5VL_dataset_create(obj->vol_obj, loc_params, obj->vol_info->vol_cls, 
                                           name, dcpl_id, dapl_id, dxpl_id, H5_REQUEST_NULL)))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to create dataset")

    /* Get an atom for the dataset */
    if((ret_value = H5VL_register_id(H5I_DATASET, dset, obj->vol_info, TRUE)) < 0)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to atomize dataset handle")

done:
    if (ret_value < 0 && dset)
        if(H5VL_dataset_close (dset, obj->vol_info->vol_cls, dxpl_id, H5_REQUEST_NULL) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CLOSEERROR, FAIL, "unable to release dataset")
    FUNC_LEAVE_API(ret_value)
} /* end H5Dcreate_ff() */


/*-------------------------------------------------------------------------
 * Function:    H5Dopen_ff
 *
 * Purpose:     Asynchronous wrapper around H5Dopen().
 *
 * Return:      Success:        The placeholder ID for a dataset.  When
 *                              the asynchronous operation completes, this
 *                              ID will transparently be modified to be a
 *                              "normal" ID.
 *              Failure:        FAIL
 *
 * Programmer:  Neil Fortner
 *              Monday, November 7, 2016
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Dopen_ff(hid_t loc_id, const char *name, hid_t dapl_id, hid_t trans_id, hid_t H5_ATTR_UNUSED estack_id)
{
    void    *dset = NULL;       /* dset token from VOL plugin */
    H5VL_object_t *obj = NULL;        /* object token of loc_id */
    hid_t dxpl_id = H5P_DATASET_XFER_DEFAULT; /* transfer property list to pass to the VOL plugin */
    H5P_genplist_t *plist;     /* Property list pointer */
    H5VL_loc_params_t loc_params;
    hid_t       ret_value;              /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE5("i", "i*siii", loc_id, name, dapl_id, trans_id, estack_id);

    /* Check args */
    if(!name || !*name)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name")

    /* Get correct property list */
    if(H5P_DEFAULT == dapl_id)
        dapl_id = H5P_DATASET_ACCESS_DEFAULT;
    else
        if(TRUE != H5P_isa_class(dapl_id, H5P_DATASET_ACCESS))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not dataset access property list")

    loc_params.type = H5VL_OBJECT_BY_SELF;
    loc_params.obj_type = H5I_get_type(loc_id);

    /* get the location object */
    if(NULL == (obj = (H5VL_object_t *)H5I_object(loc_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid location identifier")

    /* store the transaction ID in the dxpl */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")
    if(H5P_set(plist, H5VL_TRANS_ID, &trans_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set property value for trans_id")

    /* Create the dataset through the VOL */
    if(NULL == (dset = H5VL_dataset_open(obj->vol_obj, loc_params, obj->vol_info->vol_cls, name, 
                                         dapl_id, dxpl_id, H5_REQUEST_NULL)))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTOPENOBJ, FAIL, "unable to open dataset")

    /* Get an atom for the dataset */
    if((ret_value = H5VL_register_id(H5I_DATASET, dset, obj->vol_info, TRUE)) < 0)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to atomize dataset handle")

done:
    if (ret_value < 0 && dset)
        if(H5VL_dataset_close (dset, obj->vol_info->vol_cls, dxpl_id, H5_REQUEST_NULL) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CLOSEERROR, FAIL, "unable to release dataset")
    FUNC_LEAVE_API(ret_value)
} /* end H5Dopen_ff() */


/*-------------------------------------------------------------------------
 * Function:    H5Dclose_ff
 *
 * Purpose:     Closes access to a dataset (DATASET_ID) and releases
 *              resources used by it. It is illegal to subsequently use that
 *              same dataset ID in calls to other dataset functions.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Neil Fortner
 *              Monday, November 7, 2016
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Dclose_ff(hid_t dset_id, hid_t H5_ATTR_UNUSED estack_id)
{
    H5VL_object_t *dset;
    herr_t       ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "ii", dset_id, estack_id);

    /* Check args */
    if(NULL == (dset = (H5VL_object_t *)H5I_object_verify(dset_id, H5I_DATASET)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid dataset identifier")

    /*
     * Decrement the counter on the dataset.  It will be freed if the count
     * reaches zero.  
     *
     * Pass in TRUE for the 3rd parameter to tell the function to remove
     * dataset's ID even though the freeing function might fail.  Please
     * see the comments in H5I_dec_ref for details. (SLU - 2010/9/7)
     */
    if(H5I_dec_app_ref_always_close(dset_id) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTDEC, FAIL, "can't decrement count on dataset ID")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Dclose_ff() */

#endif /* H5_HAVE_EFF */

