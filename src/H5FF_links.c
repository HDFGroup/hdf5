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


/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5ESprivate.h"        /* Event Stacks                         */
#include "H5FFprivate.h"        /* FastForward wrappers                 */
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Pprivate.h"		/* Property lists			*/
#include "H5VLprivate.h"	/* VOL plugins				*/
#include "H5VLiod.h"		/* IOD plugin - tmp      		*/
#include "H5VLiod_client.h"	/* Client IOD - tmp			*/
#include "H5VLiod_server.h"	/* Server IOD - tmp			*/


/*-------------------------------------------------------------------------
 * Function:	H5Lmove_ff
 *
 * Purpose:	Renames an object within an HDF5 file and moves it to a new
 *              group.  The original name SRC is unlinked from the group graph
 *              and then inserted with the new name DST (which can specify a
 *              new path for the object) as an atomic operation. The names
 *              are interpreted relative to SRC_LOC_ID and
 *              DST_LOC_ID, which are either file IDs or group ID.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Mohamad Chaarawi
 *              May 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Lmove_ff(hid_t src_loc_id, const char *src_name, hid_t dst_loc_id, 
           const char *dst_name, hid_t lcpl_id, hid_t lapl_id, hid_t trans_id, hid_t estack_id)
{
    H5VL_object_t *obj1 = NULL;        /* object token of src_id */
    H5VL_loc_params_t loc_params1;
    H5VL_object_t *obj2 = NULL;        /* object token of dst_id */
    H5VL_loc_params_t loc_params2;
    hid_t dxpl_id = H5P_DATASET_XFER_DEFAULT; /* transfer property list to pass to the VOL plugin */
    H5P_genplist_t *plist;     /* Property list pointer */
    H5_priv_request_t *request = NULL; /* private request struct inserted in event queue */
    void **req = NULL; /* pointer to plugin generate requests (Stays NULL if plugin does not support async */
    herr_t      ret_value=SUCCEED;              /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE8("e", "i*si*siiii", src_loc_id, src_name, dst_loc_id, dst_name,
             lcpl_id, lapl_id, trans_id, estack_id);

    /* Check arguments */
    if(src_loc_id == H5L_SAME_LOC && dst_loc_id == H5L_SAME_LOC)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "source and destination should not both be H5L_SAME_LOC")
    if(!src_name || !*src_name)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no current name specified")
    if(!dst_name || !*dst_name)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no destination name specified")
    if(lcpl_id != H5P_DEFAULT && (TRUE != H5P_isa_class(lcpl_id, H5P_LINK_CREATE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a link creation property list")

    /* Check the link create property list */
    if(H5P_DEFAULT == lcpl_id)
        lcpl_id = H5P_LINK_CREATE_DEFAULT;
    /* Check the link access property list */
    if(H5P_DEFAULT == lapl_id)
        lapl_id = H5P_LINK_ACCESS_DEFAULT;

    /* set location paramter for source object */
    loc_params1.type = H5VL_OBJECT_BY_NAME;
    loc_params1.loc_data.loc_by_name.name = src_name;
    loc_params1.loc_data.loc_by_name.lapl_id = lapl_id;
    loc_params1.obj_type = H5I_get_type(src_loc_id);
    /* set location paramter for destination object */
    loc_params2.type = H5VL_OBJECT_BY_NAME;
    loc_params2.loc_data.loc_by_name.name = dst_name;
    loc_params2.loc_data.loc_by_name.lapl_id = lapl_id;
    loc_params2.obj_type = H5I_get_type(dst_loc_id);

    if(H5L_SAME_LOC != src_loc_id) {
        /* get the file object */
        if(NULL == (obj1 = (H5VL_object_t *)H5I_object(src_loc_id)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid file identifier")
    }
    if(H5L_SAME_LOC != dst_loc_id) {
        /* get the file object */
        if(NULL == (obj2 = (H5VL_object_t *)H5I_object(dst_loc_id)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid file identifier")
    }

    /* Make sure that the VOL plugins are the same */
    if(obj1 && obj2) {
        if (obj1->vol_info->vol_cls->value != obj2->vol_info->vol_cls->value)
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "Objects are accessed through different VOL plugins and can't be linked")
    }

    /* store the transaction ID in the dxpl */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")
    if(H5P_set(plist, H5VL_TRANS_ID, &trans_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't set property value for trans_id")

    if(estack_id != H5_EVENT_STACK_NULL) {
        /* create the private request */
        if(NULL == (request = (H5_priv_request_t *)H5MM_calloc(sizeof(H5_priv_request_t))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
        req = &request->req;
        request->vol_cls = (obj1 ? obj1->vol_info->vol_cls : obj2->vol_info->vol_cls);
    }

    /* Move the link through the VOL */
    if(H5VL_link_move((obj1 ? obj1->vol_obj : NULL), loc_params1, 
                      (obj2 ? obj2->vol_obj : NULL), loc_params2, 
                      (obj1 ? obj1->vol_info->vol_cls : obj2->vol_info->vol_cls),
                      lcpl_id, lapl_id, dxpl_id, req) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to create link")

    if(request && *req)
        if(H5ES_insert(estack_id, request) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to insert request in event stack")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Lmove_ff() */


/*-------------------------------------------------------------------------
 * Function:	H5Lcopy_ff
 *
 * Purpose:	Creates an identical copy of a link with the same creation
 *              time and target.  The new link can have a different name
 *              and be in a different location than the original.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Mohamad Chaarawi
 *              May 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Lcopy_ff(hid_t src_loc_id, const char *src_name, hid_t dst_loc_id,
           const char *dst_name, hid_t lcpl_id, hid_t lapl_id, hid_t trans_id, hid_t estack_id)
{
    H5VL_object_t *obj1 = NULL;        /* object token of src_id */
    H5VL_loc_params_t loc_params1;
    H5VL_object_t *obj2 = NULL;        /* object token of dst_id */
    H5VL_loc_params_t loc_params2;
    hid_t dxpl_id = H5P_DATASET_XFER_DEFAULT; /* transfer property list to pass to the VOL plugin */
    H5P_genplist_t *plist;     /* Property list pointer */
    H5_priv_request_t *request = NULL; /* private request struct inserted in event queue */
    void **req = NULL; /* pointer to plugin generate requests (Stays NULL if plugin does not support async */
    herr_t      ret_value=SUCCEED;              /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE8("e", "i*si*siiii", src_loc_id, src_name, dst_loc_id, dst_name,
             lcpl_id, lapl_id, trans_id, estack_id);

    /* Check arguments */
    if(src_loc_id == H5L_SAME_LOC && dst_loc_id == H5L_SAME_LOC)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "source and destination should not both be H5L_SAME_LOC")
    if(!src_name || !*src_name)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no current name specified")
    if(!dst_name || !*dst_name)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no destination name specified")
    if(lcpl_id != H5P_DEFAULT && (TRUE != H5P_isa_class(lcpl_id, H5P_LINK_CREATE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a link creation property list")

    /* Check the link create property list */
    if(H5P_DEFAULT == lcpl_id)
        lcpl_id = H5P_LINK_CREATE_DEFAULT;
    /* Check the link access property list */
    if(H5P_DEFAULT == lapl_id)
        lapl_id = H5P_LINK_ACCESS_DEFAULT;

    /* set location paramter for source object */
    loc_params1.type = H5VL_OBJECT_BY_NAME;
    loc_params1.loc_data.loc_by_name.name = src_name;
    loc_params1.loc_data.loc_by_name.lapl_id = lapl_id;
    loc_params1.obj_type = H5I_get_type(src_loc_id);
    /* set location paramter for destination object */
    loc_params2.type = H5VL_OBJECT_BY_NAME;
    loc_params2.loc_data.loc_by_name.name = dst_name;
    loc_params2.loc_data.loc_by_name.lapl_id = lapl_id;
    loc_params2.obj_type = H5I_get_type(dst_loc_id);

    if(H5L_SAME_LOC != src_loc_id) {
        /* get the file object */
        if(NULL == (obj1 = (H5VL_object_t *)H5I_object(src_loc_id)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid file identifier")
    }
    if(H5L_SAME_LOC != dst_loc_id) {
        /* get the file object */
        if(NULL == (obj2 = (H5VL_object_t *)H5I_object(dst_loc_id)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid file identifier")
    }
    /* Make sure that the VOL plugins are the same */
    if(obj1 && obj2) {
        if (obj1->vol_info->vol_cls->value != obj2->vol_info->vol_cls->value)
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "Objects are accessed through different VOL plugins and can't be linked")
    }

    /* store the transaction ID in the dxpl */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")
    if(H5P_set(plist, H5VL_TRANS_ID, &trans_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't set property value for trans_id")

    if(estack_id != H5_EVENT_STACK_NULL) {
        /* create the private request */
        if(NULL == (request = (H5_priv_request_t *)H5MM_calloc(sizeof(H5_priv_request_t))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
        req = &request->req;
        request->vol_cls = (obj1 ? obj1->vol_info->vol_cls : obj2->vol_info->vol_cls);
    }

    /* Copy the link through the VOL */
    if(H5VL_link_copy((obj1 ? obj1->vol_obj : NULL), loc_params1, 
                      (obj2 ? obj2->vol_obj : NULL), loc_params2, 
                      (obj1 ? obj1->vol_info->vol_cls : obj2->vol_info->vol_cls),
                      lcpl_id, lapl_id, dxpl_id, req) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to create link")

    if(request && *req)
        if(H5ES_insert(estack_id, request) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to insert request in event stack")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Lcopy_ff() */


/*-------------------------------------------------------------------------
 * Function:	H5Lcreate_soft_ff
 *
 * Purpose:	Creates a soft link from LINK_NAME to LINK_TARGET.
 *
 * 		LINK_TARGET can be anything and is interpreted at lookup
 *              time relative to the group which contains the final component
 *              of LINK_NAME.  For instance, if LINK_TARGET is `./foo' and
 *              LINK_NAME is `./x/y/bar' and a request is made for `./x/y/bar'
 *              then the actual object looked up is `./x/y/./foo'.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Mohamad Chaarawi
 *              May 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Lcreate_soft_ff(const char *link_target, hid_t link_loc_id, const char *link_name, 
                  hid_t lcpl_id, hid_t lapl_id, hid_t trans_id, hid_t estack_id)
{
    H5VL_object_t *obj = NULL;        /* object token of loc_id */
    hid_t dxpl_id = H5P_DATASET_XFER_DEFAULT; /* transfer property list to pass to the VOL plugin */
    H5VL_loc_params_t loc_params;
    H5_priv_request_t *request = NULL; /* private request struct inserted in event queue */
    void **req = NULL; /* pointer to plugin generate requests (Stays NULL if plugin does not support async */
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE7("e", "*si*siiii", link_target, link_loc_id, link_name, lcpl_id,
             lapl_id, trans_id, estack_id);

    /* Check arguments */
    if(link_loc_id == H5L_SAME_LOC)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "link location id should not be H5L_SAME_LOC")
    if(!link_target || !*link_target)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no target specified")
    if(!link_name || !*link_name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no new name specified")
    if(lcpl_id != H5P_DEFAULT && (TRUE != H5P_isa_class(lcpl_id, H5P_LINK_CREATE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a link creation property list")

    /* Check the group access property list */
    if(H5P_DEFAULT == lcpl_id)
        lcpl_id = H5P_LINK_CREATE_DEFAULT;
    /* Check the link access property list */
    if(H5P_DEFAULT == lapl_id)
        lapl_id = H5P_LINK_ACCESS_DEFAULT;

    loc_params.type = H5VL_OBJECT_BY_NAME;
    loc_params.loc_data.loc_by_name.name = link_name;
    loc_params.loc_data.loc_by_name.lapl_id = lapl_id;
    loc_params.obj_type = H5I_get_type(link_loc_id);

    /* get the location object */
    if(NULL == (obj = (H5VL_object_t *)H5VL_get_object(link_loc_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid location identifier")

    /* store the transaction ID in the dxpl */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")
    if(H5P_set(plist, H5VL_TRANS_ID, &trans_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't set property value for trans_id")

    /* Get the plist structure */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(lcpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* set creation properties */
    if(H5P_set(plist, H5VL_PROP_LINK_TARGET_NAME, &link_target) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get property value for target name")

    if(estack_id != H5_EVENT_STACK_NULL) {
        /* create the private request */
        if(NULL == (request = (H5_priv_request_t *)H5MM_calloc(sizeof(H5_priv_request_t))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
        req = &request->req;
        request->vol_cls = obj->vol_info->vol_cls;
    }

    /* Create the link through the VOL */
    if(H5VL_link_create(H5VL_LINK_CREATE_SOFT, obj->vol_obj, loc_params, obj->vol_info->vol_cls,
                        lcpl_id, lapl_id, dxpl_id, req) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to create link")

    if(request && *req)
        if(H5ES_insert(estack_id, request) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to insert request in event stack")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Lcreate_soft_ff() */


/*-------------------------------------------------------------------------
 * Function:	H5Lcreate_hard_ff
 *
 * Purpose:	Creates a hard link from NEW_NAME to CUR_NAME.
 *
 *		CUR_NAME must name an existing object.  CUR_NAME and
 *              NEW_NAME are interpreted relative to CUR_LOC_ID and
 *              NEW_LOC_ID, which are either file IDs or group IDs.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Mohamad Chaarawi
 *              May 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Lcreate_hard_ff(hid_t cur_loc_id, const char *cur_name, hid_t new_loc_id, 
                  const char *new_name, hid_t lcpl_id, hid_t lapl_id, 
                  hid_t trans_id, hid_t estack_id)
{
    H5VL_object_t *obj1 = NULL;        /* object token of loc_id */
    H5VL_object_t *obj2 = NULL;        /* object token of loc_id */
    H5VL_loc_params_t loc_params1;
    H5VL_loc_params_t loc_params2;
    H5_priv_request_t *request = NULL; /* private request struct inserted in event queue */
    void **req = NULL; /* pointer to plugin generate requests (Stays NULL if plugin does not support async */
    H5P_genplist_t *plist;      /* Property list pointer */
    hid_t dxpl_id = H5P_DATASET_XFER_DEFAULT; /* transfer property list to pass to the VOL plugin */
    herr_t   ret_value = SUCCEED;            /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE8("e", "i*si*siiii", cur_loc_id, cur_name, new_loc_id, new_name,
             lcpl_id, lapl_id, trans_id, estack_id);

    /* Check arguments */
    if(cur_loc_id == H5L_SAME_LOC && new_loc_id == H5L_SAME_LOC)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "source and destination should not be both H5L_SAME_LOC")
    if(!cur_name || !*cur_name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no current name specified")
    if(!new_name || !*new_name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no new name specified")
    if(lcpl_id != H5P_DEFAULT && (TRUE != H5P_isa_class(lcpl_id, H5P_LINK_CREATE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a link creation property list")

    /* Check the link create property list */
    if(H5P_DEFAULT == lcpl_id)
        lcpl_id = H5P_LINK_CREATE_DEFAULT;
    /* Check the link access property list */
    if(H5P_DEFAULT == lapl_id)
        lapl_id = H5P_LINK_ACCESS_DEFAULT;

    loc_params1.type = H5VL_OBJECT_BY_NAME;
    loc_params1.obj_type = H5I_get_type(cur_loc_id);
    loc_params1.loc_data.loc_by_name.name = cur_name;
    loc_params1.loc_data.loc_by_name.lapl_id = lapl_id;

    loc_params2.type = H5VL_OBJECT_BY_NAME;
    loc_params2.obj_type = H5I_get_type(new_loc_id);
    loc_params2.loc_data.loc_by_name.name = new_name;
    loc_params2.loc_data.loc_by_name.lapl_id = lapl_id;

    if(H5L_SAME_LOC != cur_loc_id) {
        /* get the file object */
        if(NULL == (obj1 = (H5VL_object_t *)H5VL_get_object(cur_loc_id)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid file identifier")
    }
    if(H5L_SAME_LOC != new_loc_id) {
        /* get the file object */
        if(NULL == (obj2 = (H5VL_object_t *)H5VL_get_object(new_loc_id)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid file identifier")
    }
    /* Make sure that the VOL plugins are the same */
    if(obj1 && obj2) {
        if (obj1->vol_info->vol_cls->value != obj2->vol_info->vol_cls->value)
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "Objects are accessed through different VOL plugins and can't be linked")
    }

    /* Get the plist structure */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(lcpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* set creation properties */
    if(H5P_set(plist, H5VL_PROP_LINK_TARGET, (obj1 ? &(obj1->vol_obj) : NULL)) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't set property value for target id")
    if(H5P_set(plist, H5VL_PROP_LINK_TARGET_LOC_PARAMS, &loc_params1) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't set property value for target name")

    /* store the transaction ID in the dxpl */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")
    if(H5P_set(plist, H5VL_TRANS_ID, &trans_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't set property value for trans_id")

    if(estack_id != H5_EVENT_STACK_NULL) {
        /* create the private request */
        if(NULL == (request = (H5_priv_request_t *)H5MM_calloc(sizeof(H5_priv_request_t))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
        req = &request->req;
        request->vol_cls = (obj1!=NULL ? obj1->vol_info->vol_cls : obj2->vol_info->vol_cls);
    }

    /* Create the link through the VOL */
    if(H5VL_link_create(H5VL_LINK_CREATE_HARD, (obj2 ? (obj2->vol_obj) : NULL), loc_params2, 
                        (obj1!=NULL ? obj1->vol_info->vol_cls : obj2->vol_info->vol_cls),
                        lcpl_id, lapl_id, dxpl_id, req) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to create link")

    if(request && *req)
        if(H5ES_insert(estack_id, request) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to insert request in event stack")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Lcreate_hard_ff() */


/*-------------------------------------------------------------------------
 * Function:	H5Ldelete_ff
 *
 * Purpose:	Removes the specified NAME from the group graph and
 *		decrements the link count for the object to which NAME
 *		points.  If the link count reaches zero then all file-space
 *		associated with the object will be reclaimed (but if the
 *		object is open, then the reclamation of the file space is
 *		delayed until all handles to the object are closed).
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Mohamad Chaarawi
 *              May 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Ldelete_ff(hid_t loc_id, const char *name, hid_t lapl_id, hid_t trans_id, hid_t estack_id)
{
    H5VL_object_t *obj = NULL;        /* object token of loc_id */
    hid_t dxpl_id = H5P_DATASET_XFER_DEFAULT; /* transfer property list to pass to the VOL plugin */
    H5P_genplist_t *plist;     /* Property list pointer */
    H5_priv_request_t *request = NULL; /* private request struct inserted in event queue */
    void **req = NULL; /* pointer to plugin generate requests (Stays NULL if plugin does not support async */
    H5VL_loc_params_t loc_params;
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE5("e", "i*siii", loc_id, name, lapl_id, trans_id, estack_id);

    /* Check arguments */
    if(!name || !*name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name")

    loc_params.type = H5VL_OBJECT_BY_NAME;
    loc_params.obj_type = H5I_get_type(loc_id);
    loc_params.loc_data.loc_by_name.name = name;
    loc_params.loc_data.loc_by_name.lapl_id = lapl_id;

    /* store the transaction ID in the dxpl */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")
    if(H5P_set(plist, H5VL_TRANS_ID, &trans_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't set property value for trans_id")

    /* get the file object */
    if(NULL == (obj = (H5VL_object_t *)H5I_object(loc_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid file identifier")

    if(estack_id != H5_EVENT_STACK_NULL) {
        /* create the private request */
        if(NULL == (request = (H5_priv_request_t *)H5MM_calloc(sizeof(H5_priv_request_t))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
        req = &request->req;
        request->vol_cls = obj->vol_info->vol_cls;
    }

    /* Delete the link through the VOL */
    if(H5VL_link_specific(obj->vol_obj, loc_params, obj->vol_info->vol_cls, H5VL_LINK_DELETE, 
                          dxpl_id, req) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTDELETE, FAIL, "unable to delete link")

    if(request && *req)
        if(H5ES_insert(estack_id, request) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to insert request in event stack")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Ldelete_ff() */


/*-------------------------------------------------------------------------
 * Function:	H5Lexists_ff
 *
 * Purpose:	Checks if a link of a given name exists in a group
 *
 * Return:	Success:	Positive
 * 		Failure:	Negative
 *
 * Programmer:	Mohamad Chaarawi
 *              May 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Lexists_ff(hid_t loc_id, const char *name, hid_t lapl_id, htri_t *ret, 
             hid_t rcxt_id, hid_t estack_id)
{
    H5VL_object_t *obj = NULL;        /* object token of loc_id */
    hid_t dxpl_id = H5P_DATASET_XFER_DEFAULT; /* transfer property list to pass to the VOL plugin */
    H5P_genplist_t *plist;     /* Property list pointer */
    H5_priv_request_t *request = NULL; /* private request struct inserted in event queue */
    void **req = NULL; /* pointer to plugin generate requests (Stays NULL if plugin does not support async */
    H5VL_loc_params_t loc_params;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)
    H5TRACE6("e", "i*si*tii", loc_id, name, lapl_id, ret, rcxt_id, estack_id);

    /* Check arguments */
    if(!name || !*name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name specified")
    if(H5P_DEFAULT == lapl_id)
        lapl_id = H5P_LINK_ACCESS_DEFAULT;
    else
        if(TRUE != H5P_isa_class(lapl_id, H5P_LINK_ACCESS))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not link access property list ID")

    loc_params.type = H5VL_OBJECT_BY_NAME;
    loc_params.obj_type = H5I_get_type(loc_id);
    loc_params.loc_data.loc_by_name.name = name;
    loc_params.loc_data.loc_by_name.lapl_id = lapl_id;

    /* get the file object */
    if(NULL == (obj = (H5VL_object_t *)H5I_object(loc_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid file identifier")

    /* store the transaction ID in the dxpl */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")
    if(H5P_set(plist, H5VL_CONTEXT_ID, &rcxt_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't set property value for rcxt_id")

    if(estack_id != H5_EVENT_STACK_NULL) {
        /* create the private request */
        if(NULL == (request = (H5_priv_request_t *)H5MM_calloc(sizeof(H5_priv_request_t))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
        req = &request->req;
        request->vol_cls = obj->vol_info->vol_cls;
    }

    /* check link existence through the VOL */
    if(H5VL_link_specific(obj->vol_obj, loc_params, obj->vol_info->vol_cls, H5VL_LINK_EXISTS,
                          dxpl_id, req, (htri_t *)ret) < 0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTGET, FAIL, "unable to get link info")

    if(request && *req)
        if(H5ES_insert(estack_id, request) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to insert request in event stack")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Lexists_ff() */


/*-------------------------------------------------------------------------
 * Function:	H5Lget_info_ff
 *
 * Purpose:	Gets metadata for a link.
 *
 * Return:	Success:	Non-negative with information in LINFO
 * 		Failure:	Negative
 *
 * Programmer:	Mohamad Chaarawi
 *              August 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Lget_info_ff(hid_t loc_id, const char *name, H5L_ff_info_t *linfo ,
               hid_t lapl_id, hid_t rcxt_id, hid_t estack_id)
{
    H5VL_object_t *obj = NULL;        /* object token of loc_id */
    hid_t dxpl_id = H5P_DATASET_XFER_DEFAULT; /* transfer property list to pass to the VOL plugin */
    H5P_genplist_t *plist;     /* Property list pointer */
    H5_priv_request_t *request = NULL; /* private request struct inserted in event queue */
    void **req = NULL; /* pointer to plugin generate requests (Stays NULL if plugin does not support async */
    H5VL_loc_params_t loc_params;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)
    H5TRACE6("e", "i*s*xiii", loc_id, name, linfo, lapl_id, rcxt_id, estack_id);

    if(!name || !*name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name specified")
    if(H5P_DEFAULT == lapl_id)
        lapl_id = H5P_LINK_ACCESS_DEFAULT;
    else
        if(TRUE != H5P_isa_class(lapl_id, H5P_LINK_ACCESS))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not link access property list ID")

    loc_params.type = H5VL_OBJECT_BY_NAME;
    loc_params.obj_type = H5I_get_type(loc_id);
    loc_params.loc_data.loc_by_name.name = name;
    loc_params.loc_data.loc_by_name.lapl_id = lapl_id;

    /* get the file object */
    if(NULL == (obj = (H5VL_object_t *)H5I_object(loc_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid file identifier")

    /* store the transaction ID in the dxpl */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")
    if(H5P_set(plist, H5VL_CONTEXT_ID, &rcxt_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't set property value for rcxt_id")

    if(estack_id != H5_EVENT_STACK_NULL) {
        /* create the private request */
        if(NULL == (request = (H5_priv_request_t *)H5MM_calloc(sizeof(H5_priv_request_t))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
        req = &request->req;
        request->vol_cls = obj->vol_info->vol_cls;
    }

    /* Get the link info through the VOL */
    if(H5VL_link_get(obj->vol_obj, loc_params, obj->vol_info->vol_cls, H5VL_LINK_GET_INFO, 
                     dxpl_id, req, linfo) < 0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTGET, FAIL, "unable to get link info")

    if(request && *req)
        if(H5ES_insert(estack_id, request) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to insert request in event stack")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Lget_info_ff() */


/*-------------------------------------------------------------------------
 * Function:	H5Lget_val_ff
 *
 * Purpose:	Returns the link value of a link whose name is NAME.  For
 *              symbolic links, this is the path to which the link points,
 *              including the null terminator.  For user-defined links, it
 *              is the link buffer.
 *
 *              At most SIZE bytes are copied to the BUF result buffer.
 *
 * Return:	Success:	Non-negative with the link value in BUF.
 *
 * 		Failure:	Negative
 *
 * Programmer:	Mohamad Chaarawi
 *              August 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Lget_val_ff(hid_t loc_id, const char *name, void *buf, size_t size,
              hid_t lapl_id, hid_t rcxt_id, hid_t estack_id)
{
    H5VL_object_t *obj = NULL;        /* object token of loc_id */
    hid_t dxpl_id = H5P_DATASET_XFER_DEFAULT; /* transfer property list to pass to the VOL plugin */
    H5P_genplist_t *plist;     /* Property list pointer */
    H5_priv_request_t *request = NULL; /* private request struct inserted in event queue */
    void **req = NULL; /* pointer to plugin generate requests (Stays NULL if plugin does not support async */
    H5VL_loc_params_t loc_params;
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE7("e", "i*s*xziii", loc_id, name, buf, size, lapl_id, rcxt_id, estack_id);

    /* Check arguments */
    if(!name || !*name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name specified")
    if(H5P_DEFAULT == lapl_id)
        lapl_id = H5P_LINK_ACCESS_DEFAULT;
    else
        if(TRUE != H5P_isa_class(lapl_id, H5P_LINK_ACCESS))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not link access property list ID")

    loc_params.type = H5VL_OBJECT_BY_NAME;
    loc_params.obj_type = H5I_get_type(loc_id);
    loc_params.loc_data.loc_by_name.name = name;
    loc_params.loc_data.loc_by_name.lapl_id = lapl_id;

    /* get the file object */
    if(NULL == (obj = (H5VL_object_t *)H5I_object(loc_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid file identifier")

    /* store the transaction ID in the dxpl */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")
    if(H5P_set(plist, H5VL_CONTEXT_ID, &rcxt_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't set property value for rcxt_id")

    if(estack_id != H5_EVENT_STACK_NULL) {
        /* create the private request */
        if(NULL == (request = (H5_priv_request_t *)H5MM_calloc(sizeof(H5_priv_request_t))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
        req = &request->req;
        request->vol_cls = obj->vol_info->vol_cls;
    }

    /* Get the link info through the VOL */
    if(H5VL_link_get(obj->vol_obj, loc_params, obj->vol_info->vol_cls, H5VL_LINK_GET_VAL, 
                     dxpl_id, req, buf, size) < 0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTGET, FAIL, "unable to get link value")

    if(request && *req)
        if(H5ES_insert(estack_id, request) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to insert request in event stack")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Lget_val_ff() */
#if 0

/*-------------------------------------------------------------------------
 * Function:	H5Literate
 *
 * Purpose:	FFwd version of H5Literate. 
 *              For now iteration is done in native order.
 *
 * Return:	Success:	The return value of the first operator that
 *				returns non-zero, or zero if all members were
 *				processed with no operator returning non-zero.
 *
 *		Failure:	Negative if something goes wrong within the
 *				library, or the negative value returned by one
 *				of the operators.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Literate_ff(hid_t obj_id, H5_index_t idx_type, H5_iter_order_t order,
            H5L_iterate_ff_t op, void *op_data, hid_t rcxt_id, hid_t H5_ATTR_UNUSED estack_id)
{
    H5VL_object_t    *obj = NULL;        /* object token of loc_id */
    H5VL_loc_params_t loc_params;
    hid_t dxpl_id = H5P_DATASET_XFER_DEFAULT; /* transfer property list to pass to the VOL plugin */
    H5P_genplist_t *plist;     /* Property list pointer */
    herr_t      ret_value;              /* Return value */

    FUNC_ENTER_API(FAIL)

    /* Check args */
    if(idx_type <= H5_INDEX_UNKNOWN || idx_type >= H5_INDEX_N)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid index type specified")
    if(order != H5_ITER_NATIVE)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "Only H5_ITER_NATIVE iteration order is supported")
    if(!op)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no callback operator specified")

    loc_params.type = H5VL_OBJECT_BY_SELF;
    loc_params.obj_type = H5I_get_type(obj_id);

    /* get the location object */
    if(NULL == (obj = H5VL_get_object(obj_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid location identifier")

    /* store the transaction ID in the dxpl */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")
    if(H5P_set(plist, H5VL_CONTEXT_ID, &rcxt_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't set property value for rcxt_id")

    /* iterate over the objects through the VOL */
    if((ret_value = H5VL_object_specific(obj->vol_obj, loc_params, obj->vol_info->vol_cls, 
                                         H5VL_OBJECT_ITERATE, dxpl_id, H5_REQUEST_NULL, 
                                         idx_type, order, op, op_data)) < 0)
	HGOTO_ERROR(H5E_OHDR, H5E_BADITER, FAIL, "link iteration failed")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Literate_ff() */


/*-------------------------------------------------------------------------
 * Function:	H5Literate_by_name_ff
 *
 * Purpose:	FFwd version of H5Literate_by_name. 
 *              For now iteration is done in native order.
 *
 * Return:	Success:	The return value of the first operator that
 *				returns non-zero, or zero if all members were
 *				processed with no operator returning non-zero.
 *
 *		Failure:	Negative if something goes wrong within the
 *				library, or the negative value returned by one
 *				of the operators.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Literate_by_name_ff(hid_t loc_id, const char *obj_name, H5_index_t idx_type,
                    H5_iter_order_t order, H5L_iterate_ff_t op, void *op_data, 
                    hid_t lapl_id, hid_t rcxt_id, hid_t H5_ATTR_UNUSED estack_id)
{
    H5VL_object_t    *obj = NULL;        /* object token of loc_id */
    H5VL_loc_params_t loc_params; 
    hid_t dxpl_id = H5P_DATASET_XFER_DEFAULT; /* transfer property list to pass to the VOL plugin */
    H5P_genplist_t *plist;     /* Property list pointer */
    herr_t      ret_value;              /* Return value */

    FUNC_ENTER_API(FAIL)

    /* Check args */
    if(!obj_name || !*obj_name)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name")
    if(idx_type <= H5_INDEX_UNKNOWN || idx_type >= H5_INDEX_N)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid index type specified")
    if(order != H5_ITER_NATIVE)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "Only H5_ITER_NATIVE iteration order is supported")
    if(!op)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no callback operator specified")
    if(H5P_DEFAULT == lapl_id)
        lapl_id = H5P_LINK_ACCESS_DEFAULT;
    else
        if(TRUE != H5P_isa_class(lapl_id, H5P_LINK_ACCESS))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not link access property list ID")

    loc_params.type = H5VL_OBJECT_BY_NAME;
    loc_params.loc_data.loc_by_name.name = obj_name;
    loc_params.loc_data.loc_by_name.lapl_id = lapl_id;
    loc_params.obj_type = H5I_get_type(loc_id);

    /* get the location object */
    if(NULL == (obj = H5VL_get_object(loc_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid location identifier")

    /* store the transaction ID in the dxpl */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")
    if(H5P_set(plist, H5VL_CONTEXT_ID, &rcxt_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't set property value for rcxt_id")

    /* iterate over the objects through the VOL */
    if((ret_value = H5VL_object_specific(obj->vol_obj, loc_params, obj->vol_info->vol_cls, 
                                         H5VL_OBJECT_ITERATE, dxpl_id, H5_REQUEST_NULL, 
                                         idx_type, order, op, op_data)) < 0)
	HGOTO_ERROR(H5E_OHDR, H5E_BADITER, FAIL, "link iteration failed")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Literate_by_name_ff() */
#endif
