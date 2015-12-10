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

#define H5O_FRIEND		/*suppress error about including H5Opkg	  */
#define H5T_FRIEND		/*suppress error about including H5Tpkg	  */


/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5ESprivate.h"        /* Event Stacks                         */
#include "H5FFprivate.h"        /* FastForward wrappers                 */
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Opkg.h"             /* Object headers			*/
#include "H5Pprivate.h"		/* Property lists			*/
#include "H5Tpkg.h"             /* Datatype access			*/
#include "H5VLprivate.h"	/* VOL plugins				*/
#include "H5VLiod.h"		/* IOD plugin - tmp      		*/
#include "H5VLiod_client.h"	/* Client IOD - tmp			*/
#include "H5VLiod_server.h"	/* Server IOD - tmp			*/


/*-------------------------------------------------------------------------
 * Function:	H5Oopen_ff
 *
 * Purpose:	Opens an object within an HDF5 file.
 *
 *              This function opens an object in the same way that H5Gopen2,
 *              H5Topen2, and H5Dopen2 do. However, H5Oopen doesn't require
 *              the type of object to be known beforehand. This can be
 *              useful in user-defined links, for instance, when only a
 *              path is known.
 *
 *              The opened object should be closed again with H5Oclose
 *              or H5Gclose, H5Tclose, or H5Dclose.
 *
 * Return:	Success:	An open object identifier
 *		Failure:	Negative
 *
 * Programmer:	Mohamad Chaarawi
 *              May 2013
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Oopen_ff(hid_t loc_id, const char *name, hid_t lapl_id, hid_t rcxt_id)
{
    H5VL_object_t *obj = NULL;        /* object token of loc_id */
    hid_t dxpl_id = H5P_DATASET_XFER_DEFAULT; /* transfer property list to pass to the VOL plugin */
    H5P_genplist_t *plist;     /* Property list pointer */
    H5I_type_t  opened_type;
    H5VL_object_t    *opened_obj = NULL;
    H5VL_loc_params_t loc_params;
    hid_t       ret_value = FAIL;

    FUNC_ENTER_API(FAIL)
    H5TRACE4("i", "i*sii", loc_id, name, lapl_id, rcxt_id);

    if(!name || !*name)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name")

    loc_params.type = H5VL_OBJECT_BY_NAME;
    loc_params.loc_data.loc_by_name.name = name;
    loc_params.loc_data.loc_by_name.lapl_id = lapl_id;
    loc_params.obj_type = H5I_get_type(loc_id);

    /* get the file object */
    if(NULL == (obj = (H5VL_object_t *)H5I_object(loc_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid file identifier")

    /* store the transaction ID in the dxpl */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")
    if(H5P_set(plist, H5VL_CONTEXT_ID, &rcxt_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't set property value for rcxt_id")

    /* Open the object through the VOL */
    if(NULL == (opened_obj = (H5VL_object_t *)H5VL_object_open(obj->vol_obj, loc_params, 
                                                               obj->vol_info->vol_cls, &opened_type, 
                                                               dxpl_id, H5_REQUEST_NULL)))
	HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, FAIL, "unable to open object")

    if((ret_value = H5VL_register_id(opened_type, opened_obj, obj->vol_info, TRUE)) < 0)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to atomize object handle")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Oopen_ff() */

/*-------------------------------------------------------------------------
 * Function:	H5Oget_token
 *
 * Purpose: This function retrieves a token representing an object in
 * HDF5. This token can be used in H5Oopen_by_token(), to open the
 * object in the same transaction it was created in. If the token
 * buffer is NULL, the token size is returned in token_size.
 *
 * Return:	Success:	Non-negative with the link value in BUF.
 * 		Failure:	Negative
 *
 * Programmer:	Mohamad Chaarawi
 *              August 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Oget_token(hid_t obj_id, void *token, size_t *token_size)
{
    H5VL_object_t *obj = NULL;        /* object token of loc_id */
    herr_t ret_value = SUCCEED;              /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE3("e", "i*x*z", obj_id, token, token_size);

    /* get the file object */
    if(NULL == (obj = H5VL_get_object(obj_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid object identifier");

    if(H5VL_iod_get_token(obj->vol_obj, token, token_size) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to get object token");

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Oget_token() */


/*-------------------------------------------------------------------------
 * Function:	H5Oopen_by_token
 *
 * Purpose:	This function opens an object using its address within the
 *              HDF5 file, similar to an HDF5 hard link. The open object
 *              is identical to an object opened with H5Oopen() and should
 *              be closed with H5Oclose() or a type-specific closing
 *              function (such as H5Gclose() ).
 *
 * Return:	Success:	An open object identifier
 *		Failure:	Negative
 *
 * Programmer:	Mohamad Chaarawi
 *              August 2013
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Oopen_by_token(const void *token, hid_t trans_id, hid_t estack_id)
{
    H5TR_t *tr = NULL;
    H5_priv_request_t  *request = NULL; /* private request struct inserted in event queue */
    void **req = NULL; /* pointer to plugin generate requests (Stays NULL if plugin does not support async */
    H5I_type_t opened_type;
    void  *opened_obj = NULL;
    H5VL_t *vol_info = NULL;      /* VOL info struct */
    hid_t ret_value = FAIL;

    FUNC_ENTER_API(FAIL)
    H5TRACE3("i", "*xii", token, trans_id, estack_id);

    if(NULL == token)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no token")

    /* get the TR object */
    if(NULL == (tr = (H5TR_t *)H5I_object_verify(trans_id, H5I_TR)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a Transaction ID")

    if(estack_id != H5_EVENT_STACK_NULL) {
        /* create the private request */
        if(NULL == (request = (H5_priv_request_t *)H5MM_calloc(sizeof(H5_priv_request_t))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
        req = &request->req;
        request->vol_cls = tr->vol_cls;
    }

    if(NULL == (opened_obj = H5VL_iod_obj_open_token(token, tr, &opened_type, req)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to open object");

    /* TODO add indexing part from H5Dopen */

    if(request && *req)
        if(H5ES_insert(estack_id, request) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to insert request in event stack")

    HDassert(tr->vol_cls->value == H5_VOL_IOD);

    /* setup VOL info struct */
    if(NULL == (vol_info = H5FL_CALLOC(H5VL_t)))
	HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, FAIL, "can't allocate VL info struct")
    vol_info->vol_cls = tr->vol_cls;
    vol_info->vol_id = H5VL_IOD_g;

    if(H5I_inc_ref(vol_info->vol_id, FALSE) < 0)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTINC, FAIL, "unable to increment ref count on VOL plugin")

    if((ret_value = H5VL_register_id(opened_type, opened_obj, vol_info, TRUE)) < 0)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to atomize object handle")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Oopen_by_token() */


/*-------------------------------------------------------------------------
 * Function:	H5Olink_ff
 *
 * Purpose:	Creates a hard link from NEW_NAME to the object specified
 *		by OBJ_ID using properties defined in the Link Creation
 *              Property List LCPL.
 *
 *		This function should be used to link objects that have just
 *              been created.
 *
 *		NEW_NAME is interpreted relative to
 *		NEW_LOC_ID, which is either a file ID or a
 *		group ID.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Mohamad Chaarawi
 *              May 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Olink_ff(hid_t obj_id, hid_t new_loc_id, const char *new_name, hid_t lcpl_id,
           hid_t lapl_id, hid_t trans_id, hid_t estack_id)
{
    H5VL_object_t *obj1 = NULL;        /* object token of loc_id */
    H5VL_object_t *obj2 = NULL;        /* object token of loc_id */
    H5VL_loc_params_t loc_params1;
    H5VL_loc_params_t loc_params2;
    H5_priv_request_t  *request = NULL; /* private request struct inserted in event queue */
    void **req = NULL; /* pointer to plugin generate requests (Stays NULL if plugin does not support async */
    H5P_genplist_t *plist;      /* Property list pointer */
    hid_t dxpl_id = H5P_DATASET_XFER_DEFAULT; /* transfer property list to pass to the VOL plugin */
    herr_t         ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE7("e", "ii*siiii", obj_id, new_loc_id, new_name, lcpl_id, lapl_id,
             trans_id, estack_id);

    /* Check arguments */
    if(new_loc_id == H5L_SAME_LOC)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "cannot use H5L_SAME_LOC when only one location is specified")
    if(!new_name || !*new_name)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name specified")
/* Avoid compiler warning on 32-bit machines */
#if H5_SIZEOF_SIZE_T > H5_SIZEOF_INT32_T
    if(HDstrlen(new_name) > H5L_MAX_LINK_NAME_LEN)
        HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, FAIL, "name too long")
#endif /* H5_SIZEOF_SIZE_T > H5_SIZEOF_INT32_T */
    if(lcpl_id != H5P_DEFAULT && (TRUE != H5P_isa_class(lcpl_id, H5P_LINK_CREATE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a link creation property list")

    /* Check the group access property list */
    if(H5P_DEFAULT == lcpl_id)
        lcpl_id = H5P_LINK_CREATE_DEFAULT;

    loc_params1.type = H5VL_OBJECT_BY_SELF;
    loc_params1.obj_type = H5I_get_type(obj_id);

    loc_params2.type = H5VL_OBJECT_BY_NAME;
    loc_params2.obj_type = H5I_get_type(new_loc_id);
    loc_params2.loc_data.loc_by_name.name = new_name;
    loc_params2.loc_data.loc_by_name.lapl_id = lapl_id;

    if(H5L_SAME_LOC != obj_id) {
        /* get the file object */
        if(NULL == (obj1 = H5VL_get_object(obj_id)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid file identifier")
    }
    if(H5L_SAME_LOC != new_loc_id) {
        /* get the file object */
        if(NULL == (obj2 = H5VL_get_object(new_loc_id)))
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
    if(H5P_set(plist, H5VL_PROP_LINK_TARGET, &obj1->vol_obj) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't set property value for target id")
    if(H5P_set(plist, H5VL_PROP_LINK_TARGET_LOC_PARAMS, &loc_params1) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't set property value for target id")

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
    if(H5VL_link_create(H5VL_LINK_CREATE_HARD, obj2->vol_obj, loc_params2, 
                        (obj1!=NULL ? obj1->vol_info->vol_cls : obj2->vol_info->vol_cls),
                        lcpl_id, lapl_id, dxpl_id, req) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to create link")

    if(request && *req)
        if(H5ES_insert(estack_id, request) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to insert request in event stack")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Olink_ff() */


/*-------------------------------------------------------------------------
 * Function:	H5Oexists_by_name_ff
 *
 * Purpose:	Determine if a linked-to object exists
 *
 * Return:	Success:	TRUE/FALSE
 * 		Failure:	Negative
 *
 * Programmer:	Mohamad Chaarawi
 *              May 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Oexists_by_name_ff(hid_t loc_id, const char *name, htri_t *ret, hid_t lapl_id,
                     hid_t rcxt_id, hid_t estack_id)
{
    H5VL_object_t *obj = NULL;        /* object token of loc_id */
    hid_t dxpl_id = H5P_DATASET_XFER_DEFAULT; /* transfer property list to pass to the VOL plugin */
    H5P_genplist_t *plist;     /* Property list pointer */
    H5_priv_request_t *request = NULL; /* private request struct inserted in event queue */
    void **req = NULL; /* pointer to plugin generate requests (Stays NULL if plugin does not support async */
    H5VL_loc_params_t loc_params;
    herr_t  ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE6("e", "i*s*tiii", loc_id, name, ret, lapl_id, rcxt_id, estack_id);

    /* Check args */
    if(!name || !*name)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name")
    if(H5P_DEFAULT == lapl_id)
        lapl_id = H5P_LINK_ACCESS_DEFAULT;
    else
        if(TRUE != H5P_isa_class(lapl_id, H5P_LINK_ACCESS))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not link access property list ID")

    loc_params.type = H5VL_OBJECT_BY_NAME;
    loc_params.loc_data.loc_by_name.name = name;
    loc_params.loc_data.loc_by_name.lapl_id = lapl_id;
    loc_params.obj_type = H5I_get_type(loc_id);

    /* get the file object */
    if(NULL == (obj = H5VL_get_object(loc_id)))
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

    /* change the ref count through the VOL */
    if(H5VL_object_specific(obj->vol_obj, loc_params, obj->vol_info->vol_cls, H5VL_OBJECT_EXISTS, 
                            dxpl_id, req, (htri_t *)ret) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTGET, FAIL, "unable to determine if '%s' exists", name)

    if(request && *req)
        if(H5ES_insert(estack_id, request) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to insert request in event stack")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Oexists_by_name_ff() */


/*-------------------------------------------------------------------------
 * Function:	H5Oset_comment_ff
 *
 * Purpose:     Gives the specified object a comment.  The COMMENT string
 *		should be a null terminated string.  An object can have only
 *		one comment at a time.  Passing NULL for the COMMENT argument
 *		will remove the comment property from the object.
 *
 * Note:	Deprecated in favor of using attributes on objects
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Mohamad Chaarawi
 *              May 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Oset_comment_ff(hid_t obj_id, const char *comment, hid_t trans_id, hid_t estack_id)
{
    H5VL_object_t *obj = NULL;        /* object token of loc_id */
    hid_t dxpl_id = H5P_DATASET_XFER_DEFAULT; /* transfer property list to pass to the VOL plugin */
    H5P_genplist_t *plist;     /* Property list pointer */
    H5_priv_request_t *request = NULL; /* private request struct inserted in event queue */
    void **req = NULL; /* pointer to plugin generate requests (Stays NULL if plugin does not support async */
    H5VL_loc_params_t loc_params;
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE4("e", "i*sii", obj_id, comment, trans_id, estack_id);

    loc_params.type = H5VL_OBJECT_BY_SELF;
    loc_params.obj_type = H5I_get_type(obj_id);

    /* get the file object */
    if(NULL == (obj = H5VL_get_object(obj_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid file identifier")

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
        request->vol_cls = obj->vol_info->vol_cls;
    }

    /* set comment on object through the VOL */
    if(H5VL_object_optional(obj->vol_obj, obj->vol_info->vol_cls, dxpl_id, req, 
                            H5VL_OBJECT_SET_COMMENT, loc_params, comment) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to set comment value")

    if(request && *req)
        if(H5ES_insert(estack_id, request) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to insert request in event stack")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Oset_comment_ff() */


/*-------------------------------------------------------------------------
 * Function:	H5Oset_comment_by_name_ff
 *
 * Purpose:     Gives the specified object a comment.  The COMMENT string
 *		should be a null terminated string.  An object can have only
 *		one comment at a time.  Passing NULL for the COMMENT argument
 *		will remove the comment property from the object.
 *
 * Note:	Deprecated in favor of using attributes on objects
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Mohamad Chaarawi
 *              May 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Oset_comment_by_name_ff(hid_t loc_id, const char *name, const char *comment,
                          hid_t lapl_id, hid_t trans_id, hid_t estack_id)
{
    H5VL_object_t *obj = NULL;        /* object token of loc_id */
    hid_t dxpl_id = H5P_DATASET_XFER_DEFAULT; /* transfer property list to pass to the VOL plugin */
    H5P_genplist_t *plist;     /* Property list pointer */
    H5_priv_request_t *request = NULL; /* private request struct inserted in event queue */
    void **req = NULL; /* pointer to plugin generate requests (Stays NULL if plugin does not support async */
    H5VL_loc_params_t loc_params;
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE6("e", "i*s*siii", loc_id, name, comment, lapl_id, trans_id, estack_id);

    /* Check args */
    if(!name || !*name)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name")
    if(H5P_DEFAULT == lapl_id)
        lapl_id = H5P_LINK_ACCESS_DEFAULT;
    else
        if(TRUE != H5P_isa_class(lapl_id, H5P_LINK_ACCESS))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not link access property list ID")

    loc_params.type = H5VL_OBJECT_BY_NAME;
    loc_params.loc_data.loc_by_name.name = name;
    loc_params.loc_data.loc_by_name.lapl_id = lapl_id;
    loc_params.obj_type = H5I_get_type(loc_id);

    /* get the file object */
    if(NULL == (obj = H5VL_get_object(loc_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid file identifier")

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
        request->vol_cls = obj->vol_info->vol_cls;
    }

    /* set comment on object through the VOL */
    if(H5VL_object_optional(obj->vol_obj, obj->vol_info->vol_cls, dxpl_id, req, 
                            H5VL_OBJECT_SET_COMMENT, loc_params, comment) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to set comment value")

    if(request && *req)
        if(H5ES_insert(estack_id, request) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to insert request in event stack")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Oset_comment_by_name_ff() */


/*-------------------------------------------------------------------------
 * Function:	H5Oget_comment_ff
 *
 * Purpose:	Retrieve comment for an object.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Mohamad Chaarawi
 *              May 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Oget_comment_ff(hid_t loc_id, char *comment, size_t bufsize, ssize_t *ret,
                  hid_t rcxt_id, hid_t estack_id)
{
    H5VL_object_t *obj = NULL;        /* object token of loc_id */
    hid_t dxpl_id = H5P_DATASET_XFER_DEFAULT; /* transfer property list to pass to the VOL plugin */
    H5P_genplist_t *plist;     /* Property list pointer */
    H5_priv_request_t *request = NULL; /* private request struct inserted in event queue */
    void **req = NULL; /* pointer to plugin generate requests (Stays NULL if plugin does not support async */
    H5VL_loc_params_t loc_params;
    herr_t ret_value = SUCCEED;              /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE6("e", "i*sz*Zsii", loc_id, comment, bufsize, ret, rcxt_id, estack_id);

    loc_params.type = H5VL_OBJECT_BY_SELF;
    loc_params.obj_type = H5I_get_type(loc_id);

    /* get the vol object */
    if(NULL == (obj = H5VL_get_object(loc_id)))
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

    if(H5VL_object_optional(obj->vol_obj, obj->vol_info->vol_cls, dxpl_id, req, 
                            H5VL_OBJECT_GET_COMMENT, loc_params, comment, bufsize, ret) < 0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTGET, FAIL, "unable to get object comment")

    if(request && *req)
        if(H5ES_insert(estack_id, request) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to insert request in event stack")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Oget_comment_ff() */


/*-------------------------------------------------------------------------
 * Function:	H5Oget_comment_by_name_ff
 *
 * Purpose:	Retrieve comment for an object.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Mohamad Chaarawi
 *              May 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Oget_comment_by_name_ff(hid_t loc_id, const char *name, char *comment, size_t bufsize,
                          ssize_t *ret, hid_t lapl_id, hid_t rcxt_id, hid_t estack_id)
{
    H5VL_object_t *obj = NULL;        /* object token of loc_id */
    hid_t dxpl_id = H5P_DATASET_XFER_DEFAULT; /* transfer property list to pass to the VOL plugin */
    H5P_genplist_t *plist;     /* Property list pointer */
    H5_priv_request_t *request = NULL; /* private request struct inserted in event queue */
    void **req = NULL; /* pointer to plugin generate requests (Stays NULL if plugin does not support async */
    H5VL_loc_params_t loc_params;
    herr_t ret_value = SUCCEED;              /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE8("e", "i*s*sz*Zsiii", loc_id, name, comment, bufsize, ret, lapl_id,
             rcxt_id, estack_id);

    /* Check args */
    if(!name || !*name)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name")
    if(H5P_DEFAULT == lapl_id)
        lapl_id = H5P_LINK_ACCESS_DEFAULT;
    else
        if(TRUE != H5P_isa_class(lapl_id, H5P_LINK_ACCESS))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not link access property list ID")

    loc_params.type = H5VL_OBJECT_BY_NAME;
    loc_params.loc_data.loc_by_name.name = name;
    loc_params.loc_data.loc_by_name.lapl_id = lapl_id;
    loc_params.obj_type = H5I_get_type(loc_id);

    /* get the file object */
    if(NULL == (obj = H5VL_get_object(loc_id)))
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

    if(H5VL_object_optional(obj->vol_obj, obj->vol_info->vol_cls, dxpl_id, req, 
                            H5VL_OBJECT_GET_COMMENT, loc_params, comment, bufsize, &ret) < 0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTGET, FAIL, "unable to get object info")

    if(request && *req)
        if(H5ES_insert(estack_id, request) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to insert request in event stack")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Oget_comment_by_name_ff() */


/*-------------------------------------------------------------------------
 * Function:	H5Oget_info_ff
 *
 * Purpose:	Retrieve information about an object.
 *
 * Return:	Success:	Non-negative
 *		Failure:	Negative
 *
 * Programmer:	Mohamad Chaarawi
 *              August 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Oget_info_ff(hid_t loc_id, H5O_ff_info_t *oinfo, hid_t rcxt_id, hid_t estack_id)
{
    H5VL_object_t *obj = NULL;        /* object token of loc_id */
    hid_t dxpl_id = H5P_DATASET_XFER_DEFAULT; /* transfer property list to pass to the VOL plugin */
    H5P_genplist_t *plist;     /* Property list pointer */
    H5_priv_request_t *request = NULL; /* private request struct inserted in event queue */
    void **req = NULL; /* pointer to plugin generate requests (Stays NULL if plugin does not support async */
    H5VL_loc_params_t loc_params;
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE4("e", "i*xii", loc_id, oinfo, rcxt_id, estack_id);

    /* Check args */
    if(!oinfo)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no info struct")

    loc_params.type = H5VL_OBJECT_BY_SELF;
    loc_params.obj_type = H5I_get_type(loc_id);

    /* get the file object */
    if(NULL == (obj = H5VL_get_object(loc_id)))
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

    /* Get the group info through the VOL using the location token */
    if(H5VL_object_optional(obj->vol_obj, obj->vol_info->vol_cls, dxpl_id, req, 
                            H5VL_OBJECT_GET_INFO, loc_params, oinfo) < 0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTGET, FAIL, "unable to get group info")

    if(request && *req)
        if(H5ES_insert(estack_id, request) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to insert request in event stack")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Oget_info_ff() */


/*-------------------------------------------------------------------------
 * Function:	H5Oget_info_by_name_ff
 *
 * Purpose:	Retrieve information about an object.
 *
 * Return:	Success:	Non-negative
 *		Failure:	Negative
 *
 * Programmer:	Mohamad Chaarawi
 *              August 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Oget_info_by_name_ff(hid_t loc_id, const char *name, H5O_ff_info_t *oinfo, 
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
    H5TRACE6("e", "i*s*xiii", loc_id, name, oinfo, lapl_id, rcxt_id, estack_id);

    /* Check args */
    if(!name || !*name)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name")
    if(!oinfo)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no info struct")
    if(H5P_DEFAULT == lapl_id)
        lapl_id = H5P_LINK_ACCESS_DEFAULT;
    else
        if(TRUE != H5P_isa_class(lapl_id, H5P_LINK_ACCESS))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not link access property list ID")

    loc_params.type = H5VL_OBJECT_BY_NAME;
    loc_params.loc_data.loc_by_name.name = name;
    loc_params.loc_data.loc_by_name.lapl_id = lapl_id;
    loc_params.obj_type = H5I_get_type(loc_id);

    /* get the file object */
    if(NULL == (obj = H5VL_get_object(loc_id)))
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

    /* Get the group info through the VOL using the location token */
    if(H5VL_object_optional(obj->vol_obj, obj->vol_info->vol_cls, dxpl_id, req, 
                            H5VL_OBJECT_GET_INFO, loc_params, oinfo) < 0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTGET, FAIL, "unable to get group info")

    if(request && *req)
        if(H5ES_insert(estack_id, request) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to insert request in event stack")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Oget_info_by_name_ff() */


/*-------------------------------------------------------------------------
 * Function:	H5Ovisit
 *
 * Purpose:	FFwd version of H5Ovisit. 
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
H5Ovisit_ff(hid_t obj_id, H5_index_t idx_type, H5_iter_order_t order,
            H5O_iterate_ff_t op, void *op_data, hid_t rcxt_id, hid_t H5_ATTR_UNUSED estack_id)
{
    H5VL_object_t    *obj = NULL;        /* object token of loc_id */
    H5VL_loc_params_t loc_params;
    hid_t dxpl_id = H5P_DATASET_XFER_DEFAULT; /* transfer property list to pass to the VOL plugin */
    H5P_genplist_t *plist;     /* Property list pointer */
    herr_t      ret_value;              /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE5("e", "iIiIox*x", obj_id, idx_type, order, op, op_data);

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
                                         H5VL_OBJECT_VISIT, dxpl_id, H5_REQUEST_NULL, 
                                         idx_type, order, op, op_data)) < 0)
	HGOTO_ERROR(H5E_OHDR, H5E_BADITER, FAIL, "link iteration failed")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Ovisit_ff() */


/*-------------------------------------------------------------------------
 * Function:	H5Ovisit_by_name_ff
 *
 * Purpose:	FFwd version of H5Ovisit_by_name. 
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
H5Ovisit_by_name_ff(hid_t loc_id, const char *obj_name, H5_index_t idx_type,
                    H5_iter_order_t order, H5O_iterate_ff_t op, void *op_data, 
                    hid_t lapl_id, hid_t rcxt_id, hid_t H5_ATTR_UNUSED estack_id)
{
    H5VL_object_t    *obj = NULL;        /* object token of loc_id */
    H5VL_loc_params_t loc_params; 
    hid_t dxpl_id = H5P_DATASET_XFER_DEFAULT; /* transfer property list to pass to the VOL plugin */
    H5P_genplist_t *plist;     /* Property list pointer */
    herr_t      ret_value;              /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE7("e", "i*sIiIox*xi", loc_id, obj_name, idx_type, order, op, op_data,
             lapl_id);

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
                                         H5VL_OBJECT_VISIT, dxpl_id, H5_REQUEST_NULL, 
                                         idx_type, order, op, op_data)) < 0)
	HGOTO_ERROR(H5E_OHDR, H5E_BADITER, FAIL, "link iteration failed")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Ovisit_by_name_ff() */


/*-------------------------------------------------------------------------
 * Function:	H5Oclose_ff
 *
 * Purpose:	Close an open file object.
 *
 *              This is the companion to H5Oopen. It is used to close any
 *              open object in an HDF5 file (but not IDs are that not file
 *              objects, such as property lists and dataspaces). It has
 *              the same effect as calling H5Gclose, H5Dclose, or H5Tclose.
 *
 * Return:	Success:	Non-negative
 *		Failure:	Negative
 *
 * Programmer:	Mohamad Chaarawi
 *              May 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Oclose_ff(hid_t object_id, hid_t estack_id)
{
    H5VL_object_t *obj;
    herr_t       ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "ii", object_id, estack_id);

    /* Get the type of the object and close it in the correct way */
    switch(H5I_get_type(object_id)) {
        case H5I_GROUP:
        case H5I_DATASET:
        case H5I_MAP:
            /* Check args */
            if(NULL == (obj = (H5VL_object_t *)H5I_object(object_id)))
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a valid object ID")

            /* set the async request and dxpl IDs to be passed on to the VOL layer */
            obj->close_estack_id = estack_id;
            obj->close_dxpl_id = H5AC_dxpl_id;

            if(H5I_dec_app_ref(object_id) < 0)
                HGOTO_ERROR(H5E_OHDR, H5E_CANTRELEASE, FAIL, "unable to close object");
            break;
        case H5I_DATATYPE:
            {
                H5T_t *dt = NULL;

                /* Check args */
                if(NULL == (dt = (H5T_t *)H5I_object(object_id)))
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a valid datatype ID");

                if (NULL != dt->vol_obj) {
                    /* set the async request and dxpl IDs to be passed on to the VOL layer */
                    dt->vol_obj->close_estack_id = estack_id;
                    dt->vol_obj->close_dxpl_id = H5AC_dxpl_id;
                }

                if(H5I_dec_app_ref(object_id) < 0)
                    HGOTO_ERROR(H5E_OHDR, H5E_CANTRELEASE, FAIL, "unable to close object");
                break;
            }
        case H5I_UNINIT:
        case H5I_BADID:
        case H5I_FILE:
        case H5I_DATASPACE:
        case H5I_ATTR:
        case H5I_REFERENCE:
        case H5I_VFL:
        case H5I_VOL:
        case H5I_ES:
        case H5I_RC:
        case H5I_TR:
        case H5I_QUERY:
        case H5I_VIEW:
        case H5I_GENPROP_CLS:
        case H5I_GENPROP_LST:
        case H5I_ERROR_CLASS:
        case H5I_ERROR_MSG:
        case H5I_ERROR_STACK:
        case H5I_NTYPES:
        default:
            HGOTO_ERROR(H5E_ARGS, H5E_CANTRELEASE, FAIL, "not a valid file object ID (dataset, group, or datatype)")
        break;
    } /* end switch */

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Oclose_ff() */
