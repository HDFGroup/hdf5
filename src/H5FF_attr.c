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


/*--------------------------------------------------------------------------
 NAME
    H5Acreate_ff
 PURPOSE
    Creates an attribute on an object
 RETURNS
    Non-negative on success/Negative on failure
--------------------------------------------------------------------------*/
hid_t
H5Acreate_ff(hid_t loc_id, const char *attr_name, hid_t type_id, hid_t space_id,
             hid_t acpl_id, hid_t aapl_id, hid_t trans_id, hid_t estack_id)
{
    void    *attr = NULL;       /* attr token from VOL plugin */
    H5VL_object_t *obj = NULL;        /* object token of loc_id */
    hid_t dxpl_id = H5P_DATASET_XFER_DEFAULT; /* transfer property list to pass to the VOL plugin */
    H5_priv_request_t *request = NULL; /* private request struct inserted in event queue */
    void **req = NULL; /* pointer to plugin generate requests (Stays NULL if plugin does not support async */
    H5VL_loc_params_t loc_params;
    H5P_genplist_t      *plist;            /* Property list pointer */
    hid_t		ret_value;              /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE8("i", "i*siiiiii", loc_id, attr_name, type_id, space_id, acpl_id,
             aapl_id, trans_id, estack_id);

    /* check arguments */
    if(H5I_ATTR == H5I_get_type(loc_id))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "location is not valid for an attribute")
    if(!attr_name || !*attr_name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no attribute name")

    /* Get correct property list */
    if(H5P_DEFAULT == acpl_id)
        acpl_id = H5P_ATTRIBUTE_CREATE_DEFAULT;

    /* Get the plist structure */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(acpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* set creation properties */
    if(H5P_set(plist, H5VL_PROP_ATTR_TYPE_ID, &type_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't set property value for datatype id")
    if(H5P_set(plist, H5VL_PROP_ATTR_SPACE_ID, &space_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't set property value for space id")

    loc_params.type = H5VL_OBJECT_BY_SELF;
    loc_params.obj_type = H5I_get_type(loc_id);

    /* store the transaction ID in the dxpl */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")
    if(H5P_set(plist, H5VL_TRANS_ID, &trans_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't set property value for trans_id")

    /* get the file object */
    if(NULL == (obj = H5VL_get_object(loc_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid identifier")

    if(estack_id != H5_EVENT_STACK_NULL) {
        /* create the private request */
        if(NULL == (request = (H5_priv_request_t *)H5MM_calloc(sizeof(H5_priv_request_t))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
        req = &request->req;
        request->vol_cls = obj->vol_info->vol_cls;
    }

    /* Create the attribute through the VOL */
    if(NULL == (attr = H5VL_attr_create(obj->vol_obj, loc_params, obj->vol_info->vol_cls, attr_name, 
                                        acpl_id, aapl_id, dxpl_id, req)))
	HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to create attribute")

    if(request && *req)
        if(H5ES_insert(estack_id, request) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to insert request in event stack")

    /* Get an atom for the attribute */
    if((ret_value = H5VL_register_id(H5I_ATTR, attr, obj->vol_info, TRUE)) < 0)
	HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to atomize attribute handle")

done:
    if (ret_value < 0 && attr)
        if(H5VL_attr_close(attr, obj->vol_info->vol_cls, dxpl_id, H5_REQUEST_NULL) < 0)
            HDONE_ERROR(H5E_ATTR, H5E_CLOSEERROR, FAIL, "unable to release attr")
    FUNC_LEAVE_API(ret_value)
} /* H5Acreate_ff() */


/*--------------------------------------------------------------------------
 NAME
    H5Acreate_by_name_ff
 PURPOSE
    Creates an attribute on an object
 RETURNS
    Non-negative on success/Negative on failure
--------------------------------------------------------------------------*/
hid_t
H5Acreate_by_name_ff(hid_t loc_id, const char *obj_name, const char *attr_name,
    hid_t type_id, hid_t space_id, hid_t acpl_id, hid_t aapl_id,
    hid_t lapl_id, hid_t trans_id, hid_t estack_id)
{
    void    *attr = NULL;       /* attr token from VOL plugin */
    H5VL_object_t *obj = NULL;        /* object token of loc_id */
    hid_t dxpl_id = H5P_DATASET_XFER_DEFAULT; /* transfer property list to pass to the VOL plugin */
    H5_priv_request_t *request = NULL; /* private request struct inserted in event queue */
    void **req = NULL; /* pointer to plugin generate requests (Stays NULL if plugin does not support async */
    H5P_genplist_t      *plist;            /* Property list pointer */
    H5VL_loc_params_t    loc_params;
    hid_t		 ret_value;        /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE10("i", "i*s*siiiiiii", loc_id, obj_name, attr_name, type_id, space_id,
             acpl_id, aapl_id, lapl_id, trans_id, estack_id);

    /* check arguments */
    if(H5I_ATTR == H5I_get_type(loc_id))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "location is not valid for an attribute")
    if(!obj_name || !*obj_name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no object name")
    if(!attr_name || !*attr_name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no attribute name")

    /* Get correct property list */
    if(H5P_DEFAULT == acpl_id)
        acpl_id = H5P_ATTRIBUTE_CREATE_DEFAULT;

    /* Get the plist structure */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(acpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* set creation properties */
    if(H5P_set(plist, H5VL_PROP_ATTR_TYPE_ID, &type_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't set property value for datatype id")
    if(H5P_set(plist, H5VL_PROP_ATTR_SPACE_ID, &space_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't set property value for space id")

    loc_params.type = H5VL_OBJECT_BY_NAME;
    loc_params.obj_type = H5I_get_type(loc_id);
    loc_params.loc_data.loc_by_name.name = obj_name;
    loc_params.loc_data.loc_by_name.lapl_id = lapl_id;

    /* store the transaction ID in the dxpl */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")
    if(H5P_set(plist, H5VL_TRANS_ID, &trans_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't set property value for trans_id")

    /* get the location object */
    if(NULL == (obj = H5VL_get_object(loc_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid location identifier")

    if(estack_id != H5_EVENT_STACK_NULL) {
        /* create the private request */
        if(NULL == (request = (H5_priv_request_t *)H5MM_calloc(sizeof(H5_priv_request_t))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
        req = &request->req;
        request->vol_cls = obj->vol_info->vol_cls;
    }

    /* Create the attribute through the VOL */
    if(NULL == (attr = H5VL_attr_create(obj->vol_obj, loc_params, obj->vol_info->vol_cls, attr_name, 
                                        acpl_id, aapl_id, dxpl_id, req)))
	HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to create attribute")

    if(request && *req)
        if(H5ES_insert(estack_id, request) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to insert request in event stack")

    /* Get an atom for the attribute */
    if((ret_value = H5VL_register_id(H5I_ATTR, attr, obj->vol_info, TRUE)) < 0)
	HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to atomize attribute handle")

done:
    if (ret_value < 0 && attr)
        if(H5VL_attr_close (attr, obj->vol_info->vol_cls, dxpl_id, H5_REQUEST_NULL) < 0)
            HDONE_ERROR(H5E_ATTR, H5E_CLOSEERROR, FAIL, "unable to release attr")
    FUNC_LEAVE_API(ret_value)
} /* H5Acreate_by_name_ff() */


/*--------------------------------------------------------------------------
 NAME
    H5Aopen_ff
 PURPOSE
    Opens an attribute for an object by looking up the attribute name
 RETURNS
    ID of attribute on success, negative on failure
--------------------------------------------------------------------------*/
hid_t
H5Aopen_ff(hid_t loc_id, const char *attr_name, hid_t aapl_id, 
           hid_t rcxt_id, hid_t estack_id)
{
    void    *attr = NULL;       /* attr token from VOL plugin */
    H5VL_object_t *obj = NULL;        /* object token of loc_id */
    hid_t dxpl_id = H5P_DATASET_XFER_DEFAULT; /* transfer property list to pass to the VOL plugin */
    H5P_genplist_t *plist;     /* Property list pointer */
    H5_priv_request_t *request = NULL; /* private request struct inserted in event queue */
    void **req = NULL; /* pointer to plugin generate requests (Stays NULL if plugin does not support async */
    H5VL_loc_params_t loc_params; 
    hid_t		ret_value;

    FUNC_ENTER_API(FAIL)
    H5TRACE5("i", "i*siii", loc_id, attr_name, aapl_id, rcxt_id, estack_id);

    /* check arguments */
    if(H5I_ATTR == H5I_get_type(loc_id))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "location is not valid for an attribute")
    if(!attr_name || !*attr_name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no attribute name")

    loc_params.type = H5VL_OBJECT_BY_SELF;
    loc_params.obj_type = H5I_get_type(loc_id);

    /* get the location object */
    if(NULL == (obj = H5VL_get_object(loc_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid location identifier")

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

    /* Open the attribute through the VOL */
    if(NULL == (attr = H5VL_attr_open(obj->vol_obj, loc_params, obj->vol_info->vol_cls, 
                                      attr_name, aapl_id, dxpl_id, req)))
	HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, FAIL, "unable to open attribute")

    if(request && *req)
        if(H5ES_insert(estack_id, request) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to insert request in event stack")

    /* Get an atom for the attribute */
    if((ret_value = H5VL_register_id(H5I_ATTR, attr, obj->vol_info, TRUE)) < 0)
	HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to atomize attribute handle")

done:
    if (ret_value < 0 && attr)
        if(H5VL_attr_close (attr, obj->vol_info->vol_cls, dxpl_id, H5_REQUEST_NULL) < 0)
            HDONE_ERROR(H5E_ATTR, H5E_CLOSEERROR, FAIL, "unable to release attr")
    FUNC_LEAVE_API(ret_value)
} /* H5Aopen_ff() */


/*--------------------------------------------------------------------------
 NAME
    H5Aopen_by_name_ff
 PURPOSE
    Opens an attribute for an object by looking up the attribute name
 RETURNS
    ID of attribute on success, negative on failure
--------------------------------------------------------------------------*/
hid_t
H5Aopen_by_name_ff(hid_t loc_id, const char *obj_name, const char *attr_name,
    hid_t aapl_id, hid_t lapl_id, hid_t rcxt_id, hid_t estack_id)
{
    void    *attr = NULL;       /* attr token from VOL plugin */
    H5VL_object_t *obj = NULL;        /* object token of loc_id */
    hid_t dxpl_id = H5P_DATASET_XFER_DEFAULT; /* transfer property list to pass to the VOL plugin */
    H5P_genplist_t *plist;     /* Property list pointer */
    H5_priv_request_t *request = NULL; /* private request struct inserted in event queue */
    void **req = NULL; /* pointer to plugin generate requests (Stays NULL if plugin does not support async */
    H5VL_loc_params_t   loc_params;
    hid_t		ret_value;

    FUNC_ENTER_API(FAIL)
    H5TRACE7("i", "i*s*siiii", loc_id, obj_name, attr_name, aapl_id, lapl_id,
             rcxt_id, estack_id);

    /* check arguments */
    if(H5I_ATTR == H5I_get_type(loc_id))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "location is not valid for an attribute")
    if(!obj_name || !*obj_name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no object name")
    if(!attr_name || !*attr_name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no attribute name")
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

    if(estack_id != H5_EVENT_STACK_NULL) {
        /* create the private request */
        if(NULL == (request = (H5_priv_request_t *)H5MM_calloc(sizeof(H5_priv_request_t))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
        req = &request->req;
        request->vol_cls = obj->vol_info->vol_cls;
    }

    /* Open the attribute through the VOL */
    if(NULL == (attr = H5VL_attr_open(obj->vol_obj, loc_params, obj->vol_info->vol_cls, 
                                      attr_name, aapl_id, dxpl_id, req)))
	HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, FAIL, "unable to open attribute")

    if(request && *req)
        if(H5ES_insert(estack_id, request) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to insert request in event stack")

    /* Get an atom for the attribute */
    if((ret_value = H5VL_register_id(H5I_ATTR, attr, obj->vol_info, TRUE)) < 0)
	HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to atomize attribute handle")

done:
    if (ret_value < 0 && attr)
        if(H5VL_attr_close (attr, obj->vol_info->vol_cls, dxpl_id, H5_REQUEST_NULL) < 0)
            HDONE_ERROR(H5E_ATTR, H5E_CLOSEERROR, FAIL, "unable to release attr")
    FUNC_LEAVE_API(ret_value)
} /* H5Aopen_by_name_ff() */


/*--------------------------------------------------------------------------
 NAME
    H5Awrite_ff
 PURPOSE
    Write out data to an attribute
 RETURNS
    Non-negative on success/Negative on failure
--------------------------------------------------------------------------*/
herr_t
H5Awrite_ff(hid_t attr_id, hid_t dtype_id, const void *buf, hid_t trans_id, hid_t estack_id)
{
    H5VL_object_t *attr;
    hid_t dxpl_id = H5P_DATASET_XFER_DEFAULT; /* transfer property list to pass to the VOL plugin */
    H5P_genplist_t *plist;     /* Property list pointer */
    H5_priv_request_t *request = NULL; /* private request struct inserted in event queue */
    void **req = NULL; /* pointer to plugin generate requests (Stays NULL if plugin does not support async */
    herr_t ret_value;           /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE5("e", "ii*xii", attr_id, dtype_id, buf, trans_id, estack_id);

    /* check arguments */
    if(NULL == (attr = (H5VL_object_t *)H5I_object_verify(attr_id, H5I_ATTR)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not an attribute")
    if(NULL == buf)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "null attribute buffer")

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
        request->vol_cls = attr->vol_info->vol_cls;
    }

    /* write the data through the VOL */
    if((ret_value = H5VL_attr_write(attr->vol_obj, attr->vol_info->vol_cls, 
                                    dtype_id, buf, dxpl_id, req)) < 0)
	HGOTO_ERROR(H5E_ATTR, H5E_READERROR, FAIL, "can't read data")

    if(request && *req)
        if(H5ES_insert(estack_id, request) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to insert request in event stack")

done:
    FUNC_LEAVE_API(ret_value)
} /* H5Awrite_ff() */


/*--------------------------------------------------------------------------
 NAME
    H5Aread_ff
 PURPOSE
    Read in data from an attribute
 RETURNS
    Non-negative on success/Negative on failure
--------------------------------------------------------------------------*/
herr_t
H5Aread_ff(hid_t attr_id, hid_t dtype_id, void *buf, hid_t rcxt_id, hid_t estack_id)
{
    H5VL_object_t *attr;
    hid_t dxpl_id = H5P_DATASET_XFER_DEFAULT; /* transfer property list to pass to the VOL plugin */
    H5P_genplist_t *plist;     /* Property list pointer */
    H5_priv_request_t *request = NULL; /* private request struct inserted in event queue */
    void **req = NULL; /* pointer to plugin generate requests (Stays NULL if plugin does not support async */
    herr_t ret_value;           /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE5("e", "ii*xii", attr_id, dtype_id, buf, rcxt_id, estack_id);

    /* check arguments */
    if(NULL == (attr = (H5VL_object_t *)H5I_object_verify(attr_id, H5I_ATTR)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not an attribute")
    if(NULL == buf)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "null attribute buffer")

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
        request->vol_cls = attr->vol_info->vol_cls;
    }

    /* Read the data through the VOL */
    if((ret_value = H5VL_attr_read(attr->vol_obj, attr->vol_info->vol_cls, 
                                   dtype_id, buf, dxpl_id, req)) < 0)
	HGOTO_ERROR(H5E_ATTR, H5E_READERROR, FAIL, "can't read data")

    if(request && *req)
        if(H5ES_insert(estack_id, request) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to insert request in event stack")

done:
    FUNC_LEAVE_API(ret_value)
} /* H5Aread_ff() */


/*-------------------------------------------------------------------------
 * Function:	H5Arename_ff
 *
 * Purpose:     Rename an attribute
 *
 * Return:	Success:             Non-negative
 *		Failure:             Negative
 *
 * Programmer:	Mohamad Chaarawi
 *              May 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Arename_ff(hid_t loc_id, const char *old_name, const char *new_name, 
             hid_t trans_id, hid_t estack_id)
{
    herr_t	ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE5("e", "i*s*sii", loc_id, old_name, new_name, trans_id, estack_id);

    /* check arguments */
    if(!old_name || !new_name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "name is nil")
    if(H5I_ATTR == H5I_get_type(loc_id))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "location is not valid for an attribute")

    /* Avoid thrashing things if the names are the same */
    if(HDstrcmp(old_name, new_name)) {
        H5VL_object_t *obj;
        hid_t dxpl_id = H5P_DATASET_XFER_DEFAULT; /* transfer property list to pass to the VOL plugin */
        H5P_genplist_t *plist;     /* Property list pointer */
        H5_priv_request_t *request = NULL; /* private request struct inserted in event queue */
        void **req = NULL; /* pointer to plugin generate requests (Stays NULL if plugin does not support async */
        H5VL_loc_params_t loc_params;

        loc_params.type = H5VL_OBJECT_BY_SELF;
        loc_params.obj_type = H5I_get_type(loc_id);

        /* get the location object */
        if(NULL == (obj = H5VL_get_object(loc_id)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid location identifier")

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

        /* rename the attribute info through the VOL */
        if((ret_value = H5VL_attr_specific(obj->vol_obj, loc_params, obj->vol_info->vol_cls, H5VL_ATTR_RENAME,
                                           dxpl_id, req, old_name, new_name)) < 0)
            HGOTO_ERROR(H5E_ATTR, H5E_CANTRENAME, FAIL, "can't rename attribute");

        if(request && *req)
            if(H5ES_insert(estack_id, request) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to insert request in event stack")
    }
done:
    FUNC_LEAVE_API(ret_value)
} /* H5Arename_ff() */


/*-------------------------------------------------------------------------
 * Function:	H5Arename_by_name_ff
 *
 * Purpose:     Rename an attribute
 *
 * Return:	Success:             Non-negative
 *		Failure:             Negative
 *
 * Programmer:	Mohamad Chaarawi
 *              May 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Arename_by_name_ff(hid_t loc_id, const char *obj_name, const char *old_attr_name,
                     const char *new_attr_name, hid_t lapl_id, hid_t trans_id, hid_t estack_id)
{
    herr_t	ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE7("e", "i*s*s*siii", loc_id, obj_name, old_attr_name, new_attr_name,
             lapl_id, trans_id, estack_id);

    /* check arguments */
    if(H5I_ATTR == H5I_get_type(loc_id))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "location is not valid for an attribute")
    if(!obj_name || !*obj_name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no object name")
    if(!old_attr_name || !*old_attr_name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no old attribute name")
    if(!new_attr_name || !*new_attr_name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no new attribute name")
    if(H5P_DEFAULT == lapl_id)
        lapl_id = H5P_LINK_ACCESS_DEFAULT;
    else
        if(TRUE != H5P_isa_class(lapl_id, H5P_LINK_ACCESS))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not link access property list ID")

    /* Avoid thrashing things if the names are the same */
    if(HDstrcmp(old_attr_name, new_attr_name)) {
        H5VL_object_t *obj;
        hid_t dxpl_id = H5P_DATASET_XFER_DEFAULT; /* transfer property list to pass to the VOL plugin */
        H5P_genplist_t *plist;     /* Property list pointer */
    H5_priv_request_t *request = NULL; /* private request struct inserted in event queue */
    void **req = NULL; /* pointer to plugin generate requests (Stays NULL if plugin does not support async */
        H5VL_loc_params_t loc_params;

        loc_params.type = H5VL_OBJECT_BY_NAME;
        loc_params.loc_data.loc_by_name.name = obj_name;
        loc_params.loc_data.loc_by_name.lapl_id = lapl_id;
        loc_params.obj_type = H5I_get_type(loc_id);

        /* store the transaction ID in the dxpl */
        if(NULL == (plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
            HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")
        if(H5P_set(plist, H5VL_TRANS_ID, &trans_id) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't set property value for trans_id")

        /* get the file object */
        if(NULL == (obj = H5VL_get_object(loc_id)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid file identifier")

        if(estack_id != H5_EVENT_STACK_NULL) {
            /* create the private request */
            if(NULL == (request = (H5_priv_request_t *)H5MM_calloc(sizeof(H5_priv_request_t))))
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
            req = &request->req;
            request->vol_cls = obj->vol_info->vol_cls;
        }

        /* rename the attribute info through the VOL */
        if((ret_value = H5VL_attr_specific(obj->vol_obj, loc_params, obj->vol_info->vol_cls, H5VL_ATTR_RENAME,
                                           dxpl_id, req, old_attr_name, new_attr_name)) < 0)
            HGOTO_ERROR(H5E_ATTR, H5E_CANTRENAME, FAIL, "can't rename attribute");

        if(request && *req)
            if(H5ES_insert(estack_id, request) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to insert request in event stack")
    } /* end if */

done:
    FUNC_LEAVE_API(ret_value)
} /* H5Arename_by_name_ff() */


/*--------------------------------------------------------------------------
 NAME
    H5Adelete_ff
 PURPOSE
    Deletes an attribute from a location
 RETURNS
    Non-negative on success/Negative on failure
--------------------------------------------------------------------------*/
herr_t
H5Adelete_ff(hid_t loc_id, const char *name, hid_t trans_id, hid_t estack_id)
{
    H5VL_object_t *obj;
    H5VL_loc_params_t loc_params;
    hid_t dxpl_id = H5P_DATASET_XFER_DEFAULT; /* transfer property list to pass to the VOL plugin */
    H5P_genplist_t *plist;     /* Property list pointer */
    H5_priv_request_t *request = NULL; /* private request struct inserted in event queue */
    void **req = NULL; /* pointer to plugin generate requests (Stays NULL if plugin does not support async */
    herr_t	ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE4("e", "i*sii", loc_id, name, trans_id, estack_id);

    /* check arguments */
    if(H5I_ATTR == H5I_get_type(loc_id))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "location is not valid for an attribute")
    if(!name || !*name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name")

    loc_params.type = H5VL_OBJECT_BY_SELF;
    loc_params.obj_type = H5I_get_type(loc_id);

    /* store the transaction ID in the dxpl */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")
    if(H5P_set(plist, H5VL_TRANS_ID, &trans_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't set property value for trans_id")

    /* get the dataset object */
    if(NULL == (obj = H5VL_get_object(loc_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid object identifier")

    if(estack_id != H5_EVENT_STACK_NULL) {
        /* create the private request */
        if(NULL == (request = (H5_priv_request_t *)H5MM_calloc(sizeof(H5_priv_request_t))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
        req = &request->req;
        request->vol_cls = obj->vol_info->vol_cls;
    }

    /* Delete the attribute through the VOL */
    if((ret_value = H5VL_attr_specific(obj->vol_obj, loc_params, obj->vol_info->vol_cls, H5VL_ATTR_DELETE,
                                       dxpl_id, req, name)) < 0)
	HGOTO_ERROR(H5E_ATTR, H5E_CANTDELETE, FAIL, "unable to delete attribute")

    if(request && *req)
        if(H5ES_insert(estack_id, request) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to insert request in event stack")

done:
    FUNC_LEAVE_API(ret_value)
} /* H5Adelete_ff() */


/*--------------------------------------------------------------------------
 NAME
    H5Adelete_by_name_ff
 PURPOSE
    Deletes an attribute from a location
 RETURNS
    Non-negative on success/Negative on failure
 DESCRIPTION
    This function removes the named attribute from an object.
--------------------------------------------------------------------------*/
herr_t
H5Adelete_by_name_ff(hid_t loc_id, const char *obj_name, const char *attr_name,
    hid_t lapl_id, hid_t trans_id, hid_t estack_id)
{
    H5VL_object_t *obj;
    H5VL_loc_params_t loc_params;
    hid_t dxpl_id = H5P_DATASET_XFER_DEFAULT; /* transfer property list to pass to the VOL plugin */
    H5P_genplist_t *plist;     /* Property list pointer */
    H5_priv_request_t *request = NULL; /* private request struct inserted in event queue */
    void **req = NULL; /* pointer to plugin generate requests (Stays NULL if plugin does not support async */
    herr_t	ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE6("e", "i*s*siii", loc_id, obj_name, attr_name, lapl_id, trans_id,
             estack_id);

    /* check arguments */
    if(H5I_ATTR == H5I_get_type(loc_id))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "location is not valid for an attribute")
    if(!obj_name || !*obj_name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no object name")
    if(!attr_name || !*attr_name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no attribute name")
    if(H5P_DEFAULT == lapl_id)
        lapl_id = H5P_LINK_ACCESS_DEFAULT;
    else
        if(TRUE != H5P_isa_class(lapl_id, H5P_LINK_ACCESS))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not link access property list ID")

    loc_params.type = H5VL_OBJECT_BY_NAME;
    loc_params.loc_data.loc_by_name.name = obj_name;
    loc_params.loc_data.loc_by_name.lapl_id = lapl_id;
    loc_params.obj_type = H5I_get_type(loc_id);

    /* store the transaction ID in the dxpl */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")
    if(H5P_set(plist, H5VL_TRANS_ID, &trans_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't set property value for trans_id")

    /* get the dataset object */
    if(NULL == (obj = H5VL_get_object(loc_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid object identifier")

    if(estack_id != H5_EVENT_STACK_NULL) {
        /* create the private request */
        if(NULL == (request = (H5_priv_request_t *)H5MM_calloc(sizeof(H5_priv_request_t))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
        req = &request->req;
        request->vol_cls = obj->vol_info->vol_cls;
    }

    /* Delete the attribute through the VOL */
    if((ret_value = H5VL_attr_specific(obj->vol_obj, loc_params, obj->vol_info->vol_cls, H5VL_ATTR_DELETE,
                                       dxpl_id, req, attr_name)) < 0)
	HGOTO_ERROR(H5E_ATTR, H5E_CANTDELETE, FAIL, "unable to delete attribute")

    if(request && *req)
        if(H5ES_insert(estack_id, request) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to insert request in event stack")

done:
    FUNC_LEAVE_API(ret_value)
} /* H5Adelete_by_name_ff() */


/*-------------------------------------------------------------------------
 * Function:	H5Aexists_ff
 *
 * Purpose:	Checks if an attribute with a given name exists on an opened
 *              object.
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
H5Aexists_ff(hid_t obj_id, const char *attr_name, htri_t *ret, hid_t rcxt_id, hid_t estack_id)
{
    H5VL_object_t *obj;
    H5VL_loc_params_t loc_params;
    hid_t dxpl_id = H5P_DATASET_XFER_DEFAULT; /* transfer property list to pass to the VOL plugin */
    H5P_genplist_t *plist;     /* Property list pointer */
    H5_priv_request_t *request = NULL; /* private request struct inserted in event queue */
    void **req = NULL; /* pointer to plugin generate requests (Stays NULL if plugin does not support async */
    herr_t	ret_value = SUCCEED;              /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE5("e", "i*s*tii", obj_id, attr_name, ret, rcxt_id, estack_id);

    /* check arguments */
    if(H5I_ATTR == H5I_get_type(obj_id))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "location is not valid for an attribute")
    if(!attr_name || !*attr_name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no attribute name")

    /* get the dataset object */
    if(NULL == (obj = H5VL_get_object(obj_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid object identifier")

    loc_params.type = H5VL_OBJECT_BY_SELF;
    loc_params.obj_type = H5I_get_type(obj_id);

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

    /* Check existence of attribute through the VOL */
    if(H5VL_attr_specific(obj->vol_obj, loc_params, obj->vol_info->vol_cls, H5VL_ATTR_EXISTS,
                          dxpl_id, req, attr_name, (htri_t *)ret) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTGET, FAIL, "unable to determine if attribute exists")

    if(request && *req)
        if(H5ES_insert(estack_id, request) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to insert request in event stack")

done:
    FUNC_LEAVE_API(ret_value)
} /* H5Aexists_ff() */


/*-------------------------------------------------------------------------
 * Function:	H5Aexists_by_name_ff
 *
 * Purpose:	Checks if an attribute with a given name exists on an object.
 *
 * Return:	Success:	Positive
 * 		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *              Thursday, November 1, 2007
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Aexists_by_name_ff(hid_t loc_id, const char *obj_name, const char *attr_name,
                     hid_t lapl_id, htri_t *ret, hid_t rcxt_id, hid_t estack_id)
{
    H5VL_object_t *obj;
    H5VL_loc_params_t loc_params;
    hid_t dxpl_id = H5P_DATASET_XFER_DEFAULT; /* transfer property list to pass to the VOL plugin */
    H5P_genplist_t *plist;     /* Property list pointer */
    H5_priv_request_t *request = NULL; /* private request struct inserted in event queue */
    void **req = NULL; /* pointer to plugin generate requests (Stays NULL if plugin does not support async */
    herr_t	ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE7("e", "i*s*si*tii", loc_id, obj_name, attr_name, lapl_id, ret, rcxt_id,
             estack_id);

    /* check arguments */
    if(H5I_ATTR == H5I_get_type(loc_id))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "location is not valid for an attribute")
    if(!obj_name || !*obj_name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no object name")
    if(!attr_name || !*attr_name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no attribute name")
    if(H5P_DEFAULT == lapl_id)
        lapl_id = H5P_LINK_ACCESS_DEFAULT;
    else
        if(TRUE != H5P_isa_class(lapl_id, H5P_LINK_ACCESS))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not link access property list ID")

    /* get the dataset object */
    if(NULL == (obj = H5VL_get_object(loc_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid object identifier")

    loc_params.type = H5VL_OBJECT_BY_NAME;
    loc_params.loc_data.loc_by_name.name = obj_name;
    loc_params.loc_data.loc_by_name.lapl_id = lapl_id;
    loc_params.obj_type = H5I_get_type(loc_id);

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

    /* Check existence of attribute through the VOL */
    if(H5VL_attr_specific(obj->vol_obj, loc_params, obj->vol_info->vol_cls, H5VL_ATTR_EXISTS,
                          dxpl_id, req, attr_name, (htri_t *)ret) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTGET, FAIL, "unable to determine if attribute exists")

    if(request && *req)
        if(H5ES_insert(estack_id, request) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to insert request in event stack")

done:
    FUNC_LEAVE_API(ret_value)
} /* H5Aexists_by_name_ff() */


/*--------------------------------------------------------------------------
 NAME
    H5Aiterate_ff
 PURPOSE
    FFwd version of H5Aiterate
--------------------------------------------------------------------------*/
herr_t
H5Aiterate_ff(hid_t loc_id, H5_index_t idx_type, H5_iter_order_t order,
              hsize_t *idx, H5A_operator_ff_t op, void *op_data,
              hid_t rcxt_id, hid_t H5_ATTR_UNUSED estack_id)
{
    H5VL_object_t *obj = NULL;        /* object token of loc_id */
    H5VL_loc_params_t loc_params;
    hid_t dxpl_id = H5P_DATASET_XFER_DEFAULT; /* transfer property list to pass to the VOL plugin */
    H5P_genplist_t *plist;     /* Property list pointer */
    herr_t	ret_value;      /* Return value */

    FUNC_ENTER_API(FAIL)

    /* check arguments */
    if(H5I_ATTR == H5I_get_type(loc_id))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "location is not valid for an attribute")
    if(order != H5_ITER_NATIVE)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "Only H5_ITER_NATIVE iteration order is supported")
    if(idx_type <= H5_INDEX_UNKNOWN || idx_type >= H5_INDEX_N)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid index type specified")

    loc_params.type = H5VL_OBJECT_BY_SELF;
    loc_params.obj_type = H5I_get_type(loc_id);

    /* get the loc object */
    if(NULL == (obj = H5VL_get_object(loc_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid location identifier")

    /* store the transaction ID in the dxpl */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")
    if(H5P_set(plist, H5VL_CONTEXT_ID, &rcxt_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't set property value for rcxt_id")

    /* iterate over the links through the VOL */
    if((ret_value = H5VL_attr_specific(obj->vol_obj, loc_params, obj->vol_info->vol_cls, H5VL_ATTR_ITER,
                                       dxpl_id, H5_REQUEST_NULL, idx_type, order, idx, op, op_data)) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_BADITER, FAIL, "attribute iteration failed")

done:
    FUNC_LEAVE_API(ret_value)
} /* H5Aiterate_ff() */


/*--------------------------------------------------------------------------
 NAME
    H5Aiterate_by_name_ff
 PURPOSE
    FFwd version of H5Aiterate_by_name_ff
--------------------------------------------------------------------------*/
herr_t
H5Aiterate_by_name_ff(hid_t loc_id, const char *obj_name, H5_index_t idx_type,
                      H5_iter_order_t order, hsize_t *idx, H5A_operator_ff_t op, void *op_data,
                      hid_t lapl_id, hid_t rcxt_id, hid_t H5_ATTR_UNUSED estack_id)
{
    H5VL_object_t *obj = NULL;        /* object token of loc_id */
    H5VL_loc_params_t loc_params;
    hid_t dxpl_id = H5P_DATASET_XFER_DEFAULT; /* transfer property list to pass to the VOL plugin */
    H5P_genplist_t *plist;     /* Property list pointer */
    herr_t   ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_API(FAIL)

    /* check arguments */
    if(H5I_ATTR == H5I_get_type(loc_id))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "location is not valid for an attribute")
    if(!obj_name || !*obj_name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no object name")
    if(idx_type <= H5_INDEX_UNKNOWN || idx_type >= H5_INDEX_N)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid index type specified")
    if(order != H5_ITER_NATIVE)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "Only H5_ITER_NATIVE iteration order is supported")
    if(H5P_DEFAULT == lapl_id)
        lapl_id = H5P_LINK_ACCESS_DEFAULT;
    else
        if(TRUE != H5P_isa_class(lapl_id, H5P_LINK_ACCESS))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not link access property list ID")

    loc_params.type = H5VL_OBJECT_BY_NAME;
    loc_params.obj_type = H5I_get_type(loc_id);
    loc_params.loc_data.loc_by_name.name = obj_name;
    loc_params.loc_data.loc_by_name.lapl_id = lapl_id;

    /* get the loc object */
    if(NULL == (obj = H5VL_get_object(loc_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid location identifier")

    /* store the transaction ID in the dxpl */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")
    if(H5P_set(plist, H5VL_CONTEXT_ID, &rcxt_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't set property value for rcxt_id")

    /* iterate over the links through the VOL */
    if((ret_value = H5VL_attr_specific(obj->vol_obj, loc_params, obj->vol_info->vol_cls, H5VL_ATTR_ITER,
                                       dxpl_id, H5_REQUEST_NULL, idx_type, order, idx, op, op_data)) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_BADITER, FAIL, "attribute iteration failed")

done:
    FUNC_LEAVE_API(ret_value)
} /* H5Aiterate_by_name_ff() */


/*--------------------------------------------------------------------------
 NAME
    H5Aclose_ff
 PURPOSE
    Close an attribute ID
 RETURNS
    Non-negative on success/Negative on failure

 DESCRIPTION
        This function releases an attribute from use.  Further use of the
    attribute ID will result in undefined behavior.
--------------------------------------------------------------------------*/
herr_t
H5Aclose_ff(hid_t attr_id, hid_t estack_id)
{
    H5VL_object_t *obj;
    herr_t ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "ii", attr_id, estack_id);

    /* Check args */
    if(NULL == (obj = (H5VL_object_t *)H5I_object_verify(attr_id, H5I_ATTR)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not an attribute ID")

    /* set the async request and dxpl IDs to be passed on to the VOL layer */
    obj->close_estack_id = estack_id;
    obj->close_dxpl_id = H5AC_dxpl_id;

    /* Decrement references to that atom (and close it) */
    if(H5I_dec_app_ref(attr_id) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTDEC, FAIL, "can't close attribute")

done:
    FUNC_LEAVE_API(ret_value)
} /* H5Aclose_ff() */
