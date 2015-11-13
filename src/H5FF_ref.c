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

static herr_t H5R_print_token(const void *token);

static herr_t H5R_encode(H5R_type_t ref_type, void *buf, size_t *nalloc, void *_obj, 
                         hid_t space_id, const char *attrname);
static herr_t H5R_encode_ext(H5R_type_t ref_type, void *buf, size_t *nalloc, const char *filename, 
                             const char *pathname, hid_t space_id, const char *attrname);


/*-------------------------------------------------------------------------
 * Function:	H5Rcreate_object_ff
 *
 * Purpose:	Create an object reference (FF)
 *
 * Return:	Success:	0
 *		Failure:	FAIL
 *
 * Programmer:	Mohamad Chaarawi
 *		November, 2015
 *
 *-------------------------------------------------------------------------
 */
herr_t 
H5Rcreate_object_ff(href_ff_t *ref, hid_t loc_id, const char *name, hid_t lapl_id, 
                    hid_t rcxt_id, hid_t H5_ATTR_UNUSED estack_id)
{
    H5VL_object_t *obj = NULL;
    H5VL_loc_params_t loc_params;
    hid_t dxpl_id = H5P_DATASET_XFER_DEFAULT;
    H5P_genplist_t *plist;
    hbool_t obj_opened = FALSE;
    H5I_type_t  opened_type;
    H5VL_object_t    *opened_obj = NULL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)

    /* Check args */
    if(ref == NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid reference pointer")
    if(!name || !*name)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name given")

    if(H5P_DEFAULT == lapl_id)
        lapl_id = H5P_LINK_ACCESS_DEFAULT;
    else
        if(TRUE != H5P_isa_class(lapl_id, H5P_LINK_ACCESS))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not link access property list ID")

    /* get the location object */
    if(NULL == (obj = H5VL_get_object(loc_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid file identifier");

    /* Need to open the object if a path to it is given */
    if(strcmp(name, ".")) {
        loc_params.type = H5VL_OBJECT_BY_NAME;
        loc_params.loc_data.loc_by_name.name = name;
        loc_params.loc_data.loc_by_name.lapl_id = lapl_id;
        loc_params.obj_type = H5I_get_type(loc_id);

        /* store the transaction ID in the dxpl */
        if(NULL == (plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
            HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");
        if(H5P_set(plist, H5VL_CONTEXT_ID, &rcxt_id) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't set property value for rcxt_id");

        /* Open the object through the VOL */
        if(NULL == (opened_obj = (H5VL_object_t *)H5VL_object_open(obj->vol_obj, loc_params, 
                                                                   obj->vol_info->vol_cls, &opened_type, 
                                                                   dxpl_id, H5_REQUEST_NULL)))
            HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, FAIL, "unable to open object");

        obj_opened = TRUE;
    }
    else {
        opened_obj = (H5VL_object_t *)obj->vol_obj;
    }

    if(H5R_encode(H5R_OBJECT, NULL, &ref->buf_size, opened_obj, FAIL, NULL) < 0)
        HGOTO_ERROR(H5E_REFERENCE, H5E_CANTENCODE, FAIL, "Can't determine buffer size to encode reference");

    if(NULL == (ref->buf = HDmalloc(ref->buf_size)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate reference buffer");

    if(H5R_encode(H5R_OBJECT, ref->buf, &ref->buf_size, opened_obj, FAIL, NULL) < 0)
        HGOTO_ERROR(H5E_REFERENCE, H5E_CANTENCODE, FAIL, "Can't determine buffer size to encode reference");

    ref->ref_type = H5R_OBJECT;

done:
    if(obj_opened) {
        switch(opened_type) {
        case H5I_DATASET:
            if(H5VL_iod_dataset_close(opened_obj, dxpl_id, H5_REQUEST_NULL) < 0)
               HDONE_ERROR(H5E_REFERENCE, H5E_CANTCLOSEOBJ, FAIL, "Can't close object");
            break;
        case H5I_DATATYPE:
            if(H5VL_iod_datatype_close(opened_obj, dxpl_id, H5_REQUEST_NULL) < 0)
               HDONE_ERROR(H5E_REFERENCE, H5E_CANTCLOSEOBJ, FAIL, "Can't close object");
            break;
        case H5I_MAP:
            if(H5VL_iod_map_close(opened_obj, H5_REQUEST_NULL) < 0)
               HDONE_ERROR(H5E_REFERENCE, H5E_CANTCLOSEOBJ, FAIL, "Can't close object");
            break;
        case H5I_GROUP:
            if(H5VL_iod_group_close(opened_obj, dxpl_id, H5_REQUEST_NULL) < 0)
               HDONE_ERROR(H5E_REFERENCE, H5E_CANTCLOSEOBJ, FAIL, "Can't close object");
            break;
        case H5I_UNINIT:
        case H5I_BADID:
        case H5I_FILE:
        case H5I_ATTR:
        case H5I_DATASPACE:
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
            HDONE_ERROR(H5E_REFERENCE, H5E_CANTCLOSEOBJ, FAIL, "invalid object type")
        }
    }
    FUNC_LEAVE_API(ret_value)
} /* H5Rcreate_object_ff */


/*-------------------------------------------------------------------------
 * Function:	H5Rcreate_region_ff
 *
 * Purpose:	Create a region reference (FF)
 *
 * Return:	Success:	0
 *		Failure:	FAIL
 *
 * Programmer:	Mohamad Chaarawi
 *		November, 2015
 *
 *-------------------------------------------------------------------------
 */
herr_t H5Rcreate_region_ff(href_ff_t *ref, hid_t loc_id, const char *name, hid_t space_id, hid_t lapl_id,
                           hid_t rcxt_id, hid_t H5_ATTR_UNUSED estack_id)
{
    H5VL_object_t *obj = NULL;
    H5VL_loc_params_t loc_params;
    hid_t dxpl_id = H5P_DATASET_XFER_DEFAULT;
    H5P_genplist_t *plist;
    hbool_t obj_opened = FALSE;
    H5I_type_t  opened_type;
    H5VL_object_t    *opened_obj = NULL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)

    /* Check args */
    if(ref == NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid reference pointer")
    if(!name || !*name)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name given")
    if(space_id == (-1))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "reference region dataspace id must be valid")

    if(H5P_DEFAULT == lapl_id)
        lapl_id = H5P_LINK_ACCESS_DEFAULT;
    else
        if(TRUE != H5P_isa_class(lapl_id, H5P_LINK_ACCESS))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not link access property list ID")

    /* get the location object */
    if(NULL == (obj = H5VL_get_object(loc_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid file identifier");

    /* Need to open the object if a path to it is given */
    if(strcmp(name, ".")) {
        loc_params.type = H5VL_OBJECT_BY_NAME;
        loc_params.loc_data.loc_by_name.name = name;
        loc_params.loc_data.loc_by_name.lapl_id = lapl_id;
        loc_params.obj_type = H5I_get_type(loc_id);

        /* store the transaction ID in the dxpl */
        if(NULL == (plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
            HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");
        if(H5P_set(plist, H5VL_CONTEXT_ID, &rcxt_id) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't set property value for rcxt_id");

        /* Open the object through the VOL */
        if(NULL == (opened_obj = (H5VL_object_t *)H5VL_object_open(obj->vol_obj, loc_params, 
                                                                   obj->vol_info->vol_cls, &opened_type, 
                                                                   dxpl_id, H5_REQUEST_NULL)))
            HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, FAIL, "unable to open object");

        obj_opened = TRUE;

        if(H5I_DATASET != opened_type)
            HGOTO_ERROR(H5E_REFERENCE, H5E_CANTOPENOBJ, FAIL, "Object is not a Dataset")
    }
    else {
        opened_obj = (H5VL_object_t *)obj->vol_obj;
    }

    if(H5R_encode(H5R_DATASET_REGION, NULL, &ref->buf_size, opened_obj, space_id, NULL) < 0)
        HGOTO_ERROR(H5E_REFERENCE, H5E_CANTENCODE, FAIL, "Can't determine buffer size to encode reference");

    if(NULL == (ref->buf = HDmalloc(ref->buf_size)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate reference buffer");

    if(H5R_encode(H5R_DATASET_REGION, ref->buf, &ref->buf_size, opened_obj, space_id, NULL) < 0)
        HGOTO_ERROR(H5E_REFERENCE, H5E_CANTENCODE, FAIL, "Can't determine buffer size to encode reference");

    ref->ref_type = H5R_DATASET_REGION;
done:
    if(obj_opened) {
        if(H5VL_iod_dataset_close(opened_obj, dxpl_id, H5_REQUEST_NULL) < 0)
           HDONE_ERROR(H5E_REFERENCE, H5E_CANTCLOSEOBJ, FAIL, "Can't close object");
    }
    FUNC_LEAVE_API(ret_value)
} /* H5Rcreate_region_ff */


/*-------------------------------------------------------------------------
 * Function:	H5Rcreate_attr_ff
 *
 * Purpose:	Create an attribute reference (FF)
 *
 * Return:	Success:	0
 *		Failure:	FAIL
 *
 * Programmer:	Mohamad Chaarawi
 *		November, 2015
 *
 *-------------------------------------------------------------------------
 */
herr_t H5Rcreate_attr_ff(href_ff_t *ref, hid_t loc_id, const char *name, const char *attr_name, hid_t lapl_id,
                         hid_t rcxt_id, hid_t H5_ATTR_UNUSED estack_id)
{
    H5VL_object_t *obj = NULL;
    H5VL_loc_params_t loc_params;
    hid_t dxpl_id = H5P_DATASET_XFER_DEFAULT;
    H5P_genplist_t *plist;
    hbool_t obj_opened = FALSE;
    H5I_type_t  opened_type;
    H5VL_object_t    *opened_obj = NULL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)

    /* Check args */
    if(ref == NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid reference pointer")
    if(!name || !*name)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name given")
    if(!attr_name || !*attr_name)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no attribute name given")

    if(H5P_DEFAULT == lapl_id)
        lapl_id = H5P_LINK_ACCESS_DEFAULT;
    else
        if(TRUE != H5P_isa_class(lapl_id, H5P_LINK_ACCESS))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not link access property list ID")

    /* get the location object */
    if(NULL == (obj = H5VL_get_object(loc_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid file identifier");

    /* Need to open the object if a path to it is given */
    if(strcmp(name, ".")) {
        loc_params.type = H5VL_OBJECT_BY_NAME;
        loc_params.loc_data.loc_by_name.name = name;
        loc_params.loc_data.loc_by_name.lapl_id = lapl_id;
        loc_params.obj_type = H5I_get_type(loc_id);

        /* store the transaction ID in the dxpl */
        if(NULL == (plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
            HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");
        if(H5P_set(plist, H5VL_CONTEXT_ID, &rcxt_id) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't set property value for rcxt_id");

        /* Open the object through the VOL */
        if(NULL == (opened_obj = (H5VL_object_t *)H5VL_object_open(obj->vol_obj, loc_params, 
                                                                   obj->vol_info->vol_cls, &opened_type, 
                                                                   dxpl_id, H5_REQUEST_NULL)))
            HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, FAIL, "unable to open object");

        obj_opened = TRUE;
    }
    else {
        opened_obj = (H5VL_object_t *)obj->vol_obj;
    }

    if(H5R_encode(H5R_ATTR, NULL, &ref->buf_size, opened_obj, FAIL, attr_name) < 0)
        HGOTO_ERROR(H5E_REFERENCE, H5E_CANTENCODE, FAIL, "Can't determine buffer size to encode reference");

    if(NULL == (ref->buf = HDmalloc(ref->buf_size)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate reference buffer");

    if(H5R_encode(H5R_ATTR, ref->buf, &ref->buf_size, opened_obj, FAIL, attr_name) < 0)
        HGOTO_ERROR(H5E_REFERENCE, H5E_CANTENCODE, FAIL, "Can't determine buffer size to encode reference");

    ref->ref_type = H5R_ATTR;
done:
    if(obj_opened) {
        switch(opened_type) {
        case H5I_DATASET:
            if(H5VL_iod_dataset_close(opened_obj, dxpl_id, H5_REQUEST_NULL) < 0)
               HDONE_ERROR(H5E_REFERENCE, H5E_CANTCLOSEOBJ, FAIL, "Can't close object");
            break;
        case H5I_DATATYPE:
            if(H5VL_iod_datatype_close(opened_obj, dxpl_id, H5_REQUEST_NULL) < 0)
               HDONE_ERROR(H5E_REFERENCE, H5E_CANTCLOSEOBJ, FAIL, "Can't close object");
            break;
        case H5I_MAP:
            if(H5VL_iod_map_close(opened_obj, H5_REQUEST_NULL) < 0)
               HDONE_ERROR(H5E_REFERENCE, H5E_CANTCLOSEOBJ, FAIL, "Can't close object");
            break;
        case H5I_GROUP:
            if(H5VL_iod_group_close(opened_obj, dxpl_id, H5_REQUEST_NULL) < 0)
               HDONE_ERROR(H5E_REFERENCE, H5E_CANTCLOSEOBJ, FAIL, "Can't close object");
            break;
        case H5I_UNINIT:
        case H5I_BADID:
        case H5I_FILE:
        case H5I_ATTR:
        case H5I_DATASPACE:
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
            HDONE_ERROR(H5E_REFERENCE, H5E_CANTCLOSEOBJ, FAIL, "invalid object type")
        }
    }
    FUNC_LEAVE_API(ret_value)
} /* H5Rcreate_attr_ff */


/*-------------------------------------------------------------------------
 * Function:	H5Rcreate_object_ext_ff
 *
 * Purpose:	Create an external object reference (FF)
 *
 * Return:	Success:	0
 *		Failure:	FAIL
 *
 * Programmer:	Mohamad Chaarawi
 *		November, 2015
 *
 *-------------------------------------------------------------------------
 */
herr_t H5Rcreate_object_ext_ff(href_ff_t *ref, const char *filename, const char *pathname)
{
    herr_t   ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)

    /* Check args */
    if(ref == NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid reference pointer")
    if(!filename || !*filename)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no filename given")
    if(!pathname || !*pathname)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no pathname given")

    if(H5R_encode_ext(H5R_OBJECT_EXT, NULL, &ref->buf_size, filename, pathname, FAIL, NULL) < 0)
        HGOTO_ERROR(H5E_REFERENCE, H5E_CANTENCODE, FAIL, "Can't determine buffer size to encode reference")

    if(NULL == (ref->buf = HDmalloc(ref->buf_size)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate reference buffer")

    if(H5R_encode_ext(H5R_OBJECT_EXT, ref->buf, &ref->buf_size, filename, pathname, FAIL, NULL) < 0)
        HGOTO_ERROR(H5E_REFERENCE, H5E_CANTENCODE, FAIL, "Can't determine buffer size to encode reference")

    ref->ref_type = H5R_OBJECT_EXT;
done:
    FUNC_LEAVE_API(ret_value)
} /* H5Rcreate_object_ext_ff */


/*-------------------------------------------------------------------------
 * Function:	H5Rcreate_region_ext_ff
 *
 * Purpose:	Create an external region reference (FF)
 *
 * Return:	Success:	0
 *		Failure:	FAIL
 *
 * Programmer:	Mohamad Chaarawi
 *		November, 2015
 *
 *-------------------------------------------------------------------------
 */
herr_t H5Rcreate_region_ext_ff(href_ff_t *ref, const char *filename, const char *pathname, 
                               hid_t space_id)
{
    herr_t   ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)

    /* Check args */
    if(ref == NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid reference pointer")
    if(!filename || !*filename)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no filename given")
    if(!pathname || !*pathname)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no pathname given")
    if(space_id == (-1))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "reference region dataspace id must be valid")

    if(H5R_encode_ext(H5R_DATASET_REGION_EXT, NULL, &ref->buf_size, filename, pathname, space_id, NULL) < 0)
        HGOTO_ERROR(H5E_REFERENCE, H5E_CANTENCODE, FAIL, "Can't determine buffer size to encode reference")

    if(NULL == (ref->buf = HDmalloc(ref->buf_size)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate reference buffer")

    if(H5R_encode_ext(H5R_DATASET_REGION_EXT, ref->buf, &ref->buf_size, filename, pathname, space_id, NULL) < 0)
        HGOTO_ERROR(H5E_REFERENCE, H5E_CANTENCODE, FAIL, "Can't determine buffer size to encode reference")

    ref->ref_type = H5R_DATASET_REGION_EXT;
done:
    FUNC_LEAVE_API(ret_value)
} /* H5Rcreate_region_ext_ff */


/*-------------------------------------------------------------------------
 * Function:	H5Rcreate_attr_ext_ff
 *
 * Purpose:	Create an external attributereference (FF)
 *
 * Return:	Success:	0
 *		Failure:	FAIL
 *
 * Programmer:	Mohamad Chaarawi
 *		November, 2015
 *
 *-------------------------------------------------------------------------
 */
herr_t H5Rcreate_attr_ext_ff(href_ff_t *ref, const char *filename, const char *pathname, 
                             const char *attr_name)
{
    herr_t   ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)

    /* Check args */
    if(ref == NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid reference pointer")
    if(!filename || !*filename)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no filename given")
    if(!pathname || !*pathname)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no pathname given")
    if(!attr_name || !*attr_name)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no attribute name given")

    if(H5R_encode_ext(H5R_ATTR_EXT, NULL, &ref->buf_size, filename, pathname, -1, attr_name) < 0)
        HGOTO_ERROR(H5E_REFERENCE, H5E_CANTENCODE, FAIL, "Can't determine buffer size to encode reference")

    if(NULL == (ref->buf = HDmalloc(ref->buf_size)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate reference buffer")

    if(H5R_encode_ext(H5R_ATTR_EXT, ref->buf, &ref->buf_size, filename, pathname, -1, attr_name) < 0)
        HGOTO_ERROR(H5E_REFERENCE, H5E_CANTENCODE, FAIL, "Can't determine buffer size to encode reference")

    ref->ref_type = H5R_ATTR_EXT;
done:
    FUNC_LEAVE_API(ret_value)
} /* H5Rcreate_attr_ext_ff */


/*-------------------------------------------------------------------------
 * Function:	H5Rdereference_ff
 *
 * Purpose:	Dereference an existing reference (FF)
 *
 * Return:	Success:	dereferenced object ID
 *		Failure:	FAIL
 *
 * Programmer:	Mohamad Chaarawi
 *		November, 2015
 *
 *-------------------------------------------------------------------------
 */
hid_t H5Rdereference_ff(hid_t loc_id, hid_t H5_ATTR_UNUSED oapl_id, const href_ff_t *ref, 
                        hid_t rcxt_id, hid_t estack_id)
{
    const uint8_t *p;
    char *pathname = NULL;
    H5VL_object_t *obj = NULL;
    hid_t ret_value;              /* Return value */

    FUNC_ENTER_API(FAIL)

    /* Check args */
    if(ref == NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid reference pointer");
    if(ref->buf == NULL || ref->buf_size == 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid reference")

    /* get the object */
    if(NULL == (obj = H5VL_get_object(loc_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid object/file identifier")

    p = (uint8_t *)ref->buf;

    if(ref->ref_type == H5R_OBJECT || ref->ref_type == H5R_DATASET_REGION || 
       ref->ref_type == H5R_ATTR) {
        size_t token_size;
        H5_priv_request_t  *request = NULL; /* private request struct inserted in event queue */
        void **req = NULL; /* pointer to plugin generate requests (Stays NULL if plugin does not support async */
        H5I_type_t opened_type;
        void  *opened_obj = NULL;
        H5TR_t tr;
        H5RC_t *rc;

        /* get the RC object */
        if(NULL == (rc = (H5RC_t *)H5I_object_verify(rcxt_id, H5I_RC)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a READ CONTEXT ID")

        if(estack_id != H5_EVENT_STACK_NULL) {
            /* create the private request */
            if(NULL == (request = (H5_priv_request_t *)H5MM_calloc(sizeof(H5_priv_request_t))))
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
            req = &request->req;
            request->vol_cls = rc->vol_cls;
        }

        UINT64DECODE(p, token_size);

        /* create the dummy TR object */
        tr.file = rc->file;
        tr.trans_num = rc->c_version;
        tr.req_info.request = NULL;
        tr.req_info.head = NULL;
        tr.req_info.tail = NULL;
        tr.req_info.num_req = 0;

        if(NULL == (opened_obj = H5VL_iod_obj_open_token((const void *)p, &tr, &opened_type, req)))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to open object");

        if(request && *req)
            if(H5ES_insert(estack_id, request) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to insert request in event stack")

        /* create hid_t for opened object */
        if((ret_value = H5VL_register_id(opened_type, opened_obj, obj->vol_info, TRUE)) < 0)
            HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to atomize object handle");

    }
    else if(ref->ref_type == H5R_OBJECT_EXT || ref->ref_type == H5R_DATASET_REGION_EXT || 
            ref->ref_type == H5R_ATTR_EXT) {
        size_t name_size;
        hid_t lapl_id = H5P_LINK_ACCESS_DEFAULT;
        hid_t dxpl_id = H5P_DATASET_XFER_DEFAULT;
        H5P_genplist_t *plist;
        H5I_type_t opened_type;
        H5VL_object_t *opened_obj = NULL;
        H5VL_loc_params_t loc_params;

        /* retrieve the pathname of the object to dereference */
        p = (uint8_t *)ref->buf;

        UINT64DECODE(p, name_size);
        p += name_size;
        UINT64DECODE(p, name_size);
        pathname = (char *)HDmalloc(name_size);
        HDmemcpy(pathname, p, name_size);
        pathname[name_size] = '\0';

        loc_params.type = H5VL_OBJECT_BY_NAME;
        loc_params.loc_data.loc_by_name.name = pathname;
        loc_params.loc_data.loc_by_name.lapl_id = lapl_id;
        loc_params.obj_type = H5I_get_type(loc_id);

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
    }
    else
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid reference type");

done:
    if(pathname) {
        HDfree(pathname);
        pathname = NULL;
    }
    FUNC_LEAVE_API(ret_value)
} /* H5Rdereference_ff */



/*-------------------------------------------------------------------------
 * Function:	H5Rdestroy_ff
 *
 * Purpose:	Free resources by a reference (FF)
 *
 * Return:	Success:	0
 *		Failure:	FAIL
 *
 * Programmer:	Mohamad Chaarawi
 *		November, 2015
 *
 *-------------------------------------------------------------------------
 */
herr_t H5Rdestroy_ff(href_ff_t *ref)
{
    herr_t   ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)

    /* Check args */
    if(ref == NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid reference pointer")
    if(ref->buf == NULL || ref->buf_size == 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid reference")

    HDfree(ref->buf);

done:
    FUNC_LEAVE_API(ret_value)
} /* H5Rdestroy_ff */


/*-------------------------------------------------------------------------
 * Function:	H5Rget_region_ff
 *
 * Purpose:	return dataspace from a region reference (FF)
 *
 * Return:	Success:	dataspace ID
 *		Failure:	FAIL
 *
 * Programmer:	Mohamad Chaarawi
 *		November, 2015
 *
 *-------------------------------------------------------------------------
 */
hid_t H5Rget_region_ff(const href_ff_t *ref)
{
    const uint8_t *p;
    H5S_t *ds = NULL;
    hid_t ret_value;              /* Return value */

    FUNC_ENTER_API(FAIL)

    /* Check args */
    if(ref == NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid reference pointer");
    if(ref->buf == NULL || ref->buf_size == 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid reference")
    if(ref->ref_type != H5R_DATASET_REGION_EXT && ref->ref_type != H5R_DATASET_REGION)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "Not a region reference");

    p = (uint8_t *)ref->buf;

    /* get the region selection */
    if(ref->ref_type == H5R_DATASET_REGION) {
        size_t token_size, space_size;

        /* token */
        UINT64DECODE(p, token_size);
        p += token_size;

        /* space */
        UINT64DECODE(p, space_size);
        if((ds = H5S_decode((const unsigned char **)&p)) == NULL)
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTDECODE, FAIL, "can't decode object");
    }
    else {
        size_t name_size, space_size;
        /* file name */
        UINT64DECODE(p, name_size);
        p += name_size;
        /* path name */
        UINT64DECODE(p, name_size);
        p += name_size;

        /* space */
        UINT64DECODE(p, space_size);
        if((ds = H5S_decode((const unsigned char **)&p)) == NULL)
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTDECODE, FAIL, "can't decode object");
    }

    /* Register the type and return the ID */
    if((ret_value = H5I_register(H5I_DATASPACE, ds, TRUE)) < 0)
	HGOTO_ERROR(H5E_DATASPACE, H5E_CANTREGISTER, FAIL, "unable to register dataspace")

done:
    FUNC_LEAVE_API(ret_value)
} /* H5Rget_region_ff */


/*-------------------------------------------------------------------------
 * Function:	H5Rget_obj_type_ff
 *
 * Purpose:	Retrieve object type from reference (FF)
 *
 * Return:	Success:	0
 *		Failure:	FAIL
 *
 * Programmer:	Mohamad Chaarawi
 *		November, 2015
 *
 *-------------------------------------------------------------------------
 */
herr_t H5Rget_obj_type_ff(const href_ff_t *ref, H5O_type_t *obj_type)
{
    const uint8_t *p;
    H5I_type_t obji_type;
    size_t token_size;
    herr_t   ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)

    /* Check args */
    if(ref == NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid reference pointer");
    if(ref->buf == NULL || ref->buf_size == 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid reference");
    if(ref->ref_type != H5R_OBJECT && ref->ref_type != H5R_DATASET_REGION &&
       ref->ref_type != H5R_ATTR)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid reference type");

    p = (uint8_t *)ref->buf;

    UINT64DECODE(p, token_size);
    p += sizeof(iod_obj_id_t) * 3;
    HDmemcpy(&obji_type, p, sizeof(H5I_type_t));

    if(obji_type == H5I_DATASET)
        *obj_type = H5O_TYPE_DATASET;
    else if(obji_type == H5I_DATATYPE)
        *obj_type = H5O_TYPE_NAMED_DATATYPE;
    else if(obji_type == H5I_GROUP)
        *obj_type = H5O_TYPE_GROUP;
    else if(obji_type == H5I_MAP)
        *obj_type = H5O_TYPE_MAP;
    else
        HDassert("Invalid Object type" && 0);

done:
    FUNC_LEAVE_API(ret_value)
} /* H5Rget_obj_type_ff */


/*-------------------------------------------------------------------------
 * Function:	H5Rget_name_ff
 *
 * Purpose:	Return object name from reference
 *
 * Return:	Success:	length of the name if successful.
 *		Failure:	FAIL
 *
 * Programmer:	Mohamad Chaarawi
 *		November, 2015
 *
 *-------------------------------------------------------------------------
 */
ssize_t H5Rget_name_ff(const href_ff_t *ref, char *name/*out*/, size_t size)
{
    const uint8_t *p;
    size_t name_size;
    ssize_t ret_value = -1;

    FUNC_ENTER_API(FAIL)

    /* Check args */
    if(ref == NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid reference pointer");
    if(ref->buf == NULL || ref->buf_size == 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid reference");
    if(ref->ref_type != H5R_OBJECT_EXT && ref->ref_type != H5R_DATASET_REGION_EXT && 
       ref->ref_type != H5R_ATTR_EXT)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid reference type");

    p = (uint8_t *)ref->buf;
    /* file name */
    UINT64DECODE(p, name_size);
    p += name_size;

    /* path name */
    UINT64DECODE(p, name_size);

    if(name && size >= name_size) {
        HDmemcpy(name, p, name_size);
        name[name_size] = '\0';
    }
    else if(name) {
        HDmemcpy(name, p, size);
        name[size] = '\0';
    }

    ret_value = (ssize_t)name_size;

done:
    FUNC_LEAVE_API(ret_value)
} /* H5Rget_name_ff */


/*-------------------------------------------------------------------------
 * Function:	H5Rget_filename_ff
 *
 * Purpose:	Filename associated with the reference (FF)
 *
 * Return:	Success:	length of the filename if successful.
 *		Failure:	FAIL
 *
 * Programmer:	Mohamad Chaarawi
 *		November, 2015
 *
 *-------------------------------------------------------------------------
 */
ssize_t H5Rget_filename_ff(const href_ff_t *ref, char *name/*out*/, size_t size)
{
    const uint8_t *p;
    size_t name_size;
    ssize_t ret_value = -1;

    FUNC_ENTER_API(FAIL)

    /* Check args */
    if(ref == NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid reference pointer");
    if(ref->buf == NULL || ref->buf_size == 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid reference");
    if(ref->ref_type != H5R_OBJECT_EXT && ref->ref_type != H5R_DATASET_REGION_EXT &&
       ref->ref_type != H5R_ATTR_EXT)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid reference type");

    p = (uint8_t *)ref->buf;
    /* file name */
    UINT64DECODE(p, name_size);

    if(name && size >= name_size) {
        HDmemcpy(name, p, name_size);
        name[name_size] = '\0';
    }
    else if(name) {
        HDmemcpy(name, p, size);
        name[size] = '\0';
    }

    ret_value = (ssize_t)name_size;

done:
    FUNC_LEAVE_API(ret_value)
} /* H5Rget_filename_ff */

static herr_t 
H5R_encode(H5R_type_t ref_type, void *buf, size_t *nalloc, void *_obj, 
           hid_t space_id, const char *attrname)
{
    H5VL_iod_object_t *obj = (H5VL_iod_object_t *)_obj;
    uint8_t *p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */
    size_t token_size;
    size_t buf_size = 0;
    herr_t ret_value = SUCCEED;     /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDcompile_assert(sizeof(size_t) <= sizeof(uint64_t));
    HDassert(ref_type == H5R_OBJECT || ref_type == H5R_DATASET_REGION ||
             ref_type == H5R_ATTR);

    if(H5VL_iod_get_token(obj, NULL, &token_size) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to get location token");
    buf_size += token_size + sizeof(uint64_t);

    if(buf) {
        UINT64ENCODE(p, token_size);
        if(H5VL_iod_get_token(obj, p, &token_size) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to get location token");
        p += token_size;
    }

    if(ref_type == H5R_DATASET_REGION) {
        size_t dspace_size;

        HDassert(space_id >= 0);

        if(H5Sencode(space_id, NULL, &dspace_size) < 0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTENCODE, FAIL, "Can't encode dataspace");
        buf_size += dspace_size + sizeof(uint64_t);

        if(buf) {
            UINT64ENCODE(p, dspace_size);
            if(H5Sencode(space_id, p, &dspace_size) < 0)
                HGOTO_ERROR(H5E_DATASPACE, H5E_CANTENCODE, FAIL, "Can't encode dataspace");
        }
    }
    else if (ref_type == H5R_ATTR) {
        uint64_t attrname_size;

        HDassert(attrname);

        attrname_size = strlen(attrname);
        buf_size += attrname_size + sizeof(uint64_t);

        if(buf) {
            UINT64ENCODE(p, attrname_size);
            HDmemcpy(p, attrname, attrname_size);
        }
    }

    *nalloc = buf_size;

done:
    FUNC_LEAVE_NOAPI(ret_value)
}

static herr_t 
H5R_encode_ext(H5R_type_t ref_type, void *buf, size_t *nalloc, const char *filename, 
               const char *pathname, hid_t space_id, const char *attrname)
{
    uint8_t *p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */
    size_t filename_size, pathname_size;
    size_t buf_size = 0;
    herr_t ret_value = SUCCEED;     /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDcompile_assert(sizeof(size_t) <= sizeof(uint64_t));
    HDassert(ref_type == H5R_OBJECT_EXT || ref_type == H5R_DATASET_REGION_EXT ||
             ref_type == H5R_ATTR_EXT);
    HDassert(filename);
    HDassert(pathname);

    filename_size = strlen(filename);
    pathname_size = strlen(pathname);
    buf_size += sizeof(uint64_t) * 2; /* size of filename and pathname */
    buf_size += filename_size;
    buf_size += pathname_size;

    if(buf) {
        UINT64ENCODE(p, filename_size);
        HDmemcpy(p, filename, filename_size);
        p += filename_size;

        UINT64ENCODE(p, pathname_size);
        HDmemcpy(p, pathname, pathname_size);
        p += pathname_size;
    }

    if(H5R_DATASET_REGION_EXT == ref_type) {
        size_t dspace_size;

        HDassert(space_id >= 0);
        if(H5Sencode(space_id, NULL, &dspace_size) < 0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTENCODE, FAIL, "Can't encode dataspace");
        buf_size += dspace_size + sizeof(uint64_t);

        if(buf) {
            UINT64ENCODE(p, dspace_size);
            if(H5Sencode(space_id, p, &dspace_size) < 0)
                HGOTO_ERROR(H5E_DATASPACE, H5E_CANTENCODE, FAIL, "Can't encode dataspace");
        }
    }
    else if(H5R_ATTR_EXT == ref_type) {
        uint64_t attrname_size;

        HDassert(attrname);

        attrname_size = strlen(attrname);
        buf_size += attrname_size + sizeof(uint64_t);
        if(buf) {
            UINT64ENCODE(p, attrname_size);
            HDmemcpy(p, attrname, attrname_size);
        }
    }

    *nalloc = buf_size;

done:
    FUNC_LEAVE_NOAPI(ret_value)
}

herr_t
H5Rprint_ref(href_ff_t *ref)
{
    const uint8_t *p;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)

    HDassert(ref);
    HDassert(ref->buf);

    p = (uint8_t *)ref->buf;

    printf("================================================\n");

    switch(ref->ref_type) {
    case H5R_OBJECT:
        {
            size_t token_size;

            printf("Ref type = %d (%s)\n", (int)ref->ref_type, "Object Ref");
            printf("Total Buffer size = %zu\n", ref->buf_size);

            UINT64DECODE(p, token_size);
            printf("Token size = %zu\n", token_size);
            H5R_print_token(p);
            p += token_size;
            break;
        }
    case H5R_DATASET_REGION:
        {
            size_t token_size;

            printf("Ref type = %d (%s)\n", (int)ref->ref_type, "Dataset Region Ref");
            printf("Total Buffer size = %zu\n", ref->buf_size);

            UINT64DECODE(p, token_size);
            printf("Token size = %zu\n", token_size);
            H5R_print_token(p);
            p += token_size;

            /* decode selection */
            {
                H5S_t *ds;
                size_t space_size;

                UINT64DECODE(p, space_size);
                printf("Region selection size = %zu\n", space_size);

                if((ds = H5S_decode((const unsigned char **)&p)) == NULL)
                    HGOTO_ERROR(H5E_DATASPACE, H5E_CANTDECODE, FAIL, "can't decode object");
                p += space_size;

                printf("Dataspace Rank = %d\n", H5S_get_simple_extent_ndims(ds));
                printf("Num points selected = %d\n", (int)H5S_get_select_npoints(ds));

                H5S_close(ds);
            }
            break;
        }
    case H5R_ATTR:
        {
            size_t token_size;

            printf("Ref type = %d (%s)\n", (int)ref->ref_type, "Attr Ref");
            printf("Total Buffer size = %zu\n", ref->buf_size);

            UINT64DECODE(p, token_size);
            printf("Token size = %zu\n", token_size);
            H5R_print_token(p);
            p += token_size;

            {
                size_t name_size;
                char name[1024];

                UINT64DECODE(p, name_size);
                HDmemcpy(name, p, name_size);
                p += name_size;
                name[name_size] = '\0';
                printf("Attribute Name length = %zu\n", name_size);
                printf("Attribute Name = %s\n", name);
            }

            break;
        }
    case H5R_OBJECT_EXT:
        {
            printf("Ref type = %d (%s)\n", (int)ref->ref_type, "External Object Ref");
            printf("Total Buffer size = %zu\n", ref->buf_size);

            {
                size_t name_size;
                char name[1024];

                UINT64DECODE(p, name_size);
                printf("File Name length = %zu\n", name_size);
                HDmemcpy(name, p, name_size);
                p += name_size;
                name[name_size] = '\0';
                printf("File Name = %s\n", name);
            }

            {
                size_t name_size;
                char name[1024];

                UINT64DECODE(p, name_size);
                HDmemcpy(name, p, name_size);
                p += name_size;
                name[name_size] = '\0';
                printf("Path Name length = %zu\n", name_size);
                printf("Path Name = %s\n", name);
            }

            break;
        }
    case H5R_DATASET_REGION_EXT:
        {
            printf("Ref type = %d (%s)\n", (int)ref->ref_type, "External Dataset Region Ref");
            printf("Total Buffer size = %zu\n", ref->buf_size);

            {
                size_t name_size;
                char name[1024];

                UINT64DECODE(p, name_size);
                HDmemcpy(name, p, name_size);
                p += name_size;
                name[name_size] = '\0';
                printf("File Name length = %zu\n", name_size);
                printf("File Name = %s\n", name);
            }

            {
                size_t name_size;
                char name[1024];

                UINT64DECODE(p, name_size);
                HDmemcpy(name, p, name_size);
                p += name_size;
                name[name_size] = '\0';
                printf("Path Name length = %zu\n", name_size);
                printf("Path Name = %s\n", name);
            }

            /* decode selection */
            {
                H5S_t *ds;
                size_t space_size;

                UINT64DECODE(p, space_size);
                printf("Region selection size = %zu\n", space_size);

                if((ds = H5S_decode((const unsigned char **)&p)) == NULL)
                    HGOTO_ERROR(H5E_DATASPACE, H5E_CANTDECODE, FAIL, "can't decode object");
                p += space_size;

                printf("Dataspace Rank = %d\n", H5S_get_simple_extent_ndims(ds));
                printf("Num points selected = %d\n", (int)H5S_get_select_npoints(ds));

                H5S_close(ds);
            }

            break;
        }
    case H5R_ATTR_EXT:
        {
            printf("Ref type = %d (%s)\n", (int)ref->ref_type, "External Attr Ref");
            printf("Total Buffer size = %zu\n", ref->buf_size);

            {
                size_t name_size;
                char name[1024];

                UINT64DECODE(p, name_size);
                HDmemcpy(name, p, name_size);
                p += name_size;
                name[name_size] = '\0';
                printf("File Name length = %zu\n", name_size);
                printf("File Name = %s\n", name);
            }

            {
                size_t name_size;
                char name[1024];

                UINT64DECODE(p, name_size);
                HDmemcpy(name, p, name_size);
                p += name_size;
                name[name_size] = '\0';
                printf("Path Name length = %zu\n", name_size);
                printf("Path Name = %s\n", name);
            }

            {
                size_t name_size;
                char name[1024];

                UINT64DECODE(p, name_size);
                HDmemcpy(name, p, name_size);
                p += name_size;
                name[name_size] = '\0';
                printf("Attribute Name length = %zu\n", name_size);
                printf("Attribute Name = %s\n", name);
            }

            break;
        }
    case H5R_BADTYPE:
    case H5R_MAXTYPE:
    default:
        HDassert("bad reference" && 0);
    }

    printf("================================================\n");
done:
    FUNC_LEAVE_API(ret_value)
}

static herr_t
H5R_print_token(const void *token)
{
    const uint8_t *buf_ptr = (const uint8_t *)token;
    H5I_type_t obj_type;
    iod_obj_id_t iod_id, mdkv_id, attrkv_id;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    HDmemcpy(&iod_id, buf_ptr, sizeof(iod_obj_id_t));
    buf_ptr += sizeof(iod_obj_id_t);
    HDmemcpy(&mdkv_id, buf_ptr, sizeof(iod_obj_id_t));
    buf_ptr += sizeof(iod_obj_id_t);
    HDmemcpy(&attrkv_id, buf_ptr, sizeof(iod_obj_id_t));
    buf_ptr += sizeof(iod_obj_id_t);
    HDmemcpy(&obj_type, buf_ptr, sizeof(H5I_type_t));
    buf_ptr += sizeof(H5I_type_t);

    switch(obj_type) {
    case H5I_ATTR:
        printf("Object type = %s\n", "ATTRIBUTE"); 

        /* decode dtype */
        {
            size_t dt_size;

            HDmemcpy(&dt_size, buf_ptr, sizeof(size_t));
            buf_ptr += sizeof(size_t);
            /* Create datatype by decoding buffer */
            //if(NULL == (dt = H5T_decode((const unsigned char *)buf_ptr)))
            //HGOTO_ERROR(H5E_DATATYPE, H5E_CANTDECODE, FAIL, "can't decode object");
            buf_ptr += dt_size;
        }

        /* decode dspace */
        {
            H5S_t *ds;
            size_t space_size;

            HDmemcpy(&space_size, buf_ptr, sizeof(size_t));
            buf_ptr += sizeof(size_t);
            if((ds = H5S_decode((const unsigned char **)&buf_ptr)) == NULL)
                HGOTO_ERROR(H5E_DATASPACE, H5E_CANTDECODE, FAIL, "can't decode object");
            buf_ptr += space_size;

            printf("Token Dataspace Rank = %d\n", H5S_get_simple_extent_ndims(ds));

            H5S_close(ds);
        }

        break;
    case H5I_DATASET:
        printf("Object type = %s\n", "DATASET"); 

        /* decode creation property list */
        {
            size_t plist_size;

            HDmemcpy(&plist_size, buf_ptr, sizeof(size_t));
            buf_ptr += sizeof(size_t);
            buf_ptr += plist_size;
        }

        /* decode dtype */
        {
            size_t dt_size;

            HDmemcpy(&dt_size, buf_ptr, sizeof(size_t));
            buf_ptr += sizeof(size_t);
            buf_ptr += dt_size;
        }

        /* decode dspace */
        {
            H5S_t *ds;
            size_t space_size;

            HDmemcpy(&space_size, buf_ptr, sizeof(size_t));
            buf_ptr += sizeof(size_t);
            if((ds = H5S_decode((const unsigned char **)&buf_ptr)) == NULL)
                HGOTO_ERROR(H5E_DATASPACE, H5E_CANTDECODE, FAIL, "can't decode object");
            buf_ptr += space_size;

            printf("Token Dataspace Rank = %d\n", H5S_get_simple_extent_ndims(ds));
            printf("Token Num points selected = %d\n", (int)H5S_get_select_npoints(ds));

            H5S_close(ds);
        }

        break;
    case H5I_DATATYPE:
        printf("Object type = %s\n", "DATATYPE"); 

        /* decode creation property list */
        {
            size_t plist_size;

            HDmemcpy(&plist_size, buf_ptr, sizeof(size_t));
            buf_ptr += sizeof(size_t);
            buf_ptr += plist_size;
        }

        /* decode dtype */
        {
            size_t dt_size;

            HDmemcpy(&dt_size, buf_ptr, sizeof(size_t));
            buf_ptr += sizeof(size_t);
            buf_ptr += dt_size;
        }

        break;
    case H5I_GROUP:
        printf("Object type = %s\n", "GROUP"); 

        /* decode creation property list */
        {
            size_t plist_size;

            HDmemcpy(&plist_size, buf_ptr, sizeof(size_t));
            buf_ptr += sizeof(size_t);
            buf_ptr += plist_size;
        }

        break;
    case H5I_MAP:
        printf("Object type = %s\n", "MAP"); 

        /* decode creation property list */
        {
            size_t plist_size;

            HDmemcpy(&plist_size, buf_ptr, sizeof(size_t));
            buf_ptr += sizeof(size_t);
            buf_ptr += plist_size;
        }

        /* decode key_type */
        {
            size_t dt_size;

            HDmemcpy(&dt_size, buf_ptr, sizeof(size_t));
            buf_ptr += sizeof(size_t);
            buf_ptr += dt_size;
        }
        /* decode val_type */
        {
            size_t dt_size;

            HDmemcpy(&dt_size, buf_ptr, sizeof(size_t));
            buf_ptr += sizeof(size_t);
            buf_ptr += dt_size;
        }

        break;
    case H5I_UNINIT:
    case H5I_BADID:
    case H5I_FILE:
    case H5I_DATASPACE:
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
        HGOTO_ERROR(H5E_ARGS, H5E_CANTINIT, FAIL, "not a valid file object (dataset, map, group, or datatype)");
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
}/* end H5R_print_token() */
