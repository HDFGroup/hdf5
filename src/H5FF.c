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
#include "H5private.h"		/* Generic Functions			*/
#include "H5Apkg.h"             /* Attribute access			*/
#include "H5Dpkg.h"             /* Dataset access			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5ESprivate.h"        /* Event Stacks                         */
#include "H5Fpkg.h"             /* File access				*/
#include "H5FFprivate.h"        /* FastForward wrappers                 */
#include "H5Gpkg.h"             /* Group access				*/
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Pprivate.h"		/* Property lists			*/
#include "H5Qprivate.h"		/* Query        			*/
#include "H5Tpkg.h"             /* Datatype access			*/

#include "H5VLiod.h"		/* IOD plugin - tmp      		*/
#include "H5VLiod_client.h"	/* Client IOD - tmp			*/
#include "H5VLiod_server.h"	/* Server IOD - tmp			*/
#include "H5Xprivate.h"         /* Indexing */

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
static herr_t H5D__apply_index_query(void *idx_handle, H5X_class_t *idx_class, 
                                     hid_t query_id, hid_t xxpl_id, hid_t *space_id);

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

    if(H5G_init() < 0)
        HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to init group interface")

    if(H5D_init() < 0)
        HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to init dataset interface")

    if(H5A_init() < 0)
        HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to init attribute interface")

    if(H5M_init() < 0)
        HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to init map interface")

    if(H5RC_init() < 0)
        HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to init map interface")

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FF__init_package() */


/*-------------------------------------------------------------------------
 * Function:	H5Fcreate_ff
 *
 * Purpose:	Asynchronous wrapper around H5Fcreate().
 *
 * Return:	Success:	The placeholder ID for a new file.  When
 *                              the asynchronous operation completes, this
 *                              ID will transparently be modified to be a
 *                              "normal" ID.
 *		Failure:	FAIL
 *
 * Programmer:	Quincey Koziol
 *		Wednesday, March 20, 2013
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Fcreate_ff(const char *filename, unsigned flags, hid_t fcpl_id, hid_t fapl_id, hid_t estack_id)
{
    void    *file = NULL;            /* file token from VOL plugin */
    H5P_genplist_t *plist;        /* Property list pointer */
    H5_priv_request_t *request = NULL; /* private request struct inserted in event queue */
    void **req = NULL;       /* pointer to plugin generate requests (Stays NULL if plugin does not support async */
    H5VL_plugin_prop_t plugin_prop;         /* Property for vol plugin ID & info */
    H5VL_class_t *vol_cls = NULL; /* VOL Class structure for callback info */
    H5VL_t *vol_info = NULL;      /* VOL info struct */
    hid_t    ret_value;              /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE5("i", "*sIuiii", filename, flags, fcpl_id, fapl_id, estack_id);

    /* Check/fix arguments */
    if(!filename || !*filename)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid file name")
    /* In this routine, we only accept the following flags:
     *          H5F_ACC_EXCL, H5F_ACC_TRUNC and H5F_ACC_DEBUG
     */
    if(flags & ~(H5F_ACC_EXCL | H5F_ACC_TRUNC | H5F_ACC_DEBUG))
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid flags")
    /* The H5F_ACC_EXCL and H5F_ACC_TRUNC flags are mutually exclusive */
    if((flags & H5F_ACC_EXCL) && (flags & H5F_ACC_TRUNC))
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "mutually exclusive flags for file creation")

    /* Check file creation property list */
    if(H5P_DEFAULT == fcpl_id)
        fcpl_id = H5P_FILE_CREATE_DEFAULT;
    else
        if(TRUE != H5P_isa_class(fcpl_id, H5P_FILE_CREATE))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not file create property list")

    /* Check the file access property list */
    if(H5P_DEFAULT == fapl_id)
        fapl_id = H5P_FILE_ACCESS_DEFAULT;
    else
        if(TRUE != H5P_isa_class(fapl_id, H5P_FILE_ACCESS))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not file access property list")

    /* get the VOL info from the fapl */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(fapl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access property list")

    if(H5P_peek(plist, H5F_ACS_VOL_NAME, &plugin_prop) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get vol plugin info")

    if(NULL == (vol_cls = (H5VL_class_t *)H5I_object_verify(plugin_prop.plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    if(estack_id != H5_EVENT_STACK_NULL) {
        /* create the private request */
        if(NULL == (request = (H5_priv_request_t *)H5MM_calloc(sizeof(H5_priv_request_t))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
        req = &request->req;
        request->vol_cls = vol_cls;
    }

    /* create a new file or truncate an existing file through the VOL */
    if(NULL == (file = H5VL_file_create(vol_cls, filename, flags, fcpl_id, fapl_id, 
                                        H5AC_dxpl_id, req)))
	HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, FAIL, "unable to create file")

    if(request && *req)
        if(H5ES_insert(estack_id, request) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to insert request in event stack")

    /* setup VOL info struct */
    if(NULL == (vol_info = H5FL_CALLOC(H5VL_t)))
	HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, FAIL, "can't allocate VL info struct")
    vol_info->vol_cls = vol_cls;
    vol_info->vol_id = plugin_prop.plugin_id;
    if(H5I_inc_ref(vol_info->vol_id, FALSE) < 0)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTINC, FAIL, "unable to increment ref count on VOL plugin")

    /* Get an atom for the file */
    if((ret_value = H5VL_register_id(H5I_FILE, file, vol_info, TRUE)) < 0)
	HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to atomize file handle")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Fcreate_ff() */


/*-------------------------------------------------------------------------
 * Function:	H5Fopen_ff
 *
 * Purpose:	Asynchronous wrapper around H5Fopen().
 *
 * Return:	Success:	The placeholder ID for a new file.  When
 *                              the asynchronous operation completes, this
 *                              ID will transparently be modified to be a
 *                              "normal" ID.
 *		Failure:	FAIL
 *
 * Programmer:	Quincey Koziol
 *		Wednesday, March 20, 2013
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Fopen_ff(const char *filename, unsigned flags, hid_t fapl_id, 
           hid_t *rcxt_id, hid_t estack_id)
{
    void    *file = NULL;            /* file token from VOL plugin */
    H5P_genplist_t *plist;        /* Property list pointer */
    H5_priv_request_t *request = NULL; /* private request struct inserted in event queue */
    void **req = NULL;       /* pointer to plugin generate requests (Stays NULL if plugin does not support async */
    H5VL_plugin_prop_t plugin_prop;         /* Property for vol plugin ID & info */
    H5VL_class_t *vol_cls = NULL; /* VOL Class structure for callback info */
    H5VL_t *vol_info = NULL;      /* VOL info struct */
    hid_t    ret_value;              /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE5("i", "*sIui*ii", filename, flags, fapl_id, rcxt_id, estack_id);

    /* Check/fix arguments. */
    if(!filename || !*filename)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid file name")
    /* Reject undefined flags (~H5F_ACC_PUBLIC_FLAGS) and the H5F_ACC_TRUNC & H5F_ACC_EXCL flags */
    if((flags & ~H5F_ACC_PUBLIC_FLAGS) ||
            (flags & H5F_ACC_TRUNC) || (flags & H5F_ACC_EXCL))
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid file open flags")
    if(H5P_DEFAULT == fapl_id)
        fapl_id = H5P_FILE_ACCESS_DEFAULT;
    else
        if(TRUE != H5P_isa_class(fapl_id, H5P_FILE_ACCESS))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not file access property list")

    /* get the VOL info from the fapl */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(fapl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access property list")

    if(H5P_peek(plist, H5F_ACS_VOL_NAME, &plugin_prop) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get vol plugin info")

    if(NULL == (vol_cls = (H5VL_class_t *)H5I_object_verify(plugin_prop.plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    /* setup VOL info struct */
    if(NULL == (vol_info = H5FL_CALLOC(H5VL_t)))
	HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, FAIL, "can't allocate VL info struct")
    vol_info->vol_cls = vol_cls;
    vol_info->vol_id = plugin_prop.plugin_id;
    if(H5I_inc_ref(vol_info->vol_id, FALSE) < 0)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTINC, FAIL, "unable to increment ref count on VOL plugin")

    /* determine if we want to acquire the latest readable version
       when the file is opened */
    if(rcxt_id) {
        H5RC_t *rc = NULL;


    /* allocate read context struct */
    if(NULL == (rc = H5FL_CALLOC(H5RC_t)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, NULL, "can't allocate read context structure")

    rc->file = file;
    rc->c_version = IOD_TID_UNKNOWN;

    rc->req_info.request = NULL;
    rc->req_info.head = NULL;
    rc->req_info.tail = NULL;
    rc->req_info.num_req = 0;

        /* create a new read context object (if user requested it) */
        /*if(NULL == (rc = H5RC_create(file, IOD_TID_UNKNOWN)))
            HGOTO_ERROR(H5E_SYM, H5E_CANTCREATE, FAIL, "unable to create read context");*/

        rc->vol_cls = vol_info->vol_cls;

        /* Get an atom for the event queue with the VOL information as the auxilary struct */
        if((*rcxt_id = H5I_register(H5I_RC, rc, TRUE)) < 0)
            HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to atomize read context handle");

        /* Get the plist structure */
        if(NULL == (plist = (H5P_genplist_t *)H5I_object(fapl_id)))
            HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")
        if(H5P_set(plist, H5VL_ACQUIRE_RC_ID, rcxt_id) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't set property value for rxct id")
    }

    if(estack_id != H5_EVENT_STACK_NULL) {
        /* create the private request */
        if(NULL == (request = (H5_priv_request_t *)H5MM_calloc(sizeof(H5_priv_request_t))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
        req = &request->req;
        request->vol_cls = vol_cls;
    }

    /* Open the file through the VOL layer */
    if(NULL == (file = H5VL_file_open(vol_cls, filename, flags, fapl_id, H5AC_dxpl_id, req)))
	HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, FAIL, "unable to create file")

    if(request && *req)
        if(H5ES_insert(estack_id, request) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to insert request in event stack")

    /* Get an atom for the file */
    if((ret_value = H5VL_register_id(H5I_FILE, file, vol_info, TRUE)) < 0)
	HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to atomize file handle")
done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Fopen_ff() */


/*-------------------------------------------------------------------------
 * Function:	H5Fclose_ff
 *
 * Purpose:	This function closes the file specified by FILE_ID by
 *		flushing all data to storage, and terminating access to the
 *		file through FILE_ID.  If objects (e.g., datasets, groups,
 *		etc.) are open in the file then the underlying storage is not
 *		closed until those objects are closed; however, all data for
 *		the file and the open objects is flushed.
 *
 * Return:	Success:	Non-negative
 *		Failure:	Negative
 *
 * Programmer:	Mohamad Chaarawi
 *              April 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Fclose_ff(hid_t file_id, hbool_t persist_flag, hid_t estack_id)
{
    H5VL_object_t *file;
    herr_t   ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)
    H5TRACE3("e", "ibi", file_id, persist_flag, estack_id);

    /* Check/fix arguments. */
    if(NULL == (file = (H5VL_object_t *)H5I_object_verify(file_id, H5I_FILE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file ID");

    /* If persist flag is FALSE (non-default), then set that flag in the file struct */
    if(FALSE == persist_flag) {
        H5VL_iod_file_t *iod_file = NULL;

        iod_file = (H5VL_iod_file_t *)file->vol_obj;
        iod_file->persist_on_close = persist_flag;
    }
    else {
        /* Check/fix arguments. */
        if(H5I_FILE != H5I_get_type(file_id))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file ID")
    }

    /* set the event request and dxpl IDs to be passed on to the VOL layer */
    file->close_estack_id = estack_id;
    file->close_dxpl_id = H5AC_dxpl_id;

    /* Decrement reference count on atom.  When it reaches zero the file will be closed. */
    if(H5I_dec_app_ref(file_id) < 0)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTCLOSEFILE, FAIL, "decrementing file ID failed")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Fclose_ff() */


/*-------------------------------------------------------------------------
 * Function:	H5Gcreate_ff
 *
 * Purpose:	Asynchronous wrapper around H5Gcreate().
 *
 * Return:	Success:	The placeholder ID for a group.  When
 *                              the asynchronous operation completes, this
 *                              ID will transparently be modified to be a
 *                              "normal" ID.
 *		Failure:	FAIL
 *
 * Programmer:	Quincey Koziol
 *		Wednesday, March 20, 2013
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Gcreate_ff(hid_t loc_id, const char *name, hid_t lcpl_id, hid_t gcpl_id, hid_t gapl_id,
             hid_t trans_id, hid_t estack_id)
{
    void    *grp = NULL;        /* dset token from VOL plugin */
    H5VL_object_t *obj = NULL;          /* object token of loc_id */
    hid_t dxpl_id = H5P_DATASET_XFER_DEFAULT; /* transfer property list to pass to the VOL plugin */
    H5VL_loc_params_t loc_params;
    H5P_genplist_t  *plist;            /* Property list pointer */
    H5_priv_request_t *request = NULL; /* private request struct inserted in event queue */
    void **req = NULL;       /* pointer to plugin generate requests (Stays NULL if plugin does not support async */
    hid_t       ret_value;              /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE7("i", "i*siiiii", loc_id, name, lcpl_id, gcpl_id, gapl_id, trans_id,
             estack_id);

    /* Check arguments */
    if(!name || !*name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name")

    /* Get correct property list */
    if(H5P_DEFAULT == lcpl_id)
        lcpl_id = H5P_LINK_CREATE_DEFAULT;
    else
        if(TRUE != H5P_isa_class(lcpl_id, H5P_LINK_CREATE))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not link creation property list")

    /* Check group creation property list */
    if(H5P_DEFAULT == gcpl_id)
        gcpl_id = H5P_GROUP_CREATE_DEFAULT;
    else
        if(TRUE != H5P_isa_class(gcpl_id, H5P_GROUP_CREATE))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not group create property list")

    /* Check the group access property list */
    if(H5P_DEFAULT == gapl_id)
        gapl_id = H5P_GROUP_ACCESS_DEFAULT;
    else
        if(TRUE != H5P_isa_class(gapl_id, H5P_GROUP_ACCESS))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not group access property list")

    /* Get the plist structure */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(gcpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")
    if(H5P_set(plist, H5VL_PROP_GRP_LCPL_ID, &lcpl_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't set property value for lcpl id")

    loc_params.type = H5VL_OBJECT_BY_SELF;
    loc_params.obj_type = H5I_get_type(loc_id);

    /* store the transaction ID in the dxpl */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")
    if(H5P_set(plist, H5VL_TRANS_ID, &trans_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't set property value for trans_id")

    /* get the location object */
    if(NULL == (obj = (H5VL_object_t *)H5I_object(loc_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid location identifier")

    if(estack_id != H5_EVENT_STACK_NULL) {
        /* create the private request */
        if(NULL == (request = (H5_priv_request_t *)H5MM_calloc(sizeof(H5_priv_request_t))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
        req = &request->req;
        request->vol_cls = obj->vol_info->vol_cls;
    }

    /* Create the group through the VOL */
    if(NULL == (grp = H5VL_group_create(obj->vol_obj, loc_params, obj->vol_info->vol_cls, name, 
                                        gcpl_id, gapl_id, dxpl_id, req)))
	HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to create group")

    if(request && *req)
        if(H5ES_insert(estack_id, request) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to insert request in event stack")

    /* Get an atom for the group */
    if((ret_value = H5VL_register_id(H5I_GROUP, grp, obj->vol_info, TRUE)) < 0)
	HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to atomize group handle")

done:
    if (ret_value < 0 && grp)
        if(H5VL_group_close (grp, obj->vol_info->vol_cls, dxpl_id, H5_REQUEST_NULL) < 0)
            HDONE_ERROR(H5E_SYM, H5E_CLOSEERROR, FAIL, "unable to release group")

    FUNC_LEAVE_API(ret_value)
} /* end H5Gcreate_ff() */


/*-------------------------------------------------------------------------
 * Function:	H5Gopen_ff
 *
 * Purpose:	Asynchronous wrapper around H5Gopen().
 *
 * Return:	Success:	The placeholder ID for a group.  When
 *                              the asynchronous operation completes, this
 *                              ID will transparently be modified to be a
 *                              "normal" ID.
 *		Failure:	FAIL
 *
 * Programmer:	Quincey Koziol
 *		Wednesday, March 20, 2013
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Gopen_ff(hid_t loc_id, const char *name, hid_t gapl_id,
           hid_t rcxt_id, hid_t estack_id)
{
    void    *grp = NULL;       /* dset token from VOL plugin */
    H5VL_object_t *obj = NULL;          /* object token of loc_id */
    hid_t dxpl_id = H5P_DATASET_XFER_DEFAULT; /* transfer property list to pass to the VOL plugin */
    H5P_genplist_t *plist;     /* Property list pointer */
    H5_priv_request_t *request = NULL; /* private request struct inserted in event queue */
    void **req = NULL;       /* pointer to plugin generate requests (Stays NULL if plugin does not support async */
    H5VL_loc_params_t loc_params;
    hid_t       ret_value;              /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE5("i", "i*siii", loc_id, name, gapl_id, rcxt_id, estack_id);

    /* Check args */
    if(!name || !*name)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name")

    /* Check the group access property list */
    if(H5P_DEFAULT == gapl_id)
        gapl_id = H5P_GROUP_ACCESS_DEFAULT;
    else
        if(TRUE != H5P_isa_class(gapl_id, H5P_GROUP_ACCESS))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not group access property list")

    loc_params.type = H5VL_OBJECT_BY_SELF;
    loc_params.obj_type = H5I_get_type(loc_id);

    /* get the location object */
    if(NULL == (obj = (H5VL_object_t *)H5I_object(loc_id)))
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

    /* Open the group through the VOL */
    if(NULL == (grp = H5VL_group_open(obj->vol_obj, loc_params, obj->vol_info->vol_cls, 
                                      name, gapl_id, dxpl_id, req)))
	HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, FAIL, "unable to open group")

    if(request && *req)
        if(H5ES_insert(estack_id, request) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to insert request in event stack")

    /* Get an atom for the group */
    if((ret_value = H5VL_register_id(H5I_GROUP, grp, obj->vol_info, TRUE)) < 0)
	HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to atomize group handle")

done:
    if (ret_value < 0 && grp)
        if(H5VL_group_close (grp, obj->vol_info->vol_cls, dxpl_id, H5_REQUEST_NULL) < 0)
            HDONE_ERROR(H5E_SYM, H5E_CLOSEERROR, FAIL, "unable to release group")

    FUNC_LEAVE_API(ret_value)
} /* end H5Gopen_ff() */


/*-------------------------------------------------------------------------
 * Function:	H5Gclose_ff
 *
 * Purpose:	Closes the specified group.  The group ID will no longer be
 *		valid for accessing the group.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Mohamad Chaarawi
 *              April 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Gclose_ff(hid_t group_id, hid_t estack_id)
{
    H5VL_object_t *obj;
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "ii", group_id, estack_id);

    /* Check args */
    if(NULL == (obj = (H5VL_object_t *)H5I_object_verify(group_id, H5I_GROUP)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a group ID")

    /* set the async request and dxpl IDs to be passed on to the VOL layer */
    obj->close_estack_id = estack_id;
    obj->close_dxpl_id = H5AC_dxpl_id;

    /*
     * Decrement the counter on the group atom.	 It will be freed if the count
     * reaches zero.
     */
    if(H5I_dec_app_ref(group_id) < 0)
    	HGOTO_ERROR(H5E_SYM, H5E_CANTRELEASE, FAIL, "unable to close group")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Gclose_ff() */


/*-------------------------------------------------------------------------
 * Function:	H5Dcreate_ff
 *
 * Purpose:	Asynchronous wrapper around H5Dcreate().
 *
 * Return:	Success:	The placeholder ID for a new dataset.  When
 *                              the asynchronous operation completes, this
 *                              ID will transparently be modified to be a
 *                              "normal" ID.
 *		Failure:	FAIL
 *
 * Programmer:	Quincey Koziol
 *		Wednesday, March 20, 2013
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Dcreate_ff(hid_t loc_id, const char *name, hid_t type_id, hid_t space_id, 
             hid_t lcpl_id, hid_t dcpl_id, hid_t dapl_id, hid_t trans_id, hid_t estack_id)
{
    void    *dset = NULL;       /* dset token from VOL plugin */
    H5VL_object_t *obj = NULL;        /* object token of loc_id */
    hid_t dxpl_id = H5P_DATASET_XFER_DEFAULT; /* transfer property list to pass to the VOL plugin */
    H5VL_loc_params_t loc_params;
    H5P_genplist_t  *plist;     /* Property list pointer */
    H5_priv_request_t *request = NULL; /* private request struct inserted in event queue */
    void **req = NULL;       /* pointer to plugin generate requests (Stays NULL if plugin does not support async */
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
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't set property value for trans_id")

    /* get the location object */
    if(NULL == (obj = (H5VL_object_t *)H5I_object(loc_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid location identifier")

    if(estack_id != H5_EVENT_STACK_NULL) {
        /* create the private request */
        if(NULL == (request = (H5_priv_request_t *)H5MM_calloc(sizeof(H5_priv_request_t))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
        req = &request->req;
        request->vol_cls = obj->vol_info->vol_cls;
    }

    /* Create the dataset through the VOL */
    if(NULL == (dset = H5VL_dataset_create(obj->vol_obj, loc_params, obj->vol_info->vol_cls, 
                                           name, dcpl_id, dapl_id, dxpl_id, req)))
	HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to create dataset")

    if(request && *req)
        if(H5ES_insert(estack_id, request) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to insert request in event stack")

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
 * Function:	H5Dcreate_anon_ff
 *
 * Purpose:	Asynchronous wrapper around H5Dcreate_anon().
 *
 * Return:	Success:	The placeholder ID for a new dataset.  When
 *                              the asynchronous operation completes, this
 *                              ID will transparently be modified to be a
 *                              "normal" ID.
 *		Failure:	FAIL
 *
 * Programmer:	Quincey Koziol
 *		Wednesday, March 20, 2013
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Dcreate_anon_ff(hid_t loc_id, hid_t type_id, hid_t space_id,
                  hid_t dcpl_id, hid_t dapl_id, hid_t trans_id, hid_t estack_id)
{
    void    *dset = NULL;       /* dset token from VOL plugin */
    H5VL_object_t *obj = NULL;        /* object token of loc_id */
    hid_t dxpl_id = H5P_DATASET_XFER_DEFAULT; /* transfer property list to pass to the VOL plugin */
    H5VL_loc_params_t loc_params;
    H5P_genplist_t  *plist;     /* Property list pointer */
    H5_priv_request_t *request = NULL; /* private request struct inserted in event queue */
    void **req = NULL;       /* pointer to plugin generate requests (Stays NULL if plugin does not support async */
    hid_t       ret_value;              /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE7("i", "iiiiiii", loc_id, type_id, space_id, dcpl_id, dapl_id, trans_id,
             estack_id);

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

    loc_params.type = H5VL_OBJECT_BY_SELF;
    loc_params.obj_type = H5I_get_type(loc_id);

    /* store the transaction ID in the dxpl */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")
    if(H5P_set(plist, H5VL_TRANS_ID, &trans_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't set property value for trans_id")

    /* get the location object */
    if(NULL == (obj = (H5VL_object_t *)H5I_object(loc_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid location identifier")

    if(estack_id != H5_EVENT_STACK_NULL) {
        /* create the private request */
        if(NULL == (request = (H5_priv_request_t *)H5MM_calloc(sizeof(H5_priv_request_t))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
        req = &request->req;
        request->vol_cls = obj->vol_info->vol_cls;
    }

    /* Create the dataset through the VOL */
    if(NULL == (dset = H5VL_dataset_create(obj->vol_obj, loc_params, obj->vol_info->vol_cls, 
                                           NULL, dcpl_id, dapl_id, 
                                           dxpl_id, req)))
	HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to create dataset")

    if(request && *req)
        if(H5ES_insert(estack_id, request) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to insert request in event stack")

    /* Get an atom for the dataset */
    if((ret_value = H5VL_register_id(H5I_DATASET, dset, obj->vol_info, TRUE)) < 0)
	HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to atomize dataset handle")

done:
    if (ret_value < 0 && dset)
        if(H5VL_dataset_close (dset, obj->vol_info->vol_cls, dxpl_id, H5_REQUEST_NULL) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CLOSEERROR, FAIL, "unable to release dataset")
    FUNC_LEAVE_API(ret_value)
} /* end H5Dcreate_anon_ff() */


/*-------------------------------------------------------------------------
 * Function:	H5Dopen_ff
 *
 * Purpose:	Asynchronous wrapper around H5Dopen().
 *
 * Return:	Success:	The placeholder ID for a dataset.  When
 *                              the asynchronous operation completes, this
 *                              ID will transparently be modified to be a
 *                              "normal" ID.
 *		Failure:	FAIL
 *
 * Programmer:	Quincey Koziol
 *		Wednesday, March 20, 2013
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Dopen_ff(hid_t loc_id, const char *name, hid_t dapl_id, hid_t rcxt_id, hid_t estack_id)
{
    void    *dset = NULL;       /* dset token from VOL plugin */
    H5VL_object_t *obj = NULL;        /* object token of loc_id */
    hid_t dxpl_id = H5P_DATASET_XFER_DEFAULT; /* transfer property list to pass to the VOL plugin */
    H5P_genplist_t *plist;     /* Property list pointer */
    H5_priv_request_t *request = NULL; /* private request struct inserted in event queue */
    void **req = NULL; /* pointer to plugin generate requests (Stays NULL if plugin does not support async */
    H5VL_loc_params_t loc_params;
    hid_t       ret_value;              /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE5("i", "i*siii", loc_id, name, dapl_id, rcxt_id, estack_id);

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
    if(H5P_set(plist, H5VL_CONTEXT_ID, &rcxt_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't set property value for rcxt_id")

    if(estack_id != H5_EVENT_STACK_NULL) {
        /* create the private request */
        if(NULL == (request = (H5_priv_request_t *)H5MM_calloc(sizeof(H5_priv_request_t))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
        req = &request->req;
        request->vol_cls = obj->vol_info->vol_cls;
    }

    /* Create the dataset through the VOL */
    if(NULL == (dset = H5VL_dataset_open(obj->vol_obj, loc_params, obj->vol_info->vol_cls, name, 
                                         dapl_id, dxpl_id, req)))
	HGOTO_ERROR(H5E_DATASET, H5E_CANTOPENOBJ, FAIL, "unable to open dataset")

    if(request && *req)
        if(H5ES_insert(estack_id, request) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to insert request in event stack")

    /* Get an atom for the dataset */
    if((ret_value = H5VL_register_id(H5I_DATASET, dset, obj->vol_info, TRUE)) < 0)
	HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to atomize dataset handle")

#ifdef H5_HAVE_INDEXING
    {
        H5X_class_t *idx_class = NULL;
        void *idx_handle = NULL;
        unsigned plugin_id;
        size_t metadata_size;
        size_t idx_count;
        void *metadata = NULL;
        H5P_genplist_t *xapl_plist; /* Property list pointer */
        hid_t xapl_id = H5P_INDEX_ACCESS_DEFAULT;

        /* Get index info if present */
        if (FAIL == H5VL_iod_dataset_get_index_info(dset, &idx_count, &plugin_id,
                &metadata_size, &metadata, rcxt_id, NULL))
            HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get index info for dataset");

        if (idx_count) {
            /* store the read context ID in the xapl */
            if(NULL == (xapl_plist = (H5P_genplist_t *)H5I_object(xapl_id)))
                HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");
            if(H5P_set(xapl_plist, H5VL_CONTEXT_ID, &rcxt_id) < 0)
                HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't set property value for trans_id");

            if (NULL == (idx_class = H5X_registered(plugin_id)))
                HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get index plugin class");

            /* Call open of index plugin */
            if (NULL == idx_class->open)
                HGOTO_ERROR(H5E_INDEX, H5E_BADVALUE, FAIL, "plugin open callback is not defined");
            if (NULL == (idx_handle = idx_class->open(loc_id, ret_value, xapl_id,
                    metadata_size, metadata)))
                HGOTO_ERROR(H5E_INDEX, H5E_CANTOPENOBJ, FAIL, "indexing open callback failed");

            /* Add idx_handle to dataset */
            if (FAIL == H5VL_iod_dataset_set_index(dset, idx_handle))
                HGOTO_ERROR(H5E_INDEX, H5E_CANTSET, FAIL, "cannot set index to dataset");
            if (FAIL == H5VL_iod_dataset_set_index_plugin_id(dset, plugin_id))
                HGOTO_ERROR(H5E_INDEX, H5E_CANTSET, FAIL, "cannot set index plugin ID to dataset");
        }
        /* No longer need metadata */
        H5MM_free(metadata);
    }
#endif /* H5_HAVE_INDEXING */

done:
    if (ret_value < 0 && dset)
        if(H5VL_dataset_close (dset, obj->vol_info->vol_cls, dxpl_id, H5_REQUEST_NULL) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CLOSEERROR, FAIL, "unable to release dataset")
    FUNC_LEAVE_API(ret_value)
} /* end H5Dopen_ff() */


/*-------------------------------------------------------------------------
 * Function:	H5Dwrite_ff
 *
 * Purpose:	Asynchronous wrapper around H5Dwrite().
 *
 * Return:	Success:	SUCCEED
 *		Failure:	FAIL
 *
 * Programmer:	Quincey Koziol
 *		Wednesday, March 20, 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Dwrite_ff(hid_t dset_id, hid_t mem_type_id, hid_t mem_space_id,
            hid_t file_space_id, hid_t dxpl_id, const void *buf,
            hid_t trans_id, hid_t estack_id)
{
    H5VL_object_t   *dset = NULL;
#ifdef H5_HAVE_INDEXING
    void       *idx_handle;
#endif
    H5P_genplist_t *plist;     /* Property list pointer */
    H5_priv_request_t *request = NULL; /* private request struct inserted in event queue */
    void **req = NULL; /* pointer to plugin generate requests (Stays NULL if plugin does not support async */
    herr_t      ret_value;              /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE8("e", "iiiii*xii", dset_id, mem_type_id, mem_space_id, file_space_id,
             dxpl_id, buf, trans_id, estack_id);

    /* check arguments */
    if(!dset_id)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset")
    if(NULL == (dset = (H5VL_object_t *)H5I_object_verify(dset_id, H5I_DATASET)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset")

    /* Get the default dataset transfer property list if the user didn't provide one */
    if(H5P_DEFAULT == dxpl_id)
        dxpl_id= H5P_DATASET_XFER_DEFAULT;
    else
        if(TRUE != H5P_isa_class(dxpl_id, H5P_DATASET_XFER))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not xfer parms")

    /* store the transaction ID in the dxpl */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")
    if(H5P_set(plist, H5VL_TRANS_ID, &trans_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't set property value for trans_id")

#ifdef H5_HAVE_INDEXING
    /* Get the index handle */
    if (NULL != (idx_handle = H5VL_iod_dataset_get_index(dset->vol_obj))) {
        H5X_class_t *idx_class = NULL;
        H5P_genplist_t *xxpl_plist; /* Property list pointer */
        unsigned plugin_id;
        hid_t xxpl_id = H5P_INDEX_XFER_DEFAULT;

        /* store the transaction ID in the xxpl */
        if(NULL == (xxpl_plist = (H5P_genplist_t *)H5I_object(xxpl_id)))
            HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")
        if(H5P_set(xxpl_plist, H5VL_TRANS_ID, &trans_id) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't set property value for trans_id")

        if (!(plugin_id = H5VL_iod_dataset_get_index_plugin_id(dset->vol_obj)))
            HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get index plugin ID from dataset");
        if (NULL == (idx_class = H5X_registered(plugin_id)))
            HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get index plugin class");

        if (NULL == idx_class->pre_update)
            HGOTO_ERROR(H5E_INDEX, H5E_BADVALUE, FAIL, "plugin pre_update callback is not defined");
        if (FAIL == idx_class->pre_update(idx_handle, mem_space_id, xxpl_id))
            HGOTO_ERROR(H5E_INDEX, H5E_CANTUPDATE, FAIL, "cannot execute index pre_update operation");
    }
#endif

    if(estack_id != H5_EVENT_STACK_NULL) {
        /* create the private request */
        if(NULL == (request = (H5_priv_request_t *)H5MM_calloc(sizeof(H5_priv_request_t))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
        req = &request->req;
        request->vol_cls = dset->vol_info->vol_cls;
    }

    /* Write the data through the VOL */
    if((ret_value = H5VL_dataset_write(dset->vol_obj, dset->vol_info->vol_cls, mem_type_id, mem_space_id, 
                                       file_space_id, dxpl_id, buf, req)) < 0)
	HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "can't write data")

    if(request && *req)
        if(H5ES_insert(estack_id, request) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to insert request in event stack")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Dwrite_ff() */


/*-------------------------------------------------------------------------
 * Function:	H5Dread_ff
 *
 * Purpose:	Asynchronous wrapper around H5Dread().
 *
 * Return:	Success:	SUCCEED
 *		Failure:	FAIL
 *
 * Programmer:	Quincey Koziol
 *		Wednesday, March 20, 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Dread_ff(hid_t dset_id, hid_t mem_type_id, hid_t mem_space_id,
           hid_t file_space_id, hid_t dxpl_id, void *buf,
           hid_t rcxt_id, hid_t estack_id)
{
    H5VL_object_t   *dset = NULL;
    H5P_genplist_t *plist;     /* Property list pointer */
    H5_priv_request_t *request = NULL; /* private request struct inserted in event queue */
    void **req = NULL; /* pointer to plugin generate requests (Stays NULL if plugin does not support async */
    herr_t      ret_value;              /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE8("e", "iiiii*xii", dset_id, mem_type_id, mem_space_id, file_space_id,
             dxpl_id, buf, rcxt_id, estack_id);

    if(NULL == (dset = (H5VL_object_t *)H5I_object_verify(dset_id, H5I_DATASET)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset")
    if(mem_space_id < 0 || file_space_id < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space")

    /* Get the default dataset transfer property list if the user didn't provide one */
    if (H5P_DEFAULT == dxpl_id)
        dxpl_id= H5P_DATASET_XFER_DEFAULT;
    else
        if(TRUE != H5P_isa_class(dxpl_id, H5P_DATASET_XFER))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not xfer parms")

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
        request->vol_cls = dset->vol_info->vol_cls;
    }

    /* Read the data through the VOL */
    if((ret_value = H5VL_dataset_read(dset->vol_obj, dset->vol_info->vol_cls, mem_type_id, mem_space_id, 
                                      file_space_id, dxpl_id, buf, req)) < 0)
	HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "can't read data")

    if(request && *req)
        if(H5ES_insert(estack_id, request) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to insert request in event stack")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Dread_ff() */


/*-------------------------------------------------------------------------
 * Function:	H5Dset_extent_ff
 *
 * Purpose:	Modifies the dimensions of a dataset.
 *		Can change to a smaller dimension.
 *
 * Return:	Non-negative on success, negative on failure
 *
 * Programmer:	Mohamad Chaarawi
 *              April 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Dset_extent_ff(hid_t dset_id, const hsize_t size[], hid_t trans_id, hid_t estack_id)
{
    H5VL_object_t *dset;
    hid_t dxpl_id = H5P_DATASET_XFER_DEFAULT; /* transfer property list to pass to the VOL plugin */
    H5P_genplist_t *plist;     /* Property list pointer */
    H5_priv_request_t *request = NULL; /* private request struct inserted in event queue */
    void **req = NULL; /* pointer to plugin generate requests (Stays NULL if plugin does not support async */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE4("e", "i*hii", dset_id, size, trans_id, estack_id);

    /* Check args */
    if(NULL == (dset = (H5VL_object_t *)H5I_object_verify(dset_id, H5I_DATASET)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid dataset identifier")

    if(!size)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no size specified")

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
        request->vol_cls = dset->vol_info->vol_cls;
    }

    /* set the extent through the VOL */
    if((ret_value = H5VL_dataset_specific(dset->vol_obj, dset->vol_info->vol_cls, H5VL_DATASET_SET_EXTENT, 
                                          dxpl_id, req, size)) < 0)
	HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to set extent of dataset")

    if(request && *req)
        if(H5ES_insert(estack_id, request) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to insert request in event stack")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Dset_extent_ff() */


/*-------------------------------------------------------------------------
 * Function:	H5Dclose_ff
 *
 * Purpose:	Closes access to a dataset (DATASET_ID) and releases
 *		resources used by it. It is illegal to subsequently use that
 *		same dataset ID in calls to other dataset functions.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Mohamad Chaarawi
 *              April 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Dclose_ff(hid_t dset_id, hid_t estack_id)
{
#ifdef H5_HAVE_INDEXING
    unsigned plugin_id;
    void *idx_handle = NULL;
#endif
    H5VL_object_t *dset;
    herr_t       ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "ii", dset_id, estack_id);

    /* Check args */
    if(NULL == (dset = (H5VL_object_t *)H5I_object_verify(dset_id, H5I_DATASET)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid dataset identifier")

    /* set the async request and dxpl IDs to be passed on to the VOL layer */
    dset->close_estack_id = estack_id;
    dset->close_dxpl_id = H5AC_dxpl_id;

#ifdef H5_HAVE_INDEXING
    /* Get the index handle if there is one */
    idx_handle = H5VL_iod_dataset_get_index(dset->vol_obj);
    if (idx_handle && !(plugin_id = H5VL_iod_dataset_get_index_plugin_id(dset->vol_obj)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get index plugin ID from dataset");
#endif

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

#ifdef H5_HAVE_INDEXING
	/*
	 * Close the index if the dataset is now closed
	 */
	if (idx_handle && (FAIL == H5I_get_ref(dset_id, FALSE))) {
        H5X_class_t *idx_class = NULL;

        if (NULL == (idx_class = H5X_registered(plugin_id)))
            HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get index plugin class");

        if (NULL == idx_class->close)
            HGOTO_ERROR(H5E_INDEX, H5E_BADVALUE, FAIL, "plugin close callback is not defined");
        if (FAIL == idx_class->close(idx_handle))
            HGOTO_ERROR(H5E_INDEX, H5E_CANTCLOSEOBJ, FAIL, "cannot close index");
	}
#endif

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Dclose() */


/*-------------------------------------------------------------------------
 * Function:	H5Tcommit_ff
 *
 * Purpose:	Save a transient datatype to a file and turn the type handle
 *		into a "named", immutable type.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Mohamad Chaarawi
 *              April 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Tcommit_ff(hid_t loc_id, const char *name, hid_t type_id, hid_t lcpl_id,
             hid_t tcpl_id, hid_t tapl_id, hid_t trans_id, hid_t estack_id)
{
    void    *dt = NULL;
    H5VL_object_t *new_obj = NULL;      /* VOL object that holds the datatype object and the VOL info */
    H5T_t   *type = NULL;
    H5VL_object_t *obj = NULL;        /* object token of loc_id */
    hid_t dxpl_id = H5P_DATASET_XFER_DEFAULT; /* transfer property list to pass to the VOL plugin */
    H5P_genplist_t *plist;     /* Property list pointer */
    H5_priv_request_t *request = NULL; /* private request struct inserted in event queue */
    void **req = NULL; /* pointer to plugin generate requests (Stays NULL if plugin does not support async */
    H5VL_loc_params_t loc_params;
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE8("e", "i*siiiiii", loc_id, name, type_id, lcpl_id, tcpl_id, tapl_id,
             trans_id, estack_id);

    /* Check arguments */
    if(!name || !*name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name")
    if(NULL == (type = (H5T_t *)H5I_object_verify(type_id, H5I_DATATYPE)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a datatype")
    if(H5T_is_named(type))
        HGOTO_ERROR(H5E_ARGS, H5E_CANTSET, FAIL, "datatype is already committed")

    /* Get correct property list */
    if(H5P_DEFAULT == lcpl_id)
        lcpl_id = H5P_LINK_CREATE_DEFAULT;
    else
        if(TRUE != H5P_isa_class(lcpl_id, H5P_LINK_CREATE))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not link creation property list")

    /* Get correct property list */
    if(H5P_DEFAULT == tcpl_id)
        tcpl_id = H5P_DATATYPE_CREATE_DEFAULT;
    else
        if(TRUE != H5P_isa_class(tcpl_id, H5P_DATATYPE_CREATE))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not datatype creation property list")

    /* Get correct property list */
    if(H5P_DEFAULT == tapl_id)
        tapl_id = H5P_DATATYPE_ACCESS_DEFAULT;
    else
        if(TRUE != H5P_isa_class(tapl_id, H5P_DATATYPE_ACCESS))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not datatype access property list")

    loc_params.type = H5VL_OBJECT_BY_SELF;
    loc_params.obj_type = H5I_get_type(loc_id);

    /* store the transaction ID in the dxpl */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")
    if(H5P_set(plist, H5VL_TRANS_ID, &trans_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't set property value for trans_id")

    /* get the object from the loc_id */
    if(NULL == (obj = (H5VL_object_t *)H5I_object(loc_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid object identifier")

    if(estack_id != H5_EVENT_STACK_NULL) {
        /* create the private request */
        if(NULL == (request = (H5_priv_request_t *)H5MM_calloc(sizeof(H5_priv_request_t))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
        req = &request->req;
        request->vol_cls = obj->vol_info->vol_cls;
    }

    /* commit the datatype through the VOL */
    if (NULL == (dt = H5VL_datatype_commit(obj->vol_obj, loc_params, obj->vol_info->vol_cls, 
                                           name, type_id, lcpl_id, tcpl_id, tapl_id, 
                                           dxpl_id, req)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to commit datatype")

    if(request && *req)
        if(H5ES_insert(estack_id, request) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to insert request in event stack")

    /* setup VOL object */
    if(NULL == (new_obj = H5FL_CALLOC(H5VL_object_t)))
	HGOTO_ERROR(H5E_VOL, H5E_NOSPACE, FAIL, "can't allocate top object structure")
    new_obj->vol_info = obj->vol_info;
    new_obj->vol_info->nrefs ++;
    new_obj->vol_obj = dt;

    /* set the committed type object to the VOL pluging pointer in the H5T_t struct */
    type->vol_obj = new_obj;

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Tcommit_ff() */


/*-------------------------------------------------------------------------
 * Function:	H5Topen_ff
 *
 * Purpose:	Opens a named datatype using a Datatype Access Property
 *              List.
 *
 * Return:	Success:	Object ID of the named datatype.
 *		Failure:	Negative
 *
 * Programmer:	Mohamad Chaarawi
 *              April 2013
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Topen_ff(hid_t loc_id, const char *name, hid_t tapl_id, hid_t rcxt_id, hid_t estack_id)
{
    void    *vol_dt = NULL;       /* datatype token from VOL plugin */
    H5VL_object_t *obj = NULL;        /* object token of loc_id */
    hid_t dxpl_id = H5P_DATASET_XFER_DEFAULT; /* transfer property list to pass to the VOL plugin */
    H5P_genplist_t *plist;     /* Property list pointer */
    H5_priv_request_t *request = NULL; /* private request struct inserted in event queue */
    void **req = NULL; /* pointer to plugin generate requests (Stays NULL if plugin does not support async */
    H5VL_loc_params_t loc_params;
    hid_t     ret_value = FAIL;      /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE5("i", "i*siii", loc_id, name, tapl_id, rcxt_id, estack_id);

    /* Check args */
     if(!name || !*name)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name")

    /* Get correct property list */
    if(H5P_DEFAULT == tapl_id)
        tapl_id = H5P_DATATYPE_ACCESS_DEFAULT;
    else
        if(TRUE != H5P_isa_class(tapl_id, H5P_DATATYPE_ACCESS))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not datatype access property list")

    loc_params.type = H5VL_OBJECT_BY_SELF;
    loc_params.obj_type = H5I_get_type(loc_id);

    /* get the location object */
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

    /* Create the datatype through the VOL */
    if(NULL == (vol_dt = H5VL_datatype_open(obj->vol_obj, loc_params, obj->vol_info->vol_cls, 
                                            name, tapl_id, dxpl_id, req)))
	HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, FAIL, "unable to open datatype")

    if(request && *req)
        if(H5ES_insert(estack_id, request) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to insert request in event stack")

    /* Get an atom for the datatype */
    if((ret_value = H5VL_register_id(H5I_DATATYPE, vol_dt, obj->vol_info, TRUE)) < 0)
	HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to atomize datatype handle")

done:
    if (ret_value < 0 && vol_dt)
        if(H5VL_datatype_close (vol_dt, obj->vol_info->vol_cls, dxpl_id, H5_REQUEST_NULL) < 0)
            HDONE_ERROR(H5E_SYM, H5E_CLOSEERROR, FAIL, "unable to release datatype")
    FUNC_LEAVE_API(ret_value)
} /* end H5Topen_ff() */


/*-------------------------------------------------------------------------
 * Function:	H5Tclose_ff
 *
 * Purpose:	Frees a datatype and all associated memory.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Mohamad Chaarawi
 *              April 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Tclose_ff(hid_t type_id, hid_t estack_id)
{
    H5T_t   *dt;                    /* Pointer to datatype to close */
    H5VL_object_t *obj;
    herr_t   ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "ii", type_id, estack_id);

    /* Check args */
    if(NULL == (dt = (H5T_t *)H5I_object_verify(type_id, H5I_DATATYPE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a datatype")
    if(H5T_STATE_IMMUTABLE == dt->shared->state)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "immutable datatype")

    if(NULL != (obj = (H5VL_object_t *)dt->vol_obj)) {
        obj->close_estack_id = estack_id;
        obj->close_dxpl_id = H5AC_dxpl_id;
    }

    /* When the reference count reaches zero the resources are freed */
    if(H5I_dec_app_ref(type_id) < 0)
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "problem freeing id")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Tclose_ff() */


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

herr_t
H5DOappend(hid_t dset_id, hid_t dxpl_id, unsigned axis, size_t extension, 
           hid_t memtype, const void *buf)
{
    hsize_t  size[H5S_MAX_RANK];
    hsize_t  start[H5S_MAX_RANK];
    hsize_t  count[H5S_MAX_RANK];
    hsize_t  stride[H5S_MAX_RANK];
    hsize_t  block[H5S_MAX_RANK];
    hsize_t  old_size=0; /* the size of the dimension to be extended */
    int      ndims, i; /* number of dimensions in dataspace */
    hid_t    space_id = FAIL; /* old File space */
    hid_t    new_space_id = FAIL; /* new file space (after extension) */
    hid_t    mem_space_id = FAIL; /* memory space for data buffer */
    hsize_t nelmts; /* number of elements in selection */
    herr_t   ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)
    H5TRACE6("e", "iiIuzi*x", dset_id, dxpl_id, axis, extension, memtype, buf);

    /* check arguments */
    if(H5I_DATASET != H5I_get_type(dset_id))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset");        

    /* Get the default dataset transfer property list if the user didn't provide one */
    if(H5P_DEFAULT == dxpl_id)
        dxpl_id= H5P_DATASET_XFER_DEFAULT;
    else
        if(TRUE != H5P_isa_class(dxpl_id, H5P_DATASET_XFER))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not xfer parms");

    /* get the dataspace of the dataset */
    if(FAIL == (space_id = H5Dget_space(dset_id)))
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTGET, FAIL, "unable to get dataspace");

    /* get the rank of this dataspace */
    if((ndims = H5Sget_simple_extent_ndims(space_id)) < 0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTGET, FAIL, "unable to get dataspace dimesnsion");

    if((int)axis >= ndims)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "Invalid axis");

    /* get the dimensions sizes of the dataspace */
    if(H5Sget_simple_extent_dims(space_id, size, NULL) < 0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTGET, FAIL, "unable to get dataspace dimesnsion sizes");

    /* adjust the dimension size of the requested dimension, 
       but first record the old dimension size */
    old_size = size[axis];
    size[axis] += extension;
    if(extension < old_size)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "extend size is smaller than current size of axis");

    /* set the extent of the dataset to the new dimension */
    if(H5Dset_extent(dset_id, size) < 0)
	HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to set extent of dataset");

    /* get the new dataspace of the dataset */
    if(FAIL == (new_space_id = H5Dget_space(dset_id)))
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTGET, FAIL, "unable to get dataspace");

    /* select a hyperslab corresponding to the append operation */
    for(i=0 ; i<ndims ; i++) {
        start[i] = 0;
        stride[i] = 1;
        count[i] = size[i];
        block[i] = 1;
        if(i == (int)axis) {
            count[i] = extension;
            start[i] = old_size;
        }
    }
    if(FAIL == H5Sselect_hyperslab(new_space_id, H5S_SELECT_SET, start, stride, count, block))
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTSET, FAIL, "unable to set selection in dataspace");

    nelmts = H5Sget_select_npoints(new_space_id);

    /* create a memory space */
    mem_space_id = H5Screate_simple(1, &nelmts, NULL);

    /* Write the data */
    if(H5Dwrite(dset_id, memtype, mem_space_id, new_space_id, dxpl_id, buf) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "can't write data")
done:

    /* close old dataspace */
    if(space_id != FAIL && H5Sclose(space_id) < 0)
        HDONE_ERROR(H5E_DATASPACE, H5E_CANTDEC, FAIL, "unable to close dataspace")

    /* close new dataspace */
    if(new_space_id != FAIL && H5Sclose(new_space_id) < 0)
        HDONE_ERROR(H5E_DATASPACE, H5E_CANTDEC, FAIL, "unable to close dataspace")

    /* close memory dataspace */
    if(mem_space_id != FAIL && H5Sclose(mem_space_id) < 0)
        HDONE_ERROR(H5E_DATASPACE, H5E_CANTDEC, FAIL, "unable to close dataspace")

    FUNC_LEAVE_API(ret_value)
}/* end H5DOappend */

herr_t
H5DOsequence(hid_t dset_id, hid_t dxpl_id, unsigned axis, hsize_t start_off, 
             size_t sequence, hid_t memtype, void *buf)
{
    hsize_t  size[H5S_MAX_RANK];
    hsize_t  start[H5S_MAX_RANK];
    hsize_t  count[H5S_MAX_RANK];
    hsize_t  stride[H5S_MAX_RANK];
    hsize_t  block[H5S_MAX_RANK];
    int      ndims, i; /* number of dimensions in dataspace */
    hid_t    space_id = FAIL; /* old File space */
    hid_t    mem_space_id = FAIL; /* memory space for data buffer */
    hsize_t nelmts; /* number of elements in selection */
    herr_t   ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)
    H5TRACE7("e", "iiIuhzi*x", dset_id, dxpl_id, axis, start_off, sequence, memtype,
             buf);

    /* check arguments */
    if(!dset_id)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset");

    /* Get the default dataset transfer property list if the user didn't provide one */
    if(H5P_DEFAULT == dxpl_id)
        dxpl_id= H5P_DATASET_XFER_DEFAULT;
    else
        if(TRUE != H5P_isa_class(dxpl_id, H5P_DATASET_XFER))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not xfer parms");

    /* get the dataspace of the dataset */
    if(FAIL == (space_id = H5Dget_space(dset_id)))
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTGET, FAIL, "unable to get dataspace");

    /* get the rank of this dataspace */
    if((ndims = H5Sget_simple_extent_ndims(space_id)) < 0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTGET, FAIL, "unable to get dataspace dimesnsion");

    if((int)axis >= ndims)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "Invalid axis");

    /* get the dimensions sizes of the dataspace */
    if(H5Sget_simple_extent_dims(space_id, size, NULL) < 0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTGET, FAIL, "unable to get dataspace dimesnsion sizes");

    /* select a hyperslab corresponding to the append operation */
    for(i=0 ; i<ndims ; i++) {
        start[i] = 0;
        stride[i] = 1;
        count[i] = size[i];
        block[i] = 1;
        if(i == (int)axis) {
            count[i] = sequence;
            start[i] = start_off;
        }
    }
    if(FAIL == H5Sselect_hyperslab(space_id, H5S_SELECT_SET, start, stride, count, block))
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTSET, FAIL, "unable to set selection in dataspace");


    nelmts = H5Sget_select_npoints(space_id);

    /* create a memory space */
    mem_space_id = H5Screate_simple(1, &nelmts, NULL);

    /* Read the data */
    if(H5Dread(dset_id, memtype, mem_space_id, space_id, dxpl_id, buf) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "can't write data")
done:

    /* close old dataspace */
    if(space_id != FAIL && H5Sclose(space_id) < 0)
        HDONE_ERROR(H5E_DATASPACE, H5E_CANTDEC, FAIL, "unable to close dataspace")

    /* close memory dataspace */
    if(mem_space_id != FAIL && H5Sclose(mem_space_id) < 0)
        HDONE_ERROR(H5E_DATASPACE, H5E_CANTDEC, FAIL, "unable to close dataspace")

    FUNC_LEAVE_API(ret_value)
}/* end H5DOsequence */

herr_t H5DOset(hid_t dset_id, hid_t dxpl_id, const hsize_t coord[],
               hid_t memtype, const void *buf)
{
    hid_t    space_id = FAIL; /* old File space */
    hid_t    mem_space_id = FAIL; /* memory space for data buffer */
    hsize_t  nelmts = 1;
    herr_t   ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)

    /* check arguments */
    if(!dset_id)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset");

    /* Get the default dataset transfer property list if the user didn't provide one */
    if(H5P_DEFAULT == dxpl_id)
        dxpl_id= H5P_DATASET_XFER_DEFAULT;
    else
        if(TRUE != H5P_isa_class(dxpl_id, H5P_DATASET_XFER))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not xfer parms");

    /* get the dataspace of the dataset */
    if(FAIL == (space_id = H5Dget_space(dset_id)))
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTGET, FAIL, "unable to get dataspace");

    if(FAIL == H5Sselect_elements(space_id, H5S_SELECT_SET, (size_t)nelmts, coord))
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTSET, FAIL, "unable to set selection in dataspace");

    /* create a memory space */
    mem_space_id = H5Screate_simple(1, &nelmts, NULL);

    /* Write the data */
    if(H5Dwrite(dset_id, memtype, mem_space_id, space_id, dxpl_id, buf) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "can't write data")
done:

    /* close old dataspace */
    if(space_id != FAIL && H5Sclose(space_id) < 0)
        HDONE_ERROR(H5E_DATASPACE, H5E_CANTDEC, FAIL, "unable to close dataspace")

    /* close memory dataspace */
    if(mem_space_id != FAIL && H5Sclose(mem_space_id) < 0)
        HDONE_ERROR(H5E_DATASPACE, H5E_CANTDEC, FAIL, "unable to close dataspace")

    FUNC_LEAVE_API(ret_value)
}/* end H5DOset */

herr_t H5DOget(hid_t dset_id, hid_t dxpl_id, const hsize_t coord[],
               hid_t memtype, void *buf)
{
    hid_t    space_id = FAIL; /* old File space */
    hid_t    mem_space_id = FAIL; /* memory space for data buffer */
    hsize_t  nelmts = 1;
    herr_t   ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)

    /* check arguments */
    if(!dset_id)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset");

    /* Get the default dataset transfer property list if the user didn't provide one */
    if(H5P_DEFAULT == dxpl_id)
        dxpl_id= H5P_DATASET_XFER_DEFAULT;
    else
        if(TRUE != H5P_isa_class(dxpl_id, H5P_DATASET_XFER))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not xfer parms");

    /* get the dataspace of the dataset */
    if(FAIL == (space_id = H5Dget_space(dset_id)))
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTGET, FAIL, "unable to get dataspace");

    if(FAIL == H5Sselect_elements(space_id, H5S_SELECT_SET, (size_t)nelmts, coord))
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTSET, FAIL, "unable to set selection in dataspace");

    /* create a memory space */
    mem_space_id = H5Screate_simple(1, &nelmts, NULL);

    /* Write the data */
    if(H5Dread(dset_id, memtype, mem_space_id, space_id, dxpl_id, buf) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "can't write data")
done:

    /* close old dataspace */
    if(space_id != FAIL && H5Sclose(space_id) < 0)
        HDONE_ERROR(H5E_DATASPACE, H5E_CANTDEC, FAIL, "unable to close dataspace")

    /* close memory dataspace */
    if(mem_space_id != FAIL && H5Sclose(mem_space_id) < 0)
        HDONE_ERROR(H5E_DATASPACE, H5E_CANTDEC, FAIL, "unable to close dataspace")

    FUNC_LEAVE_API(ret_value)
}/* end H5DOget */


herr_t H5DOappend_ff(hid_t dset_id, hid_t dxpl_id, unsigned axis, size_t extension, 
                     hid_t memtype, const void *buf, hid_t trans_id, hid_t estack_id)
{
    hsize_t  size[H5S_MAX_RANK];
    hsize_t  start[H5S_MAX_RANK];
    hsize_t  count[H5S_MAX_RANK];
    hsize_t  stride[H5S_MAX_RANK];
    hsize_t  block[H5S_MAX_RANK];
    hsize_t  old_size=0; /* the size of the dimension to be extended */
    int      ndims, i; /* number of dimensions in dataspace */
    hid_t    space_id = FAIL; /* old File space */
    hid_t    new_space_id = FAIL; /* new file space (after extension) */
    hid_t    mem_space_id = FAIL; /* memory space for data buffer */
    hsize_t nelmts; /* number of elements in selection */
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t   ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)

    /* check arguments */
    if(!dset_id)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset");

    /* Get the default dataset transfer property list if the user didn't provide one */
    if(H5P_DEFAULT == dxpl_id)
        dxpl_id= H5P_DATASET_XFER_DEFAULT;
    else
        if(TRUE != H5P_isa_class(dxpl_id, H5P_DATASET_XFER))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not xfer parms");

    /* store the transaction ID in the dxpl */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")
    if(H5P_set(plist, H5VL_TRANS_ID, &trans_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't set property value for trans_id")

    /* get the dataspace of the dataset */
    if(FAIL == (space_id = H5Dget_space(dset_id)))
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTGET, FAIL, "unable to get dataspace");

    /* get the rank of this dataspace */
    if((ndims = H5Sget_simple_extent_ndims(space_id)) < 0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTGET, FAIL, "unable to get dataspace dimesnsion");

    if((int)axis >= ndims)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "Invalid axis");

    /* get the dimensions sizes of the dataspace */
    if(H5Sget_simple_extent_dims(space_id, size, NULL) < 0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTGET, FAIL, "unable to get dataspace dimesnsion sizes");

    /* adjust the dimension size of the requested dimension, 
       but first record the old dimension size */
    old_size = size[axis];
    size[axis] += extension;
    if(extension < old_size)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "extend size is smaller than current size of axis");

    /* set the extent of the dataset to the new dimension */
    if(H5Dset_extent_ff(dset_id, size, trans_id, estack_id) < 0)
	HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to set extent of dataset");

    /* get the new dataspace of the dataset */
    if(FAIL == (new_space_id = H5Dget_space(dset_id)))
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTGET, FAIL, "unable to get dataspace");

    /* select a hyperslab corresponding to the append operation */
    for(i=0 ; i<ndims ; i++) {
        start[i] = 0;
        stride[i] = 1;
        count[i] = size[i];
        block[i] = 1;
        if(i == (int)axis) {
            count[i] = extension;
            start[i] = old_size;
        }
    }
    if(FAIL == H5Sselect_hyperslab(new_space_id, H5S_SELECT_SET, start, stride, count, block))
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTSET, FAIL, "unable to set selection in dataspace");

    nelmts = H5Sget_select_npoints(new_space_id);

    /* create a memory space */
    mem_space_id = H5Screate_simple(1, &nelmts, NULL);

    /* Write the data */
    if(H5Dwrite_ff(dset_id, memtype, mem_space_id, new_space_id, dxpl_id, buf, trans_id, estack_id) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "can't write data")
done:

    /* close old dataspace */
    if(space_id != FAIL && H5Sclose(space_id) < 0)
        HDONE_ERROR(H5E_DATASPACE, H5E_CANTDEC, FAIL, "unable to close dataspace")

    /* close new dataspace */
    if(new_space_id != FAIL && H5Sclose(new_space_id) < 0)
        HDONE_ERROR(H5E_DATASPACE, H5E_CANTDEC, FAIL, "unable to close dataspace")

    /* close memory dataspace */
    if(mem_space_id != FAIL && H5Sclose(mem_space_id) < 0)
        HDONE_ERROR(H5E_DATASPACE, H5E_CANTDEC, FAIL, "unable to close dataspace")

    FUNC_LEAVE_API(ret_value)
}/* end H5DOappend_ff */

herr_t
H5DOsequence_ff(hid_t dset_id, hid_t dxpl_id, unsigned axis, hsize_t start_off, 
                size_t sequence, hid_t memtype, void *buf, hid_t rcxt_id, hid_t estack_id)
{
    hsize_t  size[H5S_MAX_RANK];
    hsize_t  start[H5S_MAX_RANK];
    hsize_t  count[H5S_MAX_RANK];
    hsize_t  stride[H5S_MAX_RANK];
    hsize_t  block[H5S_MAX_RANK];
    int      ndims, i; /* number of dimensions in dataspace */
    hid_t    space_id = FAIL; /* old File space */
    hid_t    mem_space_id = FAIL; /* memory space for data buffer */
    hsize_t nelmts; /* number of elements in selection */
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t   ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)
    H5TRACE9("e", "iiIuhzi*xii", dset_id, dxpl_id, axis, start_off, sequence,
             memtype, buf, rcxt_id, estack_id);

    /* check arguments */
    if(!dset_id)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset");

    /* Get the default dataset transfer property list if the user didn't provide one */
    if(H5P_DEFAULT == dxpl_id)
        dxpl_id= H5P_DATASET_XFER_DEFAULT;
    else
        if(TRUE != H5P_isa_class(dxpl_id, H5P_DATASET_XFER))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not xfer parms");

    /* store the transaction ID in the dxpl */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")
    if(H5P_set(plist, H5VL_CONTEXT_ID, &rcxt_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't set property value for rcxt_id")

    /* get the dataspace of the dataset */
    if(FAIL == (space_id = H5Dget_space(dset_id)))
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTGET, FAIL, "unable to get dataspace");

    /* get the rank of this dataspace */
    if((ndims = H5Sget_simple_extent_ndims(space_id)) < 0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTGET, FAIL, "unable to get dataspace dimesnsion");

    if((int)axis >= ndims)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "Invalid axis");

    /* get the dimensions sizes of the dataspace */
    if(H5Sget_simple_extent_dims(space_id, size, NULL) < 0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTGET, FAIL, "unable to get dataspace dimesnsion sizes");

    /* select a hyperslab corresponding to the append operation */
    for(i=0 ; i<ndims ; i++) {
        start[i] = 0;
        stride[i] = 1;
        count[i] = size[i];
        block[i] = 1;
        if(i == (int)axis) {
            count[i] = sequence;
            start[i] = start_off;
        }
    }
    if(FAIL == H5Sselect_hyperslab(space_id, H5S_SELECT_SET, start, stride, count, block))
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTSET, FAIL, "unable to set selection in dataspace");

    nelmts = H5Sget_select_npoints(space_id);

    /* create a memory space */
    mem_space_id = H5Screate_simple(1, &nelmts, NULL);

    /* Read the data */
    if(H5Dread_ff(dset_id, memtype, mem_space_id, space_id, dxpl_id, buf, rcxt_id, estack_id) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "can't write data")
done:

    /* close old dataspace */
    if(space_id != FAIL && H5Sclose(space_id) < 0)
        HDONE_ERROR(H5E_DATASPACE, H5E_CANTDEC, FAIL, "unable to close dataspace")

    /* close memory dataspace */
    if(mem_space_id != FAIL && H5Sclose(mem_space_id) < 0)
        HDONE_ERROR(H5E_DATASPACE, H5E_CANTDEC, FAIL, "unable to close dataspace")

    FUNC_LEAVE_API(ret_value)
}/* end H5DOsequence_ff */

herr_t H5DOset_ff(hid_t dset_id, hid_t dxpl_id, const hsize_t coord[],
                  hid_t memtype, const void *buf, hid_t trans_id, hid_t estack_id)
{
    hid_t    space_id = FAIL; /* old File space */
    hid_t    mem_space_id = FAIL; /* memory space for data buffer */
    hsize_t  nelmts = 1;
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t   ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)

    /* check arguments */
    if(!dset_id)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset");

    /* Get the default dataset transfer property list if the user didn't provide one */
    if(H5P_DEFAULT == dxpl_id)
        dxpl_id= H5P_DATASET_XFER_DEFAULT;
    else
        if(TRUE != H5P_isa_class(dxpl_id, H5P_DATASET_XFER))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not xfer parms");

    /* store the transaction ID in the dxpl */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")
    if(H5P_set(plist, H5VL_TRANS_ID, &trans_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't set property value for trans_id")

    /* get the dataspace of the dataset */
    if(FAIL == (space_id = H5Dget_space(dset_id)))
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTGET, FAIL, "unable to get dataspace");

    if(FAIL == H5Sselect_elements(space_id, H5S_SELECT_SET, (size_t)nelmts, coord))
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTSET, FAIL, "unable to set selection in dataspace");

    /* create a memory space */
    mem_space_id = H5Screate_simple(1, &nelmts, NULL);

    /* Write the data */
    if(H5Dwrite_ff(dset_id, memtype, mem_space_id, space_id, dxpl_id, buf, trans_id, estack_id) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "can't write data")
done:

    /* close old dataspace */
    if(space_id != FAIL && H5Sclose(space_id) < 0)
        HDONE_ERROR(H5E_DATASPACE, H5E_CANTDEC, FAIL, "unable to close dataspace")

    /* close memory dataspace */
    if(mem_space_id != FAIL && H5Sclose(mem_space_id) < 0)
        HDONE_ERROR(H5E_DATASPACE, H5E_CANTDEC, FAIL, "unable to close dataspace")

    FUNC_LEAVE_API(ret_value)
}/* end H5DOset_ff */

herr_t H5DOget_ff(hid_t dset_id, hid_t dxpl_id, const hsize_t coord[],
                  hid_t memtype, void *buf, hid_t rcxt_id, hid_t estack_id)
{
    hid_t    space_id = FAIL; /* old File space */
    hid_t    mem_space_id = FAIL; /* memory space for data buffer */
    hsize_t  nelmts = 1;
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t   ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)

    /* check arguments */
    if(!dset_id)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset");

    /* Get the default dataset transfer property list if the user didn't provide one */
    if(H5P_DEFAULT == dxpl_id)
        dxpl_id= H5P_DATASET_XFER_DEFAULT;
    else
        if(TRUE != H5P_isa_class(dxpl_id, H5P_DATASET_XFER))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not xfer parms");

    /* store the transaction ID in the dxpl */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")
    if(H5P_set(plist, H5VL_CONTEXT_ID, &rcxt_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't set property value for rcxt_id")

    /* get the dataspace of the dataset */
    if(FAIL == (space_id = H5Dget_space(dset_id)))
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTGET, FAIL, "unable to get dataspace");

    if(FAIL == H5Sselect_elements(space_id, H5S_SELECT_SET, (size_t)nelmts, coord))
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTSET, FAIL, "unable to set selection in dataspace");

    /* create a memory space */
    mem_space_id = H5Screate_simple(1, &nelmts, NULL);

    /* Write the data */
    if(H5Dread_ff(dset_id, memtype, mem_space_id, space_id, dxpl_id, buf, rcxt_id, estack_id) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "can't write data")
done:

    /* close old dataspace */
    if(space_id != FAIL && H5Sclose(space_id) < 0)
        HDONE_ERROR(H5E_DATASPACE, H5E_CANTDEC, FAIL, "unable to close dataspace")

    /* close memory dataspace */
    if(mem_space_id != FAIL && H5Sclose(mem_space_id) < 0)
        HDONE_ERROR(H5E_DATASPACE, H5E_CANTDEC, FAIL, "unable to close dataspace")

    FUNC_LEAVE_API(ret_value)
}/* end H5DOget_ff */


/*-------------------------------------------------------------------------
 * Function:	H5Aprefetch_ff
 *
 * Purpose:	Prefetched a Dataset from Central Storage to Burst Buffer.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Mohamad Chaarawi
 *              February 2014
 *
 *-------------------------------------------------------------------------
 */
H5_DLL herr_t H5Aprefetch_ff(hid_t attr_id, hid_t rcxt_id, hrpl_t *replica_id,
                             hid_t dxpl_id, hid_t estack_id)
{
    H5_priv_request_t *request = NULL; /* private request struct inserted in event queue */
    void **req = NULL;       /* pointer to plugin generate requests (Stays NULL if plugin does not support async */
    H5VL_object_t *attr = NULL;       /* pointer to attr object */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)

    /* get the map object */
    if(NULL == (attr = (H5VL_object_t *)H5I_object_verify(attr_id, H5I_ATTR)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid attribute identifier")

    /* Get correct property list */
    if(H5P_DEFAULT == dxpl_id)
        dxpl_id = H5P_DATASET_XFER_DEFAULT;
    else
        if(TRUE != H5P_isa_class(dxpl_id, H5P_DATASET_XFER))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not data transfer property list")

    if(estack_id != H5_EVENT_STACK_NULL) {
        /* create the private request */
        if(NULL == (request = (H5_priv_request_t *)H5MM_calloc(sizeof(H5_priv_request_t))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
        req = &request->req;
        request->vol_cls = attr->vol_info->vol_cls;
    }

    /* Get the data through the IOD VOL */
    if((ret_value = H5VL_iod_prefetch(attr->vol_obj, rcxt_id, replica_id, dxpl_id, req)) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't prefetch attribute")

    if(request && *req)
        if(H5ES_insert(estack_id, request) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to insert request in event stack")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Aprefetch_ff() */


/*-------------------------------------------------------------------------
 * Function:	H5Aevict_ff
 *
 * Purpose:	Evicts a Dataset from Burst Buffer.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Mohamad Chaarawi
 *              February 2014
 *
 *-------------------------------------------------------------------------
 */
H5_DLL herr_t H5Aevict_ff(hid_t attr_id, uint64_t c_version, hid_t dxpl_id, hid_t estack_id)
{
    H5_priv_request_t  *request = NULL; /* private request struct inserted in event queue */
    void    **req = NULL;       /* pointer to plugin generated request pointer */
    H5VL_object_t *attr = NULL;       /* pointer to attr object */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)

    /* get the map object */
    if(NULL == (attr = (H5VL_object_t *)H5I_object_verify(attr_id, H5I_ATTR)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid attribute identifier")

    /* Get correct property list */
    if(H5P_DEFAULT == dxpl_id)
        dxpl_id = H5P_DATASET_XFER_DEFAULT;
    else
        if(TRUE != H5P_isa_class(dxpl_id, H5P_DATASET_XFER))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not dataset access property list")

    if(estack_id != H5_EVENT_STACK_NULL) {
        /* create the private request */
        if(NULL == (request = (H5_priv_request_t *)H5MM_calloc(sizeof(H5_priv_request_t))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
        req = &request->req;
        request->vol_cls = attr->vol_info->vol_cls;
    }

    /* Get the data through the IOD VOL */
    if((ret_value = H5VL_iod_evict(attr, c_version, dxpl_id, req)) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't evict attribute")

    if(request && *req)
        if(H5ES_insert(estack_id, request) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to insert request in event stack")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Aevict_ff() */


/*-------------------------------------------------------------------------
 * Function:	H5Dprefetch_ff
 *
 * Purpose:	Prefetched a Dataset from Central Storage to Burst Buffer.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Mohamad Chaarawi
 *              February 2014
 *
 *-------------------------------------------------------------------------
 */
H5_DLL herr_t H5Dprefetch_ff(hid_t dset_id, hid_t rcxt_id, hrpl_t *replica_id,
                          hid_t dxpl_id, hid_t estack_id)
{
    H5_priv_request_t  *request = NULL; /* private request struct inserted in event queue */
    void    **req = NULL;       /* pointer to plugin generate requests (Stays NULL if plugin does not support async */
    H5VL_object_t *dset = NULL;        /* pointer to dset object */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)

    /* get the map object */
    if(NULL == (dset = (H5VL_object_t *)H5I_object_verify(dset_id, H5I_DATASET)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid dataset identifier")

    /* Get correct property list */
    if(H5P_DEFAULT == dxpl_id)
        dxpl_id = H5P_DATASET_XFER_DEFAULT;
    else
        if(TRUE != H5P_isa_class(dxpl_id, H5P_DATASET_XFER))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not dataset access property list")

    if(estack_id != H5_EVENT_STACK_NULL) {
        /* create the private request */
        if(NULL == (request = (H5_priv_request_t *)H5MM_calloc(sizeof(H5_priv_request_t))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
        req = &request->req;
        request->vol_cls = dset->vol_info->vol_cls;
    }

    /* Get the data through the IOD VOL */
    if((ret_value = H5VL_iod_prefetch(dset->vol_obj, rcxt_id, replica_id, dxpl_id, req)) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't prefetch dataset")

    if(request && *req)
        if(H5ES_insert(estack_id, request) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to insert request in event stack")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Dprefetch_ff() */


/*-------------------------------------------------------------------------
 * Function:	H5Devict_ff
 *
 * Purpose:	Evicts a Dataset from Burst Buffer.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Mohamad Chaarawi
 *              February 2014
 *
 *-------------------------------------------------------------------------
 */
H5_DLL herr_t H5Devict_ff(hid_t dset_id, uint64_t c_version, hid_t dxpl_id, hid_t estack_id)
{
    H5_priv_request_t  *request = NULL; /* private request struct inserted in event queue */
    void **req = NULL;       /* pointer to plugin generated request pointer */
    H5VL_object_t *dset = NULL;       /* pointer to dset object */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)

    /* get the map object */
    if(NULL == (dset = (H5VL_object_t *)H5I_object_verify(dset_id, H5I_DATASET)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid dataset identifier")

    /* Get correct property list */
    if(H5P_DEFAULT == dxpl_id)
        dxpl_id = H5P_DATASET_XFER_DEFAULT;
    else
        if(TRUE != H5P_isa_class(dxpl_id, H5P_DATASET_XFER))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not dataset access property list")

    if(estack_id != H5_EVENT_STACK_NULL) {
        /* create the private request */
        if(NULL == (request = (H5_priv_request_t *)H5MM_calloc(sizeof(H5_priv_request_t))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
        req = &request->req;
        request->vol_cls = dset->vol_info->vol_cls;
    }

    /* Get the data through the IOD VOL */
    if((ret_value = H5VL_iod_evict(dset->vol_obj, c_version, dxpl_id, req)) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't evict dataset")

    if(request && *req)
        if(H5ES_insert(estack_id, request) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to insert request in event stack")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Devict_ff() */


/*-------------------------------------------------------------------------
 * Function:	H5Gprefetch_ff
 *
 * Purpose:	Prefetched a Group from Central Storage to Burst Buffer.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Mohamad Chaarawi
 *              February 2014
 *
 *-------------------------------------------------------------------------
 */
H5_DLL herr_t H5Gprefetch_ff(hid_t grp_id, hid_t rcxt_id, hrpl_t *replica_id,
                          hid_t dxpl_id, hid_t estack_id)
{
    H5_priv_request_t  *request = NULL; /* private request struct inserted in event queue */
    void    **req = NULL; /* pointer to plugin generate requests (Stays NULL if plugin does not support async */
    H5VL_object_t *grp = NULL;        /* pointer to grp object */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)

    /* get the map object */
    if(NULL == (grp = (H5VL_object_t *)H5I_object_verify(grp_id, H5I_GROUP)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid group identifier")

    /* Get correct property list */
    if(H5P_DEFAULT == dxpl_id)
        dxpl_id = H5P_DATASET_XFER_DEFAULT;
    else
        if(TRUE != H5P_isa_class(dxpl_id, H5P_DATASET_XFER))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not dataset access property list")

    if(estack_id != H5_EVENT_STACK_NULL) {
        /* create the private request */
        if(NULL == (request = (H5_priv_request_t *)H5MM_calloc(sizeof(H5_priv_request_t))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
        req = &request->req;
        request->vol_cls = grp->vol_info->vol_cls;
    }

    /* Get the data through the IOD VOL */
    if((ret_value = H5VL_iod_prefetch(grp->vol_obj, rcxt_id, replica_id, dxpl_id, req)) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't prefetch group")

    if(request && *req)
        if(H5ES_insert(estack_id, request) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to insert request in event stack")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Gprefetch_ff() */


/*-------------------------------------------------------------------------
 * Function:	H5Gevict_ff
 *
 * Purpose:	Evicts a Group from Burst Buffer.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Mohamad Chaarawi
 *              February 2014
 *
 *-------------------------------------------------------------------------
 */
H5_DLL herr_t H5Gevict_ff(hid_t grp_id, uint64_t c_version, hid_t dxpl_id, hid_t estack_id)
{
    H5_priv_request_t  *request = NULL; /* private request struct inserted in event queue */
    void    **req = NULL;       /* pointer to plugin generated request pointer */
    H5VL_object_t *grp = NULL;       /* pointer to grp object */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)

    /* get the map object */
    if(NULL == (grp = (H5VL_object_t *)H5I_object_verify(grp_id, H5I_GROUP)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid group identifier")

    /* Get correct property list */
    if(H5P_DEFAULT == dxpl_id)
        dxpl_id = H5P_DATASET_XFER_DEFAULT;
    else
        if(TRUE != H5P_isa_class(dxpl_id, H5P_DATASET_XFER))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not dataset access property list")

    if(estack_id != H5_EVENT_STACK_NULL) {
        /* create the private request */
        if(NULL == (request = (H5_priv_request_t *)H5MM_calloc(sizeof(H5_priv_request_t))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
        req = &request->req;
        request->vol_cls = grp->vol_info->vol_cls;
    }

    /* Get the data through the IOD VOL */
    if((ret_value = H5VL_iod_evict(grp->vol_obj, c_version, dxpl_id, req)) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't evict group")

    if(request && *req)
        if(H5ES_insert(estack_id, request) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to insert request in event stack")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Gevict_ff() */


/*-------------------------------------------------------------------------
 * Function:	H5Tprefetch_ff
 *
 * Purpose:	Prefetched a Datatype from Central Storage to Burst Buffer.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Mohamad Chaarawi
 *              February 2014
 *
 *-------------------------------------------------------------------------
 */
H5_DLL herr_t H5Tprefetch_ff(hid_t dtype_id, hid_t rcxt_id, hrpl_t *replica_id,
                          hid_t dxpl_id, hid_t estack_id)
{
    H5_priv_request_t  *request = NULL; /* private request struct inserted in event queue */
    void    **req = NULL;  /* pointer to plugin generate requests (Stays NULL if plugin does not support async */
    H5VL_object_t *dtype = NULL; /* pointer to dtype object */
    H5T_t   *type;         /* Datatype object for ID */
    htri_t   status;       /* Generic status value */
    herr_t   ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)

    /* Check arguments */
    if(NULL == (type = (H5T_t *)H5I_object_verify(dtype_id, H5I_DATATYPE)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a datatype")
    /* Check if the datatype is committed */
    if((status = H5T_is_named(type)) < 0)
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, FAIL, "can't check whether datatype is committed")

    if(FALSE == status) {
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, FAIL, "can't prefetch a non committed datatype");
    }

    /* Get correct property list */
    if(H5P_DEFAULT == dxpl_id)
        dxpl_id = H5P_DATASET_XFER_DEFAULT;
    else
        if(TRUE != H5P_isa_class(dxpl_id, H5P_DATASET_XFER))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not dataset access property list")

    /* get the named datatype object */
    dtype = type->vol_obj;

    if(estack_id != H5_EVENT_STACK_NULL) {
        /* create the private request */
        if(NULL == (request = (H5_priv_request_t *)H5MM_calloc(sizeof(H5_priv_request_t))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
        req = &request->req;
        request->vol_cls = dtype->vol_info->vol_cls;
    }

    /* Get the data through the IOD VOL */
    if((ret_value = H5VL_iod_prefetch(dtype->vol_obj, rcxt_id, replica_id, dxpl_id, req)) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't prefetch datatype")

    if(request && *req)
        if(H5ES_insert(estack_id, request) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to insert request in event stack")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Tprefetch_ff() */


/*-------------------------------------------------------------------------
 * Function:	H5Tevict_ff
 *
 * Purpose:	Evicts a Datatype from Burst Buffer.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Mohamad Chaarawi
 *              February 2014
 *
 *-------------------------------------------------------------------------
 */
H5_DLL herr_t H5Tevict_ff(hid_t dtype_id, uint64_t c_version, hid_t dxpl_id, hid_t estack_id)
{
    H5_priv_request_t  *request = NULL; /* private request struct inserted in event queue */
    void    **req = NULL;  /* pointer to plugin generate requests (Stays NULL if plugin does not support async */
    H5VL_object_t *dtype = NULL; /* pointer to dtype object */
    H5T_t   *type;         /* Datatype object for ID */
    htri_t   status;       /* Generic status value */
    herr_t   ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)

    /* Check arguments */
    if(NULL == (type = (H5T_t *)H5I_object_verify(dtype_id, H5I_DATATYPE)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a datatype")
    /* Check if the datatype is committed */
    if((status = H5T_is_named(type)) < 0)
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, FAIL, "can't check whether datatype is committed")

    if(FALSE == status) {
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, FAIL, "can't prefetch a non committed datatype");
    }

    /* Get correct property list */
    if(H5P_DEFAULT == dxpl_id)
        dxpl_id = H5P_DATASET_XFER_DEFAULT;
    else
        if(TRUE != H5P_isa_class(dxpl_id, H5P_DATASET_XFER))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not dataset access property list")

    /* get the named datatype object */
    dtype = type->vol_obj;

    if(estack_id != H5_EVENT_STACK_NULL) {
        /* create the private request */
        if(NULL == (request = (H5_priv_request_t *)H5MM_calloc(sizeof(H5_priv_request_t))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
        req = &request->req;
        request->vol_cls = dtype->vol_info->vol_cls;
    }

    /* Get the data through the IOD VOL */
    if((ret_value = H5VL_iod_evict(dtype->vol_obj, c_version, dxpl_id, req)) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't evict datatype")

    if(request && *req)
        if(H5ES_insert(estack_id, request) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to insert request in event stack")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Tevict_ff() */


/*-------------------------------------------------------------------------
 * Function:	H5VLiod_get_file_id
 *
 * Purpose:	wraps and hid_t around a container handle from IOD.
 *
 * Return:	Success:	The ID for a new file.
 *		Failure:	FAIL
 *
 * Programmer:	Mohamad Chaarawi
 *		May 28, 2014
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5VLiod_get_file_id(const char *filename, iod_handle_t coh, hid_t fapl_id, hid_t *rcxt_id)
{
    H5VL_iod_file_t *file = NULL;            /* file token from VOL plugin */
    H5P_genplist_t  *plist;          /* Property list pointer */
    H5VL_class_t    *vol_cls;        /* VOL class attached to fapl_id */
    H5VL_plugin_prop_t plugin_prop;         /* Property for vol plugin ID & info */
    H5VL_t  *vol_info;             /* VOL plugin information */
    iod_handles_t root_oh;           /* root object handle */
    iod_cont_trans_stat_t *tids = NULL;
    iod_trans_id_t rtid;
    iod_handle_t mdkv_oh;
    hid_t fcpl_id;
    uint32_t cs_scope;
    iod_ret_t ret;
    scratch_pad sp;
    iod_checksum_t sp_cs = 0;
    iod_obj_id_t root_id = 0;
    hid_t ret_value;

    FUNC_ENTER_API(FAIL)

    /* Check/fix arguments */
    if(!filename || !*filename)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid file name")

    /* get info from the fapl */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(fapl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access property list");
    if(H5P_get(plist, H5VL_CS_BITFLAG_NAME, &cs_scope) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get scope for data integrity checks");

    if(H5P_peek(plist, H5F_ACS_VOL_NAME, &plugin_prop) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get vol plugin info")

    if(NULL == (vol_cls = (H5VL_class_t *)H5I_object_verify(plugin_prop.plugin_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a VOL plugin ID")

    /* setup VOL info struct */
    if(NULL == (vol_info = H5FL_CALLOC(H5VL_t)))
	HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, FAIL, "can't allocate VL info struct")
    vol_info->vol_cls = vol_cls;
    vol_info->vol_id = plugin_prop.plugin_id;
    if(H5I_inc_ref(vol_info->vol_id, FALSE) < 0)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTINC, FAIL, "unable to increment ref count on VOL plugin")

    /* allocate the file object that is returned to the user */
    if(NULL == (file = H5FL_CALLOC(H5VL_iod_file_t)))
	HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, FAIL, "can't allocate IOD file struct");

    file->remote_file.coh = coh;
    file->my_rank = 0;
    file->num_procs = 1;
    /* Duplicate communicator and Info object. */
    if(FAIL == H5FD_mpi_comm_info_dup(MPI_COMM_SELF, MPI_INFO_NULL, &file->comm, &file->info))
	HGOTO_ERROR(H5E_INTERNAL, H5E_CANTCOPY, FAIL, "Communicator/Info duplicate failed");

    ret = iod_query_cont_trans_stat(coh, &tids, NULL);
    if(ret < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "can't get container tids status");
    rtid = tids->latest_rdable;
    file->remote_file.c_version = rtid;
    ret = iod_free_cont_trans_stat(coh, tids);
    if(ret < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "can't free container transaction status object");

    ret = iod_trans_start(coh, &rtid, NULL, 0, IOD_TRANS_R, NULL);
    if(ret < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTSET, FAIL, "can't start transaction");

    /* open the root group */
    IOD_OBJID_SETOWNER_APP(root_id)
    IOD_OBJID_SETTYPE(root_id, IOD_OBJ_KV)
    file->remote_file.root_id = root_id;
    file->remote_file.root_oh.rd_oh.cookie = IOD_OH_UNDEFINED;
    file->remote_file.root_oh.wr_oh.cookie = IOD_OH_UNDEFINED;

    if ((ret = iod_obj_open_read(coh, root_id, rtid, NULL, &root_oh.rd_oh, NULL)) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "can't open root object for read");

    /* get scratch pad of root group */
    if((ret = iod_obj_get_scratch(root_oh.rd_oh, rtid, &sp, &sp_cs, NULL)) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "can't get scratch pad for root object");
    if(sp_cs && (cs_scope & H5_CHECKSUM_IOD)) {
        /* verify scratch pad integrity */
        if(H5VL_iod_verify_scratch_pad(&sp, sp_cs) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "Scratch Pad failed integrity check");
    }

    file->remote_file.mdkv_id = sp[0];
    file->remote_file.attrkv_id = sp[1];
    file->remote_file.oidkv_id = sp[2];

    /* open the metadata KV object */
    ret = iod_obj_open_read(coh, sp[0], rtid, NULL, &mdkv_oh, NULL);
    if(ret < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "can't open MD KV");

    /* retrieve all metadata from scratch pad */
    ret = H5VL_iod_get_metadata(mdkv_oh, rtid, H5VL_IOD_PLIST, H5VL_IOD_KEY_OBJ_CPL,
                                cs_scope, NULL, &fcpl_id);
    if(SUCCEED != ret)
        HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "failed to retrieve fcpl");
    file->remote_file.fcpl_id = fcpl_id;

    file->remote_file.kv_oid_index = 0;
    file->remote_file.array_oid_index = 0;
    file->remote_file.blob_oid_index = 0;

    /* close the metadata KV */
    ret = iod_obj_close(mdkv_oh, NULL, NULL);
    if(ret < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "can't close MD KV");

    ret = iod_obj_close(root_oh.rd_oh, NULL, NULL);
    if(ret < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "can't close root group");

    file->file_name = HDstrdup(filename);
    file->flags = H5F_ACC_RDONLY;
    file->md_integrity_scope = cs_scope;
    if((file->fapl_id = H5Pcopy(fapl_id)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, FAIL, "failed to copy fapl");
    file->nopen_objs = 1;
    file->num_req = 0;
    file->persist_on_close = TRUE;

    /* initialize head and tail of the container's linked list of requests */
    file->request_list_head = NULL;
    file->request_list_tail = NULL;

    file->common.obj_type = H5I_FILE;
    /* The name of the location is the root's object name "\" */
    file->common.obj_name = HDstrdup("/");
    file->common.obj_name[1] = '\0';
    file->common.file = file;

    /* Get an atom for the file with the VOL information as the auxilary struct*/
    if((ret_value = H5VL_register_id(H5I_FILE, file, vol_info, TRUE)) < 0)
	HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to atomize file handle")

    /* determine if we want to acquire the latest readable version
       when the file is opened */
    if(rcxt_id) {
        H5RC_t *rc = NULL;

        /* create a new read context object (if user requested it) */
        if(NULL == (rc = H5RC_create(file, rtid)))
            HGOTO_ERROR(H5E_SYM, H5E_CANTCREATE, FAIL, "unable to create read context");

        rc->vol_cls = vol_info->vol_cls;

        /* Get an atom for the event queue with the VOL information as the auxilary struct */
        if((*rcxt_id = H5I_register(H5I_RC, rc, TRUE)) < 0)
            HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to atomize read context handle");
    }

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLiod_get_file_id() */


/*-------------------------------------------------------------------------
 * Function:	H5VLiod_close_file_id
 *
 * Purpose:	wraps and hid_t around a container handle from IOD.
 *
 * Return:	Success:	The ID for a new file.
 *		Failure:	FAIL
 *
 * Programmer:	Mohamad Chaarawi
 *		May 28, 2014
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLiod_close_file_id(hid_t file_id)
{
    H5VL_object_t *obj;
    H5VL_iod_file_t *file = NULL;            /* file token from VOL plugin */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)
    H5TRACE1("e", "i", file_id);

    /* get the file object */
    if(NULL == (obj = (H5VL_object_t *)H5I_remove_verify(file_id, H5I_FILE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid file identifier")

    file = (H5VL_iod_file_t *)obj->vol_obj;

    free(file->file_name);
    free(file->common.obj_name);
    if(H5FD_mpi_comm_info_free(&file->comm, &file->info) < 0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTFREE, FAIL, "Communicator/Info free failed");
    if(file->common.comment)
        HDfree(file->common.comment);
    if(file->fapl_id != H5P_FILE_ACCESS_DEFAULT && H5Pclose(file->fapl_id) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTDEC, FAIL, "failed to close plist");
    if(file->remote_file.fcpl_id != H5P_FILE_CREATE_DEFAULT && 
       H5Pclose(file->remote_file.fcpl_id) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTDEC, FAIL, "failed to close plist");
    file = H5FL_FREE(H5VL_iod_file_t, file);

    /* free file */
    if(H5VL_free_object(obj) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTDEC, FAIL, "unable to free VOL object")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLiod_close_file_id() */


/*-------------------------------------------------------------------------
 * Function:	H5Dquery
 *
 * Purpose:     returns a dataspace selection of dataset elements that
 *              matches the query
 *
 * Return:	Success:	The ID for a dataspace selection.
 *		Failure:	FAIL
 *
 * Programmer:	Mohamad Chaarawi
 *		May 29, 2014
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Dquery_ff(hid_t dset_id, hid_t query_id, hid_t scope_id, hid_t rcxt_id)
{
    H5VL_object_t *dset = NULL;
    hid_t xxpl_id = FAIL;
    unsigned plugin_id;
    void *buf = NULL;
    hid_t type_id=FAIL, space_id=scope_id, dset_space_id=FAIL;
    hbool_t use_region_scope = TRUE;
    herr_t ret;
    hid_t ret_value = FAIL;

    FUNC_ENTER_API(FAIL)
    H5TRACE4("i", "iiii", dset_id, query_id, scope_id, rcxt_id);

    if(NULL == (dset = (H5VL_object_t *)H5I_object_verify(dset_id, H5I_DATASET)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset");

#ifdef H5_HAVE_INDEXING
    /* Use indexing query callback if it exists */
    if (H5X_PLUGIN_NONE != (plugin_id = H5VL_iod_dataset_get_index_plugin_id(dset->vol_obj))) {
        void *idx_handle = NULL; /* index */
        H5X_class_t *idx_class = NULL;
        H5P_genplist_t *xxpl_plist = NULL, *plist = NULL; /* Property list pointer */

        if (NULL == (idx_handle = H5VL_iod_dataset_get_index(dset->vol_obj)))
            HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get index handle from dataset");
        if (NULL == (idx_class = H5X_registered(plugin_id)))
            HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get index plugin class");

        if(NULL == (plist = (H5P_genplist_t *)H5I_object(H5P_INDEX_XFER_DEFAULT)))
            HGOTO_ERROR(H5E_PLIST, H5E_NOTFOUND, FAIL, "property object doesn't exist");
        if((xxpl_id = H5P_copy_plist((H5P_genplist_t *)plist, TRUE)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTCOPY, FAIL, "can't copy property list");

        /* store the read context ID in the xxpl */
        if (NULL == (xxpl_plist = (H5P_genplist_t *) H5I_object(xxpl_id)))
            HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");
        if (H5P_set(xxpl_plist, H5VL_CONTEXT_ID, &rcxt_id) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't set property value for trans_id");

        if (NULL == idx_class->query)
            HGOTO_ERROR(H5E_INDEX, H5E_BADVALUE, FAIL, "plugin query callback is not defined");

        if(FAIL == H5D__apply_index_query(idx_handle, idx_class, query_id, xxpl_id, &ret_value))
            HGOTO_ERROR(H5E_INDEX, H5E_CANTCLOSEOBJ, FAIL, "cannot query index");

        {
            hsize_t start_coord[H5S_MAX_RANK + 1], end_coord[H5S_MAX_RANK + 1], nelmts;

            if (FAIL == H5Sget_select_bounds(ret_value, start_coord, end_coord))
                HGOTO_ERROR(H5E_DATASPACE, H5E_CANTSELECT, FAIL, "unable to get bounds");
            if (0 == (nelmts = (hsize_t) H5Sget_select_npoints(ret_value)))
                HGOTO_ERROR(H5E_DATASPACE, H5E_BADVALUE, FAIL, "invalid number of elements");
            printf("Created dataspace from index with %llu elements [(%llu, %llu):(%llu, %llu)]\n",
                    nelmts, start_coord[0], start_coord[1], end_coord[0], end_coord[1]);
        }
    }
    else 
#endif
    {
        /* Brute force it */
        size_t elmt_size=0, buf_size=0;
        size_t nelmts;
        H5VL__iod_get_query_data_t udata;

        if((dset_space_id = H5Dget_space(dset_id)) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't get space id from dataset");
        if((type_id = H5Dget_type(dset_id)) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't get space id from dataset");

        if(space_id < 0) {
            use_region_scope = FALSE;
            space_id = dset_space_id;
        }

        nelmts = (size_t) H5Sget_select_npoints(space_id);
        elmt_size = H5Tget_size(type_id);
        buf_size = nelmts * elmt_size;

        /* allocate buffer to hold data */
        if(NULL == (buf = malloc(buf_size)))
            HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate data buffer");

        FUNC_LEAVE_API_THREADSAFE;
        ret = H5Dread_ff(dset_id, type_id, H5S_ALL, space_id, H5P_DEFAULT, 
                               buf, rcxt_id, H5_EVENT_STACK_NULL);
        FUNC_ENTER_API_THREADSAFE;

        if(SUCCEED != ret)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't read data from dataset");

        if(FAIL == (udata.space_query = H5Scopy(space_id)))
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTCOPY, FAIL, "can't copy dataspace")
        if(H5Sselect_none(udata.space_query) < 0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTDELETE, FAIL, "can't change selection")

        udata.query_id = query_id;
        udata.num_elmts = 0;

        /* iterate over every element and apply the query on it. If the
           query is not satisfied, then remove it from the query selection */
        if(H5Diterate(buf, type_id, space_id, H5VL__iod_get_query_data_cb, &udata) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "failed to apply query on Dataset")

        ret_value = udata.space_query;

        {
            hsize_t start_coord[H5S_MAX_RANK + 1], end_coord[H5S_MAX_RANK + 1];

            if (FAIL == H5Sget_select_bounds(ret_value, start_coord, end_coord))
                HGOTO_ERROR(H5E_DATASPACE, H5E_CANTSELECT, FAIL, "unable to get bounds");
            if (0 == (nelmts = (hsize_t) H5Sget_select_npoints(ret_value)))
                HGOTO_ERROR(H5E_DATASPACE, H5E_BADVALUE, FAIL, "invalid number of elements");
            printf("Created dataspace from index with %llu elements [(%llu, %llu):(%llu, %llu)]\n",
                    nelmts, start_coord[0], start_coord[1], end_coord[0], end_coord[1]);
        }

    }

done:
    if(xxpl_id != FAIL && H5I_dec_app_ref(xxpl_id) < 0)
            HDONE_ERROR(H5E_PLIST, H5E_CANTFREE, FAIL, "can't close plist");
    if(space_id != FAIL && H5I_dec_app_ref(space_id) < 0)
        HDONE_ERROR(H5E_DATASPACE, H5E_CANTFREE, FAIL, "can't close dataspace");
    if(use_region_scope)
        if(dset_space_id != FAIL && H5I_dec_app_ref(dset_space_id) < 0)
            HDONE_ERROR(H5E_DATASPACE, H5E_CANTFREE, FAIL, "can't close dataspace");
    if(type_id != FAIL && H5I_dec_app_ref(type_id) < 0)
        HDONE_ERROR(H5E_DATATYPE, H5E_CANTFREE, FAIL, "can't close dataspace");

    if(buf) {
        free(buf);
        buf = NULL;
    }

    FUNC_LEAVE_API(ret_value)
}

static herr_t
H5D__apply_index_query(void *idx_handle, H5X_class_t *idx_class, hid_t query_id, 
                       hid_t xxpl_id, hid_t *space_id)
{
    H5Q_combine_op_t comb_type;
    H5Q_type_t q_type;
    herr_t ret;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_STATIC

    if(H5Qget_combine_op(query_id, &comb_type) < 0)
        HGOTO_ERROR_FF(FAIL, "can't get query op type");

    if(H5Qget_type(query_id, &q_type) < 0)
        HGOTO_ERROR_FF(FAIL, "can't get query op type");

    if(H5Q_SINGLETON == comb_type) {
        if (FAIL == idx_class->query(idx_handle, query_id, xxpl_id, space_id))
            HGOTO_ERROR(H5E_INDEX, H5E_CANTCLOSEOBJ, FAIL, "cannot query index");
    }
    else {
        hid_t qid1, qid2;
        hid_t sid1 = FAIL , sid2 = FAIL;

        if(H5Qget_components(query_id, &qid1, &qid2) < 0)
            HGOTO_ERROR_FF(FAIL, "can't get query components");

        ret = H5D__apply_index_query(idx_handle, idx_class, qid1, xxpl_id, &sid1);
        if(ret != 0)
            HGOTO_ERROR_FF(ret, "Error applying index query");

        ret = H5D__apply_index_query(idx_handle, idx_class, qid2, xxpl_id, &sid2);
        if(ret != 0)
            HGOTO_ERROR_FF(ret, "Error applying index query");
#if 0
        /* combine result1 and result2 */
        if(H5Q_COMBINE_AND == comb_type) {
            if(FAIL == (*space_id = H5Scombine_select(sid1, H5S_SELECT_AND, sid2)))
                HGOTO_ERROR_FF(ret, "Unable to AND 2 dataspace selections");
        }
        else if(H5Q_COMBINE_OR == comb_type) {
            if(FAIL == (*space_id = H5Scombine_select(sid1, H5S_SELECT_OR, sid2)))
                HGOTO_ERROR_FF(ret, "Unable to AND 2 dataspace selections");
        }
        else
#endif
            HGOTO_ERROR_FF(FAIL, "invalid query combine OP");

        if(sid1!=FAIL && H5Sclose(sid1) < 0)
            HGOTO_ERROR_FF(FAIL, "unable to release dataspace");
        if(sid2!=FAIL && H5Sclose(sid2) < 0)
            HGOTO_ERROR_FF(FAIL, "unable to release dataspace");
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
}
#endif /* H5_HAVE_EFF */
