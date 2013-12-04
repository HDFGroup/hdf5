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

/*
 * Programmer:  Mohamad Chaarawi <chaarawi@hdfgroup.gov>
 *              February, 2012
 *
 * Purpose:	The IOD VOL plugin where access is forwarded to the IOD library 
 *              by the function shipper.
 */

/* Interface initialization */
#define H5_INTERFACE_INIT_FUNC	H5VL_iod_init_interface

#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5FDprivate.h"        /* file drivers            		*/
#include "H5FFprivate.h"        /* Fast Forward            		*/
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Pprivate.h"		/* Property lists			*/
#include "H5Sprivate.h"		/* Dataspaces		  		*/
#include "H5VLprivate.h"	/* VOL plugins				*/
#include "H5VLiod.h"            /* Iod VOL plugin			*/
#include "H5VLiod_client.h"     /* Client IOD helper			*/

#ifdef H5_HAVE_EFF

#define H5VL_IOD_MAX_ADDR_NAME 256

/* function shipper IDs for different routines */
static hg_id_t H5VL_EFF_INIT_ID;
static hg_id_t H5VL_EFF_FINALIZE_ID;
static hg_id_t H5VL_ANALYSIS_EXECUTE_ID;
static hg_id_t H5VL_FILE_CREATE_ID;
static hg_id_t H5VL_FILE_OPEN_ID;
static hg_id_t H5VL_FILE_CLOSE_ID;
static hg_id_t H5VL_ATTR_CREATE_ID;
static hg_id_t H5VL_ATTR_OPEN_ID;
static hg_id_t H5VL_ATTR_READ_ID;
static hg_id_t H5VL_ATTR_WRITE_ID;
static hg_id_t H5VL_ATTR_EXISTS_ID;
static hg_id_t H5VL_ATTR_ITERATE_ID;
static hg_id_t H5VL_ATTR_RENAME_ID;
static hg_id_t H5VL_ATTR_REMOVE_ID;
static hg_id_t H5VL_ATTR_CLOSE_ID;
static hg_id_t H5VL_GROUP_CREATE_ID;
static hg_id_t H5VL_GROUP_OPEN_ID;
static hg_id_t H5VL_GROUP_CLOSE_ID;
static hg_id_t H5VL_MAP_CREATE_ID;
static hg_id_t H5VL_MAP_OPEN_ID;
static hg_id_t H5VL_MAP_SET_ID;
static hg_id_t H5VL_MAP_GET_ID;
static hg_id_t H5VL_MAP_GET_COUNT_ID;
static hg_id_t H5VL_MAP_EXISTS_ID;
static hg_id_t H5VL_MAP_ITERATE_ID;
static hg_id_t H5VL_MAP_DELETE_ID;
static hg_id_t H5VL_MAP_CLOSE_ID;
static hg_id_t H5VL_DSET_CREATE_ID;
static hg_id_t H5VL_DSET_OPEN_ID;
static hg_id_t H5VL_DSET_READ_ID;
static hg_id_t H5VL_DSET_GET_VL_SIZE_ID;
static hg_id_t H5VL_DSET_WRITE_ID;
static hg_id_t H5VL_DSET_SET_EXTENT_ID;
static hg_id_t H5VL_DSET_CLOSE_ID;
static hg_id_t H5VL_DTYPE_COMMIT_ID;
static hg_id_t H5VL_DTYPE_OPEN_ID;
static hg_id_t H5VL_DTYPE_CLOSE_ID;
static hg_id_t H5VL_LINK_CREATE_ID;
static hg_id_t H5VL_LINK_MOVE_ID;
static hg_id_t H5VL_LINK_ITERATE_ID;
static hg_id_t H5VL_LINK_EXISTS_ID;
static hg_id_t H5VL_LINK_GET_INFO_ID;
static hg_id_t H5VL_LINK_GET_VAL_ID;
static hg_id_t H5VL_LINK_REMOVE_ID;
static hg_id_t H5VL_OBJECT_OPEN_BY_TOKEN_ID;
static hg_id_t H5VL_OBJECT_OPEN_ID;
static hg_id_t H5VL_OBJECT_COPY_ID;
static hg_id_t H5VL_OBJECT_EXISTS_ID;
static hg_id_t H5VL_OBJECT_VISIT_ID;
static hg_id_t H5VL_OBJECT_SET_COMMENT_ID;
static hg_id_t H5VL_OBJECT_GET_COMMENT_ID;
static hg_id_t H5VL_OBJECT_GET_INFO_ID;
static hg_id_t H5VL_RC_ACQUIRE_ID;
static hg_id_t H5VL_RC_RELEASE_ID;
static hg_id_t H5VL_RC_PERSIST_ID;
static hg_id_t H5VL_RC_SNAPSHOT_ID;
static hg_id_t H5VL_TR_START_ID;
static hg_id_t H5VL_TR_FINISH_ID;
static hg_id_t H5VL_TR_SET_DEPEND_ID;
static hg_id_t H5VL_TR_SKIP_ID;
static hg_id_t H5VL_TR_ABORT_ID;
static hg_id_t H5VL_CANCEL_OP_ID;


/* global AXE list struct */
typedef struct H5VL_iod_axe_list_t {
    H5VL_iod_request_t *head;
    H5VL_iod_request_t *tail;
    AXE_task_t last_released_task;
} H5VL_iod_axe_list_t;

static na_addr_t PEER;
static na_class_t *network_class = NULL;

static AXE_task_t g_axe_id;
static H5VL_iod_axe_list_t axe_list;

/* Prototypes */
static void *H5VL_iod_fapl_copy(const void *_old_fa);
static herr_t H5VL_iod_fapl_free(void *_fa);

/* Atrribute callbacks */
static void *H5VL_iod_attribute_create(void *obj, H5VL_loc_params_t loc_params, const char *attr_name, hid_t acpl_id, hid_t aapl_id, hid_t dxpl_id, void **req);
static void *H5VL_iod_attribute_open(void *obj, H5VL_loc_params_t loc_params, const char *attr_name, hid_t aapl_id, hid_t dxpl_id, void **req);
static herr_t H5VL_iod_attribute_read(void *attr, hid_t dtype_id, void *buf, hid_t dxpl_id, void **req);
static herr_t H5VL_iod_attribute_write(void *attr, hid_t dtype_id, const void *buf, hid_t dxpl_id, void **req);
static herr_t H5VL_iod_attribute_get(void *obj, H5VL_attr_get_t get_type, hid_t dxpl_id, void **req, va_list arguments);
static herr_t H5VL_iod_attribute_remove(void *obj, H5VL_loc_params_t loc_params, const char *attr_name, hid_t dxpl_id, void **req);
static herr_t H5VL_iod_attribute_close(void *attr, hid_t dxpl_id, void **req);

/* Datatype callbacks */
static void *H5VL_iod_datatype_commit(void *obj, H5VL_loc_params_t loc_params, const char *name, hid_t type_id, hid_t lcpl_id, hid_t tcpl_id, hid_t tapl_id, hid_t dxpl_id, void **req);
static void *H5VL_iod_datatype_open(void *obj, H5VL_loc_params_t loc_params, const char *name, hid_t tapl_id, hid_t dxpl_id, void **req);
static ssize_t H5VL_iod_datatype_get_binary(void *obj, unsigned char *buf, size_t size, hid_t dxpl_id, void **req);
static herr_t H5VL_iod_datatype_get(void *obj, H5VL_datatype_get_t get_type, hid_t dxpl_id, void **req, va_list arguments);
static herr_t H5VL_iod_datatype_close(void *dt, hid_t dxpl_id, void **req);

/* Dataset callbacks */
static void *H5VL_iod_dataset_create(void *obj, H5VL_loc_params_t loc_params, const char *name, hid_t dcpl_id, hid_t dapl_id, hid_t dxpl_id, void **req);
static void *H5VL_iod_dataset_open(void *obj, H5VL_loc_params_t loc_params, const char *name, hid_t dapl_id, hid_t dxpl_id, void **req);
static herr_t H5VL_iod_dataset_read(void *dset, hid_t mem_type_id, hid_t mem_space_id,
                                    hid_t file_space_id, hid_t plist_id, void *buf, void **req);
static herr_t H5VL_iod_dataset_write(void *dset, hid_t mem_type_id, hid_t mem_space_id,
                                     hid_t file_space_id, hid_t plist_id, const void *buf, void **req);
static herr_t H5VL_iod_dataset_set_extent(void *dset, const hsize_t size[], hid_t dxpl_id, void **req);
static herr_t H5VL_iod_dataset_get(void *dset, H5VL_dataset_get_t get_type, hid_t dxpl_id, void **req, va_list arguments);
static herr_t H5VL_iod_dataset_close(void *dset, hid_t dxpl_id, void **req);

/* File callbacks */
static void *H5VL_iod_file_create(const char *name, unsigned flags, hid_t fcpl_id, hid_t fapl_id, hid_t dxpl_id, void **req);
static void *H5VL_iod_file_open(const char *name, unsigned flags, hid_t fapl_id, hid_t dxpl_id, void **req);
static herr_t H5VL_iod_file_get(void *file, H5VL_file_get_t get_type, hid_t dxpl_id, void **req, va_list arguments);
static herr_t H5VL_iod_file_close(void *file, hid_t dxpl_id, void **req);

/* Group callbacks */
static void *H5VL_iod_group_create(void *obj, H5VL_loc_params_t loc_params, const char *name, hid_t gcpl_id, hid_t gapl_id, hid_t dxpl_id, void **req);
static void *H5VL_iod_group_open(void *obj, H5VL_loc_params_t loc_params, const char *name, hid_t gapl_id, hid_t dxpl_id, void **req);
static herr_t H5VL_iod_group_get(void *obj, H5VL_group_get_t get_type, hid_t dxpl_id, void **req, va_list arguments);
static herr_t H5VL_iod_group_close(void *grp, hid_t dxpl_id, void **req);

/* Link callbacks */
static herr_t H5VL_iod_link_create(H5VL_link_create_type_t create_type, void *obj, 
                                   H5VL_loc_params_t loc_params, hid_t lcpl_id, hid_t lapl_id, hid_t dxpl_id, void **req);
static herr_t H5VL_iod_link_move(void *src_obj, H5VL_loc_params_t loc_params1,
                                 void *dst_obj, H5VL_loc_params_t loc_params2,
                                 hbool_t copy_flag, hid_t lcpl_id, hid_t lapl_id, hid_t dxpl_id, void **req);
static herr_t H5VL_iod_link_iterate(void *obj, H5VL_loc_params_t loc_params, hbool_t recursive, 
                                    H5_index_t idx_type, H5_iter_order_t order, hsize_t *idx, 
                                    H5L_iterate_t op, void *op_data, hid_t dxpl_id, void **req);
static herr_t H5VL_iod_link_get(void *obj, H5VL_loc_params_t loc_params, H5VL_link_get_t get_type, hid_t dxpl_id, void **req, va_list arguments);
static herr_t H5VL_iod_link_remove(void *obj, H5VL_loc_params_t loc_params, hid_t dxpl_id, void **req);

/* Object callbacks */
static void *H5VL_iod_object_open(void *obj, H5VL_loc_params_t loc_params, H5I_type_t *opened_type, hid_t dxpl_id, void **req);
static herr_t H5VL_iod_object_copy(void *src_obj, H5VL_loc_params_t loc_params1, const char *src_name, 
                                   void *dst_obj, H5VL_loc_params_t loc_params2, const char *dst_name, 
                                   hid_t ocpypl_id, hid_t lcpl_id, hid_t dxpl_id, void **req);
static herr_t H5VL_iod_object_visit(void *obj, H5VL_loc_params_t loc_params, H5_index_t idx_type, 
                                    H5_iter_order_t order, H5O_iterate_t op, void *op_data, hid_t dxpl_id, void **req);
static herr_t H5VL_iod_object_get(void *obj, H5VL_loc_params_t loc_params, H5VL_object_get_t get_type, hid_t dxpl_id, void **req, va_list arguments);
static herr_t H5VL_iod_object_misc(void *obj, H5VL_loc_params_t loc_params, H5VL_object_misc_t misc_type, hid_t dxpl_id, void **req, va_list arguments);

static herr_t H5VL_iod_cancel(void **req, H5ES_status_t *status);
static herr_t H5VL_iod_test(void **req, H5ES_status_t *status);
static herr_t H5VL_iod_wait(void **req, H5ES_status_t *status);

/* IOD-specific file access properties */
typedef struct H5VL_iod_fapl_t {
    MPI_Comm		comm;		/*communicator			*/
    MPI_Info		info;		/*file information		*/
} H5VL_iod_fapl_t;

H5FL_DEFINE(H5VL_iod_file_t);
H5FL_DEFINE(H5VL_iod_attr_t);
H5FL_DEFINE(H5VL_iod_group_t);
H5FL_DEFINE(H5VL_iod_map_t);
H5FL_DEFINE(H5VL_iod_dset_t);
H5FL_DEFINE(H5VL_iod_dtype_t);

static H5VL_class_t H5VL_iod_g = {
    IOD,
    "iod",					/* name */
    NULL,                                       /* initialize */
    NULL,                                       /* terminate */
    sizeof(H5VL_iod_fapl_t),		        /*fapl_size */
    H5VL_iod_fapl_copy,			        /*fapl_copy */
    H5VL_iod_fapl_free, 		        /*fapl_free */
    {                                           /* attribute_cls */
        H5VL_iod_attribute_create,              /* create */
        H5VL_iod_attribute_open,                /* open */
        H5VL_iod_attribute_read,                /* read */
        H5VL_iod_attribute_write,               /* write */
        NULL,//H5VL_iod_attr_iterate,               /* iterate */
        H5VL_iod_attribute_get,                 /* get */
        H5VL_iod_attribute_remove,              /* remove */
        H5VL_iod_attribute_close                /* close */
    },
    {                                           /* datatype_cls */
        H5VL_iod_datatype_commit,               /* commit */
        H5VL_iod_datatype_open,                 /* open */
        H5VL_iod_datatype_get_binary,           /* get_size */
        H5VL_iod_datatype_get,                  /* get */
        H5VL_iod_datatype_close                 /* close */
    },
    {                                           /* dataset_cls */
        H5VL_iod_dataset_create,                /* create */
        H5VL_iod_dataset_open,                  /* open */
        H5VL_iod_dataset_read,                  /* read */
        H5VL_iod_dataset_write,                 /* write */
        H5VL_iod_dataset_set_extent,            /* set extent */
        H5VL_iod_dataset_get,                   /* get */
        H5VL_iod_dataset_close                  /* close */
    },
    {                                           /* file_cls */
        H5VL_iod_file_create,                   /* create */
        H5VL_iod_file_open,                     /* open */
        NULL,                                   /* flush */
        H5VL_iod_file_get,                      /* get */
        NULL,                                   /* misc */
        NULL,                                   /* optional */
        H5VL_iod_file_close                     /* close */
    },
    {                                           /* group_cls */
        H5VL_iod_group_create,                  /* create */
        H5VL_iod_group_open,                    /* open */
        H5VL_iod_group_get,                     /* get */
        H5VL_iod_group_close                    /* close */
    },
    {                                           /* link_cls */
        H5VL_iod_link_create,                   /* create */
        H5VL_iod_link_move,                     /* move */
        H5VL_iod_link_iterate,                  /* iterate */
        H5VL_iod_link_get,                      /* get */
        H5VL_iod_link_remove                    /* remove */
    },
    {                                           /* object_cls */
        H5VL_iod_object_open,                   /* open */
        H5VL_iod_object_copy,                   /* copy */
        H5VL_iod_object_visit,                  /* visit */
        H5VL_iod_object_get,                    /* get */
        H5VL_iod_object_misc,                   /* misc */
        NULL,                                   /* optional */
        NULL                                    /* close */
    },
    {
        H5VL_iod_cancel,
        H5VL_iod_test,
        H5VL_iod_wait
    }
};


/*--------------------------------------------------------------------------
NAME
   H5VL_iod_init_interface -- Initialize interface-specific information
USAGE
    herr_t H5VL_iod_init_interface()

RETURNS
    Non-negative on success/Negative on failure
DESCRIPTION
    Initializes any interface-specific data or routines.  (Just calls
    H5VL_iod_init currently).

--------------------------------------------------------------------------*/
static herr_t
H5VL_iod_init_interface(void)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5VL_iod_init_interface() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_init
 *
 * Purpose:	Initialize this vol plugin by registering the driver with the
 *		library.
 *
 * Return:	Success:	The ID for the iod plugin.
 *		Failure:	Negative.
 *
 * Programmer:	Mohamad Chaarawi
 *              March, 2013
 *
 *-------------------------------------------------------------------------
 */
H5VL_class_t *
H5VL_iod_init(void)
{
    H5VL_class_t *ret_value = NULL;            /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Set return value */
    ret_value = &H5VL_iod_g;

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_init() */


/*-------------------------------------------------------------------------
 * Function:	H5VL__iod_request_remove_from_axe_list
 *
 * Purpose:	Utility routine to remove a node from the global list of 
 *              AXE tasks.
 *
 * Return:	Success:	Positive
 *		Failure:	Negative.
 *
 * Programmer:	Mohamad Chaarawi
 *              August, 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL__iod_request_remove_from_axe_list(H5VL_iod_request_t *request)
{
    H5VL_iod_request_t *prev;
    H5VL_iod_request_t *next;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(request);

    prev = request->global_prev;
    next = request->global_next;

    if (prev) {
        if (next) {
            prev->global_next = next;
            next->global_prev = prev;
        }
        else {
            prev->global_next = NULL;
            axe_list.tail = prev;
        }
    }
    else {
        if (next) {
            next->global_prev = NULL;
            axe_list.head = next;
        }
        else {
            axe_list.head = NULL;
            axe_list.tail = NULL;
        }
    }

    request->global_prev = NULL;
    request->global_next = NULL;

    H5VL_iod_request_decr_rc(request);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5VL__iod_request_remove_from_axe_list() */


/*-------------------------------------------------------------------------
 * Function:	H5VL__iod_request_add_to_axe_list
 *
 * Purpose:	Utility routine to add a node to the global list of AXE
 *              tasks. This routine also checks which tasks have completed
 *              and can be freed by the VOL callback that is calling this 
 *              routine, and updates the last_released_task global variable
 *              accordingly.
 *
 * Return:	Success:	Positive
 *		Failure:	Negative.
 *
 * Programmer:	Mohamad Chaarawi
 *              August, 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL__iod_request_add_to_axe_list(H5VL_iod_request_t *request)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(request);

    if (axe_list.tail) {
        axe_list.tail->global_next = request;
        request->global_prev = axe_list.tail;
        axe_list.tail = request;
    }
    else {
        axe_list.head = request;
        axe_list.tail = request;
        request->global_prev = NULL;
    }

    request->global_next = NULL;
    request->ref_count ++;

    /* process axe_list */
    while(axe_list.head && /* If there is a head request */
          /* and the only reference is from this global axe list OR
             from the axe list and the file list */
          (axe_list.head->ref_count == 1 || (axe_list.head->ref_count == 2 && request->req != NULL)) &&
          /* and the request has completed */
          H5VL_IOD_COMPLETED == axe_list.head->state) {

        /* add the axe IDs to the ones to free. */
        axe_list.last_released_task = axe_list.head->axe_id;

        /* remove head from axe list */
        H5VL__iod_request_remove_from_axe_list(axe_list.head);
    }

    FUNC_LEAVE_NOAPI(SUCCEED)
}/* end H5VL__iod_request_add_to_axe_list() */


/*-------------------------------------------------------------------------
 * Function:	H5VL__iod_create_and_forward
 *
 * Purpose:	Utility routine to create a mercury request and a 
 *              VOL IOD request and ship the op to the server.
 *
 * Return:	Success:	Positive
 *		Failure:	Negative.
 *
 * Programmer:	Mohamad Chaarawi
 *              August, 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL__iod_create_and_forward(hg_id_t op_id, H5RQ_type_t op_type, 
                             H5VL_iod_object_t *request_obj, htri_t track,
                             size_t num_parents, H5VL_iod_request_t **parent_reqs,
                             H5VL_iod_req_info_t *req_info,
                             void *input, void *output, void *data, void **req)
{
    hg_request_t *hg_req = NULL;
    H5VL_iod_request_t *request = NULL;
    hbool_t do_async = (req == NULL) ? FALSE : TRUE;  /* Whether we're performing async. I/O */
    axe_t *axe_info = (axe_t *) input;
    AXE_task_t *parent_axe_ids = NULL;
    unsigned u;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* get a function shipper request */
    if(NULL == (hg_req = (hg_request_t *)H5MM_malloc(sizeof(hg_request_t))))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate a HG request");

    /* Get async request for operation */
    if(NULL == (request = (H5VL_iod_request_t *)H5MM_malloc(sizeof(H5VL_iod_request_t))))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate IOD VOL request struct");

    /* get axe ID for operation */
    axe_info->axe_id = g_axe_id ++;

    /* Set up request */
    HDmemset(request, 0, sizeof(*request));
    request->type = op_type;
    request->data = data;
    request->req = hg_req;
    request->ref_count = 1;
    request->obj = request_obj;
    request->axe_id = axe_info->axe_id;
    request->file_next = request->file_prev = NULL;
    request->global_next = request->global_prev = NULL;

    if(do_async)
        request->trans_info = req_info;

    /* add request to container's linked list */
    if(HG_ANALYSIS_EXECUTE != op_id)
        H5VL_iod_request_add(request_obj->file, request);

    /* update the parent information in the request */
    request->num_parents = num_parents;
    request->parent_reqs = parent_reqs;

    if(num_parents) {
        if(NULL == (parent_axe_ids = (AXE_task_t *)H5MM_malloc(sizeof(AXE_task_t) * num_parents)))
            HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate array of parent axe IDs");

        for(u=0 ; u<num_parents ; u++)
            parent_axe_ids[u] = parent_reqs[u]->axe_id;
    }

    axe_info->num_parents = num_parents;
    axe_info->parent_axe_ids = parent_axe_ids;

    axe_info->start_range = axe_list.last_released_task + 1;
    /* add request to global axe's linked list */
    H5VL__iod_request_add_to_axe_list(request);
    axe_info->count = axe_list.last_released_task - axe_info->start_range + 1;

#if H5VL_IOD_DEBUG
    printf("Operation %"PRIu64" Dependencies: ", request->axe_id);
    for(u=0 ; u<num_parents ; u++)
        printf("%"PRIu64" ", axe_info->parent_axe_ids[u]);
    printf("\n");

    if(axe_info->count) {
        printf("Operation %"PRIu64" will finish tasks %"PRIu64" through %"PRIu64"\n",
               request->axe_id, axe_info->start_range, 
               axe_info->start_range+axe_info->count-1);
    }
#endif

    /* forward the call to the ION */
    if(HG_Forward(PEER, op_id, input, output, hg_req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to ship operation");

    /* Store/wait on request */
    if(do_async) {
        *req = request;

        /* Track request */
        if(track)
            request_obj->request = request;
    } /* end if */
    else {
        if(track)
            request_obj->request = NULL;

        if(HG_ANALYSIS_EXECUTE == op_id) {
            int ret;
            hg_status_t status;

            /* test the operation status */
            ret = HG_Wait(*((hg_request_t *)request->req), HG_MAX_IDLE_TIME,
                    &status);
            if(HG_FAIL == ret) {
                fprintf(stderr, "failed to wait on request\n");
                request->status = H5ES_STATUS_FAIL;
                request->state = H5VL_IOD_COMPLETED;
            }
            else {
                if(status) {
                    request->status = H5ES_STATUS_SUCCEED;
                    request->state = H5VL_IOD_COMPLETED;
                }
            }
        } else {
        /* Synchronously wait on the request */
        if(H5VL_iod_request_wait(request_obj->file, request) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "can't wait on HG request");
        }

        request->req = H5MM_xfree(request->req);
        H5VL_iod_request_decr_rc(request);
    } /* end else */

done:
    parent_axe_ids = (AXE_task_t *)H5MM_xfree(parent_axe_ids);
    FUNC_LEAVE_NOAPI(ret_value)
}/* end H5VL__iod_create_and_forward() */


/*-------------------------------------------------------------------------
 * Function:	EFF_init
 *
 * Purpose:	initialize to the EFF stack
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              March, 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
EFF_init(MPI_Comm comm, MPI_Info UNUSED info)
{
    //char mpi_port_name[MPI_MAX_PORT_NAME];
    int tag = 123456;
    hg_request_t hg_req;
    int num_procs, my_rank;
    na_addr_t ion_target;
    double axe_seed;
    char addr_name[H5VL_IOD_MAX_ADDR_NAME];
    herr_t ret_value = SUCCEED;

    MPI_Comm_size(comm, &num_procs);
    MPI_Comm_rank(comm, &my_rank);

    /* generate global variables to create and track axe_ids for every
       operation. Each process owns a portion of the ID space and uses
       that space incrementally. */
    axe_seed = (pow(2.0,64.0) - 1) / num_procs;
    g_axe_id = (AXE_task_t)(axe_seed * my_rank + 1);

    axe_list.last_released_task = g_axe_id - 1;
    axe_list.head = NULL;
    axe_list.tail = NULL;

    /* MSC - This is a temporary solution for connecting to the server
       using mercury */
    /* Only rank 0 reads file */
    if (my_rank == 0) {
        int count, line=0, num_ions;
        FILE *config;
        char config_addr_name[H5VL_IOD_MAX_ADDR_NAME];

        config = fopen("port.cfg", "r");

        fscanf(config, "%d\n", &num_ions);
        fprintf(stderr, "Found %d servers\n", num_ions);

        /* read a line */
        if(fgets(config_addr_name, H5VL_IOD_MAX_ADDR_NAME, config) != NULL) {
            strncpy(addr_name, config_addr_name, H5VL_IOD_MAX_ADDR_NAME);
            count = 1;
            while(num_procs > line + (count*num_ions)) {
                MPI_Send(config_addr_name, H5VL_IOD_MAX_ADDR_NAME, MPI_BYTE, 
                         line + (count*num_ions), tag, comm);
                count ++;
            }
            line++;
        }

        while (fgets(config_addr_name, H5VL_IOD_MAX_ADDR_NAME, config) != NULL) {
            count = 0;
            while(num_procs > line + (count*num_ions)) {
                MPI_Send(config_addr_name, H5VL_IOD_MAX_ADDR_NAME, MPI_BYTE, 
                         line + (count*num_ions), tag, comm);
                count ++;
            }
            line ++;
        }
        fclose(config);
    }
    else {
        MPI_Recv(addr_name, H5VL_IOD_MAX_ADDR_NAME, MPI_BYTE, 
                 0, tag, comm, MPI_STATUS_IGNORE);
    }

    fprintf(stderr, "CN %d Connecting to ION %s\n", my_rank, addr_name);

    /* initialize Mercury stuff */
    network_class = NA_MPI_Init(NULL, 0);

    if (HG_SUCCESS != HG_Init(network_class)) {
        fprintf(stderr, "Failed to initialize Mercury\n");
        return FAIL;
    }
    if (NA_SUCCESS !=  NA_Addr_lookup(network_class, addr_name, &ion_target))  {
        fprintf(stderr, "Server lookup failed\n");
        return FAIL;
    }

    PEER = ion_target;

    /* Register function and encoding/decoding functions */
    H5VL_EFF_INIT_ID     = MERCURY_REGISTER("eff_init", eff_init_in_t, ret_t);
    H5VL_EFF_FINALIZE_ID = MERCURY_REGISTER("eff_finalize", ret_t, ret_t);

    H5VL_ANALYSIS_EXECUTE_ID = MERCURY_REGISTER("analysis_execute", 
                                                analysis_execute_in_t, 
                                                analysis_execute_out_t);

    H5VL_FILE_CREATE_ID = MERCURY_REGISTER("file_create", file_create_in_t, file_create_out_t);
    H5VL_FILE_OPEN_ID   = MERCURY_REGISTER("file_open", file_open_in_t, file_open_out_t);
    H5VL_FILE_CLOSE_ID  = MERCURY_REGISTER("file_close", file_close_in_t, ret_t);

    H5VL_ATTR_CREATE_ID = MERCURY_REGISTER("attr_create", attr_create_in_t, attr_create_out_t);
    H5VL_ATTR_OPEN_ID   = MERCURY_REGISTER("attr_open", attr_open_in_t, attr_open_out_t);
    H5VL_ATTR_READ_ID   = MERCURY_REGISTER("attr_read", attr_io_in_t, ret_t);
    H5VL_ATTR_WRITE_ID  = MERCURY_REGISTER("attr_write", attr_io_in_t, ret_t);
    H5VL_ATTR_EXISTS_ID = MERCURY_REGISTER("attr_exists", attr_op_in_t, htri_t);
    H5VL_ATTR_ITERATE_ID = MERCURY_REGISTER("attr_iterate", attr_op_in_t, ret_t);
    H5VL_ATTR_RENAME_ID = MERCURY_REGISTER("attr_rename", attr_rename_in_t, ret_t);
    H5VL_ATTR_REMOVE_ID = MERCURY_REGISTER("attr_remove", attr_op_in_t, ret_t);
    H5VL_ATTR_CLOSE_ID  = MERCURY_REGISTER("attr_close", attr_close_in_t, ret_t);

    H5VL_GROUP_CREATE_ID = MERCURY_REGISTER("group_create", group_create_in_t, group_create_out_t);
    H5VL_GROUP_OPEN_ID   = MERCURY_REGISTER("group_open", group_open_in_t, group_open_out_t);
    H5VL_GROUP_CLOSE_ID  = MERCURY_REGISTER("group_close", group_close_in_t, ret_t);

    H5VL_MAP_CREATE_ID = MERCURY_REGISTER("map_create", map_create_in_t, map_create_out_t);
    H5VL_MAP_OPEN_ID   = MERCURY_REGISTER("map_open", map_open_in_t, map_open_out_t);
    H5VL_MAP_SET_ID    = MERCURY_REGISTER("map_set", map_set_in_t, ret_t);
    H5VL_MAP_GET_ID    = MERCURY_REGISTER("map_get", map_get_in_t, map_get_out_t);
    H5VL_MAP_GET_COUNT_ID = MERCURY_REGISTER("map_get_count", map_get_count_in_t, int64_t);
    H5VL_MAP_ITERATE_ID   = MERCURY_REGISTER("map_iterate", map_op_in_t, ret_t);
    H5VL_MAP_EXISTS_ID = MERCURY_REGISTER("map_exists", map_op_in_t, hbool_t);
    H5VL_MAP_DELETE_ID = MERCURY_REGISTER("map_delete", map_op_in_t, ret_t);
    H5VL_MAP_CLOSE_ID  = MERCURY_REGISTER("map_close", map_close_in_t, ret_t);

    H5VL_DSET_CREATE_ID = MERCURY_REGISTER("dset_create", dset_create_in_t, dset_create_out_t);
    H5VL_DSET_OPEN_ID   = MERCURY_REGISTER("dset_open", dset_open_in_t, dset_open_out_t);
    H5VL_DSET_READ_ID   = MERCURY_REGISTER("dset_read", dset_io_in_t, dset_read_out_t);
    H5VL_DSET_GET_VL_SIZE_ID = MERCURY_REGISTER("dset_get_vl_size", 
                                                dset_get_vl_size_in_t, dset_read_out_t);
    H5VL_DSET_WRITE_ID  = MERCURY_REGISTER("dset_write", dset_io_in_t, ret_t);
    H5VL_DSET_SET_EXTENT_ID = MERCURY_REGISTER("dset_set_extent", 
                                               dset_set_extent_in_t, ret_t);
    H5VL_DSET_CLOSE_ID  = MERCURY_REGISTER("dset_close", dset_close_in_t, ret_t);

    H5VL_DTYPE_COMMIT_ID = MERCURY_REGISTER("dtype_commit", dtype_commit_in_t, dtype_commit_out_t);
    H5VL_DTYPE_OPEN_ID   = MERCURY_REGISTER("dtype_open", dtype_open_in_t, dtype_open_out_t);
    H5VL_DTYPE_CLOSE_ID  = MERCURY_REGISTER("dtype_close", dtype_close_in_t, ret_t);

    H5VL_LINK_CREATE_ID  = MERCURY_REGISTER("link_create", link_create_in_t, ret_t);
    H5VL_LINK_MOVE_ID    = MERCURY_REGISTER("link_move", link_move_in_t, ret_t);
    H5VL_LINK_EXISTS_ID  = MERCURY_REGISTER("link_exists", link_op_in_t, htri_t);
    H5VL_LINK_GET_INFO_ID = MERCURY_REGISTER("link_get_info", link_op_in_t, linfo_t);
    H5VL_LINK_GET_VAL_ID  = MERCURY_REGISTER("link_get_val", link_get_val_in_t, 
                                             link_get_val_out_t);
    H5VL_LINK_ITERATE_ID = MERCURY_REGISTER("link_iterate", link_op_in_t, ret_t);
    H5VL_LINK_REMOVE_ID  = MERCURY_REGISTER("link_remove", link_op_in_t, ret_t);

    H5VL_OBJECT_OPEN_BY_TOKEN_ID = MERCURY_REGISTER("object_open_by_token", 
                                                    object_token_in_t, iod_handles_t);
    H5VL_OBJECT_OPEN_ID   = MERCURY_REGISTER("object_open", object_op_in_t, object_open_out_t);
    H5VL_OBJECT_COPY_ID   = MERCURY_REGISTER("object_copy", object_copy_in_t, ret_t);
    H5VL_OBJECT_EXISTS_ID = MERCURY_REGISTER("object_exists", object_op_in_t, htri_t);
    H5VL_OBJECT_VISIT_ID  = MERCURY_REGISTER("object_visit", object_op_in_t, ret_t);
    H5VL_OBJECT_SET_COMMENT_ID = MERCURY_REGISTER("set_comment", object_set_comment_in_t, ret_t);
    H5VL_OBJECT_GET_COMMENT_ID = MERCURY_REGISTER("get_comment", object_get_comment_in_t, 
                                                  object_get_comment_out_t);
    H5VL_OBJECT_GET_INFO_ID = MERCURY_REGISTER("object_get_info", object_op_in_t, oinfo_t);

    H5VL_RC_ACQUIRE_ID      = MERCURY_REGISTER("read_context_acquire", 
                                               rc_acquire_in_t, rc_acquire_out_t);
    H5VL_RC_RELEASE_ID      = MERCURY_REGISTER("read_context_release", rc_release_in_t, ret_t);
    H5VL_RC_PERSIST_ID      = MERCURY_REGISTER("read_context_persist", rc_persist_in_t, ret_t);
    H5VL_RC_SNAPSHOT_ID     = MERCURY_REGISTER("read_context_snapshot", rc_snapshot_in_t, ret_t);

    H5VL_TR_START_ID        = MERCURY_REGISTER("transaction_start", tr_start_in_t, ret_t);
    H5VL_TR_FINISH_ID       = MERCURY_REGISTER("transaction_finish", tr_finish_in_t, ret_t);
    H5VL_TR_SET_DEPEND_ID   = MERCURY_REGISTER("transaction_set_depend",tr_set_depend_in_t, ret_t);
    H5VL_TR_SKIP_ID         = MERCURY_REGISTER("transaction_skip", tr_skip_in_t, ret_t);
    H5VL_TR_ABORT_ID        = MERCURY_REGISTER("transaction_abort",tr_abort_in_t, ret_t);

    H5VL_CANCEL_OP_ID = MERCURY_REGISTER("cancel_op", uint64_t, uint8_t);

    /* forward the init call to the ION and wait for its completion */
    if(HG_SUCCESS != HG_Forward(PEER, H5VL_EFF_INIT_ID, &num_procs, &ret_value, &hg_req)) {
        fprintf(stderr, "Failed to initialize Stack\n");
        return FAIL;
    }

    /* Wait for it to compete */
    HG_Wait(hg_req, HG_MAX_IDLE_TIME, HG_STATUS_IGNORE);

    /* Free Mercury request */
    if(HG_Request_free(hg_req) != HG_SUCCESS)
        return FAIL;

    return ret_value;
} /* end EFF_init() */


/*-------------------------------------------------------------------------
 * Function:	EFF_finalize
 *
 * Purpose:	shutdown the EFF stack
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              March, 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
EFF_finalize(void)
{
    hg_request_t hg_req;
    herr_t ret_value = SUCCEED;

    H5VL_iod_request_t *cur_req = axe_list.head;

    /* process axe_list */
    while(cur_req) {
        H5VL_iod_request_t *next_req = NULL;

        next_req = cur_req->global_next;

        HDassert(H5VL_IOD_COMPLETED == cur_req->state);
        HDassert(cur_req->ref_count == 1);

        /* add the axe IDs to the ones to free. */
        axe_list.last_released_task = cur_req->axe_id;

        /* remove head from axe list */
        H5VL__iod_request_remove_from_axe_list(cur_req);

        cur_req = next_req;
    }

    /* forward the finalize call to the ION and wait for it to complete */
    if(HG_Forward(PEER, H5VL_EFF_FINALIZE_ID, &ret_value, &ret_value, &hg_req) < 0)
        return FAIL;

    HG_Wait(hg_req, HG_MAX_IDLE_TIME, HG_STATUS_IGNORE);

    /* Free Mercury request */
    if(HG_Request_free(hg_req) != HG_SUCCESS)
        return FAIL;

    /* Free addr id */
    if (HG_SUCCESS != NA_Addr_free(network_class, PEER))
        return FAIL;

    /* Finalize interface */
    if (HG_SUCCESS != HG_Bulk_finalize())
        return FAIL;
    if (HG_SUCCESS != HG_Finalize())
        return FAIL;
    if(NA_SUCCESS != NA_Finalize(network_class))
        return FAIL;

    return ret_value;
} /* end EFF_finalize() */


/*-------------------------------------------------------------------------
 * Function:	H5Pset_fapl_iod
 *
 * Purpose:	Modify the file access property list to use the H5VL_IOD
 *		plugin defined in this source file.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              March, 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_fapl_iod(hid_t fapl_id, MPI_Comm comm, MPI_Info info)
{
    H5VL_iod_fapl_t fa;
    H5P_genplist_t  *plist;      /* Property list pointer */
    herr_t          ret_value;

    FUNC_ENTER_API(FAIL)
    H5TRACE3("e", "iMcMi", fapl_id, comm, info);

    if(fapl_id == H5P_DEFAULT)
        HGOTO_ERROR(H5E_PLIST, H5E_BADVALUE, FAIL, "can't set values in default property list")

    if(NULL == (plist = H5P_object_verify(fapl_id, H5P_FILE_ACCESS)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access property list")

    if(MPI_COMM_NULL == comm)
	HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a valid communicator")

    /* Initialize driver specific properties */
    fa.comm = comm;
    fa.info = info;

    ret_value = H5P_set_vol(plist, &H5VL_iod_g, &fa);

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_fapl_iod() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_fapl_copy
 *
 * Purpose:	Copies the iod-specific file access properties.
 *
 * Return:	Success:	Ptr to a new property list
 *		Failure:	NULL
 *
 * Programmer:	Mohamad Chaarawi
 *              July 2013
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL_iod_fapl_copy(const void *_old_fa)
{
    const H5VL_iod_fapl_t *old_fa = (const H5VL_iod_fapl_t*)_old_fa;
    H5VL_iod_fapl_t	  *new_fa = NULL;
    void		  *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (new_fa = (H5VL_iod_fapl_t *)H5MM_malloc(sizeof(H5VL_iod_fapl_t))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");

    /* Copy the general information */
    /* HDmemcpy(new_fa, old_fa, sizeof(H5VL_iod_fapl_t)); */

    /* Duplicate communicator and Info object. */
    if(FAIL == H5FD_mpi_comm_info_dup(old_fa->comm, old_fa->info, &new_fa->comm, &new_fa->info))
	HGOTO_ERROR(H5E_INTERNAL, H5E_CANTCOPY, NULL, "Communicator/Info duplicate failed");

    ret_value = new_fa;

done:
    if (NULL == ret_value){
	/* cleanup */
	if (new_fa)
	    H5MM_xfree(new_fa);
    }
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_fapl_copy() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_fapl_free
 *
 * Purpose:	Frees the iod-specific file access properties.
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:	Mohamad Chaarawi
 *              July 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_iod_fapl_free(void *_fa)
{
    herr_t		ret_value = SUCCEED;
    H5VL_iod_fapl_t	*fa = (H5VL_iod_fapl_t*)_fa;

    FUNC_ENTER_NOAPI_NOINIT

    assert(fa);

    /* Free the internal communicator and INFO object */
    assert(MPI_COMM_NULL!=fa->comm);
    if(H5FD_mpi_comm_info_free(&fa->comm, &fa->info) < 0)
	HGOTO_ERROR(H5E_INTERNAL, H5E_CANTFREE, FAIL, "Communicator/Info free failed");
    /* free the struct */
    H5MM_xfree(fa);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_fapl_free() */


/*-------------------------------------------------------------------------
 * Function:	H5Pset_dxpl_checksum
 *
 * Purpose:     Modify the dataset transfer property list to set a
 *              checksum value for the data to be transfered. 
 *              This is used with write operations.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              June, 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_dxpl_checksum(hid_t dxpl_id, uint64_t cs)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "iIl", dxpl_id, cs);

    if(dxpl_id == H5P_DEFAULT)
        HGOTO_ERROR(H5E_PLIST, H5E_BADVALUE, FAIL, "can't set values in default property list")

    /* Check arguments */
    if(NULL == (plist = H5P_object_verify(dxpl_id, H5P_DATASET_XFER)))
        HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a dxpl")

    /* Set the transfer mode */
    if(H5P_set(plist, H5D_XFER_CHECKSUM_NAME, &cs) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to set checksum value")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_dxpl_checksum() */


/*-------------------------------------------------------------------------
 * Function:	H5Pget_dxpl_checksum
 *
 * Purpose:     Retrieve the checksum value that was set using 
 *              H5Pset_dxpl_checksum.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              June, 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_dxpl_checksum(hid_t dxpl_id, uint64_t *cs/*out*/)
{
    H5P_genplist_t *plist;              /* Property list pointer */
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "ix", dxpl_id, cs);

    if(NULL == (plist = H5P_object_verify(dxpl_id, H5P_DATASET_XFER)))
        HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a dxpl")

    /* Get the transfer mode */
    if(cs)
        if(H5P_get(plist, H5D_XFER_CHECKSUM_NAME, cs) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to get checksum value")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pget_dxpl_checksum() */


/*-------------------------------------------------------------------------
 * Function:	H5Pset_dxpl_checksum_ptr
 *
 * Purpose:     Set a pointer to tell the library where to insert the
 *              checksum that is received from a remote location. 
 *              This is used with read operations.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              June, 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_dxpl_checksum_ptr(hid_t dxpl_id, uint64_t *cs)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "i*Il", dxpl_id, cs);

    if(dxpl_id == H5P_DEFAULT)
        HGOTO_ERROR(H5E_PLIST, H5E_BADVALUE, FAIL, "can't set values in default property list")

    /* Check arguments */
    if(NULL == (plist = H5P_object_verify(dxpl_id, H5P_DATASET_XFER)))
        HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a dxpl")

    /* Set the transfer mode */
    if(H5P_set(plist, H5D_XFER_CHECKSUM_PTR_NAME, &cs) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to set checksum_ptr value")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_dxpl_checksum_ptr() */


/*-------------------------------------------------------------------------
 * Function:	H5Pget_dxpl_checksum_ptr
 *
 * Purpose:     Retrieve the checksum pointer value that was set using 
 *              H5Pset_dxpl_checksum_ptr.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              June, 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_dxpl_checksum_ptr(hid_t dxpl_id, uint64_t **cs/*out*/)
{
    H5P_genplist_t *plist = NULL;              /* Property list pointer */
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "ix", dxpl_id, cs);

    if(NULL == (plist = H5P_object_verify(dxpl_id, H5P_DATASET_XFER)))
        HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a dxpl")

    /* Get the transfer mode */
    if(cs)
        if(H5P_get(plist, H5D_XFER_CHECKSUM_PTR_NAME, *cs) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to get checksum_ptr value")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pget_dxpl_checksum_ptr() */


/*-------------------------------------------------------------------------
 * Function:	H5Pset_metadata_integrity_scope
 *
 * Purpose: Set the scope of checksum generation and verification for
 * metadata in the FF stack. This is a file access property so the
 * property is set on a particular container. Changing the property
 * would require closing the file and reopening it. Possible values
 * for this property are:
 * H5_CHECKSUM_NONE      = No metadata checksuming and verification 
                           is done at any part of the stack.
 * H5_CHECKSUM_TRANSFER  = Metadata is verified after being transfered 
                           through Mercury.
 * H5_CHECKSUM_IOD       = Metadata is checksumed and the checksum is 
                           given to IOD when written, and verified when read.
 * H5_CHECKSUM_MEMORY    = Metadata is verified when moved in memory 
                           (Not currently supported).
 * H5_CHECKSUM_ALL       = Metadata is checksumed and verified on all levels.
 *
 * Note that the property value is a bitflag so any combination can be
 * set for individual values using OR operation.  
 *
 * Return: Non-negative
 * on success/Negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              September 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_metadata_integrity_scope(hid_t fapl_id, uint32_t scope)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value = SUCCEED; /* return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "iIu", fapl_id, scope);

    if(scope > H5_CHECKSUM_ALL)
        HGOTO_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL, "Invalid scope for Data Integrity");

    /* Get the plist structure */
    if(NULL == (plist = H5P_object_verify(fapl_id, H5P_FILE_ACCESS)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");

    /* Set property */
    if(H5P_set(plist, H5VL_CS_BITFLAG_NAME, &scope) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET,FAIL, "can't set data integrity scope");

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_metadata_integrity_scope() */


/*-------------------------------------------------------------------------
 * Function:	H5Pget_metadata_integrity_scope
 *
 * Purpose:	Get the current bit flag indicating the data integrity scope.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              September 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_metadata_integrity_scope(hid_t fapl_id, uint32_t *scope)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value = SUCCEED; /* return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "i*Iu", fapl_id, scope);

    /* Get the plist structure */
    if(NULL == (plist = H5P_object_verify(fapl_id, H5P_FILE_ACCESS)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");

    if(scope) {
        /* Get property */
        if(H5P_get(plist, H5VL_CS_BITFLAG_NAME, scope) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get scope for data integrity checks");
    }

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pget_metadata_integrity_scope() */


/*-------------------------------------------------------------------------
 * Function:	H5Pset_rawdata_integrity_scope
 *
 * Purpose: Set the scope of checksum generation and verification for
 * rawdata in the FF stack. This is a data transfer property so the
 * property is set on a particular I/O operation (H5Dread/write, 
 * H5Mset/get, etc ...). Possible values for this property are:
 * H5_CHECKSUM_NONE      = No checksuming and verification 
                           is done at any part of the stack.
 * H5_CHECKSUM_TRANSFER  = Data is verified after being transfered 
                           through Mercury.
 * H5_CHECKSUM_IOD       = Data is checksumed and the checksum is 
                           given to IOD when written, and verified when read.
 * H5_CHECKSUM_MEMORY    = Data is verified when moved in memory 
 * H5_CHECKSUM_ALL       = Data is checksumed and verified on all levels.
 *
 * Note that the property value is a bitflag so any combination can be
 * set for individual values using OR operation. 
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              September 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_rawdata_integrity_scope(hid_t dxpl_id, uint32_t scope)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value = SUCCEED; /* return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "iIu", dxpl_id, scope);

    if(scope > H5_CHECKSUM_ALL)
        HGOTO_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL, "Invalid scope for Data Integrity");

    /* Get the plist structure */
    if(NULL == (plist = H5P_object_verify(dxpl_id, H5P_DATASET_XFER)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");

    /* Set property */
    if(H5P_set(plist, H5VL_CS_BITFLAG_NAME, &scope) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET,FAIL, "can't set data integrity scope");

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_rawdata_integrity_scope() */


/*-------------------------------------------------------------------------
 * Function:	H5Pget_rawdata_integrity_scope
 *
 * Purpose:	Get the current bit flag indicating the data integrity scope.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              September 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_rawdata_integrity_scope(hid_t dxpl_id, uint32_t *scope)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value = SUCCEED; /* return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "i*Iu", dxpl_id, scope);

    /* Get the plist structure */
    if(NULL == (plist = H5P_object_verify(dxpl_id, H5P_DATASET_XFER)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");

    if(scope) {
        /* Get property */
        if(H5P_get(plist, H5VL_CS_BITFLAG_NAME, scope) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get scope for data integrity checks");
    }

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pget_rawdata_integrity_scope() */


/*-------------------------------------------------------------------------
 * Function:	H5Pset_dxpl_inject_corruption
 *
 * Purpose:     Temporary routine to set a boolean flag that tells the 
 *              library to inject corruption in the stack.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              June, 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_dxpl_inject_corruption(hid_t dxpl_id, hbool_t flag)
{
    H5P_genplist_t *plist = NULL;      /* Property list pointer */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "ib", dxpl_id, flag);

    if(dxpl_id == H5P_DEFAULT)
        HGOTO_ERROR(H5E_PLIST, H5E_BADVALUE, FAIL, "can't set values in default property list")

    /* Check arguments */
    if(NULL == (plist = H5P_object_verify(dxpl_id, H5P_DATASET_XFER)))
        HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a dxpl")

    /* Set the transfer mode */
    if(H5P_set(plist, H5D_XFER_INJECT_CORRUPTION_NAME, &flag) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to set value")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_dxpl_inject_corruption() */


/*-------------------------------------------------------------------------
 * Function:	H5Pget_dxpl_inject_corruption
 *
 * Purpose:     Temporary routine to retrieve the boolean flag that tells the 
 *              library to inject corruption in the stack.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              June, 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_dxpl_inject_corruption(hid_t dxpl_id, hbool_t *flag/*out*/)
{
    H5P_genplist_t *plist = NULL;              /* Property list pointer */
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "ix", dxpl_id, flag);

    if(NULL == (plist = H5P_object_verify(dxpl_id, H5P_DATASET_XFER)))
        HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a dxpl")

    /* Get the transfer mode */
    if(flag)
        if(H5P_get(plist, H5D_XFER_INJECT_CORRUPTION_NAME, flag) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to get value")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pget_dxpl_inject_corruption() */


/*-------------------------------------------------------------------------
 * Function:	H5Pset_dcpl_append_only
 *
 * Purpose:     Set a boolean flag on the dataset creation property list 
 *              to indicate to the VOL plugin that access to this dataset 
 *              will always be in an append/sequence only manner.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              June, 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_dcpl_append_only(hid_t dcpl_id, hbool_t flag)
{
    H5P_genplist_t *plist = NULL;      /* Property list pointer */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "ib", dcpl_id, flag);

    if(dcpl_id == H5P_DEFAULT)
        HGOTO_ERROR(H5E_PLIST, H5E_BADVALUE, FAIL, "can't set values in default property list")

    /* Check arguments */
    if(NULL == (plist = H5P_object_verify(dcpl_id, H5P_DATASET_CREATE)))
        HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a dcpl")

    /* Set the transfer mode */
    if(H5P_set(plist, H5D_CRT_APPEND_ONLY_NAME, &flag) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to set value")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_dcpl_append_only() */


/*-------------------------------------------------------------------------
 * Function:	H5Pget_dcpl_append_only
 *
 * Purpose:     Retrieve a boolean flag on the dataset creation property list 
 *              that indicates whether access to this dataset 
 *              will always be in an append/sequence only manner.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              June, 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_dcpl_append_only(hid_t dcpl_id, hbool_t *flag/*out*/)
{
    H5P_genplist_t *plist = NULL;              /* Property list pointer */
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "ix", dcpl_id, flag);

    if(NULL == (plist = H5P_object_verify(dcpl_id, H5P_DATASET_CREATE)))
        HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a dcpl")

    /* Get the transfer mode */
    if(flag)
        if(H5P_get(plist, H5D_CRT_APPEND_ONLY_NAME, flag) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to get value")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pget_dcpl_append_only() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_analysis_execute
 *
 * Purpose:	Creates a file as a iod HDF5 file.
 *
 * Return:	Success:	the file id. 
 *		Failure:	NULL
 *
 * Programmer:  Mohamad Chaarawi
 *              March, 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_iod_analysis_execute(const char *file_name, const char *obj_name,
        hid_t query_id, const char *split_script, const char *combine_script,
        void **req)
{
    analysis_execute_in_t input;
    analysis_execute_out_t *output;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* set the input structure for the HG encode routine */
    input.file_name = file_name;
    input.obj_name = obj_name;
    input.query_id = query_id;
    input.split_script = split_script;
    input.combine_script = combine_script;

#if H5VL_IOD_DEBUG
    printf("Analysis Execute on file %s Object %s\n", 
           input.file_name, input.obj_name);
#endif

    if(NULL == (output = (analysis_execute_out_t *)H5MM_malloc(sizeof(analysis_execute_out_t))))
	HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate analysis output struct");

    if(H5VL__iod_create_and_forward(H5VL_ANALYSIS_EXECUTE_ID, HG_ANALYSIS_EXECUTE, 
                                    NULL, 0, 0, NULL,
                                    NULL, &input, output, output, req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to create and ship file create");

done:

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_analysis_execute() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_file_create
 *
 * Purpose:	Creates a file as a iod HDF5 file.
 *
 * Return:	Success:	the file id. 
 *		Failure:	NULL
 *
 * Programmer:  Mohamad Chaarawi
 *              March, 2013
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL_iod_file_create(const char *name, unsigned flags, hid_t fcpl_id, hid_t fapl_id, 
                     hid_t UNUSED dxpl_id, void **req)
{
    H5VL_iod_fapl_t *fa = NULL;
    H5P_genplist_t *plist = NULL;      /* Property list pointer */
    H5VL_iod_file_t *file = NULL;
    file_create_in_t input;
    uint32_t cs_scope;
    void  *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    /*
     * Adjust bit flags by turning on the creation bit and making sure that
     * the EXCL or TRUNC bit is set.  All newly-created files are opened for
     * reading and writing.
     */
    if(0==(flags & (H5F_ACC_EXCL|H5F_ACC_TRUNC)))
	flags |= H5F_ACC_EXCL;	 /*default*/
    flags |= H5F_ACC_RDWR | H5F_ACC_CREAT;

    /* obtain the process rank from the communicator attached to the fapl ID */
    if(NULL == (plist = H5P_object_verify(fapl_id, H5P_FILE_ACCESS)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a file access property list")
    if(NULL == (fa = (H5VL_iod_fapl_t *)H5P_get_vol_info(plist)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTGET, NULL, "can't get IOD info struct")

    if(H5P_get(plist, H5VL_CS_BITFLAG_NAME, &cs_scope) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get scope for data integrity checks");

    /* allocate the file object that is returned to the user */
    if(NULL == (file = H5FL_CALLOC(H5VL_iod_file_t)))
	HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, NULL, "can't allocate IOD file struct");
    file->remote_file.root_oh.rd_oh.cookie = IOD_OH_UNDEFINED;
    file->remote_file.root_oh.wr_oh.cookie = IOD_OH_UNDEFINED;
    file->remote_file.root_id = IOD_OBJ_INVALID;
    file->remote_file.c_version = 0;
    MPI_Comm_rank(fa->comm, &file->my_rank);
    MPI_Comm_size(fa->comm, &file->num_procs);
    /* Duplicate communicator and Info object. */
    if(FAIL == H5FD_mpi_comm_info_dup(fa->comm, fa->info, &file->comm, &file->info))
	HGOTO_ERROR(H5E_INTERNAL, H5E_CANTCOPY, NULL, "Communicator/Info duplicate failed");

    /* Generate an IOD ID for the root group to be created */
    H5VL_iod_gen_obj_id(0, file->num_procs, (uint64_t)0, IOD_OBJ_KV, &input.root_id);
    file->remote_file.root_id = input.root_id;

    /* Generate an IOD ID for the root group MDKV to be created */
    H5VL_iod_gen_obj_id(0, file->num_procs, (uint64_t)1, IOD_OBJ_KV, &input.mdkv_id);
    file->remote_file.mdkv_id = input.mdkv_id;

    /* Generate an IOD ID for the root group ATTR KV to be created */
    H5VL_iod_gen_obj_id(0, file->num_procs, (uint64_t)2, IOD_OBJ_KV, &input.attrkv_id);
    file->remote_file.attrkv_id = input.attrkv_id;

    /* set the input structure for the HG encode routine */
    input.name = name;
    input.num_peers = (uint32_t)file->num_procs;
    input.flags = flags;
    input.fcpl_id = fcpl_id;
    input.fapl_id = fapl_id;

    /* create the file object that is passed to the API layer */
    file->file_name = HDstrdup(name);
    file->flags = flags;
    file->md_integrity_scope = cs_scope;
    if((file->remote_file.fcpl_id = H5Pcopy(fcpl_id)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, NULL, "failed to copy fcpl");
    if((file->fapl_id = H5Pcopy(fapl_id)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, NULL, "failed to copy fapl");
    file->nopen_objs = 1;
    file->num_req = 0;

    /* initialize head and tail of the container's linked list of requests */
    file->request_list_head = NULL;
    file->request_list_tail = NULL;

    file->common.obj_type = H5I_FILE;
    /* The name of the location is the root's object name "\" */
    file->common.obj_name = HDstrdup("/");
    file->common.obj_name[1] = '\0';
    file->common.file = file;

#if H5VL_IOD_DEBUG
    printf("File Create %s IOD ROOT ID %"PRIu64", axe id %"PRIu64"\n", 
           name, input.root_id, g_axe_id);
#endif

    if(H5VL__iod_create_and_forward(H5VL_FILE_CREATE_ID, HG_FILE_CREATE, 
                                    (H5VL_iod_object_t *)file, 1, 0, NULL,
                                    NULL, &input, &file->remote_file, file, req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to create and ship file create");

    ret_value = (void *)file;

done:

    if(ret_value == NULL) {
        if(file != NULL) {
            file = H5FL_FREE(H5VL_iod_file_t, file);
        } /* end if */
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_file_create() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_file_open
 *
 * Purpose:	Opens a file as a iod HDF5 file.
 *
 * Return:	Success:	file id. 
 *		Failure:	NULL
 *
 * Programmer:  Mohamad Chaarawi
 *              March, 2013
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL_iod_file_open(const char *name, unsigned flags, hid_t fapl_id, 
                   hid_t UNUSED dxpl_id, void **req)
{
    H5VL_iod_fapl_t *fa;
    H5P_genplist_t *plist = NULL;      /* Property list pointer */
    H5VL_iod_file_t *file = NULL;
    file_open_in_t input;
    hid_t rcxt_id;
    uint32_t cs_scope;
    void  *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    /* obtain the process rank from the communicator attached to the fapl ID */
    if(NULL == (plist = H5P_object_verify(fapl_id, H5P_FILE_ACCESS)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a file access property list")
    if(NULL == (fa = (H5VL_iod_fapl_t *)H5P_get_vol_info(plist)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTGET, NULL, "can't get IOD info struct")

    /* determine if we want to acquire the latest readable version
       when the file is opened */
    if(H5P_get(plist, H5VL_ACQUIRE_RC_ID, &rcxt_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't set property value for rxct id")

    if(H5P_get(plist, H5VL_CS_BITFLAG_NAME, &cs_scope) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get scope for data integrity checks");

    if(FAIL != rcxt_id) {
        input.acquire = TRUE;
    }
    else {
        input.acquire = FALSE;
    }

    /* allocate the file object that is returned to the user */
    if(NULL == (file = H5FL_CALLOC(H5VL_iod_file_t)))
	HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, NULL, "can't allocate IOD file struct");

    file->remote_file.coh.cookie = IOD_OH_UNDEFINED;
    file->remote_file.root_oh.rd_oh.cookie = IOD_OH_UNDEFINED;
    file->remote_file.root_oh.wr_oh.cookie = IOD_OH_UNDEFINED;
    file->remote_file.root_id = IOD_OBJ_INVALID;
    file->remote_file.mdkv_id = IOD_OBJ_INVALID;
    file->remote_file.attrkv_id = IOD_OBJ_INVALID;
    file->remote_file.fcpl_id = -1;
    file->remote_file.c_version = IOD_TID_UNKNOWN;

    /* set input paramters in struct to give to the function shipper */
    input.name = name;
    input.flags = flags;
    input.fapl_id = fapl_id;

    /* create the file object that is passed to the API layer */
    MPI_Comm_rank(fa->comm, &file->my_rank);
    MPI_Comm_size(fa->comm, &file->num_procs);
    /* Duplicate communicator and Info object. */
    if(FAIL == H5FD_mpi_comm_info_dup(fa->comm, fa->info, &file->comm, &file->info))
	HGOTO_ERROR(H5E_INTERNAL, H5E_CANTCOPY, NULL, "Communicator/Info duplicate failed");

    file->file_name = HDstrdup(name);
    file->flags = flags;
    file->md_integrity_scope = cs_scope;
    if((file->fapl_id = H5Pcopy(fapl_id)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, NULL, "failed to copy fapl");
    file->nopen_objs = 1;
    file->num_req = 0;

    /* initialize head and tail of the container's linked list */
    file->request_list_head = NULL;
    file->request_list_tail = NULL;

    file->common.obj_type = H5I_FILE; 
    /* The name of the location is the root's object name "\" */
    file->common.obj_name = HDstrdup("/");
    file->common.obj_name[1] = '\0';
    file->common.file = file;

#if H5VL_IOD_DEBUG
    printf("File Open %s axe id %"PRIu64"\n", name, g_axe_id);
#endif

    if(H5VL__iod_create_and_forward(H5VL_FILE_OPEN_ID, HG_FILE_OPEN, 
                                    (H5VL_iod_object_t *)file, 1, 0, NULL,
                                    NULL, &input, &file->remote_file, file, req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to create and ship file open");

    ret_value = (void *)file;

done:

    if(ret_value == NULL) {
        if(file != NULL) {
            file = H5FL_FREE(H5VL_iod_file_t, file);
        } /* end if */
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_file_open() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_file_get
 *
 * Purpose:	Gets certain data about a file
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:  Mohamad Chaarawi
 *              February, 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_iod_file_get(void *_obj, H5VL_file_get_t get_type, hid_t UNUSED dxpl_id, 
                  void UNUSED **req, va_list arguments)
{
    H5VL_iod_object_t *obj = (H5VL_iod_object_t *)_obj;
    H5VL_iod_file_t *file = obj->file;
    herr_t ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    switch (get_type) {
        /* H5Fget_access_plist */
        case H5VL_FILE_GET_FAPL:
            {
                H5VL_iod_fapl_t fa, *old_fa;
                H5P_genplist_t *new_plist, *old_plist;
                hid_t *plist_id = va_arg (arguments, hid_t *);

                /* Retrieve the file's access property list */
                if((*plist_id = H5Pcopy(file->fapl_id)) < 0)
                    HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get file access property list")

                if(NULL == (new_plist = (H5P_genplist_t *)H5I_object(*plist_id)))
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");
                if(NULL == (old_plist = (H5P_genplist_t *)H5I_object(file->fapl_id)))
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");

                if(NULL == (old_fa = (H5VL_iod_fapl_t *)H5P_get_vol_info(old_plist)))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't get vol info");
                fa.comm = old_fa->comm;
                fa.info = old_fa->info;

                ret_value = H5P_set_vol(new_plist, &H5VL_iod_g, &fa);

                break;
            }
        /* H5Fget_create_plist */
        case H5VL_FILE_GET_FCPL:
            {
                hid_t *plist_id = va_arg (arguments, hid_t *);

                /* Retrieve the file's access property list */
                if((*plist_id = H5Pcopy(file->remote_file.fcpl_id)) < 0)
                    HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get file creation property list")

                break;
            }
        /* H5Fget_intent */
        case H5VL_FILE_GET_INTENT:
            {
                unsigned *ret = va_arg (arguments, unsigned *);

                if(file->flags & H5F_ACC_RDWR)
                    *ret = H5F_ACC_RDWR;
                else
                    *ret = H5F_ACC_RDONLY;
                break;
            }
        /* H5Fget_name */
        case H5VL_FILE_GET_NAME:
            {
                H5I_type_t UNUSED type = va_arg (arguments, H5I_type_t);
                size_t     size = va_arg (arguments, size_t);
                char      *name = va_arg (arguments, char *);
                ssize_t   *ret  = va_arg (arguments, ssize_t *);
                size_t     len;

                len = HDstrlen(file->file_name);

                if(name) {
                    HDstrncpy(name, file->file_name, MIN(len + 1,size));
                    if(len >= size)
                        name[size-1]='\0';
                } /* end if */

                /* Set the return value for the API call */
                *ret = (ssize_t)len;
                break;
            }
        /* H5I_get_file_id */
        case H5VL_OBJECT_GET_FILE:
            {

                H5I_type_t UNUSED type = va_arg (arguments, H5I_type_t);
                void      **ret = va_arg (arguments, void **);

                *ret = (void*)file;
                break;
            }
        /* H5Fget_obj_count */
        case H5VL_FILE_GET_OBJ_COUNT:
            {
                //unsigned types = va_arg (arguments, unsigned);
                //ssize_t *ret = va_arg (arguments, ssize_t *);
                //break;
            }
        /* H5Fget_obj_ids */
        case H5VL_FILE_GET_OBJ_IDS:
            {
                //unsigned types = va_arg (arguments, unsigned);
                //size_t max_objs = va_arg (arguments, size_t);
                //hid_t *oid_list = va_arg (arguments, hid_t *);
                //ssize_t *ret = va_arg (arguments, ssize_t *);
                //break;
            }
        default:
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't get this type of information")
    } /* end switch */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_file_get() */

#if 0

/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_file_misc
 *
 * Purpose:	Perform an operation
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:  Mohamad Chaarawi
 *              April, 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_iod_file_misc(void *obj, H5VL_file_misc_t misc_type, hid_t dxpl_id, 
                   void **req, va_list arguments)
{
    herr_t       ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    switch (misc_type) {
        /* H5Fis_accessible */
        case H5VL_FILE_IS_ACCESSIBLE:
            {
                hid_t fapl_id       = va_arg (arguments, hid_t);
                const char *name    = va_arg (arguments, const char *);
                htri_t     *ret     = va_arg (arguments, htri_t *);
                H5VL_iod_file_t *file = NULL;
#if 0
                /* attempt to open the file through the MDS plugin */
                if(NULL == (file = (H5VL_iod_file_t *)H5VL_iod_file_open(name, H5F_ACC_RDONLY, fapl_id,
                                                                         H5_REQUEST_NULL)))
                    *ret = FALSE;
                else
                    *ret = TRUE;

                /* close the file if it was succesfully opened */
                if(file && H5VL_iod_file_close((void*)file, H5_REQUEST_NULL) < 0)
                    HGOTO_ERROR(H5E_FILE, H5E_CANTDEC, FAIL, "can't close file");
                break;
#endif
            }
        /* H5Fmount */
        case H5VL_FILE_MOUNT:
            {
                H5I_type_t type        = va_arg (arguments, H5I_type_t);
                const char *name       = va_arg (arguments, const char *);
                H5VL_iod_file_t *child = va_arg (arguments, H5VL_iod_file_t *);
                hid_t plist_id         = va_arg (arguments, hid_t);
            }
        /* H5Fmount */
        case H5VL_FILE_UNMOUNT:
            {
                H5I_type_t  type       = va_arg (arguments, H5I_type_t);
                const char *name       = va_arg (arguments, const char *);
            }
        default:
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "MDS Plugin does not support this operation type")
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_file_misc() */
#endif


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_file_close
 *
 * Purpose:	Closes a file.
 *
 * Return:	Success:	0
 *		Failure:	-1, file not closed.
 *
 * Programmer:  Mohamad Chaarawi
 *              March, 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_iod_file_close(void *_file, hid_t UNUSED dxpl_id, void **req)
{
    H5VL_iod_file_t *file = (H5VL_iod_file_t *)_file;
    file_close_in_t input;
    int *status = NULL;
    H5VL_iod_request_t **parent_reqs = NULL;
    size_t num_parents = 0;
    herr_t ret_value = SUCCEED;                 /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* If this call is not asynchronous, complete and remove all
       requests that are associated with this object from the List */
    if(NULL == req) {
        if(H5VL_iod_request_wait_all(file) < 0)
            HGOTO_ERROR(H5E_FILE,  H5E_CANTGET, FAIL, "can't wait on all object requests");
    }

    /* allocate an integer to receive the return value if the file close succeeded or not */
    status = (int *)malloc(sizeof(int));

    /* determine the max indexes for the KV, Array, and BLOB IDs used
       up by all the processes */
    {
        uint64_t input_indexes[3] = {file->remote_file.kv_oid_index, 
                                      file->remote_file.array_oid_index, 
                                      file->remote_file.blob_oid_index};
        uint64_t object_indexes[3];

        if(MPI_SUCCESS != MPI_Reduce(input_indexes, object_indexes, 3, 
                                     MPI_UINT64_T, MPI_MAX, 0, file->comm))
            HGOTO_ERROR(H5E_FILE,  H5E_CANTGET, FAIL, "can't determine max value of object indexes for ID generation");

        if(0 == file->my_rank) {
            input.max_kv_index = object_indexes[0];
            input.max_array_index = object_indexes[1];
            input.max_blob_index = object_indexes[2];

            printf("File Close MAXs: %"PRIu64" %"PRIu64" %"PRIu64"\n", 
                   input.max_kv_index, input.max_array_index, input.max_blob_index);
        }
        else {
            input.max_kv_index = 0;
            input.max_array_index = 0;
            input.max_blob_index = 0;
        }
    }

    input.coh = file->remote_file.coh;
    input.root_oh = file->remote_file.root_oh;
    input.root_id = file->remote_file.root_id;
    input.cs_scope = file->md_integrity_scope;

    if(file->num_req) {
        H5VL_iod_request_t *cur_req = file->request_list_head;

        if(NULL == (parent_reqs = (H5VL_iod_request_t **)H5MM_malloc
                    (sizeof(H5VL_iod_request_t *) * file->num_req)))
            HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate array of parent reqs");

        while(cur_req) {
            if(cur_req->state == H5VL_IOD_PENDING) {
                parent_reqs[num_parents] = cur_req;
                cur_req->ref_count ++;
                num_parents ++;
            }
            cur_req = cur_req->file_next;
        }
    }

#if H5VL_IOD_DEBUG
    printf("File Close Root ID %"PRIu64" axe id %"PRIu64"\n", input.root_id, g_axe_id);
#endif

    if(H5VL__iod_create_and_forward(H5VL_FILE_CLOSE_ID, HG_FILE_CLOSE, 
                                    (H5VL_iod_object_t *)file, 1,
                                    num_parents, parent_reqs,
                                    NULL, &input, status, status, req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to create and ship file close");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_file_close() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_group_create
 *
 * Purpose:	Creates a group inside a iod h5 file.
 *
 * Return:	Success:	group
 *		Failure:	NULL
 *
 * Programmer:  Mohamad Chaarawi
 *              March, 2013
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL_iod_group_create(void *_obj, H5VL_loc_params_t UNUSED loc_params, const char *name, hid_t gcpl_id, 
                      hid_t gapl_id, hid_t dxpl_id, void **req)
{
    H5VL_iod_object_t *obj = (H5VL_iod_object_t *)_obj; /* location object to create the group */
    H5VL_iod_group_t *grp = NULL; /* the group object that is created and passed to the user */
    group_create_in_t input;
    hid_t lcpl_id;
    iod_obj_id_t iod_id;
    iod_handles_t iod_oh;
    H5VL_iod_request_t **parent_reqs = NULL;
    size_t num_parents = 0;
    hid_t trans_id;
    H5TR_t *tr = NULL;
    H5P_genplist_t *plist = NULL;
    void *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    /* Get the group creation plist structure */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(gcpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, NULL, "can't find object for ID");
    /* get creation properties */
    if(H5P_get(plist, H5VL_GRP_LCPL_ID, &lcpl_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get property value for lcpl id");

    /* get the transaction ID */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, NULL, "can't find object for ID");
    if(H5P_get(plist, H5VL_TRANS_ID, &trans_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get property value for trans_id");

    /* get the TR object */
    if(NULL == (tr = (H5TR_t *)H5I_object_verify(trans_id, H5I_TR)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "not a Transaction ID")

    /* allocate parent request array */
    if(NULL == (parent_reqs = (H5VL_iod_request_t **)
                H5MM_malloc(sizeof(H5VL_iod_request_t *) * 2)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, NULL, "can't allocate parent req element");

    /* retrieve parent requests */
    if(H5VL_iod_get_parent_requests(obj, (H5VL_iod_req_info_t *)tr, parent_reqs, &num_parents) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "Failed to retrieve parent requests");

    /* retrieve IOD info of location object */
    if(H5VL_iod_get_loc_info(obj, &iod_id, &iod_oh, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "Failed to resolve current location group info");

    /* MSC - If location object not opened yet, wait for it. */
    if(IOD_OBJ_INVALID == iod_id) {
        /* Synchronously wait on the request attached to the dataset */
        if(H5VL_iod_request_wait(obj->file, obj->request) < 0)
            HGOTO_ERROR(H5E_DATASET,  H5E_CANTGET, NULL, "can't wait on HG request");
        obj->request = NULL;
        /* retrieve IOD info of location object */
        if(H5VL_iod_get_loc_info(obj, &iod_id, &iod_oh, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "Failed to resolve current location group info");
    }

    /* allocate the group object that is returned to the user */
    if(NULL == (grp = H5FL_CALLOC(H5VL_iod_group_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate object struct");

    grp->remote_group.iod_oh.rd_oh.cookie = IOD_OH_UNDEFINED;
    grp->remote_group.iod_oh.wr_oh.cookie = IOD_OH_UNDEFINED;

    /* Generate IOD IDs for the group to be created */
    H5VL_iod_gen_obj_id(obj->file->my_rank, obj->file->num_procs, 
                        obj->file->remote_file.kv_oid_index, 
                        IOD_OBJ_KV, &input.grp_id);
    grp->remote_group.iod_id = input.grp_id;
    /* increment the index of KV objects created on the container */
    obj->file->remote_file.kv_oid_index ++;

    H5VL_iod_gen_obj_id(obj->file->my_rank, obj->file->num_procs, 
                        obj->file->remote_file.kv_oid_index, 
                        IOD_OBJ_KV, &input.mdkv_id);
    grp->remote_group.mdkv_id = input.mdkv_id;
    /* increment the index of KV objects created on the container */
    obj->file->remote_file.kv_oid_index ++;

    H5VL_iod_gen_obj_id(obj->file->my_rank, obj->file->num_procs, 
                        obj->file->remote_file.kv_oid_index, 
                        IOD_OBJ_KV, &input.attrkv_id);
    grp->remote_group.attrkv_id = input.attrkv_id;
    /* increment the index of KV objects created on the container */
    obj->file->remote_file.kv_oid_index ++;

    /* set the input structure for the HG encode routine */
    input.coh = obj->file->remote_file.coh;
    input.loc_id = iod_id;
    input.loc_oh = iod_oh;
    input.name = name;
    input.gcpl_id = gcpl_id;
    input.gapl_id = gapl_id;
    input.lcpl_id = lcpl_id;
    input.trans_num = tr->trans_num;
    input.rcxt_num  = tr->c_version;
    input.cs_scope = obj->file->md_integrity_scope;

    /* setup the local group struct */
    /* store the entire path of the group locally */
    {
        size_t obj_name_len = HDstrlen(obj->obj_name);
        size_t name_len = HDstrlen(name);

        if (NULL == (grp->common.obj_name = (char *)HDmalloc(obj_name_len + name_len + 1)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate");
        HDmemcpy(grp->common.obj_name, obj->obj_name, obj_name_len);
        HDmemcpy(grp->common.obj_name+obj_name_len, name, name_len);
        grp->common.obj_name[obj_name_len+name_len] = '\0';
    }

    /* copy property lists */
    if((grp->remote_group.gcpl_id = H5Pcopy(gcpl_id)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, NULL, "failed to copy gcpl");
    if((grp->gapl_id = H5Pcopy(gapl_id)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, NULL, "failed to copy gapl");
    /* set common object parameters */
    grp->common.obj_type = H5I_GROUP;
    grp->common.file = obj->file;
    grp->common.file->nopen_objs ++;

#if H5VL_IOD_DEBUG
    printf("Group Create %s, IOD ID %"PRIu64", axe id %"PRIu64"\n", 
           name, input.grp_id, g_axe_id);
#endif

    if(H5VL__iod_create_and_forward(H5VL_GROUP_CREATE_ID, HG_GROUP_CREATE, 
                                    (H5VL_iod_object_t *)grp, 1, 
                                    num_parents, parent_reqs, (H5VL_iod_req_info_t *)tr,
                                    &input, &grp->remote_group, grp, req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to create and ship group create");

    ret_value = (void *)grp;

done:
    if(ret_value == NULL) {
        if(grp != NULL) {
            grp = H5FL_FREE(H5VL_iod_group_t, grp);
        } /* end if */
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_group_create() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_group_open
 *
 * Purpose:	Opens a group inside a iod h5 file.
 *
 * Return:	Success:	group id. 
 *		Failure:	NULL
 *
 * Programmer:  Mohamad Chaarawi
 *              March, 2013
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL_iod_group_open(void *_obj, H5VL_loc_params_t UNUSED loc_params, const char *name, 
                    hid_t gapl_id, hid_t dxpl_id, void **req)
{
    H5VL_iod_object_t *obj = (H5VL_iod_object_t *)_obj; /* location object to create the group */
    H5VL_iod_group_t  *grp = NULL; /* the group object that is created and passed to the user */
    group_open_in_t input;
    iod_obj_id_t iod_id;
    iod_handles_t iod_oh;
    H5P_genplist_t *plist = NULL;
    hid_t rcxt_id;
    H5RC_t *rc = NULL;
    size_t num_parents = 0;
    H5VL_iod_request_t **parent_reqs = NULL;
    void *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    /* get the context ID */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, NULL, "can't find object for ID");
    if(H5P_get(plist, H5VL_CONTEXT_ID, &rcxt_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get property value for trans_id");

    /* get the RC object */
    if(NULL == (rc = (H5RC_t *)H5I_object_verify(rcxt_id, H5I_RC)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "not a READ CONTEXT ID")

    /* allocate parent request array */
    if(NULL == (parent_reqs = (H5VL_iod_request_t **)
                H5MM_malloc(sizeof(H5VL_iod_request_t *))))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, NULL, "can't allocate parent req element");

    /* retrieve parent requests */
    if(H5VL_iod_get_parent_requests(obj, (H5VL_iod_req_info_t *)rc, parent_reqs, &num_parents) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "Failed to retrieve parent requests");

    /* retrieve IOD info of location object */
    if(H5VL_iod_get_loc_info(obj, &iod_id, &iod_oh, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "Failed to resolve current location group info");

    /* MSC - If location object not opened yet, wait for it. */
    if(IOD_OBJ_INVALID == iod_id) {
        /* Synchronously wait on the request attached to the dataset */
        if(H5VL_iod_request_wait(obj->file, obj->request) < 0)
            HGOTO_ERROR(H5E_DATASET,  H5E_CANTGET, NULL, "can't wait on HG request");
        obj->request = NULL;
        /* retrieve IOD info of location object */
        if(H5VL_iod_get_loc_info(obj, &iod_id, &iod_oh, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "Failed to resolve current location group info");
    }

    /* allocate the group object that is returned to the user */
    if(NULL == (grp = H5FL_CALLOC(H5VL_iod_group_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate object struct");

    grp->remote_group.iod_oh.rd_oh.cookie = IOD_OH_UNDEFINED;
    grp->remote_group.iod_oh.wr_oh.cookie = IOD_OH_UNDEFINED;
    grp->remote_group.iod_id = IOD_OBJ_INVALID;
    grp->remote_group.mdkv_id = IOD_OBJ_INVALID;
    grp->remote_group.attrkv_id = IOD_OBJ_INVALID;

    /* set the input structure for the HG encode routine */
    input.coh = obj->file->remote_file.coh;
    input.loc_id = iod_id;
    input.loc_oh = iod_oh;
    input.name = name;
    input.gapl_id = gapl_id;
    input.rcxt_num  = rc->c_version;
    input.cs_scope = obj->file->md_integrity_scope;

#if H5VL_IOD_DEBUG
    printf("Group Open %s LOC ID %"PRIu64", axe id %"PRIu64"\n", 
           name, input.loc_id, g_axe_id);
#endif

    /* setup the local group struct */
    /* store the entire path of the group locally */
    {
        size_t obj_name_len = HDstrlen(obj->obj_name);
        size_t name_len = HDstrlen(name);

        if (NULL == (grp->common.obj_name = (char *)HDmalloc(obj_name_len + name_len + 1)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate");
        HDmemcpy(grp->common.obj_name, obj->obj_name, obj_name_len);
        HDmemcpy(grp->common.obj_name+obj_name_len, name, name_len);
        grp->common.obj_name[obj_name_len+name_len] = '\0';
    }

    /* copy property lists */
    if((grp->gapl_id = H5Pcopy(gapl_id)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, NULL, "failed to copy gapl");
    /* set common object parameters */
    grp->common.obj_type = H5I_GROUP;
    grp->common.file = obj->file;
    grp->common.file->nopen_objs ++;

    if(H5VL__iod_create_and_forward(H5VL_GROUP_OPEN_ID, HG_GROUP_OPEN, 
                                    (H5VL_iod_object_t *)grp, 1, 
                                    num_parents, parent_reqs, (H5VL_iod_req_info_t *)rc,
                                    &input, &grp->remote_group, grp, req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to create and ship group open");

    ret_value = (void *)grp;

done:
    if(ret_value == NULL) {
        if(grp != NULL) {
            grp = H5FL_FREE(H5VL_iod_group_t, grp);
        } /* end if */
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_group_open() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_group_get
 *
 * Purpose:	Gets certain data about a group
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:  Mohamad Chaarawi
 *              February, 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_iod_group_get(void *_grp, H5VL_group_get_t get_type, hid_t UNUSED dxpl_id, 
                   void UNUSED **req, va_list arguments)
{
    H5VL_iod_group_t *grp = (H5VL_iod_group_t *)_grp;
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    switch (get_type) {
        /* H5Gget_create_plist */
        case H5VL_GROUP_GET_GCPL:
            {
                hid_t *plist_id = va_arg (arguments, hid_t *);

                /* Retrieve the file's access property list */
                if((*plist_id = H5Pcopy(grp->remote_group.gcpl_id)) < 0)
                    HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get group create property list")
                break;
            }
        /* H5Gget_info */
        case H5VL_GROUP_GET_INFO:
            {
                //H5VL_loc_params_t loc_params = va_arg (arguments, H5VL_loc_params_t);
                //H5G_info_t *ginfo = va_arg (arguments, H5G_info_t *);
            }
        default:
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't get this type of information from group")
    }
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_group_get() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_group_close
 *
 * Purpose:	Closes a group.
 *
 * Return:	Success:	0
 *		Failure:	-1, group not closed.
 *
 * Programmer:  Mohamad Chaarawi
 *              March, 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_iod_group_close(void *_grp, hid_t UNUSED dxpl_id, void **req)
{
    H5VL_iod_group_t *grp = (H5VL_iod_group_t *)_grp;
    group_close_in_t input;
    int *status = NULL;
    size_t num_parents = 0;
    H5VL_iod_request_t **parent_reqs = NULL;
    herr_t ret_value = SUCCEED;                 /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* If this call is not asynchronous, complete and remove all
       requests that are associated with this object from the List */
    if(NULL == req) {
        if(H5VL_iod_request_wait_some(grp->common.file, grp) < 0)
            HGOTO_ERROR(H5E_FILE,  H5E_CANTGET, FAIL, "can't wait on all object requests");
    }

    if(IOD_OH_UNDEFINED == grp->remote_group.iod_oh.rd_oh.cookie) {
        /* Synchronously wait on the request attached to the group */
        if(H5VL_iod_request_wait(grp->common.file, grp->common.request) < 0)
            HGOTO_ERROR(H5E_SYM,  H5E_CANTGET, FAIL, "can't wait on group request");
        grp->common.request = NULL;
    }

    if(H5VL_iod_get_obj_requests((H5VL_iod_object_t *)grp, &num_parents, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't get num requests");

    if(num_parents) {
        if(NULL == (parent_reqs = (H5VL_iod_request_t **)
                    H5MM_malloc(sizeof(H5VL_iod_request_t *) * num_parents)))
            HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate array of parent reqs");
        if(H5VL_iod_get_obj_requests((H5VL_iod_object_t *)grp, &num_parents, 
                                     parent_reqs) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't get parent requests");
    }

    input.iod_oh = grp->remote_group.iod_oh;
    input.iod_id = grp->remote_group.iod_id;

    /* allocate an integer to receive the return value if the group close succeeded or not */
    status = (int *)malloc(sizeof(int));

#if H5VL_IOD_DEBUG
    printf("Group Close IOD ID %"PRIu64", axe id %"PRIu64"\n", 
           input.iod_id, g_axe_id);
#endif

    if(H5VL__iod_create_and_forward(H5VL_GROUP_CLOSE_ID, HG_GROUP_CLOSE, 
                                    (H5VL_iod_object_t *)grp, 1,
                                    num_parents, parent_reqs,
                                    NULL, &input, status, status, req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to create and ship group close");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_group_close() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_dataset_create
 *
 * Purpose:	Sends a request to the IOD to create a dataset
 *
 * Return:	Success:	dataset object. 
 *		Failure:	NULL
 *
 * Programmer:  Mohamad Chaarawi
 *              October, 2013
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL_iod_dataset_create(void *_obj, H5VL_loc_params_t UNUSED loc_params, 
                        const char *name, hid_t dcpl_id, 
                        hid_t dapl_id, hid_t dxpl_id, void **req)
{
    H5VL_iod_object_t *obj = (H5VL_iod_object_t *)_obj; /* location object to create the dataset */
    H5VL_iod_dset_t *dset = NULL; /* the dataset object that is created and passed to the user */
    dset_create_in_t input;
    iod_obj_id_t iod_id;
    iod_handles_t iod_oh;
    H5VL_iod_request_t **parent_reqs = NULL;
    H5P_genplist_t *plist = NULL;
    size_t num_parents = 0;
    hid_t trans_id;
    H5TR_t *tr = NULL;
    hid_t type_id, space_id, lcpl_id;
    void *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    /* Get the dcpl plist structure */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(dcpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, NULL, "can't find object for ID");

    /* get datatype, dataspace, and lcpl IDs that were added in the dcpl at the API layer */
    if(H5P_get(plist, H5VL_DSET_TYPE_ID, &type_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get property value for datatype id");
    if(H5P_get(plist, H5VL_DSET_SPACE_ID, &space_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get property value for space id");
    if(H5P_get(plist, H5VL_DSET_LCPL_ID, &lcpl_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get property value for lcpl id");

    /* Check that no values other than the first dimension in MAX dims
       is H5S_UNLIMITED. */
    {
        H5S_t *ds = NULL;
        int ndims, i;
        hsize_t max_dims[H5S_MAX_RANK];

        if(NULL == (ds = (H5S_t *)H5I_object_verify(space_id, H5I_DATASPACE)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a dataspace")

        ndims = (int)H5S_GET_EXTENT_NDIMS(ds);
        H5S_get_simple_extent_dims(ds, NULL, max_dims);

        for(i=1; i<ndims; i++) {
            if(max_dims[i] == H5S_UNLIMITED)
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "only first dimension can be H5S_UNLIMITED.");
        }
    }

    /* get the transaction ID */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, NULL, "can't find object for ID");
    if(H5P_get(plist, H5VL_TRANS_ID, &trans_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get property value for trans_id");

    /* get the TR object */
    if(NULL == (tr = (H5TR_t *)H5I_object_verify(trans_id, H5I_TR)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "not a Transaction ID")

    /* Retrieve the parent AXE id by traversing the path where the
       dataset should be created. */
    if(NULL == (parent_reqs = (H5VL_iod_request_t **)
                H5MM_malloc(sizeof(H5VL_iod_request_t *) * 2)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, NULL, "can't allocate parent req element");

    /* retrieve parent requests */
    if(H5VL_iod_get_parent_requests(obj, (H5VL_iod_req_info_t *)tr, parent_reqs, &num_parents) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "Failed to retrieve parent requests");

    /* retrieve IOD info of location object */
    if(H5VL_iod_get_loc_info(obj, &iod_id, &iod_oh, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "Failed to resolve current location group info");

    /* MSC - If location object not opened yet, wait for it. */
    if(IOD_OBJ_INVALID == iod_id) {
        /* Synchronously wait on the request attached to the dataset */
        if(H5VL_iod_request_wait(obj->file, obj->request) < 0)
            HGOTO_ERROR(H5E_DATASET,  H5E_CANTGET, NULL, "can't wait on HG request");
        obj->request = NULL;
        /* retrieve IOD info of location object */
        if(H5VL_iod_get_loc_info(obj, &iod_id, &iod_oh, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "Failed to resolve current location group info");
    }

    /* allocate the dataset object that is returned to the user */
    if(NULL == (dset = H5FL_CALLOC(H5VL_iod_dset_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate object struct");

    dset->remote_dset.iod_oh.rd_oh.cookie = IOD_OH_UNDEFINED;
    dset->remote_dset.iod_oh.wr_oh.cookie = IOD_OH_UNDEFINED;
    dset->remote_dset.iod_id = IOD_OBJ_INVALID;

    /* Generate IOD IDs for the dset to be created */
    H5VL_iod_gen_obj_id(obj->file->my_rank, obj->file->num_procs, 
                        obj->file->remote_file.array_oid_index, 
                        IOD_OBJ_ARRAY, &input.dset_id);
    dset->remote_dset.iod_id = input.dset_id;
    /* increment the index of ARRAY objects created on the container */
    obj->file->remote_file.array_oid_index ++;

    H5VL_iod_gen_obj_id(obj->file->my_rank, obj->file->num_procs, 
                        obj->file->remote_file.kv_oid_index, 
                        IOD_OBJ_KV, &input.mdkv_id);
    dset->remote_dset.mdkv_id = input.mdkv_id;
    /* increment the index of KV objects created on the container */
    obj->file->remote_file.kv_oid_index ++;

    H5VL_iod_gen_obj_id(obj->file->my_rank, obj->file->num_procs, 
                        obj->file->remote_file.kv_oid_index, 
                        IOD_OBJ_KV, &input.attrkv_id);
    dset->remote_dset.attrkv_id = input.attrkv_id;
    /* increment the index of KV objects created on the container */
    obj->file->remote_file.kv_oid_index ++;

    /* set the input structure for the HG encode routine */
    input.coh = obj->file->remote_file.coh;
    input.loc_id = iod_id;
    input.loc_oh = iod_oh;
    input.name = name;
    input.dcpl_id = dcpl_id;
    input.dapl_id = dapl_id;
    input.lcpl_id = lcpl_id;
    input.type_id = type_id;
    input.space_id = space_id;
    input.trans_num = tr->trans_num;
    input.rcxt_num  = tr->c_version;
    input.cs_scope = obj->file->md_integrity_scope;

    /* setup the local dataset struct */
    /* store the entire path of the dataset locally */
    {
        size_t obj_name_len = HDstrlen(obj->obj_name);
        size_t name_len = HDstrlen(name);

        if (NULL == (dset->common.obj_name = (char *)HDmalloc(obj_name_len + name_len + 1)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate");
        HDmemcpy(dset->common.obj_name, obj->obj_name, obj_name_len);
        HDmemcpy(dset->common.obj_name+obj_name_len, name, name_len);
        dset->common.obj_name[obj_name_len+name_len] = '\0';
    }

    /* copy property lists, dtype, and dspace*/
    if((dset->remote_dset.dcpl_id = H5Pcopy(dcpl_id)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, NULL, "failed to copy dcpl");
    if((dset->dapl_id = H5Pcopy(dapl_id)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, NULL, "failed to copy dapl");
    if((dset->remote_dset.type_id = H5Tcopy(type_id)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, NULL, "failed to copy dtype");
    if((dset->remote_dset.space_id = H5Scopy(space_id)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, NULL, "failed to copy dspace");

    /* set common object parameters */
    dset->common.obj_type = H5I_DATASET;
    dset->common.file = obj->file;
    dset->common.file->nopen_objs ++;

#if H5VL_IOD_DEBUG
    printf("Dataset Create %s IOD ID %"PRIu64", axe id %"PRIu64"\n", 
           name, input.dset_id, g_axe_id);
#endif

    if(H5VL__iod_create_and_forward(H5VL_DSET_CREATE_ID, HG_DSET_CREATE, 
                                    (H5VL_iod_object_t *)dset, 1, 
                                    num_parents, parent_reqs,
                                    (H5VL_iod_req_info_t *)tr, &input, &dset->remote_dset, dset, req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to create and ship dataset create");

    ret_value = (void *)dset;

done:
    if(ret_value == NULL) {
        if(dset != NULL) {
            dset = H5FL_FREE(H5VL_iod_dset_t, dset);
        } /* end if */
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_dataset_create() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_dataset_open
 *
 * Purpose:	Sends a request to the IOD to open a dataset
 *
 * Return:	Success:	dataset object. 
 *		Failure:	NULL
 *
 * Programmer:  Mohamad Chaarawi
 *              October, 2013
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL_iod_dataset_open(void *_obj, H5VL_loc_params_t UNUSED loc_params, const char *name, 
                      hid_t dapl_id, hid_t dxpl_id, void **req)
{
    H5VL_iod_object_t *obj = (H5VL_iod_object_t *)_obj; /* location object to create the dataset */
    H5VL_iod_dset_t *dset = NULL; /* the dataset object that is created and passed to the user */
    dset_open_in_t input;
    iod_obj_id_t iod_id;
    iod_handles_t iod_oh;
    H5P_genplist_t *plist = NULL;
    hid_t rcxt_id;
    H5RC_t *rc = NULL;
    size_t num_parents = 0;
    H5VL_iod_request_t **parent_reqs = NULL;
    void *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    /* get the transaction ID */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, NULL, "can't find object for ID");
    if(H5P_get(plist, H5VL_CONTEXT_ID, &rcxt_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get property value for rcxt_id");

    /* get the RC object */
    if(NULL == (rc = (H5RC_t *)H5I_object_verify(rcxt_id, H5I_RC)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "not a READ CONTEXT ID")

    /* Retrieve the parent AXE id by traversing the path where the
       dataset should be opened. */
    if(NULL == (parent_reqs = (H5VL_iod_request_t **)
                H5MM_malloc(sizeof(H5VL_iod_request_t *))))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, NULL, "can't allocate parent req element");

    /* retrieve parent requests */
    if(H5VL_iod_get_parent_requests(obj, (H5VL_iod_req_info_t *)rc, parent_reqs, &num_parents) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "Failed to retrieve parent requests");

    /* retrieve IOD info of location object */
    if(H5VL_iod_get_loc_info(obj, &iod_id, &iod_oh, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "Failed to resolve current location group info");

    /* MSC - If location object not opened yet, wait for it. */
    if(IOD_OBJ_INVALID == iod_id) {
        /* Synchronously wait on the request attached to the dataset */
        if(H5VL_iod_request_wait(obj->file, obj->request) < 0)
            HGOTO_ERROR(H5E_DATASET,  H5E_CANTGET, NULL, "can't wait on HG request");
        obj->request = NULL;
        /* retrieve IOD info of location object */
        if(H5VL_iod_get_loc_info(obj, &iod_id, &iod_oh, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "Failed to resolve current location group info");
    }

    /* allocate the dataset object that is returned to the user */
    if(NULL == (dset = H5FL_CALLOC(H5VL_iod_dset_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate object struct");

    dset->remote_dset.iod_oh.rd_oh.cookie = IOD_OH_UNDEFINED;
    dset->remote_dset.iod_oh.wr_oh.cookie = IOD_OH_UNDEFINED;
    dset->remote_dset.iod_id = IOD_OBJ_INVALID;
    dset->remote_dset.mdkv_id = IOD_OBJ_INVALID;
    dset->remote_dset.attrkv_id = IOD_OBJ_INVALID;
    dset->remote_dset.dcpl_id = -1;
    dset->remote_dset.type_id = -1;
    dset->remote_dset.space_id = -1;

    /* set the input structure for the HG encode routine */
    input.coh = obj->file->remote_file.coh;
    input.loc_id = iod_id;
    input.loc_oh = iod_oh;
    input.name = name;
    input.dapl_id = dapl_id;
    input.rcxt_num  = rc->c_version;
    input.cs_scope = obj->file->md_integrity_scope;

    /* setup the local dataset struct */
    /* store the entire path of the dataset locally */
    {
        size_t obj_name_len = HDstrlen(obj->obj_name);
        size_t name_len = HDstrlen(name);

        if (NULL == (dset->common.obj_name = (char *)HDmalloc(obj_name_len + name_len + 1)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate");
        HDmemcpy(dset->common.obj_name, obj->obj_name, obj_name_len);
        HDmemcpy(dset->common.obj_name+obj_name_len, name, name_len);
        dset->common.obj_name[obj_name_len+name_len] = '\0';
    }

    if((dset->dapl_id = H5Pcopy(dapl_id)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, NULL, "failed to copy dapl");

    /* set common object parameters */
    dset->common.obj_type = H5I_DATASET;
    dset->common.file = obj->file;
    dset->common.file->nopen_objs ++;

#if H5VL_IOD_DEBUG
    printf("Dataset Open %s LOC ID %"PRIu64", axe id %"PRIu64"\n", 
           name, input.loc_id, g_axe_id);
#endif

    if(H5VL__iod_create_and_forward(H5VL_DSET_OPEN_ID, HG_DSET_OPEN, 
                                    (H5VL_iod_object_t *)dset, 1, 
                                    num_parents, parent_reqs,
                                    (H5VL_iod_req_info_t *)rc, &input, &dset->remote_dset, dset, req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to create and ship dataset open");

    ret_value = (void *)dset;

done:
    if(ret_value == NULL) {
        if(dset != NULL) {
            dset = H5FL_FREE(H5VL_iod_dset_t, dset);
        } /* end if */
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_dataset_open() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_dataset_read
 *
 * Purpose:	Reads raw data from a dataset into a buffer.
 *
 * Return:	Success:	0
 *		Failure:	-1, data not read.
 *
 * Programmer:  Mohamad Chaarawi
 *              October, 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_iod_dataset_read(void *_dset, hid_t mem_type_id, hid_t mem_space_id,
                      hid_t file_space_id, hid_t dxpl_id, void *buf, void **req)
{
    H5VL_iod_dset_t *dset = (H5VL_iod_dset_t *)_dset;
    dset_io_in_t input;
    dset_get_vl_size_in_t input_vl;
    H5P_genplist_t *plist = NULL;
    hg_bulk_t *bulk_handle = NULL;
    H5VL_iod_read_status_t *status = NULL;
    const H5S_t *mem_space = NULL;
    const H5S_t *file_space = NULL;
    char fake_char;
    size_t type_size; /* size of mem type */
    hssize_t nelmts;    /* num elements in mem dataspace */
    H5VL_iod_io_info_t *info = NULL;
    hbool_t is_vl_data = FALSE;
    hid_t rcxt_id;
    H5RC_t *rc = NULL;
    size_t num_parents = 0;
    H5VL_iod_request_t **parent_reqs = NULL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* get the context ID */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");
    if(H5P_get(plist, H5VL_CONTEXT_ID, &rcxt_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get property value for trans_id");

    /* get the RC object */
    if(NULL == (rc = (H5RC_t *)H5I_object_verify(rcxt_id, H5I_RC)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a READ CONTEXT ID")

    /* If there is information needed about the dataset that is not present locally, wait */
    if(-1 == dset->remote_dset.dcpl_id ||
       -1 == dset->remote_dset.type_id ||
       -1 == dset->remote_dset.space_id) {
        /* Synchronously wait on the request attached to the dataset */
        if(H5VL_iod_request_wait(dset->common.file, dset->common.request) < 0)
            HGOTO_ERROR(H5E_DATASET,  H5E_CANTGET, FAIL, "can't wait on HG request");
        dset->common.request = NULL;
    }

    /* At the IOD server, the array object is actually created with
       MAX dims, not current dims. So here we need to range check the
       filespace against the current dimensions of the dataset to make
       sure that the I/O happens in the current range and not the
       extensible one. */
    {
        H5S_t *dset_space, *io_space;
        hsize_t dset_dims[H5S_MAX_RANK], io_dims[H5S_MAX_RANK];
        int dset_ndims, io_ndims, i;

        if(NULL == (dset_space = (H5S_t *)H5I_object_verify(dset->remote_dset.space_id, H5I_DATASPACE)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataspace")
        if(NULL == (io_space = (H5S_t *)H5I_object_verify(file_space_id, H5I_DATASPACE)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataspace")

        dset_ndims = (int)H5S_GET_EXTENT_NDIMS(dset_space);
        io_ndims = (int)H5S_GET_EXTENT_NDIMS(io_space);

        if(dset_ndims < io_ndims)
	    HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "selection not within dataset's dataspace");

        H5S_get_simple_extent_dims(dset_space, dset_dims, NULL);
        H5S_get_simple_extent_dims(io_space, io_dims, NULL);

        for(i=0 ; i<io_ndims ; i++) {
            if(dset_dims[i] < io_dims[i])
                HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "selection Out of range");
        }
    }

    /* check arguments */
    if(H5S_ALL != mem_space_id) {
	if(NULL == (mem_space = (const H5S_t *)H5I_object_verify(mem_space_id, H5I_DATASPACE)))
	    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space");

	/* Check for valid selection */
	if(H5S_SELECT_VALID(mem_space) != TRUE)
	    HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "selection+offset not within extent");
    } /* end if */
    if(H5S_ALL != file_space_id) {
	if(NULL == (file_space = (const H5S_t *)H5I_object_verify(file_space_id, H5I_DATASPACE)))
	    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space");

	/* Check for valid selection */
	if(H5S_SELECT_VALID(file_space) != TRUE)
	    HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "selection+offset not within extent");
    } /* end if */

    if(!buf && (NULL == file_space || H5S_GET_SELECT_NPOINTS(file_space) != 0))
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no output buffer");

    if(!buf)
        buf = &fake_char;

    /* get the memory type size */
    {
        H5T_t *dt = NULL;

        if(NULL == (dt = (H5T_t *)H5I_object_verify(mem_type_id, H5I_DATATYPE)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, 0, "not a datatype");

        type_size = H5T_GET_SIZE(dt);
    }

    /* get the number of elements selcted in dataspace */
    nelmts = H5S_GET_SELECT_NPOINTS(mem_space);

    /* allocate a bulk data transfer handle */
    if(NULL == (bulk_handle = (hg_bulk_t *)H5MM_malloc(sizeof(hg_bulk_t))))
        HGOTO_ERROR(H5E_DATASET, H5E_NOSPACE, FAIL, "can't allocate a buld data transfer handle");

    /* compute checksum and create bulk handle */
    if(H5VL_iod_pre_read(mem_type_id, mem_space_id, buf, 
                         bulk_handle, &is_vl_data) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "can't generate read parameters");

    if(NULL == (parent_reqs = (H5VL_iod_request_t **)
                H5MM_malloc(sizeof(H5VL_iod_request_t *))))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate parent req element");

    /* retrieve parent requests */
    if(H5VL_iod_get_parent_requests((H5VL_iod_object_t *)dset, (H5VL_iod_req_info_t *)rc, 
                                    parent_reqs, &num_parents) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Failed to retrieve parent requests");

    if(!is_vl_data) {
        /* Fill input structure for reading data */
        input.coh = dset->common.file->remote_file.coh;
        input.iod_oh = dset->remote_dset.iod_oh;
        input.iod_id = dset->remote_dset.iod_id;
        input.mdkv_id = dset->remote_dset.mdkv_id;
        input.bulk_handle = *bulk_handle;
        input.checksum = 0;
        input.dxpl_id = dxpl_id;
        input.space_id = file_space_id;
        input.dset_type_id = dset->remote_dset.type_id;
        input.mem_type_id = mem_type_id;
        input.rcxt_num  = rc->c_version;
        input.cs_scope = dset->common.file->md_integrity_scope;
        input.trans_num = 0;
    }
    else {
        /* Fill input structure for retrieving the buffer size needed to read */
        input_vl.coh = dset->common.file->remote_file.coh;
        input_vl.iod_oh = dset->remote_dset.iod_oh;
        input_vl.iod_id = dset->remote_dset.iod_id;
        input_vl.mdkv_id = dset->remote_dset.mdkv_id;
        input_vl.dxpl_id = dxpl_id;
        input_vl.space_id = file_space_id;
        input_vl.mem_type_id = mem_type_id;
        input_vl.rcxt_num  = rc->c_version;
        input_vl.cs_scope = dset->common.file->md_integrity_scope;
    }

    /* allocate structure to receive status of read operation
       (contains return value, checksum, and buffer size) */
    status = (H5VL_iod_read_status_t *)malloc(sizeof(H5VL_iod_read_status_t));

    /* setup info struct for I/O request. 
       This is to manage the I/O operation once the wait is called. */
    if(NULL == (info = (H5VL_iod_io_info_t *)H5MM_calloc(sizeof(H5VL_iod_io_info_t))))
        HGOTO_ERROR(H5E_DATASET, H5E_NOSPACE, FAIL, "can't allocate a request");

    info->status = status;
    info->bulk_handle = bulk_handle;
    info->buf_ptr = buf;
    info->nelmts = nelmts;
    info->type_size = type_size;
    info->cs_ptr = NULL;

    /* store a copy of the dataspace selection to be able to calculate the checksum later */
    if(NULL == (info->space = H5S_copy(mem_space, FALSE, TRUE)))
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTINIT, FAIL, "unable to copy dataspace");
    /* store the pointer to the buffer where the checksum needs to be placed */
    if(H5P_get(plist, H5D_XFER_CHECKSUM_PTR_NAME, &info->cs_ptr) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to get checksum pointer value");
    /* store the raw data integrity scope */
    if(H5P_get(plist, H5VL_CS_BITFLAG_NAME, &info->raw_cs_scope) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to get checksum pointer value");

    /* If the read is of VL data, then we need the read parameters to
       perform the actual read when the wait is called (i.e. when we
       retrieve the buffer size) */
    if(is_vl_data) {
        if((info->file_space_id = H5Scopy(file_space_id)) < 0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTINIT, FAIL, "unable to copy dataspace");
        if((info->mem_type_id = H5Tcopy(mem_type_id)) < 0)
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "unable to copy datatype");
        if((info->dxpl_id = H5P_copy_plist((H5P_genplist_t *)plist, TRUE)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTINIT, FAIL, "unable to copy dxpl");

        info->peer = PEER;
        info->read_id = H5VL_DSET_READ_ID;
    }

#if H5VL_IOD_DEBUG
    if(!is_vl_data)
        printf("Dataset Read, axe id %"PRIu64"\n", g_axe_id);
    else
        printf("Dataset GET size, axe id %"PRIu64"\n", g_axe_id);
#endif

    /* forward the call to the IONs */
    if(!is_vl_data) {
        if(H5VL__iod_create_and_forward(H5VL_DSET_READ_ID, HG_DSET_READ, 
                                        (H5VL_iod_object_t *)dset, 0,
                                        num_parents, parent_reqs, (H5VL_iod_req_info_t *)rc, 
                                        &input, status, info, req) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to create and ship dataset read");
    }
    else {
        if(H5VL__iod_create_and_forward(H5VL_DSET_GET_VL_SIZE_ID, HG_DSET_GET_VL_SIZE, 
                                        (H5VL_iod_object_t *)dset, 0,
                                        num_parents, parent_reqs, (H5VL_iod_req_info_t *)rc,
                                        &input_vl, status, info, req) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to create and ship dataset get VL size");
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_dataset_read() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_dataset_write
 *
 * Purpose:	Writes raw data from a buffer into a dataset.
 *
 * Return:	Success:	0
 *		Failure:	-1, dataset not writed.
 *
 * Programmer:  Mohamad Chaarawi
 *              October, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_iod_dataset_write(void *_dset, hid_t mem_type_id, hid_t mem_space_id,
                       hid_t file_space_id, hid_t dxpl_id, const void *buf, void **req)
{
    H5VL_iod_dset_t *dset = (H5VL_iod_dset_t *)_dset;
    dset_io_in_t input;
    H5P_genplist_t *plist = NULL;
    hg_bulk_t *bulk_handle = NULL;
    const H5S_t *mem_space = NULL;
    const H5S_t *file_space = NULL;
    char fake_char;
    int *status = NULL;
    H5VL_iod_io_info_t *info; /* info struct used to manage I/O parameters once the operation completes*/
    uint64_t internal_cs; /* internal checksum calculated in this function */
    size_t *vl_string_len = NULL; /* array that will contain lengths of strings if the datatype is a VL string type */
    H5VL_iod_request_t **parent_reqs = NULL;
    size_t num_parents = 0;
    hid_t trans_id;
    H5TR_t *tr = NULL;
    uint64_t user_cs;
    uint32_t raw_cs_scope = 0;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* If there is information needed about the dataset that is not present locally, wait */
    if(-1 == dset->remote_dset.dcpl_id ||
       -1 == dset->remote_dset.type_id ||
       -1 == dset->remote_dset.space_id) {
        /* Synchronously wait on the request attached to the dataset */
        if(H5VL_iod_request_wait(dset->common.file, dset->common.request) < 0)
            HGOTO_ERROR(H5E_DATASET,  H5E_CANTGET, FAIL, "can't wait on HG request");
        dset->common.request = NULL;
    }

    /* At the IOD server, the array object is actually created with
       MAX dims, not current dims. So here we need to range check the
       filespace against the current dimensions of the dataset to make
       sure that the I/O happens in the current range and not the
       extensible one. */
    {
        H5S_t *dset_space, *io_space;
        hsize_t dset_dims[H5S_MAX_RANK], io_dims[H5S_MAX_RANK];
        int dset_ndims, io_ndims, i;

        if(NULL == (dset_space = (H5S_t *)H5I_object_verify(dset->remote_dset.space_id, H5I_DATASPACE)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataspace")
        if(NULL == (io_space = (H5S_t *)H5I_object_verify(file_space_id, H5I_DATASPACE)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataspace")

        dset_ndims = (int)H5S_GET_EXTENT_NDIMS(dset_space);
        io_ndims = (int)H5S_GET_EXTENT_NDIMS(io_space);

        if(dset_ndims < io_ndims)
	    HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "selection not within dataset's dataspace");

        H5S_get_simple_extent_dims(dset_space, dset_dims, NULL);
        H5S_get_simple_extent_dims(io_space, io_dims, NULL);

        for(i=0 ; i<io_ndims ; i++) {
            if(dset_dims[i] < io_dims[i])
                HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "selection Out of range");
        }
    }

    /* check arguments */
    if(H5S_ALL != mem_space_id) {
	if(NULL == (mem_space = (const H5S_t *)H5I_object_verify(mem_space_id, H5I_DATASPACE)))
	    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space");

	/* Check for valid selection */
	if(H5S_SELECT_VALID(mem_space) != TRUE)
	    HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "selection+offset not within extent");
    } /* end if */
    if(H5S_ALL != file_space_id) {
	if(NULL == (file_space = (const H5S_t *)H5I_object_verify(file_space_id, H5I_DATASPACE)))
	    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space");

	/* Check for valid selection */
	if(H5S_SELECT_VALID(file_space) != TRUE)
	    HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "selection+offset not within extent");
    } /* end if */

    if(!buf && (NULL == file_space || H5S_GET_SELECT_NPOINTS(file_space) != 0))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no output buffer");

    if(!buf)
        buf = &fake_char;

    /* get the plist pointer */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");

    /* get the TR object */
    if(H5P_get(plist, H5VL_TRANS_ID, &trans_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get property value for trans_id");
    if(NULL == (tr = (H5TR_t *)H5I_object_verify(trans_id, H5I_TR)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a Transaction ID")

    /* get the data integrity scope */
    if(H5P_get(plist, H5VL_CS_BITFLAG_NAME, &raw_cs_scope) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get scope for data integrity checks");

    /* allocate a bulk data transfer handle */
    if(NULL == (bulk_handle = (hg_bulk_t *)H5MM_malloc(sizeof(hg_bulk_t))))
        HGOTO_ERROR(H5E_DATASET, H5E_NOSPACE, FAIL, "can't allocate a buld data transfer handle");

    if(raw_cs_scope) {
        /* compute checksum and create bulk handle */
        if(H5VL_iod_pre_write(mem_type_id, mem_space_id, buf, 
                              &internal_cs, bulk_handle, &vl_string_len) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "can't generate write parameters");
    }
    else {
#if H5VL_IOD_DEBUG        
        printf("NO DATA INTEGRITY CHECKS ON RAW DATA WRITTEN\n");
#endif
        /* compute checksum and create bulk handle */
        if(H5VL_iod_pre_write(mem_type_id, mem_space_id, buf, 
                              NULL, bulk_handle, &vl_string_len) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "can't generate write parameters");
        internal_cs = 0;
    }

    /* Verify the checksum value if the dxpl contains a user defined checksum */
    if(H5P_get(plist, H5D_XFER_CHECKSUM_NAME, &user_cs) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to get checksum value");

    if((raw_cs_scope & H5_CHECKSUM_MEMORY) && user_cs && 
       user_cs != internal_cs) {
        fprintf(stderr, "Errrr.. In memory Data corruption. expecting %"PRIu64", got %"PRIu64"\n",
                user_cs, internal_cs);
        HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "Checksum verification failed");
    }

    if(NULL == (parent_reqs = (H5VL_iod_request_t **)
                H5MM_malloc(sizeof(H5VL_iod_request_t *) * 2)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate parent req element");

    /* retrieve parent requests */
    if(H5VL_iod_get_parent_requests((H5VL_iod_object_t *)dset, (H5VL_iod_req_info_t *)tr, 
                                    parent_reqs, &num_parents) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Failed to retrieve parent requests");

    /* Fill input structure */
    input.coh = dset->common.file->remote_file.coh;
    input.iod_oh = dset->remote_dset.iod_oh;
    input.iod_id = dset->remote_dset.iod_id;
    input.mdkv_id = dset->remote_dset.mdkv_id;
    input.bulk_handle = *bulk_handle;
    input.checksum = internal_cs;
    input.dxpl_id = dxpl_id;
    input.space_id = file_space_id;
    input.dset_type_id = dset->remote_dset.type_id;
    input.mem_type_id = mem_type_id;
    input.trans_num = tr->trans_num;
    input.rcxt_num  = tr->c_version;
    input.cs_scope = dset->common.file->md_integrity_scope;

    status = (int *)malloc(sizeof(int));

#if H5VL_IOD_DEBUG
    printf("Dataset Write, axe id %"PRIu64"\n", g_axe_id);
#endif

    /* setup info struct for I/O request 
       This is to manage the I/O operation once the wait is called. */
    if(NULL == (info = (H5VL_iod_io_info_t *)H5MM_calloc(sizeof(H5VL_iod_io_info_t))))
	HGOTO_ERROR(H5E_DATASET, H5E_NOSPACE, FAIL, "can't allocate a request");

    info->status = status;
    info->bulk_handle = bulk_handle;
    info->vl_string_len = vl_string_len;

    if(H5VL__iod_create_and_forward(H5VL_DSET_WRITE_ID, HG_DSET_WRITE, 
                                    (H5VL_iod_object_t *)dset, 0,
                                    num_parents, parent_reqs,
                                    (H5VL_iod_req_info_t *)tr, &input, status, info, req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to create and ship dataset write");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_dataset_write() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_dataset_set_extent
 *
 * Purpose:	Set Extent of dataset
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:  Mohamad Chaarawi
 *              October, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t 
H5VL_iod_dataset_set_extent(void *_dset, const hsize_t size[], 
                            hid_t dxpl_id, void **req)
{
    H5VL_iod_dset_t *dset = (H5VL_iod_dset_t *)_dset;
    dset_set_extent_in_t input;
    int *status = NULL;
    size_t num_parents = 0;
    hid_t trans_id;
    H5TR_t *tr = NULL;
    H5P_genplist_t *plist = NULL;
    H5VL_iod_request_t **parent_reqs = NULL;
    herr_t ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* If there is information needed about the dataset that is not present locally, wait */
    if(-1 == dset->remote_dset.space_id) {
        /* Synchronously wait on the request attached to the dataset */
        if(H5VL_iod_request_wait(dset->common.file, dset->common.request) < 0)
            HGOTO_ERROR(H5E_DATASET,  H5E_CANTGET, FAIL, "can't wait on HG request");
        dset->common.request = NULL;
    }

    /* get the transaction ID */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");
    if(H5P_get(plist, H5VL_TRANS_ID, &trans_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get property value for trans_id");

    /* get the TR object */
    if(NULL == (tr = (H5TR_t *)H5I_object_verify(trans_id, H5I_TR)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a Transaction ID")

    if(H5VL_iod_get_obj_requests((H5VL_iod_object_t *)dset, &num_parents, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't get num requests");

    if(num_parents) {
        if(NULL == (parent_reqs = (H5VL_iod_request_t **)H5MM_malloc
                    (sizeof(H5VL_iod_request_t *) * num_parents)))
            HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate array of parent reqs");
        if(H5VL_iod_get_obj_requests((H5VL_iod_object_t *)dset, &num_parents, 
                                     parent_reqs) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't get parent requests");
    }

    /* Fill input structure */
    input.coh = dset->common.file->remote_file.coh;
    input.iod_oh = dset->remote_dset.iod_oh;
    input.iod_id = dset->remote_dset.iod_id;
    input.mdkv_id = dset->remote_dset.mdkv_id;
    input.dims.rank = H5Sget_simple_extent_ndims(dset->remote_dset.space_id);
    input.dims.size = size;
    input.trans_num = tr->trans_num;
    input.rcxt_num  = tr->c_version;
    input.cs_scope = dset->common.file->md_integrity_scope;

#if H5VL_IOD_DEBUG
    printf("Dataset Set Extent, axe id %"PRIu64"\n", g_axe_id);
#endif

    status = (int *)malloc(sizeof(int));

    if(H5VL__iod_create_and_forward(H5VL_DSET_SET_EXTENT_ID, HG_DSET_SET_EXTENT, 
                                    (H5VL_iod_object_t *)dset, 1,
                                    num_parents, parent_reqs,
                                    (H5VL_iod_req_info_t *)tr, &input, status, status, req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to create and ship dataset set_extent");

    /* modify the local dataspace of the dataset */
    {
        int     rank;                       /* Dataspace # of dimensions */
        H5S_t   *space;                     /* Dataset's dataspace */
        hsize_t curr_dims[H5O_LAYOUT_NDIMS];/* Current dimension sizes */

	if(NULL == (space = (H5S_t *)H5I_object_verify(dset->remote_dset.space_id, 
                                                       H5I_DATASPACE)))
	    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space");

        /* Check if we are shrinking or expanding any of the dimensions */
        if((rank = H5S_get_simple_extent_dims(space, curr_dims, NULL)) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't get dataset dimensions");

        /* Modify the size of the data space */
        if(H5S_set_extent(space, size) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to modify size of data space");
    }
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_dataset_set_extent() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_dataset_get
 *
 * Purpose:	Gets certain information about a dataset
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:  Mohamad Chaarawi
 *              March, 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_iod_dataset_get(void *_dset, H5VL_dataset_get_t get_type, 
                     hid_t UNUSED dxpl_id, 
                     void UNUSED **req, va_list arguments)
{
    H5VL_iod_dset_t *dset = (H5VL_iod_dset_t *)_dset;
    herr_t       ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    if(-1 == dset->remote_dset.dcpl_id ||
       -1 == dset->remote_dset.type_id ||
       -1 == dset->remote_dset.space_id) {
        /* Synchronously wait on the request attached to the dataset */
        if(H5VL_iod_request_wait(dset->common.file, dset->common.request) < 0)
            HGOTO_ERROR(H5E_DATASET,  H5E_CANTGET, FAIL, "can't wait on HG request");
        dset->common.request = NULL;
    }

    switch (get_type) {
        case H5VL_DATASET_GET_DCPL:
            {
                hid_t *plist_id = va_arg (arguments, hid_t *);

                /* Retrieve the file's access property list */
                if((*plist_id = H5Pcopy(dset->remote_dset.dcpl_id)) < 0)
                    HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get dset creation property list")

                break;
            }
        case H5VL_DATASET_GET_DAPL:
            {
                hid_t *plist_id = va_arg (arguments, hid_t *);

                /* Retrieve the file's access property list */
                if((*plist_id = H5Pcopy(dset->dapl_id)) < 0)
                    HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get dset access property list")

                break;
            }
        case H5VL_DATASET_GET_SPACE:
            {
                hid_t	*ret_id = va_arg (arguments, hid_t *);

                if((*ret_id = H5Scopy(dset->remote_dset.space_id)) < 0)
                    HGOTO_ERROR(H5E_ARGS, H5E_CANTGET, FAIL, "can't get dataspace ID of dataset");
                break;
            }
        case H5VL_DATASET_GET_SPACE_STATUS:
            {
                H5D_space_status_t *allocation = va_arg (arguments, H5D_space_status_t *);

                *allocation = H5D_SPACE_STATUS_NOT_ALLOCATED;
                break;
            }
        case H5VL_DATASET_GET_TYPE:
            {
                hid_t	*ret_id = va_arg (arguments, hid_t *);

                if((*ret_id = H5Tcopy(dset->remote_dset.type_id)) < 0)
                    HGOTO_ERROR(H5E_ARGS, H5E_CANTGET, FAIL, "can't get datatype ID of dataset")
            }
        case H5VL_DATASET_GET_STORAGE_SIZE:
        case H5VL_DATASET_GET_OFFSET:
        default:
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't get this type of information from dataset")
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_dataset_get() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_dataset_close
 *
 * Purpose:	Closes a dataset.
 *
 * Return:	Success:	0
 *		Failure:	-1, dataset not closed.
 *
 * Programmer:  Mohamad Chaarawi
 *              March, 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_iod_dataset_close(void *_dset, hid_t UNUSED dxpl_id, void **req)
{
    H5VL_iod_dset_t *dset = (H5VL_iod_dset_t *)_dset;
    dset_close_in_t input;
    int *status = NULL;
    size_t num_parents = 0;
    H5VL_iod_request_t **parent_reqs = NULL;
    herr_t ret_value = SUCCEED;  /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    if(-1 == dset->remote_dset.dcpl_id ||
       -1 == dset->remote_dset.type_id ||
       -1 == dset->remote_dset.space_id ||
       IOD_OH_UNDEFINED == dset->remote_dset.iod_oh.rd_oh.cookie) {
        /* Synchronously wait on the request attached to the dataset */
        if(H5VL_iod_request_wait(dset->common.file, dset->common.request) < 0)
            HGOTO_ERROR(H5E_DATASET,  H5E_CANTGET, FAIL, "can't wait dset request");
        dset->common.request = NULL;
    }

    /* If this call is not asynchronous, complete and remove all
       requests that are associated with this object from the List */
    if(NULL == req) {
        if(H5VL_iod_request_wait_some(dset->common.file, dset) < 0)
            HGOTO_ERROR(H5E_DATASET,  H5E_CANTGET, FAIL, "can't wait on all object requests");
    }

    if(H5VL_iod_get_obj_requests((H5VL_iod_object_t *)dset, &num_parents, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't get num requests");

    if(num_parents) {
        if(NULL == (parent_reqs = (H5VL_iod_request_t **)H5MM_malloc
                    (sizeof(H5VL_iod_request_t *) * num_parents)))
            HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate array of parent reqs");
        if(H5VL_iod_get_obj_requests((H5VL_iod_object_t *)dset, &num_parents, 
                                     parent_reqs) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't get parent requests");
    }

    input.iod_oh = dset->remote_dset.iod_oh;
    input.iod_id = dset->remote_dset.iod_id;

    status = (int *)malloc(sizeof(int));

#if H5VL_IOD_DEBUG
    printf("Dataset Close IOD ID %"PRIu64", axe id %"PRIu64"\n", 
           input.iod_id, g_axe_id);
#endif

    if(H5VL__iod_create_and_forward(H5VL_DSET_CLOSE_ID, HG_DSET_CLOSE, 
                                    (H5VL_iod_object_t *)dset, 1,
                                    num_parents, parent_reqs,
                                    NULL, &input, status, status, req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to create and ship dataset close");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_dataset_close() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_datatype_commit
 *
 * Purpose:	Commits a datatype inside the container.
 *
 * Return:	Success:	datatype
 *		Failure:	NULL
 *
 * Programmer:  Mohamad Chaarawi
 *              April, 2013
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL_iod_datatype_commit(void *_obj, H5VL_loc_params_t UNUSED loc_params, const char *name, 
                         hid_t type_id, hid_t lcpl_id, hid_t tcpl_id, hid_t tapl_id, 
                         hid_t dxpl_id, void **req)
{
    H5VL_iod_object_t *obj = (H5VL_iod_object_t *)_obj; /* location object to create the datatype */
    H5VL_iod_dtype_t  *dtype = NULL; /* the datatype object that is created and passed to the user */
    dtype_commit_in_t input;
    iod_obj_id_t iod_id;
    iod_handles_t iod_oh;
    H5P_genplist_t *plist = NULL;
    size_t num_parents = 0;
    hid_t trans_id;
    H5TR_t *tr = NULL;
    H5VL_iod_request_t **parent_reqs = NULL;
    void *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    /* get the transaction ID */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, NULL, "can't find object for ID");
    if(H5P_get(plist, H5VL_TRANS_ID, &trans_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get property value for trans_id");

    /* get the TR object */
    if(NULL == (tr = (H5TR_t *)H5I_object_verify(trans_id, H5I_TR)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "not a Transaction ID")

    /* allocate parent request array */
    if(NULL == (parent_reqs = (H5VL_iod_request_t **)
                H5MM_malloc(sizeof(H5VL_iod_request_t *) * 2)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, NULL, "can't allocate parent req element");

    /* retrieve parent requests */
    if(H5VL_iod_get_parent_requests(obj, (H5VL_iod_req_info_t *)tr, parent_reqs, &num_parents) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "Failed to retrieve parent requests");

    /* retrieve IOD info of location object */
    if(H5VL_iod_get_loc_info(obj, &iod_id, &iod_oh, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "Failed to resolve current location group info");

    /* MSC - If location object not opened yet, wait for it. */
    if(IOD_OBJ_INVALID == iod_id) {
        /* Synchronously wait on the request attached to the dataset */
        if(H5VL_iod_request_wait(obj->file, obj->request) < 0)
            HGOTO_ERROR(H5E_DATASET,  H5E_CANTGET, NULL, "can't wait on HG request");
        obj->request = NULL;
        /* retrieve IOD info of location object */
        if(H5VL_iod_get_loc_info(obj, &iod_id, &iod_oh, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "Failed to resolve current location group info");
    }

    /* allocate the datatype object that is returned to the user */
    if(NULL == (dtype = H5FL_CALLOC(H5VL_iod_dtype_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate object struct");

    dtype->remote_dtype.iod_oh.rd_oh.cookie = IOD_OH_UNDEFINED;
    dtype->remote_dtype.iod_oh.wr_oh.cookie = IOD_OH_UNDEFINED;
    dtype->remote_dtype.iod_id = IOD_OBJ_INVALID;

    /* Generate IOD IDs for the group to be created */
    H5VL_iod_gen_obj_id(obj->file->my_rank, obj->file->num_procs, 
                        obj->file->remote_file.blob_oid_index, 
                        IOD_OBJ_BLOB, &input.dtype_id);
    dtype->remote_dtype.iod_id = input.dtype_id;
    /* increment the index of BLOB objects created on the container */
    obj->file->remote_file.blob_oid_index ++;

    H5VL_iod_gen_obj_id(obj->file->my_rank, obj->file->num_procs, 
                        obj->file->remote_file.kv_oid_index, 
                        IOD_OBJ_KV, &input.mdkv_id);
    dtype->remote_dtype.mdkv_id = input.mdkv_id;
    /* increment the index of KV objects created on the container */
    obj->file->remote_file.kv_oid_index ++;

    H5VL_iod_gen_obj_id(obj->file->my_rank, obj->file->num_procs, 
                        obj->file->remote_file.kv_oid_index, 
                        IOD_OBJ_KV, &input.attrkv_id);
    dtype->remote_dtype.attrkv_id = input.attrkv_id;
    /* increment the index of KV objects created on the container */
    obj->file->remote_file.kv_oid_index ++;

    /* set the input structure for the HG encode routine */
    input.coh = obj->file->remote_file.coh;
    input.loc_id = iod_id;
    input.loc_oh = iod_oh;
    input.name = name;
    input.tcpl_id = tcpl_id;
    input.tapl_id = tapl_id;
    input.lcpl_id = lcpl_id;
    input.type_id = type_id;
    input.trans_num = tr->trans_num;
    input.rcxt_num  = tr->c_version;
    input.cs_scope = obj->file->md_integrity_scope;

#if H5VL_IOD_DEBUG
    printf("Datatype Commit %s IOD ID %"PRIu64", axe id %"PRIu64"\n", 
           name, input.dtype_id, g_axe_id);
#endif

    /* setup the local datatype struct */
    /* store the entire path of the datatype locally */
    {
        size_t obj_name_len = HDstrlen(obj->obj_name);
        size_t name_len = HDstrlen(name);

        if (NULL == (dtype->common.obj_name = (char *)HDmalloc(obj_name_len + name_len + 1)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate");
        HDmemcpy(dtype->common.obj_name, obj->obj_name, obj_name_len);
        HDmemcpy(dtype->common.obj_name+obj_name_len, name, name_len);
        dtype->common.obj_name[obj_name_len+name_len] = '\0';
    }

    /* store a copy of the datatype parameters*/
    if((dtype->remote_dtype.tcpl_id = H5Pcopy(tcpl_id)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, NULL, "failed to copy dcpl");
    if((dtype->tapl_id = H5Pcopy(tapl_id)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, NULL, "failed to copy dapl");
    if((dtype->remote_dtype.type_id = H5Tcopy(type_id)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, NULL, "failed to copy dtype");

    /* set common object parameters */
    dtype->common.obj_type = H5I_DATATYPE;
    dtype->common.file = obj->file;
    dtype->common.file->nopen_objs ++;

    if(H5VL__iod_create_and_forward(H5VL_DTYPE_COMMIT_ID, HG_DTYPE_COMMIT, 
                                    (H5VL_iod_object_t *)dtype, 1, 
                                    num_parents, parent_reqs,
                                    (H5VL_iod_req_info_t *)tr, &input, &dtype->remote_dtype, dtype, req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to create and ship datatype commit");

    ret_value = (void *)dtype;

done:

    if(ret_value == NULL) {
        if(dtype != NULL) {
            dtype = H5FL_FREE(H5VL_iod_dtype_t, dtype);
        } /* end if */
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_datatype_commit() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_datatype_open
 *
 * Purpose:	Opens a named datatype.
 *
 * Return:	Success:	datatype id. 
 *		Failure:	NULL
 *
 * Programmer:  Mohamad Chaarawi
 *              April, 2013
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL_iod_datatype_open(void *_obj, H5VL_loc_params_t UNUSED loc_params, const char *name, 
                       hid_t tapl_id, hid_t dxpl_id, void **req)
{
    H5VL_iod_object_t *obj = (H5VL_iod_object_t *)_obj; /* location object to create the datatype */
    H5VL_iod_dtype_t *dtype = NULL; /* the datatype object that is created and passed to the user */
    dtype_open_in_t input;
    iod_obj_id_t iod_id;
    iod_handles_t iod_oh;
    H5P_genplist_t *plist = NULL;
    hid_t rcxt_id;
    H5RC_t *rc = NULL;
    size_t num_parents = 0;
    H5VL_iod_request_t **parent_reqs = NULL;
    void *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    /* get the context ID */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, NULL, "can't find object for ID");
    if(H5P_get(plist, H5VL_CONTEXT_ID, &rcxt_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get property value for trans_id");

    /* get the RC object */
    if(NULL == (rc = (H5RC_t *)H5I_object_verify(rcxt_id, H5I_RC)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "not a READ CONTEXT ID")

    /* allocate parent request array */
    if(NULL == (parent_reqs = (H5VL_iod_request_t **)
                H5MM_malloc(sizeof(H5VL_iod_request_t *))))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, NULL, "can't allocate parent req element");

    /* retrieve parent requests */
    if(H5VL_iod_get_parent_requests(obj, (H5VL_iod_req_info_t *)rc, parent_reqs, &num_parents) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "Failed to retrieve parent requests");

    /* retrieve IOD info of location object */
    if(H5VL_iod_get_loc_info(obj, &iod_id, &iod_oh, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "Failed to resolve current location group info");

    /* MSC - If location object not opened yet, wait for it. */
    if(IOD_OBJ_INVALID == iod_id) {
        /* Synchronously wait on the request attached to the dataset */
        if(H5VL_iod_request_wait(obj->file, obj->request) < 0)
            HGOTO_ERROR(H5E_DATASET,  H5E_CANTGET, NULL, "can't wait on HG request");
        obj->request = NULL;
        /* retrieve IOD info of location object */
        if(H5VL_iod_get_loc_info(obj, &iod_id, &iod_oh, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "Failed to resolve current location group info");
    }

    /* allocate the datatype object that is returned to the user */
    if(NULL == (dtype = H5FL_CALLOC(H5VL_iod_dtype_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate object struct");

    dtype->remote_dtype.iod_oh.rd_oh.cookie = IOD_OH_UNDEFINED;
    dtype->remote_dtype.iod_oh.wr_oh.cookie = IOD_OH_UNDEFINED;
    dtype->remote_dtype.iod_id = IOD_OBJ_INVALID;
    dtype->remote_dtype.mdkv_id = IOD_OBJ_INVALID;
    dtype->remote_dtype.attrkv_id = IOD_OBJ_INVALID;
    dtype->remote_dtype.tcpl_id = -1;
    dtype->remote_dtype.type_id = -1;

    /* set the input structure for the HG encode routine */
    input.coh = obj->file->remote_file.coh;
    input.loc_id = iod_id;
    input.loc_oh = iod_oh;
    input.name = name;
    input.tapl_id = tapl_id;
    input.rcxt_num  = rc->c_version;
    input.cs_scope = obj->file->md_integrity_scope;

#if H5VL_IOD_DEBUG
    printf("Datatype Open %s LOC ID %"PRIu64", axe id %"PRIu64"\n", 
           name, input.loc_id, g_axe_id);
#endif

    /* setup the local datatype struct */
    /* store the entire path of the datatype locally */
    {
        size_t obj_name_len = HDstrlen(obj->obj_name);
        size_t name_len = HDstrlen(name);

        if (NULL == (dtype->common.obj_name = (char *)HDmalloc(obj_name_len + name_len + 1)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate");
        HDmemcpy(dtype->common.obj_name, obj->obj_name, obj_name_len);
        HDmemcpy(dtype->common.obj_name+obj_name_len, name, name_len);
        dtype->common.obj_name[obj_name_len+name_len] = '\0';
    }

    if((dtype->tapl_id = H5Pcopy(tapl_id)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, NULL, "failed to copy tapl");

    /* set common object parameters */
    dtype->common.obj_type = H5I_DATATYPE;
    dtype->common.file = obj->file;
    dtype->common.file->nopen_objs ++;

    if(H5VL__iod_create_and_forward(H5VL_DTYPE_OPEN_ID, HG_DTYPE_OPEN, 
                                    (H5VL_iod_object_t *)dtype, 1, 
                                    num_parents, parent_reqs,
                                    (H5VL_iod_req_info_t *)rc, &input, &dtype->remote_dtype, dtype, req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to create and ship datatype open");

    ret_value = (void *)dtype;

done:

    if(ret_value == NULL) {
        if(dtype != NULL) {
            dtype = H5FL_FREE(H5VL_iod_dtype_t, dtype);
        } /* end if */
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_datatype_open() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_datatype_get_binary
 *
 * Purpose:	gets size required to encode the datatype
 *
 * Return:	Success:	datatype id. 
 *		Failure:	-1
 *
 * Programmer:  Mohamad Chaarawi
 *              April, 2013
 *
 *-------------------------------------------------------------------------
 */
static ssize_t
H5VL_iod_datatype_get_binary(void *obj, unsigned char *buf, size_t size, 
                             hid_t UNUSED dxpl_id, void UNUSED **req)
{
    H5VL_iod_dtype_t *dtype = (H5VL_iod_dtype_t *)obj;
    size_t       nalloc = size;
    ssize_t      ret_value = FAIL;

    FUNC_ENTER_NOAPI_NOINIT

    if(H5Tencode(dtype->remote_dtype.type_id, buf, &nalloc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "can't determine serialized length of datatype")

    ret_value = (ssize_t) nalloc;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_datatype_get_binary() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_datatype_get
 *
 * Purpose:	Gets certain information about a datatype
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:  Mohamad Chaarawi
 *              June, 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_iod_datatype_get(void UNUSED *obj, H5VL_datatype_get_t get_type, 
                      hid_t UNUSED dxpl_id, void UNUSED **req, va_list UNUSED arguments)
{
    herr_t       ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    switch (get_type) {
        /* H5Tget_create_plist */
        case H5VL_DATATYPE_GET_TCPL:
            {
                hid_t *ret_id = va_arg (arguments, hid_t *);
                H5P_genplist_t *tcpl_plist = NULL; /* New datatype creation property list */
                hid_t tcpl_id;

                /* Copy the default datatype creation property list */
                if(NULL == (tcpl_plist = (H5P_genplist_t *)H5I_object(H5P_LST_DATATYPE_CREATE_g)))
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "can't get default creation property list")
                if((tcpl_id = H5P_copy_plist(tcpl_plist, TRUE)) < 0)
                    HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, FAIL, "unable to copy the creation property list")

                *ret_id = tcpl_id;
                break;
            }
        default:
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't get this type of information from datatype")
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_datatype_get() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_datatype_close
 *
 * Purpose:	Closes an datatype.
 *
 * Return:	Success:	0
 *		Failure:	-1, datatype not closed.
 *
 * Programmer:  Mohamad Chaarawi
 *              April, 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_iod_datatype_close(void *obj, hid_t UNUSED dxpl_id, void **req)
{
    H5VL_iod_dtype_t *dtype = (H5VL_iod_dtype_t *)obj;
    dtype_close_in_t input;
    size_t num_parents = 0;
    H5VL_iod_request_t **parent_reqs = NULL;
    int *status = NULL;
    herr_t ret_value = SUCCEED;  /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* If this call is not asynchronous, complete and remove all
       requests that are associated with this object from the List */
    if(NULL == req) {
        if(H5VL_iod_request_wait_some(dtype->common.file, dtype) < 0)
            HGOTO_ERROR(H5E_FILE,  H5E_CANTGET, FAIL, "can't wait on all object requests");
    }

    if(IOD_OH_UNDEFINED == dtype->remote_dtype.iod_oh.rd_oh.cookie) {
        /* Synchronously wait on the request attached to the dtype */
        if(H5VL_iod_request_wait(dtype->common.file, dtype->common.request) < 0)
            HGOTO_ERROR(H5E_SYM,  H5E_CANTGET, FAIL, "can't wait on dtype request");
        dtype->common.request = NULL;
    }

    if(H5VL_iod_get_obj_requests((H5VL_iod_object_t *)dtype, &num_parents, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't get num requests");

    if(num_parents) {
        if(NULL == (parent_reqs = (H5VL_iod_request_t **)
                    H5MM_malloc(sizeof(H5VL_iod_request_t *) * num_parents)))
            HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate array of parent reqs");
        if(H5VL_iod_get_obj_requests((H5VL_iod_object_t *)dtype, &num_parents, 
                                    parent_reqs) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't get parent requests");
    }

    input.iod_oh = dtype->remote_dtype.iod_oh;
    input.iod_id = dtype->remote_dtype.iod_id;

#if H5VL_IOD_DEBUG
    printf("Datatype Close IOD ID %"PRIu64", axe id %"PRIu64"\n", input.iod_id, g_axe_id);
#endif

    status = (int *)malloc(sizeof(int));

    if(H5VL__iod_create_and_forward(H5VL_DTYPE_CLOSE_ID, HG_DTYPE_CLOSE, 
                                    (H5VL_iod_object_t *)dtype, 1,
                                    num_parents, parent_reqs,
                                    NULL, &input, status, status, req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to create and ship datatype open");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_datatype_close() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_attribute_create
 *
 * Purpose:	Sends a request to the IOD to create a attribute
 *
 * Return:	Success:	attribute object. 
 *		Failure:	NULL
 *
 * Programmer:  Mohamad Chaarawi
 *              October, 2013
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL_iod_attribute_create(void *_obj, H5VL_loc_params_t loc_params, const char *attr_name, 
                          hid_t acpl_id, hid_t UNUSED aapl_id, hid_t dxpl_id, void **req)
{
    H5VL_iod_object_t *obj = (H5VL_iod_object_t *)_obj; /* location object to create the attribute */
    H5VL_iod_attr_t *attr = NULL; /* the attribute object that is created and passed to the user */
    attr_create_in_t input;
    H5P_genplist_t *plist = NULL;
    iod_obj_id_t iod_id, attrkv_id;
    iod_handles_t iod_oh;
    const char *path; /* path on where the traversal starts relative to the location object specified */
    char *loc_name = NULL;
    hid_t type_id, space_id;
    H5VL_iod_request_t **parent_reqs = NULL;
    size_t num_parents = 0;
    hid_t trans_id;
    H5TR_t *tr = NULL;
    void *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    /* Get the acpl plist structure */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(acpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, NULL, "can't find object for ID");

    /* get datatype, dataspace, and lcpl IDs that were added in the acpl at the API layer */
    if(H5P_get(plist, H5VL_ATTR_TYPE_ID, &type_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get property value for datatype id");
    if(H5P_get(plist, H5VL_ATTR_SPACE_ID, &space_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get property value for space id");

    /* get the transaction ID */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, NULL, "can't find object for ID");
    if(H5P_get(plist, H5VL_TRANS_ID, &trans_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get property value for trans_id");

    /* get the TR object */
    if(NULL == (tr = (H5TR_t *)H5I_object_verify(trans_id, H5I_TR)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "not a Transaction ID")

    /* allocate parent request array */
    if(NULL == (parent_reqs = (H5VL_iod_request_t **)
                H5MM_malloc(sizeof(H5VL_iod_request_t *) * 2)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, NULL, "can't allocate parent req element");

    /* retrieve parent requests */
    if(H5VL_iod_get_parent_requests(obj, (H5VL_iod_req_info_t *)tr, parent_reqs, &num_parents) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "Failed to retrieve parent requests");

    /* retrieve IOD info of location object */
    if(H5VL_iod_get_loc_info(obj, &iod_id, &iod_oh, NULL, &attrkv_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "Failed to resolve current location group info");

    /* MSC - If location object not opened yet, wait for it. */
    if(IOD_OBJ_INVALID == iod_id) {
        /* Synchronously wait on the request attached to the dataset */
        if(H5VL_iod_request_wait(obj->file, obj->request) < 0)
            HGOTO_ERROR(H5E_DATASET,  H5E_CANTGET, NULL, "can't wait on HG request");
        obj->request = NULL;
        /* retrieve IOD info of location object */
        if(H5VL_iod_get_loc_info(obj, &iod_id, &iod_oh, NULL, &attrkv_id) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "Failed to resolve current location group info");
    }

    /* allocate the attribute object that is returned to the user */
    if(NULL == (attr = H5FL_CALLOC(H5VL_iod_attr_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate object struct");

    attr->remote_attr.iod_oh.rd_oh.cookie = IOD_OH_UNDEFINED;
    attr->remote_attr.iod_oh.wr_oh.cookie = IOD_OH_UNDEFINED;

    /* Generate IOD IDs for the attr to be created */
    H5VL_iod_gen_obj_id(obj->file->my_rank, obj->file->num_procs, 
                        obj->file->remote_file.array_oid_index, 
                        IOD_OBJ_ARRAY, &input.attr_id);
    attr->remote_attr.iod_id = input.attr_id;
    /* increment the index of ARRAY objects created on the container */
    obj->file->remote_file.array_oid_index ++;

    H5VL_iod_gen_obj_id(obj->file->my_rank, obj->file->num_procs, 
                        obj->file->remote_file.kv_oid_index, 
                        IOD_OBJ_KV, &input.mdkv_id);
    attr->remote_attr.mdkv_id = input.mdkv_id;
    /* increment the index of KV objects created on the container */
    obj->file->remote_file.kv_oid_index ++;

    if(H5VL_OBJECT_BY_SELF == loc_params.type)
        loc_name = strdup(".");
    else if(H5VL_OBJECT_BY_NAME == loc_params.type)
        loc_name = strdup(loc_params.loc_data.loc_by_name.name);

    /* set the input structure for the HG encode routine */
    input.coh = obj->file->remote_file.coh;
    input.loc_id = iod_id;
    input.loc_attrkv_id = attrkv_id;
    input.loc_oh = iod_oh;
    input.path = loc_name;
    input.attr_name = attr_name;
    input.type_id = type_id;
    input.space_id = space_id;
    input.trans_num = tr->trans_num;
    input.rcxt_num  = tr->c_version;
    input.cs_scope = obj->file->md_integrity_scope;

    /* setup the local attribute struct */
    /* store the entire path of the attribute locally */
    if(loc_params.type == H5VL_OBJECT_BY_SELF) {
        path = NULL;
        attr->loc_name = HDstrdup(obj->obj_name);
    }
    else if (loc_params.type == H5VL_OBJECT_BY_NAME) {
        size_t obj_name_len = HDstrlen(obj->obj_name);
        size_t name_len;

        path = loc_params.loc_data.loc_by_name.name;
        name_len = HDstrlen(path);

        if (NULL == (attr->loc_name = (char *)HDmalloc(obj_name_len + name_len + 1)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate");
        HDmemcpy(attr->loc_name, obj->obj_name, obj_name_len);
        HDmemcpy(attr->loc_name+obj_name_len, path, name_len);
        attr->loc_name[obj_name_len+name_len] = '\0';
    }

    /* store the name of the attribute locally */
    attr->common.obj_name = strdup(attr_name);

#if H5VL_IOD_DEBUG
    printf("Attribute Create %s IOD ID %"PRIu64", axe id %"PRIu64"\n", 
           attr_name, input.attr_id, g_axe_id);
#endif

    /* copy dtype, and dspace */
    if((attr->remote_attr.type_id = H5Tcopy(type_id)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, NULL, "failed to copy dtype");
    if((attr->remote_attr.space_id = H5Scopy(space_id)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, NULL, "failed to copy dspace");

    /* set common object parameters */
    attr->common.obj_type = H5I_ATTR;
    attr->common.file = obj->file;
    attr->common.file->nopen_objs ++;

    if(H5VL__iod_create_and_forward(H5VL_ATTR_CREATE_ID, HG_ATTR_CREATE, 
                                    (H5VL_iod_object_t *)attr, 1, num_parents, parent_reqs,
                                    (H5VL_iod_req_info_t *)tr, &input, &attr->remote_attr, 
                                    attr, req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to create and ship attribute create");

    ret_value = (void *)attr;

done:
    if(loc_name) 
        HDfree(loc_name);

    if(ret_value == NULL) {
        if(attr != NULL) {
            attr = H5FL_FREE(H5VL_iod_attr_t, attr);
        } /* end if */
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_attribute_create() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_attribute_open
 *
 * Purpose:	Sends a request to the IOD to open a attribute
 *
 * Return:	Success:	attribute object. 
 *		Failure:	NULL
 *
 * Programmer:  Mohamad Chaarawi
 *              October, 2013
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL_iod_attribute_open(void *_obj, H5VL_loc_params_t loc_params, const char *attr_name, 
                        hid_t UNUSED aapl_id, hid_t dxpl_id, void **req)
{
    H5VL_iod_object_t *obj = (H5VL_iod_object_t *)_obj; /* location object to create the attribute */
    H5VL_iod_attr_t *attr = NULL; /* the attribute object that is created and passed to the user */
    attr_open_in_t input;
    const char *path; /* path on where the traversal starts relative to the location object specified */
    char *loc_name = NULL;
    iod_obj_id_t iod_id, attrkv_id;
    iod_handles_t iod_oh;
    H5P_genplist_t *plist = NULL;
    hid_t rcxt_id;
    H5RC_t *rc = NULL;
    size_t num_parents = 0;
    H5VL_iod_request_t **parent_reqs = NULL;
    void *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    /* get the context ID */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, NULL, "can't find object for ID");
    if(H5P_get(plist, H5VL_CONTEXT_ID, &rcxt_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get property value for trans_id");

    /* get the RC object */
    if(NULL == (rc = (H5RC_t *)H5I_object_verify(rcxt_id, H5I_RC)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "not a READ CONTEXT ID")

    /* allocate parent request array */
    if(NULL == (parent_reqs = (H5VL_iod_request_t **)
                H5MM_malloc(sizeof(H5VL_iod_request_t *))))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, NULL, "can't allocate parent req element");

    /* retrieve parent requests */
    if(H5VL_iod_get_parent_requests(obj, (H5VL_iod_req_info_t *)rc, parent_reqs, &num_parents) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "Failed to retrieve parent requests");

    /* retrieve IOD info of location object */
    if(H5VL_iod_get_loc_info(obj, &iod_id, &iod_oh, NULL, &attrkv_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "Failed to resolve current location group info");

    /* MSC - If location object not opened yet, wait for it. */
    if(IOD_OBJ_INVALID == iod_id) {
        /* Synchronously wait on the request attached to the dataset */
        if(H5VL_iod_request_wait(obj->file, obj->request) < 0)
            HGOTO_ERROR(H5E_DATASET,  H5E_CANTGET, NULL, "can't wait on HG request");
        obj->request = NULL;
        /* retrieve IOD info of location object */
        if(H5VL_iod_get_loc_info(obj, &iod_id, &iod_oh, NULL, &attrkv_id) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "Failed to resolve current location group info");
    }

    /* allocate the attribute object that is returned to the user */
    if(NULL == (attr = H5FL_CALLOC(H5VL_iod_attr_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate object struct");

    attr->remote_attr.iod_oh.rd_oh.cookie = IOD_OH_UNDEFINED;
    attr->remote_attr.iod_oh.wr_oh.cookie = IOD_OH_UNDEFINED;
    attr->remote_attr.iod_id = IOD_OBJ_INVALID;
    attr->remote_attr.mdkv_id = IOD_OBJ_INVALID;
    attr->remote_attr.type_id = -1;
    attr->remote_attr.space_id = -1;

    if(H5VL_OBJECT_BY_SELF == loc_params.type)
        loc_name = strdup(".");
    else if(H5VL_OBJECT_BY_NAME == loc_params.type)
        loc_name = strdup(loc_params.loc_data.loc_by_name.name);

    /* set the input structure for the HG encode routine */
    input.coh = obj->file->remote_file.coh;
    input.loc_id = iod_id;
    input.loc_attrkv_id = attrkv_id;
    input.loc_oh = iod_oh;
    input.path = loc_name;
    input.attr_name = attr_name;
    input.rcxt_num  = rc->c_version;
    input.cs_scope = obj->file->md_integrity_scope;

#if H5VL_IOD_DEBUG
    printf("Attribute Open %s LOC ID %"PRIu64", axe id %"PRIu64"\n", 
           attr_name, input.loc_id, g_axe_id);
#endif

    /* setup the local attribute struct */

    /* store the entire path of the attribute locally */
    if(loc_params.type == H5VL_OBJECT_BY_SELF) {
        path = NULL;
        attr->loc_name = HDstrdup(obj->obj_name);
    }
    else if (loc_params.type == H5VL_OBJECT_BY_NAME) {
        size_t obj_name_len = HDstrlen(obj->obj_name);
        size_t name_len;

        path = loc_params.loc_data.loc_by_name.name;
        name_len = HDstrlen(path);

        if (NULL == (attr->loc_name = (char *)HDmalloc(obj_name_len + name_len + 1)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate");
        HDmemcpy(attr->loc_name, obj->obj_name, obj_name_len);
        HDmemcpy(attr->loc_name+obj_name_len, path, name_len);
        attr->loc_name[obj_name_len+name_len] = '\0';
    }

    /* store the name of the attribute locally */
    attr->common.obj_name = strdup(attr_name);

    /* set common object parameters */
    attr->common.obj_type = H5I_ATTR;
    attr->common.file = obj->file;
    attr->common.file->nopen_objs ++;

    if(H5VL__iod_create_and_forward(H5VL_ATTR_OPEN_ID, HG_ATTR_OPEN, 
                                    (H5VL_iod_object_t *)attr, 1, 
                                    num_parents, parent_reqs,
                                    (H5VL_iod_req_info_t *)rc, &input, &attr->remote_attr, attr, req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to create and ship attribute open");

    ret_value = (void *)attr;

done:
    if(loc_name) 
        HDfree(loc_name);

    if(ret_value == NULL) {
        if(attr != NULL) {
            attr = H5FL_FREE(H5VL_iod_attr_t, attr);
        } /* end if */
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_attribute_open() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_attribute_read
 *
 * Purpose:	Reads raw data from a attribute into a buffer.
 *
 * Return:	Success:	0
 *		Failure:	-1, data not read.
 *
 * Programmer:  Mohamad Chaarawi
 *              October, 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_iod_attribute_read(void *_attr, hid_t type_id, void *buf, hid_t dxpl_id, void **req)
{
    H5VL_iod_attr_t *attr = (H5VL_iod_attr_t *)_attr;
    attr_io_in_t input;
    hg_bulk_t *bulk_handle = NULL;
    H5VL_iod_read_status_t *status = NULL;
    size_t size;
    H5VL_iod_io_info_t *info = NULL;
    hid_t rcxt_id;
    H5RC_t *rc = NULL;
    size_t num_parents = 0;
    H5P_genplist_t *plist = NULL;
    H5VL_iod_request_t **parent_reqs = NULL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* get the context ID */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");
    if(H5P_get(plist, H5VL_CONTEXT_ID, &rcxt_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get property value for trans_id");

    /* get the RC object */
    if(NULL == (rc = (H5RC_t *)H5I_object_verify(rcxt_id, H5I_RC)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a READ CONTEXT ID")

    if(-1 == attr->remote_attr.space_id) {
        HDassert(attr->common.request);
        /* Synchronously wait on the request attached to the attribute */
        if(H5VL_iod_request_wait(attr->common.file, attr->common.request) < 0)
            HGOTO_ERROR(H5E_ATTR,  H5E_CANTGET, FAIL, "can't wait on HG request");
        attr->common.request = NULL;
    }

    /* MSC - VLEN datatypes for attributes are not supported for now. */
    {
        H5T_class_t dt_class;
        H5T_t *dt = NULL;

        if(NULL == (dt = (H5T_t *)H5I_object_verify(type_id, H5I_DATATYPE)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, H5T_NO_CLASS, "not a datatype")

        dt_class = H5T_get_class(dt, FALSE);
        if(H5T_VLEN == dt_class || (H5T_STRING == dt_class && H5T_is_variable_str(dt)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "VLEN datatypes for attributes not supported");
    }

    /* calculate the size of the buffer needed */
    {
        hsize_t nelmts;
        size_t elmt_size;

        nelmts = (hsize_t)H5Sget_simple_extent_npoints(attr->remote_attr.space_id);
        elmt_size = H5Tget_size(type_id);

        size = elmt_size * nelmts;
    }

    /* allocate a bulk data transfer handle */
    if(NULL == (bulk_handle = (hg_bulk_t *)H5MM_malloc(sizeof(hg_bulk_t))))
	HGOTO_ERROR(H5E_ATTR, H5E_NOSPACE, FAIL, "can't allocate a buld data transfer handle");

    /* Register memory with bulk_handle */
    if(HG_SUCCESS != HG_Bulk_handle_create(buf, size, HG_BULK_READWRITE, bulk_handle))
        HGOTO_ERROR(H5E_ATTR, H5E_READERROR, FAIL, "can't create Bulk Data Handle");

    if(NULL == (parent_reqs = (H5VL_iod_request_t **)
                H5MM_malloc(sizeof(H5VL_iod_request_t *))))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate parent req element");

    /* retrieve parent requests */
    if(H5VL_iod_get_parent_requests((H5VL_iod_object_t *)attr, (H5VL_iod_req_info_t *)rc, 
                                    parent_reqs, &num_parents) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Failed to retrieve parent requests");

    /* Fill input structure */
    input.coh = attr->common.file->remote_file.coh;
    input.iod_oh = attr->remote_attr.iod_oh;
    input.iod_id = attr->remote_attr.iod_id;
    input.mdkv_id = attr->remote_attr.mdkv_id;
    input.bulk_handle = *bulk_handle;
    input.type_id = type_id;
    input.space_id = attr->remote_attr.space_id;
    input.rcxt_num  = rc->c_version;
    input.cs_scope = attr->common.file->md_integrity_scope;
    input.trans_num = 0;

    /* allocate structure to receive status of read operation (contains return value and checksum */
    status = (H5VL_iod_read_status_t *)malloc(sizeof(H5VL_iod_read_status_t));

    /* setup info struct for I/O request. 
       This is to manage the I/O operation once the wait is called. */
    if(NULL == (info = (H5VL_iod_io_info_t *)H5MM_malloc(sizeof(H5VL_iod_io_info_t))))
	HGOTO_ERROR(H5E_ATTR, H5E_NOSPACE, FAIL, "can't allocate a request");
    info->status = status;
    info->bulk_handle = bulk_handle;

#if H5VL_IOD_DEBUG
    printf("Attribute Read IOD ID %"PRIu64", axe id %"PRIu64"\n", 
           input.iod_id, g_axe_id);
#endif

    if(H5VL__iod_create_and_forward(H5VL_ATTR_READ_ID, HG_ATTR_READ, 
                                    (H5VL_iod_object_t *)attr, 0,
                                    num_parents, parent_reqs, 
                                    (H5VL_iod_req_info_t *)rc, &input, status, info, req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to create and ship attribute read");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_attribute_read() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_attribute_write
 *
 * Purpose:	Writes raw data from a buffer into a attribute.
 *
 * Return:	Success:	0
 *		Failure:	-1, attribute not writed.
 *
 * Programmer:  Mohamad Chaarawi
 *              October, 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_iod_attribute_write(void *_attr, hid_t type_id, const void *buf, hid_t dxpl_id, void **req)
{
    H5VL_iod_attr_t *attr = (H5VL_iod_attr_t *)_attr;
    attr_io_in_t input;
    H5P_genplist_t *plist = NULL;
    hg_bulk_t *bulk_handle = NULL;
    int *status = NULL;
    size_t size;
    uint64_t internal_cs; /* internal checksum calculated in this function */
    H5VL_iod_io_info_t *info;
    size_t num_parents = 0;
    hid_t trans_id;
    H5TR_t *tr = NULL;
    H5VL_iod_request_t **parent_reqs = NULL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* get the transaction ID */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");
    if(H5P_get(plist, H5VL_TRANS_ID, &trans_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get property value for trans_id");

    /* get the TR object */
    if(NULL == (tr = (H5TR_t *)H5I_object_verify(trans_id, H5I_TR)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a Transaction ID")

    if(-1 == attr->remote_attr.space_id) {
        /* Synchronously wait on the request attached to the attribute */
        if(H5VL_iod_request_wait(attr->common.file, attr->common.request) < 0)
            HGOTO_ERROR(H5E_ATTR,  H5E_CANTGET, FAIL, "can't wait on HG request");
        attr->common.request = NULL;
    }

    /* MSC - VLEN datatypes for attributes are not supported for now. */
    {
        H5T_class_t dt_class;
        H5T_t *dt = NULL;

        if(NULL == (dt = (H5T_t *)H5I_object_verify(type_id, H5I_DATATYPE)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, H5T_NO_CLASS, "not a datatype")

        dt_class = H5T_get_class(dt, FALSE);
        if(H5T_VLEN == dt_class || (H5T_STRING == dt_class && H5T_is_variable_str(dt)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "VLEN datatypes for attributes not supported");
    }

    /* calculate the size of the buffer needed */
    {
        hsize_t nelmts;
        size_t elmt_size;

        nelmts = (hsize_t)H5Sget_simple_extent_npoints(attr->remote_attr.space_id);
        elmt_size = H5Tget_size(type_id);

        size = elmt_size * nelmts;
    }

    /* calculate a checksum for the data */
    internal_cs = H5_checksum_crc64(buf, size);

    /* allocate a bulk data transfer handle */
    if(NULL == (bulk_handle = (hg_bulk_t *)H5MM_malloc(sizeof(hg_bulk_t))))
	HGOTO_ERROR(H5E_ATTR, H5E_NOSPACE, FAIL, "can't allocate a bulk data transfer handle");

    /* Register memory */
    if(HG_SUCCESS != HG_Bulk_handle_create(buf, size, HG_BULK_READ_ONLY, bulk_handle))
        HGOTO_ERROR(H5E_ATTR, H5E_WRITEERROR, FAIL, "can't create Bulk Data Handle");

    if(NULL == (parent_reqs = (H5VL_iod_request_t **)
                H5MM_malloc(sizeof(H5VL_iod_request_t *) * 2)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate parent req element");

    /* retrieve parent requests */
    if(H5VL_iod_get_parent_requests((H5VL_iod_object_t *)attr, (H5VL_iod_req_info_t *)tr, 
                                    parent_reqs, &num_parents) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Failed to retrieve parent requests");

    /* Fill input structure */
    input.coh = attr->common.file->remote_file.coh;
    input.iod_oh = attr->remote_attr.iod_oh;
    input.iod_id = attr->remote_attr.iod_id;
    input.mdkv_id = attr->remote_attr.mdkv_id;
    input.bulk_handle = *bulk_handle;
    input.type_id = type_id;
    input.space_id = attr->remote_attr.space_id;
    input.trans_num = tr->trans_num;
    input.rcxt_num  = tr->c_version;
    input.checksum = internal_cs;
    input.cs_scope = attr->common.file->md_integrity_scope;

    status = (int *)malloc(sizeof(int));

    /* setup info struct for I/O request 
       This is to manage the I/O operation once the wait is called. */
    if(NULL == (info = (H5VL_iod_io_info_t *)H5MM_malloc(sizeof(H5VL_iod_io_info_t))))
	HGOTO_ERROR(H5E_ATTR, H5E_NOSPACE, FAIL, "can't allocate a request");
    info->status = status;
    info->bulk_handle = bulk_handle;

#if H5VL_IOD_DEBUG
    printf("Attribute Write IOD ID %"PRIu64", axe id %"PRIu64"\n", 
           input.iod_id, g_axe_id);
#endif

    if(H5VL__iod_create_and_forward(H5VL_ATTR_WRITE_ID, HG_ATTR_WRITE, 
                                    (H5VL_iod_object_t *)attr, 0,
                                    num_parents, parent_reqs,
                                    (H5VL_iod_req_info_t *)tr, &input, status, info, req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to create and ship attribute write");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_attribute_write() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_attribute_remove
 *
 * Purpose:	Set Extent of attribute
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:  Mohamad Chaarawi
 *              October, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t 
H5VL_iod_attribute_remove(void *_obj, H5VL_loc_params_t loc_params, const char *attr_name, 
                          hid_t dxpl_id, void **req)
{
    H5VL_iod_object_t *obj = (H5VL_iod_object_t *)_obj; /* location object to create the attribute */
    attr_op_in_t input;
    iod_obj_id_t iod_id, attrkv_id;
    iod_handles_t iod_oh;
    size_t num_parents = 0;
    hid_t trans_id;
    H5TR_t *tr = NULL;
    H5P_genplist_t *plist = NULL;
    H5VL_iod_request_t **parent_reqs = NULL;
    int *status = NULL;
    char *loc_name = NULL;
    herr_t ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* get the transaction ID */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");
    if(H5P_get(plist, H5VL_TRANS_ID, &trans_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get property value for trans_id");

    /* get the TR object */
    if(NULL == (tr = (H5TR_t *)H5I_object_verify(trans_id, H5I_TR)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a Transaction ID")

    /* allocate parent request array */
    if(NULL == (parent_reqs = (H5VL_iod_request_t **)
                H5MM_malloc(sizeof(H5VL_iod_request_t *) * 2)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate parent req element");

    /* retrieve parent requests */
    if(H5VL_iod_get_parent_requests(obj, (H5VL_iod_req_info_t *)tr, parent_reqs, &num_parents) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Failed to retrieve parent requests");

    /* retrieve IOD info of location object */
    if(H5VL_iod_get_loc_info(obj, &iod_id, &iod_oh, NULL, &attrkv_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Failed to resolve current location group info");

    /* MSC - If location object not opened yet, wait for it. */
    if(IOD_OBJ_INVALID == iod_id) {
        /* Synchronously wait on the request attached to the dataset */
        if(H5VL_iod_request_wait(obj->file, obj->request) < 0)
            HGOTO_ERROR(H5E_DATASET,  H5E_CANTGET, FAIL, "can't wait on HG request");
        obj->request = NULL;
        /* retrieve IOD info of location object */
        if(H5VL_iod_get_loc_info(obj, &iod_id, &iod_oh, NULL, &attrkv_id) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Failed to resolve current location group info");
    }

    if(H5VL_OBJECT_BY_SELF == loc_params.type)
        loc_name = strdup(".");
    else if(H5VL_OBJECT_BY_NAME == loc_params.type)
        loc_name = strdup(loc_params.loc_data.loc_by_name.name);

    /* set the input structure for the HG encode routine */
    input.coh = obj->file->remote_file.coh;
    input.loc_id = iod_id;
    input.loc_attrkv_id = attrkv_id;
    input.loc_oh = iod_oh;
    input.path = loc_name;
    input.attr_name = attr_name;
    input.trans_num = tr->trans_num;
    input.rcxt_num  = tr->c_version;
    input.cs_scope = obj->file->md_integrity_scope;

    status = (int *)malloc(sizeof(int));

#if H5VL_IOD_DEBUG
    printf("Attribute Remove loc %s name %s, axe id %"PRIu64"\n", 
           loc_name, attr_name, g_axe_id);
#endif

    if(H5VL__iod_create_and_forward(H5VL_ATTR_REMOVE_ID, HG_ATTR_REMOVE, 
                                    (H5VL_iod_object_t *)obj, 1, 
                                    num_parents, parent_reqs,
                                    (H5VL_iod_req_info_t *)tr, &input, status, status, req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to create and ship attribute remove");

done:
    if(loc_name) 
        HDfree(loc_name);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_attribute_remove() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_attribute_get
 *
 * Purpose:	Gets certain information about a attribute
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:  Mohamad Chaarawi
 *              March, 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_iod_attribute_get(void *_obj, H5VL_attr_get_t get_type, hid_t dxpl_id, 
                       void **req, va_list arguments)
{
    H5VL_iod_object_t *obj = (H5VL_iod_object_t *)_obj; /* location of operation */
    iod_obj_id_t iod_id;
    iod_handles_t iod_oh;
    H5P_genplist_t *plist = NULL;
    hid_t rcxt_id;
    H5RC_t *rc = NULL;
    size_t num_parents = 0;
    H5VL_iod_request_t **parent_reqs = NULL;
    char *loc_name = NULL;
    herr_t  ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* get the context ID */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");
    if(H5P_get(plist, H5VL_CONTEXT_ID, &rcxt_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get property value for trans_id");

    /* get the RC object */
    if(NULL == (rc = (H5RC_t *)H5I_object_verify(rcxt_id, H5I_RC)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a READ CONTEXT ID")

    switch (get_type) {
        /* H5Aget_space */
        case H5VL_ATTR_GET_SPACE:
            {
                hid_t	*ret_id = va_arg (arguments, hid_t *);
                H5VL_iod_attr_t *attr = (H5VL_iod_attr_t *)obj;

                if(-1 == attr->remote_attr.space_id) {
                    /* Synchronously wait on the request attached to the attribute */
                    if(H5VL_iod_request_wait(attr->common.file, attr->common.request) < 0)
                        HGOTO_ERROR(H5E_ATTR,  H5E_CANTGET, FAIL, "can't wait on HG request");
                    attr->common.request = NULL;
                }

                if((*ret_id = H5Scopy(attr->remote_attr.space_id)) < 0)
                    HGOTO_ERROR(H5E_ARGS, H5E_CANTGET, FAIL, "can't get dataspace ID of attribute")
                break;
            }
        /* H5Aget_type */
        case H5VL_ATTR_GET_TYPE:
            {
                hid_t	*ret_id = va_arg (arguments, hid_t *);
                H5VL_iod_attr_t *attr = (H5VL_iod_attr_t *)obj;

                if(-1 == attr->remote_attr.type_id) {
                    /* Synchronously wait on the request attached to the attribute */
                    if(H5VL_iod_request_wait(attr->common.file, attr->common.request) < 0)
                        HGOTO_ERROR(H5E_ATTR,  H5E_CANTGET, FAIL, "can't wait on HG request");
                    attr->common.request = NULL;
                }

                if((*ret_id = H5Tcopy(attr->remote_attr.type_id)) < 0)
                    HGOTO_ERROR(H5E_ARGS, H5E_CANTGET, FAIL, "can't get datatype ID of attribute")
                break;
            }
        /* H5Aget_create_plist */
        case H5VL_ATTR_GET_ACPL:
            {
                hid_t	*ret_id = va_arg (arguments, hid_t *);
                H5VL_iod_attr_t *attr = (H5VL_iod_attr_t *)obj;

                /* Retrieve the file's access property list */
                if((*ret_id = H5Pcopy(H5P_ATTRIBUTE_CREATE_DEFAULT)) < 0)
                    HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get attr creation property list");
                break;
            }
        /* H5Aget_name */
        case H5VL_ATTR_GET_NAME:
            {
                H5VL_loc_params_t loc_params = va_arg (arguments, H5VL_loc_params_t);
                size_t	buf_size = va_arg (arguments, size_t);
                char    *buf = va_arg (arguments, char *);
                ssize_t	*ret_val = va_arg (arguments, ssize_t *);
                H5VL_iod_attr_t *attr = (H5VL_iod_attr_t *)obj;

                if(H5VL_OBJECT_BY_SELF == loc_params.type) {
                    size_t copy_len, nbytes;

                    nbytes = HDstrlen(attr->common.obj_name);
                    HDassert((ssize_t)nbytes >= 0); /*overflow, pretty unlikely --rpm*/

                    /* compute the string length which will fit into the user's buffer */
                    copy_len = MIN(buf_size - 1, nbytes);

                    /* Copy all/some of the name */
                    if(buf && copy_len > 0) {
                        HDmemcpy(buf, attr->common.obj_name, copy_len);

                        /* Terminate the string */
                        buf[copy_len]='\0';
                    } /* end if */
                    *ret_val = (ssize_t)nbytes;
                }
                else if(H5VL_OBJECT_BY_IDX == loc_params.type) {
                    HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't get name of attr");
                }
                break;
            }
        /* H5Aexists/exists_by_name */
        case H5VL_ATTR_EXISTS:
            {
                H5VL_loc_params_t loc_params = va_arg (arguments, H5VL_loc_params_t);
                char *attr_name = va_arg (arguments, char *);
                htri_t *ret = va_arg (arguments, htri_t *);
                attr_op_in_t input;

                /* allocate parent request array */
                if(NULL == (parent_reqs = (H5VL_iod_request_t **)
                            H5MM_malloc(sizeof(H5VL_iod_request_t *))))
                    HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate parent req element");

                /* retrieve parent requests */
                if(H5VL_iod_get_parent_requests(obj, (H5VL_iod_req_info_t *)rc, parent_reqs, &num_parents) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Failed to retrieve parent requests");

                /* retrieve IOD info of location object */
                if(H5VL_iod_get_loc_info(obj, &iod_id, &iod_oh, NULL, &input.loc_attrkv_id) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Failed to resolve current location group info");

                /* MSC - If location object not opened yet, wait for it. */
                if(IOD_OBJ_INVALID == iod_id) {
                    /* Synchronously wait on the request attached to the dataset */
                    if(H5VL_iod_request_wait(obj->file, obj->request) < 0)
                        HGOTO_ERROR(H5E_DATASET,  H5E_CANTGET, FAIL, "can't wait on HG request");
                    obj->request = NULL;
                    /* retrieve IOD info of location object */
                    if(H5VL_iod_get_loc_info(obj, &iod_id, &iod_oh, NULL, &input.loc_attrkv_id) < 0)
                        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Failed to resolve current location group info");
                }

                if(H5VL_OBJECT_BY_SELF == loc_params.type)
                    loc_name = strdup(".");
                else if(H5VL_OBJECT_BY_NAME == loc_params.type)
                    loc_name = strdup(loc_params.loc_data.loc_by_name.name);

                /* set the input structure for the HG encode routine */
                input.coh = obj->file->remote_file.coh;
                input.loc_id = iod_id;
                input.loc_oh = iod_oh;
                input.attr_name = attr_name;
                input.path = loc_name;
                input.rcxt_num  = rc->c_version;
                input.cs_scope = obj->file->md_integrity_scope;
                input.trans_num  = 0;

#if H5VL_IOD_DEBUG
                printf("Attribute Exists loc %s name %s, axe id %"PRIu64"\n", 
                       loc_name, attr_name, g_axe_id);
#endif

                if(H5VL__iod_create_and_forward(H5VL_ATTR_EXISTS_ID, HG_ATTR_EXISTS, 
                                                obj, 1, num_parents, parent_reqs,
                                                (H5VL_iod_req_info_t *)rc, &input, 
                                                ret, ret, req) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to create and ship attribute exists");

                if(loc_name) 
                    HDfree(loc_name);
                break;
            }
        /* H5Aget_info */
        case H5VL_ATTR_GET_INFO:
            {
#if 0
                H5VL_loc_params_t loc_params = va_arg (arguments, H5VL_loc_params_t);
                H5A_info_t *ainfo = va_arg (arguments, H5A_info_t *);

                if(H5VL_OBJECT_BY_SELF == loc_params.type) {
                    HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't get attr info")
                }
                else if(H5VL_OBJECT_BY_NAME == loc_params.type) {
                    char *attr_name = va_arg (arguments, char *);
                    HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't get attr info")
                }
                else if(H5VL_OBJECT_BY_IDX == loc_params.type) {
                    HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't get attr info")
                }
                else
                    HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't get attr info")
#endif
                break;
            }
        case H5VL_ATTR_GET_STORAGE_SIZE:
            {
                //hsize_t *ret = va_arg (arguments, hsize_t *);
                HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't get attr storage size");
                break;
            }
        default:
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't get this type of information from attr")
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_attribute_get() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_attribute_close
 *
 * Purpose:	Closes a attribute.
 *
 * Return:	Success:	0
 *		Failure:	-1, attribute not closed.
 *
 * Programmer:  Mohamad Chaarawi
 *              March, 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_iod_attribute_close(void *_attr, hid_t UNUSED dxpl_id, void **req)
{
    H5VL_iod_attr_t *attr = (H5VL_iod_attr_t *)_attr;
    attr_close_in_t input;
    int *status = NULL;
    size_t num_parents = 0;
    H5VL_iod_request_t **parent_reqs = NULL;
    herr_t ret_value = SUCCEED;  /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    if(-1 == attr->remote_attr.type_id ||
       -1 == attr->remote_attr.space_id ||
       IOD_OH_UNDEFINED == attr->remote_attr.iod_oh.rd_oh.cookie) {
        /* Synchronously wait on the request attached to the dataset */
        if(H5VL_iod_request_wait(attr->common.file, attr->common.request) < 0)
            HGOTO_ERROR(H5E_ATTR,  H5E_CANTGET, FAIL, "can't wait on HG request");
        attr->common.request = NULL;
    }

    /* If this call is not asynchronous, complete and remove all
       requests that are associated with this object from the List */
    if(NULL == req) {
        if(H5VL_iod_request_wait_some(attr->common.file, attr) < 0)
            HGOTO_ERROR(H5E_FILE,  H5E_CANTGET, FAIL, "can't wait on all object requests");
    }

    if(H5VL_iod_get_obj_requests((H5VL_iod_object_t *)attr, &num_parents, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't get num requests");

    if(num_parents) {
        if(NULL == (parent_reqs = (H5VL_iod_request_t **)H5MM_malloc
                    (sizeof(H5VL_iod_request_t *) * num_parents)))
            HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate array of parent reqs");

        if(H5VL_iod_get_obj_requests((H5VL_iod_object_t *)attr, &num_parents, 
                                     parent_reqs) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't get parent requests");
    }

    status = (int *)malloc(sizeof(int));

    input.iod_oh = attr->remote_attr.iod_oh;
    input.iod_id = attr->remote_attr.iod_id;

#if H5VL_IOD_DEBUG
    printf("Attribute Close IOD ID %"PRIu64", axe id %"PRIu64"\n", input.iod_id, g_axe_id);
#endif

    if(H5VL__iod_create_and_forward(H5VL_ATTR_CLOSE_ID, HG_ATTR_CLOSE, 
                                    (H5VL_iod_object_t *)attr, 1,
                                    num_parents, parent_reqs,
                                    NULL, &input, status, status, req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to create and ship attribute close");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_attribute_close() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_link_create
 *
 * Purpose:	Creates an hard/soft/UD/external links.
 *              For now, only Hard and Soft Links are Supported.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              May, 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_iod_link_create(H5VL_link_create_type_t create_type, void *_obj, H5VL_loc_params_t loc_params,
                     hid_t lcpl_id, hid_t lapl_id, hid_t dxpl_id, void **req)
{
    H5VL_iod_object_t *obj = (H5VL_iod_object_t *)_obj; /* location object */
    link_create_in_t input;
    int *status = NULL;
    H5P_genplist_t *plist = NULL;                      /* Property list pointer */
    char *link_value = NULL; /* Value of soft link */
    size_t num_parents = 0;
    H5VL_iod_request_t **parent_reqs = NULL;
    hid_t trans_id;
    H5TR_t *tr = NULL;
    char *loc_name = NULL, *target_name = NULL;
    herr_t ret_value = SUCCEED;        /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* get the transaction ID */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");
    if(H5P_get(plist, H5VL_TRANS_ID, &trans_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get property value for trans_id");

    /* get the TR object */
    if(NULL == (tr = (H5TR_t *)H5I_object_verify(trans_id, H5I_TR)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a Transaction ID")

    /* Get the plist structure */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(lcpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");

    /* allocate parent request array */
    if(NULL == (parent_reqs = (H5VL_iod_request_t **)
                H5MM_malloc(sizeof(H5VL_iod_request_t *) * 3)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate parent req element");

    status = (int *)malloc(sizeof(int));

    switch (create_type) {
        case H5VL_LINK_CREATE_HARD:
            {
                H5VL_iod_object_t *target_obj = NULL;
                H5VL_loc_params_t target_params;

                if(H5P_get(plist, H5VL_LINK_TARGET, &target_obj) < 0)
                    HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get property value for current location");
                if(H5P_get(plist, H5VL_LINK_TARGET_LOC_PARAMS, &target_params) < 0)
                    HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get property value for current name");

                /* object is H5L_SAME_LOC */
                if(NULL == obj && target_obj) {
                    obj = target_obj;
                }

                /* retrieve parent requests */
                if(H5VL_iod_get_parent_requests(obj, (H5VL_iod_req_info_t *)tr, parent_reqs, &num_parents) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Failed to retrieve parent requests");

                /* retrieve IOD info of location object */
                if(H5VL_iod_get_loc_info(obj, &input.loc_id, &input.loc_oh, NULL, NULL) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Failed to resolve current location group info");

                /* MSC - If location object not opened yet, wait for it. */
                if(IOD_OBJ_INVALID == input.loc_id) {
                    /* Synchronously wait on the request attached to the dataset */
                    if(H5VL_iod_request_wait(obj->file, obj->request) < 0)
                        HGOTO_ERROR(H5E_DATASET,  H5E_CANTGET, FAIL, "can't wait on HG request");
                    obj->request = NULL;
                    /* retrieve IOD info of location object */
                    if(H5VL_iod_get_loc_info(obj, &input.loc_id, &input.loc_oh, NULL, NULL) < 0)
                        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Failed to resolve current location group info");
                }

                if(H5VL_OBJECT_BY_SELF == loc_params.type)
                    loc_name = strdup(".");
                else if(H5VL_OBJECT_BY_NAME == loc_params.type)
                    loc_name = strdup(loc_params.loc_data.loc_by_name.name);

                /* Retrieve the parent info by traversing the path where the
                   link should be created. */

                /* object is H5L_SAME_LOC */
                if(NULL == target_obj && obj) {
                    target_obj = obj;
                }

                /* retrieve parent requests */
                if(H5VL_iod_get_parent_requests(target_obj, NULL, 
                                                &parent_reqs[num_parents], &num_parents) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Failed to retrieve parent requests");

                /* retrieve IOD info of location object */
                if(H5VL_iod_get_loc_info(target_obj, &input.target_loc_id, 
                                         &input.target_loc_oh, &input.target_mdkv_id, NULL) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Failed to resolve current location group info");

                /* MSC - If location object not opened yet, wait for it. */
                if(IOD_OBJ_INVALID == input.target_loc_id) {
                    /* Synchronously wait on the request attached to the dataset */
                    if(H5VL_iod_request_wait(target_obj->file, target_obj->request) < 0)
                        HGOTO_ERROR(H5E_DATASET,  H5E_CANTGET, FAIL, "can't wait on HG request");
                    target_obj->request = NULL;
                    /* retrieve IOD info of location object */
                    if(H5VL_iod_get_loc_info(target_obj, &input.target_loc_id, 
                                             &input.target_loc_oh, NULL, NULL) < 0)
                        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Failed to resolve current location group info");
                }

                if(H5VL_OBJECT_BY_SELF == target_params.type)
                    target_name = strdup(".");
                else if(H5VL_OBJECT_BY_NAME == target_params.type)
                    target_name = strdup(target_params.loc_data.loc_by_name.name);

                /* set the input structure for the HG encode routine */
                input.create_type = H5VL_LINK_CREATE_HARD;
                if(obj)
                    input.coh = obj->file->remote_file.coh;
                else
                    input.coh = target_obj->file->remote_file.coh;

                input.target_name = target_name;
                input.loc_name = loc_name;
                input.lcpl_id = lcpl_id;
                input.lapl_id = lapl_id;
                link_value = strdup("\0");
                input.link_value = link_value;

#if H5VL_IOD_DEBUG
                printf("Link Create Hard axe id %"PRIu64"\n", g_axe_id);
#endif
                break;
            }
        case H5VL_LINK_CREATE_SOFT:
            {
                H5VL_iod_object_t *target_obj = NULL;
                H5VL_loc_params_t target_params;

                if(H5P_get(plist, H5VL_LINK_TARGET_NAME, &target_name) < 0)
                    HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get property value for targe name");

                target_params.type = H5VL_OBJECT_BY_NAME;
                target_params.loc_data.loc_by_name.name = target_name;
                target_params.loc_data.loc_by_name.plist_id = lapl_id;

                if('/' == *target_name) {
                    /* The target location object is the file root */
                    target_obj = (H5VL_iod_object_t *)obj->file;
                }
                else {
                    target_obj = obj;
                }

                link_value = strdup(target_name);

                /* Retrieve the parent info by traversing the path where the
                   link should be created from. */

                /* retrieve parent requests */
                if(H5VL_iod_get_parent_requests(obj, (H5VL_iod_req_info_t *)tr, parent_reqs, &num_parents) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Failed to retrieve parent requests");

                /* retrieve IOD info of location object */
                if(H5VL_iod_get_loc_info(obj, &input.loc_id, &input.loc_oh, NULL, NULL) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Failed to resolve current location group info");

                /* MSC - If location object not opened yet, wait for it. */
                if(IOD_OBJ_INVALID == input.loc_id) {
                    /* Synchronously wait on the request attached to the dataset */
                    if(H5VL_iod_request_wait(obj->file, obj->request) < 0)
                        HGOTO_ERROR(H5E_DATASET,  H5E_CANTGET, FAIL, "can't wait on HG request");
                    obj->request = NULL;
                    /* retrieve IOD info of location object */
                    if(H5VL_iod_get_loc_info(obj, &input.loc_id, &input.loc_oh, NULL, NULL) < 0)
                        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Failed to resolve current location group info");
                }

                if(H5VL_OBJECT_BY_SELF == loc_params.type)
                    loc_name = strdup(".");
                else if(H5VL_OBJECT_BY_NAME == loc_params.type)
                    loc_name = strdup(loc_params.loc_data.loc_by_name.name);

                /* retrieve parent requests */
                if(H5VL_iod_get_parent_requests(target_obj, NULL, 
                                                &parent_reqs[num_parents], &num_parents) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Failed to retrieve parent requests");

                /* retrieve IOD info of location object */
                if(H5VL_iod_get_loc_info(target_obj, &input.target_loc_id, 
                                         &input.target_loc_oh, &input.target_mdkv_id, NULL) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Failed to resolve current location group info");

                /* MSC - If location object not opened yet, wait for it. */
                if(IOD_OBJ_INVALID == input.target_loc_id) {
                    /* Synchronously wait on the request attached to the dataset */
                    if(H5VL_iod_request_wait(target_obj->file, target_obj->request) < 0)
                        HGOTO_ERROR(H5E_DATASET,  H5E_CANTGET, FAIL, "can't wait on HG request");
                    target_obj->request = NULL;
                    /* retrieve IOD info of location object */
                    if(H5VL_iod_get_loc_info(target_obj, &input.target_loc_id, 
                                             &input.target_loc_oh, NULL, NULL) < 0)
                        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Failed to resolve current location group info");
                }

                /* set the input structure for the HG encode routine */
                input.create_type = H5VL_LINK_CREATE_SOFT;
                input.coh = obj->file->remote_file.coh;
                input.loc_name = loc_name;
                input.target_name = target_name;
                input.lcpl_id = lcpl_id;
                input.lapl_id = lapl_id;
                input.link_value = link_value;

#if H5VL_IOD_DEBUG
                printf("Link Create Soft axe id %"PRIu64"\n", g_axe_id);
#endif
                break;
            }
        /* MSC - not supported now */
        case H5VL_LINK_CREATE_UD:
            {
                H5L_type_t link_type;
                void *udata;
                size_t udata_size;

                if(H5P_get(plist, H5VL_LINK_TYPE, &link_type) < 0)
                    HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get property value for link type");
                if(H5P_get(plist, H5VL_LINK_UDATA, &udata) < 0)
                    HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get property value for udata");
                if(H5P_get(plist, H5VL_LINK_UDATA_SIZE, &udata_size) < 0)
                    HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get property value for udata size");
            }
        default:
            HGOTO_ERROR(H5E_LINK, H5E_CANTINIT, FAIL, "invalid link creation call")
    }

    input.trans_num = tr->trans_num;
    input.rcxt_num  = tr->c_version;
    input.cs_scope = obj->file->md_integrity_scope;

    if(H5VL__iod_create_and_forward(H5VL_LINK_CREATE_ID, HG_LINK_CREATE, 
                                    obj, 1, num_parents, parent_reqs,
                                    (H5VL_iod_req_info_t *)tr, &input, status, status, req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to create and ship link create");

done:

    if(link_value) 
        HDfree(link_value);
    if(loc_name) 
        HDfree(loc_name);
    if(H5VL_LINK_CREATE_HARD == create_type && target_name) 
        HDfree(target_name);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_link_create() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_link_move
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
 * Programmer:  Mohamad Chaarawi
 *              May, 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_iod_link_move(void *_src_obj, H5VL_loc_params_t loc_params1, 
                   void *_dst_obj, H5VL_loc_params_t loc_params2,
                   hbool_t copy_flag, hid_t lcpl_id, hid_t lapl_id, 
                   hid_t dxpl_id, void **req)
{
    H5VL_iod_object_t *src_obj = (H5VL_iod_object_t *)_src_obj;
    H5VL_iod_object_t *dst_obj = (H5VL_iod_object_t *)_dst_obj;
    link_move_in_t input;
    int *status = NULL;
    H5VL_iod_request_t **parent_reqs = NULL;
    H5P_genplist_t *plist = NULL;
    size_t num_parents = 0;
    hid_t trans_id;
    H5TR_t *tr = NULL;
    char *src_name = NULL, *dst_name = NULL;
    herr_t ret_value = SUCCEED;        /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* get the transaction ID */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");
    if(H5P_get(plist, H5VL_TRANS_ID, &trans_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get property value for trans_id");

    /* get the TR object */
    if(NULL == (tr = (H5TR_t *)H5I_object_verify(trans_id, H5I_TR)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a Transaction ID")

    /* allocate parent request array */
    if(NULL == (parent_reqs = (H5VL_iod_request_t **)
                H5MM_malloc(sizeof(H5VL_iod_request_t *) * 3)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate parent req element");

    /* retrieve parent requests */
    if(H5VL_iod_get_parent_requests(src_obj, (H5VL_iod_req_info_t *)tr, parent_reqs, &num_parents) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Failed to retrieve parent requests");

    /* retrieve IOD info of location object */
    if(H5VL_iod_get_loc_info(src_obj, &input.src_loc_id, &input.src_loc_oh, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Failed to resolve current location group info");

    /* MSC - If location object not opened yet, wait for it. */
    if(IOD_OBJ_INVALID == input.src_loc_id) {
        /* Synchronously wait on the request attached to the dataset */
        if(H5VL_iod_request_wait(src_obj->file, src_obj->request) < 0)
            HGOTO_ERROR(H5E_DATASET,  H5E_CANTGET, FAIL, "can't wait on HG request");
        src_obj->request = NULL;
        /* retrieve IOD info of location object */
        if(H5VL_iod_get_loc_info(src_obj, &input.src_loc_id, 
                                 &input.src_loc_oh, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Failed to resolve current location group info");
    }

    if(H5VL_OBJECT_BY_SELF == loc_params1.type)
        src_name = strdup(".");
    else if(H5VL_OBJECT_BY_NAME == loc_params1.type)
        src_name = strdup(loc_params1.loc_data.loc_by_name.name);

    /* retrieve parent requests */
    if(H5VL_iod_get_parent_requests(dst_obj, NULL, parent_reqs, &num_parents) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Failed to retrieve parent requests");

    /* retrieve IOD info of location object */
    if(H5VL_iod_get_loc_info(dst_obj, &input.dst_loc_id, &input.dst_loc_oh, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Failed to resolve current location group info");

    /* MSC - If location object not opened yet, wait for it. */
    if(IOD_OBJ_INVALID == input.dst_loc_id) {
        /* Synchronously wait on the request attached to the dataset */
        if(H5VL_iod_request_wait(dst_obj->file, dst_obj->request) < 0)
            HGOTO_ERROR(H5E_DATASET,  H5E_CANTGET, FAIL, "can't wait on HG request");
        dst_obj->request = NULL;
        /* retrieve IOD info of location object */
        if(H5VL_iod_get_loc_info(dst_obj, &input.dst_loc_id, 
                                 &input.dst_loc_oh, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Failed to resolve current location group info");
    }

    if(H5VL_OBJECT_BY_SELF == loc_params2.type)
        dst_name = strdup(".");
    else if(H5VL_OBJECT_BY_NAME == loc_params2.type)
        dst_name = strdup(loc_params2.loc_data.loc_by_name.name);

    /* if the object, to be moved is open locally, then update its
       link information */
    if(!copy_flag && 0 == strcmp(src_name, ".")) {
        char *link_name = NULL;

        /* generate the entire path of the new link */
        {
            size_t obj_name_len = HDstrlen(dst_obj->obj_name);
            size_t name_len = HDstrlen(loc_params2.loc_data.loc_by_name.name);

            if (NULL == (link_name = (char *)HDmalloc(obj_name_len + name_len + 1)))
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate");
            HDmemcpy(link_name, dst_obj->obj_name, obj_name_len);
            HDmemcpy(link_name+obj_name_len, loc_params2.loc_data.loc_by_name.name, name_len);
            link_name[obj_name_len+name_len] = '\0';
        }
        //H5VL_iod_update_link(dst_obj, loc_params2, link_name);
        free(link_name);
    }

    /* set the input structure for the HG encode routine */
    input.coh = src_obj->file->remote_file.coh;
    input.copy_flag = copy_flag;
    input.src_loc_name = src_name;
    input.dst_loc_name = dst_name;
    input.lcpl_id = lcpl_id;
    input.lapl_id = lapl_id;
    input.trans_num = tr->trans_num;
    input.rcxt_num  = tr->c_version;
    input.cs_scope = src_obj->file->md_integrity_scope;

    status = (herr_t *)malloc(sizeof(herr_t));

    if(H5VL__iod_create_and_forward(H5VL_LINK_MOVE_ID, HG_LINK_MOVE, dst_obj, 1,
                                    num_parents, parent_reqs,
                                    (H5VL_iod_req_info_t *)tr, &input, 
                                    status, status, req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to create and ship link move");

done:
    if(src_name) 
        free(src_name);
    if(dst_name) 
        free(dst_name);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_link_move() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_link_iterate
 *
 * Purpose:	Iterates through links in a group
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:  Mohamad Chaarawi
 *              May, 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t H5VL_iod_link_iterate(void UNUSED *_obj, H5VL_loc_params_t UNUSED loc_params, 
                                    hbool_t UNUSED recursive, 
                                    H5_index_t UNUSED idx_type, H5_iter_order_t UNUSED order, 
                                    hsize_t UNUSED *idx, 
                                    H5L_iterate_t UNUSED op, void UNUSED *op_data, 
                                    hid_t UNUSED dxpl_id, void UNUSED **req)
{
    //H5VL_iod_object_t *obj = (H5VL_iod_object_t *)_obj;
    //herr_t ret_value = SUCCEED;  /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5VL_iod_link_iterate() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_link_get
 *
 * Purpose:	Gets certain data about a link
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:  Mohamad Chaarawi
 *              May, 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_iod_link_get(void *_obj, H5VL_loc_params_t loc_params, H5VL_link_get_t get_type, 
                  hid_t dxpl_id, void **req, va_list arguments)
{
    H5VL_iod_object_t *obj = (H5VL_iod_object_t *)_obj;
    H5VL_iod_request_t **parent_reqs = NULL;
    size_t num_parents = 0;
    hid_t rcxt_id;
    H5RC_t *rc = NULL;
    H5P_genplist_t *plist = NULL;
    iod_obj_id_t iod_id;
    iod_handles_t iod_oh;
    char *loc_name = NULL;
    herr_t ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* get the context ID */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");
    if(H5P_get(plist, H5VL_CONTEXT_ID, &rcxt_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get property value for trans_id");

    /* get the RC object */
    if(NULL == (rc = (H5RC_t *)H5I_object_verify(rcxt_id, H5I_RC)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a READ CONTEXT ID");

    /* allocate parent request array */
    if(NULL == (parent_reqs = (H5VL_iod_request_t **)
                H5MM_malloc(sizeof(H5VL_iod_request_t *))))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate parent req element");

    /* retrieve parent requests */
    if(H5VL_iod_get_parent_requests(obj, (H5VL_iod_req_info_t *)rc, parent_reqs, &num_parents) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Failed to retrieve parent requests");

    /* retrieve IOD info of location object */
    if(H5VL_iod_get_loc_info(obj, &iod_id, &iod_oh, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Failed to resolve current location group info");

    /* MSC - If location object not opened yet, wait for it. */
    if(IOD_OBJ_INVALID == iod_id) {
        /* Synchronously wait on the request attached to the dataset */
        if(H5VL_iod_request_wait(obj->file, obj->request) < 0)
            HGOTO_ERROR(H5E_DATASET,  H5E_CANTGET, FAIL, "can't wait on HG request");
        obj->request = NULL;
        /* retrieve IOD info of location object */
        if(H5VL_iod_get_loc_info(obj, &iod_id, &iod_oh, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Failed to resolve current location group info");
    }

    switch (get_type) {
        /* H5Lexists */
        case H5VL_LINK_EXISTS:
            {
                link_op_in_t input;
                htri_t *ret    = va_arg (arguments, htri_t *);

                if(H5VL_OBJECT_BY_SELF == loc_params.type)
                    loc_name = strdup(".");
                else if(H5VL_OBJECT_BY_NAME == loc_params.type)
                    loc_name = strdup(loc_params.loc_data.loc_by_name.name);

                /* set the input structure for the HG encode routine */
                input.coh = obj->file->remote_file.coh;
                input.loc_id = iod_id;
                input.loc_oh = iod_oh;
                input.rcxt_num  = rc->c_version;
                input.cs_scope = obj->file->md_integrity_scope;
                input.trans_num  = 0;
                input.path = loc_name;

#if H5VL_IOD_DEBUG
                printf("Link Exists axe %"PRIu64": %s ID %"PRIu64"\n", 
                       g_axe_id, loc_name, input.loc_id);
#endif

                if(H5VL__iod_create_and_forward(H5VL_LINK_EXISTS_ID, HG_LINK_EXISTS, 
                                                obj, 0, num_parents, parent_reqs,
                                                (H5VL_iod_req_info_t *)rc, &input, ret, ret, req) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to create and ship link exists");

                if(loc_name) 
                    HDfree(loc_name);
                break;
            }
        /* H5Lget_info/H5Lget_info_by_idx */
        case H5VL_LINK_GET_INFO:
            {
                H5L_ff_info_t *linfo  = va_arg (arguments, H5L_ff_info_t *);
                link_op_in_t input;

                if(H5VL_OBJECT_BY_SELF == loc_params.type)
                    loc_name = strdup(".");
                else if(H5VL_OBJECT_BY_NAME == loc_params.type)
                    loc_name = strdup(loc_params.loc_data.loc_by_name.name);

                /* set the input structure for the HG encode routine */
                input.coh = obj->file->remote_file.coh;
                input.loc_id = iod_id;
                input.loc_oh = iod_oh;
                input.rcxt_num  = rc->c_version;
                input.cs_scope = obj->file->md_integrity_scope;
                input.trans_num  = 0;
                input.path = loc_name;

#if H5VL_IOD_DEBUG
                printf("Link get info axe %"PRIu64": %s ID %"PRIu64"\n", 
                       g_axe_id, loc_name, input.loc_id);
#endif

                if(H5VL__iod_create_and_forward(H5VL_LINK_GET_INFO_ID, HG_LINK_GET_INFO, 
                                                obj, 0, num_parents, parent_reqs,
                                                (H5VL_iod_req_info_t *)rc, &input, linfo, linfo, req) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to create and ship link get_info");

                if(loc_name) 
                    HDfree(loc_name);

                break;
            }
        /* H5Lget_val/H5Lget_val_by_idx */
        case H5VL_LINK_GET_VAL:
            {
                void       *buf    = va_arg (arguments, void *);
                size_t     size    = va_arg (arguments, size_t);
                link_get_val_in_t input;
                link_get_val_out_t *result;

                if(H5VL_OBJECT_BY_SELF == loc_params.type)
                    loc_name = strdup(".");
                else if(H5VL_OBJECT_BY_NAME == loc_params.type)
                    loc_name = strdup(loc_params.loc_data.loc_by_name.name);

                /* set the input structure for the HG encode routine */
                input.coh = obj->file->remote_file.coh;
                input.length = size;
                input.loc_id = iod_id;
                input.loc_oh = iod_oh;
                input.rcxt_num  = rc->c_version;
                input.cs_scope = obj->file->md_integrity_scope;
                input.path = loc_name;

                if(NULL == (result = (link_get_val_out_t *)malloc
                            (sizeof(link_get_val_out_t)))) {
                    HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate get val return struct");
                }

                result->value.val_size = input.length;
                result->value.val = buf;

#if H5VL_IOD_DEBUG
                printf("Link get val axe %"PRIu64": %s ID %"PRIu64"\n", 
                       g_axe_id, loc_name, input.loc_id);
#endif

                if(H5VL__iod_create_and_forward(H5VL_LINK_GET_VAL_ID, HG_LINK_GET_VAL, obj, 0,
                                                num_parents, parent_reqs,
                                                (H5VL_iod_req_info_t *)rc, &input, result, result, req) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to create and ship link get_val");

                if(loc_name) 
                    HDfree(loc_name);

                break;
            }
        /* H5Lget_name_by_idx */
        case H5VL_LINK_GET_NAME:
        default:
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't get this type of information from link")
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_link_get() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_link_remove
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
 * Programmer:  Mohamad Chaarawi
 *              May, 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t 
H5VL_iod_link_remove(void *_obj, H5VL_loc_params_t loc_params, hid_t dxpl_id, void **req)
{
    H5VL_iod_object_t *obj = (H5VL_iod_object_t *)_obj;
    link_op_in_t input;
    H5VL_iod_request_t **parent_reqs = NULL;
    size_t num_parents = 0;
    hid_t trans_id;
    H5TR_t *tr = NULL;
    H5P_genplist_t *plist = NULL;
    int *status = NULL;
    char *loc_name = NULL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* get the transaction ID */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");
    if(H5P_get(plist, H5VL_TRANS_ID, &trans_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get property value for trans_id");

    /* get the TR object */
    if(NULL == (tr = (H5TR_t *)H5I_object_verify(trans_id, H5I_TR)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a Transaction ID")

    /* allocate parent request array */
    if(NULL == (parent_reqs = (H5VL_iod_request_t **)
                H5MM_malloc(sizeof(H5VL_iod_request_t *) * 2)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate parent req element");

    /* retrieve parent requests */
    if(H5VL_iod_get_parent_requests(obj, (H5VL_iod_req_info_t *)tr, parent_reqs, &num_parents) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Failed to retrieve parent requests");

    /* retrieve IOD info of location object */
    if(H5VL_iod_get_loc_info(obj, &input.loc_id, &input.loc_oh, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Failed to resolve current location group info");

    /* MSC - If location object not opened yet, wait for it. */
    if(IOD_OBJ_INVALID == input.loc_id) {
        /* Synchronously wait on the request attached to the dataset */
        if(H5VL_iod_request_wait(obj->file, obj->request) < 0)
            HGOTO_ERROR(H5E_DATASET,  H5E_CANTGET, FAIL, "can't wait on HG request");
        obj->request = NULL;
        /* retrieve IOD info of location object */
        if(H5VL_iod_get_loc_info(obj, &input.loc_id, &input.loc_oh, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Failed to resolve current location group info");
    }

    if(H5VL_OBJECT_BY_SELF == loc_params.type)
        loc_name = strdup(".");
    else if(H5VL_OBJECT_BY_NAME == loc_params.type)
        loc_name = strdup(loc_params.loc_data.loc_by_name.name);

    /* set the input structure for the HG encode routine */
    input.coh = obj->file->remote_file.coh;
    input.path = loc_name;
    input.trans_num = tr->trans_num;
    input.rcxt_num  = tr->c_version;
    input.cs_scope = obj->file->md_integrity_scope;

#if H5VL_IOD_DEBUG
    printf("Link Remove axe %"PRIu64": %s ID %"PRIu64"\n", 
           g_axe_id, loc_name, input.loc_id);
#endif

    status = (int *)malloc(sizeof(int));

    if(H5VL__iod_create_and_forward(H5VL_LINK_REMOVE_ID, HG_LINK_REMOVE, obj, 1, 
                                    num_parents, parent_reqs,
                                    (H5VL_iod_req_info_t *)tr, &input, status, status, req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to create and ship link remove");

done:
    if(loc_name) 
        HDfree(loc_name);
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_link_remove() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_obj_open_token
 *
 * Purpose:	Opens a object inside IOD file using it IOD ID
 *
 * Return:	Success:	object id. 
 *		Failure:	NULL
 *
 * Programmer:  Mohamad Chaarawi
 *              September, 2013
 *
 *-------------------------------------------------------------------------
 */
void *
H5VL_iod_obj_open_token(const void *token, H5RC_t *rc, H5I_type_t *opened_type, void **req)
{
    object_token_in_t input;
    const uint8_t *buf_ptr = (const uint8_t *)token;
    H5O_type_t obj_type;
    iod_obj_id_t iod_id, mdkv_id, attrkv_id;
    H5VL_iod_dset_t *dset = NULL; /* the dataset object that is created and passed to the user */
    H5VL_iod_dtype_t *dtype = NULL; /* the datatype object that is created and passed to the user */
    H5VL_iod_group_t  *grp = NULL; /* the group object that is created and passed to the user */
    H5VL_iod_map_t  *map = NULL; /* the map object that is created and passed to the user */
    void *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    HDmemcpy(&iod_id, buf_ptr, sizeof(iod_obj_id_t));
    buf_ptr += sizeof(iod_obj_id_t);
    HDmemcpy(&mdkv_id, buf_ptr, sizeof(iod_obj_id_t));
    buf_ptr += sizeof(iod_obj_id_t);
    HDmemcpy(&attrkv_id, buf_ptr, sizeof(iod_obj_id_t));
    buf_ptr += sizeof(iod_obj_id_t);
    HDmemcpy(&obj_type, buf_ptr, sizeof(H5O_type_t));
    buf_ptr += sizeof(H5O_type_t);
    
    switch(obj_type) {
    case H5O_TYPE_DATASET:
        /* allocate the dataset object that is returned to the user */
        if(NULL == (dset = H5FL_CALLOC(H5VL_iod_dset_t)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate object struct");

        dset->remote_dset.iod_oh.rd_oh.cookie = IOD_OH_UNDEFINED;
        dset->remote_dset.iod_oh.wr_oh.cookie = IOD_OH_UNDEFINED;
        dset->remote_dset.iod_id = iod_id;
        dset->remote_dset.mdkv_id = mdkv_id;
        dset->remote_dset.attrkv_id = attrkv_id;

        dset->dapl_id = H5P_DATASET_ACCESS_DEFAULT;
        dset->remote_dset.dcpl_id = H5Pcopy(H5P_DATASET_CREATE_DEFAULT);

        /* decode dtype */
        {
            H5T_t *dt = NULL;
            size_t dt_size;

            HDmemcpy(&dt_size, buf_ptr, sizeof(size_t));
            buf_ptr += sizeof(size_t);
            /* Create datatype by decoding buffer */
            if(NULL == (dt = H5T_decode((const unsigned char *)buf_ptr)))
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTDECODE, NULL, "can't decode object");
            /* Register the type */
            if((dset->remote_dset.type_id = H5I_register(H5I_DATATYPE, dt, TRUE)) < 0)
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTREGISTER, NULL, "unable to register data type");
            buf_ptr += dt_size;
        }

        /* decode dspace */
        {
            H5S_t *ds;
            size_t space_size;

            HDmemcpy(&space_size, buf_ptr, sizeof(size_t));
            buf_ptr += sizeof(size_t);
            if((ds = H5S_decode((const unsigned char *)buf_ptr)) == NULL)
                HGOTO_ERROR(H5E_DATASPACE, H5E_CANTDECODE, NULL, "can't decode object");
            /* Register the type  */
            if((dset->remote_dset.space_id = H5I_register(H5I_DATASPACE, ds, TRUE)) < 0)
                HGOTO_ERROR(H5E_DATASPACE, H5E_CANTREGISTER, NULL, "unable to register dataspace");
            buf_ptr += space_size;
        }

        input.coh = rc->file->remote_file.coh;
        input.iod_id = iod_id;

        /* set common object parameters */
        dset->common.obj_type = H5I_DATASET;
        dset->common.file = rc->file;
        dset->common.file->nopen_objs ++;
        dset->common.obj_name = NULL;

#if H5VL_IOD_DEBUG
        printf("Dataset open by token %"PRIu64": ID %"PRIu64"\n", 
               g_axe_id, input.iod_id);
#endif

        if(H5VL__iod_create_and_forward(H5VL_OBJECT_OPEN_BY_TOKEN_ID, HG_OBJECT_OPEN_BY_TOKEN, 
                                        (H5VL_iod_object_t *)dset, 1, 0, NULL,
                                        (H5VL_iod_req_info_t *)rc, &input, 
                                        &dset->remote_dset.iod_oh, dset, req) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to create and ship dataset open_by_token");

        ret_value = (void *)dset;
        *opened_type = H5I_DATASET;
        break;
    case H5O_TYPE_NAMED_DATATYPE:
        /* allocate the datatype object that is returned to the user */
        if(NULL == (dtype = H5FL_CALLOC(H5VL_iod_dtype_t)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate object struct");

        dtype->remote_dtype.iod_oh.rd_oh.cookie = IOD_OH_UNDEFINED;
        dtype->remote_dtype.iod_oh.wr_oh.cookie = IOD_OH_UNDEFINED;
        dtype->remote_dtype.iod_id = iod_id;
        dtype->remote_dtype.mdkv_id = mdkv_id;
        dtype->remote_dtype.attrkv_id = attrkv_id;

        dtype->tapl_id = H5P_DATATYPE_ACCESS_DEFAULT;
        dtype->remote_dtype.tcpl_id = H5Pcopy(H5P_DATATYPE_CREATE_DEFAULT);

        /* decode dtype */
        {
            H5T_t *dt = NULL;
            size_t dt_size;

            HDmemcpy(&dt_size, buf_ptr, sizeof(size_t));
            buf_ptr += sizeof(size_t);
            /* Create datatype by decoding buffer */
            if(NULL == (dt = H5T_decode((const unsigned char *)buf_ptr)))
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTDECODE, NULL, "can't decode object");
            /* Register the type */
            if((dtype->remote_dtype.type_id = H5I_register(H5I_DATATYPE, dt, TRUE)) < 0)
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTREGISTER, NULL, "unable to register data type");
            buf_ptr += dt_size;
        }

        /* set the input structure for the HG encode routine */
        input.coh = rc->file->remote_file.coh;
        input.iod_id = iod_id;

        /* set common object parameters */
        dtype->common.obj_type = H5I_DATATYPE;
        dtype->common.file = rc->file;
        dtype->common.file->nopen_objs ++;
        dtype->common.obj_name = NULL;

#if H5VL_IOD_DEBUG
        printf("Named Datatype open by token %"PRIu64": ID %"PRIu64"\n", 
               g_axe_id, input.iod_id);
#endif

        if(H5VL__iod_create_and_forward(H5VL_OBJECT_OPEN_BY_TOKEN_ID, HG_OBJECT_OPEN_BY_TOKEN, 
                                        (H5VL_iod_object_t *)dtype, 1, 0, NULL,
                                        (H5VL_iod_req_info_t *)rc, &input, 
                                        &dtype->remote_dtype.iod_oh, dtype, req) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to create and ship datatype open_by_token");

        ret_value = (void *)dtype;
        *opened_type = H5I_DATATYPE;
        break;
    case H5O_TYPE_GROUP:
        /* allocate the dataset object that is returned to the user */
        if(NULL == (grp = H5FL_CALLOC(H5VL_iod_group_t)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate object struct");

        grp->remote_group.iod_oh.rd_oh.cookie = IOD_OH_UNDEFINED;
        grp->remote_group.iod_oh.wr_oh.cookie = IOD_OH_UNDEFINED;
        grp->remote_group.iod_id = iod_id;
        grp->remote_group.mdkv_id = mdkv_id;
        grp->remote_group.attrkv_id = attrkv_id;

        grp->remote_group.gcpl_id = H5Pcopy(H5P_GROUP_CREATE_DEFAULT);
        grp->gapl_id = H5P_GROUP_ACCESS_DEFAULT;

        /* set the input structure for the HG encode routine */
        input.coh = rc->file->remote_file.coh;
        input.iod_id = iod_id;

        /* set common object parameters */
        grp->common.obj_type = H5I_GROUP;
        grp->common.file = rc->file;
        grp->common.file->nopen_objs ++;
        grp->common.obj_name = NULL;

#if H5VL_IOD_DEBUG
        printf("Group open by token %"PRIu64": ID %"PRIu64"\n", 
               g_axe_id, input.iod_id);
#endif

        if(H5VL__iod_create_and_forward(H5VL_OBJECT_OPEN_BY_TOKEN_ID, HG_OBJECT_OPEN_BY_TOKEN, 
                                        (H5VL_iod_object_t *)grp, 1, 0, NULL,
                                        (H5VL_iod_req_info_t *)rc, &input, 
                                        &grp->remote_group.iod_oh, grp, req) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to create and ship group open_by_token");

        ret_value = (void *)grp;
        *opened_type = H5I_GROUP;
        break;
    case H5O_TYPE_MAP:
        /* allocate the dataset object that is returned to the user */
        if(NULL == (map = H5FL_CALLOC(H5VL_iod_map_t)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate object struct");

        map->remote_map.iod_oh.rd_oh.cookie = IOD_OH_UNDEFINED;
        map->remote_map.iod_oh.wr_oh.cookie = IOD_OH_UNDEFINED;
        map->remote_map.iod_id = iod_id;
        map->remote_map.mdkv_id = mdkv_id;
        map->remote_map.attrkv_id = attrkv_id;

        map->remote_map.mcpl_id = H5Pcopy(H5P_GROUP_CREATE_DEFAULT);
        map->mapl_id = H5P_GROUP_ACCESS_DEFAULT;

        /* decode key_type */
        {
            H5T_t *dt = NULL;
            size_t dt_size;

            HDmemcpy(&dt_size, buf_ptr, sizeof(size_t));
            buf_ptr += sizeof(size_t);
            /* Create datatype by decoding buffer */
            if(NULL == (dt = H5T_decode((const unsigned char *)buf_ptr)))
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTDECODE, NULL, "can't decode object");
            /* Register the type */
            if((map->remote_map.keytype_id = H5I_register(H5I_DATATYPE, dt, TRUE)) < 0)
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTREGISTER, NULL, "unable to register data type");
            buf_ptr += dt_size;
        }
        /* decode val_type */
        {
            H5T_t *dt = NULL;
            size_t dt_size;

            HDmemcpy(&dt_size, buf_ptr, sizeof(size_t));
            buf_ptr += sizeof(size_t);
            /* Create datatype by decoding buffer */
            if(NULL == (dt = H5T_decode((const unsigned char *)buf_ptr)))
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTDECODE, NULL, "can't decode object");
            /* Register the type */
            if((map->remote_map.valtype_id = H5I_register(H5I_DATATYPE, dt, TRUE)) < 0)
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTREGISTER, NULL, "unable to register data type");
            buf_ptr += dt_size;
        }

        /* set the input structure for the HG encode routine */
        input.coh = rc->file->remote_file.coh;
        input.iod_id = iod_id;

        /* set common object parameters */
        map->common.obj_type = H5I_MAP;
        map->common.file = rc->file;
        map->common.file->nopen_objs ++;
        map->common.obj_name = NULL;

#if H5VL_IOD_DEBUG
        printf("Map open by token %"PRIu64": ID %"PRIu64"\n", 
               g_axe_id, input.iod_id);
#endif

        if(H5VL__iod_create_and_forward(H5VL_OBJECT_OPEN_BY_TOKEN_ID, HG_OBJECT_OPEN_BY_TOKEN, 
                                        (H5VL_iod_object_t *)map, 1, 0, NULL, 
                                        (H5VL_iod_req_info_t *)rc, &input, 
                                        &map->remote_map, map, req) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to create and ship map open");

        ret_value = (void *)map;
        *opened_type = H5I_MAP;
        break;
    case H5O_TYPE_UNKNOWN:
    case H5O_TYPE_NTYPES:
    default:
        HGOTO_ERROR(H5E_ARGS, H5E_CANTINIT, NULL, "not a valid file object (dataset, map, group, or datatype)");
    }

done:

    if(ret_value == NULL) {
        switch(obj_type) {
            case H5O_TYPE_DATASET:
                if(dset != NULL) {
                    dset = H5FL_FREE(H5VL_iod_dset_t, dset);
                } /* end if */
                break;
            case H5O_TYPE_NAMED_DATATYPE:
                if(dtype != NULL) {
                    dtype = H5FL_FREE(H5VL_iod_dtype_t, dtype);
                } /* end if */
                break;
            case H5O_TYPE_GROUP:
                if(grp != NULL) {
                    grp = H5FL_FREE(H5VL_iod_group_t, grp);
                } /* end if */
                break;
            case H5O_TYPE_MAP:
                if(map != NULL) {
                    map = H5FL_FREE(H5VL_iod_map_t, map);
                } /* end if */
                break;
            case H5O_TYPE_UNKNOWN:
            case H5O_TYPE_NTYPES:
            default:
                HDONE_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "not a valid object type");
        } /* end switch */
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
}/* end H5VL_iod_obj_open_token() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_object_open
 *
 * Purpose:	Opens a object inside IOD file.
 *
 * Return:	Success:	object id. 
 *		Failure:	NULL
 *
 * Programmer:  Mohamad Chaarawi
 *              November, 2012
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL_iod_object_open(void *_obj, H5VL_loc_params_t loc_params, 
                     H5I_type_t *opened_type, hid_t dxpl_id, void UNUSED **req)
{
    H5VL_iod_object_t *obj = (H5VL_iod_object_t *)_obj; /* location object to open the group */
    H5P_genplist_t *plist = NULL;
    hid_t rcxt_id;
    H5RC_t *rc = NULL;
    size_t num_parents = 0;
    char *loc_name = NULL;
    object_op_in_t input;
    H5VL_iod_remote_object_t remote_obj; /* generic remote object structure */
    H5VL_iod_dset_t *dset = NULL; /* the dataset object that is created and passed to the user */
    H5VL_iod_dtype_t *dtype = NULL; /* the datatype object that is created and passed to the user */
    H5VL_iod_group_t  *grp = NULL; /* the group object that is created and passed to the user */
    H5VL_iod_map_t  *map = NULL; /* the map object that is created and passed to the user */
    H5VL_iod_request_t **parent_reqs = NULL;
    void *ret_value;

    FUNC_ENTER_NOAPI_NOINIT

    /* get the context ID */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, NULL, "can't find object for ID");
    if(H5P_get(plist, H5VL_CONTEXT_ID, &rcxt_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get property value for trans_id");

    /* get the RC object */
    if(NULL == (rc = (H5RC_t *)H5I_object_verify(rcxt_id, H5I_RC)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "not a READ CONTEXT ID")

    /* allocate parent request array */
    if(NULL == (parent_reqs = (H5VL_iod_request_t **)
                H5MM_malloc(sizeof(H5VL_iod_request_t *))))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, NULL, "can't allocate parent req element");

    /* retrieve parent requests */
    if(H5VL_iod_get_parent_requests(obj, (H5VL_iod_req_info_t *)rc, parent_reqs, &num_parents) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "Failed to retrieve parent requests");

    /* retrieve IOD info of location object */
    if(H5VL_iod_get_loc_info(obj, &input.loc_id, &input.loc_oh, &input.loc_mdkv_id, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "Failed to resolve current location group info");

    /* MSC - If location object not opened yet, wait for it. */
    if(IOD_OBJ_INVALID == input.loc_id) {
        /* Synchronously wait on the request attached to the dataset */
        if(H5VL_iod_request_wait(obj->file, obj->request) < 0)
            HGOTO_ERROR(H5E_DATASET,  H5E_CANTGET, NULL, "can't wait on HG request");
        obj->request = NULL;
        /* retrieve IOD info of location object */
        if(H5VL_iod_get_loc_info(obj, &input.loc_id, &input.loc_oh, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "Failed to resolve current location group info");
    }

    if(H5VL_OBJECT_BY_SELF == loc_params.type)
        loc_name = strdup(".");
    else if(H5VL_OBJECT_BY_NAME == loc_params.type)
        loc_name = strdup(loc_params.loc_data.loc_by_name.name);

    /* set the input structure for the HG encode routine */
    input.coh = obj->file->remote_file.coh;
    input.loc_name = loc_name;
    input.rcxt_num  = rc->c_version;
    input.cs_scope = obj->file->md_integrity_scope;

    /* H5Oopen has to be synchronous */
    if(H5VL__iod_create_and_forward(H5VL_OBJECT_OPEN_ID, HG_OBJECT_OPEN, 
                                    obj, 1, num_parents, parent_reqs,
                                    (H5VL_iod_req_info_t *)rc, &input, 
                                    &remote_obj, &remote_obj, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to create and ship object open");

    *opened_type = remote_obj.obj_type;

    if(loc_name) 
        HDfree(loc_name);

    switch(remote_obj.obj_type) {
    case H5I_DATASET:
        /* allocate the dataset object that is returned to the user */
        if(NULL == (dset = H5FL_CALLOC(H5VL_iod_dset_t)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate object struct");

        dset->remote_dset.iod_oh.rd_oh.cookie = remote_obj.iod_oh.rd_oh.cookie;
        dset->remote_dset.iod_oh.wr_oh.cookie = remote_obj.iod_oh.wr_oh.cookie;
        dset->remote_dset.iod_id = remote_obj.iod_id;
        dset->remote_dset.mdkv_id = remote_obj.mdkv_id;
        dset->remote_dset.attrkv_id = remote_obj.attrkv_id;
        dset->remote_dset.dcpl_id = remote_obj.cpl_id;
        dset->remote_dset.type_id = remote_obj.id1;
        dset->remote_dset.space_id = remote_obj.id2;

        if(dset->remote_dset.dcpl_id == H5P_DEFAULT){
            dset->remote_dset.dcpl_id = H5Pcopy(H5P_DATASET_CREATE_DEFAULT);
        }

        HDassert(dset->remote_dset.dcpl_id);
        HDassert(dset->remote_dset.type_id);
        HDassert(dset->remote_dset.space_id);

        /* setup the local dataset struct */
        /* store the entire path of the dataset locally */
        {
            size_t obj_name_len = HDstrlen(obj->obj_name);
            size_t name_len = HDstrlen(loc_params.loc_data.loc_by_name.name);

            if (NULL == (dset->common.obj_name = (char *)HDmalloc(obj_name_len + name_len + 1)))
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate");
            HDmemcpy(dset->common.obj_name, obj->obj_name, obj_name_len);
            HDmemcpy(dset->common.obj_name+obj_name_len, 
                     loc_params.loc_data.loc_by_name.name, name_len);
            dset->common.obj_name[obj_name_len+name_len] = '\0';
        }

        if((dset->dapl_id = H5Pcopy(H5P_DATASET_CREATE_DEFAULT)) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, NULL, "failed to copy dapl");

        /* set common object parameters */
        dset->common.obj_type = H5I_DATASET;
        dset->common.file = obj->file;
        dset->common.file->nopen_objs ++;

        ret_value = (void *)dset;
        break;
    case H5I_DATATYPE:
        /* allocate the dataset object that is returned to the user */
        if(NULL == (dtype = H5FL_CALLOC(H5VL_iod_dtype_t)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate object struct");

        dtype->remote_dtype.iod_oh.rd_oh.cookie = remote_obj.iod_oh.rd_oh.cookie;
        dtype->remote_dtype.iod_oh.wr_oh.cookie = remote_obj.iod_oh.wr_oh.cookie;
        dtype->remote_dtype.iod_id = remote_obj.iod_id;
        dtype->remote_dtype.mdkv_id = remote_obj.mdkv_id;
        dtype->remote_dtype.attrkv_id = remote_obj.attrkv_id;
        dtype->remote_dtype.tcpl_id = remote_obj.cpl_id;
        dtype->remote_dtype.type_id = remote_obj.id1;

        if(dtype->remote_dtype.tcpl_id == H5P_DEFAULT){
            dtype->remote_dtype.tcpl_id = H5Pcopy(H5P_DATATYPE_CREATE_DEFAULT);
        }

        HDassert(dtype->remote_dtype.tcpl_id);
        HDassert(dtype->remote_dtype.type_id);

        /* setup the local dataset struct */
        /* store the entire path of the dataset locally */
        {
            size_t obj_name_len = HDstrlen(obj->obj_name);
            size_t name_len = HDstrlen(loc_params.loc_data.loc_by_name.name);

            if (NULL == (dtype->common.obj_name = (char *)HDmalloc
                         (obj_name_len + name_len + 1)))
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate");
            HDmemcpy(dtype->common.obj_name, obj->obj_name, obj_name_len);
            HDmemcpy(dtype->common.obj_name+obj_name_len, 
                     loc_params.loc_data.loc_by_name.name, name_len);
            dtype->common.obj_name[obj_name_len+name_len] = '\0';
        }

        if((dtype->tapl_id = H5Pcopy(H5P_DATATYPE_CREATE_DEFAULT)) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, NULL, "failed to copy dapl");

        /* set common object parameters */
        dtype->common.obj_type = H5I_DATATYPE;
        dtype->common.file = obj->file;
        dtype->common.file->nopen_objs ++;

        ret_value = (void *)dtype;
        break;
    case H5I_GROUP:
        /* allocate the dataset object that is returned to the user */
        if(NULL == (grp = H5FL_CALLOC(H5VL_iod_group_t)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate object struct");

        grp->remote_group.iod_oh.rd_oh.cookie = remote_obj.iod_oh.rd_oh.cookie;
        grp->remote_group.iod_oh.wr_oh.cookie = remote_obj.iod_oh.wr_oh.cookie;
        grp->remote_group.iod_id = remote_obj.iod_id;
        grp->remote_group.mdkv_id = remote_obj.mdkv_id;
        grp->remote_group.attrkv_id = remote_obj.attrkv_id;
        grp->remote_group.gcpl_id = remote_obj.cpl_id;

        if(grp->remote_group.gcpl_id == H5P_DEFAULT){
            grp->remote_group.gcpl_id = H5Pcopy(H5P_GROUP_CREATE_DEFAULT);
        }

        HDassert(grp->remote_group.gcpl_id);

        /* setup the local dataset struct */
        /* store the entire path of the dataset locally */
        {
            size_t obj_name_len = HDstrlen(obj->obj_name);
            size_t name_len = HDstrlen(loc_params.loc_data.loc_by_name.name);

            if (NULL == (grp->common.obj_name = (char *)HDmalloc(obj_name_len + name_len + 1)))
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate");
            HDmemcpy(grp->common.obj_name, obj->obj_name, obj_name_len);
            HDmemcpy(grp->common.obj_name+obj_name_len, 
                     loc_params.loc_data.loc_by_name.name, name_len);
            grp->common.obj_name[obj_name_len+name_len] = '\0';
        }

        if((grp->gapl_id = H5Pcopy(H5P_GROUP_CREATE_DEFAULT)) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, NULL, "failed to copy gapl");

        /* set common object parameters */
        grp->common.obj_type = H5I_GROUP;
        grp->common.file = obj->file;
        grp->common.file->nopen_objs ++;

        ret_value = (void *)grp;
        break;
    case H5I_MAP:
        /* allocate the dataset object that is returned to the user */
        if(NULL == (map = H5FL_CALLOC(H5VL_iod_map_t)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate object struct");

        map->remote_map.iod_oh.rd_oh.cookie = remote_obj.iod_oh.rd_oh.cookie;
        map->remote_map.iod_oh.wr_oh.cookie = remote_obj.iod_oh.wr_oh.cookie;
        map->remote_map.iod_id     = remote_obj.iod_id;
        map->remote_map.mdkv_id    = remote_obj.mdkv_id;
        map->remote_map.attrkv_id  = remote_obj.attrkv_id;
        map->remote_map.mcpl_id    = remote_obj.cpl_id;
        map->remote_map.keytype_id = remote_obj.id1;
        map->remote_map.valtype_id = remote_obj.id2;

        if(map->remote_map.mcpl_id == H5P_DEFAULT){
            map->remote_map.mcpl_id = H5Pcopy(H5P_GROUP_CREATE_DEFAULT);
        }

        /* setup the local dataset struct */
        /* store the entire path of the dataset locally */
        {
            size_t obj_name_len = HDstrlen(obj->obj_name);
            size_t name_len = HDstrlen(loc_params.loc_data.loc_by_name.name);

            if (NULL == (map->common.obj_name = (char *)HDmalloc(obj_name_len + name_len + 1)))
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate");
            HDmemcpy(map->common.obj_name, obj->obj_name, obj_name_len);
            HDmemcpy(map->common.obj_name+obj_name_len, 
                     loc_params.loc_data.loc_by_name.name, name_len);
            map->common.obj_name[obj_name_len+name_len] = '\0';
        }

        if((map->mapl_id = H5Pcopy(H5P_GROUP_CREATE_DEFAULT)) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, NULL, "failed to copy mapl");

        /* set common object parameters */
        map->common.obj_type = H5I_MAP;
        map->common.file = obj->file;
        map->common.file->nopen_objs ++;

        ret_value = (void *)map;
        break;
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
    case H5I_GENPROP_CLS:
    case H5I_GENPROP_LST:
    case H5I_ERROR_CLASS:
    case H5I_ERROR_MSG:
    case H5I_ERROR_STACK:
    case H5I_NTYPES:
    default:
        HGOTO_ERROR(H5E_ARGS, H5E_CANTRELEASE, NULL, "not a valid file object (dataset, map, group, or datatype)")
            break;
    }

done:

    if(ret_value == NULL) {
        switch(remote_obj.obj_type) {
            case H5I_DATASET:
                if(dset != NULL) {
                    dset = H5FL_FREE(H5VL_iod_dset_t, dset);
                } /* end if */
                break;
            case H5I_DATATYPE:
                if(dtype != NULL) {
                    dtype = H5FL_FREE(H5VL_iod_dtype_t, dtype);
                } /* end if */
                break;
            case H5I_GROUP:
                if(grp != NULL) {
                    grp = H5FL_FREE(H5VL_iod_group_t, grp);
                } /* end if */
                break;
            case H5I_MAP:
                if(map != NULL) {
                    map = H5FL_FREE(H5VL_iod_map_t, map);
                } /* end if */
                break;
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
        case H5I_GENPROP_CLS:
        case H5I_GENPROP_LST:
        case H5I_ERROR_CLASS:
        case H5I_ERROR_MSG:
        case H5I_ERROR_STACK:
        case H5I_NTYPES:
            default:
                HDONE_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "not a valid object type");
        } /* end switch */
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_object_open */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_object_copy
 *
 * Purpose:	Copys a object through the IOD plugin.
 *
 * Return:	Success:	postive. 
 *		Failure:	NULL
 *
 * Programmer:  Mohamad Chaarawi
 *              November, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t 
H5VL_iod_object_copy(void *_src_obj, H5VL_loc_params_t UNUSED loc_params1, const char *src_name, 
                     void *_dst_obj, H5VL_loc_params_t UNUSED loc_params2, const char *dst_name, 
                     hid_t ocpypl_id, hid_t lcpl_id, hid_t dxpl_id, void **req)
{
    H5VL_iod_object_t *src_obj = (H5VL_iod_object_t *)_src_obj;
    H5VL_iod_object_t *dst_obj = (H5VL_iod_object_t *)_dst_obj;
    object_copy_in_t input;
    int *status = NULL;
    H5VL_iod_request_t **parent_reqs = NULL;
    H5P_genplist_t *plist = NULL;
    size_t num_parents = 0;
    hid_t trans_id;
    H5TR_t *tr = NULL;
    herr_t ret_value = SUCCEED;        /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* get the transaction ID */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");
    if(H5P_get(plist, H5VL_TRANS_ID, &trans_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get property value for trans_id");

    /* get the TR object */
    if(NULL == (tr = (H5TR_t *)H5I_object_verify(trans_id, H5I_TR)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a Transaction ID")

    /* allocate parent request array */
    if(NULL == (parent_reqs = (H5VL_iod_request_t **)
                H5MM_malloc(sizeof(H5VL_iod_request_t *) * 3)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate parent req element");

    /* retrieve parent requests */
    if(H5VL_iod_get_parent_requests(src_obj, (H5VL_iod_req_info_t *)tr, parent_reqs, &num_parents) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Failed to retrieve parent requests");
    /* retrieve IOD info of location object */
    if(H5VL_iod_get_loc_info(src_obj, &input.src_loc_id, &input.src_loc_oh, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Failed to resolve current location group info");

    /* retrieve parent requests */
    if(H5VL_iod_get_parent_requests(dst_obj, NULL, parent_reqs, &num_parents) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Failed to retrieve parent requests");
    /* retrieve IOD info of location object */
    if(H5VL_iod_get_loc_info(dst_obj, &input.dst_loc_id, &input.dst_loc_oh, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Failed to resolve current location group info");

    /* set the input structure for the HG encode routine */
    input.coh = src_obj->file->remote_file.coh;
    input.src_loc_name = src_name;
    input.dst_loc_name = dst_name;
    input.lcpl_id = lcpl_id;
    input.ocpypl_id = ocpypl_id;
    input.trans_num = tr->trans_num;
    input.rcxt_num  = tr->c_version;
    input.cs_scope = src_obj->file->md_integrity_scope;

    status = (herr_t *)malloc(sizeof(herr_t));

    if(H5VL__iod_create_and_forward(H5VL_OBJECT_COPY_ID, HG_OBJECT_COPY, 
                                    (H5VL_iod_object_t *)dst_obj, 1,
                                    num_parents, parent_reqs,
                                    (H5VL_iod_req_info_t *)tr, &input, status, status, req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to create and ship object copy");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_object_copy() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_object_visit
 *
 * Purpose:	Iterates through all objects linked to an object
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:  Mohamad Chaarawi
 *              November, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t H5VL_iod_object_visit(void UNUSED *_obj, H5VL_loc_params_t UNUSED loc_params, 
                                    H5_index_t UNUSED idx_type,
                                    H5_iter_order_t UNUSED order, H5O_iterate_t UNUSED op, 
                                    void UNUSED *op_data, 
                                    hid_t UNUSED dxpl_id, void UNUSED **req)
{
    //H5VL_iod_object_t *obj = (H5VL_iod_object_t *)_obj;
    //herr_t ret_value = SUCCEED;  /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5VL_iod_object_visit() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_object_misc
 *
 * Purpose:	Perform a plugin specific operation for an object
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:  Mohamad Chaarawi
 *              November, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_iod_object_misc(void *_obj, H5VL_loc_params_t loc_params, H5VL_object_misc_t misc_type, 
                     hid_t dxpl_id, void **req, va_list arguments)
{
    H5VL_iod_object_t *obj = (H5VL_iod_object_t *)_obj;
    iod_obj_id_t iod_id, mdkv_id, attrkv_id;
    iod_handles_t iod_oh;
    int *status = NULL;
    size_t num_parents = 0;
    hid_t trans_id;
    H5TR_t *tr = NULL;
    H5P_genplist_t *plist = NULL;
    H5VL_iod_request_t **parent_reqs = NULL;
    char *loc_name = NULL;
    herr_t ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* get the transaction ID */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");
    if(H5P_get(plist, H5VL_TRANS_ID, &trans_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get property value for trans_id");

    /* get the TR object */
    if(NULL == (tr = (H5TR_t *)H5I_object_verify(trans_id, H5I_TR)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a Transaction ID")

    /* allocate parent request array */
    if(NULL == (parent_reqs = (H5VL_iod_request_t **)
                H5MM_malloc(sizeof(H5VL_iod_request_t *) * 2)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate parent req element");

    /* retrieve parent requests */
    if(H5VL_iod_get_parent_requests(obj, (H5VL_iod_req_info_t *)tr, parent_reqs, &num_parents) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Failed to retrieve parent requests");

    /* retrieve IOD info of location object */
    if(H5VL_iod_get_loc_info(obj, &iod_id, &iod_oh, &mdkv_id, &attrkv_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Failed to resolve current location group info");

    /* MSC - If location object not opened yet, wait for it. */
    if(IOD_OBJ_INVALID == iod_id) {
        /* Synchronously wait on the request attached to the dataset */
        if(H5VL_iod_request_wait(obj->file, obj->request) < 0)
            HGOTO_ERROR(H5E_DATASET,  H5E_CANTGET, FAIL, "can't wait on HG request");
        obj->request = NULL;
        /* retrieve IOD info of location object */
        if(H5VL_iod_get_loc_info(obj, &iod_id, &iod_oh, &mdkv_id, &attrkv_id) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Failed to resolve current location group info");
    }

    switch (misc_type) {
        /* H5Arename/rename_by_name */
        case H5VL_ATTR_RENAME:
            {
                const char    *old_name  = va_arg (arguments, const char *);
                const char    *new_name  = va_arg (arguments, const char *);
                attr_rename_in_t input;

                if(H5VL_OBJECT_BY_SELF == loc_params.type)
                    loc_name = strdup(".");
                else if(H5VL_OBJECT_BY_NAME == loc_params.type)
                    loc_name = strdup(loc_params.loc_data.loc_by_name.name);

                /* set the input structure for the HG encode routine */
                input.coh = obj->file->remote_file.coh;
                input.old_attr_name = old_name;
                input.new_attr_name = new_name;
                input.loc_id = iod_id;
                input.loc_attrkv_id = attrkv_id;
                input.loc_oh = iod_oh;
                input.path = loc_name;
                input.trans_num = tr->trans_num;
                input.rcxt_num  = tr->c_version;
                input.cs_scope = obj->file->md_integrity_scope;

#if H5VL_IOD_DEBUG
                printf("Attribute Rename %s to %s LOC ID %"PRIu64", axe id %"PRIu64"\n", 
                       old_name, new_name, input.loc_id, g_axe_id);
#endif

                status = (herr_t *)malloc(sizeof(herr_t));

                if(H5VL__iod_create_and_forward(H5VL_ATTR_RENAME_ID, HG_ATTR_RENAME, 
                                                obj, 1, num_parents, parent_reqs,
                                                (H5VL_iod_req_info_t *)tr, &input, status, status, req) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to create and ship attribute rename");

                if(loc_name) 
                    HDfree(loc_name);

                break;
            }
        /* H5Oset_comment */
        case H5VL_OBJECT_SET_COMMENT:
            {
                const char    *comment  = va_arg (arguments, char *);
                object_set_comment_in_t input;

                if(H5VL_OBJECT_BY_SELF == loc_params.type)
                    loc_name = strdup(".");
                else if(H5VL_OBJECT_BY_NAME == loc_params.type)
                    loc_name = strdup(loc_params.loc_data.loc_by_name.name);

                /* set the input structure for the HG encode routine */
                input.coh = obj->file->remote_file.coh;
                input.comment = comment;
                input.loc_id = iod_id;
                input.loc_mdkv_id = mdkv_id;
                input.loc_oh = iod_oh;
                input.path = loc_name;
                input.trans_num = tr->trans_num;
                input.rcxt_num  = tr->c_version;
                input.cs_scope = obj->file->md_integrity_scope;

                status = (herr_t *)malloc(sizeof(herr_t));

                if(H5VL__iod_create_and_forward(H5VL_OBJECT_SET_COMMENT_ID, HG_OBJECT_SET_COMMENT, 
                                                obj, 0, num_parents, parent_reqs,
                                                (H5VL_iod_req_info_t *)tr, &input, status, status, req) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to create and ship object set_comment");

                /* store the comment locally if the object is open */
                if(loc_params.type == H5VL_OBJECT_BY_SELF)
                    obj->comment = HDstrdup(comment);

                if(loc_name) 
                    HDfree(loc_name);
                break;
            }
        /* H5Oincr_refcount / H5Odecr_refcount */
        case H5VL_OBJECT_CHANGE_REF_COUNT:
            {
                //int update_ref  = va_arg (arguments, int);
            }
        case H5VL_REF_CREATE:
            {
                /*
                void        *ref      = va_arg (arguments, void *);
                const char  *name     = va_arg (arguments, char *);
                H5R_type_t  ref_type  = va_arg (arguments, H5R_type_t);
                hid_t       space_id  = va_arg (arguments, hid_t);
                size_t ref_size = 0;

                if(ref_type == H5R_DATASET_REGION)
                    ref_size = sizeof(hdset_reg_ref_t);
                else if (ref_type == H5R_OBJECT)
                    ref_size = sizeof(hobj_ref_t);
                */
            }
        default:
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't recognize this operation type")
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_object_misc() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_object_get
 *
 * Purpose:	Gets certain data about a file
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:  Mohamad Chaarawi
 *              November, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_iod_object_get(void *_obj, H5VL_loc_params_t loc_params, H5VL_object_get_t get_type, 
                    hid_t dxpl_id, void **req, va_list arguments)
{
    H5VL_iod_object_t *obj = (H5VL_iod_object_t *)_obj;
    size_t num_parents = 0;
    hid_t rcxt_id;
    H5RC_t *rc = NULL;
    H5P_genplist_t *plist = NULL;
    iod_obj_id_t iod_id, mdkv_id, attrkv_id;
    iod_handles_t iod_oh;
    H5VL_iod_request_t **parent_reqs = NULL;
    char *loc_name = NULL;
    herr_t ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* get the context ID */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");
    if(H5P_get(plist, H5VL_CONTEXT_ID, &rcxt_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get property value for trans_id");

    /* get the RC object */
    if(NULL == (rc = (H5RC_t *)H5I_object_verify(rcxt_id, H5I_RC)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a READ CONTEXT ID");

    /* allocate parent request array */
    if(NULL == (parent_reqs = (H5VL_iod_request_t **)
                H5MM_malloc(sizeof(H5VL_iod_request_t *))))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate parent req element");

    /* retrieve parent requests */
    if(H5VL_iod_get_parent_requests(obj, (H5VL_iod_req_info_t *)rc, parent_reqs, &num_parents) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Failed to retrieve parent requests");

    /* retrieve IOD info of location object */
    if(H5VL_iod_get_loc_info(obj, &iod_id, &iod_oh, &mdkv_id, &attrkv_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Failed to resolve current location group info");

    /* MSC - If location object not opened yet, wait for it. */
    if(IOD_OBJ_INVALID == iod_id) {
        /* Synchronously wait on the request attached to the dataset */
        if(H5VL_iod_request_wait(obj->file, obj->request) < 0)
            HGOTO_ERROR(H5E_DATASET,  H5E_CANTGET, FAIL, "can't wait on HG request");
        obj->request = NULL;
        /* retrieve IOD info of location object */
        if(H5VL_iod_get_loc_info(obj, &iod_id, &iod_oh, &mdkv_id, &attrkv_id) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Failed to resolve current location group info");
    }

    switch (get_type) {
        /* H5Oexists_by_name */
        case H5VL_OBJECT_EXISTS:
            {
                htri_t	  *ret = va_arg (arguments, htri_t *);
                object_op_in_t input;

                if(H5VL_OBJECT_BY_SELF == loc_params.type)
                    loc_name = strdup(".");
                else if(H5VL_OBJECT_BY_NAME == loc_params.type)
                    loc_name = strdup(loc_params.loc_data.loc_by_name.name);

                /* set the input structure for the HG encode routine */
                input.coh = obj->file->remote_file.coh;
                input.loc_id = iod_id;
                input.loc_mdkv_id = mdkv_id;
                input.loc_oh = iod_oh;
                input.rcxt_num  = rc->c_version;
                input.cs_scope = obj->file->md_integrity_scope;
                input.loc_name = loc_name;

#if H5VL_IOD_DEBUG
                printf("Object Exists axe %"PRIu64": %s ID %"PRIu64"\n", 
                       g_axe_id, input.loc_name, input.loc_id);
#endif

                if(H5VL__iod_create_and_forward(H5VL_OBJECT_EXISTS_ID, HG_OBJECT_EXISTS, 
                                                obj, 0, num_parents, parent_reqs,
                                                (H5VL_iod_req_info_t *)rc, &input, ret, ret, req) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to create and ship object exists");

                if(loc_name) 
                    HDfree(loc_name);
                break;
            }
        /* H5Oget_comment / H5Oget_comment_by_name */
        case H5VL_OBJECT_GET_COMMENT:
            {
                char *comment =  va_arg (arguments, char *);
                size_t size  =  va_arg (arguments, size_t);
                ssize_t *ret =  va_arg (arguments, ssize_t *);
                object_get_comment_in_t input;
                object_get_comment_out_t *result = NULL;
                size_t len;

                /* If the comment is present locally, get it and return */
                if(loc_params.type == H5VL_OBJECT_BY_SELF && obj->comment) {
                    len = HDstrlen(obj->comment);

                    if(comment) {
                        HDstrncpy(comment, obj->comment, MIN(len + 1,size));
                        if(len >= size)
                            comment[size-1]='\0';
                    } /* end if */

                    /* Set the return value for the API call */
                    *ret = (ssize_t)len;
                    break;
                }

                /* Otherwise Go to the server */

                if(H5VL_OBJECT_BY_SELF == loc_params.type)
                    loc_name = strdup(".");
                else if(H5VL_OBJECT_BY_NAME == loc_params.type)
                    loc_name = strdup(loc_params.loc_data.loc_by_name.name);

                /* set the input structure for the HG encode routine */
                input.coh = obj->file->remote_file.coh;
                input.loc_id = iod_id;
                input.loc_oh = iod_oh;
                input.loc_mdkv_id = mdkv_id;
                input.rcxt_num  = rc->c_version;
                input.cs_scope = obj->file->md_integrity_scope;
                input.path = loc_name;
                if(comment)
                    input.length = size;
                else
                    input.length = 0;

                if(NULL == (result = (object_get_comment_out_t *)malloc
                            (sizeof(object_get_comment_out_t)))) {
                    HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate get comment return struct");
                }

                result->name.size = input.length;
                result->name.value_size = ret;
                result->name.value = comment;

#if H5VL_IOD_DEBUG
                printf("Object Get Comment axe %"PRIu64": %s ID %"PRIu64"\n", 
                       g_axe_id, loc_name, input.loc_id);
#endif

                if(H5VL__iod_create_and_forward(H5VL_OBJECT_GET_COMMENT_ID, HG_OBJECT_GET_COMMENT, 
                                                obj, 0, num_parents, parent_reqs,
                                                (H5VL_iod_req_info_t *)rc, &input, result, result, req) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to create and ship object get_comment");

                if(loc_name) 
                    HDfree(loc_name);
                break;
            }
        /* H5Oget_info / H5Oget_info_by_name / H5Oget_info_by_idx */
        case H5VL_OBJECT_GET_INFO:
            {
                H5O_ff_info_t  *oinfo = va_arg (arguments, H5O_ff_info_t *);
                object_op_in_t input;

                if(H5VL_OBJECT_BY_SELF == loc_params.type)
                    loc_name = strdup(".");
                else if(H5VL_OBJECT_BY_NAME == loc_params.type)
                    loc_name = strdup(loc_params.loc_data.loc_by_name.name);

                /* set the input structure for the HG encode routine */
                input.coh = obj->file->remote_file.coh;
                input.loc_id = iod_id;
                input.loc_oh = iod_oh;
                input.loc_mdkv_id = mdkv_id;
                input.loc_attrkv_id = attrkv_id;
                input.rcxt_num  = rc->c_version;
                input.cs_scope = obj->file->md_integrity_scope;
                input.loc_name = loc_name;

#if H5VL_IOD_DEBUG
                printf("Object get_info axe %"PRIu64": %s ID %"PRIu64"\n", 
                       g_axe_id, input.loc_name, input.loc_id);
#endif

                if(H5VL__iod_create_and_forward(H5VL_OBJECT_GET_INFO_ID, HG_OBJECT_GET_INFO, 
                                                obj, 0, num_parents, parent_reqs,
                                                (H5VL_iod_req_info_t *)rc, &input, oinfo, oinfo, req) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to create and ship object get_info");

                if(loc_name) 
                    HDfree(loc_name);
                break;
            }
        /* H5Rget_region */
        case H5VL_REF_GET_REGION:
            {
                //hid_t       *ret     =  va_arg (arguments, hid_t *);
                //H5R_type_t  ref_type =  va_arg (arguments, H5R_type_t);
                //void        *ref     =  va_arg (arguments, void *);
                //H5S_t       *space = NULL;    /* Dataspace object */
            }
        /* H5Rget_obj_type2 */
        case H5VL_REF_GET_TYPE:
            {
                //H5O_type_t  *obj_type  =  va_arg (arguments, H5O_type_t *);
                //H5R_type_t  ref_type   =  va_arg (arguments, H5R_type_t);
                //const void  *ref       =  va_arg (arguments, const void *);
            }
        /* H5Rget_name */
        case H5VL_REF_GET_NAME:
            {
                //ssize_t     *ret       = va_arg (arguments, ssize_t *);
                //char        *name      = va_arg (arguments, char *);
                //size_t      size       = va_arg (arguments, size_t);
                //H5R_type_t  ref_type   = va_arg (arguments, H5R_type_t);
                //void        *ref       = va_arg (arguments, void *);
            }
        default:
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't get this type of information from object")
    }
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_object_get() */

void *
H5VL_iod_map_create(void *_obj, H5VL_loc_params_t UNUSED loc_params, const char *name, 
                    hid_t keytype, hid_t valtype, hid_t lcpl_id, hid_t mcpl_id, 
                    hid_t mapl_id, hid_t trans_id, void **req)
{
    H5VL_iod_object_t *obj = (H5VL_iod_object_t *)_obj; /* location object to create the group */
    H5VL_iod_map_t *map = NULL; /* the map object that is created and passed to the user */
    map_create_in_t input;
    iod_obj_id_t iod_id;
    iod_handles_t iod_oh;
    H5VL_iod_request_t **parent_reqs = NULL;
    size_t num_parents = 0;
    H5TR_t *tr = NULL;
    void *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    /* get the TR object */
    if(NULL == (tr = (H5TR_t *)H5I_object_verify(trans_id, H5I_TR)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "not a Transaction ID")

    /* allocate parent request array */
    if(NULL == (parent_reqs = (H5VL_iod_request_t **)
                H5MM_malloc(sizeof(H5VL_iod_request_t *) * 2)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, NULL, "can't allocate parent req element");

    /* retrieve parent requests */
    if(H5VL_iod_get_parent_requests(obj, (H5VL_iod_req_info_t *)tr, parent_reqs, &num_parents) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "Failed to retrieve parent requests");

    /* retrieve IOD info of location object */
    if(H5VL_iod_get_loc_info(obj, &iod_id, &iod_oh, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "Failed to resolve current location group info");

    /* MSC - If location object not opened yet, wait for it. */
    if(IOD_OBJ_INVALID == iod_id) {
        /* Synchronously wait on the request attached to the dataset */
        if(H5VL_iod_request_wait(obj->file, obj->request) < 0)
            HGOTO_ERROR(H5E_DATASET,  H5E_CANTGET, NULL, "can't wait on HG request");
        obj->request = NULL;
        /* retrieve IOD info of location object */
        if(H5VL_iod_get_loc_info(obj, &iod_id, &iod_oh, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "Failed to resolve current location group info");
    }

    /* allocate the map object that is returned to the user */
    if(NULL == (map = H5FL_CALLOC(H5VL_iod_map_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate object struct");

    map->remote_map.iod_oh.rd_oh.cookie = IOD_OH_UNDEFINED;
    map->remote_map.iod_oh.wr_oh.cookie = IOD_OH_UNDEFINED;
    map->remote_map.iod_id = IOD_OBJ_INVALID;
    map->remote_map.mcpl_id = -1;

    /* Generate IOD IDs for the map to be created */
    H5VL_iod_gen_obj_id(obj->file->my_rank, obj->file->num_procs, 
                        obj->file->remote_file.kv_oid_index, 
                        IOD_OBJ_KV, &input.map_id);
    map->remote_map.iod_id = input.map_id;
    /* increment the index of KV objects created on the container */
    obj->file->remote_file.kv_oid_index ++;

    H5VL_iod_gen_obj_id(obj->file->my_rank, obj->file->num_procs, 
                        obj->file->remote_file.kv_oid_index, 
                        IOD_OBJ_KV, &input.mdkv_id);
    map->remote_map.mdkv_id = input.mdkv_id;
    /* increment the index of KV objects created on the container */
    obj->file->remote_file.kv_oid_index ++;

    H5VL_iod_gen_obj_id(obj->file->my_rank, obj->file->num_procs, 
                        obj->file->remote_file.kv_oid_index, 
                        IOD_OBJ_KV, &input.attrkv_id);
    map->remote_map.attrkv_id = input.attrkv_id;
    /* increment the index of KV objects created on the container */
    obj->file->remote_file.kv_oid_index ++;

    /* set the input structure for the HG encode routine */
    input.coh = obj->file->remote_file.coh;
    input.loc_id = iod_id;
    input.loc_oh = iod_oh;
    input.name = name;
    input.keytype_id = keytype;
    input.valtype_id = valtype;
    input.mcpl_id = mcpl_id;
    input.mapl_id = mapl_id;
    input.lcpl_id = lcpl_id;
    input.trans_num = tr->trans_num;
    input.rcxt_num  = tr->c_version;
    input.cs_scope = obj->file->md_integrity_scope;

#if H5VL_IOD_DEBUG
    printf("Map Create %s, IOD ID %"PRIu64", axe id %"PRIu64"\n", 
           name, input.map_id, g_axe_id);
#endif

    /* setup the local map struct */
    /* store the entire path of the map locally */
    {
        size_t obj_name_len = HDstrlen(obj->obj_name);
        size_t name_len = HDstrlen(name);

        if (NULL == (map->common.obj_name = (char *)HDmalloc(obj_name_len + name_len + 1)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate");
        HDmemcpy(map->common.obj_name, obj->obj_name, obj_name_len);
        HDmemcpy(map->common.obj_name+obj_name_len, name, name_len);
        map->common.obj_name[obj_name_len+name_len] = '\0';
    }

    /* copy property lists */
    if((map->remote_map.mcpl_id = H5Pcopy(mcpl_id)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, NULL, "failed to copy mcpl");
    if((map->mapl_id = H5Pcopy(mapl_id)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, NULL, "failed to copy mapl");
    if((map->remote_map.keytype_id = H5Tcopy(keytype)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, NULL, "failed to copy dtype");
    if((map->remote_map.valtype_id = H5Tcopy(valtype)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, NULL, "failed to copy dtype");

    /* set common object parameters */
    map->common.obj_type = H5I_MAP;
    map->common.file = obj->file;
    map->common.file->nopen_objs ++;

    if(H5VL__iod_create_and_forward(H5VL_MAP_CREATE_ID, HG_MAP_CREATE, 
                                    (H5VL_iod_object_t *)map, 1, 
                                    num_parents, parent_reqs,
                                    (H5VL_iod_req_info_t *)tr, &input, &map->remote_map, map, req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to create and ship map create");

    ret_value = (void *)map;

done:
    if(ret_value == NULL) {
        if(map != NULL) {
            map = H5FL_FREE(H5VL_iod_map_t, map);
        } /* end if */
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_map_create() */

void *
H5VL_iod_map_open(void *_obj, H5VL_loc_params_t UNUSED loc_params, const char *name, 
                  hid_t mapl_id, hid_t rcxt_id, void **req)
{
    H5VL_iod_object_t *obj = (H5VL_iod_object_t *)_obj; /* location object to create the group */
    H5VL_iod_map_t *map = NULL; /* the map object that is created and passed to the user */
    map_open_in_t input;
    iod_obj_id_t iod_id;
    iod_handles_t iod_oh;
    H5VL_iod_request_t **parent_reqs = NULL;
    H5RC_t *rc = NULL;
    size_t num_parents = 0;
    void *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    /* get the RC object */
    if(NULL == (rc = (H5RC_t *)H5I_object_verify(rcxt_id, H5I_RC)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "not a READ CONTEXT ID")

    /* allocate parent request array */
    if(NULL == (parent_reqs = (H5VL_iod_request_t **)
                H5MM_malloc(sizeof(H5VL_iod_request_t *))))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, NULL, "can't allocate parent req element");

    /* retrieve parent requests */
    if(H5VL_iod_get_parent_requests(obj, (H5VL_iod_req_info_t *)rc, parent_reqs, &num_parents) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "Failed to retrieve parent requests");

    /* retrieve IOD info of location object */
    if(H5VL_iod_get_loc_info(obj, &iod_id, &iod_oh, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "Failed to resolve current location group info");

    /* MSC - If location object not opened yet, wait for it. */
    if(IOD_OBJ_INVALID == iod_id) {
        /* Synchronously wait on the request attached to the dataset */
        if(H5VL_iod_request_wait(obj->file, obj->request) < 0)
            HGOTO_ERROR(H5E_DATASET,  H5E_CANTGET, NULL, "can't wait on HG request");
        obj->request = NULL;
        /* retrieve IOD info of location object */
        if(H5VL_iod_get_loc_info(obj, &iod_id, &iod_oh, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "Failed to resolve current location group info");
    }

    /* allocate the map object that is returned to the user */
    if(NULL == (map = H5FL_CALLOC(H5VL_iod_map_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate object struct");

    map->remote_map.iod_oh.rd_oh.cookie = IOD_OH_UNDEFINED;
    map->remote_map.iod_oh.wr_oh.cookie = IOD_OH_UNDEFINED;
    map->remote_map.iod_id = IOD_OBJ_INVALID;
    map->remote_map.mdkv_id = IOD_OBJ_INVALID;
    map->remote_map.attrkv_id = IOD_OBJ_INVALID;
    map->remote_map.keytype_id = -1;
    map->remote_map.valtype_id = -1;

    /* set the input structure for the HG encode routine */
    input.coh = obj->file->remote_file.coh;
    input.loc_id = iod_id;
    input.loc_oh = iod_oh;
    input.name = name;
    input.mapl_id = mapl_id;
    input.rcxt_num  = rc->c_version;
    input.cs_scope = obj->file->md_integrity_scope;

#if H5VL_IOD_DEBUG
    printf("Map Open %s LOC ID %"PRIu64", axe id %"PRIu64"\n", 
           name, input.loc_id, g_axe_id);
#endif

    /* setup the local map struct */
    /* store the entire path of the map locally */
    {
        size_t obj_name_len = HDstrlen(obj->obj_name);
        size_t name_len = HDstrlen(name);

        if (NULL == (map->common.obj_name = (char *)HDmalloc(obj_name_len + name_len + 1)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate");
        HDmemcpy(map->common.obj_name, obj->obj_name, obj_name_len);
        HDmemcpy(map->common.obj_name+obj_name_len, name, name_len);
        map->common.obj_name[obj_name_len+name_len] = '\0';
    }

    /* copy property lists */
    if((map->mapl_id = H5Pcopy(mapl_id)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, NULL, "failed to copy mapl");

    /* set common object parameters */
    map->common.obj_type = H5I_MAP;
    map->common.file = obj->file;
    map->common.file->nopen_objs ++;

    if(H5VL__iod_create_and_forward(H5VL_MAP_OPEN_ID, HG_MAP_OPEN, (H5VL_iod_object_t *)map, 1, 
                                    num_parents, parent_reqs,
                                    (H5VL_iod_req_info_t *)rc, &input, &map->remote_map, map, req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "failed to create and ship map open");

    ret_value = (void *)map;

done:
    if(ret_value == NULL) {
        if(map != NULL) {
            map = H5FL_FREE(H5VL_iod_map_t, map);
        } /* end if */
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_map_open() */

herr_t 
H5VL_iod_map_set(void *_map, hid_t key_mem_type_id, const void *key, 
                 hid_t val_mem_type_id, const void *value, hid_t dxpl_id, 
                 hid_t trans_id, void **req)
{
    H5VL_iod_map_t *map = (H5VL_iod_map_t *)_map;
    map_set_in_t input;
    size_t key_size, val_size;
    int *status = NULL;
    H5P_genplist_t *plist = NULL;
    size_t num_parents = 0;
    H5TR_t *tr = NULL;
    hg_bulk_t *value_handle = NULL;
    uint64_t key_cs, value_cs, user_cs;
    uint32_t raw_cs_scope;
    H5VL_iod_request_t **parent_reqs = NULL;
    H5T_class_t val_type_class;
    H5VL_iod_map_set_info_t *info = NULL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* If there is information needed about the dataset that is not present locally, wait */
    if(-1 == map->remote_map.keytype_id ||
       -1 == map->remote_map.valtype_id) {
        /* Synchronously wait on the request attached to the dataset */
        if(H5VL_iod_request_wait(map->common.file, map->common.request) < 0)
            HGOTO_ERROR(H5E_SYM,  H5E_CANTGET, FAIL, "can't wait on HG request");
        map->common.request = NULL;
    }

    /* get the key size and checksum from the provided key datatype & buffer */
    if(H5VL_iod_map_get_size(key_mem_type_id, key, &key_cs, &key_size, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't get key size");

    /* get the plist pointer */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");

    /* get the data integrity scope */
    if(H5P_get(plist, H5VL_CS_BITFLAG_NAME, &raw_cs_scope) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get scope for data integrity checks");

    if(raw_cs_scope) {
        /* get the value size and checksum from the provided value datatype & buffer */
        if(H5VL_iod_map_get_size(val_mem_type_id, value, &value_cs, 
                                 &val_size, &val_type_class) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't get value size");
    }
    else {
#if H5VL_IOD_DEBUG        
        printf("NO DATA INTEGRITY CHECKS ON RAW DATA WRITTEN\n");
#endif
        /* get the value size and checksum from the provided value datatype & buffer */
        if(H5VL_iod_map_get_size(val_mem_type_id, value, NULL, 
                                 &val_size, &val_type_class) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't get value size");
    }

    /* Verify the checksum value if the dxpl contains a user defined checksum */
    if(H5P_get(plist, H5D_XFER_CHECKSUM_NAME, &user_cs) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to get checksum value");

    if((raw_cs_scope & H5_CHECKSUM_MEMORY) && user_cs && 
       user_cs != value_cs) {
        fprintf(stderr, "Errrr.. In memory Data corruption. expecting %"PRIu64", got %"PRIu64"\n",
                user_cs, value_cs);
        HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "Checksum verification failed");
    }

    /* allocate a bulk data transfer handle */
    if(NULL == (value_handle = (hg_bulk_t *)H5MM_malloc(sizeof(hg_bulk_t))))
        HGOTO_ERROR(H5E_DATASET, H5E_NOSPACE, FAIL, "can't allocate a bulk data transfer handle");

    /* Register memory */
    if(H5T_VLEN == val_type_class) {
        /* if this is a VL type buffer, set the buffer pointer to the
           actual data (the p pointer) */
        if(HG_SUCCESS != HG_Bulk_handle_create(((const hvl_t *)value)->p, val_size, 
                                               HG_BULK_READ_ONLY, value_handle))
            HGOTO_ERROR(H5E_ATTR, H5E_WRITEERROR, FAIL, "can't create Bulk Data Handle");
    }
    else {
        if(HG_SUCCESS != HG_Bulk_handle_create(value, val_size, HG_BULK_READ_ONLY, value_handle))
            HGOTO_ERROR(H5E_ATTR, H5E_WRITEERROR, FAIL, "can't create Bulk Data Handle");
    }
    /* get the TR object */
    if(NULL == (tr = (H5TR_t *)H5I_object_verify(trans_id, H5I_TR)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a Transaction ID")

    if(NULL == (parent_reqs = (H5VL_iod_request_t **)
                H5MM_malloc(sizeof(H5VL_iod_request_t *) * 2)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate parent req element");

    /* retrieve parent requests */
    if(H5VL_iod_get_parent_requests((H5VL_iod_object_t *)map, (H5VL_iod_req_info_t *)tr, 
                                    parent_reqs, &num_parents) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Failed to retrieve parent requests");

    /* Fill input structure */
    input.coh = map->common.file->remote_file.coh;
    input.iod_oh = map->remote_map.iod_oh;
    input.iod_id = map->remote_map.iod_id;
    input.dxpl_id = dxpl_id;
    input.key_maptype_id = map->remote_map.keytype_id;
    input.key_memtype_id = key_mem_type_id;
    input.key.buf_size = key_size;
    input.key.buf = key;
    input.val_maptype_id = map->remote_map.valtype_id;
    input.val_memtype_id = val_mem_type_id;
    input.val_handle = *value_handle;
    input.val_checksum = value_cs;
    input.trans_num = tr->trans_num;
    input.rcxt_num  = tr->c_version;
    input.cs_scope = map->common.file->md_integrity_scope;

    status = (int *)malloc(sizeof(int));

#if H5VL_IOD_DEBUG
    printf("MAP set, value size %zu, axe id %"PRIu64"\n", val_size, g_axe_id);
#endif

    /* setup info struct for I/O request 
       This is to manage the I/O operation once the wait is called. */
    if(NULL == (info = (H5VL_iod_map_set_info_t *)H5MM_calloc(sizeof(H5VL_iod_map_set_info_t))))
	HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate a request");

    info->status = status;
    info->value_handle = value_handle;

    if(H5VL__iod_create_and_forward(H5VL_MAP_SET_ID, HG_MAP_SET, 
                                    (H5VL_iod_object_t *)map, 0,
                                    num_parents, parent_reqs,
                                    (H5VL_iod_req_info_t *)tr, &input, status, info, req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to create and ship map set");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_map_set() */

herr_t 
H5VL_iod_map_get(void *_map, hid_t key_mem_type_id, const void *key, 
                 hid_t val_mem_type_id, void *value, 
                 hid_t dxpl_id, hid_t rcxt_id, void **req)
{
    H5VL_iod_map_t *map = (H5VL_iod_map_t *)_map;
    map_get_in_t input;
    H5P_genplist_t *plist = NULL;
    map_get_out_t *output = NULL;
    H5VL_iod_map_io_info_t *info = NULL;
    size_t key_size, val_size;
    hg_bulk_t *value_handle = NULL;
    hg_bulk_t dummy_handle;
    uint64_t key_cs = 0;
    H5RC_t *rc = NULL;
    size_t num_parents = 0;
    hbool_t val_is_vl;
    H5VL_iod_request_t **parent_reqs = NULL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* If there is information needed about the map that is not present locally, wait */
    if(-1 == map->remote_map.keytype_id ||
       -1 == map->remote_map.valtype_id) {
        /* Synchronously wait on the request attached to the map */
        if(H5VL_iod_request_wait(map->common.file, map->common.request) < 0)
            HGOTO_ERROR(H5E_SYM,  H5E_CANTGET, FAIL, "can't wait on HG request");
        map->common.request = NULL;
    }

    /* get the key size and checksum from the provided key datatype & buffer */
    if(H5VL_iod_map_get_size(key_mem_type_id, key, &key_cs, &key_size, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't get key size");

    /* get information about the datatype of the value. Get the values
       size if it is not VL. val_size will be 0 if it is VL */
    if(H5VL_iod_map_dtype_info(val_mem_type_id, &val_is_vl, &val_size) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't get key size");

    /* allocate a bulk data transfer handle */
    if(NULL == (value_handle = (hg_bulk_t *)H5MM_malloc(sizeof(hg_bulk_t))))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate a buld data transfer handle");

    /* If the val type is not VL, then we know the size and we can
       create the bulk handle */
    if(!val_is_vl) {
        /* Register memory with bulk_handle */
        if(HG_SUCCESS != HG_Bulk_handle_create(value, val_size, 
                                               HG_BULK_READWRITE, value_handle))
            HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "can't create Bulk Data Handle");
    }
    else {
        /* Register memory with bulk_handle */
        if(HG_SUCCESS != HG_Bulk_handle_create(value, (size_t)1, 
                                               HG_BULK_READWRITE, &dummy_handle))
            HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "can't create Bulk Data Handle");
    }

    /* get the RC object */
    if(NULL == (rc = (H5RC_t *)H5I_object_verify(rcxt_id, H5I_RC)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a READ CONTEXT ID");

    if(NULL == (parent_reqs = (H5VL_iod_request_t **)
                H5MM_malloc(sizeof(H5VL_iod_request_t *))))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate parent req element");

    /* retrieve parent requests */
    if(H5VL_iod_get_parent_requests((H5VL_iod_object_t *)map, (H5VL_iod_req_info_t *)rc, 
                                    parent_reqs, &num_parents) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Failed to retrieve parent requests");

    /* Fill input structure */
    input.coh = map->common.file->remote_file.coh;
    input.iod_oh = map->remote_map.iod_oh;
    input.iod_id = map->remote_map.iod_id;
    input.dxpl_id = dxpl_id;
    input.key_memtype_id = key_mem_type_id;
    input.key_maptype_id = map->remote_map.keytype_id;
    input.val_memtype_id = val_mem_type_id;
    input.val_maptype_id = map->remote_map.valtype_id;
    input.key.buf_size = key_size;
    input.key.buf = key;
    input.val_is_vl = val_is_vl;
    input.val_size = val_size;
    input.rcxt_num = rc->c_version;
    if(!val_is_vl)
        input.val_handle = *value_handle;
    else {
        input.val_handle = dummy_handle;
    }
    input.cs_scope = map->common.file->md_integrity_scope;

#if H5VL_IOD_DEBUG
    printf("MAP Get, axe id %"PRIu64"\n", g_axe_id);
#endif

    /* get the plist pointer */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");

    if(NULL == (output = (map_get_out_t *)H5MM_calloc(sizeof(map_get_out_t))))
	HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate map get output struct");

    /* setup info struct for I/O request. 
       This is to manage the I/O operation once the wait is called. */
    if(NULL == (info = (H5VL_iod_map_io_info_t *)H5MM_calloc(sizeof(H5VL_iod_map_io_info_t))))
        HGOTO_ERROR(H5E_DATASET, H5E_NOSPACE, FAIL, "can't allocate a request");

    /* capture the parameters required to submit a get request again
       if the value type is VL, since the first Get is to retrieve the
       value size */
    info->val_ptr = value;
    info->read_buf = NULL;
    info->value_handle = value_handle;

    info->val_is_vl = val_is_vl;

    /* store the pointer to the buffer where the checksum needs to be placed */
    if(H5P_get(plist, H5D_XFER_CHECKSUM_PTR_NAME, &info->val_cs_ptr) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to get checksum pointer value");
    /* store the raw data integrity scope */
    if(H5P_get(plist, H5VL_CS_BITFLAG_NAME, &info->raw_cs_scope) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to get checksum pointer value");

    /* The value size expected to be received. If VL data, this will
       be 0, because the first call would be to get the value size */
    info->val_size = val_size;
    info->rcxt_id  = rcxt_id;
    info->key.buf_size = key_size;
    info->key.buf = key;
    info->output = output;
    if(val_is_vl) {
        if((info->val_mem_type_id = H5Tcopy(val_mem_type_id)) < 0)
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "unable to copy datatype");
        if((info->key_mem_type_id = H5Tcopy(key_mem_type_id)) < 0)
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "unable to copy datatype");
        if((info->dxpl_id = H5P_copy_plist((H5P_genplist_t *)plist, TRUE)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTINIT, FAIL, "unable to copy dxpl");

        info->peer = PEER;
        info->map_get_id = H5VL_MAP_GET_ID;
    }

    if(H5VL__iod_create_and_forward(H5VL_MAP_GET_ID, HG_MAP_GET, 
                                    (H5VL_iod_object_t *)map, 0,
                                    num_parents, parent_reqs, (H5VL_iod_req_info_t *)rc, 
                                    &input, output, info, req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to create and ship map get");

    if(val_is_vl && (HG_SUCCESS != HG_Bulk_handle_free(dummy_handle))) {
        HGOTO_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "failed to free dummy handle created");
    }
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_map_get() */

herr_t 
H5VL_iod_map_get_types(void *_map, hid_t *key_type_id, hid_t *val_type_id, 
                       hid_t UNUSED rcxt_id, void UNUSED **req)
{
    H5VL_iod_map_t *map = (H5VL_iod_map_t *)_map;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* If there is information needed about the dataset that is not present locally, wait */
    if(-1 == map->remote_map.keytype_id ||
       -1 == map->remote_map.valtype_id) {
        /* Synchronously wait on the request attached to the dataset */
        if(H5VL_iod_request_wait(map->common.file, map->common.request) < 0)
            HGOTO_ERROR(H5E_SYM,  H5E_CANTGET, FAIL, "can't wait on HG request");
        map->common.request = NULL;
    }

    if((*key_type_id = H5Tcopy(map->remote_map.keytype_id)) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_CANTGET, FAIL, "can't get datatype ID of map key")

    if((*val_type_id = H5Tcopy(map->remote_map.valtype_id)) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_CANTGET, FAIL, "can't get datatype ID of map val")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_map_get_types() */

herr_t 
H5VL_iod_map_get_count(void *_map, hsize_t *count, hid_t rcxt_id, void **req)
{
    H5VL_iod_map_t *map = (H5VL_iod_map_t *)_map;
    map_get_count_in_t input;
    H5RC_t *rc = NULL;
    size_t num_parents = 0;
    H5VL_iod_request_t **parent_reqs = NULL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* get the RC object */
    if(NULL == (rc = (H5RC_t *)H5I_object_verify(rcxt_id, H5I_RC)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a READ CONTEXT ID")

    if(NULL == (parent_reqs = (H5VL_iod_request_t **)
                H5MM_malloc(sizeof(H5VL_iod_request_t *))))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate parent req element");

    /* retrieve parent requests */
    if(H5VL_iod_get_parent_requests((H5VL_iod_object_t *)map, (H5VL_iod_req_info_t *)rc, 
                                    parent_reqs, &num_parents) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Failed to retrieve parent requests");

    /* Fill input structure */
    input.coh = map->common.file->remote_file.coh;
    input.iod_oh = map->remote_map.iod_oh;
    input.iod_id = map->remote_map.iod_id;
    input.rcxt_num  = rc->c_version;
    input.cs_scope = map->common.file->md_integrity_scope;

#if H5VL_IOD_DEBUG
    printf("MAP Get count, axe id %"PRIu64"\n", g_axe_id);
#endif

    if(H5VL__iod_create_and_forward(H5VL_MAP_GET_COUNT_ID, HG_MAP_GET_COUNT, 
                                    (H5VL_iod_object_t *)map, 0,
                                    num_parents, parent_reqs,
                                    (H5VL_iod_req_info_t *)rc, &input, count, count, req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to create and ship map get_count");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_map_get_count() */

herr_t 
H5VL_iod_map_exists(void *_map, hid_t key_mem_type_id, const void *key, 
                    hbool_t *exists, hid_t rcxt_id, void **req)
{
    H5VL_iod_map_t *map = (H5VL_iod_map_t *)_map;
    map_op_in_t input;
    size_t key_size;
    H5RC_t *rc = NULL;
    size_t num_parents = 0;
    uint64_t key_cs = 0;
    H5VL_iod_request_t **parent_reqs = NULL;
    H5VL_iod_exists_info_t *info = NULL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    if(H5VL_iod_map_get_size(key_mem_type_id, key, &key_cs, &key_size, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't get key size");

    /* get the RC object */
    if(NULL == (rc = (H5RC_t *)H5I_object_verify(rcxt_id, H5I_RC)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a READ CONTEXT ID")

    if(NULL == (parent_reqs = (H5VL_iod_request_t **)
                H5MM_malloc(sizeof(H5VL_iod_request_t *))))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate parent req element");

    /* retrieve parent requests */
    if(H5VL_iod_get_parent_requests((H5VL_iod_object_t *)map, (H5VL_iod_req_info_t *)rc, 
                                    parent_reqs, &num_parents) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Failed to retrieve parent requests");

    /* Fill input structure */
    input.coh = map->common.file->remote_file.coh;
    input.iod_oh = map->remote_map.iod_oh;
    input.iod_id = map->remote_map.iod_id;
    input.key_maptype_id = map->remote_map.keytype_id;
    input.key_memtype_id = key_mem_type_id;
    input.key.buf_size = key_size;
    input.key.buf = key;
    input.rcxt_num  = rc->c_version;
    input.cs_scope = map->common.file->md_integrity_scope;
    input.trans_num  = 0;

#if H5VL_IOD_DEBUG
    printf("MAP EXISTS, axe id %"PRIu64"\n", g_axe_id);
#endif

    /* setup info struct for exists request. 
       This is to manage the exists operation once the wait is called. */
    if(NULL == (info = (H5VL_iod_exists_info_t *)H5MM_calloc(sizeof(H5VL_iod_exists_info_t))))
        HGOTO_ERROR(H5E_DATASET, H5E_NOSPACE, FAIL, "can't allocate a request");

    info->user_bool = exists;

    if(H5VL__iod_create_and_forward(H5VL_MAP_EXISTS_ID, HG_MAP_EXISTS, 
                                    (H5VL_iod_object_t *)map, 0,
                                    num_parents, parent_reqs,
                                    (H5VL_iod_req_info_t *)rc, &input, 
                                    &info->server_ret, info, req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to create and ship map exists");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_map_exists() */

herr_t 
H5VL_iod_map_iterate(void UNUSED *map, hid_t UNUSED key_mem_type_id, hid_t UNUSED value_mem_type_id, 
                     H5M_iterate_func_t UNUSED callback_func, void UNUSED *context)
{
    //herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5VL_iod_map_iterate() */

herr_t 
H5VL_iod_map_delete(void *_map, hid_t key_mem_type_id, const void *key, 
                    hid_t trans_id, void **req)
{
    H5VL_iod_map_t *map = (H5VL_iod_map_t *)_map;
    map_op_in_t input;
    size_t key_size;
    int *status = NULL;
    size_t num_parents = 0;
    H5TR_t *tr = NULL;
    uint64_t key_cs = 0;
    H5VL_iod_request_t **parent_reqs = NULL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    if(H5VL_iod_map_get_size(key_mem_type_id, key, &key_cs, &key_size, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't get key size");

    /* get the TR object */
    if(NULL == (tr = (H5TR_t *)H5I_object_verify(trans_id, H5I_TR)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a Transaction ID")

    if(NULL == (parent_reqs = (H5VL_iod_request_t **)
                H5MM_malloc(sizeof(H5VL_iod_request_t *) * 2)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate parent req element");

    /* retrieve parent requests */
    if(H5VL_iod_get_parent_requests((H5VL_iod_object_t *)map, (H5VL_iod_req_info_t *)tr, 
                                    parent_reqs, &num_parents) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Failed to retrieve parent requests");

    /* Fill input structure */
    input.coh = map->common.file->remote_file.coh;
    input.iod_oh = map->remote_map.iod_oh;
    input.iod_id = map->remote_map.iod_id;
    input.key_maptype_id = map->remote_map.keytype_id;
    input.key_memtype_id = key_mem_type_id;
    input.key.buf_size = key_size;
    input.key.buf = key;
    input.trans_num = tr->trans_num;
    input.rcxt_num  = tr->c_version;
    input.cs_scope = map->common.file->md_integrity_scope;

#if H5VL_IOD_DEBUG
    printf("MAP DELETE, axe id %"PRIu64"\n", g_axe_id);
#endif

    status = (int *)malloc(sizeof(int));

    if(H5VL__iod_create_and_forward(H5VL_MAP_DELETE_ID, HG_MAP_DELETE, 
                                    (H5VL_iod_object_t *)map, 1,
                                    num_parents, parent_reqs,
                                    (H5VL_iod_req_info_t *)tr, &input, status, status, req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to create and ship map delete");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_map_delete() */

herr_t H5VL_iod_map_close(void *_map, void **req)
{
    H5VL_iod_map_t *map = (H5VL_iod_map_t *)_map;
    map_close_in_t input;
    int *status = NULL;
    size_t num_parents = 0;
    H5VL_iod_request_t **parent_reqs = NULL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* If this call is not asynchronous, complete and remove all
       requests that are associated with this object from the List */
    if(NULL == req) {
        if(H5VL_iod_request_wait_some(map->common.file, map) < 0)
            HGOTO_ERROR(H5E_SYM,  H5E_CANTGET, FAIL, "can't wait on all object requests");
    }

    if(IOD_OH_UNDEFINED == map->remote_map.iod_oh.rd_oh.cookie) {
        /* Synchronously wait on the request attached to the map */
        if(H5VL_iod_request_wait(map->common.file, map->common.request) < 0)
            HGOTO_ERROR(H5E_SYM,  H5E_CANTGET, FAIL, "can't wait on map request");
        map->common.request = NULL;
    }

    if(H5VL_iod_get_obj_requests((H5VL_iod_object_t *)map, &num_parents, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't get num requests");

    if(num_parents) {
        if(NULL == (parent_reqs = (H5VL_iod_request_t **)H5MM_malloc
                    (sizeof(H5VL_iod_request_t *) * num_parents)))
            HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate array of parent reqs");
        if(H5VL_iod_get_obj_requests((H5VL_iod_object_t *)map, &num_parents, 
                                     parent_reqs) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't get parents requests");
    }

    input.iod_oh = map->remote_map.iod_oh;
    input.iod_id = map->remote_map.iod_id;

#if H5VL_IOD_DEBUG
    printf("Map Close IOD ID %"PRIu64", axe id %"PRIu64"\n", input.iod_id, g_axe_id);
#endif

    status = (int *)malloc(sizeof(int));

    if(H5VL__iod_create_and_forward(H5VL_MAP_CLOSE_ID, HG_MAP_CLOSE, 
                                    (H5VL_iod_object_t *)map, 1,
                                    num_parents, parent_reqs,
                                    NULL, &input, status, status, req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to create and ship map close");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_map_close() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_rc_acquire
 *
 * Purpose:	Forwards an acquire for a read context to IOD.
 *
 * Return:	Success:	SUCCEED
 *		Failure:	FAIL
 *
 * Programmer:	Mohamad Chaarawi
 *		September 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t 
H5VL_iod_rc_acquire(H5VL_iod_file_t *file, H5RC_t *rc, uint64_t *c_version,
                    hid_t rcapl_id, void **req)
{
    rc_acquire_in_t input;
    H5VL_iod_rc_info_t *rc_info = NULL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* set the input structure for the HG encode routine */
    input.coh = file->remote_file.coh;
    input.c_version = *c_version;
    input.rcapl_id = rcapl_id;

    /* setup the info structure for updating the RC on completion */
    if(NULL == (rc_info = (H5VL_iod_rc_info_t *)H5MM_calloc(sizeof(H5VL_iod_rc_info_t))))
	HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate RC info struct");

    rc_info->read_cxt = rc;
    rc_info->c_version_ptr = c_version;

#if H5VL_IOD_DEBUG
    printf("Read Context Acquire, version %"PRIu64", axe id %"PRIu64"\n", 
           input.c_version, g_axe_id);
#endif

    if(H5VL__iod_create_and_forward(H5VL_RC_ACQUIRE_ID, HG_RC_ACQUIRE, 
                                    (H5VL_iod_object_t *)file, 0, 0, NULL,
                                    NULL, &input, &rc_info->result, rc_info, req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to create and ship VOL op");

    if(NULL != req) {
        H5VL_iod_request_t *request = (H5VL_iod_request_t *)(*req);

        rc->req_info.request = request;
        //request->ref_count ++;
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_rc_acquire() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_rc_release
 *
 * Purpose:	Forwards an release on an acquired read context to IOD
 *
 * Return:	Success:	SUCCEED
 *		Failure:	FAIL
 *
 * Programmer:	Mohamad Chaarawi
 *		September 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t 
H5VL_iod_rc_release(H5RC_t *rc, void **req)
{
    rc_release_in_t input;
    int *status = NULL;
    size_t num_parents = 0;
    H5VL_iod_request_t **parent_reqs = NULL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (parent_reqs = (H5VL_iod_request_t **)H5MM_malloc
                (sizeof(H5VL_iod_request_t *) * (rc->req_info.num_req + 1))))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate array of parent reqs");

    /* retrieve start request */
    if(H5VL_iod_get_parent_requests(NULL, (H5VL_iod_req_info_t *)rc, parent_reqs, &num_parents) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Failed to retrieve parent requests");

    if(rc->req_info.num_req) {
        H5VL_iod_request_t *cur_req = rc->req_info.head;
        H5VL_iod_request_t *next_req = NULL;
        H5VL_iod_request_t *prev;
        H5VL_iod_request_t *next;

        while(cur_req) {
            /* add a dependency if the current request in the list is pending */
            if(cur_req->state == H5VL_IOD_PENDING) {
                /* If this call is not asynchronous, wait on a dependent request */
                if(NULL == req) {
                    if(H5VL_iod_request_wait(rc->file, cur_req) < 0)
                        HGOTO_ERROR(H5E_SYM,  H5E_CANTGET, FAIL, "can't wait on request");
                }
                /* Otherwise, add a dependency */
                else {
                    parent_reqs[num_parents] = cur_req;
                    cur_req->ref_count ++;
                    num_parents ++;
                }
            }

            next_req = cur_req->trans_next;

            /* remove the current request from the linked list */
            prev = cur_req->trans_prev;
            next = cur_req->trans_next;
            if (prev) {
                if (next) {
                    prev->trans_next = next;
                    next->trans_prev = prev;
                }
                else {
                    prev->trans_next = NULL;
                    rc->req_info.tail = prev;
                }
            }
            else {
                if (next) {
                    next->trans_prev = NULL;
                    rc->req_info.head = next;
                }
                else {
                    rc->req_info.head = NULL;
                    rc->req_info.tail = NULL;
                }
            }

            cur_req->trans_prev = NULL;
            cur_req->trans_next = NULL;

            rc->req_info.num_req --;

            H5VL_iod_request_decr_rc(cur_req);

            cur_req = next_req;
        }
        HDassert(0 == rc->req_info.num_req);
    }

    /* set the input structure for the HG encode routine */
    input.coh = rc->file->remote_file.coh;
    input.c_version = rc->c_version;

    status = (int *)malloc(sizeof(int));

#if H5VL_IOD_DEBUG
    printf("Read Context Release, version %"PRIu64", axe id %"PRIu64"\n", 
           input.c_version, g_axe_id);
#endif

    if(H5VL__iod_create_and_forward(H5VL_RC_RELEASE_ID, HG_RC_RELEASE, 
                                    (H5VL_iod_object_t *)rc->file, 0, 
                                    num_parents, parent_reqs,
                                    NULL, &input, status, status, req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to create and ship VOL op");

    //if(NULL != rc->req_info.request) {
    //H5VL_iod_request_decr_rc(rc->req_info.request);
    //}

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_rc_release() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_rc_persist
 *
 * Purpose:	Forwards an persist on an acquired read context to IOD
 *
 * Return:	Success:	SUCCEED
 *		Failure:	FAIL
 *
 * Programmer:	Mohamad Chaarawi
 *		September 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t 
H5VL_iod_rc_persist(H5RC_t *rc, void **req)
{
    rc_persist_in_t input;
    int *status = NULL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    if(!(rc->file->flags & H5F_ACC_RDWR))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Can't persist a container opened for Read only")

    /* set the input structure for the HG encode routine */
    input.coh = rc->file->remote_file.coh;
    input.c_version = rc->c_version;

    status = (int *)malloc(sizeof(int));

#if H5VL_IOD_DEBUG
    printf("Read Context Persist, version %"PRIu64", axe id %"PRIu64"\n", 
           input.c_version, g_axe_id);
#endif

    if(H5VL__iod_create_and_forward(H5VL_RC_PERSIST_ID, HG_RC_PERSIST, 
                                    (H5VL_iod_object_t *)rc->file, 0, 0, NULL,
                                    NULL, &input, status, status, req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to create and ship VOL op");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_rc_persist() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_rc_snapshot
 *
 * Purpose:	Forwards an snapshot on an acquired read context to IOD
 *
 * Return:	Success:	SUCCEED
 *		Failure:	FAIL
 *
 * Programmer:	Mohamad Chaarawi
 *		September 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t 
H5VL_iod_rc_snapshot(H5RC_t *rc, const char *snapshot_name, void **req)
{
    rc_snapshot_in_t input;
    int *status = NULL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* set the input structure for the HG encode routine */
    input.coh = rc->file->remote_file.coh;
    input.c_version = rc->c_version;
    input.snapshot_name = snapshot_name;

    status = (int *)malloc(sizeof(int));

#if H5VL_IOD_DEBUG
    printf("Read Context Snapshot, version %"PRIu64", axe id %"PRIu64"\n", 
           input.c_version, g_axe_id);
#endif

    if(H5VL__iod_create_and_forward(H5VL_RC_SNAPSHOT_ID, HG_RC_SNAPSHOT, 
                                    (H5VL_iod_object_t *)rc->file, 0, 0, NULL,
                                    NULL, &input, status, status, req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to create and ship VOL op");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_rc_snapshot() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_tr_start
 *
 * Purpose:	Forwards a transaction start call to IOD
 *
 * Return:	Success:	SUCCEED
 *		Failure:	FAIL
 *
 * Programmer:	Mohamad Chaarawi
 *		September 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t 
H5VL_iod_tr_start(H5TR_t *tr, hid_t trspl_id, void **req)
{
    tr_start_in_t input;
    H5VL_iod_tr_info_t *tr_info = NULL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* set the input structure for the HG encode routine */
    input.coh = tr->file->remote_file.coh;
    input.trans_num = tr->trans_num;
    input.trspl_id = trspl_id;

#if H5VL_IOD_DEBUG
    printf("Transaction start, number %"PRIu64", axe id %"PRIu64"\n", 
           input.trans_num, g_axe_id);
#endif

    /* setup the info structure for updating the transaction on completion */
    if(NULL == (tr_info = (H5VL_iod_tr_info_t *)H5MM_calloc(sizeof(H5VL_iod_tr_info_t))))
	HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate TR info struct");

    tr_info->tr = tr;

    if(H5VL__iod_create_and_forward(H5VL_TR_START_ID, HG_TR_START, 
                                    (H5VL_iod_object_t *)tr->file, 0, 0, NULL,
                                    NULL, &input, &tr_info->result, tr_info, req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to create and ship VOL op");

    if(NULL != req) {
        H5VL_iod_request_t *request = (H5VL_iod_request_t *)(*req);

        tr->req_info.request = request;
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_tr_start() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_tr_finish
 *
 * Purpose:	Forwards a transaction finish call to IOD.
 *
 * Return:	Success:	SUCCEED
 *		Failure:	FAIL
 *
 * Programmer:	Mohamad Chaarawi
 *		September 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t 
H5VL_iod_tr_finish(H5TR_t *tr, hbool_t acquire, hid_t trfpl_id, void **req)
{
    tr_finish_in_t input;
    int *status = NULL;
    size_t num_parents = 0;
    H5VL_iod_request_t **parent_reqs = NULL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (parent_reqs = (H5VL_iod_request_t **)H5MM_malloc
                (sizeof(H5VL_iod_request_t *) * (tr->req_info.num_req + 1))))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate array of parent reqs");

    /* retrieve start request */
    if(H5VL_iod_get_parent_requests(NULL, (H5VL_iod_req_info_t *)tr, parent_reqs, &num_parents) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Failed to retrieve parent requests");

    if(tr->req_info.num_req) {
        H5VL_iod_request_t *cur_req = tr->req_info.head;
        H5VL_iod_request_t *next_req = NULL;
        H5VL_iod_request_t *prev;
        H5VL_iod_request_t *next;

        while(cur_req) {
            /* add a dependency if the current request in the list is pending */
            if(cur_req->state == H5VL_IOD_PENDING) {
                /* If this call is not asynchronous, wait on a dependent request */
                if(NULL == req) {
                    if(H5VL_iod_request_wait(tr->file, cur_req) < 0)
                        HGOTO_ERROR(H5E_SYM,  H5E_CANTGET, FAIL, "can't wait on request");
                }
                /* Otherwise, add a dependency */
                else {
                    parent_reqs[num_parents] = cur_req;
                    cur_req->ref_count ++;
                    num_parents ++;
                }
            }

            next_req = cur_req->trans_next;

            /* remove the current request from the linked list */
            prev = cur_req->trans_prev;
            next = cur_req->trans_next;
            if (prev) {
                if (next) {
                    prev->trans_next = next;
                    next->trans_prev = prev;
                }
                else {
                    prev->trans_next = NULL;
                    tr->req_info.tail = prev;
                }
            }
            else {
                if (next) {
                    next->trans_prev = NULL;
                    tr->req_info.head = next;
                }
                else {
                    tr->req_info.head = NULL;
                    tr->req_info.tail = NULL;
                }
            }

            cur_req->trans_prev = NULL;
            cur_req->trans_next = NULL;

            tr->req_info.num_req --;

            H5VL_iod_request_decr_rc(cur_req);

            cur_req = next_req;
        }
        HDassert(0 == tr->req_info.num_req);
    }

    /* set the input structure for the HG encode routine */
    input.coh = tr->file->remote_file.coh;
    input.trans_num = tr->trans_num;
    input.trfpl_id = trfpl_id;
    input.acquire = acquire;

    status = (int *)malloc(sizeof(int));

#if H5VL_IOD_DEBUG
    printf("Transaction Finish, %"PRIu64", axe id %"PRIu64"\n", 
           input.trans_num, g_axe_id);
#endif

    if(H5VL__iod_create_and_forward(H5VL_TR_FINISH_ID, HG_TR_FINISH, 
                                    (H5VL_iod_object_t *)tr->file, 0, 
                                    num_parents, parent_reqs,
                                    NULL, &input, status, status, req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to create and ship VOL op");

    //if(NULL != tr->req_info.request) {
    //H5VL_iod_request_decr_rc(tr->req_info.request);
    //}

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_tr_finish() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_tr_set_dependency
 *
 * Purpose:	Forwards a transaction set dependency to IOD
 *
 * Return:	Success:	SUCCEED
 *		Failure:	FAIL
 *
 * Programmer:	Mohamad Chaarawi
 *		September 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t 
H5VL_iod_tr_set_dependency(H5TR_t *tr, uint64_t trans_num, void **req)
{
    tr_set_depend_in_t input;
    int *status = NULL;
    size_t num_parents = 0;
    H5VL_iod_request_t **parent_reqs = NULL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (parent_reqs = (H5VL_iod_request_t **)
                H5MM_malloc(sizeof(H5VL_iod_request_t *))))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate parent req element");

    /* retrieve parent requests */
    if(H5VL_iod_get_parent_requests(NULL, (H5VL_iod_req_info_t *)tr, parent_reqs, &num_parents) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Failed to retrieve parent requests");

    /* set the input structure for the HG encode routine */
    input.coh = tr->file->remote_file.coh;
    input.child_trans_num = tr->trans_num;
    input.parent_trans_num = trans_num;

#if H5VL_IOD_DEBUG
    printf("Transaction Set Dependency, %"PRIu64" on %"PRIu64" axe id %"PRIu64"\n", 
           input.child_trans_num, input.parent_trans_num, g_axe_id);
#endif

    status = (int *)malloc(sizeof(int));

    if(H5VL__iod_create_and_forward(H5VL_TR_SET_DEPEND_ID, HG_TR_SET_DEPEND, 
                                    (H5VL_iod_object_t *)tr->file, 0, 
                                    num_parents, parent_reqs,
                                    NULL, &input, status, status, req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to create and ship VOL op");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_tr_set_dependency() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_tr_skip
 *
 * Purpose:	Forwards a transaction skip to IOD
 *
 * Return:	Success:	SUCCEED
 *		Failure:	FAIL
 *
 * Programmer:	Mohamad Chaarawi
 *		September 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t 
H5VL_iod_tr_skip(H5VL_iod_file_t *file, uint64_t start_trans_num, uint64_t count, void **req)
{
    tr_skip_in_t input;
    int *status = NULL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* set the input structure for the HG encode routine */
    input.coh = file->remote_file.coh;
    input.start_trans_num = start_trans_num;
    input.count = count;

#if H5VL_IOD_DEBUG
    printf("Transaction Skip, tr %"PRIu64" count %"PRIu64",, axe id %"PRIu64"\n", 
           input.start_trans_num, input.count, g_axe_id);
#endif

    status = (int *)malloc(sizeof(int));

    if(H5VL__iod_create_and_forward(H5VL_TR_SKIP_ID, HG_TR_SKIP, 
                                    (H5VL_iod_object_t *)file, 0, 0, NULL,
                                    NULL, &input, status, status, req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to create and ship VOL op");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_tr_skip() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_tr_abort
 *
 * Purpose:	Forwards a transaction abort to IOD
 *
 * Return:	Success:	SUCCEED
 *		Failure:	FAIL
 *
 * Programmer:	Mohamad Chaarawi
 *		September 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t 
H5VL_iod_tr_abort(H5TR_t *tr, void **req)
{
    tr_abort_in_t input;
    int *status = NULL;
    size_t num_parents = 0;
    H5VL_iod_request_t **parent_reqs = NULL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (parent_reqs = (H5VL_iod_request_t **)
                H5MM_malloc(sizeof(H5VL_iod_request_t *))))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate parent req element");

    /* retrieve parent requests */
    if(H5VL_iod_get_parent_requests(NULL, (H5VL_iod_req_info_t *)tr, parent_reqs, &num_parents) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Failed to retrieve parent requests");

    /* set the input structure for the HG encode routine */
    input.coh = tr->file->remote_file.coh;
    input.trans_num = tr->trans_num;

    /* remove all nodes from the transaction linked list */
    if(tr->req_info.num_req) {
        H5VL_iod_request_t *cur_req = tr->req_info.head;
        H5VL_iod_request_t *next_req = NULL;
        H5VL_iod_request_t *prev;
        H5VL_iod_request_t *next;

        while(cur_req) {
            next_req = cur_req->trans_next;

            prev = cur_req->trans_prev;
            next = cur_req->trans_next;
            if (prev) {
                if (next) {
                    prev->trans_next = next;
                    next->trans_prev = prev;
                }
                else {
                    prev->trans_next = NULL;
                    tr->req_info.tail = prev;
                }
            }
            else {
                if (next) {
                    next->trans_prev = NULL;
                    tr->req_info.head = next;
                }
                else {
                    tr->req_info.head = NULL;
                    tr->req_info.tail = NULL;
                }
            }

            cur_req->trans_prev = NULL;
            cur_req->trans_next = NULL;

            tr->req_info.num_req --;

            H5VL_iod_request_decr_rc(cur_req);

            cur_req = next_req;
        }
        HDassert(0 == tr->req_info.num_req);
    }

    status = (int *)malloc(sizeof(int));

#if H5VL_IOD_DEBUG
    printf("Transaction Abort, tr %"PRIu64", axe id %"PRIu64"\n", 
           input.trans_num, g_axe_id);
#endif

    if(H5VL__iod_create_and_forward(H5VL_TR_ABORT_ID, HG_TR_ABORT, 
                                    (H5VL_iod_object_t *)tr->file, 0, 
                                    num_parents, parent_reqs,
                                    NULL, &input, status, status, req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to create and ship VOL op");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_tr_abort() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_cancel
 *
 * Purpose:	Cancel an asynchronous operation
 *
 * Return:	Success:	SUCCEED
 *		Failure:	FAIL
 *
 * Programmer:	Mohamad Chaarawi
 *		April 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_iod_cancel(void **req, H5ES_status_t *status)
{
    H5VL_iod_request_t *request = *((H5VL_iod_request_t **)req);
    hg_status_t hg_status;
    int         ret;
    H5VL_iod_state_t state;
    hg_request_t hg_req;       /* Local function shipper request, for sync. operations */
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* request has completed already, can not cancel */
    if(request->state == H5VL_IOD_COMPLETED) {
        /* Call the completion function to check return values and free resources */
        if(H5VL_iod_request_complete(request->obj->file, request) < 0)
            fprintf(stderr, "Operation Failed!\n");

        *status = request->status;

        /* free the mercury request */
        request->req = H5MM_xfree(request->req);

        /* Decrement ref count on request */
        H5VL_iod_request_decr_rc(request);
    }

    /* forward the cancel call to the IONs */
    if(HG_Forward(PEER, H5VL_CANCEL_OP_ID, &request->axe_id, &state, &hg_req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to ship attribute write");

    /* Wait on the cancel request to return */
    ret = HG_Wait(hg_req, HG_MAX_IDLE_TIME, &hg_status);

    /* If the actual wait Fails, then the status of the cancel
       operation is unknown */
    if(HG_FAIL == ret)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to wait on cancel request")
    else {
        if(hg_status) {
            /* If the operation to be canceled has already completed,
               mark it so and complete it locally */
            if(state == H5VL_IOD_COMPLETED) {
                if(H5VL_IOD_PENDING == request->state) {
                    if(H5VL_iod_request_wait(request->obj->file, request) < 0)
                        HDONE_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "unable to wait for request");
                }

                *status = request->status;

                /* free the mercury request */
                request->req = H5MM_xfree(request->req);

                /* Decrement ref count on request */
                H5VL_iod_request_decr_rc(request);
            }

            /* if the status returned is cancelled, then cancel it
               locally too */
            else if (state == H5VL_IOD_CANCELLED) {
                request->status = H5ES_STATUS_CANCEL;
                request->state = H5VL_IOD_CANCELLED;
                if(H5VL_iod_request_cancel(request->obj->file, request) < 0)
                    fprintf(stderr, "Operation Failed!\n");

                *status = request->status;

                /* free the mercury request */
                request->req = H5MM_xfree(request->req);

                /* Decrement ref count on request */
                H5VL_iod_request_decr_rc(request);
            }
        }
        else
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Cancel Operation taking too long. Aborting");
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_cancel() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_test
 *
 * Purpose:	Test for an asynchronous operation's completion
 *
 * Return:	Success:	SUCCEED
 *		Failure:	FAIL
 *
 * Programmer:	Quincey Koziol
 *		Wednesday, March 20, 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_iod_test(void **req, H5ES_status_t *status)
{
    H5VL_iod_request_t *request = *((H5VL_iod_request_t **)req);
    hg_status_t hg_status;
    int         ret;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Test completion of the request if is still pending */
    if(H5VL_IOD_PENDING == request->state) {
        ret = HG_Wait(*((hg_request_t *)request->req), 0, &hg_status);
        if(HG_FAIL == ret) {
            fprintf(stderr, "failed to wait on request\n");
            request->status = H5ES_STATUS_FAIL;
            request->state = H5VL_IOD_COMPLETED;

            /* remove the request from the file linked list */
            H5VL_iod_request_delete(request->obj->file, request);

            /* free the mercury request */
            request->req = H5MM_xfree(request->req);

            /* Decrement ref count on request */
            H5VL_iod_request_decr_rc(request);
        }
        else {
            if(hg_status) {
                request->status = H5ES_STATUS_SUCCEED;
                request->state = H5VL_IOD_COMPLETED;

                /* Call the completion function to check return values and free resources */
                if(H5VL_iod_request_complete(request->obj->file, request) < 0)
                    fprintf(stderr, "Operation Failed!\n");

                *status = request->status;

                /* free the mercury request */
                request->req = H5MM_xfree(request->req);

                /* Decrement ref count on request */
                H5VL_iod_request_decr_rc(request);
            }
            /* request has not finished, set return status appropriately */
            else
                *status = request->status;
        }
    }
    else {
        *status = request->status;
        /* free the mercury request */
        request->req = H5MM_xfree(request->req);

        /* Decrement ref count on request */
        H5VL_iod_request_decr_rc(request);
    }

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5VL_iod_test() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_wait
 *
 * Purpose:	Wait for an asynchronous operation to complete
 *
 * Return:	Success:	SUCCEED
 *		Failure:	FAIL
 *
 * Programmer:	Quincey Koziol
 *		Wednesday, March 20, 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_iod_wait(void **req, H5ES_status_t *status)
{
    H5VL_iod_request_t *request = *((H5VL_iod_request_t **)req);
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* Wait on completion of the request if it was not completed */
    if(H5VL_IOD_PENDING == request->state) {
        if(H5VL_iod_request_wait(request->obj->file, request) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "unable to wait for request")
    }

    *status = request->status;

    /* free the mercury request */
    request->req = H5MM_xfree(request->req);

    /* Decrement ref count on request */
    H5VL_iod_request_decr_rc(request);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_wait() */

#endif /* H5_HAVE_EFF */
