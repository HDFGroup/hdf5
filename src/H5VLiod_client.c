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

/* Programmer:  Mohamad Chaarawi <chaarawi@hdfgroup.org>
 *
 * Purpose:	IOD plugin client code
 */

#define H5G_PACKAGE		/*suppress error about including H5Gpkg   */
#define H5D_PACKAGE		/*suppress error about including H5Dpkg   */

#include "H5private.h"		/* Generic Functions			*/
#include "H5Dpkg.h"		/* Datasets		  		*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Gpkg.h"		/* Groups		  		*/
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Pprivate.h"		/* Property lists			*/
#include "H5Sprivate.h"		/* Dataspaces		  		*/
#include "H5VLprivate.h"	/* VOL plugins				*/
#include "H5VLiod.h"            /* Iod VOL plugin			*/
#include "H5VLiod_common.h"
#include "H5VLiod_client.h"
#include "H5Xprivate.h"
#include "H5WBprivate.h"        /* Wrapped Buffers                      */

#ifdef H5_HAVE_EFF

H5FL_EXTERN(H5VL_iod_file_t);
H5FL_EXTERN(H5VL_iod_attr_t);
H5FL_EXTERN(H5VL_iod_group_t);
H5FL_EXTERN(H5VL_iod_dset_t);
H5FL_EXTERN(H5VL_iod_dtype_t);

/* H5Diterate op-data for VL traversal */
typedef struct {
    size_t buf_size;
    uint8_t *buf_ptr;
    void **off;
    size_t *len;
    int curr_seq;
    size_t *str_len; /* used only for VL strings */
} H5VL_iod_pre_write_t;

static herr_t H5VL__iod_vl_map_get_finalize(size_t buf_size, void *read_buf, void *user_buf, 
                                            hid_t mem_type_id);

herr_t 
H5VL_iod_request_decr_rc(H5VL_iod_request_t *request)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    request->ref_count --;

    if(0 == request->ref_count) {
        //request->parent_reqs = (H5VL_iod_request_t **)H5MM_xfree(request->parent_reqs);
        request = (H5VL_iod_request_t *)H5MM_xfree(request);
    }

    FUNC_LEAVE_NOAPI(SUCCEED)
}


/*-------------------------------------------------------------------------
 * Function:    H5VL_iod_request_add
 *
 * Purpose:     Adds a request pointer to the Doubly linked list on the
 *              file.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_iod_request_add(H5VL_iod_file_t *file, H5VL_iod_request_t *request)
{
    H5VL_iod_req_info_t *req_info = request->trans_info;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(request);

    if (file->request_list_tail) {
        file->request_list_tail->file_next = request;
        request->file_prev = file->request_list_tail;
        file->request_list_tail = request;
    }
    else {
        file->request_list_head = request;
        file->request_list_tail = request;
        request->file_prev = NULL;
    }
    request->file_next = NULL;
    file->num_req ++;

    if(req_info) {
        if (req_info->tail) {
            req_info->tail->trans_next = request;
            request->trans_prev = req_info->tail;
            req_info->tail = request;
        }
        else {
            req_info->head = request;
            req_info->tail = request;
            request->trans_prev = NULL;
        }
        request->trans_next = NULL;
        req_info->num_req ++;

        request->ref_count ++;
    }

    FUNC_LEAVE_NOAPI(SUCCEED)
}


/*-------------------------------------------------------------------------
 * Function:    H5VL_iod_request_delete
 *
 * Purpose:     Removes a request pointer from the Doubly linked list on the
 *              file.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_iod_request_delete(H5VL_iod_file_t *file, H5VL_iod_request_t *request)
{
    H5VL_iod_request_t *prev;
    H5VL_iod_request_t *next;
    unsigned u;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(request);

    /* decrement ref count on parent requests */
    for(u=0 ; u<request->num_parents ; u++) {
        /* Decrement ref count on request */
        H5VL_iod_request_decr_rc(request->parent_reqs[u]);
    }

    request->parent_reqs = (H5VL_iod_request_t **)H5MM_xfree(request->parent_reqs);

    /* remove the request from the container link list */
    prev = request->file_prev;
    next = request->file_next;
    if (prev) {
        if (next) {
            prev->file_next = next;
            next->file_prev = prev;
        }
        else {
            prev->file_next = NULL;
            file->request_list_tail = prev;
        }
    }
    else {
        if (next) {
            next->file_prev = NULL;
            file->request_list_head = next;
        }
        else {
            file->request_list_head = NULL;
            file->request_list_tail = NULL;
        }
    }

    if(request == request->obj->request)
        request->obj->request = NULL;
    request->file_prev = NULL;
    request->file_next = NULL;

    file->num_req --;

    FUNC_LEAVE_NOAPI(SUCCEED)
}


/*-------------------------------------------------------------------------
 * Function:    H5VL_iod_request_wait
 *
 * Purpose: 
 *    Waits for a particular request to complete. This will test
 *    the request completion using Mercury's test routine. If the
 *    request is still pending we test for completion of other requests in
 *    the file's linked list to try and keep making progress. Once the
 *    original requests completes, we remove it from the linked list 
 *    and return.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_iod_request_wait(H5VL_iod_file_t *file, H5VL_iod_request_t *request)
{
    H5VL_iod_request_t *cur_req = file->request_list_head;
    int ret;
    hg_status_t status;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(request);
    HDassert(request->req);

    /* Loop to complete the request while poking through other requests on the 
       container to avoid deadlock. */
    while(1) {
        HDassert(request->state == H5VL_IOD_PENDING);
        /* test the operation status */
        ret = HG_Wait(*((hg_request_t *)request->req), 0, &status);
        if(HG_FAIL == ret) {
            HERROR(H5E_FUNC, H5E_CANTINIT, "failed to wait on request\n");
            request->status = H5ES_STATUS_FAIL;
            request->state = H5VL_IOD_COMPLETED;
            H5VL_iod_request_delete(file, request);
            break;
        }
        else {
            if(status) {
                request->status = H5ES_STATUS_SUCCEED;
                request->state = H5VL_IOD_COMPLETED;
            }
        }

        /* if it has not completed, go through the list of requests on the container to
           test progress */
        if(!status) {
            H5VL_iod_request_t *tmp_req = NULL;

            if(cur_req) {
                if(HG_FILE_CLOSE != cur_req->type && cur_req->req != request->req) {
                    hg_status_t tmp_status;

                    tmp_req = cur_req->file_next;

                    HDassert(cur_req->state == H5VL_IOD_PENDING);
                    ret = HG_Wait(*((hg_request_t *)cur_req->req), 0, &tmp_status);
                    if(HG_FAIL == ret) {
                        HERROR(H5E_FUNC, H5E_CANTINIT, "failed to wait on request\n");
                        cur_req->status = H5ES_STATUS_FAIL;
                        cur_req->state = H5VL_IOD_COMPLETED;
                        H5VL_iod_request_delete(file, cur_req);
                    }
                    else {
                        if(tmp_status) {
                            cur_req->status = H5ES_STATUS_SUCCEED;
                            cur_req->state = H5VL_IOD_COMPLETED;
                            if(H5VL_iod_request_complete(file, cur_req) < 0) {
                                HERROR(H5E_FUNC, H5E_CANTINIT, "Operation %"PRIu64" Failed!\n", cur_req->axe_id);
                            }
                        }
                    }
                }
                /* next time, test the next request in the list */
                cur_req = tmp_req;
            }
        }
        /* request complete, remove it from list & break */
        else {
            if(H5VL_iod_request_complete(file, request) < 0) {
                HERROR(H5E_FUNC, H5E_CANTINIT, "Operation %"PRIu64" Failed!\n", request->axe_id);
            }
            break;
        }
    }

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5VL_iod_wait */


/*-------------------------------------------------------------------------
 * Function:    H5VL_iod_request_wait_all
 *
 * Purpose:     Wait and complete all the requests in the linked list.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_iod_request_wait_all(H5VL_iod_file_t *file)
{
    H5VL_iod_request_t *cur_req = file->request_list_head;
    hg_status_t status;
    int ret;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Loop to complete all requests */
    while(cur_req) {
        H5VL_iod_request_t *tmp_req = NULL;

        tmp_req = cur_req->file_next;

        HDassert(cur_req->state == H5VL_IOD_PENDING);
        ret = HG_Wait(*((hg_request_t *)cur_req->req), HG_MAX_IDLE_TIME, &status);
        if(HG_FAIL == ret) {
            HERROR(H5E_FUNC, H5E_CANTINIT, "failed to wait on request\n");
            cur_req->status = H5ES_STATUS_FAIL;
            cur_req->state = H5VL_IOD_COMPLETED;
        }
        else {
            if(!status) {
                HERROR(H5E_FUNC, H5E_CANTINIT, "Wait timeout reached\n");
                cur_req->status = H5ES_STATUS_FAIL;
                cur_req->state = H5VL_IOD_COMPLETED;
                H5VL_iod_request_delete(file, cur_req);
                goto done;
            }
            else {
                cur_req->status = H5ES_STATUS_SUCCEED;
                cur_req->state = H5VL_IOD_COMPLETED;
            }
        }

        if(H5VL_iod_request_complete(file, cur_req) < 0)
            HERROR(H5E_FUNC, H5E_CANTINIT, "Operation %"PRIu64" Failed!\n", cur_req->axe_id);

        cur_req = tmp_req;
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_request_wait_all */


/*-------------------------------------------------------------------------
 * Function:    H5VL_iod_request_wait_some
 *
 * Purpose:     Wait for some requests on the linked list, particularly 
 *              the ones that are tracked with a particular object.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_iod_request_wait_some(H5VL_iod_file_t *file, const void *object)
{
    H5VL_iod_request_t *cur_req = file->request_list_head;
    hg_status_t status;
    int ret;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Loop to complete some requests */
    while(cur_req) {
        H5VL_iod_request_t *tmp_req;

        tmp_req = cur_req->file_next;

        /* If the request is pending on the object we want, complete it */
        if(cur_req->obj == object && 
           cur_req->state == H5VL_IOD_PENDING) {
            ret = HG_Wait(*((hg_request_t *)cur_req->req), HG_MAX_IDLE_TIME, 
                          &status);
            if(HG_FAIL == ret) {
                HERROR(H5E_FUNC, H5E_CANTINIT, "failed to wait on request\n");
                cur_req->status = H5ES_STATUS_FAIL;
                cur_req->state = H5VL_IOD_COMPLETED;
                H5VL_iod_request_delete(file, cur_req);
            }
            else {
                if(!status) {
                    HERROR(H5E_FUNC, H5E_CANTINIT, "Wait timeout reached\n");
                    cur_req->status = H5ES_STATUS_FAIL;
                    cur_req->state = H5VL_IOD_COMPLETED;
                    H5VL_iod_request_delete(file, cur_req);
                }
                else {
                    cur_req->status = H5ES_STATUS_SUCCEED;
                    cur_req->state = H5VL_IOD_COMPLETED;
                    if(H5VL_iod_request_complete(file, cur_req) < 0)
                        HERROR(H5E_FUNC, H5E_CANTINIT, "Operation %"PRIu64" Failed!\n", cur_req->axe_id);
                }
            }
        }
        cur_req = tmp_req;
    }

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_request_wait_some */


/*-------------------------------------------------------------------------
 * Function:    H5VL_iod_request_complete
 *
 * Purpose:     Completion calls for every type of request. This checks 
 *              the return status from the server, and frees memory 
 *              allocated by this request.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_iod_request_complete(H5VL_iod_file_t *file, H5VL_iod_request_t *req)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(req->state == H5VL_IOD_COMPLETED);

    switch(req->type) {
    case HG_ANALYSIS_EXECUTE:
        {
            analysis_execute_out_t *output = (analysis_execute_out_t *)req->data;

            if(SUCCEED != output->ret) {
                HERROR(H5E_FUNC, H5E_CANTINIT, "Analysis Execute failed at the server\n");
                req->status = H5ES_STATUS_FAIL;
                req->state = H5VL_IOD_COMPLETED;
            }
            free(output);
            req->data = NULL;
            H5VL_iod_request_delete(file, req);
            break;
        }
    case HG_FILE_CREATE:
        if(IOD_OH_UNDEFINED == req->obj->file->remote_file.coh.cookie) {
            HERROR(H5E_FUNC, H5E_CANTINIT, "failed to create file\n");
            req->status = H5ES_STATUS_FAIL;
            req->state = H5VL_IOD_COMPLETED;
        }

        MPI_Barrier (file->comm);
        H5VL_iod_request_delete(file, req);
        break;
    case HG_FILE_OPEN:
        if(IOD_OH_UNDEFINED == req->obj->file->remote_file.coh.cookie) {
            HERROR(H5E_FUNC, H5E_CANTINIT, "failed to open file\n");
            req->status = H5ES_STATUS_FAIL;
            req->state = H5VL_IOD_COMPLETED;
        }
        else {
            H5P_genplist_t *plist = NULL;      /* Property list pointer */
            hid_t rcxt_id;

            if(NULL == (plist = H5P_object_verify(file->fapl_id, H5P_FILE_ACCESS)))
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access property list")

            /* determine if we want to update the latest readable version
               when the file is opened */
            if(H5P_get(plist, H5VL_ACQUIRE_RC_ID, &rcxt_id) < 0)
                HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't set property value for rxct id");

            if(FAIL != rcxt_id) {
                H5RC_t *rc = NULL;
                /* get the RC object */
                if(NULL == (rc = (H5RC_t *)H5I_object_verify(rcxt_id, H5I_RC)))
                    HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a READ CONTEXT ID");

                rc->c_version = file->remote_file.c_version;
                rc->file = file;
            }

            /* increment ref count on ID generated by Mercury encoding callback */
            if(H5I_inc_ref(req->obj->file->remote_file.fcpl_id, TRUE) < 0)
                HGOTO_ERROR(H5E_ATOM, H5E_CANTINC, FAIL, "can't increment ID ref count");
        }

        H5VL_iod_request_delete(file, req);
        break;
    case HG_ATTR_CREATE:
        {
            H5VL_iod_attr_t *attr = (H5VL_iod_attr_t *)req->obj;

            if(IOD_OH_UNDEFINED == attr->remote_attr.iod_oh.rd_oh.cookie) {
                HERROR(H5E_FUNC, H5E_CANTINIT, "failed to create Attribute\n");
                req->status = H5ES_STATUS_FAIL;
                req->state = H5VL_IOD_COMPLETED;
            }

            H5VL_iod_request_delete(file, req);
            break;
        }
    case HG_ATTR_OPEN:
        {
            H5VL_iod_attr_t *attr = (H5VL_iod_attr_t *)req->obj;

            if(IOD_OH_UNDEFINED == attr->remote_attr.iod_oh.rd_oh.cookie) {
                HERROR(H5E_FUNC, H5E_CANTINIT, "failed to open Attribute\n");
                req->status = H5ES_STATUS_FAIL;
                req->state = H5VL_IOD_COMPLETED;
            }
            else {
                /* increment ref count on IDs generated by Mercury encoding callback */
                if(H5I_inc_ref(attr->remote_attr.type_id, TRUE) < 0)
                    HGOTO_ERROR(H5E_ATOM, H5E_CANTINC, FAIL, "can't increment ID ref count");
                if(H5I_inc_ref(attr->remote_attr.space_id, TRUE) < 0)
                    HGOTO_ERROR(H5E_ATOM, H5E_CANTINC, FAIL, "can't increment ID ref count");
            }
            H5VL_iod_request_delete(file, req);
            break;
        }
    case HG_GROUP_CREATE:
        {
            H5VL_iod_group_t *group = (H5VL_iod_group_t *)req->obj;

            if(IOD_OH_UNDEFINED == group->remote_group.iod_oh.rd_oh.cookie) {
                HERROR(H5E_FUNC, H5E_CANTINIT, "failed to create Group\n");
                req->status = H5ES_STATUS_FAIL;
                req->state = H5VL_IOD_COMPLETED;
            }

            H5VL_iod_request_delete(file, req);
            break;
        }
    case HG_GROUP_OPEN:
        {
            H5VL_iod_group_t *group = (H5VL_iod_group_t *)req->obj;

            if(IOD_OH_UNDEFINED == group->remote_group.iod_oh.rd_oh.cookie) {
                HERROR(H5E_FUNC, H5E_CANTINIT, "failed to open Group\n");
                req->status = H5ES_STATUS_FAIL;
                req->state = H5VL_IOD_COMPLETED;
            }
            else {
                /* increment ref count on IDs generated by Mercury encoding callback */
                if(H5I_inc_ref(group->remote_group.gcpl_id, TRUE) < 0)
                    HGOTO_ERROR(H5E_ATOM, H5E_CANTINC, FAIL, "can't increment ID ref count");
            }

            H5VL_iod_request_delete(file, req);
            break;
        }
    case HG_MAP_CREATE:
        {
            H5VL_iod_map_t *map = (H5VL_iod_map_t *)req->obj;

            if(IOD_OH_UNDEFINED == map->remote_map.iod_oh.rd_oh.cookie) {
                HERROR(H5E_FUNC, H5E_CANTINIT, "failed to create Map\n");
                req->status = H5ES_STATUS_FAIL;
                req->state = H5VL_IOD_COMPLETED;
            }

            H5VL_iod_request_delete(file, req);
            break;
        }
    case HG_MAP_OPEN:
        {
            H5VL_iod_map_t *map = (H5VL_iod_map_t *)req->obj;

            if(IOD_OH_UNDEFINED == map->remote_map.iod_oh.rd_oh.cookie) {
                HERROR(H5E_FUNC, H5E_CANTINIT, "failed to open Map\n");
                req->status = H5ES_STATUS_FAIL;
                req->state = H5VL_IOD_COMPLETED;
            }
            else {
                /* increment ref count on IDs generated by Mercury encoding callback */
                if(H5I_inc_ref(map->remote_map.mcpl_id, TRUE) < 0)
                    HGOTO_ERROR(H5E_ATOM, H5E_CANTINC, FAIL, "can't increment ID ref count");
                if(H5I_inc_ref(map->remote_map.keytype_id, TRUE) < 0)
                    HGOTO_ERROR(H5E_ATOM, H5E_CANTINC, FAIL, "can't increment ID ref count");
                if(H5I_inc_ref(map->remote_map.valtype_id, TRUE) < 0)
                    HGOTO_ERROR(H5E_ATOM, H5E_CANTINC, FAIL, "can't increment ID ref count");
            }

            H5VL_iod_request_delete(file, req);
            break;
        }
    case HG_DSET_CREATE:
        {
            H5VL_iod_dset_t *dset = (H5VL_iod_dset_t *)req->obj;

            if(IOD_OH_UNDEFINED == dset->remote_dset.iod_oh.rd_oh.cookie) {
                HERROR(H5E_FUNC, H5E_CANTINIT, "failed to create Dataset\n");
                req->status = H5ES_STATUS_FAIL;
                req->state = H5VL_IOD_COMPLETED;
            }

            H5VL_iod_request_delete(file, req);
            break;
        }
    case HG_DSET_OPEN:
        {
            H5VL_iod_dset_t *dset = (H5VL_iod_dset_t *)req->obj;

            if(IOD_OH_UNDEFINED == dset->remote_dset.iod_oh.rd_oh.cookie) {
                HERROR(H5E_FUNC, H5E_CANTINIT, "failed to open Dataset\n");
                req->status = H5ES_STATUS_FAIL;
                req->state = H5VL_IOD_COMPLETED;
            }
            else {
                /* increment ref count on IDs generated by Mercury encoding callback */
                if(H5I_inc_ref(dset->remote_dset.dcpl_id, TRUE) < 0)
                    HGOTO_ERROR(H5E_ATOM, H5E_CANTINC, FAIL, "can't increment ID ref count");
                if(H5I_inc_ref(dset->remote_dset.type_id, TRUE) < 0)
                    HGOTO_ERROR(H5E_ATOM, H5E_CANTINC, FAIL, "can't increment ID ref count");
                if(H5I_inc_ref(dset->remote_dset.space_id, TRUE) < 0)
                    HGOTO_ERROR(H5E_ATOM, H5E_CANTINC, FAIL, "can't increment ID ref count");
            }

            H5VL_iod_request_delete(file, req);
            break;
        }
    case HG_DTYPE_COMMIT:
        {
            H5VL_iod_dtype_t *dtype = (H5VL_iod_dtype_t *)req->obj;

            if(IOD_OH_UNDEFINED == dtype->remote_dtype.iod_oh.rd_oh.cookie) {
                HERROR(H5E_FUNC, H5E_CANTINIT, "failed to commit Datatype\n");
                req->status = H5ES_STATUS_FAIL;
                req->state = H5VL_IOD_COMPLETED;
            }

            H5VL_iod_request_delete(file, req);
            break;
        }
    case HG_DTYPE_OPEN:
        {
            H5VL_iod_dtype_t *dtype = (H5VL_iod_dtype_t *)req->obj;

            if(IOD_OH_UNDEFINED == dtype->remote_dtype.iod_oh.rd_oh.cookie) {
                HERROR(H5E_FUNC, H5E_CANTINIT, "failed to open Datatype\n");
                req->status = H5ES_STATUS_FAIL;
                req->state = H5VL_IOD_COMPLETED;
            }
            else {
                /* increment ref count on IDs generated by Mercury encoding callback */
                if(H5I_inc_ref(dtype->remote_dtype.tcpl_id, TRUE) < 0)
                    HGOTO_ERROR(H5E_ATOM, H5E_CANTINC, FAIL, "can't increment ID ref count");
                if(H5I_inc_ref(dtype->remote_dtype.type_id, TRUE) < 0)
                    HGOTO_ERROR(H5E_ATOM, H5E_CANTINC, FAIL, "can't increment ID ref count");
            }

            H5VL_iod_request_delete(file, req);
            break;
        }
    case HG_DSET_WRITE:
        {
            H5VL_iod_write_info_t *info = (H5VL_iod_write_info_t *)req->data;

            /* Free memory handle */
            if(HG_SUCCESS != HG_Bulk_handle_free(*info->bulk_handle)) {
                HERROR(H5E_FUNC, H5E_CANTINIT, "failed to free dataset bulk handle\n");
                req->status = H5ES_STATUS_FAIL;
                req->state = H5VL_IOD_COMPLETED;
            }

            if(*info->vl_len_bulk_handle != HG_BULK_NULL && 
               HG_SUCCESS != HG_Bulk_handle_free(*info->vl_len_bulk_handle)) {
                fprintf(stderr, "failed to free dataset bulk handle\n");
                req->status = H5ES_STATUS_FAIL;
                req->state = H5VL_IOD_COMPLETED;
            }

            if(SUCCEED != *((int *)info->status)) {
                HERROR(H5E_FUNC, H5E_CANTINIT, "Errrr! Dataset Write Failure Reported from Server\n");
                req->status = H5ES_STATUS_FAIL;
                req->state = H5VL_IOD_COMPLETED;
            }

            H5VL_iod_request_delete(file, req);

#ifdef H5_HAVE_INDEXING
            /* check whether index needs to be updated or not */
            if (info->idx_handle) {
                H5X_class_t *idx_class = NULL;
                hid_t xxpl_id = H5P_INDEX_XFER_DEFAULT;
                H5P_genplist_t *xxpl_plist; /* Property list pointer */

                /* Get the index plugin class */
                if (NULL == (idx_class = H5X_registered(info->idx_plugin_id)))
                    HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get index plugin class");

                /* Store the transaction ID in the xxpl */
                if(NULL == (xxpl_plist = (H5P_genplist_t *)H5I_object(xxpl_id)))
                    HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");
                if(H5P_set(xxpl_plist, H5VL_TRANS_ID, &info->trans_id) < 0)
                    HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't set property value for trans_id");

                /* Call post_update */
                if (NULL == idx_class->post_update)
                    HGOTO_ERROR(H5E_INDEX, H5E_BADVALUE, FAIL, "plugin post_update callback is not defined");
                if (FAIL == idx_class->post_update(info->idx_handle,
                        info->buf, info->dataspace_id, xxpl_id))
                    HGOTO_ERROR(H5E_INDEX, H5E_CANTUPDATE, FAIL, "unable to issue index post_update operation");
            }
#endif

            if(info->vl_segments) {
                free(info->vl_segments);
                info->vl_segments = NULL;
            }
            if(info->vl_lengths) {
                free(info->vl_lengths);
                info->vl_lengths = NULL;
            }

            free(info->status);
            info->status = NULL;
            info->bulk_handle = (hg_bulk_t *)H5MM_xfree(info->bulk_handle);
            info->vl_len_bulk_handle = (hg_bulk_t *)H5MM_xfree(info->vl_len_bulk_handle);
            info = (H5VL_iod_write_info_t *)H5MM_xfree(info);
            req->data = NULL;
            break;
        }
    case HG_DSET_READ:
        {
            H5VL_iod_read_info_t *info = (H5VL_iod_read_info_t *)req->data;
            H5VL_iod_read_status_t *read_status = (H5VL_iod_read_status_t *)info->status;

            /* Free memory handle */
            if(HG_SUCCESS != HG_Bulk_handle_free(*info->bulk_handle)) {
                HERROR(H5E_FUNC, H5E_CANTINIT, "failed to free dataset bulk handle\n");
                req->status = H5ES_STATUS_FAIL;
                req->state = H5VL_IOD_COMPLETED;
            }
            if(SUCCEED != read_status->ret) {
                HERROR(H5E_FUNC, H5E_CANTINIT, "Errrrr!  Dataset Read Failure Reported from Server\n");
                req->status = H5ES_STATUS_FAIL;
                req->state = H5VL_IOD_COMPLETED;
            }
            else {
                uint32_t raw_cs_scope = info->raw_cs_scope;

                if(raw_cs_scope) {
                    uint64_t internal_cs = 0;

                    /* calculate a checksum for the data recieved */
                    internal_cs = H5S_checksum(info->buf_ptr, info->type_size, 
                                               (size_t)info->nelmts, info->space);

                    /* verify data integrity */
                    if((raw_cs_scope & H5_CHECKSUM_TRANSFER) &&
                       internal_cs != read_status->cs) {
                        HERROR(H5E_FUNC, H5E_CANTINIT, 
                                "Errrrr!  Dataset Read integrity failure (expecting %"PRIu64" got %"PRIu64").\n",
                                read_status->cs, internal_cs);
                        req->status = H5ES_STATUS_FAIL;
                        req->state = H5VL_IOD_COMPLETED;
                    }
#if H5VL_IOD_DEBUG
                    if(!raw_cs_scope & H5_CHECKSUM_TRANSFER) {
                        printf("NO TRANSFER DATA INTEGRITY CHECKS ON RAW DATA READ\n");
                    }
#endif

                    /* If the app gave us a buffer to store the checksum, then put it there */
                    if(info->cs_ptr)
                        *info->cs_ptr = internal_cs;
                }
            }

            H5VL_iod_type_info_reset(info->type_info);
            info->type_info = (H5VL_iod_type_info_t *)H5MM_xfree(info->type_info);
            if(info->space && H5S_close(info->space) < 0)
                HDONE_ERROR(H5E_DATASPACE, H5E_CANTRELEASE, FAIL, "unable to release dataspace");
            read_status = (H5VL_iod_read_status_t *)H5MM_xfree(read_status);
            info->bulk_handle = (hg_bulk_t *)H5MM_xfree(info->bulk_handle);
            info = (H5VL_iod_read_info_t *)H5MM_xfree(info);
            req->data = NULL;
            H5VL_iod_request_delete(file, req);
            break;
        }
    case HG_DSET_GET_VL_SIZE:
        {
            H5VL_iod_read_info_t *info = (H5VL_iod_read_info_t *)req->data;
            H5VL_iod_read_status_t *status = (H5VL_iod_read_status_t *)info->status;

            /* Free memory handle */
            if(HG_SUCCESS != HG_Bulk_handle_free(*info->bulk_handle) || 
               SUCCEED != status->ret) {

                HERROR(H5E_FUNC, H5E_CANTINIT, "Errrrr!  Dataset Read Failure Reported from Server\n");
                req->status = H5ES_STATUS_FAIL;
                req->state = H5VL_IOD_COMPLETED;

                if(H5Sclose(info->file_space_id) < 0)
                    HGOTO_ERROR(H5E_DATASPACE, H5E_CANTRELEASE, FAIL, "unable to release dataspace");
                if(H5Tclose(info->mem_type_id) < 0)
                    HGOTO_ERROR(H5E_DATATYPE, H5E_CANTRELEASE, FAIL, "unable to release datatype");
                if(H5Pclose(info->dxpl_id) < 0)
                    HGOTO_ERROR(H5E_PLIST, H5E_CANTRELEASE, FAIL, "unable to release plist");
                if(info->space && H5S_close(info->space) < 0)
                    HDONE_ERROR(H5E_DATASPACE, H5E_CANTRELEASE, FAIL, "unable to release dataspace");
                status = (H5VL_iod_read_status_t *)H5MM_xfree(status);
                info->bulk_handle = (hg_bulk_t *)H5MM_xfree(info->bulk_handle);
                info = (H5VL_iod_read_info_t *)H5MM_xfree(info);
            }
            else {
                dset_io_in_t input;
                H5VL_iod_dset_t *dset = (H5VL_iod_dset_t *)req->obj;
                hid_t rcxt_id;
                H5RC_t *rc;
                H5P_genplist_t *plist = NULL;
                hg_bulk_segment_t *segments = NULL;
                size_t num_segments = 0;
                hg_request_t hg_req;
                hg_status_t hg_status;

                /* MSC - Need to fix this to allow for nested VLs */
                HDassert(info->vl_lengths_size == status->buf_size);

                /* Create segments from vl lengths */
                if(H5VL_iod_create_segments_recv((char *)info->buf_ptr, info->type_info, 
                                                 (size_t)info->nelmts, &segments, &num_segments, 
                                                 info->vl_lengths, info->vl_lengths_size, 
                                                 NULL, NULL) < 0)
                    HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "can't create segments for bulk data transfer");
                HDassert(segments);

                /* Register non-contiguous memory segments */
                if(HG_SUCCESS != HG_Bulk_handle_create_segments(segments, num_segments, 
                                                                HG_BULK_READWRITE, 
                                                                info->bulk_handle))
                    HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "can't create Bulk Data handle");

                /* get the context ID */
                if(NULL == (plist = (H5P_genplist_t *)H5I_object(info->dxpl_id)))
                    HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");
                if(H5P_get(plist, H5VL_CONTEXT_ID, &rcxt_id) < 0)
                    HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get property value for trans_id");

                /* get the RC object */
                if(NULL == (rc = (H5RC_t *)H5I_object_verify(rcxt_id, H5I_RC)))
                    HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a READ CONTEXT ID");

                /* Fill input structure for reading data */
                input.coh = file->remote_file.coh;
                input.iod_oh = dset->remote_dset.iod_oh;
                input.iod_id = dset->remote_dset.iod_id;
                input.mdkv_id = dset->remote_dset.mdkv_id;
                input.bulk_handle = *info->bulk_handle;
                input.vl_len_bulk_handle = HG_BULK_NULL;
                input.checksum = 0;
                input.dxpl_id = info->dxpl_id;
                input.space_id = info->file_space_id;
                input.mem_type_id = info->mem_type_id;
                input.dset_type_id = dset->remote_dset.type_id;
                input.rcxt_num  = rc->c_version;
                input.cs_scope = dset->common.file->md_integrity_scope;
                input.trans_num = 0;
                input.axe_id = info->axe_id + 1;
                input.axe_info.axe_id = info->axe_id;
                input.axe_info.start_range = 0;
                input.axe_info.count = 0;
                input.axe_info.num_parents = 0;
                input.axe_info.parent_axe_ids = NULL;

                /* forward the call to the ION */
                if(HG_Forward(info->ion_target, info->read_id, &input, info->status, &hg_req) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to ship operation");

                if(HG_FAIL ==  HG_Wait(hg_req, HG_MAX_IDLE_TIME, &hg_status)) {
                    fprintf(stderr, "failed to wait on request\n");
                    req->status = H5ES_STATUS_FAIL;
                    req->state = H5VL_IOD_COMPLETED;
                }

                HG_Request_free(hg_req);
                HG_Bulk_handle_free(*info->bulk_handle);

                if(segments) {
                    free(segments);
                    segments = NULL;
                }
            }

            if(info->vl_lengths) {
                HDfree(info->vl_lengths);
                info->vl_lengths = NULL;
            }
            if(H5Sclose(info->file_space_id) < 0)
                HGOTO_ERROR(H5E_DATASPACE, H5E_CANTRELEASE, FAIL, "unable to release dataspace");
            if(H5Tclose(info->mem_type_id) < 0)
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTRELEASE, FAIL, "unable to release datatype");
            if(H5Pclose(info->dxpl_id) < 0)
                HGOTO_ERROR(H5E_PLIST, H5E_CANTRELEASE, FAIL, "unable to release plist");
            H5VL_iod_type_info_reset(info->type_info);
            info->type_info = (H5VL_iod_type_info_t *)H5MM_xfree(info->type_info);
            if(info->space && H5S_close(info->space) < 0)
                HDONE_ERROR(H5E_DATASPACE, H5E_CANTRELEASE, FAIL, "unable to release dataspace");
            status = (H5VL_iod_read_status_t *)H5MM_xfree(status);
            info->bulk_handle = (hg_bulk_t *)H5MM_xfree(info->bulk_handle);
            info = (H5VL_iod_read_info_t *)H5MM_xfree(info);
            req->data = NULL;
            H5VL_iod_request_delete(file, req);
            break;
        }
    case HG_ATTR_WRITE:
    case HG_ATTR_READ:
        {
            H5VL_iod_attr_io_info_t *info = (H5VL_iod_attr_io_info_t *)req->data;

            /* Free memory handle */
            if(HG_SUCCESS != HG_Bulk_handle_free(*info->bulk_handle)) {
                HERROR(H5E_FUNC, H5E_CANTINIT, "failed to free attribute bulk handle\n");
                req->status = H5ES_STATUS_FAIL;
                req->state = H5VL_IOD_COMPLETED;
            }
            if(SUCCEED != *((int *)info->status)) {
                HERROR(H5E_FUNC, H5E_CANTINIT, "Attribute I/O Failure Reported from Server\n");
                req->status = H5ES_STATUS_FAIL;
                req->state = H5VL_IOD_COMPLETED;
            }

            HDfree(info->status);
            info->status = NULL;
            info->bulk_handle = (hg_bulk_t *)H5MM_xfree(info->bulk_handle);
            info = (H5VL_iod_attr_io_info_t *)H5MM_xfree(info);
            req->data = NULL;
            H5VL_iod_request_delete(file, req);
            break;
        }
    case HG_MAP_SET:
        {
            H5VL_iod_map_set_info_t *info = (H5VL_iod_map_set_info_t *)req->data;

            /* Free memory handle */
            if(HG_SUCCESS != HG_Bulk_handle_free(*info->value_handle)) {
                HERROR(H5E_FUNC, H5E_CANTINIT, "failed to free Map Value bulk handle\n");
                req->status = H5ES_STATUS_FAIL;
                req->state = H5VL_IOD_COMPLETED;
            }

            if(SUCCEED != *((int *)info->status)) {
                HERROR(H5E_FUNC, H5E_CANTINIT, "Errrr! MAP set Failure Reported from Server\n");
                req->status = H5ES_STATUS_FAIL;
                req->state = H5VL_IOD_COMPLETED;
            }

            free(info->status);
            info->status = NULL;
            info->value_handle = (hg_bulk_t *)H5MM_xfree(info->value_handle);
            info = (H5VL_iod_map_set_info_t *)H5MM_xfree(info);
            req->data = NULL;
            H5VL_iod_request_delete(file, req);
            break;
        }
    case HG_MAP_DELETE:
        {
            int *status = (int *)req->data;

            if(SUCCEED != *status) {
                HERROR(H5E_FUNC, H5E_CANTINIT, "MAP delete failed at the server\n");
                req->status = H5ES_STATUS_FAIL;
                req->state = H5VL_IOD_COMPLETED;
            }

            free(status);
            req->data = NULL;
            H5VL_iod_request_delete(file, req);
            break;
        }
    case HG_MAP_GET:
        {
            H5VL_iod_map_io_info_t *info = (H5VL_iod_map_io_info_t *)req->data;
            map_get_out_t *output = info->output;

            if(SUCCEED != output->ret) {
                HERROR(H5E_FUNC, H5E_CANTINIT, "MAP get failed at the server\n");
                req->status = H5ES_STATUS_FAIL;
                req->state = H5VL_IOD_COMPLETED;

                /* free stuff associated with request */
                info->value_handle = (hg_bulk_t *)H5MM_xfree(info->value_handle);
                if(info->val_is_vl) {
                    if(H5Tclose(info->val_mem_type_id) < 0)
                        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTRELEASE, FAIL, "unable to release datatype");
                    if(H5Tclose(info->key_mem_type_id) < 0)
                        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTRELEASE, FAIL, "unable to release datatype");
                    if(H5Pclose(info->dxpl_id) < 0)
                        HGOTO_ERROR(H5E_PLIST, H5E_CANTRELEASE, FAIL, "unable to release plist");
                }

                free(info->output);
                info->output = NULL;
                info = (H5VL_iod_map_io_info_t *)H5MM_xfree(info);
                req->data = NULL;

                /* remove request from file list */
                H5VL_iod_request_delete(file, req);
                break;
            }
            else {
                /* If the data is not VL, then just free resources and
                   remove the request */
                if(!info->val_is_vl) {
                    /* Free memory handle */
                    if(HG_SUCCESS != HG_Bulk_handle_free(*info->value_handle)) {
                        HERROR(H5E_FUNC, H5E_CANTINIT, "failed to free value bulk handle\n");
                        req->status = H5ES_STATUS_FAIL;
                        req->state = H5VL_IOD_COMPLETED;
                    }
                    else {
                        uint64_t internal_cs = 0;
                        uint32_t raw_cs_scope = info->raw_cs_scope;

                        /* calculate a checksum for the data recieved */
                        internal_cs = H5_checksum_crc64(info->val_ptr, info->val_size);

                        /* verify data integrity */
                        if((raw_cs_scope & H5_CHECKSUM_TRANSFER) &&
                           internal_cs != output->val_cs) {
                            HERROR(H5E_FUNC, H5E_CANTINIT, 
                                    "Errrrr!  MAP Get integrity failure (expecting %"PRIu64" got %"PRIu64").\n",
                                    output->val_cs, internal_cs);
                            req->status = H5ES_STATUS_FAIL;
                            req->state = H5VL_IOD_COMPLETED;
                        }
#if H5VL_IOD_DEBUG
                        if(!raw_cs_scope & H5_CHECKSUM_TRANSFER) {
                            printf("NO TRANSFER DATA INTEGRITY CHECKS ON RAW DATA READ\n");
                        }
#endif
                        /* If the app gave us a buffer to store the checksum, then put it there */
                        if(info->val_cs_ptr)
                            *info->val_cs_ptr = internal_cs;
                    }

                    free(info->output);
                    info->output = NULL;
                    info->value_handle = (hg_bulk_t *)H5MM_xfree(info->value_handle);
                    info = (H5VL_iod_map_io_info_t *)H5MM_xfree(info);
                    req->data = NULL;
                    H5VL_iod_request_delete(file, req);
                    break;
                }
                else {
                    /* If this is the second roundtrip with the VL
                       data, scatter it in the user buffer */
                    if(info->val_size && output->val_size) {
                        HDassert(info->val_size == output->val_size);

                        /* scatter the data into the user's buffer */
                        if(H5VL__iod_vl_map_get_finalize(output->val_size, 
                                                         info->read_buf, 
                                                         (void *)info->val_ptr, 
                                                         info->val_mem_type_id) < 0)
                            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to store VL data in user buffer");

                        req->data = NULL;
                        H5VL_iod_request_delete(file, req);
                        break;
                    }
                    /* if the data is VL, then we need to submit another
                       get operation this time to get the actual data */
                    else {
                        void *value_buf = NULL;
                        H5VL_iod_map_t *map = (H5VL_iod_map_t *)req->obj;
                        map_get_in_t input;
                        H5RC_t *rc = NULL;
                        H5VL_iod_map_io_info_t vl_read_info;

                        /* get the RC object */
                        if(NULL == (rc = (H5RC_t *)H5I_object_verify(info->rcxt_id, H5I_RC)))
                            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a READ CONTEXT ID");

                        if(NULL == (value_buf = (void *)HDmalloc(output->val_size)))
                            HGOTO_ERROR(H5E_DATASET, H5E_NOSPACE, FAIL, "can't allocate VL recieve buffer");

                        /* Register memory with bulk_handle */
                        if(HG_SUCCESS != HG_Bulk_handle_create(value_buf, output->val_size, 
                                                               HG_BULK_READWRITE, info->value_handle))
                            HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "can't create Bulk Data Handle");

                        /* Fill input structure */
                        input.coh = file->remote_file.coh;
                        input.iod_oh = map->remote_map.iod_oh;
                        input.iod_id = map->remote_map.iod_id;
                        input.dxpl_id = info->dxpl_id;
                        input.key_memtype_id = info->key_mem_type_id;
                        input.key_maptype_id = map->remote_map.keytype_id;
                        input.val_memtype_id = info->val_mem_type_id;
                        input.val_maptype_id = map->remote_map.valtype_id;
                        input.key.buf_size = info->key.buf_size;
                        input.key.buf = info->key.buf;
                        input.val_is_vl = TRUE;
                        input.val_size = output->val_size;
                        input.val_handle = *info->value_handle;
                        input.rcxt_num = rc->c_version;
                        input.cs_scope = map->common.file->md_integrity_scope;

                        vl_read_info.output = output;
                        vl_read_info.value_handle = info->value_handle;
                        vl_read_info.val_is_vl = TRUE;
                        vl_read_info.val_ptr = info->val_ptr;
                        vl_read_info.val_mem_type_id = info->val_mem_type_id;
                        vl_read_info.val_size = input.val_size;
                        vl_read_info.read_buf = value_buf;

                        /* remove request from file list */
                        H5VL_iod_request_delete(file, req);

                        if(H5VL__iod_create_and_forward(info->map_get_id, HG_MAP_GET, 
                                                        (H5VL_iod_object_t *)map, 0,
                                                        0, NULL, (H5VL_iod_req_info_t *)rc, 
                                                        &input, output, &vl_read_info, NULL) < 0)
                            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to create and ship map get");

                        /* Free memory handle */
                        if(HG_SUCCESS != HG_Bulk_handle_free(*info->value_handle)) {
                            HERROR(H5E_FUNC, H5E_CANTINIT, "failed to free value bulk handle\n");
                            req->status = H5ES_STATUS_FAIL;
                            req->state = H5VL_IOD_COMPLETED;
                        }

                        /* free stuff associated with request */
                        info->value_handle = (hg_bulk_t *)H5MM_xfree(info->value_handle);
                        HDfree(value_buf);
                        if(H5Tclose(info->val_mem_type_id) < 0)
                            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTRELEASE, FAIL, "unable to release datatype");
                        if(H5Tclose(info->key_mem_type_id) < 0)
                            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTRELEASE, FAIL, "unable to release datatype");
                        if(H5Pclose(info->dxpl_id) < 0)
                            HGOTO_ERROR(H5E_PLIST, H5E_CANTRELEASE, FAIL, "unable to release plist");

                        free(info->output);
                        info->output = NULL;
                        info = (H5VL_iod_map_io_info_t *)H5MM_xfree(info);
                        req->data = NULL;
                        break;
                    }
                }
            }
        }
    case HG_MAP_GET_COUNT:
        {
            hsize_t *count = (hsize_t *)req->data;

            if(*count == IOD_COUNT_UNDEFINED) {
                HERROR(H5E_FUNC, H5E_CANTINIT, "MAP get_count failed at the server\n");
                req->status = H5ES_STATUS_FAIL;
                req->state = H5VL_IOD_COMPLETED;
            }

            req->data = NULL;
            H5VL_iod_request_delete(file, req);
            break;
        }
    case HG_MAP_EXISTS:
        {
            H5VL_iod_exists_info_t *info = (H5VL_iod_exists_info_t *)req->data;
            H5VL_iod_object_t *obj = (H5VL_iod_object_t *)req->obj;

            if(info->server_ret < 0) {
                HERROR(H5E_FUNC, H5E_CANTINIT, "MAP exists failed at the server\n");
                req->status = H5ES_STATUS_FAIL;
                req->state = H5VL_IOD_COMPLETED;
            }
            else if (0 == info->server_ret)
                *info->user_bool = FALSE;
            else
                *info->user_bool = TRUE;

            req->data = NULL;
            obj->request = NULL;
            info = (H5VL_iod_exists_info_t *)H5MM_xfree(info);
            H5VL_iod_request_delete(file, req);
            break;
        }
    case HG_FILE_CLOSE:
        {
            int *status = (int *)req->data;

            if(SUCCEED != *status) {
                HERROR(H5E_FUNC, H5E_CANTINIT, "FILE close failed at the server\n");
                req->status = H5ES_STATUS_FAIL;
                req->state = H5VL_IOD_COMPLETED;
            }

            if(0 == file->my_rank)
                MPI_Barrier (file->comm);

            free(status);
            req->data = NULL;
            file->common.request = NULL;
            H5VL_iod_request_delete(file, req);

            /* free everything */
            free(file->file_name);
            free(file->common.obj_name);
            if(H5FD_mpi_comm_info_free(&file->comm, &file->info) < 0)
                HGOTO_ERROR(H5E_INTERNAL, H5E_CANTFREE, FAIL, "Communicator/Info free failed");
            if(file->common.comment)
                HDfree(file->common.comment);
            if(file->fapl_id != H5P_FILE_ACCESS_DEFAULT && H5Pclose(file->fapl_id) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "failed to close plist");
            if(file->remote_file.fcpl_id != H5P_FILE_CREATE_DEFAULT && 
               H5Pclose(file->remote_file.fcpl_id) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "failed to close plist");
            file = H5FL_FREE(H5VL_iod_file_t, file);
            break;
        }
    case HG_ATTR_RENAME:
        {
            int *status = (int *)req->data;
            H5VL_iod_object_t *obj = (H5VL_iod_object_t *)req->obj;

            if(SUCCEED != *status) {
                HERROR(H5E_FUNC, H5E_CANTINIT, "ATTR rename failed at the server\n");
                req->status = H5ES_STATUS_FAIL;
                req->state = H5VL_IOD_COMPLETED;
            }

            free(status);
            req->data = NULL;
            obj->request = NULL;
            H5VL_iod_request_delete(file, req);
            break;
        }
    case HG_ATTR_REMOVE:
        {
            int *status = (int *)req->data;
            H5VL_iod_object_t *obj = (H5VL_iod_object_t *)req->obj;

            if(SUCCEED != *status) {
                HERROR(H5E_FUNC, H5E_CANTINIT, "ATTR remove failed at the server\n");
                req->status = H5ES_STATUS_FAIL;
                req->state = H5VL_IOD_COMPLETED;
            }

            free(status);
            req->data = NULL;
            obj->request = NULL;
            H5VL_iod_request_delete(file, req);
            break;
        }
    case HG_ATTR_EXISTS:
    case HG_LINK_EXISTS:
    case HG_OBJECT_EXISTS:
        {
            htri_t *ret = (htri_t *)req->data;
            H5VL_iod_object_t *obj = (H5VL_iod_object_t *)req->obj;

            if(*ret < 0) {
                HERROR(H5E_FUNC, H5E_CANTINIT, "EXIST OP failed at the server\n");
                req->status = H5ES_STATUS_FAIL;
                req->state = H5VL_IOD_COMPLETED;
            }

            req->data = NULL;
            obj->request = NULL;
            H5VL_iod_request_delete(file, req);
            break;
        }
    case HG_ATTR_CLOSE:
        {
            int *status = (int *)req->data;
            H5VL_iod_attr_t *attr = (H5VL_iod_attr_t *)req->obj;

            if(SUCCEED != *status) {
                HERROR(H5E_FUNC, H5E_CANTINIT, "ATTR close failed at the server\n");
                req->status = H5ES_STATUS_FAIL;
                req->state = H5VL_IOD_COMPLETED;
            }

            free(status);
            req->data = NULL;
            attr->common.request = NULL;
            H5VL_iod_request_delete(file, req);

            /* free attr components */
            if(attr->common.obj_name)
                free(attr->common.obj_name);
            if(attr->loc_name)
                free(attr->loc_name);
            if(attr->common.comment)
                HDfree(attr->common.comment);
            if(H5Tclose(attr->remote_attr.type_id) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "failed to close dtype");
            if(H5Sclose(attr->remote_attr.space_id) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "failed to close dspace");
            attr = H5FL_FREE(H5VL_iod_attr_t, attr);
            break;
        }
    case HG_GROUP_CLOSE:
        {
            int *status = (int *)req->data;
            H5VL_iod_group_t *grp = (H5VL_iod_group_t *)req->obj;

            if(SUCCEED != *status) {
                HERROR(H5E_FUNC, H5E_CANTINIT, "GROUP CLOSE failed at the server\n");
                req->status = H5ES_STATUS_FAIL;
                req->state = H5VL_IOD_COMPLETED;
            }

            free(status);
            req->data = NULL;
            grp->common.request = NULL;
            H5VL_iod_request_delete(file, req);

            /* free group components */
            if(grp->common.obj_name)
                free(grp->common.obj_name);
            if(grp->common.comment)
                HDfree(grp->common.comment);
            if(grp->gapl_id != H5P_GROUP_ACCESS_DEFAULT && H5Pclose(grp->gapl_id) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "failed to close plist");
            if(grp->remote_group.gcpl_id != H5P_GROUP_CREATE_DEFAULT && 
               H5Pclose(grp->remote_group.gcpl_id) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "failed to close plist");
            grp = H5FL_FREE(H5VL_iod_group_t, grp);
            break;
        }
    case HG_DSET_SET_EXTENT:
        {
            int *status = (int *)req->data;
            H5VL_iod_dset_t *dset = (H5VL_iod_dset_t *)req->obj;

            if(SUCCEED != *status) {
                HERROR(H5E_FUNC, H5E_CANTINIT, "DATASET set extent failed at the server\n");
                req->status = H5ES_STATUS_FAIL;
                req->state = H5VL_IOD_COMPLETED;
            }

            free(status);
            req->data = NULL;
            dset->common.request = NULL;
            H5VL_iod_request_delete(file, req);
            break;
        }
    case HG_DSET_CLOSE:
        {
            int *status = (int *)req->data;
            H5VL_iod_dset_t *dset = (H5VL_iod_dset_t *)req->obj;

            if(SUCCEED != *status) {
                HERROR(H5E_FUNC, H5E_CANTINIT, "DATASET %s CLOSE failed at the server\n",
                        dset->common.obj_name);
                req->status = H5ES_STATUS_FAIL;
                req->state = H5VL_IOD_COMPLETED;
            }

            /* free dset components */
            if(dset->common.obj_name)
                free(dset->common.obj_name);
            if(dset->common.comment)
                HDfree(dset->common.comment);
            if(H5Tclose(dset->remote_dset.type_id) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "failed to close dtype");
            if(H5Sclose(dset->remote_dset.space_id) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "failed to close dspace");
            if(dset->remote_dset.dcpl_id != H5P_DATASET_CREATE_DEFAULT) {
                if(H5Pclose(dset->remote_dset.dcpl_id) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "failed to close plist");
            }
            if(dset->dapl_id != H5P_DATASET_ACCESS_DEFAULT) {
                if(H5Pclose(dset->dapl_id) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "failed to close plist");
            }

            free(status);
            req->data = NULL;
            dset->common.request = NULL;
            H5VL_iod_request_delete(file, req);
            dset = H5FL_FREE(H5VL_iod_dset_t, dset);
            break;
        }
    case HG_MAP_CLOSE:
        {
            int *status = (int *)req->data;
            H5VL_iod_map_t *map = (H5VL_iod_map_t *)req->obj;

            if(H5VL_iod_request_wait_some(map->common.file, map) < 0)
                HGOTO_ERROR(H5E_SYM,  H5E_CANTGET, FAIL, "can't wait on all object requests");

            if(SUCCEED != *status) {
                HERROR(H5E_FUNC, H5E_CANTINIT, "MAP close failed at the server\n");
                req->status = H5ES_STATUS_FAIL;
                req->state = H5VL_IOD_COMPLETED;
            }

            free(status);
            req->data = NULL;
            map->common.request = NULL;
            H5VL_iod_request_delete(file, req);

            /* free map components */
            if(map->common.obj_name)
                free(map->common.obj_name);
            if(map->common.comment)
                HDfree(map->common.comment);
            if(H5Tclose(map->remote_map.keytype_id) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "failed to close dtype");
            if(H5Tclose(map->remote_map.valtype_id) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "failed to close dtype");
            map = H5FL_FREE(H5VL_iod_map_t, map);
            break;
        }
    case HG_DTYPE_CLOSE:
        {
            int *status = (int *)req->data;
            H5VL_iod_dtype_t *dtype = (H5VL_iod_dtype_t *)req->obj;

            if(SUCCEED != *status) {
                HERROR(H5E_FUNC, H5E_CANTINIT, "datatype close failed at the server\n");
                req->status = H5ES_STATUS_FAIL;
                req->state = H5VL_IOD_COMPLETED;
            }

            free(status);
            req->data = NULL;
            dtype->common.request = NULL;
            H5VL_iod_request_delete(file, req);

            /* free dtype components */
            if(dtype->common.obj_name)
                free(dtype->common.obj_name);
            if(dtype->common.comment)
                HDfree(dtype->common.comment);
            if(dtype->remote_dtype.tcpl_id != H5P_DATATYPE_CREATE_DEFAULT &&
               H5Pclose(dtype->remote_dtype.tcpl_id) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "failed to close plist");
            if(dtype->tapl_id != H5P_DATATYPE_ACCESS_DEFAULT &&
               H5Pclose(dtype->tapl_id) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "failed to close plist");
            if(H5Tclose(dtype->remote_dtype.type_id) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "failed to close dtype");
            dtype = H5FL_FREE(H5VL_iod_dtype_t, dtype);
            break;
        }
    case HG_LINK_CREATE:
    case HG_LINK_MOVE:
    case HG_LINK_REMOVE:
    case HG_OBJECT_SET_COMMENT:
    case HG_OBJECT_COPY:
        {
            int *status = (int *)req->data;

            if(SUCCEED != *status) {
                HERROR(H5E_FUNC, H5E_CANTINIT, "Link operation failed at the server\n");
                req->status = H5ES_STATUS_FAIL;
                req->state = H5VL_IOD_COMPLETED;
            }

            free(status);
            req->data = NULL;
            H5VL_iod_request_delete(file, req);
            break;
        }
    case HG_LINK_GET_INFO:
        {
            H5L_ff_info_t *linfo = (H5L_ff_info_t *)req->data;

            if(linfo->type == H5L_TYPE_ERROR) {
                HERROR(H5E_FUNC, H5E_CANTINIT, "Link get_info failed at the server\n");
                req->status = H5ES_STATUS_FAIL;
                req->state = H5VL_IOD_COMPLETED;
            }

            req->data = NULL;
            H5VL_iod_request_delete(file, req);
            break;
        }
    case HG_OBJECT_GET_INFO:
        {
            H5O_ff_info_t *oinfo = (H5O_ff_info_t *)req->data;

            if(oinfo->type == H5O_TYPE_UNKNOWN) {
                HERROR(H5E_FUNC, H5E_CANTINIT, "OBJECT get_info failed at the server\n");
                req->status = H5ES_STATUS_FAIL;
                req->state = H5VL_IOD_COMPLETED;
            }

            req->data = NULL;
            H5VL_iod_request_delete(file, req);
            break;
        }
    case HG_LINK_GET_VAL:
        {
            link_get_val_out_t *result = (link_get_val_out_t *)req->data;

            if(SUCCEED != result->ret) {
                HERROR(H5E_FUNC, H5E_CANTINIT, "get comment failed\n");
                req->status = H5ES_STATUS_FAIL;
                req->state = H5VL_IOD_COMPLETED;
            }

            /* Free Mercury request */
            if(HG_Request_free(*((hg_request_t *)req->req)) != HG_SUCCESS)
                HGOTO_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "Can't Free Mercury Request");

            free(result);
            req->data = NULL;
            H5VL_iod_request_delete(file, req);
            HGOTO_DONE(ret_value);
            //break;
        }
    case HG_OBJECT_OPEN_BY_TOKEN:
        {
            iod_handles_t *oh = (iod_handles_t *)req->data;

            if(IOD_OH_UNDEFINED == (*oh).rd_oh.cookie) {
                HERROR(H5E_FUNC, H5E_CANTINIT, "failed to Open object by token\n");
                req->status = H5ES_STATUS_FAIL;
                req->state = H5VL_IOD_COMPLETED;
            }

            req->data = NULL;
            H5VL_iod_request_delete(file, req);
            break;
        }
    case HG_OBJECT_OPEN:
        {
            H5VL_iod_remote_object_t *obj = (H5VL_iod_remote_object_t *)req->data;

            if(IOD_OH_UNDEFINED == obj->iod_oh.rd_oh.cookie) {
                HERROR(H5E_FUNC, H5E_CANTINIT, "failed to open Object\n");
                req->status = H5ES_STATUS_FAIL;
                req->state = H5VL_IOD_COMPLETED;
            }
            else {
                /* increment ref count on IDs generated by Mercury encoding callback */
                if(H5I_inc_ref(obj->cpl_id, TRUE) < 0)
                    HGOTO_ERROR(H5E_ATOM, H5E_CANTINC, FAIL, "can't increment ID ref count");
                if(FAIL != obj->id1) {
                    if(H5I_inc_ref(obj->id1, TRUE) < 0)
                        HGOTO_ERROR(H5E_ATOM, H5E_CANTINC, FAIL, "can't increment ID ref count");
                }
                if(FAIL != obj->id2) {
                    if(H5I_inc_ref(obj->id2, TRUE) < 0)
                        HGOTO_ERROR(H5E_ATOM, H5E_CANTINC, FAIL, "can't increment ID ref count");
                }
            }

            req->data = NULL;
            H5VL_iod_request_delete(file, req);
            break;
        }
    case HG_OBJECT_GET_COMMENT:
        {
            object_get_comment_out_t *result = (object_get_comment_out_t *)req->data;

            if(SUCCEED != result->ret) {
                HERROR(H5E_FUNC, H5E_CANTINIT, "get comment failed\n");
                req->status = H5ES_STATUS_FAIL;
                req->state = H5VL_IOD_COMPLETED;
            }

            /* Free Mercury request */
            if(HG_Request_free(*((hg_request_t *)req->req)) != HG_SUCCESS)
                HGOTO_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "Can't Free Mercury Request");

            free(result);
            H5VL_iod_request_delete(file, req);
            HGOTO_DONE(ret_value);
            //break;
        }
    case HG_RC_ACQUIRE:
        {
            H5VL_iod_rc_info_t *rc_info = (H5VL_iod_rc_info_t *)req->data;

            if(SUCCEED != rc_info->result.ret) {
                HERROR(H5E_FUNC, H5E_CANTINIT, "Failed to Acquire Read Context %"PRIu64"\n", *(rc_info->c_version_ptr));
                req->status = H5ES_STATUS_FAIL;
                req->state = H5VL_IOD_COMPLETED;
            }

            rc_info->read_cxt->c_version = rc_info->result.c_version;
            *rc_info->c_version_ptr = rc_info->result.c_version;
            rc_info->read_cxt->req_info.request = NULL;
            rc_info = (H5VL_iod_rc_info_t *)H5MM_xfree(rc_info);
            req->data = NULL;
            H5VL_iod_request_delete(file, req);
            break;
        }
    case HG_RC_RELEASE:
    case HG_RC_PERSIST:
    case HG_RC_SNAPSHOT:
        {
            int *status = (int *)req->data;

            if(SUCCEED != *status) {
                HERROR(H5E_FUNC, H5E_CANTINIT, "Failed to Read Context OP\n");
                req->status = H5ES_STATUS_FAIL;
                req->state = H5VL_IOD_COMPLETED;
            }

            free(status);
            req->data = NULL;
            H5VL_iod_request_delete(file, req);
            break;
        }
    case HG_TR_START:
        {
            H5VL_iod_tr_info_t *tr_info = (H5VL_iod_tr_info_t *)req->data;

            if(SUCCEED != tr_info->result) {
                HERROR(H5E_FUNC, H5E_CANTINIT, "Failed to start transaction  %"PRIu64"\n", tr_info->tr->trans_num);
                req->status = H5ES_STATUS_FAIL;
                req->state = H5VL_IOD_COMPLETED;
            }

            tr_info->tr->req_info.request = NULL;
            tr_info = (H5VL_iod_tr_info_t *)H5MM_xfree(tr_info);
            req->data = NULL;
            H5VL_iod_request_delete(file, req);
            break;
        }
    case HG_TR_FINISH:
    case HG_TR_SET_DEPEND:
    case HG_TR_SKIP:
    case HG_TR_ABORT:
    case HG_EVICT:
        {
            int *status = (int *)req->data;

            if(SUCCEED != *status) {
                HERROR(H5E_FUNC, H5E_CANTINIT, "Failed transaction OP\n");
                req->status = H5ES_STATUS_FAIL;
                req->state = H5VL_IOD_COMPLETED;
            }

            free(status);
            req->data = NULL;
            H5VL_iod_request_delete(file, req);
            break;
        }
    case HG_PREFETCH:
        {
            hrpl_t *replica_id = (hrpl_t *)req->data;

            if(0 == *replica_id) {
                HERROR(H5E_FUNC, H5E_CANTINIT, "Failed transaction OP\n");
                req->status = H5ES_STATUS_FAIL;
                req->state = H5VL_IOD_COMPLETED;
            }

            req->data = NULL;
            H5VL_iod_request_delete(file, req);
            break;
        }
    case HG_VIEW_CREATE:
        {
            H5VL_iod_view_t *view = (H5VL_iod_view_t *)req->data;

            if(FALSE == view->valid_view) {
                HERROR(H5E_FUNC, H5E_CANTINIT, "failed to create view\n");
                req->status = H5ES_STATUS_FAIL;
                req->state = H5VL_IOD_COMPLETED;
            }

            req->data = NULL;
            H5VL_iod_request_delete(file, req);
            break;
        }
#ifdef H5_HAVE_INDEXING
    case HG_DSET_SET_INDEX_INFO:
        {
            int *status = (int *)req->data;

            printf("Index info is now set\n");
            if(SUCCEED != *status) {
                HERROR(H5E_FUNC, H5E_CANTINIT, "set_index_info failed at the server\n");
                req->status = H5ES_STATUS_FAIL;
                req->state = H5VL_IOD_COMPLETED;
            }

            free(status);
            req->data = NULL;
            H5VL_iod_request_delete(file, req);
            break;
        }
    case HG_DSET_GET_INDEX_INFO:
        {
            H5VL_iod_dataset_get_index_info_t *idx_info =
                (H5VL_iod_dataset_get_index_info_t *)req->data;
            dset_get_index_info_out_t *output = idx_info->output;

            if(SUCCEED != output->ret) {
                HERROR(H5E_FUNC, H5E_CANTINIT, "get_index_info failed at the server\n");
                req->status = H5ES_STATUS_FAIL;
                req->state = H5VL_IOD_COMPLETED;
            }
            else {
                unsigned plugin_id;
                size_t count;

                printf("Get index info ret is: %d\n", output->ret);
                printf("Index count is: %ld\n", output->idx_count);
                printf("Plugin ID is: %d\n", output->idx_plugin_id);

                count = output->idx_count;
                /* MSC - for now, idx_plugin_count is always 1 */
                HDassert(1 == count || 0 == count);

                if (idx_info->count) 
                    *idx_info->count = count;

                if(0 == count) {
                    if (idx_info->plugin_id) 
                        *idx_info->plugin_id = H5X_PLUGIN_NONE;
                    if (idx_info->metadata_size) 
                        *idx_info->metadata_size = 0;
                    if (idx_info->metadata) 
                        *idx_info->metadata = NULL;
                }
                if(1 == count) {
                    plugin_id = (unsigned) output->idx_plugin_id;
                    if(!plugin_id) {
                        HERROR(H5E_INDEX, H5E_BADVALUE, "invalid index plugin ID\n");
                        req->status = H5ES_STATUS_FAIL;
                        req->state = H5VL_IOD_COMPLETED;
                    } else {
                        size_t metadata_size = idx_info->output->idx_metadata.buf_size;
                        void *metadata = idx_info->output->idx_metadata.buf;
                        void *new_metadata;

                        if (!metadata_size)
                            HGOTO_ERROR(H5E_INDEX, H5E_BADVALUE, FAIL, "invalid metadata size");

                        if (NULL == (new_metadata = H5MM_calloc(metadata_size)))
                            HGOTO_ERROR(H5E_INDEX, H5E_NOSPACE, FAIL, "can't allocate metadata");
                        HDmemcpy(new_metadata, metadata, metadata_size);

                        if (idx_info->plugin_id) 
                            *idx_info->plugin_id = plugin_id;
                        if (idx_info->metadata_size) 
                            *idx_info->metadata_size = metadata_size;
                        if (idx_info->metadata) 
                            *idx_info->metadata = new_metadata;
                    }
                }
            }

            output = (dset_get_index_info_out_t *)H5MM_xfree(output);
            idx_info = (H5VL_iod_dataset_get_index_info_t *)H5MM_xfree(idx_info);
            req->data = NULL;
            H5VL_iod_request_delete(file, req);
            break;
        }
    case HG_DSET_RM_INDEX_INFO:
        {
            int *status = (int *)req->data;

            if(SUCCEED != *status) {
                HERROR(H5E_FUNC, H5E_CANTINIT, "remove_index_info failed at the server\n");
                req->status = H5ES_STATUS_FAIL;
                req->state = H5VL_IOD_COMPLETED;
            }

            free(status);
            req->data = NULL;
            H5VL_iod_request_delete(file, req);
            break;
        }
#endif /* H5_HAVE_INDEXING */
    case HG_LINK_ITERATE:
    case HG_OBJECT_VISIT:
    case HG_MAP_ITERATE:
    default:
        req->status = H5ES_STATUS_FAIL;
        req->state = H5VL_IOD_COMPLETED;
        req->data = NULL;
        H5VL_iod_request_delete(file, req);
        HGOTO_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "Request Type not supported");
    }

    /* Free Mercury request */
    if(HG_Request_free(*((hg_request_t *)req->req)) != HG_SUCCESS)
        HGOTO_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "Can't Free Mercury Request");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_request_complete */


/*-------------------------------------------------------------------------
 * Function:    H5VL_iod_request_cancel
 *
 * Purpose:     Cancels a particular request by freeing memory 
 *              associated with it.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_iod_request_cancel(H5VL_iod_file_t *file, H5VL_iod_request_t *req)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(req->state == H5VL_IOD_CANCELLED);

    switch(req->type) {
    case HG_ANALYSIS_EXECUTE:
        {
            analysis_execute_out_t *output = (analysis_execute_out_t *)req->data;

            free(output);
            req->data = NULL;
            H5VL_iod_request_delete(file, req);
            break;
        }
    case HG_DSET_WRITE:
        {
            H5VL_iod_write_info_t *info = (H5VL_iod_write_info_t *)req->data;

            /* Free memory handle */
            if(HG_SUCCESS != HG_Bulk_handle_free(*info->bulk_handle)) {
                fprintf(stderr, "failed to free bulk handle\n");
            }

            if(info->vl_segments) {
                HDfree(info->vl_segments);
                info->vl_segments = NULL;
            }
            if(info->vl_lengths) {
                HDfree(info->vl_lengths);
                info->vl_lengths = NULL;
            }

            HDfree(info->status);
            info->status = NULL;
            info->bulk_handle = (hg_bulk_t *)H5MM_xfree(info->bulk_handle);
            info->vl_len_bulk_handle = (hg_bulk_t *)H5MM_xfree(info->vl_len_bulk_handle);
            info = (H5VL_iod_write_info_t *)H5MM_xfree(info);
            req->data = NULL;
            H5VL_iod_request_delete(file, req);
            break;
        }
    case HG_DSET_GET_VL_SIZE:
        {
            H5VL_iod_read_info_t *info = (H5VL_iod_read_info_t *)req->data;

            if(H5Sclose(info->file_space_id) < 0)
                HGOTO_ERROR(H5E_DATASPACE, H5E_CANTRELEASE, FAIL, "unable to release dataspace");
            if(H5Tclose(info->mem_type_id) < 0)
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTRELEASE, FAIL, "unable to release datatype");
            if(H5Pclose(info->dxpl_id) < 0)
                HGOTO_ERROR(H5E_PLIST, H5E_CANTRELEASE, FAIL, "unable to release plist");
        }
    case HG_DSET_READ:
        {
            H5VL_iod_read_info_t *info = (H5VL_iod_read_info_t *)req->data;

            /* Free memory handle */
            if(HG_SUCCESS != HG_Bulk_handle_free(*info->bulk_handle)) {
                HERROR(H5E_FUNC, H5E_CANTINIT, "failed to free bulk handle\n");
            }

            H5VL_iod_type_info_reset(info->type_info);
            info->type_info = (H5VL_iod_type_info_t *)H5MM_xfree(info->type_info);
            if(info->space && H5S_close(info->space) < 0)
                HDONE_ERROR(H5E_DATASPACE, H5E_CANTRELEASE, FAIL, "unable to release dataspace");
            info->status = (H5VL_iod_read_status_t *)H5MM_xfree(info->status);
            info->bulk_handle = (hg_bulk_t *)H5MM_xfree(info->bulk_handle);
            info = (H5VL_iod_read_info_t *)H5MM_xfree(info);
            req->data = NULL;
            H5VL_iod_request_delete(file, req);
            break;
        }
    case HG_ATTR_WRITE:
    case HG_ATTR_READ:
        {
            H5VL_iod_attr_io_info_t *info = (H5VL_iod_attr_io_info_t *)req->data;

            HDfree(info->status);
            info->status = NULL;
            info->bulk_handle = (hg_bulk_t *)H5MM_xfree(info->bulk_handle);
            info = (H5VL_iod_attr_io_info_t *)H5MM_xfree(info);
            req->data = NULL;
            H5VL_iod_request_delete(file, req);
            break;
        }
    case HG_FILE_CREATE:
    case HG_FILE_OPEN:
    case HG_FILE_CLOSE:
        {
            int *status = (int *)req->data;

            free(status);
            req->data = NULL;
            file->common.request = NULL;
            H5VL_iod_request_delete(file, req);

            /* free everything */
            free(file->file_name);
            free(file->common.obj_name);
            if(H5FD_mpi_comm_info_free(&file->comm, &file->info) < 0)
                HGOTO_ERROR(H5E_INTERNAL, H5E_CANTFREE, FAIL, "Communicator/Info free failed");
            if(file->common.comment)
                HDfree(file->common.comment);
            if(file->fapl_id != H5P_FILE_ACCESS_DEFAULT && H5Pclose(file->fapl_id) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "failed to close plist");
            if(file->remote_file.fcpl_id != 0 && 
               file->remote_file.fcpl_id != H5P_FILE_CREATE_DEFAULT && 
               H5Pclose(file->remote_file.fcpl_id) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "failed to close plist");
            file = H5FL_FREE(H5VL_iod_file_t, file);
            break;
        }
    case HG_ATTR_REMOVE:
    case HG_ATTR_RENAME:
        {
            int *status = (int *)req->data;
            H5VL_iod_object_t *obj = (H5VL_iod_object_t *)req->obj;

            free(status);
            req->data = NULL;
            obj->request = NULL;
            H5VL_iod_request_delete(file, req);
            break;
        }
    case HG_MAP_EXISTS:
        {
            H5VL_iod_exists_info_t *info = (H5VL_iod_exists_info_t *)req->data;
            H5VL_iod_object_t *obj = (H5VL_iod_object_t *)req->obj;

            req->data = NULL;
            obj->request = NULL;
            info = (H5VL_iod_exists_info_t *)H5MM_xfree(info);
            H5VL_iod_request_delete(file, req);
            break;
        }
    case HG_ATTR_EXISTS:
    case HG_LINK_EXISTS:
    case HG_OBJECT_EXISTS:
    case HG_MAP_GET_COUNT:
        {
            H5VL_iod_object_t *obj = (H5VL_iod_object_t *)req->obj;

            req->data = NULL;
            obj->request = NULL;
            H5VL_iod_request_delete(file, req);
            break;
        }
    case HG_ATTR_CREATE:
    case HG_ATTR_OPEN:
    case HG_ATTR_CLOSE:
        {
            int *status = (int *)req->data;
            H5VL_iod_attr_t *attr = (H5VL_iod_attr_t *)req->obj;

            free(status);
            req->data = NULL;
            attr->common.request = NULL;
            H5VL_iod_request_delete(file, req);

            /* free attr components */
            if(attr->common.obj_name)
                free(attr->common.obj_name);
            if(attr->loc_name)
                free(attr->loc_name);
            if(attr->common.comment)
                HDfree(attr->common.comment);
            if(attr->remote_attr.type_id != 0 &&
               H5Tclose(attr->remote_attr.type_id) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "failed to close dtype");
            if(attr->remote_attr.space_id != 0 &&
               H5Sclose(attr->remote_attr.space_id) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "failed to close dspace");
            attr = H5FL_FREE(H5VL_iod_attr_t, attr);
            break;
        }
    case HG_GROUP_CREATE:
    case HG_GROUP_OPEN:
    case HG_GROUP_CLOSE:
        {
            int *status = (int *)req->data;
            H5VL_iod_group_t *grp = (H5VL_iod_group_t *)req->obj;

            free(status);
            req->data = NULL;
            grp->common.request = NULL;
            H5VL_iod_request_delete(file, req);

            /* free group components */
            if(grp->common.obj_name)
                free(grp->common.obj_name);
            if(grp->common.comment)
                HDfree(grp->common.comment);
            if(grp->gapl_id != H5P_GROUP_ACCESS_DEFAULT && H5Pclose(grp->gapl_id) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "failed to close plist");
            if(grp->remote_group.gcpl_id != 0 &&
               grp->remote_group.gcpl_id != H5P_GROUP_CREATE_DEFAULT && 
               H5Pclose(grp->remote_group.gcpl_id) < 0) {
                HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "failed to close plist");
            }
            grp = H5FL_FREE(H5VL_iod_group_t, grp);
            break;
        }
    case HG_DSET_SET_EXTENT:
        {
            int *status = (int *)req->data;
            H5VL_iod_dset_t *dset = (H5VL_iod_dset_t *)req->obj;

            free(status);
            req->data = NULL;
            dset->common.request = NULL;
            H5VL_iod_request_delete(file, req);
            break;
        }
    case HG_DSET_CREATE:
    case HG_DSET_OPEN:
    case HG_DSET_CLOSE:
        {
            int *status = (int *)req->data;
            H5VL_iod_dset_t *dset = (H5VL_iod_dset_t *)req->obj;

            free(status);
            req->data = NULL;
            dset->common.request = NULL;
            H5VL_iod_request_delete(file, req);

            /* free dset components */
            if(dset->common.obj_name)
                free(dset->common.obj_name);
            if(dset->common.comment)
                HDfree(dset->common.comment);
            if(dset->remote_dset.dcpl_id != 0 &&
               dset->remote_dset.dcpl_id != H5P_DATASET_CREATE_DEFAULT &&
               H5Pclose(dset->remote_dset.dcpl_id) < 0) {
                HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "failed to close plist");
            }
            if(dset->dapl_id != H5P_DATASET_ACCESS_DEFAULT &&
               H5Pclose(dset->dapl_id) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "failed to close plist");
            if(dset->remote_dset.type_id != 0 &&
               H5Tclose(dset->remote_dset.type_id) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "failed to close dtype");
            if(dset->remote_dset.space_id != 0 &&
               H5Sclose(dset->remote_dset.space_id) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "failed to close dspace");
            dset = H5FL_FREE(H5VL_iod_dset_t, dset);
            break;
        }
    case HG_MAP_CREATE:
    case HG_MAP_OPEN:
    case HG_MAP_CLOSE:
        {
            int *status = (int *)req->data;
            H5VL_iod_map_t *map = (H5VL_iod_map_t *)req->obj;

            free(status);
            req->data = NULL;
            map->common.request = NULL;
            H5VL_iod_request_delete(file, req);

            /* free map components */
            if(map->common.obj_name)
                free(map->common.obj_name);
            if(map->common.comment)
                HDfree(map->common.comment);
            if(H5Tclose(map->remote_map.keytype_id) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "failed to close dtype");
            if(H5Tclose(map->remote_map.valtype_id) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "failed to close dtype");
            map = H5FL_FREE(H5VL_iod_map_t, map);
            break;
        }
    case HG_DTYPE_COMMIT:
    case HG_DTYPE_OPEN:
    case HG_DTYPE_CLOSE:
        {
            int *status = (int *)req->data;
            H5VL_iod_dtype_t *dtype = (H5VL_iod_dtype_t *)req->obj;

            free(status);
            req->data = NULL;
            dtype->common.request = NULL;
            H5VL_iod_request_delete(file, req);

            /* free dtype components */
            if(dtype->common.obj_name)
                free(dtype->common.obj_name);
            if(dtype->common.comment)
                HDfree(dtype->common.comment);
            if(dtype->remote_dtype.tcpl_id != 0 &&
               dtype->remote_dtype.tcpl_id != H5P_DATATYPE_CREATE_DEFAULT &&
               H5Pclose(dtype->remote_dtype.tcpl_id) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "failed to close plist");
            if(dtype->tapl_id != H5P_DATATYPE_ACCESS_DEFAULT &&
               H5Pclose(dtype->tapl_id) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "failed to close plist");
            if(dtype->remote_dtype.type_id != 0 && 
               H5Tclose(dtype->remote_dtype.type_id) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "failed to close dtype");
            dtype = H5FL_FREE(H5VL_iod_dtype_t, dtype);
            break;
        }

    case HG_MAP_GET:
        {
            map_get_out_t *output = (map_get_out_t *)req->data;

            free(output);
            req->data = NULL;
            H5VL_iod_request_delete(file, req);
            break;
        }
    case HG_MAP_SET:
        {
            H5VL_iod_map_set_info_t *info = (H5VL_iod_map_set_info_t *)req->data;

            free(info->status);
            info->status = NULL;
            info->value_handle = (hg_bulk_t *)H5MM_xfree(info->value_handle);
            info = (H5VL_iod_map_set_info_t *)H5MM_xfree(info);
            req->data = NULL;
            H5VL_iod_request_delete(file, req);
            break;
        }
    case HG_MAP_DELETE:
    case HG_LINK_CREATE:
    case HG_LINK_MOVE:
    case HG_LINK_REMOVE:
    case HG_OBJECT_SET_COMMENT:
    case HG_OBJECT_COPY:
        {
            int *status = (int *)req->data;

            free(status);
            req->data = NULL;
            H5VL_iod_request_delete(file, req);
            break;
        }
    case HG_OBJECT_GET_COMMENT:
        {
            object_get_comment_out_t *result = (object_get_comment_out_t *)req->data;

            free(result);
            req->data = NULL;
            H5VL_iod_request_delete(file, req);
            break;
        }
    case HG_OBJECT_OPEN_BY_TOKEN:
    case HG_OBJECT_OPEN:
    case HG_LINK_GET_INFO:
    case HG_OBJECT_GET_INFO:
        req->data = NULL;
        H5VL_iod_request_delete(file, req);
        break;
    case HG_LINK_GET_VAL:
        {
            link_get_val_out_t *result = (link_get_val_out_t *)req->data;

            free(result);
            req->data = NULL;
            H5VL_iod_request_delete(file, req);
            break;
        }
    case HG_RC_ACQUIRE:
        {
            H5VL_iod_rc_info_t *rc_info = (H5VL_iod_rc_info_t *)req->data;

            rc_info = (H5VL_iod_rc_info_t *)H5MM_xfree(rc_info);
            req->data = NULL;
            H5VL_iod_request_delete(file, req);
            break;
        }
    case HG_RC_RELEASE:
    case HG_RC_PERSIST:
    case HG_RC_SNAPSHOT:
    case HG_TR_START:
    case HG_TR_FINISH:
    case HG_TR_SET_DEPEND:
    case HG_TR_SKIP:
    case HG_TR_ABORT:
    case HG_EVICT:
#ifdef H5_HAVE_INDEXING
    case HG_DSET_SET_INDEX_INFO:
    case HG_DSET_RM_INDEX_INFO:
#endif /* H5_HAVE_INDEXING */
        {
            int *status = (int *)req->data;

            free(status);
            req->data = NULL;
            H5VL_iod_request_delete(file, req);
            break;
        }
#ifdef H5_HAVE_INDEXING
    case HG_DSET_GET_INDEX_INFO:
        {
            H5VL_iod_dataset_get_index_info_t *idx_info =
                (H5VL_iod_dataset_get_index_info_t *)req->data;
            dset_get_index_info_out_t *output = idx_info->output;

            output = (dset_get_index_info_out_t *)H5MM_xfree(output);
            idx_info = (H5VL_iod_dataset_get_index_info_t *)H5MM_xfree(idx_info);
            req->data = NULL;
            H5VL_iod_request_delete(file, req);
            break;
        }
#endif /* H5_HAVE_INDEXING */
    case HG_PREFETCH:
    case HG_VIEW_CREATE:
        req->data = NULL;
        H5VL_iod_request_delete(file, req);
        break;
    case HG_LINK_ITERATE:
    case HG_OBJECT_VISIT:
    case HG_MAP_ITERATE:
    default:
        H5VL_iod_request_delete(file, req);
        HGOTO_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "Request Type not supported");
    }

    /* Free Mercury request */
    if(HG_Request_free(*((hg_request_t *)req->req)) != HG_SUCCESS)
        HGOTO_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "Can't Free Mercury Request");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_request_cancel */


/*-------------------------------------------------------------------------
 * Function:    H5VL_iod_get_obj_requests
 *
 * Purpose:     returns the number of requests that are associated 
 *              with a particular object. If the parent array is not NULL, 
 *              the request pointers are stored.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_iod_get_obj_requests(H5VL_iod_object_t *obj, /*IN/OUT*/ size_t *count, 
                          /*OUT*/ H5VL_iod_request_t **parent_reqs)
{
    H5VL_iod_file_t *file = obj->file;
    H5VL_iod_request_t *cur_req = file->request_list_head;
    size_t size = 0;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    while(cur_req) {
        /* If the request is pending on the object we want, add its axe_id */
        if(cur_req->obj == obj) {
            if(cur_req->status == H5ES_STATUS_IN_PROGRESS) {
                if(NULL != parent_reqs) {
                    parent_reqs[size] = cur_req;
                    cur_req->ref_count ++;
                }
                size ++;
            }
        }
        cur_req = cur_req->file_next;
    }

    *count = size;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5VL_iod_get_obj_requests */

herr_t
H5VL_iod_get_loc_info(H5VL_iod_object_t *obj, iod_obj_id_t *iod_id, 
                      iod_handles_t *iod_oh, iod_obj_id_t *mdkv_id, 
                      iod_obj_id_t *attrkv_id)
{
    iod_obj_id_t id, md, at;
    iod_handles_t oh;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    switch(obj->obj_type) {
        case H5I_FILE:
            oh = obj->file->remote_file.root_oh;
            id = obj->file->remote_file.root_id;
            md = obj->file->remote_file.mdkv_id;
            at = obj->file->remote_file.attrkv_id;
            break;
        case H5I_GROUP:
            oh = ((const H5VL_iod_group_t *)obj)->remote_group.iod_oh;
            id = ((const H5VL_iod_group_t *)obj)->remote_group.iod_id;
            md = ((const H5VL_iod_group_t *)obj)->remote_group.mdkv_id;
            at = ((const H5VL_iod_group_t *)obj)->remote_group.attrkv_id;
            break;
        case H5I_DATASET:
            oh = ((const H5VL_iod_dset_t *)obj)->remote_dset.iod_oh;
            id = ((const H5VL_iod_dset_t *)obj)->remote_dset.iod_id;
            md = ((const H5VL_iod_dset_t *)obj)->remote_dset.mdkv_id;
            at = ((const H5VL_iod_dset_t *)obj)->remote_dset.attrkv_id;
            break;
        case H5I_DATATYPE:
            oh = ((const H5VL_iod_dtype_t *)obj)->remote_dtype.iod_oh;
            id = ((const H5VL_iod_dtype_t *)obj)->remote_dtype.iod_id;
            md = ((const H5VL_iod_dtype_t *)obj)->remote_dtype.mdkv_id;
            at = ((const H5VL_iod_dtype_t *)obj)->remote_dtype.attrkv_id;
            break;
        case H5I_MAP:
            oh = ((const H5VL_iod_map_t *)obj)->remote_map.iod_oh;
            id = ((const H5VL_iod_map_t *)obj)->remote_map.iod_id;
            md = ((const H5VL_iod_map_t *)obj)->remote_map.mdkv_id;
            at = ((const H5VL_iod_map_t *)obj)->remote_map.attrkv_id;
            break;
        case H5I_UNINIT:
        case H5I_BADID:
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
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "bad location object");
    }

    if(iod_id)
        *iod_id = id;
    if(iod_oh)
        *iod_oh = oh;
    if(mdkv_id)
        *mdkv_id = md;
    if(attrkv_id)
        *attrkv_id = at;
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_get_loc_info() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_iod_get_parent_requests
 *
 * Purpose:     Returns the parent requests associated with an object 
 *              and transaction.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_iod_get_parent_requests(H5VL_iod_object_t *obj, H5VL_iod_req_info_t *req_info, 
                             H5VL_iod_request_t **parent_reqs, size_t *num_parents)
{
    size_t count = 0;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    if(obj && obj->request && obj->request->status == H5ES_STATUS_IN_PROGRESS) {
        parent_reqs[count] = obj->request;
        obj->request->ref_count ++;
        count ++;
    }

    if(req_info && 
       req_info->request && 
       req_info->request->status == H5ES_STATUS_IN_PROGRESS) {
        parent_reqs[count] = req_info->request;
        req_info->request->ref_count ++;
        count ++;
    }

    *num_parents += count;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5VL_iod_get_parent_requests */

herr_t
H5VL_iod_gen_obj_id(int myrank, int nranks, uint64_t cur_index, 
                    iod_obj_type_t type, uint64_t *id)
{
    herr_t ret_value = SUCCEED;
    uint64_t tmp_id;

    FUNC_ENTER_NOAPI_NOINIT

    /* determine first the rank of the object with the first 60 bits
       (IOD owns 60,61,62,63). */
    tmp_id = (uint32_t)myrank + ((uint32_t)nranks * cur_index);

    /* toggle the object type bits */
    switch(type) {
    case IOD_OBJ_ARRAY:
        IOD_OBJID_SETTYPE(tmp_id, IOD_OBJ_ARRAY)
        break;
    case IOD_OBJ_KV:
        IOD_OBJID_SETTYPE(tmp_id, IOD_OBJ_KV)
        break;
    case IOD_OBJ_BLOB:
        /* This is for HDF5 committed datatypes and not for VLEN BLOBs */
        tmp_id &= ~(((uint64_t)0x1) << 59);
        IOD_OBJID_SETTYPE(tmp_id, IOD_OBJ_BLOB)
        break;
    case IOD_OBJ_ANY:
    case IOD_OBJ_INVALID:
    default:
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "bad object type");
    }

    /* toggle the owner bit */
    IOD_OBJID_SETOWNER_APP(tmp_id)

    *id = tmp_id;
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_gen_obj_id */


/*-------------------------------------------------------------------------
 * Function:    H5VL_iod_pre_write
 *
 * Depending on the type, this routine generates all the necessary
 * parameters for forwarding a write call to IOD. It sets up the
 * Mercury Bulk handle and checksums the data.
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              August, 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_iod_pre_write(hid_t type_id, H5S_t *space, const void *buf, 
                   /*out*/uint64_t *_checksum, 
                   /*out*/uint64_t *_vlen_checksum, 
                   /*out*/hg_bulk_t *bulk_handle,
                   /*out*/hg_bulk_t *vl_len_bulk_handle,
                   /*out*/hg_bulk_segment_t **_vl_segments,
                   /*out*/char **_vl_lengths)
{
    hsize_t buf_size = 0;
    uint64_t checksum = 0;
    size_t nelmts;
    H5VL_iod_type_info_t type_info;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* Get type info */
    if(H5VL_iod_get_type_info(type_id, &type_info) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to get datatype info");

    nelmts = (size_t)H5S_GET_SELECT_NPOINTS(space);

    if(type_info.vls) {
        hg_bulk_segment_t *segments = NULL;
        size_t num_segments = 0;
        char *vl_lengths = NULL;
        size_t vl_lengths_size = 0;

        HDassert(_vl_segments);
        HDassert(_vl_lengths);

        /* Create segments and vl lengths */
        if(H5VL_iod_create_segments_send((char *)buf, &type_info, nelmts, &segments, &num_segments, 
                                         &vl_lengths, &vl_lengths_size, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't create segments for bulk data transfer");
        HDassert(segments);
        HDassert(vl_lengths);

        /* Register vl lengths */
        if(HG_SUCCESS != HG_Bulk_handle_create(vl_lengths, vl_lengths_size, 
                                               HG_BULK_READ_ONLY, vl_len_bulk_handle))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't create Bulk Data Handle for vlen lengths");

        /* Register non-contiguous memory segments */
        if(HG_SUCCESS != HG_Bulk_handle_create_segments(segments, num_segments, 
                                                        HG_BULK_READ_ONLY, bulk_handle))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't create Bulk Data handle");

        if(_checksum)
            checksum = H5_checksum_crc64_segments(segments, num_segments);
        if(_vlen_checksum)
            *_vlen_checksum = H5_checksum_crc64(vl_lengths, vl_lengths_size);

        *_vl_segments = segments;
        *_vl_lengths = vl_lengths;
    }
    else {
        H5T_t *dt = NULL;
        size_t type_size;

        if(NULL == (dt = (H5T_t *)H5I_object_verify(type_id, H5I_DATATYPE)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, H5T_NO_CLASS, "not a datatype")

        *vl_len_bulk_handle = HG_BULK_NULL;
        type_size = H5T_get_size(dt);

        buf_size = type_size * nelmts;

        if(_checksum) {
            checksum = H5S_checksum(buf, type_size, nelmts, space);
        }

        /* If the memory selection is contiguous, create simple HG Bulk Handle */
        if(H5S_select_is_contiguous(space)) {
            /* Register memory with bulk_handle */
            if(HG_SUCCESS != HG_Bulk_handle_create(buf, (size_t)buf_size, 
                                                   HG_BULK_READ_ONLY, bulk_handle))
                HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "can't create Bulk Data Handle");
        }

        /* if the memory selection is non-contiguous, create a segmented selection */
        else {
            hsize_t *off = NULL; /* array that contains the memory addresses of the memory selection */
            size_t *len = NULL; /* array that contains the length of a contiguous block at each address */
            size_t count = 0; /* number of offset/length entries in selection */
            size_t i;
            hg_bulk_segment_t *bulk_segments = NULL;
            uint8_t *start_offset = (uint8_t *) buf;

            /* generate the offsets/lengths pair arrays from the memory dataspace selection */
            if(H5S_get_offsets(space, type_size, nelmts, &off, &len, &count) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "can't retrieve offets/lengths of memory space");

            /* Register memory with segmented HG handle */
            bulk_segments = (hg_bulk_segment_t *)malloc(count * sizeof(hg_bulk_segment_t));
            for (i = 0; i < count ; i++) {
                bulk_segments[i].address = (void *)(start_offset + off[i]);
                bulk_segments[i].size = len[i];
            }

            /* create Bulk handle */
            if (HG_SUCCESS != HG_Bulk_handle_create_segments(bulk_segments, count, 
                                                             HG_BULK_READ_ONLY, bulk_handle))
                HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "can't create Bulk Data Handle");

            /* cleanup */
            if(count) {
                free(bulk_segments);
                bulk_segments = NULL;
                free(len);
                len = NULL;
                free(off);
                off = NULL;
            }
        }
    }

    if(_checksum) {
        *_checksum = checksum;
    }

    H5VL_iod_type_info_reset(&type_info);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_pre_write */


/*-------------------------------------------------------------------------
 * Function:    H5VL_iod_pre_read
 *
 * Depending on the type, this routine generates all the necessary
 * parameters for forwarding a write call to IOD. It sets up the
 * Mercury Bulk handle and checksums the data. 
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              August, 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_iod_pre_read(hid_t type_id, H5S_t *space, const void *buf, hssize_t nelmts,
                  /*out*/hg_bulk_t *bulk_handle)
{
    size_t buf_size = 0;
    H5T_t *dt = NULL;
    size_t type_size;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (dt = (H5T_t *)H5I_object_verify(type_id, H5I_DATATYPE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, H5T_NO_CLASS, "not a datatype")

    type_size = H5T_get_size(dt);
    buf_size = type_size * (size_t)nelmts;

    /* If the memory selection is contiguous, create simple HG Bulk Handle */
    if(H5S_select_is_contiguous(space)) {
        /* Register memory with bulk_handle */
        if(HG_SUCCESS != HG_Bulk_handle_create(buf, buf_size, 
                                               HG_BULK_READWRITE, bulk_handle))
            HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "can't create Bulk Data Handle");
    }

    /* if the memory selection is non-contiguous, create a segmented selection */
    else {
        hsize_t *off = NULL; /* array that contains the memory addresses of the memory selection */
        size_t *len = NULL; /* array that contains the length of a contiguous block at each address */
        size_t count = 0; /* number of offset/length entries in selection */
        size_t i;
        hg_bulk_segment_t *bulk_segments = NULL;
        uint8_t *start_offset = (uint8_t *) buf;

        /* generate the offsets/lengths pair arrays from the memory dataspace selection */
        if(H5S_get_offsets(space, type_size, (size_t)nelmts, &off, &len, &count) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "can't retrieve offets/lengths of memory space");

        /* Register memory with segmented HG handle */
        bulk_segments = (hg_bulk_segment_t *)malloc(count * sizeof(hg_bulk_segment_t));
        for (i = 0; i < count ; i++) {
            bulk_segments[i].address = (void *)(start_offset + off[i]);
            bulk_segments[i].size = len[i];
        }

        /* create Bulk handle */
        if (HG_SUCCESS != HG_Bulk_handle_create_segments(bulk_segments, count, 
                                                         HG_BULK_READWRITE, bulk_handle))
            HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "can't create Bulk Data Handle");

        /* cleanup */
        if(count) {
            free(bulk_segments);
            bulk_segments = NULL;
            free(len);
            len = NULL;
            free(off);
            off = NULL;
        }
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_pre_read */


/*-------------------------------------------------------------------------
 * Function:    H5VL__iod_vl_map_get_finalize
 *
 * Finalize the data read by deserializing it into the user's buffer.
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              August, 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL__iod_vl_map_get_finalize(size_t buf_size, void *read_buf,
                              void *user_buf, hid_t mem_type_id)
{
    H5T_t *mem_dt = NULL;
    H5T_t *super = NULL;
    size_t super_size;
    H5T_class_t dt_class;
    uint8_t *buf_ptr = (uint8_t *)read_buf;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (mem_dt = (H5T_t *)H5I_object_verify(mem_type_id, H5I_DATATYPE)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, H5T_NO_CLASS, "not a datatype")
    if(NULL == (super = H5T_get_super(mem_dt)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid super type of VL type");
    
    super_size = H5T_get_size(super);
    dt_class = H5T_get_class(mem_dt, FALSE);

#if 0
    {
        size_t seq_len = buf_size, u;

        if(H5T_STRING == dt_class)
            fprintf(stderr, "String Length %zu: %s\n", buf_size, (char *)read_buf);
        else if(H5T_VLEN == dt_class) {
            int *ptr = (int *)buf_ptr;
            fprintf(stderr, "Sequence Count %zu: ", buf_size/sizeof(int));
            for(u=0 ; u<buf_size/sizeof(int) ; ++u)
                fprintf(stderr, "%d ", ptr[u]);
            fprintf(stderr, "\n");
        }
    }
#endif

    if(H5T_VLEN == dt_class) {
        hvl_t *vl = (hvl_t *)user_buf;

        vl->len = buf_size/super_size;
        vl->p = malloc(buf_size);
        HDmemcpy(vl->p, buf_ptr, buf_size);
        buf_ptr += buf_size;
    }
    else if(H5T_STRING == dt_class) {
        char **buf = (char **)user_buf;

        //elmt_size = *((size_t *)buf_ptr);
        //buf_ptr += sizeof(size_t);

        *buf = HDstrdup((char *)buf_ptr);
        //buf_ptr += elmt_size;
    }

done:
    if(super && H5T_close(super) < 0)
        HDONE_ERROR(H5E_DATATYPE, H5E_CANTDEC, FAIL, "can't close super type")
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__iod_vl_map_get_finalize */


/*-------------------------------------------------------------------------
 * Function:    H5VL_iod_map_get_size
 *
 * Purpose:     Retrieves the size of a Key or Value binary 
 *              buffer given its datatype and buffer contents.
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              August, 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_iod_map_get_size(hid_t type_id, const void *buf, 
                      /*out*/uint64_t *checksum, 
                      /*out*/size_t *size, /*out*/H5T_class_t *ret_class)
{
    size_t buf_size = 0;
    H5T_t *dt = NULL;
    H5T_class_t dt_class;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (dt = (H5T_t *)H5I_object_verify(type_id, H5I_DATATYPE)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, H5T_NO_CLASS, "not a datatype")

    dt_class = H5T_get_class(dt, FALSE);

    switch(dt_class) {
        case H5T_STRING:
            /* If this is a variable length string, get the size using strlen(). */
            if(H5T_is_variable_str(dt)) {
                buf_size = HDstrlen((const char*)buf) + 1;

                /* compute checksum */
                *checksum = H5_checksum_crc64(buf, buf_size);
                break;
            }
        case H5T_INTEGER:
        case H5T_FLOAT:
        case H5T_TIME:
        case H5T_BITFIELD:
        case H5T_OPAQUE:
        case H5T_ENUM:
        case H5T_ARRAY:
        case H5T_NO_CLASS:
        case H5T_REFERENCE:
        case H5T_NCLASSES:
        case H5T_COMPOUND:
            /* Data is not variable length, so use H5Tget_size() */
            /* MSC - This is not correct. Compound/Array can contian
               VL datatypes, but for now we don't support that. Need
               to check for that too */
            buf_size = H5T_get_size(dt);

            /* compute checksum */
            *checksum = H5_checksum_crc64(buf, buf_size);
            break;

            /* If this is a variable length datatype, iterate over it */
        case H5T_VLEN:
            {
                H5T_t *super = NULL;
                const hvl_t *vl;

                vl = (const hvl_t *)buf;

                if(NULL == (super = H5T_get_super(dt)))
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid super type of VL type");

                buf_size = H5T_get_size(super) * vl->len;

                /* compute checksum */
                *checksum = H5_checksum_crc64(vl->p, buf_size);
                H5T_close(super);
                break;
            }
        default:
            HGOTO_ERROR(H5E_ARGS, H5E_CANTINIT, FAIL, "unsupported datatype");
    }
    *size = buf_size;
    if(ret_class)
        *ret_class = dt_class;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_map_get_size */


/*-------------------------------------------------------------------------
 * Function:    H5VL_iod_map_dtype_info
 *
 * Purpose:     Retrieves information about the datatype of Map Key or
 *              value datatype, whether it's VL or not. If it is not VL
 *              return the size.
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              August, 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_iod_map_dtype_info(hid_t type_id, /*out*/ hbool_t *is_vl, /*out*/size_t *size)
{
    size_t buf_size = 0;
    H5T_t *dt = NULL;
    H5T_class_t dt_class;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (dt = (H5T_t *)H5I_object_verify(type_id, H5I_DATATYPE)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, H5T_NO_CLASS, "not a datatype")

    dt_class = H5T_get_class(dt, FALSE);

    switch(dt_class) {
        case H5T_STRING:
            /* If this is a variable length string, get the size using strlen(). */
            if(H5T_is_variable_str(dt)) {
                *is_vl = TRUE;
                break;
            }
        case H5T_INTEGER:
        case H5T_FLOAT:
        case H5T_TIME:
        case H5T_BITFIELD:
        case H5T_OPAQUE:
        case H5T_ENUM:
        case H5T_ARRAY:
        case H5T_NO_CLASS:
        case H5T_REFERENCE:
        case H5T_NCLASSES:
        case H5T_COMPOUND:
            /* Data is not variable length, so use H5Tget_size() */
            /* MSC - This is not correct. Compound/Array can contian
               VL datatypes, but for now we don't support that. Need
               to check for that too */
            buf_size = H5T_get_size(dt);
            *is_vl = FALSE;
            break;

            /* If this is a variable length datatype, iterate over it */
        case H5T_VLEN:
            *is_vl = TRUE;
            break;
        default:
            HGOTO_ERROR(H5E_ARGS, H5E_CANTINIT, FAIL, "unsupported datatype");
    }

    if(size)
        *size = buf_size;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_map_get_size */

#if 0
static herr_t
H5VL_generate_axe_ids(int myrank, int nranks, uint64_t *start_id)
{
    uint64_t seed;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    seed = (pow(2,64) - 1) / nranks;
    *start_id = seed * my_rank;

done:
    FUNC_LEAVE_NOAPI(ret_value)
}

static herr_t
H5VL_iod_get_axe_id(int myrank, int nranks, int cur_index, uint64_t *id)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT



done:
    FUNC_LEAVE_NOAPI(ret_value)
}

/*-------------------------------------------------------------------------
 * Function:    H5VL_iod_get_parent_info
 *
 * Purpose:     This routine traverses the path in name, or in loc_params 
 *              if the path is specified there, to determine the components
 *              of the path that are present locally in the ID space. 
 *              Once a component in the path is not found, the routine
 *              breaks at that point and stores the remaining path in new_name.
 *              This is where the traversal can begin at the server. 
 *              The IOD ID, OH, and axe_id belonging to the last object 
 *              present are returned too. 
 *
 * Return:	Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_iod_get_parent_info(H5VL_iod_object_t *obj, H5VL_loc_params_t loc_params, 
                         const char *name, /*OUT*/iod_obj_id_t *iod_id, 
                         /*OUT*/iod_handle_t *iod_oh, /*OUT*/H5VL_iod_request_t **parent_req, 
                         /*OUT*/char **new_name, /*OUT*/H5VL_iod_object_t **last_obj)
{
    iod_obj_id_t cur_id;
    iod_handle_t cur_oh;
    size_t cur_size; /* current size of the path traversed so far */
    char *cur_name;  /* full path to object that is currently being looked for */
    H5VL_iod_object_t *cur_obj = obj;   /* current object in the traversal loop */
    H5VL_iod_object_t *next_obj = NULL; /* the next object to traverse */
    const char *path;        /* specified path for the object to traverse to */
    H5WB_t *wb = NULL;       /* Wrapped buffer for temporary buffer */
    char comp_buf[1024];     /* Temporary buffer for path components */
    char *comp;              /* Pointer to buffer for path components */
    size_t nchars;	     /* component name length	*/
    H5VL_iod_file_t *file = obj->file; /* pointer to file where the search happens */
    hbool_t last_comp = FALSE; /* Flag to indicate that a component is the last component in the name */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    if(loc_params.type == H5VL_OBJECT_BY_SELF)
        path = name;
    else if (loc_params.type == H5VL_OBJECT_BY_NAME)
        path = loc_params.loc_data.loc_by_name.name;

    if (NULL == (cur_name = (char *)malloc(HDstrlen(obj->obj_name) + HDstrlen(path) + 2)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate");

    HDstrcpy(cur_name, obj->obj_name);
    cur_size = HDstrlen(obj->obj_name);

    if(obj->obj_type != H5I_FILE) {
        HDstrcat(cur_name, "/");
        cur_size += 1;
    }
        
    /* Wrap the local buffer for serialized header info */
    if(NULL == (wb = H5WB_wrap(comp_buf, sizeof(comp_buf))))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't wrap buffer")
    /* Get a pointer to a buffer that's large enough  */
    if(NULL == (comp = (char *)H5WB_actual(wb, (HDstrlen(path) + 1))))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't get actual buffer")

    /* Traverse the path */
    while((path = H5G__component(path, &nchars)) && *path) {
        const char *s;                  /* Temporary string pointer */

	/*
	 * Copy the component name into a null-terminated buffer so
	 * we can pass it down to the other symbol table functions.
	 */
	HDmemcpy(comp, path, nchars);
	comp[nchars] = '\0';

	/*
	 * The special name `.' is a no-op.
	 */
	if('.' == comp[0] && !comp[1]) {
	    path += nchars;
	    continue;
	} /* end if */

        /* Check if this is the last component of the name */
        if(!((s = H5G__component(path + nchars, NULL)) && *s))
            last_comp = TRUE;

        HDstrcat(cur_name, comp);
        cur_size += nchars;
        cur_name[cur_size] = '\0';

        if(NULL == (next_obj = (const H5VL_iod_object_t *)H5I_search_name(file, cur_name, H5I_GROUP))) {
            if(last_comp) {
                if(NULL == (next_obj = (const H5VL_iod_object_t *)H5I_search_name
                            (file, cur_name, H5I_DATASET))
                   && NULL == (next_obj = (H5VL_iod_object_t *)H5I_search_name
                               (file, cur_name, H5I_DATATYPE))
                   && NULL == (next_obj = (H5VL_iod_object_t *)H5I_search_name
                               (file, cur_name, H5I_MAP)))
                    break;
            }
            else {
                break;
            }
        }

#if H5VL_IOD_DEBUG
        printf("Found %s Locally\n", comp);
#endif

	/* Advance to next component in string */
	path += nchars;
        HDstrcat(cur_name, "/");
        cur_size += 1;
        cur_obj = next_obj;
    }

    switch(cur_obj->obj_type) {
        case H5I_FILE:
            cur_oh = cur_obj->file->remote_file.root_oh;
            cur_id = cur_obj->file->remote_file.root_id;
            break;
        case H5I_GROUP:
            cur_oh = ((const H5VL_iod_group_t *)cur_obj)->remote_group.iod_oh;
            cur_id = ((const H5VL_iod_group_t *)cur_obj)->remote_group.iod_id;
            break;
        case H5I_DATASET:
            cur_oh = ((const H5VL_iod_dset_t *)cur_obj)->remote_dset.iod_oh;
            cur_id = ((const H5VL_iod_dset_t *)cur_obj)->remote_dset.iod_id;
            break;
        case H5I_DATATYPE:
            cur_oh = ((const H5VL_iod_dtype_t *)cur_obj)->remote_dtype.iod_oh;
            cur_id = ((const H5VL_iod_dtype_t *)cur_obj)->remote_dtype.iod_id;
            break;
        case H5I_MAP:
            cur_oh = ((const H5VL_iod_map_t *)cur_obj)->remote_map.iod_oh;
            cur_id = ((const H5VL_iod_map_t *)cur_obj)->remote_map.iod_id;
            break;
        default:
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "bad location object");
    }

    if(cur_obj->request && cur_obj->request->status == H5ES_STATUS_IN_PROGRESS) {
        *parent_req = cur_obj->request;
        cur_obj->request->ref_count ++;
    }
    else {
        *parent_req = NULL;
        HDassert(cur_oh.cookie != IOD_OH_UNDEFINED);
    }

    *iod_id = cur_id;
    *iod_oh = cur_oh;

    if(*path)
        *new_name = strdup(path);
    else
        *new_name = strdup(".");

    if(last_obj)
        *last_obj = cur_obj;

done:
    free(cur_name);
    /* Release temporary component buffer */
    if(wb && H5WB_unwrap(wb) < 0)
        HDONE_ERROR(H5E_SYM, H5E_CANTRELEASE, FAIL, "can't release wrapped buffer")
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_get_parent_info */


/*-------------------------------------------------------------------------
 * Function:	H5I_search_name
 *
 * Purpose:     return pointer to object with given full path name
 *
 * Return:	Success:	pointer to object
 *		Failure:	NULL (not found)
 *
 * Programmer:	Mohamad Chaarawi
 *              March 2013
 *
 *-------------------------------------------------------------------------
 */
const void *
H5I_search_name(void *_file, char *name, H5I_type_t type)
{
    H5I_id_type_t   *type_ptr;      /*ptr to the type       */
    const void *ret_value = NULL;      /* Return value */

    FUNC_ENTER_NOAPI(NULL)

    type_ptr = H5I_id_type_list_g[type];
    if(type_ptr == NULL || type_ptr->count <= 0)
        HGOTO_ERROR(H5E_ATOM, H5E_BADGROUP, NULL, "invalid type")

    /* Only iterate through hash table if there are IDs in group */
    if(type_ptr->ids > 0) {
        H5I_id_info_t *id_ptr;      /*ptr to the new ID     */
        unsigned i;                 /*counter               */

        /* Start at the beginning of the array */
        for(i = 0; i < type_ptr->cls->hash_size; i++) {
            id_ptr = type_ptr->id_list[i];
            while(id_ptr) {
                H5VL_iod_file_t *file = (H5VL_iod_file_t *)_file;

                switch(type) {
                case H5I_GROUP:
                    {
                        const H5VL_iod_group_t *grp = (const H5VL_iod_group_t *)id_ptr->obj_ptr;

                        if (file == grp->common.file &&
                            0 == strcmp(grp->common.obj_name, name)) {
                            ret_value = id_ptr->obj_ptr;
                            HGOTO_DONE(id_ptr->obj_ptr);
                        }
                        break;
                    }
                case H5I_DATASET:
                    {
                        const H5VL_iod_dset_t *dset = (const H5VL_iod_dset_t *)id_ptr->obj_ptr;

                        if (file == dset->common.file && 
                            0 == strcmp(dset->common.obj_name, name)) {
                            ret_value = id_ptr->obj_ptr;
                            HGOTO_DONE(id_ptr->obj_ptr);
                        }
                        break;
                    }
                case H5I_MAP:
                    {
                        const H5VL_iod_map_t *map = (const H5VL_iod_map_t *)id_ptr->obj_ptr;

                        if (file == map->common.file &&
                            0 == strcmp(map->common.obj_name, name)) {
                            ret_value = id_ptr->obj_ptr;
                            HGOTO_DONE(id_ptr->obj_ptr);
                        }
                        break;
                    }
                case H5I_DATATYPE:
                    {
                        const H5T_t *dt = (const H5T_t *)id_ptr->obj_ptr;

                        if(H5T_committed(dt)) {
                            const H5VL_iod_dtype_t *dtype ;

                            dtype = (const H5VL_iod_dtype_t *)H5T_get_named_type(dt);

                            if (file == dtype->common.file && 
                                0 == strcmp(dtype->common.obj_name, name)) {
                                ret_value = id_ptr->obj_ptr;
                                HGOTO_DONE(id_ptr->obj_ptr);
                            }
                        }
                        break;
                    }
                default:
                    HGOTO_ERROR(H5E_ATOM, H5E_BADGROUP, NULL, "invalid type")
                }
                id_ptr = id_ptr->next;
            } /* end while */
        } /* end for */
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5I_search_name() */

#endif

#endif /* H5_HAVE_EFF */
