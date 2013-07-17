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

#include "H5private.h"		/* Generic Functions			*/
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
#include "H5WBprivate.h"        /* Wrapped Buffers                      */

#ifdef H5_HAVE_EFF

H5FL_EXTERN(H5VL_iod_file_t);
H5FL_EXTERN(H5VL_iod_attr_t);
H5FL_EXTERN(H5VL_iod_group_t);
H5FL_EXTERN(H5VL_iod_dset_t);
H5FL_EXTERN(H5VL_iod_dtype_t);


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
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(request);

    if (file->request_list_tail) {
        file->request_list_tail->next = request;
        request->prev = file->request_list_tail;
        file->request_list_tail = request;
    }
    else {
        file->request_list_head = request;
        file->request_list_tail = request;
        request->prev = NULL;
    }
    request->next = NULL;

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

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(request);

    prev = request->prev;
    next = request->next;
    if (prev) {
        if (next) {
            prev->next = next;
            next->prev = prev;
        }
        else {
            prev->next = NULL;
            file->request_list_tail = prev;
        }
    }
    else {
        if (next) {
            next->prev = NULL;
            file->request_list_head = next;
        }
        else {
            file->request_list_head = NULL;
            file->request_list_tail = NULL;
        }
    }

    request->obj->request = NULL;
    request->prev = NULL;
    request->next = NULL;

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
            fprintf(stderr, "failed to wait on request\n");
            request->status = H5AO_FAILED;
            request->state = H5VL_IOD_COMPLETED;
            H5VL_iod_request_delete(file, request);
            break;
        }
        else {
            if(status) {
                request->status = H5AO_SUCCEEDED;
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

                    tmp_req = cur_req->next;

                    HDassert(cur_req->state == H5VL_IOD_PENDING);
                    ret = HG_Wait(*((hg_request_t *)cur_req->req), 0, &tmp_status);
                    if(HG_FAIL == ret) {
                        fprintf(stderr, "failed to wait on request\n");
                        cur_req->status = H5AO_FAILED;
                        cur_req->state = H5VL_IOD_COMPLETED;
                        H5VL_iod_request_delete(file, cur_req);
                    }
                    else {
                        if(tmp_status) {
                            cur_req->status = H5AO_SUCCEEDED;
                            cur_req->state = H5VL_IOD_COMPLETED;
                            if(H5VL_iod_request_complete(file, cur_req) < 0) {
                                fprintf(stderr, "Operation Failed!\n");
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
                fprintf(stderr, "Operation Failed!\n");
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

        tmp_req = cur_req->next;

        HDassert(cur_req->state == H5VL_IOD_PENDING);
        ret = HG_Wait(*((hg_request_t *)cur_req->req), HG_MAX_IDLE_TIME, &status);
        if(HG_FAIL == ret) {
            fprintf(stderr, "failed to wait on request\n");
            cur_req->status = H5AO_FAILED;
            cur_req->state = H5VL_IOD_COMPLETED;
        }
        else {
            HDassert(status);
            cur_req->status = H5AO_SUCCEEDED;
            cur_req->state = H5VL_IOD_COMPLETED;
        }

        if(H5VL_iod_request_complete(file, cur_req) < 0)
            fprintf(stderr, "Operation Failed!\n");

        cur_req = tmp_req;
    }

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

        tmp_req = cur_req->next;

        /* If the request is pending on the object we want, complete it */
        if(cur_req->obj == object) {
            HDassert(cur_req->state == H5VL_IOD_PENDING);
            ret = HG_Wait(*((hg_request_t *)cur_req->req), HG_MAX_IDLE_TIME, 
                          &status);
            if(HG_FAIL == ret) {
                fprintf(stderr, "failed to wait on request\n");
                cur_req->status = H5AO_FAILED;
                cur_req->state = H5VL_IOD_COMPLETED;
                H5VL_iod_request_delete(file, cur_req);
            }
            else {
                HDassert(status);
                cur_req->status = H5AO_SUCCEEDED;
                cur_req->state = H5VL_IOD_COMPLETED;
                if(H5VL_iod_request_complete(file, cur_req) < 0)
                    fprintf(stderr, "Operation Failed!\n");
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
    case HG_FILE_CREATE:
    case HG_FILE_OPEN:
        if(IOD_OH_UNDEFINED == req->obj->file->remote_file.coh.cookie) {
            fprintf(stderr, "failed to create/open file\n");
            req->status = H5AO_FAILED;
            req->state = H5VL_IOD_COMPLETED;
        }

        H5VL_iod_request_delete(file, req);
        break;
    case HG_ATTR_CREATE:
    case HG_ATTR_OPEN:
        {
            H5VL_iod_attr_t *attr = (H5VL_iod_attr_t *)req->obj;

            if(IOD_OH_UNDEFINED == attr->remote_attr.iod_oh.cookie) {
                fprintf(stderr, "failed to create/open Attribute\n");
                req->status = H5AO_FAILED;
                req->state = H5VL_IOD_COMPLETED;
            }

            H5VL_iod_request_delete(file, req);
            break;
        }
    case HG_GROUP_CREATE:
    case HG_GROUP_OPEN:
        {
            H5VL_iod_group_t *group = (H5VL_iod_group_t *)req->obj;

            if(IOD_OH_UNDEFINED == group->remote_group.iod_oh.cookie) {
                fprintf(stderr, "failed to create/open Group\n");
                req->status = H5AO_FAILED;
                req->state = H5VL_IOD_COMPLETED;
            }

            H5VL_iod_request_delete(file, req);
            break;
        }
    case HG_MAP_CREATE:
    case HG_MAP_OPEN:
        {
            H5VL_iod_map_t *map = (H5VL_iod_map_t *)req->obj;

            if(IOD_OH_UNDEFINED == map->remote_map.iod_oh.cookie) {
                fprintf(stderr, "failed to create/open Map\n");
                req->status = H5AO_FAILED;
                req->state = H5VL_IOD_COMPLETED;
            }

            H5VL_iod_request_delete(file, req);
            break;
        }
    case HG_DSET_CREATE:
    case HG_DSET_OPEN:
        {
            H5VL_iod_dset_t *dset = (H5VL_iod_dset_t *)req->obj;

            if(IOD_OH_UNDEFINED == dset->remote_dset.iod_oh.cookie) {
                fprintf(stderr, "failed to create/open Dataset\n");
                req->status = H5AO_FAILED;
                req->state = H5VL_IOD_COMPLETED;
            }

            H5VL_iod_request_delete(file, req);
            break;
        }
    case HG_DTYPE_COMMIT:
    case HG_DTYPE_OPEN:
        {
            H5VL_iod_dtype_t *dtype = (H5VL_iod_dtype_t *)req->obj;

            if(IOD_OH_UNDEFINED == dtype->remote_dtype.iod_oh.cookie) {
                fprintf(stderr, "failed to create/open Attribute\n");
                req->status = H5AO_FAILED;
                req->state = H5VL_IOD_COMPLETED;
            }

            H5VL_iod_request_delete(file, req);
            break;
        }
    case HG_DSET_WRITE:
    case HG_DSET_READ:
        {
            H5VL_iod_io_info_t *info = (H5VL_iod_io_info_t *)req->data;

            /* Free memory handle */
            if(HG_SUCCESS != HG_Bulk_handle_free(*info->bulk_handle)) {
                fprintf(stderr, "failed to free dataset bulk handle\n");
                req->status = H5AO_FAILED;
                req->state = H5VL_IOD_COMPLETED;
            }
            if(HG_DSET_WRITE == req->type && SUCCEED != *((int *)info->status)) {
                fprintf(stderr, "Errrr! Dataset Write Failure Reported from Server\n");
                req->status = H5AO_FAILED;
                req->state = H5VL_IOD_COMPLETED;
            }
            else if(HG_DSET_READ == req->type) {
                H5VL_iod_read_status_t *read_status = (H5VL_iod_read_status_t *)info->status;

                if(SUCCEED != read_status->ret) {
                    fprintf(stderr, "Errrrr!  Dataset Read Failure Reported from Server\n");
                    req->status = H5AO_FAILED;
                    req->state = H5VL_IOD_COMPLETED;
                }
                else {
                    uint32_t internal_cs = 0;

                    /* calculate a checksum for the data recieved */
                    internal_cs = H5S_checksum(info->buf_ptr, info->type_size, 
                                               info->nelmts, info->space);

                    /* verify data integrity */
                    if(internal_cs != read_status->cs) {
                        fprintf(stderr, "Errrrr!  Dataset Read integrity failure (expecting %u got %u).\n",
                                read_status->cs, internal_cs);
                        req->status = H5AO_FAILED;
                        req->state = H5VL_IOD_COMPLETED;
                    }
                    if(info->space && H5S_close(info->space) < 0)
                        HDONE_ERROR(H5E_DATASPACE, H5E_CANTRELEASE, FAIL, "unable to release dataspace");

                    /* If the app gave us a buffer to store the checksum, then put it there */
                    if(info->cs_ptr)
                        *info->cs_ptr = internal_cs;
                }
            }

            free(info->status);
            info->status = NULL;
            info->bulk_handle = (hg_bulk_t *)H5MM_xfree(info->bulk_handle);
            info = (H5VL_iod_io_info_t *)H5MM_xfree(info);
            req->data = NULL;
            H5VL_iod_request_delete(file, req);
            break;
        }
    case HG_ATTR_WRITE:
    case HG_ATTR_READ:
        {
            H5VL_iod_io_info_t *info = (H5VL_iod_io_info_t *)req->data;

            /* Free memory handle */
            if(HG_SUCCESS != HG_Bulk_handle_free(*info->bulk_handle)) {
                fprintf(stderr, "failed to free attribute bulk handle\n");
                req->status = H5AO_FAILED;
                req->state = H5VL_IOD_COMPLETED;
            }
            if(SUCCEED != *((int *)info->status)) {
                fprintf(stderr, "Attribute I/O Failure Reported from Server\n");
                req->status = H5AO_FAILED;
                req->state = H5VL_IOD_COMPLETED;
            }

            free(info->status);
            info->status = NULL;
            info->bulk_handle = (hg_bulk_t *)H5MM_xfree(info->bulk_handle);
            info = (H5VL_iod_io_info_t *)H5MM_xfree(info);
            req->data = NULL;
            H5VL_iod_request_delete(file, req);
            break;
        }
    case HG_MAP_SET:
        {
            int *status = (int *)req->data;

            if(SUCCEED != *status)
                HGOTO_ERROR(H5E_FILE, H5E_CANTSET, FAIL, "MAP set failed at the server");

            free(status);
            req->data = NULL;
            H5VL_iod_request_delete(file, req);
            break;
        }
    case HG_MAP_DELETE:
        {
            int *status = (int *)req->data;

            if(SUCCEED != *status)
                HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "MAP delete failed at the server");

            free(status);
            req->data = NULL;
            H5VL_iod_request_delete(file, req);
            break;
        }
    case HG_MAP_GET:
        {
            map_get_out_t *output = (map_get_out_t *)req->data;

            if(SUCCEED != output->ret)
                HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "MAP get failed at the server");

            free(output);
            req->data = NULL;
            H5VL_iod_request_delete(file, req);
            break;
        }
    case HG_MAP_GET_COUNT:
        {
            hsize_t *count = (hsize_t *)req->data;

            if(*count < 0)
                HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "MAP get_count failed at the server");

            req->data = NULL;
            H5VL_iod_request_delete(file, req);
            break;
        }
    case HG_MAP_EXISTS:
        {
            htri_t *exists = (hbool_t *)req->data;

            if(*exists < 0)
                HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "MAP exists failed at the server");

            req->data = NULL;
            H5VL_iod_request_delete(file, req);
            break;
        }
    case HG_FILE_FLUSH:
        {
            int *status = (int *)req->data;

            if(SUCCEED != *status)
                HGOTO_ERROR(H5E_FILE, H5E_CANTFLUSH, FAIL, "file flush failed at the server");

            free(status);
            req->data = NULL;
            file->common.request = NULL;
            H5VL_iod_request_delete(file, req);
            break;
        }
    case HG_FILE_CLOSE:
        {
            int *status = (int *)req->data;

            if(SUCCEED != *status)
                HGOTO_ERROR(H5E_FILE, H5E_CANTDEC, FAIL, "file close failed at the server");

            free(status);
            req->data = NULL;
            file->common.request = NULL;
            H5VL_iod_request_delete(file, req);

            /* free everything */
            free(file->file_name);
            free(file->common.obj_name);
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

            if(SUCCEED != *status)
                HGOTO_ERROR(H5E_ATTR, H5E_CANTDEC, FAIL, "attr rename failed at the server");

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

            if(SUCCEED != *status)
                HGOTO_ERROR(H5E_ATTR, H5E_CANTDEC, FAIL, "attr remove failed at the server");

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

            if(*ret < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "exists operation failed at the server");

            req->data = NULL;
            obj->request = NULL;
            H5VL_iod_request_delete(file, req);
            break;
        }
    case HG_ATTR_CLOSE:
        {
            int *status = (int *)req->data;
            H5VL_iod_attr_t *attr = (H5VL_iod_attr_t *)req->obj;

            if(SUCCEED != *status)
                HGOTO_ERROR(H5E_FILE, H5E_CANTDEC, FAIL, "attr close failed at the server");

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
            if(attr->remote_attr.acpl_id != H5P_ATTRIBUTE_CREATE_DEFAULT &&
               H5Pclose(attr->remote_attr.acpl_id) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "failed to close plist");
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

            if(SUCCEED != *status)
                HGOTO_ERROR(H5E_FILE, H5E_CANTDEC, FAIL, "group close failed at the server");

            free(status);
            req->data = NULL;
            grp->common.request = NULL;
            H5VL_iod_request_delete(file, req);

            /* free group components */
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

            if(SUCCEED != *status)
                HGOTO_ERROR(H5E_FILE, H5E_CANTSET, FAIL, "Dataset set extent failed at the server");

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

            if(SUCCEED != *status)
                HGOTO_ERROR(H5E_FILE, H5E_CANTDEC, FAIL, "dset close failed at the server");

            free(status);
            req->data = NULL;
            dset->common.request = NULL;
            H5VL_iod_request_delete(file, req);

            /* free dset components */
            free(dset->common.obj_name);
            if(dset->common.comment)
                HDfree(dset->common.comment);
            if(dset->remote_dset.dcpl_id != H5P_DATASET_CREATE_DEFAULT &&
               H5Pclose(dset->remote_dset.dcpl_id) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "failed to close plist");
            if(dset->dapl_id != H5P_DATASET_ACCESS_DEFAULT &&
               H5Pclose(dset->dapl_id) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "failed to close plist");
            if(H5Tclose(dset->remote_dset.type_id) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "failed to close dtype");
            if(H5Sclose(dset->remote_dset.space_id) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "failed to close dspace");
            dset = H5FL_FREE(H5VL_iod_dset_t, dset);
            break;
        }
    case HG_MAP_CLOSE:
        {
            int *status = (int *)req->data;
            H5VL_iod_map_t *map = (H5VL_iod_map_t *)req->obj;

            if(SUCCEED != *status)
                HGOTO_ERROR(H5E_FILE, H5E_CANTDEC, FAIL, "map close failed at the server");

            free(status);
            req->data = NULL;
            map->common.request = NULL;
            H5VL_iod_request_delete(file, req);

            /* free map components */
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

            if(SUCCEED != *status)
                HGOTO_ERROR(H5E_FILE, H5E_CANTDEC, FAIL, "dtype close failed at the server");

            free(status);
            req->data = NULL;
            dtype->common.request = NULL;
            H5VL_iod_request_delete(file, req);

            /* free dtype components */
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

            if(SUCCEED != *status)
                HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Link operation failed at the server");

            free(status);
            req->data = NULL;
            file->common.request = NULL;
            H5VL_iod_request_delete(file, req);
            break;
        }
    case HG_OBJECT_OPEN:
        req->data = NULL;
        H5VL_iod_request_delete(file, req);
        break;
    case HG_OBJECT_GET_COMMENT:
        {
            object_get_comment_out_t *result = (object_get_comment_out_t *)req->data;

            if(SUCCEED != result->ret) {
                fprintf(stderr, "get comment failed\n");
                req->status = H5AO_FAILED;
                req->state = H5VL_IOD_COMPLETED;
            }

            free(result);
            H5VL_iod_request_delete(file, req);
            break;
        }
    case HG_LINK_ITERATE:
    case HG_OBJECT_VISIT:
    default:
        H5VL_iod_request_delete(file, req);
        HGOTO_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "Request Type not supported");
    }
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
    case HG_DSET_WRITE:
    case HG_DSET_READ:
    case HG_ATTR_WRITE:
    case HG_ATTR_READ:
        {
            H5VL_iod_io_info_t *info = (H5VL_iod_io_info_t *)req->data;

            /* Free memory handle */
            if(HG_SUCCESS != HG_Bulk_handle_free(*info->bulk_handle)) {
                fprintf(stderr, "failed to free bulk handle\n");
            }
            free(info->status);
            info->status = NULL;
            info->bulk_handle = (hg_bulk_t *)H5MM_xfree(info->bulk_handle);
            info = (H5VL_iod_io_info_t *)H5MM_xfree(info);
            req->data = NULL;
            H5VL_iod_request_delete(file, req);
            break;
        }
    case HG_FILE_FLUSH:
        {
            int *status = (int *)req->data;

            free(status);
            req->data = NULL;
            file->common.request = NULL;
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
    case HG_ATTR_EXISTS:
    case HG_LINK_EXISTS:
    case HG_OBJECT_EXISTS:
    case HG_MAP_GET_COUNT:
    case HG_MAP_EXISTS:
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
            if(attr->remote_attr.acpl_id != 0 &&
               attr->remote_attr.acpl_id != H5P_ATTRIBUTE_CREATE_DEFAULT &&
               H5Pclose(attr->remote_attr.acpl_id) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "failed to close plist");
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
    case HG_OBJECT_OPEN:
        req->data = NULL;
        H5VL_iod_request_delete(file, req);
        break;
    case HG_LINK_ITERATE:
    case HG_OBJECT_VISIT:
    default:
        H5VL_iod_request_delete(file, req);
        HGOTO_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "Request Type not supported");
    }
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_request_cancel */


/*-------------------------------------------------------------------------
 * Function:    H5VL_iod_get_axe_parents
 *
 * Purpose:     returns the number of axe_id tasks that are associated 
 *              with a particular object. If the parent array is not NULL, 
 *              the axe_ids are returned in parents too.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_iod_get_axe_parents(H5VL_iod_object_t *obj, /*IN/OUT*/ size_t *count, 
                         /*OUT*/ uint64_t *parents)
{
    H5VL_iod_file_t *file = obj->file;
    H5VL_iod_request_t *cur_req = file->request_list_head;
    size_t size = 0;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Loop to complete some requests */
    while(cur_req) {
        /* If the request is pending on the object we want, add its axe_id */
        if(cur_req->obj == obj) {
            if(cur_req->status == H5AO_PENDING){
                if(NULL != parents) {
                    parents[size] = cur_req->axe_id;
                }
                size ++;
            }
        }
        cur_req = cur_req->next;
    }

    *count = size;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5VL_iod_get_axe_parents */


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
                         /*OUT*/iod_handle_t *iod_oh, /*OUT*/uint64_t *axe_id, 
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

        if(NULL == (next_obj = (H5VL_iod_object_t *)H5I_search_name(cur_name, H5I_GROUP))) {
            if(last_comp) {
                if(NULL == (next_obj = (H5VL_iod_object_t *)H5I_search_name(cur_name, H5I_DATASET)))
                    //&& NULL == (cur_obj = (H5VL_iod_object_t *)H5I_search_name(cur_name, H5I_DATATYPE)))
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
            cur_oh = ((H5VL_iod_group_t *)cur_obj)->remote_group.iod_oh;
            cur_id = ((H5VL_iod_group_t *)cur_obj)->remote_group.iod_id;
            break;
        case H5I_DATASET:
            cur_oh = ((H5VL_iod_dset_t *)cur_obj)->remote_dset.iod_oh;
            cur_id = ((H5VL_iod_dset_t *)cur_obj)->remote_dset.iod_id;
            break;
        case H5I_DATATYPE:
            cur_oh = ((H5VL_iod_dtype_t *)cur_obj)->remote_dtype.iod_oh;
            cur_id = ((H5VL_iod_dtype_t *)cur_obj)->remote_dtype.iod_id;
            break;
        default:
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "bad location object");
    }

    if(cur_obj->request && cur_obj->request->status == H5AO_PENDING) {
        *axe_id = cur_obj->request->axe_id;
    }
    else {
        *axe_id = 0;
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
 * Function:    H5VL_iod_get_axe_parents
 *
 * Purpose:     routine to generate an IOD ID based on the object type, 
 *              rank and total ranks, and current index or 
 *              number of pre-existing IDs.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_iod_gen_obj_id(int myrank, int nranks, uint64_t cur_index, 
                    iod_obj_type_t type, uint64_t *id)
{
    herr_t ret_value = SUCCEED;
    uint64_t tmp_id;

    FUNC_ENTER_NOAPI_NOINIT

    /* determine first the rank of the object with the first 59
       bits */
    tmp_id = myrank + (nranks * cur_index);

    /* toggle the object type bits */
    switch(type) {
    case IOD_OBJ_ARRAY:
        tmp_id |= IOD_OBJ_TYPE_ARRAY;
        break;
    case IOD_OBJ_KV:
        tmp_id |= IOD_OBJ_TYPE_KV;
        break;
    case IOD_OBJ_BLOB:
        tmp_id |= IOD_OBJ_TYPE_BLOB;
        break;
    case IOD_OBJ_ANY:
    default:
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "bad object type");
    }

    /* toggle the owner bit */
    tmp_id |= IOD_OBJID_APP;

    *id = tmp_id;
done:
    FUNC_LEAVE_NOAPI(ret_value)
}

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
#endif

#endif /* H5_HAVE_EFF */
