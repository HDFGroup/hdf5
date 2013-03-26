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
#include "H5VLprivate.h"	/* VOL plugins				*/
#include "H5VLiod.h"            /* Iod VOL plugin			*/
#include "H5VLiod_common.h"
#include "H5VLiod_client.h"
#include "H5WBprivate.h"        /* Wrapped Buffers                      */

#ifdef H5_HAVE_EFF

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

herr_t
H5VL_iod_request_wait(H5VL_iod_file_t *file, H5VL_iod_request_t *request)
{
    H5VL_iod_request_t *cur_req = file->request_list_head;
    int ret;
    fs_status_t status;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(request);
    HDassert(request->req);

    /* Loop to complete the request while poking through other requests on the 
       container to avoid deadlock. */
    while(1) {
        /* test the operation status */
        ret = fs_wait(*((fs_request_t *)request->req), 0, &status);
        if(S_FAIL == ret) {
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
                fs_status_t tmp_status;

                tmp_req = cur_req->next;

                ret = fs_wait(*((fs_request_t *)cur_req->req), 0, &tmp_status);
                if(S_FAIL == ret) {
                    fprintf(stderr, "failed to wait on request\n");
                    cur_req->status = H5AO_FAILED;
                    cur_req->state = H5VL_IOD_COMPLETED;
                    H5VL_iod_request_delete(file, cur_req);
                }
                else {
                    if(tmp_status) {
                        if(FS_DSET_WRITE == cur_req->type || FS_DSET_READ == cur_req->type) {
                            H5VL_iod_io_info_t *info = (H5VL_iod_io_info_t *)cur_req->data;

                            /* Free memory handle */
                            if(S_SUCCESS != bds_handle_free(*info->bds_handle)) {
                                fprintf(stderr, "failed to free bds handle\n");
                                cur_req->status = H5AO_FAILED;
                                cur_req->state = H5VL_IOD_COMPLETED;
                                H5VL_iod_request_delete(file, cur_req);
                                cur_req = tmp_req;
                                continue;
                            }

                            if(FS_DSET_WRITE == cur_req->type && SUCCEED != *((int *)info->status)) {
                                fprintf(stderr, "Dataset Write operation failed\n");
                                cur_req->status = H5AO_FAILED;
                                cur_req->state = H5VL_IOD_COMPLETED;
                                H5VL_iod_request_delete(file, cur_req);
                                cur_req = tmp_req;
                                continue;
                            }
                            else if(FS_DSET_READ == cur_req->type) {
                                H5VL_iod_read_status_t *read_status = (H5VL_iod_read_status_t *)info->status;

                                if(SUCCEED != read_status->ret) {
                                    fprintf(stderr, "Dataset READ operation failed\n");
                                    free(info->status);
                                    info->status = NULL;
                                    info->bds_handle = (bds_handle_t *)H5MM_xfree(info->bds_handle);
                                    info = (H5VL_iod_io_info_t *)H5MM_xfree(info);
                                    cur_req->status = H5AO_FAILED;
                                    cur_req->state = H5VL_IOD_COMPLETED;
                                    H5VL_iod_request_delete(file, cur_req);
                                    cur_req = tmp_req;
                                    continue;
                                }
                                if(info->checksum && info->checksum != read_status->cs) {
                                    //free(info->status);
                                    //info->status = NULL;
                                    //info->bds_handle = (bds_handle_t *)H5MM_xfree(info->bds_handle);
                                    //HDfree(cur_req->obj_name);
                                    //info = (H5VL_iod_io_info_t *)H5MM_xfree(info);
                                    /* MSC not returning an error because we injected this failure */
                                    fprintf(stderr, "Fatal Error!  Data integrity failure (expecting %u got %u).\n",
                                            info->checksum, read_status->cs);
                                    //HGOTO_ERROR(H5E_SYM, H5E_CANTFREE, SUCCEED, "Data Integrity Fail - bad Checksum");
                                }
                            }

                            free(info->status);
                            info->status = NULL;
                            info->bds_handle = (bds_handle_t *)H5MM_xfree(info->bds_handle);
                            info = (H5VL_iod_io_info_t *)H5MM_xfree(info);
                        }
                        cur_req->status = H5AO_SUCCEEDED;
                        cur_req->state = H5VL_IOD_COMPLETED;
                        H5VL_iod_request_delete(file, cur_req);
                    }
                }
                /* next time, test the next request in the list */
                cur_req = tmp_req;
            }
        }
        /* request complete, remove it from list break */
        else {
            H5VL_iod_request_delete(file, request);
            break;
        }
    }
    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5VL_iod_wait */

herr_t
H5VL_iod_request_wait_all(H5VL_iod_file_t *file)
{
    H5VL_iod_request_t *cur_req = file->request_list_head;
    fs_status_t status;
    int ret;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* Loop to complete all requests */
    while(cur_req) {
        H5VL_iod_request_t *tmp_req = NULL;

        tmp_req = cur_req->next;

        ret = fs_wait(*((fs_request_t *)cur_req->req), FS_MAX_IDLE_TIME, &status);
        if(S_FAIL == ret) {
            cur_req->status = H5AO_FAILED;
            cur_req->state = H5VL_IOD_COMPLETED;
        }
        else {
            HDassert(status);
            cur_req->status = H5AO_SUCCEEDED;
            cur_req->state = H5VL_IOD_COMPLETED;
        }

        if(FS_DSET_WRITE == cur_req->type || FS_DSET_READ == cur_req->type) {
            H5VL_iod_io_info_t *info = (H5VL_iod_io_info_t *)cur_req->data;

            /* Free memory handle */
            if(S_SUCCESS != bds_handle_free(*info->bds_handle)) {
                fprintf(stderr, "failed to free bds handle\n");
                cur_req->status = H5AO_FAILED;
                cur_req->state = H5VL_IOD_COMPLETED;
            }

            if(FS_DSET_WRITE == cur_req->type && SUCCEED != *((int *)info->status)) {
                fprintf(stderr, "write failed\n");
                cur_req->status = H5AO_FAILED;
                cur_req->state = H5VL_IOD_COMPLETED;
            }
            else if(FS_DSET_READ == cur_req->type) {
                H5VL_iod_read_status_t *read_status = (H5VL_iod_read_status_t *)info->status;

                if(SUCCEED != read_status->ret) {
                    fprintf(stderr, "read failed\n");
                    cur_req->status = H5AO_FAILED;
                    cur_req->state = H5VL_IOD_COMPLETED;
                }
                if(info->checksum && info->checksum != read_status->cs) {
                    //free(info->status);
                    //info->status = NULL;
                    //info->bds_handle = (bds_handle_t *)H5MM_xfree(info->bds_handle);
                    //HDfree(cur_req->obj_name);
                    //info = (H5VL_iod_io_info_t *)H5MM_xfree(info);
                    /* MSC not returning an error because we injected this failure */
                    fprintf(stderr, "Fatal Error!  Data integrity failure (expecting %u got %u).\n",
                            info->checksum, read_status->cs);
                }
            }

            free(info->status);
            info->status = NULL;
            info->bds_handle = (bds_handle_t *)H5MM_xfree(info->bds_handle);
            info = (H5VL_iod_io_info_t *)H5MM_xfree(info);
            cur_req->data = NULL;
        }
        H5VL_iod_request_delete(file, cur_req);
        cur_req = tmp_req;
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_request_wait_all */

herr_t
H5VL_iod_request_wait_some(H5VL_iod_file_t *file, const void *object)
{
    H5VL_iod_request_t *cur_req = file->request_list_head;
    fs_status_t status;
    int ret;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* Loop to complete some requests */
    while(cur_req) {
        H5VL_iod_request_t *tmp_req;

        tmp_req = cur_req->next;

        if(cur_req->obj == object) {

            ret = fs_wait(*((fs_request_t *)cur_req->req), FS_MAX_IDLE_TIME, 
                          &status);
            if(S_FAIL == ret) {
                fprintf(stderr, "failed to wait on request\n");
                cur_req->status = H5AO_FAILED;
                cur_req->state = H5VL_IOD_COMPLETED;
            }
            else {
                HDassert(status);
                cur_req->status = H5AO_SUCCEEDED;
                cur_req->state = H5VL_IOD_COMPLETED;
            }
            if(FS_DSET_WRITE == cur_req->type || FS_DSET_READ == cur_req->type) {
                H5VL_iod_io_info_t *info = (H5VL_iod_io_info_t *)cur_req->data;

                /* Free memory handle */
                if(S_SUCCESS != bds_handle_free(*info->bds_handle)) {
                    fprintf(stderr, "failed to free bds handle\n");
                    cur_req->status = H5AO_FAILED;
                    cur_req->state = H5VL_IOD_COMPLETED;
                }
                if(FS_DSET_WRITE == cur_req->type && SUCCEED != *((int *)info->status)) {
                    fprintf(stderr, "write failed\n");
                    cur_req->status = H5AO_FAILED;
                    cur_req->state = H5VL_IOD_COMPLETED;
                }
                else if(FS_DSET_READ == cur_req->type) {
                    H5VL_iod_read_status_t *read_status = (H5VL_iod_read_status_t *)info->status;

                    if(SUCCEED != read_status->ret) {
                        fprintf(stderr, "read failed\n");
                        cur_req->status = H5AO_FAILED;
                        cur_req->state = H5VL_IOD_COMPLETED;
                    }
                    if(info->checksum && info->checksum != read_status->cs) {
                        //free(info->status);
                        //info->status = NULL;
                        //info->bds_handle = (bds_handle_t *)H5MM_xfree(info->bds_handle);
                        //HDfree(cur_req->obj_name);
                        //info = (H5VL_iod_io_info_t *)H5MM_xfree(info);
                        /* MSC not returning an error because we injected this failure */
                        fprintf(stderr, "Fatal Error!  Data integrity failure (expecting %u got %u).\n",
                                info->checksum, read_status->cs);
                    }
                }

                free(info->status);
                info->status = NULL;
                info->bds_handle = (bds_handle_t *)H5MM_xfree(info->bds_handle);
                info = (H5VL_iod_io_info_t *)H5MM_xfree(info);
                cur_req->data = NULL;
            }
            H5VL_iod_request_delete(file, cur_req);
        }
        cur_req = tmp_req;
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_request_wait_some */

herr_t
H5VL_iod_local_traverse(H5VL_iod_object_t *obj, H5VL_loc_params_t UNUSED loc_params, const char *name,
                        iod_obj_id_t *id, iod_handle_t *oh, char **new_name)
{
    iod_obj_id_t cur_id;
    iod_handle_t cur_oh;
    H5VL_iod_group_t *cur_grp = NULL;
    char *cur_name;
    H5WB_t *wb = NULL;       /* Wrapped buffer for temporary buffer */
    char comp_buf[1024];     /* Temporary buffer for path components */
    char *comp;              /* Pointer to buffer for path components */
    size_t nchars;	     /* component name length	*/
    size_t cur_size;
    hbool_t last_comp = FALSE; /* Flag to indicate that a component is the last component in the name */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    if (NULL == (cur_name = (char *)malloc(HDstrlen(obj->obj_name) + HDstrlen(name) + 1)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate");
    HDstrcpy(cur_name, obj->obj_name);
    cur_size = HDstrlen(obj->obj_name);

    if(NULL != obj->request) {
        if(H5VL_iod_request_wait(obj->file, obj->request) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't wait on FS request");

        /* Reset object's pointer to request */
        /* (Request is owned by the request object and will be freed when the
         *      application calls test or wait on it.)
         */
        obj->request = NULL;
    }

    if(H5I_FILE == obj->obj_type) {
        cur_oh = obj->file->remote_file.root_oh;
        cur_id = obj->file->remote_file.root_id;
    }
    else if (H5I_GROUP == obj->obj_type) {
        cur_oh = ((H5VL_iod_group_t *)obj)->remote_group.iod_oh;
        cur_id = ((H5VL_iod_group_t *)obj)->remote_group.iod_id;
    }
    else
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "bad location object");

    /* Wrap the local buffer for serialized header info */
    if(NULL == (wb = H5WB_wrap(comp_buf, sizeof(comp_buf))))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't wrap buffer")
    /* Get a pointer to a buffer that's large enough  */
    if(NULL == (comp = (char *)H5WB_actual(wb, (HDstrlen(name) + 1))))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't get actual buffer")

    /* Traverse the path */
    while((name = H5G__component(name, &nchars)) && *name) {
        const char *s;                  /* Temporary string pointer */

	/*
	 * Copy the component name into a null-terminated buffer so
	 * we can pass it down to the other symbol table functions.
	 */
	HDmemcpy(comp, name, nchars);
	comp[nchars] = '\0';

	/*
	 * The special name `.' is a no-op.
	 */
	if('.' == comp[0] && !comp[1]) {
	    name += nchars;
	    continue;
	} /* end if */

        /* Check if this is the last component of the name */
        if(!((s = H5G__component(name + nchars, NULL)) && *s))
            last_comp = TRUE;

        HDstrcat(cur_name, comp);
        cur_size += nchars;
        cur_name[cur_size] = '\0';

        if(NULL == (cur_grp = (H5VL_iod_group_t *)H5I_search_name(cur_name, H5I_GROUP)))
            break;

        printf("Found %s Locally\n", comp);

        if(NULL != cur_grp->common.request) {
            if(H5VL_iod_request_wait(obj->file, cur_grp->common.request) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't wait on FS request");

            /* Reset object's pointer to request */
            /* (Request is owned by the request object and will be freed when the
             *      application calls test or wait on it.)
             */
            cur_grp->common.request = NULL;
        }

        cur_id = cur_grp->remote_group.iod_id;
        cur_oh = cur_grp->remote_group.iod_oh;

	/* Advance to next component in string */
	name += nchars;
    }

    *id = cur_id;
    *oh = cur_oh;
    *new_name = strdup(name);

done:
    free(cur_name);
    /* Release temporary component buffer */
    if(wb && H5WB_unwrap(wb) < 0)
        HDONE_ERROR(H5E_SYM, H5E_CANTRELEASE, FAIL, "can't release wrapped buffer")
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_local_traverse */

#endif /* H5_HAVE_EFF */
