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

na_network_class_t *network_class = NULL;

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

    request->prev = NULL;
    request->next = NULL;

    FUNC_LEAVE_NOAPI(SUCCEED)
}

herr_t
H5VL_iod_request_wait(H5VL_iod_file_t *file, H5VL_iod_request_t *request)
{
    H5VL_iod_request_t *cur_req = file->request_list_head;
    fs_status_t status;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(request);
    HDassert(request->req);

    /* Loop to complete the request while poking through other requests on the 
       container to avoid deadlock. */
    while(1) {
        /* test the operation status */
        fs_wait(*((fs_request_t *)request->req), 0, &status);

        /* if it has not completed, go through the list of requests on the container to
           test progress */
        if(!status) {
            H5VL_iod_request_t *tmp_req = NULL;

            if(cur_req) {
                fs_status_t tmp_status;

                tmp_req = cur_req->next;
                fs_wait(*((fs_request_t *)cur_req->req), 0, &tmp_status);
                if(tmp_status) {
                    H5VL_iod_request_delete(file, cur_req);
                    cur_req->req = H5MM_xfree(cur_req->req);
                    cur_req = H5MM_xfree(cur_req);
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
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* Loop to complete all requests */
    while(cur_req) {
        H5VL_iod_request_t *tmp_req = NULL;

        tmp_req = cur_req->next;
        fs_wait(*((fs_request_t *)cur_req->req), FS_MAX_IDLE_TIME, &status);
        if(!status)
            HGOTO_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "a pending request did not complete");

        if(FS_DSET_WRITE == cur_req->type || FS_DSET_READ == cur_req->type) {
            H5VL_iod_io_info_t *info = (H5VL_iod_io_info_t *)cur_req->data;

            /* Free memory handle */
            if(S_SUCCESS != bds_handle_free(*info->bds_handle))
                HGOTO_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "failed to free bds handle");

            if(FS_DSET_WRITE == cur_req->type && SUCCEED != *((int *)info->status))
                HGOTO_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "Dataset I/O failed")
            else if(FS_DSET_READ == cur_req->type) {
                H5VL_iod_read_status_t *status = (H5VL_iod_read_status_t *)info->status;

                if(SUCCEED != status->ret) {
                    free(info->status);
                    info->status = NULL;
                    info->bds_handle = H5MM_xfree(info->bds_handle);
                    HDfree(cur_req->obj_name);
                    info = H5MM_xfree(info);
                    HGOTO_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "Dataset I/O failed");
                }
                if(info->checksum && info->checksum != status->cs) {
                    //free(info->status);
                    //info->status = NULL;
                    //info->bds_handle = H5MM_xfree(info->bds_handle);
                    //HDfree(cur_req->obj_name);
                    //info = H5MM_xfree(info);
                    /* MSC not returning an error because we injected this failure */
                    fprintf(stderr, "Fatal Error!  Data integrity failure (expecting %u got %u).\n",
                            info->checksum, status->cs);
                    //HGOTO_ERROR(H5E_SYM, H5E_CANTFREE, SUCCEED, "Data Integrity Fail - bad Checksum");
                }
            }

            free(info->status);
            info->status = NULL;
            info->bds_handle = H5MM_xfree(info->bds_handle);
            info = H5MM_xfree(info);
            HDfree(cur_req->obj_name);
        }
        H5VL_iod_request_delete(file, cur_req);
        cur_req->req = H5MM_xfree(cur_req->req);
        cur_req = H5MM_xfree(cur_req);
        cur_req = tmp_req;
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_request_wait_all */

herr_t
H5VL_iod_request_wait_some(H5VL_iod_file_t *file, const char *name)
{
    H5VL_iod_request_t *cur_req = file->request_list_head;
    fs_status_t status;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* Loop to complete some requests */
    while(cur_req) {
        H5VL_iod_request_t *tmp_req;

        tmp_req = cur_req->next;

        if(FS_DSET_WRITE == cur_req->type || FS_DSET_READ == cur_req->type) {
            H5VL_iod_io_info_t *info = (H5VL_iod_io_info_t *)cur_req->data;

            if(!HDstrcmp(name, cur_req->obj_name)) {
                fs_wait(*((fs_request_t *)cur_req->req), FS_MAX_IDLE_TIME, &status);
                if(!status)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "a pending request did not complete");

                /* Free memory handle */
                if(S_SUCCESS != bds_handle_free(*info->bds_handle))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "failed to free bds handle");

                if(FS_DSET_WRITE == cur_req->type && SUCCEED != *((int *)info->status))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "Dataset I/O failed")
                else if(FS_DSET_READ == cur_req->type) {
                    H5VL_iod_read_status_t *status = (H5VL_iod_read_status_t *)info->status;

                    if(SUCCEED != status->ret) {
                        free(info->status);
                        info->status = NULL;
                        info->bds_handle = H5MM_xfree(info->bds_handle);
                        HDfree(cur_req->obj_name);
                        info = H5MM_xfree(info);
                        HGOTO_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "Dataset I/O failed");
                    }
                    if(info->checksum && info->checksum != status->cs) {
                        //free(info->status);
                        //info->status = NULL;
                        //info->bds_handle = H5MM_xfree(info->bds_handle);
                        //HDfree(cur_req->obj_name);
                        //info = H5MM_xfree(info);
                        /* MSC not returning an error because we injected this failure */
                        fprintf(stderr, "Fatal Error!  Data integrity failure (expecting %u got %u).\n",
                                info->checksum, status->cs);
                        //HGOTO_ERROR(H5E_SYM, H5E_CANTFREE, SUCCEED, "Data Integrity Fail - bad Checksum");
                    }
                }

                free(info->status);
                info->status = NULL;
                info->bds_handle = H5MM_xfree(info->bds_handle);
                HDfree(cur_req->obj_name);
                info = H5MM_xfree(info);

                H5VL_iod_request_delete(file, cur_req);
                cur_req->req = H5MM_xfree(cur_req->req);
                cur_req = H5MM_xfree(cur_req);
            }
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
        obj->request->req = H5MM_xfree(obj->request->req);
        obj->request = H5MM_xfree(obj->request);
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

            cur_grp->common.request->req = H5MM_xfree(cur_grp->common.request->req);
            cur_grp->common.request = H5MM_xfree(cur_grp->common.request);
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

na_addr_t
H5VL_iod_client_eff_init(const char *mpi_port_name, MPI_Comm comm, MPI_Info UNUSED info)
{
    na_addr_t ion_target;
    int fs_ret;
    na_addr_t ret_value;
    int num_procs;
    fs_request_t fs_req;

    FUNC_ENTER_NOAPI_NOINIT

    MPI_Comm_size(comm, &num_procs);

    network_class = na_mpi_init(NULL, 0);

    fs_ret = fs_init(network_class);
    if (fs_ret != S_SUCCESS)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NA_UNDEFINED, "failed to initialize client function shipper");

    fs_ret = bds_init(network_class);
    if (fs_ret != S_SUCCESS)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NA_UNDEFINED, "failed to initialize client function shipper");

    /* Look up addr id */
    fs_ret = na_addr_lookup(network_class, mpi_port_name, &ion_target);
    if (fs_ret != S_SUCCESS)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NA_UNDEFINED, "failed to connect to network address");

    /* Register function and encoding/decoding functions */
    H5VL_EFF_INIT_ID = fs_register("eff_init", H5VL_iod_client_encode_eff_init, 
                                   H5VL_iod_client_decode_eff_init);
    H5VL_EFF_FINALIZE_ID = fs_register("eff_finalize", NULL, 
                                   H5VL_iod_client_decode_eff_init);
    H5VL_FILE_CREATE_ID = fs_register("file_create", H5VL_iod_client_encode_file_create, 
                                      H5VL_iod_client_decode_file_create);
    H5VL_FILE_OPEN_ID = fs_register("file_open", H5VL_iod_client_encode_file_open, 
                                      H5VL_iod_client_decode_file_open);
    H5VL_FILE_CLOSE_ID = fs_register("file_close", H5VL_iod_client_encode_file_close, 
                                      H5VL_iod_client_decode_file_close);
    H5VL_GROUP_CREATE_ID = fs_register("group_create", H5VL_iod_client_encode_group_create, 
                                      H5VL_iod_client_decode_group_create);
    H5VL_GROUP_OPEN_ID = fs_register("group_open", H5VL_iod_client_encode_group_open, 
                                      H5VL_iod_client_decode_group_open);
    H5VL_GROUP_CLOSE_ID = fs_register("group_close", H5VL_iod_client_encode_group_close, 
                                      H5VL_iod_client_decode_group_close);
    H5VL_DSET_CREATE_ID = fs_register("dset_create", H5VL_iod_client_encode_dset_create, 
                                      H5VL_iod_client_decode_dset_create);
    H5VL_DSET_OPEN_ID = fs_register("dset_open", H5VL_iod_client_encode_dset_open, 
                                      H5VL_iod_client_decode_dset_open);
    H5VL_DSET_READ_ID = fs_register("dset_read", H5VL_iod_client_encode_dset_io, 
                                    H5VL_iod_client_decode_dset_read);
    H5VL_DSET_WRITE_ID = fs_register("dset_write", H5VL_iod_client_encode_dset_io, 
                                     H5VL_iod_client_decode_dset_write);
    H5VL_DSET_CLOSE_ID = fs_register("dset_close", H5VL_iod_client_encode_dset_close, 
                                     H5VL_iod_client_decode_dset_close);

    /* forward the init call to the IONs */
    if(fs_forward(ion_target, H5VL_EFF_INIT_ID, &num_procs, &ret_value, &fs_req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NA_UNDEFINED, "failed to ship eff_init");

    fs_wait(fs_req, FS_MAX_IDLE_TIME, FS_STATUS_IGNORE);

    ret_value = ion_target;
done:
    FUNC_LEAVE_NOAPI(ret_value)
}

herr_t
H5VL_iod_client_eff_finalize(na_addr_t ion_target)
{
    herr_t ret_value = SUCCEED;
    fs_request_t fs_req;
    int fs_ret;

    FUNC_ENTER_NOAPI_NOINIT

    /* forward the finalize call to the IONs */
    if(fs_forward(ion_target, H5VL_EFF_FINALIZE_ID, NULL, &ret_value, &fs_req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "failed to ship eff_finalize");

    fs_wait(fs_req, FS_MAX_IDLE_TIME, FS_STATUS_IGNORE);

    /* Free addr id */
    fs_ret = na_addr_free(network_class, ion_target);
    if (fs_ret != S_SUCCESS)
        HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "failed to shutdown function shipper address");

    /* Finalize interface */
    fs_ret = fs_finalize();
    if (fs_ret != S_SUCCESS) 
        HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "failed to finalize function shipper");

    fs_ret = bds_finalize();
    if (fs_ret != S_SUCCESS) 
        HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "failed to finalize function shipper");

    H5_term_library();
done:
    FUNC_LEAVE_NOAPI(ret_value)
}
