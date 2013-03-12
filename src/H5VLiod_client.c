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
    }

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

    H5MM_free(request->req);
    H5MM_free(request);

    FUNC_LEAVE_NOAPI(SUCCEED)
}

herr_t
H5VL_iod_request_wait(H5VL_iod_request_t *request)
{
    H5VL_iod_object_t *obj = (H5VL_iod_object_t *)request->data;
    H5VL_iod_file_t *file = obj->file;
    H5VL_iod_request_t *cur_req = file->request_list_head;
    fs_status_t status;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Loop to complete the request while poking through other requests on the 
       container to avoid deadlock. */
    while(1) {
        /* test the operation status */
        fs_wait(*((fs_request_t *)request->req), 0, &status);

        /* if it has not completed, go through the list of requests on the container to
           test progress */
        if(!status) {
            H5VL_iod_request_t *tmp_req = cur_req->next;

            if(cur_req) {
                fs_status_t tmp_status;
                fs_wait(*((fs_request_t *)cur_req->req), 0, &tmp_status);
                if(tmp_status) {
                    H5VL_iod_request_delete(file, cur_req);
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
    hbool_t last_comp = FALSE; /* Flag to indicate that a component is the last component in the name */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    if (NULL == (cur_name = (char *)malloc(HDstrlen(obj->obj_name) + HDstrlen(name) + 1)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate");

    if(H5VL_iod_request_wait(obj->request) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't wait on FS request");

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

    HDstrcat(cur_name, obj->obj_name);

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

        if(NULL == (cur_grp = (H5VL_iod_group_t *)H5I_search_name(cur_name, H5I_GROUP)))
            break;

        if(H5VL_iod_request_wait(cur_grp->common.request) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't wait on FS request");
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
    na_network_class_t *network_class = NULL;
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

    /* Look up addr id */
    fs_ret = na_addr_lookup(network_class, mpi_port_name, &ion_target);
    if (fs_ret != S_SUCCESS)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NA_UNDEFINED, "failed to connect to network address");

    /* Register function and encoding/decoding functions */
    H5VL_EFF_INIT_ID = fs_register("eff_init", H5VL_iod_client_encode_eff_init, 
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
    H5VL_DSET_CLOSE_ID = fs_register("dset_close", H5VL_iod_client_encode_dset_close, 
                                      H5VL_iod_client_decode_dset_close);

    /* forward the init call to the IONs */
    if(fs_forward(ion_target, H5VL_EFF_INIT_ID, &num_procs, &ret_value, &ret_value) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NA_UNDEFINED, "failed to ship eff_init");

    fs_wait(fs_req, FS_MAX_IDLE_TIME, FS_STATUS_IGNORE);

done:
    FUNC_LEAVE_NOAPI(ret_value)
}
