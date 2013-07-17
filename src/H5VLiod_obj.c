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

#include "H5VLiod_server.h"

#ifdef H5_HAVE_EFF

/*
 * Programmer:  Mohamad Chaarawi <chaarawi@hdfgroup.gov>
 *              June, 2013
 *
 * Purpose:	The IOD plugin server side general object routines.
 */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_object_open_cb
 *
 * Purpose:	Opens an existing object in the container
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              May, 2013
 *
 *-------------------------------------------------------------------------
 */
void
H5VL_iod_server_object_open_cb(AXE_engine_t UNUSED axe_engine, 
                               size_t UNUSED num_n_parents, AXE_task_t UNUSED n_parents[], 
                               size_t UNUSED num_s_parents, AXE_task_t UNUSED s_parents[], 
                               void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    object_op_in_t *input = (object_op_in_t *)op_data->input;
    object_open_out_t output;
    iod_handle_t coh = input->coh; /* the container handle */
    iod_handle_t obj_oh; /* The handle for object */
    iod_obj_id_t obj_id; /* The ID of the object */
    iod_handle_t cur_oh, mdkv_oh;
    iod_obj_id_t cur_id;
    char *last_comp = NULL;
    iod_kv_t kv;
    scratch_pad_t sp;
    iod_size_t kv_size = sizeof(iod_obj_id_t);
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Start Object Open\n");
#endif

    /* the traversal will retrieve the location where the object needs
       to be opened to be created from. The traversal will fail if an
       intermediate group does not exist. */
    if(H5VL_iod_server_traverse(coh, input->loc_id, input->loc_oh, input->loc_name, FALSE, 
                                last_comp, &cur_id, &cur_oh) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't traverse path");

    /* lookup object in the current location */
    if(iod_kv_get_value(cur_oh, IOD_TID_UNKNOWN, last_comp, &obj_id, &kv_size, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Intermdiate group does not exist");

    /* close parent group if it is not the location we started the
       traversal into */
    if(input->loc_oh.cookie != cur_oh.cookie) {
        iod_obj_close(cur_oh, NULL, NULL);
    }

    /* open the object */
    if (iod_obj_open_write(coh, obj_id, NULL, &obj_oh, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't open current group");

    /* get scratch pad of the object */
    if(iod_obj_get_scratch(cur_oh, IOD_TID_UNKNOWN, &sp, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "can't get scratch pad for object");

    /* open the metadata scratch pad */
    if (iod_obj_open_write(coh, sp.mdkv_id, NULL /*hints*/, &mdkv_oh, NULL) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "can't open scratch pad");

    /* MSC - NEED IOD */
#if 0
    if(H5VL_iod_get_metadata(mdkv_oh, IOD_TID_UNKNOWN, H5VL_IOD_OBJECT_TYPE, "object_type",
                             NULL, NULL, NULL, &output.obj_type) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "failed to retrieve link count");

    if(H5VL_iod_get_metadata(mdkv_oh, IOD_TID_UNKNOWN, H5VL_IOD_LINK_COUNT, "link_count",
                             NULL, NULL, NULL, &output.link_count) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "failed to retrieve link count");

    switch(output.obj_type) {
    case H5I_MAP:
        break;
    case H5I_GROUP:
        if(H5VL_iod_get_metadata(mdkv_oh, IOD_TID_UNKNOWN, H5VL_IOD_PLIST, "create_plist",
                                 NULL, NULL, NULL, &output.cpl_id) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "failed to retrieve dcpl");
        break;
    case H5I_DATASET:
        if(H5VL_iod_get_metadata(mdkv_oh, IOD_TID_UNKNOWN, H5VL_IOD_PLIST, "create_plist",
                                 NULL, NULL, NULL, &output.cpl_id) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "failed to retrieve dcpl");

        if(H5VL_iod_get_metadata(mdkv_oh, IOD_TID_UNKNOWN, H5VL_IOD_DATATYPE, "datatype",
                                 NULL, NULL, NULL, &output.type_id) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "failed to retrieve datatype");

        if(H5VL_iod_get_metadata(mdkv_oh, IOD_TID_UNKNOWN, H5VL_IOD_DATASPACE, "dataspace",
                                 NULL, NULL, NULL, &output.space_id) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "failed to retrieve dataspace");
        break;
    case H5I_DATATYPE:
        {
            size_t buf_size; /* size of serialized datatype */
            void *buf = NULL;
            iod_mem_desc_t mem_desc; /* memory descriptor used for reading */
            iod_blob_iodesc_t file_desc; /* file descriptor used to write */

            /* retrieve blob size metadata from scratch pad */
            if(iod_kv_get_value(mdkv_oh, IOD_TID_UNKNOWN, "size", &buf_size, 
                                sizeof(iod_size_t), NULL, NULL) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "datatype size lookup failed");

            if(NULL == (buf = malloc(buf_size)))
                HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate BLOB read buffer");

            /* create memory descriptor for reading */
            mem_desc.nfrag = 1;
            mem_desc.frag->addr = buf;
            mem_desc.frag->len = (iod_size_t)buf_size;

            /* create file descriptor for writing */
            file_desc.nfrag = 1;
            file_desc.frag->offset = 0;
            file_desc.frag->len = (iod_size_t)buf_size;

            /* read the serialized type value from the BLOB object */
            if(iod_blob_read(cur_oh, IOD_TID_UNKNOWN, NULL, &mem_desc, &file_desc, NULL, NULL) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to write BLOB object");

            /* decode the datatype */
            if((output.type_id = H5Tdecode(buf)) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to decode datatype");

            free(buf);
        }
    default:
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Invalid object type");
    }

#endif

    /* close the metadata scratch pad */
    if(iod_obj_close(mdkv_oh, NULL, NULL))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close object");


#if H5_DO_NATIVE
    obj_oh.cookie = H5Oopen(input->loc_oh.cookie, input->loc_name, H5P_DEFAULT);
    output.obj_type = H5Iget_type(obj_oh.cookie);
    switch(output.obj_type){
    case H5I_GROUP:
        output.cpl_id = H5P_GROUP_CREATE_DEFAULT;
        output.type_id = 0;
        output.space_id = 0;
        break;
    case H5I_DATASET:
        output.cpl_id = H5P_DATASET_CREATE_DEFAULT;
        output.type_id = H5Dget_type(obj_oh.cookie);
        output.space_id = H5Dget_space(obj_oh.cookie);
        break;
    case H5I_DATATYPE:
        output.cpl_id = H5P_DATATYPE_CREATE_DEFAULT;
        output.type_id = obj_oh.cookie;
        output.space_id = 0;
        break;
    default:
        HGOTO_ERROR(H5E_ARGS, H5E_CANTINIT, FAIL, "not a valid object (dataset, group, or datatype)")
    }
#else
    /* Fake something */
    output.obj_type = H5I_GROUP;
    output.cpl_id = H5P_GROUP_CREATE_DEFAULT;
    output.type_id = 0;
    output.space_id = 0;
#endif

    output.iod_id = obj_id;
    output.iod_oh = obj_oh;

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Done with object open, sending response to client\n");
#endif

    HG_Handler_start_output(op_data->hg_handle, &output);

done:
    if(ret_value < 0) {
        output.iod_oh.cookie = IOD_OH_UNDEFINED;
        output.iod_id = IOD_ID_UNDEFINED;
        output.cpl_id = H5P_GROUP_CREATE_DEFAULT;
        HG_Handler_start_output(op_data->hg_handle, &output);
    }

    switch(output.obj_type){
    case H5I_GROUP:
        break;
    case H5I_DATASET:
#if H5_DO_NATIVE
        H5Tclose(output.type_id);
        H5Sclose(output.space_id);
#endif
        break;
    case H5I_DATATYPE:
        break;
    default:
        HGOTO_ERROR(H5E_ARGS, H5E_CANTINIT, FAIL, "not a valid object (dataset, group, or datatype)")
    }

    last_comp = (char *)H5MM_xfree(last_comp);
    input = (object_op_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

    FUNC_LEAVE_NOAPI_VOID
} /* end H5VL_iod_server_object_open_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_object_copy_cb
 *
 * Purpose:	Moves/Copies a link in the container.
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              May, 2013
 *
 *-------------------------------------------------------------------------
 */
void
H5VL_iod_server_object_copy_cb(AXE_engine_t UNUSED axe_engine, 
                               size_t UNUSED num_n_parents, AXE_task_t UNUSED n_parents[], 
                               size_t UNUSED num_s_parents, AXE_task_t UNUSED s_parents[], 
                               void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    object_copy_in_t *input = (object_copy_in_t *)op_data->input;
    iod_handle_t coh = input->coh; /* the container handle */
    iod_handle_t src_oh; /* The handle for src parent */
    iod_obj_id_t src_id; /* The ID of the src parent */
    iod_handle_t dst_oh; /* The handle for the dst object where link is created*/
    iod_obj_id_t dst_id; /* The ID of the dst object where link is created*/
    iod_obj_id_t obj_id; /* The ID of the object to be moved/copied */
    iod_obj_id_t new_id; /* The ID of the new object */
    iod_handle_t mdkv_oh;/* The handle of the metadata KV for source object */
    char *last_comp = NULL, *new_name;
    iod_kv_t kv;
    iod_size_t kv_size = sizeof(iod_obj_id_t);
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Start object copy\n");
#endif

    /* the traversal will retrieve the location where the object
       exists. The traversal will fail if an intermediate group does
       not exist. */
    if(H5VL_iod_server_traverse(coh, input->src_loc_id, input->src_loc_oh, input->src_loc_name, 
                                FALSE, &last_comp, &src_id, &src_oh) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't traverse path");

    /* the traversal will retrieve the location where the objects
       needs to be copied to. The traversal will fail if an
       intermediate group does not exist. */
    if(H5VL_iod_server_traverse(coh, input->dst_loc_id, input->dst_loc_oh, input->dst_loc_name, 
                                FALSE, &new_name, &dst_id, &dst_oh) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't traverse path");

    /* lookup object ID in the current src location */
    if(iod_kv_get_value(src_oh, IOD_TID_UNKNOWN, last_comp, &obj_id, &kv_size, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Object does not exist in source path");

    /* MSC - NEED IOD & a lot more work*/
#if 0
    /* open the object */
    if (iod_obj_open_write(coh, obj_id, NULL, &obj_oh, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't open current group");

    /* get scratch pad of the object */
    if(iod_obj_get_scratch(cur_oh, IOD_TID_UNKNOWN, &sp, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "can't get scratch pad for object");

    /* open the metadata scratch pad */
    if (iod_obj_open_write(coh, sp.mdkv_id, NULL /*hints*/, &mdkv_oh, NULL) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "can't open scratch pad");

    if(H5VL_iod_get_metadata(mdkv_oh, IOD_TID_UNKNOWN, H5VL_IOD_OBJECT_TYPE, "object_type",
                             NULL, NULL, NULL, &obj_type) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "failed to retrieve link count");

    switch(obj_type) {
    case H5I_MAP:
        break;
    case H5I_GROUP:
        if(H5VL_iod_get_metadata(mdkv_oh, IOD_TID_UNKNOWN, H5VL_IOD_PLIST, "create_plist",
                                 NULL, NULL, NULL, &output.cpl_id) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "failed to retrieve dcpl");
        break;
    case H5I_DATASET:
        if(H5VL_iod_get_metadata(mdkv_oh, IOD_TID_UNKNOWN, H5VL_IOD_PLIST, "create_plist",
                                 NULL, NULL, NULL, &output.cpl_id) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "failed to retrieve dcpl");

        if(H5VL_iod_get_metadata(mdkv_oh, IOD_TID_UNKNOWN, H5VL_IOD_DATATYPE, "datatype",
                                 NULL, NULL, NULL, &output.type_id) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "failed to retrieve datatype");

        if(H5VL_iod_get_metadata(mdkv_oh, IOD_TID_UNKNOWN, H5VL_IOD_DATASPACE, "dataspace",
                                 NULL, NULL, NULL, &output.space_id) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "failed to retrieve dataspace");
        break;
    case H5I_DATATYPE:
        {
            size_t buf_size; /* size of serialized datatype */
            void *buf = NULL;
            iod_mem_desc_t mem_desc; /* memory descriptor used for reading */
            iod_blob_iodesc_t file_desc; /* file descriptor used to write */

            /* retrieve blob size metadata from scratch pad */
            if(iod_kv_get_value(mdkv_oh, IOD_TID_UNKNOWN, "size", &buf_size, 
                                sizeof(iod_size_t), NULL, NULL) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "datatype size lookup failed");

            if(NULL == (buf = malloc(buf_size)))
                HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate BLOB read buffer");

            /* create memory descriptor for reading */
            mem_desc.nfrag = 1;
            mem_desc.frag->addr = buf;
            mem_desc.frag->len = (iod_size_t)buf_size;

            /* create file descriptor for writing */
            file_desc.nfrag = 1;
            file_desc.frag->offset = 0;
            file_desc.frag->len = (iod_size_t)buf_size;

            /* read the serialized type value from the BLOB object */
            if(iod_blob_read(cur_oh, IOD_TID_UNKNOWN, NULL, &mem_desc, &file_desc, NULL, NULL) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to write BLOB object");

            /* decode the datatype */
            if((output.type_id = H5Tdecode(buf)) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to decode datatype");

            free(buf);
        }
    default:
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Invalid object type");
    }

    /* create new object as a copy of the source object */
    /* MSC - wait to see if IOD will have an object copy */

    /* close the metadata scratch pad */
    if(iod_obj_close(mdkv_oh, NULL, NULL))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close object");
    /* close the object handle */
    if(iod_obj_close(obj_oh, NULL, NULL))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close object");

    /* Insert object in the destination path */
    if(H5VL_iod_insert_new_link(dst_oh, IOD_TID_UNKNOWN, new_name, obj_id, 
                                NULL, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't insert KV value");
#endif

    /* close source group if it is not the location we started the
       traversal into */
    if(input->src_loc_oh.cookie != src_oh.cookie) {
        iod_obj_close(src_oh, NULL, NULL);
    }

    /* close dst group if it is not the location we started the
       traversal into */
    if(input->dst_loc_oh.cookie != dst_oh.cookie) {
        iod_obj_close(dst_oh, NULL, NULL);
    }

#if H5_DO_NATIVE
    if(H5Ocopy(input->src_loc_oh.cookie, input->src_loc_name, 
               input->dst_loc_oh.cookie, input->dst_loc_name,
               H5P_DEFAULT, H5P_DEFAULT) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't copy object");
#endif

done:
#if H5VL_IOD_DEBUG
    fprintf(stderr, "Done with object Copy, sending response to client\n");
#endif

    HG_Handler_start_output(op_data->hg_handle, &ret_value);

    last_comp = (char *)H5MM_xfree(last_comp);
    if(new_name)
        free(new_name);
    input = (object_copy_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

    FUNC_LEAVE_NOAPI_VOID
} /* end H5VL_iod_server_object_copy_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_object_exists_cb
 *
 * Purpose:	Checks if an object exists.
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              May, 2013
 *
 *-------------------------------------------------------------------------
 */
void
H5VL_iod_server_object_exists_cb(AXE_engine_t UNUSED axe_engine, 
                                 size_t UNUSED num_n_parents, AXE_task_t UNUSED n_parents[], 
                                 size_t UNUSED num_s_parents, AXE_task_t UNUSED s_parents[], 
                                 void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    object_op_in_t *input = (object_op_in_t *)op_data->input;
    iod_handle_t coh = input->coh;
    iod_handle_t loc_oh = input->loc_oh;
    iod_obj_id_t loc_id = input->loc_id;
    iod_handle_t cur_oh;
    iod_obj_id_t cur_id;
    const char *loc_name = input->loc_name;
    char *last_comp = NULL;
    htri_t ret = -1;
    iod_size_t kv_size = sizeof(iod_obj_id_t);
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* the traversal will retrieve the location where the object needs
       to be checked */
    if(H5VL_iod_server_traverse(coh, loc_id, loc_oh, loc_name, FALSE, 
                                &last_comp, &cur_id, &cur_oh) < 0) {
        ret = FALSE;
        HGOTO_DONE(SUCCEED);
    }

    /* check the last component */
    if(iod_kv_get_value(cur_oh, IOD_TID_UNKNOWN, last_comp, 
                        &cur_id, &kv_size, NULL, NULL) < 0) {
        ret = FALSE;
    } /* end if */
    else {
        iod_handle_t obj_oh;
        /* try to open the object */
        if (iod_obj_open_write(coh, cur_id, NULL, &obj_oh, NULL) < 0) {
            ret = FALSE;
            HGOTO_DONE(SUCCEED);
        }
        else {
            /* close the object */
            iod_obj_close(obj_oh, NULL, NULL);
            ret = TRUE;
        }
    }

#if H5_DO_NATIVE
    ret = H5Oexists_by_name(loc_oh.cookie, loc_name, H5P_DEFAULT);
#else
    ret = FALSE;
#endif

done:

    /* close parent group if it is not the location we started the
       traversal into */
    if(loc_oh.cookie != IOD_OH_UNDEFINED && input->loc_oh.cookie != loc_oh.cookie) {
        iod_obj_close(loc_oh, NULL, NULL);
    }

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Done with Object exists, sending response to client\n");
#endif

    HG_Handler_start_output(op_data->hg_handle, &ret);

    input = (object_op_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);
    last_comp = (char *)H5MM_xfree(last_comp);

    FUNC_LEAVE_NOAPI_VOID
} /* end H5VL_iod_server_object_exists_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_object_set_comment_cb
 *
 * Purpose:	Set comment for an object.
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              May, 2013
 *
 *-------------------------------------------------------------------------
 */
void
H5VL_iod_server_object_set_comment_cb(AXE_engine_t UNUSED axe_engine, 
                                      size_t UNUSED num_n_parents, AXE_task_t UNUSED n_parents[], 
                                      size_t UNUSED num_s_parents, AXE_task_t UNUSED s_parents[], 
                                      void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    object_set_comment_in_t *input = (object_set_comment_in_t *)op_data->input;
    iod_handle_t coh = input->coh;
    iod_handle_t loc_oh = input->loc_oh;
    iod_obj_id_t loc_id = input->loc_id;
    iod_handle_t cur_oh, mdkv_oh;
    iod_obj_id_t cur_id, obj_id;
    const char *loc_name = input->path;
    const char *comment = input->comment;
    char *last_comp = NULL;
    scratch_pad_t sp;
    iod_size_t kv_size = sizeof(iod_obj_id_t);
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* the traversal will retrieve the location where the link needs
       to be removed. The traversal will fail if an intermediate group
       does not exist. */
    if(H5VL_iod_server_traverse(coh, loc_id, loc_oh, loc_name, 
                                FALSE, &last_comp, &cur_id, &cur_oh) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't traverse path");

    /* lookup object ID in the current location */
    if(iod_kv_get_value(cur_oh, IOD_TID_UNKNOWN, last_comp, &obj_id, &kv_size, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Object does not exist in source path");

    /* close parent group and its scratch pad if it is not the
       location we started the traversal into */
    if(loc_oh.cookie != cur_oh.cookie) {
        iod_obj_close(cur_oh, NULL, NULL);
    }

    /* open the object */
    if (iod_obj_open_write(coh, obj_id, NULL /*hints*/, &cur_oh, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't open object");

    /* get scratch pad of the object */
    if(iod_obj_get_scratch(cur_oh, IOD_TID_UNKNOWN, &sp, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "can't get scratch pad for object");

    /* open the metadata scratch pad */
    if (iod_obj_open_write(coh, sp.mdkv_id, NULL /*hints*/, &mdkv_oh, NULL) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "can't open scratch pad");

    {
        iod_kv_t kv;
        char *key = NULL;
        char *value = NULL;

        key = strdup("comment");
        kv.key = key;
        kv.value_len = strlen(comment) + 1;
        value = strdup(comment);
        kv.value = value;

        if (iod_kv_set(mdkv_oh, IOD_TID_UNKNOWN, NULL, &kv, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't set KV pair in parent");

        free(key);
        free(value);
    }

    /* close metadata KV and object */
    if(iod_obj_close(mdkv_oh, NULL, NULL))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close object");
    if(iod_obj_close(cur_oh, NULL, NULL))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close object");

#if H5_DO_NATIVE
    if(H5Oset_comment(loc_oh.cookie, comment) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Unable to set object comment");
#endif

done:
#if H5VL_IOD_DEBUG
    fprintf(stderr, "Done with set comment, sending response to client\n");
#endif

    HG_Handler_start_output(op_data->hg_handle, &ret_value);

    last_comp = (char *)H5MM_xfree(last_comp);
    input = (object_set_comment_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

    FUNC_LEAVE_NOAPI_VOID
} /* end H5VL_iod_server_object_set_comment_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_object_get_comment_cb
 *
 * Purpose:	Get comment for an object.
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              May, 2013
 *
 *-------------------------------------------------------------------------
 */
void
H5VL_iod_server_object_get_comment_cb(AXE_engine_t UNUSED axe_engine, 
                                      size_t UNUSED num_n_parents, AXE_task_t UNUSED n_parents[], 
                                      size_t UNUSED num_s_parents, AXE_task_t UNUSED s_parents[], 
                                      void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    object_get_comment_in_t *input = (object_get_comment_in_t *)op_data->input;
    object_get_comment_out_t output;
    name_t comment;
    iod_handle_t coh = input->coh;
    iod_handle_t loc_oh = input->loc_oh;
    iod_obj_id_t loc_id = input->loc_id;
    size_t length = input->length;
    iod_handle_t cur_oh, mdkv_oh;
    iod_obj_id_t cur_id, obj_id;
    const char *loc_name = input->path;
    char *last_comp = NULL;
    iod_kv_params_t kvs;
    iod_kv_t kv;
    iod_size_t kv_size = sizeof(iod_obj_id_t);
    scratch_pad_t sp;
    ssize_t size = 0;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* the traversal will retrieve the location where the link needs
       to be removed. The traversal will fail if an intermediate group
       does not exist. */
    if(H5VL_iod_server_traverse(coh, loc_id, loc_oh, loc_name, 
                                FALSE, &last_comp, &cur_id, &cur_oh) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't traverse path");

    /* lookup object ID in the current location */
    if(iod_kv_get_value(cur_oh, IOD_TID_UNKNOWN, last_comp, &obj_id, &kv_size, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Object does not exist in source path");

    /* close parent group and its scratch pad if it is not the
       location we started the traversal into */
    if(loc_oh.cookie != cur_oh.cookie) {
        iod_obj_close(cur_oh, NULL, NULL);
    }

    /* open the object */
    if (iod_obj_open_write(coh, obj_id, NULL /*hints*/, &cur_oh, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't open object");

    /* get scratch pad of the object */
    if(iod_obj_get_scratch(cur_oh, IOD_TID_UNKNOWN, &sp, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "can't get scratch pad for object");

    /* open the metadata scratch pad */
    if (iod_obj_open_write(coh, sp.mdkv_id, NULL /*hints*/, &mdkv_oh, NULL) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "can't open scratch pad");

    comment.value_size = (ssize_t *)malloc(sizeof(ssize_t));
    comment.value = NULL;
    comment.size = length;

    /* MSC - NEED IOD */
#if 0
    if(iod_kv_get_value(oh, IOD_TID_UNKNOWN, "comment", NULL, 
                        &size, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "lookup failed");

    if(NULL == (value = malloc (size)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate value buffer");

    if(iod_kv_get_value(oh, IOD_TID_UNKNOWN, "comment", value, 
                        &size, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "lookup failed");

    if(length)
        memcpy(comment.value, value, length);

    free(value);
#endif

    /* close metadata KV and object */
    if(iod_obj_close(mdkv_oh, NULL, NULL))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close object");
    if(iod_obj_close(cur_oh, NULL, NULL))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close object");


#if H5_DO_NATIVE
    if(0 != length) {
        size = H5Oget_comment(loc_oh.cookie, NULL, length);
        comment.value = malloc(size+1);
    }
    if((size = H5Oget_comment(loc_oh.cookie, comment.value, length)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Unable to get object comment");
#else
    if(length) {
        comment.value = strdup("fake comment");
        size = strlen(comment.value) + 1;
    }
    else
        size = 22;
#endif

    *comment.value_size = size;

done:
    output.ret = ret_value;
    output.name = comment;

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Done with get comment, sending response to client\n");
#endif

    HG_Handler_start_output(op_data->hg_handle, &output);

    if(comment.value)
        free(comment.value);
    free(comment.value_size);

    last_comp = (char *)H5MM_xfree(last_comp);
    input = (object_get_comment_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

    FUNC_LEAVE_NOAPI_VOID
} /* end H5VL_iod_server_object_get_comment_cb() */

#endif /* H5_HAVE_EFF */
