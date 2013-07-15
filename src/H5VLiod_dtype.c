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
 * Purpose:	The IOD plugin server side datatype routines.
 */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_dtype_commit_cb
 *
 * Purpose:	Commits a dtype as a iod object.
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              April, 2013
 *
 *-------------------------------------------------------------------------
 */
void
H5VL_iod_server_dtype_commit_cb(AXE_engine_t UNUSED axe_engine, 
                                size_t UNUSED num_n_parents, AXE_task_t UNUSED n_parents[], 
                                size_t UNUSED num_s_parents, AXE_task_t UNUSED s_parents[], 
                                void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    dtype_commit_in_t *input = (dtype_commit_in_t *)op_data->input;
    dtype_commit_out_t output;
    iod_handle_t coh = input->coh;
    iod_handle_t loc_handle = input->loc_oh;
    iod_obj_id_t loc_id = input->loc_id; /* The ID of the current location object */
    iod_obj_id_t dtype_id = input->dtype_id; /* The ID of the datatype that needs to be created */
    iod_handle_t dtype_oh, cur_oh, mdkv_oh;
    iod_obj_id_t cur_id, mdkv_id, attr_id;
    const char *name = input->name;
    iod_kv_t kv;
    char *last_comp; /* the name of the datatype obtained from the last component in the path */
    size_t buf_size;
    void *buf;
    iod_mem_desc_t mem_desc;
    iod_blob_iodesc_t file_desc;
    scratch_pad_t sp;
    iod_ret_t ret;
    hbool_t collective = FALSE; /* MSC - change when we allow for collective */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Start datatype Commit %s\n", name);
#endif

    /* the traversal will retrieve the location where the datatype needs
       to be created. The traversal will fail if an intermediate group
       does not exist. */
    if(H5VL_iod_server_traverse(coh, loc_id, loc_handle, name, FALSE, 
                                &last_comp, &cur_id, &cur_oh) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't traverse path");

    /* create the datatype */
    ret = iod_obj_create(coh, IOD_TID_UNKNOWN, NULL/*hints*/, IOD_OBJ_BLOB, NULL, NULL,
                         &dtype_id, NULL /*event*/);
    if(collective && (0 == ret || EEXISTS == ret)) {
        /* Datatype has been created by another process, open it */
        if (iod_obj_open_write(coh, dtype_id, NULL, &dtype_oh, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't open Datatype");
    }
    else if(!collective && 0 != ret) {
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't create Datatype");
    }

    /* for the process that succeeded in creating the datatype, update
       the parent KV, create scratch pad */
    if(0 == ret) {
        /* create the metadata KV object for the datatype */
        if(iod_obj_create(coh, IOD_TID_UNKNOWN, NULL, IOD_OBJ_KV, 
                          NULL, NULL, &mdkv_id, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't create metadata KV object");

        /* create the attribute KV object for the datatype */
        if(iod_obj_create(coh, IOD_TID_UNKNOWN, NULL, IOD_OBJ_KV, 
                          NULL, NULL, &attr_id, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't create metadata KV object");

        /* set values for the scratch pad object */
        sp.mdkv_id = mdkv_id;
        sp.attr_id = attr_id;
        sp.filler1_id = IOD_ID_UNDEFINED;
        sp.filler2_id = IOD_ID_UNDEFINED;

        /* set scratch pad in datatype */
        if (iod_obj_set_scratch(dtype_oh, IOD_TID_UNKNOWN, &sp, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't set scratch pad");

        /* Store Metadata in scratch pad */
        if (iod_obj_open_write(coh, mdkv_id, NULL, &mdkv_oh, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't create scratch pad");

        /* determine the buffer size needed to store the encoded type of the datatype */ 
        if(H5Tencode(input->type_id, NULL, &buf_size) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "failed to encode datatype type");
        if(NULL == (buf = malloc (buf_size)))
            HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate type buffer");
        /* encode datatype of the datatype */ 
        if(H5Tencode(input->type_id, buf, &buf_size) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "failed to encode datatype type");

#if 0
        /* create memory descriptor for writing */
        mem_desc.nfrag = 1;
        mem_desc.frag->addr = buf;
        mem_desc.frag->len = (iod_size_t)buf_size;

        /* create file descriptor for writing */
        file_desc.nfrag = 1;
        file_desc.frag->offset = 0;
        file_desc.frag->len = (iod_size_t)buf_size;
#endif

        /* write the serialized type value to the BLOB object */
        if(iod_blob_write(dtype_oh, IOD_TID_UNKNOWN, NULL, &mem_desc, &file_desc, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to write BLOB object");

        /* MSC - TODO store things */
#if 0
        /* insert datatype metadata into scratch pad */

        kv.key = HDstrdup("datatype_tcpl");
        /* determine the buffer size needed to store the encoded tcpl of the datatype */ 
        if(H5Pencode(input->tcpl_id,  NULL, &value_size) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "failed to encode datatype tcpl");
        if(NULL == (kv.value = malloc (value_size)))
            HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate tcpl buffer");
        /* encode tcpl of the datatype */ 
        if(H5Pencode(input->tcpl_id, kv.value, &value_size) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "failed to encode datatype tcpl");
        kv.value_len = (iod_size_t)value_size;
        /* insert kv pair into scratch pad */
        if (iod_kv_set(mdkv_oh, IOD_TID_UNKNOWN, NULL, &kv, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't set KV pair in parent");
        HDfree(kv.key);
        free(kv.value);

        kv.key = HDstrdup("datatype_size");
        kv.value_len = sizeof(iod_size_t);
        if(NULL == (kv.value = malloc (kv.value_len)))
            HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate buffer");
        memcpy(kv.value, &buf_size, kv.value_len);

        /* insert kv pair into scratch pad */
        if (iod_kv_set(mdkv_oh, IOD_TID_UNKNOWN, NULL, &kv, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't set KV pair in parent");
        HDfree(kv.key);
        free(kv.value);
#endif

        /* close the Metadata KV object */
        if(iod_obj_close(mdkv_oh, NULL, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close object");

        kv.key = HDstrdup(last_comp);
        kv.value = &dtype_id;
        kv.value_len = sizeof(iod_obj_id_t);
        /* insert new datatype in kv store of current group */
        if (iod_kv_set(cur_oh, IOD_TID_UNKNOWN, NULL, &kv, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't set KV pair in parent");
        HDfree(kv.key);
    }
#if H5_DO_NATIVE
    cur_oh.cookie = H5Tcopy(input->type_id);
    if(H5Tcommit2(loc_handle.cookie, last_comp, cur_oh.cookie, 
                  H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't commit datatype");
    fprintf(stderr, "Committed Native Datatype %s with ID %d on %d\n",
            last_comp, cur_oh.cookie, loc_handle.cookie);
#endif

    /* close parent group and its scratch pad if it is not the
       location we started the traversal into */
    if(loc_handle.cookie != cur_oh.cookie) {
        iod_obj_close(cur_oh, NULL, NULL);
    }

    output.iod_oh = cur_oh;

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Done with dtype commit, sending response to client\n");
#endif

    HG_Handler_start_output(op_data->hg_handle, &output);

done:
    if(ret_value < 0) {
        output.iod_oh.cookie = IOD_OH_UNDEFINED;
        HG_Handler_start_output(op_data->hg_handle, &output);
    }

    input = (dtype_commit_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);
    last_comp = (char *)H5MM_xfree(last_comp);
    free(buf);

    FUNC_LEAVE_NOAPI_VOID
} /* end H5VL_iod_server_dtype_commit_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_dtype_open_cb
 *
 * Purpose:	Opens a datatype as a iod object.
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              April, 2013
 *
 *-------------------------------------------------------------------------
 */
void
H5VL_iod_server_dtype_open_cb(AXE_engine_t UNUSED axe_engine, 
                              size_t UNUSED num_n_parents, AXE_task_t UNUSED n_parents[], 
                              size_t UNUSED num_s_parents, AXE_task_t UNUSED s_parents[], 
                              void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    dtype_open_in_t *input = (dtype_open_in_t *)op_data->input;
    dtype_open_out_t output;
    iod_handle_t coh = input->coh;
    iod_handle_t loc_handle = input->loc_oh;
    iod_obj_id_t loc_id = input->loc_id;
    iod_obj_id_t dtype_id;
    iod_handle_t cur_oh, mdkv_oh;
    iod_obj_id_t cur_id, mdkv_id;
    const char *name = input->name;
    char *last_comp; /* the name of the datatype obtained from the last component in the path */
    size_t buf_size;
    void *buf;
    iod_mem_desc_t mem_desc;
    iod_blob_iodesc_t file_desc;
    iod_size_t kv_size = sizeof(iod_obj_id_t);
    scratch_pad_t sp;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    fprintf(stderr, "Start datatype Open %s\n", name);

    /* the traversal will retrieve the location where the datatype needs
       to be opened. The traversal will fail if an intermediate group
       does not exist. */
    if(H5VL_iod_server_traverse(coh, loc_id, loc_handle, name, FALSE, 
                                &last_comp, &cur_id, &cur_oh) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't traverse path");

    if(iod_kv_get_value(cur_oh, IOD_TID_UNKNOWN, last_comp, &dtype_id, 
                        kv_size , NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't retrieve BLOB ID from parent KV store");

    /* open the datatype */
    if (iod_obj_open_write(coh, dtype_id, NULL /*hints*/, &cur_oh, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't open datatype");

    /* get scratch pad of the datatype */
    if(iod_obj_get_scratch(cur_oh, IOD_TID_UNKNOWN, &sp, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "can't get scratch pad for object");

    /* open the metadata scratch pad */
    if (iod_obj_open_write(coh, sp.mdkv_id, NULL /*hints*/, &mdkv_oh, NULL) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "can't open scratch pad");

    /* MSC - need to read datatype; size should be stored in metadata,
       but since no real IOD, can't do anything now */

#if 0
    /*retrieve tcpl metadata from scratch pad */
    if(iod_kv_get_value(mdkv_oh, IOD_TID_UNKNOWN, "datatype_tcpl", NULL, 
                        &output.tcpl_size, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "datatype tcpl lookup failed");
    if(NULL == (output.tcpl = H5MM_malloc (output.tcpl_size)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate tcpl buffer");
    if(iod_kv_get_value(mdkv_oh, IOD_TID_UNKNOWN, "datatype_tcpl", output.tcpl, 
                        &output.tcpl_size, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "datatype tcpl lookup failed");

    /*retrieve blob size metadata from scratch pad */
    if(iod_kv_get_value(mdkv_oh, IOD_TID_UNKNOWN, "datatype_size", &buf_size, 
                        sizeof(iod_size_t), NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "datatype size lookup failed");

    if(NULL == (buf = malloc(buf_size)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate BLOB read buffer");

    /* create memory descriptor for writing */
    mem_desc.nfrag = 1;
    mem_desc.frag->addr = buf;
    mem_desc.frag->len = (iod_size_t)buf_size;

    /* create file descriptor for writing */
    file_desc.nfrag = 1;
    file_desc.frag->offset = 0;
    file_desc.frag->len = (iod_size_t)buf_size;

    /* write the serialized type value to the BLOB object */
    if(iod_blob_write(cur_oh, IOD_TID_UNKNOWN, NULL, &mem_desc, &file_desc, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to write BLOB object");

    /* decode the datatype */
    if((output.type_id = H5Tdecode(buf)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to decode datatype");

    free(buf);
#endif

    /* close the metadata scratch pad */
    if(iod_obj_close(mdkv_oh, NULL, NULL))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close object");

#if H5_DO_NATIVE
    printf("datatype name %s    location %d\n", name, loc_handle.cookie);
    cur_oh.cookie = H5Topen(loc_handle.cookie, name, input->tapl_id);
    HDassert(cur_oh.cookie);
    output.type_id = cur_oh.cookie;
    output.tcpl_id = H5P_DATATYPE_CREATE_DEFAULT;
#else
    /* fake a type, and tcpl */
    output.type_id = H5Tcopy(H5T_NATIVE_INT);
    output.tcpl_id = H5P_DATATYPE_CREATE_DEFAULT;
#endif

    output.iod_id = dtype_id;
    output.iod_oh = cur_oh;

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Done with dtype open, sending response to client\n");
#endif

    HG_Handler_start_output(op_data->hg_handle, &output);

done:
    if(ret_value < 0) {
        output.iod_oh.cookie = IOD_OH_UNDEFINED;
        output.iod_id = IOD_ID_UNDEFINED;
        HG_Handler_start_output(op_data->hg_handle, &output);
    }

#if !H5_DO_NATIVE
    H5Tclose(output.type_id);
#endif

    last_comp = (char *)H5MM_xfree(last_comp);
    input = (dtype_open_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

    FUNC_LEAVE_NOAPI_VOID
} /* end H5VL_iod_server_dtype_open_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_dtype_close_cb
 *
 * Purpose:	Closes iod HDF5 datatype.
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              April, 2013
 *
 *-------------------------------------------------------------------------
 */
void
H5VL_iod_server_dtype_close_cb(AXE_engine_t UNUSED axe_engine, 
                               size_t UNUSED num_n_parents, AXE_task_t UNUSED n_parents[], 
                               size_t UNUSED num_s_parents, AXE_task_t UNUSED s_parents[], 
                               void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    dtype_close_in_t *input = (dtype_close_in_t *)op_data->input;
    iod_handle_t iod_oh = input->iod_oh;
    iod_obj_id_t iod_id = input->iod_id; 
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Start datatype Close\n");
#endif

    if(iod_oh.cookie != IOD_OH_UNDEFINED) {
#if H5_DO_NATIVE
        HDassert(H5Tclose(iod_oh.cookie) == SUCCEED);
#endif

        if((ret_value = iod_obj_close(iod_oh, NULL, NULL)) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close object");
    }
    else {
        /* MSC - need a way to kill object handle for this group */
        fprintf(stderr, "I do not have the OH of this datatype to close it\n");
    }

done:
#if H5VL_IOD_DEBUG
    fprintf(stderr, "Done with dtype close, sending response to client\n");
#endif

    HG_Handler_start_output(op_data->hg_handle, &ret_value);

    input = (dtype_close_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

    FUNC_LEAVE_NOAPI_VOID
} /* end H5VL_iod_server_dtype_close_cb() */

#endif /* H5_HAVE_EFF */
