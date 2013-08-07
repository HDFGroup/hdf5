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
 *              February, 2013
 *
 * Purpose:	The IOD plugin server side file routines.
 */

static iod_obj_id_t ROOT_ID;


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_file_create_cb
 *
 * Purpose:	Creates a file as a iod HDF5 file.
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              January, 2013
 *
 *-------------------------------------------------------------------------
 */
void
H5VL_iod_server_file_create_cb(AXE_engine_t UNUSED axe_engine, 
                               size_t UNUSED num_n_parents, AXE_task_t UNUSED n_parents[], 
                               size_t UNUSED num_s_parents, AXE_task_t UNUSED s_parents[], 
                               void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    file_create_in_t *input = (file_create_in_t *)op_data->input;
    file_create_out_t output;
    unsigned int mode; /* create mode */
    iod_handle_t coh; /* container handle */
    iod_handle_t root_oh; /* root object handle */
    iod_handle_t mdkv_oh; /* metadata object handle for KV to store file's metadata */
    iod_obj_id_t mdkv_id, attr_id; /* metadata and attribute KV IDs for the file */
    iod_ret_t ret;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Start file create %s %d %d\n", input->name, input->fapl_id, input->fcpl_id);
#endif

    /* convert HDF5 flags to IOD flags */
    mode = (input->flags&H5F_ACC_RDWR) ? IOD_CONT_RW : IOD_CONT_RO;
    if (input->flags&H5F_ACC_CREAT) 
        mode |= IOD_CONT_CREATE;

    /* Create the Container */
    if(iod_container_open(input->name, NULL /*hints*/, mode, &coh, NULL /*event*/) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "can't create container");

    /* create the root group */
    ret = iod_obj_create(coh, IOD_TID_UNKNOWN, NULL, IOD_OBJ_KV, NULL, NULL, 
                         &input->root_id, NULL);
    if(0 == ret || EEXISTS == ret) {
        /* root group has been created, open it */
        if (iod_obj_open_write(coh, input->root_id, NULL, &root_oh, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't open root group");
    }
    else {
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't create root group");
    }

    /* for the process that succeeded in creating the group, create
       the scratch pad for it too */
    if(0 == ret) {
        scratch_pad_t sp;
        iod_kv_t kv;
        void *key = NULL;
        void *value = NULL;
        hid_t fcpl_id;

        /* create the metadata KV object for the root group */
        if(iod_obj_create(coh, IOD_TID_UNKNOWN, NULL, IOD_OBJ_KV, 
                          NULL, NULL, &mdkv_id, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't create metadata KV object");

        /* create the attribute KV object for the root group */
        if(iod_obj_create(coh, IOD_TID_UNKNOWN, NULL, IOD_OBJ_KV, 
                          NULL, NULL, &attr_id, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't create metadata KV object");

        /* set values for the scratch pad object */
        sp.mdkv_id = mdkv_id;
        sp.attr_id = attr_id;
        sp.filler1_id = IOD_ID_UNDEFINED;
        sp.filler2_id = IOD_ID_UNDEFINED;

        /* set scratch pad in root group */
        if (iod_obj_set_scratch(root_oh, IOD_TID_UNKNOWN, &sp, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't set scratch pad");

        /* Store Metadata in scratch pad */
        if (iod_obj_open_write(coh, mdkv_id, NULL, &mdkv_oh, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't create scratch pad");

        /* store metadata */


        if(H5P_DEFAULT == input->fcpl_id)
            fcpl_id = H5P_FILE_CREATE_DEFAULT;
        else
            fcpl_id = input->fcpl_id;

        /* insert plist metadata */
        if(H5VL_iod_insert_plist(mdkv_oh, IOD_TID_UNKNOWN, fcpl_id, 
                                 NULL, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't insert link count KV value");

        /* insert initial indexes for IOD IDs */
        if(NULL == (value = malloc (sizeof(uint64_t))))
            HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate buffer");
        *((uint64_t *)value) = 1;
        kv.value_len = sizeof(uint64_t);

        key = strdup(H5VL_IOD_KEY_KV_IDS_INDEX);
        kv.key = (char *)key;
        if (iod_kv_set(mdkv_oh, IOD_TID_UNKNOWN, NULL, &kv, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't set KV pair in parent");
        free(key); 
        key = NULL;

        key = strdup(H5VL_IOD_KEY_ARRAY_IDS_INDEX);
        kv.key = (char *)key;
        if (iod_kv_set(mdkv_oh, IOD_TID_UNKNOWN, NULL, &kv, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't set KV pair in parent");
        free(key); 
        key = NULL;

        key = strdup(H5VL_IOD_KEY_BLOB_IDS_INDEX);
        kv.key = (char *)key;
        if (iod_kv_set(mdkv_oh, IOD_TID_UNKNOWN, NULL, &kv, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't set KV pair in parent");
        free(key); 
        key = NULL;

        free(value); 
        value = NULL;

        if(iod_obj_close(mdkv_oh, NULL, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close root object handle");
    }

#if H5_DO_NATIVE
    coh.cookie = H5Fcreate(input->name, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    HDassert(coh.cookie);
    root_oh.cookie = coh.cookie;
    fprintf(stderr, "Created Native file %s with ID %d\n", input->name, root_oh.cookie);
#endif

    output.coh = coh;
    output.root_oh = root_oh;
    output.kv_oid_index = 1;
    output.array_oid_index = 1;
    output.blob_oid_index = 1;

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Done with file create, sending response to client \n");
#endif

    HG_Handler_start_output(op_data->hg_handle, &output);

done:
    if(ret_value < 0) {
        output.coh.cookie = IOD_OH_UNDEFINED;
        output.root_oh.cookie = IOD_OH_UNDEFINED;
        output.kv_oid_index = 0;
        output.array_oid_index = 0;
        output.blob_oid_index = 0;
        HG_Handler_start_output(op_data->hg_handle, &ret_value);
    }

    input = (file_create_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

    FUNC_LEAVE_NOAPI_VOID
} /* end H5VL_iod_server_file_create_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_file_open_cb
 *
 * Purpose:	Opens a file as a iod HDF5 file.
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              January, 2013
 *
 *-------------------------------------------------------------------------
 */
void
H5VL_iod_server_file_open_cb(AXE_engine_t UNUSED axe_engine, 
                             size_t UNUSED num_n_parents, AXE_task_t UNUSED n_parents[], 
                             size_t UNUSED num_s_parents, AXE_task_t UNUSED s_parents[], 
                             void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    file_open_in_t *input = (file_open_in_t *)op_data->input;
    file_open_out_t output;
    unsigned int mode = input->flags; /* File Open mode */
    iod_handle_t coh; /* container handle */
    iod_handle_t root_oh; /* root object handle */
    iod_handle_t mdkv_oh; /* metadata object handle for KV to store file's metadata */
    scratch_pad_t sp;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Start file open %s %d %d\n", input->name, input->flags, input->fapl_id);
#endif

    /* open the container */
    if(iod_container_open(input->name, NULL /*hints*/, mode, &coh, NULL /*event*/))
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "can't open file");

    /* open the root group */
    if (iod_obj_open_write(coh, ROOT_ID, NULL /*hints*/, &root_oh, NULL) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "can't open root object");

    /* get scratch pad of root group */
    if(iod_obj_get_scratch(root_oh, IOD_TID_UNKNOWN, &sp, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "can't get scratch pad for root object");

    /* open the metadata scratch pad */
    if (iod_obj_open_write(coh, sp.mdkv_id, NULL /*hints*/, &mdkv_oh, NULL) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "can't open scratch pad");

    /* retrieve all metadata from scratch pad */
    /* MSC - fake for now */
    output.kv_oid_index = 1;
    output.array_oid_index = 1;
    output.blob_oid_index = 1;
    output.fcpl_id = H5P_FILE_CREATE_DEFAULT;

    /* MSC - NEED IOD */
#if 0
    if(H5VL_iod_get_metadata(mdkv_oh, IOD_TID_UNKNOWN, H5VL_IOD_PLIST, H5VL_IOD_KEY_OBJ_CPL,
                             NULL, NULL, &output.fcpl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "failed to retrieve fcpl");

    if(iod_kv_get_value(mdkv_oh, IOD_TID_UNKNOWN, H5VL_IOD_KEY_KV_IDS_INDEX, &output.kv_oid_index, 
                        sizeof(uint64_t), NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "KV index lookup failed");

    if(iod_kv_get_value(mdkv_oh, IOD_TID_UNKNOWN, H5VL_IOD_KEY_ARRAY_IDS_INDEX, &output.array_oid_index, 
                        sizeof(uint64_t), NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Array index lookup failed");

    if(iod_kv_get_value(mdkv_oh, IOD_TID_UNKNOWN, H5VL_IOD_KEY_BLOB_IDS_INDEX, &output.blob_oid_index, 
                        sizeof(uint64_t), NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "BLOB index lookup failed");
#endif

    /* close the metadata scratch pad */
    if(iod_obj_close(mdkv_oh, NULL, NULL))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close root object handle");

#if H5_DO_NATIVE
    {
        coh.cookie = H5Fopen(input->name, H5F_ACC_RDWR, H5P_DEFAULT);
        assert(coh.cookie);
        root_oh.cookie = coh.cookie;
        fprintf(stderr, "Opened Native file %s with ID %d\n", input->name, root_oh.cookie);
    }
#endif

    output.coh = coh;
    output.root_id = ROOT_ID;
    output.root_oh = root_oh;

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Done with file open, sending response to client\n");
#endif

    HG_Handler_start_output(op_data->hg_handle, &output);

done:
    if(ret_value < 0) {
        output.coh.cookie = IOD_OH_UNDEFINED;
        output.root_id = IOD_ID_UNDEFINED;
        output.root_oh.cookie = IOD_OH_UNDEFINED;
        output.fcpl_id = H5P_FILE_CREATE_DEFAULT;
        output.kv_oid_index = 0;
        output.array_oid_index = 0;
        output.blob_oid_index = 0;
        HG_Handler_start_output(op_data->hg_handle, &output);
    }

    input = (file_open_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

    FUNC_LEAVE_NOAPI_VOID
} /* end H5VL_iod_server_file_open_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_file_flush_cb
 *
 * Purpose:	Flushs iod HDF5 file.
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              January, 2013
 *
 *-------------------------------------------------------------------------
 */
void
H5VL_iod_server_file_flush_cb(AXE_engine_t UNUSED axe_engine, 
                              size_t UNUSED num_n_parents, AXE_task_t UNUSED n_parents[], 
                              size_t UNUSED num_s_parents, AXE_task_t UNUSED s_parents[], 
                              void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    file_flush_in_t *input = (file_flush_in_t *)op_data->input;
    iod_handle_t coh = input->coh;
    H5F_scope_t scope = input->scope;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* MSC - TODO */

#if H5_DO_NATIVE
    ret_value = H5Fflush(coh.cookie, scope);
#endif

done:
    if(HG_SUCCESS != HG_Handler_start_output(op_data->hg_handle, &ret_value))
        HDONE_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "can't send result of file flush to client");

    input = (file_flush_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

    FUNC_LEAVE_NOAPI_VOID
} /* end H5VL_iod_server_file_flush_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_file_close_cb
 *
 * Purpose:	Closes iod HDF5 file.
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              January, 2013
 *
 *-------------------------------------------------------------------------
 */
void
H5VL_iod_server_file_close_cb(AXE_engine_t UNUSED axe_engine, 
                              size_t UNUSED num_n_parents, AXE_task_t UNUSED n_parents[], 
                              size_t UNUSED num_s_parents, AXE_task_t UNUSED s_parents[], 
                              void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    file_close_in_t *input = (file_close_in_t *)op_data->input;
    iod_handle_t coh = input->coh;
    iod_handle_t root_oh = input->root_oh;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Start file close\n");
#endif

#if H5_DO_NATIVE
    H5Fclose(coh.cookie);
#endif

    /* close the root group */
    if(iod_obj_close(root_oh, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "can't close root object handle");

    /* close the container */
    if(iod_container_close(coh, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTDEC, FAIL, "can't close container");

done:
#if H5VL_IOD_DEBUG
    fprintf(stderr, "Done with file close, sending response to client\n");
#endif
    if(HG_SUCCESS != HG_Handler_start_output(op_data->hg_handle, &ret_value))
        HDONE_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "can't send result of file close to client");

    input = (file_close_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

    FUNC_LEAVE_NOAPI_VOID
} /* end H5VL_iod_server_file_close_cb() */

#endif /* H5_HAVE_EFF */
