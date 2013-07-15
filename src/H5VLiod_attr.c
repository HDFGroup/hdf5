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
 * Purpose:	The IOD plugin server side attribute routines.
 */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_attr_create_cb
 *
 * Purpose:	Creates a attr as a iod object.
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
H5VL_iod_server_attr_create_cb(AXE_engine_t UNUSED axe_engine, 
                               size_t UNUSED num_n_parents, AXE_task_t UNUSED n_parents[], 
                               size_t UNUSED num_s_parents, AXE_task_t UNUSED s_parents[], 
                               void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    attr_create_in_t *input = (attr_create_in_t *)op_data->input;
    attr_create_out_t output;
    iod_handle_t coh = input->coh;
    iod_handle_t loc_handle = input->loc_oh;
    iod_obj_id_t loc_id = input->loc_id; /* The ID of the current location object */
    iod_obj_id_t attr_id = input->attr_id; /* The ID of the attribute that needs to be created */
    iod_handle_t attr_oh, attr_kv_oh, cur_oh, mdkv_oh;
    iod_obj_id_t cur_id, mdkv_id;
    const char *loc_name = input->path;
    const char *attr_name = input->attr_name;
    char *last_comp = NULL;
    iod_array_struct_t array;
    iod_size_t *max_dims;
    iod_kv_t kv;
    size_t buf_size;
    scratch_pad_t sp;
    iod_ret_t ret;
    hbool_t collective = FALSE; /* MSC - change when we allow for collective */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Start attribute Create %s on object path %s\n", attr_name, loc_name);
#endif

    /* the traversal will retrieve the location where the attribute needs
       to be created. The traversal will fail if an intermediate group
       does not exist. */
    if(H5VL_iod_server_traverse(coh, loc_id, loc_handle, loc_name, FALSE, 
                                &last_comp, &cur_id, &cur_oh) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't traverse path");

    /* Set the IOD array creation parameters */
    array.cell_size = H5Tget_size(input->type_id);
    array.num_dims = H5Sget_simple_extent_ndims(input->space_id);
    if(NULL == (array.current_dims = (iod_size_t *)malloc (sizeof(iod_size_t) * array.num_dims)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate dimention size array");
    if(NULL == (max_dims = (iod_size_t *)malloc (sizeof(iod_size_t) * array.num_dims)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate dimention size array");
    if(H5Sget_simple_extent_dims(input->space_id, array.current_dims, max_dims) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't get dimentions' sizes");
    array.firstdim_max = max_dims[0];
    array.chunk_dims = NULL;
    array.dims_seq = NULL;

    /* create the attribute */
    ret = iod_obj_create(coh, IOD_TID_UNKNOWN, NULL/*hints*/, IOD_OBJ_ARRAY, NULL, &array,
                         &attr_id, NULL /*event*/);
    if(collective && (0 == ret || EEXISTS == ret)) {
        /* Attribute has been created by another process, open it */
        if (iod_obj_open_write(coh, attr_id, NULL, &attr_oh, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't open attribute");
    }
    else if(!collective && 0 != ret) {
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't create Attribute");
    }

    /* for the process that succeeded in creating the attribute, update
       the parent scratch pad, create attribute scratch pad */
    if(0 == ret) {
        /* create the metadata KV object for the attribute */
        if(iod_obj_create(coh, IOD_TID_UNKNOWN, NULL, IOD_OBJ_KV, 
                          NULL, NULL, &mdkv_id, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't create metadata KV object");

        /* set values for the scratch pad object */
        sp.mdkv_id = mdkv_id;
        sp.attr_id = IOD_ID_UNDEFINED;
        sp.filler1_id = IOD_ID_UNDEFINED;
        sp.filler2_id = IOD_ID_UNDEFINED;

        /* set scratch pad in attribute */
        if (iod_obj_set_scratch(attr_oh, IOD_TID_UNKNOWN, &sp, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't set scratch pad");

        /* Store Metadata in scratch pad */
        if (iod_obj_open_write(coh, mdkv_id, NULL, &mdkv_oh, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't create scratch pad");

        /* MSC - TODO store things */

#if 0
        /* insert attribute metadata into scratch pad */
        kv.key = HDstrdup("attribute_dtype");
        /* determine the buffer size needed to store the encoded type of the attribute */ 
        if(H5Tencode(input->type_id, NULL, &buf_size) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "failed to encode attribute type");
        if(NULL == (kv.value = malloc (buf_size)))
            HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate type buffer");
        /* encode datatype of the attribute */ 
        if(H5Tencode(input->type_id, kv.value, &buf_size) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "failed to encode attribute type");
        kv.value_len = (iod_size_t)buf_size;
        /* insert kv pair into scratch pad */
        if (iod_kv_set(mdkv_oh, IOD_TID_UNKNOWN, NULL, &kv, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't set KV pair in parent");
        HDfree(kv.key);
        free(kv.value);

        kv.key = HDstrdup("attribute_dspace");
        /* determine the buffer size needed to store the encoded space of the attribute */ 
        if(H5Sencode(input->space_id, NULL, &buf_size) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "failed to encode attribute space");
        if(NULL == (kv.value = malloc (buf_size)))
            HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate space buffer");
        /* encode dataspace of the attribute */ 
        if(H5Sencode(input->space_id, kv.value, &buf_size) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "failed to encode attribute space");
        kv.value_len = (iod_size_t)buf_size;
        /* insert kv pair into scratch pad */
        if (iod_kv_set(mdkv_oh, IOD_TID_UNKNOWN, NULL, &kv, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't set KV pair in parent");
        HDfree(kv.key);
        free(kv.value);
#endif

        /* close the Metadata KV object */
        if(iod_obj_close(mdkv_oh, NULL, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close object");

        /* get scratch pad of the parent */
        if(iod_obj_get_scratch(cur_oh, IOD_TID_UNKNOWN, &sp, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, FAIL, "can't get scratch pad for object");

        /* open the attribute KV in scratch pad */
        if (iod_obj_open_write(coh, sp.attr_id, NULL /*hints*/, &attr_kv_oh, NULL) < 0)
            HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, FAIL, "can't open scratch pad");
            
        /* insert new attribute in scratch pad of current object */
        kv.key = HDstrdup(attr_name);
        kv.value = &attr_id;
        kv.value_len = 0;
        if (iod_kv_set(attr_kv_oh, IOD_TID_UNKNOWN, NULL, &kv, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't set KV pair in parent");
        HDfree(kv.key);

        iod_obj_close(attr_kv_oh, NULL, NULL);
    }

    /* close parent group if it is not the location we started the
       traversal into */
    if(loc_handle.cookie != cur_oh.cookie) {
        iod_obj_close(cur_oh, NULL, NULL);
    }

#if H5_DO_NATIVE
    cur_oh.cookie = H5Acreate2(cur_oh.cookie, attr_name, input->type_id, 
                               input->space_id, H5P_DEFAULT, H5P_DEFAULT);
    HDassert(cur_oh.cookie);
#endif

    output.iod_oh = cur_oh;

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Done with attr create, sending response to client\n");
#endif

    HG_Handler_start_output(op_data->hg_handle, &output);

done:

    /* return an UNDEFINED oh to the client if the operation failed */
    if(ret_value < 0) {
        output.iod_oh.cookie = IOD_OH_UNDEFINED;
        HG_Handler_start_output(op_data->hg_handle, &output);
    }

    if(max_dims) free(max_dims);
    if(array.current_dims) free(array.current_dims);
    input = (attr_create_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);
    last_comp = (char *)H5MM_xfree(last_comp);

    FUNC_LEAVE_NOAPI_VOID
} /* end H5VL_iod_server_attr_create_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_attr_open_cb
 *
 * Purpose:	Opens a attribute as a iod object.
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
H5VL_iod_server_attr_open_cb(AXE_engine_t UNUSED axe_engine, 
                             size_t UNUSED num_n_parents, AXE_task_t UNUSED n_parents[], 
                             size_t UNUSED num_s_parents, AXE_task_t UNUSED s_parents[], 
                             void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    attr_open_in_t *input = (attr_open_in_t *)op_data->input;
    attr_open_out_t output;
    iod_handle_t coh = input->coh;
    iod_handle_t loc_handle = input->loc_oh;
    iod_obj_id_t loc_id = input->loc_id;
    iod_handle_t attr_kv_oh, cur_oh, mdkv_oh;
    iod_obj_id_t cur_id, mdkv_id;
    iod_obj_id_t attr_id;
    const char *loc_name = input->path;
    const char *attr_name = input->attr_name;
    char *last_comp = NULL;
    scratch_pad_t sp;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Start attribute Open %s\n", attr_name);
#endif

    /* the traversal will retrieve the location where the attribute needs
       to be opened. The traversal will fail if an intermediate group
       does not exist. */
    if(H5VL_iod_server_traverse(coh, loc_id, loc_handle, loc_name, FALSE, 
                                &last_comp, &cur_id, &cur_oh) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't traverse path");

    /* get scratch pad of the parent */
    if(iod_obj_get_scratch(cur_oh, IOD_TID_UNKNOWN, &sp, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, FAIL, "can't get scratch pad for object");

    /* MSC - Dont do this check until we have a real IOD */
#if 0
    /* if attribute KV does not exist, return error*/
    if(IOD_ID_UNDEFINED == sp.attr_id)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, FAIL, "Object has no attributes");
#endif

    /* open the attribute KV in scratch pad */
    if (iod_obj_open_write(coh, sp.attr_id, NULL /*hints*/, &attr_kv_oh, NULL) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, FAIL, "can't open scratch pad");

    /* get attribute ID */
    if(iod_kv_get_value(attr_kv_oh, IOD_TID_UNKNOWN, attr_name, &attr_id, 
                        sizeof(iod_obj_id_t) , NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't retrieve Attribute ID from parent KV store");

    /* close parent group if it is not the location we started the
       traversal into */
    if(loc_handle.cookie != cur_oh.cookie) {
        iod_obj_close(cur_oh, NULL, NULL);
    }

    iod_obj_close(attr_kv_oh, NULL, NULL);

    /* open the attribute */
    if (iod_obj_open_write(coh, attr_id, NULL /*hints*/, &cur_oh, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't open current group");

    /* get scratch pad of the attribute */
    if(iod_obj_get_scratch(cur_oh, IOD_TID_UNKNOWN, &sp, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, FAIL, "can't get scratch pad for object");

    /* open the metadata scratch pad of the attribute */
    if (iod_obj_open_write(coh, sp.mdkv_id, NULL /*hints*/, &mdkv_oh, NULL) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "can't open scratch pad");

    /* MSC - retrieve all metadata from scratch pad */

    /* close the metadata scratch pad */
    if(iod_obj_close(mdkv_oh, NULL, NULL))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close object");

    {
        hsize_t dims[1];
        //hid_t space_id, type_id;

#if H5_DO_NATIVE
        printf("attr name %s  location %d %s\n", attr_name, loc_handle.cookie, loc_name);
        if(strcmp(loc_name, ".") == 0)
            cur_oh.cookie = H5Aopen(loc_handle.cookie, attr_name, H5P_DEFAULT);
        else
            cur_oh.cookie = H5Aopen_by_name(loc_handle.cookie, loc_name, 
                                            attr_name, H5P_DEFAULT, H5P_DEFAULT);
        HDassert(cur_oh.cookie);
        output.space_id = H5Aget_space(cur_oh.cookie);
        output.type_id = H5Aget_type(cur_oh.cookie);
        output.acpl_id = H5P_ATTRIBUTE_CREATE_DEFAULT;
#else
        /* fake a dataspace, type, and dcpl */
        dims [0] = 60;
        output.space_id = H5Screate_simple(1, dims, NULL);
        output.type_id = H5Tcopy(H5T_NATIVE_INT);
        output.acpl_id = H5P_ATTRIBUTE_CREATE_DEFAULT;
#endif
    }

    output.iod_id = attr_id;
    output.iod_oh = cur_oh;

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Done with attr open, sending response to client\n");
#endif

    HG_Handler_start_output(op_data->hg_handle, &output);

done:
    if(ret_value < 0) {
        output.iod_oh.cookie = IOD_OH_UNDEFINED;
        output.iod_id = IOD_ID_UNDEFINED;
        HG_Handler_start_output(op_data->hg_handle, &output);
    }

    H5Sclose(output.space_id);
    H5Tclose(output.type_id);

    input = (attr_open_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);
    last_comp = (char *)H5MM_xfree(last_comp);

    FUNC_LEAVE_NOAPI_VOID
} /* end H5VL_iod_server_attr_open_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_attr_read_cb
 *
 * Purpose:	Reads from IOD into the function shipper BDS handle.
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
H5VL_iod_server_attr_read_cb(AXE_engine_t UNUSED axe_engine, 
                             size_t UNUSED num_n_parents, AXE_task_t UNUSED n_parents[], 
                             size_t UNUSED num_s_parents, AXE_task_t UNUSED s_parents[], 
                             void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    attr_io_in_t *input = (attr_io_in_t *)op_data->input;
    iod_handle_t coh = input->coh;
    iod_handle_t iod_oh = input->iod_oh;
    iod_obj_id_t iod_id = input->iod_id; 
    hg_bulk_t bulk_handle = input->bulk_handle;
    hid_t type_id = input->type_id;
    hg_bulk_block_t bulk_block_handle;
    hg_bulk_request_t bulk_request;
    iod_mem_desc_t mem_desc;
    iod_array_iodesc_t file_desc;
    size_t size;
    void *buf;
    na_addr_t dest = HG_Handler_get_addr(op_data->hg_handle);
    hbool_t opened_locally = FALSE;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* open the attribute if we don't have the handle yet */
    if(iod_oh.cookie == IOD_OH_UNDEFINED) {
        if (iod_obj_open_write(coh, iod_id, NULL /*hints*/, &iod_oh, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't open current group");        
        opened_locally = TRUE;
    }

    size = HG_Bulk_handle_get_size(bulk_handle);

    if(NULL == (buf = malloc(size)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate read buffer");

#if 0
    /* create memory descriptor for reading */
    mem_desc.nfrag = 1;
    mem_desc.frag->addr = buf;
    mem_desc.frag->len = (iod_size_t)size;

    /* retrieve the dataspace of the attribute and create file descriptor for reading */
    /* MSC TODO - populate file descriptor hyperslab */
#endif

    /* read from array object */
    if(iod_array_read(iod_oh, IOD_TID_UNKNOWN, NULL, &mem_desc, &file_desc, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_READERROR, FAIL, "can't read from array object");
    
    {
        int i;
        hbool_t flag;
        int *buf_ptr = (int *)buf;

#if H5_DO_NATIVE
    ret_value = H5Aread(iod_oh.cookie, type_id, buf);
#else
    for(i=0;i<60;++i)
        buf_ptr[i] = i;
#endif
    }

    /* Create a new block handle to write the data */
    HG_Bulk_block_handle_create(buf, size, HG_BULK_READ_ONLY, &bulk_block_handle);

    /* Write bulk data here and wait for the data to be there  */
    if(HG_SUCCESS != HG_Bulk_write_all(dest, bulk_handle, bulk_block_handle, &bulk_request))
        HGOTO_ERROR(H5E_SYM, H5E_READERROR, FAIL, "can't read from array object");
    /* wait for it to complete */
    if(HG_SUCCESS != HG_Bulk_wait(bulk_request, HG_BULK_MAX_IDLE_TIME, HG_BULK_STATUS_IGNORE))
        HGOTO_ERROR(H5E_SYM, H5E_READERROR, FAIL, "can't read from array object");

done:
#if H5VL_IOD_DEBUG
    fprintf(stderr, "Done with attr read, sending response to client\n");
#endif

    if(HG_SUCCESS != HG_Handler_start_output(op_data->hg_handle, &ret_value))
        HDONE_ERROR(H5E_SYM, H5E_WRITEERROR, FAIL, "can't send result of write to client");
    if(HG_SUCCESS != HG_Bulk_block_handle_free(bulk_block_handle))
        HDONE_ERROR(H5E_SYM, H5E_WRITEERROR, FAIL, "can't free bds block handle");

    input = (attr_io_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);
    free(buf);

    /* close the attribute if we opened it in this routine */
    if(opened_locally) {
        if(iod_obj_close(iod_oh, NULL, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close Array object");
    }

    FUNC_LEAVE_NOAPI_VOID
} /* end H5VL_iod_server_attr_read_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_attr_write_cb
 *
 * Purpose:	Writes from IOD into the function shipper BDS handle.
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
H5VL_iod_server_attr_write_cb(AXE_engine_t UNUSED axe_engine, 
                              size_t UNUSED num_n_parents, AXE_task_t UNUSED n_parents[], 
                              size_t UNUSED num_s_parents, AXE_task_t UNUSED s_parents[], 
                              void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    attr_io_in_t *input = (attr_io_in_t *)op_data->input;
    iod_handle_t coh = input->coh;
    iod_handle_t iod_oh = input->iod_oh;
    iod_obj_id_t iod_id = input->iod_id; 
    hg_bulk_t bulk_handle = input->bulk_handle;
    hid_t type_id = input->type_id;
    hg_bulk_block_t bulk_block_handle;
    hg_bulk_request_t bulk_request;
    iod_mem_desc_t mem_desc;
    iod_array_iodesc_t file_desc;
    size_t size;
    void *buf;
    ssize_t ret;
    na_addr_t source = HG_Handler_get_addr(op_data->hg_handle);
    hbool_t opened_locally = FALSE;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* open the attribute if we don't have the handle yet */
    if(iod_oh.cookie == IOD_OH_UNDEFINED) {
        if (iod_obj_open_write(coh, iod_id, NULL /*hints*/, &iod_oh, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't open current group");        
    }

    /* Read bulk data here and wait for the data to be here  */
    size = HG_Bulk_handle_get_size(bulk_handle);
    if(NULL == (buf = malloc(size)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate read buffer");

    HG_Bulk_block_handle_create(buf, size, HG_BULK_READWRITE, &bulk_block_handle);

    /* Write bulk data here and wait for the data to be there  */
    if(HG_SUCCESS != HG_Bulk_read_all(source, bulk_handle, bulk_block_handle, &bulk_request))
        HGOTO_ERROR(H5E_SYM, H5E_WRITEERROR, FAIL, "can't get data from function shipper");
    /* wait for it to complete */
    if(HG_SUCCESS != HG_Bulk_wait(bulk_request, HG_BULK_MAX_IDLE_TIME, HG_BULK_STATUS_IGNORE))
        HGOTO_ERROR(H5E_SYM, H5E_WRITEERROR, FAIL, "can't get data from function shipper");

    /* free the bds block handle */
    if(HG_SUCCESS != HG_Bulk_block_handle_free(bulk_block_handle))
        HGOTO_ERROR(H5E_SYM, H5E_WRITEERROR, FAIL, "can't free bds block handle");

#if H5VL_IOD_DEBUG 
    { 
        int i;
        int *buf_ptr = (int *)buf;

        fprintf(stderr, "AWRITE Received a buffer of size %d with values: ", size);
        for(i=0;i<60;++i)
            fprintf(stderr, "%d ", buf_ptr[i]);
        fprintf(stderr, "\n");
    }
#endif

#if 0
    mem_desc.nfrag = 1;
    mem_desc.frag->addr = buf;
    mem_desc.frag->len = (iod_size_t)size;

    /* retrieve the dataspace of the attribute and create file descriptor for reading */
    /* MSC TODO - populate file descriptor hyperslab */
#endif

    /* write from array object */
    if(iod_array_write(iod_oh, IOD_TID_UNKNOWN, NULL, &mem_desc, &file_desc, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_WRITEERROR, FAIL, "can't write to array object");

#if H5_DO_NATIVE
    ret_value = H5Awrite(iod_oh.cookie, type_id, buf);
#endif

done:
#if H5VL_IOD_DEBUG 
    fprintf(stderr, "Done with attr write, sending %d response to client\n", ret_value);
#endif

    if(HG_SUCCESS != HG_Handler_start_output(op_data->hg_handle, &ret_value))
        HDONE_ERROR(H5E_SYM, H5E_WRITEERROR, FAIL, "can't send result of write to client");

    input = (attr_io_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);
    free(buf);

    /* close the dataset if we opened it in this routine */
    if(opened_locally) {
        if(iod_obj_close(iod_oh, NULL, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close Array object");
    }
    FUNC_LEAVE_NOAPI_VOID
} /* end H5VL_iod_server_attr_write_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_attr_exists_cb
 *
 * Purpose:	Checks if an attribute exists on object.
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
H5VL_iod_server_attr_exists_cb(AXE_engine_t UNUSED axe_engine, 
                               size_t UNUSED num_n_parents, AXE_task_t UNUSED n_parents[], 
                               size_t UNUSED num_s_parents, AXE_task_t UNUSED s_parents[], 
                               void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    attr_op_in_t *input = (attr_op_in_t *)op_data->input;
    iod_handle_t coh = input->coh;
    iod_handle_t loc_handle = input->loc_oh;
    iod_obj_id_t loc_id = input->loc_id;
    iod_handle_t cur_oh, attr_kv_oh;
    iod_obj_id_t cur_id, attr_id;
    const char *loc_name = input->path;
    const char *attr_name = input->attr_name;
    char *last_comp = NULL;
    scratch_pad_t sp;
    htri_t ret = -1;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Start attribute Exists %s\n", attr_name);
#endif

    /* the traversal will retrieve the location where the attribute needs
       to be checked. The traversal will fail if an intermediate group
       does not exist. */
    if(H5VL_iod_server_traverse(coh, loc_id, loc_handle, loc_name, FALSE, 
                                &last_comp, &cur_id, &cur_oh) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't traverse path");

    /* get scratch pad of the parent */
    if(iod_obj_get_scratch(cur_oh, IOD_TID_UNKNOWN, &sp, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, FAIL, "can't get scratch pad for object");

    /* close parent group if it is not the location we started the
       traversal into */
    if(loc_handle.cookie != cur_oh.cookie) {
        iod_obj_close(cur_oh, NULL, NULL);
    }

    /* MSC - Dont do this check until we have a real IOD */
#if 0
    /* if attribute KV does not exist, return false*/
    if(IOD_ID_UNDEFINED == sp.attr_id) {
        ret = FALSE;
        HGOTO_DONE(SUCCEED);
    }
#endif

    /* open the attribute KV in scratch pad */
    if (iod_obj_open_write(coh, sp.attr_id, NULL /*hints*/, &attr_kv_oh, NULL) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, FAIL, "can't open scratch pad");

    /* get attribute ID */
    if(iod_kv_get_value(attr_kv_oh, IOD_TID_UNKNOWN, attr_name, &attr_id, 
                        sizeof(iod_obj_id_t) , NULL, NULL) < 0) {
        ret = FALSE;
    }
    else {
        ret = TRUE;
    }

    iod_obj_close(attr_kv_oh, NULL, NULL);

#if H5_DO_NATIVE
    ret = H5Aexists(loc_handle.cookie, attr_name);
#else
    ret = FALSE;
#endif

done:

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Done with attr exists, sending response to client\n");
#endif

    HG_Handler_start_output(op_data->hg_handle, &ret);

    input = (attr_op_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);
    last_comp = (char *)H5MM_xfree(last_comp);

    FUNC_LEAVE_NOAPI_VOID
} /* end H5VL_iod_server_attr_exists_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_attr_rename_cb
 *
 * Purpose:	Renames iod HDF5 attribute.
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
H5VL_iod_server_attr_rename_cb(AXE_engine_t UNUSED axe_engine, 
                               size_t UNUSED num_n_parents, AXE_task_t UNUSED n_parents[], 
                               size_t UNUSED num_s_parents, AXE_task_t UNUSED s_parents[], 
                               void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    attr_rename_in_t *input = (attr_rename_in_t *)op_data->input;
    iod_handle_t coh = input->coh;
    iod_handle_t loc_handle = input->loc_oh;
    iod_obj_id_t loc_id = input->loc_id;
    iod_handle_t cur_oh, attr_kv_oh;
    iod_obj_id_t cur_id, attr_id;
    const char *loc_name = input->path;
    const char *old_name = input->old_attr_name;
    const char *new_name = input->new_attr_name;
    char *last_comp = NULL;
    iod_kv_params_t kvs;
    iod_kv_t kv;
    scratch_pad_t sp;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Start attribute Rename %s to %s\n", old_name, new_name);
#endif

    /* the traversal will retrieve the location where the attribute
       needs to be renamed. The traversal will fail if an intermediate
       group does not exist. */
    if(H5VL_iod_server_traverse(coh, loc_id, loc_handle, loc_name, FALSE, 
                                &last_comp, &cur_id, &cur_oh) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't traverse path");

    /* get scratch pad of the parent */
    if(iod_obj_get_scratch(cur_oh, IOD_TID_UNKNOWN, &sp, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, FAIL, "can't get scratch pad for object");

    /* close parent group if it is not the location we started the
       traversal into */
    if(loc_handle.cookie != cur_oh.cookie) {
        iod_obj_close(cur_oh, NULL, NULL);
    }

    /* MSC - Dont do this check until we have a real IOD */
#if 0
    /* if attribute KV does not exist, return error*/
    if(IOD_ID_UNDEFINED == sp.attr_id)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, FAIL, "Object has no attributes");
#endif

    /* open the attribute KV in scratch pad */
    if (iod_obj_open_write(coh, sp.attr_id, NULL /*hints*/, &attr_kv_oh, NULL) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, FAIL, "can't open scratch pad");

    /* get attribute ID */
    if(iod_kv_get_value(attr_kv_oh, IOD_TID_UNKNOWN, old_name, &attr_id, 
                        sizeof(iod_obj_id_t) , NULL, NULL) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, FAIL, "Attribute does not exist");

    /* remove attribute with old name */
    kv.key = old_name;
    kv.value = &attr_id;
    kv.value_len = sizeof(iod_obj_id_t);
    kvs.kv = &kv;
    if(iod_kv_unlink_keys(attr_kv_oh,IOD_TID_UNKNOWN, NULL, 1, &kvs, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "Unable to unlink KV pair");

    /* insert attribute with new name */
    kv.key = strdup(new_name);
    kv.value = &attr_id;
    kv.value_len = sizeof(iod_obj_id_t);
    if (iod_kv_set(attr_kv_oh, IOD_TID_UNKNOWN, NULL, &kv, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't set KV pair in parent");
    HDfree(kv.key);

    iod_obj_close(attr_kv_oh, NULL, NULL);

#if H5_DO_NATIVE
    ret_value = H5Arename(loc_handle.cookie, old_name, new_name);
#endif

done:

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Done with attr rename, sending response to client\n");
#endif

    HG_Handler_start_output(op_data->hg_handle, &ret_value);

    input = (attr_rename_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);
    last_comp = (char *)H5MM_xfree(last_comp);

    FUNC_LEAVE_NOAPI_VOID
} /* end H5VL_iod_server_attr_rename_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_attr_remove_cb
 *
 * Purpose:	Removes iod HDF5 attribute.
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
H5VL_iod_server_attr_remove_cb(AXE_engine_t UNUSED axe_engine, 
                               size_t UNUSED num_n_parents, AXE_task_t UNUSED n_parents[], 
                               size_t UNUSED num_s_parents, AXE_task_t UNUSED s_parents[], 
                               void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    attr_op_in_t *input = (attr_op_in_t *)op_data->input;
    iod_handle_t coh = input->coh;
    iod_handle_t loc_handle = input->loc_oh;
    iod_obj_id_t loc_id = input->loc_id;
    iod_handle_t cur_oh, attr_kv_oh;
    iod_obj_id_t cur_id, attr_id;
    const char *loc_name = input->path;
    const char *attr_name = input->attr_name;
    char *last_comp = NULL;
    iod_kv_params_t kvs;
    iod_kv_t kv;
    scratch_pad_t sp;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Start attribute Remove %s\n", attr_name);
#endif

    /* the traversal will retrieve the location where the attribute
       needs to be removed. The traversal will fail if an intermediate
       group does not exist. */
    if(H5VL_iod_server_traverse(coh, loc_id, loc_handle, loc_name, FALSE, 
                                &last_comp, &cur_id, &cur_oh) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't traverse path");

    /* get scratch pad of the parent */
    if(iod_obj_get_scratch(cur_oh, IOD_TID_UNKNOWN, &sp, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, FAIL, "can't get scratch pad for object");

    /* close parent group if it is not the location we started the
       traversal into */
    if(loc_handle.cookie != cur_oh.cookie) {
        iod_obj_close(cur_oh, NULL, NULL);
    }

    /* MSC - Dont do this check until we have a real IOD */
#if 0
    /* if attribute KV does not exist, return error*/
    if(IOD_ID_UNDEFINED == sp.attr_id)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, FAIL, "Object has no attributes");
#endif

    /* open the attribute KV in scratch pad */
    if (iod_obj_open_write(coh, sp.attr_id, NULL /*hints*/, &attr_kv_oh, NULL) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, FAIL, "can't open scratch pad");

    /* get attribute ID */
    if(iod_kv_get_value(attr_kv_oh, IOD_TID_UNKNOWN, attr_name, &attr_id, 
                        sizeof(iod_obj_id_t) , NULL, NULL) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, FAIL, "Attribute does not exist");

    /* remove attribute */
    kv.key = attr_name;
    kv.value = &attr_id;
    kv.value_len = sizeof(iod_obj_id_t);
    kvs.kv = &kv;
    if(iod_kv_unlink_keys(attr_kv_oh,IOD_TID_UNKNOWN, NULL, 1, &kvs, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "Unable to unlink KV pair");

    if(iod_obj_unlink(coh, attr_id, IOD_TID_UNKNOWN, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "Unable to unlink object");

#if H5_DO_NATIVE
    ret_value = H5Adelete(loc_handle.cookie, attr_name);
#endif

done:
#if H5VL_IOD_DEBUG
    fprintf(stderr, "Done with attr remove, sending response to client\n");
#endif

    HG_Handler_start_output(op_data->hg_handle, &ret_value);

    input = (attr_op_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);
    last_comp = (char *)H5MM_xfree(last_comp);

    FUNC_LEAVE_NOAPI_VOID
} /* end H5VL_iod_server_attr_remove_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_attr_close_cb
 *
 * Purpose:	Closes iod HDF5 attribute.
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
H5VL_iod_server_attr_close_cb(AXE_engine_t UNUSED axe_engine, 
                              size_t UNUSED num_n_parents, AXE_task_t UNUSED n_parents[], 
                              size_t UNUSED num_s_parents, AXE_task_t UNUSED s_parents[], 
                              void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    attr_close_in_t *input = (attr_close_in_t *)op_data->input;
    iod_handle_t iod_oh = input->iod_oh;
    iod_obj_id_t iod_id = input->iod_id; 
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Start attribute Close\n");
#endif

    if(iod_oh.cookie != IOD_OH_UNDEFINED) {
#if H5_DO_NATIVE
        HDassert(H5Aclose(iod_oh.cookie) == SUCCEED);
#endif
        if((ret_value = iod_obj_close(iod_oh, NULL, NULL)) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close object");
    }
    else {
        /* MSC - need a way to kill object handle for this group */
        fprintf(stderr, "I do not have the OH of this attribute to close it\n");
    }

done:
#if H5VL_IOD_DEBUG
    fprintf(stderr, "Done with attr close, sending response to client\n");
#endif

    HG_Handler_start_output(op_data->hg_handle, &ret_value);

    input = (attr_close_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

    FUNC_LEAVE_NOAPI_VOID
} /* end H5VL_iod_server_attr_close_cb() */

#endif /* H5_HAVE_EFF */
