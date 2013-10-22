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
 *              October, 2013
 *
 * Purpose:	Routines to support Query/View objects.
 */


#include "H5VLiod_server.h"

#ifdef H5_HAVE_EFF

#if 0

static herr_t H5VL__iod_server_construct_view(iod_handle_t coh, iod_obj_id_t loc_id, 
                                              iod_trans_id_t rtid, hid_t query, 
                                              size_t *num_tokens, H5VL_token_t *tokens);
static herr_t H5VL__iod_query_selection(iod_handle_t coh, iod_obj_id_t obj_id, 
                                        iod_trans_id_t rtid, hid_t query, 
                                        size_t *num_tokens, H5VL_token_t *tokens); 


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_get_layout_cb
 *
 * Purpose:	Retrieves layout of object
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              September, 2013
 *
 *-------------------------------------------------------------------------
 */
void
H5VL_iod_server_get_layout_cb(AXE_engine_t UNUSED axe_engine, 
                              size_t UNUSED num_n_parents, AXE_task_t UNUSED n_parents[], 
                              size_t UNUSED num_s_parents, AXE_task_t UNUSED s_parents[], 
                              void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    get_layout_in_t *input = (get_layout_in_t *)op_data->input;
    get_layout_out_t output;;
    iod_handle_t coh = input->coh; /* the container handle */
    iod_obj_id_t obj_id = input->obj_id; /* The ID of the object */
    iod_handle_t obj_oh = input->obj_oh; /* object handle */
    H5I_type_t loc_type = input->loc_type; /* type of the location where query needs to be applied */
    iod_trans_id_t rtid = input->rcxt_num;
    iod_layout_t layout;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    if(obj_oh.cookie == IOD_OH_UNDEFINED) {
        if (iod_obj_open_read(coh, obj_id, NULL /*hints*/, &obj_oh, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't open current group");
        opened_locally = TRUE;
    }

    if(iod_obj_get_layout(obj_oh, rtid, &layout, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't get object layout");

    output.ret = ret_value;
    output.layout = layout

    HG_Handler_start_output(op_data->hg_handle, output);

done:

    if(ret_value < 0)
        HG_Handler_start_output(op_data->hg_handle, &ret_value);

    input = (view_create_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

    /* close the dataset if we opened it in this routine */
    if(opened_locally) {
        if(iod_obj_close(obj_oh, NULL, NULL) < 0)
            HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close Array object");
    }

    FUNC_LEAVE_NOAPI_VOID
} /* end H5VL_iod_server_get_layout_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_view_create_cb
 *
 * Purpose:	Creates a view from a provided query.
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              September, 2013
 *
 *-------------------------------------------------------------------------
 */
void
H5VL_iod_server_view_create_cb(AXE_engine_t UNUSED axe_engine, 
                               size_t UNUSED num_n_parents, AXE_task_t UNUSED n_parents[], 
                               size_t UNUSED num_s_parents, AXE_task_t UNUSED s_parents[], 
                               void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    view_create_in_t *input = (view_create_in_t *)op_data->input;
    view_create_out_t *output = NULL;
    iod_handle_t coh = input->coh; /* the container handle */
    iod_obj_id_t loc_id = input->loc_id; /* The ID of the object */
    H5I_type_t loc_type = input->loc_type; /* type of the location where query needs to be applied */
    hid_t query = input->query; /* query object to apply */
    iod_trans_id_t rtid = input->rcxt_num;
    uint32_t cs_scope = input->cs_scope;
    int token_count;
    int num_tokens = 0;
    H5VL_token_t *tokens = NULL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* If the location type is a raw data object, then only 1 token is needed */
    if(loc_type == H5I_MAP || loc_type == H5I_DATASET || loc_type ==  H5I_DATATYPE)
        token_count = 1;
    /* Otherwise the location type is a group or the root group, so we
       set the initial number of tokens to the upperbound total number
       of objects in the container. */
    else if(loc_type == H5I_GROUP || loc_type == H5I_FILE) {
        if(iod_container_list_obj(coh, rtid, IOD_OBJ_ANY, 0, &token_count,
                                  NULL, NULL, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "failed to retrieve total number of objects in container");
    }

    if(NULL == (tokens = (H5VL_token_t *)malloc(sizeof(H5VL_token_t) * token_count)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate token structs");

    if(H5VL__iod_server_construct_view(coh, loc_id, rtid, query, &num_tokens, tokens) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "failed to construct view");

    if(NULL == (output = (view_create_out_t *)H5MM_malloc(sizeof(view_create_out_t))))
	HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, HG_FAIL, "can't allocate output struct for view create");

    output->ret = ret_value;
    output->num_tokens = num_tokens;
    output->tokens = tokens;
    op_data->output = output;

    HG_Handler_start_output(op_data->hg_handle, output);

done:

    if(ret_value < 0)
        HG_Handler_start_output(op_data->hg_handle, &ret_value);

    input = (view_create_in_t *)H5MM_xfree(input);
    output = (view_create_out_t *)H5MM_xfree(output);
    op_data = (op_data_t *)H5MM_xfree(op_data);

    FUNC_LEAVE_NOAPI_VOID
} /* end H5VL_iod_server_view_create_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_get_view_tokens_cb
 *
 * Purpose:	Creates a view from a provided query.
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              September, 2013
 *
 *-------------------------------------------------------------------------
 */
void
H5VL_iod_server_get_view_tokens_cb(AXE_engine_t UNUSED axe_engine, 
                               size_t UNUSED num_n_parents, AXE_task_t UNUSED n_parents[], 
                               size_t UNUSED num_s_parents, AXE_task_t UNUSED s_parents[], 
                               void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    view_get_tokens_in_t *input = (view_get_tokens_in_t *)op_data->input;
    AXE_task_t view_create_id = input->view_create_id;
    hg_bulk_t bulk_handle = input->bulk_handle; /* bulk handle for data */
    hg_bulk_block_t bulk_block_handle; /* HG block handle */
    hg_bulk_request_t bulk_request; /* HG request */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT


    /* Create a new block handle to write the data */
    HG_Bulk_block_handle_create(buf, size, HG_BULK_READ_ONLY, &bulk_block_handle);

    /* Write bulk data here and wait for the data to be there  */
    if(HG_SUCCESS != HG_Bulk_write_all(dest, bulk_handle, bulk_block_handle, &bulk_request))
        HGOTO_ERROR(H5E_SYM, H5E_READERROR, FAIL, "can't read from array object");
    /* wait for it to complete */
    if(HG_SUCCESS != HG_Bulk_wait(bulk_request, HG_MAX_IDLE_TIME, HG_STATUS_IGNORE))
        HGOTO_ERROR(H5E_SYM, H5E_READERROR, FAIL, "can't read from array object");

    /* free block handle */
    if(HG_SUCCESS != HG_Bulk_block_handle_free(bulk_block_handle))
        HGOTO_ERROR(H5E_SYM, H5E_READERROR, FAIL, "can't free bds block handle");

    HG_Handler_start_output(op_data->hg_handle, output);

done:

    if(ret_value < 0)
        HG_Handler_start_output(op_data->hg_handle, &ret_value);

    input = (object_op_in_t *)H5MM_xfree(input);
    //op_data = (op_data_t *)H5MM_xfree(op_data);

    FUNC_LEAVE_NOAPI_VOID
} /* end H5VL_iod_server_get_view_tokens_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5VL__iod_server_construct_view
 *
 * Purpose: 
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 *-------------------------------------------------------------------------
 */
static herr_t 
H5VL__iod_server_construct_view(iod_handle_t coh, iod_obj_id_t loc_id, iod_trans_id_t rtid,
                                hid_t query, int *num_tokens, H5VL_token_t *tokens)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* get tokens for attributes that satisfy the query on the current
       location object */
    if(query_type & H5Q_TYPE_ATTR_NAME) {
        if(H5VL_iod_query_attribute(coh, rtid, loc_id, query, num_tokens, tokens) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to query dataset for selection");
    }

    if(loc_id & IOD_OBJ_TYPE_ARRAY) {
        /* get token for dataset and space selection for data
           elements that satisfy the query */
        if(H5VL_iod_query_selection(coh, rtid, loc_id, query, num_tokens, tokens) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to query dataset for selection");
    }

    else if(loc_id & IOD_OBJ_TYPE_KV) {
        /* get tokens for links that satisfy the query on the current
           location object. This will recursively call
           H5VL_iod_server_construct_view() on every object ID that
           has a link from the current location object. */
        if(query_type & H5Q_TYPE_LINK_NAME) {
            if(H5VL_iod_query_link(coh, rtid, loc_id, query, num_tokens, tokens) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to query dataset for selection");
        }
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__iod_server_construct_view() */


/*-------------------------------------------------------------------------
 * Function:	H5VL__iod_query_selection
 *
 * Purpose: 
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 *-------------------------------------------------------------------------
 */
static herr_t 
H5VL__iod_query_selection(iod_handle_t coh, iod_obj_id_t obj_id, 
                          iod_trans_id_t rtid, hid_t query, 
                          int *num_tokens, H5VL_token_t *tokens)
{
    iod_handle_t obj_oh = IOD_OH_UNDEFINED;
    hid_t layout_space = -1, queried_space = -1;
    size_t nelmts;
    size_t elmt_size;
    size_t buf_size=0, ds_size=0, dt_size=0;
    void *buf = NULL;
    scratch_pad sp;
    H5O_type_t otype;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* open the array object */
    if (iod_obj_open_read(coh, obj_id, NULL /*hints*/, &obj_oh, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't open current group");

    /* get scratch pad of the dataset */
    if(iod_obj_get_scratch(obj_oh, rtid, &sp, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "can't get scratch pad for object");

    /* open the metadata scratch pad */
    if (iod_obj_open_write(coh, sp[0], NULL /*hints*/, &mdkv_oh, NULL) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "can't open scratch pad");

    /* retrieve the datatype of array object */
    if(H5VL_iod_get_metadata(mdkv_oh, rtid, H5VL_IOD_DATATYPE, H5VL_IOD_KEY_OBJ_DATATYPE,
                             NULL, NULL, type_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "failed to retrieve datatype");

    /* close the metadata scratch pad */
    if(iod_obj_close(mdkv_oh, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close object");

    /* get layout dataspace from the query to read from */
    layout_space = H5Qget_space(query);

    nelmts = (size_t)H5Sget_select_npoints(layout_space);
    elmt_size = H5Tget_size(type_id);
    buf_size = nelmts * elmt_size;

    /* allocate buffer to hold data */
    if(NULL == (buf = malloc(buf_size)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate read buffer");

    /* read the data selection from IOD. */
    /* MSC - will need to do it in pieces, not it one shot. */
    if(H5VL__iod_server_final_io(coh, obj_oh, layout_space, type_id, 
                                 FALSE, buf, buf_size, 0, 0, rtid) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTREAD, FAIL, "can't read from array object");

    if(H5Qapply(query, buf, type_id, layout_space, H5P_DEFAULT, &queried_space) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't apply query on dataspace");

    if(H5Sencode(queried_space, NULL, &ds_size) < 0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTENCODE, FAIL, "can't encode dataspace");
    if(H5Tencode(type_id, NULL, &dt_size) < 0)
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTENCODE, FAIL, "can't encode datatype");

    token_size += sizeof(iod_obj_id_t)*3 + sizeof(H5O_type_t) +
        dt_size + ds_size + sizeof(size_t)*2;

    u = *num_tokens;
    tokens[u].token_size = token_size;

    if(NULL == (tokens[u].token = malloc(token_size)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate token buffer");

    buf_ptr = (uint8_t *)tokens[u].token;

    HDmemcpy(buf_ptr, &obj_id, sizeof(iod_obj_id_t));
    buf_ptr += sizeof(iod_obj_id_t);
    HDmemcpy(buf_ptr, &sp[0], sizeof(iod_obj_id_t));
    buf_ptr += sizeof(iod_obj_id_t);
    HDmemcpy(buf_ptr, &sp[1], sizeof(iod_obj_id_t));
    buf_ptr += sizeof(iod_obj_id_t);
    otype = H5O_TYPE_DATASET;
    HDmemcpy(buf_ptr, &otype, sizeof(H5O_type_t));
    buf_ptr += sizeof(H5O_type_t);

    HDmemcpy(buf_ptr, &dt_size, sizeof(size_t));
    buf_ptr += sizeof(size_t);
    if(H5Tencode(type_id, buf_ptr, &dt_size) < 0)
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTENCODE, FAIL, "can't encode datatype");
    buf_ptr += dt_size;

    HDmemcpy(buf_ptr, &ds_size, sizeof(size_t));
    buf_ptr += sizeof(size_t);
    if(H5Sencode(queried_space, buf_ptr, &ds_size) < 0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTENCODE, FAIL, "can't encode dataspace");
    buf_ptr += space_size;

    *num_tokens ++;

done:

    if(layout_space)
        H5Sclose(layout_space);
    if(queried_space)
        H5Sclose(queried_space);

    if(buf)
        free(buf);

    if(obj_oh != IOD_OH_UNDEFINED && iod_obj_close(obj_oh, NULL, NULL) < 0)
        HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close Array object");

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__iod_query_selection() */
#endif

#endif /* H5_HAVE_EFF */
