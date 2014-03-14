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
#ifdef H5_HAVE_INDEXING

/*
 * Programmer:  Mohamad Chaarawi <chaarawi@hdfgroup.gov>
 *              March, 2014
 *
 * Purpose:	The IOD plugin server side indexing routines.
 */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_dset_set_index_info_cb
 *
 * Purpose:	Stored index information of the dataset.
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              March, 2014
 *
 *-------------------------------------------------------------------------
 */
void
H5VL_iod_server_dset_set_index_info_cb(AXE_engine_t UNUSED axe_engine, 
                               size_t UNUSED num_n_parents, AXE_task_t UNUSED n_parents[], 
                               size_t UNUSED num_s_parents, AXE_task_t UNUSED s_parents[], 
                               void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    dset_set_index_info_in_t *input = (dset_set_index_info_in_t *)op_data->input;
    iod_handle_t coh = input->coh; /* container handle */
    iod_obj_id_t mdkv_id = input->mdkv_id; /* The ID of the metadata KV to be created */
    iod_trans_id_t wtid = input->trans_num;
    uint32_t cs_scope = input->cs_scope;
    iod_handle_t mdkv_oh;
    iod_kv_t kv;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

#if H5VL_IOD_DEBUG 
    fprintf(stderr, "Start dataset set_index_info\n");
#endif

    /* Open Metadata KV object for write */
    if (iod_obj_open_write(coh, mdkv_id, wtid, NULL, &mdkv_oh, NULL) < 0)
        HGOTO_ERROR2(H5E_SYM, H5E_CANTINIT, FAIL, "can't open MDKV object");

    kv.key = H5VL_IOD_IDX_PLUGIN_ID;
    kv.key_len = (iod_size_t)strlen(H5VL_IOD_IDX_PLUGIN_ID);
    kv.value = &input->idx_plugin_id;
    kv.value_len = (iod_size_t)sizeof(uint32_t);

    if(cs_scope & H5_CHECKSUM_IOD) {
        iod_checksum_t cs[2];

        cs[0] = H5_checksum_crc64(kv.key, kv.key_len);
        cs[1] = H5_checksum_crc64(kv.value, kv.value_len);
        if (iod_kv_set(mdkv_oh, wtid, NULL, &kv, cs, NULL) < 0)
            HGOTO_ERROR2(H5E_SYM, H5E_CANTINIT, FAIL, "can't set KV pair in parent");
    }
    else {
        if (iod_kv_set(mdkv_oh, wtid, NULL, &kv, NULL, NULL) < 0)
            HGOTO_ERROR2(H5E_SYM, H5E_CANTINIT, FAIL, "can't set KV pair in parent");
    }

    kv.key = H5VL_IOD_IDX_PLUGIN_MD;
    kv.key_len = (iod_size_t)strlen(H5VL_IOD_IDX_PLUGIN_MD);
    kv.value = input->idx_metadata.buf;
    kv.value_len = (iod_size_t)input->idx_metadata.buf_size;

    if(cs_scope & H5_CHECKSUM_IOD) {
        iod_checksum_t cs[2];

        cs[0] = H5_checksum_crc64(kv.key, kv.key_len);
        cs[1] = H5_checksum_crc64(kv.value, kv.value_len);
        if (iod_kv_set(mdkv_oh, wtid, NULL, &kv, cs, NULL) < 0)
            HGOTO_ERROR2(H5E_SYM, H5E_CANTINIT, FAIL, "can't set KV pair in parent");
    }
    else {
        if (iod_kv_set(mdkv_oh, wtid, NULL, &kv, NULL, NULL) < 0)
            HGOTO_ERROR2(H5E_SYM, H5E_CANTINIT, FAIL, "can't set KV pair in parent");
    }

done:
    if(HG_SUCCESS != HG_Handler_start_output(op_data->hg_handle, &ret_value))
        HDONE_ERROR(H5E_SYM, H5E_WRITEERROR, FAIL, "can't send result of write to client");

    /* close the Metadata KV object */
    if(iod_obj_close(mdkv_oh, NULL, NULL) < 0)
        HGOTO_ERROR2(H5E_SYM, H5E_CANTINIT, FAIL, "can't close object");

    input = (dset_set_index_info_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

    FUNC_LEAVE_NOAPI_VOID
} /* end H5VL_iod_server_dset_set_index_info_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_dset_get_index_info_cb
 *
 * Purpose:	Stored index information of the dataset.
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              March, 2014
 *
 *-------------------------------------------------------------------------
 */
void
H5VL_iod_server_dset_get_index_info_cb(AXE_engine_t UNUSED axe_engine, 
                               size_t UNUSED num_n_parents, AXE_task_t UNUSED n_parents[], 
                               size_t UNUSED num_s_parents, AXE_task_t UNUSED s_parents[], 
                               void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    dset_get_index_info_in_t *input = (dset_get_index_info_in_t *)op_data->input;
    dset_get_index_info_out_t output;
    iod_handle_t coh = input->coh; /* container handle */
    iod_obj_id_t mdkv_id = input->mdkv_id; /* The ID of the metadata KV to be created */
    iod_trans_id_t rtid = input->rcxt_num;
    uint32_t cs_scope = input->cs_scope;
    iod_handle_t mdkv_oh;
    iod_size_t key_size = 0;
    iod_size_t val_size = 0;
    iod_checksum_t *iod_cs = NULL;
    char *key = NULL;
    iod_ret_t ret;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

#if H5VL_IOD_DEBUG 
    fprintf(stderr, "Start dataset get_index_info\n");
#endif

    /* Open Metadata KV object for write */
    if (iod_obj_open_read(coh, mdkv_id, rtid, NULL, &mdkv_oh, NULL) < 0)
        HGOTO_ERROR2(H5E_SYM, H5E_CANTINIT, FAIL, "can't open MDKV object");

    if(cs_scope & H5_CHECKSUM_IOD) {
        iod_cs = (iod_checksum_t *)malloc(sizeof(iod_checksum_t) * 2);
    }

    key = H5VL_IOD_IDX_PLUGIN_ID;
    key_size = strlen(key);
    val_size = sizeof(uint32_t);

    if((ret = iod_kv_get_value(mdkv_oh, rtid, key, key_size, &output.idx_plugin_id, 
                               &val_size, iod_cs, NULL)) < 0) {
        if(ret == -ENOKEY) {
            fprintf(stderr, "no index to retrieve\n");

            output.ret = ret_value;
            output.idx_count = 0;
            output.idx_plugin_id = 0;
            output.idx_metadata.buf_size = 0;
            output.idx_metadata.buf = NULL;
            HG_Handler_start_output(op_data->hg_handle, &output);
            HGOTO_DONE(SUCCEED);
        }
        fprintf(stderr, "%d (%s).\n", ret, strerror(-ret));
        HGOTO_ERROR2(H5E_SYM, H5E_CANTINIT, FAIL, "lookup failed");
    }
    if(cs_scope & H5_CHECKSUM_IOD) {
        iod_checksum_t cs[2];

        cs[0] = H5_checksum_crc64(key, key_size);
        cs[1] = H5_checksum_crc64(&output.idx_plugin_id, val_size);

        if(iod_cs[0] != cs[0] && iod_cs[1] != cs[1])
            HGOTO_ERROR2(H5E_SYM, H5E_CANTINIT, FAIL, "Corruption detected when reading metadata from IOD");
    }


    key = H5VL_IOD_IDX_PLUGIN_MD;
    key_size = strlen(key);
    val_size = 0;

    if(iod_kv_get_value(mdkv_oh, rtid, key, key_size, NULL, 
                        &val_size, iod_cs, NULL) < 0)
        HGOTO_ERROR2(H5E_SYM, H5E_CANTINIT, FAIL, "lookup failed");

    output.idx_metadata.buf_size = val_size;
    output.idx_metadata.buf = malloc(val_size);

    if(iod_kv_get_value(mdkv_oh, rtid, key, key_size, (char *)output.idx_metadata.buf, 
                        &val_size, iod_cs, NULL) < 0)
        HGOTO_ERROR2(H5E_SYM, H5E_CANTINIT, FAIL, "lookup failed");

    if(cs_scope & H5_CHECKSUM_IOD) {
        iod_checksum_t cs[2];

        cs[0] = H5_checksum_crc64(key, key_size);
        cs[1] = H5_checksum_crc64(output.idx_metadata.buf, val_size);

        if(iod_cs[0] != cs[0] && iod_cs[1] != cs[1])
            HGOTO_ERROR2(H5E_SYM, H5E_CANTINIT, FAIL, "Corruption detected when reading metadata from IOD");
    }

    output.ret = ret_value;
    /* MSC for now, idx_count is always 1 */
    output.idx_count = 1;
    HG_Handler_start_output(op_data->hg_handle, &output);

done:
    if(ret_value < 0) {
        fprintf(stderr, "INDEX get info FAILED\n");
        output.ret = ret_value;
        output.idx_plugin_id = 0;
        output.idx_metadata.buf_size = 0;
        if(output.idx_metadata.buf)
            free(output.idx_metadata.buf);
        output.idx_metadata.buf = NULL;
        HG_Handler_start_output(op_data->hg_handle, &output);
    }

    /* close the Metadata KV object */
    if(iod_obj_close(mdkv_oh, NULL, NULL) < 0)
        HGOTO_ERROR2(H5E_SYM, H5E_CANTINIT, FAIL, "can't close object");

    input = (dset_get_index_info_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

    FUNC_LEAVE_NOAPI_VOID
} /* end H5VL_iod_server_dset_get_index_info_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_dset_remove_index_info_cb
 *
 * Purpose:	Stored index information of the dataset.
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              March, 2014
 *
 *-------------------------------------------------------------------------
 */
void
H5VL_iod_server_dset_remove_index_info_cb(AXE_engine_t UNUSED axe_engine, 
                               size_t UNUSED num_n_parents, AXE_task_t UNUSED n_parents[], 
                               size_t UNUSED num_s_parents, AXE_task_t UNUSED s_parents[], 
                               void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    dset_rm_index_info_in_t *input = (dset_rm_index_info_in_t *)op_data->input;
    iod_handle_t coh = input->coh; /* container handle */
    iod_obj_id_t mdkv_id = input->mdkv_id; /* The ID of the metadata KV to be created */
    iod_trans_id_t wtid = input->trans_num;
    //uint32_t cs_scope = input->cs_scope;
    iod_handle_t mdkv_oh;
    iod_kv_params_t kvs;
    iod_kv_t kv;
    iod_ret_t ret;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

#if H5VL_IOD_DEBUG 
    fprintf(stderr, "Start dataset rm_index_info\n");
#endif

    /* Open Metadata KV object for write */
    if (iod_obj_open_write(coh, mdkv_id, wtid, NULL, &mdkv_oh, NULL) < 0)
        HGOTO_ERROR2(H5E_SYM, H5E_CANTINIT, FAIL, "can't open MDKV object");

    kv.key = H5VL_IOD_IDX_PLUGIN_ID;
    kv.key_len = (iod_size_t)strlen(H5VL_IOD_IDX_PLUGIN_ID);
    kvs.kv = &kv;
    kvs.cs = NULL;
    kvs.ret = &ret;
    if(iod_kv_unlink_keys(mdkv_oh, wtid, NULL, 1, &kvs, NULL) < 0)
        HGOTO_ERROR2(H5E_SYM, H5E_CANTDEC, FAIL, "Unable to unlink KV pair");

    kv.key = H5VL_IOD_IDX_PLUGIN_MD;
    kv.key_len = (iod_size_t)strlen(H5VL_IOD_IDX_PLUGIN_MD);
    kvs.kv = &kv;
    kvs.cs = NULL;
    kvs.ret = &ret;
    if(iod_kv_unlink_keys(mdkv_oh, wtid, NULL, 1, &kvs, NULL) < 0)
        HGOTO_ERROR2(H5E_SYM, H5E_CANTDEC, FAIL, "Unable to unlink KV pair");

done:
    if(HG_SUCCESS != HG_Handler_start_output(op_data->hg_handle, &ret_value))
        HDONE_ERROR(H5E_SYM, H5E_WRITEERROR, FAIL, "can't send result of write to client");

    /* close the Metadata KV object */
    if(iod_obj_close(mdkv_oh, NULL, NULL) < 0)
        HGOTO_ERROR2(H5E_SYM, H5E_CANTINIT, FAIL, "can't close object");

    input = (dset_rm_index_info_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

    FUNC_LEAVE_NOAPI_VOID
} /* end H5VL_iod_server_dset_remove_index_info_cb() */

#endif /* H5_HAVE_INDEXING */
#endif /* H5_HAVE_EFF */
