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
H5VL_iod_server_object_open_by_token_cb(AXE_engine_t UNUSED axe_engine, 
                                        size_t UNUSED num_n_parents, AXE_task_t UNUSED n_parents[], 
                                        size_t UNUSED num_s_parents, AXE_task_t UNUSED s_parents[], 
                                        void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    object_token_in_t *input = (object_token_in_t *)op_data->input;
    iod_handle_t coh = input->coh; /* the container handle */
    iod_obj_id_t obj_id = input->iod_id; /* The ID of the object */
    iod_trans_id_t tid = input->trans_num;
    //uint32_t cs_scope = input->cs_scope;
    iod_handles_t obj_oh; /* The handle for object */
    iod_ret_t ret;
    herr_t ret_value = SUCCEED;

#if H5_EFF_DEBUG
    fprintf(stderr, "Start Object Open by token = %"PRIx64"\n", obj_id);
#endif

    ret = iod_obj_open_read(coh, obj_id, tid, NULL, &obj_oh.rd_oh, NULL);
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "can't open current group");
    ret = iod_obj_open_write(coh, obj_id, tid, NULL, &obj_oh.wr_oh, NULL);
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "can't open current group");

#if H5_EFF_DEBUG
    fprintf(stderr, "Done with object open by token, sending response to client\n");
#endif

    HG_Handler_start_output(op_data->hg_handle, &obj_oh);

done:
    if(ret_value < 0) {
        obj_oh.rd_oh.cookie = IOD_OH_UNDEFINED;
        obj_oh.wr_oh.cookie = IOD_OH_UNDEFINED;
        HG_Handler_start_output(op_data->hg_handle, &obj_oh);
    }

    input = (object_token_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

} /* end H5VL_iod_server_object_open_cb() */


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
    iod_trans_id_t rtid = input->rcxt_num;
    uint32_t cs_scope = input->cs_scope;
    iod_handles_t obj_oh; /* The handle for object */
    iod_obj_id_t obj_id; /* The ID of the object */
    iod_handle_t mdkv_oh;
    scratch_pad sp;
    iod_checksum_t sp_cs = 0;
    iod_ret_t ret;
    herr_t ret_value = SUCCEED;

#if H5_EFF_DEBUG
    fprintf(stderr, "Start Object Open on %s (OH %"PRIu64" ID %"PRIx64")\n", 
            input->loc_name, input->loc_oh.rd_oh.cookie, input->loc_id);
#endif

    /* Traverse Path and open object */
    ret = H5VL_iod_server_open_path(coh, input->loc_id, input->loc_oh, input->loc_name, 
                                    rtid, cs_scope, &obj_id, &obj_oh);
    if(ret != SUCCEED)
        HGOTO_ERROR_FF(ret, "can't open object");

    ret = iod_obj_open_write(coh, obj_id, rtid, NULL, &obj_oh.wr_oh, NULL);
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "can't open current group");

    if(obj_id != input->loc_id) {
        /* get scratch pad of the object */
        ret = iod_obj_get_scratch(obj_oh.rd_oh, rtid, &sp, &sp_cs, NULL);
        if(ret < 0)
            HGOTO_ERROR_FF(ret, "can't get scratch pad for object");

        if(sp_cs && (cs_scope & H5_CHECKSUM_IOD)) {
            /* verify scratch pad integrity */
            if(H5VL_iod_verify_scratch_pad(&sp, sp_cs) < 0)
                HGOTO_ERROR_FF(FAIL, "Scratch Pad failed integrity check");
        }

        /* open the metadata KV */
        ret = iod_obj_open_read(coh, sp[0], rtid, NULL, &mdkv_oh, NULL);
        if(ret < 0)
            HGOTO_ERROR_FF(ret, "can't open MDKV");
    }
    else {
        /* open the metadata KV */
        ret = iod_obj_open_read(coh, input->loc_mdkv_id, rtid, NULL, &mdkv_oh, NULL);
        if(ret < 0)
            HGOTO_ERROR_FF(ret, "can't open MDKV");
    }

    ret = H5VL_iod_get_metadata(mdkv_oh, rtid, H5VL_IOD_OBJECT_TYPE, H5VL_IOD_KEY_OBJ_TYPE,
                                cs_scope, NULL, &output.obj_type);
    if(ret != SUCCEED)
        HGOTO_ERROR_FF(ret, "failed to retrieve link count");

    switch(output.obj_type) {
    case H5I_MAP:
        ret = H5VL_iod_get_metadata(mdkv_oh, rtid, H5VL_IOD_PLIST, H5VL_IOD_KEY_OBJ_CPL,
                                    cs_scope, NULL, &output.cpl_id);
        if(ret != SUCCEED)
            HGOTO_ERROR_FF(ret, "failed to retrieve mcpl");

        ret = H5VL_iod_get_metadata(mdkv_oh, rtid, H5VL_IOD_DATATYPE, 
                                    H5VL_IOD_KEY_MAP_KEY_TYPE,
                                    cs_scope, NULL, &output.id1);
        if(ret != SUCCEED)
            HGOTO_ERROR_FF(ret, "failed to retrieve link count");

        ret = H5VL_iod_get_metadata(mdkv_oh, rtid, H5VL_IOD_DATATYPE, 
                                    H5VL_IOD_KEY_MAP_VALUE_TYPE,
                                    cs_scope, NULL, &output.id2);
        if(ret != SUCCEED)
            HGOTO_ERROR_FF(ret, "failed to retrieve link count");
        break;
    case H5I_GROUP:
        ret = H5VL_iod_get_metadata(mdkv_oh, rtid, H5VL_IOD_PLIST, H5VL_IOD_KEY_OBJ_CPL,
                                    cs_scope, NULL, &output.cpl_id);
        if(ret != SUCCEED)
            HGOTO_ERROR_FF(ret, "failed to retrieve dcpl");
        output.id1 = FAIL;
        output.id2 = FAIL;
        break;
    case H5I_DATASET:
        ret = H5VL_iod_get_metadata(mdkv_oh, rtid, H5VL_IOD_PLIST, H5VL_IOD_KEY_OBJ_CPL,
                                    cs_scope, NULL, &output.cpl_id);
        if(ret != SUCCEED)
            HGOTO_ERROR_FF(ret, "failed to retrieve dcpl");

        ret = H5VL_iod_get_metadata(mdkv_oh, rtid, H5VL_IOD_DATATYPE, H5VL_IOD_KEY_OBJ_DATATYPE,
                                    cs_scope, NULL, &output.id1);
        if(ret != SUCCEED)
            HGOTO_ERROR_FF(ret, "failed to retrieve datatype");

        ret = H5VL_iod_get_metadata(mdkv_oh, rtid, H5VL_IOD_DATASPACE, H5VL_IOD_KEY_OBJ_DATASPACE,
                                    cs_scope, NULL, &output.id2);
        if(ret != SUCCEED)
            HGOTO_ERROR_FF(ret, "failed to retrieve dataspace");
        break;
    case H5I_DATATYPE:
        {
            size_t buf_size; /* size of serialized datatype */
            void *buf = NULL;
            iod_mem_desc_t *mem_desc = NULL; /* memory descriptor used for reading */
            iod_blob_iodesc_t *file_desc = NULL; /* file descriptor used to write */
            iod_checksum_t dt_cs = 0, blob_cs = 0;
            iod_size_t key_size, val_size;
            iod_checksum_t iod_cs[2];

            key_size = 1 + strlen(H5VL_IOD_KEY_DTYPE_SIZE);
            val_size = sizeof(iod_size_t);

            ret = H5VL_iod_get_metadata(mdkv_oh, rtid, H5VL_IOD_PLIST, H5VL_IOD_KEY_OBJ_CPL,
                                        cs_scope, NULL, &output.cpl_id);
            if(ret != SUCCEED)
                HGOTO_ERROR_FF(ret, "failed to retrieve dcpl");

            /* retrieve blob size metadata from scratch pad */
            ret = iod_kv_get_value(mdkv_oh, rtid, H5VL_IOD_KEY_DTYPE_SIZE, key_size,
                                  &buf_size, &val_size, iod_cs, NULL);
            if(ret < 0)
                HGOTO_ERROR_FF(ret, "datatype size lookup failed");

            if(cs_scope & H5_CHECKSUM_IOD) {
                if(H5VL_iod_verify_kv_pair(H5VL_IOD_KEY_DTYPE_SIZE, key_size, 
                                           &buf_size, val_size, iod_cs) < 0)
                    HGOTO_ERROR_FF(FAIL, "Corruption detected when reading metadata from IOD");
            }

            if(NULL == (buf = malloc(buf_size)))
                HGOTO_ERROR_FF(FAIL, "can't allocate BLOB read buffer");

            /* create memory descriptor for writing */
            mem_desc = (iod_mem_desc_t *)malloc(sizeof(iod_mem_desc_t) + 
                                                sizeof(iod_mem_frag_t));
            mem_desc->nfrag = 1;
            mem_desc->frag[0].addr = buf;
            mem_desc->frag[0].len = (iod_size_t)buf_size;

            /* create file descriptor for writing */
            file_desc = (iod_blob_iodesc_t *)malloc(sizeof(iod_blob_iodesc_t) + 
                                                    sizeof(iod_blob_iofrag_t));
            file_desc->nfrag = 1;
            file_desc->frag[0].offset = 0;
            file_desc->frag[0].len = (iod_size_t)buf_size;

            /* read the serialized type value from the BLOB object */
            ret = iod_blob_read(obj_oh.rd_oh, rtid, NULL, mem_desc, file_desc, 
                                &blob_cs, NULL);
            if(ret < 0)
                HGOTO_ERROR_FF(ret, "unable to read BLOB object");

            if(blob_cs && (cs_scope & H5_CHECKSUM_IOD)) {
                /* calculate a checksum for the datatype */
                dt_cs = H5_checksum_crc64(buf, buf_size);

                /* Verify checksum against one given by IOD */
                if(blob_cs != dt_cs)
                    HGOTO_ERROR_FF(FAIL, "Data Corruption detected when reading datatype");
            }

            /* decode the datatype */
            if((output.id1 = H5Tdecode(buf)) < 0)
                HGOTO_ERROR_FF(FAIL, "unable to decode datatype");
            output.id2 = FAIL;

            free(mem_desc);
            free(file_desc);
            free(buf);
            break;
        }
    default:
        HGOTO_ERROR_FF(FAIL, "Invalid object type");
    }

    /* close the metadata scratch pad */
    ret = iod_obj_close(mdkv_oh, NULL, NULL);
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "can't close object");

    output.iod_id = obj_id;
    output.mdkv_id = sp[0];
    output.attrkv_id = sp[1];
    output.iod_oh.rd_oh.cookie = obj_oh.rd_oh.cookie;
    output.iod_oh.wr_oh.cookie = obj_oh.wr_oh.cookie;

#if H5_EFF_DEBUG
    fprintf(stderr, "Done with object open, sending response to client\n");
#endif

    HG_Handler_start_output(op_data->hg_handle, &output);

done:
    if(ret_value < 0) {
        output.iod_oh.rd_oh.cookie = IOD_OH_UNDEFINED;
        output.iod_oh.wr_oh.cookie = IOD_OH_UNDEFINED;
        output.iod_id = IOD_OBJ_INVALID;
        output.cpl_id = FAIL;
        output.id1 = FAIL;
        output.id2 = FAIL;
        HG_Handler_start_output(op_data->hg_handle, &output);
    }

    input = (object_op_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

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
    iod_trans_id_t wtid = input->trans_num;
    iod_trans_id_t rtid = input->rcxt_num;
    uint32_t cs_scope = input->cs_scope;
    iod_handles_t dst_oh; /* The handle for the dst object where link is created*/
    iod_obj_id_t dst_id; /* The ID of the dst object where link is created*/
    iod_obj_id_t obj_id; /* The ID of the object to be moved/copied */
    iod_handles_t obj_oh; /* The handle for the object to be moved/copied */
    iod_obj_id_t new_id; /* The ID of the new object */
    iod_handle_t mdkv_oh;/* The handle of the metadata KV for source object */
    char *new_name = NULL;
    iod_kv_t kv;
    iod_size_t kv_size = sizeof(H5VL_iod_link_t);
    H5VL_iod_link_t iod_link;
    scratch_pad sp;
    iod_checksum_t sp_cs = 0;
    herr_t ret_value = SUCCEED;

#if H5_EFF_DEBUG
    fprintf(stderr, "Start object copy\n");
#endif

    /* MSC - NEED IOD & a lot more work*/
#if 0

    /* Traverse Path and open object */
    if(H5VL_iod_server_open_path(coh, input->src_loc_id, input->src_loc_oh, input->src_loc_name, 
                                 rtid, cs_scope, &obj_id, &obj_oh) < 0)
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "can't open object");

    /* the traversal will retrieve the location where the objects
       needs to be copied to. The traversal will fail if an
       intermediate group does not exist. */
    if(H5VL_iod_server_traverse(coh, input->dst_loc_id, input->dst_loc_oh, input->dst_loc_name, 
                                FALSE, rtid, &new_name, &dst_id, &dst_oh) < 0)
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "can't traverse path");

    /* get scratch pad of the object */
    if(iod_obj_get_scratch(obj_oh, rtid, &sp, &sp_cs, NULL) < 0)
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "can't get scratch pad for object");

    if(sp_cs && (cs_scope & H5_CHECKSUM_IOD)) {
        /* verify scratch pad integrity */
        if(H5VL_iod_verify_scratch_pad(&sp, sp_cs) < 0)
            HGOTO_ERROR_FF(FAIL, "Scratch Pad failed integrity check");
    }

    /* open the metadata scratch pad */
    if (iod_obj_open_write(coh, sp[0], rtid, NULL /*hints*/, &mdkv_oh, NULL) < 0)
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "can't open MDKV");

    if(H5VL_iod_get_metadata(mdkv_oh, rtid, H5VL_IOD_OBJECT_TYPE, H5VL_IOD_KEY_OBJ_TYPE,
                             cs_scope, NULL, &obj_type) < 0)
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "failed to retrieve link count");

    switch(obj_type) {
    case H5I_MAP:
        if(H5VL_iod_get_metadata(mdkv_oh, rtid, H5VL_IOD_PLIST, H5VL_IOD_KEY_OBJ_CPL,
                                 cs_scope, NULL, &output.cpl_id) < 0)
        if(ret < 0)
        HGOTO_ERROR_FF(ret, "failed to retrieve mcpl");
        break;
    case H5I_GROUP:
        if(H5VL_iod_get_metadata(mdkv_oh, rtid, H5VL_IOD_PLIST, H5VL_IOD_KEY_OBJ_CPL,
                                 cs_scope, NULL, &output.cpl_id) < 0)
        if(ret < 0)
        HGOTO_ERROR_FF(ret, "failed to retrieve gcpl");
        break;
    case H5I_DATASET:
        if(H5VL_iod_get_metadata(mdkv_oh, rtid, H5VL_IOD_PLIST, H5VL_IOD_KEY_OBJ_CPL,
                                 cs_scope, NULL, &output.cpl_id) < 0)
        if(ret < 0)
        HGOTO_ERROR_FF(ret, "failed to retrieve dcpl");

        if(H5VL_iod_get_metadata(mdkv_oh, rtid, H5VL_IOD_DATATYPE, H5VL_IOD_KEY_OBJ_DATATYPE,
                                 cs_scope, NULL, &output.type_id) < 0)
        if(ret < 0)
        HGOTO_ERROR_FF(ret, "failed to retrieve datatype");

        if(H5VL_iod_get_metadata(mdkv_oh, rtid, H5VL_IOD_DATASPACE, H5VL_IOD_KEY_OBJ_DATASPACE,
                                 cs_scope, NULL, &output.space_id) < 0)
        if(ret < 0)
        HGOTO_ERROR_FF(ret, "failed to retrieve dataspace");
        break;
    case H5I_DATATYPE:
        {
            size_t buf_size; /* size of serialized datatype */
            void *buf = NULL;
            iod_mem_desc_t mem_desc; /* memory descriptor used for reading */
            iod_blob_iodesc_t file_desc; /* file descriptor used to write */
            iod_checksum_t dt_cs = 0, blob_cs = 0;
            iod_checksum_t iod_cs[2];

            /* retrieve blob size metadata from scratch pad */
            if(iod_kv_get_value(mdkv_oh, rtid, H5VL_IOD_KEY_DTYPE_SIZE, key_size,
                                &buf_size, &val_size, iod_cs, NULL) < 0)
            if(ret < 0)
        HGOTO_ERROR_FF(ret, "datatype size lookup failed");

            if(cs_scope & H5_CHECKSUM_IOD) {
                if(H5VL_iod_verify_kv_pair(H5VL_IOD_KEY_DTYPE_SIZE, key_size, 
                                           &buf_size, val_size, iod_cs) < 0)
                    HGOTO_ERROR_FF(FAIL, "Corruption detected when reading metadata from IOD");
            }

            if(NULL == (buf = malloc(buf_size)))
                HGOTO_ERROR_FF(FAIL, "can't allocate BLOB read buffer");

            /* create memory descriptor for reading */
            mem_desc.nfrag = 1;
            mem_desc.frag->addr = buf;
            mem_desc.frag->len = (iod_size_t)buf_size;

            /* create file descriptor for writing */
            file_desc.nfrag = 1;
            file_desc.frag->offset = 0;
            file_desc.frag->len = (iod_size_t)buf_size;

            /* read the serialized type value from the BLOB object */
            if(iod_blob_read(obj_oh, rtid, NULL, &mem_desc, &file_desc, NULL,
                             &blob_cs, NULL) < 0)
            if(ret < 0)
        HGOTO_ERROR_FF(ret, "unable to write BLOB object");

            if(blob_cs && (cs_scope & H5_CHECKSUM_IOD)) {
                /* calculate a checksum for the datatype */
                dt_cs = H5_checksum_crc64(buf, buf_size);

                /* Verify checksum against one given by IOD */
                if(blob_cs != dt_cs)
                    HGOTO_ERROR_FF(FAIL, "Data Corruption detected when reading datatype");
            }

            /* decode the datatype */
            if((output.type_id = H5Tdecode(buf)) < 0)
                HGOTO_ERROR_FF(FAIL, "unable to decode datatype");

            free(buf);
        }
    default:
        HGOTO_ERROR_FF(FAIL, "Invalid object type");
    }

    /* create new object as a copy of the source object */
    /* MSC - wait to see if IOD will have an object copy */

    /* close the metadata scratch pad */
    if(iod_obj_close(mdkv_oh, NULL, NULL) < 0)
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "can't close object");
    /* close the object handle */
    if(input->src_loc_oh.cookie != obj_oh.cookie && 
       iod_obj_close(obj_oh, NULL, NULL) < 0)
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "can't close object");

    /* Insert object in the destination path */
    if(H5VL_iod_insert_new_link(dst_oh, wtid, new_name, 
                                H5L_TYPE_HARD, &obj_id, cs_scope, NULL, NULL) < 0)
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "can't insert KV value");

    /* close dst group if it is not the location we started the
       traversal into */
    if(input->dst_loc_oh.cookie != dst_oh.cookie) {
        iod_obj_close(dst_oh, NULL, NULL);
    }

#endif

done:
#if H5_EFF_DEBUG
    fprintf(stderr, "Done with object Copy, sending response to client\n");
#endif

    HG_Handler_start_output(op_data->hg_handle, &ret_value);

    if(new_name)
        free(new_name);
    input = (object_copy_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

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
    iod_handles_t loc_oh = input->loc_oh;
    iod_obj_id_t loc_id = input->loc_id;
    iod_trans_id_t rtid = input->rcxt_num;
    uint32_t cs_scope = input->cs_scope;
    iod_handles_t obj_oh;
    iod_obj_id_t obj_id;
    const char *loc_name = input->loc_name;
    htri_t ret = -1;
    iod_ret_t iod_ret;
    herr_t ret_value = SUCCEED;

#if H5_EFF_DEBUG
    fprintf(stderr, "Start Object Exists on %s (OH %"PRIu64" ID %"PRIx64")\n", 
            input->loc_name, input->loc_oh.rd_oh.cookie, input->loc_id);
#endif

    /* Traverse Path and open object */
    if(H5VL_iod_server_open_path(coh, loc_id, loc_oh, loc_name, rtid, 
                                 cs_scope, &obj_id, &obj_oh) < 0) {
        ret = FALSE;
        HGOTO_DONE(SUCCEED);
    }
    else {
        /* close the object */
        if(loc_oh.rd_oh.cookie != obj_oh.rd_oh.cookie && 
           iod_obj_close(obj_oh.rd_oh, NULL, NULL) < 0)
            HGOTO_ERROR_FF(FAIL, "can't close object");

        /* set return to TRUE */
        ret = TRUE;
    }

done:

#if H5_EFF_DEBUG
    fprintf(stderr, "Done with Object exists, sending response to client\n");
#endif

    HG_Handler_start_output(op_data->hg_handle, &ret);

    input = (object_op_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

} /* end H5VL_iod_server_object_exists_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_object_get_info_cb
 *
 * Purpose:	Checks if an object get_info.
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
H5VL_iod_server_object_get_info_cb(AXE_engine_t UNUSED axe_engine, 
                                 size_t UNUSED num_n_parents, AXE_task_t UNUSED n_parents[], 
                                 size_t UNUSED num_s_parents, AXE_task_t UNUSED s_parents[], 
                                 void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    object_op_in_t *input = (object_op_in_t *)op_data->input;
    H5O_ff_info_t oinfo;
    iod_handle_t coh = input->coh;
    iod_handles_t loc_oh = input->loc_oh;
    iod_obj_id_t loc_id = input->loc_id;
    iod_trans_id_t rtid = input->rcxt_num;
    uint32_t cs_scope = input->cs_scope;
    iod_handles_t obj_oh;
    iod_handle_t mdkv_oh, attrkv_oh;
    iod_obj_id_t obj_id;
    scratch_pad sp;
    iod_checksum_t sp_cs = 0;
    H5I_type_t obj_type;
    int num_attrs = 0;
    const char *loc_name = input->loc_name;
    uint64_t link_count = 0;
    iod_ret_t ret;
    herr_t ret_value = SUCCEED;

#if H5_EFF_DEBUG
    fprintf(stderr, "Start Object Get Info on %s (OH %"PRIu64" ID %"PRIx64")\n", 
            input->loc_name, input->loc_oh.rd_oh.cookie, input->loc_id);
#endif

    /* Traverse Path and open object */
    ret = H5VL_iod_server_open_path(coh, loc_id, loc_oh, loc_name, 
                                    rtid, cs_scope, &obj_id, &obj_oh);
    if(ret != SUCCEED)
        HGOTO_ERROR_FF(ret, "object does not exist");

    oinfo.addr = obj_id;

    if(obj_id != loc_id || input->loc_mdkv_id == IOD_OBJ_INVALID) {
        /* get scratch pad of the object */
        ret = iod_obj_get_scratch(obj_oh.rd_oh, rtid, &sp, &sp_cs, NULL);
        if(ret < 0)
            HGOTO_ERROR_FF(ret, "can't get scratch pad for object");

        if(sp_cs && (cs_scope & H5_CHECKSUM_IOD)) {
            /* verify scratch pad integrity */
            if(H5VL_iod_verify_scratch_pad(&sp, sp_cs) < 0)
                HGOTO_ERROR_FF(FAIL, "Scratch Pad failed integrity check");
        }

        /* open the metadata KV */
        ret = iod_obj_open_read(coh, sp[0], rtid, NULL, &mdkv_oh, NULL);
        if(ret < 0)
            HGOTO_ERROR_FF(ret, "can't open MDKV");
        /* open the attribute KV */
        ret = iod_obj_open_read(coh, sp[1], rtid, NULL, &attrkv_oh, NULL);
        if(ret < 0)
            HGOTO_ERROR_FF(ret, "can't open ATTRKV");
    }
    else {
        /* open the metadata KV */
        ret = iod_obj_open_read(coh, input->loc_mdkv_id, rtid, NULL, &mdkv_oh, NULL);
        if(ret < 0)
            HGOTO_ERROR_FF(ret, "can't open MDKV");
        /* open the attribute KV */
        ret = iod_obj_open_read(coh, input->loc_attrkv_id, rtid, NULL, &attrkv_oh, NULL);
        if(ret < 0)
            HGOTO_ERROR_FF(ret, "can't open MDKV");
    }

    ret = H5VL_iod_get_metadata(mdkv_oh, rtid, H5VL_IOD_OBJECT_TYPE, 
                                H5VL_IOD_KEY_OBJ_TYPE,
                                cs_scope, NULL, &obj_type);
    if(ret != SUCCEED)
        HGOTO_ERROR_FF(ret, "failed to retrieve object type");

    switch(obj_type) {
    case H5I_GROUP:
        oinfo.type = H5O_TYPE_GROUP;
        break;
    case H5I_DATASET:
        oinfo.type = H5O_TYPE_DATASET;
        break;
    case H5I_DATATYPE:
        oinfo.type = H5O_TYPE_NAMED_DATATYPE;
        break;
    case H5I_MAP:
        oinfo.type = H5O_TYPE_MAP;
        break;
    default:
        HGOTO_ERROR_FF(FAIL, 
                     "unsupported object type for H5Oget_info");
    }

    ret = H5VL_iod_get_metadata(mdkv_oh, rtid, H5VL_IOD_LINK_COUNT, 
                                H5VL_IOD_KEY_OBJ_LINK_COUNT,
                                cs_scope, NULL, &link_count);
    if(ret != SUCCEED)
        HGOTO_ERROR_FF(ret, "failed to retrieve link count");

    oinfo.rc = (unsigned) link_count;

    ret = iod_kv_get_num(attrkv_oh, rtid, &num_attrs, NULL);
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "failed to retrieve attribute count");

    oinfo.num_attrs = (hsize_t)num_attrs;

    /* close the metadata KV */
    ret = iod_obj_close(mdkv_oh, NULL, NULL);
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "can't close object");
    /* close the  attribute KV */
    ret = iod_obj_close(attrkv_oh, NULL, NULL);
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "can't close object");

    if(loc_oh.rd_oh.cookie != obj_oh.rd_oh.cookie && 
       iod_obj_close(obj_oh.rd_oh, NULL, NULL) < 0)
        HGOTO_ERROR_FF(FAIL, "can't close object");

#if H5_EFF_DEBUG
    fprintf(stderr, "Done with Object get_info, sending response to client\n");
#endif

    HG_Handler_start_output(op_data->hg_handle, &oinfo);

done:
    if(ret_value < 0) {
        oinfo.type = H5O_TYPE_UNKNOWN;
        oinfo.addr = IOD_OBJ_INVALID;
        oinfo.rc = 0;
        oinfo.num_attrs = 0;
        HG_Handler_start_output(op_data->hg_handle, &oinfo);
    }
        
    input = (object_op_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

} /* end H5VL_iod_server_object_get_info_cb() */


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
    iod_handles_t loc_oh = input->loc_oh;
    iod_obj_id_t loc_id = input->loc_id;
    iod_trans_id_t wtid = input->trans_num;
    iod_trans_id_t rtid = input->rcxt_num;
    uint32_t cs_scope = input->cs_scope;
    iod_handles_t obj_oh;
    iod_handle_t mdkv_oh;
    iod_obj_id_t obj_id;
    const char *loc_name = input->path;
    const char *comment = input->comment;
    scratch_pad sp;
    iod_checksum_t sp_cs = 0;
    iod_ret_t ret;
    herr_t ret_value = SUCCEED;

#if H5_EFF_DEBUG
    fprintf(stderr, "Start Object Set comment on %s (OH %"PRIu64" ID %"PRIx64")\n", 
            input->path, input->loc_oh.rd_oh.cookie, input->loc_id);
#endif

    /* Traverse Path and open object */
    ret = H5VL_iod_server_open_path(coh, loc_id, loc_oh, loc_name, rtid, 
                                    cs_scope, &obj_id, &obj_oh);
    if(ret != SUCCEED)
        HGOTO_ERROR_FF(ret, "can't open object");

    if(loc_id != obj_id || input->loc_mdkv_id == IOD_OBJ_INVALID) {
        /* get scratch pad of the object */
        ret = iod_obj_get_scratch(obj_oh.rd_oh, rtid, &sp, &sp_cs, NULL);
        if(ret < 0)
            HGOTO_ERROR_FF(ret, "can't get scratch pad for object");

        if(sp_cs && (cs_scope & H5_CHECKSUM_IOD)) {
            /* verify scratch pad integrity */
            if(H5VL_iod_verify_scratch_pad(&sp, sp_cs) < 0)
                HGOTO_ERROR_FF(FAIL, "Scratch Pad failed integrity check");
        }

        /* open the metadata scratch pad */
        ret = iod_obj_open_write(coh, sp[0], rtid, NULL, &mdkv_oh, NULL);
        if(ret < 0)
            HGOTO_ERROR_FF(ret, "can't open MDKV");
    }
    else {
        /* open the metadata KV */
        ret = iod_obj_open_write(coh, input->loc_mdkv_id, rtid, NULL, &mdkv_oh, NULL);
        if(ret < 0)
            HGOTO_ERROR_FF(ret, "can't open MDKV");
    }

    {
        iod_kv_t kv;

        kv.key = H5VL_IOD_KEY_OBJ_COMMENT;
        kv.key_len = 1 + strlen(H5VL_IOD_KEY_OBJ_COMMENT);
        kv.value_len = strlen(comment) + 1;
        kv.value = comment;

        if(cs_scope & H5_CHECKSUM_IOD) {
            iod_checksum_t cs[2];

            cs[0] = H5_checksum_crc64(kv.key, kv.key_len);
            cs[1] = H5_checksum_crc64(kv.value, kv.value_len);
            ret = iod_kv_set(mdkv_oh, wtid, NULL, &kv, cs, NULL);
            if(ret < 0)
                HGOTO_ERROR_FF(ret, "can't set KV pair in parent");
        }
        else {
            ret = iod_kv_set(mdkv_oh, wtid, NULL, &kv, NULL, NULL);
            if(ret < 0)
                HGOTO_ERROR_FF(ret, "can't set comment in MDKV");
        }

    }

    /* close metadata KV and object */
    ret = iod_obj_close(mdkv_oh, NULL, NULL);
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "can't close object");

    if(loc_oh.rd_oh.cookie != obj_oh.rd_oh.cookie && 
       iod_obj_close(obj_oh.rd_oh, NULL, NULL) < 0)
        HGOTO_ERROR_FF(FAIL, "can't close object");

done:

#if H5_EFF_DEBUG
    fprintf(stderr, "Done with set comment, sending response to client\n");
#endif

    HG_Handler_start_output(op_data->hg_handle, &ret_value);

    input = (object_set_comment_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

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
    iod_handles_t loc_oh = input->loc_oh;
    iod_obj_id_t loc_id = input->loc_id;
    size_t length = input->length;
    iod_trans_id_t rtid = input->rcxt_num;
    uint32_t cs_scope = input->cs_scope;
    iod_handle_t mdkv_oh;
    iod_handles_t obj_oh;
    iod_obj_id_t obj_id;
    const char *loc_name = input->path;
    scratch_pad sp;
    iod_checksum_t sp_cs = 0;
    iod_size_t key_size, val_size = 0;
    void *value = NULL;
    iod_checksum_t *iod_cs = NULL;
    iod_ret_t ret;
    herr_t ret_value = SUCCEED;

#if H5_EFF_DEBUG
    fprintf(stderr, "Start Object Get comment on %s (OH %"PRIu64" ID %"PRIx64")\n", 
            input->path, input->loc_oh.rd_oh.cookie, input->loc_id);
#endif

    /* Traverse Path and open object */
    ret = H5VL_iod_server_open_path(coh, loc_id, loc_oh, loc_name, 
                                    rtid, cs_scope, &obj_id, &obj_oh);
    if(ret != SUCCEED)
        HGOTO_ERROR_FF(ret, "can't open object");

    if(loc_id != obj_id || input->loc_mdkv_id == IOD_OBJ_INVALID) {
        /* get scratch pad of the object */
        ret = iod_obj_get_scratch(obj_oh.rd_oh, rtid, &sp, &sp_cs, NULL);
        if(ret < 0)
            HGOTO_ERROR_FF(ret, "can't get scratch pad for object");

        if(sp_cs && (cs_scope & H5_CHECKSUM_IOD)) {
            /* verify scratch pad integrity */
            if(H5VL_iod_verify_scratch_pad(&sp, sp_cs) < 0)
                HGOTO_ERROR_FF(FAIL, "Scratch Pad failed integrity check");
        }

        /* open the metadata KV */
        ret = iod_obj_open_read(coh, sp[0], rtid, NULL, &mdkv_oh, NULL);
        if(ret < 0)
            HGOTO_ERROR_FF(ret, "can't open MDKV");
    }
    else {
        /* open the metadata KV */
        ret = iod_obj_open_read(coh, input->loc_mdkv_id, rtid, NULL, &mdkv_oh, NULL);
        if(ret < 0)
            HGOTO_ERROR_FF(ret, "can't open MDKV");
    }

    comment.value_size = (ssize_t *)malloc(sizeof(ssize_t));
    comment.value = NULL;
    comment.size = length;

    key_size = 1 + strlen(H5VL_IOD_KEY_OBJ_COMMENT);

    if(cs_scope & H5_CHECKSUM_IOD) {
        iod_cs = (iod_checksum_t *)malloc(sizeof(iod_checksum_t) * 2);
    }

    ret = iod_kv_get_value(mdkv_oh, rtid, H5VL_IOD_KEY_OBJ_COMMENT, key_size,
                           NULL, &val_size, NULL, NULL);
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "comment size lookup failed");

    if(NULL == (value = malloc ((size_t)val_size)))
        HGOTO_ERROR_FF(FAIL, "can't allocate value buffer");

    ret = iod_kv_get_value(mdkv_oh, rtid, H5VL_IOD_KEY_OBJ_COMMENT, key_size,
                           value, &val_size, iod_cs, NULL);
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "comment value lookup failed");

    if(cs_scope & H5_CHECKSUM_IOD) {
        if(H5VL_iod_verify_kv_pair(H5VL_IOD_KEY_OBJ_COMMENT, key_size, 
                                   value, val_size, iod_cs) < 0)
            HGOTO_ERROR_FF(FAIL, "Corruption detected when reading metadata from IOD");
    }

    if(length) {
        if(NULL == (comment.value = (char *)malloc (length)))
            HGOTO_ERROR_FF(FAIL, "can't allocate value buffer");
        memcpy(comment.value, value, length);
    }

    free(value);

    /* close metadata KV and object */
    ret = iod_obj_close(mdkv_oh, NULL, NULL);
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "can't close object");

    if(loc_oh.rd_oh.cookie != obj_oh.rd_oh.cookie && 
       iod_obj_close(obj_oh.rd_oh, NULL, NULL) < 0)
        HGOTO_ERROR_FF(FAIL, "can't close object");

    *comment.value_size = (ssize_t)val_size;

done:
    output.ret = ret_value;
    output.name = comment;

#if H5_EFF_DEBUG
    fprintf(stderr, "Done with get comment, sending response to client\n");
#endif

    HG_Handler_start_output(op_data->hg_handle, &output);

    if(comment.value) {
        free(comment.value);
        comment.value = NULL;
    }

    if(comment.value_size) {
        free(comment.value_size);
        comment.value_size = NULL;
    }

    if(iod_cs) {
        free(iod_cs);
        iod_cs = NULL;
    }

    input = (object_get_comment_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

} /* end H5VL_iod_server_object_get_comment_cb() */

#if 0

/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_object_open_by_addr_cb
 *
 * Purpose:	Opens an existing object in the container
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              February, 2014
 *
 *-------------------------------------------------------------------------
 */
void
H5VL_iod_server_object_open_by_addr_cb(AXE_engine_t UNUSED axe_engine, 
                               size_t UNUSED num_n_parents, AXE_task_t UNUSED n_parents[], 
                               size_t UNUSED num_s_parents, AXE_task_t UNUSED s_parents[], 
                               void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    object_op_in_t *input = (object_op_in_t *)op_data->input;
    object_open_out_t output;
    iod_handle_t coh = input->coh; /* the container handle */
    iod_obj_id_t obj_id = input->iod_id; /* The ID of the object */
    iod_trans_id_t rtid = input->rcxt_num;
    uint32_t cs_scope = input->cs_scope;
    iod_handles_t obj_oh; /* The handle for object */
    iod_handle_t mdkv_oh;
    scratch_pad sp;
    iod_checksum_t sp_cs = 0;
    herr_t ret_value = SUCCEED;

#if H5_EFF_DEBUG
    fprintf(stderr, "Start Object Open By addr on %"PRIx64"\n", obj_id);
#endif

    if (iod_obj_open_read(coh, obj_id, rtid, NULL, &obj_oh.rd_oh, NULL) < 0)
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "can't opeb obejct for read");
    if (iod_obj_open_write(coh, obj_id, rtid, NULL, &obj_oh.wr_oh, NULL) < 0)
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "can't open object for write");

    /* get scratch pad of the object */
    if(iod_obj_get_scratch(obj_oh.rd_oh, rtid, &sp, &sp_cs, NULL) < 0)
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "can't get scratch pad for object");

    if(sp_cs && (cs_scope & H5_CHECKSUM_IOD)) {
        /* verify scratch pad integrity */
        if(H5VL_iod_verify_scratch_pad(&sp, sp_cs) < 0)
            HGOTO_ERROR_FF(FAIL, "Scratch Pad failed integrity check");
    }

    /* open the metadata KV */
    if (iod_obj_open_read(coh, sp[0], rtid, NULL /*hints*/, &mdkv_oh, NULL) < 0)
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "can't open MDKV");

    if(H5VL_iod_get_metadata(mdkv_oh, rtid, H5VL_IOD_OBJECT_TYPE, H5VL_IOD_KEY_OBJ_TYPE,
                             cs_scope, NULL, &output.obj_type) < 0)
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "failed to retrieve link count");

    switch(output.obj_type) {
    case H5I_MAP:
        if(H5VL_iod_get_metadata(mdkv_oh, rtid, H5VL_IOD_PLIST, H5VL_IOD_KEY_OBJ_CPL,
                                 cs_scope, NULL, &output.cpl_id) < 0)
        if(ret < 0)
        HGOTO_ERROR_FF(ret, "failed to retrieve mcpl");

        if(H5VL_iod_get_metadata(mdkv_oh, rtid, H5VL_IOD_DATATYPE, 
                                 H5VL_IOD_KEY_MAP_KEY_TYPE,
                                 cs_scope, NULL, &output.id1) < 0)
        if(ret < 0)
        HGOTO_ERROR_FF(ret, "failed to retrieve link count");

        if(H5VL_iod_get_metadata(mdkv_oh, rtid, H5VL_IOD_DATATYPE, 
                                 H5VL_IOD_KEY_MAP_VALUE_TYPE,
                                 cs_scope, NULL, &output.id2) < 0)
        if(ret < 0)
        HGOTO_ERROR_FF(ret, "failed to retrieve link count");
        break;
    case H5I_GROUP:
        if(H5VL_iod_get_metadata(mdkv_oh, rtid, H5VL_IOD_PLIST, H5VL_IOD_KEY_OBJ_CPL,
                                 cs_scope, NULL, &output.cpl_id) < 0)
        if(ret < 0)
        HGOTO_ERROR_FF(ret, "failed to retrieve dcpl");
        output.id1 = FAIL;
        output.id2 = FAIL;
        break;
    case H5I_DATASET:
        if(H5VL_iod_get_metadata(mdkv_oh, rtid, H5VL_IOD_PLIST, H5VL_IOD_KEY_OBJ_CPL,
                                 cs_scope, NULL, &output.cpl_id) < 0)
        if(ret < 0)
        HGOTO_ERROR_FF(ret, "failed to retrieve dcpl");

        if(H5VL_iod_get_metadata(mdkv_oh, rtid, H5VL_IOD_DATATYPE, H5VL_IOD_KEY_OBJ_DATATYPE,
                                 cs_scope, NULL, &output.id1) < 0)
        if(ret < 0)
        HGOTO_ERROR_FF(ret, "failed to retrieve datatype");

        if(H5VL_iod_get_metadata(mdkv_oh, rtid, H5VL_IOD_DATASPACE, H5VL_IOD_KEY_OBJ_DATASPACE,
                                 cs_scope, NULL, &output.id2) < 0)
        if(ret < 0)
        HGOTO_ERROR_FF(ret, "failed to retrieve dataspace");
        break;
    case H5I_ATTR:
        if(H5VL_iod_get_metadata(mdkv_oh, rtid, H5VL_IOD_DATATYPE, H5VL_IOD_KEY_OBJ_DATATYPE,
                                 cs_scope, NULL, &output.id1) < 0)
        if(ret < 0)
        HGOTO_ERROR_FF(ret, "failed to retrieve datatype");

        if(H5VL_iod_get_metadata(mdkv_oh, rtid, H5VL_IOD_DATASPACE, H5VL_IOD_KEY_OBJ_DATASPACE,
                                 cs_scope, NULL, &output.id2) < 0)
        if(ret < 0)
        HGOTO_ERROR_FF(ret, "failed to retrieve dataspace");
        break;
    case H5I_DATATYPE:
        {
            size_t buf_size; /* size of serialized datatype */
            void *buf = NULL;
            iod_mem_desc_t *mem_desc = NULL; /* memory descriptor used for reading */
            iod_blob_iodesc_t *file_desc = NULL; /* file descriptor used to write */
            iod_checksum_t dt_cs = 0, blob_cs = 0;
            iod_size_t key_size, val_size;
            iod_checksum_t iod_cs[2];

            key_size = 1 + strlen(H5VL_IOD_KEY_DTYPE_SIZE);
            val_size = sizeof(iod_size_t);

            if(H5VL_iod_get_metadata(mdkv_oh, rtid, H5VL_IOD_PLIST, H5VL_IOD_KEY_OBJ_CPL,
                                     cs_scope, NULL, &output.cpl_id) < 0)
            if(ret < 0)
        HGOTO_ERROR_FF(ret, "failed to retrieve dcpl");

            /* retrieve blob size metadata from scratch pad */
            if(iod_kv_get_value(mdkv_oh, rtid, H5VL_IOD_KEY_DTYPE_SIZE, key_size,
                                &buf_size, &val_size, iod_cs, NULL) < 0)
            if(ret < 0)
        HGOTO_ERROR_FF(ret, "datatype size lookup failed");

            if(cs_scope & H5_CHECKSUM_IOD) {
                if(H5VL_iod_verify_kv_pair(H5VL_IOD_KEY_DTYPE_SIZE, key_size, 
                                           &buf_size, val_size, iod_cs) < 0)
                    HGOTO_ERROR_FF(FAIL, "Corruption detected when reading metadata from IOD");
            }

            if(NULL == (buf = malloc(buf_size)))
                HGOTO_ERROR_FF(FAIL, "can't allocate BLOB read buffer");

            /* create memory descriptor for writing */
            mem_desc = (iod_mem_desc_t *)malloc(sizeof(iod_mem_desc_t) + 
                                                sizeof(iod_mem_frag_t));
            mem_desc->nfrag = 1;
            mem_desc->frag[0].addr = buf;
            mem_desc->frag[0].len = (iod_size_t)buf_size;

            /* create file descriptor for writing */
            file_desc = (iod_blob_iodesc_t *)malloc(sizeof(iod_blob_iodesc_t) + 
                                                    sizeof(iod_blob_iofrag_t));
            file_desc->nfrag = 1;
            file_desc->frag[0].offset = 0;
            file_desc->frag[0].len = (iod_size_t)buf_size;

            /* read the serialized type value from the BLOB object */
            if(iod_blob_read(obj_oh.rd_oh, rtid, NULL, mem_desc, file_desc, 
                             &blob_cs, NULL) < 0)
            if(ret < 0)
        HGOTO_ERROR_FF(ret, "unable to read BLOB object");

            if(blob_cs && (cs_scope & H5_CHECKSUM_IOD)) {
                /* calculate a checksum for the datatype */
                dt_cs = H5_checksum_crc64(buf, buf_size);

                /* Verify checksum against one given by IOD */
                if(blob_cs != dt_cs)
                    HGOTO_ERROR_FF(FAIL, "Data Corruption detected when reading datatype");
            }

            /* decode the datatype */
            if((output.id1 = H5Tdecode(buf)) < 0)
                HGOTO_ERROR_FF(FAIL, "unable to decode datatype");
            output.id2 = FAIL;

            free(mem_desc);
            free(file_desc);
            free(buf);
            break;
        }
    default:
        HGOTO_ERROR_FF(FAIL, "Invalid object type");
    }

    /* close the metadata scratch pad */
    if(iod_obj_close(mdkv_oh, NULL, NULL) < 0)
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "can't close object");

    output.iod_id = obj_id;
    output.mdkv_id = sp[0];
    output.attrkv_id = sp[1];
    output.iod_oh.rd_oh.cookie = obj_oh.rd_oh.cookie;
    output.iod_oh.wr_oh.cookie = obj_oh.wr_oh.cookie;

#if H5_EFF_DEBUG
    fprintf(stderr, "Done with object open, sending response to client\n");
#endif

    HG_Handler_start_output(op_data->hg_handle, &output);

done:
    if(ret_value < 0) {
        output.iod_oh.rd_oh.cookie = IOD_OH_UNDEFINED;
        output.iod_oh.wr_oh.cookie = IOD_OH_UNDEFINED;
        output.iod_id = IOD_OBJ_INVALID;
        output.cpl_id = FAIL;
        output.id1 = FAIL;
        output.id2 = FAIL;
        HG_Handler_start_output(op_data->hg_handle, &output);
    }

    input = (object_op_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

} /* end H5VL_iod_server_object_open_by_addr_cb() */
#endif

#endif /* H5_HAVE_EFF */
