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
    iod_handle_t coh = input->coh; /* container handle */
    iod_handles_t loc_handle = input->loc_oh; /* location handle to start lookup */
    iod_obj_id_t loc_id = input->loc_id; /* The ID of the current location object */
    iod_obj_id_t dtype_id = input->dtype_id; /* The ID of the datatype that needs to be created */
    iod_obj_id_t mdkv_id = input->mdkv_id; /* The ID of the metadata KV to be created */
    iod_obj_id_t attr_id = input->attrkv_id; /* The ID of the attirbute KV to be created */
    iod_trans_id_t wtid = input->trans_num;
    iod_trans_id_t rtid = input->rcxt_num;
    uint32_t cs_scope = input->cs_scope;
    iod_handles_t dtype_oh, cur_oh;
    iod_handle_t mdkv_oh;
    iod_obj_id_t cur_id;
    const char *name = input->name; /* name of dtype including path to commit */
    hid_t tcpl_id;
    char *last_comp; /* the name of the datatype obtained from the last component in the path */
    size_t buf_size; /* size of the serialized datatype */
    void *buf;
    iod_mem_desc_t *mem_desc = NULL; /* memory descriptor used for writing */
    iod_blob_iodesc_t *file_desc = NULL; /* file descriptor used to write */
    scratch_pad sp;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Start datatype Commit %s\n", name);
#endif

    /* the traversal will retrieve the location where the datatype needs
       to be created. The traversal will fail if an intermediate group
       does not exist. */
    if(H5VL_iod_server_traverse(coh, loc_id, loc_handle, name, rtid, FALSE, 
                                &last_comp, &cur_id, &cur_oh) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't traverse path");

    /* create the datatype */
    if(iod_obj_create(coh, wtid, NULL, IOD_OBJ_BLOB, NULL, NULL, &dtype_id, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't create Datatype");

    if (iod_obj_open_read(coh, dtype_id, NULL, &dtype_oh.rd_oh, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't open Datatype");
    if (iod_obj_open_write(coh, dtype_id, NULL, &dtype_oh.wr_oh, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't open Datatype");

    /* create the metadata KV object for the datatype */
    if(iod_obj_create(coh, wtid, NULL, IOD_OBJ_KV, NULL, NULL, &mdkv_id, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't create metadata KV object");

    /* create the attribute KV object for the datatype */
    if(iod_obj_create(coh, wtid, NULL, IOD_OBJ_KV, NULL, NULL, &attr_id, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't create metadata KV object");

    /* set values for the scratch pad object */
    sp[0] = mdkv_id;
    sp[1] = attr_id;
    sp[2] = IOD_OBJ_INVALID;
    sp[3] = IOD_OBJ_INVALID;

    /* set scratch pad in datatype */
    if(cs_scope & H5_CHECKSUM_IOD) {
        iod_checksum_t sp_cs;

        sp_cs = H5_checksum_crc64(&sp, sizeof(sp));
        if (iod_obj_set_scratch(dtype_oh.wr_oh, wtid, &sp, &sp_cs, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't set scratch pad");
    }
    else {
        if (iod_obj_set_scratch(dtype_oh.wr_oh, wtid, &sp, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't set scratch pad");
    }

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

    /* create memory descriptor for writing */
    mem_desc = (iod_mem_desc_t *)malloc(sizeof(iod_mem_desc_t) + sizeof(iod_mem_frag_t));
    mem_desc->nfrag = 1;
    mem_desc->frag[0].addr = buf;
    mem_desc->frag[0].len = (iod_size_t)buf_size;

    /* create file descriptor for writing */
    file_desc = (iod_blob_iodesc_t *)malloc(sizeof(iod_blob_iodesc_t) + 
                                            sizeof(iod_blob_iofrag_t));
    file_desc->nfrag = 1;
    file_desc->frag[0].offset = 0;
    file_desc->frag[0].len = (iod_size_t)buf_size;

    /* set scratch pad in datatype */
    if(cs_scope & H5_CHECKSUM_IOD) {
        iod_checksum_t dt_cs;

        /* calculate a checksum for the datatype */
        dt_cs = H5_checksum_crc64(buf, buf_size);

        /* write the serialized type value to the BLOB object */
        if(iod_blob_write(dtype_oh.wr_oh, wtid, NULL, mem_desc, file_desc, &dt_cs, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to write BLOB object");
    }
    else {
        /* write the serialized type value to the BLOB object */
        if(iod_blob_write(dtype_oh.wr_oh, wtid, NULL, mem_desc, file_desc, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to write BLOB object");
    }

    free(mem_desc);
    free(file_desc);

    if(H5P_DEFAULT == input->tcpl_id)
        input->tcpl_id = H5Pcopy(H5P_DATATYPE_CREATE_DEFAULT);
    tcpl_id = input->tcpl_id;

    /* insert plist metadata */
    if(H5VL_iod_insert_plist(mdkv_oh, wtid, tcpl_id, NULL, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't insert KV value");

    /* insert link count metadata */
    if(H5VL_iod_insert_link_count(mdkv_oh, wtid, (uint64_t)1, NULL, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't insert KV value");

    /* insert object type metadata */
    if(H5VL_iod_insert_object_type(mdkv_oh, wtid, H5I_DATATYPE, NULL, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't insert KV value");

    /* store the datatype size */
    {
        iod_kv_t kv;
        char *key = NULL;

        key = strdup(H5VL_IOD_KEY_DTYPE_SIZE);
        kv.key = key;
        kv.key_len = strlen(key);
        kv.value_len = sizeof(iod_size_t);
        kv.value = &buf_size;

        if (iod_kv_set(mdkv_oh, wtid, NULL, &kv, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't set KV pair in parent");

        free(key);
    }

    /* close the Metadata KV object */
    if(iod_obj_close(mdkv_oh, NULL, NULL))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close object");

    /* add link in parent group to current object */
    if(H5VL_iod_insert_new_link(cur_oh.wr_oh, wtid, last_comp, 
                                H5L_TYPE_HARD, &dtype_id, NULL, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't insert KV value");

    /* close parent group and its scratch pad if it is not the
       location we started the traversal into */
    if(loc_handle.rd_oh.cookie != cur_oh.rd_oh.cookie) {
        iod_obj_close(cur_oh.rd_oh, NULL, NULL);
    }
    if(loc_handle.wr_oh.cookie != cur_oh.wr_oh.cookie) {
        iod_obj_close(cur_oh.wr_oh, NULL, NULL);
    }

    output.iod_oh.rd_oh.cookie = dtype_oh.rd_oh.cookie;
    output.iod_oh.wr_oh.cookie = dtype_oh.wr_oh.cookie;

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Done with dtype commit, sending response to client\n");
#endif

    HG_Handler_start_output(op_data->hg_handle, &output);

done:
    if(ret_value < 0) {
        output.iod_oh.rd_oh.cookie = IOD_OH_UNDEFINED;
        output.iod_oh.wr_oh.cookie = IOD_OH_UNDEFINED;
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
    iod_handle_t coh = input->coh; /* container handle */
    iod_handles_t loc_handle = input->loc_oh; /* location handle to start lookup */
    iod_obj_id_t loc_id = input->loc_id; /* The ID of the current location object */
    iod_obj_id_t dtype_id; /* ID of datatype to open */
    iod_handles_t dtype_oh;
    iod_handle_t mdkv_oh;
    const char *name = input->name; /* name of dtype including path to open */
    iod_trans_id_t rtid = input->rcxt_num;
    uint32_t cs_scope = input->cs_scope;
    size_t buf_size; /* size of serialized datatype */
    void *buf = NULL;
    iod_mem_desc_t *mem_desc = NULL; /* memory descriptor used for reading */
    iod_blob_iodesc_t *file_desc = NULL; /* file descriptor used to write */
    scratch_pad sp;
    iod_checksum_t sp_cs = 0;
    iod_checksum_t dt_cs = 0, iod_cs = 0;
    iod_size_t key_size=0, val_size=0;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Start datatype Open %s with Loc ID %"PRIu64"\n", name, loc_id);
#endif

    /* Traverse Path and open dtype */
    if(H5VL_iod_server_open_path(coh, loc_id, loc_handle, name, rtid, &dtype_id, &dtype_oh) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't open object");

    /* open a write handle on the ID. */
    if (iod_obj_open_write(coh, dtype_id, NULL, &dtype_oh.wr_oh, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't open current datatype");

    /* get scratch pad of the datatype */
    if(iod_obj_get_scratch(dtype_oh.rd_oh, rtid, &sp, &sp_cs, NULL) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "can't get scratch pad for object");

    if(sp_cs && (cs_scope & H5_CHECKSUM_IOD)) {
        /* verify scratch pad integrity */
        if(H5VL_iod_verify_scratch_pad(sp, sp_cs) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Scratch Pad failed integrity check");
    }

    /* open the metadata scratch pad */
    if (iod_obj_open_read(coh, sp[0], NULL /*hints*/, &mdkv_oh, NULL) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "can't open scratch pad");

    if(H5VL_iod_get_metadata(mdkv_oh, rtid, H5VL_IOD_PLIST, H5VL_IOD_KEY_OBJ_CPL,
                             NULL, NULL, &output.tcpl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "failed to retrieve tcpl");

    val_size = sizeof(iod_size_t);
    key_size = strlen(H5VL_IOD_KEY_DTYPE_SIZE);

    /* retrieve blob size metadata from scratch pad */
    if(iod_kv_get_value(mdkv_oh, rtid, H5VL_IOD_KEY_DTYPE_SIZE, key_size,
                        &buf_size, &val_size, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "datatype size lookup failed");

    if(NULL == (buf = malloc(buf_size)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate BLOB read buffer");

    /* close the metadata scratch pad */
    if(iod_obj_close(mdkv_oh, NULL, NULL))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close object");

    /* create memory descriptor for writing */
    mem_desc = (iod_mem_desc_t *)malloc(sizeof(iod_mem_desc_t) + sizeof(iod_mem_frag_t));
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
    if(iod_blob_read(dtype_oh.rd_oh, rtid, NULL, mem_desc, file_desc, &iod_cs, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to write BLOB object");

    if(iod_cs && (cs_scope & H5_CHECKSUM_IOD)) {
        /* calculate a checksum for the datatype */
        dt_cs = H5_checksum_crc64(buf, buf_size);

        /* Verifty checksum against one given by IOD */
        if(iod_cs != dt_cs)
            HGOTO_ERROR(H5E_SYM, H5E_READERROR, FAIL, "Data Corruption detected when reading datatype");
    }

    free(mem_desc);
    free(file_desc);

    /* decode the datatype */
    if((output.type_id = H5Tdecode(buf)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to decode datatype");

    free(buf);

    output.iod_id = dtype_id;
    output.mdkv_id = sp[0];
    output.attrkv_id = sp[1];
    output.iod_oh.rd_oh.cookie = dtype_oh.rd_oh.cookie;
    output.iod_oh.wr_oh.cookie = dtype_oh.wr_oh.cookie;

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Done with dtype open, sending response to client\n");
#endif

    HG_Handler_start_output(op_data->hg_handle, &output);

done:
    if(ret_value < 0) {
        output.iod_oh.rd_oh.cookie = IOD_OH_UNDEFINED;
        output.iod_oh.wr_oh.cookie = IOD_OH_UNDEFINED;
        output.iod_id = IOD_OBJ_INVALID;
        HG_Handler_start_output(op_data->hg_handle, &output);
    }

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
    iod_handles_t iod_oh = input->iod_oh;
    //iod_obj_id_t iod_id = input->iod_id; 
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Start datatype Close\n");
#endif

    if((iod_obj_close(iod_oh.rd_oh, NULL, NULL)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close object");
    if((iod_obj_close(iod_oh.wr_oh, NULL, NULL)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close object");

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
