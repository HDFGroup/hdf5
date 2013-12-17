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
    unsigned num_peers = input->num_peers; /* the number of peers participating in creation */
    iod_obj_id_t root_id = input->root_id;
    iod_obj_id_t mdkv_id = input->mdkv_id;
    iod_obj_id_t attrkv_id = input->attrkv_id;
    unsigned int mode; /* create mode */
    iod_handle_t coh; /* container handle */
    iod_handles_t root_oh; /* root object handle */
    iod_handle_t mdkv_oh; /* metadata object handle for KV to store file's metadata */
    iod_ret_t ret, root_ret;
    iod_trans_id_t first_tid = 0;
    uint32_t cs_scope = 0;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Start file create %s ", input->name);
    fprintf(stderr, "with MDKV %"PRIx64" ", mdkv_id), 
    fprintf(stderr, "with attrKV %"PRIx64"\n", attrkv_id), 
#endif

    /* convert HDF5 flags to IOD flags */
    mode = (input->flags&H5F_ACC_RDWR) ? IOD_CONT_RW : IOD_CONT_R;
    if (input->flags&H5F_ACC_CREAT) 
        mode |= IOD_CONT_CREATE;

    if(H5Pget_metadata_integrity_scope(input->fapl_id, &cs_scope) < 0)
        HGOTO_ERROR2(H5E_PLIST, H5E_CANTGET, FAIL, "can't get scope for data integrity checks");

    /* Create the Container */
    if((ret = iod_container_open(input->name, NULL /*hints*/, mode, &coh, NULL /*event*/)) < 0) {
        fprintf(stderr, "%d (%s).\n", ret, strerror(-ret));
        HGOTO_ERROR2(H5E_FILE, H5E_CANTINIT, FAIL, "can't create container");
    }

    if(iod_trans_start(coh, &first_tid, NULL, num_peers, IOD_TRANS_W, NULL) < 0)
        HGOTO_ERROR2(H5E_SYM, H5E_CANTSET, FAIL, "can't start transaction");

    /* create the root group */
    root_ret = iod_obj_create(coh, first_tid, NULL, IOD_OBJ_KV, NULL, NULL, 
                         &root_id, NULL);
    if(0 == root_ret || -EEXIST == root_ret) {
        //fprintf(stderr, "created Root group %"PRIx64"\n", root_id);
        /* root group has been created, open it */
        if ((ret = iod_obj_open_write(coh, root_id, first_tid, NULL, &root_oh.wr_oh, NULL)) < 0) {
            fprintf(stderr, "%d (%s).\n", ret, strerror(-ret));
            HGOTO_ERROR2(H5E_SYM, H5E_CANTINIT, FAIL, "can't open root group for write");
        }
        if ((ret = iod_obj_open_read(coh, root_id, first_tid, NULL, &root_oh.rd_oh, NULL)) < 0) {
            fprintf(stderr, "%d (%s).\n", ret, strerror(-ret));
            HGOTO_ERROR2(H5E_SYM, H5E_CANTINIT, FAIL, "can't open root group for read");
        }
    }
    else {
        HGOTO_ERROR2(H5E_SYM, H5E_CANTINIT, FAIL, "can't create root group");
    }

    /* for the process that succeeded in creating the group, create
       the scratch pad for it too. */
    if(0 == root_ret) {
        scratch_pad sp;
        iod_kv_t kv;
        uint64_t value = 1;
        hid_t fcpl_id;

        //fprintf(stderr, "created MDKV and ATTRKV %"PRIx64"  %"PRIx64"\n", 
        //mdkv_id, attrkv_id);
        /* create the metadata KV object for the root group */
        if((ret = iod_obj_create(coh, first_tid, NULL, IOD_OBJ_KV, 
                                 NULL, NULL, &mdkv_id, NULL)) < 0) {
            fprintf(stderr, "%d (%s).\n", ret, strerror(-ret));
            HGOTO_ERROR2(H5E_SYM, H5E_CANTINIT, FAIL, "can't create metadata KV object");
        }
        /* create the attribute KV object for the root group */
        if(iod_obj_create(coh, first_tid, NULL, IOD_OBJ_KV, 
                          NULL, NULL, &attrkv_id, NULL) < 0)
            HGOTO_ERROR2(H5E_SYM, H5E_CANTINIT, FAIL, "can't create attribute KV object");

        /* set values for the scratch pad object */
        sp[0] = mdkv_id;
        sp[1] = attrkv_id;
        sp[2] = IOD_OBJ_INVALID;
        sp[3] = IOD_OBJ_INVALID;

        if(cs_scope & H5_CHECKSUM_IOD) {
            iod_checksum_t sp_cs;

            sp_cs = H5_checksum_crc64(&sp, sizeof(sp));
            /* set scratch pad in root group */
            if (iod_obj_set_scratch(root_oh.wr_oh, first_tid, &sp, &sp_cs, NULL) < 0)
                HGOTO_ERROR2(H5E_SYM, H5E_CANTINIT, FAIL, "can't set scratch pad");
        }
        else {
#if H5VL_IOD_DEBUG
            fprintf(stderr, "METADATA INTEGRITY DISABLED\n");
#endif
            if (iod_obj_set_scratch(root_oh.wr_oh, first_tid, &sp, NULL, NULL) < 0)
                HGOTO_ERROR2(H5E_SYM, H5E_CANTINIT, FAIL, "can't set scratch pad");
        }

        /* Store Metadata in scratch pad */
        if (iod_obj_open_write(coh, input->mdkv_id, first_tid, NULL, &mdkv_oh, NULL) < 0)
            HGOTO_ERROR2(H5E_SYM, H5E_CANTINIT, FAIL, "can't open metadata KV");

        /* store metadata */
        if(H5P_DEFAULT == input->fcpl_id)
            input->fcpl_id = H5Pcopy(H5P_FILE_CREATE_DEFAULT);
        fcpl_id = input->fcpl_id;
        /* insert plist metadata */
        if(H5VL_iod_insert_plist(mdkv_oh, first_tid, fcpl_id, 
                                 NULL, NULL, NULL) < 0)
            HGOTO_ERROR2(H5E_SYM, H5E_CANTINIT, FAIL, "can't insert link count KV value");

        kv.value = &value;
        kv.value_len = sizeof(uint64_t);
        kv.key = (void *)H5VL_IOD_KEY_KV_IDS_INDEX;
        kv.key_len = strlen(H5VL_IOD_KEY_KV_IDS_INDEX);

        if (iod_kv_set(mdkv_oh, first_tid, NULL, &kv, NULL, NULL) < 0)
            HGOTO_ERROR2(H5E_SYM, H5E_CANTINIT, FAIL, "can't set KV pair in parent");

        kv.key = (void *)H5VL_IOD_KEY_ARRAY_IDS_INDEX;
        kv.key_len = strlen(H5VL_IOD_KEY_ARRAY_IDS_INDEX);
        if (iod_kv_set(mdkv_oh, first_tid, NULL, &kv, NULL, NULL) < 0)
            HGOTO_ERROR2(H5E_SYM, H5E_CANTINIT, FAIL, "can't set KV pair in parent");

        kv.key = (void *)H5VL_IOD_KEY_BLOB_IDS_INDEX;
        kv.key_len = strlen(H5VL_IOD_KEY_BLOB_IDS_INDEX);
        if (iod_kv_set(mdkv_oh, first_tid, NULL, &kv, NULL, NULL) < 0)
            HGOTO_ERROR2(H5E_SYM, H5E_CANTINIT, FAIL, "can't set KV pair in parent");

        if(iod_obj_close(mdkv_oh, NULL, NULL) < 0)
            HGOTO_ERROR2(H5E_SYM, H5E_CANTINIT, FAIL, "can't close root object handle");
    }

    /* Finish  the transaction */
    if(iod_trans_finish(coh, first_tid, NULL, 0, NULL) < 0)
        HGOTO_ERROR2(H5E_SYM, H5E_CANTSET, FAIL, "can't finish transaction 0");

    output.coh.cookie = coh.cookie;
    output.root_oh.rd_oh = root_oh.rd_oh;
    output.root_oh.wr_oh = root_oh.wr_oh;
    output.kv_oid_index = 3;
    output.array_oid_index = 0;
    output.blob_oid_index = 0;

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Done with file create coh: %"PRIu64" root rd: %"PRIu64"  wr: %"PRIu64"\n",
            coh.cookie, root_oh.rd_oh.cookie, root_oh.wr_oh.cookie);
#endif

    HG_Handler_start_output(op_data->hg_handle, &output);

done:
    if(ret_value < 0) {
        output.coh.cookie = IOD_OH_UNDEFINED;
        output.root_oh.rd_oh.cookie = IOD_OH_UNDEFINED;
        output.root_oh.wr_oh.cookie = IOD_OH_UNDEFINED;
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
    hbool_t acquire = input->acquire;
    iod_handle_t coh; /* container handle */
    iod_handles_t root_oh; /* root object handle */
    iod_handle_t mdkv_oh; /* metadata object handle for KV to store file's metadata */
    scratch_pad sp;
    iod_checksum_t sp_cs = 0;
    iod_cont_trans_stat_t *tids = NULL;
    iod_trans_id_t rtid;
    iod_size_t key_size = 0, val_size = 0;
    uint32_t cs_scope = 0;
    iod_ret_t ret;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Start file open %s %d %d\n", input->name, input->flags, input->fapl_id);
#endif

    if(H5F_ACC_RDWR == mode)
        mode = IOD_CONT_RW;
    else if(H5F_ACC_RDONLY == mode)
        mode = IOD_CONT_R;
    else
        HGOTO_ERROR2(H5E_FILE, H5E_CANTINIT, FAIL, "invalid mode");

    if(H5Pget_metadata_integrity_scope(input->fapl_id, &cs_scope) < 0)
        HGOTO_ERROR2(H5E_PLIST, H5E_CANTGET, FAIL, "can't get scope for data integrity checks");

    /* open the container */
    if(iod_container_open(input->name, NULL /*hints*/, mode, &coh, NULL /*event*/))
        HGOTO_ERROR2(H5E_FILE, H5E_CANTINIT, FAIL, "can't open file");

    if(iod_query_cont_trans_stat(coh, &tids, NULL) < 0)
        HGOTO_ERROR2(H5E_SYM, H5E_CANTGET, FAIL, "can't get container tids status");

    rtid = tids->latest_rdable;

    if(iod_free_cont_trans_stat(coh, tids) < 0)
        HGOTO_ERROR2(H5E_SYM, H5E_CANTINIT, FAIL, "can't free container transaction status object");

    if(iod_trans_start(coh, &rtid, NULL, 0, IOD_TRANS_R, NULL) < 0)
        HGOTO_ERROR2(H5E_SYM, H5E_CANTSET, FAIL, "can't start transaction");

    /* open the root group */
    if ((ret = iod_obj_open_read(coh, ROOT_ID, rtid, NULL /*hints*/, &root_oh.rd_oh, NULL)) < 0) {
        fprintf(stderr, "%d (%s).\n", ret, strerror(-ret));
        HGOTO_ERROR2(H5E_FILE, H5E_CANTINIT, FAIL, "can't open root object for read");
    }
    if ((ret = iod_obj_open_write(coh, ROOT_ID, rtid, NULL /*hints*/, &root_oh.wr_oh, NULL)) < 0) {
        fprintf(stderr, "%d (%s).\n", ret, strerror(-ret));
        HGOTO_ERROR2(H5E_FILE, H5E_CANTINIT, FAIL, "can't open root object for write");
    }
    /* get scratch pad of root group */
    if(iod_obj_get_scratch(root_oh.rd_oh, rtid, &sp, &sp_cs, NULL) < 0)
        HGOTO_ERROR2(H5E_FILE, H5E_CANTINIT, FAIL, "can't get scratch pad for root object");

    if(sp_cs && (cs_scope & H5_CHECKSUM_IOD)) {
        /* verify scratch pad integrity */
        if(H5VL_iod_verify_scratch_pad(sp, sp_cs) < 0)
            HGOTO_ERROR2(H5E_SYM, H5E_CANTINIT, FAIL, "Scratch Pad failed integrity check");
    }

    /* open the metadata scratch pad */
    if (iod_obj_open_read(coh, sp[0], rtid, NULL /*hints*/, &mdkv_oh, NULL) < 0)
        HGOTO_ERROR2(H5E_FILE, H5E_CANTINIT, FAIL, "can't open scratch pad");

    /* retrieve all metadata from scratch pad */
    if(H5VL_iod_get_metadata(mdkv_oh, rtid, H5VL_IOD_PLIST, H5VL_IOD_KEY_OBJ_CPL,
                             NULL, NULL, &output.fcpl_id) < 0)
        HGOTO_ERROR2(H5E_SYM, H5E_CANTGET, FAIL, "failed to retrieve fcpl");

    val_size = sizeof(uint64_t);
    key_size = strlen(H5VL_IOD_KEY_KV_IDS_INDEX);

    if(iod_kv_get_value(mdkv_oh, rtid, H5VL_IOD_KEY_KV_IDS_INDEX, key_size,
                        &output.kv_oid_index, &val_size, NULL, NULL) < 0)
        HGOTO_ERROR2(H5E_SYM, H5E_CANTINIT, FAIL, "KV index lookup failed");

    key_size = strlen(H5VL_IOD_KEY_ARRAY_IDS_INDEX);
    if(iod_kv_get_value(mdkv_oh, rtid, H5VL_IOD_KEY_ARRAY_IDS_INDEX, key_size,
                        &output.array_oid_index, &val_size, NULL, NULL) < 0)
        HGOTO_ERROR2(H5E_SYM, H5E_CANTINIT, FAIL, "Array index lookup failed");

    key_size = strlen(H5VL_IOD_KEY_BLOB_IDS_INDEX);
    if(iod_kv_get_value(mdkv_oh, rtid, H5VL_IOD_KEY_BLOB_IDS_INDEX, key_size,
                        &output.blob_oid_index, &val_size, NULL, NULL) < 0)
        HGOTO_ERROR2(H5E_SYM, H5E_CANTINIT, FAIL, "BLOB index lookup failed");

    /* close the metadata scratch pad */
    if(iod_obj_close(mdkv_oh, NULL, NULL) < 0)
        HGOTO_ERROR2(H5E_SYM, H5E_CANTINIT, FAIL, "can't close root object handle");

    output.coh.cookie = coh.cookie;
    output.root_id = ROOT_ID;
    output.mdkv_id = sp[0];
    output.attrkv_id = sp[1];
    output.root_oh.rd_oh = root_oh.rd_oh;
    output.root_oh.wr_oh = root_oh.wr_oh;
    output.c_version = rtid;

    /* If the user did not ask to acquire the latest readable version, finish it here */
    if(TRUE != acquire) {
        output.c_version = IOD_TID_UNKNOWN;
        if(iod_trans_finish(coh, rtid, NULL, 0, NULL) < 0)
            HGOTO_ERROR2(H5E_SYM, H5E_CANTSET, FAIL, "can't finish transaction 0");
    }

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Done with file open coh: %"PRIu64" root rd: %"PRIu64"  wr: %"PRIu64" CV: %"PRIu64"\n",
            coh.cookie, root_oh.rd_oh.cookie, root_oh.wr_oh.cookie, rtid);
#endif

    HG_Handler_start_output(op_data->hg_handle, &output);

done:
    if(ret_value < 0) {
        output.coh.cookie = IOD_OH_UNDEFINED;
        output.root_id = IOD_OBJ_INVALID;
        output.root_oh.rd_oh.cookie = IOD_OH_UNDEFINED;
        output.root_oh.wr_oh.cookie = IOD_OH_UNDEFINED;
        output.fcpl_id = H5P_FILE_CREATE_DEFAULT;
        output.kv_oid_index = 0;
        output.array_oid_index = 0;
        output.blob_oid_index = 0;
        output.c_version = IOD_TID_UNKNOWN;
        HG_Handler_start_output(op_data->hg_handle, &output);
    }

    input = (file_open_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

    FUNC_LEAVE_NOAPI_VOID
} /* end H5VL_iod_server_file_open_cb() */


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
    iod_handles_t root_oh = input->root_oh;
    iod_ret_t ret;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Start file close\n");
#endif

    /* The root client request will create a transaction and store the
       final indexes for used up IDs */
    if(input->max_kv_index || input->max_array_index || input->max_blob_index) {
        iod_cont_trans_stat_t *tids = NULL;
        iod_trans_id_t trans_num, rtid;
        scratch_pad sp;
        iod_checksum_t sp_cs = 0;
        iod_kv_t kv;
        iod_handle_t mdkv_oh; /* metadata object handle for KV to store file's metadata */
        uint32_t cs_scope = input->cs_scope;

        if(iod_query_cont_trans_stat(coh, &tids, NULL) < 0)
            HGOTO_ERROR2(H5E_SYM, H5E_CANTGET, FAIL, "can't get container tids status");

        trans_num = tids->latest_wrting + 1;
        rtid = tids->latest_rdable;

        if(iod_free_cont_trans_stat(coh, tids) < 0)
            HGOTO_ERROR2(H5E_SYM, H5E_CANTINIT, FAIL, "can't free container transaction status object");

#if H5VL_IOD_DEBUG
        fprintf(stderr, "File Close starting transaction %"PRIu64" rcxt %"PRIu64"\n", 
                trans_num, rtid);
#endif

        if((ret = iod_trans_start(coh, &rtid, NULL, 1, IOD_TRANS_R, NULL)) < 0) {
            fprintf(stderr, "%d (%s).\n", ret, strerror(-ret));
            HGOTO_ERROR2(H5E_SYM, H5E_CANTSET, FAIL, "can't start READ transaction");
        }

        if((ret = iod_trans_start(coh, &trans_num, NULL, 1, IOD_TRANS_W, NULL)) < 0) {
            fprintf(stderr, "%d (%s).\n", ret, strerror(-ret));
            HGOTO_ERROR2(H5E_SYM, H5E_CANTSET, FAIL, "can't start WRITE transaction");
        }

        /* get scratch pad of root group */
        if(iod_obj_get_scratch(root_oh.rd_oh, rtid, &sp, &sp_cs, NULL) < 0)
            HGOTO_ERROR2(H5E_FILE, H5E_CANTINIT, FAIL, "can't get scratch pad for root object");

        fprintf(stderr, "root OH %"PRIu64" MDKV ID %"PRIx64"\n", 
                root_oh.rd_oh.cookie, sp[0]);

        if(sp_cs && (cs_scope & H5_CHECKSUM_IOD)) {
            /* verify scratch pad integrity */
            if(H5VL_iod_verify_scratch_pad(sp, sp_cs) < 0)
                HGOTO_ERROR2(H5E_SYM, H5E_CANTINIT, FAIL, "Scratch Pad failed integrity check");
        }

        /* open the metadata scratch pad */
        if (iod_obj_open_write(coh, sp[0], rtid, NULL, &mdkv_oh, NULL) < 0)
            HGOTO_ERROR2(H5E_FILE, H5E_CANTINIT, FAIL, "can't open metadata KV");

        /* insert current indexes in the metadata KV object */
        kv.value = &input->max_kv_index;
        kv.value_len = sizeof(uint64_t);
        kv.key = (void *)H5VL_IOD_KEY_KV_IDS_INDEX;
        kv.key_len = strlen(H5VL_IOD_KEY_KV_IDS_INDEX);
        if (iod_kv_set(mdkv_oh, trans_num, NULL, &kv, NULL, NULL) < 0)
            HGOTO_ERROR2(H5E_SYM, H5E_CANTINIT, FAIL, "can't set KV pair in parent");

        kv.value = &input->max_array_index;
        kv.value_len = sizeof(uint64_t);
        kv.key = (void *)H5VL_IOD_KEY_ARRAY_IDS_INDEX;
        kv.key_len = strlen(H5VL_IOD_KEY_ARRAY_IDS_INDEX);
        if (iod_kv_set(mdkv_oh, trans_num, NULL, &kv, NULL, NULL) < 0)
            HGOTO_ERROR2(H5E_SYM, H5E_CANTINIT, FAIL, "can't set KV pair in parent");

        kv.value = &input->max_blob_index;
        kv.value_len = sizeof(uint64_t);
        kv.key = (void *)H5VL_IOD_KEY_BLOB_IDS_INDEX;
        kv.key_len = strlen(H5VL_IOD_KEY_BLOB_IDS_INDEX);
        if (iod_kv_set(mdkv_oh, trans_num, NULL, &kv, NULL, NULL) < 0)
            HGOTO_ERROR2(H5E_SYM, H5E_CANTINIT, FAIL, "can't set KV pair in parent");

        if(iod_obj_close(mdkv_oh, NULL, NULL) < 0)
            HGOTO_ERROR2(H5E_SYM, H5E_CANTINIT, FAIL, "can't close root object handle");

        /* finish the transaction */
        if(iod_trans_finish(coh, rtid, NULL, 0, NULL) < 0)
            HGOTO_ERROR2(H5E_SYM, H5E_CANTSET, FAIL, "can't finish transaction");

        /* finish the transaction */
        if(iod_trans_finish(coh, trans_num, NULL, 0, NULL) < 0)
            HGOTO_ERROR2(H5E_SYM, H5E_CANTSET, FAIL, "can't finish transaction");
    }

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Closing ROOT Group: R: %"PRIu64"  W: %"PRIu64"\n", root_oh.rd_oh, root_oh.wr_oh);
#endif

    /* close the root group */
    if(iod_obj_close(root_oh.rd_oh, NULL, NULL) < 0)
        HGOTO_ERROR2(H5E_SYM, H5E_CANTDEC, FAIL, "can't close root object handle");
    if(iod_obj_close(root_oh.wr_oh, NULL, NULL) < 0)
        HGOTO_ERROR2(H5E_SYM, H5E_CANTDEC, FAIL, "can't close root object handle");

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Closing Container: %"PRIu64"\n", coh);
#endif

    /* close the container */
    if((ret = iod_container_close(coh, NULL, NULL)) < 0) {
        fprintf(stderr, "%d (%s).\n", ret, strerror(-ret));
        HGOTO_ERROR2(H5E_FILE, H5E_CANTDEC, FAIL, "can't close container");
    }

done:
#if H5VL_IOD_DEBUG
    fprintf(stderr, "Done with file close, sending response to client\n");
#endif
    if(HG_SUCCESS != HG_Handler_start_output(op_data->hg_handle, &ret_value))
        HDONE_ERROR2(H5E_SYM, H5E_CANTDEC, FAIL, "can't send result of file close to client");

    input = (file_close_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

    FUNC_LEAVE_NOAPI_VOID
} /* end H5VL_iod_server_file_close_cb() */

#endif /* H5_HAVE_EFF */
