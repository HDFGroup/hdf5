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
#include "H5RCpublic.h"
#include "H5TRpublic.h"

#ifdef H5_HAVE_EFF

#if 0
static void check_corruptions(iod_trans_id_t trans_num);
#endif

/*
 * Programmer:  Mohamad Chaarawi <chaarawi@hdfgroup.gov>
 *              July, 2013
 *
 * Purpose:	The IOD plugin server side transaction and read context routines.
 */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_rcxt_acquire_cb
 *
 * Purpose:	Acquire a read context of a container using a version number.
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
H5VL_iod_server_rcxt_acquire_cb(AXE_engine_t UNUSED axe_engine, 
                                size_t UNUSED num_n_parents, AXE_task_t UNUSED n_parents[], 
                                size_t UNUSED num_s_parents, AXE_task_t UNUSED s_parents[], 
                                void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    rc_acquire_in_t *input = (rc_acquire_in_t *)op_data->input;
    rc_acquire_out_t output;
    iod_handle_t coh = input->coh; /* the container handle */
    iod_trans_id_t c_version = input->c_version;
    iod_trans_id_t acquired_version;
    hid_t rcapl_id;
    iod_ret_t ret;
    H5RC_request_t acquire_req;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    if(H5P_DEFAULT == input->rcapl_id)
        input->rcapl_id = H5Pcopy(H5P_RC_ACQUIRE_DEFAULT);
    rcapl_id = input->rcapl_id;

    if(H5Pget_rcapl_version_request(rcapl_id, &acquire_req) < 0) {
        HGOTO_ERROR2(H5E_PLIST, H5E_CANTSET,FAIL, "can't get acquire request property");
    }

    switch(acquire_req) {
    case H5RC_EXACT:
#if H5VL_IOD_DEBUG
        fprintf(stderr, "Exact Acquire Read Context %"PRIu64"\n", input->c_version);
#endif
        if((ret = iod_trans_start(coh, &c_version, NULL, 0, IOD_TRANS_R, NULL)) < 0) {
            fprintf(stderr, "can't acquire read context. %d (%s).\n", ret, strerror(-ret));
            HGOTO_ERROR2(H5E_SYM, H5E_CANTINIT, FAIL, "can't acquire read context");
        }
        acquired_version = c_version;
        break;
    case H5RC_LAST:
#if H5VL_IOD_DEBUG
        fprintf(stderr, "Acquire LAST Read Context\n");
#endif
        c_version = IOD_TID_UNKNOWN;
        if(iod_trans_start(coh, &c_version, NULL, 0, IOD_TRANS_R, NULL) < 0)
            HGOTO_ERROR2(H5E_SYM, H5E_CANTINIT, FAIL, "can't acquire read context");
        acquired_version = c_version;
        break;
    case H5RC_NEXT:
        {
            iod_cont_trans_stat_t *tids;
            uint64_t u;

#if H5VL_IOD_DEBUG
            fprintf(stderr, "Next Acquire Read Context %"PRIu64"\n", input->c_version);
#endif
            if(iod_query_cont_trans_stat(coh, &tids, NULL) < 0)
                HGOTO_ERROR2(H5E_SYM, H5E_CANTGET, FAIL, "can't get container tids status");

            acquired_version = IOD_TID_UNKNOWN;

            for(u=c_version; u<tids->latest_rdable ; u++) {
                if(iod_trans_start(coh, &u, NULL, 0, IOD_TRANS_R, NULL) < 0)
                    continue;
                acquired_version = u;
                break;
            }

            if(IOD_TID_UNKNOWN == acquired_version) {
                HGOTO_ERROR2(H5E_SYM, H5E_CANTGET, FAIL, 
                            "can't get a read version");
            }

            if(iod_free_cont_trans_stat(coh, tids) < 0)
                HGOTO_ERROR2(H5E_SYM, H5E_CANTINIT, FAIL, "can't free container transaction status object");
            break;
        }
    case H5RC_PREV:
        {
            iod_cont_trans_stat_t *tids;
            uint64_t u;

#if H5VL_IOD_DEBUG
            fprintf(stderr, "Next Acquire Read Context %"PRIu64"\n", input->c_version);
#endif
            if(iod_query_cont_trans_stat(coh, &tids, NULL) < 0)
                HGOTO_ERROR2(H5E_SYM, H5E_CANTGET, FAIL, "can't get container tids status");

            if(c_version >= tids->latest_rdable) {
                acquired_version = tids->latest_rdable;
                if(iod_trans_start(coh, &acquired_version, NULL, 0, IOD_TRANS_R, NULL) < 0)
                    HGOTO_ERROR2(H5E_SYM, H5E_CANTGET, FAIL, "can't acquire read context");
                break;
            }

            acquired_version = IOD_TID_UNKNOWN;
            u=c_version;

            for(u=c_version; u>0; u--) {
                if(iod_trans_start(coh, &u, NULL, 0, IOD_TRANS_R, NULL) < 0)
                    continue;
                acquired_version = u;
                break;
            }

            if(IOD_TID_UNKNOWN == acquired_version) {
                HGOTO_ERROR2(H5E_SYM, H5E_CANTGET, FAIL, 
                            "can't get a read version");
            }

            if(iod_free_cont_trans_stat(coh, tids) < 0)
                HGOTO_ERROR2(H5E_SYM, H5E_CANTINIT, FAIL, "can't free container transaction status object");

            break;
        }
    default:
        HGOTO_ERROR2(H5E_PLIST, H5E_CANTSET, FAIL, "invalid acquire request");
    }

    output.c_version = acquired_version;
    output.ret = ret_value;

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Done with Acquire Read Context. Acquired %"PRIu64"\n", output.c_version);
#endif

    HG_Handler_start_output(op_data->hg_handle, &output);

done:
    /* return an UNDEFINED oh to the client if the operation failed */
    if(ret_value < 0) {
        fprintf(stderr, "Failed to Acquire Read context\n");
        output.ret = FAIL;
        output.c_version = c_version;
        HG_Handler_start_output(op_data->hg_handle, &output);
    }

    input = (rc_acquire_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

    FUNC_LEAVE_NOAPI_VOID
} /* end H5VL_iod_server_rcxt_acquire_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_rcxt_release_cb
 *
 * Purpose:	Release a read context of a container using a version number.
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
H5VL_iod_server_rcxt_release_cb(AXE_engine_t UNUSED axe_engine, 
                                size_t UNUSED num_n_parents, AXE_task_t UNUSED n_parents[], 
                                size_t UNUSED num_s_parents, AXE_task_t UNUSED s_parents[], 
                                void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    rc_release_in_t *input = (rc_release_in_t *)op_data->input;
    iod_handle_t coh = input->coh; /* the container handle */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Release Read Context %"PRIu64"\n", input->c_version);
#endif

    if(iod_trans_finish(coh, input->c_version, NULL, 0, NULL) < 0)
        HGOTO_ERROR2(H5E_SYM, H5E_CANTSET, FAIL, "can't release Read Context");

done:
    if(HG_SUCCESS != HG_Handler_start_output(op_data->hg_handle, &ret_value))
        fprintf(stderr, "Failed to Release Read context\n");

    input = (rc_release_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

    FUNC_LEAVE_NOAPI_VOID
} /* end H5VL_iod_server_rcxt_release_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_rcxt_persist_cb
 *
 * Purpose:	Persist a read context of a container using a version number.
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
H5VL_iod_server_rcxt_persist_cb(AXE_engine_t UNUSED axe_engine, 
                                size_t UNUSED num_n_parents, AXE_task_t UNUSED n_parents[], 
                                size_t UNUSED num_s_parents, AXE_task_t UNUSED s_parents[], 
                                void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    rc_persist_in_t *input = (rc_persist_in_t *)op_data->input;
    iod_handle_t coh = input->coh; /* the container handle */  
    char *persist = NULL, *sleep_timer = NULL;
    int num_persist_retry = 0, i;
    iod_trans_id_t tid = input->c_version;
    int sleep_t = 0;
    iod_ret_t ret;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Persist Read Context %"PRIu64"\n", tid);
#endif

    persist = getenv ("H5ENV_NUM_PERSIST_RETRY");
    sleep_timer = getenv ("H5ENV_SLEEP_PERSIST_RETRY");

    if(NULL != persist)
        num_persist_retry = atoi(persist);

    if(NULL != sleep_timer)
        sleep_t = atoi(sleep_timer);

    ret = iod_trans_persist(coh, tid, NULL, NULL);
    if(ret != 0) {
        fprintf(stderr, "%d (%s).\n", ret, strerror(-ret));
    }

    if(ret != 0 && -ESHUTDOWN != ret) {
        for(i=0 ; i<num_persist_retry; i++) {
            fprintf(stderr, "Retry Persist # %d on %"PRIu64".\n", i+1, tid);
            ret = iod_trans_persist(coh, tid, NULL, NULL);
            if(0 == ret) {
                break;
            }
            else if(-ESHUTDOWN == ret) {
                fprintf(stderr, "%d (%s).\n", ret, strerror(-ret));
                HGOTO_ERROR2(H5E_SYM, H5E_CANTINIT, FAIL, "can't persist read context");
            }
            else {
                fprintf(stderr, "Retry failed.. %d (%s).\n", ret, strerror(-ret));
                if(sleep_timer)
                    sleep(sleep_t);
            }
        }
    }

    if(ret != 0) {
        HGOTO_ERROR2(H5E_SYM, H5E_CANTINIT, FAIL, "can't persist read context");
    }

done:
    if(HG_SUCCESS != HG_Handler_start_output(op_data->hg_handle, &ret_value))
        fprintf(stderr, "Failed to Persist Read context\n");

    input = (rc_persist_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

    FUNC_LEAVE_NOAPI_VOID
} /* end H5VL_iod_server_rcxt_persist_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_rcxt_snapshot_cb
 *
 * Purpose:	Snapshot a read context of a container using a version number.
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
H5VL_iod_server_rcxt_snapshot_cb(AXE_engine_t UNUSED axe_engine, 
                                 size_t UNUSED num_n_parents, AXE_task_t UNUSED n_parents[], 
                                 size_t UNUSED num_s_parents, AXE_task_t UNUSED s_parents[], 
                                 void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    rc_snapshot_in_t *input = (rc_snapshot_in_t *)op_data->input;
    iod_handle_t coh = input->coh; /* the container handle */    
    iod_trans_id_t tid = input->c_version;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Snapshot %s with Read Context %"PRIu64"\n", input->snapshot_name, input->c_version);
#endif

    /* MSC - can only snapshot latest version */
    if(iod_container_snapshot(coh, tid, input->snapshot_name, NULL, NULL) < 0)
        HGOTO_ERROR2(H5E_SYM, H5E_CANTSET, FAIL, "can't snapshot Read Context");

done:
    if(HG_SUCCESS != HG_Handler_start_output(op_data->hg_handle, &ret_value))
        fprintf(stderr, "Failed to Snapshot Read context\n");

    input = (rc_snapshot_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

    FUNC_LEAVE_NOAPI_VOID
} /* end H5VL_iod_server_rcxt_snapshot_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_trans_start_cb
 *
 * Purpose:	Start a transaction on a container.
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
H5VL_iod_server_trans_start_cb(AXE_engine_t UNUSED axe_engine, 
                               size_t UNUSED num_n_parents, AXE_task_t UNUSED n_parents[], 
                               size_t UNUSED num_s_parents, AXE_task_t UNUSED s_parents[], 
                               void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    tr_start_in_t *input = (tr_start_in_t *)op_data->input;
    iod_handle_t coh = input->coh; /* the container handle */
    hid_t trspl_id;
    iod_trans_id_t trans_num = input->trans_num;    
    unsigned num_peers; /* the number of peers starting this transaction */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Transaction Start %"PRIu64"\n", input->trans_num);
#endif

    if(H5P_DEFAULT == input->trspl_id)
        input->trspl_id = H5Pcopy(H5P_TR_START_DEFAULT);
    trspl_id = input->trspl_id;

    if(H5Pget_trspl_num_peers(trspl_id, &num_peers) < 0)
        HGOTO_ERROR2(H5E_PLIST, H5E_CANTGET, FAIL, "can't get acquire request property");

    if(iod_trans_start(coh, &trans_num, NULL, num_peers, IOD_TRANS_W, NULL) < 0)
        HGOTO_ERROR2(H5E_SYM, H5E_CANTSET, FAIL, "can't start transaction");

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Done with Transaction Start\n");
#endif

done:
    if(HG_SUCCESS != HG_Handler_start_output(op_data->hg_handle, &ret_value))
        fprintf(stderr, "Failed to Start Transaction\n");

    input = (tr_start_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

    FUNC_LEAVE_NOAPI_VOID
} /* end H5VL_iod_server_trans_start_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_trans_finish_cb
 *
 * Purpose:	Finish a transaction on a container.
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
H5VL_iod_server_trans_finish_cb(AXE_engine_t UNUSED axe_engine, 
                                size_t UNUSED num_n_parents, AXE_task_t UNUSED n_parents[], 
                                size_t UNUSED num_s_parents, AXE_task_t UNUSED s_parents[], 
                                void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    tr_finish_in_t *input = (tr_finish_in_t *)op_data->input;
    iod_handle_t coh = input->coh; /* the container handle */
    uint32_t cs_scope = input->cs_scope;
    //hid_t trfpl_id;
    iod_trans_id_t trans_num = input->trans_num;
    hbool_t acquire = input->acquire;
    uint32_t client_rank = input->client_rank;
    iod_obj_id_t oidkv_id = input->oidkv_id;
    iod_handle_t oidkv_oh;
    uint64_t oid_index[3];
    iod_kv_t kv;
    iod_ret_t ret;
    int step = 0;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Transaction Finish %"PRIu64"\n", trans_num);
#endif

    oid_index[0] = input->kv_oid_index;
    oid_index[1] = input->array_oid_index;
    oid_index[2] = input->blob_oid_index;

    ret = iod_obj_open_write(coh, oidkv_id, trans_num, NULL, &oidkv_oh, NULL);
    if(ret != 0)
        HGOTO_ERROR_IOD(ret, FAIL, "can't open oid KV");

    step ++;

    kv.value = &oid_index;
    kv.value_len = sizeof(iod_obj_id_t) * 3;
    kv.key = &client_rank;
    kv.key_len = sizeof(uint32_t);

    if(cs_scope & H5_CHECKSUM_IOD) {
        iod_checksum_t cs[2];

        cs[0] = H5_checksum_crc64(kv.key, kv.key_len);
        cs[1] = H5_checksum_crc64(kv.value, kv.value_len);
        if (iod_kv_set(oidkv_oh, trans_num, NULL, &kv, cs, NULL) < 0)
            HGOTO_ERROR2(H5E_SYM, H5E_CANTINIT, FAIL, "can't set KV pair in parent");
    }
    else {
        if (iod_kv_set(oidkv_oh, trans_num, NULL, &kv, NULL, NULL) < 0)
            HGOTO_ERROR2(H5E_SYM, H5E_CANTINIT, FAIL, "can't set KV pair in oid KV");
    }

    if(iod_obj_close(oidkv_oh, NULL, NULL) < 0)
        HGOTO_ERROR2(H5E_SYM, H5E_CANTINIT, FAIL, "can't close object handle");

    step --;

    /* Finish  the transaction */
    if((ret = iod_trans_finish(coh, trans_num, NULL, 0, NULL)) < 0) {
        fprintf(stderr, "can't finish transaction %d (%s).\n", ret, strerror(-ret));
        HGOTO_ERROR2(H5E_SYM, H5E_CANTSET, FAIL, "can't finish transaction");
    }

#if 0
    if(0 == client_rank)
        check_corruptions(trans_num);
#endif

    /* if the flag is true, acquire a read context on the finished transaction */
    if(TRUE == acquire) {
#if H5VL_IOD_DEBUG
        fprintf(stderr, "Transaction Acquire after Finish %"PRIu64"\n", trans_num);
#endif

        if(iod_trans_start(coh, &trans_num, NULL, 0, IOD_TRANS_R, NULL) < 0)
            HGOTO_ERROR2(H5E_SYM, H5E_CANTINIT, FAIL, "can't acquire read context");
    }

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Done with Transaction Finish\n");
#endif

done:
    if(HG_SUCCESS != HG_Handler_start_output(op_data->hg_handle, &ret_value))
        fprintf(stderr, "Failed to Finish Transaction\n");

    if(step == 1) {
        iod_obj_close(oidkv_oh, NULL, NULL);
    }

    input = (tr_finish_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

    FUNC_LEAVE_NOAPI_VOID
} /* end H5VL_iod_server_trans_finish_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_trans_set_dependency_cb
 *
 * Purpose:	Set Dependency between 2 transactions on a container.
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
H5VL_iod_server_trans_set_dependency_cb(AXE_engine_t UNUSED axe_engine, 
                                size_t UNUSED num_n_parents, AXE_task_t UNUSED n_parents[], 
                                size_t UNUSED num_s_parents, AXE_task_t UNUSED s_parents[], 
                                void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    tr_set_depend_in_t *input = (tr_set_depend_in_t *)op_data->input;
    iod_handle_t coh = input->coh; /* the container handle */
    iod_trans_depend_desc_t depends;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Transaction Set_Dependency %"PRIu64" on %"PRIu64"\n", 
            input->child_trans_num, input->parent_trans_num);
#endif

    /* MSC - set depends */

    //if(iod_trans_depend(coh, depends, NULL) < 0)
    //HGOTO_ERROR2(H5E_SYM, H5E_CANTINIT, FAIL, "can't set dependency between transactions");

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Done with Transaction Set_Dependency\n");
#endif

done:
    if(HG_SUCCESS != HG_Handler_start_output(op_data->hg_handle, &ret_value))
        fprintf(stderr, "Failed to Set_Dependency between Transactions\n");

    input = (tr_set_depend_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

    FUNC_LEAVE_NOAPI_VOID
} /* end H5VL_iod_server_trans_set_dependency_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_trans_skip_cb
 *
 * Purpose:	Skip a number of transactions on a container.
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
H5VL_iod_server_trans_skip_cb(AXE_engine_t UNUSED axe_engine, 
                                size_t UNUSED num_n_parents, AXE_task_t UNUSED n_parents[], 
                                size_t UNUSED num_s_parents, AXE_task_t UNUSED s_parents[], 
                                void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    tr_skip_in_t *input = (tr_skip_in_t *)op_data->input;
    iod_handle_t coh = input->coh; /* the container handle */
    iod_trans_id_t start_trans_num = input->start_trans_num;
    uint64_t count = input->count;
    iod_trans_range_desc_t skip_ranges;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Transaction Skip %"PRIu64" starting at %"PRIu64"\n", count, start_trans_num);
#endif

    /* MSC - set skip ranges */
    skip_ranges.n_range = 1;
    //if(iod_trans_skip(coh, skip_ranges, NULL) < 0)
    //HGOTO_ERROR2(H5E_SYM, H5E_CANTINIT, FAIL, "can't skip transactions");

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Done with Transaction Skip\n");
#endif

done:
    if(HG_SUCCESS != HG_Handler_start_output(op_data->hg_handle, &ret_value))
        fprintf(stderr, "Failed to Skip Transaction\n");

    input = (tr_skip_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

    FUNC_LEAVE_NOAPI_VOID
} /* end H5VL_iod_server_trans_skip_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_trans_abort_cb
 *
 * Purpose:	Abort a transaction on a container.
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
H5VL_iod_server_trans_abort_cb(AXE_engine_t UNUSED axe_engine, 
                                size_t UNUSED num_n_parents, AXE_task_t UNUSED n_parents[], 
                                size_t UNUSED num_s_parents, AXE_task_t UNUSED s_parents[], 
                                void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    tr_abort_in_t *input = (tr_abort_in_t *)op_data->input;
    iod_handle_t coh = input->coh; /* the container handle */
    iod_trans_id_t trans_num = input->trans_num;
    iod_ret_t ret;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Aborting Transaction %"PRIu64"\n", input->trans_num);
#endif

    ret = iod_trans_finish(coh, trans_num, NULL, IOD_TRANS_ABORT_DEPENDENT, NULL);
    if(ret == -IOD_EC_TRANS_DISCARDED)
        fprintf(stderr, "Transaction %"PRIu64" already discarded\n", input->trans_num);
    else if(ret < 0) {
        fprintf(stderr, "%d (%s).\n", ret, strerror(-ret));
        HGOTO_ERROR2(H5E_SYM, H5E_CANTSET, FAIL, "can't abort transaction");
    }

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Done with Transaction Abort\n");
#endif

done:
    if(HG_SUCCESS != HG_Handler_start_output(op_data->hg_handle, &ret_value))
        fprintf(stderr, "Failed to Abort Transaction\n");

    input = (tr_abort_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

    FUNC_LEAVE_NOAPI_VOID
} /* end H5VL_iod_server_trans_abort_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_prefetch_cb
 *
 * Purpose:	prefetch an object from central storage to BB.
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              February 2014
 *
 *-------------------------------------------------------------------------
 */
void
H5VL_iod_server_prefetch_cb(AXE_engine_t UNUSED axe_engine, 
                            size_t UNUSED num_n_parents, AXE_task_t UNUSED n_parents[], 
                            size_t UNUSED num_s_parents, AXE_task_t UNUSED s_parents[], 
                            void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    prefetch_in_t *input = (prefetch_in_t *)op_data->input;
    //iod_handle_t coh = input->coh; /* the container handle */
    iod_trans_id_t tid = input->rcxt_num;
    iod_handles_t iod_oh = input->iod_oh; /* object handle */
    iod_obj_id_t iod_id = input->iod_id; /* OID */
    //H5I_type_t obj_type = input->obj_type;
    //hid_t apl_id = input->apl_id;
    iod_trans_id_t replica_id;
    iod_ret_t ret;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Prefetch Object (OID %"PRIx64" OH %"PRIu64") at Version %"PRIu64"\n", 
            iod_id, iod_oh.rd_oh.cookie, tid);
#endif

    ret = iod_obj_fetch(iod_oh.rd_oh, tid, NULL, NULL, NULL, &replica_id, NULL);
    if(ret != 0) {
        fprintf(stderr, "%d (%s).\n", ret, strerror(-ret));
        HGOTO_ERROR2(H5E_SYM, H5E_CANTGET, FAIL, "can't prefetch object");
    }

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Done with Prefetch\n");
#endif

done:
    if(SUCCEED != ret_value)
        replica_id = 0;

    if(HG_SUCCESS != HG_Handler_start_output(op_data->hg_handle, &replica_id))
        fprintf(stderr, "Failed to Prefetch Object\n");

    input = (prefetch_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

    FUNC_LEAVE_NOAPI_VOID
} /* end H5VL_iod_server_prefetch_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_evict_cb
 *
 * Purpose:	evicts an object from BB.
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              February 2014
 *
 *-------------------------------------------------------------------------
 */
void
H5VL_iod_server_evict_cb(AXE_engine_t UNUSED axe_engine, 
                            size_t UNUSED num_n_parents, AXE_task_t UNUSED n_parents[], 
                            size_t UNUSED num_s_parents, AXE_task_t UNUSED s_parents[], 
                            void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    evict_in_t *input = (evict_in_t *)op_data->input;
    //iod_handle_t coh = input->coh; /* the container handle */
    iod_trans_id_t tid = input->rcxt_num;
    iod_handles_t iod_oh = input->iod_oh; /* object handle */
    iod_obj_id_t iod_id = input->iod_id; /* OID */
    //H5I_type_t obj_type = input->obj_type;
    //hid_t apl_id = input->apl_id;
    iod_trans_id_t replica_id = input->replica_id;
    iod_ret_t ret;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    if(replica_id) {
        fprintf(stderr, "Evict Object (OID %"PRIx64" OH %"PRIu64") replica tag %"PRIx64"\n", 
                iod_id, iod_oh.rd_oh.cookie, replica_id);
        ret = iod_obj_purge(iod_oh.rd_oh, replica_id, NULL, NULL);
    }
    else {
        fprintf(stderr, "Evict Object (OID %"PRIx64" OH %"PRIu64") at Version %"PRIu64"\n", 
                iod_id, iod_oh.rd_oh.cookie, tid);
        ret = iod_obj_purge(iod_oh.rd_oh, tid, NULL, NULL);
    }

    if(ret < 0) {
        fprintf(stderr, "%d (%s).\n", ret, strerror(-ret));
        HGOTO_ERROR2(H5E_SYM, H5E_CANTGET, FAIL, "can't evict object");
    }

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Done with Evict\n");
#endif

done:
    if(HG_SUCCESS != HG_Handler_start_output(op_data->hg_handle, &ret_value))
        fprintf(stderr, "Failed to Evict Object\n");

    input = (evict_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

    FUNC_LEAVE_NOAPI_VOID
} /* end H5VL_iod_server_evict_cb() */

#if 0
static void check_corruptions(iod_trans_id_t trans_num)
{
    int step;
    char *cor_step = NULL;
    char *num_procs_s = NULL;
    int num_procs = -1;
    char *cor_data_s = NULL;
    int cor_data = -1;
    int ret;

    num_procs_s = getenv ("H5ENV_NUM_CLIENTS");
    if(NULL != num_procs_s)
        num_procs = atoi(num_procs_s);
    else
        return;

    cor_data_s = getenv ("H5ENV_CORRUPT_DATA");
    if(NULL != cor_data_s)
        cor_data = atoi(cor_data_s);
    else
        return;

    cor_step = getenv ("H5ENV_STEP_CORRUPT_DSET");
    if(NULL != cor_step) {
        step = atoi(cor_step);

        if((int)trans_num == step+3) {
            uint64_t oid;

            oid = step*8*num_procs + 8;
            IOD_OBJID_SETOWNER_APP(oid)
            IOD_OBJID_SETTYPE(oid, IOD_OBJ_ARRAY)
                
            printf("CORRUPTING at VPIC step %d at trans_num %d, array ID %"PRIx64"\n", 
                   step, (int)trans_num, oid);

            ret = corrupt_data("eff_vpic", oid, trans_num, 5, cor_data);
            if(ret < 0) {
                fprintf(stderr, "cant't corrupt data. %d (%s).\n", ret, strerror(-ret));
            }
        }
    }

    cor_step = NULL;
    step = -1;

    cor_step = getenv ("H5ENV_STEP_CORRUPT_DTYPE");
    if(NULL != cor_step) {
        step = atoi(cor_step);

        if((int)trans_num == step+3) {
            uint64_t oid;

            oid = step*num_procs;
            IOD_OBJID_SETOWNER_APP(oid)
            IOD_OBJID_SETTYPE(oid, IOD_OBJ_BLOB)
                
            printf("CORRUPTING at VPIC step %d at trans_num %d, blob ID %"PRIx64"\n", 
                   step, (int)trans_num, oid);

            ret = corrupt_data("eff_vpic", oid, trans_num, 5, cor_data);
            if(ret < 0) {
                fprintf(stderr, "cant't corrupt data. %d (%s).\n", ret, strerror(-ret));
            }
        }
    }

    cor_step = NULL;
    step = -1;

    cor_step = getenv ("H5ENV_STEP_CORRUPT_GROUP");
    if(NULL != cor_step) {
        step = atoi(cor_step);

        if((int)trans_num == step+3) {
            uint64_t oid;

            oid = 5*num_procs + step*21*num_procs + 3;
            IOD_OBJID_SETOWNER_APP(oid)
            IOD_OBJID_SETTYPE(oid, IOD_OBJ_KV)
                
            printf("CORRUPTING at VPIC step %d at trans_num %d, kv ID %"PRIx64"\n", 
                   step, (int)trans_num, oid);

            ret = corrupt_kv("eff_vpic", oid, trans_num, 1, cor_data);
            if(ret < 0) {
                fprintf(stderr, "cant't corrupt data. %d (%s).\n", ret, strerror(-ret));
            }
        }
    }
}
#endif

#endif /* H5_HAVE_EFF */
