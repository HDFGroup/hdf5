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

#if H5_HAVE_IOD_CORRUPT_TOOL
static void check_ion_corruptions(iod_trans_id_t trans_num);
static void check_daos_corruptions(iod_hint_list_t *chint, iod_trans_id_t trans_num);
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
H5VL_iod_server_rcxt_acquire_cb(AXE_engine_t H5_ATTR_UNUSED axe_engine, 
                                size_t H5_ATTR_UNUSED num_n_parents, AXE_task_t H5_ATTR_UNUSED n_parents[], 
                                size_t H5_ATTR_UNUSED num_s_parents, AXE_task_t H5_ATTR_UNUSED s_parents[], 
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

    if(H5P_DEFAULT == input->rcapl_id)
        input->rcapl_id = H5Pcopy(H5P_RC_ACQUIRE_DEFAULT);
    rcapl_id = input->rcapl_id;

    if(H5Pget_rcapl_version_request(rcapl_id, &acquire_req) < 0) {
        HGOTO_ERROR_FF(FAIL, "can't get acquire request property");
    }

    switch(acquire_req) {
    case H5RC_EXACT:
#if H5_EFF_DEBUG
        fprintf(stderr, "Exact Acquire Read Context %"PRIu64"\n", input->c_version);
#endif
        fprintf(stderr, "Exact Acquire Read Context %"PRIu64"\n", input->c_version);
        if((ret = iod_trans_start(coh, &c_version, NULL, 0, IOD_TRANS_R, NULL)) < 0) {
            HGOTO_ERROR_FF(ret, "can't acquire read context");
        }
        acquired_version = c_version;
        break;
    case H5RC_LAST:
#if H5_EFF_DEBUG
        fprintf(stderr, "Acquire LAST Read Context\n");
#endif
        c_version = IOD_TID_UNKNOWN;
        if((ret = iod_trans_start(coh, &c_version, NULL, 0, IOD_TRANS_R, NULL)) < 0)
            HGOTO_ERROR_FF(ret, "can't acquire read context");
        acquired_version = c_version;
        break;
    case H5RC_NEXT:
        {
            iod_cont_trans_stat_t *tids;
            uint64_t u;

#if H5_EFF_DEBUG
            fprintf(stderr, "Next Acquire Read Context %"PRIu64"\n", input->c_version);
#endif
            ret = iod_query_cont_trans_stat(coh, &tids, NULL);
            if(ret < 0)
                HGOTO_ERROR_FF(ret, "can't get container tids status");

            acquired_version = IOD_TID_UNKNOWN;

            for(u=c_version; u<tids->latest_rdable ; u++) {
                if(iod_trans_start(coh, &u, NULL, 0, IOD_TRANS_R, NULL) < 0)
                    continue;
                acquired_version = u;
                break;
            }

            if(IOD_TID_UNKNOWN == acquired_version) {
                HGOTO_ERROR_FF(FAIL, 
                            "can't get a read version");
            }

            if(iod_free_cont_trans_stat(coh, tids) < 0)
                HGOTO_ERROR_FF(FAIL, "can't free container transaction status object");
            break;
        }
    case H5RC_PREV:
        {
            iod_cont_trans_stat_t *tids;
            uint64_t u;

#if H5_EFF_DEBUG
            fprintf(stderr, "Next Acquire Read Context %"PRIu64"\n", input->c_version);
#endif
            if(iod_query_cont_trans_stat(coh, &tids, NULL) < 0)
                HGOTO_ERROR_FF(FAIL, "can't get container tids status");

            if(c_version >= tids->latest_rdable) {
                acquired_version = tids->latest_rdable;
                ret = iod_trans_start(coh, &acquired_version, NULL, 0, IOD_TRANS_R, NULL);
                if(ret < 0)
                    HGOTO_ERROR_FF(ret, "can't acquire read context");
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
                HGOTO_ERROR_FF(FAIL, 
                            "can't get a read version");
            }

            if(iod_free_cont_trans_stat(coh, tids) < 0)
                HGOTO_ERROR_FF(FAIL, "can't free container transaction status object");

            break;
        }
    default:
        HGOTO_ERROR_FF(FAIL, "invalid acquire request");
    }

    output.c_version = acquired_version;
    output.ret = ret_value;

#if H5_EFF_DEBUG
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

    HG_Handler_free_input(op_data->hg_handle, input);
    HG_Handler_free(op_data->hg_handle);
    input = (rc_acquire_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

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
H5VL_iod_server_rcxt_release_cb(AXE_engine_t H5_ATTR_UNUSED axe_engine, 
                                size_t H5_ATTR_UNUSED num_n_parents, AXE_task_t H5_ATTR_UNUSED n_parents[], 
                                size_t H5_ATTR_UNUSED num_s_parents, AXE_task_t H5_ATTR_UNUSED s_parents[], 
                                void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    rc_release_in_t *input = (rc_release_in_t *)op_data->input;
    iod_handle_t coh = input->coh; /* the container handle */
    iod_ret_t ret;
    herr_t ret_value = SUCCEED;

#if H5_EFF_DEBUG
    fprintf(stderr, "Release Read Context %"PRIu64"\n", input->c_version);
#endif

    ret = iod_trans_finish(coh, input->c_version, NULL, 0, NULL);
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "can't release Read Context");

done:
    if(HG_SUCCESS != HG_Handler_start_output(op_data->hg_handle, &ret_value))
        fprintf(stderr, "Failed to Release Read context\n");

    HG_Handler_free_input(op_data->hg_handle, input);
    HG_Handler_free(op_data->hg_handle);
    input = (rc_release_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

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
H5VL_iod_server_rcxt_persist_cb(AXE_engine_t H5_ATTR_UNUSED axe_engine, 
                                size_t H5_ATTR_UNUSED num_n_parents, AXE_task_t H5_ATTR_UNUSED n_parents[], 
                                size_t H5_ATTR_UNUSED num_s_parents, AXE_task_t H5_ATTR_UNUSED s_parents[], 
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
    iod_hint_list_t *chint = NULL;
    herr_t ret_value = SUCCEED;

#if H5_EFF_DEBUG
    fprintf(stderr, "Persist Read Context %"PRIu64"\n", tid);
#endif

    persist = getenv ("H5ENV_NUM_PERSIST_RETRY");
    sleep_timer = getenv ("H5ENV_SLEEP_PERSIST_RETRY");

    if(NULL != persist)
        num_persist_retry = atoi(persist);

    if(NULL != sleep_timer)
        sleep_t = atoi(sleep_timer);

#if H5_HAVE_IOD_CORRUPT_TOOL
    chint = (iod_hint_list_t *)malloc(sizeof(iod_hint_list_t) + 9*sizeof(iod_hint_t));
    chint->num_hint = 0;
    check_daos_corruptions(chint, tid);

    if(chint != NULL && 0 == chint->num_hint) {
        free (chint);
        chint = NULL;
    }
#endif

    ret = iod_trans_persist(coh, tid, chint, NULL);
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
                HGOTO_ERROR_FF(FAIL, "can't persist read context");
            }
            else {
                fprintf(stderr, "Retry failed.. %d (%s).\n", ret, strerror(-ret));
                if(sleep_timer)
                    sleep(sleep_t);
            }
        }
    }

    if(ret != 0) {
        HGOTO_ERROR_FF(ret, "can't persist read context");
    }

#if H5_HAVE_IOD_CORRUPT_TOOL
    if(chint != NULL) {
        for(i=0 ; i<chint->num_hint ; i++) {
            if(chint->hint[i].key) {
                free(chint->hint[i].key);
                chint->hint[i].key = NULL;
            }
            if(chint->hint[i].value) {
                free(chint->hint[i].value);
                chint->hint[i].value = NULL;
            }
        }
        free(chint);
    }
#endif

done:
    if(HG_SUCCESS != HG_Handler_start_output(op_data->hg_handle, &ret_value))
        fprintf(stderr, "Failed to Persist Read context\n");

    HG_Handler_free_input(op_data->hg_handle, input);
    HG_Handler_free(op_data->hg_handle);
    input = (rc_persist_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

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
H5VL_iod_server_rcxt_snapshot_cb(AXE_engine_t H5_ATTR_UNUSED axe_engine, 
                                 size_t H5_ATTR_UNUSED num_n_parents, AXE_task_t H5_ATTR_UNUSED n_parents[], 
                                 size_t H5_ATTR_UNUSED num_s_parents, AXE_task_t H5_ATTR_UNUSED s_parents[], 
                                 void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    rc_snapshot_in_t *input = (rc_snapshot_in_t *)op_data->input;
    iod_handle_t coh = input->coh; /* the container handle */    
    iod_trans_id_t tid = input->c_version;
    iod_ret_t ret;
    herr_t ret_value = SUCCEED;

#if H5_EFF_DEBUG
    fprintf(stderr, "Snapshot %s with Read Context %"PRIu64"\n", input->snapshot_name, input->c_version);
#endif

    /* MSC - can only snapshot latest version */
    ret = iod_container_snapshot(coh, tid, input->snapshot_name, NULL, NULL);
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "can't snapshot Read Context");

done:
    if(HG_SUCCESS != HG_Handler_start_output(op_data->hg_handle, &ret_value))
        fprintf(stderr, "Failed to Snapshot Read context\n");

    HG_Handler_free_input(op_data->hg_handle, input);
    HG_Handler_free(op_data->hg_handle);
    input = (rc_snapshot_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

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
H5VL_iod_server_trans_start_cb(AXE_engine_t H5_ATTR_UNUSED axe_engine, 
                               size_t H5_ATTR_UNUSED num_n_parents, AXE_task_t H5_ATTR_UNUSED n_parents[], 
                               size_t H5_ATTR_UNUSED num_s_parents, AXE_task_t H5_ATTR_UNUSED s_parents[], 
                               void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    tr_start_in_t *input = (tr_start_in_t *)op_data->input;
    iod_handle_t coh = input->coh; /* the container handle */
    hid_t trspl_id;
    iod_trans_id_t trans_num = input->trans_num;    
    unsigned num_peers; /* the number of peers starting this transaction */
    iod_ret_t ret;
    herr_t ret_value = SUCCEED;

#if H5_EFF_DEBUG
    fprintf(stderr, "Transaction Start %"PRIu64"\n", input->trans_num);
#endif

    if(H5P_DEFAULT == input->trspl_id)
        input->trspl_id = H5Pcopy(H5P_TR_START_DEFAULT);
    trspl_id = input->trspl_id;

    if(H5Pget_trspl_num_peers(trspl_id, &num_peers) < 0)
        HGOTO_ERROR_FF(FAIL, "can't get acquire request property");

    ret = iod_trans_start(coh, &trans_num, NULL, num_peers, IOD_TRANS_W, NULL);
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "can't start transaction");

#if H5_EFF_DEBUG
    fprintf(stderr, "Done with Transaction Start\n");
#endif

done:
    if(HG_SUCCESS != HG_Handler_start_output(op_data->hg_handle, &ret_value))
        fprintf(stderr, "Failed to Start Transaction\n");

    HG_Handler_free_input(op_data->hg_handle, input);
    HG_Handler_free(op_data->hg_handle);
    input = (tr_start_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

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
H5VL_iod_server_trans_finish_cb(AXE_engine_t H5_ATTR_UNUSED axe_engine, 
                                size_t H5_ATTR_UNUSED num_n_parents, AXE_task_t H5_ATTR_UNUSED n_parents[], 
                                size_t H5_ATTR_UNUSED num_s_parents, AXE_task_t H5_ATTR_UNUSED s_parents[], 
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

#if H5_EFF_DEBUG
    fprintf(stderr, "Transaction Finish %"PRIu64"\n", trans_num);
#endif

    oid_index[0] = input->kv_oid_index;
    oid_index[1] = input->array_oid_index;
    oid_index[2] = input->blob_oid_index;

    ret = iod_obj_open_write(coh, oidkv_id, trans_num, NULL, &oidkv_oh, NULL);
    if(ret != 0)
        HGOTO_ERROR_FF(ret, "can't open oid KV");

    step ++;

    kv.value = &oid_index;
    kv.value_len = sizeof(iod_obj_id_t) * 3;
    kv.key = &client_rank;
    kv.key_len = sizeof(uint32_t);

    if(cs_scope & H5_CHECKSUM_IOD) {
        iod_checksum_t cs[2];

        cs[0] = H5_checksum_crc64(kv.key, kv.key_len);
        cs[1] = H5_checksum_crc64(kv.value, kv.value_len);
        ret = iod_kv_set(oidkv_oh, trans_num, NULL, &kv, cs, NULL);
        if(ret < 0)
            HGOTO_ERROR_FF(ret, "can't set KV pair in parent");
    }
    else {
        ret = iod_kv_set(oidkv_oh, trans_num, NULL, &kv, NULL, NULL);
        if(ret < 0)
            HGOTO_ERROR_FF(ret, "can't set KV pair in oid KV");
    }

    ret = iod_obj_close(oidkv_oh, NULL, NULL);
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "can't close object handle");

    step --;

    /* Finish  the transaction */
    if((ret = iod_trans_finish(coh, trans_num, NULL, 0, NULL)) < 0)
        HGOTO_ERROR_FF(ret, "can't finish transaction");

#if H5_HAVE_IOD_CORRUPT_TOOL
    if(0 == client_rank)
        check_ion_corruptions(trans_num);
#endif

    /* if the flag is true, acquire a read context on the finished transaction */
    if(TRUE == acquire) {
#if H5_EFF_DEBUG
        fprintf(stderr, "Transaction Acquire after Finish %"PRIu64"\n", trans_num);
#endif

        ret = iod_trans_start(coh, &trans_num, NULL, 0, IOD_TRANS_R, NULL);
        if(ret < 0)
            HGOTO_ERROR_FF(ret, "can't acquire read context");
    }

#if H5_EFF_DEBUG
    fprintf(stderr, "Done with Transaction Finish\n");
#endif

done:
    if(HG_SUCCESS != HG_Handler_start_output(op_data->hg_handle, &ret_value))
        fprintf(stderr, "Failed to Finish Transaction\n");

    if(step == 1) {
        iod_obj_close(oidkv_oh, NULL, NULL);
    }

    HG_Handler_free_input(op_data->hg_handle, input);
    HG_Handler_free(op_data->hg_handle);
    input = (tr_finish_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

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
H5VL_iod_server_trans_set_dependency_cb(AXE_engine_t H5_ATTR_UNUSED axe_engine, 
                                size_t H5_ATTR_UNUSED num_n_parents, AXE_task_t H5_ATTR_UNUSED n_parents[], 
                                size_t H5_ATTR_UNUSED num_s_parents, AXE_task_t H5_ATTR_UNUSED s_parents[], 
                                void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    tr_set_depend_in_t *input = (tr_set_depend_in_t *)op_data->input;
    iod_handle_t coh = input->coh; /* the container handle */
    iod_trans_id_t lower = input->parent_trans_num;
    iod_trans_id_t higher = input->child_trans_num;
    iod_trans_depend_desc_t *depends;
    iod_ret_t ret;
    herr_t ret_value = SUCCEED;

#if H5_EFF_DEBUG
    fprintf(stderr, "Transaction Set_Dependency %"PRIu64" on %"PRIu64"\n", 
            input->child_trans_num, input->parent_trans_num);
#endif

    depends = (iod_trans_depend_desc_t *)malloc(sizeof(iod_trans_depend_desc_t) + 
                                                sizeof(iod_trans_range_t));
    depends->n_depend = 1;
    depends->depend[0].lower_tid  = lower;
    depends->depend[0].higher_tid = higher;

    ret = iod_trans_depend(coh, depends, NULL);
    if(ret != 0 && -IOD_EC_TRANS_DISCARDED != ret)
        HGOTO_ERROR_FF(FAIL, "can't set dependency between transactions");

#if H5_EFF_DEBUG
    fprintf(stderr, "Done with Transaction Set_Dependency\n");
#endif

done:
    if(HG_SUCCESS != HG_Handler_start_output(op_data->hg_handle, &ret_value))
        fprintf(stderr, "Failed to Set_Dependency between Transactions\n");

    if(depends) {
        free(depends);
        depends = NULL;
    }

    HG_Handler_free_input(op_data->hg_handle, input);
    HG_Handler_free(op_data->hg_handle);
    input = (tr_set_depend_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

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
H5VL_iod_server_trans_skip_cb(AXE_engine_t H5_ATTR_UNUSED axe_engine, 
                                size_t H5_ATTR_UNUSED num_n_parents, AXE_task_t H5_ATTR_UNUSED n_parents[], 
                                size_t H5_ATTR_UNUSED num_s_parents, AXE_task_t H5_ATTR_UNUSED s_parents[], 
                                void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    tr_skip_in_t *input = (tr_skip_in_t *)op_data->input;
    iod_handle_t coh = input->coh; /* the container handle */
    iod_trans_id_t start_trans_num = input->start_trans_num;
    iod_trans_id_t tid;
    uint64_t count = input->count;
    iod_trans_range_desc_t *skip_ranges = NULL;
    iod_ret_t ret;
    herr_t ret_value = SUCCEED;

#if H5_EFF_DEBUG
    fprintf(stderr, "Transaction Skip %"PRIu64" starting at %"PRIu64"\n", count, start_trans_num);
#endif

    skip_ranges = (iod_trans_range_desc_t *)malloc(sizeof(iod_trans_range_desc_t) + 
                                                   sizeof(iod_trans_range_t));
    skip_ranges->n_range = 1;
    skip_ranges->range[0].lower_tid = start_trans_num;
    skip_ranges->range[0].higher_tid = start_trans_num + count;

    /* MSC - right now, skip by starting and finishing the
       transactions, since iod skip does not update latest_writing */
#if 0 
    if(iod_trans_skip(coh, skip_ranges, NULL) < 0)
        HGOTO_ERROR_FF(FAIL, "can't skip transactions");
#endif
    tid = start_trans_num;
    while(count) {
        ret = iod_trans_start(coh, &tid, NULL, 0, IOD_TRANS_W, NULL);
        if(ret < 0)
            HGOTO_ERROR_FF(ret, "can't start transaction");

        /* Finish  the transaction */
        ret = iod_trans_finish(coh, tid, NULL, IOD_TRANS_ABORT_DEPENDENT, NULL);
        if(ret < 0)
            HGOTO_ERROR_FF(ret, "can't finish transaction");

        tid ++;
        count --;
    }

#if H5_EFF_DEBUG
    fprintf(stderr, "Done with Transaction Skip\n");
#endif

done:
    if(HG_SUCCESS != HG_Handler_start_output(op_data->hg_handle, &ret_value))
        fprintf(stderr, "Failed to Skip Transaction\n");

    HG_Handler_free_input(op_data->hg_handle, input);
    HG_Handler_free(op_data->hg_handle);

    if(skip_ranges) {
        free(skip_ranges);
        skip_ranges = NULL;
    }
    input = (tr_skip_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

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
H5VL_iod_server_trans_abort_cb(AXE_engine_t H5_ATTR_UNUSED axe_engine, 
                                size_t H5_ATTR_UNUSED num_n_parents, AXE_task_t H5_ATTR_UNUSED n_parents[], 
                                size_t H5_ATTR_UNUSED num_s_parents, AXE_task_t H5_ATTR_UNUSED s_parents[], 
                                void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    tr_abort_in_t *input = (tr_abort_in_t *)op_data->input;
    iod_handle_t coh = input->coh; /* the container handle */
    iod_trans_id_t trans_num = input->trans_num;
    iod_ret_t ret;
    herr_t ret_value = SUCCEED;

#if H5_EFF_DEBUG
    fprintf(stderr, "Aborting Transaction %"PRIu64"\n", input->trans_num);
#endif

    ret = iod_trans_finish(coh, trans_num, NULL, IOD_TRANS_ABORT_DEPENDENT, NULL);
    if(ret == -IOD_EC_TRANS_DISCARDED)
        fprintf(stderr, "Transaction %"PRIu64" already discarded\n", input->trans_num);
    else if(ret < 0)
        HGOTO_ERROR_FF(ret, "can't abort transaction");

#if H5_EFF_DEBUG
    fprintf(stderr, "Done with Transaction Abort\n");
#endif

done:
    if(HG_SUCCESS != HG_Handler_start_output(op_data->hg_handle, &ret_value))
        fprintf(stderr, "Failed to Abort Transaction\n");

    HG_Handler_free_input(op_data->hg_handle, input);
    HG_Handler_free(op_data->hg_handle);
    input = (tr_abort_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

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
H5VL_iod_server_prefetch_cb(AXE_engine_t H5_ATTR_UNUSED axe_engine, 
                            size_t H5_ATTR_UNUSED num_n_parents, AXE_task_t H5_ATTR_UNUSED n_parents[], 
                            size_t H5_ATTR_UNUSED num_s_parents, AXE_task_t H5_ATTR_UNUSED s_parents[], 
                            void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    prefetch_in_t *input = (prefetch_in_t *)op_data->input;
    //iod_handle_t coh = input->coh; /* the container handle */
    iod_trans_id_t tid = input->rcxt_num;
    iod_handles_t iod_oh = input->iod_oh; /* object handle */
    iod_obj_id_t iod_id = input->iod_id; /* OID */
    H5I_type_t obj_type = input->obj_type;
    H5FF_layout_t layout_type = input->layout_type;
    iod_trans_id_t replica_id;
    iod_layout_t *layout = NULL;
    iod_obj_partition_t *partition = NULL;
    iod_kv_partition_t *kv_parts = NULL;
    iod_hyperslab_t *hslabs = NULL; /* IOD hyperslab generated from HDF5 filespace */
    hssize_t num_descriptors = 0, n; /* number of IOD hslabs needed to describe dataset selection */
    iod_ret_t ret;
    herr_t ret_value = SUCCEED;

#if H5_EFF_DEBUG
    fprintf(stderr, "Prefetch Object (OID %"PRIx64" OH %"PRIu64") at Version %"PRIu64"\n", 
            iod_id, iod_oh.rd_oh.cookie, tid);
#endif

    /* non-default layout (fetch to local ION) */
    if(H5_LOCAL_NODE == layout_type) {
#if H5_EFF_DEBUG
        fprintf(stderr, "Prefetch object to local ION with rank %d\n", my_rank_g);
#endif
        if(NULL == (layout = (iod_layout_t *)malloc(sizeof(iod_layout_t))))
            HGOTO_ERROR_FF(FAIL, "can't allocate layout buffer");
        layout->loc = IOD_LOC_BB;
        layout->type = IOD_LAYOUT_LOGGED;
        layout->dims_seq = NULL;
        layout->target_start = my_rank_g;
        layout->target_num = 1;
        layout->stripe_size = IOD_MAX_STRIPE_SIZE;
    }

    switch(obj_type) {
    case H5I_GROUP:
    case H5I_ATTR:
    case H5I_DATATYPE:
        break;
    case H5I_DATASET:
        {
            hid_t selection = input->selection;
            int ndims, i;  /* dataset's rank/number of dimensions */
            if(-1 != selection) {
                if(NULL == (partition = (iod_obj_partition_t *)malloc(sizeof(iod_obj_partition_t))))
                    HGOTO_ERROR_FF(FAIL, "can't allocate partition buffer");

                if((ndims = H5Sget_simple_extent_ndims(selection)) < 0)
                    HGOTO_ERROR_FF(FAIL, "unable to get dataspace dimesnsion");

                /* handle scalar dataspace */
                if(0 == ndims) {
                    ndims = 1;
                    /* allocate the IOD hyperslab descriptors needed */
                    if(NULL == (hslabs = (iod_hyperslab_t *)malloc(sizeof(iod_hyperslab_t))))
                        HGOTO_ERROR_FF(FAIL, "can't allocate iod array descriptors");

                    hslabs[0].start = (iod_size_t *)malloc(sizeof(iod_size_t));
                    hslabs[0].stride = (iod_size_t *)malloc(sizeof(iod_size_t));
                    hslabs[0].block = (iod_size_t *)malloc(sizeof(iod_size_t));
                    hslabs[0].count = (iod_size_t *)malloc(sizeof(iod_size_t));

                    num_descriptors = 1;
                    hslabs[0].start[0] = 0;
                    hslabs[0].count[0] = 1;
                    hslabs[0].block[0] = 1;
                    hslabs[0].stride[0] = 1;
                }
                else {
                    /* get the number of decriptors required, i.e. the numbers of iod
                       I/O operations needed */
                    if(H5VL_iod_get_file_desc(selection, &num_descriptors, NULL) < 0)
                        HGOTO_ERROR_FF(FAIL, "unable to generate IOD file descriptor from dataspace selection");

                    if(1 != num_descriptors)
                        HGOTO_ERROR_FF(FAIL, "can't prefetch an irregular hyperslab");

                    /* allocate the IOD hyperslab descriptors needed */
                    if(NULL == (hslabs = (iod_hyperslab_t *)malloc
                                (sizeof(iod_hyperslab_t) * (size_t)num_descriptors)))
                        HGOTO_ERROR_FF(FAIL, "can't allocate iod array descriptors");

                    for(n=0 ; n<num_descriptors ; n++) {
                        hslabs[n].start = (iod_size_t *)malloc(sizeof(iod_size_t) * (size_t)ndims);
                        hslabs[n].stride = (iod_size_t *)malloc(sizeof(iod_size_t) * (size_t)ndims);
                        hslabs[n].block = (iod_size_t *)malloc(sizeof(iod_size_t) * (size_t)ndims);
                        hslabs[n].count = (iod_size_t *)malloc(sizeof(iod_size_t) * (size_t)ndims);
                    }

                    /* generate the descriptors after allocating the array */
                    if(H5VL_iod_get_file_desc(selection, &num_descriptors, hslabs) < 0)
                        HGOTO_ERROR_FF(FAIL, "unable to generate IOD file descriptor from dataspace selection");
                }
                partition->obj_type = IOD_OBJ_ARRAY;
                partition->u_parts.array_parts = hslabs[0];

#if H5_EFF_DEBUG 
                fprintf(stderr, "Prefetch a Hyperslab of the Dataset:\n");
                for(i=0 ; i<ndims ; i++) {
                    fprintf(stderr, "Dim %d:  start %zu   stride %zu   block %zu   count %zu\n", 
                            i, (size_t)hslabs[0].start[i], (size_t)hslabs[0].stride[i], 
                            (size_t)hslabs[0].block[i], (size_t)hslabs[0].count[i]);
                }
#endif
            }
            break;
        }
    case H5I_MAP:
        {
            hid_t key_type = input->key_type;
            binary_buf_t low_key = input->low_key;
            binary_buf_t high_key = input->high_key;

            if(-1 != key_type) {
                if(NULL == (partition = (iod_obj_partition_t *)malloc
                            (sizeof(iod_obj_partition_t) + 2 * sizeof(iod_kv_t))))
                    HGOTO_ERROR_FF(FAIL, "can't allocate partition buffer");

                kv_parts = &partition->u_parts.kv_parts;

                partition->obj_type = IOD_OBJ_KV;
                kv_parts->nparts = 1;
                kv_parts->sub_partition = 1;
                kv_parts->kv[0].key = low_key.buf;
                kv_parts->kv[0].key_len = low_key.buf_size;
                kv_parts->kv[1].key = high_key.buf;
                kv_parts->kv[1].key_len = high_key.buf_size;

#if H5_EFF_DEBUG 
                fprintf(stderr, "Prefetch a Range of the Map:\n");
                fprintf(stderr, "Low Key: %d, High Key: %d\n", 
                        *((int *)low_key.buf), *((int *)high_key.buf));
#endif
            }
            break;
        }
    case H5I_UNINIT:
    case H5I_BADID:
    case H5I_FILE:
    case H5I_DATASPACE:
    case H5I_REFERENCE:
    case H5I_VFL:
    case H5I_VOL:
    case H5I_ES:
    case H5I_RC:
    case H5I_TR:
    case H5I_QUERY:
    case H5I_VIEW:
    case H5I_GENPROP_CLS:
    case H5I_GENPROP_LST:
    case H5I_ERROR_CLASS:
    case H5I_ERROR_MSG:
    case H5I_ERROR_STACK:
    case H5I_NTYPES:
    default:
        HGOTO_ERROR_FF(FAIL, "not a valid object to prefetch");
    }

    ret = iod_obj_fetch(iod_oh.rd_oh, tid, NULL, partition, layout, &replica_id, NULL);
    if(ret != 0)
        HGOTO_ERROR_FF(ret, "can't prefetch object");

#if H5_EFF_DEBUG
    fprintf(stderr, "Done with Prefetch\n");
#endif

done:
    if(SUCCEED != ret_value)
        replica_id = 0;

    if(HG_SUCCESS != HG_Handler_start_output(op_data->hg_handle, &replica_id))
        fprintf(stderr, "Failed to Prefetch Object\n");

    /* free allocated descriptors */
    for(n=0 ; n<num_descriptors ; n++) {
        free(hslabs[n].start);
        hslabs[n].start = NULL;
        free(hslabs[n].stride);
        hslabs[n].stride = NULL;
        free(hslabs[n].block);
        hslabs[n].block = NULL;
        free(hslabs[n].count);
        hslabs[n].count = NULL;
    }
    if(hslabs) {
        free(hslabs);
        hslabs = NULL;
    }

    if(layout) {
        free(layout);
        layout = NULL;
    }
    if(partition) {
        free(partition);
        partition = NULL;
    }

    HG_Handler_free_input(op_data->hg_handle, input);
    HG_Handler_free(op_data->hg_handle);
    input = (prefetch_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

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
H5VL_iod_server_evict_cb(AXE_engine_t H5_ATTR_UNUSED axe_engine, 
                            size_t H5_ATTR_UNUSED num_n_parents, AXE_task_t H5_ATTR_UNUSED n_parents[], 
                            size_t H5_ATTR_UNUSED num_s_parents, AXE_task_t H5_ATTR_UNUSED s_parents[], 
                            void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    evict_in_t *input = (evict_in_t *)op_data->input;
    iod_handle_t coh = input->coh; /* the container handle */
    iod_trans_id_t tid = input->rcxt_num;
    iod_handles_t iod_oh = input->iod_oh; /* object handle */
    iod_obj_id_t iod_id = input->iod_id; /* OID */
    iod_obj_id_t mdkv_id = input->mdkv_id; /* OID */
    iod_obj_id_t attrkv_id = input->attrkv_id; /* OID */
    //H5I_type_t obj_type = input->obj_type;
    iod_trans_id_t replica_id = input->replica_id;
    iod_handle_t mdkv_oh, attrkv_oh;
    iod_ret_t ret;
    herr_t ret_value = SUCCEED;

    if(replica_id) {
        fprintf(stderr, "Evict Object (OID %"PRIx64" OH %"PRIu64") replica tag %"PRIx64"\n", 
                iod_id, iod_oh.rd_oh.cookie, replica_id);
        ret = iod_obj_purge(iod_oh.rd_oh, replica_id, NULL, NULL);
    }
    else {
        fprintf(stderr, "Evict Object (OID %"PRIx64" OH %"PRIu64") at Version %"PRIu64"\n", 
                iod_id, iod_oh.rd_oh.cookie, tid);

        /* open the metadata KV */
        ret = iod_obj_open_read(coh, mdkv_id, tid, NULL, &mdkv_oh, NULL);
        if(ret < 0)
            HGOTO_ERROR_FF(ret, "can't open metadata KV");

        ret = iod_obj_purge(mdkv_oh, tid, NULL, NULL);
        if(ret < 0) {
            iod_obj_close(mdkv_oh, NULL, NULL);
            HGOTO_ERROR_FF(ret, "can't evict object");
        }

        /* close the metadata KV */
        ret = iod_obj_close(mdkv_oh, NULL, NULL);
        if(ret < 0)
            HGOTO_ERROR_FF(ret, "can't close object");

        if(IOD_OBJ_INVALID != attrkv_id) {
            /* open the attribute KV */
            ret = iod_obj_open_read(coh, attrkv_id, tid, NULL, &attrkv_oh, NULL);
            if(ret < 0)
                HGOTO_ERROR_FF(ret, "can't open metadata KV");

            ret = iod_obj_purge(attrkv_oh, tid, NULL, NULL);
            if(ret < 0) {
                iod_obj_close(attrkv_oh, NULL, NULL);
                HGOTO_ERROR_FF(ret, "can't evict object");
            }

            /* close the attribute KV */
            ret = iod_obj_close(attrkv_oh, NULL, NULL);
            if(ret < 0)
                HGOTO_ERROR_FF(ret, "can't close object");
        }

        ret = iod_obj_purge(iod_oh.rd_oh, tid, NULL, NULL);
    }

    if(ret < 0)
        HGOTO_ERROR_FF(ret, "can't evict object");

#if H5_EFF_DEBUG
    fprintf(stderr, "Done with Evict\n");
#endif

done:
    if(HG_SUCCESS != HG_Handler_start_output(op_data->hg_handle, &ret_value))
        fprintf(stderr, "Failed to Evict Object\n");

    HG_Handler_free_input(op_data->hg_handle, input);
    HG_Handler_free(op_data->hg_handle);
    input = (evict_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

} /* end H5VL_iod_server_evict_cb() */

#if H5_HAVE_IOD_CORRUPT_TOOL
static void check_ion_corruptions(iod_trans_id_t trans_num)
{
    int step;
    char *cor_step = NULL;
    char *num_procs_s = NULL;
    int num_procs = -1;
    char *cor_data_s = NULL;
    int cor_data = -1;
    int ret;
    char file_name[100];

    sprintf(file_name, "%s.%s", getenv("USER"), "eff_vpic");

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

    cor_step = getenv ("H5ENV_STEP_CORRUPT_BB_DSET");
    if(NULL != cor_step) {
        step = atoi(cor_step);

        if((int)trans_num == step+3) {
            uint64_t oid;

            oid = step*8*num_procs + num_procs;
            IOD_OBJID_SETOWNER_APP(oid)
            IOD_OBJID_SETTYPE(oid, IOD_OBJ_ARRAY)

            if(cor_data)
                printf("CORRUPTING DATA at step %d, ARRAY ID %"PRIx64"\n", 
                       step, oid);
            else
                printf("CORRUPTING CS at step %d, ARRAY ID %"PRIx64"\n", 
                       step, oid);

            ret = corrupt_data(file_name, oid, trans_num, 20, cor_data);
            if(ret < 0) {
                fprintf(stderr, "cant't corrupt data. %d (%s).\n", ret, strerror(-ret));
            }
        }
    }

    cor_step = NULL;
    step = -1;

    cor_step = getenv ("H5ENV_STEP_CORRUPT_BB_DTYPE");
    if(NULL != cor_step) {
        step = atoi(cor_step);

        if((int)trans_num == step+3) {
            uint64_t oid;

            oid = step*num_procs;
            IOD_OBJID_SETOWNER_APP(oid)
            IOD_OBJID_SETTYPE(oid, IOD_OBJ_BLOB)

            if(cor_data)
                printf("CORRUPTING DATA at step %d, BLOB ID %"PRIx64"\n", 
                       step, oid);
            else
                printf("CORRUPTING CS at step %d, BLOB ID %"PRIx64"\n", 
                       step, oid);

            ret = corrupt_data(file_name, oid, trans_num, 5, cor_data);
            if(ret < 0) {
                fprintf(stderr, "cant't corrupt data. %d (%s).\n", ret, strerror(-ret));
            }
        }
    }

    cor_step = NULL;
    step = -1;

    cor_step = getenv ("H5ENV_STEP_CORRUPT_BB_GROUP");
    if(NULL != cor_step) {
        step = atoi(cor_step);

        if((int)trans_num == step+3) {
            uint64_t oid;

            oid = 0;//5*num_procs + step*21*num_procs;
            IOD_OBJID_SETOWNER_APP(oid)
            IOD_OBJID_SETTYPE(oid, IOD_OBJ_KV)

            if(cor_data)
                printf("CORRUPTING DATA at step %d, KV ID %"PRIx64"\n", 
                       step, oid);
            else
                printf("CORRUPTING CS at step %d, KV ID %"PRIx64"\n", 
                       step, oid);

            ret = corrupt_kv(file_name, oid, trans_num, step*2+1, cor_data);
            if(ret < 0) {
                fprintf(stderr, "cant't corrupt data. %d (%s).\n", ret, strerror(-ret));
            }
        }
    }
}

static void check_daos_corruptions(iod_hint_list_t *chint, iod_trans_id_t trans_num)
{
    int step;
    char *cor_step = NULL;
    char *num_procs_s = NULL;
    int num_procs = -1;
    char *cor_data_s = NULL;
    int cor_data = -1;
    int ret, i;
    char file_name[100];

    sprintf(file_name, "%s.%s", getenv("USER"), "eff_vpic");

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

    chint->num_hint = 0;
    i = 0;

    cor_step = getenv ("H5ENV_STEP_CORRUPT_DAOS_DSET");
    if(NULL != cor_step) {
        uint64_t oid;

        step = atoi(cor_step);

        if((int)trans_num == step+3) {
            oid = step*8*num_procs + num_procs;
            IOD_OBJID_SETOWNER_APP(oid)
            IOD_OBJID_SETTYPE(oid, IOD_OBJ_ARRAY)

            chint->num_hint += 2;
            chint->hint[i].key = strdup("iod_hint_obj_corrupt_offset");
            chint->hint[i].value = strdup("20");
            i++;
            chint->hint[i].key = strdup("iod_hint_obj_corrupt_whichone");
            chint->hint[i].value = (char *)malloc(50);
            sprintf(chint->hint[i].value, "0x%llx", (unsigned long long)oid);
            i++;

            if(!cor_data) {
                chint->num_hint ++;
                chint->hint[i].key = strdup("iod_hint_obj_corrupt_checksum");
                chint->hint[i].value = NULL;
                i++;
                printf("CORRUPTING CS at VPIC step %d, array ID %"PRIx64"\n", 
                       step, oid);
            }
            else {
                printf("CORRUPTING Data at VPIC step %d, array ID %"PRIx64"\n", 
                       step, oid);
            }
        }
    }

    cor_step = NULL;
    step = -1;

    cor_step = getenv ("H5ENV_STEP_CORRUPT_DAOS_DTYPE");
    if(NULL != cor_step) {
        uint64_t oid;

        step = atoi(cor_step);

        if((int)trans_num == step+3) {
            oid = step*num_procs;
            IOD_OBJID_SETOWNER_APP(oid)
            IOD_OBJID_SETTYPE(oid, IOD_OBJ_BLOB)
                
            chint->num_hint += 2;
            chint->hint[i].key = strdup("iod_hint_obj_corrupt_offset");
            chint->hint[i].value = strdup("5");
            i++;
            chint->hint[i].key = strdup("iod_hint_obj_corrupt_whichone");
            chint->hint[i].value = (char *)malloc(50);
            sprintf(chint->hint[i].value, "0x%llx", (unsigned long long)oid);
            //sprintf(chint->hint[i].value, "0x%"PRIx64"", oid);
            i++;
            if(!cor_data) {
                chint->num_hint ++;
                chint->hint[i].key = strdup("iod_hint_obj_corrupt_checksum");
                chint->hint[i].value = NULL;
                i++;

                printf("CORRUPTING CS at VPIC step %d, blob ID %"PRIx64"\n", 
                       step, oid);
            }
            else {
                printf("CORRUPTING Data at VPIC step %d, blob ID %"PRIx64"\n", 
                       step, oid);
            }
        }
    }

    cor_step = NULL;
    step = -1;

    cor_step = getenv ("H5ENV_STEP_CORRUPT_DAOS_GROUP");
    if(NULL != cor_step) {
        uint64_t oid;

        step = atoi(cor_step);

        if((int)trans_num == step+3) {
            oid = 0;
            IOD_OBJID_SETOWNER_APP(oid)
            IOD_OBJID_SETTYPE(oid, IOD_OBJ_KV)
                
            chint->num_hint += 2;
            chint->hint[i].key = strdup("iod_hint_obj_corrupt_offset");
            chint->hint[i].value = (char *)malloc(10);
            sprintf(chint->hint[i].value, "%d", step*2+1);

            i++;
            chint->hint[i].key = strdup("iod_hint_obj_corrupt_whichone");
            chint->hint[i].value = (char *)malloc(50);
            sprintf(chint->hint[i].value, "0x%llx", (unsigned long long)oid);
            //sprintf(chint->hint[i].value, "0x%"PRIx64"", oid);
            i++;
            if(!cor_data) {
                chint->num_hint ++;
                chint->hint[i].key = strdup("iod_hint_obj_corrupt_checksum");
                chint->hint[i].value = NULL;
                i++;

                printf("CORRUPTING CS at VPIC step %d, KV ID %"PRIx64"\n", 
                       step, oid);
            }
            else {
                printf("CORRUPTING Data at VPIC step %d, KV ID %"PRIx64"\n", 
                       step, oid);
            }

            ret = corrupt_kv(file_name, oid, trans_num, step*2+1, cor_data);
            if(ret < 0) {
                fprintf(stderr, "cant't corrupt data. %d (%s).\n", ret, strerror(-ret));
            }
        }
    }
}

#endif

#endif /* H5_HAVE_EFF */
