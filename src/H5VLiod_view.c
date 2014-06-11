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

#include "H5Qpublic.h"
#include "H5RCpublic.h"
#include "H5TRpublic.h"
#include "H5Vpublic.h"

#ifdef H5_HAVE_EFF

/* entry describing a view object */
typedef struct H5VL_iod_view_entry_t {
    H5I_type_t obj_type;
    void *token;
    size_t token_size;
    char *link_name;
    char *attr_name;
    hid_t selection;
    struct H5VL_iod_view_entry_t *next;
} H5VL_iod_view_entry_t;

/* linked list used in constructing a View object */
typedef struct {
    size_t num_entries;
    H5VL_iod_view_entry_t *head;
    H5VL_iod_view_entry_t *tail;
} H5VL_iod_view_list_t;

typedef struct {
    H5VL_iod_view_list_t region_list;
    H5VL_iod_view_list_t attr_list;
    H5VL_iod_view_list_t obj_list;
} H5VL_iod_view_obj_t;

typedef struct {
    hid_t query_id;
    hid_t vcpl_id;
    hid_t file_id;
    hid_t rcxt_id;
    H5VL_iod_view_obj_t *view;
} H5VL_build_view_t;

/* Enum for Query results */
typedef enum qresult_t {
    QFALSE = -1,
    QNEUTRAL,
    QTRUE
} qresult_t;

static herr_t
H5VL__iod_get_token(iod_handle_t coh, H5I_type_t obj_type, iod_obj_id_t iod_id, 
                    iod_trans_id_t rtid, uint32_t cs_scope, size_t *_token_size, void **_token);
static herr_t 
H5VL__iod_apply_query(hid_t file_id, hid_t rcxt_id, hid_t qid, hid_t vcpl_id, 
                      iod_handle_t coh, iod_obj_id_t obj_id, 
                      iod_trans_id_t rtid, H5I_type_t obj_type, 
                      const char *link_name, const char *attr_name,
                      size_t num_attrs, char *attr_list[],
                      uint32_t cs_scope, qresult_t *result, hid_t *region);
static herr_t 
H5VL__iod_build_view_cb(iod_handle_t coh, iod_obj_id_t obj_id, iod_trans_id_t rtid,
                        H5I_type_t obj_type, const char *link_name, const char *attr_name,
                        uint32_t cs_scope, void *_op_data);

static hid_t 
H5VL__iod_get_elmt_region(iod_handle_t coh, iod_obj_id_t dset_id, iod_trans_id_t rtid, 
                          hid_t query_id, hid_t vcpl_id, uint32_t cs_scope);

static void
H5VL__iod_add_entry(H5VL_iod_view_list_t *list, H5VL_iod_view_entry_t* entry);

static herr_t
H5VL__iod_free_view_list(H5VL_iod_view_list_t list);

/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_view_create_cb
 *
 * Purpose:	Create a View from a query on a location in the container.
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
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
    view_create_out_t output;
    iod_handle_t coh = input->coh; /* the container handle */
    //iod_handles_t loc_handle = input->loc_oh; /* The handle for current object - could be undefined */
    iod_obj_id_t loc_id = input->loc_id; /* The ID of the current location object */
    H5I_type_t obj_type = input->obj_type;
    hid_t query_id = input->query_id;
    iod_trans_id_t rtid = input->rcxt_num;
    uint32_t cs_scope = input->cs_scope;
    hid_t vcpl_id;
    size_t i;
    H5VL_build_view_t udata;
    H5VL_iod_view_entry_t *entry = NULL;
    H5VL_iod_view_obj_t *view = NULL;
    hid_t file_id, rcxt_id, fapl_id;
    herr_t ret_value = SUCCEED;

#if H5_EFF_DEBUG 
    fprintf(stderr, "Start View create on OID %"PRIx64"\n", loc_id);
#endif

    /* wrap a file hid_t around the iod container handle */
    fapl_id = H5Pcreate (H5P_FILE_ACCESS);
    H5Pset_fapl_iod(fapl_id, MPI_COMM_SELF, MPI_INFO_NULL);
    if((file_id = H5VLiod_get_file_id("bla", coh, fapl_id, &rcxt_id)) < 0)
        HGOTO_ERROR_FF(FAIL, "can't get file ID");
    H5Pclose(fapl_id);

    if(H5P_DEFAULT == input->vcpl_id)
        input->vcpl_id = H5Pcopy(H5P_VIEW_CREATE_DEFAULT);
    vcpl_id = input->vcpl_id;

    view = (H5VL_iod_view_obj_t *)calloc(1, sizeof(H5VL_iod_view_obj_t));

    udata.view = view;
    udata.query_id = query_id;
    udata.vcpl_id = vcpl_id;
    udata.file_id = file_id;
    udata.rcxt_id = rcxt_id;

    if(H5VL_iod_server_iterate(coh, loc_id, rtid, obj_type, NULL, NULL, cs_scope, 
                               H5VL__iod_build_view_cb, &udata) < 0)
        HGOTO_ERROR_FF(FAIL, "can't iterate into container");

    output.valid_view = TRUE;

#if H5_EFF_DEBUG
    fprintf(stderr, "View has %zu Region Entries\n", view->region_list.num_entries);
    fprintf(stderr, "View has %zu Attribute Entries\n", view->attr_list.num_entries);
    fprintf(stderr, "View has %zu Object Entries\n", view->obj_list.num_entries);
#endif

    output.region_info.count = view->region_list.num_entries;
    output.attr_info.count = view->attr_list.num_entries;
    output.obj_info.count = view->obj_list.num_entries;

    /* collect dataset region results */
    if(view->region_list.num_entries) {
        output.region_info.tokens = (binary_buf_t *)malloc
            (view->region_list.num_entries * sizeof(binary_buf_t));
        output.region_info.regions = (hid_t *)malloc
            (view->region_list.num_entries * sizeof(hid_t));

        entry = view->region_list.head;
        for(i=0 ; i<output.region_info.count; i++) {
            assert(entry);
            output.region_info.tokens[i].buf = entry->token;
            output.region_info.tokens[i].buf_size = entry->token_size;
            output.region_info.regions[i] = entry->selection;
            entry = entry->next;
        }
    }

    /* collect attribute results */
    if(view->attr_list.num_entries) {
        output.attr_info.tokens = (binary_buf_t *)malloc
            (view->attr_list.num_entries * sizeof(binary_buf_t));

        entry = view->attr_list.head;
        for(i=0 ; i<output.attr_info.count; i++) {
            assert(entry);
            output.attr_info.tokens[i].buf = entry->token;
            output.attr_info.tokens[i].buf_size = entry->token_size;
            entry = entry->next;
        }
    }

    /* collect object results */
    if(view->obj_list.num_entries) {
        output.obj_info.tokens = (binary_buf_t *)malloc
            (view->obj_list.num_entries * sizeof(binary_buf_t));

        entry = view->obj_list.head;
        for(i=0 ; i<output.obj_info.count; i++) {
            assert(entry);
            output.obj_info.tokens[i].buf = entry->token;
            output.obj_info.tokens[i].buf_size = entry->token_size;
            entry = entry->next;
        }
    }

    /* free the file ID and release the read context */
    if(H5RCrelease(rcxt_id, H5_EVENT_STACK_NULL) < 0)
        HGOTO_ERROR_FF(FAIL, "can't release read context");
    H5RCclose(rcxt_id);
    if(H5VLiod_close_file_id(file_id) < 0)
        HGOTO_ERROR_FF(FAIL, "can't release file id");

done:

#if H5_EFF_DEBUG 
    fprintf(stderr, "Done with View Create, sending %d response to client \n", ret_value);
#endif

    if(HG_SUCCESS != HG_Handler_start_output(op_data->hg_handle, &output))
        HDONE_ERROR_FF(FAIL, "unable to send output to client");        

    if(output.region_info.count) {
        free(output.region_info.tokens);
        free(output.region_info.regions);
    }

    if(output.attr_info.count) {
        free(output.attr_info.tokens);
    }
    if(output.obj_info.count) {
        free(output.obj_info.tokens);
    }

    H5VL__iod_free_view_list(view->region_list);
    H5VL__iod_free_view_list(view->attr_list);
    H5VL__iod_free_view_list(view->obj_list);

    if(view)
        free(view);

    HG_Handler_free_input(op_data->hg_handle, input);
    HG_Handler_free(op_data->hg_handle);
    input = (view_create_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

} /* end H5VL_iod_server_view_create_cb() */

static herr_t 
H5VL__iod_build_view_cb(iod_handle_t coh, iod_obj_id_t obj_id, iod_trans_id_t rtid,
                        H5I_type_t obj_type, const char *link_name, const char *attr_name,
                        uint32_t cs_scope, void *_op_data)
{
    H5VL_build_view_t *op_data = (H5VL_build_view_t *)_op_data;
    qresult_t result = QFALSE;
    herr_t ret;
    hid_t region = FAIL;
    char **attr_list = NULL;
    int num_attrs = 0;
    int i;
    H5VL_iod_view_obj_t *view = op_data->view;
    herr_t ret_value = SUCCEED;

#if H5_EFF_DEBUG 
    fprintf(stderr, "Building Query on OID %"PRIx64"\n", obj_id);
#endif

    /* iterate over all attributes of that object */
    {
        iod_handle_t obj_oh;
        scratch_pad sp;
        iod_checksum_t sp_cs = 0;

        if (iod_obj_open_read(coh, obj_id, rtid, NULL, &obj_oh, NULL) < 0)
            HGOTO_ERROR_FF(FAIL, "can't open current group");

        ret = iod_obj_get_scratch(obj_oh, rtid, &sp, &sp_cs, NULL);
        if(ret < 0)
            HGOTO_ERROR_FF(ret, "can't get scratch pad for object");

        if(sp_cs && (cs_scope & H5_CHECKSUM_IOD)) {
            /* verify scratch pad integrity */
            if(H5VL_iod_verify_scratch_pad(&sp, sp_cs) < 0)
                HGOTO_ERROR_FF(FAIL, "Scratch Pad failed integrity check");
        }

        /* if attribute KV exists, iterate over attributes */
        if(IOD_OBJ_INVALID != sp[1]) {
            iod_handle_t attr_kv_oh;

            /* open the attribute KV in scratch pad */
            ret = iod_obj_open_read(coh, sp[1], rtid, NULL, &attr_kv_oh, NULL);
            if(ret < 0)
                HGOTO_ERROR_FF(ret, "can't open attribute KV");

            ret = iod_kv_get_num(attr_kv_oh, rtid, &num_attrs, NULL);
            if(ret != 0)
                HGOTO_ERROR_FF(FAIL, "can't get number of attributes");

            if(0 != num_attrs) {
                iod_kv_params_t *kvs = NULL;
                iod_kv_t *kv = NULL;
                iod_checksum_t *oid_cs = NULL;
                iod_ret_t *oid_ret = NULL;

                attr_list = (char **)malloc(sizeof(char*) * (size_t)num_attrs);
                kvs = (iod_kv_params_t *)malloc(sizeof(iod_kv_params_t) * (size_t)num_attrs);
                kv = (iod_kv_t *)malloc(sizeof(iod_kv_t) * (size_t)num_attrs);
                oid_cs = (iod_checksum_t *)malloc(sizeof(iod_checksum_t) * (size_t)num_attrs);
                oid_ret = (iod_ret_t *)malloc(sizeof(iod_ret_t) * (size_t)num_attrs);

                for(i=0 ; i<num_attrs ; i++) {
                    kv[i].key = malloc(IOD_KV_KEY_MAXLEN);
                    kv[i].key_len = IOD_KV_KEY_MAXLEN;
                    kvs[i].kv = &kv[i];
                    kvs[i].cs = &oid_cs[i];
                    kvs[i].ret = &oid_ret[i];
                }

                ret = iod_kv_list_key(attr_kv_oh, rtid, NULL, 0, &num_attrs, kvs, NULL);
                if(ret != 0)
                    HGOTO_ERROR_FF(FAIL, "can't get list of keys");

                for(i=0 ; i<num_attrs ; i++) {
                    iod_obj_id_t oid;
                    H5VL_iod_link_t value;

                    /* lookup object in the current group */
                    ret = H5VL_iod_get_metadata(attr_kv_oh, rtid, H5VL_IOD_LINK, 
                                                (char *)(kv[i].key), cs_scope, NULL, &value);
                    if(SUCCEED != ret)
                        HGOTO_ERROR_FF(ret, "failed to retrieve link value");

                    if(H5L_TYPE_SOFT == value.link_type) {
                        continue;
                    }
                    else
                        oid = value.u.iod_id;

#if H5_EFF_DEBUG
                    fprintf(stderr, "Iterating into Attribute %s OID %"PRIx64"\n", 
                            ((char *)kv[i].key), oid);
#endif
                    ret = H5VL__iod_build_view_cb(coh, oid, rtid, H5I_ATTR, link_name, 
                                                  ((char *)kv[i].key), cs_scope, op_data);
                    if(ret != 0)
                        HGOTO_ERROR_FF(FAIL, "can't apply iterate callback on current attribute");
                    attr_list[i] = strdup(((char *)kv[i].key));
                }

                for(i=0 ; i<num_attrs ; i++) {
                    free(kv[i].key);
                }

                free(kv);
                free(oid_cs);
                free(oid_ret);
                free(kvs);
            }
            iod_obj_close(attr_kv_oh, NULL, NULL);
        }
        ret = iod_obj_close(obj_oh, NULL, NULL);
        if(ret != 0)
            HGOTO_ERROR_FF(ret, "can't close object");
    }

    ret = H5VL__iod_apply_query(op_data->file_id, op_data->rcxt_id, 
                                op_data->query_id, op_data->vcpl_id, 
                                coh, obj_id, rtid, obj_type, 
                                link_name, attr_name, (size_t)num_attrs, attr_list, 
                                cs_scope, &result, &region);
    if(ret != 0)
        HGOTO_ERROR_FF(ret, "Error applying query");

    for(i=0; i<num_attrs; i++)
        free(attr_list[i]);
    if(attr_list)
        free(attr_list);

    if(result > 0) {
        H5VL_iod_view_entry_t *entry = NULL;

        entry = (H5VL_iod_view_entry_t *)calloc(1, sizeof(H5VL_iod_view_entry_t));
        entry->obj_type = obj_type;
        entry->selection = region;
        if(link_name)
            entry->link_name = strdup(link_name);
        if(attr_name)
            entry->attr_name = strdup(attr_name);

        if(H5VL__iod_get_token(coh, obj_type, obj_id, rtid, cs_scope,
                               &entry->token_size, &entry->token) < 0)
            HGOTO_ERROR_FF(FAIL, "failed to get object token");
        entry->next = NULL;

        /* add entry to view linked list */
        if(region != FAIL)
            H5VL__iod_add_entry(&view->region_list, entry);
        else if(attr_name)
            H5VL__iod_add_entry(&view->attr_list, entry);
        else
            H5VL__iod_add_entry(&view->obj_list, entry);
    }

done:
    return ret_value;
}

static herr_t 
H5VL__iod_apply_query(hid_t file_id, hid_t rcxt_id, hid_t qid, hid_t vcpl_id,
                      iod_handle_t coh, iod_obj_id_t obj_id, 
                      iod_trans_id_t rtid, H5I_type_t obj_type, 
                      const char *link_name, const char *attr_name,
                      size_t num_attrs, char *attr_list[],
                      uint32_t cs_scope, qresult_t *result, hid_t *region)
{
    H5Q_combine_op_t comb_type;
    H5Q_match_op_t match_op;
    H5Q_type_t q_type;
    herr_t ret;
    herr_t ret_value = SUCCEED;

    *result = QFALSE;

    if(H5Qget_combine_op(qid, &comb_type) < 0)
        HGOTO_ERROR_FF(FAIL, "can't get query op type");

    if(H5Q_SINGLETON == comb_type) {
        if(H5Qget_match_info(qid, &q_type, &match_op) < 0)
            HGOTO_ERROR_FF(FAIL, "can't get query info");

        if(H5Q_TYPE_DATA_ELEM == q_type) {
            if(H5I_DATASET == obj_type) {
                hid_t sid, scope_id;
                hid_t dset_id, trans_id;
                size_t token_size = 0;
                void *token = NULL;

                if(H5VL__iod_get_token(coh, obj_type, obj_id, rtid, cs_scope,
                                       &token_size, &token) < 0)
                    HGOTO_ERROR_FF(FAIL, "failed to get object token");

                trans_id = H5TRcreate(file_id, rcxt_id, rtid);
                if((dset_id = H5Oopen_by_token(token, trans_id, H5_EVENT_STACK_NULL)) < 0)
                    HGOTO_ERROR_FF(FAIL, "failed to open dataset by token");
                H5TRclose(trans_id);

                /* Check if VCPL has a dataspace specified; otherwise, read the entire dataset. */
                if(H5Pget_view_elmt_scope(vcpl_id, &scope_id) < 0)
                    HGOTO_ERROR_FF(FAIL, "can't retrieve vcpl region scope");

                sid = H5Dquery_ff(dset_id, qid, scope_id, rcxt_id);
                if(FAIL == sid)
                    HGOTO_ERROR_FF(FAIL, "failed to apply query on dataset region");

                if(H5Dclose_ff(dset_id, H5_EVENT_STACK_NULL) < 0)
                    HGOTO_ERROR_FF(FAIL, "failed to close dataset");

                if(token) {
                    free(token);
                    token = NULL;
                }

                if(H5Sget_select_npoints(sid) > 0) {
                    *result = QTRUE;
                    *region = sid;
                }

                else {
                    if(H5Sclose(sid) < 0)
                        HGOTO_ERROR_FF(FAIL, "unable to release dataspace");
                }
            }
        }
        else if(H5Q_TYPE_ATTR_VALUE == q_type) {
            /* add the attribute to the view if any of the attribute values matches the query */
            if(H5I_ATTR == obj_type) {
                hid_t sid;

                /* add object to view with region if region is not NONE */
                if((sid = H5VL__iod_get_elmt_region(coh, obj_id, rtid, qid, vcpl_id, cs_scope)) < 0)
                    HGOTO_ERROR_FF(FAIL, "can't get region from query");

                if(H5Sget_select_npoints(sid) > 0) {
                    *result = QTRUE;
                }

                if(H5Sclose(sid) < 0)
                    HGOTO_ERROR_FF(FAIL, "unable to release dataspace");
            }
        }
        else if(H5Q_TYPE_LINK_NAME == q_type) {
            hbool_t temp_result;

            if(H5Qapply(qid, &temp_result, link_name) < 0)
                HGOTO_ERROR_FF(FAIL, "can't apply link name query");

            if(TRUE == temp_result && H5I_ATTR == obj_type)
                *result = QNEUTRAL;
            else if(TRUE == temp_result)
                *result = QTRUE;
        }
        else if(H5Q_TYPE_ATTR_NAME == q_type) {
            size_t i;
            hbool_t temp_result;

            for (i=0 ; i<num_attrs ; i++) {
                if(H5Qapply(qid, &temp_result, attr_list[i]) < 0)
                    HGOTO_ERROR_FF(FAIL, "can't apply attr name query");
                if(TRUE == temp_result) {
                    *result = QTRUE;
                    break;
                }
            }
            if(H5I_ATTR == obj_type && attr_name) {
                if(H5Qapply(qid, &temp_result, attr_name) < 0)
                    HGOTO_ERROR_FF(FAIL, "can't apply attr name query");
                if(TRUE == temp_result)
                    *result = QNEUTRAL;
            }
        }
        else
            HGOTO_ERROR_FF(FAIL, "invalid query type");
    }
    else {
        qresult_t result1, result2;
        hid_t qid1, qid2;
        hid_t sid1 = FAIL , sid2 = FAIL;

        if(H5Qget_components(qid, &qid1, &qid2) < 0)
            HGOTO_ERROR_FF(FAIL, "can't get query components");

        ret = H5VL__iod_apply_query(file_id, rcxt_id, qid1, vcpl_id, coh, obj_id, rtid, 
                                    obj_type, link_name, attr_name, 
                                    num_attrs, attr_list, cs_scope, 
                                    &result1, &sid1);
        if(ret != 0)
            HGOTO_ERROR_FF(ret, "Error applying query");

        /* if the result of the first query is empty, 
           no need to apply the second one if they are anded */
        if(result1 == QFALSE && H5Q_COMBINE_AND == comb_type) {
            *result = QFALSE;
        }
        else {
            ret = H5VL__iod_apply_query(file_id, rcxt_id, qid2, vcpl_id, coh, obj_id, rtid, 
                                        obj_type, link_name, attr_name, 
                                        num_attrs, attr_list, cs_scope, 
                                        &result2, &sid2);
            if(ret != 0)
                HGOTO_ERROR_FF(ret, "Error applying query");

            /* combine result1 and result2 */
            if(H5Q_COMBINE_AND == comb_type) {
                fprintf(stderr, "ANDing %d and %d\n", result1, result2);

                *result = result1 + result2;

                if(sid1!=FAIL && sid2!=FAIL) {
                    /* MSC - AND the selections when API is available */
                    *region = H5Scopy(sid1);                    
                }
                else if(sid1!=FAIL)
                    *region = H5Scopy(sid1);
                else if (sid2!=FAIL)
                    *region = H5Scopy(sid2);
            }
            else if(H5Q_COMBINE_OR == comb_type) {
                fprintf(stderr, "ORing %d and %d\n", result1, result2);

                *result = result1 + result2 + 1;

                if(sid1!=FAIL && sid2!=FAIL) {
                    /* MSC - OR the selections when API is available */

                    *region = H5Scopy(sid2);
                }
                else if(sid1!=FAIL)
                    *region = H5Scopy(sid1);
                else if (sid2!=FAIL)
                    *region = H5Scopy(sid2);
            }
            else
                HGOTO_ERROR_FF(FAIL, "invalid query combine OP");
        }
        if(sid1!=FAIL && H5Sclose(sid1) < 0)
            HGOTO_ERROR_FF(FAIL, "unable to release dataspace");
        if(sid2!=FAIL && H5Sclose(sid2) < 0)
            HGOTO_ERROR_FF(FAIL, "unable to release dataspace");
    }
done:
    return ret_value;
} /* H5VL__iod_apply_query */

static herr_t
H5VL__iod_get_token(iod_handle_t coh, H5I_type_t obj_type, iod_obj_id_t obj_id, 
                    iod_trans_id_t rtid, uint32_t cs_scope,
                    size_t *_token_size, void **_token)
{
    size_t dt_size = 0, space_size = 0, plist_size = 0;
    size_t keytype_size = 0, valtype_size;
    iod_handle_t obj_oh, mdkv_oh;
    hid_t id1, id2, cpl_id;
    void *token = NULL;
    uint8_t *buf_ptr = NULL;
    size_t token_size = 0;
    void *dt_buf = NULL;
    scratch_pad sp;
    iod_checksum_t sp_cs;
    iod_ret_t ret;
    herr_t ret_value = SUCCEED;

    /* open the object */
    if(iod_obj_open_read(coh, obj_id, rtid, NULL, &obj_oh, NULL) < 0)
        HGOTO_ERROR_FF(FAIL, "can't open object fo read");

    /* get scratch pad */
    if(iod_obj_get_scratch(obj_oh, rtid, &sp, &sp_cs, NULL) < 0)
        HGOTO_ERROR_FF(FAIL, "can't get scratch pad for object");

    if(sp_cs && (cs_scope & H5_CHECKSUM_IOD)) {
        /* verify scratch pad integrity */
        if(H5VL_iod_verify_scratch_pad(&sp, sp_cs) < 0)
            HGOTO_ERROR_FF(FAIL, "Scratch Pad failed integrity check");
    }

    /* open the metadata scratch pad */
    if(iod_obj_open_read(coh, sp[0], rtid, NULL, &mdkv_oh, NULL) < 0)
        HGOTO_ERROR_FF(FAIL, "can't open metadata KV");

    token_size = sizeof(iod_obj_id_t)*3 + sizeof(H5I_type_t);

    if(H5I_FILE == obj_type)
        obj_type = H5I_GROUP;

    switch(obj_type) {
    case H5I_GROUP:
        if(H5VL_iod_get_metadata(mdkv_oh, rtid, H5VL_IOD_PLIST, H5VL_IOD_KEY_OBJ_CPL,
                                 cs_scope, NULL, &cpl_id) < 0)
            HGOTO_ERROR_FF(FAIL, "failed to retrieve dcpl");

        if(H5Pencode(cpl_id, NULL, &plist_size) < 0)
            HGOTO_ERROR_FF(FAIL, "can't encode plist");
        token_size += plist_size + sizeof(size_t);
        break;
    case H5I_DATASET:
        if(H5VL_iod_get_metadata(mdkv_oh, rtid, H5VL_IOD_DATATYPE, 
                                 H5VL_IOD_KEY_OBJ_DATATYPE,
                                 cs_scope, NULL, &id1) < 0)
            HGOTO_ERROR_FF(FAIL, "failed to retrieve datatype");

        if(H5VL_iod_get_metadata(mdkv_oh, rtid, H5VL_IOD_DATASPACE, 
                                 H5VL_IOD_KEY_OBJ_DATASPACE,
                                 cs_scope, NULL, &id2) < 0)
            HGOTO_ERROR_FF(FAIL, "failed to retrieve dataspace");

        if(H5VL_iod_get_metadata(mdkv_oh, rtid, H5VL_IOD_PLIST, 
                                 H5VL_IOD_KEY_OBJ_CPL,
                                 cs_scope, NULL, &cpl_id) < 0)
            HGOTO_ERROR_FF(FAIL, "failed to retrieve dcpl");

        if(H5Pencode(cpl_id, NULL, &plist_size) < 0)
            HGOTO_ERROR_FF(FAIL, "can't encode plist");

        if(H5Tencode(id1, NULL, &dt_size) < 0)
            HGOTO_ERROR_FF(FAIL, "can't encode datatype");

        if(H5Sencode(id2, NULL, &space_size) < 0)
            HGOTO_ERROR_FF(FAIL, "can't encode dataspace");

        token_size += plist_size + dt_size + space_size + sizeof(size_t)*3;
        break;
    case H5I_ATTR:
        if(H5VL_iod_get_metadata(mdkv_oh, rtid, H5VL_IOD_DATATYPE, 
                                 H5VL_IOD_KEY_OBJ_DATATYPE,
                                 cs_scope, NULL, &id1) < 0)
            HGOTO_ERROR_FF(FAIL, "failed to retrieve datatype");

        if(H5VL_iod_get_metadata(mdkv_oh, rtid, H5VL_IOD_DATASPACE, 
                                 H5VL_IOD_KEY_OBJ_DATASPACE,
                                 cs_scope, NULL, &id2) < 0)
            HGOTO_ERROR_FF(FAIL, "failed to retrieve dataspace");

        if(H5Tencode(id1, NULL, &dt_size) < 0)
            HGOTO_ERROR_FF(FAIL, "can't encode datatype");

        if(H5Sencode(id2, NULL, &space_size) < 0)
            HGOTO_ERROR_FF(FAIL, "can't encode dataspace");

        token_size += dt_size + space_size + sizeof(size_t)*3;
        break;
    case H5I_DATATYPE:
        {
            iod_mem_desc_t *mem_desc = NULL; /* memory descriptor used for reading */
            iod_blob_iodesc_t *file_desc = NULL; /* file descriptor used to write */
            iod_checksum_t dt_cs = 0, blob_cs = 0;
            iod_size_t key_size, val_size;
            iod_checksum_t iod_cs[2];

            key_size = 1 + strlen(H5VL_IOD_KEY_DTYPE_SIZE);
            val_size = sizeof(iod_size_t);

            if(H5VL_iod_get_metadata(mdkv_oh, rtid, H5VL_IOD_PLIST, 
                                     H5VL_IOD_KEY_OBJ_CPL,
                                     cs_scope, NULL, &cpl_id) < 0)
                HGOTO_ERROR_FF(FAIL, "failed to retrieve dcpl");

            /* retrieve blob size metadata from scratch pad */
            ret = iod_kv_get_value(mdkv_oh, rtid, H5VL_IOD_KEY_DTYPE_SIZE, key_size,
                                   &dt_size, &val_size, iod_cs, NULL);
            if(ret < 0)
                HGOTO_ERROR_FF(ret, "datatype size lookup failed");

            if(cs_scope & H5_CHECKSUM_IOD) {
                if(H5VL_iod_verify_kv_pair(H5VL_IOD_KEY_DTYPE_SIZE, key_size, 
                                           &dt_size, val_size, iod_cs) < 0)
                    HGOTO_ERROR_FF(FAIL, "Corruption detected when reading metadata from IOD");
            }

            if(NULL == (dt_buf = malloc(dt_size)))
                HGOTO_ERROR_FF(FAIL, "can't allocate BLOB read buffer");

            /* create memory descriptor for reading */
            mem_desc = (iod_mem_desc_t *)malloc(sizeof(iod_mem_desc_t) + 
                                                sizeof(iod_mem_frag_t));
            mem_desc->nfrag = 1;
            mem_desc->frag[0].addr = dt_buf;
            mem_desc->frag[0].len = (iod_size_t)dt_size;

            /* create file descriptor for writing */
            file_desc = (iod_blob_iodesc_t *)malloc(sizeof(iod_blob_iodesc_t) + 
                                                    sizeof(iod_blob_iofrag_t));
            file_desc->nfrag = 1;
            file_desc->frag[0].offset = 0;
            file_desc->frag[0].len = (iod_size_t)dt_size;

            /* read the serialized type value from the BLOB object */
            ret = iod_blob_read(obj_oh, rtid, NULL, mem_desc, file_desc, 
                                &blob_cs, NULL);
            if(ret < 0)
                HGOTO_ERROR_FF(ret, "unable to read BLOB object");

            if(blob_cs && (cs_scope & H5_CHECKSUM_IOD)) {
                /* calculate a checksum for the datatype */
                dt_cs = H5_checksum_crc64(dt_buf, dt_size);

                /* Verify checksum against one given by IOD */
                if(blob_cs != dt_cs)
                    HGOTO_ERROR_FF(FAIL, "Data Corruption detected when reading datatype");
            }

            if(H5Pencode(cpl_id, NULL, &plist_size) < 0)
                HGOTO_ERROR_FF(FAIL, "can't encode plist");

            token_size += plist_size + dt_size + sizeof(size_t)*2;

            free(mem_desc);
            free(file_desc);
            break;
        }
    case H5I_MAP:
        ret = H5VL_iod_get_metadata(mdkv_oh, rtid, H5VL_IOD_PLIST, 
                                    H5VL_IOD_KEY_OBJ_CPL,
                                    cs_scope, NULL, &cpl_id);
        if(ret != SUCCEED)
            HGOTO_ERROR_FF(ret, "failed to retrieve mcpl");

        ret = H5VL_iod_get_metadata(mdkv_oh, rtid, H5VL_IOD_DATATYPE, 
                                    H5VL_IOD_KEY_MAP_KEY_TYPE,
                                    cs_scope, NULL, &id1);
        if(ret != SUCCEED)
            HGOTO_ERROR_FF(ret, "failed to retrieve map keytype");

        ret = H5VL_iod_get_metadata(mdkv_oh, rtid, H5VL_IOD_DATATYPE, 
                                    H5VL_IOD_KEY_MAP_VALUE_TYPE,
                                    cs_scope, NULL, &id2);
        if(ret != SUCCEED)
            HGOTO_ERROR_FF(ret, "failed to retrieve map valtype");

        if(H5Pencode(cpl_id, NULL, &plist_size) < 0)
            HGOTO_ERROR_FF(FAIL, "can't encode plist");

        if(H5Tencode(id1, NULL, &keytype_size) < 0)
            HGOTO_ERROR_FF(FAIL, "can't encode datatype");

        if(H5Tencode(id2, NULL, &valtype_size) < 0)
            HGOTO_ERROR_FF(FAIL, "can't encode datatype");

        token_size += plist_size + keytype_size + valtype_size + sizeof(size_t)*3;
        break;
    default:
        HGOTO_ERROR_FF(FAIL, "Invalid object");
    }

    if(iod_obj_close(mdkv_oh, NULL, NULL) < 0)
        HGOTO_ERROR_FF(FAIL, "can't close iod object");
    if(iod_obj_close(obj_oh, NULL, NULL) < 0)
        HGOTO_ERROR_FF(FAIL, "can't close iod object");

    token = calloc(1, token_size);
    buf_ptr = (uint8_t *)token;

    HDmemcpy(buf_ptr, &obj_id, sizeof(iod_obj_id_t));
    buf_ptr += sizeof(iod_obj_id_t);
    HDmemcpy(buf_ptr, &sp[0], sizeof(iod_obj_id_t));
    buf_ptr += sizeof(iod_obj_id_t);
    HDmemcpy(buf_ptr, &sp[1], sizeof(iod_obj_id_t));
    buf_ptr += sizeof(iod_obj_id_t);
    HDmemcpy(buf_ptr, &obj_type, sizeof(H5I_type_t));
    buf_ptr += sizeof(H5I_type_t);

    switch(obj_type) {
    case H5I_GROUP:
        HDmemcpy(buf_ptr, &plist_size, sizeof(size_t));
        buf_ptr += sizeof(size_t);
        if(H5Pencode(cpl_id, buf_ptr, &plist_size) < 0)
            HGOTO_ERROR_FF(FAIL, "can't encode plist");
        buf_ptr += plist_size;
        break;
    case H5I_ATTR:
        HDmemcpy(buf_ptr, &dt_size, sizeof(size_t));
        buf_ptr += sizeof(size_t);
        if(H5Tencode(id1, buf_ptr, &dt_size) < 0)
            HGOTO_ERROR_FF(FAIL, "can't encode datatype");
        buf_ptr += dt_size;

        HDmemcpy(buf_ptr, &space_size, sizeof(size_t));
        buf_ptr += sizeof(size_t);
        if(H5Sencode(id2, buf_ptr, &space_size) < 0)
            HGOTO_ERROR_FF(FAIL, "can't encode dataspace");
        buf_ptr += space_size;
        break;
    case H5I_DATASET:
        HDmemcpy(buf_ptr, &plist_size, sizeof(size_t));
        buf_ptr += sizeof(size_t);
        if(H5Pencode(cpl_id, buf_ptr, &plist_size) < 0)
            HGOTO_ERROR_FF(FAIL, "can't encode plist");
        buf_ptr += plist_size;

        HDmemcpy(buf_ptr, &dt_size, sizeof(size_t));
        buf_ptr += sizeof(size_t);
        if(H5Tencode(id1, buf_ptr, &dt_size) < 0)
            HGOTO_ERROR_FF(FAIL, "can't encode datatype");
        buf_ptr += dt_size;

        HDmemcpy(buf_ptr, &space_size, sizeof(size_t));
        buf_ptr += sizeof(size_t);
        if(H5Sencode(id2, buf_ptr, &space_size) < 0)
            HGOTO_ERROR_FF(FAIL, "can't encode dataspace");
        buf_ptr += space_size;
        break;
    case H5I_DATATYPE:
        HDmemcpy(buf_ptr, &plist_size, sizeof(size_t));
        buf_ptr += sizeof(size_t);
        if(H5Pencode(cpl_id, buf_ptr, &plist_size) < 0)
            HGOTO_ERROR_FF(FAIL, "can't encode plist");
        buf_ptr += plist_size;

        HDmemcpy(buf_ptr, &dt_size, sizeof(size_t));
        buf_ptr += sizeof(size_t);
        HDmemcpy(buf_ptr, dt_buf, dt_size);
        buf_ptr += dt_size;
        break;
    case H5I_MAP:
        HDmemcpy(buf_ptr, &plist_size, sizeof(size_t));
        buf_ptr += sizeof(size_t);
        if(H5Pencode(cpl_id, buf_ptr, &plist_size) < 0)
            HGOTO_ERROR_FF(FAIL, "can't encode plist");
        buf_ptr += plist_size;

        HDmemcpy(buf_ptr, &keytype_size, sizeof(size_t));
        buf_ptr += sizeof(size_t);
        if(H5Tencode(id1, buf_ptr, &keytype_size) < 0)
            HGOTO_ERROR_FF(FAIL, "can't encode datatype");
        buf_ptr += keytype_size;

        HDmemcpy(buf_ptr, &valtype_size, sizeof(size_t));
        buf_ptr += sizeof(size_t);
        if(H5Tencode(id2, buf_ptr, &valtype_size) < 0)
            HGOTO_ERROR_FF(FAIL, "can't encode datatype");
        buf_ptr += valtype_size;
        break;
    default:
        HGOTO_ERROR_FF(FAIL, "Invalid object");
    }

    *_token_size = token_size;
    *_token = token;

done:
    if(dt_buf) {
        free(dt_buf);
        dt_buf = NULL;
    }
    return ret_value;
} /* end H5VL__iod_get_token() */

static hid_t 
H5VL__iod_get_elmt_region(iod_handle_t coh, iod_obj_id_t dset_id, iod_trans_id_t rtid, 
                          hid_t query_id, hid_t vcpl_id, uint32_t cs_scope)
{
    iod_handle_t dset_oh, mdkv_oh;
    scratch_pad sp;
    size_t nelmts;
    size_t elmt_size=0, buf_size=0;
    H5VL__iod_get_query_data_t udata;
    void *buf = NULL;
    hid_t type_id=FAIL, space_id=FAIL, dset_space_id=FAIL;
    hbool_t use_region_scope = TRUE;
    iod_checksum_t sp_cs = 0;
    hid_t ret_value = FAIL;

    /* Check if VCPL has a dataspace specified; otherwise, read the entire dataset. */
    if(H5Pget_view_elmt_scope(vcpl_id, &space_id) < 0)
        HGOTO_ERROR_FF(FAIL, "can't retrieve vcpl region scope");

    /* open the array object */
    if(iod_obj_open_read(coh, dset_id, rtid, NULL, &dset_oh, NULL) < 0)
        HGOTO_ERROR_FF(FAIL, "can't open array object fo read");

    /* get scratch pad */
    if(iod_obj_get_scratch(dset_oh, rtid, &sp, &sp_cs, NULL) < 0)
        HGOTO_ERROR_FF(FAIL, "can't get scratch pad for object");

    if(sp_cs && (cs_scope & H5_CHECKSUM_IOD)) {
        /* verify scratch pad integrity */
        if(H5VL_iod_verify_scratch_pad(&sp, sp_cs) < 0)
            HGOTO_ERROR_FF(FAIL, "Scratch Pad failed integrity check");
    }

    /* open the metadata scratch pad */
    if(iod_obj_open_read(coh, sp[0], rtid, NULL, &mdkv_oh, NULL) < 0)
        HGOTO_ERROR_FF(FAIL, "can't open scratch pad");

    if(H5VL_iod_get_metadata(mdkv_oh, rtid, H5VL_IOD_DATATYPE, 
                             H5VL_IOD_KEY_OBJ_DATATYPE,
                             7, NULL, &type_id) < 0)
        HGOTO_ERROR_FF(FAIL, "failed to retrieve datatype");

    if(H5VL_iod_get_metadata(mdkv_oh, rtid, H5VL_IOD_DATASPACE, 
                             H5VL_IOD_KEY_OBJ_DATASPACE,
                             7, NULL, &dset_space_id) < 0)
        HGOTO_ERROR_FF(FAIL, "failed to retrieve dataspace");

    if(space_id < 0) {
        use_region_scope = FALSE;
        space_id = dset_space_id;
    }

    if(iod_obj_close(mdkv_oh, NULL, NULL) < 0)
        HGOTO_ERROR_FF(FAIL, "can't close iod object");

    nelmts = (size_t) H5Sget_select_npoints(space_id);
    elmt_size = H5Tget_size(type_id);
    buf_size = nelmts * elmt_size;

    /* allocate buffer to hold data */
    if(NULL == (buf = malloc(buf_size)))
        HGOTO_ERROR_FF(FAIL, "can't allocate read buffer");

    /* read the data selection from IOD. */
    if(H5VL__iod_server_final_io(dset_oh, space_id, elmt_size, FALSE, 
                                 buf, buf_size, (uint64_t)0, 0, rtid) < 0)
        HGOTO_ERROR_FF(FAIL, "can't read from array object");

    if(iod_obj_close(dset_oh, NULL, NULL) < 0)
        HGOTO_ERROR_FF(FAIL, "can't close iod object");

    if(FAIL == (udata.space_query = H5Scopy(space_id)))
        HGOTO_ERROR_FF(FAIL, "unable to copy dataspace");
    if(H5Sselect_none(udata.space_query) < 0)
        HGOTO_ERROR_FF(FAIL, "unable to reset selection");

    udata.query_id = query_id;
    udata.num_elmts = 0;

    /* iterate over every element and apply the query on it. If the
       query is not satisfied, then remove it from the query selection */
    if(H5Diterate(buf, type_id, space_id, H5VL__iod_get_query_data_cb, &udata) < 0)
        HGOTO_ERROR_FF(FAIL, "failed to apply query on Dataset");

    ret_value = udata.space_query;

done:
    if(space_id!=FAIL && H5Sclose(space_id) < 0)
        HDONE_ERROR_FF(FAIL, "unable to release dataspace");
    if(use_region_scope) {
        if(dset_space_id!=FAIL && H5Sclose(dset_space_id) < 0)
            HDONE_ERROR_FF(FAIL, "unable to release dataspace");
    }
    if(type_id!=FAIL && H5Tclose(type_id) < 0)
        HDONE_ERROR_FF(FAIL, "unable to release datatype")

    if(buf != NULL)
        free(buf);

    return ret_value;
} /* end H5VL__iod_get_elmt_region() */

/*-------------------------------------------------------------------------
 * Function:    H5VL__iod_get_query_data_cb
 *
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t 
H5VL__iod_get_query_data_cb(void *elem, hid_t type_id, unsigned ndim, 
                            const hsize_t *point, void *_udata)
{
    H5VL__iod_get_query_data_t *udata = (H5VL__iod_get_query_data_t *)_udata;
    hbool_t result = FALSE;
    herr_t ret_value = SUCCEED;

    /* Apply the query */
    if(H5Qapply(udata->query_id, &result, type_id, elem) < 0)
        HGOTO_ERROR_FF(FAIL, "unable to apply query to data element");

    /* If element satisfies query, add it to the selection */
    if (result) {
        hsize_t count[H5S_MAX_RANK], i;
#if 0
        fprintf(stderr, "(%d) Element |%d| matches query\n", my_rank_g, *((int *) elem));
#endif

        udata->num_elmts ++;
        for(i=0 ; i<H5S_MAX_RANK ; i++)
            count[i] = 1;

        if(H5Sselect_hyperslab(udata->space_query, H5S_SELECT_OR, point, NULL, count, NULL) < 0)
        //if(H5Sselect_elements(udata->space_query, H5S_SELECT_APPEND, 1, point) < 0)
            HGOTO_ERROR_FF(FAIL, "unable to add point to selection")
    }

done:
    return ret_value;
} /* end H5VL__iod_get_query_data_cb */

static void
H5VL__iod_add_entry(H5VL_iod_view_list_t *list, H5VL_iod_view_entry_t* entry)
{
    if(list->head == NULL) {
        list->head = entry;
        list->tail = list->head;
    }
    else {
        list->tail->next = entry;
        list->tail = entry;
    }
    list->num_entries ++;

    return;
}

static herr_t
H5VL__iod_free_view_list(H5VL_iod_view_list_t list)
{
    size_t i;
    H5VL_iod_view_entry_t *entry, *next = NULL;
    herr_t ret_value = SUCCEED;

    entry = list.head;

    for(i=0 ; i<list.num_entries ; i++) {
        assert(entry);

        free(entry->token);
        entry->token = NULL;

        if(entry->link_name) {
            free(entry->link_name);
            entry->link_name = NULL;
        }

        if(entry->attr_name) {
            free(entry->attr_name);
            entry->attr_name = NULL;
        }

        if(FAIL != entry->selection) {
            if(H5Sclose(entry->selection) < 0)
                HGOTO_ERROR_FF(FAIL, "can't close dataspace");
        }

        next = entry->next;
        entry->next = NULL;
        free(entry);
        entry = next;
    }

    list.head = NULL;
    list.tail = NULL;

done:

    return ret_value;
}
#endif /* H5_HAVE_EFF */
