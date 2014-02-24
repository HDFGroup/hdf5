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
#include "H5Vpublic.h"

#ifdef H5_HAVE_EFF

typedef struct {
    hid_t query_id;
    hid_t vcpl_id;
    region_info_t region_info;
    obj_info_t obj_info;
    attr_info_t attr_info;
} H5VL_view_op_t;

static hid_t H5VL__iod_get_elmt_region(iod_handle_t coh, iod_obj_id_t dset_id,
                                       iod_trans_id_t rtid, hid_t query_id, 
                                       hid_t vcpl_id, uint32_t cs_scope, binary_buf_t *token);

static herr_t
H5VL__iod_get_token(H5O_type_t obj_type, iod_obj_id_t iod_id, iod_obj_id_t mdkv_id, 
                    iod_obj_id_t attrkv_id, hid_t cpl_id, hid_t id1, hid_t id2, 
                    binary_buf_t *token);

static herr_t
H5VL__iod_view_iterate_cb(iod_handle_t coh, iod_obj_id_t obj_id, iod_trans_id_t rtid,
                       H5I_type_t obj_type, uint32_t cs_scope, void *_op_data)
{
    H5VL_view_op_t *op_data = (H5VL_view_op_t *)_op_data;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    if(obj_type == H5I_DATASET) {
        hsize_t i = op_data->region_info.count;

        op_data->region_info.tokens = (binary_buf_t *)realloc(op_data->region_info.tokens,
                                                              (i+1) * sizeof(binary_buf_t));
        op_data->region_info.regions = (hid_t *)realloc(op_data->region_info.regions,
                                                        (i+1) * sizeof(hid_t));

        if((op_data->region_info.regions[i] = 
            H5VL__iod_get_elmt_region(coh, obj_id, rtid, op_data->query_id, op_data->vcpl_id,
                                      cs_scope, &op_data->region_info.tokens[i])) < 0)
            HGOTO_ERROR2(H5E_FILE, H5E_CANTINIT, FAIL, "can't get region from query");

        op_data->region_info.count ++;
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
}

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
    iod_handles_t loc_handle = input->loc_oh; /* The handle for current object - could be undefined */
    iod_obj_id_t loc_id = input->loc_id; /* The ID of the current location object */
    iod_obj_id_t mdkv_id = input->loc_mdkv_id; /* The ID of the metadata KV of the location */
    H5I_type_t obj_type = input->obj_type;
    hid_t query_id = input->query_id;
    hid_t vcpl_id;
    iod_trans_id_t rtid = input->rcxt_num;
    uint32_t cs_scope = input->cs_scope;
    hsize_t i;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

#if H5VL_IOD_DEBUG 
    fprintf(stderr, "Start View create on OID %"PRIx64"\n", loc_id);
#endif

    if(H5P_DEFAULT == input->vcpl_id)
        input->vcpl_id = H5Pcopy(H5P_VIEW_CREATE_DEFAULT);
    vcpl_id = input->vcpl_id;

    if(H5I_DATASET == obj_type) {
        output.region_info.count = 1;
        output.region_info.tokens = (binary_buf_t *)malloc(sizeof(binary_buf_t));
        output.region_info.regions = (hid_t *)malloc(sizeof(hid_t));

        if((output.region_info.regions[0] = 
            H5VL__iod_get_elmt_region(coh, loc_id, rtid, query_id, vcpl_id,
                                      cs_scope, output.region_info.tokens)) < 0)
            HGOTO_ERROR2(H5E_FILE, H5E_CANTINIT, FAIL, "can't get region from query");

        output.valid_view = TRUE;
        output.obj_info.count = 0;
        output.obj_info.tokens = NULL;
        output.attr_info.count = 0;
        output.attr_info.tokens = NULL;
    }
    else if (H5I_GROUP == obj_type) {
        H5VL_view_op_t udata;

        udata.query_id = query_id;
        udata.vcpl_id = vcpl_id;

        udata.region_info.count = 0;
        udata.region_info.tokens = NULL;
        udata.region_info.regions = NULL;
        udata.obj_info.count = 0;
        udata.obj_info.tokens = NULL;
        udata.attr_info.count = 0;
        udata.attr_info.tokens = NULL;

        if(H5VL_iod_server_iterate(coh, loc_id, rtid, obj_type, cs_scope, 
                                   H5VL__iod_view_iterate_cb, &udata) < 0)
            HGOTO_ERROR2(H5E_SYM, H5E_CANTINIT, FAIL, "can't iterate to create group");

        output.region_info.count = udata.region_info.count;
        output.region_info.tokens = udata.region_info.tokens;
        output.region_info.regions = udata.region_info.regions;
        output.valid_view = TRUE;
        output.obj_info.count = 0;
        output.obj_info.tokens = NULL;
        output.attr_info.count = 0;
        output.attr_info.tokens = NULL;
    }
    else {
        /* MSC - for now this is only what is supported */
        assert(0);
    }

done:
    if(SUCCEED != ret_value) {
        output.valid_view = FALSE;
        output.region_info.count = 0;
        output.region_info.tokens = NULL;
        output.region_info.regions = NULL;
        output.obj_info.count = 0;
        output.obj_info.tokens = NULL;
        output.attr_info.count = 0;
        output.attr_info.tokens = NULL;
    }

    HG_Handler_start_output(op_data->hg_handle, &output);

    for(i=0 ; i<output.region_info.count ; i++) {
        free(output.region_info.tokens[i].buf);
        H5Sclose(output.region_info.regions[i]);
    }

    for(i=0 ; i<output.obj_info.count ; i++) {
        free(output.obj_info.tokens[i].buf);
    }

    for(i=0 ; i<output.attr_info.count ; i++) {
        free(output.attr_info.tokens[i].buf);
    }

    if(output.region_info.tokens)
        free(output.region_info.tokens);

    if(output.region_info.regions)
        free(output.region_info.regions);

    if(output.obj_info.tokens)
        free(output.obj_info.tokens);

    if(output.attr_info.tokens)
        free(output.attr_info.tokens);

    input = (view_create_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

    FUNC_LEAVE_NOAPI_VOID
} /* end H5VL_iod_server_view_create_cb() */

static hid_t 
H5VL__iod_get_elmt_region(iod_handle_t coh, iod_obj_id_t dset_id,
                          iod_trans_id_t rtid, hid_t query_id, hid_t vcpl_id,
                          uint32_t cs_scope, binary_buf_t *token)
{
    iod_handle_t dset_oh, mdkv_oh;
    scratch_pad sp;
    size_t nelmts;
    size_t elmt_size=0, buf_size=0;
    H5VL__iod_get_query_data_t udata;
    void *buf = NULL;
    hid_t type_id=0, space_id=0, dset_space_id=0, dcpl_id=0;
    hbool_t use_region_scope = TRUE;
    iod_checksum_t sp_cs = 0;
    hid_t ret_value = FAIL;

    FUNC_ENTER_NOAPI_NOINIT

    /* Check if VCPL has a dataspace specified; otherwise, read the entire dataset. */
    if(H5Pget_view_elmt_scope(vcpl_id, &space_id) < 0)
        HGOTO_ERROR2(H5E_PLIST, H5E_CANTGET, FAIL, "can't retrieve vcpl region scope");;

    /* open the array object */
    if(iod_obj_open_read(coh, dset_id, rtid, NULL, &dset_oh, NULL) < 0)
        HGOTO_ERROR2(H5E_SYM, H5E_CANTINIT, FAIL, "can't open array object fo read");

    /* get scratch pad */
    if(iod_obj_get_scratch(dset_oh, rtid, &sp, &sp_cs, NULL) < 0)
        HGOTO_ERROR2(H5E_FILE, H5E_CANTINIT, FAIL, "can't get scratch pad for object");

    if(sp_cs && (cs_scope & H5_CHECKSUM_IOD)) {
        /* verify scratch pad integrity */
        if(H5VL_iod_verify_scratch_pad(&sp, sp_cs) < 0)
            HGOTO_ERROR2(H5E_SYM, H5E_CANTINIT, FAIL, "Scratch Pad failed integrity check");
    }

    /* open the metadata scratch pad */
    if(iod_obj_open_read(coh, sp[0], rtid, NULL, &mdkv_oh, NULL) < 0)
        HGOTO_ERROR2(H5E_FILE, H5E_CANTINIT, FAIL, "can't open scratch pad");

    if(H5VL_iod_get_metadata(mdkv_oh, rtid, H5VL_IOD_DATATYPE, 
                             H5VL_IOD_KEY_OBJ_DATATYPE,
                             7, NULL, &type_id) < 0)
        HGOTO_ERROR2(H5E_SYM, H5E_CANTGET, FAIL, "failed to retrieve datatype");

    if(H5VL_iod_get_metadata(mdkv_oh, rtid, H5VL_IOD_DATASPACE, 
                             H5VL_IOD_KEY_OBJ_DATASPACE,
                             7, NULL, &dset_space_id) < 0)
        HGOTO_ERROR2(H5E_SYM, H5E_CANTGET, FAIL, "failed to retrieve dataspace");

    if(H5VL_iod_get_metadata(mdkv_oh, rtid, H5VL_IOD_PLIST, H5VL_IOD_KEY_OBJ_CPL,
                             cs_scope, NULL, &dcpl_id) < 0)
        HGOTO_ERROR2(H5E_SYM, H5E_CANTGET, FAIL, "failed to retrieve dcpl");

    if(space_id < 0) {
        use_region_scope = FALSE;
        space_id = dset_space_id;
    }

    if(iod_obj_close(mdkv_oh, NULL, NULL) < 0)
        HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close Array object");

    nelmts = (size_t) H5Sget_select_npoints(space_id);
    elmt_size = H5Tget_size(type_id);
    buf_size = nelmts * elmt_size;

    /* allocate buffer to hold data */
    if(NULL == (buf = malloc(buf_size)))
        HGOTO_ERROR2(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate read buffer");

    /* read the data selection from IOD. */
    elmt_size = H5Tget_size(type_id);
    if(H5VL__iod_server_final_io(dset_oh, space_id, elmt_size, FALSE, 
                                 buf, buf_size, (uint64_t)0, 0, rtid) < 0)
        HGOTO_ERROR2(H5E_SYM, H5E_READERROR, FAIL, "can't read from array object");

    if(iod_obj_close(dset_oh, NULL, NULL) < 0)
        HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close Array object");

    if(FAIL == (udata.space_query = H5Scopy(space_id)))
        HGOTO_ERROR2(H5E_DATASPACE, H5E_CANTINIT, FAIL, "unable to copy dataspace");
    if(H5Sselect_none(udata.space_query) < 0)
        HGOTO_ERROR2(H5E_DATASPACE, H5E_CANTINIT, FAIL, "unable to reset selection");

    udata.query_id = query_id;
    udata.num_elmts = 0;

    /* iterate over every element and apply the query on it. If the
       query is not satisfied, then remove it from the query selection */
    if(H5Diterate(buf, type_id, space_id, H5VL__iod_get_query_data_cb, &udata) < 0)
        HGOTO_ERROR2(H5E_SYM, H5E_CANTINIT, FAIL, "failed to compute buffer size");

    ret_value = udata.space_query;

    if(H5VL__iod_get_token(H5O_TYPE_DATASET, dset_id, sp[0], sp[1], dcpl_id, type_id,
                           dset_space_id, token) < 0)
        HGOTO_ERROR2(H5E_SYM, H5E_CANTINIT, FAIL, "failed to get object token");

done:
    if(space_id && H5Sclose(space_id) < 0)
        HDONE_ERROR(H5E_DATASPACE, H5E_CANTRELEASE, FAIL, "unable to release dataspace");
    if(use_region_scope) {
        if(dset_space_id && H5Sclose(dset_space_id) < 0)
            HDONE_ERROR(H5E_DATASPACE, H5E_CANTRELEASE, FAIL, "unable to release dataspace");
    }
    if(type_id && H5Tclose(type_id) < 0)
        HDONE_ERROR(H5E_DATATYPE, H5E_CANTRELEASE, FAIL, "unable to release datatype")
    if(dcpl_id && H5Pclose(dcpl_id) < 0)
        HDONE_ERROR(H5E_DATATYPE, H5E_CANTRELEASE, FAIL, "unable to release property list")

    if(buf != NULL)
        free(buf);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__iod_get_elmt_region() */

static herr_t
H5VL__iod_get_token(H5O_type_t obj_type, iod_obj_id_t iod_id, iod_obj_id_t mdkv_id, 
                    iod_obj_id_t attrkv_id, hid_t cpl_id, hid_t id1, hid_t id2, 
                    binary_buf_t *token)
{
    size_t dt_size = 0, space_size = 0, plist_size = 0;
    size_t keytype_size = 0, valtype_size;
    uint8_t *buf_ptr = NULL;
    size_t token_size = 0;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    token_size = sizeof(iod_obj_id_t)*3 + sizeof(H5O_type_t);

    switch(obj_type) {
    case H5O_TYPE_GROUP:
        if(H5Pencode(cpl_id, NULL, &plist_size) < 0)
            HGOTO_ERROR2(H5E_PLIST, H5E_CANTENCODE, FAIL, "can't encode plist");
        token_size += plist_size + sizeof(size_t);
        break;
    case H5O_TYPE_DATASET:
        if(H5Pencode(cpl_id, NULL, &plist_size) < 0)
            HGOTO_ERROR2(H5E_PLIST, H5E_CANTENCODE, FAIL, "can't encode plist");

        if(H5Tencode(id1, NULL, &dt_size) < 0)
            HGOTO_ERROR2(H5E_DATATYPE, H5E_CANTENCODE, FAIL, "can't encode datatype");

        if(H5Sencode(id2, NULL, &space_size) < 0)
            HGOTO_ERROR2(H5E_DATASPACE, H5E_CANTENCODE, FAIL, "can't encode dataspace");

        token_size += plist_size + dt_size + space_size + sizeof(size_t)*3;
        break;
    case H5O_TYPE_NAMED_DATATYPE:
        if(H5Pencode(cpl_id, NULL, &plist_size) < 0)
            HGOTO_ERROR2(H5E_PLIST, H5E_CANTENCODE, FAIL, "can't encode plist");

        if(H5Tencode(id1, NULL, &dt_size) < 0)
            HGOTO_ERROR2(H5E_DATATYPE, H5E_CANTENCODE, FAIL, "can't encode datatype");

        token_size += plist_size + dt_size + sizeof(size_t)*2;
        break;
    case H5O_TYPE_MAP:
        if(H5Pencode(cpl_id, NULL, &plist_size) < 0)
            HGOTO_ERROR2(H5E_PLIST, H5E_CANTENCODE, FAIL, "can't encode plist");

        if(H5Tencode(id1, NULL, &keytype_size) < 0)
            HGOTO_ERROR2(H5E_DATATYPE, H5E_CANTENCODE, FAIL, "can't encode datatype");

        if(H5Tencode(id2, NULL, &valtype_size) < 0)
            HGOTO_ERROR2(H5E_DATATYPE, H5E_CANTENCODE, FAIL, "can't encode datatype");

        token_size += plist_size + keytype_size + valtype_size + sizeof(size_t)*3;
        break;
    case H5O_TYPE_UNKNOWN:
    case H5O_TYPE_NTYPES:
    default:
        HGOTO_ERROR2(H5E_SYM, H5E_CANTINIT, FAIL, "bad object");
    }

    token->buf_size = token_size;
    token->buf = malloc(token_size);
    buf_ptr = (uint8_t *)token->buf;

    HDmemcpy(buf_ptr, &iod_id, sizeof(iod_obj_id_t));
    buf_ptr += sizeof(iod_obj_id_t);
    HDmemcpy(buf_ptr, &mdkv_id, sizeof(iod_obj_id_t));
    buf_ptr += sizeof(iod_obj_id_t);
    HDmemcpy(buf_ptr, &attrkv_id, sizeof(iod_obj_id_t));
    buf_ptr += sizeof(iod_obj_id_t);
    HDmemcpy(buf_ptr, &obj_type, sizeof(H5O_type_t));
    buf_ptr += sizeof(H5O_type_t);

    switch(obj_type) {
    case H5O_TYPE_GROUP:
        HDmemcpy(buf_ptr, &plist_size, sizeof(size_t));
        buf_ptr += sizeof(size_t);
        if(H5Pencode(cpl_id, buf_ptr, &plist_size) < 0)
            HGOTO_ERROR2(H5E_PLIST, H5E_CANTENCODE, FAIL, "can't encode plist");
        buf_ptr += plist_size;
        break;
    case H5O_TYPE_DATASET:
        HDmemcpy(buf_ptr, &plist_size, sizeof(size_t));
        buf_ptr += sizeof(size_t);
        if(H5Pencode(cpl_id, buf_ptr, &plist_size) < 0)
            HGOTO_ERROR2(H5E_PLIST, H5E_CANTENCODE, FAIL, "can't encode plist");
        buf_ptr += plist_size;

        HDmemcpy(buf_ptr, &dt_size, sizeof(size_t));
        buf_ptr += sizeof(size_t);
        if(H5Tencode(id1, buf_ptr, &dt_size) < 0)
            HGOTO_ERROR2(H5E_DATATYPE, H5E_CANTENCODE, FAIL, "can't encode datatype");
        buf_ptr += dt_size;

        HDmemcpy(buf_ptr, &space_size, sizeof(size_t));
        buf_ptr += sizeof(size_t);
        if(H5Sencode(id2, buf_ptr, &space_size) < 0)
            HGOTO_ERROR2(H5E_DATASPACE, H5E_CANTENCODE, FAIL, "can't encode dataspace");
        buf_ptr += space_size;
        break;
    case H5O_TYPE_NAMED_DATATYPE:
        HDmemcpy(buf_ptr, &plist_size, sizeof(size_t));
        buf_ptr += sizeof(size_t);
        if(H5Pencode(cpl_id, buf_ptr, &plist_size) < 0)
            HGOTO_ERROR2(H5E_PLIST, H5E_CANTENCODE, FAIL, "can't encode plist");
        buf_ptr += plist_size;

        HDmemcpy(buf_ptr, &dt_size, sizeof(size_t));
        buf_ptr += sizeof(size_t);
        if(H5Tencode(id1, buf_ptr, &dt_size) < 0)
            HGOTO_ERROR2(H5E_DATATYPE, H5E_CANTENCODE, FAIL, "can't encode datatype");
        buf_ptr += dt_size;
        break;
    case H5O_TYPE_MAP:
        HDmemcpy(buf_ptr, &plist_size, sizeof(size_t));
        buf_ptr += sizeof(size_t);
        if(H5Pencode(cpl_id, buf_ptr, &plist_size) < 0)
            HGOTO_ERROR2(H5E_PLIST, H5E_CANTENCODE, FAIL, "can't encode plist");
        buf_ptr += plist_size;

        HDmemcpy(buf_ptr, &keytype_size, sizeof(size_t));
        buf_ptr += sizeof(size_t);
        if(H5Tencode(id1, buf_ptr, &keytype_size) < 0)
            HGOTO_ERROR2(H5E_DATATYPE, H5E_CANTENCODE, FAIL, "can't encode datatype");
        buf_ptr += keytype_size;

        HDmemcpy(buf_ptr, &valtype_size, sizeof(size_t));
        buf_ptr += sizeof(size_t);
        if(H5Tencode(id2, buf_ptr, &valtype_size) < 0)
            HGOTO_ERROR2(H5E_DATATYPE, H5E_CANTENCODE, FAIL, "can't encode datatype");
        buf_ptr += valtype_size;
        break;
    case H5O_TYPE_UNKNOWN:
    case H5O_TYPE_NTYPES:
    default:
        HGOTO_ERROR2(H5E_SYM, H5E_CANTINIT, FAIL, "bad object");
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__iod_get_token() */

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
