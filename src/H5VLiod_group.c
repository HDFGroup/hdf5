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
 * Purpose:	The IOD plugin server side group routines.
 */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_group_create_cb
 *
 * Purpose:	Creates a group as a iod object.
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              February, 2013
 *
 *-------------------------------------------------------------------------
 */
void
H5VL_iod_server_group_create_cb(AXE_engine_t UNUSED axe_engine, 
                                size_t UNUSED num_n_parents, AXE_task_t UNUSED n_parents[], 
                                size_t UNUSED num_s_parents, AXE_task_t UNUSED s_parents[], 
                                void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    group_create_in_t *input = (group_create_in_t *)op_data->input;
    group_create_out_t output;
    iod_handle_t coh = input->coh; /* the container handle */
    iod_handles_t loc_handle = input->loc_oh; /* The handle for current object - could be undefined */
    iod_obj_id_t loc_id = input->loc_id; /* The ID of the current location object */
    iod_obj_id_t grp_id = input->grp_id; /* The ID of the group that needs to be created */
    iod_obj_id_t mdkv_id = input->mdkv_id; /* The ID of the metadata KV to be created */
    iod_obj_id_t attrkv_id = input->attrkv_id; /* The ID of the attirbute KV to be created */
    const char *name = input->name; /* path relative to loc_id and loc_oh  */
    iod_trans_id_t wtid = input->trans_num;
    iod_trans_id_t rtid = input->rcxt_num;
    uint32_t cs_scope = input->cs_scope;
    iod_handles_t grp_oh, cur_oh;
    iod_handle_t mdkv_oh;
    iod_obj_id_t cur_id;
    char *last_comp = NULL; /* the name of the group obtained from traversal function */
    iod_hint_list_t *obj_create_hint = NULL;
    hbool_t enable_checksum = FALSE;
    hid_t gcpl_id;
    scratch_pad sp;
    iod_ret_t ret;
    int step = 0;
    herr_t ret_value = SUCCEED;

#if H5_EFF_DEBUG
    fprintf(stderr, "Start group create %s at %"PRIu64"\n", name, loc_handle.wr_oh.cookie);
#endif

    if(H5P_DEFAULT == input->gcpl_id)
        input->gcpl_id = H5Pcopy(H5P_GROUP_CREATE_DEFAULT);
    gcpl_id = input->gcpl_id;

    /* get the scope for data integrity checks */
    if(H5Pget_ocpl_enable_checksum(gcpl_id, &enable_checksum) < 0)
        HGOTO_ERROR_FF(FAIL, "can't get scope for data integrity checks");

    if((cs_scope & H5_CHECKSUM_IOD) && enable_checksum) {
        obj_create_hint = (iod_hint_list_t *)malloc(sizeof(iod_hint_list_t) + sizeof(iod_hint_t));
        obj_create_hint->num_hint = 1;
        obj_create_hint->hint[0].key = "iod_hint_obj_enable_cksum";
    }

    /* the traversal will retrieve the location where the group needs
       to be created. The traversal will fail if an intermediate group
       does not exist. */
    ret = H5VL_iod_server_traverse(coh, loc_id, loc_handle, name, wtid, rtid, FALSE, cs_scope,
                                   &last_comp, &cur_id, &cur_oh);
    if(ret != SUCCEED)
        HGOTO_ERROR_FF(ret, "can't traverse path");

#if H5_EFF_DEBUG
    fprintf(stderr, "Creating Group ID %"PRIx64" (CV %"PRIu64", TR %"PRIu64") ", 
            grp_id, rtid, wtid);
    fprintf(stderr, "at (OH %"PRIu64" ID %"PRIx64") ", cur_oh.wr_oh.cookie, cur_id);
    if((cs_scope & H5_CHECKSUM_IOD) && enable_checksum)
        fprintf(stderr, "with Data integrity ENABLED\n");
    else
        fprintf(stderr, "with Data integrity DISABLED\n");
#endif

    /* create the group */
    ret = iod_obj_create(coh, wtid, obj_create_hint, IOD_OBJ_KV, 
                         NULL, NULL, &grp_id, NULL);
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "can't create Group");

    ret = iod_obj_open_read(coh, grp_id, wtid, NULL, &grp_oh.rd_oh, NULL);
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "can't open Group for read");
    ret = iod_obj_open_write(coh, grp_id, wtid, NULL, &grp_oh.wr_oh, NULL);
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "can't open Group for write");

    step += 1;

    /* create the metadata KV object for the group */
    ret = iod_obj_create(coh, wtid, obj_create_hint, IOD_OBJ_KV, 
                         NULL, NULL, &mdkv_id, NULL);
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "can't create metadata KV object");

    /* create the attribute KV object for the group */
    ret = iod_obj_create(coh, wtid, obj_create_hint, IOD_OBJ_KV, 
                         NULL, NULL, &attrkv_id, NULL);
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "can't create metadata KV object");

    /* set values for the scratch pad object */
    sp[0] = mdkv_id;
    sp[1] = attrkv_id;
    sp[2] = IOD_OBJ_INVALID;
    sp[3] = IOD_OBJ_INVALID;

    /* set scratch pad in group */
    if(cs_scope & H5_CHECKSUM_IOD) {
        iod_checksum_t sp_cs;

        sp_cs = H5_checksum_crc64(&sp, sizeof(sp));
        ret = iod_obj_set_scratch(grp_oh.wr_oh, wtid, &sp, &sp_cs, NULL);
        if(ret < 0)
            HGOTO_ERROR_FF(ret, "can't set scratch pad");
    }
    else {
        ret = iod_obj_set_scratch(grp_oh.wr_oh, wtid, &sp, NULL, NULL);
        if(ret < 0)
            HGOTO_ERROR_FF(ret, "can't set scratch pad");
    }

    /* store metadata */
    /* Open Metadata KV object for write */
    ret = iod_obj_open_write(coh, mdkv_id, wtid, NULL, &mdkv_oh, NULL);
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "can't create scratch pad");

    step ++;

    /* insert plist metadata */
    ret = H5VL_iod_insert_plist(mdkv_oh, wtid, gcpl_id, cs_scope, NULL, NULL);
    if(ret != SUCCEED)
        HGOTO_ERROR_FF(ret, "can't insert KV value");

    /* insert link count metadata */
    ret = H5VL_iod_insert_link_count(mdkv_oh, wtid, (uint64_t)1,cs_scope, NULL, NULL);
    if(ret != SUCCEED)
        HGOTO_ERROR_FF(ret, "can't insert KV value");

    /* insert object type metadata */
    ret = H5VL_iod_insert_object_type(mdkv_oh, wtid, H5I_GROUP, cs_scope, NULL, NULL);
    if(ret != SUCCEED)
        HGOTO_ERROR_FF(ret, "can't insert KV value");

    /* close Metadata KV object */
    ret = iod_obj_close(mdkv_oh, NULL, NULL);
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "can't close object");

    step --;

    /* add link in parent group to current object */
    ret = H5VL_iod_insert_new_link(cur_oh.wr_oh, wtid, last_comp, H5L_TYPE_HARD, 
                                   &grp_id, cs_scope, NULL, NULL);
    if(ret != SUCCEED)
        HGOTO_ERROR_FF(ret, "can't insert KV value");

#if H5_EFF_DEBUG
    fprintf(stderr, "Done with group create, sending response to client\n");
#endif

    /* return the object handle for the group to the client */
    output.iod_oh.rd_oh.cookie = grp_oh.rd_oh.cookie;
    output.iod_oh.wr_oh.cookie = grp_oh.wr_oh.cookie;
    HG_Handler_start_output(op_data->hg_handle, &output);

    fprintf(stderr, "Created group RD_OH %"PRIu64" WR_OH %"PRIu64"\n", 
            grp_oh.rd_oh.cookie, grp_oh.wr_oh.cookie);

done:

    /* close parent group if it is not the location we started the
       traversal into */
    if(loc_handle.rd_oh.cookie != cur_oh.rd_oh.cookie) {
        iod_obj_close(cur_oh.rd_oh, NULL, NULL);
    }
    if(loc_handle.wr_oh.cookie != cur_oh.wr_oh.cookie) {
        iod_obj_close(cur_oh.wr_oh, NULL, NULL);
    }

    /* return an UNDEFINED oh to the client if the operation failed */
    if(ret_value < 0) {
        fprintf(stderr, "Failed Group Create\n");

        if(step == 2) {
            iod_obj_close(mdkv_oh, NULL, NULL);
            step --;
        }
        if(step == 1) {
            iod_obj_close(grp_oh.rd_oh, NULL, NULL);
            iod_obj_close(grp_oh.wr_oh, NULL, NULL);
        }

        output.iod_oh.rd_oh.cookie = IOD_OH_UNDEFINED;
        output.iod_oh.wr_oh.cookie = IOD_OH_UNDEFINED;
        HG_Handler_start_output(op_data->hg_handle, &output);
    }

    if(obj_create_hint) {
        free(obj_create_hint);
        obj_create_hint = NULL;
    }

    last_comp = (char *)H5MM_xfree(last_comp);
    input = (group_create_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

} /* end H5VL_iod_server_group_create_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_group_open_cb
 *
 * Purpose:	Opens a group as a iod object.
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              February, 2013
 *
 *-------------------------------------------------------------------------
 */
void
H5VL_iod_server_group_open_cb(AXE_engine_t UNUSED axe_engine, 
                              size_t UNUSED num_n_parents, AXE_task_t UNUSED n_parents[], 
                              size_t UNUSED num_s_parents, AXE_task_t UNUSED s_parents[], 
                              void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    group_open_in_t *input = (group_open_in_t *)op_data->input;
    group_open_out_t output;
    iod_handle_t coh = input->coh;
    iod_handles_t loc_handle = input->loc_oh; /* location handle to start lookup */
    iod_obj_id_t loc_id = input->loc_id; /* The ID of the current location object */
    const char *name = input->name; /* group name including path to open */
    iod_trans_id_t rtid = input->rcxt_num;
    uint32_t cs_scope = input->cs_scope;
    iod_obj_id_t grp_id; /* The ID of the group that needs to be opened */
    iod_handles_t grp_oh; /* The group handle */
    iod_handle_t mdkv_oh; /* The metadata KV handle */
    scratch_pad sp;
    iod_checksum_t sp_cs = 0;
    int step = 0;
    iod_ret_t ret;
    herr_t ret_value = SUCCEED;

#if H5_EFF_DEBUG
    fprintf(stderr, "Start group open %s at (OH %"PRIu64" ID %"PRIx64")\n", 
            name, loc_handle.rd_oh.cookie, loc_id);
#endif

    /* if we are opening the root group, no need to traverse */
    if(0 == strcmp(name, "/")) {
        grp_id = ROOT_ID;
        /* open a write handle on the ID. */
        ret = iod_obj_open_read(coh, grp_id, rtid, NULL, &grp_oh.rd_oh, NULL);
        if(ret < 0)
            HGOTO_ERROR_FF(ret, "can't open current group");
    }
    else {
        /* Traverse Path and open group */
        ret = H5VL_iod_server_open_path(coh, loc_id, loc_handle, name, rtid, 
                                        cs_scope, &grp_id, &grp_oh);
        if(ret != SUCCEED)
            HGOTO_ERROR_FF(ret, "can't open object");
    }

    /* open a write handle on the ID. */
    ret = iod_obj_open_write(coh, grp_id, rtid, NULL, &grp_oh.wr_oh, NULL);
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "can't open current group");
    step ++;

    /* get scratch pad of group */
    ret = iod_obj_get_scratch(grp_oh.rd_oh, rtid, &sp, &sp_cs, NULL);
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "can't get scratch pad for object");

    if(sp_cs && (cs_scope & H5_CHECKSUM_IOD)) {
        /* verify scratch pad integrity */
        if(H5VL_iod_verify_scratch_pad(&sp, sp_cs) < 0)
            HGOTO_ERROR_FF(FAIL, "Scratch Pad failed integrity check");
    }

    /* open the metadata scratch pad */
    ret = iod_obj_open_read(coh, sp[0], rtid, NULL, &mdkv_oh, NULL);
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "can't open scratch pad");
    step ++;

    ret = H5VL_iod_get_metadata(mdkv_oh, rtid, H5VL_IOD_PLIST, H5VL_IOD_KEY_OBJ_CPL, 
                                cs_scope, NULL, &output.gcpl_id);
    if(ret != SUCCEED)
        HGOTO_ERROR_FF(ret, "failed to retrieve gcpl");

    /* close the metadata scratch pad */
    ret = iod_obj_close(mdkv_oh, NULL, NULL);
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "can't close meta data KV handle");
    step --;

    output.iod_id = grp_id;
    output.iod_oh.rd_oh.cookie = grp_oh.rd_oh.cookie;
    output.iod_oh.wr_oh.cookie = grp_oh.wr_oh.cookie;
    output.mdkv_id = sp[0];
    output.attrkv_id = sp[1];

#if H5_EFF_DEBUG
    fprintf(stderr, "Done with group open, sending response to client\n");
#endif

    HG_Handler_start_output(op_data->hg_handle, &output);

done:
    if(ret_value < 0) {
        output.iod_oh.rd_oh.cookie = IOD_OH_UNDEFINED;
        output.iod_oh.wr_oh.cookie = IOD_OH_UNDEFINED;
        output.iod_id = IOD_OBJ_INVALID;
        output.gcpl_id = H5P_GROUP_CREATE_DEFAULT;

        if(step == 2) {
            iod_obj_close(mdkv_oh, NULL, NULL);
            step --;
        }
        if(step == 1) {
            iod_obj_close(grp_oh.rd_oh, NULL, NULL);
            iod_obj_close(grp_oh.wr_oh, NULL, NULL);
        }

        HG_Handler_start_output(op_data->hg_handle, &output);
    }

    input = (group_open_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

} /* end H5VL_iod_server_group_open_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_group_close_cb
 *
 * Purpose:	Closes iod HDF5 group.
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
H5VL_iod_server_group_close_cb(AXE_engine_t UNUSED axe_engine, 
                               size_t UNUSED num_n_parents, AXE_task_t UNUSED n_parents[], 
                               size_t UNUSED num_s_parents, AXE_task_t UNUSED s_parents[], 
                               void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    group_close_in_t *input = (group_close_in_t *)op_data->input;
    iod_handles_t iod_oh = input->iod_oh;
    iod_ret_t ret;
    herr_t ret_value = SUCCEED;

#if H5_EFF_DEBUG
    fprintf(stderr, "Start group close\n");
#endif

    if(IOD_OH_UNDEFINED == iod_oh.wr_oh.cookie ||
       IOD_OH_UNDEFINED == iod_oh.rd_oh.cookie) {
        HGOTO_ERROR_FF(FAIL, "can't close object with invalid handle");
    }

    ret = iod_obj_close(iod_oh.rd_oh, NULL, NULL);
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "can't close object");
    ret = iod_obj_close(iod_oh.wr_oh, NULL, NULL);
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "can't close object");

done:
#if H5_EFF_DEBUG
    fprintf(stderr, "Done with group close, sending response to client\n");
#endif

    HG_Handler_start_output(op_data->hg_handle, &ret_value);

    input = (group_close_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

} /* end H5VL_iod_server_group_close_cb() */

#endif /* H5_HAVE_EFF */
