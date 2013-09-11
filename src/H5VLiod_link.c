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
 * Purpose:	The IOD plugin server side link routines.
 */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_link_create_cb
 *
 * Purpose:	Creates a new link in the container (Hard or Soft).
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
H5VL_iod_server_link_create_cb(AXE_engine_t UNUSED axe_engine, 
                               size_t UNUSED num_n_parents, AXE_task_t UNUSED n_parents[], 
                               size_t UNUSED num_s_parents, AXE_task_t UNUSED s_parents[], 
                               void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    link_create_in_t *input = (link_create_in_t *)op_data->input;
    H5VL_link_create_type_t create_type = input->create_type;
    iod_handle_t coh = input->coh; /* the container handle */
    iod_trans_id_t wtid = input->trans_num;
    iod_trans_id_t rtid = input->rcxt_num;
    iod_handle_t src_oh; /* The handle for creation src object */
    iod_obj_id_t src_id; /* The ID of the creation src object */
    iod_handle_t target_oh;
    iod_obj_id_t target_id; /* The ID of the target object where link is created*/
    char *src_last_comp = NULL, *dst_last_comp = NULL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Start Link create\n");
#endif

    /* the traversal will retrieve the location where the link needs
       to be created from. The traversal will fail if an intermediate group
       does not exist. */
    if(H5VL_iod_server_traverse(coh, input->loc_id, input->loc_oh, input->loc_name, 
                                rtid, FALSE, &src_last_comp, &src_id, &src_oh) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't traverse path");

#if H5VL_IOD_DEBUG
    fprintf(stderr, "new link name = %s\n", src_last_comp);
#endif

    if(H5VL_LINK_CREATE_HARD == create_type) {
        scratch_pad_t sp;
        iod_handle_t mdkv_oh;
        uint64_t link_count = 0;

        /* Traverse Path and open the target object */
        if(H5VL_iod_server_open_path(coh, input->target_loc_id, input->target_loc_oh, 
                                     input->target_name, rtid, &target_id, &target_oh) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't open object");

        /* add link in parent group to current object */
        if(H5VL_iod_insert_new_link(src_oh, wtid, src_last_comp, 
                                    H5L_TYPE_HARD, &target_id, NULL, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't insert KV value");

        /* MSC - must get mdkvID from client, not get here. */
        /* get scratch pad */
        if(iod_obj_get_scratch(target_oh, rtid, &sp, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "can't get scratch pad for object");

        /* open the metadata scratch pad */
        if (iod_obj_open_write(coh, sp.mdkv_id, NULL /*hints*/, &mdkv_oh, NULL) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "can't open scratch pad");

        if(H5VL_iod_get_metadata(mdkv_oh, rtid, H5VL_IOD_LINK_COUNT, 
                                 H5VL_IOD_KEY_OBJ_LINK_COUNT,
                                 NULL, NULL, &link_count) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "failed to retrieve link count");

        link_count ++;

        /* insert link count metadata */
        if(H5VL_iod_insert_link_count(mdkv_oh, wtid, link_count, 
                                      NULL, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't insert KV value");

        /* close the metadata scratch pad */
        if(iod_obj_close(mdkv_oh, NULL, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close object");

        /* close the target location */
        if(input->target_loc_oh.cookie != target_oh.cookie) {
            if(iod_obj_close(target_oh, NULL, NULL))
                HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close object");
        }
    }
    else if(H5VL_LINK_CREATE_SOFT == create_type) {
        /* add link in parent group to the source location */
        if(H5VL_iod_insert_new_link(src_oh, wtid, src_last_comp, 
                                    H5L_TYPE_SOFT, input->link_value, 
                                    NULL, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't insert KV value");

#if H5VL_IOD_DEBUG
        fprintf(stderr, "Soft link Value = %s\n", input->link_value);
#endif
    }
    else
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Invalid Link type");

    /* close the source location */
    if(input->loc_oh.cookie != src_oh.cookie) {
        if(iod_obj_close(src_oh, NULL, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close object");
    }

#if H5_DO_NATIVE
    if(H5VL_LINK_CREATE_HARD == create_type) {
        if(H5Lcreate_hard(input->target_loc_oh.cookie, input->target_name, 
                          input->loc_oh.cookie, input->loc_name, H5P_DEFAULT, H5P_DEFAULT) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't create hard link");
    }
    else if(H5VL_LINK_CREATE_SOFT == create_type) {
        if(H5Lcreate_soft(input->target_name, input->loc_oh.cookie, input->loc_name, 
                          H5P_DEFAULT, H5P_DEFAULT) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't create soft link");
    }
    else
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Invalid Link type");    
#endif

done:
#if H5VL_IOD_DEBUG
    fprintf(stderr, "Done with link create, sending response %d to client\n", ret_value);
#endif

    HG_Handler_start_output(op_data->hg_handle, &ret_value);

    src_last_comp = (char *)H5MM_xfree(src_last_comp);
    dst_last_comp = (char *)H5MM_xfree(dst_last_comp);
    input = (link_create_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

    FUNC_LEAVE_NOAPI_VOID
} /* end H5VL_iod_server_link_create_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_link_move_cb
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
H5VL_iod_server_link_move_cb(AXE_engine_t UNUSED axe_engine, 
                             size_t UNUSED num_n_parents, AXE_task_t UNUSED n_parents[], 
                             size_t UNUSED num_s_parents, AXE_task_t UNUSED s_parents[], 
                             void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    link_move_in_t *input = (link_move_in_t *)op_data->input;
    hbool_t copy_flag = input->copy_flag;
    iod_handle_t coh = input->coh; /* the container handle */
    iod_trans_id_t wtid = input->trans_num;
    iod_trans_id_t rtid = input->rcxt_num;
    iod_handle_t src_oh; /* The handle for src object group */
    iod_obj_id_t src_id; /* The ID of the src object */
    iod_handle_t dst_oh; /* The handle for the dst object where link is created*/
    iod_obj_id_t dst_id; /* The ID of the dst object where link is created*/
    char *src_last_comp = NULL, *dst_last_comp = NULL;
    iod_kv_t kv;
    H5VL_iod_link_t iod_link;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Start link move\n");
#endif

    /* the traversal will retrieve the location where the link needs
       to be moved/copied from. The traversal will fail if an intermediate group
       does not exist. */
    if(H5VL_iod_server_traverse(coh, input->src_loc_id, input->src_loc_oh, input->src_loc_name, 
                                rtid, FALSE, &src_last_comp, &src_id, &src_oh) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't traverse path");

    /* the traversal will retrieve the location where the link needs
       to be moved/copied to. The traversal will fail if an intermediate group
       does not exist. */
    if(H5VL_iod_server_traverse(coh, input->dst_loc_id, input->dst_loc_oh, input->dst_loc_name, 
                                rtid, FALSE, &dst_last_comp, &dst_id, &dst_oh) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't traverse path");

    /* get the link value */
    if(H5VL_iod_get_metadata(src_oh, rtid, H5VL_IOD_LINK, 
                             src_last_comp, NULL, NULL, &iod_link) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "failed to retrieve link value");

    /* Insert object in the destination path */
    if(H5L_TYPE_HARD == iod_link.link_type) {
        if(H5VL_iod_insert_new_link(dst_oh, wtid, dst_last_comp, 
                                    iod_link.link_type, &iod_link.u.iod_id, 
                                    NULL, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't insert KV value");
    }
    else if(H5L_TYPE_SOFT == iod_link.link_type) {
        if(H5VL_iod_insert_new_link(dst_oh, wtid, dst_last_comp, 
                                    iod_link.link_type, &iod_link.u.symbolic_name, 
                                    NULL, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't insert KV value");
    }

    /* if the operation type is a Move, remove the KV pair from the source object */
    if(!copy_flag) {
        iod_kv_params_t kvs;

        kv.key = src_last_comp;
        kvs.kv = &kv;

        /* remove link from source object */
        if(iod_kv_unlink_keys(src_oh, wtid, NULL, (iod_size_t)1, &kvs, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "Unable to unlink KV pair");
    }

    /* adjust link count on target object */
    {
        iod_handle_t target_oh;
        iod_handle_t mdkv_oh;
        scratch_pad_t sp;
        uint64_t link_count = 0;

        /* open the current group */
        if (iod_obj_open_write(coh, iod_link.iod_id, NULL, &target_oh, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't open current group");

        /* get scratch pad */
        if(iod_obj_get_scratch(target_oh, rtid, &sp, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "can't get scratch pad for object");

        /* open the metadata scratch pad */
        if (iod_obj_open_write(coh, sp.mdkv_id, NULL /*hints*/, &mdkv_oh, NULL) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "can't open scratch pad");

        if(H5VL_iod_get_metadata(mdkv_oh, rtid, H5VL_IOD_LINK_COUNT, 
                                 H5VL_IOD_KEY_OBJ_LINK_COUNT,
                                 NULL, NULL, &link_count) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "failed to retrieve link count");

        link_count ++;

        /* insert link count metadata */
        if(H5VL_iod_insert_link_count(mdkv_oh, wtid, link_count, 
                                      NULL, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't insert KV value");

        /* close the metadata scratch pad */
        if(iod_obj_close(mdkv_oh, NULL, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close object");

        /* close the target location */
        if(iod_obj_close(target_oh, NULL, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close object");
    }

    /* close source group if it is not the location we started the
       traversal into */
    if(input->src_loc_oh.cookie != src_oh.cookie) {
        iod_obj_close(src_oh, NULL, NULL);
    }

    /* close parent group if it is not the location we started the
       traversal into */
    if(input->dst_loc_oh.cookie != dst_oh.cookie) {
        iod_obj_close(dst_oh, NULL, NULL);
    }

#if H5_DO_NATIVE
    if(copy_flag) {
        if(H5Lcopy(input->src_loc_oh.cookie, input->src_loc_name, 
                   input->dst_loc_oh.cookie, input->dst_loc_name,
                   H5P_DEFAULT, H5P_DEFAULT) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't create hard link");
    }
    else {
        if(H5Lmove(input->src_loc_oh.cookie, input->src_loc_name, 
                   input->dst_loc_oh.cookie, input->dst_loc_name,
                   H5P_DEFAULT, H5P_DEFAULT) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't create hard link");
    }
#endif

done:
#if H5VL_IOD_DEBUG
    fprintf(stderr, "Done with link move, sending response to client\n");
#endif

    HG_Handler_start_output(op_data->hg_handle, &ret_value);

    src_last_comp = (char *)H5MM_xfree(src_last_comp);
    dst_last_comp = (char *)H5MM_xfree(dst_last_comp);
    input = (link_move_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

    if(iod_link.link_type == H5L_TYPE_SOFT) {
        if(iod_link.u.symbolic_name)
            free(iod_link.u.symbolic_name);
    }

    FUNC_LEAVE_NOAPI_VOID
} /* end H5VL_iod_server_link_move_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_link_exists_cb
 *
 * Purpose:	Checks if a link exists.
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
H5VL_iod_server_link_exists_cb(AXE_engine_t UNUSED axe_engine, 
                               size_t UNUSED num_n_parents, AXE_task_t UNUSED n_parents[], 
                               size_t UNUSED num_s_parents, AXE_task_t UNUSED s_parents[], 
                               void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    link_op_in_t *input = (link_op_in_t *)op_data->input;
    iod_handle_t coh = input->coh;
    iod_handle_t loc_oh = input->loc_oh;
    iod_obj_id_t loc_id = input->loc_id;
    iod_handle_t cur_oh;
    iod_obj_id_t cur_id;
    const char *loc_name = input->path;
    iod_trans_id_t rtid = input->rcxt_num;
    char *last_comp = NULL;
    htri_t ret = -1;
    iod_size_t kv_size = 0;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Start link Exists\n");
#endif

    /* the traversal will retrieve the location where the link needs
       to be checked */
    if(H5VL_iod_server_traverse(coh, loc_id, loc_oh, loc_name, rtid, FALSE, 
                                &last_comp, &cur_id, &cur_oh) < 0) {
        ret = FALSE;
        HGOTO_DONE(SUCCEED);
    }

    /* check the last component */
    if(iod_kv_get_value(cur_oh, rtid, last_comp, 
                        NULL, &kv_size, NULL, NULL) < 0) {
        ret = FALSE;
    } /* end if */
    else {
        ret = TRUE;
    }

#if H5_DO_NATIVE
    ret = H5Lexists(loc_oh.cookie, loc_name, H5P_DEFAULT);
#else
    ret = FALSE;
#endif

done:

    /* close parent group if it is not the location we started the
       traversal into */
    if(input->loc_oh.cookie != cur_oh.cookie) {
        iod_obj_close(loc_oh, NULL, NULL);
    }

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Done with link exists, sending response to client\n");
#endif

    HG_Handler_start_output(op_data->hg_handle, &ret);

    input = (link_op_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);
    last_comp = (char *)H5MM_xfree(last_comp);

    FUNC_LEAVE_NOAPI_VOID
} /* end H5VL_iod_server_link_exists_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_link_get_info_cb
 *
 * Purpose:	Checks if a link get_info.
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
H5VL_iod_server_link_get_info_cb(AXE_engine_t UNUSED axe_engine, 
                                 size_t UNUSED num_n_parents, AXE_task_t UNUSED n_parents[], 
                                 size_t UNUSED num_s_parents, AXE_task_t UNUSED s_parents[], 
                                 void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    link_op_in_t *input = (link_op_in_t *)op_data->input;
    H5L_ff_info_t linfo;
    iod_handle_t coh = input->coh;
    iod_handle_t loc_oh = input->loc_oh;
    iod_obj_id_t loc_id = input->loc_id;
    iod_handle_t cur_oh;
    iod_obj_id_t cur_id;
    const char *loc_name = input->path;
    iod_trans_id_t rtid = input->rcxt_num;
    char *last_comp = NULL;
    H5VL_iod_link_t iod_link;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    cur_oh.cookie = 0;

    /* the traversal will retrieve the location where the link needs
       to be checked */
    if(H5VL_iod_server_traverse(coh, loc_id, loc_oh, loc_name, rtid, FALSE, 
                                &last_comp, &cur_id, &cur_oh) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't traverse path");

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Link Get_Info on link %s\n", last_comp);
#endif
    
    /* lookup link information in the current location */
    if(H5VL_iod_get_metadata(cur_oh, rtid, H5VL_IOD_LINK, 
                             last_comp, NULL, NULL, &iod_link) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "failed to retrieve link value");

    /* setup link info */
    linfo.type = iod_link.link_type;
    linfo.cset = 0;
    switch (linfo.type) {
    case H5L_TYPE_HARD:
        linfo.u.address = iod_link.u.iod_id;
        break;
    case H5L_TYPE_SOFT:
        linfo.u.val_size = strlen(iod_link.u.symbolic_name) + 1;
    default:
        HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "unsuppored link type");
    }

    /* MSC - fake for now */
    linfo.type = H5L_TYPE_SOFT;
    linfo.u.val_size = 10;

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Done with link get_info, sending response to client\n");
#endif

    HG_Handler_start_output(op_data->hg_handle, &linfo);

done:

    if(ret_value < 0) {
        fprintf(stderr, "FAILED link get_info, sending ERROR to client\n");
        linfo.type = H5L_TYPE_ERROR;
        HG_Handler_start_output(op_data->hg_handle, &linfo);
    }

    /* close parent group if it is not the location we started the
       traversal into */
    if(input->loc_oh.cookie != cur_oh.cookie && cur_oh.cookie != 0) {
        iod_obj_close(loc_oh, NULL, NULL);
    }

    input = (link_op_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);
    last_comp = (char *)H5MM_xfree(last_comp);

    if(iod_link.link_type == H5L_TYPE_SOFT) {
        if(iod_link.u.symbolic_name)
            free(iod_link.u.symbolic_name);
    }

    FUNC_LEAVE_NOAPI_VOID
} /* end H5VL_iod_server_link_get_info_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_link_get_val_cb
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
H5VL_iod_server_link_get_val_cb(AXE_engine_t UNUSED axe_engine, 
                                size_t UNUSED num_n_parents, AXE_task_t UNUSED n_parents[], 
                                size_t UNUSED num_s_parents, AXE_task_t UNUSED s_parents[], 
                                void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    link_get_val_in_t *input = (link_get_val_in_t *)op_data->input;
    link_get_val_out_t output;
    iod_handle_t coh = input->coh;
    iod_handle_t loc_oh = input->loc_oh;
    iod_obj_id_t loc_id = input->loc_id;
    size_t length = input->length;
    iod_trans_id_t rtid = input->rcxt_num;
    iod_handle_t cur_oh;
    iod_obj_id_t cur_id;
    const char *loc_name = input->path;
    char *last_comp = NULL;
    ssize_t size = 0;
    H5VL_iod_link_t iod_link;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* the traversal will retrieve the location where the link needs
       to be checked */
    if(H5VL_iod_server_traverse(coh, loc_id, loc_oh, loc_name, rtid, FALSE, 
                                &last_comp, &cur_id, &cur_oh) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't traverse path");

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Link Get_val on link %s\n", last_comp);
#endif
    
    /* lookup link information in the current location */
    if(H5VL_iod_get_metadata(cur_oh, rtid, H5VL_IOD_LINK, 
                             last_comp, NULL, NULL, &iod_link) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "failed to retrieve link value");

    /* MSC - fake for now */
    iod_link.link_type = H5L_TYPE_SOFT;
    iod_link.u.symbolic_name = strdup("FAKE_SOFT");
    size = strlen(iod_link.u.symbolic_name)+1;

    if(H5L_TYPE_SOFT != iod_link.link_type)
        HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "link is not SOFT");

    output.value.val_size = length;
    output.value.val = NULL;

    if(length) {
        if(NULL == (output.value.val = (void *)malloc (length)))
            HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate value buffer");
        memcpy(output.value.val, iod_link.u.symbolic_name, length);
    }

    output.ret = ret_value;
    
    HG_Handler_start_output(op_data->hg_handle, &output);

done:
#if H5VL_IOD_DEBUG
    fprintf(stderr, "Done with get link_val, sending (%s) response to client\n", output.value.val);
#endif
    if(ret_value < 0) {
        output.ret = ret_value;
        output.value.val = NULL;
        output.value.val_size = 0;
        HG_Handler_start_output(op_data->hg_handle, &output);
    }

    input = (link_get_val_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);
    last_comp = (char *)H5MM_xfree(last_comp);

    if(output.value.val)
        free(output.value.val);

    if(iod_link.link_type == H5L_TYPE_SOFT) {
        if(iod_link.u.symbolic_name)
            free(iod_link.u.symbolic_name);
    }

    FUNC_LEAVE_NOAPI_VOID
} /* end H5VL_iod_server_link_get_val_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_link_remove_cb
 *
 * Purpose:	Removes a link from a container.
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
H5VL_iod_server_link_remove_cb(AXE_engine_t UNUSED axe_engine, 
                               size_t UNUSED num_n_parents, AXE_task_t UNUSED n_parents[], 
                               size_t UNUSED num_s_parents, AXE_task_t UNUSED s_parents[], 
                               void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    link_op_in_t *input = (link_op_in_t *)op_data->input;
    iod_handle_t coh = input->coh;
    iod_handle_t loc_oh = input->loc_oh;
    iod_obj_id_t loc_id = input->loc_id;
    iod_trans_id_t wtid = input->trans_num;
    iod_trans_id_t rtid = input->rcxt_num;
    iod_handle_t cur_oh;
    iod_obj_id_t cur_id;
    const char *loc_name = input->path;
    char *last_comp = NULL;
    iod_kv_params_t kvs;
    iod_kv_t kv;
    H5VL_iod_link_t iod_link;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Start link Remove\n");
#endif

    /* the traversal will retrieve the location where the link needs
       to be removed. The traversal will fail if an intermediate group
       does not exist. */
    if(H5VL_iod_server_traverse(coh, loc_id, loc_oh, loc_name, rtid, 
                                FALSE, &last_comp, &cur_id, &cur_oh) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't traverse path");

    /* lookup object ID in the current location */
    if(H5VL_iod_get_metadata(cur_oh, rtid, H5VL_IOD_LINK, 
                             last_comp, NULL, NULL, &iod_link) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "failed to retrieve link value");

    /* unlink object from conainer */
    kv.key = last_comp;
    kvs.kv = &kv;
    if(iod_kv_unlink_keys(cur_oh, wtid, NULL, (iod_size_t)1, &kvs, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "Unable to unlink KV pair");

    /* MSC - NEED IOD */
#if 0
    /* check the metadata information for the object and remove
       it from the container if this is the last link to it */
    if(iod_link.link_type == H5L_TYPE_HARD) {
        iod_handle_t obj_oh;
        iod_handle_t mdkv_oh;
        scratch_pad_t sp;
        uint64_t link_count = 0;

        obj_id = iod_link.u.iod_id;

        /* open the current group */
        if (iod_obj_open_write(coh, obj_id, NULL, &obj_oh, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't open current group");

        /* get scratch pad */
        if(iod_obj_get_scratch(obj_oh, rtid, &sp, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "can't get scratch pad for object");

        /* open the metadata scratch pad */
        if (iod_obj_open_write(coh, sp.mdkv_id, NULL /*hints*/, &mdkv_oh, NULL) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "can't open scratch pad");

        if(H5VL_iod_get_metadata(mdkv_oh, rtid, H5VL_IOD_LINK_COUNT, 
                                 H5VL_IOD_KEY_OBJ_LINK_COUNT,
                                 NULL, NULL, &link_count) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "failed to retrieve link count");

        link_count --;

        /* if this is not the only link to the object, update the link count */
        if(0 != link_count) {
            /* insert link count metadata */
            if(H5VL_iod_insert_link_count(mdkv_oh, wtid, link_count, 
                                          NULL, NULL, NULL) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't insert KV value");
        }
        /* close the metadata scratch pad */
        if(iod_obj_close(mdkv_oh, NULL, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close object");
        /* close the object */
        if(iod_obj_close(obj_oh, NULL, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close object");

        /* If this was the only link to the object, remove the object */
        if(0 == link_count) {
            if(iod_obj_unlink(coh, obj_id, wtid, NULL) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "Unable to unlink object");
            if(iod_obj_unlink(coh, sp.mdkv_id, wtid, NULL) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "Unable to unlink object");
            if(iod_obj_unlink(coh, sp.attrkv_id, wtid, NULL) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "Unable to unlink object");
        }
    }
#endif

    /* close location object */
    if(input->loc_oh.cookie != cur_oh.cookie) {
        iod_obj_close(cur_oh, NULL, NULL);
    }

#if H5_DO_NATIVE
    if(H5Ldelete(loc_oh.cookie, loc_name, H5P_DEFAULT) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "Unable to unlink KV pair");
#endif

done:

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Done with link remove, sending response to client\n");
#endif

    HG_Handler_start_output(op_data->hg_handle, &ret_value);

    last_comp = (char *)H5MM_xfree(last_comp);
    input = (link_op_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

    if(iod_link.link_type == H5L_TYPE_SOFT) {
        if(iod_link.u.symbolic_name)
            free(iod_link.u.symbolic_name);
    }
        
    FUNC_LEAVE_NOAPI_VOID
} /* end H5VL_iod_server_link_remove_cb() */

#endif /* H5_HAVE_EFF */
