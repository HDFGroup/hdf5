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
    iod_handle_t src_oh; /* The handle for creation src object */
    iod_obj_id_t src_id; /* The ID of the creation src object */
    iod_handle_t cur_oh, target_oh;
    iod_obj_id_t cur_id;
    iod_obj_id_t target_id; /* The ID of the target object where link is created*/
    char *src_last_comp = NULL, *dst_last_comp = NULL;
    iod_kv_t kv;
    iod_size_t kv_size = sizeof(iod_obj_id_t);
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Start link create\n");
#endif

    /* the traversal will retrieve the location where the link needs
       to be created from. The traversal will fail if an intermediate group
       does not exist. */
    if(H5VL_iod_server_traverse(coh, input->loc_id, input->loc_oh, input->loc_name, FALSE, 
                                &src_last_comp, &cur_id, &cur_oh) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't traverse path");

    /* lookup group in the current location */
    if(iod_kv_get_value(cur_oh, IOD_TID_UNKNOWN, src_last_comp, &src_id, &kv_size, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Intermdiate group does not exist");

    /* close parent group if it is not the location we started the
       traversal into */
    if(input->loc_oh.cookie != cur_oh.cookie) {
        iod_obj_close(cur_oh, NULL, NULL);
    }

    /* open the source group for link creation*/
    if (iod_obj_open_write(coh, src_id, NULL, &src_oh, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't open current group");

    if(H5VL_LINK_CREATE_HARD == create_type) {
        /* Retrieve the parent of the object where the new link points
           to. The traversal must not fail. */
        if(H5VL_iod_server_traverse(coh, input->target_loc_id, input->target_loc_oh, 
                                    input->target_name, FALSE, 
                                    &dst_last_comp, &cur_id, &cur_oh) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't traverse path");

        /* lookup target object in the current location - the lookup
           must succeed since this is a hard link. */
        if(iod_kv_get_value(cur_oh, IOD_TID_UNKNOWN, dst_last_comp, &target_id, &kv_size, 
                            NULL, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Intermdiate group does not exist");
    }
    else if(H5VL_LINK_CREATE_SOFT == create_type) {
        /* Retrieve the parent of the object where the new link points
           to. The traversal must not fail. */
        if(H5VL_iod_server_traverse(coh, input->target_loc_id, input->target_loc_oh, 
                                    input->target_name, FALSE, 
                                    &dst_last_comp, &cur_id, &cur_oh) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't traverse path");

        /* lookup target object in the current location. The lookup
           might fail since this is a soft link */
        if(iod_kv_get_value(cur_oh, IOD_TID_UNKNOWN, dst_last_comp, 
                            &target_id, &kv_size, NULL, NULL) < 0) {
            /* the lookup failed so just insert the target_id as
               undefined in the src object */
            /* MSC - Figure out what to do when reaccessing this
               object after it was created */
            target_id = IOD_ID_UNDEFINED;
        }
    }
    else
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Invalid Link type");

    /* close parent group if it is not the location we started the
       traversal into */
    if(input->loc_oh.cookie != cur_oh.cookie) {
        iod_obj_close(cur_oh, NULL, NULL);
    }

    /* insert new link (target group's ID) in kv store of the source object */
    kv.key = HDstrdup(src_last_comp);
    kv.value = &target_id;
    kv.value_len = kv_size;
    if (iod_kv_set(src_oh, IOD_TID_UNKNOWN, NULL, &kv, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't set KV pair in parent");
    HDfree(kv.key);

    /* close the source group */
    if(src_oh.cookie != input->loc_oh.cookie) {
        iod_obj_close(src_oh, NULL, NULL);
    }

    /* open the target object */
    if (iod_obj_open_write(coh, target_id, NULL, &target_oh, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't open current group");

    /* MSC - update metadata link information for this object */

    /* close the target object */
    iod_obj_close(target_oh, NULL, NULL);

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
    fprintf(stderr, "Done with link create, sending response to client\n");
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
    iod_handle_t src_oh; /* The handle for src object group */
    iod_obj_id_t src_id; /* The ID of the src object */
    iod_handle_t dst_oh; /* The handle for the dst object where link is created*/
    iod_obj_id_t dst_id; /* The ID of the dst object where link is created*/
    iod_obj_id_t obj_id; /* The ID of the object to be moved/copied */
    char *last_comp = NULL;
    iod_kv_t kv;
    iod_size_t kv_size = sizeof(iod_obj_id_t);
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Start link move\n");
#endif

    /* the traversal will retrieve the location where the link needs
       to be moved/copied from. The traversal will fail if an intermediate group
       does not exist. */
    if(H5VL_iod_server_traverse(coh, input->src_loc_id, input->src_loc_oh, input->src_loc_name, 
                                FALSE, &last_comp, &src_id, &src_oh) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't traverse path");

    /* the traversal will retrieve the location where the link needs
       to be moved/copied to. The traversal will fail if an intermediate group
       does not exist. */
    if(H5VL_iod_server_traverse(coh, input->dst_loc_id, input->dst_loc_oh, input->dst_loc_name, 
                                FALSE, NULL, &dst_id, &dst_oh) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't traverse path");

    /* lookup object ID in the current src location */
    if(iod_kv_get_value(src_oh, IOD_TID_UNKNOWN, last_comp, &obj_id, &kv_size, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Object does not exist in source path");

    /* Insert object in the destination path */
    kv.key = HDstrdup(last_comp);
    kv.value = &obj_id;
    kv.value_len = kv_size;
    if (iod_kv_set(dst_oh, IOD_TID_UNKNOWN, NULL, &kv, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't set KV pair in parent");

    /* if the operation type is a Move, remove the KV pair from the source object */
    if(!copy_flag) {
        iod_kv_params_t kvs;

        kvs.kv = &kv;
        if(iod_kv_unlink_keys(src_oh,IOD_TID_UNKNOWN, NULL, 1, &kvs, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "Unable to unlink KV pair");
    }

    HDfree(kv.key);

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

    last_comp = (char *)H5MM_xfree(last_comp);
    input = (link_move_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

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
    char *last_comp = NULL;
    htri_t ret = -1;
    iod_size_t kv_size = sizeof(iod_obj_id_t);
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Start link Exists\n");
#endif

    /* the traversal will retrieve the location where the link needs
       to be checked */
    if(H5VL_iod_server_traverse(coh, loc_id, loc_oh, loc_name, FALSE, 
                                &last_comp, &cur_id, &cur_oh) < 0) {
        ret = FALSE;
        HGOTO_DONE(SUCCEED);
    }

    /* check the last component */
    if(iod_kv_get_value(cur_oh, IOD_TID_UNKNOWN, last_comp, 
                        &cur_id, &kv_size, NULL, NULL) < 0) {
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
    if(loc_oh.cookie != IOD_OH_UNDEFINED && input->loc_oh.cookie != loc_oh.cookie) {
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
    iod_handle_t cur_oh;
    iod_obj_id_t cur_id, obj_id;
    const char *loc_name = input->path;
    char *last_comp = NULL;
    iod_kv_params_t kvs;
    iod_kv_t kv;
    iod_size_t kv_size = sizeof(iod_obj_id_t);
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Start link Remove\n");
#endif

    /* the traversal will retrieve the location where the link needs
       to be removed. The traversal will fail if an intermediate group
       does not exist. */
    if(H5VL_iod_server_traverse(coh, loc_id, loc_oh, loc_name, 
                                FALSE, &last_comp, &cur_id, &cur_oh) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't traverse path");

    /* lookup object ID in the current location */
    if(iod_kv_get_value(cur_oh, IOD_TID_UNKNOWN, last_comp, &obj_id, &kv_size, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Object does not exist in source path");

    /* unlink object from conainer */
    kv.key = HDstrdup(last_comp);
    kv.value = &obj_id;
    kv.value_len = kv_size;
    kvs.kv = &kv;
    if(iod_kv_unlink_keys(cur_oh, IOD_TID_UNKNOWN, NULL, 1, &kvs, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "Unable to unlink KV pair");
    HDfree(kv.key);

    /* MSC - check the metadata information for the object and remove
       it from the container if this is the last link to it */

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

    FUNC_LEAVE_NOAPI_VOID
} /* end H5VL_iod_server_link_remove_cb() */

#endif /* H5_HAVE_EFF */
