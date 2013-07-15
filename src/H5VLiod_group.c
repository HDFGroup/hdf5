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

#define EEXISTS 1


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
    iod_handle_t loc_handle = input->loc_oh; /* The handle for current object - could be undefined */
    iod_obj_id_t loc_id = input->loc_id; /* The ID of the current location object */
    iod_obj_id_t grp_id = input->grp_id; /* The ID of the group that needs to be created */
    const char *name = input->name; /* path relative to loc_id and loc_oh  */
    iod_handle_t grp_oh, cur_oh, mdkv_oh;
    iod_obj_id_t cur_id, mdkv_id, attr_id;
    char *last_comp; /* the name of the group obtained from traversal function */
    iod_kv_t kv;
    scratch_pad_t sp;
    iod_ret_t ret;
    hbool_t collective = FALSE; /* MSC - change when we allow for collective */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Start group create %s\n", name);
#endif

    /* the traversal will retrieve the location where the group needs
       to be created. The traversal will fail if an intermediate group
       does not exist. */
    if(H5VL_iod_server_traverse(coh, loc_id, loc_handle, name, FALSE, 
                                &last_comp, &cur_id, &cur_oh) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't traverse path");

    /* create the group */
    ret = iod_obj_create(coh, IOD_TID_UNKNOWN, NULL, IOD_OBJ_KV, 
                         NULL, NULL, &grp_id, NULL);
    if(collective && (0 == ret || EEXISTS == ret)) {
        /* group has been created by another process, open it */
        if (iod_obj_open_write(coh, grp_id, NULL, &grp_oh, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't open Group");
    }
    else if(!collective && 0 != ret) {
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't create Group");
    }

    /* for the process that succeeded in creating the group, create
       the scratch pad for it too */
    if(0 == ret) {
        /* create the metadata KV object for the group */
        if(iod_obj_create(coh, IOD_TID_UNKNOWN, NULL, IOD_OBJ_KV, 
                          NULL, NULL, &mdkv_id, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't create metadata KV object");

        /* create the attribute KV object for the group */
        if(iod_obj_create(coh, IOD_TID_UNKNOWN, NULL, IOD_OBJ_KV, 
                          NULL, NULL, &attr_id, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't create metadata KV object");

        /* set values for the scratch pad object */
        sp.mdkv_id = mdkv_id;
        sp.attr_id = attr_id;
        sp.filler1_id = IOD_ID_UNDEFINED;
        sp.filler2_id = IOD_ID_UNDEFINED;

        /* set scratch pad in group */
        if (iod_obj_set_scratch(grp_oh, IOD_TID_UNKNOWN, &sp, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't set scratch pad");

        /* Store Metadata in scratch pad */
        if (iod_obj_open_write(coh, mdkv_id, NULL, &mdkv_oh, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't create scratch pad");

        /* MSC - TODO store things */

        if(iod_obj_close(mdkv_oh, NULL, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close object");

        /* insert new group in kv store of parent object */
        kv.key = HDstrdup(last_comp);
        kv.value = &grp_id;
        kv.value_len = sizeof(iod_obj_id_t);
        if (iod_kv_set(cur_oh, IOD_TID_UNKNOWN, NULL, &kv, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't set KV pair in parent");
        HDfree(kv.key);
    } /* end if */

    /* close parent group if it is not the location we started the
       traversal into */
    if(loc_handle.cookie != cur_oh.cookie) {
        iod_obj_close(cur_oh, NULL, NULL);
    }

#if H5_DO_NATIVE
    grp_oh.cookie = H5Gcreate2(loc_handle.cookie, name, input->lcpl_id, 
                        input->gcpl_id, input->gapl_id);
    HDassert(grp_oh.cookie);
#endif

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Done with group create, sending response to client\n");
#endif

    /* return the object handle for the group to the client */
    output.iod_oh = grp_oh;
    HG_Handler_start_output(op_data->hg_handle, &output);

done:
    /* return an UNDEFINED oh to the client if the operation failed */
    if(ret_value < 0) {
        fprintf(stderr, "Failed Group Create\n");
        output.iod_oh.cookie = IOD_OH_UNDEFINED;
        HG_Handler_start_output(op_data->hg_handle, &output);
    }

    last_comp = (char *)H5MM_xfree(last_comp);
    input = (group_create_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

    FUNC_LEAVE_NOAPI_VOID
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
    iod_handle_t loc_handle = input->loc_oh;
    iod_obj_id_t loc_id = input->loc_id;
    const char *name = input->name;
    iod_obj_id_t grp_id; /* The ID of the group that needs to be opened */
    iod_handle_t cur_oh, mdkv_oh;
    iod_obj_id_t cur_id, mdkv_id;
    char *last_comp; /* the name of the group obtained from traversal function */
    iod_size_t kv_size;
    scratch_pad_t sp;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Start group open %s\n", name);
#endif

    /* the traversal will retrieve the location where the group needs
       to be created. The traversal will fail if an intermediate group
       does not exist. */
    if(H5VL_iod_server_traverse(coh, loc_id, loc_handle, name, FALSE, 
                                &last_comp, &cur_id, &cur_oh) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't traverse path");

    kv_size = sizeof(iod_obj_id_t);

    /* lookup group in the current location */
    if(iod_kv_get_value(cur_oh, IOD_TID_UNKNOWN, last_comp, &grp_id, &kv_size, NULL, NULL) < 0) {
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Intermdiate group does not exist");
    } /* end if */

    /* close parent group and its scratch pad if it is not the
       location we started the traversal into */
    if(loc_handle.cookie != cur_oh.cookie) {
        iod_obj_close(cur_oh, NULL, NULL);
    }

    /* open the group */
    if (iod_obj_open_write(coh, grp_id, NULL, &cur_oh, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't open current group");

    /* get scratch pad of group */
    if(iod_obj_get_scratch(cur_oh, IOD_TID_UNKNOWN, &sp, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "can't get scratch pad for object");

    /* open the metadata scratch pad */
    if (iod_obj_open_write(coh, sp.mdkv_id, NULL /*hints*/, &mdkv_oh, NULL) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "can't open scratch pad");

    /* MSC - retrieve metadata */
#if 0 
    /* When we have a real IOD, open the scratch pad and read the
       group's metadata */
    if(iod_kv_get_value(mdkv_oh, IOD_TID_UNKNOWN, "dataset_gcpl", NULL, 
                        &output.gcpl_size, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "dataset gcpl lookup failed");
    if(NULL == (output.gcpl = H5MM_malloc (output.gcpl_size)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate gcpl buffer");
    if(iod_kv_get_value(mdkv_oh, IOD_TID_UNKNOWN, "dataset_gcpl", output.gcpl, 
                        &output.gcpl_size, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "dataset dcpl lookup failed");
#endif

    /* close the metadata scratch pad */
    if(iod_obj_close(mdkv_oh, NULL, NULL))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close meta data KV handle");

#if H5_DO_NATIVE
    cur_oh.cookie = H5Gopen(loc_handle.cookie, name, input->gapl_id);
    HDassert(cur_oh.cookie);
#endif

    output.iod_id = grp_id;
    output.iod_oh = cur_oh;
    output.gcpl_id = H5P_GROUP_CREATE_DEFAULT;

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Done with group open, sending response to client\n");
#endif

    HG_Handler_start_output(op_data->hg_handle, &output);

done:
    if(ret_value < 0) {
        output.iod_oh.cookie = IOD_OH_UNDEFINED;
        output.iod_id = IOD_ID_UNDEFINED;
        output.gcpl_id = H5P_GROUP_CREATE_DEFAULT;
        HG_Handler_start_output(op_data->hg_handle, &output);
    }

    last_comp = (char *)H5MM_xfree(last_comp);
    input = (group_open_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

    FUNC_LEAVE_NOAPI_VOID
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
    iod_handle_t iod_oh = input->iod_oh;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Start group close\n");
#endif

    if(iod_oh.cookie != IOD_OH_UNDEFINED) {
#if H5_DO_NATIVE
        ret_value = H5Gclose(iod_oh.cookie);
#endif
        if((ret_value = iod_obj_close(iod_oh, NULL, NULL)) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close object");
    }
    else {
        /* MSC - need a way to kill object handle for this group */
        fprintf(stderr, "I do not have the OH of this group to close it\n");
    }
done:
#if H5VL_IOD_DEBUG
    fprintf(stderr, "Done with group close, sending response to client\n");
#endif

    HG_Handler_start_output(op_data->hg_handle, &ret_value);

    input = (group_close_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

    FUNC_LEAVE_NOAPI_VOID
} /* end H5VL_iod_server_group_close_cb() */

#endif /* H5_HAVE_EFF */
