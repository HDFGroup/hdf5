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
 * Purpose:	The IOD plugin server side routines.
 */

static AXE_engine_t engine;
static MPI_Comm iod_comm;
static iod_obj_id_t ROOT_ID;
static int num_peers = 0;
static int terminate_requests = 0;
static hbool_t shutdown = FALSE;

herr_t
H5VLiod_start_handler(MPI_Comm comm, MPI_Info UNUSED info)
{
    na_class_t *network_class = NULL;
    int num_procs;
    AXE_engine_attr_t engine_attr;
    herr_t ret_value = SUCCEED;

    MPI_Comm_size(comm, &num_procs);

    iod_comm = comm;

    /* initialize the netwrok class */
    network_class = NA_MPI_Init(NULL, MPI_INIT_SERVER);
    if(HG_SUCCESS != HG_Handler_init(network_class))
        return FAIL;
    if(HG_SUCCESS != HG_Bulk_init(network_class))
        return FAIL;

    /* Register function and encoding/decoding functions */
    MERCURY_HANDLER_REGISTER("eff_init", H5VL_iod_server_eff_init, 
                             eff_init_in_t, ret_t);
    MERCURY_HANDLER_REGISTER("eff_finalize", H5VL_iod_server_eff_finalize, 
                             ret_t, ret_t);

    MERCURY_HANDLER_REGISTER("file_create", H5VL_iod_server_file_create, 
                             file_create_in_t, file_create_out_t);
    MERCURY_HANDLER_REGISTER("file_open", H5VL_iod_server_file_open, 
                             file_open_in_t, file_open_out_t);
    MERCURY_HANDLER_REGISTER("file_flush", H5VL_iod_server_file_flush, 
                             file_flush_in_t, ret_t);
    MERCURY_HANDLER_REGISTER("file_close", H5VL_iod_server_file_close, 
                             file_close_in_t, ret_t);

    MERCURY_HANDLER_REGISTER("attr_create", H5VL_iod_server_attr_create, 
                             attr_create_in_t, attr_create_out_t);
    MERCURY_HANDLER_REGISTER("attr_open", H5VL_iod_server_attr_open, 
                             attr_open_in_t, attr_open_out_t);
    MERCURY_HANDLER_REGISTER("attr_read", H5VL_iod_server_attr_read, 
                             attr_io_in_t, ret_t);
    MERCURY_HANDLER_REGISTER("attr_write", H5VL_iod_server_attr_write, 
                             attr_io_in_t, ret_t);
    MERCURY_HANDLER_REGISTER("attr_exists", H5VL_iod_server_attr_exists, 
                             attr_op_in_t, htri_t);
    MERCURY_HANDLER_REGISTER("attr_rename", H5VL_iod_server_attr_rename, 
                             attr_rename_in_t, ret_t);
    MERCURY_HANDLER_REGISTER("attr_remove", H5VL_iod_server_attr_remove, 
                             attr_op_in_t, ret_t);
    MERCURY_HANDLER_REGISTER("attr_close", H5VL_iod_server_attr_close, 
                             attr_close_in_t, ret_t);

    MERCURY_HANDLER_REGISTER("group_create", H5VL_iod_server_group_create, 
                             group_create_in_t, group_create_out_t);
    MERCURY_HANDLER_REGISTER("group_open", H5VL_iod_server_group_open, 
                             group_open_in_t, group_open_out_t);
    MERCURY_HANDLER_REGISTER("group_close", H5VL_iod_server_group_close, 
                             group_close_in_t, ret_t);

    MERCURY_HANDLER_REGISTER("map_create", H5VL_iod_server_map_create,
                             map_create_in_t, map_create_out_t);
    MERCURY_HANDLER_REGISTER("map_open", H5VL_iod_server_map_open,
                             map_open_in_t, map_open_out_t);
    MERCURY_HANDLER_REGISTER("map_set", H5VL_iod_server_map_set,
                             map_set_in_t, ret_t);
    MERCURY_HANDLER_REGISTER("map_get", H5VL_iod_server_map_get,
                             map_get_in_t, map_get_out_t);
    MERCURY_HANDLER_REGISTER("map_get_count", H5VL_iod_server_map_get_count,
                             map_get_count_in_t, int64_t);
    MERCURY_HANDLER_REGISTER("map_exists", H5VL_iod_server_map_exists,
                             map_op_in_t, hbool_t);
    MERCURY_HANDLER_REGISTER("map_delete", H5VL_iod_server_map_delete,
                             map_op_in_t, ret_t);
    MERCURY_HANDLER_REGISTER("map_close", H5VL_iod_server_map_close,
                             map_close_in_t, ret_t);

    MERCURY_HANDLER_REGISTER("dset_create", H5VL_iod_server_dset_create, 
                             dset_create_in_t, dset_create_out_t);
    MERCURY_HANDLER_REGISTER("dset_open", H5VL_iod_server_dset_open, 
                             dset_open_in_t, dset_open_out_t);
    MERCURY_HANDLER_REGISTER("dset_read", H5VL_iod_server_dset_read, 
                             dset_io_in_t, dset_read_out_t);
    MERCURY_HANDLER_REGISTER("dset_get_vl_size", H5VL_iod_server_dset_get_vl_size, 
                             dset_get_vl_size_in_t, dset_read_out_t);
    MERCURY_HANDLER_REGISTER("dset_write", H5VL_iod_server_dset_write, 
                             dset_io_in_t, ret_t);
    MERCURY_HANDLER_REGISTER("dset_set_extent", H5VL_iod_server_dset_set_extent, 
                             dset_set_extent_in_t, ret_t);
    MERCURY_HANDLER_REGISTER("dset_close", H5VL_iod_server_dset_close, 
                             dset_close_in_t, ret_t);

    MERCURY_HANDLER_REGISTER("dtype_commit", H5VL_iod_server_dtype_commit, 
                             dtype_commit_in_t, dtype_commit_out_t);
    MERCURY_HANDLER_REGISTER("dtype_open", H5VL_iod_server_dtype_open, 
                             dtype_open_in_t, dtype_open_out_t);
    MERCURY_HANDLER_REGISTER("dtype_close", H5VL_iod_server_dtype_close, 
                             dtype_close_in_t, ret_t);

    MERCURY_HANDLER_REGISTER("link_create", H5VL_iod_server_link_create, 
                             link_create_in_t, ret_t);
    MERCURY_HANDLER_REGISTER("link_move", H5VL_iod_server_link_move, 
                             link_move_in_t, ret_t);
    MERCURY_HANDLER_REGISTER("link_exists", H5VL_iod_server_link_exists, 
                             link_op_in_t, htri_t);
    MERCURY_HANDLER_REGISTER("link_get_info", H5VL_iod_server_link_get_info, 
                             link_op_in_t, linfo_t);
    MERCURY_HANDLER_REGISTER("link_get_val", H5VL_iod_server_link_get_val, 
                             link_get_val_in_t, link_get_val_out_t);
    MERCURY_HANDLER_REGISTER("link_iterate", H5VL_iod_server_link_iterate, 
                             link_op_in_t, ret_t);
    MERCURY_HANDLER_REGISTER("link_remove", H5VL_iod_server_link_remove, 
                             link_op_in_t, ret_t);

    MERCURY_HANDLER_REGISTER("object_open", H5VL_iod_server_object_open, 
                             object_op_in_t, object_open_out_t);
    MERCURY_HANDLER_REGISTER("object_copy", H5VL_iod_server_object_copy, 
                             object_copy_in_t, ret_t);
    MERCURY_HANDLER_REGISTER("object_exists", H5VL_iod_server_object_exists, 
                             object_op_in_t, htri_t);
    MERCURY_HANDLER_REGISTER("object_visit", H5VL_iod_server_object_visit, 
                             object_op_in_t, ret_t);
    MERCURY_HANDLER_REGISTER("set_comment", H5VL_iod_server_object_set_comment, 
                             object_set_comment_in_t, ret_t);
    MERCURY_HANDLER_REGISTER("get_comment", H5VL_iod_server_object_get_comment, 
                             object_get_comment_in_t, object_get_comment_out_t);
    MERCURY_HANDLER_REGISTER("object_get_info", H5VL_iod_server_object_get_info,
                             object_op_in_t, oinfo_t);

    MERCURY_HANDLER_REGISTER("cancel_op", H5VL_iod_server_cancel_op, uint64_t, uint8_t);

    /* Initialize engine attribute */
    if(AXEengine_attr_init(&engine_attr) != AXE_SUCCEED)
        return FAIL;

    /* Set number of threads in AXE engine */
    if(AXEset_num_threads(&engine_attr, 1) != AXE_SUCCEED)
        return FAIL;

    /* Create AXE engine */
    if(AXEcreate_engine(&engine, &engine_attr) != AXE_SUCCEED)
        return FAIL;

    /* Loop tp receive requests from clients */
    while(1) {
        HG_Handler_process(0, HG_STATUS_IGNORE);
        if(shutdown)
            break;
    }

    if(AXE_SUCCEED != AXEterminate_engine(engine, TRUE))
        return FAIL;

    if(AXEengine_attr_destroy(&engine_attr) != AXE_SUCCEED)
        return FAIL;

    if(HG_SUCCESS != HG_Bulk_finalize())
        return FAIL;
    if(HG_SUCCESS != HG_Handler_finalize())
        return FAIL;
    if(NA_SUCCESS != NA_Finalize(network_class))
        return FAIL;

    return ret_value;
}


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_eff_init
 *
 * Purpose:	Function shipper registered call for initializing the eff stack.
 *              this will initialize the IOD library
 *
 * Return:	Success:	HG_SUCCESS 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              January, 2013
 *
 *-------------------------------------------------------------------------
 */
int
H5VL_iod_server_eff_init(hg_handle_t handle)
{
    int num_procs;
    int ret_value = HG_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    /* get the input from the client connecting */
    if(HG_FAIL == HG_Handler_get_input(handle, &num_procs))
	HGOTO_ERROR(H5E_FILE, H5E_CANTGET, HG_FAIL, "can't get input parameters");

    /* initialize the IOD library */
    if(iod_initialize(iod_comm, NULL, num_procs, num_procs, NULL) < 0 )
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, HG_FAIL, "can't initialize");

    /* MSC - this needs to be changed to be the number of peers connecting to this server */
    num_peers = num_procs;

done:
    HG_Handler_start_output(handle, &ret_value);
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_eff_init() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_eff_finalize
 *
 * Purpose:	Function to shutdown server
 *
 * Return:	Success:	HG_SUCCESS 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              January, 2013
 *
 *-------------------------------------------------------------------------
 */
int
H5VL_iod_server_eff_finalize(hg_handle_t handle)
{
    int ret_value = HG_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    /* increment the number of terminate requests received so far */
    terminate_requests ++;

    /* if all the peers that connected at the beginning have sent the
       terminate request, then finalize IOD and indicate that it is
       time to shutdown the server */
    if(terminate_requests == num_peers) {
        if(iod_finalize(NULL, NULL) < 0 )
            HGOTO_ERROR(H5E_FILE, H5E_CANTDEC, HG_FAIL, "can't finalize IOD");
        shutdown = TRUE;
    }

done:
    HG_Handler_start_output(handle, &ret_value);
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_eff_finalize() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_cancel_op
 *
 * Purpose:	Function to cancel an AXE operation
 *
 * Return:	Success:	HG_SUCCESS
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              May, 2013
 *
 *-------------------------------------------------------------------------
 */
int
H5VL_iod_server_cancel_op(hg_handle_t handle)
{
    AXE_task_t axe_id;
    AXE_remove_status_t remove_status;
    H5_status_t ret_value = H5AO_PENDING;

    FUNC_ENTER_NOAPI_NOINIT

    if(HG_FAIL == HG_Handler_get_input(handle, &axe_id))
	HGOTO_ERROR(H5E_SYM, H5E_CANTGET, HG_FAIL, "can't get input parameters");

    /* Try to remove the task. */
    if(AXEremove(engine, axe_id, &remove_status) != AXE_SUCCEED)
        HGOTO_ERROR(H5E_SYM, H5E_CANTREMOVE, HG_FAIL, "can't remove AXE task; it has children");

    if(remove_status == AXE_CANCELED)
        HGOTO_DONE(H5VL_IOD_CANCELLED)
    else if(remove_status == AXE_ALL_DONE)
        HGOTO_DONE(H5VL_IOD_COMPLETED)
    else if(remove_status == AXE_NOT_CANCELED) {
        void *op_data;

        fprintf(stderr, "Task is running. Attempting to cancel Manually\n");
        if(AXEget_op_data(engine, axe_id, &op_data) != AXE_SUCCEED)
            HGOTO_ERROR(H5E_SYM, H5E_CANTGET, HG_FAIL, "can't get op data");
        /* Attempt to cancel the task manually */
    }
done:
    HG_Handler_start_output(handle, &ret_value);
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_cancel_op() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_file_create
 *
 * Purpose:	Function shipper registered call for File Create.
 *              Inserts the real worker routine into the Async Engine.
 *
 * Return:	Success:	HG_SUCCESS 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              January, 2013
 *
 *-------------------------------------------------------------------------
 */
int
H5VL_iod_server_file_create(hg_handle_t handle)
{
    op_data_t *op_data = NULL;
    file_create_in_t *input = NULL;
    int ret_value = HG_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (op_data = (op_data_t *)H5MM_malloc(sizeof(op_data_t))))
	HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, HG_FAIL, "can't allocate axe op_data struct");

    if(NULL == (input = (file_create_in_t *)H5MM_malloc(sizeof(file_create_in_t))))
	HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, HG_FAIL, "can't allocate input struct for decoding");

    if(HG_FAIL == HG_Handler_get_input(handle, input))
	HGOTO_ERROR(H5E_FILE, H5E_CANTGET, HG_FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "AXE engine not started");

    op_data->hg_handle = handle;
    op_data->input = (void *)input;

    if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 0, NULL, 0, NULL, 
                                      H5VL_iod_server_file_create_cb, op_data, NULL))
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_file_create() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_file_open
 *
 * Purpose:	Function shipper registered call for File Open.
 *              Inserts the real worker routine into the Async Engine.
 *
 * Return:	Success:	HG_SUCCESS 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              January, 2013
 *
 *-------------------------------------------------------------------------
 */
int
H5VL_iod_server_file_open(hg_handle_t handle)
{
    op_data_t *op_data = NULL;
    file_open_in_t *input = NULL;
    int ret_value = HG_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (op_data = (op_data_t *)H5MM_malloc(sizeof(op_data_t))))
	HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, HG_FAIL, "can't allocate axe op_data struct");

    if(NULL == (input = (file_open_in_t *) H5MM_malloc(sizeof(file_open_in_t))))
	HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, HG_FAIL, "can't allocate input struct for decoding");

    if(HG_FAIL == HG_Handler_get_input(handle, input))
	HGOTO_ERROR(H5E_FILE, H5E_CANTGET, HG_FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "AXE engine not started");

    op_data->hg_handle = handle;
    op_data->input = (void *)input;

    if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 0, NULL, 0, NULL, 
                                      H5VL_iod_server_file_open_cb, op_data, NULL))
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_file_open() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_file_flush
 *
 * Purpose:	Function shipper registered call for File Flush.
 *              Inserts the real worker routine into the Async Engine.
 *
 * Return:	Success:	HG_SUCCESS 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              January, 2013
 *
 *-------------------------------------------------------------------------
 */
int
H5VL_iod_server_file_flush(hg_handle_t handle)
{
    op_data_t *op_data = NULL;
    file_flush_in_t *input = NULL;
    int ret_value = HG_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (op_data = (op_data_t *)H5MM_malloc(sizeof(op_data_t))))
	HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, HG_FAIL, "can't allocate axe op_data struct");

    if(NULL == (input = (file_flush_in_t *) H5MM_malloc(sizeof(file_flush_in_t))))
	HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, HG_FAIL, "can't allocate input struct for decoding");

    if(HG_FAIL == HG_Handler_get_input(handle, input))
	HGOTO_ERROR(H5E_FILE, H5E_CANTGET, HG_FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "AXE engine not started");

    op_data->hg_handle = handle;
    op_data->input = (void *)input;

    if (AXE_SUCCEED != AXEcreate_barrier_task(engine, input->axe_id,
                                              H5VL_iod_server_file_flush_cb, op_data, NULL))
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_file_flush() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_file_close
 *
 * Purpose:	Function shipper registered call for File Close.
 *              Inserts the real worker routine into the Async Engine.
 *
 * Return:	Success:	HG_SUCCESS 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              January, 2013
 *
 *-------------------------------------------------------------------------
 */
int
H5VL_iod_server_file_close(hg_handle_t handle)
{
    op_data_t *op_data = NULL;
    file_close_in_t *input = NULL;
    int ret_value = HG_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (op_data = (op_data_t *)H5MM_malloc(sizeof(op_data_t))))
	HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, HG_FAIL, "can't allocate axe op_data struct");

    if(NULL == (input = (file_close_in_t *)
                H5MM_malloc(sizeof(file_close_in_t))))
	HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, HG_FAIL, "can't allocate input struct for decoding");

    if(HG_FAIL == HG_Handler_get_input(handle, input))
	HGOTO_ERROR(H5E_FILE, H5E_CANTGET, HG_FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "AXE engine not started");

    op_data->hg_handle = handle;
    op_data->input = (void *)input;

    if (AXE_SUCCEED != AXEcreate_barrier_task(engine, input->axe_id,
                                              H5VL_iod_server_file_close_cb, op_data, NULL))
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_file_close() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_attr_create
 *
 * Purpose:	Function shipper registered call for Attr Create.
 *              Inserts the real worker routine into the Async Engine.
 *
 * Return:	Success:	HG_SUCCESS 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              April, 2013
 *
 *-------------------------------------------------------------------------
 */
int
H5VL_iod_server_attr_create(hg_handle_t handle)
{
    op_data_t *op_data = NULL;
    attr_create_in_t *input = NULL;
    int ret_value = HG_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (op_data = (op_data_t *)H5MM_malloc(sizeof(op_data_t))))
	HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, HG_FAIL, "can't allocate axe op_data struct");

    if(NULL == (input = (attr_create_in_t *)
                H5MM_malloc(sizeof(attr_create_in_t))))
	HGOTO_ERROR(H5E_ATTR, H5E_NOSPACE, HG_FAIL, "can't allocate input struct for decoding");

    if(HG_FAIL == HG_Handler_get_input(handle, input))
	HGOTO_ERROR(H5E_ATTR, H5E_CANTGET, HG_FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "AXE engine not started");

    op_data->hg_handle = handle;
    op_data->input = (void *)input;

    if(input->parent_axe_id) {
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 
                                          1, &input->parent_axe_id, 0, NULL, 
                                          H5VL_iod_server_attr_create_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }
    else {
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 0, NULL, 0, NULL, 
                                          H5VL_iod_server_attr_create_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_attr_create() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_attr_open
 *
 * Purpose:	Function shipper registered call for Attr Open.
 *              Inserts the real worker routine into the Async Engine.
 *
 * Return:	Success:	HG_SUCCESS 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              April, 2013
 *
 *-------------------------------------------------------------------------
 */
int
H5VL_iod_server_attr_open(hg_handle_t handle)
{
    op_data_t *op_data = NULL;
    attr_open_in_t *input = NULL;
    int ret_value = HG_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (op_data = (op_data_t *)H5MM_malloc(sizeof(op_data_t))))
	HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, HG_FAIL, "can't allocate axe op_data struct");

    if(NULL == (input = (attr_open_in_t *)
                H5MM_malloc(sizeof(attr_open_in_t))))
	HGOTO_ERROR(H5E_ATTR, H5E_NOSPACE, HG_FAIL, "can't allocate input struct for decoding");

    if(HG_FAIL == HG_Handler_get_input(handle, input))
	HGOTO_ERROR(H5E_ATTR, H5E_CANTGET, HG_FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "AXE engine not started");

    op_data->hg_handle = handle;
    op_data->input = (void *)input;

    if(input->parent_axe_id) {
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 
                                          1, &input->parent_axe_id, 0, NULL, 
                                          H5VL_iod_server_attr_open_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }
    else {
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 0, NULL, 0, NULL, 
                                          H5VL_iod_server_attr_open_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_attr_open() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_attr_read
 *
 * Purpose:	Function shipper registered call for Attr Read.
 *              Inserts the real worker routine into the Async Engine.
 *
 * Return:	Success:	HG_SUCCESS 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              April, 2013
 *
 *-------------------------------------------------------------------------
 */
int
H5VL_iod_server_attr_read(hg_handle_t handle)
{
    op_data_t *op_data = NULL;
    attr_io_in_t *input = NULL;
    int ret_value = HG_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (op_data = (op_data_t *)H5MM_malloc(sizeof(op_data_t))))
	HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, HG_FAIL, "can't allocate axe op_data struct");

    if(NULL == (input = (attr_io_in_t *)
                H5MM_malloc(sizeof(attr_io_in_t))))
	HGOTO_ERROR(H5E_ATTR, H5E_NOSPACE, HG_FAIL, "can't allocate input struct for decoding");

    if(HG_FAIL == HG_Handler_get_input(handle, input))
	HGOTO_ERROR(H5E_ATTR, H5E_CANTGET, HG_FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "AXE engine not started");

    op_data->hg_handle = handle;
    op_data->input = (void *)input;

    if(input->parent_axe_id) {
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 
                                          1, &input->parent_axe_id, 0, NULL, 
                                          H5VL_iod_server_attr_read_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }
    else {
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 0, NULL, 0, NULL, 
                                          H5VL_iod_server_attr_read_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_attr_read() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_attr_write
 *
 * Purpose:	Function shipper registered call for Attr Write.
 *              Inserts the real worker routine into the Async Engine.
 *
 * Return:	Success:	HG_SUCCESS 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              April, 2013
 *
 *-------------------------------------------------------------------------
 */
int
H5VL_iod_server_attr_write(hg_handle_t handle)
{
    op_data_t *op_data = NULL;
    attr_io_in_t *input = NULL;
    int ret_value = HG_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (op_data = (op_data_t *)H5MM_malloc(sizeof(op_data_t))))
	HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, HG_FAIL, "can't allocate axe op_data struct");

    if(NULL == (input = (attr_io_in_t *)
                H5MM_malloc(sizeof(attr_io_in_t))))
	HGOTO_ERROR(H5E_ATTR, H5E_NOSPACE, HG_FAIL, "can't allocate input struct for decoding");

    if(HG_FAIL == HG_Handler_get_input(handle, input))
	HGOTO_ERROR(H5E_ATTR, H5E_CANTGET, HG_FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "AXE engine not started");

    op_data->hg_handle = handle;
    op_data->input = (void *)input;

    if(input->parent_axe_id) {
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 
                                          1, &input->parent_axe_id, 0, NULL, 
                                          H5VL_iod_server_attr_write_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }
    else {
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 0, NULL, 0, NULL, 
                                          H5VL_iod_server_attr_write_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_attr_write() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_attr_exists
 *
 * Purpose:	Function shipper registered call for Attr Exists.
 *              Inserts the real worker routine into the Async Engine.
 *
 * Return:	Success:	HG_SUCCESS 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              April, 2013
 *
 *-------------------------------------------------------------------------
 */
int
H5VL_iod_server_attr_exists(hg_handle_t handle)
{
    op_data_t *op_data = NULL;
    attr_op_in_t *input = NULL;
    int ret_value = HG_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (op_data = (op_data_t *)H5MM_malloc(sizeof(op_data_t))))
	HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, HG_FAIL, "can't allocate axe op_data struct");

    if(NULL == (input = (attr_op_in_t *)
                H5MM_malloc(sizeof(attr_op_in_t))))
	HGOTO_ERROR(H5E_ATTR, H5E_NOSPACE, HG_FAIL, "can't allocate input struct for decoding");

    if(HG_FAIL == HG_Handler_get_input(handle, input))
	HGOTO_ERROR(H5E_ATTR, H5E_CANTGET, HG_FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "AXE engine not started");

    op_data->hg_handle = handle;
    op_data->input = (void *)input;

    if(input->parent_axe_id) {
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 
                                          1, &input->parent_axe_id, 0, NULL, 
                                          H5VL_iod_server_attr_exists_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }
    else {
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 0, NULL, 0, NULL, 
                                          H5VL_iod_server_attr_exists_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_attr_exists() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_attr_rename
 *
 * Purpose:	Function shipper registered call for Attr Rename.
 *              Inserts the real worker routine into the Async Engine.
 *
 * Return:	Success:	HG_SUCCESS 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              April, 2013
 *
 *-------------------------------------------------------------------------
 */
int
H5VL_iod_server_attr_rename(hg_handle_t handle)
{
    op_data_t *op_data = NULL;
    attr_rename_in_t *input = NULL;
    int ret_value = HG_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (op_data = (op_data_t *)H5MM_malloc(sizeof(op_data_t))))
	HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, HG_FAIL, "can't allocate axe op_data struct");

    if(NULL == (input = (attr_rename_in_t *)
                H5MM_malloc(sizeof(attr_rename_in_t))))
	HGOTO_ERROR(H5E_ATTR, H5E_NOSPACE, HG_FAIL, "can't allocate input struct for decoding");

    if(HG_FAIL == HG_Handler_get_input(handle, input))
	HGOTO_ERROR(H5E_ATTR, H5E_CANTGET, HG_FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "AXE engine not started");

    op_data->hg_handle = handle;
    op_data->input = (void *)input;

    if(input->parent_axe_id) {
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 
                                          1, &input->parent_axe_id, 0, NULL, 
                                          H5VL_iod_server_attr_rename_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }
    else {
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 0, NULL, 0, NULL, 
                                          H5VL_iod_server_attr_rename_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_attr_rename() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_attr_remove
 *
 * Purpose:	Function shipper registered call for Attr Remove.
 *              Inserts the real worker routine into the Async Engine.
 *
 * Return:	Success:	HG_SUCCESS 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              April, 2013
 *
 *-------------------------------------------------------------------------
 */
int
H5VL_iod_server_attr_remove(hg_handle_t handle)
{
    op_data_t *op_data = NULL;
    attr_op_in_t *input = NULL;
    int ret_value = HG_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (op_data = (op_data_t *)H5MM_malloc(sizeof(op_data_t))))
	HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, HG_FAIL, "can't allocate axe op_data struct");

    if(NULL == (input = (attr_op_in_t *)
                H5MM_malloc(sizeof(attr_op_in_t))))
	HGOTO_ERROR(H5E_ATTR, H5E_NOSPACE, HG_FAIL, "can't allocate input struct for decoding");

    if(HG_FAIL == HG_Handler_get_input(handle, input))
	HGOTO_ERROR(H5E_ATTR, H5E_CANTGET, HG_FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "AXE engine not started");

    op_data->hg_handle = handle;
    op_data->input = (void *)input;

    if(input->parent_axe_id) {
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 
                                          1, &input->parent_axe_id, 0, NULL, 
                                          H5VL_iod_server_attr_remove_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }
    else {
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 0, NULL, 0, NULL, 
                                          H5VL_iod_server_attr_remove_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_attr_remove() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_attr_close
 *
 * Purpose:	Function shipper registered call for Attr Close.
 *              Inserts the real worker routine into the Async Engine.
 *
 * Return:	Success:	HG_SUCCESS 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              April, 2013
 *
 *-------------------------------------------------------------------------
 */
int
H5VL_iod_server_attr_close(hg_handle_t handle)
{
    op_data_t *op_data = NULL;
    attr_close_in_t *input = NULL;
    int ret_value = HG_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (op_data = (op_data_t *)H5MM_malloc(sizeof(op_data_t))))
	HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, HG_FAIL, "can't allocate axe op_data struct");

    if(NULL == (input = (attr_close_in_t *)
                H5MM_malloc(sizeof(attr_close_in_t))))
	HGOTO_ERROR(H5E_ATTR, H5E_NOSPACE, HG_FAIL, "can't allocate input struct for decoding");

    if(HG_FAIL == HG_Handler_get_input(handle, input))
	HGOTO_ERROR(H5E_ATTR, H5E_CANTGET, HG_FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "AXE engine not started");

    op_data->hg_handle = handle;
    op_data->input = (void *)input;

    if(input->parent_axe_ids.count) {
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 
                                          input->parent_axe_ids.count, input->parent_axe_ids.ids, 
                                          0, NULL, 
                                          H5VL_iod_server_attr_close_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }
    else {
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 0, NULL, 0, NULL, 
                                          H5VL_iod_server_attr_close_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_attr_close() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_group_create
 *
 * Purpose:	Function shipper registered call for Group Create.
 *              Inserts the real worker routine into the Async Engine.
 *
 * Return:	Success:	HG_SUCCESS 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              January, 2013
 *
 *-------------------------------------------------------------------------
 */
int
H5VL_iod_server_group_create(hg_handle_t handle)
{
    op_data_t *op_data = NULL;
    group_create_in_t *input;
    int ret_value = HG_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (op_data = (op_data_t *)H5MM_malloc(sizeof(op_data_t))))
	HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, HG_FAIL, "can't allocate axe op_data struct");

    if(NULL == (input = (group_create_in_t *)H5MM_malloc(sizeof(group_create_in_t))))
	HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, HG_FAIL, "can't allocate axe op_data struct");

    if(HG_FAIL == HG_Handler_get_input(handle, input))
	HGOTO_ERROR(H5E_SYM, H5E_CANTGET, HG_FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "AXE engine not started");

    op_data->hg_handle = handle;
    op_data->input = (void *)input;

    if(input->parent_axe_id) {
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 
                                          1, &input->parent_axe_id, 0, NULL, 
                                          H5VL_iod_server_group_create_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }
    else {
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 0, NULL, 0, NULL, 
                                          H5VL_iod_server_group_create_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_group_create() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_group_open
 *
 * Purpose:	Function shipper registered call for Group Open.
 *              Inserts the real worker routine into the Async Engine.
 *
 * Return:	Success:	HG_SUCCESS 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              January, 2013
 *
 *-------------------------------------------------------------------------
 */
int
H5VL_iod_server_group_open(hg_handle_t handle)
{
    op_data_t *op_data = NULL;
    group_open_in_t *input;
    int ret_value = HG_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (op_data = (op_data_t *)H5MM_malloc(sizeof(op_data_t))))
	HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, HG_FAIL, "can't allocate axe op_data struct");

    if(NULL == (input = (group_open_in_t *)H5MM_malloc(sizeof(group_open_in_t))))
	HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, HG_FAIL, "can't allocate axe op_data struct");

    if(HG_FAIL == HG_Handler_get_input(handle, input))
	HGOTO_ERROR(H5E_SYM, H5E_CANTGET, HG_FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "AXE engine not started");

    op_data->hg_handle = handle;
    op_data->input = (void *)input;

    if(input->parent_axe_id) {
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 
                                          1, &input->parent_axe_id, 0, NULL, 
                                          H5VL_iod_server_group_open_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }
    else {
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 0, NULL, 0, NULL, 
                                          H5VL_iod_server_group_open_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_group_open() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_group_close
 *
 * Purpose:	Function shipper registered call for Group Close.
 *              Inserts the real worker routine into the Async Engine.
 *
 * Return:	Success:	HG_SUCCESS 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              January, 2013
 *
 *-------------------------------------------------------------------------
 */
int
H5VL_iod_server_group_close(hg_handle_t handle)
{
    op_data_t *op_data = NULL;
    group_close_in_t *input;
    int ret_value = HG_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (op_data = (op_data_t *)H5MM_malloc(sizeof(op_data_t))))
	HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, HG_FAIL, "can't allocate axe op_data struct");

    if(NULL == (input = (group_close_in_t *)H5MM_malloc(sizeof(group_close_in_t))))
	HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, HG_FAIL, "can't allocate axe op_data struct");

    if(HG_FAIL == HG_Handler_get_input(handle, input))
	HGOTO_ERROR(H5E_SYM, H5E_CANTGET, HG_FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "AXE engine not started");

    op_data->hg_handle = handle;
    op_data->input = (void *)input;

    if(input->parent_axe_id) {
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 
                                          1, &input->parent_axe_id, 0, NULL, 
                                          H5VL_iod_server_group_close_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }
    else {
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 0, NULL, 0, NULL, 
                                          H5VL_iod_server_group_close_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_group_close() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_dset_create
 *
 * Purpose:	Function shipper registered call for Dset Create.
 *              Inserts the real worker routine into the Async Engine.
 *
 * Return:	Success:	HG_SUCCESS 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              January, 2013
 *
 *-------------------------------------------------------------------------
 */
int
H5VL_iod_server_dset_create(hg_handle_t handle)
{
    op_data_t *op_data = NULL;
    dset_create_in_t *input = NULL;
    int ret_value = HG_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (op_data = (op_data_t *)H5MM_malloc(sizeof(op_data_t))))
	HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, HG_FAIL, "can't allocate axe op_data struct");

    if(NULL == (input = (dset_create_in_t *)
                H5MM_malloc(sizeof(dset_create_in_t))))
	HGOTO_ERROR(H5E_DATASET, H5E_NOSPACE, HG_FAIL, "can't allocate input struct for decoding");

    if(HG_FAIL == HG_Handler_get_input(handle, input))
	HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, HG_FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "AXE engine not started");

    op_data->hg_handle = handle;
    op_data->input = (void *)input;

    if(input->parent_axe_id) {
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 
                                          1, &input->parent_axe_id, 0, NULL, 
                                          H5VL_iod_server_dset_create_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }
    else {
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 0, NULL, 0, NULL, 
                                          H5VL_iod_server_dset_create_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_dset_create() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_dset_open
 *
 * Purpose:	Function shipper registered call for Dset Open.
 *              Inserts the real worker routine into the Async Engine.
 *
 * Return:	Success:	HG_SUCCESS 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              January, 2013
 *
 *-------------------------------------------------------------------------
 */
int
H5VL_iod_server_dset_open(hg_handle_t handle)
{
    op_data_t *op_data = NULL;
    dset_open_in_t *input = NULL;
    int ret_value = HG_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (op_data = (op_data_t *)H5MM_malloc(sizeof(op_data_t))))
	HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, HG_FAIL, "can't allocate axe op_data struct");

    if(NULL == (input = (dset_open_in_t *)
                H5MM_malloc(sizeof(dset_open_in_t))))
	HGOTO_ERROR(H5E_DATASET, H5E_NOSPACE, HG_FAIL, "can't allocate input struct for decoding");

    if(HG_FAIL == HG_Handler_get_input(handle, input))
	HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, HG_FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "AXE engine not started");

    op_data->hg_handle = handle;
    op_data->input = (void *)input;

    if(input->parent_axe_id) {
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 
                                          1, &input->parent_axe_id, 0, NULL, 
                                          H5VL_iod_server_dset_open_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }
    else {
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 0, NULL, 0, NULL, 
                                          H5VL_iod_server_dset_open_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_dset_open() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_dset_read
 *
 * Purpose:	Function shipper registered call for Dset Read.
 *              Inserts the real worker routine into the Async Engine.
 *
 * Return:	Success:	HG_SUCCESS 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              January, 2013
 *
 *-------------------------------------------------------------------------
 */
int
H5VL_iod_server_dset_read(hg_handle_t handle)
{
    op_data_t *op_data = NULL;
    dset_io_in_t *input = NULL;
    int ret_value = HG_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (op_data = (op_data_t *)H5MM_malloc(sizeof(op_data_t))))
	HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, HG_FAIL, "can't allocate axe op_data struct");

    if(NULL == (input = (dset_io_in_t *)
                H5MM_malloc(sizeof(dset_io_in_t))))
	HGOTO_ERROR(H5E_DATASET, H5E_NOSPACE, HG_FAIL, "can't allocate input struct for decoding");

    if(HG_FAIL == HG_Handler_get_input(handle, input))
	HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, HG_FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "AXE engine not started");

    op_data->hg_handle = handle;
    op_data->input = (void *)input;

    if(input->parent_axe_id) {
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 
                                          1, &input->parent_axe_id, 0, NULL, 
                                          H5VL_iod_server_dset_read_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }
    else {
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 0, NULL, 0, NULL, 
                                          H5VL_iod_server_dset_read_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_dset_read() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_dset_get_vl_size
 *
 * Purpose:	Function shipper registered call for dset get VL buffer size.
 *              Inserts the real worker routine into the Async Engine.
 *
 * Return:	Success:	HG_SUCCESS 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              January, 2013
 *
 *-------------------------------------------------------------------------
 */
int
H5VL_iod_server_dset_get_vl_size(hg_handle_t handle)
{
    op_data_t *op_data = NULL;
    dset_get_vl_size_in_t *input = NULL;
    int ret_value = HG_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (op_data = (op_data_t *)H5MM_malloc(sizeof(op_data_t))))
	HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, HG_FAIL, "can't allocate axe op_data struct");

    if(NULL == (input = (dset_get_vl_size_in_t *)
                H5MM_malloc(sizeof(dset_get_vl_size_in_t))))
	HGOTO_ERROR(H5E_DATASET, H5E_NOSPACE, HG_FAIL, "can't allocate input struct for decoding");

    if(HG_FAIL == HG_Handler_get_input(handle, input))
	HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, HG_FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "AXE engine not started");

    op_data->hg_handle = handle;
    op_data->input = (void *)input;

    if(input->parent_axe_id) {
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 
                                          1, &input->parent_axe_id, 0, NULL, 
                                          H5VL_iod_server_dset_get_vl_size_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }
    else {
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 0, NULL, 0, NULL, 
                                          H5VL_iod_server_dset_get_vl_size_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_dset_get_vl_size() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_dset_write
 *
 * Purpose:	Function shipper registered call for Dset Write.
 *              Inserts the real worker routine into the Async Engine.
 *
 * Return:	Success:	HG_SUCCESS 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              January, 2013
 *
 *-------------------------------------------------------------------------
 */
int
H5VL_iod_server_dset_write(hg_handle_t handle)
{
    op_data_t *op_data = NULL;
    dset_io_in_t *input = NULL;
    int ret_value = HG_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (op_data = (op_data_t *)H5MM_malloc(sizeof(op_data_t))))
	HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, HG_FAIL, "can't allocate axe op_data struct");

    if(NULL == (input = (dset_io_in_t *)
                H5MM_malloc(sizeof(dset_io_in_t))))
	HGOTO_ERROR(H5E_DATASET, H5E_NOSPACE, HG_FAIL, "can't allocate input struct for decoding");

    if(HG_FAIL == HG_Handler_get_input(handle, input))
	HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, HG_FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "AXE engine not started");

    op_data->hg_handle = handle;
    op_data->input = (void *)input;

    if(input->parent_axe_id) {
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 
                                          1, &input->parent_axe_id, 0, NULL, 
                                          H5VL_iod_server_dset_write_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }
    else {
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 0, NULL, 0, NULL, 
                                          H5VL_iod_server_dset_write_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_dset_write() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_dset_set_extent
 *
 * Purpose:	Function shipper registered call for Dset Set_Extent.
 *              Inserts the real worker routine into the Async Engine.
 *
 * Return:	Success:	HG_SUCCESS 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              January, 2013
 *
 *-------------------------------------------------------------------------
 */
int
H5VL_iod_server_dset_set_extent(hg_handle_t handle)
{
    op_data_t *op_data = NULL;
    dset_set_extent_in_t *input = NULL;
    int ret_value = HG_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (op_data = (op_data_t *)H5MM_malloc(sizeof(op_data_t))))
	HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, HG_FAIL, "can't allocate axe op_data struct");

    if(NULL == (input = (dset_set_extent_in_t *)
                H5MM_malloc(sizeof(dset_set_extent_in_t))))
	HGOTO_ERROR(H5E_DATASET, H5E_NOSPACE, HG_FAIL, "can't allocate input struct for decoding");

    if(HG_FAIL == HG_Handler_get_input(handle, input))
	HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, HG_FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "AXE engine not started");

    op_data->hg_handle = handle;
    op_data->input = (void *)input;

    if(input->parent_axe_ids.count) {
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 
                                          input->parent_axe_ids.count, input->parent_axe_ids.ids, 
                                          0, NULL, 
                                          H5VL_iod_server_dset_set_extent_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }
    else {
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 0, NULL, 0, NULL, 
                                          H5VL_iod_server_dset_set_extent_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_dset_set_extent() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_dset_close
 *
 * Purpose:	Function shipper registered call for Dset Close.
 *              Inserts the real worker routine into the Async Engine.
 *
 * Return:	Success:	HG_SUCCESS 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              January, 2013
 *
 *-------------------------------------------------------------------------
 */
int
H5VL_iod_server_dset_close(hg_handle_t handle)
{
    op_data_t *op_data = NULL;
    dset_close_in_t *input = NULL;
    int ret_value = HG_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (op_data = (op_data_t *)H5MM_malloc(sizeof(op_data_t))))
	HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, HG_FAIL, "can't allocate axe op_data struct");

    if(NULL == (input = (dset_close_in_t *)
                H5MM_malloc(sizeof(dset_close_in_t))))
	HGOTO_ERROR(H5E_DATASET, H5E_NOSPACE, HG_FAIL, "can't allocate input struct for decoding");

    if(HG_FAIL == HG_Handler_get_input(handle, input))
	HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, HG_FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "AXE engine not started");

    op_data->hg_handle = handle;
    op_data->input = (void *)input;

    if(input->parent_axe_ids.count) {
#if 0
        int i;
        AXE_status_t status;
        for(i=0 ; i<input->parent_axe_ids.count ; i++) {
            if(AXEget_status(engine, input->parent_axe_ids.ids[i], &status) < 0) {
                fprintf(stderr, "GET STATUS FAILED\n");
                exit(1);
            }
            fprintf(stderr, "%d: AXE ID %llu status %d\n", 
                    i, input->parent_axe_ids.ids[i], status);
        }
#endif
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 
                                          input->parent_axe_ids.count, input->parent_axe_ids.ids, 
                                          0, NULL, 
                                          H5VL_iod_server_dset_close_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }
    else {
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 0, NULL, 0, NULL, 
                                          H5VL_iod_server_dset_close_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_dset_close() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_dtype_commit
 *
 * Purpose:	Function shipper registered call for Dtype Commit.
 *              Inserts the real worker routine into the Async Engine.
 *
 * Return:	Success:	HG_SUCCESS 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              April, 2013
 *
 *-------------------------------------------------------------------------
 */
int
H5VL_iod_server_dtype_commit(hg_handle_t handle)
{
    op_data_t *op_data = NULL;
    dtype_commit_in_t *input = NULL;
    int ret_value = HG_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (op_data = (op_data_t *)H5MM_malloc(sizeof(op_data_t))))
	HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, HG_FAIL, "can't allocate axe op_data struct");

    if(NULL == (input = (dtype_commit_in_t *)
                H5MM_malloc(sizeof(dtype_commit_in_t))))
	HGOTO_ERROR(H5E_DATATYPE, H5E_NOSPACE, HG_FAIL, "can't allocate input struct for decoding");

    if(HG_FAIL == HG_Handler_get_input(handle, input))
	HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, HG_FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "AXE engine not started");

    op_data->hg_handle = handle;
    op_data->input = (void *)input;

    if(input->parent_axe_id) {
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 
                                          1, &input->parent_axe_id, 0, NULL, 
                                          H5VL_iod_server_dtype_commit_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }
    else {
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 0, NULL, 0, NULL, 
                                          H5VL_iod_server_dtype_commit_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_dtype_commit() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_dtype_open
 *
 * Purpose:	Function shipper registered call for Dtype Open.
 *              Inserts the real worker routine into the Async Engine.
 *
 * Return:	Success:	HG_SUCCESS 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              April, 2013
 *
 *-------------------------------------------------------------------------
 */
int
H5VL_iod_server_dtype_open(hg_handle_t handle)
{
    op_data_t *op_data = NULL;
    dtype_open_in_t *input = NULL;
    int ret_value = HG_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (op_data = (op_data_t *)H5MM_malloc(sizeof(op_data_t))))
	HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, HG_FAIL, "can't allocate axe op_data struct");

    if(NULL == (input = (dtype_open_in_t *)
                H5MM_malloc(sizeof(dtype_open_in_t))))
	HGOTO_ERROR(H5E_DATATYPE, H5E_NOSPACE, HG_FAIL, "can't allocate input struct for decoding");

    if(HG_FAIL == HG_Handler_get_input(handle, input))
	HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, HG_FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "AXE engine not started");

    op_data->hg_handle = handle;
    op_data->input = (void *)input;

    if(input->parent_axe_id) {
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 
                                          1, &input->parent_axe_id, 0, NULL, 
                                          H5VL_iod_server_dtype_open_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }
    else {
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 0, NULL, 0, NULL, 
                                          H5VL_iod_server_dtype_open_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_dtype_open() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_dtype_close
 *
 * Purpose:	Function shipper registered call for Dtype Close.
 *              Inserts the real worker routine into the Async Engine.
 *
 * Return:	Success:	HG_SUCCESS 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              April, 2013
 *
 *-------------------------------------------------------------------------
 */
int
H5VL_iod_server_dtype_close(hg_handle_t handle)
{
    op_data_t *op_data = NULL;
    dtype_close_in_t *input = NULL;
    int ret_value = HG_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (op_data = (op_data_t *)H5MM_malloc(sizeof(op_data_t))))
	HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, HG_FAIL, "can't allocate axe op_data struct");

    if(NULL == (input = (dtype_close_in_t *)
                H5MM_malloc(sizeof(dtype_close_in_t))))
	HGOTO_ERROR(H5E_DATATYPE, H5E_NOSPACE, HG_FAIL, "can't allocate input struct for decoding");

    if(HG_FAIL == HG_Handler_get_input(handle, input))
	HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, HG_FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "AXE engine not started");

    op_data->hg_handle = handle;
    op_data->input = (void *)input;

    if(input->parent_axe_id) {
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 
                                          1, &input->parent_axe_id, 0, NULL, 
                                          H5VL_iod_server_dtype_close_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }
    else {
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 0, NULL, 0, NULL, 
                                          H5VL_iod_server_dtype_close_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_dtype_close() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_link_create
 *
 * Purpose:	Function shipper registered call for Link Creation.
 *              Inserts the real worker routine into the Async Engine.
 *
 * Return:	Success:	HG_SUCCESS 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              May, 2013
 *
 *-------------------------------------------------------------------------
 */
int
H5VL_iod_server_link_create(hg_handle_t handle)
{
    op_data_t *op_data = NULL;
    link_create_in_t *input = NULL;
    int ret_value = HG_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (op_data = (op_data_t *)H5MM_malloc(sizeof(op_data_t))))
	HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, HG_FAIL, "can't allocate axe op_data struct");

    if(NULL == (input = (link_create_in_t *)
                H5MM_malloc(sizeof(link_create_in_t))))
	HGOTO_ERROR(H5E_DATATYPE, H5E_NOSPACE, HG_FAIL, "can't allocate input struct for decoding");

    if(HG_FAIL == HG_Handler_get_input(handle, input))
	HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, HG_FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "AXE engine not started");

    op_data->hg_handle = handle;
    op_data->input = (void *)input;

    if(input->parent_axe_id && input->target_parent_axe_id) {
        AXE_task_t tasks[2] = {input->parent_axe_id, input->target_parent_axe_id};

        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 
                                          2, tasks, 0, NULL, 
                                          H5VL_iod_server_link_create_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }
    else if(input->parent_axe_id){
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 
                                          1, &input->parent_axe_id, 0, NULL, 
                                          H5VL_iod_server_link_create_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }
    else if(input->target_parent_axe_id){
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 
                                          1, &input->target_parent_axe_id, 0, NULL, 
                                          H5VL_iod_server_link_create_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }
    else {
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 0, NULL, 0, NULL, 
                                          H5VL_iod_server_link_create_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_link_create() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_link_move
 *
 * Purpose:	Function shipper registered call for Link Move.
 *              Inserts the real worker routine into the Async Engine.
 *
 * Return:	Success:	HG_SUCCESS 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              May, 2013
 *
 *-------------------------------------------------------------------------
 */
int
H5VL_iod_server_link_move(hg_handle_t handle)
{
    op_data_t *op_data = NULL;
    link_move_in_t *input = NULL;
    int ret_value = HG_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (op_data = (op_data_t *)H5MM_malloc(sizeof(op_data_t))))
	HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, HG_FAIL, "can't allocate axe op_data struct");

    if(NULL == (input = (link_move_in_t *)
                H5MM_malloc(sizeof(link_move_in_t))))
	HGOTO_ERROR(H5E_DATATYPE, H5E_NOSPACE, HG_FAIL, "can't allocate input struct for decoding");

    if(HG_FAIL == HG_Handler_get_input(handle, input))
	HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, HG_FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "AXE engine not started");

    op_data->hg_handle = handle;
    op_data->input = (void *)input;

    if(input->src_parent_axe_id && input->dst_parent_axe_id) {
        AXE_task_t tasks[2] = {input->src_parent_axe_id, input->dst_parent_axe_id};

        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 
                                          2, tasks, 0, NULL, 
                                          H5VL_iod_server_link_move_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }
    else if(input->src_parent_axe_id){
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 
                                          1, &input->src_parent_axe_id, 0, NULL, 
                                          H5VL_iod_server_link_move_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }
    else if(input->dst_parent_axe_id){
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 
                                          1, &input->dst_parent_axe_id, 0, NULL, 
                                          H5VL_iod_server_link_move_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }
    else {
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 0, NULL, 0, NULL, 
                                          H5VL_iod_server_link_move_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_link_move() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_link_iterate
 *
 * Purpose:	Function shipper registered call for Link Iteration.
 *              Inserts the real worker routine into the Async Engine.
 *
 * Return:	Success:	HG_SUCCESS 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              May, 2013
 *
 *-------------------------------------------------------------------------
 */
int
H5VL_iod_server_link_iterate(hg_handle_t handle)
{
    op_data_t *op_data = NULL;
    int ret_value = HG_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

#if 0
    if(NULL == (op_data = (op_data_t *)H5MM_malloc(sizeof(op_data_t))))
	HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, HG_FAIL, "can't allocate axe op_data struct");

    if(NULL == (input = (link_iterate_in_t *)
                H5MM_malloc(sizeof(link_iterate_in_t))))
	HGOTO_ERROR(H5E_DATATYPE, H5E_NOSPACE, HG_FAIL, "can't allocate input struct for decoding");

    if(HG_FAIL == HG_Handler_get_input(handle, input))
	HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, HG_FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "AXE engine not started");

    op_data->hg_handle = handle;
    op_data->input = (void *)input;
#endif

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_link_iterate() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_link_exists
 *
 * Purpose:	Function shipper registered call for Link Existance.
 *              Inserts the real worker routine into the Async Engine.
 *
 * Return:	Success:	HG_SUCCESS 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              May, 2013
 *
 *-------------------------------------------------------------------------
 */
int
H5VL_iod_server_link_exists(hg_handle_t handle)
{
    op_data_t *op_data = NULL;
    link_op_in_t *input = NULL;
    int ret_value = HG_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (op_data = (op_data_t *)H5MM_malloc(sizeof(op_data_t))))
	HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, HG_FAIL, "can't allocate axe op_data struct");

    if(NULL == (input = (link_op_in_t *)
                H5MM_malloc(sizeof(link_op_in_t))))
	HGOTO_ERROR(H5E_DATATYPE, H5E_NOSPACE, HG_FAIL, "can't allocate input struct for decoding");

    if(HG_FAIL == HG_Handler_get_input(handle, input))
	HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, HG_FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "AXE engine not started");

    op_data->hg_handle = handle;
    op_data->input = (void *)input;

    if(input->parent_axe_id){
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 
                                          1, &input->parent_axe_id, 0, NULL, 
                                          H5VL_iod_server_link_exists_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }
    else {
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 0, NULL, 0, NULL, 
                                          H5VL_iod_server_link_exists_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_link_exists() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_link_get_info
 *
 * Purpose:	Function shipper registered call for Link get_info.
 *              Inserts the real worker routine into the Async Engine.
 *
 * Return:	Success:	HG_SUCCESS 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              May, 2013
 *
 *-------------------------------------------------------------------------
 */
int
H5VL_iod_server_link_get_info(hg_handle_t handle)
{
    op_data_t *op_data = NULL;
    link_op_in_t *input = NULL;
    int ret_value = HG_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (op_data = (op_data_t *)H5MM_malloc(sizeof(op_data_t))))
	HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, HG_FAIL, "can't allocate axe op_data struct");

    if(NULL == (input = (link_op_in_t *)
                H5MM_malloc(sizeof(link_op_in_t))))
	HGOTO_ERROR(H5E_DATATYPE, H5E_NOSPACE, HG_FAIL, "can't allocate input struct for decoding");

    if(HG_FAIL == HG_Handler_get_input(handle, input))
	HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, HG_FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "AXE engine not started");

    op_data->hg_handle = handle;
    op_data->input = (void *)input;

    if(input->parent_axe_id){
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 
                                          1, &input->parent_axe_id, 0, NULL, 
                                          H5VL_iod_server_link_get_info_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }
    else {
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 0, NULL, 0, NULL, 
                                          H5VL_iod_server_link_get_info_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_link_get_info() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_link_get_val
 *
 * Purpose:	Function shipper registered call for Link get_val.
 *              Inserts the real worker routine into the Async Engine.
 *
 * Return:	Success:	HG_SUCCESS 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              May, 2013
 *
 *-------------------------------------------------------------------------
 */
int
H5VL_iod_server_link_get_val(hg_handle_t handle)
{
    op_data_t *op_data = NULL;
    link_get_val_in_t *input = NULL;
    int ret_value = HG_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (op_data = (op_data_t *)H5MM_malloc(sizeof(op_data_t))))
	HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, HG_FAIL, "can't allocate axe op_data struct");

    if(NULL == (input = (link_get_val_in_t *)
                H5MM_malloc(sizeof(link_get_val_in_t))))
	HGOTO_ERROR(H5E_DATATYPE, H5E_NOSPACE, HG_FAIL, "can't allocate input struct for decoding");

    if(HG_FAIL == HG_Handler_get_input(handle, input))
	HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, HG_FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "AXE engine not started");

    op_data->hg_handle = handle;
    op_data->input = (void *)input;

    if(input->parent_axe_id){
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 
                                          1, &input->parent_axe_id, 0, NULL, 
                                          H5VL_iod_server_link_get_val_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }
    else {
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 0, NULL, 0, NULL, 
                                          H5VL_iod_server_link_get_val_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_link_get_val() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_link_remove
 *
 * Purpose:	Function shipper registered call for Link Removal.
 *              Inserts the real worker routine into the Async Engine.
 *
 * Return:	Success:	HG_SUCCESS 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              May, 2013
 *
 *-------------------------------------------------------------------------
 */
int
H5VL_iod_server_link_remove(hg_handle_t handle)
{
    op_data_t *op_data = NULL;
    link_op_in_t *input = NULL;
    int ret_value = HG_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (op_data = (op_data_t *)H5MM_malloc(sizeof(op_data_t))))
	HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, HG_FAIL, "can't allocate axe op_data struct");

    if(NULL == (input = (link_op_in_t *)
                H5MM_malloc(sizeof(link_op_in_t))))
	HGOTO_ERROR(H5E_DATATYPE, H5E_NOSPACE, HG_FAIL, "can't allocate input struct for decoding");

    if(HG_FAIL == HG_Handler_get_input(handle, input))
	HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, HG_FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "AXE engine not started");

    op_data->hg_handle = handle;
    op_data->input = (void *)input;

    if(input->parent_axe_id){
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 
                                          1, &input->parent_axe_id, 0, NULL, 
                                          H5VL_iod_server_link_remove_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }
    else {
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 0, NULL, 0, NULL, 
                                          H5VL_iod_server_link_remove_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_link_remove() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_object_open
 *
 * Purpose:	Function shipper registered call for Object Open.
 *              Inserts the real worker routine into the Async Engine.
 *
 * Return:	Success:	HG_SUCCESS 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              May, 2013
 *
 *-------------------------------------------------------------------------
 */
int
H5VL_iod_server_object_open(hg_handle_t handle)
{
    op_data_t *op_data = NULL;
    object_op_in_t *input = NULL;
    int ret_value = HG_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (op_data = (op_data_t *)H5MM_malloc(sizeof(op_data_t))))
	HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, HG_FAIL, "can't allocate axe op_data struct");

    if(NULL == (input = (object_op_in_t *)
                H5MM_malloc(sizeof(object_op_in_t))))
	HGOTO_ERROR(H5E_DATATYPE, H5E_NOSPACE, HG_FAIL, "can't allocate input struct for decoding");

    if(HG_FAIL == HG_Handler_get_input(handle, input))
	HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, HG_FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "AXE engine not started");

    op_data->hg_handle = handle;
    op_data->input = (void *)input;

    if(input->parent_axe_id){
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 
                                          1, &input->parent_axe_id, 0, NULL, 
                                          H5VL_iod_server_object_open_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }
    else {
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 0, NULL, 0, NULL, 
                                          H5VL_iod_server_object_open_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_object_open() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_object_copy
 *
 * Purpose:	Function shipper registered call for Object Copy.
 *              Inserts the real worker routine into the Async Engine.
 *
 * Return:	Success:	HG_SUCCESS 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              May, 2013
 *
 *-------------------------------------------------------------------------
 */
int
H5VL_iod_server_object_copy(hg_handle_t handle)
{
    op_data_t *op_data = NULL;
    object_copy_in_t *input = NULL;
    int ret_value = HG_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (op_data = (op_data_t *)H5MM_malloc(sizeof(op_data_t))))
	HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, HG_FAIL, "can't allocate axe op_data struct");

    if(NULL == (input = (object_copy_in_t *)
                H5MM_malloc(sizeof(object_copy_in_t))))
	HGOTO_ERROR(H5E_DATATYPE, H5E_NOSPACE, HG_FAIL, "can't allocate input struct for decoding");

    if(HG_FAIL == HG_Handler_get_input(handle, input))
	HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, HG_FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "AXE engine not started");

    op_data->hg_handle = handle;
    op_data->input = (void *)input;

    if(input->src_parent_axe_id && input->dst_parent_axe_id) {
        AXE_task_t tasks[2] = {input->src_parent_axe_id, input->dst_parent_axe_id};

        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 
                                          2, tasks, 0, NULL, 
                                          H5VL_iod_server_object_copy_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }
    else if(input->src_parent_axe_id){
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 
                                          1, &input->src_parent_axe_id, 0, NULL, 
                                          H5VL_iod_server_object_copy_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }
    else if(input->dst_parent_axe_id){
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 
                                          1, &input->dst_parent_axe_id, 0, NULL, 
                                          H5VL_iod_server_object_copy_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }
    else {
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 0, NULL, 0, NULL, 
                                          H5VL_iod_server_object_copy_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_object_copy() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_object_visit
 *
 * Purpose:	Function shipper registered call for Object Visit.
 *              Inserts the real worker routine into the Async Engine.
 *
 * Return:	Success:	HG_SUCCESS 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              May, 2013
 *
 *-------------------------------------------------------------------------
 */
int
H5VL_iod_server_object_visit(hg_handle_t handle)
{
    op_data_t *op_data = NULL;
    int ret_value = HG_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_object_visit() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_object_exists
 *
 * Purpose:	Function shipper registered call for Object Existance.
 *              Inserts the real worker routine into the Async Engine.
 *
 * Return:	Success:	HG_SUCCESS 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              May, 2013
 *
 *-------------------------------------------------------------------------
 */
int
H5VL_iod_server_object_exists(hg_handle_t handle)
{
    op_data_t *op_data = NULL;
    object_op_in_t *input = NULL;
    int ret_value = HG_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (op_data = (op_data_t *)H5MM_malloc(sizeof(op_data_t))))
	HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, HG_FAIL, "can't allocate axe op_data struct");

    if(NULL == (input = (object_op_in_t *)
                H5MM_malloc(sizeof(object_op_in_t))))
	HGOTO_ERROR(H5E_DATATYPE, H5E_NOSPACE, HG_FAIL, "can't allocate input struct for decoding");

    if(HG_FAIL == HG_Handler_get_input(handle, input))
	HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, HG_FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "AXE engine not started");

    op_data->hg_handle = handle;
    op_data->input = (void *)input;

    if(input->parent_axe_id){
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 
                                          1, &input->parent_axe_id, 0, NULL, 
                                          H5VL_iod_server_object_exists_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }
    else {
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 0, NULL, 0, NULL, 
                                          H5VL_iod_server_object_exists_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_object_exists() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_object_set_comment
 *
 * Purpose:	Function shipper registered call for Set Comment.
 *              Inserts the real worker routine into the Async Engine.
 *
 * Return:	Success:	HG_SUCCESS 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              May, 2013
 *
 *-------------------------------------------------------------------------
 */
int
H5VL_iod_server_object_set_comment(hg_handle_t handle)
{
    op_data_t *op_data = NULL;
    object_set_comment_in_t *input = NULL;
    int ret_value = HG_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (op_data = (op_data_t *)H5MM_malloc(sizeof(op_data_t))))
	HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, HG_FAIL, "can't allocate axe op_data struct");

    if(NULL == (input = (object_set_comment_in_t *)
                H5MM_malloc(sizeof(object_set_comment_in_t))))
	HGOTO_ERROR(H5E_DATATYPE, H5E_NOSPACE, HG_FAIL, "can't allocate input struct for decoding");

    if(HG_FAIL == HG_Handler_get_input(handle, input))
	HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, HG_FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "AXE engine not started");

    op_data->hg_handle = handle;
    op_data->input = (void *)input;

    if(input->parent_axe_id){
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 
                                          1, &input->parent_axe_id, 0, NULL, 
                                          H5VL_iod_server_object_set_comment_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }
    else {
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 0, NULL, 0, NULL, 
                                          H5VL_iod_server_object_set_comment_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_object_set_comment() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_object_get_comment
 *
 * Purpose:	Function shipper registered call for Get Comment.
 *              Inserts the real worker routine into the Async Engine.
 *
 * Return:	Success:	HG_SUCCESS 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              May, 2013
 *
 *-------------------------------------------------------------------------
 */
int
H5VL_iod_server_object_get_comment(hg_handle_t handle)
{
    op_data_t *op_data = NULL;
    object_get_comment_in_t *input = NULL;
    int ret_value = HG_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (op_data = (op_data_t *)H5MM_malloc(sizeof(op_data_t))))
	HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, HG_FAIL, "can't allocate axe op_data struct");

    if(NULL == (input = (object_get_comment_in_t *)
                H5MM_malloc(sizeof(object_get_comment_in_t))))
	HGOTO_ERROR(H5E_DATATYPE, H5E_NOSPACE, HG_FAIL, "can't allocate input struct for decoding");

    if(HG_FAIL == HG_Handler_get_input(handle, input))
	HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, HG_FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "AXE engine not started");

    op_data->hg_handle = handle;
    op_data->input = (void *)input;

    if(input->parent_axe_id){
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 
                                          1, &input->parent_axe_id, 0, NULL, 
                                          H5VL_iod_server_object_get_comment_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }
    else {
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 0, NULL, 0, NULL, 
                                          H5VL_iod_server_object_get_comment_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_object_get_comment() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_object_get_info
 *
 * Purpose:	Function shipper registered call for Object get_info.
 *              Inserts the real worker routine into the Async Engine.
 *
 * Return:	Success:	HG_SUCCESS 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              May, 2013
 *
 *-------------------------------------------------------------------------
 */
int
H5VL_iod_server_object_get_info(hg_handle_t handle)
{
    op_data_t *op_data = NULL;
    object_op_in_t *input = NULL;
    int ret_value = HG_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (op_data = (op_data_t *)H5MM_malloc(sizeof(op_data_t))))
	HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, HG_FAIL, "can't allocate axe op_data struct");

    if(NULL == (input = (object_op_in_t *)
                H5MM_malloc(sizeof(object_op_in_t))))
	HGOTO_ERROR(H5E_DATATYPE, H5E_NOSPACE, HG_FAIL, "can't allocate input struct for decoding");

    if(HG_FAIL == HG_Handler_get_input(handle, input))
	HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, HG_FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "AXE engine not started");

    op_data->hg_handle = handle;
    op_data->input = (void *)input;

    if(input->parent_axe_id){
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 
                                          1, &input->parent_axe_id, 0, NULL, 
                                          H5VL_iod_server_object_get_info_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }
    else {
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 0, NULL, 0, NULL, 
                                          H5VL_iod_server_object_get_info_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_object_get_info() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_map_create
 *
 * Purpose:	Function shipper registered call for Map Create.
 *              Inserts the real worker routine into the Async Engine.
 *
 * Return:	Success:	HG_SUCCESS 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              July, 2013
 *
 *-------------------------------------------------------------------------
 */
int
H5VL_iod_server_map_create(hg_handle_t handle)
{
    op_data_t *op_data = NULL;
    map_create_in_t *input;
    int ret_value = HG_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (op_data = (op_data_t *)H5MM_malloc(sizeof(op_data_t))))
	HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, HG_FAIL, "can't allocate axe op_data struct");

    if(NULL == (input = (map_create_in_t *)H5MM_malloc(sizeof(map_create_in_t))))
	HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, HG_FAIL, "can't allocate axe op_data struct");

    if(HG_FAIL == HG_Handler_get_input(handle, input))
	HGOTO_ERROR(H5E_SYM, H5E_CANTGET, HG_FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "AXE engine not started");

    op_data->hg_handle = handle;
    op_data->input = (void *)input;

    if(input->parent_axe_id) {
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 
                                          1, &input->parent_axe_id, 0, NULL, 
                                          H5VL_iod_server_map_create_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }
    else {
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 0, NULL, 0, NULL, 
                                          H5VL_iod_server_map_create_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_map_create() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_map_open
 *
 * Purpose:	Function shipper registered call for Map Open.
 *              Inserts the real worker routine into the Async Engine.
 *
 * Return:	Success:	HG_SUCCESS 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              July, 2013
 *
 *-------------------------------------------------------------------------
 */
int
H5VL_iod_server_map_open(hg_handle_t handle)
{
    op_data_t *op_data = NULL;
    map_open_in_t *input;
    int ret_value = HG_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (op_data = (op_data_t *)H5MM_malloc(sizeof(op_data_t))))
	HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, HG_FAIL, "can't allocate axe op_data struct");

    if(NULL == (input = (map_open_in_t *)H5MM_malloc(sizeof(map_open_in_t))))
	HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, HG_FAIL, "can't allocate axe op_data struct");

    if(HG_FAIL == HG_Handler_get_input(handle, input))
	HGOTO_ERROR(H5E_SYM, H5E_CANTGET, HG_FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "AXE engine not started");

    op_data->hg_handle = handle;
    op_data->input = (void *)input;

    if(input->parent_axe_id) {
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 
                                          1, &input->parent_axe_id, 0, NULL, 
                                          H5VL_iod_server_map_open_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }
    else {
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 0, NULL, 0, NULL, 
                                          H5VL_iod_server_map_open_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_map_open() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_map_set
 *
 * Purpose:	Function shipper registered call for Map Set.
 *              Inserts the real worker routine into the Async Engine.
 *
 * Return:	Success:	HG_SUCCESS 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              July, 2013
 *
 *-------------------------------------------------------------------------
 */
int
H5VL_iod_server_map_set(hg_handle_t handle)
{
    op_data_t *op_data = NULL;
    map_set_in_t *input;
    int ret_value = HG_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (op_data = (op_data_t *)H5MM_malloc(sizeof(op_data_t))))
	HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, HG_FAIL, "can't allocate axe op_data struct");

    if(NULL == (input = (map_set_in_t *)H5MM_malloc(sizeof(map_set_in_t))))
	HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, HG_FAIL, "can't allocate axe op_data struct");

    if(HG_FAIL == HG_Handler_get_input(handle, input))
	HGOTO_ERROR(H5E_SYM, H5E_CANTGET, HG_FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "AXE engine not started");

    op_data->hg_handle = handle;
    op_data->input = (void *)input;

    if(input->parent_axe_id) {
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 
                                          1, &input->parent_axe_id, 0, NULL, 
                                          H5VL_iod_server_map_set_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }
    else {
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 0, NULL, 0, NULL, 
                                          H5VL_iod_server_map_set_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_map_set() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_map_get
 *
 * Purpose:	Function shipper registered call for Map Get.
 *              Inserts the real worker routine into the Async Engine.
 *
 * Return:	Success:	HG_SUCCESS 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              July, 2013
 *
 *-------------------------------------------------------------------------
 */
int
H5VL_iod_server_map_get(hg_handle_t handle)
{
    op_data_t *op_data = NULL;
    map_get_in_t *input;
    int ret_value = HG_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (op_data = (op_data_t *)H5MM_malloc(sizeof(op_data_t))))
	HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, HG_FAIL, "can't allocate axe op_data struct");

    if(NULL == (input = (map_get_in_t *)H5MM_malloc(sizeof(map_get_in_t))))
	HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, HG_FAIL, "can't allocate axe op_data struct");

    if(HG_FAIL == HG_Handler_get_input(handle, input))
	HGOTO_ERROR(H5E_SYM, H5E_CANTGET, HG_FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "AXE engine not started");

    op_data->hg_handle = handle;
    op_data->input = (void *)input;

    if(input->parent_axe_id) {
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 
                                          1, &input->parent_axe_id, 0, NULL, 
                                          H5VL_iod_server_map_get_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }
    else {
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 0, NULL, 0, NULL, 
                                          H5VL_iod_server_map_get_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_map_get() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_map_get_count
 *
 * Purpose:	Function shipper registered call for Map Get_Count.
 *              Inserts the real worker routine into the Async Engine.
 *
 * Return:	Success:	HG_SUCCESS 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              July, 2013
 *
 *-------------------------------------------------------------------------
 */
int
H5VL_iod_server_map_get_count(hg_handle_t handle)
{
    op_data_t *op_data = NULL;
    map_get_count_in_t *input;
    int ret_value = HG_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (op_data = (op_data_t *)H5MM_malloc(sizeof(op_data_t))))
	HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, HG_FAIL, "can't allocate axe op_data struct");

    if(NULL == (input = (map_get_count_in_t *)H5MM_malloc(sizeof(map_get_count_in_t))))
	HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, HG_FAIL, "can't allocate axe op_data struct");

    if(HG_FAIL == HG_Handler_get_input(handle, input))
	HGOTO_ERROR(H5E_SYM, H5E_CANTGET, HG_FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "AXE engine not started");

    op_data->hg_handle = handle;
    op_data->input = (void *)input;

    if(input->parent_axe_id) {
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 
                                          1, &input->parent_axe_id, 0, NULL, 
                                          H5VL_iod_server_map_get_count_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }
    else {
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 0, NULL, 0, NULL, 
                                          H5VL_iod_server_map_get_count_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_map_get_count() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_map_exists
 *
 * Purpose:	Function shipper registered call for Map Exists.
 *              Inserts the real worker routine into the Async Engine.
 *
 * Return:	Success:	HG_SUCCESS 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              July, 2013
 *
 *-------------------------------------------------------------------------
 */
int
H5VL_iod_server_map_exists(hg_handle_t handle)
{
    op_data_t *op_data = NULL;
    map_op_in_t *input;
    int ret_value = HG_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (op_data = (op_data_t *)H5MM_malloc(sizeof(op_data_t))))
	HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, HG_FAIL, "can't allocate axe op_data struct");

    if(NULL == (input = (map_op_in_t *)H5MM_malloc(sizeof(map_op_in_t))))
	HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, HG_FAIL, "can't allocate axe op_data struct");

    if(HG_FAIL == HG_Handler_get_input(handle, input))
	HGOTO_ERROR(H5E_SYM, H5E_CANTGET, HG_FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "AXE engine not started");

    op_data->hg_handle = handle;
    op_data->input = (void *)input;

    if(input->parent_axe_id) {
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 
                                          1, &input->parent_axe_id, 0, NULL, 
                                          H5VL_iod_server_map_exists_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }
    else {
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 0, NULL, 0, NULL, 
                                          H5VL_iod_server_map_exists_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_map_exists() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_map_delete
 *
 * Purpose:	Function shipper registered call for Map Delete.
 *              Inserts the real worker routine into the Async Engine.
 *
 * Return:	Success:	HG_SUCCESS 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              July, 2013
 *
 *-------------------------------------------------------------------------
 */
int
H5VL_iod_server_map_delete(hg_handle_t handle)
{
    op_data_t *op_data = NULL;
    map_op_in_t *input;
    int ret_value = HG_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (op_data = (op_data_t *)H5MM_malloc(sizeof(op_data_t))))
	HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, HG_FAIL, "can't allocate axe op_data struct");

    if(NULL == (input = (map_op_in_t *)H5MM_malloc(sizeof(map_op_in_t))))
	HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, HG_FAIL, "can't allocate axe op_data struct");

    if(HG_FAIL == HG_Handler_get_input(handle, input))
	HGOTO_ERROR(H5E_SYM, H5E_CANTGET, HG_FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "AXE engine not started");

    op_data->hg_handle = handle;
    op_data->input = (void *)input;

    if(input->parent_axe_id) {
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 
                                          1, &input->parent_axe_id, 0, NULL, 
                                          H5VL_iod_server_map_delete_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }
    else {
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 0, NULL, 0, NULL, 
                                          H5VL_iod_server_map_delete_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_map_delete() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_map_close
 *
 * Purpose:	Function shipper registered call for Map Close.
 *              Inserts the real worker routine into the Async Engine.
 *
 * Return:	Success:	HG_SUCCESS 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              July, 2013
 *
 *-------------------------------------------------------------------------
 */
int
H5VL_iod_server_map_close(hg_handle_t handle)
{
    op_data_t *op_data = NULL;
    map_close_in_t *input;
    int ret_value = HG_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (op_data = (op_data_t *)H5MM_malloc(sizeof(op_data_t))))
	HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, HG_FAIL, "can't allocate axe op_data struct");

    if(NULL == (input = (map_close_in_t *)H5MM_malloc(sizeof(map_close_in_t))))
	HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, HG_FAIL, "can't allocate axe op_data struct");

    if(HG_FAIL == HG_Handler_get_input(handle, input))
	HGOTO_ERROR(H5E_SYM, H5E_CANTGET, HG_FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "AXE engine not started");

    op_data->hg_handle = handle;
    op_data->input = (void *)input;

    if(input->parent_axe_ids.count) {
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 
                                          input->parent_axe_ids.count, input->parent_axe_ids.ids, 
                                          0, NULL, 
                                          H5VL_iod_server_map_close_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }
    else {
        if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_id, 0, NULL, 0, NULL, 
                                          H5VL_iod_server_map_close_cb, op_data, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_map_close() */

#endif /* H5_HAVE_EFF */
