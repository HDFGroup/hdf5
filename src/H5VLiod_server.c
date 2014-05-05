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

#ifdef H5_HAVE_PYTHON
#include <Python.h>
#endif

#define MAX_LOC_NAME 256

#ifdef H5_HAVE_EFF

/*
 * Programmer:  Mohamad Chaarawi <chaarawi@hdfgroup.gov>
 *              February, 2013
 *
 * Purpose:	The IOD plugin server side routines.
 */

static AXE_engine_t engine;
static MPI_Comm iod_comm;
static int num_peers = 0;
static int terminate_requests = 0;
static hbool_t shutdown = FALSE;

iod_obj_id_t ROOT_ID = 0;

int num_ions_g = 0;
int my_rank_g = 0;
na_addr_t *server_addr_g = NULL;
char **server_loc_g = NULL;
hg_id_t H5VL_EFF_OPEN_CONTAINER;
hg_id_t H5VL_EFF_CLOSE_CONTAINER;
hg_id_t H5VL_EFF_ANALYSIS_FARM;
hg_id_t H5VL_EFF_ANALYSIS_FARM_TRANSFER;

#define H5VL_AXE_TASK_CB(func_name, struct_name) \
        int \
        func_name(hg_handle_t handle) \
        { \
            op_data_t *op_data = NULL; \
            struct_name *input = NULL;     \
            int ret_value = HG_SUCCESS; \
            \
            if(NULL == (op_data = (op_data_t *)H5MM_malloc(sizeof(op_data_t)))) \
                HGOTO_ERROR_FF(HG_FAIL, "can't allocate axe op_data struct"); \
            \
            if(NULL == (input = (struct_name *) H5MM_malloc(sizeof(struct_name)))) \
                HGOTO_ERROR_FF(HG_FAIL, "can't allocate input struct for decoding"); \
            \
            if(HG_FAIL == HG_Handler_get_input(handle, input)) \
                HGOTO_ERROR_FF(HG_FAIL, "can't get input parameters");  \
            \
            if(NULL == engine) \
                HGOTO_ERROR_FF(HG_FAIL, "AXE engine not started");      \
            \
            if(input->axe_info.count && \
               H5VL__iod_server_finish_axe_tasks(engine, input->axe_info.start_range, \
                                                 input->axe_info.count) < 0) \
                HGOTO_ERROR_FF(HG_FAIL, "Unable to cleanup AXE tasks"); \
            \
            op_data->hg_handle = handle; \
            op_data->input = (void *)input; \
            \
            if (AXE_SUCCEED != AXEcreate_task(engine, input->axe_info.axe_id, \
                                              input->axe_info.num_parents, \
                                              input->axe_info.parent_axe_ids, \
                                              0, NULL, func_name ## _cb, op_data, NULL)) \
                HGOTO_ERROR_FF(HG_FAIL, "can't insert task into async engine"); \
            \
        done: \
            return ret_value; \
        }

herr_t
EFF_start_server(MPI_Comm comm, MPI_Info UNUSED info)
{
    na_class_t *network_class = NULL;
    AXE_engine_attr_t engine_attr;
    int i;
    const char *addr_name;
    char **na_addr_table = NULL;
    FILE *config = NULL;
    herr_t ret_value = SUCCEED;

    MPI_Comm_size(comm, &num_ions_g);
    MPI_Comm_rank(comm, &my_rank_g);

    /******************* Initialize mercury ********************/
    /* initialize the netwrok class */
    //network_class = NA_MPI_Init(NULL, MPI_INIT_SERVER);
    network_class = NA_Initialize("tcp@mpi://0.0.0.0:0", 1);

    /* Allocate table addrs */
    na_addr_table = (char**) malloc((size_t)num_ions_g * sizeof(char*));
    for (i = 0; i < num_ions_g; i++) {
        na_addr_table[i] = (char*) malloc(MPI_MAX_PORT_NAME);
    }

    addr_name = NA_MPI_Get_port_name(network_class);
    strcpy(na_addr_table[my_rank_g], addr_name);

#ifdef NA_HAS_MPI
    for (i = 0; i < num_ions_g; i++) {
        MPI_Bcast(na_addr_table[i], MPI_MAX_PORT_NAME,
                  MPI_BYTE, i, comm);
    }
#endif

    /* Only rank 0 writes file */
    if (my_rank_g == 0) {
        char cwd[1024];

        if (getcwd(cwd, sizeof(cwd)) != NULL)
            fprintf(stdout, "Writing port.cfg to: %s\n", cwd);
        else {
            fprintf(stderr, "etcwd() error\n");
            return FAIL;
        }

        config = fopen("port.cfg", "w+");
        if (config != NULL) {
            fprintf(config, "%d\n", num_ions_g);
            for (i = 0; i < num_ions_g; i++) {
                fprintf(config, "%s\n", na_addr_table[i]);
            }
            fclose(config);
        }
	else {
            fprintf(stderr, "could not open port.cfg file.\n");
            return FAIL;
	}
    }

    iod_comm = comm;

    if(HG_SUCCESS != HG_Init(network_class))
        return FAIL;

    /* Look up addr id */
    /* We do the lookup here but this may not be optimal */
    server_addr_g = (na_addr_t *) malloc((size_t)num_ions_g * sizeof(na_addr_t));
    for (i = 0; i < num_ions_g; i++) {
        if(NA_SUCCESS != NA_Addr_lookup_wait(network_class, na_addr_table[i], &server_addr_g[i])) {
            fprintf(stderr, "Could not find addr\n");
            return FAIL;
        }
    }

    /* Get array of loc string */
    server_loc_g = (char **) malloc((unsigned int) num_ions_g * sizeof(char *));
    for (i = 0; i < num_ions_g; i++) {
        server_loc_g[i] = (char *) malloc(MAX_LOC_NAME);
    }

    /* Only rank 0 read loc file */
    if (my_rank_g == 0) {
        config = fopen("loc.cfg", "r+");
        if (!config) {
            fprintf(stderr, "Warning, no loc config was found\n");
        } else {
            for (i = 0; i < num_ions_g; i++) {
                fscanf(config, "%s\n", server_loc_g[i]);
            }
            fclose(config);
        }
    }
    /* TODO may not be necessary to do a broadcast here */
    for (i = 0; i < num_ions_g; i++)
        MPI_Bcast(server_loc_g[i], MAX_LOC_NAME, MPI_BYTE, 0, comm);


    /***************** END Initialize mercury *******************/

    EFF__mercury_register_callbacks();

    /* register server specific callbacks */
    H5VL_EFF_OPEN_CONTAINER = MERCURY_REGISTER("container_open", hg_const_string_t, iod_handle_t,
                                               H5VL_iod_server_container_open);
    H5VL_EFF_CLOSE_CONTAINER = MERCURY_REGISTER("container_close", iod_handle_t, ret_t,
                                                H5VL_iod_server_container_close);
    H5VL_EFF_ANALYSIS_FARM = MERCURY_REGISTER("analysis_farm", analysis_farm_in_t, 
                                              analysis_farm_out_t, H5VL_iod_server_analysis_farm);
    H5VL_EFF_ANALYSIS_FARM_TRANSFER = MERCURY_REGISTER("analysis_transfer", analysis_transfer_in_t, 
                                                       analysis_transfer_out_t,
                                                       H5VL_iod_server_analysis_transfer);

    /* Initialize engine attribute */
    if(AXEengine_attr_init(&engine_attr) != AXE_SUCCEED)
        return FAIL;

    /* Set number of threads in AXE engine */
    if(AXEset_num_threads(&engine_attr, 16) != AXE_SUCCEED)
        return FAIL;

    /* Create AXE engine */
    if(AXEcreate_engine(&engine, &engine_attr) != AXE_SUCCEED)
        return FAIL;

    /* Initialize Python runtime */
#ifdef H5_HAVE_PYTHON
    Py_Initialize();
#endif

    /* Loop to receive requests from clients */
    while(1) {
        HG_Handler_process(0, HG_STATUS_IGNORE);
        if(shutdown)
            break;
    }

    /* Finalize Python runtime */
#ifdef H5_HAVE_PYTHON
    Py_Finalize();
#endif

    if(AXE_SUCCEED != AXEterminate_engine(engine, TRUE))
        return FAIL;

    if(AXEengine_attr_destroy(&engine_attr) != AXE_SUCCEED)
        return FAIL;

    /******************* Finalize mercury ********************/
    for (i = 0; i < num_ions_g; i++) {
        free(server_loc_g[i]);
    }
    free(server_loc_g);

    for (i = 0; i < num_ions_g; i++) {
        NA_Addr_free(network_class, server_addr_g[i]);
    }
    free(server_addr_g);

    if(HG_SUCCESS != HG_Finalize())
        return FAIL;
    if(NA_SUCCESS != NA_Finalize(network_class))
        return FAIL;

    if (na_addr_table) {
        for (i = 0; i < num_ions_g; i++) {
            free(na_addr_table[i]);
        }
        free(na_addr_table);
        na_addr_table = NULL;
        num_ions_g = 0;
    }
    /***************** END Finalize mercury *******************/

    return ret_value;
}

static herr_t
H5VL__iod_server_finish_axe_tasks(AXE_engine_t axe_engine, AXE_task_t start_range, 
                                  size_t count)
{
    AXE_task_t u;
    herr_t ret_value = SUCCEED;

    for(u=start_range ; u<count+start_range ; u++) {
        if(AXE_SUCCEED != AXEfinish(axe_engine, u))
            HGOTO_ERROR_FF(FAIL, "Unable to cleanup AXE task")
    }

done:
    return ret_value;
} /* end H5VL__iod_server_finish_axe_tasks() */


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
    uint32_t num_procs;
    int ret_value = HG_SUCCESS;

    /* get the input from the client connecting */
    if(HG_FAIL == HG_Handler_get_input(handle, &num_procs))
	HGOTO_ERROR_FF(FAIL, "can't get input parameters");

    /* initialize the IOD library */
    if(iod_initialize(iod_comm, NULL, num_procs, num_procs) < 0)
        HGOTO_ERROR_FF(FAIL, "can't initialize");

    /* set the root ID */
    IOD_OBJID_SETOWNER_APP(ROOT_ID)
    IOD_OBJID_SETTYPE(ROOT_ID, IOD_OBJ_KV)

    num_peers ++;

done:
    HG_Handler_start_output(handle, &ret_value);
    HG_Handler_free(handle);

    return ret_value;
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

    /* increment the number of terminate requests received so far */
    terminate_requests ++;

    if(iod_finalize(NULL) < 0 )
        HGOTO_ERROR_FF(FAIL, "can't finalize IOD");

    /* if all the peers that connected at the beginning have sent the
       terminate request, then finalize IOD and indicate that it is
       time to shutdown the server */
    if(terminate_requests == num_peers) {
        shutdown = TRUE;
    }

done:
    HG_Handler_start_output(handle, &ret_value);
    HG_Handler_free(handle);
    return ret_value;
} /* end H5VL_iod_server_eff_finalize() */

herr_t
EFF_setup_coresident(MPI_Comm comm, MPI_Info UNUSED info)
{
    AXE_engine_attr_t engine_attr;
    herr_t ret_value = SUCCEED;

    MPI_Comm_size(comm, &num_ions_g);
    MPI_Comm_rank(comm, &my_rank_g);

    iod_comm = comm;

    /* register server specific callbacks */
    H5VL_EFF_OPEN_CONTAINER = MERCURY_REGISTER("container_open", hg_const_string_t, iod_handle_t,
                                               H5VL_iod_server_container_open);
    H5VL_EFF_CLOSE_CONTAINER = MERCURY_REGISTER("container_close", iod_handle_t, ret_t,
                                                H5VL_iod_server_container_close);
    H5VL_EFF_ANALYSIS_FARM = MERCURY_REGISTER("analysis_farm", analysis_farm_in_t, 
                                              analysis_farm_out_t, H5VL_iod_server_analysis_farm);
    H5VL_EFF_ANALYSIS_FARM_TRANSFER = MERCURY_REGISTER("analysis_transfer", analysis_transfer_in_t, 
                                                       analysis_transfer_out_t,
                                                       H5VL_iod_server_analysis_transfer);

    /* Initialize engine attribute */
    if(AXEengine_attr_init(&engine_attr) != AXE_SUCCEED)
        return FAIL;

    /* Set number of threads in AXE engine */
    if(AXEset_num_threads(&engine_attr, 2) != AXE_SUCCEED)
        return FAIL;

    /* Create AXE engine */
    if(AXEcreate_engine(&engine, &engine_attr) != AXE_SUCCEED)
        return FAIL;

    if(AXEengine_attr_destroy(&engine_attr) != AXE_SUCCEED)
        return FAIL;

    /* Initialize Python runtime */
#ifdef H5_HAVE_PYTHON
    Py_Initialize();
#endif

    return ret_value;
}

herr_t 
EFF_terminate_coresident(void)
{
    herr_t ret_value = SUCCEED;

    /* Finalize Python runtime */
#ifdef H5_HAVE_PYTHON
    Py_Finalize();
#endif

    if(AXE_SUCCEED != AXEterminate_engine(engine, TRUE))
        return FAIL;

    return ret_value;
}

H5VL_AXE_TASK_CB(H5VL_iod_server_analysis_execute, analysis_execute_in_t)
H5VL_AXE_TASK_CB(H5VL_iod_server_file_create, file_create_in_t)
H5VL_AXE_TASK_CB(H5VL_iod_server_file_open, file_open_in_t)
H5VL_AXE_TASK_CB(H5VL_iod_server_attr_create, attr_create_in_t)
H5VL_AXE_TASK_CB(H5VL_iod_server_attr_open, attr_open_in_t)
H5VL_AXE_TASK_CB(H5VL_iod_server_attr_read, attr_io_in_t)
H5VL_AXE_TASK_CB(H5VL_iod_server_attr_write, attr_io_in_t)
H5VL_AXE_TASK_CB(H5VL_iod_server_attr_exists, attr_op_in_t)
H5VL_AXE_TASK_CB(H5VL_iod_server_attr_rename, attr_rename_in_t)
H5VL_AXE_TASK_CB(H5VL_iod_server_attr_remove, attr_op_in_t)
H5VL_AXE_TASK_CB(H5VL_iod_server_attr_close, attr_close_in_t)
H5VL_AXE_TASK_CB(H5VL_iod_server_group_create, group_create_in_t)
H5VL_AXE_TASK_CB(H5VL_iod_server_group_open, group_open_in_t)
H5VL_AXE_TASK_CB(H5VL_iod_server_group_close, group_close_in_t)
H5VL_AXE_TASK_CB(H5VL_iod_server_dset_create, dset_create_in_t)
H5VL_AXE_TASK_CB(H5VL_iod_server_dset_open, dset_open_in_t)
H5VL_AXE_TASK_CB(H5VL_iod_server_dset_read, dset_io_in_t)
H5VL_AXE_TASK_CB(H5VL_iod_server_dset_get_vl_size, dset_io_in_t)
H5VL_AXE_TASK_CB(H5VL_iod_server_dset_write, dset_io_in_t)
H5VL_AXE_TASK_CB(H5VL_iod_server_dset_set_extent, dset_set_extent_in_t)
H5VL_AXE_TASK_CB(H5VL_iod_server_dset_close, dset_close_in_t)
#ifdef H5_HAVE_INDEXING
H5VL_AXE_TASK_CB(H5VL_iod_server_dset_set_index_info, dset_set_index_info_in_t)
H5VL_AXE_TASK_CB(H5VL_iod_server_dset_get_index_info, dset_get_index_info_in_t)
H5VL_AXE_TASK_CB(H5VL_iod_server_dset_remove_index_info, dset_rm_index_info_in_t)
#endif
H5VL_AXE_TASK_CB(H5VL_iod_server_dtype_commit, dtype_commit_in_t)
H5VL_AXE_TASK_CB(H5VL_iod_server_dtype_open, dtype_open_in_t)
H5VL_AXE_TASK_CB(H5VL_iod_server_dtype_close, dtype_close_in_t)
H5VL_AXE_TASK_CB(H5VL_iod_server_link_create, link_create_in_t)
H5VL_AXE_TASK_CB(H5VL_iod_server_link_move, link_move_in_t)
H5VL_AXE_TASK_CB(H5VL_iod_server_link_exists, link_op_in_t)
H5VL_AXE_TASK_CB(H5VL_iod_server_link_get_info, link_op_in_t)
H5VL_AXE_TASK_CB(H5VL_iod_server_link_get_val, link_get_val_in_t)
H5VL_AXE_TASK_CB(H5VL_iod_server_link_remove, link_op_in_t)
H5VL_AXE_TASK_CB(H5VL_iod_server_object_open_by_token, object_token_in_t)
H5VL_AXE_TASK_CB(H5VL_iod_server_object_open, object_op_in_t)
H5VL_AXE_TASK_CB(H5VL_iod_server_object_exists, object_op_in_t)
H5VL_AXE_TASK_CB(H5VL_iod_server_object_set_comment, object_set_comment_in_t)
H5VL_AXE_TASK_CB(H5VL_iod_server_object_get_comment, object_get_comment_in_t)
H5VL_AXE_TASK_CB(H5VL_iod_server_object_get_info, object_op_in_t)
H5VL_AXE_TASK_CB(H5VL_iod_server_map_create, map_create_in_t)
H5VL_AXE_TASK_CB(H5VL_iod_server_map_open, map_open_in_t)
H5VL_AXE_TASK_CB(H5VL_iod_server_map_set, map_set_in_t)
H5VL_AXE_TASK_CB(H5VL_iod_server_map_get, map_get_in_t)
H5VL_AXE_TASK_CB(H5VL_iod_server_map_get_count, map_get_count_in_t)
H5VL_AXE_TASK_CB(H5VL_iod_server_map_exists, map_op_in_t)
H5VL_AXE_TASK_CB(H5VL_iod_server_map_delete, map_op_in_t)
H5VL_AXE_TASK_CB(H5VL_iod_server_map_close, map_close_in_t)
H5VL_AXE_TASK_CB(H5VL_iod_server_rcxt_acquire, rc_acquire_in_t)
H5VL_AXE_TASK_CB(H5VL_iod_server_rcxt_release, rc_release_in_t)
H5VL_AXE_TASK_CB(H5VL_iod_server_rcxt_persist, rc_persist_in_t)
H5VL_AXE_TASK_CB(H5VL_iod_server_rcxt_snapshot, rc_snapshot_in_t)
H5VL_AXE_TASK_CB(H5VL_iod_server_trans_start, tr_start_in_t)
H5VL_AXE_TASK_CB(H5VL_iod_server_trans_finish, tr_finish_in_t)
H5VL_AXE_TASK_CB(H5VL_iod_server_trans_set_dependency, tr_set_depend_in_t)
H5VL_AXE_TASK_CB(H5VL_iod_server_trans_skip, tr_skip_in_t)
H5VL_AXE_TASK_CB(H5VL_iod_server_trans_abort, tr_abort_in_t)
H5VL_AXE_TASK_CB(H5VL_iod_server_prefetch, prefetch_in_t)
H5VL_AXE_TASK_CB(H5VL_iod_server_evict, evict_in_t)
H5VL_AXE_TASK_CB(H5VL_iod_server_view_create, view_create_in_t)


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_analysis_farm
 *
 * Purpose:	Function shipper registered call for Analysis Farming.
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
H5VL_iod_server_analysis_farm(hg_handle_t handle)
{
    op_data_t *op_data = NULL;
    analysis_farm_in_t *input = NULL;
    AXE_task_t axe_id;
    int ret_value = HG_SUCCESS;

    if(NULL == (op_data = (op_data_t *)H5MM_malloc(sizeof(op_data_t))))
	HGOTO_ERROR_FF(FAIL, "can't allocate axe op_data struct");

    if(NULL == (input = (analysis_farm_in_t *)H5MM_malloc(sizeof(analysis_farm_in_t))))
	HGOTO_ERROR_FF(FAIL, "can't allocate input struct for decoding");

    if(HG_FAIL == HG_Handler_get_input(handle, input))
	HGOTO_ERROR_FF(FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR_FF(FAIL, "AXE engine not started");

    if(AXE_SUCCEED != AXEgenerate_task_id(engine, &axe_id))
        HGOTO_ERROR_FF(FAIL, "Unable to generate ID for AXE task");

    op_data->hg_handle = handle;
    op_data->axe_id = axe_id;
    op_data->input = (void *)input;

    if (AXE_SUCCEED != AXEcreate_task(engine, axe_id, 0, NULL, 0, NULL, 
                                      H5VL_iod_server_analysis_farm_cb, op_data, NULL))
        HGOTO_ERROR_FF(FAIL, "can't insert task into async engine");

done:
    return ret_value;
} /* end H5VL_iod_server_analysis_farm() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_analysis_transfer
 *
 * Purpose:	Function shipper registered call for Analysis Farming.
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
H5VL_iod_server_analysis_transfer(hg_handle_t handle)
{
    op_data_t *op_data = NULL;
    AXE_task_t *input = NULL;
    AXE_task_t axe_id;
    int ret_value = HG_SUCCESS;

    if(NULL == (op_data = (op_data_t *)H5MM_malloc(sizeof(op_data_t))))
	HGOTO_ERROR_FF(FAIL, "can't allocate axe op_data struct");

    if(NULL == (input = (AXE_task_t *)H5MM_malloc(sizeof(AXE_task_t))))
	HGOTO_ERROR_FF(FAIL, "can't allocate input struct for decoding");

    if(HG_FAIL == HG_Handler_get_input(handle, input))
	HGOTO_ERROR_FF(FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR_FF(FAIL, "AXE engine not started");

    op_data->hg_handle = handle;
    op_data->input = (void *)input;

    if(AXE_SUCCEED != AXEgenerate_task_id(engine, &axe_id))
        HGOTO_ERROR_FF(FAIL, "Unable to generate ID for AXE task");

    if (AXE_SUCCEED != AXEcreate_task(engine, axe_id, 0, NULL, 0, NULL, 
                                      H5VL_iod_server_analysis_transfer_cb, op_data, NULL))
        HGOTO_ERROR_FF(FAIL, "can't insert task into async engine");

done:
    return ret_value;
} /* end H5VL_iod_server_analysis_transfer() */

int
H5VL_iod_server_container_open(hg_handle_t handle)
{
    const char *file_name;
    iod_handle_t coh;
    int ret_value = HG_SUCCESS;

    if(HG_FAIL == HG_Handler_get_input(handle, &file_name))
	HGOTO_ERROR_FF(FAIL, "can't get input parameters");

    /* open the container */
    printf("Calling iod_container_open on %s\n", file_name);
    if(iod_container_open(file_name, NULL, IOD_CONT_R, &coh, NULL))
        HGOTO_ERROR_FF(FAIL, "can't open file");

    HG_Handler_start_output(handle, &coh);

done:
    if(ret_value < 0) {
        coh.cookie = IOD_OH_UNDEFINED;
        HG_Handler_start_output(handle, &coh);
    }
    return ret_value;
} /* end H5VL_iod_server_container_open() */

int
H5VL_iod_server_container_close(hg_handle_t handle)
{
    iod_handle_t coh;
    int ret_value = HG_SUCCESS;

    if(HG_FAIL == HG_Handler_get_input(handle, &coh))
	HGOTO_ERROR_FF(FAIL, "can't get input parameters");

    /* open the container */
    if(iod_container_close(coh, NULL, NULL))
        HGOTO_ERROR_FF(FAIL, "can't open file");

done:
    HG_Handler_start_output(handle, &ret_value);
    return ret_value;
} /* end H5VL_iod_server_container_open() */



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
    H5ES_status_t status = H5ES_STATUS_IN_PROGRESS;
    int ret_value = HG_SUCCESS;

    if(HG_FAIL == HG_Handler_get_input(handle, &axe_id))
	HGOTO_ERROR_FF(FAIL, "can't get input parameters");

    /* Try to remove the task. */
    if(AXEremove(engine, axe_id, &remove_status) != AXE_SUCCEED)
        HGOTO_ERROR_FF(FAIL, "can't remove AXE task; it has children");

    if(remove_status == AXE_CANCELED)
        HGOTO_DONE(H5VL_IOD_CANCELLED)
    else if(remove_status == AXE_ALL_DONE)
        HGOTO_DONE(H5VL_IOD_COMPLETED)
    else if(remove_status == AXE_NOT_CANCELED) {
        void *op_data;

        fprintf(stderr, "Task is running. Attempting to cancel Manually\n");
        if(AXEget_op_data(engine, axe_id, &op_data) != AXE_SUCCEED)
            HGOTO_ERROR_FF(FAIL, "can't get op data");
        /* Attempt to cancel the task manually */
    }

done:
    HG_Handler_start_output(handle, &status);
    return ret_value;
} /* end H5VL_iod_server_cancel_op() */


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

    if(NULL == (op_data = (op_data_t *)H5MM_malloc(sizeof(op_data_t))))
	HGOTO_ERROR_FF(FAIL, "can't allocate axe op_data struct");

    if(NULL == (input = (file_close_in_t *)
                H5MM_malloc(sizeof(file_close_in_t))))
	HGOTO_ERROR_FF(FAIL, "can't allocate input struct for decoding");

    if(HG_FAIL == HG_Handler_get_input(handle, input))
	HGOTO_ERROR_FF(FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR_FF(FAIL, "AXE engine not started");

    if(input->axe_info.count && 
       H5VL__iod_server_finish_axe_tasks(engine, input->axe_info.start_range,  
                                         input->axe_info.count) < 0)
        HGOTO_ERROR_FF(FAIL, "Unable to cleanup AXE tasks");

    op_data->hg_handle = handle;
    op_data->input = (void *)input;

    if (AXE_SUCCEED != AXEcreate_barrier_task(engine, input->axe_info.axe_id,
                                              H5VL_iod_server_file_close_cb, op_data, NULL))
        HGOTO_ERROR_FF(FAIL, "can't insert task into async engine");

done:
    return ret_value;
} /* end H5VL_iod_server_file_close() */

#endif /* H5_HAVE_EFF */
