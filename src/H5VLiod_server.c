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

#define H5G_PACKAGE		/*suppress error about including H5Gpkg   */

#include "H5private.h"		/* Generic Functions			*/
#include "H5Apublic.h"		/* Attributes				*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Gpkg.h"		/* Groups		  		*/
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Oprivate.h"         /* Object headers			*/
#include "H5Pprivate.h"		/* Property lists			*/
#include "H5Sprivate.h"		/* Dataspaces				*/
#include "H5Tprivate.h"		/* Datatypes				*/
#include "H5VLprivate.h"	/* VOL plugins				*/
#include "H5VLiod_server.h"
#include "H5WBprivate.h"        /* Wrapped Buffers                      */

#ifdef H5_HAVE_EFF

#define H5_DO_NATIVE 0
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

#define EEXISTS 1

static herr_t H5VL_iod_server_get_loc(iod_handle_t coh, iod_handle_t loc_handle, const char *path,
                                      hbool_t create_interm_grps, hbool_t break_on_last_comp,
                                      char **last_comp, iod_obj_id_t *iod_id, iod_handle_t *iod_oh, 
                                      iod_obj_id_t *scratch_id, iod_handle_t *scratch_oh);

static herr_t H5VL_iod_server_file_create_cb(size_t num_necessary_parents, AXE_task_t necessary_parents[], 
                                             size_t num_sufficient_parents, AXE_task_t sufficient_parents[], 
                                             void *op_data);
static herr_t H5VL_iod_server_file_open_cb(size_t num_necessary_parents, AXE_task_t necessary_parents[], 
                                           size_t num_sufficient_parents, AXE_task_t sufficient_parents[], 
                                           void *op_data);
static herr_t H5VL_iod_server_file_close_cb(size_t num_necessary_parents, AXE_task_t necessary_parents[], 
                                            size_t num_sufficient_parents, AXE_task_t sufficient_parents[], 
                                            void *op_data);
static herr_t H5VL_iod_server_file_flush_cb(size_t num_necessary_parents, AXE_task_t necessary_parents[], 
                                            size_t num_sufficient_parents, AXE_task_t sufficient_parents[], 
                                            void *op_data);
static herr_t H5VL_iod_server_attr_create_cb(size_t num_necessary_parents, AXE_task_t necessary_parents[], 
                                             size_t num_sufficient_parents, AXE_task_t sufficient_parents[], 
                                             void *op_data);
static herr_t H5VL_iod_server_attr_open_cb(size_t num_necessary_parents, AXE_task_t necessary_parents[], 
                                           size_t num_sufficient_parents, AXE_task_t sufficient_parents[], 
                                           void *op_data);
static herr_t H5VL_iod_server_attr_read_cb(size_t num_necessary_parents, AXE_task_t necessary_parents[], 
                                           size_t num_sufficient_parents, AXE_task_t sufficient_parents[], 
                                           void *op_data);
static herr_t H5VL_iod_server_attr_write_cb(size_t num_necessary_parents, AXE_task_t necessary_parents[], 
                                            size_t num_sufficient_parents, AXE_task_t sufficient_parents[], 
                                            void *op_data);
static herr_t H5VL_iod_server_attr_exists_cb(size_t num_necessary_parents, AXE_task_t necessary_parents[], 
                                             size_t num_sufficient_parents, AXE_task_t sufficient_parents[], 
                                             void *op_data);
static herr_t H5VL_iod_server_attr_remove_cb(size_t num_necessary_parents, AXE_task_t necessary_parents[], 
                                             size_t num_sufficient_parents, AXE_task_t sufficient_parents[], 
                                             void *op_data);
static herr_t H5VL_iod_server_attr_close_cb(size_t num_necessary_parents, AXE_task_t necessary_parents[], 
                                            size_t num_sufficient_parents, AXE_task_t sufficient_parents[], 
                                            void *op_data);
static herr_t H5VL_iod_server_group_create_cb(size_t num_necessary_parents, AXE_task_t necessary_parents[], 
                                              size_t num_sufficient_parents, AXE_task_t sufficient_parents[], 
                                              void *op_data);
static herr_t H5VL_iod_server_group_open_cb(size_t num_necessary_parents, AXE_task_t necessary_parents[], 
                                            size_t num_sufficient_parents, AXE_task_t sufficient_parents[], 
                                            void *op_data);
static herr_t H5VL_iod_server_group_close_cb(size_t num_necessary_parents, AXE_task_t necessary_parents[], 
                                             size_t num_sufficient_parents, AXE_task_t sufficient_parents[], 
                                             void *op_data);
static herr_t H5VL_iod_server_dset_create_cb(size_t num_necessary_parents, AXE_task_t necessary_parents[], 
                                             size_t num_sufficient_parents, AXE_task_t sufficient_parents[], 
                                             void *op_data);
static herr_t H5VL_iod_server_dset_open_cb(size_t num_necessary_parents, AXE_task_t necessary_parents[], 
                                           size_t num_sufficient_parents, AXE_task_t sufficient_parents[], 
                                           void *op_data);
static herr_t H5VL_iod_server_dset_read_cb(size_t num_necessary_parents, AXE_task_t necessary_parents[], 
                                           size_t num_sufficient_parents, AXE_task_t sufficient_parents[], 
                                           void *op_data);
static herr_t H5VL_iod_server_dset_write_cb(size_t num_necessary_parents, AXE_task_t necessary_parents[], 
                                            size_t num_sufficient_parents, AXE_task_t sufficient_parents[], 
                                            void *op_data);
static herr_t H5VL_iod_server_dset_set_extent_cb(size_t num_necessary_parents, AXE_task_t necessary_parents[], 
                                                 size_t num_sufficient_parents, AXE_task_t sufficient_parents[], 
                                                 void *op_data);
static herr_t H5VL_iod_server_dset_close_cb(size_t num_necessary_parents, AXE_task_t necessary_parents[], 
                                            size_t num_sufficient_parents, AXE_task_t sufficient_parents[], 
                                            void *op_data);
static herr_t H5VL_iod_server_dtype_commit_cb(size_t num_necessary_parents, AXE_task_t necessary_parents[], 
                                              size_t num_sufficient_parents, AXE_task_t sufficient_parents[], 
                                              void *op_data);
static herr_t H5VL_iod_server_dtype_open_cb(size_t num_necessary_parents, AXE_task_t necessary_parents[], 
                                            size_t num_sufficient_parents, AXE_task_t sufficient_parents[], 
                                            void *op_data);
static herr_t H5VL_iod_server_dtype_close_cb(size_t num_necessary_parents, AXE_task_t necessary_parents[], 
                                             size_t num_sufficient_parents, AXE_task_t sufficient_parents[], 
                                             void *op_data);
herr_t
H5VLiod_start_handler(MPI_Comm comm, MPI_Info UNUSED info)
{
    na_class_t *network_class = NULL;
    int num_procs;
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

    MERCURY_HANDLER_REGISTER("dset_create", H5VL_iod_server_dset_create, 
                                   dset_create_in_t, dset_create_out_t);
    MERCURY_HANDLER_REGISTER("dset_open", H5VL_iod_server_dset_open, 
                                   dset_open_in_t, dset_open_out_t);
    MERCURY_HANDLER_REGISTER("dset_read", H5VL_iod_server_dset_read, 
                                   dset_io_in_t, dset_read_out_t);
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

    /* Loop tp receive requests from clients */
    while(1) {
        fprintf(stderr, "Server In Loop\n");
        /* Receive new function calls */
        if(HG_SUCCESS != HG_Handler_process(HG_HANDLER_MAX_IDLE_TIME))
            return FAIL;
        if(shutdown)
            break;
    }

    if(HG_SUCCESS != HG_Bulk_finalize())
        return FAIL;
    if(HG_SUCCESS != HG_Handler_finalize())
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
 *              January, 2012
 *
 *-------------------------------------------------------------------------
 */
int
H5VL_iod_server_eff_init(hg_handle_t handle)
{
    int num_procs;
    int ret_value = HG_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    if(HG_FAIL == HG_Handler_get_input(handle, &num_procs))
	HGOTO_ERROR(H5E_FILE, H5E_CANTGET, HG_FAIL, "can't get input parameters");

    if(iod_initialize(iod_comm, NULL, num_procs, num_procs, NULL) < 0 )
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, HG_FAIL, "can't initialize");

    /* MSC - this needs to be changed to be the number of peers connecting to this server */
    num_peers = num_procs;

    if(AXE_SUCCEED != AXEcreate_engine(4, &engine))
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, HG_FAIL, "can't start AXE engine");

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
 *              January, 2012
 *
 *-------------------------------------------------------------------------
 */
int
H5VL_iod_server_eff_finalize(hg_handle_t handle)
{
    int ret_value = HG_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    terminate_requests ++;

    if(terminate_requests == num_peers) {
        if(iod_finalize(NULL, NULL) < 0 )
            HGOTO_ERROR(H5E_FILE, H5E_CANTDEC, HG_FAIL, "can't finalize IOD");
        if(AXE_SUCCEED != AXEterminate_engine(engine, TRUE))
            HGOTO_ERROR(H5E_FILE, H5E_CANTDEC, HG_FAIL, "can't shutdown AXE engine");
        shutdown = TRUE;
    }

done:
    HG_Handler_start_output(handle, &ret_value);
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_eff_finalize() */


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
    H5VL_iod_file_create_input_t *input = NULL;
    AXE_task_t task;
    int ret_value = HG_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (input = (H5VL_iod_file_create_input_t *)
                H5MM_malloc(sizeof(H5VL_iod_file_create_input_t))))
	HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, HG_FAIL, "can't allocate input struct for decoding");

    if(HG_FAIL == HG_Handler_get_input(handle, input))
	HGOTO_ERROR(H5E_FILE, H5E_CANTGET, HG_FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "AXE engine not started");

    input->hg_handle = handle;
    if (AXE_SUCCEED != AXEcreate_task(engine, &task, 0, NULL, 0, NULL, H5VL_iod_server_file_create_cb, 
                                      input, NULL))
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
    H5VL_iod_file_open_input_t *input = NULL;
    AXE_task_t task;
    int ret_value = HG_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (input = (H5VL_iod_file_open_input_t *)
                H5MM_malloc(sizeof(H5VL_iod_file_open_input_t))))
	HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, HG_FAIL, "can't allocate input struct for decoding");

    if(HG_FAIL == HG_Handler_get_input(handle, input))
	HGOTO_ERROR(H5E_FILE, H5E_CANTGET, HG_FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "AXE engine not started");

    input->hg_handle = handle;
    if (AXE_SUCCEED != AXEcreate_task(engine, &task, 0, NULL, 0, NULL, H5VL_iod_server_file_open_cb, 
                                      input, NULL))
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
    H5VL_iod_file_flush_input_t *input = NULL;
    AXE_task_t task;
    int ret_value = HG_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (input = (H5VL_iod_file_flush_input_t *)
                H5MM_malloc(sizeof(H5VL_iod_file_flush_input_t))))
	HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, HG_FAIL, "can't allocate input struct for decoding");

    if(HG_FAIL == HG_Handler_get_input(handle, input))
	HGOTO_ERROR(H5E_FILE, H5E_CANTGET, HG_FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "AXE engine not started");

    input->hg_handle = handle;
    if (AXE_SUCCEED != AXEcreate_task(engine, &task, 0, NULL, 0, NULL, H5VL_iod_server_file_flush_cb, 
                                      input, NULL))
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
    H5VL_iod_remote_file_t *input = NULL;
    AXE_task_t task;
    int ret_value = HG_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (input = (H5VL_iod_remote_file_t *)
                H5MM_malloc(sizeof(H5VL_iod_remote_file_t))))
	HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, HG_FAIL, "can't allocate input struct for decoding");

    if(HG_FAIL == HG_Handler_get_input(handle, input))
	HGOTO_ERROR(H5E_FILE, H5E_CANTGET, HG_FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "AXE engine not started");

    input->hg_handle = handle;
    if (AXE_SUCCEED != AXEcreate_task(engine, &task, 0, NULL, 0, NULL, H5VL_iod_server_file_close_cb, 
                                      input, NULL))
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
    H5VL_iod_attr_create_input_t *input = NULL;
    AXE_task_t task;
    int ret_value = HG_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (input = (H5VL_iod_attr_create_input_t *)
                H5MM_malloc(sizeof(H5VL_iod_attr_create_input_t))))
	HGOTO_ERROR(H5E_ATTR, H5E_NOSPACE, HG_FAIL, "can't allocate input struct for decoding");

    if(HG_FAIL == HG_Handler_get_input(handle, input))
	HGOTO_ERROR(H5E_ATTR, H5E_CANTGET, HG_FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "AXE engine not started");

    input->hg_handle = handle;
    if (AXE_SUCCEED != AXEcreate_task(engine, &task, 0, NULL, 0, NULL, H5VL_iod_server_attr_create_cb, 
                                      input, NULL))
        HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");

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
    H5VL_iod_attr_open_input_t *input = NULL;
    AXE_task_t task;
    int ret_value = HG_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (input = (H5VL_iod_attr_open_input_t *)
                H5MM_malloc(sizeof(H5VL_iod_attr_open_input_t))))
	HGOTO_ERROR(H5E_ATTR, H5E_NOSPACE, HG_FAIL, "can't allocate input struct for decoding");

    if(HG_FAIL == HG_Handler_get_input(handle, input))
	HGOTO_ERROR(H5E_ATTR, H5E_CANTGET, HG_FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "AXE engine not started");

    input->hg_handle = handle;
    if (AXE_SUCCEED != AXEcreate_task(engine, &task, 0, NULL, 0, NULL, H5VL_iod_server_attr_open_cb, 
                                      input, NULL))
        HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");

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
    H5VL_iod_attr_io_input_t *input = NULL;
    AXE_task_t task;
    int ret_value = HG_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (input = (H5VL_iod_attr_io_input_t *)
                H5MM_malloc(sizeof(H5VL_iod_attr_io_input_t))))
	HGOTO_ERROR(H5E_ATTR, H5E_NOSPACE, HG_FAIL, "can't allocate input struct for decoding");

    if(HG_FAIL == HG_Handler_get_input(handle, input))
	HGOTO_ERROR(H5E_ATTR, H5E_CANTGET, HG_FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "AXE engine not started");

    input->hg_handle = handle;
    if (AXE_SUCCEED != AXEcreate_task(engine, &task, 0, NULL, 0, NULL, H5VL_iod_server_attr_read_cb, 
                                      input, NULL))
        HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");

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
    H5VL_iod_attr_io_input_t *input = NULL;
    AXE_task_t task;
    int ret_value = HG_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (input = (H5VL_iod_attr_io_input_t *)
                H5MM_malloc(sizeof(H5VL_iod_attr_io_input_t))))
	HGOTO_ERROR(H5E_ATTR, H5E_NOSPACE, HG_FAIL, "can't allocate input struct for decoding");

    if(HG_FAIL == HG_Handler_get_input(handle, input))
	HGOTO_ERROR(H5E_ATTR, H5E_CANTGET, HG_FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "AXE engine not started");

    input->hg_handle = handle;
    if (AXE_SUCCEED != AXEcreate_task(engine, &task, 0, NULL, 0, NULL, H5VL_iod_server_attr_write_cb, 
                                      input, NULL))
        HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");

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
    H5VL_iod_attr_op_input_t *input = NULL;
    AXE_task_t task;
    int ret_value = HG_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (input = (H5VL_iod_attr_op_input_t *)
                H5MM_malloc(sizeof(H5VL_iod_attr_op_input_t))))
	HGOTO_ERROR(H5E_ATTR, H5E_NOSPACE, HG_FAIL, "can't allocate input struct for decoding");

    if(HG_FAIL == HG_Handler_get_input(handle, input))
	HGOTO_ERROR(H5E_ATTR, H5E_CANTGET, HG_FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "AXE engine not started");

    input->hg_handle = handle;

    if (AXE_SUCCEED != AXEcreate_task(engine, &task, 0, NULL, 0, NULL, 
                                      H5VL_iod_server_attr_exists_cb, input, NULL))
        HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_attr_exists() */


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
    H5VL_iod_attr_op_input_t *input = NULL;
    AXE_task_t task;
    int ret_value = HG_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (input = (H5VL_iod_attr_op_input_t *)
                H5MM_malloc(sizeof(H5VL_iod_attr_op_input_t))))
	HGOTO_ERROR(H5E_ATTR, H5E_NOSPACE, HG_FAIL, "can't allocate input struct for decoding");

    if(HG_FAIL == HG_Handler_get_input(handle, input))
	HGOTO_ERROR(H5E_ATTR, H5E_CANTGET, HG_FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "AXE engine not started");

    input->hg_handle = handle;

    if (AXE_SUCCEED != AXEcreate_task(engine, &task, 0, NULL, 0, NULL, 
                                      H5VL_iod_server_attr_remove_cb, input, NULL))
        HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");

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
    H5VL_iod_remote_attr_t *input = NULL;
    AXE_task_t task;
    int ret_value = HG_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (input = (H5VL_iod_remote_attr_t *)
                H5MM_malloc(sizeof(H5VL_iod_remote_attr_t))))
	HGOTO_ERROR(H5E_ATTR, H5E_NOSPACE, HG_FAIL, "can't allocate input struct for decoding");

    if(HG_FAIL == HG_Handler_get_input(handle, input))
	HGOTO_ERROR(H5E_ATTR, H5E_CANTGET, HG_FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "AXE engine not started");

    input->hg_handle = handle;
    if (AXE_SUCCEED != AXEcreate_task(engine, &task, 0, NULL, 0, NULL, H5VL_iod_server_attr_close_cb, 
                                      input, NULL))
        HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");

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
    H5VL_iod_group_create_input_t *input = NULL;
    AXE_task_t task;
    int ret_value = HG_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (input = (H5VL_iod_group_create_input_t *)
                H5MM_malloc(sizeof(H5VL_iod_group_create_input_t))))
	HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, HG_FAIL, "can't allocate input struct for decoding");

    if(HG_FAIL == HG_Handler_get_input(handle, input))
	HGOTO_ERROR(H5E_SYM, H5E_CANTGET, HG_FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "AXE engine not started");

    input->hg_handle = handle;
    if (AXE_SUCCEED != AXEcreate_task(engine, &task, 0, NULL, 0, NULL, H5VL_iod_server_group_create_cb, 
                                      input, NULL))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");

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
    H5VL_iod_group_open_input_t *input = NULL;
    AXE_task_t task;
    int ret_value = HG_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (input = (H5VL_iod_group_open_input_t *)
                H5MM_malloc(sizeof(H5VL_iod_group_open_input_t))))
	HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, HG_FAIL, "can't allocate input struct for decoding");

    if(HG_FAIL == HG_Handler_get_input(handle, input))
	HGOTO_ERROR(H5E_SYM, H5E_CANTGET, HG_FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "AXE engine not started");

    input->hg_handle = handle;
    if (AXE_SUCCEED != AXEcreate_task(engine, &task, 0, NULL, 0, NULL, H5VL_iod_server_group_open_cb, 
                                      input, NULL))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");

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
    H5VL_iod_remote_group_t *input = NULL;
    AXE_task_t task;
    int ret_value = HG_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (input = (H5VL_iod_remote_group_t *)
                H5MM_malloc(sizeof(H5VL_iod_remote_group_t))))
	HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, HG_FAIL, "can't allocate input struct for decoding");

    if(HG_FAIL == HG_Handler_get_input(handle, input))
	HGOTO_ERROR(H5E_SYM, H5E_CANTGET, HG_FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "AXE engine not started");

    input->hg_handle = handle;
    if (AXE_SUCCEED != AXEcreate_task(engine, &task, 0, NULL, 0, NULL, H5VL_iod_server_group_close_cb, 
                                      input, NULL))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");

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
    H5VL_iod_dset_create_input_t *input = NULL;
    AXE_task_t task;
    int ret_value = HG_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (input = (H5VL_iod_dset_create_input_t *)
                H5MM_malloc(sizeof(H5VL_iod_dset_create_input_t))))
	HGOTO_ERROR(H5E_DATASET, H5E_NOSPACE, HG_FAIL, "can't allocate input struct for decoding");

    if(HG_FAIL == HG_Handler_get_input(handle, input))
	HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, HG_FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "AXE engine not started");

    input->hg_handle = handle;
    if (AXE_SUCCEED != AXEcreate_task(engine, &task, 0, NULL, 0, NULL, H5VL_iod_server_dset_create_cb, 
                                      input, NULL))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");

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
    H5VL_iod_dset_open_input_t *input = NULL;
    AXE_task_t task;
    int ret_value = HG_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (input = (H5VL_iod_dset_open_input_t *)
                H5MM_malloc(sizeof(H5VL_iod_dset_open_input_t))))
	HGOTO_ERROR(H5E_DATASET, H5E_NOSPACE, HG_FAIL, "can't allocate input struct for decoding");

    if(HG_FAIL == HG_Handler_get_input(handle, input))
	HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, HG_FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "AXE engine not started");

    input->hg_handle = handle;
    if (AXE_SUCCEED != AXEcreate_task(engine, &task, 0, NULL, 0, NULL, H5VL_iod_server_dset_open_cb, 
                                      input, NULL))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");

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
    H5VL_iod_dset_io_input_t *input = NULL;
    AXE_task_t task;
    int ret_value = HG_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (input = (H5VL_iod_dset_io_input_t *)
                H5MM_malloc(sizeof(H5VL_iod_dset_io_input_t))))
	HGOTO_ERROR(H5E_DATASET, H5E_NOSPACE, HG_FAIL, "can't allocate input struct for decoding");

    if(HG_FAIL == HG_Handler_get_input(handle, input))
	HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, HG_FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "AXE engine not started");

    input->hg_handle = handle;
    if (AXE_SUCCEED != AXEcreate_task(engine, &task, 0, NULL, 0, NULL, H5VL_iod_server_dset_read_cb, 
                                      input, NULL))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_dset_read() */


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
    H5VL_iod_dset_io_input_t *input = NULL;
    AXE_task_t task;
    int ret_value = HG_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (input = (H5VL_iod_dset_io_input_t *)
                H5MM_malloc(sizeof(H5VL_iod_dset_io_input_t))))
	HGOTO_ERROR(H5E_DATASET, H5E_NOSPACE, HG_FAIL, "can't allocate input struct for decoding");

    if(HG_FAIL == HG_Handler_get_input(handle, input))
	HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, HG_FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "AXE engine not started");

    input->hg_handle = handle;
    if (AXE_SUCCEED != AXEcreate_task(engine, &task, 0, NULL, 0, NULL, H5VL_iod_server_dset_write_cb, 
                                      input, NULL))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");

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
    H5VL_iod_dset_set_extent_input_t *input = NULL;
    AXE_task_t task;
    int ret_value = HG_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (input = (H5VL_iod_dset_set_extent_input_t *)
                H5MM_malloc(sizeof(H5VL_iod_dset_set_extent_input_t))))
	HGOTO_ERROR(H5E_DATASET, H5E_NOSPACE, HG_FAIL, "can't allocate input struct for decoding");

    if(HG_FAIL == HG_Handler_get_input(handle, input))
	HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, HG_FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "AXE engine not started");

    input->hg_handle = handle;

    if (AXE_SUCCEED != AXEcreate_task(engine, &task, 0, NULL, 0, NULL, 
                                      H5VL_iod_server_dset_set_extent_cb, input, NULL))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");

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
    H5VL_iod_remote_dset_t *input = NULL;
    AXE_task_t task;
    int ret_value = HG_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (input = (H5VL_iod_remote_dset_t *)
                H5MM_malloc(sizeof(H5VL_iod_remote_dset_t))))
	HGOTO_ERROR(H5E_DATASET, H5E_NOSPACE, HG_FAIL, "can't allocate input struct for decoding");

    if(HG_FAIL == HG_Handler_get_input(handle, input))
	HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, HG_FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "AXE engine not started");

    input->hg_handle = handle;
    if (AXE_SUCCEED != AXEcreate_task(engine, &task, 0, NULL, 0, NULL, H5VL_iod_server_dset_close_cb, 
                                      input, NULL))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");

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
 *              April, 2012
 *
 *-------------------------------------------------------------------------
 */
int
H5VL_iod_server_dtype_commit(hg_handle_t handle)
{
    H5VL_iod_dtype_commit_input_t *input = NULL;
    AXE_task_t task;
    int ret_value = HG_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (input = (H5VL_iod_dtype_commit_input_t *)
                H5MM_malloc(sizeof(H5VL_iod_dtype_commit_input_t))))
	HGOTO_ERROR(H5E_DATATYPE, H5E_NOSPACE, HG_FAIL, "can't allocate input struct for decoding");

    if(HG_FAIL == HG_Handler_get_input(handle, input))
	HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, HG_FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "AXE engine not started");

    input->hg_handle = handle;
    if (AXE_SUCCEED != AXEcreate_task(engine, &task, 0, NULL, 0, NULL, H5VL_iod_server_dtype_commit_cb, 
                                      input, NULL))
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");

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
 *              April, 2012
 *
 *-------------------------------------------------------------------------
 */
int
H5VL_iod_server_dtype_open(hg_handle_t handle)
{
    H5VL_iod_dtype_open_input_t *input = NULL;
    AXE_task_t task;
    int ret_value = HG_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (input = (H5VL_iod_dtype_open_input_t *)
                H5MM_malloc(sizeof(H5VL_iod_dtype_open_input_t))))
	HGOTO_ERROR(H5E_DATATYPE, H5E_NOSPACE, HG_FAIL, "can't allocate input struct for decoding");

    if(HG_FAIL == HG_Handler_get_input(handle, input))
	HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, HG_FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "AXE engine not started");

    input->hg_handle = handle;
    if (AXE_SUCCEED != AXEcreate_task(engine, &task, 0, NULL, 0, NULL, H5VL_iod_server_dtype_open_cb, 
                                      input, NULL))
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");

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
 *              April, 2012
 *
 *-------------------------------------------------------------------------
 */
int
H5VL_iod_server_dtype_close(hg_handle_t handle)
{
    H5VL_iod_remote_dtype_t *input = NULL;
    AXE_task_t task;
    int ret_value = HG_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (input = (H5VL_iod_remote_dtype_t *)
                H5MM_malloc(sizeof(H5VL_iod_remote_dtype_t))))
	HGOTO_ERROR(H5E_DATATYPE, H5E_NOSPACE, HG_FAIL, "can't allocate input struct for decoding");

    if(HG_FAIL == HG_Handler_get_input(handle, input))
	HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, HG_FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HG_FAIL, "AXE engine not started");

    input->hg_handle = handle;
    if (AXE_SUCCEED != AXEcreate_task(engine, &task, 0, NULL, 0, NULL, H5VL_iod_server_dtype_close_cb, 
                                      input, NULL))
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, HG_FAIL, "can't insert task into async engine");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_dtype_close() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_file_create_cb
 *
 * Purpose:	Creates a file as a iod HDF5 file.
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              January, 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_iod_server_file_create_cb(size_t UNUSED num_necessary_parents, AXE_task_t UNUSED necessary_parents[], 
                               size_t UNUSED num_sufficient_parents, AXE_task_t UNUSED sufficient_parents[], 
                               void *op_data)
{
    H5VL_iod_file_create_input_t *input = (H5VL_iod_file_create_input_t *)op_data;
    H5VL_iod_remote_file_t output;
    unsigned int mode;
    iod_handle_t coh;
    iod_handle_t root_handle, scratch_oh;
    iod_obj_id_t root_id, scratch_pad;
    iod_ret_t status;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    fprintf(stderr, "Start file create %s %d %d\n", input->name, input->fapl_id, input->fcpl_id);

    /* convert HDF5 flags to IOD flags */
    mode = (input->flags&H5F_ACC_RDWR) ? IOD_CONT_RW : IOD_CONT_RO;
    if (input->flags&H5F_ACC_CREAT) 
        mode |= IOD_CONT_CREATE;

    status = iod_container_open(input->name, NULL /*hints*/, mode, &coh, NULL /*event*/);

    if (EEXISTS != status && status == 0) {
        /* create root group KV store */
        if (iod_obj_create(coh, IOD_TID_UNKNOWN, NULL/*hints*/, IOD_OBJ_KV, NULL, NULL,
                              &root_id, NULL /*event*/) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "can't create root object");

        ROOT_ID = root_id;

        /* create scratch pad for root group */
        if (iod_obj_create(coh, IOD_TID_UNKNOWN, NULL/*hints*/, IOD_OBJ_KV, NULL, NULL,
                              &scratch_pad, NULL /*event*/) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "can't create scratch pad");

        /* open the root group */
        if (iod_obj_open_write(coh, root_id, NULL /*hints*/, &root_handle, NULL) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "can't open root object");

        /* add the scratch pad to the root group */
        if (iod_obj_set_scratch(root_handle, IOD_TID_UNKNOWN, &scratch_pad, NULL /*cs*/, NULL) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "can't set scratch pad for root object");

        /* open the scratch pad */
        if (iod_obj_open_write(coh, scratch_pad, NULL /*hints*/, &scratch_oh, NULL) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "can't open scratch pad");
    }
    else if (EEXISTS == status) {
        /* spin trying to open the root group */
        while (1) {
            if ((status = iod_obj_open_write(coh, root_id, NULL /*hints*/, &root_handle, NULL)) < 0)
                continue;
            else
                break;
        }
        /* spin trying to get scratch pad for the root group */
        while (1) {
            if((status = iod_obj_get_scratch(root_handle, IOD_TID_UNKNOWN, &scratch_pad, 
                                             NULL, NULL)) < 0)
                continue;
            else
                break;
        }

        /* open the scratch pad */
        if (iod_obj_open_write(coh, scratch_pad, NULL /*hints*/, &scratch_oh, NULL) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "can't open scratch pad");
    }
    else
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "can't create file");

#if H5_DO_NATIVE
    coh.cookie = H5Fcreate(input->name, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    HDassert(coh.cookie);
    root_handle.cookie = coh.cookie;
    fprintf(stderr, "Created Native file %s with ID %d\n", input->name, root_handle.cookie);
#endif

    output.coh = coh;
    output.root_id = root_id;
    output.root_oh = root_handle;
    output.scratch_id = scratch_pad;
    output.scratch_oh = scratch_oh;

    fprintf(stderr, "Done with file create, sending response to client\n");
    HG_Handler_start_output(input->hg_handle, &output);

done:
    if(ret_value < 0)
        HG_Handler_start_output(input->hg_handle, &ret_value);

    input = H5MM_xfree(input);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_file_create_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_file_open_cb
 *
 * Purpose:	Opens a file as a iod HDF5 file.
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              January, 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_iod_server_file_open_cb(size_t UNUSED num_necessary_parents, AXE_task_t UNUSED necessary_parents[], 
                             size_t UNUSED num_sufficient_parents, AXE_task_t UNUSED sufficient_parents[], 
                             void *op_data)
{
    H5VL_iod_file_open_input_t *input = (H5VL_iod_file_open_input_t *)op_data;
    H5VL_iod_remote_file_t output;
    unsigned int mode = input->flags;
    iod_handle_t coh;
    iod_handle_t root_handle, scratch_oh;
    iod_obj_id_t scratch_pad;
    iod_ret_t status;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    fprintf(stderr, "Start file open %s %d %d\n", input->name, input->flags, input->fapl_id);

    if((status = iod_container_open(input->name, NULL /*hints*/, mode, &coh, NULL /*event*/)) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "can't open file");

    /* open the root group */
    if (iod_obj_open_write(coh, ROOT_ID, NULL /*hints*/, &root_handle, NULL) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "can't open root object");

    /* get scratch pad of root group */
    if(iod_obj_get_scratch(root_handle, IOD_TID_UNKNOWN, &scratch_pad, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "can't get scratch pad for root object");

    /* open the scratch pad */
    if (iod_obj_open_write(coh, scratch_pad, NULL /*hints*/, &scratch_oh, NULL) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "can't open scratch pad");

#if H5_DO_NATIVE
    {
        int file_id;
        file_id = H5Fopen(input->name, H5F_ACC_RDWR, H5P_DEFAULT);
        HDassert(file_id);
        coh.cookie = file_id;
        root_handle.cookie = file_id;
    }
#endif

    output.coh = coh;
    output.root_id = ROOT_ID;
    output.root_oh = root_handle;
    output.scratch_id = scratch_pad;
    output.scratch_oh = scratch_oh;
    output.fcpl_id = H5P_FILE_CREATE_DEFAULT;

    fprintf(stderr, "Done with file open, sending response to client\n");
    HG_Handler_start_output(input->hg_handle, &output);

done:
    if(ret_value < 0)
        HG_Handler_start_output(input->hg_handle, &ret_value);

    input = H5MM_xfree(input);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_file_open_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_file_flush_cb
 *
 * Purpose:	Flushs iod HDF5 file.
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              January, 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_iod_server_file_flush_cb(size_t UNUSED num_necessary_parents, AXE_task_t UNUSED necessary_parents[], 
                             size_t UNUSED num_sufficient_parents, AXE_task_t UNUSED sufficient_parents[], 
                             void *op_data)
{
    H5VL_iod_file_flush_input_t *input = (H5VL_iod_file_flush_input_t *)op_data;
    iod_handle_t coh = input->coh;
    H5F_scope_t scope = input->scope;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    fprintf(stderr, "Start file flush with scope %d\n", scope);

#if H5_DO_NATIVE
    ret_value = H5Fflush(coh.cookie, scope);
#endif

done:
    fprintf(stderr, "Done with file flush, sending response to client\n");
    if(HG_SUCCESS != HG_Handler_start_output(input->hg_handle, &ret_value))
        HDONE_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "can't send result of file flush to client");

    input = H5MM_xfree(input);
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_file_flush_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_file_close_cb
 *
 * Purpose:	Closes iod HDF5 file.
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              January, 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_iod_server_file_close_cb(size_t UNUSED num_necessary_parents, AXE_task_t UNUSED necessary_parents[], 
                             size_t UNUSED num_sufficient_parents, AXE_task_t UNUSED sufficient_parents[], 
                             void *op_data)
{
    H5VL_iod_remote_file_t *input = (H5VL_iod_remote_file_t *)op_data;
    iod_handle_t coh = input->coh;
    iod_handle_t root_oh = input->root_oh;
    iod_handle_t scratch_oh = input->scratch_oh;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    fprintf(stderr, "Start file close\n");

#if H5_DO_NATIVE
    H5Fclose(coh.cookie);
#endif

    if(iod_obj_close(scratch_oh, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "can't close scratch object handle");
    if(iod_obj_close(root_oh, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "can't close root object handle");
    if(iod_container_close(coh, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTDEC, FAIL, "can't close container");

done:
    fprintf(stderr, "Done with file close, sending response to client\n");
    if(HG_SUCCESS != HG_Handler_start_output(input->hg_handle, &ret_value))
        HDONE_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "can't send result of file close to client");

    input = H5MM_xfree(input);
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_file_close_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_group_create_cb
 *
 * Purpose:	Creates a group as a iod object.
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              February, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_iod_server_group_create_cb(size_t UNUSED num_necessary_parents, AXE_task_t UNUSED necessary_parents[], 
                                size_t UNUSED num_sufficient_parents, AXE_task_t UNUSED sufficient_parents[], 
                                void *op_data)
{
    H5VL_iod_group_create_input_t *input = (H5VL_iod_group_create_input_t *)op_data;
    H5VL_iod_remote_group_t output;
    iod_handle_t coh = input->coh;
    iod_handle_t loc_handle = input->loc_oh;
    iod_handle_t cur_oh, scratch_oh;
    iod_obj_id_t cur_id, scratch_pad;
    const char *name = input->name;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    fprintf(stderr, "Start group create %s\n", name);

    /* the traversal will count the created group as an intermediate
       group and create it in the process */
    if(H5VL_iod_server_get_loc(coh, loc_handle, name, TRUE, FALSE, NULL,
                               &cur_id, &cur_oh, &scratch_pad, &scratch_oh) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't traverse path");

#if H5_DO_NATIVE
    cur_oh.cookie = H5Gcreate2(loc_handle.cookie, name, input->lcpl_id, 
                               input->gcpl_id, input->gapl_id);
    HDassert(cur_oh.cookie);
#endif

    output.iod_id = cur_id;
    output.iod_oh = cur_oh;
    output.scratch_id = scratch_pad;
    output.scratch_oh = scratch_oh;

    fprintf(stderr, "Done with group create, sending response to client\n");
    HG_Handler_start_output(input->hg_handle, &output);

done:
    if(ret_value < 0)
        HG_Handler_start_output(input->hg_handle, &ret_value);
    input = H5MM_xfree(input);

    FUNC_LEAVE_NOAPI(ret_value)
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
 *              February, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_iod_server_group_open_cb(size_t UNUSED num_necessary_parents, AXE_task_t UNUSED necessary_parents[], 
                              size_t UNUSED num_sufficient_parents, AXE_task_t UNUSED sufficient_parents[], 
                              void *op_data)
{
    H5VL_iod_group_open_input_t *input = (H5VL_iod_group_open_input_t *)op_data;
    H5VL_iod_remote_group_t output;
    iod_handle_t coh = input->coh;
    iod_handle_t loc_handle = input->loc_oh;
    iod_handle_t cur_oh, scratch_oh;
    iod_obj_id_t cur_id, scratch_pad;
    const char *name = input->name;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    fprintf(stderr, "Start group open %s\n", name);

    if(H5VL_iod_server_get_loc(coh, loc_handle, name, FALSE, FALSE, NULL,
                               &cur_id, &cur_oh, &scratch_pad, &scratch_oh) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't traverse path");

#if 0 
    if(iod_kv_get_value(scratch_oh, IOD_TID_UNKNOWN, "dataset_gcpl", NULL, 
                        &output.gcpl_size, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "dataset gcpl lookup failed");
    if(NULL == (output.gcpl = H5MM_malloc (output.gcpl_size)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate gcpl buffer");
    if(iod_kv_get_value(scratch_oh, IOD_TID_UNKNOWN, "dataset_gcpl", output.gcpl, 
                        &output.gcpl_size, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "dataset dcpl lookup failed");
#endif

#if H5_DO_NATIVE
    cur_oh.cookie = H5Gopen(loc_handle.cookie, name, input->gapl_id);
    HDassert(cur_oh.cookie);
#endif

    output.iod_id = cur_id;
    output.iod_oh = cur_oh;
    output.scratch_id = scratch_pad;
    output.scratch_oh = scratch_oh;
    output.gcpl_id = H5P_GROUP_CREATE_DEFAULT;

    fprintf(stderr, "Done with group open, sending response to client\n");
    HG_Handler_start_output(input->hg_handle, &output);

done:
    if(ret_value < 0)
        HG_Handler_start_output(input->hg_handle, &ret_value);
    input = H5MM_xfree(input);

    FUNC_LEAVE_NOAPI(ret_value)
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
static herr_t
H5VL_iod_server_group_close_cb(size_t UNUSED num_necessary_parents, AXE_task_t UNUSED necessary_parents[], 
                               size_t UNUSED num_sufficient_parents, AXE_task_t UNUSED sufficient_parents[], 
                               void *op_data)
{
    H5VL_iod_remote_group_t *input = (H5VL_iod_remote_group_t *)op_data;
    iod_handle_t iod_oh = input->iod_oh;
    iod_handle_t scratch_oh = input->scratch_oh;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    fprintf(stderr, "Start group close\n");

#if H5_DO_NATIVE
    ret_value = H5Gclose(iod_oh.cookie);
#endif

    if((ret_value = iod_obj_close(scratch_oh, NULL, NULL)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close scratch object handle");
    if((ret_value = iod_obj_close(iod_oh, NULL, NULL)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close root object handle");

done:
    fprintf(stderr, "Done with group close, sending response to client\n");
    HG_Handler_start_output(input->hg_handle, &ret_value);

    input = H5MM_xfree(input);
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_group_close_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_dset_create_cb
 *
 * Purpose:	Creates a dset as a iod object.
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              February, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_iod_server_dset_create_cb(size_t UNUSED num_necessary_parents, AXE_task_t UNUSED necessary_parents[], 
                                  size_t UNUSED num_sufficient_parents, AXE_task_t UNUSED sufficient_parents[], 
                                  void *op_data)
{
    H5VL_iod_dset_create_input_t *input = (H5VL_iod_dset_create_input_t *)op_data;
    H5VL_iod_remote_dset_t output;
    iod_handle_t coh = input->coh;
    iod_handle_t loc_handle = input->loc_oh;
    iod_handle_t cur_oh, scratch_oh;
    iod_obj_id_t cur_id, scratch_pad;
    const char *name = input->name;
    char *last_comp; /* the name of the dataset obtained from the last component in the path */
    iod_kv_t kv;
    iod_array_struct_t array;
    iod_size_t *max_dims;
    size_t buf_size;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    fprintf(stderr, "Start dataset Create %s\n", name);

    /* traverse and break at the last component in the path, i.e. the dataset */
    if(H5VL_iod_server_get_loc(coh, loc_handle, name, TRUE, TRUE, &last_comp,
                               &cur_id, &cur_oh, &scratch_pad, &scratch_oh) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't traverse path");

    /* Set the IOD array creation parameters */
    array.cell_size = H5Tget_size(input->type_id);
    array.num_dims = H5Sget_simple_extent_ndims(input->space_id);
    if(NULL == (array.current_dims = (iod_size_t *)malloc (sizeof(iod_size_t) * array.num_dims)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate dimention size array");
    if(NULL == (max_dims = (iod_size_t *)malloc (sizeof(iod_size_t) * array.num_dims)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate dimention size array");
    if(H5Sget_simple_extent_dims(input->space_id, array.current_dims, max_dims) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't get dimentions' sizes");
    array.firstdim_max = max_dims[0];
    array.chunk_dims = NULL;
    /* MSC - NEED TO FIX THAT */
#if 0
    if(layout.type == H5D_CHUNKED) {
        if(NULL == (array.chunk_dims = malloc (sizeof(iod_size_t) * layout.u.chunk.ndims)))
            HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate chunk dimention size array");
        array.chunk_dims;
    }
#endif
    array.dims_seq = NULL;

    fprintf(stderr, "now creating the dataset %s cellsize %d num dimenstions %d\n",
            last_comp, array.cell_size, array.num_dims);

    /* create the dataset */
    if (iod_obj_create(coh, IOD_TID_UNKNOWN, NULL/*hints*/, IOD_OBJ_ARRAY, NULL, &array,
                       &cur_id, NULL /*event*/) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't create current object handle");

    kv.key = HDstrdup(last_comp);
    kv.value = &cur_id;
    kv.value_len = sizeof(iod_obj_id_t);
    /* insert new dataset in kv store of current group */
    if (iod_kv_set(cur_oh, IOD_TID_UNKNOWN, NULL, &kv, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't set KV pair in parent");
    HDfree(kv.key);

    if(loc_handle.cookie != cur_oh.cookie) {
        /* close parent group and its scratch pad if it is not the
           location we started the traversal into */
        iod_obj_close(cur_oh, NULL, NULL);
        iod_obj_close(scratch_oh, NULL, NULL);
    }

    /* create scratch pad for dataset */
    if (iod_obj_create(coh, IOD_TID_UNKNOWN, NULL/*hints*/, IOD_OBJ_KV, NULL, NULL,
                       &scratch_pad, NULL /*event*/) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't create scratch pad");

    /* open the dataset */
    if (iod_obj_open_write(coh, cur_id, NULL /*hints*/, &cur_oh, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't open current group");

    /* add the scratch pad to the dataset */
    if (iod_obj_set_scratch(cur_oh, IOD_TID_UNKNOWN, &scratch_pad, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't set scratch pad");

    /* open the scratch pad */
    if (iod_obj_open_write(coh, scratch_pad, NULL /*hints*/, &scratch_oh, NULL) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "can't open scratch pad");

#if 0
    /* insert layout metadata into scratch pad */
    kv.key = HDstrdup("dataset_dcpl");
    /* determine the buffer size needed to store the encoded dcpl of the dataset */ 
    if(H5Pencode(input->dcpl_id,  NULL, &buf_size) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "failed to encode dataset dcpl");
    if(NULL == (kv.value = malloc (buf_size)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate dcpl buffer");
    /* encode dcpl of the dataset */ 
    if(H5Pencode(input->dcpl_id, kv.value, &buf_size) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "failed to encode dataset dcpl");
    kv.value_len = (iod_size_t)buf_size;
    /* insert kv pair into scratch pad */
    if (iod_kv_set(scratch_oh, IOD_TID_UNKNOWN, NULL, &kv, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't set KV pair in parent");
    HDfree(kv.key);
    free(kv.value);

    /* insert datatyoe metadata into scratch pad */
    kv.key = HDstrdup("dataset_dtype");
    /* determine the buffer size needed to store the encoded type of the dataset */ 
    if(H5Tencode(input->type_id, NULL, &buf_size) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "failed to encode dataset type");
    if(NULL == (kv.value = malloc (buf_size)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate type buffer");
    /* encode datatype of the dataset */ 
    if(H5Tencode(input->type_id, kv.value, &buf_size) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "failed to encode dataset type");
    kv.value_len = (iod_size_t)buf_size;
    /* insert kv pair into scratch pad */
    if (iod_kv_set(scratch_oh, IOD_TID_UNKNOWN, NULL, &kv, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't set KV pair in parent");
    HDfree(kv.key);
    free(kv.value);

    kv.key = HDstrdup("dataset_dspace");
    /* determine the buffer size needed to store the encoded space of the dataset */ 
    if(H5Sencode(input->space_id, NULL, &buf_size) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "failed to encode dataset space");
    if(NULL == (kv.value = malloc (buf_size)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate space buffer");
    /* encode dataspace of the dataset */ 
    if(H5Sencode(input->space_id, kv.value, &buf_size) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "failed to encode dataset space");
    kv.value_len = (iod_size_t)buf_size;
    /* insert kv pair into scratch pad */
    if (iod_kv_set(scratch_oh, IOD_TID_UNKNOWN, NULL, &kv, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't set KV pair in parent");
    HDfree(kv.key);
    free(kv.value);
#endif

#if H5_DO_NATIVE
    cur_oh.cookie = H5Dcreate2(loc_handle.cookie, last_comp, input->type_id, 
                               input->space_id, input->lcpl_id, 
                               input->dcpl_id, input->dapl_id);
    HDassert(cur_oh.cookie);
#endif

    output.iod_id = cur_id;
    output.iod_oh = cur_oh;
    output.scratch_id = scratch_pad;
    output.scratch_oh = scratch_oh;

    free(max_dims);
    free(array.current_dims);
    free(last_comp);

    fprintf(stderr, "Done with dset create, sending response to client\n");
    HG_Handler_start_output(input->hg_handle, &output);

done:
    if(ret_value < 0)
        HG_Handler_start_output(input->hg_handle, &ret_value);
    input = H5MM_xfree(input);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_dset_create_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_dset_open_cb
 *
 * Purpose:	Opens a dataset as a iod object.
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              February, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_iod_server_dset_open_cb(size_t UNUSED num_necessary_parents, AXE_task_t UNUSED necessary_parents[], 
                                size_t UNUSED num_sufficient_parents, AXE_task_t UNUSED sufficient_parents[], 
                                void *op_data)
{
    H5VL_iod_dset_open_input_t *input = (H5VL_iod_dset_open_input_t *)op_data;
    H5VL_iod_remote_dset_t output;
    iod_handle_t coh = input->coh;
    iod_handle_t loc_handle = input->loc_oh;
    iod_handle_t cur_oh, scratch_oh;
    iod_obj_id_t cur_id, scratch_pad;
    char *name = input->name;
    char *last_comp;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    fprintf(stderr, "Start dataset Open %s\n", name);

    /* traverse the path and open the dataset object as we do it */
    if(H5VL_iod_server_get_loc(coh, loc_handle, name, FALSE, FALSE, &last_comp,
                               &cur_id, &cur_oh, &scratch_pad, &scratch_oh) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't traverse path");

#if 0
    /*retrieve all metadata from scratch pad */
    if(iod_kv_get_value(scratch_oh, IOD_TID_UNKNOWN, "dataset_dcpl", NULL, 
                        &output.dcpl_size, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "dataset dcpl lookup failed");
    if(NULL == (output.dcpl = H5MM_malloc (output.dcpl_size)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate dcpl buffer");
    if(iod_kv_get_value(scratch_oh, IOD_TID_UNKNOWN, "dataset_dcpl", output.dcpl, 
                        &output.dcpl_size, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "dataset dcpl lookup failed");

    if(iod_kv_get_value(scratch_oh, IOD_TID_UNKNOWN, "dataset_dtype", NULL, 
                        &output.dtype_size, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "dataset dtype lookup failed");
    if(NULL == (output.dtype = H5MM_malloc (output.dtype_size)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate dtype buffer");
    if(iod_kv_get_value(scratch_oh, IOD_TID_UNKNOWN, "dataset_dtype", output.dtype, 
                        &output.dtype_size, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "dataset dtype lookup failed");

    if(iod_kv_get_value(scratch_oh, IOD_TID_UNKNOWN, "dataset_dspace", NULL, 
                        &output.dspace_size, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "dataset dspace lookup failed");
    if(NULL == (output.dspace = H5MM_malloc (output.dspace_size)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate dspace buffer");
    if(iod_kv_get_value(scratch_oh, IOD_TID_UNKNOWN, "dataset_dspace", output.dspace, 
                        &output.dspace_size, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "dataset dspace lookup failed");
#endif

    {
        hsize_t dims[1];
        //hid_t space_id, type_id;

#if H5_DO_NATIVE
        printf("dataset name %s    location %d\n", last_comp, loc_handle.cookie);
        cur_oh.cookie = H5Dopen(loc_handle.cookie, last_comp, input->dapl_id);
        HDassert(cur_oh.cookie);
        output.space_id = H5Dget_space(cur_oh.cookie);
        output.type_id = H5Dget_type(cur_oh.cookie);
        output.dcpl_id = H5Dget_create_plist(cur_oh.cookie);
#else
        /* fake a dataspace, type, and dcpl */
        dims [0] = 60;
        output.space_id = H5Screate_simple(1, dims, NULL);
        output.type_id = H5Tcopy(H5T_NATIVE_INT);
        output.dcpl_id = H5P_DATASET_CREATE_DEFAULT;
#endif

#if 0
        output.dcpl_size = 0;
        output.dcpl = NULL;

        /* get Type size to encode */
        if(H5Tencode(type_id, NULL, &output.dtype_size) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "failed to encode dataset type");
        if(NULL == (output.dtype = H5MM_malloc (output.dtype_size)))
            HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate datatype buffer");
        if(H5Tencode(type_id, output.dtype, &output.dtype_size) < 0)
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTENCODE, FAIL, "can't encode datatype");

        /* get Dataspace size to encode */
        if(H5Sencode(space_id, NULL, &output.dspace_size)<0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTENCODE, FAIL, "can't encode dataspace");
        if(NULL == (output.dspace = H5MM_malloc (output.dspace_size)))
            HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate datatype buffer");
        if(H5Sencode(space_id, output.dspace, &output.dspace_size) < 0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTENCODE, FAIL, "can't encode dataspace");

        H5Sclose(space_id);
#endif

    }

    output.iod_id = cur_id;
    output.iod_oh = cur_oh;
    output.scratch_id = scratch_pad;
    output.scratch_oh = scratch_oh;

    fprintf(stderr, "Done with dset open, sending response to client\n");
    HG_Handler_start_output(input->hg_handle, &output);

done:
    if(ret_value < 0)
        HG_Handler_start_output(input->hg_handle, &ret_value);

    input = H5MM_xfree(input);
    free(last_comp);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_dset_open_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_dset_read_cb
 *
 * Purpose:	Reads from IOD into the function shipper BDS handle.
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              January, 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_iod_server_dset_read_cb(size_t UNUSED num_necessary_parents, AXE_task_t UNUSED necessary_parents[], 
                             size_t UNUSED num_sufficient_parents, AXE_task_t UNUSED sufficient_parents[], 
                             void *op_data)
{
    H5VL_iod_dset_io_input_t *input = (H5VL_iod_dset_io_input_t *)op_data;
    H5VL_iod_read_status_t output;
    iod_handle_t iod_oh = input->iod_oh;
    iod_handle_t scratch_oh = input->scratch_oh;
    hg_bulk_t bulk_handle = input->bulk_handle;
    hid_t space_id = input->space_id;
    hid_t dxpl_id = input->dxpl_id;
    hg_bulk_block_t bulk_block_handle;
    iod_mem_desc_t mem_desc;
    iod_array_iodesc_t file_desc;
    size_t size;
    void *buf;
    uint32_t cs = 0;
    na_addr_t dest = HG_Handler_get_addr(input->hg_handle);
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    size = HG_Bulk_handle_get_size(bulk_handle);
    fprintf(stderr, "Start dataset Read of size %d\n", size);
    if(NULL == (buf = malloc(size)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate read buffer");

    mem_desc.nfrag = 1;
    //mem_desc.frag[0].addr = buf;
    //mem_desc.frag[0].len = (iod_size_t)size;

    /* MSC TODO - populate file location hyperslab */

    /* read from array object */
    if(iod_array_read(iod_oh, IOD_TID_UNKNOWN, NULL, &mem_desc, &file_desc, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_READERROR, FAIL, "can't read from array object");
    
    {
        int i;
        hbool_t flag;
        int *buf_ptr = (int *)buf;

#if H5_DO_NATIVE
    ret_value = H5Dread(iod_oh.cookie, H5T_NATIVE_INT, H5S_ALL, space_id, dxpl_id, buf);
#else
    for(i=0;i<60;++i)
        buf_ptr[i] = i;
#endif
        if(H5Pget_dxpl_inject_bad_checksum(dxpl_id, &flag) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_READERROR, FAIL, "can't read property list");
        if(flag) {
            fprintf(stderr, "Injecting a bad data value to generate a bad checksum \n");
            buf_ptr[0] = 10;
        }
        cs = H5_checksum_fletcher32(buf, size);
        fprintf(stderr, "Checksum Generated for data at server: %u\n", cs);
    }

    /* Create a new block handle to write the data */
    HG_Bulk_block_handle_create(buf, size, HG_BULK_READ_ONLY, &bulk_block_handle);

    /* Write bulk data here and wait for the data to be there  */
    if(HG_SUCCESS != HG_Bulk_write(bulk_handle, dest, bulk_block_handle))
        HGOTO_ERROR(H5E_SYM, H5E_READERROR, FAIL, "can't read from array object");
    /* wait for it to complete */
    if(HG_SUCCESS != HG_Bulk_wait(bulk_block_handle, HG_BULK_MAX_IDLE_TIME))
        HGOTO_ERROR(H5E_SYM, H5E_READERROR, FAIL, "can't read from array object");

done:
    output.ret = ret_value;
    output.cs = cs;

    fprintf(stderr, "Done with dset read, sending response to client\n");

    if(HG_SUCCESS != HG_Handler_start_output(input->hg_handle, &output))
        HDONE_ERROR(H5E_SYM, H5E_WRITEERROR, FAIL, "can't send result of write to client");
    if(HG_SUCCESS != HG_Bulk_block_handle_free(bulk_block_handle))
        HDONE_ERROR(H5E_SYM, H5E_WRITEERROR, FAIL, "can't free bds block handle");

    input = H5MM_xfree(input);
    free(buf);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_dset_read_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_dset_write_cb
 *
 * Purpose:	Writes from IOD into the function shipper BDS handle.
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              January, 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_iod_server_dset_write_cb(size_t UNUSED num_necessary_parents, AXE_task_t UNUSED necessary_parents[], 
                             size_t UNUSED num_sufficient_parents, AXE_task_t UNUSED sufficient_parents[], 
                             void *op_data)
{
    H5VL_iod_dset_io_input_t *input = (H5VL_iod_dset_io_input_t *)op_data;
    iod_handle_t iod_oh = input->iod_oh;
    iod_handle_t scratch_oh = input->scratch_oh;
    hg_bulk_t bulk_handle = input->bulk_handle;
    hid_t space_id = input->space_id;
    hid_t dxpl_id = input->dxpl_id;
    uint32_t cs = input->checksum;
    hg_bulk_block_t bulk_block_handle;
    iod_mem_desc_t mem_desc;
    iod_array_iodesc_t file_desc;
    size_t size;
    void *buf;
    ssize_t ret;
    na_addr_t source = HG_Handler_get_addr(input->hg_handle);
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    fprintf(stderr, "Start dataset Write with checksum %u\n", cs);

    /* Read bulk data here and wait for the data to be here  */
    size = HG_Bulk_handle_get_size(bulk_handle);
    if(NULL == (buf = malloc(size)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate read buffer");

    HG_Bulk_block_handle_create(buf, size, HG_BULK_READWRITE, &bulk_block_handle);

    /* Write bulk data here and wait for the data to be there  */
    if(HG_SUCCESS != HG_Bulk_read(bulk_handle, source, bulk_block_handle))
        HGOTO_ERROR(H5E_SYM, H5E_WRITEERROR, FAIL, "can't get data from function shipper");
    /* wait for it to complete */
    if(HG_SUCCESS != HG_Bulk_wait(bulk_block_handle, HG_BULK_MAX_IDLE_TIME))
        HGOTO_ERROR(H5E_SYM, H5E_WRITEERROR, FAIL, "can't get data from function shipper");

    /* free the bds block handle */
    if(HG_SUCCESS != HG_Bulk_block_handle_free(bulk_block_handle))
        HGOTO_ERROR(H5E_SYM, H5E_WRITEERROR, FAIL, "can't free bds block handle");

    { 
        int i;
        int *buf_ptr = (int *)buf;

        fprintf(stderr, "Received a buffer of size %d with values: ", size);
        for(i=0;i<60;++i)
            fprintf(stderr, "%d ", buf_ptr[i]);
        fprintf(stderr, "\n");
    }

    mem_desc.nfrag = 1;
    //mem_desc.frag[0].addr = buf;
    //mem_desc.frag[0].len = (iod_size_t)size;

    /* MSC TODO - populate file location hyperslab */

    /* write from array object */
    if(iod_array_write(iod_oh, IOD_TID_UNKNOWN, NULL, &mem_desc, &file_desc, &cs, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_WRITEERROR, FAIL, "can't write to array object");

#if H5_DO_NATIVE
    ret_value = H5Dwrite(iod_oh.cookie, H5T_NATIVE_INT, H5S_ALL, space_id, dxpl_id, buf);
#endif

done:
    fprintf(stderr, "Done with dset write, sending %d response to client\n", ret_value);
    if(HG_SUCCESS != HG_Handler_start_output(input->hg_handle, &ret_value))
        HDONE_ERROR(H5E_SYM, H5E_WRITEERROR, FAIL, "can't send result of write to client");

    input = H5MM_xfree(input);
    free(buf);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_dset_write_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_dset_set_extent_cb
 *
 * Purpose:	Set_Extents iod HDF5 dataset.
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              January, 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_iod_server_dset_set_extent_cb(size_t UNUSED num_necessary_parents, AXE_task_t UNUSED necessary_parents[], 
                                   size_t UNUSED num_sufficient_parents, AXE_task_t UNUSED sufficient_parents[], 
                                   void *op_data)
{
    H5VL_iod_dset_set_extent_input_t *input = (H5VL_iod_dset_set_extent_input_t *)op_data;
    iod_handle_t iod_oh = input->iod_oh;
    int rank = input->dims.rank;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    fprintf(stderr, "Start dataset Extend on the first dimension to %d\n", input->dims.size[0]);

    if(iod_array_extend(iod_oh, IOD_TID_UNKNOWN, (iod_size_t)input->dims.size[0], NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't extend dataset");

#if H5_DO_NATIVE
    ret_value = H5Dset_extent(iod_oh.cookie, input->dims.size);
#endif

done:
    fprintf(stderr, "Done with dset set_extent, sending response to client\n");
    HG_Handler_start_output(input->hg_handle, &ret_value);
    input = H5MM_xfree(input);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_dset_set_extent_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_dset_close_cb
 *
 * Purpose:	Closes iod HDF5 dataset.
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              January, 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_iod_server_dset_close_cb(size_t UNUSED num_necessary_parents, AXE_task_t UNUSED necessary_parents[], 
                              size_t UNUSED num_sufficient_parents, AXE_task_t UNUSED sufficient_parents[], 
                              void *op_data)
{
    H5VL_iod_remote_dset_t *input = (H5VL_iod_remote_dset_t *)op_data;
    iod_handle_t iod_oh = input->iod_oh;
    iod_handle_t scratch_oh = input->scratch_oh;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    fprintf(stderr, "Start dataset Close\n");

#if H5_DO_NATIVE
    ret_value = H5Dclose(iod_oh.cookie);
#endif

    if((ret_value = iod_obj_close(scratch_oh, NULL, NULL)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close scratch object handle");
    if((ret_value = iod_obj_close(iod_oh, NULL, NULL)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close root object handle");

done:
    fprintf(stderr, "Done with dset close, sending response to client\n");
    HG_Handler_start_output(input->hg_handle, &ret_value);

    input = H5MM_xfree(input);
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_dset_close_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_dtype_commit_cb
 *
 * Purpose:	Commits a dtype as a iod object.
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              April, 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_iod_server_dtype_commit_cb(size_t UNUSED num_necessary_parents, AXE_task_t UNUSED necessary_parents[], 
                                size_t UNUSED num_sufficient_parents, AXE_task_t UNUSED sufficient_parents[], 
                                void *op_data)
{
    H5VL_iod_dtype_commit_input_t *input = (H5VL_iod_dtype_commit_input_t *)op_data;
    H5VL_iod_remote_dtype_t output;
    iod_handle_t coh = input->coh;
    iod_handle_t loc_handle = input->loc_oh;
    iod_handle_t cur_oh, scratch_oh;
    iod_obj_id_t cur_id, scratch_pad;
    const char *name = input->name;
    iod_kv_t kv;
    char *last_comp; /* the name of the datatype obtained from the last component in the path */
    size_t buf_size;
    void *buf;
    iod_mem_desc_t mem_desc;
    iod_blob_iodesc_t file_desc;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    fprintf(stderr, "Start datatype Commit %s\n", name);

    /* traverse and break at the last component in the path, i.e. the dataset */
    if(H5VL_iod_server_get_loc(coh, loc_handle, name, TRUE, TRUE, &last_comp,
                               &cur_id, &cur_oh, &scratch_pad, &scratch_oh) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't traverse path");

    fprintf(stderr, "now creating the Blob object for datatype \n");

    /* create the datatype */
    if (iod_obj_create(coh, IOD_TID_UNKNOWN, NULL/*hints*/, IOD_OBJ_BLOB, NULL, NULL,
                       &cur_id, NULL /*event*/) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't create current object handle");

    kv.key = HDstrdup(last_comp);
    kv.value = &cur_id;
    kv.value_len = sizeof(iod_obj_id_t);
    /* insert new datatype in kv store of current group */
    if (iod_kv_set(cur_oh, IOD_TID_UNKNOWN, NULL, &kv, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't set KV pair in parent");
    HDfree(kv.key);

    /* create scratch pad for datatype */
    if (iod_obj_create(coh, IOD_TID_UNKNOWN, NULL/*hints*/, IOD_OBJ_KV, NULL, NULL,
                       &scratch_pad, NULL /*event*/) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't create scratch pad");

    /* open the datatype */
    if (iod_obj_open_write(coh, cur_id, NULL /*hints*/, &cur_oh, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't open current group");

    /* add the scratch pad to the datatype */
    if (iod_obj_set_scratch(cur_oh, IOD_TID_UNKNOWN, &scratch_pad, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't set scratch pad");

    /* open the scratch pad */
    if (iod_obj_open_write(coh, scratch_pad, NULL /*hints*/, &scratch_oh, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't open scratch pad");

    /* determine the buffer size needed to store the encoded type of the datatype */ 
    if(H5Tencode(input->type_id, NULL, &buf_size) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "failed to encode datatype type");
    if(NULL == (buf = malloc (buf_size)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate type buffer");
    /* encode datatype of the datatype */ 
    if(H5Tencode(input->type_id, buf, &buf_size) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "failed to encode datatype type");

    mem_desc.nfrag = 1;
    //mem_desc.frag[0].addr = buf;
    //mem_desc.frag[0].len = (iod_size_t)buf_size;

    if(iod_blob_write(cur_oh, IOD_TID_UNKNOWN, NULL, &mem_desc, &file_desc, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to write BLOB object");

#if 0
    /* insert datatype metadata into scratch pad */
    kv.key = HDstrdup("datatype_tcpl");
    /* determine the buffer size needed to store the encoded tcpl of the datatype */ 
    if(H5Pencode(input->tcpl_id,  NULL, &buf_size) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "failed to encode datatype tcpl");
    if(NULL == (kv.value = malloc (buf_size)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate tcpl buffer");
    /* encode tcpl of the datatype */ 
    if(H5Pencode(input->tcpl_id, kv.value, &buf_size) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "failed to encode datatype tcpl");
    kv.value_len = (iod_size_t)buf_size;
    /* insert kv pair into scratch pad */
    if (iod_kv_set(scratch_oh, IOD_TID_UNKNOWN, NULL, &kv, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't set KV pair in parent");
    HDfree(kv.key);
    free(kv.value);
#endif

#if H5_DO_NATIVE
    cur_oh.cookie = H5Tcopy(input->type_id);
    if(H5Tcommit2(loc_handle.cookie, last_comp, cur_oh.cookie, 
                  H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't commit datatype");
    fprintf(stderr, "Committed Native Datatype %s with ID %d on %d\n",
            last_comp, cur_oh.cookie, loc_handle.cookie);
#endif

    output.iod_id = cur_id;
    output.iod_oh = cur_oh;
    output.scratch_id = scratch_pad;
    output.scratch_oh = scratch_oh;

    fprintf(stderr, "Done with dtype commit, sending response to client\n");
    HG_Handler_start_output(input->hg_handle, &output);

done:
    if(ret_value < 0)
        HG_Handler_start_output(input->hg_handle, &ret_value);

    input = H5MM_xfree(input);
    free(last_comp);
    free(buf);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_dtype_commit_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_dtype_open_cb
 *
 * Purpose:	Opens a datatype as a iod object.
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              April, 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_iod_server_dtype_open_cb(size_t UNUSED num_necessary_parents, AXE_task_t UNUSED necessary_parents[], 
                              size_t UNUSED num_sufficient_parents, AXE_task_t UNUSED sufficient_parents[], 
                              void *op_data)
{
    H5VL_iod_dtype_open_input_t *input = (H5VL_iod_dtype_open_input_t *)op_data;
    H5VL_iod_remote_dtype_t output;
    iod_handle_t coh = input->coh;
    iod_handle_t loc_handle = input->loc_oh;
    iod_handle_t cur_oh, scratch_oh;
    iod_obj_id_t cur_id, scratch_pad;
    char *name = input->name;
    size_t buf_size;
    void *buf;
    iod_mem_desc_t mem_desc;
    iod_blob_iodesc_t file_desc;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    fprintf(stderr, "Start datatype Open %s\n", name);

    /* traverse the path and open the dataset object as we do it */
    if(H5VL_iod_server_get_loc(coh, loc_handle, name, FALSE, FALSE, NULL,
                               &cur_id, &cur_oh, &scratch_pad, &scratch_oh) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't traverse path");

    /* MSC - need to read datatype; size should be stored in metadata,
       but since no real IOD, can't do anything now */

#if 0
    /*retrieve all metadata from scratch pad */
    if(iod_kv_get_value(scratch_oh, IOD_TID_UNKNOWN, "datatype_tcpl", NULL, 
                        &output.tcpl_size, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "datatype tcpl lookup failed");
    if(NULL == (output.tcpl = H5MM_malloc (output.tcpl_size)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate tcpl buffer");
    if(iod_kv_get_value(scratch_oh, IOD_TID_UNKNOWN, "datatype_tcpl", output.tcpl, 
                        &output.tcpl_size, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "datatype tcpl lookup failed");
#endif

    {
        hsize_t dims[1];
        //hid_t space_id, type_id;

#if H5_DO_NATIVE
        printf("datatype name %s    location %d\n", name, loc_handle.cookie);
        cur_oh.cookie = H5Topen(loc_handle.cookie, name, input->tapl_id);
        HDassert(cur_oh.cookie);
        output.type_id = cur_oh.cookie;//H5Tget_type(cur_oh.cookie);
        output.tcpl_id = H5Tget_create_plist(cur_oh.cookie);
#else
        /* fake a type, and tcpl */
        dims [0] = 60;
        output.type_id = H5Tcopy(H5T_NATIVE_INT);
        output.tcpl_id = H5P_DATATYPE_CREATE_DEFAULT;
#endif

#if 0
        output.tcpl_size = 0;
        output.tcpl = NULL;

        /* get Type size to encode */
        if(H5Tencode(type_id, NULL, &output.dtype_size) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "failed to encode datatype type");
        if(NULL == (output.dtype = H5MM_malloc (output.dtype_size)))
            HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate datatype buffer");
        if(H5Tencode(type_id, output.dtype, &output.dtype_size) < 0)
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTENCODE, FAIL, "can't encode datatype");
#endif
    }

    output.iod_id = cur_id;
    output.iod_oh = cur_oh;
    output.scratch_id = scratch_pad;
    output.scratch_oh = scratch_oh;

    fprintf(stderr, "Done with dtype open, sending response to client\n");
    HG_Handler_start_output(input->hg_handle, &output);

done:
    if(ret_value < 0)
        HG_Handler_start_output(input->hg_handle, &ret_value);

    input = H5MM_xfree(input);
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_dtype_open_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_dtype_close_cb
 *
 * Purpose:	Closes iod HDF5 datatype.
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              April, 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_iod_server_dtype_close_cb(size_t UNUSED num_necessary_parents, AXE_task_t UNUSED necessary_parents[], 
                              size_t UNUSED num_sufficient_parents, AXE_task_t UNUSED sufficient_parents[], 
                              void *op_data)
{
    H5VL_iod_remote_dtype_t *input = (H5VL_iod_remote_dtype_t *)op_data;
    iod_handle_t iod_oh = input->iod_oh;
    iod_handle_t scratch_oh = input->scratch_oh;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    fprintf(stderr, "Start datatype Close %d\n", iod_oh.cookie);

#if H5_DO_NATIVE
    HDassert(H5Tclose(iod_oh.cookie) == SUCCEED);
#endif

    if((ret_value = iod_obj_close(scratch_oh, NULL, NULL)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close scratch object handle");
    if((ret_value = iod_obj_close(iod_oh, NULL, NULL)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close root object handle");

done:
    fprintf(stderr, "Done with dtype close, sending response to client\n");
    HG_Handler_start_output(input->hg_handle, &ret_value);

    input = H5MM_xfree(input);
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_dtype_close_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_attr_create_cb
 *
 * Purpose:	Creates a attr as a iod object.
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              April, 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_iod_server_attr_create_cb(size_t UNUSED num_necessary_parents, AXE_task_t UNUSED necessary_parents[], 
                               size_t UNUSED num_sufficient_parents, AXE_task_t UNUSED sufficient_parents[], 
                               void *op_data)
{
    H5VL_iod_attr_create_input_t *input = (H5VL_iod_attr_create_input_t *)op_data;
    H5VL_iod_remote_attr_t output;
    iod_handle_t coh = input->coh;
    iod_handle_t loc_handle = input->loc_oh;
    iod_handle_t cur_oh, scratch_oh;
    iod_obj_id_t cur_id, scratch_pad;
    const char *loc_name = input->path;
    const char *attr_name = input->attr_name;
    char *last_comp = NULL;
    iod_array_struct_t array;
    iod_size_t *max_dims;
    iod_kv_t kv;
    size_t buf_size;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    fprintf(stderr, "Start attribute Create %s on object path %s\n", attr_name, loc_name);

    /* traverse path */
    if(H5VL_iod_server_get_loc(coh, loc_handle, loc_name, FALSE, FALSE, &last_comp,
                               &cur_id, &cur_oh, &scratch_pad, &scratch_oh) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't traverse path");

    /* Set the IOD array creation parameters */
    array.cell_size = H5Tget_size(input->type_id);
    array.num_dims = H5Sget_simple_extent_ndims(input->space_id);
    if(NULL == (array.current_dims = (iod_size_t *)malloc (sizeof(iod_size_t) * array.num_dims)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate dimention size array");
    if(NULL == (max_dims = (iod_size_t *)malloc (sizeof(iod_size_t) * array.num_dims)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate dimention size array");
    if(H5Sget_simple_extent_dims(input->space_id, array.current_dims, max_dims) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't get dimentions' sizes");
    array.firstdim_max = max_dims[0];
    array.chunk_dims = NULL;
    array.dims_seq = NULL;

    /* create the dataset */
    if (iod_obj_create(coh, IOD_TID_UNKNOWN, NULL/*hints*/, IOD_OBJ_ARRAY, NULL, &array,
                       &cur_id, NULL /*event*/) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't create current object handle");

    /* insert new attribute in scratch pad of current object */
    kv.key = HDstrdup(attr_name);
    kv.value = &cur_id;
    kv.value_len = 0;
    if (iod_kv_set(scratch_oh, IOD_TID_UNKNOWN, NULL, &kv, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't set KV pair in parent");
    HDfree(kv.key);

    if(loc_handle.cookie != cur_oh.cookie) {
        /* close parent group and its scratch pad if it is not the
           location we started the traversal into */
        iod_obj_close(cur_oh, NULL, NULL);
        iod_obj_close(scratch_oh, NULL, NULL);
    }

    /* create scratch pad for attribute */
    if (iod_obj_create(coh, IOD_TID_UNKNOWN, NULL/*hints*/, IOD_OBJ_KV, NULL, NULL,
                       &scratch_pad, NULL /*event*/) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't create scratch pad");

    /* open the attribute */
    if (iod_obj_open_write(coh, cur_id, NULL /*hints*/, &cur_oh, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't open current group");

    /* add the scratch pad to the attribute */
    if (iod_obj_set_scratch(cur_oh, IOD_TID_UNKNOWN, &scratch_pad, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't set scratch pad");

    /* open the scratch pad */
    if (iod_obj_open_write(coh, scratch_pad, NULL /*hints*/, &scratch_oh, NULL) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "can't open scratch pad");

#if 0
    /* insert datatype metadata into scratch pad */
    kv.key = HDstrdup("attribute_dtype");
    /* determine the buffer size needed to store the encoded type of the attribute */ 
    if(H5Tencode(input->type_id, NULL, &buf_size) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "failed to encode attribute type");
    if(NULL == (kv.value = malloc (buf_size)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate type buffer");
    /* encode datatype of the attribute */ 
    if(H5Tencode(input->type_id, kv.value, &buf_size) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "failed to encode attribute type");
    kv.value_len = (iod_size_t)buf_size;
    /* insert kv pair into scratch pad */
    if (iod_kv_set(scratch_oh, IOD_TID_UNKNOWN, NULL, &kv, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't set KV pair in parent");
    HDfree(kv.key);
    free(kv.value);

    kv.key = HDstrdup("attribute_dspace");
    /* determine the buffer size needed to store the encoded space of the attribute */ 
    if(H5Sencode(input->space_id, NULL, &buf_size) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "failed to encode attribute space");
    if(NULL == (kv.value = malloc (buf_size)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate space buffer");
    /* encode dataspace of the attribute */ 
    if(H5Sencode(input->space_id, kv.value, &buf_size) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "failed to encode attribute space");
    kv.value_len = (iod_size_t)buf_size;
    /* insert kv pair into scratch pad */
    if (iod_kv_set(scratch_oh, IOD_TID_UNKNOWN, NULL, &kv, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't set KV pair in parent");
    HDfree(kv.key);
    free(kv.value);
#endif

#if H5_DO_NATIVE
    cur_oh.cookie = H5Acreate2(cur_oh.cookie, attr_name, input->type_id, 
                               input->space_id, H5P_DEFAULT, H5P_DEFAULT);
    HDassert(cur_oh.cookie);
#endif

    output.iod_id = cur_id;
    output.iod_oh = cur_oh;
    output.scratch_id = scratch_pad;
    output.scratch_oh = scratch_oh;

    free(max_dims);
    free(array.current_dims);

    fprintf(stderr, "Done with attr create, sending response to client\n");
    HG_Handler_start_output(input->hg_handle, &output);

done:
    if(ret_value < 0)
        HG_Handler_start_output(input->hg_handle, &ret_value);
    input = H5MM_xfree(input);
    if(last_comp)
        free(last_comp);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_attr_create_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_attr_open_cb
 *
 * Purpose:	Opens a attribute as a iod object.
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              April, 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_iod_server_attr_open_cb(size_t UNUSED num_necessary_parents, AXE_task_t UNUSED necessary_parents[], 
                             size_t UNUSED num_sufficient_parents, AXE_task_t UNUSED sufficient_parents[], 
                             void *op_data)
{
    H5VL_iod_attr_open_input_t *input = (H5VL_iod_attr_open_input_t *)op_data;
    H5VL_iod_remote_attr_t output;
    iod_handle_t coh = input->coh;
    iod_handle_t loc_handle = input->loc_oh;
    iod_handle_t cur_oh, scratch_oh;
    iod_obj_id_t cur_id, scratch_pad;
    const char *loc_name = input->path;
    const char *attr_name = input->attr_name;
    char *last_comp = NULL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    fprintf(stderr, "Start attribute Open %s\n", attr_name);

    /* traverse the path and open the location object as we do it */
    if(H5VL_iod_server_get_loc(coh, loc_handle, loc_name, FALSE, FALSE, &last_comp,
                               &cur_id, &cur_oh, &scratch_pad, &scratch_oh) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't traverse path");

    /*retrieve that attribute ID from the scratch pad and open it */
    if(iod_kv_get_value(scratch_oh, IOD_TID_UNKNOWN, attr_name, &cur_id, 
                        sizeof(iod_obj_id_t), NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Failed to retrieve attribute ID");

    /* open the attribute */
    if (iod_obj_open_write(coh, cur_id, NULL /*hints*/, &cur_oh, NULL) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "can't open scratch pad");

    {
        hsize_t dims[1];
        //hid_t space_id, type_id;

#if H5_DO_NATIVE
        cur_oh.cookie = H5Aopen(loc_handle.cookie, attr_name, H5P_DEFAULT);
        HDassert(cur_oh.cookie);
        output.space_id = H5Aget_space(cur_oh.cookie);
        output.type_id = H5Aget_type(cur_oh.cookie);
        output.acpl_id = H5P_ATTRIBUTE_CREATE_DEFAULT;
#else
        /* fake a dataspace, type, and dcpl */
        dims [0] = 60;
        output.space_id = H5Screate_simple(1, dims, NULL);
        output.type_id = H5Tcopy(H5T_NATIVE_INT);
        output.acpl_id = H5P_ATTRIBUTE_CREATE_DEFAULT;
#endif
    }

    output.iod_id = cur_id;
    output.iod_oh = cur_oh;
    output.scratch_id = scratch_pad;
    output.scratch_oh = scratch_oh;

    fprintf(stderr, "Done with attr open, sending response to client\n");
    HG_Handler_start_output(input->hg_handle, &output);

done:
    if(ret_value < 0)
        HG_Handler_start_output(input->hg_handle, &ret_value);

    input = H5MM_xfree(input);
    if(last_comp)
        free(last_comp);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_attr_open_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_attr_read_cb
 *
 * Purpose:	Reads from IOD into the function shipper BDS handle.
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              April, 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_iod_server_attr_read_cb(size_t UNUSED num_necessary_parents, AXE_task_t UNUSED necessary_parents[], 
                             size_t UNUSED num_sufficient_parents, AXE_task_t UNUSED sufficient_parents[], 
                             void *op_data)
{
    H5VL_iod_attr_io_input_t *input = (H5VL_iod_attr_io_input_t *)op_data;
    iod_handle_t iod_oh = input->iod_oh;
    iod_handle_t scratch_oh = input->scratch_oh;
    hg_bulk_t bulk_handle = input->bulk_handle;
    hid_t type_id = input->type_id;
    hg_bulk_block_t bulk_block_handle;
    iod_mem_desc_t mem_desc;
    iod_array_iodesc_t file_desc;
    size_t size;
    void *buf;
    na_addr_t dest = HG_Handler_get_addr(input->hg_handle);
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    size = HG_Bulk_handle_get_size(bulk_handle);
    fprintf(stderr, "Start attribute Read of size %d\n", size);
    if(NULL == (buf = malloc(size)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate read buffer");

    mem_desc.nfrag = 1;
    //mem_desc.frag[0].addr = buf;
    //mem_desc.frag[0].len = (iod_size_t)size;

    /* MSC TODO - populate file location hyperslab */

    /* read from array object */
    if(iod_array_read(iod_oh, IOD_TID_UNKNOWN, NULL, &mem_desc, &file_desc, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_READERROR, FAIL, "can't read from array object");
    
    {
        int i;
        hbool_t flag;
        int *buf_ptr = (int *)buf;

#if H5_DO_NATIVE
    ret_value = H5Aread(iod_oh.cookie, type_id, buf);
#else
    for(i=0;i<60;++i)
        buf_ptr[i] = i;
#endif
    }

    /* Create a new block handle to write the data */
    HG_Bulk_block_handle_create(buf, size, HG_BULK_READ_ONLY, &bulk_block_handle);

    /* Write bulk data here and wait for the data to be there  */
    if(HG_SUCCESS != HG_Bulk_write(bulk_handle, dest, bulk_block_handle))
        HGOTO_ERROR(H5E_SYM, H5E_READERROR, FAIL, "can't read from array object");
    /* wait for it to complete */
    if(HG_SUCCESS != HG_Bulk_wait(bulk_block_handle, HG_BULK_MAX_IDLE_TIME))
        HGOTO_ERROR(H5E_SYM, H5E_READERROR, FAIL, "can't read from array object");

done:
    fprintf(stderr, "Done with attr read, sending response to client\n");

    if(HG_SUCCESS != HG_Handler_start_output(input->hg_handle, &ret_value))
        HDONE_ERROR(H5E_SYM, H5E_WRITEERROR, FAIL, "can't send result of write to client");
    if(HG_SUCCESS != HG_Bulk_block_handle_free(bulk_block_handle))
        HDONE_ERROR(H5E_SYM, H5E_WRITEERROR, FAIL, "can't free bds block handle");

    input = H5MM_xfree(input);
    free(buf);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_attr_read_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_attr_write_cb
 *
 * Purpose:	Writes from IOD into the function shipper BDS handle.
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              April, 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_iod_server_attr_write_cb(size_t UNUSED num_necessary_parents, AXE_task_t UNUSED necessary_parents[], 
                             size_t UNUSED num_sufficient_parents, AXE_task_t UNUSED sufficient_parents[], 
                             void *op_data)
{
    H5VL_iod_attr_io_input_t *input = (H5VL_iod_attr_io_input_t *)op_data;
    iod_handle_t iod_oh = input->iod_oh;
    iod_handle_t scratch_oh = input->scratch_oh;
    hg_bulk_t bulk_handle = input->bulk_handle;
    hid_t type_id = input->type_id;
    hg_bulk_block_t bulk_block_handle;
    iod_mem_desc_t mem_desc;
    iod_array_iodesc_t file_desc;
    size_t size;
    void *buf;
    ssize_t ret;
    na_addr_t source = HG_Handler_get_addr(input->hg_handle);
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    fprintf(stderr, "Start attribute Write\n");

    /* Read bulk data here and wait for the data to be here  */
    size = HG_Bulk_handle_get_size(bulk_handle);
    if(NULL == (buf = malloc(size)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate read buffer");

    HG_Bulk_block_handle_create(buf, size, HG_BULK_READWRITE, &bulk_block_handle);

    /* Write bulk data here and wait for the data to be there  */
    if(HG_SUCCESS != HG_Bulk_read(bulk_handle, source, bulk_block_handle))
        HGOTO_ERROR(H5E_SYM, H5E_WRITEERROR, FAIL, "can't get data from function shipper");
    /* wait for it to complete */
    if(HG_SUCCESS != HG_Bulk_wait(bulk_block_handle, HG_BULK_MAX_IDLE_TIME))
        HGOTO_ERROR(H5E_SYM, H5E_WRITEERROR, FAIL, "can't get data from function shipper");

    /* free the bds block handle */
    if(HG_SUCCESS != HG_Bulk_block_handle_free(bulk_block_handle))
        HGOTO_ERROR(H5E_SYM, H5E_WRITEERROR, FAIL, "can't free bds block handle");

    { 
        int i;
        int *buf_ptr = (int *)buf;

        fprintf(stderr, "Received a buffer of size %d with values: ", size);
        for(i=0;i<60;++i)
            fprintf(stderr, "%d ", buf_ptr[i]);
        fprintf(stderr, "\n");
    }

    mem_desc.nfrag = 1;
    //mem_desc.frag[0].addr = buf;
    //mem_desc.frag[0].len = (iod_size_t)size;

    /* MSC TODO - populate file location hyperslab */

    /* write from array object */
    if(iod_array_write(iod_oh, IOD_TID_UNKNOWN, NULL, &mem_desc, &file_desc, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_WRITEERROR, FAIL, "can't write to array object");

#if H5_DO_NATIVE
    ret_value = H5Awrite(iod_oh.cookie, type_id, buf);
#endif

done:
    fprintf(stderr, "Done with attr write, sending %d response to client\n", ret_value);
    if(HG_SUCCESS != HG_Handler_start_output(input->hg_handle, &ret_value))
        HDONE_ERROR(H5E_SYM, H5E_WRITEERROR, FAIL, "can't send result of write to client");

    input = H5MM_xfree(input);
    free(buf);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_attr_write_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_attr_exists_cb
 *
 * Purpose:	Checks if an attribute exists on object.
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              April, 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_iod_server_attr_exists_cb(size_t UNUSED num_necessary_parents, AXE_task_t UNUSED necessary_parents[], 
                               size_t UNUSED num_sufficient_parents, AXE_task_t UNUSED sufficient_parents[], 
                               void *op_data)
{
    H5VL_iod_attr_op_input_t *input = (H5VL_iod_attr_op_input_t *)op_data;
    iod_handle_t coh = input->coh;
    iod_handle_t loc_handle = input->loc_oh;
    iod_handle_t cur_oh, scratch_oh;
    iod_obj_id_t cur_id, scratch_pad;
    const char *loc_name = input->path;
    const char *attr_name = input->attr_name;
    char *last_comp = NULL;
    htri_t ret = -1;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    fprintf(stderr, "Start attribute Exists %s\n", attr_name);

    /* traverse the path and open the dataset object as we do it */
    if(H5VL_iod_server_get_loc(coh, loc_handle, loc_name, FALSE, FALSE, &last_comp,
                               &cur_id, &cur_oh, &scratch_pad, &scratch_oh) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't traverse path");

    /*MSC - check the attribute in the scratch pad KV store when it is
      there */

#if H5_DO_NATIVE
    ret = H5Aexists(loc_handle.cookie, attr_name);
#else
    ret = FALSE;
#endif

done:
    fprintf(stderr, "Done with attr exists, sending response to client\n");
    HG_Handler_start_output(input->hg_handle, &ret);

    input = H5MM_xfree(input);
    if(last_comp)
        free(last_comp);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_attr_exists_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_attr_remove_cb
 *
 * Purpose:	Removes iod HDF5 attribute.
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              April, 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_iod_server_attr_remove_cb(size_t UNUSED num_necessary_parents, AXE_task_t UNUSED necessary_parents[], 
                               size_t UNUSED num_sufficient_parents, AXE_task_t UNUSED sufficient_parents[], 
                               void *op_data)
{
    H5VL_iod_attr_op_input_t *input = (H5VL_iod_attr_op_input_t *)op_data;
    iod_handle_t coh = input->coh;
    iod_handle_t loc_handle = input->loc_oh;
    iod_handle_t cur_oh, scratch_oh;
    iod_obj_id_t cur_id, scratch_pad;
    const char *loc_name = input->path;
    const char *attr_name = input->attr_name;
    char *last_comp = NULL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    fprintf(stderr, "Start attribute Remove %s\n", attr_name);

    /* traverse the path and open the dataset object as we do it */
    if(H5VL_iod_server_get_loc(coh, loc_handle, loc_name, FALSE, FALSE, &last_comp,
                               &cur_id, &cur_oh, &scratch_pad, &scratch_oh) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't traverse path");

    /*MSC - Remove the attribute from the scratch pad KV store when it
      is there */

#if H5_DO_NATIVE
    ret_value = H5Adelete(loc_handle.cookie, attr_name);
#endif

done:
    fprintf(stderr, "Done with attr remove, sending response to client\n");
    HG_Handler_start_output(input->hg_handle, &ret_value);

    input = H5MM_xfree(input);
    if(last_comp)
        free(last_comp);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_attr_remove_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_attr_close_cb
 *
 * Purpose:	Closes iod HDF5 attribute.
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              April, 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_iod_server_attr_close_cb(size_t UNUSED num_necessary_parents, AXE_task_t UNUSED necessary_parents[], 
                              size_t UNUSED num_sufficient_parents, AXE_task_t UNUSED sufficient_parents[], 
                              void *op_data)
{
    H5VL_iod_remote_attr_t *input = (H5VL_iod_remote_attr_t *)op_data;
    iod_handle_t iod_oh = input->iod_oh;
    iod_handle_t scratch_oh = input->scratch_oh;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    fprintf(stderr, "Start attribute Close\n");

#if H5_DO_NATIVE
    HDassert(H5Aclose(iod_oh.cookie) == SUCCEED);
#endif

    if((ret_value = iod_obj_close(scratch_oh, NULL, NULL)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close scratch object handle");
    if((ret_value = iod_obj_close(iod_oh, NULL, NULL)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close root object handle");

done:
    fprintf(stderr, "Done with attr close, sending response to client\n");
    HG_Handler_start_output(input->hg_handle, &ret_value);

    input = H5MM_xfree(input);
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_attr_close_cb() */

static herr_t 
H5VL_iod_server_get_loc(iod_handle_t coh, iod_handle_t loc_handle, const char *path,
                        hbool_t create_interm_grps, hbool_t break_on_last_comp,
                        char **last_comp, iod_obj_id_t *iod_id, iod_handle_t *iod_oh, 
                        iod_obj_id_t *scratch_id, iod_handle_t *scratch_oh)
{
    char comp_buf[1024];     /* Temporary buffer for path components */
    char *comp;          /* Pointer to buffer for path components */
    H5WB_t *wb = NULL;     /* Wrapped buffer for temporary buffer */
    size_t nchars;	     /* component name length	*/
    iod_handle_t cur_oh, prev_oh;
    iod_obj_id_t cur_id;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    cur_oh = loc_handle;

    /* Wrap the local buffer for serialized header info */
    if(NULL == (wb = H5WB_wrap(comp_buf, sizeof(comp_buf))))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't wrap buffer")
    /* Get a pointer to a buffer that's large enough  */
    if(NULL == (comp = (char *)H5WB_actual(wb, (HDstrlen(path) + 1))))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't get actual buffer")

    /* Traverse the path */
    while((path = H5G__component(path, &nchars)) && *path) {
        const char *s;                  /* Temporary string pointer */
        iod_size_t kv_size;

	/*
	 * Copy the component path into a null-terminated buffer so
	 * we can pass it down to the other symbol table functions.
	 */
	HDmemcpy(comp, path, nchars);
	comp[nchars] = '\0';

	/*
	 * The special path `.' is a no-op.
	 */
	if('.' == comp[0] && !comp[1]) {
	    path += nchars;
	    continue;
	} /* end if */

        /* Check if this is the last component of the path */
        if(!((s = H5G__component(path + nchars, NULL)) && *s)) {
            if(last_comp)
                *last_comp = HDstrdup(comp);
            if(break_on_last_comp)
                break;
        }

        kv_size = sizeof(iod_obj_id_t);

        prev_oh = cur_oh;

        /* lookup next object in the current group */
        if(iod_kv_get_value(cur_oh, IOD_TID_UNKNOWN, comp, &cur_id, 
                            &kv_size, NULL, NULL) < 0) {
            if(create_interm_grps) {
                iod_kv_t kv;

                fprintf(stderr, "creating intermediate group %s\n",comp);

                /* create the current group */
                if (iod_obj_create(coh, IOD_TID_UNKNOWN, NULL/*hints*/, IOD_OBJ_KV, NULL, NULL,
                                   &cur_id, NULL /*event*/) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't create current object handle");

                /* insert new object in kv store of current object */
                kv.key = HDstrdup(comp);
                kv.value = &cur_id;
                kv.value_len = sizeof(iod_obj_id_t);
                if (iod_kv_set(cur_oh, IOD_TID_UNKNOWN, NULL, &kv, NULL, NULL) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't set KV pair in parent");;
                HDfree(kv.key);

                /* create scratch pad for current group */
                if (iod_obj_create(coh, IOD_TID_UNKNOWN, NULL/*hints*/, IOD_OBJ_KV, NULL, NULL,
                                   scratch_id, NULL /*event*/) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't create scratch pad");

                /* Close previous handle unless it is the original one */
                if(loc_handle.cookie != prev_oh.cookie && 
                   iod_obj_close(prev_oh, NULL, NULL) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close current object handle");

                /* open the current group */
                if (iod_obj_open_write(coh, cur_id, NULL /*hints*/, &cur_oh, NULL) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't open current group");

                /* add the scratch pad to the current group */
                if (iod_obj_set_scratch(cur_oh, IOD_TID_UNKNOWN, scratch_id, NULL, NULL) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't set scratch pad");
            }
            else {
                HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "group does not exist");
            }
        } /* end if */
        else {
            /* Close previous handle unless it is the original one */
            if(loc_handle.cookie != prev_oh.cookie && 
               iod_obj_close(prev_oh, NULL, NULL) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close current object handle");

            /* open the current group */
            if (iod_obj_open_write(coh, cur_id, NULL, &cur_oh, NULL) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't open current group");
        }

	/* Advance to next component in string */
	path += nchars;
    } /* end while */

    /* get scratch pad of current location */
    if(iod_obj_get_scratch(cur_oh, IOD_TID_UNKNOWN, scratch_id, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "can't get scratch pad for root object");

    /* open the scratch pad */
    if (iod_obj_open_write(coh, *scratch_id, NULL /*hints*/, scratch_oh, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't open current group");

    /* Release temporary component buffer */
    if(wb && H5WB_unwrap(wb) < 0)
        HDONE_ERROR(H5E_SYM, H5E_CANTRELEASE, FAIL, "can't release wrapped buffer");

    *iod_id = cur_id;
    *iod_oh = cur_oh;

done:
    FUNC_LEAVE_NOAPI(ret_value)
}
#endif /* H5_HAVE_EFF */
