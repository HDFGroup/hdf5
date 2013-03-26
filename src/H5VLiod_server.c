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
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Gpkg.h"		/* Groups		  		*/
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Oprivate.h"         /* Object headers			*/
#include "H5Pprivate.h"		/* Property lists			*/
#include "H5Sprivate.h"		/* Dataspaces				*/
#include "H5Tprivate.h"		/* Datatypes				*/
#include "H5VLprivate.h"	/* VOL plugins				*/
#include "H5VLiod.h"            /* Iod VOL plugin			*/
#include "H5VLiod_common.h"
#include "H5VLiod_server.h"
#include "H5WBprivate.h"        /* Wrapped Buffers                      */

#ifdef H5_HAVE_EFF

/*
 * Programmer:  Mohamad Chaarawi <chaarawi@hdfgroup.gov>
 *              February, 2012
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

herr_t
H5VLiod_start_handler(MPI_Comm comm, MPI_Info UNUSED info)
{
    na_network_class_t *network_class = NULL;
    int num_procs;
    herr_t ret_value = SUCCEED;

    MPI_Comm_size(comm, &num_procs);

    iod_comm = comm;

    /* initialize the netwrok class */
    network_class = na_mpi_init(NULL, MPI_INIT_SERVER);
    if(S_SUCCESS != fs_handler_init(network_class))
        return FAIL;
    if(S_SUCCESS != fs_handler_use_manual_proc())
        return FAIL;
    if(S_SUCCESS != bds_init(network_class))
        return FAIL;

    /* Register function and encoding/decoding functions */
    fs_handler_register("eff_init", 
                        H5VL_iod_server_eff_init,
                        H5VL_iod_server_decode_eff_init, 
                        H5VL_iod_server_encode_eff_init);
    fs_handler_register("eff_finalize", 
                        H5VL_iod_server_eff_finalize,
                        NULL,
                        H5VL_iod_server_encode_eff_init);
    fs_handler_register("file_create", 
                        H5VL_iod_server_file_create,
                        H5VL_iod_server_decode_file_create, 
                        H5VL_iod_server_encode_file_create);
    fs_handler_register("file_open", 
                        H5VL_iod_server_file_open,
                        H5VL_iod_server_decode_file_open, 
                        H5VL_iod_server_encode_file_open);
    fs_handler_register("file_flush", 
                        H5VL_iod_server_file_flush,
                        H5VL_iod_server_decode_file_flush, 
                        H5VL_iod_server_encode_file_flush);
    fs_handler_register("file_close", 
                        H5VL_iod_server_file_close,
                        H5VL_iod_server_decode_file_close, 
                        H5VL_iod_server_encode_file_close);
    fs_handler_register("group_create", 
                        H5VL_iod_server_group_create,
                        H5VL_iod_server_decode_group_create, 
                        H5VL_iod_server_encode_group_create);
    fs_handler_register("group_open", 
                        H5VL_iod_server_group_open,
                        H5VL_iod_server_decode_group_open, 
                        H5VL_iod_server_encode_group_open);
    fs_handler_register("group_close", 
                        H5VL_iod_server_group_close,
                        H5VL_iod_server_decode_group_close, 
                        H5VL_iod_server_encode_group_close);
    fs_handler_register("dset_create", 
                        H5VL_iod_server_dset_create,
                        H5VL_iod_server_decode_dset_create, 
                        H5VL_iod_server_encode_dset_create);
    fs_handler_register("dset_open", 
                        H5VL_iod_server_dset_open,
                        H5VL_iod_server_decode_dset_open, 
                        H5VL_iod_server_encode_dset_open);
    fs_handler_register("dset_read",
                        H5VL_iod_server_dset_read,
                        H5VL_iod_server_decode_dset_io, 
                        H5VL_iod_server_encode_dset_read);
    fs_handler_register("dset_write",
                        H5VL_iod_server_dset_write,
                        H5VL_iod_server_decode_dset_io, 
                        H5VL_iod_server_encode_dset_write);
    fs_handler_register("dset_set_extent",
                        H5VL_iod_server_dset_set_extent,
                        H5VL_iod_server_decode_dset_set_extent, 
                        H5VL_iod_server_encode_dset_set_extent);
    fs_handler_register("dset_close", 
                        H5VL_iod_server_dset_close,
                        H5VL_iod_server_decode_dset_close, 
                        H5VL_iod_server_encode_dset_close);

    /* Loop tp receive requests from clients */
    while(1) {
        fprintf(stderr, "Server In Loop\n");
        /* Receive new function calls */
        if(S_SUCCESS != fs_handler_process(FS_HANDLER_MAX_IDLE_TIME))
            return FAIL;
        if(shutdown)
            break;
    }

    if(S_SUCCESS != bds_finalize())
        return FAIL;
    if(S_SUCCESS != fs_handler_finalize())
        return FAIL;

    return ret_value;
}


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_eff_init
 *
 * Purpose:	Function shipper registered call for initializing the eff stack.
 *              this will initialize the IOD library
 *
 * Return:	Success:	S_SUCCESS 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              January, 2012
 *
 *-------------------------------------------------------------------------
 */
int
H5VL_iod_server_eff_init(fs_handle_t handle)
{
    int num_procs;
    int ret_value = S_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    if(S_FAIL == fs_handler_get_input(handle, &num_procs))
	HGOTO_ERROR(H5E_FILE, H5E_CANTGET, S_FAIL, "can't get input parameters");

    if(iod_initialize(iod_comm, NULL, num_procs, num_procs, NULL) < 0 )
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, S_FAIL, "can't initialize");

    /* MSC - this needs to be changed to be the number of peers connecting to this server */
    num_peers = num_procs;

    if(AXE_SUCCEED != AXEcreate_engine(4, &engine))
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, S_FAIL, "can't start AXE engine");

done:
    fs_handler_complete(handle, &ret_value);
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_eff_init() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_eff_finalize
 *
 * Purpose:	Function to shutdown server
 *
 * Return:	Success:	S_SUCCESS 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              January, 2012
 *
 *-------------------------------------------------------------------------
 */
int
H5VL_iod_server_eff_finalize(fs_handle_t handle)
{
    int ret_value = S_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    terminate_requests ++;

    if(terminate_requests == num_peers) {
        if(iod_finalize(NULL, NULL) < 0 )
            HGOTO_ERROR(H5E_FILE, H5E_CANTDEC, S_FAIL, "can't finalize IOD");
        if(AXE_SUCCEED != AXEterminate_engine(engine, TRUE))
            HGOTO_ERROR(H5E_FILE, H5E_CANTDEC, S_FAIL, "can't shutdown AXE engine");
        shutdown = TRUE;
    }

done:
    fs_handler_complete(handle, &ret_value);
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_eff_finalize() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_file_create
 *
 * Purpose:	Function shipper registered call for File Create.
 *              Inserts the real worker routine into the Async Engine.
 *
 * Return:	Success:	S_SUCCESS 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              January, 2012
 *
 *-------------------------------------------------------------------------
 */
int
H5VL_iod_server_file_create(fs_handle_t handle)
{
    H5VL_iod_file_create_input_t *input = NULL;
    AXE_task_t task;
    int ret_value = S_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (input = (H5VL_iod_file_create_input_t *)
                H5MM_malloc(sizeof(H5VL_iod_file_create_input_t))))
	HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, S_FAIL, "can't allocate input struct for decoding");

    if(S_FAIL == fs_handler_get_input(handle, input))
	HGOTO_ERROR(H5E_FILE, H5E_CANTGET, S_FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, S_FAIL, "AXE engine not started");

    input->fs_handle = handle;
    if (AXE_SUCCEED != AXEcreate_task(engine, &task, 0, NULL, 0, NULL, H5VL_iod_server_file_create_cb, 
                                      input, NULL))
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, S_FAIL, "can't insert task into async engine");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_file_create() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_file_open
 *
 * Purpose:	Function shipper registered call for File Open.
 *              Inserts the real worker routine into the Async Engine.
 *
 * Return:	Success:	S_SUCCESS 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              January, 2012
 *
 *-------------------------------------------------------------------------
 */
int
H5VL_iod_server_file_open(fs_handle_t handle)
{
    H5VL_iod_file_open_input_t *input = NULL;
    AXE_task_t task;
    int ret_value = S_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (input = (H5VL_iod_file_open_input_t *)
                H5MM_malloc(sizeof(H5VL_iod_file_open_input_t))))
	HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, S_FAIL, "can't allocate input struct for decoding");

    if(S_FAIL == fs_handler_get_input(handle, input))
	HGOTO_ERROR(H5E_FILE, H5E_CANTGET, S_FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, S_FAIL, "AXE engine not started");

    input->fs_handle = handle;
    if (AXE_SUCCEED != AXEcreate_task(engine, &task, 0, NULL, 0, NULL, H5VL_iod_server_file_open_cb, 
                                      input, NULL))
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, S_FAIL, "can't insert task into async engine");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_file_open() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_file_flush
 *
 * Purpose:	Function shipper registered call for File Flush.
 *              Inserts the real worker routine into the Async Engine.
 *
 * Return:	Success:	S_SUCCESS 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              January, 2012
 *
 *-------------------------------------------------------------------------
 */
int
H5VL_iod_server_file_flush(fs_handle_t handle)
{
    H5VL_iod_file_flush_input_t *input = NULL;
    AXE_task_t task;
    int ret_value = S_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (input = (H5VL_iod_file_flush_input_t *)
                H5MM_malloc(sizeof(H5VL_iod_file_flush_input_t))))
	HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, S_FAIL, "can't allocate input struct for decoding");

    if(S_FAIL == fs_handler_get_input(handle, input))
	HGOTO_ERROR(H5E_FILE, H5E_CANTGET, S_FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, S_FAIL, "AXE engine not started");

    input->fs_handle = handle;
    if (AXE_SUCCEED != AXEcreate_task(engine, &task, 0, NULL, 0, NULL, H5VL_iod_server_file_flush_cb, 
                                      input, NULL))
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, S_FAIL, "can't insert task into async engine");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_file_flush() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_file_close
 *
 * Purpose:	Function shipper registered call for File Close.
 *              Inserts the real worker routine into the Async Engine.
 *
 * Return:	Success:	S_SUCCESS 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              January, 2012
 *
 *-------------------------------------------------------------------------
 */
int
H5VL_iod_server_file_close(fs_handle_t handle)
{
    H5VL_iod_remote_file_t *input = NULL;
    AXE_task_t task;
    int ret_value = S_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (input = (H5VL_iod_remote_file_t *)
                H5MM_malloc(sizeof(H5VL_iod_remote_file_t))))
	HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, S_FAIL, "can't allocate input struct for decoding");

    if(S_FAIL == fs_handler_get_input(handle, input))
	HGOTO_ERROR(H5E_FILE, H5E_CANTGET, S_FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, S_FAIL, "AXE engine not started");

    input->fs_handle = handle;
    if (AXE_SUCCEED != AXEcreate_task(engine, &task, 0, NULL, 0, NULL, H5VL_iod_server_file_close_cb, 
                                      input, NULL))
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, S_FAIL, "can't insert task into async engine");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_file_close() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_group_create
 *
 * Purpose:	Function shipper registered call for Group Create.
 *              Inserts the real worker routine into the Async Engine.
 *
 * Return:	Success:	S_SUCCESS 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              January, 2012
 *
 *-------------------------------------------------------------------------
 */
int
H5VL_iod_server_group_create(fs_handle_t handle)
{
    H5VL_iod_group_create_input_t *input = NULL;
    AXE_task_t task;
    int ret_value = S_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (input = (H5VL_iod_group_create_input_t *)
                H5MM_malloc(sizeof(H5VL_iod_group_create_input_t))))
	HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, S_FAIL, "can't allocate input struct for decoding");

    if(S_FAIL == fs_handler_get_input(handle, input))
	HGOTO_ERROR(H5E_SYM, H5E_CANTGET, S_FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, S_FAIL, "AXE engine not started");

    input->fs_handle = handle;
    if (AXE_SUCCEED != AXEcreate_task(engine, &task, 0, NULL, 0, NULL, H5VL_iod_server_group_create_cb, 
                                      input, NULL))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, S_FAIL, "can't insert task into async engine");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_group_create() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_group_open
 *
 * Purpose:	Function shipper registered call for Group Open.
 *              Inserts the real worker routine into the Async Engine.
 *
 * Return:	Success:	S_SUCCESS 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              January, 2012
 *
 *-------------------------------------------------------------------------
 */
int
H5VL_iod_server_group_open(fs_handle_t handle)
{
    H5VL_iod_group_open_input_t *input = NULL;
    AXE_task_t task;
    int ret_value = S_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (input = (H5VL_iod_group_open_input_t *)
                H5MM_malloc(sizeof(H5VL_iod_group_open_input_t))))
	HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, S_FAIL, "can't allocate input struct for decoding");

    if(S_FAIL == fs_handler_get_input(handle, input))
	HGOTO_ERROR(H5E_SYM, H5E_CANTGET, S_FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, S_FAIL, "AXE engine not started");

    input->fs_handle = handle;
    if (AXE_SUCCEED != AXEcreate_task(engine, &task, 0, NULL, 0, NULL, H5VL_iod_server_group_open_cb, 
                                      input, NULL))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, S_FAIL, "can't insert task into async engine");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_group_open() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_group_close
 *
 * Purpose:	Function shipper registered call for Group Close.
 *              Inserts the real worker routine into the Async Engine.
 *
 * Return:	Success:	S_SUCCESS 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              January, 2012
 *
 *-------------------------------------------------------------------------
 */
int
H5VL_iod_server_group_close(fs_handle_t handle)
{
    H5VL_iod_remote_group_t *input = NULL;
    AXE_task_t task;
    int ret_value = S_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (input = (H5VL_iod_remote_group_t *)
                H5MM_malloc(sizeof(H5VL_iod_remote_group_t))))
	HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, S_FAIL, "can't allocate input struct for decoding");

    if(S_FAIL == fs_handler_get_input(handle, input))
	HGOTO_ERROR(H5E_SYM, H5E_CANTGET, S_FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, S_FAIL, "AXE engine not started");

    input->fs_handle = handle;
    if (AXE_SUCCEED != AXEcreate_task(engine, &task, 0, NULL, 0, NULL, H5VL_iod_server_group_close_cb, 
                                      input, NULL))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, S_FAIL, "can't insert task into async engine");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_group_close() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_dset_create
 *
 * Purpose:	Function shipper registered call for Dset Create.
 *              Inserts the real worker routine into the Async Engine.
 *
 * Return:	Success:	S_SUCCESS 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              January, 2012
 *
 *-------------------------------------------------------------------------
 */
int
H5VL_iod_server_dset_create(fs_handle_t handle)
{
    H5VL_iod_dset_create_input_t *input = NULL;
    AXE_task_t task;
    int ret_value = S_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (input = (H5VL_iod_dset_create_input_t *)
                H5MM_malloc(sizeof(H5VL_iod_dset_create_input_t))))
	HGOTO_ERROR(H5E_DATASET, H5E_NOSPACE, S_FAIL, "can't allocate input struct for decoding");

    if(S_FAIL == fs_handler_get_input(handle, input))
	HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, S_FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, S_FAIL, "AXE engine not started");

    input->fs_handle = handle;
    if (AXE_SUCCEED != AXEcreate_task(engine, &task, 0, NULL, 0, NULL, H5VL_iod_server_dset_create_cb, 
                                      input, NULL))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, S_FAIL, "can't insert task into async engine");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_dset_create() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_dset_open
 *
 * Purpose:	Function shipper registered call for Dset Open.
 *              Inserts the real worker routine into the Async Engine.
 *
 * Return:	Success:	S_SUCCESS 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              January, 2012
 *
 *-------------------------------------------------------------------------
 */
int
H5VL_iod_server_dset_open(fs_handle_t handle)
{
    H5VL_iod_dset_open_input_t *input = NULL;
    AXE_task_t task;
    int ret_value = S_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (input = (H5VL_iod_dset_open_input_t *)
                H5MM_malloc(sizeof(H5VL_iod_dset_open_input_t))))
	HGOTO_ERROR(H5E_DATASET, H5E_NOSPACE, S_FAIL, "can't allocate input struct for decoding");

    if(S_FAIL == fs_handler_get_input(handle, input))
	HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, S_FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, S_FAIL, "AXE engine not started");

    input->fs_handle = handle;
    if (AXE_SUCCEED != AXEcreate_task(engine, &task, 0, NULL, 0, NULL, H5VL_iod_server_dset_open_cb, 
                                      input, NULL))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, S_FAIL, "can't insert task into async engine");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_dset_open() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_dset_read
 *
 * Purpose:	Function shipper registered call for Dset Read.
 *              Inserts the real worker routine into the Async Engine.
 *
 * Return:	Success:	S_SUCCESS 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              January, 2012
 *
 *-------------------------------------------------------------------------
 */
int
H5VL_iod_server_dset_read(fs_handle_t handle)
{
    H5VL_iod_dset_io_input_t *input = NULL;
    AXE_task_t task;
    int ret_value = S_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (input = (H5VL_iod_dset_io_input_t *)
                H5MM_malloc(sizeof(H5VL_iod_dset_io_input_t))))
	HGOTO_ERROR(H5E_DATASET, H5E_NOSPACE, S_FAIL, "can't allocate input struct for decoding");

    if(S_FAIL == fs_handler_get_input(handle, input))
	HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, S_FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, S_FAIL, "AXE engine not started");

    input->fs_handle = handle;
    if (AXE_SUCCEED != AXEcreate_task(engine, &task, 0, NULL, 0, NULL, H5VL_iod_server_dset_read_cb, 
                                      input, NULL))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, S_FAIL, "can't insert task into async engine");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_dset_read() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_dset_write
 *
 * Purpose:	Function shipper registered call for Dset Write.
 *              Inserts the real worker routine into the Async Engine.
 *
 * Return:	Success:	S_SUCCESS 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              January, 2012
 *
 *-------------------------------------------------------------------------
 */
int
H5VL_iod_server_dset_write(fs_handle_t handle)
{
    H5VL_iod_dset_io_input_t *input = NULL;
    AXE_task_t task;
    int ret_value = S_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (input = (H5VL_iod_dset_io_input_t *)
                H5MM_malloc(sizeof(H5VL_iod_dset_io_input_t))))
	HGOTO_ERROR(H5E_DATASET, H5E_NOSPACE, S_FAIL, "can't allocate input struct for decoding");

    if(S_FAIL == fs_handler_get_input(handle, input))
	HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, S_FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, S_FAIL, "AXE engine not started");

    input->fs_handle = handle;
    if (AXE_SUCCEED != AXEcreate_task(engine, &task, 0, NULL, 0, NULL, H5VL_iod_server_dset_write_cb, 
                                      input, NULL))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, S_FAIL, "can't insert task into async engine");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_dset_write() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_dset_set_extent
 *
 * Purpose:	Function shipper registered call for Dset Set_Extent.
 *              Inserts the real worker routine into the Async Engine.
 *
 * Return:	Success:	S_SUCCESS 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              January, 2012
 *
 *-------------------------------------------------------------------------
 */
int
H5VL_iod_server_dset_set_extent(fs_handle_t handle)
{
    H5VL_iod_dset_set_extent_input_t *input = NULL;
    AXE_task_t task;
    int ret_value = S_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (input = (H5VL_iod_dset_set_extent_input_t *)
                H5MM_malloc(sizeof(H5VL_iod_dset_set_extent_input_t))))
	HGOTO_ERROR(H5E_DATASET, H5E_NOSPACE, S_FAIL, "can't allocate input struct for decoding");

    if(S_FAIL == fs_handler_get_input(handle, input))
	HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, S_FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, S_FAIL, "AXE engine not started");

    input->fs_handle = handle;

    if (AXE_SUCCEED != AXEcreate_task(engine, &task, 0, NULL, 0, NULL, 
                                      H5VL_iod_server_dset_set_extent_cb, input, NULL))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, S_FAIL, "can't insert task into async engine");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_dset_set_extent() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_dset_close
 *
 * Purpose:	Function shipper registered call for Dset Close.
 *              Inserts the real worker routine into the Async Engine.
 *
 * Return:	Success:	S_SUCCESS 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              January, 2012
 *
 *-------------------------------------------------------------------------
 */
int
H5VL_iod_server_dset_close(fs_handle_t handle)
{
    H5VL_iod_remote_dset_t *input = NULL;
    AXE_task_t task;
    int ret_value = S_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (input = (H5VL_iod_remote_dset_t *)
                H5MM_malloc(sizeof(H5VL_iod_remote_dset_t))))
	HGOTO_ERROR(H5E_DATASET, H5E_NOSPACE, S_FAIL, "can't allocate input struct for decoding");

    if(S_FAIL == fs_handler_get_input(handle, input))
	HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, S_FAIL, "can't get input parameters");

    if(NULL == engine)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, S_FAIL, "AXE engine not started");

    input->fs_handle = handle;
    if (AXE_SUCCEED != AXEcreate_task(engine, &task, 0, NULL, 0, NULL, H5VL_iod_server_dset_close_cb, 
                                      input, NULL))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, S_FAIL, "can't insert task into async engine");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_dset_close() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_file_create_cb
 *
 * Purpose:	Creates a file as a iod HDF5 file.
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              January, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_iod_server_file_create_cb(size_t UNUSED num_necessary_parents, AXE_task_t UNUSED necessary_parents[], 
                               size_t UNUSED num_sufficient_parents, AXE_task_t UNUSED sufficient_parents[], 
                               void *op_data)
{
    H5VL_iod_file_create_input_t *input = (H5VL_iod_file_create_input_t *)op_data;
    H5VL_iod_server_remote_file_t output;
    unsigned int mode;
    iod_handle_t coh;
    iod_handle_t root_handle, scratch_handle;
    iod_obj_id_t root_id, scratch_pad;
    iod_ret_t status;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    fprintf(stderr, "Start file create\n");

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
        if (iod_obj_open_write(coh, scratch_pad, NULL /*hints*/, &scratch_handle, NULL) < 0)
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
        if (iod_obj_open_write(coh, scratch_pad, NULL /*hints*/, &scratch_handle, NULL) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "can't open scratch pad");
    }
    else
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "can't create file");

    output.coh = coh;
    output.root_id = root_id;
    output.root_oh = root_handle;
    output.scratch_id = scratch_pad;
    output.scratch_oh = scratch_handle;

    fprintf(stderr, "Done with file create, sending response to client\n");
    fs_handler_complete(input->fs_handle, &output);

done:
    if(ret_value < 0)
        fs_handler_complete(input->fs_handle, &ret_value);

    if(H5P_FILE_CREATE_DEFAULT != input->fcpl_id)
        H5Pclose(input->fcpl_id);
    if(H5P_FILE_ACCESS_DEFAULT != input->fapl_id)
        H5Pclose(input->fapl_id);
    H5MM_free(input->name);
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
 *              January, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_iod_server_file_open_cb(size_t UNUSED num_necessary_parents, AXE_task_t UNUSED necessary_parents[], 
                             size_t UNUSED num_sufficient_parents, AXE_task_t UNUSED sufficient_parents[], 
                             void *op_data)
{
    H5VL_iod_file_open_input_t *input = (H5VL_iod_file_open_input_t *)op_data;
    H5VL_iod_server_remote_file_t output;
    unsigned int mode;
    iod_handle_t coh;
    iod_handle_t root_handle, scratch_handle;
    iod_obj_id_t scratch_pad;
    iod_ret_t status;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    fprintf(stderr, "Start file open\n");

    if((status = iod_container_open(input->name, NULL /*hints*/, mode, &coh, NULL /*event*/)) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "can't open file");

    /* open the root group */
    if (iod_obj_open_write(coh, ROOT_ID, NULL /*hints*/, &root_handle, NULL) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "can't open root object");

    /* get scratch pad of root group */
    if(iod_obj_get_scratch(root_handle, IOD_TID_UNKNOWN, &scratch_pad, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "can't get scratch pad for root object");

    /* open the scratch pad */
    if (iod_obj_open_write(coh, scratch_pad, NULL /*hints*/, &scratch_handle, NULL) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "can't open scratch pad");


    output.coh = coh;
    output.root_id = ROOT_ID;
    output.root_oh = root_handle;
    output.scratch_id = scratch_pad;
    output.scratch_oh = scratch_handle;
    output.fcpl_size = 0;

    fprintf(stderr, "Done with file open, sending response to client\n");
    fs_handler_complete(input->fs_handle, &output);

done:
    if(ret_value < 0)
        fs_handler_complete(input->fs_handle, &ret_value);

    if(H5P_FILE_ACCESS_DEFAULT != input->fapl_id)
        H5Pclose(input->fapl_id);
    H5MM_free(input->name);

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
 *              January, 2012
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

done:
    fprintf(stderr, "Done with file flush, sending response %d to client\n", ret_value);
    if(S_SUCCESS != fs_handler_complete(input->fs_handle, &ret_value))
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
 *              January, 2012
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

    if(iod_obj_close(scratch_oh, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "can't close scratch object handle");
    if(iod_obj_close(root_oh, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "can't close root object handle");
    if(iod_container_close(coh, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTDEC, FAIL, "can't close container");

done:
    fprintf(stderr, "Done with file close, sending response %d to client\n", ret_value);
    if(S_SUCCESS != fs_handler_complete(input->fs_handle, &ret_value))
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
    H5VL_iod_server_remote_group_t output;
    iod_handle_t coh = input->coh;
    iod_handle_t loc_handle = input->loc_oh;
    iod_handle_t cur_oh, scratch_handle;
    iod_obj_id_t cur_id, scratch_pad;
    const char *name = input->name;
    char comp_buf[1024];     /* Temporary buffer for path components */
    char *comp;          /* Pointer to buffer for path components */
    H5WB_t *wb = NULL;     /* Wrapped buffer for temporary buffer */
    size_t nchars;	     /* component name length	*/
    hbool_t last_comp = FALSE; /* Flag to indicate that a component is the last component in the name */
    hbool_t close_handle = FALSE; /* Flag to indicate whether to close the current handle */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    fprintf(stderr, "Start group create %s\n", name);

    cur_oh = loc_handle;

    /* Wrap the local buffer for serialized header info */
    if(NULL == (wb = H5WB_wrap(comp_buf, sizeof(comp_buf))))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't wrap buffer")
    /* Get a pointer to a buffer that's large enough  */
    if(NULL == (comp = (char *)H5WB_actual(wb, (HDstrlen(name) + 1))))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't get actual buffer")

    /* Traverse the path */
    while((name = H5G__component(name, &nchars)) && *name) {
        const char *s;                  /* Temporary string pointer */
        iod_size_t kv_size;

	/*
	 * Copy the component name into a null-terminated buffer so
	 * we can pass it down to the other symbol table functions.
	 */
	HDmemcpy(comp, name, nchars);
	comp[nchars] = '\0';

	/*
	 * The special name `.' is a no-op.
	 */
	if('.' == comp[0] && !comp[1]) {
	    name += nchars;
	    continue;
	} /* end if */

        /* Check if this is the last component of the name */
        if(!((s = H5G__component(name + nchars, NULL)) && *s))
            last_comp = TRUE;

        kv_size = sizeof(iod_obj_id_t);
        /* lookup next object in the current group */
        /* MSC - if else need to be flipped when we have a real IOD */
        if(iod_kv_get_value(cur_oh, IOD_TID_UNKNOWN, comp, &cur_id, 
                            &kv_size, NULL, NULL) >= 0) {
            iod_kv_t kv;

            fprintf(stderr, "creating group %s\n",comp);
            /* we don't want to close the handle for the group we start on */
            if(close_handle)
                if(iod_obj_close(cur_oh, NULL, NULL) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close current object handle");

            /* create the current group */
            if (iod_obj_create(coh, IOD_TID_UNKNOWN, NULL/*hints*/, IOD_OBJ_KV, NULL, NULL,
                               &cur_id, NULL /*event*/) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't create current object handle");

            kv.key = HDstrdup(comp);
            kv.value = &cur_id;
            kv.value_len = sizeof(iod_obj_id_t);

            /* insert new object in kv store of current object */
            if (iod_kv_set(cur_oh, IOD_TID_UNKNOWN, NULL, &kv, NULL, NULL) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't set KV pair in parent");;

            HDfree(kv.key);

            /* create scratch pad for current group */
            if (iod_obj_create(coh, IOD_TID_UNKNOWN, NULL/*hints*/, IOD_OBJ_KV, NULL, NULL,
                               &scratch_pad, NULL /*event*/) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't create scratch pad");

            /* open the current group */
            if (iod_obj_open_write(coh, cur_id, NULL /*hints*/, &cur_oh, NULL) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't open current group");

            /* add the scratch pad to the current group */
            if (iod_obj_set_scratch(cur_oh, IOD_TID_UNKNOWN, &scratch_pad, NULL, NULL) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't set scratch pad");

            if(last_comp) {
                /* open the scratch pad */
                if (iod_obj_open_write(coh, scratch_pad, NULL /*hints*/, &scratch_handle, NULL) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't open current group");
                break;
            }
        } /* end if */
        else {
            /* if this is the last component, then the group create should fail since object
               exists */
            if (last_comp)
                HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Can't create group, already exists");

            /* open the current group */
            if (iod_obj_open_write(coh, cur_id, NULL, &cur_oh, NULL) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't open current group");
        } /* end else */
        close_handle = TRUE;
	/* Advance to next component in string */
	name += nchars;
    } /* end while */

    /* Release temporary component buffer */
    if(wb && H5WB_unwrap(wb) < 0)
        HDONE_ERROR(H5E_SYM, H5E_CANTRELEASE, FAIL, "can't release wrapped buffer");

    output.iod_id = cur_id;
    output.iod_oh = cur_oh;
    output.scratch_id = scratch_pad;
    output.scratch_oh = scratch_handle;

    fprintf(stderr, "Done with group create, sending response to client\n");
    fs_handler_complete(input->fs_handle, &output);

done:
    if(ret_value < 0)
        fs_handler_complete(input->fs_handle, &ret_value);

    if(H5P_GROUP_CREATE_DEFAULT != input->gcpl_id)
        H5Pclose(input->gcpl_id);
    if(H5P_GROUP_ACCESS_DEFAULT != input->gapl_id)
        H5Pclose(input->gapl_id);
    if(H5P_LINK_CREATE_DEFAULT != input->lcpl_id)
        H5Pclose(input->lcpl_id);
    H5MM_free(input->name);
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
    H5VL_iod_server_remote_group_t output;
    iod_handle_t coh = input->coh;
    iod_handle_t loc_handle = input->loc_oh;
    iod_handle_t cur_oh, scratch_handle;
    iod_obj_id_t cur_id, scratch_pad;
    const char *name = input->name;
    char comp_buf[1024];     /* Temporary buffer for path components */
    char *comp;          /* Pointer to buffer for path components */
    H5WB_t *wb = NULL;     /* Wrapped buffer for temporary buffer */
    hbool_t last_comp = FALSE; /* Flag to indicate that a component is the last component in the name */
    hbool_t close_handle = FALSE; /* Flag to indicate whether to close the current handle */
    size_t nchars;	     /* component name length	*/
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    fprintf(stderr, "Start group open %s\n", name);
    cur_oh = loc_handle;

    /* Wrap the local buffer for serialized header info */
    if(NULL == (wb = H5WB_wrap(comp_buf, sizeof(comp_buf))))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't wrap buffer")
    /* Get a pointer to a buffer that's large enough  */
    if(NULL == (comp = (char *)H5WB_actual(wb, (HDstrlen(name) + 1))))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't get actual buffer")

    /* Traverse the path */
    while((name = H5G__component(name, &nchars)) && *name) {
        iod_size_t kv_size;
        const char *s;                  /* Temporary string pointer */

	/*
	 * Copy the component name into a null-terminated buffer so
	 * we can pass it down to the other symbol table functions.
	 */
	HDmemcpy(comp, name, nchars);
	comp[nchars] = '\0';

	/*
	 * The special name `.' is a no-op.
	 */
	if('.' == comp[0] && !comp[1]) {
	    name += nchars;
	    continue;
	} /* end if */

        /* Check if this is the last component of the name */
        if(!((s = H5G__component(name + nchars, NULL)) && *s))
            last_comp = TRUE;

        kv_size = sizeof(iod_obj_id_t);
        /* lookup next object in the current group */
        if(iod_kv_get_value(cur_oh, IOD_TID_UNKNOWN, comp, &cur_id, 
                            &kv_size, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "object lookup failed");

        /* open the current group */
        if (iod_obj_open_write(coh, cur_id, NULL /*hints*/, &cur_oh, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't open current group");

        if(last_comp)
                break;
        close_handle = TRUE;
    }

    /* get scratch pad of current group */
    if(iod_obj_get_scratch(cur_oh, IOD_TID_UNKNOWN, &scratch_pad, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "can't get scratch pad for root object");

    /* open the scratch pad */
    if (iod_obj_open_write(coh, scratch_pad, NULL /*hints*/, &scratch_handle, NULL) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "can't open scratch pad");

    /* MSC - need to store the gcpl in create */
    output.gcpl_size = 0;
    output.gcpl = NULL;
#if 0 
    if(iod_kv_get_value(scratch_handle, IOD_TID_UNKNOWN, "dataset_gcpl", NULL, 
                        &output.gcpl_size, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "dataset gcpl lookup failed");
    if(NULL == (output.gcpl = H5MM_malloc (output.gcpl_size)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate gcpl buffer");
    if(iod_kv_get_value(scratch_handle, IOD_TID_UNKNOWN, "dataset_gcpl", output.gcpl, 
                        &output.gcpl_size, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "dataset dcpl lookup failed");
#endif

    /* Release temporary component buffer */
    if(wb && H5WB_unwrap(wb) < 0)
        HDONE_ERROR(H5E_SYM, H5E_CANTRELEASE, FAIL, "can't release wrapped buffer")

    output.iod_id = cur_id;
    output.iod_oh = cur_oh;
    output.scratch_id = scratch_pad;
    output.scratch_oh = scratch_handle;

    fprintf(stderr, "Done with group open, sending response to client\n");
    fs_handler_complete(input->fs_handle, &output);

done:
    if(ret_value < 0)
        fs_handler_complete(input->fs_handle, &ret_value);


    H5MM_xfree(output.gcpl);
    if(H5P_GROUP_ACCESS_DEFAULT != input->gapl_id)
        H5Pclose(input->gapl_id);
    H5MM_free(input->name);
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
 *              January, 2012
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

    if((ret_value = iod_obj_close(scratch_oh, NULL, NULL)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close scratch object handle");
    if((ret_value = iod_obj_close(iod_oh, NULL, NULL)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close root object handle");

done:
    fprintf(stderr, "Done with group close, sending response to client\n");
    fs_handler_complete(input->fs_handle, &ret_value);

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
    H5VL_iod_server_remote_dset_t output;
    iod_handle_t coh = input->coh;
    iod_handle_t loc_handle = input->loc_oh;
    iod_handle_t cur_oh, scratch_handle;
    iod_obj_id_t cur_id, scratch_pad;
    const char *name = input->name;
    char comp_buf[1024];     /* Temporary buffer for path components */
    char *comp;          /* Pointer to buffer for path components */
    H5WB_t *wb = NULL;     /* Wrapped buffer for temporary buffer */
    hbool_t last_comp = FALSE; /* Flag to indicate that a component is the last component in the name */
    hbool_t close_handle = FALSE; /* Flag to indicate whether to close the current handle */
    iod_kv_t kv;
    size_t nchars;           /* component name length   */
    iod_array_struct_t array;
    iod_size_t *max_dims;
    size_t buf_size;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    fprintf(stderr, "Start dataset Create %s\n", name);
    cur_oh = loc_handle;

    /* Wrap the local buffer for serialized header info */
    if(NULL == (wb = H5WB_wrap(comp_buf, sizeof(comp_buf))))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't wrap buffer")
    /* Get a pointer to a buffer that's large enough  */
    if(NULL == (comp = (char *)H5WB_actual(wb, (HDstrlen(name) + 1))))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't get actual buffer")

    /* Traverse the path */
    while((name = H5G__component(name, &nchars)) && *name) {
        const char *s;                  /* Temporary string pointer */
        iod_size_t kv_size;

	/*
	 * Copy the component name into a null-terminated buffer so
	 * we can pass it down to the other symbol table functions.
	 */
	HDmemcpy(comp, name, nchars);
	comp[nchars] = '\0';

	/*
	 * The special name `.' is a no-op.
	 */
	if('.' == comp[0] && !comp[1]) {
	    name += nchars;
	    continue;
	} /* end if */

        /* Check if this is the last component of the name */
        if(!((s = H5G__component(name + nchars, NULL)) && *s)) {
            last_comp = TRUE;
            break;
        }

        kv_size = sizeof(iod_obj_id_t);
        /* lookup next object in the current group */
        /* MSC - if else need to be flipped when we have a real IOD */
        if(iod_kv_get_value(cur_oh, IOD_TID_UNKNOWN, comp, &cur_id, 
                            &kv_size, NULL, NULL) >= 0) {
            fprintf(stderr, "creating intermediate group %s\n",comp);
            /* we don't want to close the handle for the group we start on */
            if(close_handle)
                if(iod_obj_close(cur_oh, NULL, NULL) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close current object handle");

            /* create the current group */
            if (iod_obj_create(coh, IOD_TID_UNKNOWN, NULL/*hints*/, IOD_OBJ_KV, NULL, NULL,
                                  &cur_id, NULL /*event*/) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't create current object handle");

            kv.key = HDstrdup(comp);
            kv.value = &cur_id;
            kv.value_len = sizeof(iod_obj_id_t);

            /* insert new object in kv store of current object */
            if (iod_kv_set(cur_oh, IOD_TID_UNKNOWN, NULL, &kv, NULL, NULL) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't set KV pair in parent");;

            /* create scratch pad for current group */
            if (iod_obj_create(coh, IOD_TID_UNKNOWN, NULL/*hints*/, IOD_OBJ_KV, NULL, NULL,
                                  &scratch_pad, NULL /*event*/) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't create scratch pad");

            /* open the current group */
            if (iod_obj_open_write(coh, cur_id, NULL /*hints*/, &cur_oh, NULL) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't open current group");

            /* add the scratch pad to the current group */
            if (iod_obj_set_scratch(cur_oh, IOD_TID_UNKNOWN, &scratch_pad, NULL, NULL) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't set scratch pad");
        } /* end if */
        else {
            /* open the current group */
            if (iod_obj_open_write(coh, cur_id, NULL, &cur_oh, NULL) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't open current group");
        } /* end else */
        close_handle = TRUE;
	/* Advance to next component in string */
	name += nchars;
    } /* end while */

    printf("HERE1\n");
    array.cell_size = H5Tget_size(input->type_id);
    printf("HERE1\n");
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
            comp, array.cell_size, array.num_dims);

    /* create the dataset */
    if (iod_obj_create(coh, IOD_TID_UNKNOWN, NULL/*hints*/, IOD_OBJ_ARRAY, NULL, &array,
                       &cur_id, NULL /*event*/) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't create current object handle");

    kv.key = HDstrdup(comp);
    kv.value = &cur_id;
    kv.value_len = sizeof(iod_obj_id_t);

    /* insert new dataset in kv store of current group */
    if (iod_kv_set(cur_oh, IOD_TID_UNKNOWN, NULL, &kv, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't set KV pair in parent");

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
    if (iod_obj_open_write(coh, scratch_pad, NULL /*hints*/, &scratch_handle, NULL) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "can't open scratch pad");

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
    if (iod_kv_set(scratch_handle, IOD_TID_UNKNOWN, NULL, &kv, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't set KV pair in parent");
    HDfree(kv.key);
    free(kv.value);
    if(H5P_DATASET_CREATE_DEFAULT != input->dcpl_id)
        H5Pclose(input->dcpl_id);

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
    if (iod_kv_set(scratch_handle, IOD_TID_UNKNOWN, NULL, &kv, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't set KV pair in parent");
    HDfree(kv.key);
    free(kv.value);
    H5Tclose(input->type_id);

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
    if (iod_kv_set(scratch_handle, IOD_TID_UNKNOWN, NULL, &kv, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't set KV pair in parent");
    HDfree(kv.key);
    free(kv.value);
    H5Sclose(input->space_id);

    /* Release temporary component buffer */
    if(wb && H5WB_unwrap(wb) < 0)
        HDONE_ERROR(H5E_SYM, H5E_CANTRELEASE, FAIL, "can't release wrapped buffer")

    output.iod_id = cur_id;
    output.iod_oh = cur_oh;
    output.scratch_id = scratch_pad;
    output.scratch_oh = scratch_handle;

    free(max_dims);
    free(array.current_dims);

    fprintf(stderr, "Done with dset create, sending response to client\n");
    fs_handler_complete(input->fs_handle, &output);

done:
    if(ret_value < 0)
        fs_handler_complete(input->fs_handle, &ret_value);

    if(H5P_DATASET_ACCESS_DEFAULT != input->dapl_id)
        H5Pclose(input->dapl_id);
    if(H5P_LINK_CREATE_DEFAULT != input->lcpl_id)
        H5Pclose(input->lcpl_id);
    H5MM_free(input->name);

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
    H5VL_iod_server_remote_dset_t output;
    iod_handle_t coh = input->coh;
    iod_handle_t loc_handle = input->loc_oh;
    iod_handle_t cur_oh, scratch_handle;
    iod_obj_id_t cur_id, scratch_pad;
    char *name = input->name;
    char comp_buf[1024];     /* Temporary buffer for path components */
    char *comp;          /* Pointer to buffer for path components */
    H5WB_t *wb = NULL;     /* Wrapped buffer for temporary buffer */
    hbool_t last_comp = FALSE; /* Flag to indicate that a component is the last component in the name */
    hbool_t close_handle = FALSE; /* Flag to indicate whether to close the current handle */
    size_t nchars;           /* component name length   */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    fprintf(stderr, "Start dataset Open %s\n", name);
    cur_oh = loc_handle;

    /* Wrap the local buffer for serialized header info */
    if(NULL == (wb = H5WB_wrap(comp_buf, sizeof(comp_buf))))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't wrap buffer")
    /* Get a pointer to a buffer that's large enough  */
    if(NULL == (comp = (char *)H5WB_actual(wb, (HDstrlen(name) + 1))))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't get actual buffer")

    /* Traverse the path */
    while((name = H5G__component(name, &nchars)) && *name) {
        const char *s;                  /* Temporary string pointer */
        iod_size_t kv_size;

	/*
	 * Copy the component name into a null-terminated buffer so
	 * we can pass it down to the other symbol table functions.
	 */
	HDmemcpy(comp, name, nchars);
	comp[nchars] = '\0';

	/*
	 * The special name `.' is a no-op.
	 */
	if('.' == comp[0] && !comp[1]) {
	    name += nchars;
	    continue;
	} /* end if */

        /* Check if this is the last component of the name */
        if(!((s = H5G__component(name + nchars, NULL)) && *s))
            last_comp = TRUE;

        kv_size = sizeof(iod_obj_id_t);
        /* lookup next object in the current group */
        if(iod_kv_get_value(cur_oh, IOD_TID_UNKNOWN, comp, &cur_id, 
                            &kv_size, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "object lookup failed");

        /* open the current group */
        if (iod_obj_open_write(coh, cur_id, NULL /*hints*/, &cur_oh, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't open current group");

        close_handle = TRUE;
	/* Advance to next component in string */
	name += nchars;
    }

    /* get scratch pad of dataset */
    if(iod_obj_get_scratch(cur_oh, IOD_TID_UNKNOWN, &scratch_pad, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "can't get scratch pad for root object");

    /* open the scratch pad */
    if (iod_obj_open_write(coh, scratch_pad, NULL /*hints*/, &scratch_handle, NULL) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "can't open scratch pad");

#if 0
    /*retrieve all metadata from scratch pad */
    output.dcpl_size = 0;
    if(iod_kv_get_value(scratch_handle, IOD_TID_UNKNOWN, "dataset_dcpl", NULL, 
                        &output.dcpl_size, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "dataset dcpl lookup failed");
    if(NULL == (output.dcpl = H5MM_malloc (output.dcpl_size)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate dcpl buffer");
    if(iod_kv_get_value(scratch_handle, IOD_TID_UNKNOWN, "dataset_dcpl", output.dcpl, 
                        &output.dcpl_size, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "dataset dcpl lookup failed");

    output.dtype_size = 0;
    if(iod_kv_get_value(scratch_handle, IOD_TID_UNKNOWN, "dataset_dtype", NULL, 
                        &output.dtype_size, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "dataset dtype lookup failed");
    if(NULL == (output.dtype = H5MM_malloc (output.dtype_size)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate dtype buffer");
    if(iod_kv_get_value(scratch_handle, IOD_TID_UNKNOWN, "dataset_dtype", output.dtype, 
                        &output.dtype_size, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "dataset dtype lookup failed");

    output.dspace_size = 0;
    if(iod_kv_get_value(scratch_handle, IOD_TID_UNKNOWN, "dataset_dspace", NULL, 
                        &output.dspace_size, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "dataset dspace lookup failed");
    if(NULL == (output.dspace = H5MM_malloc (output.dspace_size)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate dspace buffer");
    if(iod_kv_get_value(scratch_handle, IOD_TID_UNKNOWN, "dataset_dspace", output.dspace, 
                        &output.dspace_size, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "dataset dspace lookup failed");
#endif
#if 1
    /* fake a dataspace, type, and dcpl */
    {
        hsize_t dims[1];
        hid_t space_id, type_id;

        dims [0] = 60;
        space_id = H5Screate_simple(1, dims, NULL);
        type_id = H5T_NATIVE_INT;

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
    }
#endif

    /* Release temporary component buffer */
    if(wb && H5WB_unwrap(wb) < 0)
        HDONE_ERROR(H5E_SYM, H5E_CANTRELEASE, FAIL, "can't release wrapped buffer");

    output.iod_id = cur_id;
    output.iod_oh = cur_oh;
    output.scratch_id = scratch_pad;
    output.scratch_oh = scratch_handle;

    fprintf(stderr, "Done with dset open, sending response to client\n");
    fs_handler_complete(input->fs_handle, &output);

done:
    if(ret_value < 0)
        fs_handler_complete(input->fs_handle, &ret_value);

    H5MM_xfree(output.dcpl);
    H5MM_xfree(output.dtype);
    H5MM_xfree(output.dspace);

    if(H5P_DATASET_ACCESS_DEFAULT != input->dapl_id)
        H5Pclose(input->dapl_id);
    H5MM_free(input->name);

    input = H5MM_xfree(input);
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
 *              January, 2012
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
    bds_handle_t bds_handle = input->bds_handle;
    hid_t space_id = input->space_id;
    hid_t dxpl_id = input->dxpl_id;
    bds_block_handle_t bds_block_handle;
    iod_mem_desc_t mem_desc;
    iod_array_iodesc_t file_desc;
    size_t size;
    void *buf;
    uint32_t cs = 0;
    na_addr_t dest = fs_handler_get_addr(input->fs_handle);
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    size = bds_handle_get_size(bds_handle);
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

        for(i=0;i<60;++i)
            buf_ptr[i] = i;
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
    bds_block_handle_create(buf, size, BDS_READ_ONLY, &bds_block_handle);

    /* Write bulk data here and wait for the data to be there  */
    if(S_SUCCESS != bds_write(bds_handle, dest, bds_block_handle))
        HGOTO_ERROR(H5E_SYM, H5E_READERROR, FAIL, "can't read from array object");
    /* wait for it to complete */
    if(S_SUCCESS != bds_wait(bds_block_handle, BDS_MAX_IDLE_TIME))
        HGOTO_ERROR(H5E_SYM, H5E_READERROR, FAIL, "can't read from array object");

done:
    output.ret = ret_value;
    output.cs = cs;

    fprintf(stderr, "Done with dset read, sending response to client\n");

    if(S_SUCCESS != fs_handler_complete(input->fs_handle, &output))
        HDONE_ERROR(H5E_SYM, H5E_WRITEERROR, FAIL, "can't send result of write to client");
    if(S_SUCCESS != bds_handle_free(input->bds_handle))
        HDONE_ERROR(H5E_SYM, H5E_WRITEERROR, FAIL, "can't free bds block handle");
    if(S_SUCCESS != bds_block_handle_free(bds_block_handle))
        HDONE_ERROR(H5E_SYM, H5E_WRITEERROR, FAIL, "can't free bds block handle");

    if(H5P_DATASET_XFER_DEFAULT != input->dxpl_id)
        H5Pclose(input->dxpl_id);
    H5Sclose(input->space_id);

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
 *              January, 2012
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
    bds_handle_t bds_handle = input->bds_handle;
    hid_t space_id = input->space_id;
    hid_t dxpl_id = input->dxpl_id;
    uint32_t cs = input->checksum;
    bds_block_handle_t bds_block_handle;
    iod_mem_desc_t mem_desc;
    iod_array_iodesc_t file_desc;
    size_t size;
    void *buf;
    ssize_t ret;
    na_addr_t source = fs_handler_get_addr(input->fs_handle);
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    fprintf(stderr, "Start dataset Write with checksum %u\n", cs);

    /* Read bulk data here and wait for the data to be here  */
    size = bds_handle_get_size(bds_handle);
    if(NULL == (buf = malloc(size)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate read buffer");

    bds_block_handle_create(buf, size, BDS_READWRITE, &bds_block_handle);

    /* Write bulk data here and wait for the data to be there  */
    if(S_SUCCESS != bds_read(bds_handle, source, bds_block_handle))
        HGOTO_ERROR(H5E_SYM, H5E_WRITEERROR, FAIL, "can't get data from function shipper");
    /* wait for it to complete */
    if(S_SUCCESS != bds_wait(bds_block_handle, BDS_MAX_IDLE_TIME))
        HGOTO_ERROR(H5E_SYM, H5E_WRITEERROR, FAIL, "can't get data from function shipper");

    /* free the bds block handle */
    if(S_SUCCESS != bds_block_handle_free(bds_block_handle))
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

done:
    fprintf(stderr, "Done with dset write, sending response to client\n");
    if(S_SUCCESS != fs_handler_complete(input->fs_handle, &ret_value))
        HDONE_ERROR(H5E_SYM, H5E_WRITEERROR, FAIL, "can't send result of write to client");
    if(S_SUCCESS != bds_handle_free(input->bds_handle))
        HDONE_ERROR(H5E_SYM, H5E_WRITEERROR, FAIL, "can't free bds block handle");

    if(H5P_DATASET_XFER_DEFAULT != input->dxpl_id)
        H5Pclose(input->dxpl_id);
    H5Sclose(input->space_id);

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
 *              January, 2012
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
    int rank = input->rank;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    fprintf(stderr, "Start dataset Extend on the first dimension to %d\n", input->size[0]);

    if(iod_array_extend(iod_oh, IOD_TID_UNKNOWN, (iod_size_t)input->size[0], NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't extend dataset");

done:
    fprintf(stderr, "Done with dset set_extent, sending response to client\n");
    fs_handler_complete(input->fs_handle, &ret_value);

    free(input->size);
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
 *              January, 2012
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

    if((ret_value = iod_obj_close(scratch_oh, NULL, NULL)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close scratch object handle");
    if((ret_value = iod_obj_close(iod_oh, NULL, NULL)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close root object handle");

done:
    fprintf(stderr, "Done with dset close, sending response to client\n");
    fs_handler_complete(input->fs_handle, &ret_value);

    input = H5MM_xfree(input);
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_dset_close_cb() */

#endif /* H5_HAVE_EFF */
