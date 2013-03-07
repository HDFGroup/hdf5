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
#include "H5VLprivate.h"	/* VOL plugins				*/
#include "H5VLiod.h"            /* Iod VOL plugin			*/
#include "H5VLiod_common.h"
#include "H5VLiod_server.h"
#include "H5WBprivate.h"        /* Wrapped Buffers                      */

/*
 * Programmer:  Mohamad Chaarawi <chaarawi@hdfgroup.gov>
 *              February, 2012
 *
 * Purpose:	The IOD plugin server side routines.
 */

AXE_engine_t engine = NULL;

#define EEXISTS 0
iod_obj_id_t ROOT_ID;

static herr_t H5VL_iod_server_file_create_cb(size_t num_necessary_parents, AXE_task_t necessary_parents[], 
                                             size_t num_sufficient_parents, AXE_task_t sufficient_parents[], 
                                             void *op_data);
static herr_t H5VL_iod_server_file_open_cb(size_t num_necessary_parents, AXE_task_t necessary_parents[], 
                                           size_t num_sufficient_parents, AXE_task_t sufficient_parents[], 
                                           void *op_data);
static herr_t H5VL_iod_server_file_close_cb(size_t num_necessary_parents, AXE_task_t necessary_parents[], 
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
static herr_t H5VL_iod_server_dset_close_cb(size_t num_necessary_parents, AXE_task_t necessary_parents[], 
                                               size_t num_sufficient_parents, AXE_task_t sufficient_parents[], 
                                               void *op_data);


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

    if(NULL == engine) {
        if(AXE_SUCCEED != AXEcreate_engine(4, &engine))
            HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, S_FAIL, "can't start AXE engine");
    }
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

    if(NULL == engine) {
        if(AXE_SUCCEED != AXEcreate_engine(4, &engine))
            HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, S_FAIL, "can't start AXE engine");
    }
    input->fs_handle = handle;
    if (AXE_SUCCEED != AXEcreate_task(engine, &task, 0, NULL, 0, NULL, H5VL_iod_server_file_open_cb, 
                                      input, NULL))
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, S_FAIL, "can't insert task into async engine");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_file_open() */


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

    if(NULL == engine) {
        if(AXE_SUCCEED != AXEcreate_engine(4, &engine))
            HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, S_FAIL, "can't start AXE engine");
    }
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

    if(NULL == engine) {
        if(AXE_SUCCEED != AXEcreate_engine(4, &engine))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, S_FAIL, "can't start AXE engine");
    }
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

    if(NULL == engine) {
        if(AXE_SUCCEED != AXEcreate_engine(4, &engine))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, S_FAIL, "can't start AXE engine");
    }
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

    if(NULL == engine) {
        if(AXE_SUCCEED != AXEcreate_engine(4, &engine))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, S_FAIL, "can't start AXE engine");
    }
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

    if(NULL == engine) {
        if(AXE_SUCCEED != AXEcreate_engine(4, &engine))
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, S_FAIL, "can't start AXE engine");
    }
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

    if(NULL == engine) {
        if(AXE_SUCCEED != AXEcreate_engine(4, &engine))
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, S_FAIL, "can't start AXE engine");
    }
    input->fs_handle = handle;
    if (AXE_SUCCEED != AXEcreate_task(engine, &task, 0, NULL, 0, NULL, H5VL_iod_server_dset_open_cb, 
                                      input, NULL))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, S_FAIL, "can't insert task into async engine");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_dset_open() */


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

    if(NULL == engine) {
        if(AXE_SUCCEED != AXEcreate_engine(4, &engine))
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, S_FAIL, "can't start AXE engine");
    }
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

    /* convert HDF5 flags to IOD flags */
    mode = (input->flags&H5F_ACC_RDWR) ? IOD_CONT_RW : IOD_CONT_RO;
    if (input->flags&H5F_ACC_CREAT) 
        mode |= IOD_CONT_CREATE;

    status = iod_container_open(input->name, NULL /*hints*/, mode, &coh, NULL /*event*/);

    if (EEXISTS != status && status > 0) {
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

    fs_handler_complete(input->fs_handle, &output);

done:
    if(ret_value < 0)
        fs_handler_complete(input->fs_handle, &ret_value);

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

    fs_handler_complete(input->fs_handle, &output);

done:
    if(ret_value < 0)
        fs_handler_complete(input->fs_handle, &ret_value);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_file_open_cb() */


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

    if((ret_value = iod_obj_close(scratch_oh, NULL, NULL)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close scratch object handle");
    if((ret_value = iod_obj_close(root_oh, NULL, NULL)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close root object handle");
    if((ret_value = iod_container_close(coh, NULL, NULL)) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "can't close container");

done:
    fs_handler_complete(input->fs_handle, &ret_value);
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
                            &kv_size, NULL, NULL) < 0) {
            iod_kv_t kv;

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

    fs_handler_complete(input->fs_handle, &output);

done:
    if(ret_value < 0)
        fs_handler_complete(input->fs_handle, &ret_value);

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

    fs_handler_complete(input->fs_handle, &output);

done:
    if(ret_value < 0)
        fs_handler_complete(input->fs_handle, &ret_value);

    H5MM_xfree(output.gcpl);

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

    if((ret_value = iod_obj_close(scratch_oh, NULL, NULL)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close scratch object handle");
    if((ret_value = iod_obj_close(iod_oh, NULL, NULL)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close root object handle");

done:
    fs_handler_complete(input->fs_handle, &ret_value);
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
        if(iod_kv_get_value(cur_oh, IOD_TID_UNKNOWN, comp, &cur_id, 
                            &kv_size, NULL, NULL) < 0) {
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

    /* setup the array struct */
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
    if(H5Pencode(input->dcpl_id, NULL, &buf_size) < 0)
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

    /* Release temporary component buffer */
    if(wb && H5WB_unwrap(wb) < 0)
        HDONE_ERROR(H5E_SYM, H5E_CANTRELEASE, FAIL, "can't release wrapped buffer")

    output.iod_id = cur_id;
    output.iod_oh = cur_oh;
    output.scratch_id = scratch_pad;
    output.scratch_oh = scratch_handle;

    free(max_dims);
    fs_handler_complete(input->fs_handle, &output);

done:
    if(ret_value < 0)
        fs_handler_complete(input->fs_handle, &ret_value);

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
    }

    /* get scratch pad of dataset */
    if(iod_obj_get_scratch(cur_oh, IOD_TID_UNKNOWN, &scratch_pad, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "can't get scratch pad for root object");

    /* open the scratch pad */
    if (iod_obj_open_write(coh, scratch_pad, NULL /*hints*/, &scratch_handle, NULL) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "can't open scratch pad");

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

    /* Release temporary component buffer */
    if(wb && H5WB_unwrap(wb) < 0)
        HDONE_ERROR(H5E_SYM, H5E_CANTRELEASE, FAIL, "can't release wrapped buffer");

    output.iod_id = cur_id;
    output.iod_oh = cur_oh;
    output.scratch_id = scratch_pad;
    output.scratch_oh = scratch_handle;

    fs_handler_complete(input->fs_handle, &output);

done:
    if(ret_value < 0)
        fs_handler_complete(input->fs_handle, &ret_value);

    H5MM_xfree(output.dcpl);
    H5MM_xfree(output.dtype);
    H5MM_xfree(output.dspace);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_dset_open_cb() */


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

    if((ret_value = iod_obj_close(scratch_oh, NULL, NULL)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close scratch object handle");
    if((ret_value = iod_obj_close(iod_oh, NULL, NULL)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close root object handle");

done:
    fs_handler_complete(input->fs_handle, &ret_value);
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_dset_close_cb() */
