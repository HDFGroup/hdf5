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
 * Purpose:	The IOD plugin server side dataset routines.
 */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_dset_create_cb
 *
 * Purpose:	Creates a dset as a iod object.
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
H5VL_iod_server_dset_create_cb(AXE_engine_t UNUSED axe_engine, 
                               size_t UNUSED num_n_parents, AXE_task_t UNUSED n_parents[], 
                               size_t UNUSED num_s_parents, AXE_task_t UNUSED s_parents[], 
                               void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    dset_create_in_t *input = (dset_create_in_t *)op_data->input;
    dset_create_out_t output;
    iod_handle_t coh = input->coh;
    iod_handle_t loc_handle = input->loc_oh;
    iod_obj_id_t loc_id = input->loc_id; /* The ID of the current location object */
    iod_obj_id_t dset_id = input->dset_id; /* The ID of the dataset that needs to be created */
    iod_handle_t dset_oh, cur_oh, mdkv_oh;
    iod_obj_id_t cur_id, mdkv_id, attr_id;
    const char *name = input->name;
    char *last_comp; /* the name of the dataset obtained from the last component in the path */
    iod_kv_t kv;
    iod_array_struct_t array;
    iod_size_t *max_dims;
    size_t buf_size;
    scratch_pad_t sp;
    iod_ret_t ret;
    hbool_t collective = FALSE; /* MSC - change when we allow for collective */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* the traversal will retrieve the location where the dataset needs
       to be created. The traversal will fail if an intermediate group
       does not exist. */
    if(H5VL_iod_server_traverse(coh, loc_id, loc_handle, name, FALSE, 
                                &last_comp, &cur_id, &cur_oh) < 0)
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

    /* MSC - NEED TO FIX THAT */
#if 0
    if(layout.type == H5D_CHUNKED) {
        if(NULL == (array.chunk_dims = malloc (sizeof(iod_size_t) * layout.u.chunk.ndims)))
            HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate chunk dimention size array");
        array.chunk_dims;
    }
#endif

#if H5VL_IOD_DEBUG 
    fprintf(stderr, "now creating the dataset %s cellsize %d num dimenstions %d\n",
            last_comp, array.cell_size, array.num_dims);
#endif

    /* create the dataset */
    ret = iod_obj_create(coh, IOD_TID_UNKNOWN, NULL/*hints*/, IOD_OBJ_ARRAY, NULL, &array,
                         &dset_id, NULL /*event*/);
    if(collective && (0 == ret || EEXISTS == ret)) {
        /* Dataset has been created by another process, open it */
        if (iod_obj_open_write(coh, dset_id, NULL, &dset_oh, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't open Dataset");
    }
    else if(!collective && 0 != ret) {
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't create Dataset");
    }

    /* for the process that succeeded in creating the dataset, update
       the parent KV, create scratch pad */
    if(0 == ret) {
        /* create the attribute KV object for the dataset */
        if(iod_obj_create(coh, IOD_TID_UNKNOWN, NULL, IOD_OBJ_KV, 
                          NULL, NULL, &attr_id, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't create metadata KV object");

        /* create the metadata KV object for the dataset */
        if(iod_obj_create(coh, IOD_TID_UNKNOWN, NULL, IOD_OBJ_KV, 
                          NULL, NULL, &mdkv_id, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't create metadata KV object");

        /* set values for the scratch pad object */
        sp.mdkv_id = mdkv_id;
        sp.attr_id = attr_id;
        sp.filler1_id = IOD_ID_UNDEFINED;
        sp.filler2_id = IOD_ID_UNDEFINED;

        /* set scratch pad in dataset */
        if (iod_obj_set_scratch(dset_oh, IOD_TID_UNKNOWN, &sp, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't set scratch pad");

        /* Store Metadata in scratch pad */
        if (iod_obj_open_write(coh, mdkv_id, NULL, &mdkv_oh, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't create scratch pad");

        /* MSC - TODO store things */
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
        if (iod_kv_set(mdkv_oh, IOD_TID_UNKNOWN, NULL, &kv, NULL, NULL) < 0)
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
        if (iod_kv_set(mdkv_oh, IOD_TID_UNKNOWN, NULL, &kv, NULL, NULL) < 0)
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
        if (iod_kv_set(mdkv_oh, IOD_TID_UNKNOWN, NULL, &kv, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't set KV pair in parent");
        HDfree(kv.key);
        free(kv.value);
#endif
        /* close the Metadata KV object */
        if(iod_obj_close(mdkv_oh, NULL, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close object");

        kv.key = HDstrdup(last_comp);
        kv.value = &dset_id;
        kv.value_len = sizeof(iod_obj_id_t);
        /* insert new dataset in kv store of current group */
        if (iod_kv_set(cur_oh, IOD_TID_UNKNOWN, NULL, &kv, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't set KV pair in parent");
        HDfree(kv.key);
    }

    /* close parent group if it is not the location we started the
       traversal into */
    if(loc_handle.cookie != cur_oh.cookie) {
        iod_obj_close(cur_oh, NULL, NULL);
    }

#if H5_DO_NATIVE
    cur_oh.cookie = H5Dcreate2(loc_handle.cookie, last_comp, input->type_id, 
                               input->space_id, input->lcpl_id, 
                               input->dcpl_id, input->dapl_id);
    HDassert(cur_oh.cookie);
#endif

    output.iod_oh = cur_oh;

#if H5VL_IOD_DEBUG 
    fprintf(stderr, "Done with dset create, sending response to client\n");
#endif

    HG_Handler_start_output(op_data->hg_handle, &output);

done:
    /* return an UNDEFINED oh to the client if the operation failed */
    if(ret_value < 0) {
        output.iod_oh.cookie = IOD_OH_UNDEFINED;
        HG_Handler_start_output(op_data->hg_handle, &output);
    }

    if(max_dims) free(max_dims);
    if(array.current_dims) free(array.current_dims);
    last_comp = (char *)H5MM_xfree(last_comp);
    input = (dset_create_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

    FUNC_LEAVE_NOAPI_VOID
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
 *              February, 2013
 *
 *-------------------------------------------------------------------------
 */
void
H5VL_iod_server_dset_open_cb(AXE_engine_t UNUSED axe_engine, 
                             size_t UNUSED num_n_parents, AXE_task_t UNUSED n_parents[], 
                             size_t UNUSED num_s_parents, AXE_task_t UNUSED s_parents[], 
                             void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    dset_open_in_t *input = (dset_open_in_t *)op_data->input;
    dset_open_out_t output;
    iod_handle_t coh = input->coh;
    iod_handle_t loc_handle = input->loc_oh;
    iod_obj_id_t loc_id = input->loc_id;
    iod_handle_t cur_oh, mdkv_oh;
    iod_obj_id_t cur_id;
    iod_obj_id_t dset_id;
    char *name = input->name;
    char *last_comp;
    scratch_pad_t sp;
    iod_size_t kv_size = sizeof(iod_obj_id_t);
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

#if H5VL_IOD_DEBUG 
    fprintf(stderr, "Start dataset Open %s\n", name);
#endif

    /* the traversal will retrieve the location where the dataset needs
       to be opened. The traversal will fail if an intermediate group
       does not exist. */
    if(H5VL_iod_server_traverse(coh, loc_id, loc_handle, name, FALSE, 
                                &last_comp, &cur_id, &cur_oh) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't traverse path");

    if(iod_kv_get_value(cur_oh, IOD_TID_UNKNOWN, last_comp, &dset_id, 
                        kv_size , NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't retrieve Array ID from parent KV store");

    /* close parent group and its scratch pad if it is not the
       location we started the traversal into */
    if(loc_handle.cookie != cur_oh.cookie) {
        iod_obj_close(cur_oh, NULL, NULL);
    }

    /* open the dataset */
    if (iod_obj_open_write(coh, dset_id, NULL /*hints*/, &cur_oh, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't open dataset");

    /* get scratch pad of the dataset */
    if(iod_obj_get_scratch(cur_oh, IOD_TID_UNKNOWN, &sp, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "can't get scratch pad for object");

    /* open the metadata scratch pad */
    if (iod_obj_open_write(coh, sp.mdkv_id, NULL /*hints*/, &mdkv_oh, NULL) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "can't open scratch pad");

#if 0
    /* MSC - retrieve all metadata from scratch pad */
    if(iod_kv_get_value(mdkv_oh, IOD_TID_UNKNOWN, "dataset_dcpl", NULL, 
                        &output.dcpl_size, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "dataset dcpl lookup failed");
    if(NULL == (output.dcpl = H5MM_malloc (output.dcpl_size)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate dcpl buffer");
    if(iod_kv_get_value(mdkv_oh, IOD_TID_UNKNOWN, "dataset_dcpl", output.dcpl, 
                        &output.dcpl_size, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "dataset dcpl lookup failed");

    if(iod_kv_get_value(mdkv_oh, IOD_TID_UNKNOWN, "dataset_dtype", NULL, 
                        &output.dtype_size, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "dataset dtype lookup failed");
    if(NULL == (output.dtype = H5MM_malloc (output.dtype_size)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate dtype buffer");
    if(iod_kv_get_value(mdkv_oh, IOD_TID_UNKNOWN, "dataset_dtype", output.dtype, 
                        &output.dtype_size, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "dataset dtype lookup failed");

    if(iod_kv_get_value(mdkv_oh, IOD_TID_UNKNOWN, "dataset_dspace", NULL, 
                        &output.dspace_size, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "dataset dspace lookup failed");
    if(NULL == (output.dspace = H5MM_malloc (output.dspace_size)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate dspace buffer");
    if(iod_kv_get_value(mdkv_oh, IOD_TID_UNKNOWN, "dataset_dspace", output.dspace, 
                        &output.dspace_size, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "dataset dspace lookup failed");
#endif

    /* close the metadata scratch pad */
    if(iod_obj_close(mdkv_oh, NULL, NULL))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close object");

    {
        hsize_t dims[1];
        //hid_t space_id, type_id;

#if H5_DO_NATIVE
        printf("dataset name %s    location %d\n", name, loc_handle.cookie);
        cur_oh.cookie = H5Dopen(loc_handle.cookie, name, input->dapl_id);
        HDassert(cur_oh.cookie);
        output.space_id = H5Dget_space(cur_oh.cookie);
        output.type_id = H5Dget_type(cur_oh.cookie);
        output.dcpl_id = H5P_DATASET_CREATE_DEFAULT;
#else
        /* fake a dataspace, type, and dcpl */
        dims [0] = 60;
        output.space_id = H5Screate_simple(1, dims, NULL);
        output.type_id = H5Tcopy(H5T_NATIVE_INT);
        output.dcpl_id = H5P_DATASET_CREATE_DEFAULT;
        cur_oh.cookie = 1;
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

    dset_id = 1;
    output.iod_id = dset_id;
    output.iod_oh.cookie = cur_oh.cookie;

#if H5VL_IOD_DEBUG 
    fprintf(stderr, "Done with dset open, sending response to client\n");
#endif

    HG_Handler_start_output(op_data->hg_handle, &output);

done:
    if(ret_value < 0) {
        output.iod_oh.cookie = IOD_OH_UNDEFINED;
        output.iod_id = IOD_ID_UNDEFINED;
        HG_Handler_start_output(op_data->hg_handle, &output);
    }

    H5Tclose(output.type_id);
    H5Sclose(output.space_id);

    input = (dset_open_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);
    last_comp = (char *)H5MM_xfree(last_comp);

    FUNC_LEAVE_NOAPI_VOID
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
void
H5VL_iod_server_dset_read_cb(AXE_engine_t UNUSED axe_engine, 
                             size_t UNUSED num_n_parents, AXE_task_t UNUSED n_parents[], 
                             size_t UNUSED num_s_parents, AXE_task_t UNUSED s_parents[], 
                             void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    dset_io_in_t *input = (dset_io_in_t *)op_data->input;
    dset_read_out_t output;
    iod_handle_t coh = input->coh;
    iod_handle_t iod_oh = input->iod_oh;
    iod_obj_id_t iod_id = input->iod_id;    
    hg_bulk_t bulk_handle = input->bulk_handle;
    hid_t space_id = input->space_id;
    hid_t dxpl_id = input->dxpl_id;
    hid_t src_id = input->dset_type_id;
    hid_t dst_id = input->mem_type_id;
    hg_bulk_block_t bulk_block_handle;
    hg_bulk_request_t bulk_request;
    iod_mem_desc_t mem_desc;
    iod_array_iodesc_t file_desc;
    iod_hyperslab_t *hslabs = NULL;
    size_t size, buf_size, src_size, dst_size;
    void *buf;
    uint8_t *buf_ptr;
    hssize_t num_descriptors = 0, n;
    int ndims, i;
    uint32_t cs = 0;
    size_t nelmts;
    na_addr_t dest = HG_Handler_get_addr(op_data->hg_handle);
    hbool_t opened_locally = FALSE;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* open the dataset if we don't have the handle yet */
    if(iod_oh.cookie == IOD_OH_UNDEFINED) {
        if (iod_obj_open_write(coh, iod_id, NULL /*hints*/, &iod_oh, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't open current group");
        opened_locally = TRUE;
    }

    size = HG_Bulk_handle_get_size(bulk_handle);

    nelmts = (size_t)H5Sget_select_npoints(space_id);

    src_size = H5Tget_size(src_id);
    dst_size = H5Tget_size(dst_id);

    /* adjust buffer size for datatype conversion */
    if(src_size > dst_size) {
        buf_size = src_size * nelmts;
#if H5VL_IOD_DEBUG 
        fprintf(stderr, "Adjusted Buffer size because of datatype conversion from %d to %d: ", 
                size, buf_size);        
#endif
    }
    else {
        buf_size = dst_size * nelmts;
        assert(buf_size == size);
    }

    if(NULL == (buf = malloc(buf_size)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate read buffer");

    /* get the rank of the dataspace */
    if((ndims = H5Sget_simple_extent_ndims(space_id)) < 0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTGET, FAIL, "unable to get dataspace dimesnsion");

    /* get the number of decriptors required, i.e. the numbers of iod
       I/O operations needed */
    if(H5VL_iod_get_file_desc(space_id, &num_descriptors, NULL) < 0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTGET, FAIL, "unable to generate IOD file descriptor from dataspace selection");

    /* allocate the IOD hyperslab descriptors needed */
    if(NULL == (hslabs = (iod_hyperslab_t *)malloc
                (sizeof(iod_hyperslab_t) * (size_t)num_descriptors)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate iod array descriptors");

    for(n=0 ; n<num_descriptors ; n++) {
        hslabs[n].start = (iod_size_t *)malloc(sizeof(iod_size_t) * ndims);
        hslabs[n].stride = (iod_size_t *)malloc(sizeof(iod_size_t) * ndims);
        hslabs[n].block = (iod_size_t *)malloc(sizeof(iod_size_t) * ndims);
        hslabs[n].count = (iod_size_t *)malloc(sizeof(iod_size_t) * ndims);
    }

    /* generate the descriptors after allocating the array */
    if(H5VL_iod_get_file_desc(space_id, &num_descriptors, hslabs) < 0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTGET, FAIL, "unable to generate IOD file descriptor from dataspace selection");

    buf_ptr = (uint8_t *)buf;

    /* read each descriptore from the IOD container */
    for(n=0 ; n<num_descriptors ; n++) {
        hsize_t num_bytes = 0;
        hsize_t num_elems = 0;

        /* determine how many bytes the current descriptor holds */
        for(i=0 ; i<ndims ; i++)
            num_elems *= (hslabs[n].count[i] * hslabs[n].block[i]);
        num_bytes = num_elems * src_size;

#if 0
        /* set the memory descriptor */
        mem_desc.nfrag = 1;
        mem_desc.frag->addr = (void *)buf_ptr;
        mem_desc.frag->len = (iod_size_t)num_bytes;
#endif

        buf_ptr += num_bytes;

        /* set the file descriptor */
        file_desc = hslabs[n];

#if H5VL_IOD_DEBUG 
        for(i=0 ; i<ndims ; i++) {
            fprintf(stderr, "Dim %d:  start %zu   stride %zu   block %zu   count %zu\n", 
                    i, (size_t)file_desc.start[i], (size_t)file_desc.stride[i], 
                    (size_t)file_desc.block[i], (size_t)file_desc.count[i]);
        }
#endif

        /* read from array object */
        if(iod_array_read(iod_oh, IOD_TID_UNKNOWN, NULL, &mem_desc, &file_desc, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_READERROR, FAIL, "can't read from array object");
    }
    
    {
        hbool_t flag = FALSE;
        int *ptr = (int *)buf;

#if H5_DO_NATIVE
        ret_value = H5Dread(iod_oh.cookie, src_id, H5S_ALL, space_id, dxpl_id, buf);
#else /* fake data */
        for(i=0;i<60;++i)
            ptr[i] = i;
#endif
        if(H5Tconvert(src_id, dst_id, nelmts, buf, NULL, dxpl_id) < 0)
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "data type conversion failed");

        /* calculate a checksum for the data to be sent */
        cs = H5checksum(buf, size, NULL);

        /* MSC - check if client requested to corrupt data */
        if(dxpl_id != H5P_DEFAULT && H5Pget_dxpl_inject_corruption(dxpl_id, &flag) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_READERROR, FAIL, "can't read property list");
        if(flag) {
            fprintf(stderr, "Injecting a bad data value to cause corruption \n");
            ptr[0] = 10;
        }
    }

    /* Create a new block handle to write the data */
    HG_Bulk_block_handle_create(buf, size, HG_BULK_READ_ONLY, &bulk_block_handle);

    /* Write bulk data here and wait for the data to be there  */
    if(HG_SUCCESS != HG_Bulk_write_all(dest, bulk_handle, bulk_block_handle, &bulk_request))
        HGOTO_ERROR(H5E_SYM, H5E_READERROR, FAIL, "can't read from array object");
    /* wait for it to complete */
    if(HG_SUCCESS != HG_Bulk_wait(bulk_request, HG_BULK_MAX_IDLE_TIME, HG_BULK_STATUS_IGNORE))
        HGOTO_ERROR(H5E_SYM, H5E_READERROR, FAIL, "can't read from array object");

done:

    output.ret = ret_value;
    output.cs = cs;

#if H5VL_IOD_DEBUG 
    fprintf(stderr, "Done with dset read, checksum %u, sending response to client\n", cs);
#endif

    if(HG_SUCCESS != HG_Handler_start_output(op_data->hg_handle, &output))
        HDONE_ERROR(H5E_SYM, H5E_WRITEERROR, FAIL, "can't send result of write to client");
    if(HG_SUCCESS != HG_Bulk_block_handle_free(bulk_block_handle))
        HDONE_ERROR(H5E_SYM, H5E_WRITEERROR, FAIL, "can't free bds block handle");

    input = (dset_io_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);
    free(buf);

    /* free allocated descriptors */
    for(n=0 ; n<num_descriptors ; n++) {
        free(hslabs[n].start);
        free(hslabs[n].stride);
        free(hslabs[n].block);
        free(hslabs[n].count);
    }
    if(hslabs)
        free(hslabs);

    /* close the dataset if we opened it in this routine */
    if(opened_locally) {
        if(iod_obj_close(iod_oh, NULL, NULL))
            HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close Array object");
    }
    FUNC_LEAVE_NOAPI_VOID
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
void
H5VL_iod_server_dset_write_cb(AXE_engine_t UNUSED axe_engine, 
                              size_t UNUSED num_n_parents, AXE_task_t UNUSED n_parents[], 
                              size_t UNUSED num_s_parents, AXE_task_t UNUSED s_parents[], 
                              void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    dset_io_in_t *input = (dset_io_in_t *)op_data->input;
    iod_handle_t coh = input->coh;
    iod_handle_t iod_oh = input->iod_oh;
    iod_obj_id_t iod_id = input->iod_id; 
    hg_bulk_t bulk_handle = input->bulk_handle;
    hid_t space_id = input->space_id;
    hid_t dxpl_id = input->dxpl_id;
    uint32_t cs = input->checksum;
    hid_t src_id = input->mem_type_id;
    hid_t dst_id = input->dset_type_id;
    uint32_t data_cs = 0;
    hg_bulk_block_t bulk_block_handle;
    hg_bulk_request_t bulk_request;
    iod_mem_desc_t mem_desc;
    iod_array_iodesc_t file_desc;
    iod_hyperslab_t *hslabs = NULL;
    size_t size, buf_size, src_size, dst_size;
    hssize_t num_descriptors = 0, n;
    int ndims, i;
    void *buf;
    uint8_t *buf_ptr;
    size_t nelmts;
    hbool_t flag = FALSE;
    na_addr_t source = HG_Handler_get_addr(op_data->hg_handle);
    hbool_t opened_locally = FALSE;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Dataset Write with AXE ID %llu\n",input->axe_id);
#endif
    /* open the dataset if we don't have the handle yet */
    if(iod_oh.cookie == IOD_OH_UNDEFINED) {
        if (iod_obj_open_write(coh, iod_id, NULL /*hints*/, &iod_oh, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't open current group");
        opened_locally = TRUE;
    }

    /* Read bulk data here and wait for the data to be here  */
    size = HG_Bulk_handle_get_size(bulk_handle);

    nelmts = (size_t)H5Sget_select_npoints(space_id);

    src_size = H5Tget_size(src_id);
    dst_size = H5Tget_size(dst_id);

    /* adjust buffer size for datatype conversion */
    if(src_size < dst_size) {
        buf_size = dst_size * nelmts;
#if H5VL_IOD_DEBUG 
        fprintf(stderr, "Adjusted Buffer size because of datatype conversion from %d to %d\n", 
                size, buf_size);
#endif
    }
    else {
        buf_size = src_size * nelmts;
        assert(buf_size == size);
    }

    if(NULL == (buf = malloc(buf_size)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate read buffer");

    HG_Bulk_block_handle_create(buf, size, HG_BULK_READWRITE, &bulk_block_handle);

    /* Write bulk data here and wait for the data to be there  */
    if(HG_SUCCESS != HG_Bulk_read_all(source, bulk_handle, bulk_block_handle, &bulk_request))
        HGOTO_ERROR(H5E_SYM, H5E_WRITEERROR, FAIL, "can't get data from function shipper");
    /* wait for it to complete */
    if(HG_SUCCESS != HG_Bulk_wait(bulk_request, HG_BULK_MAX_IDLE_TIME, HG_BULK_STATUS_IGNORE))
        HGOTO_ERROR(H5E_SYM, H5E_WRITEERROR, FAIL, "can't get data from function shipper");

    /* free the bds block handle */
    if(HG_SUCCESS != HG_Bulk_block_handle_free(bulk_block_handle))
        HGOTO_ERROR(H5E_SYM, H5E_WRITEERROR, FAIL, "can't free bds block handle");

    /* MSC - check if client requested to corrupt data */
    if(dxpl_id != H5P_DEFAULT && H5Pget_dxpl_inject_corruption(dxpl_id, &flag) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_READERROR, FAIL, "can't read property list");
    if(flag) {
        ((int *)buf)[0] = 10;
    }

    /* If client specified a checksum, verify it */
    if(dxpl_id != H5P_DEFAULT && H5Pget_dxpl_checksum(dxpl_id, &data_cs) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_READERROR, FAIL, "can't read property list");
    if(data_cs != 0) {
        cs = H5_checksum_lookup4(buf, size, NULL);
        if(cs != data_cs) {
            fprintf(stderr, "Errrr.. Network transfer Data corruption. expecting %u, got %u\n",
                    data_cs, cs);
            HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "Checksum verification failed");
        }
    }

    if(H5Tconvert(src_id, dst_id, nelmts, buf, NULL, dxpl_id) < 0)
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "data type conversion failed")

#if H5VL_IOD_DEBUG 
    { 
        int *ptr = (int *)buf;

        fprintf(stderr, "DWRITE Received a buffer of size %d with values: ", size);
        for(i=0 ; i<size/sizeof(int) ; ++i)
            fprintf(stderr, "%d ", ptr[i]);
        fprintf(stderr, "\n");
    }
#endif

    /* get the rank of the dataspace */
    if((ndims = H5Sget_simple_extent_ndims(space_id)) < 0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTGET, FAIL, "unable to get dataspace dimesnsion");

    /* get the number of decriptors required, i.e. the numbers of iod
       I/O operations needed */
    if(H5VL_iod_get_file_desc(space_id, &num_descriptors, NULL) < 0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTGET, FAIL, "unable to generate IOD file descriptor from dataspace selection");

    /* allocate the IOD hyperslab descriptors needed */
    if(NULL == (hslabs = (iod_hyperslab_t *)malloc
                (sizeof(iod_hyperslab_t) * (size_t)num_descriptors)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate iod array descriptors");

    for(n=0 ; n<num_descriptors ; n++) {
        hslabs[n].start = (iod_size_t *)malloc(sizeof(iod_size_t) * ndims);
        hslabs[n].stride = (iod_size_t *)malloc(sizeof(iod_size_t) * ndims);
        hslabs[n].block = (iod_size_t *)malloc(sizeof(iod_size_t) * ndims);
        hslabs[n].count = (iod_size_t *)malloc(sizeof(iod_size_t) * ndims);
    }

    /* generate the descriptors after allocating the array */
    if(H5VL_iod_get_file_desc(space_id, &num_descriptors, hslabs) < 0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTGET, FAIL, "unable to generate IOD file descriptor from dataspace selection");

    buf_ptr = (uint8_t *)buf;

    /* write each descriptore to the IOD container */
    for(n=0 ; n<num_descriptors ; n++) {
        hsize_t num_bytes = 0;
        hsize_t num_elems = 0;

        /* determine how many bytes the current descriptor holds */
        for(i=0 ; i<ndims ; i++)
            num_elems *= (hslabs[n].count[i] * hslabs[n].block[i]);
        num_bytes = num_elems * dst_size;

#if 0
        /* set the memory descriptor */
        mem_desc.nfrag = 1;
        mem_desc.frag->addr = (void *)buf_ptr;
        mem_desc.frag->len = (iod_size_t)num_bytes;
#endif

        buf_ptr += num_bytes;

        /* set the file descriptor */
        file_desc = hslabs[n];

#if H5VL_IOD_DEBUG 
        for(i=0 ; i<ndims ; i++) {
            fprintf(stderr, "Dim %d:  start %zu   stride %zu   block %zu   count %zu\n", 
                    i, (size_t)file_desc.start[i], (size_t)file_desc.stride[i], 
                    (size_t)file_desc.block[i], (size_t)file_desc.count[i]);
        }
#endif

        /* write from array object */
        if(iod_array_write(iod_oh, IOD_TID_UNKNOWN, NULL, &mem_desc, &file_desc, &cs, NULL) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "can't write to array object");
    }

#if H5_DO_NATIVE
    ret_value = H5Dwrite(iod_oh.cookie, H5T_NATIVE_INT, H5S_ALL, space_id, dxpl_id, buf);
#endif

done:
#if H5VL_IOD_DEBUG 
    fprintf(stderr, "Done with dset write, sending %d response to client\n", ret_value);
#endif

    if(HG_SUCCESS != HG_Handler_start_output(op_data->hg_handle, &ret_value))
        HDONE_ERROR(H5E_SYM, H5E_WRITEERROR, FAIL, "can't send result of write to client");
#if 0
    /* Shut down datatype info for operation */
    if(type_info_init && H5VL_iod_typeinfo_term(&type_info) < 0)
        HDONE_ERROR(H5E_DATASET, H5E_CANTCLOSEOBJ, FAIL, "unable to shut down type info")
#endif

    input = (dset_io_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);
    free(buf);

    /* free allocated descriptors */
    for(n=0 ; n<num_descriptors ; n++) {
        free(hslabs[n].start);
        free(hslabs[n].stride);
        free(hslabs[n].block);
        free(hslabs[n].count);
    }
    if(hslabs)
        free(hslabs);

    /* close the dataset if we opened it in this routine */
    if(opened_locally) {
        if(iod_obj_close(iod_oh, NULL, NULL))
            HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close Array object");
    }
    FUNC_LEAVE_NOAPI_VOID
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
void
H5VL_iod_server_dset_set_extent_cb(AXE_engine_t UNUSED axe_engine, 
                                   size_t UNUSED num_n_parents, AXE_task_t UNUSED n_parents[], 
                                   size_t UNUSED num_s_parents, AXE_task_t UNUSED s_parents[], 
                                   void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    dset_set_extent_in_t *input = (dset_set_extent_in_t *)op_data->input;
    iod_handle_t coh = input->coh;
    iod_handle_t iod_oh = input->iod_oh;
    iod_obj_id_t iod_id = input->iod_id; 
    int rank = input->dims.rank;
    hbool_t opened_locally = FALSE;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

#if H5VL_IOD_DEBUG 
    fprintf(stderr, "Start dataset Extend on the first dimension to %d\n", input->dims.size[0]);
#endif

    /* open the dataset if we don't have the handle yet */
    if(iod_oh.cookie == IOD_OH_UNDEFINED) {
        if (iod_obj_open_write(coh, iod_id, NULL /*hints*/, &iod_oh, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't open current group");        
        opened_locally = TRUE;
    }

    /* extend along the first dimension only */
    if(iod_array_extend(iod_oh, IOD_TID_UNKNOWN, (iod_size_t)input->dims.size[0], NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't extend dataset");

#if H5_DO_NATIVE
    ret_value = H5Dset_extent(iod_oh.cookie, input->dims.size);
#endif

done:
#if H5VL_IOD_DEBUG
    fprintf(stderr, "Done with dset set_extent, sending response to client\n");
#endif

    HG_Handler_start_output(op_data->hg_handle, &ret_value);

    input = (dset_set_extent_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

    /* close the dataset if we opened it in this routine */
    if(opened_locally) {
        if(iod_obj_close(iod_oh, NULL, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close Array object");
    }

    FUNC_LEAVE_NOAPI_VOID
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
void
H5VL_iod_server_dset_close_cb(AXE_engine_t UNUSED axe_engine, 
                              size_t UNUSED num_n_parents, AXE_task_t UNUSED n_parents[], 
                              size_t UNUSED num_s_parents, AXE_task_t UNUSED s_parents[], 
                              void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    dset_close_in_t *input = (dset_close_in_t *)op_data->input;
    iod_handle_t iod_oh = input->iod_oh;
    iod_obj_id_t iod_id = input->iod_id; 
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Start dataset Close\n");
#endif

    if(iod_oh.cookie != IOD_OH_UNDEFINED) {
#if H5_DO_NATIVE
        ret_value = H5Dclose(iod_oh.cookie);
#endif

        if((ret_value = iod_obj_close(iod_oh, NULL, NULL)) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close object");
    }
    else {
        /* MSC - need a way to kill object handle for this group */
        fprintf(stderr, "I do not have the OH of this dataset to close it\n");
    }

done:
#if H5VL_IOD_DEBUG
    fprintf(stderr, "Done with dset close, sending response to client\n");
#endif

    HG_Handler_start_output(op_data->hg_handle, &ret_value);

    input = (dset_close_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

    FUNC_LEAVE_NOAPI_VOID
} /* end H5VL_iod_server_dset_close_cb() */

#endif /* H5_HAVE_EFF */
