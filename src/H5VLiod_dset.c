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
#include "H5VLiod_compactor_queue.h"
#include "H5VLiod_compactor.h"

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
    iod_handle_t coh = input->coh; /* container handle */
    iod_handle_t loc_handle = input->loc_oh; /* location handle to start lookup */
    iod_obj_id_t loc_id = input->loc_id; /* The ID of the current location object */
    iod_obj_id_t dset_id = input->dset_id; /* The ID of the dataset that needs to be created */
    iod_handle_t dset_oh, cur_oh, mdkv_oh;
    iod_obj_id_t cur_id, mdkv_id, attr_id;
    const char *name = input->name; /* name of dset including path to create */
    char *last_comp; /* the name of the dataset obtained from the last component in the path */
    hid_t dcpl_id;
    iod_array_struct_t array; /* IOD array struct describing the dataset's dimensions */
    iod_size_t *max_dims;
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

        /* Open Metadata KV object for write */
        if (iod_obj_open_write(coh, mdkv_id, NULL, &mdkv_oh, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't create scratch pad");

        if(H5P_DEFAULT == input->dcpl_id)
            dcpl_id = H5P_DATASET_CREATE_DEFAULT;
        else
            dcpl_id = input->dcpl_id;

        /* insert plist metadata */
        if(H5VL_iod_insert_plist(mdkv_oh, IOD_TID_UNKNOWN, dcpl_id, 
                                 NULL, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't insert KV value");

        /* insert link count metadata */
        if(H5VL_iod_insert_link_count(mdkv_oh, IOD_TID_UNKNOWN, (uint64_t)1, 
                                      NULL, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't insert KV value");

        /* insert object type metadata */
        if(H5VL_iod_insert_object_type(mdkv_oh, IOD_TID_UNKNOWN, H5I_DATASET, 
                                       NULL, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't insert KV value");

        /* MSC - need to check size of datatype if it fits in
           entry otherwise create a BLOB*/
        /* insert datatype metadata */
        if(H5VL_iod_insert_datatype(mdkv_oh, IOD_TID_UNKNOWN, input->type_id, 
                                    NULL, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't insert KV value");

        /* insert dataspace metadata */
        if(H5VL_iod_insert_dataspace(mdkv_oh, IOD_TID_UNKNOWN, input->space_id, 
                                     NULL, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't insert KV value");

        /* close the Metadata KV object */
        if(iod_obj_close(mdkv_oh, NULL, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close object");

        /* add link in parent group to current object */
        if(H5VL_iod_insert_new_link(cur_oh, IOD_TID_UNKNOWN, last_comp, 
                                    H5L_TYPE_HARD, dset_id, NULL, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't insert KV value");
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
    assert(cur_oh.cookie);
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
    iod_handle_t coh = input->coh; /* container handle */
    iod_handle_t loc_handle = input->loc_oh; /* location handle to start lookup */
    iod_obj_id_t loc_id = input->loc_id; /* The ID of the current location object */
    iod_obj_id_t dset_id; /* ID of the dataset to open */
    iod_handle_t dset_oh, mdkv_oh;
    const char *name = input->name; /* name of dset including path to open */
    scratch_pad_t sp;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

#if H5VL_IOD_DEBUG 
    fprintf(stderr, "Start dataset Open %s\n", name);
#endif

    /* Traverse Path and open dset */
    if(H5VL_iod_server_open_path(coh, loc_id, loc_handle, name, &dset_id, &dset_oh) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't open object");

    /* get scratch pad of the dataset */
    if(iod_obj_get_scratch(dset_oh, IOD_TID_UNKNOWN, &sp, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "can't get scratch pad for object");

    /* open the metadata scratch pad */
    if (iod_obj_open_write(coh, sp.mdkv_id, NULL /*hints*/, &mdkv_oh, NULL) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "can't open scratch pad");

    /* MSC - retrieve metadata - NEED IOD */
#if 0
    if(H5VL_iod_get_metadata(mdkv_oh, IOD_TID_UNKNOWN, H5VL_IOD_PLIST, H5VL_IOD_KEY_OBJ_CPL,
                             NULL, NULL, &output.dcpl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "failed to retrieve dcpl");

    if(H5VL_iod_get_metadata(mdkv_oh, IOD_TID_UNKNOWN, H5VL_IOD_LINK_COUNT, H5VL_IOD_KEY_OBJ_LINK_COUNT,
                             NULL, NULL, &output.link_count) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "failed to retrieve link count");

    if(H5VL_iod_get_metadata(mdkv_oh, IOD_TID_UNKNOWN, H5VL_IOD_DATATYPE, H5VL_IOD_KEY_OBJ_DATATYPE,
                             NULL, NULL, &output.type_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "failed to retrieve datatype");

    if(H5VL_iod_get_metadata(mdkv_oh, IOD_TID_UNKNOWN, H5VL_IOD_DATASPACE, H5VL_IOD_KEY_OBJ_DATASPACE,
                             NULL, NULL, &output.space_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "failed to retrieve dataspace");
#endif

    /* close the metadata scratch pad */
    if(iod_obj_close(mdkv_oh, NULL, NULL))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close object");

    {

        //hid_t space_id, type_id;

#if H5_DO_NATIVE
      /*        printf("dataset name %s    location %d\n", name, loc_handle.cookie); */
        dset_oh.cookie = H5Dopen(loc_handle.cookie, name, input->dapl_id);
        HDassert(dset_oh.cookie);
        output.space_id = H5Dget_space(dset_oh.cookie);
        output.type_id = H5Dget_type(dset_oh.cookie);
        output.dcpl_id = H5P_DATASET_CREATE_DEFAULT;
#else
        /* fake a dataspace, type, and dcpl */
        dims [0] = 60;
        output.space_id = H5Screate_simple(1, dims, NULL);
        output.type_id = H5Tcopy(H5T_NATIVE_INT);
        output.dcpl_id = H5P_DATASET_CREATE_DEFAULT;
        dset_oh.cookie = 1;
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
    output.iod_oh.cookie = dset_oh.cookie;

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
    iod_handle_t coh = input->coh; /* container handle */
    iod_handle_t iod_oh = input->iod_oh; /* dset object handle */
    iod_obj_id_t iod_id = input->iod_id; /* dset ID */
    hg_bulk_t bulk_handle = input->bulk_handle; /* bulk handle for data */
    hid_t space_id = input->space_id; /* file space selection */
    hid_t dxpl_id = input->dxpl_id; /* transfer property list */
    hid_t src_id = input->dset_type_id; /* the datatype of the dataset's element */
    hid_t dst_id = input->mem_type_id; /* the memory type of the elements */
    hg_bulk_block_t bulk_block_handle; /* HG block handle */
    hg_bulk_request_t bulk_request; /* HG request */
    iod_mem_desc_t mem_desc; /* memory descriptor used for reading array */
    iod_array_iodesc_t file_desc; /* file descriptor used to read array */
    iod_hyperslab_t *hslabs = NULL; /* IOD hyperslab generated from HDF5 filespace */
    size_t size, buf_size, src_size, dst_size;
    void *buf = NULL; /* buffer to hold outgoing data */
    uint8_t *buf_ptr = NULL;
    hssize_t num_descriptors = 0, n; /* number of IOD file descriptors needed to describe filespace selection */
    int ndims, i; /* dataset's rank/number of dimensions */
    uint32_t cs = 0; /* checksum value */
    size_t nelmts; /* number of elements selected to read */
    na_addr_t dest = HG_Handler_get_addr(op_data->hg_handle); /* destination address to push data to */
    hbool_t opened_locally = FALSE; /* flag to indicate whether we opened the dset here or if it was already open */
    iod_checksum_t *cs_list = NULL;
    iod_ret_t *ret_list = NULL;
    iod_array_io_t *io_array = NULL; /* arary for list I/O */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* open the dataset if we don't have the handle yet */
    if(iod_oh.cookie == IOD_OH_UNDEFINED) {
        if (iod_obj_open_write(coh, iod_id, NULL /*hints*/, &iod_oh, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't open current group");
        opened_locally = TRUE;
    }

    /* retrieve size of bulk data asked for to be read */
    size = HG_Bulk_handle_get_size(bulk_handle);

    /* get the number of points selected */
    nelmts = (size_t)H5Sget_select_npoints(space_id);

    /* retrieve source and destination datatype sizes for data conversion */
    src_size = H5Tget_size(src_id);
    dst_size = H5Tget_size(dst_id);

    /* adjust buffer size for datatype conversion */
    if(src_size > dst_size) {
        buf_size = src_size * nelmts;
#if H5VL_IOD_DEBUG 
        fprintf(stderr, "Adjusted Buffer size because of datatype conversion from %d to %d: \n", 
                size, buf_size);        
#endif
    }
    else {
        buf_size = dst_size * nelmts;
        if(buf_size != size)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "data size is not equal to expected size");
    }

    /* allocate buffer to hold data */
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

    /* allocate the IOD array parameters for reading */
    if(NULL == (io_array = (iod_array_io_t *)malloc
                (sizeof(iod_array_io_t) * (size_t)num_descriptors)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate iod array");

    /* allocate cs array */
    if(NULL == (cs_list = (iod_checksum_t *)calloc
                (sizeof(iod_checksum_t), (size_t)num_descriptors)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate checksum array");

    /* allocate return array */
    if(NULL == (ret_list = (iod_ret_t *)calloc
                (sizeof(iod_ret_t), (size_t)num_descriptors)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate iod array");
 
    /* Set up I/O list */
    for(n=0 ; n<num_descriptors ; n++) {
      hsize_t num_bytes = 0;
      hsize_t num_elems = 1;
      
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

        /* setup list I/O parameters */
        io_array[n].oh = iod_oh;
        io_array[n].hints = NULL;
        io_array[n].mem_desc = &mem_desc;
        io_array[n].io_desc = &file_desc;
        io_array[n].cs = &cs_list[n];
        io_array[n].ret = &ret_list[n];
    }

    /* Read list IO */
    if(iod_array_read_list(coh, IOD_TID_UNKNOWN, (iod_size_t)num_descriptors, 
                           io_array, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_READERROR, FAIL, "can't read from array object");

    /* verify return values */
    for(n=0 ; n<num_descriptors ; n++) {
        if(ret_list[n] < 0)
            HGOTO_ERROR(H5E_SYM, H5E_READERROR, FAIL, "can't read from array object");
    }

    {
        hbool_t flag = FALSE;
        int *ptr = (int *)buf;

#if H5_DO_NATIVE
        ret_value = H5Dread(iod_oh.cookie, src_id, H5S_ALL, space_id, dxpl_id, buf);
#else /* fake data */
        for(i=0;i<64;++i)
            ptr[i] = i;
#endif

        /* do data conversion */
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
    if(HG_SUCCESS != HG_Bulk_wait(bulk_request, HG_MAX_IDLE_TIME, HG_STATUS_IGNORE))
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

    if(buf)
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
    if(io_array)
        free(io_array);
    if(cs_list)
        free(cs_list);
    if(ret_list)
        free(ret_list);

    /* close the dataset if we opened it in this routine */
    if(opened_locally) {
        if(iod_obj_close(iod_oh, NULL, NULL))
            HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close Array object");
    }
    FUNC_LEAVE_NOAPI_VOID
} /* end H5VL_iod_server_dset_read_cb() */

 /*-------------------------------------------------------------------------
  * Function:	H5VL_iod_server_dset_compactor_cb
  *
  * Purpose:     Compacts the requests and calls the appropriate read/write.
  *
  * Return:	Success:	SUCCEED 
  *		Failure:	Negative
  *
  * Programmer:  Vishwanath Venkatesan
  *              July, 2013
  *
  *-------------------------------------------------------------------------
  */

 void
 H5VL_iod_server_dset_compactor_cb (AXE_engine_t UNUSED axe_engine, 
				    size_t UNUSED num_n_parents, AXE_task_t UNUSED n_parents[], 
				    size_t UNUSED num_s_parents, AXE_task_t UNUSED s_parents[], 
				    void *_queue)
 {

   compactor *cqueue = (compactor *)_queue;
   int i = 0;
   herr_t ret_value = SUCCEED;
   request_list_t *wlist=NULL, *rlist=NULL;
   dataset_container_t *dlist=NULL, *drlist=NULL;
   int nentries = 0, ndatasets = 0, nrentries = 0, nrdatasets = 0;

   FUNC_ENTER_NOAPI_NOINIT
 
#if DEBUG_COMPACTOR
     fprintf(stderr, " COMPACTOR CB: Enters Call BACK!\n");
     fprintf (stderr, "COMPACTOR CB: Number of requests : %d from call back in queue : %p\n", 
	    H5VL_iod_get_number_of_requests(cqueue), (void *)cqueue);
    fprintf(stderr, " COMPACTOR CB: Sleeping\n");
    fflush(stderr);
#endif     

#if H5_DO_NATIVE
    usleep(2000);
#endif
    

    
     while(pthread_mutex_trylock(&lock) != 0);
     compactor_queue_flag = 0;
     curr_queue = NULL;
     pthread_mutex_unlock(&lock);  

#if DEBUG_COMPACTOR     
     fprintf (stderr,"Creating WRITE request list \n");
#endif
     
     ret_value = H5VL_iod_create_request_list (cqueue,
					       &wlist,
					       &nentries,
					       &dlist,
					       &ndatasets,
					       WRITE);
     
#if DEBUG_COMPACTOR
     if (ret_value != CP_SUCCESS){
       fprintf(stderr,"ERROR !! Compactor create write request list failed with error %d  \n",
	       ret_value);
     }
#endif

     
#if DEBUG_COMPACTOR     
     fprintf (stderr,"Creating READ request list \n");
#endif
     ret_value = H5VL_iod_create_request_list (cqueue,
					       &rlist,
					       &nrentries,
					       &drlist,
					       &nrdatasets,
					       READ);

 #if DEBUG_COMPACTOR
     if (ret_value != CP_SUCCESS){
       fprintf(stderr,"ERROR !! Compactor create read request list failed with error %d  \n",
	       ret_value);
     }

     fprintf (stderr,"nrentries: %d, nrdatasets: %d \n",
	      nrentries, nrdatasets);

#endif

     

      for ( i = 0; i < ndatasets; i ++){

	H5VL_iod_compact_requests (wlist, &nentries,dlist[i].num_requests,
				  dlist[i].requests);    
      }
      
      if (nentries && nrentries){
	H5VL_iod_steal_writes (wlist, nentries,
			       rlist, &nrentries); 
      }
      
      for ( i = 0; i < ndatasets; i ++){
	if (CP_SUCCESS != H5VL_iod_server_compactor_write (wlist, nentries)){
#if DEBUG_COMPACTOR
	  fprintf (stderr,"COMPACTOR CB: compactor write failed \n");
#endif
	  HGOTO_ERROR(H5E_SYM, H5E_WRITEERROR, CP_FAIL, "Lower lever write failed\n");
	}	
      }
      
      
      
        

      for ( i = 0; i < nrdatasets; i++){

	if (CP_SUCCESS != H5VL_iod_server_compactor_read (rlist, nrentries)){
#if DEBUG_COMPACTOR
	  fprintf (stderr,"COMPACTOR CB: compactor read failed \n");
#endif
	  HGOTO_ERROR(H5E_SYM, H5E_WRITEERROR, CP_FAIL, "Lower lever read failed\n");

	}
      }
      /*Call here "iod_array_write_list"

      array_write array has been constructed. 
      The COH value can come from any of the list requests 

      Once that is done --> execute the func : H5VL_iod_server_send_result
       This sends results to all the clients about their completion.

     */

      if (CP_SUCCESS != H5VL_iod_free_memory_buffer (wlist, nentries)){
	HGOTO_ERROR(H5E_SYM, H5E_WRITEERROR, CP_FAIL, "Failed while freeing memory buffer\n");
      }
      
      if (NULL != wlist){
	free(wlist);
	wlist = NULL;
      }
      if (NULL != rlist){
	free(rlist);
	wlist = NULL;
      }
      if (NULL != dlist){
	free(dlist);
	dlist = NULL;
      }
     if (NULL != drlist){
       free(drlist);
       drlist = NULL;
     }
     
     
     if (CP_SUCCESS != H5VL_iod_destroy_compactor_queue(cqueue)){
       HGOTO_ERROR(H5E_HEAP, H5E_NOSPACE, CP_FAIL, "Cannot free NULL queue\n");
     }
     
 done:
    FUNC_LEAVE_NOAPI_VOID   
} /*end H5VL_iod_server_dset_compactor_cb */

/*------------------------------------------------------------------------
 * Function:	H5VL_iod_server_compactor_read
 *
 * Purpose:     Compactor lower lever read operations
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 * Programmer:  Vishwanath Venkatesan
 *              August, 2013
 *
 *-------------------------------------------------------------------------
 */

int H5VL_iod_server_compactor_read (void *_list, int num_requests)
{

  int ret_value = CP_SUCCESS;
  int i, request_counter= 0;
  int ndims;
  request_list_t *list = (request_list_t *)_list;
  op_data_t *op_data;
  dset_io_in_t *input;
  iod_handle_t coh, iod_oh;
  iod_obj_id_t iod_id;    
  hid_t space_id, dst_id, src_id;
  size_t size, dst_size, src_size;
  size_t nelmts; 
  void *buf = NULL;
  size_t buf_size, x;
  uint8_t *buf_ptr;
  hg_bulk_t bulk_handle;
  hssize_t num_descriptors = 0, n =0;
  hbool_t opened_locally = FALSE;
  iod_hyperslab_t *hslabs = NULL;
  iod_mem_desc_t *mem_desc = NULL;
  iod_array_iodesc_t file_desc;
  iod_array_io_t *io_array = NULL;
  iod_checksum_t *cs_list = NULL;
  iod_ret_t *ret_list = NULL;
  size_t curr_x = 0, tot_bytes = 0;
  size_t curr_len = 0;
  hsize_t curr_offset;
#if H5_DO_NATIVE
  char *read_buf = NULL, *read_buf_ptr = NULL;
  hid_t mem_dataspace;
  hsize_t *native_dims = NULL;
#endif
  FUNC_ENTER_NOAPI_NOINIT
    
  if (num_requests <= 0){
#if DEBUG_COMPACTOR
    fprintf (stderr,"COMPACTOR_READ: Request < 0 We should not be here!\n");
#endif
    ret_value = CP_FAIL;
    goto done;
  }
 
#if DEBUG_COMPACTOR
  fprintf (stderr,"Entering COMPACTOR READ with requests %d\n", num_requests);
#endif

#if H5_DO_NATIVE
  native_dims = (hsize_t *) malloc (sizeof(hsize_t));
  if (NULL == native_dims){
    HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate native_dims");
  }
#endif

  for (request_counter = 0; request_counter < num_requests; request_counter++){
    

    
    if (list[request_counter].merged == USED_IN_SS)
      continue;

#if DEBUG_COMPACTOR
    fprintf (stderr, "merged: %d\n",
	     list[request_counter].merged);
#endif

    op_data = (op_data_t *)list[request_counter].op_data;
    input = (dset_io_in_t *)op_data->input;
    coh = input->coh;
    iod_oh = input->iod_oh;
    iod_id = input->iod_id;
    bulk_handle = input->bulk_handle;
    space_id = list[request_counter].selection_id;
    dst_id = input->dset_type_id;
    src_id = input->mem_type_id;
    dst_size = H5Tget_size(dst_id);
    src_size = H5Tget_size(src_id);
    
    if (NOT_SS == list[request_counter].merged){
      buf = (void *)list[request_counter].mem_buf;
      buf_size = list[request_counter].mem_length;
    }
    /* open the dataset if we don't have the handle yet */
    if(iod_oh.cookie == (int)IOD_OH_UNDEFINED) {
      if (iod_obj_open_write(coh, iod_id, NULL /*hints*/, &iod_oh, NULL) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't open current group");
      opened_locally = TRUE;
    }
    
    /* retrieve size of bulk data asked for to be read */
    size = HG_Bulk_handle_get_size(bulk_handle);
    /* get the number of points selected */
    nelmts = (size_t)H5Sget_select_npoints(space_id);



    if (list[request_counter].merged != SS){
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
      
      if (list[request_counter].merged != SPLIT_FOR_SS)
	buf_ptr = (uint8_t *)buf;
      
      /* allocate the IOD array parameters for reading */
      if(NULL == (io_array = (iod_array_io_t *)malloc
		  (sizeof(iod_array_io_t) * (size_t)num_descriptors)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate iod array");
      
      /* allocate cs array */
      if(NULL == (cs_list = (iod_checksum_t *)calloc
		  (sizeof(iod_checksum_t), (size_t)num_descriptors)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate checksum array");
      
      /* allocate return array */
      if(NULL == (ret_list = (iod_ret_t *)calloc
		  (sizeof(iod_ret_t), (size_t)num_descriptors)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate iod array");
      
      /* Set up I/O list */
      for(n=0 ; n<num_descriptors ; n++) {
	
	hsize_t num_bytes = 0;
	hsize_t num_elems = 1;
	
	/* determine how many bytes the current descriptor holds */
	for(i=0 ; i<ndims ; i++)
	  num_elems *= (hslabs[n].count[i] * hslabs[n].block[i]);
	num_bytes = num_elems * src_size;
	tot_bytes = 0;

	if (list[request_counter].merged == SPLIT_FOR_SS){
	  mem_desc = (iod_mem_desc_t *)
	    malloc(sizeof(iod_mem_desc_t) + 
		   list[request_counter].num_mblocks *
		   sizeof(iod_mem_frag_t));
	  mem_desc->nfrag = list[request_counter].num_mblocks;
	  for ( x = curr_x; x < list[request_counter].num_mblocks;
		x++){
	    if (curr_offset){
	      mem_desc->frag[x].addr =  (void *)(uintptr_t)
		curr_offset;
	      mem_desc->frag[x].len = (iod_size_t)curr_len;
	      curr_offset = 0;
	      curr_len = 0;
	    }
	    if (x > 0 && ((tot_bytes + 
			   list[request_counter].mblocks[x].len)
			  <= num_bytes)){
	      mem_desc->frag[x].addr = (void *)(uintptr_t)
		list[request_counter].mblocks[x].offset;
	      
	      mem_desc->frag[x].len = (iod_size_t)
		list[request_counter].mblocks[x].len;
	      tot_bytes += list[request_counter].mblocks[x].len;
	    }
	    else{
	      mem_desc->frag[x].addr = (void *)(uintptr_t)
                list[request_counter].mblocks[x].offset;
	      mem_desc->frag[x].len = num_bytes - tot_bytes;
	      curr_offset = list[request_counter].mblocks[x].offset +
		mem_desc->frag[x].len;
	      curr_len  = list[request_counter].mblocks[x].len - 
		mem_desc->frag[x].len;
	      tot_bytes = num_bytes;
	      break;
	    }
	  }
	  mem_desc->nfrag = x + 1;
	}

	else{
	  mem_desc = (iod_mem_desc_t *)malloc(sizeof(iod_mem_desc_t) + sizeof(iod_mem_frag_t));
	  mem_desc->nfrag = 1;
	  mem_desc->frag[0].addr = (void *)buf_ptr;
	  mem_desc->frag[0].len = (iod_size_t)num_bytes;
	  buf_ptr += num_bytes;
	}
	/* set the file descriptor */
	file_desc = hslabs[n];
	
#if H5VL_IOD_DEBUG 
        for(i=0 ; i<ndims ; i++) {
	  fprintf(stderr, "Dim %d:  start %zu   stride %zu   block %zu   count %zu\n", 
		  i, (size_t)file_desc.start[i], (size_t)file_desc.stride[i], 
		  (size_t)file_desc.block[i], (size_t)file_desc.count[i]);
        }
#endif
        /* setup list I/O parameters */
        io_array[n].oh = iod_oh;
        io_array[n].hints = NULL;
        io_array[n].mem_desc = mem_desc;
        io_array[n].io_desc = &file_desc;
        io_array[n].cs = &cs_list[n];
        io_array[n].ret = &ret_list[n];
      }

      
      /* Read list IO */
      if(iod_array_read_list(coh, IOD_TID_UNKNOWN, (iod_size_t)num_descriptors, 
			     io_array, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_READERROR, FAIL, "can't read from array object");
      
      /* verify return values */
      for(n=0 ; n<num_descriptors ; n++) {
        if(ret_list[n] < 0)
	  HGOTO_ERROR(H5E_SYM, H5E_READERROR, FAIL, "can't read from array object");
      }
      

#if H5_DO_NATIVE
      if (list[request_counter].merged == SPLIT_FOR_SS){
	read_buf = (char *)malloc (list[request_counter].mem_length * sizeof(char));
	if (NULL  == read_buf){
	  HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "cannot allocate read_buf");
	}
	*native_dims = list[request_counter].mem_length/src_size;
      }
      else{
	read_buf = (char *)buf;
	*native_dims = size/src_size;
      }

      read_buf_ptr = read_buf;
      mem_dataspace = H5Screate_simple (1, native_dims, NULL);
      ret_value = H5Dread(iod_oh.cookie, src_id, mem_dataspace,
			  space_id, H5P_DEFAULT, read_buf);
      if (list[request_counter].merged == SPLIT_FOR_SS){
	for (x = 0; x< list[request_counter].num_mblocks; x++){
	  memcpy((void*)(uintptr_t)list[request_counter].mblocks[x].offset,
		 read_buf_ptr,
		 list[request_counter].mblocks[x].len);
	  read_buf_ptr += list[request_counter].mblocks[x].len;
	}
	if (read_buf){
	  free(read_buf);
	  read_buf = NULL;
	}
      }
      
#else 
      /* fake data */
      if (list[request_counter].merged == SPLIT_FOR_SS){
	for (x = 0; x < list[request_counter].num_mblocks; x++){
#if DEBUG_COMPACTOR
	  fprintf (stderr,"SPLIT CR: Offset: %lli\n",
		   list[request_counter].mblocks[x].offset);
#endif
	  ptr = (int *)(uintptr_t)
	    list[request_counter].mblocks[x].offset;
	  for (m = 0; 
	       m <(list[request_counter].mblocks[x].len)/sizeof(int);
	       m++){
	    ptr[m] = 1024;
	  }
	}
      }
      else{
	ptr = (int *)buf;
	for(i = 0; i <(int)(buf_size/sizeof(int));++i)
	  ptr[i] = i;
      }
#endif
      /* free allocated descriptors */
      for(n=0 ; n<num_descriptors ; n++) {
	free(hslabs[n].start);
	free(hslabs[n].stride);
	free(hslabs[n].block);
	free(hslabs[n].count);
      }
      if(hslabs)
	free(hslabs);
      if(io_array)
	free(io_array);
      if(cs_list)
	free(cs_list);
      if(ret_list)
	free(ret_list);
      if (mem_desc)
	free(mem_desc);
    }
  }
  
  if (SUCCEED != 
      H5VL_iod_server_send_result (list, num_requests, READ)){
    HGOTO_ERROR(H5E_SYM, H5E_WRITEERROR, HG_FAIL, "Error while sending result to client");
  }
  
  /* close the dataset if we opened it in this routine */
  if(opened_locally) {
    if(iod_obj_close(iod_oh, NULL, NULL))
      HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close Array object");
  }

 done:  
  FUNC_LEAVE_NOAPI(ret_value);

}/* end H5VL_iod_compactor_read */

/*------------------------------------------------------------------------
 * Function:	H5VL_iod_server_compactor_write
 *
 * Purpose:     Compactor lower lever write operations
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 * Programmer:  Vishwanath Venkatesan
 *              July, 2013
 *
 *-------------------------------------------------------------------------
 */

int H5VL_iod_server_compactor_write (void *_list, int num_requests)
{

  int ret_value = CP_SUCCESS;
  int i,  request_counter= 0;
  int *ptr = NULL;
  int ndims;
  request_list_t *list = (request_list_t *)_list;
  op_data_t *op_data;
  dset_io_in_t *input;
  iod_handle_t coh;
  iod_handle_t iod_oh;
  iod_obj_id_t iod_id;    
  hg_bulk_t bulk_handle;
  hid_t space_id, dst_id, src_id;
  size_t size, dst_size, src_size,ii = 0;
  void *buf = NULL;
  size_t hi;
  uint8_t *buf_ptr;
  hssize_t num_descriptors = 0, n =0;
  hbool_t opened_locally = FALSE;
  iod_hyperslab_t *hslabs = NULL;
  iod_mem_desc_t *mem_desc = NULL, *tmp = NULL;
  iod_array_iodesc_t file_desc;
  hsize_t num_bytes = 0, mem_reqs = 0, start_reqs = 0;
  hsize_t num_elems = 0, k = 0, j=0, curr_k = 0;
  hsize_t curr_j = 0, bytes_left = 0, curr_offset = 0;
  iod_array_io_t *io_array = NULL; /* arary for list I/O */
  iod_checksum_t *cs_list = NULL;
  iod_ret_t *ret_list = NULL;

#if H5_DO_NATIVE
  size_t native_length = 0, total_length = 0;
  char *write_buf = NULL, *write_buf_ptr = NULL;
  hid_t mem_dataspace;
  hsize_t native_dims[1];
#endif



  FUNC_ENTER_NOAPI_NOINIT
   
  if (num_requests <= 0){
#if DEBUG_COMPACTOR
    fprintf (stderr,"COMPACTOR_WRITE: Request < 0 We should not be here!\n");
#endif
    ret_value = CP_FAIL;
    goto done;
  }
   
#if DEBUG_COMPACTOR
  fprintf (stderr,"Entering COMPACTOR WRITE with requests %d\n", num_requests);
#endif
 
  for (request_counter = 0; request_counter < num_requests; request_counter++){
     
    if (list[request_counter].merged != USED_IN_MERGING){ 

      op_data = (op_data_t *)list[request_counter].op_data;
      input = (dset_io_in_t *)op_data->input;
      coh = input->coh;
      iod_oh = input->iod_oh;
      iod_id = input->iod_id;
      bulk_handle = input->bulk_handle;
      space_id = list[request_counter].selection_id;
      dst_id = input->dset_type_id;
      src_id = input->mem_type_id;
      dst_size = H5Tget_size(dst_id);
      src_size = H5Tget_size(src_id);
      
      /*When the request was used in merging we have to just
	manage the handle*/
      if (list[request_counter].merged == NOT_MERGED){
#if DEBUG_COMPACTOR
	fprintf (stderr,"COMPACTOR space_id: %d, selection_id: %d\n",
		 space_id, list[request_counter].selection_id);
#endif
	/*Even in the case its not merged the buffer was already extracted*/
	size  = list[request_counter].mem_length;
#if DEBUG_COMPACTOR
	fprintf (stderr,"COMPACTOR WRITE: Request %d has not been merged\n",
		 request_counter+1);
#endif
	buf = (void *)list[request_counter].mem_buf;

#if H5_DO_NATIVE	
	native_dims[0] = (hsize_t)(size/src_size); 
	write_buf = (char *)buf;
#endif
	buf_ptr = (uint8_t *)buf;
      }     

      if (list[request_counter].merged == MERGED){
	
#if H5_DO_NATIVE
	for (hi = 0; hi < list[request_counter].num_mblocks; hi++){
	  total_length += list[request_counter].mblocks[hi].len;
	}
	native_dims[0] = (hsize_t)(total_length/src_size); 
#endif


#if PROFILE
	fprintf (stdout,
		 "# Requests Merged: %d \n",
		 request_counter);

#endif
#if DEBUG_COMPACTOR
	/*Even in the case its not merged the buffer was already extracted*/
	fprintf (stderr,"COMPACTOR WRITE: Request %d has been merged \n", 
		 request_counter+1);
#endif
      }
      

      /*#####################################################################*/
      if(iod_oh.cookie == (int)IOD_OH_UNDEFINED) {
	if (iod_obj_open_write(coh, iod_id, NULL /*hints*/, &iod_oh, NULL) < 0)
	  HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't open current group");
	opened_locally = TRUE;
      }
      /* get the rank of the dataspace */
      if((ndims = H5Sget_simple_extent_ndims(space_id)) < 0)
	HGOTO_ERROR(H5E_INTERNAL, H5E_CANTGET, FAIL, "unable to get dataspace dimesnsion");
      
      /* get the number of decriptors required, i.e. the numbers of iod
	 I/O operations needed */
      if(H5VL_iod_get_file_desc(space_id, &num_descriptors, NULL) < 0)
	HGOTO_ERROR(H5E_DATASPACE, H5E_CANTGET, FAIL, "unable to generate IOD file descriptor from dataspace selection");
      /*#####################################################################*/
      
#if DEBUG_COMPACTOR
      fprintf (stderr,"COMPACTOR num_descriptors : %llu, ndims: %d\n", 
	       num_descriptors,
	       ndims);
#endif

      /* allocate the IOD hyperslab descriptors needed */
      if(NULL == (hslabs = (iod_hyperslab_t *)malloc
		  (sizeof(iod_hyperslab_t) * (size_t)num_descriptors)))
	HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate iod array descriptors");
      
      for (n = 0; n < num_descriptors; n++){
	hslabs[n].start = (iod_size_t *)malloc(sizeof(iod_size_t) * ndims);
	hslabs[n].stride = (iod_size_t *)malloc(sizeof(iod_size_t) * ndims);
	hslabs[n].block = (iod_size_t *)malloc(sizeof(iod_size_t) * ndims);
	hslabs[n].count = (iod_size_t *)malloc(sizeof(iod_size_t) * ndims);
      }
      
      /* generate the descriptors after allocating the array */
      if(H5VL_iod_get_file_desc(space_id, &num_descriptors, hslabs) < 0)
	HGOTO_ERROR(H5E_DATASPACE, H5E_CANTGET, FAIL, "unable to generate IOD file descriptor from dataspace selection");
      
#if DEBUG_COMPACTOR
      for (n = 0; n < num_descriptors; n++){
	fprintf (stderr,"COMPACTOR WRITE n: %llu\n", n);
	for (i = 0; i < ndims; i++){
	  fprintf (stderr,"COMPACTOR WRITE count[%d]: %llu, blocks[%d]: %llu\n",
		   i, hslabs[n].count[i], 
		   i, hslabs[n].block[i]);
	}
      }
#endif
      
      /* allocate the IOD array parameters for writing */
      if(NULL == (io_array = (iod_array_io_t *)malloc
		  (sizeof(iod_array_io_t) * (size_t)num_descriptors)))
	HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate iod array");
      
      /* allocate cs array */
      if(NULL == (cs_list = (iod_checksum_t *)calloc
		  (sizeof(iod_checksum_t), (size_t)num_descriptors)))
	HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate checksum array");
      
      /* allocate return array */
      if(NULL == (ret_list = (iod_ret_t *)calloc
		  (sizeof(iod_ret_t), (size_t)num_descriptors)))
	HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate iod array");
      
#if H5_DO_NATIVE
      if (list[request_counter].merged == MERGED){
	if (NULL == write_buf){
	  write_buf = (char *)malloc ((size_t) total_length);
	  if (NULL == write_buf){
	    HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate write_buf");
	  }
	}
      }
#endif

      curr_j = 0;
      for(n=0 ; n<num_descriptors ; n++) {
	
	num_bytes = 0;
	num_elems = 1;
	
	/* determine how many bytes the current descriptor holds */
	for(i=0 ; i<ndims ; i++)
	  num_elems *= (hslabs[n].count[i] * hslabs[n].block[i]);
	num_bytes = num_elems * dst_size;

	/*new memory descriptor for this hslab descriptor*/
	if (mem_desc != NULL)
	  mem_desc = NULL;
	
	mem_desc = (iod_mem_desc_t *) malloc (sizeof(*mem_desc) + sizeof(mem_desc->frag[0]));
	if (NULL == mem_desc){
	  HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL,"Can't allocate mem_desc \n");
	}
	
	if (list[request_counter].merged == NOT_MERGED){

#if DEBUG_COMPACTOR
	  ptr = (int *) buf;
	  fprintf(stderr,"COMPACTOR WRITE: Received a buffer of size %zd with values :",
		  size);
	  for(i=0;i< (int)size/4;++i){
	    fprintf(stderr, "COMPACTOR WRITE: buf[%d] : %d\n",
		    i, ptr[i]);
	  }
	  fprintf(stderr, "\n");
#endif
	  /* set the memory descriptor */
	  mem_desc->nfrag = 1;
	  mem_desc->frag[0].addr = (void *)buf_ptr;
	  mem_desc->frag[0].len = (iod_size_t)num_bytes;
	  file_desc = hslabs[n];
	  buf_ptr += num_bytes;
	}
	
	if (list[request_counter].merged == MERGED){

#if DEBUG_COMPACTOR
	  fprintf (stderr, "COMPACTOR WRITE i: %d, num_mblocks: %zd, num_bytes: %lli, start_reqs: %lli\n",
		   request_counter, list[request_counter].num_mblocks, num_bytes, start_reqs);
	  for ( j = curr_j; j < list[request_counter].num_mblocks; j++){
	    fprintf (stderr,"COMPACTOR WRITE j: %lli, len: %zd\n",
		     j, list[request_counter].mblocks[j].len);
	  }
#endif

	  
#if H5_DO_NATIVE
	  write_buf_ptr = write_buf;
#endif

	  k = 0;
	  mem_reqs = 0;
#if DEBUG_COMPACTOR
	  fprintf (stderr,"COMPACTOR, curr_j: %lli, j: %lli, num_mblocks: %zd\n",
		   curr_j, j, list[request_counter].num_mblocks);
#endif
	  
	  start_reqs = curr_j;
	  for (j = curr_j; j < list[request_counter].num_mblocks; j++){
	    if (k < num_bytes){
	      if (bytes_left){
		k += bytes_left;
		mem_reqs += 1;
		continue;
	      }
	      else if (k + list[request_counter].mblocks[j].len > num_bytes){
		bytes_left = list[request_counter].mblocks[j].len - (num_bytes - k );
		curr_j = j;
		curr_offset = list[request_counter].mblocks[j].offset + 
		  (list[request_counter].mblocks[j].len - bytes_left);
		mem_reqs += 1;
		break;
	      }
	      else{ /* <= case*/
		k += list[request_counter].mblocks[j].len;
		curr_j = j;
		mem_reqs += 1;
	      }
	    }
	  }
	  curr_j += 1;
	  /*Determined the number of entries in the memory block for this hslab
	    descriptor*/
	  
	  tmp = (iod_mem_desc_t *) realloc
	    (mem_desc, sizeof(*tmp) + (size_t)(mem_reqs * sizeof(mem_desc->frag[0])));
	  
	  if (tmp){
	    mem_desc = tmp;
	    mem_desc->nfrag = mem_reqs;
	    tmp = NULL;
	  }
	  
	  k  = 0;
	  for ( j = start_reqs; j < (start_reqs + mem_reqs); j++){
	    if ((j == curr_j) && (bytes_left)){
	      mem_desc->frag[k].addr = (void *)(uintptr_t)(curr_offset);
	      mem_desc->frag[k].len = bytes_left;
	    }
	    else{
	      mem_desc->frag[k].addr =  (void *)(uintptr_t)
		(list[request_counter].mblocks[j].offset);
	      mem_desc->frag[k].len  = list[request_counter].mblocks[j].len;
	    }

#if H5_DO_NATIVE
	    write_buf_ptr = write_buf + native_length;   
	    memcpy ( (void *)write_buf_ptr, mem_desc->frag[k].addr,
		     (size_t)mem_desc->frag[k].len);
	    native_length += mem_desc->frag[k].len;
#endif
	    k++;
	  }
	  curr_k += k;
  
#if H5_DO_NATIVE
#if DEBUG_COMPACTOR
	  fprintf (stderr, "Printing the native constructed buffer of length :%zd\n", 
		   native_length);
	  ptr = (hsize_t *) write_buf;
	  for (ii = 0; ii < native_length/sizeof(hsize_t); ii++){
	    fprintf(stderr, "%d ", ptr[ii]);
	  }
	  fprintf(stderr, "\n");
#endif
#endif

#if DEBUG_COMPACTOR
	  for ( j = 0 ; j <  k; j++){
	    ptr = (int *)(mem_desc->frag[j].addr);
	    fprintf(stderr, "COMPACTOR MERGED WRITE IOD_BUFFER j: %lli, k: %lli  len: %llu\n",
		    j,k, mem_desc->frag[j].len);
	    for (ii = 0; ii < mem_desc->frag[j].len/sizeof(int); ii++)
	      fprintf(stderr, "%d ", ptr[ii]);
	    fprintf(stderr, "\n");
	  }
	  fflush(stderr);
#endif
	}
	
	file_desc = hslabs[n];
      
#if H5VL_IOD_DEBUG 
	for(i=0 ; i<ndims ; i++) {
	  fprintf(stderr, "Dim %d:  start %zu   stride %zu   block %zu   count %zu\n", 
		  i, (size_t)file_desc.start[i], (size_t)file_desc.stride[i], 
		  (size_t)file_desc.block[i], (size_t)file_desc.count[i]);
	}
#endif	 
	io_array[n].oh = iod_oh;
	io_array[n].hints = NULL;
	io_array[n].mem_desc = mem_desc;
	io_array[n].io_desc = &file_desc;
	io_array[n].cs = &cs_list[n];
	io_array[n].ret = &ret_list[n];
      }

#if (!H5_DO_NATIVE)
      if(iod_array_write_list(coh, IOD_TID_UNKNOWN, (iod_size_t)num_descriptors, 
			      io_array, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_READERROR, FAIL, "can't write array object");
#endif
      
#if H5_DO_NATIVE
      mem_dataspace = H5Screate_simple(1, native_dims, NULL);
      ret_value = H5Dwrite(iod_oh.cookie,dst_id, mem_dataspace, space_id, H5P_DEFAULT, 
			   write_buf);

      if (NULL != write_buf){
	free(write_buf);
	write_buf = NULL;
      }
#endif
      
      if (NULL != mem_desc){
	free(mem_desc);
	mem_desc = NULL;
      }
      
      if (NULL != hslabs){
	for(n=0 ; n<num_descriptors ; n++){
	  free(hslabs[n].start);
	  free(hslabs[n].stride);
	  free(hslabs[n].block);
	  free(hslabs[n].count);
	}
	free(hslabs);
      }
      if(io_array)
	free(io_array);
      if(cs_list)
	free(cs_list);
      if(ret_list)
	free(ret_list);
      /* write from array object */
      /* TODO: VV Change checksum once that is fixed*/           
      if(opened_locally) {
	if(iod_obj_close(iod_oh, NULL, NULL))
	  HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close Array object");
      }
    }
  }

  for (i = 0; i< num_requests; i++){
    if (list[i].merged != MERGED){
      op_data = (op_data_t *)list[i].op_data;
      input = (dset_io_in_t *)op_data->input;
      if(HG_SUCCESS != HG_Handler_start_output(op_data->hg_handle, &ret_value))
	HDONE_ERROR(H5E_SYM, H5E_WRITEERROR, FAIL, "can't send result of write to client");
      input = (dset_io_in_t *)H5MM_xfree(input);
      op_data = (op_data_t *)H5MM_xfree(op_data);
    }
  }
  
 done:
  FUNC_LEAVE_NOAPI(ret_value);
  
}/*H5VL_iod_server_compactor_write*/


/*------------------------------------------------------------------------
 * Function:	H5VL_iod_server_send_result
 *
 * Purpose:     Send output back using the HG Handle. 
 *              This is needed as we tend to perform reads/write
 *              cummulatively.
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 * Programmer:  Vishwanath Venkatesan
 *              July, 2013
 *
 *-------------------------------------------------------------------------
 */



int H5VL_iod_server_send_result (void *_list,
				 int num_requests, 
				 int type){
  

  int i;
  int ret_value = SUCCEED;
  op_data_t *op_data = NULL;
  dset_io_in_t *input = NULL;
  request_list_t *list = (request_list_t *)_list;
  hid_t src_id, dst_id, dxpl_id;
  void *buf = NULL;
  uint32_t cs = 0;
  size_t size;
  hbool_t flag;
  hg_bulk_t bulk_handle;
  hg_bulk_block_t bulk_block_handle;
  hg_bulk_request_t bulk_request;
  na_addr_t dest;
  dset_read_out_t output;
  size_t nelmts; 
  int *ptr = NULL;
 
  FUNC_ENTER_NOAPI(NULL);
  if (type == WRITE){
    for (i = 0; i< num_requests; i++){
      if (list[i].merged != MERGED){
	op_data = (op_data_t *)list[i].op_data;
	input = (dset_io_in_t *)op_data->input;
	if(HG_SUCCESS != HG_Handler_start_output(op_data->hg_handle, &ret_value))
	  HDONE_ERROR(H5E_SYM, H5E_WRITEERROR, HG_FAIL, "can't send result of write to client");
	input = (dset_io_in_t *)H5MM_xfree(input);
	op_data = (op_data_t *)H5MM_xfree(op_data);
      }
    }
  }

  else{
    for (i = 0; i< num_requests; i++){

      if (list[i].merged == NOT_SS || list[i].merged == USED_IN_SS){
#if DEBUG_COMPACTOR
	fprintf (stderr,
		 "Enters send server result\n");
#endif
	op_data = (op_data_t *)list[i].op_data;
	input = (dset_io_in_t *)op_data->input;
	bulk_handle = input->bulk_handle;
	buf = (void *)list[i].mem_buf;
	dxpl_id = input->dxpl_id;
	dst_id = input->dset_type_id;
	src_id = input->mem_type_id;
	size = HG_Bulk_handle_get_size(bulk_handle);
	nelmts = (size_t)H5Sget_select_npoints(list[i].selection_id);
	flag = FALSE;
	ptr = (int *) buf;

#if DEBUG_COMPACTOR
	fprintf (stderr, "in %s:%d merged : %d, size: %zd\n",
		 __FILE__,__LINE__,list[i].merged, 
		 size);
#endif

	if(H5Tconvert(src_id, dst_id, nelmts, buf, NULL, dxpl_id) < 0)
	  HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "data type conversion failed");
	/* calculate a checksum for the data to be sent */
	cs = H5checksum(buf, size, NULL);
	if(dxpl_id != H5P_DEFAULT && H5Pget_dxpl_inject_corruption(dxpl_id, &flag) < 0)
	  HGOTO_ERROR(H5E_SYM, H5E_READERROR, FAIL, "can't read property list");
	if(flag) {
	  fprintf(stderr, "Injecting a bad data value to cause corruption \n");
	  ptr[0] = 10;
	}

	/* Create a new block handle to write the data */
	HG_Bulk_block_handle_create(buf, size, HG_BULK_READ_ONLY, &bulk_block_handle);
	dest = HG_Handler_get_addr (op_data->hg_handle);
	if(HG_SUCCESS != HG_Bulk_write_all(dest, bulk_handle, bulk_block_handle, &bulk_request))
	  HGOTO_ERROR(H5E_SYM, H5E_READERROR, FAIL, "can't read from array object");
	/* wait for it to complete */
	if(HG_SUCCESS != HG_Bulk_wait(bulk_request, HG_MAX_IDLE_TIME, HG_STATUS_IGNORE))
	  HGOTO_ERROR(H5E_SYM, H5E_READERROR, FAIL, "can't read from array object");
	
	output.ret = ret_value;
	output.cs = cs;

	if(HG_SUCCESS != HG_Handler_start_output(op_data->hg_handle, &output))
	  HDONE_ERROR(H5E_SYM, H5E_WRITEERROR, FAIL, "can't send result of write to client");
	if(HG_SUCCESS != HG_Bulk_block_handle_free(bulk_block_handle))
	  HDONE_ERROR(H5E_SYM, H5E_WRITEERROR, FAIL, "can't free bds block handle");

	input = (dset_io_in_t *)H5MM_xfree(input);
	op_data = (op_data_t *)H5MM_xfree(op_data);
	
	if(buf)
	  free(buf);
      }
    }
  }
  
 done:
  FUNC_LEAVE_NOAPI(ret_value);

}



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
    iod_handle_t coh = input->coh; /* container handle */
    iod_handle_t iod_oh = input->iod_oh; /* dset object handle */
    iod_obj_id_t iod_id = input->iod_id; /* dset ID */
    hg_bulk_t bulk_handle = input->bulk_handle; /* bulk handle for data */
    hid_t space_id = input->space_id; /* file space selection */
    hid_t dxpl_id = input->dxpl_id; /* transfer property list */
    uint32_t cs = input->checksum; /* checksum recieved for data */
    hid_t src_id = input->mem_type_id; /* the memory type of the elements */
    hid_t dst_id = input->dset_type_id; /* the datatype of the dataset's element */
    hg_bulk_block_t bulk_block_handle; /* HG block handle */
    hg_bulk_request_t bulk_request; /* HG request */
    iod_mem_desc_t mem_desc; /* memory descriptor used for writing array */
    iod_array_iodesc_t file_desc; /* file descriptor used to write array */
    iod_hyperslab_t *hslabs = NULL; /* IOD hyperslab generated from HDF5 filespace */
    size_t size, buf_size, src_size, dst_size;
    uint32_t data_cs = 0;
    hssize_t num_descriptors = 0, n; /* number of IOD file descriptors needed to describe filespace selection */
    int ndims, i; /* dataset's rank/number of dimensions */
    void *buf = NULL;
    uint8_t *buf_ptr = NULL;
    size_t nelmts; /* number of elements selected to write */
    hbool_t flag = FALSE; /* temp flag to indicate whether corruption will be inserted */
    na_addr_t source = HG_Handler_get_addr(op_data->hg_handle); /* source address to pull data from */
    hbool_t opened_locally = FALSE; /* flag to indicate whether we opened the dset here or if it was already open */
    iod_checksum_t *cs_list = NULL;
    iod_ret_t *ret_list = NULL;
    iod_array_io_t *io_array = NULL; /* arary for list I/O */
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

    /* retrieve size of incoming bulk data */
    size = HG_Bulk_handle_get_size(bulk_handle);

    /* get the number of points selected */
    nelmts = (size_t)H5Sget_select_npoints(space_id);

    /* retrieve source and destination datatype sizes for data conversion */
    src_size = H5Tget_size(src_id);
    dst_size = H5Tget_size(dst_id);

    /* adjust buffer size for data conversion */
    if(src_size < dst_size) {
        buf_size = dst_size * nelmts;
#if H5VL_IOD_DEBUG 
        fprintf(stderr, "Adjusted Buffer size because of datatype conversion from %d to %d\n", 
                size, buf_size);
#endif
    }
    else {
        buf_size = src_size * nelmts;
        if(buf_size != size)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Incoming data size is not equal to expected size");
    }

    /* allocate buffer to hold data */
    if(NULL == (buf = malloc(buf_size)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate read buffer");

    /* create a Mercury block handle for transfer */
    HG_Bulk_block_handle_create(buf, size, HG_BULK_READWRITE, &bulk_block_handle);

    /* Write bulk data here and wait for the data to be there  */
    if(HG_SUCCESS != HG_Bulk_read_all(source, bulk_handle, bulk_block_handle, &bulk_request))
        HGOTO_ERROR(H5E_SYM, H5E_WRITEERROR, FAIL, "can't get data from function shipper");
    /* wait for it to complete */
    if(HG_SUCCESS != HG_Bulk_wait(bulk_request, HG_MAX_IDLE_TIME, HG_STATUS_IGNORE))
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
        cs = H5checksum(buf, size, NULL);
        if(cs != data_cs) {
            fprintf(stderr, "Errrr.. Network transfer Data corruption. expecting %u, got %u\n",
                    data_cs, cs);
            HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "Checksum verification failed");
        }
    }

    /* convert data if needed */
    if(H5Tconvert(src_id, dst_id, nelmts, buf, NULL, dxpl_id) < 0)
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "data type conversion failed")

#if H5VL_IOD_DEBUG 
    { 
        int *ptr = (int *)buf;

        fprintf(stderr, "DWRITE Received a buffer of size %d with values: ", size);
        for(u=0 ; u<size/sizeof(int) ; ++u)
            fprintf(stderr, "%d ", ptr[u]);
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

    /* allocate the IOD array parameters for writing */
    if(NULL == (io_array = (iod_array_io_t *)malloc
                (sizeof(iod_array_io_t) * (size_t)num_descriptors)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate iod array");

    /* allocate cs array */
    if(NULL == (cs_list = (iod_checksum_t *)calloc
                (sizeof(iod_checksum_t), (size_t)num_descriptors)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate checksum array");

    /* allocate return array */
    if(NULL == (ret_list = (iod_ret_t *)calloc
                (sizeof(iod_ret_t), (size_t)num_descriptors)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate iod array");

    /* write each descriptore to the IOD container */
    for(n=0 ; n<num_descriptors ; n++) {
        hsize_t num_bytes = 0;
        hsize_t num_elems = 1;

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

        /* setup list I/O parameters */
        io_array[n].oh = iod_oh;
        io_array[n].hints = NULL;
        io_array[n].mem_desc = &mem_desc;
        io_array[n].io_desc = &file_desc;
        io_array[n].cs = &cs_list[n];
        io_array[n].ret = &ret_list[n];
    }

    /* Write list IO */
    if(iod_array_read_list(coh, IOD_TID_UNKNOWN, (iod_size_t)num_descriptors, 
                           io_array, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_READERROR, FAIL, "can't read from array object");

    /* verify return values */
    for(n=0 ; n<num_descriptors ; n++) {
        if(ret_list[n] < 0)
            HGOTO_ERROR(H5E_SYM, H5E_READERROR, FAIL, "can't read from array object");
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

    if(buf)
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
    if(io_array)
        free(io_array);
    if(cs_list)
        free(cs_list);
    if(ret_list)
        free(ret_list);

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
    /* int rank = input->dims.rank;  rank of dataset */
    hbool_t opened_locally = FALSE;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

#if H5VL_IOD_DEBUG 
        fprintf(stderr, "Start dataset Extend\n");
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
    //iod_obj_id_t iod_id = input->iod_id; 
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
        /* MSC - need a way to kill object handle for this dataset */
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
