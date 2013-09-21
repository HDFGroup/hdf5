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
 * Purpose:	The IOD plugin server side attribute routines.
 */


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
void
H5VL_iod_server_attr_create_cb(AXE_engine_t UNUSED axe_engine, 
                               size_t UNUSED num_n_parents, AXE_task_t UNUSED n_parents[], 
                               size_t UNUSED num_s_parents, AXE_task_t UNUSED s_parents[], 
                               void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    attr_create_in_t *input = (attr_create_in_t *)op_data->input;
    attr_create_out_t output;
    iod_handle_t coh = input->coh; /* container handle */
    iod_handle_t loc_handle = input->loc_oh; /* location handle to start lookup */
    iod_obj_id_t loc_id = input->loc_id; /* The ID of the current location object */
    iod_obj_id_t loc_attrkv_id = input->loc_attrkv_id; /* The ID of the attribute KV of the location object*/
    iod_obj_id_t attr_id = input->attr_id; /* The ID of the attribute that needs to be created */
    iod_obj_id_t mdkv_id = input->mdkv_id; /* The ID of the metadata KV to be created */
    iod_trans_id_t wtid = input->trans_num;
    iod_trans_id_t rtid = input->rcxt_num;
    uint32_t cs_scope = input->cs_scope;
    iod_handle_t attr_oh, attr_kv_oh, obj_oh, mdkv_oh; /* object handles */
    iod_obj_id_t obj_id;
    const char *loc_name = input->path; /* path to start hierarchy traversal */
    const char *attr_name = input->attr_name; /* attribute's name */
    iod_array_struct_t array; /* IOD array structure for attribute's creation */
    iod_size_t *max_dims; /* MAX dims for IOD */
    scratch_pad sp;
    iod_checksum_t sp_cs = 0;
    iod_ret_t ret;
    hbool_t collective = FALSE; /* MSC - change when we allow for collective */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Start attribute Create %s on object path %s\n", attr_name, loc_name);
#endif

    /* Open the object where the attribute needs to be created. */
    if(H5VL_iod_server_open_path(coh, loc_id, loc_handle, loc_name, rtid, &obj_id, &obj_oh) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't open object");

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

    /* create the attribute */
    ret = iod_obj_create(coh, wtid, NULL/*hints*/, IOD_OBJ_ARRAY, NULL, &array,
                         &attr_id, NULL /*event*/);
    if(collective && (0 == ret || EEXISTS == ret)) {
        /* Attribute has been created by another process, open it */
        if (iod_obj_open_write(coh, attr_id, NULL, &attr_oh, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't open attribute");
    }
    else if(!collective && 0 != ret) {
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't create Attribute");
    }

    /* for the process that succeeded in creating the attribute, update
       the parent scratch pad, create attribute scratch pad */
    if(0 == ret) {
        /* create the metadata KV object for the attribute */
        if(iod_obj_create(coh, wtid, NULL, IOD_OBJ_KV, 
                          NULL, NULL, &mdkv_id, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't create metadata KV object");

        /* set values for the scratch pad object */
        sp[0] = mdkv_id;
        sp[1] = IOD_ID_UNDEFINED;
        sp[2] = IOD_ID_UNDEFINED;
        sp[3] = IOD_ID_UNDEFINED;

        /* set scratch pad in attribute */
        if(cs_scope & H5_CHECKSUM_IOD) {
            sp_cs = H5checksum(&sp, sizeof(sp), NULL);
            if (iod_obj_set_scratch(attr_oh, wtid, &sp, &sp_cs, NULL) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't set scratch pad");
        }
        else {
            if (iod_obj_set_scratch(attr_oh, wtid, &sp, NULL, NULL) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't set scratch pad");
        }

        /* Open Metadata KV object for write */
        if (iod_obj_open_write(coh, mdkv_id, NULL, &mdkv_oh, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't create scratch pad");

        /* insert object type metadata */
        if(H5VL_iod_insert_object_type(mdkv_oh, wtid, H5I_ATTR, 
                                       NULL, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't insert KV value");

        /* MSC - need to check size of datatype if it fits in
           entry otherwise create a BLOB*/
        /* insert datatype metadata */
        if(H5VL_iod_insert_datatype(mdkv_oh, wtid, input->type_id, 
                                    NULL, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't insert KV value");

        /* insert dataspace metadata */
        if(H5VL_iod_insert_dataspace(mdkv_oh, wtid, input->space_id, 
                                     NULL, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't insert KV value");

        /* close the Metadata KV object */
        if(iod_obj_close(mdkv_oh, NULL, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close object");

        /* if the starting location is not the last component, need to
           read the attrkv_id of the last object where attribute needs
           to be created */
        if(loc_id != obj_id) {
            /* get scratch pad of the parent */
            if(iod_obj_get_scratch(obj_oh, rtid, &sp, &sp_cs, NULL) < 0)
                HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, FAIL, "can't get scratch pad for object");

            if(sp_cs && (cs_scope & H5_CHECKSUM_IOD)) {
                /* verify scratch pad integrity */
                if(H5VL_iod_verify_scratch_pad(sp, sp_cs) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Scratch Pad failed integrity check");
            }

            /* open the attribute KV in scratch pad */
            if (iod_obj_open_write(coh, sp[1], NULL /*hints*/, &attr_kv_oh, NULL) < 0)
                HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, FAIL, "can't open scratch pad");
        }
        else {
            /* open the attribute KV */
            if (iod_obj_open_write(coh, loc_attrkv_id, NULL /*hints*/, &attr_kv_oh, NULL) < 0)
                HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, FAIL, "can't open scratch pad");
        }

        /* insert new attribute in scratch pad of current object */
        if(H5VL_iod_insert_new_link(attr_kv_oh, wtid, attr_name, 
                                    H5L_TYPE_HARD, &attr_id, NULL, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't insert KV value");

        /* close the Attribute KV object */
        if(iod_obj_close(attr_kv_oh, NULL, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close object");
    }

    /* close parent group if it is not the location we started the
       traversal into */
    if(loc_handle.cookie != obj_oh.cookie) {
        iod_obj_close(obj_oh, NULL, NULL);
    }

#if H5_DO_NATIVE
    obj_oh.cookie = H5Acreate2(obj_oh.cookie, attr_name, input->type_id, 
                               input->space_id, H5P_DEFAULT, H5P_DEFAULT);
    HDassert(obj_oh.cookie);
#endif

    output.iod_oh = obj_oh;

    /* MSC - fake so not to fail */
    output.iod_oh.cookie = 10;

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Done with attr create, sending response to client\n");
#endif

    HG_Handler_start_output(op_data->hg_handle, &output);

done:

    /* return an UNDEFINED oh to the client if the operation failed */
    if(ret_value < 0) {
        fprintf(stderr, "Failed Attribute Create\n");
        output.iod_oh.cookie = IOD_OH_UNDEFINED;
        HG_Handler_start_output(op_data->hg_handle, &output);
    }

    if(max_dims) free(max_dims);
    if(array.current_dims) free(array.current_dims);
    input = (attr_create_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

    FUNC_LEAVE_NOAPI_VOID
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
void
H5VL_iod_server_attr_open_cb(AXE_engine_t UNUSED axe_engine, 
                             size_t UNUSED num_n_parents, AXE_task_t UNUSED n_parents[], 
                             size_t UNUSED num_s_parents, AXE_task_t UNUSED s_parents[], 
                             void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    attr_open_in_t *input = (attr_open_in_t *)op_data->input;
    attr_open_out_t output;
    iod_handle_t coh = input->coh; /* container handle */
    iod_handle_t loc_handle = input->loc_oh; /* location handle to start traversal */
    iod_obj_id_t loc_id = input->loc_id; /* location ID */
    iod_obj_id_t loc_attrkv_id = input->loc_attrkv_id; /* The ID of the attribute KV of the location object*/
    iod_trans_id_t rtid = input->rcxt_num;
    uint32_t cs_scope = input->cs_scope;
    iod_handle_t attr_kv_oh, attr_oh, obj_oh, mdkv_oh;
    iod_obj_id_t obj_id;
    iod_obj_id_t attr_id;
    const char *loc_name = input->path; /* current  path to start traversal */
    const char *attr_name = input->attr_name; /* attribute's name to open */
    scratch_pad sp;
    iod_checksum_t sp_cs = 0;
    H5VL_iod_link_t iod_link;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Start attribute Open %s\n", attr_name);
#endif

    /* Open the object where the attribute needs to be opened. */
    if(H5VL_iod_server_open_path(coh, loc_id, loc_handle, loc_name, 
                                 rtid, &obj_id, &obj_oh) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't open object");

    /* if the starting location is not the last component, need to
       read the attrkv_id of the last object where attribute needs
       to be created */
    if(loc_id != obj_id) {
        /* get scratch pad of the object */
        if(iod_obj_get_scratch(obj_oh, rtid, &sp, &sp_cs, NULL) < 0)
            HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, FAIL, "can't get scratch pad for object");

        if(sp_cs && (cs_scope & H5_CHECKSUM_IOD)) {
            /* verify scratch pad integrity */
            if(H5VL_iod_verify_scratch_pad(sp, sp_cs) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Scratch Pad failed integrity check");
        }

        /* MSC - Dont do this check until we have a real IOD */
#if 0
        /* if attribute KV does not exist, return error*/
        if(IOD_ID_UNDEFINED == sp[1])
            HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, FAIL, "Object has no attributes");
#endif

        /* open the attribute KV in scratch pad */
        if (iod_obj_open_write(coh, sp[1], NULL /*hints*/, &attr_kv_oh, NULL) < 0)
            HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, FAIL, "can't open scratch pad");
    }
    else {
        /* open the attribute KV */
        if (iod_obj_open_write(coh, loc_attrkv_id, NULL /*hints*/, &attr_kv_oh, NULL) < 0)
            HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, FAIL, "can't open scratch pad");
    }

    /* get attribute ID */
    if(H5VL_iod_get_metadata(attr_kv_oh, rtid, H5VL_IOD_LINK, 
                             attr_name, NULL, NULL, &iod_link) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't retrieve Attribute ID from parent KV store");

    HDassert(iod_link.link_type == H5L_TYPE_HARD);
    attr_id = iod_link.u.iod_id;

    /* close parent group if it is not the location we started the
       traversal into */
    if(loc_handle.cookie != obj_oh.cookie) {
        iod_obj_close(obj_oh, NULL, NULL);
    }
    /* close the attribute KV holder */
    iod_obj_close(attr_kv_oh, NULL, NULL);

    /* open the attribute */
    if (iod_obj_open_write(coh, attr_id, NULL /*hints*/, &attr_oh, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't open current group");

    /* get scratch pad of the attribute */
    if(iod_obj_get_scratch(attr_oh, rtid, &sp, &sp_cs, NULL) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, FAIL, "can't get scratch pad for object");

    if(sp_cs && (cs_scope & H5_CHECKSUM_IOD)) {
        /* verify scratch pad integrity */
        if(H5VL_iod_verify_scratch_pad(sp, sp_cs) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Scratch Pad failed integrity check");
    }

    /* open the metadata scratch pad of the attribute */
    if (iod_obj_open_write(coh, sp[0], NULL /*hints*/, &mdkv_oh, NULL) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "can't open scratch pad");

#if 0
    if(H5VL_iod_get_metadata(mdkv_oh, rtid, H5VL_IOD_DATATYPE, H5VL_IOD_KEY_OBJ_DATATYPE,
                             NULL, NULL, &output.type_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "failed to retrieve datatype");

    if(H5VL_iod_get_metadata(mdkv_oh, rtid, H5VL_IOD_DATASPACE, H5VL_IOD_KEY_OBJ_DATASPACE,
                             NULL, NULL, &output.space_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "failed to retrieve dataspace");
#endif

    /* close the metadata scratch pad */
    if(iod_obj_close(mdkv_oh, NULL, NULL))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close object");

    {
        hsize_t dims[1];
        //hid_t space_id, type_id;

#if H5_DO_NATIVE
        printf("attr name %s  location %d %s\n", attr_name, loc_handle.cookie, loc_name);
        if(strcmp(loc_name, ".") == 0)
            attr_oh.cookie = H5Aopen(loc_handle.cookie, attr_name, H5P_DEFAULT);
        else
            attr_oh.cookie = H5Aopen_by_name(loc_handle.cookie, loc_name, 
                                            attr_name, H5P_DEFAULT, H5P_DEFAULT);
        HDassert(attr_oh.cookie);
        output.space_id = H5Aget_space(attr_oh.cookie);
        output.type_id = H5Aget_type(attr_oh.cookie);
        output.acpl_id = H5P_ATTRIBUTE_CREATE_DEFAULT;
#else
        /* fake a dataspace, type, and dcpl */
        dims [0] = 60;
        output.space_id = H5Screate_simple(1, dims, NULL);
        output.type_id = H5Tcopy(H5T_NATIVE_INT);
        output.acpl_id = H5P_ATTRIBUTE_CREATE_DEFAULT;
#endif
    }

    output.iod_id = attr_id;
    output.mdkv_id = sp[0];
    output.iod_oh = attr_oh;

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Done with attr open, sending response to client\n");
#endif

    HG_Handler_start_output(op_data->hg_handle, &output);

done:
    if(ret_value < 0) {
        output.iod_oh.cookie = IOD_OH_UNDEFINED;
        output.iod_id = IOD_ID_UNDEFINED;
        HG_Handler_start_output(op_data->hg_handle, &output);
    }

    H5Sclose(output.space_id);
    H5Tclose(output.type_id);

    input = (attr_open_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

    FUNC_LEAVE_NOAPI_VOID
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
void
H5VL_iod_server_attr_read_cb(AXE_engine_t UNUSED axe_engine, 
                             size_t UNUSED num_n_parents, AXE_task_t UNUSED n_parents[], 
                             size_t UNUSED num_s_parents, AXE_task_t UNUSED s_parents[], 
                             void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    attr_io_in_t *input = (attr_io_in_t *)op_data->input;
    iod_handle_t coh = input->coh; /* container handle */
    iod_handle_t iod_oh = input->iod_oh; /* attribute's object handle */
    iod_obj_id_t iod_id = input->iod_id; /* attribute's ID */
    iod_obj_id_t mdkv_id = input->mdkv_id; /* The ID of the metadata KV */
    hg_bulk_t bulk_handle = input->bulk_handle; /* bulk handle for data */
    hid_t type_id = input->type_id; /* datatype ID of data */
    hid_t space_id = input->space_id; /* dataspace of attribute */
    iod_trans_id_t rtid = input->rcxt_num;
    uint32_t cs_scope = input->cs_scope;
    hg_bulk_block_t bulk_block_handle; /* HG block handle */
    hg_bulk_request_t bulk_request; /* HG request */
    iod_mem_desc_t *mem_desc = NULL; /* memory descriptor used for reading array */
    iod_array_iodesc_t file_desc; /* file descriptor used to read array */
    iod_hyperslab_t hslabs; /* IOD hyperslab generated from HDF5 filespace */
    size_t size; /* size of outgoing bulk data */
    void *buf; /* buffer to hold outgoing data */
    iod_handle_t mdkv_oh; /* metadata KV handle of attribute */
    iod_checksum_t iod_cs = 0, attr_cs = 0;
    int ndims; /* dataset's rank/number of dimensions */
    hssize_t num_descriptors = 0; /* number of IOD file descriptors needed to describe filespace selection */
    na_addr_t dest = HG_Handler_get_addr(op_data->hg_handle); /* destination address to push data to */
    hbool_t opened_locally = FALSE; /* flag to indicate whether we opened the attribute here or if it was already open */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* open the attribute if we don't have the handle yet */
    if(iod_oh.cookie == IOD_OH_UNDEFINED) {
        if (iod_obj_open_write(coh, iod_id, NULL /*hints*/, &iod_oh, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't open current group");        
        opened_locally = TRUE;
    }

    size = HG_Bulk_handle_get_size(bulk_handle);

    if(NULL == (buf = malloc(size)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate read buffer");

    /* MSC - Not needed if dataspace is available */
#if 0
    /* open the metadata scratch pad of the attribute */
    if (iod_obj_open_read(coh, mdkv_id, NULL /*hints*/, &mdkv_oh, NULL) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "can't open scratch pad");

    if(H5VL_iod_get_metadata(mdkv_oh, rtid, H5VL_IOD_DATASPACE, H5VL_IOD_KEY_OBJ_DATASPACE,
                             NULL, NULL, &space_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "failed to retrieve dataspace");

    /* close the metadata scratch pad */
    if(iod_obj_close(mdkv_oh, NULL, NULL))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close object");
#endif

    /* set the memory descriptor */
    mem_desc = (iod_mem_desc_t *)malloc(sizeof(iod_mem_desc_t) + sizeof(iod_mem_frag_t));
    mem_desc->nfrag = 1;
    mem_desc->frag[0].addr = buf;
    mem_desc->frag[0].len = (iod_size_t)size;

    num_descriptors = 1;

    /* get the rank of the dataspace */
    if((ndims = H5Sget_simple_extent_ndims(space_id)) < 0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTGET, FAIL, "unable to get dataspace dimesnsion");

    hslabs.start = (iod_size_t *)malloc(sizeof(iod_size_t) * ndims);
    hslabs.stride = (iod_size_t *)malloc(sizeof(iod_size_t) * ndims);
    hslabs.block = (iod_size_t *)malloc(sizeof(iod_size_t) * ndims);
    hslabs.count = (iod_size_t *)malloc(sizeof(iod_size_t) * ndims);

    /* generate the descriptor */
    if(H5VL_iod_get_file_desc(space_id, &num_descriptors, &hslabs) < 0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTGET, FAIL, "unable to generate IOD file descriptor from dataspace selection");

    /* set the file descriptor */
    file_desc = hslabs;

    /* read from array object */
    if(iod_array_read(iod_oh, rtid, NULL, mem_desc, &file_desc, &iod_cs, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_READERROR, FAIL, "can't read from array object");

    /* MSC - NEED IOD */
#if 0
    attr_cs = H5checksum(buf, size, NULL);
    if(attr_cs != iod_cs)
        HGOTO_ERROR(H5E_SYM, H5E_READERROR, FAIL, "Data corruption detected when reading attribute");
#endif

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
    if(HG_SUCCESS != HG_Bulk_write_all(dest, bulk_handle, bulk_block_handle, &bulk_request))
        HGOTO_ERROR(H5E_SYM, H5E_READERROR, FAIL, "can't read from array object");
    /* wait for it to complete */
    if(HG_SUCCESS != HG_Bulk_wait(bulk_request, HG_MAX_IDLE_TIME, HG_STATUS_IGNORE))
        HGOTO_ERROR(H5E_SYM, H5E_READERROR, FAIL, "can't read from array object");

done:
#if H5VL_IOD_DEBUG
    fprintf(stderr, "Done with attr read, sending response to client\n");
#endif

    if(HG_SUCCESS != HG_Handler_start_output(op_data->hg_handle, &ret_value))
        HDONE_ERROR(H5E_SYM, H5E_WRITEERROR, FAIL, "can't send result of write to client");
    if(HG_SUCCESS != HG_Bulk_block_handle_free(bulk_block_handle))
        HDONE_ERROR(H5E_SYM, H5E_WRITEERROR, FAIL, "can't free bds block handle");

    input = (attr_io_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

    free(buf);

    /* free allocated descriptors */
    free(hslabs.start);
    free(hslabs.stride);
    free(hslabs.block);
    free(hslabs.count);
    if(mem_desc)
        free(mem_desc);

    /* close the attribute if we opened it in this routine */
    if(opened_locally) {
        if(iod_obj_close(iod_oh, NULL, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close Array object");
    }

    FUNC_LEAVE_NOAPI_VOID
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
void
H5VL_iod_server_attr_write_cb(AXE_engine_t UNUSED axe_engine, 
                              size_t UNUSED num_n_parents, AXE_task_t UNUSED n_parents[], 
                              size_t UNUSED num_s_parents, AXE_task_t UNUSED s_parents[], 
                              void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    attr_io_in_t *input = (attr_io_in_t *)op_data->input;
    iod_handle_t coh = input->coh; /* container handle */
    iod_handle_t iod_oh = input->iod_oh; /* attribute's object handle */
    iod_obj_id_t iod_id = input->iod_id; /* attribute's ID */
    iod_obj_id_t mdkv_id = input->mdkv_id; /* The ID of the metadata KV */
    hg_bulk_t bulk_handle = input->bulk_handle; /* bulk handle for data */
    hid_t type_id = input->type_id; /* datatype ID of data */
    hid_t space_id = input->space_id; /* dataspace of attribute */
    iod_trans_id_t wtid = input->trans_num;
    iod_trans_id_t rtid = input->rcxt_num;
    uint32_t cs_scope = input->cs_scope;
    hg_bulk_block_t bulk_block_handle; /* HG block handle */
    hg_bulk_request_t bulk_request; /* HG request */
    iod_mem_desc_t *mem_desc; /* memory descriptor used for writing array */
    iod_array_iodesc_t file_desc; /* file descriptor used to write array */
    iod_hyperslab_t hslabs; /* IOD hyperslab generated from HDF5 filespace */
    size_t size; /* size of outgoing bulk data */
    void *buf; /* buffer to hold outgoing data */
    int ndims; /* dataset's rank/number of dimensions */
    scratch_pad sp;
    iod_checksum_t sp_cs = 0, attr_cs = 0;
    iod_handle_t mdkv_oh; /* metadata KV handle of attribute */
    hssize_t num_descriptors = 0; /* number of IOD file descriptors needed to describe filespace selection*/
    na_addr_t source = HG_Handler_get_addr(op_data->hg_handle); /* source address to pull data from */
    hbool_t opened_locally = FALSE; /* flag to indicate whether we opened the attribute here or if it was already opened */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* open the attribute if we don't have the handle yet */
    if(iod_oh.cookie == IOD_OH_UNDEFINED) {
        if (iod_obj_open_write(coh, iod_id, NULL /*hints*/, &iod_oh, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't open current group");        
    }

    /* Read bulk data here and wait for the data to be here  */
    size = HG_Bulk_handle_get_size(bulk_handle);
    if(NULL == (buf = malloc(size)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate read buffer");

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

#if H5VL_IOD_DEBUG 
    { 
        int i;
        int *buf_ptr = (int *)buf;

        fprintf(stderr, "AWRITE Received a buffer of size %d with values: ", size);
        for(i=0;i<60;++i)
            fprintf(stderr, "%d ", buf_ptr[i]);
        fprintf(stderr, "\n");
    }
#endif

    /* MSC - Not needed if dataspace is available */
#if 0
    /* open the metadata scratch pad of the attribute */
    if (iod_obj_open_read(coh, mdkv_id, NULL /*hints*/, &mdkv_oh, NULL) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "can't open scratch pad");

    if(H5VL_iod_get_metadata(mdkv_oh, rtid, H5VL_IOD_DATASPACE, H5VL_IOD_KEY_OBJ_DATASPACE,
                             NULL, NULL, &space_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "failed to retrieve dataspace");

    /* close the metadata scratch pad */
    if(iod_obj_close(mdkv_oh, NULL, NULL))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close object");
#endif

    /* set the memory descriptor */
    mem_desc = (iod_mem_desc_t *)malloc(sizeof(iod_mem_desc_t) + sizeof(iod_mem_frag_t));
    mem_desc->nfrag = 1;
    mem_desc->frag[0].addr = buf;
    mem_desc->frag[0].len = (iod_size_t)size;

    num_descriptors = 1;

    /* get the rank of the dataspace */
    if((ndims = H5Sget_simple_extent_ndims(space_id)) < 0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTGET, FAIL, "unable to get dataspace dimesnsion");

    hslabs.start = (iod_size_t *)malloc(sizeof(iod_size_t) * ndims);
    hslabs.stride = (iod_size_t *)malloc(sizeof(iod_size_t) * ndims);
    hslabs.block = (iod_size_t *)malloc(sizeof(iod_size_t) * ndims);
    hslabs.count = (iod_size_t *)malloc(sizeof(iod_size_t) * ndims);

    /* generate the descriptor */
    if(H5VL_iod_get_file_desc(space_id, &num_descriptors, &hslabs) < 0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTGET, FAIL, "unable to generate IOD file descriptor from dataspace selection");

    /* set the file descriptor */
    file_desc = hslabs;

    attr_cs = H5checksum(buf, size, NULL);

    /* write from array object */
    if(iod_array_write(iod_oh, wtid, NULL, mem_desc, &file_desc, &attr_cs, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_WRITEERROR, FAIL, "can't write to array object");

#if H5_DO_NATIVE
    ret_value = H5Awrite(iod_oh.cookie, type_id, buf);
#endif

done:
#if H5VL_IOD_DEBUG 
    fprintf(stderr, "Done with attr write, sending %d response to client\n", ret_value);
#endif

    if(HG_SUCCESS != HG_Handler_start_output(op_data->hg_handle, &ret_value))
        HDONE_ERROR(H5E_SYM, H5E_WRITEERROR, FAIL, "can't send result of write to client");

    input = (attr_io_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

    free(buf);

    /* free allocated descriptors */
    free(hslabs.start);
    free(hslabs.stride);
    free(hslabs.block);
    free(hslabs.count);
    if(mem_desc)
        free(mem_desc);

    /* close the attribute if we opened it in this routine */
    if(opened_locally) {
        if(iod_obj_close(iod_oh, NULL, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close Array object");
    }
    FUNC_LEAVE_NOAPI_VOID
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
void
H5VL_iod_server_attr_exists_cb(AXE_engine_t UNUSED axe_engine, 
                               size_t UNUSED num_n_parents, AXE_task_t UNUSED n_parents[], 
                               size_t UNUSED num_s_parents, AXE_task_t UNUSED s_parents[], 
                               void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    attr_op_in_t *input = (attr_op_in_t *)op_data->input;
    iod_handle_t coh = input->coh; /* container handle */
    iod_handle_t loc_handle = input->loc_oh; /* location handle to start lookup */
    iod_obj_id_t loc_id = input->loc_id; /* The ID of the current location object */
    iod_trans_id_t rtid = input->rcxt_num;
    uint32_t cs_scope = input->cs_scope;
    iod_handle_t obj_oh; /* current object handle accessed */
    iod_handle_t attr_kv_oh; /* KV handle holding attributes for object */
    iod_obj_id_t obj_id;
    const char *loc_name = input->path; /* path to start hierarchy traversal */
    const char *attr_name = input->attr_name; /* attribute's name */
    scratch_pad sp;
    iod_checksum_t sp_cs = 0;
    iod_size_t kv_size = 0;
    htri_t ret = -1;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Start attribute Exists %s\n", attr_name);
#endif

    /* Open the object where the attribute needs to be checked. */
    if(H5VL_iod_server_open_path(coh, loc_id, loc_handle, loc_name, rtid, &obj_id, &obj_oh) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't open object");

    if(obj_id != loc_id) {
        /* get scratch pad of the parent */
        if(iod_obj_get_scratch(obj_oh, rtid, &sp, &sp_cs, NULL) < 0)
            HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, FAIL, "can't get scratch pad for object");

        if(sp_cs && (cs_scope & H5_CHECKSUM_IOD)) {
            /* verify scratch pad integrity */
            if(H5VL_iod_verify_scratch_pad(sp, sp_cs) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Scratch Pad failed integrity check");
        }

        /* MSC - Dont do this check until we have a real IOD */
#if 0
        /* if attribute KV does not exist, return false*/
        if(IOD_ID_UNDEFINED == sp[1]) {
            ret = FALSE;
            HGOTO_DONE(SUCCEED);
        }
#endif

        /* open the attribute KV in scratch pad */
        if (iod_obj_open_write(coh, sp[1], NULL /*hints*/, &attr_kv_oh, NULL) < 0)
            HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, FAIL, "can't open scratch pad");
    }
    else {
        /* open the attribute KV  */
        if (iod_obj_open_write(coh, input->loc_attrkv_id, NULL /*hints*/, &attr_kv_oh, NULL) < 0)
            HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, FAIL, "can't open scratch pad");
    }

    /* close parent group if it is not the location we started the
       traversal into */
    if(loc_handle.cookie != obj_oh.cookie) {
        iod_obj_close(obj_oh, NULL, NULL);
    }

    /* get attribute ID */
    if(iod_kv_get_value(attr_kv_oh, rtid, attr_name, NULL, 
                        &kv_size, NULL, NULL) < 0) {
        ret = FALSE;
    }
    else {
        ret = TRUE;
    }

    iod_obj_close(attr_kv_oh, NULL, NULL);

#if H5_DO_NATIVE
    ret = H5Aexists(loc_handle.cookie, attr_name);
#else
    ret = FALSE;
#endif

done:

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Done with attr exists, sending response to client\n");
#endif

    HG_Handler_start_output(op_data->hg_handle, &ret);

    input = (attr_op_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

    FUNC_LEAVE_NOAPI_VOID
} /* end H5VL_iod_server_attr_exists_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_attr_rename_cb
 *
 * Purpose:	Renames iod HDF5 attribute.
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              April, 2013
 *
 *-------------------------------------------------------------------------
 */
void
H5VL_iod_server_attr_rename_cb(AXE_engine_t UNUSED axe_engine, 
                               size_t UNUSED num_n_parents, AXE_task_t UNUSED n_parents[], 
                               size_t UNUSED num_s_parents, AXE_task_t UNUSED s_parents[], 
                               void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    attr_rename_in_t *input = (attr_rename_in_t *)op_data->input;
    iod_handle_t coh = input->coh; /* container handle */
    iod_handle_t loc_handle = input->loc_oh; /* location handle to start lookup */
    iod_obj_id_t loc_id = input->loc_id; /* The ID of the current location object */
    iod_handle_t obj_oh; /* current object handle accessed */
    iod_handle_t attr_kv_oh; /* KV handle holding attributes for object */
    iod_obj_id_t obj_id, attr_id;
    const char *loc_name = input->path; /* path to start hierarchy traversal */
    const char *old_name = input->old_attr_name;
    const char *new_name = input->new_attr_name;
    iod_trans_id_t wtid = input->trans_num;
    iod_trans_id_t rtid = input->rcxt_num;
    uint32_t cs_scope = input->cs_scope;
    iod_kv_params_t kvs; /* KV lists for objects - used to unlink attribute object */
    iod_kv_t kv; /* KV entry */
    H5VL_iod_link_t iod_link;
    scratch_pad sp;
    iod_checksum_t sp_cs = 0;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Start attribute Rename %s to %s\n", old_name, new_name);
#endif

    /* Open the object where the attribute needs to be checked. */
    if(H5VL_iod_server_open_path(coh, loc_id, loc_handle, loc_name, rtid, &obj_id, &obj_oh) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't open object");

    if(loc_id != obj_id) {
        /* get scratch pad of the parent */
        if(iod_obj_get_scratch(obj_oh, rtid, &sp, &sp_cs, NULL) < 0)
            HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, FAIL, "can't get scratch pad for object");

        if(sp_cs && (cs_scope & H5_CHECKSUM_IOD)) {
            /* verify scratch pad integrity */
            if(H5VL_iod_verify_scratch_pad(sp, sp_cs) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Scratch Pad failed integrity check");
        }

        /* MSC - Dont do this check until we have a real IOD */
#if 0
        /* if attribute KV does not exist, return error*/
        if(IOD_ID_UNDEFINED == sp[1])
            HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, FAIL, "Object has no attributes");
#endif

        /* open the attribute KV in scratch pad */
        if (iod_obj_open_write(coh, sp[1], NULL /*hints*/, &attr_kv_oh, NULL) < 0)
            HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, FAIL, "can't open scratch pad");
    }
    else {
        /* open the attribute KV  */
        if (iod_obj_open_write(coh, input->loc_attrkv_id, NULL /*hints*/, &attr_kv_oh, NULL) < 0)
            HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, FAIL, "can't open scratch pad");
    }

    /* close parent group if it is not the location we started the
       traversal into */
    if(loc_handle.cookie != obj_oh.cookie) {
        iod_obj_close(obj_oh, NULL, NULL);
    }

    /* get attribute ID */
    if(H5VL_iod_get_metadata(attr_kv_oh, rtid, H5VL_IOD_LINK, 
                             old_name, NULL, NULL, &iod_link) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't retrieve Attribute ID from parent KV store");

    HDassert(iod_link.link_type == H5L_TYPE_HARD);
    attr_id = iod_link.u.iod_id;

    /* remove attribute with old name */
    kv.key = old_name;
    kvs.kv = &kv;
    if(iod_kv_unlink_keys(attr_kv_oh, wtid, NULL, (iod_size_t)1, &kvs, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "Unable to unlink KV pair");

    /* insert attribute with new name */
    if(H5VL_iod_insert_new_link(attr_kv_oh, wtid, new_name, 
                                H5L_TYPE_HARD, &attr_id, NULL, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't insert KV value");

    /* close the Attribute KV object */
    if(iod_obj_close(attr_kv_oh, NULL, NULL))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close object");

#if H5_DO_NATIVE
    ret_value = H5Arename(loc_handle.cookie, old_name, new_name);
#endif

done:

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Done with attr rename, sending response to client\n");
#endif

    HG_Handler_start_output(op_data->hg_handle, &ret_value);

    input = (attr_rename_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

    FUNC_LEAVE_NOAPI_VOID
} /* end H5VL_iod_server_attr_rename_cb() */


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
void
H5VL_iod_server_attr_remove_cb(AXE_engine_t UNUSED axe_engine, 
                               size_t UNUSED num_n_parents, AXE_task_t UNUSED n_parents[], 
                               size_t UNUSED num_s_parents, AXE_task_t UNUSED s_parents[], 
                               void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    attr_op_in_t *input = (attr_op_in_t *)op_data->input;
    iod_handle_t coh = input->coh; /* container handle */
    iod_handle_t loc_handle = input->loc_oh; /* location handle to start lookup */
    iod_obj_id_t loc_id = input->loc_id; /* The ID of the current location object */
    iod_handle_t obj_oh; /* current object handle accessed */
    iod_handle_t attr_kv_oh; /* KV handle holding attributes for object */
    iod_obj_id_t obj_id, attr_id;
    const char *loc_name = input->path; /* path to start hierarchy traversal */
    const char *attr_name = input->attr_name; /* attribute's name */
    iod_trans_id_t wtid = input->trans_num;
    iod_trans_id_t rtid = input->rcxt_num;
    uint32_t cs_scope = input->cs_scope;
    iod_kv_params_t kvs;
    iod_kv_t kv;
    H5VL_iod_link_t iod_link;
    scratch_pad sp;
    iod_checksum_t sp_cs = 0;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Start attribute Remove %s\n", attr_name);
#endif

    /* Open the object where the attribute needs to be checked. */
    if(H5VL_iod_server_open_path(coh, loc_id, loc_handle, loc_name, rtid, &obj_id, &obj_oh) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't open object");

    if(loc_id != obj_id) {
        /* get scratch pad of the parent */
        if(iod_obj_get_scratch(obj_oh, rtid, &sp, &sp_cs, NULL) < 0)
            HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, FAIL, "can't get scratch pad for object");

        if(sp_cs && (cs_scope & H5_CHECKSUM_IOD)) {
            /* verify scratch pad integrity */
            if(H5VL_iod_verify_scratch_pad(sp, sp_cs) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Scratch Pad failed integrity check");
        }

        /* MSC - Dont do this check until we have a real IOD */
#if 0
        /* if attribute KV does not exist, return error*/
        if(IOD_ID_UNDEFINED == sp[1])
            HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, FAIL, "Object has no attributes");
#endif

        /* open the attribute KV in scratch pad */
        if (iod_obj_open_write(coh, sp[1], NULL /*hints*/, &attr_kv_oh, NULL) < 0)
            HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, FAIL, "can't open scratch pad");
    }
    else {
        /* open the attribute KV  */
        if (iod_obj_open_write(coh, input->loc_attrkv_id, NULL /*hints*/, &attr_kv_oh, NULL) < 0)
            HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, FAIL, "can't open scratch pad");
    }

    /* close parent group if it is not the location we started the
       traversal into */
    if(loc_handle.cookie != obj_oh.cookie) {
        iod_obj_close(obj_oh, NULL, NULL);
    }

    /* get attribute ID */
    if(H5VL_iod_get_metadata(attr_kv_oh, rtid, H5VL_IOD_LINK, 
                             attr_name, NULL, NULL, &iod_link) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't retrieve Attribute ID from parent KV store");

    HDassert(iod_link.link_type == H5L_TYPE_HARD);
    attr_id = iod_link.u.iod_id;

    /* remove attribute */
    kv.key = attr_name;
    kvs.kv = &kv;
    if(iod_kv_unlink_keys(attr_kv_oh,wtid, NULL, (iod_size_t)1, &kvs, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "Unable to unlink KV pair");
    if(iod_obj_unlink(coh, attr_id, wtid, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "Unable to unlink object");

#if H5_DO_NATIVE
    ret_value = H5Adelete(loc_handle.cookie, attr_name);
#endif

done:
#if H5VL_IOD_DEBUG
    fprintf(stderr, "Done with attr remove, sending response to client\n");
#endif

    HG_Handler_start_output(op_data->hg_handle, &ret_value);

    input = (attr_op_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

    FUNC_LEAVE_NOAPI_VOID
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
void
H5VL_iod_server_attr_close_cb(AXE_engine_t UNUSED axe_engine, 
                              size_t UNUSED num_n_parents, AXE_task_t UNUSED n_parents[], 
                              size_t UNUSED num_s_parents, AXE_task_t UNUSED s_parents[], 
                              void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    attr_close_in_t *input = (attr_close_in_t *)op_data->input;
    iod_handle_t iod_oh = input->iod_oh; /* iod handle to close */
    //iod_obj_id_t iod_id = input->iod_id; /* iod id of object to close */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Start attribute Close\n");
#endif

    if(iod_oh.cookie != IOD_OH_UNDEFINED) {
#if H5_DO_NATIVE
        HDassert(H5Aclose(iod_oh.cookie) == SUCCEED);
#endif
        if((ret_value = iod_obj_close(iod_oh, NULL, NULL)) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close object");
    }
    else {
        /* MSC - need a way to kill object handle for this group */
        fprintf(stderr, "I do not have the OH of this attribute to close it\n");
    }

done:
#if H5VL_IOD_DEBUG
    fprintf(stderr, "Done with attr close, sending response to client\n");
#endif

    HG_Handler_start_output(op_data->hg_handle, &ret_value);

    input = (attr_close_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

    FUNC_LEAVE_NOAPI_VOID
} /* end H5VL_iod_server_attr_close_cb() */

#endif /* H5_HAVE_EFF */
