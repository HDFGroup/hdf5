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

#define H5D_PACKAGE		/*suppress error about including H5Dpkg	  */
#include "H5Dpkg.h"		/* Datasets 				*/
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5Gprivate.h"		/* IDs			  		*/
#include "H5RSprivate.h"		/* IDs			  		*/

#include "H5VLiod_server.h"

#ifdef H5_HAVE_EFF

/*
 * Programmer:  Mohamad Chaarawi <chaarawi@hdfgroup.gov>
 *              June, 2013
 *
 * Purpose:	The IOD plugin server side dataset routines.
 */

/* User data for VL traverssal */
typedef struct {
    iod_handle_t coh;
    iod_handle_t iod_oh;
    uint8_t *buf_ptr;
    hbool_t write_op;
    size_t buf_size;
    hid_t mem_super_type;
    hid_t dset_super_type;
    size_t mem_type_size;
    size_t dset_type_size;
    hsize_t nelmts;
    iod_trans_id_t tid;
} H5VL_iod_server_vl_io_t;

static herr_t 
H5VL__iod_server_final_io(iod_handle_t coh, iod_handle_t iod_oh, hid_t space_id, 
                          hid_t type_id, hbool_t write_op, void *buf,
                          size_t buf_size, iod_trans_id_t tid);

static herr_t 
H5VL__iod_server_vl_data_io(iod_handle_t coh, iod_handle_t iod_oh, hid_t space_id, 
                            hid_t mem_type_id, hid_t dset_type_id, hbool_t write_op, 
                            void *buf, size_t buf_size, hid_t dxpl_id, iod_trans_id_t tid);

static herr_t 
H5VL__iod_server_vl_data_io_cb(void UNUSED *elem, hid_t type_id, unsigned ndims, 
                               const hsize_t *point, void *_udata);


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
    iod_trans_id_t wtid = input->trans_num;
    iod_trans_id_t rtid = input->rcxt_num;
    iod_handle_t dset_oh, cur_oh, mdkv_oh;
    iod_obj_id_t cur_id, mdkv_id, attr_id;
    const char *name = input->name; /* name of dset including path to create */
    char *last_comp; /* the name of the dataset obtained from the last component in the path */
    hid_t dcpl_id;
    iod_array_struct_t array; /* IOD array struct describing the dataset's dimensions */
    iod_size_t *max_dims;
    scratch_pad sp;
    iod_ret_t ret;
    hbool_t collective = FALSE; /* MSC - change when we allow for collective */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* the traversal will retrieve the location where the dataset needs
       to be created. The traversal will fail if an intermediate group
       does not exist. */
    if(H5VL_iod_server_traverse(coh, loc_id, loc_handle, name, rtid, FALSE, 
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
    ret = iod_obj_create(coh, wtid, NULL/*hints*/, IOD_OBJ_ARRAY, NULL, &array,
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
        uint32_t sp_cs;

        /* create the attribute KV object for the dataset */
        if(iod_obj_create(coh, wtid, NULL, IOD_OBJ_KV, 
                          NULL, NULL, &attr_id, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't create metadata KV object");

        /* create the metadata KV object for the dataset */
        if(iod_obj_create(coh, wtid, NULL, IOD_OBJ_KV, 
                          NULL, NULL, &mdkv_id, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't create metadata KV object");

        /* set values for the scratch pad object */
        sp[0] = mdkv_id;
        sp[1] = attr_id;
        sp[2] = IOD_ID_UNDEFINED;
        sp[3] = IOD_ID_UNDEFINED;

        sp_cs = H5checksum(&sp, sizeof(sp), NULL);

        /* set scratch pad in dataset */
        if (iod_obj_set_scratch(dset_oh, wtid, &sp, &sp_cs, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't set scratch pad");

        /* Open Metadata KV object for write */
        if (iod_obj_open_write(coh, mdkv_id, NULL, &mdkv_oh, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't create scratch pad");

        if(H5P_DEFAULT == input->dcpl_id)
            input->dcpl_id = H5Pcopy(H5P_DATASET_CREATE_DEFAULT);
        dcpl_id = input->dcpl_id;

        /* insert plist metadata */
        if(H5VL_iod_insert_plist(mdkv_oh, wtid, dcpl_id, 
                                 NULL, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't insert KV value");

        /* insert link count metadata */
        if(H5VL_iod_insert_link_count(mdkv_oh, wtid, (uint64_t)1, 
                                      NULL, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't insert KV value");

        /* insert object type metadata */
        if(H5VL_iod_insert_object_type(mdkv_oh, wtid, H5I_DATASET, 
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

        /* add link in parent group to current object */
        if(H5VL_iod_insert_new_link(cur_oh, wtid, last_comp, 
                                    H5L_TYPE_HARD, &dset_id, NULL, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't insert KV value");
    }

    /* close parent group if it is not the location we started the
       traversal into */
    if(loc_handle.cookie != cur_oh.cookie) {
        iod_obj_close(cur_oh, NULL, NULL);
    }

#if H5_DO_NATIVE
    H5Pset_alloc_time(input->dcpl_id,H5D_ALLOC_TIME_EARLY);
    cur_oh.cookie = H5Dcreate2(loc_handle.cookie, last_comp, input->type_id, 
                               input->space_id, input->lcpl_id, 
                               input->dcpl_id, input->dapl_id);
    assert(cur_oh.cookie);
#endif

    output.iod_oh = dset_oh;

#if H5VL_IOD_DEBUG 
    fprintf(stderr, "Done with dset create, sending response to client\n");
#endif

    HG_Handler_start_output(op_data->hg_handle, &output);

done:
    /* return an UNDEFINED oh to the client if the operation failed */
    if(ret_value < 0) {
        fprintf(stderr, "failed to create/open Dataset\n");
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
    iod_trans_id_t rtid = input->rcxt_num;
    const char *name = input->name; /* name of dset including path to open */
    iod_obj_id_t dset_id; /* ID of the dataset to open */
    iod_handle_t dset_oh, mdkv_oh;
    scratch_pad sp;
    uint32_t sp_cs = 0;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

#if H5VL_IOD_DEBUG 
    fprintf(stderr, "Start dataset Open %s with Loc ID %llu\n", name, loc_id);
#endif

    /* Traverse Path and open dset */
    if(H5VL_iod_server_open_path(coh, loc_id, loc_handle, name, rtid, &dset_id, &dset_oh) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't open object");

    /* get scratch pad of the dataset */
    if(iod_obj_get_scratch(dset_oh, rtid, &sp, &sp_cs, NULL) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "can't get scratch pad for object");

    if(sp_cs) {
        /* verify scratch pad integrity */
        if(H5VL_iod_verify_scratch_pad(sp, sp_cs) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Scratch Pad failed integrity check");
    }

    /* open the metadata scratch pad */
    if (iod_obj_open_write(coh, sp[0], NULL /*hints*/, &mdkv_oh, NULL) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "can't open scratch pad");

    /* MSC - retrieve metadata - NEED IOD */
#if 0
    if(H5VL_iod_get_metadata(mdkv_oh, rtid, H5VL_IOD_PLIST, H5VL_IOD_KEY_OBJ_CPL,
                             NULL, NULL, &output.dcpl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "failed to retrieve dcpl");

    if(H5VL_iod_get_metadata(mdkv_oh, rtid, H5VL_IOD_LINK_COUNT, H5VL_IOD_KEY_OBJ_LINK_COUNT,
                             NULL, NULL, &output.link_count) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "failed to retrieve link count");

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
        dset_oh.cookie = H5Dopen(loc_handle.cookie, name, input->dapl_id);
        fprintf(stderr, "dataset name %s    Location %d    ID %d\n", 
               name, loc_handle.cookie, dset_oh.cookie);
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
    iod_trans_id_t rtid = input->rcxt_num;
    hg_bulk_block_t bulk_block_handle; /* HG block handle */
    hg_bulk_request_t bulk_request; /* HG request */
    size_t size, buf_size;
    void *buf = NULL; /* buffer to hold outgoing data */
    uint32_t cs = 0; /* checksum value */
    hbool_t is_vl_data;
    size_t nelmts; /* number of elements selected to read */
    na_addr_t dest = HG_Handler_get_addr(op_data->hg_handle); /* destination address to push data to */
    hbool_t opened_locally = FALSE; /* flag to indicate whether we opened the dset here or if it was already open */
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

    /* allocate buffer to hold data */
    if(NULL == (buf = malloc(size)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate read buffer");

    /* get the number of points selected */
    nelmts = (size_t)H5Sget_select_npoints(space_id);
    buf_size = 0;

    /* Adjust buffer is type conversion is needed. If the data
       elements are of variable length, just return that they are in
       is_vl_data for special processing */
    if(H5VL__iod_server_adjust_buffer(dst_id, src_id, nelmts, dxpl_id, 
                                      size, &buf, &is_vl_data, &buf_size) < 0) {
        fprintf(stderr, "failed to setup write operation");
        ret_value = FAIL;
        goto done;
    }

    if(!is_vl_data) {
        /* If the data is not VL, we can read the data from the array the normal way */
        if(H5VL__iod_server_final_io(coh, iod_oh, space_id, src_id, 
                                     FALSE, buf, buf_size, rtid) < 0) {
            fprintf(stderr, "can't read from array object\n");
            ret_value = FAIL;
            goto done;
        }

        {
            hbool_t flag = FALSE;
            int *ptr = (int *)buf;
            int i;

#if H5_DO_NATIVE
            {
                hid_t mspace;
                hsize_t dims[1];

                dims[0] = nelmts;
                mspace = H5Screate_simple(1, dims, NULL);
                ret_value = H5Dread(iod_oh.cookie, src_id, mspace, space_id, dxpl_id, buf);
                H5Sclose(mspace);
            }
#else /* fake data */
            for(i=0;i<60;++i)
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
    }
    else {
        /* If the data is of variable length, special access is required */
        if(H5VL__iod_server_vl_data_io(coh, iod_oh, space_id, dst_id, src_id, 
                                       FALSE, buf, buf_size, dxpl_id, rtid) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_READERROR, FAIL, "can't read from array object");

#if H5_DO_NATIVE
            ret_value = H5Dread(iod_oh.cookie, src_id, H5S_ALL, space_id, dxpl_id, buf);
#else /* fake data */
            {
                H5T_class_t class;

                class = H5Tget_class(dst_id);

                if(H5T_VLEN == class) {
                    int i=0, n=0, k=0, j=0, increment=4;
                    int *ptr = (int *)buf;

                    for(i = 0; i < 5; i++) {
                        int temp = i*increment + increment;

                        ptr[k++] = temp;
                        for(j = 0; j < temp; j++)
                            ptr[k++] = n ++;
                    } /* end for */
                }
                else if (H5T_STRING == class) {
                    uint8_t *ptr = (uint8_t *)buf;
                    size_t temp_size;

                    temp_size = 93;
                    memcpy(ptr, &temp_size, sizeof(size_t));
                    ptr += sizeof(size_t);
                    strcpy((char *)ptr, "Four score and seven years ago our forefathers brought forth on this continent a new nation,");
                    ptr += temp_size;

                    temp_size = 86;
                    memcpy(ptr, &temp_size, sizeof(size_t));
                    ptr += sizeof(size_t);
                    strcpy((char *)ptr, "conceived in liberty and dedicated to the proposition that all men are created equal.");
                    ptr += temp_size;

                    temp_size = 41;
                    memcpy(ptr, &temp_size, sizeof(size_t));
                    ptr += sizeof(size_t);
                    strcpy((char *)ptr, "Now we are engaged in a great civil war,");
                    ptr += temp_size;

                    temp_size = 89;
                    memcpy(ptr, &temp_size, sizeof(size_t));
                    ptr += sizeof(size_t);
                    strcpy((char *)ptr, "testing whether that nation or any nation so conceived and so dedicated can long endure.");
                    ptr += temp_size;

                }
            }
#endif
            /* calculate a checksum for the data to be sent */
            cs = H5checksum(buf, buf_size, NULL);
    }

    /* Create a new block handle to write the data */
    HG_Bulk_block_handle_create(buf, size, HG_BULK_READ_ONLY, &bulk_block_handle);

    /* Write bulk data here and wait for the data to be there  */
    if(HG_SUCCESS != HG_Bulk_write_all(dest, bulk_handle, bulk_block_handle, &bulk_request))
        HGOTO_ERROR(H5E_SYM, H5E_READERROR, FAIL, "can't read from array object");
    /* wait for it to complete */
    if(HG_SUCCESS != HG_Bulk_wait(bulk_request, HG_MAX_IDLE_TIME, HG_STATUS_IGNORE))
        HGOTO_ERROR(H5E_SYM, H5E_READERROR, FAIL, "can't read from array object");

    /* free block handle */
    if(HG_SUCCESS != HG_Bulk_block_handle_free(bulk_block_handle))
        HGOTO_ERROR(H5E_SYM, H5E_READERROR, FAIL, "can't free bds block handle");
done:

    output.ret = ret_value;
    output.cs = cs;
    output.buf_size = buf_size;

    if(HG_SUCCESS != HG_Handler_start_output(op_data->hg_handle, &output))
        HDONE_ERROR(H5E_SYM, H5E_WRITEERROR, FAIL, "can't send result of write to client");

#if H5VL_IOD_DEBUG 
    fprintf(stderr, "Done with dset read, checksum %u, sending response to client\n", cs);
#endif

    input = (dset_io_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

    if(buf)
        free(buf);

    /* close the dataset if we opened it in this routine */
    if(opened_locally) {
        if(iod_obj_close(iod_oh, NULL, NULL))
            HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close Array object");
    }
    FUNC_LEAVE_NOAPI_VOID
} /* end H5VL_iod_server_dset_read_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_dset_get_vl_size_cb
 *
 * Purpose:	Retrieve the size required to store a selection of VL data.
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              July, 2013
 *
 *-------------------------------------------------------------------------
 */
void
H5VL_iod_server_dset_get_vl_size_cb(AXE_engine_t UNUSED axe_engine, 
                                    size_t UNUSED num_n_parents, AXE_task_t UNUSED n_parents[], 
                                    size_t UNUSED num_s_parents, AXE_task_t UNUSED s_parents[], 
                                    void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    dset_get_vl_size_in_t *input = (dset_get_vl_size_in_t *)op_data->input;
    dset_read_out_t output;
    iod_handle_t coh = input->coh; /* container handle */
    iod_handle_t iod_oh = input->iod_oh; /* dset object handle */
    iod_obj_id_t iod_id = input->iod_id; /* dset ID */
    hid_t space_id = input->space_id; /* file space selection */
    hid_t dxpl_id = input->dxpl_id; /* transfer property list */
    iod_trans_id_t rtid = input->rcxt_num;
    size_t buf_size;
    void *buf = NULL; /* buffer to hold blob IDs */
    size_t nelmts; /* number of elements selected to read */
    hssize_t num_descriptors = 0, n; /* number of IOD file descriptors needed to describe filespace selection */
    iod_mem_desc_t *mem_desc; /* memory descriptor used for reading array */
    iod_array_iodesc_t file_desc; /* file descriptor used to read array */
    iod_hyperslab_t *hslabs = NULL; /* IOD hyperslab generated from HDF5 filespace */
    iod_checksum_t *cs_list = NULL;
    iod_ret_t *ret_list = NULL;
    iod_array_io_t *io_array = NULL; /* arary for list I/O */
    uint8_t *buf_ptr = NULL;
    int ndims, i; /* dataset's rank/number of dimensions */
    hbool_t opened_locally = FALSE; /* flag to indicate whether we opened the dset here or if it was already open */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* open the dataset if we don't have the handle yet */
    if(iod_oh.cookie == IOD_OH_UNDEFINED) {
        if (iod_obj_open_write(coh, iod_id, NULL /*hints*/, &iod_oh, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't open current group");
        opened_locally = TRUE;
    }

    /* get the number of points selected */
    nelmts = (size_t)H5Sget_select_npoints(space_id);
    ndims = H5Sget_simple_extent_ndims(space_id);

    /* allocate buffer to hold blob IDs */
    if(NULL == (buf = malloc(nelmts * sizeof(iod_obj_id_t) + sizeof(iod_size_t))))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate read buffer");

    /* buffer always contains the length of each sequence, so
       initialize it to the size required to store those lengths */
    buf_size = nelmts * sizeof(size_t);

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

    /* read each descriptore from the IOD container */
    for(n=0 ; n<num_descriptors ; n++) {
        hsize_t num_bytes = 0;
        hsize_t num_elems = 1;

        /* determine how many bytes the current descriptor holds */
        for(i=0 ; i<ndims ; i++)
            num_elems *= (hslabs[n].count[i] * hslabs[n].block[i]);
        num_bytes = num_elems * sizeof(iod_obj_id_t) + sizeof(iod_size_t);

        /* set the memory descriptor */
        mem_desc = (iod_mem_desc_t *)malloc(sizeof(iod_mem_desc_t) + sizeof(iod_mem_frag_t));
        mem_desc->nfrag = 1;
        mem_desc->frag[0].addr = (void *)buf_ptr;
        mem_desc->frag[0].len = (iod_size_t)num_bytes;

        buf_ptr += num_bytes;

        /* set the file descriptor */
        file_desc = hslabs[n];

        /* setup list I/O parameters */
        io_array[n].oh = iod_oh;
        io_array[n].hints = NULL;
        io_array[n].mem_desc = mem_desc;
        io_array[n].io_desc = &file_desc;
        io_array[n].cs = &cs_list[n];
        io_array[n].ret = &ret_list[n];
    }

    /* Read list IO */
    if(iod_array_read_list(coh, rtid, (iod_size_t)num_descriptors, 
                           io_array, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_READERROR, FAIL, "can't read from array object");

    /* verify return values */
    for(n=0 ; n<num_descriptors ; n++) {
        if(ret_list[n] < 0)
            HGOTO_ERROR(H5E_SYM, H5E_READERROR, FAIL, "can't read from array object");

        free(io_array[n].mem_desc);
    }

    /* compute the buf size */
    buf_ptr = (uint8_t *)buf;
    buf_ptr += sizeof(iod_obj_id_t);
    for(n=0 ; n<num_descriptors ; n++) {
        size_t seq_len;

        seq_len = *((size_t *)buf_ptr);
        buf_ptr += sizeof(iod_size_t) + sizeof(iod_obj_id_t);
        buf_size += seq_len;
    }

    /* MSC - fake now */
    if(H5T_VLEN == H5Tget_class(input->mem_type_id))
        buf_size = 260;
    else
        buf_size = 325;

done:

    output.ret = ret_value;
    output.cs = 0;
    output.buf_size = buf_size;

    if(HG_SUCCESS != HG_Handler_start_output(op_data->hg_handle, &output))
        HDONE_ERROR(H5E_SYM, H5E_WRITEERROR, FAIL, "can't send result of write to client");

#if H5VL_IOD_DEBUG 
    fprintf(stderr, "Done with dset get vl size (%zu), sending response to client\n", buf_size);
#endif

    input = (dset_get_vl_size_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

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

    if(buf)
        free(buf);

    /* close the dataset if we opened it in this routine */
    if(opened_locally) {
        if(iod_obj_close(iod_oh, NULL, NULL))
            HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close Array object");
    }
    FUNC_LEAVE_NOAPI_VOID
} /* end H5VL_iod_server_dset_get_vl_size_cb() */


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
    iod_trans_id_t wtid = input->trans_num;
    iod_trans_id_t rtid = input->rcxt_num;
    hg_bulk_block_t bulk_block_handle; /* HG block handle */
    hg_bulk_request_t bulk_request; /* HG request */
    size_t size, buf_size;
    hbool_t is_vl_data;
    uint32_t data_cs = 0;
    unsigned u;
    void *buf = NULL;
    size_t nelmts; /* number of elements selected to read */
    hbool_t flag = FALSE; /* temp flag to indicate whether corruption will be inserted */
    na_addr_t source = HG_Handler_get_addr(op_data->hg_handle); /* source address to pull data from */
    hbool_t opened_locally = FALSE; /* flag to indicate whether we opened the dset here or if it was already open */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

#if H5VL_IOD_DEBUG
    fprintf(stderr, "Dataset Write with AXE ID\n");
#endif

    /* open the dataset if we don't have the handle yet */
    if(iod_oh.cookie == IOD_OH_UNDEFINED) {
        if (iod_obj_open_write(coh, iod_id, NULL /*hints*/, &iod_oh, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't open current group");
        opened_locally = TRUE;
    }

    /* retrieve size of incoming bulk data */
    size = HG_Bulk_handle_get_size(bulk_handle);

    /* allocate buffer to hold data */
    if(NULL == (buf = malloc(size)))
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
            ret_value = FAIL;
            goto done;
        }
    }

    nelmts = (size_t)H5Sget_select_npoints(space_id);
    buf_size = 0;

    /* Adjust buffer is type conversion is needed. If the data
       elements are of variable length, just return that they are in
       is_vl_data for special processing */
    if(H5VL__iod_server_adjust_buffer(src_id, dst_id, nelmts, dxpl_id, 
                                      size, &buf, &is_vl_data, &buf_size) < 0) {
        fprintf(stderr, "failed to setup write operation");
        ret_value = FAIL;
        goto done;
    }

    /* If the data is not VL, we can write the data to the array the normal way */
    if(!is_vl_data) {
        /* convert data if needed */
        if(H5Tconvert(src_id, dst_id, nelmts, buf, NULL, dxpl_id) < 0)
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "data type conversion failed")

        if(H5VL__iod_server_final_io(coh, iod_oh, space_id, dst_id, 
                                     TRUE, buf, buf_size, wtid) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_READERROR, FAIL, "can't read from array object");

#if H5VL_IOD_DEBUG 
        { 
            int *ptr = (int *)buf;
 
           fprintf(stderr, "DWRITE Received a buffer of size %d with values: ", size);
            for(u=0 ; u<size/sizeof(int) ; ++u)
                fprintf(stderr, "%d ", ptr[u]);
            fprintf(stderr, "\n");
        }
#endif
    }
    else {
        /* If the data is of variable length, special access is required */
        if(H5VL__iod_server_vl_data_io(coh, iod_oh, space_id, src_id, dst_id, 
                                       TRUE, buf, buf_size, dxpl_id, wtid) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_READERROR, FAIL, "can't read from array object");
    }

#if H5_DO_NATIVE
    {
        hid_t mspace;
        hsize_t dims[1];

        dims[0] = nelmts;
        mspace = H5Screate_simple(1, dims, NULL);
        ret_value = H5Dwrite(iod_oh.cookie, dst_id, mspace, space_id, dxpl_id, buf);
        H5Sclose(mspace);
        {
            char *temp_name;
            H5D_t *dset;
            H5G_name_t *g_name;

            dset = (H5D_t *)H5I_object(iod_oh.cookie);
            g_name = H5D_nameof(dset);
            temp_name = H5RS_get_str(g_name->user_path_r);
            fprintf(stderr, "Dset name %s\n", temp_name);
        }
        assert(ret_value == 0);
    }
#endif

done:
#if H5VL_IOD_DEBUG 
    fprintf(stderr, "Done with dset write, sending %d response to client\n", ret_value);
#endif

    if(HG_SUCCESS != HG_Handler_start_output(op_data->hg_handle, &ret_value))
        HDONE_ERROR(H5E_SYM, H5E_WRITEERROR, FAIL, "can't send result of write to client");

    input = (dset_io_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

    if(buf)
        free(buf);

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
    iod_trans_id_t wtid = input->trans_num;
    iod_trans_id_t rtid = input->rcxt_num;
    /* int rank = input->dims.rank;  rank of dataset */
    hbool_t opened_locally = FALSE;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

#if H5VL_IOD_DEBUG 
        fprintf(stderr, "Start dataset Set Extent\n");
#endif

    /* open the dataset if we don't have the handle yet */
    if(iod_oh.cookie == IOD_OH_UNDEFINED) {
        if (iod_obj_open_write(coh, iod_id, NULL /*hints*/, &iod_oh, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't open current group");        
        opened_locally = TRUE;
    }

    /* extend along the first dimension only */
    if(iod_array_extend(iod_oh, wtid, (iod_size_t)input->dims.size[0], NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't extend dataset");

#if 0  /* MSC - NEED IOD */
    /* modify the dataspace of the dataset */
    {
        int rank;
        hid_t space_id;
        scratch_pad sp;
        uint32_t sp_cs = 0;
        iod_handle_t mdkv_oh;

        /* get scratch pad of the dataset */
        if(iod_obj_get_scratch(iod_oh, rtid, &sp, &sp_cs, NULL) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "can't get scratch pad for object");

        if(sp_cs) {
            /* verify scratch pad integrity */
            if(H5VL_iod_verify_scratch_pad(sp, sp_cs) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Scratch Pad failed integrity check");
        }

        /* open the metadata scratch pad */
        if (iod_obj_open_write(coh, sp[0], NULL /*hints*/, &mdkv_oh, NULL) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "can't open scratch pad");

        /* get the stored dataset dataspace */
        if(H5VL_iod_get_metadata(mdkv_oh, rtid, H5VL_IOD_DATASPACE, H5VL_IOD_KEY_OBJ_DATASPACE,
                                 NULL, NULL, &space_id) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "failed to retrieve dataspace");

        /* Check if we are shrinking or expanding any of the dimensions */
        if((rank = H5Sget_simple_extent_ndims(space_id)) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't get dataset dimensions");

        /* Modify the size of the data space */
        if(H5Sset_extent_simple(space_id, rank, dims.size, NULL) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to modify size of data space");

        /* insert dataspace metadata */
        if(H5VL_iod_insert_dataspace(mdkv_oh, wtid, space_id, 
                                     NULL, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't insert KV value");

        /* close the metadata scratch pad */
        if(iod_obj_close(mdkv_oh, NULL, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close object");
    }
#endif

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


/*-------------------------------------------------------------------------
 * Function:	H5VL__iod_server_final_io
 *
 * Read/Write to an IOD array object with an HDF5
 * selection. This is the normal way to access data given that the
 * datatype is not of variable length.
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              August, 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t 
H5VL__iod_server_final_io(iod_handle_t coh, iod_handle_t iod_oh, hid_t space_id, 
                          hid_t type_id, hbool_t write_op, void *buf, 
                          size_t buf_size, iod_trans_id_t tid)
{
    int ndims, i; /* dataset's rank/number of dimensions */
    hssize_t num_descriptors = 0, n; /* number of IOD file descriptors needed to describe filespace selection */
    iod_mem_desc_t *mem_desc; /* memory descriptor used for reading array */
    iod_array_iodesc_t file_desc; /* file descriptor used to read array */
    iod_hyperslab_t *hslabs = NULL; /* IOD hyperslab generated from HDF5 filespace */
    iod_checksum_t *cs_list = NULL;
    iod_ret_t *ret_list = NULL;
    iod_array_io_t *io_array = NULL; /* arary for list I/O */
    uint8_t *buf_ptr = NULL;
    size_t elmt_size;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    elmt_size = H5Tget_size(type_id);

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
        num_bytes = num_elems * elmt_size;

        /* set the memory descriptor */
        mem_desc = (iod_mem_desc_t *)malloc(sizeof(iod_mem_desc_t) + sizeof(iod_mem_frag_t));
        mem_desc->nfrag = 1;
        mem_desc->frag[0].addr = (void *)buf_ptr;
        mem_desc->frag[0].len = (iod_size_t)num_bytes;

        /* If this is a write op, compute the checksum for each memory fragment */
        if(write_op)
            cs_list[n] = H5checksum(buf_ptr, (size_t)num_bytes, NULL);

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
        io_array[n].mem_desc = mem_desc;
        io_array[n].io_desc = &file_desc;
        io_array[n].cs = &cs_list[n];
        io_array[n].ret = &ret_list[n];
    }

    if(write_op) {
        /* Write list IO */
        if(iod_array_write_list(coh, tid, (iod_size_t)num_descriptors, 
                                io_array, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_READERROR, FAIL, "can't read from array object");
    }
    else {
        /* Read list IO */
        if(iod_array_read_list(coh, tid, (iod_size_t)num_descriptors, 
                               io_array, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_READERROR, FAIL, "can't read from array object");
    }

    /* verify return values */
    for(n=0 ; n<num_descriptors ; n++) {
        if(ret_list[n] < 0)
            HGOTO_ERROR(H5E_SYM, H5E_READERROR, FAIL, "can't read from array object");

        free(io_array[n].mem_desc);
    }

    /* MSC - NEED IOD */
#if 0
    /* If this is a read operation, compute checksum for each IOD
       read, and compare it against checksum returned from IOD */
    if(!write_op) {
        hsize_t num_bytes = 0;
        hsize_t num_elems = 1;
        uint32_t checksum;
        H5_checksum_seed_t cs;

        buf_ptr = (uint8_t *)buf;
        cs.a = cs.b = cs.c = cs.state = 0;
        cs.total_length = buf_size;

        for(n=0 ; n<num_descriptors ; n++) {
            /* determine how many bytes the current descriptor holds */
            for(i=0 ; i<ndims ; i++)
                num_elems *= (hslabs[n].count[i] * hslabs[n].block[i]);
            num_bytes = num_elems * elmt_size;

            checksum = H5checksum(buf_ptr, (size_t)num_bytes, &cs);

            if(checksum != cs_list[n]) {
                fprintf(stderr, "Data Corruption detected when reading\n");
                ret_value = FAIL;
                goto done;
            }
            buf_ptr += num_bytes;
        }
    }
#endif

done:

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

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_final_io() */


/*-------------------------------------------------------------------------
 * Function:    H5VL__iod_server_vl_data_io
 *
 * Iterates over every (variable sized) element in the dataspace
 * selection and read/write it from IOD.
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              August, 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t 
H5VL__iod_server_vl_data_io(iod_handle_t coh, iod_handle_t iod_oh, hid_t space_id, 
                            hid_t mem_type_id, hid_t dset_type_id, hbool_t write_op, 
                            void *buf, size_t buf_size, hid_t dxpl_id, iod_trans_id_t tid)
{
    char bogus;                 /* bogus value to pass to H5Diterate() */
    H5VL_iod_server_vl_io_t udata;
    H5T_class_t class;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* get number of elements selected in dataspace */
    udata.nelmts = (size_t)H5Sget_select_npoints(space_id);

    /* set other parameters needed to do IO */
    udata.coh = coh;
    udata.iod_oh = iod_oh;
    udata.buf_ptr = (uint8_t *)buf;
    udata.write_op = write_op;
    udata.buf_size = buf_size;
    udata.tid = tid;

    class = H5Tget_class(mem_type_id);

    if(H5T_VLEN == class) {
        if((udata.mem_super_type = H5Tget_super(mem_type_id)) < 0)
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid super type of VL type");
        if((udata.dset_super_type = H5Tget_super(dset_type_id)) < 0)
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid super type of VL type");

        udata.mem_type_size = H5Tget_size(udata.mem_super_type);
        udata.dset_type_size = H5Tget_size(udata.dset_super_type);
    }
    else if(H5T_STRING == class) {
        assert(H5Tis_variable_str(mem_type_id));

        udata.mem_type_size = 1;
        udata.dset_type_size = 1;
    }

    /* iterate over every element and read/write it as a BLOB object */
    if(H5Diterate(&bogus, mem_type_id, space_id, H5VL__iod_server_vl_data_io_cb, &udata) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to compute buffer size");

    if(H5T_VLEN == class) {
        if(H5Tclose(udata.mem_super_type) < 0)
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTRELEASE, FAIL, "Can't close mem super type");
        if(H5Tclose(udata.dset_super_type) < 0)
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTRELEASE, FAIL, "Can't close dset super type");
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
}/* end H5VL__iod_server_vl_data_io */


/*-------------------------------------------------------------------------
 * Function:    H5VL__iod_server_vl_data_io_cb
 *
 * The callback to the H5Diterate routine called in
 * H5VL__iod_server_vl_data_io. This will access every element in the
 * array object and resolves it to a BLOB object. Then the actual data
 * is read/written from/to the BLOB object.
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              August, 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t 
H5VL__iod_server_vl_data_io_cb(void UNUSED *elem, hid_t type_id, unsigned ndims, 
                               const hsize_t *point, void *_udata)
{
    H5VL_iod_server_vl_io_t *udata = (H5VL_iod_server_vl_io_t *)_udata;
    iod_handle_t coh = udata->coh; /* container handle */
    size_t nelmts = udata->nelmts;
    iod_trans_id_t tid = udata->tid;
    iod_obj_id_t blob_id = 0;
    iod_handle_t blob_oh;
    iod_hyperslab_t hslab;
    iod_mem_desc_t *mem_desc; /* memory descriptor used for reading array */
    iod_array_iodesc_t file_desc; /* file descriptor used to read array */
    iod_blob_iodesc_t *blob_desc; /* blob descriptor */
    size_t old_seq_len = 0;
    unsigned u;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* read in the point from the array object */
    hslab.start = (iod_size_t *)malloc(sizeof(iod_size_t) * ndims);
    hslab.stride = (iod_size_t *)malloc(sizeof(iod_size_t) * ndims);
    hslab.block = (iod_size_t *)malloc(sizeof(iod_size_t) * ndims);
    hslab.count = (iod_size_t *)malloc(sizeof(iod_size_t) * ndims);

    memcpy(hslab.start, point, sizeof(size_t) * ndims);
    for(u=0 ; u<ndims ; u++) {
        hslab.stride[u] = 1;
        hslab.block[u] = 1;
        hslab.count[u] = 1;
    }

    /* set the memory descriptor */
    mem_desc = (iod_mem_desc_t *)malloc(sizeof(iod_mem_desc_t) + sizeof(iod_mem_frag_t) * 2);
    mem_desc->nfrag = 2;
    mem_desc->frag[0].addr = &blob_id;
    mem_desc->frag[0].len = sizeof(iod_obj_id_t);
    mem_desc->frag[1].addr = &old_seq_len;
    mem_desc->frag[1].len = sizeof(iod_size_t);

    file_desc = hslab;

    if(iod_array_read(udata->iod_oh, tid, NULL, 
                      mem_desc, &file_desc, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_READERROR, FAIL, "can't read from array object");

    free(mem_desc);

    /* write operation */
    if(udata->write_op) {
        size_t seq_len, buf_size;

        /* create a blob object if one has not been created yet */
        if(0 == blob_id) {
            if(iod_obj_create(coh, tid, NULL/*hints*/, IOD_OBJ_BLOB, NULL, NULL,
                              &blob_id, NULL /*event*/) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Failed to create BLOB object");
        }
        /* Open blob object */
        if (iod_obj_open_write(coh, blob_id, NULL, &blob_oh, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't open Datatype");

        seq_len = *((size_t *)(udata->buf_ptr));
        udata->buf_ptr += sizeof(size_t);

        buf_size = seq_len * udata->mem_type_size;

#if H5VL_IOD_DEBUG 
        {
            H5T_class_t class;

            class = H5Tget_class(type_id);

            if(H5T_STRING == class)
                fprintf(stderr, "String Length %zu: %s\n", seq_len, (char *)udata->buf_ptr);
            else if(H5T_VLEN == class) {
                int *ptr = (int *)udata->buf_ptr;

                fprintf(stderr, "Sequence Count %zu: ", seq_len);
                for(u=0 ; u<seq_len ; ++u)
                    fprintf(stderr, "%d ", ptr[u]);
                fprintf(stderr, "\n");
            }
        }
#endif


        /* MSC - type conversion ?? */

        /* create memory descriptor for writing */
        mem_desc = (iod_mem_desc_t *)malloc(sizeof(iod_mem_desc_t) + sizeof(iod_mem_frag_t));
        mem_desc->nfrag = 1;
        mem_desc->frag[0].addr = (void *)udata->buf_ptr;
        mem_desc->frag[0].len = (iod_size_t)buf_size;

        /* create file descriptor for writing */
        blob_desc = (iod_blob_iodesc_t *)malloc(sizeof(iod_blob_iodesc_t) + 
                                                sizeof(iod_blob_iofrag_t));
        blob_desc->nfrag = 1;
        blob_desc->frag[0].offset = 0;
        blob_desc->frag[0].len = (iod_size_t)buf_size;

        /* write the VL data to the blob */
        if(iod_blob_write(blob_oh, tid, NULL, mem_desc, blob_desc, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to write BLOB object");

        free(mem_desc);
        free(blob_desc);

        /* close BLOB */
        if(iod_obj_close(blob_oh, NULL, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close object");

        /* update the array element with the blob_id and sequence length */
        mem_desc = (iod_mem_desc_t *)malloc(sizeof(iod_mem_desc_t) + sizeof(iod_mem_frag_t) * 2);
        mem_desc->nfrag = 2;
        mem_desc->frag[0].addr = &blob_id;
        mem_desc->frag[0].len = sizeof(iod_obj_id_t);
        mem_desc->frag[1].addr = &seq_len;
        mem_desc->frag[1].len = sizeof(iod_size_t);

        /* write the blob ID & size to the array element */
        if(iod_array_write(udata->iod_oh, tid, NULL, 
                           mem_desc, &file_desc, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_READERROR, FAIL, "can't read from array object");

        free(mem_desc);

        /* advance buffer pointer */
        udata->buf_ptr += buf_size;
    }
    /* read operation */
    else {
        /* copy the sequence length as the first element in the buffer */
        memcpy(udata->buf_ptr, &old_seq_len, sizeof(size_t));
        udata->buf_ptr += sizeof(size_t);

        /* Open blob object */
        if (iod_obj_open_write(coh, blob_id, NULL, &blob_oh, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't open Datatype");

        /* create memory descriptor for reading */
        mem_desc = (iod_mem_desc_t *)malloc(sizeof(iod_mem_desc_t) + sizeof(iod_mem_frag_t));
        mem_desc->nfrag = 1;
        mem_desc->frag[0].addr = (void *)udata->buf_ptr;
        mem_desc->frag[0].len = old_seq_len * udata->mem_type_size;

        /* create file descriptor for writing */
        blob_desc = (iod_blob_iodesc_t *)malloc(sizeof(iod_blob_iodesc_t) + 
                                                sizeof(iod_blob_iofrag_t));
        blob_desc->nfrag = 1;
        blob_desc->frag[0].offset = 0;
        blob_desc->frag[0].len = old_seq_len * udata->mem_type_size;

        /* read the VL data from the blob */
        if(iod_blob_read(blob_oh, tid, NULL, mem_desc, blob_desc, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to write BLOB object");

        udata->buf_ptr += old_seq_len * udata->mem_type_size;

        /* close BLOB */
        if(iod_obj_close(blob_oh, NULL, NULL))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close object");

        free(mem_desc);
        free(blob_desc);
    }

    free(hslab.start);
    free(hslab.stride);
    free(hslab.block);
    free(hslab.count);
done:
    FUNC_LEAVE_NOAPI(ret_value)
}/* end H5VL__iod_server_vl_data_io_cb */

#endif /* H5_HAVE_EFF */
