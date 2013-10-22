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

/*
 * Programmer:  Mohamad Chaarawi <chaarawi@hdfgroup.gov>
 *              October, 2013
 *
 * Purpose:	Routines to support Analysis Shipping.
 */

#include "H5VLiod_server.h"

#ifdef H5_HAVE_EFF

static hid_t H5VL__iod_get_space_layout(iod_layout_t layout, hid_t space, 
                                        uint32_t target_index);

static herr_t H5VL__iod_get_query_data_cb(void *elem, hid_t type_id, unsigned ndim, 
                                          const hsize_t *point, void *_udata);

static herr_t H5VL__iod_get_query_data(iod_handle_t coh, iod_obj_id_t dset_id, 
                                       iod_trans_id_t rtid, hid_t query_id, 
                                       hid_t space_id, hid_t type_id, 
                                       size_t *num_elmts, void **data);

static herr_t H5VL__iod_read_selection(iod_handle_t coh, iod_obj_id_t obj_id, 
                                       iod_trans_id_t rtid, hid_t space_id,
                                       hid_t type_id, void *buf);


/* User data for dataspace iteration to query elements. */
typedef struct {
    size_t num_elmts;
    hid_t query_id;
    hid_t space_query;
} H5VL__iod_get_query_data_t;


/*-------------------------------------------------------------------------
 * Function:    H5VL__iod_get_space_layout
 *
 * Purpose:     Generates a dataspace from the IOD layout for a particular
 *              ION or OST. The dataspace returned must be closed with H5Sclose().
 *
 * Return:	Success:	space id
 *		Failure:	FAIL
 *
 * Programmer:  Mohamad Chaarawi
 *              October, 2013
 *
 *-------------------------------------------------------------------------
 */
static hid_t
H5VL__iod_get_space_layout(iod_layout_t layout, hid_t space_id, uint32_t target_index)
{
    hsize_t dims[H5S_MAX_RANK];
    int ndims, i;
    size_t nelmts, u, start_elmt;
    hid_t space_layout = FAIL, ret_value = FAIL;

    FUNC_ENTER_NOAPI_NOINIT

    /* retrieve number of dimensions and dimensions. */
    ndims = H5Sget_simple_extent_dims(space_id, dims, NULL);

    /* number of elements striped on every ION. If the target ION is
       that last ION, special processing is required. */
    if((layout.target_start+layout.target_num-1) == target_index) {
        hssize_t npoints;

        npoints = H5Sget_simple_extent_npoints(space_id);
        nelmts = npoints % layout.stripe_size;
    }
    else {
        nelmts = layout.stripe_size;
    }

    /* copy the original dataspace and reset selection to NONE */
    if(FAIL == (space_layout = H5Scopy(space_id)))
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTINIT, FAIL, "unable to copy dataspace");
    if(H5Sselect_none(space_layout) < 0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTDELETE, FAIL, "can't delete default selection");

    start_elmt = nelmts * target_index;
    /* Add each element as one hyperslab to the selection */
    for(u=0 ; u<nelmts ; u++) {
        hsize_t start[H5S_MAX_RANK];
        hsize_t cur;

        /* generate hyperslab stuff for each point. only start is
           required; the stride, block, count are 1, i.e. can be
           NULL */
        cur = dims[0];
        start[0] = start_elmt % dims[0];

        for(i=1 ; i<ndims ; i++) {
            start[i] = start_elmt / cur;
            cur *= dims[i];
        }

        if(H5Sselect_hyperslab(space_layout, H5S_SELECT_OR, start, NULL, NULL, NULL))
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTSET, FAIL, "unable to add point to selection")
    }

    ret_value = space_layout;

done:
    if(ret_value < 0) {
        if(FAIL != space_layout && H5Sclose(space_layout) < 0)
            HDONE_ERROR(H5E_DATASPACE, H5E_CANTRELEASE, FAIL, "unable to release dataspace")
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__iod_get_space_layout() */


/*-------------------------------------------------------------------------
 * Function:    H5VL__iod_get_query_data_cb
 *
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
H5VL__iod_get_query_data_cb(void UNUSED *elem, hid_t UNUSED type_id, unsigned ndim, 
                            const hsize_t *point, void *_udata)
{
    H5VL__iod_get_query_data_t *udata = (H5VL__iod_get_query_data_t *)_udata;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(ndim == 1);

    /* MSC - NEED H5Q iterface */
    //if(H5Qapply(udata->query_id, elem, type_id)) {
    if (1) {
        udata->num_elmts ++;
        if(H5Sselect_elements(udata->space_query, H5S_SELECT_APPEND, 1, point))
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTSET, FAIL, "unable to add point to selection")
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__iod_get_query_data_cb */


/*-------------------------------------------------------------------------
 * Function:    H5VL__iod_get_query_data
 *
 * Purpose:     Generates a dataspace from the query specified. The dataspace 
 *              returned must be closed with H5Sclose().
 *
 * Return:	Success:	space id
 *		Failure:	FAIL
 *
 * Programmer:  Mohamad Chaarawi
 *              October, 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL__iod_get_query_data(iod_handle_t coh, iod_obj_id_t dset_id, 
                         iod_trans_id_t rtid, hid_t query_id, 
                         hid_t space_id, hid_t type_id, 
                         size_t *num_elmts, void **data)
{
    hsize_t dims[1];
    hssize_t nelmts;
    size_t elmt_size, buf_size;
    H5VL__iod_get_query_data_t udata;
    void *buf = NULL;
    hid_t space_query = FAIL, mem_space = FAIL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    nelmts = (size_t)H5Sget_select_npoints(space_id);
    elmt_size = H5Tget_size(type_id);
    buf_size = nelmts * elmt_size;

    /* allocate buffer to hold data */
    if(NULL == (buf = malloc(buf_size)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate read buffer");

    /* read the data local on the ION specified in the layout selection */
    if(H5VL__iod_read_selection(coh, dset_id, rtid, space_id, type_id, buf) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_READERROR, FAIL, "can't read local data");

    dims[0] = (hsize_t)nelmts;
    /* create a 1-D selection to describe the data read in memory */
    if(FAIL == (mem_space = H5Screate_simple(1, dims, NULL)))
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTCREATE, FAIL, "can't create simple dataspace");

    if(FAIL == (space_query = H5Scopy(mem_space)))
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTINIT, FAIL, "unable to copy dataspace");

    udata.query_id = query_id;
    udata.space_query = space_query;
    udata.num_elmts = 0;

    /* iterate over every element and apply the query on it. If the
       query is not satisfied, then remove it from the query selection */
    if(H5Diterate(buf, type_id, mem_space, H5VL__iod_get_query_data_cb, &udata) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to compute buffer size");

    if(udata.num_elmts) {
        buf_size = udata.num_elmts * elmt_size;

        /* allocate buffer to hold data */
        if(NULL == (*data = malloc(buf_size)))
            HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate data buffer");

        if(H5Dgather(space_query, buf, type_id, buf_size, *data, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_IO, H5E_CANTCOPY, FAIL, "gather failed")
    }

    *num_elmts = udata.num_elmts;

done:
    if(space_query && H5Sclose(space_query) < 0)
        HDONE_ERROR(H5E_DATASPACE, H5E_CANTRELEASE, FAIL, "unable to release dataspace")
    if(mem_space && H5Sclose(mem_space) < 0)
        HDONE_ERROR(H5E_DATASPACE, H5E_CANTRELEASE, FAIL, "unable to release dataspace")
    if(buf != NULL)
        free(buf);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__iod_get_query_data() */


/*-------------------------------------------------------------------------
 * Function:    H5VL__iod_read_selection
 *
 * Purpose: 
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 *-------------------------------------------------------------------------
 */
static herr_t 
H5VL__iod_read_selection(iod_handle_t coh, iod_obj_id_t obj_id, 
                         iod_trans_id_t rtid, hid_t space_id,
                         hid_t type_id, void *buf)
{
    iod_handle_t obj_oh;
    size_t buf_size=0;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* open the array object */
    if (iod_obj_open_read(coh, obj_id, NULL /*hints*/, &obj_oh, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't open current group");

    /* read the data selection from IOD. */
    /* MSC - will need to do it in pieces, not it one shot. */
    if(H5VL__iod_server_final_io(coh, obj_oh, space_id, type_id, 
                                 FALSE, buf, buf_size, 0, 0, rtid) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_READERROR, FAIL, "can't read from array object");

done:
    if(obj_oh.cookie != IOD_OH_UNDEFINED && iod_obj_close(obj_oh, NULL, NULL) < 0)
        HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close Array object");

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__iod_read_selection() */

#endif /* H5_HAVE_EFF */
