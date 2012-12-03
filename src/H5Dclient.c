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

/* Programmer: 	Mohamad Chaarawi <chaarawi@hdfgroup.gov>
 *	       	November  8, 2012
 *
 * Purpose:	A forwarding layer that queries chunk locations from the MDS.
 */

/****************/
/* Module Setup */
/****************/

#define H5D_PACKAGE		/*suppress error about including H5Dpkg	  */


/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Dpkg.h"		/* Datasets				*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Fprivate.h"		/* Files				*/
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5MDprivate.h"	/* MDS server private			*/
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Oprivate.h"		/* Object headers		  	*/
#include "H5Vprivate.h"		/* Vector and array functions		*/

#ifdef H5_HAVE_PARALLEL
/****************/
/* Local Macros */
/****************/

/******************/
/* Local Typedefs */
/******************/

/********************/
/* Local Prototypes */
/********************/


static herr_t H5D__client_idx_insert(const H5D_chk_idx_info_t *idx_info,
    H5D_chunk_ud_t *udata);
static herr_t H5D__client_idx_get_addr(const H5D_chk_idx_info_t *idx_info,
    H5D_chunk_ud_t *udata);
static int H5D__client_idx_iterate(const H5D_chk_idx_info_t *idx_info,
    H5D_chunk_cb_func_t chunk_cb, void *chunk_udata);
#if 0
/* Chunked layout indexing callbacks */
static herr_t H5D__client_idx_init(const H5D_chk_idx_info_t *idx_info,
    const H5S_t *space, haddr_t dset_ohdr_addr);
static herr_t H5D__client_idx_create(const H5D_chk_idx_info_t *idx_info);
static hbool_t H5D__client_idx_is_space_alloc(const H5O_storage_chunk_t *storage);

static herr_t H5D__client_idx_remove(const H5D_chk_idx_info_t *idx_info,
    H5D_chunk_common_ud_t *udata);
static herr_t H5D__client_idx_delete(const H5D_chk_idx_info_t *idx_info);
static herr_t H5D__client_idx_copy_setup(const H5D_chk_idx_info_t *idx_info_src,
    const H5D_chk_idx_info_t *idx_info_dst);
static herr_t H5D__client_idx_copy_shutdown(H5O_storage_chunk_t *storage_src,
    H5O_storage_chunk_t *storage_dst, hid_t dxpl_id);
static herr_t H5D__client_idx_size(const H5D_chk_idx_info_t *idx_info,
    hsize_t *size);
static herr_t H5D__client_idx_reset(H5O_storage_chunk_t *storage, hbool_t reset_addr);
static herr_t H5D__client_idx_dump(const H5O_storage_chunk_t *storage,
    FILE *stream);
static herr_t H5D__client_idx_dest(const H5D_chk_idx_info_t *idx_info);
#endif

/*********************/
/* Package Variables */
/*********************/

/* v1 B-tree indexed chunk I/O ops */
const H5D_chunk_ops_t H5D_COPS_CLIENT[1] = {{
    NULL,
    NULL,
    NULL,
    H5D__client_idx_insert,
    H5D__client_idx_get_addr,
    NULL,
    H5D__client_idx_iterate,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL
}};


/*****************************/
/* Library Private Variables */
/*****************************/

/*******************/
/* Local Variables */
/*******************/



/*-------------------------------------------------------------------------
 * Function:	H5D__client_idx_insert
 *
 * Purpose:	Create the chunk it if it doesn't exist, or reallocate the
 *              chunk if its size changed.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Mohamad Chaarawi
 *              November  8, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__client_idx_insert(const H5D_chk_idx_info_t *idx_info, H5D_chunk_ud_t *udata)
{
    void *send_buf = NULL, *recv_buf = NULL;
    size_t buf_size = 0, dxpl_size = 0;
    MPI_Status status;
    int incoming_msg_size; /* incoming buffer size for MDS returned udata */
    hid_t mds_dset_id = FAIL;
    H5P_genplist_t *dxpl = NULL;
    uint8_t *p, *p1;
    unsigned u;
    herr_t ret_value = SUCCEED;		/* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(idx_info);
    HDassert(idx_info->f);
    HDassert(idx_info->pline);
    HDassert(idx_info->layout);
    HDassert(idx_info->storage);
    HDassert(H5F_addr_defined(idx_info->storage->idx_addr));
    HDassert(udata);

    if(NULL == (dxpl = (H5P_genplist_t *)H5I_object_verify(idx_info->dxpl_id, H5I_GENPROP_LST)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");

    if(H5P_get(dxpl, H5VL_DSET_MDS_ID, &mds_dset_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get property value for datatype id")

    /* get size for property lists to encode */
    if(H5P_DATASET_XFER_DEFAULT != idx_info->dxpl_id) {
        if((ret_value = H5P__encode(dxpl, FALSE, NULL, &dxpl_size)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
    }

    buf_size = 1 + sizeof(hid_t) + 1 + H5V_limit_enc_size((uint64_t)dxpl_size) + dxpl_size + 
        1 + H5V_limit_enc_size((uint64_t)(idx_info->storage->idx_addr)) + 
        sizeof(unsigned)*2 + sizeof(uint32_t) +
        1 + H5V_limit_enc_size((uint64_t)(udata->addr));

    for(u = 0; u < idx_info->layout->ndims; u++)
        buf_size += 1 + H5V_limit_enc_size((uint64_t)(udata->common.offset[u]));

    /* allocate the buffer for encoding the parameters */
    if(NULL == (send_buf = H5MM_malloc(buf_size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

    p = (uint8_t *)send_buf;

    /* encode request type */
    *p++ = (uint8_t)H5VL_CHUNK_INSERT;

    /* encode the object id */
    INT32ENCODE(p, mds_dset_id);

    /* encode udata */
    H5_ENCODE_UNSIGNED(p, udata->idx_hint);
    UINT32ENCODE(p, udata->nbytes);
    H5_ENCODE_UNSIGNED(p, udata->filter_mask);
    UINT64ENCODE_VARLEN(p, udata->addr);
    for(u = 0; u < idx_info->layout->ndims; u++)
        UINT64ENCODE_VARLEN(p,udata->common.offset[u]);

    /* encode the index address */
    UINT64ENCODE_VARLEN(p, idx_info->storage->idx_addr);

    /* encode the plist size */
    UINT64ENCODE_VARLEN(p, dxpl_size);

    /* encode dxpl if not default*/
    if(dxpl_size) {
        if((ret_value = H5P__encode(dxpl, FALSE, p, &dxpl_size)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
        p += dxpl_size;
    }

    MPI_Pcontrol(0);
    /* send the request to the MDS process */
    if(MPI_SUCCESS != MPI_Send(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, H5VL_MDS_LISTEN_TAG,
                               MPI_COMM_WORLD))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");

    /* probe for a message from the mds */
    if(MPI_SUCCESS != MPI_Probe(MPI_ANY_SOURCE, H5VL_MDS_SEND_TAG, MPI_COMM_WORLD, &status))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to probe for a message");

    /* get the incoming message size from the probe result */
    if(MPI_SUCCESS != MPI_Get_count(&status, MPI_BYTE, &incoming_msg_size))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to get incoming message size");

    /* allocate the receive buffer */
    recv_buf = (void *)H5MM_malloc (incoming_msg_size);

    /* receive the actual message */
    if(MPI_SUCCESS != MPI_Recv (recv_buf, incoming_msg_size, MPI_BYTE, status.MPI_SOURCE, 
                                H5VL_MDS_SEND_TAG, MPI_COMM_WORLD, &status))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to receive message");
    MPI_Pcontrol(1);
    p1 = (uint8_t *)recv_buf;

    /* encode udata */
    H5_DECODE_UNSIGNED(p1, udata->idx_hint);
    UINT32DECODE(p1, udata->nbytes);
    H5_DECODE_UNSIGNED(p1, udata->filter_mask);
    UINT64DECODE_VARLEN(p1, udata->addr);

    H5MM_free(send_buf);
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5D__client_idx_insert() */


/*-------------------------------------------------------------------------
 * Function:	H5D__client_idx_get_addr
 *
 * Purpose:	Get the file address of a chunk if file space has been
 *		assigned.  Save the retrieved information in the udata
 *		supplied.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Albert Cheng
 *              June 27, 1998
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__client_idx_get_addr(const H5D_chk_idx_info_t *idx_info, H5D_chunk_ud_t *udata)
{
    void *send_buf = NULL, *recv_buf = NULL;
    size_t buf_size = 0, dxpl_size = 0;
    MPI_Status status;
    int incoming_msg_size; /* incoming buffer size for MDS returned udata */
    hid_t mds_dset_id = FAIL;
    H5P_genplist_t *dxpl = NULL;
    uint8_t *p, *p1;
    unsigned u;
    herr_t ret_value = SUCCEED;		/* Return value */

    FUNC_ENTER_STATIC

    HDassert(idx_info);
    HDassert(idx_info->f);
    HDassert(idx_info->pline);
    HDassert(idx_info->layout);
    HDassert(idx_info->layout->ndims > 0);
    HDassert(idx_info->storage);
    HDassert(H5F_addr_defined(idx_info->storage->idx_addr));
    HDassert(udata);

    if(NULL == (dxpl = (H5P_genplist_t *)H5I_object_verify(idx_info->dxpl_id, H5I_GENPROP_LST)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");

    if(H5P_get(dxpl, H5VL_DSET_MDS_ID, &mds_dset_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get property value for datatype id")

    /* get size for property lists to encode */
    if(H5P_DATASET_XFER_DEFAULT != idx_info->dxpl_id) {
        if((ret_value = H5P__encode(dxpl, FALSE, NULL, &dxpl_size)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
    }

    buf_size = 1 + sizeof(hid_t) + 1 + H5V_limit_enc_size((uint64_t)dxpl_size) + dxpl_size +  
        1 + H5V_limit_enc_size((uint64_t)(idx_info->storage->idx_addr)) + 
        sizeof(unsigned)*2 + sizeof(uint32_t) +
        1 + H5V_limit_enc_size((uint64_t)(udata->addr));

    for(u = 0; u < idx_info->layout->ndims; u++)
        buf_size += 1 + H5V_limit_enc_size((uint64_t)(udata->common.offset[u]));

    /* allocate the buffer for encoding the parameters */
    if(NULL == (send_buf = H5MM_malloc(buf_size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

    p = (uint8_t *)send_buf;

    /* encode request type */
    *p++ = (uint8_t)H5VL_CHUNK_GET_ADDR;

    /* encode the object id */
    INT32ENCODE(p, mds_dset_id);

    /* encode udata */
    H5_ENCODE_UNSIGNED(p, udata->idx_hint);
    UINT32ENCODE(p, udata->nbytes);
    H5_ENCODE_UNSIGNED(p, udata->filter_mask);
    UINT64ENCODE_VARLEN(p, udata->addr);
    for(u = 0; u < idx_info->layout->ndims; u++)
        UINT64ENCODE_VARLEN(p,udata->common.offset[u]);

    /* encode the index address */
    UINT64ENCODE_VARLEN(p, idx_info->storage->idx_addr);

    /* encode the plist size */
    UINT64ENCODE_VARLEN(p, dxpl_size);
    /* encode dxpl if not default*/
    if(dxpl_size) {
        if((ret_value = H5P__encode(dxpl, FALSE, p, &dxpl_size)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
        p += dxpl_size;
    }

    MPI_Pcontrol(0);
    /* send the request to the MDS process */
    if(MPI_SUCCESS != MPI_Send(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, H5VL_MDS_LISTEN_TAG,
                               MPI_COMM_WORLD))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");

    /* probe for a message from the mds */
    if(MPI_SUCCESS != MPI_Probe(MPI_ANY_SOURCE, H5VL_MDS_SEND_TAG, MPI_COMM_WORLD, &status))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to probe for a message");

    /* get the incoming message size from the probe result */
    if(MPI_SUCCESS != MPI_Get_count(&status, MPI_BYTE, &incoming_msg_size))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to get incoming message size");

    /* allocate the receive buffer */
    recv_buf = (void *)H5MM_malloc (incoming_msg_size);

    /* receive the actual message */
    if(MPI_SUCCESS != MPI_Recv (recv_buf, incoming_msg_size, MPI_BYTE, status.MPI_SOURCE, 
                                H5VL_MDS_SEND_TAG, MPI_COMM_WORLD, &status))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to receive message");
    MPI_Pcontrol(1);
    p1 = (uint8_t *)recv_buf;

    /* encode udata */
    H5_DECODE_UNSIGNED(p1, udata->idx_hint);
    UINT32DECODE(p1, udata->nbytes);
    H5_DECODE_UNSIGNED(p1, udata->filter_mask);
    UINT64DECODE_VARLEN(p1, udata->addr);

    H5MM_free(send_buf);
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5D__client_idx_get_addr() */


/*-------------------------------------------------------------------------
 * Function:	H5D__client_idx_iterate
 *
 * Purpose:	Iterate over the chunks in an index, making a callback
 *              for each one.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Mohamad Chaarawi
 *              November  8, 2012
 *
 *-------------------------------------------------------------------------
 */
static int
H5D__client_idx_iterate(const H5D_chk_idx_info_t *idx_info,
    H5D_chunk_cb_func_t chunk_cb, void *chunk_udata)
{
    void *send_buf = NULL;
    uint8_t *p = NULL;
    H5P_genplist_t *dxpl = NULL;
    hid_t mds_dset_id = FAIL;
    size_t buf_size = 0, dxpl_size = 0;
    int ret_value;              /* Return value */

    FUNC_ENTER_STATIC

    HDassert(idx_info);
    HDassert(idx_info->f);
    HDassert(idx_info->pline);
    HDassert(idx_info->layout);
    HDassert(idx_info->storage);
    HDassert(H5F_addr_defined(idx_info->storage->idx_addr));
    HDassert(chunk_cb);
    HDassert(chunk_udata);

    if(NULL == (dxpl = (H5P_genplist_t *)H5I_object_verify(idx_info->dxpl_id, H5I_GENPROP_LST)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");

    if(H5P_get(dxpl, H5VL_DSET_MDS_ID, &mds_dset_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get property value for datatype id")

    /* get size for property lists to encode */
    if(H5P_DATASET_XFER_DEFAULT != idx_info->dxpl_id) {
        if((ret_value = H5P__encode(dxpl, FALSE, NULL, &dxpl_size)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
    }

    buf_size = 1 + sizeof(hid_t) + 
        1 + H5V_limit_enc_size((uint64_t)dxpl_size) + dxpl_size + 
        1 + H5V_limit_enc_size((uint64_t)(idx_info->storage->idx_addr));

    /* allocate the buffer for encoding the parameters */
    if(NULL == (send_buf = H5MM_malloc(buf_size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

    p = (uint8_t *)send_buf;

    /* encode request type */
    *p++ = (uint8_t)H5VL_CHUNK_ITERATE;

    /* encode the object id */
    INT32ENCODE(p, mds_dset_id);

    /* encode the index address */
    UINT64ENCODE_VARLEN(p, idx_info->storage->idx_addr);

    /* encode the plist size */
    UINT64ENCODE_VARLEN(p, dxpl_size);
    /* encode dxpl if not default*/
    if(dxpl_size) {
        if((ret_value = H5P__encode(dxpl, FALSE, p, &dxpl_size)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
        p += dxpl_size;
    }

    MPI_Pcontrol(0);
    /* send the request to the MDS process */
    if(MPI_SUCCESS != MPI_Send(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, 
                               H5VL_MDS_LISTEN_TAG, MPI_COMM_WORLD))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");
    H5MM_free(send_buf);
    MPI_Pcontrol(1);

    while (1) {
        MPI_Status status;
        void *recv_buf = NULL;
        int incoming_msg_size = 0;
        int ret;
        H5D_chunk_rec_t chunk_rec;
        unsigned u;

        MPI_Pcontrol(0);
        /* probe for a message from the mds */
        if(MPI_SUCCESS != MPI_Probe(MDS_RANK, H5VL_MDS_SEND_TAG, MPI_COMM_WORLD, &status))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to probe for a message");
        /* get the incoming message size from the probe result */
        if(MPI_SUCCESS != MPI_Get_count(&status, MPI_BYTE, &incoming_msg_size))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to get incoming message size");

        /* allocate the receive buffer */
        if(NULL == (recv_buf = H5MM_malloc(incoming_msg_size)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

        /* receive the actual message */
        if(MPI_SUCCESS != MPI_Recv(recv_buf, incoming_msg_size, MPI_BYTE, MDS_RANK, 
                                   H5VL_MDS_SEND_TAG, MPI_COMM_WORLD, &status))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to receive message");
        MPI_Pcontrol(1);

        p = (uint8_t *)recv_buf;

        /* decode the ret value from the server to see whether we need to continue iterating or stop */
        INT32DECODE(p, ret_value);
        if(ret_value == SUCCEED || ret_value == FAIL) {
            H5MM_free(recv_buf);
            break;
        }
        HDassert(ret_value == H5VL_MDS_CHUNK_ITERATE);

        UINT32DECODE(p, chunk_rec.nbytes);
        for(u=0 ; u<H5O_LAYOUT_NDIMS; u++)
            UINT64DECODE_VARLEN(p, chunk_rec.offset[u]);
        H5_DECODE_UNSIGNED(p, chunk_rec.filter_mask);    
        UINT64DECODE_VARLEN(p, chunk_rec.chunk_addr);

        H5MM_free(recv_buf);

        /* execute the iterate callback */
        ret = chunk_cb(&chunk_rec, chunk_udata);

        /* send result to MDS */
        MPI_Pcontrol(0);
        /* send the request to the MDS process and recieve the return value */
        if(MPI_SUCCESS != MPI_Send(&ret, sizeof(int), MPI_BYTE, MDS_RANK, 
                                   H5VL_MDS_LISTEN_TAG, MPI_COMM_WORLD))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");
        MPI_Pcontrol(1);
    }
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__client_idx_iterate() */

#if 0

/*-------------------------------------------------------------------------
 * Function:	H5D__client_idx_init
 *
 * Purpose:	Initialize the indexing information for a dataset.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Mohamad Chaarawi
 *              November  8, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__client_idx_init(const H5D_chk_idx_info_t *idx_info, const H5S_t UNUSED *space,
    haddr_t dset_ohdr_addr)
{
    herr_t      ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_STATIC

    /* Check args */
    HDassert(idx_info);
    HDassert(idx_info->f);
    HDassert(idx_info->pline);
    HDassert(idx_info->layout);
    HDassert(idx_info->storage);
    HDassert(H5F_addr_defined(dset_ohdr_addr));

    //idx_info->storage->u.client.dset_ohdr_addr = dset_ohdr_addr;
    //idx_info->storage->u.client.mdfile_id = ((H5FD_mdc_t *)(idx_info->f->shared->lf))->mdfile_id;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__client_idx_init() */


/*-------------------------------------------------------------------------
 * Function:	H5D__client_idx_create
 *
 * Purpose:	Creates a new indexed-storage B-tree and initializes the
 *		layout struct with information about the storage.  The
 *		struct should be immediately written to the object header.
 *
 *		This function must be called before passing LAYOUT to any of
 *		the other indexed storage functions!
 *
 * Return:	Non-negative on success (with the LAYOUT argument initialized
 *		and ready to write to an object header). Negative on failure.
 *
 * Programmer:	Mohamad Chaarawi
 *		November  8, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__client_idx_create(const H5D_chk_idx_info_t *idx_info)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_STATIC

    /* Check args */
    HDassert(idx_info);
    HDassert(idx_info->f);
    HDassert(idx_info->pline);
    HDassert(idx_info->layout);
    HDassert(idx_info->storage);
    HDassert(!H5F_addr_defined(idx_info->storage->idx_addr));

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__client_idx_create() */


/*-------------------------------------------------------------------------
 * Function:	H5D__client_idx_is_space_alloc
 *
 * Purpose:	Query if space is allocated for index method
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Mohamad Chaarawi
 *		November  8, 2012
 *
 *-------------------------------------------------------------------------
 */
static hbool_t
H5D__client_idx_is_space_alloc(const H5O_storage_chunk_t *storage)
{
    hbool_t ret_value;          /* Return value */

    FUNC_ENTER_STATIC_NOERR

    /* Check args */
    HDassert(storage);

    /* Set return value */
    ret_value = TRUE;//(hbool_t)H5F_addr_defined(storage->idx_addr);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__client_idx_is_space_alloc() */


/*-------------------------------------------------------------------------
 * Function:	H5D__client_idx_remove
 *
 * Purpose:	Remove chunk from index.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Mohamad Chaarawi
 *              November  8, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__client_idx_remove(const H5D_chk_idx_info_t *idx_info, H5D_chunk_common_ud_t *udata)
{
    herr_t	ret_value = SUCCEED;		/* Return value */

    FUNC_ENTER_STATIC

    HDassert(idx_info);
    HDassert(idx_info->f);
    HDassert(idx_info->pline);
    HDassert(idx_info->layout);
    HDassert(idx_info->storage);
    HDassert(H5F_addr_defined(idx_info->storage->idx_addr));
    HDassert(udata);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5D__client_idx_remove() */


/*-------------------------------------------------------------------------
 * Function:	H5D__client_idx_delete
 *
 * Purpose:	Delete index and raw data storage for entire dataset
 *              (i.e. all chunks)
 *
 * Return:	Success:	Non-negative
 *		Failure:	negative
 *
 * Programmer:	Mohamad Chaarawi
 *              November  8, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__client_idx_delete(const H5D_chk_idx_info_t *idx_info)
{
    herr_t ret_value = SUCCEED;     /* Return value */

    FUNC_ENTER_STATIC

    /* Sanity checks */
    HDassert(idx_info);
    HDassert(idx_info->f);
    HDassert(idx_info->pline);
    HDassert(idx_info->layout);
    HDassert(idx_info->storage);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__client_idx_delete() */


/*-------------------------------------------------------------------------
 * Function:	H5D__client_idx_copy_setup
 *
 * Purpose:	Set up any necessary information for copying chunks
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Mohamad Chaarawi
 *              November  8, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__client_idx_copy_setup(const H5D_chk_idx_info_t *idx_info_src,
    const H5D_chk_idx_info_t *idx_info_dst)
{
    herr_t      ret_value = SUCCEED;        /* Return value */

    FUNC_ENTER_STATIC_TAG(idx_info_dst->dxpl_id, H5AC__COPIED_TAG, FAIL)

    HDassert(idx_info_src);
    HDassert(idx_info_src->f);
    HDassert(idx_info_src->pline);
    HDassert(idx_info_src->layout);
    HDassert(idx_info_src->storage);
    HDassert(idx_info_dst);
    HDassert(idx_info_dst->f);
    HDassert(idx_info_dst->pline);
    HDassert(idx_info_dst->layout);
    HDassert(idx_info_dst->storage);
    HDassert(!H5F_addr_defined(idx_info_dst->storage->idx_addr));


done:
    FUNC_LEAVE_NOAPI_TAG(ret_value, FAIL)
} /* end H5D__client_idx_copy_setup() */


/*-------------------------------------------------------------------------
 * Function:	H5D__client_idx_copy_shutdown
 *
 * Purpose:	Shutdown any information from copying chunks
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Mohamad Chaarawi
 *              November  8, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__client_idx_copy_shutdown(H5O_storage_chunk_t *storage_src,
    H5O_storage_chunk_t *storage_dst,
    hid_t UNUSED dxpl_id)
{
    herr_t      ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_STATIC

    HDassert(storage_src);
    HDassert(storage_dst);


done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__client_idx_copy_shutdown() */


/*-------------------------------------------------------------------------
 * Function:    H5D__client_idx_size
 *
 * Purpose:     Retrieve the amount of index storage for chunked dataset
 *
 * Return:      Success:        Non-negative
 *              Failure:        negative
 *
 * Programmer:  Mohamad Chaarawi
 *              November  8, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__client_idx_size(const H5D_chk_idx_info_t *idx_info, hsize_t *index_size)
{
    H5D_chunk_common_ud_t udata;              /* User-data for loading B-tree nodes */
    H5B_info_t bt_info;                 /* B-tree info */
    hbool_t shared_init = FALSE;        /* Whether shared B-tree info is initialized */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_STATIC

    /* Check args */
    HDassert(idx_info);
    HDassert(idx_info->f);
    HDassert(idx_info->pline);
    HDassert(idx_info->layout);
    HDassert(idx_info->storage);
    HDassert(index_size);


    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__client_idx_size() */


/*-------------------------------------------------------------------------
 * Function:	H5D__client_idx_reset
 *
 * Purpose:	Reset indexing information.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Mohamad Chaarawi
 *              November  8, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__client_idx_reset(H5O_storage_chunk_t *storage, hbool_t reset_addr)
{
    FUNC_ENTER_STATIC_NOERR

    HDassert(storage);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5D__client_idx_reset() */


/*-------------------------------------------------------------------------
 * Function:	H5D__client_idx_dump
 *
 * Purpose:	Dump indexing information to a stream.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Mohamad Chaarawi
 *              November  8, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__client_idx_dump(const H5O_storage_chunk_t *storage, FILE *stream)
{
    FUNC_ENTER_STATIC_NOERR

    HDassert(storage);
    HDassert(stream);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5D__client_idx_dump() */


/*-------------------------------------------------------------------------
 * Function:	H5D__client_idx_dest
 *
 * Purpose:	Release indexing information in memory.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Mohamad Chaarawi
 *              November  8, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__client_idx_dest(const H5D_chk_idx_info_t *idx_info)
{
    herr_t      ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_STATIC

    HDassert(idx_info);
    HDassert(idx_info->f);
    HDassert(idx_info->pline);
    HDassert(idx_info->layout);
    HDassert(idx_info->storage);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__client_idx_dest() */


/*-------------------------------------------------------------------------
 * Function:	H5D_client_debug
 *
 * Purpose:	Debugs a B-tree node for indexed raw data storage.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Mohamad Chaarawi
 *              November  8, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D_client_debug(H5F_t *f, hid_t dxpl_id, haddr_t addr, FILE * stream, int indent,
		 int fwidth, unsigned ndims)
{
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D_client_debug() */
#endif
#endif /* H5_HAVE_PARALLEL */
