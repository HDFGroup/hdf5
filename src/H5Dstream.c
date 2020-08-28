/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/* Programmer:  Quincey Koziol
 *              Saturday, August 22, 2020
 *
 * Purpose:     Streaming I/O routines, for native VOL connector.
 *
 */

/****************/
/* Module Setup */
/****************/

#include "H5Dmodule.h"          /* This source code file is part of the H5D module */


/***********/
/* Headers */
/***********/
#include "H5private.h"          /* Generic Functions                        */
#include "H5CXprivate.h"        /* API Contexts                             */
#include "H5Dpkg.h"             /* Datasets                                 */
#include "H5Eprivate.h"         /* Error handling                           */
#include "H5Iprivate.h"         /* IDs                                      */


/****************/
/* Local Macros */
/****************/


/******************/
/* Local Typedefs */
/******************/


/********************/
/* Local Prototypes */
/********************/


/*********************/
/* Package Variables */
/*********************/


/*******************/
/* Local Variables */
/*******************/



/*-------------------------------------------------------------------------
 * Function:    H5D__stream_start
 *
 * Purpose:     Start streaming I/O for a dataset.
 *
 *              This routine caches information that would otherwise be
 *              repeatedly set up / torn down by append & sequence operations.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              August 22, 2020
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D__stream_start(H5D_t *dset, hid_t dxpl_id, unsigned axis, size_t extension,
    hid_t mem_type_id)
{
    H5T_t *dt, *copied_dt = NULL;   /* Datatype for memory buffer */
    H5P_genplist_t *dxpl = NULL;    /* DXPL for operation */
    hsize_t nelmts;                 /* Number of elements in selection */
    unsigned u;                     /* Local index variable */
    herr_t ret_value = SUCCEED;     /* Return value */

    FUNC_ENTER_PACKAGE

    /* Sanity checks */
    /* (Check datatype is convertible) */
    /* (Check axis is in range) */
    /* (Check layout is chunked) */
    /* (Don't allow streamed I/O to be set twice) */

    /* Set streamed I/O flag for dataset */
    dset->shared->is_streaming = TRUE;


    /* Cache the streaming parameters */

    /* Copy the DXPL for the streaming operations */
    if(NULL == (dxpl = (H5P_genplist_t *)H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't get dataset transfer property list")
    if((dset->shared->stream.dxpl_id = H5P_copy_plist(dxpl, FALSE)) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTCOPY, FAIL, "can't copy dataset transfer property list");

    /* Cache streamed I/O dataspace information */
    dset->shared->stream.axis = axis;
    dset->shared->stream.extension = extension;

    /* Copy datatype for memory buffer */
    if(NULL == (dt = (H5T_t *)H5I_object(mem_type_id)))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't get datatype for memory type ID")
    if(NULL == (copied_dt = H5T_copy(dt, H5T_COPY_TRANSIENT)))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTCOPY, FAIL, "can't copy datatype for memory type ID")
    if((dset->shared->stream.mem_type_id = H5I_register(H5I_DATATYPE, copied_dt, FALSE)) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTREGISTER, FAIL, "unable to register memory buffer datatype ID")


    /* Generate info needed for each append / sequence operation */

    /* Make a copy of the dataset's dataspace */
    if(NULL == (dset->shared->stream.append.file_space = H5S_copy(dset->shared->space, TRUE, FALSE)))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTCOPY, FAIL, "can't copy dataset's dataspace")

    /* Set up hyperslab parameters for the append operation */
    nelmts = 1;
    for(u = 0 ; u < dset->shared->ndims ; u++) {
        if(u == axis) {
            dset->shared->stream.append.offset = dset->shared->curr_dims[u];
            dset->shared->stream.append.hyper_start[u] = dset->shared->curr_dims[u];
            dset->shared->stream.append.hyper_count[u] = extension;
        } /* end if */
        else {
            dset->shared->stream.append.hyper_start[u] = 0;
            dset->shared->stream.append.hyper_count[u] = dset->shared->curr_dims[u];
        } /* end else */

        /* Accumulate # of elements in selection */
        nelmts *= dset->shared->stream.append.hyper_count[u];
    } /* end for */

    /* Create a memory space for the buffer */
    if(NULL == (dset->shared->stream.append.mem_space = H5S_create_simple(1, &nelmts, NULL)))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTCREATE, FAIL, "can't create memory space for buffer")


done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__stream_start() */


/*-------------------------------------------------------------------------
 * Function:    H5D__stream_append
 *
 * Purpose:     Append data to dataset
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              August 22, 2020
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D__stream_append(H5D_t *dset, const void *buf)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_PACKAGE_TAG(dset->oloc.addr)

    /* Sanity checks */
    /* (Must be in streamed I/O mode) */

    /* Set DXPL for operation */
    H5CX_set_dxpl(dset->shared->stream.dxpl_id);

    /* Update the dimension for the dataset */
    dset->shared->curr_dims[dset->shared->stream.axis] += dset->shared->stream.extension;

    /* Extend the dataset in the requested dimension */
    if(H5D__set_extent(dset, dset->shared->curr_dims, TRUE) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTSET, FAIL, "can't extend dataset dimensions")

    /* Select hyperslab that covers newly extended region */
    if(H5S_select_hyperslab(dset->shared->stream.append.file_space, H5S_SELECT_SET, dset->shared->stream.append.hyper_start, NULL, dset->shared->stream.append.hyper_count, NULL) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTSELECT, FAIL, "can't select hyperslab to cover extended region")

    /* Write the data to the newly extended area of the dataset */
    if(H5D__write(dset, dset->shared->stream.mem_type_id, dset->shared->stream.append.mem_space, dset->shared->stream.append.file_space, buf, TRUE) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "can't write to newly extended area of dataset")

    /* Update the append location */
    dset->shared->stream.append.offset += dset->shared->stream.extension;
    dset->shared->stream.append.hyper_start[dset->shared->stream.axis] += dset->shared->stream.extension;


done:
    FUNC_LEAVE_NOAPI_TAG(ret_value)
} /* end H5D__stream_append() */


/*-------------------------------------------------------------------------
 * Function:    H5D__stream_sequence
 *
 * Purpose:     Sequentially read data to dataset
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              August 22, 2020
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D__stream_sequence(H5D_t *dset, void *buf)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_PACKAGE_TAG(dset->oloc.addr)

    /* Sanity checks */
    /* (Must be in streamed I/O mode) */


    /* Set DXPL for operation */
    H5CX_set_dxpl(dset->shared->stream.dxpl_id);

done:
    FUNC_LEAVE_NOAPI_TAG(ret_value)
} /* end H5D__stream_sequence() */


/*-------------------------------------------------------------------------
 * Function:    H5D__stream_is_streaming
 *
 * Purpose:     Query if a dataset is in streamed I/O mode
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              August 22, 2020
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D__stream_is_streaming(const H5D_t *dset, hbool_t *is_streaming)
{
    FUNC_ENTER_PACKAGE_NOERR

    /* Sanity checks */
    HDassert(dset);
    HDassert(dset->shared);
    HDassert(is_streaming);

    *is_streaming = dset->shared->is_streaming;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5D__stream_is_streaming() */


/*-------------------------------------------------------------------------
 * Function:    H5D__stream_cleanup
 *
 * Purpose:     Clean up cached information for streamed I/O mode
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              August 24, 2020
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D__stream_cleanup(H5D_t *dset)
{
    hbool_t free_failed = FALSE;        /* Set if freeing sub-components failed */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_PACKAGE

    /* Sanity checks */
    HDassert(dset->shared->is_streaming);

    /* Release cached information -
     * not much we can do if one of these fails, so we just continue.
     */
    free_failed |= (H5I_dec_ref(dset->shared->stream.mem_type_id) < 0) ||
                      (H5I_dec_ref(dset->shared->stream.dxpl_id) < 0);

    /* Release temporary objects */
    if(dset->shared->stream.append.file_space && H5S_close(dset->shared->stream.append.file_space) < 0)
        HDONE_ERROR(H5E_DATASET, H5E_CANTCLOSEOBJ, FAIL, "can't close copy of dataset's dataspace")
    if(dset->shared->stream.append.mem_space && H5S_close(dset->shared->stream.append.mem_space) < 0)
        HDONE_ERROR(H5E_DATASET, H5E_CANTCLOSEOBJ, FAIL, "can't close memory dataspace")

    /* Check if anything failed in the middle... */
    if(free_failed)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTRELEASE, FAIL, "couldn't free a streamed I/O component")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__stream_cleanup() */


/*-------------------------------------------------------------------------
 * Function:    H5D__stream_stop
 *
 * Purpose:     Stop streaming I/O for a dataset.
 *
 *              This routine releases cached streaming information.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              August 22, 2020
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D__stream_stop(H5D_t *dset)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_PACKAGE

    /* Sanity checks */
    /* (Must be in streamed I/O mode) */

    /* Release any cached info */
    if(H5D__stream_cleanup(dset) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTRELEASE, FAIL, "unable to destroy streaming I/O information")

    /* Reset streamed I/O flag for dataset */
    dset->shared->is_streaming = FALSE;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__stream_stop() */

