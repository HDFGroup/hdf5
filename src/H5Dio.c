/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/****************/
/* Module Setup */
/****************/

#include "H5Dmodule.h" /* This source code file is part of the H5D module */

/***********/
/* Headers */
/***********/
#include "H5private.h"   /* Generic Functions                        */
#include "H5CXprivate.h" /* API Contexts                             */
#include "H5Dpkg.h"      /* Dataset functions                        */
#include "H5Eprivate.h"  /* Error handling                           */
#include "H5FLprivate.h" /* Free Lists                               */
#include "H5Iprivate.h"  /* IDs                                      */
#include "H5MMprivate.h" /* Memory management                        */
#include "H5Sprivate.h"  /* Dataspace                                */
#include "H5VLprivate.h" /* Virtual Object Layer                     */ /*!FIXME -NAF */

#include "H5VLnative_private.h" /* Native VOL connector                     */

/****************/
/* Local Macros */
/****************/

/******************/
/* Local Typedefs */
/******************/

/********************/
/* Local Prototypes */
/********************/

/* Setup/teardown routines */
static herr_t H5D__ioinfo_init(H5D_t *dset, H5D_dset_info_t *dset_info, H5D_storage_t *store,
                               H5D_io_info_t *io_info);
static herr_t H5D__typeinfo_init(const H5D_t *dset, hid_t mem_type_id, hbool_t do_write,
                                 H5D_type_info_t *type_info);
#ifdef H5_HAVE_PARALLEL
static herr_t H5D__ioinfo_adjust(const size_t count, H5D_io_info_t *io_info);
#endif /* H5_HAVE_PARALLEL */
static herr_t H5D__typeinfo_term(const H5D_type_info_t *type_info);

/* Internal I/O routines */
static herr_t H5D__final_mdset_sel_io(H5D_io_info_t *io_info);

/*********************/
/* Package Variables */
/*********************/

/*******************/
/* Local Variables */
/*******************/

/* Declare a free list to manage blocks of type conversion data */
H5FL_BLK_DEFINE(type_conv);

/* Declare a free list to manage the H5D_dset_info_t struct */
H5FL_DEFINE(H5D_dset_info_t);

/*-------------------------------------------------------------------------
 * Function:    H5D__pre_read
 *
 * Purpose:     Checks if multi dataset reads are possible and forwards
 *              the call to H5D__read if it is, otherwise breaks it into
 *              single dataset calls to H5D__read.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D__pre_read(size_t count, H5D_dset_info_t *dset_info)
{
    hbool_t broke_mdset = FALSE; /* Whether to break multi-dataset option */
    size_t  u;                   /* Local index variable */
    herr_t  ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_PACKAGE

    /* check args */
    HDassert(count > 0);
    HDassert(dset_info);

    {
#ifdef H5_HAVE_PARALLEL
        H5FD_mpio_xfer_t xfer_mode; /* Parallel I/O transfer mode */

        /* Get the transfer mode */
        if (H5CX_get_io_xfer_mode(&xfer_mode) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to get value")

        /* In independent mode or with an unsupported layout, for now just
           read each dataset individually */
        if (xfer_mode == H5FD_MPIO_INDEPENDENT)
            broke_mdset = TRUE;
        else
#endif /*H5_HAVE_PARALLEL*/
            /* Multi-dset I/O currently supports CHUNKED and internal CONTIGUOUS
             * only, not external CONTIGUOUS (EFL) or COMPACT.  Fall back to
             * individual dataset reads if any dataset uses an unsupported layout.
             */
            for (u = 0; u < count; u++) {
                if (!(dset_info[u].dset->shared->layout.type == H5D_CHUNKED ||
                      (dset_info[u].dset->shared->layout.type == H5D_CONTIGUOUS &&
                       dset_info[u].dset->shared->layout.ops != H5D_LOPS_EFL))) {
                    broke_mdset = TRUE;
                    break;
                }
            } /* end for */
    }

    if (broke_mdset) {
        /* Read raw data from each dataset by itself */
        for (u = 0; u < count; u++)
            if (H5D__read(1, &dset_info[u], FALSE) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "can't read data")
    } /* end if */
    else if (H5D__read(count, dset_info, TRUE) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "can't read data")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__pre_read() */

/*-------------------------------------------------------------------------
 * Function:    H5D__pre_write
 *
 * Purpose:     Checks if multi dataset writes are possible and forwards
 *              the call to H5D__write if it is, otherwise breaks it into
 *              single dataset calls to H5D__write.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D__pre_write(size_t count, H5D_dset_info_t *dset_info)
{
    hbool_t broke_mdset = FALSE; /* Whether to break multi-dataset option */
    size_t  u;                   /* Local index variable */
    herr_t  ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_PACKAGE

    /* check args */
    HDassert(count > 0);
    HDassert(dset_info);

    {
#ifdef H5_HAVE_PARALLEL
        H5FD_mpio_xfer_t xfer_mode; /* Parallel I/O transfer mode */

        /* Get the transfer mode */
        if (H5CX_get_io_xfer_mode(&xfer_mode) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to get value")

        /* In independent mode or with an unsupported layout, for now
           just write each dataset individually */
        if (xfer_mode == H5FD_MPIO_INDEPENDENT)
            broke_mdset = TRUE;
        else
#endif /*H5_HAVE_PARALLEL*/
            /* Multi-dset I/O currently supports CHUNKED and internal CONTIGUOUS
             * only, not external CONTIGUOUS (EFL) or COMPACT.  Fall back to
             * individual dataset writes if any dataset uses an unsupported layout.
             */
            for (u = 0; u < count; u++) {
                if (!(dset_info[u].dset->shared->layout.type == H5D_CHUNKED ||
                      (dset_info[u].dset->shared->layout.type == H5D_CONTIGUOUS &&
                       dset_info[u].dset->shared->layout.ops != H5D_LOPS_EFL))) {
                    broke_mdset = TRUE;
                    break;
                }
            } /* end for */
    }

    if (broke_mdset) {
        /* Write raw data to each dataset by itself */
        for (u = 0; u < count; u++)
            if (H5D__write(1, &dset_info[u], FALSE) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "can't write data")
    } /* end if */
    else if (H5D__write(count, dset_info, TRUE) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "can't write data")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__pre_write() */

/*-------------------------------------------------------------------------
 * Function:	H5D__read
 *
 * Purpose:	Reads multiple (parts of) DATASETs into application memory BUFs.
 *              See H5Dread_multi() for complete details.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D__read(size_t count, H5D_dset_info_t *dset_info, hbool_t is_mdset)
{
    H5D_io_info_t io_info;                    /* Dataset I/O info  for multi dsets */
    size_t        type_info_init      = 0;    /* Number of datatype info structs that have been initialized */
    H5S_t        *projected_mem_space = NULL; /* If not NULL, ptr to dataspace containing a     */
                                              /* projection of the supplied mem_space to a new  */
                                              /* dataspace with rank equal to that of           */
                                              /* file_space.                                    */
                                              /*                                                */
                                              /* This field is only used if                     */
                                              /* H5S_select_shape_same() returns TRUE when      */
                                              /* comparing the mem_space and the data_space,    */
                                              /* and the mem_space have different rank.         */
                                              /*                                                */
                                              /* Note that this is a temporary variable - the   */
                                              /* projected memory space is stored in dset_info, */
                                              /* and will be freed when that structure is       */
                                              /* freed. */
    H5D_storage_t  store_local;               /* Local buffer for store */
    H5D_storage_t *store = &store_local;      /* Union of EFL and chunk pointer in file space */
    hsize_t        nelmts;                    /* Total number of elmts	*/
    size_t         io_op_init = 0;            /* Number I/O ops that have been initialized */
    size_t         i;                         /* Local index variable */
    char           fake_char;                 /* Temporary variable for NULL buffer pointers */
    herr_t         ret_value = SUCCEED;       /* Return value	*/

    FUNC_ENTER_NOAPI(FAIL)

    /* init io_info */
    io_info.sel_pieces    = NULL;
    io_info.store_faddr   = 0;
    io_info.base_maddr.vp = NULL;
    io_info.is_mdset      = is_mdset;

    /* Start without multi-dataset I/O ops. If we're not using the collective
     * I/O path then we will call the single dataset callbacks in a loop. */
    io_info.md_io_ops.multi_read_md   = NULL;
    io_info.md_io_ops.multi_write_md  = NULL;
    io_info.md_io_ops.single_read_md  = NULL;
    io_info.md_io_ops.single_write_md = NULL;

    /* Create global piece skiplist */
    if (NULL == (io_info.sel_pieces = H5SL_create(H5SL_TYPE_HADDR, NULL)))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTCREATE, FAIL, "can't create skip list for piece selections")

    /* Use provided dset_info */
    io_info.dsets_info = dset_info;

    /* Allocate store buffer if necessary */
    if (count > 1)
        if (NULL == (store = (H5D_storage_t *)H5MM_malloc(count * sizeof(H5D_storage_t))))
            HGOTO_ERROR(H5E_STORAGE, H5E_CANTALLOC, FAIL, "couldn't allocate dset storage info array buffer")

#ifdef H5_HAVE_PARALLEL
    /* Check for non-MPI-based VFD.  Only need to check first dataset since all
     * share the same file. */
    if (!(H5F_HAS_FEATURE(dset_info[0].dset->oloc.file, H5FD_FEAT_HAS_MPI))) {
        H5FD_mpio_xfer_t io_xfer_mode; /* MPI I/O transfer mode */

        /* Get I/O transfer mode */
        if (H5CX_get_io_xfer_mode(&io_xfer_mode) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't get MPI-I/O transfer mode")

        /* Collective access is not permissible without a MPI based VFD */
        if (io_xfer_mode == H5FD_MPIO_COLLECTIVE)
            HGOTO_ERROR(H5E_DATASET, H5E_UNSUPPORTED, FAIL, "collective access for MPI-based drivers only")
    }  /* end if */
#endif /*H5_HAVE_PARALLEL*/

    /* iterate over all dsets and construct I/O information necessary to do I/O */
    for (i = 0; i < count; i++) {
        haddr_t prev_tag = HADDR_UNDEF;

        /* check args */
        if (NULL == dset_info[i].dset)
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset")
        if (NULL == dset_info[i].dset->oloc.file)
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file")

        /* set metadata tagging with dset oheader addr */
        H5AC_tag(dset_info[i].dset->oloc.addr, &prev_tag);

        /* Set up datatype info for operation */
        if (H5D__typeinfo_init(dset_info[i].dset, dset_info[i].mem_type_id, FALSE,
                               &(dset_info[i].type_info)) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to set up type info")
        type_info_init++;

        /* Make certain that the number of elements in each selection is the same */
        nelmts = H5S_GET_SELECT_NPOINTS(dset_info[i].mem_space);
        if (nelmts != H5S_GET_SELECT_NPOINTS(dset_info[i].file_space))
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
                        "src and dest dataspaces have different number of elements selected")

        /* Check for a NULL buffer */
        if (NULL == dset_info[i].buf.vp) {
            /* Check for any elements selected (which is invalid) */
            if (nelmts > 0)
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no output buffer")

            /* If the buffer is nil, and 0 element is selected, make a fake buffer.
             * This is for some MPI package like ChaMPIon on NCSA's tungsten which
             * doesn't support this feature.
             */
            dset_info[i].buf.vp = &fake_char;
        } /* end if */

        /* Make sure that both selections have their extents set */
        if (!(H5S_has_extent(dset_info[i].file_space)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "file dataspace does not have extent set")
        if (!(H5S_has_extent(dset_info[i].mem_space)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "memory dataspace does not have extent set")

        /* H5S_select_shape_same() has been modified to accept topologically identical
         * selections with different rank as having the same shape (if the most
         * rapidly changing coordinates match up), but the I/O code still has
         * difficulties with the notion.
         *
         * To solve this, we check to see if H5S_select_shape_same() returns true,
         * and if the ranks of the mem and file spaces are different.  If they are,
         * construct a new mem space that is equivalent to the old mem space, and
         * use that instead.
         *
         * Note that in general, this requires us to touch up the memory buffer as
         * well.
         */
        if (nelmts > 0 && TRUE == H5S_SELECT_SHAPE_SAME(dset_info[i].mem_space, dset_info[i].file_space) &&
            H5S_GET_EXTENT_NDIMS(dset_info[i].mem_space) != H5S_GET_EXTENT_NDIMS(dset_info[i].file_space)) {
            ptrdiff_t buf_adj = 0;

            /* Attempt to construct projected dataspace for memory dataspace */
            if (H5S_select_construct_projection(dset_info[i].mem_space, &projected_mem_space,
                                                (unsigned)H5S_GET_EXTENT_NDIMS(dset_info[i].file_space),
                                                (hsize_t)dset_info[i].type_info.dst_type_size, &buf_adj) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to construct projected memory dataspace")
            HDassert(projected_mem_space);

            /* Adjust the buffer by the given amount */
            dset_info[i].buf.vp = (void *)(((uint8_t *)dset_info[i].buf.vp) + buf_adj);

            /* Check if we need to free the old mem_space, and make sure we mark
             * the memory dataspace as having been allocated so it gets freed
             * eventually */
            if (dset_info[i].mem_space_alloc) {
                HDassert(dset_info[i].mem_space);
                if (H5S_close(dset_info[i].mem_space) < 0)
                    HGOTO_ERROR(H5E_DATASET, H5E_CANTRELEASE, FAIL,
                                "unable to release temporary memory dataspace")
            }
            else
                dset_info[i].mem_space_alloc = TRUE;

            /* Switch to using projected memory dataspace & adjusted buffer */
            dset_info[i].mem_space = projected_mem_space;
            projected_mem_space    = NULL;
        } /* end if */

        /* If space hasn't been allocated and not using external storage,
         * return fill value to buffer if fill time is upon allocation, or
         * do nothing if fill time is never.  If the dataset is compact and
         * fill time is NEVER, there is no way to tell whether part of data
         * has been overwritten.  So just proceed in reading.
         */
        if (nelmts > 0 && dset_info[i].dset->shared->dcpl_cache.efl.nused == 0 &&
            !(*dset_info[i].dset->shared->layout.ops->is_space_alloc)(
                &dset_info[i].dset->shared->layout.storage) &&
            !(dset_info[i].dset->shared->layout.ops->is_data_cached &&
              (*dset_info[i].dset->shared->layout.ops->is_data_cached)(dset_info[i].dset->shared))) {
            H5D_fill_value_t fill_status; /* Whether/How the fill value is defined */

            /* Retrieve dataset's fill-value properties */
            if (H5P_is_fill_value_defined(&dset_info[i].dset->shared->dcpl_cache.fill, &fill_status) < 0)
                HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't tell if fill value defined")

            /* Should be impossible, but check anyway... */
            if (fill_status == H5D_FILL_VALUE_UNDEFINED &&
                (dset_info[i].dset->shared->dcpl_cache.fill.fill_time == H5D_FILL_TIME_ALLOC ||
                 dset_info[i].dset->shared->dcpl_cache.fill.fill_time == H5D_FILL_TIME_IFSET))
                HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL,
                            "read failed: dataset doesn't exist, no data can be read")

            /* If we're never going to fill this dataset, just leave the junk in the user's buffer */
            if (dset_info[i].dset->shared->dcpl_cache.fill.fill_time == H5D_FILL_TIME_NEVER)
                HGOTO_DONE(SUCCEED)

            /* Go fill the user's selection with the dataset's fill value */
            if (H5D__fill(dset_info[i].dset->shared->dcpl_cache.fill.buf, dset_info[i].dset->shared->type,
                          dset_info[i].buf.vp, dset_info[i].type_info.mem_type, dset_info[i].mem_space) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "filling buf failed")
            else
                HGOTO_DONE(SUCCEED)
        } /* end if */

        /* Set up I/O operation */
        io_info.op_type = H5D_IO_OP_READ;
        if (H5D__ioinfo_init(dset_info[i].dset, &(dset_info[i]), &(store[i]), &io_info) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_UNSUPPORTED, FAIL, "unable to set up I/O operation")

        /* Sanity check that space is allocated, if there are elements */
        if (nelmts > 0)
            HDassert((*dset_info[i].dset->shared->layout.ops->is_space_alloc)(
                         &dset_info[i].dset->shared->layout.storage) ||
                     (dset_info[i].dset->shared->layout.ops->is_data_cached &&
                      (*dset_info[i].dset->shared->layout.ops->is_data_cached)(dset_info[i].dset->shared)) ||
                     dset_info[i].dset->shared->dcpl_cache.efl.nused > 0 ||
                     dset_info[i].dset->shared->layout.type == H5D_COMPACT);

        /* Call storage method's I/O initialization routine */
        if (dset_info[i].layout_ops.io_init &&
            (dset_info[i].layout_ops.io_init)(&io_info, &dset_info[i].type_info, nelmts,
                                              dset_info[i].file_space, dset_info[i].mem_space,
                                              &(dset_info[i])) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "can't initialize I/O info")
        io_op_init++;

        /* Reset metadata tagging */
        H5AC_tag(prev_tag, NULL);
    } /* end of for loop */

    HDassert(type_info_init == count);
    HDassert(io_op_init == count);

#ifdef H5_HAVE_PARALLEL
    /* Adjust I/O info for any parallel I/O */
    if (H5D__ioinfo_adjust(count, &io_info) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to adjust I/O info for parallel I/O")
#endif /*H5_HAVE_PARALLEL*/

    /* If multi dataset I/O callback is not provided, perform read IO via
     * single-dset path with looping */
    if (io_info.md_io_ops.multi_read_md) {
        /* Invoke correct "high level" I/O routine */
        if ((*io_info.md_io_ops.multi_read_md)(count, &io_info) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "can't read data")
    } /* end if */
    else {
        haddr_t prev_tag = HADDR_UNDEF;

        /* Loop with serial & single-dset read IO path */
        for (i = 0; i < count; i++) {
            /* set metadata tagging with dset oheader addr */
            H5AC_tag(dset_info[i].dset->oloc.addr, &prev_tag);

            io_info.dsets_info = &(dset_info[i]);

            /* Invoke correct "high level" I/O routine */
            if ((*dset_info[i].io_ops.multi_read)(
                    &io_info, &(dset_info[i].type_info), H5S_GET_SELECT_NPOINTS(dset_info[i].mem_space),
                    dset_info[i].file_space, dset_info[i].mem_space, &dset_info[i]) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "can't read data")

            /* Reset metadata tagging */
            H5AC_tag(prev_tag, NULL);
        }

        /* Make final multi dataset selection I/O call if we are using both
         * features - in this case the multi_read callbacks did not perform the
         * actual I/O */
        if (is_mdset && io_info.use_select_io && H5D__final_mdset_sel_io(&io_info) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "can't read data")
    }

done:
    /* Shut down the I/O op information */
    for (i = 0; i < io_op_init; i++)
        if (dset_info[i].layout_ops.io_term &&
            (*dset_info[i].layout_ops.io_term)(&io_info, &(dset_info[i])) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CANTCLOSEOBJ, FAIL, "unable to shut down I/O op info")

    /* Shut down datatype info for operation */
    for (i = 0; i < type_info_init; i++)
        if (H5D__typeinfo_term(&(dset_info[i].type_info)) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CANTCLOSEOBJ, FAIL, "unable to shut down type info")

    /* Discard projected mem space if it was created and not placed in dset_info */
    if (projected_mem_space) {
        HDassert(ret_value < 0);
        if (H5S_close(projected_mem_space) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CANTCLOSEOBJ, FAIL, "unable to shut down projected memory dataspace")
    }

    /* Free global piece skiplist */
    if (io_info.sel_pieces)
        if (H5SL_close(io_info.sel_pieces) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTFREE, FAIL, "can't close dataset skip list")

    /* Free store array if it was allocated */
    if (store != &store_local)
        H5MM_free(store);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__read() */

/*-------------------------------------------------------------------------
 * Function:	H5D__write
 *
 * Purpose:	Writes multiple (part of) DATASETs to a file from application
 *          memory BUFs. See H5Dwrite_multi() for complete details.
 *
 *          This was referred from H5D__write for multi-dset work.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D__write(size_t count, H5D_dset_info_t *dset_info, hbool_t is_mdset)
{
    H5D_io_info_t io_info;                    /* Dataset I/O info for multi dsets */
    size_t        type_info_init      = 0;    /* Number of datatype info structs that have been initialized */
    H5S_t        *projected_mem_space = NULL; /* If not NULL, ptr to dataspace containing a     */
                                              /* projection of the supplied mem_space to a new  */
                                              /* dataspace with rank equal to that of           */
                                              /* file_space.                                    */
                                              /*                                                */
                                              /* This field is only used if                     */
                                              /* H5S_select_shape_same() returns TRUE when      */
                                              /* comparing the mem_space and the data_space,    */
                                              /* and the mem_space have different rank.         */
                                              /*                                                */
                                              /* Note that this is a temporary variable - the   */
                                              /* projected memory space is stored in dset_info, */
                                              /* and will be freed when that structure is       */
                                              /* freed. */
    H5D_storage_t  store_local;               /* Local buffer for store */
    H5D_storage_t *store = &store_local;      /* Union of EFL and chunk pointer in file space */
    hsize_t        nelmts;                    /* Total number of elmts	*/
    size_t         io_op_init = 0;            /* Number I/O ops that have been initialized */
    size_t         i;                         /* Local index variable */
    char           fake_char;                 /* Temporary variable for NULL buffer pointers */
    herr_t         ret_value = SUCCEED;       /* Return value	*/

    FUNC_ENTER_NOAPI(FAIL)

    /* Init io_info */
    io_info.sel_pieces     = NULL;
    io_info.store_faddr    = 0;
    io_info.base_maddr.cvp = NULL;
    io_info.is_mdset       = is_mdset;

    /* Start without multi-dataset I/O ops. If we're not using the collective
     * I/O path then we will call the single dataset callbacks in a loop. */
    io_info.md_io_ops.multi_read_md   = NULL;
    io_info.md_io_ops.multi_write_md  = NULL;
    io_info.md_io_ops.single_read_md  = NULL;
    io_info.md_io_ops.single_write_md = NULL;

    /* Create global piece skiplist */
    if (NULL == (io_info.sel_pieces = H5SL_create(H5SL_TYPE_HADDR, NULL)))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTCREATE, FAIL, "can't create skip list for piece selections")

    /* Use provided dset_info */
    io_info.dsets_info = dset_info;

    /* Allocate store buffer if necessary */
    if (count > 1)
        if (NULL == (store = (H5D_storage_t *)H5MM_malloc(count * sizeof(H5D_storage_t))))
            HGOTO_ERROR(H5E_STORAGE, H5E_CANTALLOC, FAIL, "couldn't allocate dset storage info array buffer")

    /* iterate over all dsets and construct I/O information */
    for (i = 0; i < count; i++) {
        hbool_t should_alloc_space = FALSE; /* Whether or not to initialize dataset's storage */
        haddr_t prev_tag           = HADDR_UNDEF;

        /* check args */
        if (NULL == dset_info[i].dset)
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset")
        if (NULL == dset_info[i].dset->oloc.file)
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file")

        /* set metadata tagging with dset oheader addr */
        H5AC_tag(dset_info[i].dset->oloc.addr, &prev_tag);

        /* All filters in the DCPL must have encoding enabled. */
        if (!dset_info[i].dset->shared->checked_filters) {
            if (H5Z_can_apply(dset_info[i].dset->shared->dcpl_id, dset_info[i].dset->shared->type_id) < 0)
                HGOTO_ERROR(H5E_PLINE, H5E_CANAPPLY, FAIL, "can't apply filters")

            dset_info[i].dset->shared->checked_filters = TRUE;
        } /* end if */

        /* Check if we are allowed to write to this file */
        if (0 == (H5F_INTENT(dset_info[i].dset->oloc.file) & H5F_ACC_RDWR))
            HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "no write intent on file")

        /* Set up datatype info for operation */
        if (H5D__typeinfo_init(dset_info[i].dset, dset_info[i].mem_type_id, TRUE, &(dset_info[i].type_info)) <
            0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to set up type info")
        type_info_init++;

        /* Various MPI based checks */
#ifdef H5_HAVE_PARALLEL
        if (H5F_HAS_FEATURE(dset_info[i].dset->oloc.file, H5FD_FEAT_HAS_MPI)) {
            /* If MPI based VFD is used, no VL or region reference datatype support yet. */
            /* This is because they use the global heap in the file and we don't */
            /* support parallel access of that yet */
            if (H5T_is_vl_storage(dset_info[i].type_info.mem_type) > 0)
                HGOTO_ERROR(H5E_DATASET, H5E_UNSUPPORTED, FAIL,
                            "Parallel IO does not support writing VL or region reference datatypes yet")
        } /* end if */
        else {
            H5FD_mpio_xfer_t io_xfer_mode; /* MPI I/O transfer mode */

            /* Get I/O transfer mode */
            if (H5CX_get_io_xfer_mode(&io_xfer_mode) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't get MPI-I/O transfer mode")

            /* Collective access is not permissible without a MPI based VFD */
            if (io_xfer_mode == H5FD_MPIO_COLLECTIVE)
                HGOTO_ERROR(H5E_DATASET, H5E_UNSUPPORTED, FAIL, "collective access for MPI-based driver only")
        } /* end else */
#endif    /*H5_HAVE_PARALLEL*/

        /* Make certain that the number of elements in each selection is the same */
        nelmts = H5S_GET_SELECT_NPOINTS(dset_info[i].mem_space);
        if (nelmts != H5S_GET_SELECT_NPOINTS(dset_info[i].file_space))
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
                        "src and dest dataspaces have different number of elements selected")

        /* Check for a NULL buffer */
        if (NULL == dset_info[i].buf.cvp) {
            /* Check for any elements selected (which is invalid) */
            if (nelmts > 0)
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no output buffer")

            /* If the buffer is nil, and 0 element is selected, make a fake buffer.
             * This is for some MPI package like ChaMPIon on NCSA's tungsten which
             * doesn't support this feature.
             */
            dset_info[i].buf.cvp = &fake_char;
        } /* end if */

        /* Make sure that both selections have their extents set */
        if (!(H5S_has_extent(dset_info[i].file_space)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "file dataspace does not have extent set")
        if (!(H5S_has_extent(dset_info[i].mem_space)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "memory dataspace does not have extent set")

        /* H5S_select_shape_same() has been modified to accept topologically
         * identical selections with different rank as having the same shape
         * (if the most rapidly changing coordinates match up), but the I/O
         * code still has difficulties with the notion.
         *
         * To solve this, we check to see if H5S_select_shape_same() returns
         * true, and if the ranks of the mem and file spaces are different.
         * If they are, construct a new mem space that is equivalent to the
         * old mem space, and use that instead.
         *
         * Note that in general, this requires us to touch up the memory buffer
         * as well.
         */
        if (nelmts > 0 && TRUE == H5S_SELECT_SHAPE_SAME(dset_info[i].mem_space, dset_info[i].file_space) &&
            H5S_GET_EXTENT_NDIMS(dset_info[i].mem_space) != H5S_GET_EXTENT_NDIMS(dset_info[i].file_space)) {
            ptrdiff_t buf_adj = 0;

            /* Attempt to construct projected dataspace for memory dataspace */
            if (H5S_select_construct_projection(dset_info[i].mem_space, &projected_mem_space,
                                                (unsigned)H5S_GET_EXTENT_NDIMS(dset_info[i].file_space),
                                                dset_info[i].type_info.src_type_size, &buf_adj) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to construct projected memory dataspace")
            HDassert(projected_mem_space);

            /* Adjust the buffer by the given amount */
            dset_info[i].buf.cvp = (const void *)(((const uint8_t *)dset_info[i].buf.cvp) + buf_adj);

            /* Check if we need to free the old mem_space, and make sure we mark
             * the memory dataspace as having been allocated so it gets freed
             * eventually */
            if (dset_info[i].mem_space_alloc) {
                HDassert(dset_info[i].mem_space);
                if (H5S_close(dset_info[i].mem_space) < 0)
                    HGOTO_ERROR(H5E_DATASET, H5E_CANTRELEASE, FAIL,
                                "unable to release temporary memory dataspace")
            }
            else
                dset_info[i].mem_space_alloc = TRUE;

            /* Switch to using projected memory dataspace & adjusted buffer */
            dset_info[i].mem_space = projected_mem_space;
            projected_mem_space    = NULL;
        } /* end if */

        /* Retrieve dataset properties */
        /* <none needed currently> */

        /* Set up I/O operation */
        io_info.op_type = H5D_IO_OP_WRITE;
        if (H5D__ioinfo_init(dset_info[i].dset, &(dset_info[i]), &(store[i]), &io_info) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to set up I/O operation")

        /* Allocate dataspace and initialize it if it hasn't been. */
        should_alloc_space = dset_info[i].dset->shared->dcpl_cache.efl.nused == 0 &&
                             !(*dset_info[i].dset->shared->layout.ops->is_space_alloc)(
                                 &dset_info[i].dset->shared->layout.storage);

        /*
         * If not using an MPI-based VFD, we only need to allocate
         * and initialize storage if there's a selection in the
         * dataset's dataspace. Otherwise, we always need to participate
         * in the storage allocation since this may use collective
         * operations and we will hang if we don't participate.
         */
        if (!H5F_HAS_FEATURE(dset_info[i].dset->oloc.file, H5FD_FEAT_HAS_MPI))
            should_alloc_space = should_alloc_space && (nelmts > 0);

        if (should_alloc_space) {
            hssize_t file_nelmts;    /* Number of elements in file dataset's dataspace */
            hbool_t  full_overwrite; /* Whether we are over-writing all the elements */

            /* Get the number of elements in file dataset's dataspace */
            if ((file_nelmts = H5S_GET_EXTENT_NPOINTS(dset_info[i].file_space)) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_BADVALUE, FAIL,
                            "can't retrieve number of elements in file dataset")

            /* Always allow fill values to be written if the dataset has a VL datatype */
            if (H5T_detect_class(dset_info[i].dset->shared->type, H5T_VLEN, FALSE))
                full_overwrite = FALSE;
            else
                full_overwrite = (hbool_t)((hsize_t)file_nelmts == nelmts ? TRUE : FALSE);

            io_info.dset = dset_info[i].dset;
            /* Allocate storage */
            if (H5D__alloc_storage(&io_info, H5D_ALLOC_WRITE, full_overwrite, NULL) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to initialize storage")
        } /* end if */

        /* Call storage method's I/O initialization routine */
        /* Init io_info.dset_info[] and generate piece_info in skip list */
        if (dset_info[i].layout_ops.io_init &&
            (*dset_info[i].layout_ops.io_init)(&io_info, &(dset_info[i].type_info), nelmts,
                                               dset_info[i].file_space, dset_info[i].mem_space,
                                               &(dset_info[i])) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "can't initialize I/O info")
        io_op_init++;

        /* Reset metadata tagging */
        H5AC_tag(prev_tag, NULL);
    } /* end of for loop */

    HDassert(type_info_init == count);
    HDassert(io_op_init == count);

#ifdef H5_HAVE_PARALLEL
    /* Adjust I/O info for any parallel I/O */
    if (H5D__ioinfo_adjust(count, &io_info) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to adjust I/O info for parallel I/O")
#endif /*H5_HAVE_PARALLEL*/

    /* If multi dataset I/O callback is not provided, perform write IO via
     * single-dset path with looping */
    if (io_info.md_io_ops.multi_write_md) {
        /* Invoke correct "high level" I/O routine */
        if ((*io_info.md_io_ops.multi_write_md)(count, &io_info) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "can't write data")
    } /* end if */
    else {
        haddr_t prev_tag = HADDR_UNDEF;

        /* loop with serial & single-dset write IO path */
        for (i = 0; i < count; i++) {
            /* set metadata tagging with dset oheader addr */
            H5AC_tag(dset_info->dset->oloc.addr, &prev_tag);

            io_info.dsets_info = &(dset_info[i]);

            /* Invoke correct "high level" I/O routine */
            if ((*dset_info[i].io_ops.multi_write)(
                    &io_info, &(dset_info[i].type_info), H5S_GET_SELECT_NPOINTS(dset_info[i].mem_space),
                    dset_info[i].file_space, dset_info[i].mem_space, &dset_info[i]) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "can't write data")

            /* Reset metadata tagging */
            H5AC_tag(prev_tag, NULL);
        }

        /* Make final multi dataset selection I/O call if we are using both
         * features - in this case the multi_read callbacks did not perform the
         * actual I/O */
        if (is_mdset && io_info.use_select_io && H5D__final_mdset_sel_io(&io_info) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "can't read data")
    }

#ifdef OLD_WAY
    /*
     * This was taken out because it can be called in a parallel program with
     * independent access, causing the metadata cache to get corrupted. Its been
     * disabled for all types of access (serial as well as parallel) to make the
     * modification time consistent for all programs. -QAK
     *
     * We should set a value in the dataset's shared information instead and flush
     * it to the file when the dataset is being closed. -QAK
     */
    /*
     * Update modification time.  We have to do this explicitly because
     * writing to a dataset doesn't necessarily change the object header.
     */
    if (H5O_touch(&(dataset->oloc), FALSE) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to update modification time")
#endif /* OLD_WAY */

done:
    /* Shut down the I/O op information */
    for (i = 0; i < io_op_init; i++)
        if (dset_info[i].layout_ops.io_term &&
            (*dset_info[i].layout_ops.io_term)(&io_info, &(dset_info[i])) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CANTCLOSEOBJ, FAIL, "unable to shut down I/O op info")

    /* Shut down datatype info for operation */
    for (i = 0; i < type_info_init; i++)
        if (H5D__typeinfo_term(&(dset_info[i].type_info)) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CANTCLOSEOBJ, FAIL, "unable to shut down type info")

    /* Discard projected mem space if it was created and not placed in dset_info */
    if (projected_mem_space) {
        HDassert(ret_value < 0);
        if (H5S_close(projected_mem_space) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CANTCLOSEOBJ, FAIL, "unable to shut down projected memory dataspace")
    }

    /* Free global piece skiplist */
    if (io_info.sel_pieces)
        if (H5SL_close(io_info.sel_pieces) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTFREE, FAIL, "can't close dataset skip list")

    /* Free store array if it was allocated */
    if (store != &store_local)
        H5MM_free(store);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__write */

/*-------------------------------------------------------------------------
 * Function:	H5D__ioinfo_init
 *
 * Purpose:	Routine for determining correct I/O operations for each I/O action.
 *
 *          This was derived from H5D__ioinfo_init for multi-dset work.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__ioinfo_init(H5D_t *dset, H5D_dset_info_t *dset_info, H5D_storage_t *store, H5D_io_info_t *io_info)
{
    FUNC_ENTER_PACKAGE_NOERR

    /* check args */
    HDassert(dset);
    HDassert(dset->oloc.file);
    // HDassert(&(dset_info->type_info));
    HDassert(dset_info->type_info.tpath);
    HDassert(io_info);

    /* Set up "normal" I/O fields */
    dset_info->dset  = dset;
    io_info->f_sh    = H5F_SHARED(dset->oloc.file);
    dset_info->store = store;

    /* Set I/O operations to initial values */
    dset_info->layout_ops = *dset->shared->layout.ops;

    /* Set the "high-level" I/O operations for the dataset */
    dset_info->io_ops.multi_read  = dset->shared->layout.ops->ser_read;
    dset_info->io_ops.multi_write = dset->shared->layout.ops->ser_write;

    /* Set the I/O operations for reading/writing single blocks on disk */
    if (dset_info->type_info.is_xform_noop && dset_info->type_info.is_conv_noop) {
        /*
         * If there is no data transform or type conversion then read directly
         * into the application's buffer.
         * This saves at least one mem-to-mem copy.
         */
        dset_info->io_ops.single_read  = H5D__select_read;
        dset_info->io_ops.single_write = H5D__select_write;
    } /* end if */
    else {
        /*
         * This is the general case (type conversion, usually).
         */
        dset_info->io_ops.single_read  = H5D__scatgath_read;
        dset_info->io_ops.single_write = H5D__scatgath_write;
    } /* end else */

    /* Start with selection I/O on if the global is on, layout callback will
     * turn it off if appropriate */
    io_info->use_select_io = H5_use_selection_io_g;

#ifdef H5_HAVE_PARALLEL
    /* Determine if the file was opened with an MPI VFD */
    io_info->using_mpi_vfd = H5F_HAS_FEATURE(dset->oloc.file, H5FD_FEAT_HAS_MPI);
#endif /* H5_HAVE_PARALLEL */

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5D__ioinfo_init() */

/*-------------------------------------------------------------------------
 * Function:	H5D__typeinfo_init
 *
 * Purpose:	Routine for determining correct datatype information for
 *              each I/O action.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Tuesday, March  4, 2008
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__typeinfo_init(const H5D_t *dset, hid_t mem_type_id, hbool_t do_write, H5D_type_info_t *type_info)
{
    const H5T_t      *src_type;            /* Source datatype */
    const H5T_t      *dst_type;            /* Destination datatype */
    H5Z_data_xform_t *data_transform;      /* Data transform info */
    herr_t            ret_value = SUCCEED; /* Return value	*/

    FUNC_ENTER_PACKAGE

    /* check args */
    HDassert(type_info);
    HDassert(dset);

    /* Patch the top level file pointer for dt->shared->u.vlen.f if needed */
    if (H5T_patch_vlen_file(dset->shared->type, H5F_VOL_OBJ(dset->oloc.file)) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTOPENOBJ, FAIL, "can't patch VL datatype file pointer")

    /* Initialize type info safely */
    HDmemset(type_info, 0, sizeof(*type_info));

    /* Get the memory & dataset datatypes */
    if (NULL == (type_info->mem_type = (const H5T_t *)H5I_object_verify(mem_type_id, H5I_DATATYPE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a datatype")
    type_info->dset_type = dset->shared->type;

    if (do_write) {
        src_type               = type_info->mem_type;
        dst_type               = dset->shared->type;
        type_info->src_type_id = mem_type_id;
        type_info->dst_type_id = dset->shared->type_id;
    } /* end if */
    else {
        src_type               = dset->shared->type;
        dst_type               = type_info->mem_type;
        type_info->src_type_id = dset->shared->type_id;
        type_info->dst_type_id = mem_type_id;
    } /* end else */

    /* Locate the type conversion function and dataspace conversion
     * functions, and set up the element numbering information. If a data
     * type conversion is necessary then register datatype IDs. Data type
     * conversion is necessary if the user has set the `need_bkg' to a high
     * enough value in xfer_parms since turning off datatype conversion also
     * turns off background preservation.
     */
    if (NULL == (type_info->tpath = H5T_path_find(src_type, dst_type)))
        HGOTO_ERROR(H5E_DATASET, H5E_UNSUPPORTED, FAIL, "unable to convert between src and dest datatype")

    /* Retrieve info from API context */
    if (H5CX_get_data_transform(&data_transform) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't get data transform info")

    /* Precompute some useful information */
    type_info->src_type_size = H5T_get_size(src_type);
    type_info->dst_type_size = H5T_get_size(dst_type);
    type_info->max_type_size = MAX(type_info->src_type_size, type_info->dst_type_size);
    type_info->is_conv_noop  = H5T_path_noop(type_info->tpath);
    type_info->is_xform_noop = H5Z_xform_noop(data_transform);
    if (type_info->is_xform_noop && type_info->is_conv_noop) {
        type_info->cmpd_subset = NULL;
        type_info->need_bkg    = H5T_BKG_NO;
    } /* end if */
    else {
        void     *tconv_buf;     /* Temporary conversion buffer pointer */
        void     *bkgr_buf;      /* Background conversion buffer pointer */
        size_t    max_temp_buf;  /* Maximum temporary buffer size */
        H5T_bkg_t bkgr_buf_type; /* Background buffer type */
        size_t    target_size;   /* Desired buffer size	*/

        /* Get info from API context */
        if (H5CX_get_max_temp_buf(&max_temp_buf) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't retrieve max. temp. buf size")
        if (H5CX_get_tconv_buf(&tconv_buf) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't retrieve temp. conversion buffer pointer")
        if (H5CX_get_bkgr_buf(&bkgr_buf) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't retrieve background conversion buffer pointer")
        if (H5CX_get_bkgr_buf_type(&bkgr_buf_type) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't retrieve background buffer type")

        /* Check if the datatypes are compound subsets of one another */
        type_info->cmpd_subset = H5T_path_compound_subset(type_info->tpath);

        /* Check if we need a background buffer */
        if (do_write && H5T_detect_class(dset->shared->type, H5T_VLEN, FALSE))
            type_info->need_bkg = H5T_BKG_YES;
        else {
            H5T_bkg_t path_bkg; /* Type conversion's background info */

            if ((path_bkg = H5T_path_bkg(type_info->tpath))) {
                /* Retrieve the bkgr buffer property */
                type_info->need_bkg = bkgr_buf_type;
                type_info->need_bkg = MAX(path_bkg, type_info->need_bkg);
            } /* end if */
            else
                type_info->need_bkg = H5T_BKG_NO; /*never needed even if app says yes*/
        }                                         /* end else */

        /* Set up datatype conversion/background buffers */

        target_size = max_temp_buf;

        /* If the buffer is too small to hold even one element, try to make it bigger */
        if (target_size < type_info->max_type_size) {
            hbool_t default_buffer_info; /* Whether the buffer information are the defaults */

            /* Detect if we have all default settings for buffers */
            default_buffer_info =
                (hbool_t)((H5D_TEMP_BUF_SIZE == max_temp_buf) && (NULL == tconv_buf) && (NULL == bkgr_buf));

            /* Check if we are using the default buffer info */
            if (default_buffer_info)
                /* OK to get bigger for library default settings */
                target_size = type_info->max_type_size;
            else
                /* Don't get bigger than the application has requested */
                HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "temporary buffer max size is too small")
        } /* end if */

        /* Compute the number of elements that will fit into buffer */
        type_info->request_nelmts = target_size / type_info->max_type_size;

        /* Sanity check elements in temporary buffer */
        if (type_info->request_nelmts == 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "temporary buffer max size is too small")

        /* Get a temporary buffer for type conversion unless the app has already
         * supplied one through the xfer properties. Instead of allocating a
         * buffer which is the exact size, we allocate the target size.
         */
        if (NULL == (type_info->tconv_buf = (uint8_t *)tconv_buf)) {
            /* Allocate temporary buffer */
            if (NULL == (type_info->tconv_buf = H5FL_BLK_CALLOC(type_conv, target_size)))
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for type conversion")
            type_info->tconv_buf_allocated = TRUE;
        } /* end if */
        if (type_info->need_bkg && NULL == (type_info->bkg_buf = (uint8_t *)bkgr_buf)) {
            size_t bkg_size; /* Desired background buffer size	*/

            /* Compute the background buffer size */
            /* (don't try to use buffers smaller than the default size) */
            bkg_size = type_info->request_nelmts * type_info->dst_type_size;
            if (bkg_size < max_temp_buf)
                bkg_size = max_temp_buf;

            /* Allocate background buffer */
            /* (Need calloc()-like call since memory needs to be initialized) */
            if (NULL == (type_info->bkg_buf = H5FL_BLK_CALLOC(type_conv, bkg_size)))
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL,
                            "memory allocation failed for background conversion")
            type_info->bkg_buf_allocated = TRUE;
        } /* end if */
    }     /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__typeinfo_init() */

#ifdef H5_HAVE_PARALLEL

/*-------------------------------------------------------------------------
 * Function:	H5D__ioinfo_adjust
 *
 * Purpose:	Adjust operation's I/O info for any parallel I/O
 *
 *          This was derived from H5D__ioinfo_adjust for multi-dset work.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__ioinfo_adjust(const size_t count, H5D_io_info_t *io_info)
{
    H5D_t *dset0;               /* only the first dset , also for single dsets case */
    herr_t ret_value = SUCCEED; /* Return value	*/

    FUNC_ENTER_PACKAGE

    /* check args */
    HDassert(io_info);

    /* check the first dset, should exist either single or multi dset cases */
    HDassert(io_info->dsets_info[0].dset);
    dset0 = io_info->dsets_info[0].dset;
    HDassert(dset0->oloc.file);

    /* Reset the actual io mode properties to the default values in case
     * the DXPL (if it's non-default) was previously used in a collective
     * I/O operation.
     */
    if (!H5CX_is_def_dxpl()) {
        H5CX_set_mpio_actual_chunk_opt(H5D_MPIO_NO_CHUNK_OPTIMIZATION);
        H5CX_set_mpio_actual_io_mode(H5D_MPIO_NO_COLLECTIVE);
    } /* end if */

    /* Make any parallel I/O adjustments */
    if (io_info->using_mpi_vfd) {
        H5FD_mpio_xfer_t xfer_mode; /* Parallel transfer for this request */
        htri_t           opt;       /* Flag whether a selection is optimizable */

        /* Get the original state of parallel I/O transfer mode */
        if (H5CX_get_io_xfer_mode(&xfer_mode) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't get MPI-I/O transfer mode")

        /* Get MPI communicator */
        if (MPI_COMM_NULL == (io_info->comm = H5F_mpi_get_comm(dset0->oloc.file)))
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTGET, FAIL, "can't retrieve MPI communicator")
        /* Check if we can set direct MPI-IO read/write functions */
        if ((opt = H5D__mpio_opt_possible(count, io_info)) < 0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "invalid check for direct IO dataspace ")

        /* Check if we can use the optimized parallel I/O routines */
        if (opt == TRUE) {
            /* Override the I/O op pointers to the MPI-specific routines, unless
             * selection I/O is to be used - in this case the file driver will
             * handle collective I/O */
            /* Check for selection/vector support in file driver? -NAF */
            if (!io_info->use_select_io) {
                io_info->md_io_ops.multi_read_md   = dset0->shared->layout.ops->par_read;
                io_info->md_io_ops.multi_write_md  = dset0->shared->layout.ops->par_write;
                io_info->md_io_ops.single_read_md  = H5D__mpio_select_read;
                io_info->md_io_ops.single_write_md = H5D__mpio_select_write;
            } /* end if */
        }     /* end if */
        else {
            /* Fail when file sync is required, since it requires collective write */
            if (io_info->op_type == H5D_IO_OP_WRITE) {
                hbool_t mpi_file_sync_required = FALSE;
                if (H5F_shared_get_mpi_file_sync_required(io_info->f_sh, &mpi_file_sync_required) < 0)
                    HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't get MPI file_sync_required flag")

                if (mpi_file_sync_required)
                    HGOTO_ERROR(
                        H5E_DATASET, H5E_NO_INDEPENDENT, FAIL,
                        "Can't perform independent write when MPI_File_sync is required by ROMIO driver.")
            }

            /* Check if there are any filters in the pipeline. If there are,
             * we cannot break to independent I/O if this is a write operation
             * with multiple ranks involved; otherwise, there will be metadata
             * inconsistencies in the file.
             */
            if (io_info->op_type == H5D_IO_OP_WRITE &&
                io_info->dsets_info[0].dset->shared->dcpl_cache.pline.nused > 0) { /*!FIXME -NAF */
                int comm_size = 0;

                /* Retrieve size of MPI communicator used for file */
                if ((comm_size = H5F_shared_mpi_get_size(io_info->f_sh)) < 0)
                    HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "can't get MPI communicator size")

                if (comm_size > 1) {
                    char local_no_coll_cause_string[512];
                    char global_no_coll_cause_string[512];

                    if (H5D__mpio_get_no_coll_cause_strings(local_no_coll_cause_string, 512,
                                                            global_no_coll_cause_string, 512) < 0)
                        HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL,
                                    "can't get reasons for breaking collective I/O")

                    HGOTO_ERROR(H5E_IO, H5E_NO_INDEPENDENT, FAIL,
                                "Can't perform independent write with filters in pipeline.\n"
                                "    The following caused a break from collective I/O:\n"
                                "        Local causes: %s\n"
                                "        Global causes: %s",
                                local_no_coll_cause_string, global_no_coll_cause_string);
                }
            }

            /* If we won't be doing collective I/O, but the user asked for
             * collective I/O, change the request to use independent I/O
             */
            if (xfer_mode == H5FD_MPIO_COLLECTIVE) {
                /* Change the xfer_mode to independent for handling the I/O */
                if (H5CX_set_io_xfer_mode(H5FD_MPIO_INDEPENDENT) < 0)
                    HGOTO_ERROR(H5E_DATASET, H5E_CANTSET, FAIL, "can't set MPI-I/O transfer mode")
            } /* end if */
        }     /* end else */
    }         /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__ioinfo_adjust() */
#endif /* H5_HAVE_PARALLEL */

/*-------------------------------------------------------------------------
 * Function:    H5D__final_mdset_sel_io
 *
 * Purpose:     Routine for determining correct datatype information for
 *              each I/O action.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__final_mdset_sel_io(H5D_io_info_t *io_info)
{
    H5S_t           **mem_spaces    = NULL; /* Array of chunk memory spaces */
    H5S_t           **file_spaces   = NULL; /* Array of chunk file spaces */
    haddr_t          *addrs         = NULL; /* Array of chunk addresses */
    size_t           *element_sizes = NULL; /* Array of element sizes */
    void            **rbufs         = NULL; /* Array of read buffers */
    const void      **wbufs         = NULL; /* Array of write buffers */
    size_t            num_pieces;           /* Number of pieces */
    H5SL_node_t      *piece_node;           /* Skiplist node for piece */
    H5D_piece_info_t *piece_info;           /* Info for current piece */
    size_t            i         = 0;        /* Local index */
    herr_t            ret_value = SUCCEED;  /* Return value */

    FUNC_ENTER_PACKAGE

    /* check args */
    HDassert(io_info);

    /* Count number of pieces in I/O */
    num_pieces = H5SL_count(io_info->sel_pieces);

    if (num_pieces) {
        /* Allocate arrays of dataspaces, offsets, sizes, and buffers for use with
         * selection I/O */
        if (NULL == (mem_spaces = H5MM_malloc(num_pieces * sizeof(H5S_t *))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "memory allocation failed for memory space list")
        if (NULL == (file_spaces = H5MM_malloc(num_pieces * sizeof(H5S_t *))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "memory allocation failed for file space list")
        if (NULL == (addrs = H5MM_malloc(num_pieces * sizeof(haddr_t))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "memory allocation failed for piece address list")
        if (NULL == (element_sizes = H5MM_malloc(num_pieces * sizeof(size_t))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "memory allocation failed for element size list")
        if (io_info->op_type == H5D_IO_OP_READ) {
            if (NULL == (rbufs = H5MM_malloc(num_pieces * sizeof(void *))))
                HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL,
                            "memory allocation failed for read buffer list")
        }
        else if (NULL == (wbufs = H5MM_malloc(num_pieces * sizeof(void *))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "memory allocation failed for write buffer list")

        /* Iterate through skiplist of pieces */
        piece_node = H5SL_first(io_info->sel_pieces);
        while (piece_node) {
            /* Get piece info */
            piece_info = H5SL_item(piece_node);
            HDassert(piece_info);

            /* Only valid pieces should have been inserted into this list */
            HDassert(piece_info->mspace);
            HDassert(piece_info->fspace);
            HDassert(piece_info->faddr);
            HDassert(piece_info->dset_info->type_info.src_type_size);
            HDassert(piece_info->dset_info->type_info.src_type_size ==
                     piece_info->dset_info->type_info.dst_type_size);
            HDassert(io_info->op_type == H5D_IO_OP_READ ? piece_info->dset_info->buf.vp
                                                        : piece_info->dset_info->buf.cvp);

            /* Add this piece to selection I/O arrays */
            mem_spaces[i]    = piece_info->mspace;
            file_spaces[i]   = piece_info->fspace;
            addrs[i]         = piece_info->faddr;
            element_sizes[i] = piece_info->dset_info->type_info.src_type_size;
            if (io_info->op_type == H5D_IO_OP_READ)
                rbufs[i] = piece_info->dset_info->buf.vp;
            else
                wbufs[i] = piece_info->dset_info->buf.cvp;

            /* Advance to next piece */
            piece_node = H5SL_next(piece_node);
            i++;
        }
    }

    /* Issue appropriate lower level selection I/O call (we can skip the page
     * buffer because we've already verified it won't be used, and the metadata
     * accumulator because this is raw data) */
    if (io_info->op_type == H5D_IO_OP_READ) {
        if (H5F_shared_select_read(H5F_SHARED(io_info->dsets_info[0].dset->oloc.file), H5FD_MEM_DRAW,
                                   (uint32_t)num_pieces, mem_spaces, file_spaces, addrs, element_sizes,
                                   rbufs) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "chunk selection read failed")
    }
    else if (H5F_shared_select_write(H5F_SHARED(io_info->dsets_info[0].dset->oloc.file), H5FD_MEM_DRAW,
                                     (uint32_t)num_pieces, mem_spaces, file_spaces, addrs, element_sizes,
                                     wbufs) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "chunk selection write failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__final_mdset_sel_io() */

/*-------------------------------------------------------------------------
 * Function:    H5D__typeinfo_term
 *
 * Purpose:     Common logic for terminating a type info object
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              Thursday, March  6, 2008
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__typeinfo_term(const H5D_type_info_t *type_info)
{
    FUNC_ENTER_PACKAGE_NOERR

    /* Check for releasing datatype conversion & background buffers */
    if (type_info->tconv_buf_allocated) {
        HDassert(type_info->tconv_buf);
        (void)H5FL_BLK_FREE(type_conv, type_info->tconv_buf);
    } /* end if */
    if (type_info->bkg_buf_allocated) {
        HDassert(type_info->bkg_buf);
        (void)H5FL_BLK_FREE(type_conv, type_info->bkg_buf);
    } /* end if */

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5D__typeinfo_term() */
