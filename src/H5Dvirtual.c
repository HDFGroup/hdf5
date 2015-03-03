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
 * Programmer:  Neil Fortner <nfortne2@hdfgroup.org>
 *              Wednesday, January 28, 2015
 *
 * Purpose:
 *      VDSINC
 */

/****************/
/* Module Setup */
/****************/

#define H5D_PACKAGE             /* Suppress error about including H5Dpkg */


/***********/
/* Headers */
/***********/
#include "H5private.h"          /* Generic Functions                    */
#include "H5Dpkg.h"             /* Dataset functions                    */
#include "H5Eprivate.h"         /* Error handling                       */
#include "H5Fprivate.h"         /* Files                                */
#include "H5HGprivate.h"        /* Global Heaps                         */
#include "H5MMprivate.h"        /* Memory management                    */
#include "H5Oprivate.h"         /* Object headers                       */
#include "H5Sprivate.h"         /* Dataspaces                           */


/****************/
/* Local Macros */
/****************/


/******************/
/* Local Typedefs */
/******************/


/********************/
/* Local Prototypes */
/********************/

/* Layout operation callbacks */
static herr_t H5D__virtual_construct(H5F_t *f, H5D_t *dset);
static herr_t H5D__virtual_io_init(const H5D_io_info_t *io_info,
    const H5D_type_info_t *type_info, hsize_t nelmts, const H5S_t *file_space,
    const H5S_t *mem_space, H5D_chunk_map_t *cm);
static herr_t H5D__virtual_read(H5D_io_info_t *io_info, const H5D_type_info_t
    *type_info, hsize_t nelmts, const H5S_t *file_space, const H5S_t *mem_space,
    H5D_chunk_map_t *fm);
static herr_t H5D__virtual_write(H5D_io_info_t *io_info,
    const H5D_type_info_t *type_info, hsize_t nelmts, const H5S_t *file_space,
    const H5S_t *mem_space, H5D_chunk_map_t *fm);
static herr_t H5D__virtual_flush(H5D_t *dset, hid_t dxpl_id);


/*********************/
/* Package Variables */
/*********************/

/* Contiguous storage layout I/O ops */
const H5D_layout_ops_t H5D_LOPS_VIRTUAL[1] = {{
    H5D__virtual_construct,
    NULL,
    H5D__virtual_is_space_alloc,
    H5D__virtual_io_init,
    H5D__virtual_read,
    H5D__virtual_write,
#ifdef H5_HAVE_PARALLEL
    H5D__virtual_collective_read, //VDSINC
    H5D__virtual_collective_write, //VDSINC
#endif /* H5_HAVE_PARALLEL */
    NULL,
    NULL,
    H5D__virtual_flush,
    NULL
}};


/*******************/
/* Local Variables */
/*******************/



/*-------------------------------------------------------------------------
 * Function:    H5D__virtual_copy_layout
 *
 * Purpose:     Deep copies virtual storage layout message in memory.
 *              This function assumes that the top-level struct has
 *              already been copied (so the source struct retains
 *              ownership of the fields passed to this function).
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Neil Fortner
 *              February 10, 2015
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D__virtual_copy_layout(H5O_layout_t *layout)
{
    H5O_storage_virtual_ent_t *orig_list = NULL;
    size_t          i;
    herr_t          ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE

    HDassert(layout);
    HDassert(layout->type == H5D_VIRTUAL);

    if(layout->storage.u.virt.list_nused > 0) {
        HDassert(layout->storage.u.virt.list);

        /* Save original entry list for use as the "source" */
        orig_list = layout->storage.u.virt.list;

        /* Allocate memory for the list */
        if(NULL == (layout->storage.u.virt.list = (H5O_storage_virtual_ent_t *)H5MM_calloc(layout->storage.u.virt.list_nused * sizeof(H5O_storage_virtual_ent_t))))
            HGOTO_ERROR(H5E_DATASET, H5E_NOSPACE, FAIL, "unable to allocate memory for virtual dataset entry list")
        layout->storage.u.virt.list_nalloc = layout->storage.u.virt.list_nused;

        /* Copy the list entries */
        for(i = 0; i < layout->storage.u.virt.list_nused; i++) {
            if(NULL == (layout->storage.u.virt.list[i].source_file_name
                    = HDstrdup(orig_list[i].source_file_name)))
                HGOTO_ERROR(H5E_DATASET, H5E_RESOURCE, FAIL, "unable to duplicate source file name")
            if(NULL == (layout->storage.u.virt.list[i].source_dset_name
                    = HDstrdup(orig_list[i].source_dset_name)))
                HGOTO_ERROR(H5E_DATASET, H5E_RESOURCE, FAIL, "unable to duplicate source dataset name")
            if(NULL == (layout->storage.u.virt.list[i].source_select
                    = H5S_copy(orig_list[i].source_select, FALSE, TRUE)))
                HGOTO_ERROR(H5E_DATASET, H5E_CANTCOPY, FAIL, "unable to copy source selection")
            if(NULL == (layout->storage.u.virt.list[i].virtual_select
                    = H5S_copy(orig_list[i].virtual_select, FALSE, TRUE)))
                HGOTO_ERROR(H5E_DATASET, H5E_CANTCOPY, FAIL, "unable to copy virtual selection")
        } /* end for */
    } /* end if */
    else {
        HDassert(0 && "checking code coverage...");//VDSINC
        /* Zero out other fields related to list, just to be sure */
        layout->storage.u.virt.list = NULL;
        layout->storage.u.virt.list_nalloc = 0;
    } /* end else */

done:
    /* Release allocated resources on failure */
    if((ret_value < 0) && orig_list
            && (orig_list != layout->storage.u.virt.list))
        if(H5D__virtual_reset_layout(layout) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CANTFREE, FAIL, "unable to reset virtual layout")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__virtual_copy_layout() */


/*-------------------------------------------------------------------------
 * Function:    H5D__virtual_reset_layout
 *
 * Purpose:     Frees internal structures in a virtual storage layout
 *              message in memory.  This function is safe to use on
 *              incomplete structures (for recovery from failure) provided
 *              the internal structures are initialized with all bytes set
 *              to 0.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Neil Fortner
 *              February 11, 2015
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D__virtual_reset_layout(H5O_layout_t *layout)
{
    size_t          i;
    herr_t          ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE

    HDassert(layout);
    HDassert(layout->type == H5D_VIRTUAL);

    /* Free the list entries.  Note we always attempt to free everything even in
     * the case of a failure. */
    for(i = 0; i < layout->storage.u.virt.list_nused; i++) {
        layout->storage.u.virt.list[i].source_file_name = (char *)H5MM_xfree(layout->storage.u.virt.list[i].source_file_name);
        layout->storage.u.virt.list[i].source_dset_name = (char *)H5MM_xfree(layout->storage.u.virt.list[i].source_dset_name);
        if(layout->storage.u.virt.list[i].source_select
                && H5S_close(layout->storage.u.virt.list[i].source_select) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CLOSEERROR, FAIL, "unable to release source selection")
        layout->storage.u.virt.list[i].source_select = NULL;
        if(layout->storage.u.virt.list[i].virtual_select
                && H5S_close(layout->storage.u.virt.list[i].virtual_select) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CLOSEERROR, FAIL, "unable to release virtual selection")
        layout->storage.u.virt.list[i].virtual_select = NULL;
        /* Close dataset here?  VDSINC */
    } /* end for */

    /* Free the list */
    layout->storage.u.virt.list = (H5O_storage_virtual_ent_t *)H5MM_xfree(layout->storage.u.virt.list);

    /* Note the lack of a done: label.  This is because there are no HGOTO_ERROR
     * calls.  If one is added, a done: label must also be added */
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__virtual_reset_layout() */


/*-------------------------------------------------------------------------
 * Function:    H5D__virtual_delete
 *
 * Purpose:     Delete the file space for a virtual dataset
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Neil Fortner
 *              February 6, 2015
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D__virtual_delete(H5F_t *f, hid_t dxpl_id, H5O_storage_t *storage)
{
    herr_t ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_PACKAGE

    /* check args */
    HDassert(f);
    HDassert(storage);
    HDassert(storage->type == H5D_VIRTUAL);

    /* Need to add stuff for private data here VDSINC */

    /* Delete the global heap block */
    if(H5HG_remove(f, dxpl_id, (H5HG_t *)&(storage->u.virt.serial_list_hobjid)) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "Unable to remove heap object")

    /* Clear global heap ID in storage */
    storage->u.virt.serial_list_hobjid.addr = HADDR_UNDEF;
    storage->u.virt.serial_list_hobjid.idx = 0;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__virtual_delete */


/*-------------------------------------------------------------------------
 * Function:    H5D__virtual_construct
 *
 * Purpose:     Constructs new virtual layout information for dataset
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Neil Fortner
 *              Monday, February 2, 2015
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__virtual_construct(H5F_t UNUSED *f, H5D_t UNUSED *dset)
{
    FUNC_ENTER_STATIC_NOERR

    /* No-op for now VDSINC */

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5D__virtual_construct() */


/*-------------------------------------------------------------------------
 * Function:    H5D__virtual_is_space_alloc
 *
 * Purpose:     Query if space is allocated for layout
 *
 * Return:      TRUE if space is allocated
 *              FALSE if it is not
 *              Negative on failure
 *
 * Programmer:  Neil Fortner
 *              February 6, 2015
 *
 *-------------------------------------------------------------------------
 */
hbool_t
H5D__virtual_is_space_alloc(const H5O_storage_t *storage)
{
    hbool_t ret_value;                  /* Return value */

    FUNC_ENTER_PACKAGE_NOERR

    /* Need to decide what to do here.  For now just return TRUE if the global
     * heap block has been allocated.  VDSINC */
    ret_value = storage->u.virt.serial_list_hobjid.addr != HADDR_UNDEF;

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__virtual_is_space_alloc() */


/*-------------------------------------------------------------------------
 * Function:    H5D__virtual_io_init
 *
 * Purpose:     Performs initialization before any sort of I/O on the raw data
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Neil Fortner
 *              February 6, 2015
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__virtual_io_init(const H5D_io_info_t UNUSED *io_info, const H5D_type_info_t UNUSED *type_info,
    hsize_t UNUSED nelmts, const H5S_t UNUSED *file_space, const H5S_t UNUSED *mem_space,
    H5D_chunk_map_t UNUSED *cm)
{
    FUNC_ENTER_STATIC_NOERR

    HDassert(0 && "Not yet implemented...");//VDSINC

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5D__virtual_io_init() */


/*-------------------------------------------------------------------------
 * Function:    H5D__virtual_read
 *
 * Purpose:     Read from a virtual dataset.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Neil Fortner
 *              February 6, 2015
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D__virtual_read(H5D_io_info_t UNUSED *io_info, const H5D_type_info_t UNUSED *type_info,
    hsize_t UNUSED nelmts, const H5S_t UNUSED *file_space, const H5S_t UNUSED *mem_space,
    H5D_chunk_map_t UNUSED *fm)
{
    FUNC_ENTER_PACKAGE_NOERR

    HDassert(0 && "Not yet implemented...");//VDSINC

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5D__virtual_read() */


/*-------------------------------------------------------------------------
 * Function:    H5D__virtual_write
 *
 * Purpose:     Write to a virtual dataset.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Neil Fortner
 *              February 6, 2015
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D__virtual_write(H5D_io_info_t UNUSED *io_info, const H5D_type_info_t UNUSED *type_info,
    hsize_t UNUSED nelmts, const H5S_t UNUSED *file_space, const H5S_t UNUSED *mem_space,
    H5D_chunk_map_t UNUSED *fm)
{
    FUNC_ENTER_PACKAGE_NOERR

    HDassert(0 && "Not yet implemented...");//VDSINC

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5D__virtual_write() */


/*-------------------------------------------------------------------------
 * Function:    H5D__virtual_flush
 *
 * Purpose:     Writes all dirty data to disk.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Neil Fortner
 *              February 6, 2015
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__virtual_flush(H5D_t UNUSED *dset, hid_t UNUSED dxpl_id)
{
    FUNC_ENTER_STATIC_NOERR

    /* No-op for now VDSINC */

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5D__virtual_flush() */


/*-------------------------------------------------------------------------
 * Function:    H5D__virtual_copy
 *
 * Purpose:     Copy virtual storage raw data from SRC file to DST file.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Neil Fortner
 *              February 6, 2015
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D__virtual_copy(H5F_t UNUSED *f_src, const H5O_storage_virtual_t UNUSED *storage_src,
    H5F_t UNUSED *f_dst, H5O_storage_virtual_t UNUSED *storage_dst, H5T_t UNUSED *dt_src,
    H5O_copy_t UNUSED *cpy_info, hid_t UNUSED dxpl_id)
{
    FUNC_ENTER_PACKAGE_NOERR

    HDassert(0 && "Not yet implemented...");//VDSINC

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5D__virtual_copy() */

